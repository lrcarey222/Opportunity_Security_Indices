# Subsystem map: the dashboard data files the app ships (qc worktree, `feat/qc-review-system`, HEAD eabf5591)

All paths below are under `C:/Users/LCarey/osi-worktrees/qc/` unless stated otherwise. Data root: `dashboard/public/data/`. Sizes are on disk. The `.osb` files are stored gzipped, exactly as they are served.

## 0. Bundle inventory and sizes

| Bundle | Files | Size | TS contract |
|---|---|---|---|
| Strategic Insights engine (legacy ES/EO/SI/PSI) | `manifest.json` (58 KB), `core.0dbc8572.osb` (1.2 MB), `vars.3c1eaadc.osb` (1.1 MB), `partner.f3d1c9b5.osb` (12.5 MB), `ties.a7e97a64.osb` (327 KB), 66 × `pcell-<tech>-<stage>.<md5>.osb` (21–46 KB each) | ~17.8 MB gz (`manifest.budget.all_gz` 17,753,376) | `dashboard/src/engine/types.ts:18` ChunkInfo, `:27` Manifest, `:204` Bundle; `engine/bundle.ts:7` SCHEMA `'osi-dashboard'` |
| Posture (canonical Need → Prize → Position → Choice) | `posture/manifest.json` (9 KB), `posture-core.32c987d5.json` (2.54 MB), 214 × `posture-detail-<ISO3>.<md5>.json` (~96–138 KB each) | 28 MB | `dashboard/src/posture/contract.ts` |
| Sector | `sector/manifest.json` (72 KB) + 19 `.osb` chunks | 5.9 MB (sector_gz 5,976,367) | `dashboard/src/sector/contract.ts:597–684` |
| Country | `country/manifest.json` + `facts-` ×219, `drivers-` ×167, `devpart-` ×209, `trade-` ×30, memo files ×32 (json/md/pdf/docx), figures, plus `memos/*.md` ×31 | 100 MB (mostly PDF/DOCX) | `dashboard/src/country/contract.ts` |
| AIB | `aib/manifest.json`, `compacts.7b238c82.json` (2.69 MB), `scenario-baseline.349f38ae.json` (1.89 MB), `scenario-managed.932adcee.json` (794 KB) | 5.2 MB | `dashboard/src/aib/contract.ts` |
| Partnerships | `partnerships/manifest.json` + 13 memos × (json/md/pdf/docx) | 18 MB | `dashboard/src/conclusion/memos.ts:14–57`; documents reuse `MemoDoc` (`country/contract.ts:858`) |

## 1. The .osb format: how to decode it in Node

- **Writer:** `R/dashboard/dash_osb.R`.
- **Decoder:** `dashboard/src/engine/codec.ts`. It is pure: bytes go in, an object tree comes out.

Byte layout (`codec.ts:73–96`):
1. Bytes 0–3 are the magic `"OSB1"`.
2. Bytes 4–7 are a uint32 LE, `H`.
3. The next `H` bytes are a UTF-8 JSON header: `{format:"osb", version:1, schema_version:1, kind:"chunk"|"fixture", name, objects:{<dotted.path>: Node}}`.
4. Padding brings the offset up to an 8-byte boundary. The data section starts there, and every `node.off` is relative to it.

Node types (`codec.ts:101–176`):
- `f64` (LE float64; NA = NaN) and `i32` (NA = −2147483648, `engine/numeric.ts:11`).
- `bool` (uint8; NA = 255) and `bits` (LSB-first).
- `dict16`: uint16 codes into a nested f64 `dict`; 65535 = NA.
- `fct`: `levels` plus i32 `codes`; a negative code means null.
- `strv`, `const`, which carries `dtype` (f64/i32/bool/chr), and `json`, which holds the value inline.
- `table` `{nrow, cols}`.
- `dim:true` → `NDArray {shape, data}`.

`decodeOsb()` returns `{header, flat, objects}`. `objects` is `flat` unflattened on the `.` separator (`codec.ts:178–201`).

**Reusable Node loaders.** Nothing in `dashboard/scripts/` decodes `.osb`. That folder holds only `check-budget.mjs`, which gzips files to measure them, and `prepare-fence-pages.mjs`. The Node-side loaders that exist:
- `dashboard/src/engine/__tests__/parity/harness.ts:12–18`: `readOsbFile(path)` → `gunzipSync`, then copy into an aligned `Uint8Array`, then `decodeOsb`.
- The same file, `:20–32`: `loadWebBundle(dir, chunks)` assembles the full engine bundle.
- `dashboard/src/sector/__tests__/real.sector.test.ts:67–68` does the same for sector chunks.

**Verified:** Node v24.19.0 imports the TS sources directly with `node --experimental-strip-types`, for example `import("./src/engine/codec.ts")` and `import("./src/posture/contract.ts")`. QC scripts can therefore call the app's own `decodeOsb`, `decodeCore` and `checkPostureDetail` without a build step.

Sector tables are integer-coded. In `facts.obs` (cols `var,item,geo,year,value,status,obs_year,obs_from`), `var` indexes rows of `dict.vars`, `item` indexes `dims.items` and `geo` indexes `dims.geo`, all in `sector-meta`.

## 2. ID schemes

- **Countries:** ISO3 throughout.
  - Posture has 214 economies (31 `core`, 28 `partner`, 155 `none`). HKG, MAC and PAN are absent.
  - The engine core `dims.countries` has 231, sector `dims.geo` 265, country manifest 247 economies / 219 sheets, AIB `countries` 117.
  - Partnership pair keys are `A_B`, e.g. `USA_JPN`, `USA_EUU`.
- **Technologies:** 11.
  - Posture and engine use display names: `Batteries, Coal, Electric Grid, Electric Vehicles, Gas, Geothermal, Green Hydrogen, Nuclear, Oil, Solar, Wind`.
  - Country and sector use slugs (`batteries`, `electric-grid`, …; `country/contract.ts:75–87`).
  - The sector bundle adds `semiconductors` (69 cells = 66 headline + Semiconductors).
- **Stages:** `SUPPLY_STAGES = ['Extraction','Refining','Components','Assembly','Deployment','Recovery']` (`src/lib/stages.ts:32`).
  - Slugs are lower-case (`stageSlug`, `:174`).
  - Display names (`posture-core meta.stage_display`): "Refining & Processing", "Component Manufacturing", "Final Assembly & Equipment", "Deployment & Use", "Recovery & Recycling".
- **Cell keys:** posture uses `"Tech|Stage"` (meta.phrases, yf_nodes `"tech|stage|layer"`). Country/sector use `"batteries-refining"`.
- **AIB:** a separate taxonomy.
  - 77 nodes with HS-group slugs (`lithium-chemicals`, `rare-earth-metals`).
  - Sub-technologies such as "Battery materials" and "Cells/packs".
  - 28 operational compacts `AIB-C01…`.
  - 10 strategic compacts: `SC-MAGNETS, SC-BATTERIES, SC-SEMIS, SC-SOLAR, SC-WIND, SC-NUCLEAR, SC-GRID, SC-EV, SC-HEAT, SC-INDUSTRY`.

## 3. Posture bundle (the canonical layer for golden cases)

**Manifest:** `posture/manifest.json`.
- `schema "osi-posture"`, `schema_version 2`, `created "2026-10-31T15:52:49Z"`, `git_commit "4bb2adc"`.
- `details` map ISO3 → file.
- `counts {countries 214, cells 14124, division_nodes 39, choices{...}}`.
- TS type: `PostureManifest` `posture/contract.ts:76`. It omits `counts.choices` and `git_commit`.

**Core** (columnar JSON; decoded and validated by `decodeCore`, `contract.ts:461`). Top-level keys: `meta, countries{iso3,name,tier}, cells, division, aib_header, design`.

`cells` columns:
- Required (`contract.ts:457`): `c,t,s` (indexes into countries/meta.techs/meta.stages), `p` (S/C/D/N), `m` (compete mode), `dm` (deploy mode), `need`, `prize`, `cp`.
- Optional (`:458`):
  - `v` (variant), `sec` (rationale), `nt` (need type), `share`, `pw` (pathway), `bd` (borderline inputs, `;`-joined).
  - `conf`, `fl` (consistency flag), `ev` (evidence high/medium/low/none), `al` / `alc` / `alf` (policy alignment, its confidence and flag).
  - `p1` (partner), `gap`, `mom`, `mq`, `io` (market prize), `yd` (yard code S/M/L/SM/ML/SL/SML).

Decoded row type: `PostureCell` `contract.ts:169`. Other types: `DivisionNode` `:214`, `PostureCore` `:235`.

`meta`:
- `thresholds {need 0.67, prize 0.6, cp 0.75, deployment 0.6, borderline_band 0.05}`.
- `prize_measure "strategic_value"`, `benchmark_period "2022-2024"`, `momentum_period "Jan 2025 - Dec 2025"`.
- `family_scenarios {demand: current_policy, policy_gap: national_commitments, system: current_policy}`.
- `core_members 31`, `partner_members 28`.
- 66 `phrases`, the 10 `large_yard_signals`, and 90 `yf_nodes`.

The thresholds match `config/strategic_position.yml:416–425` (`need_threshold 0.67`, `prize_threshold 0.6`, `cp_preserve_pct 0.75`, `deployment_threshold: 0.6`, `borderline_band: 0.05`) and `docs/strategic_position.md:489`.

Rules that matter for margin checks:
- Supply-chain need also has `score_floor: 0.10` (`yml:447`). It is not in the core.
- A large-yard case needs ≥5 signals. That threshold is in the detail trace, not in meta.
- Prize cap: "market_only" values are capped at 0.59, so "a capped value is never borderline" (`yml:501–505`).

`design` holds 16 keys `variant|delivery` → `{fence, fence_level, gate, build, benchmark, exit}`. These are the policy texts.

**Detail** (`posture-detail-<ISO3>.json`).
- Header: `{schema, schema_version 2, created, iso3, country, cells[66]}`. There is **no git_commit**.
- Validated by `checkPostureDetail` (`contract.ts:584`) and normalised to `NodeDetail` (`posture/detail.ts:204`, `nodeDetail` `:350`).
- Sub-types: `SupplyChainNeed` `:59`, `DeploymentNeed`/`Family` `:83`/`:31`, `Prize` `:94`, `CpFamily` `:114`, `TraceStep` `:121`, `PolicyEvidence` `:129`, `TradeEvidence` `:161`, `YardChild` `:195`.

Raw-key coverage across all 14,124 cells:
- `trace`, `policy`, `trade` are on every cell. `prize` is on 11,770, `cp_families` on 9,376.
- `pivotal` / `choice_possible` are on 1,289. `watch_reasons` 6,585, `not_assessed_reason` 4,887.
- `dn_*` (deployment families) 2,354. `partners` 1,650; `security_rationale` / `security_routes` / `mitigation` 206.
- `consistency_flag` is on only 12.

### (a) Two worked cells

**USA × Batteries × Refining** (core and `posture-detail-USA.623…`): Compete · Catch Up (`variant compete_catch_up`), confidence high, `delivery domestic`.

- **Need:** `need 0.453` (`trace` need step: threshold 0.67 → `low`).
  - Need families: `import_dependence 0.437`, `import_hhi 0.313`, `china_bloc 0.521`, `trusted_alternatives 0.703`.
  - Raw shares: `top_supplier CAN 0.173`, `china_share 0.135`, `opposite_bloc_share 0.148`, `imports_to_gdp 0.000197`, `trusted_share 0.0404`, `coercive_top CHN 0.944`, `coercive_import_share 0.148`.
  - Engine attributes: `substitutability hard`, `reconstitution_time y1to3`, `systemic_essentiality moderate`, `operational_control no`.
- **Prize:** `{value 0.834 (strategic_value), market 0.781, node_prize 0.633, investment_momentum 0.35, home_market 0.929 (national), maturity 9, basis node_and_home_market, outlook growing, note "World demand +176% 2024-35 (IEA minerals STEPS); capex -13% 2021-22 to 2024-25 (CIM)."}`.
- **Position:** `cp 0.312`, `home_share 0.0051`, `cp_families {export_position 0.661, export_position_structural 0.729, production_position 0.554, manufacturing_capacity 0.191, investment_position 0.76}`.
- **Pathway:** `pathway_credible yes`, basis `direct_supply_share;direct_export_share;operating_capacity;committed_pipeline;…`.
- **Trace:**
  1. secure_override → no
  2. large_yard_case 0 vs 5 → no
  3. supply_chain_need 0.453 vs 0.67 → low
  4. strategic_industrial_value 0.834 vs 0.6 → high
  5. cp_contender_pct 0.312 vs 0.75 → not_top_quarter
  6. pathway → yes
  7. choice → catch_up
- **Policy:** `{priority 0.598, support 0.987, protection 0.91, momentum 0.72, alignment mixed (high), flag sources_disagree, sources [nipo, pams]}`.
- **Trade:** `{benchmark_source comtrade, benchmark_share 0.034, benchmark_rca 0.358, benchmark_exports 2.82e9, momentum_score 0.276, months_observed 24, exports_current 2.54e9, exports_previous 2.68e9}`.
- `reason`: a text sentence.
- `yard_children`: bess-anode, bess-cathode and li-chemicals, all medium / catch_up.

**KOR × Batteries × Components:** Compete · Catch Up, confidence moderate, `pivotal cp_contender_pct`, `choice_possible preserve|catch_up`, `borderline cp_contender_pct`.
- `need 0.727` (high), `import_hhi 0.894`, `china_share 0.771`, `opposite_bloc 0.845`.
- `prize 0.787` (market 0.845), `cp 0.711` (margin −0.039), `cp_families {export_position 0.868, manufacturing_capacity 0.569, investment_position 0.479}`.
- `trade.benchmark_rca 3.48`, `benchmark_share 0.107`.

**Missing for a full trace (conclusion → components → indicators → raw → transform → source → year):**
- Need families are shipped as percentile values, not as the underlying measures with orientation. The family-to-measure recipe lives only in `config/strategic_position.yml:131–175` (e.g. `deficit_gdp` invert, `import_hhi` as_is).
- No per-family weights for the need, prize or CP composites ship in posture. The prize sub-weights (e.g. `investment_momentum` weight 0.25) are only in config.
- No per-indicator source id or data year. The only vintages are prose in `prize.note`, `trade.benchmark_source` and the core-level `benchmark_period` / `momentum_period`.
- No contender list or n behind `cp` percentiles. No raw production quantities.
- Indicator-level provenance with `raw {v, unit, year, source, source_label, note}` (`RawValue`, `country/contract.ts:492`) and weights (`DriverVar.weight`, `:507`) exists only for the **legacy ES/EO pillars** (country `drivers-*.json`, engine `vars.osb` `pillars.es.var`). In the USA batteries-refining Investment driver, `raw` is null.
- Sector `dict.vars` (75 vars: `source, data_year, licence, transform, prov`) and `dict.sources` (39) are the best source and vintage dictionary. They are not keyed to posture fields.

## 4. Engine bundle (legacy, Strategic Insights)

- `core.osb` (56 objects):
  - `dims.*`, with `dims.cells` = 66.
  - `pillars.es|eo.score` NDArray [11837,10] / categories.
  - `policy.*`, `baseline.core.si.cells` (11,419 rows with `es_need, eo, pol, sc_weight, tech_weight`).
  - `baseline.core.si.weights {policy 0.2, economic_opportunity 0.3, energy_security_need 0.3, supply_chain 0.1, technology 0.1}`.
  - `baseline.psi_partner`.
- `vars.osb`: `pillars.es.var` (62,261 rows: `cell, category, variable, component_value, cwwo, imputed, missing_rule_applied`) and `baseline.contrib.es|eo`.
- `ties.osb`: IMF DIP, CM-FDI and agreements.
- `pcell-*`: per-cell partner tops.

`manifest.validation`: 70 checks, all PASS, plus 1 INFO: "41 members change pick" with the endowment screen off.

## 5. Country bundle

`country/manifest.json` (`CountryManifest` `country/contract.ts:307`):
- `schema "osi-country"`, `schema_version 3`, `created 2026-10-31T15:58:15Z`, `git_commit 4bb2adc`, `as_of 2026-10-30`.
- `strategic_bundle_id c1736c4a…` and `sector_bundle_id 8f4910df…`.
- `nipo.vintage 2026-10`.
- `countries[]` (`CountryEntry` `:275`): `file`, `drivers`, `trade_monthly`, `devpart`, `memo{json,pdf,docx,md,figures,generated_at,verified_at,facts_sha256,stale,format}`.
- `sources[17]`, each with `edition` / `licence`, e.g. `comtrade_matrix "pulled 2026-10-07 to 2026-10-09"` and `atlas "Dataverse V18 … data to 2024"`.
- Statuses: 177 ok, 28 no_data, 42 non_rankable.

The facts sheet is `FactSheet` (`:801`). Cells are `CellFacts` (`:545`) with `{v, d}` value/display pairs (`Fv` `:232`), and there is a `src_map` (prefix → source id). This is a ready "field → source" provenance mechanism, but **for the legacy ES/EO/SI corners** (`legacy_corner`), not for posture choices.

Memos:
- 32 memos: 9 `format "brief"` (AUS, CAN, DEU, FRA, GBR, IND, JPN, KOR, USA) and 23 legacy, all `stale:true`.
- Brief inlines `{t:"value", v, path, claim, src}` (`MemoInline` `:824`) are claim-level provenance.
- R builds a per-claim trace CSV/MD: `country_brief_trace`, `R/dashboard/country/country_brief_objects.R:344`, called at `scripts/92_export_country_briefs.R:131–133`. It is not shipped.

## 6. Sector bundle

`sector/manifest.json` (`SectorManifest` `sector/contract.ts:623`):
- `schema "osi-sector"` v2, `bundle_id 8f4910df…`, `created 2026-10-31T15:58:13Z`, `git_commit 4bb2adc`.
- `pipeline_marker "pipeline 04-25 finished 2026-10-12 09:09:32 …"`, `processed_mtime_max 2026-10-30T17:28:02Z`.
- `headlines` (e.g. CHN graphite share 0.944, "IEA Critical Minerals Dataset 2026", year 2025).
- `sources[]` with md5s of inputs, `notes`, `validation`.

Chunks:
- `sector-meta`: `dims.geo` with a `re_export_hub` flag (only HKG is TRUE), `dict.vars`, `dict.sources`, `summary`, `reconcile`.
- `sector-facts`: `facts.obs` 107,218 rows.
- `sector-trade-<tech>`: `trade.flows` (item, exporter, importer, v2020, v2024, s2020, s2024), `trade.codes` (HS6 + description), `trade.excluded`. There are trade chunks for batteries, coal, electric-grid, EV, gas, nuclear, oil, semiconductors, solar and wind; none for geothermal or green hydrogen.

## 7. AIB bundle

- `aib/manifest.json`: `schema "osi-aib"`, `version 3`, `created 2026-10-30T21:38:20Z`, headlineShock `china_100`, headlineScenario `reallocation`. **No git_commit.**
- `compacts.*.json` (`AibDoc` `aib/contract.ts:787`) has keys `meta, countries, compacts[28], nodes[77], network{edges 1934}, investmentFlows, strategic`.
  - `meta.tradeYear 2024`, `built "2026-10-30T12:47:52"`.
  - `meta.thresholds {low_exposure 0.15, adequate_coverage 0.9, build_gap 0.1, coordinate_dividend 0.05, manage_largest_share 0.4, manage_top3_share 0.75, manage_cap 0.4, manage_coverage_loss 0.05, high_severity 0.4}`.
  - Classes: `COORDINATE, BUILD, MANAGE SPECIALIZATION, LOW INTERVENTION, INSUFFICIENT DATA`. The 77 nodes break down as BUILD 42, LOW INTERVENTION 30, INSUFFICIENT DATA 3, COORDINATE 2.
- `AibNode` (`:699`) has `cls`, `reason`, `physicalValidation`, `physicalDivergence`, `variants` (class under china_50, largest_*, conservative, release, production_constrained, trade_only) and `growth[]`. These are ready-made robustness inputs: 60 of 77 nodes change class in at least one variant. Some of the variants are different shocks rather than perturbations of the same one.
- `strategic` (`AibStrategic*` `:1176–1290`): `thresholds {pPrimary 0.9, pSecondary 0.75, maxPrimary 3, maxSecondary 2}`, `compacts[10]`, `compactRoles`, `theses`.
- Scenario files (`AibScenarioDoc` `:1106`): `schema "osi-aib-scenario"`, `version 1`, `created 2026-10-30T12:47:52`.

### (b) Compact headline and evidence fields
**SC-BATTERIES** (`strategic.compacts`):
- `purpose`, `steps[{order,label,nodes,functions}]`, `workstreams`.
- `core[{iso3,role,step}]`: USA "Resource supplier" (Minerals & recycled feed), COD, JPN/IDN "Processing anchor", KOR "Specialized technology supplier", DEU "Demand anchor" (Cells & packs).
- `nonAllied`.
- `scenarios.managed`: `{counts {build 3, low 2, insufficient 1, notStressed 3, notTested 5}, missions[{slot, cls, text, basis, targeted}], statement}`, plus `division`.
- Evidence: node-level `cls/reason/variants/physicalValidation` and `compactRoles[{leadNode, leadShare, leadBasis, supplier, demand}]`.

## 8. Partnerships bundle

`partnerships/manifest.json`:
- `schema "osi-partnership-memos"` v1.
- `pairs`: USA_JPN, JPN_KOR, USA_DEU, USA_KOR, USA_AUS, KOR_CAN, KOR_AUS, USA_IND, USA_CAN, USA_EUU.
- `compacts`: SC-SOLAR, SC-BATTERIES, SC-CRITMIN, each with `members[]`.
- Each entry has `generated_at` (2026-11-06 / 2026-11-13), `as_of 2026-10-30`, `evidence_access`, `verified:false` (all 13).

Memo JSON uses `schema "osi-partnership-memo"`, the brief AST, `writer "rules (R/partnerships)"`, `verifier "not independently verified"`. Sections: `summary` + `s1…s11`, with s11 the "Evidence Register".

**Headline conclusion:** the `choices` block in `summary`. Four items: `Build together` (kind preserve), `Trade and standards` (partner), `Institutions` (accelerate), `Stay out` (watch).

**Supporting evidence** is text and tables only:
- Appendix tables: "Compact/Node/Stage/US share/US need/Japan share/Japan need/AIB class/Coercive top supplier" (75 rows), the complementarity table (39) and actions (22).
- s11 states: "256 items (partial 197, unverified 18, verified 41)".
- There are **zero `value` inlines** (no path/claim provenance). The cited `evidence_register.csv` is not shipped.

## (c) Version stamps (current values)

| Where | created / generated | commit | other |
|---|---|---|---|
| `data/manifest.json` | 2026-10-30T21:17:20Z | `a54a9d3`, `osi_branch feat/supply-chain-stages` | `bundle_id c1736c4a25c7a33e73d56466eac415e6`, generator `scripts/85_export_dashboard_data.R` (commit at `:699`), R 4.4.1, input `sources[{path, mtime, md5}]` |
| posture manifest + core | 2026-10-31T15:52:49Z | `4bb2adc` (scripts/91:55–61; `posture_commit_stamp` adds `-dirty`, `R/dashboard/posture/posture_export.R:346`) | detail files have `created` only |
| sector | 2026-10-31T15:58:13Z | `4bb2adc` (scripts/87:1255) | bundle_id 8f4910df… |
| country | 2026-10-31T15:58:15Z | `4bb2adc` (scripts/88:111) | as_of 2026-10-30; memos `generated_at` / `verified_at` / `facts_sha256` / `src_sha256` / `claims_sha256` |
| AIB | 2026-10-30T21:38:20Z; meta.built 2026-10-30T12:47:52 | none | tradeYear 2024 |
| partnerships | per memo generated_at | none | evidence_access |

There is no "model version" or "data release" field beyond `schema_version` and the bundle_id md5s. Both a54a9d3 and 4bb2adc are ancestors of the redesign branch, but neither is HEAD (eabf5591).

## (d) Counts (posture core decoded with `decodeCore`)

- 214 countries × 11 technologies × 6 stages = **14,124 cells** (66 nodes per country).
- Choices:
  - Secure **206**: systemic_resilience 175, coercive_chokepoint 27, operational_control 4.
  - Preserve **103**, Catch Up **355**, Partner **1,474**.
  - Accelerate **514**, Watch **6,585**, Not assessed **4,887**.

  These equal `manifest.counts.choices`.
- By stage:
  - Recovery N 1,947 / D 310 / C 97.
  - Deployment D 1,683 / N 671.
  - Secure: Refining 81, Extraction 98, Assembly 24, Components 3.
- Secure nodes: Oil/Refining 42, Gas/Extraction 41, Oil/Extraction 32, Coal/Extraction 25, Electric Grid/Assembly 24, Gas/Refining 19, EV/Refining 17, Nuclear/Refining 3, Nuclear/Components 3.
- Confidence: high 6,494, moderate 2,558, low 5,072.
- Alignment: insufficient_data 6,835, aligned 5,826, over_prioritized 679, under_prioritized 310, mixed 233, deployment_gap 182, misaligned 59.
- Borderline tags: large_yard_case 1,163, supply_chain_need 482, strategic_industrial_value 389, deployment_need 257, cp_contender_pct 63.
- Flags: review_strong_position_low_prize 8, review_top_quarter_below_floor 4.
- Not-assessed reasons: "Supply-chain need is not measured…" 4,216; "Deployment need is not measured…" 671.
- Watch reasons: low_need 3,283, ample_diversified_supply 2,946, large_yard_case 2,133, weak_domestic_case 1,747, low_prize 1,536, low_deployment_need 1,169.

## (e) Candidate golden and anchor cells (shipped bundle 2026-10-31)

Format: need / prize / cp / share.

- **Obvious Secure**
  - JPN EV/Refining: Secure coercive_chokepoint, .998 / .842 / .318, top supplier CHN .47. Also on routes `operational_control;coercive_chokepoint`.
  - KOR Nuclear/Refining: Secure operational_control, .978 / .882 / .20.
  - CHN Gas/Extraction: Secure systemic_resilience, .998 / .319 / .833.
  - KAZ Nuclear/Refining: Secure systemic_resilience, .998 / .743.
- **Watch / low intervention**
  - USA Solar/Extraction: Watch, .86 / .286, yard large. Shows high need overridden by a low prize.
  - NLD Oil/Refining: Watch, .198 / .239 / .803.
  - CHN Solar/Deployment: Watch, deployment need .501.
  - KOR Batteries/Deployment: Watch, .559 against the 0.6 bar (borderline).
  - AIB node `lithium-minerals`: LOW INTERVENTION ("the shock removes 12% of demand").
- **Strong producers**
  - CHN Batteries/Refining: Preserve, .056 / .834 / .938 / .848.
  - CHN Batteries/Assembly: Preserve, .017 / 1.0 / .974 / .552.
  - KOR Batteries/Assembly: Preserve, .831 / .973 / .816 / .047.
  - USA Batteries/Components: Preserve, .062 / .814 / .921 / .301 (see Problem 8).
- **Import-dependent**
  - JPN Batteries/Extraction: Partner, .983 / .571 / 0, partner AUS.
  - KOR Batteries/Refining: Catch Up, .993 / .767 / cp 0, share .001.
  - IND Batteries/Extraction: Partner, .90 / .571 / 0.
  - DEU Batteries/Extraction: Partner, .85, partner AUS.
- **Hubs / entrepots**
  - BEL Oil/Extraction: **Secure** systemic_resilience, need .998, cp 0, share 0.
  - SGP Batteries/Refining: Catch Up, .934 / cp 0.
  - SGP Oil/Refining: Watch, cp .773, share .053.
  - NLD Batteries/Assembly: Catch Up, .919 / .973 / .184.
  - ARE Batteries/Components: Partner, .929.
  - ARE Oil/Assembly: Catch Up, .934 / .604.
  - HKG: absent from posture.
- **Sparse / not assessed**
  - USA, JPN, KOR, AUS, IND, DEU and CHN Solar/Recovery: all N (prize ≈ .44, confidence low).
  - USA Coal/Recovery: N, prize .009, outlook declining.
  - Geothermal and Coal/Gas Recovery, and Electric Grid/Deployment, are in `no_es_cells`.
- **Near a threshold** (margin = value − threshold, from `pivotal` plus the trace)
  - DEU Solar/Assembly: cp = 0.75 exactly → Preserve (margin 0).
  - IND EV/Assembly: cp 0.75 → Preserve (0).
  - IND Solar/Refining: cp 0.75 → Preserve (0), flag review_strong_position_low_prize.
  - IND Wind/Refining: prize .603 (+.003).
  - AUS Wind/Refining: prize .604 (+.004).
  - CHN EV/Deployment: dn .604 (+.004), Accelerate.
  - DEU Wind/Deployment: dn .591 (−.009), Watch.
  - USA Wind/Refining: prize .583 (−.017), Partner.
  - KOR Batteries/Components: cp .711 (−.039).
  - JPN Batteries/Assembly: need .713 and cp .711, confidence low.
  - 1,209 cells have a numeric pivotal margin. 165 sit exactly on a threshold, most at large_yard_case = 5.
- **Central NEIS cells** (Batteries; Solar)
  - USA: Batteries Partner / Catch Up / Preserve / Preserve / Accelerate (.639) / Catch Up. Solar Watch / Watch / Watch / Preserve / Watch / N.
  - JPN: Batteries Partner / Catch Up / Catch Up / Catch Up / Watch / Catch Up. Solar Watch ×3 / Catch Up / Accelerate / N.
  - KOR: Batteries Partner / Catch Up / Catch Up / Preserve / Watch / Catch Up. Solar Watch ×3 / Catch Up / Accelerate / N.
  - AUS: Batteries Watch / Catch Up / Partner (KOR) / Partner (POL) / Accelerate / Partner (USA). Solar Watch ×3 / Partner (JPN) / Accelerate / N.
  - IND: Batteries Partner / Catch Up ×3 / Accelerate / Catch Up. Solar Watch / Preserve / Partner / Preserve / Accelerate / N.
  - DEU: Batteries Partner / Catch Up ×3 / Accelerate / Catch Up. Solar Watch ×3 / Preserve / Accelerate / N.
  - Critical minerals have no posture technology of their own. They live as Batteries/EV Extraction and Refining, and in the AIB as SC-BATTERIES nodes and the SC-CRITMIN memo.
- **Surprising results**
  - CHN Solar/Components: Watch, prize capped .59, cp .983, share .259.
  - CHN Batteries/Extraction: Watch, share .537.
  - IND Solar/Components: Partner despite cp .948 (flag review_top_quarter_below_floor).
  - USA Batteries/Components need .062 vs Assembly .895.
  - USA as SC-BATTERIES "Resource supplier".
  - VNM "Processing anchor" and THA "Resource supplier" as core members of SC-MAGNETS.

An existing guard to reuse: `dashboard/src/regression/golden.test.ts` writes `__snapshots__/golden-outputs.json` (664 KB). It snapshots all 66 cells for USA, JPN, KOR, AUS, IND and DEU, plus AIB compacts, through the app's own decoders (`dashboard_redesign/regression_check.md:18–67`).

## Problems noticed

1. **The shipped posture bundle is stale against the newer run.**
   - `stages/dashboard/public/data/posture/manifest.json` was created 2026-11-17T23:35:30Z at commit 3760baae, with Secure 212, Preserve 111, Catch Up 351, Partner 1,475, Watch 6,574.
   - **207 cells change choice** between the two bundles. KOR Batteries/Components goes Catch Up → Preserve (cp .711 → .763). JPN Batteries/Assembly goes Catch Up → Preserve. AUS Grid/Gas/Oil Assembly go Watch → Partner.
   - Golden cases must pin the bundle hash.
2. **CP is volatile between runs.** USA Nuclear/Assembly cp .173 → .904; KOR Gas/Assembly .985 → .591; KOR Oil/Assembly .985 → .515. These are strong candidates for "fragile" in sensitivity classification.
3. **The TS fallback threshold is wrong.** `DEFAULT_THRESHOLDS.deployment = 0.67` (`posture/contract.ts:346`), while config, data and docs say 0.6. It is used only when the core lacks the field, but it is wrong.
4. **Provenance gaps.**
   - Posture detail docs, the AIB manifest and the partnerships manifest carry no git commit.
   - The engine manifest (a54a9d3) and the posture/sector/country manifests (4bb2adc) differ, and none of them is the branch HEAD.
   - No release id ties all the bundles together. Country/sector carry `strategic_bundle_id`; posture and AIB do not.
5. **The prize cap clusters values just under the bar.** 432 cells have prize exactly **0.59** (the market_only cap), so margin checks will show a spurious −0.01 cluster. Config says these are "never borderline".
6. **Discrete contender percentiles create knife-edges.** cp lands exactly on 0.75 in several NEIS cells (DEU Solar/Assembly, IND EV/Assembly, IND Solar/Refining). The inclusive ≥ rule decides Preserve.
7. **Shared baskets duplicate evidence.**
   - Batteries/Extraction and EV/Extraction are identical for JPN (need .983, import_hhi .756, top ZAF .377).
   - Batteries/Refining and EV/Refining share `coercive_top_share .944` and `trusted_share .0404`, yet JPN gets Secure on EV/Refining and Catch Up on Batteries/Refining.
   - Counts double-count the shared evidence.
8. **USA Batteries/Components `share`/`home_share` is 0.301**, against `trade.benchmark_share` 0.137. The share basis is unclear; this needs a trade-vs-production diagnostic.
9. **Entrepot handling is thin.**
   - `exclude_iso3: [HKG, MAC, MHL, LBR, PAN]` (`config/strategic_position.yml:168`). The sector bundle flags only HKG as `re_export_hub`.
   - SGP, NLD, BEL and ARE are scored as ordinary importers and producers. BEL Oil/Extraction comes out Secure with need .998.
   - Excluded economies are dropped from posture entirely rather than shown as "not assessed (entrepot)".
10. **Two frameworks ship side by side.** The country fact sheets carry the legacy ES/EO corners. USA Batteries/Refining shows ES at the 98th percentile ("legacy_grow") there, while posture need is 0.453, low. Reviewers could see contradictory "security" signals.
11. **Memo verification status.**
    - 23 of 32 country memos are `stale:true`.
    - Brief `facts_sha256` never equals the sheet's `checks.facts_sha256` (32/32). This is expected for briefs per `scripts/88:935–938`, but it is confusing for QC.
    - All 13 partnership/compact memos are `verified:false`, with no claim-level provenance. `evidence_register.csv` is not shipped.
12. **SC-CRITMIN has no structured data.** It has a memo, but it is not one of the 10 AIB strategic compacts, so the bundle has no `strategic.compacts` record behind it.
13. **Open validation WARNs.**
    - Country: NIPO measures tagged "Cross-cutting, Midstream, Upstream" rather than the six stages; AUT has no monthly trade.
    - Sector: matrix vs exporter-reported China shares differ by more than 3 points in 15 cells, including batteries-refining, -components and -assembly; there is also an IEA pathways vintage drift, e.g. gen_coal nze 2035 +73%.
14. **`MemoChoiceKind` has no `catch_up`** (`country/contract.ts:836`), so a brief's choices box cannot show Catch Up distinctly.
15. **Odd import mix in one Secure anchor.** JPN EV/Refining has `opposite_bloc_share 0.884`, well above `china_share 0.47`. It is worth checking before using the cell as an anchor.