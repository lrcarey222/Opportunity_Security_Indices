# Subsystem map: data inputs, provenance, existing QC, tests and diagnostics

Scope: worktree `C:/Users/LCarey/osi-worktrees/qc` (HEAD eabf5591). Processed reference tables: `C:/Users/LCarey/osi-worktrees/stages/data/processed`. Everything below was read only. All paths are relative to the qc worktree unless marked `stages:`.

## 1. Raw inputs manifest and source metadata

**Manifest, `config/raw_inputs_manifest.yml`.** Its header says `version: 2.0`, and its `generated_by` is `scripts/01_generate_raw_inputs_manifest.R`. It has 79 entries: 25 `api`, 42 `manual`, 10 `derived`, 2 `generated`, and 0 `unknown`.
- Allowed values are defined in `scripts/utils/raw_inputs.R:9-10`. `OPSI_RAW_SOURCE_TYPES` is `c("api","manual","derived","generated","unknown")` and `OPSI_RAW_STAGED_FROM` is `c("sharepoint","pipeline","repo")`.
- The per-entry fields come from `serialize_entry()` in `scripts/01_generate_raw_inputs_manifest.R:120-145`: `id, path, pattern, subdir, resolve, min_reporters, reporter_col, fetch_policy, source_type, source_name, url, cadence, owner, vintage, licence, staged_from, optional, retain, notes, required_by`.
- Division of labour, as stated at line 72: "Discovery owns wiring; humans own provenance." Discovery is `discover_raw_input_references()` (`raw_inputs.R:237`). It scans `scripts/` and `R/` and carries curated metadata across runs by `id`.
- **Field completeness:** `source_name` 79/79, `url` 64/79, `vintage` 40/79, `licence` 23/79. Vintage and licence are free text, not structured. For example, `comtrade_bulk_annual_totals` has the vintage "2020-2025 (2025 a stub of 8 reporters)" (manifest line 48). Licences run from "CC0-1.0" to "RESTRICTED…" (`iea_energy_value_added`, near line 1298).
- **Coverage floor.** `min_reporters: 50` with `reporter_col: reporter_iso` is set on `critmin_export`, `critmin_import` and `critmin_total_export` (lines 122, 136, 171). The resolver `resolve_versioned_raw_input()` (`raw_inputs.R:488-593`) walks vintages newest-first and skips any below the floor. If no vintage reaches the floor, it falls back to the best-covered one with a warning (line 562).
- `write_resolved_vintages()` (`raw_inputs.R:597`) writes `data/raw/raw_inputs_resolved.yml`. In stages it exists, dated `resolved_at_utc: 2026-10-30 17:23:50 UTC`, and records `id, file, exists, vintage_key, mtime_utc, reporters`.

**`scripts/02_render_sources_doc.R`** renders `docs/sources.md` (marked GENERATED). The table header at line 94 is `| Source | File | Cadence | Vintage in use | Owner | Read by |`. **It has no licence column and no URL column; the URL appears only as the link on the name.** The footer reports "Inputs with no named owner: **0**" and "`source_type: unknown`: **0**".

**`scripts/03_check_raw_inputs.R`** is a read-only preflight. It classifies each staged entry as BLOCKING, fetch-covered or optional (lines 64-79) and counts repo crosswalks present in `data/reference` (lines 104-114). It runs no content checks: no schema, row count or checksum.

**`docs/data_dictionary.md`** documents the tidy theme schema (lines 3-19) plus per-table notes. It was last committed 2026-10-21. Its stage list is **stale**: line 11 still says ``supply_chain` … (`Upstream`, `Midstream`, `Downstream`)``, but the canonical constant is `SUPPLY_CHAIN_STAGES <- c("Extraction","Refining","Components","Assembly","Deployment","Recovery")` (`R/utils/supply_chain_stages.R:10`).

**`config/`** holds 36 files. The provenance-bearing ones are `raw_inputs_manifest.yml`, `sector_dictionary.yml` and `index_definition.yml`.

**`config/sector_dictionary.yml`** is the richest per-indicator provenance and is read only by `scripts/87`.
- **75 `vars`**, each with `source`, `data_year`, `licence`, `caveats`, `prov` (48 `P` = processed verbatim, 27 `X` = read from raw through a pipeline function; no `D` in the current file), `parity`, `stale_years` (set on 3) and `min_base` (set on 3).
- **39 `sources`**, each with `id, short, citation, publisher, edition, url, licence, attribution, accessed, transform, issues`.
- It is validated by `sector_dictionary_validate()` (`R/dashboard/sector/sector_dictionary.R:112`).
- Some `edition` and `accessed` values are template placeholders filled at export, for example `{critmin_year} data`, `export of {pams_export_date}` and `{wdi_years} extract`.

**Theme rows carry provenance per row.** `validate_schema()` (`R/utils/schema.R:2-62`) requires `Country, tech, supply_chain, category, variable, data_type, value, Year, source, explanation`. It checks that `Year` is a non-NA integer and that `data_type ∈ {raw, index, weight, contribution}`. It does **not** check the `supply_chain` vocabulary, `iso3c`, or value ranges. The function has 16 call sites outside `schema.R` (counted by grep; the files are listed in section 2).

**What the shipped dashboard bundles carry (`dashboard/public/data`, tracked in git: 1,221 files)**

| Manifest | Provenance carried |
|---|---|
| `manifest.json` (strategic) | `schema "osi-dashboard"`, `created 2026-10-30T21:17:20Z`, `git_commit a54a9d3`, `osi_branch feat/supply-chain-stages`. `sources`: 27 entries of `{path, mtime, size, md5}`, no names, licences or years. `validation`: 70 rows (69 PASS, 1 INFO). |
| `sector/manifest.json` | `schema_version 2`, `git_commit 4bb2adc`. `sources`: 80 file records `{path, mtime, bytes, md5}`. `validation`: 180 rows (176 PASS, 4 WARN). `pipeline_marker` "pipeline 04-25 finished 2026-10-12…". Per-measure source and year travel in the chunks from the dictionary. |
| `country/manifest.json` | `schema_version 3`, `as_of 2026-10-30`. 17 `sources` of `{id, label, edition, licence, url}`. **`agreements` has edition null and licence null.** `osi_indices`, `wdi` and `iea_etp` have edition null. `validation`: 47 rows (44 PASS, 3 WARN). Country sheets carry `cells.trade.year`, `cells.trade.sources`, `cells.drivers.{es,eo}.vars.raw.{year,source,source_label}` and a per-sheet `checks`. |
| `posture/manifest.json` | `created, git_commit, core, details, counts` only: **no sources, md5 or validation**. Detail cells carry `trade.benchmark_source`, `policy.sources` and `partner_source`, but no years. Core meta carries `benchmark_period` and `momentum_period`. |
| `aib/manifest.json` | `schema, version, created, files…` (547 bytes): **no sources or validation**. |
| `partnerships/manifest.json` | **no created date, commit or sources.** |

**Data & sources page, `dashboard/src/views/Data/DataView.tsx`.**
- It reads only the strategic `manifest.json`.
- Sources are mapped to `{path, mtime, size, md5}` (lines 141-143) and shown as a File/Modified/Size table captioned "The pipeline files this export was built from." (lines 315-327).
- The validation table ("Validation of this export", lines 238-258) shows the status, layer, check, N, max |diff| and note of `m.validation`.
- A browser "Self-check" recomputes the baseline (lines 259-277).
- It does **not** show source names, licences, data years, or the sector, country or posture provenance.

**Existing UI provenance primitives** (`dashboard/src/conclusion/primitives.tsx`):
- `EvidenceDrawer` (146)
- `SourceMeta` (161; renders `data-testid="source-meta"`)
- `EvidenceGrade` (183). Its grades are `strong` "Several independent measures agree.", `moderate` "The main measures agree, with gaps." and `weak` "Thin or single-source data: treat as a lead, not a finding." (175-179).
- `NotAssessed` (203)
- `GUARDRAIL_MISSING` (257-263: fiscal cost, price gap, time to build, policy durability, probability of success)
- `MemoProvenance` (308)

**No "Report a data issue" control and no "Why this result?" provenance drawer exists.** `content/vocabulary.ts:546-548` has only "Why this compact?" question copy.

## 2. Existing validators and diagnostics

**Generic helpers** (`R/utils/schema.R`): `validate_schema` (2), `require_columns` (64), `assert_unique_keys` (82), and `assert_country_iso3_consistent` (99, warn or error). `validate_schema`, `assert_unique_keys` and `assert_country_iso3_consistent` together have 41 call sites. Most are in `cost_competitiveness.R` (10), `build_economic_opportunity_index.R` (7), `future_demand.R` (4) and `market_share_manufacturing.R` (3).

**Numeric bounds:** `validate_score_bounds()` (`R/indices/build_economic_opportunity_index_v2.R:80`) flags score variables outside [0, 1]. No equivalent exists in the ES, SP or posture engines.

**Domain checkers:**
- `posture_check_invariants()` (`R/dashboard/posture/posture_export.R:149-166`) enforces that Compete has a mode and no deploy mode, Deploy has a deploy mode and no mode, Secure and not-assessed have neither, there is no Compete at Deployment, Accelerate appears only at Deployment, and the posture is known. It stops on any violation.
- `sp_cp_check_contrib` and `sp_cp_stop_if_absent` (`R/strategic_position/competitive_position.R:114,175`).
- `aib_assert_six_stage_table` (`R/aib/aib_scores.R:24`).
- The `aib_check_*` family (`R/dashboard/aib/aib_check.R:16-724`). Its canonical vocabularies: classes `"COORDINATE","BUILD","MANAGE SPECIALIZATION","LOW INTERVENTION","INSUFFICIENT DATA"` (274); compact types `"candidate compact","bilateral arrangement sufficient","compact unnecessary: existing relationships do nearly as well"` (335); archetypes (336); strategic codes `RPA, STM, SMI, DMA, CIA, DSP, CBH` (720).
- `country_check_sheet` and `country_check_manifest` (`R/dashboard/country/country_check.R:37,228`).
- `sector_check_*` (`R/dashboard/sector/sector_check.R:205-381`).
- `dash_validate_bundle` (`R/dashboard/dash_engine.R:150`, shape and stage checks).
- `yf_validate_bundle` (`R/dashboard/yards_fences/yf_build.R:357`).
- Map validators: `climate_trace_map_validate`, `climate_sources_map_validate`, `iea_pathways_map_validate`, `uranium_validate_reference`, `technological_readiness_validate_rules`, `geo_validate_no_vote_map`, and `validate_outputs` (geo).

**Export-time check logs (the closest thing to automated data QC)**
- `scripts/85_export_dashboard_data.R`: `add_check(check, layer, status, n, max_abs_diff, note)` (146) and `cmp_num(..., tol = 1e-10)` (150). It writes `<r-out>/validation_report.csv` (1015) and stops publishing on any FAIL unless `--allow-fail` is given (1019; header line 35). The checks are mainly engine-vs-stored parity: "Friendshore index bit-identical" (851), "PSI partner view == psi_tbl" (883), "IMF position unit" (933), and CM-FDI attribution "CC BY" (995).
- `scripts/87_export_sector_data.R`: `add_check(check, status, n, detail)` (118). It writes `data/processed/dashboard/sector_validation.csv` (1298).
  - Hard guard (408-410): `if (n_critmin_reporters < 100) stop("…a stub vintage?")`, logged as "guard: critical-mineral exports from at least 100 reporting economies".
  - "guard: Comtrade dyads have at least 100 importer reporters in" the max trade year (618).
  - "guard: HS6 basket distinct, one sub-sector per code and cell, no excluded code in the basket" (609).
  - "guard: IEA CM top-3 share recomputed = IEA 'Top 3 share' (0.001)" (536).
  - A family of "parity X" checks against raw sources (411, 436, 465, 561, 578).
  - "trade bases: matrix and exporter-reported China shares within 3 points" (shipped as a WARN).
- `scripts/88_export_country_data.R`: writes `country_validation.csv` (1018).
- `scripts/89_export_yards_fences_data.R`: `--check-only` mode (header line 25).

**`diagnostics/`** (140 tracked files)
- NIPO scripts `00_nipo_baseline.R` through `09_attribution_ladder.R`, `run_diagnostics.R`, and their CSVs.
- `diagnostics/strategic_position/`: 106 files written by `scripts/27_review_strategic_position.R` (`diag_dir` at line 151; `decision_cards.md` at 145). They include:
  - **sensitivity:** `choice_threshold_sensitivity.csv`, `posture_threshold_sensitivity.csv`, `legacy_quadrant_threshold_sensitivity.csv`, `large_yard_sensitivity.csv`, `trade_momentum_weight_sensitivity.csv`
  - **consistency:** `consistency_cells.csv` (436 lines, for example check `partner_despite_cp_x75`) and `consistency_before_after.csv`
  - **coverage:** `evidence_coverage_by_{Country,index,supply_chain,tech}.csv`, `choice_input_coverage.csv`, `deployment_need_coverage.csv`
  - **trade:** `trade_momentum_{reporters,small_base,cutoff,prices,movers,availability}.csv`
  - **validation:** `opportunity_validation_*.csv`, `yf_paper_reproduction.csv`, `partner_recs_sanity.csv`
  - **review cards:** `decision_cards.md`, `choice_cards.md`, `decision_review.md`
  - Last commit e41cb5bb (2026-10-30).
- `diagnostics/us_trade_restrictiveness/`: `run_info.csv`, `tracker_reproduction.csv`, `tri_benchmarks.csv`, `tri_cell_drivers.csv`, `tri_vs_posture.csv`, `verdict_threshold_sensitivity.csv`.
- **`diagnostics/baseline/` does not exist** in qc (it is an untracked directory in the OneDrive checkout only).
- **There is no `outputs/` directory.** `output/` holds only `debug`.
- **`docs/replication_log.md` is stale and narrow.** It covers only the geopolitical-distance replication and says "`Rscript` is not available in the execution environment". It points to `output/geopolitical_distance/validation_checks.csv`, which does not exist.

## 3. R test suite (`tests/testthat`)

- **Size:** 129 `test-*.R` files plus 3 helpers and a `fixtures/` directory (133 entries in all), with 1,528 `test_that` blocks.
- **Fixtures:**
  - `tests/fixtures/`: `energy_security_theme.csv`, `index_definition.yml`, `missing_data_{zero,global_average}.yml`, `iea_critical_minerals_fixture.xlsx`, `imf_pcps_energy_prices_minimal.csv`, `technological_readiness_iea_fixture.csv`, `iea_clean_tech_guide_subset_timeseries.csv`
  - `tests/testthat/fixtures/`: `country_broken_cases.json`, `country_fmt_cases.json`
- **Helpers:**
  - `helper-config.R:5` resolves the root with `normalizePath(test_path("..", ".."), …)`, sets `options(opportunity_security.repo_root=…)`, and falls back to `OPSI_CONFIG=config/config.example.yml`.
  - `helper-fixtures.R` provides `fixture_path()` (`test_path("..","fixtures",…)` with a repo-relative fallback), `read_fixture_csv`, `read_fixture_yaml` and `set_test_index_definition`.
  - 123 files use `test_path("..","..")`.
- **Coverage by kind:**
  - *Synthetic unit and logic tests* are the large majority, for example `test-strategic-index.R:52` "the SI is 0.2 pol + 0.3 EO + 0.3 (1 - ES) + 0.1 stage + 0.1 climate" (golden formula constants), `test-decision-engine.R`, `test-strategic-posture.R` and `test-yard-rollup.R`. 28 files use `tolerance`.
  - *Schema and contract:* `test-schema.R`, `test-raw-inputs-manifest.R`, `test-sector-chunks.R`, `test-sector-dictionary.R`, `test-country-check.R`, `test-dashboard-osb.R`.
    - `test-raw-inputs-manifest.R` checks unique ids, no newline in paths, a valid regex `pattern`, `source_type ∈ OPSI_RAW_SOURCE_TYPES`, and drift against `discover_raw_input_references()`. It does **not** check licence, vintage or url.
  - *Coverage and stub guards:* `test-raw-inputs-coverage.R` (33, 57, 150 "the staged manifest puts a 50-reporter floor on every critmin pull", 160 "…resolve past the 2025 stub") and `test-comtrade-energy-trade-coverage.R` (27/44/58: every basket code in both flows, full reporter set, a total for every reporter as the RCA denominator).
  - *Parity:* `test-sector-parity.R` (prov P identical, prov X 1e-12 relative).
  - *Data-dependent:* 22 files `skip_if_not(file.exists(...))` on staged `data/raw` or processed files. For example, `test-hs6-crosswalk.R:17,385,481`, `test-usgs-mcs-reserves.R:318` and `test-nipo-policy-index.R:1194` skip with "Task 0 baseline not present". These silently skip in CI.
  - *Golden values against real outputs:* effectively none. There are no `expect_snapshot` calls and no `_snaps/` directory.
- **CI** (`.github/workflows`, every job on `runs-on: ubuntu-latest`):
  - `r-check.yml` (on PR and push to main) runs a secret scan (26-50), `testthat::test_dir("tests/testthat")`, and a smoke test of `build_energy_security_index_v2` on fixtures.
  - `r-tests.yml` (on every push and PR) runs `test_dir` again, so the suite runs twice per PR.
  - `r-ci.yml` runs "Build themes", which catches missing raw data and skips, so it is a no-op in CI.
  - Packages come from the Posit noble binaries. cowplot, ggrepel and data.table are not installed.
  - `dashboard.yml` (paths `dashboard/**`) runs Node `node-version: 24` (package.json engines `>=24`), then `npm ci`, `typecheck`, `npm test`, `build` and `budget`.
  - `news-scoring.yml` runs Node 22.

## 4. Dashboard test suite

- `dashboard/vitest.config.ts` defines these projects:
  - `unit`: node, `src/**/*.test.ts` minus the parity, sector, country and aib real tests
  - `ui`: jsdom, `src/**/*.test.tsx`
  - `parity-synthetic`: `src/engine/__tests__/parity/synthetic.parity.test.ts`
  - `parity-real`: `…/real.parity.test.ts`, 600 s
  - `sector-real`: `src/sector/__tests__/real.sector.test.ts`
  - `country-real`: `src/country/__tests__/real.country.test.ts`, env `COUNTRY_REAL=1`
  - `aib-real`: `src/aib/__tests__/real.aib.test.ts`
- `npm test` runs only unit, ui and parity-synthetic (package.json `"test"`). The real projects never run in CI.
- There are 127 `*.test.ts(x)` files under `src`. `dashboard/tests/fixtures/sector` holds the synthetic fixtures.
- **Golden regression** (`dashboard/src/regression/golden.test.ts`, in the `unit` project, so it runs in CI because `public/data` is committed):
  - It decodes the posture core and details, the country manifest with devpart, and the AIB compacts with the managed scenario, through the app's own derive functions, for `['USA','JPN','KOR','AUS','IND','DEU']`.
  - It rounds to 10 significant digits (`tidy`) and hashes large sections with SHA-256 (`hash`).
  - It compares with `toMatchFileSnapshot('./__snapshots__/golden-outputs.json')` (line 151; file 664,668 bytes), behind `describe.skipIf(!have)` (84).
  - It covers per cell: posture, mode, deployMode, verb, fence, yards, need, prize, market, cp, share, rationale, pathway, alignment\*, partner1, partnerGap, momentum, evidence, flag and borderline. It also covers counts, the strategy sentence, rankings, fences, devpart and AIB roles and scenarios.
  - The process is documented in `dashboard_redesign/regression_check.md:18,90-104` and is updated with `-u` only after a deliberate change.
- **Budgets:** `dashboard/scripts/check-budget.mjs` (for example, "All data <= 52 MB", "Country data <= 23 MB").

## 5. Trade vs production evidence (stages `data/processed`)

All tables below use the tidy schema unless noted.

| Table | Rows | Coverage | Source(s) |
|---|---|---|---|
| `critical_minerals_production_tbl` | 4,866 | 218 countries; 16 techs incl. minerals; **Extraction only**; Year 2024-2025; variables `<Tech> Production`, `Overall Production Index` | "EI Statistical Review of World Energy (2026 edition)"; "OECD-NEA/IAEA (2026), Uranium 2026…NEA No. 7770"; "Author calculation" |
| `critical_minerals_processing_tbl` (grouped_df) | 2,616 | 109 countries; 6 techs; Extraction + Refining; Year 2025; category `Foreign Dependency`, variable `Mineral Supply` | "IEA Critical Minerals Database" |
| `market_share_manufacturing_tbl` | 1,602 | 108 countries. ETP `Overall Market Share` **Year 2035** at Components/Assembly for Batteries, Solar, Wind, Green Hydrogen, Heat Pumps and "Clean Tech Midstream" (30 countries each). EV Assembly/Deployment 2024 (`production`, `net_export`, `market_share`…) | "IEA Energy Technology Perspectives 2024"; "IEA EV Outlook" |
| `production_depth_momentum_tbl` | 5,828 | 218 countries; 10 techs; Deployment/Extraction/Refining; Year 2025; `production_latest`, `size`, `growth_abs`, `Overall Production` | EI 2026; EIA Intl; IEA CM; Red Book 2022/2024/2026 |
| `major_supplier_shares_tbl` (non-tidy) | 7,573 | 211 economies; columns `iso3, country, tech, supply_chain, measure, mineral, measure_label, mineral_demand_share, year, value, unit, world_share, is_aggregate, source, threshold (0.05/0.10), major` | Measures: `atlas_export_share` (2024, 5 stages), `iea_mineral_mining`/`iea_mineral_refining` (2025), `ei_mineral_mining`, `ei_production`, `ei_reserves` (2020), `usgs_reserves`, `uranium_mine_production` (2024), `uranium_resources`, `iea_manufacturing_capacity` (2024; **only 15 Assembly and 5 Components rows**) |
| `reserves_tbl` | 5,787 | 124 countries; Extraction | USGS MCS 2026; EI 2026; Red Book 2026 |
| `cm_fdi_projects_tbl` (project-level) | 1,042 | 38 columns incl. `home_iso3, destination_iso3, status, year_announced, year_completed, capital_usd_m, capital_disclosed, index_use, index_exclusion, stages ("Components\|Assembly"), supply_chain ("Midstream"), china_parent, attribution_weight`; techs Batteries, EV, Solar, Wind; per-field `source_*` URLs, some prefixed "[UNVERIFIED]" | CM-FDI 2026.10.05 |
| `critical_minerals_trade_tbl` | 23,004 | 217 countries; Extraction/Refining/Recovery; 2024; `critmin_market_share`, `critmin_export_share`, `critmin_hhi` | "UN Comtrade (reported and mirror exports)"; "(mirror exports: partner-reported imports, FOB basis)" |

The sector bundle's `stale_years` field and the dictionary `issues` field (for example comtrade_matrix: "Technology-attributed: codes count in every cell that lists them. CAN, MEX and ZAF declare imports FOB.") already record source caveats.

**How trade feeds Position** (`config/strategic_position.yml`)
- The `competitive_position` family `export_position` (stages Extraction, Refining, Components, Assembly) uses `EO/Trade/market_share` and `rca`, both `fill: na` (34-42).
- The other families:
  - `production_position` (`Production/size`, `structural_zero`, Extraction and Refining)
  - `manufacturing_capacity` (`Investment/Operating Capacity Index`, GCIM)
  - `investment_position`
  - `cost_position` (BNEF LCOE)
  - `resource_endowment`
  - `deployment_scale`
- The score is the mean of observed families with equal weights, taken as a mid-rank percentile within the cell. `min_countries_per_cell: 5`.
- ETP `Overall Market Share` is explicitly context only: "IEA ETP 2035 pipeline share, five entities".
- `trade_benchmark` (101-109) replaces market_share and rca with Comtrade World exports averaged over `years: [2022, 2023, 2024]`, `min_years: 2`, with "the Atlas share kept only for economies that do not report".
- `trade_momentum` has `momentum_weight: 0.15`, `min_complete_share: 0.8`, `small_base_min_usd: 1.0e+6`, `winsorize [0.05, 0.95]`, `reporter_mismatch {caution: 0.10, exclude: 0.25}` and `min_reporters_per_cell: 5`.
- The code is `sp_trade_benchmark` (`R/strategic_position/trade_benchmark.R:23`; "a code in several cells counts in each", 15-16), `sp_trade_benchmark_index` (92-115, Atlas fallback) and `sp_apply_trade_benchmark` (121).
- Decision thresholds (417-425): `need_threshold: 0.67`, `prize_threshold: 0.6`, `cp_preserve_pct: 0.75`, `deployment_threshold: 0.6`, `borderline_band: 0.05`.

**Re-export and entrepot handling**
- `config/index_definition.yml:282-283` sets `re_export_hubs: [HKG]` **only**.
- These hubs are dropped from the Comtrade exports, imports, RCA, export size and HHI (`R/categories/trade/trade_core.R:19-23, 632-640`) and from the CP benchmark (`scripts/26_build_strategic_position.R:190,199`). Their Atlas variables "stay".
- The AIB uses `config/aib.yml:35-36`, `reexport_hubs: CHN: [HKG, MAC]`. Hubs hold no anchor, specialist or scale position (`R/aib/aib_strategic.R:762-850`) and inherit the origin's shock (`R/aib/aib_trade_model.R:35-43`).
- Critical-minerals trade relies on origin attribution by importers (`R/categories/minerals_trade/critical_minerals_trade.R:199-200`).
- **SGP, NLD, BEL and ARE get no handling anywhere.**

## 6. Documented data traps

- **Comtrade stub vintages:** `raw_inputs.R:478-482`, the manifest floor, the 100-reporter guards in `scripts/87:408-410,618`, and bulk totals "2025 a stub of 8 reporters".
- **HS6 crosswalk duplication:** `hs6_normalize_master()` (`R/utils/hs6_crosswalk.R:127-134`, "unique() alone kept both halves of a conflicting pair, so NIPO counted those codes twice"). `data/reference/energy_hs6_master.csv` has 509 rows, and **139 HS6 codes appear in more than one row**, counted in every cell that lists them by design.
- **HS 281290:**
  - `energy_hs6_master.csv` lines 177, 317 and 449 file it under "Array Cables & Collection Systems" (Wind), "Solar Cabling & Connectors" (Solar) and "Grid Cables & Accessories" (Electric Grid), with `stage` Refining, essential TRUE, not excluded.
  - `data/reference/electro_industrial_hs6_dictionary.csv:96,226` correctly labels it "NF3 and other non-metal halides" and "SF6 insulating gas".
  - No guard exists in code.
- **Other caveats:**
  - EI hydro world residual (sector WARN)
  - carried-forward climate years (WARN)
  - IEA WEO 2024 vs 2025 pathway shifts (WARN: "gen_coal nze 2040 -100%")
  - matrix vs reporter China shares beyond 3 points in 15 cells (WARN)

## Gap list

- **Schema:** `validate_schema` has no stage vocabulary, ISO3, or (Country, tech, supply_chain, variable, Year) key-uniqueness check by default. Posture, AIB and partnership manifests have no schema-level provenance. The raw-manifest test does not check required provenance fields.
- **Missingness:** no systematic missingness report per indicator × country × stage. Coverage exists only for SP (`evidence_coverage_*`, `choice_input_coverage.csv`). 4,887 of 14,124 cells are `not_assessed` (`choice_threshold_sensitivity.csv` base row), and nothing on screen explains this per cell.
- **Numeric validity:** a [0, 1] bounds check exists only for EO v2. There is no check for negative trade values, shares summing above 1, or outliers. Parity checks prove only that bundles reproduce the pipeline, not that the inputs are right.
- **Trade consistency:** no mirror-vs-reported check outside critmin. There is no production-vs-export plausibility test (for example export share far above production share), and entrepots other than HKG are not flagged.
- **Classification logic:** posture invariants exist. However, `posture_threshold_sensitivity.csv` shows that 11 of 16 perturbations change 0 of 14,124 cells (for example `position_strong_0.05/0.15`, `coercion_0.4/0.6`). Nothing tells robust apart from inert-parameter results. There is no per-cell robustness class.
- **Provenance completeness:** manifest licence is filled for 23/79 entries and vintage for 40/79. `docs/sources.md` has no licence column. The strategic sources have no names or years. Country `agreements` has a null edition and licence. The posture, AIB and partnerships bundles have no provenance at all.
- **Version drift:**
  - `dashboard/public/data` is tracked in git, with 28 commits touching it (for example 9321a80a, e41cb5bb, f7e740f5), so earlier releases can be diffed from history.
  - Manifests carry md5s: the bundle's `index_outputs.rds` md5 `25c5e2a9…` matches the stages file today.
  - `stages: data/processed/_backup_2026-10-21_partner_pre_tradefix/` and `_backup_2026-10-22_partner_pre_oecd/` (13 files) hold pre-fix partner tables.
  - `stages: data/processed/vintages/` holds `index_vintage_source_years.csv` and `index_vintage_theme_provenance.csv`.
  - The 2026-11-20 removed worktrees are backed up at `C:/Users/LCarey/osi-worktrees/_backups/removed-2026-11-20`.
  - **No tool diffs one release against the previous one.**

## Problems noticed

1. **HKG re-enters Competitive Position through the Atlas fallback.** It is excluded from the Comtrade benchmark, so `sp_trade_benchmark_index` (`trade_benchmark.R:96-104`) fills it from Atlas.
   - In stages `strategic_position_tbl.rds` (dated 2026-11-17, newer than the 2026-10-31 posture bundle), HKG has `trade_benchmark_source = atlas_fallback` on 34 rows. Its CP percentile is 0.84-0.91 at Components and Assembly for Batteries, Electric Grid, EV and Solar (for example Solar Assembly 0.91).
   - SGP Electric Grid Components shows a 9.6% world share and CP 0.95. NLD Solar Assembly shows 6.3% and CP 0.93.
   - These are likely entrepot artifacts that no rule handles.
2. **HS 281290 (SF6/NF3)** sits in three cabling sub-sectors at Refining in the live basket (`energy_hs6_master.csv:177,317,449`), with no exclusion.
3. **Licence drift.** The sector dictionary marks `iea_weo`, `iea_weo_pathways`, `iea_weo_regions` and `iea_nze2021` as `licence: attribution`. The raw manifest records CC BY-NC-SA 4.0 or CC BY-NC-SA 3.0 IGO (`weo_annex_a`, `weo_annex_a_regions`, `iea_nze2021_annex_a`) and RESTRICTED for `iea_energy_value_added`. The country manifest gives `iea_etp` "International Energy Agency terms of use".
4. **Country bundle WARN:** "NIPO: measures tagged with the six supply-chain stages … stages in the run: Cross-cutting, Midstream, Upstream". The shipped country sheets have no per-sector NIPO lists.
5. **Legacy stage labels persist.** `docs/data_dictionary.md:11` still names Upstream/Midstream/Downstream, and `cm_fdi_projects_tbl$supply_chain` is "Midstream" alongside a six-stage `stages` column.
6. **`market_share_manufacturing_tbl` stores a 2035 ETP projection in `Year`** (2035). The same capacity appears as `year 2024` in `major_supplier_shares_tbl`, covering only 15 Assembly and 5 Components rows.
7. **Stale or missing generated files.** The `comtrade_vintage.yml` manifest entry (lines 544-545) has no file in stages `data/raw`. The sector `pipeline_marker` says 2026-10-12 while the strategic bundle is from 2026-10-30.
8. **The branch does not contain origin/main.** It lacks #366, the `ubuntu-24.04` R CI pin (all workflows still use `ubuntu-latest`), and the 2026-11-17 current-trade AIB and posture re-exports. Merging main will change the golden snapshot (`golden-outputs.json`, baseline 2d0ae52d).
9. **`docs/replication_log.md` is obsolete** (see section 2).
10. **The R suite runs twice per PR** (in r-check and r-tests). The 22 data-dependent test files always skip in CI. Four real-data vitest projects never run in CI.
11. **Several posture threshold perturbations have zero effect.** `position_strong_*`, `position_moderate_0.05`, `coercion_*` and `allied_alternative_*` change 0 cells, so these thresholds may be inert or not wired.
