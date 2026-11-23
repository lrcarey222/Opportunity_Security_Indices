## AIB subsystem map (repo root `C:/Users/LCarey/osi-worktrees/qc`; paths below are relative to it)

**Where things are.** Config: `config/aib.yml`, `config/aib_taxonomy_dictionary.yml` (default overlay, `aib.yml:17`), `config/aib_strategic_layer.yml`, `config/aib_strategic_compacts.yml`, `config/aib_node_functions.yml`, `config/coalition.yml`, `config/partnership_memos.yml`, `config/development_partnerships.yml`. Pure R: `R/aib/*.R`, `R/partnerships/pm_*.R`, `R/dashboard/aib/aib_export.R` + `aib_check.R`. IO: `scripts/30` (allied network design, a separate normative MILP), `31` (trade network/inputs), `32` (stress test, investment, compacts, scenarios), `33` (strategic layer), `34` (memos), `84` (AIB bundle), `93` (development partnerships). Bundle: `dashboard/public/data/aib/compacts.7b238c82.json` (schema `osi-aib` **version 3**, built 2026-10-30T12:47:52), plus `scenario-managed.932adcee.json` and `scenario-baseline.349f38ae.json` (schema `osi-aib-scenario` v1). Processed CSVs: `C:/Users/LCarey/osi-worktrees/stages/data/processed/outputs/aib/` (same 2026-10-30 run).

---

### 1. Trusted / allied partners: four different definitions are in use

1. **Coalition tiers (the AIB demand scope).** `config/coalition.yml:6-34`, `R/indices/coalition.R:9-22`, built by `scripts/13`.
   - **core**: the "U.S.-leaning" UNGA bloc (`us_bloc`, :27), with WDI GDP >= `gdp_floor_usd: 1.0e+11` (:18) and its own UN votes (`core_requires_own_vote: true`, :31).
   - **partner** comes by one of two routes:
     - agreement: a member of `U.S. FTA, IPEF, MSP, G7, NATO` (:40-45) with GDP above the floor;
     - supplier: not China-leaning, with >= `supplier_share: 0.05` (:20) of production, refining, capacity or exports in a supply stage, or >= `supplier_reserves_share: 0.10` (:26) of reserves.
   - The blocs come from Bailey-Strezhnev-Voeten ideal points, cut at quartiles: `bloc_rule: quartile`, `bloc_quantile: 0.25`, `tau: 0` (hard blocs) (`config/geopolitical_distance.yml:59-61`).
   - The AIB uses tiers `[core, partner]` (`aib.yml:54-57`), resolved in `scripts/32:146-162`.
   - The 2026-10-30 run has **59** economies (`meta.nDemandScope`; `aib_host_screen.csv in_demand_scope`). That includes COD, GAB, GHA, GIN, NAM, MDG and GTM. **TWN and CHE are not in it.**
2. **Candidate-partner universe: global** (`aib.yml:58-61`). Every exporter can supply if it has >= USD 1m **and** >= 0.05% of the node's world exports (`aib.yml:31-32`; `aib_trade_model.R` `aib_node_problem`, cand filter ~line 152). Open registries are excluded: `[LBR, MHL, PAN, BHS]` (`aib.yml:40`).
3. **Host alignment screen** (`aib.yml:252-255`; `R/aib/aib_host_screen.R:17-31`). Hosts are the demand scope, plus every partner not in `exclude_blocs: [China-leaning]`. An economy with no bloc is kept. In this run 48 of 204 partners are screened (CHN, HKG, MAC, RUS, MMR, LAO, KAZ, ARE, EGY, NGA, ZWE, VEN, ...). Friendshore is explicitly **not** used (`aib.yml:247-249`).
4. **Strategic-layer "allied set"**: the demand scope plus `treat_as_allied: [TWN, CHE]` (`config/aib_strategic_layer.yml:101-102`). Their own demand stays unprotected.

Two further definitions sit outside the AIB proper:
- The memos use `coercive_suppliers: [CHN, HKG, MAC, RUS]` (`config/partnership_memos.yml:26`), with "trusted" meaning not coercive.
- Development partnerships use the donor = high-income + demand scope, and the recipient inside the AIB trust screen (`docs/development_partnerships.md:20-22, 64`).

There is **no continuous alignment threshold** in the AIB; alignment is the hard-bloc membership above. There is a Friendshore composite, but it only breaks ties for hosts and is never a gate.

---

### 2. Stress-test logic

**Shocks** (`aib.yml:68-108`; `aib_trade_model.R:41-96` `aib_shock_targets`):

| id | what it does |
|---|---|
| `china_100` | China removed (`fraction 1.0`); **headline** (`headline_shock`, :117) |
| `china_50` | China halved (0.5) |
| `largest_subsector` / `largest_stage` / `largest_technology` | the largest exporter at node, six-stage stage or technology level is removed (`top_by`, :46-54) |
| `diversify_benchmark` | the "Managed Interdependence" scenario |

- HKG and MAC inherit China's shock (`reexport_hubs`, :35-36). No other hub is handled.
- Semantics are `hold` (headline: the shocked economy keeps buying) and `release` (two-way cut, a sensitivity) (`aib.yml:115`).
- Supply scenarios are `conservative` and `reallocation` (headline, :125-126).
- Physical constraint modes are `production_constrained` (headline) and `trade_only` (`aib.yml:145-149`):
  - `min_physical_completeness 0.80`
  - `producer_min_share 0.0`
  - `divergence_threshold 0.25`
- Only 11 nodes have credible physical data (`docs/aib_methodology.md:272-275`).

**States** (`aib_solve_state`, `aib_trade_model.R:313-458`). All quantities are shares of the demand scope's baseline *traded* demand D.
- **A static**: the shocked supply disappears (`coverage = 1 - L/D`, :288-294).
- **B existing network**: only 2024 links plus domestic retention. Each link may grow to at most `(1+existing_relationship_growth)` times its size, with `existing_relationship_growth: 1.0`, i.e. 2x (`aib.yml:173`; code :353-357).
- **C optimized**: any candidate can serve any scope importer.
- Available supply (`aib_available_supply`, :260-274):
  - conservative = current shipments to the scope plus freed supply;
  - reallocation also counts exports to outside markets (that part is reported as `diversion`);
  - with physical data, a non-producer is capped at its current shipments to the scope.
- **Lexicographic objectives** (`aib.yml:153-188`), in this order:
  1. coverage (`tolerance_coverage 0.0005`);
  2. stability, i.e. the smallest cut to surviving shipments (`1e-6`; a module addition);
  3. specialization, weighted `min(RCA,4)/4` (`rca_cap: 4`; tolerance 0.02);
  4. Partnership Opportunity (0.02);
  5. PAMS + CAT compatibility.
  - Missing tie-break scores are neutral at `0.5`.
- State C also has a closed form, `aib_optimized_analytic` (:461+).

**Definitions** (`aib_trade_model.R:613-649`):

| quantity | rule |
|---|---|
| `shock_exposure` | L/D |
| `coordination_dividend` | `pmax(C - B, 0)` (:621) |
| `residual_gap` | `max(1 - C, 0)` (:622); `residual_gap_usd` = that x D |
| `existing_recovery` / `optimized_recovery` | (B-A)/(L/D) and (C-A)/(L/D) |
| `optimized_diversion` | supply pulled from buyers outside the scope (:639) |

**"New capacity required" = the residual gap after optimized trade.** Trade is judged insufficient as follows (`aib.yml:278-297`; `aib_compacts.R:16-62`):
1. exposure < `low_exposure 0.15` → LOW INTERVENTION;
2. else residual >= `build_gap 0.10` → **BUILD**;
3. else concentrated → MANAGE SPECIALIZATION. Concentrated means `optimized_largest_share >= 0.40`, or top3 >= `0.75`, or the coverage loss at a 0.40 cap >= `0.05`;
4. else existing coverage >= `adequate_coverage 0.90` → LOW;
5. else dividend >= `coordinate_dividend 0.05` → COORDINATE;
6. else LOW.

Two more gates:
- The investment simulation runs only where the residual is >= `min_residual_gap 0.05` (`aib.yml:203`).
- `high_severity: 0.40` marks high-severity exposure for the network counts.

**Concentration / HHI:**
- HHI = sum of squared supplier shares (`aib_supply_stats`, :276-286; `aib_trade.R:5`; duplicate `aib_diversify.R:8`).
- Frontier caps: `max_supplier_share: [0.25, 0.40, 0.60, 1.00]`, headline 1.00 (:179-180).
- `tightest_costless_max_share` is the tightest cap within `frontier_coverage_tolerance 0.02` of the unconstrained coverage.
- `min_required_diversification` is the loosest cap whose re-optimized N-1 coverage is >= `frontier_acceptable_coverage 0.80` (`aib_frontier_minimum`, :705-729).
- N-1 has two readings:
  - instantaneous = `coverage x (1 - largest share)` (:539);
  - true N-1 removes the largest supplier and re-optimizes (`aib_n1_reoptimized`, :546; `aib_remove_supplier`, :217).
- In the classifier, `need_manage = pmax(ls-0.25,0)+pmax(t3-0.5,0)+loss` (`aib_compacts.R:46`). These 0.25 and 0.5 are hard-coded, not in config.

**Managed-interdependence benchmark** (`aib.yml:94-108`; `R/aib/aib_diversify.R`):
- The benchmark is the trade-weighted mean exporter HHI of the *other* HS6 lines of the node's HS4 heading (Atlas HS92, 2023). The fallback is the other nodes of the node's sector.
- `min_excess 0.005`.
- `aib_diversify_cap` (:41) finds the supplier-share cap that brings the node's HHI down to the benchmark. Suppliers above the cap lose the excess of their sales to the scope.
- `cap_basis = "demand"` (`aib_trade_model.R:120`) keeps the cap in states B and C.
- The strategic layer marks a node `NOT TESTED` when its node HHI >= `0.10` and its benchmark HHI >= `0.18` (`aib_strategic_layer.yml:26-28`).

**Lambda: not part of the AIB.** `network_diversity_lambda <- 0.78` belongs to the separate normative design in `scripts/30:465`.
- The calibration rule (designed median HHI <= observed median export HHI) is at :426-464.
- It is documented as knife-edge: margin 0.0036, 0.77 misses by 0.0002, and the curve is non-monotone (:459-463).
- It is flagged "WAS CALIBRATED ON THE THREE-STAGE NETWORK and must be recalibrated" (:437). The portfolio caps (2-10 by log GDP) were also not recalibrated (:410-424).
- Design constants (:521-546):
  - `max_share 0.40`, `min_share 0.05`, `min_producers 3`
  - `epsilon_supplier_share 0.10`, `min_suppliers_per_consumer 2`
  - `w_node 1, w_edge 0.5, w_dev 0`, `portfolio_top_k 5`

---

### 3. Before/after quantities already exported (no new modelling needed)

**Per node, Decoupling** (`compacts.*.json nodes[]`; mapping in `R/dashboard/aib/aib_export.R:240-290`):

| measure | before | after |
|---|---|---|
| Concentration | `baseline.hhi` | `hhi` (optimized_hhi) |
| Largest supplier | `baseLargest` / `baseLargestShare` (also `baseline.largest` / `largestShare`) | `largest` / `largestShare` |
| Top-3 share | `baseline.top3Share` | no post-shock field in the JSON (CSV: `optimized_top3_share`) |

- Risk and loss: `exposure`, `lostUsd`, `demandUsd`.
- Coverage path: `static` → `existing` → `optimized`, with `dividend` and `residual` (= new capacity share). `residualUsd` is not on the Decoupling node; compute it as residual x demandUsd.
- Diversified vs shifted: `diversion` (share drawn from outside buyers).
- N-1: `n1Reoptimized`, `instantaneousN1`, and per cap `frontier[{cap, coverage, largestShare, largest, instantaneousN1, n1Reoptimized, n1Removed, specialization}]`; also `tightestCostless`, `minRequiredDiversification`.
- Robustness: `variants{shock|scenario|semantics|mode: class}`, `variantWarnings`, `growth[{label, existing, dividend, cls, warning}]`, `coordinateSensitiveToGrowth`.
- Physical evidence: `warning`, `physicalValidation`, `physicalData`, `physicalDivergence`.
- Hosts: `hosts`, `specialtyHosts`, `developmentHosts`, `bestGain`, `screenedHost{host, gain, cost}` (the field is absent when null).
- Top five observed suppliers: `baseline.suppliers`.

**Per node, Managed Interdependence** (`node.diversify`):
- `currentHhi`, `benchmarkHhi`, `cap`, `capped[]`, `excess` (supply to shift);
- coverage path `static/existing/optimized/residual/residualUsd`;
- after the cap: `hhi`, `largest`, `largestShare`, `top3Share`;
- `newLinks`, `expandedLinks`, `lostUsd`, `dividend`, `cls`, `status`.

**Per compact (AIB-Cxx):**
- `metrics{demandUsd, lostUsd, exposure, static, existing, trade, investment, dividend, residual, residualAfterInvestment, globalOptimum, anchorFragmented, anchorCompact, compactGain}`;
- `managed{nodesRun, static, existing, optimized, residual, residualUsd, classMix}`, `baseline{hhi, suppliers}`;
- coalition sizes: `coalition{members, t90, t95, t99, criticalPartners, criticalExternal, criticalExternalMarginal, prospectiveHosts, external}` (gives the number of credible suppliers); `path[{step, added, minCoverage, meanCoverage, nodeCoverage}]`;
- the scenario-managed and scenario-baseline files carry each compact's coalition and top-10 suppliers (`[iso, usd, share]`).

**Per strategic compact** (`strategic.compacts[].scenarios.{baseline, managed, partial, decoupling}`):
- `coverage{static, existing, coordinated, postInvestment}`, `lostUsd`, `exposure`, `counts`, `needs`;
- `criticalExternal`, `nonAlliedReliedOn`, `buildHosts`, `buildHostTies`;
- `nodes[[id, status, coordinated, host, tieN]]`.

**Hosts** (`investmentFlows.hosts[]`): `gain`, `before`, `after`, `newCapacityUsed`, `servedUsd`, `hostIncrement`, `type`, `rankBasis`.

**CSV only:**
- `optimized_n_suppliers`, `static_hhi`, `existing_hhi`, `existing_/static_largest_share`, `n_candidates`, `redirectable_supply` (`allied_trade_stress_test.csv`);
- `delta_hhi`, `hhi_before/after`, `largest_share_before/after`, `residual_gap_before/after` per host (`allied_investment_partnerships.csv`).

**Limits for a threat-amelioration module:**
- "Risky exposure after" exists only as the shocked supplier's removal plus `nonAlliedReliedOn` and `criticalExternal`. Non-allied shares of the post-shock supply are not exported.
- `postInvestment` is coordinated + min(0.10, gap) **by construction** (`docs/aib_methodology.md` 16.3, "arithmetic, not a finding").

---

### 4. Scoring, roles, hosts, compact selection, policy alignment

- **There is no composite partnership score.** The AIB uses lexicographic objectives (section 2). The memo dyad fields are all `min()` / `max()` rules, "none is a weighted index" (`docs/partnership_memos.md:95-113`):
  - `trade_fit = min(supplier CP, buyer need)`;
  - `investment_fit` is 1 / 0.67 / 0.33;
  - `compact_fit` is 1 / 0.5 / 0;
  - priority tiers: tier 1 is a "joint bet" (importance >= 0.67, evidence >= 0.5, additionality not low).
  - Relationship states are first-match (`R/partnerships/pm_dyad.R:93` `pm_classify_dyad`): chokepoint >= 0.50, severe >= 0.70, trusted capacity < 0.30, both dependent >= 0.40 (`config/partnership_memos.yml:29-35`).
- **Policy alignment:**
  - In the AIB it is objective 4 and the 5th host tie-break: PAMS intensity similarity plus CAT rating similarity, `components: [pams, cat]` (`aib.yml:194-195`; `R/aib/aib_scores.R:117` `aib_compatibility`). "No geopolitical alignment score enters."
  - In the memos, `policy_alignment_a/b` is the engine's NIPO/PAMS priority percentile, which "never enters the state".
- **Investment hosts** (`R/aib/aib_investment.R`):
  - Eligibility (:29-79), any of:
    - RCA >= 1 on material exports (>= min(USD 50m, 1% of the node's world trade));
    - production share >= 0.01;
    - world share >= 0.005;
    - RCA >= 1 in a materially linked node.
  - `max_hosts_per_node 20`, taken by `host_scale_usd` (`aib_host_shortlist`, host_screen.R:37).
  - The increment is `min(increment, 1.0 x host_scale_usd/D)` (:86-91), with increments `[0.05, 0.10, 0.20]`, headline 0.10.
  - Ranking (`aib_rank_hosts`, :292-329), in order:
    1. scaling hosts first;
    2. gain under the 0.40 cap, tie 0.005;
    3. ΔHHI, tie 0.005;
    4. capability tier and value;
    5. need-weighted Friendshore;
    6. compatibility.
  - Host types (`aib.yml:267-275`): scaling = full increment or >= 5% share; specialty = high income, RCA >= 2, >= USD 100m; development = LMIC/UMIC ranked by the Development Potential Index, never counted toward targets.
  - The alignment cost is the best screened gain minus the best eligible gain, floored at 0 (host_screen.R:49-71).
- **Strategic AIB roles** (`aib_strategic.R:1015` `aib_assign_strategic_roles`; `aib_strategic_layer.yml:74-92`):
  - Seven roles (RPA, STM, SMI, DMA, CIA, DSP, CBH), each with two percentile gates.
  - Primary needs both >= 0.90; secondary both >= 0.75. At most 3 primary and 2 secondary.
  - Positions: anchor >= 0.10; specialist RCA >= 1.5; presence 0.05 / 0.02; momentum +1pt, >= 2%, >= 1.25x.
  - Compact-specific roles come from `aib_compact_country_roles` (:1082). The core countries (at most 6) come from `aib_strategic_core_long` (:1296).
- **Compact formation (AIB-Cxx)** (`aib.yml:300-342`; `aib_compacts.R`):
  - Only intervention nodes are grouped (complete linkage).
  - Three tests: chain coherence within 3 hops; partner cosine >= 0.35; policy-need cosine >= 0.50.
  - Anchors hold 60% of the lost supply (at most 6).
  - The "smallest useful coalition" is the exact GLPK minimum at targets `[0.90, 0.95, 0.99]`, with 0.95 as the headline (`aib_coalition_exact`, :240). It uses `coalition_increment 0.10` at the top-5 scaling and specialty hosts.
  - The type is set at `scripts/32:1007-1009`: `compact unnecessary` if the gain is < `min_compact_gain 0.05`; `bilateral arrangement sufficient` if there are < 3 countries.
  - Bundle: 27 candidate compacts and 1 bilateral; 167 memberships; 7 prospective hosts.
- **Strategic compact selection (SC-*)**:
  - A mission is material when its nodes carry >= `material_share 0.25` of lost supply or >= `material_usd 1e9` (`aib_strategic_layer.yml:46-49`; `aib_strategic.R:534-538`).
  - The UI decision is in `dashboard/src/compact/decide.ts:1-11, 64-70`:
    - material primary mission → "Sectoral Compact";
    - targeted → "Narrower arrangement";
    - no mission → "No compact needed".
  - Bundle: Decoupling 9 sectoral / 1 none; Managed 6 / 3 / 1.
- **"Eight criteria" screen: NOT implemented.** No table exists in R, config or the bundle (`dashboard_redesign/product_redesign.md:36`; `future_analytical_changes.md:39` #19). `CompactSelectionView.tsx:27-35` lists Additionality, Commercial viability, Time to usable supply and **Threat amelioration** as "Not assessed" (also #20).
- **Development partnerships**:
  - The rules are in `R/dashboard/country/country_devpart.R` and `config/development_partnerships.yml`.
  - Donor reasons are in tiers 1-5; the recipient must be Catch Up with a credible pathway and pass the trust screen.
  - Pairs are routed development / commercial / review / excluded and ordered lexicographically (`docs/development_partnerships.md:16-125`).

---

### 5. Red-team cards (consequential judgment calls)

1. **Demand = traded imports only.** Domestic production is invisible, so "coverage" is coverage of traded demand (`aib_trade_model.R:13-17`; doc 15). Example: China's own output never counts against allied dependence. The alternative is apparent consumption from production proxies. This affects exposure, every class and every gap.
2. **Hold semantics as headline** (`aib.yml:115`). China keeps buying, so Myanmar and Laos rare-earth concentrates stay in China. Under release, BUILD falls from 42 to 32 (doc:880). Release or a blend is the alternative. This affects all Decoupling classes.
3. **Existing links may only double** (`existing_relationship_growth 1.0`). COORDINATE counts are 3/2/1/0 at 1.25x/2x/4x/unlimited (doc:883-885), and the two headline COORDINATE nodes are growth-sensitive. The alternatives are a node-specific elasticity or lead-time data. This affects COORDINATE, the dividend and compact archetypes.
4. **Reallocation (diversion from non-scope buyers) as headline.** USD 111bn of recovery is "all drawn from buyers outside the scope" (doc:775-779). Conservative gives BUILD 50. The alternative is a cap on diversion. This affects coverage and the dividend.
5. **BUILD at a residual of 10% / LOW below 15% exposure.** These are round-number bars with no stated calibration. Alternatives: USD-weighted bars, or bars scaled by node criticality. This affects class counts, missions and compact selection.
6. **The host-scale cap multiple is 1** (a host can at most double). It concentrates the host role in DEU and JPN (297 and 259 selected relationships, doc:914). Multiple 2 gives mid-sized hosts (4.9% vs 9.2% median share, doc:392-399). This affects host ranks, coalitions and DSP roles.
7. **Simulated capacity counts toward the coalition target.** Without it, ferrite magnets need 26 members instead of 6 and 5 compacts miss 95% (doc:975-979). The alternative is current supply only. This affects coalition sizes and prospective hosts.
8. **The alignment screen is hard UNGA blocs, demand scope exempt.** It screens KAZ, ARE and EGY but keeps SAU, IND and BRA (doc 15). The alternatives are `tau>0` smoothing, sanctions lists, or Friendshore >= 0.50 (tested, doc:483-487). This affects hosts and compacts (28 vs 31).
9. **The Managed benchmark is the HS4 sibling-line HHI.** Rare-earth oxides read "already diversified" (0.19 vs 0.21) while China holds 84% of refining (doc:752-755). The alternatives are a production-based HHI or a fixed HHI ceiling (e.g. 0.18). This affects the whole Managed scenario and NOT TESTED.
10. **Materiality floor 25% OR USD 1bn.** SC-NUCLEAR becomes a Sectoral Compact on US$0.2bn (`future_analytical_changes.md:41`). The alternative is an AND rule or a USD-only floor. This affects the compact decision.
11. **Physical constraint as headline, applied in only 11 nodes.** Wafers are LOW on trade and BUILD with GCIM (doc:739-748). The alternative is trade-only headline with warnings. This affects the class in 11 nodes and the warnings.
12. **Re-export hubs = HKG/MAC only** (`aib.yml:35-36`). SGP is a core "Specialized technology supplier" in SC-SEMIS, and LUX→ESP and BEL cathode flows are treated as genuine. The alternative is to add SGP, NLD, BEL and ARE, or use origin-adjusted trade. This affects roles, core countries and hosts.
13. **Percentile gates on zero-inflated metrics** (doc:1518-1523). Any holder clears p75. The alternative is ranking among holders, which strips JPN and KOR of their primaries. This affects strategic roles.
14. **Curated compact map (10 SC), not discovered** (doc:1080-1088). AIB-C07 is placed by override. Alternatives are data-driven clustering or a different spine. This affects every strategic output.

---

### 6. Known pathologies and fixes (do not undo)

- **Standardized increment → smallest-host bias.** Fixed with the host-scale cap (doc:385-416, 2026-10-20).
- **Tier-first shortlist** omitted DEU and USA. Fixed with the scale-first shortlist (`aib_host_screen.R:33-36`).
- **Myanmar, Russia and China as hosts.** Fixed with the alignment screen (doc:465-480).
- **Unbounded state B → zero dividend in 89 of 91 nodes.** Fixed with the growth bound (`aib.yml:169-173`).
- **Wide coverage tolerance faked the dividend.** Fixed by `tolerance_coverage 0.0005` (`aib.yml:157-159`).
- **Austria a critical member through simulated capacity alone.** Fixed with `drop_current` criticality and prospective hosts (doc:637-649).
- **Input demand leading compact roles.** Fixed (doc:1316-1321).
- **Ship registries as huge traders.** Excluded (`aib.yml:37-40`).
- **Semiconductor wafers in HS 3818.00.** Handled by the physical constraint.
- **Solar HS2022 cells/modules split.** Flagged (doc:166-167).
- **Insufficient nodes merged upward** (`aib_quality_merge_map`).
- **Lambda knife-edge** (`scripts/30:459-464`, see section 2).
- **Open issues** listed in doc 16.11 (:1488-1523): US "resource supplier" on scrap; AUS/VNM rare-earth metal shares need an HS6 check; theses don't separate the largest pairs.

---

### 7. Cost of re-running for sensitivity

- Everything in `R/aib/` is pure, and state C has a closed form (`aib_optimized_analytic`), so node-level what-ifs are cheap given `aib_inputs.rds` and `aib_checkpoint_trade.rds` / `aib_checkpoint_investment.rds` (present in the outputs dir).
- `scripts/33` reads only the RDS files, so the strategic layer (gates, materiality, missions) is cheap to re-run.
- A full `scripts/32` is expensive: LPs per node x shock x scenario x semantics x mode, plus GLPK coalition solves with `solver_time_limit 10` s (200 s for the minimum size).
- **There is no TypeScript implementation of the optimizer or scenarios.** `dashboard/src/aib/derive/*` only reads exported figures. The UI can vary display rules only (e.g. `decide.ts`). Even the materiality floor is applied in R (the `targeted` flag); its share appears only inside the `basis` string.

---

### Problems noticed

1. **Doc/data drift on the demand scope.** The doc says 57 economies (`docs/aib_methodology.md:244, 860, 1265`). The bundle and `aib_host_screen.csv` have **59**. `run_32.log` (Oct 26) says 57, with "strategic scope electro_industrial (70 of 92 nodes)", which does not match the bundle's `strategicScope: all`, 77 nodes.
2. **Bundle schema is v3; the doc says v2** (doc:836).
3. **Node count conflict inside the doc.** Line 87-88 says "78 nodes, 74 optimized"; lines 856-857 and the bundle say 77.
4. **Managed scenario drift.** The doc says it ran on 38 nodes with 2 COORDINATE (hydro turbines and reactor parts) (doc:827, 1007). The bundle has 37 "diversified to the benchmark", 3 "within the cap" and 1 COORDINATE (`reactor-parts`).
5. **Decoupling rank-1 host count.** The bundle has 50 rank-1 host rows; the doc says 49 nodes simulated (doc:897).
6. **Stale "hosts all tie" claim.** Doc 16.3 (:1212-1215) says every BUILD node ties and no compact names a host. The bundle shows `buildHostTies: 0` and named hosts (SC-MAGNETS `buildHosts [DEU, JPN, THA, USA]`).
7. **Stale "China is a rank-1 host" claim.** Doc 16.11 (:1509) says Managed lists China as rank-1 host. China is screened, and no CHN host rows exist in either scenario file.
8. **Zero-gain rank-1 host.** Managed has `blade-core-wood-balsa` rank-1 host PNG with gain 0 and newCapacityUsed 0, ranked by "scaling host first". A threat module must filter on gain > 0.
9. **Partner scores mostly missing in the network.** Partnership Opportunity and Friendshore are null on 1,701 of 1,934 network edges, 680 of 2,545 investment pairs, and 332 of 785 compact trade rows. These fall to the neutral 0.5 in tie-breaks.
10. **Anchor coverage vs trade coverage.** Anchor coverage reaches 1.0 in many compacts, for example AIB-C02: `anchorCompact 1` vs `trade 0.856`. AIB-C01 has 0.964 vs 0.454. "Compact gain" is largely redistribution within the scope plus simulated capacity (acknowledged at doc 12a), but the UI headline numbers can mislead.
11. **Two different HHIs for the same node.** `baseline.hhi` (scope supply, e.g. ferrite 0.3625) vs `diversify.currentHhi` (world exports, HK/MO merged, 0.3538). The labels don't distinguish them.
12. **Hard-coded threshold.** `aib_production_flags` is called with the default `gap = 0.25` (`scripts/31:329`); it does not read `supply_constraint.divergence_threshold`. The need-profile constants 0.25 and 0.5 are also hard-coded (`aib_compacts.R:46`).
13. **Duplicate function.** `aib_hhi` is defined twice (`aib_trade.R:5`, `aib_diversify.R:8`); the last one sourced wins.
14. **Implausible core countries.** THA "Resource supplier" in SC-MAGNETS; SGP (a likely re-export hub) and BRA as SC-SEMIS core; ESP and POL "Resource supplier" in SC-WIND; USA "Resource supplier" (scrap) in SC-BATTERIES. Only HKG and MAC are treated as hubs.
15. **No single trusted-partner definition.** At least five definitions coexist (coalition tiers, global candidate universe, bloc host screen, allied set + TWN/CHE, memo coercive list incl. RUS). A QC module needs one documented mapping.
16. **Memos are unverified.** Every memo in `public/data/partnerships/manifest.json` has `"verified": false`.
17. **Two compact rules that can disagree.** The AIB-Cxx `min_compact_gain` and the SC-* materiality rule produce independent "compact or not" decisions.
18. **Uncalibrated network design.** The `scripts/30` allied network design still runs `lambda 0.78`, uncalibrated for six stages, and its portfolio caps are expected not to converge (:410-424, 437-444). Its outputs should be treated as unreported.