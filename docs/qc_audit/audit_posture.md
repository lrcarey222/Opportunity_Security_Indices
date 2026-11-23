# Canonical strategic decision framework: subsystem map (Need → Prize → Position → Choice → Policy)

Note on versions. The code in `qc` (eabf5591) and in `stages` (076c7170) applies the same choice rule. The only differences in `decision_engine.R` (checked with `diff --strip-trailing-cr`) are a descriptive deployment-case tag in stages: `SP_DEPLOY_CASES` and `case_watch`, which never change a choice. The stages branch also adds a Competitive Position family, `innovation_position` (stages `config/strategic_position.yml` ~81-94), and a current-trade import blend (~144-164). Neither exists in qc. So `stages/data/processed/outputs/strategic_position_tbl.{rds,csv}` (2026-11-17, 14,454 rows, 219 economies) comes from stages code, not qc code. Its counts are Secure 212, Preserve 111, Catch Up 351, Partner 1,475, Accelerate 514, Watch 6,574, not assessed 4,887. The docs and `qc/diagnostics` quote an older run: 206 / 103 / 355 / 1,474 / 514 / 6,585 / 4,887. Every example cell below comes from the stages table.

## 1. Canonical labels: code keys, then display

**Code (R).** These are in `R/strategic_position/decision_engine.R:43-53`:
- `SP_CHOICES = c("secure","preserve","catch_up","partner","accelerate","watch","not_assessed")`
- `SP_CHOICE_POSTURES = c("secure","compete","deploy","not_assessed")`
- `SP_DEPLOY_MODES = c("accelerate","watch")`
- `SP_CHOICE_COMPETE_MODES = c("preserve","catch_up","partner")`
- Verb → posture map `SP_CHOICE_VERB_POSTURE` (secure→secure; preserve, catch_up, partner→compete; accelerate, watch→deploy).

Flags are `review_strong_position_low_prize` and `review_top_quarter_below_floor` (line 51). Output columns:
- `strategic_choice`, `strategic_posture`, `compete_mode`, `deploy_mode`
- `posture_variant`: compete_preserve, compete_catch_up, compete_partner, deploy_accelerate, deploy, deploy_floor, deploy_diversify, and secure_* from the engine (lines 314-322)
- `decision_step`: secure_override, deployment_need, large_yard, triage, not_assessed (lines 343-345)

**Yards.** `yard_rollup.R:30-45` maps choices to yards:
- small = secure
- medium = preserve, catch_up, partner, and accelerate at Deployment
- large = watch
- not_assessed → NA

Fences follow `SP_YARD_FENCE` (small=high, medium=medium, large=low). Roll-ups are `mixed_high_medium`, `mixed_medium_low`, `mixed_high_low` and `mixed_high_medium_low` (`yard_rollup.R:31,48-54`). In the bundle the yard codes are S/M/L/SM/ML/SL/SML (`meta.yard_codes`).

**Dashboard types.** In `dashboard/src/posture/contract.ts`:
- `PostureCode 'S'|'C'|'D'|'N'` (:35)
- `PostureWord` (:39)
- `CompeteMode`/`DeployMode` (:40-41)
- `Verb` = secure plus the five modes (:43). `not_assessed` is not a Verb.
- `Pathway 'yes'|'no'|'unknown'` (:47)
- `Alignment` (:49)
- `Yard` / `YardRollup` (:59-61)

**Display.** In `dashboard/src/content/vocabulary.ts`:
- `CHOICE_NAME` (:243-250): **Secure, Preserve, Catch Up, Partner, Accelerate, Watch**. VOCABULARY.md:14 says these names "do not change". `posture/grammar.ts:58` sets `VERB_LABEL = CHOICE_NAME`.
- `POSTURE_LABEL` (grammar.ts:45): S 'Secure', C 'Compete', D 'Deploy', N 'Not assessed'.
- **"Keep open" is never a choice name.** It appears only in:
  - `CHOICE_ACTION.watch` 'Keep open: buy competitively, do not force localization' (:259)
  - `FENCE_TITLE.low` 'Low fence · Keep open' (:325)
  - `STRATEGY_STEP.choice.plain` '…Watch (keep open).' (:385)
  - the glossary term 'Watch (keep open)' (`glossary.ts:734-741`)
  - `country/derive/strategyPage.ts:36`
- Other Watch wording: `CHOICE_PROSE.watch` 'leave open' (:282), `WATCH_PROSE_LABEL` 'Low domestic priority' (:286), and `WATCH_DEPLOYMENT_DEFINITION` (:288-289).
- `PARTNER_LONG_NAME` 'Partner / Diversify' (:291).
- `STATE.notAssessed` 'Not assessed: the available data are not strong enough to make a strategic call.' (:623).
- Yards and fences:
  - `YARD_NAME` 'Small yard' / 'Medium yard' / 'Large yard' (:297-301)
  - `FENCE_NAME` (:302)
  - `YARD_AND_FENCE` 'Small yard / High fence', 'Medium yard / medium, permeable fence', 'Large yard / Low fence' (:304-308)
  - `FENCE_TITLE`: 'High fence · Secure', 'Medium fence · Build, preserve, partner and deploy', 'Low fence · Keep open' (:322-326)
  - `FENCE_OF_VERB` (grammar.ts:116)
- Step names (`STRATEGY_STEP` :361-393): Need = 'Strategic Need', Prize = 'Strategic Industrial Value', Position = 'Competitive Position', Choice = 'Strategic choice', Policy = 'Policy alignment'.
- `MEASURE_PLAIN.supplyChainNeed` = 'Supply-chain vulnerability' (:397).
- Alignment display (`ALIGNMENT_NAME` :420-427): aligned 'Broadly aligned', under_prioritized 'Needs more attention', over_prioritized 'Too much intervention', misaligned 'Wrong tool for the problem', mixed 'Mixed signals', insufficient_data 'Not enough policy evidence'. deployment_gap is shown as 'Needs more attention: deployment policy lags' (:429).
- Secure rationales (`RATIONALE_NAME` :443-447): operational_control 'Control-system risk', coercive_chokepoint 'Chokepoint risk', systemic_resilience 'System resilience'.
- The separate Draw the Fence tool uses capitalised 'Small Yard'… plus 'Mixed' and 'Indeterminate' (`dashboard/src/fence/engine/classify.ts:30-36`).

## 2. Decision rules and thresholds

All thresholds are inclusive ("high = at or above"; `docs/strategic_position.md:106-107`). They live in `config/strategic_position.yml` under `decision_framework` (line 416). Defaults are hard-coded again in `sp_choice_thresholds()` (`decision_engine.R:57-62`). All percentiles are mid-rank `(rank-0.5)/n` (`sp_helpers.R:6`).

| constant | value | config line | used at |
|---|---|---|---|
| `need_threshold` | 0.67 | 417 | decision_engine.R:162 |
| `prize_threshold` | 0.6 | 418 | :90, strategic_value.R:85 (cap) |
| `cp_preserve_pct` | 0.75 | 419 | :153 |
| `deployment_threshold` | 0.6 | 424 | :170 |
| `borderline_band` | 0.05 | 425 | :224 |
| `thresholds_sensitivity` | need [0.60,0.75]; prize [0.50,0.55,0.65,0.70]; cp [0.67,0.85]; deployment [0.55,0.67,0.75] | 426-430 | scripts/26:742 |
| `contenders` | {min_share 0.005, min_n 5} | 433 | :119-148 |
| `capability_floor` | [moderate, strong] | 436 | :153 |
| `self_supply_share` | 0.10 | 437 | supply_chain_need.R:35 |
| `trusted_alternatives_min_coverage` | 0.8 (observed 0.946 → 4th family on) | 441 | supply_chain_need.R:52-54 |
| `supply_chain_need.score_floor` | 0.10 | 447 | supply_chain_need.R:71-78 |
| IO base/horizon/scenario | 2024 / 2035 / `stated`; minerals 2025 | 453-457 | industrial_opportunity.R |
| `outlook` | declining_below -0.05, growing_from 0.05 | 458 | |
| `caps` | declining 0.33, oversupplied 0.5 (`oversupplied_nodes: []`) | 462-463 | |
| `investment_momentum` | latest [2024,2025] vs [2021,2022], min_baseline 0.5bn, weight 0.25 | 473-476 | |
| `home_market_mode` | lift (bases [national]) | 479-483 | |
| `strategic_value.prize_measure` | strategic_value | 496 | decision_engine.R:66-68 |
| `strategic_value.weights` | market 1, commons 1, option 1 | 499 | strategic_value.R:76-78 |
| `strategic_value.gate` | 0.5 | 504 | strategic_value.R:226 |
| `cap_margin` | 0.01 (cap = 0.59) | 506 | |
| `require_market` | true | 509 | |
| `large_yard.enabled / required` | true / [mature, passive] | 517 / 520 | |
| `large_yard.min_signals` | 5 (of 8 others) | 526 | |
| `diversified_top_share_max` | 0.5 | 532 | |
| `leap_veto` | true | 537 | |
| `learning_veto_points / learning_floor_points` | 2 / 1 | 545-546 | |
| `position_first` | true | 552 | decision_engine.R:166 |
| `pathway.rule` | anchor | 558 | pathway.R |
| `direct_share_min` | 0.005 | 560 | |
| `operating_capacity_share_min` | 0.005 | 561 | |
| `committed_pipeline_share_min` | 0.01 | 562 | |
| `adjacent_share` | 0.05 | 563 | |
| `announced_pipeline_share_min` | 0.02 | 565 | |
| `adjacent_cp_within_pct` | 0.6 | 566 | |
| `feasibility_pct` | 0.75 | 567 | |
| `learning_levels` | [moderate, strong] | 568 | |
| `input_cost_pct` | 0.75 | 569 | |
| `min_legs / pipeline_min` | 2 / 0 (legs rule only) | 571-572 | |

**Rule.** This is the pure function `sp_choice_rule()` in `decision_engine.R:160-182`:
1. Secure (the engine's `strategic_posture == "secure"`) → `secure`.
2. Deployment: `deployment_need` NA → not_assessed; ≥ 0.6 → accelerate; otherwise → watch.
3. Producing stages: if `supply_chain_need` or the prize is NA → `not_assessed`. This check comes before the large-yard case, so a missing input is never Watch.
4. Large yard: `large_yard_case` TRUE and NOT (`position_first` AND cp_high) → `watch`.
5. Triage:
   - hp & ch → preserve
   - hp & pathway yes → catch_up
   - hp & hn → partner
   - hp → watch
   - hn & ch → preserve, flagged `review_strong_position_low_prize`
   - hn → partner
   - otherwise → watch

Definitions:
- **High prize:** `prize >= 0.6` AND `siv_cap != "market_only"` (`:90`).
- **cp_high:** `cp_contender_pct >= 0.75` AND (`cp_contender_basis=="all_countries"` OR `cp_is_contender`) AND `t3_capability_lo %in% c("moderate","strong")` (`:152-154`).
- **Contender:** measured world share (`home_share_exposure`, falling back to `home_share`, `:106-109`) ≥ 0.005. With fewer than 5 contenders the cross-country percentile stands in (`:134-140`).

One consequence: **low prize + low need + top-quarter position = Watch.** Preserve requires a high prize or high need.

**Flag rules** (`:279-283`):
- `review_strong_position_low_prize`: preserve & !hp & hn & cp_high.
- `review_top_quarter_below_floor`: catch_up/partner, top quarter among contenders, but not cp_high.

`greenfield_option` is hp with no cp_high and no anchor, decided Partner or Watch (`:357-358`).

**Borderline and confidence** (`:223-301`):
- An input within 0.05 of its threshold, a large-yard case on exactly `min_signals`, or a case that fails by one signal (`large_yard_near`) is recorded in `decision_borderline`. Each is flipped to compute `choice_possible`, `posture_possible` and `pivotal_inputs`.
- Confidence counts issues: 0 → high, 1 → moderate, ≥2 → low. not_assessed is always low. Secure keeps the engine's confidence.
- Issues counted:
  - each borderline input (whether or not flipping it changes the choice)
  - a Secure that is still possible
  - an unknown pathway that decides the cell
  - an unmeasured CP where the rule reads it
  - low CP evidence on Preserve, Catch Up or Partner
  - a flag
  - fewer than 2 need families
  - fewer than 2 deployment-need families at Deployment

**Need floor** (`supply_chain_need.R:71-78`): if the uncapped percentile is ≥ `need_threshold - band` (0.62) and `supply_chain_need_score < 0.10`, the need is set to `min(pct, 0.67-0.05-0.01)` = **0.61** and `supply_chain_need_cap = "score_below_floor"`.

**Market-only cap** (`strategic_value.R:226-234`): if neither commons nor option is ≥ 0.5 (an Unknown counts 0), `strategic_industrial_value = min(pct, 0.59)` and `siv_cap = "market_only"`.

**Pathway, anchor rule** (`pathway.R:190-261`):
- Anchors:
  - `direct_supply_share` ≥ 0.005 (measured share)
  - `direct_export_share` (`trade_benchmark_share`) ≥ 0.005
  - `operating_capacity` ≥ 0.005
  - `committed_pipeline` ≥ 0.01
  - `adjacent_stage_share` ≥ 0.05
  - `resource_base` (Extraction only)
- Supporting signals: feasibility ≥ 0.75, input_cost ≥ 0.75, `adjacent_stage_relative` ≥ 0.6, learning ∈ {moderate, strong}, `announced_pipeline` ≥ 0.02.
- Evidence families (`SP_PATHWAY_FAMILY` :62-65):
  - direct (both share anchors)
  - capacity
  - pipeline (committed and announced)
  - adjacency (absolute and relative)
  - resource
  - feasibility
  - input_cost
  - learning
- yes = any anchor AND at least 2 distinct families fired (`:225`).
- Extraction with no resource base → no; resource base unknown → unknown.
- Basis codes: `no_anchor`, `anchor_unobserved`, `anchor_uncorroborated`, `corroboration_unobserved`, `no_evidence`, `no_resource_base`, `resource_base_unknown`. An unknown pathway counts as not credible.

**Secure override (posture engine)**, documented at `docs/strategic_position.md:754-851`; config `strategic_posture` from line 255:
- Test 1, critical consequence: exposure lines 1e6 USD, 0.0005 of GDP, home share 0.005, deployer pct 0.5 (287-290).
- Digital vendor share {0.25, marginal 0.15} (294).
- Coercion {0.5, marginal 0.4} (298); coercive import share {0.25, 0.15} (299).
- Self-supply 0.10 (316).
- Test 3 position {strong 0.10, moderate 0.03, foothold_within_country_pct 0.5} (321).
- `catch_up_market_pct` 0.75 (333); climate cost GHG < 0.5 (339).
- Premium {10, 30} (346); allied alternative 0.5 (347).
- Delivery {0.10, 0.03, 0.3, 0.5} (349); concentration watch {coercive 0.4, HHI 0.18} (352); confidence {0.75, 0.4} (355).
- Dual-use {0.25, 0.15} (361).
- Routes are curated per node in `config/security_route_rules.csv`.
- The TS twin of these screens is `dashboard/src/fence/engine/rules.ts:6-74`: the same 0.5/0.4 coercion, 0.10/0.03 position, 10/30 premium, 0.75/0.4 confidence.

**Policy alignment** comes after the choice and never feeds it (config 592-608):
- `matched_min` 0.5, `over_min` 0.67, `source_gap_mixed` 0.5
- `fence_without_build` {protection ≥ 0.67, support ≤ 0.33}
- `partner_autarky` protection ≥ 0.67
- `deployment_gap_max` 0.5
- The rule table is at `strategic_position.md:892-905`.

**Policy-design fence height** follows the yard. `policy_design.R::ppd_read_check()` enforces it: secure_* high; compete_* and deploy_accelerate moderate; deploy, deploy_floor and deploy_diversify low (`strategic_position.md:1022-1026`).

## 3. How each component score is computed

**Need, producing stages** (`supply_chain_need.R:33-84`). `supply_chain_need` is a within-cell mid-rank percentile across countries of `supply_chain_need_score`, which is the mean of the available families:
- `sn_import_dependence`: net imports of the stage basket / GDP; fossil self-sufficiency
- `sn_supplier_concentration`: own import HHI
- `sn_risky_supplier_exposure`: China+HKG+MAC and opposite-bloc share
- `sn_trusted_alternatives`: within-cell percentile of 1 - (trusted world share - own share). It is included only when coverage ≥ 0.8; the actual coverage is 0.946.

Each family is the mean of its measures' within-cell percentiles (`strategic_need.R`). Further rules:
- The composition families are zeroed when home share ≥ 0.10 (`:61-62`) and for immaterial importers (< USD 1m).
- Thin cells (< 5 material importers) and entrepots (HKG, MAC, MHL, LBR, PAN) are NA (`strategic_position.md:125-131,158-162`).
- Deployment rows have NA need. The Assembly proxy there is legacy only.
- Year: import exposure comes from scripts/15, last trade year. The stages branch blends in the trailing 12 months of current trade.

**Need, Deployment** (`deployment_need.R:770-860`; `config/deployment_need.yml`):
- Measures are oriented, then percentile-ranked within the technology.
- Family = percentile of the mean of its measure percentiles. Families:
  - `dn_demand` (current_policy)
  - `dn_policy_gap` (national_commitments, Ember 2030 targets)
  - `dn_system` (current_policy)
- `deployment_need` = mean of the observed family percentiles (`:842`).
- `dn_confidence` by families observed: high 3 / moderate 2 / low 1 (yml:229-232).
- Scenario base 2024 → 2035 (yml:42-43). `dn_climate_aligned` is display-only.
- `deployment_need_pct` exists but the rule does not read it.

**Prize** (`strategic_value.R:209-239`):
- `siv_score` = weighted mean (1:1:1) of three families:
  - market = `industrial_opportunity`
  - commons = (process_knowledge + workforce_suppliers + adjacent_innovation, scored yes 1 / partly 0.5 / no 0 / Unknown 0) / 3
  - option = the sum of four leap tests / 4, and only if `leap_candidate` = yes, else 0
- `strategic_industrial_value` is the mid-rank percentile of `siv_score` across all rows with a score (scripts/26:449 passes no `ref`). Layers are placed among the cells' scores.
- Commons and option are **node attributes** (`data/reference/posture_node_attributes.csv`), so the prize varies by country only through the home-market lift. 48% of producing cells are lifted (`io_home_market_scored`).
- `industrial_opportunity` (`strategic_position.md:252-259`):
  - node prize = (1-0.25)·growth pct + 0.25·capex-momentum pct where momentum is observed (10 nodes)
  - growth pct = 2024→2035 world demand growth, WEO 2025 STEPS (`stated`), ranked across 55 producing nodes; minerals use the Critical Minerals Outlook 2025→2035
  - declining markets are capped at 0.33
  - lifted to mean(node prize, national home market) where the home market is higher
- `SP_IO_FORBIDDEN` and `SP_SIV_FORBIDDEN_PATTERNS` (`strategic_value.R:58-59`) refuse CP, capability, share, pathway and need columns in the prize.

**Large-yard case** (`strategic_value.R:262-394`; `strategic_position.md:299-339`). There are ten signals. `mature` and `passive` are required. Of the other eight (standardized, substitutable, easy_qualification, diversified, low_learning, low_spillover, low_differentiation, commoditized), at least 5 must hold. Vetoes: a credible leap, or commons points ≥ 2. A case with commons points ≥ 1 keeps a capability floor (`deploy_floor`). Market size is never a signal.

**Position** (`competitive_position.R:217`; `strategic_position.md:413-423`):
- Families:
  - export_position: world export share + RCA on the 2022-24 Comtrade benchmark, 85% structural + 15% trade momentum (config 112)
  - production_position
  - manufacturing_capacity: GCIM operating plants
  - investment_position: GCIM capex
  - cost_position: LCOE, Deployment only
  - resource_endowment
  - deployment_scale
- Score = mean of the observed families. `competitive_position_percentile` is cross-country within the cell; no percentile below 5 scored countries.
- The choice reads `cp_contender_pct`, the score placed among contenders (`decision_engine.R:93-98`).
- Recovery has no CP: it is NA and treated as not strong.

**Stages.** Six stages: Extraction, Refining ("Refining & Processing"), Components ("Component Manufacturing"), Assembly ("Final Assembly & Equipment"), Deployment ("Deployment & Use"), Recovery ("Recovery & Recycling") (`dashboard/src/lib/stages.ts:32,75-82`; `strategic_position.md:55-57`). Upstream / Midstream / Downstream is a legacy roll-up only.

## 4. The most consequential judgment calls (red-team cards)

1. **Low prize + low need overrides a top-quarter position → Watch** (`decision_engine.R:180`).
   - Stated rationale: Watch is a positive conclusion (`strategic_position.md:519-523`).
   - Example: AUS Batteries Extraction (lithium) is Watch. cp_contender 0.864, capability strong, need 0.115, prize capped at 0.590 (market 0.78). The trace shows position "high" and then a watch choice. USA, DEU and JPN Electric Grid Components are also Watch (cp 0.914, 0.948, 0.845; SIV 0.467).
   - Alternatives: Preserve whenever cp_high, or a "Preserve-for-review" flag as in the D2 case.
   - Affects: Watch and Preserve counts, Low fence, the Country "Keep open" section.
2. **Market-only gate 0.5 with Unknown = 0, applied to node attributes** (config 504; `strategic_value.R:226`).
   - Rationale: "market size alone never makes a prize" (`strategic_position.md:210-222`).
   - Effect: every Extraction stage row is capped (1,627 of 1,627 assessed Extraction cells; Batteries Extraction is partly/partly/no = 0.33), so **no Extraction cell can be Catch Up**. Solar Components and EV Extraction sit at 0.590 for every economy.
   - Example: IND Solar Components is Partner with cp 0.948.
   - Alternatives: gate 0.33; a mean of observed attributes (18 vs 39 nodes capped); Unknown counted as 0.5.
   - Affects: the prize, Catch Up, Watch, `greenfield_option`.
3. **Prize thresholding is node-level in practice.** Electric Grid Assembly SIV = **0.613 for all 219 economies**, 0.013 over the bar. Green Hydrogen Refining is 0.632.
   - Example: JPN Electric Grid Assembly is Catch Up and borderline on need, prize and cp (`choice_possible` preserve|catch_up|partner, confidence low).
   - Alternatives: a prize threshold of 0.65 (69 changes); a within-node or within-country prize.
   - Affects: whole nodes flip together.
4. **Inclusive cp ≥ 0.75 with discrete contender ranks.** 15 triage cells sit at exactly 0.750, and 8 of them are Preserve.
   - Example: IND Solar Refining is Preserve at exactly 0.750, which also bypasses a 6-of-8 large-yard case (position_first). It is flagged `review_strong_position_low_prize`.
   - Alternatives: a strict > threshold, a minimum contender count, cp 0.67 or 0.85 (32 / 40 changes).
5. **position_first = true** (config 552).
   - Rationale: the paper's gate order.
   - Effect: `position_last` changes 5 cells (`large_yard_sensitivity.csv`).
   - Example: IND Solar Refining, USA bess-enclosure (Medium, not Large).
6. **Large-yard min_signals = 5 and the diversification line at 0.5.**
   - min_signals 4 / 6 change 270 / 160 choices (stages run).
   - PV modules sit on exactly 5 (`yf_paper_reproduction.csv` margin 0).
   - 648 large-yard decisions sit on exactly 5 signals; 774 cells are `large_yard_near`.
   - Batteries and EV Extraction split on world_top_share 0.5.
   - Alternatives: grouped signal families. Three of the eight signals are facets of one property (`strategic_position.md:341-345`).
7. **Need floor 0.10, implemented as a cap at 0.61** (config 447; `supply_chain_need.R:75-77`).
   - Example: ARG Nuclear Refining is Watch (score 0.07, uncapped percentile 0.83). CAN, DEU, NLD, NAM and GBR Nuclear Refining are Catch Up. 38 cells are capped.
   - Alternatives: floor 0.05; an absolute-score need; a ratio to the node median.
8. **Need = a within-cell percentile across ~219 economies.** That makes "high need" relative (the top third by construction) rather than absolute dependence.
   - Example: KOR Batteries Extraction 0.998; AUS Electric Grid Extraction 0.850 (Watch via large yard).
   - Alternatives: an absolute import-dependence share; a peer-group percentile.
9. **The self-supply guard** zeroes three composition families at home share ≥ 0.10 (`supply_chain_need.R:61-62`).
   - Alternatives: scale with share; keep risky-supplier exposure.
10. **Anchor + corroboration pathway** (config 558-572).
    - Catch Up counts by rule: legs_1 1,942, legs_2 919, anchor 351 (`catch_up_rule_comparison.csv`).
    - At Extraction, `resource_base` is both an anchor and a requirement, so resource base + input cost would qualify.
    - Example: USA Batteries Refining is Catch Up on 5 anchors.
    - Alternatives: the legs rule; requiring an anchor at the node itself.
11. **Deployment need = a mean of 1-3 family percentiles, threshold 0.6** (config 424; `deployment_need.R:842`).
    - Rationale: matches the selectivity of the 0.67 percentile (`decision_framework_plan.md:89`).
    - 137 Accelerate cells rest on one family.
    - Thresholds 0.55 / 0.67 / 0.75 change 155 / 142 / 269 choices.
    - Alternatives: `deployment_need_pct`; requiring ≥ 2 families.
12. **Equal SIV weights 1:1:1** (config 499). The rationale is "neutral prior". There is no sensitivity output for the weights: `strategic_value_vs_market.csv` compares prize measures only.
13. **Home-market lift, not mean** (config 483). It raises the prize but never lowers it.
    - Example: USA and DEU Batteries Assembly SIV = 1.000.
    - Alternative: `mean`, which is in config but has no diagnostic.
14. **Contender line 0.5% of measured world supply; capability floor = Test 3 lower bound ≥ moderate** (config 433, 436, 321). Examples: IND Solar Components and the 6 `review_top_quarter_below_floor` cells.
15. **Need-alone Partner sits in the Medium fence** (user decision 8, `decision_framework_report.md:1137-1138`), whereas the paper puts dependence inside the Large yard.
    - Example: CHL Batteries Extraction is Partner (need 0.718, prize capped).
16. **Control layers are Small only where coercive control is measured for that country.** Grid SCADA, inverter firmware and reactor controls are Large for USA and DEU and Small for JPN (`strategic_position.md:692-698`; `yf_paper_reproduction.csv`).
17. **Prize scenario** is WEO STEPS growth 2024→2035 (config 455), with capex momentum at 0.25. The plan says weight 0.5 overweighted the EV capex dip (`decision_framework_plan.md:90`). Doc limitation: a stock-vs-flow mismatch, Solar Refining 0.20 vs Components 0.85 (`strategic_position.md:287-290`).
18. **Missing → not_assessed** for 4,887 cells, which is 4,216 producing cells (18 nodes in every economy, plus thin cells). Recovery is 1,947 of 2,354 not assessed (`choice_by_stage.csv`).

## 5. What can be re-run cheaply

- **Pure, from stored columns.**
  - `sp_choice_rule(stage, secure, need, prize, cp_high, pathway, dn, th, large, capped)` (`:160`)
  - `sp_choice_rerun(tbl, cfg)` (`:570-578`): takes a modified `decision_framework` list and the final table.
  - `sp_choice_threshold_sensitivity(tbl, cfg, grid)` (`:545-565`): one parameter at a time, no joint grid, does not recompute borderline or confidence.
  - `sp_choice_measure_comparison()` (`:584`)
  - Required columns: `supply_chain`, `engine_posture`, `supply_chain_need`, `strategic_industrial_value`/`industrial_opportunity`, `siv_cap`, `deployment_need`, `cp_contender_pct`, `cp_contender_basis`, `cp_is_contender`, `t3_capability_lo`, `pathway_credible`, `large_yard_case`, `large_yard_near`, `strategic_choice`. All are in `strategic_position_tbl.rds`.
- **Needs node attributes.** `sp_large_yard_case(tbl, nodes, cfg)` and `sp_strategic_value(tbl, nodes, cfg)` take the resolved `posture_node_attributes`, as in scripts/26:449-450 and 770-779.
- **Pure, recomputes from stored columns.** `sp_supply_chain_need(tbl, cfg)` reruns the score floor and coverage from the `sn_*`, `home_share` and `trusted_world_share` columns.
- **Needs side tables.** `sp_pathway()` needs pipeline, covered, input-cost and capacity tables. The stored `pw_*` columns would support a thin re-threshold wrapper, but none exists.
- **Not cheap.** The Secure engine (`sp_strategic_posture`), CP weights (scripts/26:716-731 already reruns the choice for momentum weights) and the Deployment-need inputs (scripts/95).
- **No TypeScript equivalent of the choice rule.** The dashboard reads choices from the bundle. It uses thresholds only for display: `CellDrawer.tsx:196,235`, `sections.ts:53,106` (`prizeQuadrant`, "tendencies, not rules"), `PostureScatter.tsx:25`. The bundle exports only need, prize, cp, deployment and borderline_band (`R/dashboard/posture/posture_export.R:248`). `dashboard/src/fence/engine` (gates.ts, classify.ts) is the paper's Yards & Fences engine, a separate rule set.

## 6. Existing review and diagnostic outputs

All are in `diagnostics/strategic_position/`, written by scripts/26:738-801 and scripts/27.
- `decision_review.md` (spec XXI), with sections: Distribution; 1 Legacy vs new; 2 IO validation (Spearman with CP 0.095 pooled, 0.38 within node); 3 Position-choice consistency; 4 Deployment review; 5 Secure selectivity (212 Secure; 198 high-need; 8.3% of high-need cells); 6 Watch with high policy intensity.
- `choice_cards.md` covers JPN, USA, DEU, KOR, IND and BRA. `decision_cards.md` holds the engine's test cards.
- Threshold sensitivity, stages run:
  - need 0.60/0.75 change 308/367 cells
  - prize 0.50/0.55/0.65/0.70 change 56/30/71/83
  - cp 0.67/0.85 change 29/48
  - deployment 0.55/0.67/0.75 change 155/142/269
- `large_yard_sensitivity.csv`, `strategic_value_vs_market{,_transitions}.csv` (strategic value alone 230, large yard alone 593, both 795), `catch_up_rule_comparison.csv`, `trade_momentum_weight_sensitivity.csv` (weight 0 → 22 choice changes).
- `consistency_before_after.csv`, stages run: Partner with contender cp ≥ 0.75: 15 → 4; Catch Up with cross-country cp ≥ 0.9: 141; Preserve without capability: 63 → 0.
- `yf_paper_reproduction.csv` (`sp_yf_reproduction`, yard_rollup.R:212): `agree` 7 / differ 12 / no paper placement 3; USA agrees 10 / differs 9; carries `large_yard_margin`.
- **Margin lists.** `decision_borderline`, `choice_possible`, `pivotal_inputs` and `large_yard_n` / `large_yard_near` on the table give the threshold-adjacent cells. Stages counts:
  - borderline: large_yard_case 1,114; supply_chain_need 387; strategic_industrial_value 338; deployment_need 257; cp 46; plus combinations
  - triage cells within ±0.02 of a threshold: need 188, prize 615, cp 37
- No ranked "closest to flipping" list exists.

## Problems noticed

1. **The threshold sensitivity ignores the need-floor cap.** Capped need is stored as 0.61 (`supply_chain_need.R:77`) and `sp_choice_inputs()` (`decision_engine.R:84`) does not read `supply_chain_need_cap`. At need = 0.60, all 13 triage-decided capped Watch cells (Nuclear Refining in ARG, GRC, ISR, ITA, LTU, NOR, SGP, CHE, TZA, THA; Nuclear Extraction in CAN, KAZ, NAM…) flip to Partner or Preserve. These are artifacts inside the reported 306/308 changes. The prize cap is handled as a flag; the need cap is not.
2. **The reference data does not match the qc code.** The stages outputs include `cp_innovation_position` (1,274 non-NA) and current-trade blending that qc lacks. The qc posture bundle is dated 2026-10-31 (4bb2adc); the stages bundle is 2026-11-17 (3760baae). `qc/diagnostics/strategic_position/*` has 2026-11-20 mtimes but the 2026-10-30 content: its `choice_overall.csv` differs from stages.
3. **Scenario labels conflict.** The prize uses WEO STEPS (`stated`, config 455), but `methods.md:8` and `strategic_position.md:254` call it "current policy". Deployment-need demand and system families use WEO Current Policies (`deployment_need.yml:51,59,174-176`). So the prize and Deployment Need read different scenarios.
4. **Stale doc.** `strategic_position.md:670-672` says the drawer still labels the prize "Industrial Opportunity". `CellDrawer.tsx:174` now branches on `prize_measure`.
5. **Confidence counts borderline inputs that cannot change the choice.** Example: AUS Batteries Extraction is borderline on large_yard_case, its `choice_possible` is only "watch", yet its confidence drops to moderate (`decision_engine.R:295`).
6. **`methods.md:8` simplifies the large-yard bypass.** It says the bypass applies if the country "holds a top-quarter position". The code also requires contender status and the capability floor (`:152-154,166`).
7. **Doc misdescribes Extraction attributes.** `strategic_position.md:219` says every Extraction row is capped because its learning attributes are "Unknown or low". The attributes are partly/partly/no (0.33). Only Geothermal is Unknown.
8. **Inert calibrations and dead paths.**
   - `learning_veto_points` / `learning_floor_points` change 0 cells.
   - The `few_contenders` / `all_countries` CP bases never occur among assessed producing cells: all are `contenders` or NA (545).
   - `oversupplied_nodes: []`.
   - `market_outlook_override: {}`.
9. **Rank reference is ambiguous.** The SIV percentile ranks every row with a score, including need-unassessed cells and possibly the five aggregates (scripts/26:449, no `ref`). The doc says "across the assessed producing-stage cells" (`strategic_position.md:207`).
10. **Counts drift across docs.** `decision_framework_plan.md:49` says not assessed is 4,624; the other docs say 4,887. The `consistency_before_after.csv` "after" label still reads 2026-10-29.
11. **Casing differs** between the Draw the Fence 'Small Yard' (`classify.ts:31`) and the Explorer 'Small yard' (`vocabulary.ts:298`).
12. **The bundle carries 5 of the thresholds.** score_floor, gate, min_signals and the pathway minima are missing from it (`posture_export.R:248`), so the UI can quote them only through generated reasons.
