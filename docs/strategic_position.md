# Strategic Position: the canonical decision framework

Branch `feat/supply-chain-stages`, 2026-10-29; updated 2026-10-30 for the capability-anchor Catch Up rule
(ea6089a, d65ae03), the Atlas 2024 data and basket change (56e9271, 42dd358), the need floor (26d0131), fences by
yard (37f8f13) and the Yards & Fences tightening (315cff0), all rerun together (scripts 26 and 27 on a54a9d3; bundles
and diagnostics e41cb5b). Counts are from that run unless labelled "two-leg run" (the 2026-10-30 rerun of 7d847ba on
the old data). The framework is **Need -> Prize -> Position -> Choice -> Policy alignment**, per country x
technology x six-stage node. Results and validation are in
[strategic_position_readout.md](strategic_position_readout.md) and
`diagnostics/strategic_position/decision_review.md` (spec Part XXI); the per-country choice cards are in
`diagnostics/strategic_position/choice_cards.md`; the implementation plan is
[decision_framework_plan.md](decision_framework_plan.md). The old Energy Security x Economic Opportunity
framework (Grow / Upgrade / Exposed / Stable, the Strategic Index) is kept only for replication and
comparison: see [Legacy OSI framework](#legacy-osi-framework) at the end.

**Yards & Fences tightening (merged 315cff0, 2026-10-30).** The Prize is now **Strategic Industrial Value**
(market, industrial-commons and future-option value, with a market-only cap); a **large-yard case** (mature,
passive, commodity nodes) is tested before the prize; **sensitive layers are yard units of their own**, rolled up
to technology x stage (mixed where they differ); and the policy-design fence height follows the yard. See sections
2, 4 and 7. Current run: Secure 206, Preserve 103, Catch Up 355, Partner 1,474, Accelerate 514, Watch 6,585, not
assessed 4,887; yards by cell small 181, medium 1,809, large 6,051, mixed 1,196.

The unifying question:

> What does this country need, where is there a prize worth pursuing, where does it already have an
> advantage, and therefore what should it Secure, Preserve, Catch Up in, rely on Partners for, Deploy
> faster, and deliberately not prioritize? And does its current policy match those choices?

## The decision chain

| Step | Question | Output (per country x technology x six-stage node) |
|---|---|---|
| **Need** | What problem does the country need to solve? | `supply_chain_need` at Extraction, Refining, Components, Assembly and Recovery; `deployment_need` at Deployment & Use; `need_type` (supply_chain / deployment) and the family scores. Never combined into one score. |
| **Prize** | Is domestic productive capability in this node strategically valuable? | `strategic_industrial_value` (0-1: market, industrial-commons and future-option value; `siv_*`), built on the market prize `industrial_opportunity` (`io_node_prize`, `io_investment_momentum`, `io_home_market`); and the countervailing `large_yard_case` |
| **Position** | How strong is the country today? | `competitive_position_percentile` (cross-country), `cp_contender_pct` (among the node's contenders), the seven families and trade momentum |
| **Choice** | What strategic posture follows? | `strategic_posture` (secure / compete / deploy / not_assessed), `compete_mode` (preserve / catch_up / partner), `deploy_mode` (accelerate / watch), `security_rationale`, `delivery_mode`, `decision_trace`, `posture_confidence`, `decision_borderline`, `consistency_flag`; per cell and per sensitive layer, with the `yard` of each and the cell's `yard_rollup` |
| **Policy alignment** | Does observed policy match the choice? | `policy_priority_percentile`, `policy_alignment` (aligned / under_prioritized / over_prioritized / misaligned / deployment_gap / mixed / insufficient_data), `policy_alignment_confidence` |

The six choices: **Secure**; **Compete . Preserve**, **Compete . Catch Up**, **Compete . Partner**;
**Deploy . Accelerate**, **Deploy . Watch**. A node whose Need or Prize is not measured is **not assessed**
(never Watch: Watch is a positive conclusion, "domestic industrial priority: low").

**Need x Prize triages the problem; Competitive Position decides the industrial response.** Need x Prize
alone no longer assigns Preserve vs Catch Up (the 2026-10-28 rule did, and produced Partner despite strong
capability, Catch Up despite a leading position and Preserve without capability).

**Hard rule.** Observed policy never enters Need, Prize, Position or Choice. Revealed Policy Priority is
compared with the choice afterwards (Policy alignment). Otherwise the model is circular: government
prioritizes it, therefore it is strategic, therefore government is right to prioritize it.
`tests/testthat/test-decision-engine.R` checks that changing every policy column leaves every choice,
mode, confidence, borderline, flag, delivery mode and trace unchanged.

### Principles

- **Six stages are the canonical taxonomy** (Extraction, Refining & Processing, Component Manufacturing,
  Final Assembly & Equipment, Deployment & Use, Recovery & Recycling; data ids Extraction ... Recovery).
  Upstream / Midstream / Downstream is a derived roll-up (`sp_legacy_rollup()`), for compatibility only.
- **Need is stage-specific** (spec V-VI). Supply-chain need measures dependence on external supply at the
  producing stages; deployment need measures how much more deployment the energy system needs at
  Deployment & Use. They answer different questions and are never averaged, maxed or otherwise combined.
- **The Prize is not capability and not need.** Nothing that measures current capability (export share,
  RCA, production, operating capacity, annual investment, reserves, deployment scale) and nothing that
  measures need (HHI, import dependence, risky suppliers) enters `industrial_opportunity`
  (`SP_IO_FORBIDDEN`, refused by `sp_io_check_inputs()`) or `strategic_industrial_value` (the same list
  plus every `cp_*`, `home_share*`, `pw_*`, pathway and need column, refused by `sp_siv_check_inputs()`).
- **Market size alone never makes a prize, and never makes a Medium yard.** A large market adds to the
  market family of the prize; a node with little industrial-commons and future-option value is capped below
  the prize threshold, and a mature, passive, commodity node is a large yard however large its market.
- **Yards are judged on the specific capability first.** Sensitive layers (control software, firmware,
  enrichment, magnets, modules, racking...) take their own choice and yard; the technology x stage cell
  shows the roll-up, mixed where its units differ.
- **Position is demonstrated capability only**: no future market growth, technology importance, need,
  policy, climate goals, supplier concentration or generic readiness enters Competitive Position.
- **Source evidence can be coarser than six stages.** Every input carries an evidence status and each cell
  reports how much of its score rests on stage-specific observations. Evidence quality is reported next
  to a score (strong / moderate / weak in the Explorer) and never changes it.
- **Missing is not zero.** A family that is not observed is NA with basis `not_assessed`; a choice that
  lacks its Need or Prize is `not_assessed`.
- **Policy activity is not evidence of competitive advantage**, and a low observed policy priority is
  low *observed* NIPO / IEA PAMS intervention, not proof that the government gives the node no priority.
- **No stage prior and no technology prior** in any default output.

### Code and data

| Piece | File |
|---|---|
| Rules and thresholds | `config/strategic_position.yml` (`decision_framework`, `strategic_posture`, `policy_alignment`, `node_phrases`), `config/deployment_need.yml` |
| Need: supply chain | `R/strategic_position/import_exposure.R`, `strategic_need.R` (inputs), `supply_chain_need.R` (the score) |
| Need: deployment | `R/strategic_position/deployment_need.R`, `home_market.R`, `scripts/95_build_deployment_need.R` -> `data/processed/deployment_need_tbl.rds`, `home_market_tbl.rds` |
| Prize | `R/strategic_position/industrial_opportunity.R` (the market prize: IEA WEO 2025 / Critical Minerals via `scripts/17`, Clean Investment Monitor, TRL), `strategic_value.R` (strategic industrial value and the large-yard case; node attributes `data/reference/posture_node_attributes.csv`, documented in `docs/yf_node_attributes.md`) |
| Position | `R/strategic_position/competitive_position.R`, `trade_benchmark.R`, `trade_momentum.R` |
| Choice | `R/strategic_position/posture_engine.R` (Secure override; engine Compete tests as diagnostics), `pathway.R`, `decision_engine.R`, `yard_rollup.R` (layers as yard units, the roll-up, the paper-reproduction diagnostic); node attributes `data/reference/posture_node_attributes.csv`, `config/security_route_rules.csv` |
| Allied strategy | `R/strategic_position/partner_recs.R` |
| Policy alignment | `R/strategic_position/policy_priority.R`, `posture_engine.R` (`sp_policy_alignment*`) |
| Policy design | `config/posture_policy_design.csv`, `config/posture_delivery_modes.csv`, `R/strategic_position/policy_design.R` |
| Diagnostics | `R/strategic_position/sp_diag_choice.R`, `sp_diag_framework.R`, `sp_diagnostics.R`, `legacy_si_sensitivity.R` |
| Legacy (comparison only) | `R/strategic_position/decision_quadrant.R` (the 2026-10-28 need x EO quadrant), the ES / EO / SI builders in `R/indices/` |
| Build | `scripts/26_build_strategic_position.R`; review `scripts/27_review_strategic_position.R`; Explorer bundle `scripts/91_export_posture_data.R` (`R/dashboard/posture/posture_export.R`) |

Run order after an engine change: (10, 11, 17, 18, 20 when their inputs change) -> 95 -> 26 -> 27 -> 91.
Outputs: `<outputs>/strategic_position_tbl.rds|csv` (one row per Country x tech x stage),
`strategic_posture_layers_tbl.rds` (sensitive layers), `strategic_aib_division_tbl.rds|csv`,
`strategic_position_inputs.rds`, `strategic_position_trade_partner_inputs.rds` (incl. the node prize table),
`diagnostics/strategic_position/*.csv|md`, and review PNGs in `<processed>/charts/strategic_review/`.

Shared conventions: functions in `R/` are pure; percentiles are mid-rank, `(rank - 0.5) / n` over the
non-missing values (a singleton gets 0.5); thresholds are inclusive (high = at or above).

## 1. Need

### Supply-chain need (producing stages)

How costly or dangerous is dependence on external supply for this node? `supply_chain_need` (0-1) is the
within-cell mid-rank percentile, across countries, of the mean of four family scores (higher = more need):

- `sn_import_dependence`: net imports of the stage basket / GDP; fossil-fuel self-sufficiency;
- `sn_supplier_concentration`: HHI of the country's own import suppliers;
- `sn_risky_supplier_exposure`: import share from China (with Hong Kong and Macao) and opposite-bloc
  partners;
- `sn_trusted_alternatives`: 1 - the world-supply share of suppliers the country trusts (less its own),
  the Secure engine's Test 5 input. It is a scored family because it is observed for 94% of the assessed
  producing cells (design D4 line: 80%, `supply_chain_need.trusted_alternatives_min_coverage`); below that
  it would be a drawer subcomponent only.

Rules: concentration, risky-supplier and trusted-alternative needs are 0 for immaterial importers (under
USD 1m) and, with the **self-supply guard**, for a country holding 10% or more of world supply of the node
(`self_supply_share`, the Secure engine's line): a large producer is not dependent on the composition of
its small imports. Import dependence is kept for everyone. Thin cells (fewer than five comparable
importers) and entrepots are NA. Substitutability, qualification / reconstitution time, inventories,
systemic essentiality and operational-control risk stay Secure-engine inputs (curated node attributes) and
are shown in the drawer as subcomponents; they do not enter the percentile.

**Need floor** (user decision 2026-10-30, `decision_framework.supply_chain_need.score_floor`, 0.10). In a thinly
traded node most economies import too little to register and score 0, so a tiny positive score ranks above most of
the field (after yellowcake moved to Nuclear Extraction, Australia and Canada read 0.77 and 0.75 need on uranium for
need scores of 0.03 and 0.02). A percentile at or above the need threshold less its borderline band (0.62) stands
only where `supply_chain_need_score` reaches the floor; below it the percentile is capped at threshold - band - 0.01
(0.61), so the node reads plainly as low need and is never borderline on need. `supply_chain_need_cap` says
"score_below_floor" and `supply_chain_need_uncapped` keeps the percentile; the choice reason names it ("a need score
of 0.03, below the 0.10 floor, though it ranks at 0.77 in a thinly traded node"). Current run: 40 cells capped (Nuclear
Extraction 21, Nuclear Refining 18, EV-minerals Refining 1), 39 of which (the nuclear ones) would otherwise be high need; measured need
for 7,554 of 11,770 producing cells, high need 2,386.

At Deployment & Use, `supply_chain_need` is NA: the Assembly-equipment proxy that `strategic_need.R`
writes on the Deployment row (`strategic_need_percentile`) is kept only for the legacy comparison and is
never read by the choice.

### Supply-chain need: the inputs (strategic_need.R)

How exposed is the country if supply of this cell is disrupted? Families, each the mean of its
measures' within-cell percentiles (higher = more need):

- `import_dependence`: net imports of the stage basket / GDP; fossil-fuel self-sufficiency;
- `supplier_concentration`: HHI of the country's own import suppliers;
- `risky_supplier_exposure`: import share from China (with Hong Kong and Macao) and from
  opposite-bloc partners (`geo_alignment == 0`).

Import exposure comes from the reconciled exporter -> importer flows of scripts/15 (last trade
year). Concentration and risky-supplier measures are 0 below USD 1m of imports; a cell with fewer
than 5 importers above that is thin. Entrepots and ship registries (HKG, MAC, MHL, LBR, PAN) are not
ranked and do not enter others' percentiles. A nonaligned importer's opposite-bloc share is NA (it is
0 by construction), so its risky family rests on the China share. On the Deployment row
`strategic_need.R` still writes the technology's Assembly need (proxy) and, for coal, gas and oil,
Extraction fuel import dependence (shared), as `strategic_need_percentile`, for the legacy comparison
only: `supply_chain_need` is NA there and the choice never reads the proxy. No world constant and
nothing from Competitive Position enters.

### Deployment need (Deployment & Use)

How much additional deployment does this country need to meet expected energy-system requirements or
its own stated goals? It is not supply-chain vulnerability. `deployment_need` (0-1) is the mean of the
observed family percentiles (each family ranked within the technology across countries), for eleven
technologies (Batteries = stationary storage), fossil technologies assessed exactly like clean ones:

| family | scenario | measures (technology attribution in `config/deployment_need.yml`) |
|---|---|---|
| `dn_demand` demand adequacy | current policy | electricity demand growth (WEO 2025 Current Policies for the six single-country WEO regions, else the Ember 5-year trend); the technology's own generation growth; EV share of new-car sales (GEVO 2026); oil-use trend |
| `dn_policy_gap` climate / policy gap | national commitments | 2030 shortfall against the country's own targets and approved national plans (Ember 2030 Renewable Target Tracker): projected installed (installed + recent build rate) vs target, per installed fleet; the total-renewables target for Solar, Wind and, as the integration requirement, Batteries and the Grid |
| `dn_system` system need | current policy | imported fuel the technology displaces; flexibility need (solar + wind share of generation) for storage, grid and gas; firm-capacity need for dispatchable technologies |

Every family carries `basis` (national / observed_trend / not_assessed; regional values are context only)
and its scenario label. A climate-aligned value (World WEO 2025 Net Zero vs Current Policies at 2035,
`dn_climate_aligned`, a ratio, not a score) is shown only as an explicitly labelled alternative and never
enters the score: no 1.5 C pathway is treated as objective fact. Missing evidence stays NA; a technology
x country with no family is not assessed. Coverage (2026-10-29): 1,683 of the 2,354 classified Deployment cells
(1,713 of 2,409 over all 219 economies in the table).
Not assessed by design: grid congestion / transmission adequacy (no national data), hydrogen and EV
targets, geothermal targets, oil system need, the committed pipeline (GEM needs a contact form).

## 2. Prize: Strategic Industrial Value

Is domestic productive capability in this specific node strategically valuable? A large future market does
not by itself make domestic manufacturing strategically valuable (user spec 2026-10-30, Yards & Fences item
2), so the prize the choice reads is **`strategic_industrial_value`** (0-1, producing stages only;
`R/strategic_position/strategic_value.R::sp_strategic_value()`), built from three families per cell or
sensitive layer:

| family | inputs | scale |
|---|---|---|
| **market** (`siv_market`) | the market prize `industrial_opportunity`, unchanged (below): world demand growth, global investment momentum, lifted by national home-market growth | 0-1, varies by country through the home market |
| **industrial commons** (`siv_commons`) | `process_knowledge` (the production-innovation linkage: does making it build process knowledge that carries into the next generation), `workforce_suppliers` (the supplier ecosystem and workforce), `adjacent_innovation` (cross-sector spillovers) | yes 1 / partly 0.5 / no 0, **summed over the three and divided by three, an Unknown counting 0** (the strict reading of the paper's learning score, `gates.ts` `learningScore` at its low bound, and of engine Test 3): one "yes" among three questions is a third, never the whole family. `siv_commons_n` = how many were observed |
| **future option** (`siv_option`) | for a leap candidate (`leap_candidate` yes, with its named `successor`), the four leap tests `beyond_novelty`, `cost_perf_advantage`, `incumbent_vulnerable`, `scale_pathway` (next-generation leadership indicators) | the same scale, summed over four (Unknown 0); 0 for a node that is not a leap candidate. Candidacy gates the family rather than scoring in it, so a candidate whose tests all fail has no option value. `siv_option_n` = tests observed |

- `siv_score` = the weighted mean of the three families (`decision_framework.strategic_value.weights`, 1:1:1;
  renormalised only where the market prize is missing and `require_market` is false). **Calibration choice
  to report**: equal weights are a neutral prior, not an estimate.
- `strategic_industrial_value` = the mid-rank percentile of `siv_score` across the assessed producing-stage
  cells (as the node prize is a percentile across the producing nodes), so `prize_threshold` (0.6) keeps its
  selectivity: about the top 40% of producing cells. Layers are placed among the cells' scores.
- **Market-only cap** (`siv_cap = market_only`): where neither commons nor option reaches
  `strategic_value.gate` (0.5), observed or not (an unobserved family counts 0, so less evidence never
  lifts the cap), the value is capped at `prize_threshold - cap_margin` (0.59). Market size alone never makes
  the prize high. The cap is a rule, not a measurement: a capped prize is never high at any prize threshold
  (the rule reads `siv_cap`, so `choice_threshold_sensitivity.csv` at 0.50 or 0.55 does not lift it) and never
  borderline. **Calibration choice to report** (the gate: 0.5 on commons is 1.5 learning points of 3, between
  the paper's moderate, 1, and strong, 2). On the curated attributes the cap now covers 10 of the 12
  producing-stage control layers (process knowledge is not assessed for software, so their commons is at most
  two thirds; HVDC control and reactor controls escape it), the Recovery rows other than Batteries and Nuclear and
  other rows whose learning attributes are Unknown or low (every Extraction stage row; Coal, Gas and Geothermal
  Refining; Oil Components), battery enclosures, grid cables, PV racking and hydrogen PGMs, and Solar Components
  (its stage row's learning attributes are Unknown; its option family is 0.375): 39 producing nodes or layers
  against 18 under the mean-of-observed reading (`strategic_value_families.csv`, current run).
- Without the market prize (no demand series) the node stays not assessed (`require_market: true`): the
  curated attributes alone never decide a prize.
- `siv_families_observed` counts the families with at least one observed input; `siv_commons_n` and
  `siv_option_n` count the observed attributes within commons (of 3) and option (of 4 leap tests).
- **No double counting of position, and capability kept separate.** Competitive Position, the capability
  floor, the measured supply share, the pathway legs and every need variable are refused
  (`sp_siv_check_inputs()`); the country's own `complementary_capabilities` are not read. The commons and
  option attributes describe the node (a country row may refine one), not how good the country is at it.
- `decision_framework.strategic_value.prize_measure: market` restores the market prize as the prize (the rule
  until 2026-10-30); `strategic_value_vs_market.csv` compares the four combinations of prize measure and
  large-yard case on the stored inputs.

Current run: `strategic_industrial_value` over the 11,770 producing cells has quartiles 0.25 / 0.50 / 0.74; 3,044 of
the 7,554 assessed producing cells (40%) have a high prize. All 219 economies share each node's market-only state
(`strategic_value_families.csv`: a node is market-only in every economy or in none). On the stored inputs, against
the market prize without the large-yard case, the strategic value alone changes 231 producing-cell choices, the
large-yard case alone 595 and both 798 (`strategic_value_vs_market.csv`; the moves are listed under Choice).

The attributes behind the commons and option families, and the three product-market attributes of the
large-yard case, are curated in `posture_node_attributes.csv` with a reason and source for every value
(`docs/yf_node_attributes.md`); `Unknown` is not observed. They never encode a yard or the paper's
placement (`paper_placement` is outside the node contract, `PN_COLUMNS`).

### The market prize: Industrial Opportunity

Is there a plausible economic or technological market prize in this technology x stage node?
`industrial_opportunity` (0-1, producing stages only; `R/strategic_position/industrial_opportunity.R`), the
market family of the strategic industrial value:

| component | what it is | varies by country? |
|---|---|---|
| `io_growth` -> `io_growth_pct` | expected growth of world demand 2024 -> 2035 under current policy: IEA WEO 2025 World, Stated Policies, for the node's primary series (`config/iea_pathways_map.yml`); for the mineral Extraction and Refining nodes the IEA Critical Minerals Outlook Stated Policies key-mineral demand of the technology (2025 -> 2035). Recovery (and Geothermal Refining), with no series of their own, take the technology's deployment series as a proxy (`io_growth_basis = proxy_deployment`). Percentile across the 55 producing nodes. | no: technology context |
| `io_investment_momentum` -> `_pct` | global manufacturing investment momentum: log change of world capex, 2021-22 to 2024-25 (Clean Investment Monitor), observed for ten clean-tech manufacturing nodes and ranked among them; weight 0.25 in the node prize where observed (demand growth is the prize, the capex trend corroborates it) | no |
| `io_node_prize` | (1 - w) x growth pct + w x momentum pct where momentum is observed, else the growth pct; capped at 0.33 where the world market is declining (and at 0.5 for an oversupplied node, from a shippable source only: none is staged, BNEF overcapacity never ships) | no |
| `io_home_market` | national expected growth of domestic deployment (WEO 2025 single-country Stated Policies, Ember targets and approved plans, Ember / GEVO observed additions; `home_market_tbl.rds`, scripts/95); national basis only, regional values never scored | yes |
| `industrial_opportunity` | the node prize, **lifted** to mean(node prize, home market) where the national home market is above it (`home_market_mode: lift`, the user's "lifted by national home-market growth"): a fast-growing home market adds to the prize; a small one does not take a world-market prize away from an export-oriented producer. A declining world market stays capped. | through the home market only |
| `io_maturity` | IEA Clean Technology Guide TRL 2025 and the curated maturity: shown, never scored; an emerging node that lands on Watch carries the `option_value` reason | no |

Removed (spec IV): every capability variable of the legacy EO (market share, RCA, export size,
production size and growth, operating capacity, annual investment, reserves, LCOE), every need variable
(world HHI, import dependence, risky suppliers) and the country factor cost (Input Cost Index, 25% of
legacy EO weight and its main within-cell driver). Feasibility, the investment pipeline, adjacency,
curated learning and input cost are not in the market prize: they are the **credible-pathway test** for
Catch Up. (The curated learning attributes of the node do enter the strategic industrial value, as its
industrial-commons family: the value of the node to any producer. The pathway reads the same attributes
only on the country's own base, Test 3's learning lower bound, and only as a supporting leg, never as a
capability anchor.)
The old market-outlook list typed in the config is replaced by data (`sp_io_tech_outlook()`: the outlook
of the technology's deployment series, with documented overrides in
`decision_framework.industrial_opportunity.market_outlook_override`, none in use); the engine's catch-up
diagnostic still reads it.

Before 2026-10-30 the rule read this market prize directly: high prize `industrial_opportunity >= prize_threshold` (0.6). Distribution and sensitivity:
`decision_review.md` (2026-10-29: median 0.54, 4,734 of 12,045 producing cells at or above 0.6; those counts
include the five aggregates, and for the 214 economies it is 4,629 of 11,770). The legacy
EO-based column is `legacy_eo_opportunity` (and `legacy_eo_opportunity_pct`); it is never read by the
choice.

Validation (spec XXI.2, `opportunity_validation_*.csv`, current run): Spearman with Competitive Position 0.095
pooled and 0.38 median within the 37 nodes where the home market varies the prize; with supply-chain need 0.04 /
0.10; with the legacy EO 0.26; world demand growth carries 82% of the prize, the capex trend 4%, the home market 14%
(two-leg run: 0.09 / 0.36, 0.04 / 0.13). The market prize is not a disguised capability score: within a node only
the home market moves it, and large home markets are partly large producers (hence 0.38).

Known limitation: the manufacturing nodes use WEO capacity (a stock), the mineral nodes annual mineral
demand (a flow). Where annual additions plateau while the installed base keeps growing (solar), the
manufacturing prize is an upper bound and the mineral prize low (Solar Refining 0.20 vs Solar Components
0.85). Next refinement: WEO capacity additions for the manufacturing nodes.

### The large-yard case

The countervailing test (user spec 2026-10-30, Yards & Fences item 3): where affordable, diversified access
should generally dominate domestic industrial policy, a node is a **large yard** however large its future
market. `sp_large_yard_case()` reads ten signals per cell or layer, each TRUE / FALSE / not observed; **market
size is never one**:

| signal | holds when | source |
|---|---|---|
| `mature` | `maturity` mature | node attribute |
| `passive` | `digitally_active` no. On a layer, evidence. On a stage row, a convention: the stage row is the remainder of the stage after its sensitive layers, whose control software is assessed as control layers (digitally active), so the remainder reads passive (`docs/yf_node_attributes.md`) | node attribute |
| `standardized` | `standardization` high | node attribute (2026-10-30) |
| `substitutable` | `substitutability` easy or moderate | node attribute |
| `easy_qualification` | `qualification_time` under 3 months or 3-12 months | node attribute |
| `diversified` | the largest supplier's share of world supply (`world_top_share`, `supply_structure.R`; a mineral cell's driving mineral) below `diversified_top_share_max` (0.5), and no coercive supplier at or above the line (the largest coercive supplier is a lower bound of the largest supplier, and the two can come from different scopes) | data; a layer with its own world-supply measure or mineral reads its own (`posture_inputs.R` swaps `world_top_share` with the coercive shares: pv-modules reads module capacity, magnets rare-earth refining), any other layer its cell's |
| `low_learning` | `process_knowledge` no | node attribute |
| `low_spillover` | `adjacent_innovation` no | node attribute |
| `low_differentiation` | `differentiation` low | node attribute (2026-10-30) |
| `commoditized` | `commoditization` yes | node attribute (2026-10-30) |

`large_yard_case` is TRUE when every **required** signal holds (`large_yard.required`: mature and passive,
the paper's Large Yard: a passive capability past its learning curve), at least **`min_signals`** (5) of the
other eight hold, and **no Medium-yard rationale of the node vetoes it**:

- a **credible leap** (`large_yard.leap_veto`; `sp_leap_credible()`, the paper's `leapCredible`): a leap
  candidate with `beyond_novelty` yes, `cost_perf_advantage` yes and two supports not "no" among
  `incumbent_vulnerable`, complementary capabilities and `scale_pathway`. Complementary capabilities are the
  country's own strengths, not a node attribute, so they are read as not assessed: a credible leap needs both
  node supports, and one leaves the test undecided. Candidacy alone is not a veto (the paper says of a
  candidate with only "partly" evidence: "not yet evidence beyond novelty");
- **strong industrial learning** (`large_yard.learning_veto_points`, 2): the commons points (process knowledge
  + workforce and suppliers + adjacent innovation, yes 1 / partly 0.5) at or above 2 even with every unknown at
  0 (the paper's strong learning, a Medium-yard rationale on its own); undecided where an unknown could reach 2.

FALSE when a required signal fails, a veto holds, or too few of the others could hold even if every unobserved
one did; NA (undecided) otherwise, an undecided veto included. A large yard whose learning points reach
`learning_floor_points` (1, the paper's moderate learning) keeps a **capability floor** (`large_yard_floor`;
the choice variant `deploy_floor`, whose policy design fences a capability floor): the paper's `large-floor`,
which its tool reaches when a moderate capability case meets a domestic premium above 30%; here the commodity
evidence stands in for that cost. Columns: `large_yard_case`, `large_yard_signals` (those that hold),
`large_yard_n` (how many of the eight), `large_yard_unknown`, `large_yard_reason` (one sentence),
`large_yard_floor`, `large_yard_near` (FALSE only for want of one signal). **Calibration choices to report**:
`min_signals` (`large_yard_sensitivity.csv` runs 4 and 6), the 0.5 diversification line, and the learning
veto and floor points (run `no_learning_veto`). This is the Yards & Fences engine's route to its Large Yard,
spelled out: no strong Medium-yard rationale holds (no present position, with `position_first`; no credible
leap; no strong learning), and "production becomes commoditised and manufacturing generates little future
learning" (`dashboard/src/fence/engine/gates.ts`, `classify.ts`); where a moderate learning case holds, the
large yard carries a capability floor rather than claiming that no capability rationale holds.

Departures from the paper's tool, kept deliberately: concentration is one of the eight votes (the 2026-10-30
spec lists global supply diversification as a signal), where the paper uses it only to add diversification
inside the Large yard; and three of the eight signals (standardized, low differentiation, commoditized) are
facets of one product-market property. Grouping the signals into product-market, learning and supply
families and requiring one from each is an alternative not adopted.

On the curated attributes (pinned in `tests/testthat/test-strategic-value.R` with world supply read as
diversified, then concentrated), passive racking, battery enclosures and grid cables are large yards (8, 7 and 7 of 8
when diversified; one fewer when concentrated); conventional PV modules are a large yard **with a capability floor**
(moderate learning, 1.5 of 3), on 6 of 8 signals where world supply reads diversified and on exactly 5 (borderline:
`min_signals` 6, or differentiation "moderate", flips it) where its own module supply reads concentrated, as it does;
next-generation PV, battery cells, polysilicon, magnets, enrichment, transformers and motors are not (strong
industrial learning, 2 or more of 3); control layers are not (digitally active). On the current run's world supply
data (`yf_paper_reproduction.csv`): racking 8, grid cables 7, battery enclosures 6 (their cell's supply reads
concentrated) and PV modules 5. Whether a node actually is one is decided by its attributes and the world supply
data, never by the paper's placement.

Current run (`large_yard_case.csv`, `large_yard_sensitivity.csv`): the case holds for 2,338 assessed producing cells,
is undecided for 408, and decides 2,133 Watch cells (1,253 with a capability floor); the rest are Secure (159), a
top-quarter Preserve read first (5) or Watch on the triage (41). The large-yard cells are commodity Extraction and
Refining: Electric Grid Refining and Extraction, Solar Extraction and Refining, Nuclear Extraction, Coal, Gas and Oil
Extraction and Refining in nearly every economy, and Batteries and EV-minerals Extraction where their world supply
reads diversified (49 economies each; Watch by the case in 48 and 47); among layers, battery enclosures, grid cables, PV racking and PV modules.
`min_signals` 4 / 6 change 271 / 159 choices; `position_last` 5; `no_learning_veto` none.

## 3. Position: Competitive Position

How strong is this country today in this node? Demonstrated capability only (spec III). The choice reads
it among the node's **contenders** (`cp_contender_pct`): the Competitive Position score placed among the
economies holding at least 0.5% of world supply of the node. With fewer than five contenders the
cross-country percentile stands in (`cp_contender_basis = few_contenders`), but a high position still
requires the country to be a contender: a non-producer cannot preserve a mining node on its ore
re-exports. Where no supply share is measured at all (Deployment & Use, and Green Hydrogen Refining), the
cross-country percentile alone is used (`all_countries`). **High Competitive
Position** (user decision 2026-10-29) = the top quarter of contenders (`cp_contender_pct >= 0.75`), being a
contender, and the posture engine's capability lower bound (`t3_capability_lo` moderate or strong: an
established world-share position, a foothold in one of the country's stronger cells, or transferable
learning on a domestic base). A top-quarter score that fails the test says why: an economy below the
contender line has "a top-quarter score without a contender share" (trace result `not_a_contender`,
checked first), a contender below the floor "a top-quarter score below the capability floor"
(`capability_floor_not_met`, flagged for review).

**Unmeasured position.** Competitive Position is not measured for every economy at Recovery & Recycling
(no supply share and no Competitive Position family exists for recycling) and for a few cells elsewhere
(`cp_contender_pct` NA). The choice rule treats an unmeasured position as **not strong**: such a node can be
Catch Up, Partner or Watch, never Preserve. Where the rule reads the position (a high prize, or high need
with a low prize) the missing measurement lowers `posture_confidence` by one step, as an unknown pathway
does (design D3); the choice reason says "Competitive Position not measured" and the trace's position
result is `unknown`. Where need and prize are both low the node is Watch whatever its position, so nothing
is lowered.

### Evidence specificity

`index_variable_stage_evidence.csv` has one row per (pillar, category, variable, tech, stage), with
`*` wildcards (a specific row beats `*`), `evidence_status`, `allocation_method`,
`country_variation` (country, regional, global, pair, single_country), `source` and `notes`. Every
EO and ES variable x technology x stage combination in the current build resolves to exactly one row.

Per cell and dimension, over the dimension's long `*_inputs` table (`weight` = the input's intended
share of the cell's score):

- `evidence_specificity` = weight on observed inputs whose status is exact or split / all weight;
- `exact_`, `split_`, `shared_`, `proxy_`, `tech_wide_evidence_share` over observed weight;
- `stage_evidence_quality`: high (>= 0.67), medium (>= 0.33), low (> 0), none.

`stage_evidence_quality` on the canonical table is Competitive Position's.

### Competitive Position

Relative to other countries, how strong is the country's demonstrated position in this technology
and stage? Families (config `competitive_position.families`), each the mean of its observed variables:

| Family | Stages | Variables |
|---|---|---|
| export_position | E, R, C, A | world export share; RCA (2022-24 benchmark, with current momentum) |
| production_position | E, R | mined / refined / fuel production share (structural zero for unlisted) |
| manufacturing_capacity | R, C, A | GCIM operating plants |
| investment_position | R, C, A, Dep | GCIM annual capex |
| cost_position | Dep | BNEF LCOE |
| resource_endowment | E, Dep | reserves; solar, wind, geothermal resource |
| deployment_scale | Dep | generation |

`competitive_position_score` = mean of the observed families; `_percentile` and `_rank` across
countries within the cell (no percentile below 5 scored countries). Deployment is labelled
`market_deployment_position`.

Kept out as context: future or global demand (Technology Demand), TRL, world price volatility, world
export HHI, export feasibility (HS4 proximity, used instead by the posture's complementary test), the
input-cost index, announced pipeline, the IEA ETP production-cost index (six regional values, EU
scaled to 0), the IEA GEVO regional EV production and net exports, BNEF installed capacity per capita
(a regional broadcast), absolute generation growth (rewards a small decline), and energy use per
capita. Recovery has no family: its baskets are scrap and spent material, and a large export share
means the country ships it abroad for processing, not that it recycles.

#### Trade evidence: structural benchmark and current momentum

`scripts/94_pull_comtrade_trade_refresh.R` pulls Comtrade exports to the World of every basket code
and total merchandise exports, annual 2022-2024 for every Comtrade reporter and monthly over 38 months
for the analysis universe (coalition core and partner members plus economies with GDP of at least
USD 200bn). Pure code: `R/strategic_position/trade_benchmark.R`, `trade_momentum.R`; config
`competitive_position.trade_benchmark` / `trade_momentum`.

- **Structural benchmark.** For each reporter-year with a reported total: basket exports, the world
  export share (over that year's reporters; re-export hubs excluded) and the Balassa RCA; each averaged
  over the years observed (at least two). The S-curve ranks across countries within the cell replace
  EO's single-year `market_share` (Atlas, 2024 since the V18 release) and `rca` (Comtrade 2024). An economy that does not
  report to Comtrade keeps its Atlas share (`trade_benchmark_source = atlas_fallback`) and no RCA.
- **Common cutoff.** The latest month M at which at least 80% of the universe's monthly reporters have
  all 24 months M-23..M (`trade_momentum_cutoff.csv`). Twelve universe members publish no monthly trade
  at all (they are listed, never counted as complete), so the 80% is of the monthly reporters; the
  share of the whole universe is reported beside it. A reporter without the 24 matched months gets NA,
  never 0; there is no partial-year comparison and no mirror (partner-reported) fill.
- **Measures**, trailing 12 months against the preceding 12: (A) log export growth
  (`trade_momentum_raw`); (B) specialization momentum, the change in the basket's share of total
  merchandise exports = A - log total-export growth (`trade_momentum_specialization`); (C) world-share
  momentum where the complete reporters hold at least 80% of the cell's benchmark exports
  (`trade_momentum_share_change`). Within a cell C differs from A by a constant and ranks countries
  exactly as A does, so it is diagnostic only.
- **Guards.** A small-base floor on the previous twelve months (the cell's lower quartile of positive
  values, never below USD 1m); an HS-revision check (a reporter that changed HS edition between the
  windows with more than 25% of the cell's value in HS4 headings present in one window only:
  `hs_break`); a reporter check (calendar-2024 monthly totals against the annual total: more than 10%
  apart is `caution`, more than 25% `reporter_mismatch`, excluded); winsorisation of A and B at the 5th
  and 95th percentiles across all cells; at least five valid reporters per cell.
- **Score.** `trade_momentum_score` = mean of the mid-rank percentiles of A and B across the valid
  reporters within the cell. A world price swing that moves every exporter alike moves no rank
  (`trade_momentum_prices.csv` reports the common component and the value-volume gap for mineral and
  fuel cells).
- **Trade pillar.** The export family = 85% structural + 15% momentum where momentum exists,
  structural alone otherwise (`cp_export_position_structural` keeps the structural value;
  `trade_momentum_weight` says which). Sensitivity at 10 / 15 / 20%:
  `trade_momentum_weight_sensitivity.csv` (rank correlations, movers above 5 and 10 percentile points).
  Competitive Position now decides the response (the contenders' top quarter preserves), so the
  sensitivity file re-runs the canonical choice at each weight on the stored need, prize, pathway and
  floor: on the current run (the re-pulled 2022-24 benchmark and momentum) dropping momentum (weight 0) changes
  27 choices and 1 review flag; 0.10 changes 12 choices and no flag, 0.20 changes 15 and 1 (two-leg run: 16 and
  5, 7 and 2, 9 and 1).

Fields: `trade_benchmark_period`, `trade_benchmark_source`, `trade_benchmark_share`,
`trade_benchmark_rca`, `trade_benchmark_exports`, `trade_momentum_period`,
`trade_momentum_previous_period`, `trade_months_observed`, `trade_momentum_quality` (ok, caution,
no_monthly_data, incomplete_coverage, small_base, hs_break, reporter_mismatch, thin_cell,
outside_universe), `trade_momentum_coverage`, `trade_momentum_raw`, `trade_momentum_score`.

## 4. Choice

`R/strategic_position/decision_engine.R::sp_decision_choice()`, after the posture engine, supply-chain
need, the prize, the contender percentile, the pathway test and deployment need. Thresholds are config
(`decision_framework`): need 0.67, prize 0.6, Competitive Position 0.75 among contenders, deployment need
0.6, borderline band 0.05.

**Step 1, Secure override.** If the engine finds an acute vulnerability (Test 1 consequence AND one of the
three routes below), the node is **Secure** whatever its need, prize or position, with its
`security_rationale` (operational_control / coercive_chokepoint / systemic_resilience), `delivery_mode`
and mitigation (the policy design's build text). Secure is a rule-based override, not a high-need label:
191 of the 2,386 high-need producing cells (8%) are Secure (206 Secure cells in all; 174 on the two-leg run); 175 on
systemic resilience, 27 on a coercive chokepoint, 4 on operational control. Secure does not mean reshoring: delivery
follows trusted supply (104 allied, 42 diversified global, 60 mixed).

**Step 2, Deployment & Use.** Decided on `deployment_need` alone: missing -> not assessed; at or above 0.6
-> **Deploy . Accelerate** (the energy system needs more of it; this says nothing about where the
equipment is made); below -> **Deploy . Watch** (no major acceleration required). A Deployment node is
never Partner, Preserve or Catch Up: those are supply-chain answers. 0.6 on deployment need (a mean of up
to three family percentiles, narrower than a percentile) takes about the top 30% of assessed Deployment
cells, the selectivity of supply-chain need at 0.67 (a percentile); 0.67 took 22%.

**Step 3, producing stages** (Extraction, Refining, Components, Assembly, Recovery). First, need or prize
missing -> **not assessed** (never Watch, whatever the large-yard case). Then the **large-yard case** (Prize,
above), after a high Competitive Position (`large_yard.position_first: true`, the paper's gate order: a present
position worth defending is a Medium-yard rationale that comes before Large, so a top-quarter producer of a
commodity node goes on to the triage): TRUE -> **Deploy . Watch** (`decision_step` large_yard, `watch_reasons`
beginning `large_yard_case`, variant deploy_floor where the case keeps a capability floor, else deploy, or
deploy_diversify where supply is concentrated), whatever need and prize say; Secure has already come first, so
a genuine chokepoint is never waved through. Otherwise, with high need = `supply_chain_need >= 0.67` (after the need
floor, under Need), high prize
= `strategic_industrial_value >= 0.6` and not capped as market-only (`prize_measure: market` reads
`industrial_opportunity`, with no cap) and high position as defined under Position:

| | high Competitive Position | otherwise, credible pathway | otherwise, high need | otherwise |
|---|---|---|---|---|
| **high prize** | Compete . **Preserve** | Compete . **Catch Up** | Compete . **Partner** | Deploy . **Watch** (keep R&D options) |
| **low prize, high need** | Compete . **Preserve**, flagged `review_strong_position_low_prize` | | Compete . **Partner** | |
| **low prize, low need** | Deploy . **Watch** | | | Deploy . **Watch** |

- Preserve applies whether need is high or low. A strong position with high need and a low prize is not
  mechanically Partner (spec VIII): it is Preserve with a review flag (design D2; 8 cells on the current run, 16 on
  the two-leg run), so strategic redundancy, Secure or a recalibration of the prize can be considered by a reader. A
  top-quarter score that fails the capability floor gives Catch Up or Partner with `review_top_quarter_below_floor`
  (4 cells on the current run: Brazil Wind Assembly, Catch Up; India, Thailand and Viet Nam Solar Components,
  Partner; 8 on the two-leg run): a leading score in a fragmented node, for review rather than silent override.
- **Credible pathway: a capability anchor plus corroboration** (user spec 2026-10-30,
  `decision_framework.pathway.rule: anchor`, `pathway.R`). `pathway_credible` yes when the country has at
  least one **capability anchor**, evidence of a real industrial base in or next to the node, and at least
  one more signal from a **different evidence family** (another anchor or a supporting signal). The anchors
  are absolute measures, never a Competitive Position score:
  - direct supply share: the node's **measured** world supply share (`home_share_exposure`, never a
    listing's residual) >= `direct_share_min` (0.5%, the contender floor) [family direct];
  - direct export share: the node basket's 2022-24 benchmark world export share
    (`trade_benchmark_share`) >= 0.5% [family direct];
  - operating capacity: GCIM operating capacity >= 0.5% of world operating capacity of the node's most
    material product (`investment_capacity_shares()`) [family capacity];
  - committed pipeline: GCIM capacity under construction or built and not yet online >= 1% of world
    operating capacity [family pipeline];
  - adjacent stage share: an adjacent stage of the same technology with measured world share >= 5%
    (absolute adjacency) [family adjacency];
  - resource base: at Extraction, recorded reserves or resource above the cell floor [family resource].

  Supporting signals corroborate an anchor and never qualify on their own: Atlas product-space
  feasibility in the top quarter (`feasibility_pct` 0.75); an input-cost advantage (Input Cost Index in the
  cheapest quarter); relative adjacency (an adjacent stage among the country's own stronger cells,
  within-country CP percentile >= 0.6) [family adjacency]; curated learning inputs (engine Test 3 learning
  lower bound moderate or strong, whose "base" can be feasibility or relative adjacency alone, so context
  rather than capability); announced or paused GCIM capacity >= 2% of world operating capacity [family
  pipeline]. A signal in the anchor's own family does not corroborate it (the two direct shares are one
  family; relative adjacency does not corroborate absolute adjacency). Extraction also needs a resource
  base. `no` = an anchor family is observed and none fires (`pathway_basis` "...;no_anchor"), or the
  anchor is not corroborated though other evidence is observed ("...;anchor_uncorroborated"), or no
  resource base at Extraction ("no_resource_base"); `unknown` = the evidence cannot decide (no anchor family
  observed: "anchor_unobserved"; an anchor with nothing else observed: "corroboration_unobserved"; nothing
  observed: "no_evidence"; a corroborated anchor at Extraction with the resource base unknown:
  "...;resource_base_unknown"), read as not credible with lower confidence. `catch_up_anchor` (TRUE / FALSE
  / NA where no anchor family is observed), `catch_up_anchor_type` and `pathway_support` name what fired,
  and the choice reason names the anchor and its corroboration, or what is missing. A high prize without a
  high position or an anchor, decided Partner or Watch, carries `greenfield_option` (2,529 cells on the
  current run: Partner 792, Watch 1,737): an attractive but unsupported opportunity, never Catch Up.

  The **legs rule** (`rule: legs`; design D3 at `min_legs` 1, the two-leg rule of the morning of 2026-10-30
  at 2) is kept for the sensitivity: yes when `min_legs` of feasibility, pipeline (raw GCIM pipeline > 0),
  adjacent stage (absolute, on `home_share` as before, or relative), learning and input cost fire, with the
  `too_few_legs` / `too_few_observed` bases. It reads `home_share`, residuals included, so its counts
  reproduce; the anchors read the measured share because the residual gave every economy that a partial
  listing omits 0.87% of battery refining, which made Afghanistan, Andorra, Bermuda and 150 other small
  economies Catch Up there on a direct-supply anchor until d65ae03.

  Current run (`catch_up_rule_comparison.csv`, `catch_up_capability_bands.csv`, `catch_up_anchor_types.csv`,
  `pathway_by_stage.csv`): Catch Up 1,952 under one leg, 928 under two, 355 under the anchor rule. Every Catch
  Up cell has an anchor; none has a cross-country Competitive Position below 0.25 (36 under two legs, 273
  under one). Anchors among them: direct export share 303, direct supply share 256, adjacent stage share 54,
  operating capacity 43, committed pipeline 31 (no Extraction cell is Catch Up: every Extraction
  stage row is capped as market-only, and most are large yards). Of the 2,902 assessed high-prize producing cells without a high position, not Secure and not a
  large yard, 12% have a credible pathway (Catch Up 355), 28% go Partner (800) and 60% Watch (1,747): not
  every prize is forced into Catch Up. On the old data the anchor rule took Catch Up from 748 (two legs) to
  357: 456 cells left it (164 for Partner, 292 for Watch), 65 joined it (one-leg cells with an anchor, e.g.
  Chilean and Argentine battery-minerals Extraction).
- **Watch reasons** (`watch_reasons`): large_yard_case (a mature, passive commodity node), low_need,
  low_prize, ample_diversified_supply (no concentration watch), weak_domestic_case (a prize without a
  top-quarter position or a pathway), option_value (an emerging node), low_deployment_need. Watch is a
  positive conclusion: domestic industrial priority low.
- **The large-yard case in the machinery.** A large-yard Watch reads need and prize only to know they are
  measured, and none of prize level, position or pathway, so their borderlines and confidence issues do not
  apply to it, and its `watch_reasons` do not list the triage reasons (low_need, low_prize,
  weak_domestic_case). A case that holds on exactly `min_signals`, or fails for want of one signal
  (`large_yard_near`), is borderline (`decision_borderline` large_yard_case; the choice the other way is in
  `choice_possible`); an undecided case (NA) that would change the choice is a confidence issue and adds Watch
  to `choice_possible` (`pivotal_inputs` large_yard_case). The trace gains a `large_yard` step (the signals
  that hold against `min_signals`). `large_yard.position_first` was false until the 2026-10-30 fidelity
  review and is now true (the paper's order); `large_yard_sensitivity.csv` run `position_last` gives the other
  order. The legacy-vs-choice diagnostics attribute a large-yard Watch to `large_yard_case` and a prize change
  to the prize column the rule read (`strategic_industrial_value`).
- **What the strategic value and the large yard moved** (current run against the market prize without the
  large-yard case, need floor applied in both; `strategic_value_vs_market_transitions.csv`): 798 producing
  cells. Partner -> Watch 567, all by the large-yard case (commodity Extraction and Refining: Electric Grid,
  Coal, Solar, Gas, Oil, Nuclear, Batteries and EV-minerals); Watch -> Catch Up 73, Partner -> Catch Up 24 and Watch ->
  Preserve 26 on equipment nodes with high industrial commons (Electric Grid, Nuclear, Gas, Oil and Coal Assembly,
  Nuclear Components and Refining, Coal and Gas Components); Catch Up -> Watch 69, Catch Up -> Partner 30 and
  Preserve -> Watch 9 on market-only or large-yard nodes (Batteries and EV-minerals Extraction, Solar
  Components, Gas Extraction and Refining, Nuclear Extraction).

**Outputs** (`strategic_position_tbl`): `strategic_choice` (the six verbs or not_assessed),
`strategic_posture` (secure / compete / deploy / not_assessed), `compete_mode`, `deploy_mode`,
`posture_variant` (secure_* from the engine; compete_preserve / compete_catch_up / compete_partner;
deploy_accelerate for Accelerate since 2026-10-30; deploy, deploy_diversify, deploy_floor: a large yard with a
capability floor, or the engine's own), `prize_measure` and `choice_prize` (the prize the rule read),
`greenfield_option`, `security_rationale`, `delivery_mode` (domestic / allied /
diversified_global / mixed: Preserve domestic, Catch Up domestic or mixed, Partner allied where trusted
partners hold 30% of world supply else diversified global, Deploy diversified global or allied), `need_type`,
`need_high`, `prize_high`, `cp_high`, `decision_step` (secure_override / deployment_need / large_yard /
triage / not_assessed), `decision_trace` (compact JSON array of `{step, test, value, threshold, result}`: secure ->
need -> prize -> position -> pathway -> choice; the engine's full test trace is `engine_trace`),
`posture_confidence` (high / moderate / low: one point per borderline input, a still-possible Secure, an
unknown pathway deciding the node, an unmeasured Competitive Position where the rule reads it, weak
capability evidence, a consistency flag, fewer than two observed need families; not assessed is low;
Secure keeps the engine's confidence), `decision_borderline` (the
inputs within 0.05 of their threshold), `choice_possible` and `posture_possible` (the choices and
postures the borderline inputs and an unknown pathway allow), `pivotal_inputs`, `consistency_flag`,
`watch_reasons`, `choice_reason` (one plain sentence from the stored inputs); with the pathway's
`catch_up_anchor`, `catch_up_anchor_type`, `pathway_support`, `pathway_rule` and `pathway_credible_legs_1`,
`_legs_2`, `_anchor` (each rule's verdict, for the comparison), the strategic-value columns (`siv_*`,
`strategic_industrial_value`), the large-yard columns (`large_yard_*`) and the yard columns (`yard`,
`yard_rollup`, `yard_children`, `yard_units`, `yard_mixed`, `yard_fence_scope`). The engine's own Compete
reading is kept as `engine_posture`, `engine_variant` and the `t3`-`t5` columns; the 2026-10-28 quadrant as
`legacy_quadrant`, `legacy_quadrant_posture`, `legacy_quadrant_mode`, `legacy_quadrant_flag`,
`legacy_quadrant_borderline`.

### Layer-first yards and the roll-up

User spec 2026-10-30, Yards & Fences item 1: the technology x six-stage cell is too coarse for Yards &
Fences (grid-control software vs passive grid hardware, inverter firmware vs inverter hardware, BMS controls
vs battery cells, enrichment and reactor controls vs the rest of nuclear manufacturing, magnets vs the rest
of EV and wind components). So each **sensitive layer is a yard unit of its own**
(`yard_rollup.R::sp_layer_choice()`, `strategic_posture_layers_tbl`):

- **Secure** by the engine's own layer tests, exactly as before (a layer's Secure never comes from its cell);
- otherwise its **own strategic industrial value** (its own commons and option attributes with its cell's
  market prize, placed among the cells' scores) and its **own large-yard case** (its own attributes, and its
  own world supply where it has a measure or mineral of its own, else its cell's), with its **cell's need,
  Competitive Position and pathway** (layers have no trade data of their own), through the same rule,
  borderline and confidence machinery as the cells, with its own `choice_reason`. A control layer that misses
  Secure is therefore still classified on its parent's need, position and pathway; its own attributes keep it
  out of the large yard (digitally active) and, with process knowledge not assessed for software, usually cap
  its prize as market-only.

Each unit's choice maps to the paper's three yards (`sp_yard_of_choice()`): **small** (high fence) = Secure;
**medium** (medium fence) = Preserve, Catch Up, Partner, and Accelerate at Deployment & Use; **large** (low
fence) = Watch; not assessed has none. The cell's units are its stage row (the rest of the stage) and its
layers; `sp_yard_rollup()` gives the cell `yard` (the stage row's own), `yard_rollup` (the single yard where
the units agree, else `mixed_high_medium`, `mixed_medium_low`, `mixed_high_low` or `mixed_high_medium_low`,
named by the fences spanned), `yard_children` (a JSON array `[{layer_id, label, yard, choice, variant,
reason}]`, a single child still an array; `variant` carries a Secure child's fence scope), `yard_units`,
`yard_mixed` and `yard_fence_scope` (`extends_to_hardware` where a Secure child's control cannot be separated
from its hardware, `secure_integrated` or `secure_whole`, the paper's small-integrated and small-whole, so the
high fence covers hardware the stage row's own yard does not show; `unclear` where separability is not
established; `layer` otherwise; NA without a Secure child); every layer carries its own `yard` and its cell's
`yard_rollup`. Units not assessed do not enter the roll-up. The posture bundle carries them: in the core
`cells.yd` (the roll-up, coded), `cells.io` (the market prize beside `cells.prize`, the value the choice read,
named in `meta.prize_measure`) and `meta.yf_nodes` (since a54a9d3: one row per producing-stage node and layer with
its label, large-yard case and signals, and the strategic-value families that do not vary by country); in the detail
the prize the choice read and the market prize, the large-yard case only where the country's differs from its
node's, the fence scope where it reaches beyond the layer, and the child layers as `{layer_id, yard, choice,
variant}` (the drawer composes their reasons). At e41cb5b the dashboard decoders (`dashboard/src/posture/`) do not
read these fields yet and the drawer still labels the prize "Industrial Opportunity" though `cells.prize` now holds
the strategic industrial value; the dashboard copy-refresh work is bringing the page up to the bundle.

Current run (`yard_rollup_by_cell.csv`, `yard_layers.csv`): cells by roll-up small 181, medium 1,809, large 6,051,
mixed 1,196 (mixed_high_medium 320, mixed_medium_low 350, mixed_high_low 362, mixed_high_medium_low 164), not
assessed 4,887; `yard_fence_scope` `extends_to_hardware` on 439 cells and `layer` on 412. Layers (35 per economy,
7,490 rows for the 214 economies; `strategic_posture_layers_tbl` adds 175 for the five aggregates, which take no
choice): small 1,106, medium 1,724, large 4,022, not assessed 638. Small layers are control layers and magnets,
rare-earth separation, enrichment, fuel fabrication, grain-oriented steel and transformers where the engine finds an
acute vulnerability for the country.

**Departure from the paper, by the user's fence mapping (decided 2026-10-30).** Partner is a Medium-yard
choice, so high import need alone (no prize, no position) gives a Medium yard; in the paper's tool dependence and
concentration only add a diversification posture inside the Large yard (`gates.ts` `evaluateLargeYard`,
`large-diversify`), and strong learning alone is a Medium rationale, while here it raises the prize and still needs
a position, a pathway or high need to leave Watch. So next-generation PV and battery cells are a Medium yard in
seven of the eight combinations of need, position and pathway, and Watch (Large) for a country with low need, no
position and no pathway. The reproduction diagnostic shows the choice behind each agreement (below) so an agreement
by Partner (need) is not read as an agreement on capability. The user kept need-alone Partner in the Medium fence:
a documented departure.

**Control layers are Small only where coercive control is measured for the country (decided 2026-10-30).** A
control layer is Secure (Small) only when the engine's digital path holds for that country: remote command, update
or data access, harm within months, and at least 25% of the carrying hardware from coercive suppliers. Otherwise it
is classified like any other unit, its prize is usually market-only (process knowledge is not assessed for
software) and it is kept out of the large-yard case (digitally active), so with low need it is Watch (Large). Grid
SCADA, utility inverter firmware and reactor controls are therefore Small for Japan but Large for the United States
and Germany: a documented departure from the paper, which places them Small.

**Validation against the paper** (`yf_paper_reproduction.csv`, `sp_yf_reproduction()`): for each of the 21
curated Yards & Fences components, the unit the tool classifies (the layer linked by `yf_component_id`;
with none, the stage rows its three-stage label covers: Midstream = Components / Assembly), the derived yard
for the United States, Japan, Germany, India and Australia and the modal yard over all countries, the
choice behind the modal yard (`modal_choice`), the large-yard signal count of the first country's unit and its
distance from `min_signals` (`large_yard_n`, `large_yard_margin`: 0 = holds on exactly the threshold), the
large yards with a capability floor (`large_yard_floor`), the reasons, and the paper's `paper_placement`
beside it with agree / differ. This is the only place the
placement is read; it is never an input to a classification (tests check that no classification function
reads it). Diagnostics: `yard_rollup_by_cell.csv` (roll-ups by technology x stage), `yard_layers.csv`
(layer x choice and yard counts), `large_yard_case.csv` and `strategic_value_families.csv` (by node).

Current run: of the 19 components the paper places, the model's yard agrees for Japan on 17, Germany on 12 and the
United States on 10, and the modal yard over all economies on 7 (most small economies have low need and no position,
so most of their units are Watch). Battery-management and communications layers are small, and cells, cathode,
anode, power conversion and next-generation PV medium, in all three; cables and racking large; magnets mostly small;
PV modules large only on the threshold (5 of 8 signals) and Preserve (medium) where the cell holds a top-quarter
position (the United States, Germany); grid SCADA, inverter firmware and reactor controls small for Japan only (the
control-layer decision above); LEU and HALEU enrichment medium (Catch Up) where the paper has small. The full table is
in [decision_framework_report.md](decision_framework_report.md) section 28.

### The Secure override in detail (the posture engine)

The Secure override is the Yards & Fences decision engine (`posture_engine.R`, ported from the Draw the
Fence tool) applied to every node and sensitive layer. Its Compete tests (3-5) no longer decide anything;
they are diagnostics and supply the capability floor and the learning leg of the pathway test.

#### Nodes and sensitive layers

The node is the cell (`layer_id = "stage"`). Where six stages are too coarse, the cell carries
sensitive layers: control software and firmware, fuels, critical materials, specific hardware
(`posture_node_attributes.csv`: 66 cell rows, 35 layers, 41 country rows; the 21 Draw the Fence
components are linked and inherit their curated values). A layer's posture never changes its cell's:
inverter firmware can be Secure while Electric Grid Assembly is Deploy (the cell's `yard_rollup` then reads
mixed). Whole-cell nodes are judged on the physical path only; control vulnerabilities live in control layers.

Node attributes are curated assessments with sources and confidence (consequence, time to harm,
substitutability, qualification and reconstitution time, stockpile cover, whether making the node
builds transferable learning, maturity, leap candidacy, deployment criticality, market scope). They
describe the node, not a country; `Unknown` is used wherever the public record does not support a
value. Country rows hold only documented country facts (oil stock cover, a few gas and nuclear
inventories).

#### Supply structure

From `major_supplier_shares_tbl` and `geo_alignment_tbl`, per country x cell (and mineral): the
country's world share on the cell's primary measure (Atlas exports; IEA refining or mining shares for
minerals, weighted by the technology's demand share; traded supply for fossil fuels; none at
Deployment and Recovery), the largest **potentially coercive** supplier's world share, and **trusted**
supply (the country's own plus same-bloc partners'). Coercive suppliers of country i: opposite-bloc
partners, plus China (with Hong Kong and Macao) and Russia unless i shares their bloc. A nonaligned
country trusts every partner that is not coercive to it. On the import side: the share of the
country's own imports from coercive suppliers.

#### The five tests (tri-state, unknowns bracketed)

1. **Critical consequence**: the node's consequence is high or severe AND the country depends on it
   at scale: imports of at least USD 1m and 0.05% of GDP, at least 0.5% of world supply produced at
   home (or of the technology's components or assemblies, for its upstream inputs; an unlisted home
   share counts at its lower bound), or a place in the upper half of the technology's deployers (the
   Deployment position percentile).
2. **Exploitable vulnerability**
   - digital path (control layers): remote command, update or data access, harm within months, and at
     least 25% of the carrying hardware from coercive suppliers;
   - physical path: the largest coercive supplier holds at least 50% of world supply (of the
     country's own supply in regional markets such as pipeline gas) AND at least 25% of the country's
     imports come from coercive suppliers AND substitution is hard AND trusted supply could not be
     restored before harm becomes intolerable plus stockpile cover (restore time = qualification time
     when trusted supply holds 50% of the world, else the longer of qualification and
     reconstitution);
   - a country holding at least 10% of world supply is a major supplier itself (for a control layer at
     Deployment, of the technology's Assembly), and imports below USD 1m do not describe a country's
     supply (in a regional market they must also reach 0.05% of GDP): in either case the import legs
     are not met. **Concentration alone never satisfies Test 2.**
3. **Capability case** (strong / moderate / none), at producing stages only: present position (at
   least 10% of world supply; 3-10% only with complementary capabilities and in the stronger half of
   the country's own cells, since a large diversified exporter holds 3-10% of many baskets without
   specializing), industrial learning (counted only where the country has a base: a position,
   foothold or complementary capabilities), and a credible leap (leap candidates only). Complementary
   capabilities: yes = a 5% world share and a within-country strength (top 40% of the country's
   cells) in an adjacent stage of the same technology; partly = a within-country strength (top half)
   with at least a 0.5% world share there, or HS4 export proximity in the top quarter across countries.
4. **Credible strategy** (`compete_strategy`): preserve (present position); adjacent_capability (an
   established neighbouring stage and a learning node); catch_up (complementary capabilities, a
   learning node, a domestic market in the top quarter of deployers and a growing world market per the
   IEA WEO Stated Policies outlook); leap_ahead (a credible successor technology). At Extraction,
   adjacent and catch-up cases also need a resource base (reserves or resource above the floor). No
   strategy, no Compete: a globally important technology is not a reason by itself.
5. **Proportionality**: a moderate case fails when the domestic premium is above 30% (IEA ETP
   relative cost, regional), or when the premium is unmeasured, trusted partners (trusted supply less
   the country's own share) hold half of world supply and the good is deployment-critical, or for
   high-emission technologies (climate cost; GHG index below 0.5: coal, oil, gas). A strong case stays
   Compete as `compete_costly` (with a low fence). For Secure nodes the test shapes delivery and fence
   scope, not the posture.

**Posture.** Secure if Tests 1 and 2 hold; else Compete at a producing stage with a strategy and a
capability case proven at least moderate (its lower bound over the unknowns) that passes Test 5, or
proven strong; else Deploy. Variants: secure_control / integrated / whole / unclear (by
separability), secure_input, secure_both; compete_preserve, compete_adjacent_capability,
compete_catch_up, compete_leap_ahead, compete_costly; deploy, deploy_floor (a moderate case with a
credible strategy that fails on cost: keep a specified capability floor), deploy_diversify (coercive
supplier above 40% of world supply, or own-import HHI above 0.18 on material imports).

**Unknowns** are never filled. The tests are evaluated at the four corners of the unknown inputs
(security inputs at their open or security end x capability inputs at their weak or strong end):
`posture_possible` lists every posture the unknowns allow and `pivotal_inputs` those whose resolution
alone would narrow it. The recommendation is the strict reading: an unproven case for intervention
is not met. `posture_confidence` (low, moderate, high) reflects a settled posture, marginal decisive
answers, `evidence_completeness` (share of the inputs that can still matter which are known), the
curator's confidence and Competitive Position's evidence quality.

**Delivery mode**: Secure is domestic when the country holds 10% of world supply (of the driving
mineral, in a mineral cell), mixed with a foothold and allied supply, allied when partners hold at
least 30% of world supply, diversified_global when non-coercive supply is half the world, and mixed
otherwise, when no source holds enough trusted supply (new trusted capacity at home and with partners,
bridged by stockpiles); Compete is domestic for preserve and leap, mixed for adjacent and catch-up
strategies with allied supply; Deploy is diversified_global, or allied for a diversifying Deploy with
allied supply. A curated coercive share (HALEU: Russia 90%) applies only to countries for which the
named supplier is coercive.

#### Three Secure routes

Test 2 is met through one of three routes, each allowed node by node in an explicit rule table,
`config/security_route_rules.csv` (technology, six_stage, subnode, critical_function,
digitally_active, dual_use, systemic_essentiality, substitutability, reconstitution_time_class,
rationale, evidence_source, evidence_confidence, notes; checked by `sr_read_check()` against the node
attributes, whose substitutability and reconstitution time it must repeat). The routes are curated,
never inferred from trade; data only decide whether a country meets a route its node allows. There
is no quota.

- **operational_control** (route A): the digital path above, for control layers; or a **dual-use**
  node (enrichment, isotope separation, rare-earth separation and NdFeB magnets) where coercive
  suppliers carry at least 25% of the country's own material imports.
- **coercive_chokepoint** (route B): the physical path above. Concentration alone is not enough.
- **systemic_resilience** (route C): a node whose rule marks an essential function (grid transformers,
  switchgear, cables and electrical steel; reactor fuel; crude oil, refined products, gas, LPG and coal
  supply) where the country depends on imports (not self-supplied, material imports against GDP, net
  import dependence in the top half of the cell), supply takes a year or more to rebuild, and trusted
  partners (less the country's own share) hold under half of world supply. No coercive supplier is
  needed; the serious consequence is Test 1.

`security_rationale` names the primary route (operational control through a command path, else the
chokepoint, else the dual-use reading, else resilience); `security_routes_met` lists all. Variants:
the digital path keeps secure_control / integrated / whole / unclear, the chokepoint secure_input
(secure_both with the digital path), the dual-use reading secure_dual_use and resilience
secure_resilience. Every Secure variant is a high fence since 2026-10-30 (the fence follows the yard;
Policy design below), but its scope and instruments differ by rationale: around the control layer or the
dual-use end uses; around the chokepoint input, phased in as trusted supply qualifies; and for resilience
a fence that guards the essential supply itself (screening of foreign control of essential assets,
trusted long-term supply, stockholding), never import protection, since restricting imports would
deepen the shortfall it guards against (the build is reserves, long-lead procurement, spares and
trusted capacity agreements). Secure does not mean reshoring: delivery follows where trusted supply is.

### Allied strategy: partner recommendations and the allied division of labour

`R/strategic_position/partner_recs.R`. For each Compete . Partner node, and each Secure node whose mitigation relies on trusted foreign capacity (delivery allied, mixed or diversified_global; `partner_rec_for` = partner / secure): up to three trusted partners
(`recommended_partner_1..3`, each with `_capability`, `_cp`, `_relationship`, `_diversification`),
`partner_n_credible`, `partner_gap` (no trusted supplier with at least 2% of supply),
`partner_capability_source`, `partner_evidence_quality` and `partner_reason`.

- Trusted: the same rule as the engine's trusted supply (bloc trust; nonaligned importers trust every
  non-coercive partner); never the country itself, never a coercive supplier.
- Capability: the Allied Industrial Base position share (`aib_strategic.rds` evidence) averaged over
  the AIB nodes mapped to the cell, weighted by world trade x OSI coverage, where the AIB covers the
  cell (25 of the 66 cells: none for coal, gas, geothermal, green hydrogen or Deployment); elsewhere
  the world supply share (evidence `low`).
- Ranking: capability (against the best candidate) x relationship (Friendshore Index, reporter rating
  the partner as a supplier) x diversification (the fall in the reporter's import HHI if 10% of its
  imports moved from its coercive suppliers, or its largest supplier, to the candidate; 2024 dyads).
- The division matrix (`strategic_aib_division_tbl`, the Explorer's AIB headline): per node the
  primary allied holder, two backups, `redundancy` (allied holders with at least 2%) and `aib_gap`:
  no_trusted_supplier, single_point_of_failure (one holder with half of allied supply, or an
  irreplaceable member in the AIB leave-one-out), insufficient_redundancy (fewer than three holders),
  weak_evidence (no AIB node, or under half of its weight high-confidence), else none; with the Partner
  and Secure demand on the node.

AIB status (spec XVI.1): `partner_aib_status` covered | gap with `partner_gap_reasons`: no_trusted_supplier,
single_credible_supplier, weak_evidence, insufficient_allied_capacity (credible trusted partners together
below 30% of world supply, `partner_trusted_capacity`), excessive_concentration (a coercive supplier with
50% or more of world supply). A gap is no trusted supplier, a single credible one, or insufficient allied
capacity; weak evidence and excessive concentration are reported reasons, not gaps by themselves. A
genuine "no credible allied solution yet" is reported, never forced (current run: Partner 692 covered /
782 gap; Secure 110 / 96; two-leg run: Partner 1,003 / 847, Secure 56 / 118). The division matrix adds `allied_world_share`, `coercive_world_share` and the
same two capacity reasons after the original four in `aib_gaps` (the primary `aib_gap` is unchanged where
one of those applies), with `secure_foreign_demand` and `partner_aib_gaps` per node.

## 5. Policy alignment

Does observed policy match the choice? Downstream only (`sp_policy_alignment()`,
`sp_policy_alignment_flag()`, `sp_policy_alignment_confidence()` in `posture_engine.R`; config
`policy_alignment`). Rules, in order (design D5, spec X):

| choice | observed policy | `policy_alignment` | flag |
|---|---|---|---|
| not assessed, or no policy priority | | insufficient_data | |
| Partner | NIPO protection / localisation in the top third | misaligned (wrong treatment: autarky where trusted partners are the answer) | partner_autarkic |
| Partner | otherwise | aligned (domestic records cannot show partnership instruments) | |
| Secure, Preserve, Catch Up | NIPO and PAMS on opposite sides of 0.5 at least 0.5 apart, or a fence without a build (protection top third, support bottom third) | mixed | sources_disagree, fence_without_build |
| Secure | policy priority below 0.5 | under_prioritized | |
| Preserve, Catch Up | capability-building support (the NIPO support stock where measured, else policy priority) below 0.5 | under_prioritized | |
| Deploy . Accelerate | deployment-policy priority (PAMS, the only deployment source) below 0.5 | deployment_gap (under-response) | weak_deployment_policy |
| Deploy . Watch | protection / localisation in the top third (NIPO; at Deployment the technology's Assembly-equipment protection, i.e. local-content rules) | over_prioritized | watch_protection, watch_localisation_deployment |
| Deploy . Watch | NIPO observes the cell (a protection stock is recorded) but protection is not in the top third: a zero stock, or one too thin to rank (the economy protects fewer than three cells) | aligned (protection alone decides; no fallback to total attention) | |
| Deploy . Watch | NIPO does not observe the cell (no protection stock at all): total attention in the top third with an attributed record (producing stages) | over_prioritized, always low confidence (the total-attention fallback) | watch_attention |
| Deploy . Watch | the over test fires but other choices are still possible | insufficient_data | |
| otherwise | | aligned (restraint is success for Watch) | |

`policy_alignment_confidence`: high with both sources ranked, medium with one, low with none; one level
lower where the rule needed the NIPO support / protection split and it is not ranked (an observed zero
protection stock is a measurement, not a gap); at most medium at Deployment (PAMS only, by construction);
low whenever the total-attention fallback decides the alignment (design D5), whatever the number of
sources; insufficient_data is low. No overall policy score is computed.
Current run (`choice_policy_alignment.csv`): aligned 5,826; under_prioritized 310 (Catch Up 176, Secure 104,
Preserve 30); deployment_gap 182; over_prioritized 679 (all Watch: 151 producing cells on NIPO protection, 64
Deployment cells on the equipment's local-content protection, and 464 producing cells on total attention because
NIPO does not observe them, all low confidence); misaligned 59 (Partner); mixed 233; insufficient_data 6,835 (4,887
of them not assessed). Alignment confidence: high 2,096, medium 1,350, low 10,678. 1,412 Watch cells carry top-third
policy intensity (`watch_high_policy_intensity.csv`). The rise in over_prioritized follows the Watch count: the
large-yard case sends commodity cells to Watch and they keep their observed policy attention. Two-leg run: aligned
5,486, under_prioritized 516, over_prioritized 503 (122 / 65 / 316), misaligned 76, mixed 220, insufficient_data
7,141; under the one-leg rule (2026-10-29) over_prioritized was 375 and under_prioritized 947.

### Revealed Policy Priority

Where is the government concentrating observable intervention, relative to its own other cells?

**IEA PAMS.** Each policy x country is classed by what it names: tech- and stage-specific records
are attributed 1/k to the k cells they name (exact when k = 1, split otherwise); tech-specific but
stage-cross-cutting records are tech_wide, tech-cross-cutting records shared, and both cross_cutting:
these are reported, not ranked. Own policy = national, subnational, local and regional records plus
international records filed under one country. Stock = in force; flow = adopted in the trailing
window ending at the extract's as-of year (2025).

**NIPO, simplified.** From the raw GTA extract, mapped directly to the six-stage HS6 master (never via
the three-stage mapping, never expanded):

- one row per GTA intervention; EU-level acts collapse to `EUU` and are reported to members
  (`eu_supranational_*`), never added to their own counts; sanctions and liberalising measures are
  excluded and counted;
- `config/nipo_instrument_posture.csv` maps each of the GTA intervention types to support and/or
  protection, with a strength class (1 ordinary, 2 major or direct) used only in a sensitivity;
- a measure is **attributed** when it is narrow: at least 50% of its HS6 codes in the basket, or at
  most 20 codes with at least 10% in the basket. Its share in a cell is the share of its basket codes
  in the cell (a code shared by k cells counts 1/k), so shares sum to 1 per measure and summing
  across stages never counts a measure twice. Broad measures are **tech_wide** (one technology) or
  **cross_cutting**; measures with no basket code are **unmapped**. Only attributed measures enter
  cells. NIPO has no Deployment, Geothermal or Green Hydrogen evidence (no basket): those cells are
  no data, not zero;
- **stock**: implemented on or before the as-of date and not removed; grants, public finance, export
  support and state aid with no removal date count only for 5 years after implementation (GTA gives
  one-off measures no end date). **Flow**: announced in the trailing 1, 3 or 5 years;
- separately reported, never multiplied together: measures in stock, distinct instruments, new
  measures, identified subsidy and trade covered (USD), fiscal and trade data coverage, and the
  attributed / tech-wide / cross-cutting / unmapped shares.

The legacy Domestic Intervention Score (`nipo_policy_index.R`) is untouched and remains a research
output; it is no longer the basis of policy priority.

**Priority.** For source s and country i: attention `a = x_isc / sum_c x_isc` over the source's
observable cells; baseline `b_c` = mean attention in cell c over ranked countries; smoothed relative
attention `rca = (a + l) / (b + l)`, `l = 0.5 / n_cells` (a cell with no record stays lowest). The
priority is the within-country percentile of `rca` (countries need records in 3 cells). Columns:
`policy_support_priority` and `policy_protection_priority` (NIPO), `nipo_priority`, `pams_priority`,
`policy_priority_score` (mean of the available NIPO and PAMS priorities; `policy_priority_sources`
names them) and `policy_priority_percentile`; `policy_momentum` is the same on 3-year flow (1 and 5
years as sensitivities).

### Competitive-policy alignment (a diagnostic of the posture)

`competitive_policy_alignment` crosses the within-country Competitive Position percentile with the
within-country policy priority at 0.5: aligned_strength, latent_advantage, policy_led_bet,
low_priority (insufficient_data without both). Position is compared within the country because
across ~218 economies 94% of a large economy's cells sit above the world median.
`priority_gap` = within-country position percentile minus policy priority percentile over the cells
with both (positive: capability ahead of attention). It explains a posture; it does not set one.

## 6. Validation (spec Part XXI)

`scripts/27` writes `diagnostics/strategic_position/decision_review.md` (all six checks), `choice_cards.md`
(JPN, USA, DEU, KOR, IND, BRA) and `legacy_osi_corners_vs_choice_*.csv`; `scripts/26` writes the tables
behind them (`choice_by_country.csv`, `choice_threshold_sensitivity.csv`, `consistency_before_after.csv`,
`consistency_cells.csv`, `legacy_quadrant_vs_choice.csv`, `opportunity_validation_*.csv`,
`io_node_prize.csv`, `pathway_by_stage.csv`, `secure_selectivity.csv`, `deployment_review.csv`,
`manufacturing_vs_deployment.csv`, `watch_high_policy_intensity.csv`, `choice_policy_alignment.csv`).

Current run, 14,124 classified cells (`choice_overall.csv`): Secure 206, Preserve 103, Catch Up 355, Partner
1,474, Accelerate 514, Watch 6,585 (1,169 of them at Deployment, 2,133 decided by the large-yard case), not assessed
4,887 (4,216 producing cells without a measured supply-chain need, the same 18 nodes in every economy plus thin
cells; 671 Deployment cells without deployment need). Two-leg run (old data): Secure 174, Preserve 86, Catch Up 748,
Partner 1,850, Accelerate 514, Watch 5,457, not assessed 5,295; under the one-leg rule (2026-10-29): Catch Up 1,660,
Partner 1,592, Watch 4,803.

Position-choice consistency, before (the 2026-10-28 need x EO quadrant, recomputed on the current data) -> after,
both counted on the same Competitive Position and measured supply shares (`consistency_before_after.csv`; the
file's "after" label still reads 2026-10-29 but its counts are the current run's):

| contradiction | before | after | after, two-leg run |
|---|---|---|---|
| Partner with Competitive Position in the contenders' top quarter (contenders only) | 10 | 3 (all below the capability floor, flagged) | 3 |
| Partner with cross-country CP >= 0.75 | 415 of 1,521 (27%) | 275 of 1,474 (19%) | 396 of 1,850 (21%) |
| Catch Up with 10% or more of world supply | 11 | 5 | 6 |
| Catch Up with CP in the contenders' top decile (contenders only) | 10 | 0 | 2 |
| Catch Up with cross-country CP >= 0.9 | 50 of 115 (43%) | 152 of 355 (43%) | 173 of 748 (23%) |
| Preserve without capability (cross-country CP < 0.25 or < 0.5% of world supply) | 63 | 0 | 0 |
| Preserve below the contenders' median | 42 | 0 | 0 |

By construction Preserve needs a top-quarter position among contenders, so Preserve without capability is gone and
Partner despite a top-quarter position falls from 10 to 3. The anchor rule keeps only Catch Up cells with a real
base, so the Catch Up set sits higher in the cross-country ranking: 43% have a cross-country CP of 0.9 or more,
against 23% on the two-leg run. A cross-country top-decile CP is not a leading position in a node with 30 or more
contenders: these are contenders below the top quarter of crowded nodes, Catch Up by design, but they read as
"Catch Up despite a strong position". The remaining contender-level Partner cases fail the capability floor and carry
`review_top_quarter_below_floor`.

## 7. Policy design

`config/posture_policy_design.csv`, one row per variant: fence (level, scope, instruments), gate (level,
instruments), build (instruments), the purposes the variant needs, success metrics, benchmark, exit
condition and review interval. `config/posture_delivery_modes.csv` adds a build and gate note per delivery
mode and posture (allied Secure: build trusted capacity with partners through compacts, offtake,
co-investment and shared stockpiles, not at home).

**The fence height follows the yard** (user decision 2026-10-30, as in the Yards & Fences paper):
small yard (Secure) high, medium yard (Preserve, Catch Up, Partner, Accelerate) medium, large yard (Watch) low.
`policy_design.R::ppd_read_check()` refuses a `fence_level` that does not follow the yard (secure_* high;
compete_* and deploy_accelerate moderate; deploy, deploy_floor and deploy_diversify low). Current run: high 206,
medium 2,446 (compete 1,932, deploy_accelerate 514), low 6,585.

- **Secure** (high fence): a fence targeted at the vulnerable node (trusted-source and cybersecurity
  requirements, export and investment controls, adversary-source restrictions; for an input, phased in as
  trusted supply qualifies; for resilience, a guard on the essential supply itself, never import protection); a
  selective gate open to allied technology, investment, equipment and alternative inputs; a build that repairs the
  vulnerability (domestic or allied capacity, stockpiles, redundancy, surge, supplier qualification, trusted
  control architecture). The goal is mitigation, not maximum localization. The Explorer shows the first sentence
  of the build text as the node's mitigation.
- **Compete** (Preserve, Catch Up, Partner; medium fence): a calibrated, temporary, permeable fence (leap-ahead,
  costly and partner variants included, which had low fences until 2026-10-30); an open gate that keeps
  competitive pressure and learning; a build of production support, finance, infrastructure, workforce, R&D,
  clusters and supplier development, with benchmarks, review dates and sunset conditions; Partner builds with
  trusted partners.
- **Accelerate** (`deploy_accelerate`, medium fence, since 2026-10-30): open to every trusted supplier the
  build-out needs, with trusted-supplier and security conditions on public deployment support and connected
  equipment; a build that enables deployment.
- **Watch** (deploy, deploy_floor, deploy_diversify; low fence): open unless a specific problem justifies
  intervention: a low fence, a wide-open gate, and a build that enables deployment (infrastructure, permitting,
  interoperability, procurement, transparency); `deploy_floor` keeps a specified capability floor (a large yard
  with moderate learning), `deploy_diversify` adds supplier diversification.

Benchmarks and exit conditions say when to reclassify: a Secure node moves to Compete or Deploy when a
prolonged cutoff could be survived; Compete support sunsets unless benchmarks are met; Deploy is judged
on delivered cost, diversified supply, deployment pace and adequate resilience, never on domestic
production share, and is re-tested if concentration rises or a control layer is identified.

## Decision cards

`choice_cards.md` (scripts/27, `sp_choice_cards_md()`): per review country, the six-verb counts and up to
three clear examples of each choice with the engine's reason, rationale, delivery, partner, AIB status,
policy alignment and confidence. `decision_cards.md` keeps the engine's test-by-test cards (`sp_card_cases()`
picks one node for each of: Secure, Compete, Deploy, under-prioritized Secure, under-supported Compete,
over-prioritized Deploy, and high need with low domestic position delivered with allies), read from
`engine_trace`.

## Legacy OSI framework

The OSI paper framework scored every country x technology x stage on two composite indices and read the
result as a quadrant:

- **Energy Security Index (ES)**: trade, production, reserves, consumption, energy prices and imports,
  concentration and dependence, combined by category weights (`config/weights.yml`);
- **Economic Opportunity Index (EO)**: trade (market share, RCA, export size, feasibility, world HHI),
  production size and growth, investment (annual investment, operating capacity, pipeline, momentum),
  technology demand (world demand, regional demand, BCG addressable market, overcapacity), technological
  readiness (TRL), energy prices, input cost, foreign dependency, consumption and energy access;
- the **corners** of each country's sectors, split at the country's own median sector (Country fact
  sheets, `legacy_corner`): Grow (high EO, high ES), Upgrade (high EO, low ES), Exposed (low EO, low ES),
  Stable (low EO, high ES);
- the **Strategic Index (SI)**: EO, a security need term, policy, a supply-chain-stage weight and a
  technology emissions weight; its top sectors were the "priority sectors".

All are kept, unchanged and byte-stable, for replication: `index_outputs.rds` (`energy_security_index`,
`economic_opportunity_index`, `strategic_index`), `strategic_index_legacy` and `legacy_eo_opportunity` in
the strategic position table, the paper figures, and the Explorer's Evidence & Methodology -> Legacy OSI
section and the Strategic views (labelled "Legacy OSI framework"). They are not a current recommendation.

**Conceptual mapping.** Grow ~ Preserve, Upgrade ~ Catch Up, Exposed ~ Partner, Stable ~ Watch.

**Why the results differ.** ES and EO each mix capability, vulnerability, production, trade, investment,
demand, reserves, concentration and deployment. A Grow corner can be high on EO because the country is
cheap (input cost) or already exports a lot (capability), and high on ES because it produces or holds
reserves; the canonical framework separates those questions: need is only dependence on external supply
(or, at Deployment, the system's need for more deployment), the prize is only the market, and position is
only demonstrated capability. The corners are also relative to the country's own median sector, while the
choice compares countries within a node. Against the 6,351 country x six-stage cells with a corner (current
run): Grow -> Preserve 4%, Upgrade -> Catch Up 4%, Exposed -> Partner 18%, Stable -> Watch 55%
(`legacy_osi_corners_vs_choice_pairs.csv`; two-leg run 3%, 6%, 26%, 44%). The disagreements come from cells not
assessed now (1,119), the strategic industrial value (1,072 cells: a legacy EO high on input cost or capability, or
low where world demand or industrial commons are high), Deployment decided on its own need (1,053), the large-yard
case (1,031), the cleaned need (449), Competitive Position (374) and Secure (165)
(`legacy_osi_corners_vs_choice_drivers.csv`).

### The need x EO quadrant (the posture rule of 2026-10-28)

Between 2026-10-27 and 2026-10-29 the producing-stage posture came from Strategic Need x the legacy EO
placed among the node's contenders (`decision_quadrant.R`, kept as `legacy_quadrant_*` columns for
comparison only): high need + high EO = Catch Up, high need + low EO = Partner, low need + high EO =
Preserve, low need + low EO = Watch, either missing = insufficient data shown as Deploy; Deployment was
always Deploy. Competitive Position was a consistency check only. It was replaced because Need x
Opportunity alone assigned Preserve vs Catch Up (Preserve meant low need, not strong position), because
within a node the legacy EO ranks countries mainly by input cost (median Spearman 0.77) and capability
(0.57), and because 60% of producing cells were silently Deploy for lack of an EO value.
`legacy_quadrant_vs_choice.csv` gives every transition with its driver.
