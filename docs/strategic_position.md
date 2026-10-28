# Strategic Position and Strategic Posture

Branch `feat/supply-chain-stages`, 2026-10-27. Results and validation are in
[strategic_position_readout.md](strategic_position_readout.md); the decision cards are in
`diagnostics/strategic_position/decision_cards.md`.

The unifying question:

> Where should a country secure critical vulnerabilities, where should it compete industrially, where
> should it deliberately prioritize affordable deployment instead, and does its current policy mix
> match those choices?

## One system in four steps

| Step | Question | Output (per country x technology x six-stage node) |
|---|---|---|
| 1. Strategic Position | What does the evidence say? | Competitive Position, Strategic Need, Revealed Policy Priority, evidence quality |
| 2. Strategic Posture | What kind of problem is this? | Secure, Compete or Deploy, with the five decision tests and their trace |
| 3. Policy Design | How should government intervene? | Fence, Gate, Build, by posture variant and delivery mode |
| 4. Evaluation | Is policy aligned, and is it working? | Observed vs recommended policy; benchmarks, review dates, exit conditions |

Step 1 is the evidence layer; it has no composite score. Step 2 is the Yards & Fences decision
engine (small yard = Secure, medium yard = Compete, large yard = Deploy) applied to every node. It is
categorical: there is no yards index, fence index or posture score. Step 3 is the Gates & Fences
policy design. Step 4 compares what governments do with what the evidence recommends.

**Hard rule.** Observed policy never enters a Step 2 test. Revealed Policy Priority is compared with
the recommendation afterwards. Otherwise the model is circular: government prioritizes it, therefore
it is strategic, therefore government is right to prioritize it.

The old question was "what is a country's overall Strategic Index score in a technology x stage?".
The Strategic Index mixed demonstrated capability, vulnerability, policy attention and two normative
priors (a supply-chain-stage weight and a technology emissions weight). It is kept, unchanged, as
`strategic_index` in `index_outputs.rds` and as `strategic_index_legacy` in the new table, for the
dashboard export and the paper. It is deprecated.

### Principles

- **Six stages are the canonical taxonomy** (Extraction, Refining, Components, Assembly, Deployment,
  Recovery). Upstream / Midstream / Downstream is a derived roll-up (`sp_legacy_rollup()`), for
  compatibility and charts only.
- **Source evidence can be coarser than six stages.** Every input carries an evidence status (exact,
  split, shared, proxy, tech_wide, no_data), and each cell reports how much of its score rests on
  stage-specific observations. Evidence quality is reported next to a score and never changes it.
- **The three dimensions are independent.** A country can have any combination of high or low
  Competitive Position, Strategic Need and policy priority; none of them is computed from another.
- **Policy activity is not evidence of competitive advantage**, and a low observed policy priority is
  low *observed* NIPO / IEA PAMS intervention, not proof that the government gives the node no
  priority (GTA records no standards, technical regulation, most domestic regulation or agreed
  international measures, and NIPO has no Deployment evidence).
- **CAT is context**, not technology x stage policy evidence (`cat_context`, country-level).
- **No stage prior and no technology prior** in any default output. Stage preferences exist only as
  named scenarios (`stage_scenarios`) for the legacy-index sensitivity diagnostics.
- **The framework informs** specialization, build / partner decisions, Gates & Fences analysis and
  allied industrial-base strategy.

### Code and data

| Piece | File |
|---|---|
| Rules and thresholds | `config/strategic_position.yml` |
| Evidence metadata | `data/reference/index_variable_stage_evidence.csv` (machine-readable; cites `map_id` in `index_variable_stage_map.csv`), `R/utils/variable_stage_evidence.R` |
| Competitive Position | `R/strategic_position/competitive_position.R` |
| Strategic Need | `R/strategic_position/import_exposure.R`, `strategic_need.R` (import exposure built by the scripts/15 hook) |
| Revealed Policy Priority | `R/strategic_position/policy_priority.R`; NIPO in `R/categories/policy/nipo_stage_policy.R`, `config/nipo_instrument_posture.csv`, `scripts/11_build_nipo_stage_policy.R` |
| Competitive-policy alignment (diagnostic) | `R/strategic_position/alignment.R` |
| Posture: node attributes | `data/reference/posture_node_attributes.csv`, `R/strategic_position/posture_nodes.R` (inherits `data/reference/yards_fences_assessments.csv`) |
| Posture: supply structure | `R/strategic_position/supply_structure.R` |
| Posture: engine and inputs | `R/strategic_position/posture_engine.R`, `posture_inputs.R` |
| Policy design | `config/posture_policy_design.csv`, `config/posture_delivery_modes.csv`, `R/strategic_position/policy_design.R` |
| Decision cards | `R/strategic_position/decision_cards.R` |
| Diagnostics | `R/strategic_position/sp_diagnostics.R`, `legacy_si_sensitivity.R` |
| Build | `scripts/26_build_strategic_position.R` |
| Review charts and cards | `scripts/27_review_strategic_position.R`, `R/charts/strategic_position_plots.R` |

Outputs: `<outputs>/strategic_position_tbl.rds|csv` (one row per Country x tech x stage),
`strategic_posture_layers_tbl.rds` (sensitive layers), `strategic_position_inputs.rds` (long input
tables), `diagnostics/strategic_position/*.csv` and `decision_cards.md`, and review PNGs in
`<processed>/charts/strategic_review/` (not tracked). Pipeline order: 10 -> 11 -> ... -> 20 -> 15 ->
25 -> 26 -> 27.

Shared conventions: functions in `R/` are pure; percentiles are mid-rank, `(rank - 0.5) / n` over
the non-missing values (a singleton gets 0.5); missing is not zero unless the config names the fill.

## Step 1. Strategic Position

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
  EO's single-year `market_share` (Atlas 2023) and `rca` (Comtrade 2024). An economy that does not
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
  The posture does not read Competitive Position (Secure reads supply shares, the quadrant reads need
  and opportunity), so momentum moves postures only through the quadrant's consistency flags.

Fields: `trade_benchmark_period`, `trade_benchmark_source`, `trade_benchmark_share`,
`trade_benchmark_rca`, `trade_benchmark_exports`, `trade_momentum_period`,
`trade_momentum_previous_period`, `trade_months_observed`, `trade_momentum_quality` (ok, caution,
no_monthly_data, incomplete_coverage, small_base, hs_break, reporter_mismatch, thin_cell,
outside_universe), `trade_momentum_coverage`, `trade_momentum_raw`, `trade_momentum_score`.

### Strategic Need

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
0 by construction), so its risky family rests on the China share. Deployment takes the technology's
Assembly need (proxy) and, for coal, gas and oil, Extraction fuel import dependence (shared). No
world constant and nothing from Competitive Position enters.

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

## Step 2. Strategic Posture

### Nodes and sensitive layers

The node is the cell (`layer_id = "stage"`). Where six stages are too coarse, the cell carries
sensitive layers: control software and firmware, fuels, critical materials, specific hardware
(`posture_node_attributes.csv`: 66 cell rows, 35 layers, 41 country rows; the 21 Draw the Fence
components are linked and inherit their curated values). A layer's posture never changes its cell's:
inverter firmware can be Secure while Electric Grid Assembly is Deploy. Whole-cell nodes are judged
on the physical path only; control vulnerabilities live in control layers.

Node attributes are curated assessments with sources and confidence (consequence, time to harm,
substitutability, qualification and reconstitution time, stockpile cover, whether making the node
builds transferable learning, maturity, leap candidacy, deployment criticality, market scope). They
describe the node, not a country; `Unknown` is used wherever the public record does not support a
value. Country rows hold only documented country facts (oil stock cover, a few gas and nuclear
inventories).

### Supply structure

From `major_supplier_shares_tbl` and `geo_alignment_tbl`, per country x cell (and mineral): the
country's world share on the cell's primary measure (Atlas exports; IEA refining or mining shares for
minerals, weighted by the technology's demand share; traded supply for fossil fuels; none at
Deployment and Recovery), the largest **potentially coercive** supplier's world share, and **trusted**
supply (the country's own plus same-bloc partners'). Coercive suppliers of country i: opposite-bloc
partners, plus China (with Hong Kong and Macao) and Russia unless i shares their bloc. A nonaligned
country trusts every partner that is not coercive to it. On the import side: the share of the
country's own imports from coercive suppliers.

### The five tests (tri-state, unknowns bracketed)

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

### Three Secure routes

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
secure_resilience. The fence differs by rationale: high around the control layer or the dual-use
end uses, targeted on a chokepoint input, none for resilience (restricting imports would deepen the
shortfall it guards against; the build is reserves, long-lead procurement, spares and trusted
capacity agreements). Secure does not mean reshoring: delivery follows where trusted supply is.

### The decision quadrant inside the medium yard

After the Secure override, at the producing stages, the posture comes from two axes (no new score):

| | Industrial Opportunity high | Industrial Opportunity low |
|---|---|---|
| **Strategic Need high** | Compete, `catch_up` | Compete, `partner` |
| **Strategic Need low** | Compete, `preserve` | Deploy (`decision_quadrant = watch`) |

- Strategic Need: `strategic_need_percentile` (across countries within the cell).
- Industrial Opportunity: the EO index placed among the node's **contenders**, the economies holding
  at least 0.5% of world supply in the node (`opportunity_percentile`; with fewer than five contenders,
  among the peers: coalition members and economies with GDP of at least USD 200bn). The EO index is
  compressed (median 0.48, 90th percentile 0.58), so no absolute threshold separates opportunity, and
  its category mix differs by technology, so a percentile pooled across technologies is not
  comparable. Coal (a declining world market) is capped below the thresholds. Never within a country.
- Thresholds 0.67 on both axes (`strategic_posture.decision_quadrant`), tested at 0.60 / 0.67 / 0.70
  (`quadrant_threshold_sensitivity.csv`, `posture_threshold_sensitivity.csv`). A node within 0.05 of a
  threshold is `quadrant_borderline`, loses confidence, and its `posture_possible` and
  `pivotal_inputs` include the neighbouring quadrant.
- Deployment keeps the engine's posture (Deploy unless a control layer is Secure); its quadrant is
  reported for information. A node without either axis is `insufficient_data` and Deploy.
- Competitive Position is a consistency check, never an override (`quadrant_consistency_flag`):
  catch_up where the country is already in the top 10% (`catch_up_already_leading`), preserve below
  the bottom quarter or with under 0.5% of world supply (`preserve_without_capability`), partner in the
  top quarter (`partner_despite_capability`).
- The engine's own Compete reading (Tests 3-5) is kept as `engine_posture` / `engine_variant` and the
  `t3`-`t5` columns, as a diagnostic.
- Delivery: preserve domestic; catch_up domestic or mixed; partner allied where partners hold 30% of
  world supply, else diversified_global.

### Partner nodes and the allied division of labour

`R/strategic_position/partner_recs.R`. For each Partner node: up to three trusted partners
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

Output columns: `strategic_posture`, `posture_variant`, `posture_confidence`, `evidence_completeness`,
`posture_possible`, `pivotal_inputs`, `posture_primary_rationale` (security, capability, openness),
`posture_secondary_rationale`, `compete_strategy`, `delivery_mode`, `t1_consequence` ...
`t5_proportionality` (with path, coercion, position, learning and leap sub-flags), `decision_trace`
(JSON: each test's state, marginal flag, detail and inputs with provenance data / assessment /
unknown), and `sensitive_layers_secure`.

## Step 3. Policy Design

`config/posture_policy_design.csv`, one row per variant: fence (level, scope, instruments), gate
(level, instruments), build (instruments), the purposes the variant needs (from the Draw the Fence
`needsFor()`), success metrics (`successMetrics()`), benchmark, exit condition and review interval.
`config/posture_delivery_modes.csv` adds a build and gate note per delivery mode and posture (allied
Secure: build trusted capacity with partners through compacts, offtake, co-investment and shared
stockpiles, not at home).

- **Secure**: a fence targeted at the vulnerable node (trusted-source and cybersecurity requirements,
  export and investment controls, adversary-source restrictions); a selective gate open to allied
  technology, investment, equipment and alternative inputs; a build that repairs the vulnerability
  (domestic or allied capacity, stockpiles, redundancy, surge, supplier qualification, trusted control
  architecture). The goal is mitigation, not maximum localization.
- **Compete**: a calibrated, temporary fence; an open gate that keeps competitive pressure and
  learning; a build of production support, finance, infrastructure, workforce, R&D, clusters and
  supplier development, with benchmarks, review dates and sunset conditions.
- **Deploy**: open unless a specific problem justifies intervention: a low fence (genuine trade
  remedies, forced-labour restrictions, basic security standards), a wide-open gate, and a build that
  enables deployment (infrastructure, permitting, interoperability, procurement, transparency).

## Step 4. Evaluation

`policy_alignment` compares the recommendation with Revealed Policy Priority:

- Secure or Compete with policy priority >= 0.5: aligned; below: under_prioritized;
- Deploy with policy priority >= 0.67 and an attributed record: over_prioritized; otherwise aligned
  (restraint is success); at Deployment, deployment support is what Deploy asks for (aligned);
- mixed when NIPO and PAMS sit on opposite sides of 0.5 at least 0.5 apart, or a Compete node has
  top-third protection with bottom-third support (a fence without a build);
- insufficient_data without policy data, or where Deploy is not settled (the unknowns still allow
  Secure or Compete: an unproven case is not a weak one).

Benchmarks and exit conditions (Step 3 table) say when to reclassify: a Secure node moves to Compete
or Deploy when a prolonged cutoff could be survived; Compete support sunsets unless benchmarks are
met; Deploy is judged on delivered cost, diversified supply, deployment pace and adequate resilience,
never on domestic production share, and is re-tested if concentration rises or a control layer is
identified.

## Decision cards

`sp_card_cases()` picks one node for each of: Secure, Compete, Deploy, under-prioritized Secure,
under-supported Compete, over-prioritized Deploy, and high need with low domestic position delivered
with allies (which may be a sensitive layer); review countries, confidence, settled postures and
evidence quality come first. `sp_decision_cards_md()` renders them with the tests, alignment, policy
design and evaluation.
