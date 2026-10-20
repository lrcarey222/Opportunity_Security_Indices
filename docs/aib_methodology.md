# Allied Industrial Base (AIB): trade partnerships, investment prioritization and Sectoral Compacts

(`config/aib.yml`; pure functions in `R/aib/`; IO in `scripts/31_build_aib_trade_network.R`,
`scripts/32_build_aib_partnerships.R` and `scripts/84_export_aib_data.R`; tests
`tests/testthat/test-aib-*.R`; dashboard view `dashboard/src/aib/`.)

The Allied Industrial Base is treated as an **emergent network of bilateral trade and investment
relationships**. Sectoral Compacts are the institutional structures that coordinate groups of those
relationships where joint action materially improves supply-chain resilience. The module is a layer
beside the OSI indices and the allied network design (`scripts/30`, section 10 of
`docs/methodology.md`); it reads their inputs and changes neither.

The chain:

```
observed trade network -> supply-chain node shock -> existing-relationship response
  -> optimized trade partnerships -> coordination dividend + specialization/resilience frontier
  -> residual capacity gaps -> marginal investment partnerships -> emergent AIB network
  -> recurring, complementary relationships grouped -> Sectoral Compact candidates
  -> node-specific policy treatment (the Gates & Fences / Yards & Fences handoff)
```

What it does **not** do, by design: no composite AIB score; no alliance fixed in advance as a
supplier set (the economies the AIB protects, the **demand scope**, and the economies that may supply
them, the **candidate-partner universe**, are separate settings: by default the allied core and
partner tiers are protected and any economy may supply them); no assumption that maximum diversification is
optimal; no reading of observed trade as spare physical capacity; no reading of the synthetic
investment increment as a project; Friendshore never qualifies a host that lacks industrial
capability; no assumption that every technology needs a compact or that every node of a technology
belongs in one. Every one of these outcomes is reachable: no intervention, existing trade
sufficient, new bilateral coordination helps, new capacity required, concentrated specialization
worth preserving, no compact needed, several compacts in one technology.

## 1. Audit: what the module reuses

| Need | Reused from |
|---|---|
| Index > sector > stage > HS6 (the nodes) | `data/reference/electro_industrial_hs6_dictionary.csv` (core, primary rows); the OSI basket `data/reference/energy_hs6_master.csv` for the master taxonomy and the crosswalk of the partner scores |
| Bilateral flows (importer's declaration first, else exporter's x 1.06) | `partnership_strength_build_trade_cells()` run at node grain through a pseudo-crosswalk (`aib_node_crosswalk()`) |
| RCA (Balassa) | `trade_core_build_comtrade_rca()` on node exports |
| Partnership Opportunity (the OSI index name; the outputs keep the column `export_opportunity`), exporter -> market | `partner_opportunity_tbl.rds`, variable `Opportunity Index` |
| Friendshore, investor/buyer -> host | `partner_friendshore_tbl.rds`, variable `Friendshore Index` |
| Production / capacity shares | `major_supplier_shares_tbl.rds` (IEA refining and mining), GCIM operational capacity |
| Policy / climate | IEA PAMS (`policy_outputs.rds$policy_agg`), CAT ratings (`cat_prepare_ratings()`, `partnership_strength_clean_policy()`) |
| Coalition (the default demand scope: core and partner tiers) | `coalition_tbl.rds` |
| LP solver | GLPK via Rglpk (installed with the allied network's MILP stack) |

The allied network design is normative and closed (index scores, no USD, no shocks, no sub-sector
grain); the AIB module is built on the observed USD flows instead and does not reuse its objective
or its GDP-derived portfolio caps.

## 2. Nodes

Since 2026-10-19 the nodes come from the **electro-industrial HS6 dictionary**
(`data/reference/electro_industrial_hs6_dictionary.csv`; `docs/electro_industrial_dictionary/`), not
from the OSI energy basket. The taxonomy is a setting: `taxonomy` in `config/aib.yml` (or
`--taxonomy`) picks the overlay `config/aib_taxonomy_dictionary.yml` (default) or
`config/aib_taxonomy_master.yml` (the OSI basket, `data/reference/energy_hs6_master.csv`, kept for
comparison runs). Every taxonomy-specific setting lives in the overlay: the node source and splits,
the strategic scope, the supply-chain and material links, the production proxies and the validation
cases.

**The dictionary.** RMI's Electro-Industrial Stack: 6 indices (A Materials & Inputs, B
Electrotechnologies, C Grid, D Generation, E Electrified End Uses, F Industrial Transition), 39
sectors, HS2022 codes, each with one stage (Upstream / Midstream / Downstream), a specificity (1
dedicated, 2 predominant, 3 shared) and exactly one `primary` row. The headline uses the **core**
scope (182 codes in 32 sectors: the dedicated and predominant codes, specificity 1-2, plus 12 shared codes the
dictionary keeps as core with a written justification; seven sectors, such as HVDC, charging and stationary
storage, have no HS line of their own); `--dictionary-scope extended` adds the 136 shared codes as a
sensitivity (`aib_extended/`). Only primary rows are used, so every code sits in one node and
sector totals never count a code twice (the dictionary's own aggregation rule). Fossil fuels are not
in the dictionary, so every node is in the strategic scope.

**Node grain.** The hierarchy is index > sector (the AIB's "technology") > stage > node. A node is a
sector x stage cell (`aib_dictionary_master()`), split where the cell mixes physical chains the
compacts and the physical constraint must keep apart (`nodes.splits`; a split names parts by HS6 code
and sends the rest to `rest`): rare-earth ores / oxides & compounds / metals; lithium minerals,
lithium chemicals, nickel and cobalt intermediates, manganese dioxide, battery recycling feed; wafers
/ gallium-germanium-indium / piezo quartz; copper ores / smelting & refining / wire & rod, aluminium
wire rod; GOES / NGO steel; uranium ores / conversion & enrichment / fuel assemblies / special
materials; Li-ion vs other accumulators; NdFeB vs ferrite magnets; electric motors vs AC generators;
PV cells / cover glass / modules / solar thermal; blades / balsa / towers / turbines; overhead
conductors / power cables / insulators; electric cars / buses and trucks / rail / two-wheelers;
refrigerants / compressors / heat pumps; hydrogen derivatives vs biofuels. On the 2026-10 data: 78
nodes, 74 optimized (cobalt ores, monazite and the uranium ores / spent fuel aggregate are too thin).

**The OSI crosswalk.** Partnership Opportunity, Friendshore and the PAMS policy compatibility are kept by
OSI technology x stage. `aib_osi_crosswalk()` places each node in the OSI cells its codes sit in,
weighted by 2024 world imports: `score_techs` (OSI technologies holding at least 20% of the node's
matched trade) and `score_stage` read the scores; `yf_cells` and `osi_subcomponents` are the Yards &
Fences / Sector Profile ids through which the dashboard joins nodes to OSI cells. 56 of 77 nodes have
basket codes (median coverage 100% of their trade); the rest (heat pumps, refrigerants and
compressors, electric furnaces, electrolysers, hydrogen derivatives and biofuels, hydro turbines,
electric rail, PGMs, silver powder, piezo quartz, balsa, aluminium wire rod, sensors) get neutral
tie-break scores and no OSI cell.

**Trade data.** The dictionary needs 155 codes no earlier pull held; they were pulled on 2026-10-19
with `scripts/98` unchanged (`osi-worktrees/scratch_eid`, combined with the live dyads, the existing
codes verified identical). `scripts/31 --dyads <file>` reads that combined file until it is installed
as `data/raw/comtrade_energy_dyads_all.csv`.

With the master taxonomy a node is a `(tech, supply_chain, sub_sector)` cell of the basket, cells
with an identical HS6 set in several technologies are one physical node, and the October 2026
configuration splits Solar `Polysilicon & Wafers` into `Polysilicon` and `Wafers`.

The HS6 codes are the empirical trade data, the node is the optimization unit, the stage is the
aggregation and policy frame and the sector (technology) a system summary. **Every metric is
recomputed from flows summed to the node; no HS6-level score is averaged.**

## 3. Observed trade network

Flows are 2024 (`trade.year`) exporter -> importer USD at node grain from the all-reporter Comtrade
dyads, reconciled by the partner themes' rule: the importer's declaration (CIF) when the importer
reports the node that year, otherwise the exporter's declaration x 1.06 (mirror), otherwise
unobserved. Self-dyads are dropped; flows under `trade.min_flow_usd` are not relationships.

Per node: importer **demand proxy** (its observed imports: trade-based, so domestic production used
at home is outside the model), supplier shares, import HHI, largest supplier; exporter observed
exports, **world export share**, markets served and **RCA** (node share of the economy's total
goods exports over the world's; totals from `comtrade_total_export.csv`, earlier bulk-total years for
non-reporters, recorded in `total_year`); production share where a proxy exists.

**Observed exports are a proxy for revealed tradable capability, not physical spare capacity.**

## 4. Data-quality guardrails

`aib_data_quality()` flags each node and assigns `High confidence`, `Usable with caution` or
`Insufficient for optimization` (`data_quality` thresholds):

* insufficient: world trade below USD 50m, fewer than 3 material exporters or 5 material importers;
* caution: mirror flows >= 30% of value; RCA missing for exporters with >= 20% of exports; most codes
  non-essential; general-purpose codes "kept with caveat"; a code also counted in another node
  (multi-use, e.g. wafers 3818.00 with Semiconductors); legacy-HS bridges; HS2022 codes with no 2020
  equivalent; **solar**: PV cells (8541.42) and modules (8541.43) split in HS2022, and 2020 trade
  exists only as 8541.40 with LEDs (filed under cells), so no cells-vs-modules time series;
  production far more concentrated than trade (`aib_production_flags()`: e.g. China 84% of
  rare-earth refining against 16% of 2846.90 exports), where trade-based exposure understates
  dependence.

Insufficient nodes are **aggregated upward** (`aib_quality_merge_map()`): into a sibling linked by a
material flow in the same technology stage first (monazite into rare-earth compounds, uranium ores
into natural uranium compounds), else together within their technology stage; a node still alone
is reported and not optimized.

## 5. Stress-test engine and the Trade Partnership Network

**Shocks** (`shocks`): China 100% (`china_100`), China 50% (`china_50`), and the largest supplier
removed at sub-sector, stage or technology level (`largest_subsector`, `largest_stage`,
`largest_technology`; the largest exporter of the trade summed over that level's nodes). Hong Kong
and Macao re-exports follow China (`trade.reexport_hubs`).

**The diversification scenario** (`diversify_benchmark`, a shock of type `diversify`;
`R/aib/aib_diversify.R`) removes no supplier. It asks what trade and investment would look like if a
supply-chain step were as diversified as its higher-level sector: electric cars as diversified as cars
in general, lithium-ion cells as accumulators in general.

* **Benchmark** (`benchmark: hs4`): the trade-weighted mean exporter HHI of the *other* HS6 lines of
  the node's parent HS4 heading(s), from the Atlas HS92 export panel (latest year, 2023; electric cars
  against the petrol, diesel and hybrid lines of 8703). A node's own lines in HS92 are the
  dictionary's `hs1992_join` codes and are left out. Where the heading has no other line, the
  benchmark falls back to `sector`: the trade-weighted mean HHI of the other nodes of the node's
  sector. Concentration is measured on world exports, with Hong Kong and Macao counted with China.
* **The cap.** A node more concentrated than its benchmark (by more than `min_excess`, 0.005 of HHI;
  a node within it counts as already as diversified) gets the
  supplier-share cap that brings its HHI down to the benchmark when every supplier above it is cut to
  it and the excess goes to the others in proportion (`aib_diversify_cap()`). Where the benchmark is
  below what the number of suppliers allows, the cap is equal shares and `cap_reaches_benchmark` is
  false.
* **The shock.** The cap binds on the demand scope's supply: every supplier above it there loses the
  excess of its sales to the scope (`excess_share`, a share of the scope's demand, equal to the run's
  shock exposure). The cap is a ceiling of cap x the scope's demand on every supplier's total, in the
  existing-network and the optimized states alike (`aib_node_problem(cap_basis = "demand")`); the
  specialization frontier's limits are shares of the supply served instead, and on that basis a
  supplier would be cut further whenever trade cannot refill the market, pushing coverage below the
  static state. A capped ally stays a buyer: its own purchases remain in the protected demand
  (`protect_shocked`), where an economy removed by a shock is not one. The rest of the chain is
  unchanged: existing relationships, optimized trade, the residual gap (the capacity diversification
  needs; with it filled, the cap and so the benchmark concentration hold), the marginal investment
  simulation (`investment.shocks`; capped suppliers are not hosts) and the node's class under this
  scenario (`class_diversify_benchmark`, empty where the scenario does not run). The emergent network
  and the country roles sum over the supply shocks only, without this scenario.

`aib_diversification.csv` has one row per node: current and benchmark HHI, the basis, the cap, the
capped suppliers, the supply to replace, the coverage after existing and optimized trade, the gap,
the leading new relationships and investment hosts. Nodes the scenario does not run on are listed
with their status: already as diversified as their benchmark; within the cap in the allied market
(above the benchmark worldwide, but no supplier to the demand scope exceeds the cap); or no
benchmark. They have no class under the scenario.

**Shock semantics** (`shocked_demand`, every one run, the first is the headline):

* **hold** (headline): an export / supply shock. The shocked economy keeps buying, so other
  exporters' sales to it stay with it in both scenarios (they compete for the same supply).
* **release**: a two-way cut. The shocked economy's purchases fall by the same fraction and that share
  of the exports headed to it is freed for others; a sensitivity. (The dashboard's "Decoupling"
  scenario is the headline shock under hold, section 13.) Release is therefore the more
  optimistic reading wherever third countries sell much to the shocked economy (rare-earth
  concentrates from Myanmar and Laos, Chilean lithium).

**Demand scope and candidate-partner universe** are separate (`demand_scope`,
`candidate_partner_universe`; modes `configured` (a list), `coalition` (tiers of `coalition_tbl.rds`)
or `global`; `--demand-scope` / `--partner-universe` override them). The **demand scope** is the set of
importers whose lost supply is restored (default: the allied core and partner tiers, 57 economies,
less the shocked economy); coverage is the share of their baseline traded demand served after the
shock and shock exposure the share lost. The **candidate partners** are the exporters that may open,
expand or redirect relationships toward the scope (default: every economy) with at least USD 1m
**and** 0.05% of the node's world exports (the larger bar binds); smaller exporters, and exporters
outside the universe, keep their current shipments but are not re-optimized. Every output carries
`demand_scope` and `candidate_universe`. Global demand with global partners (the first full run,
2026-10-15) is kept as a sensitivity: it treats every importer's shortfall, including the shocked
economy's other customers, as the AIB's problem. Open ship registries (Liberia, the Marshall Islands,
Panama, the Bahamas; `trade.exclude_from_universe`) are in neither set: vessels (HS 89, tankers and
platforms in the Oil and Gas baskets) trade through the flag they fly, so those economies appear as
large importers and exporters of ships. Their shipments stay as observed.

**Supply scenarios**, kept apart in every output:

* **conservative**: every surviving shipment into the demand scope stays where it is; only the supply
  the shock frees (under release) can move;
* **reallocation**: a candidate partner can also redirect its observed exports to markets outside the
  scope (and its sales to the shocked economy as far as release frees them) toward the scope. What it
  draws beyond its own shipments to the scope and its freed supply is reported as `diversion` (supply
  taken from buyers outside the scope).

**Physical supply constraint** (`supply_constraint`, every mode run, the first is the headline):

* **production_constrained** (headline): where credible production or capacity shares exist
  (`production_proxies`: IEA refining / mining shares, GCIM operational capacity; credible when the
  proxy covers at least `min_physical_completeness`, 80%, of world production), **physical evidence
  decides who can supply**. An exporter the data list as a producer (share above
  `producer_min_share`) keeps its full trade response; one they do not list (a re-exporter, stock, or
  a different product under the same HS code, such as semiconductor wafers from Japan, Taiwan and
  Korea in HS 3818.00, against GCIM's solar-wafer capacity) keeps its current shipments to the scope
  but cannot expand or redirect supply toward it. Shares are **not** converted into trade values:
  production is physical volume and trade is value, and where the dominant producer uses most of its
  own output (polysilicon, wafers) world trade is far below world production, so `share x world
  trade` would cap genuine producers (Chile in lithium chemicals, Germany in polysilicon) below their
  own exports. The share of world trade held this way is reported (`trade_share_without_physical_data`).
  In the 2026-10 data 11 nodes qualify (rare-earth compounds and metals, natural graphite, anodes,
  lithium chemicals, cobalt and nickel refining, polysilicon, wafers, solar cells and modules, battery
  cells), all at least 93% complete. Nodes without credible physical data keep their trade-only result
  and are labelled so (`physical_validation`).
* **trade_only**: observed exports alone bound supply (the earlier behaviour), kept beside it.

Both results are written (`supply_constraint` column); the classification table gives the class under
each. **Physical validation** (`aib_physical_validation()`): where physical and trade concentration
diverge by `divergence_threshold` (25 points, on the largest supplier's share or China's share) or more,
or physical data are incomplete, a LOW INTERVENTION or COORDINATE class is never presented without the
warning **PHYSICAL CAPABILITY NOT VALIDATED**: in trade-only mode whenever the data diverge; in the
production-constrained mode when the class rests on low **trade** exposure (the physical rule
validates who supplies, not exposure: polysilicon dependence arrives embodied in wafers and cells, so
its trade exposure is low). The warning is computed for the class under every variant (other shocks,
scenario, semantics, mode, growth bound: `classification_warning_<variant>`), not only the headline.

**States**:

* **A static**: the shocked supply disappears; nothing adapts.
* **B existing-network adaptation**: candidates reallocate only through relationships observed in
  2024 (plus keeping their own exports at home, `allow_domestic_retention`), and an existing
  relationship may grow to at most `1 + existing_relationship_growth` times its observed size
  (default: it may double). Without that bound any USD 10k relationship could carry unlimited volume,
  "existing relationships" were as flexible as new ones, and on the first full run (2026-10-15) the
  coordination dividend was ~0 in 89 of 91 nodes for that reason alone. Because the dividend (and so
  COORDINATE) depends on this bound, `existing_growth_sensitivity` reruns state B at about 1.25x, 2x,
  4x and unlimited (`allied_trade_growth_sensitivity.csv`: existing coverage, dividend and class per
  bound; `coordinate_sensitive_to_growth` marks a node whose COORDINATE class appears or disappears
  across the bounds).
* **C optimized partnerships**: any candidate may serve any importer in the demand scope.

`coordination_dividend = optimized_coverage - existing_coverage` (C - B) is the value of new or
expanded relationships, i.e. of coordination.

**Lexicographic optimization** (`aib_solve_state()`, `aib_lp_lexicographic()`), each objective
optimized only among solutions within the tolerance of the earlier optimum:

1. maximize demand served (`tolerance_coverage` 0.05%: a wider band let state B's tie-breaks buy
   Partnership Opportunity with coverage, which faked a coordination dividend);
   1b. stability, **a module addition to the four specified objectives**: cut surviving shipments as
   little as possible (`tolerance_stability`), so the optimizer never reshuffles trade the shock did
   not touch; only coverage or the supplier-share limit can require a cut. In reallocation,
   specialization therefore acts on the supply stability leaves free;
2. preserve revealed specialization: exporter weight `min(RCA, rca_cap) / rca_cap`, or the
   exporter's production / capacity share where a proxy exists and is larger (trade misses
   production used at home), ties by world export share (`aib_specialization_weight()`);
3. favour relationships with stronger Partnership Opportunity (exporter -> market);
4. revealed policy/climate compatibility: mean of the available components, IEA PAMS policy intensity
   similarity in the node's technology and stage and CAT rating similarity
   (`aib_compatibility()`). No geopolitical alignment score enters.

Missing tie-break scores are neutral (0.5). No objective is a weighted composite of another.

Outputs per node x shock x scenario: shock exposure, static / existing / optimized coverage,
coordination dividend, residual supply gap (share and USD), baseline / optimized HHI, largest
supplier share and top-three share, new and expanded relationships; stage and technology aggregates
recomputed from the summed USD (`aib_aggregate_levels()`), with their HHI and largest supplier
recomputed from the exporters' summed supply (`aib_aggregate_concentration()`). A node shared by
technologies whose largest supplier differs (rare-earth compounds and metals under the stage and
technology shocks) gets one run per target, kept apart by `target_iso` in every output.

In state C every candidate can reach every importer, so coverage and each exporter's total have a
closed form (`aib_optimized_analytic()`: largest `T <= fixed + sum_e min(avail_e, cap*T)`, then
stability, then specialization). It agrees with the LP (tests) and runs the frontier and the
investment simulation; the LP runs where the importer allocation is reported.

## 6. Diversification vs specialization

A configurable **maximum supplier share** caps any candidate's share of the demand scope's post-shock
supply; the frontier runs 25%, 40%, 60% and 100% (`max_supplier_share`) and records, per limit,
coverage, HHI, largest and top-three shares, the **specialization score** (supply-weighted
objective-2 weight of the optimized supply) and two N-1 readings of the resilience a tighter limit
buys:

* `instantaneous_n1_retained_coverage` = `coverage x (1 - largest share)`: the optimized allocation
  loses its largest supplier and nothing adapts (the mechanical reading, formerly "second-shock
  coverage");
* `n1_reoptimized_coverage`: the **true N-1 test**: the largest remaining supplier
  (`n1_removed_supplier`) is removed from the problem entirely (its shipments and its candidacy,
  `aib_remove_supplier()`) and the trade is re-optimized under the same limit.

Two readings answer the policy question:

* `tightest_costless_max_share`: the tightest limit whose coverage stays within 2 points of the
  unconstrained optimum (diversifying that far costs nothing against this shock);
* `min_required_diversification`: the **loosest** limit at which the true N-1 coverage still reaches
  `frontier_acceptable_coverage` (80%), i.e. the least diversification that buys acceptable
  resilience, with `specialization_retained` (specialization score there over the unconstrained one);
  `min_required_diversification_instantaneous` reads the mechanical N-1 instead. NA means no tested
  limit reaches it: the node needs capacity, not diversification.

Maximum diversification is not assumed desirable: where coverage falls as the limit tightens,
specialization pays. In the conservative scenario a supplier whose locked shipments already exceed
the limit may not expand; in reallocation the limit can require cuts, chosen to be as small as
possible.

## 7. Investment Partnership Prioritization (marginal capacity simulation)

Only after optimized trade, and only for nodes whose residual gap is at least `min_residual_gap`
(5%) under the simulated shocks and scenarios (`investment.shocks`, `investment.scenarios`: both
scenarios, kept apart). For each eligible host a
standardized synthetic increment (5%, 10%, 20% of the node's demand-scope demand) is added and the
optimizer rerun (headline shock semantics, both supply-constraint modes; hosts come from the
candidate-partner universe):

`marginal_resilience_gain = coverage_after_added_capacity - coverage_before_added_capacity`

with residual gap before/after, additional demand served, change in HHI and largest-supplier share,
the gain under the diversified cap (40%), host RCA / export share / production share.

**Eligibility** (`aib_eligible_hosts()`) requires revealed capability at material scale: RCA >= 1
on exports of at least USD 50m or 1% of the node's world trade, a production share >= 1%, a world
export share >= 0.5%, or RCA >= 1 at that scale in a node joined to it by a **material link** (in
either direction: the feedstock or the product). Open ship registries (`exclude_hosts`) are never
hosts. Friendshore plays no part in eligibility. Within a capability tier hosts are compared on
capability at scale: the larger of their world export share and production share in the node (RCA is
the gate, not the measure, because it saturates for small specialists).

**Ordering** (`aib_rank_hosts()`): (1) resilience contribution, (2) diversification contribution
(fall in HHI), (3) host specialization/capability (tier, then value), (4) Friendshore
(investor/buyer -> host, weighted by the importers' lost supply), (5) revealed policy/climate
compatibility. Without a supplier limit every host's increment adds the same coverage
(`min(increment, gap)`), so the resilience contribution used for ranking is the gain with the
diversified limit (40%) in force, where capacity at an already dominant supplier adds little; the
unlimited gain is reported beside it. Ties are measured from the group's leader: gains within 0.5
points of the best, HHI changes within 0.005 of the lowest. The investor -> host pairs
(which importers the new capacity serves, with their Friendshore) are worked out for the five best
hosts at the 10% increment on top of the optimized allocation: with every other flow fixed, the
lexicographic optimum is greedy, filling the importers' unmet demand in descending Export
Opportunity of the host to them, then compatibility.

This is a marginal capacity simulation, not a project-level investment optimization.

## 8. Emergent AIB network

`aib_network_pairs()` gives one record per directed bilateral relationship, oriented as goods move
(supplier -> buyer): trade edges as selected (exporter -> importer) and investment edges turned round
(host -> investor, the host's new capacity serving the investor). Only each node's first-ranked host
counts as a selected investment relationship, so alternative hosts are not summed. Per pair:
`n_trade_selected`, `n_investment_selected`, shocks, sub-sectors and technologies; participation in
high-severity nodes (exposure >= 40%); Upstream / Midstream / Downstream participation; headline
trade volume and contributions; `cumulative_trade_resilience_contribution`,
`cumulative_coordination_dividend` (new links only), `cumulative_investment_resilience_contribution`;
mean Partnership Opportunity (supplier -> buyer) and Friendshore (buyer rating the supplier as host).
Where a shared node has two targets under one shock, the run with the larger exposure stands for it.
`emergent_aib_country_summary.csv` keeps each economy's roles apart (trade supplier, trade buyer,
investment host, investor, each with stage participation and the coordination dividend); there is no
single ranking. The network is the primary result.

The headline network sums the supply shocks (its `headline_*` fields are the headline shock alone,
its `cumulative_*` fields every supply shock). Two more networks are built with the same functions
for the dashboard's scenarios (section 13): the **diversification scenario's** network from that
shock alone (`emergent_aib_edges_managed.csv`; its headline and cumulative fields coincide; country
exposure from the scenario's own lost supply by importer, recomputed from the node problems without
an LP), and the **observed (Baseline) network**: every reference-year shipment of a strategic-scope,
optimized node into the demand scope as a pseudo-shock `baseline` (scenario `observed`, exposure 0,
relationship `existing`), so a pair's trade contribution is its observed share of node demand summed
over the nodes and its volume the observed USD; the dividend, investment and high-severity counts are
0 by construction. Pairs below 0.5% of node demand (summed over nodes) are dropped
(`emergent_aib_edges_baseline.csv`: about 1,100 of 5,000 pairs, 96% of the USD), and the country
roles are read from the pairs kept.

## 9. Node classification (policy interpretation)

`aib_classify_nodes()`, headline shock and scenario, thresholds in `classification`, in this order:

1. **INSUFFICIENT DATA**: not optimized;
2. **LOW INTERVENTION**: shock exposure below 15%;
3. **BUILD**: residual gap >= 10% after optimized trade;
4. **MANAGE SPECIALIZATION**: resilient, but the optimized solution's largest supplier holds >= 40%,
   or its three largest suppliers >= 75%, or diversifying to a 40% limit costs >= 5 points;
5. **LOW INTERVENTION**: existing relationships already restore >= 90%;
6. **COORDINATE**: coordination dividend >= 5 points (with a small residual gap, given step 3);
7. otherwise **LOW INTERVENTION** (existing trade nearly sufficient).

Concentration is tested before adequate existing resilience because the two definitions overlap: a
node that copes through a few suppliers is a specialization to manage. The class under every other
shock, scenario, shock semantics, supply-constraint mode and growth bound is reported too
(`class_<shock>`, `class_<scenario>`, `class_release`, `class_trade_only` /
`class_production_constrained`, `class_growth_<bound>`); the compacts use the headline.

The single class is a headline label; a node can need several instruments at once. **Multi-label
policy needs** (`needs_coordinate`: dividend >= 5 points; `needs_build`: residual gap >= 10%;
`needs_manage_specialization`: the concentration test above; each only when exposure >= 15%) are kept
in `policy_needs` (e.g. `BUILD + MANAGE SPECIALIZATION`), in the node table and in the dashboard
tooltips. The labels are interpretations; the continuous columns stay beside them. Each node also
gets a continuous policy-need profile (coordination = dividend, capacity = residual gap,
specialization = excess concentration + coverage cost of diversifying) used for policy coherence.

## 10. Sectoral Compact formation

Only nodes in the **strategic scope** form compacts (`strategic_scope`: `electro_industrial` =
Batteries, Electric Vehicles, Solar, Wind, Electric Grid, Semiconductors, Nuclear; or `all`; `--scope`
overrides it). Coal, oil and gas nodes stay modelled and in every table (`in_strategic_scope`), but
are not headline compacts or network edges.

Intervention nodes (COORDINATE, BUILD, MANAGE) are grouped (`aib_group_nodes()`, complete linkage, so
every pair inside a compact passes) only when all three hold:

* **A supply-chain coherence** (`aib_chain_coherence()`): one node is upstream of the other within
  `chain_max_hops` (3) links of the curated, directed **physical / process chain**
  `supply_chain_links` (rare-earth oxide -> metal / alloy -> permanent magnet -> motor / generator;
  lithium minerals -> lithium chemicals -> cathode -> cell -> pack -> vehicle; graphite -> anode ->
  cell; silicon metal -> polysilicon -> wafer -> cell -> module; copper -> cables / transformers; GOES
  -> transformers; ...; `Tech::Sub-sector` where a name recurs), or (`allow_co_inputs`) both feed the
  same downstream node directly (blades and nacelles into turbines). Sharing a technology or stage is
  **not** enough: wind foundations and permanent magnets are both Wind Midstream but not one chain.
  After grouping, a compact must also be **one connected piece** of the chain
  (`aib_split_disconnected()`): its nodes link directly or through nodes that are in no compact (or in
  this one), so natural graphite and battery packs, linked only through anodes and cells grouped
  elsewhere, are not one compact;
* **B partner-network overlap or complementarity**: cosine similarity >= 0.35 of the nodes' supplier
  vectors, or of one node's suppliers with the other's buyers (lost supply). The supplier vectors are
  the **recurring** relationships: optimized supply shares summed over every shock, plus the leading
  investment hosts of every simulated shock;
* **C policy coherence**: cosine similarity >= 0.5 of the policy-need profiles.

So anodes and cathodes join only if their partners and needs match, and a technology can yield
several compacts or none.

**Minimum coalition, exact** (`aib_coalition_exact()`): demand anchors are the importers holding 60%
of the compact's lost supply (at most 6). The anchors' coverage in a node when the members `S`
coordinate is linear in membership, `min(1, (F + sum_{c in S} g_c) / d_A)`: `F` is what reaches the
anchors with nobody coordinating (non-candidates' shipments and every candidate's current shipments),
`g_c` what candidate `c` adds by coordinating (reallocation: its available supply, physically bounded
where known, less what it already ships to the anchors; conservative: its freed supply within its
physical room; plus one 10% increment at each candidate that is a leading host of a node that needs
capacity). The minimum coalition is therefore an exact binary program, solved with GLPK: minimize the
number of countries subject to every node reaching the target, the anchors always members; among
coalitions of that size, the most robust one (a second program maximizes the smallest margin over the
target across the compact's nodes, so the coalition does not sit exactly on the target where losing
any small supplier would tip a node below it), then the one adding the most supply. It is solved at
90%, 95% and 99%; the 95% coalition is the headline (`minimum_coalition`, `coalition_method`). No
supplier limit applies in this test. The greedy path (add the economy that most reduces the
shortfall, `aib_coalition_path()`) is kept as `greedy_sufficient_coalition` and for the "coverage as
countries join" view. Where even every candidate cannot reach 95%, the members are the widest greedy
set and `coalition_method` says so.

**Coalition tiers and the leave-one-country-out test** (`aib_coalition_substitution()`,
`aib_coalition_critical()`):

* **core coordinating members**: the exact 95% coalition;
* **critical supply partners**: members that are needed (with their supply removed the rest of the
  coalition misses the target) and that **no single other candidate** can replace (each non-member is
  tried alone); `substitutable_by` lists the single substitutes of the others, `substitutes_needed`
  how many additions the exact program needs when no single one suffices (NA: nothing can). In a
  minimum coalition every added member is needed by construction, so "needed" alone does not
  distinguish them; substitutability does. The same test applies to anchors and other members;
* **associate partners**: the fewest additions to the core coalition (held in) that reach 99%, so the
  tiers nest (`minimum_coalition_99` is the independent 99% optimum);
* **critical external dependencies**: every non-member whose loss (its current shipments to the
  anchors) pushes a node below 95% with the coalition fixed (where a node is below 95% already, whose
  loss lowers its coverage by a point or more). Every critical supplier is therefore either in the
  coalition or reported: those supplying at least `critical_min_supply_share` (1%) of the anchors'
  demand in `critical_external_dependencies`, the rest (critical only because the coalition sits near
  the target) in `critical_external_dependencies_marginal`. `aib_compact_leave_one_out.csv` keeps
  every supplier's test;
* **external dependencies**: other non-members supplying at least 5% of the anchors' demand in a node.

**Compact or not.** The anchors' coverage through their existing relationships (state B, no
coordination) is the fragmented baseline. A group is a `candidate compact` only when its coalition
lifts the anchors' coverage by at least `min_compact_gain` (5 points) over that baseline; otherwise
it is `compact unnecessary`; a coalition of fewer than 3 countries is a `bilateral arrangement
sufficient`.

**Roles** come from the results: demand anchor; specialized supplier (RCA >= 1 and >= 5% of the
anchors' optimized supply, Midstream) or resource supplier (the same, Upstream); capacity-building /
investment host (a leading host of a BUILD node); technology partner (RCA >= 1 and >= 5% of world
exports in a BUILD or MANAGE node); bridge partner (supplies one node and is a large loser of another,
supplies several stages, or sits in several compacts' coalitions); the tiers above. A country can
hold several roles; `aib_compact_members.csv` carries `coalition_tier`, `critical_supplier` and
`substitutable_by`.

**Archetype** from the lost-supply-weighted class mix: Trade / Coordination (COORDINATE dominant,
>= 60%), Investment / Capacity (BUILD), Specialization (MANAGE), otherwise Integrated. The full mix
and a secondary class are reported, and `key_policy_needs` keeps the instruments of every class
holding at least 20% of the lost supply, so a mixed chain keeps both sets of tools. A group whose
minimum coalition has fewer than 3 countries is a bilateral arrangement, not a compact.

**Coverage comparison** per compact (demand-scope demand of its nodes): shock (static) -> existing
relationships -> trade compact (every economy keeps its existing-relationship response and the
minimum coalition's members may also open or expand relationships, so it is never below the existing
state) -> trade + investment (the 10% increment at up to two member hosts per BUILD node). The optimum with every economy coordinating is reported apart
(`coverage_global_optimum`).

## 11. Relationship to Gates & Fences

Compacts work at the supply-chain level; the node table (`aib_compact_nodes.csv`) hands each node to
node-level treatment: sub-component, strategic problem, trade solution, investment need,
concentration risk and recommended policy treatment, with `yf_cell` (`<tech>-<stage>`) and
`yf_subcomponent_id` (`<cell>::<sub_sector>`) matching the Yards & Fences evidence ids.

## 12. Validation

`aib_validation_cases.csv` and `.md` write the full chain for rare-earth permanent magnets, rare-earth
oxides and compounds, anode-grade synthetic graphite and wafers (`validation_cases`), once per
supply-constraint mode: shock exposure, existing resilience, optimized relationships, coordination
dividend, residual gap, investment partners, compact membership with its critical supply partners and
critical external dependencies, the node's policy needs and its physical validation. They report what
the model produced; nothing was tuned to them. Dictionary taxonomy, China removed, reallocation,
allied demand, hold, 2026-10-19:

| Case | Mode | Exposure | Existing | Optimized | Dividend | Gap | Class (needs) | Compact |
|---|---|---|---|---|---|---|---|---|
| Rare-earth permanent magnets (no physical data) | headline: production-constrained | 65.0% | 36.3% | 36.3% | 0 | 63.7% | BUILD | with wind turbine generating sets: AUS BRA DEU MEX MMR TWN USA VNM ZAF; no critical partner or external dependency |
| | trade only | identical (no physical data) | | | | | BUILD | magnets alone: AUS DEU IND KOR MEX MMR TWN USA VNM; critical external CHE JPN PHL |
| Rare-earth oxides & compounds | headline | 39.9% | 61.8% | 61.8% | 0 | 38.2% | BUILD | alone: JPN MYS USA |
| | trade only | 39.9% | 66.2% | 66.2% | 0 | 33.8% | BUILD | same |
| Synthetic graphite (anode-grade) | headline | 63.2% | 37.8% | 37.8% | 0 | 62.2% | BUILD | alone: AUT CAN HUN IDN MDG MYS POL USA |
| | trade only | 63.2% | 38.1% | 38.1% | 0 | 61.9% | BUILD | the six-node battery compact (foils and electrolyte, separators, Li-ion cells and three more) |
| Wafers | headline | 27.8% | 74.5% | 74.5% | 0 | 25.5% | BUILD | with solar cells: IND JPN KOR THA TUR |
| | trade only | 27.8% | 91.0% | 93.4% | 2.4 pts | 6.6% | LOW, **PHYSICAL CAPABILITY NOT VALIDATED** | none |

The classes are those of the 2026-10-16 run on the OSI basket (`--taxonomy master`; its table is in the
git history) in every case and mode. The magnet node is now metal permanent magnets alone (8505.11,
without the ferrite magnets of 8505.19, now their own node, or the parts of 8505.90, outside the core), so its
exposure is higher (65% against 52%).

Reading them:

* Rare-earth processing does not look resilient in either mode. Under hold the shocked economy keeps
  buying, so the concentrates Myanmar and Laos sell to China stay there; in the production-constrained
  mode exporters absent from the IEA refining data (62% of world trade in the heading) cannot expand
  either. The gap is separation capacity.
* Wafers look resilient only on trade: HS 3818.00 is mostly semiconductor wafers (Japan, Taiwan,
  Korea), which cannot replace Chinese solar wafers. With GCIM solar-wafer capacity deciding who can
  supply, 61% of the heading's trade is held at its current shipments and a 26% gap appears; the
  trade-only LOW carries the warning.
* Magnets and anode graphite are capacity gaps under every variant; coordination adds nothing; finance,
  offtake and the listed hosts do.
* Under the diversification scenario magnets are the clearest case: an exporter HHI of 0.43 against
  0.12 for the other lines of heading 8505 gives an 18% cap; China's supply above it is 47% of the
  allied market's demand, and 46% (USD 2.1bn) is left for new capacity (hosts KOR, THA, CHE). Anode
  graphite (0.37 against 0.31 for heading 3801) has 10% to replace and keeps a 9% gap; wafers (0.16
  against their sector's 0.15) a 1% gap. Rare-earth oxides are already as diversified as the other lines of heading 2846 on
  trade (0.19 against 0.21) while China holds 84% of refining: the benchmark is a trade measure, so it
  inherits the trade data's blind spot for processing concentration (section 15).

## 12a. How to read compact coverage

A coalition's gain for its anchors (`compact_gain_over_fragmented`) can come partly from members
keeping or redirecting their own exports toward each other, which is zero-sum for the importers
outside the coalition. `coverage_with_trade_coordination` (all importers of the compact's nodes)
shows whether the compact adds supply to the system: in the BUILD compacts it equals the existing
state, so the gains there come from the investment response, not from trade coordination. The same
holds for the demand scope as a whole: under hold, the allied scope recovers from 77.1% (static) to
84.1% of its traded demand by drawing about USD 111bn of supply away from buyers outside it
(`optimized_diversion`, all of the recovery); with global demand there is no outside buyer and
coverage stays at the static 76.1%.

## 13. Outputs (`<outputs_dir>/aib/`)

| File | Key | Content |
|---|---|---|
| `allied_trade_stress_test.csv` | level x tech x stage x sub-sector x shock x semantics x supply constraint x scenario | exposure, static / existing / optimized coverage, dividend, residual gap, diversion, N-1, HHI, largest supplier, data quality |
| `allied_trade_partnerships.csv` | node x shock x scenario x exporter x importer | optimized new / expanded relationships: baseline and optimized USD, share of the importer's loss restored, RCA, world share, Partnership Opportunity, compatibility |
| `allied_trade_frontier.csv` | node / stage x shock x semantics x supply constraint x scenario x max supplier share | coverage, coverage loss vs unconstrained, HHI, largest and top-three shares, specialization score, instantaneous and re-optimized N-1 coverage, tightest costless share, minimum required diversification |
| `allied_trade_growth_sensitivity.csv` | node x scenario x growth bound | existing coverage, dividend and class at 1.25x / 2x / 4x / unlimited |
| `allied_investment_partnerships.csv` | node x shock x host x increment | marginal resilience gain, residual gap before / after, HHI and largest-share change, capability, Friendshore, rank |
| `allied_investment_pairs.csv` | node x shock x investor x host | supply from the new capacity, Friendshore |
| `emergent_aib_edges.csv` | supplier x buyer x scenario | the emergent network: trade and investment selections, contributions, stage participation, Partnership Opportunity, Friendshore |
| `emergent_aib_country_summary.csv` | economy x scenario | roles kept apart |
| `aib_node_classification.csv` | node | class, multi-label policy needs, reason, physical validation and warning, continuous results, the class under every other variant |
| `aib_compact_candidates.csv` | supply constraint x compact | the compact summary (exact and greedy coalitions, tiers, roles, coverage comparison, policy needs, physical warnings) |
| `aib_compact_nodes.csv` | compact x node x technology | the Gates & Fences handoff table |
| `aib_compact_members.csv`, `aib_compact_coalition_path.csv` | compact x economy / step | roles and coalition tiers; coverage as members are added (greedy path) |
| `aib_compact_leave_one_out.csv` | compact x node x supplier | the leave-one-country-out test |
| `aib_diversification.csv` | node x scenario | the diversification scenario: current vs benchmark HHI, cap, capped suppliers, supply to replace, coverage after existing and optimized trade, capacity gap, hosts |
| `allied_subsector_data_quality.csv` | node | flags and category |
| `aib_validation_cases.csv` / `.md` | supply constraint x case x step | validation chains, trade-only and production-constrained |
| `aib_observed_supply.csv` | node x exporter | Baseline: observed supply to the demand scope, the node's demand, share and rank |
| `emergent_aib_edges_baseline.csv`, `emergent_aib_country_summary_baseline.csv` | supplier x buyer; economy | Baseline: the observed network (pairs with at least 0.5% of node demand) and the economies' roles in it |
| `emergent_aib_edges_managed.csv`, `emergent_aib_country_summary_managed.csv` | supplier x buyer x scenario; economy x scenario | Managed Interdependence: the network of the diversification shock alone, with its importer exposure |
| `aib_compact_candidates_managed.csv`, `aib_compact_members_managed.csv`, `aib_compact_coalition_path_managed.csv`, `aib_compact_leave_one_out_managed.csv` | compact (x economy / step / node) | Managed Interdependence: the headline compacts solved under the diversification shock (headline supply constraint), with `n_nodes_run` and `coalition_reason` |

**The dashboard's three scenarios.** The dashboard compares three counterfactual market structures,
not forecasts: **Baseline**, trade as it is in the reference year (2024), before any shock;
**Managed Interdependence**, the diversification shock (section 5: no supplier is removed; each supplier
above its node's benchmark cap gives up the excess, which trade and new capacity replace); and
**Decoupling**, the headline shock under hold (China stops supplying the allied market and keeps
buying). (Decoupling here is the dashboard's name for the headline export shock; the `release`
semantics, where the shocked economy's purchases also fall, is the two-way cut of section 5.) Every
Decoupling output above is unchanged. For Managed Interdependence, `scripts/32` keeps the headline
compacts (the same ids and nodes, formed under the headline shock, so the scenarios compare) and solves
each again under the diversification shock with the same functions: its node problems, its importer
loss (recomputed from the node problems, no LP), its optimized and existing-network flows (states B and
C solved again exactly as the trade model solved them, and checked against the stored coverage; the
trade checkpoint keeps the headline shock's flows only), its investment hosts and its node classes.
Coverage metrics are demand-weighted over all the compact's nodes, a node the scenario did not run on
counting as unchanged (coverage 1, nothing lost); the anchors' coverage is over the nodes it ran on. The
scenario's supplier-share cap binds in the compact tests as in its own states B and C (2026-10-20): each
node problem carries its run's cap (`share_cap`, a share of the scope's demand), the trade-compact and
investment coverages are solved under it (`aib_compact_trade_coverage()`), and in the coalition terms
(`aib_anchor_terms()`) no economy adds more than the cap less what it already counts against it, so a
compact's trade coverage never passes the scenario's global optimum (`scripts/32` warns if one does). The
anchors' own coverage can still reach 100% where their demand is within the capped supply (the coalition
test lets members redirect supply from the scope's other buyers to the anchors, as under Decoupling). A
compact none of whose nodes the scenario ran on has no coalition and says why. The Baseline network is
built as in section 8. Under the 2026-10-19 run, the scenario runs on 38 nodes (USD 117.3bn of supply
shifts away from the capped suppliers; USD 86.5bn is left for new capacity), 26 of the 44 compact
nodes and 21 of the 33 compacts; all 21 reach the 95% and 99% targets, with smaller minimum coalitions
than under Decoupling in 14 (mean 5.3 economies against 6.4), the same size in 6 and a larger one in 1.
With the cap in the compact tests (the 2026-10-20 rerun) the coalitions are unchanged; AIB-C04's trade
and investment coverage fall from 100% to its 97.4% optimum (dividend 0), and the investment coverage of
AIB-C31 (93.0% to 84.1%) and AIB-C32 (89.9% to 89.4%) falls. Under the scenario 13 compacts are candidate
compacts, 7 compacts are unnecessary (compact gain under 5 points) and 1 is bilateral; the type, archetype
and policy needs of each are in `aib_compact_candidates_managed.csv` and the scenario file. Eleven compacts
have no node above its benchmark by more than the small tolerance and one has no supplier above its cap in
the allied market. The observed network keeps 1,126 pairs of about 5,000 (USD 1,519bn of 1,587bn).

The dashboard bundle (`dashboard/public/data/aib/`, schema `osi-aib` v2 since 2026-10-20, when `meta.scenarios` became the scenario list (an app reading v1 asks for a reload), with optional additions:
scope, semantics and constraint ids, multi-label needs, physical warnings, variant classes, growth
sensitivity, both N-1 readings, coalition method and tiers) is written by
`scripts/84_export_aib_data.R`. Its `network` section (headline variant) feeds the dashboard's country
roles: a role is named when the economy serves at least 1 point of a node's demand under the headline
shock (summed over nodes) and listed as minor below that; the CSVs and the paper count every positive
role. Its `investmentFlows` section (headline variant, strategic-scope nodes only) gives investor ->
host flows; only rank 1 is the selected host, and ranks 2-5 are alternatives for the same increment,
never summed. That document is the Decoupling scenario; since 2026-10-19 it also carries
`meta.scenarios` (the three scenarios' labels, shocks and words; the supply scenarios move to
`meta.supplyScenarios`), `node.baseline` (observed concentration and top suppliers), the run fields of
`node.diversify` and, per compact, `managed` (its nodes under Managed Interdependence) and `baseline`
(its observed HHI and top suppliers). The other two scenarios are lazily loaded files named in
`manifest.files.scenarios` (schema `osi-aib-scenario` v1): `scenario-managed.<hash>.json` (the
compacts solved under the diversification shock, its network and investment flows) and
`scenario-baseline.<hash>.json` (the observed network and each node's and compact's top ten
suppliers). The dashboard's scenario toggle (Decoupling | Managed Interdependence) reads them; every
comparison shows Baseline, Managed Interdependence and Decoupling side by side.

## 14. Results of the 2026-10-19 dictionary run

77 nodes from the 182 core codes of the electro-industrial dictionary (primary rows): 47 high
confidence, 27 usable with caution, 3 insufficient for optimization; 74 optimized; all in the
strategic scope. Headline: allied demand scope (core and partner tiers, 57 economies), global
candidate partners, China removed, hold, production-constrained, reallocation. The 2026-10-16 run on
the OSI basket (70 nodes in scope, BUILD 37, 22 compacts) is in the git history.

* The allied scope's traded demand in these nodes is USD 1,587bn and China's removal takes 22.9% of it
  (USD 364bn). Coverage goes from 77.1% (static) to 83.9% with existing relationships and 84.1% with
  optimized trade, leaving a USD 252bn capacity gap; the USD 111bn that trade recovers is all drawn
  from buyers outside the scope (`optimized_diversion`).
* Exposure and residual gap by index (share of the index's allied demand): Generation 45% / 42%
  (USD 30bn), Electrotechnologies 26% / 17% (USD 166bn, two thirds of the total gap), Electrified End
  Uses 22% / 16% (USD 43bn), Grid 19% / 6%, Materials & Inputs 8% / 6%, Industrial Transition 3% / 1%.
* Classes: BUILD 42, LOW INTERVENTION 30, COORDINATE 2 (graphite electrodes and hydro turbine parts,
  dividends of 9.4 and 7.4 points), INSUFFICIENT DATA 3, MANAGE SPECIALIZATION 0. Multi-label needs:
  lithium chemicals, cathode materials, wind turbine generating sets and rare-earth metals are BUILD +
  MANAGE SPECIALIZATION. PHYSICAL CAPABILITY NOT VALIDATED: nickel intermediates, copper ores, copper
  smelting and refining, polysilicon (LOW on trade while production is concentrated); in trade-only
  mode also cobalt intermediates and wafers.
* Sensitivities: release gives BUILD 32, LOW 37, COORDINATE 3, MANAGE SPECIALIZATION 2 (lithium
  chemicals: Chile; cathode materials: Korea); trade-only BUILD 40; conservative BUILD 50; China halved
  BUILD 20. BUILD is 42 at every growth bound, while COORDINATE is 3, 2, 1 and 0 at 1.25x, 2x, 4x and
  unlimited growth and both headline COORDINATE nodes are flagged growth-sensitive: unlike the basket
  run, coordination here depends on how far existing relationships may grow.
* Global demand with global partners (`aib_global/`): BUILD 53, LOW 21, INSUFFICIENT DATA 3,
  COORDINATE 0; hydro turbines and polysilicon join the BUILD + MANAGE SPECIALIZATION nodes. Under
  hold trade recovers nothing (coverage stays at the static 76.1% of USD 1,806bn; gap USD 431bn)
  because every buyer but China is in scope and China's purchases stay with it; release gives BUILD
  46, MANAGE SPECIALIZATION 3. The only solver problems, here and in the extended run: GLPK stops
  short of an optimal existing-network state in integrated circuits' release variants (status 1).
* Extended dictionary scope (`--dictionary-scope extended`, `aib_extended/`; adds the specificity-3
  codes): 97 nodes, 94 optimized; BUILD 48, LOW 44, COORDINATE 2 (the same two); exposure 19.5% of
  USD 2,343bn, optimized coverage 87.1%, gap USD 302bn; silicon metal joins the BUILD + MANAGE
  SPECIALIZATION nodes; 39 compacts.
* 33 candidate compacts: 31 Investment / Capacity and 2 Trade / Coordination (graphite electrodes;
  hydro turbine parts). The multi-node ones: Li-ion cells with electric buses and trucks; power
  electronics components, converters and power semiconductors; heat-pump compressors, heat pumps and
  motors; PV cover glass with modules; non-oriented electrical steel with motor parts; wafers with
  solar cells; magnets with wind turbine generating sets; lithium chemicals with cathode materials;
  furnace parts with electric furnaces. The other 24 are single nodes, from electric cars to
  piezo-electric quartz. The exact and greedy coalitions have the same size in all 33 and different
  members in 15.
* **Diversification scenario** (headline variant). 39 of the 74 optimized nodes are more concentrated
  than their benchmark (34 against the other lines of their heading, 5 against their sector's other
  nodes); 35 are already as diversified. In 38 a supplier is above the cap in the allied market
  (cobalt intermediates and chemicals is within it). Bringing them to the cap means shifting USD 117bn,
  18% of the USD 660bn allied demand in those nodes; trade under the cap restores USD 31bn (coverage
  82.2% before trade, 86.8% with existing relationships, 86.9% optimized) and USD 86bn is left for new
  capacity. China is capped in 26 nodes, Germany in 6, the United States in 5, Japan in 4. The largest
  gaps: Li-ion cells and packs (HHI 0.38 against 0.09 for the other accumulators of heading 8507; a 12%
  cap; 48% of demand, USD 52bn; hosts MYS, FIN, VNM) and PV modules (0.36 against 0.12; 32%, USD 14bn).
  Electric cars are only mildly over-concentrated (0.13 against 0.10 for the petrol, diesel and hybrid
  cars of heading 8703): the 18% cap binds on Germany, whose excess is 7% of allied demand; trade
  replaces two thirds of it and leaves 2.6% (USD 4.6bn). Rare-earth permanent magnets (0.43 against
  0.12) and PV cover glass (0.40 against 0.08) keep gaps of 46% and 45%, small in dollars (USD 2.1bn
  and 1.7bn). Electrotechnologies carries USD 58bn of the gap, Generation USD 18bn (32% of its
  diversified nodes' demand). Under the scenario 17 nodes are BUILD, 19 LOW and 2 COORDINATE (hydro
  turbines, reactor parts). Extended scope: 40 of 94 nodes above their benchmark (38 with a supplier
  above the cap), USD 125bn to shift, USD 86bn gap. Global demand: 39 nodes, USD 170bn to shift and,
  under hold, nothing restored by trade.

## 15. Limitations

* Trade-based demand omits domestic production consumed at home; coverage is of traded demand.
* HS6 headings mix grades and uses (rare-earth compounds mix concentrates and separated oxides;
  wafers are mostly semiconductor wafers; cables and converters serve many technologies). The
  data-quality flags say where.
* One reference year (2024); 2020 is used only for the time-series check.
* Shocks are applied node by node; there is no cross-node capacity coupling (a magnet shortage does
  not by itself reduce motor exports in the model).
* Partnership Opportunity and Friendshore are technology-stage indices, not sub-sector ones.
* The coalition test pools members' supply without link restrictions (state C logic) and uses the
  headline supplier-share limit.
* The physical rule decides who can supply, not how much: listed producers keep their full trade
  response even where their production is small, and unlisted exporters with real but unrecorded
  production (lumped into "rest of world" by the proxy) are held at their current shipments. Exposure
  is still measured on trade, so dependence embodied in downstream goods (polysilicon in wafers and
  cells) shows only as a warning. Magnets, motors and most equipment have no physical proxy.
* With global partners the existing network at 2x growth already carries what trade can do, so the
  coordination dividend is near zero and COORDINATE does not occur in the headline; the module's
  answer there is capacity (BUILD), and trade gains for the allied scope are diversion from other
  buyers.
* The diversification benchmark is a trade measure (world exporter HHI), so it cannot see
  concentration in production that trade does not show (rare-earth oxides: diversified on trade,
  84% Chinese in refining), and an HS4 heading is a customs grouping, not a market: its other lines
  can be mature products or other grades. The cap is a counterfactual market structure, not a
  forecast of what diversification policy would achieve.
* Compacts are grouped by complete linkage and then split into connected chain pieces; a node whose
  partners or needs differ from its chain neighbours forms its own compact (rare-earth compounds and
  metals sit apart from magnets in the headline).
