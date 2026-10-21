# Allied Industrial Base (AIB): trade partnerships, investment prioritization and Sectoral Compacts

(`config/aib.yml`; pure functions in `R/aib/`; IO in `scripts/31_build_aib_trade_network.R`,
`scripts/32_build_aib_partnerships.R`, `scripts/33_build_aib_strategic_layer.R` (the strategic layer,
section 16) and `scripts/84_export_aib_data.R`; tests `tests/testthat/test-aib-*.R`; dashboard view
`dashboard/src/aib/`.)

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
  -> (section 16) Strategic Sectoral Compacts above the compacts, which become their workstreams;
     Strategic AIB Roles and compact-specific roles of countries
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
scenarios, kept apart). For each eligible host a synthetic increment of capacity is added and the
optimizer rerun (headline shock semantics, both supply-constraint modes; hosts come from the
candidate-partner universe after the alignment screen, below). The increment is a share of the node's demand-scope demand (nominal
`increment` 5%, 10%, 20%), **capped at the host's current scale in the node**
(`aib_host_increment()`, `host_scale_multiple`, default 1):

`host_increment = min(increment, host_scale_multiple x host_scale_usd / demand)`

where `host_scale_usd` is the larger of the host's world exports of the node and its production share
times the node's world exports. With the default a host can at most double its output; a host qualified
only through a linked node has no scale in this node and gets no increment. Then

`marginal_resilience_gain = coverage_after_added_capacity - coverage_before_added_capacity`

with residual gap before/after, additional demand served, change in HHI and largest-supplier share,
the gain under the diversified cap (40%), host RCA / export share / production share, the increment
simulated (`host_increment`, `host_scale_share`, `increment_capped`).

**Why the cap.** Before 2026-10-20 every host received the same standardized increment. The resilience
gain then tied for every host in all 49 nodes of the headline simulation (rank 1 and rank 2 within 0.5
points everywhere) and the next criterion, the fall in HHI, decided. Adding a fixed amount to a small
supplier always raises concentration least, so the ranking systematically chose the smallest qualified
supplier: the rank-1 host's median world export share was 1.5% (34 of 49 nodes under 2%, median export
rank 11), and the largest non-China exporter was ranked 19th of 20 at the median and first in one node
(grain-oriented electrical steel: Austria, 1.2% of world exports, first and Japan, 25%, 20th; cathode
materials: Belgium, 0.8%, first and Korea, 51%, 20th). A 10%-of-demand plant is not a credible
expansion for an economy that exports 1% of the world's supply. Options compared on that run (gains
for the capped increment interpolated from the 5/10/20% simulations; headline variant, 49 nodes):

| Option | Rank-1 hosts changed | Rank-1 median world share | Rank-1 under 2% | Rank-1 median export rank | Largest non-China exporter: median rank / ranked 1st | Rank-1 median HHI change |
|---|---|---|---|---|---|---|
| Standardized increment (old) | - | 1.5% | 34 | 11 | 19 / 1 | -0.003 |
| Capability before HHI | 48 | 12.0% | 0 | 2 | 1 / 38 | +0.030 |
| HHI in 0.025 bins, then capability | 45 | 7.2% | 5 | 4 | 18 / 8 | +0.011 |
| Rank 1 needs >= 5% world or production share | 43 | 5.7% | 15 | 4 | 4 / 4 | +0.007 |
| **Host-scaled increment, multiple 1 (adopted)** | 46 | 9.2% | 1 | 3 | 2 / 19 | +0.017 |
| Host-scaled increment, multiple 2 | 39 | 4.9% | 5 | 5 | 5 / 2 | +0.004 |
| Tied shortlist | median 19 of 20 hosts tie on the gain: no shortlist | | | | | |

The full rerun with the cap reproduces the adopted row (45 rank-1 hosts changed; the other figures
equal); section 14 has the results.

The host-scaled increment is the only option that makes the first criterion (the resilience gain)
decide where hosts differ in what they can credibly add, keeps the diversification criterion for the
hosts that can carry the full increment (so the largest exporter is not simply first: Korea, above the
40% limit in cathodes, still drops), and treats the coalition test's synthetic capacity the same way.
Putting capability first would make the largest ally the host almost everywhere and raise concentration
most. A multiple of 2 gives more varied, mid-sized hosts; it is the main sensitivity
(`host_scale_multiple`). Each host row also reports `gain_shortlist` (how many hosts tie with the best
on the gain) and `rank_basis` (the first criterion separating it from the next host up), so a reader can
see when a tie-break, not the gain, decides.

**Eligibility** (`aib_eligible_hosts()`) requires revealed capability at material scale: RCA >= 1
on exports of at least USD 50m or 1% of the node's world trade, a production share >= 1%, a world
export share >= 0.5%, or RCA >= 1 at that scale in a node joined to it by a **material link** (in
either direction: the feedstock or the product). Open ship registries (`exclude_hosts`) are never
hosts, and neither are the economies the **alignment screen** keeps out (below). Friendshore plays no
part in eligibility. Within a capability tier hosts are compared on capability at scale: the larger of
their world export share and production share in the node (RCA is the gate, not the measure, because it
saturates for small specialists). The `max_hosts_per_node` (20) hosts simulated in a node are the
**largest by current scale** (`host_scale_usd`, which bounds each host's increment and so its gain;
`aib_host_shortlist()`), across capability tiers. Until 2026-10-20 the list was cut by tier first, so in
five nodes small RCA specialists (down to 0.2% of world exports) filled it and large exporters without
RCA >= 1 were never simulated: Germany in power electronics (6.8% of world exports, now second) and in
transformers, the United States in motor parts, electric motors and static converters.

**Ordering** (`aib_rank_hosts()`): (1) resilience contribution, (2) diversification contribution
(fall in HHI), (3) host specialization/capability (tier, then value), (4) Friendshore
(investor/buyer -> host, weighted by the importers' lost supply), (5) revealed policy/climate
compatibility. Without a supplier limit a host's increment adds `min(host_increment, gap)`, so the
resilience contribution used for ranking is the gain with the diversified limit (40%) in force, where
capacity at an already dominant supplier adds little; the unlimited gain is reported beside it. Every
host that can carry the full increment therefore ties on the gain, and among them the fall in HHI
decides; a host whose increment is capped adds less and ranks after them, by its scale. Ties are
measured from the group's leader: gains within 0.5 points of the best, HHI changes within 0.005 of the
lowest. The investor -> host pairs
(which importers the new capacity serves, with their Friendshore) are worked out for the five best
hosts at the 10% increment (capped) on top of the optimized allocation: with every other flow fixed, the
lexicographic optimum is greedy, filling the importers' unmet demand in descending Export
Opportunity of the host to them, then compatibility.

**Host types** (2026-10-20; `investment.host_types`, `aib_scaling_hosts()`, `aib_host_types()`). One
ranking cannot say both who can build volume and who adds a niche or a development prospect: with one
standardized increment the HHI tie-break silently preferred small specialists (above). Each eligible host
therefore has at most one type, each ranked on its own:

| Type | Who | Ranked by | In the compact tests |
|---|---|---|---|
| **Scaling host** | established producers: the host can carry the full headline increment, or holds at least 5% (`scaling_min_share`) of world exports or production in the node | the ordering above (gain under the 40% limit, HHI, capability, Friendshore, compatibility) | yes, the five best, each at most at its own scale |
| **Specialty host** | high-income economies (`specialty_income`) below scale with RCA >= 2 on at least USD 100m of the node's exports (the floor keeps out re-export hubs and micro-exporters: Luxembourg in battery foil, Uruguay in electrolysers) | RCA, then the fall in HHI | yes, the five best, at their own (capped) scale |
| **Development host** | lower- and upper-middle-income economies (`development_income`) below scale with revealed capability in the node or a linked node | OSI's Development Potential Index for the node's technology and stage (non-high-income partners; an economy's mean where the node has no OSI cell), then capability | never: their capacity is future capacity, a development-finance prospect |

The types exclude each other in that order: a middle-income economy that is already at scale (Viet Nam in
PV modules) is a scaling host, and a large high-income specialist (Japan in grain-oriented steel, RCA 8)
is a scaling host, not a specialty host; `income_group` is reported for every host. Scaling hosts are
ranked first (`host_rank`; `scaling_rank` among them), so the investment partnerships, the emergent
network's selected investment relationship (the best scaling host) and the node tables' leading hosts
(`priority_investment_hosts`) are the scaling hosts'; `specialty_rank`, `development_rank`,
`development_index` and `host_type` sit beside them, and the node tables add `specialty_hosts` and
`development_hosts`. The leading specialty and development candidates are always simulated, beside the
20 largest hosts. Income groups are the World Bank's (`wdi_country_info.csv`; Taiwan, missing
there, is set high income in `income_overrides`).

**Alignment screen** (2026-10-20; `investment.host_screen`, `aib_host_screen()`,
`R/aib/aib_host_screen.R`). The candidate-partner universe is global, so before the screen an economy
voting with China could rank as the best place for allied capital: Myanmar was the first host for
rare-earth oxides (Laos second), Russia the second for grain-oriented steel, Nigeria fourth for lithium
minerals, and outside the headline shock Iran was first for nuclear special materials, Russia for reactor
parts and PGMs and China itself in nine node-variants of the largest-supplier and diversification shocks.
The default mode, `bloc`, keeps as hosts the demand scope (the economies the AIB protects: refusing their
capacity while restoring their demand would be incoherent, so Indonesia, Oman and Viet Nam stay hosts) and
every other partner outside the China-leaning UNGA voting bloc (`exclude_blocs`; the blocs of
`geo_alignment_tbl.rds`, scripts/12: the US-China axis of the Bailey-Strezhnev-Voeten ideal points,
quartile cuts; an economy without a bloc, such as Taiwan, is nonaligned and kept). 48 of the 204 candidate
partners are screened (`aib_host_screen.csv`). The screen applies before the shortlist and the host types,
so a screened economy is in no ranking and no host type; it stays a trade partner and can be a coalition
member or an external dependency through its current supply.

Two alternatives were tested on the run before the screen (`data/processed/an/host_screen_recommendation.md`):

| Screen | Rank-1 hosts changed | Top-5 slots removed | Rank-1 gain lost | Compact coalitions changed | What it removes |
|---|---|---|---|---|---|
| Friendshore >= 0.50 | 1 | 4 | 0 | 0 | LAO MMR NGA RUS (but in other shocks misses CHN, IRN, KAZ) |
| Coalition tiers only (`demand_scope`) | 4 | 18 | 2.9 pts (graphite) | 2 | also TWN (8 top-5 slots), CHE, MDG, MOZ, TZA, SVN |
| **UNGA bloc, demand scope exempt (adopted)** | 1 | 5 | 0 | 0 | LAO MMR NGA RUS |

Friendshore is not an alignment measure: the bloc is about a quarter of its weight (the rest is the
buyer's imports from the partner, its energy-security need, the partner's economic opportunity, outbound
investment and Chinese-parent exposure, which repeat the ranking's own criteria), its missing values sit at
the neutral 0.5 (where many core hosts sit), and no floor separates the cases: Russia scores 0.38-0.59
across its nodes while partner-tier hosts go down to 0.40 (Brazil). The coalition tiers were built for the
network design with a USD 100bn GDP floor, so they leave out Taiwan (no WDI GDP), Switzerland and the EU and
NATO members below the floor (Slovenia, Estonia, Lithuania, Latvia, Cyprus, Malta, Iceland), none of them
China-leaning; `demand_scope` is kept as the strict sensitivity.

The screened hosts are not dropped silently: the largest `screened_hosts_simulated` (5) per node are
simulated like the others, unranked (`allied_investment_screened_hosts.csv`, with their bloc and
`screen_reason`), and every host row and node carries the **alignment cost**
(`aib_alignment_cost()`): the best screened host's gain minus the best eligible host's, both with the 40%
supplier limit (the gain the ranking reads), floored at 0, with `best_screened_host` (ties: the larger host).
The node tables report `screened_host`, `screened_host_gain` and `alignment_cost`; the dashboard shows the
line under the node's hosts.

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
physical room; plus `h_c`, a 10% increment, capped at the host's scale as in section 7, at each
candidate that is one of the five leading scaling hosts or the five leading specialty hosts of a node
that needs capacity; development hosts never add simulated capacity). The minimum coalition
is therefore an exact binary program, solved with GLPK: minimize the
number of countries subject to every node reaching the target, the anchors always members; among
coalitions of that size, the most robust one (a second program maximizes the smallest margin over the
target across the compact's nodes, so the coalition does not sit exactly on the target where losing
any small supplier would tip a node below it), then the one adding the most supply. It is solved at
90%, 95% and 99%; the 95% coalition is the headline (`minimum_coalition`, `coalition_method`). No
supplier limit applies in this test. The greedy path (add the economy that most reduces the
shortfall, `aib_coalition_path()`) is kept as `greedy_sufficient_coalition` and for the "coverage as
countries join" view. Where even every candidate cannot reach 95%, the members are the widest greedy
set and `coalition_method` says so.

**Current supply and simulated capacity are kept apart.** The coalition test counts the synthetic
capacity `h_c` of the leading hosts, so an economy can earn membership through a hypothetical plant
alone (before 2026-10-20 Austria was a core member and leave-one-out critical in the accumulator-parts
compact while supplying 0.008% of the anchors' demand: its 20-point "contribution" was all simulated;
likewise in synthetic graphite and power electronics). Criticality is therefore measured on **current
supply** only: the leave-one-out and
substitution tests remove an economy's current shipments and current supply (`drop_current`) and keep
its simulated capacity, which is reported apart (`critical_simulated`, `coverage_without_simulated`,
`added_share_simulated` in `aib_compact_leave_one_out.csv`). Each member gets a
`membership_basis` (`aib_membership_basis()`): demand anchor; current supply; or **simulated
capacity**, a member that carries simulated capacity and whose current supply is critical in no node.
The latter are **prospective hosts**.

**Coalition tiers and the leave-one-country-out test** (`aib_coalition_substitution()`,
`aib_coalition_critical()`):

* **core coordinating members**: the exact 95% coalition less its prospective hosts
  (`core_coordinating_members`; `minimum_coalition` keeps the whole solved set);
* **prospective hosts**: members in the coalition only for the capacity the model simulates at them
  (`prospective_hosts`, tier `prospective host`). They are not core members, never critical supply
  partners and not leave-one-out critical; whether the coalition's target relies on their simulated
  capacity is in `loo_critical_simulated`;
* **critical supply partners**: members that are needed (with their current supply removed the rest of
  the coalition misses the target) and that **no single other candidate** can replace (each non-member is
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
* **external dependencies**: other non-members supplying at least 5% of the anchors' demand in a node;
* **development hosts**: non-members whose only role is development host (below): listed with the
  compact as development partners for its BUILD nodes, outside the coalition and its target.

**Compact or not.** The anchors' coverage through their existing relationships (state B, no
coordination) is the fragmented baseline. A group is a `candidate compact` only when its coalition
lifts the anchors' coverage by at least `min_compact_gain` (5 points) over that baseline; otherwise
it is `compact unnecessary`; a coalition of fewer than 3 countries is a `bilateral arrangement
sufficient`.

**Roles** come from the results: demand anchor; specialized supplier (RCA >= 1 and >= 5% of the
anchors' optimized supply, Midstream) or resource supplier (the same, Upstream); scaling host and
specialty host (one of the five leading hosts of that type in a BUILD node, in the coalition; a
prospective host when that is its only basis; `investment_hosts` and `specialty_hosts`); development
host (one of the three leading development hosts of a BUILD node, member or not; `development_hosts`;
the role was "capacity-building / investment host" before the host types); technology partner (RCA >= 1 and >= 5% of world
exports in a BUILD or MANAGE node); bridge partner (supplies one node and is a large loser of another,
supplies several stages, or sits in several compacts' coalitions); the tiers above. A country can
hold several roles; `aib_compact_members.csv` carries `coalition_tier`, `membership_basis`,
`critical_supplier`, `loo_critical` (current supply), `loo_critical_simulated` and `substitutable_by`.

**Archetype** from the lost-supply-weighted class mix: Trade / Coordination (COORDINATE dominant,
>= 60%), Investment / Capacity (BUILD), Specialization (MANAGE), otherwise Integrated. The full mix
and a secondary class are reported, and `key_policy_needs` keeps the instruments of every class
holding at least 20% of the lost supply, so a mixed chain keeps both sets of tools. A group whose
minimum coalition has fewer than 3 countries is a bilateral arrangement, not a compact.

**Coverage comparison** per compact (demand-scope demand of its nodes): shock (static) -> existing
relationships -> trade compact (every economy keeps its existing-relationship response and the
minimum coalition's members may also open or expand relationships, so it is never below the existing
state) -> trade + investment (the 10% increment, capped at each host's scale, at up to two member hosts per BUILD node). The optimum with every economy coordinating is reported apart
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
| `allied_investment_partnerships.csv` | node x shock x host x increment | marginal resilience gain, residual gap before / after, HHI and largest-share change, capability, Friendshore, the increment simulated (`host_increment`, capped at the host's scale), rank, `gain_shortlist`, `rank_basis` |
| `allied_investment_pairs.csv` | node x shock x investor x host | supply from the new capacity, Friendshore |
| `allied_investment_screened_hosts.csv` | node x shock x screened host x increment | the hosts the alignment screen keeps out, simulated unranked: the same columns, `bloc`, `screen_reason`, `alignment_cost` and the best screened and eligible gains |
| `aib_host_screen.csv` | candidate partner | UNGA bloc, in the demand scope, screened or not (with the reason) |
| `emergent_aib_edges.csv` | supplier x buyer x scenario | the emergent network: trade and investment selections, contributions, stage participation, Partnership Opportunity, Friendshore |
| `emergent_aib_country_summary.csv` | economy x scenario | roles kept apart |
| `aib_node_classification.csv` | node | class, multi-label policy needs, reason, physical validation and warning, continuous results, the class under every other variant |
| `aib_compact_candidates.csv` | supply constraint x compact | the compact summary (exact and greedy coalitions, tiers, roles, coverage comparison, policy needs, physical warnings) |
| `aib_compact_nodes.csv` | compact x node x technology | the Gates & Fences handoff table |
| `aib_compact_members.csv`, `aib_compact_coalition_path.csv` | compact x economy / step | roles and coalition tiers; coverage as members are added (greedy path) |
| `aib_compact_leave_one_out.csv` | compact x node x supplier | the leave-one-country-out test (current supply; with simulated capacity apart) |
| `aib_diversification.csv` | node x scenario | the diversification scenario: current vs benchmark HHI, cap, capped suppliers, supply to replace, coverage after existing and optimized trade, capacity gap, hosts |
| `allied_subsector_data_quality.csv` | node | flags and category |
| `aib_validation_cases.csv` / `.md` | supply constraint x case x step | validation chains, trade-only and production-constrained |
| `aib_observed_supply.csv` | node x exporter | Baseline: observed supply to the demand scope, the node's demand, share and rank |
| `emergent_aib_edges_baseline.csv`, `emergent_aib_country_summary_baseline.csv` | supplier x buyer; economy | Baseline: the observed network (pairs with at least 0.5% of node demand) and the economies' roles in it |
| `emergent_aib_edges_managed.csv`, `emergent_aib_country_summary_managed.csv` | supplier x buyer x scenario; economy x scenario | Managed Interdependence: the network of the diversification shock alone, with its importer exposure |
| `aib_compact_candidates_managed.csv`, `aib_compact_members_managed.csv`, `aib_compact_coalition_path_managed.csv`, `aib_compact_leave_one_out_managed.csv` | compact (x economy / step / node) | Managed Interdependence: the headline compacts solved under the diversification shock (headline supply constraint), with `n_nodes_run` and `coalition_reason` |
| `aib_strategic_*.csv`, `aib_compact_workstreams.csv`, `aib_compact_country_roles.csv`, `aib_compact_division_of_labour.csv`, `aib_country_strategic_role*.csv`, `aib_country_role_narratives.csv`, `aib_relationship_theses.csv`, `aib_strategic.rds` | see section 16.8 | the strategic layer of `scripts/33` |

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
comparison shows Baseline, Managed Interdependence and Decoupling side by side. Since 2026-10-20 the main
document also carries the optional `strategic` section (section 16.9).

## 14. Results of the 2026-10-19 dictionary run

77 nodes from the 182 core codes of the electro-industrial dictionary (primary rows): 47 high
confidence, 27 usable with caution, 3 insufficient for optimization; 74 optimized; all in the
strategic scope. Headline: allied demand scope (core and partner tiers, 57 economies), global
candidate partners, China removed, hold, production-constrained, reallocation. The 2026-10-16 run on
the OSI basket (70 nodes in scope, BUILD 37, 22 compacts) is in the git history. The headline run was
redone on 2026-10-20 with the host-scale cap and the host types (section 7) and the prospective-host and
development-host tiers (section 10), then with the alignment screen and the scale shortlist (section 7;
the run before them is in `aib_before_hostscreen/`); the trade model, classes and coverage figures below
are unchanged by it, the hosts and compacts are the 2026-10-20 ones (the last bullet on hosts gives what
the screen changed). The global and extended sensitivity runs were not redone and keep the old host ranking.

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
* **Investment hosts** (49 nodes simulated under the headline shock). With each host's increment capped
  at its current output, the rank-1 host's median world export share is 9.2% (1.5% with the old
  standardized increment), one rank-1 host exports under 2% of the world total (34 before; the one is a
  producer, Chile in lithium minerals, by production share), the median rank-1 host is the third-largest
  exporter (11th) and the largest non-China exporter is first in 19 nodes (1). The resilience gain now
  decides rank 1 in 23 nodes, the HHI change in 20 and capability in 6; the median node has 2 hosts tied
  on the gain (19 before); the increment is capped at 12 rank-1 hosts. Before -> after (top three):
  grain-oriented electrical steel AUT, NLD, ITA -> DEU, RUS, KOR (DEU, KOR, JPN once Russia is screened;
  Japan, 25% of world exports, would pass the 40% limit); accumulator parts AUT, SRB, PHL -> JPN, KOR, POL; cathode materials BEL, GBR, IDN ->
  JPN, USA, DEU (Korea, 51%, is above the 40% limit: no diversified gain, ranked 20th); Li-ion cells FIN,
  NLD, CAN -> USA, POL, HUN; synthetic graphite IDN, CAN, MDG -> JPN, DEU, USA; power electronics
  components MYS, CZE, AUT -> JPN, PHL, MEX; converters CZE, FIN, AUT -> DEU, THA, JPN; permanent
  magnets TWN, MMR, AUS -> JPN, VNM, PHL; electrolysers TWN, TUR, NOR -> DEU, USA, JPN; polysilicon DNK,
  KOR, ITA -> MYS, JPN, DEU; lithium minerals unchanged (CHL, ARG, BRA by production share; Australia,
  53% of exports, ties on the gain and ranks 12th because adding to it raises concentration most).
  The cost is concentration of the host role: Germany and Japan are first in
  21 nodes, the rank-1 host's median HHI change is +0.017 (-0.003 before), and in the emergent network
  Germany holds 297 selected investment relationships (14 before) and Japan 259 (2), while Sweden (166),
  Czechia (56), Portugal (42), Ireland (32), Belgium (23) and Austria (20) hold none.
  `host_scale_multiple: 2` (at most tripling) gives mid-sized hosts instead (rank-1 median share 4.9%,
  largest non-China exporter first in 2 nodes).
* **Host types.** Every one of the 49 nodes has a scaling host (Japan first in 11, Germany in 10; 36 of the
  49 are high-income, 9 upper-middle-income, among them Thailand, Malaysia and Türkiye); 22 have a specialty
  host (median RCA 5.5 on a 2.5% world share: Austria, Bulgaria, Finland, Greece and Slovakia first in two
  each); all 49 have a development host (Viet Nam first in 16, India 11, Serbia and Türkiye 5; median
  Development Potential Index 0.83). Top three by type: grain-oriented steel DEU, RUS, KOR (DEU, KOR, JPN
  after the screen) / POL / TUR, IND, MYS; accumulator parts JPN, KOR / SVK, HUN, POL / SRB, VNM, TUR; Li-ion cells USA, POL, HUN / FIN /
  VNM, MYS; static converters DEU / FIN, HUN, EST / VNM, IND, MYS; transformers TUR, KOR, MEX / HRV, AUT,
  SVN / VNM, IND, COL; PV modules THA, VNM, IND / none / MYS, IDN, MEX; cathode materials JPN, KOR / none /
  TUR, ZAF, IDN. Only one specialty host joins a coalition (Spain); the solver rarely needs their small
  capacity. Development hosts hold 100 compact roles, 74 of them outside the coalition (62 with no other
  role: tier development host): India in 18 compacts, Türkiye and Viet Nam in 13, Malaysia in 12.
  The types change the coalitions of 6 compacts and add 11 memberships (176 -> 187): the five best hosts
  used to include hosts of no type (high-income generalists below scale, such as the United States and
  Italy in heat pumps) and middle-income hosts below scale (Mexico in motor parts), whose simulated capacity
  no longer counts, so the heat-pump, magnet and motor chain needs 12 members instead of 7, synthetic
  graphite 10 instead of 7 and lithium chemicals with cathodes 8 instead of 6. The investment response
  adds 1 point less anchor coverage (demand-weighted 83.5% after investment, 84.4% before the types).
* 31 compacts (33 with the old ranking; the leading hosts enter the partner vectors that group nodes):
  29 Investment / Capacity and 2 Trade / Coordination (graphite electrodes; hydro turbine parts); 30
  candidate compacts and 1 bilateral arrangement (gallium, germanium and indium: Korea and the United
  States). The multi-node ones: Li-ion cells
  with electric buses and trucks; power electronics components, converters and power semiconductors;
  heat-pump compressors, heat pumps, permanent magnets, motor parts and motors; PV cover glass, solar
  cells and modules; sensors with wafers; lithium chemicals with cathode materials; furnace parts with
  electric furnaces; rare-earth metals with oxides. The other 23 are single nodes, from electric cars to
  piezo-electric quartz. The exact and greedy coalitions have the same size in all 31 and different
  members in 8. Coalitions total 187 memberships (196 with the old ranking). Austria leaves all three
  coalitions it was in (power electronics, accumulator parts, synthetic graphite), Switzerland one of two
  and the Netherlands two of seven. Seven memberships are prospective hosts, all scaling hosts: Finland
  (cobalt), Spain and Greece (manganese dioxide), Mexico (electricity meters), Malaysia (rare earths),
  Taiwan (piezo-electric quartz) and Viet Nam (solar). The investment response adds 14.5 points of
  anchor coverage on average with the scale cap alone (17.9 with the old ranking): capped hosts add less.
* **Alignment screen and scale shortlist** (the last 2026-10-20 rerun). 48 of the 204 candidate partners
  are screened. Headline: one rank-1 host changes (rare-earth oxides: Myanmar -> United States, the same
  10-point gain) and the top five of 9 nodes. Russia leaves grain-oriented steel, Nigeria lithium minerals,
  Laos solar cells, Myanmar and Laos rare-earth oxides. The shortlist brings large exporters without
  RCA >= 1 into five top fives: Germany second in power electronics (9.0 points) and fifth in transformers,
  the United States third in motor parts and fourth in electric motors and static converters. Screened
  hosts are simulated in 26 headline nodes and the alignment cost is 0 in all of them: an eligible host
  adds as much (Myanmar and Laos 10.0 points in rare-earth oxides, Russia 7.0 in grain-oriented steel,
  Nigeria, Zimbabwe and Russia 9.2 in lithium minerals). Elsewhere it is not. Under the largest-supplier
  shock Iran would add 4.2 points more than the best eligible host in nuclear special materials (16.7
  against 12.5), China 1.6 in nuclear reactors and enrichment machinery and 1.0 in electric cars, the UAE
  0.9 in copper wire and rod (conservative); under the diversification scenario Russia 4.7 in reactor parts
  (conservative). Rank 1 changes in 22 node-variants in all. The emergent network loses the 138 selected
  investment relationships of screened hosts (1,159 -> 1,106); Germany keeps 297, Japan 270. Compacts go
  from 31 to 28 and memberships from 187 to 167, because the hosts enter the partner vectors that group
  nodes: the battery chain (Li-ion cells with buses, accumulator parts, battery foil and electrolyte,
  natural and synthetic graphite) is one compact of 10 members (DEU HUN IND JPN KOR MDG MOZ POL USA VNM)
  instead of five with 32 memberships, and electric cars join motors and motor parts (6 members) while
  heat-pump compressors with heat pumps (6) and permanent magnets (8) stand apart (one 12-member chain and
  electric cars, 6, before). No class changes, the same 44 intervention nodes sit in compacts, the
  prospective hosts are the same, and the investment response gives 83.9% anchor coverage (83.4% before).
  Russia is in one compact table only, as an external dependency of grain-oriented steel.
* **Coalition rule, not changed.** With no simulated capacity at all, 26 of the 31 compacts still reach
  95%, but several coalitions grow sharply (ferrite magnets 6 -> 26 members, accumulator parts 5 -> 11,
  natural graphite 5 -> 11); Li-ion cells, the heat-pump and motor chain, lithium chemicals and
  cathodes, synthetic graphite and piezo-electric quartz do not reach it. The rule therefore still lets
  the leading hosts' simulated capacity count, and labels the members it brings in.
* **Diversification scenario** (headline variant). 39 of the 74 optimized nodes are more concentrated
  than their benchmark (34 against the other lines of their heading, 5 against their sector's other
  nodes); 35 are already as diversified. In 38 a supplier is above the cap in the allied market
  (cobalt intermediates and chemicals is within it). Bringing them to the cap means shifting USD 117bn,
  18% of the USD 660bn allied demand in those nodes; trade under the cap restores USD 31bn (coverage
  82.2% before trade, 86.8% with existing relationships, 86.9% optimized) and USD 86bn is left for new
  capacity. China is capped in 26 nodes, Germany in 6, the United States in 5, Japan in 4. The largest
  gaps: Li-ion cells and packs (HHI 0.38 against 0.09 for the other accumulators of heading 8507; a 12%
  cap; 48% of demand, USD 52bn; scaling hosts USA, HUN, POL, specialty FIN, development VNM, MYS; the old
ranking had MYS, FIN, VNM) and PV modules (0.36 against 0.12; 32%, USD 14bn; scaling hosts THA, IND,
VNM, development MYS, IDN, MEX; were USA, DNK, PRT).
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
* The host-scale cap measures scale by exports (or production share times world trade), so a host
  whose output is consumed at home looks smaller than it is, and greenfield entry (a host qualified
  only through a linked node) gets no increment at all. It favours established producers; the
  multiple (`host_scale_multiple`) sets how far.
* The host alignment screen reads UNGA voting with hard blocs and quartile cuts, a coarse proxy for
  where allied capital can go: it screens Kazakhstan, the UAE and Egypt but keeps Saudi Arabia, India and
  Brazil (nonaligned), and it says nothing about sanctions, investment screening or political risk
  inside a bloc. Its cost is reported (`alignment_cost`), not hidden; `config/geopolitical_distance.yml`
  `tau` > 0 gives smoothed blocs for a sensitivity.
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

## 16. The strategic layer: Strategic Sectoral Compacts and Strategic AIB Roles

(`scripts/33_build_aib_strategic_layer.R`; pure functions in `R/aib/aib_strategic.R`; configs
`config/aib_strategic_compacts.yml`, `config/aib_node_functions.yml`, `config/aib_strategic_layer.yml`;
tests `tests/testthat/test-aib-strategic.R`. Added 2026-10-20.)

An interpretive layer above sections 1-14. It reads `aib_results.rds` and `aib_inputs.rds` and never
changes them: the node stress tests, classes, trade optimization, investment simulation, scenarios,
compact clustering and operational roles stay as they are and become the evidence underneath. The
hierarchy it adds:

```
ELECTRO-INDUSTRIAL SYSTEM
  -> STRATEGIC SECTORAL COMPACT   persistent institutional architecture around one coherent value chain
  -> COMPACT WORKSTREAM           the compact candidates of section 10 (AIB-C01 ...), ids unchanged
  -> NODE POLICY                  BUILD / COORDINATE / MANAGE SPECIALIZATION / LOW INTERVENTION
  -> BILATERAL / PROJECT ACTIONS  trade, investment, finance, offtake, R&D, procurement

COUNTRY
  -> STRATEGIC AIB ROLE           up to 3 primary and 2 secondary of seven roles
  -> COMPACT-SPECIFIC ROLE        what it contributes within each strategic compact
  -> OPERATIONAL NETWORK ROLES    supplier / buyer / investor / host / bridge (section 8, section 10)
```

Nothing here is a composite score. Every rule compares named evidence with a stated bar; no rule sums
evidence across roles, compacts or countries.

### 16.1 The strategic compact taxonomy

`config/aib_strategic_compacts.yml` maps every node to one strategic compact, one value-chain step
(`value_chain_order`, a label and a description) and an optional `family`. **The mapping is curated
from industrial-process knowledge, not discovered.** It starts from the dictionary's index > sector >
stage structure and the curated `supply_chain_links` / `material_links` of
`config/aib_taxonomy_dictionary.yml`; no clustering, partner similarity or optimization result decides
which compact a node belongs to, so physically linked nodes stay together even when their best partners
differ (rare-earth ore -> oxides -> metals -> magnets -> motors; polysilicon -> wafers -> cells ->
modules). Every judgement call is in the file's `mapping_notes` with the rejected alternative. Ten
compacts:

| id | name | family | spine (steps with a node in the 2026-10-20 run) | nodes |
|---|---|---|---|---|
| SC-MAGNETS | Permanent Magnets & Drive Systems | Electrotechnology supply chains | rare-earth ore -> separated oxides -> metals & alloys -> magnets & core steel -> motor & generator parts -> motors & generators | 9 |
| SC-BATTERIES | Battery Materials & Cells | Electrotechnology supply chains | minerals & recycled feed -> refined chemicals -> electrode materials & components -> cells & packs | 14 |
| SC-SEMIS | Semiconductors & Power Electronics | Electrotechnology supply chains | semiconductor materials -> fab equipment -> chips, power devices & sensors -> power-electronics components -> converters & controllers | 9 |
| SC-SOLAR | Solar Manufacturing | Power generation equipment | polysilicon -> wafers & silver paste -> cells & cover glass -> modules | 6 |
| SC-WIND | Wind & Hydropower Equipment | Power generation equipment | blade materials -> blades, towers & turbine parts -> turbines | 6 |
| SC-NUCLEAR | Nuclear Fuel & Reactor Systems | Power generation equipment | uranium ore & spent fuel -> conversion & enrichment -> fuel & nuclear materials -> reactor components -> reactors & enrichment machinery | 6 |
| SC-GRID | Grid Equipment & Conductors | Networks | copper ore -> refined copper -> wire rod & transformer steel -> insulators & components -> network equipment | 11 |
| SC-EV | Electric Vehicles | Electrified end uses | electric vehicles (one step: the vehicles integrate cells, motors and inverters from three other compacts) | 4 |
| SC-HEAT | Heat Pumps & Cooling | Electrified end uses | refrigerants -> compressors -> heating & cooling units | 4 |
| SC-INDUSTRY | Industrial Electrification & Hydrogen | Electrified end uses | catalyst metals -> electrolysers -> hydrogen & derivatives -> green iron & furnace inputs -> electric furnaces & heating | 7 |

76 of the run's 77 nodes are mapped; `biofuels-and-wood-pellets` is unmapped (no process link to any
compact). Grid deliberately excludes power electronics (HS 850440 converters are mostly non-grid; they
sit in SC-SEMIS), so it does not become a collection of all electrical equipment.

**Validation contract** (`aib_strategic_validate_map()`; `scripts/33` stops on a problem): every node of
the run is mapped to exactly one compact and step or listed as unmapped; `value_chain_order` rises
strictly within a compact; ids are unique; overrides name known compacts. A listed id that is not a
node of the run is not an error (extended scope, or a sub-sector merged into an aggregated node); an
aggregated node the map does not list takes the compact and step of its member with the most world trade;
a step with no node in the run is left off the spine and a compact with no node is left out of every
output.

**Node functions** (`config/aib_node_functions.yml`, keyed by node id, curated): each mapped node is
`resource` (ores, concentrates, recycled feed), `processing` (chemical or metallurgical transformation),
`components` (manufactured materials, components and production equipment) or `systems` (assembled
systems and end products). A node the file does not list falls back on its dictionary stage (Upstream
-> resource, Midstream -> components, Downstream -> systems) with a note. Keying by node id means a
revision of the compact map (moving a node, renumbering steps) never changes a node's function.

### 16.2 Workstreams

Each compact candidate of section 10 becomes a **workstream** of exactly one strategic compact: by
default the compact holding the largest share of its allied demand (`classification$demand_usd` summed
over its nodes; the split is recorded). A `workstream_overrides` entry takes precedence when its `nodes`
equal the workstream's node set exactly, whatever its id; an override no workstream matches is reported
as stale and the demand rule applies. `also_spans` names the other compacts a straddling workstream
touches (shown as "also touching" there). Nodes outside any workstream still sit on their compact's
spine. In the 2026-10-20 run: 33 workstreams, 3 straddling, all 3 placed by override:

* AIB-C09 (rare-earth magnets + wind turbine sets; demand 51% Wind / 49% Magnets) -> SC-MAGNETS, also
  SC-WIND: filing it under Wind would leave the magnets compact without the workstream holding its
  magnets node.
* AIB-C03 (motors, compressors, heat pumps; 58% Magnets / 42% Heat) -> SC-HEAT, also SC-MAGNETS: the
  motor -> hermetic compressor -> heat-pump chain HVAC makers build in-house.
* AIB-C01 (Li-ion cells + electric buses; 92% / 8%) -> SC-BATTERIES, also SC-EV.

The others fall wholly in one compact: Magnets C06 C19 C26 C30; Batteries C10 C11 C12 C15 C22 C23 C27
C32; Semis C02 C16 C28 C33; Solar C05 C08; Wind C13 C29; Grid C18 C20 C21; EV C04 C07; Heat C14 C31;
Industry C17 C24 C25. SC-NUCLEAR has no workstream (every nuclear node is LOW INTERVENTION or
INSUFFICIENT DATA under Decoupling); its card and spine render with zero workstreams.

### 16.3 Scenarios, portfolio, missions, policy menu and coverage

A strategic compact's id, nodes and steps are identical in every scenario; a scenario sets only each
node's **status**, the compact's portfolio, missions, coverage and critical partners. Four scenarios
(`aib_strategic_scenario_defs()`):

* **baseline**: trade as it is in 2024, no shock. Every node is `NO SHOCK`, coverage is 1 and there is no
  mission ("no shock applied"). Nothing is invented.
* **managed** (Managed Interdependence, the institutional-design default of the dashboard): the
  diversification shock. A node the scenario ran on has its scenario class; a node within its
  benchmark is `NOT STRESSED` (coverage 1, nothing lost), except `NOT TESTED` (2026-10-21 review): a
  node that is itself concentrated (exporter HHI >= 0.10) and was left alone only because its benchmark
  heading is concentrated too (benchmark HHI >= 0.18, "highly concentrated" in the 2023 US merger
  guidelines; `scenarios.managed_untested`). Being as diversified as a concentrated benchmark says
  nothing, so the node gets no coverage credit and its status detail points to its Decoupling class.
* **partial** (partial supplier disruption, `china_50`, present because the run classified it): the
  stress rows and recorded class of that shock; no investment simulation, so no post-investment coverage
  and no coalitions.
* **decoupling**: the headline shock (`china_100`, hold), the classes of section 9.

A node the trade model did not optimize is `INSUFFICIENT DATA` in every shocked scenario. Per compact and
scenario (`aib_strategic_portfolio()`):

* **Class counts**: BUILD, COORDINATE, MANAGE SPECIALIZATION, LOW INTERVENTION, INSUFFICIENT DATA, NOT
  STRESSED, NOT TESTED and NO SHOCK nodes (they sum to the compact's nodes), plus the nodes carrying each multi-label
  need. An absent class is a zero, not a dropped column (Decoupling has no MANAGE SPECIALIZATION node).
* **Missions** (`aib_strategic_missions()`): each intervention class present is a candidate, ordered by
  its share of the compact's lost supply, then node count, then BUILD, MANAGE SPECIALIZATION, COORDINATE.
  A multi-label need a node carries beside its headline class (section 9) fills a slot left free,
  ordered by the lost supply of the nodes that carry it. The first three are the `primary_mission`,
  `secondary_mission` and `supporting_mission`, each a template filled with the value-chain steps of its
  nodes (largest lost supply first, at most 3): BUILD "build missing capacity in {steps}", MANAGE
  SPECIALIZATION "preserve and second-source specialized production in {steps}", COORDINATE
  "coordinate trade to redirect existing supply in {steps}". A step whose other nodes have another
  class is named through the mission's own nodes ("solar water heaters (heating & cooling units)"), so a
  step whose main node is LOW is not announced as BUILD (2026-10-21 review). The basis is kept ("BUILD:
  97% of lost supply, 7 nodes"; "of supply to shift" under Managed Interdependence). **Materiality**
  (2026-10-21 review): a mission is material when its nodes carry >= 25% of the compact's lost supply or
  >= US$1bn (`missions.material_share`, `material_usd`). A compact whose primary mission is not material
  is led by "monitor: no intervention class reaches the materiality floor (...)" and its missions are
  listed as targeted actions (`mission_lead`, `*_mission_material`; `targeted` in the bundle). With no
  intervention node the mission is "monitor: existing trade restores supply in every node", or "... in
  every node the model could test (N ...)" when some nodes are INSUFFICIENT DATA or NOT TESTED. No score:
  an ordering of named evidence.
* **Policy menu** (`config/aib_strategic_layer.yml` `policy_menu`): the instruments of every class (and
  need) among the missions. BUILD: coordinated investment, DFI / ECA financing, guarantees, price
  stabilization, long-term offtake, demand aggregation. COORDINATE: reciprocal market access, procurement
  alignment, customs / trade facilitation, standards, emergency supply arrangements. MANAGE
  SPECIALIZATION: strategic inventories, mutual supply guarantees, contingency planning, long-term
  contracts, targeted second-source capacity, R&D / technology cooperation. Interpretive, not an
  optimized prescription.
* **Coverage**: demand-weighted over the compact's nodes that have coverage (INSUFFICIENT DATA and NOT
  TESTED nodes are left out of the weights; NOT STRESSED and NO SHOCK count as 1): `coverage_existing`
  (existing relationships), `coverage_coordinated` (the optimized trade solution) and
  `coverage_post_investment` = coordinated + the best host's simulated gain at the headline increment
  (10%), capped at 1, per node, then weighted. **By construction** this is coordinated + min(increment,
  residual gap) on each node: every host that can supply the gap gains the same, so the figure is
  arithmetic, not a finding, and it is left out of the headline sentence (2026-10-21 review). Not
  simulated under the partial disruption.
* **Hosts** (2026-10-21 review): the ranked hosts of almost every node tie with the model's rank 1 on its
  own findings (gain within `tie_resilience` and HHI change within `tie_hhi` of rank 1, config/aib.yml
  `investment`; `aib_strategic_tie_group()`), so rank 1 is the model's capability / Friendshore
  tie-break. A host is named (`top_host`, `build_hosts`) only when no other host ties with it; otherwise
  the spine keeps `host_tie_n` and `host_tied`, and `build_host_ties` counts the BUILD nodes whose hosts
  tie. In the 2026-10-20 run every BUILD node ties (42 under Decoupling, 17 under Managed
  Interdependence), so no compact names a host.
* **Countries**: `core_countries` is structural, the same in every scenario (the five allied economies
  with a compact role of largest relevance, 16.6). The institutional slots are the scenario's, from the
  coalitions of the compact's workstreams (Managed Interdependence and Decoupling only), restricted to
  the allied set (the demand scope plus `allied.treat_as_allied`, Taiwan and Switzerland; 2026-10-21
  review): `coalition_members` (allied minimum-coalition members), `critical_supply_partners` (allied
  critical partners with >= 5% of world supply of a node of the compact), `critical_external_dependencies`
  (non-allied only), `allied_outside_core` (allied economies a workstream lists as a critical external
  dependency that are in no core coalition of the compact and are not among its `core_countries`),
  `non_allied_relied_on` (non-allied minimum-
  coalition members) and `build_hosts` / `build_hosts_non_allied`.
* **Stress lens** (2026-10-21 review): when the compact's leading non-allied supplier (largest US$ supply
  equivalent over its nodes) is not a target of the headline shock, `stress_note` (Decoupling and partial
  rows) says the Decoupling shock does not bind hardest on it and gives its nodes' classes under the
  largest-supplier shock (`class_largest_subsector`): SC-NUCLEAR (Kazakhstan) and SC-INDUSTRY (Trinidad
  and Tobago) in the 2026-10-20 run.

### 16.4 Strategic AIB Roles: evidence and country metrics

The design and its calibration are in the strategic-roles method note of 2026-10-20; the rules below
are what `R/aib/aib_strategic.R` implements (`aib_country_node_evidence()`,
`aib_country_role_metrics()`). Structural evidence carries the roles (2024 trade, physical production,
outward investment); model evidence (hosts, coalitions, bridges) is the union of Managed Interdependence
and Decoupling, so a country's strategic role does not flip with the scenario toggle.

**Evidence.** From `aib_inputs.rds`: exports, world share and RCA by exporter and node; production
shares (the maximum over sources); bilateral flows 2024 and 2020 (allied-scope demand shares, net
imports, world import shares, the 2020 -> 2024 export-share change); world trade; `optimize`; material
links; the demand scope. From `aib_results.rds`: investment hosts at the headline increment under both
institutional scenarios; node demand; workstream coalitions, roles, bases and tiers in both scenarios;
partnerships; the Managed Interdependence edges. Capital (read-only partner data): the Cleantech
Manufacturing FDI projects (`cm_fdi_projects_tbl.rds` of `scripts/15`, `index_use` and cross-border,
capital x attribution weight; NZIPL / Johns Hopkins, CC BY 4.0) and the IMF Direct Investment Positions
(outward net position, all instruments, reported official data, 2023 or 2022, summed over counterparts
that are not conduit centres; conduit reporters, `partnership_strength_conduit_centres()`, are not
assessed). A missing capital input leaves the capital role "not assessed" (NA, never 0).

**Node flags** (country i, node n): `position_share` = max(world export share, production share);
**anchor position** = position >= 10% and (RCA >= 1 or production share >= 10%); **specialist
position** = a `components` node with world export share >= 10% and RCA >= 1.5; **momentum** = export
share up >= 1 point 2020 -> 2024, >= 2% in 2024 and >= 1.25x its 2020 share; **presence** = position >=
2%; **capable host** = ranked as a host in either institutional scenario with capability scale >= 2%
(its own world export or production share in n, or its position in a node joined to n by a material
link); **credited host** (2026-10-21 review) = a capable host in rank 1's tie group (16.3) on a node that
needs new capacity in that scenario (BUILD, or a BUILD need beside its class); **host gap** = its credit,
the best gain x node demand split equally among the credited hosts, the larger of the two scenarios
(before: the whole residual gap for any host ranked 1-5, LOW nodes included). `best_host_rank` is the
model's rank where it is credited (evidence; inside a tie it is the capability tie-break). Re-export hubs
(config/aib.yml `trade.reexport_hubs`: Hong Kong, Macao) hold no anchor, specialist or scale position.

**Assessed economies.** The 57 economies of the demand scope plus the non-allied major suppliers (>= 10%
of world supply of one mapped node, or >= 5% of three): 73 in the run. Non-allied economies are
assessed on the three supply roles only; they appear as suppliers and dependencies, never as members.

### 16.5 The seven roles: two gates each

| code | role | gate 1 (size) | gate 2 (breadth / depth) | assessed among |
|---|---|---|---|---|
| RPA | Resource & Processing Anchor | anchor positions in resource and processing nodes | mean position share over those nodes | allied + major suppliers |
| STM | Specialized Technology & Manufacturing Anchor | exports in specialist positions / world trade of the manufacturing nodes | compacts with a specialist position | allied + major suppliers |
| SMI | Scale Manufacturing & Integration Platform | manufacturing exports / world trade of the manufacturing nodes | compacts with >= 5% of world exports in >= 2 manufacturing nodes (nodes, not steps, since 2026-10-21: a one-step chain such as EVs could never count) | allied + major suppliers |
| DMA | Demand & Market Anchor | mean share of allied demand | mean net-import share of end-use goods (the nodes of each compact's last step; manufactured goods before 2026-10-21, which counted input buying) | allied |
| CIA | Capital & Investment Anchor | outward CM-FDI capital | outward IMF DIP position (conduit-free) | allied, not conduits |
| DSP | Development & Scale-Up Platform | its credited share of new capacity on nodes that need it (US$) | scale-up rate (momentum nodes / presence nodes) | allied |
| CBH | Coordination / Bridge Hub | workstreams where it is a vertical bridge partner | strategic compacts reached through minimum coalitions | allied |

**Rules** (`aib_assign_strategic_roles()`; parameters in `config/aib_strategic_layer.yml` `roles`):

1. Each gate is a percentile (`dplyr::percent_rank`, 1 = top) among the role's eligible economies. A role
   **qualifies as primary** when both gate values are > 0 and both percentiles are >= 0.90, as
   **secondary** when both are >= 0.75. `role_strength` is the lower (binding) percentile; it orders a
   country's own roles and nothing else. The gates are never averaged.
2. Order: primary-qualifying first, then `role_strength` in whole percentile points, then the other
   gate, then the fixed order RPA, STM, SMI, DMA, CIA, DSP, CBH.
3. Cap: the first three primary-qualifying roles are primary, a fourth is demoted to secondary; at most
   two secondary; any further qualifying role is reported as "qualifies beyond the cap" (evidence, not
   in the narrative).

Every gate value, percentile and supporting metric is kept (`aib_country_strategic_roles.csv`,
`aib_country_strategic_role_metrics.csv`). Why these gates: DMA needs net demand for manufactured goods,
so a large exporter that also imports is not a demand anchor; DSP needs a scale-up rate, which keeps
incumbents present almost everywhere out and is why no economy qualifies for all seven roles; a
specialist position must be a component (EVs, modules and turbines are scale, not specialization).

### 16.6 Compact-specific country roles

One row per country x strategic compact where the country performs at least one function
(`aib_compact_country_roles()`), from node-level evidence inside the compact:

| function | rule |
|---|---|
| Resource supplier | an anchor position in a `resource` node |
| Processing anchor | an anchor position in a `processing` node |
| Specialized technology supplier | a specialist position |
| Scale manufacturer | >= 5% of world exports of a manufacturing node that is not a specialist position (node level since 2026-10-21: a step bundling several products hid transformer and meter makers; the step shares stay as evidence, `scale_steps10`, `scale_steps5`, `f_scale_step`) |
| Demand anchor (allied) | >= 10% of the allied scope's imports of a step's optimized nodes, or "Demand anchor" in a workstream of the compact |
| Investment anchor (allied) | >= 10% of the outward CM-FDI capital in the compact's CM-FDI sectors (Battery -> SC-BATTERIES, EV -> SC-EV, Solar, Wind; NA elsewhere) |
| Capacity-building host (allied) | a credited host in a node of the compact (16.4), or a capable host with momentum there |
| Bridge / coordination partner (allied) | "Bridge partner" in a workstream of the compact, or a member of >= 2 of its workstreams' minimum coalitions |
| Diversification partner (allied) | any coalition tier in a workstream of the compact |

The **lead role** (`compact_role`) is the function behind the country's largest position: each supply
function's largest node share of world supply against, for demand, its largest share of world imports
of an optimized node in a qualifying step (both world-denominated). Positions within 3 points are tied;
a supply function beats demand in a tie, and among supply functions the larger US$ position leads
(2026-10-21 review: US$ no longer compares a demand node with a supply node). A demand position leads
only with >= 5% of world imports of its node. Input demand does not lead (2026-10-21): a demand position
in a step before the country's last supply step (its furthest resource, processing, specialist or scale
node) is input buying for its own downstream production; it stays among the functions, but only demand
at or after that step (any step, for a country that supplies nothing in the compact) competes for the
lead. Investment, host, bridge and diversification lead only without a supply or demand lead; demand
alone leads as a last resort. `compact_role_detail` adds up to two other substantive functions. The row
keeps the evidence: lead node, step and share; key nodes (prose labels); supplier, demand and investment
contributions; the resilience contribution in each scenario (the US$ its new and expanded relationships
restore in the compact's nodes, `resilience_usd_*`, and that over the compact's allied demand; before
2026-10-21 a sum of node-level shares); criticality (critical supply partner with >= 5% of world supply of
a node of the compact; critical external dependency for a non-allied economy only; "allied supplier
outside the core coalition" for an allied one in no core coalition of the compact and not among its core
countries); and the relevance
used to order countries, value-weighted: the larger of its supplier contribution (share of the compact's
world trade) and demand contribution (share of its allied demand); the unweighted step means
(`mean_step_supply`, `mean_step_demand`) stay as evidence.
The compact's **core countries** (the same in every scenario; `aib_strategic_core_long()`) are its major
players plus its upstream anchors, at most six: the three allied economies with the highest relevance;
then, for each upstream function (resource supplier, processing anchor), the allied economy with the
strongest lead position in it if that position is at least 10% of world supply and it is not already in;
then the next most relevant, up to six; listed in chain order (resource, processing, specialized
technology, scale manufacturing, demand). A value ranking alone filled the magnets core with motor makers
and left out the rare-earth stages, and one leader per function alone let niche leaders (monazite ore,
quartz) push out the largest manufacturers; both were tried on 2026-10-21. Non-allied lead suppliers are
listed apart. The country x step and country x node tables behind
the division-of-labour matrix are `aib_compact_division_of_labour.csv` and `steps` in
`aib_strategic.rds`.

### 16.7 Narratives and bilateral theses

**Narratives** (`aib_country_role_narratives()`): one per allied economy and per non-allied supplier
with a role, assembled from slots so the dashboard can show them separately; no text is written for a
particular country. `headline` (no role / no primary role / the one primary role / "{Role 1} with ...
roles" when it leads the second by >= 3 points / otherwise "Combines ... roles"), `secondary`, `first_on`
(gate metrics where it ranks first among allied economies), `contributes` (its three anchor or
specialist positions of largest US$ value, share x the node's world trade), `focus` (the node functions
where it is most present), `needs` (its three largest net imports in US$ among those >= 5% of allied
demand), `dependency` (top-quartile net buyer of upstream materials / manufactured goods), `compacts` (its
three most relevant compacts with compact role and lead step), `partners` (the allied economies with which
new and expanded trade restores the most US$ under Managed Interdependence and Decoupling; investment
edges are left out since their hosts tie) and `capacity` (up to three nodes where it is a credited host,
with its credited US$). Value, not share, orders the lists (2026-10-21 review: a 45% share of a US$0.1bn
node no longer headlines a country); the share is the number shown. Beside each list slot the
table carries its items as codes (`items_first_on` gate metrics; `items_contributes`, `items_needs` and
`items_capacity` node ids with `items_contributes_share`, `items_needs_share` and
`items_capacity_usd_bn`; `items_focus` node functions; `items_compacts` compact ids; `items_partners`
ISO3; 2026-10-21), so a reader joins them to the bundle instead of splitting the prose. An aggregated
node reads as its members in prose ("Spent fuel & nuclear scrap and uranium ores (aggregated)").

**Bilateral theses** (`aib_relationship_theses()`; every allied pair with a modelled trade or investment
relationship in either institutional network, 1,133 pairs with Taiwan and Switzerland): the priority
compacts (shared compacts, complementary ones first, where one side is the demand anchor and the other
supplies, then those where the lead roles differ, then by the smaller of the two relevances; a compact
where both are demand anchors is left out; at most 3; 2026-10-21 review), the complementarity (each side's
lead role in them) and the
AIB logic, every label whose evidence holds: trade and investment (a modelled relationship of that kind),
technology (in a priority compact one side is the specialized technology supplier and the other is not),
development / capacity building (one side is a capacity-building host where the other is an investment
anchor or holds the capital role), market access (a demand anchor facing a supplier) and resilience /
second sourcing (a demand anchor facing a diversification or bridge partner). The four compact-role
labels are read on the priority compacts only (2026-10-21): read on every shared compact, large
economies sharing up to nine compacts got nearly every label and the theses did not tell pairs apart
(Japan - Korea had five). The thesis reads "an" before "investment". No bilateral score.

### 16.8 Outputs (`<outputs_dir>/aib/`)

| File | Key | Content |
|---|---|---|
| `aib_strategic_compacts.csv` | strategic compact x scenario | portfolio, class counts, missions with class and basis, policy mix and menu, demand, lost supply, coverage (static, existing, coordinated, post-investment), headline, core countries and roles, critical supply partners and external dependencies, coalition members, build hosts |
| `aib_compact_workstreams.csv` | workstream (AIB-Cxx) | its strategic compact, assignment basis, demand split, also-spans, straddles, override reason, activity per scenario, the Decoupling name, type, archetype and coalition, the Managed nodes run and coalition |
| `aib_strategic_spine.csv` | compact x step x node x scenario | node status, status detail, policy needs, demand, lost supply, coverage states, rank-1 host and gain, largest supplier, workstream |
| `aib_compact_country_roles.csv` | compact x country | compact role, detail, functions (and their flags), lead node / step / share, key nodes, contributions, resilience contribution per scenario, criticality, relevance |
| `aib_compact_division_of_labour.csv` | compact x country x node | position, world and production shares, RCA, allied demand, world import and net-import shares, host rank, capable host, host gap, momentum, anchor and specialist flags (cells with >= 1% supply or demand, or a host rank) |
| `aib_country_strategic_roles.csv` | country x role | eligibility, both gates (metric, value, percentile), role strength, primary / secondary / qualifies beyond the cap / not assigned, role rank, supporting metrics |
| `aib_country_strategic_role_metrics.csv` | country | every role metric |
| `aib_country_role_narratives.csv` | country | role lists, the ten slots, their items as codes (`items_*`), the narrative |
| `aib_relationship_theses.csv` | allied pair | priority compacts, shared compacts, complementarity, logic, thesis |
| `aib_strategic.rds` | | all of the above plus the map, node functions, evidence, notes and meta (read by `scripts/84`) |

### 16.9 The dashboard bundle's `strategic` section

`scripts/84` adds an optional `strategic` section to the main document (`aib_dash_strategic()`; checked
by `aib_check_strategic()`, whose header lists every rule) when `aib_strategic.rds` exists. Additive:
the rest of the document is unchanged and a bundle without it stays valid. Version 1:

* `version`, `defaultScenario` (`managed`), `framing` ("Strategic compact = persistent architecture;
  scenario = the stress applied to it."), `scenarios` [{id, label, shockId, words, investment,
  coalitions}], `roleDefs` [{code, name, noun, gates, gateLabels, supplyRole}], `thresholds` {pPrimary,
  pSecondary, maxPrimary, maxSecondary}.
* `compacts` [{id, name, family, purpose, steps [{order, label, description, nodes, functions}],
  workstreams, alsoTouching, core / nonAllied [{iso3, role, step}], scenarios {baseline, managed,
  partial, decoupling: {counts {build, coordinate, manage, low, insufficient, notStressed, notTested,
  noShock}, needs {build, coordinate, manage}, missions [{slot, cls, text, basis, targeted}], statement,
  policyMix, menu [{cls, instruments, nodes}], demandUsd, lostUsd, exposure, coverage {static, existing,
  coordinated, postInvestment}, activeWorkstreams, criticalPartners / criticalExternal / coalition /
  alliedOutsideCore / nonAlliedReliedOn (null where the scenario has no coalitions), buildHosts,
  buildHostsNonAllied, buildHostTies, classMix, headline, stressNote, nodes [[node, status, coordinated
  coverage, named host, hosts tied with rank 1]] in spine order}}, division [[ISO3, node, share of world
  supply, share of allied demand, best host rank where credited, flags]]}], flags: `a` anchor, `s`
  specialist, `h` credited host, `m` momentum. The 2026-10-21 fields are optional in both checkers
  (`notTested`, `targeted`, the new slots, the fifth spine element), so an older bundle stays valid.
* `workstreams` [{id, compact, basis (demand / override / none), share, split [[compact, share]],
  alsoSpans, straddles, overrideReason, active (scenario ids)}]: every compact of the document once.
* `countries` [{iso3, allied, majorSupplier, primary, secondary, beyondCap, roles [[code, status, rank,
  strength, gate 1 value, gate 1 percentile, gate 2 value, gate 2 percentile]], metrics {...}, slots {the
  ten slots} or null, items {firstOn, contributes [[node, share]], focus, needs [[node, share]], compacts,
  partners, capacity [[node, US$bn]], capacityUsdBn} (optional; absent before 2026-10-21) or null,
  narrative}].
* `compactRoles` [{compact, iso3, allied, role, detail, functions, leadNode, leadStep, leadShare,
  leadBasis, keyNodes, supplier, demand, investment, resilienceManaged, resilienceDecoupling,
  criticality, relevance}].
* `theses` [[ISO3, ISO3, priority compact ids, logic labels]]: the complementarity text is the two
  countries' compact roles in those compacts.

Every ISO3 code it uses is in the document's `countries`. On the 2026-10-21 export the section is 545 kB,
68 kB gzip; the main document goes from 200 to 268 kB gzip (`npm run budget`: Compacts landing 500 of
700 kB, Strategic compact view 612 of 900 kB).

### 16.10 Results (scripts/32 run of 2026-10-20, strategic layer of 2026-10-21)

Managed Interdependence (the default lens) and Decoupling (the stress test), with core countries (the
same in both):

| compact | Managed Interdependence: classes, primary mission | Decoupling: classes, primary mission | core countries |
|---|---|---|---|
| SC-MAGNETS | 1 BUILD, 3 LOW, 1 INSUFF, 2 not stressed, 2 not tested; build missing capacity in rare-earth permanent magnets (magnets & core steel) | 7 BUILD, 1 LOW, 1 INSUFF; build missing capacity in electric motors (motors & generators), motor & generator parts and magnets & core steel | THA VNM DEU USA JPN MEX |
| SC-BATTERIES | 4 BUILD, 3 LOW, 1 INSUFF, 1 not stressed, 5 not tested; build missing capacity in Li-ion cells & packs, cathode materials & precursors (electrode materials & components) and lithium chemicals and manganese dioxide (EMD) (refined chemicals) | 10 BUILD, 3 LOW, 1 INSUFF; build missing capacity in cells & packs; electrode materials & components and lithium chemicals, cobalt intermediates & chemicals and manganese dioxide (EMD) (refined chemicals) | USA COD JPN IDN KOR DEU |
| SC-SEMIS | 1 BUILD, 3 LOW, 3 not stressed, 2 not tested; monitor (below the materiality floor); targeted: build missing capacity in piezo-electric quartz (semiconductor materials) | 6 BUILD, 3 LOW; build missing capacity in static converters & inverters (converters & controllers); power-electronics components and power semiconductors and sensors & controller parts (chips, power devices & sensors) | BRA TWN SGP KOR USA VNM |
| SC-SOLAR | 4 BUILD, 2 LOW; build missing capacity in modules, cells & cover glass and silver powder (wafers & silver paste) | 4 BUILD, 2 LOW; build missing capacity in modules, cells & cover glass and wafers (wafers & silver paste) | DEU JPN KOR VNM USA IND |
| SC-WIND | 2 BUILD, 1 COORD, 2 not stressed, 1 not tested; build missing capacity in wind turbine generating sets (turbines) and blade materials | 2 BUILD, 1 COORD, 3 LOW; build missing capacity in wind blades & hubs (blades, towers & turbine parts) and wind turbine generating sets (turbines) | ESP POL IND DEU USA GBR |
| SC-NUCLEAR | 2 BUILD, 1 COORD, 1 INSUFF, 1 not stressed, 1 not tested; build missing capacity in reactors & enrichment machinery and nuclear special materials (fuel & nuclear materials) | 5 LOW, 1 INSUFF; monitor | AUS CAN IND USA FRA GBR |
| SC-GRID | 1 BUILD, 4 LOW, 6 not stressed; monitor (below the materiality floor); targeted: build missing capacity in grain-oriented electrical steel (GOES) (wire rod & transformer steel) | 3 BUILD, 8 LOW; build missing capacity in grain-oriented electrical steel (GOES) (wire rod & transformer steel), electricity meters (network equipment) and insulators & components | CHL PER COD JPN DEU USA |
| SC-EV | 1 LOW, 2 not stressed, 1 not tested; monitor | 3 BUILD, 1 LOW; build missing capacity in electric & plug-in cars; electric buses, trucks & tractors and electric two-wheelers & works trucks (electric vehicles) | DEU USA FRA JPN KOR GBR |
| SC-HEAT | 1 BUILD, 2 LOW, 1 not stressed; monitor (below the materiality floor); targeted: build missing capacity in solar thermal water heaters (heating & cooling units) | 4 BUILD; build missing capacity in heating & cooling units, compressors and refrigerants | USA NLD THA MEX DEU ITA |
| SC-INDUSTRY | 1 BUILD, 1 LOW, 3 not stressed, 2 not tested; build missing capacity in catalyst metals | 3 BUILD, 1 COORD, 3 LOW; build missing capacity in electric furnaces & heating, electrolysers and furnace parts (green iron & furnace inputs) | IND OMN ZAF USA SAU MAR |

Strategic roles (upper case primary, lower case secondary; beyond the cap in brackets): Japan {CIA, STM,
SMI, cbh}; Korea {STM, SMI, cbh, cia}; United States {DMA, CBH, CIA, smi, stm} (RPA); Australia {RPA};
India {dsp, stm} (CBH, RPA); Germany {CBH, CIA, SMI, dma, stm} (RPA); China (supply roles only) {RPA, SMI,
STM}; Viet Nam {cbh, dsp}; Mexico {dma, smi} (CBH). Japan, Korea and the US differ: Japan and Korea share
the two manufacturing roles and differ on capital (primary for Japan, secondary for Korea); the US is the
only one led by demand. No economy qualifies for all seven roles; 23 of the 59 allied economies (the demand
scope plus Taiwan and Switzerland) hold a strategic role at these bars, the rest are described by their
compact roles. No economy holds DSP as a primary role, and India, Mexico and Viet Nam hold no primary role.
The capital roles need the partner inputs (`--partner-dir`, `--dip`): without them scripts/33 warns and
does not assess the Capital & Investment Anchor.

Compact roles of the validation economies (lead role, lead step and share): Permanent Magnets: Japan
scale manufacturer (magnets & core steel 9%), Korea specialized technology supplier (NGO steel 14%), the
US scale manufacturer (motors & generators 27%), Australia and Viet Nam processing anchors (rare-earth
metals 25% and 38%), Germany demand anchor (motors 14%). Batteries: Korea specialized technology supplier
(cathodes 51%), Japan processing anchor (refined chemicals 18%), Australia resource supplier (lithium
minerals 53%), the US resource supplier (battery recycling feed 44%), Indonesia and Chile processing
anchors. Solar: the US and India demand anchors, Germany processing anchor (polysilicon 34%), Japan
specialized technology supplier (wafers & silver paste 59%), Viet Nam scale manufacturer (modules 13%).
Grid: the US demand anchor (network equipment 34%), Chile and Peru resource suppliers, DR Congo
processing anchor (refined copper 20%), Germany scale manufacturer (network equipment 11%), Japan
specialized technology supplier (GOES 25%). The 2026-10-21 rules moved lead roles from input demand to
what the country supplies (Japan in Magnets, from demand anchor for rare-earth metals; Viet Nam in Solar,
modules; Korea in Solar; the US in Semiconductors, fab equipment, and in Nuclear, reactor components;
Japan in Industry, electrolysers) and, in the one-step Electric Vehicles compact, made Korea (11%), Japan
(7%) and Mexico (6%) scale manufacturers. Theses: 31 of 1,042 pairs carry all six logic labels (45
before the priority-compact rule) and Japan - Korea two (five before).

### 16.11 Results that still look implausible

Reported, not forced (the rules decide; no country outcome is hard-coded; validation of 2026-10-21):

* **Australia has no Development & Scale-Up role**: its hostable unmet demand is p95 (US$5.0bn: rare-earth
  magnets, lithium chemicals) but its scale-up rate p73 is below the p75 bar. The potential shows in its
  narrative's capacity slot.
* **Korea's capital role is beyond the cap** although Korea is the largest allied cleantech-manufacturing
  investor (US$61.8bn, p100): its IMF outward position is p82, so the role is secondary-qualifying and
  the cap (two secondary roles) hides it; the narrative names it ("first among allied economies in ...
  outward cleantech-manufacturing FDI").
* **The US in Batteries leads as a resource supplier** on battery recycling feed (44% of world exports of
  the scrap heading): scrap exports follow consumption, not resource endowment. The US in Magnets leads
  as a scale manufacturer on AC generators (27%) rather than as the market it also is (20% of allied
  demand).
* **Australia in Magnets is a processing anchor on rare-earth metals** (25%) rather than a resource
  supplier: the ore node has insufficient data (Nigeria 75% of the heading). These shares, and Viet Nam's
  38% of rare-earth metals, need an HS6 check.
* **Japan in Magnets is a scale manufacturer (9% of magnets & core steel)**, not a specialized technology
  supplier: its magnet and NGO exports are under the 10% specialist bar against China's share.
* **Magnets' Decoupling mission names motors, motor parts and magnets** (the largest lost supply); the
  BUILD nodes in separated oxides and metals & alloys (US$0.4bn and 0.1bn lost) are on the spine but not
  in the three-step mission text.
* **Managed Interdependence lists China as the rank-1 host** for silver powder and nuclear reactors &
  enrichment machinery (no supplier is removed in that scenario, so China stays a candidate host).
* **Relationship theses still do not tell the largest pairs apart**: Japan - US, Korea - US, India - US,
  India - Japan and Australia - Korea carry all six labels; trade holds for nearly every pair and the US
  is a demand anchor in most compacts. A lead logic (the label supported in the most priority compacts)
  would need a new tuple field.
* **Viet Nam's lead on hostable unmet demand** comes from integrated circuits and depends on the
  investment-host ranking, which the aib-eid revision changes; DSP, the capacity slot and the host
  functions should be rerun after it.
* Integrated circuits (about 70% of SC-SEMIS demand) and the conductor metals (about 65% of SC-GRID)
  dominate those compacts' demand-weighted coverage; read their BUILD nodes beside the figure.
* Percentile gates are zero-inflated (70-77% of economies have no specialist position, workstream bridge
  or integrated compact), so any holder clears the p75 secondary bar on that gate and the role rests on
  its other gate. Tested 2026-10-21: requiring the median of the holders changes no validation economy;
  ranking among holders only strips Japan and Korea of their manufacturing primaries. Kept.
