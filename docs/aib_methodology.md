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

What it does **not** do, by design: no composite AIB score; no alliance fixed in advance (the
candidate universe is a setting, global by default); no assumption that maximum diversification is
optimal; no reading of observed trade as spare physical capacity; no reading of the synthetic
investment increment as a project; Friendshore never qualifies a host that lacks industrial
capability; no assumption that every technology needs a compact or that every node of a technology
belongs in one. Every one of these outcomes is reachable: no intervention, existing trade
sufficient, new bilateral coordination helps, new capacity required, concentrated specialization
worth preserving, no compact needed, several compacts in one technology.

## 1. Audit: what the module reuses

| Need | Reused from |
|---|---|
| Technology > stage > sub-sector > HS6 | `data/reference/energy_hs6_master.csv` (in-basket rows; `R/utils/hs6_crosswalk.R`) |
| Bilateral flows (importer's declaration first, else exporter's x 1.06) | `partnership_strength_build_trade_cells()` run at node grain through a pseudo-crosswalk (`aib_node_crosswalk()`) |
| RCA (Balassa) | `trade_core_build_comtrade_rca()` on node exports |
| Export Opportunity, exporter -> market | `partner_opportunity_tbl.rds`, variable `Opportunity Index` |
| Friendshore, investor/buyer -> host | `partner_friendshore_tbl.rds`, variable `Friendshore Index` |
| Production / capacity shares | `major_supplier_shares_tbl.rds` (IEA refining and mining), GCIM operational capacity |
| Policy / climate | IEA PAMS (`policy_outputs.rds$policy_agg`), CAT ratings (`cat_prepare_ratings()`, `partnership_strength_clean_policy()`) |
| Coalition (sensitivity universe only) | `coalition_tbl.rds` |
| LP solver | GLPK via Rglpk (installed with the allied network's MILP stack) |

The allied network design is normative and closed (index scores, no USD, no shocks, no sub-sector
grain); the AIB module is built on the observed USD flows instead and does not reuse its objective
or its GDP-derived portfolio caps.

## 2. Nodes

A node is a `(tech, supply_chain, sub_sector)` cell of the master (in-basket rows), with
`nodes$splits` refining a sub-sector where its HS6 codes allow it. The October 2026 configuration
splits Solar `Polysilicon & Wafers` into `Polysilicon` (2804.61) and `Wafers` (3818.00).

Cells whose HS6 set is identical in several technologies are one **physical node**, counted once,
which keeps the list of technologies it serves (2026-10 data: permanent magnets in EV and Wind,
rare-earth compounds and metals in EV and Wind, transformers in Grid and Solar, pipelines in Oil and
Gas). The hierarchy outputs carry one row per technology; aggregates across technologies are
"technology-attributed" where nodes are shared, and network aggregates count each physical node once.

The HS6 codes are the empirical trade data, the sub-sector is the optimization unit, the stage is
the aggregation and policy frame and the technology a system summary. **Every metric is recomputed
from flows summed to the node; no HS6-level score is averaged.**

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
and Macao re-exports follow China (`trade.reexport_hubs`). With `shocked_demand: release` (default)
the shocked economy is removed from the network in both directions, so exports that were headed to
it are freed; `hold` keeps its purchases.

The **demand scope** is every importer in the candidate universe other than the shocked economy;
coverage is the share of that baseline demand served after the shock; shock exposure is the share
lost. Open ship registries (Liberia, the Marshall Islands, Panama, the Bahamas;
`trade.exclude_from_universe`) are outside the universe: vessels (HS 89, tankers and platforms in the
Oil and Gas baskets) trade through the flag they fly, so those economies appear as large importers and
exporters of ships. Their shipments stay as observed.

**Candidate partners** are universe exporters with at least USD 1m **and** 0.05% of the node's world
exports (the larger bar binds); smaller exporters keep their current shipments but are not
re-optimized.

**Supply scenarios**, kept apart in every output:

* **conservative**: every surviving shipment into the universe stays where it is; only the supply the
  shock frees can move;
* **reallocation**: an exporter can redirect any of its observed global exports (to third markets
  and to the shocked economy's remaining demand) toward the universe.

**States**:

* **A static**: the shocked supply disappears; nothing adapts.
* **B existing-network adaptation**: candidates reallocate only through relationships observed in
  2024 (plus keeping their own exports at home, `allow_domestic_retention`), and an existing
  relationship may grow to at most `1 + existing_relationship_growth` times its observed size
  (default: it may double). Without that bound any USD 10k relationship could carry unlimited volume,
  "existing relationships" were as flexible as new ones, and on the first full run (2026-10-15) the
  coordination dividend was ~0 in 89 of 91 nodes for that reason alone.
* **C optimized partnerships**: any candidate may serve any importer in the universe.

`coordination_dividend = optimized_coverage - existing_coverage` (C - B) is the value of new or
expanded relationships, i.e. of coordination.

**Lexicographic optimization** (`aib_solve_state()`, `aib_lp_lexicographic()`), each objective
optimized only among solutions within the tolerance of the earlier optimum:

1. maximize demand served (`tolerance_coverage` 0.05%: a wider band let state B's tie-breaks buy
   Export Opportunity with coverage, which faked a coordination dividend);
   1b. stability, **a module addition to the four specified objectives**: cut surviving shipments as
   little as possible (`tolerance_stability`), so the optimizer never reshuffles trade the shock did
   not touch; only coverage or the supplier-share limit can require a cut. In reallocation,
   specialization therefore acts on the supply stability leaves free;
2. preserve revealed specialization: exporter weight `min(RCA, rca_cap) / rca_cap`, or the
   exporter's production / capacity share where a proxy exists and is larger (trade misses
   production used at home), ties by world export share (`aib_specialization_weight()`);
3. favour relationships with stronger Export Opportunity (exporter -> market);
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

A configurable **maximum supplier share** caps any candidate's share of the universe's post-shock
supply; the frontier runs 25%, 40%, 60% and 100% (`max_supplier_share`) and records, per limit,
coverage, HHI, largest and top-three shares, the **specialization score** (supply-weighted
objective-2 weight of the optimized supply) and the **second-shock coverage** (coverage if the
optimized allocation then also lost its largest supplier, `coverage x (1 - largest share)`: the
resilience a tighter limit buys). Two readings answer the policy question:

* `tightest_costless_max_share`: the tightest limit whose coverage stays within 2 points of the
  unconstrained optimum (diversifying that far costs nothing against this shock);
* `min_required_diversification`: the **loosest** limit at which second-shock coverage still reaches
  `frontier_acceptable_coverage` (80%), i.e. the least diversification that buys acceptable
  resilience, with `specialization_retained` (specialization score there over the unconstrained one).
  NA means no tested limit reaches it: the node needs capacity, not diversification.

Maximum diversification is not assumed desirable: where coverage falls as the limit tightens,
specialization pays. In the conservative scenario a supplier whose locked shipments already exceed
the limit may not expand; in reallocation the limit can require cuts, chosen to be as small as
possible.

## 7. Investment Partnership Prioritization (marginal capacity simulation)

Only after optimized trade, and only for nodes whose residual gap is at least `min_residual_gap`
(5%) under the simulated shocks and scenarios (`investment.shocks`, `investment.scenarios`: both
scenarios, kept apart). For each eligible host a
standardized synthetic increment (5%, 10%, 20% of the node's universe demand) is added and the
optimizer rerun:

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
mean Export Opportunity (supplier -> buyer) and Friendshore (buyer rating the supplier as host).
Where a shared node has two targets under one shock, the run with the larger exposure stands for it.
`emergent_aib_country_summary.csv` keeps each economy's roles apart (trade supplier, trade buyer,
investment host, investor, each with stage participation and the coordination dividend); there is no
single ranking. The network is the primary result.

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
shock and under the other scenario is reported too (`class_<shock>`, `class_<scenario>`); the
compacts use the headline.

The labels are interpretations; the continuous columns stay beside them. Each node also gets a
policy-need profile (coordination = dividend, capacity = residual gap, specialization = excess
concentration + coverage cost of diversifying) used for policy coherence.

## 10. Sectoral Compact formation

Intervention nodes (COORDINATE, BUILD, MANAGE) are grouped (`aib_group_nodes()`, complete linkage, so
every pair inside a compact passes) only when all three hold:

* **A supply-chain coherence**: within two steps on the supply-chain graph, whose edges are the same
  stage of one technology plus the material links of `material_links` (rare-earth ores -> compounds
  -> metals -> magnets -> motors / generators; graphite -> anodes; lithium minerals -> lithium
  chemicals -> cathodes; silicon -> polysilicon -> wafers -> cells -> modules; ...);
* **B partner-network overlap or complementarity**: cosine similarity >= 0.35 of the nodes' supplier
  vectors, or of one node's suppliers with the other's buyers (lost supply). The supplier vectors are
  the **recurring** relationships: optimized supply shares summed over every shock, plus the leading
  investment hosts of every simulated shock;
* **C policy coherence**: cosine similarity >= 0.5 of the policy-need profiles.

So anodes and cathodes join only if their partners and needs match, and a technology can yield
several compacts or none.

**Minimum sufficient coalition** (`aib_coalition_path()`): demand anchors are the importers holding
60% of the compact's lost supply (at most 6). Starting from them, the economy that most reduces the
shortfall against the 99% target (summed over nodes, each capped) is added until every node reaches
it or nothing helps; country count is the coordination-complexity proxy. Non-members' surviving
shipments continue; members pool what they can direct to the anchors (conservative: their shipments
already headed there plus freed supply; reallocation: all their observed exports) plus one 10%
increment at each member that is a leading host of a BUILD node. No supplier limit applies in this
test. The coalitions at 90%, 95% and 99% come from the same path, so membership grows visibly as the
target rises; the 95% coalition is the minimum coalition, and economies added only for 99% are
associate partners.

**Compact or not.** The anchors' coverage through their existing relationships (state B, no
coordination) is the fragmented baseline. A group is a `candidate compact` only when its coalition
lifts the anchors' coverage by at least `min_compact_gain` (5 points) over that baseline; otherwise
it is `compact unnecessary`; a coalition of fewer than 3 countries is a `bilateral arrangement
sufficient`.

**Roles** come from the results: demand anchor; specialized supplier (RCA >= 1 and >= 5% of the
anchors' optimized supply, Midstream) or resource supplier (the same, Upstream); capacity-building /
investment host (a leading host of a BUILD node); technology partner (RCA >= 1 and >= 5% of world
exports in a BUILD or MANAGE node); bridge partner (supplies one node and is a large loser of another,
supplies several stages, or sits in several compacts' coalitions); associate partner. A country can
hold several roles.

**Archetype** from the lost-supply-weighted class mix: Trade / Coordination (COORDINATE dominant,
>= 60%), Investment / Capacity (BUILD), Specialization (MANAGE), otherwise Integrated. The full mix
and a secondary class are reported, and `key_policy_needs` keeps the instruments of every class
holding at least 20% of the lost supply, so a mixed chain keeps both sets of tools. A group whose
minimum coalition has fewer than 3 countries is a bilateral arrangement, not a compact.

**Coverage comparison** per compact (universe demand of its nodes): shock (static) -> existing
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

`aib_validation_cases.csv` and `.md` write the full chain for permanent magnets, rare-earth compounds,
battery anodes and solar wafers (`validation_cases`). They report what the model produced; nothing
was tuned to them. Results of the 2026-10-16 run (global universe, China removed, reallocation):

| Case | Exposure | Existing | Optimized | Dividend | Residual gap | Class | Compact |
|---|---|---|---|---|---|---|---|
| Permanent magnets | 53.0% | 53.2% | 53.2% | 0 | 46.8% | BUILD (hosts Malaysia, Romania, UK) | AIB-C01, the EV / wind drive-train compact |
| Rare-earth compounds | 39.1% | 80.9% | 100% | 19.1 pts | 0 | COORDINATE | AIB-C11 with rare-earth metals: Japan-US, a bilateral arrangement |
| Battery anodes | 59.0% | 49.8% | 49.8% | 0 | 50.2% | BUILD (hosts Indonesia, UK, Brazil) | AIB-C02, the battery compact |
| Solar wafers | 26.2% | 90.5% | 90.6% | 0 | 9.4% | LOW INTERVENTION | none |

Reading them, with the data-quality flags the chains carry:

* Magnets and anodes: China's exports are not replaceable by redirecting anyone's observed exports;
  the gap is capacity. Coordination adds nothing; finance and offtake do.
* Rare-earth compounds: Myanmar, Laos and Malaysia ship enough of HS 2846.90 to China that, freed,
  it covers everyone else's lost supply, but only through new relationships (Laos to Japan, Malaysia to
  Korea). The heading mixes mined concentrates with separated oxides, and China holds 84% of refining
  against 15% of these exports: the trade answer is an upper bound and separation capacity is the
  real constraint (flagged).
* Wafers: HS 3818.00 is mostly semiconductor wafers (Japan, Taiwan, Korea), so the node copes; China
  holds 98% of solar-wafer capacity (GCIM), which the trade data cannot see (flagged). The solar
  wafer exposure is a production question, not a trade one.

## 12a. How to read compact coverage

A coalition's gain for its anchors (`compact_gain_over_fragmented`) can come partly from members
keeping or redirecting their own exports toward each other, which is zero-sum for the importers
outside the coalition. `coverage_with_trade_coordination` (all importers of the compact's nodes)
shows whether the compact adds supply to the system: in the BUILD compacts it equals the existing
state, so the gains there come from the investment response, not from trade coordination.

## 13. Outputs (`<outputs_dir>/aib/`)

| File | Key | Content |
|---|---|---|
| `allied_trade_stress_test.csv` | level x tech x stage x sub-sector x shock x scenario | exposure, static / existing / optimized coverage, dividend, residual gap, HHI, largest supplier, data quality |
| `allied_trade_partnerships.csv` | node x shock x scenario x exporter x importer | optimized new / expanded relationships: baseline and optimized USD, share of the importer's loss restored, RCA, world share, Export Opportunity, compatibility |
| `allied_trade_frontier.csv` | node / stage x shock x scenario x max supplier share | coverage, coverage loss vs unconstrained, HHI, largest and top-three shares, specialization score, second-shock coverage, tightest costless share, minimum required diversification |
| `allied_investment_partnerships.csv` | node x shock x host x increment | marginal resilience gain, residual gap before / after, HHI and largest-share change, capability, Friendshore, rank |
| `allied_investment_pairs.csv` | node x shock x investor x host | supply from the new capacity, Friendshore |
| `emergent_aib_edges.csv` | supplier x buyer x scenario | the emergent network: trade and investment selections, contributions, stage participation, Export Opportunity, Friendshore |
| `emergent_aib_country_summary.csv` | economy x scenario | roles kept apart |
| `aib_node_classification.csv` | node | class, reasons, continuous results |
| `aib_compact_candidates.csv` | compact | the compact summary (coalitions at each target, roles, coverage comparison, policy needs) |
| `aib_compact_nodes.csv` | compact x node x technology | the Gates & Fences handoff table |
| `aib_compact_members.csv`, `aib_compact_coalition_path.csv` | compact x economy / step | roles; coverage as members are added |
| `allied_subsector_data_quality.csv` | node | flags and category |
| `aib_validation_cases.csv` / `.md` | case x step | validation chains |

The dashboard bundle (`dashboard/public/data/aib/`, schema `osi-aib` v1) is written by
`scripts/84_export_aib_data.R`.

## 14. Results of the 2026-10-16 run (global universe)

92 nodes after the upward aggregation (monazite into rare-earth compounds, uranium ores into uranium
compounds): 23 high confidence, 68 usable with caution, 1 insufficient (cobalt ores, alone in its
stage); 91 optimized. China removed, reallocation:

* Exposure by technology (share of traded demand): Batteries 48%, Solar 27%, EV 26%, Wind 23%,
  Electric Grid 18%, Semiconductors 19%, Nuclear 11%, Gas 5%, Coal 4%, Oil 3%. Residual gap after
  optimized trade: Batteries 41%, EV 22%, Solar 19%, Wind 19%, Grid 12%, Nuclear 7%, others below 4%.
* Classes: BUILD 43, LOW INTERVENTION 45, MANAGE SPECIALIZATION 2 (lithium chemicals: Chile holds 61%
  of the optimized supply; polysilicon: the United States 41%), COORDINATE 1 (rare-earth compounds),
  INSUFFICIENT DATA 1. In a global universe the coordination dividend is small almost everywhere:
  the existing network is dense enough to route the supply the shock frees, so the binding
  constraint is capacity, not connections.
* 13 groups: 10 candidate compacts (all Investment / Capacity: EV and wind drive train; batteries;
  two grid-equipment compacts; oil and gas equipment; solar silicon and cells; coal plant; coke;
  nuclear balance of plant; natural graphite), two bilateral arrangements (lithium chemicals,
  Chile-Korea; rare earths, Japan-US) and one "compact unnecessary" (polysilicon: the coalition adds
  4 points over existing relationships). Batteries are one compact here; electric-grid equipment splits
  into two compacts with different partner networks.
* Network: the relationships that recur most are Korea -> India, Japan -> Brazil, Korea -> Singapore
  and Germany -> USA in trade (each selected in 80-150 node-shock runs across 8-10 technologies), and
  Finland -> Germany and Malaysia -> USA in investment.
* 6 of 524 existing-network runs (integrated circuits, heavy fuel oils) report a tie-break step that
  GLPK could not solve; their coverage is from the solved coverage step (status columns say so).

## 15. Limitations

* Trade-based demand omits domestic production consumed at home; coverage is of traded demand.
* HS6 headings mix grades and uses (rare-earth compounds mix concentrates and separated oxides;
  wafers are mostly semiconductor wafers; cables and converters serve many technologies). The
  data-quality flags say where.
* One reference year (2024); 2020 is used only for the time-series check.
* Shocks are applied node by node; there is no cross-node capacity coupling (a magnet shortage does
  not by itself reduce motor exports in the model).
* Export Opportunity and Friendshore are technology-stage indices, not sub-sector ones.
* The coalition test pools members' supply without link restrictions (state C logic) and uses the
  headline supplier-share limit.
