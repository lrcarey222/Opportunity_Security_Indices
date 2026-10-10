# Data dictionary

## Theme tables (tidy schema)

Every theme table (`data/processed/*_tbl.rds` from `scripts/10_build_themes.R`) uses this schema:

| Column | Description |
| --- | --- |
| `Country` | Canonical economy name (one per ISO3 code; the WDI name, `Taiwan` for TWN). Aggregates and residual rows are dropped. |
| `tech` | Technology (e.g. Solar, Gas, Electric Vehicles). |
| `supply_chain` | Supply-chain stage (`Upstream`, `Midstream`, `Downstream`). |
| `sub_sector` | Optional sub-sector label; `"All"` unless `include_sub_sector` is on. |
| `category` | Thematic category (e.g. Trade, Production, Consumption). |
| `variable` | Metric name within a category. |
| `data_type` | `raw` or `index` (pre- or post-normalization); some themes also emit `weight` and `contribution`. |
| `value` | Numeric value. |
| `Year` | Observation year (an ISO date is read as its leading year). |
| `source` | Source dataset or citation. |
| `explanation` | Calculation or interpretation notes. |

`config/index_definition.yml` defines which Overall variables are used for category scores, how they are composed, and the level (`variable_levels`) each is checked at. `sub_sector` is controlled by the global `include_sub_sector` flag; when it is off, outputs use `sub_sector = "All"` so keys stay unique.

`reserves_tbl`: `source` is "USGS Mineral Commodity Summaries 2026" for the nine mineral reserve variables and their technology roll-ups, "EI Statistical Review of World Energy (2025)" for oil, gas and coal, and the OECD-NEA/IAEA Red Book for `Nuclear Reserves`. A holder USGS lists without a figure (NA, W, s) has raw and index `value` NA, like an economy USGS does not list; a reported zero ('—') is 0.

## Pillar indices (`index_outputs.rds`, `scripts/20`)

`energy_security_index` and `economic_opportunity_index`, one row per Country × tech × supply_chain:

| Column | Description |
| --- | --- |
| `Energy_Security_Index` / `Economic_Opportunity_Index` | Pillar value, available-case weighted mean of the observed category scores (0–1; ES higher = more secure). |
| `imputed_weight_share` | Share of the index weight resting on components filled by `config/missing_data.yml` (zero and average fills). |
| `rankable` | Sovereign economy and `imputed_weight_share <= tau` (`config/rankability.yml`, 0.5). Rankings use rankable cells only. |
| `rank_exclusion` | Why a cell is not rankable (`not scored`, `not a sovereign economy`, `imputed weight share above <tau>`, both, or `imputed weight share unknown`); `NA` when rankable. |

`*_category_scores` (with `imputed`, `missing_rule_applied`), `*_category_contributions` and `*_variable_contributions` decompose each value; the builders' `diagnostics` are described in `docs/energy_security_index_v2.md`.

`policy_index`: the tidy schema (`variable = "Overall Policy Index"`) plus `n_components` (components observed in the cell) and `policy_basis` (the country's basis: `"IEA PAMS + CAT"` or `"IEA PAMS only"`). `policy_eligibility`: one row per country with `n_components` (weighted components observed anywhere), `eligible`, `policy_basis` (`NA` when not eligible), `missing_components` (weighted components not observed), `missing_required` (required components not observed; `NA` when eligible) and `has_<component>`. `policy_index_provenance`: components, weights, required components, excluded components and why, eligibility rule, technologies, scored countries (`n_countries_eligible`, `eligible_countries`, `countries_by_basis`), the count with every component (`n_countries_every_component`), the component build record of `scripts/10`.

`strategic_index`, one row per Country × tech × supply_chain:

| Column | Description |
| --- | --- |
| `sector` | `"<tech> - <supply_chain>"`. |
| `Economic_Opportunity_Index`, `Energy_Security_Index` | The pillars as built. |
| `energy_security_need` | `1 - Energy_Security_Index`, the term in the formula. |
| `value` | Policy index value. |
| `policy_basis` | Basis of the country's Policy index (`"IEA PAMS + CAT"`, `"IEA PAMS only"`), joined by cell: `NA` where the cell has no Policy index value (never on a scored cell). |
| `pol` | Within-country median S-curve of the Policy index over in-sample sectors. |
| `eo`, `es_need` | Within-country S-curves of EO and of the need over the country's rankable cells (chart inputs; not in the SI). `es` is a deprecated alias of `es_need`. |
| `sc_weight` | Stage weight (Midstream 0.75, Upstream 0.50, Downstream 0.25). |
| `ghg_index`, `tech_weight` | Technology climate weight (1 = lowest lifecycle GHG intensity). |
| `trl_index` | EO Technological Readiness score of the cell. |
| `strategic_index` | 0.2 pol + 0.3 EO + 0.3 need + 0.1 stage + 0.1 climate; `NA` when excluded. |
| `si_exclusion` | Reasons a cell is out (`country not policy-eligible` (no IEA PAMS), `missing policy index`, `missing energy security`, `economic opportunity not rankable (...)`, ...); `NA` when scored. |
| `resource_base`, `resource_evidence`, `pick_exclusion` | Endowment screen of the allied pick: `TRUE`/`FALSE` in a screened Upstream sector (a positive technology-level reserve or production value; `resource_evidence` names the category), `NA` elsewhere; `pick_exclusion = "no resource base"` where the cell cannot be picked. The index is unchanged. |

`coalition_members`: the coalition `scripts/20` used (see `coalition_members()` below). `charts/allied_strategic.csv`: each member's top Upstream/Midstream sector with `iso3`, `tier`, `network_node` and `policy_basis`.

## Alignment (`scripts/12`)

`geo_ideal_points_tbl.rds`, one row per economy:

| Column | Description |
| --- | --- |
| `theta`, `n_sessions`, `sessions_used`, `n_votes` | Mean ideal point over the window and the sessions behind it. |
| `window_sessions`, `window_years` | The window (5 sessions; all-votes series: 2019–2023). |
| `d_us`, `d_china`, `s_axis`, `seg`, `beyond_anchor` | Distances to the anchors and position on the US-China axis (1 = US, 0 = China, clamped). |
| `u_us`, `u_china`, `p_us`, `bloc` | Bloc memberships, US-side membership p and bloc (`U.S.-leaning`, `Nonaligned`, `China-leaning`). |
| `bloc_rule`, `tau`, `s_us_cut`, `s_china_cut` | Rule settings and the quartile cuts. |
| `alignment_basis`, `imputed_from` | `own_votes`, `inherited` (from `imputed_from`), `neutral` or `none`. |
| `series`, `source_file`, `dataset_version` | Ideal-point series and Dataverse release. |

`geo_alignment_tbl.rds`, one row per ordered pair: `theta_*`, `s_*`, `p_*`, `bloc_*`, `basis_*` for reporter and partner, `ipd` (ideal-point distance) and `geo_alignment` = p_i p_j + (1 − p_i)(1 − p_j), plus the rule and source columns.

`geo_coalition_tbl.rds`: the alignment coalition (`member`, `reason`, `gdp_usd`, `gdp_year`, settings).

## Coalition (`scripts/13`)

`coalition_tbl.rds`, one row per economy:

| Column | Description |
| --- | --- |
| `tier` | `core`, `partner` or `none`. |
| `routes`, `route_core`, `route_agreement`, `route_supplier`, `route_supplier_reserves` | Every route the country satisfies; `routes` lists `core`, `agreement`, `supplier` (a flow measure ≥ `supplier_share`) and `supplier: reserves` (a reserves measure ≥ `supplier_reserves_share`); `route_supplier` is the supplier route by either. |
| `bloc`, `alignment_basis`, `own_vote` | Alignment inputs. |
| `gdp_usd`, `gdp_year`, `gdp_floor_usd` | Latest WDI GDP and the floor. |
| `agreements`, `qualifying_agreements` | Agreement memberships (all, and the partner-route ones). |
| `supplier_cells`, `max_supplier_share`, `max_reserves_share`, `supplier_excluded`, `supplier_share`, `supplier_reserves_share` | Supplier evidence (cells at or above their measure's bar, with measure, mineral and share), the largest flow share (read against `supplier_share`, 5%), the largest reserves share (against `supplier_reserves_share`, 10%), whether excluded as China-leaning or without a bloc, and the two bars. |
| `eu_member`, `network_node`, `node_tier` | EU membership, the network node (`EUU` for EU members) and the node's tier. |
| `reason`, `tier_with_sensitivity` | Plain-language reason; tier with the MSP Forum as an extra route. |

`eu_stage_weights_tbl.rds`: per EU member × tech × stage, `weight` (sums to 1 per stage), `basis` (`production`, `exports`, `gdp`), `measures`, `year`, `member_value`, `eu_world_share`, `coverage`.

`major_supplier_shares_tbl.rds`: per country × tech × stage × measure (flows: `iea_mineral_mining`, `iea_mineral_refining`, `ei_mineral_mining`, `ei_production`, `uranium_mine_production`, `iea_manufacturing_capacity`, `atlas_export_share`; reserves, Upstream: `usgs_reserves` (critical-mineral reserves, USGS MCS), `ei_reserves` (oil, gas and coal proved reserves, EI), `uranium_resources`): `mineral`, `measure_label`, `mineral_demand_share`, `year`, `value`, `unit`, `world_share`, `is_aggregate`, `source`, `threshold` (the row's bar: 0.10 for the reserves measures, 0.05 otherwise), `major` (country row with share ≥ `threshold`).

`coalition_members()` (`R/indices/build_indices.R`) returns the members (tier core or partner): `iso3`, `country`, `tier`, `network_node`, `source` and, when present, `routes`, `bloc`, `gdp_usd`, `eu_member`, `node_tier`.

## Partner themes (`scripts/15`) and PSI (`scripts/25`)

Direction: in Friendshore tables `reporter_iso` is the importer and `partner_iso` the supplier; in Opportunity tables `reporter_iso` is the exporter and `partner_iso` the market. Development tables are partner-level.

- `partner_{friendshore,opportunity}_tbl.rds` / `_variable_tbl.rds`: dyad × tech × stage component and index rows; `_inputs_tbl.rds` the components; `_contributions_tbl.rds` the weighted contributions; `_country_tbl.rds` the partner roll-up (mean over reporters); `_top5_tbl.rds` the dyad's top-5 (tech, stage) mean per variable, with `imputed_weight_share` on the index rows.
- `partner_development_*_tbl.rds`: the same for the partner-level Development Potential Index.
- `partner_trade_coverage_tbl.rds`: per theme × tech × stage, `n_cells`, `n_trade` (positive flow), `n_zero` (observed zero), `n_imputed` (unobserved, cell mean), `n_no_data` (no HS6 code), `share_imputed`, `trade_source`, `trade_file`, `cif_fob_factor`.

`psi_tbl.rds` (core-coalition view, headline), `psi_full_coalition_tbl.rds`, `psi_world_tbl.rds`, one row per partner (tidy schema with `tech = supply_chain = "All"`, `variable = "Partnership Strength Index"`) plus:

| Column | Description |
| --- | --- |
| `partner_iso` | Partner ISO3. |
| `n_reporters_friendshore`, `n_reporters_opportunity` | Reporters averaged in the view. |
| `imputed_weight_share`, `{friendshore,opportunity,development}_imputed_share` | PSI imputed weight share and its category parts. |
| `viewpoint` | `coalition`, `full_coalition` or `world`. |
| `rankable`, `rank_exclusion`, `rank` | PSI rankability (sovereign and share ≤ 0.5) and rank among rankable partners (1 = highest). |

`psi_*category_contributions_tbl.rds`: per partner and category, `category_score`, `n_obs`, `weight`, `weight_sum`, `category_weight`, `weighted_category_contribution`, `viewpoint`.

`psi_dyad_tbl.rds`, one row per (reporter, partner): `friendshore`, `opportunity`, `development`, `n_categories`, `Partnership_Strength_Index`, `imputed_weight_share`, `rankable`, `rank_exclusion`, `rank` (within reporter), `coalition_reporter` (reporter is core), `reporter_tier`.

## Allied network (`scripts/30`, `outputs/allied_network_*.csv`)

Codes are network nodes (`iso3c` = the node; `EUU` for the EU). In edges and flows `reporter_iso` is the producer and `partner_iso` the consumer.

| File | Columns (main) |
| --- | --- |
| `_specialization.csv` | tech, supply_chain, node (`iso3c`, `tier`, `network_node`, `n_members`, `country`), `producer_score`, `connectivity_out`, `dev_potential`, `demand_weight`, `selected`, `production_share`, GDP and demand components, `policy_imputed`, `portfolio_topk_count`, `portfolio_cap` |
| `_flows.csv` | producer → consumer `edge_weight`, `friendshore_index`, `opportunity_index`, `flow_share` |
| `_diagnostics.csv` | per stage: `method`, `fallback_reason`, `producers_forbidden`, `n_producers`, `hhi`, `effective_producers`, `max_production_share`, `min_suppliers_realized`, `min_supplier_share_realized`, `diversity_lambda`, `node_policy`, objective terms (`objective_node`, `objective_edge`, `objective_dev`, `diversity_penalty`), `hhi_tangent_max_error`, portfolio convergence |
| `_members.csv` | the countries read: `iso3`, `tier`, `network_node`, `node_tier`, `node_only` (EU members outside the coalition's own tiers) |
| `_member_shares.csv` | node share split to members: `node_production_share`, `member_weight`, `weight_basis`, `production_share_derived`, `share_is_derived` |
| `_nodes.csv` | `network_node`, `tier`, `n_members`, `members`, `gdp_usd` |
| `_node_edges.csv` | per component and stage, edge counts by `weight_basis` (`member`: two single-member nodes; `trade`, `stage`, `equal`: the weights used for an edge of a bundled node) and intra-node member edges dropped |
| `_observed_hhi.csv` | per basket stage: node export HHI (EU extra-EU), `top_node`, `top_share`, `effective_exporters`, `year` (the lambda calibration target) |
| `_portfolio_caps.csv`, `_portfolio_counts.csv`, `_topk_by_stage.csv` | caps from GDP, top-5 counts, top-5 membership by stage |
| `_build_candidates.csv`, `_input_checks.csv` | candidate producers; input uniqueness checks |
