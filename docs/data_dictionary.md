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

`reserves_tbl`: `source` is "USGS Mineral Commodity Summaries 2026" for the nine mineral reserve variables and their technology roll-ups, "EI Statistical Review of World Energy (2026 edition)" for oil, gas and coal, and the OECD-NEA/IAEA Red Book for `Nuclear Reserves`. A holder USGS lists without a figure (NA, W, s) has raw and index `value` NA, like an economy USGS does not list; a reported zero ('—') is 0.

**EI source label.** Every theme built from the EI Statistical Review (energy access, energy consumption, energy imports, reserves, critical-minerals production, and the fuel rows of production depth and momentum) carries `source = "EI Statistical Review of World Energy (2026 edition)"`. The edition is derived from the newest year of the EI panel the builder reads (data year + 1; `ei_stat_review_source()`, `R/utils/schema.R`), not written out; until October 2026 the labels read "(2025)", the data year.

Theme variables whose name, unit or values a reader needs to know (changed 2026-10-10):

| Table | Variable (`data_type`) | Meaning |
| --- | --- | --- |
| `production_depth_momentum_tbl` | `production_latest` (raw), mineral Upstream cells of Batteries, Electric Grid, Electric Vehicles, Green Hydrogen, Solar, Wind | Demand-weighted share of world mined supply of the technology's minerals in the IEA base year, 0–1; `source = "IEA Critical Minerals Database"`. It was an unweighted mean of mined kilotonnes, labelled EI. |
| `production_depth_momentum_tbl` | `size`, `growth_abs`, `Overall Production` (index), same cells | S-curves of the level, of its absolute change to 2035 (in base-year world-supply units; 0 for a listed non-producer) and of 2 × size + growth. Fuel rows: EI production; Nuclear Upstream: Red Book. |
| `future_demand_tbl` | `2035`, `global_growth` (raw, EJ), `global_demand_size`, `global_demand_growth` (index), `Overall Global Demand` | IEA WEO global terms, copied to every country (the three middle names had no `global_` prefix before, and collided with the BNEF terms `growth`, `demand_size`, `demand_growth`). The table has one row per (Country, tech, supply_chain, variable). |
| `market_share_manufacturing_tbl`, `foreign_dependency_tbl` | `Overall Market Share` / `Market Share` (raw), clean-tech Midstream | Share of IEA world manufacturing capacity in 2035 (the pipeline), the source's "Other" row included. "Other" itself is not a row: it is 1 minus the listed entities' shares, counting the EU once. The technology is `Heat Pumps` (it read `Heat.Pumps`). |
| `foreign_dependency_tbl`, `market_share_manufacturing_tbl` | EV Midstream: `Domestic.sales`, `Domestic.Production` (raw, million vehicles), `net_export_ratio` (raw), `market_share` (raw and index), `production`, `sales`, `net_export`, `ev_Midstream` (index) | Values of the economy's IEA EV region, copied to each EI economy in it; `explanation` ends "IEA region: <region> (the regional value is copied to each economy in it)". `net_export_ratio` = (production − sales) / sales, positive = net exporter (was `import_share`; index `net_export` was `import`). |
| `import_dependence_tbl` | `Self-sufficiency` (raw, %) | 100 × production / consumption for oil, gas, coal and fossil fuels; 100 balanced, 0 fully imported; NA where consumption is 0. It replaced `Import share` (= self-sufficiency − 100). `Production surplus/deficit` raw is the balance in EJ, its index row the Energy Imports index. |
| `import_dependence_tbl` | `Production surplus/deficit` (raw and index), `Self-sufficiency`, `Overall Energy Imports Index` (changed 2026-10-11) | Where EI has no production row for the economy and fuel ("not reported separately": none, or inside an EI "Other <region>" residual), production is the EIA International Energy Statistics value in EI's units (same year; gas and coal from EIA's latest, 2024, for 2025), observed data: `source` reads "<EI source>; EIA International Energy Statistics" and the `explanation` of the raw and `Production surplus/deficit` index rows ends "EI does not report <fuel> production for this economy separately; EIA International Energy Statistics (<year>): <conversion>". NA only where EIA has no comparable value (2025: Estonia coal, EI counting oil shale as coal); the explanation then ends "Not reported separately: ... the value is NA and the index takes the pillar's missing-data rule" (`global_average`, flagged imputed). A reported EI zero stays 0; an EIA zero is 0. Fossil sums the fuels with a value; NA only where none has one. Where a fuel's production is of an earlier year than its consumption (2025 gas and coal production from EIA 2024), `Self-sufficiency` is that of the production year (EI consumption of 2024) and `Production surplus/deficit` = consumption × (self-sufficiency − 1); the explanation then adds "Production is EIA's <year> value and consumption EI's <year>, so self-sufficiency pairs them in <year>: ... The balance applies that share to the <year> consumption of ... (production taken as the <year> value x <factor>, the change in EI's <fuel> consumption <year>-<year>)", and the Fossil rows "Each fuel's production enters with its consumption of the same year". The producers EI lists without consumption (EIA 2024 consumption, EI 2025 production) are not paired; their explanation names the EIA consumption year. The overall row keeps the generic overall text. |
| `production_depth_momentum_tbl` | Oil, Gas, Coal Upstream: `production_latest` (raw), `size`, `growth_abs`, `Overall Production` (changed 2026-10-11) | A year EI does not report for an economy it lists elsewhere takes the EIA value of that year in EI's units (2025 gas and coal: EIA 2024); `source` adds "; EIA International Energy Statistics" and `explanation` ends "EI does not report <fuel> production for this economy separately; EIA International Energy Statistics for <years> (<conversion>)". Estonia coal is NA in every year (EIA's coal is not EI's: oil shale), so its index rows are NA and the pillar's `global_average` rule fills them. Was 0 until 2026-10-11. Downstream (generation) cells EI does not report are still 0. |
| `trade_concentration_tbl` (ES), `export_feasibility_tbl` (EO) | all trade variables (changed 2026-10-11) | 20 cells since the Upstream baskets of 2026-10-11 (15 before): the 18 headline cells with an HS6 basket, among them the new Electric Vehicles, Solar, Wind and Nuclear Upstream, plus Semiconductors Upstream and Midstream, which no index scores. The HHI index ranks the 18 headline cells and places the Semiconductors cells against them (`median_scurve_ref()`). The basket codes and their sub-components are in `data/reference/energy_hs6_master.csv`. |
| `data/raw/eia_intl_fossil_production.csv` | `iso3`, `country`, `fuel` (oil, gas, coal), `flow` (production, consumption), `component`, `year`, `value`, `unit`, `series_id`, `value_note`, `last_updated`, `fetched_on` | Staged extract of the EIA bulk file (`scripts/fetchers/eia_intl.R`), one row per series and year, 2000 on, ISO3 geographies only. Units as EIA publishes them: TBPD (thousand b/d), BCM, TJ (gross calorific value). `value` is empty where EIA gives no number, with EIA's code in `value_note` ("--" not available, "NA", "ie" insufficient data, "w" withheld). Components: `crude_condensate` (series 57), `ngpl` (58), `dry_gas` (26), `coal` (7), `petroleum` (5, consumption, TBPD and TJ). Conversions to EI's units: `eia_intl_conversions()`. |
| `energy_prices_tbl` | `latest_price` (raw) | In the commodity's own unit, stated in `explanation` ("Unit: ...") and listed in `energy_prices_pcps_units()` (`clean`, `imf_code`, `unit`, `benchmark`), from the IMF PCPS series metadata: USD per barrel (oil), per MMBtu (gas, LNG), per gallon (propane), per pound (uranium U3O8), per metric tonne (coal, metals, iron ore, DAP). "unit to confirm" where no unit is verified (urea, the Energy Transition Metal index). `yoy_price_change_pct` is in percent. |
| `cost_competitiveness_tbl` | `nominal_rate` (raw, % per annum) | The selected IMF MFS_IR rate; zero or negative lending-rate entries, and the IMF averages built on them, are dropped as placeholders (New Zealand now reads 4.76%, 2017; Solomon Islands 10.35%, 2021). `Year` is the year of the reading. |

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
- `trade_index` (Opportunity) and `imp_trade_index` (Friendshore) rows of those tables: the two-stage trade index (README, Bilateral trade). It is the S-curve of 2024 level and 2020→2024 growth (2:1) across trading dyads within (tech, stage), then within the reporter. Growth is scored only where the 2020 flow is at least `growth_min_base_usd` (USD 1m, `config/partnership.yml` `trade`). Other trading dyads (no 2020 trade, or a smaller base) take the neutral growth score 0.5. Changed 2026-10-20: before, a smaller base was scored as growth and a missing one left the dyad on its level alone. `trade_status` is unchanged.
- `partner_development_*_tbl.rds`: the same for the partner-level Development Potential Index.
- `partner_trade_coverage_tbl.rds`: per theme × tech × stage, `n_cells`, `n_trade` (positive flow), `n_zero` (observed zero), `n_imputed` (unobserved, cell mean), `n_no_data` (no HS6 code), `share_imputed`, `trade_source`, `trade_file`, `cif_fob_factor`.
- `cm_fdi_projects_tbl.rds` (since 2026-10-11): one row per CM-FDI project (Cleantech Manufacturing FDI Dataset, Net Zero Industrial Policy Lab, edition 2026.10.05; `R/themes/partnership_strength/cm_fdi.R`): `project_id`, `sector`, `parent`, `home_country`, `home_iso3` (the parent's HQ economy), `destination_country`, `destination_iso3`, `destination_city`; `status`, `operational`, `year_announced`, `year_completed`; `technology` (CM-FDI label), `tech`, `activity`; `capital_usd_m` (US$ m as announced, NA = undisclosed, never 0), `capital_disclosed`; `expansion`, `phase`, `joint_venture`, `jv_local`, `jv_local_state`, `jv_company`; `source_*` (CM-FDI's source text per field: parent, status, location, amount, ownership, announced, completed, activity, jv); `index_use` (used by the indices), `index_exclusion` (first excluding rule), `supply_chain`, `china_parent` (HQ in `china_hq_iso3`: CHN, HKG, MAC), `attribution_weight` (1, or `jv_parent_share` = 0.5 for a joint venture).
- `partner_cm_fdi_ties_tbl.rds`: one row per (`investor_iso`, `destination_iso`, `tech`, `supply_chain`) with a project used by the indices: `n_projects` (attributed), `n_disclosed` (attributed, with a disclosed value), `value_usd_m` (attributed disclosed value; NA when none is disclosed). Friendshore reads it with reporter = investor and partner = destination.
- `partner_cm_fdi_exposure_tbl.rds`: one row per (`destination_iso`, `tech`, `supply_chain`) with a defined exposure: `n_project_rows`, `n_projects` (attributed), `n_china`, `n_disclosed`, `value_usd_m`, `value_china_usd_m`, `share_count`, `share_value`, `chinese_parent_exposure` (the share used), `exposure_basis` (`value` / `blend` / `count`; `china_hq` for CHN, HKG and MAC, set to 1). No row = undefined (fewer than `min_projects` project rows), not 0.
- `partner_cm_fdi_coverage_tbl.rds`: per covered (tech, stage), `n_cells` (dyad cells), `n_tie` (cells holding a CM-FDI tie), `n_tie_reporters`, `n_exposure` (cells with an exposure value), `n_exposure_partners`.
- CM-FDI in `partner_friendshore_*`: the variable `chinese_parent_exposure` holds the share, with rows only where it is defined (read a missing row as NA); its contribution is (1 − share) × weight / available weight. The `outbound_index` rows of (Solar, Wind, Batteries, Electric Vehicles) × Midstream are the CM-FDI tie and cite CM-FDI in `source`; the dyad builder's internal table carries `outbound_source` (`cm_fdi` / `imf_dip`). In those cells both CM-FDI parts count as observed in `imputed_weight_share` where defined and are left out of the cell where undefined.

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
| `_observed_hhi.csv` | per basket stage (18 rows since the Upstream baskets of 2026-10-11; 14 before): node export HHI (EU extra-EU), `top_node`, `top_share`, `effective_exporters`, `year` (the lambda calibration target) |
| `_portfolio_caps.csv`, `_portfolio_counts.csv`, `_topk_by_stage.csv` | caps from GDP, top-5 counts, top-5 membership by stage |
| `_build_candidates.csv`, `_input_checks.csv` | candidate producers; input uniqueness checks |

## Allied Industrial Base (`scripts/31`, `scripts/32`, `outputs/aib/`)

Method: `docs/aib_methodology.md`. Shares are of the demand scope's baseline traded demand for the
node (coverage 0.8 = 80% served after the shock). Every table carries `demand_scope` (the importers
whose supply is restored: `coalition`, `configured` or `global`), `candidate_universe` (the exporters
that may supply them), `scenario` (`conservative` / `reallocation`) and `shock_id`; the trade tables
also carry `shock_semantics` (`hold` / `release`) and `supply_constraint` (`production_constrained`
/ `trade_only`), every variant in one file. The compact tables are for the headline shock, scenario
and semantics, one block per supply-constraint mode (`config/aib.yml`). `in_strategic_scope` marks
nodes in the strategic scope (`electro_industrial` by default); only they form compacts. Trade edges run exporter -> importer; investment edges investor (the
importer served) -> host.

| File | Columns (main) |
| --- | --- |
| `allied_trade_stress_test.csv` | `level` (subsector / stage / technology), `demand_scope`, `candidate_universe`, `universe` (both, as a label), `shock_semantics`, `supply_constraint`, `tech`, `supply_chain`, `sub_sector`, `node_id`, shock (`shock_id`, `shock_label`, `shock_level`, `target_iso`), `scenario`, `demand_usd`, `lost_usd`, `shock_exposure`, `static_coverage`, `existing_coverage`, `optimized_coverage`, `coordination_dividend`, `residual_gap`, `residual_gap_usd`, recoveries, `redirectable_supply`, baseline / static / existing / optimized `hhi`, baseline and optimized largest supplier and share, `existing_diversion` / `optimized_diversion` (supply drawn from buyers outside the scope), `optimized_instantaneous_n1_retained_coverage`, `optimized_n1_reoptimized_coverage`, `physically_constrained`, `n_candidates`, `n_new_links`, `n_expanded_links`, `max_supplier_share`, solver status, `data_quality`, node trade facts, `in_strategic_scope` |
| `allied_trade_partnerships.csv` | node, shock, scenario, `exporter_iso`, `importer_iso`, `relationship` (new / expanded / domestic retention), `target_iso`, `baseline_usd`, `optimized_usd`, `delta_usd`, `delta_share_of_demand`, `in_existing_network_solution`, `share_of_importer_loss_restored`, `export_opportunity` (NA when unscored), `compatibility`, exporter RCA, world share and production share |
| `allied_trade_frontier.csv` | `level`, node or stage, shock, `target_iso`, scenario, `max_supplier_share`, `optimized_coverage`, `existing_coverage`, `static_coverage`, `coverage_loss_vs_unconstrained`, optimized HHI, largest and top-three shares, `specialization_score`, `instantaneous_n1_retained_coverage` (coverage x (1 - largest share)), `n1_removed_supplier`, `n1_reoptimized_coverage` (the largest remaining supplier removed and the trade re-optimized), `tightest_costless_max_share`, `min_required_diversification` (on the re-optimized N-1), `min_required_diversification_instantaneous`, `specialization_retained`; `shock_semantics`, `supply_constraint` |
| `allied_trade_growth_sensitivity.csv` | node, scenario, `existing_growth_label` (1.25x / 2x / 4x / unlimited), `existing_growth`, `existing_coverage`, `coordination_dividend`, `node_classification` at that bound (headline scenario), optimized and static coverage |
| `allied_investment_partnerships.csv` | node, shock, scenario, `shock_semantics`, `supply_constraint`, `increment`, `host_rank`, `host_iso`, coverage and residual gap before / after, `marginal_resilience_gain`, `additional_demand_served_usd`, HHI and largest-share change, the gain under the diversified cap, `capability_tier`, `capability_basis`, `capability_value`, node RCA / world share / production share, `friendshore_need_weighted`, `compatibility_need_weighted`, `top_investor_partners` |
| `allied_investment_pairs.csv` | node, shock, `investor_iso`, `host_iso`, `supply_from_new_capacity(_usd)`, `investor_lost_share`, `friendshore` |
| `emergent_aib_edges.csv` | one row per directed relationship as goods move (`supplier_iso` -> `buyer_iso`) and scenario: `n_trade_selected`, `n_investment_selected` (first-ranked hosts only), `n_shocks`, `n_subsectors`, `n_technologies`, `technologies`, `subsectors`, `n_high_severity`, `upstream` / `midstream` / `downstream`, headline trade volume and contributions, `cumulative_trade_resilience_contribution`, `cumulative_coordination_dividend`, `cumulative_investment_resilience_contribution`, `export_opportunity` (supplier -> buyer), `friendshore` (buyer rating the supplier as host) |
| `emergent_aib_country_summary.csv` | per economy and scenario, the trade-supplier, trade-buyer, investment-host and investor sides kept apart (partners, selections, contributions, coordination dividend, partners by stage), technologies, roles, headline lost supply |
| `aib_node_classification.csv` | node, data quality, `in_strategic_scope`, `node_classification` (headline mode), `policy_needs` (multi-label, e.g. `BUILD + MANAGE SPECIALIZATION`) and `needs_coordinate` / `needs_build` / `needs_manage_specialization`, `classification_reason`, `classification_warning` (PHYSICAL CAPABILITY NOT VALIDATED) and `physical_validation`, `physical_data` (complete / incomplete / none), `physical_divergence`, `trade_share_without_physical_data` (world trade from exporters the physical data do not list: held at their current shipments in the production-constrained mode), the continuous results it reads (incl. N-1 and diversion), priority hosts, policy-need profile, `class_<shock>`, `class_<scenario>`, `class_release` (with its dividend and gap), `class_trade_only` / `class_production_constrained` (and the other mode's warning and validation), `class_growth_<bound>`, `classification_warning_<variant>` beside every variant class, `coordinate_sensitive_to_growth` |
| `aib_compact_candidates.csv` | `compact_id`, `compact_name`, `technology`, `included_subsectors`, `compact_type` (candidate compact / compact unnecessary / bilateral arrangement sufficient), `compact_archetype`, `class_mix`, `secondary_class`, `coalition_method` (exact binary program), `minimum_coalition` (95%) and `minimum_coalition_90/95/99`, `minimum_coalition_size`, `target_reached_95/99`, `best_min_anchor_coverage`, `greedy_sufficient_coalition` (the heuristic), `core_coordinating_members`, `critical_supply_partners` (no single substitute), `critical_external_dependencies` (leave-one-out, supplying >= 1% of the anchors' demand), `critical_external_dependencies_marginal` (leave-one-out, below 1%), `external_dependencies`, `associates_reach_99`, roles (`demand_anchors`, `specialized_suppliers`, `investment_hosts`, `technology_partners`, `bridge_partners`, `associate_partners`), `node_policy_needs`, `physical_warnings`, `baseline_shock_exposure`, `coverage_static`, `coverage_without_coordination`, `coverage_with_trade_coordination` (coalition members only), `coverage_after_investment_simulation`, `coverage_global_optimum`, `coordination_dividend`, `residual_gap(_after_investment)`, `anchor_coverage_fragmented`, `anchor_coverage_compact`, `compact_gain_over_fragmented`, `key_policy_needs`, `supply_constraint` |
| `aib_compact_nodes.csv` | `compact_id` (empty: no compact needed), `technology`, `stage`, `sub_sector`, `node_classification`, `severity`, `coordination_dividend`, `residual_gap`, `concentration`, `key_trade_partners`, `priority_investment_hosts`, `strategic_problem`, `trade_solution`, `investment_need`, `concentration_risk`, `recommended_policy_treatment`, `yf_cell`, `yf_subcomponent_id`, `policy_needs`, `classification_warning`, `physical_validation`, `supply_constraint` |
| `aib_compact_members.csv`, `aib_compact_coalition_path.csv` | members with `role`, `basis`, `in_minimum_coalition`, `coalition_tier` (core coordinating member / critical supply partner / associate partner / critical external dependency (marginal) / external dependency), `critical_supplier`, `loo_critical` (leave-one-out with the coalition fixed), `substitutable_by` (single substitutes), `substitutes_needed`; the greedy path (`step`, `added`, `members`, `min_coverage`, `node_coverage`) |
| `aib_compact_leave_one_out.csv` | compact, node, `iso3`, `member`, `supply_share` (its current shipments / the anchors' demand), `added_share`, `coverage_with`, `coverage_without` (coalition fixed), `coverage_without_open` (every candidate free to join), `critical`, `irreplaceable`, `material`, `target` |
| `allied_subsector_data_quality.csv` | node, `data_quality`, `optimize`, `physical_data`, `physical_completeness`, `physical_divergence`, `physically_divergent`, `trade_share_without_physical_data`, `flags`, HS6 counts, essential / legacy / caveat / shared / HS2022 code counts, trade facts, production-concentration fields, `aggregated_into` |
