# Case audit: CASE_012 United States / Nuclear / Deployment

Generated 2026-11-20T00:00:00Z by scripts/qc/build_case_audit.R from <processed-dir>/outputs/strategic_position_tbl.rds (pipeline-5882f35f044e) and config/strategic_position.yml. Shipped posture bundle: created 2026-11-17T20:33:31Z, git 0496e7ba.

**Choice (pipeline): watch. Choice (shipped bundle): watch.** 19 links checked: 8 reproduced, 0 differ, 6 not reproducible from shipped/pipeline data, 1 provenance gaps, 3 recorded only. Bundle fields differing or missing: 0.

Case: United States: deployment of nuclear power

## 1. Source datasets

| item | value | detail |
|---|---|---|
| families_applicable | 3 |  |
| candidate_raw_inputs | 48 | raw_inputs_manifest.yml entries whose required_by names a script that builds a family's processed table (an upper bound: the manifest records inputs per script, not per cell) |

**families**

| family | processed_table | scripts | stages | raw_inputs | n_raw_inputs |
|---|---|---|---|---|---|
| position: CP families | outputs/index_outputs.rds (economic_opportunity / energy_security variable contributions); outputs/innovation/innovation_theme.csv (innovation_position) | scripts/20_build_indices.R;scripts/10_build_themes.R;scripts/29_build_innovation_position.R | all | comtrade_energy_trade, comtrade_total_export, critmin_export, critmin_import, critmin_mirror_import, critmin_total_export, eia_intl_fossil_production, ilo_earnings_by_activity, imf_commodity_prices, imf_lending_rates, imf_ppi, wdi_country_info, wdi_gdp, consolidated_hs6_energy_tech_long, dual_use_scores, hs6_categories_with_essential, hts_codes_categories_bolstered_final, comtrade_vintage, bcg_market_size, bnef_lcoe_viewer, bnef_neo, bnef_supply_chains, cat_country_ratings, ei_stat_review, ei_stat_review_wide, ev_midstream_capacity, gcim_investment_capacity, geothermal_lcoe, gta_nipo, hs92_country_product_year_4, hs92_country_product_year_6, iea_clean_tech_guide, iea_cleantech_midstream, iea_critical_minerals, iea_ev_outlook, iea_pams, iea_relative_costs, ipcc_ghg_intensity, solar_potential, uranium_production, uranium_resources, usgs_mcs_world_data, wb_wind_country, weo_annex_a, uspto_y02_patents, iea_rdd_public | 46 |
| deployment need | deployment_need_tbl.rds | scripts/95_build_deployment_need.R | Deployment | ember_yearly_electricity, ember_renewable_targets_2030 |  2 |
| Secure engine (Tests 1-5) and capability floor | outputs/strategic_position_inputs.rds (posture inputs) | scripts/26_build_strategic_position.R | all |  |  0 |

**raw inputs**

| id | source_name | vintage | licence | url |
|---|---|---|---|---|
| comtrade_energy_trade | UN Comtrade (energy-tech HS6 exports and imports) | 2024; World rows of the all-reporter dyad pull (main 2026-10-07/08, Electric Grid Upstream supplement 2026-10-09, Upstream-basket supplement 2026-10-12 UTC), derived 2026-10-09 and extended 2026-10-12 UTC |  | https://comtradeplus.un.org/ |
| comtrade_total_export | UN Comtrade (total exports, TOTAL commodity code) | 2024; pulled 2026-10-09 |  | https://comtradeplus.un.org/ |
| critmin_export | UN Comtrade (critical minerals exports) |  |  | https://comtradeplus.un.org/ |
| critmin_import | UN Comtrade (critical minerals imports) |  |  | https://comtradeplus.un.org/ |
| critmin_mirror_import | UN Comtrade (partner-reported imports from non-reporting economies, mirror exports) |  |  | https://comtradeapi.un.org/public/v1/preview/C/A/HS |
| critmin_total_export | UN Comtrade (critical minerals total exports) |  |  | https://comtradeplus.un.org/ |
| eia_intl_fossil_production | U.S. Energy Information Administration, International Energy Statistics (bulk file INTL.zip), fossil fuel production and consumption by country | downloaded (accessed) 2026-10-11; INTL.txt dated 2026-10-03 (24,153,479-byte zip); oil production to 2025, gas and coal to 2024, oil consumption to 2024 (2025 for a few economies) | US government work, public domain (no API key) | https://www.eia.gov/opendata/bulk/INTL.zip |
| ilo_earnings_by_activity | ILOSTAT, average monthly earnings of employees by sex, economic activity and currency (EAR_EMTA_SEX_ECO_CUR_NB_A) | downloaded 2026-09-30 (the copy scripts/40 cached as data/processed/vintages/_cache_ilo_earnings.csv) |  | https://rplumber.ilo.org/data/indicator/?id=EAR_EMTA_SEX_ECO_CUR_NB_A&lang=en&type=label&format=.csv&channel=ilostat |
| imf_commodity_prices | IMF Primary Commodity Price System (wide export, includes annual YoY series) |  |  | https://www.imf.org/en/research/commodity-prices |
| imf_lending_rates | IMF lending rates (MFS_IR) |  |  | https://data.imf.org/en/Data-Explorer |
| imf_ppi | IMF producer price indices (PPI) |  |  | https://data.imf.org/en/Data-Explorer |
| wdi_country_info | World Bank WDI country reference table |  |  | https://databank.worldbank.org/source/world-development-indicators |
| wdi_gdp | World Bank World Development Indicators (NY.GDP.MKTP.CD) |  |  | https://databank.worldbank.org/source/world-development-indicators |
| consolidated_hs6_energy_tech_long | HS6 to technology / supply-chain view (tech, supply_chain, sub_sector, HS6) |  |  |  |
| dual_use_scores | Dual-use scoring by primary/secondary/tertiary category |  |  |  |
| hs6_categories_with_essential | HS6 view carrying the essential-goods flag (Technology, Value Chain, Sub.Sector, HS6, essential) |  |  |  |
| hts_codes_categories_bolstered_final | HS6 to technology / supply-chain view (Technology, Value Chain, Sub.Sector, HS6) |  |  |  |
| comtrade_vintage | Comtrade retrieval vintage stamp |  |  |  |
| bcg_market_size | BCG market sizing for technology and supply chain |  | proprietary |  |
| bnef_lcoe_viewer | BloombergNEF LCOE Data Viewer | 2026-08-11 | subscription | https://about.bnef.com/insights/clean-energy/new-energy-outlook/ |
| bnef_neo | BloombergNEF New Energy Outlook | 2024-10-29 | subscription | https://about.bnef.com/insights/clean-energy/new-energy-outlook/ |
| bnef_supply_chains | BloombergNEF Energy Transition Supply Chains | 2025 | subscription | https://about.bnef.com/insights/clean-energy/new-energy-outlook/ |
| cat_country_ratings | Climate Action Tracker country ratings |  |  | https://climateactiontracker.org/countries/ |
| ei_stat_review | Energy Institute Statistical Review of World Energy (long format) | 2026 edition (data 1965-2025) |  | https://www.energyinst.org/statistical-review |
| ei_stat_review_wide | Energy Institute Statistical Review of World Energy (workbook) | 2026 edition (Contents sheet; fuel reserve sheets at end-2020) |  | https://www.energyinst.org/statistical-review |

_23 more rows in the JSON._

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| position: CP families: raw rows -> this cell | not reproducible from shipped/pipeline data |  | 46 | the manifest names candidate raw files by script; row-level lineage from raw file to this cell is not recorded |
| deployment need: raw rows -> this cell | not reproducible from shipped/pipeline data |  | 2 | the manifest names candidate raw files by script; row-level lineage from raw file to this cell is not recorded |
| Secure engine (Tests 1-5) and capability floor: raw inputs | not reproducible from shipped/pipeline data |  |  | no raw_inputs_manifest.yml entry names the producing script in required_by: the raw source of this family is not recorded |
| candidate raw inputs: vintage and licence recorded | provenance_gap | 48 | 11 | 37 of 48 lack a vintage or a licence in config/raw_inputs_manifest.yml: comtrade_energy_trade, comtrade_total_export, critmin_export, critmin_import, critmin_mirror_import, critmin_total_export, ilo_earnings_by_activity, imf_commodity_prices, imf_lending_rates, imf_ppi, wdi_country_info, wdi_gdp, consolidated_hs6_energy_tech_long, dual_use_scores, hs6_categories_with_essential, hts_codes_categories_bolstered_final, comtrade_vintage, bcg_market_size, cat_country_ratings, ei_stat_review, ei_stat_review_wide, ev_midstream_capacity, gcim_investment_capacity, geothermal_lcoe, hs92_country_product_year_4, hs92_country_product_year_6, iea_clean_tech_guide, iea_cleantech_midstream, iea_ev_outlook, iea_pams, iea_relative_costs, ipcc_ghg_intensity, solar_potential, uranium_production, uranium_resources, wb_wind_country, uspto_y02_patents |

## 2. Source rows

**deployment need**

| deployment_need | dn_families_observed | dn_confidence | dn_demand | dn_policy_gap | dn_system | dn_demand_source | dn_policy_gap_source | dn_system_source |
|---|---|---|---|---|---|---|---|---|
| 0.5972418 | 2 | moderate | 0.8179012 |  | 0.3765823 | IEA WEO 2025 Current Policies (single-country region): electricity generation \| IEA WEO 2025 Current Policies (single-country region): generation (gen_nuclear) |  | Ember generation shares x EI Statistical Review / EIA import dependence \| Ember yearly electricity data: dispatchable capacity and demand |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| deployment_need <- deployment_need_tbl | reproduced | 0.5972 | 0.5972 |  |

## 3. Cleaning and exclusions

| item | value | detail |
|---|---|---|
| entrepot_or_registry | FALSE | exclude_iso3: HKG, MAC, MHL, LBR, PAN |
| sn_exclusion |  |  |
| posture_exclusion |  |  |
| thin_need_cell | FALSE |  |
| thin_reason |  |  |
| material_importers_in_cell | 124 | thin below 5 |
| import_material | TRUE | immaterial below USD 1,000,000 |
| self_supplied |  | self-supply guard at home_share >= 0.1 |
| cp_thin_cell | FALSE |  |
| current_trade_flag |  |  |
| opposite_bloc_basis | bloc |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| supply-chain cleaning rules | not_applicable |  |  | Deployment is decided on deployment need |

## 4. Transformations

| item | value | detail |
|---|---|---|
| percentile_rule | mid-rank (rank - 0.5) / n, ties averaged (sp_helpers.R sp_midrank_pct) |  |
| thresholds_inclusive | TRUE |  |

**recipes**

| component | variable | source | orient | rule |
|---|---|---|---|---|
| position: investment_position | Annual Investment Index | EO Investment | fill structural_zero | pillar component value (within-cell index); family = mean of observed variables |
| position: cost_position | LCOE Index | EO Cost Competitiveness | fill na | pillar component value (within-cell index); family = mean of observed variables |
| position: resource_endowment |  Reserves$ | ES Reserves | fill structural_zero | pillar component value (within-cell index); family = mean of observed variables |
| position: resource_endowment | Potential Index$ | ES Reserves | fill na | pillar component value (within-cell index); family = mean of observed variables |
| position: deployment_scale | size | EO Production | fill na | pillar component value (within-cell index); family = mean of observed variables |
| deployment need | dn_demand; dn_policy_gap; dn_system | deployment_need_tbl | config/deployment_need.yml | family = percentile of the mean of its measure percentiles; deployment_need = mean of observed families |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| CP families in the table are defined in the configuration | reproduced |  | investment_position, deployment_scale |  |
| measure-level percentiles (raw value -> measure percentile) | not reproducible from shipped/pipeline data |  |  | measure values other than import HHI, China and opposite-bloc shares are not stored per cell (index_outputs contributions) |

## 5. Normalized indicators

| item | value | detail |
|---|---|---|
| dn_demand | 0.8179 |  |
| dn_policy_gap |  |  |
| dn_system | 0.3766 |  |
| dn_families_observed | 2 |  |
| cp_investment_position | 0.3586 |  |
| cp_deployment_scale | 1 |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| CP family values <- pillar variable contributions | not reproducible from shipped/pipeline data |  |  | variable contributions live in index_outputs.rds, not in the cell table |

## 6. Weights

| item | value | detail |
|---|---|---|
| cp_families | equal over observed families (2 of 4 applicable) |  |
| export_position_momentum_weight | 0 |  |
| siv_weights | market 1, commons 1, option 1 |  |
| io_investment_momentum_weight | 0.25 |  |
| deployment_need | equal over observed families |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| weights | recorded |  |  | read from config/strategic_position.yml; no sensitivity of the SIV weights exists |

## 7. Intermediate scores

| item | value | detail |
|---|---|---|
| deployment_need | 0.5972 |  |
| dn_confidence | moderate |  |
| competitive_position_score | 0.6793 |  |
| competitive_position_percentile | 0.9656 |  |
| cp_contender_pct | 0.9656 |  |
| cp_contender_basis | all_countries |  |
| cp_n_contenders | 0 |  |
| cp_is_contender | FALSE |  |
| home_share_measured |  |  |
| t3_capability_lo | n_a |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| deployment_need = mean of observed family percentiles | reproduced | 0.5972 | 0.5972 |  |
| dn_families_observed | reproduced | 2 | 2 |  |
| CP score = mean of observed CP families | reproduced | 0.6793 | 0.6793 | families: investment_position, deployment_scale |
| cp_contender_pct = cross-country percentile (fewer than min_n contenders) | reproduced | 0.9656 | 0.9656 |  |
| capability lower bound (Secure engine Test 3) | not reproducible from shipped/pipeline data |  | n_a | the posture engine is not rerun by the audit (its inputs are in strategic_position_inputs.rds) |

## 8. Threshold decisions

| item | value | detail |
|---|---|---|
| decision_step | deployment_need |  |
| borderline | deployment_need |  |

**steps**

| step | test | value | threshold | margin | result | note |
|---|---|---|---|---|---|---|
| secure_engine | t1_consequence | yes |  |  | yes | posture engine test as stored (not rerun) |
| secure_engine | t2_vulnerability | no |  |  | no | posture engine test as stored (not rerun) |
| secure_engine | t2_path | none |  |  | none | posture engine test as stored (not rerun) |
| secure_engine | t3_capability | none |  |  | none | posture engine test as stored (not rerun) |
| secure_engine | t3_position | n_a |  |  | n_a | posture engine test as stored (not rerun) |
| secure_engine | t4_strategy | n_a |  |  | n_a | posture engine test as stored (not rerun) |
| secure_engine | t5_proportionality | n_a |  |  | n_a | posture engine test as stored (not rerun) |
| secure | secure_override (posture engine) | deploy | secure |  | no |  |
| deployment | deployment_need | 0.5972 | 0.6 | -0.0028 | low | 2 families observed |
| position | cp_contender_pct | 0.9656 | 0.75 | 0.2156 | not_top_quarter | contender FALSE, capability lower bound n_a (floor moderate/strong) |

## 9. Final classification

| item | value | detail |
|---|---|---|
| strategic_choice | watch |  |
| choice_label | Watch |  |
| strategic_posture | deploy |  |
| compete_mode |  |  |
| deploy_mode | watch |  |
| decision_step | deployment_need |  |
| posture_variant | deploy |  |
| posture_confidence | moderate |  |
| choice_possible | accelerate\|watch |  |
| pivotal_inputs | deployment_need |  |
| consistency_flag |  |  |
| robustness_class | fragile |  |
| robustness_local_flips | deployment_minus:accelerate |  |
| robustness_structural_flips |  |  |
| robustness_fence_change | TRUE |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| choice = canonical rule rerun from the stored inputs | reproduced | watch | watch |  |

## 10. Resulting recommendation

| item | value | detail |
|---|---|---|
| choice | watch |  |
| choice_label | Watch |  |
| choice_action | Keep open: buy competitively, do not force localization |  |
| yard | large |  |
| yard_and_fence | Large yard / Low fence |  |
| fence | Low fence |  |
| yard_rollup | large |  |
| fence_level | low |  |
| fence_posture | A low fence: cross-cutting safeguards only (genuine trade remedies, forced-labour restrictions, basic security and safety standards). |  |
| gate_level | wide_open |  |
| build_posture | Build what enables deployment (infrastructure, permitting, interoperability, procurement efficiency, supply-chain transparency), not domestic production. Delivery (diversified global): Supply comes from a diversified global base; the build is enabling infrastructure, not production. |  |
| benchmark | Delivered cost and deployment pace match or beat peers, with diversified supply and adequate resilience. Domestic production share is not a benchmark. |  |
| success_metrics | Delivered cost and pace of deployment; Supplier diversity (HHI) and the share held by any potentially coercive supplier; Compliance with cross-cutting safeguards (forced labour, safety, trade law) |  |
| exit_condition | Re-test if concentration rises (the largest coercive supplier or the import HHI crosses its watch line), a control layer or physical chokepoint is identified, or a credible capability case emerges. |  |
| review_interval_years | 5 |  |
| reason | Deployment need 0.60 is below 0.60: no major acceleration is required. |  |
| watch_reasons | low_deployment_need |  |
| policy_alignment | over_prioritized |  |
| policy_alignment_label | Too much intervention |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| yard = yard of the choice (yard_rollup.R) | reproduced | large | large |  |
| policy-design text <- config/posture_policy_design.csv by posture variant | recorded |  | deploy |  |
| policy alignment | recorded |  | over_prioritized | after the choice; never an input to it |

## Shipped bundle comparison

Posture bundle created 2026-11-17T20:33:31Z (git 0496e7ba), detail file posture/posture-detail-USA.b26e3b4b.json. 0 of 18 fields differ from the pipeline or are missing on one side. Note: the bundle's import_dependence, import_hhi and china_bloc are the need-family percentiles, not raw shares.

| field | pipeline | bundle | status |
|---|---|---|---|
| choice (core) | watch | watch | same |
| yard roll-up (core yd) | L | L | same |
| posture | deploy | deploy | same |
| variant | deploy | deploy | same |
| deploy_mode | watch | watch | same |
| confidence | moderate | moderate | same |
| need | 0.5972 | 0.597 | same |
| pivotal | deployment_need | deployment_need | same |
| cp | 0.9656 | 0.966 | same |
| borderline | deployment_need | deployment_need | same |
| choice_possible | accelerate\|watch | accelerate\|watch | same |
| reason | Deployment need 0.60 is below 0.60: no major acceleration is required. | Deployment need 0.60 is below 0.60: no major acceleration is required. | same |
| policy.alignment | over_prioritized | over_prioritized | same |
| dn_demand.value | 0.8179 | 0.818 | same |
| dn_system.value | 0.3766 | 0.377 | same |
| dn_families_observed | 2 | 2 | same |
| cp_families.investment_position | 0.3586 | 0.359 | same |
| cp_families.deployment_scale | 1 | 1 | same |

## Gaps: links not reproduced

- 1. position: CP families: raw rows -> this cell: not reproducible from shipped/pipeline data (the manifest names candidate raw files by script; row-level lineage from raw file to this cell is not recorded)
- 1. deployment need: raw rows -> this cell: not reproducible from shipped/pipeline data (the manifest names candidate raw files by script; row-level lineage from raw file to this cell is not recorded)
- 1. Secure engine (Tests 1-5) and capability floor: raw inputs: not reproducible from shipped/pipeline data (no raw_inputs_manifest.yml entry names the producing script in required_by: the raw source of this family is not recorded)
- 1. candidate raw inputs: vintage and licence recorded: provenance_gap (37 of 48 lack a vintage or a licence in config/raw_inputs_manifest.yml: comtrade_energy_trade, comtrade_total_export, critmin_export, critmin_import, critmin_mirror_import, critmin_total_export, ilo_earnings_by_activity, imf_commodity_prices, imf_lending_rates, imf_ppi, wdi_country_info, wdi_gdp, consolidated_hs6_energy_tech_long, dual_use_scores, hs6_categories_with_essential, hts_codes_categories_bolstered_final, comtrade_vintage, bcg_market_size, cat_country_ratings, ei_stat_review, ei_stat_review_wide, ev_midstream_capacity, gcim_investment_capacity, geothermal_lcoe, hs92_country_product_year_4, hs92_country_product_year_6, iea_clean_tech_guide, iea_cleantech_midstream, iea_ev_outlook, iea_pams, iea_relative_costs, ipcc_ghg_intensity, solar_potential, uranium_production, uranium_resources, wb_wind_country, uspto_y02_patents)
- 4. measure-level percentiles (raw value -> measure percentile): not reproducible from shipped/pipeline data (measure values other than import HHI, China and opposite-bloc shares are not stored per cell (index_outputs contributions))
- 5. CP family values <- pillar variable contributions: not reproducible from shipped/pipeline data (variable contributions live in index_outputs.rds, not in the cell table)
- 7. capability lower bound (Secure engine Test 3): not reproducible from shipped/pipeline data (the posture engine is not rerun by the audit (its inputs are in strategic_position_inputs.rds))

