# Case audit: CASE_002 Japan / Electric Vehicles / Refining

Generated 2026-11-20T00:00:00Z by scripts/qc/build_case_audit.R from <processed-dir>/outputs/strategic_position_tbl.rds (pipeline-5882f35f044e) and config/strategic_position.yml. Shipped posture bundle: created 2026-11-17T20:33:31Z, git 0496e7ba.

**Choice (pipeline): secure. Choice (shipped bundle): secure.** 56 links checked: 31 reproduced, 0 differ, 18 not reproducible from shipped/pipeline data, 1 provenance gaps, 6 recorded only. Bundle fields differing or missing: 0.

Case: Japan: rare-earth and battery-metal refining for electric vehicles

## 1. Source datasets

| item | value | detail |
|---|---|---|
| families_applicable | 10 |  |
| candidate_raw_inputs | 64 | raw_inputs_manifest.yml entries whose required_by names a script that builds a family's processed table (an upper bound: the manifest records inputs per script, not per cell) |

**families**

| family | processed_table | scripts | stages | raw_inputs | n_raw_inputs |
|---|---|---|---|---|---|
| need: import dependence | outputs/index_outputs.rds (energy_security_variable_contributions: deficit_gdp, Production surplus/deficit) | scripts/20_build_indices.R;scripts/10_build_themes.R | producing | comtrade_energy_trade, comtrade_total_export, critmin_export, critmin_import, critmin_mirror_import, critmin_total_export, eia_intl_fossil_production, ilo_earnings_by_activity, imf_commodity_prices, imf_lending_rates, imf_ppi, wdi_country_info, wdi_gdp, consolidated_hs6_energy_tech_long, dual_use_scores, hs6_categories_with_essential, hts_codes_categories_bolstered_final, comtrade_vintage, bcg_market_size, bnef_lcoe_viewer, bnef_neo, bnef_supply_chains, cat_country_ratings, ei_stat_review, ei_stat_review_wide, ev_midstream_capacity, gcim_investment_capacity, geothermal_lcoe, gta_nipo, hs92_country_product_year_4, hs92_country_product_year_6, iea_clean_tech_guide, iea_cleantech_midstream, iea_critical_minerals, iea_ev_outlook, iea_pams, iea_relative_costs, ipcc_ghg_intensity, solar_potential, uranium_production, uranium_resources, usgs_mcs_world_data, wb_wind_country, weo_annex_a | 44 |
| need: supplier concentration and risky suppliers | import_exposure_tbl.rds; import_partner_cells_tbl.rds | scripts/15_build_partner_themes.R | producing | allied_comtrade_energy_data, comtrade_energy_dyads_all, wb_doingbusiness, wb_wdi, wdi_country_info, wdi_gdp, energy_hs6_master, cat_country_ratings, cm_fdi_projects, imf_dip, ipcc_ghg_intensity, oecd_crs_purpose, oci_energy_finance, oci_energy_finance_institutions, oci_energy_finance_crosswalk, oecd_crs_purpose_crosswalk | 16 |
| need: current-trade blend | current_import_exposure_tbl.rds | scripts/38_build_current_import_exposure.R;scripts/37_pull_comtrade_monthly_dyads.R | producing | comtrade_monthly_dyads, comtrade_monthly_dyads_dictionary_extra, comtrade_monthly_dyads_windows, ember_china_cleantech_exports |  4 |
| need: trusted alternatives and world supply structure | trade_concentration_tbl.rds; major_supplier_shares_tbl.rds; critical_minerals_production_tbl.rds | scripts/10_build_themes.R | producing | comtrade_energy_trade, comtrade_total_export, critmin_export, critmin_import, critmin_mirror_import, critmin_total_export, eia_intl_fossil_production, ilo_earnings_by_activity, imf_commodity_prices, imf_lending_rates, imf_ppi, wdi_country_info, wdi_gdp, consolidated_hs6_energy_tech_long, dual_use_scores, hs6_categories_with_essential, hts_codes_categories_bolstered_final, comtrade_vintage, bcg_market_size, bnef_lcoe_viewer, bnef_neo, bnef_supply_chains, cat_country_ratings, ei_stat_review, ei_stat_review_wide, ev_midstream_capacity, gcim_investment_capacity, geothermal_lcoe, gta_nipo, hs92_country_product_year_4, hs92_country_product_year_6, iea_clean_tech_guide, iea_cleantech_midstream, iea_critical_minerals, iea_ev_outlook, iea_pams, iea_relative_costs, ipcc_ghg_intensity, solar_potential, uranium_production, uranium_resources, usgs_mcs_world_data, wb_wind_country, weo_annex_a | 44 |
| position: CP families | outputs/index_outputs.rds (economic_opportunity / energy_security variable contributions); outputs/innovation/innovation_theme.csv (innovation_position) | scripts/20_build_indices.R;scripts/10_build_themes.R;scripts/29_build_innovation_position.R | all | comtrade_energy_trade, comtrade_total_export, critmin_export, critmin_import, critmin_mirror_import, critmin_total_export, eia_intl_fossil_production, ilo_earnings_by_activity, imf_commodity_prices, imf_lending_rates, imf_ppi, wdi_country_info, wdi_gdp, consolidated_hs6_energy_tech_long, dual_use_scores, hs6_categories_with_essential, hts_codes_categories_bolstered_final, comtrade_vintage, bcg_market_size, bnef_lcoe_viewer, bnef_neo, bnef_supply_chains, cat_country_ratings, ei_stat_review, ei_stat_review_wide, ev_midstream_capacity, gcim_investment_capacity, geothermal_lcoe, gta_nipo, hs92_country_product_year_4, hs92_country_product_year_6, iea_clean_tech_guide, iea_cleantech_midstream, iea_critical_minerals, iea_ev_outlook, iea_pams, iea_relative_costs, ipcc_ghg_intensity, solar_potential, uranium_production, uranium_resources, usgs_mcs_world_data, wb_wind_country, weo_annex_a, uspto_y02_patents, iea_rdd_public | 46 |
| position: trade benchmark and momentum | Comtrade energy trade (read inside scripts/26; no processed table) | scripts/26_build_strategic_position.R | traded |  |  0 |
| prize: market (demand growth, investment momentum, home market) | iea_pathways_*_tbl.rds; investment_momentum_tbl.rds; home_market_tbl.rds | scripts/17_build_iea_pathways.R;scripts/10_build_themes.R;scripts/95_build_deployment_need.R | producing | comtrade_energy_trade, comtrade_total_export, critmin_export, critmin_import, critmin_mirror_import, critmin_total_export, eia_intl_fossil_production, ilo_earnings_by_activity, imf_commodity_prices, imf_lending_rates, imf_ppi, wdi_country_info, wdi_gdp, consolidated_hs6_energy_tech_long, dual_use_scores, hs6_categories_with_essential, hts_codes_categories_bolstered_final, comtrade_vintage, bcg_market_size, bnef_lcoe_viewer, bnef_neo, bnef_supply_chains, cat_country_ratings, ei_stat_review, ei_stat_review_wide, ev_midstream_capacity, gcim_investment_capacity, geothermal_lcoe, gta_nipo, hs92_country_product_year_4, hs92_country_product_year_6, iea_clean_tech_guide, iea_cleantech_midstream, iea_critical_minerals, iea_ev_outlook, iea_pams, iea_relative_costs, ipcc_ghg_intensity, solar_potential, uranium_production, uranium_resources, usgs_mcs_world_data, wb_wind_country, weo_annex_a, ember_yearly_electricity, ember_renewable_targets_2030 | 46 |
| prize: commons and option; large-yard signals | data/reference/posture_node_attributes.csv; data/reference/yards_fences_assessments.csv |  | producing |  |  0 |
| Secure engine (Tests 1-5) and capability floor | outputs/strategic_position_inputs.rds (posture inputs) | scripts/26_build_strategic_position.R | all |  |  0 |
| pathway (Catch Up anchors) | GCIM pipeline and capacity side tables (read inside scripts/26; not saved) | scripts/26_build_strategic_position.R | producing |  |  0 |

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

_39 more rows in the JSON._

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| need: import dependence: raw rows -> this cell | not reproducible from shipped/pipeline data |  | 44 | the manifest names candidate raw files by script; row-level lineage from raw file to this cell is not recorded |
| need: supplier concentration and risky suppliers: raw rows -> this cell | not reproducible from shipped/pipeline data |  | 16 | the manifest names candidate raw files by script; row-level lineage from raw file to this cell is not recorded |
| need: current-trade blend: raw rows -> this cell | not reproducible from shipped/pipeline data |  | 4 | the manifest names candidate raw files by script; row-level lineage from raw file to this cell is not recorded |
| need: trusted alternatives and world supply structure: raw rows -> this cell | not reproducible from shipped/pipeline data |  | 44 | the manifest names candidate raw files by script; row-level lineage from raw file to this cell is not recorded |
| position: CP families: raw rows -> this cell | not reproducible from shipped/pipeline data |  | 46 | the manifest names candidate raw files by script; row-level lineage from raw file to this cell is not recorded |
| position: trade benchmark and momentum: raw inputs | not reproducible from shipped/pipeline data |  |  | no raw_inputs_manifest.yml entry names the producing script in required_by: the raw source of this family is not recorded |
| prize: market (demand growth, investment momentum, home market): raw rows -> this cell | not reproducible from shipped/pipeline data |  | 46 | the manifest names candidate raw files by script; row-level lineage from raw file to this cell is not recorded |
| prize: commons and option; large-yard signals: raw inputs | not reproducible from shipped/pipeline data |  |  | no raw_inputs_manifest.yml entry names the producing script in required_by: the raw source of this family is not recorded |
| Secure engine (Tests 1-5) and capability floor: raw inputs | not reproducible from shipped/pipeline data |  |  | no raw_inputs_manifest.yml entry names the producing script in required_by: the raw source of this family is not recorded |
| pathway (Catch Up anchors): raw inputs | not reproducible from shipped/pipeline data |  |  | no raw_inputs_manifest.yml entry names the producing script in required_by: the raw source of this family is not recorded |
| candidate raw inputs: vintage and licence recorded | provenance_gap | 64 | 13 | 51 of 64 lack a vintage or a licence in config/raw_inputs_manifest.yml: comtrade_energy_trade, comtrade_total_export, critmin_export, critmin_import, critmin_mirror_import, critmin_total_export, ilo_earnings_by_activity, imf_commodity_prices, imf_lending_rates, imf_ppi, wdi_country_info, wdi_gdp, consolidated_hs6_energy_tech_long, dual_use_scores, hs6_categories_with_essential, hts_codes_categories_bolstered_final, comtrade_vintage, bcg_market_size, cat_country_ratings, ei_stat_review, ei_stat_review_wide, ev_midstream_capacity, gcim_investment_capacity, geothermal_lcoe, hs92_country_product_year_4, hs92_country_product_year_6, iea_clean_tech_guide, iea_cleantech_midstream, iea_ev_outlook, iea_pams, iea_relative_costs, ipcc_ghg_intensity, solar_potential, uranium_production, uranium_resources, wb_wind_country, allied_comtrade_energy_data, comtrade_energy_dyads_all, wb_doingbusiness, wb_wdi, energy_hs6_master, imf_dip, oecd_crs_purpose, oci_energy_finance, oci_energy_finance_institutions, oci_energy_finance_crosswalk, oecd_crs_purpose_crosswalk, comtrade_monthly_dyads, comtrade_monthly_dyads_dictionary_extra, comtrade_monthly_dyads_windows, uspto_y02_patents |

## 2. Source rows

| item | value | detail |
|---|---|---|
| exposure_basis | blend |  |
| exposure_weight | 0.5 |  |
| import_partners_n | 16 |  |
| import_partners_total_usd | 395,716,496 |  |
| trade_benchmark_source | comtrade |  |
| trade_benchmark_period | 2022-2024 |  |
| trade_benchmark_exports_usd | 190,573,541 |  |
| trade_benchmark_share | 0.09051 |  |
| trade_benchmark_rca | 2.705 |  |

**import exposure**

| importer_iso | tech | supply_chain | observed | import_source | imports_usd | n_suppliers | top_supplier_iso3 | top_supplier_share | import_hhi | china_share | opposite_bloc_share | coercive_import_share | trusted_import_share | alignment_coverage_share | gdp_usd | imports_to_gdp | trade_year |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| JPN | Electric Vehicles | Refining | TRUE | importer | 395716496 | 16 | CHN | 0.4701801 | 0.3962394 | 0.4701801 | 0.8842079 | 0.8842079 | 0.04444206 | 1 | 4.190008e+12 | 9.444289e-05 | 2024 |

**current import exposure**

| importer_iso | tech | supply_chain | observed | import_source | imports_usd | n_suppliers | top_supplier_iso3 | top_supplier_share | import_hhi | china_share | opposite_bloc_share | coercive_import_share | trusted_import_share | alignment_coverage_share | gdp_usd | imports_to_gdp | window_end | months_used | window_start | current_basis |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| JPN | Electric Vehicles | Refining | TRUE | importer | 647699253 | 24 | CHN | 0.4737083 | 0.3961604 | 0.4737083 | 0.8858329 | 0.8858329 | 0.04536822 | 1 | 4.435163e+12 | 0.0001460373 | 202608 | 11 | 202509 | own_monthly |

**import partners top10**

| partner_iso | imports_usd | share |
|---|---|---|
| CHN | 186058036.1 | 0.470180137 |
| VNM | 163814545.9 | 0.413969464 |
| THA |  21804072.8 | 0.055100237 |
| ITA |   8942148.5 | 0.022597361 |
| PHL |   4294482.8 | 0.010852423 |
| EST |   2563337.9 | 0.006477713 |
| AUT |   2257001.6 | 0.005703582 |
| IND |   2135818.8 | 0.005397346 |
| KOR |   1889178.7 | 0.004774071 |
| DEU |    991647.4 | 0.002505954 |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| imports_usd <- import exposure (base/current blend) | reproduced | 521,707,875 | 521,707,875 | (1 - 0.5) x base 395,716,496 + 0.5 x current 647,699,253 |
| import_hhi <- import exposure (base/current blend) | reproduced | 0.3962 | 0.3962 | (1 - 0.5) x base 0.3962 + 0.5 x current 0.3962 |
| china_share <- import exposure (base/current blend) | reproduced | 0.4719 | 0.4719 | (1 - 0.5) x base 0.4702 + 0.5 x current 0.4737 |
| opposite_bloc_share <- import exposure (base/current blend) | reproduced | 0.885 | 0.885 | (1 - 0.5) x base 0.8842 + 0.5 x current 0.8858 |
| top_supplier_share <- import exposure (base/current blend) | reproduced | 0.4719 | 0.4719 | (1 - 0.5) x base 0.4702 + 0.5 x current 0.4737 |
| imports_to_gdp <- import exposure (base/current blend) | reproduced | 0.0001202 | 0.0001202 | (1 - 0.5) x base 9.444e-05 + 0.5 x current 0.000146 |
| top supplier <- import exposure | reproduced | CHN | CHN |  |
| partner rows: self-trade (re-import) imports, USD | reproduced | 0 | 0 | import_partner_cells_tbl.rds lists the importer as its own supplier |
| base import HHI <- partner rows without self-trade (sum of squared shares) | reproduced | 0.3962 | 0.3962 | import_partner_cells_tbl.rds against import_exposure_tbl.rds, base year |
| base imports_usd <- partner rows without self-trade | reproduced | 395,716,496 | 395,716,496 |  |
| base top supplier <- partner rows | reproduced | CHN | CHN |  |
| home_share (world supply share) <- production / supply structure | not reproducible from shipped/pipeline data |  | 0.02636 | the supply structure is assembled inside scripts/26 (Atlas trade shares, USGS / EI production, GCIM) and not saved per source; no production-table rows for this cell |
| trade benchmark <- Comtrade energy trade rows | not reproducible from shipped/pipeline data |  | 0.09051 | the 2022-24 benchmark is computed inside scripts/26 from the raw Comtrade pull; no processed table holds its rows |

## 3. Cleaning and exclusions

| item | value | detail |
|---|---|---|
| entrepot_or_registry | FALSE | exclude_iso3: HKG, MAC, MHL, LBR, PAN |
| sn_exclusion |  |  |
| posture_exclusion |  |  |
| thin_need_cell | FALSE |  |
| thin_reason |  |  |
| material_importers_in_cell | 38 | thin below 5 |
| import_material | TRUE | immaterial below USD 1,000,000 |
| self_supplied | FALSE | self-supply guard at home_share >= 0.1 |
| cp_thin_cell | FALSE |  |
| current_trade_flag |  |  |
| opposite_bloc_basis | bloc |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| entrepot rule -> need not ranked | reproduced | FALSE | FALSE |  |
| immaterial-importer rule (imports_usd vs min_import_usd) | reproduced | TRUE | TRUE | imports_usd 521,707,875 |
| self-supply guard (home_share >= self_supply_share) | reproduced | FALSE | FALSE | home_share 0.02636 |
| which countries enter the cell's percentiles | recorded |  | 203 | the ranked universe (es_contributions countries less exclude_iso3) is rebuilt only inside scripts/26 |

## 4. Transformations

| item | value | detail |
|---|---|---|
| percentile_rule | mid-rank (rank - 0.5) / n, ties averaged (sp_helpers.R sp_midrank_pct) |  |
| thresholds_inclusive | TRUE |  |

**recipes**

| component | variable | source | orient | rule |
|---|---|---|---|---|
| need: import_dependence | deficit_gdp | es_contributions | invert | within-cell mid-rank percentile (rank - 0.5) / n; family = mean of its measures |
| need: import_dependence | Production surplus/deficit | es_contributions | invert | within-cell mid-rank percentile (rank - 0.5) / n; family = mean of its measures |
| need: supplier_concentration | import_hhi | import_exposure | as_is | within-cell mid-rank percentile (rank - 0.5) / n; family = mean of its measures |
| need: risky_supplier_exposure | china_share | import_exposure | as_is | within-cell mid-rank percentile (rank - 0.5) / n; family = mean of its measures |
| need: risky_supplier_exposure | opposite_bloc_share | import_exposure | as_is | within-cell mid-rank percentile (rank - 0.5) / n; family = mean of its measures |
| position: export_position | market_share | EO Trade | fill na | pillar component value (within-cell index); family = mean of observed variables |
| position: export_position | rca | EO Trade | fill na | pillar component value (within-cell index); family = mean of observed variables |
| position: production_position | size | EO Production | fill structural_zero | pillar component value (within-cell index); family = mean of observed variables |
| position: manufacturing_capacity | Operating Capacity Index | EO Investment | fill structural_zero | pillar component value (within-cell index); family = mean of observed variables |
| position: investment_position | Annual Investment Index | EO Investment | fill structural_zero | pillar component value (within-cell index); family = mean of observed variables |
| prize | siv_score | industrial_opportunity; node attributes | as_is | weighted mean of market, commons, option; strategic_industrial_value = mid-rank percentile across all scored rows; market-only cap at prize_threshold - cap_margin |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| CP families in the table are defined in the configuration | reproduced |  | export_position, production_position, manufacturing_capacity, investment_position |  |
| measure-level percentiles (raw value -> measure percentile) | not reproducible from shipped/pipeline data |  |  | measure values other than import HHI, China and opposite-bloc shares are not stored per cell (index_outputs contributions) |

## 5. Normalized indicators

| item | value | detail |
|---|---|---|
| sn_import_dependence | 0.9495 |  |
| sn_supplier_concentration | 0.9039 |  |
| sn_risky_supplier_exposure | 0.9745 |  |
| sn_trusted_alternatives | 0.9975 |  |
| siv_market | 0.6352 |  |
| siv_commons | 0.8333 |  |
| siv_option | 0 |  |
| io_node_prize | 0.6352 |  |
| io_growth_pct | 0.7636 |  |
| io_investment_momentum_pct | 0.25 |  |
| io_home_market | 0.4066 |  |
| io_outlook | growing |  |
| io_growth_series | minerals:Electric vehicles |  |
| cp_export_position | 0.794 |  |
| cp_production_position | 0.7101 |  |
| cp_manufacturing_capacity | 0.2554 |  |
| cp_investment_position | 0 |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| sn_supplier_concentration <- percentile of import HHI in the cell | reproduced | 0.9039 | 0.9039 | import_hhi 0.3962 among 203 observed importers |
| sn_risky_supplier_exposure <- mean percentile of China and opposite-bloc shares | reproduced | 0.9745 | 0.9745 | china_share 0.4719, opposite_bloc_share 0.885; share floor 0.01 |
| sn_import_dependence <- deficit_gdp / production surplus percentiles | not reproducible from shipped/pipeline data |  | 0.9495 | its measures live in index_outputs.rds contributions, not in the cell table |
| sn_trusted_alternatives <- 1 - (trusted world share - own share), percentile | recorded |  | 0.9975 | trusted_world_share 0.04043, home_share 0.02636 |
| siv_market = industrial_opportunity | reproduced | 0.6352 | 0.6352 |  |
| industrial_opportunity <- node prize, lifted to mean(node, home market) where the home market is higher | reproduced | 0.6352 | 0.6352 |  |
| io_node_prize <- WEO / minerals demand growth and capex momentum | not reproducible from shipped/pipeline data |  | 0.6352 | node-level growth percentiles are ranked across the 55 producing nodes inside scripts/26 |
| CP family values <- pillar variable contributions | not reproducible from shipped/pipeline data |  |  | variable contributions live in index_outputs.rds, not in the cell table |

## 6. Weights

| item | value | detail |
|---|---|---|
| need_families | equal over observed families (4; trusted alternatives in the score: TRUE) |  |
| cp_families | equal over observed families (4 of 4 applicable) |  |
| export_position_momentum_weight | 0.15 |  |
| siv_weights | market 1, commons 1, option 1 |  |
| io_investment_momentum_weight | 0.25 |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| weights | recorded |  |  | read from config/strategic_position.yml; no sensitivity of the SIV weights exists |

## 7. Intermediate scores

| item | value | detail |
|---|---|---|
| supply_chain_need_score | 0.9564 |  |
| supply_chain_need_uncapped | 0.9975 |  |
| supply_chain_need | 0.9975 |  |
| supply_chain_need_cap |  |  |
| siv_score | 0.4895 |  |
| strategic_industrial_value | 0.8422 |  |
| siv_cap |  |  |
| large_yard_case | FALSE |  |
| large_yard_n | 2 |  |
| large_yard_signals | mature;passive;standardized;low_differentiation |  |
| large_yard_unknown |  |  |
| pathway_credible | yes |  |
| pathway_basis | direct_supply_share;direct_export_share;feasibility;adjacent_stage_relative;learning |  |
| catch_up_anchor | TRUE |  |
| pathway_anchors_fired | direct_supply_share;direct_export_share |  |
| pathway_support_fired | feasibility;adjacent_stage_relative;learning |  |
| competitive_position_score | 0.4399 |  |
| competitive_position_percentile | 0.9521 |  |
| cp_contender_pct | 0.3182 |  |
| cp_contender_basis | contenders |  |
| cp_n_contenders | 11 |  |
| cp_is_contender | TRUE |  |
| home_share_measured | 0.01819 |  |
| t3_capability_lo | strong |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| need score = mean of observed need families (self-supply guard applied) | reproduced | 0.9564 | 0.9564 |  |
| need percentile = within-cell mid-rank of the need score | reproduced | 0.9975 | 0.9975 | 203 scored economies in the cell |
| need-floor cap (score below score_floor with percentile >= need - band) | reproduced | 0.9975 | 0.9975 | score_floor 0.1: not capped |
| siv_score = weighted mean of market, commons, option | reproduced | 0.4895 | 0.4895 |  |
| strategic_industrial_value = mid-rank among all scored rows, market-only cap | reproduced | 0.8422 | 0.8422 | 12045 scored rows; gate 0.5 |
| siv_cap | reproduced |  |  | both absent |
| large_yard_n = commodity signals holding (required signals excluded) | reproduced | 2 | 2 |  |
| large-yard signals <- node attributes and world_top_share | recorded |  | mature;passive;standardized;low_differentiation | recomputed for every cell by scripts/qc/run_sensitivity.R (sp_large_yard_case reproduction check) |
| pathway_credible <- anchor rule re-thresholded from the stored pw_* signals | reproduced | yes | yes | rule anchor; 4 evidence families fired of 7 observed |
| pw_* signals <- GCIM pipeline, capacity and input-cost side tables | not reproducible from shipped/pipeline data |  |  | sp_pathway() builds the signals from side tables scripts/26 does not save; only the thresholding is re-run |
| CP score = mean of observed CP families | reproduced | 0.4399 | 0.4399 | families: export_position, production_position, manufacturing_capacity, investment_position |
| cp_contender_pct = CP score placed among the node's contenders | reproduced | 0.3182 | 0.3182 | 11 contenders (measured share >= 0.005) |
| capability lower bound (Secure engine Test 3) | not reproducible from shipped/pipeline data |  | strong | the posture engine is not rerun by the audit (its inputs are in strategic_position_inputs.rds) |

## 8. Threshold decisions

| item | value | detail |
|---|---|---|
| decision_step | secure_override |  |
| borderline |  |  |

**steps**

| step | test | value | threshold | margin | result | note |
|---|---|---|---|---|---|---|
| secure_engine | t1_consequence | yes |  |  | yes | posture engine test as stored (not rerun) |
| secure_engine | t2_vulnerability | yes |  |  | yes | posture engine test as stored (not rerun) |
| secure_engine | t2_path | physical |  |  | physical | posture engine test as stored (not rerun) |
| secure_engine | t3_capability | strong |  |  | strong | posture engine test as stored (not rerun) |
| secure_engine | t3_position | no |  |  | no | posture engine test as stored (not rerun) |
| secure_engine | t4_strategy | no |  |  | no | posture engine test as stored (not rerun) |
| secure_engine | t5_proportionality | pass |  |  | pass | posture engine test as stored (not rerun) |
| secure | secure_override (posture engine) | secure | secure |  | yes | coercive_chokepoint |
| position | cp_contender_pct | 0.3182 | 0.75 | -0.4318 | not_top_quarter | contender TRUE, capability lower bound strong (floor moderate/strong) |

## 9. Final classification

| item | value | detail |
|---|---|---|
| strategic_choice | secure |  |
| choice_label | Secure |  |
| strategic_posture | secure |  |
| compete_mode |  |  |
| deploy_mode |  |  |
| decision_step | secure_override |  |
| posture_variant | secure_input |  |
| posture_confidence | moderate |  |
| choice_possible | secure |  |
| pivotal_inputs |  |  |
| consistency_flag |  |  |
| robustness_class | not_testable |  |
| robustness_local_flips |  |  |
| robustness_structural_flips |  |  |
| robustness_fence_change | FALSE |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| choice = canonical rule rerun from the stored inputs | reproduced | secure | secure |  |

## 10. Resulting recommendation

| item | value | detail |
|---|---|---|
| choice | secure |  |
| choice_label | Secure |  |
| choice_action | Protect what cannot be allowed to fail |  |
| yard | small |  |
| yard_and_fence | Small yard / High fence |  |
| fence | High fence |  |
| yard_rollup | small |  |
| fence_level | high |  |
| fence_posture | A high, targeted fence around the input: limit a coercive supplier's leverage over it in critical uses and over trusted capacity, phased in so supply is not cut before trusted alternatives qualify. |  |
| gate_level | selective |  |
| build_posture | Repair the chokepoint until a prolonged cutoff could be survived: qualified trusted supply (domestic or allied), stockpiles, surge and redundancy. The goal is mitigation, not maximum localisation. Delivery (mixed): No source holds enough trusted supply to repair the vulnerability alone: create new trusted capacity at home (where a foothold exists or can be built) and with allied partners through co-investment and offtake, and bridge the years until it qualifies with stockpiles. |  |
| benchmark | Critical production could continue through a prolonged cutoff on inventories plus qualified trusted supply. |  |
| success_metrics | Months critical production could continue through a cutoff (inventories plus qualified alternative supply); Qualified trusted capacity as a share of critical demand, and how fast it can surge; Share of supply held by the largest potentially coercive supplier; Liquidity and survival of sub-tier suppliers through a price war or cutoff |  |
| exit_condition | Reclassify to Compete or Deploy when a prolonged cutoff could be survived (trusted supply, stockpiles and surge cover the restore time) or the largest coercive supplier falls below the screening line. Announced capacity does not count until it is qualified. |  |
| review_interval_years | 2 |  |
| reason | An acute vulnerability (coercive chokepoint): Secure, whatever the prize. |  |
| recommended_partners | USA, AUS |  |
| policy_alignment | mixed |  |
| policy_alignment_label | Mixed signals |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| yard = yard of the choice (yard_rollup.R) | reproduced | small | small |  |
| policy-design text <- config/posture_policy_design.csv by posture variant | recorded |  | secure_input |  |
| policy alignment | recorded |  | mixed | after the choice; never an input to it |

## Shipped bundle comparison

Posture bundle created 2026-11-17T20:33:31Z (git 0496e7ba), detail file posture/posture-detail-JPN.bccf1e26.json. 0 of 44 fields differ from the pipeline or are missing on one side. Note: the bundle's import_dependence, import_hhi and china_bloc are the need-family percentiles, not raw shares.

| field | pipeline | bundle | status |
|---|---|---|---|
| choice (core) | secure | secure | same |
| yard roll-up (core yd) | S | S | same |
| posture | secure | secure | same |
| variant | secure_input | secure_input | same |
| confidence | moderate | moderate | same |
| need | 0.9975 | 0.998 | same |
| prize.value | 0.8422 | 0.842 | same |
| prize.market | 0.6352 | 0.635 | same |
| prize.node_prize | 0.6352 | 0.635 | same |
| prize.outlook | growing | growing | same |
| prize.measure | strategic_value | strategic_value | same |
| prize.investment_momentum | 0.25 | 0.25 | same |
| prize.home_market | 0.4066 | 0.407 | same |
| cp | 0.3182 | 0.318 | same |
| home_share | 0.01819 | 0.0182 | same |
| pathway_credible | yes | yes | same |
| pathway_basis | direct_supply_share;direct_export_share;feasibility;adjacent_stage_relative;learning | direct_supply_share;direct_export_share;feasibility;adjacent_stage_relative;learning | same |
| reason | An acute vulnerability (coercive chokepoint): Secure, whatever the prize. | An acute vulnerability (coercive chokepoint): Secure, whatever the prize. | same |
| policy.alignment | mixed | mixed | same |
| partners[1].iso3 | USA | USA | same |
| import_dependence (sn_import_dependence, a percentile) | 0.9495 | 0.95 | same |
| import_hhi (sn_supplier_concentration, a percentile) | 0.9039 | 0.904 | same |
| china_bloc (sn_risky_supplier_exposure, a percentile) | 0.9745 | 0.974 | same |
| trusted_alternatives | 0.9975 | 0.998 | same |
| top_supplier | CHN | CHN | same |
| top_supplier_share | 0.4719 | 0.472 | same |
| china_share | 0.4719 | 0.472 | same |
| opposite_bloc_share | 0.885 | 0.885 | same |
| imports_to_gdp | 0.0001202 | 0.00012 | same |
| trusted_share | 0.04043 | 0.0404 | same |
| coercive_top | CHN | CHN | same |
| coercive_top_share | 0.944 | 0.944 | same |
| coercive_import_share | 0.885 | 0.885 | same |
| trade.benchmark_share | 0.09051 | 0.0905 | same |
| trade.benchmark_rca | 2.705 | 2.71 | same |
| trade.momentum_score | 0.3421 | 0.342 | same |
| trade.benchmark_exports | 190,573,541 | 191,000,000 | same |
| current_trade.weight | 0.5 | 0.5 | same |
| current_trade.china_share_base | 0.4702 | 0.47 | same |
| current_trade.china_share_latest | 0.4737 | 0.474 | same |
| cp_families.export_position | 0.794 | 0.794 | same |
| cp_families.production_position | 0.7101 | 0.71 | same |
| cp_families.manufacturing_capacity | 0.2554 | 0.255 | same |
| cp_families.investment_position | 0 | 0 | same |

## Gaps: links not reproduced

- 1. need: import dependence: raw rows -> this cell: not reproducible from shipped/pipeline data (the manifest names candidate raw files by script; row-level lineage from raw file to this cell is not recorded)
- 1. need: supplier concentration and risky suppliers: raw rows -> this cell: not reproducible from shipped/pipeline data (the manifest names candidate raw files by script; row-level lineage from raw file to this cell is not recorded)
- 1. need: current-trade blend: raw rows -> this cell: not reproducible from shipped/pipeline data (the manifest names candidate raw files by script; row-level lineage from raw file to this cell is not recorded)
- 1. need: trusted alternatives and world supply structure: raw rows -> this cell: not reproducible from shipped/pipeline data (the manifest names candidate raw files by script; row-level lineage from raw file to this cell is not recorded)
- 1. position: CP families: raw rows -> this cell: not reproducible from shipped/pipeline data (the manifest names candidate raw files by script; row-level lineage from raw file to this cell is not recorded)
- 1. position: trade benchmark and momentum: raw inputs: not reproducible from shipped/pipeline data (no raw_inputs_manifest.yml entry names the producing script in required_by: the raw source of this family is not recorded)
- 1. prize: market (demand growth, investment momentum, home market): raw rows -> this cell: not reproducible from shipped/pipeline data (the manifest names candidate raw files by script; row-level lineage from raw file to this cell is not recorded)
- 1. prize: commons and option; large-yard signals: raw inputs: not reproducible from shipped/pipeline data (no raw_inputs_manifest.yml entry names the producing script in required_by: the raw source of this family is not recorded)
- 1. Secure engine (Tests 1-5) and capability floor: raw inputs: not reproducible from shipped/pipeline data (no raw_inputs_manifest.yml entry names the producing script in required_by: the raw source of this family is not recorded)
- 1. pathway (Catch Up anchors): raw inputs: not reproducible from shipped/pipeline data (no raw_inputs_manifest.yml entry names the producing script in required_by: the raw source of this family is not recorded)
- 1. candidate raw inputs: vintage and licence recorded: provenance_gap (51 of 64 lack a vintage or a licence in config/raw_inputs_manifest.yml: comtrade_energy_trade, comtrade_total_export, critmin_export, critmin_import, critmin_mirror_import, critmin_total_export, ilo_earnings_by_activity, imf_commodity_prices, imf_lending_rates, imf_ppi, wdi_country_info, wdi_gdp, consolidated_hs6_energy_tech_long, dual_use_scores, hs6_categories_with_essential, hts_codes_categories_bolstered_final, comtrade_vintage, bcg_market_size, cat_country_ratings, ei_stat_review, ei_stat_review_wide, ev_midstream_capacity, gcim_investment_capacity, geothermal_lcoe, hs92_country_product_year_4, hs92_country_product_year_6, iea_clean_tech_guide, iea_cleantech_midstream, iea_ev_outlook, iea_pams, iea_relative_costs, ipcc_ghg_intensity, solar_potential, uranium_production, uranium_resources, wb_wind_country, allied_comtrade_energy_data, comtrade_energy_dyads_all, wb_doingbusiness, wb_wdi, energy_hs6_master, imf_dip, oecd_crs_purpose, oci_energy_finance, oci_energy_finance_institutions, oci_energy_finance_crosswalk, oecd_crs_purpose_crosswalk, comtrade_monthly_dyads, comtrade_monthly_dyads_dictionary_extra, comtrade_monthly_dyads_windows, uspto_y02_patents)
- 2. home_share (world supply share) <- production / supply structure: not reproducible from shipped/pipeline data (the supply structure is assembled inside scripts/26 (Atlas trade shares, USGS / EI production, GCIM) and not saved per source; no production-table rows for this cell)
- 2. trade benchmark <- Comtrade energy trade rows: not reproducible from shipped/pipeline data (the 2022-24 benchmark is computed inside scripts/26 from the raw Comtrade pull; no processed table holds its rows)
- 4. measure-level percentiles (raw value -> measure percentile): not reproducible from shipped/pipeline data (measure values other than import HHI, China and opposite-bloc shares are not stored per cell (index_outputs contributions))
- 5. sn_import_dependence <- deficit_gdp / production surplus percentiles: not reproducible from shipped/pipeline data (its measures live in index_outputs.rds contributions, not in the cell table)
- 5. io_node_prize <- WEO / minerals demand growth and capex momentum: not reproducible from shipped/pipeline data (node-level growth percentiles are ranked across the 55 producing nodes inside scripts/26)
- 5. CP family values <- pillar variable contributions: not reproducible from shipped/pipeline data (variable contributions live in index_outputs.rds, not in the cell table)
- 7. pw_* signals <- GCIM pipeline, capacity and input-cost side tables: not reproducible from shipped/pipeline data (sp_pathway() builds the signals from side tables scripts/26 does not save; only the thresholding is re-run)
- 7. capability lower bound (Secure engine Test 3): not reproducible from shipped/pipeline data (the posture engine is not rerun by the audit (its inputs are in strategic_position_inputs.rds))

