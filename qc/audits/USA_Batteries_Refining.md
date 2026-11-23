# Case audit: CASE_004 United States / Batteries / Refining

Generated 2026-11-20T00:00:00Z by scripts/qc/build_case_audit.R from <processed-dir>/outputs/strategic_position_tbl.rds (pipeline-5882f35f044e) and config/strategic_position.yml. Shipped posture bundle: created 2026-11-17T20:33:31Z, git 0496e7ba.

**Choice (pipeline): catch_up. Choice (shipped bundle): catch_up.** 67 links checked: 42 reproduced, 0 differ, 18 not reproducible from shipped/pipeline data, 1 provenance gaps, 6 recorded only. Bundle fields differing or missing: 0.

Case: United States: battery-material refining and processing

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
| import_partners_n | 77 |  |
| import_partners_total_usd | 5,784,007,058 |  |
| trade_benchmark_source | comtrade |  |
| trade_benchmark_period | 2022-2024 |  |
| trade_benchmark_exports_usd | 2,820,145,000 |  |
| trade_benchmark_share | 0.034 |  |
| trade_benchmark_rca | 0.3581 |  |

**import exposure**

| importer_iso | tech | supply_chain | observed | import_source | imports_usd | n_suppliers | top_supplier_iso3 | top_supplier_share | import_hhi | china_share | opposite_bloc_share | coercive_import_share | trusted_import_share | alignment_coverage_share | gdp_usd | imports_to_gdp | trade_year |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| USA | Batteries | Refining | TRUE | importer | 5784007058 | 77 | CAN | 0.1734461 | 0.07387901 | 0.1350302 | 0.148269 | 0.148269 | 0.6168079 | 1 | 2.929801e+13 | 0.0001974198 | 2024 |

**current import exposure**

| importer_iso | tech | supply_chain | observed | import_source | imports_usd | n_suppliers | top_supplier_iso3 | top_supplier_share | import_hhi | china_share | opposite_bloc_share | coercive_import_share | trusted_import_share | alignment_coverage_share | gdp_usd | imports_to_gdp | window_end | months_used | window_start | current_basis |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| USA | Batteries | Refining | TRUE | importer | 6456715866 | 78 | CAN | 0.2131886 | 0.08183868 | 0.07189615 | 0.1560004 | 0.1560004 | 0.6266699 | 1 | 3.07697e+13 | 0.0002098401 | 202608 | 12 | 202509 | own_monthly |

**import partners top10**

| partner_iso | imports_usd | share |
|---|---|---|
| CAN | 1003213232 | 0.17344606 |
| CHN |  779956308 | 0.13484705 |
| JPN |  432789946 | 0.07482528 |
| DEU |  412200019 | 0.07126548 |
| KOR |  322654909 | 0.05578398 |
| NOR |  312537086 | 0.05403470 |
| ITA |  185383121 | 0.03205098 |
| FIN |  174428072 | 0.03015696 |
| IND |  174314559 | 0.03013734 |
| AUS |  171126443 | 0.02958614 |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| imports_usd <- import exposure (base/current blend) | reproduced | 6,120,361,462 | 6,120,361,462 | (1 - 0.5) x base 5,784,007,058 + 0.5 x current 6,456,715,866 |
| import_hhi <- import exposure (base/current blend) | reproduced | 0.07786 | 0.07786 | (1 - 0.5) x base 0.07388 + 0.5 x current 0.08184 |
| china_share <- import exposure (base/current blend) | reproduced | 0.1035 | 0.1035 | (1 - 0.5) x base 0.135 + 0.5 x current 0.0719 |
| opposite_bloc_share <- import exposure (base/current blend) | reproduced | 0.1521 | 0.1521 | (1 - 0.5) x base 0.1483 + 0.5 x current 0.156 |
| top_supplier_share <- import exposure (base/current blend) | reproduced | 0.1933 | 0.1933 | (1 - 0.5) x base 0.1734 + 0.5 x current 0.2132 |
| imports_to_gdp <- import exposure (base/current blend) | reproduced | 0.0002036 | 0.0002036 | (1 - 0.5) x base 0.0001974 + 0.5 x current 0.0002098 |
| top supplier <- import exposure | reproduced | CAN | CAN |  |
| partner rows: self-trade (re-import) imports, USD | reproduced | 0 | 0 | import_partner_cells_tbl.rds lists the importer as its own supplier |
| base import HHI <- partner rows without self-trade (sum of squared shares) | reproduced | 0.07388 | 0.07388 | import_partner_cells_tbl.rds against import_exposure_tbl.rds, base year |
| base imports_usd <- partner rows without self-trade | reproduced | 5,784,007,058 | 5,784,007,058 |  |
| base top supplier <- partner rows | reproduced | CAN | CAN |  |
| home_share (world supply share) <- production / supply structure | not reproducible from shipped/pipeline data |  | 0.005104 | the supply structure is assembled inside scripts/26 (Atlas trade shares, USGS / EI production, GCIM) and not saved per source; no production-table rows for this cell |
| trade benchmark <- Comtrade energy trade rows | not reproducible from shipped/pipeline data |  | 0.034 | the 2022-24 benchmark is computed inside scripts/26 from the raw Comtrade pull; no processed table holds its rows |

## 3. Cleaning and exclusions

| item | value | detail |
|---|---|---|
| entrepot_or_registry | FALSE | exclude_iso3: HKG, MAC, MHL, LBR, PAN |
| sn_exclusion |  |  |
| posture_exclusion |  |  |
| thin_need_cell | FALSE |  |
| thin_reason |  |  |
| material_importers_in_cell | 143 | thin below 5 |
| import_material | TRUE | immaterial below USD 1,000,000 |
| self_supplied | FALSE | self-supply guard at home_share >= 0.1 |
| cp_thin_cell | FALSE |  |
| current_trade_flag |  |  |
| opposite_bloc_basis | bloc |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| entrepot rule -> need not ranked | reproduced | FALSE | FALSE |  |
| immaterial-importer rule (imports_usd vs min_import_usd) | reproduced | TRUE | TRUE | imports_usd 6,120,361,462 |
| self-supply guard (home_share >= self_supply_share) | reproduced | FALSE | FALSE | home_share 0.005104 |
| which countries enter the cell's percentiles | recorded |  | 204 | the ranked universe (es_contributions countries less exclude_iso3) is rebuilt only inside scripts/26 |

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
| sn_import_dependence | 0.4369 |  |
| sn_supplier_concentration | 0.3177 |  |
| sn_risky_supplier_exposure | 0.5201 |  |
| sn_trusted_alternatives | 0.7034 |  |
| siv_market | 0.7808 |  |
| siv_commons | 0.6667 |  |
| siv_option | 0 |  |
| io_node_prize | 0.633 |  |
| io_growth_pct | 0.7273 |  |
| io_investment_momentum_pct | 0.35 |  |
| io_home_market | 0.9286 |  |
| io_outlook | growing |  |
| io_growth_series | minerals:Electric vehicles\|Grid battery storage |  |
| cp_export_position | 0.6614 |  |
| cp_production_position | 0.5542 |  |
| cp_manufacturing_capacity | 0.1907 |  |
| cp_investment_position | 0.7597 |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| sn_supplier_concentration <- percentile of import HHI in the cell | reproduced | 0.3177 | 0.3177 | import_hhi 0.07786 among 203 observed importers |
| sn_risky_supplier_exposure <- mean percentile of China and opposite-bloc shares | reproduced | 0.5201 | 0.5201 | china_share 0.1035, opposite_bloc_share 0.1521; share floor 0.01 |
| sn_import_dependence <- deficit_gdp / production surplus percentiles | not reproducible from shipped/pipeline data |  | 0.4369 | its measures live in index_outputs.rds contributions, not in the cell table |
| sn_trusted_alternatives <- 1 - (trusted world share - own share), percentile | recorded |  | 0.7034 | trusted_world_share 0.04043, home_share 0.005104 |
| siv_market = industrial_opportunity | reproduced | 0.7808 | 0.7808 |  |
| industrial_opportunity <- node prize, lifted to mean(node, home market) where the home market is higher | reproduced | 0.7808 | 0.7808 |  |
| io_node_prize <- WEO / minerals demand growth and capex momentum | not reproducible from shipped/pipeline data |  | 0.633 | node-level growth percentiles are ranked across the 55 producing nodes inside scripts/26 |
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
| supply_chain_need_score | 0.4945 |  |
| supply_chain_need_uncapped | 0.4534 |  |
| supply_chain_need | 0.4534 |  |
| supply_chain_need_cap |  |  |
| siv_score | 0.4825 |  |
| strategic_industrial_value | 0.8336 |  |
| siv_cap |  |  |
| large_yard_case | FALSE |  |
| large_yard_n | 0 |  |
| large_yard_signals | passive |  |
| large_yard_unknown |  |  |
| pathway_credible | yes |  |
| pathway_basis | direct_supply_share;direct_export_share;operating_capacity;committed_pipeline;adjacent_stage_share;feasibility;adjacent_stage_relative;learning;announced_pipeline |  |
| catch_up_anchor | TRUE |  |
| pathway_anchors_fired | direct_supply_share;direct_export_share;operating_capacity;committed_pipeline;adjacent_stage_share |  |
| pathway_support_fired | feasibility;adjacent_stage_relative;learning;announced_pipeline |  |
| competitive_position_score | 0.5415 |  |
| competitive_position_percentile | 0.9656 |  |
| cp_contender_pct | 0.3125 |  |
| cp_contender_basis | contenders |  |
| cp_n_contenders | 8 |  |
| cp_is_contender | TRUE |  |
| home_share_measured | 0.005104 |  |
| t3_capability_lo | strong |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| need score = mean of observed need families (self-supply guard applied) | reproduced | 0.4945 | 0.4945 |  |
| need percentile = within-cell mid-rank of the need score | reproduced | 0.4534 | 0.4534 | 204 scored economies in the cell |
| need-floor cap (score below score_floor with percentile >= need - band) | reproduced | 0.4534 | 0.4534 | score_floor 0.1: not capped |
| siv_score = weighted mean of market, commons, option | reproduced | 0.4825 | 0.4825 |  |
| strategic_industrial_value = mid-rank among all scored rows, market-only cap | reproduced | 0.8336 | 0.8336 | 12045 scored rows; gate 0.5 |
| siv_cap | reproduced |  |  | both absent |
| large_yard_n = commodity signals holding (required signals excluded) | reproduced | 0 | 0 |  |
| large-yard signals <- node attributes and world_top_share | recorded |  | passive | recomputed for every cell by scripts/qc/run_sensitivity.R (sp_large_yard_case reproduction check) |
| pathway_credible <- anchor rule re-thresholded from the stored pw_* signals | reproduced | yes | yes | rule anchor; 6 evidence families fired of 7 observed |
| pw_* signals <- GCIM pipeline, capacity and input-cost side tables | not reproducible from shipped/pipeline data |  |  | sp_pathway() builds the signals from side tables scripts/26 does not save; only the thresholding is re-run |
| CP score = mean of observed CP families | reproduced | 0.5415 | 0.5415 | families: export_position, production_position, manufacturing_capacity, investment_position |
| cp_contender_pct = CP score placed among the node's contenders | reproduced | 0.3125 | 0.3125 | 8 contenders (measured share >= 0.005) |
| capability lower bound (Secure engine Test 3) | not reproducible from shipped/pipeline data |  | strong | the posture engine is not rerun by the audit (its inputs are in strategic_position_inputs.rds) |

## 8. Threshold decisions

| item | value | detail |
|---|---|---|
| decision_step | triage |  |
| borderline |  |  |

**steps**

| step | test | value | threshold | margin | result | note |
|---|---|---|---|---|---|---|
| secure_engine | t1_consequence | no |  |  | no | posture engine test as stored (not rerun) |
| secure_engine | t2_vulnerability | no |  |  | no | posture engine test as stored (not rerun) |
| secure_engine | t2_path | none |  |  | none | posture engine test as stored (not rerun) |
| secure_engine | t3_capability | strong |  |  | strong | posture engine test as stored (not rerun) |
| secure_engine | t3_position | no |  |  | no | posture engine test as stored (not rerun) |
| secure_engine | t4_strategy | adjacent_capability |  |  | adjacent_capability | posture engine test as stored (not rerun) |
| secure_engine | t5_proportionality | pass |  |  | pass | posture engine test as stored (not rerun) |
| secure | secure_override (posture engine) | compete | secure |  | no |  |
| large_yard | large_yard_case | 0 | 5 | -5 | no | Not a large yard: strong industrial learning (2.0 of 3: process knowledge yes, workforce and suppliers partly, adjacent innovation partly) keeps a Medium-yard rationale. |
| need | supply_chain_need | 0.4534 | 0.67 | -0.2166 | low |  |
| prize | strategic_industrial_value | 0.8336 | 0.6 | 0.2336 | high |  |
| pathway | pathway_credible | yes | yes |  | yes | read only for a high prize without a high position |
| position | cp_contender_pct | 0.3125 | 0.75 | -0.4375 | not_top_quarter | contender TRUE, capability lower bound strong (floor moderate/strong) |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| trace large_yard: value | reproduced | 0 | 0 | stored trace rounds to 3 decimals |
| trace large_yard: threshold | reproduced | 5 | 5 |  |
| trace need: value | reproduced | 0.4534 | 0.453 | stored trace rounds to 3 decimals |
| trace need: threshold | reproduced | 0.67 | 0.67 |  |
| trace need: result | reproduced | FALSE | FALSE | stored low |
| trace prize: value | reproduced | 0.8336 | 0.834 | stored trace rounds to 3 decimals |
| trace prize: threshold | reproduced | 0.6 | 0.6 |  |
| trace prize: result | reproduced | TRUE | TRUE | stored high |
| trace position: value | reproduced | 0.3125 | 0.312 | stored trace rounds to 3 decimals |
| trace position: threshold | reproduced | 0.75 | 0.75 |  |
| trace position: result | reproduced | FALSE | FALSE | stored not_top_quarter |

## 9. Final classification

| item | value | detail |
|---|---|---|
| strategic_choice | catch_up |  |
| choice_label | Catch Up |  |
| strategic_posture | compete |  |
| compete_mode | catch_up |  |
| deploy_mode |  |  |
| decision_step | triage |  |
| posture_variant | compete_catch_up |  |
| posture_confidence | high |  |
| choice_possible | catch_up |  |
| pivotal_inputs |  |  |
| consistency_flag |  |  |
| robustness_class | stable |  |
| robustness_local_flips |  |  |
| robustness_structural_flips |  |  |
| robustness_fence_change | FALSE |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| choice = canonical rule rerun from the stored inputs | reproduced | catch_up | catch_up |  |

## 10. Resulting recommendation

| item | value | detail |
|---|---|---|
| choice | catch_up |  |
| choice_label | Catch Up |  |
| choice_action | Build from a credible industrial base |  |
| yard | medium |  |
| yard_and_fence | Medium yard / medium, permeable fence |  |
| fence | Medium fence |  |
| yard_rollup | medium |  |
| fence_level | moderate |  |
| fence_posture | Calibrated, temporary protection for the learning base, with the gate left open to the frontier. |  |
| gate_level | open |  |
| build_posture | Close the gap to the frontier: time-limited production incentives, first-of-a-kind finance, workforce and supplier development and demand creation, all tied to benchmarks. Delivery (domestic): Build at home: production support, finance, workforce and supplier development for the domestic industry, tied to benchmarks. |  |
| benchmark | The cost and quality gap to the frontier narrows on a stated schedule while support per unit falls. |  |
| success_metrics | Yields, costs and quality relative to the global frontier; Domestic engineers able to diagnose and modify the process without outside assistance; Domestic suppliers qualifying for more demanding parts of the value chain; Process improvements and successor-technology R&D generated domestically; Public support per unit falling as competitiveness improves |  |
| exit_condition | Support sunsets on the stated schedule unless the gap is narrowing. Move to Deploy if benchmarks are missed at successive reviews or the capability case fails, and re-test for Secure if a control layer or coercive chokepoint is identified. |  |
| review_interval_years | 3 |  |
| reason | a high strategic industrial value (0.83; market 0.78, commons 0.67, option 0.00), a position below the top quarter (0.31) and a credible pathway (capability anchor: direct supply share, direct export share, operating capacity, committed pipeline, adjacent stage share; corroborated by feasibility, adjacent stage relative, learning, announced pipeline): Catch Up. |  |
| policy_alignment | mixed |  |
| policy_alignment_label | Mixed signals |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| yard = yard of the choice (yard_rollup.R) | reproduced | medium | medium |  |
| policy-design text <- config/posture_policy_design.csv by posture variant | recorded |  | compete_catch_up |  |
| policy alignment | recorded |  | mixed | after the choice; never an input to it |

## Shipped bundle comparison

Posture bundle created 2026-11-17T20:33:31Z (git 0496e7ba), detail file posture/posture-detail-USA.b26e3b4b.json. 0 of 44 fields differ from the pipeline or are missing on one side. Note: the bundle's import_dependence, import_hhi and china_bloc are the need-family percentiles, not raw shares.

| field | pipeline | bundle | status |
|---|---|---|---|
| choice (core) | catch_up | catch_up | same |
| yard roll-up (core yd) | M | M | same |
| posture | compete | compete | same |
| variant | compete_catch_up | compete_catch_up | same |
| mode | catch_up | catch_up | same |
| confidence | high | high | same |
| need | 0.4534 | 0.453 | same |
| prize.value | 0.8336 | 0.834 | same |
| prize.market | 0.7808 | 0.781 | same |
| prize.node_prize | 0.633 | 0.633 | same |
| prize.outlook | growing | growing | same |
| prize.measure | strategic_value | strategic_value | same |
| prize.investment_momentum | 0.35 | 0.35 | same |
| prize.home_market | 0.9286 | 0.929 | same |
| cp | 0.3125 | 0.312 | same |
| home_share | 0.005104 | 0.0051 | same |
| pathway_credible | yes | yes | same |
| pathway_basis | direct_supply_share;direct_export_share;operating_capacity;committed_pipeline;adjacent_stage_share;feasibility;adjacent_stage_relative;learning;announced_pipeline | direct_supply_share;direct_export_share;operating_capacity;committed_pipeline;adjacent_stage_share;feasibility;adjacent_stage_relative;learning;announced_pipeline | same |
| reason | a high strategic industrial value (0.83; market 0.78, commons 0.67, option 0.00), a position below the top quarter (0.31) and a credible pathway (capability anchor: direct supply share, direct export share, operating capacity, committed pipeline, adjacent stage share; corroborated by feasibility, adjacent stage relative, learning, announced pipeline): Catch Up. | a high strategic industrial value (0.83; market 0.78, commons 0.67, option 0.00), a position below the top quarter (0.31) and a credible pathway (capability anchor: direct supply share, direct export share, operating capacity, committed pipeline, adjacent stage share; corroborated by feasibility, adjacent stage relative, learning, announced pipeline): Catch Up. | same |
| policy.alignment | mixed | mixed | same |
| import_dependence (sn_import_dependence, a percentile) | 0.4369 | 0.437 | same |
| import_hhi (sn_supplier_concentration, a percentile) | 0.3177 | 0.318 | same |
| china_bloc (sn_risky_supplier_exposure, a percentile) | 0.5201 | 0.52 | same |
| trusted_alternatives | 0.7034 | 0.703 | same |
| top_supplier | CAN | CAN | same |
| top_supplier_share | 0.1933 | 0.193 | same |
| china_share | 0.1035 | 0.103 | same |
| opposite_bloc_share | 0.1521 | 0.152 | same |
| imports_to_gdp | 0.0002036 | 0.000204 | same |
| trusted_share | 0.04043 | 0.0404 | same |
| coercive_top | CHN | CHN | same |
| coercive_top_share | 0.944 | 0.944 | same |
| coercive_import_share | 0.1521 | 0.152 | same |
| trade.benchmark_share | 0.034 | 0.034 | same |
| trade.benchmark_rca | 0.3581 | 0.358 | same |
| trade.momentum_score | 0.2763 | 0.276 | same |
| trade.benchmark_exports | 2,820,145,000 | 2,820,000,000 | same |
| current_trade.weight | 0.5 | 0.5 | same |
| current_trade.china_share_base | 0.135 | 0.135 | same |
| current_trade.china_share_latest | 0.0719 | 0.0719 | same |
| cp_families.export_position | 0.6614 | 0.661 | same |
| cp_families.production_position | 0.5542 | 0.554 | same |
| cp_families.manufacturing_capacity | 0.1907 | 0.191 | same |
| cp_families.investment_position | 0.7597 | 0.76 | same |

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

