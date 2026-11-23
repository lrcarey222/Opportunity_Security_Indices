# Case audit: CASE_001 South Korea / Batteries / Components

Generated 2026-11-20T00:00:00Z by scripts/qc/build_case_audit.R from <processed-dir>/outputs/strategic_position_tbl.rds (pipeline-5882f35f044e) and config/strategic_position.yml. Shipped posture bundle: created 2026-11-17T20:33:31Z, git 0496e7ba.

**Choice (pipeline): preserve. Choice (shipped bundle): preserve.** 67 links checked: 42 reproduced, 0 differ, 18 not reproducible from shipped/pipeline data, 1 provenance gaps, 6 recorded only. Bundle fields differing or missing: 0.

Case: Korea: battery component manufacturing

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
| import_partners_n | 57 |  |
| import_partners_total_usd | 258,281,906 |  |
| trade_benchmark_source | comtrade |  |
| trade_benchmark_period | 2022-2024 |  |
| trade_benchmark_exports_usd | 623,412,134 |  |
| trade_benchmark_share | 0.1067 |  |
| trade_benchmark_rca | 3.477 |  |

**import exposure**

| importer_iso | tech | supply_chain | observed | import_source | imports_usd | n_suppliers | top_supplier_iso3 | top_supplier_share | import_hhi | china_share | opposite_bloc_share | coercive_import_share | trusted_import_share | alignment_coverage_share | gdp_usd | imports_to_gdp | trade_year |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| KOR | Batteries | Components | TRUE | importer | 258281906 | 57 | CHN | 0.7706501 | 0.6027046 | 0.7708056 | 0.8447809 | 0.8447809 | 0.1419457 | 1 | 1.875388e+12 | 0.0001377218 | 2024 |

**current import exposure**

| importer_iso | tech | supply_chain | observed | import_source | imports_usd | n_suppliers | top_supplier_iso3 | top_supplier_share | import_hhi | china_share | opposite_bloc_share | coercive_import_share | trusted_import_share | alignment_coverage_share | gdp_usd | imports_to_gdp | window_end | months_used | window_start | current_basis |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| KOR | Batteries | Components | TRUE | importer | 306256957 | 43 | CHN | 0.7620994 | 0.5947421 | 0.7621144 | 0.8679473 | 0.8679473 | 0.121861 | 1 | 1.872375e+12 | 0.000163566 | 202512 | 12 | 202501 | own_monthly |

**import partners top10**

| partner_iso | imports_usd | share |
|---|---|---|
| CHN | 199044981 | 0.770650117 |
| VNM |  19087269 | 0.073900914 |
| ITA |   9825660 | 0.038042386 |
| DEU |   7595585 | 0.029408119 |
| ESP |   5046043 | 0.019536959 |
| USA |   3894630 | 0.015078989 |
| THA |   3069596 | 0.011884673 |
| HRV |   2837843 | 0.010987386 |
| JPN |   2150875 | 0.008327626 |
| LUX |   1960285 | 0.007589711 |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| imports_usd <- import exposure (base/current blend) | reproduced | 282,269,432 | 282,269,432 | (1 - 0.5) x base 258,281,906 + 0.5 x current 306,256,957 |
| import_hhi <- import exposure (base/current blend) | reproduced | 0.5987 | 0.5987 | (1 - 0.5) x base 0.6027 + 0.5 x current 0.5947 |
| china_share <- import exposure (base/current blend) | reproduced | 0.7665 | 0.7665 | (1 - 0.5) x base 0.7708 + 0.5 x current 0.7621 |
| opposite_bloc_share <- import exposure (base/current blend) | reproduced | 0.8564 | 0.8564 | (1 - 0.5) x base 0.8448 + 0.5 x current 0.8679 |
| top_supplier_share <- import exposure (base/current blend) | reproduced | 0.7664 | 0.7664 | (1 - 0.5) x base 0.7707 + 0.5 x current 0.7621 |
| imports_to_gdp <- import exposure (base/current blend) | reproduced | 0.0001506 | 0.0001506 | (1 - 0.5) x base 0.0001377 + 0.5 x current 0.0001636 |
| top supplier <- import exposure | reproduced | CHN | CHN |  |
| partner rows: self-trade (re-import) imports, USD | reproduced | 0 | 0 | import_partner_cells_tbl.rds lists the importer as its own supplier |
| base import HHI <- partner rows without self-trade (sum of squared shares) | reproduced | 0.6027 | 0.6027 | import_partner_cells_tbl.rds against import_exposure_tbl.rds, base year |
| base imports_usd <- partner rows without self-trade | reproduced | 258,281,906 | 258,281,906 |  |
| base top supplier <- partner rows | reproduced | CHN | CHN |  |
| home_share (world supply share) <- production / supply structure | not reproducible from shipped/pipeline data |  | 0.06173 | the supply structure is assembled inside scripts/26 (Atlas trade shares, USGS / EI production, GCIM) and not saved per source; no production-table rows for this cell |
| trade benchmark <- Comtrade energy trade rows | not reproducible from shipped/pipeline data |  | 0.1067 | the 2022-24 benchmark is computed inside scripts/26 from the raw Comtrade pull; no processed table holds its rows |

## 3. Cleaning and exclusions

| item | value | detail |
|---|---|---|
| entrepot_or_registry | FALSE | exclude_iso3: HKG, MAC, MHL, LBR, PAN |
| sn_exclusion |  |  |
| posture_exclusion |  |  |
| thin_need_cell | FALSE |  |
| thin_reason |  |  |
| material_importers_in_cell | 86 | thin below 5 |
| import_material | TRUE | immaterial below USD 1,000,000 |
| self_supplied | FALSE | self-supply guard at home_share >= 0.1 |
| cp_thin_cell | FALSE |  |
| current_trade_flag |  |  |
| opposite_bloc_basis | bloc |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| entrepot rule -> need not ranked | reproduced | FALSE | FALSE |  |
| immaterial-importer rule (imports_usd vs min_import_usd) | reproduced | TRUE | TRUE | imports_usd 282,269,432 |
| self-supply guard (home_share >= self_supply_share) | reproduced | FALSE | FALSE | home_share 0.06173 |
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
| position: manufacturing_capacity | Operating Capacity Index | EO Investment | fill structural_zero | pillar component value (within-cell index); family = mean of observed variables |
| position: investment_position | Annual Investment Index | EO Investment | fill structural_zero | pillar component value (within-cell index); family = mean of observed variables |
| position: innovation_position | Patent Innovation Index | INN Innovation | fill na | pillar component value (within-cell index); family = mean of observed variables |
| position: innovation_position | Public RD&D Index | INN Innovation | fill na | pillar component value (within-cell index); family = mean of observed variables |
| prize | siv_score | industrial_opportunity; node attributes | as_is | weighted mean of market, commons, option; strategic_industrial_value = mid-rank percentile across all scored rows; market-only cap at prize_threshold - cap_margin |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| CP families in the table are defined in the configuration | reproduced |  | export_position, manufacturing_capacity, investment_position, innovation_position |  |
| measure-level percentiles (raw value -> measure percentile) | not reproducible from shipped/pipeline data |  |  | measure values other than import HHI, China and opposite-bloc shares are not stored per cell (index_outputs contributions) |

## 5. Normalized indicators

| item | value | detail |
|---|---|---|
| sn_import_dependence | 0.01295 |  |
| sn_supplier_concentration | 0.8941 |  |
| sn_risky_supplier_exposure | 0.9578 |  |
| sn_trusted_alternatives | 0.7512 |  |
| siv_market | 0.8455 |  |
| siv_commons | 0.5 |  |
| siv_option | 0 |  |
| io_node_prize | 0.8455 |  |
| io_growth_pct | 0.8273 |  |
| io_investment_momentum_pct | 0.9 |  |
| io_home_market | 0.3095 |  |
| io_outlook | growing |  |
| io_growth_series | elec_transport |  |
| cp_export_position | 0.8675 |  |
| cp_manufacturing_capacity | 0.5695 |  |
| cp_investment_position | 0.4791 |  |
| cp_innovation_position | 0.7228 |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| sn_supplier_concentration <- percentile of import HHI in the cell | reproduced | 0.8941 | 0.8941 | import_hhi 0.5987 among 203 observed importers |
| sn_risky_supplier_exposure <- mean percentile of China and opposite-bloc shares | reproduced | 0.9578 | 0.9578 | china_share 0.7665, opposite_bloc_share 0.8564; share floor 0.01 |
| sn_import_dependence <- deficit_gdp / production surplus percentiles | not reproducible from shipped/pipeline data |  | 0.01295 | its measures live in index_outputs.rds contributions, not in the cell table |
| sn_trusted_alternatives <- 1 - (trusted world share - own share), percentile | recorded |  | 0.7512 | trusted_world_share 0.6344, home_share 0.06173 |
| siv_market = industrial_opportunity | reproduced | 0.8455 | 0.8455 |  |
| industrial_opportunity <- node prize, lifted to mean(node, home market) where the home market is higher | reproduced | 0.8455 | 0.8455 |  |
| io_node_prize <- WEO / minerals demand growth and capex momentum | not reproducible from shipped/pipeline data |  | 0.8455 | node-level growth percentiles are ranked across the 55 producing nodes inside scripts/26 |
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
| supply_chain_need_score | 0.654 |  |
| supply_chain_need_uncapped | 0.7315 |  |
| supply_chain_need | 0.7315 |  |
| supply_chain_need_cap |  |  |
| siv_score | 0.4485 |  |
| strategic_industrial_value | 0.7868 |  |
| siv_cap |  |  |
| large_yard_case | FALSE |  |
| large_yard_n | 3 |  |
| large_yard_signals | passive;substitutable;easy_qualification;diversified |  |
| large_yard_unknown | low_differentiation;commoditized |  |
| pathway_credible | yes |  |
| pathway_basis | direct_supply_share;direct_export_share;operating_capacity;committed_pipeline;feasibility;adjacent_stage_relative;learning |  |
| catch_up_anchor | TRUE |  |
| pathway_anchors_fired | direct_supply_share;direct_export_share;operating_capacity;committed_pipeline |  |
| pathway_support_fired | feasibility;adjacent_stage_relative;learning |  |
| competitive_position_score | 0.6597 |  |
| competitive_position_percentile | 0.979 |  |
| cp_contender_pct | 0.7632 |  |
| cp_contender_basis | contenders |  |
| cp_n_contenders | 19 |  |
| cp_is_contender | TRUE |  |
| home_share_measured | 0.06173 |  |
| t3_capability_lo | strong |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| need score = mean of observed need families (self-supply guard applied) | reproduced | 0.654 | 0.654 |  |
| need percentile = within-cell mid-rank of the need score | reproduced | 0.7315 | 0.7315 | 203 scored economies in the cell |
| need-floor cap (score below score_floor with percentile >= need - band) | reproduced | 0.7315 | 0.7315 | score_floor 0.1: not capped |
| siv_score = weighted mean of market, commons, option | reproduced | 0.4485 | 0.4485 |  |
| strategic_industrial_value = mid-rank among all scored rows, market-only cap | reproduced | 0.7868 | 0.7868 | 12045 scored rows; gate 0.5 |
| siv_cap | reproduced |  |  | both absent |
| large_yard_n = commodity signals holding (required signals excluded) | reproduced | 3 | 3 |  |
| large-yard signals <- node attributes and world_top_share | recorded |  | passive;substitutable;easy_qualification;diversified | recomputed for every cell by scripts/qc/run_sensitivity.R (sp_large_yard_case reproduction check) |
| pathway_credible <- anchor rule re-thresholded from the stored pw_* signals | reproduced | yes | yes | rule anchor; 6 evidence families fired of 7 observed |
| pw_* signals <- GCIM pipeline, capacity and input-cost side tables | not reproducible from shipped/pipeline data |  |  | sp_pathway() builds the signals from side tables scripts/26 does not save; only the thresholding is re-run |
| CP score = mean of observed CP families | reproduced | 0.6597 | 0.6597 | families: export_position, manufacturing_capacity, investment_position, innovation_position |
| cp_contender_pct = CP score placed among the node's contenders | reproduced | 0.7632 | 0.7632 | 19 contenders (measured share >= 0.005) |
| capability lower bound (Secure engine Test 3) | not reproducible from shipped/pipeline data |  | strong | the posture engine is not rerun by the audit (its inputs are in strategic_position_inputs.rds) |

## 8. Threshold decisions

| item | value | detail |
|---|---|---|
| decision_step | triage |  |
| borderline | cp_contender_pct |  |

**steps**

| step | test | value | threshold | margin | result | note |
|---|---|---|---|---|---|---|
| secure_engine | t1_consequence | no |  |  | no | posture engine test as stored (not rerun) |
| secure_engine | t2_vulnerability | no |  |  | no | posture engine test as stored (not rerun) |
| secure_engine | t2_path | none |  |  | none | posture engine test as stored (not rerun) |
| secure_engine | t3_capability | strong |  |  | strong | posture engine test as stored (not rerun) |
| secure_engine | t3_position | yes |  |  | yes | posture engine test as stored (not rerun) |
| secure_engine | t4_strategy | preserve |  |  | preserve | posture engine test as stored (not rerun) |
| secure_engine | t5_proportionality | pass |  |  | pass | posture engine test as stored (not rerun) |
| secure | secure_override (posture engine) | compete | secure |  | no |  |
| large_yard | large_yard_case | 3 | 5 | -2 | no | Not a large yard: not mature (scaling). |
| need | supply_chain_need | 0.7315 | 0.67 | 0.0615 | high |  |
| prize | strategic_industrial_value | 0.7868 | 0.6 | 0.1868 | high |  |
| pathway | pathway_credible | yes | yes |  | yes | read only for a high prize without a high position |
| position | cp_contender_pct | 0.7632 | 0.75 | 0.0132 | high | contender TRUE, capability lower bound strong (floor moderate/strong) |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| trace large_yard: value | reproduced | 3 | 3 | stored trace rounds to 3 decimals |
| trace large_yard: threshold | reproduced | 5 | 5 |  |
| trace need: value | reproduced | 0.7315 | 0.732 | stored trace rounds to 3 decimals |
| trace need: threshold | reproduced | 0.67 | 0.67 |  |
| trace need: result | reproduced | TRUE | TRUE | stored high |
| trace prize: value | reproduced | 0.7868 | 0.787 | stored trace rounds to 3 decimals |
| trace prize: threshold | reproduced | 0.6 | 0.6 |  |
| trace prize: result | reproduced | TRUE | TRUE | stored high |
| trace position: value | reproduced | 0.7632 | 0.763 | stored trace rounds to 3 decimals |
| trace position: threshold | reproduced | 0.75 | 0.75 |  |
| trace position: result | reproduced | TRUE | TRUE | stored high |

## 9. Final classification

| item | value | detail |
|---|---|---|
| strategic_choice | preserve |  |
| choice_label | Preserve |  |
| strategic_posture | compete |  |
| compete_mode | preserve |  |
| deploy_mode |  |  |
| decision_step | triage |  |
| posture_variant | compete_preserve |  |
| posture_confidence | moderate |  |
| choice_possible | preserve\|catch_up |  |
| pivotal_inputs | cp_contender_pct |  |
| consistency_flag |  |  |
| robustness_class | fragile |  |
| robustness_local_flips | cp_plus:catch_up |  |
| robustness_structural_flips |  |  |
| robustness_fence_change | FALSE |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| choice = canonical rule rerun from the stored inputs | reproduced | preserve | preserve |  |

## 10. Resulting recommendation

| item | value | detail |
|---|---|---|
| choice | preserve |  |
| choice_label | Preserve |  |
| choice_action | Keep and strengthen an existing strength |  |
| yard | medium |  |
| yard_and_fence | Medium yard / medium, permeable fence |  |
| fence | Medium fence |  |
| yard_rollup | medium |  |
| fence_level | moderate |  |
| fence_posture | Calibrated, temporary protection only where unfair competition threatens an established base. |  |
| gate_level | open |  |
| build_posture | Keep the established position competitive: process and next-generation R&D, workforce and supplier development, and finance for expansion, tied to benchmarks. Delivery (domestic): Build at home: production support, finance, workforce and supplier development for the domestic industry, tied to benchmarks. |  |
| benchmark | Yields, cost and quality stay at the global frontier while public support per unit falls. |  |
| success_metrics | Yields, costs and quality relative to the global frontier; Domestic engineers able to diagnose and modify the process without outside assistance; Domestic suppliers qualifying for more demanding parts of the value chain; Process improvements and successor-technology R&D generated domestically; Public support per unit falling as competitiveness improves |  |
| exit_condition | Support sunsets at each review unless the benchmarks are met. Move to Deploy if the capability case fails, and re-test for Secure if a control layer or coercive chokepoint is identified. |  |
| review_interval_years | 3 |  |
| reason | a high strategic industrial value (0.79; market 0.85, commons 0.50, option 0.00) and a top-quarter position among contenders (0.76): Preserve and extend it. |  |
| policy_alignment | aligned |  |
| policy_alignment_label | Broadly aligned |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| yard = yard of the choice (yard_rollup.R) | reproduced | medium | medium |  |
| policy-design text <- config/posture_policy_design.csv by posture variant | recorded |  | compete_preserve |  |
| policy alignment | recorded |  | aligned | after the choice; never an input to it |

## Shipped bundle comparison

Posture bundle created 2026-11-17T20:33:31Z (git 0496e7ba), detail file posture/posture-detail-KOR.31997f8d.json. 0 of 47 fields differ from the pipeline or are missing on one side. Note: the bundle's import_dependence, import_hhi and china_bloc are the need-family percentiles, not raw shares.

| field | pipeline | bundle | status |
|---|---|---|---|
| choice (core) | preserve | preserve | same |
| yard roll-up (core yd) | M | M | same |
| posture | compete | compete | same |
| variant | compete_preserve | compete_preserve | same |
| mode | preserve | preserve | same |
| confidence | moderate | moderate | same |
| need | 0.7315 | 0.732 | same |
| prize.value | 0.7868 | 0.787 | same |
| prize.market | 0.8455 | 0.845 | same |
| prize.node_prize | 0.8455 | 0.845 | same |
| prize.outlook | growing | growing | same |
| prize.measure | strategic_value | strategic_value | same |
| prize.investment_momentum | 0.9 | 0.9 | same |
| prize.home_market | 0.3095 | 0.31 | same |
| pivotal | cp_contender_pct | cp_contender_pct | same |
| cp | 0.7632 | 0.763 | same |
| home_share | 0.06173 | 0.0617 | same |
| pathway_credible | yes | yes | same |
| pathway_basis | direct_supply_share;direct_export_share;operating_capacity;committed_pipeline;feasibility;adjacent_stage_relative;learning | direct_supply_share;direct_export_share;operating_capacity;committed_pipeline;feasibility;adjacent_stage_relative;learning | same |
| borderline | cp_contender_pct | cp_contender_pct | same |
| choice_possible | preserve\|catch_up | preserve\|catch_up | same |
| reason | a high strategic industrial value (0.79; market 0.85, commons 0.50, option 0.00) and a top-quarter position among contenders (0.76): Preserve and extend it. | a high strategic industrial value (0.79; market 0.85, commons 0.50, option 0.00) and a top-quarter position among contenders (0.76): Preserve and extend it. | same |
| policy.alignment | aligned | aligned | same |
| import_dependence (sn_import_dependence, a percentile) | 0.01295 | 0.013 | same |
| import_hhi (sn_supplier_concentration, a percentile) | 0.8941 | 0.894 | same |
| china_bloc (sn_risky_supplier_exposure, a percentile) | 0.9578 | 0.958 | same |
| trusted_alternatives | 0.7512 | 0.751 | same |
| top_supplier | CHN | CHN | same |
| top_supplier_share | 0.7664 | 0.766 | same |
| china_share | 0.7665 | 0.766 | same |
| opposite_bloc_share | 0.8564 | 0.856 | same |
| imports_to_gdp | 0.0001506 | 0.000151 | same |
| trusted_share | 0.6344 | 0.634 | same |
| coercive_top | CHN | CHN | same |
| coercive_top_share | 0.2673 | 0.267 | same |
| coercive_import_share | 0.8564 | 0.856 | same |
| trade.benchmark_share | 0.1067 | 0.107 | same |
| trade.benchmark_rca | 3.477 | 3.48 | same |
| trade.momentum_score | 0.8429 | 0.843 | same |
| trade.benchmark_exports | 623,412,134 | 623,000,000 | same |
| current_trade.weight | 0.5 | 0.5 | same |
| current_trade.china_share_base | 0.7708 | 0.771 | same |
| current_trade.china_share_latest | 0.7621 | 0.762 | same |
| cp_families.export_position | 0.8675 | 0.868 | same |
| cp_families.manufacturing_capacity | 0.5695 | 0.569 | same |
| cp_families.investment_position | 0.4791 | 0.479 | same |
| cp_families.innovation_position | 0.7228 | 0.723 | same |

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

