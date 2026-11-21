# Case audit: CASE_005 India / Solar / Components

Generated 2026-11-20T00:00:00Z by scripts/qc/build_case_audit.R from <processed-dir>/outputs/strategic_position_tbl.rds (pipeline-5882f35f044e) and config/strategic_position.yml. Shipped posture bundle: created 2026-11-17T20:33:31Z, git 0496e7ba.

**Choice (pipeline): partner. Choice (shipped bundle): partner.** 67 links checked: 41 reproduced, 1 differ, 18 not reproducible from shipped/pipeline data, 1 provenance gaps, 6 recorded only. Bundle fields differing or missing: 0.

Case: India: wafer and PV cell manufacturing

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
| import_partners_n | 100 |  |
| import_partners_total_usd | 4,956,550,554 |  |
| trade_benchmark_source | comtrade |  |
| trade_benchmark_period | 2022-2024 |  |
| trade_benchmark_exports_usd | 1,147,448,400 |  |
| trade_benchmark_share | 0.007585 |  |
| trade_benchmark_rca | 0.3729 |  |

**import exposure**

| importer_iso | tech | supply_chain | observed | import_source | imports_usd | n_suppliers | top_supplier_iso3 | top_supplier_share | import_hhi | china_share | opposite_bloc_share | coercive_import_share | trusted_import_share | alignment_coverage_share | gdp_usd | imports_to_gdp | trade_year |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| IND | Solar | Components | TRUE | importer | 4956550554 | 100 | CHN | 0.653561 | 0.4365438 | 0.6597828 | 0 | 0.660227 | 0 | 1 | 3.760813e+12 | 0.001317946 | 2024 |

**current import exposure**

| importer_iso | tech | supply_chain | observed | import_source | imports_usd | n_suppliers | top_supplier_iso3 | top_supplier_share | import_hhi | china_share | opposite_bloc_share | coercive_import_share | trusted_import_share | alignment_coverage_share | gdp_usd | imports_to_gdp | window_end | months_used | window_start | current_basis |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| IND | Solar | Components | TRUE | importer | 9778515267 | 111 | CHN | 0.5109244 | 0.2867351 | 0.5143452 | 0 | 0.5146027 | 0 | 1 | 3.956067e+12 | 0.002471777 | 202608 | 12 | 202509 | own_monthly |

**import partners self**

| partner_iso | imports_usd |
|---|---|
| IND | 8862637 |

**import partners top10**

| partner_iso | imports_usd | share |
|---|---|---|
| CHN | 3239408024 | 0.65356098 |
| JPN |  276743544 | 0.05583390 |
| THA |  205732755 | 0.04150724 |
| DEU |  185674675 | 0.03746046 |
| VNM |  155588337 | 0.03139045 |
| MYS |  111112090 | 0.02241722 |
| USA |  103186466 | 0.02081820 |
| KOR |   96217348 | 0.01941216 |
| SGP |   69639365 | 0.01404997 |
| FRA |   58151586 | 0.01173227 |

**production**

| variable | data_type | value | Year | source |
|---|---|---|---|---|
| Overall Market Share | raw | 0.0600000 | 2035 | IEA Energy Technology Perspectives 2024 |
| Overall Market Share | index | 0.6339746 | 2035 | IEA Energy Technology Perspectives 2024 |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| imports_usd <- import exposure (base/current blend) | reproduced | 7,367,532,910 | 7,367,532,910 | (1 - 0.5) x base 4,956,550,554 + 0.5 x current 9,778,515,267 |
| import_hhi <- import exposure (base/current blend) | reproduced | 0.3616 | 0.3616 | (1 - 0.5) x base 0.4365 + 0.5 x current 0.2867 |
| china_share <- import exposure (base/current blend) | reproduced | 0.5871 | 0.5871 | (1 - 0.5) x base 0.6598 + 0.5 x current 0.5143 |
| opposite_bloc_share <- not applicable for a nonaligned importer (strategic_need.R rule 6) | reproduced |  |  | import exposure 0 |
| top_supplier_share <- import exposure (base/current blend) | reproduced | 0.5822 | 0.5822 | (1 - 0.5) x base 0.6536 + 0.5 x current 0.5109 |
| imports_to_gdp <- import exposure (base/current blend) | reproduced | 0.001895 | 0.001895 | (1 - 0.5) x base 0.001318 + 0.5 x current 0.002472 |
| top supplier <- import exposure | reproduced | CHN | CHN |  |
| partner rows: self-trade (re-import) imports, USD | differs | 0 | 8,862,637 | import_partner_cells_tbl.rds lists the importer as its own supplier (0.001785 of its partner rows); import_exposure_tbl.rds leaves these out, but sp_partner_recs() diversification and the partnership memo read the partner table as it is |
| base import HHI <- partner rows without self-trade (sum of squared shares) | reproduced | 0.4365 | 0.4365 | import_partner_cells_tbl.rds against import_exposure_tbl.rds, base year |
| base imports_usd <- partner rows without self-trade | reproduced | 4,956,550,554 | 4,956,550,554 |  |
| base top supplier <- partner rows | reproduced | CHN | CHN |  |
| home_share (world supply share) <- production / supply structure | not reproducible from shipped/pipeline data |  | 0.01015 | the supply structure is assembled inside scripts/26 (Atlas trade shares, USGS / EI production, GCIM) and not saved per source; 2 production-table rows shown for context |
| trade benchmark <- Comtrade energy trade rows | not reproducible from shipped/pipeline data |  | 0.007585 | the 2022-24 benchmark is computed inside scripts/26 from the raw Comtrade pull; no processed table holds its rows |

## 3. Cleaning and exclusions

| item | value | detail |
|---|---|---|
| entrepot_or_registry | FALSE | exclude_iso3: HKG, MAC, MHL, LBR, PAN |
| sn_exclusion |  |  |
| posture_exclusion |  |  |
| thin_need_cell | FALSE |  |
| thin_reason |  |  |
| material_importers_in_cell | 182 | thin below 5 |
| import_material | TRUE | immaterial below USD 1,000,000 |
| self_supplied | FALSE | self-supply guard at home_share >= 0.1 |
| cp_thin_cell | FALSE |  |
| current_trade_flag | china_down |  |
| opposite_bloc_basis | nonaligned |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| entrepot rule -> need not ranked | reproduced | FALSE | FALSE |  |
| immaterial-importer rule (imports_usd vs min_import_usd) | reproduced | TRUE | TRUE | imports_usd 7,367,532,910 |
| self-supply guard (home_share >= self_supply_share) | reproduced | FALSE | FALSE | home_share 0.01015 |
| which countries enter the cell's percentiles | recorded |  | 205 | the ranked universe (es_contributions countries less exclude_iso3) is rebuilt only inside scripts/26 |

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
| sn_import_dependence | 0.7239 |  |
| sn_supplier_concentration | 0.6872 |  |
| sn_risky_supplier_exposure | 0.8292 |  |
| sn_trusted_alternatives | 0.4366 |  |
| siv_market | 0.8781 |  |
| siv_commons | 0 |  |
| siv_option | 0.375 |  |
| io_node_prize | 0.85 |  |
| io_growth_pct | 0.9 |  |
| io_investment_momentum_pct | 0.7 |  |
| io_home_market | 0.9061 |  |
| io_outlook | growing |  |
| io_growth_series | cap_solar |  |
| cp_export_position | 0.7205 |  |
| cp_manufacturing_capacity | 0.755 |  |
| cp_investment_position | 0.7948 |  |
| cp_innovation_position | 0.6088 |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| sn_supplier_concentration <- percentile of import HHI in the cell | reproduced | 0.6872 | 0.6872 | import_hhi 0.3616 among 203 observed importers |
| sn_risky_supplier_exposure <- mean percentile of China and opposite-bloc shares | reproduced | 0.8292 | 0.8292 | china_share 0.5871, opposite_bloc_share NA; share floor 0.01 |
| sn_import_dependence <- deficit_gdp / production surplus percentiles | not reproducible from shipped/pipeline data |  | 0.7239 | its measures live in index_outputs.rds contributions, not in the cell table |
| sn_trusted_alternatives <- 1 - (trusted world share - own share), percentile | recorded |  | 0.4366 | trusted_world_share 0.7063, home_share 0.01015 |
| siv_market = industrial_opportunity | reproduced | 0.8781 | 0.8781 |  |
| industrial_opportunity <- node prize, lifted to mean(node, home market) where the home market is higher | reproduced | 0.8781 | 0.8781 |  |
| io_node_prize <- WEO / minerals demand growth and capex momentum | not reproducible from shipped/pipeline data |  | 0.85 | node-level growth percentiles are ranked across the 55 producing nodes inside scripts/26 |
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
| supply_chain_need_score | 0.6692 |  |
| supply_chain_need_uncapped | 0.8512 |  |
| supply_chain_need | 0.8512 |  |
| supply_chain_need_cap |  |  |
| siv_score | 0.4177 |  |
| strategic_industrial_value | 0.59 |  |
| siv_cap | market_only |  |
| large_yard_case | FALSE |  |
| large_yard_n | 3 |  |
| large_yard_signals | passive;substitutable;easy_qualification;diversified |  |
| large_yard_unknown | low_learning;low_spillover |  |
| pathway_credible | yes |  |
| pathway_basis | direct_supply_share;direct_export_share;operating_capacity;committed_pipeline;feasibility;adjacent_stage_relative |  |
| catch_up_anchor | TRUE |  |
| pathway_anchors_fired | direct_supply_share;direct_export_share;operating_capacity;committed_pipeline |  |
| pathway_support_fired | feasibility;adjacent_stage_relative |  |
| competitive_position_score | 0.7198 |  |
| competitive_position_percentile | 0.993 |  |
| cp_contender_pct | 0.9483 |  |
| cp_contender_basis | contenders |  |
| cp_n_contenders | 29 |  |
| cp_is_contender | TRUE |  |
| home_share_measured | 0.01015 |  |
| t3_capability_lo | none |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| need score = mean of observed need families (self-supply guard applied) | reproduced | 0.6692 | 0.6692 |  |
| need percentile = within-cell mid-rank of the need score | reproduced | 0.8512 | 0.8512 | 205 scored economies in the cell |
| need-floor cap (score below score_floor with percentile >= need - band) | reproduced | 0.8512 | 0.8512 | score_floor 0.1: not capped |
| siv_score = weighted mean of market, commons, option | reproduced | 0.4177 | 0.4177 |  |
| strategic_industrial_value = mid-rank among all scored rows, market-only cap | reproduced | 0.59 | 0.59 | 12045 scored rows; gate 0.5: market only, capped |
| siv_cap | reproduced | market_only | market_only |  |
| large_yard_n = commodity signals holding (required signals excluded) | reproduced | 3 | 3 |  |
| large-yard signals <- node attributes and world_top_share | recorded |  | passive;substitutable;easy_qualification;diversified | recomputed for every cell by scripts/qc/run_sensitivity.R (sp_large_yard_case reproduction check) |
| pathway_credible <- anchor rule re-thresholded from the stored pw_* signals | reproduced | yes | yes | rule anchor; 5 evidence families fired of 6 observed |
| pw_* signals <- GCIM pipeline, capacity and input-cost side tables | not reproducible from shipped/pipeline data |  |  | sp_pathway() builds the signals from side tables scripts/26 does not save; only the thresholding is re-run |
| CP score = mean of observed CP families | reproduced | 0.7198 | 0.7198 | families: export_position, manufacturing_capacity, investment_position, innovation_position |
| cp_contender_pct = CP score placed among the node's contenders | reproduced | 0.9483 | 0.9483 | 29 contenders (measured share >= 0.005) |
| capability lower bound (Secure engine Test 3) | not reproducible from shipped/pipeline data |  | none | the posture engine is not rerun by the audit (its inputs are in strategic_position_inputs.rds) |

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
| secure_engine | t3_capability | unknown |  |  | unknown | posture engine test as stored (not rerun) |
| secure_engine | t3_position | no |  |  | no | posture engine test as stored (not rerun) |
| secure_engine | t4_strategy | no |  |  | no | posture engine test as stored (not rerun) |
| secure_engine | t5_proportionality | fail |  |  | fail | posture engine test as stored (not rerun) |
| secure | secure_override (posture engine) | deploy | secure |  | no |  |
| large_yard | large_yard_case | 3 | 5 | -2 | no | Not a large yard: not mature (scaling). |
| need | supply_chain_need | 0.8512 | 0.67 | 0.1812 | high |  |
| prize | strategic_industrial_value | 0.59 | 0.6 | -0.01 | low (market-only cap) |  |
| pathway | pathway_credible | yes | yes |  | yes | read only for a high prize without a high position |
| position | cp_contender_pct | 0.9483 | 0.75 | 0.1983 | not_top_quarter | contender TRUE, capability lower bound none (floor moderate/strong) |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| trace large_yard: value | reproduced | 3 | 3 | stored trace rounds to 3 decimals |
| trace large_yard: threshold | reproduced | 5 | 5 |  |
| trace need: value | reproduced | 0.8512 | 0.851 | stored trace rounds to 3 decimals |
| trace need: threshold | reproduced | 0.67 | 0.67 |  |
| trace need: result | reproduced | TRUE | TRUE | stored high |
| trace prize: value | reproduced | 0.59 | 0.59 | stored trace rounds to 3 decimals |
| trace prize: threshold | reproduced | 0.6 | 0.6 |  |
| trace prize: result | reproduced | FALSE | FALSE | stored low |
| trace position: value | reproduced | 0.9483 | 0.948 | stored trace rounds to 3 decimals |
| trace position: threshold | reproduced | 0.75 | 0.75 |  |
| trace position: result | reproduced | FALSE | FALSE | stored capability_floor_not_met |

## 9. Final classification

| item | value | detail |
|---|---|---|
| strategic_choice | partner |  |
| choice_label | Partner |  |
| strategic_posture | compete |  |
| compete_mode | partner |  |
| deploy_mode |  |  |
| decision_step | triage |  |
| posture_variant | compete_partner |  |
| posture_confidence | moderate |  |
| choice_possible | partner |  |
| pivotal_inputs |  |  |
| consistency_flag | review_top_quarter_below_floor |  |
| robustness_class | sensitive |  |
| robustness_local_flips |  |  |
| robustness_structural_flips | siv_gate_033:catch_up;siv_unknown_half:catch_up |  |
| robustness_fence_change | FALSE |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| choice = canonical rule rerun from the stored inputs | reproduced | partner | partner |  |

## 10. Resulting recommendation

| item | value | detail |
|---|---|---|
| choice | partner |  |
| choice_label | Partner |  |
| choice_action | Rely on trusted partners or diversified supply |  |
| yard | medium |  |
| yard_and_fence | Medium yard / medium, permeable fence |  |
| fence | Medium fence |  |
| yard_rollup | medium |  |
| fence_level | moderate |  |
| fence_posture | A medium, permeable fence: open to the recommended trusted partners, closed to coercive control (screening of coercive suppliers' investment, trusted-source conditions in critical uses); partnership terms, not tariffs, decide who supplies. |  |
| gate_level | open |  |
| build_posture | Build the partnership, not a domestic replica: offtake and co-investment with the recommended partners, diversified contracts and the infrastructure that brings partner supply in. Delivery (allied): Build the capability jointly with partners (co-investment, joint ventures, offtake, shared R&D); the domestic share is the segment where the country is strongest. |  |
| benchmark | Trusted partners supply the bulk of the country's need at a delivered cost close to the world price. |  |
| success_metrics | Share of the country's supply from trusted partners; Supplier diversity (HHI) and the share of any potentially coercive supplier; Delivered cost of the partner supply |  |
| exit_condition | Public finance for partner capacity sunsets on the stated schedule unless partner supply is growing. Re-test when the country's opportunity in the node rises (Catch Up or Preserve), its need falls (Watch), or a partner gap opens (no credible trusted supplier). |  |
| review_interval_years | 3 |  |
| reason | High supply-chain need (0.85) and a strategic industrial value held below the threshold (0.59: the market prize alone; market 0.88, commons 0.00, option 0.38): a security problem without a domestic industrial prize. Partner with trusted suppliers. |  |
| recommended_partners | JPN, MYS, DEU |  |
| policy_alignment | misaligned |  |
| policy_alignment_label | Wrong tool for the problem |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| yard = yard of the choice (yard_rollup.R) | reproduced | medium | medium |  |
| policy-design text <- config/posture_policy_design.csv by posture variant | recorded |  | compete_partner |  |
| policy alignment | recorded |  | misaligned | after the choice; never an input to it |

## Shipped bundle comparison

Posture bundle created 2026-11-17T20:33:31Z (git 0496e7ba), detail file posture/posture-detail-IND.3cd17ed1.json. 0 of 45 fields differ from the pipeline or are missing on one side. Note: the bundle's import_dependence, import_hhi and china_bloc are the need-family percentiles, not raw shares.

| field | pipeline | bundle | status |
|---|---|---|---|
| choice (core) | partner | partner | same |
| yard roll-up (core yd) | M | M | same |
| posture | compete | compete | same |
| variant | compete_partner | compete_partner | same |
| mode | partner | partner | same |
| confidence | moderate | moderate | same |
| need | 0.8512 | 0.851 | same |
| prize.value | 0.59 | 0.59 | same |
| prize.market | 0.8781 | 0.878 | same |
| prize.node_prize | 0.85 | 0.85 | same |
| prize.outlook | growing | growing | same |
| prize.measure | strategic_value | strategic_value | same |
| prize.investment_momentum | 0.7 | 0.7 | same |
| prize.home_market | 0.9061 | 0.906 | same |
| cp | 0.9483 | 0.948 | same |
| home_share | 0.01015 | 0.0101 | same |
| pathway_credible | yes | yes | same |
| pathway_basis | direct_supply_share;direct_export_share;operating_capacity;committed_pipeline;feasibility;adjacent_stage_relative | direct_supply_share;direct_export_share;operating_capacity;committed_pipeline;feasibility;adjacent_stage_relative | same |
| consistency_flag | review_top_quarter_below_floor | review_top_quarter_below_floor | same |
| reason | High supply-chain need (0.85) and a strategic industrial value held below the threshold (0.59: the market prize alone; market 0.88, commons 0.00, option 0.38): a security problem without a domestic industrial prize. Partner with trusted suppliers. | High supply-chain need (0.85) and a strategic industrial value held below the threshold (0.59: the market prize alone; market 0.88, commons 0.00, option 0.38): a security problem without a domestic industrial prize. Partner with trusted suppliers. | same |
| policy.alignment | misaligned | misaligned | same |
| partners[1].iso3 | JPN | JPN | same |
| import_dependence (sn_import_dependence, a percentile) | 0.7239 | 0.724 | same |
| import_hhi (sn_supplier_concentration, a percentile) | 0.6872 | 0.687 | same |
| china_bloc (sn_risky_supplier_exposure, a percentile) | 0.8292 | 0.829 | same |
| trusted_alternatives | 0.4366 | 0.437 | same |
| top_supplier | CHN | CHN | same |
| top_supplier_share | 0.5822 | 0.582 | same |
| china_share | 0.5871 | 0.587 | same |
| imports_to_gdp | 0.001895 | 0.00189 | same |
| trusted_share | 0.7063 | 0.706 | same |
| coercive_top | CHN | CHN | same |
| coercive_top_share | 0.2595 | 0.259 | same |
| coercive_import_share | 0.5874 | 0.587 | same |
| trade.benchmark_share | 0.007585 | 0.00759 | same |
| trade.benchmark_rca | 0.3729 | 0.373 | same |
| trade.momentum_score | 0.8816 | 0.882 | same |
| trade.benchmark_exports | 1,147,448,400 | 1,150,000,000 | same |
| current_trade.weight | 0.5 | 0.5 | same |
| current_trade.china_share_base | 0.6598 | 0.66 | same |
| current_trade.china_share_latest | 0.5143 | 0.514 | same |
| cp_families.export_position | 0.7205 | 0.72 | same |
| cp_families.manufacturing_capacity | 0.755 | 0.755 | same |
| cp_families.investment_position | 0.7948 | 0.795 | same |
| cp_families.innovation_position | 0.6088 | 0.609 | same |

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
- 2. partner rows: self-trade (re-import) imports, USD: differs (import_partner_cells_tbl.rds lists the importer as its own supplier (0.001785 of its partner rows); import_exposure_tbl.rds leaves these out, but sp_partner_recs() diversification and the partnership memo read the partner table as it is)
- 2. home_share (world supply share) <- production / supply structure: not reproducible from shipped/pipeline data (the supply structure is assembled inside scripts/26 (Atlas trade shares, USGS / EI production, GCIM) and not saved per source; 2 production-table rows shown for context)
- 2. trade benchmark <- Comtrade energy trade rows: not reproducible from shipped/pipeline data (the 2022-24 benchmark is computed inside scripts/26 from the raw Comtrade pull; no processed table holds its rows)
- 4. measure-level percentiles (raw value -> measure percentile): not reproducible from shipped/pipeline data (measure values other than import HHI, China and opposite-bloc shares are not stored per cell (index_outputs contributions))
- 5. sn_import_dependence <- deficit_gdp / production surplus percentiles: not reproducible from shipped/pipeline data (its measures live in index_outputs.rds contributions, not in the cell table)
- 5. io_node_prize <- WEO / minerals demand growth and capex momentum: not reproducible from shipped/pipeline data (node-level growth percentiles are ranked across the 55 producing nodes inside scripts/26)
- 5. CP family values <- pillar variable contributions: not reproducible from shipped/pipeline data (variable contributions live in index_outputs.rds, not in the cell table)
- 7. pw_* signals <- GCIM pipeline, capacity and input-cost side tables: not reproducible from shipped/pipeline data (sp_pathway() builds the signals from side tables scripts/26 does not save; only the thresholding is re-run)
- 7. capability lower bound (Secure engine Test 3): not reproducible from shipped/pipeline data (the posture engine is not rerun by the audit (its inputs are in strategic_position_inputs.rds))

