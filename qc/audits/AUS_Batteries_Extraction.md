# Case audit: CASE_003 Australia / Batteries / Extraction

Generated 2026-11-20T00:00:00Z by scripts/qc/build_case_audit.R from <processed-dir>/outputs/strategic_position_tbl.rds (pipeline-5882f35f044e) and config/strategic_position.yml. Shipped posture bundle: created 2026-11-17T20:33:31Z, git 0496e7ba.

**Choice (pipeline): watch. Choice (shipped bundle): watch.** 67 links checked: 41 reproduced, 1 differ, 18 not reproducible from shipped/pipeline data, 1 provenance gaps, 6 recorded only. Bundle fields differing or missing: 0.

Case: Australia: battery mineral mining

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
| import_partners_n | 36 |  |
| import_partners_total_usd | 40,306,135 |  |
| trade_benchmark_source | comtrade |  |
| trade_benchmark_period | 2022-2024 |  |
| trade_benchmark_exports_usd | 8,219,361,015 |  |
| trade_benchmark_share | 0.4405 |  |
| trade_benchmark_rca | 25.21 |  |

**import exposure**

| importer_iso | tech | supply_chain | observed | import_source | imports_usd | n_suppliers | top_supplier_iso3 | top_supplier_share | import_hhi | china_share | opposite_bloc_share | coercive_import_share | trusted_import_share | alignment_coverage_share | gdp_usd | imports_to_gdp | trade_year |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| AUS | Batteries | Extraction | TRUE | importer | 40306135 | 36 | GAB | 0.4662068 | 0.3617813 | 0.01883577 | 0.02050186 | 0.02050186 | 0.133181 | 1 | 1.757022e+12 | 2.294002e-05 | 2024 |

**current import exposure**

| importer_iso | tech | supply_chain | observed | import_source | imports_usd | n_suppliers | top_supplier_iso3 | top_supplier_share | import_hhi | china_share | opposite_bloc_share | coercive_import_share | trusted_import_share | alignment_coverage_share | gdp_usd | imports_to_gdp | window_end | months_used | window_start | current_basis |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| AUS | Batteries | Extraction | TRUE | importer | 8073506 | 35 | FIN | 0.4425945 | 0.2369288 | 0.09961126 | 0.1076239 | 0.1076239 | 0.786942 | 1 | 1.798519e+12 | 4.488975e-06 | 202608 | 12 | 202509 | own_monthly |

**import partners self**

| partner_iso | imports_usd |
|---|---|
| AUS | 22517.48 |

**import partners top10**

| partner_iso | imports_usd | share |
|---|---|---|
| GAB | 18790994.94 | 0.466206818 |
| ZAF | 14859708.04 | 0.368671122 |
| FIN |  3509774.54 | 0.087077923 |
| USA |   764068.19 | 0.018956623 |
| CHN |   755561.69 | 0.018745575 |
| NZL |   422055.09 | 0.010471237 |
| IND |   305537.47 | 0.007580421 |
| DEU |   183394.46 | 0.004550038 |
| ITA |   177239.12 | 0.004397324 |
| NLD |    62356.25 | 0.001547066 |

**production**

| variable | data_type | value | Year | source |
|---|---|---|---|---|
| Batteries Production | index | 0.843387 | 2025 | EI Statistical Review of World Energy (2026 edition) |
| Overall Production Index | index | 0.843387 | 2025 | Author calculation |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| imports_usd <- import exposure (base/current blend) | reproduced | 24,189,821 | 24,189,821 | (1 - 0.5) x base 40,306,135 + 0.5 x current 8,073,506 |
| import_hhi <- import exposure (base/current blend) | reproduced | 0.2994 | 0.2994 | (1 - 0.5) x base 0.3618 + 0.5 x current 0.2369 |
| china_share <- import exposure (base/current blend) | reproduced | 0.05922 | 0.05922 | (1 - 0.5) x base 0.01884 + 0.5 x current 0.09961 |
| opposite_bloc_share <- import exposure (base/current blend) | reproduced | 0.06406 | 0.06406 | (1 - 0.5) x base 0.0205 + 0.5 x current 0.1076 |
| top_supplier_share <- import exposure (base/current blend) | reproduced | 0.4544 | 0.4544 | (1 - 0.5) x base 0.4662 + 0.5 x current 0.4426 |
| imports_to_gdp <- import exposure (base/current blend) | reproduced | 1.371e-05 | 1.371e-05 | (1 - 0.5) x base 2.294e-05 + 0.5 x current 4.489e-06 |
| top supplier <- import exposure | reproduced | GAB | GAB |  |
| partner rows: self-trade (re-import) imports, USD | differs | 0 | 22520 | import_partner_cells_tbl.rds lists the importer as its own supplier (0.0005583 of its partner rows); import_exposure_tbl.rds leaves these out, but sp_partner_recs() diversification and the partnership memo read the partner table as it is |
| base import HHI <- partner rows without self-trade (sum of squared shares) | reproduced | 0.3618 | 0.3618 | import_partner_cells_tbl.rds against import_exposure_tbl.rds, base year |
| base imports_usd <- partner rows without self-trade | reproduced | 40,306,135 | 40,306,135 |  |
| base top supplier <- partner rows | reproduced | GAB | GAB |  |
| home_share (world supply share) <- production / supply structure | not reproducible from shipped/pipeline data |  | 0.1601 | the supply structure is assembled inside scripts/26 (Atlas trade shares, USGS / EI production, GCIM) and not saved per source; 2 production-table rows shown for context |
| trade benchmark <- Comtrade energy trade rows | not reproducible from shipped/pipeline data |  | 0.4405 | the 2022-24 benchmark is computed inside scripts/26 from the raw Comtrade pull; no processed table holds its rows |

## 3. Cleaning and exclusions

| item | value | detail |
|---|---|---|
| entrepot_or_registry | FALSE | exclude_iso3: HKG, MAC, MHL, LBR, PAN |
| sn_exclusion |  |  |
| posture_exclusion |  |  |
| thin_need_cell | FALSE |  |
| thin_reason |  |  |
| material_importers_in_cell | 81 | thin below 5 |
| import_material | TRUE | immaterial below USD 1,000,000 |
| self_supplied | TRUE | self-supply guard at home_share >= 0.1 |
| cp_thin_cell | FALSE |  |
| current_trade_flag |  |  |
| opposite_bloc_basis | bloc |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| entrepot rule -> need not ranked | reproduced | FALSE | FALSE |  |
| immaterial-importer rule (imports_usd vs min_import_usd) | reproduced | TRUE | TRUE | imports_usd 24,189,821 |
| self-supply guard (home_share >= self_supply_share) | reproduced | TRUE | TRUE | home_share 0.1601 |
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
| position: resource_endowment |  Reserves$ | ES Reserves | fill structural_zero | pillar component value (within-cell index); family = mean of observed variables |
| position: resource_endowment | Potential Index$ | ES Reserves | fill na | pillar component value (within-cell index); family = mean of observed variables |
| prize | siv_score | industrial_opportunity; node attributes | as_is | weighted mean of market, commons, option; strategic_industrial_value = mid-rank percentile across all scored rows; market-only cap at prize_threshold - cap_margin |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| CP families in the table are defined in the configuration | reproduced |  | export_position, production_position, resource_endowment |  |
| measure-level percentiles (raw value -> measure percentile) | not reproducible from shipped/pipeline data |  |  | measure values other than import HHI, China and opposite-bloc shares are not stored per cell (index_outputs contributions) |

## 5. Normalized indicators

| item | value | detail |
|---|---|---|
| sn_import_dependence | 0.05108 |  |
| sn_supplier_concentration | 0.8202 |  |
| sn_risky_supplier_exposure | 0.7511 |  |
| sn_trusted_alternatives | 0 |  |
| siv_market | 0.7803 |  |
| siv_commons | 0.3333 |  |
| siv_option | 0 |  |
| io_node_prize | 0.7273 |  |
| io_growth_pct | 0.7273 |  |
| io_investment_momentum_pct |  |  |
| io_home_market | 0.8333 |  |
| io_outlook | growing |  |
| io_growth_series | minerals:Electric vehicles\|Grid battery storage |  |
| cp_export_position | 0.8619 |  |
| cp_production_position | 0.8386 |  |
| cp_resource_endowment | 0.8025 |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| sn_supplier_concentration <- percentile of import HHI in the cell | reproduced | 0.8202 | 0.8202 | import_hhi 0.2994 among 203 observed importers |
| sn_risky_supplier_exposure <- mean percentile of China and opposite-bloc shares | reproduced | 0.7511 | 0.7511 | china_share 0.05922, opposite_bloc_share 0.06406; share floor 0.01 |
| sn_import_dependence <- deficit_gdp / production surplus percentiles | not reproducible from shipped/pipeline data |  | 0.05108 | its measures live in index_outputs.rds contributions, not in the cell table |
| sn_trusted_alternatives <- 1 - (trusted world share - own share), percentile | recorded |  | 0 | trusted_world_share 0, home_share 0.1601 |
| siv_market = industrial_opportunity | reproduced | 0.7803 | 0.7803 |  |
| industrial_opportunity <- node prize, lifted to mean(node, home market) where the home market is higher | reproduced | 0.7803 | 0.7803 |  |
| io_node_prize <- WEO / minerals demand growth and capex momentum | not reproducible from shipped/pipeline data |  | 0.7273 | node-level growth percentiles are ranked across the 55 producing nodes inside scripts/26 |
| CP family values <- pillar variable contributions | not reproducible from shipped/pipeline data |  |  | variable contributions live in index_outputs.rds, not in the cell table |

## 6. Weights

| item | value | detail |
|---|---|---|
| need_families | equal over observed families (4; trusted alternatives in the score: TRUE) |  |
| cp_families | equal over observed families (3 of 3 applicable) |  |
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
| supply_chain_need_score | 0.01277 |  |
| supply_chain_need_uncapped | 0.1152 |  |
| supply_chain_need | 0.1152 |  |
| supply_chain_need_cap |  |  |
| siv_score | 0.3712 |  |
| strategic_industrial_value | 0.59 |  |
| siv_cap | market_only |  |
| large_yard_case | FALSE |  |
| large_yard_n | 4 |  |
| large_yard_signals | mature;passive;substitutable;easy_qualification;low_spillover;low_differentiation |  |
| large_yard_unknown |  |  |
| pathway_credible | yes |  |
| pathway_basis | direct_supply_share;direct_export_share;resource_base;adjacent_stage_relative;learning |  |
| catch_up_anchor | TRUE |  |
| pathway_anchors_fired | direct_supply_share;direct_export_share;resource_base |  |
| pathway_support_fired | adjacent_stage_relative;learning |  |
| competitive_position_score | 0.8343 |  |
| competitive_position_percentile | 0.9931 |  |
| cp_contender_pct | 0.8636 |  |
| cp_contender_basis | contenders |  |
| cp_n_contenders | 11 |  |
| cp_is_contender | TRUE |  |
| home_share_measured | 0.1601 |  |
| t3_capability_lo | strong |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| need score = mean of observed need families (self-supply guard applied) | reproduced | 0.01277 | 0.01277 |  |
| need percentile = within-cell mid-rank of the need score | reproduced | 0.1152 | 0.1152 | 204 scored economies in the cell |
| need-floor cap (score below score_floor with percentile >= need - band) | reproduced | 0.1152 | 0.1152 | score_floor 0.1: not capped |
| siv_score = weighted mean of market, commons, option | reproduced | 0.3712 | 0.3712 |  |
| strategic_industrial_value = mid-rank among all scored rows, market-only cap | reproduced | 0.59 | 0.59 | 12045 scored rows; gate 0.5: market only, capped |
| siv_cap | reproduced | market_only | market_only |  |
| large_yard_n = commodity signals holding (required signals excluded) | reproduced | 4 | 4 |  |
| large-yard signals <- node attributes and world_top_share | recorded |  | mature;passive;substitutable;easy_qualification;low_spillover;low_differentiation | recomputed for every cell by scripts/qc/run_sensitivity.R (sp_large_yard_case reproduction check) |
| pathway_credible <- anchor rule re-thresholded from the stored pw_* signals | reproduced | yes | yes | rule anchor; 4 evidence families fired of 6 observed |
| pw_* signals <- GCIM pipeline, capacity and input-cost side tables | not reproducible from shipped/pipeline data |  |  | sp_pathway() builds the signals from side tables scripts/26 does not save; only the thresholding is re-run |
| CP score = mean of observed CP families | reproduced | 0.8343 | 0.8343 | families: export_position, production_position, resource_endowment |
| cp_contender_pct = CP score placed among the node's contenders | reproduced | 0.8636 | 0.8636 | 11 contenders (measured share >= 0.005) |
| capability lower bound (Secure engine Test 3) | not reproducible from shipped/pipeline data |  | strong | the posture engine is not rerun by the audit (its inputs are in strategic_position_inputs.rds) |

## 8. Threshold decisions

| item | value | detail |
|---|---|---|
| decision_step | triage |  |
| borderline | large_yard_case |  |

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
| large_yard | large_yard_case | 4 | 5 | -1 | no | Not a large yard: only 4 of 8 commodity signals hold (substitutable, quick to qualify, little cross-sector spillover, little product differentiation), where 5 are required. |
| need | supply_chain_need | 0.1152 | 0.67 | -0.5548 | low |  |
| prize | strategic_industrial_value | 0.59 | 0.6 | -0.01 | low (market-only cap) |  |
| pathway | pathway_credible | yes | yes |  | yes | read only for a high prize without a high position |
| position | cp_contender_pct | 0.8636 | 0.75 | 0.1136 | high | contender TRUE, capability lower bound strong (floor moderate/strong) |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| trace large_yard: value | reproduced | 4 | 4 | stored trace rounds to 3 decimals |
| trace large_yard: threshold | reproduced | 5 | 5 |  |
| trace need: value | reproduced | 0.1152 | 0.115 | stored trace rounds to 3 decimals |
| trace need: threshold | reproduced | 0.67 | 0.67 |  |
| trace need: result | reproduced | FALSE | FALSE | stored low |
| trace prize: value | reproduced | 0.59 | 0.59 | stored trace rounds to 3 decimals |
| trace prize: threshold | reproduced | 0.6 | 0.6 |  |
| trace prize: result | reproduced | FALSE | FALSE | stored low |
| trace position: value | reproduced | 0.8636 | 0.864 | stored trace rounds to 3 decimals |
| trace position: threshold | reproduced | 0.75 | 0.75 |  |
| trace position: result | reproduced | TRUE | TRUE | stored high |

## 9. Final classification

| item | value | detail |
|---|---|---|
| strategic_choice | watch |  |
| choice_label | Watch |  |
| strategic_posture | deploy |  |
| compete_mode |  |  |
| deploy_mode | watch |  |
| decision_step | triage |  |
| posture_variant | deploy_diversify |  |
| posture_confidence | moderate |  |
| choice_possible | watch |  |
| pivotal_inputs |  |  |
| consistency_flag |  |  |
| robustness_class | sensitive |  |
| robustness_local_flips |  |  |
| robustness_structural_flips | siv_gate_033:preserve |  |
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
| fence_posture | A low fence: manage concentration by diversifying supply, not by restricting it. |  |
| gate_level | wide_open |  |
| build_posture | Build what enables deployment and diversify supply toward trusted producers, not a domestic replica. Delivery (diversified global): Supply comes from a diversified global base; the build is enabling infrastructure, not production. |  |
| benchmark | The largest potentially coercive supplier's share of the country's supply falls below the watch line without a material rise in delivered cost. |  |
| success_metrics | Delivered cost and pace of deployment; Supplier diversity (HHI) and the share held by any potentially coercive supplier; Compliance with cross-cutting safeguards (forced labour, safety, trade law) |  |
| exit_condition | Returns to deploy when supply is diversified below the watch lines. Re-test for Secure if the coercive supplier's share passes the screening line and the substitution and timing legs hold, or if a control layer is identified. |  |
| review_interval_years | 3 |  |
| reason | Low supply-chain need (0.12) and a strategic industrial value held below the threshold (0.59: the market prize alone; market 0.78, commons 0.33, option 0.00): Watch. Domestic industrial priority is low. |  |
| watch_reasons | low_need;low_prize |  |
| policy_alignment | mixed |  |
| policy_alignment_label | Mixed signals |  |

**Links**

| link | status | expected | observed | detail |
|---|---|---|---|---|
| yard = yard of the choice (yard_rollup.R) | reproduced | large | large |  |
| policy-design text <- config/posture_policy_design.csv by posture variant | recorded |  | deploy_diversify |  |
| policy alignment | recorded |  | mixed | after the choice; never an input to it |

## Shipped bundle comparison

Posture bundle created 2026-11-17T20:33:31Z (git 0496e7ba), detail file posture/posture-detail-AUS.dbd8729a.json. 0 of 43 fields differ from the pipeline or are missing on one side. Note: the bundle's import_dependence, import_hhi and china_bloc are the need-family percentiles, not raw shares.

| field | pipeline | bundle | status |
|---|---|---|---|
| choice (core) | watch | watch | same |
| yard roll-up (core yd) | L | L | same |
| posture | deploy | deploy | same |
| variant | deploy_diversify | deploy_diversify | same |
| deploy_mode | watch | watch | same |
| confidence | moderate | moderate | same |
| need | 0.1152 | 0.115 | same |
| prize.value | 0.59 | 0.59 | same |
| prize.market | 0.7803 | 0.78 | same |
| prize.node_prize | 0.7273 | 0.727 | same |
| prize.outlook | growing | growing | same |
| prize.measure | strategic_value | strategic_value | same |
| prize.home_market | 0.8333 | 0.833 | same |
| cp | 0.8636 | 0.864 | same |
| home_share | 0.1601 | 0.16 | same |
| pathway_credible | yes | yes | same |
| pathway_basis | direct_supply_share;direct_export_share;resource_base;adjacent_stage_relative;learning | direct_supply_share;direct_export_share;resource_base;adjacent_stage_relative;learning | same |
| borderline | large_yard_case | large_yard_case | same |
| reason | Low supply-chain need (0.12) and a strategic industrial value held below the threshold (0.59: the market prize alone; market 0.78, commons 0.33, option 0.00): Watch. Domestic industrial priority is low. | Low supply-chain need (0.12) and a strategic industrial value held below the threshold (0.59: the market prize alone; market 0.78, commons 0.33, option 0.00): Watch. Domestic industrial priority is low. | same |
| policy.alignment | mixed | mixed | same |
| import_dependence (sn_import_dependence, a percentile) | 0.05108 | 0.0511 | same |
| import_hhi (sn_supplier_concentration, a percentile) | 0.8202 | 0.82 | same |
| china_bloc (sn_risky_supplier_exposure, a percentile) | 0.7511 | 0.751 | same |
| trusted_alternatives | 0 | 0 | same |
| top_supplier | GAB | GAB | same |
| top_supplier_share | 0.4544 | 0.454 | same |
| china_share | 0.05922 | 0.0592 | same |
| opposite_bloc_share | 0.06406 | 0.0641 | same |
| imports_to_gdp | 1.371e-05 | 1.37e-05 | same |
| trusted_share | 0 | 0 | same |
| coercive_top | CHN | CHN | same |
| coercive_top_share | 0.8043 | 0.804 | same |
| coercive_import_share | 0.06406 | 0.0641 | same |
| trade.benchmark_share | 0.4405 | 0.441 | same |
| trade.benchmark_rca | 25.21 | 25.2 | same |
| trade.momentum_score | 0.4444 | 0.444 | same |
| trade.benchmark_exports | 8,219,361,015 | 8,220,000,000 | same |
| current_trade.weight | 0.5 | 0.5 | same |
| current_trade.china_share_base | 0.01884 | 0.0188 | same |
| current_trade.china_share_latest | 0.09961 | 0.0996 | same |
| cp_families.export_position | 0.8619 | 0.862 | same |
| cp_families.production_position | 0.8386 | 0.839 | same |
| cp_families.resource_endowment | 0.8025 | 0.802 | same |

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
- 2. partner rows: self-trade (re-import) imports, USD: differs (import_partner_cells_tbl.rds lists the importer as its own supplier (0.0005583 of its partner rows); import_exposure_tbl.rds leaves these out, but sp_partner_recs() diversification and the partnership memo read the partner table as it is)
- 2. home_share (world supply share) <- production / supply structure: not reproducible from shipped/pipeline data (the supply structure is assembled inside scripts/26 (Atlas trade shares, USGS / EI production, GCIM) and not saved per source; 2 production-table rows shown for context)
- 2. trade benchmark <- Comtrade energy trade rows: not reproducible from shipped/pipeline data (the 2022-24 benchmark is computed inside scripts/26 from the raw Comtrade pull; no processed table holds its rows)
- 4. measure-level percentiles (raw value -> measure percentile): not reproducible from shipped/pipeline data (measure values other than import HHI, China and opposite-bloc shares are not stored per cell (index_outputs contributions))
- 5. sn_import_dependence <- deficit_gdp / production surplus percentiles: not reproducible from shipped/pipeline data (its measures live in index_outputs.rds contributions, not in the cell table)
- 5. io_node_prize <- WEO / minerals demand growth and capex momentum: not reproducible from shipped/pipeline data (node-level growth percentiles are ranked across the 55 producing nodes inside scripts/26)
- 5. CP family values <- pillar variable contributions: not reproducible from shipped/pipeline data (variable contributions live in index_outputs.rds, not in the cell table)
- 7. pw_* signals <- GCIM pipeline, capacity and input-cost side tables: not reproducible from shipped/pipeline data (sp_pathway() builds the signals from side tables scripts/26 does not save; only the thresholding is re-run)
- 7. capability lower bound (Secure engine Test 3): not reproducible from shipped/pipeline data (the posture engine is not rerun by the audit (its inputs are in strategic_position_inputs.rds))

