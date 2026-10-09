# Sources

<!-- GENERATED FILE - do not edit by hand.
     Regenerate with: Rscript scripts/02_render_sources_doc.R
     Edit config/raw_inputs_manifest.yml instead. -->

Every raw input the pipeline reads, grouped by how it arrives. There are 56 inputs in total.

- **api** — fetched automatically during ingestion; no human step.
- **manual** — staged by hand into `sharepoint_raw_dir` before a run.
- **derived** — authored by the project, not fetched from anywhere.
- **generated** — written by the pipeline as bookkeeping.

### Automated (api)

Fetched programmatically; no human step. Comtrade requires `COMTRADE_API_KEY`. Entries showing a fetch policy also have a staged copy: `prefer` means the API is the authority, `fallback` means the staged file wins and the fetcher only runs when no local copy exists.

| Source | File | Cadence | Vintage in use | Owner | Read by |
| --- | --- | --- | --- | --- | --- |
| [IMF lending rates (MFS_IR)](https://data.imf.org/en/Data-Explorer) | `imf_lending_rates.csv`<br>fetch: fallback (staged copy wins) | monthly | — | lcarey@rmi.org | `10_build_themes.R`, `40_build_index_vintages.R` |
| [IMF Primary Commodity Price System (wide export, includes annual YoY series)](https://www.imf.org/en/research/commodity-prices) | `imf_commodity_prices.csv`<br>fetch: prefer (API wins) | monthly | — | lcarey@rmi.org | `10_build_themes.R`, `40_build_index_vintages.R` |
| [IMF producer price indices (PPI)](https://data.imf.org/en/Data-Explorer) | `imf_ppi.csv`<br>fetch: fallback (staged copy wins) | monthly | — | lcarey@rmi.org | `10_build_themes.R`, `40_build_index_vintages.R` |
| [UN Comtrade (all-reporter energy HS6 reporter-partner dyads)](https://comtradeplus.un.org/) | `comtrade_energy_dyads_all.csv` | annual | 2020 and 2024; main pull 2026-10-07/08 (298 codes), supplement 2026-10-09 (740200 740311 760110 760120) | lcarey@rmi.org | `98_pull_comtrade_energy_dyads.R` |
| [UN Comtrade (allied reporter-partner dyads)](https://comtradeplus.un.org/) | `allied_comtrade_energy_data.csv` | annual | — | lcarey@rmi.org | `05_ingest_sources.R`, `15_build_partner_themes.R` |
| [UN Comtrade (critical minerals exports)](https://comtradeplus.un.org/) | `critmin_export_2024.csv`<br>newest match: `^critmin_export_\d{4}\.csv$` | annual | — | lcarey@rmi.org | `10_build_themes.R` |
| [UN Comtrade (critical minerals imports)](https://comtradeplus.un.org/) | `critmin_import_2024.csv`<br>newest match: `^critmin_import_\d{4}\.csv$` | annual | — | lcarey@rmi.org | `10_build_themes.R` |
| [UN Comtrade (critical minerals total exports)](https://comtradeplus.un.org/) | `critmin_total_export_2024.csv`<br>newest match: `^critmin_total_export_\d{4}\.csv$` | annual | — | lcarey@rmi.org | `10_build_themes.R` |
| [UN Comtrade (energy-tech HS6 exports and imports)](https://comtradeplus.un.org/) | `comtrade_energy_trade.csv` | annual | 2024; staged 2026-09-30 (228 codes), plus the World rows of 740200 740311 760110 760120 from the 2026-10-09 supplementary dyad pull | lcarey@rmi.org | `05_ingest_sources.R`, `10_build_themes.R`, `98_pull_comtrade_energy_dyads.R` |
| [UN Comtrade (partner-reported imports from non-reporting economies, mirror exports)](https://comtradeapi.un.org/public/v1/preview/C/A/HS) | `critmin_mirror_import.csv`<br>fetch: prefer (API wins) | annual | — | lcarey@rmi.org | `10_build_themes.R` |
| [UN Comtrade (total exports, TOTAL commodity code)](https://comtradeplus.un.org/) | `comtrade_total_export.csv` | annual | — | lcarey@rmi.org | `05_ingest_sources.R`, `10_build_themes.R` |
| [UN General Assembly ideal points, all-votes series (Bailey, Strezhnev and Voeten 2017; Voeten et al., Harvard Dataverse)](https://doi.org/10.7910/DVN/LEJUQZ) | `un_ga_ideal_points_all.csv`<br>fetch: prefer (API wins) | annual | Dataverse V39.0 (2026-07-30), IdealpointsJuly2025.tab (idealpointall), sessions 1-78 (1946-2023) | lcarey@rmi.org | `12_build_geopolitical_alignment.R` |
| [UN General Assembly ideal points, final-passage series (Bailey, Strezhnev and Voeten 2017; Voeten et al., Harvard Dataverse)](https://doi.org/10.7910/DVN/LEJUQZ) | `un_ga_ideal_points.csv`<br>fetch: prefer (API wins) | annual | Dataverse V39.0 (2026-07-30), IdealpointestimatesFP_2026FP.csv, sessions 1-80 (1946-2025) | lcarey@rmi.org | `12_build_geopolitical_alignment.R` |
| [World Bank Doing Business indicators (historical)](https://archive.doingbusiness.org/en/data) | `wb_doingbusiness.csv`<br>fetch: prefer (API wins) | discontinued | — | lcarey@rmi.org | `15_build_partner_themes.R` |
| [World Bank WDI country reference table](https://databank.worldbank.org/source/world-development-indicators) | `wdi_country_info.csv` | annual | — | lcarey@rmi.org | `05_ingest_sources.R`, `10_build_themes.R`, `15_build_partner_themes.R`, `30_build_allied_network_design.R`, `40_build_index_vintages.R` |
| [World Bank WDI development indicator panel](https://databank.worldbank.org/source/world-development-indicators) | `wb_wdi.csv`<br>fetch: prefer (API wins) | annual | — | lcarey@rmi.org | `15_build_partner_themes.R` |
| [World Bank World Development Indicators (NY.GDP.MKTP.CD)](https://databank.worldbank.org/source/world-development-indicators) | `wdi_gdp.csv` | annual | — | lcarey@rmi.org | `05_ingest_sources.R`, `10_build_themes.R`, `12_build_geopolitical_alignment.R`, `13_build_coalition.R`, `15_build_partner_themes.R`, `30_build_allied_network_design.R` |

### Manual staging required

Each of these must be downloaded and placed in `sharepoint_raw_dir` before a run. Ingestion compares size and mtime, so replacing a file with a newer vintage is enough; entries with a pattern also pick up a renamed release automatically.

| Source | File | Cadence | Vintage in use | Owner | Read by |
| --- | --- | --- | --- | --- | --- |
| BCG market sizing for technology and supply chain | `Market Size for Technology and Supply Chain.xlsx` | ad-hoc | — | lcarey@rmi.org | `10_build_themes.R` |
| [BloombergNEF Energy Transition Supply Chains](https://about.bnef.com/insights/clean-energy/new-energy-outlook/) | `BNEF_Energy Transition Supply Chains 2025.xlsx`<br>newest match: `^BNEF_Energy Transition Supply Chains \d{4}\.xlsx$` | annual | 2025 | lcarey@rmi.org | `10_build_themes.R` |
| [BloombergNEF LCOE Data Viewer](https://about.bnef.com/insights/clean-energy/new-energy-outlook/) | `2026-08-11 - LCOE Data.xlsb`<br>newest match: `^\d{4}-\d{2}-\d{2} - .*LCOE Data\.xlsb$\|^\d{4}-\d{2}-\d{2} - \d{4} LCOE Data Viewer Tool\.csv$` | semiannual | 2026-08-11 | lcarey@rmi.org | `10_build_themes.R` |
| [BloombergNEF New Energy Outlook](https://about.bnef.com/insights/clean-energy/new-energy-outlook/) | `2024-10-29 - New Energy Outlook 2024.csv`<br>newest match: `^\d{4}-\d{2}-\d{2} - New Energy Outlook \d{4}\.csv$` | annual | 2024-10-29 | lcarey@rmi.org | `10_build_themes.R`, `40_build_index_vintages.R` |
| [Clean Investment Monitor investment and capacity (aggregated)](https://www.cleaninvestmentmonitor.org/) | `GCIM_Investment_Capacity_aggregated.xlsx` | quarterly | — | lcarey@rmi.org | `10_build_themes.R`, `40_build_index_vintages.R` |
| [Climate Action Tracker country ratings](https://climateactiontracker.org/countries/) | `CAT_country ratings data.csv` | irregular | — | lcarey@rmi.org | `10_build_themes.R`, `15_build_partner_themes.R` |
| [Columbia University Critical Minerals Dashboard HS mapping](https://www.energypolicy.columbia.edu/critical-materials/) | `Columbia University Critical Minerals Dashboard/unique_comtrade.csv` | irregular | — | lcarey@rmi.org | `05_ingest_sources.R` |
| Economies without a UN General Assembly vote and the alignment each takes (sovereign's bloc for territories and SARs, neutral for Taiwan, Palestine and Kosovo) | `territory_sovereign_map.csv` | ad-hoc | project-authored 2026-10-08 | lcarey@rmi.org | `12_build_geopolitical_alignment.R` |
| [Energy Institute Statistical Review of World Energy (long format)](https://www.energyinst.org/statistical-review) | `ei_stat_review_world_energy.csv` | annual | — | lcarey@rmi.org | `10_build_themes.R`, `13_build_coalition.R`, `40_build_index_vintages.R` |
| [Energy Institute Statistical Review of World Energy (workbook)](https://www.energyinst.org/statistical-review) | `ei_stat_review_world_energy_wide.xlsx` | annual | — | lcarey@rmi.org | `10_build_themes.R`, `13_build_coalition.R` |
| Geothermal LCOE and capacity potential extract | `geothermal_lcoe_mw.csv` | irregular | — | lcarey@rmi.org | `10_build_themes.R` |
| [Global Solar Atlas country potential](https://globalsolaratlas.info) | `solar_potential_clean.csv` | irregular | — | lcarey@rmi.org | `10_build_themes.R` |
| [Global Trade Alert New Industrial Policy Observatory](https://www.globaltradealert.org/) | `GTA NIPO - February 2026.xlsx`<br>newest match: `^GTA NIPO - .*\.xlsx$` | monthly | February 2026 | lcarey@rmi.org | `10_build_themes.R` |
| [Global Wind Atlas country potential](https://globalwindatlas.info/) | `wb_wind_country.csv` | irregular | — | lcarey@rmi.org | `10_build_themes.R` |
| [Harvard Atlas of Economic Complexity (HS92 6-digit product-year totals)](https://atlas.hks.harvard.edu/) | `hs92_product_year_6.csv` | annual | — | lcarey@rmi.org | `04_build_hs6_views.R` |
| [Harvard Atlas of Economic Complexity (HS92, 4-digit)](https://atlas.hks.harvard.edu/) | `hs92_country_product_year_4.csv` | annual | — | lcarey@rmi.org | `10_build_themes.R`, `40_build_index_vintages.R` |
| [Harvard Atlas of Economic Complexity (HS92, 6-digit)](https://atlas.hks.harvard.edu/) | `hs92_country_product_year_6.csv` | annual | — | lcarey@rmi.org | `10_build_themes.R`, `40_build_index_vintages.R` |
| [IEA clean technology midstream manufacturing capacity](https://www.iea.org/data-and-statistics/data-tools/etp-clean-energy-technology-guide) | `iea_cleantech_Midstream.csv` | annual | — | lcarey@rmi.org | `10_build_themes.R`, `13_build_coalition.R` |
| [IEA Critical Minerals Dataset](https://www.iea.org/data-and-statistics/data-product/critical-minerals-dataset) | `IEA Critical Minerals Dataset 2026.xlsx`<br>newest match: `^IEA Critical Minerals Dataset \d{4}\.xlsx$\|^iea_criticalminerals_\d{2}\.csv$` | annual | 2026 | lcarey@rmi.org | `05_ingest_sources.R`, `10_build_themes.R`, `40_build_index_vintages.R` |
| [IEA ETP Clean Energy Technology Guide](https://www.iea.org/data-and-statistics/data-tools/etp-clean-energy-technology-guide) | `IEACleanTechGuidepublicdataset.csv`<br>newest match: `^IEACleanTechGuide.*\.csv$\|^IEA_Clean_Tech_Guide\.csv$` | annual | 2026 | lcarey@rmi.org | `10_build_themes.R` |
| [IEA EV midstream manufacturing capacity extract](https://www.iea.org/data-and-statistics/data-tools/global-ev-data-explorer) | `ev_Midstream_capacity.csv` | annual | — | lcarey@rmi.org | `10_build_themes.R` |
| [IEA Global EV Outlook](https://www.iea.org/data-and-statistics/data-product/global-ev-outlook-2026) | `EV data by country 2026.csv`<br>newest match: `^EV data by country \d{4}\.csv$\|^IEA_EVDataExplorer\d{4}\.xlsx$` | annual | 2026 | lcarey@rmi.org | `10_build_themes.R` |
| [IEA Policies and Measures database export](https://www.iea.org/policies) | `IEA_PAMS_Export.csv` | continuous | — | lcarey@rmi.org | `10_build_themes.R` |
| [IEA relative technology cost extract](https://www.iea.org/data-and-statistics) | `Relative_Costs_IEA.csv` | irregular | — | lcarey@rmi.org | `10_build_themes.R`, `40_build_index_vintages.R` |
| [IEA World Energy Outlook Annex A free dataset](https://www.iea.org/reports/world-energy-outlook-2025) | `WEO2025_AnnexA_Free_Dataset_World.csv`<br>newest match: `^WEO\d{4}_AnnexA_Free_Dataset_World\.csv$` | annual | 2025 | lcarey@rmi.org | `10_build_themes.R` |
| [IMF Coordinated Direct Investment Survey positions](https://data.imf.org/en/Data-Explorer) | `imf_dip.csv` | annual | — | lcarey@rmi.org | `15_build_partner_themes.R` |
| [IPCC lifecycle GHG intensity of electricity supply technologies (AR5 WGIII Annex III, Table A.III.2)](https://www.ipcc.ch/site/assets/uploads/2018/02/ipcc_wg3_ar5_annex-iii.pdf) | `ipcc_ghg_intensity.csv` | irregular | AR5 (2014) | lcarey@rmi.org | `10_build_themes.R`, `15_build_partner_themes.R` |
| [Membership of U.S. and allied economic-security agreements (U.S. FTA partners, IPEF, Minerals Security Partnership and MSP Forum, G7, NATO)](https://ustr.gov/trade-agreements/free-trade-agreements) | `economic_security_agreements.csv` | ad-hoc | accessed 2026-10-08 | lcarey@rmi.org | `13_build_coalition.R` |
| [OECD-NEA/IAEA Uranium Resources, Production and Demand (Red Book), Table 1.18 historical uranium production](https://www.oecd-nea.org/jcms/pl_121658/uranium-2026-resources-production-and-demand) | `uranium_production.csv` | biennial | Uranium 2026 (NEA No. 7770) for 2022-2024, Uranium 2024 (NEA No. 7683) for 2020-2021, Uranium 2022 (NEA No. 7634) for 2019; accessed 2026-10-07 | lcarey@rmi.org | `10_build_themes.R`, `13_build_coalition.R` |
| [OECD-NEA/IAEA Uranium Resources, Production and Demand (Red Book), Table 1.2a identified recoverable resources](https://www.oecd-nea.org/jcms/pl_121658/uranium-2026-resources-production-and-demand) | `uranium_resources.csv` | biennial | Uranium 2026 (NEA No. 7770), resources as of 1 January 2025; accessed 2026-10-07 | lcarey@rmi.org | `10_build_themes.R`, `13_build_coalition.R` |
| [OECD Creditor Reporting System aid flows](https://data-explorer.oecd.org/) | `oecd_crs_api.csv` | annual | — | lcarey@rmi.org | `15_build_partner_themes.R` |

### Project-authored crosswalks (derived)

Maintained by the team rather than fetched. Entries marked `staged_from: repo` live in `data/reference/` under version control, so a fresh clone can build without access to anyone's OneDrive; ingestion copies them into `data/raw/`.

| Source | File | Cadence | Vintage in use | Owner | Read by |
| --- | --- | --- | --- | --- | --- |
| Allied reporter list for Comtrade dyad pulls | `allies.csv` | ad-hoc | — | lcarey@rmi.org | `05_ingest_sources.R` |
| Dual-use scoring by primary/secondary/tertiary category | `dual_use_scores_primary_secondary_tertiary.csv` | ad-hoc | — | lcarey@rmi.org | `10_build_themes.R` |
| HS6 to technology / supply-chain view (tech, supply_chain, sub_sector, HS6) | `consolidated_hs6_energy_tech_long.csv` | ad-hoc | — | lcarey@rmi.org | `10_build_themes.R` |
| HS6 to technology / supply-chain view (Technology, Value Chain, Sub.Sector, HS6) | `hts_codes_categories_bolstered_final.csv` | ad-hoc | — | lcarey@rmi.org | `05_ingest_sources.R`, `10_build_themes.R`, `40_build_index_vintages.R`, `96_pull_trade_timeseries.R`, `97_bulk_comtrade_downloads.R` |
| HS6 view carrying the essential-goods flag (Technology, Value Chain, Sub.Sector, HS6, essential) | `hs6_categories_with_essential.csv` | ad-hoc | — | lcarey@rmi.org | `10_build_themes.R`, `96_pull_trade_timeseries.R`, `97_bulk_comtrade_downloads.R` |
| Master energy HS6 catalogue | `energy_hs6_master.csv` | ad-hoc | — | lcarey@rmi.org | `trade_charts.R`, `15_build_partner_themes.R` |

### Pipeline bookkeeping (generated)

Written by the pipeline to record what a given run used.

| Source | File | Cadence | Vintage in use | Owner | Read by |
| --- | --- | --- | --- | --- | --- |
| Comtrade retrieval vintage stamp | `comtrade_vintage.yml` | per-run | — | lcarey@rmi.org | `05_ingest_sources.R`, `10_build_themes.R` |
| Resolved vintage stamp for pattern-matched inputs | `raw_inputs_resolved.yml` | per-run | — | lcarey@rmi.org | — |

## Coverage gaps

- Inputs with no named owner: **0**
- Inputs with `source_type: unknown`: **0**

## Refreshing

```bash
# re-copy every staged input even when mtimes look current
OPSI_FORCE_REFRESH=true Rscript scripts/05_ingest_sources.R
```

Ingestion is mtime- and size-aware, so a newer file in the staging area replaces the
local copy automatically. Pattern-matched inputs resolve to the newest available
vintage, and `data/raw/raw_inputs_resolved.yml` records what each run actually used.

