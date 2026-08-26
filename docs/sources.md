# Sources

<!-- GENERATED FILE - do not edit by hand.
     Regenerate with: Rscript scripts/02_render_sources_doc.R
     Edit config/raw_inputs_manifest.yml instead. -->

Every raw input the pipeline reads, grouped by how it arrives. There are 51 inputs in total.

- **api** â fetched automatically during ingestion; no human step.
- **manual** â staged by hand into `sharepoint_raw_dir` before a run.
- **derived** â authored by the project, not fetched from anywhere.
- **generated** â written by the pipeline as bookkeeping.

### Automated (api)

Fetched programmatically; no human step. Comtrade requires `COMTRADE_API_KEY`. Entries showing a fetch policy also have a staged copy: `prefer` means the API is the authority, `fallback` means the staged file wins and the fetcher only runs when no local copy exists.

| Source | File | Cadence | Vintage in use | Owner | Read by |
| --- | --- | --- | --- | --- | --- |
| [IMF lending rates (MFS_IR)](https://data.imf.org/en/Data-Explorer) | `imf_lending_rates.csv`<br>fetch: fallback (staged copy wins) | monthly | â | lcarey@rmi.org | `10_build_themes.R` |
| [IMF PCPS derived volatility by commodity series](https://www.imf.org/en/research/commodity-prices) | `imf_pcps_price_volatility_series.csv` | monthly | â | lcarey@rmi.org | `05_ingest_sources.R` |
| [IMF PCPS derived volatility by technology](https://www.imf.org/en/research/commodity-prices) | `imf_pcps_price_volatility.csv` | monthly | â | lcarey@rmi.org | `05_ingest_sources.R` |
| [IMF Primary Commodity Price System (monthly price panel)](https://www.imf.org/en/research/commodity-prices) | `imf_pcps_prices.csv` | monthly | â | lcarey@rmi.org | `05_ingest_sources.R`, `10_build_themes.R` |
| [IMF producer price indices (PPI)](https://data.imf.org/en/Data-Explorer) | `imf_ppi.csv`<br>fetch: fallback (staged copy wins) | monthly | â | lcarey@rmi.org | `10_build_themes.R` |
| [UN Comtrade (allied reporter-partner dyads)](https://comtradeplus.un.org/) | `allied_comtrade_energy_data.csv` | annual | â | lcarey@rmi.org | `05_ingest_sources.R`, `15_build_partner_themes.R` |
| [UN Comtrade (critical minerals exports)](https://comtradeplus.un.org/) | `critmin_export_2024.csv`<br>newest match: `^critmin_export_\d{4}\.csv$` | annual | â | lcarey@rmi.org | `10_build_themes.R` |
| [UN Comtrade (critical minerals imports)](https://comtradeplus.un.org/) | `critmin_import_2024.csv`<br>newest match: `^critmin_import_\d{4}\.csv$` | annual | â | lcarey@rmi.org | `10_build_themes.R` |
| [UN Comtrade (critical minerals total exports)](https://comtradeplus.un.org/) | `critmin_total_export_2024.csv`<br>newest match: `^critmin_total_export_\d{4}\.csv$` | annual | â | lcarey@rmi.org | `10_build_themes.R` |
| [UN Comtrade (energy-tech HS6 exports and imports)](https://comtradeplus.un.org/) | `comtrade_energy_trade.csv` | annual | â | lcarey@rmi.org | `05_ingest_sources.R`, `10_build_themes.R` |
| [UN Comtrade (total exports, TOTAL commodity code)](https://comtradeplus.un.org/) | `comtrade_total_export.csv` | annual | â | lcarey@rmi.org | `05_ingest_sources.R`, `10_build_themes.R` |
| [World Bank Doing Business indicators (historical)](https://archive.doingbusiness.org/en/data) | `wb_doingbusiness.csv`<br>fetch: prefer (API wins) | discontinued | â | lcarey@rmi.org | `15_build_partner_themes.R` |
| [World Bank WDI country reference table](https://databank.worldbank.org/source/world-development-indicators) | `wdi_country_info.csv` | annual | â | lcarey@rmi.org | `05_ingest_sources.R`, `10_build_themes.R`, `15_build_partner_themes.R`, `30_build_allied_network_design.R` |
| [World Bank WDI development indicator panel](https://databank.worldbank.org/source/world-development-indicators) | `wb_wdi.csv`<br>fetch: prefer (API wins) | annual | â | lcarey@rmi.org | `15_build_partner_themes.R` |
| [World Bank World Development Indicators (NY.GDP.MKTP.CD)](https://databank.worldbank.org/source/world-development-indicators) | `wdi_gdp.csv` | annual | â | lcarey@rmi.org | `05_ingest_sources.R`, `10_build_themes.R`, `15_build_partner_themes.R`, `30_build_allied_network_design.R` |

### Manual staging required

Each of these must be downloaded and placed in `sharepoint_raw_dir` before a run. Ingestion compares size and mtime, so replacing a file with a newer vintage is enough; entries with a pattern also pick up a renamed release automatically.

| Source | File | Cadence | Vintage in use | Owner | Read by |
| --- | --- | --- | --- | --- | --- |
| BCG market sizing for technology and supply chain | `Market Size for Technology and Supply Chain.xlsx` | ad-hoc | â | lcarey@rmi.org | `10_build_themes.R` |
| [BloombergNEF Energy Transition Supply Chains](https://about.bnef.com/insights/clean-energy/new-energy-outlook/) | `BNEF_Energy Transition Supply Chains 2025.xlsx`<br>newest match: `^BNEF_Energy Transition Supply Chains \d{4}\.xlsx$` | annual | 2025 | lcarey@rmi.org | `10_build_themes.R` |
| [BloombergNEF LCOE Data Viewer](https://about.bnef.com/insights/clean-energy/new-energy-outlook/) | `2025-03-24 - 2025 LCOE Data Viewer Tool.csv`<br>newest match: `^\d{4}-\d{2}-\d{2} - \d{4} LCOE Data Viewer Tool\.csv$` | semiannual | 2025-03-24 | lcarey@rmi.org | `10_build_themes.R` |
| [BloombergNEF New Energy Outlook](https://about.bnef.com/insights/clean-energy/new-energy-outlook/) | `2024-10-29 - New Energy Outlook 2024.csv`<br>newest match: `^\d{4}-\d{2}-\d{2} - New Energy Outlook \d{4}\.csv$` | annual | 2024-10-29 | lcarey@rmi.org | `10_build_themes.R` |
| [Clean Investment Monitor investment and capacity (aggregated)](https://www.cleaninvestmentmonitor.org/) | `GCIM_Investment_Capacity_aggregated.xlsx` | quarterly | â | lcarey@rmi.org | `10_build_themes.R` |
| [Climate Action Tracker country ratings](https://climateactiontracker.org/countries/) | `CAT_country ratings data.csv` | irregular | â | lcarey@rmi.org | `10_build_themes.R`, `15_build_partner_themes.R` |
| [Columbia University Critical Minerals Dashboard HS mapping](https://www.energypolicy.columbia.edu/critical-materials/) | `Columbia University Critical Minerals Dashboard/unique_comtrade.csv` | irregular | â | lcarey@rmi.org | `05_ingest_sources.R` |
| [Energy Institute Statistical Review of World Energy (long format)](https://www.energyinst.org/statistical-review) | `ei_stat_review_world_energy.csv` | annual | â | lcarey@rmi.org | `10_build_themes.R` |
| [Energy Institute Statistical Review of World Energy (workbook)](https://www.energyinst.org/statistical-review) | `ei_stat_review_world_energy_wide.xlsx` | annual | â | lcarey@rmi.org | `10_build_themes.R` |
| Geothermal LCOE and capacity potential extract | `geothermal_lcoe_mw.csv` | irregular | â | lcarey@rmi.org | `10_build_themes.R` |
| [Global Solar Atlas country potential](https://globalsolaratlas.info) | `solar_potential_clean.csv` | irregular | â | lcarey@rmi.org | `10_build_themes.R` |
| [Global Trade Alert New Industrial Policy Observatory](https://www.globaltradealert.org/) | `GTA NIPO - February 2026.xlsx`<br>newest match: `^GTA NIPO - .*\.xlsx$` | monthly | February 2026 | lcarey@rmi.org | `10_build_themes.R` |
| [Global Wind Atlas country potential](https://globalwindatlas.info/) | `wb_wind_country.csv` | irregular | â | lcarey@rmi.org | `10_build_themes.R` |
| [Harvard Atlas of Economic Complexity (HS92, 4-digit)](https://atlas.hks.harvard.edu/) | `hs92_country_product_year_4.csv` | annual | â | lcarey@rmi.org | `10_build_themes.R` |
| [Harvard Atlas of Economic Complexity (HS92, 6-digit)](https://atlas.hks.harvard.edu/) | `hs92_country_product_year_6.csv` | annual | â | lcarey@rmi.org | `10_build_themes.R` |
| [IEA clean technology midstream manufacturing capacity](https://www.iea.org/data-and-statistics/data-tools/etp-clean-energy-technology-guide) | `iea_cleantech_Midstream.csv` | annual | â | lcarey@rmi.org | `10_build_themes.R` |
| [IEA Critical Minerals Dataset](https://www.iea.org/data-and-statistics/data-product/critical-minerals-dataset) | `iea_criticalminerals_25.csv`<br>newest match: `^iea_criticalminerals_\d{2}\.csv$` | annual | 2025 | lcarey@rmi.org | `05_ingest_sources.R`, `10_build_themes.R` |
| [IEA ETP Clean Energy Technology Guide](https://www.iea.org/data-and-statistics/data-tools/etp-clean-energy-technology-guide) | `IEA_Clean_Tech_Guide.csv` | annual | â | lcarey@rmi.org | `10_build_themes.R` |
| [IEA EV midstream manufacturing capacity extract](https://www.iea.org/data-and-statistics/data-tools/global-ev-data-explorer) | `ev_Midstream_capacity.csv` | annual | â | lcarey@rmi.org | `10_build_themes.R` |
| [IEA Global EV Data Explorer](https://www.iea.org/data-and-statistics/data-tools/global-ev-data-explorer) | `IEA_EVDataExplorer2025.xlsx`<br>newest match: `^IEA_EVDataExplorer\d{4}\.xlsx$` | annual | 2025 | lcarey@rmi.org | `10_build_themes.R` |
| [IEA Policies and Measures database export](https://www.iea.org/policies) | `IEA_PAMS_Export.csv` | continuous | â | lcarey@rmi.org | `10_build_themes.R` |
| [IEA relative technology cost extract](https://www.iea.org/data-and-statistics) | `Relative_Costs_IEA.csv` | irregular | â | lcarey@rmi.org | `10_build_themes.R` |
| [IEA World Energy Outlook Annex A free dataset](https://www.iea.org/reports/world-energy-outlook-2025) | `WEO2025_AnnexA_Free_Dataset_World.csv`<br>newest match: `^WEO\d{4}_AnnexA_Free_Dataset_World\.csv$` | annual | 2025 | lcarey@rmi.org | `10_build_themes.R` |
| [IMF commodity prices (wide export, includes annual YoY series)](https://www.imf.org/en/research/commodity-prices) | `imf_commodity_prices.csv` | monthly | â | lcarey@rmi.org | `10_build_themes.R` |
| [IMF Coordinated Direct Investment Survey positions](https://data.imf.org/en/Data-Explorer) | `imf_dip.csv` | annual | â | lcarey@rmi.org | `15_build_partner_themes.R` |
| [IMF Primary Commodity Price System full workbook](https://www.imf.org/en/research/commodity-prices) | `IMF_PCPS_all.xlsx` | monthly | â | lcarey@rmi.org | `05_ingest_sources.R` |
| [IPCC technology GHG intensity factors](https://www.ipcc.ch/report/ar6/wg3/) | `ipcc_ghg_intensity.csv` | irregular | â | lcarey@rmi.org | `10_build_themes.R`, `15_build_partner_themes.R` |
| [OECD Creditor Reporting System aid flows](https://data-explorer.oecd.org/) | `oecd_crs_api.csv` | annual | â | lcarey@rmi.org | `15_build_partner_themes.R` |

### Project-authored crosswalks (derived)

Maintained by the team rather than fetched. Entries marked `staged_from: repo` live in `data/reference/` under version control, so a fresh clone can build without access to anyone's OneDrive; ingestion copies them into `data/raw/`.

| Source | File | Cadence | Vintage in use | Owner | Read by |
| --- | --- | --- | --- | --- | --- |
| Allied reporter list for Comtrade dyad pulls | `allies.csv` | ad-hoc | â | lcarey@rmi.org | `05_ingest_sources.R` |
| Dual-use scoring by primary/secondary/tertiary category | `dual_use_scores_primary_secondary_tertiary.csv` | ad-hoc | â | lcarey@rmi.org | `10_build_themes.R` |
| HS6 to technology / supply-chain view (tech, supply_chain, sub_sector, HS6) | `consolidated_hs6_energy_tech_long.csv` | ad-hoc | â | lcarey@rmi.org | `10_build_themes.R` |
| HS6 to technology / supply-chain view (Technology, Value Chain, Sub.Sector, HS6) | `hts_codes_categories_bolstered_final.csv` | ad-hoc | â | lcarey@rmi.org | `05_ingest_sources.R`, `10_build_themes.R`, `96_pull_trade_timeseries.R`, `97_bulk_comtrade_downloads.R` |
| HS6 view carrying the essential-goods flag (Technology, Value Chain, Sub.Sector, HS6, essential) | `hs6_categories_with_essential.csv` | ad-hoc | â | lcarey@rmi.org | `10_build_themes.R`, `96_pull_trade_timeseries.R`, `97_bulk_comtrade_downloads.R` |
| Master energy HS6 catalogue | `energy_hs6_master.csv` | ad-hoc | â | lcarey@rmi.org | `trade_charts.R`, `15_build_partner_themes.R` |

### Pipeline bookkeeping (generated)

Written by the pipeline to record what a given run used.

| Source | File | Cadence | Vintage in use | Owner | Read by |
| --- | --- | --- | --- | --- | --- |
| Comtrade retrieval vintage stamp | `comtrade_vintage.yml` | per-run | â | lcarey@rmi.org | `05_ingest_sources.R`, `10_build_themes.R` |
| Resolved vintage stamp for pattern-matched inputs | `raw_inputs_resolved.yml` | per-run | â | lcarey@rmi.org | â |

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

