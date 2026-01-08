# Sources

The pipeline draws from multiple datasets. The legacy implementation references the following (non-exhaustive) sources and vintages:

- **World Bank WDI** (GDP and macro indicators).
- **Energy Institute Statistical Review** (`ei_stat_review_world_energy.csv`).
- **BloombergNEF New Energy Outlook 2024** (`2024-10-29 - New Energy Outlook 2024.csv`).
- **IEA Critical Minerals** (`iea_criticalminerals_25.csv`).
- **Yao et al. supply chain inputs** (`yao_etal_nature_supplychains.xlsx`).
- **USGS Mineral Commodity Summaries** (`MCS2025_World_Data.csv`).
- **IEA cleantech midstream capacity** (`iea_cleantech_Midstream.csv`).
- **EV midstream capacity** (`ev_Midstream_capacity.csv`).
- **Greenplexity export competitiveness** (`greenplexity_scrape_full.csv`).
- **BACI/Atlas of Economic Complexity trade data** (`hs92_country_product_year_4.csv`, `hs92_country_product_year_6.csv`).
- **Comtrade critical minerals dashboard** (`unique_comtrade.csv`).
- **BNEF LCOE data viewer** (`2025-03-24 - 2025 LCOE Data Viewer Tool.csv`).
- **ILO earnings data** (ILO API export for average monthly earnings).
- **BIS debt service ratios** (`bis_debtserviceratio.csv`).
- **IMF lending rates + PPI** (`imf_lending_rates.csv`, `imf_ppi.csv`).
- **WIPO indicator trends** (`wipo_treated_indicator_trends_2025.csv`).
- **BNEF Energy Transition Supply Chains 2025** (`BNEF_Energy Transition Supply Chains 2025.xlsx`).
- **IEA WEO 2024 Annex A** (`WEO2024_AnnexA_Free_Dataset_World.csv`).
- **IEA EV Data Explorer 2025** (`IEA_EVDataExplorer2025.xlsx`).
- **BCG market size** (`Market Size for Technology and Supply Chain.xlsx`).
- **Clean energy foreign policy heatmap** (`clean_energy_foreign_policy_heatmap.xlsx`).
- **IPCC GHG intensity** (`ipcc_ghg_intensity.csv`).
- **Climate Action Tracker** (`CAT_country ratings data.csv`).
- **World Bank Doing Business + WDI extracts** (`wb_doingbusiness.csv`, `wb_wdi.csv`).
- **OECD development assistance** (`oecd_1215.csv`, `oecd_1518.csv`, `oecd_1823.csv`).
- **NAICS clean/fossil crosswalks** (`clean_industry_naics_test.xlsx`, `fossil_industry_naics.xlsx`).
- **US EPA NEI facility-level emissions** (`2020 Facility-Level Data for Point  Emissions ... .xlsx`).
- **GHG emissions by fuel** (`ghg_ems_fossil_combustion_fuel.csv`).

Sources enter the pipeline during ingestion (`scripts/05_ingest_sources.R`), then flow into theme and index construction.

## Manifest-driven ingestion

Raw inputs are allowlisted in `config/raw_inputs_manifest.yml`. The ingestion step copies only the files listed in that manifest from the SharePoint/OneDrive staging area (`sharepoint_raw_dir` in `config/config.yml`) into the snapshot folder at `data/raw/<snapshot_date>/`, preserving subfolder structure. Optional entries in the manifest (for legacy-generated caches like `allied_comtrade_energy_data.csv` and `scatter_index.csv`) do not fail ingestion when missing.

When legacy scripts change, regenerate the manifest by running:

```
Rscript scripts/01_generate_raw_inputs_manifest.R
```

Commit the updated `config/raw_inputs_manifest.yml` alongside the script changes so ingestion stays in sync with the legacy raw-input references.
