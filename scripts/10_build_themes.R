source(local({
  resolve_bootstrap_path <- function() {
    candidate_starts <- character()

    sf <- tryCatch(sys.frame(1)$ofile, error = function(e) "")
    if (!is.null(sf) && nzchar(sf)) candidate_starts <- c(candidate_starts, dirname(sf))

    frame_ofiles <- vapply(sys.frames(), function(fr) {
      val <- tryCatch(fr$ofile, error = function(e) "")
      if (is.null(val) || !nzchar(val)) "" else dirname(val)
    }, character(1))
    candidate_starts <- c(candidate_starts, frame_ofiles[nzchar(frame_ofiles)])

    fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(fa) > 0) candidate_starts <- c(candidate_starts, dirname(sub("^--file=", "", fa[1])))

    candidate_starts <- unique(c(candidate_starts, getwd()))

    for (start in candidate_starts) {
      d <- normalizePath(start, winslash = "/", mustWork = FALSE)
      while (dirname(d) != d) {
        bootstrap <- file.path(d, "scripts", "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)

        bootstrap <- file.path(d, "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)

        d <- dirname(d)
      }
    }

    stop("Unable to resolve script path for bootstrap.")
  }

  resolve_bootstrap_path()
}))

source(file.path(repo_root, "scripts", "utils", "raw_inputs.R"))
source(file.path(repo_root, "scripts", "utils", "bnef_lcoe.R"))
source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "levels.R"))
source(file.path(repo_root, "R", "utils", "supply_chain_stages.R"))
source(file.path(repo_root, "R", "utils", "iea_critical_minerals.R"))
source(file.path(repo_root, "R", "utils", "iea_ev.R"))
source(file.path(repo_root, "R", "utils", "eia_intl.R"))
source(file.path(repo_root, "R", "categories", "shared", "overall_index.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(repo_root, "R", "categories", "policy", "iea_policy_index.R"))
source(file.path(repo_root, "R", "categories", "policy", "nipo_policy_index.R"))
source(file.path(repo_root, "R", "categories", "policy", "cat_policy_index.R"))
source(file.path(repo_root, "R", "categories", "policy", "dual_use_scores.R"))
source(file.path(repo_root, "R", "categories", "trade", "trade_core.R"))
source(file.path(repo_root, "R", "categories", "foreign_dependency", "critical_minerals_processing.R"))
source(file.path(repo_root, "R", "categories", "production", "critical_minerals_production.R"))
source(file.path(repo_root, "R", "categories", "minerals_trade", "critical_minerals_trade.R"))
source(file.path(repo_root, "R", "categories", "energy_access", "energy_access_consumption.R"))
source(file.path(repo_root, "R", "categories", "energy_access", "solar_pv_potential.R"))
source(file.path(repo_root, "R", "categories", "energy_access", "wind_potential.R"))
source(file.path(repo_root, "R", "categories", "energy_access", "geothermal_potential.R"))
source(file.path(repo_root, "R", "categories", "consumption", "energy_consumption.R"))
source(file.path(repo_root, "R", "categories", "energy_prices", "energy_prices.R"))
source(file.path(repo_root, "R", "categories", "foreign_dependency", "foreign_dependency.R"))
source(file.path(repo_root, "R", "categories", "energy_imports", "import_dependence.R"))
source(file.path(repo_root, "R", "categories", "reserves", "reserves.R"))
source(file.path(repo_root, "R", "categories", "reserves", "uranium.R"))
source(file.path(repo_root, "R", "categories", "reserves", "usgs_mcs.R"))
source(file.path(repo_root, "R", "categories", "trade", "trade_concentration.R"))
source(file.path(repo_root, "R", "categories", "technology_demand", "future_demand.R"))
source(file.path(repo_root, "R", "categories", "trade", "export_feasibility.R"))
source(file.path(repo_root, "R", "categories", "energy_prices", "lcoe_competitiveness.R"))
source(file.path(repo_root, "R", "categories", "foreign_dependency", "market_share_manufacturing.R"))
source(file.path(repo_root, "R", "categories", "production", "production_depth_momentum.R"))
source(file.path(repo_root, "R", "categories", "technology_demand", "overcapacity_premium.R"))
source(file.path(repo_root, "R", "categories", "investment", "investment_momentum.R"))
source(file.path(repo_root, "R", "categories", "economic opportunity", "cost_competitiveness.R"))
source(file.path(repo_root, "R", "categories", "technological_readiness", "technological_readiness.R"))

# rebuild_theme_overall_indices() and standardize_theme_types() moved to R/utils so the
# vintage builder (scripts/40_build_index_vintages.R) can reuse them.
source(file.path(repo_root, "R", "utils", "theme_standardize.R"))


config <- getOption("opportunity_security.config")
if (is.null(config)) {
  stop("Config not loaded; run scripts/00_setup.R first.")
}

processed_dir <- file.path(repo_root, config$processed_dir)
if (!dir.exists(processed_dir)) {
  dir.create(processed_dir, recursive = TRUE)
}

write_processed_tbl <- function(tbl, name, processed_dir) {
  if (is.null(tbl)) {
    return(invisible(NULL))
  }
  saveRDS(tbl, file.path(processed_dir, paste0(name, ".rds")))
  invisible(tbl)
}

is_skip_data_downloads <- function() {
  tolower(Sys.getenv("SKIP_DATA_DOWNLOADS")) %in% c("1", "true", "yes")
}
skip_data_downloads <- is_skip_data_downloads()

raw_data_dir <- config$raw_data_dir
if (is.null(raw_data_dir)) {
  stop("Config missing raw_data_dir.")
}

resolve_raw_data_dir <- function(root_dir, raw_data_dir, skip_data_downloads = FALSE) {
  raw_base_dir <- file.path(root_dir, raw_data_dir)
  if (!dir.exists(raw_base_dir)) {
    if (skip_data_downloads) {
      message("Skipping raw data lookup; directory not found: ", raw_base_dir)
      return(NULL)
    }
    stop("Raw data directory not found: ", raw_base_dir)
  }

  raw_base_dir
}

raw_data_path <- resolve_raw_data_dir(repo_root, raw_data_dir, skip_data_downloads)
if (is.null(raw_data_path)) {
  invisible(list())
  return()
}

manifest_path <- raw_inputs_manifest_path(repo_root)
raw_manifest <- read_raw_inputs_manifest(manifest_path)

# Assemble required raw file paths for theme builders.
#
# Inputs whose file name carries a release vintage are resolved to the newest match in
# data/raw rather than pinned to one release, so a new upstream publication flows in
# without a code edit. `fallback` keeps the previously pinned name working when no
# other candidate is present.
raw_path <- file.path(raw_data_path, "ei_stat_review_world_energy.csv")
# EIA International Energy Statistics fossil fuel production and consumption, staged by
# scripts/fetchers/eia_intl.R (manifest id eia_intl_fossil_production). It supplies the oil, gas
# and coal production EI does not report for an economy (R/utils/eia_intl.R; import dependence
# and production themes).
eia_intl_path <- file.path(raw_data_path, "eia_intl_fossil_production.csv")
reserves_excel_path <- file.path(raw_data_path, "ei_stat_review_world_energy_wide.xlsx")
# Critical-mineral reserves: USGS Mineral Commodity Summaries 2026, the commodity table of the
# ScienceBase data release (doi:10.5066/P1WKQ63T) staged under this name. Pinned rather than
# resolved by vintage: the 2025 world-data file beside it has another layout, and the errata
# usgs_mcs_reserves() applies belong to one edition, so a new edition is a deliberate update.
usgs_mcs_path <- file.path(raw_data_path, "MCS2026_World_Data.csv")
critical_minerals_path <- resolve_versioned_raw_input(
  raw_data_path,
  # Top-level alternation, no capture group: the manifest scanner in scripts/utils
  # reads this call as source text and stops at the first closing bracket it finds.
  pattern = "^IEA Critical Minerals Dataset \\d{4}\\.xlsx$|^iea_criticalminerals_\\d{2}\\.csv$",
  fallback = "IEA Critical Minerals Dataset 2026.xlsx",
  label = "IEA Critical Minerals Dataset"
)
cleantech_midstream_path <- file.path(raw_data_path, "iea_cleantech_Midstream.csv")
iea_cleantech_guide_path <- resolve_versioned_raw_input(
  raw_data_path,
  # The 2026 public dataset ships under a new name and a new schema; the hand-built
  # extract stays matchable so an older stage still builds. Neither name carries a
  # vintage, so the newer file on disk wins, and the theme builder detects which
  # layout it was handed. No brackets in this comment - the manifest scanner reads
  # the call as source text and stops at the first closing bracket it finds.
  pattern = "^IEACleanTechGuide.*\\.csv$|^IEA_Clean_Tech_Guide\\.csv$",
  fallback = "IEACleanTechGuidepublicdataset.csv",
  label = "IEA Clean Tech Guide"
)
ev_midstream_path <- file.path(raw_data_path, "ev_Midstream_capacity.csv")
trade_codes_path <- file.path(raw_data_path, "consolidated_hs6_energy_tech_long.csv")
trade_hs4_path <- file.path(raw_data_path, "hs92_country_product_year_4.csv")
trade_hs6_path <- file.path(raw_data_path, "hs92_country_product_year_6.csv")
comtrade_energy_trade_path <- file.path(raw_data_path, "comtrade_energy_trade.csv")
comtrade_total_export_path <- file.path(raw_data_path, "comtrade_total_export.csv")
bnef_neo_path <- resolve_versioned_raw_input(
  raw_data_path,
  pattern = "^\\d{4}-\\d{2}-\\d{2} - New Energy Outlook \\d{4}\\.csv$",
  fallback = "2024-10-29 - New Energy Outlook 2024.csv",
  label = "BNEF New Energy Outlook"
)
wdi_gdp_path <- file.path(raw_data_path, "wdi_gdp.csv")
wdi_country_path <- file.path(raw_data_path, "wdi_country_info.csv")
# The manifest gives the critmin entries a min_reporters floor, so the resolver walks past a
# stub vintage to the newest file with enough Comtrade reporters.
critmin_import_path <- resolve_versioned_raw_input(
  raw_data_path,
  pattern = "^critmin_import_\\d{4}\\.csv$",
  fallback = "critmin_import_2024.csv",
  label = "Comtrade critical minerals imports",
  manifest = raw_manifest
)
critmin_export_path <- resolve_versioned_raw_input(
  raw_data_path,
  pattern = "^critmin_export_\\d{4}\\.csv$",
  fallback = "critmin_export_2024.csv",
  label = "Comtrade critical minerals exports",
  manifest = raw_manifest
)
critmin_total_export_path <- resolve_versioned_raw_input(
  raw_data_path,
  pattern = "^critmin_total_export_\\d{4}\\.csv$",
  fallback = "critmin_total_export_2024.csv",
  label = "Comtrade critical minerals total exports",
  manifest = raw_manifest
)

# Market shares divide one year's exports by the same year's world total, so the three
# critmin files must come from one vintage even though each is resolved on its own floor.
critmin_years <- unique(sub(
  "^.*_(\\d{4})\\.csv$", "\\1",
  basename(c(critmin_import_path, critmin_export_path, critmin_total_export_path))
))
if (length(critmin_years) != 1) {
  stop(
    "Comtrade critical minerals files resolve to different vintages (",
    paste(basename(c(critmin_import_path, critmin_export_path, critmin_total_export_path)), collapse = ", "),
    "); stage a complete set for one year."
  )
}
# Every reporter's imports from the economies missing from the export pulls (Russia, Guinea,
# Gabon, ...) - the theme's mirror exports - plus the own exports of those that have reported
# since (the DR Congo, Jordan, ...), fetched by scripts/fetchers/comtrade_mirror_critmin.R for
# the same vintage. Optional, so a build without network access still runs.
critmin_mirror_path <- file.path(raw_data_path, "critmin_mirror_import.csv")

energy_prices_lcoe_path <- resolve_versioned_raw_input(
  raw_data_path,
  # BNEF stopped shipping a CSV export after the 2025 release and now stages the whole
  # macro workbook as .xlsb. Top-level alternation with no capture group, because the
  # manifest scanner reads this call as source text and stops at the first bracket.
  pattern = "^\\d{4}-\\d{2}-\\d{2} - .*LCOE Data\\.xlsb$|^\\d{4}-\\d{2}-\\d{2} - \\d{4} LCOE Data Viewer Tool\\.csv$",
  fallback = "2026-08-11 - LCOE Data.xlsb",
  label = "BNEF LCOE Data Viewer"
)
iea_weo_path <- resolve_versioned_raw_input(
  raw_data_path,
  pattern = "^WEO\\d{4}_AnnexA_Free_Dataset_World\\.csv$",
  fallback = "WEO2025_AnnexA_Free_Dataset_World.csv",
  label = "IEA WEO Annex A"
)
iea_ev_path <- resolve_versioned_raw_input(
  raw_data_path,
  # The IEA published this extract as the EV Data Explorer workbook through the 2025
  # release and as a flat CSV from Global EV Outlook 2026 onward; both names still
  # resolve and R/utils/iea_ev.R reads either form. Top-level alternation with no capture
  # group, because the manifest scanner reads this call as source text.
  pattern = "^EV data by country \\d{4}\\.csv$|^IEA_EVDataExplorer\\d{4}\\.xlsx$",
  fallback = "EV data by country 2026.csv",
  label = "IEA Global EV Outlook"
)
bcg_future_demand_path <- file.path(raw_data_path, "Market Size for Technology and Supply Chain.xlsx")
bnef_supply_chain_path <- resolve_versioned_raw_input(
  raw_data_path,
  pattern = "^BNEF_Energy Transition Supply Chains \\d{4}\\.xlsx$",
  fallback = "BNEF_Energy Transition Supply Chains 2025.xlsx",
  label = "BNEF Energy Transition Supply Chains"
)
relative_costs_iea_path <- file.path(raw_data_path, "Relative_Costs_IEA.csv")
imf_lending_rates_path <- file.path(raw_data_path, "imf_lending_rates.csv")
imf_ppi_path <- file.path(raw_data_path, "imf_ppi.csv")
solar_pv_potential_path <- file.path(raw_data_path, "solar_potential_clean.csv")
wind_potential_path <- file.path(raw_data_path, "wb_wind_country.csv")
geothermal_potential_path <- file.path(raw_data_path, "geothermal_lcoe_mw.csv")
iea_pams_path <- file.path(
  raw_data_path,"IEA_PAMS_Export.csv")
nipo_policy_path <- resolve_versioned_raw_input(
  raw_data_path,
  pattern = "^GTA NIPO - .*\\.xlsx$",
  fallback = "GTA NIPO - February 2026.xlsx",
  label = "GTA New Industrial Policy Observatory"
)
hs6_category_path <- file.path(
  raw_data_path,"hts_codes_categories_bolstered_final.csv")
tech_ghg_path <- file.path(
  raw_data_path,"ipcc_ghg_intensity.csv")
cat_policy_path <- file.path(
  raw_data_path,"CAT_country ratings data.csv")
dual_use_scores_path <- file.path(
  raw_data_path,"dual_use_scores_primary_secondary_tertiary.csv")
investment_monitor_path <- file.path(
  raw_data_path, "GCIM_Investment_Capacity_aggregated.xlsx")

# Energy prices input: the wide IMF PCPS export, fetched from the SDMX API by
# scripts/fetchers/imf_sdmx.R or hand-staged from the IMF Data Explorer. Both carry the
# annual year-on-year series the theme reports alongside volatility.
imf_price_path <- file.path(raw_data_path, "imf_commodity_prices.csv")

# Uranium resources and mine production (Nuclear Upstream), transcribed from the OECD-NEA/IAEA
# Red Book into data/reference and copied into data/raw by scripts/05_ingest_sources.R. Read
# from data/reference when the ingest step has not copied them yet.
uranium_resources_path <- file.path(raw_data_path, "uranium_resources.csv")
if (!file.exists(uranium_resources_path)) {
  uranium_resources_path <- file.path(raw_inputs_reference_dir(repo_root), "uranium_resources.csv")
}
uranium_production_path <- file.path(raw_data_path, "uranium_production.csv")
if (!file.exists(uranium_production_path)) {
  uranium_production_path <- file.path(raw_inputs_reference_dir(repo_root), "uranium_production.csv")
}


# Fail fast (or skip) if required raw inputs are missing.
missing_files <- c(
  raw_path,
  eia_intl_path,
  reserves_excel_path,
  usgs_mcs_path,
  critical_minerals_path,
  cleantech_midstream_path,
  iea_cleantech_guide_path,
  ev_midstream_path,
  trade_codes_path,
  trade_hs4_path,
  trade_hs6_path,
  comtrade_energy_trade_path,
  comtrade_total_export_path,
  bnef_neo_path,
  wdi_gdp_path,
  wdi_country_path,
  critmin_import_path,
  critmin_export_path,
  critmin_total_export_path,
  energy_prices_lcoe_path,
  iea_weo_path,
  iea_ev_path,
  bcg_future_demand_path,
  bnef_supply_chain_path,
  relative_costs_iea_path,
  imf_lending_rates_path,
  imf_ppi_path,
  imf_price_path,
  solar_pv_potential_path,
  geothermal_potential_path,
  iea_pams_path,
  nipo_policy_path,
  hs6_category_path,
  tech_ghg_path,
  cat_policy_path,
  dual_use_scores_path,
  uranium_resources_path,
  uranium_production_path,
  investment_monitor_path
)
missing_files <- missing_files[!file.exists(missing_files)]

if (investment_monitor_path %in% missing_files && !skip_data_downloads) {
  stop(
    "Missing GCIM investment monitor file at data/raw/GCIM_Investment_Capacity_aggregated.xlsx (resolved: ",
    investment_monitor_path,
    ")."
  )
}

if (eia_intl_path %in% missing_files && !skip_data_downloads) {
  stop(
    "Missing the EIA International Energy Statistics extract at ", eia_intl_path, ". The import ",
    "dependence and production themes take the oil, gas and coal production EI does not report ",
    "from it. Stage it with `Rscript scripts/fetchers/eia_intl.R` (downloads ",
    "https://www.eia.gov/opendata/bulk/INTL.zip, public, no key) or scripts/05_ingest_sources.R, ",
    "or copy eia_intl_fossil_production.csv from the SharePoint raw folder."
  )
}

if (length(missing_files) > 0 && !skip_data_downloads) {
  expected_list <- paste0("- ", missing_files, collapse = "\n")
  stop("Missing required raw data. Expected raw files:\n", expected_list)
}

# Stamp which vintage each pattern-resolved input actually landed on, so an index run
# can be replicated later even after newer releases arrive in data/raw.
write_resolved_vintages(
  list(
    iea_critical_minerals = critical_minerals_path,
    bnef_neo = bnef_neo_path,
    bnef_lcoe_viewer = energy_prices_lcoe_path,
    bnef_supply_chains = bnef_supply_chain_path,
    critmin_import = critmin_import_path,
    critmin_export = critmin_export_path,
    critmin_total_export = critmin_total_export_path,
    weo_annex_a = iea_weo_path,
    iea_ev_outlook = iea_ev_path,
    gta_nipo = nipo_policy_path,
    energy_prices_input = imf_price_path
  ),
  raw_data_path
)

country_info <- read.csv(wdi_country_path)
country_info <- standardize_country_info(country_info)


  ei <- read.csv(raw_path)
  # Fills the EI fuel production gaps of the import dependence and production themes. Only empty
  # cells are missing: "NA" in value_note is EIA's own code for "not available".
  eia_intl <- read.csv(eia_intl_path, stringsAsFactors = FALSE, na.strings = "")
  # Shared WDI country reference for multiple themes.
  gdp_data <- read.csv(wdi_gdp_path)
  country_reference <- foreign_dependency_build_country_reference(ei, year = 2025)
  

  # Theme: Energy access and consumption (EI data).
  energy_access_tbl <- energy_access_consumption(ei, country_info = country_info)
  energy_access_tbl <- standardize_theme_types(energy_access_tbl, country_info = country_info)
  write_processed_tbl(energy_access_tbl, "energy_access_tbl", processed_dir)

  # Theme: Solar PV potential (Global Solar Atlas GIS data).
  solar_pv_raw <- read.csv(solar_pv_potential_path)
  # The ranking is restricted to the economies the index scores - WDI's plus the EI roster,
  # which adds Taiwan - so the GIS layer's non-economy polygons (Northern Cyprus, Siachen
  # Glacier, Somaliland) do not take a place in the percent ranks.
  potential_roster <- dplyr::distinct(dplyr::bind_rows(
    dplyr::select(country_info, country),
    tibble::tibble(country = country_reference$Country)
  ))
  solar_pv_potential_tbl <- solar_pv_potential(solar_pv_raw, country_info = potential_roster)
  solar_pv_potential_tbl <- standardize_theme_types(
    solar_pv_potential_tbl,
    country_info = country_info
  )
  write_processed_tbl(solar_pv_potential_tbl, "solar_pv_potential_tbl", processed_dir)

  # Theme: Wind potential (Global Wind Atlas country data).
  wind_raw <- read.csv(wind_potential_path)
  wind_potential_tbl <- wind_potential(wind_raw, country_info = potential_roster)
  wind_potential_tbl <- standardize_theme_types(
    wind_potential_tbl,
    country_info = country_info
  )
  write_processed_tbl(wind_potential_tbl, "wind_potential_tbl", processed_dir)

  # Theme: Geothermal potential (LCOE and resource potential data).
  geothermal_raw <- read.csv(geothermal_potential_path)
  geothermal_potential_tbl <- geothermal_potential(geothermal_raw, country_info = country_info)
  geothermal_potential_tbl <- standardize_theme_types(
    geothermal_potential_tbl,
    country_info = country_info
  )
  write_processed_tbl(geothermal_potential_tbl, "geothermal_potential_tbl", processed_dir)

  # Theme: Import dependence (EI data).
  # Production EI does not report comes from EIA (same year; gas and coal, a year behind in EIA,
  # from its latest year) and counts as observed. Exporters EI lists with production but no
  # consumption row (Angola, Nigeria, Libya, Guyana, ...) take EIA consumption too
  # (eia_fill_consumption, adopted 2026-10-11 with the production fill; README, ES "2) Energy Imports").
  import_dependence_tbl <- import_dependence(ei, eia = eia_intl, eia_fill_consumption = TRUE)
  import_dependence_tbl <- standardize_theme_types(import_dependence_tbl, country_info = country_info)
  write_processed_tbl(import_dependence_tbl, "import_dependence_tbl", processed_dir)

  # Theme: Foreign dependency inputs (critical minerals + IEA datasets).
  critical <- read_iea_critical_minerals(critical_minerals_path)
  mineral_demand_clean <- reserves_build_mineral_demand_clean(critical)

  # Reserves sources (user decision, 2026-10-09): EI proved reserves for oil, gas and coal
  # (reserves_specs()), USGS MCS for the nine critical minerals, the Red Book for uranium.
  reserve_inputs <- lapply(reserves_specs(), function(spec) {
    spec$data <- readxl::read_excel(reserves_excel_path, sheet = spec$sheet, skip = spec$skip)
    spec
  })

  # The USGS table is Windows-1252 and prints "NA" as a value code ("not available"), so it is
  # read as text and every code is parsed explicitly by usgs_mcs_reserves().
  usgs_reserves <- usgs_mcs_reserves(readr::read_csv(
    usgs_mcs_path,
    col_types = readr::cols(.default = readr::col_character()),
    locale = readr::locale(encoding = "windows-1252"),
    na = character(),
    progress = FALSE
  ))
  usgs_unquantified <- usgs_mcs_unquantified(usgs_reserves)
  if (nrow(usgs_unquantified) > 0) {
    # Listed holders without a figure enter the theme as missing (imputed), not as zeros.
    message(
      "USGS MCS reserves listed without a figure (scored as missing, not zero): ",
      paste0(usgs_unquantified$tech, " ", usgs_unquantified$ISO3166_alpha3, " (", usgs_unquantified$status, ")", collapse = ", ")
    )
  }

  # Nuclear Upstream: Red Book uranium resources ("Nuclear Reserves") and mine production,
  # used by the reserves, critical minerals production and production depth themes.
  uranium_resources <- read.csv(uranium_resources_path, stringsAsFactors = FALSE)
  uranium_production <- read.csv(uranium_production_path, stringsAsFactors = FALSE)

  reserves_tbl <- reserves(
    ei, reserve_inputs, mineral_demand_clean,
    uranium_resources = uranium_resources,
    usgs_reserves = usgs_reserves
  )
  reserves_tbl <- standardize_theme_types(reserves_tbl, country_info = country_info)
  write_processed_tbl(reserves_tbl, "reserves_tbl", processed_dir)

  cleantech_midstream <- read.csv(cleantech_midstream_path)
  ev_midstream <- read.csv(ev_midstream_path)
  foreign_dependency_tbl <- foreign_dependency(
    critical = critical,
    mineral_demand_clean = mineral_demand_clean,
    ei = ei,
    cleantech_midstream = cleantech_midstream,
    ev_midstream = ev_midstream
  )
  foreign_dependency_tbl <- standardize_theme_types(foreign_dependency_tbl, country_info = country_info)
  write_processed_tbl(foreign_dependency_tbl, "foreign_dependency_tbl", processed_dir)

  # Theme: Market share manufacturing (IEA midstream data).
  market_share_manufacturing_tbl <- market_share_manufacturing(
    ei = ei,
    cleantech_midstream = cleantech_midstream,
    ev_midstream = ev_midstream
  )
  market_share_manufacturing_tbl <- standardize_theme_types(
    market_share_manufacturing_tbl,
    country_info = country_info
  )
  write_processed_tbl(
    market_share_manufacturing_tbl,
    "market_share_manufacturing_tbl",
    processed_dir
  )

  # Theme: Technological readiness is built after Export feasibility, whose countries it is
  # copied to.

  # Theme: Critical minerals processing (IEA data).
  critical_minerals_processing_tbl <- critical_minerals_processing(
    critical = critical,
    mineral_demand_clean = mineral_demand_clean,
    country_info = country_info,
    country_reference = country_reference
  )
  critical_minerals_processing_tbl <- standardize_theme_types(
    critical_minerals_processing_tbl,
    country_info = country_info
  )
  write_processed_tbl(
    critical_minerals_processing_tbl,
    "critical_minerals_processing_tbl",
    processed_dir
  )

  # Theme: Critical minerals production (EI data).
  critical_minerals_production_inputs <- lapply(critical_minerals_production_specs(), function(spec) {
    spec$data <- readxl::read_excel(reserves_excel_path, sheet = spec$sheet, skip = spec$skip)
    spec
  })
  critical_minerals_production_tbl <- critical_minerals_production(
    production_inputs = critical_minerals_production_inputs,
    mineral_demand_clean = mineral_demand_clean,
    country_reference = country_reference
  )
  # Uranium mine production joins as "Nuclear Production", ranked like the EI minerals; this
  # is the Energy Security production theme. The Red Book lists every producer, so the zeros
  # are completed over every economy (EI roster plus WDI), not only the EI roster.
  critical_minerals_production_tbl <- uranium_bind_theme_rows(
    critical_minerals_production_tbl,
    uranium_production_table(
      uranium_production,
      country_reference = dplyr::bind_rows(
        country_reference,
        dplyr::transmute(country_info, Country = country, iso3c = iso3c)
      )
    ) %>%
      energy_security_add_overall_index()
  )
  critical_minerals_production_tbl <- standardize_theme_types(
    critical_minerals_production_tbl,
    country_info = country_info
  )
  write_processed_tbl(
    critical_minerals_production_tbl,
    "critical_minerals_production_tbl",
    processed_dir
  )

  # Theme: Critical minerals trade (UN Comtrade).
  critmin_import <- read.csv(critmin_import_path)
  critmin_export <- read.csv(critmin_export_path)
  total_export <- read.csv(critmin_total_export_path)
  critmin_mirror <- if (file.exists(critmin_mirror_path)) {
    read.csv(critmin_mirror_path, colClasses = c(cmd_code = "character"))
  } else {
    warning(
      basename(critmin_mirror_path), " is missing (run scripts/05 with network access): ",
      "Minerals Trade falls back to reporter-side exports and non-reporters score 0.",
      call. = FALSE
    )
    NULL
  }
  critical_minerals_trade_tbl <- critical_minerals_trade(
    critmin_import = critmin_import,
    critmin_export = critmin_export,
    total_export = total_export,
    mineral_demand_clean = mineral_demand_clean,
    country_info = country_info,
    mirror_import = critmin_mirror
  )
  critical_minerals_trade_tbl <- standardize_theme_types(
    critical_minerals_trade_tbl,
    country_info = country_info
  )
  write_processed_tbl(
    critical_minerals_trade_tbl,
    "critical_minerals_trade_tbl",
    processed_dir
  )
  # Exporter x mineral exports, shares and HHIs with their provenance (reported / mirror).
  critical_minerals_trade_flows_tbl <- critical_minerals_trade_flows(
    critmin_import = critmin_import,
    critmin_export = critmin_export,
    total_export = total_export,
    mineral_demand_clean = mineral_demand_clean,
    country_info = country_info,
    mirror_import = critmin_mirror
  ) %>%
    dplyr::rename(iso3c = reporter_iso)
  write_processed_tbl(
    critical_minerals_trade_flows_tbl,
    "critical_minerals_trade_flows_tbl",
    processed_dir
  )

  # Theme: Energy consumption (EI + BNEF data).
  bnef_neo <- read.csv(bnef_neo_path, skip = 2)
  energy_consumption_tbl <- energy_consumption(
    ei = ei,
    bnef_neo = bnef_neo,
    country_info = country_info
  )
  energy_consumption_tbl <- standardize_theme_types(energy_consumption_tbl, country_info = country_info)
  write_processed_tbl(energy_consumption_tbl, "energy_consumption_tbl", processed_dir)

  # Theme: Energy prices (IMF PCPS data).
  imf_commodity_prices <- read.csv(imf_price_path)
  energy_prices_tbl <- energy_prices(
    imf_price = imf_commodity_prices,
    mineral_demand_clean = mineral_demand_clean,
    country_info = country_info
  )
  energy_prices_tbl <- standardize_theme_types(energy_prices_tbl, country_info = country_info)
  write_processed_tbl(energy_prices_tbl, "energy_prices_tbl", processed_dir)

  # Theme: LCOE competitiveness (BNEF data).
  # read_bnef_lcoe() flattens the .xlsb release to CSV once (cached under data/raw_cache)
  # and attaches the release's reference year, which is the "current" year the theme scores
  # against 2050. It moved from 2024 to 2025 with the 2026 release.
  lcoe_bnef <- read_bnef_lcoe(energy_prices_lcoe_path, root_dir = repo_root)
  lcoe_competitiveness_tbl <- lcoe_competitiveness(
    lcoe_bnef = lcoe_bnef,
    source_label = paste0(
      "BNEF LCOE Data Viewer (",
      sub("^(\\d{4}-\\d{2}-\\d{2}).*$", "\\1", basename(energy_prices_lcoe_path)),
      ")"
    ),
    # Markets outside country_info (Taiwan) are not ranked.
    country_info = country_info
  )
  lcoe_competitiveness_tbl <- standardize_theme_types(
    lcoe_competitiveness_tbl,
    country_info = country_info
  )
  write_processed_tbl(
    lcoe_competitiveness_tbl,
    "lcoe_competitiveness_tbl",
    processed_dir
  )

  # Theme: Trade concentration (Atlas data + WDI country reference).
  subcat <- readr::read_csv(hs6_category_path, show_col_types = FALSE)

  # The Atlas panels are 0.4 and 0.9 GB. Read only the columns the trade themes use, keep
  # product codes as text (read as integers, chapters 01-09 lose their leading zero), and
  # keep only the newest year: both legs read the newest year the staged panel carries, so
  # market share, the trade balance and feasibility describe the same year (the theme
  # defaults, HS4 2022 and HS6 2023, had the feasibility leg a year behind).
  read_atlas_panel <- function(path, columns) {
    as.data.frame(data.table::fread(
      path,
      select = columns,
      colClasses = list(character = "product_hs92_code"),
      # Trade values pass 2^31; fread would otherwise return bit64 integers, which base
      # arithmetic misreads without bit64 attached.
      integer64 = "double",
      showProgress = FALSE
    ))
  }
  aec_6_data <- read_atlas_panel(
    trade_hs6_path,
    c("country_iso3_code", "product_hs92_code", "year", "export_value", "import_value", "global_market_share")
  )
  atlas_year_6 <- max(suppressWarnings(as.integer(aec_6_data$year)), na.rm = TRUE)
  aec_6_data <- aec_6_data[aec_6_data$year == atlas_year_6, , drop = FALSE]
  gc()

  aec_4_data <- read_atlas_panel(
    trade_hs4_path,
    c("country_iso3_code", "product_hs92_code", "year", "distance")
  )
  atlas_year_4 <- max(suppressWarnings(as.integer(aec_4_data$year[!is.na(aec_4_data$distance)])), na.rm = TRUE)
  aec_4_data <- aec_4_data[aec_4_data$year == atlas_year_4, , drop = FALSE]
  gc()

  comtrade_energy_trade <- read.csv(comtrade_energy_trade_path)
  comtrade_total_export <- read.csv(comtrade_total_export_path)

  comtrade_vintage_path <- file.path(raw_data_path, "comtrade_vintage.yml")
  comtrade_year_option <- NA_integer_
  if (file.exists(comtrade_vintage_path)) {
    comtrade_vintage <- yaml::read_yaml(comtrade_vintage_path)
    comtrade_year_option <- suppressWarnings(as.integer(comtrade_vintage$actual_year_end_used))
  }
  if (is.na(comtrade_year_option)) {
    fallback_col <- intersect(c("ref_year", "period", "year", "Year"), names(comtrade_energy_trade))
    if (length(fallback_col) > 0) {
      fallback_years <- suppressWarnings(as.integer(stringr::str_extract(as.character(comtrade_energy_trade[[fallback_col[[1]]]]), "\\d{4}")))
      fallback_years <- fallback_years[!is.na(fallback_years)]
      if (length(fallback_years) > 0) {
        comtrade_year_option <- max(fallback_years)
      }
    }
  }
  options(opportunity_security.comtrade_year = comtrade_year_option)

  include_sub_sector <- isTRUE(if (!is.null(config$include_sub_sector)) {
    config$include_sub_sector
  } else {
    config$energy_security_include_sub_sector
  })

  # atlas_year_6 / atlas_year_4 were set where the Atlas panels were read, above.
  # gdp_data (WDI) scales the basket's net imports in the ES exposure term.
  # The trade themes rank and keep the economies of their roster; Taiwan is not in WDI, so it
  # is added here (add_unlisted_economies(), R/utils/country.R) rather than dropped at the join.
  trade_country_info <- add_unlisted_economies(country_info)

  # The export-HHI S-curve ranks the cells of the headline technologies only (the technologies
  # the indices score, config/coalition.yml headline_techs), so an unscored basket such as
  # Semiconductors cannot move a scored cell's index (user decision 2026-10-09). Re-export hubs
  # (config/index_definition.yml trade$re_export_hubs: Hong Kong) leave the Comtrade measures -
  # exports, imports, RCA, export size and the export HHI - and their world aggregates; their
  # Atlas variables stay.
  trade_hhi_rank_techs <- unlist(yaml::read_yaml(file.path(repo_root, "config", "coalition.yml"))$headline_techs)
  if (length(trade_hhi_rank_techs) == 0) {
    stop("config/coalition.yml has no headline_techs, the ranking universe of the trade HHI index.")
  }
  trade_reexport_hubs <- toupper(unlist(getOption("opportunity_security.index_definition")$trade$re_export_hubs))
  message(
    "Trade themes: export HHI ranked across the cells of ", length(trade_hhi_rank_techs),
    " headline technologies; re-export hubs left out of the Comtrade measures: ",
    if (length(trade_reexport_hubs) > 0) paste(trade_reexport_hubs, collapse = ", ") else "none"
  )

  trade_concentration_tbl <- trade_concentration(
    subcat = subcat,
    aec_4_data = aec_4_data,
    aec_6_data = aec_6_data,
    comtrade_trade = comtrade_energy_trade,
    comtrade_total_export = comtrade_total_export,
    country_info = trade_country_info,
    year_4 = atlas_year_4,
    year_6 = atlas_year_6,
    year_comtrade = getOption("opportunity_security.comtrade_year"),
    include_sub_sector = include_sub_sector,
    gdp_data = gdp_data,
    hhi_rank_techs = trade_hhi_rank_techs,
    reexport_hubs = trade_reexport_hubs
  )
  
  trade_concentration_tbl <- standardize_theme_types(trade_concentration_tbl, country_info = country_info)
  
  write_processed_tbl(
    trade_concentration_tbl,
    "trade_concentration_tbl",
    processed_dir
  )

  # Theme: Export feasibility (Atlas/Comtrade trade data).
  export_feasibility_tbl <- export_feasibility(
    aec_4_data = aec_4_data,
    aec_6_data = aec_6_data,
    subcat = subcat,
    country_info = trade_country_info,
    gdp_data = gdp_data,
    comtrade_trade = comtrade_energy_trade,
    comtrade_total_export = comtrade_total_export,
    year_4 = atlas_year_4,
    year_6 = atlas_year_6,
    year_comtrade = getOption("opportunity_security.comtrade_year"),
    include_sub_sector = include_sub_sector,
    hhi_rank_techs = trade_hhi_rank_techs,
    reexport_hubs = trade_reexport_hubs
  )
  export_feasibility_tbl <- standardize_theme_types(export_feasibility_tbl, country_info = country_info)
  write_processed_tbl(
    export_feasibility_tbl,
    "export_feasibility_tbl",
    processed_dir
  )

  # Theme: Overcapacity premium (BNEF supply chains data + trade reference).
  overcapacity_bnef <- readxl::read_excel(bnef_supply_chain_path, sheet = 3, skip = 9)
  overcapacity_premium_tbl <- overcapacity_premium(
    overcapacity_raw = overcapacity_bnef,
    trade_tidy = export_feasibility_tbl
  )
  overcapacity_premium_tbl <- standardize_theme_types(
    overcapacity_premium_tbl,
    country_info = country_info
  )
  write_processed_tbl(
    overcapacity_premium_tbl,
    "overcapacity_premium_tbl",
    processed_dir
  )

  # Theme: Technological readiness (IEA Clean Tech Guide).
  # The 2026 public dataset ships with a UTF-8 BOM, which would otherwise land in the first
  # column name and hide it from the layout check. The TRL window (2020-2025 for the 2026
  # release) is read off whichever TRL columns the file carries.
  # TRL is one value per technology. Like overcapacity it is copied to the countries the
  # trade theme scores, so the pillar reads it as observed; a single "Global" row was
  # imputed to every country instead. The trade countries, not all of country_info: seven
  # WDI microstates (Monaco, Liechtenstein, ...) appear in no other EO input and would
  # otherwise enter the EO grid on TRL alone.
  iea_cleantech_guide <- read.csv(iea_cleantech_guide_path, fileEncoding = "UTF-8-BOM")
  technological_readiness_tbl <- technological_readiness(
    iea_cleantech_all = iea_cleantech_guide,
    country_reference = unique(export_feasibility_tbl$Country)
  )
  write_processed_tbl(
    technological_readiness_tbl,
    "technological_readiness_tbl",
    processed_dir
  )

  # Theme: Future demand (IEA + BNEF + EV + BCG data).
  iea_weo <- read.csv(iea_weo_path)
  iea_ev <- read_iea_ev(iea_ev_path)
  bcg <- readxl::read_excel(bcg_future_demand_path, sheet = 1)

  future_demand_tbl <- future_demand(
    iea_weo = iea_weo,
    bnef_neo = bnef_neo,
    iea_ev = iea_ev,
    bcg = bcg,
    country_info = country_info,
    country_reference = country_reference
  )
  future_demand_tbl <- standardize_theme_types(future_demand_tbl, country_info = country_info)
  write_processed_tbl(future_demand_tbl, "future_demand_tbl", processed_dir)

  # Theme: Production depth + momentum (EI + IEA critical minerals + Red Book uranium).
  production_depth_momentum_tbl <- production_depth_momentum(
    ei = ei,
    critical = critical,
    mineral_demand_clean = mineral_demand_clean,
    country_info = country_info,
    uranium_production = uranium_production,
    # Oil, gas and coal production EI does not report, from EIA (as for import dependence).
    eia = eia_intl
  )
  production_depth_momentum_tbl <- standardize_theme_types(
    production_depth_momentum_tbl,
    country_info = country_info
  )
  write_processed_tbl(
    production_depth_momentum_tbl,
    "production_depth_momentum_tbl",
    processed_dir
  )

  # Theme: Investment momentum + capacity (GCIM investment monitor).
  # Critical-minerals investment is split between Batteries and EV Upstream by the IEA
  # base-year demand shares for battery minerals instead of being counted in full in both.
  investment_momentum_tbl <- investment_momentum_from_excel(
    investment_monitor_path,
    country_reference = country_reference$Country,
    critical_minerals_split = investment_critical_minerals_split(mineral_demand_clean)
  )
  investment_momentum_tbl <- standardize_theme_types(
    investment_momentum_tbl,
    country_info = country_info
  )
  write_processed_tbl(
    investment_momentum_tbl,
    "investment_momentum_tbl",
    processed_dir
  )

  # Theme: Cost competitiveness (IEA relative costs).
  iea_relative_costs <- read.csv(relative_costs_iea_path)
  # ILO average monthly earnings by economic activity (ILOSTAT EAR_EMTA_SEX_ECO_CUR_NB_A), read from
  # the staged copy in data/raw (manifest id ilo_earnings_by_activity). It used to be read live from
  # the ILO API on every run, which made reruns depend on the day and failed outright on 2026-10-10,
  # when the API began returning empty responses to scripted requests. Without the staged file the
  # script tries one download into it and stops if nothing usable comes back.
  ilo_path <- file.path(raw_data_path, "ilo_earnings_by_activity.csv")
  if (!file.exists(ilo_path)) {
    ilo_url <- "https://rplumber.ilo.org/data/indicator/?id=EAR_EMTA_SEX_ECO_CUR_NB_A&lang=en&type=label&format=.csv&channel=ilostat&title=average-monthly-earnings-of-employees-by-sex-economic-activity-and-currency-annual"
    message("Staged ILO earnings not found; downloading to ", ilo_path, " ...")
    old_timeout <- options(timeout = max(600, getOption("timeout")))
    download_ok <- tryCatch(
      utils::download.file(ilo_url, ilo_path, mode = "wb", quiet = TRUE) == 0,
      error = function(e) FALSE
    )
    options(old_timeout)
    if (!download_ok || !file.exists(ilo_path) || file.size(ilo_path) == 0) {
      if (file.exists(ilo_path)) file.remove(ilo_path)
      stop("ILO earnings are not staged and the download failed or came back empty: stage ",
           "ILOSTAT EAR_EMTA_SEX_ECO_CUR_NB_A (CSV, labels) as ", ilo_path, ".")
    }
  }
  ilo_raw <- read.csv(ilo_path)
  imf_lending_rates <- read.csv(imf_lending_rates_path)
  imf_ppi <- read.csv(imf_ppi_path)
  cost_competitiveness_tbl <- cost_competitiveness(
    iea_cost_raw = iea_relative_costs,
    ei = ei,
    country_info = country_info,
    ilo_raw = ilo_raw,
    imf_lending_rates = imf_lending_rates,
    imf_ppi = imf_ppi
  )
  cost_competitiveness_tbl <- standardize_theme_types(
    cost_competitiveness_tbl,
    country_info = country_info
  )
  write_processed_tbl(
    cost_competitiveness_tbl,
    "cost_competitiveness_tbl",
    processed_dir
  )

  pams_raw <- readr::read_csv(iea_pams_path, show_col_types = FALSE)

  # OPSI_SKIP_NIPO=true keeps the NIPO component of the stored policy_component_tbl.rds
  # instead of rescoring the GTA extract, which needs ~4-5 GB on its own (see the NIPO
  # memory note in the README). IEA PAMS, CAT and the dual-use scores are still rebuilt.
  # The stored component carries whatever extract and parameters produced it, so the run
  # records that it was reused rather than recomputed.
  skip_nipo <- tolower(Sys.getenv("OPSI_SKIP_NIPO", "false")) %in% c("1", "true", "yes")
  stored_nipo_component <- NULL
  if (skip_nipo) {
    stored_policy_path <- file.path(processed_dir, "policy_component_tbl.rds")
    if (!file.exists(stored_policy_path)) {
      stop("OPSI_SKIP_NIPO is set but no stored policy_component_tbl.rds exists to reuse.")
    }
    stored_nipo_component <- readRDS(stored_policy_path) %>%
      dplyr::filter(.data$variable == "NIPO Policy Index")
    if (nrow(stored_nipo_component) == 0) {
      stop("OPSI_SKIP_NIPO is set but the stored policy_component_tbl.rds has no NIPO rows.")
    }
    message(
      "OPSI_SKIP_NIPO: reusing ", nrow(stored_nipo_component), " stored NIPO rows for ",
      dplyr::n_distinct(stored_nipo_component$Country), " countries; GTA NIPO not rescored."
    )
  }
  nipo_raw <- if (skip_nipo) NULL else readxl::read_excel(nipo_policy_path, sheet = 1)

  # ---- HS6 -> (Technology, Value Chain) crosswalk for NIPO --------------------
  # This ONE table plays two roles in nipo_policy_outputs(), and both must be
  # passed explicitly (see the call below):
  #
  #   subcat_raw        the crosswalk that maps a policy's HS6 codes to
  #                     Technology x Value.Chain.
  #   hs6_essential_tbl the `essential` flag, which decides WHICH codes are
  #                     allowed to drive that classification. 382 of 482 rows
  #                     are essential; the other 100 are generic inputs
  #                     (plastics, steel, softwood) kept for context only.
  #
  # It is the same file for both because it carries the codes and the flag
  # together. `hts_codes_categories_bolstered_final.csv` holds the identical
  # (Technology, HS6) set without the flag, so nothing is lost by using this one.
  #
  # Two properties of this file to be aware of:
  #   * 482 rows collapse to 466 distinct (Technology, Value.Chain, Sub.Sector,
  #     HS6) keys. The 16 extras differ ONLY in `essential`, i.e. the same code
  #     is flagged both TRUE and FALSE. TRUE wins, both via any() in
  #     prepare_subcat_mapping() and via the filter in clean_nipo_raw(). Made
  #     explicit here rather than left to those internals.
  #   * An HS6 code deliberately maps to several technologies (71 codes map to
  #     2, 47 to 3, 6 to 4). A static converter is filed under Electric Grid,
  #     Solar, Wind and EVs. Technology totals therefore OVERLAP: describe them
  #     as "X-attributed" policy, never as a self-contained sector.
  hs6_categories_essential <- readr::read_csv(
    file.path(raw_data_path, "hs6_categories_with_essential.csv"),
    show_col_types = FALSE
  ) %>%
    rename("Value.Chain" = "Value Chain") %>%
    # Resolve the duplicate-key conflict explicitly: a code is essential if ANY
    # of its rows says so.
    group_by(.data$Technology, .data$Value.Chain, .data$Sub.Sector, .data$HS6) %>%
    summarise(essential = any(as.logical(.data$essential), na.rm = TRUE), .groups = "drop")

  # Guard the assumption the de-duplication rests on: one (Technology, HS6) must
  # not span two value chains or sub-sectors, or collapsing would lose a mapping.
  stopifnot(
    nrow(dplyr::distinct(hs6_categories_essential, .data$Technology, .data$HS6)) ==
      nrow(hs6_categories_essential)
  )

  tech_ghg_raw <- readr::read_csv(tech_ghg_path, show_col_types = FALSE)
  cat_policy_raw <- readr::read_csv(cat_policy_path, show_col_types = FALSE) %>%
    rename("Overall.rating"="Overall rating")
  dual_use_scores_raw <- readr::read_csv(dual_use_scores_path, show_col_types = FALSE)

  iea_policy_outputs <- iea_policy_index(pams_raw, split_strength = FALSE)
  iea_policy_clean <- iea_policy_outputs$outputs$policy_clean %>%
    select(iso3,country,title_text:year,policy_strength,tech,supply_chain,policy_type_bucket,w_type:w_sc) %>%
    arrange(desc(policy_strength)) %>%
    arrange(desc(year))
  iea_policy_index_tbl <- iea_policy_outputs$index_tbl
  rm(pams_raw)

  # The legacy 23-member ally list, used only by OPSI_NIPO_SCOPE=allies to reproduce the Feb and
  # June 2026 NIPO builds. It is not the allied coalition (that is derived, coalition_members()
  # in R/indices/build_indices.R: coalition_tbl.rds from scripts/13, or geo_coalition_tbl.rds
  # from scripts/12 until that exists), and NIPO is excluded from the composite Policy index
  # since Oct 2026 (scripts/20), so this list no longer reaches any index.
  ally_iso3 <- c(
    "USA", "CAN", "JPN", "AUS", "IND", "MEX", "KOR", "GBR", "DEU", "FRA", "ITA", "BRA", "SAU",
    "ZAF", "IDN", "NOR", "ARE", "VNM", "KEN", "DNK", "ARG", "MAR", "CHL"
  )

  country_lookup <- country_info %>%
    dplyr::transmute(
      country = standardize_country_names(.data$country),
      iso3c = toupper(as.character(.data$iso3c))
    )

  if (!skip_nipo) {
  nipo_allies <- nipo_raw %>%
    dplyr::mutate(
      implementing_country = standardize_country_names(.data$`Implementing Jurisdiction`)
    ) %>%
    dplyr::left_join(country_lookup, by = c("implementing_country" = "country")) %>%
    dplyr::rename(iso = .data$iso3c) %>%
    dplyr::filter(.data$iso %in% ally_iso3) %>%
    dplyr::select(-"implementing_country", -"iso")

  # Which implementing jurisdictions NIPO scores. "all" (the default, and the committed
  # behaviour since Feb 2026) keeps every jurisdiction in the extract (77 in the 2026
  # extracts), so the S-curve across countries within each tech x stage ranks a country
  # against all the economies GTA monitors and the Policy index is not conditioned on allied
  # status. "allies" reproduces the Feb and June 2026 builds, which ran on the 23 allies
  # only. OPSI_NIPO_SCOPE=allies is also the fallback if the full extract runs out of memory.
  nipo_scope <- tolower(Sys.getenv("OPSI_NIPO_SCOPE", "all"))
  if (!nipo_scope %in% c("all", "allies")) {
    stop("OPSI_NIPO_SCOPE must be 'all' or 'allies', not '", nipo_scope, "'.")
  }
  nipo_input <- if (identical(nipo_scope, "allies")) nipo_allies else nipo_raw
  rm(nipo_raw, nipo_allies)
  invisible(gc())

  # The yearly panel (by_tech_sc_year) stops at the extract's release year, read from the
  # file name and never later than the run year, so a measure with no announcement date
  # and a scheduled 2027-28 implementation date cannot add years past the end of the data.
  # NULL (the latest announcement year, the nipo_policy_outputs() default) when the file
  # name carries no vintage.
  nipo_vintage_key <- raw_input_vintage_key(nipo_policy_path)
  nipo_year_max <- if (is.na(nipo_vintage_key)) {
    NULL
  } else {
    min(as.integer(nipo_vintage_key %/% 10000), as.integer(format(Sys.Date(), "%Y")))
  }

  # ============================================================================
  # Theme: NIPO Domestic Intervention Score (DIS)
  # ----------------------------------------------------------------------------
  # Method reference: R/categories/policy/NIPO Index.md
  #
  # Every argument below is passed BY NAME. The two crosswalk arguments in
  # particular used to be positional, which read as though the essential-goods
  # table were the only input; it is in fact doing double duty, and
  # hs6_essential_tbl was being resolved through a global-variable side channel
  # (resolve_hs6_essential_tbl() falls back to exists("hs6_categories_essential")).
  # That worked only by name coincidence: rename the local and the essential
  # filter would silently stop applying, letting all 100 generic codes drive
  # tech classification. Passed explicitly now.
  #
  # The de-biasing arguments are stated explicitly even where they match the
  # defaults, so a stored output records which methodology produced it. Set
  # dis_legacy_mode = TRUE to reproduce the pre-remediation numbers exactly.
  nipo_policy_out <- nipo_domestic_intervention_outputs(
    raw_nipo = nipo_input,

    # HS6 -> Technology x Value.Chain crosswalk, and the essential-goods flag
    # that decides which codes may drive that mapping. Same table, two roles.
    subcat_raw = hs6_categories_essential,
    hs6_essential_tbl = hs6_categories_essential,

    country_info = country_info,

    # as_of_date is inferred. clamp_future_as_of keeps that inference at or
    # before today: 241 records carry implementation dates out to 2028-10-01
    # (staged phase-ins of one EU sanctions package), which would otherwise set
    # the stock date more than two years past the end of the data.
    clamp_future_as_of = TRUE,

    # by_hs6 and by_cpc are never used downstream and are the memory peak on a full
    # extract (one row per policy x HS6 code with every policy column attached).
    include_hs6 = FALSE,
    include_cpc = FALSE,
    year_max = nipo_year_max,

    # --- Strength chain -------------------------------------------------------
    # Affected-jurisdiction breadth is a directionality property, not domestic
    # intensity, so it no longer multiplies strength; it is still reported, and
    # the NEIS partner-side metrics use the same field. Removing it was the
    # single largest correction to the cross-country ranking.
    include_geo_in_strength = FALSE,
    strength_constant = 1,          # was an undocumented bare 2

    # Trade coverage rather than max(trade, subsidy): the two measure different
    # things, and trade coverage is the more consistent series (44-56% of active
    # policies populated in every year, against 16-35% for subsidy value).
    scale_mode = "exposure",

    # Firm-level test ordered above the named-beneficiary test, so a
    # firm-specific measure scores as firm-targeted whether or not GTA recorded
    # the beneficiary.
    scope_mode = "firm_first",

    # --- Mapping confidence ---------------------------------------------------
    # Confidence is epistemic: how sure we are a policy belongs in a cell, not
    # how interventionist it is. It is reported, not multiplied into strength.
    confidence_mode = "none",

    # --- Attribution ----------------------------------------------------------
    # Policies with no HS6 codes (3,285, carrying 2.96% of strength) stay in
    # their own Cross-cutting row instead of being smeared across all 39
    # technology x stage cells, which conserved the total but invented the
    # attribution and manufactured six otherwise-empty Upstream cells.
    crosscutting_mode = "report_only",

    # EU-wide and member-state rows are both labelled via eu_view rather than
    # either being dropped. NOTE: EU acts are recorded once per member state,
    # and 82.9% of EU-member strength sits in such replicated acts. That is
    # intentional for a country-level index - EU policy a member has outsourced
    # to Brussels is part of that member's stance - but it means EU and non-EU
    # scores are not like-for-like. See NIPO Index.md, Structural limits.
    eu_mode = "both_flagged",

    # --- Aggregation (unchanged by the de-biasing work) -----------------------
    rolling_window_years = 3,
    balance_alpha = 0.5,
    weight_by_active_fraction = TRUE,

    include_neis_panel = TRUE
  )
  rm(nipo_input)

  # Record what the run actually applied, so a stored output is self-describing.
  message(
    "NIPO DIS settings: ",
    paste(names(nipo_policy_out$dis_settings),
          unlist(nipo_policy_out$dis_settings), sep = "=", collapse = ", ")
  )

  # ---- Outputs ---------------------------------------------------------------
  # by_tech_sc is the only table that can reach a composite: it becomes the
  # "NIPO Policy Index" variable in policy_component_tbl, which scripts/20 has kept
  # out of the composite Policy index since Oct 2026 (it stays on disk so it can
  # return). Everything else here is diagnostic.
  nipo_policy_index_tbl <- nipo_policy_out$by_tech_sc
  nipo_tech_year <- nipo_policy_out$by_tech_sc_year

  # Policy-level detail for inspection. mapping_confidence_mean is reported
  # rather than applied (confidence_mode = "none"), so a low value now flags a
  # weak mapping without having silently shrunk the score.
  nipo_policy_all <- nipo_policy_out$by_policy %>%
    select(
      `Implementing Jurisdiction`, Title, domestic_intervention_index,
      `Implementation Date`, `Removal Date`,
      bite_strength_base:policy_strength_pkg,
      tech_csv, supply_chain_csv, mapping_confidence_mean,
      # Reported-only columns added by the de-biasing work: the two inputs to
      # mapping_confidence, the scale terms kept separate, and the real duration
      # signal that m_duration cannot express (it is exactly 1.0 for every
      # in-force measure).
      mapped_share, evidence_mean,
      m_scale_exposure, m_scale_fiscal,
      scale_exposure_available, scale_fiscal_available,
      m_geo, m_geo_applied, partner_n,
      pending_implementation, impl_lag_days,
      observed_duration_days, exposure_days, duration_censored,
      Source, URL
    ) %>%
    arrange(desc(mapping_confidence_mean)) %>%
    arrange(desc(domestic_intervention_index))

  # NEIS framework layers: motive structure, delivery conversion, taper profile,
  # fence posture, inbound exposure and rival direction. Deliberately not a
  # composite - each metric keeps its own column.
  nipo_neis_panel <- nipo_policy_out$neis_panel$panel
  nipo_as_of_date <- max(nipo_policy_index_tbl$as_of_date, na.rm = TRUE)
  rm(nipo_policy_out)
  invisible(gc())

  write_processed_tbl(nipo_tech_year, "nipo_tech_year", processed_dir)
  write_processed_tbl(nipo_policy_index_tbl, "nipo_policy_index_tbl", processed_dir)
  write_processed_tbl(nipo_policy_all, "nipo_policy_all", processed_dir)
  if (!is.null(nipo_neis_panel)) {
    write_processed_tbl(nipo_neis_panel, "nipo_neis_panel", processed_dir)
  }
  } else {
    nipo_scope <- "stored component (not rescored)"
    nipo_as_of_date <- as.Date(NA)
    nipo_policy_index_tbl <- stored_nipo_component
  }


  policy_outputs <- list(
    policy_agg = iea_policy_outputs$outputs$policy_agg,
    policy_clean = iea_policy_outputs$outputs$policy_clean,
    iea = iea_policy_outputs$outputs,
    nipo = nipo_policy_index_tbl,
    # Read back by scripts/20, which warns when a component table carries no provenance.
    provenance = list(
      built_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
      nipo_extract = if (skip_nipo) "stored policy_component_tbl.rds" else basename(nipo_policy_path),
      nipo_scope = nipo_scope,
      nipo_as_of_date = nipo_as_of_date,
      nipo_jurisdictions = dplyr::n_distinct(
        if (skip_nipo) nipo_policy_index_tbl$Country else nipo_policy_index_tbl$country
      ),
      iea_pams_extract = basename(iea_pams_path),
      cat_ratings = basename(cat_policy_path)
    )
  )
  policy_agg <- policy_outputs$policy_agg
  policy_clean <- policy_outputs$policy_clean

  tech_ghg <- partnership_strength_clean_ghg(tech_ghg_raw)
  # CAT labels resolved to WDI names, and EU members without their own rating given the EU
  # bloc rating (R/categories/policy/cat_policy_index.R); rating_scope marks which.
  cat_ratings <- cat_prepare_ratings(cat_policy_raw, country_info = country_info)
  cat_policy_tbl <- partnership_strength_clean_policy(cat_ratings, country_info = country_info) %>%
    dplyr::left_join(dplyr::distinct(cat_ratings, Country, rating_scope), by = "Country")
  cat_policy_index_tbl <- cat_policy_index(tech_ghg, cat_policy_tbl)
  policy_outputs$provenance$cat_eu_bloc_members <- sort(
    cat_ratings$Country[cat_ratings$rating_scope == "EU bloc rating"]
  )
  write_processed_tbl(tech_ghg, "tech_ghg_tbl", processed_dir)

  dual_use_scores_tbl <- clean_dual_use_scores(
    dual_use_scores_raw,
    countries = country_info$country
  )
  dual_use_scores_tbl <- standardize_theme_types(
    dual_use_scores_tbl,
    country_info = country_info
  )
  write_processed_tbl(dual_use_scores_tbl, "dual_use_scores_tbl", processed_dir)

  # iso3c is carried so the rows take the WDI name by code rather than by spelling.
  # standardize_theme_types() needs `category`; without it this step stopped with "object
  # 'category' not found", which is why the June 2026 run wrote the NIPO tables but never
  # policy_component_tbl or policy_outputs.
  if (skip_nipo) {
    nipo_policy_index_tbl <- stored_nipo_component %>%
      standardize_theme_types(country_info = country_info)
  } else {
  nipo_policy_index_tbl <- nipo_policy_index_tbl %>%
    transmute(
      Country = country,
      iso3c = iso3,
      tech,
      supply_chain,
      category = "Policy",
      variable = "NIPO Policy Index",
      data_type = "index",
      value = domestic_intervention_index,
      Year = as.integer(format(as_of_date, "%Y")),
      source = paste0("GTA NIPO (", basename(nipo_policy_path), ")"),
      explanation = paste0(
        "Domestic intervention stock in force on ", format(as_of_date),
        ", S-curved across countries within tech x stage"
      )
    ) %>%
    standardize_theme_types(country_info = country_info)
  }

  policy_component_tbl <- dplyr::bind_rows(
    iea_policy_index_tbl,
    nipo_policy_index_tbl,
    cat_policy_index_tbl,
    dual_use_scores_tbl
  ) %>%
    standardize_theme_types(country_info = country_info)
  write_processed_tbl(policy_component_tbl, "policy_component_tbl", processed_dir)
  write_processed_tbl(policy_outputs, "policy_outputs", processed_dir)

  # Collect all theme outputs in a named list for downstream consumers.
  theme_outputs <- list(
    critical_minerals_processing = critical_minerals_processing_tbl,
    critical_minerals_production = critical_minerals_production_tbl,
    critical_minerals_trade = critical_minerals_trade_tbl,
    energy_access_consumption = energy_access_tbl,
    solar_pv_potential = solar_pv_potential_tbl,
    wind_potential = wind_potential_tbl,
    geothermal_potential = geothermal_potential_tbl,
    energy_consumption = energy_consumption_tbl,
    energy_prices = energy_prices_tbl,
    foreign_dependency = foreign_dependency_tbl,
    import_dependence = import_dependence_tbl,
    reserves = reserves_tbl,
    trade_concentration = trade_concentration_tbl,
    export_feasibility = export_feasibility_tbl,
    overcapacity_premium = overcapacity_premium_tbl,
    future_demand = future_demand_tbl,
    lcoe_competitiveness = lcoe_competitiveness_tbl,
    market_share_manufacturing = market_share_manufacturing_tbl,
    production_depth_momentum = production_depth_momentum_tbl,
    investment_momentum = investment_momentum_tbl,
    technological_readiness = technological_readiness_tbl,
    cost_competitiveness = cost_competitiveness_tbl
  )

