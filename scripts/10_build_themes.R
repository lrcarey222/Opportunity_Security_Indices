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
source(file.path(repo_root, "R", "utils", "iea_critical_minerals.R"))
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
reserves_excel_path <- file.path(raw_data_path, "ei_stat_review_world_energy_wide.xlsx")
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
critmin_import_path <- resolve_versioned_raw_input(
  raw_data_path,
  pattern = "^critmin_import_\\d{4}\\.csv$",
  fallback = "critmin_import_2024.csv",
  label = "Comtrade critical minerals imports"
)
critmin_export_path <- resolve_versioned_raw_input(
  raw_data_path,
  pattern = "^critmin_export_\\d{4}\\.csv$",
  fallback = "critmin_export_2024.csv",
  label = "Comtrade critical minerals exports"
)
critmin_total_export_path <- resolve_versioned_raw_input(
  raw_data_path,
  pattern = "^critmin_total_export_\\d{4}\\.csv$",
  fallback = "critmin_total_export_2024.csv",
  label = "Comtrade critical minerals total exports"
)
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
  pattern = "^IEA_EVDataExplorer\\d{4}\\.xlsx$",
  fallback = "IEA_EVDataExplorer2025.xlsx",
  label = "IEA Global EV Data Explorer"
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



# Fail fast (or skip) if required raw inputs are missing.
missing_files <- c(
  raw_path,
  reserves_excel_path,
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
    iea_ev_data_explorer = iea_ev_path,
    gta_nipo = nipo_policy_path,
    energy_prices_input = imf_price_path
  ),
  raw_data_path
)

country_info <- read.csv(wdi_country_path)
country_info <- standardize_country_info(country_info)


  ei <- read.csv(raw_path)
  # Shared WDI country reference for multiple themes.
  gdp_data <- read.csv(wdi_gdp_path)
  country_reference <- foreign_dependency_build_country_reference(ei, year = 2025)
  

  # Theme: Energy access and consumption (EI data).
  energy_access_tbl <- energy_access_consumption(ei, country_info = country_info)
  energy_access_tbl <- standardize_theme_types(energy_access_tbl, country_info = country_info)
  write_processed_tbl(energy_access_tbl, "energy_access_tbl", processed_dir)

  # Theme: Solar PV potential (Global Solar Atlas GIS data).
  solar_pv_raw <- read.csv(solar_pv_potential_path)
  solar_pv_potential_tbl <- solar_pv_potential(solar_pv_raw)
  solar_pv_potential_tbl <- standardize_theme_types(
    solar_pv_potential_tbl,
    country_info = country_info
  )
  write_processed_tbl(solar_pv_potential_tbl, "solar_pv_potential_tbl", processed_dir)

  # Theme: Wind potential (Global Wind Atlas country data).
  wind_raw <- read.csv(wind_potential_path)
  wind_potential_tbl <- wind_potential(wind_raw)
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
  import_dependence_tbl <- import_dependence(ei)
  import_dependence_tbl <- standardize_theme_types(import_dependence_tbl, country_info = country_info)
  write_processed_tbl(import_dependence_tbl, "import_dependence_tbl", processed_dir)

  # Theme: Foreign dependency inputs (critical minerals + IEA datasets).
  critical <- read_iea_critical_minerals(critical_minerals_path)
  mineral_demand_clean <- reserves_build_mineral_demand_clean(critical)

  reserve_inputs <- lapply(reserves_specs(), function(spec) {
    spec$data <- readxl::read_excel(reserves_excel_path, sheet = spec$sheet, skip = spec$skip)
    spec
  })

  reserves_tbl <- reserves(ei, reserve_inputs, mineral_demand_clean)
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

  
  # Theme: Technological readiness (IEA Clean Tech Guide).
  # The 2026 public dataset ships with a UTF-8 BOM, which would otherwise land in the first
  # column name and hide it from the layout check. The TRL window (2020-2025 for the 2026
  # release) is read off whichever TRL columns the file carries.
  iea_cleantech_guide <- read.csv(iea_cleantech_guide_path, fileEncoding = "UTF-8-BOM")
  technological_readiness_tbl <- technological_readiness(
    iea_cleantech_all = iea_cleantech_guide
  )
  write_processed_tbl(
    technological_readiness_tbl,
    "technological_readiness_tbl",
    processed_dir
  )

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
  critical_minerals_trade_tbl <- critical_minerals_trade(
    critmin_import = critmin_import,
    critmin_export = critmin_export,
    total_export = total_export,
    mineral_demand_clean = mineral_demand_clean,
    country_info = country_info
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
    )
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

  aec_4_data <- read.csv(trade_hs4_path)
  aec_6_data <- read.csv(trade_hs6_path)
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

  trade_concentration_tbl <- trade_concentration(
    subcat = subcat,
    aec_4_data = aec_4_data,
    aec_6_data = aec_6_data,
    comtrade_trade = comtrade_energy_trade,
    comtrade_total_export = comtrade_total_export,
    country_info = country_info,
    year_comtrade = getOption("opportunity_security.comtrade_year"),
    include_sub_sector = include_sub_sector
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
    country_info = country_info,
    gdp_data = gdp_data,
    comtrade_trade = comtrade_energy_trade,
    comtrade_total_export = comtrade_total_export,
    year_comtrade = getOption("opportunity_security.comtrade_year"),
    include_sub_sector = include_sub_sector
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

  # Theme: Future demand (IEA + BNEF + EV + BCG data).
  iea_weo <- read.csv(iea_weo_path)
  iea_ev <- readxl::read_excel(iea_ev_path, sheet = 1)
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

  # Theme: Production depth + momentum (EI + IEA critical minerals).
  production_depth_momentum_tbl <- production_depth_momentum(
    ei = ei,
    critical = critical,
    mineral_demand_clean = mineral_demand_clean,
    country_info = country_info
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
  investment_momentum_tbl <- investment_momentum_from_excel(
    investment_monitor_path,
    country_reference = country_reference$Country
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
  ilo_url <- "https://rplumber.ilo.org/data/indicator/?id=EAR_EMTA_SEX_ECO_CUR_NB_A&lang=en&type=label&format=.csv&channel=ilostat&title=average-monthly-earnings-of-employees-by-sex-economic-activity-and-currency-annual"
  ilo_raw <- read.csv(ilo_url)
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
  nipo_raw <- readxl::read_excel(nipo_policy_path, sheet = 1)
  hs6_categories_essential <- readr::read_csv(file.path(
    raw_data_path,"hs6_categories_with_essential.csv")) %>%
    rename("Value.Chain"="Value Chain")
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
  
  ally_iso3 <- c(
    "USA", "CAN", "JPN", "AUS", "IND", "MEX", "KOR", "GBR", "DEU", "FRA", "ITA", "BRA", "SAU",
    "ZAF", "IDN", "NOR", "ARE", "VNM", "KEN", "DNK", "ARG", "MAR", "CHL"
  )

  country_lookup <- country_info %>%
    dplyr::transmute(
      country = standardize_country_names(.data$country),
      iso3c = toupper(as.character(.data$iso3c))
    )

  nipo_allies <- nipo_raw %>%
    dplyr::mutate(
      implementing_country = standardize_country_names(.data$`Implementing Jurisdiction`)
    ) %>%
    dplyr::left_join(country_lookup, by = c("implementing_country" = "country")) %>%
    dplyr::rename(iso = .data$iso3c) %>%
    dplyr::filter(.data$iso %in% ally_iso3)

  
  nipo_us<-nipo_raw %>%
    filter(`Implementing Jurisdiction`=="United States of America")
  
  nipo_policy_out <- nipo_domestic_intervention_outputs(
    raw_nipo = nipo_raw,
    hs6_categories_essential,
    country_info = country_info,
    rolling_window_years = 3,
    balance_alpha = 0.5,
    weight_by_active_fraction = TRUE
  )


  nipo_policy_all <- nipo_policy_out$by_policy%>% 
    select(`Implementing Jurisdiction`,Title,domestic_intervention_index,
           `Implementation Date`,`Removal Date`, bite_strength_base:policy_strength_pkg,
           tech_csv,supply_chain_csv,mapping_confidence_mean,Source,URL) %>%
    arrange(desc(mapping_confidence_mean)) %>%
    arrange(desc(domestic_intervention_index))
  
  nipo_hs6 <- nipo_policy_out$by_hs6
  nipo_tech_year <- nipo_policy_out$by_tech_sc_year
  nipo_policy_cpc <- nipo_policy_out$by_cpc
  nipo_policy_index_tbl <- nipo_policy_out$by_tech_sc
  
  write_processed_tbl(nipo_tech_year, "nipo_tech_year", processed_dir)
  write_processed_tbl(nipo_policy_index_tbl, "nipo_policy_index_tbl", processed_dir)
  write_processed_tbl(nipo_policy_all, "nipo_policy_all", processed_dir)
  
  
  policy_outputs <- list(
    policy_agg = iea_policy_outputs$outputs$policy_agg,
    policy_clean = iea_policy_outputs$outputs$policy_clean,
    iea = iea_policy_outputs$outputs,
    nipo = nipo_policy_index_tbl
  )
  policy_agg <- policy_outputs$policy_agg
  policy_clean <- policy_outputs$policy_clean

  tech_ghg <- partnership_strength_clean_ghg(tech_ghg_raw)
  cat_policy_tbl <- partnership_strength_clean_policy(cat_policy_raw,country_info=country_info)
  cat_policy_index_tbl <- cat_policy_index(tech_ghg, cat_policy_tbl)
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

  nipo_policy_index_tbl <- nipo_policy_index_tbl %>%
    transmute(
      Country = country,
      tech,
      supply_chain,
      variable = "NIPO Policy Index",
      data_type = "Index",
      value = domestic_intervention_index,
      Year = 2026,
      source = "NIPO",
      explanation = "See README"
    ) %>%
    standardize_theme_types(country_info = country_info)

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

