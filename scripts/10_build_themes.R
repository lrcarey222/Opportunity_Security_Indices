# Build theme-level tables.
if (!exists("repo_root")) {
  repo_root <- resolve_repo_root()
}

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "themes", "energy_security", "critical_minerals_processing.R"))
source(file.path(repo_root, "R", "themes", "energy_security", "critical_minerals_production.R"))
source(file.path(repo_root, "R", "themes", "energy_security", "critical_minerals_trade.R"))
source(file.path(repo_root, "R", "themes", "energy_security", "energy_access_consumption.R"))
source(file.path(repo_root, "R", "themes", "energy_security", "energy_consumption.R"))
source(file.path(repo_root, "R", "themes", "energy_security", "foreign_dependency.R"))
source(file.path(repo_root, "R", "themes", "energy_security", "import_dependence.R"))
source(file.path(repo_root, "R", "themes", "energy_security", "reserves.R"))
source(file.path(repo_root, "R", "themes", "energy_security", "trade_concentration.R"))

config <- getOption("opportunity_security.config")
if (is.null(config)) {
  stop("Config not loaded; run scripts/00_setup.R first.")
}

skip_data_downloads <- Sys.getenv("SKIP_DATA_DOWNLOADS") == "1"

raw_data_dir <- config$raw_data_dir
if (is.null(raw_data_dir)) {
  stop("Config missing raw_data_dir.")
}

latest_raw_snapshot <- function(root_dir, raw_data_dir, skip_data_downloads = FALSE) {
  raw_base_dir <- file.path(root_dir, raw_data_dir)
  if (!dir.exists(raw_base_dir)) {
    if (skip_data_downloads) {
      message("Skipping raw data lookup; directory not found: ", raw_base_dir)
      return(NULL)
    }
    stop("Raw data directory not found: ", raw_base_dir)
  }

  snapshot_dirs <- list.dirs(raw_base_dir, recursive = FALSE, full.names = TRUE)
  if (length(snapshot_dirs) == 0) {
    if (skip_data_downloads) {
      message("Skipping raw data lookup; no snapshots in: ", raw_base_dir)
      return(NULL)
    }
    stop("No raw data snapshots found in: ", raw_base_dir)
  }

  snapshot_info <- file.info(snapshot_dirs)
  snapshot_dirs[order(snapshot_info$mtime, decreasing = TRUE)][1]
}

latest_snapshot <- latest_raw_snapshot(repo_root, raw_data_dir, skip_data_downloads)
if (is.null(latest_snapshot)) {
  invisible(list())
  return()
}

# Assemble required raw file paths for theme builders.
raw_path <- file.path(latest_snapshot, "ei_stat_review_world_energy.csv")
reserves_excel_path <- file.path(latest_snapshot, "ei_stat_review_world_energy_wide.xlsx")
critical_minerals_path <- file.path(latest_snapshot, "iea_criticalminerals_25.csv")
cleantech_midstream_path <- file.path(latest_snapshot, "iea_cleantech_Midstream.csv")
ev_midstream_path <- file.path(latest_snapshot, "ev_Midstream_capacity.csv")
trade_codes_path <- file.path(latest_snapshot, "hts_codes_categories_bolstered_final.csv")
trade_hs4_path <- file.path(latest_snapshot, "hs92_country_product_year_4.csv")
trade_hs6_path <- file.path(latest_snapshot, "hs92_country_product_year_6.csv")
bnef_neo_path <- file.path(latest_snapshot, "2024-10-29 - New Energy Outlook 2024.csv")
wdi_gdp_path <- file.path(latest_snapshot, "wdi_gdp.csv")
wdi_country_path <- file.path(latest_snapshot, "wdi_country_info.csv")
critmin_import_path <- file.path(latest_snapshot, "critmin_import_2024.csv")
critmin_export_path <- file.path(latest_snapshot, "critmin_export_2024.csv")
critmin_total_export_path <- file.path(latest_snapshot, "critmin_total_export_2024.csv")

# Fail fast (or skip) if required raw inputs are missing.
missing_files <- c(
  raw_path,
  reserves_excel_path,
  critical_minerals_path,
  cleantech_midstream_path,
  ev_midstream_path,
  trade_codes_path,
  trade_hs4_path,
  trade_hs6_path,
  bnef_neo_path,
  wdi_gdp_path,
  wdi_country_path,
  critmin_import_path,
  critmin_export_path,
  critmin_total_export_path
)
missing_files <- missing_files[!file.exists(missing_files)]

if (length(missing_files) > 0) {
  if (skip_data_downloads) {
    message("Skipping raw data lookup; missing file(s): ", paste(missing_files, collapse = ", "))
    invisible(list())
    return()
  }
  expected_list <- paste0("- ", missing_files)
  stop("Missing required raw data. Expected raw files:\n", expected_list)
}

ei <- read.csv(raw_path)

# Theme: Energy access and consumption (EI data).
energy_access_tbl <- energy_access_consumption(ei)

# Theme: Import dependence (EI data).
import_dependence_tbl <- import_dependence(ei)

# Theme: Foreign dependency inputs (critical minerals + IEA datasets).
critical <- read.csv(critical_minerals_path)
mineral_demand_clean <- reserves_build_mineral_demand_clean(critical)

reserve_inputs <- lapply(reserves_specs(), function(spec) {
  spec$data <- readxl::read_excel(reserves_excel_path, sheet = spec$sheet, skip = spec$skip)
  spec
})

reserves_tbl <- reserves(ei, reserve_inputs, mineral_demand_clean)
cleantech_midstream <- read.csv(cleantech_midstream_path)
ev_midstream <- read.csv(ev_midstream_path)
foreign_dependency_tbl <- foreign_dependency(
  critical = critical,
  mineral_demand_clean = mineral_demand_clean,
  ei = ei,
  cleantech_midstream = cleantech_midstream,
  ev_midstream = ev_midstream
)

# Shared WDI country reference for multiple themes.
gdp_data <- read.csv(wdi_gdp_path)
country_info <- read.csv(wdi_country_path)
country_reference <- foreign_dependency_build_country_reference(ei, year = 2024)

# Theme: Critical minerals processing (IEA data).
critical_minerals_processing_tbl <- critical_minerals_processing(
  critical = critical,
  mineral_demand_clean = mineral_demand_clean,
  country_info = country_info,
  country_reference = country_reference
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

# Theme: Energy consumption (EI + BNEF data).
bnef_neo <- read.csv(bnef_neo_path, skip = 2)
energy_consumption_tbl <- energy_consumption(
  ei = ei,
  bnef_neo = bnef_neo,
  country_info = country_info
)

# Theme: Trade concentration (Atlas data + WDI country reference).
subcat <- read.csv(trade_codes_path)
aec_4_data <- read.csv(trade_hs4_path)
aec_6_data <- read.csv(trade_hs6_path)

trade_concentration_tbl <- trade_concentration(
  subcat = subcat,
  aec_4_data = aec_4_data,
  aec_6_data = aec_6_data,
  country_info = country_info,
  gdp_data = gdp_data
)

# Collect all theme outputs in a named list for downstream consumers.
theme_outputs <- list(
  critical_minerals_processing = critical_minerals_processing_tbl,
  critical_minerals_production = critical_minerals_production_tbl,
  critical_minerals_trade = critical_minerals_trade_tbl,
  energy_access_consumption = energy_access_tbl,
  energy_consumption = energy_consumption_tbl,
  foreign_dependency = foreign_dependency_tbl,
  import_dependence = import_dependence_tbl,
  reserves = reserves_tbl,
  trade_concentration = trade_concentration_tbl
)

invisible(theme_outputs)
# build_themes (placeholder).
# TODO: implement.
build_themes_stub <- function() {
  NULL
}
