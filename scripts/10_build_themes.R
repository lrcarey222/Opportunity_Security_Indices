# Build theme-level tables.
if (!exists("repo_root")) {
  repo_root <- resolve_repo_root()
}

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "themes", "energy_security", "overall_index.R"))
source(file.path(repo_root, "R", "themes", "energy_security", "critical_minerals_processing.R"))
source(file.path(repo_root, "R", "themes", "energy_security", "critical_minerals_production.R"))
source(file.path(repo_root, "R", "themes", "energy_security", "critical_minerals_trade.R"))
source(file.path(repo_root, "R", "themes", "energy_security", "energy_access_consumption.R"))
source(file.path(repo_root, "R", "themes", "energy_security", "energy_consumption.R"))
source(file.path(repo_root, "R", "themes", "energy_security", "energy_prices.R"))
source(file.path(repo_root, "R", "themes", "energy_security", "foreign_dependency.R"))
source(file.path(repo_root, "R", "themes", "energy_security", "import_dependence.R"))
source(file.path(repo_root, "R", "themes", "energy_security", "reserves.R"))
source(file.path(repo_root, "R", "themes", "energy_security", "trade_concentration.R"))
source(file.path(repo_root, "R", "themes", "economic_opportunity", "future_demand.R"))
source(file.path(repo_root, "R", "themes", "economic_opportunity", "export_feasibility.R"))
source(file.path(repo_root, "R", "themes", "economic_opportunity", "lcoe_competitiveness.R"))
source(file.path(repo_root, "R", "themes", "economic_opportunity", "market_share_manufacturing.R"))
source(file.path(repo_root, "R", "themes", "economic_opportunity", "production_depth_momentum.R"))

standardize_theme_types <- function(tbl, country_info = NULL) {
  if (is.null(tbl)) {
    return(tbl)
  }

  standardized <- tbl %>%
    dplyr::mutate(
      Country = as.character(Country),
      tech = as.character(tech),
      supply_chain = as.character(supply_chain),
      sub_sector = if ("sub_sector" %in% names(tbl)) as.character(sub_sector) else NULL,
      category = as.character(category),
      variable = as.character(variable),
      data_type = as.character(data_type),
      Year = suppressWarnings(as.integer(stringr::str_extract(as.character(Year), "\\d{4}$"))),
      value = suppressWarnings(as.numeric(value)),
      source = as.character(source),
      explanation = as.character(explanation)
    )

  standardize_country_table(standardized, country_info = country_info)
}

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
trade_codes_path <- file.path(latest_snapshot, "consolidated_hs6_energy_tech_long.csv")
trade_hs4_path <- file.path(latest_snapshot, "hs92_country_product_year_4.csv")
trade_hs6_path <- file.path(latest_snapshot, "hs92_country_product_year_6.csv")
comtrade_energy_trade_path <- file.path(latest_snapshot, "comtrade_energy_trade.csv")
comtrade_total_export_path <- file.path(latest_snapshot, "comtrade_total_export.csv")
bnef_neo_path <- file.path(latest_snapshot, "2024-10-29 - New Energy Outlook 2024.csv")
wdi_gdp_path <- file.path(latest_snapshot, "wdi_gdp.csv")
wdi_country_path <- file.path(latest_snapshot, "wdi_country_info.csv")
critmin_import_path <- file.path(latest_snapshot, "critmin_import_2024.csv")
critmin_export_path <- file.path(latest_snapshot, "critmin_export_2024.csv")
critmin_total_export_path <- file.path(latest_snapshot, "critmin_total_export_2024.csv")
energy_prices_lcoe_path <- file.path(latest_snapshot, "2025-03-24 - 2025 LCOE Data Viewer Tool.csv")
iea_weo_path <- file.path(latest_snapshot, "WEO2024_AnnexA_Free_Dataset_World.csv")
iea_ev_path <- file.path(latest_snapshot, "IEA_EVDataExplorer2025.xlsx")
bcg_future_demand_path <- file.path(latest_snapshot, "Market Size for Technology and Supply Chain.xlsx")
bnef_supply_chain_path <- file.path(latest_snapshot, "BNEF_Energy Transition Supply Chains 2025.xlsx")

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
  bnef_supply_chain_path
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
country_info <- read.csv(wdi_country_path)
country_info <- standardize_country_info(country_info)

# Theme: Energy access and consumption (EI data).
energy_access_tbl <- energy_access_consumption(ei)
energy_access_tbl <- standardize_theme_types(energy_access_tbl, country_info = country_info)

# Theme: Import dependence (EI data).
import_dependence_tbl <- import_dependence(ei)
import_dependence_tbl <- standardize_theme_types(import_dependence_tbl, country_info = country_info)

# Theme: Foreign dependency inputs (critical minerals + IEA datasets).
critical <- read.csv(critical_minerals_path)
mineral_demand_clean <- reserves_build_mineral_demand_clean(critical)

reserve_inputs <- lapply(reserves_specs(), function(spec) {
  spec$data <- readxl::read_excel(reserves_excel_path, sheet = spec$sheet, skip = spec$skip)
  spec
})

reserves_tbl <- reserves(ei, reserve_inputs, mineral_demand_clean)
reserves_tbl <- standardize_theme_types(reserves_tbl, country_info = country_info)
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

# Shared WDI country reference for multiple themes.
gdp_data <- read.csv(wdi_gdp_path)
country_reference <- foreign_dependency_build_country_reference(ei, year = 2024)

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

# Theme: Energy consumption (EI + BNEF data).
bnef_neo <- read.csv(bnef_neo_path, skip = 2)
energy_consumption_tbl <- energy_consumption(
  ei = ei,
  bnef_neo = bnef_neo,
  country_info = country_info
)
energy_consumption_tbl <- standardize_theme_types(energy_consumption_tbl, country_info = country_info)

# Theme: Energy prices (EI + BNEF data).
gas_price_sheet <- readxl::read_excel(reserves_excel_path, sheet = 40, skip = 3)
coal_price_sheet <- readxl::read_excel(reserves_excel_path, sheet = 50, skip = 3)
lcoe_bnef <- read.csv(energy_prices_lcoe_path, skip = 8)
energy_prices_tbl <- energy_prices(
  ei = ei,
  gas_price_sheet = gas_price_sheet,
  coal_price_sheet = coal_price_sheet,
  lcoe_bnef = lcoe_bnef
)
energy_prices_tbl <- standardize_theme_types(energy_prices_tbl, country_info = country_info)

# Theme: LCOE competitiveness (BNEF data).
lcoe_competitiveness_tbl <- lcoe_competitiveness(lcoe_bnef = lcoe_bnef)
lcoe_competitiveness_tbl <- standardize_theme_types(
  lcoe_competitiveness_tbl,
  country_info = country_info
)

# Theme: Trade concentration (Atlas data + WDI country reference).
subcat <- read.csv(trade_codes_path)
aec_4_data <- read.csv(trade_hs4_path)
aec_6_data <- read.csv(trade_hs6_path)
comtrade_energy_trade <- read.csv(comtrade_energy_trade_path)
comtrade_total_export <- read.csv(comtrade_total_export_path)
include_sub_sector <- isTRUE(config$energy_security_include_sub_sector)

trade_concentration_tbl <- trade_concentration(
  subcat = subcat,
  aec_4_data = aec_4_data,
  aec_6_data = aec_6_data,
  comtrade_trade = comtrade_energy_trade,
  comtrade_total_export = comtrade_total_export,
  country_info = country_info,
  gdp_data = gdp_data,
  include_sub_sector = include_sub_sector
)
trade_concentration_tbl <- standardize_theme_types(trade_concentration_tbl, country_info = country_info)

# Theme: Export feasibility (Atlas/Comtrade trade data).
export_feasibility_tbl <- export_feasibility(
  aec_4_data = aec_4_data,
  aec_6_data = aec_6_data,
  subcat = subcat,
  country_info = country_info,
  gdp_data = gdp_data
)
export_feasibility_tbl <- standardize_theme_types(export_feasibility_tbl, country_info = country_info)

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

# Collect all theme outputs in a named list for downstream consumers.
theme_outputs <- list(
  critical_minerals_processing = critical_minerals_processing_tbl,
  critical_minerals_production = critical_minerals_production_tbl,
  critical_minerals_trade = critical_minerals_trade_tbl,
  energy_access_consumption = energy_access_tbl,
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
  production_depth_momentum = production_depth_momentum_tbl
)

invisible(theme_outputs)
# build_themes (placeholder).
# TODO: implement.
build_themes_stub <- function() {
  NULL
}
