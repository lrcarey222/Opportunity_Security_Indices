# Build theme-level tables.
if (!exists("repo_root")) {
  repo_root <- resolve_repo_root()
}

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "levels.R"))
source(file.path(repo_root, "R", "categories", "shared", "overall_index.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(repo_root, "R", "categories", "policy", "iea_policy_index.R"))
source(file.path(repo_root, "R", "categories", "policy", "cat_policy_index.R"))
source(file.path(repo_root, "R", "categories", "policy", "dual_use_scores.R"))
source(file.path(repo_root, "R", "categories", "trade", "trade_core.R"))
source(file.path(repo_root, "R", "categories", "foreign_dependency", "critical_minerals_processing.R"))
source(file.path(repo_root, "R", "categories", "production", "critical_minerals_production.R"))
source(file.path(repo_root, "R", "categories", "minerals_trade", "critical_minerals_trade.R"))
source(file.path(repo_root, "R", "categories", "energy_access", "energy_access_consumption.R"))
source(file.path(repo_root, "R", "categories", "energy_access", "solar_pv_potential.R"))
source(file.path(repo_root, "R", "categories", "energy_access", "wind_potential.R"))
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
source(file.path(repo_root, "R", "categories", "economic opportunity", "cost_competitiveness.R"))
source(file.path(repo_root, "R", "categories", "technological_readiness", "technological_readiness.R"))

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

manifest_path <- file.path(repo_root, "config", "raw_inputs_manifest.yml")
if (!file.exists(manifest_path)) {
  stop("Raw inputs manifest not found: ", manifest_path)
}
raw_manifest <- yaml::read_yaml(manifest_path)
if (length(raw_manifest) == 0) {
  stop("Raw inputs manifest is empty: ", manifest_path)
}

find_manifest_path <- function(pattern, label) {
  hits <- vapply(raw_manifest, function(entry) {
    is.character(entry$path) && stringr::str_detect(entry$path, pattern)
  }, logical(1))
  if (!any(hits)) {
    stop(label, " not found in raw inputs manifest.")
  }
  if (sum(hits) > 1) {
    stop("Multiple entries found for ", label, "; keep only one entry.")
  }
  raw_manifest[[which(hits)[1]]]$path
}

# Assemble required raw file paths for theme builders.
raw_path <- file.path(latest_snapshot, "ei_stat_review_world_energy.csv")
reserves_excel_path <- file.path(latest_snapshot, "ei_stat_review_world_energy_wide.xlsx")
critical_minerals_path <- file.path(latest_snapshot, "iea_criticalminerals_25.csv")
cleantech_midstream_path <- file.path(latest_snapshot, "iea_cleantech_Midstream.csv")
iea_cleantech_guide_path <- file.path(latest_snapshot, "IEA_Clean_Tech_Guide.csv")
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
relative_costs_iea_path <- file.path(latest_snapshot, "Relative_Costs_IEA.csv")
imf_lending_rates_path <- file.path(latest_snapshot, "imf_lending_rates.csv")
imf_ppi_path <- file.path(latest_snapshot, "imf_ppi.csv")
imf_commodity_prices_path <- file.path(latest_snapshot, "imf_commodity_prices.csv")
solar_pv_potential_path <- file.path(latest_snapshot, "solar_potential_clean.csv")
wind_potential_path <- file.path(latest_snapshot, "wb_wind_country.csv")
iea_pams_path <- file.path(
  latest_snapshot,
  find_manifest_path("IEA_PAMS_Export", "PAMS export")
)
tech_ghg_path <- file.path(
  latest_snapshot,
  find_manifest_path("ipcc_ghg_intensity.csv$", "IPCC GHG intensity")
)
cat_policy_path <- file.path(
  latest_snapshot,
  find_manifest_path("CAT_country ratings data.csv$", "CAT policy ratings")
)
dual_use_scores_path <- file.path(
  latest_snapshot,
  find_manifest_path("dual_use_scores_primary_secondary_tertiary.csv$", "Dual-use scores")
)

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
  imf_commodity_prices_path,
  solar_pv_potential_path,
  iea_pams_path,
  tech_ghg_path,
  cat_policy_path,
  dual_use_scores_path
)
missing_files <- missing_files[!file.exists(missing_files)]

if (length(missing_files) > 0 && !skip_data_downloads) {
  expected_list <- paste0("- ", missing_files)
  stop("Missing required raw data. Expected raw files:\n", expected_list)
}

if (length(missing_files) > 0 && skip_data_downloads) {
  message("Skipping raw data lookup; missing file(s): ", paste(missing_files, collapse = ", "))
  theme_outputs <- list()
} else {
  ei <- read.csv(raw_path)
  country_info <- read.csv(wdi_country_path)
  country_info <- standardize_country_info(country_info)

  # Theme: Energy access and consumption (EI data).
  energy_access_tbl <- energy_access_consumption(ei)
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

  # Theme: Import dependence (EI data).
  import_dependence_tbl <- import_dependence(ei)
  import_dependence_tbl <- standardize_theme_types(import_dependence_tbl, country_info = country_info)
  write_processed_tbl(import_dependence_tbl, "import_dependence_tbl", processed_dir)

  # Theme: Foreign dependency inputs (critical minerals + IEA datasets).
  critical <- read.csv(critical_minerals_path)
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

  # Shared WDI country reference for multiple themes.
  gdp_data <- read.csv(wdi_gdp_path)
  country_reference <- foreign_dependency_build_country_reference(ei, year = 2024)

  # Theme: Technological readiness (IEA Clean Tech Guide).
  iea_cleantech_guide <- read.csv(iea_cleantech_guide_path)
  technological_readiness_tbl <- technological_readiness(
    iea_cleantech_all = iea_cleantech_guide
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
  imf_commodity_prices <- read.csv(imf_commodity_prices_path)
  energy_prices_tbl <- energy_prices(
    imf_price = imf_commodity_prices,
    mineral_demand_clean = mineral_demand_clean,
    country_info = country_info
  )
  energy_prices_tbl <- standardize_theme_types(energy_prices_tbl, country_info = country_info)
  write_processed_tbl(energy_prices_tbl, "energy_prices_tbl", processed_dir)

  # Theme: LCOE competitiveness (BNEF data).
  lcoe_bnef <- read.csv(energy_prices_lcoe_path, skip = 8)
  lcoe_competitiveness_tbl <- lcoe_competitiveness(lcoe_bnef = lcoe_bnef)
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
  subcat <- read.csv(trade_codes_path)
  aec_4_data <- read.csv(trade_hs4_path)
  aec_6_data <- read.csv(trade_hs6_path)
  comtrade_energy_trade <- read.csv(comtrade_energy_trade_path)
  comtrade_total_export <- read.csv(comtrade_total_export_path)
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
    gdp_data = gdp_data,
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

  # Theme: Cost competitiveness (IEA relative costs).
  iea_relative_costs <- read.csv(relative_costs_iea_path)
  ilo_url <- "https://rplumber.ilo.org/data/indicator/?id=EAR_4MTH_SEX_ECO_CUR_NB_A&lang=en&type=label&format=.csv&channel=ilostat&title=average-monthly-earnings-of-employees-by-sex-and-economic-activity-annual"
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
  tech_ghg_raw <- readr::read_csv(tech_ghg_path, show_col_types = FALSE) 
  cat_policy_raw <- readr::read_csv(cat_policy_path, show_col_types = FALSE)
  dual_use_scores_raw <- readr::read_csv(dual_use_scores_path, show_col_types = FALSE)

  iea_policy_outputs <- iea_policy_index(pams_raw, split_strength = FALSE)
  iea_policy_index_tbl <- iea_policy_outputs$index_tbl
  policy_outputs <- iea_policy_outputs$outputs
  policy_agg <- policy_outputs$policy_agg
  policy_clean <- policy_outputs$policy_clean

  tech_ghg <- partnership_strength_clean_ghg(tech_ghg_raw)
  cat_policy_tbl <- partnership_strength_clean_policy(cat_policy_raw,country_info=country_info)
  cat_policy_index_tbl <- cat_policy_index(tech_ghg, cat_policy_tbl)
  write_processed_tbl(tech_ghg, "tech_ghg_tbl", processed_dir)

  dual_use_scores_tbl <- clean_dual_use_scores(dual_use_scores_raw)
  dual_use_scores_tbl <- standardize_theme_types(
    dual_use_scores_tbl,
    country_info = country_info
  )
  write_processed_tbl(dual_use_scores_tbl, "dual_use_scores_tbl", processed_dir)

  policy_component_tbl <- dplyr::bind_rows(
    iea_policy_index_tbl,
    cat_policy_index_tbl,
    dual_use_scores_tbl
  )
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
    technological_readiness = technological_readiness_tbl,
    cost_competitiveness = cost_competitiveness_tbl
  )
}

invisible(theme_outputs)
# build_themes (placeholder).
# TODO: implement.
build_themes_stub <- function() {
  NULL
}
