# Build partnership strength theme tables.
if (!exists("repo_root")) {
  repo_root <- resolve_repo_root()
}

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "indices", "aggregate_economic_opportunity_index.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "prosperous_opportunity.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "safer_friendshore.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "stronger_development.R"))

config <- getOption("opportunity_security.config")
weights <- getOption("opportunity_security.weights")
if (is.null(config)) {
  stop("Config not loaded; run scripts/00_setup.R first.")
}
if (is.null(weights)) {
  stop("Weights not loaded; run scripts/00_setup.R first.")
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

read_index_outputs <- function(config, processed_dir) {
  outputs_rds_path <- Sys.getenv("OPSI_OUTPUTS_RDS", "")
  if (!nzchar(outputs_rds_path)) {
    outputs_rds_path <- if (!is.null(config$outputs_rds) && nzchar(config$outputs_rds)) {
      file.path(repo_root, config$outputs_rds)
    } else {
      file.path(processed_dir, "outputs", "index_outputs.rds")
    }
  }
  if (!file.exists(outputs_rds_path)) {
    stop("Index outputs not found: ", outputs_rds_path)
  }
  readRDS(outputs_rds_path)
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

comtrade_dyads_path <- file.path(latest_snapshot, "allied_comtrade_energy_data.csv")
subcat_path <- file.path(latest_snapshot, "consolidated_hs6_energy_tech_long.csv")
fdi_path <- file.path(latest_snapshot, "imf_dip.csv")
tech_ghg_path <- file.path(latest_snapshot, "ipcc_ghg_intensity.csv")
cat_policy_path <- file.path(latest_snapshot, "CAT_country ratings data.csv")
country_info_path <- file.path(latest_snapshot, "wdi_country_info.csv")
country_gdp_path <- file.path(latest_snapshot, "wdi_gdp.csv")
wb_doingbusiness_path <- file.path(latest_snapshot, "wb_doingbusiness.csv")
wb_wdi_path <- file.path(latest_snapshot, "wb_wdi.csv")
oecd_1215_path <- file.path(latest_snapshot, "oecd_1215.csv")
oecd_1518_path <- file.path(latest_snapshot, "oecd_1518.csv")
oecd_1823_path <- file.path(latest_snapshot, "oecd_1823.csv")
oecd_api_path <- file.path(latest_snapshot, "oecd_crs_api.csv")

missing_files <- c(
  comtrade_dyads_path,
  subcat_path,
  fdi_path,
  tech_ghg_path,
  cat_policy_path,
  country_info_path,
  country_gdp_path,
  wb_doingbusiness_path,
  wb_wdi_path,
  oecd_1215_path,
  oecd_1518_path,
  oecd_1823_path,
  oecd_api_path
)
missing_files <- missing_files[!file.exists(missing_files)]

if (length(missing_files) > 0) {
  if (skip_data_downloads) {
    message("Skipping partnership themes; missing file(s): ", paste(missing_files, collapse = ", "))
    invisible(list())
    return()
  }
  expected_list <- paste0("- ", missing_files)
  stop("Missing required raw data. Expected raw files:\n", expected_list)
}

if (!exists("economic_opportunity_outputs") || !exists("energy_security_outputs")) {
  index_outputs <- read_index_outputs(config, processed_dir)
  economic_opportunity_outputs <- index_outputs$economic_opportunity_outputs
  energy_security_outputs <- index_outputs$energy_security_outputs
}

comtrade_dyads <- read.csv(comtrade_dyads_path)
subcat <- read.csv(subcat_path)
fdi_raw <- read.csv(fdi_path)
tech_ghg_raw <- read.csv(tech_ghg_path)
cat_raw <- read.csv(cat_policy_path)
country_info <- read.csv(country_info_path)
country_gdp <- read.csv(country_gdp_path)
wb_doingbusiness <- read.csv(wb_doingbusiness_path)
wb_wdi <- read.csv(wb_wdi_path)
oecd_1215 <- read.csv(oecd_1215_path)
oecd_1518 <- read.csv(oecd_1518_path)
oecd_1823 <- read.csv(oecd_1823_path)
oecd_api_raw <- read.csv(oecd_api_path)

country_info <- standardize_country_info(country_info)

econ_opp_index <- economic_opportunity_outputs$index
energy_security_index <- energy_security_outputs$index

tech_ghg <- partnership_strength_clean_ghg(tech_ghg_raw)
policy_tbl <- partnership_strength_clean_policy(cat_raw)

friendshore_outputs <- safer_friendshore(
  comtrade_dyads = comtrade_dyads,
  subcat = subcat,
  econ_opp_index = econ_opp_index,
  energy_security_index = energy_security_index,
  tech_ghg = tech_ghg,
  policy = policy_tbl,
  fdi_raw = fdi_raw,
  country_info = country_info,
  gdp_data = country_gdp,
  component_weights = weights$partnership_friendshore_components
)

partner_friendshore_tbl <- friendshore_outputs$dyads
partner_friendshore_country_tbl <- friendshore_outputs$country
partner_friendshore_inputs_tbl <- friendshore_outputs$inputs_country
write_processed_tbl(partner_friendshore_tbl, "partner_friendshore_tbl", processed_dir)
write_processed_tbl(
  partner_friendshore_country_tbl,
  "partner_friendshore_country_tbl",
  processed_dir
)
write_processed_tbl(
  partner_friendshore_inputs_tbl,
  "partner_friendshore_inputs_tbl",
  processed_dir
)

opportunity_outputs <- prosperous_opportunity(
  comtrade_dyads = comtrade_dyads,
  subcat = subcat,
  econ_opp_index = econ_opp_index,
  energy_security_index = energy_security_index,
  tech_ghg = tech_ghg,
  policy = policy_tbl,
  country_info = country_info,
  component_weights = weights$partnership_opportunity_components
)

partner_opportunity_tbl <- opportunity_outputs$dyads
partner_opportunity_country_tbl <- opportunity_outputs$country
partner_opportunity_inputs_tbl <- opportunity_outputs$inputs_country
write_processed_tbl(partner_opportunity_tbl, "partner_opportunity_tbl", processed_dir)
write_processed_tbl(
  partner_opportunity_country_tbl,
  "partner_opportunity_country_tbl",
  processed_dir
)
write_processed_tbl(
  partner_opportunity_inputs_tbl,
  "partner_opportunity_inputs_tbl",
  processed_dir
)

development_outputs <- stronger_development(
  comtrade_dyads = comtrade_dyads,
  subcat = subcat,
  country_info = country_info,
  gdp_data = country_gdp,
  economic_opportunity_index = econ_opp_index,
  energy_security_index = energy_security_index,
  wb_wdi = wb_wdi,
  wb_doingbusiness = wb_doingbusiness,
  oecd_sector_raw = dplyr::bind_rows(oecd_1215, oecd_1518, oecd_1823),
  oecd_api_raw = oecd_api_raw
)

partner_development_tbl <- development_outputs$dyads
partner_development_country_tbl <- development_outputs$country
write_processed_tbl(partner_development_tbl, "partner_development_tbl", processed_dir)
write_processed_tbl(
  partner_development_country_tbl,
  "partner_development_country_tbl",
  processed_dir
)
