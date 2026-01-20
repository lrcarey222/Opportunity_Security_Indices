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
if (is.null(config)) {
  stop("Config not loaded; run scripts/00_setup.R first.")
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

missing_files <- c(
  comtrade_dyads_path,
  subcat_path,
  fdi_path,
  tech_ghg_path,
  cat_policy_path,
  country_info_path,
  country_gdp_path
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

if (!exists("economic_opportunity_outputs")) {
  stop("economic_opportunity_outputs not found; run scripts/20_build_indices.R first.")
}
if (!exists("energy_security_outputs")) {
  stop("energy_security_outputs not found; run scripts/20_build_indices.R first.")
}

comtrade_dyads <- read.csv(comtrade_dyads_path)
subcat <- read.csv(subcat_path)
fdi_raw <- read.csv(fdi_path)
tech_ghg_raw <- read.csv(tech_ghg_path)
cat_raw <- read.csv(cat_policy_path)
country_info <- read.csv(country_info_path)
country_gdp <- read.csv(country_gdp_path)

country_info <- standardize_country_info(country_info)

econ_opp_index <- aggregate_economic_opportunity_index(economic_opportunity_outputs)
energy_security_index <- aggregate_energy_security_index(energy_security_outputs)

tech_ghg <- partnership_strength_clean_ghg(tech_ghg_raw)
policy_tbl <- partnership_strength_clean_policy(cat_raw)

partner_friendshore_tbl <- safer_friendshore(
  comtrade_dyads = comtrade_dyads,
  subcat = subcat,
  econ_opp_index = econ_opp_index,
  energy_security_index = energy_security_index,
  tech_ghg = tech_ghg,
  policy = policy_tbl,
  fdi_raw = fdi_raw,
  country_info = country_info,
  gdp_data = country_gdp
)

partner_opportunity_tbl <- prosperous_opportunity(
  comtrade_dyads = comtrade_dyads,
  subcat = subcat,
  econ_opp_index = econ_opp_index,
  energy_security_index = energy_security_index,
  tech_ghg = tech_ghg,
  policy = policy_tbl,
  country_info = country_info
)

partner_development_tbl <- stronger_development(
  comtrade_dyads = comtrade_dyads,
  subcat = subcat,
  fdi_raw = fdi_raw,
  country_info = country_info,
  gdp_data = country_gdp
)
