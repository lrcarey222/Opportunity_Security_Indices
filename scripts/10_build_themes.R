# Build theme-level tables.
if (!exists("repo_root")) {
  repo_root <- resolve_repo_root()
}

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "themes", "energy_security", "energy_access_consumption.R"))

config <- getOption("opportunity_security.config")
if (is.null(config)) {
  stop("Config not loaded; run scripts/00_setup.R first.")
}

processed_dir <- config$processed_dir
if (is.null(processed_dir)) {
  stop("Config missing processed_dir.")
}

processed_path <- file.path(repo_root, processed_dir, "ei_stat_review_world_energy.csv")
if (!file.exists(processed_path)) {
  expected_list <- paste0("- ", processed_path)
  stop("Missing required processed data. Expected processed files:\n", expected_list)
}

ei <- read.csv(processed_path)

energy_access_tbl <- energy_access_consumption(ei)

theme_outputs <- list(
  energy_access_consumption = energy_access_tbl
)

invisible(theme_outputs)
# build_themes (placeholder).
# TODO: implement.
build_themes_stub <- function() {
  NULL
}
