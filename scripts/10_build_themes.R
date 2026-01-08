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

raw_data_dir <- config$raw_data_dir
if (is.null(raw_data_dir)) {
  stop("Config missing raw_data_dir.")
}

ei_path <- file.path(raw_data_dir, "ei_stat_review_world_energy.csv")
if (!file.exists(ei_path)) {
  stop("Energy Institute data not found: ", ei_path)
}

ei <- read.csv(ei_path)

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
