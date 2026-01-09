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

snapshot_dirs <- list.dirs(raw_base_dir, recursive = FALSE, full.names = TRUE)
if (length(snapshot_dirs) == 0) {
  stop("No raw data snapshots found in: ", raw_base_dir)
}

snapshot_info <- file.info(snapshot_dirs)
latest_snapshot <- snapshot_dirs[order(snapshot_info$mtime, decreasing = TRUE)][1]

raw_path <- file.path(latest_snapshot, "ei_stat_review_world_energy.csv")
if (!file.exists(raw_path)) {
  expected_list <- paste0("- ", raw_path)
  stop("Missing required raw data. Expected raw files:\n", expected_list)
}

ei <- read.csv(raw_path)

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
