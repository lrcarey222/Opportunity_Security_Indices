# Build theme-level tables.
if (!exists("repo_root")) {
  repo_root <- resolve_repo_root()
}

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "themes", "energy_security", "energy_access_consumption.R"))
source(file.path(repo_root, "R", "themes", "energy_security", "foreign_dependency.R"))
source(file.path(repo_root, "R", "themes", "energy_security", "reserves.R"))

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

# Fail fast (or skip) if required raw inputs are missing.
missing_files <- c(
  raw_path,
  reserves_excel_path,
  critical_minerals_path,
  cleantech_midstream_path,
  ev_midstream_path
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

# Collect all theme outputs in a named list for downstream consumers.
theme_outputs <- list(
  energy_access_consumption = energy_access_tbl,
  foreign_dependency = foreign_dependency_tbl,
  reserves = reserves_tbl
)

invisible(theme_outputs)
# build_themes (placeholder).
# TODO: implement.
build_themes_stub <- function() {
  NULL
}
