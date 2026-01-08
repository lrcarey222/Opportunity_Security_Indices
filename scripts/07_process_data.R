# Process raw inputs into standardized datasets.
if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("Package 'yaml' is required to load the manifest.")
}

resolve_repo_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  script_path <- if (length(file_arg) > 0) {
    sub("^--file=", "", file_arg[1])
  } else if (!is.null(sys.frame(1)$ofile)) {
    sys.frame(1)$ofile
  } else {
    ""
  }
  dirname(normalizePath(script_path, winslash = "/", mustWork = FALSE))
}

repo_root <- resolve_repo_root()
config <- getOption("opportunity_security.config")
if (is.null(config)) {
  stop("Config not loaded. Run scripts/00_setup.R first.")
}
if (is.null(config$raw_data_dir)) {
  stop("Config is missing raw_data_dir.")
}
if (is.null(config$processed_dir)) {
  stop("Config is missing processed_dir.")
}

manifest_path <- file.path(repo_root, "config", "raw_inputs_manifest.yml")
if (!file.exists(manifest_path)) {
  stop("Raw inputs manifest not found: ", manifest_path)
}

raw_manifest <- yaml::read_yaml(manifest_path)
if (length(raw_manifest) == 0) {
  stop("Raw inputs manifest is empty: ", manifest_path)
}

raw_base_dir <- file.path(repo_root, config$raw_data_dir)
if (!dir.exists(raw_base_dir)) {
  stop("Raw data directory not found: ", raw_base_dir)
}

snapshot_dirs <- list.dirs(raw_base_dir, recursive = FALSE, full.names = TRUE)
if (length(snapshot_dirs) == 0) {
  stop("No raw data snapshots found in: ", raw_base_dir)
}

snapshot_info <- file.info(snapshot_dirs)
latest_snapshot <- snapshot_dirs[order(snapshot_info$mtime, decreasing = TRUE)][1]

processed_dir <- file.path(repo_root, config$processed_dir)
if (!dir.exists(processed_dir)) {
  dir.create(processed_dir, recursive = TRUE)
}

source(file.path(repo_root, "R", "process", "process_all.R"))
plan <- process_all(latest_snapshot, processed_dir, list(raw_inputs_manifest = raw_manifest))

missing_processed <- character()
for (i in seq_along(plan$raw_paths)) {
  raw_path <- plan$raw_paths[i]
  processed_path <- plan$processed_paths[i]
  is_optional <- plan$optional[i]

  if (!file.exists(raw_path)) {
    if (!is_optional) {
      missing_processed <- c(missing_processed, processed_path)
    }
    next
  }

  processed_dirname <- dirname(processed_path)
  if (!dir.exists(processed_dirname)) {
    dir.create(processed_dirname, recursive = TRUE)
  }

  copied <- file.copy(raw_path, processed_path, overwrite = TRUE)
  if (!copied) {
    stop("Failed to write processed data: ", processed_path)
  }
}

if (length(missing_processed) > 0) {
  missing_list <- paste(paste0("- ", missing_processed), collapse = "\n")
  stop("Missing required raw inputs for processing. Expected processed files:\n", missing_list)
}

message("Processed data written to: ", processed_dir)
