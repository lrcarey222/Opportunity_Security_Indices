source(local({
  # Prefer sys.frame(1)$ofile when sourced (e.g., from run_pipeline.R).
  sf <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  this_file <- if (!is.null(sf) && nzchar(sf)) sf else {
    # Fallback for direct Rscript execution of this script.
    fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(fa) > 0) sub("^--file=", "", fa[1]) else ""
  }
  if (!nzchar(this_file)) stop("Unable to resolve script path for bootstrap.")
  file.path(dirname(normalizePath(this_file, winslash = "/", mustWork = FALSE)), "utils", "bootstrap.R")
}))

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

processed_dir <- file.path(repo_root, config$processed_dir)
if (!dir.exists(processed_dir)) {
  dir.create(processed_dir, recursive = TRUE)
}

source(file.path(repo_root, "R", "process", "process_all.R"))
plan <- process_all(raw_base_dir, processed_dir, list(raw_inputs_manifest = raw_manifest))

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
