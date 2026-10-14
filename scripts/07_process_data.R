source(local({
  resolve_bootstrap_path <- function() {
    candidate_starts <- character()

    sf <- tryCatch(sys.frame(1)$ofile, error = function(e) "")
    if (!is.null(sf) && nzchar(sf)) candidate_starts <- c(candidate_starts, dirname(sf))

    frame_ofiles <- vapply(sys.frames(), function(fr) {
      val <- tryCatch(fr$ofile, error = function(e) "")
      if (is.null(val) || !nzchar(val)) "" else dirname(val)
    }, character(1))
    candidate_starts <- c(candidate_starts, frame_ofiles[nzchar(frame_ofiles)])

    fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(fa) > 0) candidate_starts <- c(candidate_starts, dirname(sub("^--file=", "", fa[1])))

    candidate_starts <- unique(c(candidate_starts, getwd()))

    for (start in candidate_starts) {
      d <- normalizePath(start, winslash = "/", mustWork = FALSE)
      while (dirname(d) != d) {
        bootstrap <- file.path(d, "scripts", "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)

        bootstrap <- file.path(d, "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)

        d <- dirname(d)
      }
    }

    stop("Unable to resolve script path for bootstrap.")
  }

  resolve_bootstrap_path()
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
