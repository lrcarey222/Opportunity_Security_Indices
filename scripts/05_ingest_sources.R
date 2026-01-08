# Ingest raw sources from SharePoint into a snapshot folder.
if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("Package 'yaml' is required to load the manifest.")
}

repo_root <- getOption("opportunity_security.repo_root")
if (is.null(repo_root) || !nzchar(repo_root)) {
  if (!requireNamespace("rprojroot", quietly = TRUE)) {
    stop("Repo root not set. Run from the repo or set OPSI_CONFIG and OPSI_WEIGHTS.")
  }
  repo_root <- rprojroot::find_root(rprojroot::is_git_root, path = getwd())
}
if (is.null(repo_root) || !nzchar(repo_root)) {
  stop("Repo root not set. Run from the repo or set OPSI_CONFIG and OPSI_WEIGHTS.")
}

config <- getOption("opportunity_security.config")
if (is.null(config)) {
  config_path <- Sys.getenv("OPSI_CONFIG", file.path(repo_root, "config", "config.yml"))
  if (!file.exists(config_path)) {
    stop("Config file not found: ", config_path)
  }
  config <- yaml::read_yaml(config_path)
}

if (is.null(config$sharepoint_raw_dir)) {
  stop("Config is missing sharepoint_raw_dir.")
}
if (is.null(config$raw_data_dir)) {
  stop("Config is missing raw_data_dir.")
}

sharepoint_raw_dir <- config$sharepoint_raw_dir
raw_data_dir <- file.path(repo_root, config$raw_data_dir)

manifest_path <- file.path(repo_root, "config", "raw_inputs_manifest.yml")
if (!file.exists(manifest_path)) {
  stop("Raw inputs manifest not found: ", manifest_path)
}

manifest <- yaml::read_yaml(manifest_path)
if (length(manifest) == 0) {
  stop("Raw inputs manifest is empty: ", manifest_path)
}

snapshot_date <- format(Sys.Date(), "%Y-%m-%d")
snapshot_dir <- file.path(raw_data_dir, snapshot_date)
if (!dir.exists(snapshot_dir)) {
  dir.create(snapshot_dir, recursive = TRUE)
}

missing <- character()

for (entry in manifest) {
  if (is.null(entry$path) || !nzchar(entry$path)) {
    next
  }
  is_optional <- isTRUE(entry$optional)
  source_path <- file.path(sharepoint_raw_dir, entry$path)
  dest_path <- file.path(snapshot_dir, entry$path)

  if (!file.exists(source_path)) {
    if (!is_optional) {
      missing <- c(missing, entry$path)
    }
    next
  }

  dest_dir <- dirname(dest_path)
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  copied <- file.copy(source_path, dest_path, overwrite = TRUE)
  if (!copied) {
    stop("Failed to copy raw input: ", source_path, " -> ", dest_path)
  }
}

if (length(missing) > 0) {
  missing_list <- paste(paste0("- ", missing), collapse = "\n")
  stop("Missing required raw inputs in sharepoint_raw_dir:\n", missing_list)
}

message("Raw inputs snapshot created at: ", snapshot_dir)
