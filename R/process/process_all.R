# Build processing plan for raw inputs.
process_all <- function(raw_dir, processed_dir, config) {
  if (is.null(config$raw_inputs_manifest) || length(config$raw_inputs_manifest) == 0) {
    stop("raw_inputs_manifest is required in config.")
  }
  if (is.null(raw_dir) || !nzchar(raw_dir)) {
    stop("raw_dir is required.")
  }
  if (is.null(processed_dir) || !nzchar(processed_dir)) {
    stop("processed_dir is required.")
  }

  manifest <- config$raw_inputs_manifest
  raw_paths <- vapply(manifest, function(entry) {
    file.path(raw_dir, entry$path)
  }, character(1))
  processed_paths <- vapply(manifest, function(entry) {
    file.path(processed_dir, entry$path)
  }, character(1))
  optional_flags <- vapply(manifest, function(entry) isTRUE(entry$optional), logical(1))

  list(
    raw_paths = raw_paths,
    processed_paths = processed_paths,
    optional = optional_flags,
    manifest = manifest
  )
}
