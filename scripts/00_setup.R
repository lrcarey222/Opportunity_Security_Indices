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

