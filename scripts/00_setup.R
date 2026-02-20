source(local({
  # Prefer sys.frame(1)$ofile when sourced (e.g., from run_pipeline.R).
  sf <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  this_file <- if (!is.null(sf) && nzchar(sf)) sf else {
    ofiles <- vapply(sys.frames(), function(fr) {
      of <- tryCatch(fr$ofile, error = function(e) NULL)
      if (is.null(of) || !nzchar(of)) "" else as.character(of)
    }, character(1))
    ofiles <- ofiles[nzchar(ofiles)]
    if (length(ofiles) > 0) ofiles[[length(ofiles)]] else {
      # Fallback for direct Rscript execution of this script.
      fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
      if (length(fa) > 0) sub("^--file=", "", fa[1]) else ""
    }
  }
  if (!nzchar(this_file)) {
    candidate <- file.path(normalizePath(getwd(), winslash = "/", mustWork = FALSE), "scripts", "utils", "bootstrap.R")
    if (file.exists(candidate)) return(candidate)
    stop("Unable to resolve script path for bootstrap.")
  }
  file.path(dirname(normalizePath(this_file, winslash = "/", mustWork = FALSE)), "utils", "bootstrap.R")
}))

