# Compatibility shim.
# Canonical implementation now lives in scripts/96_pull_trade_timeseries.R.

source(local({
  candidate_roots <- c(getOption("opportunity_security.repo_root"), getwd())
  candidate_roots <- unique(candidate_roots[nzchar(candidate_roots)])

  for (root in candidate_roots) {
    d <- normalizePath(root, winslash = "/", mustWork = FALSE)
    while (dirname(d) != d) {
      target <- file.path(d, "scripts", "96_pull_trade_timeseries.R")
      if (file.exists(target)) {
        return(target)
      }
      d <- dirname(d)
    }
  }

  stop("Unable to locate scripts/96_pull_trade_timeseries.R")
}))
