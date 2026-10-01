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

source(file.path(repo_root, "R", "utils", "hs6_crosswalk.R"))
source(file.path(repo_root, "scripts", "utils", "raw_inputs.R"))

# Regenerate the HS6 crosswalk views in data/raw from the single master in
# data/reference. Called by scripts/05_ingest_sources.R; also runnable directly:
#
#   Rscript scripts/04_build_hs6_views.R

build_hs6_views <- function(repo_root, raw_data_path, quiet = FALSE) {
  master_path <- file.path(raw_inputs_reference_dir(repo_root), "energy_hs6_master.csv")
  if (!file.exists(master_path)) {
    stop("HS6 master not found: ", master_path)
  }

  master <- utils::read.csv(master_path, check.names = FALSE, stringsAsFactors = FALSE)
  views <- hs6_build_views(master)

  if (!dir.exists(raw_data_path)) dir.create(raw_data_path, recursive = TRUE)

  written <- character()
  for (name in names(views)) {
    dest <- file.path(raw_data_path, name)
    # Write via a temp file so a partial write can never leave a truncated crosswalk.
    tmp <- paste0(dest, ".tmp-view")
    on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)

    utils::write.csv(views[[name]], tmp, row.names = FALSE, na = "")
    if (!file.rename(tmp, dest) && !file.copy(tmp, dest, overwrite = TRUE)) {
      stop("Could not write HS6 view: ", dest)
    }
    written <- c(written, name)

    if (!quiet) {
      message(sprintf("  %-44s %4d rows", name, nrow(views[[name]])))
    }
  }

  # scripts/15_build_partner_themes.R reads the master itself from data/raw. Nothing
  # refreshed that copy, so the partner themes could score a basket the trade themes had
  # moved on from; it is now copied alongside the views.
  master_copy <- file.path(raw_data_path, "energy_hs6_master.csv")
  if (normalizePath(master_copy, winslash = "/", mustWork = FALSE) !=
    normalizePath(master_path, winslash = "/", mustWork = FALSE)) {
    if (!file.copy(master_path, master_copy, overwrite = TRUE)) {
      stop("Could not copy the HS6 master to ", master_copy)
    }
    written <- c(written, basename(master_copy))
  }

  if (!quiet) {
    master_long <- hs6_normalize_master(master)
    codes <- length(unique(master_long$hs6))
    message("HS6 views rebuilt from ", basename(master_path), " (", codes, " distinct HS6 codes)")

    # The Atlas panel is HS92, so codes created by later revisions never match it. Report
    # the count against the Atlas product list whenever it is staged.
    hs92_path <- file.path(raw_data_path, "hs92_product_year_6.csv")
    if (file.exists(hs92_path)) {
      hs92 <- utils::read.csv(hs92_path, colClasses = c(product_hs92_code = "character"))
      report <- hs6_match_report(master_long, unique(hs92$product_hs92_code))
      message(sprintf(
        "  %d of %d codes have no HS92 counterpart in the Atlas panel (%d legacy codes bridge older revisions)",
        report$n_unmatched, report$n_codes, report$n_legacy
      ))
    }
  }

  invisible(written)
}

# Build only when invoked directly; 05_ingest_sources.R sources this for the function
# and calls it at the right point in its own sequence.
opsi_hs6_views_run_directly <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  length(file_arg) > 0 &&
    identical(basename(sub("^--file=", "", file_arg[1])), "04_build_hs6_views.R")
}

if (opsi_hs6_views_run_directly()) {
  invisible(build_hs6_views(
    repo_root = repo_root,
    raw_data_path = file.path(repo_root, config$raw_data_dir)
  ))
}
