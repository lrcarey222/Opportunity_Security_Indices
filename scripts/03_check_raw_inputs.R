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

source(file.path(repo_root, "scripts", "utils", "raw_inputs.R"))
source(file.path(repo_root, "scripts", "utils", "fetchers.R"))
source_fetcher_files(repo_root)

# Read-only preflight: report which raw inputs would block a build, and where each one
# is expected to come from. Answers "why won't ingestion run?" without running it.
#
#   Rscript scripts/03_check_raw_inputs.R

sharepoint_raw_dir <- config$sharepoint_raw_dir
raw_data_path <- file.path(repo_root, config$raw_data_dir)
reference_dir <- raw_inputs_reference_dir(repo_root)

manifest <- read_raw_inputs_manifest(raw_inputs_manifest_path(repo_root))
staged <- raw_inputs_staged_entries(manifest)

source_dir_for <- function(entry) {
  if (identical(entry$staged_from, "repo")) reference_dir else sharepoint_raw_dir
}

has_source_copy <- function(entry) {
  dir <- source_dir_for(entry)
  if (is.null(dir) || !nzchar(dir)) return(FALSE)
  if (!is.na(entry$pattern)) {
    return(dir.exists(dir) && length(list.files(dir, pattern = entry$pattern)) > 0)
  }
  file.exists(file.path(dir, entry$path))
}

blocking <- character()
tolerated <- character()
fetch_covered <- character()

for (entry in staged) {
  local_ok <- raw_input_present_locally(entry, raw_data_path)
  source_ok <- has_source_copy(entry)
  fetchable <- !is.null(get_fetcher(entry$id)) && !identical(entry$fetch_policy, "never")

  if (local_ok || source_ok) next

  label <- sprintf("%s  [%s]", entry$path, entry$staged_from)
  if (fetchable) {
    fetch_covered <- c(fetch_covered, label)
  } else if (isTRUE(entry$optional)) {
    tolerated <- c(tolerated, label)
  } else {
    blocking <- c(blocking, label)
  }
}

message("Raw input preflight")
message("  manifest entries : ", length(manifest))
message("  staged entries   : ", length(staged))
message("  sharepoint_raw_dir: ", sharepoint_raw_dir)
message("  reference dir     : ", reference_dir)
message("  raw data dir      : ", raw_data_path)

if (length(blocking) > 0) {
  message("\nBLOCKING - ingestion will stop on these:")
  message(paste0("  - ", blocking, collapse = "\n"))
} else {
  message("\nBLOCKING: none. Ingestion's requirement check will pass.")
}

if (length(fetch_covered) > 0) {
  message("\nAbsent locally but a fetcher will supply them:")
  message(paste0("  - ", fetch_covered, collapse = "\n"))
}

if (length(tolerated) > 0) {
  message("\nAbsent but optional (the pipeline degrades gracefully):")
  message(paste0("  - ", tolerated, collapse = "\n"))
}

repo_entries <- Filter(function(e) identical(e$staged_from, "repo"), staged)
if (length(repo_entries) > 0) {
  bad <- Filter(function(e) !file.exists(file.path(reference_dir, e$path)), repo_entries)
  message(
    "\nVersion-controlled crosswalks: ",
    length(repo_entries) - length(bad), "/", length(repo_entries), " present in data/reference"
  )
  if (length(bad) > 0) {
    message(paste0("  MISSING: ", vapply(bad, function(e) e$path, character(1)), collapse = "\n"))
  }
}

invisible(list(blocking = blocking, tolerated = tolerated, fetch_covered = fetch_covered))
