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

# Regenerate config/raw_inputs_manifest.yml from the ACTIVE pipeline.
#
# The previous version scraped legacy/ and "Legacy Scripts/", which guaranteed the
# manifest drifted away from what scripts/ actually reads. Discovery now walks the
# live scripts and R helpers, and curated metadata (source_type, url, cadence, owner,
# vintage) is carried across regeneration by entry id.

## Discovery ------------------------------------------------------------

# Shared with tests/testthat/test-raw-inputs-manifest.R so the drift check and the
# generator can never disagree about what the pipeline reads.
discovered <- discover_raw_input_references(repo_root)

if (length(discovered) == 0) {
  stop("Discovered no raw inputs; refusing to write an empty manifest.")
}

## Merge with curated metadata ------------------------------------------

manifest_path <- raw_inputs_manifest_path(repo_root)
existing <- if (file.exists(manifest_path)) read_raw_inputs_manifest(manifest_path) else list()

entries <- lapply(names(discovered), function(key) {
  fields <- discovered[[key]]
  curated <- match_raw_input_entry(existing, fields)

  if (is.null(curated)) {
    curated <- normalize_raw_input_entry(list(
      path = if (is.na(fields$path)) NULL else fields$path,
      pattern = if (is.na(fields$pattern)) NULL else fields$pattern,
      source_type = "unknown"
    ))
  }

  # Discovery owns wiring; humans own provenance.
  curated$path <- if (is.na(fields$path)) curated$path else fields$path
  curated$pattern <- if (is.na(fields$pattern)) curated$pattern else fields$pattern
  if (!is.null(fields$subdir) && !is.na(fields$subdir)) curated$subdir <- fields$subdir
  curated$required_by <- sort(unique(fields$required_by))
  curated
})

# Two references can resolve to one curated entry (a literal name plus the pattern that
# supersedes it). Collapse them so ids stay unique and required_by stays complete.
entries <- Reduce(
  function(acc, e) {
    prior <- acc[[e$id]]
    if (!is.null(prior)) {
      e$required_by <- sort(unique(c(prior$required_by, e$required_by)))
      if (is.na(e$pattern)) e$pattern <- prior$pattern
      if (is.na(e$path)) e$path <- prior$path
    }
    acc[[e$id]] <- e
    acc
  },
  entries,
  init = list()
)
entries <- unname(entries)

# Keep curated entries that discovery cannot see (e.g. read by notebooks or ad-hoc
# analyses) only when they opt in via retain: true.
retained <- Filter(function(e) isTRUE(e$retain), existing)
discovered_ids <- vapply(entries, function(e) e$id, character(1))
retained <- Filter(function(e) !(e$id %in% discovered_ids), retained)

dropped <- Filter(function(e) {
  !(e$id %in% discovered_ids) && !isTRUE(e$retain)
}, existing)

# Names would make yaml::write_yaml emit a mapping instead of a sequence.
entries <- unname(c(entries, retained))

## Write ----------------------------------------------------------------

drop_empty <- function(x) {
  x[!vapply(x, function(v) is.null(v) || length(v) == 0 || (length(v) == 1 && is.na(v)), logical(1))]
}

serialize_entry <- function(e) {
  drop_empty(list(
    id = e$id,
    path = e$path,
    pattern = e$pattern,
    subdir = e$subdir,
    resolve = if (!is.na(e$pattern)) e$resolve else NULL,
    fetch_policy = if (identical(e$fetch_policy, "fallback")) NULL else e$fetch_policy,
    source_type = e$source_type,
    source_name = e$source_name,
    url = e$url,
    cadence = e$cadence,
    owner = e$owner,
    vintage = e$vintage,
    licence = e$licence,
    staged_from = e$staged_from,
    optional = if (isTRUE(e$optional)) TRUE else NULL,
    retain = if (isTRUE(e$retain)) TRUE else NULL,
    notes = e$notes,
    required_by = if (length(e$required_by) > 0) as.list(e$required_by) else NULL
  ))
}

sort_key <- vapply(entries, function(e) paste(e$source_type, e$id, sep = "|"), character(1))
entries <- entries[order(sort_key)]

payload <- list(
  version = 2,
  generated_by = "scripts/01_generate_raw_inputs_manifest.R",
  inputs = lapply(entries, serialize_entry)
)

if (!dir.exists(dirname(manifest_path))) {
  dir.create(dirname(manifest_path), recursive = TRUE)
}
yaml::write_yaml(payload, manifest_path)

message("Wrote raw inputs manifest: ", manifest_path)
message("  entries discovered in active pipeline: ", length(discovered_ids))
message("  curated entries retained (retain: true): ", length(retained))

if (length(dropped) > 0) {
  message(
    "  dropped ", length(dropped), " entr(ies) no longer referenced by scripts/ or R/:\n",
    paste0("    - ", vapply(dropped, function(e) e$id, character(1)), collapse = "\n"),
    "\n  Set 'retain: true' on any of these that should survive regeneration."
  )
}

unknown <- Filter(function(e) identical(e$source_type, "unknown"), entries)
if (length(unknown) > 0) {
  message(
    "  ", length(unknown), " entr(ies) still have source_type: unknown and need curation:\n",
    paste0("    - ", vapply(unknown, function(e) e$id, character(1)), collapse = "\n")
  )
}
