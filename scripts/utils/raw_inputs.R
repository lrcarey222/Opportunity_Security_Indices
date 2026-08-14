# Raw-input manifest helpers.
#
# All file IO for raw inputs lives here (scripts/), never in R/. Three concerns:
#   1. Reading + normalizing config/raw_inputs_manifest.yml (schema v2, v1 tolerated).
#   2. Syncing staged files from sharepoint_raw_dir into data/raw (mtime/size aware).
#   3. Resolving vintage-bearing filenames (e.g. "GTA NIPO - July 2026.xlsx") to the
#      newest available file instead of a hard-coded release.

OPSI_RAW_SOURCE_TYPES <- c("api", "manual", "derived", "generated", "unknown")
OPSI_RAW_STAGED_FROM <- c("sharepoint", "pipeline", "repo")

opsi_force_refresh <- function() {
  tolower(Sys.getenv("OPSI_FORCE_REFRESH", "false")) %in% c("1", "true", "yes")
}

raw_inputs_manifest_path <- function(repo_root) {
  file.path(repo_root, "config", "raw_inputs_manifest.yml")
}

# Stable id from a path or pattern, used to carry curated metadata across regeneration.
raw_input_default_id <- function(x) {
  base <- basename(x)
  base <- sub("\\.[A-Za-z0-9]+$", "", base)
  base <- tolower(base)
  base <- gsub("[^a-z0-9]+", "_", base)
  gsub("^_+|_+$", "", base)
}

normalize_raw_input_entry <- function(entry) {
  if (is.character(entry) && length(entry) == 1) {
    entry <- list(path = entry)
  }
  if (!is.list(entry)) {
    stop("Raw input manifest entries must be mappings or strings.")
  }

  path <- if (is.null(entry$path)) NA_character_ else as.character(entry$path)[1]
  pattern <- if (is.null(entry$pattern)) NA_character_ else as.character(entry$pattern)[1]

  if (is.na(path) && is.na(pattern)) {
    stop("Raw input manifest entry needs either 'path' or 'pattern'.")
  }

  # Manifest paths are repo-relative under data/raw; strip stray leading slashes and
  # collapse the embedded newlines that v1 multi-line YAML folding produced.
  if (!is.na(path)) {
    path <- gsub("[\r\n]+", " ", path)
    path <- gsub("\\s{2,}", " ", trimws(path))
    path <- sub("^/+", "", path)
  }

  id <- if (is.null(entry$id)) {
    raw_input_default_id(if (!is.na(path)) path else pattern)
  } else {
    as.character(entry$id)[1]
  }

  source_type <- if (is.null(entry$source_type)) "unknown" else as.character(entry$source_type)[1]
  if (!source_type %in% OPSI_RAW_SOURCE_TYPES) {
    stop(
      "Unknown source_type '", source_type, "' for raw input '", id, "'. Expected one of: ",
      paste(OPSI_RAW_SOURCE_TYPES, collapse = ", ")
    )
  }

  staged_from <- if (is.null(entry$staged_from)) {
    if (source_type %in% c("api", "generated")) "pipeline" else "sharepoint"
  } else {
    as.character(entry$staged_from)[1]
  }
  if (!staged_from %in% OPSI_RAW_STAGED_FROM) {
    stop(
      "Unknown staged_from '", staged_from, "' for raw input '", id, "'. Expected one of: ",
      paste(OPSI_RAW_STAGED_FROM, collapse = ", ")
    )
  }

  as_chr <- function(x, default = NA_character_) {
    if (is.null(x) || length(x) == 0) default else as.character(x)[1]
  }

  # How a registered fetcher relates to the staged copy:
  #   prefer   - fetch on the source's cadence; the API is the authority
  #   fallback - only fetch when no local file exists (curated staging wins)
  #   never    - do not fetch even if a fetcher is registered
  fetch_policy <- if (is.null(entry$fetch_policy)) "fallback" else as.character(entry$fetch_policy)[1]
  if (!fetch_policy %in% c("prefer", "fallback", "never")) {
    stop(
      "Unknown fetch_policy '", fetch_policy, "' for raw input '", id,
      "'. Expected one of: prefer, fallback, never"
    )
  }

  list(
    id = id,
    path = path,
    pattern = pattern,
    subdir = as_chr(entry$subdir),
    fetch_policy = fetch_policy,
    resolve = if (is.null(entry$resolve)) (if (!is.na(pattern)) "newest" else "exact") else as.character(entry$resolve)[1],
    source_type = source_type,
    source_name = as_chr(entry$source_name),
    url = as_chr(entry$url),
    cadence = as_chr(entry$cadence, "unknown"),
    owner = as_chr(entry$owner, "unassigned"),
    vintage = as_chr(entry$vintage),
    licence = as_chr(entry$licence),
    notes = as_chr(entry$notes),
    optional = isTRUE(entry$optional),
    retain = isTRUE(entry$retain),
    staged_from = staged_from,
    required_by = if (is.null(entry$required_by)) character() else as.character(unlist(entry$required_by))
  )
}

read_raw_inputs_manifest <- function(path) {
  if (!file.exists(path)) {
    stop("Raw inputs manifest not found: ", path)
  }
  doc <- yaml::read_yaml(path)
  if (length(doc) == 0) {
    stop("Raw inputs manifest is empty: ", path)
  }

  # Schema v2 wraps entries under `inputs:`; v1 was a bare sequence of {path, optional}.
  entries <- if (!is.null(doc$inputs)) doc$inputs else doc
  normalized <- lapply(entries, normalize_raw_input_entry)

  ids <- vapply(normalized, function(e) e$id, character(1))
  duplicated_ids <- unique(ids[duplicated(ids)])
  if (length(duplicated_ids) > 0) {
    stop("Duplicate raw input ids in manifest: ", paste(duplicated_ids, collapse = ", "))
  }

  names(normalized) <- ids
  normalized
}

# Entries the ingest step must place into data/raw before a build: staged manually into
# sharepoint_raw_dir, or version controlled under data/reference.
raw_inputs_staged_entries <- function(manifest) {
  Filter(
    function(e) e$staged_from %in% c("sharepoint", "repo") && !is.na(e$path),
    manifest
  )
}

# Project-authored inputs live here so the pipeline is reproducible without OneDrive.
raw_inputs_reference_dir <- function(repo_root) {
  file.path(repo_root, "data", "reference")
}

## Discovery ------------------------------------------------------------

# Files that describe or test the manifest are not themselves raw inputs.
RAW_INPUT_SCAN_EXCLUDE <- "scripts/(utils/raw_inputs|01_generate_raw_inputs_manifest|02_render_sources_doc)\\.R$"

raw_input_scan_files <- function(repo_root) {
  scan_dirs <- file.path(repo_root, c("scripts", "R"))
  scan_dirs <- scan_dirs[dir.exists(scan_dirs)]
  if (length(scan_dirs) == 0) {
    stop("No active script directories found (expected scripts/ and/or R/).")
  }

  files <- sort(unlist(lapply(
    scan_dirs,
    list.files,
    pattern = "\\.R$", full.names = TRUE, recursive = TRUE
  )))
  files <- gsub("\\\\", "/", files)
  files[!grepl(RAW_INPUT_SCAN_EXCLUDE, files)]
}

# Discovery reads source text, so a literal written as "\\d" arrives with both
# backslashes. Convert R string escapes to the characters the running code sees,
# otherwise every extracted regex is subtly wrong.
raw_input_unescape <- function(x) {
  vapply(x, function(s) {
    if (!grepl("\\", s, fixed = TRUE)) return(s)

    chars <- strsplit(s, "", fixed = TRUE)[[1]]
    out <- character(length(chars))
    n_out <- 0L
    i <- 1L
    while (i <= length(chars)) {
      if (chars[i] == "\\" && i < length(chars)) {
        nxt <- chars[i + 1L]
        n_out <- n_out + 1L
        out[n_out] <- switch(nxt, n = "\n", t = "\t", r = "\r", nxt)
        i <- i + 2L
      } else {
        n_out <- n_out + 1L
        out[n_out] <- chars[i]
        i <- i + 1L
      }
    }
    paste(out[seq_len(n_out)], collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

raw_input_extract_quoted <- function(text) {
  hits <- regmatches(text, gregexpr("(['\"])(?:(?!\\1).)*\\1", text, perl = TRUE))[[1]]
  if (length(hits) == 0) return(character())
  raw_input_unescape(substr(hits, 2, nchar(hits) - 1))
}

raw_input_extract_calls <- function(text, pattern) {
  matches <- regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1]]
  if (length(matches) == 0) return(character())
  vapply(matches, function(m) sub(pattern, "\\1", m, perl = TRUE), character(1), USE.NAMES = FALSE)
}

raw_input_named_arg <- function(call_text, arg) {
  m <- stringr::str_match(call_text, paste0(arg, "\\s*=\\s*([\"'])((?:(?!\\1).)*)\\1"))
  if (is.na(m[1, 1])) NA_character_ else raw_input_unescape(m[1, 3])
}

raw_input_is_file_like <- function(path) {
  nzchar(path) & !grepl("/$", path) & grepl("\\.[A-Za-z0-9]+$", path)
}

# Every raw input the active pipeline references, as a list of
# {path, pattern, subdir, required_by} records keyed by path or "pattern:<regex>".
#
# Two call shapes are recognised:
#   file.path(raw_data_path, "x.csv") / paste0(raw_data_path, "x.csv")  -> exact
#   resolve_versioned_raw_input(raw_data_path, pattern = ..., fallback = ...) -> pattern
discover_raw_input_references <- function(repo_root, files = raw_input_scan_files(repo_root)) {
  pattern_literal_call <- "(?s)(?:file\\.path|paste0)\\s*\\(\\s*raw_data_path\\s*,([^()]*?)\\)"
  pattern_versioned_call <- "(?s)resolve_versioned_raw_input\\s*\\((.*?)\\)"

  repo_prefix <- paste0("^", gsub("([.|()\\^{}+$*?\\[\\]])", "\\\\\\1", gsub("\\\\", "/", repo_root)), "/?")

  discovered <- list()
  record <- function(key, fields, script) {
    existing <- discovered[[key]]
    if (is.null(existing)) {
      fields$required_by <- script
      discovered[[key]] <<- fields
    } else {
      existing$required_by <- unique(c(existing$required_by, script))
      discovered[[key]] <<- existing
    }
  }

  for (script in files) {
    text <- paste(readLines(script, warn = FALSE), collapse = "\n")
    # A few sources carry stray non-UTF-8 bytes; perl regex refuses to scan them.
    # File names are ASCII, so dropping the invalid bytes costs nothing here.
    text <- iconv(text, from = "UTF-8", to = "UTF-8", sub = "")
    script_rel <- sub(repo_prefix, "", gsub("\\\\", "/", normalizePath(script, winslash = "/", mustWork = FALSE)))

    for (call_body in raw_input_extract_calls(text, pattern_literal_call)) {
      parts <- trimws(raw_input_extract_quoted(call_body))
      parts <- parts[nzchar(parts)]
      if (length(parts) == 0) next

      path <- paste(parts, collapse = "/")
      if (!raw_input_is_file_like(path)) next

      record(path, list(path = path, pattern = NA_character_, subdir = NA_character_), script_rel)
    }

    for (call_body in raw_input_extract_calls(text, pattern_versioned_call)) {
      pattern <- raw_input_named_arg(call_body, "pattern")
      if (is.na(pattern)) next

      record(
        paste0("pattern:", pattern),
        list(
          path = raw_input_named_arg(call_body, "fallback"),
          pattern = pattern,
          subdir = raw_input_named_arg(call_body, "subdir")
        ),
        script_rel
      )
    }
  }

  discovered
}

# Manifest entry backing a discovered reference, or NULL when the manifest has drifted.
match_raw_input_entry <- function(manifest, reference) {
  if (!is.na(reference$pattern)) {
    hit <- Filter(function(e) identical(e$pattern, reference$pattern), manifest)
    if (length(hit) > 0) return(hit[[1]])
  }
  if (!is.na(reference$path)) {
    hit <- Filter(function(e) identical(e$path, reference$path), manifest)
    if (length(hit) > 0) return(hit[[1]])

    # An exact reference is also covered when a pattern entry matches its file name.
    hit <- Filter(function(e) {
      !is.na(e$pattern) && grepl(e$pattern, basename(reference$path))
    }, manifest)
    if (length(hit) > 0) return(hit[[1]])
  }
  NULL
}

## Sync -----------------------------------------------------------------

# Copy-once ingestion silently pins stale vintages, so compare size and mtime and
# recopy whenever the staged source has moved ahead of the local copy.
raw_input_needs_sync <- function(source_path, dest_path, force = opsi_force_refresh()) {
  if (!file.exists(source_path)) return(FALSE)
  if (!file.exists(dest_path)) return(TRUE)
  if (isTRUE(force)) return(TRUE)

  src <- file.info(source_path)
  dst <- file.info(dest_path)
  if (!identical(as.numeric(src$size), as.numeric(dst$size))) return(TRUE)

  # OneDrive/FAT round mtimes, so require a margin before calling the source newer.
  as.numeric(src$mtime) > (as.numeric(dst$mtime) + 2)
}

# Returns one of "copied", "current", "missing".
sync_raw_file <- function(source_path, dest_path, force = opsi_force_refresh()) {
  if (!file.exists(source_path)) return("missing")
  if (!raw_input_needs_sync(source_path, dest_path, force = force)) return("current")

  dest_dir <- dirname(dest_path)
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

  # copy.date keeps mtimes comparable so the next run does not recopy unchanged files.
  ok <- file.copy(source_path, dest_path, overwrite = TRUE, copy.date = TRUE)
  if (!ok) stop("Failed to copy raw input: ", source_path, " -> ", dest_path)
  "copied"
}

# Sync one manifest entry from the staging area into data/raw.
#
# Pattern entries copy the newest matching file in the staging area, which is how a
# freshly published vintage (a new "GTA NIPO - <month> <year>.xlsx", say) reaches the
# pipeline without a code edit.
sync_raw_input_entry <- function(entry,
                                 sharepoint_raw_dir,
                                 raw_data_path,
                                 force = opsi_force_refresh(),
                                 reference_dir = NULL) {
  # Repo-staged inputs are project-authored crosswalks copied out of version control
  # rather than a personal OneDrive, so a fresh clone can build without staging.
  if (identical(entry$staged_from, "repo")) {
    if (is.null(reference_dir) || !nzchar(reference_dir)) return("missing")
    return(sync_raw_file(
      file.path(reference_dir, entry$path),
      file.path(raw_data_path, entry$path),
      force = force
    ))
  }

  if (is.null(sharepoint_raw_dir) || !nzchar(sharepoint_raw_dir)) return("missing")

  if (!is.na(entry$pattern)) {
    rel_dir <- if (!is.na(entry$subdir)) entry$subdir else dirname(entry$path)
    rel_dir <- if (identical(rel_dir, ".") || is.na(rel_dir)) "" else rel_dir

    source_dir <- if (nzchar(rel_dir)) file.path(sharepoint_raw_dir, rel_dir) else sharepoint_raw_dir
    dest_dir <- if (nzchar(rel_dir)) file.path(raw_data_path, rel_dir) else raw_data_path

    if (!dir.exists(source_dir)) return("missing")
    candidates <- list.files(source_dir, pattern = entry$pattern, full.names = FALSE)
    if (length(candidates) == 0) return("missing")

    keys <- vapply(candidates, raw_input_vintage_key, numeric(1))
    mtimes <- as.numeric(file.info(file.path(source_dir, candidates))$mtime)
    newest <- candidates[order(keys, mtimes, decreasing = TRUE, na.last = TRUE)[1]]

    return(sync_raw_file(file.path(source_dir, newest), file.path(dest_dir, newest), force = force))
  }

  sync_raw_file(
    file.path(sharepoint_raw_dir, entry$path),
    file.path(raw_data_path, entry$path),
    force = force
  )
}

# TRUE when a manifest entry has at least one local file backing it.
raw_input_present_locally <- function(entry, raw_data_path) {
  if (!is.na(entry$pattern)) {
    rel_dir <- if (!is.na(entry$subdir)) entry$subdir else dirname(entry$path)
    search_dir <- if (identical(rel_dir, ".") || is.na(rel_dir)) raw_data_path else file.path(raw_data_path, rel_dir)
    if (!dir.exists(search_dir)) return(FALSE)
    return(length(list.files(search_dir, pattern = entry$pattern)) > 0)
  }
  file.exists(file.path(raw_data_path, entry$path))
}

## Vintage resolution ---------------------------------------------------

RAW_INPUT_MONTHS <- c(
  january = 1, february = 2, march = 3, april = 4, may = 5, june = 6,
  july = 7, august = 8, september = 9, october = 10, november = 11, december = 12
)

# Sortable YYYYMMDD key extracted from a filename, or NA when it carries no vintage.
raw_input_vintage_key <- function(filename) {
  name <- basename(filename)

  iso <- stringr::str_match(name, "(19|20)(\\d{2})[-_](\\d{2})[-_](\\d{2})")
  if (!is.na(iso[1, 1])) {
    return(as.numeric(paste0(iso[1, 2], iso[1, 3], iso[1, 4], iso[1, 5])))
  }

  month_year <- stringr::str_match(
    tolower(name),
    "(january|february|march|april|may|june|july|august|september|october|november|december)[ _-]+((?:19|20)\\d{2})"
  )
  if (!is.na(month_year[1, 1])) {
    return(as.numeric(sprintf("%s%02d01", month_year[1, 3], RAW_INPUT_MONTHS[[month_year[1, 2]]])))
  }

  years <- stringr::str_extract_all(name, "(19|20)\\d{2}")[[1]]
  if (length(years) > 0) {
    return(as.numeric(paste0(max(as.integer(years)), "0101")))
  }

  # Trailing two-digit vintages such as iea_criticalminerals_25.csv.
  short_year <- stringr::str_match(name, "[_-](\\d{2})\\.[A-Za-z0-9]+$")
  if (!is.na(short_year[1, 1])) {
    return(as.numeric(paste0(2000L + as.integer(short_year[1, 2]), "0101")))
  }

  NA_real_
}

# Resolve a versioned raw input to the newest available match.
#
# pattern is a regex matched against file names inside raw_data_path. Falls back to
# `fallback` (a literal file name) when nothing matches, so pinned legacy names keep
# working while a repo migrates.
resolve_versioned_raw_input <- function(raw_data_path,
                                        pattern,
                                        fallback = NULL,
                                        label = pattern,
                                        subdir = NULL,
                                        quiet = FALSE) {
  search_dir <- if (is.null(subdir)) raw_data_path else file.path(raw_data_path, subdir)

  candidates <- if (dir.exists(search_dir)) {
    list.files(search_dir, pattern = pattern, full.names = FALSE)
  } else {
    character()
  }

  if (length(candidates) == 0) {
    if (!is.null(fallback)) {
      return(file.path(search_dir, fallback))
    }
    # Return a non-existent conventional path; callers report it via missing-file checks.
    return(file.path(search_dir, label))
  }

  keys <- vapply(candidates, raw_input_vintage_key, numeric(1))
  mtimes <- as.numeric(file.info(file.path(search_dir, candidates))$mtime)

  # Newest declared vintage wins; mtime breaks ties and covers unversioned names.
  ordering <- order(keys, mtimes, decreasing = TRUE, na.last = TRUE)
  chosen <- candidates[ordering[1]]

  if (!quiet && length(candidates) > 1) {
    message(
      "Resolved ", label, " -> ", chosen,
      " (", length(candidates) - 1, " older candidate(s) ignored)"
    )
  }

  file.path(search_dir, chosen)
}

# Record what each versioned input actually resolved to, so an index run is replicable.
write_resolved_vintages <- function(resolved, raw_data_path) {
  resolved <- resolved[!vapply(resolved, is.null, logical(1))]
  if (length(resolved) == 0) return(invisible(NULL))

  payload <- list(
    resolved_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    inputs = lapply(names(resolved), function(nm) {
      path <- resolved[[nm]]
      list(
        id = nm,
        file = basename(path),
        exists = file.exists(path),
        vintage_key = {
          key <- raw_input_vintage_key(path)
          if (is.na(key)) NULL else format(key, scientific = FALSE)
        },
        mtime_utc = if (file.exists(path)) {
          format(file.info(path)$mtime, tz = "UTC", usetz = TRUE)
        } else {
          NULL
        }
      )
    })
  )

  out_path <- file.path(raw_data_path, "raw_inputs_resolved.yml")
  yaml::write_yaml(payload, out_path)
  invisible(out_path)
}
