# Generate raw inputs manifest from legacy scripts.
resolve_repo_root <- function() {
  # Prefer rprojroot if available (most robust)
  if (requireNamespace("rprojroot", quietly = TRUE)) {
    return(rprojroot::find_root(rprojroot::is_git_root))
  }
  
  # Fallback: start from script path if we have it, otherwise from getwd()
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  
  start <- if (length(file_arg) > 0) {
    sub("^--file=", "", file_arg[1])
  } else if (!is.null(sys.frame(1)$ofile)) {
    sys.frame(1)$ofile
  } else {
    ""
  }
  
  d <- if (nzchar(start)) {
    dirname(normalizePath(start, winslash = "/", mustWork = FALSE))
  } else {
    normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  }
  
  # Walk up until we find a .git directory
  while (!file.exists(file.path(d, ".git")) && dirname(d) != d) {
    d <- dirname(d)
  }
  
  if (!file.exists(file.path(d, ".git"))) {
    stop("Could not locate repo root (no .git found). Run from the repo directory or set OPSI_CONFIG/OPSI_WEIGHTS.")
  }
  
  d
}


if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("Package 'yaml' is required to write the manifest.")
}

repo_root <- resolve_repo_root()
legacy_dirs <- file.path(repo_root, c("legacy", "Legacy Scripts"))
legacy_dirs <- legacy_dirs[dir.exists(legacy_dirs)]

if (length(legacy_dirs) == 0) {
  stop("No legacy script directories found.")
}

legacy_files <- sort(unlist(lapply(legacy_dirs, list.files,
  pattern = "\\.R$", full.names = TRUE
)))

if (length(legacy_files) == 0) {
  stop("No legacy R scripts found in: ", paste(legacy_dirs, collapse = ", "))
}

extract_group_matches <- function(lines, pattern) {
  out <- character()
  for (line in lines) {
    matches <- regmatches(line, gregexpr(pattern, line, perl = TRUE))
    if (length(matches) == 0 || length(matches[[1]]) == 0) {
      next
    }
    for (match in matches[[1]]) {
      out <- c(out, sub(pattern, "\\1", match, perl = TRUE))
    }
  }
  out
}

normalize_relative_path <- function(path) {
  cleaned <- trimws(path)
  cleaned <- sub("^/+", "", cleaned)
  cleaned
}

is_file_like <- function(path) {
  path <- trimws(path)
  nzchar(path) & !grepl("/$", path) & grepl("\\.[A-Za-z0-9]+$", path)
}


pattern_paste0 <- "paste0\\(\\s*raw_data\\s*,\\s*['\"]([^'\"]+)['\"]\\s*\\)"
pattern_file_path <- "file\\.path\\(\\s*raw_data\\s*,\\s*['\"]([^'\"]+)['\"]\\s*\\)"
pattern_onedrive <- "['\"](OneDrive - RMI/[^'\"]+)['\"]"

all_paths <- character()
write_paths <- character()

for (script in legacy_files) {
  lines <- readLines(script, warn = FALSE)

  paste0_paths <- extract_group_matches(lines, pattern_paste0)
  file_path_paths <- extract_group_matches(lines, pattern_file_path)
  onedrive_paths <- extract_group_matches(lines, pattern_onedrive)

  onedrive_paths <- onedrive_paths[is_file_like(onedrive_paths)]

  raw_paths <- c(paste0_paths, file_path_paths)
  raw_paths <- vapply(raw_paths, normalize_relative_path, character(1))
  raw_paths <- raw_paths[raw_paths != ""]

  candidate_paths <- c(raw_paths, onedrive_paths)
  if (length(candidate_paths) > 0) {
    for (path in candidate_paths) {
      if (!path %in% all_paths) {
        all_paths <- c(all_paths, path)
      }
    }
  }

  write_lines <- lines[grepl("write\\.csv\\(", lines)]
  if (length(write_lines) > 0) {
    write_raw <- extract_group_matches(write_lines, pattern_paste0)
    write_raw <- vapply(write_raw, normalize_relative_path, character(1))
    write_raw <- write_raw[write_raw != ""]

    write_onedrive <- extract_group_matches(write_lines, pattern_onedrive)
    write_onedrive <- write_onedrive[is_file_like(write_onedrive)]

    write_paths <- unique(c(write_paths, write_raw, write_onedrive))
  }
}

manifest_entries <- lapply(all_paths, function(path) {
  entry <- list(path = path)
  if (path %in% write_paths) {
    entry$optional <- TRUE
  }
  entry
})

manifest_path <- file.path(repo_root, "config", "raw_inputs_manifest.yml")
if (!dir.exists(dirname(manifest_path))) {
  dir.create(dirname(manifest_path), recursive = TRUE)
}

yaml::write_yaml(manifest_entries, manifest_path)

message("Wrote raw inputs manifest: ", manifest_path)
