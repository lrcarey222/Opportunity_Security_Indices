# Change-layer stamps (docs/change-layer-architecture.md 2, 4): the IO half of attribution. Used by
# scripts/26 (the engine input ledger), scripts/91 (stamps written into the posture manifest) and scripts/45
# (fingerprints of historical commits). The pure halves live in R/change/posture_vintage.R.
#
# - Input ledger: every data file the engine run reads, with its md5. readRDS / read.table (read.csv) /
#   data.table::fread / readr::read_csv / readxl::read_excel / excel_sheets are traced for the run. Two runs whose
#   ledgers match read byte-identical inputs, so a result difference between them is the code's alone.
# - Method fingerprint: the git blob id of every file under config/change_layer.yml method_paths (engine, upstream,
#   presentation), from a commit (git ls-tree) or the working tree (git hash-object, so an export from uncommitted
#   code is fingerprinted as it ran). Blob ids, not tree ids: the two sources then agree file for file.
# - Raw-input stamp: id -> vintage key from raw_inputs_resolved.yml (scripts/utils/raw_inputs.R).

change_ledger_env <- new.env(parent = emptyenv())

# R's own files (package metadata read while namespaces load) are not data: skip anything under the R home or a
# library path.
change_ledger_skip <- local({
  roots <- NULL
  function(p) {
    if (is.null(roots)) roots <<- paste0(sub("/+$", "", normalizePath(unique(c(R.home(), .libPaths())), winslash = "/", mustWork = FALSE)), "/")
    any(startsWith(tolower(p), tolower(roots)))
  }
})

change_ledger_note <- function(x) {
  if (is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x) && file.exists(x) && !dir.exists(x)) {
    p <- normalizePath(x, winslash = "/", mustWork = FALSE)
    if (!change_ledger_skip(p)) assign(p, TRUE, envir = change_ledger_env)
  }
  invisible(NULL)
}

CHANGE_LEDGER_TRACES <- list(
  list(fn = "readRDS", ns = "base", arg = quote(file)),
  list(fn = "read.table", ns = "utils", arg = quote(file)),
  list(fn = "fread", ns = "data.table", arg = quote(if (!missing(file)) file else input)),
  list(fn = "read_csv", ns = "readr", arg = quote(file)),
  list(fn = "read_excel", ns = "readxl", arg = quote(path)),
  list(fn = "excel_sheets", ns = "readxl", arg = quote(path))
)

# Start recording the data files this R session reads (clears any earlier record).
change_ledger_start <- function() {
  rm(list = ls(change_ledger_env), envir = change_ledger_env)
  note <- change_ledger_note
  for (t in CHANGE_LEDGER_TRACES) {
    if (t$ns != "base" && !requireNamespace(t$ns, quietly = TRUE)) next
    suppressMessages(trace(t$fn, tracer = bquote(.(note)(tryCatch(.(t$arg), error = function(e) NULL))),
                           where = asNamespace(t$ns), print = FALSE))
  }
  invisible(TRUE)
}

change_ledger_stop <- function() {
  for (t in CHANGE_LEDGER_TRACES) {
    if (t$ns != "base" && !requireNamespace(t$ns, quietly = TRUE)) next
    suppressMessages(try(untrace(t$fn, where = asNamespace(t$ns)), silent = TRUE))
  }
  invisible(TRUE)
}

# The recorded files with md5 and size, paths relative to root when under it. Returns the list it writes.
change_ledger_write <- function(path, root, exclude = character()) {
  files <- sort(ls(change_ledger_env))
  files <- files[file.exists(files) & !normalizePath(files, winslash = "/", mustWork = FALSE) %in% normalizePath(exclude, winslash = "/", mustWork = FALSE)]
  root_n <- paste0(sub("/+$", "", normalizePath(root, winslash = "/", mustWork = FALSE)), "/")
  rel <- ifelse(startsWith(files, root_n), substring(files, nchar(root_n) + 1L), files)
  md5 <- unname(tools::md5sum(files))
  doc <- list(schema = "osi-engine-inputs", created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
              files = stats::setNames(as.list(md5), rel), bytes = stats::setNames(as.list(file.size(files)), rel))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(jsonlite::toJSON(doc, auto_unbox = TRUE, pretty = TRUE), path, useBytes = TRUE)
  invisible(doc)
}

change_qpath <- function(x) shQuote(x, type = if (.Platform$OS.type == "windows") "cmd" else "sh")

# path -> git blob id for every file under `paths`: at `commit` (git ls-tree), or in the working tree when commit is
# NULL (tracked and untracked-not-ignored files, hashed as git would store them).
change_git_blobs <- function(repo_root, paths, commit = NULL) {
  paths <- unlist(paths)
  if (!length(paths)) return(list())
  git <- function(args, input = NULL) suppressWarnings(system2("git", c("-C", change_qpath(repo_root), args), stdout = TRUE, stderr = FALSE, input = input))
  if (!is.null(commit)) {
    out <- git(c("ls-tree", "-r", commit, "--", paths))
    if (!length(out)) return(list())
    sha <- sub("^\\S+ \\S+ (\\S+)\t.*$", "\\1", out)
    nm <- sub("^[^\t]*\t", "", out)
    return(as.list(stats::setNames(sha, nm))[order(nm)])
  }
  files <- unique(c(git(c("ls-files", "--", paths)), git(c("ls-files", "--others", "--exclude-standard", "--", paths))))
  files <- sort(files[file.exists(file.path(repo_root, files))])
  if (!length(files)) return(list())
  sha <- git(c("hash-object", "--stdin-paths"), input = files)
  if (length(sha) != length(files)) stop("git hash-object returned ", length(sha), " ids for ", length(files), " files.")
  as.list(stats::setNames(sha, files))
}

# The method stamp: engine, upstream and presentation fingerprints, and the attribution id (engine + upstream: the
# code the engine run executes). method_paths: config/change_layer.yml method_paths.
change_method_stamp <- function(repo_root, method_paths, label = NULL, commit = NULL) {
  sets <- if (is.list(method_paths) && !is.null(names(method_paths))) method_paths else list(engine = method_paths)
  blobs <- lapply(sets, function(p) change_git_blobs(repo_root, p, commit))
  ids <- lapply(blobs, change_method_id)
  attr_blobs <- c(blobs$engine %||% list(), blobs$upstream %||% list())
  list(id = change_method_id(attr_blobs), engine = ids$engine %||% NA_character_, upstream = ids$upstream %||% NA_character_,
       presentation = ids$presentation %||% NA_character_, label = label, n_files = length(attr_blobs))
}

# id -> vintage key (or file name) from a raw_inputs_resolved.yml; NULL when the file is absent.
change_raw_inputs_stamp <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  y <- yaml::read_yaml(path)
  rows <- y$inputs %||% list()
  if (!length(rows)) return(NULL)
  stats::setNames(lapply(rows, function(r) as.character(r$vintage_key %||% r$file %||% "-")), vapply(rows, function(r) as.character(r$id), ""))
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
