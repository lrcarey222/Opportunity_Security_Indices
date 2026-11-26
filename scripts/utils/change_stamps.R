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
# - Input store and bridge runs: ledger files copied content-addressed; a bridge run of scripts/26 re-reads a previous
#   vintage's inputs from the store inside a sandbox and reports any read outside them.
# Needs R/change/posture_vintage.R (change_hash, change_method_id) and R/change/posture_bridge.R.

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
  list(fn = "readRDS", ns = "base", var = "file"),
  list(fn = "read.table", ns = "utils", var = "file"),
  list(fn = "fread", ns = "data.table", var = c("file", "input")),
  list(fn = "read_csv", ns = "readr", var = "file"),
  list(fn = "read_excel", ns = "readxl", var = "path"),
  list(fn = "excel_sheets", ns = "readxl", var = "path")
)

# The tracer for one reader: records the path read and, in a bridge run, rewrites it to the stored copy of the
# previous vintage's input (fn: function(path) -> path). For fread the first of file / input that was supplied.
change_tracer <- function(t, fn) {
  # a supplied argument only (read.table(text = ...) has no file); a failing tracer never breaks the read
  one <- function(v) bquote(if (!missing(.(as.name(v)))) .(as.name(v)) <- tryCatch(.(fn)(.(as.name(v))), error = function(e) .(as.name(v))))
  if (length(t$var) == 1L) return(one(t$var))
  bquote(if (!missing(.(as.name(t$var[1])))) .(one(t$var[1])) else .(one(t$var[2])))
}

# Start recording the data files this R session reads (clears any earlier record). bridge: a change_bridge_setup()
# result, to serve the previous vintage's inputs instead of the live files.
change_ledger_start <- function(bridge = NULL) {
  rm(list = ls(change_ledger_env), envir = change_ledger_env)
  fn <- if (is.null(bridge)) function(x) { change_ledger_note(x); x } else function(x) { change_ledger_note(x); change_bridge_redirect(bridge, x) }
  for (t in CHANGE_LEDGER_TRACES) {
    if (t$ns != "base" && !requireNamespace(t$ns, quietly = TRUE)) next
    suppressMessages(trace(t$fn, tracer = change_tracer(t, fn), where = asNamespace(t$ns), print = FALSE))
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

# ---- input store and bridge runs (docs/change-layer-architecture.md 4) --------------------------------------------

# Copy every ledger file into the content-addressed store (<store>/<md5[1:2]>/<md5>.<ext>); a copy already there is
# kept. Files over max_mb are not stored (a bridge that needs one reports it missing). Returns counts.
change_store_inputs <- function(ledger, root, store_dir, max_mb = 2048) {
  files <- ledger$files %||% list()
  if (!length(files)) return(invisible(list(stored = 0L, kept = 0L, skipped = character())))
  plan <- bridge_plan(files, root, dirs = list(), sandbox_dirs = list(), store_dir = store_dir)
  stored <- 0L; kept <- 0L; skipped <- character()
  for (i in seq_len(nrow(plan))) {
    src <- plan$abs[i]; dst <- plan$store[i]
    if (file.exists(dst)) { kept <- kept + 1L; next }
    if (!file.exists(src) || file.size(src) > max_mb * 1024^2) { skipped <- c(skipped, src); next }
    dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
    tmp <- paste0(dst, ".part")
    if (isTRUE(file.copy(src, tmp, overwrite = TRUE)) && identical(unname(tools::md5sum(tmp)), plan$md5[i])) {
      file.rename(tmp, dst); stored <- stored + 1L
    } else {
      unlink(tmp); skipped <- c(skipped, src)   # changed while copying, or the copy failed
    }
  }
  invisible(list(stored = stored, kept = kept, skipped = skipped))
}

# The previous vintage's ledger, from a JSON file (a posture manifest with stamps, or an engine input ledger) or a
# vintage id in data/vintages/posture/index.json (read from git at that vintage's commit).
change_bridge_source <- function(source, root) {
  if (file.exists(source)) {
    doc <- jsonlite::fromJSON(source, simplifyVector = FALSE)
  } else {
    ix_path <- file.path(root, "data", "vintages", "posture", "index.json")
    ix <- if (file.exists(ix_path)) jsonlite::fromJSON(ix_path, simplifyVector = FALSE) else list(vintages = list())
    h <- Filter(function(v) identical(v$vintage_id, source), ix$vintages)
    if (!length(h)) stop("Bridge source ", source, " is neither a file nor a vintage id in data/vintages/posture/index.json.")
    h <- h[[1]]
    if (is.null(h$core_commit) || is.na(h$core_commit)) stop("Vintage ", source, " is provisional (not committed): bridge from its manifest file instead.")
    tf <- tempfile(fileext = ".json")
    system2("git", c("-C", change_qpath(root), "show", paste0(h$core_commit, ":", dirname(h$core_file), "/manifest.json")), stdout = tf, stderr = FALSE)
    doc <- jsonlite::fromJSON(tf, simplifyVector = FALSE)
  }
  inputs <- doc$stamps$inputs %||% doc
  if (is.null(inputs$files) || !length(inputs$files)) {
    stop("Bridge source ", source, " has no engine input ledger: it predates input stamping (2026-11-25), so it cannot be bridged.")
  }
  id <- inputs$id %||% paste0("i-", change_hash(paste0(names(inputs$files), "=", unlist(inputs$files))))
  list(source = source, inputs_id = id, files = inputs$files)
}

# Prepare a bridge run: the sandbox directories (writes go there, never to the live outputs), the previous inputs
# materialized in them from the store, and the redirect map for inputs outside the data directories. Returns NULL
# when source is empty (a normal run). dirs: named live data directories (outputs, processed, raw, diagnostics).
change_bridge_setup <- function(source, root, dirs, sandbox, store_dir) {
  if (is.null(source) || !nzchar(source)) return(NULL)
  src <- change_bridge_source(source, root)
  if (!nzchar(sandbox)) sandbox <- file.path(root, "outputs", "change_layer", "bridge", src$inputs_id)
  unlink(sandbox, recursive = TRUE)
  sdirs <- bridge_sandbox_dirs(dirs, sandbox)
  for (d in sdirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  plan <- bridge_plan(src$files, root, dirs, sdirs, store_dir)
  have <- file.exists(plan$store)
  for (i in which(have & !is.na(plan$target))) {
    dir.create(dirname(plan$target[i]), recursive = TRUE, showWarnings = FALSE)
    if (!isTRUE(suppressWarnings(file.link(plan$store[i], plan$target[i])))) file.copy(plan$store[i], plan$target[i], overwrite = TRUE)
  }
  state <- new.env(parent = emptyenv())
  state$redirected <- character(); state$leaks <- character()
  state$missing <- plan$abs[!have]
  norm <- function(p) sub("/+$", "", normalizePath(p, winslash = "/", mustWork = FALSE))
  list(source = src$source, inputs_id = src$inputs_id, sandbox = norm(sandbox), dirs = sdirs, plan = plan,
       redirect = stats::setNames(plan$store, tolower(plan$abs))[have & is.na(plan$target)], state = state,
       n_materialized = sum(have & !is.na(plan$target)))
}

# Serve one read in a bridge run: a sandbox file as is; an earlier input outside the data directories from the
# store; anything else is a leak (the run read data the previous vintage did not), recorded, read live.
change_bridge_redirect <- function(bridge, x) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) return(x)
  a <- sub("/+$", "", normalizePath(x, winslash = "/", mustWork = FALSE))
  if (change_ledger_skip(a) || startsWith(tolower(a), tolower(paste0(bridge$sandbox, "/")))) return(x)
  hit <- bridge$redirect[tolower(a)]
  if (!is.na(hit)) { bridge$state$redirected <- c(bridge$state$redirected, a); return(unname(hit)) }
  if (file.exists(a) && !dir.exists(a)) bridge$state$leaks <- unique(c(bridge$state$leaks, a))
  x
}

# The bridge report beside the sandboxed outputs. pure: no leak and nothing missing from the store.
change_bridge_finish <- function(bridge, method) {
  st <- bridge$state
  rep <- list(schema = "osi-bridge-run", created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
              source = bridge$source, inputs_id = bridge$inputs_id, method = method,
              pure = !length(st$leaks) && !length(st$missing),
              n_materialized = bridge$n_materialized, n_redirected = length(unique(st$redirected)),
              leaks = I(st$leaks), missing = I(st$missing))
  writeLines(jsonlite::toJSON(rep, auto_unbox = TRUE, pretty = TRUE, null = "null"), file.path(bridge$sandbox, "bridge_report.json"), useBytes = TRUE)
  invisible(rep)
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
