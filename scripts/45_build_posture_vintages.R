# Repo root: walk up from this script (or the working directory) to the folder holding config/change_layer.yml.
# This script reads only git history and committed files, so it does not need scripts/utils/bootstrap.R and the
# local config/config.yml data paths.
repo_root <- local({
  fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  starts <- unique(c(if (length(fa)) dirname(sub("^--file=", "", fa[1])), getwd()))
  for (s in starts) {
    d <- normalizePath(s, winslash = "/", mustWork = FALSE)
    while (dirname(d) != d) {
      if (file.exists(file.path(d, "config", "change_layer.yml"))) return(d)
      d <- dirname(d)
    }
  }
  stop("Run from inside the repository: config/change_layer.yml not found.")
})

# Change layer, slice 1 (docs/change-layer-architecture.md): posture vintages and what changed between them.
#
# 1. Walks the git history of the posture bundle (dashboard/public/data/posture/manifest.json on HEAD's ancestry),
#    reads each committed core with `git show`, dedupes by content and records each distinct core as a vintage:
#    data/vintages/posture/<vintage_id>.csv.gz (normalized cells) + index.json (headers: stamps, method
#    fingerprint from `git rev-parse <commit>:<path>` over config/change_layer.yml method_paths, data stamp).
# 2. Compares every vintage with its previous comparable vintage (R/change/posture_compare.R), attaches accepted
#    analytical impacts (config/review/analytical_impacts.yml) and writes outputs/change_layer/posture_changes.csv.
# 3. Writes the dashboard bundle dashboard/public/data/changes/{manifest.json, changes.<md5-8>.json}.
#
#   --include-worktree   add the working tree's posture core as a provisional vintage when it is not committed
#   --raw-inputs PATH    raw_inputs_resolved.yml for the provisional vintage (makes its data stamp verifiable)
#   --dry-run            compute and report; write nothing
# Idempotent: the same history gives the same vintages, changes and bundle (bar the bundle's created stamp).

library(dplyr)
for (f in c("R/qc/qc_core.R", "R/qc/qc_drift.R", "R/change/posture_vintage.R", "R/change/posture_compare.R",
            "R/change/posture_explain.R", "R/change/posture_impacts.R", "R/change/change_export.R")) source(file.path(repo_root, f))

args <- commandArgs(trailingOnly = TRUE)
include_worktree <- "--include-worktree" %in% args
arg_value <- function(name) { i <- match(name, args); if (is.na(i) || i == length(args)) NULL else args[i + 1] }
# --raw-inputs PATH: a raw_inputs_resolved.yml (scripts/utils/raw_inputs.R write_resolved_vintages) for the
# provisional vintage; it makes that vintage's data stamp verifiable (id -> vintage_key per input)
raw_inputs <- local({
  p <- arg_value("--raw-inputs")
  if (is.null(p)) return(NULL)
  y <- yaml::read_yaml(p)
  rows <- y$inputs %||% y
  stats::setNames(lapply(rows, function(r) as.character(r$vintage_key %||% r$file %||% "-")), vapply(rows, function(r) as.character(r$id), ""))
})
dry_run <- "--dry-run" %in% args
cfg <- yaml::read_yaml(file.path(repo_root, "config", "change_layer.yml"))
posture_dir <- "dashboard/public/data/posture"
vint_dir <- file.path(repo_root, "data", "vintages", "posture")
bundle_dir <- file.path(repo_root, "dashboard", "public", "data", "changes")

qroot <- shQuote(repo_root, type = if (.Platform$OS.type == "windows") "cmd" else "sh")
git <- function(...) suppressWarnings(system2("git", c("-C", qroot, ...), stdout = TRUE, stderr = FALSE))
git_to_file <- function(spec) {
  tf <- tempfile(fileext = ".json")
  status <- suppressWarnings(system2("git", c("-C", qroot, "show", spec), stdout = tf, stderr = FALSE))
  if (!identical(as.integer(status), 0L) || !file.exists(tf) || file.size(tf) == 0) { unlink(tf); return(NULL) }
  tf
}
git_has <- function(rev) identical(as.integer(suppressWarnings(system2("git", c("-C", qroot, "cat-file", "-e", paste0(rev, "^{commit}")), stdout = FALSE, stderr = FALSE))), 0L)
md5_file <- function(p) unname(tools::md5sum(p))
md5_8 <- function(txt) { tf <- tempfile(); on.exit(unlink(tf)); writeBin(charToRaw(enc2utf8(txt)), tf); substr(md5_file(tf), 1, 8) }
method_of <- function(commit, unverified) {
  shas <- stats::setNames(lapply(cfg$method_paths, function(p) { s <- git("rev-parse", paste0(commit, ":", p)); if (length(s)) s[1] else "absent" }), unlist(cfg$method_paths))
  list(id = change_method_id(shas), label = cfg$method_label, code_commit = commit, unverified = unverified, paths = shas)
}

# ---- 1. vintages ------------------------------------------------------------------------------------------------
# (system2 passes arguments unquoted: keep each one free of spaces)
log <- git("log", "--reverse", "--format=%H", "--", file.path(posture_dir, "manifest.json"))
cands <- lapply(log, function(x) list(commit = x, short = substr(x, 1, 8), worktree = FALSE))
if (include_worktree) cands <- c(cands, list(list(commit = NA_character_, short = "worktree", worktree = TRUE)))

seen <- character()
vintages <- list()
tables <- list()
for (cd in cands) {
  man_path <- if (cd$worktree) file.path(repo_root, posture_dir, "manifest.json") else git_to_file(paste0(cd$commit, ":", posture_dir, "/manifest.json"))
  if (is.null(man_path) || !file.exists(man_path)) next
  man <- jsonlite::fromJSON(man_path, simplifyVector = FALSE)
  core_file <- man$core
  core_path <- if (cd$worktree) file.path(repo_root, posture_dir, core_file) else git_to_file(paste0(cd$commit, ":", posture_dir, "/", core_file))
  if (is.null(core_path) || !file.exists(core_path)) { message("skip ", cd$short, ": core ", core_file, " not in the commit"); next }
  # the export names each core by the md5 of its JSON text, so the file name is the content key (a checked-out copy can
  # differ in line endings from the blob, so its own md5 is not)
  h <- core_file
  if (h %in% seen) next
  seen <- c(seen, h)
  core <- jsonlite::fromJSON(core_path, simplifyVector = FALSE)
  if (is.null(core$cells$c) || is.null(core$meta$stages)) { message("skip ", cd$short, ": not a columnar posture core"); next }
  tbl <- posture_vintage_from_core(core)
  stamp <- core$git_commit %||% man$git_commit
  stamp_source <- if (!is.null(stamp)) "bundle" else "bundle_commit"
  dirty <- !is.null(stamp) && grepl("-dirty$", stamp)
  code_commit <- if (!is.null(stamp) && git_has(sub("-dirty$", "", stamp))) sub("-dirty$", "", stamp) else if (cd$worktree) "HEAD" else cd$commit
  if (!is.null(stamp) && code_commit != sub("-dirty$", "", stamp)) stamp_source <- "bundle_commit"   # stamp not in this repo's history
  created <- core$created %||% man$created
  vid <- sprintf("%s-%s", substr(created, 1, 10), sub("^posture-core\\.([0-9a-f]+)\\.json$", "\\1", core_file))
  csv_txt <- paste(utils::capture.output(utils::write.csv(tbl, row.names = FALSE, na = "")), collapse = "\n")
  hdr <- posture_vintage_header(
    vintage_id = vid, created = created, git_commit = stamp %||% cd$short, core_file = file.path(posture_dir, core_file),
    core_commit = if (cd$worktree) NA_character_ else cd$short, method = method_of(code_commit, dirty || cd$worktree),
    data_id = change_data_id(core$meta, unlist(cfg$data_stamp_fields), raw_inputs = if (cd$worktree) raw_inputs else NULL),
    data_stamp = c(stats::setNames(lapply(unlist(cfg$data_stamp_fields), function(f) core$meta[[f]] %||% NA_character_), unlist(cfg$data_stamp_fields)),
                   list(verified = cd$worktree && !is.null(raw_inputs), raw_inputs = if (cd$worktree) raw_inputs else NULL)),
    techs = unlist(core$meta$techs), stages = unlist(core$meta$stages),
    bars = posture_vintage_bars(core$meta, cfg$thresholds$score$default_bars), n_cells = nrow(tbl),
    sha256 = digest::digest(csv_txt, algo = "sha256", serialize = FALSE), stamp_source = stamp_source, provisional = cd$worktree)
  vintages[[vid]] <- hdr
  tables[[vid]] <- tbl
  message(sprintf("vintage %s  commit %-14s method %s%s  data %s  cells %d", vid, hdr$git_commit, hdr$method$id,
                  if (hdr$method$unverified) "?" else "", hdr$data$id, nrow(tbl)))
}
ord <- order(vapply(vintages, function(h) h$created, ""))
vintages <- vintages[ord]
tables <- tables[ord]
if (length(vintages) < 2) stop("Fewer than two posture vintages found: nothing to compare.")

# ---- 2. compare -------------------------------------------------------------------------------------------------
impacts_path <- file.path(repo_root, "config", "review", "analytical_impacts.yml")
impacts <- if (file.exists(impacts_path)) (yaml::read_yaml(impacts_path)$impacts %||% list()) else list()
probs <- validate_analytical_impacts(impacts)
if (length(probs)) stop("config/review/analytical_impacts.yml:\n  ", paste(probs, collapse = "\n  "))

ids <- names(vintages)
prev_k <- posture_previous_comparable(unname(vintages))
results <- list()
for (i in seq_along(ids)[-1]) {
  if (is.na(prev_k[i]) || prev_k[i] != i - 1L) {
    results[[length(results) + 1]] <- posture_compare(tables[[i - 1]], tables[[i]], vintages[[i - 1]], vintages[[i]], cfg)
  }
  if (!is.na(prev_k[i])) {
    k <- prev_k[i]
    r <- posture_compare(tables[[k]], tables[[i]], vintages[[k]], vintages[[i]], cfg)
    r$changes <- attach_impacts(r$changes, impacts, vintages)
    results[[length(results) + 1]] <- r
  }
}
for (r in results) {
  p <- r$pair
  message(sprintf("%s -> %s  %s  method %s  data %s  %s", p$prev, p$cur, if (p$comparable) "comparable" else paste("NOT comparable:", p$reason),
                  if (p$method_changed) "CHANGED" else "same", if (p$data_changed) "changed" else "same",
                  if (p$comparable) sprintf("meaningful %d / %d (rec %d, score %d, data %d, ev %d, conf %d; real %d, method %d, mixed %d, unattributed %d)",
                                            p$counts$meaningful, p$n_cells, p$counts$recommendation, p$counts$score, p$counts$data,
                                            p$counts$evidence, p$counts$confidence, p$counts$real_world, p$counts$methodology, p$counts$mixed, p$counts$unattributed) else ""))
}
if (dry_run) { message("--dry-run: nothing written"); quit(save = "no") }

# ---- write ------------------------------------------------------------------------------------------------------
to_json <- function(x, pretty = FALSE) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, na = "null", null = "null", digits = NA, pretty = pretty))
dir.create(vint_dir, recursive = TRUE, showWarnings = FALSE)
keep <- character()
for (vid in ids) {
  if (isTRUE(vintages[[vid]]$provisional)) next   # provisional vintages are compared, never archived
  f <- file.path(vint_dir, paste0(vid, ".csv.gz"))
  keep <- c(keep, basename(f))
  if (!file.exists(f)) { con <- gzfile(f, "w"); utils::write.csv(tables[[vid]], con, row.names = FALSE, na = ""); close(con) }
}
idx <- lapply(Filter(function(h) !isTRUE(h$provisional), vintages), function(h) { h$method$paths <- as.list(h$method$paths); h })
writeLines(to_json(list(schema = "osi-posture-vintages", layer_version = cfg$layer_version, vintages = unname(idx)), pretty = TRUE),
           file.path(vint_dir, "index.json"), useBytes = TRUE)
all_changes <- dplyr::bind_rows(Filter(nrow, lapply(results, `[[`, "changes")))
# every change record, for analysts: derived from the vintages + config, so it goes to the git-ignored outputs/
out_dir <- file.path(repo_root, "outputs", "change_layer")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(all_changes, file.path(out_dir, "posture_changes.csv"), row.names = FALSE, na = "")

created <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
head_sha <- git("rev-parse", "--short", "HEAD")[1]
# the dashboard carries full records for the last `detail_pairs` comparable pairs; older pairs keep their summary
# (every record stays in outputs/change_layer/posture_changes.csv)
comp <- which(vapply(results, function(r) isTRUE(r$pair$comparable), logical(1)))
detail <- utils::tail(comp, cfg$bundle$detail_pairs %||% 4L)
bres <- lapply(seq_along(results), function(i) { r <- results[[i]]; r$pair$detail <- i %in% detail; if (!i %in% detail) r$changes <- r$changes[0, ]; r })
bundle <- change_bundle(unname(vintages), bres, impacts = impacts, sources = cfg$sources, created = created, git_commit = head_sha)
txt <- to_json(bundle)
dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)
unlink(list.files(bundle_dir, pattern = "^changes\\..*\\.json$", full.names = TRUE))
file <- sprintf("changes.%s.json", md5_8(txt))
writeLines(txt, file.path(bundle_dir, file), useBytes = TRUE)
writeLines(to_json(list(schema = CHANGE_SCHEMA, schema_version = CHANGE_SCHEMA_VERSION, created = created, git_commit = head_sha,
                        changes = file, latest = utils::tail(ids, 1), n_vintages = length(ids), n_changes = nrow(all_changes)), pretty = TRUE),
           file.path(bundle_dir, "manifest.json"), useBytes = TRUE)
message(sprintf("wrote %d vintages to %s, %d change records, bundle %s (%.0f KB)", length(keep), vint_dir, nrow(all_changes), file, nchar(txt, "bytes") / 1024))
