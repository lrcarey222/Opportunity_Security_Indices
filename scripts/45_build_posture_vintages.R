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
#   --bridge DIR         ingest a bridge run of scripts/26 (OPSI_BRIDGE; DIR holds bridge_report.json and the sandboxed
#                        strategic_position_tbl.rds): stored as data/vintages/posture/bridges/<prev>__<cur>.csv.gz and
#                        used, when pure, to split that pair's method and data changes exactly
#   --prune-store        prune the input store (config input_store.dir) to the inputs of stamped vintages and of the
#                        current engine ledger: what a bridge could still need
#   --dry-run            compute and report; write nothing
# Idempotent: the same history gives the same vintages, changes and bundle (bar the bundle's created stamp).

library(dplyr)
for (f in c("R/qc/qc_core.R", "R/qc/qc_drift.R", "R/change/posture_vintage.R", "R/change/posture_compare.R",
            "R/change/posture_explain.R", "R/change/posture_impacts.R", "R/change/change_export.R",
            "R/change/posture_bridge.R")) source(file.path(repo_root, f))

args <- commandArgs(trailingOnly = TRUE)
include_worktree <- "--include-worktree" %in% args
arg_value <- function(name) { i <- match(name, args); if (is.na(i) || i == length(args)) NULL else args[i + 1] }
dry_run <- "--dry-run" %in% args
cfg <- yaml::read_yaml(file.path(repo_root, "config", "change_layer.yml"))
source(file.path(repo_root, "scripts", "utils", "change_stamps.R"))
# --raw-inputs PATH: a raw_inputs_resolved.yml for a provisional vintage whose manifest carries no stamps
raw_inputs_arg <- change_raw_inputs_stamp(arg_value("--raw-inputs"))
posture_dir <- "dashboard/public/data/posture"
vint_dir <- file.path(repo_root, "data", "vintages", "posture")
bundle_dir <- file.path(repo_root, "dashboard", "public", "data", "changes")

qroot <- change_qpath(repo_root)
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
to_json_b <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, na = "null", null = "null", digits = NA, pretty = TRUE))
vintage_col_classes <- stats::setNames(ifelse(POSTURE_VINTAGE_COLS %in% POSTURE_VINTAGE_NUM, "numeric", "character"), POSTURE_VINTAGE_COLS)
read_vintage <- function(f) {
  hdr <- names(utils::read.csv(gzfile(f), nrows = 1, check.names = FALSE))
  x <- utils::read.csv(gzfile(f), colClasses = vintage_col_classes[intersect(names(vintage_col_classes), hdr)], na.strings = "", check.names = FALSE)
  for (m in setdiff(POSTURE_VINTAGE_COLS, names(x))) x[[m]] <- if (m %in% POSTURE_VINTAGE_NUM) NA_real_ else NA_character_
  tibble::as_tibble(x[, POSTURE_VINTAGE_COLS])
}

# Archived committed vintages of this layer version are reused (header + table) instead of re-parsing their cores;
# provisional ones are rebuilt.
cache <- local({
  p <- file.path(vint_dir, "index.json")
  if (!file.exists(p)) return(list())
  ix <- jsonlite::fromJSON(p, simplifyVector = FALSE)
  if (!identical(as.integer(ix$layer_version %||% 0L), as.integer(cfg$layer_version))) return(list())
  keep <- Filter(function(h) !isTRUE(h$provisional) && file.exists(file.path(vint_dir, paste0(h$vintage_id, ".csv.gz"))), ix$vintages)
  stats::setNames(keep, vapply(keep, function(h) basename(h$core_file), ""))
})

# ---- 1. vintages ------------------------------------------------------------------------------------------------
# (system2 passes arguments unquoted: keep each one free of spaces)
# --first-parent: this branch's own line of shipped bundles. A merge that brings in another branch's bundle is one
# step on that line (the merge commit carries it); the other branch's intermediate exports are not interleaved, which
# would make parallel work read as changes flipping back and forth. (The default path-limited walk is wrong too: at a
# merge that kept the incoming bundle it follows only that side and drops this branch's own vintages.)
log <- git("log", "--first-parent", "--reverse", "--format=%H", "--", file.path(posture_dir, "manifest.json"))
cands <- lapply(log, function(x) list(commit = x, short = substr(x, 1, 8), worktree = FALSE))
if (include_worktree) cands <- c(cands, list(list(commit = NA_character_, short = "worktree", worktree = TRUE)))

seen <- character()
store_keep <- character()   # md5s of every stamped vintage's inputs (what a bridge could still need)
cache_hits <- list()
vintages <- list()
tables <- list()
for (cd in cands) {
  man_path <- if (cd$worktree) file.path(repo_root, posture_dir, "manifest.json") else git_to_file(paste0(cd$commit, ":", posture_dir, "/manifest.json"))
  if (is.null(man_path) || !file.exists(man_path)) next
  man <- jsonlite::fromJSON(man_path, simplifyVector = FALSE)
  core_file <- man$core
  # the export names each core by the md5 of its JSON text, so the file name is the content key (a checked-out copy
  # can differ in line endings from the blob, so its own md5 is not)
  store_keep <- c(store_keep, unlist(man$stamps$inputs$files))
  if (core_file %in% seen) next
  hit <- if (!cd$worktree) cache[[core_file]] else NULL
  if (!is.null(hit)) {
    seen <- c(seen, core_file)
    vintages[[hit$vintage_id]] <- hit
    cache_hits[[hit$vintage_id]] <- TRUE
    tables[[hit$vintage_id]] <- read_vintage(file.path(vint_dir, paste0(hit$vintage_id, ".csv.gz")))
    next
  }
  core_path <- if (cd$worktree) file.path(repo_root, posture_dir, core_file) else git_to_file(paste0(cd$commit, ":", posture_dir, "/", core_file))
  if (is.null(core_path) || !file.exists(core_path)) { message("skip ", cd$short, ": core ", core_file, " not in the commit"); next }
  seen <- c(seen, core_file)
  core <- jsonlite::fromJSON(core_path, simplifyVector = FALSE)
  if (is.null(core$cells$c) || is.null(core$meta$stages)) { message("skip ", cd$short, ": not a columnar posture core"); next }
  tbl <- posture_vintage_from_core(core)
  stamp <- core$git_commit %||% man$git_commit
  stamp_source <- if (!is.null(stamp)) "bundle" else "bundle_commit"
  dirty <- !is.null(stamp) && grepl("-dirty$", stamp)
  code_commit <- if (!is.null(stamp) && git_has(sub("-dirty$", "", stamp))) sub("-dirty$", "", stamp) else if (cd$worktree) "HEAD" else cd$commit
  if (!is.null(stamp) && code_commit != sub("-dirty$", "", stamp)) stamp_source <- "bundle_commit"   # stamp not in this repo's history
  # Stamps written by scripts/91 at export are exact: they fingerprint the tree the bundle was built from, dirty or
  # not. Older bundles are fingerprinted from git at their code commit, unverified when built from uncommitted code.
  st <- man$stamps
  method <- if (!is.null(st$method$id)) {
    c(st$method[intersect(c("id", "engine", "upstream", "presentation", "label", "n_files"), names(st$method))], list(unverified = FALSE, source = "manifest"))
  } else {
    m <- change_method_stamp(repo_root, cfg$method_paths, label = cfg$method_label, commit = if (cd$worktree) NULL else code_commit)
    c(m, list(unverified = dirty || cd$worktree, source = if (cd$worktree) "worktree" else "git"))
  }
  raw <- st$raw_inputs %||% (if (cd$worktree) raw_inputs_arg else NULL)
  fields <- unlist(cfg$data_stamp_fields)
  created <- core$created %||% man$created
  vid <- sprintf("%s-%s", substr(created, 1, 10), sub("^posture-core[.]([0-9a-f]+)[.]json$", "\\1", core_file))
  csv_txt <- paste(utils::capture.output(utils::write.csv(tbl, row.names = FALSE, na = "")), collapse = "\n")
  hdr <- posture_vintage_header(
    vintage_id = vid, created = created, git_commit = stamp %||% cd$short, core_file = file.path(posture_dir, core_file),
    core_commit = if (cd$worktree) NA_character_ else cd$short, method = method,
    data_id = change_data_id(core$meta, fields, raw_inputs = raw),
    data_stamp = c(stats::setNames(lapply(fields, function(f) core$meta[[f]] %||% NA_character_), fields),
                   list(verified = !is.null(raw), inputs_id = st$inputs$id %||% NA_character_, n_inputs = st$inputs$n %||% NA_integer_,
                        n_raw_inputs = length(raw),
                        # the ledger (path -> md5) without method-path files: compared file by file between vintages
                        inputs = if (is.null(st$inputs$files)) NULL else {
                          d <- change_input_diff(list(), st$inputs$files, unlist(cfg$method_paths))
                          as.list(stats::setNames(unlist(st$inputs$files)[d$path], d$path))
                        })),
    techs = unlist(core$meta$techs), stages = unlist(core$meta$stages),
    bars = posture_vintage_bars(core$meta, cfg$thresholds$score$default_bars), n_cells = nrow(tbl),
    sha256 = digest::digest(csv_txt, algo = "sha256", serialize = FALSE), stamp_source = stamp_source, provisional = cd$worktree)
  vintages[[vid]] <- hdr
  tables[[vid]] <- tbl
}
for (h in vintages) message(sprintf("vintage %s  commit %-14s method %s%s  data %s  inputs %s  cells %d%s", h$vintage_id, h$git_commit, h$method$id,
                                    if (isTRUE(h$method$unverified)) "?" else "", h$data$id, h$data$inputs_id %||% "-", h$n_cells,
                                    if (isTRUE(h$provisional)) "  (provisional)" else ""))
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

# ---- bridges ----------------------------------------------------------------------------------------------------
bridge_dir <- file.path(vint_dir, "bridges")
bridges_path <- file.path(bridge_dir, "bridges.json")
bridges <- if (file.exists(bridges_path)) jsonlite::fromJSON(bridges_path, simplifyVector = FALSE)$bridges %||% list() else list()
new_bridge <- arg_value("--bridge")
if (!is.null(new_bridge)) {
  rep_path <- file.path(new_bridge, "bridge_report.json")
  if (!file.exists(rep_path)) stop("--bridge ", new_bridge, ": no bridge_report.json (run scripts/26 with OPSI_BRIDGE first).")
  rep <- jsonlite::fromJSON(rep_path, simplifyVector = FALSE)
  tbl_path <- list.files(new_bridge, pattern = "^strategic_position_tbl[.]rds$", recursive = TRUE, full.names = TRUE)
  if (!length(tbl_path)) stop("--bridge ", new_bridge, ": no strategic_position_tbl.rds in the sandbox.")
  prev_id <- names(vintages)[vapply(vintages, function(h) identical(h$data$inputs_id, rep$inputs_id), logical(1))]
  cur_id <- names(vintages)[vapply(vintages, function(h) identical(h$method$id, rep$method$id), logical(1))]
  if (!length(prev_id)) stop("--bridge: no vintage has the bridge's inputs (", rep$inputs_id, ").")
  if (!length(cur_id)) stop("--bridge: no vintage was built with the bridge's method (", rep$method$id, "): export one with this code first.")
  prev_id <- prev_id[1]; cur_id <- utils::tail(cur_id, 1)
  if (identical(prev_id, cur_id)) {
    # same inputs and same method as one vintage: a reproducibility check, nothing to split
    # compare like for like: the vintage is decoded from the core (headline techs, assessed cells, 3 dp)
    btbl <- posture_vintage_from_table(readRDS(tbl_path[1]))
    v <- tables[[cur_id]]
    m <- merge(v, btbl, by = c("iso3", "tech", "stage"), suffixes = c("_v", "_b"))
    num_bad <- sum(vapply(POSTURE_VINTAGE_NUM, function(f) sum(abs(round(m[[paste0(f, "_b")]], 3) - m[[paste0(f, "_v")]]) > 1e-9, na.rm = TRUE) +
                                                         sum(xor(is.na(m[[paste0(f, "_b")]]), is.na(m[[paste0(f, "_v")]]))), numeric(1)))
    choice_bad <- sum(dplyr::coalesce(m$choice_v, "-") != dplyr::coalesce(m$choice_b, "-"))
    same <- nrow(m) == nrow(v) && num_bad == 0 && choice_bad == 0
    message(sprintf("  %d of %d vintage cells matched; %d choice and %d value differences", nrow(m), nrow(v), choice_bad, num_bad))
    message(sprintf("bridge has the inputs and method of %s itself: a reproducibility check (%s, %s); not stored", cur_id,
                    if (isTRUE(rep$pure)) "pure" else "IMPURE", if (same) "it reproduces the vintage" else "it does NOT reproduce the vintage"))
    new_bridge <- NULL
  }
}
if (!is.null(new_bridge)) {
  btbl <- posture_vintage_from_table(readRDS(tbl_path[1]))
  btbl <- btbl[btbl$tech %in% unlist(vintages[[cur_id]]$techs) & btbl$stage %in% unlist(vintages[[cur_id]]$stages), , drop = FALSE]
  entry <- list(prev = prev_id, cur = cur_id, file = sprintf("%s__%s.csv.gz", prev_id, cur_id), pure = isTRUE(rep$pure),
                inputs_id = rep$inputs_id, method = list(id = rep$method$id), created = rep$created, source = rep$source,
                n_leaks = length(rep$leaks), n_missing = length(rep$missing), n_cells = nrow(btbl))
  if (!dry_run) {
    dir.create(bridge_dir, recursive = TRUE, showWarnings = FALSE)
    con <- gzfile(file.path(bridge_dir, entry$file), "w"); utils::write.csv(btbl, con, row.names = FALSE, na = ""); close(con)
    bridges <- c(Filter(function(b) !(identical(b$prev, prev_id) && identical(b$cur, cur_id)), bridges), list(entry))
    writeLines(to_json_b(list(schema = "osi-posture-bridges", bridges = bridges)), bridges_path, useBytes = TRUE)
  } else {
    bridges <- c(bridges, list(entry))
  }
  message(sprintf("bridge %s -> %s ingested (%s; %d leaks, %d missing)", prev_id, cur_id, if (entry$pure) "pure" else "IMPURE: not used",
                  entry$n_leaks, entry$n_missing))
}
bridge_for <- function(k, i) {
  b <- Filter(function(b) identical(b$prev, ids[k]) && identical(b$cur, ids[i]), bridges)
  if (!length(b)) return(NULL)
  b <- b[[1]]
  if (!bridge_usable(b, vintages[[k]], vintages[[i]])) { message("bridge ", b$prev, " -> ", b$cur, " not used (impure or stale)"); return(NULL) }
  f <- file.path(bridge_dir, b$file)
  if (!file.exists(f)) return(NULL)
  read_vintage(f)
}
prev_k <- posture_previous_comparable(unname(vintages))
results <- list()
for (i in seq_along(ids)[-1]) {
  if (is.na(prev_k[i]) || prev_k[i] != i - 1L) {
    results[[length(results) + 1]] <- posture_compare(tables[[i - 1]], tables[[i]], vintages[[i - 1]], vintages[[i]], cfg)
  }
  if (!is.na(prev_k[i])) {
    k <- prev_k[i]
    r <- posture_compare(tables[[k]], tables[[i]], vintages[[k]], vintages[[i]], cfg, bridge = bridge_for(k, i))
    r$changes <- attach_impacts(r$changes, impacts, vintages)
    results[[length(results) + 1]] <- r
  }
}
for (r in results) {
  p <- r$pair
  message(sprintf("%s -> %s  %s  method %s  data %s  %s", p$prev, p$cur, if (p$comparable) "comparable" else paste("NOT comparable:", p$reason),
                  if (p$method_changed) "CHANGED" else "same", p$data_relation,
                  if (p$comparable) sprintf("meaningful %d / %d (rec %d, score %d, data %d, ev %d, conf %d; real %d, method %d, mixed %d, unattributed %d)",
                                            p$counts$meaningful, p$n_cells, p$counts$recommendation, p$counts$score, p$counts$data,
                                            p$counts$evidence, p$counts$confidence, p$counts$real_world, p$counts$methodology, p$counts$mixed, p$counts$unattributed) else ""))
}
# ---- input store ------------------------------------------------------------------------------------------------
prune_store <- function() {
  if (!"--prune-store" %in% args) return(invisible(NULL))
  store_dir <- file.path(repo_root, cfg$input_store$dir %||% "data/vintages/inputs")
  # the current engine ledger too (a run not yet exported may be exported next)
  local_cfg <- file.path(repo_root, "config", "config.yml")
  if (file.exists(local_cfg)) {
    lc <- yaml::read_yaml(local_cfg)
    od <- if (!is.null(lc$outputs_dir) && nzchar(lc$outputs_dir)) file.path(repo_root, lc$outputs_dir) else file.path(repo_root, lc$processed_dir %||% "data/processed", "outputs")
    lp <- file.path(od, cfg$engine_inputs_file %||% "strategic_position_inputs.json")
    if (file.exists(lp)) store_keep <<- c(store_keep, unlist(jsonlite::fromJSON(lp, simplifyVector = FALSE)$files))
  }
  pr <- change_store_prune(store_dir, unique(store_keep), dry_run = dry_run)
  message(sprintf("input store %s: %d files %s (%.1f MB), %d kept", store_dir, pr$removed, if (dry_run) "would be removed" else "removed",
                  pr$bytes / 1024^2, pr$kept))
}

if (dry_run) { prune_store(); message("--dry-run: nothing written"); quit(save = "no") }

# ---- write ------------------------------------------------------------------------------------------------------
to_json <- function(x, pretty = FALSE) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, na = "null", null = "null", digits = NA, pretty = pretty))
dir.create(vint_dir, recursive = TRUE, showWarnings = FALSE)
keep <- character()
# a provisional vintage (an export not yet committed, from --include-worktree) is archived too, flagged; once its core
# is committed the next run finds it in history and rewrites its header from there
for (vid in ids) {
  f <- file.path(vint_dir, paste0(vid, ".csv.gz"))
  keep <- c(keep, basename(f))
  if (!file.exists(f) || !vid %in% names(cache_hits)) { con <- gzfile(f, "w"); utils::write.csv(tables[[vid]], con, row.names = FALSE, na = ""); close(con) }
}
# drop archives no vintage claims any more (a provisional export that was replaced before it was committed)
unlink(setdiff(list.files(vint_dir, pattern = "[.]csv[.]gz$", full.names = TRUE), file.path(vint_dir, keep)))
writeLines(to_json(list(schema = "osi-posture-vintages", layer_version = cfg$layer_version, vintages = unname(vintages)), pretty = TRUE),
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
# (counted over pairs that changed something: a pair with no change costs nothing and rides along)
comp <- which(vapply(results, function(r) isTRUE(r$pair$comparable), logical(1)))
comp_nz <- comp[vapply(results[comp], function(r) (r$pair$counts$meaningful %||% 0L) > 0, logical(1))]
detail <- if (length(comp_nz)) comp[comp >= min(utils::tail(comp_nz, cfg$bundle$detail_pairs %||% 2L))] else comp
bres <- lapply(seq_along(results), function(i) { r <- results[[i]]; r$pair$detail <- i %in% detail; if (!i %in% detail) r$changes <- r$changes[0, ]; r })
bundle <- change_bundle(unname(vintages), bres, impacts = impacts, sources = cfg$sources, created = created, git_commit = head_sha)
txt <- to_json(bundle)
dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)
unlink(list.files(bundle_dir, pattern = "^changes\\..*\\.json$", full.names = TRUE))
file <- sprintf("changes.%s.json", md5_8(txt))
writeLines(txt, file.path(bundle_dir, file), useBytes = TRUE)
# the assessment index for NEIS Intelligence (its rule-based impact proposer): the latest vintage's assessed cell ids
# and the taxonomy crosswalk, so an event's tags can be matched to assessments without NEIS keeping its own copy
latest_tbl <- tables[[utils::tail(ids, 1)]]
assessed <- latest_tbl[dplyr::coalesce(latest_tbl$choice, "not_assessed") != "not_assessed", , drop = FALSE]
crosswalk <- yaml::read_yaml(file.path(repo_root, "config", "neis_intelligence_crosswalk.yml"))
ax_txt <- to_json(list(schema = "osi-aib-assessments", schema_version = 1L, created = created, vintage = utils::tail(ids, 1),
                       id_prefix = cfg$assessment_id_prefix %||% "aib:posture",
                       crosswalk = crosswalk[c("geography", "sector", "value_chain_node")],
                       assessed = I(sort(assessment_id(assessed$iso3, assessed$tech, assessed$stage, cfg$assessment_id_prefix %||% "aib:posture")))))
unlink(list.files(bundle_dir, pattern = "^assessments\\..*\\.json$", full.names = TRUE))
ax_file <- sprintf("assessments.%s.json", md5_8(ax_txt))
writeLines(ax_txt, file.path(bundle_dir, ax_file), useBytes = TRUE)
writeLines(to_json(list(schema = CHANGE_SCHEMA, schema_version = CHANGE_SCHEMA_VERSION, created = created, git_commit = head_sha,
                        changes = file, assessments = ax_file, latest = utils::tail(ids, 1), n_vintages = length(ids),
                        n_changes = nrow(all_changes)), pretty = TRUE),
           file.path(bundle_dir, "manifest.json"), useBytes = TRUE)
message(sprintf("wrote %d vintages to %s, %d change records, bundle %s (%.0f KB)", length(keep), vint_dir, nrow(all_changes), file, nchar(txt, "bytes") / 1024))
prune_store()
