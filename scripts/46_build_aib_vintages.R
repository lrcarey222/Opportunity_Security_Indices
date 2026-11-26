# Repo root: walk up from this script (or the working directory) to the folder holding config/change_layer.yml.
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

# Change layer for partnerships (R/change/aib_vintage.R): AIB vintages and what changed between them.
#
# 1. Walks this branch's first-parent history of the AIB bundle (dashboard/public/data/aib/manifest.json), reads each
#    committed compacts document with a strategic layer, and archives it as data/vintages/aib/<id>.json.gz (pair
#    theses, compact roles, profiles) + index.json (with the AIB method fingerprint, config aib_method_paths).
# 2. Compares each vintage with the previous one (aib_compare): pairs whose shared priority compacts grew or shrank,
#    economies joining or leaving a compact or changing role, primary-role shifts.
# 3. Writes dashboard/public/data/changes/{aib-manifest.json, aib-changes.<md5-8>.json}: every pair summary, and the
#    records of the last `bundle.detail_pairs` pairs that changed something.
#
#   --include-worktree   add the working tree's AIB bundle as a provisional vintage when it is not committed
#   --dry-run            compute and report; write nothing

library(dplyr)
for (f in c("R/change/posture_vintage.R", "R/change/aib_vintage.R", "scripts/utils/change_stamps.R")) source(file.path(repo_root, f))

args <- commandArgs(trailingOnly = TRUE)
include_worktree <- "--include-worktree" %in% args
dry_run <- "--dry-run" %in% args
cfg <- yaml::read_yaml(file.path(repo_root, "config", "change_layer.yml"))
aib_dir <- "dashboard/public/data/aib"
vint_dir <- file.path(repo_root, "data", "vintages", "aib")
bundle_dir <- file.path(repo_root, "dashboard", "public", "data", "changes")

qroot <- change_qpath(repo_root)
git <- function(...) suppressWarnings(system2("git", c("-C", qroot, ...), stdout = TRUE, stderr = FALSE))
git_to_file <- function(spec) {
  tf <- tempfile(fileext = ".json")
  status <- suppressWarnings(system2("git", c("-C", qroot, "show", spec), stdout = tf, stderr = FALSE))
  if (!identical(as.integer(status), 0L) || !file.exists(tf) || file.size(tf) == 0) { unlink(tf); return(NULL) }
  tf
}
to_json <- function(x, pretty = FALSE) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, na = "null", null = "null", digits = NA, pretty = pretty))
md5_8 <- function(txt) { tf <- tempfile(); on.exit(unlink(tf)); writeBin(charToRaw(enc2utf8(txt)), tf); substr(unname(tools::md5sum(tf)), 1, 8) }

# ---- vintages ---------------------------------------------------------------------------------------------------
log <- git("log", "--first-parent", "--reverse", "--format=%H", "--", file.path(aib_dir, "manifest.json"))
cands <- lapply(log, function(x) list(commit = x, short = substr(x, 1, 8), worktree = FALSE))
if (include_worktree) cands <- c(cands, list(list(commit = NA_character_, short = "worktree", worktree = TRUE)))

seen <- character(); headers <- list(); tables <- list()
for (cd in cands) {
  man_path <- if (cd$worktree) file.path(repo_root, aib_dir, "manifest.json") else git_to_file(paste0(cd$commit, ":", aib_dir, "/manifest.json"))
  if (is.null(man_path) || !file.exists(man_path)) next
  man <- jsonlite::fromJSON(man_path, simplifyVector = FALSE)
  doc_file <- man$files$compacts
  if (is.null(doc_file) || doc_file %in% seen) next
  doc_path <- if (cd$worktree) file.path(repo_root, aib_dir, doc_file) else git_to_file(paste0(cd$commit, ":", aib_dir, "/", doc_file))
  if (is.null(doc_path) || !file.exists(doc_path)) next
  seen <- c(seen, doc_file)
  v <- aib_vintage_from_doc(jsonlite::fromJSON(doc_path, simplifyVector = FALSE))
  if (is.null(v)) { message("skip ", cd$short, ": no strategic layer"); next }
  date <- if (cd$worktree) format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") else git("show", "-s", "--format=%cI", cd$commit)[1]
  created <- man$created %||% date
  code_commit <- if (!is.null(man$git_commit) && nzchar(man$git_commit)) sub("-dirty$", "", man$git_commit) else if (cd$worktree) NULL else cd$commit
  method <- change_method_stamp(repo_root, cfg$aib_method_paths, commit = code_commit)
  vid <- sprintf("%s-%s", substr(created, 1, 10), sub("^compacts[.]([0-9a-f]+)[.]json$", "\\1", doc_file))
  headers[[vid]] <- list(vintage_id = vid, created = created, git_commit = man$git_commit %||% cd$short, doc_file = doc_file,
                         bundle_commit = if (cd$worktree) NA_character_ else cd$short, provisional = cd$worktree,
                         method = list(id = method$id), n_pairs = nrow(v$theses), n_roles = nrow(v$roles))
  tables[[vid]] <- v
}
ord <- order(vapply(headers, function(h) h$created, ""))
headers <- headers[ord]; tables <- tables[ord]
ids <- names(headers)
for (h in headers) message(sprintf("aib vintage %s  method %s  pairs %d  roles %d%s", h$vintage_id, h$method$id, h$n_pairs, h$n_roles,
                                   if (isTRUE(h$provisional)) "  (provisional)" else ""))
if (length(ids) < 2) stop("Fewer than two AIB vintages with a strategic layer: nothing to compare.")

# ---- compare ----------------------------------------------------------------------------------------------------
results <- lapply(seq_along(ids)[-1], function(i) {
  r <- aib_compare(tables[[i - 1]], tables[[i]], headers[[i - 1]], headers[[i]])
  r$pairs$explanation <- aib_pair_explain(r$pairs, tables[[i]]$compact_names)
  r
})
for (r in results) message(sprintf("%s -> %s  method %s  pairs more %d / less %d (new %d, dropped %d)  membership %d  profiles %d", r$pair$prev, r$pair$cur,
                                   if (r$pair$method_changed) "CHANGED" else "same", r$pair$n_more, r$pair$n_less, r$pair$n_new, r$pair$n_dropped,
                                   r$pair$n_membership, r$pair$n_profiles))
if (dry_run) { message("--dry-run: nothing written"); quit(save = "no") }

# ---- write ------------------------------------------------------------------------------------------------------
dir.create(vint_dir, recursive = TRUE, showWarnings = FALSE)
keep <- character()
for (vid in ids) {
  f <- file.path(vint_dir, paste0(vid, ".json.gz"))
  keep <- c(keep, basename(f))
  v <- tables[[vid]]
  con <- gzfile(f, "w"); writeLines(to_json(list(schema = AIB_VINTAGE_SCHEMA, theses = v$theses, roles = v$roles, profiles = v$profiles)), con); close(con)
}
unlink(setdiff(list.files(vint_dir, pattern = "[.]json[.]gz$", full.names = TRUE), file.path(vint_dir, keep)))
writeLines(to_json(list(schema = "osi-aib-vintages", vintages = unname(headers)), pretty = TRUE), file.path(vint_dir, "index.json"), useBytes = TRUE)

changed <- which(vapply(results, function(r) nrow(r$pairs) + nrow(r$membership) > 0, logical(1)))
detail <- if (length(changed)) seq.int(min(utils::tail(changed, cfg$bundle$detail_pairs %||% 2L)), length(results)) else integer()
rows <- function(df) if (!nrow(df)) list() else lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
created <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
bundle <- list(
  schema = "osi-aib-changes", schema_version = 1L, created = created, git_commit = git("rev-parse", "--short", "HEAD")[1],
  vintages = unname(lapply(headers, function(h) h[c("vintage_id", "created", "git_commit", "provisional", "n_pairs", "n_roles")])),
  compact_names = tables[[utils::tail(ids, 1)]]$compact_names,
  pairs = lapply(seq_along(results), function(i) {
    r <- results[[i]]
    c(r$pair, list(detail = i %in% detail,
                   changes = if (i %in% detail) rows(r$pairs[, c("a", "b", "kind", "direction", "delta", "n_compacts_prev", "n_compacts_cur", "compacts_added",
                                                               "compacts_removed", "purposes_prev", "purposes_cur", "attribution", "explanation")]) else list(),
                   membership = if (i %in% detail) rows(r$membership) else list()))
  })
)
txt <- to_json(bundle)
dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)
unlink(list.files(bundle_dir, pattern = "^aib-changes[.].*[.]json$", full.names = TRUE))
file <- sprintf("aib-changes.%s.json", md5_8(txt))
writeLines(txt, file.path(bundle_dir, file), useBytes = TRUE)
writeLines(to_json(list(schema = "osi-aib-changes", schema_version = 1L, created = created, changes = file, latest = utils::tail(ids, 1),
                        latest_doc = headers[[utils::tail(ids, 1)]]$doc_file), pretty = TRUE),
           file.path(bundle_dir, "aib-manifest.json"), useBytes = TRUE)
message(sprintf("wrote %d AIB vintages, bundle %s (%.0f KB)", length(ids), file, nchar(txt, "bytes") / 1024))
