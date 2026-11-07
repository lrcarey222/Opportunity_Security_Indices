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

# Partnership agreements, step 3 of 3 (docs/partnership_agreements.md, section 9): export the
# dashboard data for the Agreements views.
#
#   Rscript scripts/37_export_agreement_data.R [--in-dir data/processed/agreements]
#     [--dashboard-dir dashboard/public/data/agreements] [--country-manifest <file>]
#
# Inputs (--in-dir, written by scripts/36): canonical_agreements, canonical_codes, agreement_links,
#   pair_profiles, agreement_gaps, execution_gaps, agreement_delivery (.rds) and build_meta.rds.
#   Economy names: dashboard/public/data/country/manifest.json (--country-manifest), else countrycode.
# Outputs (--dashboard-dir): agreements-global.<h8>.json, agreements-<ISO3>.<h8>.json for every
#   economy with at least one agreement (its own or the EU's), and manifest.json (written last).
#   <h8> is the first 8 hex characters of the MD5 of the file. Each hashed file is written once from
#   a temporary file outside the served directory, then copied; stale hashed files of this bundle
#   are removed. JSON arrays are I()-wrapped so one-item arrays stay arrays.
# Functions: R/agreements/agr_export.R (pure).

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) default else args[[i + 1]]
}
step <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)
abs_path <- function(p) normalizePath(if (grepl("^([A-Za-z]:)?[/\\]", p)) p else file.path(repo_root, p), winslash = "/", mustWork = FALSE)

for (f in c("agr_schema.R", "agr_text_coding.R", "agr_link.R", "agr_pairs.R", "agr_export.R")) {
  source(file.path(repo_root, "R", "agreements", f))
}
cfg <- yaml::read_yaml(file.path(repo_root, "config", "partnership_agreements.yml"))

in_dir <- abs_path(arg_value("--in-dir", file.path(config$processed_dir, "agreements")))
out_dir <- abs_path(arg_value("--dashboard-dir", file.path("dashboard", "public", "data", "agreements")))
cm_path <- abs_path(arg_value("--country-manifest", file.path("dashboard", "public", "data", "country", "manifest.json")))

need_file <- function(p) { if (!file.exists(p)) stop("input not found: ", p, " (run scripts/36 first)"); p }
rd <- function(name) readRDS(need_file(file.path(in_dir, paste0(name, ".rds"))))
canonical <- rd("canonical_agreements")
codes <- rd("canonical_codes")
links <- rd("agreement_links")
pairs <- rd("pair_profiles")
gaps <- rd("agreement_gaps")
exec_gaps <- rd("execution_gaps")
delivery <- rd("agreement_delivery")
meta <- rd("build_meta")
step(sprintf("in: %d canonical agreements, %d pairs, %d gap rows", nrow(canonical), nrow(pairs), nrow(gaps)))

flags <- canonical[, c("canonical_agreement_id", "ei", "trade_depth_index")]
objs <- agr_export_agreements(canonical, codes, links, flags, delivery, cfg)
cidx <- agr_country_agreement_ids(canonical, meta$eu_members, cfg$pairs$eu_code)

to_json <- function(obj) jsonlite::toJSON(obj, auto_unbox = TRUE, na = "null", null = "null", digits = NA)
# Written once under a hashed name: hashed from a temporary copy outside the served directory (a
# rename inside it can be missed by a running dev server's file index).
write_hashed <- function(obj, stem) {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  writeLines(to_json(obj), tmp, useBytes = TRUE)
  h8 <- substr(unname(tools::md5sum(tmp)), 1, 8)
  f <- sprintf("%s.%s.json", stem, h8)
  if (!file.exists(file.path(out_dir, f))) file.copy(tmp, file.path(out_dir, f))
  f
}
gz_size <- function(path) length(memCompress(readBin(path, "raw", file.info(path)$size), "gzip"))

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
files <- list()
sizes <- data.frame(file = character(), bytes = numeric(), gz = numeric(), stringsAsFactors = FALSE)
add_size <- function(f) {
  p <- file.path(out_dir, f)
  sizes[nrow(sizes) + 1, ] <<- list(f, file.info(p)$size, gz_size(p))
}
for (iso in names(cidx)) {
  obj <- agr_export_country(iso, cidx, objs, pairs, gaps, exec_gaps, cfg)
  files[[iso]] <- write_hashed(obj, paste0("agreements-", iso))
  add_size(files[[iso]])
}
gf <- write_hashed(agr_export_global(pairs, cidx, objs), "agreements-global")
add_size(gf)

# Names: the Country manifest, then countrycode, for every code in the bundle.
codes_seen <- sort(unique(c(names(cidx), pairs$a, pairs$b, gaps$need_iso3, gaps$supplier_iso3)))
nm <- stats::setNames(rep(NA_character_, length(codes_seen)), codes_seen)
if (file.exists(cm_path)) {
  cm <- jsonlite::read_json(cm_path)
  ci <- vapply(cm$countries, function(x) as.character(x$iso3 %||% ""), "")
  cn <- vapply(cm$countries, function(x) as.character(x$name %||% ""), "")
  hit <- match(codes_seen, ci)
  nm[!is.na(hit)] <- cn[hit[!is.na(hit)]]
}
# A name equal to its code (the Country manifest names a few territories by code: COK, ESH) is missing.
miss <- is.na(nm) | !nzchar(nm) | nm == codes_seen
if (any(miss)) {
  nm[miss] <- suppressWarnings(countrycode::countrycode(codes_seen[miss], "iso3c", "country.name",
                                                        custom_match = c(EUU = "European Union", XKX = "Kosovo")))
}
nm[is.na(nm)] <- codes_seen[is.na(nm)]

git_commit <- tryCatch({
  h <- system2("git", c("-C", shQuote(repo_root), "rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE)[1]
  dirty <- system2("git", c("-C", shQuote(repo_root), "status", "--porcelain", "--untracked-files=no"), stdout = TRUE, stderr = FALSE)
  if (length(dirty)) paste0(h, "-dirty") else h
}, error = function(e) NA_character_)

src_n <- vapply(meta$sources, function(m) as.integer(m$n), 0L)
# When each source was last updated upstream: the newest source_last_updated of its records (the
# dataset release for GSP and DESTA, the access date for curated rows), else the ingest date.
src_upd <- vapply(names(meta$sources), function(id) {
  f <- file.path(in_dir, paste0("source_", id, ".rds"))
  u <- if (file.exists(f)) { r <- readRDS(f)$records; v <- r$source_last_updated[nzchar(r$source_last_updated)]; if (length(v)) max(v) else "" } else ""
  if (nzchar(u)) u else as.character(meta$sources[[id]]$ingested_on %||% "")
}, "")
# Strategic compact names (config/aib_strategic_compacts.yml) for the shared compacts of gap rows.
compact_names <- tryCatch({
  y <- yaml::read_yaml(file.path(repo_root, "config", "aib_strategic_compacts.yml"))
  found <- list()
  walk <- function(x) {
    if (is.list(x)) {
      if (!is.null(x$strategic_compact_id) && !is.null(x$strategic_compact_name)) found[[x$strategic_compact_id]] <<- x$strategic_compact_name
      for (el in x) walk(el)
    }
  }
  walk(y)
  unlist(found)
}, error = function(e) character())
caveats <- c(
  paste("There is no comprehensive global registry of non-binding MOUs, joint statements and joint initiatives.",
        "Every source here misses some of them: an empty cell means no agreement was found in these sources,",
        "not that no cooperation exists."),
  paste("Partnership depth classes are provisional rules over the coded agreements; keyword mentions in a broad",
        "joint statement never make an agreement operational."),
  paste("GSP documents run from 1992 to 2023, most before 2021; later agreements come from DESTA, the curated table",
        "and the memo evidence, which are selective."),
  paste("EU member states cooperate mostly through EU law and EU programmes, which these sources do not record as",
        "bilateral agreements between members; EU-level agreements are attributed to each member state (via the EU)."),
  paste("Agreement gaps are labelled rules over the exported components (need, capability, recommendation, UN-vote",
        "alignment, stage-by-stage coverage), not a score. Complementarity between economies in opposite UN-vote blocs",
        "is flagged, not counted.")
)
manifest <- list(
  version = 1L, created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), as_of = meta$as_of, git_commit = git_commit,
  files = list(global = gf, countries = files[order(names(files))]),
  vocab = agr_export_vocab(cfg, src_n, src_upd, compact_names),
  names = as.list(nm),
  eu_members = I(sort(as.character(unlist(meta$eu_members)))),
  counts = list(records = sum(src_n), canonical = nrow(canonical), pairs = nrow(pairs), countries = length(files),
                by_source = as.list(src_n)),
  caveats = I(caveats)
)
# Remove hashed files of this bundle that the new manifest does not name, then write the manifest.
old <- list.files(out_dir, "^agreements-([A-Z0-9]{3}|global)[.][0-9a-f]{8}[.]json$")
unlink(file.path(out_dir, setdiff(old, c(gf, unlist(files)))))
mtmp <- tempfile(fileext = ".json")
writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE, na = "null", null = "null", digits = NA, pretty = TRUE), mtmp, useBytes = TRUE)
invisible(file.copy(mtmp, file.path(out_dir, "manifest.json"), overwrite = TRUE))
unlink(mtmp)
sizes <- sizes[order(-sizes$gz), ]
step(sprintf("wrote %d economy files + global + manifest to %s; %.0f kB raw, %.0f kB gz in all; largest %s (%.0f kB raw, %.0f kB gz)",
             length(files), out_dir, sum(sizes$bytes) / 1024, sum(sizes$gz) / 1024, sizes$file[1], sizes$bytes[1] / 1024,
             sizes$gz[1] / 1024))
over <- sizes[sizes$gz > 120 * 1024 & !grepl("global", sizes$file), ]
if (nrow(over)) warning("economy files over 120 kB gz: ", paste(over$file, collapse = ", "))
saveRDS(sizes, file.path(in_dir, "export_sizes.rds"))
