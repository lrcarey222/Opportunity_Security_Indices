source(local({
  resolve_bootstrap_path <- function() {
    candidate_starts <- character()
    fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(fa) > 0) candidate_starts <- c(candidate_starts, dirname(sub("^--file=", "", fa[1])))
    sf <- tryCatch(sys.frame(1)$ofile, error = function(e) "")
    if (!is.null(sf) && nzchar(sf)) candidate_starts <- c(candidate_starts, dirname(sf))
    candidate_starts <- unique(c(candidate_starts, getwd()))
    for (start in candidate_starts) {
      d <- normalizePath(start, winslash = "/", mustWork = FALSE)
      while (dirname(d) != d) {
        bootstrap <- file.path(d, "scripts", "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)
        d <- dirname(d)
      }
    }
    stop("Unable to resolve script path for bootstrap.")
  }
  resolve_bootstrap_path()
}))

# The Explorer's six-stage posture bundle, schema 2 (dashboard/public/data/posture): the canonical
# decision of the strategic position framework (scripts/26: Need -> Prize -> Position -> Choice -> Policy
# alignment), its Secure rationale, partner recommendations and AIB status, trade freshness and the allied
# division of labour, for the Country, Sector and AIB views (dashboard/src/posture/contract.ts).
# Writes manifest.json, posture-core.<md5>.json and posture-detail-<ISO3>.<md5>.json, and stops if the core
# is over 3 MiB or a detail document over 128 KiB.
#   --out-dir DIR   default dashboard/public/data/posture

library(dplyr)
for (f in c("R/utils/supply_chain_stages.R", "R/dashboard/posture/posture_export.R")) source(file.path(repo_root, f))

config <- getOption("opportunity_security.config")
processed_dir <- file.path(repo_root, config$processed_dir)
outputs_dir <- if (!is.null(config$outputs_dir) && nzchar(config$outputs_dir)) file.path(repo_root, config$outputs_dir) else file.path(processed_dir, "outputs")
sp_cfg <- yaml::read_yaml(Sys.getenv("OPSI_STRATEGIC_POSITION", file.path(repo_root, "config", "strategic_position.yml")))

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(name, default = NULL) {
  i <- match(name, args)
  if (is.na(i) || i == length(args)) default else args[i + 1]
}
out_dir <- arg_value("--out-dir", file.path(repo_root, "dashboard", "public", "data", "posture"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

tbl <- readRDS(file.path(outputs_dir, "strategic_position_tbl.rds"))
if (!"strategic_choice" %in% names(tbl)) stop("strategic_position_tbl.rds predates the canonical choice: rerun scripts/26.")
stage_cfg <- yaml::read_yaml(file.path(repo_root, "config", "supply_chain_stages.yml"))
stage_display <- stats::setNames(vapply(stage_cfg$stages, function(s) s$label, ""),
                                 vapply(stage_cfg$stages, function(s) paste0(toupper(substr(s$id, 1, 1)), substr(s$id, 2, nchar(s$id))), ""))
stopifnot(setequal(names(stage_display), SUPPLY_CHAIN_STAGES))
division <- readRDS(file.path(outputs_dir, "strategic_aib_division_tbl.rds"))
coalition <- readRDS(file.path(processed_dir, "coalition_tbl.rds"))
countries <- data.frame(iso3 = coalition$iso3, name = coalition$country, tier = coalition$tier, stringsAsFactors = FALSE)
created <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
git_commit <- tryCatch(system2("git", c("-C", shQuote(repo_root), "rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE)[1],
                       error = function(e) NA_character_)

to_json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, na = "null", null = "null", digits = NA))
md5_8 <- function(txt) {
  tf <- tempfile()
  on.exit(unlink(tf))
  writeBin(charToRaw(enc2utf8(txt)), tf)
  substr(unname(tools::md5sum(tf)), 1, 8)
}
write_doc <- function(prefix, doc) {
  txt <- to_json(doc)
  file <- sprintf("%s.%s.json", prefix, md5_8(txt))
  writeLines(txt, file.path(out_dir, file), useBytes = TRUE)
  list(file = file, bytes = nchar(txt, type = "bytes"))
}

# Remove the previous bundle's files first (names change with their content).
old <- list.files(out_dir, pattern = "^posture-.*\\.json$", full.names = TRUE)
unlink(old)

core <- posture_core_doc(tbl, division, countries, sp_cfg, created, git_commit, stage_display = stage_display)
core_file <- write_doc("posture-core", core)
if (core_file$bytes > POSTURE_BUDGET_CORE) stop("posture core is ", core_file$bytes, " bytes, over the 3 MiB budget.")
isos <- core$countries$iso3
details <- lapply(isos, function(iso) write_doc(paste0("posture-detail-", iso), posture_detail_doc(tbl, iso, sp_cfg, created)))
detail_bytes <- vapply(details, function(d) d$bytes, numeric(1))
if (any(detail_bytes > POSTURE_BUDGET_DETAIL)) {
  stop("posture detail over the 128 KiB budget: ", paste(isos[detail_bytes > POSTURE_BUDGET_DETAIL], collapse = ", "))
}
manifest <- list(
  schema = POSTURE_SCHEMA, schema_version = POSTURE_SCHEMA_VERSION, created = created, git_commit = git_commit,
  core = core_file$file, core_bytes = core_file$bytes,
  details = stats::setNames(lapply(details, function(d) d$file), isos),
  counts = list(countries = length(isos), cells = length(core$cells$p), division_nodes = length(core$division$t),
                choices = as.list(table(factor(tbl$strategic_choice[!is.na(tbl$strategic_choice)],
                                               c("secure", "preserve", "catch_up", "partner", "accelerate", "watch", "not_assessed")))))
)
writeLines(to_json(manifest), file.path(out_dir, "manifest.json"), useBytes = TRUE)
message("Posture bundle (schema ", POSTURE_SCHEMA_VERSION, "): ", length(isos), " countries, ", format(length(core$cells$p), big.mark = ","),
        " cells, core ", round(core_file$bytes / 1024), " kB, largest detail ", round(max(detail_bytes) / 1024), " kB, written to ", out_dir)
