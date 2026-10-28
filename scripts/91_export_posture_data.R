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

# The Explorer's six-stage posture bundle (dashboard/public/data/posture): the strategic position
# framework's posture, quadrant, Secure rationale, partner recommendations, trade freshness and the
# allied division of labour (scripts/26), for the headline charts of the Country, Sector and AIB views.
# Writes manifest.json, posture-core.<md5>.json and posture-detail-<ISO3>.<md5>.json.
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

core <- posture_core_doc(tbl, division, countries, sp_cfg, created, git_commit)
core_file <- write_doc("posture-core", core)
isos <- core$countries$iso3
details <- lapply(isos, function(iso) write_doc(paste0("posture-detail-", iso), posture_detail_doc(tbl, iso, sp_cfg, created)))
manifest <- list(
  schema = POSTURE_SCHEMA, schema_version = POSTURE_SCHEMA_VERSION, created = created, git_commit = git_commit,
  core = core_file$file, core_bytes = core_file$bytes,
  details = stats::setNames(lapply(details, function(d) d$file), isos),
  counts = list(countries = length(isos), cells = length(core$cells$p), division_nodes = length(core$division$t))
)
writeLines(to_json(manifest), file.path(out_dir, "manifest.json"), useBytes = TRUE)
message("Posture bundle: ", length(isos), " countries, ", format(length(core$cells$p), big.mark = ","), " cells, core ",
        round(core_file$bytes / 1024), " kB, written to ", out_dir)
