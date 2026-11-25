# QC: build end-to-end, source-to-conclusion audits of Country x Technology x Stage cells (docs/QC_SYSTEM_DESIGN.md
# 6.4; R/qc/qc_audit.R). Reads the pipeline tables, the configuration, the raw-inputs manifest, the canonical
# vocabulary and the shipped posture bundle, and writes <out>/<ISO3>_<Tech>_<Stage>.json and .md for each cell
# (plus <CASE_###>.json / .md when the cell is a posture_cell case of the case catalogue).
#
# Usage:
#   Rscript scripts/qc/build_case_audit.R [--processed-dir <dir>]
#     (--case CASE_001[,CASE_004] [--cases-file config/review/review_cases.yml]
#      | --iso3 KOR --tech "Batteries" --stage Components
#      | --cells "KOR/Batteries/Components;USA/Batteries/Refining"
#      | --all-posture-cases)
#     [--bundle-dir dashboard/public/data] [--out qc/audits] [--sensitivity <long sensitivity csv>]
#     [--sp-config config/strategic_position.yml] [--manifest config/raw_inputs_manifest.yml] [--date 2026-11-20T00:00:00Z]
# --processed-dir defaults to processed_dir of config/config.yml (else config/config.example.yml). --sensitivity
# defaults to the untrimmed qc/private/qc_sensitivity_pipeline_full.csv that scripts/qc/run_sensitivity.R writes (a
# class row for every cell), else the committed qc/results/qc_sensitivity_pipeline.csv (fragile and sensitive cells
# only: a stable or not-testable cell then gets its class from qc/results/pipeline_cell_classes.csv).

local({
  fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  d <- if (length(fa)) dirname(normalizePath(sub("^--file=", "", fa[1]), winslash = "/")) else getwd()
  while (!file.exists(file.path(d, "R", "strategic_position", "decision_engine.R")) && dirname(d) != d) d <- dirname(d)
  assign("qc_repo_root", d, envir = globalenv())
})
suppressPackageStartupMessages(library(dplyr))
for (f in c("R/utils/supply_chain_stages.R", "R/strategic_position/sp_helpers.R", "R/strategic_position/industrial_opportunity.R",
            "R/strategic_position/strategic_value.R", "R/strategic_position/decision_engine.R", "R/strategic_position/yard_rollup.R",
            "R/qc/qc_audit.R", "R/qc/qc_report.R")) {
  source(file.path(qc_repo_root, f))
}

args <- qc_report_cli_args(commandArgs(trailingOnly = TRUE))
rel <- function(p) if (is.null(p)) NULL else if (grepl("^([A-Za-z]:)?[/\\\\]", p)) p else file.path(qc_repo_root, p)
conf_path <- file.path(qc_repo_root, "config", c("config.yml", "config.example.yml"))
conf_path <- conf_path[file.exists(conf_path)][1]
conf <- if (is.na(conf_path)) list() else yaml::read_yaml(conf_path)
processed_dir <- rel(args[["processed-dir"]] %||% qc_report_processed_default(conf))
out_dir <- rel(args$out %||% "qc/audits")
bundle_dir <- rel(args[["bundle-dir"]] %||% "dashboard/public/data")
sp_cfg_path <- args[["sp-config"]] %||% "config/strategic_position.yml"
manifest_path <- args$manifest %||% "config/raw_inputs_manifest.yml"
cases_path <- rel(args[["cases-file"]] %||% "config/review/review_cases.yml")
sens_path <- rel(args$sensitivity %||% {
  full <- "qc/private/qc_sensitivity_pipeline_full.csv"
  if (file.exists(rel(full))) full else "qc/results/qc_sensitivity_pipeline.csv"
})
cell_classes_path <- rel("qc/results/pipeline_cell_classes.csv")
generated_at <- args$date %||% format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# ---- which cells ----
cases <- if (file.exists(cases_path)) yaml::read_yaml(cases_path)$cases else list()
posture_cases <- Filter(function(cs) identical(cs$kind, "posture_cell"), cases)
case_of <- function(iso3, tech, stage) {
  hit <- Filter(function(cs) identical(cs$key$iso3, iso3) && identical(cs$key$tech, tech) && identical(cs$key$stage, stage), posture_cases)
  if (length(hit)) hit[[1]] else NULL
}
targets <- list()
if (!is.null(args$case)) {
  for (id in trimws(strsplit(args$case, ",", fixed = TRUE)[[1]])) {
    cs <- Filter(function(x) identical(x$case_id, id), cases)
    if (!length(cs)) stop("build_case_audit.R: case ", id, " not in ", cases_path, call. = FALSE)
    if (!identical(cs[[1]]$kind, "posture_cell")) stop("build_case_audit.R: case ", id, " is a ", cs[[1]]$kind, ", not a posture_cell", call. = FALSE)
    targets[[length(targets) + 1]] <- list(iso3 = cs[[1]]$key$iso3, tech = cs[[1]]$key$tech, stage = cs[[1]]$key$stage)
  }
}
if (isTRUE(args[["all-posture-cases"]])) {
  for (cs in posture_cases) targets[[length(targets) + 1]] <- list(iso3 = cs$key$iso3, tech = cs$key$tech, stage = cs$key$stage)
}
if (!is.null(args$iso3)) targets[[length(targets) + 1]] <- list(iso3 = args$iso3, tech = args$tech, stage = args$stage)
if (!is.null(args$cells)) {
  for (cl in trimws(strsplit(args$cells, ";", fixed = TRUE)[[1]])) {
    p <- trimws(strsplit(cl, "/", fixed = TRUE)[[1]])
    targets[[length(targets) + 1]] <- list(iso3 = p[1], tech = p[2], stage = p[3])
  }
}
targets <- targets[!duplicated(vapply(targets, function(t) paste(t$iso3, t$tech, t$stage, sep = "|"), character(1)))]
if (!length(targets)) stop("build_case_audit.R: give --case, --iso3/--tech/--stage, --cells or --all-posture-cases", call. = FALSE)

# ---- inputs ----
tbl_path <- c(file.path(processed_dir, "outputs", "strategic_position_tbl.rds"), file.path(processed_dir, "strategic_position_tbl.rds"))
tbl_path <- tbl_path[file.exists(tbl_path)][1]
if (is.na(tbl_path)) stop("build_case_audit.R: strategic_position_tbl.rds not found under ", processed_dir, call. = FALSE)
tbl <- as.data.frame(readRDS(tbl_path), stringsAsFactors = FALSE)
release <- paste0("pipeline-", substr(unname(tools::md5sum(tbl_path)), 1, 12))
cfg <- yaml::read_yaml(rel(sp_cfg_path))
manifest <- yaml::read_yaml(rel(manifest_path))
labels <- qc_audit_labels(readLines(rel("dashboard/src/content/vocabulary.ts"), warn = FALSE, encoding = "UTF-8"),
                          readLines(rel("dashboard/src/posture/grammar.ts"), warn = FALSE, encoding = "UTF-8"))
rd <- function(name) {
  p <- file.path(processed_dir, name)
  if (file.exists(p)) as.data.frame(readRDS(p), stringsAsFactors = FALSE) else NULL
}
src_tabs <- list(import_exposure = rd("import_exposure_tbl.rds"), current_import_exposure = rd("current_import_exposure_tbl.rds"),
                 import_partners = rd("import_partner_cells_tbl.rds"), critmin = rd("critical_minerals_production_tbl.rds"),
                 manuf = rd("market_share_manufacturing_tbl.rds"), deployment_need = rd("deployment_need_tbl.rds"))
sens <- if (file.exists(sens_path)) utils::read.csv(sens_path, stringsAsFactors = FALSE, na.strings = "") else NULL
if (!is.null(sens)) sens <- sens[sens$row_type == "cell_class", , drop = FALSE]
# a trimmed long table has no row for a stable / not-testable cell: take its class from the per-cell file
cell_classes <- if (file.exists(cell_classes_path)) utils::read.csv(cell_classes_path, stringsAsFactors = FALSE, na.strings = "") else NULL

pmf_path <- file.path(bundle_dir, "posture", "manifest.json")
pmf <- if (file.exists(pmf_path)) jsonlite::fromJSON(pmf_path, simplifyVector = FALSE) else NULL
core <- if (!is.null(pmf)) jsonlite::fromJSON(file.path(bundle_dir, "posture", pmf$core), simplifyVector = TRUE) else NULL

filt <- function(d, iso_col, iso3, tech, stage) {
  if (is.null(d) || !all(c(iso_col, "tech", "supply_chain") %in% names(d))) return(NULL)
  d[d[[iso_col]] %in% iso3 & d$tech %in% tech & d$supply_chain %in% stage, , drop = FALSE]
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
written <- character()
for (t in targets) {
  row <- tbl[tbl$iso3 == t$iso3 & tbl$tech == t$tech & tbl$supply_chain == t$stage, , drop = FALSE]
  if (nrow(row) != 1) {
    message("No unique row for ", t$iso3, " / ", t$tech, " / ", t$stage, ": skipped")
    next
  }
  peers <- tbl[tbl$tech == t$tech & tbl$supply_chain == t$stage,
               intersect(c("iso3", "supply_chain_need_score", "import_hhi", "china_share", "opposite_bloc_share", "sn_import_material",
                           "competitive_position_score", "home_share", "home_share_exposure"), names(tbl)), drop = FALSE]
  prod <- rbind(
    if (!is.null(x <- filt(src_tabs$critmin, "iso3c", t$iso3, t$tech, t$stage))) x[, intersect(c("variable", "data_type", "value", "Year", "source"), names(x)), drop = FALSE],
    if (!is.null(x <- filt(src_tabs$manuf, "iso3c", t$iso3, t$tech, t$stage))) x[, intersect(c("variable", "data_type", "value", "Year", "source"), names(x)), drop = FALSE])
  ip <- filt(src_tabs$import_partners, "reporter_iso", t$iso3, t$tech, t$stage)
  dn <- filt(src_tabs$deployment_need, "iso3", t$iso3, t$tech, t$stage)
  if (!is.null(dn)) dn <- dn[, intersect(c("deployment_need", "dn_families_observed", "dn_confidence", "dn_demand", "dn_policy_gap", "dn_system",
                                           "dn_demand_source", "dn_policy_gap_source", "dn_system_source"), names(dn)), drop = FALSE]
  sources <- list(
    import_exposure = filt(src_tabs$import_exposure, "importer_iso", t$iso3, t$tech, t$stage),
    current_import_exposure = filt(src_tabs$current_import_exposure, "importer_iso", t$iso3, t$tech, t$stage),
    import_partners = if (!is.null(ip)) ip[, c("partner_iso", "imports_usd"), drop = FALSE] else NULL,
    production = prod, deployment_need = dn)
  # the shipped bundle cell
  bundle <- NULL
  if (!is.null(pmf)) {
    dfile <- pmf$details[[t$iso3]]
    detail <- NULL
    if (!is.null(dfile)) {
      dj <- jsonlite::fromJSON(file.path(bundle_dir, "posture", dfile), simplifyVector = FALSE)
      hit <- Filter(function(cl) identical(cl$tech, t$tech) && identical(cl$stage, t$stage), dj$cells)
      if (length(hit)) detail <- hit[[1]]
    }
    core_cell <- NULL
    if (!is.null(core)) {
      cc <- core$cells
      i <- which(core$countries$iso3[cc$c + 1] == t$iso3 & unlist(core$meta$techs)[cc$t + 1] == t$tech &
                   unlist(core$meta$stages)[cc$s + 1] == t$stage)
      if (length(i) == 1) core_cell <- lapply(cc[c("p", "m", "dm", "yd", "need", "prize", "cp")], function(v) v[[i]])
    }
    bundle <- list(detail = detail, core = core_cell, created = pmf$created, git_commit = pmf$git_commit,
                   detail_file = paste0("posture/", dfile %||% "(none)"), core_file = paste0("posture/", pmf$core))
  }
  cs <- case_of(t$iso3, t$tech, t$stage)
  key <- paste(t$iso3, t$tech, t$stage, sep = "|")
  rb <- if (!is.null(sens)) sens[sens$case_key == key, , drop = FALSE] else NULL
  if ((is.null(rb) || !nrow(rb)) && !is.null(cell_classes)) {
    cc1 <- cell_classes[cell_classes$iso3 == t$iso3 & cell_classes$tech == t$tech & cell_classes$stage == t$stage, , drop = FALSE]
    if (nrow(cc1)) rb <- data.frame(case_key = key, class = cc1$class[1], local_flips = "", structural_flips = "", fence_change = cc1$fence_change[1],
                                    stringsAsFactors = FALSE)
  }
  audit <- qc_audit_cell(row, peers = peers, siv_ref = tbl$siv_score, cfg = cfg, manifest = manifest, sources = sources, bundle = bundle,
                         labels = labels, robustness = rb,
                         meta = list(case_id = cs$case_id %||% NA_character_, case_title = cs$title %||% NA_character_, generated_at = generated_at,
                                     pipeline_table = sub(paste0("^", normalizePath(processed_dir, winslash = "/", mustWork = FALSE), "/?"), "<processed-dir>/",
                                                          normalizePath(tbl_path, winslash = "/")),
                                     pipeline_release = release, sp_config = sp_cfg_path))
  audit$gaps <- I(audit$gaps)
  json <- jsonlite::toJSON(audit, auto_unbox = TRUE, pretty = TRUE, na = "null", null = "null", digits = NA, dataframe = "rows")
  md <- qc_audit_markdown(audit)
  stems <- c(qc_audit_stem(t$iso3, t$tech, t$stage), if (!is.null(cs)) cs$case_id)
  for (s in stems) {
    writeLines(json, file.path(out_dir, paste0(s, ".json")), useBytes = TRUE)
    writeLines(md, file.path(out_dir, paste0(s, ".md")), useBytes = TRUE)
    written <- c(written, s)
  }
  message(sprintf("%-40s pipeline %-12s bundle %-12s links %d: %d reproduced, %d differ, %d not reproducible; bundle fields differing %s",
                  key, audit$summary$choice_pipeline, audit$summary$choice_bundle %||% "n/a", audit$summary$n_links,
                  audit$summary$reproduced, audit$summary$differs, audit$summary$not_reproducible, audit$summary$bundle_differences))
}
message("Wrote ", length(written), " audits to ", out_dir, ": ", paste(written, collapse = ", "))
