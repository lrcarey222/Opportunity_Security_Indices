#!/usr/bin/env Rscript
# Pipeline-level automated data QC (docs/QC_SYSTEM_DESIGN.md 6.2). IO only: reads the processed tables, the
# shipped dashboard bundles and the provenance configs, runs the pure checks in R/qc/, and writes qc/results/.
#
# Usage:
#   Rscript scripts/qc/run_data_qc.R --processed-dir <dir> [--bundle-dir dashboard/public/data]
#       [--previous-table <strategic_position_tbl.rds>] [--out qc/results] [--raw-dir <dir>]
#       [--golden config/qc/golden_cases.yml] [--theme-tables a,b,c] [--dyads current_trade_dyads_tbl|none]
#       [--as-of YYYY-MM-DD]
# --raw-dir defaults to the raw directory beside --processed-dir (data/raw), when it exists; it is only listed
# (the staged-file check of config/raw_inputs_manifest.yml), never read.
#
# --processed-dir defaults to processed_dir of config/config.yml (else config/config.example.yml), relative to
# the repo root; relative --bundle-dir / --out / --golden paths are relative to the repo root too. Writes
# pipeline_data_qc_checks.csv, pipeline_trade_production_flags.csv, pipeline_drift_transitions.csv,
# pipeline_drift_cells.csv, pipeline_golden_case_status.csv (plus pipeline_drift_aib.csv,
# pipeline_drift_aib_compacts.csv, pipeline_drift_ranks.csv, pipeline_drift_distribution.csv) and
# pipeline_qc_summary.md. Exit status 1 only when a check row is 'fail'. Never reads a partnership outputs RDS
# (it segfaults R).

suppressPackageStartupMessages({
  library(dplyr)
})

# ---- arguments --------------------------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
arg <- function(name, default = NULL) {
  i <- which(args == paste0("--", name))
  if (length(i) && i[1] < length(args)) return(args[i[1] + 1])
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) return(sub(paste0("^--", name, "="), "", hit[1]))
  default
}
`%||%` <- function(a, b) if (is.null(a)) b else a
script_path <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
repo_root <- arg("repo-root", if (!is.na(script_path)) normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/") else getwd())
rel <- function(p) if (is.null(p) || grepl("^([A-Za-z]:|/)", p)) p else file.path(repo_root, p)

cfg_file <- if (file.exists(file.path(repo_root, "config", "config.yml"))) "config/config.yml" else "config/config.example.yml"
processed_dir <- rel(arg("processed-dir", yaml::read_yaml(file.path(repo_root, cfg_file))$processed_dir %||% "data/processed"))
bundle_dir <- rel(arg("bundle-dir", "dashboard/public/data"))
out_dir <- rel(arg("out", "qc/results"))
prev_path <- rel(arg("previous-table", NULL))
raw_dir <- rel(arg("raw-dir", NULL))
# default: the raw directory next to the processed one (data/raw beside data/processed), when it exists
if (is.null(raw_dir) && dir.exists(file.path(dirname(normalizePath(processed_dir, winslash = "/", mustWork = FALSE)), "raw"))) {
  raw_dir <- file.path(dirname(normalizePath(processed_dir, winslash = "/", mustWork = FALSE)), "raw")
}
golden_path <- rel(arg("golden", "config/qc/golden_cases.yml"))
as_of <- as.Date(arg("as-of", format(Sys.Date())))
theme_tables <- strsplit(arg("theme-tables", paste(c("critical_minerals_production_tbl", "critical_minerals_processing_tbl",
  "production_depth_momentum_tbl", "market_share_manufacturing_tbl", "major_supplier_shares_tbl", "critical_minerals_trade_tbl",
  "import_dependence_tbl", "reserves_tbl"), collapse = ",")), ",")[[1]]
dyads_name <- arg("dyads", "current_trade_dyads_tbl")
if (!dir.exists(processed_dir)) stop("processed dir not found: ", processed_dir)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (f in c("R/utils/schema.R", "R/utils/supply_chain_stages.R", "R/strategic_position/sp_helpers.R", "R/strategic_position/posture_engine.R",
            "R/strategic_position/industrial_opportunity.R", "R/strategic_position/strategic_value.R",
            "R/strategic_position/decision_engine.R", "R/strategic_position/yard_rollup.R",
            "R/qc/qc_core.R", "R/qc/qc_data_checks.R", "R/qc/qc_trade_production.R", "R/qc/qc_drift.R", "R/qc/qc_golden.R")) {
  source(file.path(repo_root, f))
}

read_rds <- function(name) {
  if (grepl("partnership", basename(name), ignore.case = TRUE)) stop("refusing to read a partnership RDS (it segfaults R): ", name)
  p <- if (file.exists(name)) name else file.path(processed_dir, paste0(name, ".rds"))
  if (!file.exists(p)) return(NULL)
  as.data.frame(dplyr::ungroup(readRDS(p)))
}
file_md5 <- function(p) if (file.exists(p)) unname(tools::md5sum(p)) else NA_character_
read_json <- function(p) jsonlite::fromJSON(p, simplifyVector = TRUE)
sha256 <- function(x, file = FALSE) {
  if (!requireNamespace("digest", quietly = TRUE)) return(NA_character_)
  if (file) digest::digest(file = x, algo = "sha256") else digest::digest(x, algo = "sha256", serialize = FALSE)
}

sp_cfg_all <- yaml::read_yaml(file.path(repo_root, "config", "strategic_position.yml"))
df_cfg <- sp_cfg_all$decision_framework
checks <- list()
add <- function(x) checks[[length(checks) + 1L]] <<- x

# ---- identity ---------------------------------------------------------------------------------------------
sp_path <- file.path(processed_dir, "outputs", "strategic_position_tbl.rds")
if (!file.exists(sp_path)) stop("strategic_position_tbl.rds not found under ", processed_dir, "/outputs")
sp <- read_rds(sp_path)
sp_mtime_t <- file.info(sp_path)$mtime
utc <- function(t) format(t, "%Y-%m-%d %H:%M UTC", tz = "UTC")
sp_mtime <- utc(sp_mtime_t)
parse_utc <- function(x) if (is.null(x) || !length(x) || is.na(x)) as.POSIXct(NA) else as.POSIXct(sub("Z$", "", x), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
stamps <- list()
mf <- function(sub) { p <- file.path(bundle_dir, sub, "manifest.json"); if (file.exists(p)) read_json(p) else NULL }
pm <- mf("posture"); am <- mf("aib")
if (dir.exists(bundle_dir)) {
  cm <- mf("country"); sm <- mf("sector")
  st <- if (file.exists(file.path(bundle_dir, "manifest.json"))) read_json(file.path(bundle_dir, "manifest.json")) else NULL
  memo_p <- file.path(bundle_dir, "partnerships", "manifest.json")
  stamps <- list(posture = list(created = pm$created, git_commit = pm$git_commit), country = list(created = cm$created, git_commit = cm$git_commit),
                 sector = list(bundle_id = sm$bundle_id), aib = list(created = am$created), strategic = list(bundle_id = st$bundle_id),
                 # LF-normalised, as the browser and the Node review-data build hash it (qc_memos_manifest_text)
                 memos = list(manifest_sha256 = if (file.exists(memo_p)) {
                   sha256(qc_memos_manifest_text(rawToChar(readBin(memo_p, "raw", file.info(memo_p)$size))))
                 } else NULL))
}
release_key <- qc_release_key(stamps)
release_id <- paste0("rel-", substr(sha256(release_key), 1, 12))
add(qc_check("run_identity", "identity", "run", "info", nrow(sp),
             # the file name only: committed results must not carry a machine-specific absolute path
             sprintf("pipeline table <processed-dir>/outputs/%s (modified %s, md5 %s); shipped bundle release %s (%s); as of %s", basename(sp_path), sp_mtime,
                     file_md5(sp_path), release_id, release_key, format(as_of))))

# ---- schema, missingness, numeric validity: strategic position table --------------------------------------
cat("strategic_position_tbl:", nrow(sp), "rows\n")
prev <- if (!is.null(prev_path)) read_rds(prev_path) else NULL
add(qc_try(qc_schema_sp(sp), "schema_sp", "schema", "strategic_position_tbl"))
add(qc_try(qc_missingness_sp(sp, previous = prev), "missing_sp", "missingness", "strategic_position_tbl"))
add(qc_try(qc_numeric_validity(sp, "strategic_position_tbl"), "numeric_sp", "numeric_validity", "strategic_position_tbl"))
add(qc_try(qc_importer_shares(sp), "trade_shares_sp", "trade_consistency", "strategic_position_tbl"))

# ---- classification logic -----------------------------------------------------------------------------------
add(qc_try(qc_choice_reproduction(sp, df_cfg), "class_choice_reproduced", "classification", "strategic_position_tbl"))
add(qc_try(qc_excluded_contenders(sp), "class_excluded_contenders", "classification", "strategic_position_tbl"))
add(qc_try(qc_posture_invariants_sp(sp), "class_invariants", "classification", "strategic_position_tbl"))
add(qc_try(qc_threshold_boundaries(sp, df_cfg), "class_thresholds", "classification", "strategic_position_tbl"))
add(qc_try(qc_need_cap(sp, df_cfg), "class_need_floor_capped", "classification", "strategic_position_tbl"))

# ---- theme tables -------------------------------------------------------------------------------------------
tp_names <- c("critical_minerals_production_tbl", "production_depth_momentum_tbl", "market_share_manufacturing_tbl",
              "major_supplier_shares_tbl", "critical_minerals_processing_tbl")
tp_tables <- list(sp = sp)
for (nm in unique(c(theme_tables, tp_names))) {
  x <- read_rds(nm)
  if (is.null(x)) { add(qc_check("theme_table_missing", "schema", nm, "warn", 1L, "theme table not found in the processed dir")); next }
  if (nm %in% theme_tables) {
    cat(nm, ":", nrow(x), "rows\n")
    add(qc_try(qc_schema_theme(x, nm), "schema_theme", "schema", nm))
    add(qc_try(qc_missingness_theme(x, nm), "missing_theme", "missingness", nm))
    add(qc_try(qc_numeric_theme(x, nm), "numeric_theme", "numeric_validity", nm))
    add(qc_try(qc_units(x, nm), "trade_units", "trade_consistency", nm))
  }
  if (nm %in% tp_names) tp_tables[[nm]] <- x
  rm(x); invisible(gc(verbose = FALSE))
}
cmfdi <- read_rds("cm_fdi_projects_tbl")
if (!is.null(cmfdi)) {
  tp_tables$cm_fdi_projects_tbl <- cmfdi
  add(qc_try(qc_schema_theme(cmfdi, "cm_fdi_projects_tbl", key = intersect(c("project_id", "id"), names(cmfdi))),
             "schema_theme", "schema", "cm_fdi_projects_tbl"))
}

# ---- trade consistency: dyads and the HS6 basket ------------------------------------------------------------
if (!identical(dyads_name, "none")) {
  dy <- read_rds(dyads_name)
  if (!is.null(dy)) { add(qc_try(qc_trade_dyads(dy, dyads_name), "trade_dyads", "trade_consistency", dyads_name)); rm(dy); invisible(gc(verbose = FALSE)) }
}
hs6_path <- file.path(repo_root, "data", "reference", "energy_hs6_master.csv")
if (file.exists(hs6_path)) add(qc_try(qc_hs6_crosswalk(utils::read.csv(hs6_path, colClasses = c(hs6 = "character"))), "hs6", "trade_consistency", "energy_hs6_master"))

# ---- provenance -----------------------------------------------------------------------------------------------
man <- yaml::read_yaml(file.path(repo_root, "config", "raw_inputs_manifest.yml"))
dict <- yaml::read_yaml(file.path(repo_root, "config", "sector_dictionary.yml"))
repo_files <- list.files(repo_root, recursive = TRUE, pattern = "\\.R$", full.names = FALSE)
repo_files <- repo_files[grepl("^(scripts|R)/", repo_files)]
raw_files <- if (!is.null(raw_dir) && dir.exists(raw_dir)) list.files(raw_dir, recursive = TRUE, include.dirs = TRUE, all.files = FALSE) else NULL
add(qc_try(qc_provenance(man$inputs, dict$sources, repo_files = repo_files, raw_files = raw_files, as_of = as_of), "prov", "provenance", "raw_inputs_manifest"))

# ---- trade vs production ------------------------------------------------------------------------------------
ev <- qc_production_evidence(tp_tables, as_of_year = as.integer(format(as_of, "%Y")))
tsn <- qc_trade_share_nodes(tp_tables$major_supplier_shares_tbl)
flags <- qc_trade_production(sp, ev, cfg = list(cp_high = df_cfg$cp_preserve_pct %||% 0.75), trade_share_nodes = tsn)
add(qc_trade_production_checks(flags))
rm(tp_tables, ev); invisible(gc(verbose = FALSE))

# ---- drift --------------------------------------------------------------------------------------------------
std_cur <- qc_sp_standardize(sp)
th <- list(need = df_cfg$need_threshold, prize = df_cfg$prize_threshold, cp = df_cfg$cp_preserve_pct, deployment = df_cfg$deployment_threshold)
trans_all <- list(); cells_all <- list(); ranks_all <- list(); dist_all <- list()
record <- function(j, label, aib = NULL, aib_label = label) {
  cells <- qc_drift_cells(j, th)
  dist <- qc_drift_distribution(j)
  ranks <- qc_drift_ranks(j)
  add(qc_drift_checks(j, cells, dist, ranks, aib = aib, label = label, aib_label = aib_label))
  trans_all[[label]] <<- dplyr::mutate(qc_drift_transitions(j), comparison = label)
  cells_all[[label]] <<- dplyr::mutate(cells, comparison = label)
  ranks_all[[label]] <<- dplyr::mutate(ranks, comparison = label)
  dist_all[[label]] <<- dplyr::mutate(dist, comparison = label)
  invisible(list(cells = cells, j = j))
}
alignment <- list()
bstd <- NULL
if (!is.null(pm)) {
  core <- read_json(file.path(bundle_dir, "posture", pm$core))
  bstd <- qc_posture_core_to_table(core)
  rm(core); invisible(gc(verbose = FALSE))
  lab <- sprintf("posture bundle %s (%s) -> pipeline %s", substr(pm$created, 1, 10), pm$git_commit, substr(sp_mtime, 1, 10))
  rp <- record(qc_drift_join(bstd, std_cur), lab)
  # reproduced: no choice changes, every cell on both sides, and no input further than the bundle's 3-decimal rounding
  dcols <- intersect(paste0("d_", QC_DRIFT_INPUTS), names(rp$j))
  maxd <- suppressWarnings(max(abs(unlist(rp$j[rp$j$presence == "both", dcols])), na.rm = TRUE))
  odd <- sum(rp$j$presence != "both" & !qc_drift_expected_absence(rp$j))
  alignment$posture <- data.frame(component = "posture", bundle_created = parse_utc(pm$created), pipeline_time = sp_mtime_t,
                                  reproduced = sum(rp$cells$choice_changed) == 0 && odd == 0 && maxd <= 5e-4 + 1e-9,
                                  detail = sprintf("%d choice changes, %d cells on one side only, max input difference %.4g", sum(rp$cells$choice_changed), odd, maxd))
}
if (!is.null(prev)) {
  lab <- sprintf("previous table %s -> pipeline %s", basename(prev_path), substr(sp_mtime, 1, 10))
  record(qc_drift_join(qc_sp_standardize(prev), std_cur), lab)
}

# AIB: the pipeline's node classification and compacts against the shipped AIB bundle, oldest first.
aib_dir <- file.path(processed_dir, "outputs", "aib")
aib_csv <- file.path(aib_dir, "aib_node_classification.csv")
aib_nodes_cur <- NULL; aib_nodes_bundle <- NULL; aib_drift <- NULL; aib_label <- NULL
if (file.exists(aib_csv)) {
  z <- utils::read.csv(aib_csv, stringsAsFactors = FALSE, check.names = FALSE)
  aib_nodes_cur <- data.frame(node_id = z$node_id, class = z$node_classification,
                              z[, intersect(unname(QC_GOLDEN_AIB_FACTS), names(z)), drop = FALSE], check.names = FALSE)
  aib_label_map <- stats::setNames(z$node_id, z$sub_sector)
}
if (!is.null(am) && !is.null(am$files$compacts)) {
  comp <- jsonlite::fromJSON(file.path(bundle_dir, "aib", am$files$compacts), simplifyVector = TRUE)
  bn <- comp$nodes
  pick <- function(nm) if (nm %in% names(bn)) bn[[nm]] else rep(NA_real_, nrow(bn))
  aib_nodes_bundle <- data.frame(node_id = bn$id, class = bn$cls, shock_exposure = pick("exposure"), static_coverage = pick("static"),
                                 existing_coverage = pick("existing"), optimized_coverage = pick("optimized"),
                                 coordination_dividend = pick("dividend"), residual_gap = pick("residual"),
                                 optimized_largest_share = pick("largestShare"), optimized_n1_reoptimized_coverage = pick("n1Reoptimized"),
                                 optimized_diversion = pick("diversion"))
  if (!is.null(aib_nodes_cur)) {
    bc <- data.frame(compact_id = comp$compacts$id)
    bc$nodes <- comp$compacts$nodes
    bc$members <- comp$compacts$coalition$members
    cc_csv <- file.path(aib_dir, "aib_compact_candidates.csv")
    cc <- NULL
    if (file.exists(cc_csv)) {
      zc <- utils::read.csv(cc_csv, stringsAsFactors = FALSE)
      labs <- strsplit(zc$included_subsectors, "; ", fixed = TRUE)
      cc <- data.frame(compact_id = zc$compact_id)
      cc$nodes <- lapply(labs, function(l) ifelse(l %in% names(aib_label_map), aib_label_map[l], l))
      cc$members <- zc$minimum_coalition
    }
    csv_date <- format(file.info(aib_csv)$mtime, "%Y-%m-%d", tz = "UTC")
    b_date <- substr(am$created, 1, 10)
    csv_nodes <- aib_nodes_cur[, c("node_id", "class")]
    b_nodes <- aib_nodes_bundle[, c("node_id", "class")]
    if (csv_date <= b_date) {
      aib_label <- sprintf("pipeline outputs/aib %s -> AIB bundle %s", csv_date, b_date)
      aib_drift <- qc_drift_aib(csv_nodes, b_nodes, cc, if (!is.null(cc)) bc else NULL)
    } else {
      aib_label <- sprintf("AIB bundle %s -> pipeline outputs/aib %s", b_date, csv_date)
      aib_drift <- qc_drift_aib(b_nodes, csv_nodes, if (!is.null(cc)) bc else NULL, cc)
    }
    add(qc_drift_aib_checks(aib_drift, aib_label))
    alignment$aib <- data.frame(component = "aib", bundle_created = parse_utc(am$created), pipeline_time = file.info(aib_csv)$mtime,
                                reproduced = !any(aib_drift$nodes$changed),
                                detail = sprintf("outputs/aib/aib_node_classification.csv vs %s: %d of %d node classes differ", am$files$compacts,
                                                 sum(aib_drift$nodes$changed), nrow(aib_drift$nodes)))
  }
  rm(comp); invisible(gc(verbose = FALSE))
}

if (length(alignment)) add(qc_release_alignment(do.call(rbind, alignment)))

# ---- golden cases -------------------------------------------------------------------------------------------
golden <- if (file.exists(golden_path)) yaml::read_yaml(golden_path) else NULL
aib_csv_label <- if (file.exists(aib_csv)) sprintf("pipeline outputs/aib %s", utc(file.info(aib_csv)$mtime)) else "no AIB node table"
gres <- qc_golden_eval(golden, std_cur, raw = sp, aib_nodes = aib_nodes_cur,
                       table_label = sprintf("pipeline strategic_position_tbl %s", sp_mtime), aib_label = aib_csv_label)
if (!is.null(bstd)) {
  gb <- qc_golden_eval(golden, bstd, raw = NULL, aib_nodes = aib_nodes_bundle,
                       table_label = sprintf("posture bundle %s (%s)", substr(pm$created, 1, 10), pm$git_commit),
                       aib_label = if (!is.null(am)) sprintf("AIB bundle %s", substr(am$created, 1, 10)) else "no AIB bundle")
  gres <- dplyr::bind_rows(gres, gb)
}
add(qc_golden_checks(gres))

# ---- write ----------------------------------------------------------------------------------------------------
all_checks <- qc_bind(checks)
wcsv <- function(x, name) utils::write.csv(x, file.path(out_dir, name), row.names = FALSE, na = "")
wcsv(all_checks, "pipeline_data_qc_checks.csv")
wcsv(flags, "pipeline_trade_production_flags.csv")
wcsv(dplyr::bind_rows(trans_all), "pipeline_drift_transitions.csv")
cells_out <- dplyr::bind_rows(cells_all)
wcsv(cells_out, "pipeline_drift_cells.csv")
wcsv(dplyr::bind_rows(ranks_all), "pipeline_drift_ranks.csv")
wcsv(dplyr::bind_rows(dist_all), "pipeline_drift_distribution.csv")
if (!is.null(aib_drift)) {
  wcsv(dplyr::mutate(aib_drift$nodes, comparison = aib_label), "pipeline_drift_aib.csv")
  if (!is.null(aib_drift$compacts)) wcsv(dplyr::mutate(aib_drift$compacts, comparison = aib_label), "pipeline_drift_aib_compacts.csv")
}
wcsv(gres, "pipeline_golden_case_status.csv")

cnt <- qc_count_status(all_checks)
fails <- all_checks[all_checks$status == "fail", ]
warns <- all_checks[all_checks$status == "warn", ]
fl_cnt <- if (nrow(flags)) table(flags$flag) else integer()
md_line <- function(x) sprintf("- `%s` (%s, n=%d): %s", x$check_id, x$table, x$n, gsub("\\|", "/", x$detail))
md <- c(
  "# Pipeline data QC summary",
  "",
  sprintf("Generated %s by `scripts/qc/run_data_qc.R`. Layer: **pipeline** (processed tables), not the shipped bundle.", format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
  "",
  sprintf("- Pipeline table: `outputs/strategic_position_tbl.rds` modified %s, md5 `%s`, %d rows", sp_mtime, file_md5(sp_path), nrow(sp)),
  sprintf("- Shipped bundle release compared against: `%s` (`%s`)", release_id, release_key),
  sprintf("- Checks: %d fail, %d warn, %d info", cnt[["fail"]], cnt[["warn"]], cnt[["info"]]),
  "",
  "Passing checks are not evidence the analysis is right; they only say these defects were not found.",
  "",
  "## Release alignment (can each shipped bundle be traced to these processed outputs?)",
  { ra <- all_checks[grepl("^release_alignment_", all_checks$check_id), ]; if (nrow(ra)) sprintf("- [%s] %s", ra$status, ra$detail) else "- not checked" },
  "",
  "## Fails",
  if (nrow(fails)) md_line(fails) else "- none",
  "",
  "## Warnings needing a human explanation",
  { w <- warns[warns$needs_human_explanation, ]; if (nrow(w)) md_line(w) else "- none" },
  "",
  "## Trade vs production flags (`pipeline_trade_production_flags.csv`)",
  if (length(fl_cnt)) unlist(lapply(names(fl_cnt), function(f) {
    x <- flags[flags$flag == f, ]
    x <- x[order(-(x$strategic_choice %in% c("preserve", "catch_up", "secure")), -dplyr::coalesce(as.numeric(x$trade_benchmark_share), 0)), ]
    c(sprintf("- %s: %d", f, nrow(x)),
      sprintf("  - %s %s/%s (%s): %s", x$iso3, x$tech, x$supply_chain, dplyr::coalesce(x$strategic_choice, "excluded"), x$detail)[seq_len(min(5, nrow(x)))])
  })) else "- none",
  "",
  "## Drift (`pipeline_drift_*.csv`)",
  if (length(trans_all)) unlist(lapply(names(trans_all), function(l) {
    tr <- trans_all[[l]]
    ch <- tr[tr$changed, ]
    c(sprintf("- %s: %d cells change choice", l, sum(ch$n[!grepl("^\\(", ch$from) & !grepl("^\\(", ch$to)])),
      if (nrow(ch)) sprintf("  - %s -> %s: %d", ch$from, ch$to, ch$n)[seq_len(min(12, nrow(ch)))])
  })) else "- no comparison run",
  if (!is.null(aib_drift)) sprintf("- AIB (%s): %d of %d node classes change; compacts matched on node set: %d membership changes, %d node sets on one side only; %d compact IDs reused for a different node set",
                                   aib_label, sum(aib_drift$nodes$changed), nrow(aib_drift$nodes),
                                   sum(aib_drift$compacts$presence == "both" & aib_drift$compacts$changed),
                                   sum(aib_drift$compacts$presence != "both"), length(aib_drift$id_reused)) else NULL,
  "",
  "## Golden cases (`pipeline_golden_case_status.csv`)",
  "A fail on a table other than the release a case was pinned to is drift evidence between the two (see Release alignment for which is newer); a fail on the pinned release means the case is out of date or wrong.",
  "",
  if (nrow(gres)) unlist(lapply(unique(gres$evaluated_against), function(a) {
    g <- gres[gres$evaluated_against == a, ]
    sprintf("- %s: %s", a, paste(names(table(g$status)), table(g$status), sep = "=", collapse = ", "))
  })) else "- config/qc/golden_cases.yml absent or empty"
)
writeLines(md, file.path(out_dir, "pipeline_qc_summary.md"))

cat(sprintf("\nQC: %d fail, %d warn, %d info -> %s\n", cnt[["fail"]], cnt[["warn"]], cnt[["info"]], out_dir))
if (nrow(fails)) cat(sprintf("FAIL %s (%s, n=%d): %s\n", fails$check_id, fails$table, fails$n, fails$detail), sep = "")
if (length(fl_cnt)) cat("trade vs production:", paste(names(fl_cnt), as.integer(fl_cnt), sep = "=", collapse = ", "), "\n")
if (nrow(cells_out)) cat("drift: ", sum(cells_out$choice_changed, na.rm = TRUE), " changed choices across comparisons\n", sep = "")
if (!is.null(aib_drift)) cat("AIB drift (", aib_label, "): ", sum(aib_drift$nodes$changed), " node classes change\n", sep = "")
if (nrow(gres)) for (a in unique(gres$evaluated_against)) {
  g <- gres[gres$evaluated_against == a, ]
  cat("golden vs ", a, ": ", paste(names(table(g$status)), table(g$status), sep = "=", collapse = ", "), "\n", sep = "")
}
quit(status = if (nrow(fails)) 1L else 0L)
