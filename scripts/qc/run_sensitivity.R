# QC: canonical (pipeline-level) sensitivity of the strategic choice, per cell (docs/QC_SYSTEM_DESIGN.md 4.6,
# 6.3). Reads strategic_position_tbl.rds from the processed outputs and the node attributes, re-runs the
# canonical choice rule under the local and structural specs (R/qc/qc_sensitivity.R) and writes
#   <results>/qc_sensitivity_pipeline.csv       cell x changed-spec rows + the class rows of the fragile and
#                                               sensitive cells (layer pipeline; qc_sens_trim, kept under ~1 MB)
#   <results>/pipeline_cell_classes.csv         one short row per cell: baseline choice, class, fence_change
#   <results>/pipeline_robustness_summary.csv   class counts (overall, tech x stage, choice, NEIS economies),
#                                               per-spec change counts and status, reproduction checks
#   <full-out>                                  the untrimmed long table (a class row per cell; default
#                                               qc/private/qc_sensitivity_pipeline_full.csv, git-ignored), which
#                                               scripts/qc/build_case_audit.R reads for every audited cell's class
#
# Usage:
#   Rscript scripts/qc/run_sensitivity.R [--processed-dir <dir>] [--out qc/results] [--full-out <csv>]
#     [--sp-config config/strategic_position.yml] [--node-attributes data/reference/posture_node_attributes.csv]
#     [--yf-components data/reference/yards_fences_assessments.csv] [--no-node-attributes]
# --processed-dir defaults to processed_dir of config/config.yml (else config/config.example.yml); the table is
# read from <processed-dir>/outputs/strategic_position_tbl.rds (or <processed-dir>/strategic_position_tbl.rds).

local({
  fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  d <- if (length(fa)) dirname(normalizePath(sub("^--file=", "", fa[1]), winslash = "/")) else getwd()
  while (!file.exists(file.path(d, "R", "strategic_position", "decision_engine.R")) && dirname(d) != d) d <- dirname(d)
  assign("qc_repo_root", d, envir = globalenv())
})
suppressPackageStartupMessages(library(dplyr))
for (f in c("R/utils/supply_chain_stages.R", "R/strategic_position/sp_helpers.R", "R/strategic_position/industrial_opportunity.R",
            "R/strategic_position/strategic_value.R", "R/strategic_position/decision_engine.R", "R/strategic_position/yard_rollup.R",
            "R/strategic_position/posture_nodes.R", "R/strategic_position/supply_chain_need.R", "R/qc/qc_sensitivity.R",
            "R/qc/qc_report.R")) {
  source(file.path(qc_repo_root, f))
}

args <- qc_report_cli_args(commandArgs(trailingOnly = TRUE))
rel <- function(p) if (is.null(p)) NULL else if (grepl("^([A-Za-z]:)?[/\\\\]", p)) p else file.path(qc_repo_root, p)
conf_path <- file.path(qc_repo_root, "config", c("config.yml", "config.example.yml"))
conf_path <- conf_path[file.exists(conf_path)][1]
conf <- if (is.na(conf_path)) list() else yaml::read_yaml(conf_path)
processed_dir <- rel(args[["processed-dir"]] %||% qc_report_processed_default(conf))
out_dir <- rel(args$out %||% "qc/results")
full_out <- rel(args[["full-out"]] %||% "qc/private/qc_sensitivity_pipeline_full.csv")
sp_cfg_path <- rel(args[["sp-config"]] %||% "config/strategic_position.yml")
tbl_path <- c(file.path(processed_dir, "outputs", "strategic_position_tbl.rds"), file.path(processed_dir, "strategic_position_tbl.rds"))
tbl_path <- tbl_path[file.exists(tbl_path)][1]
if (is.na(tbl_path)) stop("run_sensitivity.R: strategic_position_tbl.rds not found under ", processed_dir, call. = FALSE)

message("Reading ", tbl_path)
tbl <- as.data.frame(readRDS(tbl_path), stringsAsFactors = FALSE)
keep <- c("Country", "iso3", "tech", "supply_chain", "strategic_choice", "engine_posture", "posture_exclusion",
          "supply_chain_need", "supply_chain_need_uncapped", "supply_chain_need_score", "supply_chain_need_cap",
          "strategic_industrial_value", "industrial_opportunity", "siv_cap", "deployment_need", "dn_families_observed",
          "cp_contender_pct", "cp_contender_basis", "cp_is_contender", "t3_capability_lo", "pathway_credible",
          "large_yard_case", "large_yard_near", "large_yard_n", "world_top_share", "coercive_top_share",
          "sn_import_dependence", "sn_supplier_concentration", "sn_risky_supplier_exposure", "strategic_need_percentile",
          "trusted_world_share", "home_share", "sn_import_material")
tbl <- tbl[, intersect(keep, names(tbl)), drop = FALSE]
gc(verbose = FALSE)
data_release <- paste0("pipeline-", substr(unname(tools::md5sum(tbl_path)), 1, 12))
sp_cfg <- yaml::read_yaml(sp_cfg_path)
cfg <- sp_cfg$decision_framework

nodes <- NULL
if (!isTRUE(args[["no-node-attributes"]])) {
  # the node table scripts/26 reads (strategic_position.yml policy_design.node_table)
  na_path <- rel(args[["node-attributes"]] %||% sp_cfg$policy_design$node_table %||% "data/reference/posture_node_attributes.csv")
  yf_path <- rel(args[["yf-components"]] %||% "data/reference/yards_fences_assessments.csv")
  if (file.exists(na_path) && file.exists(yf_path)) {
    rd <- function(p) utils::read.csv(p, colClasses = "character", check.names = FALSE, na.strings = character())
    yf <- rd(yf_path)
    nodes <- pn_resolve(pn_read_check(rd(na_path), yf), yf, iso3 = sort(unique(tbl$iso3)))
    nodes <- nodes[nodes$tech %in% unique(tbl$tech), , drop = FALSE]
  } else {
    message("Node attributes not found: the node-attribute specs are skipped")
  }
}

message("Reproduction checks")
repro <- qc_sens_reproduction(tbl, cfg, nodes)
print(repro[, c("check", "n_compared", "n_mismatch", "max_abs_diff")])
message("Running specs")
run <- qc_sens_run(tbl, cfg, nodes)
classes <- qc_sens_classify(tbl, run, cfg)
long <- qc_sens_long(tbl, run, classes, data_release)
artifacts <- qc_sens_need_cap_artifacts(tbl, run)
summ <- qc_sens_summary(classes, run, repro, data_release = data_release, artifacts = artifacts)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
trimmed <- qc_sens_trim(long)
utils::write.csv(trimmed, file.path(out_dir, "qc_sensitivity_pipeline.csv"), row.names = FALSE, na = "")
utils::write.csv(qc_sens_cell_classes(classes), file.path(out_dir, "pipeline_cell_classes.csv"), row.names = FALSE, na = "", quote = FALSE)
utils::write.csv(summ, file.path(out_dir, "pipeline_robustness_summary.csv"), row.names = FALSE, na = "")
dir.create(dirname(full_out), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(long, full_out, row.names = FALSE, na = "")
message("Wrote ", file.path(out_dir, "qc_sensitivity_pipeline.csv"), " (", nrow(trimmed), " of ", nrow(long), " rows), pipeline_cell_classes.csv (",
        nrow(classes), " cells), pipeline_robustness_summary.csv and ", full_out)
print(table(classes$class))
print(run$specs[, c("spec_id", "kind", "status", "note")])
message("Need-cap artifacts at need 0.60 (naive flips the cap-aware reading does not make): ", nrow(artifacts))
if (nrow(artifacts)) print(artifacts[, c("case_key", "supply_chain_need", "supply_chain_need_uncapped", "supply_chain_need_score",
                                         "naive_choice_at_060")], row.names = FALSE)
