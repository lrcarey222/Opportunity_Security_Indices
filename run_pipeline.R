# Pipeline runner: execute scripts in order.
# Resolve paths relative to this script so Rscript can run from any working directory.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) > 0) {
  sub("^--file=", "", file_arg[1])
} else if (!is.null(sys.frame(1)$ofile)) {
  sys.frame(1)$ofile
} else {
  ""
}
if (script_path == "") {
  stop("Unable to determine run_pipeline.R path for repo-root anchoring.")
}

start_dir <- normalizePath(dirname(script_path), winslash = "/", mustWork = TRUE)
repo_root <- start_dir
while (!file.exists(file.path(repo_root, ".git")) && dirname(repo_root) != repo_root) {
  repo_root <- dirname(repo_root)
}
if (!file.exists(file.path(repo_root, ".git"))) {
  stop("Could not locate repo root (no .git found).")
}

source(file.path(repo_root, "scripts", "00_setup.R"))
source(file.path(repo_root, "scripts", "05_ingest_sources.R"))
source(file.path(repo_root, "scripts", "07_process_data.R"))
source(file.path(repo_root, "scripts", "10_build_themes.R"))
# Climate TRACE sector emissions mapped to technology x stage (reads tech_ghg_tbl.rds from scripts/10;
# scripts/87 reads its tables for the Sector bundle).
source(file.path(repo_root, "scripts", "16_build_climate_trace.R"))
source(file.path(repo_root, "scripts", "12_build_geopolitical_alignment.R"))
source(file.path(repo_root, "scripts", "13_build_coalition.R"))
source(file.path(repo_root, "scripts", "20_build_indices.R"))
source(file.path(repo_root, "scripts", "15_build_partner_themes.R"))
source(file.path(repo_root, "scripts", "25_build_partner_indices.R"))

# The original allied network design (scripts/30_build_allied_network_design.R and
# R/indices/allied_network_design.R) is retired (2026-10-17) and no longer runs here: the Allied
# Industrial Base analysis below replaces it. Its code is kept, dormant; it still runs by hand
# (Rscript scripts/30_build_allied_network_design.R), but nothing in the pipeline or the dashboard reads
# its outputs.

# Allied Industrial Base: trade partnership network, investment prioritization, emergent network
# and Sectoral Compacts (docs/aib_methodology.md). Reads the partner indices and the coalition table.
run_aib <- tolower(Sys.getenv("OPSI_RUN_AIB", "true")) %in% c("1", "true", "yes")
if (run_aib) {
  source(file.path(repo_root, "scripts", "31_build_aib_trade_network.R"))
  source(file.path(repo_root, "scripts", "32_build_aib_partnerships.R"))
}

source(file.path(repo_root, "scripts", "80_write_outputs.R"))
