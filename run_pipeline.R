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
# NIPO mapped directly to the six-stage HS6 master: support and protection stock and flow by country x
# technology x stage (docs/strategic_position.md section 4); read by scripts/26.
source(file.path(repo_root, "scripts", "11_build_nipo_stage_policy.R"))
# Climate TRACE sector emissions mapped to technology x stage (reads tech_ghg_tbl.rds from scripts/10;
# scripts/87 reads its tables for the Sector bundle).
source(file.path(repo_root, "scripts", "16_build_climate_trace.R"))
# IEA World pathways (WEO Annex A) and mineral demand by scenario mapped to technology x stage (the
# optional climate.nze_* tables of the Sector bundle's sector-climate chunk).
source(file.path(repo_root, "scripts", "17_build_iea_pathways.R"))
# Climate sources (GCP national and fuel CO2, EI Country Transition Tracker, IEA WEO regions, NZE 2021
# milestones, World Energy Investment; config/climate_sources_map.yml): the Sector bundle's lazy
# sector-climate-src chunk. After scripts/17 (it checks the NZE 2021 pairs against its series).
source(file.path(repo_root, "scripts", "18_build_climate_sources.R"))
source(file.path(repo_root, "scripts", "12_build_geopolitical_alignment.R"))
source(file.path(repo_root, "scripts", "13_build_coalition.R"))
source(file.path(repo_root, "scripts", "20_build_indices.R"))
source(file.path(repo_root, "scripts", "15_build_partner_themes.R"))
source(file.path(repo_root, "scripts", "25_build_partner_indices.R"))
# Strategic position (Competitive Position, Strategic Need, Revealed Policy Priority), the Yards &
# Fences posture (Secure / Compete / Deploy) with its policy design, and their diagnostics; then the
# review charts and decision cards (docs/strategic_position.md). Reads scripts/20, 11 and 15 outputs.
source(file.path(repo_root, "scripts", "26_build_strategic_position.R"))
source(file.path(repo_root, "scripts", "27_review_strategic_position.R"))

# The original allied network design (scripts/30_build_allied_network_design.R and
# R/indices/allied_network_design.R) is retired (2026-10-17) and no longer runs here: the Allied
# Industrial Base analysis below replaces it. Its code is kept, dormant; it still runs by hand
# (Rscript scripts/30_build_allied_network_design.R), but nothing in the pipeline or the dashboard reads
# its outputs.

# Allied Industrial Base: trade partnership network, investment prioritization, emergent network
# and Sectoral Compacts (docs/aib_methodology.md). Reads the partner indices and the coalition table.
# scripts/33 adds the strategic layer above scripts/32's outputs (Strategic Sectoral Compacts, their
# workstreams, Strategic AIB Roles and compact-specific country roles; section 16); it reads the CM-FDI
# projects scripts/15 writes and the raw IMF DIP file, and without them leaves the capital role "not assessed".
run_aib <- tolower(Sys.getenv("OPSI_RUN_AIB", "true")) %in% c("1", "true", "yes")
if (run_aib) {
  source(file.path(repo_root, "scripts", "31_build_aib_trade_network.R"))
  source(file.path(repo_root, "scripts", "32_build_aib_partnerships.R"))
  source(file.path(repo_root, "scripts", "33_build_aib_strategic_layer.R"))
}

source(file.path(repo_root, "scripts", "80_write_outputs.R"))
