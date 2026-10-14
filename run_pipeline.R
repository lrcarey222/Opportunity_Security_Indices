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
source(file.path(repo_root, "scripts", "20_build_indices.R"))
source(file.path(repo_root, "scripts", "15_build_partner_themes.R"))
source(file.path(repo_root, "scripts", "25_build_partner_indices.R"))

run_allied <- tolower(Sys.getenv("OPSI_RUN_ALLIED_NETWORK", "true")) %in% c("1", "true", "yes")
if (run_allied) source(file.path(repo_root, "scripts", "30_build_allied_network_design.R"))

source(file.path(repo_root, "scripts", "80_write_outputs.R"))
