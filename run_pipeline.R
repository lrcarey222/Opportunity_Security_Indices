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
# Anchor sources to the repo root (this script's directory) so getwd() doesn't matter.
repo_root <- normalizePath(dirname(script_path), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "scripts", "00_setup.R"))
source(file.path(repo_root, "scripts", "05_ingest_sources.R"))
source(file.path(repo_root, "scripts", "07_process_data.R"))
source(file.path(repo_root, "scripts", "10_build_themes.R"))
source(file.path(repo_root, "scripts", "20_build_indices.R"))
source(file.path(repo_root, "scripts", "80_write_outputs.R"))
source(file.path(repo_root, "scripts", "90_build_charts.R"))
