# ==============================================================================
# Task 0 - NIPO DIS baseline capture
# ------------------------------------------------------------------------------
# Runs nipo_policy_outputs() with exactly the arguments scripts/10_build_themes.R
# uses, and writes by_tech_sc / by_tech_sc_year to diagnostics/baseline/.
#
# This is the reference the `dis_legacy_mode = TRUE` regression test compares
# against, so it must be captured BEFORE the strength chain is touched.
#
# Run from the repo root:
#   Rscript diagnostics/00_nipo_baseline.R
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
})

repo_root <- normalizePath(
  if (nzchar(Sys.getenv("OPSI_REPO_ROOT"))) Sys.getenv("OPSI_REPO_ROOT") else getwd(),
  winslash = "/", mustWork = TRUE
)

source(file.path(repo_root, "scripts", "utils", "raw_inputs.R"))
source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "categories", "policy", "nipo_policy_index.R"))

raw_data_path <- file.path(repo_root, "data", "raw")
out_dir <- file.path(repo_root, "diagnostics", "baseline")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

nipo_policy_path <- resolve_versioned_raw_input(
  raw_data_path,
  pattern = "^GTA NIPO - .*\\.xlsx$",
  fallback = "GTA NIPO - February 2026.xlsx",
  label = "GTA New Industrial Policy Observatory"
)
message("NIPO export: ", basename(nipo_policy_path))

nipo_raw <- readxl::read_excel(nipo_policy_path, sheet = 1)

hs6_categories_essential <- readr::read_csv(
  file.path(raw_data_path, "hs6_categories_with_essential.csv"),
  show_col_types = FALSE
) %>%
  rename("Value.Chain" = "Value Chain")

country_info <- standardize_country_info(
  utils::read.csv(file.path(raw_data_path, "wdi_country_info.csv"))
)

# Same call signature as scripts/10_build_themes.R:729.
nipo_policy_out <- nipo_domestic_intervention_outputs(
  raw_nipo = nipo_raw,
  hs6_categories_essential,
  country_info = country_info,
  rolling_window_years = 3,
  balance_alpha = 0.5,
  weight_by_active_fraction = TRUE
)

by_tech_sc <- nipo_policy_out$by_tech_sc
by_tech_sc_year <- nipo_policy_out$by_tech_sc_year

utils::write.csv(by_tech_sc,
                 file.path(out_dir, "by_tech_sc.csv"), row.names = FALSE)
utils::write.csv(by_tech_sc_year,
                 file.path(out_dir, "by_tech_sc_year.csv"), row.names = FALSE)
saveRDS(by_tech_sc, file.path(out_dir, "by_tech_sc.rds"))
saveRDS(by_tech_sc_year, file.path(out_dir, "by_tech_sc_year.rds"))

# Provenance, so a later comparison knows what it is comparing against.
writeLines(
  c(
    paste0("captured_at_utc: ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
    paste0("nipo_export: ", basename(nipo_policy_path)),
    paste0("git_head: ", tryCatch(
      system2("git", c("-C", shQuote(repo_root), "rev-parse", "HEAD"), stdout = TRUE),
      error = function(e) "unknown"
    )),
    paste0("as_of_date: ", as.character(unique(by_tech_sc$as_of_date)[1])),
    paste0("by_tech_sc_rows: ", nrow(by_tech_sc)),
    paste0("by_tech_sc_year_rows: ", nrow(by_tech_sc_year)),
    paste0("policy_base_rows: ", nrow(nipo_policy_out$internals$policy_base)),
    paste0("policy_asof_rows: ", nrow(nipo_policy_out$internals$policy_asof)),
    paste0("alloc_rows: ", nrow(nipo_policy_out$internals$policy_alloc_tech_sc))
  ),
  file.path(out_dir, "MANIFEST.txt")
)

message("Baseline written to ", out_dir)
print(utils::head(by_tech_sc, 3))
