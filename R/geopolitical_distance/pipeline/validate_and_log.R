repo_root <- getwd()
for (pkg in c("readr", "yaml", "dplyr", "knitr")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, repos = "https://cloud.r-project.org")
}
library(readr); library(yaml); library(dplyr)
source(file.path(repo_root, "R", "geopolitical_distance", "core.R"))

cfg <- read_yaml(file.path(repo_root, "config", "geopolitical_distance.yml"))
ipd <- read_csv(file.path(repo_root, cfg$outputs$ipd), show_col_types = FALSE)
seg <- read_csv(file.path(repo_root, cfg$outputs$seg), show_col_types = FALSE)
blocs <- read_csv(file.path(repo_root, cfg$outputs$blocs), show_col_types = FALSE)

val_rows <- list()
for (spec_name in unique(seg$spec_name)) {
  v <- validate_outputs(ipd %>% filter(spec_name == spec_name),
                        seg %>% filter(spec_name == spec_name),
                        blocs %>% filter(spec_name == spec_name),
                        cfg$anchors$us_iso3, cfg$anchors$china_iso3)
  val_rows[[spec_name]] <- data.frame(spec_name = spec_name,
                                      ipd_symmetric = v$ipd_symmetric,
                                      ipd_diagonal_zero = v$ipd_diagonal_zero,
                                      gp_norm_bounds = v$gp_norm_bounds,
                                      seg_us = ifelse(length(v$seg_us) == 0, NA, v$seg_us[1]),
                                      seg_china = ifelse(length(v$seg_china) == 0, NA, v$seg_china[1]),
                                      quartile_check_us = v$quartile_check_us,
                                      quartile_check_china = v$quartile_check_china)
  }
validation <- bind_rows(val_rows)
dir.create(file.path(repo_root, "output", "geopolitical_distance"), showWarnings = FALSE, recursive = TRUE)
write_csv(validation, file.path(repo_root, cfg$outputs$validation))

log_path <- file.path(repo_root, "docs", "replication_log.md")
writeLines(c(
  "# Geopolitical Distance Replication Log",
  "",
  "## Data sources",
  "- `unvotes` package datasets: `un_votes`, `un_roll_calls`, `un_roll_call_issues`.",
  "- Optional bilateral trade shares from `data_raw/bilateral_trade_shares.csv`.",
  "",
  "## Model settings",
  "- Primary estimator implemented: yearly 1D Bayesian ideal points via `pscl::ideal`.",
  "- Vote coding: Yes=1, No=0, abstain/absent dropped for estimation (documented deviation).",
  "- Four specs configured in `config/geopolitical_distance.yml`.",
  "",
  "## Ambiguities and handling",
  "- Economic-vote definition ambiguous: implemented issue-text filter containing 'econ'/'economic'.",
  "- Dynamic country-year model from Bailey/Strezhnev/Voeten not fully reproduced; this pipeline uses a faithful ideal-point approach but is an informed approximation.",
  "- Normalization implemented as yearly cross-country theta range (literal formula).",
  "",
  "## Validation",
  paste0("- Validation output written to `", cfg$outputs$validation, "`."),
  "",
  "## Deviations",
  "- If published BST country-year ideal points are later provided, swap them into `country_year_ideal_points.csv` and rerun downstream scripts."
), con = log_path)
message("Validation and replication log complete.")
