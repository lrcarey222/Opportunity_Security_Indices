repo_root <- getwd()
for (pkg in c("readr", "yaml", "dplyr")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, repos = "https://cloud.r-project.org")
}
library(readr)
library(yaml)
library(dplyr)
source(file.path(repo_root, "R", "geopolitical_distance", "core.R"))

cfg <- read_yaml(file.path(repo_root, "config", "geopolitical_distance.yml"))
ideal <- read_csv(file.path(repo_root, cfg$outputs$ideal_points), show_col_types = FALSE)

assert_required_columns(ideal, c("iso3", "country", "year", "theta", "theta_se", "spec_name"), "ideal_points")

out <- list()
for (spec_name in names(cfg$specifications)) {
  yr <- cfg$specifications[[spec_name]]$snapshot_year
  out[[spec_name]] <- compute_ipd_matrix(ideal, yr, spec_name)
}
ipd <- bind_rows(out)
write_csv(ipd, file.path(repo_root, cfg$outputs$ipd))
message("Saved bilateral IPD.")
