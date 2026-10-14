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
skipped <- character()
for (spec_name in names(cfg$specifications)) {
  yr <- cfg$specifications[[spec_name]]$snapshot_year
  spec_rows <- ideal %>% filter(spec_name == .env$spec_name, year == .env$yr)
  if (nrow(spec_rows) == 0) {
    warning("Skipping ", spec_name, ": no ideal-point rows for snapshot year ", yr, ".")
    skipped <- c(skipped, spec_name)
    next
  }
  out[[spec_name]] <- compute_ipd_matrix(ideal, yr, spec_name)
}

if (length(out) == 0) {
  stop("No IPD outputs were generated. Check ideal-point estimation outputs and configuration snapshot years.")
}

ipd <- bind_rows(out)
write_csv(ipd, file.path(repo_root, cfg$outputs$ipd))
message("Saved bilateral IPD.")
if (length(skipped) > 0) {
  message("Skipped specifications: ", paste(skipped, collapse = ", "))
}
