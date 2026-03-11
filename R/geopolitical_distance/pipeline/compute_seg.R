repo_root <- getwd()
for (pkg in c("readr", "yaml", "dplyr")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, repos = "https://cloud.r-project.org")
}
library(readr)
library(yaml)
library(dplyr)
source(file.path(repo_root, "R", "geopolitical_distance", "core.R"))

cfg <- read_yaml(file.path(repo_root, "config", "geopolitical_distance.yml"))
ipd <- read_csv(file.path(repo_root, cfg$outputs$ipd), show_col_types = FALSE)
ideal <- read_csv(file.path(repo_root, cfg$outputs$ideal_points), show_col_types = FALSE)
country_map <- ideal %>% distinct(iso3, country)

seg_out <- list()
for (spec_name in names(cfg$specifications)) {
  yr <- cfg$specifications[[spec_name]]$snapshot_year
  seg_out[[spec_name]] <- compute_seg_scores(ipd, country_map, yr, spec_name, cfg$anchors$us_iso3, cfg$anchors$china_iso3)
}
seg <- bind_rows(seg_out)
write_csv(seg, file.path(repo_root, cfg$outputs$seg))
message("Saved seg scores.")
