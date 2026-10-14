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
skipped <- character()
for (spec_name in names(cfg$specifications)) {
  yr <- cfg$specifications[[spec_name]]$snapshot_year
  if (nrow(ipd %>% filter(spec_name == .env$spec_name, year == .env$yr)) == 0) {
    warning("Skipping ", spec_name, " in seg computation: no IPD rows for snapshot year ", yr, ".")
    skipped <- c(skipped, spec_name)
    next
  }
  seg_out[[spec_name]] <- compute_seg_scores(ipd, country_map, yr, spec_name, cfg$anchors$us_iso3, cfg$anchors$china_iso3)
}

if (length(seg_out) == 0) {
  stop("No seg outputs were generated. Check upstream IPD results.")
}

seg <- bind_rows(seg_out)
write_csv(seg, file.path(repo_root, cfg$outputs$seg))
message("Saved seg scores.")
if (length(skipped) > 0) {
  message("Skipped specifications: ", paste(skipped, collapse = ", "))
}
