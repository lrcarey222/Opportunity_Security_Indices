repo_root <- getwd()
for (pkg in c("readr", "yaml", "dplyr", "countrycode", "pscl")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, repos = "https://cloud.r-project.org")
}

library(readr)
library(yaml)
library(dplyr)

source(file.path(repo_root, "R", "geopolitical_distance", "ideal_points.R"))

cfg <- yaml::read_yaml(file.path(repo_root, "config", "geopolitical_distance.yml"))
votes <- readr::read_csv(file.path(repo_root, "data_raw", "un_votes_enriched.csv"), show_col_types = FALSE)
issues <- readr::read_csv(file.path(repo_root, "data_raw", "un_roll_call_issues.csv"), show_col_types = FALSE)

all_points <- list()
for (spec_name in names(cfg$specifications)) {
  spec <- cfg$specifications[[spec_name]]
  message("Estimating ", spec_name)
  filtered <- filter_votes_for_spec(votes, issues, spec)
  pts <- estimate_ideal_points_yearly(filtered)
  if (is.null(pts) || nrow(pts) == 0) next

  iso_map <- filtered %>% distinct(country, iso3)
  pts <- pts %>% left_join(iso_map, by = "country") %>% mutate(spec_name = spec_name) %>%
    select(iso3, country, year, theta, theta_se, spec_name)
  all_points[[spec_name]] <- pts
}

all_points_df <- bind_rows(all_points)

required_cols <- c("iso3", "country", "year", "theta", "theta_se", "spec_name")
if (!all(required_cols %in% names(all_points_df))) {
  all_points_df <- tibble::tibble(
    iso3 = character(),
    country = character(),
    year = integer(),
    theta = double(),
    theta_se = double(),
    spec_name = character()
  )
}

if (nrow(all_points_df) == 0) {
  warning(
    "No ideal points were estimated for any specification. ",
    "Downstream pipeline steps will fail unless input vote data or filters are adjusted."
  )
}

dir.create(file.path(repo_root, "data_processed"), showWarnings = FALSE, recursive = TRUE)
write_csv(all_points_df, file.path(repo_root, cfg$outputs$ideal_points))
message("Saved ideal points.")
