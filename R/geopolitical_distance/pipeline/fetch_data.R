args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  repo_root <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
} else {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  while (!file.exists(file.path(repo_root, ".git")) && dirname(repo_root) != repo_root) {
    repo_root <- dirname(repo_root)
  }
}

if (!file.exists(file.path(repo_root, ".git"))) {
  stop("Could not locate repository root (.git).")
}

for (pkg in c("unvotes", "dplyr", "readr", "countrycode")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, repos = "https://cloud.r-project.org")
}

library(unvotes)
library(dplyr)
library(readr)
library(countrycode)

dir.create(file.path(repo_root, "data_raw"), showWarnings = FALSE, recursive = TRUE)

data("un_votes")
data("un_roll_calls")
data("un_roll_call_issues")

votes <- un_votes %>%
  left_join(un_roll_calls %>% select(rcid, session, year), by = "rcid") %>%
  mutate(iso3 = countrycode(country, origin = "country.name", destination = "iso3c", warn = FALSE))

write_csv(votes, file.path(repo_root, "data_raw", "un_votes_enriched.csv"))
write_csv(un_roll_call_issues, file.path(repo_root, "data_raw", "un_roll_call_issues.csv"))
write_csv(un_roll_calls, file.path(repo_root, "data_raw", "un_roll_calls.csv"))

message("Fetched UN voting data to data_raw/.")
