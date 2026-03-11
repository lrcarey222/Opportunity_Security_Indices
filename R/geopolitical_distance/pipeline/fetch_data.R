args <- commandArgs(trailingOnly = TRUE)
repo_root <- ifelse(length(args) > 0, args[[1]], normalizePath(file.path(dirname(sys.frame(1)$ofile), "..", ".."), mustWork = TRUE))

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
