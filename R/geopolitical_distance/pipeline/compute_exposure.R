repo_root <- getwd()
for (pkg in c("readr", "yaml", "dplyr")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, repos = "https://cloud.r-project.org")
}
library(readr)
library(yaml)
library(dplyr)
source(file.path(repo_root, "R", "geopolitical_distance", "core.R"))

cfg <- read_yaml(file.path(repo_root, "config", "geopolitical_distance.yml"))
trade_path <- file.path(repo_root, "data_raw", "bilateral_trade_shares.csv")
if (!file.exists(trade_path)) {
  message("No bilateral_trade_shares.csv found. Writing empty exposure output.")
  write_csv(data.frame(iso3 = character(), country = character(), year = integer(), exposure = numeric(), spec_name = character()),
            file.path(repo_root, cfg$outputs$exposure))
  quit(save = "no")
}

ipd <- read_csv(file.path(repo_root, cfg$outputs$ipd), show_col_types = FALSE)
trade <- read_csv(trade_path, show_col_types = FALSE)
exp <- compute_exposure(trade, ipd)
write_csv(exp, file.path(repo_root, cfg$outputs$exposure))
message("Saved exposure series.")
