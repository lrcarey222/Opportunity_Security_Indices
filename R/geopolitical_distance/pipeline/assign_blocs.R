repo_root <- getwd()
for (pkg in c("readr", "yaml", "dplyr")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, repos = "https://cloud.r-project.org")
}
library(readr)
library(yaml)
library(dplyr)
source(file.path(repo_root, "R", "geopolitical_distance", "core.R"))

cfg <- read_yaml(file.path(repo_root, "config", "geopolitical_distance.yml"))
seg <- read_csv(file.path(repo_root, cfg$outputs$seg), show_col_types = FALSE)

blocs <- seg %>% group_by(spec_name, year) %>% group_modify(~assign_blocs(.x)) %>% ungroup()
pairs <- blocs %>% group_by(spec_name, year) %>% group_modify(~build_pair_bloc_labels(.x)) %>% ungroup()

write_csv(blocs, file.path(repo_root, cfg$outputs$blocs))
write_csv(pairs, file.path(repo_root, cfg$outputs$pair_bloc))
message("Saved bloc assignments and pair labels.")
