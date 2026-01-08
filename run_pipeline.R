# Pipeline runner: execute scripts in order.
source("scripts/10_build_themes.R")
source("scripts/20_build_indices.R")
source("scripts/80_write_outputs.R")
source("scripts/90_build_charts.R")
