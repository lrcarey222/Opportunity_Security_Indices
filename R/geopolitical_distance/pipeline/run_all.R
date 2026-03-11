repo_root <- getwd()
steps <- c(
  "R/geopolitical_distance/pipeline/fetch_data.R",
  "R/geopolitical_distance/pipeline/estimate_ideal_points.R",
  "R/geopolitical_distance/pipeline/compute_ipd.R",
  "R/geopolitical_distance/pipeline/compute_seg.R",
  "R/geopolitical_distance/pipeline/assign_blocs.R",
  "R/geopolitical_distance/pipeline/compute_exposure.R",
  "R/geopolitical_distance/pipeline/make_figures.R",
  "R/geopolitical_distance/pipeline/validate_and_log.R"
)

for (s in steps) {
  message("Running ", s)
  sys.source(file.path(repo_root, s), envir = new.env(parent = globalenv()))
}
message("Geopolitical-distance pipeline complete.")
