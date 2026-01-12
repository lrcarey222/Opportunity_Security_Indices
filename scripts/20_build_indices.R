# Build index-level tables.
if (!exists("repo_root")) {
  repo_root <- resolve_repo_root()
}

source(file.path(repo_root, "R", "indices", "build_energy_security_index.R"))

config <- getOption("opportunity_security.config")
weights <- getOption("opportunity_security.weights")
if (is.null(config) || is.null(weights)) {
  stop("Config or weights not loaded; run scripts/00_setup.R first.")
}

allow_partial_categories <- isTRUE(config$allow_partial_categories)

energy_security_inputs <- list(
  energy_access_tbl = energy_access_tbl,
  import_dependence_tbl = import_dependence_tbl,
  reserves_tbl = reserves_tbl,
  foreign_dependency_tbl = foreign_dependency_tbl,
  critical_minerals_processing_tbl = critical_minerals_processing_tbl,
  critical_minerals_production_tbl = critical_minerals_production_tbl,
  critical_minerals_trade_tbl = critical_minerals_trade_tbl,
  energy_consumption_tbl = energy_consumption_tbl,
  trade_concentration_tbl = trade_concentration_tbl,
  energy_prices_tbl = energy_prices_tbl
)

energy_security_outputs <- build_energy_security_index(
  theme_tables = energy_security_inputs,
  weights = weights$energy_security,
  allow_partial_categories = allow_partial_categories
)

energy_security_category_scores <- energy_security_outputs$category_scores
energy_security_category_contributions <- energy_security_outputs$category_contributions
energy_security_variable_contributions <- energy_security_outputs$variable_contributions
energy_security_index <- energy_security_outputs$index

outputs_dir <- if (!is.null(config$outputs_dir) && nzchar(config$outputs_dir)) {
  file.path(repo_root, config$outputs_dir)
} else {
  file.path(repo_root, config$processed_dir, "outputs")
}

if (!dir.exists(outputs_dir)) {
  dir.create(outputs_dir, recursive = TRUE)
}

utils::write.csv(
  energy_security_category_contributions,
  file = file.path(outputs_dir, "energy_security_category_contributions.csv"),
  row.names = FALSE
)

utils::write.csv(
  energy_security_variable_contributions,
  file = file.path(outputs_dir, "energy_security_variable_contributions.csv"),
  row.names = FALSE
)
