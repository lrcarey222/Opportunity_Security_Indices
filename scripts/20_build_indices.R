# Build index-level tables.
if (!exists("repo_root")) {
  repo_root <- resolve_repo_root()
}

source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "indices", "build_energy_security_index.R"))
source(file.path(repo_root, "R", "indices", "build_economic_opportunity_index.R"))
source(file.path(repo_root, "R", "indices", "couple_pillar_scores_by_hhi.R"))

config <- getOption("opportunity_security.config")
weights <- getOption("opportunity_security.weights")
missing_data <- getOption("opportunity_security.missing_data")
if (is.null(config) || is.null(weights) || is.null(missing_data)) {
  stop("Config, weights, or missing data config not loaded; run scripts/00_setup.R first.")
}

allow_partial_categories <- isTRUE(config$allow_partial_categories)
include_sub_sector <- isTRUE(config$energy_security_include_sub_sector)

energy_security_inputs <- list(
  energy_access_consumption = energy_access_tbl,
  import_dependence = import_dependence_tbl,
  reserves = reserves_tbl,
  foreign_dependency = foreign_dependency_tbl,
  critical_minerals_processing = critical_minerals_processing_tbl,
  critical_minerals_production = critical_minerals_production_tbl,
  critical_minerals_trade = critical_minerals_trade_tbl,
  energy_consumption = energy_consumption_tbl,
  trade_concentration = trade_concentration_tbl,
  energy_prices = energy_prices_tbl
)

energy_security_outputs <- build_energy_security_index(
  theme_tables = energy_security_inputs,
  weights = weights$energy_security,
  missing_data = missing_data$energy_security,
  allow_partial_categories = allow_partial_categories,
  include_sub_sector = include_sub_sector
)

energy_security_category_scores <- energy_security_outputs$category_scores
energy_security_category_contributions <- energy_security_outputs$category_contributions
energy_security_variable_contributions <- energy_security_outputs$variable_contributions
energy_security_index <- energy_security_outputs$index

economic_opportunity_inputs <- list(
  future_demand = future_demand_tbl,
  lcoe_competitiveness = lcoe_competitiveness_tbl
)

economic_opportunity_outputs <- build_economic_opportunity_index(
  theme_tables = economic_opportunity_inputs,
  weights = weights$economic_opportunity,
  allow_partial_categories = allow_partial_categories
)

economic_opportunity_category_scores <- economic_opportunity_outputs$category_scores
economic_opportunity_category_contributions <- economic_opportunity_outputs$category_contributions
economic_opportunity_variable_contributions <- economic_opportunity_outputs$variable_contributions
economic_opportunity_index <- economic_opportunity_outputs$index

hhi_tbl <- trade_concentration_tbl %>%
  dplyr::filter(variable == "HHI", data_type == "index") %>%
  dplyr::select(tech, supply_chain, sub_sector, Year, HHI = value) %>%
  dplyr::distinct()

energy_security_index_coupled <- couple_pillar_scores_by_hhi(
  pillar_tbl = energy_security_index,
  hhi_tbl = hhi_tbl,
  score_col = energy_security_index,
  include_sub_sector = include_sub_sector
)

if (exists("economic_opportunity_index")) {
  economic_opportunity_index_coupled <- couple_pillar_scores_by_hhi(
    pillar_tbl = economic_opportunity_index,
    hhi_tbl = hhi_tbl,
    score_col = economic_opportunity_index,
    include_sub_sector = include_sub_sector
  )
} else {
  economic_opportunity_index_coupled <- NULL
  warning("economic_opportunity_index not found; skipping HHI coupling.")
}

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
