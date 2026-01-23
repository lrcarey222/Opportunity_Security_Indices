# Build index-level tables.
if (!exists("repo_root")) {
  repo_root <- resolve_repo_root()
}

source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "levels.R"))
source(file.path(repo_root, "R", "indices", "build_energy_security_index_v2.R"))
source(file.path(repo_root, "R", "indices", "build_economic_opportunity_index_v2.R"))
source(file.path(repo_root, "R", "indices", "couple_pillar_scores_by_hhi.R"))

config <- getOption("opportunity_security.config")
weights <- getOption("opportunity_security.weights")
missing_data <- getOption("opportunity_security.missing_data")
if (is.null(config) || is.null(weights) || is.null(missing_data)) {
  stop("Config, weights, or missing data config not loaded; run scripts/00_setup.R first.")
}

allow_partial_categories <- isTRUE(config$allow_partial_categories)
include_sub_sector <- isTRUE(if (!is.null(config$include_sub_sector)) {
  config$include_sub_sector
} else {
  config$energy_security_include_sub_sector
})
processed_dir <- file.path(repo_root, config$processed_dir)
energy_security_inputs <- list(
  energy_access_consumption = energy_access_tbl,
  solar_pv_potential = solar_pv_potential_tbl,
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

energy_security_outputs <- build_energy_security_index_v2(
  theme_tables = energy_security_inputs,
  weights = weights$energy_security,
  missing_data = missing_data$energy_security,
  allow_partial_categories = allow_partial_categories,
  include_sub_sector = include_sub_sector
)

energy_security_category_scores <- energy_security_outputs$category_scores
energy_security_category_contributions <- energy_security_outputs$category_contributions
energy_security_variable_contributions <- energy_security_outputs$variable_contributions
energy_security_index <- if (!is.null(energy_security_outputs$index)) {
  energy_security_outputs$index
} else {
  energy_security_outputs$energy_security_index
}

economic_opportunity_inputs <- list(
  energy_access_consumption = energy_access_tbl,
  solar_pv_potential = solar_pv_potential_tbl,
  energy_consumption = energy_consumption_tbl,
  energy_prices = energy_prices_tbl,
  export_feasibility = export_feasibility_tbl,
  foreign_dependency = foreign_dependency_tbl,
  future_demand = future_demand_tbl,
  lcoe_competitiveness = lcoe_competitiveness_tbl,
  market_share_manufacturing = market_share_manufacturing_tbl,
  cost_competitiveness = cost_competitiveness_tbl,
  production_depth_momentum = production_depth_momentum_tbl,
  overcapacity_premium = overcapacity_premium_tbl
)

economic_opportunity_outputs <- build_economic_opportunity_index_v2(
  theme_tables = economic_opportunity_inputs,
  weights = weights$economic_opportunity,
  allow_partial_categories = allow_partial_categories,
  include_sub_sector = include_sub_sector,
  missing_data = missing_data$economic_opportunity
)

economic_opportunity_category_scores <- economic_opportunity_outputs$category_scores
economic_opportunity_category_contributions <- economic_opportunity_outputs$category_contributions
economic_opportunity_variable_contributions <- economic_opportunity_outputs$variable_contributions
economic_opportunity_index <- economic_opportunity_outputs$index

if (!exists("policy_component_tbl") || !exists("policy_outputs")) {
  stop("Policy theme outputs not found; run scripts/10_build_themes.R first.")
}

policy_agg <- policy_outputs$policy_agg
policy_clean <- policy_outputs$policy_clean

policy_index <- policy_component_tbl %>%
  dplyr::group_by(.data$Country, .data$tech, .data$supply_chain) %>%
  dplyr::summarize(
    value = if (all(is.na(.data$value))) NA_real_ else mean(.data$value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    category = "Policy",
    variable = "Overall Policy Index",
    data_type = "index",
    Year = 0L,
    source = "Author calculation",
    explanation = "Mean of IEA PAMS and CAT policy indices"
  ) %>%
  dplyr::select(
    Country,
    tech,
    supply_chain,
    category,
    variable,
    data_type,
    value,
    Year,
    source,
    explanation
  )


strategic_index<-left_join(economic_opportunity_index,energy_security_index,by=c("Country","tech","supply_chain")) %>%
  mutate(strategic_index=Economic_Opportunity_Index + Energy_Security_Index )




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

outputs_rds_path <- Sys.getenv("OPSI_OUTPUTS_RDS", "")
if (!nzchar(outputs_rds_path)) {
  outputs_rds_path <- if (!is.null(config$outputs_rds) && nzchar(config$outputs_rds)) {
    file.path(repo_root, config$outputs_rds)
  } else {
    file.path(outputs_dir, "index_outputs.rds")
  }
}

outputs_rds_dir <- dirname(outputs_rds_path)
if (!dir.exists(outputs_rds_dir)) {
  dir.create(outputs_rds_dir, recursive = TRUE)
}

saveRDS(
  list(
    energy_security_outputs = energy_security_outputs,
    economic_opportunity_outputs = economic_opportunity_outputs,
    policy_outputs = policy_outputs,
    energy_security_category_scores = energy_security_category_scores,
    energy_security_category_contributions = energy_security_category_contributions,
    energy_security_variable_contributions = energy_security_variable_contributions,
    energy_security_index = energy_security_index,
    economic_opportunity_category_scores = economic_opportunity_category_scores,
    economic_opportunity_category_contributions = economic_opportunity_category_contributions,
    economic_opportunity_variable_contributions = economic_opportunity_variable_contributions,
    economic_opportunity_index = economic_opportunity_index,
    policy_component_tbl = policy_component_tbl,
    policy_index = policy_index,
    policy_agg = policy_agg,
    policy_clean = policy_clean
  ),
  outputs_rds_path
)
