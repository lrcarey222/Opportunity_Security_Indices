# Build index-level tables.
if (!exists("repo_root")) {
  repo_root <- resolve_repo_root()
}

source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "levels.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(repo_root, "R", "categories", "policy", "iea_policy_index.R"))
source(file.path(repo_root, "R", "categories", "policy", "cat_policy_index.R"))
source(file.path(repo_root, "R", "indices", "build_energy_security_index_v2.R"))
source(file.path(repo_root, "R", "indices", "build_economic_opportunity_index_v2.R"))
source(file.path(repo_root, "R", "indices", "build_indices.R"))
source(file.path(repo_root, "R", "indices", "build_policy_index.R"))
source(file.path(repo_root, "R", "indices", "couple_pillar_scores_by_hhi.R"))

techs <- c("Electric Vehicles",
           "Nuclear","Coal","Batteries","Green Hydrogen","Wind","Oil",                       
           "Solar", "Gas", "Geothermal","Electric Grid")

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
if (!dir.exists(processed_dir)) {
  stop("Processed data directory not found: ", processed_dir)
}

read_processed_tbl <- function(name, processed_dir) {
  path <- file.path(processed_dir, paste0(name, ".rds"))
  if (!file.exists(path)) {
    stop("Processed table not found: ", path)
  }
  readRDS(path)
}

energy_access_tbl <- read_processed_tbl("energy_access_tbl", processed_dir)
solar_pv_potential_tbl <- read_processed_tbl("solar_pv_potential_tbl", processed_dir)
wind_potential_tbl <- read_processed_tbl("wind_potential_tbl", processed_dir)
geothermal_potential_tbl <- read_processed_tbl("geothermal_potential_tbl", processed_dir)
import_dependence_tbl <- read_processed_tbl("import_dependence_tbl", processed_dir)
reserves_tbl <- read_processed_tbl("reserves_tbl", processed_dir)
foreign_dependency_tbl <- read_processed_tbl("foreign_dependency_tbl", processed_dir)
critical_minerals_processing_tbl <- read_processed_tbl(
  "critical_minerals_processing_tbl",
  processed_dir
)
critical_minerals_production_tbl <- read_processed_tbl(
  "critical_minerals_production_tbl",
  processed_dir
)
critical_minerals_trade_tbl <- read_processed_tbl("critical_minerals_trade_tbl", processed_dir)
energy_consumption_tbl <- read_processed_tbl("energy_consumption_tbl", processed_dir)
trade_concentration_tbl <- read_processed_tbl("trade_concentration_tbl", processed_dir)
energy_prices_tbl <- read_processed_tbl("energy_prices_tbl", processed_dir)
export_feasibility_tbl <- read_processed_tbl("export_feasibility_tbl", processed_dir)
future_demand_tbl <- read_processed_tbl("future_demand_tbl", processed_dir)
lcoe_competitiveness_tbl <- read_processed_tbl("lcoe_competitiveness_tbl", processed_dir)
market_share_manufacturing_tbl <- read_processed_tbl(
  "market_share_manufacturing_tbl",
  processed_dir
)
cost_competitiveness_tbl <- read_processed_tbl("cost_competitiveness_tbl", processed_dir)
production_depth_momentum_tbl <- read_processed_tbl(
  "production_depth_momentum_tbl",
  processed_dir
)
overcapacity_premium_tbl <- read_processed_tbl("overcapacity_premium_tbl", processed_dir)
technological_readiness_tbl <- read_processed_tbl(
  "technological_readiness_tbl",
  processed_dir
)
policy_component_tbl <- read_processed_tbl("policy_component_tbl", processed_dir)
policy_outputs <- read_processed_tbl("policy_outputs", processed_dir)
nipo_tech_year<-read_processed_tbl("nipo_tech_year", processed_dir)

tech_ghg <- read_processed_tbl("tech_ghg_tbl", processed_dir)

energy_security_inputs <- list(
  energy_access_consumption = energy_access_tbl,
  solar_pv_potential = solar_pv_potential_tbl,
  wind_potential = wind_potential_tbl,
  geothermal_potential = geothermal_potential_tbl,
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
energy_security_index <- energy_security_outputs$index %>%
  filter(tech %in% techs)

economic_opportunity_inputs <- list(
  energy_access_consumption = energy_access_tbl,
  solar_pv_potential = solar_pv_potential_tbl,
  wind_potential = wind_potential_tbl,
  geothermal_potential = geothermal_potential_tbl,
  energy_consumption = energy_consumption_tbl,
  energy_prices = energy_prices_tbl,
  export_feasibility = export_feasibility_tbl,
  foreign_dependency = foreign_dependency_tbl,
  future_demand = future_demand_tbl,
  lcoe_competitiveness = lcoe_competitiveness_tbl,
  market_share_manufacturing = market_share_manufacturing_tbl,
  cost_competitiveness = cost_competitiveness_tbl,
  production_depth_momentum = production_depth_momentum_tbl,
  overcapacity_premium = overcapacity_premium_tbl,
  technological_readiness = technological_readiness_tbl
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
economic_opportunity_index <- economic_opportunity_outputs$index%>%
  filter(tech %in% techs)

technological_readiness_index <- economic_opportunity_category_scores %>%
  dplyr::filter(.data$category == "Technological Readiness") %>%
  dplyr::select(
    Country,
    tech,
    supply_chain,
    trl_index = category_score
  )

if (!exists("policy_component_tbl") || !exists("policy_outputs")) {
  stop("Policy theme outputs not found; run scripts/10_build_themes.R first.")
}

required_vars <- c(
  "IEA PAMS Policy Index",
  "NIPO Policy Index",
  "CAT Policy Index",
  "Dual-use policy score"
)

weights_tbl <- tibble::tibble(
  variable = required_vars,
  w = c(3, 4, 2, 1)  # IEA=3, NIPO=4, CAT=2, Dual-Use=1 (same order as required_vars)
)

# (Optional but robust) collapse to one value per Country×tech×supply_chain×variable
policy_components_clean <- policy_component_tbl %>%
  dplyr::filter(tech %in% techs, variable %in% required_vars) %>%
  dplyr::group_by(Country, tech, supply_chain, variable) %>%
  dplyr::summarise(
    value = dplyr::if_else(all(is.na(value)), NA_real_, mean(value, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::left_join(weights_tbl, by = "variable")

# Country-level eligibility: must have ALL 4 variables somewhere (non-NA)
eligible_countries <- policy_components_clean %>%
  dplyr::group_by(Country, variable) %>%
  dplyr::summarise(has = any(!is.na(value)), .groups = "drop") %>%
  tidyr::complete(Country, variable = required_vars, fill = list(has = FALSE)) %>%
  dplyr::group_by(Country) %>%
  dplyr::summarise(ok = all(has), .groups = "drop") %>%
  dplyr::filter(ok) %>%
  dplyr::pull(Country)

policy_index <- policy_components_clean %>%
  dplyr::filter(Country %in% eligible_countries) %>%
  dplyr::group_by(Country, tech, supply_chain) %>%
  dplyr::summarise(
    value = {
      denom <- sum(w[!is.na(value)])
      if (denom == 0) NA_real_ else sum(value * w, na.rm = TRUE) / denom
    },
    .groups = "drop"
  ) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::mutate(
    category = "Policy",
    variable = "Overall Policy Index",
    data_type = "index",
    Year = 0L,
    source = "Author calculation",
    explanation = "Weighted mean of NIPO (4), IEA PAMS (3), CAT (2), Dual-use (1); countries kept only if all 4 exist somewhere"
  ) %>%
  dplyr::select(
    Country, tech, supply_chain,
    category, variable, data_type,
    value, Year, source, explanation
  )

economic_opportunity_index <- index_outputs$economic_opportunity_index
energy_security_index <- index_outputs$energy_security_index
policy_index <-index_outputs$policy_index

strategic_index <- left_join(
  economic_opportunity_index,
  energy_security_index,
  by = c("Country","tech","supply_chain")
) %>%
  left_join(
    policy_index %>% dplyr::select(Country, tech, supply_chain, value),
    by = c("Country","tech","supply_chain")
  ) %>%
  # tech_weight from ghg_index
  left_join(
    tech_ghg %>% dplyr::select(tech, ghg_index),
    by = "tech"
  ) %>%
  # trl2023 from IEA (default to 11 if missing)
  left_join(
    technological_readiness_index %>% dplyr::select(Country, tech, supply_chain, trl_index),
    by = c("Country","tech","supply_chain")
  ) %>%
  filter(tech %in% techs) %>%
  group_by(Country,supply_chain) %>%
  mutate(
    
    # impute NAs to country mean (computed components)
    Economic_Opportunity_Index  = if_else(is.na(Economic_Opportunity_Index),  mean(Economic_Opportunity_Index,  na.rm = TRUE), Economic_Opportunity_Index),
    Energy_Security_Index  = if_else(is.na(Energy_Security_Index),  mean(Energy_Security_Index,  na.rm = TRUE), Energy_Security_Index),
    Energy_Security_Index=1-Energy_Security_Index,
    value = if_else(is.na(value), mean(value, na.rm = TRUE), value)
  ) %>%
  group_by(Country) %>%
  mutate(
    # component indices
    eo  = median_scurve(Economic_Opportunity_Index),
    es  = 1 - median_scurve(Energy_Security_Index),
    pol = median_scurve(value),
    
     # supply-chain weights
    sc_weight = case_when(
      supply_chain == "Upstream"   ~ 0.50,
      supply_chain == "Midstream"  ~ 0.75,
      supply_chain == "Downstream" ~ 0.25,
      TRUE ~ NA_real_
    ),
    # tech weight (ghg_index) - safer global fallback than within-country
    tech_weight = coalesce(ghg_index, mean(tech_ghg$ghg_index, na.rm = TRUE)),
    
    # base score
    base_score = eo + es + pol,
    
    # UPDATED strategic index weights
    strategic_index =
      0.2 * pol +
      #0.25 * trl_index +
      0.3 * Economic_Opportunity_Index +
      0.3 * Energy_Security_Index +
      0.1 * sc_weight +
      0.1 * tech_weight
  ) %>%
  ungroup() %>%
  mutate(
    sector = paste(tech, supply_chain, sep = " - ")
  ) %>%
  filter(sector != "Geothermal - Midstream") %>%
  arrange(desc(strategic_index))





if (exists("economic_opportunity_index")) {
  interdependence_path <- file.path(
    repo_root,
    config$raw_data_dir,
    "interdependence_edges_primary_secondary_tertiary.csv"
  )
  if (!file.exists(interdependence_path)) {
    stop("Interdependence edges file not found: ", interdependence_path)
  }
  interdependence_edges <- utils::read.csv(interdependence_path, stringsAsFactors = FALSE)

  economic_opportunity_index_coupled <- couple_pillar_scores_by_hhi(
    pillar_tbl = economic_opportunity_index,
    interdependence_tbl = interdependence_edges,
    score_col = economic_opportunity_index,
    include_sub_sector = include_sub_sector
  )
} else {
  economic_opportunity_index_coupled <- NULL
  warning("economic_opportunity_index not found; skipping interdependence coupling.")
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
    strategic_index = strategic_index
  ),
  outputs_rds_path
)


# install.packages("openxlsx")
library(dplyr)
library(openxlsx)

# Countries to export (data may store Korea as "South Korea")
countries_export <- c("Japan", "India", "South Korea", "Viet Nam", "United States")

# Build the 4 data frames (and name sheets as requested)
sheets <- list(
  "Japan"   = strategic_index %>% filter(Country == "Japan"),
  "India"   = strategic_index %>% filter(Country == "India"),
  "Korea"   = strategic_index %>% filter(Country %in% c("Korea", "South Korea")),
  "Viet Nam"= strategic_index %>% filter(Country == "Viet Nam"),
  "USA"= strategic_index %>% filter(Country == "United States"),
  "UK" = strategic_index %>% filter(Country=="United Kingdom")
)

# Keep only requested columns, sort, and (optionally) rename for the sheet
sheets <- lapply(sheets, function(df) {
  df %>%
    transmute(
      Country,
      sector,
      tech,
      supply_chain,
      eo,
      es,
      pol,
      neis_weight=sc_weight,
      climate_weight=tech_weight,
      trl_index,
      `strategic index` = strategic_index
    ) %>%
    arrange(desc(`strategic index`))
})

# Write to Excel with one tab per country
out_path <- file.path(processed_dir, "strategic_index_selected_countries.xlsx")
openxlsx::write.xlsx(
  x = sheets,
  file = out_path,
  overwrite = TRUE
)

out_path

library(dplyr)
library(tidyr)
library(ggplot2)

# Decompose strategic_index into non-overlapping weighted contributions
# strategic_index =
#   0.20*(eo+es+pol) + 0.15*trl_index + 0.15*eo + 0.15*es + 0.35*(sc_weight*tech_weight)
# => contributions:
#   EO      = 0.35*eo
#   ES      = 0.35*es
#   Policy  = 0.20*pol
#   TRL     = 0.15*trl_index
#   SCxTech = 0.35*(sc_weight*tech_weight)

top10_by_country <- strategic_index %>%
  mutate(
    sector = paste(tech, supply_chain, sep = " - "),
    contrib_eo      = 0.25 * Economic_Opportunity_Index,
    contrib_es      = 0.25 * Energy_Security_Index,
    contrib_policy  = 0.2 * pol,
    contrib_trl     = 0 * trl_index,
    contrib_sc = 0.15 * (sc_weight ),
    contrib_ghg = 0.15* tech_weight
  ) %>%
  filter(sector != "Geothermal - Midstream") %>%
  group_by(Country) %>%
  slice_max(order_by = strategic_index, n = 10, with_ties = FALSE) %>%
  arrange(Country, strategic_index) %>%   # ascending so highest ends up at top after coord_flip()
  mutate(
    # unique key per facet to enforce ordering
    sector_key = paste(Country, sector, sep = "||"),
    sector_key = factor(sector_key, levels = unique(sector_key))
  ) %>%
  ungroup()

plot_df <- top10_by_country %>%
  filter(Country %in% c("Japan","South Korea","India","Viet Nam","United Kingdom")) %>%
  select(Country, sector, sector_key, strategic_index,
         contrib_eo, contrib_es, contrib_policy, contrib_sc,contrib_ghg) %>%
  pivot_longer(
    cols = starts_with("contrib_"),
    names_to = "component",
    values_to = "contribution"
  ) %>%
  mutate(
    component = recode(component,
                       contrib_eo      = "Economic opportunity",
                       contrib_es      = "Energy security",
                       contrib_policy  = "Policy",
                       contrib_ghg     = "Climate",
                       contrib_sc = "Supply chain"
    )
  )

rmi_palette <- c("#0BD0D9",
                 "#0989B1",
                 "#003A61",
                 "#FFCA08",
                 "#F8931D",
                 "#548538",
                 "#7F7F7F")

ggplot(plot_df %>% filter(Country=="United Kingdom"), aes(x = sector_key, y = contribution, fill = component)) +
  geom_col() +
  coord_flip() +
  #facet_wrap(~ Country, scales = "free_y") +
  scale_x_discrete(labels = function(x) sub("^.*\\|\\|", "", x)) +
  scale_fill_manual(values = rmi_palette) +
  labs(x = NULL, y = "Weighted contribution to strategic_index", fill = NULL) +
  theme_minimal()

