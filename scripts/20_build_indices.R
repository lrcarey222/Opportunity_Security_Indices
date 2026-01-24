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
energy_security_inputs <- list(
  energy_access_consumption = energy_access_tbl,
  solar_pv_potential = solar_pv_potential_tbl,
  wind_potential = wind_potential_tbl,
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
  wind_potential = wind_potential_tbl,
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
library(dplyr)

iea_trl <- read.csv("C:/Users/LCarey/Downloads/iea_trl_tech.csv") %>%
  mutate(
    tech = as.character(tech),
    trl2023 = as.numeric(trl2023)
  )

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
    iea_trl %>% dplyr::select(tech, trl2023),
    by = "tech"
  ) %>%
  mutate(trl2023 = dplyr::coalesce(as.numeric(trl2023), 11)) %>%
  filter(tech %in% techs) %>%
  group_by(Country) %>%
  mutate(
    # component indices
    eo  = median_scurve(Economic_Opportunity_Index),
    es  = 1 - median_scurve(Energy_Security_Index),
    pol = median_scurve(value),
    
    # impute NAs to country mean (computed components)
    eo  = if_else(is.na(eo),  mean(eo,  na.rm = TRUE), eo),
    es  = if_else(is.na(es),  mean(es,  na.rm = TRUE), es),
    pol = if_else(is.na(pol), mean(pol, na.rm = TRUE), pol),
    
    # supply-chain weights
    sc_weight = case_when(
      supply_chain == "Upstream"   ~ 0.50,
      supply_chain == "Midstream"  ~ 0.75,
      supply_chain == "Downstream" ~ 0.25,
      TRUE ~ NA_real_
    ),
    
    # tech weight (ghg_index) - safer global fallback than within-country
    tech_weight = coalesce(ghg_index, mean(tech_ghg$ghg_index, na.rm = TRUE)),
    
    # TRL index (scaled 0-1 using your existing curve)
    trl_index = median_scurve(trl2023),
    
    # base score
    base_score = eo + es + pol,
    
    # UPDATED strategic index weights
    strategic_index =
      0.20 * base_score +
      0.25 * trl_index +
      0.15 * eo +
      0.15 * es +
      0.35 * (sc_weight * tech_weight)
  ) %>%
  ungroup()





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
  "USA"= strategic_index %>% filter(Country == "United States")
)

# Keep only requested columns, sort, and (optionally) rename for the sheet
sheets <- lapply(sheets, function(df) {
  df %>%
    transmute(
      Country,
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
out_path <-  "C:/Users/LCarey/Downloads/strategic_index_selected_countries.xlsx"
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
    contrib_eo      = 0.35 * eo,
    contrib_es      = 0.35 * es,
    contrib_policy  = 0.20 * pol,
    contrib_trl     = 0.25 * trl_index,
    contrib_sc_tech = 0.35 * (sc_weight * tech_weight)
  ) %>%
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
  filter(Country %in% c("Japan","South Korea","India","Viet Nam")) %>%
  select(Country, sector, sector_key, strategic_index,
         contrib_eo, contrib_es, contrib_policy, contrib_trl, contrib_sc_tech) %>%
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
                       contrib_trl     = "TRL",
                       contrib_sc_tech = "Supply chain × Tech weight"
    )
  )

rmi_palette <- c("#0BD0D9",
                 "#0989B1",
                 "#003A61",
                 "#FFCA08",
                 "#F8931D",
                 "#548538",
                 "#7F7F7F")

ggplot(plot_df %>% filter(Country=="India"), aes(x = sector_key, y = contribution, fill = component)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ Country, scales = "free_y") +
  scale_x_discrete(labels = function(x) sub("^.*\\|\\|", "", x)) +
  scale_fill_manual(values = rmi_palette) +
  labs(x = NULL, y = "Weighted contribution to strategic_index", fill = NULL) +
  theme_minimal()
