# CAT policy index (country x tech x supply_chain).

cat_policy_index <- function(tech_ghg,
                             cat_policy,
                             supply_chain_universe = c("Upstream", "Midstream", "Downstream"),
                             year = 0L) {
  require_columns(tech_ghg, c("tech", "ghg_index"), label = "tech_ghg")
  require_columns(cat_policy, c("Country", "climate_policy_index"), label = "cat_policy")

  base_tbl <- tidyr::crossing(cat_policy, tech_ghg) %>%
    tidyr::crossing(supply_chain = supply_chain_universe) %>%
    dplyr::mutate(
      value = .data$ghg_index * .data$climate_policy_index
    )

  base_tbl %>%
    dplyr::transmute(
      Country = .data$Country,
      tech = .data$tech,
      supply_chain = .data$supply_chain,
      category = "Policy",
      variable = "CAT Policy Index",
      data_type = "index",
      value = .data$value,
      Year = as.integer(year),
      source = "Climate Action Tracker & IPCC",
      explanation = "Policy index computed as ghg_index × climate_policy_index"
    )
}
