# LCOE competitiveness theme builder functions.

lcoe_competitiveness_clean_bnef <- function(lcoe_bnef) {
  require_columns(
    lcoe_bnef,
    c("Scenario", "Metric", "Technology", "Region", "X2024", "X2050"),
    label = "lcoe_bnef"
  )

  lcoe_bnef %>%
    dplyr::mutate(
      Technology = dplyr::recode(
        Technology,
        "CCGT" = "Gas",
        "Coal" = "Coal",
        "PV fixed-axis" = "Solar",
        "PV fixed-axis + storage" = "Solar",
        "Wind onshore" = "Wind",
        "Utility-scale battery (1h)" = "Batteries",
        "Utility-scale battery (4h)" = "Batteries",
        .default = NA_character_
      )
    ) %>%
    dplyr::filter(
      Scenario == "Mid",
      Metric == "LCOE",
      !is.na(Technology)
    ) %>%
    dplyr::select(Region, Technology, X2024, X2050)
}

lcoe_competitiveness_build_indices <- function(lcoe_clean, gamma = 0.5) {
  require_columns(lcoe_clean, c("Region", "Technology", "X2024", "X2050"), label = "lcoe_clean")

  lcoe_clean %>%
    dplyr::group_by(Technology, Region) %>%
    dplyr::summarize(
      lcoe_24_raw = mean(as.numeric(X2024), na.rm = TRUE),
      lcoe_50_raw = mean(as.numeric(X2050), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(Technology) %>%
    dplyr::mutate(
      lcoe_24_index = median_scurve(-lcoe_24_raw, gamma = gamma),
      lcoe_50_index = median_scurve(-lcoe_50_raw, gamma = gamma)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(overall_lcoe_index = mean(dplyr::c_across(dplyr::ends_with("_index")), na.rm = TRUE)) %>%
    dplyr::ungroup()
}

lcoe_competitiveness_build_table <- function(lcoe_indices,
                                             lcoe_years = c(2024, 2050)) {
  lcoe_year <- as.integer(max(lcoe_years))

  lcoe_indices %>%
    dplyr::select(
      Region,
      Technology,
      lcoe_24_raw,
      lcoe_50_raw,
      lcoe_24_index,
      lcoe_50_index,
      overall_lcoe_index
    ) %>%
    tidyr::pivot_longer(
      cols = c(lcoe_24_raw:overall_lcoe_index),
      names_to = c("variable", "data_type"),
      names_pattern = "(.*)_(raw|index)",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      variable = dplyr::case_when(
        variable == "overall_lcoe" ~ "Overall input cost index",
        TRUE ~ variable
      )
    ) %>%
    dplyr::transmute(
      Country = Region,
      tech = Technology,
      supply_chain = "Downstream",
      category = "Energy Prices",
      variable,
      data_type,
      value,
      Year = lcoe_year,
      source = "BNEF LCOE Estimates (2025)",
      explanation = dplyr::case_when(
        data_type == "raw" ~ "Levelized cost of energy",
        data_type == "index" ~ "Percent-rank of LCOE across countries"
      )
    )
}

lcoe_competitiveness <- function(lcoe_bnef,
                                 lcoe_years = c(2024, 2050),
                                 gamma = 0.5) {
  lcoe_clean <- lcoe_competitiveness_clean_bnef(lcoe_bnef)
  lcoe_indices <- lcoe_competitiveness_build_indices(lcoe_clean, gamma = gamma)
  output <- lcoe_competitiveness_build_table(lcoe_indices, lcoe_years = lcoe_years)

  output <- standardize_theme_table(output)
  validate_schema(output)
  output
}
