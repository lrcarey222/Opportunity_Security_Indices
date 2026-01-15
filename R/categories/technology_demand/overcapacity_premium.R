# Overcapacity premium theme builder functions.

overcapacity_premium_build_overcapacity_clean <- function(overcapacity_raw) {
  require_columns(
    overcapacity_raw,
    c("Sector", "Component", "Overcapacity Ratio"),
    label = "overcapacity_raw"
  )

  overcapacity_raw %>%
    tidyr::fill(Sector, .direction = "down") %>%
    dplyr::mutate(
      Sector = dplyr::case_when(
        Sector == "H???" ~ "Green Hydrogen",
        Sector == "Battery" ~ "Batteries",
        TRUE ~ Sector
      ),
      Sector = dplyr::case_when(
        Component %in% c("Separators", "Electrolytes", "Cathodes", "Anodes", "cells") ~ "Batteries",
        Component %in% c("Onshore nacelles") ~ "Wind",
        Component %in% c("Wafers", "Cells", "Modules") ~ "Solar",
        TRUE ~ Sector
      )
    )
}

overcapacity_premium_build_overcapacity_indices <- function(overcapacity_clean, year = 2025L) {
  require_columns(
    overcapacity_clean,
    c("Sector", "Overcapacity Ratio"),
    label = "overcapacity_clean"
  )

  overcapacity_clean %>%
    dplyr::group_by(Sector) %>%
    dplyr::summarize(
      Overcapacity = mean(`Overcapacity Ratio`, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::transmute(
      tech = Sector,
      supply_chain = "Midstream",
      Overcapacity
    ) %>%
    dplyr::mutate(`Overall Overcapacity_index` = 2^(-Overcapacity)) %>%
    tidyr::pivot_longer(
      cols = c(Overcapacity, `Overall Overcapacity_index`),
      names_to = "variable",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      data_type = dplyr::if_else(stringr::str_ends(variable, "_index"), "index", "raw"),
      variable = stringr::str_remove(variable, "_index$"),
      category = "Technology Demand",
      Year = as.integer(year),
      source = "BNEF Energy Transition Supply Chains 2025",
      explanation = "Ratio of Midstream capacity to forecast"
    ) %>%
    dplyr::select(
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
}

overcapacity_premium_build_country_reference <- function(trade_tidy) {
  require_columns(trade_tidy, c("Country", "tech", "supply_chain"), label = "trade_tidy")

  trade_tidy %>%
    dplyr::select(Country, tech, supply_chain) %>%
    dplyr::distinct()
}

overcapacity_premium <- function(overcapacity_raw, trade_tidy, year = 2025L) {
  overcapacity_clean <- overcapacity_premium_build_overcapacity_clean(overcapacity_raw)
  overcapacity_indices <- overcapacity_premium_build_overcapacity_indices(
    overcapacity_clean,
    year = year
  )
  country_reference <- overcapacity_premium_build_country_reference(trade_tidy)

  assert_unique_keys(country_reference, c("Country", "tech", "supply_chain"), label = "overcapacity_country_reference")
  assert_unique_keys(
    overcapacity_indices %>% dplyr::distinct(tech, supply_chain),
    c("tech", "supply_chain"),
    label = "overcapacity_indices"
  )

  overcapacity_tidy <- country_reference %>%
    dplyr::left_join(
      overcapacity_indices,
      by = c("tech", "supply_chain"),
      relationship = "many-to-many"
    ) %>%
    dplyr::filter(!is.na(variable))

  output <- standardize_theme_table(overcapacity_tidy)
  validate_schema(output)
  output
}
