# Solar photovoltaic power potential theme builder functions.

solar_pv_potential_clean_raw <- function(pv_raw) {
  pv_raw %>%
    dplyr::transmute(
      Country = standardize_country_names(name_long),
      pvout_total = suppressWarnings(as.numeric(pvout_total)),
      pvout_mean = suppressWarnings(as.numeric(pvout_mean))
    ) %>%
    dplyr::filter(
      !is.na(Country),
      nzchar(Country),
      !is.na(pvout_total),
      !is.na(pvout_mean)
    )
}

solar_pv_potential_build_indices <- function(pv_clean, gamma = 0.5) {
  pv_clean %>%
    dplyr::mutate(
      solar_pot_tot_index = median_scurve(pvout_total, gamma = gamma),
      solar_pot_mean_index = median_scurve(pvout_mean, gamma = gamma),
      solar_pot_index = (solar_pot_tot_index + solar_pot_mean_index) / 2,
      solar_pot_index = median_scurve(solar_pot_index, gamma = gamma)
    )
}

solar_pv_potential_build_table <- function(pv_indices, year) {
  raw_tbl <- pv_indices %>%
    dplyr::select(Country, pvout_total, pvout_mean) %>%
    tidyr::pivot_longer(
      cols = c(pvout_total, pvout_mean),
      names_to = "metric",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      variable = dplyr::case_when(
        metric == "pvout_total" ~ "Solar PV power potential",
        metric == "pvout_mean" ~ "Solar PV power potential per sq km",
        TRUE ~ metric
      ),
      data_type = "raw"
    )

  index_tbl <- pv_indices %>%
    dplyr::select(Country, solar_pot_tot_index, solar_pot_mean_index, solar_pot_index) %>%
    tidyr::pivot_longer(
      cols = c(solar_pot_tot_index, solar_pot_mean_index, solar_pot_index),
      names_to = "metric",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      variable = dplyr::case_when(
        metric == "solar_pot_tot_index" ~ "Solar PV power potential",
        metric == "solar_pot_mean_index" ~ "Solar PV power potential per sq km",
        metric == "solar_pot_index" ~ "Overall Solar PV Potential Index",
        TRUE ~ metric
      ),
      data_type = "index"
    )

  dplyr::bind_rows(raw_tbl, index_tbl) %>%
    dplyr::transmute(
      Country,
      tech = "Solar",
      supply_chain = "Downstream",
      category = "Reserves",
      variable,
      data_type,
      value,
      Year = as.integer(year),
      source = "Global Solar Atlas (country GIS data)",
      explanation = dplyr::case_when(
        variable == "Solar PV power potential" & data_type == "raw" ~
          "Photovoltaic power potential (total) from GIS data",
        variable == "Solar PV power potential" & data_type == "index" ~
          "Normalized index of total photovoltaic power potential",
        variable == "Solar PV power potential per sq km" & data_type == "raw" ~
          "Photovoltaic power potential per square kilometer (GIS mean)",
        variable == "Solar PV power potential per sq km" & data_type == "index" ~
          "Normalized index of photovoltaic power potential per square kilometer",
        variable == "Overall Solar PV Potential Index" ~
          "Composite index based on total and per-area photovoltaic power potential",
        TRUE ~ NA_character_
      )
    )
}

solar_pv_potential <- function(pv_raw, year = 2023, gamma = 0.5) {
  pv_clean <- solar_pv_potential_clean_raw(pv_raw)
  pv_indices <- solar_pv_potential_build_indices(pv_clean, gamma = gamma)
  output <- solar_pv_potential_build_table(pv_indices, year = year)
  output <- energy_security_add_overall_index(output)

  output <- standardize_theme_table(output)
  validate_schema(output)
  output
}
