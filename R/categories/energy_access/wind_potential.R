# Wind power potential theme builder functions.

wind_potential_clean_raw <- function(wind_raw) {
  wind_raw %>%
    dplyr::rename(Country=name_long) %>%
    dplyr::transmute(
      Country = standardize_country_names(Country),
      pd_total_MW_equiv = suppressWarnings(as.numeric(pd_total_MW_equiv)),
      pd_mean_Wm2 = suppressWarnings(as.numeric(pd_mean_Wm2)),
      pd_share_over_threshold = suppressWarnings(as.numeric(pd_share_over_threshold))
    ) %>%
    dplyr::filter(
      !is.na(Country),
      nzchar(Country),
      !is.na(pd_total_MW_equiv),
      !is.na(pd_mean_Wm2),
      !is.na(pd_share_over_threshold)
    )
}

wind_potential_build_indices <- function(wind_clean, gamma = 0.5) {
  wind_clean %>%
    dplyr::mutate(
      wind_total_index = median_scurve(pd_total_MW_equiv, gamma = gamma),
      wind_mean_index = median_scurve(pd_mean_Wm2, gamma = gamma),
      wind_threshold_index = median_scurve(pd_share_over_threshold, gamma = gamma),
      wind_overall_index = rowMeans(
        cbind(wind_total_index, wind_mean_index, wind_threshold_index),
        na.rm = TRUE
      )
    )
}

wind_potential_build_table <- function(wind_indices, year) {
  raw_tbl <- wind_indices %>%
    dplyr::select(Country, pd_total_MW_equiv, pd_mean_Wm2, pd_share_over_threshold) %>%
    tidyr::pivot_longer(
      cols = c(pd_total_MW_equiv, pd_mean_Wm2, pd_share_over_threshold),
      names_to = "metric",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      variable = dplyr::case_when(
        metric == "pd_total_MW_equiv" ~ "Wind power potential",
        metric == "pd_mean_Wm2" ~ "Wind power potential per sq km",
        metric == "pd_share_over_threshold" ~ "Wind power potential share over threshold",
        TRUE ~ metric
      ),
      data_type = "raw"
    )

  index_tbl <- wind_indices %>%
    dplyr::select(Country, wind_total_index, wind_mean_index, wind_threshold_index, wind_overall_index) %>%
    tidyr::pivot_longer(
      cols = c(wind_total_index, wind_mean_index, wind_threshold_index, wind_overall_index),
      names_to = "metric",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      variable = dplyr::case_when(
        metric == "wind_total_index" ~ "Wind power potential",
        metric == "wind_mean_index" ~ "Wind power potential per sq km",
        metric == "wind_threshold_index" ~ "Wind power potential share over threshold",
        metric == "wind_overall_index" ~ "Wind power potential index",
        TRUE ~ metric
      ),
      data_type = "index"
    )

  dplyr::bind_rows(raw_tbl, index_tbl) %>%
    dplyr::transmute(
      Country,
      tech = "Wind",
      supply_chain = "Downstream",
      category = "Energy Access",
      variable,
      data_type,
      value,
      Year = as.integer(year),
      source = "Global Wind Atlas (country data)",
      explanation = dplyr::case_when(
        variable == "Wind power potential" & data_type == "raw" ~
          "Wind power potential (total) from country data",
        variable == "Wind power potential" & data_type == "index" ~
          "Normalized index of total wind power potential",
        variable == "Wind power potential per sq km" & data_type == "raw" ~
          "Wind power potential per square kilometer (mean power density)",
        variable == "Wind power potential per sq km" & data_type == "index" ~
          "Normalized index of wind power potential per square kilometer",
        variable == "Wind power potential share over threshold" & data_type == "raw" ~
          "Share of area above wind power density threshold",
        variable == "Wind power potential share over threshold" & data_type == "index" ~
          "Normalized index of share above wind power density threshold",
        variable == "Wind power potential index" ~
          "Composite index based on total, mean, and threshold wind potential",
        TRUE ~ NA_character_
      )
    )
}

wind_potential <- function(wind_raw, year = 2023, gamma = 0.5) {
  wind_clean <- wind_potential_clean_raw(wind_raw)
  wind_indices <- wind_potential_build_indices(wind_clean, gamma = gamma)
  output <- wind_potential_build_table(wind_indices, year = year)

  output <- standardize_theme_table(output)
  validate_schema(output)
  output
}
