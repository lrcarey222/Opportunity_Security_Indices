# Geothermal potential theme builder functions.

geothermal_potential_clean_raw <- function(geo_raw, country_info = NULL) {
  geo_clean <- geo_raw %>%
    dplyr::transmute(
      iso3 = toupper(as.character(iso3)),
      lcoe_for_500mw = suppressWarnings(as.numeric(lcoe_for_500mw)),
      lcoe_for_5gw = suppressWarnings(as.numeric(lcoe_for_5gw)),
      total_mw = suppressWarnings(as.numeric(total_mw)),
      lcoe_500mw_index = suppressWarnings(as.numeric(lcoe_500mw_index)),
      lcoe_5gw_index = suppressWarnings(as.numeric(lcoe_5gw_index)),
      total_mw_index = suppressWarnings(as.numeric(total_mw_index)),
      geothermal_index = suppressWarnings(as.numeric(geothermal_index))
    ) %>%
    dplyr::filter(!is.na(iso3), nzchar(iso3))

  if (!is.null(country_info)) {
    require_columns(country_info, c("iso3c", "country"), label = "country_info")
    country_map <- country_info %>%
      dplyr::select(iso3c, country) %>%
      dplyr::distinct()
    geo_clean <- geo_clean %>%
      dplyr::left_join(country_map, by = c("iso3" = "iso3c")) %>%
      dplyr::transmute(
        Country = standardize_country_names(country),
        lcoe_for_500mw,
        lcoe_for_5gw,
        total_mw,
        lcoe_500mw_index,
        lcoe_5gw_index,
        total_mw_index,
        geothermal_index
      )
  } else {
    geo_clean <- geo_clean %>%
      dplyr::transmute(
        Country = standardize_country_names(iso3),
        lcoe_for_500mw,
        lcoe_for_5gw,
        total_mw,
        lcoe_500mw_index,
        lcoe_5gw_index,
        total_mw_index,
        geothermal_index
      )
  }

  geo_clean %>%
    dplyr::filter(
      !is.na(Country),
      nzchar(Country),
      !is.na(lcoe_for_500mw) |
        !is.na(lcoe_for_5gw) |
        !is.na(total_mw) |
        !is.na(lcoe_500mw_index) |
        !is.na(lcoe_5gw_index) |
        !is.na(total_mw_index) |
        !is.na(geothermal_index)
    )
}

geothermal_potential_build_table <- function(geo_clean, year) {
  raw_tbl <- geo_clean %>%
    dplyr::select(Country, lcoe_for_500mw, lcoe_for_5gw, total_mw) %>%
    tidyr::pivot_longer(
      cols = c(lcoe_for_500mw, lcoe_for_5gw, total_mw),
      names_to = "metric",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      variable = dplyr::case_when(
        metric == "lcoe_for_500mw" ~ "Geothermal LCOE (500 MW)",
        metric == "lcoe_for_5gw" ~ "Geothermal LCOE (5 GW)",
        metric == "total_mw" ~ "Geothermal total potential (MW)",
        TRUE ~ metric
      ),
      data_type = "raw"
    )

  index_tbl <- geo_clean %>%
    dplyr::select(
      Country,
      lcoe_500mw_index,
      lcoe_5gw_index,
      total_mw_index,
      geothermal_index
    ) %>%
    tidyr::pivot_longer(
      cols = c(lcoe_500mw_index, lcoe_5gw_index, total_mw_index, geothermal_index),
      names_to = "metric",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      variable = dplyr::case_when(
        metric == "lcoe_500mw_index" ~ "Geothermal LCOE (500 MW)",
        metric == "lcoe_5gw_index" ~ "Geothermal LCOE (5 GW)",
        metric == "total_mw_index" ~ "Geothermal total potential (MW)",
        metric == "geothermal_index" ~ "Overall Geothermal Potential Index",
        TRUE ~ metric
      ),
      data_type = "index"
    )

  dplyr::bind_rows(raw_tbl, index_tbl) %>%
    dplyr::transmute(
      Country,
      tech = "Geothermal",
      supply_chain = "Downstream",
      category = "Reserves",
      variable,
      data_type,
      value,
      Year = as.integer(year),
      source = paste(
        "Franzmann, David; Heinrichs, Heidi; Stolten, Detlef (2025),",
        "\u201cGlobal Technical Energy Potentials of Enhanced Geothermal Systems\u201d,",
        "Mendeley Data, V2, doi: 10.17632/sbs8k66bwf.2"
      ),
      explanation = dplyr::case_when(
        variable == "Geothermal LCOE (500 MW)" & data_type == "raw" ~
          "Levelized cost of geothermal energy for a 500 MW resource",
        variable == "Geothermal LCOE (500 MW)" & data_type == "index" ~
          "Normalized index of geothermal LCOE for a 500 MW resource",
        variable == "Geothermal LCOE (5 GW)" & data_type == "raw" ~
          "Levelized cost of geothermal energy for a 5 GW resource",
        variable == "Geothermal LCOE (5 GW)" & data_type == "index" ~
          "Normalized index of geothermal LCOE for a 5 GW resource",
        variable == "Geothermal total potential (MW)" & data_type == "raw" ~
          "Estimated total geothermal potential in megawatts",
        variable == "Geothermal total potential (MW)" & data_type == "index" ~
          "Normalized index of total geothermal potential in megawatts",
        variable == "Overall Geothermal Potential Index" ~
          "Composite index based on geothermal LCOE and total potential",
        TRUE ~ NA_character_
      )
    )
}

geothermal_potential <- function(geo_raw, country_info = NULL, year = 2023) {
  geo_clean <- geothermal_potential_clean_raw(geo_raw, country_info = country_info)
  output <- geothermal_potential_build_table(geo_clean, year = year)
  output <- energy_security_add_overall_index(output)

  output <- standardize_theme_table(output)
  validate_schema(output)
  output
}
