# Energy access & consumption theme.
energy_access_clean_raw <- function(ei) {
  ei %>%
    dplyr::mutate(
      Country = dplyr::if_else(Country == "US", "United States", Country)
    )
}

energy_access_build_per_capita <- function(ei_clean, year, country_info = NULL) {
  per_capita_tbl <- ei_clean %>%
    dplyr::filter(
      Year == year,
      Var %in% c("pop", "coal_tes_ej", "oil_tes_ej", "gas_tes_ej", "solar_tes_ej", "wind_tes_ej", "nuclear_tes_ej"),
      !grepl("World|Other|Total|OECD|OPEC", Country)
    ) %>%
    dplyr::select(Country, Var, Value) %>%
    tidyr::pivot_wider(names_from = Var, values_from = Value) %>%
    dplyr::transmute(
      Country,
      coal_raw = coal_tes_ej / pop,
      oil_raw = oil_tes_ej / pop,
      gas_raw = gas_tes_ej / pop,
      solar_raw = solar_tes_ej / pop,
      wind_raw = wind_tes_ej / pop,
      nuclear_raw = nuclear_tes_ej / pop
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("_raw"), ~tidyr::replace_na(.x, 0)))

  # Keep this builder resilient to country reference mismatches; enforce iso3c
  # later in the shared theme standardization flow.
  standardize_country_table(per_capita_tbl, country_info = NULL) %>%
    dplyr::select(-dplyr::any_of("iso3c"))
}

energy_access_build_indices <- function(ec_per_capita, year, gamma = 0.5) {
  ec_per_capita %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::ends_with("_raw"),
        ~median_scurve(.x, gamma = gamma),
        .names = "{stringr::str_remove(.col, '_raw')}_index"
      )
    ) %>%
    tidyr::pivot_longer(
      cols = tidyselect::matches("_(raw|index)$"),
      names_to = c("tech", "data_type"),
      names_pattern = "(.*)_(raw|index)",
      values_to = "value"
    ) %>%
    dplyr::transmute(
      Country,
      tech = stringr::str_to_sentence(tech),
      supply_chain = "Downstream",
      category = "Energy Access",
      variable = "Energy consumption per capita",
      data_type,
      value,
      Year = as.character(year),
      source = "EI Statistical Review of World Energy (2025)",
      explanation = dplyr::case_when(
        data_type == "raw" ~ stringr::str_glue(
          "Per-capita {tech} consumption = {stringr::str_to_lower(tech)}_tes_ej / pop"
        ),
        data_type == "index" ~ stringr::str_glue("Normalized index of per-capita {tech} consumption"),
        TRUE ~ NA_character_
      )
    )
}

energy_access_build_growth <- function(ec_base, ec_target, base_year, target_year, gamma = 0.5) {
  ec_base %>%
    dplyr::inner_join(
      ec_target,
      by = c("Country", "tech", "supply_chain", "category", "variable", "data_type"),
      suffix = c("_base", "_target")
    ) %>%
    dplyr::filter(data_type == "raw") %>%
    dplyr::mutate(growth_raw = (value_target - value_base) / value_base) %>%
    # Normalize each tech's growth ACROSS countries, matching how the level index in
    # energy_access_build_indices() scales each tech column. Grouping by Country instead
    # ranks a country's fuels against each other, which hands every country a 1.000 for
    # whichever of its own fuels grew fastest.
    dplyr::group_by(tech) %>%
    dplyr::mutate(growth_index = median_scurve(growth_raw, gamma = gamma)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-data_type) %>%
    tidyr::pivot_longer(
      cols = c(growth_raw, growth_index),
      names_to = c("metric", "data_type"),
      names_pattern = "(.*)_(raw|index)",
      values_to = "value"
    ) %>%
    dplyr::transmute(
      Country,
      tech = stringr::str_to_sentence(tech),
      supply_chain = "Downstream",
      category = "Energy Access",
      variable = paste(variable, metric),
      data_type,
      value,
      Year = paste0(base_year, "-", target_year),
      source = "EI Statistical Review of World Energy (2025)",
      explanation = dplyr::case_when(
        data_type == "raw" ~ stringr::str_glue("{base_year}-{target_year} growth of per-capita consumption"),
        data_type == "index" ~ "Normalized index of per-capita consumption growth",
        TRUE ~ NA_character_
      )
    )
}

energy_access_consumption <- function(ei,
                                      country_info = NULL,
                                      base_year = 2020,
                                      target_year = 2025,
                                      gamma = 0.5) {
  ei_clean <- energy_access_clean_raw(ei)

  ec_base <- energy_access_build_indices(
    energy_access_build_per_capita(ei_clean, base_year, country_info = country_info),
    base_year,
    gamma = gamma
  )
  ec_target <- energy_access_build_indices(
    energy_access_build_per_capita(ei_clean, target_year, country_info = country_info),
    target_year,
    gamma = gamma
  )

  ec_growth <- energy_access_build_growth(ec_base, ec_target, base_year, target_year, gamma = gamma)

  standardized <- lapply(
    list(ec_target, ec_growth),
    standardize_bind_rows_inputs
  )

  energy_security_add_overall_index(
    dplyr::bind_rows(standardized)
  )
}
