# Energy access & consumption theme.
energy_access_consumption <- function(ei, base_year = 2019, target_year = 2024, gamma = 0.5) {
  make_ec_long <- function(year) {
    ei %>%
      dplyr::filter(
        Year == year,
        Var %in% c("pop", "coalcons_ej", "oilcons_ej", "gascons_ej", "solar_ej", "wind_ej", "nuclear_ej"),
        !grepl("World|Other|Total|OECD|OPEC", Country)
      ) %>%
      dplyr::select(Country, Var, Value) %>%
      tidyr::pivot_wider(names_from = Var, values_from = Value) %>%
      dplyr::transmute(
        Country,
        coal_raw = coalcons_ej / pop,
        oil_raw = oilcons_ej / pop,
        gas_raw = gascons_ej / pop,
        solar_raw = solar_ej / pop,
        wind_raw = wind_ej / pop,
        nuclear_raw = nuclear_ej / pop
      ) %>%
      dplyr::mutate(dplyr::across(dplyr::ends_with("_raw"), ~dplyr::coalesce(.x, 0))) %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::ends_with("_raw"),
          ~median_scurve(.x, gamma = gamma),
          .names = "{stringr::str_remove(.col, '_raw')}_index"
        )
      ) %>%
      tidyr::pivot_longer(
        cols = -Country,
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
        Year = year,
        source = "EI Statistical Review of World Energy (2024)",
        explanation = dplyr::case_when(
          data_type == "raw" ~ stringr::str_glue("Per-capita {tech} consumption = {tech}cons_ej ÷ pop"),
          data_type == "index" ~ stringr::str_glue("Normalized index of per-capita {tech} consumption"),
          TRUE ~ NA_character_
        )
      )
  }

  ec_base <- make_ec_long(base_year)
  ec_target <- make_ec_long(target_year)

  ec_growth <- ec_base %>%
    dplyr::inner_join(
      ec_target,
      by = c("Country", "tech", "supply_chain", "category", "variable", "data_type"),
      suffix = c("_base", "_target")
    ) %>%
    dplyr::filter(data_type == "raw") %>%
    dplyr::mutate(growth_raw = (value_target - value_base) / value_base) %>%
    dplyr::group_by(Country) %>%
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
      source = "EI Statistical Review of World Energy (2024)",
      explanation = dplyr::case_when(
        data_type == "raw" ~ "Per-capita consumption growth between base and target years",
        data_type == "index" ~ "Normalized index of per-capita consumption growth",
        TRUE ~ NA_character_
      )
    )

  dplyr::bind_rows(ec_target, ec_growth)
}
