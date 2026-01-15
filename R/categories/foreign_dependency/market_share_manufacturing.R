# Market Share theme builder functions.

market_share_manufacturing_build_cleantech_midstream <- function(ei,
                                                                 cleantech_midstream,
                                                                 year = 2035L,
                                                                 gamma = 0.5) {
  require_columns(ei, c("Country", "EU"), label = "ei")
  require_columns(cleantech_midstream, "X", label = "cleantech_midstream")

  cleantech_long <- cleantech_midstream %>%
    dplyr::rename(Country = X) %>%
    tidyr::pivot_longer(
      cols = -Country,
      names_to = c("Technology", "Status"),
      names_pattern = "(.*)\\.\\.\\.(Current|Pipeline)",
      values_to = "Value"
    ) %>%
    dplyr::mutate(
      Year = dplyr::if_else(Status == "Current", 2024L, 2035L)
    )

  country_map <- ei %>%
    dplyr::distinct(Country, EU) %>%
    dplyr::mutate(
      Country_clean = dplyr::if_else(Country == "Vietnam", "Viet Nam", Country),
      country2 = dplyr::if_else(EU == 1, "EU", Country_clean)
    ) %>%
    dplyr::select(country2) %>%
    dplyr::distinct()

  assert_unique_keys(country_map, "country2", label = "country_map")

  ct_ms <- country_map %>%
    dplyr::left_join(
      cleantech_long,
      by = c("country2" = "Country"),
      relationship = "one-to-many"
    ) %>%
    dplyr::filter(Year == as.integer(year)) %>%
    dplyr::group_by(Technology) %>%
    dplyr::mutate(
      market_share = Value / sum(Value, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(Country = country2, Technology, market_share)

  ct_idx <- ct_ms %>%
    dplyr::group_by(Technology) %>%
    dplyr::mutate(
      market_share_index = median_scurve(market_share, gamma = gamma)
    ) %>%
    dplyr::ungroup()

  cleantech_clean <- ct_idx %>%
    tidyr::pivot_longer(
      cols = c(market_share, market_share_index),
      names_to = "name",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      tech = Technology,
      supply_chain = "Midstream",
      category = "Foreign Dependency",
      variable = "Market Share",
      data_type = dplyr::if_else(name == "market_share", "raw", "index"),
      Year = as.integer(year),
      source = "IEA Energy Technology Perspectives 2024",
      explanation = dplyr::case_when(
        data_type == "raw" ~ stringr::str_glue(
          "2035 market share for {tech} = Value ÷ global total for {tech}"
        ),
        data_type == "index" ~ stringr::str_glue(
          "Percent-rank of {tech} market share across countries"
        )
      )
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

  comp_ct <- ct_idx %>%
    dplyr::group_by(Country) %>%
    dplyr::summarize(
      mean_idx = mean(market_share_index, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      composite_index = median_scurve(mean_idx, gamma = gamma),
      tech = "Clean Tech Midstream",
      supply_chain = "Midstream",
      category = "Foreign Dependency",
      variable = "Market Share",
      data_type = "index",
      Year = as.integer(year),
      source = "IEA Energy Technology Perspectives 2024",
      explanation = "Percent-rank of mean of individual clean-tech Midstream indexes(NB: All EU countries are given same market share)"
    ) %>%
    dplyr::select(
      Country,
      tech,
      supply_chain,
      category,
      variable,
      data_type,
      value = composite_index,
      Year,
      source,
      explanation
    )

  cleantech_final <- dplyr::bind_rows(
    lapply(list(cleantech_clean, comp_ct), standardize_bind_rows_inputs)
  ) %>%
    dplyr::mutate(Country = dplyr::if_else(Country == "US", "United States", Country))

  ei %>%
    dplyr::distinct(Country, EU) %>%
    dplyr::mutate(
      Country = dplyr::if_else(Country == "Vietnam", "Viet Nam", Country),
      country2 = dplyr::if_else(EU == 1, "EU", Country),
      country2 = dplyr::if_else(country2 == "US", "United States", country2)
    ) %>%
    dplyr::left_join(
      cleantech_final,
      by = c("country2" = "Country"),
      relationship = "many-to-many"
    ) %>%
    dplyr::filter(!is.na(tech)) %>%
    dplyr::select(-country2, -EU) %>%
    dplyr::mutate(Country = dplyr::if_else(Country == "US", "United States", Country))
}

market_share_manufacturing_build_ev_midstream <- function(ei,
                                                          ev_midstream,
                                                          year = 2024L,
                                                          gamma = 0.5) {
  require_columns(ei, c("Country", "EU", "SubRegion"), label = "ei")
  require_columns(
    ev_midstream,
    c("Year", "Region", "Domestic.Production", "Domestic.sales", "import_share", "market_share"),
    label = "ev_midstream"
  )

  eu_map <- ei %>%
    dplyr::distinct(Country, EU)
  assert_unique_keys(eu_map, c("Country", "EU"), label = "ei_eu_map")

  ev_midstream %>%
    dplyr::filter(Year == as.character(year)) %>%
    dplyr::mutate(EU = dplyr::if_else(Region == "European Union", 1, 2)) %>%
    dplyr::left_join(eu_map, by = c("EU"), relationship = "many-to-many") %>%
    dplyr::mutate(
      SubRegion = dplyr::if_else(
        Region == "North America",
        "North America",
        dplyr::if_else(
          Region == "Other Asia Pacific",
          "Asia Pacific",
          dplyr::if_else(
            Region == "European Union",
            "EU",
            dplyr::if_else(Region == "China", "CHN", "Rest of World")
          )
        )
      )
    ) %>%
    dplyr::left_join(
      ei %>%
        dplyr::filter(Country != "China") %>%
        dplyr::distinct(Country, SubRegion),
      by = c("SubRegion"),
      relationship = "many-to-many"
    ) %>%
    dplyr::left_join(
      ei %>%
        dplyr::distinct(Country, SubRegion) %>%
        dplyr::mutate(
          SubRegion = dplyr::if_else(
            SubRegion %in% c("Asia Pacific", "North America", "Europe"),
            "boop",
            "Rest of World"
          )
        ),
      by = c("SubRegion"),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      country = dplyr::if_else(
        Region == "China",
        "China",
        dplyr::if_else(
          SubRegion == "North America",
          Country.y,
          dplyr::if_else(
            SubRegion == "Asia Pacific",
            Country.y,
            dplyr::if_else(EU == 1, Country.x, Country)
          )
        )
      )
    ) %>%
    dplyr::mutate(
      production_index = median_scurve(Domestic.Production, gamma = gamma),
      sales_index = median_scurve(Domestic.sales, gamma = gamma),
      import_index = median_scurve(import_share, gamma = gamma),
      market_share_index = median_scurve(market_share, gamma = gamma)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ev_Midstream_index = mean(dplyr::c_across(production_index:market_share_index))) %>%
    dplyr::ungroup() %>%
    dplyr::select(country, Domestic.sales:market_share, production_index:ev_Midstream_index) %>%
    tidyr::pivot_longer(
      cols = -country,
      names_to = "variable",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      tech = "Electric Vehicles",
      supply_chain = "Midstream",
      category = "Foreign Dependency",
      data_type = dplyr::if_else(stringr::str_detect(variable, "_index$"), "index", "raw"),
      variable = stringr::str_remove(variable, "_index$"),
      Year = as.integer(year),
      source = "IEA EV Outlook",
      explanation = "Combined index of domestic sales, production, imports and market share"
    ) %>%
    dplyr::rename(Country = country) %>%
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
    ) %>%
    dplyr::distinct()
}

market_share_manufacturing <- function(ei,
                                       cleantech_midstream,
                                       ev_midstream,
                                       year = list(cleantech = 2035L, ev = 2024L),
                                       gamma = 0.5) {
  cleantech_tbl <- market_share_manufacturing_build_cleantech_midstream(
    ei = ei,
    cleantech_midstream = cleantech_midstream,
    year = year$cleantech,
    gamma = gamma
  ) %>%
    dplyr::mutate(
      variable = dplyr::if_else(variable == "Market Share", "Overall Market Share", variable)
    )

  ev_tbl <- market_share_manufacturing_build_ev_midstream(
    ei = ei,
    ev_midstream = ev_midstream,
    year = year$ev,
    gamma = gamma
  ) %>%
    dplyr::mutate(
      variable = dplyr::if_else(variable == "ev_Midstream", "Overall Production", variable)
    )

  standardized <- lapply(list(cleantech_tbl, ev_tbl), standardize_bind_rows_inputs)

  output <- dplyr::bind_rows(standardized)
  output <- standardize_theme_table(output)
  validate_schema(output)
  output
}
