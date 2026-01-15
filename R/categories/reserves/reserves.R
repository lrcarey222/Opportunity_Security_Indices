# Reserves theme builder functions.
reserves_specs <- function() {
  list(
    list(
      sheet = 13,
      skip = 4,
      nm_col = "Thousand million barrels",
      val_col = "2020...42",
      tech_name = "Oil",
      unit_desc = "Thousand million barrels"
    ),
    list(
      sheet = 32,
      skip = 4,
      nm_col = "Trillion cubic metres",
      val_col = "2020...42",
      tech_name = "Gas",
      unit_desc = "Trillion cubic metres"
    ),
    list(
      sheet = 46,
      skip = 5,
      nm_col = "Million tonnes",
      val_col = "Total",
      tech_name = "Coal",
      unit_desc = "Million tonnes"
    ),
    list(
      sheet = 83,
      skip = 2,
      nm_col = "Thousand tonnes",
      val_col = "At end of 2024",
      tech_name = "Cobalt",
      unit_desc = "Thousand tonnes"
    ),
    list(
      sheet = 84,
      skip = 2,
      nm_col = "Thousand tonnes of Lithium content",
      val_col = "At end of 2024",
      tech_name = "Lithium",
      unit_desc = "Thousand tonnes"
    ),
    list(
      sheet = 85,
      skip = 2,
      nm_col = "Thousand tonnes",
      val_col = "At end of 2024",
      tech_name = "Graphite",
      unit_desc = "Thousand tonnes"
    ),
    list(
      sheet = 86,
      skip = 2,
      nm_col = "Thousand tonnes1",
      val_col = "At end of 2024",
      tech_name = "Rare Earths",
      unit_desc = "Thousand tonnes"
    ),
    list(
      sheet = 87,
      skip = 2,
      nm_col = "Thousand tonnes",
      val_col = "At end of 2024",
      tech_name = "Copper",
      unit_desc = "Thousand tonnes"
    ),
    list(
      sheet = 88,
      skip = 2,
      nm_col = "Thousand tonnes",
      val_col = "At end of 2024",
      tech_name = "Manganese",
      unit_desc = "Thousand tonnes"
    ),
    list(
      sheet = 89,
      skip = 2,
      nm_col = "Thousand tonnes",
      val_col = "At end of 2024",
      tech_name = "Nickel",
      unit_desc = "Thousand tonnes"
    ),
    list(
      sheet = 90,
      skip = 2,
      nm_col = "Thousand tonnes",
      val_col = "At end of 2024",
      tech_name = "Zinc",
      unit_desc = "Thousand tonnes"
    ),
    list(
      sheet = 91,
      skip = 2,
      nm_col = "Thousand tonnes",
      val_col = "At end of 2024",
      tech_name = "PGMs",
      unit_desc = "Thousand tonnes"
    )
  )
}

reserves_build_country_reference <- function(ei, year = 2024) {
  ei %>%
    dplyr::filter(
      Year == year,
      !grepl("World|Other|Total|OECD|OPEC", Country)
    ) %>%
    dplyr::mutate(Country = standardize_country_names(Country)) %>%
    dplyr::distinct(ISO3166_alpha3, Country)
}

reserves_country_names <- function(country_reference) {
  country_reference$Country
}

reserves_build_mineral_demand_clean <- function(critical) {
  mineral_demand <- critical %>%
    dplyr::filter(
      Pillar == "3.1 Cleantech demand by tech",
      !grepl("Other|Total", `Sector.Country`)
    ) %>%
    dplyr::mutate(growth = X2035 / X2024 - 1) %>%
    dplyr::group_by(Pillar, Mineral) %>%
    dplyr::mutate(
      share_24 = X2024 / sum(X2024),
      share_35 = X2035 / sum(X2035)
    ) %>%
    dplyr::select(1:3, X2024, X2035, growth, share_24, share_35) %>%
    dplyr::ungroup()

  mineral_demand %>%
    dplyr::mutate(
      tech = dplyr::case_when(
        stringr::str_detect(`Sector.Country`, stringr::regex("^Solar", ignore_case = TRUE)) ~ "Solar",
        stringr::str_detect(`Sector.Country`, stringr::regex("^Wind", ignore_case = TRUE)) ~ "Wind",
        stringr::str_detect(`Sector.Country`, stringr::regex("Electric vehicles?", ignore_case = TRUE)) ~ "Electric Vehicles",
        stringr::str_detect(`Sector.Country`, stringr::regex("Grid battery storage", ignore_case = TRUE)) ~ "Batteries",
        stringr::str_detect(`Sector.Country`, stringr::regex("Electricity networks?", ignore_case = TRUE)) ~ "Electric Grid",
        stringr::str_detect(`Sector.Country`, stringr::regex("Hydrogen", ignore_case = TRUE)) ~ "Green Hydrogen",
        stringr::str_detect(`Sector.Country`, stringr::regex("Heat pumps?", ignore_case = TRUE)) ~ "Heat Pumps",
        stringr::str_detect(`Sector.Country`, stringr::regex("^Coal$", ignore_case = TRUE)) ~ "Coal",
        stringr::str_detect(`Sector.Country`, stringr::regex("^Nuclear$", ignore_case = TRUE)) ~ "Nuclear",
        stringr::str_detect(`Sector.Country`, stringr::regex("^Oil$", ignore_case = TRUE)) ~ "Oil",
        stringr::str_detect(`Sector.Country`, stringr::regex("^Gas$", ignore_case = TRUE)) ~ "Gas",
        stringr::str_detect(`Sector.Country`, stringr::regex("Hydroelectric", ignore_case = TRUE)) ~ "Hydroelectric Power",
        stringr::str_detect(`Sector.Country`, stringr::regex("Geothermal", ignore_case = TRUE)) ~ "Geothermal",
        TRUE ~ NA_character_
      ),
      Mineral = dplyr::if_else(stringr::str_detect(Mineral, "Graphite"), "Graphite", Mineral)
    )
}

reserves_build_reserve_table <- function(sheet_data,
                                         nm_col,
                                         val_col,
                                         tech_name,
                                         unit_desc,
                                         sheet_id,
                                         country_reference,
                                         year = 2024,
                                         gamma = 0.5) {
  country_reference <- country_reference %>%
    dplyr::filter(!is.na(ISO3166_alpha3), nzchar(ISO3166_alpha3)) %>%
    dplyr::distinct(ISO3166_alpha3, Country)

  raw_inputs <- sheet_data %>%
    dplyr::rename(
      Country = dplyr::all_of(nm_col),
      raw_value = dplyr::all_of(val_col)
    ) %>%
    dplyr::mutate(
      Country = standardize_country_names(Country),
      Country = dplyr::case_when(
        Country %in% c("Rest of World", "Rest of world", "Rest of World^") ~ "Rest of World",
        Country == "US" ~ "United States",
        Country == "DR Congo" ~ "Democratic Republic of Congo",
        Country == "Russia Federation" ~ "Russia",
        TRUE ~ Country
      ),
      raw_value = as.numeric(raw_value)
    ) %>%
    dplyr::filter(!is.na(Country))

  unmapped <- raw_inputs %>%
    dplyr::filter(
      !grepl("Total World|Other|OECD|OPEC|Orinoco", Country),
      Country != "Rest of World"
    ) %>%
    dplyr::distinct(Country) %>%
    dplyr::anti_join(country_reference, by = "Country")

  if (nrow(unmapped) > 0) {
    warning(
      "Reserves sheet ",
      sheet_id,
      " contains countries without ISO3 mappings after standardization: ",
      paste(sort(unmapped$Country), collapse = ", ")
    )
  }

  raw <- raw_inputs %>%
    dplyr::filter(
      !grepl("Total World|Other|OECD|OPEC|Orinoco", Country),
      Country != "Rest of World"
    ) %>%
    dplyr::inner_join(country_reference, by = "Country") %>%
    dplyr::select(ISO3166_alpha3, raw_value)

  dummy_zero <- tibble::tibble(ISO3166_alpha3 = "_ZERO_", raw_value = 0)

  dplyr::bind_rows(
    lapply(list(raw, dummy_zero), standardize_bind_rows_inputs)
  ) %>%
    dplyr::mutate(index_value = median_scurve(raw_value, gamma = gamma)) %>%
    dplyr::filter(ISO3166_alpha3 != "_ZERO_") %>%
    tidyr::complete(
      ISO3166_alpha3 = country_reference$ISO3166_alpha3
    ) %>%
    dplyr::left_join(country_reference, by = "ISO3166_alpha3") %>%
    tidyr::pivot_longer(
      c(raw_value, index_value),
      names_to = "data_type",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      tech = tech_name,
      supply_chain = "Upstream",
      category = "Reserves",
      variable = stringr::str_glue("{tech_name} Reserves"),
      data_type = dplyr::if_else(data_type == "raw_value", "raw", "index"),
      Year = year,
      source = "EI Statistical Review of World Energy (2024)",
      explanation = dplyr::case_when(
        data_type == "raw" ~ stringr::str_glue("{tech_name} reserves ({unit_desc}) from sheet {sheet_id}"),
        data_type == "index" ~ "Percent-rank of reserves across reporting entities (countries + RoW)",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(
      Country,
      ISO3166_alpha3,
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

reserves_build_critical_mineral_reserves <- function(mineral_reserves,
                                                     mineral_demand_clean,
                                                     country_reference,
                                                     year = 2024) {
  country_reference <- country_reference %>%
    dplyr::filter(!is.na(ISO3166_alpha3), nzchar(ISO3166_alpha3)) %>%
    dplyr::distinct(ISO3166_alpha3, Country)

  critical_min_reserves <- dplyr::bind_rows(
    lapply(mineral_reserves, standardize_bind_rows_inputs)
  ) %>%
    dplyr::rename(Mineral = "tech") %>%
    dplyr::select(-Country) %>%
    dplyr::inner_join(
      mineral_demand_clean %>%
        dplyr::ungroup() %>%
        dplyr::select(Mineral, tech, share_24) %>%
        dplyr::mutate(
          Mineral = dplyr::if_else(
            stringr::str_detect(Mineral, stringr::regex("rare", ignore_case = TRUE)),
            "Rareearths",
            dplyr::if_else(Mineral == "Battery-grade graphite", "Graphite", Mineral)
          )
        ),
      by = c("Mineral")
    ) %>%
    dplyr::filter(!is.na(tech)) %>%
    dplyr::group_by(ISO3166_alpha3, tech, data_type) %>%
    dplyr::mutate(share_24 = share_24 / sum(share_24)) %>%
    dplyr::ungroup()

  critical_min_reserves %>%
    dplyr::filter(data_type == "index") %>%
    dplyr::group_by(ISO3166_alpha3, tech, supply_chain, category, data_type, source) %>%
    dplyr::summarize(value = stats::weighted.mean(value, w = share_24, na.rm = TRUE)) %>%
    dplyr::mutate(
      variable = stringr::str_glue("{tech} Reserves"),
      explanation = "Weighted average of reserve availability by share of demand in technology",
      Year = year
    ) %>%
    dplyr::filter(value > 0) %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(value = median_scurve(value)) %>%
    {
      dplyr::bind_rows(
        standardize_bind_rows_inputs(.),
        standardize_bind_rows_inputs(
          critical_min_reserves %>%
            dplyr::select(-tech) %>%
            dplyr::rename(tech = "Mineral") %>%
            dplyr::distinct(
              ISO3166_alpha3,
              tech,
              supply_chain,
              category,
              data_type,
              variable,
              value,
              Year,
              source,
              explanation
            )
        )
      )
    } %>%
    dplyr::group_by(tech, supply_chain, category, data_type, source, variable, explanation, Year) %>%
    tidyr::complete(ISO3166_alpha3 = country_reference$ISO3166_alpha3) %>%
    dplyr::left_join(country_reference, by = "ISO3166_alpha3")
}

reserves_build_clean_table <- function(reserve_tables, country_reference) {
  country_reference <- country_reference %>%
    dplyr::filter(!is.na(ISO3166_alpha3), nzchar(ISO3166_alpha3)) %>%
    dplyr::distinct(ISO3166_alpha3, Country)

  dplyr::bind_rows(
    lapply(reserve_tables, standardize_bind_rows_inputs)
  ) %>%
    dplyr::select(-Country) %>%
    dplyr::group_by(tech, supply_chain, category, data_type, source, variable, explanation, Year) %>%
    tidyr::complete(ISO3166_alpha3 = country_reference$ISO3166_alpha3) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(country_reference, by = "ISO3166_alpha3") %>%
    dplyr::select(Country, ISO3166_alpha3, dplyr::everything())
}

reserves <- function(ei, reserve_inputs, mineral_demand_clean, year = 2024, gamma = 0.5) {
  country_reference <- reserves_build_country_reference(ei, year = year)

  reserve_tables <- lapply(reserve_inputs, function(spec) {
    reserves_build_reserve_table(
      sheet_data = spec$data,
      nm_col = spec$nm_col,
      val_col = spec$val_col,
      tech_name = spec$tech_name,
      unit_desc = spec$unit_desc,
      sheet_id = spec$sheet,
      country_reference = country_reference,
      year = year,
      gamma = gamma
    )
  })

  mineral_reserves <- lapply(
    reserve_tables,
    function(tbl) if (tbl$tech[[1]] %in% c("Cobalt", "Lithium", "Copper", "Rare Earths", "Graphite",
                                          "Manganese", "Zinc", "Nickel", "PGMs")) tbl else NULL
  )
  mineral_reserves <- Filter(Negate(is.null), mineral_reserves)

  energy_reserves <- lapply(
    reserve_tables,
    function(tbl) if (tbl$tech[[1]] %in% c("Oil", "Gas", "Coal")) tbl else NULL
  )
  energy_reserves <- Filter(Negate(is.null), energy_reserves)

  critical_min_reserves <- reserves_build_critical_mineral_reserves(
    mineral_reserves = mineral_reserves,
    mineral_demand_clean = mineral_demand_clean,
    country_reference = country_reference,
    year = year
  )

  reserves_build_clean_table(
    reserve_tables = c(list(critical_min_reserves), energy_reserves),
    country_reference = country_reference
  ) %>%
    energy_security_add_overall_index()
}
