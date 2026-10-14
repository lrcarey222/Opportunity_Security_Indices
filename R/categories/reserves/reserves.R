# Reserves theme builder functions.
#
# Sheets are addressed by name rather than by position: the EI workbook adds and drops
# sheets between releases (the 2026 edition inserted "Data Centre Demand" and "SAF prices"
# ahead of the mineral block), so positional indices silently read the wrong sheet.
reserves_specs <- function() {
  list(
    list(
      sheet = "Oil - Proved reserves history",
      skip = 4,
      nm_col = "Thousand million barrels",
      val_col = "2020...42",
      tech_name = "Oil",
      unit_desc = "Thousand million barrels"
    ),
    list(
      sheet = "Gas - Proved reserves history",
      skip = 4,
      nm_col = "Trillion cubic metres",
      val_col = "2020...42",
      tech_name = "Gas",
      unit_desc = "Trillion cubic metres"
    ),
    list(
      sheet = "Coal - Reserves",
      skip = 5,
      nm_col = "Million tonnes",
      val_col = "Total",
      tech_name = "Coal",
      unit_desc = "Million tonnes"
    ),
    list(
      sheet = "Cobalt P-R",
      skip = 2,
      nm_col = "Thousand tonnes",
      val_col = "At end of 2025",
      tech_name = "Cobalt",
      unit_desc = "Thousand tonnes"
    ),
    list(
      sheet = "Lithium P-R",
      skip = 2,
      nm_col = "Thousand tonnes of Lithium content",
      val_col = "At end of 2025",
      tech_name = "Lithium",
      unit_desc = "Thousand tonnes"
    ),
    list(
      sheet = "Natural Graphite P-R",
      skip = 2,
      nm_col = "Thousand tonnes",
      val_col = "At end of 2025",
      tech_name = "Graphite",
      unit_desc = "Thousand tonnes"
    ),
    list(
      sheet = "Rare Earth metals P-R",
      skip = 2,
      nm_col = "Thousand tonnes1",
      val_col = "At end of 2025",
      tech_name = "Rare Earths",
      unit_desc = "Thousand tonnes"
    ),
    list(
      sheet = "Copper P-R",
      skip = 2,
      nm_col = "Thousand tonnes",
      val_col = "At end of 2025",
      tech_name = "Copper",
      unit_desc = "Thousand tonnes"
    ),
    list(
      sheet = "Manganese P-R",
      skip = 2,
      nm_col = "Thousand tonnes",
      val_col = "At end of 2025",
      tech_name = "Manganese",
      unit_desc = "Thousand tonnes"
    ),
    list(
      sheet = "Nickel P-R",
      skip = 2,
      nm_col = "Thousand tonnes",
      val_col = "At end of 2025",
      tech_name = "Nickel",
      unit_desc = "Thousand tonnes"
    ),
    list(
      sheet = "Zinc P-R",
      skip = 2,
      nm_col = "Thousand tonnes",
      val_col = "At end of 2025",
      tech_name = "Zinc",
      unit_desc = "Thousand tonnes"
    ),
    list(
      # Renamed from "Platinum Group Metals P-R" and restated in kilograms in the 2026 edition.
      sheet = "PGM",
      skip = 2,
      nm_col = "Kilogram",
      val_col = "At end of 2025",
      tech_name = "PGMs",
      unit_desc = "Kilograms"
    )
  )
}

# Resolve the reserves value column on a mineral P-R sheet.
#
# The mineral sheets label reserves "At end of <year>", so the pinned name in
# reserves_specs() goes stale every release. When the pinned column is absent, fall back to
# the latest "At end of <year>" column actually present and say so, rather than erroring on
# a column name that only differs by vintage.
reserves_resolve_val_col <- function(sheet_data, val_col, sheet_id = NULL) {
  if (val_col %in% names(sheet_data)) {
    return(val_col)
  }

  candidates <- grep("^At end of \\d{4}$", names(sheet_data), value = TRUE)
  if (length(candidates) == 0) {
    return(val_col)
  }

  resolved <- candidates[which.max(as.integer(sub("^At end of ", "", candidates)))]
  message(
    "Reserves sheet ", sheet_id %||% "<unnamed>", ": '", val_col,
    "' not found; using '", resolved, "'."
  )
  resolved
}

reserves_build_country_reference <- function(ei, year = 2025) {
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

# Demand-by-technology shares from the IEA critical minerals dataset.
#
# The pillar label carries the release's own wording ("3.1 Cleantech demand by tech" in the
# 2025 vintage, "3.1 Energy demand by tech" in 2026), so only the section number is matched.
# Likewise the base year moves with each release, so the base and 2035 columns are resolved
# from the data. `share_24`/`share_35` keep their names — they are the base-year and 2035
# shares, and every downstream weighting step joins on them.
reserves_build_mineral_demand_clean <- function(critical,
                                               base_year = iea_critical_minerals_base_year(critical),
                                               horizon_year = 2035) {
  base_col <- iea_critical_minerals_year_col(critical, base_year, label = "mineral demand")
  horizon_col <- iea_critical_minerals_year_col(critical, horizon_year, label = "mineral demand")

  mineral_demand <- critical %>%
    dplyr::filter(
      grepl("^3\\.1", Pillar),
      !grepl("Other|Total", `Sector.Country`)
    ) %>%
    dplyr::mutate(
      demand_base = as.numeric(.data[[base_col]]),
      demand_horizon = as.numeric(.data[[horizon_col]]),
      growth = demand_horizon / demand_base - 1
    ) %>%
    dplyr::group_by(Pillar, Mineral) %>%
    dplyr::mutate(
      share_24 = demand_base / sum(demand_base),
      share_35 = demand_horizon / sum(demand_horizon)
    ) %>%
    dplyr::select(
      Pillar, Mineral, `Sector.Country`,
      demand_base, demand_horizon, growth, share_24, share_35
    ) %>%
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
                                         year = 2025,
                                         gamma = 0.5) {
  country_reference <- country_reference %>%
    dplyr::filter(!is.na(ISO3166_alpha3), nzchar(ISO3166_alpha3)) %>%
    dplyr::distinct(ISO3166_alpha3, Country)

  val_col <- reserves_resolve_val_col(sheet_data, val_col, sheet_id = sheet_id)

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
      source = "EI Statistical Review of World Energy (2025)",
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
                                                     year = 2025) {
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

reserves <- function(ei, reserve_inputs, mineral_demand_clean, year = 2025, gamma = 0.5) {
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
