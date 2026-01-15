# Foreign Dependency theme builder functions.
foreign_dependency_build_country_reference <- function(ei, year = 2024) {
  # Use EI data as the authoritative country reference and apply legacy naming.
  # Use EI as the authoritative country reference and harmonize legacy naming tweaks.
  ei %>%
    dplyr::filter(
      Year == year,
      !grepl("World|Other|Total|OECD|OPEC", Country)
    ) %>%
    dplyr::mutate(Country = standardize_country_names(Country)) %>%
    dplyr::distinct(Country)
}

foreign_dependency_build_mineral_supply <- function(critical,
                                                    mineral_demand_clean,
                                                    country_reference,
                                                    year = 2024,
                                                    gamma = 0.5) {
  critical <- critical %>%
    dplyr::mutate(`Sector.Country` = standardize_country_names(`Sector.Country`))

  # Identify minerals in the IEA critical minerals dataset.
  minerals <- critical %>%
    dplyr::filter(
      grepl("Total supply", Pillar),
      !grepl("Top 3|Total", `Sector.Country`),
      `Sector.Country` != "",
      Mineral != ""
    ) %>%
    tidyr::separate(Mineral, into = c("mineral", "supply_chain_raw"), sep = " - ", extra = "merge") %>%
    dplyr::distinct(mineral)

  # Keep only country entries that appear in the EI-derived reference list.
  countries <- critical %>%
    dplyr::distinct(`Sector.Country`) %>%
    dplyr::inner_join(
      country_reference %>% dplyr::transmute(country = Country),
      by = c("Sector.Country" = "country")
    )

  # Compute market shares, HHI, and the composite supply security index.
  mineral_supply <- critical %>%
    dplyr::filter(
      grepl("Total supply", Pillar),
      !grepl("Top 3|Total", `Sector.Country`),
      `Sector.Country` != "",
      Mineral != ""
    ) %>%
    dplyr::rename(country = `Sector.Country`) %>%
    tidyr::separate(Mineral, into = c("mineral", "supply_chain_raw"), sep = " - ", extra = "merge") %>%
    dplyr::mutate(
      supply_chain = dplyr::if_else(
        stringr::str_detect(supply_chain_raw, stringr::regex("Refining|Chemical", ignore_case = TRUE)),
        "Upstream",
        "Upstream"
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(mineral, country, supply_chain, X2024, X2035) %>%
    tidyr::complete(
      mineral = minerals$mineral,
      supply_chain = c("Upstream"),
      country = countries$`Sector.Country`,
      fill = list(X2024 = 0, X2035 = 0)
    ) %>%
    dplyr::inner_join(
      mineral_demand_clean %>%
        dplyr::filter(!is.na(tech)) %>%
        dplyr::ungroup() %>%
        dplyr::select(Mineral, tech, share_24, share_35),
      by = c("mineral" = "Mineral"),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      supply_24 = share_24 * X2024,
      supply_35 = share_35 * X2035
    ) %>%
    dplyr::group_by(mineral, tech, supply_chain) %>%
    dplyr::mutate(
      share_frac_24 = supply_24 / sum(supply_24, na.rm = TRUE),
      HHI_24 = sum(share_frac_24^2, na.rm = TRUE),
      share_frac_35 = supply_35 / sum(supply_35, na.rm = TRUE),
      HHI_35 = sum(share_frac_35^2, na.rm = TRUE)
    ) %>%
    dplyr::filter(
      country != "Rest of world",
      !is.na(share_frac_24),
      !is.na(share_frac_35)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      market_share24_index = median_scurve(share_frac_24, gamma = gamma),
      market_share35_index = median_scurve(share_frac_35, gamma = gamma),
      hhi24_index = median_scurve(1 - HHI_24, gamma = gamma),
      hhi35_index = median_scurve(1 - HHI_35, gamma = gamma)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      security_index = mean(dplyr::c_across(c(market_share24_index:hhi35_index)), na.rm = TRUE)
    ) %>%
    dplyr::group_by(supply_chain) %>%
    dplyr::arrange(dplyr::desc(security_index))

  # Aggregate into the canonical schema with completed country coverage.
  mineral_supply %>%
    dplyr::group_by(country, tech, supply_chain) %>%
    dplyr::summarize(
      security_index = stats::weighted.mean(security_index, w = share_35, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(tech, supply_chain) %>%
    tidyr::pivot_longer(
      cols = c(security_index),
      names_to = "name",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      category = "Foreign Dependency",
      variable = "Mineral Supply",
      data_type = "index",
      Year = as.character(year),
      Year = year,
      source = "IEA Critical Minerals Database",
      explanation = dplyr::case_when(
        data_type == "index" ~ stringr::str_glue(
          "Percent-rank of {tech} market share across countries, and HHI across critical minerals"
        )
      )
    ) %>%
    dplyr::select(
      country,
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
    dplyr::group_by(tech, supply_chain, category, data_type, source, variable, explanation, Year) %>%
    tidyr::complete(
      country = country_reference$Country,
      fill = list(value = 0)
    ) %>%
    dplyr::rename(Country = country)
}

foreign_dependency_build_cleantech_midstream <- function(ei,
                                                         cleantech_midstream,
                                                         year = 2035,
                                                         gamma = 0.5) {
  # Reshape IEA clean tech midstream data into a long form with year tags.
  cleantech_long <- cleantech_midstream %>%
    dplyr::rename(Country = X) %>%
    tidyr::pivot_longer(
      cols = -Country,
      names_to = c("Technology", "Status"),
      names_pattern = "(.*)\\.\\.\\.(Current|Pipeline)",
      values_to = "Value"
    ) %>%
    dplyr::mutate(
      Year = dplyr::if_else(Status == "Current", "2024", "2035")
    )

  # Map EU member countries into a shared "EU" entity to match legacy logic.
  country_map <- ei %>%
    dplyr::distinct(Country, EU) %>%
    dplyr::mutate(
      Country_clean = dplyr::if_else(Country == "Vietnam", "Viet Nam", Country),
      country2 = dplyr::if_else(EU == 1, "EU", Country_clean)
    ) %>%
    dplyr::select(country2) %>%
    dplyr::distinct()

  # Compute global market shares per technology for the target year.
  ct_ms <- country_map %>%
    dplyr::left_join(cleantech_long, by = c("country2" = "Country")) %>%
    dplyr::filter(Year == as.character(year)) %>%
    dplyr::group_by(Technology) %>%
    dplyr::mutate(
      market_share = Value / sum(Value, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(Country = country2, Technology, market_share)

  # Convert market shares into percentile-based indices.
  ct_idx <- ct_ms %>%
    dplyr::group_by(Technology) %>%
    dplyr::mutate(
      market_share_index = median_scurve(market_share, gamma = gamma)
    ) %>%
    dplyr::ungroup()

  # Emit tech-level raw and indexed market shares in canonical form.
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
      Year = as.character(year),
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

  # Build the composite clean-tech midstream index across technologies.
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
      Year = as.character(year),
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

  # Expand the EU rollup back to individual countries via EI mapping.
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

foreign_dependency_build_ev_midstream <- function(ei,
                                                  ev_midstream,
                                                  year = 2024,
                                                  gamma = 0.5) {
  # Recreate the legacy regional expansion logic for EV midstream data.
  ev_midstream %>%
    dplyr::filter(Year == as.character(year)) %>%
    dplyr::mutate(EU = dplyr::if_else(Region == "European Union", 1, 2)) %>%
    dplyr::left_join(ei %>% dplyr::distinct(Country, EU), by = c("EU")) %>%
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
      by = c("SubRegion")
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
      by = c("SubRegion")
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
    # Convert raw metrics into median_scurve indices.
    dplyr::mutate(
      production_index = median_scurve(Domestic.Production, gamma = gamma),
      sales_index = median_scurve(Domestic.sales, gamma = gamma),
      import_index = median_scurve(import_share, gamma = gamma),
      market_share_index = median_scurve(market_share, gamma = gamma)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ev_Midstream_index = mean(dplyr::c_across(production_index:market_share_index))) %>%
    dplyr::select(country, Domestic.sales:market_share, production_index:ev_Midstream_index) %>%
    tidyr::pivot_longer(
      cols = -country,
      names_to = "variable",
      values_to = "value"
    ) %>%
    # Standardize variables into raw/index pairs in the canonical schema.
    dplyr::mutate(
      tech = "Electric Vehicles",
      supply_chain = "Midstream",
      category = "Foreign Dependency",
      data_type = dplyr::if_else(stringr::str_detect(variable, "_index$"), "index", "raw"),
      variable = stringr::str_remove(variable, "_index$"),
      Year = as.character(year),
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

foreign_dependency <- function(critical,
                               mineral_demand_clean,
                               ei,
                               cleantech_midstream,
                               ev_midstream,
                               year = list(minerals = "2024", cleantech = "2035", ev = "2024"),
                               gamma = 0.5) {
  # Reference country list and run each foreign dependency sub-theme builder.
  country_reference <- foreign_dependency_build_country_reference(ei, year = year$minerals)

  mineral_supply <- foreign_dependency_build_mineral_supply(
    critical = critical,
    mineral_demand_clean = mineral_demand_clean,
    country_reference = country_reference,
    year = year$minerals,
    gamma = gamma
  )

  cleantech <- foreign_dependency_build_cleantech_midstream(
    ei = ei,
    cleantech_midstream = cleantech_midstream,
    year = year$cleantech,
    gamma = gamma
  )

  ev_midstream_tbl <- foreign_dependency_build_ev_midstream(
    ei = ei,
    ev_midstream = ev_midstream,
    year = year$ev,
    gamma = gamma
  )
  standardized <- lapply(
    list(mineral_supply, cleantech, ev_midstream_tbl),
    standardize_bind_rows_inputs
  )

  dplyr::bind_rows(standardized) %>%
    energy_security_add_overall_index()
}
