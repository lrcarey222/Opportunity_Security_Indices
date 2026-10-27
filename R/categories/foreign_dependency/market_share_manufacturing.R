# Market Share theme builder functions.

market_share_manufacturing_build_cleantech_midstream <- function(ei,
                                                                 cleantech_midstream,
                                                                 year = 2035L,
                                                                 gamma = 0.5) {
  require_columns(ei, c("Country", "EU"), label = "ei")
  require_columns(cleantech_midstream, "X", label = "cleantech_midstream")

  cleantech_long <- cleantech_midstream %>%
    dplyr::rename(Country = X) %>%
    dplyr::mutate(Country = standardize_country_names(Country)) %>%
    tidyr::pivot_longer(
      cols = -Country,
      names_to = c("Technology", "Status"),
      names_pattern = "(.*)\\.\\.\\.(Current|Pipeline)",
      values_to = "Value"
    ) %>%
    dplyr::mutate(
      Year = dplyr::if_else(Status == "Current", 2024L, 2035L),
      # "Heat.Pumps" is read.csv()'s name for the IEA header "Heat Pumps - Current".
      Technology = csv_header_label(Technology),
      # IEA reports electrolyser manufacturing capacity; in this pipeline that is Green
      # Hydrogen Assembly, which otherwise has no Foreign Dependency input.
      Technology = dplyr::if_else(Technology == "Electrolyzers", "Green Hydrogen", Technology)
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

  # The denominator is the IEA world total: every entity the table lists, including its
  # "Other" row (6-28% of capacity), which has no EI country and is dropped by the join
  # below. Summing after the join left "Other" out and overstated every listed entity
  # (China's 2035 solar share read 0.90 against the IEA's 80%). Each technology's shares
  # shrink by one common factor, so their ranks, and the index, do not move.
  world_totals <- cleantech_long %>%
    dplyr::filter(Year == as.integer(year)) %>%
    dplyr::group_by(Technology) %>%
    dplyr::summarize(world_total = sum(Value, na.rm = TRUE), .groups = "drop")

  ct_ms <- country_map %>%
    dplyr::left_join(
      cleantech_long,
      by = c("country2" = "Country"),
      relationship = "one-to-many"
    ) %>%
    dplyr::filter(Year == as.integer(year)) %>%
    dplyr::left_join(world_totals, by = "Technology") %>%
    dplyr::mutate(market_share = Value / world_total) %>%
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
      # As foreign_dependency_cleantech_stage(), inline so this file can be sourced on its own:
      # ETP measures end-product capacity for solar (modules), wind (nacelles), electrolysers
      # and heat pumps, which is Assembly, and cell capacity for batteries, which is Components.
      supply_chain = dplyr::if_else(Technology == "Batteries", "Components", "Assembly"),
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

  # The composite is one value per country, so it takes one stage: Assembly, the step four of
  # its five inputs measure (solar, wind, electrolyser and heat-pump end-product capacity). The
  # fifth, battery cell capacity, is already scored in Components by its own row.
  comp_ct <- ct_idx %>%
    dplyr::group_by(Country) %>%
    dplyr::summarize(
      mean_idx = mean(market_share_index, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      composite_index = median_scurve(mean_idx, gamma = gamma),
      tech = "Clean Tech Midstream",
      supply_chain = "Assembly",
      category = "Foreign Dependency",
      variable = "Market Share",
      data_type = "index",
      Year = as.integer(year),
      source = "IEA Energy Technology Perspectives 2024",
      explanation = paste(
        "Percent-rank of the mean of the per-technology manufacturing-capacity indexes, filed under",
        "Assembly: solar, wind, electrolyser and heat-pump capacity is end-product capacity, battery",
        "capacity is cells (NB: All EU countries are given same market share)"
      )
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

  # Same expansion as foreign_dependency_build_ev_midstream() (see the comments there), kept
  # inline so this file can be sourced on its own. The five IEA regions partition the world;
  # Rest of World is the remainder, so it holds non-EU Europe. The legacy joins sent the EI
  # "Europe" sub-region to a placeholder ("boop") that matched nothing, which dropped the
  # United Kingdom, Norway, Turkiye, Ukraine, Switzerland, Iceland, Serbia and North
  # Macedonia from the EV rows.
  ev_regions <- c("China", "European Union", "North America", "Other Asia Pacific", "Rest of World")
  countries <- ei %>%
    dplyr::distinct(Country, EU, SubRegion) %>%
    dplyr::mutate(
      Region = dplyr::case_when(
        Country == "China" ~ "China",
        EU %in% 1 ~ "European Union",
        SubRegion %in% "North America" ~ "North America",
        SubRegion %in% "Asia Pacific" ~ "Other Asia Pacific",
        TRUE ~ "Rest of World"
      )
    )
  assert_unique_keys(countries, "Country", label = "ei")

  # The extract's `import_share` is (production - sales) / sales, a net export ratio (positive
  # = net exporter), and is ranked that way round; it is renamed for what it holds and checked
  # against that definition so a true import share cannot enter with the wrong sign.
  regional <- ev_midstream %>%
    dplyr::filter(as.character(Year) == as.character(year)) %>%
    dplyr::transmute(
      Region,
      Domestic.sales,
      Domestic.Production,
      net_export_ratio = import_share,
      market_share
    )
  if (!setequal(regional$Region, ev_regions) || anyDuplicated(regional$Region) > 0) {
    stop(
      "ev_midstream must have one row per IEA region for ", year, " (",
      paste(ev_regions, collapse = ", "), "); found: ", paste(regional$Region, collapse = ", ")
    )
  }
  implied <- (regional$Domestic.Production - regional$Domestic.sales) / regional$Domestic.sales
  off <- abs(regional$net_export_ratio - implied) > 1e-6
  if (any(off | is.na(off))) {
    stop(
      "ev_midstream import_share is not (production - sales) / sales for ",
      paste(regional$Region[off | is.na(off)], collapse = ", "),
      "; the EV Assembly net export term assumes that definition."
    )
  }

  explanations <- c(
    Domestic.sales = "EV sales in the region (million vehicles)",
    Domestic.Production = "EV production in the region (million vehicles)",
    net_export_ratio = paste(
      "Net export ratio of the region = (production - sales) / sales; positive means net",
      "exporter, negative net importer (the IEA extract labels it import_share)"
    ),
    market_share = "The region's share of world EV production",
    production_index = "Percent-ranked EV production of the region",
    sales_index = "Percent-ranked EV sales of the region",
    net_export_index = "Percent-ranked net export ratio of the region (higher = less dependent on imported EVs)",
    market_share_index = "Percent-ranked share of world EV production",
    ev_Midstream_index = "Mean of the percent-ranked production, net export ratio and market share"
  )

  countries %>%
    dplyr::inner_join(regional, by = "Region", relationship = "many-to-one") %>%
    dplyr::mutate(
      production_index = median_scurve(Domestic.Production, gamma = gamma),
      sales_index = median_scurve(Domestic.sales, gamma = gamma),
      net_export_index = median_scurve(net_export_ratio, gamma = gamma),
      market_share_index = median_scurve(market_share, gamma = gamma)
    ) %>%
    dplyr::rowwise() %>%
    # Sales are demand, not manufacturing: they stay out of the composite and are emitted as
    # Deployment rows (user decision 2026-10-26, docs/supply_chain_stages.md).
    dplyr::mutate(ev_Midstream_index = mean(c(production_index, net_export_index, market_share_index))) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      Country, Region,
      Domestic.sales, Domestic.Production, net_export_ratio, market_share,
      production_index, sales_index, net_export_index, market_share_index, ev_Midstream_index
    ) %>%
    tidyr::pivot_longer(
      cols = -c(Country, Region),
      names_to = "variable",
      values_to = "value"
    ) %>%
    # Production, market share, the net export ratio (a production surplus) and the composite
    # are Assembly, the vehicle-manufacturing step; sales are Deployment.
    dplyr::mutate(
      tech = "Electric Vehicles",
      supply_chain = dplyr::if_else(variable %in% c("Domestic.sales", "sales_index"), "Deployment", "Assembly"),
      category = "Foreign Dependency",
      data_type = dplyr::if_else(stringr::str_detect(variable, "_index$"), "index", "raw"),
      explanation = paste0(
        unname(explanations[variable]), ". IEA region: ", Region,
        " (the regional value is copied to each economy in it)"
      ),
      variable = stringr::str_remove(variable, "_index$"),
      Year = as.integer(year),
      source = "IEA EV Outlook"
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
    ) %>%
    dplyr::distinct()
}

market_share_manufacturing <- function(ei,
                                       cleantech_midstream,
                                       ev_midstream,
                                       year = list(cleantech = 2035L, ev = 2024L),
                                       gamma = 0.5) {
  # One standardized row per EI country for the regional expansions, as in
  # foreign_dependency_ei_countries(): the expansions rank every row they produce, so EI
  # aggregates ("Total Asia Pacific", "USSR") and raw spellings ("US", "Vietnam") are removed
  # before ranking. Kept inline so this file can be sourced on its own.
  ei_year <- max(suppressWarnings(as.integer(ei$Year)), na.rm = TRUE)
  ei <- ei %>%
    dplyr::filter(
      as.character(Year) == as.character(ei_year),
      !grepl("World|Other|Total|OECD|OPEC|USSR", Country)
    ) %>%
    dplyr::mutate(Country = standardize_country_names(Country)) %>%
    dplyr::group_by(Country) %>%
    dplyr::summarize(
      EU = max(as.integer(EU), na.rm = TRUE),
      SubRegion = dplyr::first(SubRegion[!is.na(SubRegion) & nzchar(SubRegion)]),
      .groups = "drop"
    )

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
