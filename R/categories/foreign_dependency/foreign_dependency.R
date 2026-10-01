# Foreign Dependency theme builder functions.
foreign_dependency_build_country_reference <- function(ei, year = 2025) {
  # Use EI as the authoritative country reference and harmonize legacy naming tweaks.
  # "USSR" is EI's CIS aggregate, still published for the latest year.
  ei %>%
    dplyr::filter(
      Year == year,
      !grepl("World|Other|Total|OECD|OPEC|USSR", Country)
    ) %>%
    dplyr::mutate(Country = standardize_country_names(Country)) %>%
    dplyr::distinct(Country)
}

# EI country rows for the regional expansions, one per country: standardized name, EU flag,
# EI sub-region.
#
# The expansions below copy regional values to countries and then rank every row they
# produce, so aggregates ("Total Asia Pacific", "Other Europe", "USSR") and spellings that
# only collapse later ("US", "Vietnam") have to be gone before the ranking, not after. EI
# flags Cyprus and Estonia EU = 1 on some variables and 0 on others; max() keeps one row.
foreign_dependency_ei_countries <- function(ei, year = NULL) {
  year <- year %||% max(suppressWarnings(as.integer(ei$Year)), na.rm = TRUE)

  ei %>%
    dplyr::filter(
      as.character(Year) == as.character(year),
      !grepl("World|Other|Total|OECD|OPEC|USSR", Country)
    ) %>%
    dplyr::mutate(Country = standardize_country_names(Country)) %>%
    dplyr::group_by(Country) %>%
    dplyr::summarize(
      EU = max(as.integer(EU), na.rm = TRUE),
      SubRegion = dplyr::first(SubRegion[!is.na(SubRegion) & nzchar(SubRegion)]),
      .groups = "drop"
    )
}

foreign_dependency_build_mineral_supply <- function(critical,
                                                    mineral_demand_clean,
                                                    country_reference,
                                                    year = iea_critical_minerals_base_year(critical),
                                                    gamma = 0.5,
                                                    horizon_year = 2035) {
  # The IEA restates its base year each release, so address the supply columns by year.
  base_col <- iea_critical_minerals_year_col(critical, year, label = "mineral supply")
  horizon_col <- iea_critical_minerals_year_col(critical, horizon_year, label = "mineral supply")

  critical <- critical %>%
    dplyr::mutate(`Sector.Country` = standardize_country_names(`Sector.Country`))

  # Every roster country gets a zero-supply row for each (mineral, stage) it is not listed
  # for, so a non-supplier is scored the same way whether or not the IEA lists it for some
  # other mineral: own share 0, plus the world-concentration (HHI) terms every country shares.
  # Completing only the listed countries scored those at the tied-zero rank and left the
  # unlisted ones at a flat 0 in the final completion below. Listed suppliers outside the
  # roster are completed too (as critical_minerals_processing() does): Lao PDR mines only
  # rare earths, and without zero rows for the other minerals its EV score averaged rare
  # earths alone and ranked second to China.
  countries <- critical %>%
    dplyr::filter(
      grepl("Total supply", Pillar),
      !grepl("Top 3|Total", `Sector.Country`),
      !`Sector.Country` %in% c("", "Rest of world")
    ) %>%
    dplyr::distinct(`Sector.Country`) %>%
    dplyr::bind_rows(dplyr::distinct(country_reference, `Sector.Country` = Country)) %>%
    dplyr::distinct(`Sector.Country`)

  # Compute market shares, HHI, and the composite supply security index.
  #
  # Mining rows score the Upstream stage and refining/processing rows the Midstream stage:
  # pooling them put ore and refined-metal tonnage into one share and one HHI, and left
  # refining dependence out of Midstream entirely. Shares and HHIs are therefore taken within
  # (mineral, tech, stage).
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
      mineral = harmonize_mineral_names(mineral),
      supply_chain = iea_critical_minerals_supply_stage(supply_chain_raw)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      mineral, country, supply_chain,
      supply_base = dplyr::all_of(base_col),
      supply_horizon = dplyr::all_of(horizon_col)
    ) %>%
    # Countries absent from a (mineral, stage) block are not among its listed suppliers.
    tidyr::complete(
      tidyr::nesting(mineral, supply_chain),
      country = countries$`Sector.Country`,
      fill = list(supply_base = 0, supply_horizon = 0)
    ) %>%
    dplyr::inner_join(
      mineral_demand_clean %>%
        dplyr::filter(!is.na(tech)) %>%
        dplyr::ungroup() %>%
        dplyr::transmute(Mineral = harmonize_mineral_names(Mineral), tech, share_24, share_35),
      by = c("mineral" = "Mineral"),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      supply_24 = share_24 * supply_base,
      supply_35 = share_35 * supply_horizon
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
    # Ranked within stage (pooled across minerals and techs, as before the stage split), so a
    # refining share is compared with other refining shares. A zero share scores 0 and the
    # suppliers are ranked above it (iea_critical_minerals_share_index()).
    dplyr::group_by(supply_chain) %>%
    dplyr::mutate(
      market_share24_index = iea_critical_minerals_share_index(share_frac_24, gamma = gamma),
      market_share35_index = iea_critical_minerals_share_index(share_frac_35, gamma = gamma),
      hhi24_index = median_scurve(1 - HHI_24, gamma = gamma),
      hhi35_index = median_scurve(1 - HHI_35, gamma = gamma)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      security_index = mean(dplyr::c_across(c(market_share24_index:hhi35_index)), na.rm = TRUE)
    ) %>%
    dplyr::group_by(supply_chain) %>%
    dplyr::arrange(dplyr::desc(security_index))

  # Aggregate into the canonical schema with completed country coverage (every roster country
  # already has rows, so the 0 fill below is only a safeguard).
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
      Year = year,
      source = "IEA Critical Minerals Database",
      explanation = dplyr::case_when(
        data_type == "index" ~ stringr::str_glue(
          "Percent-rank of {tech} market share across countries, and HHI across critical minerals, ",
          "of {dplyr::if_else(supply_chain == 'Midstream', 'refined', 'mined')} supply"
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
  # Reshape IEA clean tech midstream data into a long form with year tags. Row labels go
  # through the same name standardization as the EI roster they are joined to ("US").
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
                               year = list(
                                 minerals = iea_critical_minerals_base_year(critical),
                                 cleantech = "2035",
                                 ev = "2024"
                               ),
                               ei_year = "2025",
                               gamma = 0.5) {
  # Reference country list and run each foreign dependency sub-theme builder. The EI country
  # roster follows the EI release year, which moves independently of the IEA vintages in `year`.
  country_reference <- foreign_dependency_build_country_reference(ei, year = ei_year)
  ei_countries <- foreign_dependency_ei_countries(ei, year = ei_year)

  mineral_supply <- foreign_dependency_build_mineral_supply(
    critical = critical,
    mineral_demand_clean = mineral_demand_clean,
    country_reference = country_reference,
    year = year$minerals,
    gamma = gamma
  )

  cleantech <- foreign_dependency_build_cleantech_midstream(
    ei = ei_countries,
    cleantech_midstream = cleantech_midstream,
    year = year$cleantech,
    gamma = gamma
  )

  ev_midstream_tbl <- foreign_dependency_build_ev_midstream(
    ei = ei_countries,
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
