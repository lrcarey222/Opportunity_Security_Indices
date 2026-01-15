# Production depth + momentum theme builder functions.

production_depth_momentum_clean_energy_production <- function(ei,
                                                              years = c(2014, 2019, 2024),
                                                              vars = c(
                                                                "oilprod_kbd",
                                                                "gasprod_ej",
                                                                "coalprod_ej",
                                                                "solar_twh",
                                                                "wind_twh",
                                                                "nuclear_twh",
                                                                "electbyfuel_coal",
                                                                "electbyfuel_gas",
                                                                "electbyfuel_oil"
                                                              )) {
  require_columns(ei, c("Country", "Year", "Var", "Value"), label = "ei")

  ei %>%
    dplyr::select(Country, Year, Var, Value) %>%
    dplyr::mutate(
      Year = as.integer(Year),
      Value = as.numeric(Value)
    ) %>%
    dplyr::filter(
      !grepl("Other|Total|OECD|OPEC|World", Country),
      Year %in% years,
      Var %in% vars
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      Var = dplyr::case_when(
        Var == "oilprod_kbd" ~ "Oil Upstream",
        Var == "gasprod_ej" ~ "Gas Upstream",
        Var == "coalprod_ej" ~ "Coal Upstream",
        Var == "solar_twh" ~ "Solar Downstream",
        Var == "wind_twh" ~ "Wind Downstream",
        Var == "nuclear_twh" ~ "Nuclear Downstream",
        Var == "electbyfuel_coal" ~ "Coal Downstream",
        Var == "electbyfuel_gas" ~ "Gas Downstream",
        Var == "electbyfuel_oil" ~ "Oil Downstream",
        TRUE ~ Var
      )
    ) %>%
    dplyr::group_by(Country, Year, Var) %>%
    dplyr::summarize(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    tidyr::complete(
      Country,
      Year,
      Var,
      fill = list(Value = 0)
    ) %>%
    tidyr::pivot_wider(names_from = "Year", values_from = "Value") %>%
    dplyr::mutate(
      change_5yr = (`2024` / `2019` - 1) * 100,
      change_10yr = (`2024` / `2014` - 1) * 100,
      change_5yr_abs = `2024` - `2019`
    ) %>%
    tidyr::pivot_longer(
      cols = `2014`:change_5yr_abs,
      names_to = "year",
      values_to = "value"
    ) %>%
    tidyr::pivot_wider(names_from = year, values_from = value)
}

production_depth_momentum_build_energy_indices <- function(production_clean, gamma = 0.5) {
  require_columns(
    production_clean,
    c("Country", "Var", "2014", "2019", "2024", "change_5yr", "change_10yr", "change_5yr_abs"),
    label = "production_clean"
  )

  production_clean %>%
    tidyr::separate(
      Var,
      into = c("tech", "supply_chain"),
      sep = " (?=Upstream|Downstream)",
      remove = TRUE
    ) %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(
      size_index = median_scurve(`2024`, gamma = gamma),
      growth_index = median_scurve(change_5yr, gamma = gamma),
      growth_abs_index = median_scurve(change_5yr_abs, gamma = gamma)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      overall_production_index = mean(dplyr::c_across(dplyr::ends_with("_index")), na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(overall_production_index = median_scurve(overall_production_index, gamma = gamma)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Country = dplyr::if_else(Country == "US", "United States", Country))
}

production_depth_momentum_build_mineral_supply <- function(critical,
                                                           mineral_demand_clean,
                                                           country_info,
                                                           gamma = 0.5) {
  require_columns(
    critical,
    c("Pillar", "Sector.Country", "Mineral", "X2024", "X2035"),
    label = "critical"
  )
  require_columns(
    mineral_demand_clean,
    c("Mineral", "tech", "share_24", "share_35"),
    label = "mineral_demand_clean"
  )
  require_columns(country_info, "country", label = "country_info")

  minerals <- critical %>%
    dplyr::filter(
      grepl("Total supply", Pillar),
      !grepl("Top 3|Total", `Sector.Country`),
      `Sector.Country` != "",
      Mineral != ""
    ) %>%
    tidyr::separate(Mineral, into = c("mineral", "supply_chain_raw"), sep = " - ", extra = "merge") %>%
    dplyr::distinct(mineral)

  if (nrow(minerals) == 0) {
    stop("No minerals found in critical supply data.")
  }

  countries <- critical %>%
    dplyr::distinct(`Sector.Country`) %>%
    dplyr::inner_join(
      country_info %>% dplyr::select(country),
      by = c("Sector.Country" = "country")
    )

  if (nrow(countries) == 0) {
    stop("No matching countries found in critical supply data.")
  }

  critical %>%
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
        "Midstream",
        "Upstream"
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(mineral, country, supply_chain, X2024, X2035) %>%
    tidyr::complete(
      mineral = minerals$mineral,
      supply_chain = c("Upstream", "Midstream"),
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
      security_index = mean(dplyr::c_across(market_share24_index:hhi35_index), na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(supply_chain) %>%
    dplyr::arrange(dplyr::desc(security_index))
}

production_depth_momentum_build_mineral_production <- function(mineral_supply, gamma = 0.5) {
  require_columns(
    mineral_supply,
    c("mineral", "country", "tech", "supply_chain", "X2024", "X2035", "HHI_24"),
    label = "mineral_supply"
  )

  mineral_supply %>%
    dplyr::mutate(supply_growth = (X2035 - X2024) / X2024) %>%
    dplyr::select(mineral, country, tech, supply_chain, X2024, X2035, supply_growth, HHI_24) %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(
      size_index = median_scurve(X2024, gamma = gamma),
      growth_abs_index = median_scurve(supply_growth, gamma = gamma),
      HHI_index = median_scurve(-HHI_24, gamma = gamma)
    ) %>%
    dplyr::group_by(country, tech, supply_chain) %>%
    dplyr::summarize(dplyr::across(X2024:HHI_index, mean, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(
      size_index = median_scurve(size_index, gamma = gamma),
      growth_abs_index = median_scurve(growth_abs_index, gamma = gamma),
      HHI_index = median_scurve(HHI_index, gamma = gamma)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      overall_production_index = 2 * size_index + 1 * growth_abs_index + 1 * HHI_index
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(overall_production_index = median_scurve(overall_production_index, gamma = gamma)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Country = dplyr::if_else(country == "US", "United States", country)) %>%
    dplyr::select(-country)
}

production_depth_momentum_build_combined <- function(production_growth, mineral_production) {
  require_columns(
    production_growth,
    c("Country", "tech", "supply_chain", "2024", "size_index", "growth_abs_index", "overall_production_index"),
    label = "production_growth"
  )
  require_columns(
    mineral_production,
    c("Country", "tech", "supply_chain", "X2024", "size_index", "growth_abs_index", "overall_production_index"),
    label = "mineral_production"
  )

  production_tbl <- production_growth %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      X2024 = `2024`,
      size_index,
      growth_abs_index,
      overall_production_index
    )
  mineral_tbl <- mineral_production %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      X2024,
      size_index,
      growth_abs_index,
      overall_production_index
    )

  production_tbl <- standardize_bind_rows_inputs(production_tbl)
  mineral_tbl <- standardize_bind_rows_inputs(mineral_tbl)

  dplyr::bind_rows(production_tbl, mineral_tbl)
}

production_depth_momentum_build_table <- function(production_combined,
                                                  year = 2024,
                                                  source = "EI Statistical Review World Energy") {
  require_columns(
    production_combined,
    c("Country", "tech", "supply_chain", "X2024", "size_index", "growth_abs_index", "overall_production_index"),
    label = "production_combined"
  )

  production_combined %>%
    dplyr::filter(supply_chain != "Midstream") %>%
    tidyr::pivot_longer(
      cols = c(X2024:overall_production_index),
      names_to = "metric",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      data_type = dplyr::if_else(stringr::str_ends(metric, "_index"), "index", "raw"),
      metric_clean = stringr::str_remove(metric, "_index$")
    ) %>%
    dplyr::mutate(
      category = "Production",
      source = source,
      explanation = dplyr::case_when(
        metric_clean %in% c("X2024") ~
          glue::glue("Raw production of {tech} ({supply_chain}) in {metric_clean}"),
        metric_clean == "size" ~
          glue::glue("Min-max scaled production in 2024 for {tech}"),
        metric_clean == "growth_abs" ~
          glue::glue("Min-max scaled absolute growth index for {tech}"),
        metric_clean == "overall_production" ~
          glue::glue("Composite index: mean of size & growth indices, then min-max scaled for {tech}"),
        TRUE ~ metric_clean
      )
    ) %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      category,
      variable = dplyr::if_else(metric_clean == "overall_production", "Overall Production", metric_clean),
      data_type,
      value,
      Year = as.integer(year),
      source,
      explanation
    )
}

production_depth_momentum <- function(ei,
                                      critical,
                                      mineral_demand_clean,
                                      country_info,
                                      year = 2024,
                                      gamma = 0.5) {
  production_clean <- production_depth_momentum_clean_energy_production(ei)
  production_growth <- production_depth_momentum_build_energy_indices(production_clean, gamma = gamma)
  mineral_supply <- production_depth_momentum_build_mineral_supply(
    critical = critical,
    mineral_demand_clean = mineral_demand_clean,
    country_info = country_info,
    gamma = gamma
  )
  mineral_production <- production_depth_momentum_build_mineral_production(mineral_supply, gamma = gamma)
  production_combined <- production_depth_momentum_build_combined(
    production_growth = production_growth,
    mineral_production = mineral_production
  )

  output <- production_depth_momentum_build_table(
    production_combined = production_combined,
    year = year
  )

  output <- standardize_theme_table(output)
  validate_schema(output)
  output
}
