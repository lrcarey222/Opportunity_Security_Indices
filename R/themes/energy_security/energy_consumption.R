# Energy consumption theme builder functions.

# === Energy consumption (EI per-capita) ===
# This section mirrors the legacy energy-consumption-per-capita pipeline and
# keeps the same normalization and growth calculations.

# ---- Clean EI raw inputs ----
# Filter and reshape the EI statistical review data for per-capita consumption
# metrics and ensure missing fuels resolve to zero.
energy_consumption_clean_ei <- function(ei, year) {
  ei %>%
    dplyr::mutate(
      Country = dplyr::if_else(Country == "US", "United States", Country)
    ) %>%
    dplyr::filter(
      Year == year,
      Var %in% c("pop", "coalcons_ej", "oilcons_ej", "gascons_ej",
                "solar_ej", "wind_ej", "nuclear_ej"),
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
    dplyr::mutate(dplyr::across(dplyr::ends_with("_raw"), ~tidyr::replace_na(.x, 0)))
}

# ---- Build EI per-capita indices ----
energy_consumption_build_ei_indices <- function(ei_clean, year, gamma = 0.5) {
  ei_clean %>%
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
        data_type == "index" ~ stringr::str_glue("Normalized index of per-capita {tech} consumption")
      )
    )
}

# ---- Compute EI growth between base and target years ----
energy_consumption_build_ei_growth <- function(ei_base, ei_target, base_year, target_year, gamma = 0.5) {
  ei_base %>%
    dplyr::inner_join(
      ei_target,
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
        data_type == "raw" ~ stringr::str_glue("{base_year}-{target_year} growth of per-capita consumption"),
        data_type == "index" ~ "Normalized index of per-capita consumption growth"
      )
    )
}

# === BNEF installed capacity / consumption ===
# This section mirrors the legacy BNEF New Energy Outlook logic, including
# population normalization, tech recoding, and the region-to-country expansion.

# ---- Build population reference ----
# Extract 2024/2035 population totals used to compute per-capita metrics.
energy_consumption_build_bnef_population <- function(bnef_neo) {
  bnef_neo %>%
    dplyr::filter(
      Indicator == "Population",
      Macro.sector == "All sectors",
      !Region %in% c("Global", "Rest of World"),
      Scenario == "ETS"
    ) %>%
    dplyr::transmute(
      Country = dplyr::recode(Region, "US" = "United States", "UK" = "United Kingdom"),
      pop_2024 = as.numeric(X2024),
      pop_2035 = as.numeric(X2035)
    )
}

# ---- Build BNEF per-capita capacity metrics ----
# Filter to installed capacity/final energy consumption, normalize per-capita,
# and compute legacy share + growth indices.
energy_consumption_build_bnef_metrics <- function(bnef_neo, pop_tbl, techs, country_info) {
  neo_cap <- bnef_neo %>%
    dplyr::filter(
      Indicator %in% c("Installed electric capacity", "Final energy consumption"),
      Macro.sector %in% c("All sectors", "Energy industry"),
      !Region %in% c("Global", "Rest of World"),
      Scenario == "ETS",
      (Indicator == "Installed electric capacity" & Fuel.type != "Hydrogen") |
        (Indicator == "Final energy consumption" & Fuel.type == "Hydrogen")
    ) %>%
    dplyr::mutate(
      Fuel.type = dplyr::recode(
        Fuel.type,
        "CCGT" = "Gas",
        "Coal with CCS" = "Coal",
        "Gas peaker with CCS" = "Gas",
        "CCGT with CCS" = "Gas",
        "Utility-scale PV" = "Solar",
        "Unabated oil" = "Oil",
        "Gas production" = "Gas",
        "Gas peaker" = "Gas",
        "Hydrogen" = "Green Hydrogen",
        "Battery storage" = "Batteries",
        "Small modular nuclear" = "Nuclear",
        "Small-scale PV" = "Solar"
      )
    ) %>%
    dplyr::select(
      Country = Region,
      tech = Fuel.type,
      Indicator,
      X2024,
      X2035
    ) %>%
    dplyr::mutate(
      X2024 = as.numeric(X2024),
      X2035 = as.numeric(X2035),
      Country = dplyr::recode(Country, "US" = "United States", "UK" = "United Kingdom")
    ) %>%
    dplyr::left_join(pop_tbl, by = "Country") %>%
    dplyr::mutate(
      X2024_pc = X2024 / pop_2024,
      X2035_pc = X2035 / pop_2035
    ) %>%
    dplyr::group_by(Country, tech) %>%
    dplyr::summarize(
      X2024_pc = sum(X2024_pc, na.rm = TRUE),
      X2035_pc = sum(X2035_pc, na.rm = TRUE),
      X2024 = sum(X2024, na.rm = TRUE),
      X2035 = sum(X2035, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(Country) %>%
    dplyr::mutate(
      share_24 = X2024 / sum(X2024),
      growth_2435 = (X2035_pc - X2024_pc) / X2024_pc
    ) %>%
    dplyr::filter(tech %in% techs) %>%
    dplyr::group_by(tech) %>%
    dplyr::mutate(installed_cap_index = median_scurve(X2024_pc)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(elec_growth_index = median_scurve(growth_2435)) %>%
    tidyr::pivot_longer(
      cols = c(
        X2024_pc,
        X2035_pc,
        share_24,
        growth_2435,
        installed_cap_index,
        elec_growth_index
      ),
      names_to = "variable",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      Country = dplyr::recode(
        Country,
        "Other Latin America" = "Latin America & Caribbean",
        "Other Southeast Asia" = "East Asia & Pacific",
        "MENAT" = "Middle East & North Africa",
        "Other Asia Pacific" = "East Asia & Pacific",
        "Other Europe" = "Europe & Central Asia"
      )
    ) %>%
    dplyr::left_join(country_info %>% dplyr::select(country, region), by = c("Country" = "region"))

  bnef_countries <- unique(neo_cap$Country)

  neo_cap %>%
    dplyr::filter(!country %in% bnef_countries) %>%
    dplyr::mutate(Country = dplyr::if_else(is.na(country), Country, country)) %>%
    dplyr::mutate(
      supply_chain = "Downstream",
      category = "Consumption",
      data_type = dplyr::if_else(variable %in% c("installed_cap_index", "elec_growth_index"), "index", "raw"),
      Year = NA_integer_,
      source = "BNEF New Energy Outlook 2024",
      explanation = dplyr::case_when(
        variable == "X2024_pc" ~ "2024 installed electric capacity **per capita**",
        variable == "X2035_pc" ~ "Forecast 2035 installed electric capacity **per capita**",
        variable == "share_24" ~ "2024 share of *per-capita* installed capacity",
        variable == "installed_cap_index" ~ "Index of per-capita capacity share (2024)",
        variable == "growth_2435" ~ "Growth in per-capita capacity 2024-35",
        variable == "elec_growth_index" ~ "Index of per-capita capacity growth 2024-35"
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
}

# === Public theme entrypoint ===
energy_consumption <- function(ei,
                               bnef_neo,
                               country_info,
                               base_year = 2019,
                               target_year = 2024,
                               techs = c(
                                 "Electric Vehicles",
                                 "Nuclear",
                                 "Coal",
                                 "Batteries",
                                 "Green Hydrogen",
                                 "Wind",
                                 "Oil",
                                 "Solar",
                                 "Gas",
                                 "Geothermal",
                                 "Electric Grid"
                               ),
                               gamma = 0.5) {
  ei_base <- energy_consumption_build_ei_indices(
    energy_consumption_clean_ei(ei, base_year),
    base_year,
    gamma = gamma
  )
  ei_target <- energy_consumption_build_ei_indices(
    energy_consumption_clean_ei(ei, target_year),
    target_year,
    gamma = gamma
  )
  ei_growth <- energy_consumption_build_ei_growth(
    ei_base,
    ei_target,
    base_year,
    target_year,
    gamma = gamma
  )

  pop_tbl <- energy_consumption_build_bnef_population(bnef_neo)
  bnef_metrics <- energy_consumption_build_bnef_metrics(bnef_neo, pop_tbl, techs, country_info)

  dplyr::bind_rows(ei_target, ei_growth, bnef_metrics)
}
