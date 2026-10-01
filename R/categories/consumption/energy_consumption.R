# Energy consumption theme builder functions.

# === Energy consumption (EI per-capita) ===
# This section mirrors the legacy energy-consumption-per-capita pipeline and
# keeps the same normalization and growth calculations.

# ---- Clean EI raw inputs ----
# Filter and reshape the EI statistical review data for per-capita consumption
# metrics and ensure missing fuels resolve to zero.
energy_consumption_clean_ei <- function(ei, year) {
  # Names are standardized before anything is ranked, and EI's CIS aggregate "USSR" (still
  # published for the latest year) is dropped with the other aggregates: left in, it was
  # ranked as a country and later collapsed onto Russia's ISO3.
  ei %>%
    dplyr::mutate(Country = standardize_country_names(Country)) %>%
    dplyr::filter(
      Year == year,
      Var %in% c("pop", "coal_tes_ej", "oil_tes_ej", "gas_tes_ej",
                "solar_tes_ej", "wind_tes_ej", "nuclear_tes_ej"),
      !grepl("World|Other|Total|OECD|OPEC|USSR", Country)
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
      source = "EI Statistical Review of World Energy (2025)",
      explanation = dplyr::case_when(
        data_type == "raw" ~ stringr::str_glue(
          "Per-capita {tech} consumption = {stringr::str_to_lower(tech)}_tes_ej ÷ pop"
        ),
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
    # Normalize each tech's growth ACROSS countries, matching how the level index in
    # energy_consumption_build_ei_indices() scales each tech column. Grouping by Country
    # instead ranks a country's fuels against each other, which hands every country a
    # 1.000 for whichever of its own fuels grew fastest.
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
      Country = standardize_country_names(Region),
      pop_2024 = as.numeric(X2024),
      pop_2035 = as.numeric(X2035)
    )
}

# Which BNEF NEO aggregate region each WDI country falls in, for countries BNEF does not
# model on their own.
#
# BNEF splits East Asia & Pacific into "Other Southeast Asia" and "Other Asia Pacific". Both
# used to be recoded to the WDI region "East Asia & Pacific", so every country there was
# expanded twice (two rows per country, tech and variable). The UN sub-region separates them.
# Turkiye is the "T" in BNEF's MENAT, although WDI files it under Europe & Central Asia.
# WDI South Asia and North America have no BNEF aggregate and are left out, as before.
energy_consumption_bnef_region_map <- function(country_info) {
  country_info %>%
    dplyr::distinct(country, .keep_all = TRUE) %>%
    dplyr::mutate(
      un_subregion = suppressWarnings(
        countrycode::countrycode(iso3c, "iso3c", "un.regionsub.name", warn = FALSE)
      ),
      bnef_region = dplyr::case_when(
        iso3c %in% "TUR" ~ "MENAT",
        region == "Latin America & Caribbean" ~ "Other Latin America",
        region == "Middle East & North Africa" ~ "MENAT",
        region == "Europe & Central Asia" ~ "Other Europe",
        region == "Sub-Saharan Africa" ~ "Sub-Saharan Africa",
        region == "East Asia & Pacific" & un_subregion %in% "South-eastern Asia" ~ "Other Southeast Asia",
        region == "East Asia & Pacific" ~ "Other Asia Pacific",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(bnef_region)) %>%
    dplyr::select(country, bnef_region)
}

# ---- Build BNEF per-capita capacity metrics ----
# Filter to installed capacity/final energy consumption, normalize per-capita,
# and compute legacy share + growth indices.
energy_consumption_build_bnef_metrics <- function(bnef_neo, pop_tbl, techs, country_info) {
  region_map <- energy_consumption_bnef_region_map(country_info)

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
        "Small-scale PV" = "Solar",
        # Without these Wind never matched `techs`, so Wind Downstream had no Consumption.
        "Onshore wind" = "Wind",
        "Offshore wind" = "Wind"
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
      # Standardized before the region expansion tests membership: BNEF's "Vietnam" never
      # matched WDI's "Viet Nam", so Viet Nam got its own row and a regional copy.
      Country = standardize_country_names(Country)
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
    # Both indices rank countries within a technology. The growth index used to be ranked
    # after ungroup(), i.e. pooled across techs, so it mostly said which technology grows
    # fastest (batteries, hydrogen) rather than which country grows fastest in it - the issue
    # the EI growth index had already been fixed for (energy_consumption_build_ei_growth()).
    dplyr::group_by(tech) %>%
    dplyr::mutate(
      installed_cap_index = median_scurve(X2024_pc),
      elec_growth_index = median_scurve(growth_2435)
    ) %>%
    dplyr::ungroup() %>%
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
    # Aggregate regions are copied to their member countries; a country BNEF models on its
    # own matches no region here and keeps its own row.
    dplyr::left_join(region_map, by = c("Country" = "bnef_region"), relationship = "many-to-many")

  bnef_countries <- unique(neo_cap$Country)

  neo_cap %>%
    dplyr::filter(!country %in% bnef_countries) %>%
    dplyr::mutate(Country = dplyr::if_else(is.na(country), Country, country)) %>%
    dplyr::mutate(
      supply_chain = "Downstream",
      category = "Consumption",
      data_type = dplyr::if_else(variable %in% c("installed_cap_index", "elec_growth_index"), "index", "raw"),
      # The base year of the per-capita levels (growth runs 2024-35). It was NA, which the
      # index builders' year handling could only read as "no year".
      Year = 2024L,
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
                               base_year = 2020,
                               target_year = 2025,
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

  standardized <- lapply(
    list(ei_target, ei_growth, bnef_metrics),
    standardize_bind_rows_inputs
  )

  energy_security_add_overall_index(
    dplyr::bind_rows(standardized)
  )
}
