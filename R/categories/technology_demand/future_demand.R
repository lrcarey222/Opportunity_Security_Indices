# Technology Demand theme (future demand).
future_demand_expand_global <- function(tbl, country_reference) {
  if (is.null(country_reference)) {
    stop("country_reference is required for future_demand global data.")
  }

  countries <- country_reference
  if (is.vector(countries) && !is.list(countries)) {
    countries <- tibble::tibble(Country = as.character(countries))
  }

  require_columns(countries, "Country", label = "country_reference")
  countries <- countries %>%
    dplyr::transmute(Country = as.character(Country)) %>%
    dplyr::distinct()

  assert_unique_keys(countries, "Country", label = "country_reference")
  tidyr::crossing(countries, tbl)
}

future_demand_clean_weo <- function(iea_weo) {
  require_columns(
    iea_weo,
    c("SCENARIO", "CATEGORY", "PRODUCT", "FLOW", "YEAR", "VALUE"),
    label = "iea_weo"
  )

  iea_weo %>%
    dplyr::filter(
      SCENARIO == "Stated Policies Scenario",
      CATEGORY == "Energy",
      PRODUCT %in% c(
        "Solar",
        "Wind",
        "Nuclear",
        "Natural gas",
        "Oil",
        "Coal",
        "Solar PV",
        "Hydrogen: low-emissions",
        "Geothermal"
      ),
      FLOW == "Total energy supply" | stringr::str_detect(FLOW, "Hydrogen")
    )
}

future_demand_build_weo <- function(iea_weo, country_reference, gamma = 0.5) {
  weo_wide <- future_demand_clean_weo(iea_weo) %>%
    dplyr::group_by(YEAR, PRODUCT) %>%
    dplyr::summarize(VALUE = sum(VALUE, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = "YEAR", values_from = "VALUE")

  required_years <- c("2022", "2035")
  missing_years <- setdiff(required_years, names(weo_wide))
  if (length(missing_years) > 0) {
    stop("IEA WEO data missing year columns: ", paste(missing_years, collapse = ", "))
  }

  weo_wide <- weo_wide %>%
    dplyr::mutate(
      `2022` = as.numeric(`2022`),
      `2035` = as.numeric(`2035`),
      growth = `2035` - `2022`
    )

  weo_indexed <- weo_wide %>%
    dplyr::mutate(
      PRODUCT = dplyr::case_when(
        PRODUCT == "Natural gas" ~ "Gas",
        stringr::str_detect(PRODUCT, "Hydrogen") ~ "Green Hydrogen",
        TRUE ~ PRODUCT
      ),
      demand_size_index = median_scurve(`2035`, gamma = gamma),
      demand_growth_index = median_scurve(growth, gamma = gamma)
    ) %>%
    dplyr::rename(tech = PRODUCT) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      demand_index = (demand_size_index + 2 * demand_growth_index) / 3
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(demand_index = median_scurve(demand_index, gamma = gamma)) %>%
    dplyr::mutate(
      Upstream = "Upstream",
      Midstream = "Midstream",
      Downstream = "Downstream"
    ) %>%
    tidyr::pivot_longer(
      cols = c(Upstream:Downstream),
      names_to = "supply_chain",
      values_to = "supply_chain2"
    ) %>%
    dplyr::select(tech, supply_chain, `2035`, growth, demand_size_index, demand_growth_index, demand_index) %>%
    tidyr::pivot_longer(
      cols = c(`2035`:demand_index),
      names_to = "variable",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      data_type = dplyr::if_else(stringr::str_ends(variable, "_index"), "index", "raw"),
      variable = stringr::str_remove(variable, "_index$")
    ) %>%
    dplyr::mutate(
      category = "Technology Demand",
      source = "IEA World Energy Outlook",
      explanation = dplyr::case_when(
        variable == "2035" ~ "Global energy demand in 2035",
        variable == "growth" ~ "Modelled demand growth, 2022-2035, IEA States Policies Scenario",
        variable == "demand_size" ~ "Index of global energy demand in 2035",
        variable == "demand_growth_index" ~
          "Index of modelled demand growth, 2022-2035, IEA States Policies Scenario",
        variable == "demand_index" ~ "Mean of growth and size indices",
        TRUE ~ variable
      )
    ) %>%
    dplyr::mutate(variable = dplyr::if_else(variable == "demand", "Overall Global Demand", variable)) %>%
    dplyr::mutate(
      Year = 2035L,
      supply_chain = as.character(supply_chain)
    ) %>%
    dplyr::select(
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

  future_demand_expand_global(weo_indexed, country_reference = country_reference)
}

future_demand_build_bnef <- function(bnef_neo, gamma = 0.5) {
  require_columns(
    bnef_neo,
    c(
      "Scenario",
      "Indicator",
      "Region",
      "Sector",
      "Fuel.type",
      "X2024",
      "X2035",
      "Macro.sector"
    ),
    label = "bnef_neo"
  )

  bnef_neo %>%
    dplyr::filter(
      Scenario == "ETS",
      Indicator %in% c("Primary energy consumption", "Final energy consumption"),
      !Region %in% c("Global", "MENAT"),
      !grepl("Other|Rest|Africa", Region),
      Sector == "All sectors",
      Fuel.type %in% c(
        "Electric Vehicles",
        "Nuclear",
        "Coal",
        "Batteries",
        "Hydrogen",
        "Wind",
        "Oil",
        "Solar",
        "Gas",
        "Geothermal"
      )
    ) %>%
    dplyr::mutate(
      Region = standardize_country_names(Region)
    ) %>%
    dplyr::mutate(
      dplyr::across(dplyr::matches("^X\\d{4}$"), as.numeric),
      growth = (X2035 - X2024) / X2024
    ) %>%
    dplyr::group_by(Fuel.type) %>%
    dplyr::mutate(
      demand_size_index = median_scurve(X2035, gamma = gamma),
      demand_growth_index = median_scurve(growth, gamma = gamma)
    ) %>%
    dplyr::rename(tech = Fuel.type) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      demand_index = (demand_size_index + 2 * demand_growth_index) / 3
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(demand_index = median_scurve(demand_index, gamma = gamma)) %>%
    dplyr::mutate(
      Upstream = "Upstream",
      Midstream = "Midstream",
      Downstream = "Downstream"
    ) %>%
    tidyr::pivot_longer(
      cols = c(Upstream:Downstream),
      names_to = "supply_chain",
      values_to = "supply_chain2"
    ) %>%
    dplyr::select(Region, tech, supply_chain, X2024, X2035, growth, demand_size_index, demand_growth_index, demand_index) %>%
    tidyr::pivot_longer(
      cols = c(X2024:demand_index),
      names_to = "variable",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      data_type = dplyr::if_else(stringr::str_ends(variable, "_index"), "index", "raw"),
      variable = stringr::str_remove(variable, "_index$")
    ) %>%
    dplyr::transmute(
      Country = Region,
      tech,
      supply_chain,
      category = "Technology Demand",
      variable,
      data_type,
      value,
      Year = 2035L,
      source = "BNEF New Energy Outlook",
      explanation = dplyr::case_when(
        variable == "X2035" ~ "Energy demand (PJ) in 2035",
        variable == "growth" ~ "Modelled demand growth, 2024-2035, BNEF ETS Scenario",
        variable == "demand_size" ~ "Index of energy demand in 2035",
        variable == "demand_growth_index" ~
          "Index of modelled demand growth, 2024-2035, BNEF ETS Scenario",
        variable == "demand_index" ~ "Mean of growth and size indices",
        TRUE ~ variable
      )
    ) %>%
    dplyr::mutate(variable = dplyr::if_else(variable == "demand", "Overall Demand", variable))
}

future_demand_build_ev <- function(iea_ev, country_info, gamma = 0.5) {
  require_columns(
    iea_ev,
    c("region_country", "mode", "parameter", "year", "value"),
    label = "iea_ev"
  )
  require_columns(country_info, c("iso3c", "country"), label = "country_info")
  assert_unique_keys(country_info, "country", label = "country_info")

  evs <- iea_ev %>%
    dplyr::mutate(region_country = standardize_country_names(region_country)) %>%
    dplyr::inner_join(
      country_info %>% dplyr::select(iso3c, country),
      by = c("region_country" = "country")
    ) %>%
    dplyr::filter(
      mode == "Cars",
      parameter %in% c("EV stock", "EV sales", "EV sales share")
    ) %>%
    dplyr::group_by(region_country, parameter, year) %>%
    dplyr::summarize(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = "year", values_from = "value")

  required_years <- c("2021", "2024", "2030")
  missing_years <- setdiff(required_years, names(evs))
  if (length(missing_years) > 0) {
    stop("IEA EV data missing year columns: ", paste(missing_years, collapse = ", "))
  }

  evs <- evs %>%
    dplyr::mutate(
      growth_2124 = `2024` / `2021` - 1,
      forecast_growth = `2030` / `2024` - 1
    ) %>%
    dplyr::group_by(parameter) %>%
    dplyr::mutate(
      growth_index = median_scurve(growth_2124, gamma = gamma),
      size_index = median_scurve(`2024`, gamma = gamma),
      forecast_growth_index = median_scurve(forecast_growth, gamma = gamma),
      forecast_size_index = median_scurve(`2030`, gamma = gamma)
    ) %>%
    dplyr::select(
      region_country,
      parameter,
      growth_index,
      size_index,
      forecast_growth_index,
      forecast_size_index,
      `2030`,
      `2024`,
      `2021`,
      growth_2124,
      forecast_growth
    )

  value_cols <- c(
    "growth_index",
    "size_index",
    "forecast_growth_index",
    "forecast_size_index",
    "2030",
    "2024",
    "growth_2124",
    "forecast_growth"
  )

  evs %>%
    dplyr::mutate(
      param_tag = dplyr::case_when(
        stringr::str_detect(tolower(parameter), "sales share") ~ "share",
        stringr::str_detect(tolower(parameter), "stock") ~ "stock",
        stringr::str_detect(tolower(parameter), "sales") ~ "sales",
        TRUE ~ "other"
      )
    ) %>%
    tidyr::pivot_wider(
      id_cols = region_country,
      names_from = param_tag,
      values_from = dplyr::all_of(value_cols),
      names_glue = "{param_tag}_{.value}",
      values_fn = \(x) dplyr::first(na.omit(x))
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ev_index = mean(dplyr::c_across(dplyr::ends_with("_index")), na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ev_index = median_scurve(ev_index, gamma = gamma)) %>%
    dplyr::arrange(dplyr::desc(ev_index)) %>%
    dplyr::select(region_country, sales_2030:ev_index) %>%
    tidyr::pivot_longer(
      cols = c(sales_2030:ev_index),
      names_to = "variable",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      data_type = dplyr::if_else(stringr::str_ends(variable, "_index"), "index", "raw"),
      variable = stringr::str_remove(variable, "_index$")
    ) %>%
    dplyr::transmute(
      Country = region_country,
      tech = "Electric Vehicles",
      supply_chain = "Downstream",
      category = "Technology Demand",
      variable,
      data_type,
      value,
      Year = 2030L,
      source = "IEA EVs Outlook",
      explanation = dplyr::case_when(
        variable == "ev_index" ~
          "Index of EV sales, stock and sales share, growth and absolute (index)",
        TRUE ~ variable
      )
    ) %>%
    dplyr::mutate(variable = dplyr::if_else(variable == "ev", "Overall Demand", variable))
}

future_demand_build_bcg <- function(bcg, gamma = 0.5) {
  require_columns(bcg, c("Energy source", "Upstream", "Midstream", "Downstream"), label = "bcg")

  bcg %>%
    dplyr::rename(tech = `Energy source`) %>%
    tidyr::pivot_longer(
      cols = c(Upstream, Midstream, Downstream),
      names_to = "supply_chain",
      values_to = "SAM"
    ) %>%
    dplyr::mutate(
      sam_index = median_scurve(SAM, gamma = gamma),
      Country = "United States"
    ) %>%
    tidyr::pivot_longer(
      cols = c(SAM, sam_index),
      names_to = "variable",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      data_type = dplyr::if_else(stringr::str_ends(variable, "_index"), "index", "raw"),
      variable = stringr::str_remove(variable, "_index$")
    ) %>%
    dplyr::mutate(
      category = "Technology Demand",
      source = "BCG",
      explanation = dplyr::case_when(
        variable == "SAM" ~ "Serviceable Addressable Market, 2020-2050",
        variable == "sam" ~ "Serviceable Addressable Market, 2020-2050 (index)",
        TRUE ~ variable
      )
    ) %>%
    dplyr::mutate(variable = dplyr::if_else(variable == "sam", "Overall Addressable Market", variable)) %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      category,
      variable,
      data_type,
      value,
      Year = 2030L,
      source,
      explanation
    )
}

future_demand <- function(iea_weo,
                          bnef_neo,
                          iea_ev,
                          bcg,
                          country_info,
                          country_reference,
                          gamma = 0.5) {
  weo_tbl <- future_demand_build_weo(iea_weo, country_reference, gamma = gamma)
  bnef_tbl <- future_demand_build_bnef(bnef_neo, gamma = gamma)
  ev_tbl <- future_demand_build_ev(iea_ev, country_info, gamma = gamma)
  bcg_tbl <- future_demand_build_bcg(bcg, gamma = gamma)

  standardized <- list(weo_tbl, bnef_tbl, ev_tbl, bcg_tbl) %>%
    lapply(standardize_theme_table)

  output <- dplyr::bind_rows(standardized)
  validate_schema(output)
  output
}
