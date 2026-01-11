# Energy prices theme builder functions.

# ---- Clean EI price sheet (gas/coal) ----
energy_prices_clean_ei_sheet <- function(price_sheet,
                                         price_col,
                                         drop_regex,
                                         eu_marker,
                                         country_recode,
                                         year_range = 2019:2024) {
  price_col <- rlang::ensym(price_col)

  price_sheet %>%
    dplyr::rename(Year = `...1`) %>%
    dplyr::filter(stringr::str_detect(Year, "^\\d{4}$")) %>%
    dplyr::select(-dplyr::matches(drop_regex)) %>%
    dplyr::mutate(
      dplyr::across(-Year, ~ dplyr::na_if(as.character(.x), "-")),
      dplyr::across(-Year, readr::parse_number)
    ) %>%
    dplyr::filter(Year %in% year_range) %>%
    tidyr::pivot_longer(
      cols = -Year,
      names_to = "Country",
      values_to = "Value",
      values_drop_na = TRUE
    ) %>%
    dplyr::mutate(
      Country = Country %>%
        stringr::str_remove_all("\\d+") %>%
        stringr::str_remove_all("\\s*\\(Mainland\\)") %>%
        stringr::str_trim() %>%
        dplyr::recode(!!!country_recode),
      EU = dplyr::if_else(Country == eu_marker, 1L, 0L),
      country2 = dplyr::if_else(EU == 1L, "EU", Country)
    ) %>%
    dplyr::group_by(country2) %>%
    dplyr::summarize(!!price_col := mean(Value, na.rm = TRUE), .groups = "drop")
}

# ---- Build EI price indices (gas + coal) ----
energy_prices_build_ei_indices <- function(ei, gas_prices, coal_prices, gamma = 0.5) {
  ei %>%
    dplyr::mutate(country2 = dplyr::if_else(EU == 1L, "EU", Country)) %>%
    dplyr::distinct(Country, country2) %>%
    dplyr::left_join(gas_prices %>% dplyr::select(country2, Gas), by = "country2") %>%
    dplyr::mutate(
      Country = dplyr::if_else(Country == "US", "United States", Country),
      country2 = dplyr::if_else(country2 == "US", "United States", country2)
    ) %>%
    dplyr::left_join(coal_prices %>% dplyr::select(country2, Coal), by = "country2") %>%
    dplyr::select(-country2) %>%
    dplyr::rename(
      gas_price = Gas,
      coal_price = Coal
    ) %>%
    dplyr::filter(!is.na(gas_price), !is.na(coal_price)) %>%
    dplyr::mutate(
      gas_price_index = median_scurve(gas_price, gamma = gamma),
      coal_price_index = median_scurve(coal_price, gamma = gamma)
    ) %>%
    tidyr::pivot_longer(
      cols = c(gas_price:coal_price_index),
      names_to = "tech",
      values_to = "value"
    )
}

# ---- Build EI energy price table ----
energy_prices_build_ei_table <- function(ei_price_indices, year_range = 2019:2024) {
  price_year <- paste0(min(year_range), "-", max(year_range))

  ei_price_indices %>%
    dplyr::mutate(
      data_type = dplyr::if_else(stringr::str_detect(tech, "_index$"), "index", "raw"),
      tech = tech %>%
        stringr::str_remove("_index$") %>%
        stringr::str_replace_all("_", " ") %>%
        stringr::str_to_sentence(),
      supply_chain = "Downstream",
      category = "Energy Prices",
      variable = "Fuel Prices",
      Year = price_year,
      source = "EI Statistical Review of World Energy (2024)",
      explanation = dplyr::case_when(
        data_type == "raw" ~ "Price",
        data_type == "index" ~ "Percent-rank of prices among countries"
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

# ---- Build BNEF LCOE table ----
energy_prices_build_lcoe_table <- function(lcoe_bnef, lcoe_years = c(2024, 2050), gamma = 0.5) {
  lcoe_bnef %>%
    dplyr::mutate(
      Technology = dplyr::recode(
        Technology,
        "CCGT" = "Gas",
        "Coal" = "Coal",
        "PV fixed-axis" = "Solar",
        "PV fixed-axis + storage" = "Solar",
        "Wind onshore" = "Wind",
        "Utility-scale battery (1h)" = "Batteries",
        "Utility-scale battery (4h)" = "Batteries",
        .default = NA_character_
      )
    ) %>%
    dplyr::filter(
      Scenario == "Mid",
      Metric == "LCOE",
      !is.na(Technology)
    ) %>%
    dplyr::group_by(Technology, Region) %>%
    dplyr::summarize(
      lcoe_24_raw = mean(as.numeric(X2024), na.rm = TRUE),
      lcoe_50_raw = mean(as.numeric(X2050), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(Technology) %>%
    dplyr::mutate(
      lcoe_24_index = median_scurve(-lcoe_24_raw, gamma = gamma),
      lcoe_50_index = median_scurve(-lcoe_50_raw, gamma = gamma)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(overall_lcoe_index = mean(dplyr::c_across(dplyr::ends_with("_index")), na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      Region,
      Technology,
      lcoe_24_raw,
      lcoe_50_raw,
      lcoe_24_index,
      lcoe_50_index,
      overall_lcoe_index
    ) %>%
    tidyr::pivot_longer(
      cols = c(lcoe_24_raw:overall_lcoe_index),
      names_to = c("variable", "data_type"),
      names_pattern = "(.*)_(raw|index)",
      values_to = "value"
    ) %>%
    dplyr::transmute(
      Country = Region,
      tech = Technology,
      supply_chain = "Downstream",
      category = "Energy Prices",
      variable,
      data_type,
      value,
      Year = paste0(min(lcoe_years), "-", max(lcoe_years)),
      source = "BNEF LCOE Estimates (2025)",
      explanation = dplyr::case_when(
        data_type == "raw" ~ "Levelized cost of energy",
        data_type == "index" ~ "Percent-rank of LCOE across countries"
      )
    )
}

# ---- Energy prices theme ----
energy_prices <- function(ei,
                          gas_price_sheet,
                          coal_price_sheet,
                          lcoe_bnef,
                          year_range = 2019:2024,
                          gamma = 0.5) {
  gas_prices <- energy_prices_clean_ei_sheet(
    price_sheet = gas_price_sheet,
    price_col = "Gas",
    drop_regex = "US Gulf Coast6|Northwest Europe6|Middle East7|Far East Asia6",
    eu_marker = "Zeebrugge",
    country_recode = c("UK" = "United Kingdom"),
    year_range = year_range
  )

  coal_prices <- energy_prices_clean_ei_sheet(
    price_sheet = coal_price_sheet,
    price_col = "Coal",
    drop_regex = "Canada9",
    eu_marker = "Northwest Europe",
    country_recode = c(
      "South China" = "China",
      "UK" = "United Kingdom"
    ),
    year_range = year_range
  )

  ei_price_indices <- energy_prices_build_ei_indices(
    ei = ei,
    gas_prices = gas_prices,
    coal_prices = coal_prices,
    gamma = gamma
  )

  ei_prices_tbl <- energy_prices_build_ei_table(
    ei_price_indices = ei_price_indices,
    year_range = year_range
  )

  lcoe_tbl <- energy_prices_build_lcoe_table(
    lcoe_bnef = lcoe_bnef,
    gamma = gamma
  )

  energy_security_add_overall_index(
    dplyr::bind_rows(ei_prices_tbl, lcoe_tbl)
  )
}
