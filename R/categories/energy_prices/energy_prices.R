# Energy prices theme builder functions (IMF price volatility).

energy_prices_imf_patterns <- list(
  Aluminum = "aluminum.*(unit prices|us dollars|usd)",
  Oil_APSP = "apsp.*crude oil|crude oil.*apsp",
  Oil_Brent = "brent.*crude|brent.*oil",
  Oil_WTI = "wti.*crude|wti.*oil",
  Chromium = "chromium",
  Coal = "coal",
  Cobalt = "cobalt",
  Copper = "copper",
  Diammonium_Phosphate = "diammonium phosphate",
  Dubai_Crude = "dubai.*crude|crude.*dubai",
  Energy_Index = "^energy index|commodity price index.*energy",
  Energy_Transition_Metal_Index = "energy transition metal index",
  All_Metals_Index = "^all metals index",
  Base_Metals_Index = "^base metals index",
  Iron_Ore = "iron ore",
  Lithium = "lithium",
  Lead = "\\blead\\b",
  LNG = "lng|liquefied natural gas",
  Manganese = "manganese",
  Molybdenum = "\\bmolybdenum\\b",
  Natural_Gas_Index = "natural gas index|commodity price index.*natural gas",
  Natural_Gas_EU = "natural gas.*eu",
  Natural_Gas_Henry_Hub = "henry hub|us henry hub",
  Nickel = "nickel",
  Potassium_Fertilizer = "potassium fertilizer",
  Propane = "\\bpropane\\b",
  Rare_Earths = "rare earth",
  Silicon = "silicon",
  Tin = "\\btin\\b",
  Urea = "\\burea\\b",
  Uranium = "uranium",
  Vanadium = "\\bvanadium\\b",
  Zinc = "zinc"
)

energy_prices_normalize_mineral <- function(x) {
  normalized <- stringr::str_to_lower(stringr::str_replace_all(x, "_", " "))

  dplyr::case_when(
    stringr::str_detect(normalized, "rare earth") ~ "rare earth",
    TRUE ~ normalized
  )
}

energy_prices_imf_monthly_long <- function(imf_price) {
  monthly_re <- "^X\\d{4}\\.M\\d{2}$"

  monthly_source <- imf_price

  if ("FREQUENCY" %in% names(monthly_source)) {
    monthly_source <- monthly_source %>% dplyr::filter(FREQUENCY == "Monthly")
  }

  if ("DATA_TRANSFORMATION" %in% names(monthly_source)) {
    monthly_source <- monthly_source %>% dplyr::filter(DATA_TRANSFORMATION == "US dollars")
  }

  monthly_source %>%
    dplyr::select(INDICATOR, dplyr::matches(monthly_re)) %>%
    tidyr::pivot_longer(
      cols = dplyr::matches(monthly_re),
      names_to = "period",
      values_to = "value_raw"
    ) %>%
    dplyr::mutate(
      year = as.integer(stringr::str_match(period, "^X(\\d{4})\\.M\\d{2}$")[, 2]),
      month = as.integer(stringr::str_match(period, "^X\\d{4}\\.M(\\d{2})$")[, 2]),
      date = as.Date(sprintf("%04d-%02d-01", year, month)),
      value = suppressWarnings(as.numeric(stringr::str_replace_all(as.character(value_raw), ",", "")))
    ) %>%
    dplyr::select(INDICATOR, date, value) %>%
    dplyr::filter(!is.na(date))
}


energy_prices_imf_monthly_usd_long <- function(imf_price) {
  if (!all(c("FREQUENCY", "DATA_TRANSFORMATION") %in% names(imf_price))) {
    return(energy_prices_imf_monthly_long(imf_price))
  }

  imf_price %>%
    dplyr::filter(
      FREQUENCY == "Monthly",
      DATA_TRANSFORMATION == "US dollars"
    ) %>%
    energy_prices_imf_monthly_long()
}

energy_prices_imf_annual_yoy_long <- function(imf_price) {
  year_re <- "^X\\d{4}$"

  if (!all(c("INDICATOR", "FREQUENCY", "DATA_TRANSFORMATION") %in% names(imf_price))) {
    return(tibble::tibble(INDICATOR = character(), date = as.Date(character()), value = numeric()))
  }

  imf_price %>%
    dplyr::filter(
      FREQUENCY == "Annual",
      DATA_TRANSFORMATION == "Index, percent change from a year ago"
    ) %>%
    dplyr::select(INDICATOR, dplyr::matches(year_re)) %>%
    tidyr::pivot_longer(
      cols = dplyr::matches(year_re),
      names_to = "period",
      values_to = "value_raw"
    ) %>%
    dplyr::mutate(
      year = as.integer(stringr::str_match(period, "^X(\\d{4})$")[, 2]),
      date = as.Date(sprintf("%04d-01-01", year)),
      value = suppressWarnings(as.numeric(stringr::str_replace_all(as.character(value_raw), ",", "")))
    ) %>%
    dplyr::select(INDICATOR, date, value) %>%
    dplyr::filter(!is.na(date), !is.na(value))
}

energy_prices_long_from_pcps <- function(imf_price) {
  if (is.null(imf_price) || nrow(imf_price) == 0) {
    return(tibble::tibble(INDICATOR = character(), date = as.Date(character()), value = numeric()))
  }

  indicator <- if ("INDICATOR" %in% names(imf_price)) {
    imf_price$INDICATOR
  } else if ("commodity_label" %in% names(imf_price)) {
    imf_price$commodity_label
  } else if ("commodity_code" %in% names(imf_price)) {
    imf_price$commodity_code
  } else if ("tech" %in% names(imf_price)) {
    imf_price$tech
  } else {
    NA_character_
  }

  date_str <- as.character(imf_price$date)
  date_str <- dplyr::case_when(
    nchar(date_str) == 7 ~ paste0(date_str, "-01"),
    nchar(date_str) == 4 ~ paste0(date_str, "-01-01"),
    TRUE ~ date_str
  )

  tibble::tibble(
    INDICATOR = as.character(indicator),
    date = as.Date(date_str),
    value = suppressWarnings(as.numeric(imf_price$value))
  ) %>%
    dplyr::filter(!is.na(date))
}

energy_prices_imf_clean <- function(imf_monthly_long,
                                    patterns = energy_prices_imf_patterns,
                                    include_optional_indices = FALSE) {
  imf_monthly_long %>%
    dplyr::mutate(ind_lc = stringr::str_to_lower(INDICATOR)) %>%
    dplyr::mutate(
      clean = dplyr::case_when(
        stringr::str_detect(ind_lc, stringr::regex(patterns$Aluminum, ignore_case = TRUE)) ~ "Aluminum",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Oil_APSP, ignore_case = TRUE)) ~ "Oil_APSP",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Oil_Brent, ignore_case = TRUE)) ~ "Oil_Brent",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Dubai_Crude, ignore_case = TRUE)) ~ "Dubai_Crude",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Oil_WTI, ignore_case = TRUE)) ~ "Oil_WTI",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Chromium, ignore_case = TRUE)) ~ "Chromium",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Coal, ignore_case = TRUE)) ~ "Coal",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Cobalt, ignore_case = TRUE)) ~ "Cobalt",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Copper, ignore_case = TRUE)) ~ "Copper",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Diammonium_Phosphate, ignore_case = TRUE)) ~ "Diammonium_Phosphate",
        include_optional_indices && stringr::str_detect(ind_lc, stringr::regex(patterns$Energy_Index, ignore_case = TRUE)) ~ "Energy_Index",
        include_optional_indices && stringr::str_detect(ind_lc, stringr::regex(patterns$Energy_Transition_Metal_Index, ignore_case = TRUE)) ~ "Energy_Transition_Metal_Index",
        include_optional_indices && stringr::str_detect(ind_lc, stringr::regex(patterns$All_Metals_Index, ignore_case = TRUE)) ~ "All_Metals_Index",
        include_optional_indices && stringr::str_detect(ind_lc, stringr::regex(patterns$Base_Metals_Index, ignore_case = TRUE)) ~ "Base_Metals_Index",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Iron_Ore, ignore_case = TRUE)) ~ "Iron_Ore",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Lithium, ignore_case = TRUE)) ~ "Lithium",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Lead, ignore_case = TRUE)) ~ "Lead",
        stringr::str_detect(ind_lc, stringr::regex(patterns$LNG, ignore_case = TRUE)) ~ "LNG",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Manganese, ignore_case = TRUE)) ~ "Manganese",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Molybdenum, ignore_case = TRUE)) ~ "Molybdenum",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Natural_Gas_Index, ignore_case = TRUE)) ~ "Natural_Gas_Index",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Natural_Gas_EU, ignore_case = TRUE)) ~ "Natural_Gas_EU",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Natural_Gas_Henry_Hub, ignore_case = TRUE)) ~ "Natural_Gas_Henry_Hub",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Nickel, ignore_case = TRUE)) ~ "Nickel",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Potassium_Fertilizer, ignore_case = TRUE)) ~ "Potassium_Fertilizer",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Propane, ignore_case = TRUE)) ~ "Propane",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Rare_Earths, ignore_case = TRUE)) ~ "Rare_Earths",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Silicon, ignore_case = TRUE)) ~ "Silicon",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Tin, ignore_case = TRUE)) ~ "Tin",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Urea, ignore_case = TRUE)) ~ "Urea",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Uranium, ignore_case = TRUE)) ~ "Uranium",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Vanadium, ignore_case = TRUE)) ~ "Vanadium",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Zinc, ignore_case = TRUE)) ~ "Zinc",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(-ind_lc) %>%
    dplyr::filter(!is.na(value), !is.na(clean))
}

energy_prices_pcps_match_report <- function(imf_monthly_long,
                                            patterns = energy_prices_imf_patterns,
                                            include_optional_indices = FALSE) {
  matched <- energy_prices_imf_clean(
    imf_monthly_long = imf_monthly_long,
    patterns = patterns,
    include_optional_indices = include_optional_indices
  ) %>%
    dplyr::distinct(INDICATOR, matched_clean = clean)

  imf_monthly_long %>%
    dplyr::distinct(INDICATOR) %>%
    dplyr::left_join(matched, by = "INDICATOR") %>%
    dplyr::mutate(matched = !is.na(matched_clean))
}

energy_prices_extra_mineral_map <- function(include_fertilizer_inputs = FALSE) {
  # PCPS-only extensions for transition-input coverage where IEA mineral map is sparse.
  extra_map <- tibble::tribble(
    ~clean_key, ~tech,
    "vanadium", "Batteries",
    "rare earth", "Wind",
    "iron ore", "Batteries",
    "iron ore", "Electric Grid",
    "iron ore", "Wind",
    "iron ore", "Solar",
    "diammonium phosphate", "Batteries"
  )

  if (include_fertilizer_inputs) {
    extra_map <- dplyr::bind_rows(
      extra_map,
      tibble::tribble(
        ~clean_key, ~tech,
        "urea", "Green Hydrogen",
        "potassium fertilizer", "Green Hydrogen"
      )
    )
  }

  extra_map
}

energy_prices_calc_vol <- function(df, years_back, min_months = 24) {
  stopifnot(all(c("date", "value") %in% names(df)))
  end_date <- max(df$date, na.rm = TRUE)
  start_date <- lubridate::`%m-%`(end_date, lubridate::years(years_back))

  x <- df %>%
    dplyr::filter(date > start_date, date <= end_date) %>%
    dplyr::arrange(date) %>%
    dplyr::filter(!is.na(value))

  n_obs <- nrow(x)

  lr <- x %>%
    dplyr::filter(value > 0) %>%
    dplyr::mutate(log_ret = log(value) - dplyr::lag(log(value))) %>%
    dplyr::pull(log_ret)

  lr <- lr[is.finite(lr) & !is.na(lr)]

  tibble::tibble(
    window_years = years_back,
    end_date = end_date,
    start_date = start_date,
    n_months = n_obs,
    vol_logret_annualized = if (length(lr) >= min_months) sqrt(12) * stats::sd(lr, na.rm = TRUE) else NA_real_,
    vol_level_sd = if (n_obs >= min_months) stats::sd(x$value, na.rm = TRUE) else NA_real_,
    vol_level_cv = if (n_obs >= min_months) stats::sd(x$value, na.rm = TRUE) / mean(x$value, na.rm = TRUE) else NA_real_
  )
}

energy_prices_extract_unit <- function(indicator) {
  indicator_lc <- stringr::str_to_lower(indicator)

  dplyr::case_when(
    stringr::str_detect(indicator_lc, "\\$/bbl|\\busd per barrel\\b|\\bus dollars per barrel\\b") ~ "USD per barrel",
    stringr::str_detect(indicator_lc, "us cents per gallon") ~ "US cents per gallon",
    stringr::str_detect(indicator_lc, "us dollars per metric tonne|usd per metric tonne") ~ "USD per metric tonne",
    stringr::str_detect(indicator_lc, "us dollars per mmbtu|usd per mmbtu") ~ "USD per MMBtu",
    stringr::str_detect(indicator_lc, "us dollars per kilogram|usd per kilogram") ~ "USD per kilogram",
    stringr::str_detect(indicator_lc, "unit prices") ~ "Indicator-native IMF unit",
    TRUE ~ "Indicator-native IMF unit"
  )
}

energy_prices_latest_and_yoy <- function(df) {
  x <- df %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::arrange(date)

  if (nrow(x) == 0) {
    return(tibble::tibble(latest_price = NA_real_, yoy_price_change_pct = NA_real_))
  }

  latest_row <- x %>% dplyr::slice_tail(n = 1)
  latest_date <- latest_row$date[[1]]
  latest_price <- latest_row$value[[1]]

  shift_months <- function(date, n_months) {
    as.Date(seq(date, by = sprintf("%+d months", n_months), length.out = 2)[2])
  }

  current_window_start <- shift_months(latest_date, -11)
  previous_window_end <- shift_months(current_window_start, -1)
  previous_window_start <- shift_months(previous_window_end, -11)

  current_window <- x %>%
    dplyr::filter(date >= current_window_start, date <= latest_date)
  previous_window <- x %>%
    dplyr::filter(date >= previous_window_start, date <= previous_window_end)

  current_avg <- if (nrow(current_window) == 12) mean(current_window$value, na.rm = TRUE) else NA_real_
  previous_avg <- if (nrow(previous_window) == 12) mean(previous_window$value, na.rm = TRUE) else NA_real_

  yoy_price_change_pct <- if (is.finite(current_avg) && is.finite(previous_avg) && !is.na(previous_avg) && previous_avg != 0) {
    100 * (current_avg / previous_avg - 1)
  } else {
    NA_real_
  }

  tibble::tibble(
    latest_price = latest_price,
    yoy_price_change_pct = yoy_price_change_pct
  )
}

energy_prices_sub_sector_unit_lookup <- function(imf_price,
                                                 include_optional_indices = FALSE) {
  imf_monthly_long <- if (all(c("date", "value") %in% names(imf_price))) {
    energy_prices_long_from_pcps(imf_price)
  } else {
    energy_prices_imf_monthly_long(imf_price)
  }

  imf_monthly <- energy_prices_imf_clean(
    imf_monthly_long = imf_monthly_long,
    include_optional_indices = include_optional_indices
  )

  imf_monthly %>%
    dplyr::mutate(
      sub_sector = dplyr::case_when(
        clean %in% c("Oil_APSP", "Oil_Brent", "Oil_WTI", "Dubai_Crude") ~ clean,
        clean %in% c("Natural_Gas_Index", "Natural_Gas_EU", "Natural_Gas_Henry_Hub", "LNG", "Propane") ~ clean,
        clean == "Coal" ~ "Coal",
        TRUE ~ clean
      ),
      unit = energy_prices_extract_unit(INDICATOR)
    ) %>%
    dplyr::group_by(sub_sector) %>%
    dplyr::summarize(
      unit_description = paste(sort(unique(unit[!is.na(unit)])), collapse = "; "),
      .groups = "drop"
    ) %>%
    dplyr::arrange(sub_sector)
}

energy_prices_imf_annual_yoy_lookup <- function(imf_price,
                                                include_optional_indices = FALSE) {
  yoy_long <- energy_prices_imf_annual_yoy_long(imf_price)
  if (nrow(yoy_long) == 0) {
    return(tibble::tibble(INDICATOR = character(), clean = character(), yoy_price_change_pct_annual = numeric()))
  }

  energy_prices_imf_clean(
    imf_monthly_long = yoy_long,
    include_optional_indices = include_optional_indices
  ) %>%
    dplyr::group_by(INDICATOR, clean) %>%
    dplyr::arrange(date, .by_group = TRUE) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      INDICATOR,
      clean,
      yoy_price_change_pct_annual = value
    )
}

energy_prices_build_volatility <- function(imf_monthly,
                                           mineral_demand_clean,
                                           years_back = c(5, 10, 20),
                                           min_months = 24,
                                           include_fertilizer_inputs = FALSE,
                                           annual_yoy_lookup = NULL,
                                           imf_monthly_latest = NULL) {
  if (is.null(annual_yoy_lookup)) {
    annual_yoy_lookup <- tibble::tibble(
      INDICATOR = character(),
      clean = character(),
      yoy_price_change_pct_annual = numeric()
    )
  }

  if (is.null(imf_monthly_latest)) {
    imf_monthly_latest <- imf_monthly
  }

  tech_groups <- c(
    "Electric Vehicles", "Nuclear", "Coal", "Batteries", "Green Hydrogen",
    "Wind", "Oil", "Solar", "Gas", "Geothermal", "Electric Grid"
  )

  mineral_map <- mineral_demand_clean %>%
    dplyr::mutate(
      Mineral = dplyr::if_else(
        stringr::str_detect(Mineral, stringr::regex("graphite", ignore_case = TRUE)),
        "Graphite",
        Mineral
      ),
      clean_key = energy_prices_normalize_mineral(Mineral)
    ) %>%
    dplyr::select(clean_key, tech) %>%
    dplyr::bind_rows(energy_prices_extra_mineral_map(include_fertilizer_inputs = include_fertilizer_inputs)) %>%
    dplyr::distinct()

  volatility_by_indicator <- imf_monthly %>%
    dplyr::group_by(INDICATOR, clean) %>%
    dplyr::group_modify(~ dplyr::bind_rows(lapply(years_back, function(window_years) {
      energy_prices_calc_vol(.x, window_years, min_months = min_months)
    }))) %>%
    dplyr::left_join(
      imf_monthly_latest %>%
        dplyr::group_by(INDICATOR, clean) %>%
        dplyr::group_modify(~ energy_prices_latest_and_yoy(.x)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(unit = energy_prices_extract_unit(INDICATOR)),
      by = c("INDICATOR", "clean")
    ) %>%
    dplyr::left_join(annual_yoy_lookup, by = c("INDICATOR", "clean")) %>%
    dplyr::mutate(yoy_price_change_pct = dplyr::coalesce(yoy_price_change_pct_annual, yoy_price_change_pct)) %>%
    dplyr::select(-yoy_price_change_pct_annual) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(clean_key = energy_prices_normalize_mineral(clean)) %>%
    dplyr::left_join(mineral_map, by = "clean_key") %>%
    dplyr::mutate(
      tech = dplyr::case_when(
        clean %in% c("Oil_APSP", "Oil_Brent", "Oil_WTI", "Dubai_Crude") ~ "Oil",
        clean %in% c("Natural_Gas_Index", "Natural_Gas_EU", "Natural_Gas_Henry_Hub", "LNG", "Propane") ~ "Gas",
        clean == "Coal" ~ "Coal",
        clean == "Uranium" ~ "Nuclear",
        !is.na(tech) ~ tech,
        TRUE ~ NA_character_
      ),
      sub_sector = dplyr::case_when(
        clean %in% c("Oil_APSP", "Oil_Brent", "Oil_WTI", "Dubai_Crude") ~ clean,
        clean %in% c("Natural_Gas_Index", "Natural_Gas_EU", "Natural_Gas_Henry_Hub", "LNG", "Propane") ~ clean,
        clean == "Coal" ~ "Coal",
        !is.na(tech) ~ clean,
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(-clean_key)

  volatility_by_indicator %>%
    dplyr::filter(!is.na(tech), tech %in% tech_groups) %>%
    dplyr::group_by(tech, sub_sector) %>%
    dplyr::summarize(
      vol_logret_annualized = dplyr::if_else(all(is.na(vol_logret_annualized)), NA_real_, mean(vol_logret_annualized, na.rm = TRUE)),
      vol_level_sd = dplyr::if_else(all(is.na(vol_level_sd)), NA_real_, mean(vol_level_sd, na.rm = TRUE)),
      vol_level_cv = dplyr::if_else(all(is.na(vol_level_cv)), NA_real_, mean(vol_level_cv, na.rm = TRUE)),
      latest_price = dplyr::if_else(all(is.na(latest_price)), NA_real_, mean(latest_price, na.rm = TRUE)),
      yoy_price_change_pct = dplyr::if_else(all(is.na(yoy_price_change_pct)), NA_real_, mean(yoy_price_change_pct, na.rm = TRUE)),
      unit = paste(unique(unit[!is.na(unit)]), collapse = "; "),
      n_series = sum(!is.na(vol_logret_annualized)),
      .groups = "drop"
    )
}

energy_prices_build_table <- function(volatility_by_tech,
                                      as_of_year,
                                      country_info = NULL,
                                      gamma = 0.5) {
  base_tbl <- if (!is.null(country_info) && "country" %in% names(country_info)) {
    countries <- country_info %>%
      dplyr::distinct(country) %>%
      dplyr::rename(Country = country)
    tidyr::crossing(countries, volatility_by_tech)
  } else {
    volatility_by_tech %>% dplyr::mutate(Country = "Global")
  }

  base_tbl %>%
    dplyr::mutate(
      price_volatility = suppressWarnings(as.numeric(vol_logret_annualized)),
      price_volatility_index = median_scurve(-price_volatility, gamma = gamma),
      latest_price = suppressWarnings(as.numeric(latest_price)),
      yoy_price_change_pct = suppressWarnings(as.numeric(yoy_price_change_pct))
    ) %>%
    tidyr::pivot_longer(
      cols = c(price_volatility, price_volatility_index, latest_price, yoy_price_change_pct),
      names_to = "variable",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      supply_chain = "Upstream",
      category = "Energy Prices",
      data_type = dplyr::if_else(stringr::str_detect(variable, "_index$"), "index", "raw"),
      variable = stringr::str_remove(variable, "_index$"),
      Year = as_of_year,
      source = "IMF Commodity Prices",
      explanation = dplyr::case_when(
        variable == "price_volatility" & data_type == "raw" ~ "Annualized volatility of monthly log returns.",
        variable == "price_volatility" & data_type == "index" ~ "Percent-rank of lower price volatility.",
        variable == "latest_price" ~ paste0("Latest observed commodity price level for ", sub_sector, " (average across mapped IMF series). Unit: ", unit, "."),
        variable == "yoy_price_change_pct" ~ paste0("Year-on-year percentage change in 12-month average commodity prices for ", sub_sector, " (average across mapped IMF series). Unit: ", unit, "."),
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(
      Country,
      tech,
      sub_sector,
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

energy_prices_add_overall_fallback <- function(tbl) {
  if (is.null(tbl) || nrow(tbl) == 0) {
    return(tbl)
  }

  has_overall <- any(
    tbl$variable == "Overall Energy Prices Index" & tbl$data_type == "index",
    na.rm = TRUE
  )
  if (has_overall) {
    return(tbl)
  }

  fallback <- tbl %>%
    dplyr::filter(variable == "price_volatility", data_type == "index") %>%
    dplyr::mutate(
      variable = "Overall Energy Prices Index",
      source = "Author calculation",
      explanation = "Author calculation across category indices"
    )

  dplyr::bind_rows(tbl, fallback)
}

energy_prices <- function(imf_price,
                          mineral_demand_clean,
                          country_info = NULL,
                          years_back = c(5, 10, 20),
                          min_months = 24,
                          gamma = 0.5,
                          include_optional_indices = FALSE,
                          include_fertilizer_inputs = FALSE,
                          verbose = FALSE,
                          ...) {
  imf_monthly_long <- if (all(c("date", "value") %in% names(imf_price))) {
    energy_prices_long_from_pcps(imf_price)
  } else {
    energy_prices_imf_monthly_long(imf_price)
  }
  imf_monthly_latest_long <- if (all(c("date", "value") %in% names(imf_price))) {
    imf_monthly_long
  } else {
    energy_prices_imf_monthly_usd_long(imf_price)
  }

  imf_monthly <- energy_prices_imf_clean(
    imf_monthly_long = imf_monthly_long,
    include_optional_indices = include_optional_indices
  )
  imf_monthly_latest <- energy_prices_imf_clean(
    imf_monthly_long = imf_monthly_latest_long,
    include_optional_indices = include_optional_indices
  )

  if (isTRUE(verbose)) {
    report <- energy_prices_pcps_match_report(
      imf_monthly_long = imf_monthly_long,
      include_optional_indices = include_optional_indices
    )

    match_count <- sum(report$matched, na.rm = TRUE)
    total_count <- nrow(report)
    match_pct <- if (total_count > 0) 100 * match_count / total_count else 0
    message(sprintf("Energy Prices PCPS match coverage: %d/%d (%.1f%%)", match_count, total_count, match_pct))

    keyword_re <- "oil|gas|lng|coal|uranium|lithium|nickel|cobalt|manganese|copper|aluminum|rare earth|silicon|zinc|iron ore|vanadium|phosphate|urea|propane"
    unmatched <- report %>%
      dplyr::filter(!matched, stringr::str_detect(stringr::str_to_lower(INDICATOR), keyword_re)) %>%
      dplyr::slice_head(n = 20) %>%
      dplyr::pull(INDICATOR)

    if (length(unmatched) > 0) {
      message("Top unmatched energy-related IMF indicators:")
      message(paste0(" - ", unmatched, collapse = "\n"))
    }
  }

  annual_yoy_lookup <- if (!all(c("date", "value") %in% names(imf_price))) {
    energy_prices_imf_annual_yoy_lookup(
      imf_price = imf_price,
      include_optional_indices = include_optional_indices
    )
  } else {
    NULL
  }

  volatility_by_tech <- energy_prices_build_volatility(
    imf_monthly = imf_monthly,
    mineral_demand_clean = mineral_demand_clean,
    years_back = years_back,
    min_months = min_months,
    include_fertilizer_inputs = include_fertilizer_inputs,
    annual_yoy_lookup = annual_yoy_lookup,
    imf_monthly_latest = imf_monthly_latest
  )

  as_of_year <- lubridate::year(max(imf_monthly$date, na.rm = TRUE))

  energy_prices_build_table(
    volatility_by_tech = volatility_by_tech,
    as_of_year = as_of_year,
    country_info = country_info,
    gamma = gamma
  ) %>%
    energy_prices_add_overall_fallback() %>%
    energy_security_add_overall_index(include_sub_sector = TRUE)
}
