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
  Lithium = "lithium",
  LNG = "lng|liquefied natural gas",
  Manganese = "manganese",
  Natural_Gas_Index = "natural gas index|commodity price index.*natural gas",
  Natural_Gas_EU = "natural gas.*eu",
  Natural_Gas_Henry_Hub = "henry hub|us henry hub",
  Nickel = "nickel",
  Rare_Earths = "rare earth",
  Silicon = "silicon",
  Uranium = "uranium",
  Zinc = "zinc"
)

energy_prices_normalize_mineral <- function(x) {
  stringr::str_to_lower(stringr::str_replace_all(x, "_", " "))
}

energy_prices_imf_monthly_long <- function(imf_price) {
  monthly_re <- "^X\\d{4}\\.M\\d{2}$"

  imf_price %>%
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

energy_prices_imf_clean <- function(imf_monthly_long, patterns = energy_prices_imf_patterns) {
  imf_monthly_long %>%
    dplyr::mutate(ind_lc = stringr::str_to_lower(INDICATOR)) %>%
    dplyr::mutate(
      clean = dplyr::case_when(
        stringr::str_detect(ind_lc, stringr::regex(patterns$Aluminum, ignore_case = TRUE)) ~ "Aluminum",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Oil_APSP, ignore_case = TRUE)) ~ "Oil_APSP",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Oil_Brent, ignore_case = TRUE)) ~ "Oil_Brent",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Oil_WTI, ignore_case = TRUE)) ~ "Oil_WTI",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Chromium, ignore_case = TRUE)) ~ "Chromium",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Coal, ignore_case = TRUE)) ~ "Coal",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Cobalt, ignore_case = TRUE)) ~ "Cobalt",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Copper, ignore_case = TRUE)) ~ "Copper",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Lithium, ignore_case = TRUE)) ~ "Lithium",
        stringr::str_detect(ind_lc, stringr::regex(patterns$LNG, ignore_case = TRUE)) ~ "LNG",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Manganese, ignore_case = TRUE)) ~ "Manganese",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Natural_Gas_Index, ignore_case = TRUE)) ~ "Natural_Gas_Index",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Natural_Gas_EU, ignore_case = TRUE)) ~ "Natural_Gas_EU",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Natural_Gas_Henry_Hub, ignore_case = TRUE)) ~ "Natural_Gas_Henry_Hub",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Nickel, ignore_case = TRUE)) ~ "Nickel",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Rare_Earths, ignore_case = TRUE)) ~ "Rare_Earths",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Silicon, ignore_case = TRUE)) ~ "Silicon",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Uranium, ignore_case = TRUE)) ~ "Uranium",
        stringr::str_detect(ind_lc, stringr::regex(patterns$Zinc, ignore_case = TRUE)) ~ "Zinc",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(-ind_lc) %>%
    dplyr::filter(!is.na(value), !is.na(clean))
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

energy_prices_build_volatility <- function(imf_monthly, mineral_demand_clean, years_back = c(5, 10, 20), min_months = 24) {
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
    dplyr::distinct()

  volatility_by_indicator <- imf_monthly %>%
    dplyr::group_by(INDICATOR, clean) %>%
    dplyr::group_modify(~ dplyr::bind_rows(lapply(years_back, function(window_years) {
      energy_prices_calc_vol(.x, window_years, min_months = min_months)
    }))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(clean_key = energy_prices_normalize_mineral(clean)) %>%
    dplyr::left_join(mineral_map, by = "clean_key") %>%
    dplyr::mutate(
      tech = dplyr::case_when(
        clean %in% c("Oil_APSP", "Oil_Brent", "Oil_WTI") ~ "Oil",
        clean %in% c("Natural_Gas_Index", "Natural_Gas_EU", "Natural_Gas_Henry_Hub", "LNG") ~ "Gas",
        clean == "Coal" ~ "Coal",
        !is.na(tech) ~ tech,
        TRUE ~ clean
      )
    ) %>%
    dplyr::select(-clean_key)

  volatility_by_indicator %>%
    dplyr::filter(!is.na(tech)) %>%
    dplyr::group_by(tech) %>%
    dplyr::summarize(
      vol_logret_annualized = mean(vol_logret_annualized, na.rm = TRUE),
      vol_level_sd = mean(vol_level_sd, na.rm = TRUE),
      vol_level_cv = mean(vol_level_cv, na.rm = TRUE),
      n_series = sum(!is.na(vol_logret_annualized)),
      .groups = "drop"
    )
}

energy_prices_build_table <- function(volatility_by_tech, as_of_year, gamma = 0.5) {
  volatility_by_tech %>%
    dplyr::mutate(
      price_volatility = vol_logret_annualized,
      price_volatility_index = median_scurve(-price_volatility, gamma = gamma)
    ) %>%
    tidyr::pivot_longer(
      cols = c(price_volatility, price_volatility_index),
      names_to = "variable",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      Country = "Global",
      supply_chain = "Upstream",
      category = "Energy Prices",
      data_type = dplyr::if_else(stringr::str_detect(variable, "_index$"), "index", "raw"),
      variable = stringr::str_remove(variable, "_index$"),
      Year = as_of_year,
      source = "IMF Commodity Prices",
      explanation = dplyr::case_when(
        data_type == "raw" ~ "Annualized volatility of monthly log returns.",
        data_type == "index" ~ "Percent-rank of lower price volatility."
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

energy_prices <- function(imf_price,
                          mineral_demand_clean,
                          years_back = c(5, 10, 20),
                          min_months = 24,
                          gamma = 0.5) {
  imf_monthly_long <- energy_prices_imf_monthly_long(imf_price)
  imf_monthly <- energy_prices_imf_clean(imf_monthly_long)

  volatility_by_tech <- energy_prices_build_volatility(
    imf_monthly = imf_monthly,
    mineral_demand_clean = mineral_demand_clean,
    years_back = years_back,
    min_months = min_months
  )

  as_of_year <- lubridate::year(max(imf_monthly$date, na.rm = TRUE))

  energy_prices_build_table(
    volatility_by_tech = volatility_by_tech,
    as_of_year = as_of_year,
    gamma = gamma
  ) %>%
    energy_security_add_overall_index()
}
