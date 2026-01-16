# Cost competitiveness theme builder functions.

cost_competitiveness_normalize_iso_ilo <- function(x) {
  cleaned <- stringr::str_squish(x)
  custom <- c(
    "United States of America" = "USA",
    "United Kingdom of Great Britain and Northern Ireland" = "GBR",
    "Republic of Korea" = "KOR",
    "Russian Federation" = "RUS",
    "Hong Kong, China" = "HKG",
    "Macao, China" = "MAC",
    "T\u00fcrkiye" = "TUR",
    "C\u00f4te d'Ivoire" = "CIV",
    "Cura\u00e7ao" = "CUW",
    "Lao People's Democratic Republic" = "LAO",
    "Tanzania, United Republic of" = "TZA",
    "Congo, Democratic Republic of the" = "COD",
    "Congo" = "COG",
    "Eswatini" = "SWZ",
    "Cabo Verde" = "CPV",
    "Viet Nam" = "VNM",
    "Kosovo" = "XKX",
    "United States Virgin Islands" = "VIR"
  )

  iso <- countrycode::countrycode(
    cleaned,
    "country.name",
    "iso3c",
    custom_match = custom,
    warn = TRUE
  )

  iso <- dplyr::if_else(
    is.na(iso),
    countrycode::countrycode(
      stringr::str_replace(cleaned, ",\\s*China$", ""),
      "country.name",
      "iso3c",
      custom_match = custom,
      warn = TRUE
    ),
    iso
  )

  iso
}

cost_competitiveness_labor_weights <- function() {
  tibble::tribble(
    ~Technology, ~supply_chain, ~labor_share,
    "Solar", "Upstream", 0.15,
    "Solar", "Midstream", 0.15,
    "Solar", "Downstream", 0.07,
    "Wind", "Upstream", 0.15,
    "Wind", "Midstream", 0.12,
    "Wind", "Downstream", 0.15,
    "Geothermal", "Upstream", 0.15,
    "Geothermal", "Midstream", 0.12,
    "Geothermal", "Downstream", 0.22,
    "Nuclear", "Upstream", 0.18,
    "Nuclear", "Midstream", 0.15,
    "Nuclear", "Downstream", 0.18,
    "Gas", "Upstream", 0.18,
    "Gas", "Midstream", 0.10,
    "Gas", "Downstream", 0.07,
    "Coal", "Upstream", 0.40,
    "Coal", "Midstream", 0.15,
    "Coal", "Downstream", 0.15,
    "Oil", "Upstream", 0.18,
    "Oil", "Midstream", 0.10,
    "Oil", "Downstream", 0.15,
    "Hydrogen", "Upstream", 0.06,
    "Hydrogen", "Midstream", 0.10,
    "Hydrogen", "Downstream", 0.08,
    "Electric Grid", "Upstream", 0.20,
    "Electric Grid", "Midstream", 0.25,
    "Electric Grid", "Downstream", 0.38,
    "Electric Vehicles", "Upstream", 0.20,
    "Electric Vehicles", "Midstream", 0.20,
    "Electric Vehicles", "Downstream", 0.10,
    "Batteries", "Upstream", 0.22,
    "Batteries", "Midstream", 0.08,
    "Batteries", "Downstream", 0.08
  )
}

cost_competitiveness_capital_weights <- function() {
  tibble::tribble(
    ~Technology, ~supply_chain, ~cap_share,
    "Solar", "Upstream", 0.20,
    "Solar", "Midstream", 0.80,
    "Solar", "Downstream", 0.90,
    "Wind", "Upstream", 0.25,
    "Wind", "Midstream", 0.80,
    "Wind", "Downstream", 0.85,
    "Geothermal", "Upstream", 0.68,
    "Geothermal", "Midstream", 0.80,
    "Geothermal", "Downstream", 0.70,
    "Nuclear", "Upstream", 0.50,
    "Nuclear", "Midstream", 0.80,
    "Nuclear", "Downstream", 0.70,
    "Gas", "Upstream", 0.50,
    "Gas", "Midstream", 0.60,
    "Gas", "Downstream", 0.20,
    "Coal", "Upstream", 0.35,
    "Coal", "Midstream", 0.68,
    "Coal", "Downstream", 0.30,
    "Oil", "Upstream", 0.55,
    "Oil", "Midstream", 0.60,
    "Oil", "Downstream", 0.25,
    "Hydrogen", "Upstream", 0.30,
    "Hydrogen", "Midstream", 0.55,
    "Hydrogen", "Downstream", 0.18,
    "Electric Grid", "Upstream", 0.28,
    "Electric Grid", "Midstream", 0.68,
    "Electric Grid", "Downstream", 0.53,
    "Electric Vehicles", "Upstream", 0.45,
    "Electric Vehicles", "Midstream", 0.20,
    "Electric Vehicles", "Downstream", 0.20,
    "Batteries", "Upstream", 0.45,
    "Batteries", "Midstream", 0.15,
    "Batteries", "Downstream", 0.30
  )
}

cost_competitiveness_clean_iea <- function(iea_cost_raw) {
  require_columns(
    iea_cost_raw,
    c("Product", "Region", "Value"),
    label = "iea_cost_raw"
  )

  iea_cost_raw %>%
    dplyr::mutate(
      Product = dplyr::recode(
        Product,
        "Solar PV" = "Solar",
        "Wind turbines" = "Wind",
        "Electrolysers" = "Green Hydrogen"
      ),
      Region = dplyr::if_else(Region == "Korea", "South Korea", Region),
      supply_chain = "Midstream",
      country2 = dplyr::case_when(
        Region == "European Union" ~ "EU",
        Region == "Other Southeast Asia" ~ "Asia Pacific",
        TRUE ~ Region
      ),
      tech = Product
    ) %>%
    dplyr::select(Region, country2, tech, supply_chain, Value)
}

cost_competitiveness_build_iea_indices <- function(iea_cost_clean, gamma = 0.5) {
  require_columns(
    iea_cost_clean,
    c("Region", "country2", "tech", "supply_chain", "Value"),
    label = "iea_cost_clean"
  )

  iea_cost_clean %>%
    dplyr::group_by(tech) %>%
    dplyr::mutate(cost_index = 1 - median_scurve(Value, gamma = gamma)) %>%
    dplyr::ungroup()
}

cost_competitiveness_build_ilo_indices <- function(ilo_raw) {
  require_columns(
    ilo_raw,
    c(
      "ref_area.label",
      "classif1.label",
      "sex.label",
      "classif2.label",
      "time",
      "obs_value"
    ),
    label = "ilo_raw"
  )

  ilo_raw %>%
    dplyr::filter(
      classif1.label %in% c(
        "Economic activity (Aggregate): Total",
        "Economic activity (Aggregate): Manufacturing",
        "Economic activity (Aggregate): Agriculture",
        "Economic activity (Aggregate): Construction"
      ),
      sex.label == "Total",
      classif2.label == "Currency: 2021 PPP $"
    ) %>%
    dplyr::mutate(country_std = cost_competitiveness_normalize_iso_ilo(ref_area.label)) %>%
    dplyr::slice_max(time, n = 1, with_ties = FALSE, by = c(ref_area.label, classif1.label)) %>%
    dplyr::arrange(dplyr::desc(obs_value)) %>%
    dplyr::filter(ref_area.label != "Zimbabwe") %>%
    dplyr::group_by(classif1.label) %>%
    dplyr::mutate(labor_index = median_scurve(obs_value)) %>%
    dplyr::ungroup()
}

cost_competitiveness_build_labor_scaffold <- function(ilo_indices) {
  require_columns(
    ilo_indices,
    c("country_std", "classif1.label", "labor_index"),
    label = "ilo_indices"
  )

  ilo_indices %>%
    dplyr::mutate(
      supply_chain = dplyr::case_when(
        stringr::str_detect(classif1.label, "Agriculture") ~ "Upstream",
        stringr::str_detect(classif1.label, "Manufacturing") ~ "Midstream",
        stringr::str_detect(classif1.label, "Construction") ~ "Downstream",
        stringr::str_detect(classif1.label, "Total") ~ "Total",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(supply_chain != "Total") %>%
    dplyr::select(country_std, supply_chain, labor_index)
}

cost_competitiveness_build_labor_table <- function(ilo_sc,
                                                   labor_weights,
                                                   country_info,
                                                   year = 2024L) {
  require_columns(
    ilo_sc,
    c("country_std", "supply_chain", "labor_index"),
    label = "ilo_sc"
  )
  require_columns(labor_weights, c("Technology", "supply_chain", "labor_share"), label = "labor_weights")
  require_columns(country_info, c("iso3c", "country"), label = "country_info")

  country_map <- country_info %>%
    dplyr::select(iso3c, country) %>%
    dplyr::distinct()
  assert_unique_keys(country_map, "iso3c", label = "cost_competitiveness_country_info")

  ilo_sc %>%
    dplyr::inner_join(labor_weights, by = "supply_chain") %>%
    dplyr::mutate(
      labor_index = 1 - labor_index,
      labor_index_weighted = labor_index * labor_share
    ) %>%
    dplyr::select(country_std, Technology, supply_chain, labor_share, labor_index, labor_index_weighted) %>%
    dplyr::left_join(country_map, by = c("country_std" = "iso3c"), relationship = "many-to-one") %>%
    tidyr::pivot_longer(
      cols = labor_share:labor_index_weighted,
      names_to = "variable",
      values_to = "value"
    ) %>%
    dplyr::transmute(
      Country = country,
      tech = Technology,
      supply_chain,
      category = "Cost Competitiveness",
      variable,
      data_type = dplyr::case_when(
        variable == "labor_index" ~ "index",                 # median_scurve output
        variable == "labor_share" ~ "weight",
        variable == "labor_index_weighted" ~ "contribution",
        TRUE ~ "raw"
      ),
      value,
      Year = as.integer(year),
      source = "International Labor Organization",
      explanation = dplyr::case_when(
        variable == "labor_share" ~ "Estimated Labor Share of Costs",
        variable == "labor_index" ~ "Estimated weekly earnings by economic activity, indexed (median_scurve)",
        variable == "labor_index_weighted" ~ "Labor index x labor share (contribution, not an index)",
        TRUE ~ variable
      )
    )
}

cost_competitiveness_clean_imf_rates <- function(imf_lending_rates) {
  require_columns(
    imf_lending_rates,
    c("COUNTRY", "INDICATOR", "FREQUENCY"),
    label = "imf_lending_rates"
  )

  imf_lending_rates %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("X"),
      names_to = "period",
      values_to = "value"
    ) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(
      period = stringr::str_remove(period, "^X"),
      freq_tag = dplyr::case_when(
        stringr::str_detect(period, "\\.M\\d{2}$") ~ "M",
        stringr::str_detect(period, "\\.Q\\d$") ~ "Q",
        TRUE ~ "A"
      ),
      year = as.integer(stringr::str_sub(period, 1, 4)),
      month = dplyr::case_when(
        freq_tag == "M" ~ as.integer(stringr::str_sub(period, -2, -1)),
        freq_tag == "Q" ~ as.integer(stringr::str_sub(period, -1, -1)) * 3L,
        TRUE ~ 12L
      ),
      period_date = as.Date(sprintf("%04d-%02d-01", year, month))
    )
}

cost_competitiveness_select_imf_rates <- function(imf_long) {
  require_columns(
    imf_long,
    c("COUNTRY", "INDICATOR", "period_date", "value"),
    label = "imf_long"
  )

  imf_long %>%
    dplyr::mutate(
      priority = dplyr::case_when(
        INDICATOR == "Lending Rate, Percent per annum" ~ 1L,
        stringr::str_detect(
          INDICATOR,
          "Harmonized Euro.*Loans.*New Business.*Non-financial corporations"
        ) ~ 2L,
        stringr::str_detect(
          INDICATOR,
          "Harmonized Euro.*Loans.*Outstanding.*Non-financial corporations"
        ) ~ 3L,
        INDICATOR == "Money market Rate, Percent per annum" ~ 4L,
        stringr::str_detect(INDICATOR, "^Monetary policy-related, Rate") ~ 5L,
        INDICATOR == "Deposit Rate, Percent per annum" ~ 6L,
        TRUE ~ 99L
      )
    ) %>%
    dplyr::filter(priority < 99L)
}

cost_competitiveness_build_rate_index <- function(imf_scored) {
  require_columns(
    imf_scored,
    c("COUNTRY", "INDICATOR", "period_date", "value", "priority"),
    label = "imf_scored"
  )

  best_rate <- imf_scored %>%
    dplyr::arrange(COUNTRY, priority, dplyr::desc(period_date)) %>%
    dplyr::mutate(COUNTRY = dplyr::if_else(stringr::str_detect(COUNTRY, "Macao"), "China", COUNTRY)) %>%
    dplyr::group_by(COUNTRY) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      COUNTRY,
      chosen_indicator = INDICATOR,
      period_date,
      nominal_rate = value
    )

  best_rate <- best_rate %>%
    dplyr::mutate(
      country_std = countrycode::countrycode(
        COUNTRY,
        "country.name",
        "iso3c",
        custom_match = c("Poland, Republic of" = "POL")
      )
    )

  if (any(is.na(best_rate$country_std))) {
    unmatched <- unique(best_rate$COUNTRY[is.na(best_rate$country_std)])
    warning(
      "Unmatched IMF lending rate countries: ",
      paste(unmatched, collapse = ", ")
    )
  }

  best_rate <- best_rate %>% dplyr::filter(!is.na(country_std))

  q <- stats::quantile(best_rate$nominal_rate, c(0.05, 0.95), na.rm = TRUE)

  best_rate %>%
    dplyr::mutate(
      rate_clip = pmin(pmax(nominal_rate, q[1]), q[2]),
      capital_cost_index = (rate_clip - min(rate_clip, na.rm = TRUE)) /
        (max(rate_clip, na.rm = TRUE) - min(rate_clip, na.rm = TRUE))
    ) %>%
    dplyr::select(country_std, chosen_indicator, period_date, nominal_rate, capital_cost_index)
}

cost_competitiveness_build_ppi <- function(imf_ppi) {
  require_columns(
    imf_ppi,
    c("COUNTRY", "INDICATOR", "TYPE_OF_TRANSFORMATION"),
    label = "imf_ppi"
  )

  ppi_long <- imf_ppi %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("X"),
      names_to = "period",
      values_to = "value"
    ) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(
      period = stringr::str_remove(period, "^X"),
      freq_tag = dplyr::case_when(
        stringr::str_detect(period, "\\.M\\d{2}$") ~ "M",
        stringr::str_detect(period, "\\.Q\\d$") ~ "Q",
        TRUE ~ "A"
      ),
      year = as.integer(stringr::str_sub(period, 1, 4)),
      month = dplyr::case_when(
        freq_tag == "M" ~ as.integer(stringr::str_sub(period, -2, -1)),
        freq_tag == "Q" ~ as.integer(stringr::str_sub(period, -1, -1)) * 3L,
        TRUE ~ 12L
      ),
      period_date = as.Date(sprintf("%04d-%02d-01", year, month))
    ) %>%
    dplyr::filter(
      INDICATOR == "Producer price index (PPI)",
      TYPE_OF_TRANSFORMATION == "Index"
    ) %>%
    dplyr::arrange(COUNTRY, dplyr::desc(period_date)) %>%
    dplyr::group_by(COUNTRY) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(ppi_index = median_scurve(value)) %>%
    dplyr::transmute(
      COUNTRY,
      INDICATOR,
      period_date,
      ppi = value,
      ppi_index
    )

  ppi_long %>%
    dplyr::mutate(
      country_std = countrycode::countrycode(
        COUNTRY,
        "country.name",
        "iso3c",
        custom_match = c(
          "Azerbaijan, Republic of" = "AZE",
          "Belarus, Republic of" = "BLR"
        )
      )
    ) %>%
    dplyr::filter(!is.na(country_std), !is.na(ppi), !is.na(period_date)) %>%
    dplyr::mutate(year = as.integer(format(period_date, "%Y")))
}

cost_competitiveness_build_capital_base <- function(rate_index,
                                                    ppi_clean,
                                                    country_info,
                                                    alpha = 0.60) {
  require_columns(
    rate_index,
    c("country_std", "capital_cost_index", "nominal_rate"),
    label = "rate_index"
  )
  require_columns(ppi_clean, c("country_std", "ppi_index", "ppi"), label = "ppi_clean")
  require_columns(country_info, c("iso3c", "region", "income"), label = "country_info")

  country_ref <- country_info %>%
    dplyr::select(iso3c, region, income) %>%
    dplyr::distinct()
  assert_unique_keys(country_ref, "iso3c", label = "cost_competitiveness_country_ref")

  rate_index %>%
    dplyr::left_join(ppi_clean, by = "country_std", relationship = "one-to-one") %>%
    dplyr::left_join(country_ref, by = c("country_std" = "iso3c"), relationship = "many-to-one") %>%
    dplyr::group_by(region, income) %>%
    dplyr::mutate(ppi_index = dplyr::if_else(is.na(ppi_index), mean(ppi_index, na.rm = TRUE), ppi_index)) %>%
    dplyr::mutate(
      income = dplyr::if_else(income == "Low income", "Lower middle income", income)
    ) %>%
    dplyr::group_by(income) %>%
    dplyr::mutate(ppi_index = dplyr::if_else(is.na(ppi_index), mean(ppi_index, na.rm = TRUE), ppi_index)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      cap_cost_index = alpha * capital_cost_index +
        (1 - alpha) * dplyr::coalesce(ppi_index, capital_cost_index)
    ) %>%
    dplyr::select(
      country_std,
      region,
      income,
      cap_cost_index,
      nominal_rate,
      rate_index = capital_cost_index,
      ppi_index,
      ppi
    )
}

cost_competitiveness_build_capital_table <- function(cap_cost_base,
                                                     cap_weights,
                                                     supply_chain_scaffold,
                                                     country_info,
                                                     year = 2024L) {
  require_columns(
    cap_cost_base,
    c("country_std", "cap_cost_index", "nominal_rate", "rate_index", "ppi_index", "ppi"),
    label = "cap_cost_base"
  )
  require_columns(cap_weights, c("Technology", "supply_chain", "cap_share"), label = "cap_weights")
  require_columns(supply_chain_scaffold, c("country_std", "supply_chain"), label = "supply_chain_scaffold")
  require_columns(country_info, c("iso3c", "country"), label = "country_info")

  country_map <- country_info %>%
    dplyr::select(iso3c, country) %>%
    dplyr::distinct()
  assert_unique_keys(country_map, "iso3c", label = "cost_competitiveness_country_map")

  cap_weights <- cap_weights %>% dplyr::distinct(Technology, supply_chain, cap_share)
  assert_unique_keys(cap_weights, c("Technology", "supply_chain"), label = "cap_weights")

  supply_chain_scaffold %>%
    dplyr::distinct(country_std, supply_chain) %>%
    tidyr::crossing(cap_weights %>% dplyr::distinct(Technology)) %>%
    dplyr::left_join(cap_cost_base, by = "country_std", relationship = "many-to-one") %>%
    dplyr::left_join(
      cap_weights,
      by = c("Technology", "supply_chain"),
      relationship = "many-to-one"
    ) %>%
    dplyr::mutate(
      cap_cost_index = 1 - cap_cost_index,
      cap_index_weighted = cap_cost_index * cap_share
    ) %>%
    dplyr::select(
      country_std,
      Technology,
      supply_chain,
      cap_share,
      cap_cost_index,
      cap_index_weighted,
      nominal_rate,
      rate_index,
      ppi_index,
      ppi
    ) %>%
    dplyr::left_join(country_map, by = c("country_std" = "iso3c"), relationship = "many-to-one") %>%
    tidyr::pivot_longer(
      cols = cap_share:ppi,
      names_to = "variable",
      values_to = "value"
    ) %>%
    dplyr::transmute(
      Country = country,
      tech = Technology,
      supply_chain,
      category = "Cost Competitiveness",
      variable,
      data_type = dplyr::case_when(
        variable %in% c("ppi_index", "rate_index", "cap_cost_index") ~ "index",
        variable %in% c("cap_share") ~ "weight",
        stringr::str_ends(variable, "_weighted") ~ "contribution",
        TRUE ~ "raw"
      ),
      value,
      Year = as.integer(year),
      source = "International Monetary Fund",
      explanation = dplyr::case_when(
        variable == "cap_share" ~ "Estimated Capital Share of Costs",
        variable == "ppi_index" ~ "Producer Price Index, indexed (median_scurve)",
        variable == "cap_index_weighted" ~ "Capital cost score * capital share (contribution, not an index)",
        variable == "nominal_rate" ~ "Lending rate, %",
        variable == "rate_index" ~ "Lending rate, index (NOT median_scurve as currently implemented)",
        variable == "ppi" ~ "Producer Price Index, 2010=100",
        TRUE ~ variable
      )
    )
}

cost_competitiveness_build_input_cost_index <- function(labor_weights,
                                                        cap_weights,
                                                        labor_scaffold,
                                                        cap_cost_base,
                                                        country_info,
                                                        year = 2024L) {
  require_columns(labor_weights, c("Technology", "supply_chain", "labor_share"), label = "labor_weights")
  require_columns(cap_weights, c("Technology", "supply_chain", "cap_share"), label = "cap_weights")
  require_columns(labor_scaffold, c("country_std", "supply_chain", "labor_index"), label = "labor_scaffold")
  require_columns(cap_cost_base, c("country_std", "cap_cost_index"), label = "cap_cost_base")
  require_columns(country_info, c("iso3c", "country"), label = "country_info")

  shares_norm <- labor_weights %>%
    dplyr::inner_join(cap_weights, by = c("Technology", "supply_chain")) %>%
    dplyr::mutate(
      lk_sum = labor_share + cap_share,
      wL = dplyr::if_else(lk_sum > 0, labor_share / lk_sum, NA_real_),
      wK = dplyr::if_else(lk_sum > 0, cap_share / lk_sum, NA_real_)
    )

  labor_comp_cc <- labor_scaffold %>%
    dplyr::mutate(labor_comp = 1 - labor_index) %>%
    dplyr::select(country_std, supply_chain, labor_comp)

  cap_comp_c <- cap_cost_base %>%
    dplyr::mutate(cap_comp = 1 - cap_cost_index) %>%
    dplyr::select(country_std, cap_comp)

  scaffold <- labor_comp_cc %>%
    dplyr::select(country_std, supply_chain) %>%
    dplyr::distinct()

  input_cost_index <- scaffold %>%
    dplyr::inner_join(
      shares_norm %>% dplyr::select(Technology, supply_chain, labor_share, cap_share, wL, wK),
      by = "supply_chain",
      relationship = "many-to-many"
    ) %>%
    dplyr::left_join(labor_comp_cc, by = c("country_std", "supply_chain"), relationship = "many-to-many") %>%
    dplyr::left_join(cap_comp_c, by = "country_std", relationship = "many-to-one") %>%
    dplyr::mutate(input_cost_index = wL * labor_comp + wK * cap_comp) %>%
    dplyr::left_join(
      country_info %>% dplyr::select(iso3c, country),
      by = c("country_std" = "iso3c"),
      relationship = "many-to-one"
    ) %>%
    dplyr::relocate(country, country_std, Technology, supply_chain)

  input_cost_index %>%
    dplyr::transmute(
      Country = country,
      country_std,
      tech = Technology,
      supply_chain,
      category = "Cost Competitiveness",
      variable = "Input Cost Index",
      data_type = "index",
      value = input_cost_index,
      Year = as.integer(year),
      source = "ILO + IMF (rates & PPI)",
      explanation = "Composite input cost competitiveness index (wL*Labor + wK*Capital), 0-1; higher = more cost-competitive"
    )
}

cost_competitiveness_build_country_maps <- function(ei) {
  require_columns(ei, c("Country", "EU", "SubRegion"), label = "ei")

  base_ei <- ei %>%
    dplyr::mutate(Country = dplyr::if_else(Country == "US", "United States", Country))

  country_map <- base_ei %>%
    dplyr::distinct(Country, EU) %>%
    dplyr::mutate(country2 = dplyr::if_else(EU == 1L, "EU", Country))

  subregion_map <- base_ei %>%
    dplyr::filter(Country != "South Korea") %>%
    dplyr::distinct(Country, SubRegion)

  assert_unique_keys(country_map, c("Country", "EU"), label = "cost_competitiveness_country_map")
  assert_unique_keys(subregion_map, c("Country", "SubRegion"), label = "cost_competitiveness_subregion_map")

  list(
    country_map = country_map,
    subregion_map = subregion_map
  )
}

cost_competitiveness_build_iea_table <- function(cost_indices,
                                                 country_map,
                                                 subregion_map,
                                                 year = 2024L) {
  require_columns(
    cost_indices,
    c("Region", "country2", "tech", "supply_chain", "cost_index"),
    label = "cost_indices"
  )
  require_columns(country_map, c("Country", "EU", "country2"), label = "country_map")
  require_columns(subregion_map, c("Country", "SubRegion"), label = "subregion_map")

  assert_unique_keys(
    cost_indices %>% dplyr::distinct(country2, tech, supply_chain),
    c("country2", "tech", "supply_chain"),
    label = "cost_competitiveness_indices"
  )

  cost_indices %>%
    dplyr::left_join(country_map, by = "country2", relationship = "many-to-many") %>%
    dplyr::left_join(subregion_map, by = c("country2" = "SubRegion"), relationship = "many-to-many") %>%
    dplyr::mutate(
      Country = dplyr::if_else(is.na(Country.x), Country.y, Country.x),
      supply_chain = dplyr::if_else(tech == "Ammonia", "Downstream", supply_chain),
      tech = dplyr::if_else(tech == "Ammonia", "Green Hydrogen", tech)
    ) %>%
    dplyr::transmute(
      Country,
      tech,
      supply_chain,
      category = "Cost Competitiveness",
      variable = "IEA Cost index",
      data_type = "index",
      value = cost_index,
      Year = as.integer(year),
      source = "IEA Energy Technology Perspectives 2024",
      explanation = "Cost competitiveness relative to China"
    )
}

cost_competitiveness_validate_data_types <- function(tbl) {
  require_columns(tbl, c("variable", "data_type"), label = "cost_competitiveness_tbl")

  data_types <- unique(tbl$data_type)
  if (length(data_types) == 1 && data_types == "index") {
    stop("All cost competitiveness rows are labeled data_type == 'index'; this indicates a stamping bug.")
  }

  required_types <- c("raw", "weight", "contribution", "index")
  missing_types <- setdiff(required_types, data_types)
  if (length(missing_types) > 0) {
    stop(
      "Cost competitiveness output is missing expected data_type(s): ",
      paste(missing_types, collapse = ", ")
    )
  }

  must_not_index <- c("cap_share", "labor_share", "ppi", "nominal_rate")
  invalid_index <- tbl %>%
    dplyr::filter(variable %in% must_not_index, data_type == "index") %>%
    dplyr::distinct(variable)
  if (nrow(invalid_index) > 0) {
    stop(
      "Cost competitiveness data_type stamping detected; these variables must not be 'index': ",
      paste(invalid_index$variable, collapse = ", ")
    )
  }

  must_index <- c("ppi_index", "rate_index", "cap_cost_index", "labor_index", "Input Cost Index", "IEA Cost index")
  invalid_type <- tbl %>%
    dplyr::filter(variable %in% must_index, data_type != "index") %>%
    dplyr::distinct(variable, data_type)
  if (nrow(invalid_type) > 0) {
    stop(
      "Cost competitiveness index variables must be labeled 'index'; mismatches found for: ",
      paste(invalid_type$variable, collapse = ", ")
    )
  }

  weighted_mismatch <- tbl %>%
    dplyr::filter(stringr::str_ends(variable, "_weighted"), data_type != "contribution") %>%
    dplyr::distinct(variable, data_type)
  if (nrow(weighted_mismatch) > 0) {
    stop(
      "Weighted contribution variables must be labeled 'contribution'; mismatches found for: ",
      paste(weighted_mismatch$variable, collapse = ", ")
    )
  }

  invisible(tbl)
}

cost_competitiveness <- function(iea_cost_raw,
                                 ei,
                                 country_info,
                                 ilo_raw,
                                 imf_lending_rates,
                                 imf_ppi,
                                 year = 2024L,
                                 gamma = 0.5,
                                 alpha = 0.60) {
  iea_cost_clean <- cost_competitiveness_clean_iea(iea_cost_raw)
  cost_indices <- cost_competitiveness_build_iea_indices(iea_cost_clean, gamma = gamma)
  country_maps <- cost_competitiveness_build_country_maps(ei)
  iea_cost_tbl <- cost_competitiveness_build_iea_table(
    cost_indices,
    country_map = country_maps$country_map,
    subregion_map = country_maps$subregion_map,
    year = year
  )

  ilo_indices <- cost_competitiveness_build_ilo_indices(ilo_raw)
  ilo_scaffold <- cost_competitiveness_build_labor_scaffold(ilo_indices)
  labor_weights <- cost_competitiveness_labor_weights()
  labor_cost_tbl <- cost_competitiveness_build_labor_table(
    ilo_sc = ilo_scaffold,
    labor_weights = labor_weights,
    country_info = country_info,
    year = year
  )

  imf_long <- cost_competitiveness_clean_imf_rates(imf_lending_rates)
  imf_scored <- cost_competitiveness_select_imf_rates(imf_long)
  rate_index <- cost_competitiveness_build_rate_index(imf_scored)
  ppi_clean <- cost_competitiveness_build_ppi(imf_ppi)
  cap_cost_base <- cost_competitiveness_build_capital_base(
    rate_index = rate_index,
    ppi_clean = ppi_clean,
    country_info = country_info,
    alpha = alpha
  )
  cap_weights <- cost_competitiveness_capital_weights()
  capital_cost_tbl <- cost_competitiveness_build_capital_table(
    cap_cost_base = cap_cost_base,
    cap_weights = cap_weights,
    supply_chain_scaffold = ilo_scaffold,
    country_info = country_info,
    year = year
  )

  input_cost_tbl <- cost_competitiveness_build_input_cost_index(
    labor_weights = labor_weights,
    cap_weights = cap_weights,
    labor_scaffold = ilo_scaffold,
    cap_cost_base = cap_cost_base,
    country_info = country_info,
    year = year
  )

  output <- dplyr::bind_rows(
    lapply(
      list(iea_cost_tbl, labor_cost_tbl, capital_cost_tbl, input_cost_tbl),
      standardize_bind_rows_inputs
    )
  )

  output <- standardize_theme_table(output)
  cost_competitiveness_validate_data_types(output)
  validate_schema(output)
  output
}
