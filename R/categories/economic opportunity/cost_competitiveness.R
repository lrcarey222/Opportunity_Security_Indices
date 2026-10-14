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
    c("Product", "Region", "Year", "Value"),
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
      tech = Product,
      # The extract's own reference year, so the emitted rows are not stamped with the
      # build year. The staged file reports 2023 costs.
      iea_year = suppressWarnings(as.integer(Year))
    ) %>%
    dplyr::select(Region, country2, tech, supply_chain, iea_year, Value)
}

cost_competitiveness_build_iea_indices <- function(iea_cost_clean, gamma = 0.5) {
  require_columns(
    iea_cost_clean,
    c("Region", "country2", "tech", "supply_chain", "iea_year", "Value"),
    label = "iea_cost_clean"
  )

  iea_cost_clean %>%
    dplyr::group_by(tech) %>%
    dplyr::mutate(cost_index = 1 - median_scurve(Value, gamma = gamma)) %>%
    dplyr::ungroup()
}

# Observations older than this many years are held out of the theme's rankings and imputed
# from regional peers instead.
#
# Both the ILO and the IMF panels report very unevenly: before this floor existed the labour
# leg ranked a 1980 wage against 2025 wages, and the capital leg scored Guinea on a 2001
# lending rate and Sweden on a 2006 one. Ten years is deliberately generous - it targets
# observations that cannot stand in for the current period at all, and leaves merely-old but
# still plausible readings (Japan and Canada at 2017) in place, since a country's own stale
# rate is often closer than a regional mean. Tighten it here if that trade changes.
COST_COMPETITIVENESS_MAX_OBS_AGE <- 10L

# TRUE where an observation is too old to stand in for the current period. The reference is
# the newest observation in the panel rather than the system clock, so a vintage rebuild
# that slices its inputs to an earlier year applies the floor relative to that year instead
# of declaring the whole panel stale.
cost_competitiveness_is_stale <- function(years, max_age = COST_COMPETITIVENESS_MAX_OBS_AGE) {
  reference <- suppressWarnings(max(years, na.rm = TRUE))
  if (!is.finite(reference)) {
    return(rep(FALSE, length(years)))
  }

  is.na(years) | years < reference - as.integer(max_age)
}

# Fill missing values from the closest peer group available, trying each grouping in turn.
# `groups` is a list of lists of key vectors, applied narrowest first - typically
# region-and-income, then income alone. Mirrors the imputation the producer-price leg has
# always used, so the legs degrade the same way.
#
# `fallback` covers anything still missing after every grouping came up empty. It is left
# NULL where the caller has its own last resort - the producer-price leg falls back to the
# country's own rate index, which beats injecting a global median inflation rank - and given
# a value where a remaining NA would drop the country out of the composite.
cost_competitiveness_impute_by_peers <- function(values, groups, fallback = NULL) {
  for (group in groups) {
    if (!any(is.na(values) | is.nan(values))) {
      break
    }

    key <- do.call(paste, c(as.list(group), list(sep = "\r")))
    peer_mean <- tapply(values, key, function(v) mean(v, na.rm = TRUE))
    # tapply hands back a 1-d array; as.numeric() strips the dim and dimnames that would
    # otherwise ride along into the column.
    values <- dplyr::if_else(is.na(values), as.numeric(peer_mean[key]), values)
  }

  # An all-missing peer group averages to NaN. Normalising it back to NA keeps the caller's
  # own is.na()/coalesce() fallbacks working on the result.
  values[is.nan(values)] <- NA_real_

  if (!is.null(fallback)) {
    values <- dplyr::if_else(is.na(values), fallback, values)
  }

  values
}

# Which ILO economic-activity aggregate stands in for each supply-chain stage, and in what
# order of preference.
#
# Upstream reads mining and utilities rather than agriculture. The upstream stages this
# theme scores are extraction and refining, and farm wages proxy neither - agriculture was
# a placeholder. That series is thinner than the others (133 reporting economies against
# 172 for manufacturing, and it omits much of the EU), so Upstream falls back to the
# economy-wide total wherever mining is not reported, which keeps stage coverage at the
# same 165+ economies as before.
cost_competitiveness_ilo_activities <- function() {
  tibble::tribble(
    ~classif1.label, ~supply_chain, ~activity_rank,
    "Economic activity (Aggregate): Mining and quarrying; Electricity, gas and water supply", "Upstream", 1L,
    "Economic activity (Aggregate): Total", "Upstream", 2L,
    "Economic activity (Aggregate): Manufacturing", "Midstream", 1L,
    "Economic activity (Aggregate): Construction", "Downstream", 1L
  )
}

# Market-rate US dollars, not PPP. PPP conversion strips out exactly the price-level
# difference that constitutes a labour cost advantage, so the "Currency: 2021 PPP $" series
# this theme read until now understated low-cost producers: switching to market rates moves
# Egypt 36 places and Viet Nam 26 on the manufacturing ranking. US dollars is also the wider
# series - no economy reports PPP earnings without also reporting them in dollars.
COST_COMPETITIVENESS_ILO_CURRENCY <- "Currency: U.S. dollars"

cost_competitiveness_build_ilo_indices <- function(ilo_raw,
                                                   currency = COST_COMPETITIVENESS_ILO_CURRENCY,
                                                   max_age = COST_COMPETITIVENESS_MAX_OBS_AGE) {
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

  activities <- cost_competitiveness_ilo_activities()

  ilo_raw %>%
    dplyr::filter(
      classif1.label %in% activities$classif1.label,
      sex.label == "Total",
      classif2.label == currency,
      !is.na(obs_value)
    ) %>%
    dplyr::mutate(country_std = cost_competitiveness_normalize_iso_ilo(ref_area.label)) %>%
    dplyr::filter(!is.na(country_std)) %>%
    dplyr::slice_max(time, n = 1, with_ties = FALSE, by = c(country_std, classif1.label)) %>%
    # The floor is measured against the newest observation in the whole panel, not per
    # activity, so all three stages share one reference year.
    dplyr::mutate(labor_stale = cost_competitiveness_is_stale(time, max_age = max_age)) %>%
    # Ranked within the reporting series, so each country is placed against like-for-like
    # earnings instead of a pool that mixes mining wages with economy-wide averages. Stale
    # observations are held out of the ranking entirely - they would otherwise occupy
    # positions in a distribution they are decades removed from - and return as NA for
    # cost_competitiveness_impute_stale_labor() to fill.
    dplyr::group_by(classif1.label) %>%
    dplyr::mutate(
      labor_index = median_scurve(dplyr::if_else(labor_stale, NA_real_, obs_value))
    ) %>%
    dplyr::ungroup()
}

cost_competitiveness_build_labor_scaffold <- function(ilo_indices) {
  require_columns(
    ilo_indices,
    c("country_std", "classif1.label", "time", "obs_value", "labor_index", "labor_stale"),
    label = "ilo_indices"
  )

  ilo_indices %>%
    dplyr::inner_join(
      cost_competitiveness_ilo_activities(),
      by = "classif1.label",
      relationship = "many-to-one"
    ) %>%
    dplyr::mutate(
      # Best available series per stage; rank 2 is the economy-wide Upstream fallback. A
      # series held out by the recency floor is demoted below every usable one, so a country
      # with a stale mining wage but a current economy-wide figure reads the current one
      # rather than falling through to imputation.
      series_rank = dplyr::if_else(
        is.na(labor_index),
        activity_rank + 100L,
        activity_rank
      )
    ) %>%
    dplyr::slice_min(
      series_rank,
      n = 1,
      with_ties = FALSE,
      by = c(country_std, supply_chain)
    ) %>%
    dplyr::transmute(
      country_std,
      supply_chain,
      labor_activity = classif1.label,
      labor_year = as.integer(time),
      labor_stale,
      earnings_usd = obs_value,
      labor_index
    )
}

# The ILO panel reports a handful of economies that country_info does not carry. They would
# otherwise reach the theme table with a missing Country, which is not a usable key
# downstream, so they are dropped here - once, where the loss is visible - rather than
# turning into nameless rows at each of the three joins that key off this scaffold.
cost_competitiveness_filter_known_countries <- function(labor_scaffold, country_info) {
  require_columns(labor_scaffold, "country_std", label = "labor_scaffold")
  require_columns(country_info, "iso3c", label = "country_info")

  known <- unique(as.character(country_info$iso3c))
  unknown <- setdiff(unique(labor_scaffold$country_std), known)

  if (length(unknown) > 0) {
    warning(
      "Dropping ILO economies absent from country_info: ",
      paste(sort(unknown), collapse = ", ")
    )
  }

  labor_scaffold %>% dplyr::filter(country_std %in% known)
}

# Fill the labour index for economies whose newest ILO observation fell below the recency
# floor at every stage.
#
# They keep their row rather than being dropped, because the country universe for the whole
# theme - the capital leg and the composite included - is this scaffold: dropping a country
# for a stale wage would silently remove its cost of capital too. `earnings_usd` and
# `labor_year` keep reporting the real observation and its real year, so a reader can always
# see which countries are standing on imputed ground.
cost_competitiveness_impute_stale_labor <- function(labor_scaffold, country_info) {
  require_columns(
    labor_scaffold,
    c("country_std", "supply_chain", "labor_index", "labor_year"),
    label = "labor_scaffold"
  )
  require_columns(country_info, c("iso3c", "region", "income"), label = "country_info")

  country_ref <- country_info %>%
    dplyr::select(iso3c, region, income) %>%
    dplyr::distinct()
  assert_unique_keys(country_ref, "iso3c", label = "cost_competitiveness_labor_country_ref")

  imputed <- labor_scaffold %>%
    dplyr::left_join(country_ref, by = c("country_std" = "iso3c"), relationship = "many-to-one") %>%
    dplyr::mutate(labor_imputed = is.na(labor_index)) %>%
    dplyr::group_by(supply_chain) %>%
    dplyr::mutate(
      labor_index = cost_competitiveness_impute_by_peers(
        labor_index,
        groups = list(list(region, income), list(income)),
        fallback = stats::median(labor_index, na.rm = TRUE)
      )
    ) %>%
    dplyr::ungroup()

  stale <- imputed %>%
    dplyr::filter(labor_imputed) %>%
    dplyr::distinct(country_std, labor_year) %>%
    dplyr::arrange(labor_year)

  if (nrow(stale) > 0) {
    warning(
      "Imputing labour cost from regional peers for economies whose latest ILO earnings ",
      "observation predates the recency floor: ",
      paste0(stale$country_std, " (", stale$labor_year, ")", collapse = ", ")
    )
  }

  imputed %>% dplyr::select(-region, -income)
}

cost_competitiveness_build_labor_table <- function(ilo_sc,
                                                   labor_weights,
                                                   country_info,
                                                   year = 2024L) {
  require_columns(
    ilo_sc,
    c("country_std", "supply_chain", "labor_index", "labor_year", "earnings_usd"),
    label = "ilo_sc"
  )
  require_columns(labor_weights, c("Technology", "supply_chain", "labor_share"), label = "labor_weights")
  require_columns(country_info, c("iso3c", "country"), label = "country_info")

  country_map <- country_info %>%
    dplyr::select(iso3c, country) %>%
    dplyr::distinct()
  assert_unique_keys(country_map, "iso3c", label = "cost_competitiveness_country_info")

  ilo_sc %>%
    # Every country-stage row fans out across every technology sharing that stage.
    dplyr::inner_join(labor_weights, by = "supply_chain", relationship = "many-to-many") %>%
    dplyr::mutate(
      labor_index = 1 - labor_index,
      labor_index_weighted = labor_index * labor_share
    ) %>%
    dplyr::select(
      country_std,
      Technology,
      supply_chain,
      labor_year,
      earnings_usd,
      labor_share,
      labor_index,
      labor_index_weighted
    ) %>%
    dplyr::left_join(country_map, by = c("country_std" = "iso3c"), relationship = "many-to-one") %>%
    tidyr::pivot_longer(
      cols = c(earnings_usd, labor_share, labor_index, labor_index_weighted),
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
      # The year of the ILO observation behind the row, not the build year. ILO reporting
      # lags very unevenly, so a single stamp hid the fact that some countries are read off
      # a 2025 survey and others off one more than a decade old.
      Year = dplyr::coalesce(labor_year, as.integer(year)),
      source = "International Labor Organization",
      explanation = dplyr::case_when(
        variable == "labor_share" ~ "Estimated Labor Share of Costs",
        variable == "earnings_usd" ~ "Average monthly earnings, market-rate US dollars",
        variable == "labor_index" ~ paste(
          "Average monthly earnings by economic activity, market-rate US dollars,",
          "indexed (median_scurve); Upstream reads mining and utilities where reported,",
          "otherwise the economy-wide total"
        ),
        variable == "labor_index_weighted" ~ "Labor index x labor share (contribution, not an index)",
        TRUE ~ variable
      )
    )
}

# IMF Data Explorer exports carry one column per period ("2025-M07"), which read.csv
# renames to "X2025.M07". Both wide IMF inputs this theme reads are parsed the same way, so
# the parsing lives here rather than being duplicated per input.
#
# `id_cols` is narrowed to what the caller actually filters on. The exports also carry some
# forty metadata columns (DOI, FULL_DESCRIPTION, METHODOLOGY, SUGGESTED_CITATION and the
# like), and carrying those long strings through a reshape that turns 3,400 rows into three
# million is what made this step the theme's memory ceiling.
cost_competitiveness_parse_imf_periods <- function(imf_wide, id_cols) {
  imf_wide %>%
    dplyr::select(dplyr::any_of(id_cols), dplyr::starts_with("X")) %>%
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
      year = suppressWarnings(as.integer(stringr::str_sub(period, 1, 4))),
      month = dplyr::case_when(
        freq_tag == "M" ~ suppressWarnings(as.integer(stringr::str_sub(period, -2, -1))),
        freq_tag == "Q" ~ suppressWarnings(as.integer(stringr::str_sub(period, -1, -1))) * 3L,
        TRUE ~ 12L
      ),
      period_date = as.Date(sprintf("%04d-%02d-01", year, month))
    ) %>%
    # A handful of export columns are metadata rather than periods; they parse to NA here
    # instead of raising a coercion warning mid-pipeline.
    dplyr::filter(!is.na(period_date))
}

# Shift a period date by whole years, keeping the month, for year-on-year comparisons.
cost_competitiveness_shift_year <- function(dates, years = -1L) {
  as.Date(sprintf(
    "%04d-%02d-01",
    as.integer(format(dates, "%Y")) + as.integer(years),
    as.integer(format(dates, "%m"))
  ))
}

cost_competitiveness_clean_imf_rates <- function(imf_lending_rates) {
  require_columns(
    imf_lending_rates,
    c("COUNTRY", "INDICATOR", "FREQUENCY"),
    label = "imf_lending_rates"
  )

  cost_competitiveness_parse_imf_periods(
    imf_lending_rates,
    id_cols = c("COUNTRY", "INDICATOR", "FREQUENCY")
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

# Borrowing rates for economies IMF MFS_IR does not carry.
#
# Mainland China is absent from the flow entirely - it publishes only the Hong Kong and
# Macao SARs - and this theme previously renamed Macao to China, so China's cost of capital
# was a Macanese commercial lending rate of 5.5% against a Chinese policy benchmark of
# 3.0%. China anchors the IEA relative-cost leg and sits near the top of the theme, so the
# substitution biased the whole index against it. Overrides are quoted with the date the
# rate was read and are re-checked when the theme is rebuilt.
cost_competitiveness_rate_overrides <- function() {
  tibble::tribble(
    ~country_std, ~nominal_rate, ~period_date, ~chosen_indicator,
    "CHN", 3.00, as.Date("2026-08-20"),
    "PBoC 1-year Loan Prime Rate (override: IMF MFS_IR carries no mainland China series)"
  )
}

cost_competitiveness_build_rate_index <- function(imf_scored,
                                                  overrides = cost_competitiveness_rate_overrides(),
                                                  max_age = COST_COMPETITIVENESS_MAX_OBS_AGE) {
  require_columns(
    imf_scored,
    c("COUNTRY", "INDICATOR", "period_date", "value", "priority"),
    label = "imf_scored"
  )
  require_columns(
    overrides,
    c("country_std", "nominal_rate", "period_date", "chosen_indicator"),
    label = "cost_competitiveness_rate_overrides"
  )

  best_rate <- imf_scored %>%
    dplyr::arrange(COUNTRY, priority, dplyr::desc(period_date)) %>%
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
      # The SARs are the only "China" entries MFS_IR carries, and their full labels name
      # both the SAR and China, which countrycode's regex matches twice - it discards an
      # ambiguous match and warns. Shortening the label first resolves them cleanly, and
      # they resolve to themselves: mainland China comes from the override table above.
      # Whatever is still unmatched after this is a currency union or a dependent territory
      # the index does not score.
      country_label = dplyr::case_when(
        stringr::str_detect(COUNTRY, "^Hong Kong Special Administrative Region") ~ "Hong Kong",
        stringr::str_detect(COUNTRY, "^Macao Special Administrative Region") ~ "Macao",
        TRUE ~ COUNTRY
      ),
      country_std = countrycode::countrycode(
        country_label,
        "country.name",
        "iso3c",
        custom_match = c(
          "Poland, Republic of" = "POL",
          "Kosovo, Republic of" = "XKX"
        )
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

  # Overrides win over anything the flow reported, and are winsorized and scaled with the
  # rest of the panel so the override does not sit outside the index's own scale.
  best_rate <- best_rate %>%
    dplyr::filter(!country_std %in% overrides$country_std) %>%
    dplyr::bind_rows(overrides %>% dplyr::mutate(COUNTRY = country_std))

  best_rate <- best_rate %>%
    dplyr::mutate(
      rate_year = as.integer(format(period_date, "%Y")),
      rate_stale = cost_competitiveness_is_stale(rate_year, max_age = max_age)
    )

  # Stale rates are held out of the winsorization as well as the scaling. A 2001 lending
  # rate cannot stand in for today's cost of capital, and letting it set the clip bounds
  # would distort every other country's score too. They return as NA for the
  # region-and-income imputation in cost_competitiveness_build_capital_base().
  q <- stats::quantile(
    best_rate$nominal_rate[!best_rate$rate_stale],
    c(0.05, 0.95),
    na.rm = TRUE
  )

  best_rate %>%
    dplyr::mutate(
      rate_clip = dplyr::if_else(
        rate_stale,
        NA_real_,
        pmin(pmax(nominal_rate, q[1]), q[2])
      ),
      capital_cost_index = (rate_clip - min(rate_clip, na.rm = TRUE)) /
        (max(rate_clip, na.rm = TRUE) - min(rate_clip, na.rm = TRUE))
    ) %>%
    dplyr::select(
      country_std,
      chosen_indicator,
      period_date,
      rate_year,
      rate_stale,
      nominal_rate,
      capital_cost_index
    )
}

# Producer prices enter as year-on-year inflation, not as the index level.
#
# The IMF publishes each country's PPI on that country's own base year, so the levels are
# not comparable across countries: the staged export runs from the Philippines at 60 to
# Uzbekistan at 1124, which is a difference in base year and inflation history rather than
# in cost level. Ranking those levels - what this theme did until now - scored Uzbekistan as
# having near-worst input costs purely because of its rebasing. The year-on-year change is
# comparable across countries and reads as input-cost momentum: producer prices rising
# faster than peers erode a country's cost position.
cost_competitiveness_build_ppi <- function(imf_ppi) {
  require_columns(
    imf_ppi,
    c("COUNTRY", "INDICATOR", "TYPE_OF_TRANSFORMATION"),
    label = "imf_ppi"
  )

  ppi_long <- cost_competitiveness_parse_imf_periods(
    imf_ppi,
    id_cols = c("COUNTRY", "INDICATOR", "TYPE_OF_TRANSFORMATION")
  ) %>%
    dplyr::filter(
      INDICATOR == "Producer price index (PPI)",
      TYPE_OF_TRANSFORMATION == "Index"
    ) %>%
    dplyr::group_by(COUNTRY, freq_tag, period_date) %>%
    dplyr::summarize(value = mean(value, na.rm = TRUE), .groups = "drop")

  # One frequency per country - whichever reports most recently, monthly ahead of quarterly
  # ahead of annual on a tie - so both ends of the year-on-year pair come from one series.
  freq_rank <- c(M = 1L, Q = 2L, A = 3L)
  chosen_freq <- ppi_long %>%
    dplyr::group_by(COUNTRY, freq_tag) %>%
    dplyr::summarize(latest_date = max(period_date), .groups = "drop") %>%
    dplyr::mutate(freq_rank = freq_rank[freq_tag]) %>%
    dplyr::arrange(COUNTRY, dplyr::desc(latest_date), freq_rank) %>%
    dplyr::distinct(COUNTRY, .keep_all = TRUE)

  latest <- chosen_freq %>%
    dplyr::inner_join(
      ppi_long,
      by = c("COUNTRY", "freq_tag", "latest_date" = "period_date"),
      relationship = "one-to-one"
    ) %>%
    dplyr::transmute(
      COUNTRY,
      freq_tag,
      period_date = latest_date,
      ppi = value,
      prior_date = cost_competitiveness_shift_year(latest_date, -1L)
    )

  prior <- ppi_long %>%
    dplyr::transmute(COUNTRY, freq_tag, prior_date = period_date, ppi_prior = value)

  latest %>%
    dplyr::left_join(
      prior,
      by = c("COUNTRY", "freq_tag", "prior_date"),
      relationship = "many-to-one"
    ) %>%
    dplyr::mutate(
      # Countries without a matching prior-year observation fall through as NA and pick up
      # the region-and-income imputation in cost_competitiveness_build_capital_base().
      ppi_yoy = dplyr::if_else(
        !is.na(ppi_prior) & ppi_prior > 0,
        100 * (ppi / ppi_prior - 1),
        NA_real_
      ),
      ppi_index = median_scurve(ppi_yoy),
      country_std = countrycode::countrycode(
        COUNTRY,
        "country.name",
        "iso3c",
        custom_match = c(
          "Azerbaijan, Republic of" = "AZE",
          "Belarus, Republic of" = "BLR"
        )
      ),
      ppi_year = as.integer(format(period_date, "%Y"))
    ) %>%
    dplyr::filter(!is.na(country_std), !is.na(ppi)) %>%
    dplyr::select(
      COUNTRY,
      country_std,
      period_date,
      ppi_year,
      ppi,
      ppi_prior,
      ppi_yoy,
      ppi_index
    )
}

cost_competitiveness_build_capital_base <- function(rate_index,
                                                    ppi_clean,
                                                    country_info,
                                                    alpha = 0.60) {
  require_columns(
    rate_index,
    c("country_std", "capital_cost_index", "nominal_rate", "rate_year", "rate_stale"),
    label = "rate_index"
  )
  require_columns(
    ppi_clean,
    c("country_std", "ppi_index", "ppi", "ppi_yoy", "ppi_year"),
    label = "ppi_clean"
  )
  require_columns(country_info, c("iso3c", "region", "income"), label = "country_info")

  country_ref <- country_info %>%
    dplyr::select(iso3c, region, income) %>%
    dplyr::distinct()
  assert_unique_keys(country_ref, "iso3c", label = "cost_competitiveness_country_ref")

  rate_index %>%
    dplyr::left_join(ppi_clean, by = "country_std", relationship = "one-to-one") %>%
    dplyr::left_join(country_ref, by = c("country_std" = "iso3c"), relationship = "many-to-one") %>%
    # Merged before imputing rather than between the two passes, as it used to be. Low
    # income economies are few and rarely report a PPI at all, so their own region-and-income
    # cell is almost always empty; borrowing from the wider band on the first pass lands on a
    # closer peer group than falling through to the income-only one.
    dplyr::mutate(
      income = dplyr::if_else(income == "Low income", "Lower middle income", income)
    ) %>%
    dplyr::mutate(
      # No fallback: anything still missing is handled by the coalesce below, which reads the
      # country's own rate index.
      ppi_index = cost_competitiveness_impute_by_peers(
        ppi_index,
        groups = list(list(region, income), list(income))
      ),
      # Rates held out by the recency floor are filled the same way, so a country with an
      # unusably old rate keeps a capital score instead of dropping out of the composite.
      capital_cost_index = cost_competitiveness_impute_by_peers(
        capital_cost_index,
        groups = list(list(region, income), list(income)),
        fallback = stats::median(capital_cost_index, na.rm = TRUE)
      )
    ) %>%
    dplyr::mutate(
      cap_cost_index = alpha * capital_cost_index +
        (1 - alpha) * dplyr::coalesce(ppi_index, capital_cost_index),
      # The blend is only as current as its stalest input. Where the producer-price side was
      # imputed there is no PPI year, so the rate year stands alone.
      cap_year = pmin(rate_year, ppi_year, na.rm = TRUE)
    ) %>%
    dplyr::select(
      country_std,
      region,
      income,
      cap_cost_index,
      cap_year,
      nominal_rate,
      rate_year,
      rate_index = capital_cost_index,
      ppi_index,
      ppi,
      ppi_yoy,
      ppi_year
    )
}

cost_competitiveness_build_capital_table <- function(cap_cost_base,
                                                     cap_weights,
                                                     supply_chain_scaffold,
                                                     country_info,
                                                     year = 2024L) {
  require_columns(
    cap_cost_base,
    c(
      "country_std", "cap_cost_index", "cap_year", "nominal_rate", "rate_year",
      "rate_index", "ppi_index", "ppi", "ppi_yoy", "ppi_year"
    ),
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
      cap_year,
      rate_year,
      ppi_year,
      cap_share,
      cap_cost_index,
      cap_index_weighted,
      nominal_rate,
      rate_index,
      ppi_index,
      ppi,
      ppi_yoy
    ) %>%
    dplyr::left_join(country_map, by = c("country_std" = "iso3c"), relationship = "many-to-one") %>%
    tidyr::pivot_longer(
      cols = c(
        cap_share, cap_cost_index, cap_index_weighted,
        nominal_rate, rate_index, ppi_index, ppi, ppi_yoy
      ),
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
      # Each row carries the year of the observation it is built from: the rate side and the
      # producer-price side are read at different vintages, and the blend takes the stalest.
      Year = dplyr::coalesce(
        dplyr::case_when(
          variable %in% c("nominal_rate", "rate_index") ~ rate_year,
          variable %in% c("ppi", "ppi_yoy", "ppi_index") ~ ppi_year,
          TRUE ~ cap_year
        ),
        as.integer(year)
      ),
      source = "International Monetary Fund",
      explanation = dplyr::case_when(
        variable == "cap_share" ~ "Estimated Capital Share of Costs",
        variable == "ppi_index" ~ "Producer price inflation, year on year, indexed (median_scurve)",
        variable == "cap_index_weighted" ~ "Capital cost score * capital share (contribution, not an index)",
        variable == "nominal_rate" ~ "Lending rate, %",
        variable == "rate_index" ~ "Lending rate, winsorized min-max index (not median_scurve)",
        variable == "ppi_yoy" ~ "Producer price inflation, % year on year",
        variable == "ppi" ~ paste(
          "Producer price index level, country-specific base year;",
          "reported for context only and not comparable across countries"
        ),
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
  require_columns(
    labor_scaffold,
    c("country_std", "supply_chain", "labor_index", "labor_year"),
    label = "labor_scaffold"
  )
  require_columns(cap_cost_base, c("country_std", "cap_cost_index", "cap_year"), label = "cap_cost_base")
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
    dplyr::select(country_std, supply_chain, labor_comp, labor_year)

  cap_comp_c <- cap_cost_base %>%
    dplyr::mutate(cap_comp = 1 - cap_cost_index) %>%
    dplyr::select(country_std, cap_comp, cap_year)

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
      # A composite is only as current as its stalest leg.
      Year = dplyr::coalesce(pmin(labor_year, cap_year, na.rm = TRUE), as.integer(year)),
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
    c("Region", "country2", "tech", "supply_chain", "iea_year", "cost_index"),
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
      Year = dplyr::coalesce(iea_year, as.integer(year)),
      source = "IEA Energy Technology Perspectives 2024",
      explanation = paste(
        "Cost competitiveness relative to China; a regional value, so every country",
        "mapped to an IEA region carries the same score"
      )
    )
}

cost_competitiveness_validate_data_types <- function(tbl) {
  require_columns(tbl, c("variable", "data_type", "Country", "Year"), label = "cost_competitiveness_tbl")

  # Every leg reads at a different vintage, and within the ILO and IMF legs the vintage
  # varies by country. A single Year across a multi-country table means the observation
  # years have stopped propagating and rows are being stamped with the build year again.
  if (nrow(tbl) > 0 && dplyr::n_distinct(tbl$Country) > 1 && dplyr::n_distinct(tbl$Year) == 1) {
    stop(
      "All cost competitiveness rows carry Year == ",
      unique(tbl$Year),
      "; per-observation years are no longer being propagated."
    )
  }

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

  must_not_index <- c(
    "cap_share", "labor_share", "ppi", "ppi_yoy", "nominal_rate", "earnings_usd"
  )
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

# `year` is the fallback label only. Each leg stamps rows with the year of the observation
# behind them - the ILO survey year, the IMF rate or PPI period, the IEA extract's reference
# year - and `year` is used only where no observation year can be derived. Callers that
# slice the raw inputs to a target vintage (scripts/40_build_index_vintages.R) still pass it
# so those rows fall back to the vintage they were built for.
cost_competitiveness <- function(iea_cost_raw,
                                 ei,
                                 country_info,
                                 ilo_raw,
                                 imf_lending_rates,
                                 imf_ppi,
                                 year = 2024L,
                                 gamma = 0.5,
                                 alpha = 0.60,
                                 max_obs_age = COST_COMPETITIVENESS_MAX_OBS_AGE) {
  iea_cost_clean <- cost_competitiveness_clean_iea(iea_cost_raw)
  cost_indices <- cost_competitiveness_build_iea_indices(iea_cost_clean, gamma = gamma)
  country_maps <- cost_competitiveness_build_country_maps(ei)
  iea_cost_tbl <- cost_competitiveness_build_iea_table(
    cost_indices,
    country_map = country_maps$country_map,
    subregion_map = country_maps$subregion_map,
    year = year
  )

  ilo_indices <- cost_competitiveness_build_ilo_indices(ilo_raw, max_age = max_obs_age)
  ilo_scaffold <- cost_competitiveness_impute_stale_labor(
    cost_competitiveness_filter_known_countries(
      cost_competitiveness_build_labor_scaffold(ilo_indices),
      country_info = country_info
    ),
    country_info = country_info
  )
  labor_weights <- cost_competitiveness_labor_weights()
  labor_cost_tbl <- cost_competitiveness_build_labor_table(
    ilo_sc = ilo_scaffold,
    labor_weights = labor_weights,
    country_info = country_info,
    year = year
  )

  imf_long <- cost_competitiveness_clean_imf_rates(imf_lending_rates)
  imf_scored <- cost_competitiveness_select_imf_rates(imf_long)
  rate_index <- cost_competitiveness_build_rate_index(imf_scored, max_age = max_obs_age)
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
