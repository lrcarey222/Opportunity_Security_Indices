# Guards the five defects the Cost Competitiveness theme carried before this test existed:
#   1. labour earnings read in PPP dollars, which erases the cost advantage being measured
#   2. agriculture wages standing in for the extraction/refining Upstream stage
#   3. producer-price *levels* ranked across countries that publish on different base years
#   4. Macao's commercial lending rate emitted as mainland China's cost of capital
#   5. every row stamped with the build year, hiding vintages from the 1980s to 2025

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "categories", "economic opportunity", "cost_competitiveness.R"))

# --- fixtures ---------------------------------------------------------------------------

cc_countries <- function() {
  tibble::tibble(
    name = c("Germany", "India", "Viet Nam", "Brazil", "China", "United States of America"),
    iso3c = c("DEU", "IND", "VNM", "BRA", "CHN", "USA")
  )
}

# ILO earnings in both currencies. The PPP and USD orderings are deliberately different:
# Viet Nam is cheap in market dollars but mid-pack in PPP terms, which is the distortion the
# theme used to inherit. Germany reports no mining aggregate, so it must fall back to Total.
cc_ilo_fixture <- function() {
  countries <- cc_countries()

  activity <- c(
    mining = "Economic activity (Aggregate): Mining and quarrying; Electricity, gas and water supply",
    manufacturing = "Economic activity (Aggregate): Manufacturing",
    construction = "Economic activity (Aggregate): Construction",
    total = "Economic activity (Aggregate): Total",
    agriculture = "Economic activity (Aggregate): Agriculture"
  )

  # Viet Nam is well below China on market rates and just above it in PPP terms. That
  # inversion is the whole point: it is what the PPP series used to hide. Both report
  # currently, so the recency floor cannot interfere with the comparison.
  usd <- c(Germany = 5000, India = 220, `Viet Nam` = 350, Brazil = 700, China = 830, `United States of America` = 7000)
  ppp <- c(Germany = 5200, India = 900, `Viet Nam` = 1700, Brazil = 1400, China = 1600, `United States of America` = 7100)

  grid <- tidyr::expand_grid(
    ref_area.label = countries$name,
    classif1.label = unname(activity),
    classif2.label = c("Currency: U.S. dollars", "Currency: 2021 PPP $")
  )

  grid %>%
    dplyr::filter(
      # Germany publishes no mining aggregate, exactly as in the live panel.
      !(ref_area.label == "Germany" & classif1.label == activity[["mining"]])
    ) %>%
    dplyr::mutate(
      sex.label = "Total",
      # Brazil's only observation is ancient; everyone else reports recently. The theme must
      # carry that difference through rather than stamping one year on all of them.
      time = dplyr::if_else(ref_area.label == "Brazil", 1995L, 2025L),
      base = dplyr::if_else(
        classif2.label == "Currency: U.S. dollars",
        unname(usd[ref_area.label]),
        unname(ppp[ref_area.label])
      ),
      # Mining pays above manufacturing, construction below it, agriculture far below - so a
      # stage reading the wrong activity produces a visibly different index.
      obs_value = base * dplyr::case_when(
        classif1.label == activity[["mining"]] ~ 1.4,
        classif1.label == activity[["manufacturing"]] ~ 1.0,
        classif1.label == activity[["construction"]] ~ 0.8,
        classif1.label == activity[["agriculture"]] ~ 0.3,
        TRUE ~ 0.95
      )
    ) %>%
    dplyr::select(ref_area.label, classif1.label, sex.label, classif2.label, time, obs_value)
}

# Wide IMF export, as read.csv delivers it: metadata columns plus "X<year>.M<mm>" periods.
cc_imf_rates_fixture <- function() {
  rates <- tibble::tribble(
    ~COUNTRY, ~INDICATOR, ~rate_2025, ~rate_2024,
    "Germany", "Lending Rate, Percent per annum", 4.1, 3.9,
    "India", "Lending Rate, Percent per annum", 9.2, 9.0,
    "Viet Nam", "Lending Rate, Percent per annum", 7.4, 7.6,
    "Brazil", "Lending Rate, Percent per annum", 12.5, 12.1,
    "United States", "Lending Rate, Percent per annum", 7.5, 8.0,
    # The two SARs are the only "China" entries the live flow carries.
    "Macao Special Administrative Region, People's Republic of China",
    "Lending Rate, Percent per annum", 5.5, 5.4,
    "Hong Kong Special Administrative Region, People's Republic of China",
    "Lending Rate, Percent per annum", 5.2, 5.3
  )

  out <- tibble::tibble(
    DATASET = "IMF.STA:MFS_IR",
    COUNTRY = rates$COUNTRY,
    INDICATOR = rates$INDICATOR,
    FREQUENCY = "Monthly",
    # A long metadata column: the parser must not drag this through the reshape.
    FULL_DESCRIPTION = strrep("x", 200)
  )
  out$X2025.M06 <- rates$rate_2025
  out$X2024.M06 <- rates$rate_2024
  out
}

# PPI levels on deliberately incomparable base years. Brazil sits highest in level while
# running middling inflation; Germany sits lowest in level while inflating fastest. Ranking
# levels and ranking year-on-year change therefore give opposite answers.
cc_imf_ppi_fixture <- function() {
  ppi <- tibble::tribble(
    ~COUNTRY, ~latest, ~prior,
    "Germany", 104.0, 100.0,   # +4.0%, lowest level
    "India", 190.0, 187.0,     # +1.6%
    "Viet Nam", 260.0, 258.0,  # +0.8%
    "Brazil", 640.0, 625.0,    # +2.4%, highest level
    "China", 130.0, 129.0,     # +0.8%
    "United States", 150.0, 146.0
  )

  out <- tibble::tibble(
    COUNTRY = ppi$COUNTRY,
    INDICATOR = "Producer price index (PPI)",
    TYPE_OF_TRANSFORMATION = "Index",
    FULL_DESCRIPTION = strrep("y", 200)
  )
  out$X2025.M06 <- ppi$latest
  out$X2024.M06 <- ppi$prior
  out
}

cc_iea_fixture <- function(year = 2023L) {
  tidyr::expand_grid(
    Product = c("Solar PV", "Wind turbines", "Batteries"),
    Region = c("China", "United States", "European Union", "India")
  ) %>%
    dplyr::mutate(
      Unit = "%",
      Year = year,
      Value = dplyr::case_when(
        Region == "China" ~ 100,
        Region == "United States" ~ 120,
        Region == "European Union" ~ 145,
        TRUE ~ 110
      )
    )
}

cc_country_info <- function() {
  tibble::tibble(
    iso3c = c("DEU", "IND", "VNM", "BRA", "CHN", "USA"),
    country = c("Germany", "India", "Vietnam", "Brazil", "China", "United States"),
    region = c("Europe & Central Asia", "South Asia", "East Asia & Pacific",
               "Latin America & Caribbean", "East Asia & Pacific", "North America"),
    income = c("High income", "Lower middle income", "Lower middle income",
               "Upper middle income", "Upper middle income", "High income")
  )
}

cc_ei <- function() {
  tibble::tibble(
    Country = c("Germany", "India", "Vietnam", "Brazil", "China", "US"),
    EU = c(1L, 0L, 0L, 0L, 0L, 0L),
    SubRegion = c("Europe", "Asia Pacific", "Asia Pacific", "S. & Cent. America",
                  "Asia Pacific", "North America")
  )
}

# Brazil is deliberately below the recency floor, so a full build always warns about
# imputing it. Suppressed here to keep the other tests' output readable; the warning itself
# is asserted in "the recency floor holds stale observations out of the ranking".
cc_build <- function(...) {
  suppressWarnings(cost_competitiveness(
    iea_cost_raw = cc_iea_fixture(),
    ei = cc_ei(),
    country_info = cc_country_info(),
    ilo_raw = cc_ilo_fixture(),
    imf_lending_rates = cc_imf_rates_fixture(),
    imf_ppi = cc_imf_ppi_fixture(),
    ...
  ))
}

# --- labour leg -------------------------------------------------------------------------

test_that("labour earnings are read in market dollars, not PPP", {
  expect_identical(COST_COMPETITIVENESS_ILO_CURRENCY, "Currency: U.S. dollars")

  indices <- cost_competitiveness_build_ilo_indices(cc_ilo_fixture())
  scaffold <- cost_competitiveness_build_labor_scaffold(indices)

  midstream <- scaffold %>% dplyr::filter(supply_chain == "Midstream")
  usd <- c(DEU = 5000, IND = 220, VNM = 350, BRA = 700, CHN = 830, USA = 7000)
  expect_equal(
    midstream$earnings_usd[match(names(usd), midstream$country_std)],
    unname(usd),
    tolerance = 1e-9
  )

  # Viet Nam is cheaper than China on market rates and dearer on PPP. Reading dollars, its
  # labour index must therefore sit below China's - the ordering the PPP series inverted.
  vnm <- midstream$labor_index[midstream$country_std == "VNM"]
  chn <- midstream$labor_index[midstream$country_std == "CHN"]
  expect_lt(vnm, chn)

  # And the PPP series really would have inverted it, so the test above is not vacuous.
  ppp_scaffold <- cost_competitiveness_build_labor_scaffold(
    cost_competitiveness_build_ilo_indices(cc_ilo_fixture(), currency = "Currency: 2021 PPP $")
  ) %>%
    dplyr::filter(supply_chain == "Midstream")
  expect_gt(
    ppp_scaffold$labor_index[ppp_scaffold$country_std == "VNM"],
    ppp_scaffold$labor_index[ppp_scaffold$country_std == "CHN"]
  )
})

test_that("Upstream reads mining and utilities, falling back to the economy-wide total", {
  scaffold <- cost_competitiveness_build_labor_scaffold(
    cost_competitiveness_build_ilo_indices(cc_ilo_fixture())
  )
  upstream <- scaffold %>% dplyr::filter(supply_chain == "Upstream")

  # India reports mining, so it must be read off mining - at 1.4x manufacturing.
  ind <- upstream %>% dplyr::filter(country_std == "IND")
  expect_match(ind$labor_activity, "Mining and quarrying")
  expect_equal(ind$earnings_usd, 220 * 1.4, tolerance = 1e-9)

  # Germany reports none, so it falls back to Total at 0.95x - never to agriculture at 0.3x.
  deu <- upstream %>% dplyr::filter(country_std == "DEU")
  expect_match(deu$labor_activity, "Total$")
  expect_equal(deu$earnings_usd, 5000 * 0.95, tolerance = 1e-9)

  # Agriculture must not reach any stage.
  expect_false(any(grepl("Agriculture", scaffold$labor_activity)))

  # Every country keeps a row at every stage despite the thinner mining series.
  expect_setequal(unique(upstream$country_std), cc_countries()$iso3c)
  expect_setequal(unique(scaffold$supply_chain), c("Upstream", "Midstream", "Downstream"))
})

# --- recency floor ----------------------------------------------------------------------

test_that("staleness is measured against the panel, not the system clock", {
  # A panel whose newest reading is 2025 with a 10-year floor: 2014 is out, 2015 is in.
  expect_equal(
    cost_competitiveness_is_stale(c(2025L, 2016L, 2015L, 2014L, 1980L), max_age = 10L),
    c(FALSE, FALSE, FALSE, TRUE, TRUE)
  )

  # Slide the whole panel back and the floor slides with it, so a vintage rebuild against
  # inputs sliced to 2010 does not declare every one of its own observations stale.
  expect_equal(
    cost_competitiveness_is_stale(c(2010L, 2000L, 1999L), max_age = 10L),
    c(FALSE, FALSE, TRUE)
  )

  expect_equal(cost_competitiveness_is_stale(c(NA_integer_, NA_integer_)), c(FALSE, FALSE))
  expect_equal(cost_competitiveness_is_stale(c(2025L, NA_integer_)), c(FALSE, TRUE))
})

test_that("the recency floor holds stale observations out of the ranking", {
  indices <- cost_competitiveness_build_ilo_indices(cc_ilo_fixture())

  # Brazil's 1995 reading is kept and reported, but carries no index of its own.
  bra <- indices %>% dplyr::filter(country_std == "BRA", grepl("Manufacturing", classif1.label))
  expect_true(bra$labor_stale)
  expect_true(is.na(bra$labor_index))
  expect_equal(bra$obs_value, 700, tolerance = 1e-9)

  # And it is excluded from the ranking, not merely flagged: the five fresh countries take
  # the full 0-1 spread between them.
  fresh <- indices %>%
    dplyr::filter(grepl("Manufacturing", classif1.label), !labor_stale)
  expect_equal(nrow(fresh), 5L)
  expect_equal(min(fresh$labor_index), 0, tolerance = 1e-9)
  expect_equal(max(fresh$labor_index), 1, tolerance = 1e-9)

  # Raising the floor past 1995 brings Brazil back into the ranking.
  lenient <- cost_competitiveness_build_ilo_indices(cc_ilo_fixture(), max_age = 40L)
  expect_false(any(lenient$labor_stale))
  expect_false(any(is.na(lenient$labor_index)))
})

test_that("a stale country keeps its row, imputed from regional peers", {
  scaffold <- cost_competitiveness_build_labor_scaffold(
    cost_competitiveness_build_ilo_indices(cc_ilo_fixture())
  )
  expect_true(all(is.na(scaffold$labor_index[scaffold$country_std == "BRA"])))

  expect_warning(
    imputed <- cost_competitiveness_impute_stale_labor(scaffold, cc_country_info()),
    "BRA \\(1995\\)"
  )

  bra <- imputed %>% dplyr::filter(country_std == "BRA")
  # Every stage keeps a usable index...
  expect_equal(nrow(bra), 3L)
  expect_false(any(is.na(bra$labor_index)))
  expect_true(all(bra$labor_imputed))
  # ...while the raw observation and its real year stay visible for audit.
  expect_equal(unique(bra$labor_year), 1995L)
  expect_false(any(is.na(bra$earnings_usd)))

  # Countries with current data are untouched.
  fresh <- imputed %>% dplyr::filter(country_std != "BRA")
  expect_false(any(fresh$labor_imputed))
})

test_that("a fresh fallback series beats a stale preferred one", {
  fixture <- cc_ilo_fixture()
  mining <- "Economic activity (Aggregate): Mining and quarrying; Electricity, gas and water supply"

  # India's mining reading goes stale while its economy-wide total stays current.
  fixture$time[fixture$ref_area.label == "India" & fixture$classif1.label == mining] <- 1990L

  upstream <- cost_competitiveness_build_labor_scaffold(
    cost_competitiveness_build_ilo_indices(fixture)
  ) %>%
    dplyr::filter(supply_chain == "Upstream", country_std == "IND")

  # It must read the current total rather than falling through to imputation.
  expect_match(upstream$labor_activity, "Total$")
  expect_false(is.na(upstream$labor_index))
  expect_equal(upstream$labor_year, 2025L)
})

test_that("stale rates are excluded from the winsorization and then imputed", {
  fixture <- cc_imf_rates_fixture()
  # Brazil's 12.5% is the panel's top rate. Move its only reading back to 2010 and it must
  # stop both scoring and setting the winsorization bounds.
  fixture$X2025.M06[fixture$COUNTRY == "Brazil"] <- NA_real_
  fixture$X2024.M06[fixture$COUNTRY == "Brazil"] <- NA_real_
  fixture$X2010.M06 <- NA_real_
  fixture$X2010.M06[fixture$COUNTRY == "Brazil"] <- 12.5

  rates <- cost_competitiveness_build_rate_index(
    cost_competitiveness_select_imf_rates(cost_competitiveness_clean_imf_rates(fixture))
  )

  bra <- rates %>% dplyr::filter(country_std == "BRA")
  expect_true(bra$rate_stale)
  expect_equal(bra$nominal_rate, 12.5, tolerance = 1e-9)   # the observation is preserved
  expect_true(is.na(bra$capital_cost_index))               # but it is not scored

  # The scale is now set by the fresh countries alone: India's 9.2% tops it.
  expect_equal(rates$country_std[which.max(rates$capital_cost_index)], "IND")

  # The capital base then fills Brazil in from its peers rather than dropping it.
  base <- cost_competitiveness_build_capital_base(
    rate_index = rates,
    ppi_clean = cost_competitiveness_build_ppi(cc_imf_ppi_fixture()),
    country_info = cc_country_info()
  )
  expect_false(is.na(base$cap_cost_index[base$country_std == "BRA"]))
})

test_that("peer imputation walks outward and only then uses the fallback", {
  values <- c(NA, 0.4, 0.6, NA, 0.8)
  region <- c("A", "A", "A", "B", "C")
  income <- c("high", "high", "high", "low", "low")

  filled <- cost_competitiveness_impute_by_peers(
    values,
    groups = list(list(region, income), list(income)),
    fallback = -1
  )

  # Position 1 has region-and-income peers: mean(0.4, 0.6).
  expect_equal(filled[1], 0.5, tolerance = 1e-9)
  # Position 4's own cell is empty, so it falls to the income band: 0.8 is its only peer.
  expect_equal(filled[4], 0.8, tolerance = 1e-9)
  expect_equal(filled[c(2, 3, 5)], c(0.4, 0.6, 0.8), tolerance = 1e-9)

  # With no peers anywhere, the fallback applies - and without one, the NA survives for the
  # caller's own last resort.
  expect_equal(
    cost_competitiveness_impute_by_peers(c(NA_real_), groups = list(list("A")), fallback = -1),
    -1
  )
  expect_true(is.na(
    cost_competitiveness_impute_by_peers(c(NA_real_), groups = list(list("A")))
  ))
})

# --- producer prices --------------------------------------------------------------------

test_that("producer prices are indexed on year-on-year change, not on the level", {
  ppi <- cost_competitiveness_build_ppi(cc_imf_ppi_fixture())

  expect_setequal(
    c("COUNTRY", "country_std", "period_date", "ppi_year", "ppi", "ppi_prior", "ppi_yoy", "ppi_index"),
    names(ppi)
  )
  expect_equal(
    ppi$ppi_yoy[ppi$country_std == "DEU"],
    100 * (104 / 100 - 1),
    tolerance = 1e-9
  )

  # Germany has the lowest level but the fastest inflation, Viet Nam the reverse. Indexing
  # the level would rank Germany cheapest; indexing the change must rank it dearest.
  expect_gt(
    ppi$ppi_index[ppi$country_std == "DEU"],
    ppi$ppi_index[ppi$country_std == "VNM"]
  )
  expect_equal(
    ppi$country_std[which.max(ppi$ppi_index)],
    "DEU"
  )
  expect_gt(ppi$ppi[ppi$country_std == "BRA"], ppi$ppi[ppi$country_std == "DEU"])
})

test_that("a country with no prior-year observation yields no inflation reading", {
  fixture <- cc_imf_ppi_fixture()
  fixture$X2024.M06[fixture$COUNTRY == "India"] <- NA_real_

  ppi <- cost_competitiveness_build_ppi(fixture)
  ind <- ppi %>% dplyr::filter(country_std == "IND")

  expect_true(is.na(ind$ppi_yoy))
  expect_true(is.na(ind$ppi_index))
  # The level is still reported for context.
  expect_equal(ind$ppi, 190, tolerance = 1e-9)
})

# --- cost of capital --------------------------------------------------------------------

test_that("mainland China takes the override rate, not Macao's lending rate", {
  scored <- cost_competitiveness_select_imf_rates(
    cost_competitiveness_clean_imf_rates(cc_imf_rates_fixture())
  )
  rates <- cost_competitiveness_build_rate_index(scored)

  chn <- rates %>% dplyr::filter(country_std == "CHN")
  expect_equal(nrow(chn), 1L)
  expect_equal(chn$nominal_rate, 3.00, tolerance = 1e-9)
  expect_match(chn$chosen_indicator, "Loan Prime Rate")

  # Macao and Hong Kong stay themselves rather than being renamed into China.
  expect_equal(rates$nominal_rate[rates$country_std == "MAC"], 5.5, tolerance = 1e-9)
  expect_equal(rates$nominal_rate[rates$country_std == "HKG"], 5.2, tolerance = 1e-9)

  # China's rate is the panel's lowest, so it must score most competitive on the cost side.
  expect_equal(rates$country_std[which.min(rates$capital_cost_index)], "CHN")
})

test_that("the override is scaled with the panel and can be swapped out", {
  scored <- cost_competitiveness_select_imf_rates(
    cost_competitiveness_clean_imf_rates(cc_imf_rates_fixture())
  )

  swapped <- cost_competitiveness_build_rate_index(
    scored,
    overrides = tibble::tibble(
      country_std = "CHN",
      nominal_rate = 20,
      period_date = as.Date("2026-01-01"),
      chosen_indicator = "test"
    )
  )

  expect_equal(swapped$nominal_rate[swapped$country_std == "CHN"], 20, tolerance = 1e-9)
  # Winsorized min-max, so the override sits inside the index's own 0-1 scale.
  expect_true(all(swapped$capital_cost_index >= 0 & swapped$capital_cost_index <= 1))

  empty <- cost_competitiveness_build_rate_index(
    scored,
    overrides = tibble::tibble(
      country_std = character(),
      nominal_rate = numeric(),
      period_date = as.Date(character()),
      chosen_indicator = character()
    )
  )
  expect_false("CHN" %in% empty$country_std)
})

# --- vintages ---------------------------------------------------------------------------

cc_build_iea_year <- function(year) {
  suppressWarnings(cost_competitiveness(
    iea_cost_raw = cc_iea_fixture(year),
    ei = cc_ei(),
    country_info = cc_country_info(),
    ilo_raw = cc_ilo_fixture(),
    imf_lending_rates = cc_imf_rates_fixture(),
    imf_ppi = cc_imf_ppi_fixture()
  ))
}

test_that("rows carry the year of the observation behind them", {
  out <- cc_build()

  # Brazil's only ILO observation is from 1995; everyone else's is 2025.
  labor <- out %>% dplyr::filter(variable == "labor_index")
  expect_equal(unique(labor$Year[labor$Country == "Brazil"]), 1995L)
  expect_equal(unique(labor$Year[labor$Country == "India"]), 2025L)

  # The rate side is read at its own period: the IMF panel at 2025, China at the override's
  # own as-of date, which is later.
  rates <- out %>% dplyr::filter(variable == "nominal_rate", !is.na(value))
  expect_equal(unique(rates$Year[rates$Country != "China"]), 2025L)
  expect_equal(unique(rates$Year[rates$Country == "China"]), 2026L)
  expect_equal(unique(out$Year[out$variable == "ppi_yoy" & !is.na(out$value)]), 2025L)

  # The IEA leg takes the extract's reference year, not the build year.
  expect_equal(unique(out$Year[out$variable == "IEA Cost index"]), 2023L)
  reindexed <- cc_build_iea_year(2021L)
  expect_equal(unique(reindexed$Year[reindexed$variable == "IEA Cost index"]), 2021L)

  # A composite is only as current as its stalest leg.
  brazil_composite <- out %>%
    dplyr::filter(variable == "Input Cost Index", Country == "Brazil")
  expect_equal(unique(brazil_composite$Year), 1995L)
})

test_that("the validator rejects a table stamped with a single year", {
  out <- cc_build()
  expect_error(cost_competitiveness_validate_data_types(out), NA)

  stamped <- out
  stamped$Year <- 2024L
  expect_error(
    cost_competitiveness_validate_data_types(stamped),
    "no longer being propagated"
  )
})

# --- whole theme ------------------------------------------------------------------------

test_that("the theme emits a valid table with the expected variables and data types", {
  out <- cc_build()

  expect_error(validate_schema(out), NA)
  expect_true(all(
    c("labor_index", "earnings_usd", "cap_cost_index", "ppi_yoy", "Input Cost Index",
      "IEA Cost index") %in% out$variable
  ))
  expect_setequal(unique(out$category), "Cost Competitiveness")

  # Raw observations must never be labelled as indices.
  raw_only <- out %>%
    dplyr::filter(variable %in% c("earnings_usd", "ppi", "ppi_yoy", "nominal_rate"))
  expect_setequal(unique(raw_only$data_type), "raw")

  # The composite stays inside 0-1 and higher must mean more cost-competitive: India's
  # cheap labour and China's cheap capital should both beat the United States.
  composite <- out %>%
    dplyr::filter(variable == "Input Cost Index", tech == "Solar", supply_chain == "Midstream")
  expect_true(all(composite$value >= 0 & composite$value <= 1, na.rm = TRUE))
  expect_gt(
    composite$value[composite$Country == "India"],
    composite$value[composite$Country == "United States"]
  )
})

test_that("ILO economies absent from country_info are dropped with a warning", {
  scaffold <- cost_competitiveness_build_labor_scaffold(
    cost_competitiveness_build_ilo_indices(cc_ilo_fixture())
  )
  trimmed_info <- cc_country_info() %>% dplyr::filter(iso3c != "VNM")

  expect_warning(
    kept <- cost_competitiveness_filter_known_countries(scaffold, trimmed_info),
    "VNM"
  )
  expect_false("VNM" %in% kept$country_std)

  # No warning when every economy is known.
  expect_warning(
    cost_competitiveness_filter_known_countries(scaffold, cc_country_info()),
    NA
  )
})

test_that("the IMF parser keeps only the columns its caller filters on", {
  parsed <- cost_competitiveness_parse_imf_periods(
    cc_imf_rates_fixture(),
    id_cols = c("COUNTRY", "INDICATOR", "FREQUENCY")
  )

  expect_false("FULL_DESCRIPTION" %in% names(parsed))
  expect_false("DATASET" %in% names(parsed))
  expect_true(all(c("COUNTRY", "INDICATOR", "FREQUENCY", "period_date", "value") %in% names(parsed)))
  expect_setequal(unique(format(parsed$period_date, "%Y-%m")), c("2025-06", "2024-06"))
})
