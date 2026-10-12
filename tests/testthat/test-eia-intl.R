# EIA International Energy Statistics fill of the EI fuel production gaps (user decision of
# 2026-10-11): the bulk-file parser and unit conversions (R/utils/eia_intl.R), the fetcher
# (scripts/fetchers/eia_intl.R, with the HTTP layer stubbed), and the import dependence and
# production themes with the extract. EI's production tables omit economies they fold into
# "Other <region>" residuals; those cells take the EIA value of the same year (gas and coal, a
# year behind in EIA, from its latest year), an EIA zero is a real zero, EI stays the source
# where it reports, and a value whose fuel is not EI's (Estonia coal: EI counts oil shale) is
# not used.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "levels.R"))
source(file.path(repo_root, "R", "utils", "eia_intl.R"))
source(file.path(repo_root, "R", "categories", "shared", "overall_index.R"))
source(file.path(repo_root, "R", "categories", "energy_imports", "import_dependence.R"))
source(file.path(repo_root, "R", "categories", "production", "production_depth_momentum.R"))

# ---- Bulk-file lines -----------------------------------------------------------------------

# Lines as INTL.txt has them: one JSON object per series, plus category records and series the
# extract does not keep.
eia_bulk_lines <- function() {
  c(
    '{"category_id":"2134384","parent_category_id":"2134155","name":"Dry natural gas production"}',
    paste0(
      '{"series_id":"INTL.26-1-NZL-BCM.A","name":"Dry natural gas production, New Zealand, Annual",',
      '"units":"billion cubic meters","f":"A","last_updated":"2026-01-27T14:44:16-05:00",',
      '"data":[["2025","--"],["2024",3.344],["2023",3.9],["1999",5]]}'
    ),
    paste0(
      '{"series_id":"INTL.57-1-KOR-TBPD.A","name":"Crude oil including lease condensate production, Korea, South, Annual",',
      '"units":"thousand barrels per day","f":"A","data":[["2025",0],["2024",null],["2023","ie"]]}'
    ),
    # A world aggregate, a monthly series, another product, another unit: not kept.
    '{"series_id":"INTL.57-1-WORL-TBPD.A","name":"Crude oil including lease condensate production, World, Annual","data":[["2025",80000]]}',
    '{"series_id":"INTL.57-1-KOR-TBPD.M","name":"Crude oil including lease condensate production, Korea, South, Monthly","data":[["202501",0]]}',
    '{"series_id":"INTL.44-1-KOR-QBTU.A","name":"Total energy production, Korea, South, Annual","data":[["2025",2]]}',
    '{"series_id":"INTL.26-1-NZL-TJ.A","name":"Dry natural gas production, New Zealand, Annual","data":[["2024",130000]]}',
    # Broken JSON is skipped, not an error; a name is data, never evaluated.
    '{"series_id":"INTL.26-1-AUS-BCM.A","name":"Dry natural gas production, Australia, Annual","data":[["2024",',
    '{"series_id":"INTL.7-1-EST-TJ.A","name":"Coal production, Estonia, Annual","data":[["2024",0],["2023","w"]]}',
    '{"series_id":"INTL.7-2-EST-TJ.A","name":"system(\\"echo hi\\"), Estonia, Annual","data":[["2024",80]]}'
  )
}

test_that("the parser keeps the wanted annual ISO3 series and reads non-numbers as missing", {
  out <- eia_intl_parse_lines(eia_bulk_lines())

  expect_identical(names(out), EIA_INTL_COLUMNS)
  expect_setequal(unique(out$series_id), c("INTL.26-1-NZL-BCM.A", "INTL.57-1-KOR-TBPD.A", "INTL.7-1-EST-TJ.A", "INTL.7-2-EST-TJ.A"))
  nz <- out[out$series_id == "INTL.26-1-NZL-BCM.A", ]
  expect_identical(nz$year, c(1999L, 2023L, 2024L, 2025L))
  expect_equal(nz$value, c(5, 3.9, 3.344, NA))
  expect_identical(nz$value_note, c("", "", "", "--"))
  expect_identical(unique(nz$country), "New Zealand")
  expect_identical(unique(nz$fuel), "gas")
  expect_identical(unique(nz$flow), "production")

  kor <- out[out$iso3 == "KOR", ]
  expect_identical(unique(kor$country), "Korea, South")
  # A reported 0 is 0; JSON null and EIA's codes are missing with the code kept, never 0.
  expect_identical(kor$value[kor$year == 2025], 0)
  expect_true(is.na(kor$value[kor$year == 2024]))
  expect_identical(kor$value_note[kor$year == 2024], "NA")
  expect_identical(kor$value_note[kor$year == 2023], "ie")
  expect_identical(out$value_note[out$series_id == "INTL.7-1-EST-TJ.A" & out$year == 2023], "w")
  # The odd name is only a string.
  expect_identical(unique(out$country[out$series_id == "INTL.7-2-EST-TJ.A"]), "Estonia")

  # min_year drops earlier years (the 1999 New Zealand value).
  expect_true(1999L %in% out$year)
  expect_true(all(eia_intl_parse_lines(eia_bulk_lines(), min_year = 2000)$year >= 2000L))
})

# ---- Staged extract, as data/raw/eia_intl_fossil_production.csv has it -------------------------

eia_row <- function(iso3, fuel, flow, component, unit, values, product, activity) {
  tibble::tibble(
    iso3 = iso3, country = iso3, fuel = fuel, flow = flow, component = component,
    year = as.integer(names(values)), value = unname(values), unit = unit,
    series_id = paste0("INTL.", product, "-", activity, "-", iso3, "-", unit, ".A"),
    value_note = ifelse(is.na(unname(values)), "--", "")
  )
}

eia_extract_fixture <- function() {
  dplyr::bind_rows(
    # New Zealand: oil (crude + NGPL) to 2025, gas to 2024 only, as in the real file.
    eia_row("NZL", "oil", "production", "crude_condensate", "TBPD", c("2015" = 30, "2020" = 25, "2025" = 8), 57, 1),
    eia_row("NZL", "oil", "production", "ngpl", "TBPD", c("2015" = 6, "2020" = 5, "2025" = 4), 58, 1),
    eia_row("NZL", "gas", "production", "dry_gas", "BCM", c("2015" = 4.5, "2020" = 4, "2024" = 3.3, "2025" = NA), 26, 1),
    eia_row("NZL", "gas", "consumption", "dry_gas", "BCM", c("2015" = 4.4, "2020" = 4, "2024" = 2.8), 26, 2),
    # Netherlands: EI reports oil (20); EIA's 23 must not replace it. No EI coal production row;
    # EIA reports 0.
    eia_row("NLD", "oil", "production", "crude_condensate", "TBPD", c("2025" = 23), 57, 1),
    eia_row("NLD", "coal", "production", "coal", "TJ", c("2024" = 0, "2025" = NA), 7, 1),
    eia_row("NLD", "coal", "consumption", "coal", "TJ", c("2024" = 200000), 7, 2),
    # Spain: EI reports coal production 0; EIA's positive value must not replace it.
    eia_row("ESP", "coal", "production", "coal", "TJ", c("2024" = 5000), 7, 1),
    eia_row("ESP", "oil", "production", "crude_condensate", "TBPD", c("2025" = 2), 57, 1),
    # Singapore: zeros.
    eia_row("SGP", "oil", "production", "crude_condensate", "TBPD", c("2025" = 0), 57, 1),
    eia_row("SGP", "gas", "production", "dry_gas", "BCM", c("2024" = 0), 26, 1),
    eia_row("SGP", "coal", "production", "coal", "TJ", c("2024" = 0), 7, 1),
    # Iceland: no coal production series at all, but EIA covers Iceland: a real 0.
    eia_row("ISL", "oil", "production", "crude_condensate", "TBPD", c("2025" = 0), 57, 1),
    eia_row("ISL", "gas", "production", "dry_gas", "BCM", c("2024" = 0), 26, 1),
    # Estonia: EIA coal is 0 and EIA's coal consumption a sliver of EI's (EI counts oil shale).
    eia_row("EST", "oil", "production", "crude_condensate", "TBPD", c("2025" = 0), 57, 1),
    eia_row("EST", "gas", "production", "dry_gas", "BCM", c("2024" = 0), 26, 1),
    eia_row("EST", "coal", "production", "coal", "TJ", c("2015" = 0, "2020" = 0, "2024" = 0), 7, 1),
    eia_row("EST", "coal", "consumption", "coal", "TJ", c("2015" = 300, "2020" = 100, "2024" = 80), 7, 2),
    # Testland: a gas series with no number for 2024 or 2025.
    eia_row("TST", "gas", "production", "dry_gas", "BCM", c("2023" = 1, "2024" = NA, "2025" = NA), 26, 1),
    eia_row("TST", "oil", "production", "crude_condensate", "TBPD", c("2025" = 1), 57, 1)
  )
}

test_that("EIA values are converted to EI's units", {
  units <- eia_intl_ei_units(eia_extract_fixture())
  get <- function(iso3, var, year) units$value[units$iso3 == iso3 & units$ei_var == var & units$year == year]

  # Oil: crude incl. lease condensate + NGPL, thousand b/d.
  expect_equal(get("NZL", "oilprod_kbd", 2025), 12)
  # Gas: bcm x 0.036 EJ/bcm, EI's own factor. Coal: gross TJ x 0.95 to net EJ.
  expect_equal(get("NZL", "gasprod_ej", 2024), 3.3 * 0.036)
  expect_equal(get("NLD", "coal_tes_ej", 2024), 200000 * 0.95e-6)
  # A missing year is missing, not 0.
  expect_true(is.na(get("NZL", "gasprod_ej", 2025)))
  expect_equal(eia_intl_conversions()$factor[eia_intl_conversions()$ei_var == "gasprod_ej"], 0.036)
})

test_that("the lookup takes the same year, else the latest within the lag, and reads a missing series as 0", {
  cells <- tibble::tribble(
    ~iso3, ~ei_var,       ~year,
    "NZL", "oilprod_kbd", 2025L,
    "NZL", "gasprod_ej",  2025L,
    "TST", "gasprod_ej",  2025L,
    "ISL", "coalprod_ej", 2025L,
    "KEN", "oilprod_kbd", 2025L
  )
  out <- eia_intl_lookup(cells, eia_extract_fixture(), max_lag = 1L)

  expect_equal(out$value[1], 12)
  expect_identical(out$eia_year[1], 2025L)
  # Gas 2025 is "--": the 2024 value stands in.
  expect_equal(out$value[2], 3.3 * 0.036)
  expect_identical(out$eia_year[2], 2024L)
  expect_identical(out$eia_basis[2], "series")
  # No number within one year: missing.
  expect_true(is.na(out$value[3]))
  # Iceland has EIA series, but none for coal production: no production, a real 0.
  expect_identical(out$value[4], 0)
  expect_identical(out$eia_basis[4], "no_series")
  # An economy EIA does not cover at all stays missing.
  expect_true(is.na(out$value[5]))

  # With no lag, 2025 gas has no value.
  expect_true(is.na(eia_intl_lookup(cells[2, ], eia_extract_fixture(), max_lag = 0L)$value))
  expect_error(eia_intl_lookup(cells, eia_extract_fixture(), max_lag = -1), "max_lag")
})

test_that("a value whose fuel is not EI's fails the comparability check", {
  cells <- tibble::tibble(iso3 = c("EST", "NLD", "SGP"), ei_var = "coalprod_ej", year = 2025L)
  # EI's coal consumption: Estonia 0.09 EJ (oil shale), Netherlands 0.19, Singapore 0.0126.
  out <- eia_intl_lookup(cells, eia_extract_fixture(), ei_consumption = c(0.09, 0.19, 0.0126))

  expect_true(is.na(out$value[1]))
  expect_identical(out$eia_basis[1], "not_comparable")
  expect_equal(out$eia_consumption_coverage[1], 80 * 0.95e-6 / 0.09)
  # EIA covers the Netherlands' coal: the 0 is used.
  expect_identical(out$value[2], 0)
  expect_identical(out$eia_basis[2], "series")
  # Singapore has no EIA coal consumption series: nothing to compare, the 0 is used.
  expect_identical(out$value[3], 0)
  expect_true(is.na(out$eia_consumption_coverage[3]))
  # Below the materiality floor (gap <= 0.01 EJ) a low coverage does not exclude.
  tiny <- eia_intl_lookup(cells[1, ], eia_extract_fixture(), ei_consumption = 0.005)
  expect_identical(tiny$value, 0)
  expect_error(eia_intl_lookup(cells, eia_extract_fixture(), ei_consumption = 1), "one value per row")
  expect_match(eia_intl_not_comparable_text("Coal", c(0.00084, 0.0019)), "0.084-0.19% of EI's")
})

# ---- Import dependence -------------------------------------------------------------------

ei_gap_fixture <- function() {
  rows <- tibble::tribble(
    ~Country,             ~ISO3166_alpha3, ~Var,          ~Value,
    "Norway",             "NOR",           "oil_tes_ej",  0.4,
    "Norway",             "NOR",           "oilcons_kbd", 200,
    "Norway",             "NOR",           "oilprod_kbd", 2000,
    "Norway",             "NOR",           "gas_tes_ej",  0.2,
    "Norway",             "NOR",           "gasprod_ej",  4,
    "Norway",             "NOR",           "coal_tes_ej", 0.03,
    "Norway",             "NOR",           "coalprod_ej", 0.01,
    "Netherlands",        "NLD",           "oil_tes_ej",  1.8,
    "Netherlands",        "NLD",           "oilcons_kbd", 900,
    "Netherlands",        "NLD",           "oilprod_kbd", 20,
    "Netherlands",        "NLD",           "gas_tes_ej",  1.1,
    "Netherlands",        "NLD",           "gasprod_ej",  0.4,
    "Netherlands",        "NLD",           "coal_tes_ej", 0.19,
    "New Zealand",        "NZL",           "oil_tes_ej",  0.32,
    "New Zealand",        "NZL",           "oilcons_kbd", 160,
    "New Zealand",        "NZL",           "gas_tes_ej",  0.1,
    "New Zealand",        "NZL",           "coal_tes_ej", 0.04,
    "New Zealand",        "NZL",           "coalprod_ej", 0.07,
    "Spain",              "ESP",           "oil_tes_ej",  2.5,
    "Spain",              "ESP",           "oilcons_kbd", 1250,
    "Spain",              "ESP",           "gas_tes_ej",  1.2,
    "Spain",              "ESP",           "coal_tes_ej", 0.1,
    "Spain",              "ESP",           "coalprod_ej", 0,
    "Singapore",          "SGP",           "oil_tes_ej",  3,
    "Singapore",          "SGP",           "oilcons_kbd", 1500,
    "Singapore",          "SGP",           "gas_tes_ej",  0.4,
    "Singapore",          "SGP",           "coal_tes_ej", 0.0126,
    "Estonia",            "EST",           "oil_tes_ej",  0.05,
    "Estonia",            "EST",           "oilcons_kbd", 25,
    "Estonia",            "EST",           "gas_tes_ej",  0.011,
    "Estonia",            "EST",           "coal_tes_ej", 0.09,
    "Testland",           "TST",           "oil_tes_ej",  0.1,
    "Testland",           "TST",           "oilcons_kbd", 50,
    "Testland",           "TST",           "gas_tes_ej",  0.05,
    "Other Asia Pacific", NA,              "oil_tes_ej",  1,
    "Other Asia Pacific", NA,              "oilcons_kbd", 500,
    "Other Asia Pacific", NA,              "gasprod_ej",  0.7
  )
  dplyr::bind_rows(
    dplyr::mutate(rows, Year = 2025L, .after = Country),
    # EI's 2024 gas consumption of New Zealand, the year of its EIA gas production: the 2025
    # self-sufficiency pairs the two (import_dependence_pair_years()).
    tibble::tibble(Country = "New Zealand", Year = 2024L, ISO3166_alpha3 = "NZL", Var = "gas_tes_ej", Value = 0.12)
  )
}

test_that("import dependence takes EIA production only where EI has no row, and flags it", {
  cleaned <- import_dependence_clean_raw(ei_gap_fixture(), year = 2025, eia = eia_extract_fixture())
  value <- function(country, col) cleaned[[col]][cleaned$Country == country]

  # EI wins wherever it reports, a reported 0 included.
  expect_identical(value("Netherlands", "oilprod_kbd"), 20)
  expect_identical(value("Spain", "coalprod_ej"), 0)
  expect_true(is.na(value("Netherlands", "oilprod_kbd_eia_year")))
  # Gaps take EIA: New Zealand oil (2025) and gas (2024, EIA's latest), Spain oil. The 2024 gas
  # is set against EI's 2024 consumption (0.12 EJ), and carried to 2025 consumption (0.1 EJ).
  expect_equal(value("New Zealand", "oilprod_kbd"), 12)
  expect_equal(value("New Zealand", "gasprod_ej_pair_production"), 3.3 * 0.036)
  expect_equal(value("New Zealand", "gasprod_ej"), 3.3 * 0.036 * 0.1 / 0.12)
  expect_identical(value("New Zealand", "gasprod_ej_eia_year"), 2024L)
  expect_equal(value("Spain", "oilprod_kbd"), 2)
  # EIA zeros are real zeros.
  expect_identical(value("Singapore", "oilprod_kbd"), 0)
  expect_identical(value("Netherlands", "coalprod_ej"), 0)
  # Not comparable (Estonia coal) and no EIA value (Testland gas) stay missing.
  expect_true(is.na(value("Estonia", "coalprod_ej")))
  expect_identical(value("Estonia", "coalprod_ej_eia_basis"), "not_comparable")
  expect_true(is.na(value("Testland", "gasprod_ej")))
  expect_true(is.na(value("Testland", "gasprod_ej_eia_basis")))
})

test_that("an EIA value is observed data with its source, and the remaining gaps say why", {
  out <- withr::with_options(
    list(opportunity_security.index_definition = yaml::read_yaml(file.path(repo_root, "config", "index_definition.yml"))),
    import_dependence(ei_gap_fixture(), year = 2025, eia = eia_extract_fixture())
  )
  cell <- function(country, fuel, variable, data_type = "raw") {
    out[out$Country == country & out$tech == fuel & out$variable == variable & out$data_type == data_type, ]
  }

  nz <- cell("New Zealand", "Gas", "Self-sufficiency")
  # EIA's 2024 production against EI's 2024 consumption, not EI's 2025 (0.1 EJ).
  expect_equal(nz$value, 100 * 3.3 * 0.036 / 0.12)
  expect_match(nz$source, "; EIA International Energy Statistics$")
  expect_match(nz$explanation, "EI does not report gas production for this economy separately; EIA International Energy Statistics \\(2024, the latest EIA year, for 2025\\): dry natural gas, bcm x 0.036 EJ/bcm")
  expect_match(nz$explanation, "Production is EIA's 2024 value and consumption EI's 2025, so self-sufficiency pairs them in 2024: 0.1188 EJ produced against EI's 2024 consumption of 0.12 EJ \\(99.0%\\)", fixed = FALSE)
  expect_false(is.na(cell("New Zealand", "Gas", "Production surplus/deficit", "index")$value))
  # Unchanged for EI-reported cells.
  expect_false(grepl("EIA", cell("Norway", "Oil", "Self-sufficiency")$source))
  expect_identical(cell("Spain", "Coal", "Self-sufficiency")$value, 0)
  # Singapore oil: 0% self-sufficient, observed (formerly NA, global average).
  expect_identical(cell("Singapore", "Oil", "Self-sufficiency")$value, 0)
  expect_false(is.na(cell("Singapore", "Oil", "Production surplus/deficit", "index")$value))

  est <- cell("Estonia", "Coal", "Self-sufficiency")
  expect_true(is.na(est$value))
  expect_match(est$explanation, "value is not used: EIA's coal consumption is .*% of EI's")
  expect_false(grepl("EIA International Energy Statistics$", est$source))
  expect_match(cell("Testland", "Gas", "Self-sufficiency")$explanation, "EIA International Energy Statistics give no value")
  expect_false(any(is.nan(out$value)))

  # Without the extract the theme is the NA version of 2026-10-11 (no EIA anywhere).
  plain <- import_dependence(ei_gap_fixture(), year = 2025)
  expect_true(is.na(plain$value[plain$Country == "New Zealand" & plain$tech == "Gas" & plain$variable == "Self-sufficiency"]))
  expect_false(any(grepl("EIA", plain$source)))
})

test_that("production and consumption of different years are set against each other in one year", {
  # Slovenia-like coal: EIA has 2024 production only; EI's consumption fell 39% in 2025. Dividing
  # EIA 2024 by EI 2025 read 121% self-sufficient and a positive balance (HEAD of 2026-10-11).
  ei <- dplyr::bind_rows(
    ei_gap_fixture(),
    tibble::tribble(
      ~Country,   ~Year, ~ISO3166_alpha3, ~Var,          ~Value,
      "Slovenia", 2025L, "SVN",           "oil_tes_ej",  0.2,
      "Slovenia", 2025L, "SVN",           "oilcons_kbd", 100,
      "Slovenia", 2025L, "SVN",           "gas_tes_ej",  0.03,
      "Slovenia", 2025L, "SVN",           "coal_tes_ej", 0.0197,
      "Slovenia", 2024L, "SVN",           "coal_tes_ej", 0.0325
    )
  )
  base <- eia_extract_fixture()
  eia <- dplyr::bind_rows(
    # Testland: EIA 2024 gas production, but EI has no consumption row of 2024: left unpaired.
    base[base$series_id != "INTL.26-1-TST-BCM.A", ],
    eia_row("TST", "gas", "production", "dry_gas", "BCM", c("2024" = 0.5), 26, 1),
    eia_row("SVN", "coal", "production", "coal", "TJ", c("2024" = 25200), 7, 1),
    eia_row("SVN", "coal", "consumption", "coal", "TJ", c("2024" = 33000), 7, 2),
    eia_row("SVN", "oil", "production", "crude_condensate", "TBPD", c("2025" = 0), 57, 1),
    eia_row("SVN", "gas", "production", "dry_gas", "BCM", c("2024" = 0), 26, 1)
  )
  cleaned <- import_dependence_clean_raw(ei, year = 2025, eia = eia)
  built <- import_dependence_build_imports(cleaned)
  svn <- built[built$Country == "Slovenia", ]
  p2024 <- 25200 * 0.95e-6

  # The self-sufficiency is 2024's: EIA 2024 production over EI 2024 consumption (73.7%).
  expect_identical(svn$coalprod_ej_production_year, 2024L)
  expect_identical(svn$coalprod_ej_consumption_year, 2025L)
  expect_identical(svn$coalprod_ej_pair_year, 2024L)
  expect_equal(svn$coalprod_ej_pair_production, p2024)
  expect_equal(svn$coalprod_ej_pair_consumption, 0.0325)
  expect_equal(svn$coal_self_sufficiency, 100 * p2024 / 0.0325)
  # The balance applies it to the 2025 consumption: production is EIA's 2024 value scaled by EI's
  # consumption change, so a net importer stays a net importer.
  expect_equal(svn$coalprod_ej, p2024 * 0.0197 / 0.0325)
  expect_equal(svn$coal_surplus_ej, 0.0197 * (p2024 / 0.0325 - 1))
  expect_lt(svn$coal_surplus_ej, 0)
  # Fossil adds the paired production (gas and oil are EIA zeros) to 2025 consumption.
  expect_equal(svn$fossilprod_ej, svn$coalprod_ej)
  expect_equal(svn$fossil_self_sufficiency, 100 * svn$coalprod_ej / (0.2 + 0.03 + 0.0197))
  # Oil (EIA 2025 against EI 2025) and New Zealand oil are of one year: untouched.
  expect_true(is.na(svn$oilprod_kbd_pair_year))
  expect_identical(svn$oilprod_kbd, 0)
  nz <- built[built$Country == "New Zealand", ]
  expect_true(is.na(nz$oilprod_kbd_pair_year))
  expect_equal(nz$oilprod_kbd, 12)
  # EIA zeros of an earlier year stay 0 (0 in any year): Singapore and Netherlands, 2024.
  expect_identical(built$gasprod_ej[built$Country == "Singapore"], 0)
  expect_true(is.na(built$gasprod_ej_pair_year[built$Country == "Singapore"]))
  expect_identical(built$coalprod_ej[built$Country == "Netherlands"], 0)
  # No EI consumption of 2024: compared as they are, and flagged.
  tst <- built[built$Country == "Testland", ]
  expect_equal(tst$gasprod_ej, 0.5 * 0.036)
  expect_true(tst$gasprod_ej_unpaired)
  expect_true(is.na(tst$gasprod_ej_pair_year))
  expect_false(any(built$coalprod_ej_unpaired[built$Country != "Testland"]))

  # balance = consumption x (self-sufficiency - 1) for every fuel, paired or not.
  for (f in c("oil", "gas", "coal", "fossil")) {
    cons <- built[[paste0(f, "_tes_ej")]]
    ok <- !is.na(built[[paste0(f, "_self_sufficiency")]])
    expect_equal(built[[paste0(f, "_surplus_ej")]][ok], (cons * (built[[paste0(f, "_self_sufficiency")]] / 100 - 1))[ok], info = f)
  }

  out <- import_dependence_build_tidy(built, year = 2025)
  ex <- function(country, fuel) out$explanation[out$Country == country & out$tech == fuel & out$variable == "Self-sufficiency"]
  expect_match(ex("Slovenia", "Coal"), "Production is EIA's 2024 value and consumption EI's 2025, so self-sufficiency pairs them in 2024: 0.02394 EJ produced against EI's 2024 consumption of 0.0325 EJ \\(73.7%\\)")
  expect_match(ex("Slovenia", "Coal"), "The balance applies that share to the 2025 consumption of 0.0197 EJ \\(production taken as the 2024 value x 0.6062, the change in EI's coal consumption 2024-2025\\)")
  expect_match(ex("Slovenia", "Fossil"), "Each fuel's production enters with its consumption of the same year")
  expect_match(ex("Testland", "Gas"), "Production is EIA's 2024 value and consumption EI's 2025, and no value of the earlier year is available")
  expect_false(grepl("pairs them", ex("Norway", "Gas")))
  expect_false(grepl("same year", ex("Norway", "Fossil")))
})

test_that("the consumption fill puts producers without an EI consumption row on the roster", {
  ei <- dplyr::bind_rows(
    ei_gap_fixture(),
    tibble::tibble(Country = "Angola", Year = 2025L, ISO3166_alpha3 = "AGO", Var = "oilprod_kbd", Value = 1100),
    # EI's 2024 oil production, the year of the EIA consumption: not used, since EI's 2025
    # production is observed (consumption of an earlier year is not paired).
    tibble::tibble(Country = "Angola", Year = 2024L, ISO3166_alpha3 = "AGO", Var = "oilprod_kbd", Value = 1150)
  )
  eia <- dplyr::bind_rows(
    eia_extract_fixture(),
    eia_row("AGO", "oil", "consumption", "petroleum", "TBPD", c("2024" = 130), 5, 2),
    eia_row("AGO", "oil", "consumption", "petroleum", "TJ", c("2024" = 273000), 5, 2),
    eia_row("AGO", "gas", "consumption", "dry_gas", "BCM", c("2024" = 1.2), 26, 2),
    eia_row("AGO", "coal", "consumption", "coal", "TJ", c("2024" = 0), 7, 2),
    eia_row("AGO", "gas", "production", "dry_gas", "BCM", c("2024" = 5.8), 26, 1)
  )
  default <- import_dependence_clean_raw(ei, year = 2025, eia = eia)
  expect_false("Angola" %in% default$Country)

  proposed <- import_dependence_build_imports(import_dependence_clean_raw(ei, year = 2025, eia = eia, eia_fill_consumption = TRUE))
  ago <- proposed[proposed$Country == "Angola", ]
  expect_equal(ago$oil_tes_ej, 273000 * 0.95e-6)
  expect_identical(ago$consumption_source, "EIA")
  expect_identical(ago$oil_production_source, "EI")
  expect_identical(ago$gas_production_source, "EIA")
  # Consumption is EIA's 2024 (its latest), carried to 2025; production stays EI's observed 2025
  # value (1100, not 2024's 1150): consumption of an earlier year is not paired.
  expect_true(is.na(ago$oilprod_kbd_pair_year))
  expect_false(ago$oilprod_kbd_unpaired)
  expect_identical(ago$oilprod_kbd_production_year, 2025L)
  expect_identical(ago$oilprod_kbd_consumption_year, 2024L)
  expect_equal(ago$oilprod_kbd, 1100)
  expect_equal(ago$oil_self_sufficiency, 100 * 1100 / 130)
  expect_equal(ago$oil_surplus_ej, ago$oil_tes_ej * (1100 / 130 - 1))
  # Gas production and consumption are both EIA 2024: nothing to pair.
  expect_true(is.na(ago$gasprod_ej_pair_year))
  expect_equal(ago$gas_self_sufficiency, 100 * 5.8 / 1.2)
  tidy <- import_dependence_build_tidy(proposed, year = 2025)
  ex <- tidy$explanation[tidy$Country == "Angola" & tidy$tech == "Oil" & tidy$variable == "Self-sufficiency"]
  expect_match(ex, "EIA International Energy Statistics \\(2024, the latest EIA year, for 2025\\): petroleum and other liquids consumption")
  expect_false(grepl("pairs them|pair them", ex))
})

# ---- Production depth + momentum ---------------------------------------------------------

ei_production_fixture <- function() {
  grid <- tidyr::expand_grid(
    tibble::tibble(Country = c("Norway", "New Zealand", "Estonia"), ISO3166_alpha3 = c("NOR", "NZL", "EST")),
    Year = c(2015L, 2020L, 2025L)
  )
  dplyr::bind_rows(
    dplyr::mutate(grid[grid$Country == "Norway", ], Var = "oilprod_kbd", Value = c(1900, 2000, 2000)),
    dplyr::mutate(grid[grid$Country == "Norway", ], Var = "gasprod_ej", Value = c(4, 4.2, 4.4)),
    dplyr::mutate(grid[grid$Country == "Norway", ], Var = "electbyfuel_gas", Value = c(3, 3, 3)),
    dplyr::mutate(grid[grid$Country == "New Zealand", ], Var = "coalprod_ej", Value = c(0.1, 0.08, 0.07)),
    dplyr::mutate(grid[grid$Country == "New Zealand", ], Var = "solar_twh", Value = c(0.1, 0.2, 0.5)),
    dplyr::mutate(grid[grid$Country == "Estonia", ], Var = "coal_tes_ej", Value = c(0.15, 0.1, 0.09)),
    dplyr::mutate(grid[grid$Country == "Estonia", ], Var = "wind_twh", Value = c(0.7, 0.8, 1.5))
  )
}

test_that("the production theme fills absent EI fuel production from EIA and leaves generation at 0", {
  eia <- eia_extract_fixture()
  out <- production_depth_momentum_clean_energy_production(ei_production_fixture(), eia = eia)
  get <- function(country, var, year) out[[as.character(year)]][out$Country == country & out$Var == var]

  # EI values stand.
  expect_identical(get("Norway", "Oil Upstream", 2025), 2000)
  # New Zealand oil and gas from EIA; gas 2025 from EIA 2024.
  expect_equal(get("New Zealand", "Oil Upstream", 2015), 36)
  expect_equal(get("New Zealand", "Oil Upstream", 2025), 12)
  expect_equal(get("New Zealand", "Gas Upstream", 2025), 3.3 * 0.036)
  nz_gas <- out[out$Country == "New Zealand" & out$Var == "Gas Upstream", ]
  expect_true(nz_gas$eia_used)
  expect_match(nz_gas$eia_note, "EIA International Energy Statistics for 2015, 2020, 2025 \\(EIA 2024, its latest year\\)")
  # Estonia: EIA's 0 coal is not EI's coal (oil shale), so NA; Estonia gas is a real 0.
  expect_true(all(is.na(unlist(out[out$Country == "Estonia" & out$Var == "Coal Upstream", c("2015", "2020", "2025")]))))
  est_coal <- out[out$Country == "Estonia" & out$Var == "Coal Upstream", ]
  expect_false(est_coal$eia_used)
  expect_match(est_coal$eia_note, "NA in 2015, 2020, 2025: the EIA International Energy Statistics value is not used")
  expect_identical(get("Estonia", "Gas Upstream", 2025), 0)
  # Norway has no EIA rows in the fixture, so its absent coal stays NA; generation EI does not
  # report is still 0 (EIA is not used for generation).
  expect_true(is.na(get("Norway", "Coal Upstream", 2025)))
  expect_identical(get("New Zealand", "Gas Downstream", 2025), 0)
  expect_true(is.na(out$eia_note[out$Country == "Norway" & out$Var == "Oil Upstream"]))

  # Without the extract every absent cell is 0, as before.
  plain <- production_depth_momentum_clean_energy_production(ei_production_fixture())
  expect_identical(plain$`2025`[plain$Country == "New Zealand" & plain$Var == "Gas Upstream"], 0)
  expect_false("eia_note" %in% names(plain))
})

test_that("production rows name EIA in their source only where EIA supplied a value", {
  clean <- production_depth_momentum_clean_energy_production(ei_production_fixture(), eia = eia_extract_fixture())
  growth <- production_depth_momentum_build_energy_indices(clean, latest_year = 2025)
  combined <- dplyr::transmute(
    growth, Country, tech, supply_chain, production_latest = `2025`, raw_year = 2025L, size_index,
    growth_abs_index, overall_production_index, block = "energy", source = "EI", eia_note, eia_used
  )
  tbl <- production_depth_momentum_build_table(combined, year = 2025)
  src <- function(country, tech) unique(tbl$source[tbl$Country == country & tbl$tech == tech & tbl$supply_chain == "Upstream"])

  expect_identical(src("New Zealand", "Gas"), "EI; EIA International Energy Statistics")
  expect_identical(src("Norway", "Oil"), "EI")
  expect_identical(src("Estonia", "Coal"), "EI")
  est <- tbl[tbl$Country == "Estonia" & tbl$tech == "Coal" & tbl$supply_chain == "Upstream", ]
  expect_true(all(is.na(est$value)))
  expect_false(any(is.nan(tbl$value)))
})

test_that("the Sector Insights exporter ships a fuel production NA as nsr, not an NA status", {
  # Estonia coal is NA in production_depth_momentum_tbl (no EI row, EIA value not comparable);
  # sector_obs_tables() stops on an NA status, so scripts/87 would fail on it.
  env <- new.env(parent = globalenv())
  sys.source(file.path(repo_root, "R", "dashboard", "sector", "sector_files.R"), envir = env)
  for (f in env$sector_files()) sys.source(file.path(repo_root, f), envir = env)
  pdm <- tibble::tibble(
    iso3c = c("EST", "EST", "NZL"), tech = c("Coal", "Gas", "Gas"), supply_chain = "Upstream",
    variable = "production_latest", value = c(NA, 0, 0.12), data_type = "raw"
  )
  x <- env$sector_facts_production(pdm, tibble::tibble(ei_var = character(), iso3 = character()))
  est <- x$facts[x$facts$geo == "EST" & x$facts$item_key == "fuel:coal", ]
  expect_identical(est$status, "nsr")
  expect_true(is.nan(est$value))
  expect_false(anyNA(x$facts$status))
  expect_false("EST fuel:coal" %in% paste(x$ledger$geo, x$ledger$item_key))
})

# ---- Fetcher and scripts/10 --------------------------------------------------------------

test_that("the fetcher reads INTL.txt from the downloaded zip as data and cleans up", {
  skip_if_not_installed("zip")
  source(file.path(repo_root, "scripts", "utils", "fetchers.R"), local = FALSE)
  source(file.path(repo_root, "scripts", "fetchers", "eia_intl.R"), local = FALSE)
  expect_false(is.null(get_fetcher("eia_intl_fossil_production")))

  src <- withr::local_tempdir()
  writeLines(eia_bulk_lines(), file.path(src, "INTL.txt"))
  zip_path <- file.path(withr::local_tempdir(), "INTL.zip")
  zip::zipr(zip_path, file.path(src, "INTL.txt"))
  bytes <- readBin(zip_path, "raw", file.info(zip_path)$size)

  # The HTTP layer returns the zip's bytes; nothing touches the network.
  original <- get("opsi_http_get", envir = globalenv())
  assign("opsi_http_get", function(url, ...) bytes, envir = globalenv())
  withr::defer(assign("opsi_http_get", original, envir = globalenv()))

  work_dir <- file.path(withr::local_tempdir(), "eia_dl")
  out <- eia_intl_fetch(url = "https://example.invalid/INTL.zip", work_dir = work_dir)
  expect_false(dir.exists(work_dir))
  expect_true(all(out$year >= EIA_INTL_MIN_YEAR))
  expect_setequal(unique(out$series_id), c("INTL.26-1-NZL-BCM.A", "INTL.57-1-KOR-TBPD.A", "INTL.7-1-EST-TJ.A", "INTL.7-2-EST-TJ.A"))
  expect_true("fetched_on" %in% names(out))

  # The download goes only into a new directory, and an existing one is left as it was.
  existing <- withr::local_tempdir()
  writeLines("keep", file.path(existing, "keep.txt"))
  expect_error(eia_intl_download(existing, url = "https://example.invalid/INTL.zip"), "already exists")
  expect_error(eia_intl_fetch(url = "https://example.invalid/INTL.zip", work_dir = existing), "already exists")
  expect_true(file.exists(file.path(existing, "keep.txt")))

  # Written as scripts/10 reads it: a missing value is an empty cell, EIA's "NA" code a string.
  csv <- file.path(withr::local_tempdir(), "eia.csv")
  utils::write.csv(out, csv, row.names = FALSE, na = "")
  back <- read.csv(csv, stringsAsFactors = FALSE, na.strings = "")
  expect_true(is.na(back$value[back$series_id == "INTL.57-1-KOR-TBPD.A" & back$year == 2024]))
  expect_identical(back$value_note[back$series_id == "INTL.57-1-KOR-TBPD.A" & back$year == 2024], "NA")
})

test_that("the staged extract meets the fetcher contract", {
  path <- file.path(repo_root, "data", "raw", "eia_intl_fossil_production.csv")
  skip_if_not(file.exists(path), "EIA extract not staged")
  source(file.path(repo_root, "scripts", "utils", "fetchers.R"), local = FALSE)
  source(file.path(repo_root, "scripts", "fetchers", "eia_intl.R"), local = FALSE)
  staged <- read.csv(path, stringsAsFactors = FALSE, na.strings = "")
  expect_null(opsi_validate_contract(staged, get_fetcher("eia_intl_fossil_production")$contract, "staged"))
  expect_silent(eia_intl_check_extract(staged))
})

test_that("scripts/10 requires the extract and says how to stage it", {
  text <- paste(readLines(file.path(repo_root, "scripts", "10_build_themes.R"), warn = FALSE), collapse = "\n")
  block <- stringr::str_match(text, "(?s)missing_files <- c\\((.*?)\\)\\s*\\n")[1, 2]
  expect_true("eia_intl_path" %in% trimws(strsplit(block, ",")[[1]]))
  expect_match(text, "Missing the EIA International Energy Statistics extract", fixed = TRUE)
  expect_match(text, "Rscript scripts/fetchers/eia_intl.R", fixed = TRUE)
  expect_match(text, "import_dependence(ei, eia = eia_intl, eia_fill_consumption = TRUE)", fixed = TRUE)
})
