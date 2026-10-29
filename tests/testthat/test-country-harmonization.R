# One canonical name and ISO3 code per economy. The 2026-09 index tables carried 272 Country
# strings for 237 economies ("Viet Nam"/"Vietnam", "Slovak Republic"/"Slovakia", three DR
# Congo spellings, ...) plus pseudo-countries ("Global", "USSR"), each ranked and imputed as a
# separate country.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "theme_standardize.R"))

theme_row <- function(Country, value, iso3c = NA_character_, variable = "x", source = "src",
                      tech = "Solar", Year = 2024L) {
  tibble::tibble(
    Country = Country,
    iso3c = iso3c,
    tech = tech,
    supply_chain = "Extraction",
    category = "Trade",
    variable = variable,
    data_type = "index",
    value = value,
    Year = Year,
    source = source,
    explanation = "test"
  )
}

test_country_info <- function() {
  standardize_country_info(tibble::tibble(
    iso3c = c("VNM", "SVK", "KOR", "COD", "USA", "RUS", "CYP", "WLD"),
    country = c(
      "Viet Nam", "Slovak Republic", "Korea, Rep.", "Congo, Dem. Rep.", "United States",
      "Russian Federation", "Cyprus", "World"
    ),
    region = c(rep("Somewhere", 7), "Aggregates")
  ))
}

test_that("every spelling of an economy maps to its one canonical name", {
  cases <- c(
    "Vietnam" = "Viet Nam",
    "Viet Nam" = "Viet Nam",
    "Slovakia" = "Slovak Republic",
    "Taiwan, Province of China" = "Taiwan",
    "Chinese Taipei" = "Taiwan",
    "China Hong Kong SAR" = "Hong Kong",
    "Hong Kong (China)" = "Hong Kong",
    "Macao" = "Macau",
    "Congo, Democratic Republic of the" = "Democratic Republic of Congo",
    "Democratic Republic of the Congo" = "Democratic Republic of Congo",
    "Republic of the Congo" = "Congo",
    "Korea" = "South Korea",
    "Korea, Rep." = "South Korea",
    "Republic of Korea" = "South Korea",
    "Dem. Rep. Korea" = "North Korea",
    "Turkey" = "Turkiye",
    "Türkiye" = "Turkiye",
    "Republic of Turkiye" = "Turkiye",
    "Syria" = "Syrian Arab Republic",
    "Brunei" = "Brunei Darussalam",
    "Côte d'Ivoire" = "Cote d'Ivoire",
    "Tanzania, United Republic of" = "Tanzania",
    "Kyrgyzstan" = "Kyrgyz Republic",
    "Laos" = "Lao PDR",
    "Trinidad & Tobago" = "Trinidad and Tobago",
    "Saint Lucia" = "St. Lucia",
    "Palestine" = "West Bank and Gaza",
    "China (Mainland)" = "China",
    "Russian Federation" = "Russia",
    "Tahiti" = "French Polynesia",
    "US" = "United States",
    # Comtrade and IMF spellings countrycode misses.
    "Br. Virgin Isds" = "British Virgin Islands",
    "FS Micronesia" = "Micronesia",
    "Kosovo, Republic of" = "Kosovo",
    "Hong Kong Special Administrative Region, People's Republic of China" = "Hong Kong",
    "Macao Special Administrative Region, People's Republic of China" = "Macau",
    # Present-day economies whose old names carry words the aggregate rules look for.
    "Comoros, Union of the" = "Comoros",
    "The former Yugoslav Republic of Macedonia" = "North Macedonia",
    "Congo, Dem. Rep. (ex-Zaire)" = "Democratic Republic of Congo"
  )

  expect_equal(standardize_country_names(names(cases)), unname(cases))
})

test_that("standardize_country_names keeps its contract", {
  expect_null(standardize_country_names(NULL))
  expect_identical(standardize_country_names(character()), character())

  input <- c(" Viet  Nam ", NA, "", "Bigland", "^ less than 0.05", "Brazil1")
  out <- standardize_country_names(input)
  expect_type(out, "character")
  expect_length(out, length(input))
  expect_equal(out, c("Viet Nam", NA, "", "Bigland", "^ less than 0.05", "Brazil"))

  # Factors come back as character, as before.
  expect_equal(standardize_country_names(factor(c("Slovakia", "Slovakia"))), rep("Slovak Republic", 2))
})

test_that("aggregates, residual rows and historical entities keep their names and get no code", {
  non_countries <- c(
    "World", "Global", "Total World", "European Union", "EU", "OECD", "Other Asia Pacific",
    "Rest of World", "Total Non-OECD", "Sub-Saharan Africa", "Middle East & North Africa",
    "Areas, nes", "USSR", "Soviet Union", "Netherlands Antilles", "Northern Cyprus",
    "Sub-Saharan Africa excluding South Africa", "Asia excl. China", "China Region",
    "Upper middle income", "Middle East (developing only)",
    # countrycode reads each of these as a single country.
    "Greater China", "Asia Pacific (ex China)", "Sudan (former)", "Ethiopia (former)",
    "Euro Area (EA)", "West African Economic and Monetary Union (WAEMU)"
  )

  expect_equal(standardize_country_names(non_countries), non_countries)
  expect_true(all(is.na(country_name_to_iso3(non_countries))))
  expect_true(all(is_aggregate_or_residual(non_countries)))
})

test_that("canonical names are never flagged as aggregates", {
  canonical <- unname(country_canonical_names())
  expect_false(any(is_aggregate_or_residual(canonical)))
  expect_false(any(is_aggregate_or_residual(c("South Africa", "Central African Republic", "American Samoa"))))

  # WDI aggregate codes are flagged even when the name is unfamiliar.
  expect_equal(is_aggregate_or_residual(c("Some bloc", "Japan"), iso3c = c("EUU", "JPN")), c(TRUE, FALSE))
})

test_that("names and ISO3 codes are one-to-one, and the hard cases resolve", {
  canonical <- country_canonical_names()
  expect_equal(anyDuplicated(names(canonical)), 0)
  expect_equal(anyDuplicated(unname(canonical)), 0)

  expect_equal(
    country_name_to_iso3(c("Taiwan", "Taiwan, Province of China", "Kosovo", "Micronesia", "Channel Islands", "USSR")),
    c("TWN", "TWN", "XKX", "FSM", "CHI", NA)
  )
  expect_equal(country_name_from_iso3(c("TWN", "VNM", "KOR", NA)), c("Taiwan", "Viet Nam", "South Korea", NA))
  # Territories outside the WDI roster still get a name.
  expect_false(is.na(country_name_from_iso3("AIA")))
})

test_that("the canonical table matches the WDI roster the pipeline standardizes", {
  wdi_path <- file.path(repo_root, "data", "raw", "wdi_country_info.csv")
  skip_if_not(file.exists(wdi_path), "wdi_country_info.csv not staged")

  wdi <- standardize_country_info(utils::read.csv(wdi_path, encoding = "UTF-8"))
  canonical <- country_canonical_names()

  expect_setequal(setdiff(names(canonical), wdi$iso3c), "TWN")
  expect_equal(unname(canonical[wdi$iso3c]), wdi$country)

  # Against the raw WDI names after the curated recodes only: standardize_country_info()
  # itself resolves through the table, so a WDI rename would otherwise pass unnoticed.
  raw <- utils::read.csv(wdi_path, encoding = "UTF-8")
  raw <- raw[raw$region != "Aggregates" & nzchar(raw$iso3c), ]
  expect_equal(unname(canonical[raw$iso3c]), country_clean_names(raw$country))
})

test_that("standardize_country_table gives every coded row its canonical name", {
  tbl <- tibble::tibble(
    Country = c("Vietnam", "Taiwan, Province of China", "USSR", "Northern Cyprus", "Slovakia", "Other Asia Pacific"),
    iso3c = c(NA, NA, "RUS", "CYP", "SVK", NA),
    value = 1:6
  )

  out <- standardize_country_table(tbl, country_info = test_country_info())

  # USSR and Northern Cyprus lose the codes an earlier step tagged them with, so they are not
  # read as Russia and Cyprus; the residual row has no code either.
  expect_equal(out$Country, c("Slovak Republic", "Viet Nam", "Taiwan"))
  expect_equal(out$iso3c, c("SVK", "VNM", "TWN"))
})

test_that("standardize_country_table keeps the groups it was handed", {
  tbl <- dplyr::group_by(
    tibble::tibble(Country = c("Vietnam", "Slovakia"), tech = c("Solar", "Wind"), value = 1:2),
    tech
  )
  out <- standardize_country_table(tbl, country_info = test_country_info())
  expect_equal(dplyr::group_vars(out), "tech")
  expect_setequal(out$Country, c("Viet Nam", "Slovak Republic"))
})

test_that("standardize_theme_types drops aggregates and collapses merged spellings", {
  tbl <- dplyr::bind_rows(
    # Two spellings of one cell: the NA spelling gives way to the one with a value.
    theme_row("Vietnam", NA_real_),
    theme_row("Viet Nam", 0.4),
    # Two spellings with different values: the canonical spelling wins, with a warning.
    theme_row("Slovakia", 0.9, variable = "y"),
    theme_row("Slovak Republic", 0.2, variable = "y"),
    # Within one spelling, two sources for one cell are both kept.
    theme_row("United States", 0.5, source = "A"),
    theme_row("United States", 0.6, source = "B"),
    # Aggregates, residuals and historical entities go, even when coded.
    theme_row("World", 0.5, iso3c = "WLD"),
    theme_row("Other Asia Pacific", 0.5),
    theme_row("USSR", 0.5, iso3c = "RUS")
  )

  expect_warning(
    out <- suppressMessages(standardize_theme_types(tbl, country_info = test_country_info())),
    "different non-NA values"
  )

  out <- dplyr::arrange(out, Country, variable, source)
  expect_equal(out$Country, c("Slovak Republic", "United States", "United States", "Viet Nam"))
  expect_equal(out$value, c(0.2, 0.5, 0.6, 0.4))
  expect_equal(out$iso3c, c("SVK", "USA", "USA", "VNM"))
  expect_false(".country_input" %in% names(out))
})

test_that("standardize_theme_types without country_info harmonises names too", {
  # This path rebuilds overall rows when an index definition is loaded; an earlier test file
  # (bootstrap) may have set one globally, and the rebuild is not what is tested here.
  old <- options(opportunity_security.index_definition = NULL)
  on.exit(options(old), add = TRUE)

  tbl <- dplyr::bind_rows(theme_row("Vietnam", 0.3), theme_row("Taiwan, Province of China", 0.7), theme_row("Global", 0.1))
  out <- suppressMessages(standardize_theme_types(tbl))
  expect_setequal(out$Country, c("Viet Nam", "Taiwan"))
  # Codes come from the names when there is no WDI roster to join.
  expect_equal(out$iso3c[match(c("Viet Nam", "Taiwan"), out$Country)], c("VNM", "TWN"))
})

test_that("a table of technology-level Global rows passes through for its consumer to broadcast", {
  trl <- dplyr::bind_rows(theme_row("Global", 0.6), theme_row("Global", 0.5, tech = "Wind"))
  out <- suppressWarnings(standardize_theme_types(trl, country_info = test_country_info()))
  expect_equal(nrow(out), 2)
  expect_equal(unique(out$Country), "Global")
})

test_that("assert_country_iso3_consistent reports names and codes that are not one-to-one", {
  ok <- tibble::tibble(Country = c("Viet Nam", "Viet Nam", "Japan"), iso3c = c("VNM", "VNM", "JPN"))
  expect_silent(assert_country_iso3_consistent(ok))

  bad <- tibble::tibble(Country = c("Viet Nam", "Vietnam", "Japan"), iso3c = c("VNM", "VNM", "JPN"))
  expect_warning(assert_country_iso3_consistent(bad, label = "toy"), "not one-to-one in toy")
  expect_error(assert_country_iso3_consistent(bad, action = "error"), "Vietnam = VNM")
})
