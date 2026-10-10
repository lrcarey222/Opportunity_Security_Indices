# EV Midstream (IEA Global EV Outlook regions) in Foreign Dependency, ES and EO.
#
# Guards two defects. The regional expansion sent the whole EI "Europe" sub-region to a
# placeholder ("boop") that matched nothing, so the United Kingdom, Norway, Turkiye and the
# rest of non-EU Europe had no EV Midstream rows, although the IEA counts them in Rest of
# World. And the extract's "import_share" column, published and scored under that name, is
# (production - sales) / sales: a net export ratio, positive for exporters.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "categories", "foreign_dependency", "foreign_dependency.R"))
source(file.path(repo_root, "R", "categories", "foreign_dependency", "market_share_manufacturing.R"))

# foreign_dependency_ei_countries() shape: one row per economy, EI EU flag and sub-region.
ev_ei_fixture <- function() {
  tibble::tibble(
    Country = c(
      "China", "Japan", "Germany", "France", "United Kingdom", "Norway", "Turkiye",
      "United States", "Mexico", "Brazil", "Russia", "Nigeria"
    ),
    EU = c(0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
    SubRegion = c(
      "Asia Pacific", "Asia Pacific", "Europe", "Europe", "Europe", "Europe", "Europe",
      "North America", "North America", "South America", "CIS", "Western Africa"
    )
  )
}

# The 2024 rows of data/raw/ev_Midstream_capacity.csv as read.csv() names them, plus a 2023
# row to check the year filter.
ev_midstream_fixture <- function() {
  tibble::tibble(
    Region = c("European Union", "North America", "Other Asia Pacific", "Rest of World", "China", "China"),
    Year = c(2024L, 2024L, 2024L, 2024L, 2024L, 2023L),
    Domestic.sales = c(2.226, 1.838, 0.783, 1.21, 11.23, 8.137),
    Domestic.Production = c(2.369, 1.374, 1.017, 0.128, 12.4047, 8.9741),
    import_share = c(0.064240791, -0.252448313, 0.298850575, -0.894214876, 0.10460374, 0.102875753),
    market_share = c(0.136994223, 0.079455493, 0.058810943, 0.007401967, 0.717337374, 0.654179515)
  )
}

ev_value <- function(out, country, variable, data_type = "raw") {
  out$value[out$Country == country & out$variable == variable & out$data_type == data_type]
}

test_that("every economy takes its IEA region's values, with non-EU Europe in Rest of World", {
  out <- foreign_dependency_build_ev_midstream(ev_ei_fixture(), ev_midstream_fixture(), year = 2024)

  expect_setequal(unique(out$Country), ev_ei_fixture()$Country)
  expect_equal(nrow(dplyr::count(out, Country, variable, data_type)), nrow(out))

  sales <- function(country) ev_value(out, country, "Domestic.sales")
  for (country in c("United Kingdom", "Norway", "Turkiye", "Brazil", "Russia", "Nigeria")) {
    expect_equal(sales(country), 1.21, info = country)
  }
  expect_equal(sales("Germany"), 2.226)
  expect_equal(sales("France"), 2.226)
  expect_equal(sales("United States"), 1.838)
  expect_equal(sales("Mexico"), 1.838)
  expect_equal(sales("Japan"), 0.783)
  expect_equal(sales("China"), 11.23)

  uk <- out$explanation[out$Country == "United Kingdom" & out$variable == "Domestic.sales"]
  expect_match(uk, "IEA region: Rest of World", fixed = TRUE)
})

test_that("the net export ratio is named for what it is and ranks exporters higher", {
  out <- foreign_dependency_build_ev_midstream(ev_ei_fixture(), ev_midstream_fixture(), year = 2024)

  expect_false(any(c("import_share", "import") %in% out$variable))
  expect_setequal(
    unique(out$variable[out$data_type == "index"]),
    c("production", "sales", "net_export", "market_share", "ev_Midstream")
  )

  expect_equal(ev_value(out, "China", "net_export_ratio"), (12.4047 - 11.23) / 11.23, tolerance = 1e-6)
  expect_lt(ev_value(out, "United Kingdom", "net_export_ratio"), 0)

  # Other Asia Pacific (+0.30) > China (+0.10) > EU (+0.06) > North America (-0.25) > Rest of
  # World (-0.89): higher = less dependent on imported EVs, as the pillars read every term.
  net_export <- function(country) ev_value(out, country, "net_export", "index")
  expect_gt(net_export("Japan"), net_export("China"))
  expect_gt(net_export("China"), net_export("Germany"))
  expect_gt(net_export("Germany"), net_export("United States"))
  expect_gt(net_export("United States"), net_export("United Kingdom"))
})

test_that("an extract that is not a net export ratio, or lacks a region, stops", {
  true_import_share <- ev_midstream_fixture() %>%
    dplyr::mutate(import_share = pmax(Domestic.sales - Domestic.Production, 0) / Domestic.sales)
  expect_error(
    foreign_dependency_build_ev_midstream(ev_ei_fixture(), true_import_share, year = 2024),
    "not (production - sales) / sales",
    fixed = TRUE
  )
  expect_error(
    market_share_manufacturing_build_ev_midstream(ev_ei_fixture(), true_import_share, year = 2024L),
    "not (production - sales) / sales",
    fixed = TRUE
  )

  no_china <- dplyr::filter(ev_midstream_fixture(), Region != "China")
  expect_error(
    foreign_dependency_build_ev_midstream(ev_ei_fixture(), no_china, year = 2024),
    "one row per IEA region"
  )
})

test_that("the Energy Security and Economic Opportunity builders expand the same way", {
  es <- foreign_dependency_build_ev_midstream(ev_ei_fixture(), ev_midstream_fixture(), year = 2024)
  eo <- market_share_manufacturing_build_ev_midstream(ev_ei_fixture(), ev_midstream_fixture(), year = 2024L)

  key <- c("Country", "variable", "data_type")
  expect_identical(
    dplyr::arrange(dplyr::select(es, dplyr::all_of(key), value, explanation), Country, variable, data_type),
    dplyr::arrange(dplyr::select(eo, dplyr::all_of(key), value, explanation), Country, variable, data_type)
  )
})

test_that("the Foreign Dependency overall lists the EV terms the builders emit", {
  index_definition <- yaml::read_yaml(file.path(repo_root, "config", "index_definition.yml"))
  components <- unlist(index_definition$overall_variables[["Overall Foreign Dependency Index"]]$components)

  expect_true(all(c("production", "sales", "net_export", "market_share") %in% components))
  expect_false("import" %in% components)
})
