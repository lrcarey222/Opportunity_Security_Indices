# Conduit financial centres in the Friendshore outbound-investment component
# (partnership_strength_build_outbound_edges(), R/themes/partnership_strength/safer_friendshore.R):
# a DIP position with a conduit is unobserved, not an outbound link.

poc_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(poc_root, "R", "utils", "scurve.R"))
source(file.path(poc_root, "R", "utils", "country.R"))
source(file.path(poc_root, "R", "utils", "schema.R"))
source(file.path(poc_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(poc_root, "R", "themes", "partnership_strength", "prosperous_opportunity.R"))
source(file.path(poc_root, "R", "themes", "partnership_strength", "safer_friendshore.R"))

poc_country_info <- function() {
  tibble::tibble(
    country = c("United States", "Japan", "India", "Netherlands", "Singapore", "Switzerland", "Luxembourg"),
    iso3c = c("USA", "JPN", "IND", "NLD", "SGP", "CHE", "LUX"),
    income = "High income"
  )
}

poc_fdi <- function() {
  tibble::tibble(
    COUNTRY = c(rep("United States", 6), "Netherlands", "Netherlands"),
    COUNTERPART_COUNTRY = c(
      "Japan", "India", "Netherlands", "Singapore", "Switzerland",
      "Hong Kong Special Administrative Region, People's Republic of China",
      "Japan", "India"
    ),
    DV_TYPE = "Reported official data",
    INDICATOR = "Outward Direct investment, Assets (gross), Debt instruments, All entities",
    # The centres hold the largest US positions, as in the IMF data.
    X2022 = c(50, 10, 900, 800, 700, 600, 300, 20)
  )
}

poc_gdp <- function() {
  tibble::tibble(
    iso3c = c("USA", "JPN", "IND", "NLD", "SGP", "CHE", "LUX", "HKG"),
    year = 2023,
    NY.GDP.MKTP.CD = c(27, 4, 3.5, 1.1, 0.5, 0.9, 0.09, 0.4) * 1e12
  )
}

test_that("the conduit list is the Damgaard-Elkjaer-Johannesen ten plus the previous exclusions", {
  centres <- partnership_strength_conduit_centres()
  expect_false(anyDuplicated(centres$iso3) > 0)
  expect_setequal(
    centres$iso3[centres$dej_top10],
    c("NLD", "LUX", "HKG", "VGB", "BMU", "SGP", "CYM", "CHE", "IRL", "MUS")
  )
  # Every centre excluded before October 2026 is still on the list.
  expect_setequal(
    centres$iso3[centres$previous_exclusion],
    c("VGB", "CYM", "BMU", "LUX", "BHS", "BRB", "TCA", "IRL", "IMN", "GGY", "CYP", "MUS")
  )
  expect_equal(nrow(centres), 16)
})

test_that("positions with a conduit are dropped on both sides by default, not scored", {
  out <- partnership_strength_build_outbound_edges(poc_fdi(), poc_country_info(), poc_gdp(), gdp_year = 2023)

  # The US ranks Japan and India only; the Netherlands reports no position at all.
  expect_setequal(out$partner_iso, c("JPN", "IND"))
  expect_setequal(out$reporter_iso, "USA")
  got <- stats::setNames(out$outbound_index, out$partner_iso)
  expect_equal(got[["JPN"]], 1)
  expect_equal(got[["IND"]], 0)
})

test_that("counterpart scope keeps a conduit's own positions; the legacy rule keeps the new centres", {
  cp <- partnership_strength_build_outbound_edges(
    poc_fdi(), poc_country_info(), poc_gdp(),
    gdp_year = 2023, conduit_scope = "counterpart"
  )
  expect_setequal(cp$partner_iso[cp$reporter_iso == "USA"], c("JPN", "IND"))
  expect_setequal(cp$partner_iso[cp$reporter_iso == "NLD"], c("JPN", "IND"))

  legacy <- partnership_strength_conduit_rule("legacy")
  expect_equal(legacy$scope, "counterpart")
  expect_false(any(c("NLD", "SGP", "CHE", "HKG") %in% legacy$iso3))
  old <- partnership_strength_build_outbound_edges(
    poc_fdi(), poc_country_info(), poc_gdp(),
    gdp_year = 2023, conduit_iso = legacy$iso3, conduit_scope = legacy$scope
  )
  # Before the fix the US's top outbound partners were the centres.
  us <- old[old$reporter_iso == "USA", ]
  expect_setequal(us$partner_iso, c("JPN", "IND", "NLD", "SGP", "CHE", "HKG"))
  expect_true(us$partner_iso[which.max(us$outbound_index)] %in% c("NLD", "SGP", "CHE", "HKG"))
  expect_lt(max(us$outbound_index[us$partner_iso %in% c("JPN", "IND")]), 1)

  default <- partnership_strength_conduit_rule()
  expect_equal(default$scope, "both")
  expect_setequal(default$iso3, partnership_strength_conduit_centres()$iso3)
  expect_error(partnership_strength_conduit_rule("bogus"))
})

test_that("config/partnership.yml selects the conduit rule scripts/15 applies", {
  cfg <- yaml::read_yaml(file.path(poc_root, "config", "partnership.yml"))
  expect_equal(cfg$outbound_fdi$conduit_rule, "conduits")
  expect_equal(cfg$outbound_fdi$conduit_scope, "both")
  rule <- partnership_strength_conduit_rule(cfg$outbound_fdi$conduit_rule, cfg$outbound_fdi$conduit_scope)
  expect_true(all(c("NLD", "SGP", "HKG", "CHE", "LUX", "IRL") %in% rule$iso3))
  s15 <- paste(readLines(file.path(poc_root, "scripts", "15_build_partner_themes.R"), warn = FALSE), collapse = "\n")
  expect_match(s15, "conduit_iso = outbound_conduits$iso3", fixed = TRUE)
})
