repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "uspto_patents.R"))

inventor <- function(...) {
  addrs <- lapply(list(...), function(a) list(countryCode = a[[1]], postalAddressCategory = a[[2]]))
  list(correspondenceAddressBag = addrs)
}
record <- function(app, ...) list(applicationNumberText = app, applicationMetaData = list(inventorBag = list(...)))

test_that("CPC patterns become the ODP's padded, escaped stored form", {
  expect_equal(uspto_cpc_pattern("Y02E10/5*"), "Y02E\\ \\ 10\\/5*")
  expect_equal(uspto_cpc_pattern("Y02T10/7072"), "Y02T\\ \\ 10\\/7072")
  expect_equal(uspto_cpc_pattern("Y02E30/*"), "Y02E\\ \\ 30\\/*")
  expect_equal(uspto_cpc_pattern("Y04S*"), "Y04S*")
  expect_equal(uspto_cpc_pattern("H01M 1000/52"), "H01M1000\\/52")
  expect_error(uspto_cpc_pattern("solar"), "Not a CPC pattern")
  clause <- uspto_cpc_clause(c("Y02E60/10", "Y04S*"))
  expect_match(clause, "^\\(applicationMetaData.cpcClassificationBag:Y02E.* OR applicationMetaData.cpcClassificationBag:Y04S\\*\\)$")
})

test_that("inventor country prefers residence and falls back to the first address", {
  rec <- record("1",
                inventor(c("US", "postal"), c("CN", "residence")),
                inventor(c("JP", "postal")),
                list(correspondenceAddressBag = list()))
  expect_equal(uspto_record_inventor_countries(rec), c("CN", "JP", NA))
})

test_that("whole and fractional weights follow the counting rules", {
  recs <- list(
    record("a", inventor(c("US", "residence")), inventor(c("US", "residence")), inventor(c("DE", "residence"))),
    record("b", inventor(c("CN", "residence"))),
    record("c", inventor(c("NA", "residence")), list(correspondenceAddressBag = list()))
  )
  w <- uspto_records_to_country_weights(recs)
  a <- w[w$application == "a", ]
  expect_equal(a$fractional[a$inventor_iso2 == "US"], 2 / 3)
  expect_equal(a$fractional[a$inventor_iso2 == "DE"], 1 / 3)
  expect_true(all(a$whole == 1))
  # Namibia ("NA") is a country, not a missing value; the unknown inventor still dilutes the fraction.
  expect_equal(w$fractional[w$application == "c"], 0.5)
  agg <- uspto_aggregate_counts(w, "Solar", 2024)
  expect_equal(agg$patents_whole[agg$inventor_iso2 == USPTO_PATENTS_WORLD], 3)
  expect_equal(agg$patents_whole[agg$inventor_iso2 == "US"], 1)
  expect_equal(sum(agg$patents_fractional[agg$inventor_iso2 != USPTO_PATENTS_WORLD]), 2.5)
})

test_that("indicators: share, RTA and growth with small-base guards", {
  row <- function(y, tech, cc, whole, frac = whole) data.frame(grant_year = y, tech = tech, inventor_iso2 = cc,
                                                               patents_whole = whole, patents_fractional = frac)
  counts <- rbind(
    row(2024, "Solar", "AA", 60), row(2024, "Solar", "BB", 40), row(2024, "Solar", "CC", 2), row(2024, "Solar", "WLD", 100),
    row(2019, "Solar", "AA", 30), row(2019, "Solar", "BB", 40), row(2019, "Solar", "WLD", 70),
    row(2024, "_ALL", "AA", 600, NA), row(2024, "_ALL", "BB", 1600, NA), row(2024, "_ALL", "CC", 50, NA), row(2024, "_ALL", "WLD", 2000, NA)
  )
  ind <- uspto_patent_indicators(counts, 2024, 2019, min_patents = 5)
  aa <- ind[ind$inventor_iso2 == "AA", ]
  expect_equal(aa$share_world, 0.6)
  expect_equal(aa$rta, (60 / 600) / (100 / 2000))
  expect_equal(aa$growth, log(2))
  cc <- ind[ind$inventor_iso2 == "CC", ]
  expect_true(is.na(cc$rta))
  expect_true(is.na(cc$growth))
  expect_equal(sort(uspto_patent_universe(ind, min_total = 100)), c("AA", "BB"))

  scored <- uspto_patent_scores(ind)
  expect_true(all(scored$innovation_index >= 0 & scored$innovation_index <= 1))
  expect_equal(scored$inventor_iso2[which.max(scored$innovation_index)], "AA")

  scored$Country <- c("A", "B", "C")[match(scored$inventor_iso2, c("AA", "BB", "CC"))]
  theme <- uspto_patent_theme_table(scored, 2024, "2024 vs 2019")
  expect_silent(validate_schema(theme))
  expect_setequal(unique(theme$category), "Innovation")
})
