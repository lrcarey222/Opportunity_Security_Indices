repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "iea_rdd.R"))

raw_row <- function(country, tech, year, value, unit = "USD_R", sector = "PUBLIC", type = "RDD") {
  data.frame(COUNTRY = country, RDD_SECTOR = sector, RDD_TYPE = type, RDD_TECH = tech, UNIT = unit,
             TIME_PERIOD = as.character(year), OBS_VALUE = as.character(value), stringsAsFactors = FALSE)
}

test_that("public values keep public RD&D in constant USD and the GDP units of the total", {
  raw <- rbind(
    raw_row("JAPAN", "TOTAL", 2024, 3000), raw_row("JAPAN", "TOTAL", 2024, 0.5, unit = "U_GDP"),
    raw_row("JAPAN", "C12", 2024, 100), raw_row("JAPAN", "C12", 2024, 90, sector = "GOV"),
    raw_row("JAPAN", "C12", 2024, 80, type = "RD"), raw_row("JAPAN", "C12", 2024, 15000, unit = "NC_N"),
    raw_row("IEATOT", "C12", 2024, 999), raw_row("EU_COMMISSION", "C12", 2024, 999)
  )
  v <- iea_rdd_public_values(raw)
  expect_setequal(v$iso3, "JPN")
  expect_equal(v$usd_m[v$code == "C12"], 100)
  expect_equal(v$gdp_units[v$code == "TOTAL"], 0.5)
  expect_true(is.na(v$gdp_units[v$code == "C12"]))
})

test_that("nested code lists are refused and missing codes count as zero", {
  expect_true(iea_rdd_codes_nested(c("C1", "C12")))
  expect_false(iea_rdd_codes_nested(c("F511", "A3111")))
  values <- data.frame(iso3 = c("AAA", "AAA", "BBB"), code = c("TOTAL", "C12", "TOTAL"), year = 2024L,
                       usd_m = c(1000, 50, 400), gdp_units = c(0.4, NA, 0.2))
  expect_error(iea_rdd_tech_values(values, list(Solar = c("C1", "C12"))), "Nested")
  tv <- iea_rdd_tech_values(values, list(Solar = "C12"))
  expect_equal(tv$usd_m[tv$iso3 == "BBB"], 0)
  expect_equal(tv$usd_m[tv$iso3 == "AAA"], 50)
})

test_that("the code tree is read from prefixes and unallocated remainders are pushed down", {
  p <- iea_rdd_parent(c("TOTAL", "UN", "C", "C1", "C12", "C1UN", "C11", "E", "E11", "E1", "E1UN"))
  expect_equal(unname(p[c("C", "UN", "C1", "C12", "C1UN", "E11", "E1UN")]), c("TOTAL", "TOTAL", "C", "C1", "C1", "E1", "E1"))

  v <- data.frame(
    iso3 = c(rep("AAA", 6), rep("BBB", 4)), year = 2024L,
    code = c("TOTAL", "E", "E1", "E11", "E12", "E1UN", "TOTAL", "E", "E1", "E1UN"),
    usd_m = c(100, 100, 100, 30, 10, 60, 50, 50, 50, 50), gdp_units = NA_real_
  )
  a <- iea_rdd_allocate(v)
  # AAA: its own split (30:10) carries the 60 unallocated.
  expect_equal(a$usd_adj[a$iso3 == "AAA" & a$code == "E11"], 75)
  expect_equal(a$usd_adj[a$iso3 == "AAA" & a$code == "E12"], 25)
  # BBB allocated nothing below E1, so it takes the reporters' split that year (30:10).
  bbb <- rbind(v[v$iso3 == "BBB", ], data.frame(iso3 = "BBB", year = 2024L, code = c("E11", "E12"), usd_m = 0, gdp_units = NA_real_))
  b <- iea_rdd_allocate(rbind(v[v$iso3 == "AAA", ], bbb))
  expect_equal(b$usd_adj[b$iso3 == "BBB" & b$code == "E11"], 37.5)
  tv <- iea_rdd_tech_values(b, list(`Green Hydrogen` = "E11"))
  expect_equal(tv$usd_m[tv$iso3 == "BBB"], 37.5)
})

test_that("indicators and scores", {
  tv <- data.frame(
    tech = "Solar", iso3 = rep(c("AAA", "BBB", "CCC"), each = 2), year = rep(c(2019L, 2024L), 3),
    usd_m = c(20, 60, 40, 40, 0, 0.5), total_usd_m = c(800, 1000, 1000, 1000, 100, 100),
    gdp_units = c(0.4, 0.5, 0.2, 0.2, 0.1, 0.1)
  )
  ind <- iea_rdd_indicators(tv, 2024L, 2019L)
  a <- ind[ind$iso3 == "AAA", ]
  expect_equal(a$share_reporting, 60 / 100.5)
  expect_equal(a$intensity, 60 / 1000 * 0.5)
  expect_equal(a$specialisation, (60 / 1000) / (100.5 / 2100))
  expect_equal(a$growth, log(3))
  expect_true(is.na(ind$growth[ind$iso3 == "CCC"]))

  sc <- iea_rdd_scores(ind)
  expect_equal(sc$iso3[which.max(sc$rdd_index)], "AAA")
  sc$Country <- sc$iso3
  theme <- iea_rdd_theme_table(sc, 2024, "2024 vs 2019")
  expect_silent(validate_schema(theme))
})
