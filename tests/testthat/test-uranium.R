# Uranium (Nuclear - Upstream): Red Book resources and mine production.
#
# Nuclear - Upstream used to have no country data at all - its only input was the world
# uranium price - so Energy Security was the same for every economy and the Strategic Index
# had to drop the sector. These tests pin the transcribed Red Book tables to the published
# totals and check that "Nuclear Reserves" and "Nuclear Production" are built the way the
# reserve and production themes build the other fuels and minerals.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "levels.R"))
source(file.path(repo_root, "R", "utils", "iea_critical_minerals.R"))
source(file.path(repo_root, "R", "categories", "shared", "overall_index.R"))
source(file.path(repo_root, "R", "categories", "reserves", "reserves.R"))
source(file.path(repo_root, "R", "categories", "reserves", "uranium.R"))
source(file.path(repo_root, "R", "categories", "production", "production_depth_momentum.R"))

# No index definition: the themes' own "Overall ..." rows are not what these tests check.
withr::local_options(list(opportunity_security.index_definition = NULL))

reference_csv <- function(name) {
  utils::read.csv(file.path(repo_root, "data", "reference", name), stringsAsFactors = FALSE)
}

uranium_resources_fixture <- function() {
  src <- "OECD-NEA/IAEA (2026), Uranium 2026: Resources, Production and Demand, NEA No. 7770, Table 1.2a"
  tibble::tribble(
    ~country,     ~iso3c, ~lt_usd130, ~lt_usd260,
    "Australia",  "AUS",     1701600,    1959200,
    "Kazakhstan", "KAZ",      827500,     886900,
    "Namibia",    "NAM",      392800,     494500,
    # Reports no cost data, so it appears in the <USD 260/kgU category only (note d).
    "India",      "IND",           0,     273700
  ) %>%
    tidyr::pivot_longer(c(lt_usd130, lt_usd260), names_to = "cat", values_to = "value") %>%
    dplyr::transmute(
      country, iso3c,
      variable = paste0("identified_recoverable_resources_", cat, "_kgu"),
      value, unit = "tU", year = 2025L,
      source = src, url = "https://www.oecd-nea.org/", accessed = "2026-10-07"
    )
}

uranium_production_fixture <- function() {
  tibble::tribble(
    ~country,     ~iso3c, ~year, ~value,
    "Kazakhstan", "KAZ",  2019, 22808,
    "Kazakhstan", "KAZ",  2024, 23270,
    "Canada",     "CAN",  2019,  6944,
    "Canada",     "CAN",  2024, 14327,
    "Namibia",    "NAM",  2019,  5477,
    "Namibia",    "NAM",  2024,  7332,
    "Niger",      "NER",  2019,  2982,
    "Niger",      "NER",  2024,   959,
    # A historical producer listed with no current output.
    "France",     "FRA",  2019,     0,
    "France",     "FRA",  2024,     0
  ) %>%
    dplyr::mutate(
      variable = "mine_production", unit = "tU",
      source = dplyr::if_else(
        year == 2024,
        "OECD-NEA/IAEA (2026), Uranium 2026: Resources, Production and Demand, NEA No. 7770, Table 1.18",
        "OECD-NEA/IAEA (2023), Uranium 2022: Resources, Production and Demand, NEA No. 7634, Table 1.18"
      ),
      url = "https://www.oecd-nea.org/", accessed = "2026-10-07"
    )
}

# An EI-style roster that, like the real one, omits Namibia and Niger.
ei_roster_fixture <- function() {
  tibble::tibble(
    Country = c("Australia", "Kazakhstan", "Canada", "France", "India", "Japan"),
    ISO3166_alpha3 = c("AUS", "KAZ", "CAN", "FRA", "IND", "JPN")
  )
}

# ---- The transcribed reference tables ---------------------------------------------------------

test_that("the Red Book resource table is transcribed in full and traceable", {
  res <- reference_csv("uranium_resources.csv")
  expect_true(all(uranium_reference_columns() %in% names(res)))
  expect_no_error(uranium_validate_reference(res, label = "uranium_resources.csv"))
  expect_true(all(nzchar(res$source) & nzchar(res$url) & nzchar(res$accessed)))
  expect_true(all(grepl("^https://www\\.oecd-nea\\.org/", res$url)))

  # Table 1.2a (as of 1 January 2025) lists 59 countries in four cumulative cost categories,
  # so each category's value can only grow from the cheapest to the dearest.
  expect_equal(dplyr::n_distinct(res$iso3c), 59)
  wide <- res %>%
    dplyr::select(iso3c, variable, value) %>%
    tidyr::pivot_wider(names_from = variable, values_from = value)
  expect_true(all(wide$identified_recoverable_resources_lt_usd40_kgu <= wide$identified_recoverable_resources_lt_usd80_kgu))
  expect_true(all(wide$identified_recoverable_resources_lt_usd80_kgu <= wide$identified_recoverable_resources_lt_usd130_kgu))
  expect_true(all(wide$identified_recoverable_resources_lt_usd130_kgu <= wide$identified_recoverable_resources_lt_usd260_kgu))

  # Published totals; country rows are rounded to 100 tU, so the sums may miss by a few hundred.
  totals <- tapply(res$value, res$variable, sum)
  expect_lt(abs(totals[["identified_recoverable_resources_lt_usd260_kgu"]] - 8104500), 1000)
  expect_lt(abs(totals[["identified_recoverable_resources_lt_usd130_kgu"]] - 5864500), 1000)
  expect_equal(wide$identified_recoverable_resources_lt_usd260_kgu[wide$iso3c == "AUS"], 1959200)
  expect_equal(wide$identified_recoverable_resources_lt_usd260_kgu[wide$iso3c == "KAZ"], 886900)
  # The Red Book reports Greenland's deposits under Denmark; they are Greenland's.
  expect_true("GRL" %in% res$iso3c)
  expect_false("DNK" %in% res$iso3c)
})

test_that("the Red Book production table reconciles with the published world totals", {
  prod <- reference_csv("uranium_production.csv")
  expect_no_error(uranium_validate_reference(prod, label = "uranium_production.csv"))
  expect_setequal(unique(prod$variable), "mine_production")
  expect_equal(sort(unique(prod$year)), 2019:2024)

  # Table 1.18 world totals: Uranium 2022 (2019), Uranium 2024 (2020-2021), Uranium 2026
  # (2022-2024). Every producer is listed, so the rows sum to the world.
  world <- c(`2019` = 54478, `2020` = 47588, `2021` = 47361, `2022` = 49490, `2023` = 54563, `2024` = 61924)
  sums <- tapply(prod$value, prod$year, sum)
  expect_equal(as.numeric(round(sums[names(world)])), as.numeric(world))

  # Each year comes from the newest edition that reports it as an actual.
  expect_true(all(grepl("NEA No. 7634", prod$source[prod$year == 2019])))
  expect_true(all(grepl("NEA No. 7683", prod$source[prod$year %in% 2020:2021])))
  expect_true(all(grepl("NEA No. 7770", prod$source[prod$year >= 2022])))
  expect_equal(prod$value[prod$iso3c == "KAZ" & prod$year == 2024], 23270)
  expect_equal(prod$value[prod$iso3c == "CAN" & prod$year == 2024], 14327)
})

test_that("reference validation rejects duplicates, negatives and unsourced rows", {
  res <- uranium_resources_fixture()
  expect_error(uranium_validate_reference(dplyr::bind_rows(res, res[1, ])), "more than one value")
  bad <- res; bad$value[1] <- -1
  expect_error(uranium_validate_reference(bad), "non-negative")
  bad <- res; bad$source[1] <- ""
  expect_error(uranium_validate_reference(bad), "source")
  expect_error(uranium_validate_reference(dplyr::select(res, -url)), "url")
})

# ---- Index construction -----------------------------------------------------------------------

test_that("a reported zero scores like the zero anchor, not above unlisted economies", {
  idx <- uranium_anchor_index(c(100, 0, NA, 10, 50))
  expect_equal(idx[2], 0)
  expect_true(is.na(idx[3]))
  expect_equal(idx[1], 1)
  expect_gt(idx[4], 0)
  expect_equal(idx[c(1, 4, 5)], median_scurve(c(0, 100, 10, 50))[-1])
})

test_that("Nuclear Reserves keeps producers the EI roster omits and scores the headline category", {
  out <- uranium_reserves_table(uranium_resources_fixture(), country_reference = ei_roster_fixture())

  expect_setequal(unique(out$variable), "Nuclear Reserves")
  expect_setequal(unique(paste(out$tech, out$supply_chain, out$category)), "Nuclear Upstream Reserves")
  expect_equal(nrow(out), 2 * dplyr::n_distinct(out$ISO3166_alpha3))

  idx <- out %>% dplyr::filter(data_type == "index")
  # Namibia is not on the EI roster but holds resources; dropping it was the inner-join failure.
  expect_true("Namibia" %in% idx$Country)
  expect_equal(idx$value[idx$Country == "Australia"], 1)
  # Unlisted economies stay NA here; the reserves missing-data rule scores them 0.
  expect_true(is.na(idx$value[idx$Country == "Japan"]))
  # India reports only the <USD 260/kgU category; the headline category credits it.
  expect_gt(idx$value[idx$Country == "India"], 0)
  expect_true(all(grepl("USD 260", out$explanation[out$data_type == "raw"])))
  expect_true(all(grepl("NEA No. 7770", out$source)))

  out130 <- uranium_reserves_table(
    uranium_resources_fixture(),
    country_reference = ei_roster_fixture(),
    cost_category = "lt_usd130"
  )
  idx130 <- out130 %>% dplyr::filter(data_type == "index")
  expect_equal(idx130$value[idx130$Country == "India"], 0)
  expect_error(
    uranium_reserves_table(uranium_resources_fixture(), ei_roster_fixture(), cost_category = "lt_usd40"),
    "no rows"
  )
})

test_that("reserves() adds Nuclear Reserves without touching the other reserve variables", {
  ei <- tibble::tibble(
    Country = c("Australia", "China", "Canada", "Kazakhstan", "Total World"),
    Year = 2025,
    ISO3166_alpha3 = c("AUS", "CHN", "CAN", "KAZ", NA)
  )
  inputs <- list(
    list(
      sheet = "Oil - Proved reserves history", nm_col = "Thousand million barrels",
      val_col = "2020...42", tech_name = "Oil", unit_desc = "Thousand million barrels",
      data_year = 2020,
      data = tibble::tibble(
        `Thousand million barrels` = c("Canada", "China", "Australia", "Total World"),
        `2020...42` = c(168.1, 26.0, 2.4, 1732.4)
      )
    ),
    list(
      sheet = "Copper P-R", nm_col = "Thousand tonnes", val_col = "At end of 2025",
      tech_name = "Copper", unit_desc = "Thousand tonnes",
      data = tibble::tibble(
        `Thousand tonnes` = c("Australia", "China", "Rest of World", "Total World"),
        `At end of 2025` = c(100000, 41000, 500000, 641000)
      )
    )
  )
  demand <- tibble::tibble(
    Pillar = "3.1 Energy demand by tech", Mineral = "Copper", `Sector.Country` = "Wind",
    X2025 = 10, X2035 = 12
  )
  mdc <- reserves_build_mineral_demand_clean(as.data.frame(demand))

  without <- suppressWarnings(reserves(ei, inputs, mdc)) %>% dplyr::ungroup()
  with <- suppressWarnings(reserves(ei, inputs, mdc, uranium_resources = uranium_resources_fixture())) %>%
    dplyr::ungroup()

  expect_false(any(without$tech == "Nuclear"))
  nuc <- with %>% dplyr::filter(tech == "Nuclear", supply_chain == "Upstream")
  expect_setequal(unique(nuc$variable), "Nuclear Reserves")
  expect_true("Namibia" %in% nuc$Country)

  key <- c("ISO3166_alpha3", "tech", "supply_chain", "variable", "data_type")
  same <- dplyr::inner_join(
    without %>% dplyr::select(dplyr::all_of(key), old = value),
    with %>% dplyr::filter(tech != "Nuclear") %>% dplyr::select(dplyr::all_of(key), new = value),
    by = key
  )
  expect_equal(nrow(same), nrow(without))
  expect_identical(same$old, same$new)
})

test_that("Nuclear Production ranks current producers and zero-fills everyone else", {
  roster <- dplyr::bind_rows(
    ei_roster_fixture(),
    tibble::tibble(Country = c("Chad", "Norway"), iso3c = c("TCD", "NOR"))
  )
  out <- uranium_production_table(uranium_production_fixture(), country_reference = roster)
  idx <- out %>% dplyr::filter(data_type == "index")
  raw <- out %>% dplyr::filter(data_type == "raw")

  expect_setequal(unique(out$variable), "Nuclear Production")
  expect_setequal(unique(out$category), "Production")
  expect_equal(unique(out$Year), 2024L)
  expect_equal(raw$value[raw$Country == "Canada"], 14327)
  expect_equal(idx$value[idx$Country == "Kazakhstan"], 1)
  expect_true(all(c("Namibia", "Niger", "Chad", "Norway") %in% idx$Country))
  # France is listed with zero output: it scores the same 0 as an economy the table omits.
  expect_equal(idx$value[idx$Country == "France"], 0)
  expect_equal(idx$value[idx$Country == "Chad"], 0)
  expect_gt(min(idx$value[idx$Country %in% c("Kazakhstan", "Canada", "Namibia", "Niger")]), 0)
  expect_gt(idx$value[idx$Country == "Namibia"], idx$value[idx$Country == "Niger"])
  expect_true(all(grepl("NEA No. 7770", out$source)))
  expect_error(uranium_production_table(uranium_production_fixture(), roster, year = 2010), "2010")
})

test_that("uranium production is scored with the fuels' depth-and-momentum indices", {
  clean <- uranium_production_clean(
    uranium_production_fixture(),
    countries = c("Kazakhstan", "Canada", "Japan", "Europe")
  )
  expect_true(all(c("2019", "2024", "change_5yr", "change_10yr", "change_5yr_abs") %in% names(clean)))
  expect_setequal(unique(clean$Var), "Nuclear Upstream")
  # Red Book producers outside the EI roster join it; aggregates do not.
  expect_true(all(c("Namibia", "Niger", "Japan") %in% clean$Country))
  expect_false("Europe" %in% clean$Country)
  expect_equal(clean$`2024`[clean$Country == "Japan"], 0)
  expect_equal(clean$change_5yr_abs[clean$Country == "Niger"], 959 - 2982)
  expect_true(all(is.na(clean$change_10yr)))
  expect_error(
    uranium_production_clean(uranium_production_fixture(), countries = "Japan", latest_year = 2025),
    "2020"
  )

  tbl <- production_depth_momentum_build_uranium(
    uranium_production_fixture(),
    countries = c("Kazakhstan", "Canada", "Japan"),
    year = 2025
  )
  expect_setequal(unique(tbl$variable), c("production_latest", "size", "growth_abs", "Overall Production"))
  expect_setequal(unique(paste(tbl$tech, tbl$supply_chain)), "Nuclear Upstream")
  expect_true(all(grepl("2024", tbl$explanation[tbl$variable %in% c("production_latest", "size")])))
  size <- tbl %>% dplyr::filter(variable == "size")
  expect_equal(size$value[size$Country == "Kazakhstan"], 1)
  growth <- tbl %>% dplyr::filter(variable == "growth_abs")
  # Canada doubled its output over 2019-2024; Niger lost two-thirds of it.
  expect_equal(growth$value[growth$Country == "Canada"], 1)
  expect_equal(growth$value[growth$Country == "Niger"], 0)

  # An earlier vintage reads no later output: the latest year not after `year` is used, and a
  # vintage whose 5-year base the table does not reach stops rather than borrowing 2024.
  expect_equal(uranium_production_latest_year(uranium_production_fixture()), 2024)
  expect_equal(uranium_production_latest_year(uranium_production_fixture(), year = 2022), 2019)
  expect_error(uranium_production_latest_year(uranium_production_fixture(), year = 2015), "up to 2015")
  expect_error(
    production_depth_momentum_build_uranium(
      uranium_production_fixture(),
      countries = c("Kazakhstan", "Canada"),
      year = 2020
    ),
    "2014"
  )
})

test_that("production_depth_momentum() adds Nuclear Upstream only when given uranium data", {
  ei <- tidyr::expand_grid(
    Country = c("Kazakhstan", "Canada", "Australia", "China"),
    Year = c(2015L, 2020L, 2025L),
    Var = c("oilprod_kbd", "nuclear_twh")
  ) %>%
    dplyr::mutate(Value = seq_along(Country) * 10)
  critical <- dplyr::bind_rows(
    tibble::tibble(
      Pillar = "3.1 Energy demand by tech", Mineral = "Copper",
      `Sector.Country` = c("Wind", "Electricity networks"), X2025 = c(10, 30), X2035 = c(12, 40)
    ),
    tibble::tibble(
      Pillar = "2 Total supply for key minerals", Mineral = "Copper - Mining",
      `Sector.Country` = c("Australia", "China", "Rest of world"), X2025 = c(50, 30, 20), X2035 = c(45, 35, 20)
    )
  ) %>%
    as.data.frame()
  mdc <- reserves_build_mineral_demand_clean(critical)
  # Norway is a country_info economy outside the EI fuel roster that the Red Book omits.
  country_info <- tibble::tibble(country = c("Kazakhstan", "Canada", "Australia", "China", "Norway"))

  without <- production_depth_momentum(ei, critical, mdc, country_info)
  with <- production_depth_momentum(ei, critical, mdc, country_info, uranium_production = uranium_production_fixture())

  expect_false(any(without$tech == "Nuclear" & without$supply_chain == "Upstream"))
  nuc <- with %>% dplyr::filter(tech == "Nuclear", supply_chain == "Upstream")
  expect_gt(nrow(nuc), 0)
  expect_true("Namibia" %in% nuc$Country)
  expect_true(all(grepl("Uranium", nuc$source)))
  # Table 1.18 lists every producer, so Norway is an observed zero, as on the Energy Security
  # side, rather than an economy the missing-data rule fills with the global average.
  norway <- nuc %>% dplyr::filter(Country == "Norway")
  expect_setequal(norway$variable, c("production_latest", "size", "growth_abs", "Overall Production"))
  expect_equal(norway$value[norway$variable == "production_latest"], 0)
  expect_false(any(without$Country == "Norway"))

  key <- c("Country", "tech", "supply_chain", "variable", "data_type")
  same <- dplyr::inner_join(
    without %>% dplyr::select(dplyr::all_of(key), old = value),
    with %>% dplyr::filter(!(tech == "Nuclear" & supply_chain == "Upstream")) %>%
      dplyr::select(dplyr::all_of(key), new = value),
    by = key
  )
  expect_equal(nrow(same), nrow(without))
  expect_identical(same$old, same$new)
})

test_that("uranium rows never silently merge with a variable another input already emits", {
  theme <- tibble::tibble(
    Country = "Canada", tech = "Nuclear", supply_chain = "Upstream", category = "Reserves",
    variable = "Nuclear Reserves", data_type = "index", value = 0.5, Year = 2025L,
    source = "IEA", explanation = "mineral rollup"
  )
  uranium <- uranium_reserves_table(uranium_resources_fixture(), ei_roster_fixture())
  expect_error(uranium_bind_theme_rows(theme, uranium), "Nuclear Upstream Nuclear Reserves")

  other <- theme %>% dplyr::mutate(tech = "Wind", variable = "Wind Reserves")
  expect_equal(nrow(uranium_bind_theme_rows(other, uranium)), 1 + nrow(uranium))
})

test_that("the theme build reads the Red Book tables and passes them to all three themes", {
  script <- paste(readLines(file.path(repo_root, "scripts", "10_build_themes.R"), warn = FALSE), collapse = "\n")
  expect_match(script, "source\\(file\\.path\\(repo_root, \"R\", \"categories\", \"reserves\", \"uranium\\.R\"\\)\\)")
  expect_match(script, "file\\.path\\(raw_data_path, \"uranium_resources\\.csv\"\\)")
  expect_match(script, "file\\.path\\(raw_data_path, \"uranium_production\\.csv\"\\)")
  expect_match(script, "uranium_resources = uranium_resources")
  expect_match(script, "uranium_production = uranium_production")
  expect_match(script, "uranium_production_table\\(")
})
