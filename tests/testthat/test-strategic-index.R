# Tests for the Strategic Index and the allied sector pick (R/indices/build_indices.R).
repo_root <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "indices", "build_indices.R"))

si_techs <- c("Solar", "Wind", "Coal", "Geothermal")
si_stages <- c("Upstream", "Midstream", "Downstream")

# Two countries x 4 techs x 3 stages with distinct, reproducible pillar values.
si_fixture <- function() {
  keys <- tidyr::crossing(Country = c("A", "B"), tech = si_techs, supply_chain = si_stages)
  n <- nrow(keys)
  list(
    eo = keys %>% dplyr::mutate(Economic_Opportunity_Index = seq(0.20, 0.80, length.out = n)),
    es = keys %>% dplyr::mutate(Energy_Security_Index = rev(seq(0.10, 0.90, length.out = n))),
    # 7 and 25 are coprime, so the 24 policy values are distinct (no rank ties).
    pol = keys %>% dplyr::mutate(value = ((seq_len(n) * 7) %% 25) / 25),
    ghg = tibble::tibble(tech = si_techs, ghg_index = c(0.95, 0.99, 0, 0.9))
  )
}

build_fixture_si <- function(fx = si_fixture(), ...) {
  build_strategic_index(
    economic_opportunity_index = fx$eo,
    energy_security_index = fx$es,
    policy_index = fx$pol,
    tech_ghg = fx$ghg,
    ...
  )
}

test_that("the SI is 0.2 pol + 0.3 EO + 0.3 (1 - ES) + 0.1 stage + 0.1 climate", {
  si <- build_fixture_si() %>% dplyr::filter(!is.na(.data$strategic_index))

  expected <- 0.2 * si$pol +
    0.3 * si$Economic_Opportunity_Index +
    0.3 * (1 - si$Energy_Security_Index) +
    0.1 * si$sc_weight +
    0.1 * si$tech_weight
  expect_equal(si$strategic_index, expected)
  expect_equal(unname(STRATEGIC_INDEX_WEIGHTS), c(0.2, 0.3, 0.3, 0.1, 0.1))
  expect_equal(sum(STRATEGIC_INDEX_WEIGHTS), 1)
  expect_equal(unname(si$sc_weight[si$supply_chain == "Midstream"][1]), 0.75)
  expect_equal(unname(si$sc_weight[si$supply_chain == "Upstream"][1]), 0.50)
  expect_equal(unname(si$sc_weight[si$supply_chain == "Downstream"][1]), 0.25)

  # The contributions reproduce the index exactly.
  contrib <- strategic_index_contributions(si)
  total <- contrib$contrib_policy + contrib$contrib_eo + contrib$contrib_es_need +
    contrib$contrib_sc + contrib$contrib_ghg
  expect_equal(total, si$strategic_index)
})

test_that("Energy_Security_Index keeps its security meaning; the SI uses the need", {
  fx <- si_fixture()
  si <- build_fixture_si(fx)
  joined <- si %>%
    dplyr::inner_join(
      fx$es %>% dplyr::rename(es_input = "Energy_Security_Index"),
      by = c("Country", "tech", "supply_chain")
    )

  expect_equal(joined$Energy_Security_Index, joined$es_input)
  expect_equal(joined$energy_security_need, 1 - joined$es_input)

  # Raising a cell's security, all else equal, lowers its SI by 0.3 per unit.
  bumped <- fx
  hit <- bumped$es$Country == "A" & bumped$es$tech == "Solar" & bumped$es$supply_chain == "Upstream"
  bumped$es$Energy_Security_Index[hit] <- bumped$es$Energy_Security_Index[hit] + 0.05
  si_bumped <- build_fixture_si(bumped)
  pick <- function(x) x$strategic_index[x$Country == "A" & x$tech == "Solar" & x$supply_chain == "Upstream"]
  expect_equal(pick(si_bumped) - pick(si), -0.3 * 0.05)

  # The within-country S-curve of the need is the mirror of the S-curve of security.
  a <- si %>% dplyr::filter(.data$Country == "A", !is.na(.data$strategic_index))
  expect_equal(a$es_need, 1 - median_scurve(a$Energy_Security_Index))
  expect_equal(a$es, a$es_need)
})

test_that("cells with a missing pillar or policy value are dropped, not imputed", {
  fx <- si_fixture()
  miss_es <- fx$es$Country == "A" & fx$es$tech == "Wind" & fx$es$supply_chain == "Upstream"
  fx$es$Energy_Security_Index[miss_es] <- NA
  fx$eo <- fx$eo %>%
    dplyr::filter(!(.data$Country == "A" & .data$tech == "Coal" & .data$supply_chain == "Midstream")) %>%
    dplyr::bind_rows(tibble::tibble(
      Country = "A", tech = "Coal", supply_chain = "Midstream", Economic_Opportunity_Index = NA_real_
    ))
  fx$pol <- fx$pol %>%
    dplyr::filter(!(.data$Country == "A" & .data$tech == "Solar" & .data$supply_chain == "Downstream"))

  si <- build_fixture_si(fx)
  cell <- function(tech, sc) si[si$Country == "A" & si$tech == tech & si$supply_chain == sc, ]

  expect_true(is.na(cell("Wind", "Upstream")$strategic_index))
  expect_equal(cell("Wind", "Upstream")$si_exclusion, "missing energy security")
  expect_true(is.na(cell("Coal", "Midstream")$strategic_index))
  expect_equal(cell("Coal", "Midstream")$si_exclusion, "missing economic opportunity")
  expect_true(is.na(cell("Solar", "Downstream")$strategic_index))
  expect_equal(cell("Solar", "Downstream")$si_exclusion, "missing policy index")

  # The within-country policy ranks are taken over the scored cells only.
  a <- si %>% dplyr::filter(.data$Country == "A", !is.na(.data$strategic_index))
  expect_equal(a$pol, median_scurve(a$value))
  expect_false(any(is.na(a$pol)))
})

test_that("countries without a Policy index get no SI and say why", {
  fx <- si_fixture()
  fx$pol <- fx$pol %>% dplyr::filter(.data$Country == "A")

  si <- build_fixture_si(fx)
  b <- si %>% dplyr::filter(.data$Country == "B")

  expect_true(all(is.na(b$strategic_index)))
  expect_true(all(b$si_exclusion == "country not policy-eligible"))

  # eo and es_need are not in the SI and feed the charts' EO vs ES-risk views, so they stay
  # defined (ranked over the country's observed pillar values) without a Policy index.
  expect_equal(b$eo, median_scurve(b$Economic_Opportunity_Index))
  expect_equal(b$es_need, median_scurve(b$energy_security_need))
  expect_true(all(is.na(b$pol)))
})

test_that("eo and es_need are ranked over the observed pillar, pol over the scored cells", {
  fx <- si_fixture()
  miss_es <- fx$es$Country == "A" & fx$es$tech == "Wind" & fx$es$supply_chain == "Upstream"
  fx$es$Energy_Security_Index[miss_es] <- NA

  si <- build_fixture_si(fx)
  a <- si %>% dplyr::filter(.data$Country == "A")
  wind_up <- a$tech == "Wind" & a$supply_chain == "Upstream"

  expect_false(is.na(a$eo[wind_up]))
  expect_true(is.na(a$es_need[wind_up]))
  expect_true(is.na(a$pol[wind_up]))
  expect_equal(a$eo, median_scurve(a$Economic_Opportunity_Index))
  scored <- !is.na(a$strategic_index)
  expect_equal(a$pol[scored], median_scurve(a$value[scored]))
})

test_that("Geothermal - Midstream is removed before the within-country S-curves", {
  si <- build_fixture_si()

  expect_false("Geothermal - Midstream" %in% si$sector)
  a <- si %>% dplyr::filter(.data$Country == "A")
  # 4 techs x 3 stages minus one excluded sector, ranked among themselves.
  expect_equal(nrow(a), 11L)
  expect_equal(a$pol, median_scurve(a$value))
  expect_equal(range(a$pol), c(0, 1))
})

test_that("duplicated keys in an input stop the build instead of duplicating rows", {
  fx <- si_fixture()
  fx$es <- dplyr::bind_rows(fx$es, fx$es[1, ])

  expect_error(build_fixture_si(fx), "duplicated Country x tech x supply_chain")
})

test_that("a pillar that is constant across countries within a sector is flagged", {
  fx <- si_fixture()
  flat <- fx$es$tech == "Coal" & fx$es$supply_chain == "Upstream"
  fx$es$Energy_Security_Index[flat] <- 0

  deg <- strategic_index_degenerate_sectors(build_fixture_si(fx))

  expect_equal(deg$sector, "Coal - Upstream")
  expect_true(deg$es_constant)
  expect_equal(deg$es_value, 0)
  expect_false(deg$eo_constant)
  expect_equal(nrow(strategic_index_degenerate_sectors(build_fixture_si())), 0L)
})

test_that("the allied pick takes the top Upstream/Midstream sector outside Geothermal", {
  si <- tibble::tibble(
    Country = c("A", "A", "A", "A", "B"),
    tech = c("Geothermal", "Solar", "Wind", "Coal", "Wind"),
    supply_chain = c("Upstream", "Downstream", "Midstream", "Upstream", "Upstream"),
    strategic_index = c(0.9, 0.8, 0.6, 0.5, NA)
  )

  out <- select_allied_strategic_sectors(si, ally_countries = c("A", "B"))

  expect_equal(out$Country, "A")
  expect_equal(out$tech, "Wind")
  expect_equal(out$supply_chain, "Midstream")
})

test_that("the ally list uses valid ISO3 codes (ARE, not UAE)", {
  expect_true("ARE" %in% STRATEGIC_ALLY_ISO3)
  expect_false("UAE" %in% STRATEGIC_ALLY_ISO3)
  expect_equal(length(STRATEGIC_ALLY_ISO3), 23L)

  wdi <- file.path(repo_root, "data", "raw", "wdi_country_info.csv")
  skip_if_not(file.exists(wdi), "staged WDI country file not available")
  iso <- toupper(utils::read.csv(wdi, stringsAsFactors = FALSE)$iso3c)
  expect_equal(setdiff(STRATEGIC_ALLY_ISO3, iso), character(0))
})
