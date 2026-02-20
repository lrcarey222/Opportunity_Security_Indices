repo_root <- getwd()

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(repo_root, "R", "indices", "allied_network_design.R"))

make_base_inputs <- function(es_values, gdp_values) {
  countries <- c("Bigland", "Midland", "Smalland")
  iso <- c("BIG", "MID", "SML")

  economic_opportunity_index <- tibble::tibble(
    Country = countries,
    tech = "solar",
    supply_chain = "manufacturing",
    Economic_Opportunity_Index = c(0.8, 0.6, 0.4)
  )
  energy_security_index <- tibble::tibble(
    Country = countries,
    tech = "solar",
    supply_chain = "manufacturing",
    Energy_Security_Index = es_values
  )
  policy_index <- tibble::tibble(
    Country = countries,
    tech = "solar",
    supply_chain = "manufacturing",
    value = c(0.5, 0.5, 0.5)
  )
  country_info <- tibble::tibble(iso3c = iso, country = countries)
  country_size_tbl <- tibble::tibble(iso3c = iso, country = countries, year = 2023, gdp_usd = gdp_values, gdp_imputed = FALSE)

  edges <- tidyr::crossing(reporter_iso = iso, partner_iso = iso) %>%
    dplyr::mutate(
      Country = partner_iso,
      tech = "solar",
      supply_chain = "manufacturing",
      category = "friendshore",
      variable = "Friendshore Index",
      data_type = "index",
      value = 0.8
    )
  partner_friendshore_tbl <- edges
  partner_opportunity_tbl <- edges %>% dplyr::mutate(category = "opportunity", variable = "Opportunity Index", value = 0.7)

  list(
    eo = economic_opportunity_index, es = energy_security_index, policy = policy_index,
    country_info = country_info, size = country_size_tbl,
    friend = partner_friendshore_tbl, opp = partner_opportunity_tbl, iso = iso
  )
}

test_that("mixed demand normalizes and responds to GDP when ES is equal", {
  x <- make_base_inputs(es_values = c(0.4, 0.4, 0.4), gdp_values = c(1000, 500, 100))
  res <- allied_network_design(
    economic_opportunity_index = x$eo,
    energy_security_index = x$es,
    policy_index = x$policy,
    partner_friendshore_tbl = x$friend,
    partner_opportunity_tbl = x$opp,
    country_info = x$country_info,
    iso3c_network = x$iso,
    method = "greedy",
    country_size_tbl = x$size,
    demand_mode = "mixed",
    demand_weights = list(need = 0.5, size = 0.5),
    min_producers = 2,
    portfolio_enable = FALSE
  )

  s <- res$specialization
  expect_equal(sum(s$demand_weight), 1, tolerance = 1e-8)
  expect_true(all(c("size_idx", "need_idx", "demand_need_component", "demand_size_component") %in% names(s)))
  expect_true(all(is.finite(s$size_idx)))
  expect_true(all(is.finite(s$need_idx)))

  ord <- s %>% dplyr::arrange(dplyr::desc(gdp_usd))
  expect_gte(ord$demand_weight[[1]], ord$demand_weight[[3]])
})

test_that("mixed demand responds to ES need when GDP is equal", {
  x <- make_base_inputs(es_values = c(0.8, 0.5, 0.2), gdp_values = c(500, 500, 500))
  res <- allied_network_design(
    economic_opportunity_index = x$eo,
    energy_security_index = x$es,
    policy_index = x$policy,
    partner_friendshore_tbl = x$friend,
    partner_opportunity_tbl = x$opp,
    country_info = x$country_info,
    iso3c_network = x$iso,
    method = "greedy",
    country_size_tbl = x$size,
    demand_mode = "mixed",
    demand_weights = list(need = 0.5, size = 0.5),
    min_producers = 2,
    portfolio_enable = FALSE
  )

  s <- res$specialization %>% dplyr::arrange(need_idx)
  expect_gte(s$demand_weight[[3]], s$demand_weight[[1]])
})


test_that("partner development accepts lowercase country column", {
  x <- make_base_inputs(es_values = c(0.4, 0.4, 0.4), gdp_values = c(1000, 500, 100))
  dev_tbl <- tibble::tibble(
    country = c("Bigland", "Midland", "Smalland"),
    tech = "solar",
    supply_chain = "manufacturing",
    category = "development",
    variable = "Development Potential Index",
    data_type = "index",
    value = c(0.9, 0.6, 0.3)
  )

  res <- allied_network_design(
    economic_opportunity_index = x$eo,
    energy_security_index = x$es,
    policy_index = x$policy,
    partner_friendshore_tbl = x$friend,
    partner_opportunity_tbl = x$opp,
    partner_development_country_tbl = dev_tbl,
    country_info = x$country_info,
    iso3c_network = x$iso,
    method = "greedy",
    country_size_tbl = x$size,
    demand_mode = "need",
    min_producers = 2,
    portfolio_enable = FALSE
  )

  s <- res$specialization %>% dplyr::arrange(iso3c)
  expect_equal(s$dev_potential, c(0.9, 0.6, 0.3))
})
