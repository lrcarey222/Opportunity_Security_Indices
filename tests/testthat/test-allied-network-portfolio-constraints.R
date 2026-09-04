# testthat runs with the working directory set to tests/testthat, so the repo root is
# resolved from this file rather than from getwd().
repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(repo_root, "R", "indices", "allied_network_design.R"))

test_that("portfolio constraints cap top-k appearances", {
  countries <- c("Bigland", "Midland", "Smalland")
  iso <- c("BIG", "MID", "SML")
  techs <- c("solar", "wind")
  stages <- c("upstream", "downstream")

  grid <- tidyr::crossing(Country = countries, tech = techs, supply_chain = stages)
  economic_opportunity_index <- grid %>% dplyr::mutate(Economic_Opportunity_Index = dplyr::case_when(Country == "Bigland" ~ 0.95, Country == "Midland" ~ 0.5, TRUE ~ 0.3))
  energy_security_index <- grid %>% dplyr::mutate(Energy_Security_Index = 0.4)
  policy_index <- grid %>% dplyr::mutate(value = 0.5)
  country_info <- tibble::tibble(iso3c = iso, country = countries)
  country_size_tbl <- tibble::tibble(iso3c = iso, country = countries, year = 2023, gdp_usd = c(10000, 1000, 200), gdp_imputed = FALSE)

  edge_base <- tidyr::crossing(reporter_iso = iso, partner_iso = iso, tech = techs, supply_chain = stages)
  partner_friendshore_tbl <- edge_base %>%
    dplyr::mutate(
      Country = partner_iso,
      category = "friendshore",
      variable = "Friendshore Index",
      data_type = "index",
      value = 0.9
    )
  partner_opportunity_tbl <- edge_base %>%
    dplyr::mutate(
      Country = partner_iso,
      category = "opportunity",
      variable = "Opportunity Index",
      data_type = "index",
      value = 0.9
    )

  res <- allied_network_design(
    economic_opportunity_index = economic_opportunity_index,
    energy_security_index = energy_security_index,
    policy_index = policy_index,
    partner_friendshore_tbl = partner_friendshore_tbl,
    partner_opportunity_tbl = partner_opportunity_tbl,
    country_info = country_info,
    iso3c_network = iso,
    method = "greedy",
    min_producers = 2,
    max_share = 0.8,
    country_size_tbl = country_size_tbl,
    demand_mode = "mixed",
    demand_weights = list(need = 0.5, size = 0.5),
    portfolio_enable = TRUE,
    portfolio_top_k = 2,
    portfolio_min_cap = 1,
    portfolio_max_cap = 3,
    portfolio_max_iters = 10
  )

  expect_true(isTRUE(res$portfolio$converged) || identical(res$portfolio$violations_remaining, 0L))
  expect_true(all(res$portfolio$counts$topk_count <= res$portfolio$counts$portfolio_cap))
  expect_true(all(c("portfolio_cap", "portfolio_topk_count") %in% names(res$specialization)))
})
