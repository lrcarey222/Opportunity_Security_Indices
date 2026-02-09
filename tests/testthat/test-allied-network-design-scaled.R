repo_root <- getwd()

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(repo_root, "R", "indices", "allied_network_design.R"))

test_that("scaled greedy solver meets target with build when feasible", {
  nodes_stage <- tibble::tibble(
    iso3c = c("USA", "CAN", "JPN", "KOR"),
    country = c("United States", "Canada", "Japan", "South Korea"),
    producer_score = c(0.8, 0.7, 0.6, 0.5),
    demand_weight = c(0.25, 0.25, 0.25, 0.25),
    dev_potential = c(0.7, 0.6, 0.5, 0.4)
  )

  edges_stage <- tidyr::crossing(
    reporter_iso = nodes_stage$iso3c,
    partner_iso = nodes_stage$iso3c
  ) %>%
    dplyr::mutate(edge_weight = ifelse(reporter_iso == partner_iso, 0.7, 0.9))

  cap0 <- c(USA = 20, CAN = 20, JPN = 20, KOR = 20)
  cap_max <- c(USA = 35, CAN = 30, JPN = 25, KOR = 20)
  build_max <- cap_max - cap0

  sol <- allied_network_solve_stage_greedy_scaled(
    nodes_stage = nodes_stage,
    edges_stage = edges_stage,
    target_total_usd = 100,
    cap0_usd = cap0,
    capMax_usd = cap_max,
    build_max_usd = build_max,
    min_producers = 3,
    max_share = 0.5,
    min_share = 0,
    min_suppliers_per_consumer = 1,
    epsilon_supplier_share = 0,
    allow_self = TRUE
  )

  expect_equal(sum(sol$flows$flow_usd), 100, tolerance = 1e-6)
  expect_lte(max(sol$specialization$production_usd - sol$specialization$capMax_exports_usd), 1e-6)
  expect_lte(max(sol$specialization$build_needed_usd - sol$specialization$build_max_usd), 1e-6)
  expect_equal(sol$unmet_demand_usd, 0, tolerance = 1e-6)
})

test_that("scaled greedy solver reports unmet demand when infeasible", {
  nodes_stage <- tibble::tibble(
    iso3c = c("USA", "CAN", "JPN", "KOR"),
    country = c("United States", "Canada", "Japan", "South Korea"),
    producer_score = c(0.8, 0.7, 0.6, 0.5),
    demand_weight = c(0.25, 0.25, 0.25, 0.25),
    dev_potential = c(0.7, 0.6, 0.5, 0.4)
  )

  edges_stage <- tidyr::crossing(
    reporter_iso = nodes_stage$iso3c,
    partner_iso = nodes_stage$iso3c
  ) %>%
    dplyr::mutate(edge_weight = ifelse(reporter_iso == partner_iso, 0.7, 0.9))

  cap0 <- c(USA = 10, CAN = 10, JPN = 10, KOR = 10)
  cap_max <- c(USA = 20, CAN = 20, JPN = 20, KOR = 20)
  build_max <- cap_max - cap0

  sol <- allied_network_solve_stage_greedy_scaled(
    nodes_stage = nodes_stage,
    edges_stage = edges_stage,
    target_total_usd = 100,
    cap0_usd = cap0,
    capMax_usd = cap_max,
    build_max_usd = build_max,
    min_producers = 3,
    max_share = 0.6,
    min_share = 0,
    min_suppliers_per_consumer = 1,
    epsilon_supplier_share = 0,
    allow_self = TRUE
  )

  expect_gt(sol$unmet_demand_usd, 0)
  expect_false(sum(cap_max) >= 100)
})
