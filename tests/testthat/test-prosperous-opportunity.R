repo_root <- getwd()

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "prosperous_opportunity.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "safer_friendshore.R"))

test_that("opportunity trade_index is scaled within each reporter country", {
  ds_export <- tibble::tibble(
    reporter_iso = c("USA", "USA", "CHN", "CHN"),
    partner_iso = c("JPN", "DEU", "JPN", "DEU"),
    tech = "Solar",
    supply_chain = "Upstream",
    level_last = c(100, 200, 300, 400),
    level_first = c(80, 120, 250, 500),
    growth = c(0.25, 0.67, 0.2, -0.2)
  )

  trade_indices <- partnership_strength_build_export_indices(ds_export)

  by_reporter <- trade_indices %>%
    dplyr::group_by(reporter_iso, tech, supply_chain) %>%
    dplyr::summarize(
      min_trade_index = min(trade_index, na.rm = TRUE),
      max_trade_index = max(trade_index, na.rm = TRUE),
      .groups = "drop"
    )

  expect_equal(by_reporter$min_trade_index, rep(0, nrow(by_reporter)))
  expect_equal(by_reporter$max_trade_index, rep(1, nrow(by_reporter)))
})

test_that("friendshore imp_trade_index is scaled within each reporter country", {
  ds_import <- tibble::tibble(
    reporter_iso = c("USA", "USA", "CHN", "CHN"),
    partner_iso = c("JPN", "DEU", "JPN", "DEU"),
    tech = "Solar",
    supply_chain = "Upstream",
    level_last = c(140, 80, 500, 250),
    level_first = c(100, 100, 300, 400),
    growth = c(0.4, -0.2, 0.67, -0.38)
  )

  import_indices <- partnership_strength_build_import_indices(ds_import)

  by_reporter <- import_indices %>%
    dplyr::group_by(reporter_iso, tech, supply_chain) %>%
    dplyr::summarize(
      min_imp_trade_index = min(imp_trade_index, na.rm = TRUE),
      max_imp_trade_index = max(imp_trade_index, na.rm = TRUE),
      .groups = "drop"
    )

  expect_equal(by_reporter$min_imp_trade_index, rep(0, nrow(by_reporter)))
  expect_equal(by_reporter$max_imp_trade_index, rep(1, nrow(by_reporter)))
})
