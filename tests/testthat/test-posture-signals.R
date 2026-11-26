# Landing signals (R/dashboard/posture/posture_signals.R): export growth only where momentum quality is ok, China's
# import share rising only where flagged, the trusted network split out.

ps_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(ps_root, "R/dashboard/posture/posture_signals.R"))

ps_tbl <- function() {
  tibble::tibble(
    iso3 = c("USA", "JPN", "KAZ", "CHN", "DEU", "USA"),
    tech = c("Solar", "Solar", "Solar", "Batteries", "Batteries", "Wind"),
    supply_chain = c("Components", "Components", "Components", "Assembly", "Assembly", "Assembly"),
    strategic_posture = c("compete", "compete", "deploy", "secure", "compete", NA),
    trade_momentum_raw = c(0.4, -0.3, 2.5, 0.2, 0.15, 0.9),
    trade_momentum_quality = c("ok", "ok", "small_base", "ok", "ok", "ok"),
    home_share_exposure = c(0.05, 0.08, 0.0001, 0.6, 0.04, 0.1),
    china_share_base = c(0.30, 0.20, NA, NA, 0.10, 0.5),
    china_share_t12m = c(0.45, 0.10, NA, NA, 0.40, 0.9),
    coercive_import_share_base = c(0.32, 0.22, NA, NA, 0.12, 0.5),
    coercive_import_share_t12m = c(0.46, 0.12, NA, NA, 0.41, 0.9),
    current_trade_flag = c("china_up", "china_down", NA, NA, "china_up", "china_up"),
    exposure_current_source = c("own_monthly", "own_monthly", NA, NA, "ember_fill", "own_monthly"),
    exposure_window_end = c(202608, 202608, NA, NA, 202607, 202608)
  )
}
ps_countries <- data.frame(iso3 = c("USA", "JPN", "KAZ", "CHN", "DEU"), tier = c("core", "core", "none", "none", "core"))

test_that("export growth leaves out small bases and unassessed cells, and splits out the trusted network", {
  d <- posture_signals_doc(ps_tbl(), techs = c("Solar", "Batteries", "Wind"), countries = ps_countries, created = "x", git_commit = "y",
                           meta = list(momentum_band = 0.1, momentum_period = "Sep 2025 - Aug 2026"))
  e <- d$exports
  expect_equal(e$n_assessed, 4)          # KAZ small_base and the unassessed USA Wind cell are out
  expect_equal(e$n_up, 3)
  expect_equal(e$n_down, 1)
  # listed by world export share (growth is winsorised, so it cannot rank the top): CHN 60%, USA 5%, DEU 4%
  expect_equal(vapply(e$growing, `[[`, "", "iso3"), c("CHN", "USA", "DEU"))
  expect_equal(e$growing[[2]]$growth, round(exp(0.4) - 1, 3))
  expect_true(e$growing[[2]]$capped)                 # 0.4 is the largest ok value: at the ceiling
  expect_false(e$growing[[1]]$capped)
  expect_equal(vapply(e$growing_trusted, `[[`, "", "iso3"), c("USA", "DEU"))
  expect_equal(vapply(e$falling_trusted, `[[`, "", "iso3"), "JPN")
  expect_equal(e$period, "Sep 2025 - Aug 2026")
})

test_that("rising China dependence reads the flag, orders by the rise and keeps the coercive share beside it", {
  d <- posture_signals_doc(ps_tbl(), techs = c("Solar", "Batteries", "Wind"), countries = ps_countries,
                           treat_as_allied = character(), created = "x", git_commit = "y")
  dep <- d$dependence
  expect_equal(dep$n_china_up, 2)        # USA Wind is flagged but unassessed
  expect_equal(dep$n_china_down, 1)
  expect_equal(vapply(dep$rising_trusted, `[[`, "", "iso3"), c("DEU", "USA"))   # +30 points before +15
  expect_equal(dep$rising_trusted[[1]]$coercive_latest, 0.41)
  expect_equal(dep$window_end, 202608)
  js <- jsonlite::toJSON(d, auto_unbox = TRUE, null = "null", na = "null")
  expect_match(as.character(js), '"schema":"osi-posture-signals"')
})
