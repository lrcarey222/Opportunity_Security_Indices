repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "strategic_position", "current_trade.R"))

test_that("month arithmetic crosses year ends", {
  expect_equal(ctm_month_add(202601L, -1L), 202512L)
  expect_equal(ctm_month_add(202512L, 1L), 202601L)
  expect_equal(ctm_window_months(202608L, 12L), c(202509:202512, 202601:202608))
})

test_that("windows end at each reporter's latest month and drop stale or gappy reporters", {
  da <- rbind(
    data.frame(reporter_iso = "DEU", period = ctm_window_months(202608L, 14L)),
    data.frame(reporter_iso = "KOR", period = ctm_window_months(202512L, 12L)),
    data.frame(reporter_iso = "CHN", period = ctm_window_months(202412L, 12L)),
    data.frame(reporter_iso = "GAP", period = ctm_window_months(202608L, 12L)[c(1, 3, 5, 7, 9, 11, 12)])
  )
  w <- ctm_reporter_windows(da, min_end = 202512L)
  expect_setequal(w$reporter_iso, c("DEU", "KOR"))
  expect_equal(w$window_start[w$reporter_iso == "DEU"], 202509L)
  expect_equal(w$window_end[w$reporter_iso == "KOR"], 202512L)
  d <- attr(w, "dropped")
  expect_equal(d$reason[d$reporter_iso == "CHN"], "stale")
  expect_equal(d$reason[d$reporter_iso == "GAP"], "gappy")
})

monthly_fixture <- function() {
  months <- ctm_window_months(202608L, 12L)
  grid <- expand.grid(period = months, cmd_code = sprintf("%06d", 1:10), partner_iso = c("CHN", "KOR"), stringsAsFactors = FALSE)
  grid$reporter_iso <- "USA"
  grid$flow_direction <- "import"
  grid$primary_value <- ifelse(grid$partner_iso == "CHN", 3, 1)
  # A partial April release: only two codes.
  grid <- grid[!(grid$period == 202604L & !grid$cmd_code %in% c("000001", "000002")), ]
  # A month before the window that must not count.
  extra <- grid[grid$period == 202608L, ]
  extra$period <- 202508L
  rbind(grid, extra)
}

test_that("completeness flags a partial month and T12M sums scale to twelve months", {
  m <- monthly_fixture()
  comp <- ctm_month_completeness(m)
  expect_false(comp$complete[comp$period == 202604L])
  expect_true(all(comp$complete[comp$period != 202604L]))
  w <- data.frame(reporter_iso = "USA", window_end = 202608L)
  t <- ctm_t12m_dyads(m, w, comp, min_months = 10L)
  expect_equal(unique(t$months_used), 11L)
  chn <- t[t$partner_iso == "CHN" & t$cmd_code == "000003", ]
  expect_equal(chn$primary_value, 3 * 11 * 12 / 11)
  expect_equal(unique(t$period), 9999L)
  expect_error(ctm_t12m_dyads(m, w, comp, min_months = 12L), "no reporter-flow")
})

test_that("compare keeps only own monthly reports as current and flags large China moves", {
  base <- data.frame(importer_iso = c("USA", "AUT", "IND"), tech = "Solar", supply_chain = "Assembly", observed = TRUE,
                     imports_usd = c(5e9, 1e9, 3e9), china_share = c(0.5, 0.4, 0.8), top_supplier_iso3 = c("CHN", "CHN", "CHN"),
                     top_supplier_share = c(0.5, 0.4, 0.8), import_hhi = c(0.3, 0.2, 0.65))
  current <- data.frame(importer_iso = c("USA", "AUT", "IND"), tech = "Solar", supply_chain = "Assembly", observed = TRUE,
                        import_source = c("importer", "mirror", "importer"), imports_usd = c(6e9, 9e8, 2e9),
                        china_share = c(0.45, 0.05, 0.5), top_supplier_iso3 = c("CHN", "DEU", "MYS"),
                        top_supplier_share = c(0.45, 0.5, 0.3), import_hhi = c(0.28, 0.3, 0.3))
  cmp <- ctm_exposure_compare(base, current)
  aut <- cmp[cmp$importer_iso == "AUT", ]
  expect_equal(aut$current_basis, "no_monthly_report")
  expect_true(is.na(aut$china_share_now))
  expect_false(aut$flag_china_move)
  ind <- cmp[cmp$importer_iso == "IND", ]
  expect_equal(ind$china_share_change, -0.3)
  expect_true(ind$flag_china_move)
  expect_true(ind$top_supplier_changed)
  expect_false(cmp$flag_china_move[cmp$importer_iso == "USA"])
})
