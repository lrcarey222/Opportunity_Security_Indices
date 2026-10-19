repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "scripts", "utils", "comtrade_monthly_utils.R"))

test_that("month arithmetic crosses year boundaries", {
  expect_equal(comtrade_month_add(202601, -1), 202512L)
  expect_equal(comtrade_month_add(202512, 1), 202601L)
  expect_equal(comtrade_month_add(202607, -11), 202508L)
  expect_equal(comtrade_month_window(202602, 3), c(202512L, 202601L, 202602L))
})

test_that("each reporter keeps its own 12-month window and missing reporters are reported", {
  da <- data.frame(
    reporter_iso = c(rep("USA", 19), rep("UKR", 20)),
    period = c(comtrade_month_window(202607, 19), comtrade_month_window(202509, 21)[-5])
  )
  da <- rbind(da, data.frame(reporter_iso = "UKR", period = 202606L))  # a stray later month counts
  w <- comtrade_monthly_windows(da, c("USA", "UKR", "AUT"))
  expect_equal(w$last_month[w$reporter_iso == "USA"], 202607L)
  expect_equal(w$first_month[w$reporter_iso == "USA"], 202508L)
  expect_equal(w$n_missing[w$reporter_iso == "USA"], 0L)
  expect_equal(w$last_month[w$reporter_iso == "UKR"], 202606L)
  expect_true(is.na(w$last_month[w$reporter_iso == "AUT"]))
  expect_equal(w$reason[w$reporter_iso == "AUT"], "no monthly data")
})

test_that("the basket drops legacy bridge codes and pads codes", {
  m <- data.frame(hs6 = c("850760", "854140", "28530"), legacy_hs = c("FALSE", "TRUE", "FALSE"))
  extra <- data.frame(hs6 = c("841861", "850760"))
  expect_equal(comtrade_monthly_basket(list(m, extra)), c("028530", "841861", "850760"))
})

test_that("the plan groups reporters by window end, flow and code group", {
  w <- data.frame(reporter_iso = c("USA", "JPN", "FRA", "AUT"), first_month = c(202508L, 202509L, 202501L, NA),
                  last_month = c(202607L, 202608L, 202512L, NA), n_missing = 0L, reason = "")
  p <- comtrade_monthly_plan(w, n_code_groups = 2L, max_reporters = 15L)
  expect_equal(nrow(p), 3 * 2 * 2)
  expect_false(any(grepl("AUT", p$reporters)))
  expect_true(all(p$last_month - p$first_month %in% c(99L, 11L)))  # 12 months: same year or across years
  expect_equal(unique(p$first_month[p$last_month == 202512L]), 202501L)
  expect_equal(length(unique(p$chunk_id)), nrow(p))
})

test_that("trim keeps only months inside each reporter's window", {
  w <- data.frame(reporter_iso = c("USA", "FRA"), first_month = c(202508L, 202501L), last_month = c(202607L, 202512L))
  df <- data.frame(reporter_iso = c("USA", "USA", "FRA", "FRA"), period = c("202507", "202607", "202512", "202601"))
  out <- comtrade_monthly_trim(df, w)
  expect_equal(out$period, c("202607", "202512"))
})
