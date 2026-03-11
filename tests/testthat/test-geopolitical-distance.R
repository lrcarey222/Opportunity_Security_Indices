testthat::test_that("IPD and normalization formulas work", {
  source("R/geopolitical_distance/core.R")
  th <- data.frame(iso3 = c("USA", "CHN", "JPN"), country = c("United States", "China", "Japan"), year = 2021,
                   theta = c(1, -1, 0.5), theta_se = c(0.1, 0.1, 0.1), spec_name = "s")
  ipd <- compute_ipd_matrix(th, 2021, "s")
  testthat::expect_true(all(ipd$gp_norm >= 0 & ipd$gp_norm <= 1))
  testthat::expect_equal(ipd$ipd[ipd$iso3_i == "USA" & ipd$iso3_j == "CHN"], 2)
})

testthat::test_that("seg and bloc assignment are consistent", {
  source("R/geopolitical_distance/core.R")
  ipd <- data.frame(
    iso3_i = c("USA", "USA", "CHN", "CHN", "AAA", "AAA"),
    iso3_j = c("USA", "CHN", "USA", "CHN", "USA", "CHN"),
    year = 2021,
    ipd = c(0, 2, 2, 0, 0.2, 1.8),
    gp_norm = c(0, 1, 1, 0, 0.1, 0.9),
    spec_name = "s"
  )
  cm <- data.frame(iso3 = c("USA", "CHN", "AAA"), country = c("United States", "China", "Aland"))
  seg <- compute_seg_scores(ipd, cm, 2021, "s")
  testthat::expect_equal(seg$seg[seg$iso3 == "USA"], 1)
  testthat::expect_equal(seg$seg[seg$iso3 == "CHN"], -1)
  blocs <- assign_blocs(seg)
  testthat::expect_true(all(blocs$bloc %in% c("U.S.-leaning", "China-leaning", "Nonaligned")))
})
