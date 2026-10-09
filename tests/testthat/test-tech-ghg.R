# Technology GHG intensities (partnership_strength_clean_ghg) and their IPCC source.
#
# data/raw/ipcc_ghg_intensity.csv is staged from SharePoint. Its rows are lifecycle medians
# from IPCC AR5 WGIII Annex III, Table A.III.2, except oil-fired generation (720), which
# that table does not have; the cleaning step replaces it with the SRREN Annex II,
# Table A.II.4 median of 840 gCO2eq/kWh.
repo_root <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))

# The staged table as shipped (values and labels, en dashes included).
ipcc_staged <- function(oil = 720) {
  tibble::tibble(
    Tech = c(
      "Coal (pulverised)", "Natural-gas combined cycle", "Oil-fired steam", "Biomass (dedicated)",
      "Solar PV – utility scale", "Solar PV – rooftop", "Concentrated solar power",
      "Geothermal", "Hydropower (typical reservoir)", "Wind – onshore", "Wind – offshore",
      "Nuclear (light-water)"
    ),
    ghg_intensity = c(820, 490, oil, 230, 48, 41, 27, 38, 24, 11, 12, 12)
  )
}

test_that("oil-fired generation takes the IPCC SRREN lifecycle median, 840 gCO2eq/kWh", {
  expect_equal(IPCC_OIL_LIFECYCLE_MEDIAN_G_PER_KWH, 840)

  out <- partnership_strength_clean_ghg(ipcc_staged())
  idx <- stats::setNames(out$ghg_index, out$tech)

  # Oil is now the most carbon-intensive row (840 > coal's 820), so it anchors the scale at 0
  # and coal sits just above it; onshore wind (11) is the clean end.
  expect_equal(idx[["Oil"]], 0)
  expect_equal(idx[["Coal"]], (840 - 820) / (840 - 11))
  expect_equal(idx[["Gas"]], (840 - 490) / (840 - 11))
  expect_equal(idx[["Nuclear"]], (840 - 12) / (840 - 11))
  expect_gt(idx[["Coal"]], idx[["Oil"]])

  # The correction does not depend on the staged value, so a corrected file gives the same.
  fixed <- partnership_strength_clean_ghg(ipcc_staged(oil = 840))
  expect_equal(fixed, out)
})

test_that("the staged non-oil rows are the AR5 Annex III Table A.III.2 lifecycle medians", {
  path <- file.path(repo_root, "data", "raw", "ipcc_ghg_intensity.csv")
  skip_if_not(file.exists(path), "ipcc_ghg_intensity.csv is not staged")
  raw <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8")
  names(raw) <- sub("^﻿", "", names(raw))
  raw$Tech <- gsub("[–—]", "-", raw$Tech)

  # IPCC AR5 WGIII Annex III, Table A.III.2, lifecycle emissions, median (p. 1335).
  ar5_median <- c(
    "Coal (pulverised)" = 820, "Natural-gas combined cycle" = 490, "Biomass (dedicated)" = 230,
    "Solar PV - utility scale" = 48, "Solar PV - rooftop" = 41, "Concentrated solar power" = 27,
    "Geothermal" = 38, "Hydropower (typical reservoir)" = 24, "Wind - onshore" = 11,
    "Wind - offshore" = 12, "Nuclear (light-water)" = 12
  )
  staged <- stats::setNames(raw$ghg_intensity, raw$Tech)
  expect_true(all(names(ar5_median) %in% names(staged)))
  expect_equal(staged[names(ar5_median)], ar5_median)
})
