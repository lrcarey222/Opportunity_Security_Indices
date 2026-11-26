source(file.path(testthat::test_path("..", ".."), "R", "dashboard", "sector", "sector_dims.R"))

test_that("sector_geo groups treat_as_allied economies with the trusted partners (one allied set)", {
  skip_if_not_installed("countrycode")
  ci <- data.frame(
    iso3c = c("USA", "IDN", "CHE", "CHN", "LKA"),
    country = c("United States", "Indonesia", "Switzerland", "China", "Sri Lanka"),
    region = "X", income = "X", longitude = NA, latitude = NA, stringsAsFactors = FALSE
  )
  coal <- data.frame(
    iso3 = c("USA", "IDN"), country = c("United States", "Indonesia"), tier = c("core", "partner"),
    eu_member = FALSE, reason = c("r1", "r2"), stringsAsFactors = FALSE
  )
  ideal <- data.frame(iso3 = character(), bloc = character(), s_axis = double(), alignment_basis = character())
  extra <- data.frame(code = "TWN", name = "Taiwan", stringsAsFactors = FALSE)
  g <- sector_geo(ci, coal, ideal, hubs = character(), extra = extra, treat_as_allied = c("TWN", "CHE"))
  grp <- setNames(g$group, g$code)
  expect_equal(unname(grp[c("USA", "IDN", "CHE", "TWN", "CHN", "LKA")]), c("core", "partner", "partner", "partner", "china", "rest"))
  tier <- setNames(g$tier, g$code)
  expect_equal(unname(tier[c("CHE", "TWN")]), c("none", "none"))
  g0 <- sector_geo(ci, coal, ideal, hubs = character(), extra = extra)
  expect_equal(g0$group[g0$code == "CHE"], "rest")
})
