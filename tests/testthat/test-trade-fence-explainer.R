repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "strategic_position", "trade_restrictiveness.R"))
source(file.path(repo_root, "R", "trade_fence_explainer", "explainer_data.R"))

test_that("audit verdicts map to the four explainer verdicts", {
  v <- tfe_verdict4(c("In line", "Taxes trusted partners", "Fence above recommendation", "No tariff fence", "Partial fence", "Not assessed"))
  expect_equal(v, c(TFE_VERDICTS[1:4], TFE_VERDICTS[4], NA))
})

test_that("dollar weighting counts each HS6 once and leaves Deployment out", {
  cells <- data.frame(tech = c("A", "A", "B", "A"), stage = c("Assembly", "Refining", "Assembly", "Deployment"),
                      hs6 = c("111111", "222222", "111111", "111111"))
  verdicts <- data.frame(tech = c("A", "A", "B", "A"), stage = c("Assembly", "Refining", "Assembly", "Deployment"),
                         verdict4 = c("Taxes trusted partners", "Aligned", "Fence above recommendation", "Fence above recommendation"))
  goods <- data.frame(hs6 = c("111111", "111111", "222222"), group = c("Trusted", "Coercive", "Trusted"), w = c(60, 40, 50))
  d <- tfe_dollar_weighted(goods, cells, verdicts)
  ded <- d[d$basis == "dedup_equal_split" & d$suppliers == "All suppliers", ]
  expect_equal(sum(ded$imports_usd), 150)
  expect_equal(ded$imports_usd[ded$verdict == "Taxes trusted partners"], 50)
  expect_equal(ded$imports_usd[ded$verdict == "Fence above recommendation"], 50)
  gross <- d[d$basis == "gross" & d$suppliers == "All suppliers", ]
  expect_equal(sum(gross$imports_usd), 350)
  tr <- d[d$basis == "dedup_equal_split" & d$suppliers == "Trusted partners", ]
  expect_equal(sum(tr$imports_usd), 110)
  dep <- d[d$basis == "dedup_deployment" & d$suppliers == "All suppliers", ]
  expect_equal(dep$imports_usd[dep$verdict == "Fence above recommendation"], 100)
})

test_that("scenario rates add the statutory change, floor at the 2024 rate and cap", {
  expect_equal(tfe_scenario_tau(20, 2, 15, 0), 35)
  expect_equal(tfe_scenario_tau(20, 2, -30, 0), 2)
  expect_equal(tfe_scenario_tau(1, 2, -5, 0), 1)
  expect_equal(tfe_scenario_tau(NA, 3, 10, 0), 13)
  expect_equal(tfe_scenario_tau(50, 0, 40, 30), 100)
})

test_that("group rates add a coercive-excluding-China average", {
  x <- data.frame(group = c("Trusted", "Coercive", "Coercive", "Nonaligned"), iso3 = c("JPN", "CHN", "VNM", "MEX"),
                  effective_rate = c(5, 20, 10, 3), imports_usd = c(1, 3, 1, 2))
  g <- tfe_group_rates(x)
  expect_equal(g$effective_rate[g$group == "Coercive"], (60 + 10) / 4)
  expect_equal(g$effective_rate[g$group == "Coercive excl. China"], 10)
})

source(file.path(repo_root, "R", "trade_fence_explainer", "trade_overview.R"))

test_that("stage weights split a code equally across the stages it sits in", {
  pc <- data.frame(tech = c("A", "B", "A"), stage = c("Assembly", "Components", "Refining"), hs6 = c("111111", "111111", "222222"))
  w <- tfe_stage_weights(pc)
  expect_equal(sum(w$weight[w$hs6 == "111111"]), 1)
  expect_equal(w$weight[w$hs6 == "222222"], 1)
})

test_that("sourcing shares sum to one and HHI ignores the unnamed residual", {
  pm <- data.frame(partner = c("China", "Japan", "Mexico", "Rest of world"), iso3 = c("CHN", "JPN", "MEX", "ROW"),
                   group = c("Coercive", "Trusted", "Nonaligned", "Unassigned (rest of world)"), hs6 = "111111", value = c(40, 30, 20, 10))
  s <- tfe_sourcing(pm, "111111")
  expect_equal(s$china_share + s$coercive_excl_china_share + s$trusted_share + s$nonaligned_share + s$unassigned_share, 1)
  expect_equal(s$hhi, 0.4^2 + 0.3^2 + 0.2^2)
  expect_equal(s$top3_share, 0.9)
  expect_equal(s$north_america_share, 0.2)
  expect_equal(s$n_suppliers_1pct, 3)
})

test_that("quadrants follow import growth and China-share change", {
  expect_equal(tfe_quadrant(c(10, -10, 10, -10), c(-1, -1, 1, 1)),
               c("Diversification during expansion", "Diversification through contraction / substitution",
                 "Deepening dependence", "Concentration despite contraction"))
})
