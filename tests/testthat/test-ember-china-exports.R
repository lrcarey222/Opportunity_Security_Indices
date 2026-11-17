repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "strategic_position", "ember_china_exports.R"))

skip_if_not_installed("countrycode")

ember_raw <- function(rows) {
  out <- data.frame(Area = rows$area, Date = rows$date, `Area type` = rows$type, Region = "",
                    `Commodity code` = rows$code, `Commodity category` = rows$cat,
                    `Commodity sub-category` = rows$sub, `Amount (USD)` = rows$usd,
                    `Amount 12 month rolling sum (USD)` = NA, check.names = FALSE, stringsAsFactors = FALSE)
  out
}

months_2024 <- sprintf("2024-%02d-01", 1:12)
months_latest <- c(sprintf("2025-%02d-01", 9:12), sprintf("2026-%02d-01", 1:8))
fixture <- function() {
  grid <- expand.grid(date = c(months_2024, months_latest), area = c("Germany", "Micronesia"),
                      code = c("850760", "85044030", "841510"), stringsAsFactors = FALSE)
  grid$usd <- ifelse(grid$date %in% months_2024, 10, 20)
  grid <- rbind(grid, data.frame(date = "2026-08-01", area = "World", code = "850760", usd = 1e6))
  grid$type <- ifelse(grid$area == "World", "Region", "Country or economy")
  cats <- c(`850760` = "Batteries", `85044030` = "Grid", `841510` = "Heating and cooling systems")
  subs <- c(`850760` = "Lithium ion batteries", `85044030` = "Inverters", `841510` = "AC units")
  grid$cat <- unname(cats[grid$code])
  grid$sub <- unname(subs[grid$code])
  ember_raw(grid)
}
basket <- data.frame(tech = c("Batteries", "Electric Vehicles", "Solar", "Solar"),
                     stage = c("Assembly", "Components", "Assembly", "Deployment"),
                     hs6 = c("850760", "850760", "850440", "850440"), stringsAsFactors = FALSE)

test_that("prepare resolves ISO3, keeps regions apart and flags 8-digit codes", {
  p <- ember_cte_prepare(fixture())
  expect_setequal(unique(p$iso3[!p$is_region]), c("DEU", "FSM"))
  expect_true(all(is.na(p$iso3[p$is_region])))
  expect_true(all(p$partial_hs6[p$code == "85044030"]))
  expect_false(any(p$partial_hs6[p$code == "850760"]))
  expect_equal(unique(p$hs6[p$code == "85044030"]), "850440")
  modules <- ember_cte_prepare(ember_raw(data.frame(area = "Germany", date = "2024-01-01", type = "Country or economy",
                                                    code = "85414300", cat = "Solar PV", sub = "Solar panels", usd = 1)))
  expect_false(modules$partial_hs6)
  expect_equal(modules$hs6, "854143")
  bad <- fixture()
  bad$`Amount (USD)` <- NULL
  expect_error(ember_cte_prepare(bad), "Amount \\(USD\\)")
})

test_that("code cells attribute a code to every basket cell, drop Deployment and report unmatched codes", {
  cc <- ember_cte_code_cells(ember_cte_prepare(fixture()), basket)
  expect_false("Deployment" %in% cc$stage)
  expect_equal(sort(cc$tech[cc$code == "850760"]), c("Batteries", "Electric Vehicles"))
  expect_equal(unique(cc$n_cells[cc$code == "850760"]), 2L)
  expect_equal(cc$tech[cc$code == "85044030"], "Solar")
  expect_equal(attr(cc, "unmatched")$code, "841510")
})

test_that("windows sum the latest months and the base year, and growth follows", {
  p <- ember_cte_prepare(fixture())
  w <- ember_cte_code_windows(p, base_year = 2024L)
  expect_false(any(is.na(w$iso3)))
  deu <- w[w$iso3 == "DEU" & w$code == "850760", ]
  expect_equal(deu$usd_latest, 240)
  expect_equal(deu$usd_base, 120)
  expect_equal(deu$latest_end, "2026-08")
  cells <- ember_cte_cells(w, ember_cte_code_cells(p, basket))
  b <- cells[cells$iso3 == "DEU" & cells$tech == "Batteries", ]
  expect_equal(b$growth, 2)
  expect_false(b$any_partial_hs6)
  expect_true(cells$any_partial_hs6[cells$iso3 == "DEU" & cells$tech == "Solar"])
  expect_error(ember_cte_code_windows(p, end = "2027-01"), "no data")
})

test_that("compare uses full HS6 codes only and keeps one-sided rows", {
  p <- ember_cte_prepare(fixture())
  w <- ember_cte_code_windows(p, base_year = 2024L)
  cc <- ember_cte_code_cells(p, basket)
  ct <- data.frame(reporter_iso = c("DEU", "DEU", "FRA"), hs6 = c("850760", "850440", "850760"),
                   imports_from_china = c(150, 999, 50))
  cmp <- ember_cte_compare(w, cc, ct)
  expect_false("850440" %in% cmp$hs6)
  expect_equal(cmp$ratio[cmp$iso3 == "DEU"], 120 / 150)
  expect_true("FRA" %in% cmp$iso3)
  expect_equal(cmp$ember_usd[cmp$iso3 == "FRA"], 0)
  expect_false(cmp$comtrade_reported[cmp$iso3 == "FSM"])
  mod_raw <- ember_raw(data.frame(area = "Germany", date = months_2024, type = "Country or economy",
                                  code = "85414300", cat = "Solar PV", sub = "Solar panels", usd = 5))
  mp <- ember_cte_prepare(mod_raw)
  mcc <- ember_cte_code_cells(mp, data.frame(tech = "Solar", stage = "Assembly", hs6 = "854143"))
  mcmp <- ember_cte_compare(ember_cte_code_windows(mp, end = "2024-12"), mcc,
                            data.frame(reporter_iso = "DEU", hs6 = "854143", imports_from_china = 60))
  expect_equal(mcmp$hs6, "854143")
  expect_equal(mcmp$ratio, 1)
  s <- ember_cte_compare_summary(cmp)
  expect_equal(s$n_reporters, 1L)
  expect_equal(s$ratio_total, 120 / 150)
})

test_that("nowcast scales China by Ember growth and holds other suppliers flat", {
  base <- data.frame(iso3 = c("DEU", "FRA"), tech = "Batteries", stage = "Assembly", china_usd = c(50, 10), total_usd = c(100, 0))
  cells <- data.frame(iso3 = c("DEU", "FRA"), tech = "Batteries", stage = "Assembly", growth = c(2, 3), latest_end = "2026-08")
  n <- ember_cte_nowcast_share(base, cells)
  deu <- n[n$iso3 == "DEU", ]
  expect_equal(deu$share_base, 0.5)
  expect_equal(deu$share_now, 100 / 150)
  expect_equal(deu$share_change, 100 / 150 - 0.5)
  expect_true(is.na(n$share_now[n$iso3 == "FRA"]))
})
