# Concentration summaries, world totals, coverage glyph and headlines of the Sector Insights bundle
# (R/dashboard/sector/sector_summary.R) on hand-computed cases. src/sector/derive/concentration.ts
# mirrors these rules.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "dashboard", "sector", "sector_files.R"))
for (f in sector_files()) source(file.path(repo_root, f))

test_that("shares, HHI, effective number, top-1 / top-3 and the group split on a listed sum", {
  cc <- sector_concentration(c(50, 30, 20), geo_idx = c(3L, 1L, 2L), groups = c("china", "core", "partner"))
  expect_identical(cc$W, 100)
  expect_identical(cc$top1, 3L)
  expect_identical(cc$top1_share, 0.5)
  expect_identical(cc$top3_share, 0.5 + 0.3 + 0.2)
  expect_equal(cc$hhi, 0.38, tolerance = 1e-15)
  expect_identical(cc$eff_n, 1 / cc$hhi)
  expect_identical(cc$hhi_lower_bound, FALSE)
  expect_identical(c(cc$china, cc$core, cc$partner, cc$rest), c(0.5, 0.3, 0.2, 0))
  expect_identical(cc$unattributed, 1 - (0.5 + 0.3 + 0.2))
  expect_identical(cc$n, 3L)
})

test_that("a source world total leaves an unattributed residual and makes the HHI a lower bound", {
  cc <- sector_concentration(c(50, 30, 20), geo_idx = 1:3, groups = c("china", "core", "rest"), W = 200)
  expect_identical(cc$top1_share, 0.25)
  expect_equal(cc$hhi, 0.0625 + 0.0225 + 0.01, tolerance = 1e-15)
  expect_equal(cc$unattributed, 0.5, tolerance = 1e-15)
  expect_true(cc$hhi_lower_bound)
  expect_equal(cc$china + cc$core + cc$partner + cc$rest + cc$unattributed, 1, tolerance = 1e-15)
  # 5 points or fewer unattributed: not a lower bound.
  expect_false(sector_concentration(c(50, 46), 1:2, c("core", "core"), W = 100)$hhi_lower_bound)
  expect_true(sector_concentration(c(50, 44), 1:2, c("core", "core"), W = 100)$hhi_lower_bound)
})

test_that("a listed sum above the world total is clamped to no residual and reported", {
  cc <- sector_concentration(c(60, 40), 1:2, c("china", "core"), W = 99)
  expect_identical(cc$unattributed, 0)
  expect_equal(cc$clamped, 100 / 99 - 1, tolerance = 1e-12)
  expect_false(cc$hhi_lower_bound)
})

test_that("a mixed region (group na) is a holder, but its share is not attributed to a group", {
  # IEA EV regions: China, EU (core) and a mixed Other Asia Pacific (repair review 2026-10-11).
  cc <- sector_concentration(c(70, 20, 10), 1:3, c("china", "core", "na"))
  expect_identical(cc$top1, 1L)
  expect_identical(cc$n, 3L)
  expect_equal(cc$hhi, 0.49 + 0.04 + 0.01, tolerance = 1e-15)
  expect_identical(c(cc$china, cc$core, cc$partner, cc$rest), c(0.7, 0.2, 0, 0))
  expect_equal(cc$unattributed, 0.1, tolerance = 1e-15)
  expect_false(cc$hhi_lower_bound)
  expect_equal(cc$china + cc$core + cc$partner + cc$rest + cc$unattributed, 1, tolerance = 1e-15)
})

test_that("ties break by the dims.geo row order; zeros are holders but not counted in n", {
  cc <- sector_concentration(c(10, 10, 5, 0), geo_idx = c(5L, 2L, 9L, 1L), groups = rep("rest", 4))
  expect_identical(cc$top1, 2L)
  expect_identical(cc$n, 3L)
  expect_identical(cc$top3_share, 1)
  empty <- sector_concentration(numeric(), integer(), character())
  expect_true(is.na(empty$hhi))
  expect_true(is.na(sector_concentration(c(0, 0), 1:2, c("core", "rest"))$top1_share))
})

summary_geo <- function() {
  tibble::tibble(
    code = c("CHN", "DEU", "FRA", "USA", "IDN", "EUU", "WLD", "R:iea_ev:china", "R:iea_ev:european-union", "RES:ei"),
    kind = c("country", "country", "country", "country", "country", "eu", "world", "region", "region", "residual"),
    group = c("china", "core", "core", "core", "partner", "core", "na", "china", "core", "na"),
    eu_member = c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )
}
summary_vars <- function() {
  tibble::tibble(
    id = c("production.generation", "investment.annual", "production.ev_region", "prices.wages"),
    agg = c("sum", "sum", "sum", "none"),
    world_basis = c("source_world", "sum_all", "sum_listed", "none"),
    native_level = c("country", "country", "region", "country")
  )
}
fact <- function(var, item, geo, year, value, status = "obs") {
  tibble::tibble(var = var, item_key = item, geo = geo, year = as.integer(year), value = value, status = status)
}

test_that("holders: the measure's own level only, never residuals or nq / na_growth rows", {
  facts <- dplyr::bind_rows(
    fact("production.generation", "fuel:solar", c("CHN", "DEU", "FRA", "USA", "IDN", "RES:ei"), 2025, c(800, 60, 25, 300, NaN, 115), c(rep("obs", 4), "nq", "obs")),
    fact("production.generation", "fuel:solar", "EUU", 2025, 100),
    fact("production.ev_region", "cell:electric-vehicles-midstream", c("R:iea_ev:china", "R:iea_ev:european-union", "USA"), 2024, c(12, 3, 1)),
    fact("prices.wages", "activity:midstream", c("CHN", "USA"), 2025, c(1200, 6000))
  )
  world <- tibble::tibble(var = "production.generation", item_key = "fuel:solar", year = 2025L, value = 1300, basis = "source_world")
  s <- sector_summary(facts, world, summary_geo(), summary_vars())
  # wages have no world basis: not summarised
  expect_false("prices.wages" %in% s$var)
  m <- s[s$var == "production.generation" & s$eu_mode == "members", ]
  expect_identical(m$W, 1300)
  expect_identical(m$basis, "source_world")
  expect_identical(m$n, 4L)
  expect_identical(m$china, 800 / 1300)
  expect_equal(m$core, (60 + 25 + 300) / 1300, tolerance = 1e-15)
  expect_equal(m$unattributed, 1 - (800 + 60 + 25 + 300) / 1300, tolerance = 1e-15)
  expect_true(m$hhi_lower_bound)
  # EI's own EU row replaces the members in bloc mode.
  b <- s[s$var == "production.generation" & s$eu_mode == "bloc", ]
  expect_identical(b$n, 3L)
  expect_equal(b$core, (300 + 100) / 1300, tolerance = 1e-15)
  # A region measure ranks regions only; without a world row the basis is the listed sum.
  r <- s[s$var == "production.ev_region" & s$eu_mode == "members", ]
  expect_identical(r$W, 15)
  expect_identical(r$basis, "sum_listed")
  expect_identical(r$n, 2L)
  expect_identical(r$top1_geo, 8L)
})

test_that("bloc mode without an own EU row sums the members; growth is a CAGR with a 1% floor", {
  yrs <- 2018:2025
  facts <- dplyr::bind_rows(
    fact("investment.annual", "cell:batteries-midstream", "CHN", yrs, c(5, 6, 7, 8, 30, 33, 34, 33)),
    fact("investment.annual", "cell:batteries-midstream", "DEU", yrs, c(1, 1, 2, 3, 4, 4, 3, 2)),
    fact("investment.annual", "cell:batteries-midstream", "FRA", yrs, c(0, 0, 0, 1, 1, 2, 2, 3)),
    fact("investment.annual", "cell:batteries-midstream", "USA", yrs, c(2, 2, 3, 6, 20, 25, 30, 28.5)),
    fact("investment.annual", "cell:wind-midstream", "USA", yrs, c(0.001, 0, 0, 0, 0.5, 0.6, 0.7, 0.8)),
    fact("investment.annual", "cell:solar-midstream", "USA", yrs, c(0, 0, 0, 0, 0, 1, 2, 3))
  )
  world <- tibble::tibble(var = character(), item_key = character(), year = integer(), value = double(), basis = character())
  s <- sector_summary(facts, world, summary_geo(), summary_vars())
  b25 <- s[s$item_key == "cell:batteries-midstream" & s$year == 2025 & s$eu_mode == "bloc", ]
  expect_identical(b25$n, 3L)
  expect_identical(b25$W, 33 + 2 + 3 + 28.5)
  expect_equal(b25$core, (2 + 3 + 28.5) / b25$W, tolerance = 1e-15)
  m25 <- s[s$item_key == "cell:batteries-midstream" & s$year == 2025 & s$eu_mode == "members", ]
  m18 <- s[s$item_key == "cell:batteries-midstream" & s$year == 2018 & s$eu_mode == "members", ]
  expect_identical(m25$growth_kind, "cagr_2018_2025")
  expect_identical(m25$growth, (m25$W / m18$W)^(1 / 7) - 1)
  expect_true(is.na(m18$growth))
  expect_true(is.na(m18$growth_kind))
  # A 2018 base below 1% of 2025 or a zero base reads "new" (NA).
  w25 <- s[s$item_key == "cell:wind-midstream" & s$year == 2025 & s$eu_mode == "members", ]
  expect_true(is.na(w25$growth))
  expect_identical(w25$growth_kind, "cagr_2018_2025")
  expect_true(is.na(s$growth[s$item_key == "cell:solar-midstream" & s$year == 2025 & s$eu_mode == "members"]))
})

test_that("every summarised slice gets a world row; a missing source world total is reported", {
  facts <- dplyr::bind_rows(
    fact("production.generation", "fuel:wind", c("CHN", "USA"), 2025, c(1000, 450)),
    fact("investment.annual", "cell:wind-midstream", c("CHN", "USA", "RES:ei"), 2025, c(10, 5, 99))
  )
  cw <- sector_complete_world(facts, tibble::tibble(var = character(), item_key = character(), year = integer(), value = double(), basis = character()),
                              summary_vars(), summary_geo())
  w <- cw$world
  expect_identical(w$value[w$var == "investment.annual"], 15)
  expect_identical(w$basis[w$var == "investment.annual"], "sum_all")
  expect_identical(w$basis[w$var == "production.generation"], "sum_listed")
  expect_identical(cw$missing, "production.generation fuel:wind 2025")
})

test_that("the coverage glyph counts the lens families only", {
  items <- tibble::tibble(axis = "cell", slug = c("electric-grid-downstream", "batteries-midstream"), stage = c("Downstream", "Midstream"),
                          headline = TRUE, item_key = c("cell:electric-grid-downstream", "cell:batteries-midstream"))
  coverage <- tibble::tibble(
    item_key = c(rep("cell:electric-grid-downstream", 3), rep("cell:batteries-midstream", 2)),
    family = c("policy", "context", "technology", "exports", "policy"),
    status = c("country", "country", "global", "country", "country")
  )
  g <- sector_coverage_glyph(items, coverage)
  expect_identical(vapply(g, `[[`, logical(1), "country_data"), c(FALSE, TRUE))
  expect_identical(g[[1]]$tech, "electric-grid")
  expect_identical(g[[1]]$stage, "downstream")
})

test_that("headlines follow the contract's rules and keys", {
  dict <- sector_dictionary_tables(yaml::read_yaml(file.path(repo_root, "config", "sector_dictionary.yml")), fill = sector_synthetic_fill())
  parts <- sector_synthetic_parts(dict)
  have <- unique(sector_data_keys(parts, parts$geo)$var)
  b <- sector_assemble(parts, dict, ship_vars = dict$vars$id[dict$vars$id %in% have])
  keys <- list(
    supply = list(values = c("share", "value_kt", "total_kt"), labels = c("geo", "geo_name", "item", "item_text")),
    concentration = list(values = c("n", "n_total"), labels = character()),
    dependence = list(values = c("share", "total_usd"), labels = c("cell", "cell_label")),
    investment = list(values = c("total_bn", "china", "coalition"), labels = character()),
    reserves = list(values = "share", labels = c("geo", "geo_name", "item", "item_text")),
    energy = list(values = c("exporter_ej", "importer_ej"), labels = c("exporter", "exporter_name", "importer", "importer_name")),
    policy = list(values = "n", labels = character()),
    prices = list(values = "vol", labels = c("item", "item_text"))
  )
  ids <- vapply(b$headlines, `[[`, "", "id")
  expect_setequal(ids, names(keys))
  for (h in b$headlines) {
    expect_identical(h$template, paste0(h$id, "@1"))
    expect_setequal(names(h$values), keys[[h$id]]$values)
    expect_setequal(names(h$labels), keys[[h$id]]$labels)
    expect_true(all(vapply(h$values, function(v) is.numeric(v) && length(v) == 1 && is.finite(v), logical(1))), info = h$id)
    expect_match(h$href, "^/sector")
  }
  dep <- b$headlines[[match("dependence", ids)]]
  s <- b$summary[b$summary$var == "trade.exports" & b$summary$item_key == "cell:batteries-midstream" & b$summary$year == 2024 &
                   b$summary$eu_mode == "members", ]
  expect_identical(dep$values$share, s$china)
  expect_identical(dep$values$total_usd, s$W)
  expect_identical(dep$labels$cell_label, "Batteries — Midstream")
  expect_identical(sector_lower_first(c("Natural gas, EU (TTF)", "PGMs", "LNG, Asia", NA)), c("natural gas, EU (TTF)", "PGMs", "LNG, Asia", NA))
})
