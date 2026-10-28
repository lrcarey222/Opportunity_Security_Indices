# Guards the structural trade benchmark and the current trade momentum of Competitive Position
# (R/strategic_position/trade_benchmark.R, trade_momentum.R; docs/strategic_position.md section 2.2).

tm_repo_root <- function() normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(dplyr))
for (f in c("R/utils/scurve.R", "R/utils/supply_chain_stages.R", "R/strategic_position/sp_helpers.R",
            "R/strategic_position/trade_benchmark.R", "R/strategic_position/trade_momentum.R")) {
  source(file.path(tm_repo_root(), f))
}
tm_cfg <- function() yaml::read_yaml(file.path(tm_repo_root(), "config", "strategic_position.yml"))$competitive_position$trade_momentum

tm_codes <- data.frame(tech = c("Solar", "Solar", "Wind"), supply_chain = c("Assembly", "Assembly", "Components"),
                       code6 = c("854143", "850440", "850300"), stringsAsFactors = FALSE)

# Annual rows: one basket value per reporter-year for each code, plus totals.
tm_annual <- function(values, years = 2022:2024, total = 1000) {
  rows <- list()
  for (r in names(values)) for (y in years) {
    v <- values[[r]]
    rows[[length(rows) + 1]] <- data.frame(reporter_iso = r, period = y, cmd_code = c("854143", "850300", "TOTAL"),
                                           primary_value = c(v, v / 2, total), stringsAsFactors = FALSE)
  }
  do.call(rbind, rows)
}

# Monthly rows for reporters over months, basket value per month `v(reporter, month)`, totals `t`.
tm_monthly <- function(reporters, months, v = function(r, m) 10, t = function(r, m) 1000, hs = "H6") {
  rows <- list()
  for (r in reporters) for (m in months) {
    rows[[length(rows) + 1]] <- data.frame(reporter_iso = r, period = m, cmd_code = c("854143", "850300", "TOTAL"),
                                           primary_value = c(v(r, m), v(r, m), t(r, m)), classification_code = hs,
                                           net_wgt = c(v(r, m), v(r, m), NA), stringsAsFactors = FALSE)
  }
  do.call(rbind, rows)
}
months24 <- sp_month_add(202508, -23:0)
reps <- c("AAA", "BBB", "CCC", "DDD", "EEE", "FFF")

test_that("the structural benchmark averages exports, world share and RCA over the benchmark years", {
  a <- tm_annual(list(AAA = 100, BBB = 300))
  a$primary_value[a$reporter_iso == "AAA" & a$period == 2024 & a$cmd_code == "854143"] <- 400
  b <- sp_trade_benchmark(a, tm_codes, years = 2022:2024)
  s <- b[b$reporter_iso == "AAA" & b$tech == "Solar", ]
  expect_equal(s$bench_n_years, 3)
  expect_equal(s$bench_exports, (100 + 100 + 400) / 3)
  expect_equal(s$bench_share, mean(c(100 / 400, 100 / 400, 400 / 700)))
  expect_equal(s$bench_years, "2022;2023;2024")
  # RCA: (x / T) / (world x / world T), averaged over the years
  rca22 <- (100 / 1000) / (400 / 2000)
  rca24 <- (400 / 1000) / (700 / 2000)
  expect_equal(s$bench_rca, mean(c(rca22, rca22, rca24)))
})

test_that("a reporter that reported its total and no basket code exported zero, and re-export hubs are left out", {
  a <- rbind(tm_annual(list(AAA = 100)), data.frame(reporter_iso = "ZZZ", period = 2022:2024, cmd_code = "TOTAL", primary_value = 500),
             tm_annual(list(HKG = 999)))
  b <- sp_trade_benchmark(a, tm_codes, years = 2022:2024, exclude_reporters = "HKG")
  expect_equal(b$bench_exports[b$reporter_iso == "ZZZ" & b$tech == "Solar"], 0)
  expect_false("HKG" %in% b$reporter_iso)
  expect_equal(b$bench_share[b$reporter_iso == "AAA" & b$tech == "Solar"], 1)
})

test_that("too few benchmark years leave the averages missing, never zero", {
  a <- tm_annual(list(AAA = 100, BBB = 100))
  a <- a[!(a$reporter_iso == "BBB" & a$period != 2024), ]
  b <- sp_trade_benchmark(a, tm_codes, years = 2022:2024, min_years = 2)
  bbb <- b[b$reporter_iso == "BBB" & b$tech == "Solar", ]
  expect_equal(bbb$bench_n_years, 1)
  expect_true(is.na(bbb$bench_exports))
  expect_true(is.na(bbb$bench_share))
})

test_that("the benchmark index falls back to the Atlas share only for non-reporters, with no RCA", {
  b <- sp_trade_benchmark(tm_annual(list(AAA = 100, BBB = 300)), tm_codes, years = 2022:2024)
  atlas <- data.frame(iso3 = c("AAA", "QQQ"), tech = "Solar", supply_chain = "Assembly", market_share = c(0.9, 0.05))
  idx <- sp_trade_benchmark_index(b, atlas_share = atlas)
  q <- idx[idx$iso3 == "QQQ", ]
  expect_equal(q$trade_benchmark_source, "atlas_fallback")
  expect_true(is.na(q$rca_index))
  expect_equal(idx$trade_benchmark_source[idx$iso3 == "AAA" & idx$tech == "Solar"], "comtrade")
  # AAA's Comtrade share is kept, not the Atlas 0.9
  expect_equal(idx$bench_share[idx$iso3 == "AAA" & idx$tech == "Solar"], 0.25)
})

test_that("applying the benchmark replaces market_share and rca and leaves other variables alone", {
  eo <- data.frame(Country = c("A", "A", "A"), tech = "Solar", supply_chain = "Assembly", category = c("Trade", "Trade", "Investment"),
                   variable = c("market_share", "rca", "Operating Capacity Index"), component_value = c(0.1, 0.2, 0.3),
                   imputed = FALSE, stringsAsFactors = FALSE)
  idx <- data.frame(iso3 = "AAA", tech = "Solar", supply_chain = "Assembly", market_share_index = 0.8, rca_index = NA_real_)
  out <- sp_apply_trade_benchmark(eo, idx, function(x) ifelse(x == "A", "AAA", NA))
  expect_equal(out$component_value, c(0.8, NA, 0.3))
  expect_equal(out$imputed, c(FALSE, TRUE, FALSE))
})

test_that("the cutoff is the latest month at which the required share of monthly reporters has all 24 months", {
  m <- rbind(tm_monthly(reps[1:4], months24), tm_monthly(reps[5], months24[1:23]))
  cut <- sp_tm_cutoff(m, c(reps[1:5], "NOM"), window = 12, min_share = 0.8)
  expect_equal(cut$cutoff, 202508L)
  expect_equal(cut$eligible, reps[1:5])
  expect_equal(cut$no_monthly, "NOM")
  expect_equal(cut$complete, reps[1:4])
  expect_equal(cut$current, sp_month_add(202508, -11:0))
  expect_equal(cut$previous, sp_month_add(202508, -23:-12))
  # at 100% the cutoff moves back to the last month everyone covers... and with 23 months nobody has 24
  cut2 <- sp_tm_cutoff(m, reps[1:5], window = 12, min_share = 1)
  expect_true(is.na(cut2$cutoff))
  expect_equal(cut$coverage$share_eligible[cut$coverage$cutoff == 202508], 0.8)
})

test_that("a reporter without complete matched coverage gets NA momentum, never 0, and no partial-year comparison", {
  m <- rbind(tm_monthly(reps, months24, v = function(r, mo) if (mo >= 202409) 20 else 10), tm_monthly("GGG", months24[-5]))
  cut <- sp_tm_cutoff(m, c(reps, "GGG", "NOM"), min_share = 0.8)
  out <- sp_trade_momentum(m, tm_codes, cut, cfg = list(small_base_min_usd = 1))
  g <- out$cells[out$cells$iso3 == "GGG", ]
  expect_true(all(is.na(g$trade_momentum_score)))
  expect_true(all(is.na(g$trade_momentum_raw)))
  expect_true(all(g$trade_momentum_quality == "incomplete_coverage"))
  expect_equal(unique(g$trade_months_observed), 23L)
  n <- out$cells[out$cells$iso3 == "NOM", ]
  expect_true(all(n$trade_momentum_quality == "no_monthly_data"))
  expect_true(all(is.na(n$trade_momentum_score)))
  a <- out$cells[out$cells$iso3 == "AAA" & out$cells$tech == "Solar", ]
  expect_equal(a$trade_momentum_raw, log(2))
  expect_equal(a$trade_months_observed, 24L)
  expect_equal(a$trade_momentum_period, "Sep 2024 - Aug 2025")
  expect_equal(a$trade_momentum_previous_period, "Sep 2023 - Aug 2024")
})

test_that("specialization momentum nets out total-export growth, and a common price swing leaves the score unchanged", {
  grow <- c(AAA = 1, BBB = 1.2, CCC = 1.5, DDD = 2, EEE = 0.8, FFF = 0.5)
  v <- function(r, mo) if (mo >= 202409) 10 * grow[[r]] else 10
  t <- function(r, mo) if (mo >= 202409) 2000 else 1000
  m <- tm_monthly(reps, months24, v = v, t = t)
  cut <- sp_tm_cutoff(m, reps)
  cfg <- list(small_base_min_usd = 1, winsorize = c(0, 1))
  out <- sp_trade_momentum(m, tm_codes, cut, cfg = cfg)$cells
  s <- out[out$tech == "Solar", ]
  expect_equal(s$trade_momentum_specialization, s$trade_momentum_raw - log(2))
  # every exporter's current value doubles (a price swing): ranks and score unchanged
  m2 <- tm_monthly(reps, months24, v = function(r, mo) v(r, mo) * ifelse(mo >= 202409, 2, 1), t = t)
  out2 <- sp_trade_momentum(m2, tm_codes, cut, cfg = cfg)$cells
  expect_equal(out2$trade_momentum_score, out$trade_momentum_score)
  expect_true(all(out$trade_momentum_score >= 0 & out$trade_momentum_score <= 1, na.rm = TRUE))
  expect_equal(which.max(s$trade_momentum_score), which(s$iso3 == "DDD"))
})

test_that("the small-base floor excludes tiny previous-year baskets", {
  base <- c(AAA = 1e9, BBB = 2e9, CCC = 3e9, DDD = 4e9, EEE = 5e9, FFF = 1e3, GGG = 6e9)
  m <- tm_monthly(names(base), months24, v = function(r, mo) base[[r]] / 24 * ifelse(mo >= 202409, 3, 1))
  cut <- sp_tm_cutoff(m, names(base))
  out <- sp_trade_momentum(m, tm_codes, cut, cfg = list(small_base_quantile = 0.25))
  f <- out$cells[out$cells$iso3 == "FFF", ]
  expect_true(all(f$trade_momentum_quality == "small_base"))
  expect_true(all(is.na(f$trade_momentum_score)))
  fl <- out$floors$small_base_floor[out$floors$tech == "Solar"]
  expect_gte(fl, 1e6)
})

test_that("winsorisation caps extreme growth before ranking", {
  x <- c(-5, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 9)
  w <- sp_winsorize(x, c(0.1, 0.9))
  expect_lt(max(w), 9)
  expect_gt(min(w), -5)
  expect_equal(w[3:8], x[3:8])
})

test_that("an HS revision with value in headings present in one window only is flagged and not scored", {
  m <- tm_monthly(reps, months24)
  # FFF moves its solar exports to another heading and changes HS edition in the current window
  cur <- m$reporter_iso == "FFF" & m$period >= 202409
  m$classification_code[cur] <- "H7"
  m$cmd_code[cur & m$cmd_code == "854143"] <- "850440"
  cut <- sp_tm_cutoff(m, reps)
  out <- sp_trade_momentum(m, tm_codes, cut, cfg = list(small_base_min_usd = 1))
  f <- out$cells[out$cells$iso3 == "FFF" & out$cells$tech == "Solar", ]
  expect_equal(f$trade_momentum_quality, "hs_break")
  expect_true(is.na(f$trade_momentum_score))
  expect_true(out$reporters$hs_revision[out$reporters$reporter_iso == "FFF"])
})

test_that("a reporter whose monthly totals disagree with its annual total is flagged or excluded", {
  months <- sp_month_add(202512, -35:0)
  m <- tm_monthly(reps, months)
  annual_totals <- data.frame(reporter_iso = reps, year = 2024, total_exports = c(12000, 12000, 12000, 12000, 13500, 20000))
  cut <- sp_tm_cutoff(m, reps)
  out <- sp_trade_momentum(m, tm_codes, cut, annual_totals = annual_totals, cfg = list(small_base_min_usd = 1, reporter_check_year = 2024))
  q <- setNames(out$reporters$reporter_quality, out$reporters$reporter_iso)
  expect_equal(unname(q[c("AAA", "EEE", "FFF")]), c("ok", "caution", "reporter_mismatch"))
  expect_true(all(is.na(out$cells$trade_momentum_score[out$cells$iso3 == "FFF"])))
})

test_that("world-share momentum ranks exactly as log growth within a cell (diagnostic only)", {
  grow <- c(AAA = 1, BBB = 1.2, CCC = 1.5, DDD = 2, EEE = 0.8, FFF = 0.5)
  m <- tm_monthly(reps, months24, v = function(r, mo) if (mo >= 202409) 10 * grow[[r]] else 10)
  cut <- sp_tm_cutoff(m, reps)
  bench <- data.frame(reporter_iso = reps, tech = "Solar", supply_chain = "Assembly", bench_exports = 100)
  out <- sp_trade_momentum(m, tm_codes, cut, bench = bench, cfg = list(small_base_min_usd = 1, winsorize = c(0, 1)))$cells
  s <- out[out$tech == "Solar", ]
  expect_equal(rank(s$trade_momentum_share_change), rank(s$trade_momentum_raw))
  expect_equal(unique(s$trade_momentum_coverage), 1)
  # below the coverage line it is not computed
  bench2 <- rbind(bench, data.frame(reporter_iso = "BIG", tech = "Solar", supply_chain = "Assembly", bench_exports = 1e4))
  out2 <- sp_trade_momentum(m, tm_codes, cut, bench = bench2, cfg = list(small_base_min_usd = 1))$cells
  expect_true(all(is.na(out2$trade_momentum_share_change[out2$tech == "Solar"])))
})

test_that("momentum takes its configured weight of the export family where it exists, structural alone otherwise", {
  cfg <- tm_cfg()
  expect_equal(cfg$momentum_weight, 0.15)
  expect_equal(unlist(cfg$weight_sensitivity), c(0.10, 0.15, 0.20))
  cells <- data.frame(Country = c("A", "B", "C"), tech = "Solar", supply_chain = "Assembly",
                      cp_export_position = c(0.8, 0.4, 0.6), cp_investment_position = c(0.2, 0.6, NA),
                      competitive_position_score = c(0.5, 0.5, 0.6), cp_thin_cell = FALSE, stringsAsFactors = FALSE)
  tm <- data.frame(iso3 = c("AAA", "BBB"), tech = "Solar", supply_chain = "Assembly", trade_momentum_score = c(0.2, NA))
  iso <- function(x) c(A = "AAA", B = "BBB", C = "CCC")[x]
  out <- sp_cp_blend_momentum(cells, tm, iso, families = c("export_position", "investment_position"), weight = 0.15)
  expect_equal(out$cp_export_position, c(0.85 * 0.8 + 0.15 * 0.2, 0.4, 0.6))
  expect_equal(out$cp_export_position_structural, c(0.8, 0.4, 0.6))
  expect_equal(out$trade_momentum_weight, c(0.15, 0, 0))
  expect_equal(out$competitive_position_score, c((0.85 * 0.8 + 0.15 * 0.2 + 0.2) / 2, 0.5, 0.6))
  expect_false(any(is.na(out$competitive_position_percentile)))
  w0 <- sp_cp_blend_momentum(cells, tm, iso, families = c("export_position", "investment_position"), weight = 0)
  expect_equal(w0$cp_export_position, cells$cp_export_position)
})

test_that("the trade fields carry the benchmark and momentum periods", {
  m <- tm_monthly(reps, months24)
  out <- sp_trade_momentum(m, tm_codes, sp_tm_cutoff(m, reps), cfg = list(small_base_min_usd = 1))$cells
  expect_true(all(c("trade_momentum_period", "trade_momentum_previous_period", "trade_months_observed", "trade_momentum_quality",
                    "trade_momentum_coverage", "trade_momentum_raw", "trade_momentum_score") %in% names(out)))
  expect_true(all(out$trade_momentum_quality %in% SP_TM_QUALITY))
})
