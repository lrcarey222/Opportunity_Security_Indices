# Country Insights derivation rules (spec_country.md A9, A12): percentiles and medians, the plotted
# position, quadrants (against the country's own median sector, 2026-10-15), Strategic Index level terms and the priority
# without policy, thirds and agreement classes (ties, no variation), drivers (top selection,
# constant-in-cell measures never top, combined missing-rule words, labels), IEA policy record counts
# with expanded cross-cutting rows, the Climate Action Tracker EU-bloc scope, partner reasons and
# Development Potential words, trade means (never a sum across baskets), the agreements rule, the
# industrial-policy measures (dates, the active rule, top five per sector) and the display formatter.
# Pure helpers only; base R + jsonlite + yaml.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "dashboard", "country", "country_files.R"))
for (f in country_helper_files()) source(file.path(repo_root, f))
cfg <- yaml::read_yaml(file.path(repo_root, "config", "country_insights.yml"))

test_that("percentiles count the others below and half of the ties; medians are over S_c", {
  expect_equal(country_percentile(c(1, 2, 3, 4), 2.5), 50)
  expect_equal(country_percentile(c(1, 2, 2, 3), 2), 50)          # 1 below + 0.5 x 2 equal of 4
  expect_equal(country_percentile(c(5), 5), 50)                   # n = 1, tied
  expect_equal(country_percentile(c(5), 6), 100)
  expect_true(is.na(country_percentile(numeric(), 1)))            # |O| = 0
  expect_true(is.na(country_percentile(c(1, 2), NA)))
  # even n: the median is the mean of the two middle values, as views/Space/derive.ts median()
  es <- data.frame(iso3 = c("A", "B", "C", "D"), key = "oil-upstream", value = c(0.1, 0.2, 0.3, 0.4), rankable = TRUE)
  eo <- data.frame(iso3 = c("A", "B", "C", "D"), key = "oil-upstream", value = c(0.4, 0.3, 0.2, 0.1), rankable = c(TRUE, TRUE, TRUE, FALSE))
  st <- country_cell_stats(es, eo, keys = "oil-upstream")[["oil-upstream"]]
  expect_setequal(st$iso3, c("A", "B", "C"))                      # D is not rankable on EO: outside S_c
  expect_equal(st$es_med, 0.2)
  expect_equal(st$eo_med, 0.3)
})

test_that("the plotted position sits on the side of the median its value is on", {
  # [1, 2, 2, 3, 4], x = 2: pct 37.5 but x >= median 2 -> pos 50
  others <- c(1, 2, 3, 4)
  pct <- country_percentile(others, 2)
  expect_equal(pct, 37.5)
  expect_equal(country_pos(pct, 2, 2), 50)
  expect_equal(country_pos(80, 3, 2), 80)
  expect_equal(country_pos(60, 1.9, 2), 49.5)
  expect_equal(country_pos(20, 1, 2), 20)
})

test_that("quadrants: at or above the split is high (ties go high)", {
  expect_equal(country_quadrant(0.5, 0.5, 0.5, 0.5), "grow")
  expect_equal(country_quadrant(0.4, 0.6, 0.5, 0.5), "upgrade")
  expect_equal(country_quadrant(0.4, 0.4, 0.5, 0.5), "secure")
  expect_equal(country_quadrant(0.6, 0.4, 0.5, 0.5), "watch")
})

test_that("corners split at the country's own median sector: the median of the plotted positions (2026-10-15)", {
  cell <- function(k, es_pos, eo_pos, es_v = 0.5, es_med = 0.4, placed = TRUE) list(
    key = k, placed = placed, quadrant = NULL,
    es = list(v = es_v, pos = es_pos, med = list(v = es_med)), eo = list(v = 0.5, pos = eo_pos, med = list(v = 0.4)))
  cells <- list(cell("a", 90, 80), cell("b", 70, 20), cell("c", 30, 60), cell("d", 10, 40), cell("e", 50, 50),
                cell("x", 100, 100, placed = FALSE))
  m <- country_median_sector(cells)
  expect_equal(m, list(es = 50, eo = 50))                              # over the placed sectors only
  q <- country_set_quadrants(cells)
  expect_equal(vapply(q$cells[1:5], function(c) c$quadrant, ""), c("grow", "watch", "upgrade", "secure", "grow"))  # e ties: high on both
  expect_null(q$cells[[6]]$quadrant)
  expect_true("quadrant" %in% names(q$cells[[6]]))                     # kept as null for the JSON
  # an even number of sectors: the mean of the two middle positions
  expect_equal(country_median_sector(cells[1:4]), list(es = 50, eo = 50))
  # the earlier rule (sector_median): the score against the sector's median economy, whatever the positions
  expect_equal(vapply(country_set_quadrants(cells, "sector_median")$cells[1:5], function(c) c$quadrant, ""), rep("grow", 5))
  expect_equal(country_median_sector(list(cell("x", 1, 1, placed = FALSE))), list(es = NA_real_, eo = NA_real_))
})

test_that("Strategic Index parts are the level terms and sum to the index; the S-curved columns are not used", {
  cells <- country_cells_tbl()
  w <- c(pol = 0.2, eo = 0.3, need = 0.3, sc = 0.1, tech = 0.1)
  k <- cells$key[1:4]
  eo_level <- c(0.6, 0.2, 0.5, 0.4); need <- c(0.3, 0.7, 0.4, 0.5); pol <- c(1, 0, 0.5, 0.5)
  sc <- c(0.5, 0.75, 0.25, 0.5); tw <- c(0.9, 0.9, 0.9, 0.2)
  si <- w[["eo"]] * eo_level + w[["need"]] * need + w[["pol"]] * pol + w[["sc"]] * sc + w[["tech"]] * tw
  si_c <- data.frame(key = k, eo_level = eo_level, need = need, pol = pol, sc_weight = sc, tech_weight = tw, si = si,
                     eo = c(0.99, 0.01, 0.5, 0.5), es_need = c(0.01, 0.99, 0.5, 0.5),   # S-curved chart inputs: must not matter
                     si_exclusion = NA, resource_base = NA, pick_exclusion = NA, stringsAsFactors = FALSE)
  out <- country_si(si_c, w)
  for (i in seq_along(k)) {
    f <- out$facts[[k[i]]]
    expect_true(f$in_sample)
    expect_equal(sum(vapply(f$parts, function(p) p$v, 1)), si[i], tolerance = 1e-9)
  }
  expect_equal(out$si_order[1], k[which.max(si)])
  expect_equal(out$facts[[out$si_order[1]]]$rank$d, "1st")
  expect_equal(out$n, 4)
  expect_false(out$facts[[cells$key[5]]]$in_sample)
  # the priority without policy drops 0.2 x pol (not renormalised)
  pp <- country_priority_policy(out, list(), data.frame(key = character(), records_specific = integer(), records_tech = integer(),
                                                        records_all = integer(), latest_year = numeric()))
  expect_equal(pp$priority[[k[1]]]$v$v, round(si[1] - 0.2 * pol[1], 6))
})

test_that("thirds use k = round(n / 3) on average ranks; classes; no variation gives no classes", {
  expect_equal(country_thirds(1:9, 9), rep(c("high", "middle", "low"), each = 3))
  expect_equal(country_thirds(c(1, 2, 3, 4, 5), 5), c("high", "high", "middle", "low", "low"))   # k = 2
  # tied sectors share a third: average rank 1.5 is beyond k = 1, so both sit in the middle third
  expect_equal(country_thirds(c(1.5, 1.5, 3), 3), c("middle", "middle", "low"))
  expect_equal(country_agreement_class(c("high", "high", "low", "low", "middle"), c("high", "low", "high", "low", "high")),
               c("aligned_high", "gap", "policy_ahead", "aligned_low", "partial"))
  cells <- country_cells_tbl()
  k <- cells$key[1:6]
  si_c <- data.frame(key = k, eo_level = seq(0.2, 0.7, length.out = 6), need = 0.5, pol = 0.5, sc_weight = 0.5, tech_weight = 0.5,
                     si_exclusion = NA, resource_base = NA, pick_exclusion = NA, stringsAsFactors = FALSE)
  si_c$si <- 0.3 * si_c$eo_level + 0.3 * 0.5 + 0.2 * 0.5 + 0.05 + 0.05
  out <- country_si(si_c, c(pol = 0.2, eo = 0.3, need = 0.3, sc = 0.1, tech = 0.1))
  rec <- data.frame(key = k, records_specific = 1L, records_tech = 1L, records_all = 1L, latest_year = 2020)
  pp <- country_priority_policy(out, list(), rec)
  expect_false(pp$agreement$variation)
  expect_null(pp$policy[[k[1]]]$class)
  expect_length(pp$agreement$by_class$gap, 0)
})

test_that("priority and policy ranks run 1..n over the in-sample sectors when some are out of sample", {
  cells <- country_cells_tbl()
  k <- cells$key[1:6]
  si_c <- data.frame(key = k, eo_level = seq(0.2, 0.7, length.out = 6), need = 0.5, pol = c(0.9, 0.1, 0.5, 0.3, 0.7, 0.2),
                     sc_weight = 0.5, tech_weight = 0.5, si_exclusion = NA, resource_base = NA, pick_exclusion = NA,
                     stringsAsFactors = FALSE)
  si_c$si <- 0.3 * si_c$eo_level + 0.3 * 0.5 + 0.2 * si_c$pol + 0.05 + 0.05
  si_c$si[c(2, 4)] <- NA   # two sectors out of sample; the other cells of the grid are absent too
  out <- country_si(si_c, c(pol = 0.2, eo = 0.3, need = 0.3, sc = 0.1, tech = 0.1))
  rec <- data.frame(key = k, records_specific = 1L, records_tech = 1L, records_all = 1L, latest_year = 2020)
  pp <- country_priority_policy(out, list(), rec)
  prank <- vapply(k, function(x) pp$priority[[x]]$rank$v %||% NA_real_, 0)
  qrank <- vapply(k, function(x) pp$policy[[x]]$rank$v %||% NA_real_, 0)
  expect_equal(unname(prank), c(4, NA, 3, NA, 2, 1))     # priority without policy rises with eo_level
  expect_equal(unname(qrank), c(1, NA, 3, NA, 2, 4))     # policy 0.9 > 0.7 > 0.5 > 0.2
  expect_equal(pp$priority[[k[6]]]$third, "high")
  expect_equal(pp$priority[[k[1]]]$third, "low")
})

test_that("top drivers: below-median ES takes the most negative impacts; constant categories never count", {
  impact <- c(-0.05, -0.2, 0.1, -0.3)
  const <- c(FALSE, FALSE, FALSE, TRUE)
  expect_equal(country_top_drivers(impact, const, "es", above_median = FALSE), c(2L, 1L))
  expect_equal(country_top_drivers(impact, const, "eo", above_median = TRUE), 3L)
  expect_equal(country_top_drivers(c(0.1, -0.4, 0.2), c(FALSE, FALSE, FALSE), "es", above_median = TRUE), c(2L, 3L))  # largest absolute
  expect_length(country_top_drivers(c(0, 0), c(FALSE, TRUE), "es", FALSE), 0)
})

test_that("missing-rule words split combined codes and keep unknown codes", {
  w <- cfg$missing_rules
  expect_equal(country_rule_words("zero", w, "Japan"), "not reported by the source: counted as zero")
  expect_equal(country_rule_words("global_average; global_average; zero", w, "Japan"),
               "missing for Japan: filled with the world average and not reported by the source: counted as zero")
  expect_equal(country_rule_words("odd_rule", w, "Japan"), "odd_rule")
  expect_true(is.na(country_rule_words(NA, w, "Japan")))
})

test_that("every index-only driver variable of the final run has a plain label", {
  vars <- list(c("Consumption", "elec_growth_index"), c("Consumption", "installed_cap_index"), c("Cost Competitiveness", "Input Cost Index"),
               c("Investment", "Pipeline Capacity Index"), c("Production", "growth_abs"), c("Production", "Overall Production"),
               c("Production", "Batteries Production"), c("Reserves", "Electric Vehicles Reserves"), c("Reserves", "Overall Wind Potential Index"),
               c("Technology Demand", "Overall Overcapacity"), c("Foreign Dependency", "Mineral Supply"), c("Foreign Dependency", "Overall Production"))
  for (v in vars) {
    lab <- country_var_label(cfg$var_labels, v[1], v[2])
    expect_false(is.null(lab), info = paste(v, collapse = " / "))
    expect_false(grepl("_|Index$", lab), info = lab)
  }
  expect_equal(country_var_label(cfg$var_labels, "Production", "Overall Production"), "Production: size and growth combined")
  expect_equal(country_var_label(cfg$var_labels, "Foreign Dependency", "Overall Production"), "Share of world manufacturing output")
  expect_null(country_var_label(cfg$var_labels, "Trade", "HHI"))
})

test_that("a raw_tables entry that matches two raw rows is reported, never resolved", {
  raw_rows <- data.frame(table = "t", iso3 = "JPN", tech = "Oil", stage = "Upstream", category = "Energy Imports",
                         variable = "Production surplus/deficit", value = c(-1, -2), Year = 2024, source = "ei", explanation = "x",
                         stringsAsFactors = FALSE)
  idx <- country_raw_index(raw_rows)
  map <- list(list(pillar = "es", category = "Energy Imports", variable = "Production surplus/deficit", table = "t",
                   raw_variable = "Production surplus/deficit", raw_tech = "cell", label = "Production minus consumption"))
  dups <- character()
  fn <- country_raw_fn("JPN", list(key = "oil-upstream", tech = "Oil", stage = "Upstream"), "es", raw_rows, idx, map, list(), cfg,
                       dup_fn = function(e, n) dups <<- c(dups, paste(e$variable, n)))
  r <- fn("Energy Imports", "Production surplus/deficit")
  expect_null(r$raw)
  expect_equal(dups, "Production surplus/deficit 2")
  expect_null(fn("Energy Imports", "Something else"))
})

test_that("IEA records: specific, technology and all counts with expanded cross-cutting rows", {
  pc <- data.frame(
    policy_id = c("p1", "p2", "p2", "p3", "p3", "p3", "p4", "p5"),
    iso3 = "JPN",
    tech = c("Solar", "Solar", "Solar", "Solar", "Wind", "Oil", "Critical Minerals", "Solar"),
    supply_chain = c("Upstream", "Upstream", "Midstream", "Upstream", "Upstream", "Upstream", "Upstream", "Upstream"),
    tech_crosscut = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE),
    sc_crosscut = c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
    status_norm = c("In force", "Ended", "Ended", "Unknown", "Unknown", "Unknown", "In force", "In force"),
    year = c(2020, 2018, 2018, 2015, 2015, 2015, 2023, 2030), stringsAsFactors = FALSE)
  all <- country_pams_counts_all(pc)
  x <- country_pams_counts(all, "JPN")
  cell <- function(k) x$cells[x$cells$key == k, ]
  expect_equal(cell("solar-upstream")$records_specific, 2L)     # p1, p5 (p2 names no stage, p3 no technology)
  expect_equal(cell("solar-upstream")$records_tech, 3L)         # p1, p2, p5 at any stage
  expect_equal(cell("solar-upstream")$records_all, 4L)          # p1, p2, p3, p5
  expect_equal(cell("solar-upstream")$latest_year, 2030)        # future years print as they are
  expect_equal(cell("wind-upstream")$records_specific, 0L)
  expect_equal(x$facts$pams_total$v, 5)
  expect_equal(x$facts$pams_no_tech$v, 1)
  expect_equal(x$facts$pams_critical_minerals$v, 1)
  expect_equal(x$facts$pams_by_status[["Unknown"]]$v, 1)
  expect_equal(names(x$facts$pams_by_status), c("In force", "Ended", "Unknown"))
})

test_that("the Climate Action Tracker: own rating, else the EU's for an EU member", {
  cat_tbl <- data.frame(geo = c("DEU", "EUU", "JPN"), overall = c("Insufficient", "Insufficient", "Highly insufficient"),
                        policies_action = NA, domestic_target = NA, fair_share = NA, climate_finance = NA, ndc = NA,
                        updated = c("Nov-24", "Dec-23", "Nov-24"), stringsAsFactors = FALSE)
  expect_equal(country_cat(cat_tbl, "DEU", eu_member = TRUE)$scope, "national")
  fr <- country_cat(cat_tbl, "FRA", eu_member = TRUE)
  expect_equal(fr$scope, "eu_bloc")
  expect_equal(fr$date_label, "December 2023")
  expect_null(country_cat(cat_tbl, "NGA", eu_member = FALSE))
})

test_that("partner reasons take the component with the highest percentile; Development Potential words by income group", {
  # percentiles among the rankable partners: C's Development Potential (0.75) beats its Friendshore and
  # Opportunity (0.375 each); D ties Friendshore and Opportunity (0.625) and takes the first
  psi_c <- data.frame(partner_iso = c("A", "B", "C", "D"), friendshore = c(0.9, 0.1, 0.3, 0.5), opportunity = c(0.2, 0.8, 0.3, 0.5),
                      development = c(NA, NA, 0.95, 0.2), rankable = TRUE, stringsAsFactors = FALSE)
  expect_equal(unname(country_partner_reason(psi_c)), c("fs", "po", "dp", "fs"))
  party_fn <- function(j) list(name = j, group = "rest", tier = "none", income_group = if (j == "A") "High income" else "Lower middle income")
  psi_c$psi <- c(0.7, 0.6, 0.5, 0.4); psi_c$rank <- 1:4; psi_c$rank_exclusion <- NA
  ties <- data.frame(partner = character(), imp_mean = numeric(), exp_mean = numeric(), tie = numeric(), tie_rank = integer(),
                     top3_import_cells = integer(), n_import_cells = integer())
  rows <- country_partner_rows("JPN", psi_c, data.frame(partner_iso = character(), side = character(), cells = character()), ties,
                               country_fdi(data.frame(pid = character(), key = character(), home = character(), dest = character(), index_use = logical()), "JPN"),
                               data.frame(iso3 = character(), agreement = character()),
                               data.frame(partner = character(), alignment = numeric(), bloc = character(), basis = character()),
                               party_fn, cfg$words, cfg)
  r <- stats::setNames(rows$rows, vapply(rows$rows, function(x) x$iso3, ""))
  expect_equal(r$A$dp$d, "not scored (high-income partner)")
  expect_equal(r$B$dp$d, "not scored (missing data)")
  expect_equal(r$C$reason_d, "development partner")
})

test_that("trade: partner shares are averaged over the economy's baskets (zero where absent), never summed", {
  shares <- data.frame(key = c("oil-upstream", "oil-upstream", "gas-upstream", "oil-upstream"), side = c("imp", "imp", "imp", "exp"),
                       partner = c("SAU", "USA", "USA", "CHN"), value = c(80, 20, 50, 10), share = c(0.8, 0.2, 1, 1), status = "importer",
                       stringsAsFactors = FALSE)
  pt <- country_partner_trade(shares)
  t <- pt$ties
  expect_equal(pt$n_import_cells, 2L)
  expect_equal(t$imp_mean[t$partner == "SAU"], 0.4)       # (0.8 + 0) / 2
  expect_equal(t$imp_mean[t$partner == "USA"], 0.6)       # (0.2 + 1) / 2
  expect_equal(t$exp_mean[t$partner == "CHN"], 1)
  expect_equal(t$tie[t$partner == "USA"], 0.3)            # mean of imports 0.6 and exports 0
  expect_equal(t$top3_import_cells[t$partner == "USA"], 2L)
  expect_true(all(t$imp_mean <= 1))
})

test_that("agreements: U.S. FTA only with the United States; EU rows not propagated; forum members count", {
  ag <- data.frame(iso3 = c("USA", "KOR", "JPN", "DEU", "EUU", "KOR", "JPN", "DEU", "KOR"),
                   agreement = c("U.S. FTA", "U.S. FTA", "MSP Forum", "MSP Forum", "MSP", "G7", "G7", "G7", "MSP Forum"),
                   membership_status = "member", stringsAsFactors = FALSE)
  expect_equal(country_agreements_shared(ag, "USA", "KOR"), "U.S. FTA")
  expect_equal(country_agreements_shared(ag, "JPN", "KOR"), c("G7", "MSP Forum"))
  expect_false("U.S. FTA" %in% country_agreements_shared(ag, "KOR", "JPN"))
  expect_false("MSP" %in% country_agreements_shared(ag, "DEU", "JPN"))      # the EU's own MSP row is not Germany's
})

test_that("industrial-policy measures: dates in any format, the active rules, the top five per sector", {
  expect_equal(country_nipo_date(as.POSIXct(c("2025-12-18 00:00:00", NA), tz = "UTC")), as.Date(c("2025-12-18", NA)))
  expect_equal(country_nipo_date(c("2025-12-18", "18/12/2025", "", NA)), as.Date(c("2025-12-18", "2025-12-18", NA, NA)))
  expect_equal(country_nipo_date(45000), as.Date("2023-03-15"))
  nipo <- data.frame(
    iso3 = "JPN", title = c("A", "B", "C", "D", "E", "F", "G", "A"),
    impl_date = as.Date(c("2024-01-01", "2023-01-01", NA, "2027-01-01", "2022-01-01", "2021-01-01", "2020-01-01", "2024-01-01")),
    removal_date = as.Date(c(NA, "2025-01-01", NA, NA, "2030-01-01", NA, NA, NA)),
    strength = c(1.5, 2.0, 1.8, 1.7, 0.9, 0.8, 0, 1.2), strength_base = 1, confidence = 1,
    techs = c("Solar", "Solar", "Solar|Wind", "Solar", "Solar", "Cross-cutting", "Solar", "Solar"), stages = "Upstream",
    url = NA, source = NA, stringsAsFactors = FALSE)
  # the user's rule: not removed by 2026-10-12 (B removed in 2025); announced (C) and scheduled (D) count, marked
  x <- country_nipo_top(nipo, "JPN", "2026-10-12", n = 5, rule = "not_removed")
  titles <- vapply(x$policies, function(p) p$title, "")[x$cells[["solar-upstream"]] + 1]
  expect_equal(titles, c("C", "D", "A", "E"))                       # by strength; G has 0 strength; A once (deduplicated)
  inf <- vapply(x$policies, function(p) isTRUE(p$in_force), TRUE)[x$cells[["solar-upstream"]] + 1]
  expect_equal(inf, c(FALSE, FALSE, TRUE, TRUE))
  expect_equal(x$n_not_in_force$v, 2)
  expect_equal(x$n_crosscut$v, 1)
  expect_equal(x$n_by_cell[["wind-upstream"]]$v, 1)
  expect_equal(x$active_rule, "not_removed")
  # the NIPO pipeline's rule: implemented by the date as well
  y <- country_nipo_top(nipo, "JPN", "2026-10-12", n = 5, rule = "in_force")
  expect_equal(vapply(y$policies, function(p) p$title, "")[y$cells[["solar-upstream"]] + 1], c("A", "E"))
  expect_equal(y$n_not_in_force$v, 2)
  expect_false(country_nipo_top(nipo, "DEU", "2026-10-12")$covered)
  # economy-wide measures (mapped to broad_min or more technologies) come after the sector-specific ones;
  # counts unchanged; the removal date in words
  z <- country_nipo_top(nipo, "JPN", "2026-10-12", n = 5, rule = "not_removed", broad_min = 2L)
  zt <- vapply(z$policies, function(p) p$title, "")[z$cells[["solar-upstream"]] + 1]
  expect_equal(zt, c("D", "A", "E", "C"))
  expect_true(z$policies[[match("C", vapply(z$policies, function(p) p$title, ""))]]$economy_wide)
  expect_false(z$policies[[match("A", vapply(z$policies, function(p) p$title, ""))]]$economy_wide)
  expect_equal(z$n_by_cell[["solar-upstream"]]$v, x$n_by_cell[["solar-upstream"]]$v)
  expect_equal(z$n_active$v, x$n_active$v)
  expect_equal(z$economy_wide_min_techs, 2L)
  e <- z$policies[[match("E", vapply(z$policies, function(p) p$title, ""))]]
  expect_equal(e$removal, "2030-01-01")
  expect_equal(e$removal_label, country_date_label(as.Date("2030-01-01")))
})

test_that("display strings follow the shared cases (A8.3)", {
  cases <- jsonlite::fromJSON(file.path(repo_root, "tests", "testthat", "fixtures", "country_fmt_cases.json"), simplifyVector = FALSE)$cases
  for (cs in cases) {
    got <- if (identical(cs$kind, "raw")) country_raw_text(cs$value, cs$unit_kind, cs$unit %||% "", cs$scale %||% 1) else country_fmt(cs$value, cs$kind)
    expect_identical(got, cs$expected, info = paste(cs$kind, cs$value))
  }
})

test_that("an EU member's regional figure carries the eu_region_share note and words; others do not", {
  # the IEA ETP 2035 share is copied to every EU member (0.15); a non-member and a member with its own value
  raw_rows <- data.frame(table = "market_share_manufacturing_tbl", iso3 = c("FIN", "BEL", "DEU", "FRA", "SWE", "USA"), tech = "Wind",
                         stage = "Midstream", category = "Foreign Dependency", variable = "Overall Market Share",
                         value = c(0.15, 0.15, 0.15, 0.15, 0.2, 0.15), Year = 2035, source = "IEA", explanation = "x", stringsAsFactors = FALSE)
  map <- list(list(pillar = "eo", category = "Foreign Dependency", variable = "Overall Market Share", table = "market_share_manufacturing_tbl",
                   raw_variable = "Overall Market Share", raw_tech = "cell", label = "Share of world manufacturing capacity, 2035 (announced)",
                   unit = "", unit_kind = "share", source = "iea_etp", eu_region = "the EU's announced share, carried by each EU member"))
  eu <- c("FIN", "BEL", "DEU", "FRA", "SWE")
  vals <- country_eu_region_values(raw_rows, map, eu, min_share = 0.75)
  expect_length(vals, 1)
  expect_equal(unname(vals), 0.15)
  idx <- country_raw_index(raw_rows)
  cell <- list(key = "wind-midstream", tech = "Wind", stage = "Midstream")
  raw_of <- function(iso, member) country_raw_fn(iso, cell, "eo", raw_rows, idx, map, list(), cfg, eu_member = member, eu_vals = vals)(
    "Foreign Dependency", "Overall Market Share")$raw
  fin <- raw_of("FIN", TRUE)
  expect_equal(fin$note, "eu_region_share")
  expect_equal(fin$note_d, "the EU's announced share, carried by each EU member")
  expect_null(raw_of("USA", FALSE)$note)                 # same value, not a member
  expect_null(raw_of("SWE", TRUE)$note)                  # a member with its own value
  expect_null(raw_of("SWE", TRUE)$note_d)
  # the config flags the ETP shares and the EV-region figures, both pillars
  flagged <- Filter(function(e) !is.null(e$eu_region), cfg$raw_tables)
  expect_setequal(unique(vapply(flagged, function(e) e$table, "")), c("foreign_dependency_tbl", "market_share_manufacturing_tbl"))
  expect_true(all(c("Market Share", "Overall Market Share", "market_share", "net_export", "production", "sales") %in% vapply(flagged, function(e) e$variable, "")))
  # the caveat reaches the sheet's caveat list and the memo trigger
  expect_false(is.null(cfg$caveats$eu_region_share))
  expect_true("eu_region_share" %in% COUNTRY_CAVEAT_IDS)
})

test_that("an index made only of the EU's regional figures carries the EU note on an EU member (EV midstream)", {
  cat_c <- data.frame(key = "electric-vehicles-midstream", category = "Foreign Dependency", category_weight = 0.5, category_score = 0.7,
                      imputed = FALSE, missing_rule_applied = NA_character_, stringsAsFactors = FALSE)
  var_c <- data.frame(category = "Foreign Dependency", variable = c("market_share", "production", "Overall Production"),
                      component_value = c(0.72, 0.72, 0.69), component_weight_within_overall = 0.2, imputed = FALSE,
                      missing_rule_applied = NA_character_, stringsAsFactors = FALSE)
  dstats <- list(cat = new.env(), var = new.env())
  assign(paste("electric-vehicles-midstream", "Foreign Dependency", sep = "\r"), list(median = 0.5, constant = FALSE), envir = dstats$cat)
  raw_fn <- function(note) function(category, variable) {
    if (variable == "Overall Production") return(list(raw = NULL))
    list(raw = country_fv_raw(0.137, "share", year = "2024", source = "iea_ev", source_label = "IEA EV regions", note = note,
                              note_d = if (is.na(note)) NA else "the EU's share, carried by each EU member"), label = variable)
  }
  drv <- function(note) country_cell_drivers("ESP", "Spain", "electric-vehicles-midstream", "eo", cat_c, var_c, dstats, TRUE, raw_fn(note), cfg)[[1]]
  vars_of <- function(d) stats::setNames(d$vars, vapply(d$vars, function(v) v$variable, ""))
  op <- vars_of(drv("eu_region_share"))[["Overall Production"]]
  expect_equal(op$raw$note, "eu_region_share")
  expect_equal(op$raw$note_d, "the EU's index, carried by each EU member")
  expect_null(op$raw$v)                                   # an index: no raw figure to print
  expect_equal(op$label, "Share of world manufacturing output")
  expect_null(vars_of(drv(NA_character_))[["Overall Production"]]$raw)   # a non-member (siblings without the note)
})

test_that("the allied pick names the technologies excluded by rule and says the rule in words", {
  expect_setequal(COUNTRY_PICK_EXCLUDED_TECHS, c("Green Hydrogen", "Geothermal"))
  w <- country_pick_rule_words(cfg$pick_rule)
  expect_match(w, "geothermal and green hydrogen", fixed = TRUE)
  expect_match(w, "resource base", fixed = TRUE)
  expect_null(country_pick_rule_words(NULL))
  si_rows <- data.frame(key = c("green-hydrogen-midstream", "batteries-midstream", "wind-upstream"), tech = c("Green Hydrogen", "Batteries", "Wind"),
                        stage = c("Midstream", "Midstream", "Upstream"), si = c(0.9, 0.8, 0.7), pick_exclusion = NA_character_, stringsAsFactors = FALSE)
  allied <- data.frame(tech = "Batteries", supply_chain = "Midstream", strategic_index = 0.8, stringsAsFactors = FALSE)
  pick <- country_allied_pick("core", allied, si_rows, 0.01, rule_words = w)
  expect_equal(pick$cell, "batteries-midstream")      # green hydrogen ranks higher but is excluded by rule
  expect_equal(pick$runner_up, "wind-upstream")
  expect_setequal(unlist(pick$excluded), COUNTRY_PICK_EXCLUDED_TECHS)
  expect_equal(pick$rule_d, w)
  expect_true(any(vapply(cfg$path_meanings, function(e) identical(e$path, "pick.rule_d"), TRUE)))
  # outside the coalition, or a member without an allied row: no pick
  expect_null(country_allied_pick("none", allied, si_rows))
  expect_null(country_allied_pick("core", allied[0, ], si_rows))
  # the retired network design leaves no path meanings, sources or caveat behind (2026-10-17)
  expect_false(any(vapply(cfg$path_meanings, function(e) grepl("^network", e$path), TRUE)))
  expect_null(cfg$caveats$eu_node)
})

test_that("memos written before the revision cut-off are under revision (2026-10-17)", {
  rev <- list(before = "2026-10-17T00:00:00Z")
  expect_true(country_memo_under_revision(list(generated_at = "2026-10-14T15:56:16Z"), rev))
  expect_false(country_memo_under_revision(list(generated_at = "2026-10-18T09:00:00Z"), rev))
  expect_true(country_memo_under_revision(list(), rev))
  expect_false(country_memo_under_revision(list(generated_at = "2026-10-14T15:56:16Z"), NULL))
  expect_identical(cfg$memo_revision$before, "2026-10-17T00:00:00Z")
})

test_that("industrial-policy measures are marked EU-wide, shared or national and counted", {
  nipo <- data.frame(
    iso3 = c("BEL", "FRA", "DEU", "NLD", "LUX", "BEL", "BEL", "FRA", "BEL", "BEL"),
    title = c(rep("EU: Innovation Fund call for net-zero manufacturing", 5), "Belgium: Offshore wind tender",
              "Belgium and France: Joint hydrogen corridor", "Belgium and France: Joint hydrogen corridor",
              "EC: Commission approves Belgian state aid for battery plant", "Belgium: Expired grant"),
    impl_date = as.Date(c(rep("2024-01-01", 5), "2023-06-01", "2022-01-01", "2022-01-01", "2025-01-01", "2015-01-01")),
    removal_date = as.Date(c(rep(NA, 9), "2020-01-01")),
    strength = c(rep(1.8, 5), 1.5, 1.2, 1.2, 1.9, 2.5), strength_base = 1, confidence = 1,
    techs = c(rep("Batteries|Solar", 5), "Wind", "Green Hydrogen", "Green Hydrogen", "Batteries", "Wind"),
    stages = c(rep("Midstream", 5), "Downstream", "Midstream", "Midstream", "Midstream", "Downstream"),
    url = NA, source = NA, stringsAsFactors = FALSE)
  sc <- country_nipo_scope(nipo, unlist(cfg$nipo$eu_prefixes), 5L)
  expect_equal(sc$scope[c(1, 6, 7, 9)], c("eu", "national", "multi", "national"))   # an EC approval of a national scheme is national
  expect_equal(sc$n_juris[c(1, 6, 7)], c(5L, 1L, 2L))
  expect_equal(country_nipo_scope(nipo, unlist(cfg$nipo$eu_prefixes), 6L)$scope[1], "multi")  # too few members to be EU-wide
  x <- country_nipo_top(sc, "BEL", "2026-10-12", n = 5, rule = "not_removed")
  expect_equal(x$n_active$v, 4)                      # the expired grant is not active
  expect_equal(x$n_active_eu$v, 1)
  expect_equal(x$n_active_shared$v, 2)
  expect_equal(x$n_listed$v, 4)
  expect_equal(x$n_listed_eu$v, 1)
  expect_equal(x$n_listed_shared$v, 2)
  scope <- stats::setNames(vapply(x$policies, function(p) p$scope, ""), vapply(x$policies, function(p) p$title, ""))
  expect_equal(unname(scope[["Innovation Fund call for net-zero manufacturing"]]), "eu")
  expect_equal(x$policies[[which(names(scope) == "Innovation Fund call for net-zero manufacturing")]]$n_jurisdictions$v, 5)
  # computed on the fly when the table has no scope yet (meta carries the prefixes)
  y <- country_nipo_top(nipo, "BEL", "2026-10-12", n = 5, meta = list(eu_prefixes = cfg$nipo$eu_prefixes, eu_min_jurisdictions = 5L), rule = "not_removed")
  expect_equal(y$n_listed_eu$v, 1)
  # words for the scope enum
  expect_setequal(names(cfg$words$nipo_scope), c("eu", "multi", "national"))
})

test_that("caveats print running-text names, spell out the sources and keep the NIPO caveat short", {
  expect_equal(country_prose_name("Netherlands"), "the Netherlands")
  expect_equal(country_prose_name("United States"), "the United States")
  expect_equal(country_prose_name("Japan"), "Japan")
  expect_equal(country_poss("the Netherlands"), "the Netherlands'")
  expect_equal(country_fill_caveat("{country}'s counts leave out the EU's own {n} records.", list(country = "Netherlands", n = "438")),
               "The Netherlands' counts leave out the EU's own 438 records.")
  expect_equal(country_fill_caveat("Scores compare {country} with others; its alignment is {sovereign}'s.", list(country = "Czech Republic", sovereign = "United States")),
               "Scores compare the Czech Republic with others; its alignment is the United States'.")
  vals <- list(country = "United Kingdom", as_of_label = "12 October 2026", few = 10, sovereign = "United States", cmfdi_edition = "2026.10.05",
               share = "37%", n = "438", nipo_vintage = cfg$nipo$vintage_label)
  for (id in names(cfg$caveats)) {
    txt <- country_fill_caveat(cfg$caveats[[id]]$text, vals)
    expect_false(grepl("\\b(GTA|IEA|OSI|NIPO|PAMS|CAT)\\b", txt), info = paste(id, ":", txt))
    expect_false(grepl("{", txt, fixed = TRUE), info = id)
  }
  nc <- country_fill_caveat(cfg$caveats$nipo_context$text, vals)
  expect_lte(length(strsplit(nc, "[[:space:]]+")[[1]]), 27)
  expect_match(nc, "October 2026 extract (as of 5 October 2026)", fixed = TRUE)
  expect_match(nc, "context only", fixed = TRUE)
  expect_match(nc, "Policy index", fixed = TRUE)
  expect_false(grepl("\\bGTA\\b", cfg$nipo$vintage_label))
  for (w in cfg$words$policy_basis) expect_false(grepl("\\b(IEA|PAMS|CAT)\\b", w))
  for (s in cfg$sources) expect_false(grepl("\\b(IEA|GTA|OSI)\\b", s$label), info = s$id)
  for (id in names(cfg$source_names)) for (f in c("label", "short")) if (!is.null(cfg$source_names[[id]][[f]])) expect_false(grepl("\\bIEA\\b", cfg$source_names[[id]][[f]]), info = id)
})
