# Guards the golden-case evaluation on the pipeline table (R/qc/qc_golden.R; docs/QC_SYSTEM_DESIGN.md 4.5, 6.1) on
# synthetic cases and tables: pass / fail / missing / not_applicable per case, the canonical choice keys, choice_in,
# the fence from yard_rollup.R, numeric ranges with the bundle's 3-decimal rounding slack, deployment need read at
# Deployment, facts outside the standard columns read from the raw table, AIB node cases on the node table's
# scenario only, and that a golden failure here is a warn (drift evidence), never a fail.

qg_root <- function() normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(dplyr))
for (f in c("R/strategic_position/yard_rollup.R", "R/qc/qc_core.R", "R/qc/qc_drift.R", "R/qc/qc_golden.R")) {
  source(file.path(qg_root(), f))
}

qg_raw <- function() {
  data.frame(
    iso3 = c("JPN", "DEU", "USA", "HKG", "KOR"),
    tech = c("Electric Vehicles", "Electric Grid", "Nuclear", "Solar", "Batteries"),
    supply_chain = c("Refining", "Components", "Deployment", "Components", "Components"),
    strategic_choice = c("secure", "watch", "watch", NA, "preserve"),
    posture_exclusion = c(NA, NA, NA, "entrepot", NA),
    supply_chain_need = c(0.998, 0.30, NA, 0.5, 0.80),
    strategic_industrial_value = c(0.9, 0.4665, NA, 0.5, 0.70),
    industrial_opportunity = c(0.9, 0.4665, NA, 0.5, 0.70),
    cp_contender_pct = c(0.5, 0.2, NA, 0.5, 0.82),
    deployment_need = c(NA, NA, 0.597, NA, NA),
    home_share_exposure = c(0.01, 0.05, NA, 0.03, 0.12),
    china_share = c(0.472, 0.3, 0.1, 0.9, 0.6),
    stringsAsFactors = FALSE
  )
}

qg_case <- function(id, key, expect, kind = "posture_cell", exact = TRUE, category = "obvious_secure") {
  list(id = id, category = category, kind = kind, key = key, expect = expect, exact = exact, change_requires_review = TRUE,
       rationale = "synthetic", approved = list(by = "test", on = "2026-11-20", release = NULL))
}

qg_eval <- function(cases, ...) {
  raw <- qg_raw()
  qc_golden_eval(list(cases = cases), qc_sp_standardize(raw), raw = raw, ...)
}

test_that("a posture case passes on choice, fence and numeric ranges, and fails when any of them differs", {
  r <- qg_eval(list(
    qg_case("GC_001", list(iso3 = "JPN", tech = "Electric Vehicles", stage = "Refining"),
            list(choice = "secure", fence = "high", numeric = list(need = c(0.97, 1.0)))),
    qg_case("GC_002", list(iso3 = "JPN", tech = "Electric Vehicles", stage = "Refining"),
            list(choice = "preserve", fence = "medium")),
    qg_case("GC_003", list(iso3 = "JPN", tech = "Electric Vehicles", stage = "Refining"),
            list(choice = "secure", numeric = list(need = c(0.5, 0.9))))
  ))
  expect_equal(r$status, c("pass", "fail", "fail"))
  expect_match(r$observed[1], "fence=high")
  expect_match(r$failures[2], "choice secure not in preserve")
  expect_match(r$failures[2], "fence high, expected medium")
  expect_match(r$failures[3], "need 0.998 outside \\[0.5, 0.9\\]")
  expect_equal(r$evaluated_against, rep("strategic_position_tbl", 3))
})

test_that("choice_in accepts any listed key unless the case is exact on a single choice", {
  r <- qg_eval(list(
    qg_case("GC_010", list(iso3 = "KOR", tech = "Batteries", stage = "Components"),
            list(choice_in = c("preserve", "catch_up")), exact = FALSE),
    qg_case("GC_011", list(iso3 = "KOR", tech = "Batteries", stage = "Components"),
            list(choice_in = c("partner", "catch_up")), exact = FALSE)
  ))
  expect_equal(r$status, c("pass", "fail"))
})

test_that("a range pinned at the bundle's rounding accepts the unrounded value that displays the same, nothing further", {
  key <- list(iso3 = "DEU", tech = "Electric Grid", stage = "Components")
  r <- qg_eval(list(
    qg_case("GC_005", key, list(choice = "watch", numeric = list(prize = c(0.467, 0.467)))),
    qg_case("GC_006", key, list(choice = "watch", numeric = list(prize = c(0.468, 0.468))))
  ))
  expect_equal(r$status, c("pass", "fail"))
  strict <- qg_eval(list(qg_case("GC_005", key, list(numeric = list(prize = c(0.467, 0.467))))), tol = 0)
  expect_equal(strict$status, "fail")
})

test_that("deployment need is read at Deployment and facts outside the standard columns come from the raw table", {
  r <- qg_eval(list(
    qg_case("GC_016", list(iso3 = "USA", tech = "Nuclear", stage = "Deployment"),
            list(choice = "watch", fence = "low", numeric = list(deployment_need = c(0.585, 0.599)))),
    qg_case("GC_020", list(iso3 = "JPN", tech = "Electric Vehicles", stage = "Refining"),
            list(numeric = list(china_share = c(0.45, 0.50), home_share = c(0, 0.02))))
  ))
  expect_equal(r$status, c("pass", "pass"))
  expect_match(r$observed[2], "china_share=0.472")
  # without the raw table china_share is not observable; home_share still is
  raw <- qg_raw()
  r2 <- qc_golden_eval(list(cases = list(qg_case("GC_020", list(iso3 = "JPN", tech = "Electric Vehicles", stage = "Refining"),
                                                 list(numeric = list(china_share = c(0.45, 0.50), home_share = c(0, 0.02)))))),
                       qc_sp_standardize(raw))
  expect_equal(r2$status, "pass")
  expect_match(r2$note, "not observable in this table: china_share")
})

test_that("a missing key, an excluded row and an unobservable expectation are 'missing'; compacts are not applicable", {
  r <- qg_eval(list(
    qg_case("GC_030", list(iso3 = "ZZZ", tech = "Solar", stage = "Components"), list(choice = "watch")),
    qg_case("GC_031", list(iso3 = "HKG", tech = "Solar", stage = "Components"), list(choice = "watch")),
    qg_case("GC_032", list(iso3 = "KOR", tech = "Batteries", stage = "Components"), list(numeric = list(nonexistent = c(0, 1)))),
    qg_case("GC_034", list(sid = "SC-BATTERIES", scenario = "decoupling"), list(choice = "sectoral"), kind = "compact")
  ))
  expect_equal(r$status, c("missing", "missing", "missing", "not_applicable"))
  expect_match(r$note[2], "excluded")
  expect_match(r$note[3], "nothing the case expects is observable")
})

test_that("AIB node cases are evaluated on the node table's scenario only", {
  nodes <- data.frame(node_id = c("rare-earth-permanent-magnets", "integrated-circuits"), class = c("BUILD", "LOW INTERVENTION"),
                      shock_exposure = c(0.618, 0.20), residual_gap = c(0.607, 0.107))
  cases <- list(
    qg_case("GC_029", list(node = "rare-earth-permanent-magnets", scenario = "decoupling"),
            list(choice = "BUILD", numeric = list(exposure = c(0.588, 0.648), residual = c(0.577, 0.637))), kind = "aib_node"),
    qg_case("GC_033", list(node = "integrated-circuits", scenario = "decoupling"),
            list(choice = "BUILD", numeric = list(residual = c(0.10, 0.115))), kind = "aib_node"),
    qg_case("GC_032", list(node = "li-ion-cells-and-packs", scenario = "managed"), list(choice = "BUILD"), kind = "aib_node"),
    qg_case("GC_040", list(node = "not-a-node", scenario = "decoupling"), list(choice = "BUILD"), kind = "aib_node")
  )
  r <- qg_eval(cases, aib_nodes = nodes, aib_label = "aib nodes")
  expect_equal(r$status, c("pass", "fail", "not_applicable", "missing"))
  expect_match(r$failures[2], "class LOW INTERVENTION not in BUILD")
  expect_equal(r$evaluated_against[1], "aib nodes")
  expect_equal(qg_eval(cases[1])$status, "not_applicable")   # no node table given
})

test_that("golden check rows are warn with a human explanation on failures and never fail", {
  r <- qg_eval(list(
    qg_case("GC_001", list(iso3 = "JPN", tech = "Electric Vehicles", stage = "Refining"), list(choice = "secure")),
    qg_case("GC_002", list(iso3 = "JPN", tech = "Electric Vehicles", stage = "Refining"), list(choice = "watch")),
    qg_case("GC_030", list(iso3 = "ZZZ", tech = "Solar", stage = "Components"), list(choice = "watch"))
  ))
  ck <- qc_golden_checks(r)
  expect_false(any(ck$status == "fail"))
  f <- ck[ck$check_id == "golden_cases_fail", ]
  expect_equal(f$status, "warn")
  expect_equal(f$n, 1L)
  expect_true(f$needs_human_explanation)
  expect_match(f$detail, "pass=1, fail=1, missing=1")
  expect_match(f$examples, "GC_002")
  expect_equal(ck$n[ck$check_id == "golden_cases_missing"], 1L)
  empty <- qc_golden_checks(qc_golden_eval(NULL, qc_sp_standardize(qg_raw())))
  expect_equal(empty$status, "info")
  expect_equal(empty$n, 0L)
})

test_that("the case list is found at the top level or under cases / golden_cases", {
  one <- qg_case("GC_001", list(iso3 = "JPN", tech = "Electric Vehicles", stage = "Refining"), list(choice = "secure"))
  expect_length(qc_golden_cases(list(cases = list(one))), 1)
  expect_length(qc_golden_cases(list(golden_cases = list(one, one))), 2)
  expect_length(qc_golden_cases(list(one)), 1)
  expect_length(qc_golden_cases(NULL), 0)
})
