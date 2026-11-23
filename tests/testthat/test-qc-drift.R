# Guards the release-drift comparison (R/qc/qc_drift.R; docs/QC_SYSTEM_DESIGN.md 6.2 "version drift") on small
# synthetic releases: standardising a strategic_position table and the shipped columnar posture core into the
# same columns (need and deployment need kept apart), the transitions matrix (excluded rows missing from the
# shipped core are expected, a decided cell on one side only is not), changed cells with the inputs that moved and
# the thresholds they crossed, large moves, quantile shifts, Spearman rank changes, AIB class and compact changes
# (compacts matched on their node set, reused IDs caught), release alignment, and that drift never fails.

qr_root <- function() normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(dplyr))
for (f in c("R/qc/qc_core.R", "R/qc/qc_drift.R")) source(file.path(qr_root(), f))

qr_th <- list(need = 0.67, prize = 0.6, cp = 0.75, deployment = 0.6)

# A strategic_position-shaped table: one row per (iso3, stage) for Solar.
qr_sp <- function(iso3, stage, choice, need = 0.5, prize = 0.5, cp = 0.5, dn = NA_real_, excluded = FALSE) {
  data.frame(iso3 = iso3, tech = "Solar", supply_chain = stage, strategic_choice = if (excluded) NA_character_ else choice,
             posture_exclusion = if (excluded) "entrepot" else NA_character_, supply_chain_need = need,
             strategic_industrial_value = prize, industrial_opportunity = prize, cp_contender_pct = cp, deployment_need = dn,
             home_share_exposure = 0.02, decision_borderline = NA_character_, stringsAsFactors = FALSE)
}

qr_prev <- function() rbind(
  qr_sp("AAA", "Components", "preserve", need = 0.8, prize = 0.7, cp = 0.80),
  qr_sp("BBB", "Components", "watch", need = 0.3, prize = 0.3, cp = 0.10),
  qr_sp("CCC", "Components", "partner", need = 0.9, prize = 0.7, cp = 0.20),
  qr_sp("DDD", "Components", "watch", need = 0.2, prize = 0.2, cp = 0.40),
  qr_sp("EEE", "Components", "watch", need = 0.1, prize = 0.1, cp = 0.60),
  qr_sp("AAA", "Deployment", "accelerate", need = NA, prize = NA, cp = NA, dn = 0.65),
  qr_sp("HKG", "Components", NA, excluded = TRUE)
)

test_that("standardising keeps supply-chain need and deployment need apart", {
  s <- qc_sp_standardize(qr_prev())
  dep <- s[s$supply_chain == "Deployment", ]
  expect_true(is.na(dep$need))
  expect_equal(dep$dn, 0.65)
  expect_true(all(is.na(s$dn[s$supply_chain != "Deployment"])))
  expect_true(is.na(s$choice[s$iso3 == "HKG"]))
  expect_true(all(c("need", "dn", "prize", "market", "cp", "share") %in% names(s)))
})

test_that("the columnar posture core decodes to the same standard columns", {
  core <- list(
    countries = list(iso3 = c("AAA", "BBB")),
    meta = list(techs = c("Solar"), stages = c("Components", "Deployment")),
    cells = list(c = c(0, 0, 1, 1), t = c(0, 0, 0, 0), s = c(0, 1, 0, 1), p = c("C", "D", "S", "N"),
                 m = list("preserve", NULL, NULL, NULL), dm = list(NULL, "accelerate", NULL, NULL),
                 need = c(0.8, 0.65, 0.95, NA), prize = c(0.7, NA, 0.4, NA), io = c(0.7, NA, 0.4, NA),
                 cp = c(0.8, NA, 0.2, NA), share = c(0.02, NA, 0, NA), bd = list(NULL, "need", NULL, NULL))
  )
  b <- qc_posture_core_to_table(core)
  expect_equal(nrow(b), 4)
  expect_equal(b$choice, c("preserve", "accelerate", "secure", "not_assessed"))
  expect_equal(b$iso3, c("AAA", "AAA", "BBB", "BBB"))
  expect_equal(b$supply_chain, c("Components", "Deployment", "Components", "Deployment"))
  # the core's need at Deployment is the deployment need
  expect_equal(b$dn[2], 0.65)
  expect_true(is.na(b$need[2]))
  expect_equal(b$need[1], 0.8)
  expect_true(is.na(b$prize[2]))
  expect_equal(b$borderline[2], "need")
})

test_that("transitions count choice changes, treat excluded rows missing from the core as expected, and flag a lost cell", {
  prev <- qc_sp_standardize(qr_prev())
  cur_raw <- qr_prev()
  cur_raw$strategic_choice[1] <- "catch_up"; cur_raw$cp_contender_pct[1] <- 0.70      # AAA: cp crosses 0.75
  cur_raw$strategic_choice[3] <- "watch"                                              # CCC: no input moves (a gate)
  cur_raw$strategic_choice[6] <- "watch"; cur_raw$deployment_need[6] <- 0.55          # AAA Deployment: dn crosses 0.6
  cur_raw <- cur_raw[cur_raw$iso3 != "EEE", ]                                         # a decided cell disappears
  cur <- qc_sp_standardize(cur_raw)
  prev <- prev[prev$iso3 != "HKG", ]                                                  # the shipped core omits excluded rows
  j <- qc_drift_join(prev, cur)
  tr <- qc_drift_transitions(j)
  n_of <- function(f, t) sum(tr$n[tr$from == f & tr$to == t])
  expect_equal(n_of("preserve", "catch_up"), 1L)
  expect_equal(n_of("partner", "watch"), 1L)
  expect_equal(n_of("accelerate", "watch"), 1L)
  expect_equal(n_of("watch", "(absent)"), 1L)
  hk <- tr[tr$from == "(absent)" & tr$to == "(excluded)", ]
  expect_equal(hk$n, 1L)
  expect_true(hk$expected_absence)
  expect_false(hk$changed)

  cells <- qc_drift_cells(j, qr_th)
  a <- cells[cells$iso3 == "AAA" & cells$supply_chain == "Components", ]
  expect_true(a$choice_changed)
  expect_match(a$moved_inputs, "cp")
  expect_match(a$thresholds_crossed, "cp 0.800->0.700 \\(th 0.75\\)")
  expect_false(a$unexplained)
  d <- cells[cells$iso3 == "AAA" & cells$supply_chain == "Deployment", ]
  expect_match(d$thresholds_crossed, "dn 0.650->0.550 \\(th 0.60\\)")
  cc <- cells[cells$iso3 == "CCC", ]
  expect_true(cc$unexplained)
  expect_true(any(cells$iso3 == "EEE" & cells$presence == "only_prev"))
  expect_false(any(cells$iso3 == "HKG"))
  expect_true(all(cells$status == "warn"))

  ch <- qc_drift_checks(j, cells, label = "synthetic")
  expect_false(any(ch$status == "fail"))
  cr <- ch[ch$check_id == "drift_choice_changes", ]
  expect_equal(cr$n, 3L)
  expect_true(cr$needs_human_explanation)
  expect_match(cr$detail, "1 of them with no input crossing")
  pr <- ch[ch$check_id == "drift_presence", ]
  expect_equal(pr$n, 1L)
  expect_match(pr$examples, "EEE \\(only_prev\\)")
  expect_match(pr$detail, "1 more present on one side only as excluded rows")
})

test_that("large moves, quantile shifts and rank reversals are reported as warn, never fail", {
  prev <- qc_sp_standardize(qr_prev()[1:5, ])
  cur_raw <- qr_prev()[1:5, ]
  cur_raw$cp_contender_pct <- c(0.1, 0.8, 0.6, 0.4, 0.2)      # the ranking 5,1,2,3,4 reverses to 1,5,4,3,2
  cur_raw$strategic_choice <- qr_prev()$strategic_choice[1:5]  # choices kept: only the inputs move
  cur <- qc_sp_standardize(cur_raw)
  j <- qc_drift_join(prev, cur)
  cells <- qc_drift_cells(j, qr_th, large = 0.15)
  expect_equal(sum(cells$large_move), 4L)                      # DDD keeps 0.4
  expect_false(any(cells$choice_changed))
  dist <- qc_drift_distribution(j)
  expect_true(all(c("input", "stat", "prev", "cur", "delta") %in% names(dist)))
  expect_equal(dist$delta[dist$input == "cp" & dist$stat == "p50"], 0)
  ranks <- qc_drift_ranks(j, min_n = 5)
  rho <- ranks$spearman[ranks$input == "cp"]
  expect_equal(rho, -1)
  ch <- qc_drift_checks(j, cells, dist, ranks, label = "synthetic", large = 0.15)
  expect_equal(ch$status[ch$check_id == "drift_rank_changes"], "warn")
  expect_equal(ch$status[ch$check_id == "drift_large_input_moves"], "warn")
  expect_true(ch$needs_human_explanation[ch$check_id == "drift_large_input_moves"])
  expect_false(any(ch$status == "fail"))
})

test_that("AIB drift reports class changes and matches compacts on their node set, catching reused IDs", {
  pn <- data.frame(node_id = c("magnets", "cells", "poly"), class = c("BUILD", "COORDINATE", "LOW INTERVENTION"))
  cn <- data.frame(node_id = c("magnets", "cells", "wafers"), class = c("BUILD", "BUILD", "BUILD"))
  pc <- data.frame(compact_id = c("AIB-C01", "AIB-C02"))
  pc$nodes <- list(c("magnets"), c("cells", "anodes"))
  pc$members <- c("AUS JPN", "KOR;USA")
  cc <- data.frame(compact_id = c("AIB-C01", "AIB-C02"))
  cc$nodes <- list(c("anodes", "cells"), c("wafers"))      # the cells compact moved from C02 to C01
  cc$members <- list(c("KOR", "USA", "JPN"), c("TWN"))
  a <- qc_drift_aib(pn, cn, pc, cc)
  ch_nodes <- a$nodes[a$nodes$changed, ]
  expect_setequal(ch_nodes$node_id, c("cells", "poly", "wafers"))
  both <- a$compacts[a$compacts$presence == "both", ]
  expect_equal(nrow(both), 1L)
  expect_equal(both$compact_id_prev, "AIB-C02")
  expect_equal(both$compact_id_cur, "AIB-C01")
  expect_equal(both$joined, "JPN")
  expect_equal(both$left, "")
  expect_setequal(a$id_reused, c("AIB-C01", "AIB-C02"))
  ck <- qc_drift_aib_checks(a, "synthetic")
  expect_equal(ck$n[ck$check_id == "drift_aib_class_changes"], 3L)
  expect_equal(ck$n[ck$check_id == "drift_compact_membership_changes"], 1L)
  expect_equal(ck$n[ck$check_id == "drift_compact_set_changes"], 2L)
  expect_equal(ck$n[ck$check_id == "drift_compact_id_reused"], 2L)
  expect_false(any(ck$status == "fail"))
})

test_that("release alignment warns when the processed output predates the bundle or does not reproduce it", {
  t <- function(s) as.POSIXct(s, tz = "UTC")
  x <- data.frame(component = c("posture", "aib", "country"),
                  bundle_created = t(c("2026-11-17 20:33:00", "2026-11-17 19:07:00", "2026-10-31 15:58:00")),
                  pipeline_time = t(c("2026-11-17 23:16:00", "2026-10-30 19:47:00", "2026-11-01 00:00:00")),
                  reproduced = c(TRUE, FALSE, FALSE), detail = c("0 changes", "7 classes differ", ""))
  r <- qc_release_alignment(x)
  expect_equal(r$status, c("info", "warn", "warn"))
  expect_match(r$detail[2], "predates the shipped bundle")
  expect_match(r$detail[3], "does not reproduce it")
  expect_true(all(r$needs_human_explanation[2:3]))
  expect_false(any(r$status == "fail"))
  expect_equal(nrow(qc_release_alignment(x[0, ])), 0L)
})
