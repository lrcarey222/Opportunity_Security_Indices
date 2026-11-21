# Guards the canonical per-cell robustness classes (R/qc/qc_sensitivity.R; docs/QC_SYSTEM_DESIGN.md 4.6): a
# fragile, a sensitive and a stable cell, the not-testable cases, the fence change, the skipped node-attribute
# specs, and the need-floor cap artifact (a capped need stored as 0.61 must not flip at need 0.60).

qs_root <- function() normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(dplyr))
for (f in c("R/utils/supply_chain_stages.R", "R/strategic_position/sp_helpers.R", "R/strategic_position/industrial_opportunity.R",
            "R/strategic_position/strategic_value.R", "R/strategic_position/decision_engine.R", "R/strategic_position/yard_rollup.R",
            "R/strategic_position/supply_chain_need.R", "R/qc/qc_sensitivity.R")) {
  source(file.path(qs_root(), f))
}
qs_cfg <- function() yaml::read_yaml(file.path(qs_root(), "config", "strategic_position.yml"))$decision_framework

# One producing cell per call; strategic_choice is filled from the canonical rule unless given.
qs_cell <- function(id, ...) {
  base <- list(Country = id, iso3 = id, tech = "Solar", supply_chain = "Components", engine_posture = "deploy",
               posture_exclusion = NA_character_, supply_chain_need = 0.9, supply_chain_need_uncapped = 0.9,
               supply_chain_need_score = 0.5, supply_chain_need_cap = NA_character_, strategic_industrial_value = 0.3,
               industrial_opportunity = 0.3, siv_cap = NA_character_, deployment_need = NA_real_, dn_families_observed = NA_integer_,
               cp_contender_pct = 0.2, cp_contender_basis = "contenders", cp_is_contender = TRUE, t3_capability_lo = "moderate",
               pathway_credible = "no", large_yard_case = FALSE, large_yard_near = FALSE, strategic_choice = NA_character_)
  a <- list(...)
  base[names(a)] <- a
  as.data.frame(base, stringsAsFactors = FALSE)
}
qs_tbl <- function(...) {
  x <- do.call(rbind, list(...))
  fill <- is.na(x$strategic_choice) & !(x$iso3 %in% "EXC")
  x$strategic_choice[fill] <- sp_choice_rerun(transform(x, strategic_choice = "tmp"), qs_cfg())[fill]
  x
}
qs_fixture <- function() qs_tbl(
  # high need just over the bar, low prize, weak position: Partner, Watch at need 0.72 -> fragile, fence change
  qs_cell("FRG", supply_chain_need = 0.70, supply_chain_need_uncapped = 0.70),
  # need 0.61, uncapped, score above the floor: Watch at every local need (0.62, 0.72), Partner at 0.60 -> sensitive
  qs_cell("SNS", supply_chain_need = 0.61, supply_chain_need_uncapped = 0.61),
  # far from every threshold -> stable
  qs_cell("STB", supply_chain_need = 0.95, supply_chain_need_uncapped = 0.95, cp_contender_pct = 0.1),
  # the need-floor cap: uncapped 0.80, score 0.03 below the floor 0.10 (and below 0.05) -> stored 0.61, Watch
  qs_cell("CAP", supply_chain_need = 0.61, supply_chain_need_uncapped = 0.80, supply_chain_need_score = 0.03,
          supply_chain_need_cap = "score_below_floor"),
  # high prize, position exactly 0.75 with capability: Preserve; strictly above 0.75 fails -> Watch: fragile
  qs_cell("EDG", supply_chain_need = 0.2, supply_chain_need_uncapped = 0.2, strategic_industrial_value = 0.8, cp_contender_pct = 0.75),
  # prize 0.62 with a top-quarter position: Preserve; at prize 0.65 (local + band, and the structural 0.65) Watch
  qs_cell("PRZ", supply_chain_need = 0.2, supply_chain_need_uncapped = 0.2, strategic_industrial_value = 0.62, cp_contender_pct = 0.9),
  # Deployment, deployment need 0.9 on one family: Accelerate; Watch when Accelerate needs 2 families -> sensitive
  qs_cell("DEP", supply_chain = "Deployment", supply_chain_need = NA_real_, supply_chain_need_uncapped = NA_real_,
          supply_chain_need_score = NA_real_, strategic_industrial_value = NA_real_, deployment_need = 0.9, dn_families_observed = 1L),
  qs_cell("SEC", engine_posture = "secure", supply_chain_need = 0.70, supply_chain_need_uncapped = 0.70),
  qs_cell("NAS", supply_chain_need = NA_real_, supply_chain_need_uncapped = NA_real_, supply_chain_need_score = NA_real_),
  qs_cell("EXC", posture_exclusion = "entrepot"),
  # a stored choice the rule does not give from the stored inputs
  qs_cell("BAD", supply_chain_need = 0.95, supply_chain_need_uncapped = 0.95, strategic_choice = "preserve")
)

test_that("the fixture's stored choices are the canonical rule's", {
  x <- qs_fixture()
  ch <- setNames(x$strategic_choice, x$iso3)
  expect_equal(unname(ch[c("FRG", "SNS", "STB", "CAP", "EDG", "DEP", "SEC", "NAS")]),
               c("partner", "watch", "partner", "watch", "preserve", "accelerate", "secure", "not_assessed"))
  expect_true(is.na(ch[["EXC"]]))
})

test_that("cells are classed fragile, sensitive, stable and not testable as design 4.6 defines", {
  x <- qs_fixture()
  run <- qc_sens_run(x, qs_cfg(), nodes = NULL)
  cl <- qc_sens_classify(x, run, qs_cfg())
  got <- setNames(cl$class, cl$iso3)
  expect_equal(got[["FRG"]], "fragile")
  expect_equal(got[["EDG"]], "fragile")
  expect_equal(got[["PRZ"]], "fragile")
  expect_equal(got[["SNS"]], "sensitive")
  expect_equal(got[["DEP"]], "sensitive")
  expect_equal(got[["STB"]], "stable")
  expect_equal(got[["CAP"]], "stable")
  for (k in c("SEC", "NAS", "EXC", "BAD")) expect_equal(got[[k]], "not_testable")
  note <- setNames(cl$note, cl$iso3)
  expect_match(note[["SEC"]], "Secure override")
  expect_match(note[["NAS"]], "not assessed")
  expect_match(note[["EXC"]], "entrepot")
  expect_match(note[["BAD"]], "not the stored preserve")
  # drivers and flip lists
  expect_match(cl$local_flips[cl$iso3 == "FRG"], "need_plus:watch")
  expect_equal(cl$drivers[cl$iso3 == "FRG"], "supply_chain_need")
  expect_match(cl$local_flips[cl$iso3 == "EDG"], "cp_strict:watch")
  expect_match(cl$structural_flips[cl$iso3 == "SNS"], "need_060:partner")
  expect_true(is.na(cl$local_flips[cl$iso3 == "SNS"]))
  expect_match(cl$structural_flips[cl$iso3 == "DEP"], "dn_min_families_2:watch")
  # fence: Partner (medium) -> Watch (large) is a fence change; so is Preserve -> Watch
  expect_true(cl$fence_change[cl$iso3 == "FRG"])
  expect_true(cl$fence_change[cl$iso3 == "EDG"])
  expect_false(cl$fence_change[cl$iso3 == "STB"])
})

test_that("a capped need stored as 0.61 flips at need 0.60 only on the naive reading (the engine sensitivity's artifact)", {
  x <- qs_fixture()
  cfg <- qs_cfg()
  cap <- x[x$iso3 == "CAP", ]
  # the engine's own threshold sensitivity moves the threshold over the stored, capped value
  naive <- sp_choice_threshold_sensitivity(cap, cfg, list(need = 0.60))
  expect_equal(naive$changed_vs_base[naive$parameter == "need"], 1L)
  # cap-aware: the uncapped percentile with the cap rule at 0.60 keeps the need capped (0.54)
  expect_equal(qc_sens_need(cap, 0.60, 0.05, 0.10), 0.54)
  run <- qc_sens_run(x, cfg, nodes = NULL)
  i <- which(x$iso3 == "CAP")
  expect_equal(run$alt$need_060_naive[i], "partner")
  expect_equal(run$alt$need_060[i], "watch")
  long <- qc_sens_long(x, run, qc_sens_classify(x, run, cfg), "pipeline-test")
  f <- long[long$row_type == "flip" & long$iso3 == "CAP", ]
  expect_equal(f$spec_id, "need_060_naive")
  expect_equal(f$spec_kind, "diagnostic")
  # at the configured threshold the cap-aware need reproduces the stored need
  expect_equal(qc_sens_need(x, 0.67, 0.05, 0.10)[x$supply_chain != "Deployment"],
               x$supply_chain_need[x$supply_chain != "Deployment"])
  # a lower floor releases the cap only where the score reaches it (0.03 < 0.05: still capped)
  expect_equal(qc_sens_need(cap, 0.67, 0.05, 0.05), 0.61)
  expect_equal(qc_sens_need(transform(cap, supply_chain_need_score = 0.07), 0.67, 0.05, 0.05), 0.80)
  # the artifact list names the cell, and the summary carries it as a diagnostic row
  art <- qc_sens_need_cap_artifacts(x, run)
  expect_equal(art$case_key, "CAP|Solar|Components")
  expect_equal(art$naive_choice_at_060, "partner")
  expect_equal(art$cap_aware_choice_at_060, "watch")
  s <- qc_sens_summary(qc_sens_classify(x, run, cfg), run, artifacts = art)
  d <- s[s$group_type == "diagnostic", ]
  expect_equal(d$n_changed, 1L)
  expect_match(d$note, "CAP|Solar|Components", fixed = TRUE)
})

test_that("a structural spec that repeats a local one is labelled as unable to classify on its own", {
  x <- qs_fixture()
  run <- qc_sens_run(x, qs_cfg(), nodes = NULL)
  # prize 0.65 is both the structural spec and the local prize threshold + band (0.60 + 0.05)
  expect_match(run$specs$note[run$specs$spec_id == "prize_065"], "same choices as the local spec prize_plus")
  expect_true(is.na(run$specs$note[run$specs$spec_id == "need_060"]))
})

test_that("specs that need node attributes are skipped with a reason, and the summary counts classes", {
  x <- qs_fixture()
  run <- qc_sens_run(x, qs_cfg(), nodes = NULL)
  sk <- run$specs[run$specs$status == "skipped", ]
  expect_setequal(sk$spec_id, c("ly_min_signals_4", "ly_min_signals_6", "siv_gate_033", "siv_unknown_half"))
  expect_true(all(grepl("node attributes", sk$note)))
  cl <- qc_sens_classify(x, run, qs_cfg())
  s <- qc_sens_summary(cl, run, qc_sens_reproduction(x, qs_cfg()), neis = c("FRG"), data_release = "pipeline-test")
  all_row <- s[s$group_type == "overall", ]
  expect_equal(all_row$n_cells, nrow(x))
  expect_equal(all_row$fragile + all_row$sensitive + all_row$stable + all_row$not_testable, nrow(x))
  expect_equal(s$fragile[s$group_type == "neis_economy"], 1L)
  expect_equal(s$status[s$group_type == "spec" & s$group == "siv_gate_033"], "skipped")
  rep_row <- s[s$group_type == "reproduction" & s$group == "choice_rule_rerun", ]
  expect_equal(rep_row$n_changed, 1L) # BAD
  expect_true(all(s$layer == "pipeline"))
})

test_that("Unknown attributes counted 0.5 read as partly, other values untouched", {
  n <- data.frame(tech = "Solar", stage = "Components", process_knowledge = c("Unknown", "yes"), workforce_suppliers = c("", "no"),
                  adjacent_innovation = c(NA, "partly"), maturity = c("Unknown", "mature"), stringsAsFactors = FALSE)
  h <- qc_sens_unknown_half(n)
  expect_equal(h$process_knowledge, c("partly", "yes"))
  expect_equal(h$workforce_suppliers, c("partly", "no"))
  expect_equal(h$adjacent_innovation, c("partly", "partly"))
  expect_equal(h$maturity, c("Unknown", "mature"))
})

test_that("the cap-aware need at 0.60 is what sp_supply_chain_need() gives at 0.60, and the naive reading is not", {
  cfg <- qs_cfg()
  # one producing cell, 40 economies: 22 import nothing (score 0); the tiny scorers rank 0.56-0.66
  n <- 40
  imp <- c(rep(0, 22), 0.01, 0.02, 0.03, 0.05, 0.08, seq(0.2, 0.9, length.out = 13))
  x <- data.frame(iso3 = sprintf("C%02d", seq_len(n)), tech = "Nuclear", supply_chain = "Refining",
                  sn_import_dependence = imp, sn_supplier_concentration = imp, sn_risky_supplier_exposure = imp,
                  strategic_need_percentile = 0.5, trusted_world_share = NA_real_, home_share = 0, sn_import_material = imp > 0,
                  stringsAsFactors = FALSE)
  stored <- sp_supply_chain_need(x, cfg)
  eng60 <- sp_supply_chain_need(x, utils::modifyList(cfg, list(need_threshold = 0.60)))
  # some cell the 0.60 engine caps although the stored 0.67 run left it uncapped (the cap line moves with the threshold)
  moved <- is.na(stored$supply_chain_need_cap) & eng60$supply_chain_need_cap %in% "score_below_floor"
  expect_true(any(moved))
  expect_equal(qc_sens_need(stored, 0.60, 0.05, cfg$supply_chain_need$score_floor), eng60$supply_chain_need)
  # the naive reading (the stored need against 0.60) calls those cells high need; the engine at 0.60 does not
  naive_high <- moved & stored$supply_chain_need >= 0.60
  expect_true(any(naive_high))
  expect_true(all(eng60$supply_chain_need[naive_high] < 0.60))
  rep <- qc_sens_reproduction(transform(stored, Country = iso3, strategic_choice = "watch", engine_posture = "deploy",
                                        strategic_industrial_value = 0.3, industrial_opportunity = 0.3, siv_cap = NA_character_,
                                        deployment_need = NA_real_, cp_contender_pct = 0.1, cp_contender_basis = "contenders",
                                        cp_is_contender = TRUE, t3_capability_lo = "weak", pathway_credible = "no",
                                        large_yard_case = FALSE), cfg)
  r <- rep[rep$check == "need_cap_aware_060_vs_engine", ]
  expect_equal(nrow(r), 1L)
  expect_equal(r$n_mismatch, 0L)
})

test_that("an uncapped need just under the cap line is capped by the cap-aware reading at need 0.60", {
  # stored 0.615 (below the 0.62 cap line at 0.67, so uncapped) with a score under the floor: at 0.60 the line is 0.55
  x <- qs_cell("MOV", supply_chain_need = 0.615, supply_chain_need_uncapped = 0.615, supply_chain_need_score = 0.09)
  expect_equal(qc_sens_need(x, 0.67, 0.05, 0.10), 0.615)
  expect_equal(qc_sens_need(x, 0.60, 0.05, 0.10), 0.54)
})

test_that("the committed long table keeps every flip and the fragile / sensitive class rows; the per-cell file keeps every class", {
  x <- qs_fixture()
  run <- qc_sens_run(x, qs_cfg(), nodes = NULL)
  cl <- qc_sens_classify(x, run, qs_cfg())
  long <- qc_sens_long(x, run, cl, "pipeline-test")
  tr <- qc_sens_trim(long)
  expect_equal(sum(tr$row_type == "flip"), sum(long$row_type == "flip"))
  kept <- tr[tr$row_type == "cell_class", ]
  expect_setequal(kept$class, c("fragile", "sensitive"))
  expect_equal(nrow(kept), sum(cl$class %in% c("fragile", "sensitive")))
  cc <- qc_sens_cell_classes(cl)
  expect_equal(nrow(cc), nrow(cl))
  expect_named(cc, c("iso3", "tech", "stage", "baseline_choice", "class", "fence_change"))
  # written unquoted: no field may hold a comma or a quote
  expect_false(any(grepl("[,\"]", unlist(cc[c("iso3", "tech", "stage", "baseline_choice", "class")]))))
  expect_equal(setNames(cc$class, cc$iso3)[["STB"]], "stable")
  expect_equal(setNames(cc$class, cc$iso3)[["SEC"]], "not_testable")
})
