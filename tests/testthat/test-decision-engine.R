# Guards the canonical decision (R/strategic_position/decision_engine.R; docs/strategic_position.md,
# "Choice"; spec Parts VIII-X and XXII "Decision engine"): Secure override first, Deployment & Use decided
# on deployment_need, Need x Prize triage with Competitive Position deciding the response, not_assessed
# never Watch, and observed policy never an input (alignment is downstream).

de_repo_root <- function() normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(dplyr))
for (f in c("R/utils/supply_chain_stages.R", "R/strategic_position/sp_helpers.R", "R/strategic_position/posture_engine.R",
            "R/strategic_position/decision_engine.R", "R/strategic_position/sp_diag_choice.R")) {
  source(file.path(de_repo_root(), f))
}
de_full_cfg <- function() yaml::read_yaml(file.path(de_repo_root(), "config", "strategic_position.yml"))
de_cfg <- function() de_full_cfg()$decision_framework
de_post <- function() de_full_cfg()$strategic_posture

# One cell as it reaches sp_decision_choice() (engine Deploy, a producing stage, all inputs set).
de_cells <- function(...) {
  base <- list(
    Country = "Testland", iso3 = "TST", tech = "Solar", supply_chain = "Components",
    strategic_posture = "deploy", posture_variant = "deploy", posture_possible = "deploy", pivotal_inputs = NA_character_,
    posture_confidence = "high", posture_exclusion = NA_character_, security_rationale = NA_character_,
    security_routes_met = NA_character_, delivery_mode = "diversified_global",
    decision_trace = "{\"tests\":[],\"posture\":\"deploy\"}",
    supply_chain_need = 0.9, industrial_opportunity = 0.9, deployment_need = NA_real_,
    cp_contender_pct = 0.5, cp_contender_basis = "contenders", cp_is_contender = TRUE, t3_capability_lo = "moderate",
    pathway_credible = "no", pathway_basis = "none_observed_fires",
    home_share = 0.05, trusted_world_share = 0.6, coercive_world_share = 0.3, coercive_top_share = 0.2,
    import_hhi = 0.1, imports_usd = 1e9, stage_evidence_quality_cp = "high", supply_chain_need_n_families = 4L,
    io_maturity_class = "mature"
  )
  args <- list(...)
  base[names(args)] <- args
  as.data.frame(base, stringsAsFactors = FALSE)
}
de <- function(...) sp_decision_choice(de_cells(...), de_cfg(), de_post())

test_that("the Secure override comes first, whatever need, prize and position say", {
  for (n in c(0.1, 0.9)) for (o in c(0.1, 0.9)) for (cp in c(0.1, 0.95)) {
    s <- de(strategic_posture = "secure", posture_variant = "secure_input", security_rationale = "coercive_chokepoint",
            delivery_mode = "allied", supply_chain_need = n, industrial_opportunity = o, cp_contender_pct = cp)
    expect_equal(s$strategic_choice, "secure")
    expect_equal(s$strategic_posture, "secure")
    expect_true(is.na(s$compete_mode))
    expect_true(is.na(s$deploy_mode))
    expect_equal(s$security_rationale, "coercive_chokepoint")
    expect_equal(s$delivery_mode, "allied")
    expect_equal(s$posture_variant, "secure_input")
    expect_equal(s$decision_step, "secure_override")
  }
  # a Secure Deployment control layer is still Secure
  expect_equal(de(supply_chain = "Deployment", strategic_posture = "secure", deployment_need = 0.1)$strategic_choice, "secure")
})

test_that("Deployment & Use is decided on deployment_need: high -> Accelerate, low -> Watch, missing -> not assessed", {
  a <- de(supply_chain = "Deployment", deployment_need = 0.9)
  expect_equal(a$strategic_posture, "deploy")
  expect_equal(a$deploy_mode, "accelerate")
  expect_true(is.na(a$compete_mode))
  expect_equal(a$decision_step, "deployment_need")
  w <- de(supply_chain = "Deployment", deployment_need = 0.2)
  expect_equal(w$deploy_mode, "watch")
  expect_equal(w$strategic_choice, "watch")
  na <- de(supply_chain = "Deployment", deployment_need = NA_real_)
  expect_equal(na$strategic_posture, "not_assessed")
  expect_true(is.na(na$deploy_mode))
  # the supply-chain inputs at Deployment are ignored (no Assembly proxy decides anything there)
  expect_equal(de(supply_chain = "Deployment", deployment_need = 0.9, supply_chain_need = 0.1, industrial_opportunity = 0.1)$deploy_mode,
               "accelerate")
  expect_equal(de(supply_chain = "Deployment", deployment_need = 0.1, supply_chain_need = 0.99, industrial_opportunity = 0.99,
                  cp_contender_pct = 0.99, t3_capability_lo = "strong", pathway_credible = "yes")$deploy_mode, "watch")
})

test_that("a Deployment cell never becomes Partner, Preserve or Catch Up", {
  grid <- expand.grid(dn = c(NA, 0.1, 0.5, 0.66, 0.67, 0.9), n = c(NA, 0.1, 0.9), o = c(NA, 0.1, 0.9), cp = c(0.1, 0.95),
                      pw = c("yes", "no", "unknown"), stringsAsFactors = FALSE)
  x <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) de_cells(
    iso3 = paste0("T", i), supply_chain = "Deployment", deployment_need = grid$dn[i], supply_chain_need = grid$n[i],
    industrial_opportunity = grid$o[i], cp_contender_pct = grid$cp[i], t3_capability_lo = "strong", pathway_credible = grid$pw[i])))
  out <- sp_decision_choice(x, de_cfg(), de_post())
  expect_true(all(is.na(out$compete_mode)))
  expect_true(all(out$strategic_posture %in% c("deploy", "not_assessed")))
  expect_true(all(out$strategic_choice %in% c("accelerate", "watch", "not_assessed")))
})

test_that("high prize: strong position -> Preserve; weak position with a credible pathway -> Catch Up", {
  p <- de(industrial_opportunity = 0.9, cp_contender_pct = 0.9, t3_capability_lo = "strong")
  expect_equal(p$strategic_posture, "compete")
  expect_equal(p$compete_mode, "preserve")
  expect_true(is.na(p$deploy_mode))
  expect_equal(p$delivery_mode, "domestic")
  # preserve whether need is high or low
  expect_equal(de(supply_chain_need = 0.1, industrial_opportunity = 0.9, cp_contender_pct = 0.9)$compete_mode, "preserve")
  c1 <- de(industrial_opportunity = 0.9, cp_contender_pct = 0.3, pathway_credible = "yes", pathway_basis = "feasibility")
  expect_equal(c1$compete_mode, "catch_up")
  expect_equal(de(supply_chain_need = 0.1, industrial_opportunity = 0.9, cp_contender_pct = 0.3, pathway_credible = "yes")$compete_mode,
               "catch_up")
})

test_that("high prize without position or pathway: Partner where need is high, Watch where it is low", {
  expect_equal(de(supply_chain_need = 0.9, industrial_opportunity = 0.9, cp_contender_pct = 0.3, pathway_credible = "no")$compete_mode,
               "partner")
  w <- de(supply_chain_need = 0.2, industrial_opportunity = 0.9, cp_contender_pct = 0.3, pathway_credible = "no")
  expect_equal(w$strategic_choice, "watch")
  expect_match(w$watch_reasons, "weak_domestic_case")
  # an unknown pathway is not credible
  expect_equal(de(supply_chain_need = 0.9, industrial_opportunity = 0.9, cp_contender_pct = 0.3, pathway_credible = "unknown")$compete_mode,
               "partner")
})

test_that("high need, low prize, weak position -> Partner; strong position -> Preserve flagged for review", {
  p <- de(supply_chain_need = 0.9, industrial_opportunity = 0.2, cp_contender_pct = 0.3)
  expect_equal(p$compete_mode, "partner")
  expect_true(is.na(p$consistency_flag))
  r <- de(supply_chain_need = 0.9, industrial_opportunity = 0.2, cp_contender_pct = 0.9, t3_capability_lo = "strong")
  expect_equal(r$compete_mode, "preserve")
  expect_equal(r$consistency_flag, "review_strong_position_low_prize")
  expect_true(r$posture_confidence %in% c("moderate", "low"))
})

test_that("low need and low prize -> Deploy . Watch, a positive conclusion with its reasons", {
  w <- de(supply_chain_need = 0.2, industrial_opportunity = 0.2, cp_contender_pct = 0.9, t3_capability_lo = "strong")
  expect_equal(w$strategic_posture, "deploy")
  expect_equal(w$deploy_mode, "watch")
  expect_true(is.na(w$compete_mode))
  expect_match(w$watch_reasons, "low_need")
  expect_match(w$watch_reasons, "low_prize")
})

test_that("missing need or prize at a producing stage is not assessed, never Watch", {
  for (args in list(list(supply_chain_need = NA_real_), list(industrial_opportunity = NA_real_),
                    list(supply_chain_need = NA_real_, industrial_opportunity = NA_real_))) {
    x <- do.call(de, args)
    expect_equal(x$strategic_choice, "not_assessed")
    expect_equal(x$strategic_posture, "not_assessed")
    expect_true(is.na(x$deploy_mode))
    expect_true(is.na(x$compete_mode))
    expect_equal(x$posture_confidence, "low")
  }
  # an excluded cell (entrepot) is not classified at all
  expect_true(is.na(de(posture_exclusion = "entrepot")$strategic_choice))
})

test_that("high Competitive Position needs the top quarter of contenders and the capability floor", {
  below <- de(industrial_opportunity = 0.9, cp_contender_pct = 0.9, t3_capability_lo = "none", pathway_credible = "no")
  expect_equal(below$compete_mode, "partner")
  # a top-quarter score that fails the capability floor is flagged for review, not silently overridden
  expect_equal(below$consistency_flag, "review_top_quarter_below_floor")
  expect_equal(de(industrial_opportunity = 0.9, cp_contender_pct = 0.9, t3_capability_lo = "none", pathway_credible = "yes")$consistency_flag,
               "review_top_quarter_below_floor")
  expect_true(is.na(de(industrial_opportunity = 0.9, cp_contender_pct = 0.5, t3_capability_lo = "none", pathway_credible = "no")$consistency_flag))
  expect_equal(de(industrial_opportunity = 0.9, cp_contender_pct = 0.9, cp_is_contender = FALSE, pathway_credible = "no")$compete_mode,
               "partner")
  # where no supply share is measured (Recovery) the cross-country percentile stands in, contender or not
  expect_equal(de(industrial_opportunity = 0.9, cp_contender_pct = 0.9, cp_contender_basis = "all_countries", cp_is_contender = FALSE)$compete_mode,
               "preserve")
  # where the node measures supply but has few contenders, a non-producer cannot preserve
  expect_equal(de(industrial_opportunity = 0.9, cp_contender_pct = 0.9, cp_contender_basis = "few_contenders", cp_is_contender = FALSE,
                  pathway_credible = "no")$compete_mode, "partner")
  expect_equal(de(industrial_opportunity = 0.9, cp_contender_pct = 0.9, cp_contender_basis = "few_contenders", cp_is_contender = TRUE)$compete_mode,
               "preserve")
  th <- sp_choice_thresholds(de_cfg())
  expect_equal(th$cp, 0.75)
  expect_equal(de(industrial_opportunity = 0.9, cp_contender_pct = th$cp)$compete_mode, "preserve")   # inclusive
})

test_that("every output respects the posture / mode invariants", {
  grid <- expand.grid(stage = c("Extraction", "Components", "Deployment", "Recovery"), n = c(NA, 0.2, 0.9), o = c(NA, 0.2, 0.9),
                      cp = c(0.3, 0.9), pw = c("yes", "no", "unknown"), dn = c(NA, 0.2, 0.9), sec = c(FALSE, TRUE),
                      stringsAsFactors = FALSE)
  x <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) de_cells(
    iso3 = paste0("T", i), supply_chain = grid$stage[i], supply_chain_need = grid$n[i], industrial_opportunity = grid$o[i],
    cp_contender_pct = grid$cp[i], pathway_credible = grid$pw[i], deployment_need = grid$dn[i],
    strategic_posture = ifelse(grid$sec[i], "secure", "deploy"), posture_variant = ifelse(grid$sec[i], "secure_input", "deploy"),
    security_rationale = ifelse(grid$sec[i], "systemic_resilience", NA_character_))))
  out <- sp_decision_choice(x, de_cfg(), de_post())
  expect_true(all(out$strategic_choice %in% SP_CHOICES))
  expect_true(all(out$strategic_posture %in% SP_CHOICE_POSTURES))
  cmp <- out$strategic_posture == "compete"
  expect_true(all(!is.na(out$compete_mode[cmp]) & is.na(out$deploy_mode[cmp])))
  dep <- out$strategic_posture == "deploy"
  expect_true(all(!is.na(out$deploy_mode[dep]) & is.na(out$compete_mode[dep])))
  oth <- out$strategic_posture %in% c("secure", "not_assessed")
  expect_true(all(is.na(out$compete_mode[oth]) & is.na(out$deploy_mode[oth])))
  expect_true(all(out$deploy_mode[out$supply_chain != "Deployment"] %in% c("watch", NA)))
  expect_true(all(is.na(out$security_rationale[out$strategic_posture != "secure"])))
  expect_true(all(out$delivery_mode[out$strategic_posture %in% c("secure", "compete", "deploy")] %in%
                    c("domestic", "allied", "diversified_global", "mixed")))
  expect_true(all(is.na(out$delivery_mode[out$strategic_posture == "not_assessed"])))
  expect_true(all(out$posture_confidence %in% c("high", "moderate", "low")))
})

test_that("observed policy never determines the recommendation", {
  set.seed(7)
  x <- do.call(rbind, lapply(1:60, function(i) de_cells(
    iso3 = paste0("T", i), supply_chain = sample(c("Refining", "Assembly", "Deployment"), 1),
    supply_chain_need = runif(1), industrial_opportunity = runif(1), cp_contender_pct = runif(1), deployment_need = runif(1),
    pathway_credible = sample(c("yes", "no", "unknown"), 1))))
  a <- x
  a$policy_priority_percentile <- runif(60)
  a$policy_support_priority <- runif(60)
  a$policy_protection_priority <- runif(60)
  a$nipo_priority <- runif(60)
  a$pams_priority <- runif(60)
  b <- a
  b[, c("policy_priority_percentile", "policy_support_priority", "policy_protection_priority", "nipo_priority", "pams_priority")] <-
    1 - b[, c("policy_priority_percentile", "policy_support_priority", "policy_protection_priority", "nipo_priority", "pams_priority")]
  cols <- c("strategic_choice", "strategic_posture", "compete_mode", "deploy_mode", "posture_confidence", "decision_borderline",
            "consistency_flag", "delivery_mode", "decision_trace")
  ref <- sp_decision_choice(x, de_cfg(), de_post())[, cols]
  expect_identical(sp_decision_choice(a, de_cfg(), de_post())[, cols], ref)
  expect_identical(sp_decision_choice(b, de_cfg(), de_post())[, cols], ref)
  # the rule's own arguments carry no policy input
  expect_false(any(grepl("policy", names(formals(sp_choice_rule)))))
})

test_that("borderline inputs are named, lower confidence and give the other possible choices", {
  th <- sp_choice_thresholds(de_cfg())
  b <- de(supply_chain_need = th$need + 0.01, industrial_opportunity = 0.2, cp_contender_pct = 0.3)
  expect_equal(b$compete_mode, "partner")
  expect_equal(b$decision_borderline, "supply_chain_need")
  expect_equal(b$choice_possible, "partner|watch")
  expect_equal(b$posture_possible, "compete|deploy")
  expect_match(b$pivotal_inputs, "supply_chain_need")
  expect_true(b$posture_confidence %in% c("moderate", "low"))
  clear <- de(supply_chain_need = 0.95, industrial_opportunity = 0.1, cp_contender_pct = 0.1)
  expect_true(is.na(clear$decision_borderline))
  expect_equal(clear$choice_possible, "partner")
  d <- de(supply_chain = "Deployment", deployment_need = th$deployment - 0.01)
  expect_equal(d$decision_borderline, "deployment_need")
  expect_equal(d$choice_possible, "accelerate|watch")
})

test_that("the compact decision trace is a JSON array of {step, test, value, threshold, result}", {
  x <- rbind(de_cells(iso3 = "A", industrial_opportunity = 0.9, cp_contender_pct = 0.3, pathway_credible = "yes", pathway_basis = "pipeline"),
             de_cells(iso3 = "B", supply_chain = "Deployment", deployment_need = 0.8),
             de_cells(iso3 = "C", strategic_posture = "secure", posture_variant = "secure_input", security_routes_met = "physical"))
  out <- sp_decision_choice(x, de_cfg(), de_post())
  tr <- lapply(out$decision_trace, jsonlite::fromJSON, simplifyVector = FALSE)
  for (t in tr) {
    expect_true(all(vapply(t, function(s) all(c("step", "test", "result") %in% names(s)), logical(1))))
    expect_equal(t[[1]]$step, "secure")
    expect_equal(t[[length(t)]]$step, "choice")
  }
  expect_equal(vapply(tr[[1]], `[[`, "", "step"), c("secure", "need", "prize", "position", "pathway", "choice"))
  expect_equal(tr[[1]][[5]]$value, "pipeline")
  expect_equal(tr[[1]][[6]]$result, "catch_up")
  expect_equal(vapply(tr[[2]], `[[`, "", "test"), c("secure_override", "deployment_need", "choice"))
  expect_equal(tr[[2]][[2]]$result, "high")
  expect_equal(vapply(tr[[3]], `[[`, "", "step"), c("secure", "choice"))
  # the engine's full trace is kept
  expect_true(all(!is.na(out$engine_trace)))
  expect_true(all(nchar(out$decision_trace) < 700))
})

test_that("the engine's own reading survives as engine_* diagnostics", {
  x <- de(strategic_posture = "compete", posture_variant = "compete_catch_up", supply_chain_need = 0.2, industrial_opportunity = 0.2)
  expect_equal(x$engine_posture, "compete")
  expect_equal(x$engine_variant, "compete_catch_up")
  expect_equal(x$strategic_choice, "watch")
})

test_that("choice reasons and threshold sensitivity come from the stored inputs", {
  x <- rbind(de_cells(iso3 = "A", industrial_opportunity = 0.9, cp_contender_pct = 0.9),
             de_cells(iso3 = "B", supply_chain_need = 0.9, industrial_opportunity = 0.2, cp_contender_pct = 0.3),
             de_cells(iso3 = "C", supply_chain = "Deployment", deployment_need = 0.9),
             de_cells(iso3 = "D", supply_chain_need = NA_real_))
  out <- sp_decision_choice(x, de_cfg(), de_post())
  r <- sp_choice_reason(out, de_cfg())
  expect_match(r[1], "Preserve")
  expect_match(r[2], "Partner")
  expect_match(r[3], "says nothing about where the equipment is made")
  expect_match(r[4], "not assessed")
  s <- sp_choice_threshold_sensitivity(out, de_cfg(), list(cp = c(0.95)))
  expect_equal(s$changed_vs_base[s$parameter == "base"], 0L)
  expect_equal(unname(unlist(s[s$parameter == "base", SP_CHOICES])), as.numeric(table(factor(out$strategic_choice, SP_CHOICES))))
  expect_gt(s$changed_vs_base[s$parameter == "cp"], 0)
})

test_that("Competitive Position among contenders uses the contenders where there are enough of them", {
  x <- data.frame(iso3 = paste0("C", 1:8), tech = "Solar", supply_chain = "Components",
                  competitive_position_score = c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.95),
                  competitive_position_percentile = seq(0.1, 0.8, by = 0.1),
                  home_share = c(0.2, 0.1, 0.05, 0.02, 0.01, 0.001, 0.001, 0.001))
  out <- sp_cp_contender_pct(x, list(contenders = list(min_share = 0.005, min_n = 5)))
  expect_equal(out$cp_contender_basis, rep("contenders", 8))
  expect_equal(out$cp_n_contenders, rep(5L, 8))
  expect_equal(out$cp_contender_pct[1], 0.9)   # the best contender: 4 below + half itself, of 5
  expect_equal(out$cp_is_contender, c(rep(TRUE, 5), FALSE, FALSE, FALSE))
  few <- sp_cp_contender_pct(x, list(contenders = list(min_share = 0.05, min_n = 5)))
  expect_equal(few$cp_contender_basis, rep("few_contenders", 8))
  expect_equal(few$cp_contender_pct, x$competitive_position_percentile)
  none <- sp_cp_contender_pct(transform(x, home_share = NA_real_), list(contenders = list(min_share = 0.005, min_n = 5)))
  expect_equal(none$cp_contender_basis, rep("all_countries", 8))
  expect_false(any(none$cp_is_contender))
  # an economy a partial listing omits carries the engine's bracket (the listing's residual, here 0.0087)
  # in home_share but a measured share of 0: it is not a contender (the 2026-10-29 Batteries Refining case)
  br <- transform(x, home_share = c(0.2, 0.1, 0.05, 0.02, 0.01, 0.0087, 0.0087, 0.0087),
                  home_share_exposure = c(0.2, 0.1, 0.05, 0.02, 0.01, 0, 0, 0))
  out_br <- sp_cp_contender_pct(br, list(contenders = list(min_share = 0.005, min_n = 5)))
  expect_equal(out_br$cp_is_contender, c(rep(TRUE, 5), FALSE, FALSE, FALSE))
  expect_equal(out_br$cp_n_contenders, rep(5L, 8))
  expect_equal(sp_measured_share(br), br$home_share_exposure)
  expect_equal(sp_measured_share(x), x$home_share)
})

test_that("policy alignment follows spec X and never feeds back", {
  a_cfg <- de_full_cfg()$policy_alignment
  mk <- function(choice_args, ...) {
    x <- do.call(de, choice_args)
    pol <- list(...)
    for (n in names(pol)) x[[n]] <- pol[[n]]
    for (n in setdiff(c("policy_priority_percentile", "nipo_priority", "pams_priority", "policy_support_priority",
                        "policy_protection_priority", "deployment_localisation_priority"), names(x))) x[[n]] <- NA_real_
    x$pp_n_sources <- sum(!is.na(c(x$nipo_priority, x$pams_priority)))
    x
  }
  al <- function(x) sp_policy_alignment(x, a_cfg, has_record = TRUE)
  conf <- function(x) sp_policy_alignment_confidence(x, al(x), a_cfg, has_record = TRUE)
  sec <- list(strategic_posture = "secure", posture_variant = "secure_input")
  expect_equal(al(mk(sec, policy_priority_percentile = 0.2, nipo_priority = 0.2, pams_priority = 0.2)), "under_prioritized")
  expect_equal(al(mk(sec, policy_priority_percentile = 0.8, nipo_priority = 0.8, pams_priority = 0.8)), "aligned")
  pres <- list(industrial_opportunity = 0.9, cp_contender_pct = 0.9)
  expect_equal(al(mk(pres, policy_priority_percentile = 0.9, nipo_priority = 0.9, pams_priority = 0.9, policy_support_priority = 0.2,
                     policy_protection_priority = 0.5)), "under_prioritized")
  cu <- list(industrial_opportunity = 0.9, cp_contender_pct = 0.3, pathway_credible = "yes")
  expect_equal(al(mk(cu, policy_priority_percentile = 0.6, nipo_priority = 0.6, pams_priority = 0.6, policy_support_priority = 0.1,
                     policy_protection_priority = 0.4)), "under_prioritized")
  par <- list(supply_chain_need = 0.9, industrial_opportunity = 0.2, cp_contender_pct = 0.3)
  m <- mk(par, policy_priority_percentile = 0.5, nipo_priority = 0.5, pams_priority = 0.5, policy_support_priority = 0.5,
          policy_protection_priority = 0.9)
  expect_equal(al(m), "misaligned")
  expect_equal(sp_policy_alignment_flag(m, a_cfg, has_record = TRUE), "partner_autarkic")
  acc <- list(supply_chain = "Deployment", deployment_need = 0.9)
  g <- mk(acc, policy_priority_percentile = 0.2, pams_priority = 0.2)
  expect_equal(al(g), "deployment_gap")
  expect_equal(sp_policy_alignment_flag(g, a_cfg, has_record = TRUE), "weak_deployment_policy")
  expect_equal(conf(g), "medium")    # one source at Deployment by construction
  expect_equal(al(mk(acc, policy_priority_percentile = 0.8, pams_priority = 0.8)), "aligned")
  wat <- list(supply_chain_need = 0.2, industrial_opportunity = 0.2)
  o <- mk(wat, policy_priority_percentile = 0.5, nipo_priority = 0.5, pams_priority = 0.5, policy_support_priority = 0.3,
          policy_protection_priority = 0.9)
  expect_equal(al(o), "over_prioritized")
  expect_equal(sp_policy_alignment_flag(o, a_cfg, has_record = TRUE), "watch_protection")
  expect_equal(conf(o), "high")
  # no support / protection split: judged on total attention, with low confidence
  oa <- mk(wat, policy_priority_percentile = 0.9, pams_priority = 0.9)
  expect_equal(al(oa), "over_prioritized")
  expect_equal(conf(oa), "low")
  # a Watch deployment whose equipment carries heavy local-content protection
  dw <- mk(list(supply_chain = "Deployment", deployment_need = 0.2), policy_priority_percentile = 0.4, pams_priority = 0.4,
           deployment_localisation_priority = 0.9)
  expect_equal(al(dw), "over_prioritized")
  expect_equal(sp_policy_alignment_flag(dw, a_cfg, has_record = TRUE), "watch_localisation_deployment")
  # incomplete data: never a guess
  expect_equal(al(mk(sec)), "insufficient_data")
  expect_equal(conf(mk(sec)), "low")
  expect_equal(al(mk(list(supply_chain_need = NA_real_), policy_priority_percentile = 0.9)), "insufficient_data")
  expect_true(all(c(al(g), al(o), al(m)) %in% SP_POLICY_ALIGNMENTS))
})

test_that("the position-choice consistency counts read the right columns", {
  x <- data.frame(competitive_position_percentile = c(0.95, 0.95, 0.1, 0.5), cp_contender_pct = c(0.9, 0.95, 0.1, 0.4),
                  home_share = c(0.2, 0.15, 0.001, 0.02))
  out <- sp_consistency_counts(x, c("partner", "catch_up", "preserve", "watch"), "test")
  expect_equal(out$partner_despite_cp_x75, 1)
  expect_equal(out$catch_up_despite_cp_x90, 1)
  expect_equal(out$catch_up_despite_leading_share10, 1)
  expect_equal(out$preserve_without_capability, 1)
  expect_equal(out$preserve_cp_contender_below50, 1)
})
