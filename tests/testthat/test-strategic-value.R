# Guards the Prize as Strategic Industrial Value and the large-yard case (R/strategic_position/strategic_value.R;
# docs/strategic_position.md, "2. Prize: Strategic Industrial Value" and "The large-yard case"; user spec
# 2026-10-30, Yards & Fences items 2 and 3): three families, the market-only cap, no capability or need input,
# the ten large-yard signals and the case, and their place in the choice rule. Fixtures are synthetic.

sv_repo_root <- function() normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(dplyr))
for (f in c("R/utils/supply_chain_stages.R", "R/strategic_position/sp_helpers.R", "R/strategic_position/posture_engine.R",
            "R/strategic_position/industrial_opportunity.R", "R/strategic_position/strategic_value.R",
            "R/strategic_position/decision_engine.R", "R/strategic_position/yard_rollup.R")) {
  source(file.path(sv_repo_root(), f))
}
sv_full_cfg <- function() yaml::read_yaml(file.path(sv_repo_root(), "config", "strategic_position.yml"))
sv_cfg <- function() sv_full_cfg()$decision_framework
sv_post <- function() sv_full_cfg()$strategic_posture

# Node attributes of one row: a mature, passive commodity by default (every large-yard signal holds).
sv_attr <- function(...) {
  base <- list(maturity = "mature", digitally_active = "no", standardization = "high", differentiation = "low",
               commoditization = "yes", substitutability = "easy", qualification_time = "lt3m", process_knowledge = "no",
               workforce_suppliers = "no", adjacent_innovation = "no", leap_candidate = "no", successor = "",
               beyond_novelty = "", cost_perf_advantage = "", incumbent_vulnerable = "", scale_pathway = "")
  a <- list(...)
  base[names(a)] <- a
  as.data.frame(base, stringsAsFactors = FALSE)
}
# Rows (iso3, tech, supply_chain, layer_id, market prize, world supply) and their attributes, one per spec.
sv_rows <- function(specs) {
  # a null-only default: an explicit NA in a spec must stay NA, whatever another test file made of %||%
  or <- function(a, b) if (is.null(a)) b else a
  rows <- lapply(seq_along(specs), function(i) {
    s <- specs[[i]]
    data.frame(iso3 = or(s$iso3, "USA"), tech = or(s$tech, "Solar"), supply_chain = or(s$stage, "Assembly"),
               layer_id = or(s$layer, paste0("l", i)), industrial_opportunity = or(s$market, 0.5),
               world_top_share = or(s$top, 0.3), coercive_top_share = or(s$coer, 0.2), stringsAsFactors = FALSE)
  })
  tbl <- do.call(rbind, rows)
  attrs <- do.call(rbind, lapply(seq_along(specs), function(i) cbind(tbl[i, c("iso3", "tech", "layer_id")], stage = tbl$supply_chain[i],
                                                                    do.call(sv_attr, or(specs[[i]]$attr, list())))))
  list(tbl = tbl, attrs = attrs)
}
sv_rich <- list(process_knowledge = "yes", workforce_suppliers = "yes", adjacent_innovation = "yes", maturity = "scaling")
sv_leap <- list(leap_candidate = "yes", successor = "tandem cells", beyond_novelty = "yes", cost_perf_advantage = "yes",
                incumbent_vulnerable = "partly", scale_pathway = "partly")

# ---- Strategic industrial value -------------------------------------------------------------------------------
test_that("the three families: market unchanged, commons and option on yes 1 / partly 0.5 / no 0, Unknown 0", {
  r <- sv_rows(list(
    list(market = 0.8, attr = list(process_knowledge = "yes", workforce_suppliers = "partly", adjacent_innovation = "Unknown")),
    list(market = 0.4, attr = c(sv_rich, sv_leap)),
    list(market = 0.6, attr = list(process_knowledge = "Unknown", workforce_suppliers = "Unknown", adjacent_innovation = "Unknown",
                                   leap_candidate = "Unknown")),
    list(market = 0.9, stage = "Deployment")
  ))
  x <- sp_strategic_value(r$tbl, r$attrs, sv_cfg())
  expect_true(all(SP_SIV_COLUMNS %in% names(x)))
  expect_equal(x$siv_market[1:3], c(0.8, 0.4, 0.6))
  expect_equal(x$siv_commons[1], 0.5)                   # (yes 1 + partly 0.5 + Unknown 0) / 3: the strict reading
  expect_equal(x$siv_commons_n[1], 2L)
  expect_equal(x$siv_option[1], 0)                      # not a leap candidate: 0
  expect_equal(x$siv_option_n[1], 0L)
  expect_equal(x$siv_commons[2], 1)
  expect_equal(x$siv_option[2], (1 + 1 + 0.5 + 0.5) / 4)  # the four leap tests; candidacy and the successor gate, never score
  expect_equal(x$siv_option_n[2], 4L)
  expect_equal(c(x$siv_commons[3], x$siv_option[3]), c(0, 0))   # nothing observed is no value, never a missing family
  expect_equal(x$siv_families_observed[1:3], c(3L, 3L, 1L))
  # equal weights over the three families
  expect_equal(x$siv_score[1], mean(c(0.8, 0.5, 0)))
  expect_equal(x$siv_score[3], 0.6 / 3)
  expect_equal(x$siv_cap[1:3], c(NA, NA, "market_only"))
  # a mid-rank percentile across the producing rows; Deployment is decided on deployment_need
  expect_equal(x$strategic_industrial_value[1:3], sp_midrank_pct(x$siv_score[1:3]))
  expect_true(all(is.na(unlist(x[4, SP_SIV_COLUMNS[SP_SIV_COLUMNS != "siv_cap"]]))))
  # layers are placed among the cells' scores
  ref <- c(0.2, 0.4, x$siv_score[2], 0.9)
  y <- sp_strategic_value(r$tbl, r$attrs, sv_cfg(), ref = ref)
  expect_equal(y$strategic_industrial_value[1], 2 / 4)            # two below
  expect_equal(y$strategic_industrial_value[2], (2 + 0.5) / 4)    # two below, one equal
  # without the market prize the node stays not assessed
  r$tbl$industrial_opportunity[2] <- NA
  expect_true(is.na(sp_strategic_value(r$tbl, r$attrs, sv_cfg())$strategic_industrial_value[2]))
})

test_that("less evidence never lifts the prize: unobserved commons count 0 and the market-only cap still applies", {
  # the review's case: a top market prize, the three commons attributes Unknown, among six rows with commons "partly"
  specs <- c(list(list(stage = "Recovery", market = 1, attr = list(process_knowledge = "Unknown", workforce_suppliers = "Unknown",
                                                                     adjacent_innovation = "Unknown"))),
             lapply(1:6, function(i) list(market = 0.1 * i, attr = list(process_knowledge = "partly", workforce_suppliers = "partly",
                                                                       adjacent_innovation = "partly"))))
  r <- sv_rows(specs)
  unk <- sp_strategic_value(r$tbl, r$attrs, sv_cfg())
  r$attrs[1, c("process_knowledge", "workforce_suppliers", "adjacent_innovation")] <- "no"
  obs <- sp_strategic_value(r$tbl, r$attrs, sv_cfg())
  expect_equal(unk$siv_cap[1], "market_only")
  expect_equal(obs$siv_cap[1], "market_only")
  expect_lte(unk$strategic_industrial_value[1], obs$strategic_industrial_value[1])
  expect_equal(unk$siv_commons_n[1], 0L)
  # one "yes" among three questions is a third of the family, not all of it (a control layer with learning Unknown)
  r1 <- sv_rows(list(list(attr = list(process_knowledge = "Unknown", workforce_suppliers = "Unknown", adjacent_innovation = "yes"))))
  expect_equal(sp_strategic_value(r1$tbl, r1$attrs, sv_cfg())$siv_commons, 1 / 3)
})

test_that("future option: the four leap tests of a leap candidate; candidacy alone is worth nothing", {
  r <- sv_rows(list(list(attr = list(leap_candidate = "yes", successor = "x", beyond_novelty = "no", cost_perf_advantage = "no",
                                     incumbent_vulnerable = "no", scale_pathway = "no")),
                    list(attr = list(leap_candidate = "yes", successor = "x", beyond_novelty = "partly", cost_perf_advantage = "partly",
                                     incumbent_vulnerable = "partly", scale_pathway = "partly")),
                    list(attr = list(leap_candidate = "yes", successor = "x", beyond_novelty = "yes", cost_perf_advantage = "Unknown",
                                     incumbent_vulnerable = "partly", scale_pathway = "")),
                    list(attr = list(leap_candidate = "no", scale_pathway = "yes", cost_perf_advantage = "yes"))))
  x <- sp_strategic_value(r$tbl, r$attrs, sv_cfg())
  expect_equal(x$siv_option, c(0, 0.5, 1.5 / 4, 0))
  expect_equal(x$siv_option_n, c(4L, 4L, 2L, 0L))
  # the paper's leap test (gates.ts leapCredible), tri-state
  a <- sp_node_attrs(r$tbl, r$attrs, SP_SIV_OPTION)
  expect_equal(sp_leap_credible(a), c(FALSE, FALSE, NA, FALSE))
  a2 <- a
  a2$cost_perf_advantage[3] <- "yes"
  a2$scale_pathway[3] <- "partly"
  expect_true(sp_leap_credible(a2)[3])
  # one node support: undecided, since the country's complementary capabilities (not a node attribute) could be
  # the second; with no node support it fails at both ends
  a2$scale_pathway[3] <- "no"
  expect_true(is.na(sp_leap_credible(a2)[3]))
  a2$incumbent_vulnerable[3] <- "no"
  expect_false(sp_leap_credible(a2)[3])
})

test_that("market only: a top market prize with low commons and option is not high", {
  r <- sv_rows(c(list(list(market = 1, attr = list(workforce_suppliers = "partly"))),
                 lapply(1:6, function(i) list(market = 0.1 + 0.01 * i, attr = list(adjacent_innovation = "partly")))))
  x <- sp_strategic_value(r$tbl, r$attrs, sv_cfg())
  th <- sp_choice_thresholds(sv_cfg())
  expect_equal(which.max(x$siv_score), 1L)              # the highest score of all...
  expect_equal(x$siv_cap[1], "market_only")
  expect_lt(x$strategic_industrial_value[1], th$prize)  # ...held below the prize threshold
  expect_equal(x$strategic_industrial_value[1], th$prize - sv_cfg()$strategic_value$cap_margin)
  # commons or option at the gate lifts the cap
  r2 <- r
  r2$attrs$process_knowledge[1] <- "yes"
  r2$attrs$workforce_suppliers[1] <- "yes"
  x2 <- sp_strategic_value(r2$tbl, r2$attrs, sv_cfg())
  expect_true(is.na(x2$siv_cap[1]))
  expect_gte(x2$strategic_industrial_value[1], th$prize)
  # in the choice: the market prize alone would be high, the strategic value is not, and the cap is never borderline
  cells <- cbind(x[1, ], Country = "Testland", strategic_posture = "deploy", posture_variant = "deploy", posture_possible = "deploy",
                 pivotal_inputs = NA_character_, posture_confidence = "high", posture_exclusion = NA_character_,
                 security_rationale = NA_character_, delivery_mode = "diversified_global", supply_chain_need = 0.2,
                 cp_contender_pct = 0.9, cp_contender_basis = "contenders", cp_is_contender = TRUE, t3_capability_lo = "strong",
                 pathway_credible = "no", supply_chain_need_n_families = 4L, stage_evidence_quality_cp = "high",
                 large_yard_case = FALSE, large_yard_n = 0L, stringsAsFactors = FALSE)
  cells$cp_contender_pct <- 0.3
  d <- sp_decision_choice(cells, sv_cfg(), sv_post())
  expect_false(d$prize_high)
  expect_equal(d$strategic_choice, "watch")
  expect_match(d$watch_reasons, "low_prize")
  expect_true(is.na(d$decision_borderline))
  expect_match(sp_choice_reason(d, sv_cfg()), "the market prize alone")
  m_cfg <- sv_cfg()
  m_cfg$strategic_value$prize_measure <- "market"
  expect_true(sp_decision_choice(cells, m_cfg, sv_post())$prize_high)
})

test_that("no Competitive Position, capability or need variable enters the strategic industrial value", {
  for (bad in c("cp_contender_pct", "competitive_position_percentile", "home_share", "home_share_exposure", "pw_legs",
                "pathway_credible", "supply_chain_need", "import_hhi", "market_share", "rca", "t3_capability_lo", "sn_import_dependence")) {
    expect_error(sp_siv_check_inputs(c("industrial_opportunity", bad)), "Competitive Position, capability or need", info = bad)
  }
  expect_silent(sp_siv_check_inputs(c("industrial_opportunity", SP_SIV_COMMONS, SP_SIV_OPTION)))
  cfg <- sv_cfg()
  cfg$strategic_value$market_column <- "competitive_position_percentile"
  r <- sv_rows(list(list(market = 0.5), list(market = 0.7, attr = sv_rich)))
  expect_error(sp_strategic_value(r$tbl, r$attrs, cfg), "Competitive Position")
  # changing every position and need column leaves the value unchanged
  a <- r$tbl
  b <- r$tbl
  for (cl in c("cp_contender_pct", "competitive_position_percentile", "home_share", "home_share_exposure", "supply_chain_need",
               "strategic_need_percentile", "pathway_credible")) {
    a[[cl]] <- c(0.1, 0.9)
    b[[cl]] <- c(0.9, 0.1)
  }
  a$pathway_credible <- c("yes", "no")
  b$pathway_credible <- c("no", "yes")
  expect_identical(sp_strategic_value(a, r$attrs, sv_cfg())[, SP_SIV_COLUMNS], sp_strategic_value(b, r$attrs, sv_cfg())[, SP_SIV_COLUMNS])
  # the country's own complementary capabilities are not a node attribute of the value
  expect_false("complementary_capabilities" %in% c(SP_SIV_COMMONS, SP_SIV_OPTION))
})

# ---- The large-yard case ---------------------------------------------------------------------------------------
test_that("the ten signals read the node attributes and the world supply structure", {
  r <- sv_rows(list(list(top = 0.3), list(top = 0.6), list(top = NA, coer = 0.7), list(top = NA, coer = 0.2),
                    list(attr = list(maturity = "scaling", digitally_active = "yes", standardization = "moderate", differentiation = "high",
                                     commoditization = "partly", substitutability = "hard", qualification_time = "y1to3",
                                     process_knowledge = "partly", adjacent_innovation = "yes"), top = 0.9),
                    list(attr = list(standardization = "Unknown", differentiation = "", commoditization = "Unknown"))))
  s <- sp_large_yard_signals(r$tbl, r$attrs, sv_cfg())
  expect_equal(names(s), SP_LY_SIGNALS)
  expect_true(all(unlist(s[1, ])))
  expect_equal(s$diversified, c(TRUE, FALSE, FALSE, NA, FALSE, TRUE))   # a coercive share at the line is a lower bound
  # a coercive supplier at the line is not diversified whatever the largest-supplier share says (another scope)
  r2 <- sv_rows(list(list(top = 0.3, coer = 0.6), list(top = 0.3, coer = 0.49)))
  expect_equal(sp_large_yard_signals(r2$tbl, r2$attrs, sv_cfg())$diversified, c(FALSE, TRUE))
  expect_false(any(unlist(s[5, ])))
  expect_true(all(is.na(unlist(s[6, c("standardized", "low_differentiation", "commoditized")]))))
  # a table without the three new attribute columns reads them as not observed
  s0 <- sp_large_yard_signals(r$tbl, r$attrs[, setdiff(names(r$attrs), c("standardization", "differentiation", "commoditization"))], sv_cfg())
  expect_true(all(is.na(s0$standardized)))
})

test_that("a mature, passive, standardized, substitutable, low-learning node is a large yard even with the top market prize", {
  r <- sv_rows(list(list(market = 1, attr = list(qualification_time = "m3to12", adjacent_innovation = "partly", commoditization = "partly",
                                                 differentiation = "moderate")),
                    list(market = 0.2)))
  x <- sp_large_yard_case(sp_strategic_value(r$tbl, r$attrs, sv_cfg()), r$attrs, sv_cfg())
  expect_true(x$large_yard_case[1])
  expect_equal(x$large_yard_n[1], 5L)                      # standardized, substitutable, easy_qualification, diversified, low_learning
  expect_match(x$large_yard_signals[1], "^mature;passive;standardized;substitutable;easy_qualification;diversified;low_learning$")
  expect_match(x$large_yard_reason[1], "whatever the size of the market")
  # market size is never a signal: the case does not move with the market prize
  r2 <- r
  r2$tbl$industrial_opportunity <- c(0, 0)
  expect_identical(sp_large_yard_case(r2$tbl, r$attrs, sv_cfg())[, c("large_yard_case", "large_yard_signals", "large_yard_n")],
                   sp_large_yard_case(r$tbl, r$attrs, sv_cfg())[, c("large_yard_case", "large_yard_signals", "large_yard_n")])
})

test_that("a credible leap, strong learning, a scaling node and a digitally active one are not large yards", {
  r <- sv_rows(list(list(attr = sv_leap), list(attr = sv_rich), list(attr = list(digitally_active = "yes")),
                    list(attr = list(standardization = "low", differentiation = "high", commoditization = "no", substitutability = "hard",
                                     qualification_time = "y1to3", process_knowledge = "partly", adjacent_innovation = "partly"), top = 0.9),
                    list(attr = list(maturity = "scaling"))))
  x <- sp_large_yard_case(sp_strategic_value(r$tbl, r$attrs, sv_cfg()), r$attrs, sv_cfg())
  expect_equal(x$large_yard_case, c(FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_match(x$large_yard_reason[1], "credible leap ahead")
  expect_match(x$large_yard_reason[2], "strong industrial learning \\(3.0 of 3")
  expect_match(x$large_yard_reason[3], "digitally active")
  expect_match(x$large_yard_reason[4], "none of the 8 commodity signals holds")
  expect_match(x$large_yard_reason[5], "not mature \\(scaling\\)")
  # leap candidacy alone is not a veto: a mature commodity candidate whose leap test fails (no evidence beyond
  # novelty) is a large yard; one whose test an unknown could pass is undecided
  r5 <- sv_rows(list(list(attr = list(leap_candidate = "yes", successor = "x", beyond_novelty = "partly", cost_perf_advantage = "partly",
                                     incumbent_vulnerable = "partly", scale_pathway = "partly")),
                     list(attr = list(leap_candidate = "yes", successor = "x", beyond_novelty = "Unknown", cost_perf_advantage = "yes",
                                     incumbent_vulnerable = "partly", scale_pathway = "partly")),
                     list(attr = list(leap_candidate = "no", scale_pathway = "yes", cost_perf_advantage = "yes"))))
  x5 <- sp_large_yard_case(sp_strategic_value(r5$tbl, r5$attrs, sv_cfg()), r5$attrs, sv_cfg())
  expect_equal(x5$large_yard_case, c(TRUE, NA, TRUE))
  expect_match(x5$large_yard_reason[2], "whether a leap ahead is credible is not assessed")
  # switched off, the leap test does not veto
  off <- sv_cfg()
  off$large_yard$leap_veto <- FALSE
  expect_true(sp_large_yard_case(r$tbl[1, ], r$attrs[1, ], off)$large_yard_case)
  # the retired option_veto setting is refused, so an old config cannot silently mean something else
  old <- sv_cfg()
  old$large_yard$option_veto <- 0.5
  expect_error(sp_ly_cfg(old), "leap_veto")
})

test_that("moderate learning leaves a capability floor, strong learning vetoes, undecided learning is undecided", {
  r <- sv_rows(list(list(attr = list(process_knowledge = "partly", workforce_suppliers = "partly", adjacent_innovation = "partly")),
                    list(attr = list(process_knowledge = "yes", workforce_suppliers = "yes", adjacent_innovation = "no")),
                    list(attr = list(process_knowledge = "Unknown", workforce_suppliers = "yes", adjacent_innovation = "no")),
                    list()))
  x <- sp_large_yard_case(sp_strategic_value(r$tbl, r$attrs, sv_cfg()), r$attrs, sv_cfg())
  expect_equal(x$large_yard_case, c(TRUE, FALSE, NA, TRUE))
  expect_equal(x$large_yard_floor, c(TRUE, FALSE, FALSE, FALSE))
  expect_match(x$large_yard_reason[1], "^Large yard with a capability floor: .*moderate industrial learning \\(1.5 of 3")
  expect_match(x$large_yard_reason[2], "strong industrial learning \\(2.0 of 3")
  expect_match(x$large_yard_reason[3], "industrial learning not fully assessed")
  expect_match(x$large_yard_reason[4], "^Large yard: .*no Medium-yard rationale")
  # in the choice: the floor is the paper's large-floor variant
  cell <- cbind(x[1, ], Country = "Testland", strategic_posture = "deploy", posture_variant = "deploy", posture_possible = "deploy",
                pivotal_inputs = NA_character_, posture_confidence = "high", posture_exclusion = NA_character_, security_rationale = NA_character_,
                delivery_mode = "diversified_global", supply_chain_need = 0.9, cp_contender_pct = 0.2, cp_contender_basis = "contenders",
                cp_is_contender = TRUE, t3_capability_lo = "weak", pathway_credible = "no", supply_chain_need_n_families = 4L,
                stage_evidence_quality_cp = "high", stringsAsFactors = FALSE)
  d <- sp_decision_choice(cell, sv_cfg(), sv_post())
  expect_equal(d$strategic_choice, "watch")
  expect_equal(d$posture_variant, "deploy_floor")
  # the switch-off of the learning veto and floor (large_yard_sensitivity.csv no_learning_veto)
  off <- sv_cfg()
  off$large_yard$learning_veto_points <- Inf
  off$large_yard$learning_floor_points <- Inf
  y <- sp_large_yard_case(r$tbl, r$attrs, off)
  expect_equal(y$large_yard_case, c(TRUE, TRUE, TRUE, TRUE))
  expect_false(any(y$large_yard_floor))
})

test_that("a case that fails for want of one signal is marked near", {
  r <- sv_rows(list(list(attr = list(standardization = "moderate", differentiation = "moderate", commoditization = "partly",
                                     qualification_time = "y1to3")),
                    list(attr = list(standardization = "moderate", differentiation = "moderate", commoditization = "partly",
                                     qualification_time = "y1to3", substitutability = "hard")),
                    list(attr = list(standardization = "moderate", differentiation = "moderate", commoditization = "Unknown",
                                     qualification_time = "y1to3"))))
  x <- sp_large_yard_case(r$tbl, r$attrs, sv_cfg())
  expect_equal(x$large_yard_n, c(4L, 3L, 4L))
  expect_equal(x$large_yard_case, c(FALSE, FALSE, NA))
  expect_equal(x$large_yard_near, c(TRUE, FALSE, FALSE))  # undecided is not near: it is undecided
})

test_that("an undecided case is NA and names what is missing; too few possible signals is FALSE", {
  r <- sv_rows(list(list(attr = list(standardization = "Unknown", differentiation = "Unknown", commoditization = "Unknown",
                                     process_knowledge = "partly", adjacent_innovation = "partly"), top = 0.9),
                    list(attr = list(maturity = "Unknown")),
                    list(attr = list(standardization = "Unknown", differentiation = "moderate", commoditization = "no", substitutability = "hard",
                                     qualification_time = "y1to3", process_knowledge = "partly", adjacent_innovation = "partly"))))
  x <- sp_large_yard_case(r$tbl, r$attrs, sv_cfg())
  expect_true(is.na(x$large_yard_case[1]))                # 2 hold, 3 unknown: could reach 5
  expect_match(x$large_yard_reason[1], "undecided")
  expect_match(x$large_yard_unknown[1], "standardized;.*low_differentiation;commoditized")
  expect_true(is.na(x$large_yard_case[2]))                # maturity unknown
  expect_match(x$large_yard_reason[2], "mature not assessed")
  expect_false(x$large_yard_case[3])                      # 1 holds + 1 unknown < 5
  expect_true(is.na(sp_large_yard_case(transform(r$tbl, supply_chain = "Deployment"), transform(r$attrs, stage = "Deployment"),
                                       sv_cfg())$large_yard_case[1]))
})

test_that("the configuration: required signals, min_signals and the switch-off", {
  r <- sv_rows(list(list(attr = list(qualification_time = "m3to12", adjacent_innovation = "partly", commoditization = "partly",
                                     differentiation = "moderate"))))
  cfg6 <- sv_cfg()
  cfg6$large_yard$min_signals <- 6L
  expect_false(sp_large_yard_case(r$tbl, r$attrs, cfg6)$large_yard_case)
  bad <- sv_cfg()
  bad$large_yard$required <- c("mature", "cheap")
  expect_error(sp_large_yard_case(r$tbl, r$attrs, bad), "unknown required")
  bad$large_yard$required <- c("mature", "passive")
  bad$large_yard$min_signals <- 9L
  expect_error(sp_large_yard_case(r$tbl, r$attrs, bad), "min_signals")
  bad <- sv_cfg()
  bad$strategic_value$prize_measure <- "size"
  expect_error(sp_siv_cfg(bad), "prize_measure")
})

# ---- In the choice rule -------------------------------------------------------------------------------------------
sv_cell <- function(...) {
  base <- list(Country = "Testland", iso3 = "TST", tech = "Solar", supply_chain = "Assembly",
               strategic_posture = "deploy", posture_variant = "deploy", posture_possible = "deploy", pivotal_inputs = NA_character_,
               posture_confidence = "high", posture_exclusion = NA_character_, security_rationale = NA_character_,
               security_routes_met = NA_character_, delivery_mode = "diversified_global",
               supply_chain_need = 0.9, industrial_opportunity = 0.95, strategic_industrial_value = 0.95, siv_cap = NA_character_,
               siv_market = 0.95, siv_commons = 0.8, siv_option = 0, deployment_need = NA_real_,
               cp_contender_pct = 0.9, cp_contender_basis = "contenders", cp_is_contender = TRUE, t3_capability_lo = "strong",
               pathway_credible = "yes", pathway_basis = "feasibility;pipeline", home_share = 0.2, trusted_world_share = 0.6,
               coercive_world_share = 0.3, coercive_top_share = 0.5, import_hhi = 0.3, imports_usd = 1e9,
               stage_evidence_quality_cp = "high", supply_chain_need_n_families = 4L, io_maturity_class = "mature",
               large_yard_case = TRUE, large_yard_n = 7L, large_yard_signals = "mature;passive;standardized",
               large_yard_reason = "Large yard: mature, passive, and 7 of 8 commodity signals (standardized), with no future option to keep.")
  a <- list(...)
  base[names(a)] <- a
  as.data.frame(base, stringsAsFactors = FALSE)
}

test_that("a large-yard case is Watch before the prize and the triage are read, after a high position; Secure comes first", {
  lo <- function(...) sv_cell(cp_contender_pct = 0.3, ...)
  d <- sp_decision_choice(lo(), sv_cfg(), sv_post())
  expect_equal(d$strategic_choice, "watch")
  expect_equal(d$strategic_posture, "deploy")
  expect_equal(d$decision_step, "large_yard")
  expect_match(d$watch_reasons, "^large_yard_case")
  # the triage reasons were not read by the rule, so they are not given
  expect_false(grepl("low_need|low_prize|weak_domestic_case", d$watch_reasons))
  expect_equal(d$posture_variant, "deploy")               # the engine read Deploy: its variant
  # where the engine read Compete, concentrated supply gives the concentration watch: keep it open and diversify
  expect_equal(sp_decision_choice(lo(strategic_posture = "compete", posture_variant = "compete_preserve"), sv_cfg(),
                                  sv_post())$posture_variant, "deploy_diversify")
  expect_false(d$greenfield_option)
  expect_true(is.na(d$decision_borderline))
  expect_equal(d$posture_confidence, "high")
  tr <- jsonlite::fromJSON(d$decision_trace, simplifyVector = FALSE)
  expect_equal(vapply(tr, `[[`, "", "step"), c("secure", "large_yard", "choice"))
  expect_equal(tr[[2]]$value, 7L)
  expect_match(sp_choice_reason(d, sv_cfg()), "Large yard: .*Watch: keep it open")
  # Secure first
  expect_equal(sp_decision_choice(lo(strategic_posture = "secure", posture_variant = "secure_input"), sv_cfg(), sv_post())$strategic_choice,
               "secure")
  # the same node without the case: Catch Up on its prize and pathway
  expect_equal(sp_decision_choice(lo(large_yard_case = FALSE), sv_cfg(), sv_post())$strategic_choice, "catch_up")
  # Deployment & Use is unchanged: decided on deployment need whatever the large-yard case
  expect_equal(sp_decision_choice(sv_cell(supply_chain = "Deployment", deployment_need = 0.9), sv_cfg(), sv_post())$strategic_choice,
               "accelerate")
  # position_first (the default, the paper's gate order): a high Competitive Position goes on to the triage
  expect_true(sp_ly_cfg(sv_cfg())$position_first)
  expect_equal(sp_decision_choice(sv_cell(), sv_cfg(), sv_post())$strategic_choice, "preserve")
  # position last: the case first, whatever the position
  pl <- sv_cfg()
  pl$large_yard$position_first <- FALSE
  expect_equal(sp_decision_choice(sv_cell(), pl, sv_post())$strategic_choice, "watch")
  # switched off (enabled false) the case is not read
  off <- sv_cfg()
  off$large_yard$enabled <- FALSE
  expect_equal(sp_decision_choice(lo(), off, sv_post())$strategic_choice, "catch_up")
  # the rule's own argument
  th <- sp_choice_thresholds(sv_cfg())
  expect_equal(sp_choice_rule("Assembly", FALSE, 0.9, 0.9, FALSE, "yes", NA, th, large = TRUE), "watch")
  expect_equal(sp_choice_rule("Assembly", FALSE, 0.9, 0.9, FALSE, "yes", NA, th, large = NA), "catch_up")
  expect_equal(sp_choice_rule("Assembly", FALSE, 0.9, 0.9, TRUE, "yes", NA, th, large = TRUE), "preserve")
})

test_that("a cell without need or prize is not assessed, never a large-yard Watch", {
  na <- sp_decision_choice(sv_cell(cp_contender_pct = 0.3, supply_chain_need = NA_real_, industrial_opportunity = NA_real_,
                                   strategic_industrial_value = NA_real_, supply_chain_need_n_families = NA_integer_, large_yard_n = 6L),
                           sv_cfg(), sv_post())
  expect_equal(na$strategic_choice, "not_assessed")
  expect_equal(na$decision_step, "not_assessed")
  expect_equal(na$posture_confidence, "low")
  expect_true(is.na(sp_yard_of_choice(na$strategic_choice, na$supply_chain)))
  th <- sp_choice_thresholds(sv_cfg())
  expect_equal(sp_choice_rule("Assembly", FALSE, NA, 0.9, FALSE, "no", NA, th, large = TRUE), "not_assessed")
  expect_equal(sp_choice_rule("Assembly", FALSE, 0.9, NA, FALSE, "no", NA, th, large = TRUE), "not_assessed")
  # choice_possible and pivotal_inputs agree with the rule for a not-assessed cell with an undecided case
  un <- sp_decision_choice(sv_cell(cp_contender_pct = 0.3, supply_chain_need = NA_real_, large_yard_case = NA), sv_cfg(), sv_post())
  expect_equal(un$strategic_choice, "not_assessed")
  expect_equal(un$choice_possible, "not_assessed")
  expect_true(is.na(un$pivotal_inputs))
})

test_that("a market-only cap holds at every prize threshold of the sensitivity", {
  cap <- sv_cell(cp_contender_pct = 0.3, large_yard_case = FALSE, large_yard_n = 1L, supply_chain_need = 0.2, industrial_opportunity = 0.95,
                 strategic_industrial_value = 0.59, siv_cap = "market_only", pathway_credible = "yes")
  d <- sp_decision_choice(cap, sv_cfg(), sv_post())
  expect_equal(d$strategic_choice, "watch")
  expect_false(d$prize_high)
  s <- sp_choice_threshold_sensitivity(d, sv_cfg(), list(prize = c(0.5, 0.55)))
  expect_equal(s$changed_vs_base, c(0L, 0L, 0L))
  expect_equal(s$catch_up, c(0L, 0L, 0L))
  expect_equal(sp_choice_rerun(d, sv_cfg()), "watch")
  # the market measure has no cap
  m_cfg <- sv_cfg()
  m_cfg$strategic_value$prize_measure <- "market"
  expect_equal(sp_choice_rerun(d, m_cfg), "catch_up")
  expect_false(sp_choice_inputs(d, m_cfg)$capped)
})

test_that("a narrow large-yard case is borderline both ways; an undecided one that matters lowers confidence and opens Watch", {
  th <- sp_choice_thresholds(sv_cfg())
  nb <- sp_decision_choice(sv_cell(cp_contender_pct = 0.3, large_yard_n = th$large_min), sv_cfg(), sv_post())
  expect_equal(nb$strategic_choice, "watch")
  expect_equal(nb$decision_borderline, "large_yard_case")
  expect_equal(nb$choice_possible, "catch_up|watch")
  expect_match(nb$pivotal_inputs, "large_yard_case")
  expect_equal(nb$posture_confidence, "moderate")
  # one signal short: Partner on need, Watch possible
  nm <- sp_decision_choice(sv_cell(cp_contender_pct = 0.3, large_yard_case = FALSE, large_yard_n = th$large_min - 1L, large_yard_near = TRUE,
                                   strategic_industrial_value = 0.3, pathway_credible = "no"), sv_cfg(), sv_post())
  expect_equal(nm$strategic_choice, "partner")
  expect_equal(nm$decision_borderline, "large_yard_case")
  expect_equal(nm$choice_possible, "partner|watch")
  expect_match(nm$pivotal_inputs, "large_yard_case")
  expect_equal(nm$posture_confidence, "moderate")
  un <- sp_decision_choice(sv_cell(cp_contender_pct = 0.3, large_yard_case = NA, large_yard_n = 3L), sv_cfg(), sv_post())
  expect_equal(un$strategic_choice, "catch_up")
  expect_equal(un$choice_possible, "catch_up|watch")
  expect_match(un$pivotal_inputs, "large_yard_case")
  expect_equal(un$posture_confidence, "moderate")
  tr <- jsonlite::fromJSON(un$decision_trace, simplifyVector = FALSE)
  expect_equal(tr[[2]]$result, "unknown")
  # undecided where the answer is Watch anyway: no issue
  wa <- sp_decision_choice(sv_cell(large_yard_case = NA, supply_chain_need = 0.2, strategic_industrial_value = 0.2, cp_contender_pct = 0.2),
                           sv_cfg(), sv_post())
  expect_equal(wa$strategic_choice, "watch")
  expect_equal(wa$posture_confidence, "high")
})

test_that("the prize the rule reads is strategic_industrial_value; the market measure keeps the old rule", {
  hi_siv <- sv_cell(large_yard_case = FALSE, industrial_opportunity = 0.3, strategic_industrial_value = 0.8, cp_contender_pct = 0.3,
                    supply_chain_need = 0.2, pathway_credible = "no")
  d <- sp_decision_choice(hi_siv, sv_cfg(), sv_post())
  expect_true(d$prize_high)
  expect_equal(d$prize_measure, "strategic_value")
  expect_equal(d$choice_prize, 0.8)
  tr <- jsonlite::fromJSON(d$decision_trace, simplifyVector = FALSE)
  expect_equal(tr[[4]]$test, "strategic_industrial_value")
  expect_match(sp_choice_reason(d, sv_cfg()), "a high strategic industrial value \\(0.80; market 0.95, commons 0.80, option 0.00\\)")
  m_cfg <- sv_cfg()
  m_cfg$strategic_value$prize_measure <- "market"
  m <- sp_decision_choice(hi_siv, m_cfg, sv_post())
  expect_false(m$prize_high)
  expect_equal(m$prize_measure, "market")
  # the measure comparison counts both rules from the stored inputs
  cmp <- sp_choice_measure_comparison(rbind(d, sp_decision_choice(sv_cell(cp_contender_pct = 0.3), sv_cfg(), sv_post())), sv_cfg())
  expect_setequal(cmp$counts$rule, c("market", "siv_only", "market_large_yard", "configured"))
  expect_equal(cmp$counts$changed_vs_market[cmp$counts$rule == "configured"], 1)   # the large-yard Watch; the other cell is Watch both ways
})

# ---- paper_placement is never an input --------------------------------------------------------------------------
test_that("paper_placement is never read by the classification functions", {
  fns <- list(sp_strategic_value, sp_large_yard_case, sp_large_yard_signals, sp_node_attrs, sp_siv_families, sp_leap_credible, sp_decision_choice,
              sp_choice_rule, sp_choice_inputs, sp_choice_reason, sp_yard_of_choice, sp_yard_combine, sp_yard_rollup,
              sp_layer_inputs, sp_layer_choice)
  for (f in fns) expect_false(any(grepl("placement", deparse(body(f)))))
  # an attribute table that carries the paper's placement gives the same answer whatever the placement says
  r <- sv_rows(list(list(market = 0.9), list(market = 0.4, attr = sv_rich), list(market = 0.7, attr = sv_leap)))
  a1 <- cbind(r$attrs, paper_placement = c("small", "small", "small"))
  a2 <- cbind(r$attrs, paper_placement = c("large", "medium", "large"))
  run <- function(a) sp_large_yard_case(sp_strategic_value(r$tbl, a, sv_cfg()), a, sv_cfg())
  expect_identical(run(a1), run(a2))
  expect_identical(run(a1), run(r$attrs))
})

# ---- The curated reference attributes (not synthetic) ---------------------------------------------------------
# Pins the tool's own outcome on data/reference/posture_node_attributes.csv for the layers linked to the 21
# curated Yards & Fences components, at a market prize of 0.9 and two world-supply readings (the largest supplier
# at 30%: diversified; at 80%: concentrated), so an attribute edit or a calibration change that flips one shows
# here. These are the tool's results on the curated evidence, not the paper's placements (never read here).
test_that("the curated attributes give the pinned commons, option and large-yard case for the Y&F-linked layers", {
  source(file.path(sv_repo_root(), "R/strategic_position/posture_nodes.R"))
  rd <- function(f) utils::read.csv(file.path(sv_repo_root(), "data", "reference", f), colClasses = "character", check.names = FALSE,
                                    na.strings = character())
  yf <- rd("yards_fences_assessments.csv")
  nodes <- pn_resolve(pn_read_check(rd("posture_node_attributes.csv"), yf), yf, iso3 = "USA")
  u <- nodes[!is.na(nodes$yf_component_id) & nzchar(nodes$yf_component_id) & nodes$layer_id != "stage", ]
  run <- function(top) {
    tbl <- data.frame(iso3 = "USA", tech = u$tech, supply_chain = u$stage, layer_id = u$layer_id, industrial_opportunity = 0.9,
                      world_top_share = top, coercive_top_share = NA_real_, stringsAsFactors = FALSE)
    x <- sp_large_yard_case(sp_strategic_value(tbl, nodes, sv_cfg()), nodes, sv_cfg())
    x$key <- paste(x$tech, x$layer_id, sep = "|")
    x[order(x$key), ]
  }
  d <- run(0.3)
  k <- run(0.8)
  pin <- data.frame(
    key = c("Batteries|bess-anode", "Batteries|bess-cathode", "Batteries|bess-cells", "Batteries|bess-comms", "Batteries|bess-control",
            "Batteries|bess-enclosure", "Electric Grid|bess-pcs", "Electric Grid|grid-cables", "Electric Grid|grid-inverter-firmware",
            "Electric Grid|grid-scada", "Electric Grid|grid-transformers", "Electric Vehicles|ev-motors", "Electric Vehicles|ree-magnets",
            "Nuclear|nuc-haleu", "Nuclear|nuc-leu", "Nuclear|nuc-reactor-controls", "Solar|pv-cells-wafers", "Solar|pv-modules",
            "Solar|pv-nextgen", "Solar|pv-racking", "Wind|ree-magnets", "Wind|ree-separation"),
    commons = c(0.5, 1, 1, 1 / 6, 1 / 3, 0, 5 / 6, 1 / 6, 1 / 3, 1 / 3, 2 / 3, 1, 5 / 6, 1, 5 / 6, 0.5, 2 / 3, 0.5, 5 / 6, 0, 5 / 6, 5 / 6),
    option = c(0, 0, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0, 0.5, 0, 0, 0),
    case_div = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                 TRUE, FALSE, TRUE, FALSE, FALSE),
    n_div = c(1L, 1L, 3L, 2L, 2L, 7L, 3L, 7L, 2L, 2L, 1L, 3L, 1L, 2L, 4L, 2L, 4L, 6L, 2L, 8L, 1L, 3L),
    n_conc = c(0L, 0L, 2L, 1L, 1L, 6L, 2L, 6L, 1L, 1L, 0L, 2L, 0L, 1L, 3L, 1L, 3L, 5L, 1L, 7L, 0L, 2L),
    stringsAsFactors = FALSE)
  pin <- pin[order(pin$key), ]
  expect_equal(d$key, pin$key)
  expect_equal(d$siv_commons, pin$commons, tolerance = 1e-9)
  expect_equal(d$siv_option, pin$option)
  expect_equal(d$large_yard_case, pin$case_div)
  expect_equal(d$large_yard_n, pin$n_div)
  expect_equal(k$large_yard_case, pin$case_div)            # the same cases when world supply reads concentrated...
  expect_equal(k$large_yard_n, pin$n_conc)                 # ...with one signal fewer
  # pv-modules: a large yard with a capability floor (moderate learning, 1.5 of 3), on exactly min_signals when its
  # own world supply is concentrated (borderline: one signal less, or min_signals 6, and it is not)
  pm <- k[k$key == "Solar|pv-modules", ]
  expect_true(pm$large_yard_floor)
  expect_equal(pm$large_yard_n, sp_ly_cfg(sv_cfg())$min_signals)
  # battery cells and next-generation PV are kept out of the large yard by strong industrial learning, not by size
  expect_match(d$large_yard_reason[d$key %in% c("Batteries|bess-cells", "Solar|pv-nextgen")], "strong industrial learning")
})
