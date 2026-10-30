# Guards the three Secure routes (R/strategic_position/security_routes.R and the route tests in
# posture_engine.R; docs/strategic_position.md section 7.2) and the rule table
# config/security_route_rules.csv.

sr_repo_root <- function() normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(dplyr))
for (f in c("R/utils/supply_chain_stages.R", "R/strategic_position/sp_helpers.R", "R/strategic_position/posture_engine.R",
            "R/strategic_position/security_routes.R", "R/strategic_position/posture_nodes.R", "R/strategic_position/policy_design.R")) {
  source(file.path(sr_repo_root(), f))
}
sr_cfg <- function() yaml::read_yaml(file.path(sr_repo_root(), "config", "strategic_position.yml"))$strategic_posture
sr_read <- function(p) utils::read.csv(file.path(sr_repo_root(), p), colClasses = "character", check.names = FALSE, na.strings = character())

sr_node <- function(...) {
  base <- list(
    Country = "Testland", iso3 = "TST", tech = "Electric Grid", supply_chain = "Assembly",
    layer_id = "stage", layer_type = "stage", critical_function = "test function",
    consequence = "high", time_horizon = "months", digitally_active = "no", remote_control = "no",
    separability = NA, substitutability = "moderate", qualification_time = "m3to12",
    reconstitution_time = "y1to3", stockpile = NA, process_knowledge = "no", workforce_suppliers = "no",
    adjacent_innovation = "no", complementary = "no", beyond_novelty = NA, cost_perf_advantage = NA,
    incumbent_vulnerable = NA, scale_pathway = NA, maturity = "mature", leap_candidate = "no",
    deployment_critical = "no", market_scope = "global",
    coercive_top_share = 0.1, coercive_import_share = 0.05, trusted_world_share = 0.3,
    home_share = 0.01, market_pct = 0.5, premium_pct = NA, exposed = TRUE, adjacent_established = FALSE,
    coercive_top_iso3 = "XXX", coercive_world_share = 0.2, import_hhi = 0.1,
    imports_usd = 5e9, imports_to_gdp = 0.002, sn_import_dependence = 0.8,
    sr_rationale = "coercive_chokepoint;systemic_resilience", dual_use = "no", systemic_essentiality = "high"
  )
  args <- list(...)
  base[names(args)] <- args
  as.data.frame(base, stringsAsFactors = FALSE)
}
sr_classify <- function(...) sp_strategic_posture(sr_node(...), sr_cfg())

test_that("the rule table is complete, valid and agrees with the node attributes", {
  yf <- sr_read("data/reference/yards_fences_assessments.csv")
  nodes <- pn_resolve(pn_read_check(sr_read("data/reference/posture_node_attributes.csv"), yf), yf, iso3 = "USA")
  rules <- sr_read_check(sr_read("config/security_route_rules.csv"), nodes)
  expect_equal(names(rules), SR_COLUMNS)
  expect_equal(nrow(rules), nrow(nodes))
  routes <- unlist(strsplit(rules$rationale[nzchar(rules$rationale)], ";"))
  expect_setequal(unique(routes), SP_SECURITY_RATIONALES)
  # every control layer can take the operational route; no Deployment node takes a physical route
  expect_true(all(grepl("operational_control", rules$rationale[nodes$layer_type[match(paste(rules$technology, rules$six_stage, rules$subnode),
                                                                                   paste(nodes$tech, nodes$stage, nodes$layer_id))] %in% "control"])))
  expect_false(any(grepl("coercive_chokepoint|systemic_resilience", rules$rationale[rules$six_stage == "Deployment"])))
  # the resilience route needs a curated essential function
  expect_true(all(rules$systemic_essentiality[grepl("systemic_resilience", rules$rationale)] == "high"))
})

test_that("the rule table check refuses an unknown route, a duplicate node and a resilience route without essentiality", {
  rules <- sr_read("config/security_route_rules.csv")
  bad <- rules
  bad$rationale[1] <- "reshoring"
  expect_error(sr_read_check(bad), "unknown route")
  expect_error(sr_read_check(rbind(rules, rules[1, ])), "duplicate node")
  bad2 <- rules
  i <- which(bad2$systemic_essentiality != "high" & bad2$six_stage != "Deployment")[1]
  bad2$rationale[i] <- "systemic_resilience"
  expect_error(sr_read_check(bad2), "systemic_essentiality high")
})

test_that("systemic resilience makes an essential, import-dependent, slow-to-rebuild node Secure without a coercive supplier", {
  out <- sr_classify()
  expect_equal(out$strategic_posture, "secure")
  expect_equal(out$security_rationale, "systemic_resilience")
  expect_equal(out$posture_variant, "secure_resilience")
  expect_equal(out$t2_resilience, "yes")
  expect_equal(out$t2_coercion, "no")
  expect_equal(out$t2_path, "resilience")
  tr <- jsonlite::fromJSON(out$decision_trace)
  expect_true("t2_resilience" %in% tr$tests$id)
})

test_that("each condition of the resilience route is necessary", {
  expect_equal(sr_classify(systemic_essentiality = "moderate")$strategic_posture, "deploy")
  expect_equal(sr_classify(reconstitution_time = "m6to12")$strategic_posture, "deploy")
  expect_equal(sr_classify(trusted_world_share = 0.7)$strategic_posture, "deploy")     # trusted partners can replace it
  expect_equal(sr_classify(home_share = 0.2, trusted_world_share = 0.3)$t2_resilience, "no")  # self-supplied
  expect_equal(sr_classify(sn_import_dependence = 0.2)$strategic_posture, "deploy")     # not dependent
  expect_equal(sr_classify(consequence = "moderate")$strategic_posture, "deploy")       # no serious consequence
  expect_equal(sr_classify(sr_rationale = "coercive_chokepoint")$strategic_posture, "deploy")  # route not allowed
})

test_that("an unknown input keeps the resilience route open without deciding it", {
  out <- sr_classify(sn_import_dependence = NA)
  expect_equal(out$strategic_posture, "deploy")
  expect_match(out$posture_possible, "secure")
  expect_match(out$pivotal_inputs, "sn_import_dependence")
})

test_that("concentration alone still does not make a coercive chokepoint", {
  out <- sr_classify(coercive_top_share = 0.9, coercive_import_share = 0.9, substitutability = "easy", systemic_essentiality = "moderate",
                     sr_rationale = "coercive_chokepoint")
  expect_equal(out$t2_physical, "no")
  expect_equal(out$strategic_posture, "deploy")
  hard <- sr_classify(coercive_top_share = 0.9, coercive_import_share = 0.9, substitutability = "impossible", reconstitution_time = "gt3y",
                      qualification_time = "y1to3", time_horizon = "weeks", stockpile = "none", trusted_world_share = 0.05,
                      systemic_essentiality = "moderate", sr_rationale = "coercive_chokepoint")
  expect_equal(hard$strategic_posture, "secure")
  expect_equal(hard$security_rationale, "coercive_chokepoint")
  expect_equal(hard$posture_variant, "secure_input")
  # a rule that does not allow the chokepoint route keeps it out
  off <- sr_classify(coercive_top_share = 0.9, coercive_import_share = 0.9, substitutability = "impossible", reconstitution_time = "gt3y",
                     qualification_time = "y1to3", time_horizon = "weeks", stockpile = "none", trusted_world_share = 0.05,
                     systemic_essentiality = "moderate", sr_rationale = "")
  expect_equal(off$strategic_posture, "deploy")
})

test_that("a dual-use node is Secure on the operational route when coercive suppliers carry a quarter of its imports", {
  out <- sr_classify(dual_use = "yes", sr_rationale = "operational_control;coercive_chokepoint", systemic_essentiality = "moderate",
                     coercive_import_share = 0.4, coercive_top_share = 0.3)
  expect_equal(out$strategic_posture, "secure")
  expect_equal(out$security_rationale, "operational_control")
  expect_equal(out$posture_variant, "secure_dual_use")
  low <- sr_classify(dual_use = "yes", sr_rationale = "operational_control", systemic_essentiality = "moderate", coercive_import_share = 0.1)
  expect_equal(low$strategic_posture, "deploy")
  self <- sr_classify(dual_use = "yes", sr_rationale = "operational_control", systemic_essentiality = "moderate", coercive_import_share = 0.4,
                      home_share = 0.3)
  expect_equal(self$t2_dual_use, "no")
})

test_that("a control layer reaches Secure through the operational route, not from trade concentration", {
  out <- sr_classify(layer_id = "ctl", layer_type = "control", digitally_active = "yes", remote_control = "yes", time_horizon = "days",
                     separability = "separable", coercive_import_share = 0.5, sr_rationale = "operational_control", systemic_essentiality = "moderate")
  expect_equal(out$strategic_posture, "secure")
  expect_equal(out$security_rationale, "operational_control")
  expect_equal(out$posture_variant, "secure_control")
  nocmd <- sr_classify(layer_id = "ctl", layer_type = "control", digitally_active = "yes", remote_control = "no", time_horizon = "days",
                       coercive_import_share = 0.9, sr_rationale = "operational_control", systemic_essentiality = "moderate")
  expect_equal(nocmd$strategic_posture, "deploy")
})

test_that("a node without a rule keeps the original two routes and no resilience route", {
  legacy <- sr_classify(sr_rationale = NA, systemic_essentiality = NA, dual_use = NA)
  expect_equal(legacy$strategic_posture, "deploy")
  expect_equal(legacy$t2_resilience, "n_a")
})

test_that("there is no quota: every node that meets a route is Secure", {
  x <- do.call(rbind, lapply(1:12, function(i) sr_node(iso3 = sprintf("T%02d", i), Country = paste("Testland", i))))
  out <- sp_strategic_posture(x, sr_cfg())
  expect_true(all(out$strategic_posture == "secure"))
})

test_that("Secure does not mean reshoring: a resilience node with little home supply is delivered with partners", {
  out <- sr_classify(home_share = 0.0, trusted_world_share = 0.45)
  expect_equal(out$strategic_posture, "secure")
  expect_false(out$delivery_mode == "domestic")
  d <- ppd_read_check(sr_read("config/posture_policy_design.csv"), sr_read("config/posture_delivery_modes.csv"))$design
  lvl <- setNames(d$fence_level, d$posture_variant)
  # the fence height follows the yard (high for every Secure route, user decision 2026-10-30); what it guards
  # differs by rationale: the essential supply itself for resilience (never import protection), the
  # national-security end uses for dual use, the coercive supplier's leverage over the input for a chokepoint
  expect_equal(unname(lvl[c("secure_resilience", "secure_dual_use", "secure_input")]), c("high", "high", "high"))
  scope <- setNames(d$fence_scope, d$posture_variant)
  expect_equal(unname(scope[c("secure_resilience", "secure_dual_use", "secure_input")]), c("essential_supply", "critical_end_use", "input"))
  expect_equal(d$needs_protect[d$posture_variant == "secure_resilience"], "avoid")
  expect_match(d$fence_instruments[d$posture_variant == "secure_resilience"], "Strategic stockholding", fixed = TRUE)
})

test_that("route inputs are checked against their vocabulary", {
  expect_error(sr_classify(systemic_essentiality = "vital"), "systemic_essentiality")
  expect_error(sr_classify(sr_rationale = "reshoring"), "unknown route")
})
