# Guards the Secure / Compete / Deploy decision engine (R/strategic_position/posture_engine.R), the
# Yards & Fences decision layer of the strategic position framework (docs/strategic_position.md,
# section 7).

posture_repo_root <- function() {
  normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
}

source(file.path(posture_repo_root(), "R", "utils", "supply_chain_stages.R"))
source(file.path(posture_repo_root(), "R", "strategic_position", "sp_helpers.R"))
source(file.path(posture_repo_root(), "R", "strategic_position", "posture_engine.R"))

posture_cfg <- function() {
  yaml::read_yaml(file.path(posture_repo_root(), "config", "strategic_position.yml"))$strategic_posture
}
alignment_cfg <- function() {
  yaml::read_yaml(file.path(posture_repo_root(), "config", "strategic_position.yml"))$policy_alignment
}

# One node with neutral defaults; override any field.
posture_node <- function(...) {
  base <- list(
    Country = "Testland", iso3 = "TST", tech = "Batteries", supply_chain = "Components",
    layer_id = "stage", layer_type = "stage", critical_function = "test function",
    consequence = "moderate", time_horizon = "years", digitally_active = "no", remote_control = "no",
    separability = NA, substitutability = "moderate", qualification_time = "m3to12",
    reconstitution_time = "y1to3", stockpile = NA, process_knowledge = "no", workforce_suppliers = "no",
    adjacent_innovation = "no", complementary = "no", beyond_novelty = NA, cost_perf_advantage = NA,
    incumbent_vulnerable = NA, scale_pathway = NA, maturity = "mature", leap_candidate = "no",
    deployment_critical = "no", market_scope = "global",
    coercive_top_share = 0.1, coercive_import_share = 0.1, trusted_world_share = 0.6,
    home_share = 0.01, market_pct = 0.5, premium_pct = NA, exposed = TRUE, adjacent_established = FALSE,
    coercive_top_iso3 = "XXX", coercive_world_share = 0.2, import_hhi = 0.1
  )
  args <- list(...)
  base[names(args)] <- args
  as.data.frame(base, stringsAsFactors = FALSE)
}
classify <- function(...) sp_strategic_posture(posture_node(...), posture_cfg())

# An acute physical chokepoint: severe consequence, coercive supplier above half of world supply, the
# country importing from it, hard substitution, slow restoration, no trusted surge.
chokepoint <- list(
  consequence = "severe", time_horizon = "months", substitutability = "impossible",
  qualification_time = "y1to3", reconstitution_time = "gt3y", stockpile = "none",
  coercive_top_share = 0.85, coercive_import_share = 0.7, trusted_world_share = 0.1
)

test_that("an acute physical chokepoint is Secure even with no domestic position", {
  out <- do.call(classify, c(chokepoint, list(home_share = 0)))
  expect_equal(out$strategic_posture, "secure")
  expect_equal(out$posture_variant, "secure_input")
  expect_equal(out$t2_path, "physical")
  expect_equal(out$posture_primary_rationale, "security")
  expect_equal(out$posture_confidence, "high")
})

test_that("high concentration alone does not make a node Secure", {
  # 90% from a coercive supplier, but substitution is easy and the consequence moderate.
  out <- classify(coercive_top_share = 0.9, coercive_import_share = 0.9, substitutability = "easy")
  expect_equal(out$strategic_posture, "deploy")
  expect_equal(out$posture_variant, "deploy_diversify")
  expect_equal(out$posture_secondary_rationale, "diversification")
  # Severe consequence and concentration, but substitution easy: still not Secure.
  out2 <- classify(consequence = "severe", coercive_top_share = 0.9, coercive_import_share = 0.9, substitutability = "easy")
  expect_equal(out2$strategic_posture, "deploy")
  expect_equal(out2$t2_vulnerability, "no")
})

test_that("Compete needs a capability case and a credible strategy", {
  pres <- classify(home_share = 0.2, process_knowledge = "yes")
  expect_equal(pres$strategic_posture, "compete")
  expect_equal(pres$compete_strategy, "preserve")
  expect_equal(pres$posture_variant, "compete_preserve")
  # A node that carries strong learning in general, but the country has no base: Deploy.
  nobase <- classify(home_share = 0.005, process_knowledge = "yes", workforce_suppliers = "yes", adjacent_innovation = "yes")
  expect_equal(nobase$strategic_posture, "deploy")
  expect_equal(nobase$t3_learning, "weak")
  # Learning on a base of complementary capabilities, a large domestic market: catch up.
  catch <- classify(home_share = 0.01, complementary = "yes", process_knowledge = "yes", workforce_suppliers = "yes", market_pct = 0.9)
  expect_equal(catch$strategic_posture, "compete")
  expect_equal(catch$compete_strategy, "catch_up")
  # Same, but a small domestic market and no established adjacent stage: no credible path.
  nomarket <- classify(home_share = 0.01, complementary = "yes", process_knowledge = "yes", workforce_suppliers = "yes", market_pct = 0.2)
  expect_equal(nomarket$strategic_posture, "deploy")
  expect_equal(nomarket$t4_strategy, "no")
  # An established neighbouring stage in the same technology: adjacent capability.
  adj <- classify(home_share = 0.005, complementary = "partly", process_knowledge = "yes", adjacent_established = TRUE)
  expect_equal(adj$compete_strategy, "adjacent_capability")
})

test_that("a moderate capability case fails proportionality when allies can supply a deployment-critical good", {
  # A 12% share is a single rationale (position); trusted suppliers hold 70% of world supply.
  out <- classify(home_share = 0.12, deployment_critical = "yes", trusted_world_share = 0.7)
  expect_equal(out$t3_capability, "moderate")
  expect_equal(out$t5_proportionality, "fail")
  expect_equal(out$strategic_posture, "deploy")
  expect_equal(out$posture_variant, "deploy_floor")
  # A measured high premium does the same; a low one passes.
  expect_equal(classify(home_share = 0.12, premium_pct = 45)$strategic_posture, "deploy")
  expect_equal(classify(home_share = 0.12, premium_pct = 5)$strategic_posture, "compete")
  # A strong case stays Compete, flagged costly.
  strong <- classify(home_share = 0.2, process_knowledge = "yes", workforce_suppliers = "yes", premium_pct = 45)
  expect_equal(strong$strategic_posture, "compete")
  expect_equal(strong$posture_variant, "compete_costly")
})

test_that("Deploy remains possible for a globally important technology", {
  out <- classify(tech = "Solar", supply_chain = "Assembly", consequence = "high", deployment_critical = "yes",
                  coercive_top_share = 0.8, coercive_import_share = 0.6, substitutability = "moderate",
                  home_share = 0.004)
  expect_equal(out$strategic_posture, "deploy")
})

test_that("Deployment nodes are never Compete and judge supply risk at the producing stages", {
  out <- do.call(classify, c(chokepoint, list(supply_chain = "Deployment", home_share = 0.5)))
  expect_equal(out$strategic_posture, "deploy")
  expect_equal(out$t3_capability, "none")
  expect_equal(out$t5_proportionality, "n_a")
  expect_equal(out$t2_physical, "no")
})

test_that("a sensitive control layer can be Secure while its cell is not", {
  cell <- posture_node(consequence = "high", coercive_import_share = 0.6)
  layer <- posture_node(layer_id = "inverter-firmware", layer_type = "control", consequence = "severe",
                        time_horizon = "days", digitally_active = "yes", remote_control = "yes",
                        separability = "separable", coercive_import_share = 0.6)
  out <- sp_strategic_posture(rbind(cell, layer), posture_cfg())
  expect_equal(out$strategic_posture, c("deploy", "secure"))
  expect_equal(out$posture_variant[2], "secure_control")
  expect_equal(out$t2_path[2], "digital")
  # A whole-stage row is never judged on the digital path.
  expect_equal(out$t2_digital[1], "no")
})

test_that("the same node gets different postures in different countries", {
  a <- do.call(posture_node, c(chokepoint, list(Country = "Importer", iso3 = "IMP", home_share = 0)))
  # Same attributes, but this country imports little from coercive suppliers and is a large producer.
  b <- do.call(posture_node, c(chokepoint, list(Country = "Producer", iso3 = "PRD", coercive_import_share = 0.05,
                                                home_share = 0.3, process_knowledge = "yes")))
  out <- sp_strategic_posture(rbind(a, b), posture_cfg())
  expect_equal(out$strategic_posture, c("secure", "compete"))
})

test_that("observed policy priority cannot change the recommended posture", {
  x <- do.call(posture_node, c(chokepoint, list(home_share = 0)))
  x2 <- rbind(x, transform(x, iso3 = "TS2"))
  x2$policy_priority_percentile <- c(0.01, 0.99)
  x2$policy_support_priority <- c(0, 1)
  out <- sp_strategic_posture(x2, posture_cfg())
  expect_identical(out$strategic_posture[1], out$strategic_posture[2])
  expect_identical(out$decision_trace[1], out$decision_trace[2])
  # The engine never reads a policy column.
  expect_false(any(grepl("policy", deparse(body(sp_posture_gates)))))
})

test_that("unknowns are bracketed, never filled; the strict reading puts the burden on intervention", {
  out <- classify(consequence = NA, substitutability = NA, qualification_time = NA, reconstitution_time = NA,
                  coercive_top_share = 0.8, coercive_import_share = 0.6, trusted_world_share = 0.1)
  expect_equal(out$strategic_posture, "deploy")
  expect_equal(out$t1_consequence, "unknown")
  expect_match(out$posture_possible, "secure")
  expect_match(out$pivotal_inputs, "consequence")
  expect_equal(out$posture_confidence, "low")
  expect_lt(out$evidence_completeness, 0.75)
})

test_that("allied delivery is recommended for a Secure node with weak domestic position and trusted supply", {
  out <- do.call(classify, c(chokepoint, list(trusted_world_share = 0.45, home_share = 0.01)))
  # Trusted supply of 45% does not cover the loss on the timing test (needs 50%), so the node is Secure,
  # and allies hold enough of world supply to deliver it.
  expect_equal(out$strategic_posture, "secure")
  expect_equal(out$delivery_mode, "allied")
  # a mineral cell: below the self-supply line on the demand-weighted share, 15% of the driving mineral
  dom <- do.call(classify, c(chokepoint, list(home_share = 0.05, delivery_home_share = 0.15, trusted_world_share = 0.3)))
  expect_equal(dom$strategic_posture, "secure")
  expect_equal(dom$delivery_mode, "domestic")
  # no trusted supply anywhere: mixed (new trusted capacity at home and with partners)
  none <- do.call(classify, c(chokepoint, list(home_share = 0.01, trusted_world_share = 0.04, coercive_world_share = 0.95)))
  expect_equal(none$delivery_mode, "mixed")
})

test_that("a capability case proven at least moderate competes even when learning is unknown", {
  out <- classify(home_share = 0.27, process_knowledge = NA, workforce_suppliers = NA, adjacent_innovation = NA)
  expect_equal(out$t3_capability, "unknown")
  expect_equal(out$strategic_posture, "compete")
  expect_equal(out$compete_strategy, "preserve")
})

test_that("a capability floor needs a credible strategy", {
  # moderate learning on a partial base, no position, no strategy, high premium: plain Deploy, not a floor
  out <- classify(home_share = 0.01, complementary = "partly", process_knowledge = "yes", workforce_suppliers = "partly",
                  premium_pct = 45)
  expect_equal(out$t4_strategy, "no")
  expect_false(out$posture_variant == "deploy_floor")
  floor <- classify(home_share = 0.12, premium_pct = 45)
  expect_equal(floor$posture_variant, "deploy_floor")
})

test_that("allied alternatives in Test 5 exclude the country's own share", {
  # trusted supply 55%, of which the country itself holds 53%: partners hold 2%, so Test 5 passes
  own <- classify(home_share = 0.53, deployment_critical = "yes", trusted_world_share = 0.55)
  expect_equal(own$t5_proportionality, "pass")
  partners <- classify(home_share = 0.12, deployment_critical = "yes", trusted_world_share = 0.7)
  expect_equal(partners$t5_proportionality, "fail")
})

test_that("import legs need material imports, and regional markets material imports against GDP", {
  base <- c(chokepoint, list(home_share = 0))
  expect_equal(do.call(classify, c(base, list(imports_usd = 5e8, imports_to_gdp = 0.01)))$strategic_posture, "secure")
  token <- do.call(classify, c(base, list(imports_usd = 1300, imports_to_gdp = 1e-8)))
  expect_equal(token$t2_physical, "no")
  gas <- c(chokepoint, list(tech = "Gas", supply_chain = "Extraction", market_scope = "regional", home_share = 0,
                            coercive_top_share = 0.1, coercive_import_share = 1))
  expect_equal(do.call(classify, c(gas, list(imports_usd = 2e9, imports_to_gdp = 0.01)))$strategic_posture, "secure")
  expect_equal(do.call(classify, c(gas, list(imports_usd = 2e6, imports_to_gdp = 0.00001)))$strategic_posture, "deploy")
})

test_that("Extraction catch-up needs a resource base", {
  args <- list(supply_chain = "Extraction", home_share = 0.01, complementary = "yes", process_knowledge = "yes",
               workforce_suppliers = "yes", market_pct = 0.9)
  expect_equal(do.call(classify, c(args, list(resource_base = TRUE)))$compete_strategy, "catch_up")
  expect_equal(do.call(classify, c(args, list(resource_base = FALSE)))$strategic_posture, "deploy")
})

test_that("the strategy test is not applicable at Deployment, in the column and in the trace", {
  out <- classify(supply_chain = "Deployment")
  expect_equal(out$t4_strategy, "n_a")
  tr <- jsonlite::fromJSON(out$decision_trace)
  expect_equal(tr$tests$state[tr$tests$id == "t4_strategy"], "n_a")
})

test_that("regional markets use the country's own import share in the coercion leg", {
  gas <- c(chokepoint, list(tech = "Gas", supply_chain = "Extraction", market_scope = "regional",
                            coercive_top_share = 0.12, coercive_import_share = 0.8, home_share = 0))
  expect_equal(do.call(classify, gas)$strategic_posture, "secure")
  gas$market_scope <- "global"
  expect_equal(do.call(classify, gas)$strategic_posture, "deploy")
})

test_that("the decision trace is valid JSON with all five tests and the two route tests", {
  out <- sp_strategic_posture(rbind(
    do.call(posture_node, c(chokepoint, list(iso3 = "AAA"))),
    posture_node(iso3 = "BBB", home_share = 0.2, process_knowledge = "yes"),
    posture_node(iso3 = "CCC", consequence = NA)
  ), posture_cfg())
  for (tr in out$decision_trace) {
    j <- jsonlite::fromJSON(tr)
    expect_setequal(j$tests$id, c("t1_consequence", "t2_vulnerability", "t2_dual_use", "t2_resilience", "t3_capability",
                                  "t4_strategy", "t5_proportionality"))
    expect_true(all(nzchar(j$tests$detail)))
    expect_true(j$posture %in% SP_POSTURES)
  }
})

test_that("no numeric yards, fence or posture index is exposed", {
  out <- classify(home_share = 0.2, process_knowledge = "yes")
  expect_false(any(grepl("index|score", names(out))))
  numeric_cols <- names(out)[vapply(out, is.numeric, logical(1))]
  expect_equal(numeric_cols, "evidence_completeness")
  expect_true(all(out$strategic_posture %in% SP_POSTURES))
  expect_true(all(out$posture_variant %in% SP_POSTURE_VARIANTS))
})

test_that("policy alignment finds under- and over-prioritization and treats Deploy restraint as aligned", {
  tbl <- data.frame(
    strategic_posture = c("secure", "secure", "compete", "deploy", "deploy", "deploy", "compete", "deploy"),
    supply_chain = c("Refining", "Refining", "Components", "Assembly", "Assembly", "Deployment", "Assembly", "Assembly"),
    policy_priority_percentile = c(0.8, 0.2, 0.3, 0.9, 0.1, 0.95, 0.7, NA),
    nipo_priority = c(0.8, 0.2, 0.3, 0.9, 0.1, NA, 0.7, NA),
    pams_priority = c(0.7, 0.3, 0.3, 0.8, 0.1, 0.95, 0.7, NA),
    policy_support_priority = c(0.8, 0.2, 0.3, 0.9, 0.1, NA, 0.1, NA),
    policy_protection_priority = c(0.5, 0.2, 0.3, 0.9, 0.1, NA, 0.9, NA),
    stringsAsFactors = FALSE
  )
  out <- sp_policy_alignment(tbl, alignment_cfg())
  expect_equal(out, c("aligned", "under_prioritized", "under_prioritized", "over_prioritized", "aligned",
                      "aligned", "mixed", "insufficient_data"))
})

test_that("a major world supplier is not exposed through its imports", {
  # A chokepoint by world shares, but the country itself holds 30% of world supply.
  out <- do.call(classify, c(chokepoint, list(home_share = 0.3)))
  expect_false(out$strategic_posture == "secure")
  expect_equal(out$t2_physical, "no")
  layer <- classify(layer_id = "fw", layer_type = "control", consequence = "severe", time_horizon = "days",
                    digitally_active = "yes", remote_control = "yes", separability = "separable",
                    coercive_import_share = 0.8, home_share = 0.25)
  expect_equal(layer$t2_digital, "no")
})

test_that("a foothold counts as present position only where the country is specialised", {
  spec <- classify(home_share = 0.06, complementary = "yes", process_knowledge = "yes", cp_within_country_pct = 0.8)
  expect_equal(spec$t3_position, "yes")
  diversified <- classify(home_share = 0.06, complementary = "yes", process_knowledge = "yes", cp_within_country_pct = 0.2)
  expect_equal(diversified$t3_position, "no")
  # an established share counts whatever the country's other cells look like
  expect_equal(classify(home_share = 0.2, cp_within_country_pct = 0.1)$t3_position, "yes")
})

test_that("the strict-only path agrees with the full classification", {
  x <- rbind(do.call(posture_node, c(chokepoint, list(iso3 = "AAA"))),
             posture_node(iso3 = "BBB", home_share = 0.2, process_knowledge = "yes"),
             posture_node(iso3 = "CCC", consequence = NA))
  expect_equal(sp_posture_strict(x, posture_cfg()), sp_strategic_posture(x, posture_cfg())$strategic_posture)
})

test_that("a country that does not depend on the node at scale has no acute consequence", {
  # Trivial imports that happen to come from a coercive supplier: the exposure condition of Test 1
  # (material imports, production or deployment at scale) is not met.
  out <- do.call(classify, c(chokepoint, list(home_share = 0, exposed = FALSE)))
  expect_equal(out$t1_consequence, "no")
  expect_equal(out$strategic_posture, "deploy")
  expect_equal(do.call(classify, c(chokepoint, list(home_share = 0, exposed = TRUE)))$strategic_posture, "secure")
})

test_that("heavy policy on an unsettled Deploy is not called over-prioritization", {
  tbl <- data.frame(strategic_posture = c("deploy", "deploy"), supply_chain = "Assembly",
                    posture_possible = c("deploy", "compete|deploy"), policy_priority_percentile = 0.9,
                    nipo_priority = 0.9, pams_priority = 0.9, stringsAsFactors = FALSE)
  expect_equal(sp_policy_alignment(tbl, alignment_cfg()), c("over_prioritized", "insufficient_data"))
})
