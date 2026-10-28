# Guards the Policy Design layer of the strategic posture (R/strategic_position/policy_design.R,
# config/posture_policy_design.csv, config/posture_delivery_modes.csv) and the decision cards
# (R/strategic_position/decision_cards.R): the Yards & Fences port of needsFor() and
# successMetrics(), vocabularies, delivery notes, missing values, card rendering and case selection.

ppd_repo_root <- function() {
  normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
}

source(file.path(ppd_repo_root(), "R", "strategic_position", "policy_design.R"))
source(file.path(ppd_repo_root(), "R", "strategic_position", "decision_cards.R"))

library(dplyr)

ppd_config <- function() {
  rd <- function(f) utils::read.csv(file.path(ppd_repo_root(), "config", f), colClasses = "character", check.names = FALSE)
  list(design = rd("posture_policy_design.csv"), delivery = rd("posture_delivery_modes.csv"))
}

# needsFor() in dashboard/src/fence/engine/policy.ts, transcribed: (secure, buffer, build, protect,
# open, discipline) for each yard variant, under its six-stage posture_variant name.
TS_NEEDS <- list(
  secure_control = c("required", "helpful", "helpful", "avoid", "neutral", "helpful"),
  secure_integrated = c("required", "helpful", "helpful", "avoid", "neutral", "helpful"),
  secure_whole = c("required", "helpful", "helpful", "avoid", "neutral", "helpful"),
  secure_unclear = c("required", "helpful", "helpful", "avoid", "neutral", "helpful"),
  secure_input = c("helpful", "required", "required", "neutral", "neutral", "helpful"),
  secure_both = c("required", "required", "required", "neutral", "neutral", "helpful"),
  compete_preserve = c("avoid", "neutral", "required", "neutral", "helpful", "required"),
  compete_adjacent_capability = c("avoid", "neutral", "required", "neutral", "helpful", "required"),
  compete_leap_ahead = c("avoid", "neutral", "required", "neutral", "helpful", "required"),
  compete_costly = c("avoid", "neutral", "required", "avoid", "helpful", "required"),
  deploy = c("avoid", "neutral", "neutral", "avoid", "helpful", "neutral"),
  deploy_floor = c("avoid", "neutral", "helpful", "avoid", "helpful", "required"),
  deploy_diversify = c("avoid", "helpful", "neutral", "avoid", "helpful", "neutral")
)

# A synthetic posture table: the engine's output columns for a handful of nodes.
ppd_trace <- function(posture, variant, possible = posture, pivotal = character(), details = NULL) {
  ids <- SP_CARD_TESTS$id
  if (is.null(details)) details <- paste("Detail for", ids)
  tests <- lapply(seq_along(ids), function(i) list(
    id = ids[i], state = "no", marginal = i == 2, detail = details[i],
    inputs = list(list(id = "consequence", value = "high", provenance = "assessment"))
  ))
  as.character(jsonlite::toJSON(list(tests = tests, posture = posture, variant = variant,
                                     possible = I(possible), pivotal = I(pivotal)), auto_unbox = TRUE))
}

ppd_fixture <- function() {
  base <- tibble::tibble(
    Country = c("Japan", "Japan", "China", "Germany", "India", "Chile", "Australia", "Peru"),
    iso3 = c("JPN", "JPN", "CHN", "DEU", "IND", "CHL", "AUS", "PER"),
    tech = c("Batteries", "Electric Grid", "Solar", "Wind", "Solar", "Batteries", "Batteries", "Solar"),
    supply_chain = c("Components", "Components", "Assembly", "Refining", "Components", "Refining", "Refining", "Assembly"),
    layer_id = "stage",
    strategic_posture = c("compete", "secure", "deploy", "secure", "compete", "deploy", "secure", NA),
    posture_variant = c("compete_preserve", "secure_control", "deploy_diversify", "secure_input",
                        "compete_catch_up", "deploy", "secure_input", NA),
    posture_confidence = c("high", "high", "moderate", "high", "moderate", "low", "high", NA),
    evidence_completeness = c(0.82, 0.9, 0.6, 0.8, 0.5, 0.3, 0.85, NA),
    posture_possible = c("compete", "secure", "compete|deploy", "secure", "secure|compete", "deploy", "secure", NA),
    pivotal_inputs = c(NA, NA, "process_knowledge", NA, "substitutability;qualification_time", NA, NA, NA),
    posture_primary_rationale = c("capability", "security", "openness", "security", "capability", "openness", "security", NA),
    posture_secondary_rationale = c(NA, NA, "diversification", "capability", NA, NA, NA, NA),
    compete_strategy = c("preserve", NA, NA, NA, "catch_up", NA, NA, NA),
    delivery_mode = c("domestic", "domestic", "diversified_global", "allied", "domestic", "diversified_global", "allied", NA),
    t1_consequence = c("no", "yes", "no", "yes", "unknown", "no", "yes", NA),
    t2_vulnerability = c("no", "yes", "no", "yes", "unknown", "no", "yes", NA),
    t2_path = c("none", "digital", "none", "physical", "unknown", "none", "physical", NA),
    t3_capability = c("strong", "moderate", "moderate", "none", "moderate", "none", "none", NA),
    t4_strategy = c("preserve", "no", "no", "no", "catch_up", "no", "no", NA),
    t5_proportionality = c("pass", "pass", "pass", "n_a", "pass", "pass", "n_a", NA),
    competitive_position_percentile = c(0.91, 0.7, 0.99, 0.3, 0.6, 0.4, 0.2, NA),
    strategic_need_percentile = c(0.34, 0.8, 0.1, 0.85, 0.5, 0.2, 0.9, NA),
    policy_priority_percentile = c(0.72, 0.2, 0.9, 0.1, 0.3, 0.8, 0.35, NA),
    competitive_policy_alignment = c("aligned_strength", "latent_advantage", "aligned_strength", "low_priority",
                                     "low_priority", "policy_led_bet", "low_priority", NA),
    policy_alignment = c("aligned", "under_prioritized", "over_prioritized", "under_prioritized",
                         "under_prioritized", "over_prioritized", "under_prioritized", NA),
    stage_evidence_quality = c("high", "medium", "high", "high", "low", "low", "medium", NA),
    home_share = c(0.12, 0.05, 0.8, 0.01, 0.04, 0.02, 0.003, NA),
    coercive_top_iso3 = c("CHN", "CHN", NA, "CHN", "CHN", "CHN", "CHN", NA),
    coercive_top_share = c(0.68, 0.55, NA, 0.9, 0.8, 0.45, 0.9, NA),
    trusted_world_share = c(0.3, 0.25, 0.1, 0.4, 0.15, 0.3, 0.45, NA),
    sensitive_layers_secure = c(NA, NA, NA, NA, NA, NA, NA, NA),
    posture_exclusion = c(NA, NA, NA, NA, NA, NA, NA, "entrepot")
  )
  base$decision_trace <- c(
    ppd_trace("compete", "compete_preserve"),
    ppd_trace("secure", "secure_control"),
    ppd_trace("deploy", "deploy_diversify", c("compete", "deploy"), "process_knowledge"),
    ppd_trace("secure", "secure_input"),
    ppd_trace("compete", "compete_catch_up", c("secure", "compete"), c("substitutability", "qualification_time")),
    ppd_trace("deploy", "deploy"),
    ppd_trace("secure", "secure_input"),
    NA
  )
  base
}

ppd_designed <- function() {
  cfg <- ppd_config()
  sp_policy_design(ppd_fixture(), cfg$design, cfg$delivery)
}

# ---- Config ---------------------------------------------------------------------------------------

test_that("the shipped design and delivery tables pass the check, one row per variant and pair", {
  cfg <- ppd_config()
  chk <- ppd_read_check(cfg$design, cfg$delivery)
  expect_setequal(chk$design$posture_variant, PPD_VARIANTS)
  expect_equal(nrow(chk$design), length(PPD_VARIANTS))
  expect_equal(nrow(chk$delivery), length(PPD_DELIVERY_MODES) * length(PPD_POSTURES))
  expect_type(chk$design$review_interval_years, "integer")
  expect_true(all(chk$design$strategic_posture == ppd_variant_posture(chk$design$posture_variant)))
})

test_that("the variant list matches the posture engine's", {
  f <- file.path(ppd_repo_root(), "R", "strategic_position", "posture_engine.R")
  skip_if_not(file.exists(f), "posture engine not present")
  exprs <- tryCatch(parse(f), error = function(e) NULL)
  skip_if(is.null(exprs), "posture engine does not parse yet")
  env <- new.env()
  for (e in exprs) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("SP_POSTURE_VARIANTS"))) eval(e, env)
  }
  skip_if_not(exists("SP_POSTURE_VARIANTS", envir = env, inherits = FALSE), "no SP_POSTURE_VARIANTS in the engine")
  expect_setequal(get("SP_POSTURE_VARIANTS", envir = env), PPD_VARIANTS)
})

test_that("needs follow needsFor() in policy.ts, with open required for catch-up", {
  d <- ppd_config()$design
  for (v in names(TS_NEEDS)) {
    expect_equal(unname(ppd_needs(d, v)), TS_NEEDS[[v]], info = v)
  }
  # Extension: closing the gate is the one thing policy.ts flags specifically for catch-up.
  expect_equal(unname(ppd_needs(d, "compete_catch_up")), c("avoid", "neutral", "required", "neutral", "required", "required"))
})

test_that("success metrics carry successMetrics() from policy.ts", {
  d <- ppd_config()$design
  m <- function(v) ppd_split_list(d$success_metrics[d$posture_variant == v])
  control <- c(
    "Share of the installed base running certified, trusted control software or isolated from remote access",
    "Time to detect and patch a firmware vulnerability across the fleet",
    "Availability of certified trusted replacements at deployment scale",
    "Whether the control layer can be swapped without replacing the hardware (the fence can shrink)"
  )
  input <- c(
    "Months critical production could continue through a cutoff (inventories plus qualified alternative supply)",
    "Qualified trusted capacity as a share of critical demand, and how fast it can surge",
    "Share of supply held by the largest potentially coercive supplier",
    "Liquidity and survival of sub-tier suppliers through a price war or cutoff"
  )
  expect_equal(m("secure_control"), control)
  expect_equal(m("secure_input"), input)
  expect_equal(m("secure_both"), c(control[1:2], input[1:2]))
  for (v in c("compete_preserve", "compete_catch_up", "compete_costly", "compete_leap_ahead", "compete_adjacent_capability")) {
    expect_true("Public support per unit falling as competitiveness improves" %in% m(v), info = v)
  }
  expect_true("Whether the domestic floor is preserving the capability it was specified to preserve" %in% m("deploy_floor"))
  expect_equal(m("deploy")[1], "Delivered cost and pace of deployment")
})

test_that("the design follows the Yards & Fences logic", {
  d <- ppd_config()$design
  sec <- d$strategic_posture == "secure"
  com <- d$strategic_posture == "compete"
  dep <- d$strategic_posture == "deploy"
  # No posture closes the gate: Secure stays open to allied supply, Deploy is wide open.
  expect_false(any(d$gate_level == "closed"))
  expect_true(all(d$gate_level[sec] == "selective"))
  expect_true(all(d$gate_level[dep] == "wide_open"))
  expect_true(all(grepl("allied", d$gate_posture[sec], ignore.case = TRUE)))
  # Every Compete measure carries benchmarks, a review and an exit; capability building is required.
  expect_true(all(d$needs_discipline[com] == "required"))
  expect_true(all(d$needs_build[com] == "required"))
  expect_true(all(grepl("sunset", d$exit_condition[com], ignore.case = TRUE) | grepl("Stop scaling", d$exit_condition[com])))
  # Deploy: no security tools, no protection, a low fence, and never judged by domestic production share.
  expect_true(all(d$needs_secure[dep] == "avoid" & d$needs_protect[dep] == "avoid"))
  expect_true(all(d$fence_level[dep] == "low"))
  expect_true(grepl("Domestic production share is not a benchmark", d$benchmark[d$posture_variant == "deploy"]))
  expect_false(any(grepl("domestic production share|self-sufficiency", d$success_metrics[dep], ignore.case = TRUE)))
  # Secure is mitigation, not localisation: protection is never required.
  expect_false(any(d$needs_protect[sec] == "required"))
  expect_true(grepl("not maximum localisation", d$build_posture[d$posture_variant == "secure_input"]))
  # Exit conditions say what reclassifies the node.
  expect_true(all(grepl("Compete or Deploy", d$exit_condition[sec])))
  expect_true(all(grepl("[Rr]e-test", d$exit_condition[dep])))
})

test_that("the check reports bad vocabulary, missing variants and missing delivery pairs", {
  cfg <- ppd_config()
  d <- cfg$design
  l <- cfg$delivery
  bad <- d; bad$fence_level[1] <- "very_high"
  expect_error(ppd_read_check(bad, l), "fence_level")
  bad <- d; bad$gate_level[2] <- "ajar"
  expect_error(ppd_read_check(bad, l), "gate_level")
  bad <- d; bad$needs_open[3] <- "maybe"
  expect_error(ppd_read_check(bad, l), "needs_open")
  expect_error(ppd_read_check(d[-1, ], l), "missing posture_variant: secure_control")
  expect_error(ppd_read_check(rbind(d, d[1, ]), l), "duplicate posture_variant")
  bad <- d; bad$strategic_posture[1] <- "compete"
  expect_error(ppd_read_check(bad, l), "does not match")
  bad <- d; bad$review_interval_years[1] <- "0"
  expect_error(ppd_read_check(bad, l), "review_interval_years")
  bad <- d; bad$fence_instruments[1] <- "A; ; B"
  expect_error(ppd_read_check(bad, l), "fence_instruments")
  expect_error(ppd_read_check(d, l[-1, ]), "missing delivery_mode x strategic_posture: domestic secure")
  bad <- l; bad$delivery_mode[1] <- "offshore"
  expect_error(ppd_read_check(d, bad), "invalid delivery_mode")
  expect_error(ppd_read_check(d[, setdiff(names(d), "benchmark")], l), "missing column")
})

# ---- sp_policy_design() ---------------------------------------------------------------------------

test_that("every variant gets its design row and the row order is kept", {
  cfg <- ppd_config()
  tbl <- tibble::tibble(Country = "X", posture_variant = rev(PPD_VARIANTS), delivery_mode = NA_character_)
  out <- sp_policy_design(tbl, cfg$design, cfg$delivery)
  expect_equal(out$posture_variant, rev(PPD_VARIANTS))
  ref <- cfg$design[match(rev(PPD_VARIANTS), cfg$design$posture_variant), ]
  for (col in c("fence_posture", "gate_posture", "build_posture", "benchmark", "exit_condition", "fence_level", "gate_level")) {
    expect_equal(out[[col]], ref[[col]], info = col)
  }
  expect_equal(out$review_interval_years, as.integer(ref$review_interval_years))
  expect_true(all(PPD_OUTPUT_COLS %in% names(out)))
  expect_false(any(PPD_DETAIL_COLS %in% names(out)))
})

test_that("delivery notes adjust the build and the gate", {
  cfg <- ppd_config()
  tbl <- tibble::tibble(posture_variant = c("secure_input", "secure_input", "compete_preserve", "deploy_diversify"),
                        delivery_mode = c("allied", "domestic", "mixed", "allied"))
  out <- sp_policy_design(tbl, cfg$design, cfg$delivery)
  note <- function(m, p, col) cfg$delivery[[col]][cfg$delivery$delivery_mode == m & cfg$delivery$strategic_posture == p]
  base <- cfg$design$build_posture[cfg$design$posture_variant == "secure_input"]
  expect_true(startsWith(out$build_posture[1], base))
  expect_true(grepl(note("allied", "secure", "build_note"), out$build_posture[1], fixed = TRUE))
  expect_true(grepl("Delivery (allied)", out$build_posture[1], fixed = TRUE))
  expect_true(grepl("not at home", out$build_posture[1], fixed = TRUE))
  expect_true(grepl(note("allied", "secure", "gate_note"), out$gate_posture[1], fixed = TRUE))
  expect_true(grepl(note("domestic", "secure", "build_note"), out$build_posture[2], fixed = TRUE))
  expect_false(identical(out$build_posture[1], out$build_posture[2]))
  expect_true(grepl(note("mixed", "compete", "build_note"), out$build_posture[3], fixed = TRUE))
  expect_true(grepl(note("allied", "deploy", "gate_note"), out$gate_posture[4], fixed = TRUE))
  # The fence does not depend on delivery.
  expect_equal(out$fence_posture[1], out$fence_posture[2])
})

test_that("a missing variant gives a missing design and a missing delivery mode no note", {
  cfg <- ppd_config()
  tbl <- tibble::tibble(posture_variant = c(NA, "deploy", NA), delivery_mode = c("allied", NA, NA),
                        strategic_posture = c(NA, "deploy", NA))
  out <- sp_policy_design(tbl, cfg$design, cfg$delivery)
  for (col in PPD_OUTPUT_COLS) expect_true(all(is.na(out[[col]][c(1, 3)])), info = col)
  expect_equal(out$build_posture[2], cfg$design$build_posture[cfg$design$posture_variant == "deploy"])
  expect_false(grepl("Delivery (", out$build_posture[2], fixed = TRUE))
  # No delivery_mode column at all is the same as a missing mode.
  out2 <- sp_policy_design(tibble::tibble(posture_variant = "deploy"), cfg$design, cfg$delivery)
  expect_equal(out2$build_posture, out$build_posture[2])
})

test_that("unknown variants or modes and a posture that disagrees with the variant stop", {
  cfg <- ppd_config()
  expect_error(sp_policy_design(tibble::tibble(posture_variant = "medium"), cfg$design, cfg$delivery), "unknown posture_variant")
  expect_error(sp_policy_design(tibble::tibble(posture_variant = "deploy", delivery_mode = "offshore"), cfg$design, cfg$delivery),
               "unknown delivery_mode")
  expect_error(sp_policy_design(tibble::tibble(posture_variant = "deploy", strategic_posture = "secure"), cfg$design, cfg$delivery),
               "disagrees")
  expect_error(sp_policy_design(tibble::tibble(x = 1), cfg$design, cfg$delivery), "posture_variant")
})

test_that("a re-run replaces the design columns and detail adds instruments and needs", {
  cfg <- ppd_config()
  once <- sp_policy_design(ppd_fixture(), cfg$design, cfg$delivery)
  twice <- sp_policy_design(once, cfg$design, cfg$delivery)
  expect_identical(names(once), names(twice))
  expect_identical(once, twice)
  expect_false(any(grepl("\\.(x|y)$", names(twice))))
  det <- sp_policy_design(ppd_fixture(), cfg$design, cfg$delivery, detail = TRUE)
  expect_true(all(PPD_DETAIL_COLS %in% names(det)))
  expect_equal(det$needs_open[det$posture_variant %in% "compete_catch_up"], "required")
  expect_equal(nrow(sp_policy_design(ppd_fixture()[0, ], cfg$design, cfg$delivery)), 0)
})

test_that("no numeric yard or fence index appears in the design output", {
  out <- ppd_designed()
  added <- setdiff(names(out), names(ppd_fixture()))
  expect_setequal(added, PPD_OUTPUT_COLS)
  expect_false(any(grepl("index|score", added)))
  numeric_added <- added[vapply(out[added], is.numeric, logical(1))]
  expect_equal(numeric_added, "review_interval_years")
  expect_true(all(out$fence_level %in% c(PPD_FENCE_LEVELS, NA)))
  expect_true(all(out$gate_level %in% c(PPD_GATE_LEVELS, NA)))
})

# ---- Decision cards -------------------------------------------------------------------------------

test_that("ordinals read as percentiles", {
  expect_equal(vapply(c(0.01, 0.02, 0.03, 0.11, 0.12, 0.13, 0.21, 0.22, 0.91, 0.5), sp_ordinal_pct, character(1)),
               c("1st", "2nd", "3rd", "11th", "12th", "13th", "21st", "22nd", "91st", "50th"))
  expect_equal(sp_ordinal_pct(0.001), "1st")
  expect_equal(sp_ordinal_pct(0.999), "99th")
  expect_true(is.na(sp_ordinal_pct(NA)))
})

test_that("a card renders every section from the row", {
  out <- ppd_designed()
  card <- sp_decision_card(out[1, ], iso3_names = c(CHN = "China"))
  expect_true(startsWith(card, "### Japan - Batteries - Components"))
  expect_match(card, "**Recommended posture: COMPETE** (compete_preserve)", fixed = TRUE)
  expect_match(card, "Confidence: High (82% of relevant inputs known)", fixed = TRUE)
  expect_match(card, "Primary rationale: Capability", fixed = TRUE)
  expect_match(card, "Compete strategy: Preserve", fixed = TRUE)
  expect_match(card, "Delivery: Domestic", fixed = TRUE)
  expect_match(card, "Competitive Position 91st percentile (across countries)", fixed = TRUE)
  expect_match(card, "Strategic Need 34th percentile", fixed = TRUE)
  expect_match(card, "Observed Policy Priority 72nd percentile (within the country)", fixed = TRUE)
  expect_match(card, "Evidence specificity: High", fixed = TRUE)
  expect_match(card, "world share 12%", fixed = TRUE)
  expect_match(card, "China (CHN) with 68% of world supply", fixed = TRUE)
  for (lab in SP_CARD_TESTS$label) expect_match(card, paste0("- ", lab), fixed = TRUE)
  expect_match(card, "Exploitable vulnerability (no path): **No** (marginal). Detail for t2_vulnerability", fixed = TRUE)
  expect_match(card, "Competitive capability: **Strong**. Detail for t3_capability", fixed = TRUE)
  expect_match(card, "Competitive-policy alignment (diagnostic): Aligned strength", fixed = TRUE)
  expect_match(card, "Policy alignment (observed vs recommended): Aligned", fixed = TRUE)
  expect_match(card, paste0("- Fence (moderate, product): ", out$fence_posture[1]), fixed = TRUE)
  expect_match(card, paste0("- Gate (open): ", out$gate_posture[1]), fixed = TRUE)
  expect_match(card, "- Build: ", fixed = TRUE)
  expect_match(card, paste0("**Benchmark:** ", out$benchmark[1]), fixed = TRUE)
  expect_match(card, paste0("**Exit condition:** ", out$exit_condition[1]), fixed = TRUE)
  expect_match(card, "**Review:** every 3 years", fixed = TRUE)
  # Metrics are split on ";" only: their items contain commas.
  metrics <- ppd_split_list(out$success_metrics[1])
  expect_match(card, paste0("\n  - ", metrics[1], "\n"), fixed = TRUE)
  expect_equal(lengths(regmatches(card, gregexpr("\n  - ", card))), length(metrics))
  # Settled, no layers: those sections are left out.
  expect_no_match(card, "What would change", fixed = TRUE)
  expect_no_match(card, "Sensitive layers", fixed = TRUE)
  expect_no_match(card, "NA", fixed = TRUE)
})

test_that("a card omits lines whose data are missing", {
  out <- ppd_designed()
  r <- out[1, ]
  r$policy_priority_percentile <- NA
  r$coercive_top_iso3 <- NA
  r$coercive_top_share <- NA
  r$home_share <- NA
  r$trusted_world_share <- NA
  r$competitive_policy_alignment <- NA
  r$policy_alignment <- NA
  r$benchmark <- NA
  r$posture_confidence <- NA
  card <- sp_decision_card(r)
  expect_no_match(card, "Observed Policy Priority", fixed = TRUE)
  expect_no_match(card, "**Supply:**", fixed = TRUE)
  expect_no_match(card, "**Alignment**", fixed = TRUE)
  expect_no_match(card, "Benchmark", fixed = TRUE)
  expect_no_match(card, "Confidence", fixed = TRUE)
  expect_match(card, "Exit condition", fixed = TRUE)
  expect_no_match(card, "NA", fixed = TRUE)
  # The canonical table's own names are accepted for the diagnostic and the evidence band.
  r2 <- out[1, ]
  names(r2)[names(r2) == "competitive_policy_alignment"] <- "alignment_class"
  names(r2)[names(r2) == "stage_evidence_quality"] <- "stage_evidence_quality_cp"
  card2 <- sp_decision_card(r2)
  expect_match(card2, "Competitive-policy alignment (diagnostic): Aligned strength", fixed = TRUE)
  expect_match(card2, "Evidence specificity: High", fixed = TRUE)
  # Without a usable trace the test states still come from the columns, without the details.
  r$decision_trace <- "{not json"
  card <- sp_decision_card(r)
  expect_match(card, "- Critical consequence: **No**", fixed = TRUE)
  expect_no_match(card, "Detail for", fixed = TRUE)
  # An unclassified node says so.
  card <- sp_decision_card(out[8, ])
  expect_match(card, "**Recommended posture: not classified** (entrepot)", fixed = TRUE)
  expect_no_match(card, "**Policy design**", fixed = TRUE)
})

test_that("an unsettled posture shows its pivotal inputs and possible postures", {
  out <- ppd_designed()
  card <- sp_decision_card(out[5, ])
  expect_match(card, "**What would change the answer:** the unknown inputs allow Secure or Compete", fixed = TRUE)
  expect_match(card, "Pivotal inputs (each alone would decide it): substitutability, qualification time.", fixed = TRUE)
  expect_match(card, "Under-prioritized", fixed = TRUE)
  expect_match(card, "not proof that the node has no priority", fixed = TRUE)
  # posture_possible and pivotal_inputs fall back to the trace when the columns are missing.
  r <- out[3, ]
  r$posture_possible <- NA
  r$pivotal_inputs <- NA
  card <- sp_decision_card(r)
  expect_match(card, "allow Compete or Deploy", fixed = TRUE)
  expect_match(card, "process knowledge", fixed = TRUE)
})

test_that("sensitive layers are listed with their postures", {
  out <- ppd_designed()
  r <- out[2, ]
  r$sensitive_layers_secure <- "inverter_firmware;scada"
  card <- sp_decision_card(r)
  expect_match(card, "**Sensitive layers in this cell**", fixed = TRUE)
  expect_match(card, "- inverter_firmware: Secure", fixed = TRUE)
  expect_match(card, "never changes the cell's posture", fixed = TRUE)
  layers <- tibble::tibble(
    Country = c("Japan", "Japan", "China"), iso3 = c("JPN", "JPN", "CHN"),
    tech = "Electric Grid", supply_chain = "Components",
    layer_id = c("inverter_firmware", "transformer_core", "inverter_firmware"),
    layer_label = c("Inverter firmware", "Transformer cores", "Inverter firmware"),
    layer_type = c("control", "hardware", "control"),
    strategic_posture = c("secure", "deploy", "deploy"), posture_variant = c("secure_control", "deploy", "deploy")
  )
  card <- sp_decision_card(out[2, ], layers = layers)
  expect_match(card, "- Inverter firmware (control): Secure (secure_control)", fixed = TRUE)
  expect_match(card, "- Transformer cores (hardware): Deploy (deploy)", fixed = TRUE)
  expect_equal(lengths(regmatches(card, gregexpr("Inverter firmware", card))), 1)
})

# ---- Case selection and the document --------------------------------------------------------------

test_that("each case is picked when present, preferring review countries and distinct nodes", {
  out <- ppd_designed()
  cases <- sp_card_cases(out, countries = c("Japan", "China", "Germany", "Australia"))
  expect_equal(cases$case, SP_CARD_CASES$case)
  expect_true(all(cases$case_found))
  pick <- function(k) cases[cases$case == k, ]
  # Three review-country Secure nodes with high confidence: the one with high evidence specificity wins.
  expect_equal(pick("under_prioritized_secure")$Country, "Germany")
  expect_equal(pick("under_supported_compete")$Country, "India")       # the only one, outside the review list
  expect_equal(pick("over_prioritized_deploy")$Country, "China")       # a review country beats Chile
  expect_equal(pick("allied_high_need_low_position")$Country, "Australia")  # Germany's node is already used
  expect_equal(pick("secure")$Country, "Japan")                       # the other Secure nodes are used
  expect_equal(pick("compete")$Country, "Japan")
  expect_equal(pick("deploy")$Country, "Chile")
  expect_true(all(pick("allied_high_need_low_position")$delivery_mode == "allied"))
  # Rows are distinct where the table allows.
  keys <- paste(cases$Country, cases$tech, cases$supply_chain)
  expect_equal(anyDuplicated(keys), 0)
})

test_that("absent cases are reported, not dropped", {
  out <- ppd_designed()
  out <- out[out$policy_alignment != "over_prioritized" | is.na(out$policy_alignment), ]
  out <- out[!(out$delivery_mode %in% "allied"), ]
  cases <- sp_card_cases(out, countries = "Japan")
  expect_equal(nrow(cases), nrow(SP_CARD_CASES))
  expect_equal(cases$case[!cases$case_found], c("deploy", "over_prioritized_deploy", "allied_high_need_low_position"))
  expect_true(is.na(cases$Country[cases$case == "over_prioritized_deploy"]))
  md <- sp_decision_cards_md(out, cases)
  expect_match(md, "No node in the table meets: Deploy; Over-prioritized Deploy; High need, low position, allied delivery.", fixed = TRUE)
  expect_match(md, "## Over-prioritized Deploy\n\n_[^\n]*_\n\nNo node in the table meets this case.")
  # Sensitive layers are not cells: a layer row is never picked.
  lay <- out[1, ]
  lay$layer_id <- "bms_firmware"
  lay$strategic_posture <- "secure"
  lay$posture_variant <- "secure_control"
  lay$policy_alignment <- "under_prioritized"
  cases2 <- sp_card_cases(dplyr::bind_rows(out, lay), countries = "Japan")
  expect_false(any(cases2$layer_id %in% "bms_firmware"))
})

test_that("the document explains the architecture and the NIPO caveat and carries no posture index", {
  out <- ppd_designed()
  cases <- sp_card_cases(out, countries = c("Japan", "China"))
  md <- sp_decision_cards_md(out, cases, iso3_names = c(CHN = "China"))
  expect_true(startsWith(md, "# Strategic posture decision cards"))
  expect_match(md, "**Strategic Position**", fixed = TRUE)
  expect_match(md, "**Strategic Posture**", fixed = TRUE)
  expect_match(md, "**Policy Design**", fixed = TRUE)
  expect_match(md, "**Evaluation**", fixed = TRUE)
  expect_match(md, "low observed NIPO/PAMS intervention, not proof that the government gives the node no priority", fixed = TRUE)
  expect_match(md, "The table holds 8 nodes: 3 Secure, 2 Compete, 2 Deploy, 1 not classified.", fixed = TRUE)
  for (lab in SP_CARD_CASES$case_label) expect_match(md, paste0("## ", lab), fixed = TRUE)
  expect_equal(lengths(regmatches(md, gregexpr("\n### ", md))), nrow(SP_CARD_CASES))
  expect_false(grepl("(yards?|fence|posture)[ _-]*(index|score)", md, ignore.case = TRUE))
  expect_no_match(md, "\\bNA\\b")
})
