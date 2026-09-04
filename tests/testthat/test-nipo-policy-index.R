# Tests for R/categories/policy/nipo_policy_index.R
#
# Resolve the repo root from the test file rather than getwd(): testthat sets the
# working directory to tests/testthat, so relative source() paths would break.
repo_root <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = TRUE)

# median_scurve() and standardize_country_names() must exist before the index
# script is sourced, or its own fallback source() block fires and fails.
source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "categories", "policy", "nipo_policy_index.R"))


# ==============================================================================
# Pre-existing tests
# ------------------------------------------------------------------------------
# These four blocks predate the DIS de-biasing work and are kept as-is, except
# that the two keyword blocks passed tech/stage labels that are not keys in
# TECH_KEYWORDS ("Semiconductors Midstream", "Magnets Upstream (rare earths)").
# TECH_KEYWORDS[["Semiconductors Midstream"]] is NULL, so keyword_evidence()
# returned FALSE and those expect_true() calls could never have held. They now
# use the real dictionary keys, preserving the original intent.
# ==============================================================================

test_that("normalize_chr_vec trims and de-duplicates tech labels", {
  vals <- c("Green Hydrogen", " Green Hydrogen ", "Geothermal", "Geothermal  ", "")

  expect_equal(
    normalize_chr_vec(vals),
    c("Green Hydrogen", "Geothermal")
  )
})

test_that("clean_nipo_raw trims Technology and Value.Chain labels from subcat mappings", {
  raw_nipo <- tibble::tibble(
    `Product: HS 6-digit (2022)` = "123456",
    `Implementing Jurisdiction` = "United States"
  )

  subcat_raw <- tibble::tibble(
    HS6 = "123456",
    Technology = " Green Hydrogen ",
    Value.Chain = " Upstream ",
    Sub.Sector = "Hydrogen"
  )

  out <- clean_nipo_raw(raw_nipo = raw_nipo, subcat_raw = subcat_raw)

  expect_equal(out$Technology[[1]], "Green Hydrogen")
  expect_equal(out$`Value.Chain`[[1]], "Upstream")
})

test_that("keyword matching detects added semiconductor and magnet technologies", {
  expect_true(keyword_evidence(
    "Semiconductors",
    title = "New wafer fab and advanced chip packaging incentives",
    source = ""
  ))

  expect_true(keyword_evidence(
    "Semiconductors",
    title = "Tax relief for AI datacenter GPU clusters",
    source = ""
  ))

  expect_true(keyword_evidence(
    "Magnets",
    title = "Rare earth mining and NdPr extraction support",
    source = ""
  ))

  expect_true(keyword_evidence(
    "Magnets",
    title = "Investment in NdFeB magnet manufacturing",
    source = ""
  ))
})

test_that("supply-chain keyword matching captures semiconductor and AI stage terms", {
  expect_true(supply_chain_keyword_evidence(
    "Midstream",
    title = "Domestic foundry, fab, and ATMP capacity expansion",
    source = ""
  ))

  expect_true(supply_chain_keyword_evidence(
    "Downstream",
    title = "Cloud datacenter build-out for AI inference",
    source = ""
  ))
})


# ==============================================================================
# Task 1 - keyword word-boundary matching
# ==============================================================================

test_that("neis_bound_kws() wraps plain terms and leaves regex terms alone", {
  expect_identical(neis_bound_kws("ai"), "\\bai\\b")
  expect_identical(neis_bound_kws("turbine"), "\\bturbine\\b")

  # Already-anchored and regex-carrying terms pass through untouched.
  expect_identical(neis_bound_kws("\\bpv\\b"), "\\bpv\\b")
  expect_identical(neis_bound_kws("cells?"), "cells?")
  expect_identical(neis_bound_kws("hook[- ]?up"), "hook[- ]?up")

  # Prefix regexes must survive, or stage detection breaks outright.
  expect_identical(neis_bound_kws("\\bmanufactur\\w*"), "\\bmanufactur\\w*")
})

test_that("neis_bound_kws() optional plural suffix behaves", {
  expect_identical(neis_bound_kws("turbine", allow_plural = TRUE), "\\bturbines?\\b")
  # No doubled suffix on terms that already end in s.
  expect_identical(neis_bound_kws("gas", allow_plural = TRUE), "\\bgas\\b")
  # Default is the strict form.
  expect_identical(neis_bound_kws("turbine"), "\\bturbine\\b")
})

test_that("bare 'ai' no longer fires inside unrelated words", {
  # The mechanism behind the bug: 'ai' as an unanchored substring.
  for (word in c("Ukraine", "rail", "chain", "certain", "against", "aid", "maintain")) {
    expect_false(
      stringr::str_detect(stringr::str_to_lower(word), "\\bai\\b"),
      info = paste("bounded ai should not match", word)
    )
  }
  # ...while a standalone mention still matches, which is why \bai\b is retained
  # in the Semiconductors dictionary.
  expect_true(stringr::str_detect("support for ai chip fabrication", "\\bai\\b"))
})

test_that("the four reviewed titles no longer match Semiconductors", {
  titles <- c(
    "Russia: Sanctions on entities in Ukraine-related list",
    "Germany: Financial grant for rail freight operators",
    "Japan: Subsidy for offshore wind supply chain development",
    "EU: State aid for maintenance of hydropower plants"
  )

  for (t in titles) {
    # Every one of these matched Semiconductors under the legacy dictionary.
    expect_true(
      keyword_evidence("Semiconductors", t, "", dict = TECH_KEYWORDS_LEGACY),
      info = paste("legacy should match (regression guard):", t)
    )
    expect_false(
      keyword_evidence("Semiconductors", t, "", dict = TECH_KEYWORDS),
      info = paste("fixed dictionary should not match:", t)
    )
  }
})

test_that("the reviewed titles no longer spuriously match Downstream", {
  spurious <- c(
    "Russia: Sanctions on entities in Ukraine-related list",
    "Germany: Financial grant for rail freight operators",
    "Japan: Subsidy for offshore wind supply chain development"
  )

  for (t in spurious) {
    expect_true(
      supply_chain_keyword_evidence("Downstream", t, "", dict = SUPPLY_CHAIN_KEYWORDS_LEGACY),
      info = paste("legacy should match (regression guard):", t)
    )
    expect_false(
      supply_chain_keyword_evidence("Downstream", t, "", dict = SUPPLY_CHAIN_KEYWORDS),
      info = paste("fixed dictionary should not match:", t)
    )
  }

  # The fourth reviewed title DOES still match Downstream, and should. It
  # contains the standalone word "maintenance", a deliberate Downstream term
  # that the review did not ask to remove - O&M of a generation asset is a
  # genuine downstream activity. Its legacy match was a false positive for the
  # wrong reason ("ai" inside "maintenance"/"aid"); the fixed match is a true
  # positive for the right one.
  expect_true(
    supply_chain_keyword_evidence(
      "Downstream", "EU: State aid for maintenance of hydropower plants", "",
      dict = SUPPLY_CHAIN_KEYWORDS
    )
  )
})

test_that("prefix stems still match their inflections after bounding", {
  expect_true(supply_chain_keyword_evidence("Midstream", "battery manufacturing plant", ""))
  expect_true(supply_chain_keyword_evidence("Upstream", "nickel refining capacity", ""))
  expect_true(supply_chain_keyword_evidence("Upstream", "copper smelting support", ""))
  expect_true(supply_chain_keyword_evidence("Downstream", "rooftop solar installation", ""))
  expect_true(supply_chain_keyword_evidence("Downstream", "grid deployment programme", ""))
})

test_that("'commission' matches commissioning but not the European Commission", {
  expect_true(supply_chain_keyword_evidence("Downstream", "Commissioning of new reactor", ""))
  expect_true(supply_chain_keyword_evidence("Downstream", "plant was commissioned in 2024", ""))
  expect_false(
    supply_chain_keyword_evidence("Downstream", "EU: European Commission approves state aid", "")
  )
  expect_false(
    supply_chain_keyword_evidence("Downstream", "EU: Commission Regulation on import duties", "")
  )
})

test_that("'processing' carries no stage information and is gone from both stages", {
  # It sat in Upstream AND Midstream, so it could never discriminate between them.
  expect_false(any(grepl("processing", SUPPLY_CHAIN_KEYWORD_TERMS$Upstream)))
  expect_false(any(grepl("processing", SUPPLY_CHAIN_KEYWORD_TERMS$Midstream)))
  # Legacy had it in both - guards against a silent revert.
  expect_true(any(grepl("processing", SUPPLY_CHAIN_KEYWORD_TERMS_LEGACY$Upstream)))
  expect_true(any(grepl("processing", SUPPLY_CHAIN_KEYWORD_TERMS_LEGACY$Midstream)))
})

test_that("reviewed terms were removed from their dictionaries", {
  expect_false("packaging" %in% TECH_KEYWORD_TERMS$Semiconductors)
  expect_false("assembly" %in% TECH_KEYWORD_TERMS$Semiconductors)
  expect_false("cloud" %in% TECH_KEYWORD_TERMS$Semiconductors)
  expect_false("module" %in% TECH_KEYWORD_TERMS$Solar)
  expect_false("ai" %in% SUPPLY_CHAIN_KEYWORD_TERMS$Downstream)
  expect_false("service" %in% SUPPLY_CHAIN_KEYWORD_TERMS$Downstream)
  expect_false("operations" %in% SUPPLY_CHAIN_KEYWORD_TERMS$Downstream)

  # "ai" is deliberately RETAINED for Semiconductors, where bounded it is
  # genuinely diagnostic (242 hits / 0.4% of the July 2026 export).
  expect_true("ai" %in% TECH_KEYWORD_TERMS$Semiconductors)
})

# ==============================================================================
# Task 2 - mapping confidence separated from policy strength
# ==============================================================================

test_that("dis_confidence_weight() implements the four documented modes", {
  mc <- c(0.10, 0.25, 0.50, 0.75, 1.00, 1.50, 2.00)

  # "none": confidence contributes nothing to the product.
  expect_equal(dis_confidence_weight(mc, "none"), rep(1, length(mc)))

  # "filter": a hard gate at conf_threshold, not a weight.
  expect_equal(
    dis_confidence_weight(mc, "filter", conf_threshold = 0.75),
    c(0, 0, 0, 1, 1, 1, 1)
  )

  # "downweight": bounded reliability weight that never amplifies.
  expect_equal(dis_confidence_weight(mc, "downweight"), c(0.10, 0.25, 0.50, 0.75, 1, 1, 1))
  expect_true(all(dis_confidence_weight(mc, "downweight") <= 1))

  # "legacy": the multiplier as computed, which can exceed 1.
  expect_equal(dis_confidence_weight(mc, "legacy"), mc)
  expect_true(any(dis_confidence_weight(mc, "legacy") > 1))
})

test_that("dis_confidence_weight() honours conf_threshold and handles NA", {
  mc <- c(0.5, 0.9)
  expect_equal(dis_confidence_weight(mc, "filter", conf_threshold = 0.4), c(1, 1))
  expect_equal(dis_confidence_weight(mc, "filter", conf_threshold = 0.95), c(0, 0))

  # Missing confidence is treated as neutral (1), never as zero strength.
  expect_equal(dis_confidence_weight(c(NA, 1), "none"), c(1, 1))
  expect_equal(dis_confidence_weight(NA_real_, "downweight"), 1)
  expect_equal(dis_confidence_weight(NA_real_, "legacy"), 1)
})

test_that("dis_confidence_weight() rejects an unknown mode", {
  expect_error(dis_confidence_weight(1, "nonsense"))
})

test_that("dis_confidence_weight() returns one weight per input", {
  # It is multiplied into a column, so length must be preserved.
  for (m in DIS_CONFIDENCE_MODES) {
    expect_length(dis_confidence_weight(runif(7), m), 7)
  }
  expect_length(dis_confidence_weight(numeric(0), "none"), 0)
})

test_that("the confidence mode constants are the documented set", {
  expect_equal(DIS_CONFIDENCE_MODES, c("none", "filter", "downweight", "legacy"))
  expect_equal(DIS_DEFAULT_CONF_THRESHOLD, 0.75)
})

test_that("mapped rows cannot reach CONFIDENCE_FLOOR, so the realised range is narrower", {
  # The review described an 8x range from CONFIDENCE_FLOOR (0.25) to
  # CONFIDENCE_CAP (2). But for a mapped row, mapped_share > 0 and
  # evidence_mean >= 1, so the formula cannot fall below 0.75.
  conf <- function(mapped_share, evidence_mean) {
    pmin(CONFIDENCE_CAP, pmax(CONFIDENCE_FLOOR, 0.75 + 0.75 * mapped_share * evidence_mean))
  }
  smallest_mapped <- conf(1e-9, 1)
  expect_gte(smallest_mapped, 0.75)
  expect_equal(conf(1, 1), 1.5)
  expect_equal(conf(1, 2), CONFIDENCE_CAP)

  # The 8x swing comes from the pinned non-mapped rows instead.
  expect_equal(CONFIDENCE_CROSSCUTTING, 0.25)
  expect_equal(CONFIDENCE_UNMAPPED, 0.10)
})

# ==============================================================================
# Task 3 - geographic reach and the bare constant removed from the product
# ==============================================================================

# Minimal table with every column build_policy_base() requires, so the strength
# product can be tested without touching the real export.
make_policy_fixture <- function(n = 3,
                                partner_csv = c("Brazil", "Brazil, China",
                                                "Brazil, China, India, Japan"),
                                announce_date = as.Date("2020-01-01"),
                                impl_date = as.Date("2020-06-01"),
                                removal_date = as.Date(NA)) {
  rep_to_n <- function(x) if (length(x) == n) x else rep(x, length.out = n)

  tibble::tibble(
    nipo_row_id = seq_len(n),
    `State Act ID` = seq_len(n),
    `Entry ID` = seq_len(n),
    iso3 = rep("USA", n),
    country = rep("United States", n),
    `GTA Intervention Type` = rep("Production subsidy", n),
    `Initial Assessment (Change Relative to 1 Jan 2009)` = rep("Distortive", n),
    `Level of Government Implementation` = rep("National", n),
    `Affected Trade Flow` = rep("inward", n),
    `Announcement Date` = rep_to_n(announce_date),
    `Implementation Date` = rep_to_n(impl_date),
    `Removal Date` = rep_to_n(removal_date),
    total_hs6 = rep(4L, n),
    matched_hs6 = rep(2L, n),
    `Affected Jurisdiction` = partner_csv[seq_len(n)],
    `Sector: CPC 3-digit (v2.1)` = rep("461, 462", n),
    `Trade Covered (USD Million)` = rep(100, n),
    `Size of Subsidy (USD Million)` = rep(50, n),
    `Is Horizontal` = rep(FALSE, n),
    `Levels of Policy Intervention` = rep("Policy or regulation", n),
    `Firm: Beneficiary` = rep(NA_character_, n),
    `Is Export Policy` = rep(FALSE, n),
    `Is Import Policy` = rep(FALSE, n),
    `Is Trade Defence` = rep(FALSE, n),
    `Is Subsidy` = rep(TRUE, n),
    `Is Export Incentive` = rep(FALSE, n),
    `Is FDI Policy` = rep(FALSE, n),
    `Is Procurement Policy` = rep(FALSE, n),
    `Is Localisation Policy` = rep(FALSE, n),
    `Is Other Policy` = rep(FALSE, n),
    sector_low_carbon = rep(TRUE, n),
    sector_dual_use = rep(FALSE, n),
    sector_critical_minerals = rep(FALSE, n),
    sector_advanced_tech = rep(FALSE, n)
  )
}

test_that("m_geo is excluded from the strength product by default", {
  base <- build_policy_base(make_policy_fixture())

  # m_geo_applied records what actually entered the product.
  expect_true(all(base$m_geo_applied == 1))
  expect_equal(
    base$scale_strength_base,
    base$bite_strength_base * base$m_breadth * base$m_scale
  )
})

test_that("include_geo_in_strength = TRUE restores m_geo and the constant", {
  legacy <- build_policy_base(make_policy_fixture(),
                              include_geo_in_strength = TRUE,
                              strength_constant = 2)

  expect_equal(legacy$m_geo_applied, legacy$m_geo)
  expect_equal(
    legacy$scale_strength_base,
    legacy$bite_strength_base * legacy$m_breadth * legacy$m_geo * 2 * legacy$m_scale
  )
})

test_that("m_geo and partner_n survive as reported columns in both modes", {
  for (geo in c(FALSE, TRUE)) {
    base <- build_policy_base(make_policy_fixture(), include_geo_in_strength = geo)
    expect_true(all(c("m_geo", "partner_n", "m_geo_applied") %in% names(base)))
    # partner_n counts the CSV tokens in Affected Jurisdiction. count_csv_tokens()
    # vapply()s over a character vector, so the result carries names; unname()
    # rather than assert the incidental names.
    expect_equal(unname(base$partner_n), c(1L, 2L, 4L))
    # m_geo still varies with partner count even when it does not enter strength.
    expect_true(all(base$m_geo >= 1))
    expect_gt(base$m_geo[3], base$m_geo[1])
  }
})

test_that("dropping m_geo removes a term that varies across measures", {
  # The point of the fix: under legacy, three otherwise IDENTICAL measures get
  # different strengths purely because they name different numbers of affected
  # jurisdictions.
  legacy <- build_policy_base(make_policy_fixture(),
                              include_geo_in_strength = TRUE,
                              strength_constant = 2)
  fixed <- build_policy_base(make_policy_fixture())

  expect_gt(length(unique(round(legacy$scale_strength_base, 10))), 1)
  expect_equal(length(unique(round(fixed$scale_strength_base, 10))), 1)
})

test_that("strength_constant only rescales and cannot change any ranking", {
  one <- build_policy_base(make_policy_fixture(), strength_constant = 1)
  two <- build_policy_base(make_policy_fixture(), strength_constant = 2)

  expect_equal(two$scale_strength_base, one$scale_strength_base * 2)
  expect_equal(
    rank(one$scale_strength_base),
    rank(two$scale_strength_base)
  )
})

# ==============================================================================
# Task 4 - announced-but-unimplemented measures stop being discarded
# ==============================================================================

test_that("pending_implementation flags announced measures with no impl date", {
  base <- build_policy_base(make_policy_fixture(
    n = 3,
    announce_date = as.Date(c("2020-01-01", "2020-01-01", NA)),
    impl_date = as.Date(c("2020-06-01", NA, NA))
  ))

  expect_equal(base$pending_implementation, c(FALSE, TRUE, FALSE))
})

test_that("impl_lag_days measures the announcement-to-implementation gap", {
  base <- build_policy_base(make_policy_fixture(
    n = 4,
    announce_date = as.Date(c("2020-01-01", "2020-01-01", "2020-06-01", NA)),
    impl_date = as.Date(c("2020-01-31", NA, "2020-01-01", "2020-06-01"))
  ))

  # 30-day gap; pending; negative lag (data error) -> NA; missing announce -> NA.
  expect_equal(base$impl_lag_days, c(30, NA, NA, NA))
})

test_that("m_duration still zeroes pending measures, preserving stock semantics", {
  base <- build_policy_base(make_policy_fixture(
    n = 2,
    announce_date = as.Date(c("2020-01-01", "2020-01-01")),
    impl_date = as.Date(c("2020-06-01", NA))
  ))

  expect_equal(base$m_duration[2], 0)
  # And so the whole product is zero for that row - the behaviour Task 4
  # documents rather than changes.
  expect_equal(base$scale_strength_pkg[2], 0)
  expect_gt(base$scale_strength_pkg[1], 0)
})

test_that("m_duration saturates at 1 for every in-force measure", {
  # The reason m_duration carries no information for the 69% of the export that
  # is implemented with no removal date: planned_end is impl + 60 months, so
  # m_duration = min(1, sqrt(60/24)) = 1 regardless of actual age.
  base <- build_policy_base(make_policy_fixture(
    n = 2,
    impl_date = as.Date(c("2009-01-01", "2026-01-01")),
    removal_date = as.Date(c(NA, NA))
  ))

  expect_equal(base$m_duration, c(1, 1))
})

test_that("observed_duration_days is NA while in force and exact once removed", {
  asof <- as.Date("2026-06-30")
  base <- add_asof_flags(
    build_policy_base(make_policy_fixture(
      n = 3,
      impl_date = as.Date(c("2020-01-01", "2020-01-01", NA)),
      removal_date = as.Date(c("2020-01-31", NA, NA))
    )),
    as_of_date = asof
  )

  expect_equal(base$observed_duration_days, c(30, NA, NA))
  expect_equal(base$duration_censored, c(FALSE, TRUE, FALSE))
})

test_that("exposure_days censors at as_of_date", {
  asof <- as.Date("2020-12-31")
  base <- add_asof_flags(
    build_policy_base(make_policy_fixture(
      n = 3,
      impl_date = as.Date(c("2020-01-01", "2020-01-01", NA)),
      removal_date = as.Date(c("2020-01-31", NA, NA))
    )),
    as_of_date = asof
  )

  # Removed measure: full lifetime. In-force: censored at as_of. Pending: NA.
  expect_equal(base$exposure_days, c(30, 365, NA))
})

test_that("a removal date after as_of_date is censored, not counted", {
  base <- add_asof_flags(
    build_policy_base(make_policy_fixture(
      n = 1, impl_date = as.Date("2020-01-01"),
      removal_date = as.Date("2030-01-01")
    )),
    as_of_date = as.Date("2020-12-31")
  )

  expect_equal(base$exposure_days, 365)
  expect_true(base$duration_censored)
})

test_that("add_asof_flags warns when the as-of date is in the future", {
  fixture <- build_policy_base(make_policy_fixture(n = 1))
  expect_warning(
    add_asof_flags(fixture, as_of_date = Sys.Date() + 400),
    "in the future"
  )
  expect_silent(add_asof_flags(fixture, as_of_date = Sys.Date() - 1))
})

test_that("the new duration columns do not enter the strength product", {
  # Two measures identical except that one has ended, so they differ in
  # observed_duration_days and exposure_days but must not differ in strength.
  base <- add_asof_flags(
    build_policy_base(make_policy_fixture(
      n = 2, partner_csv = c("Brazil", "Brazil"),
      impl_date = as.Date(c("2020-01-01", "2020-01-01")),
      removal_date = as.Date(c(as.Date("2026-01-01"), NA))
    )),
    as_of_date = as.Date("2026-06-30")
  )

  expect_false(identical(base$exposure_days[1], base$exposure_days[2]))
  # m_duration does differ (the removed one is downweighted, the perverse
  # behaviour Task 4 documents), but nothing reads the new columns.
  expect_true(all(c("observed_duration_days", "exposure_days",
                    "impl_lag_days", "pending_implementation") %in% names(base)))
})

test_that("pending rows survive into the allocation table", {
  # The requirement: pending measures must reach the allocation table so the
  # Task 9 delivery metrics can use them, even though they carry zero strength
  # and are excluded from the active stock.
  fixture <- make_policy_fixture(
    n = 2,
    announce_date = as.Date(c("2020-01-01", "2020-01-01")),
    impl_date = as.Date(c("2020-06-01", NA))
  )
  fixture$Technology <- list("Solar", "Solar")
  fixture$`Value.Chain` <- list("Midstream", "Midstream")
  fixture$hs6_codes <- list("854143", "854143")
  fixture$cpc3_codes <- list("461", "461")
  fixture$cpc3_n <- c(1L, 1L)
  fixture$allowed_pairs_cpc <- list(character(0), character(0))
  fixture$Title <- c("solar module plant", "solar module plant")
  fixture$source_text <- c("", "")
  # allocate_policy_to_tech_sc() reads coalesce(source_text, Source, ""), so
  # both text columns must exist.
  fixture$Source <- c("", "")

  asof <- add_asof_flags(build_policy_base(fixture), as_of_date = as.Date("2026-06-30"))
  expect_equal(nrow(asof), 2)

  alloc <- allocate_policy_to_tech_sc(asof)
  # Both policies present in the allocation, pending one included.
  expect_setequal(unique(alloc$policy_id), c(1, 2))
  expect_true(any(alloc$pending_implementation))

  # ...but the pending one is not part of the active stock.
  expect_equal(sum(asof$is_active_asof), 1)
})

# ==============================================================================
# Task 5 - the scale multiplier split into exposure and fiscal
# ==============================================================================

# Scale fields vary per row: one trade-only, one subsidy-only, one both, one
# neither. p95 is computed within the call, so compare across modes not to
# hard-coded multiplier values.
make_scale_fixture <- function() {
  f <- make_policy_fixture(n = 4, partner_csv = rep("Brazil", 4))
  f$`Trade Covered (USD Million)` <- c(500, NA, 500, NA)
  f$`Size of Subsidy (USD Million)` <- c(NA, 500, 500, NA)
  f
}

test_that("m_scale_exposure and m_scale_fiscal are reported separately", {
  base <- build_policy_base(make_scale_fixture())

  expect_true(all(c("m_scale_exposure", "m_scale_fiscal",
                    "scale_exposure_available", "scale_fiscal_available")
                  %in% names(base)))
  # The legacy m_trade / m_subsidy columns are kept, not renamed away.
  expect_true(all(c("m_trade", "m_subsidy", "m_scale") %in% names(base)))
  expect_equal(base$m_scale_exposure, base$m_trade)
  expect_equal(base$m_scale_fiscal, base$m_subsidy)
})

test_that("availability flags distinguish missing from smallest-scale", {
  base <- build_policy_base(make_scale_fixture())

  expect_equal(base$scale_exposure_available, c(TRUE, FALSE, TRUE, FALSE))
  expect_equal(base$scale_fiscal_available, c(FALSE, TRUE, TRUE, FALSE))

  # This is the defect the flags expose: log_mult() maps NA to 0 and returns a
  # multiplier of 1, so a missing field is numerically identical to a genuinely
  # zero-value one. Row 4 has neither field populated yet still scores 1.
  expect_equal(base$m_scale_exposure[2], 1)
  expect_equal(base$m_scale_exposure[4], 1)
  expect_false(base$scale_exposure_available[2])
})

test_that("scale_mode selects the intended term", {
  exposure <- build_policy_base(make_scale_fixture(), scale_mode = "exposure")
  fiscal <- build_policy_base(make_scale_fixture(), scale_mode = "fiscal")
  legacy <- build_policy_base(make_scale_fixture(), scale_mode = "max")
  none <- build_policy_base(make_scale_fixture(), scale_mode = "none")

  expect_equal(exposure$m_scale, exposure$m_scale_exposure)
  expect_equal(fiscal$m_scale, fiscal$m_scale_fiscal)
  expect_equal(legacy$m_scale, pmax(legacy$m_trade, legacy$m_subsidy))
  expect_equal(none$m_scale, rep(1, 4))
})

test_that("exposure is the default scale_mode", {
  default <- build_policy_base(make_scale_fixture())
  exposure <- build_policy_base(make_scale_fixture(), scale_mode = "exposure")
  expect_equal(default$m_scale, exposure$m_scale)
})

test_that("legacy max() lets exposure stand in for fiscal commitment", {
  # The substantive complaint: a trade-heavy measure with NO fiscal outlay
  # (row 1) scores the same under "max" as a large subsidy with no trade
  # coverage (row 2), because max() cannot tell the two apart.
  legacy <- build_policy_base(make_scale_fixture(), scale_mode = "max")
  expect_equal(legacy$m_scale[1], legacy$m_scale[2])

  # Under "fiscal" they separate, because row 1 committed no money.
  fiscal <- build_policy_base(make_scale_fixture(), scale_mode = "fiscal")
  expect_lt(fiscal$m_scale[1], fiscal$m_scale[2])
})

test_that("scale_mode rejects an unknown value", {
  expect_error(build_policy_base(make_scale_fixture(), scale_mode = "nonsense"))
})

test_that("neis_audit_keywords() reports hit rates in the documented shape", {
  txt <- c(
    "support for ai chip fabrication",
    "grant for rail freight operators",
    "offshore wind supply chain"
  )
  audit <- neis_audit_keywords(TECH_KEYWORDS, txt, warn_hit_rate = 0.5)

  expect_true(all(c("group", "term", "hits", "hit_rate", "flagged") %in% names(audit)))
  # Sorted by hit rate, descending.
  expect_equal(audit$hit_rate, sort(audit$hit_rate, decreasing = TRUE))
  expect_true(all(audit$hit_rate >= 0 & audit$hit_rate <= 1))
  expect_equal(audit$hits, audit$hit_rate * length(txt))

  ai_row <- audit[audit$group == "Semiconductors" & audit$term == "\\bai\\b", ]
  expect_equal(ai_row$hits, 1L)
})
