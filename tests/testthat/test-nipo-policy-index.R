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
