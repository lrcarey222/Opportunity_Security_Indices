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
# As-of date, weights and memory-saving switches (policy_si fixes, Sep 2026)
# ==============================================================================

test_that("nipo_as_of_cap_date caps at the release month end and the run date", {
  # "GTA NIPO - July 2026.xlsx" carries the vintage key 20260701.
  expect_equal(nipo_as_of_cap_date(20260701, run_date = as.Date("2026-09-30")), as.Date("2026-07-31"))
  expect_equal(nipo_as_of_cap_date(20260701, run_date = as.Date("2026-07-10")), as.Date("2026-07-10"))
  expect_equal(nipo_as_of_cap_date(20260201), as.Date("2026-02-28"))
  expect_equal(nipo_as_of_cap_date(as.Date("2026-06-15")), as.Date("2026-06-30"))
  expect_true(is.na(nipo_as_of_cap_date(NULL, NULL)))
  expect_true(is.na(nipo_as_of_cap_date(NA, NA)))
})

test_that("add_asof_flags defaults to the latest announcement, not a scheduled implementation", {
  base <- tibble::tibble(
    policy_id = 1:3,
    announce_date = as.Date(c("2025-01-10", "2026-03-01", "2026-06-22")),
    impl_date = as.Date(c("2025-02-01", "2026-04-01", "2028-10-01")),
    removal_date = as.Date(NA)
  )

  out <- add_asof_flags(base)
  expect_equal(unique(out$as_of_date), as.Date("2026-06-22"))
  # The measure scheduled for 2028 is not yet in force.
  expect_equal(out$is_active_asof, c(TRUE, TRUE, FALSE))

  capped <- add_asof_flags(base, as_of_cap = as.Date("2026-03-15"))
  expect_equal(unique(capped$as_of_date), as.Date("2026-03-15"))
  expect_equal(capped$is_active_asof, c(TRUE, FALSE, FALSE))
})

test_that("the 2026 GTA intervention types have their own weights", {
  new_types <- c(
    "Equity stake", "Financial investment support", "Lending support", "Debt purchase",
    "Corporate control order", "Selective import channel restriction", "Distribution restriction"
  )
  expect_true(all(new_types %in% POLICY_TYPE_WEIGHTS$intervention_type))
  expect_equal(anyDuplicated(POLICY_TYPE_WEIGHTS$intervention_type), 0L)
  expect_true(all(POLICY_TYPE_WEIGHTS$w_type > 0 & POLICY_TYPE_WEIGHTS$w_type <= 1))
})

test_that("nipo_allocation_input keeps only the allocation columns", {
  wide <- tibble::tibble(
    nipo_row_id = 1L, policy_id = 1L, Title = "x", scale_strength_pkg = 2,
    unused_column = "drop me", another_unused = 3
  )
  out <- nipo_allocation_input(wide)
  expect_setequal(names(out), c("nipo_row_id", "policy_id", "Title", "scale_strength_pkg"))
})
