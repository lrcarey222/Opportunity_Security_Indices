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
