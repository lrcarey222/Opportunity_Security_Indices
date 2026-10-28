# Tests for R/categories/policy/nipo_stage_policy.R: the NIPO direct six-stage policy layer
# (docs/strategic_position.md, section 4 "NIPO"). Synthetic fixtures only.

nipo_stage_repo_root <- function() {
  normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
}

# median_scurve() and standardize_country_names() must exist before the legacy index file is
# sourced, or its own fallback source() block fires; that file supplies clean_code() and friends.
source(file.path(nipo_stage_repo_root(), "R", "utils", "scurve.R"))
source(file.path(nipo_stage_repo_root(), "R", "utils", "country.R"))
source(file.path(nipo_stage_repo_root(), "R", "utils", "supply_chain_stages.R"))
source(file.path(nipo_stage_repo_root(), "R", "categories", "policy", "nipo_policy_index.R"))
source(file.path(nipo_stage_repo_root(), "R", "categories", "policy", "nipo_stage_policy.R"))

nipo_fx_as_of <- as.Date("2026-09-18")

# The legacy three-stage supply_chain column is filled with values that would put Components
# codes on Assembly (and Refining on Extraction) if it were read.
nipo_fx_master <- function() {
  tibble::tribble(
    ~tech, ~supply_chain, ~hs6, ~essential, ~stage,
    "Solar", "Midstream", "854142", "TRUE", "Components",
    "Solar", "Midstream", "854143", "TRUE", "Assembly",
    "Wind", "Midstream", "850231", "TRUE", "Assembly",
    "Electric Vehicles", "Midstream", "870380", "TRUE", "Assembly",
    "Electric Vehicles", "Midstream", "850440", "TRUE", "Assembly",
    "Solar", "Midstream", "850440", "TRUE", "Assembly",
    "Electric Grid", "Midstream", "850440", "TRUE", "Assembly",
    "Batteries", "Upstream", "282520", "TRUE", "Refining",
    "Batteries", "Upstream", "253090", "TRUE", "Extraction",
    "Electric Grid", "Midstream", "760511", "FALSE", "Components",
    # A Deployment row: HS6 cannot evidence deployment, so NIPO must drop it.
    "Solar", "Downstream", "999999", "TRUE", "Deployment"
  )
}

nipo_fx_posture <- function() {
  tibble::tribble(
    ~gta_intervention_type, ~family, ~support, ~protection, ~strength_class, ~notes,
    "Financial grant", "grant", 1, 0, 2, "",
    "State loan", "public_finance", 1, 0, 1, "",
    "Import tariff", "tariff", 0, 1, 1, "",
    "Local content incentive", "localisation_incentive", 1, 1, 1, "",
    "Export ban", "export_restriction", 0, 1, 2, ""
  )
}

# Thirty codes outside the basket, to make a measure broad.
nipo_fx_filler <- function(n = 29) paste(sprintf("%06d", 10101 + seq_len(n)), collapse = ",")

nipo_fx_row <- function(entry, juris, type, hs, ann, impl = ann, rem = NA, assess = "Distortive",
                        sanction = FALSE, level = "National", subsidy = NA, trade = NA, act = entry,
                        flags = list()) {
  out <- tibble::tibble(
    `State Act ID` = act,
    `Entry ID` = entry,
    `Implementing Jurisdiction` = juris,
    `Level of Government Implementation` = level,
    `Initial Assessment (Change Relative to 1 Jan 2009)` = assess,
    `Announcement Date` = as.POSIXct(ann, tz = "UTC"),
    `Implementation Date` = as.POSIXct(impl, tz = "UTC"),
    `Removal Date` = as.POSIXct(rem, tz = "UTC"),
    `Product: HS 6-digit (2022)` = hs,
    `Is Sanction` = sanction,
    `GTA Intervention Type` = type,
    `Trade Covered (USD Million)` = trade,
    `Size of Subsidy (USD Million)` = subsidy
  )
  out
}

nipo_fx_raw <- function() {
  us <- "United States of America"
  dplyr::bind_rows(
    # 1 narrow grant at Solar Components only
    nipo_fx_row(1, us, "Financial grant", "854142", "2024-01-01", subsidy = 10),
    # 2 tariff on two Assembly codes of two technologies
    nipo_fx_row(2, us, "Import tariff", "854143,850231", "2024-02-01", trade = 100),
    # 3 local content incentive: support and protection
    nipo_fx_row(3, us, "Local content incentive", "870380", "2024-03-01"),
    # 4 broad, one basket technology: tech_wide
    nipo_fx_row(4, us, "Financial grant", paste0("850231,", nipo_fx_filler()), "2024-04-01"),
    # 5 broad, two technologies: cross_cutting
    nipo_fx_row(5, us, "Financial grant", paste0("850231,870380,", nipo_fx_filler(28)), "2024-05-01"),
    # 6 no basket code; 7 no HS6 at all
    nipo_fx_row(6, us, "Financial grant", "010121", "2024-06-01"),
    nipo_fx_row(7, us, "Financial grant", NA_character_, "2024-07-01"),
    # 8 sanction; 9 liberalising
    nipo_fx_row(8, us, "Export ban", "282520", "2024-08-01", sanction = TRUE),
    nipo_fx_row(9, us, "Import tariff", "854142", "2024-09-01", assess = "Liberalising"),
    # 10 removed before the as-of date
    nipo_fx_row(10, us, "Financial grant", "854142", "2015-01-01", rem = "2020-01-01"),
    # 11 announced, not implemented
    nipo_fx_row(11, us, "Financial grant", "854142", "2026-06-01", impl = NA),
    # 12 announced exactly three years before as-of (outside the 3-year window); 13 one day later
    nipo_fx_row(12, us, "Financial grant", "854142", "2023-09-18", impl = NA),
    nipo_fx_row(13, us, "Financial grant", "854142", "2023-09-19", impl = NA),
    # 14 a code shared by three technologies
    nipo_fx_row(14, us, "State loan", "850440", "2024-10-01"),
    # 15 an essential and a non-essential code: placed by the essential one only
    nipo_fx_row(15, us, "State loan", "854142,760511", "2024-11-01"),
    # 16 only a non-essential code: placed by it (link_basis all)
    nipo_fx_row(16, us, "State loan", "760511", "2024-12-01"),
    # 17 only a Deployment code: unmapped for NIPO
    nipo_fx_row(17, us, "State loan", "999999", "2025-01-01"),
    # 20 an EU act recorded once per member state
    nipo_fx_row(20, "Germany", "Financial grant", "282520", "2025-02-01", level = "Supra-national", subsidy = 50),
    nipo_fx_row(20, "France", "Financial grant", "282520", "2025-02-01", level = "Supra-national", subsidy = 50),
    # 21 Germany's own measure at the same cell
    nipo_fx_row(21, "Germany", "State loan", "282520", "2025-03-01")
  )
}

nipo_fx_cfg <- function() {
  list(
    flow_window_years = 3,
    flow_windows_sensitivity = c(1, 3, 5),
    as_of = format(nipo_fx_as_of),
    nipo = list(
      narrow_max_codes = 20, narrow_min_basket_share = 0.5, exclude_sanctions = TRUE,
      count_assessments = c("Red", "Amber"), eu_supranational = "report_only"
    )
  )
}

nipo_fx_out <- function() {
  nipo_stage_policy_outputs(nipo_fx_raw(), nipo_fx_master(), nipo_fx_posture(), nipo_fx_cfg())
}

nipo_fx_measure <- function(out, id) out$measures[out$measures$intervention_id == id, ]

nipo_fx_cell <- function(out, iso3, tech, stage) {
  out$cells[out$cells$iso3 == iso3 & out$cells$tech == tech & out$cells$supply_chain == stage, ]
}

test_that("the output tables carry the contract's columns", {
  out <- nipo_fx_out()
  expect_identical(names(out$measures), NIPO_STAGE_MEASURE_COLUMNS)
  expect_identical(names(out$links), NIPO_STAGE_LINK_COLUMNS)
  expect_identical(names(out$cells), c(
    "iso3", "tech", "supply_chain", "support_stock", "protection_stock", "all_stock",
    "support_stock_iw", "protection_stock_iw", "all_stock_iw", "support_flow_1y", "support_flow_3y",
    "support_flow_5y", "protection_flow_1y", "protection_flow_3y", "protection_flow_5y",
    "all_flow_1y", "all_flow_3y", "all_flow_5y", "all_flow_3y_iw", "n_measures_stock",
    "n_instruments_stock", "n_new_1y", "n_new_3y", "n_new_5y", "subsidy_usd_stock",
    "trade_covered_usd_stock", "fiscal_data_coverage", "trade_data_coverage",
    "eu_supranational_stock", "eu_supranational_flow_3y"
  ))
  expect_identical(names(out$country), c(
    "iso3", "as_of", "n_measures", "n_counted", "n_excluded_sanction", "n_excluded_liberalising",
    "share_attributed", "share_tech_wide", "share_cross_cutting", "share_unmapped", "n_stock", "n_flow_3y"
  ))
  expect_equal(anyDuplicated(out$measures$intervention_id), 0)
  expect_equal(out$as_of, nipo_fx_as_of)
})

test_that("a code is placed on its own six-stage cell, never through the legacy three stages", {
  out <- nipo_fx_out()
  l1 <- out$links[out$links$intervention_id == "1", ]
  expect_equal(nrow(l1), 1)
  expect_equal(l1$tech, "Solar")
  expect_equal(l1$supply_chain, "Components")
  expect_equal(l1$attribution_share, 1)

  # The cells are exactly the master's tech x stage cells (no Upstream / Midstream labels, no
  # copy onto the other manufacturing stage, no Deployment).
  master_cells <- unique(paste(nipo_fx_master()$tech, nipo_fx_master()$stage))
  expect_setequal(
    unique(paste(out$cells$tech, out$cells$supply_chain)),
    setdiff(master_cells, "Solar Deployment")
  )
  assert_supply_chain_stages(out$cells$supply_chain, "cells")
  assert_supply_chain_stages(out$links$supply_chain, "links")
  expect_equal(nipo_fx_cell(out, "USA", "Solar", "Assembly")$support_stock, 1 / 3) # only measure 14's share
})

test_that("Deployment is absent and a Deployment-only measure is unmapped", {
  out <- nipo_fx_out()
  expect_false("Deployment" %in% out$cells$supply_chain)
  expect_false("Deployment" %in% out$links$supply_chain)
  expect_equal(nipo_fx_measure(out, "17")$mapping_status, "unmapped")
  # Techs without a basket (Geothermal here) have no cells at all: no_data, not zero.
  expect_false("Geothermal" %in% out$cells$tech)
})

test_that("broad measures are classified tech_wide or cross_cutting and enter no cell", {
  out <- nipo_fx_out()
  m4 <- nipo_fx_measure(out, "4")
  m5 <- nipo_fx_measure(out, "5")
  expect_equal(m4$mapping_status, "tech_wide")
  expect_equal(m4$mapped_techs, "Wind")
  expect_equal(m4$n_codes, 30L)
  expect_equal(m4$n_basket_codes, 1L)
  expect_equal(m5$mapping_status, "cross_cutting")
  expect_equal(m5$mapped_techs, "Electric Vehicles;Wind")
  expect_false(any(c("4", "5") %in% out$links$intervention_id))
  expect_equal(nipo_fx_measure(out, "6")$mapping_status, "unmapped")
  expect_equal(nipo_fx_measure(out, "7")$mapping_status, "unmapped")
  expect_equal(nipo_fx_measure(out, "7")$n_codes, 0L)

  # Wind Assembly holds only measure 2's half share, not the broad grants.
  wind <- nipo_fx_cell(out, "USA", "Wind", "Assembly")
  expect_equal(wind$all_stock, 0.5)
  expect_equal(wind$support_stock, 0)

  # A broad measure that is narrow by basket share is attributed.
  narrow_by_share <- nipo_stage_map_measures(
    tibble::tibble(intervention_id = "x", iso3 = "USA", hs6_list = paste(rep(c("854142", "854143"), 1), collapse = ",")),
    nipo_stage_prepare_master(nipo_fx_master()),
    narrow_max_codes = 1, narrow_min_basket_share = 0.5
  )
  expect_equal(narrow_by_share$measures$mapping_status, "attributed")
})

test_that("attribution shares sum to 1 per measure and never exceed the measure count", {
  out <- nipo_fx_out()
  sums <- tapply(out$links$attribution_share, out$links$intervention_id, sum)
  expect_true(all(abs(sums - 1) < 1e-12))
  attributed <- out$measures$intervention_id[out$measures$mapping_status == "attributed"]
  expect_setequal(unique(out$links$intervention_id), attributed)

  # A code in three technologies splits its weight three ways.
  l14 <- out$links[out$links$intervention_id == "14", ]
  expect_equal(nrow(l14), 3)
  expect_equal(l14$attribution_share, rep(1 / 3, 3))

  # Summed over a country's cells, stock equals its counted attributed measures in force.
  m <- out$measures
  for (iso in unique(out$cells$iso3)) {
    n_active <- sum(m$iso3 == iso & m$counted & m$mapping_status == "attributed" & m$active_at_as_of)
    expect_equal(sum(out$cells$all_stock[out$cells$iso3 == iso]), n_active, info = iso)
    expect_lte(sum(out$cells$support_stock[out$cells$iso3 == iso]), n_active)
  }
})

test_that("essential codes place a measure when it has any; otherwise all its codes do", {
  out <- nipo_fx_out()
  m15 <- nipo_fx_measure(out, "15")
  expect_equal(m15$link_basis, "essential")
  l15 <- out$links[out$links$intervention_id == "15", ]
  expect_equal(paste(l15$tech, l15$supply_chain), "Solar Components")
  expect_equal(l15$attribution_share, 1)
  expect_equal(m15$n_basket_codes, 2L)

  m16 <- nipo_fx_measure(out, "16")
  expect_equal(m16$link_basis, "all")
  l16 <- out$links[out$links$intervention_id == "16", ]
  expect_equal(paste(l16$tech, l16$supply_chain), "Electric Grid Components")
  expect_equal(l16$n_codes_in_cell, 1L)
  expect_equal(l16$n_essential_codes_in_cell, 0L)
})

test_that("support and protection come from the posture table and a measure can be both", {
  out <- nipo_fx_out()
  ev <- nipo_fx_cell(out, "USA", "Electric Vehicles", "Assembly")
  # measure 3 (both) plus a third of measure 14 (support)
  expect_equal(ev$support_stock, 1 + 1 / 3)
  expect_equal(ev$protection_stock, 1)
  expect_equal(ev$all_stock, 1 + 1 / 3)

  sc <- nipo_fx_cell(out, "USA", "Solar", "Components")
  # measures 1 (grant, strength 2) and 15 (loan, strength 1) in stock
  expect_equal(sc$support_stock, 2)
  expect_equal(sc$support_stock_iw, 3)
  expect_equal(sc$n_measures_stock, 2L)
  expect_equal(sc$n_instruments_stock, 2L)

  # Unknown instrument types stop the build rather than defaulting silently.
  raw <- nipo_fx_raw()
  raw$`GTA Intervention Type`[1] <- "Brand new instrument"
  expect_error(
    nipo_stage_clean_measures(raw, nipo_fx_posture()),
    "no row for GTA Intervention Type"
  )
})

test_that("stock is in force on the as-of date and flow is announced in the window", {
  out <- nipo_fx_out()
  expect_false(nipo_fx_measure(out, "10")$active_at_as_of)
  expect_false(nipo_fx_measure(out, "11")$active_at_as_of)
  expect_true(nipo_fx_measure(out, "1")$active_at_as_of)

  sc <- nipo_fx_cell(out, "USA", "Solar", "Components")
  # 1-year window: measure 11 only (announced 2026-06-01, not implemented)
  expect_equal(sc$support_flow_1y, 1)
  expect_equal(sc$n_new_1y, 1L)
  # 3-year window (2023-09-18, 2026-09-18]: 1, 11, 13 and 15; 12 sits on the open edge
  expect_equal(sc$n_new_3y, 4L)
  expect_equal(sc$all_flow_3y_iw, 2 + 2 + 2 + 1)
  # 5-year window adds 12
  expect_equal(sc$n_new_5y, 5L)

  expect_true(nipo_stage_announced_within(as.Date("2026-09-18"), nipo_fx_as_of, 1))
  expect_false(nipo_stage_announced_within(as.Date("2025-09-18"), nipo_fx_as_of, 1))
  expect_false(nipo_stage_announced_within(as.Date("2026-09-19"), nipo_fx_as_of, 1))
  expect_false(nipo_stage_active(as.Date("2020-01-01"), as.Date("2026-09-18"), nipo_fx_as_of))
  expect_true(nipo_stage_active(as.Date("2020-01-01"), as.Date("2026-09-19"), nipo_fx_as_of))
  expect_true(nipo_stage_active(as.Date("2026-09-18"), as.Date(NA), nipo_fx_as_of))
})

test_that("sanctions and liberalising measures are excluded from the counts and reported", {
  out <- nipo_fx_out()
  m8 <- nipo_fx_measure(out, "8")
  m9 <- nipo_fx_measure(out, "9")
  expect_false(m8$counted)
  expect_equal(m8$exclusion_reason, "sanction")
  expect_false(m9$counted)
  expect_equal(m9$exclusion_reason, "liberalising")
  # Distortive is GTA's red or amber, so it counts under count_assessments [Red, Amber].
  expect_true(nipo_fx_measure(out, "1")$counted)

  us <- out$country[out$country$iso3 == "USA", ]
  expect_equal(us$n_measures, 17L)
  expect_equal(us$n_counted, 15L)
  expect_equal(us$n_excluded_sanction, 1L)
  expect_equal(us$n_excluded_liberalising, 1L)
  expect_equal(us$share_tech_wide, 1 / 15)
  expect_equal(us$share_cross_cutting, 1 / 15)
  expect_equal(us$share_unmapped, 3 / 15)
  expect_equal(us$share_attributed, 10 / 15)

  # The sanctioned export ban maps to Batteries Refining but adds nothing there.
  expect_equal(nipo_fx_cell(out, "USA", "Batteries", "Refining")$protection_stock, 0)

  # With exclude_sanctions off it counts.
  cfg <- nipo_fx_cfg()
  cfg$nipo$exclude_sanctions <- FALSE
  out2 <- nipo_stage_policy_outputs(nipo_fx_raw(), nipo_fx_master(), nipo_fx_posture(), cfg)
  expect_equal(nipo_fx_cell(out2, "USA", "Batteries", "Refining")$protection_stock, 1)
})

test_that("EU acts are the EU's and stay out of members' own stock", {
  out <- nipo_fx_out()
  eu <- nipo_fx_measure(out, "20")
  expect_equal(nrow(eu), 1)
  expect_equal(eu$iso3, "EUU")
  expect_true(eu$is_eu_supranational)
  expect_equal(eu$subsidy_usd, 50e6) # the act's budget once, not once per member

  de <- nipo_fx_cell(out, "DEU", "Batteries", "Refining")
  fr <- nipo_fx_cell(out, "FRA", "Batteries", "Refining")
  eu_cell <- nipo_fx_cell(out, "EUU", "Batteries", "Refining")
  expect_equal(de$all_stock, 1) # Germany's own state loan only
  expect_equal(de$eu_supranational_stock, 1)
  expect_equal(eu_cell$all_stock, 1)
  expect_true(is.na(eu_cell$eu_supranational_stock))
  expect_true(is.na(nipo_fx_cell(out, "USA", "Batteries", "Refining")$eu_supranational_stock))
  # France has no counted measure of its own, so it has no cells (the contract's universe).
  expect_false("FRA" %in% out$cells$iso3)
  expect_equal(nrow(fr), 0)

  # Multi-member state aid that is not EU-wide stays with each member.
  raw <- dplyr::bind_rows(
    nipo_fx_raw(),
    nipo_fx_row(30, "Germany", "State loan", "253090", "2025-04-01", subsidy = 90),
    nipo_fx_row(30, "France", "State loan", "253090", "2025-04-01", subsidy = 90)
  )
  out3 <- nipo_stage_policy_outputs(raw, nipo_fx_master(), nipo_fx_posture(), nipo_fx_cfg())
  expect_setequal(out3$measures$intervention_id[grepl("^30", out3$measures$intervention_id)], c("30-DEU", "30-FRA"))
  expect_equal(nipo_fx_cell(out3, "FRA", "Batteries", "Extraction")$all_stock, 1)
  # The repeated budget is split between the two members' records.
  expect_equal(sum(out3$measures$subsidy_usd[grepl("^30", out3$measures$intervention_id)]), 90e6)
})

test_that("identified subsidy and trade values are attributed and their coverage reported", {
  out <- nipo_fx_out()
  sc <- nipo_fx_cell(out, "USA", "Solar", "Components")
  expect_equal(sc$subsidy_usd_stock, 10e6)
  # support stock 2: measure 1 reports a subsidy size, measure 15 does not
  expect_equal(sc$fiscal_data_coverage, 0.5)
  expect_true(is.na(sc$trade_data_coverage))
  sa <- nipo_fx_cell(out, "USA", "Solar", "Assembly")
  expect_equal(sa$trade_covered_usd_stock, 50e6)
  expect_equal(sa$trade_data_coverage, 1)
  expect_true(is.na(sa$subsidy_usd_stock)) # stock exists, no subsidy figure
  empty <- nipo_fx_cell(out, "USA", "Batteries", "Extraction")
  expect_equal(empty$all_stock, 0)
  expect_equal(empty$subsidy_usd_stock, 0)
})

test_that("the posture cross-check compares the table with GTA's family flags", {
  raw <- nipo_fx_raw()
  raw$`Is Subsidy` <- raw$`GTA Intervention Type` %in% c("Financial grant", "State loan")
  raw$`Is Import Policy` <- raw$`GTA Intervention Type` == "Import tariff"
  raw$`Is Localisation Policy` <- raw$`GTA Intervention Type` == "Local content incentive"
  raw$`Is Other Policy` <- raw$`GTA Intervention Type` == "Export ban" # deliberately inconsistent
  chk <- nipo_stage_posture_flag_check(raw, nipo_fx_posture())
  expect_true(chk$agrees[chk$gta_intervention_type == "Financial grant"])
  expect_true(chk$agrees[chk$gta_intervention_type == "Local content incentive"])
  expect_false(chk$agrees[chk$gta_intervention_type == "Export ban"])
})

test_that("the committed posture table is valid and uses the major class sparingly", {
  posture <- utils::read.csv(
    file.path(nipo_stage_repo_root(), "config", "nipo_instrument_posture.csv"),
    stringsAsFactors = FALSE
  )
  p <- nipo_stage_prepare_posture(posture)
  expect_true(all(c("Financial grant", "Import tariff", "Anti-dumping", "Export ban") %in% p$gta_intervention_type))
  expect_lte(sum(p$strength_class == 2), 10)
  expect_true(all(p$support == 1 | p$protection == 1 | p$family == "other"))
  expect_true(any(p$support == 1 & p$protection == 1))
})

test_that("one-off measures without a removal date lapse from stock after one_off_stock_years", {
  us <- "United States of America"
  # as_of 2026-09-18, five years: the window is (2021-09-18, 2026-09-18].
  raw <- dplyr::bind_rows(
    nipo_fx_raw(),
    # 40 grant implemented on the window's open edge: lapsed; 41 one day later: in stock
    nipo_fx_row(40, us, "Financial grant", "253090", "2021-09-18"),
    nipo_fx_row(41, us, "Financial grant", "253090", "2021-09-19"),
    # 42 an old state loan with no removal date: lapsed; 43 the same with a future removal date:
    # GTA knows when it ends, so it stays until then
    nipo_fx_row(42, us, "State loan", "253090", "2012-05-01"),
    nipo_fx_row(43, us, "State loan", "253090", "2012-05-01", rem = "2030-01-01"),
    # 44 an old tariff (not a one-off family): stays until removed
    nipo_fx_row(44, us, "Import tariff", "253090", "2012-05-01")
  )
  out <- nipo_stage_policy_outputs(raw, nipo_fx_master(), nipo_fx_posture(), nipo_fx_cfg())
  act <- stats::setNames(out$measures$active_at_as_of, out$measures$intervention_id)
  rule <- stats::setNames(out$measures$stock_rule, out$measures$intervention_id)
  expect_false(act[["40"]])
  expect_true(act[["41"]])
  expect_false(act[["42"]])
  expect_true(act[["43"]])
  expect_true(act[["44"]])
  expect_equal(unname(rule[c("40", "42", "43", "44")]), c("one_off_window", "one_off_window", "until_removed", "until_removed"))
  expect_equal(out$settings$one_off_stock_years, 5L)

  # Cells and the country table use the same rule: 41 (grant, support), 43 (loan, support) and
  # 44 (tariff, protection) at Batteries Extraction; 40 and 42 drop out.
  be <- nipo_fx_cell(out, "USA", "Batteries", "Extraction")
  expect_equal(be$n_measures_stock, 3L)
  expect_equal(be$support_stock, 2)
  expect_equal(be$protection_stock, 1)
  m <- out$measures
  expect_equal(
    out$country$n_stock[out$country$iso3 == "USA"],
    sum(m$iso3 == "USA" & m$counted & m$active_at_as_of)
  )
  # Flow is announcement-dated: the 5-year flow window has the same open edge, so it holds 41
  # only, with or without the stock rule.
  expect_equal(be$n_new_5y, 1L)

  # The window length and the families come from the config; an empty list switches it off.
  cfg <- nipo_fx_cfg()
  cfg$nipo$one_off_stock_years <- 20
  out20 <- nipo_stage_policy_outputs(raw, nipo_fx_master(), nipo_fx_posture(), cfg)
  expect_true(all(out20$measures$active_at_as_of[out20$measures$intervention_id %in% c("40", "42")]))
  cfg$nipo$one_off_stock_years <- 5
  cfg$nipo$one_off_families <- list("public_finance")
  out_pf <- nipo_stage_policy_outputs(raw, nipo_fx_master(), nipo_fx_posture(), cfg)
  expect_true(out_pf$measures$active_at_as_of[out_pf$measures$intervention_id == "40"]) # grants now literal
  expect_false(out_pf$measures$active_at_as_of[out_pf$measures$intervention_id == "42"])
  cfg$nipo$one_off_families <- list()
  out_off <- nipo_stage_policy_outputs(raw, nipo_fx_master(), nipo_fx_posture(), cfg)
  expect_true(all(out_off$measures$stock_rule == "until_removed"))
  expect_equal(nipo_fx_cell(out_off, "USA", "Batteries", "Extraction")$n_measures_stock, 5L)
  expect_equal(nipo_fx_cell(out_off, "USA", "Batteries", "Extraction")$n_new_5y, 1L)

  # Configured families the posture table lacks are reported, not silently ignored.
  cfg$nipo$one_off_families <- list("grant", "grants")
  expect_equal(nipo_stage_policy_outputs(raw, nipo_fx_master(), nipo_fx_posture(), cfg)$settings$one_off_families_unknown, "grants")
  cfg$nipo$one_off_stock_years <- 2.5
  expect_error(nipo_stage_policy_outputs(raw, nipo_fx_master(), nipo_fx_posture(), cfg), "whole number")

  # The primitive keeps the literal rule unless asked.
  old <- as.Date("2012-05-01")
  expect_true(nipo_stage_active(old, as.Date(NA), nipo_fx_as_of))
  expect_false(nipo_stage_active(old, as.Date(NA), nipo_fx_as_of, one_off = TRUE, one_off_years = 5))
  expect_true(nipo_stage_active(old, as.Date(NA), nipo_fx_as_of, one_off = FALSE, one_off_years = 5))
})

test_that("the committed one-off families exist in the committed posture table", {
  root <- nipo_stage_repo_root()
  posture <- utils::read.csv(file.path(root, "config", "nipo_instrument_posture.csv"), stringsAsFactors = FALSE)
  cfg <- yaml::read_yaml(file.path(root, "config", "strategic_position.yml"))$policy_priority$nipo
  fams <- as.character(unlist(if (is.null(cfg$one_off_families)) NIPO_ONE_OFF_FAMILIES else cfg$one_off_families))
  expect_true(all(fams %in% posture$family), info = paste(setdiff(fams, posture$family), collapse = ", "))
  expect_true(all(NIPO_ONE_OFF_FAMILIES %in% posture$family))
})

test_that("the count clause of the narrow rule needs a minimum basket share", {
  master <- nipo_stage_prepare_master(nipo_fx_master())
  # 15 codes, one in the basket (6.7%): broad under the 10% floor; 8 codes with one in the basket
  # (12.5%): narrow; 25 codes with 13 in the basket would pass the share clause anyway.
  one_in_15 <- paste0("854142,", nipo_fx_filler(14))
  one_in_8 <- paste0("854142,", nipo_fx_filler(7))
  m <- tibble::tibble(intervention_id = c("a", "b"), iso3 = "USA", hs6_list = c(one_in_15, one_in_8))
  out <- nipo_stage_map_measures(m, master, narrow_max_codes = 20, narrow_min_basket_share = 0.5,
                                 narrow_count_min_basket_share = 0.1)
  st <- stats::setNames(out$measures$mapping_status, out$measures$intervention_id)
  expect_equal(st[["a"]], "tech_wide")
  expect_equal(st[["b"]], "attributed")
  expect_false("a" %in% out$links$intervention_id)
  # Exactly at the floor counts (one basket code in ten).
  at_floor <- nipo_stage_map_measures(
    tibble::tibble(intervention_id = "c", iso3 = "USA", hs6_list = paste0("854142,", nipo_fx_filler(9))),
    master, narrow_count_min_basket_share = 0.1
  )
  expect_equal(at_floor$measures$mapping_status, "attributed")
  # Floor 0 is the count-only rule.
  out0 <- nipo_stage_map_measures(m, master, narrow_count_min_basket_share = 0)
  expect_true(all(out0$measures$mapping_status == "attributed"))
  # The share clause stands alone: 16 codes (over narrow_max_codes = 5), 6 of them in the basket
  # (0.375 >= 0.3), attributed without the count clause.
  wide <- tibble::tibble(
    intervention_id = "d", iso3 = "USA",
    hs6_list = paste(c("854142", "854143", "850231", "870380", "282520", "253090", nipo_fx_filler(10)), collapse = ",")
  )
  dw <- nipo_stage_map_measures(wide, master, narrow_max_codes = 5, narrow_min_basket_share = 0.3)
  expect_equal(dw$measures$n_codes, 16L)
  expect_equal(dw$measures$mapping_status, "attributed")

  # Through the wrapper, the config key is read: a 15-code measure with one basket code drops out
  # of the cells.
  us <- "United States of America"
  raw <- dplyr::bind_rows(nipo_fx_raw(), nipo_fx_row(50, us, "Financial grant", one_in_15, "2025-05-01"))
  out_cfg <- nipo_stage_policy_outputs(raw, nipo_fx_master(), nipo_fx_posture(), nipo_fx_cfg())
  expect_equal(nipo_fx_measure(out_cfg, "50")$mapping_status, "tech_wide")
  expect_equal(out_cfg$settings$narrow_count_min_basket_share, 0.1)
  cfg <- nipo_fx_cfg()
  cfg$nipo$narrow_count_min_basket_share <- 0
  out_lo <- nipo_stage_policy_outputs(raw, nipo_fx_master(), nipo_fx_posture(), cfg)
  expect_equal(nipo_fx_measure(out_lo, "50")$mapping_status, "attributed")
  expect_equal(
    nipo_fx_cell(out_lo, "USA", "Solar", "Components")$all_stock -
      nipo_fx_cell(out_cfg, "USA", "Solar", "Components")$all_stock,
    1
  )
})

test_that("assessment equivalents, the EU-act threshold and the value split come from the config", {
  cfg <- nipo_fx_cfg()
  # Liberalising re-mapped to Amber: measure 9 is then counted.
  cfg$nipo$assessment_equivalents <- list(Distortive = list("Red", "Amber"), Liberalising = list("Amber"))
  out <- nipo_stage_policy_outputs(nipo_fx_raw(), nipo_fx_master(), nipo_fx_posture(), cfg)
  expect_true(nipo_fx_measure(out, "9")$counted)
  # An explicit argument wins over the config.
  out_arg <- nipo_stage_policy_outputs(nipo_fx_raw(), nipo_fx_master(), nipo_fx_posture(), cfg,
                                       assessment_equivalents = NIPO_ASSESSMENT_EQUIVALENTS)
  expect_false(nipo_fx_measure(out_arg, "9")$counted)

  # Two members recorded at National level: an EU act only when eu_wide_min_members <= 2.
  raw <- dplyr::bind_rows(
    nipo_fx_raw(),
    nipo_fx_row(31, "Germany", "State loan", "253090", "2025-04-01", subsidy = 90),
    nipo_fx_row(31, "France", "State loan", "253090", "2025-04-01", subsidy = 90)
  )
  cfg2 <- nipo_fx_cfg()
  cfg2$nipo$eu_wide_min_members <- 2
  out2 <- nipo_stage_policy_outputs(raw, nipo_fx_master(), nipo_fx_posture(), cfg2)
  expect_equal(nipo_fx_measure(out2, "31")$iso3, "EUU")
  cfg2$nipo$eu_wide_min_members <- NULL
  m2 <- nipo_stage_policy_outputs(raw, nipo_fx_master(), nipo_fx_posture(), cfg2)$measures
  expect_setequal(m2$iso3[grepl("^31", m2$intervention_id)], c("DEU", "FRA"))

  # split_repeated_values: false keeps the repeated figure on each record.
  raw3 <- dplyr::bind_rows(
    nipo_fx_raw(),
    nipo_fx_row(32, "Japan", "State loan", "253090", "2025-04-01", subsidy = 30, act = 900),
    nipo_fx_row(33, "Japan", "Financial grant", "253090", "2025-04-01", subsidy = 30, act = 900)
  )
  cfg3 <- nipo_fx_cfg()
  cfg3$nipo$split_repeated_values <- FALSE
  m3 <- nipo_stage_policy_outputs(raw3, nipo_fx_master(), nipo_fx_posture(), cfg3)$measures
  expect_equal(m3$subsidy_usd[m3$intervention_id %in% c("32", "33")], c(30e6, 30e6))
  m3s <- nipo_stage_policy_outputs(raw3, nipo_fx_master(), nipo_fx_posture(), nipo_fx_cfg())$measures
  expect_equal(m3s$subsidy_usd[m3s$intervention_id %in% c("32", "33")], c(15e6, 15e6))
})

test_that("GTA ids keep their digits", {
  expect_equal(nipo_stage_id_chr(c(100000, 160026, NA)), c("100000", "160026", NA))
})
