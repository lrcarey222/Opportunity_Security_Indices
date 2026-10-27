# Bilateral trade of the partnership themes (R/themes/partnership_strength/partnership_strength_helpers.R):
# one reconciled exporter -> importer flow per (tech, stage) cell and year, the importer's
# declaration first and the exporter's as a mirror (on a CIF basis), recorded zeros, and the
# two-stage trade index built on it.

ptf_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(ptf_root, "R", "utils", "scurve.R"))
source(file.path(ptf_root, "R", "utils", "country.R"))
source(file.path(ptf_root, "R", "utils", "schema.R"))
source(file.path(ptf_root, "R", "utils", "supply_chain_stages.R"))
source(file.path(ptf_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(ptf_root, "R", "themes", "partnership_strength", "prosperous_opportunity.R"))
source(file.path(ptf_root, "R", "themes", "partnership_strength", "safer_friendshore.R"))

`%>%` <- dplyr::`%>%`

# USA reports imports in both cells it trades; JPN reports Solar imports in 2024 only; CHN reports
# exports only; IND reports nothing; VNM appears only as a partner. Solar Assembly = 854140,
# Wind Assembly = 850231 (their stages in the HS6 master).
ptf_comtrade <- function() {
  row <- function(reporter, partner, flow, period, code, value) {
    tibble::tibble(
      reporter_iso = reporter, partner_iso = partner, flow_direction = flow,
      period = period, cmd_code = code, primary_value = value
    )
  }
  dplyr::bind_rows(
    row("USA", "CHN", "import", 2020, "854140", 100),
    row("USA", "CHN", "import", 2024, "854140", 200),
    row("USA", "JPN", "import", 2024, "854140", 50),
    row("USA", "IND", "import", 2024, "850231", 7),
    # A re-import: dropped.
    row("USA", "USA", "import", 2024, "854140", 999),
    row("JPN", "CHN", "import", 2024, "854140", 80),
    # CHN's exports: to JPN and USA in 2024 the importers' own reports win; to IND and VNM they
    # are the only declarations (mirror).
    row("CHN", "JPN", "export", 2024, "854140", 70),
    row("CHN", "USA", "export", 2024, "854140", 190),
    row("CHN", "IND", "export", 2020, "854140", 10),
    row("CHN", "IND", "export", 2024, "854140", 30),
    row("CHN", "VNM", "export", 2024, "850231", 5),
    # Outside the theme years: ignored.
    row("USA", "CHN", "import", 2022, "854140", 5000)
  )
}

ptf_subcat <- function() {
  tibble::tibble(
    tech = c("Solar", "Wind", "Solar"),
    # The crosswalk as scripts/15 passes it: each code's six-stage `stage` in supply_chain.
    supply_chain = c("Assembly", "Assembly", "Assembly"),
    # A duplicated crosswalk row counts once.
    hs6 = c("854140", "850231", "854140")
  )
}

ptf_cell <- function(cells, exporter, importer, tech) {
  out <- cells[cells$exporter_iso == exporter & cells$importer_iso == importer & cells$tech == tech, ]
  if (nrow(out) == 0) return(NULL)
  stopifnot(nrow(out) == 1)
  out
}

test_that("trade cells take the importer's declaration first and the exporter's as a CIF-basis mirror", {
  cells <- partnership_strength_build_trade_cells(ptf_comtrade(), ptf_subcat(), years = 2020:2024)

  expect_false(anyDuplicated(cells[c("exporter_iso", "importer_iso", "tech", "supply_chain")]) > 0)
  expect_false(any(cells$exporter_iso == cells$importer_iso))

  # USA reports Solar imports in both years: its own figures, 2022 ignored.
  us_cn <- ptf_cell(cells, "CHN", "USA", "Solar")
  expect_equal(c(us_cn$value_first, us_cn$value_last), c(100, 200))
  expect_equal(c(us_cn$source_first, us_cn$source_last), c("importer", "importer"))
  # CHN's own 190 is not used: the importer reports the cell.
  expect_false(any(cells$value_last == 190 * 1.06, na.rm = TRUE))

  # JPN reports in 2024 only. Its 2024 report (80) wins over CHN's 70; in 2020 CHN reports Solar
  # exports (to IND) but none to JPN: a recorded zero from the mirror side.
  jp_cn <- ptf_cell(cells, "CHN", "JPN", "Solar")
  expect_equal(jp_cn$value_last, 80)
  expect_equal(jp_cn$source_last, "importer")
  expect_equal(jp_cn$value_first, 0)
  expect_equal(jp_cn$source_first, "mirror")

  # IND reports nothing: CHN's exports, times the CIF/FOB factor.
  in_cn <- ptf_cell(cells, "CHN", "IND", "Solar")
  expect_equal(c(in_cn$value_first, in_cn$value_last), c(10, 30) * PARTNERSHIP_STRENGTH_CIF_FOB_FACTOR)
  expect_equal(in_cn$source_last, "mirror")
})

test_that("a reporting importer's missing partners are recorded zeros; pairs nobody reports are unobserved", {
  cells <- partnership_strength_build_trade_cells(ptf_comtrade(), ptf_subcat(), years = 2020:2024)

  # USA records no Solar imports from IND or VNM in either year.
  for (e in c("IND", "VNM")) {
    z <- ptf_cell(cells, e, "USA", "Solar")
    expect_equal(c(z$value_first, z$value_last), c(0, 0), info = e)
  }
  # JPN reports Solar in 2024 only and IND files no exports: 2024 is a zero, 2020 unobserved.
  z <- ptf_cell(cells, "IND", "JPN", "Solar")
  expect_equal(z$value_last, 0)
  expect_true(is.na(z$value_first))
  # Nobody reports USA -> IND Solar (IND files nothing, USA no Solar exports): no row at all.
  expect_null(ptf_cell(cells, "USA", "IND", "Solar"))

  # Reporting is judged per cell: USA reports Wind imports in 2024 only, so its Wind 2020 is
  # unobserved rather than a zero, and IND -> USA has no growth.
  w <- ptf_cell(cells, "IND", "USA", "Wind")
  expect_equal(w$value_last, 7)
  expect_true(is.na(w$value_first))
  expect_equal(ptf_cell(cells, "CHN", "USA", "Wind")$value_last, 0)
  expect_equal(ptf_cell(cells, "CHN", "VNM", "Wind")$value_last, 5 * PARTNERSHIP_STRENGTH_CIF_FOB_FACTOR)
})

test_that("the CIF/FOB factor and the mirror switch are honoured", {
  plain <- partnership_strength_build_trade_cells(ptf_comtrade(), ptf_subcat(), cif_fob_factor = 1)
  expect_equal(ptf_cell(plain, "CHN", "IND", "Solar")$value_last, 30)

  own_only <- partnership_strength_build_trade_cells(ptf_comtrade(), ptf_subcat(), mirror = FALSE)
  expect_null(ptf_cell(own_only, "CHN", "IND", "Solar"))
  expect_true(all(c(own_only$source_first, own_only$source_last) %in% c("importer", NA)))

  expect_error(partnership_strength_build_trade_cells(ptf_comtrade(), ptf_subcat(), cif_fob_factor = 0), "positive")
})

test_that("trade cells key on the crosswalk's supply_chain; scripts/15 and 30 copy the six-stage stage into it", {
  # The master's layout: the three-stage supply_chain next to the six-stage stage.
  master <- tibble::tibble(
    tech = c("Solar", "Solar", "Batteries", "Batteries"),
    supply_chain = c("Midstream", "Midstream", "Upstream", "Midstream"),
    hs6 = c("854140", "381800", "282520", "850760"),
    stage = c("Assembly", "Refining", "", "Assembly")
  )
  # Readers not yet on six stages (the Sector export, scripts/87; the paper figures) pass the
  # master as it is and keep the three-stage cells.
  expect_equal(partnership_strength_build_trade_mapping(master)$supply_chain, master$supply_chain)
  cells <- partnership_strength_build_trade_cells(ptf_comtrade(), dplyr::mutate(ptf_subcat(), supply_chain = "Midstream"))
  expect_equal(unique(cells$supply_chain), "Midstream")
  # The six-stage readers copy `stage` in first; a blank stage is NA (the trade builder drops it;
  # scripts/15 stops on it).
  six <- dplyr::mutate(master, supply_chain = stage)
  expect_equal(partnership_strength_build_trade_mapping(six)$supply_chain, c("Assembly", "Refining", NA, "Assembly"))

  # The master scripts/04 stages: every in-basket code has one of the six stages, and no code is
  # Deployment, so every Deployment cell is no_data (as every Downstream cell was).
  hs6 <- utils::read.csv(file.path(ptf_root, "data", "reference", "energy_hs6_master.csv"), stringsAsFactors = FALSE)
  basket <- hs6[!as.logical(hs6$excluded), setdiff(names(hs6), "excluded"), drop = FALSE]
  basket$supply_chain <- basket$stage
  basket_map <- partnership_strength_build_trade_mapping(basket)
  expect_silent(assert_supply_chain_stages(basket_map$supply_chain, "HS6 master stage"))
  expect_false("Deployment" %in% basket_map$supply_chain)

  # scripts/15 and scripts/30 check the staged copy and copy its stage in before they build the cells.
  s15 <- paste(readLines(file.path(ptf_root, "scripts", "15_build_partner_themes.R"), warn = FALSE), collapse = "\n")
  expect_match(s15, "subcat$supply_chain <- subcat$stage", fixed = TRUE)
  expect_match(s15, "partnership_strength_build_trade_mapping(subcat)$supply_chain", fixed = TRUE)
  expect_match(s15, "has no stage column", fixed = TRUE)
  s30 <- paste(readLines(file.path(ptf_root, "scripts", "30_build_allied_network_design.R"), warn = FALSE), collapse = "\n")
  expect_match(s30, "hs6_master$supply_chain <- hs6_master$stage", fixed = TRUE)
})

test_that("trade views orient the matrix for each theme and keep only observed last-year flows", {
  cells <- partnership_strength_build_trade_cells(ptf_comtrade(), ptf_subcat(), years = 2020:2024)
  imp <- partnership_strength_trade_view(cells, "import")
  exp <- partnership_strength_trade_view(cells, "export")

  # Friendshore: reporter = importer.
  r <- imp[imp$reporter_iso == "JPN" & imp$partner_iso == "CHN" & imp$tech == "Solar", ]
  expect_equal(r$level_last, 80)
  # First-year zero: no growth.
  expect_true(is.na(r$growth))
  r <- imp[imp$reporter_iso == "USA" & imp$partner_iso == "CHN" & imp$tech == "Solar", ]
  expect_equal(r$growth, 1)
  # Opportunity: reporter = exporter, same values.
  r <- exp[exp$reporter_iso == "CHN" & exp$partner_iso == "IND" & exp$tech == "Solar", ]
  expect_equal(r$level_last, 30 * PARTNERSHIP_STRENGTH_CIF_FOB_FACTOR)
  expect_equal(r$growth, 2)
  expect_false(any(is.na(imp$level_last)))
  expect_equal(nrow(imp), sum(!is.na(cells$value_last)))
})

test_that("the two-stage index scores dyads without first-year trade and ranks recorded zeros below every trading partner", {
  ds <- tibble::tibble(
    reporter_iso = c(rep("USA", 6), "JPN", "JPN"),
    partner_iso = c("A", "B", "C", "D", "E", "F", "A", "B"),
    tech = "Solar",
    supply_chain = "Extraction",
    # A grows, B has no first-year trade (no growth: the neutral growth score), C and D are
    # recorded zeros, E stopped trading, F declined.
    level_last = c(100, 10, 0, 0, 0, 40, 30, 60),
    level_first = c(50, 0, 0, NA, 20, 80, 10, 20)
  ) %>%
    dplyr::mutate(growth = dplyr::if_else(!is.na(level_first) & level_first > 0, level_last / level_first - 1, NA_real_))

  out <- partnership_strength_build_import_indices(ds)
  got <- stats::setNames(out$imp_trade_index[out$reporter_iso == "USA"], out$partner_iso[out$reporter_iso == "USA"])
  status <- stats::setNames(out$trade_status[out$reporter_iso == "USA"], out$partner_iso[out$reporter_iso == "USA"])

  # B used to lose its score (NA numerator) and be imputed; it is scored now.
  expect_false(anyNA(got))
  expect_equal(unname(status[c("A", "B", "E", "F")]), rep("trade", 4))
  expect_equal(unname(status[c("C", "D")]), c("zero", "zero"))
  # The zeros are one tie block below every trading partner, at the block's mid-rank.
  expect_equal(got[["C"]], got[["D"]])
  expect_lt(got[["C"]], min(got[c("A", "B", "E", "F")]))
  expect_equal(got[["C"]], median_scurve(c(0, 0, 1, 2, 3, 4))[1])
  expect_equal(max(got), 1)

  # Stage 1 ranks only trading dyads, so adding recorded zeros does not move the trading
  # partners' order.
  trading_only <- partnership_strength_build_export_indices(ds[!ds$partner_iso %in% c("C", "D"), ])
  with_zeros <- partnership_strength_build_export_indices(ds)
  key <- function(t) paste(t$reporter_iso, t$partner_iso)
  m <- match(key(trading_only), key(with_zeros))
  expect_equal(
    rank(trading_only$trade_index[trading_only$reporter_iso == "USA"]),
    rank(with_zeros$trade_index[m][trading_only$reporter_iso == "USA"])
  )
})

# One (tech, stage) in USD: USA exports to MEX (an established market with modest growth), KAZ (a
# USD 5k first-year base, the 2024 Kazakhstan case) and NEW (no first-year trade), beside 20 other
# exporters' dyads with bases above USD 1m and growth from -30% to +90%.
ptf_growth_cell <- function() {
  bg_last <- exp(seq(log(2e6), log(4e7), length.out = 20))
  bg_growth <- seq(-0.3, 0.9, length.out = 20)
  tibble::tibble(
    reporter_iso = c("USA", "USA", "USA", sprintf("R%02d", 1:20)),
    partner_iso = c("MEX", "KAZ", "NEW", rep("P", 20)),
    tech = "Electric Grid",
    supply_chain = "Assembly",
    level_last = c(2.1e9, 4.2e7, 9.6e7, bg_last),
    level_first = c(2.1e9 / 1.1, 5e3, 0, bg_last / (1 + bg_growth))
  ) %>%
    dplyr::mutate(growth = dplyr::if_else(!is.na(level_first) & level_first > 0, level_last / level_first - 1, NA_real_))
}

ptf_usa <- function(out) {
  u <- out[out$reporter_iso == "USA", ]
  stats::setNames(u$trade_index, u$partner_iso)
}

test_that("growth is scored only from a minimum first-year base, so a tiny or missing base cannot top the reporter", {
  ds <- ptf_growth_cell()
  legacy <- list(min_base_usd = 0, unscored = "level_only")

  # The rule before 2026-10-20: the USD 5k base is the fastest growth of the cell and NEW is
  # scored on its level alone, so both rank above MEX, 22 times KAZ's 2024 flow.
  old <- ptf_usa(partnership_strength_build_export_indices(ds, trade_growth = legacy))
  expect_equal(names(old)[which.max(old)], "KAZ")
  expect_lt(old[["MEX"]], min(old[c("KAZ", "NEW")]))

  # Default: growth is scored from USD 1m; KAZ and NEW take the neutral growth score.
  new <- ptf_usa(partnership_strength_build_export_indices(ds))
  expect_equal(names(new)[which.max(new)], "MEX")
  expect_gt(new[["MEX"]], max(new[c("KAZ", "NEW")]))
  # Unscored growth is neutral, whatever the multiple: any base below the floor gives the same index.
  ds_bigger_base <- ds
  ds_bigger_base$level_first[ds$partner_iso == "KAZ"] <- 9e5
  ds_bigger_base$growth[ds$partner_iso == "KAZ"] <- 4.2e7 / 9e5 - 1
  expect_equal(ptf_usa(partnership_strength_build_export_indices(ds_bigger_base)), new)
  # ... and a tiny base scores as no first-year trade at the same level.
  ds_same_level <- ds
  ds_same_level$level_last[ds$partner_iso == "NEW"] <- 4.2e7
  same <- ptf_usa(partnership_strength_build_export_indices(ds_same_level))
  expect_equal(same[["KAZ"]], same[["NEW"]])

  # The floor is inclusive: at USD 5k KAZ's growth is scored again and KAZ tops the reporter.
  at_floor <- ptf_usa(partnership_strength_build_export_indices(ds, trade_growth = list(min_base_usd = 5e3, unscored = "neutral")))
  expect_equal(names(at_floor)[which.max(at_floor)], "KAZ")

  # The Friendshore view applies the same rule.
  imp <- partnership_strength_build_import_indices(ds)
  expect_equal(imp$imp_trade_index, partnership_strength_build_export_indices(ds)$trade_index)
})

test_that("growth_min_base_usd 0 with level_only reproduces the index before 2026-10-20", {
  ds <- ptf_growth_cell()
  # The earlier stage 1, written out: growth S-curved over every trading dyad with a positive
  # first-year flow; a dyad without growth scored on its level alone.
  old <- ds %>%
    dplyr::mutate(trading = level_last > 0 | (!is.na(level_first) & level_first > 0)) %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(
      level_idx = median_scurve(dplyr::if_else(trading, level_last, NA_real_)),
      growth_idx = median_scurve(dplyr::if_else(trading, growth, NA_real_)),
      combined = dplyr::if_else(is.na(growth_idx), level_idx, (2 * level_idx + growth_idx) / 3)
    ) %>%
    dplyr::group_by(reporter_iso, tech, supply_chain) %>%
    dplyr::mutate(trade_index = median_scurve(combined)) %>%
    dplyr::ungroup()
  got <- partnership_strength_build_export_indices(ds, trade_growth = list(min_base_usd = 0, unscored = "level_only"))
  expect_equal(got$trade_index, old$trade_index)
})

test_that("the trade growth settings read config/partnership.yml and reject bad values", {
  expect_equal(partnership_strength_trade_growth_settings(), list(min_base_usd = 1e6, unscored = "neutral"))
  expect_equal(
    partnership_strength_trade_growth_settings(list(source = "auto", growth_min_base_usd = "250000", growth_unscored = "Level_Only")),
    list(min_base_usd = 250000, unscored = "level_only")
  )
  expect_error(partnership_strength_trade_growth_settings(list(growth_min_base_usd = -1)), ">= 0")
  expect_error(partnership_strength_trade_growth_settings(list(growth_min_base_usd = "a lot")), ">= 0")
  expect_error(partnership_strength_trade_growth_settings(list(growth_unscored = "drop")), "neutral or level_only")
  expect_error(partnership_strength_build_export_indices(ptf_growth_cell(), trade_growth = list(min_base_usd = NA, unscored = "neutral")), ">= 0")

  cfg <- yaml::read_yaml(file.path(ptf_root, "config", "partnership.yml"))
  expect_equal(partnership_strength_trade_growth_settings(cfg$trade), list(min_base_usd = 1e6, unscored = "neutral"))
})

# Dyad cells of a reporter with cells that have trade data and cells that do not (no_data).
ptf_no_data_cells <- function() {
  tibble::tribble(
    ~reporter_iso, ~partner_iso, ~tech, ~supply_chain, ~trade_index, ~trade_status,
    "USA", "MEX", "Solar", "Refining", 0.9, "trade",
    "USA", "MEX", "Solar", "Assembly", 0.7, "trade",
    "USA", "MEX", "Solar", "Deployment", 0.5, "no_data",
    "USA", "MEX", "Wind", "Assembly", 0.3, "zero",
    "USA", "MEX", "Green Hydrogen", "Deployment", 0.5, "no_data",
    # STP: Solar is imputed at the cell mean (no information on the dyad); Wind a recorded zero.
    "USA", "STP", "Solar", "Assembly", 0.6, "imputed",
    "USA", "STP", "Wind", "Assembly", 0.2, "zero",
    "USA", "STP", "Solar", "Deployment", 0.5, "no_data",
    # BDI: no observed cell at all.
    "USA", "BDI", "Solar", "Assembly", 0.6, "imputed",
    "USA", "BDI", "Solar", "Deployment", 0.5, "no_data"
  )
}

test_that("a cell without trade data takes the dyad's own trade index, the same technology first", {
  cells <- ptf_no_data_cells()
  out <- partnership_strength_fill_no_data_trade(cells, "trade_index")
  cell <- function(p, t, s) out[out$partner_iso == p & out$tech == t & out$supply_chain == s, ]

  # Same technology: the mean of MEX's observed Solar stages.
  expect_equal(cell("MEX", "Solar", "Deployment")$trade_index, mean(c(0.9, 0.7)))
  expect_equal(cell("MEX", "Solar", "Deployment")$trade_fill, "same_tech")
  # No observed Green Hydrogen cell: the mean over all of MEX's observed cells, zeros included.
  expect_equal(cell("MEX", "Green Hydrogen", "Deployment")$trade_index, mean(c(0.9, 0.7, 0.3)))
  expect_equal(cell("MEX", "Green Hydrogen", "Deployment")$trade_fill, "all_tech")
  # Imputed cells are not used: STP's Solar Deployment reads its recorded Wind zero.
  expect_equal(cell("STP", "Solar", "Deployment")$trade_index, 0.2)
  expect_equal(cell("STP", "Solar", "Deployment")$trade_fill, "all_tech")
  # A dyad without an observed cell keeps the constant.
  expect_equal(cell("BDI", "Solar", "Deployment")$trade_index, 0.5)
  expect_equal(cell("BDI", "Solar", "Deployment")$trade_fill, "constant")

  # Cells with trade data and every trade_status are untouched.
  has_data <- cells$trade_status != "no_data"
  expect_equal(out$trade_index[has_data], cells$trade_index[has_data])
  expect_true(all(is.na(out$trade_fill[has_data])))
  expect_equal(out$trade_status, cells$trade_status)

  # "constant" is the rule before 2026-10-20: 0.5 in every no_data cell.
  const <- partnership_strength_fill_no_data_trade(cells, "trade_index", fill = "constant")
  expect_equal(const$trade_index, cells$trade_index)
  expect_equal(const$trade_fill[!has_data], rep("constant", sum(!has_data)))
  # A table without no_data cells comes back unchanged (plus an all-NA trade_fill).
  plain <- partnership_strength_fill_no_data_trade(cells[has_data, ], "trade_index")
  expect_equal(plain$trade_index, cells$trade_index[has_data])
  expect_true(all(is.na(plain$trade_fill)))
})

test_that("the no_data fill setting reads config/partnership.yml and rejects bad values", {
  expect_equal(partnership_strength_no_data_fill_setting(), "dyad")
  expect_equal(partnership_strength_no_data_fill_setting(list(source = "auto", no_data_fill = "Constant")), "constant")
  expect_error(partnership_strength_no_data_fill_setting(list(no_data_fill = "mean")), "no_data_fill must be one of dyad, constant")
  expect_error(partnership_strength_fill_no_data_trade(ptf_no_data_cells(), "trade_index", fill = "zero"), "no_data_fill")

  cfg <- yaml::read_yaml(file.path(ptf_root, "config", "partnership.yml"))
  expect_equal(partnership_strength_no_data_fill_setting(cfg$trade), "dyad")
})

test_that("a market the reporter barely trades with no longer tops a cell without trade data on its need alone", {
  ci <- tibble::tibble(
    country = c("United States", "Mexico", "Sao Tome and Principe", "Japan"),
    iso3c = c("USA", "MEX", "STP", "JPN"),
    income = c("High income", "Upper middle income", "Lower middle income", "High income")
  )
  grid <- function(col, values) {
    tbl <- tidyr::expand_grid(Country = ci$country, tech = c("Solar", "Wind"), supply_chain = c("Assembly", "Deployment"))
    tbl[[col]] <- rep(values, each = 4)
    tbl
  }
  eo <- grid("Economic_Opportunity_Index", c(0.9, 0.5, 0.1, 0.8))
  # Sao Tome is the least secure market (the highest need), Mexico a middling one.
  es <- grid("Energy_Security_Index", c(0.95, 0.6, 0.1, 0.9))
  # US exports: large to Mexico, recorded zeros to Sao Tome, Solar only to Japan. No HS6 code maps
  # to Deployment, so every Deployment cell is no_data.
  trade <- tibble::tribble(
    ~reporter_iso, ~partner_iso, ~tech, ~supply_chain, ~trade_index, ~trade_status,
    "USA", "MEX", "Solar", "Assembly", 1.0, "trade",
    "USA", "MEX", "Wind", "Assembly", 0.9, "trade",
    "USA", "STP", "Solar", "Assembly", 0.2, "zero",
    "USA", "STP", "Wind", "Assembly", 0.2, "zero",
    "USA", "JPN", "Solar", "Assembly", 0.6, "trade"
  )
  op_with <- function(no_data_fill) {
    partnership_strength_build_opportunity_dyads(
      trade, eo, es,
      tibble::tibble(tech = c("Solar", "Wind"), ghg_index = 1),
      tibble::tibble(Country = "Japan", climate_policy_index = 0.5),
      ci,
      component_weights = list(trade_index = 3, econ_opp_index = 3, partner_es_need = 1, climate_policy_weight = 0),
      no_data_fill = no_data_fill
    )
  }
  cell <- function(op, r, p, t, s) op[op$reporter_iso == r & op$partner_iso == p & op$tech == t & op$supply_chain == s, ]
  old <- op_with("constant")
  new <- op_with("dyad")

  # Before: the Deployment trade term is 0.5 for every market, so Sao Tome's need puts it first.
  expect_true(all(old$trade_index[old$supply_chain == "Deployment"] == 0.5))
  expect_gt(cell(old, "USA", "STP", "Solar", "Deployment")$opportunity_index, cell(old, "USA", "MEX", "Solar", "Deployment")$opportunity_index)

  # Now: each market's Deployment trade term is its own Assembly trade in the technology.
  expect_equal(cell(new, "USA", "MEX", "Solar", "Deployment")$trade_index, 1.0)
  expect_equal(cell(new, "USA", "STP", "Solar", "Deployment")$trade_index, 0.2)
  expect_equal(cell(new, "USA", "MEX", "Solar", "Deployment")$trade_fill, "same_tech")
  expect_gt(cell(new, "USA", "MEX", "Solar", "Deployment")$opportunity_index, cell(new, "USA", "STP", "Solar", "Deployment")$opportunity_index)
  # Japan has no observed Wind cell with the US: its Wind Deployment reads its Solar trade.
  expect_equal(cell(new, "USA", "JPN", "Wind", "Deployment")$trade_index, 0.6)
  expect_equal(cell(new, "USA", "JPN", "Wind", "Deployment")$trade_fill, "all_tech")
  # A reporter with no observed export cell keeps the constant.
  expect_true(all(new$trade_index[new$reporter_iso == "JPN" & new$supply_chain == "Deployment"] == 0.5))
  expect_true(all(new$trade_fill[new$reporter_iso == "JPN" & new$supply_chain == "Deployment"] == "constant"))

  # The status, the cells with trade data and the imputed weight share do not change.
  expect_equal(new$trade_status, old$trade_status)
  expect_true(all(new$trade_status[new$supply_chain == "Deployment"] == "no_data"))
  mid <- new$supply_chain == "Assembly"
  expect_equal(new$opportunity_index[mid], old$opportunity_index[mid])
  expect_equal(new$imputed_weight_share, old$imputed_weight_share)
})

test_that("the theme builders report trade coverage: observed trade, recorded zeros and imputed cells", {
  ci <- tibble::tibble(
    country = c("United States", "Japan", "China", "India", "Viet Nam"),
    iso3c = c("USA", "JPN", "CHN", "IND", "VNM"),
    income = c("High income", "High income", "Upper middle income", "Lower middle income", "Lower middle income")
  )
  idx <- function(col, values) {
    tbl <- tidyr::expand_grid(
      Country = ci$country,
      tech = c("Solar", "Wind"),
      supply_chain = c("Assembly", "Deployment")
    )
    tbl[[col]] <- values
    tbl
  }
  es <- idx("Energy_Security_Index", seq(0.05, 1, length.out = 20))
  eo <- idx("Economic_Opportunity_Index", rev(seq(0.05, 1, length.out = 20)))
  cells <- partnership_strength_build_trade_cells(ptf_comtrade(), ptf_subcat(), years = 2020:2024)

  fs_with <- function(no_data_fill) {
    safer_friendshore(
      NULL, ptf_subcat(), eo, es, tibble::tibble(tech = c("Solar", "Wind"), ghg_index = 1),
      tibble::tibble(Country = "Japan", climate_policy_index = 0.5),
      tibble::tibble(
        COUNTRY = character(), COUNTERPART_COUNTRY = character(), DV_TYPE = character(),
        INDICATOR = character(), X2022 = numeric()
      ),
      ci,
      tibble::tibble(iso3c = ci$iso3c, year = 2023, NY.GDP.MKTP.CD = c(27, 4, 18, 3.5, 0.4) * 1e12),
      trade_cells = cells,
      no_data_fill = no_data_fill
    )
  }
  fs <- fs_with("dyad")
  cov <- fs$trade_coverage
  expect_setequal(
    names(cov),
    c(
      "theme", "tech", "supply_chain", "n_cells", "n_trade", "n_zero", "n_imputed", "n_no_data",
      "n_no_data_same_tech", "n_no_data_all_tech", "n_no_data_constant", "share_imputed"
    )
  )
  expect_equal(cov$n_cells, cov$n_trade + cov$n_zero + cov$n_imputed + cov$n_no_data)
  expect_equal(cov$n_no_data, cov$n_no_data_same_tech + cov$n_no_data_all_tech + cov$n_no_data_constant)

  # Solar Assembly, importer view (5 x 4 = 20 dyad cells): USA reports (CHN 200 and JPN 50;
  # IND and VNM zeros); JPN reports 2024 (CHN 80; USA, IND and VNM zeros); IND and VNM are seen
  # through CHN's export declarations (IND 30, VNM a zero); nobody reports CHN's imports or the
  # other exporters to IND and VNM.
  sd <- cov[cov$tech == "Solar" & cov$supply_chain == "Assembly", ]
  expect_equal(sd$theme, "friendshore")
  expect_equal(sd$n_trade, 2 + 1 + 1)
  expect_equal(sd$n_zero, 2 + 3 + 1)
  expect_equal(sd$n_imputed, 20 - 10)
  expect_equal(sd$n_no_data, 0)
  expect_equal(sd$share_imputed, 10 / 20)
  # A (tech, stage) no Comtrade code maps to has no trade data: counted as no_data rather than
  # imputed, and it has no imputed share. Its trade term is the dyad's own: the 10 dyads observed
  # in Solar Assembly take that cell's index (same technology).
  sm <- cov[cov$tech == "Solar" & cov$supply_chain == "Deployment", ]
  expect_equal(sm$n_no_data, 20)
  expect_equal(sm$n_imputed, 0)
  expect_true(is.na(sm$share_imputed))
  expect_equal(sm$n_no_data_same_tech, sd$n_trade + sd$n_zero)
  trade_rows <- function(out, stage) {
    v <- out$variable
    v <- v[v$tech == "Solar" & v$supply_chain == stage & v$variable == "imp_trade_index", ]
    stats::setNames(v$value, paste(v$reporter_iso, v$partner_iso))
  }
  fs_sm <- trade_rows(fs, "Deployment")
  fs_sd <- trade_rows(fs, "Assembly")
  expect_equal(fs_sm[["USA CHN"]], fs_sd[["USA CHN"]])
  expect_equal(fs_sm[["JPN VNM"]], fs_sd[["JPN VNM"]])
  expect_gt(fs_sm[["USA CHN"]], fs_sm[["USA VNM"]])

  # no_data_fill "constant" (the rule before 2026-10-20): every dyad gets 0.5.
  fs_const <- fs_with("constant")
  expect_true(all(trade_rows(fs_const, "Deployment") == 0.5))
  expect_equal(fs_const$trade_coverage$n_no_data_constant, fs_const$trade_coverage$n_no_data)
  # Cells with trade data are the same under both rules.
  expect_equal(trade_rows(fs_const, "Assembly"), fs_sd)
})

test_that("the trade source prefers the all-reporter pull and flags the allied fallback", {
  expect_equal(
    partnership_strength_resolve_trade_source("auto", TRUE, TRUE),
    list(source = "all_reporters", fallback = FALSE)
  )
  expect_equal(
    partnership_strength_resolve_trade_source(NULL, FALSE, TRUE),
    list(source = "allied", fallback = TRUE)
  )
  expect_equal(partnership_strength_resolve_trade_source("allied", TRUE, TRUE)$source, "allied")
  expect_error(partnership_strength_resolve_trade_source("all_reporters", FALSE, TRUE), "missing")
  expect_error(partnership_strength_resolve_trade_source("auto", FALSE, FALSE), "No Comtrade")
  expect_error(partnership_strength_resolve_trade_source("bogus", TRUE, TRUE), "auto, all_reporters or allied")

  cfg <- yaml::read_yaml(file.path(ptf_root, "config", "partnership.yml"))
  expect_equal(cfg$trade$source, "auto")
  expect_equal(cfg$trade$cif_fob_factor, PARTNERSHIP_STRENGTH_CIF_FOB_FACTOR)
})

test_that("scripts/15 reads the all-reporter pull through the source switch", {
  s15 <- paste(readLines(file.path(ptf_root, "scripts", "15_build_partner_themes.R"), warn = FALSE), collapse = "\n")
  expect_match(s15, "file.path(raw_data_path, \"comtrade_energy_dyads_all.csv\")", fixed = TRUE)
  expect_match(s15, "partnership_strength_resolve_trade_source(", fixed = TRUE)
  expect_match(s15, "trade_cells = trade_cells", fixed = TRUE)
})
