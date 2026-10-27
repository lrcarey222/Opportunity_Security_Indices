# CM-FDI (Cleantech Manufacturing FDI Dataset, Net Zero Industrial Policy Lab) in the Friendshore
# theme: the parser and index flags (R/themes/partnership_strength/cm_fdi.R), the investment ties
# that replace the IMF outbound measure in (Solar, Wind, Batteries, EV) x (Components, Assembly),
# and the chinese_parent_exposure component (R/themes/partnership_strength/safer_friendshore.R).

cmf_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(cmf_root, "R", "utils", "scurve.R"))
source(file.path(cmf_root, "R", "utils", "country.R"))
source(file.path(cmf_root, "R", "utils", "schema.R"))
source(file.path(cmf_root, "R", "utils", "supply_chain_stages.R"))
source(file.path(cmf_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(cmf_root, "R", "themes", "partnership_strength", "prosperous_opportunity.R"))
source(file.path(cmf_root, "R", "themes", "partnership_strength", "safer_friendshore.R"))
source(file.path(cmf_root, "R", "themes", "partnership_strength", "cm_fdi.R"))

`%>%` <- dplyr::`%>%`

# A raw CM-FDI table as read from the CSV (every column text), one row per spec.
cmf_raw <- function(...) {
  rows <- list(...)
  cols <- c(cm_fdi_columns(), cm_fdi_source_columns())
  defaults <- list(
    "Sector" = "Battery", "Parent company" = "Parent", "Company Country (HQ)" = "China",
    "Project Status" = "Operational", "Destination Country" = "Indonesia", "Destination city" = "City",
    "Year Announced" = "2022", "Year Completed" = "N/A", "Technology" = "Battery",
    "Industry activity" = "Manufacturing", "Capital Investment (US$m)" = "100", "Expansion" = "0",
    "Phase" = "", "Joint Venture" = "0", "Joint Venture Local" = "0", "Joint Venture Local State" = "",
    "Joint Venture Company" = "", "Operational" = "Operational"
  )
  out <- lapply(seq_along(rows), function(i) {
    r <- utils::modifyList(defaults, rows[[i]])
    r[["Project ID"]] <- if (is.null(rows[[i]][["Project ID"]])) paste0("P", i) else rows[[i]][["Project ID"]]
    vals <- vapply(unname(cols), function(cn) if (is.null(r[[cn]])) "https://example.org/src" else as.character(r[[cn]]), character(1))
    as.data.frame(as.list(stats::setNames(vals, unname(cols))), check.names = FALSE, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

test_that("the parser types the table, maps countries and technologies, and keeps undisclosed values missing", {
  raw <- cmf_raw(
    list("Company Country (HQ)" = "Hong Kong", "Destination Country" = "Vietnam", "Capital Investment (US$m)" = ""),
    list("Company Country (HQ)" = "Taiwan", "Destination Country" = "Czechia", "Technology" = "EV", "Year Completed" = "2025"),
    list("Company Country (HQ)" = "South Korea", "Destination Country" = "UAE", "Technology" = "Solar",
         "Capital Investment (US$m)" = "1,250.5", "Joint Venture" = "1", "Joint Venture Local" = "1"),
    list("Company Country (HQ)" = "Germany", "Destination Country" = "Czech Republic", "Technology" = "Wind",
         "Year Completed" = "")
  )
  # A byte-order mark on the first column name (a CSV read without fileEncoding = "UTF-8-BOM").
  names(raw)[1] <- paste0(intToUtf8(0xFEFF), names(raw)[1])
  p <- cm_fdi_clean_projects(raw)

  expect_equal(p$home_iso3, c("HKG", "TWN", "KOR", "DEU"))
  expect_equal(p$destination_iso3, c("VNM", "CZE", "ARE", "CZE"))
  expect_equal(p$tech, c("Batteries", "Electric Vehicles", "Solar", "Wind"))
  # Undisclosed is NA, never 0; a thousands separator is read.
  expect_true(is.na(p$capital_usd_m[1]))
  expect_false(p$capital_disclosed[1])
  expect_equal(p$capital_usd_m[3], 1250.5)
  expect_equal(p$year_completed, c(NA, 2025L, NA, NA))
  expect_identical(p$joint_venture, c(FALSE, FALSE, TRUE, FALSE))
  expect_true(is.na(p$jv_local_state[1]))
  # The source links are kept for the descriptive views.
  expect_true(all(c("source_parent", "source_amount", "source_jv") %in% names(p)))

  expect_error(cm_fdi_clean_projects(cmf_raw(list("Technology" = "Hydrogen"))), "Technology values")
  expect_error(cm_fdi_clean_projects(cmf_raw(list("Destination Country" = "Atlantis"))), "cannot map: Atlantis")
  expect_error(cm_fdi_clean_projects(cmf_raw(list("Capital Investment (US$m)" = "about 5"))), "not numbers")
  expect_error(cm_fdi_clean_projects(cmf_raw(list("Project ID" = "X"), list("Project ID" = "X"))), "not unique")
  expect_error(cm_fdi_clean_projects(cmf_raw(list())[, -3]), "lacks columns")
})

test_that("settings resolve from config, refuse bad values, and the shipped block is the documented one", {
  s <- cm_fdi_resolve_settings(NULL)
  expect_true(is.na(s$announced_from))
  expect_equal(s$jv_parent_share, 0.5)
  expect_equal(cm_fdi_resolve_settings(list(announced_from = NULL))$announced_from, NA_integer_)
  expect_equal(cm_fdi_resolve_settings(list(announced_from = 2015))$announced_from, 2015L)
  expect_error(cm_fdi_resolve_settings(list(jv_parent_share = 0)), "jv_parent_share")
  expect_error(cm_fdi_resolve_settings(list(exposure_weighting = "median")), "should be one of")
  expect_error(cm_fdi_resolve_settings(list(china_hq_iso3 = "HKG")), "include CHN")
  expect_error(cm_fdi_resolve_settings(list(windows = 1)), "Unknown cm_fdi settings")
  expect_error(cm_fdi_resolve_settings(list(min_projects = 0)), "min_projects")
  # Stages: six-stage names only; the three-stage `stage` key of before 2026-10-27 stops.
  expect_equal(cm_fdi_resolve_settings(NULL)$stages, SUPPLY_CHAIN_MANUFACTURING_STAGES)
  expect_equal(cm_fdi_resolve_settings(list(stages = list("Refining", "Components")))$stages, c("Refining", "Components"))
  expect_error(cm_fdi_resolve_settings(list(stage = "Midstream")), "replaced on 2026-10-27 by stages")
  expect_error(cm_fdi_resolve_settings(list(stages = list("Midstream"))), "cm_fdi stages must list distinct supply-chain stages")
  expect_error(cm_fdi_resolve_settings(list(stages = list("Components", "Components"))), "distinct")
  expect_error(cm_fdi_resolve_settings(list(stages = list())), "got none")

  cfg <- yaml::read_yaml(file.path(cmf_root, "config", "partnership.yml"))$cm_fdi
  shipped <- cm_fdi_resolve_settings(cfg)
  expect_true(shipped$investment_ties)
  expect_true(shipped$chinese_parent_exposure)
  expect_equal(unname(shipped$technologies[c("Battery", "Solar", "Wind", "EV")]), c("Batteries", "Solar", "Wind", "Electric Vehicles"))
  expect_equal(shipped$stages, c("Components", "Assembly"))
  expect_null(cfg[["stage"]])
  expect_setequal(shipped$keep_status, c("Announced", "Under Construction", "Completed", "Operational"))
  expect_true("Ceased operations" %in% shipped$drop_operational)
  expect_equal(shipped$manufacturing_activities, "Manufacturing")
  expect_true(is.na(shipped$announced_from))
  expect_equal(shipped$china_hq_iso3, c("CHN", "HKG", "MAC"))
  expect_equal(shipped$exposure_weighting, "blend")
  expect_equal(shipped$min_projects, 2L)
  # Every mapped technology is a headline technology of the partner themes.
  expect_true(all(shipped$technologies %in% partnership_strength_headline_techs()))

  # The weight sits next to geo_alignment in config/weights.yml.
  w <- yaml::read_yaml(file.path(cmf_root, "config", "weights.yml"))$partnership_friendshore_components
  expect_equal(w$chinese_parent_exposure, 1)
  expect_equal(w$geo_alignment, 2)
})

test_that("index flags drop cancelled, paused, closed, ceased and non-manufacturing projects and weight JVs", {
  raw <- cmf_raw(
    list(),
    list("Project Status" = "Cancelled"),
    list("Project Status" = "Paused", "Operational" = "Paused"),
    list("Project Status" = "Closed", "Operational" = "Ceased operations"),
    list("Project Status" = "Completed", "Operational" = "Ceased operations"),
    list("Industry activity" = "Research & Development"),
    list("Year Announced" = "2010"),
    list("Destination Country" = "China"),
    list("Joint Venture" = "1", "Joint Venture Local" = "1", "Company Country (HQ)" = "Japan")
  )
  p <- cm_fdi_clean_projects(raw)
  f <- cm_fdi_flag_index_use(p, cm_fdi_resolve_settings(NULL))
  expect_equal(f$index_use, c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE))
  expect_equal(f$index_exclusion[2], "status: Cancelled")
  expect_equal(f$index_exclusion[5], "operational: Ceased operations")
  expect_equal(f$index_exclusion[6], "activity: Research & Development")
  expect_equal(f$index_exclusion[8], "home economy = destination")
  expect_equal(f$attribution_weight, c(rep(1, 8), 0.5))
  expect_equal(f$china_parent, c(rep(TRUE, 8), FALSE))
  # One row per project: the stages it counts in, and their shared roll-up as supply_chain.
  expect_equal(nrow(f), nrow(p))
  expect_true(all(f$stages == "Components|Assembly"))
  expect_true(all(f$supply_chain == "Midstream"))
  # Stages that span roll-ups have no single one.
  span <- cm_fdi_flag_index_use(p, cm_fdi_resolve_settings(list(stages = list("Refining", "Components"))))
  expect_true(all(is.na(span$supply_chain)))

  # The time window.
  f15 <- cm_fdi_flag_index_use(p, cm_fdi_resolve_settings(list(announced_from = 2015)))
  expect_false(f15$index_use[7])
  expect_equal(f15$index_exclusion[7], "announced before 2015")
  # Hong Kong parents count as Chinese.
  hk <- cm_fdi_flag_index_use(cm_fdi_clean_projects(cmf_raw(list("Company Country (HQ)" = "Hong Kong"))))
  expect_true(hk$china_parent)
})

test_that("ties sum attributed counts and disclosed values, with undisclosed values left missing", {
  p <- cm_fdi_clean_projects(cmf_raw(
    list("Company Country (HQ)" = "Japan", "Destination Country" = "United States", "Capital Investment (US$m)" = "300"),
    list("Company Country (HQ)" = "Japan", "Destination Country" = "United States", "Capital Investment (US$m)" = "",
         "Joint Venture" = "1"),
    list("Company Country (HQ)" = "Japan", "Destination Country" = "Mexico", "Capital Investment (US$m)" = ""),
    list("Company Country (HQ)" = "Japan", "Destination Country" = "Mexico", "Project Status" = "Cancelled")
  ))
  ties <- cm_fdi_investment_ties(cm_fdi_flag_index_use(p))
  # Each tie in both manufacturing stages, at the full count and value (copied, not split).
  expect_equal(sort(unique(ties$supply_chain)), c("Assembly", "Components"))
  for (s in c("Components", "Assembly")) {
    usa <- ties[ties$destination_iso == "USA" & ties$supply_chain == s, ]
    expect_equal(usa$n_projects, 1.5)
    expect_equal(usa$n_disclosed, 1)
    expect_equal(usa$value_usd_m, 300)
    mex <- ties[ties$destination_iso == "MEX" & ties$supply_chain == s, ]
    expect_equal(mex$n_projects, 1)
    expect_true(is.na(mex$value_usd_m))
  }
  expect_equal(nrow(ties), 2 * 2)

  # No project used (every one cancelled), or every used plant in China: no ties and only the
  # China-headquarters rows of the exposure, as with three stages, not an error.
  china_hq <- cm_fdi_resolve_settings()$china_hq_iso3
  none <- cm_fdi_flag_index_use(p[p$status == "Cancelled", ])
  expect_equal(nrow(cm_fdi_investment_ties(none)), 0)
  expect_true(all(cm_fdi_chinese_parent_exposure(none)$destination_iso %in% china_hq))
  in_china <- cm_fdi_flag_index_use(dplyr::mutate(p, destination_iso3 = "CHN", destination_country = "China"))
  expect_true(all(cm_fdi_chinese_parent_exposure(in_china)$destination_iso %in% china_hq))
})

test_that("exposure blends disclosed values with undisclosed counts and needs min_projects projects", {
  # Destination IDN, Batteries: a Chinese plant of 300 and a Korean one of 100 (disclosed), a
  # Chinese plant without a value, and a Chinese JV of 200 (attributed half: 0.5 x 200).
  p <- cm_fdi_clean_projects(cmf_raw(
    list("Capital Investment (US$m)" = "300"),
    list("Company Country (HQ)" = "South Korea", "Capital Investment (US$m)" = "100"),
    list("Capital Investment (US$m)" = ""),
    list("Capital Investment (US$m)" = "200", "Joint Venture" = "1", "Joint Venture Local" = "1"),
    # Two non-Chinese plants in Mexico, no values: count basis.
    list("Company Country (HQ)" = "Japan", "Destination Country" = "Mexico", "Capital Investment (US$m)" = ""),
    list("Company Country (HQ)" = "United States", "Destination Country" = "Mexico", "Capital Investment (US$m)" = ""),
    # One plant in Hungary: below min_projects.
    list("Destination Country" = "Hungary")
  ))
  f <- cm_fdi_flag_index_use(p)
  s <- cm_fdi_resolve_settings(NULL)
  ex_all <- cm_fdi_chinese_parent_exposure(f, s)
  # The same exposure in Components and Assembly; the checks below read Assembly.
  comp <- ex_all[ex_all$supply_chain == "Components", ]
  asm <- ex_all[ex_all$supply_chain == "Assembly", ]
  expect_equal(nrow(comp) + nrow(asm), nrow(ex_all))
  expect_equal(comp[setdiff(names(comp), "supply_chain")], asm[setdiff(names(asm), "supply_chain")])
  ex <- asm
  idn <- ex[ex$destination_iso == "IDN", ]
  # A = 3.5 attributed projects, D = 2.5 disclosed; disclosed value 300 + 100 + 100, Chinese 400.
  expect_equal(idn$n_projects, 3.5)
  expect_equal(idn$share_value, 400 / 500)
  expect_equal(idn$chinese_parent_exposure, (1 + 2.5 * 0.8) / 3.5)
  expect_equal(idn$exposure_basis, "blend")
  mex <- ex[ex$destination_iso == "MEX", ]
  expect_equal(mex$chinese_parent_exposure, 0)
  expect_equal(mex$exposure_basis, "count")
  # Fewer than two projects: undefined (no row), as with none.
  expect_false("HUN" %in% ex$destination_iso)
  expect_true("HUN" %in% cm_fdi_chinese_parent_exposure(f, cm_fdi_resolve_settings(list(min_projects = 1)))$destination_iso)
  # China and the SARs are exposed by construction in every covered cell.
  chn <- ex[ex$destination_iso %in% c("CHN", "HKG", "MAC"), ]
  expect_equal(nrow(chn), 3 * 4)
  expect_true(all(chn$chinese_parent_exposure == 1 & chn$exposure_basis == "china_hq"))
  expect_false(any(cm_fdi_chinese_parent_exposure(f, cm_fdi_resolve_settings(list(china_supplier = "missing")))$destination_iso == "CHN"))

  # The alternatives.
  ev <- cm_fdi_chinese_parent_exposure(f, cm_fdi_resolve_settings(list(exposure_weighting = "value")))
  expect_equal(ev$chinese_parent_exposure[ev$destination_iso == "IDN"], c(0.8, 0.8))
  ec <- cm_fdi_chinese_parent_exposure(f, cm_fdi_resolve_settings(list(exposure_weighting = "count")))
  expect_equal(ec$chinese_parent_exposure[ec$destination_iso == "IDN"], rep(2.5 / 3.5, 2))
  expect_true(all(ex_all$chinese_parent_exposure >= 0 & ex_all$chinese_parent_exposure <= 1))
})

test_that("the tie index ranks a reporter's destinations above its zeros and leaves non-investors missing", {
  dyads <- tidyr::expand_grid(
    reporter_iso = c("JPN", "KOR", "USA"),
    partner_iso = c("USA", "MEX", "IDN", "VNM", "KOR", "JPN"),
    tech = "Batteries",
    supply_chain = "Assembly"
  ) %>% dplyr::filter(reporter_iso != partner_iso)
  ties <- tibble::tibble(
    investor_iso = c("JPN", "JPN", "KOR", "KOR"),
    destination_iso = c("USA", "MEX", "USA", "IDN"),
    tech = "Batteries",
    supply_chain = "Assembly",
    n_projects = c(5, 1, 3, 1),
    value_usd_m = c(5000, NA, 9000, 100)
  )
  out <- partnership_strength_cm_fdi_tie_index(dyads, ties)
  expect_equal(nrow(out), nrow(dyads))
  expect_equal(out[c("reporter_iso", "partner_iso")], dyads[c("reporter_iso", "partner_iso")])
  # USA has no project abroad: no tie information, NA for every partner.
  expect_true(all(is.na(out$cm_fdi_tie[out$reporter_iso == "USA"])))
  jpn <- out[out$reporter_iso == "JPN", ]
  expect_true(all(jpn$cm_fdi_tie[jpn$partner_iso %in% c("USA", "MEX")] > max(jpn$cm_fdi_tie[!jpn$partner_iso %in% c("USA", "MEX")])))
  expect_gt(jpn$cm_fdi_tie[jpn$partner_iso == "USA"], jpn$cm_fdi_tie[jpn$partner_iso == "MEX"])
  # Partners without a project share one value (the zero block's mid-rank).
  expect_equal(dplyr::n_distinct(jpn$cm_fdi_tie[!jpn$partner_iso %in% c("USA", "MEX")]), 1)
  expect_true(all(out$cm_fdi_tie >= 0 & out$cm_fdi_tie <= 1, na.rm = TRUE))
  expect_equal(out$cm_fdi_projects[out$reporter_iso == "KOR" & out$partner_iso == "MEX"], 0)
  # A duplicated tie would add rows and shift the result against the dyads it is assigned to.
  expect_error(partnership_strength_cm_fdi_tie_index(dyads, rbind(ties, ties[1, ])), "Duplicate rows in CM-FDI ties")
})

# Friendshore fixture: five economies, Batteries Assembly (covered) and Nuclear Assembly (not), by
# default; `stages` adds the other stages of both technologies.
cmf_fs_inputs <- function(stages = "Assembly") {
  ci <- tibble::tibble(
    country = c("United States", "Japan", "China", "Indonesia", "Mexico"),
    iso3c = c("USA", "JPN", "CHN", "IDN", "MEX")
  )
  cells <- tidyr::expand_grid(tech = c("Batteries", "Nuclear"), supply_chain = stages)
  idx <- function(col, vals) {
    tbl <- tidyr::expand_grid(Country = ci$country, cells)
    tbl[[col]] <- vals(nrow(tbl))
    tbl
  }
  list(
    ci = ci,
    es = idx("Energy_Security_Index", function(n) seq(0.1, 1, length.out = n)),
    eo = idx("Economic_Opportunity_Index", function(n) rev(seq(0.1, 1, length.out = n))),
    imports = tidyr::expand_grid(reporter_iso = ci$iso3c, partner_iso = ci$iso3c, cells) %>%
      dplyr::filter(reporter_iso != partner_iso) %>%
      dplyr::mutate(imp_trade_index = seq(0.05, 0.95, length.out = dplyr::n())),
    outbound = tibble::tibble(reporter_iso = c("USA", "USA", "JPN"), partner_iso = c("JPN", "MEX", "USA"), outbound_index = c(0.7, 0.4, 0.9)),
    ghg = tibble::tibble(tech = c("Batteries", "Nuclear"), ghg_index = 1),
    policy = tibble::tibble(Country = "Japan", climate_policy_index = 0.5),
    cm_fdi = list(
      cells = tibble::tibble(tech = "Batteries", supply_chain = "Assembly"),
      ties = tibble::tibble(investor_iso = c("JPN", "CHN", "CHN"), destination_iso = c("USA", "IDN", "MEX"), tech = "Batteries",
                            supply_chain = "Assembly", n_projects = c(4, 6, 1), value_usd_m = c(8000, 3000, NA)),
      exposure = tibble::tibble(destination_iso = c("IDN", "USA", "CHN"), tech = "Batteries", supply_chain = "Assembly",
                                chinese_parent_exposure = c(0.8, 0.1, 1))
    )
  )
}

cmf_weights <- function(exposure = 1) {
  list(imp_trade_index = 2, econ_opp_raw = 0, es_need = 2, eo_partner = 2, outbound_index = 2, geo_alignment = 2,
       chinese_parent_exposure = exposure)
}

cmf_dyads <- function(cm_fdi, exposure_weight = 1, stages = "Assembly") {
  x <- cmf_fs_inputs(stages)
  partnership_strength_build_friendshore_dyads(
    x$imports, x$eo, x$es, x$outbound, x$ghg, x$policy, x$ci,
    component_weights = cmf_weights(exposure_weight), cm_fdi = cm_fdi
  )
}

test_that("CM-FDI replaces the IMF measure in covered cells only and adds the exposure as 1 - share", {
  x <- cmf_fs_inputs()
  without <- cmf_dyads(NULL)
  with <- cmf_dyads(x$cm_fdi)
  key <- c("reporter_iso", "partner_iso", "tech", "supply_chain")
  expect_equal(with[key], without[key])

  # Without inputs: the IMF build, no exposure.
  expect_true(all(without$outbound_source == "imf_dip"))
  expect_true(all(is.na(without$chinese_parent_exposure)))

  # Uncovered cells (Nuclear) are unchanged, to the bit.
  nuc <- with$tech == "Nuclear"
  same <- c("outbound_index", "fsi_raw", "fsi_adj", "friendshore_index", "imputed_weight_share")
  expect_identical(with[nuc, same], without[nuc, same])
  expect_true(all(with$outbound_source[nuc] == "imf_dip"))
  expect_true(all(is.na(with$chinese_parent_exposure[nuc])))

  bat <- with[with$tech == "Batteries", ]
  expect_true(all(bat$outbound_source == "cm_fdi"))
  # USA, IDN, MEX have no projects abroad: no tie (NA), even where the IMF had a USA position.
  expect_true(all(is.na(bat$outbound_index[bat$reporter_iso %in% c("USA", "IDN", "MEX")])))
  # JPN's USA tie tops its partners; CHN ranks IDN (6 projects) above MEX (1).
  jpn <- bat[bat$reporter_iso == "JPN", ]
  expect_equal(jpn$partner_iso[which.max(jpn$outbound_index)], "USA")
  chn <- bat[bat$reporter_iso == "CHN", ]
  expect_gt(chn$outbound_index[chn$partner_iso == "IDN"], chn$outbound_index[chn$partner_iso == "MEX"])

  # Exposure joined on the partner, the same for every reporter; NA where undefined (JPN, MEX).
  expect_true(all(bat$chinese_parent_exposure[bat$partner_iso == "IDN"] == 0.8))
  expect_true(all(is.na(bat$chinese_parent_exposure[bat$partner_iso %in% c("JPN", "MEX")])))

  # fsi_raw: the available-case weighted mean with the exposure scored 1 - share (ghg 1: no penalty).
  r <- bat[bat$reporter_iso == "JPN" & bat$partner_iso == "IDN", ]
  expect_equal(
    r$fsi_raw,
    (2 * r$imp_trade_index + 2 * r$es_need + 2 * r$eo_partner + 2 * r$outbound_index + 1 * (1 - 0.8)) / 9
  )
  # A higher exposure lowers Friendshore.
  hi <- x$cm_fdi
  hi$exposure$chinese_parent_exposure[hi$exposure$destination_iso == "IDN"] <- 1
  r_hi <- cmf_dyads(hi) %>% dplyr::filter(reporter_iso == "JPN", partner_iso == "IDN", tech == "Batteries")
  expect_lt(r_hi$fsi_raw, r$fsi_raw)

  # Imputed weight share (no alignment table here, so geo_alignment is missing and counts in full):
  # CM-FDI parts count as observed where defined and are left out where undefined, while the IMF
  # rule (a missing position is unobserved) still holds in Nuclear.
  u <- bat[bat$reporter_iso == "USA" & bat$partner_iso == "MEX", ]
  # trade 2 (observed), eo_partner 2 (observed), geo 2 (missing); no tie, no exposure: left out.
  expect_equal(u$imputed_weight_share, 2 / (2 + 2 + 2))
  j <- bat[bat$reporter_iso == "JPN" & bat$partner_iso == "IDN", ]
  # trade, eo_partner, tie (observed zero block), exposure (defined) observed; geo missing.
  expect_equal(j$imputed_weight_share, 2 / (2 + 2 + 2 + 2 + 1))
  n_usa_cn <- with[with$reporter_iso == "USA" & with$partner_iso == "CHN" & nuc, ]
  # trade, eo_partner observed; IMF outbound and geo missing.
  expect_equal(n_usa_cn$imputed_weight_share, (2 + 2) / (2 + 2 + 2 + 2))
})

test_that("the exposure switch and a zero weight leave the component out of the score", {
  x <- cmf_fs_inputs()
  off <- x$cm_fdi
  off$exposure <- NULL
  a <- cmf_dyads(off)
  b <- cmf_dyads(x$cm_fdi, exposure_weight = 0)
  expect_true(all(is.na(a$chinese_parent_exposure)))
  expect_equal(a$fsi_raw, b$fsi_raw)
  expect_equal(a$friendshore_index, b$friendshore_index)
  # Ties off: the IMF measure everywhere.
  no_ties <- x$cm_fdi
  no_ties$ties <- NULL
  nt <- cmf_dyads(no_ties)
  expect_true(all(nt$outbound_source == "imf_dip"))
  expect_equal(nt$outbound_index, cmf_dyads(NULL)$outbound_index)
  # Bad exposure values are refused.
  bad <- x$cm_fdi
  bad$exposure$chinese_parent_exposure[1] <- 1.2
  expect_error(cmf_dyads(bad), "\\[0, 1\\]")
})

test_that("tables carry the exposure, its contribution adds up as 1 - share, and rows cite CM-FDI", {
  x <- cmf_fs_inputs()
  dy <- cmf_dyads(x$cm_fdi)
  tbl <- partnership_strength_build_friendshore_dyads_table(dy, x$ci, cmf_weights())
  expect_true("chinese_parent_exposure" %in% tbl$variable)
  r <- tbl[tbl$reporter_iso == "JPN" & tbl$partner_iso == "IDN" & tbl$tech == "Batteries", ]
  parts <- r[r$variable != "Friendshore Index" & !is.na(r$weighted_component), ]
  expect_equal(sum(parts$weighted_component), dy$fsi_raw[dy$reporter_iso == "JPN" & dy$partner_iso == "IDN" & dy$tech == "Batteries"])
  ex <- r[r$variable == "chinese_parent_exposure", ]
  expect_equal(ex$value, 0.8)
  expect_equal(ex$weighted_component, (1 - 0.8) * 1 / 9)
  expect_match(ex$source, "Cleantech Manufacturing FDI")
  expect_match(r$source[r$variable == "outbound_index"], "Cleantech Manufacturing FDI")
  expect_equal(tbl$source[tbl$variable == "outbound_index" & tbl$tech == "Nuclear"][1], "Author calculation")

  cov <- partnership_strength_cm_fdi_coverage(dy)
  expect_equal(cov$tech, "Batteries")
  expect_equal(cov$n_tie_reporters, 2)
  expect_equal(cov$n_exposure_partners, 3)
  expect_equal(nrow(partnership_strength_cm_fdi_coverage(cmf_dyads(NULL))), 0)
})

test_that("a project counts in both Components and Assembly; Refining keeps the IMF measure", {
  p <- cm_fdi_clean_projects(cmf_raw(
    list("Company Country (HQ)" = "Japan", "Destination Country" = "United States", "Capital Investment (US$m)" = "800"),
    list("Company Country (HQ)" = "Japan", "Destination Country" = "Indonesia", "Capital Investment (US$m)" = "100"),
    list("Company Country (HQ)" = "China", "Destination Country" = "Indonesia", "Capital Investment (US$m)" = "300"),
    list("Company Country (HQ)" = "China", "Destination Country" = "Mexico", "Capital Investment (US$m)" = "")
  ))
  inp <- partnership_strength_cm_fdi_inputs(p, cm_fdi_resolve_settings(NULL))
  # The project table stays one row per project.
  expect_equal(nrow(inp$projects), 4)
  expect_equal(nrow(inp$cells), 4 * 2)
  expect_setequal(paste(inp$cells$tech, inp$cells$supply_chain)[inp$cells$tech == "Batteries"], c("Batteries Components", "Batteries Assembly"))
  # Ties: every tie in both stages with the full attributed count (4 projects per stage).
  expect_equal(sum(inp$ties$n_projects[inp$ties$supply_chain == "Components"]), 4)
  expect_equal(sum(inp$ties$n_projects[inp$ties$supply_chain == "Assembly"]), 4)

  stages <- c("Refining", "Components", "Assembly")
  with <- cmf_dyads(inp, stages = stages)
  without <- cmf_dyads(NULL, stages = stages)
  bat <- function(tbl, s) {
    out <- tbl[tbl$tech == "Batteries" & tbl$supply_chain == s, ]
    out[order(out$reporter_iso, out$partner_iso), ]
  }
  # Components and Assembly carry the same tie and exposure for every dyad.
  for (col in c("outbound_source", "outbound_index", "chinese_parent_exposure")) {
    expect_identical(bat(with, "Components")[[col]], bat(with, "Assembly")[[col]], info = col)
  }
  expect_true(all(bat(with, "Assembly")$outbound_source == "cm_fdi"))
  jpn <- bat(with, "Assembly")
  jpn <- jpn[jpn$reporter_iso == "JPN", ]
  expect_equal(jpn$partner_iso[which.max(jpn$outbound_index)], "USA")
  # IDN: one Chinese plant of 300 and a Japanese one of 100, all disclosed: exposure 0.75.
  expect_true(all(bat(with, "Components")$chinese_parent_exposure[bat(with, "Components")$partner_iso == "IDN"] == 0.75))
  # Refining (battery materials) is not covered: the IMF measure, no exposure, as without CM-FDI.
  ref <- bat(with, "Refining")
  expect_true(all(ref$outbound_source == "imf_dip"))
  expect_true(all(is.na(ref$chinese_parent_exposure)))
  expect_identical(ref$outbound_index, bat(without, "Refining")$outbound_index)
  expect_identical(ref$friendshore_index, bat(without, "Refining")$friendshore_index)
  # The coverage table lists both manufacturing cells, with the same counts.
  cov <- partnership_strength_cm_fdi_coverage(with)
  expect_equal(sort(cov$supply_chain[cov$tech == "Batteries"]), c("Assembly", "Components"))
  expect_equal(cov$n_tie[cov$supply_chain == "Components"], cov$n_tie[cov$supply_chain == "Assembly"])
})

test_that("the manifest declares the staged CM-FDI file for scripts/15", {
  m <- yaml::read_yaml(file.path(cmf_root, "config", "raw_inputs_manifest.yml"))$inputs
  e <- Filter(function(x) identical(x$id, "cm_fdi_projects"), m)
  expect_length(e, 1)
  e <- e[[1]]
  expect_equal(e$path, "cm_fdi_projects.csv")
  expect_equal(e$licence, "CC-BY-4.0")
  expect_match(e$vintage, "2026.10.05", fixed = TRUE)
  expect_match(e$notes, "Bandara, P. and the Net Zero Industrial Policy Lab", fixed = TRUE)
  expect_true("scripts/15_build_partner_themes.R" %in% unlist(e$required_by))
})

test_that("the staged CM-FDI file parses to the 2026.10.05 edition", {
  path <- file.path(cmf_root, "data", "raw", "cm_fdi_projects.csv")
  skip_if_not(file.exists(path), "data/raw/cm_fdi_projects.csv is not staged")
  raw <- utils::read.csv(path, colClasses = "character", check.names = FALSE, fileEncoding = "UTF-8-BOM",
                         na.strings = character(), stringsAsFactors = FALSE)
  s <- cm_fdi_resolve_settings(yaml::read_yaml(file.path(cmf_root, "config", "partnership.yml"))$cm_fdi)
  inp <- partnership_strength_cm_fdi_inputs(cm_fdi_clean_projects(raw, s$technologies), s)
  p <- inp$projects
  expect_gte(nrow(p), 1042)
  expect_equal(sum(p$activity == "Manufacturing"), 974)
  expect_true(all(!is.na(p$home_iso3) & !is.na(p$destination_iso3)))
  expect_equal(sum(is.na(p$capital_usd_m)), 259)
  expect_true(all(p$index_use == is.na(p$index_exclusion)))
  expect_true(all(inp$exposure$chinese_parent_exposure >= 0 & inp$exposure$chinese_parent_exposure <= 1))
  expect_false(anyDuplicated(inp$ties[c("investor_iso", "destination_iso", "tech", "supply_chain")]) > 0)
})
