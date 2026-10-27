# The PSI rankability rule: the imputed weight share of the Friendshore, Opportunity and
# Development cells (R/themes/partnership_strength/), its top-5 and partner-level averages, and the
# pillars' rule applied to partners (psi_flag_rankable(), R/indices/build_partnership_strength_index.R).

ppr_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(ppr_root, "R", "utils", "scurve.R"))
source(file.path(ppr_root, "R", "utils", "country.R"))
source(file.path(ppr_root, "R", "utils", "schema.R"))
source(file.path(ppr_root, "R", "utils", "levels.R"))
source(file.path(ppr_root, "R", "indices", "index_builder_core.R"))
source(file.path(ppr_root, "R", "indices", "build_partnership_strength_index.R"))
source(file.path(ppr_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(ppr_root, "R", "themes", "partnership_strength", "prosperous_opportunity.R"))
source(file.path(ppr_root, "R", "themes", "partnership_strength", "safer_friendshore.R"))
source(file.path(ppr_root, "R", "themes", "partnership_strength", "stronger_development.R"))

`%>%` <- dplyr::`%>%`

ppr_country_info <- function() {
  tibble::tibble(
    country = c("United States", "Japan", "China", "India", "Viet Nam"),
    iso3c = c("USA", "JPN", "CHN", "IND", "VNM"),
    income = c("High income", "High income", "Upper middle income", "Lower middle income", "Lower middle income")
  )
}

# A pillar table (Solar, two stages) with an imputed_weight_share per country.
ppr_index_tbl <- function(value_col, values, shares = NULL) {
  tbl <- tidyr::expand_grid(
    Country = c("United States", "Japan", "China", "India", "Viet Nam"),
    tech = "Solar",
    supply_chain = c("Extraction", "Deployment")
  )
  tbl[[value_col]] <- values
  if (!is.null(shares)) tbl$imputed_weight_share <- if (length(shares) == 1) shares else rep(shares, each = 2)
  tbl
}

test_that("the cell share weighs the unobserved fraction of each component and skips absent ones", {
  share <- partnership_strength_imputed_weight_share(
    list(c(1, NA, 0), c(0.2, 0.2, NA), c(0, 1, NA)),
    c(2, 2, 1)
  )
  expect_equal(share, c((2 * 1 + 2 * 0.2 + 0) / 5, (2 * 0.2 + 1) / 3, 0))
  # A zero weight takes the component out; no component at all gives NA.
  expect_equal(partnership_strength_imputed_weight_share(list(1, 0), c(0, 3)), 0)
  expect_true(is.na(partnership_strength_imputed_weight_share(list(NA_real_), 1)))
  expect_error(partnership_strength_imputed_weight_share(list(1, 0), 1), "align")

  expect_equal(partnership_strength_trade_unobserved(c("trade", "zero", "imputed", "no_data", NA)), c(0, 0, 1, NA, 0))
  expect_equal(partnership_strength_pillar_unobserved(c(0.3, NA, 0.8), c(0.25, 0.1, NA)), c(0.25, 1, 0))
  expect_equal(partnership_strength_pillar_unobserved(c(0.3, NA), NULL), c(0, 1))
})

test_that("the top-5 share averages the cells the top-5 value averages", {
  cells <- tibble::tibble(
    reporter_iso = "USA",
    partner_iso = rep(c("JPN", "IND"), c(7, 2)),
    tech = c(letters[1:7], "a", "b"),
    supply_chain = "Assembly",
    friendshore_index = c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, NA, 0.2, 0.1),
    imputed_weight_share = c(0, 0, 0, 0, 1, 1, 1, 0.5, 0.3)
  )
  out <- partnership_strength_top_k_share(cells, c("reporter_iso", "partner_iso"), "friendshore_index", k = 5L)
  expect_equal(out$imputed_weight_share[out$partner_iso == "JPN"], 0.2)
  expect_equal(out$imputed_weight_share[out$partner_iso == "IND"], 0.4)
  # Ties at the k-th place are broken by tech order, so the result is reproducible.
  tied <- tibble::tibble(u = "A", tech = c("b", "a"), supply_chain = "x", v = 1, imputed_weight_share = c(1, 0))
  expect_equal(partnership_strength_top_k_share(tied, "u", "v", k = 1L)$imputed_weight_share, 0)

  top5 <- tibble::tibble(u = "A", variable = c("Index", "component"), value = 1)
  marked <- partnership_strength_add_top5_share(top5, tibble::tibble(u = "A", imputed_weight_share = 0.4), "u", "Index")
  expect_equal(marked$imputed_weight_share, c(0.4, NA))
})

test_that("Friendshore cells count imputed trade, the partner's EO share, missing outbound and assigned blocs", {
  ci <- ppr_country_info()
  eo <- ppr_index_tbl("Economic_Opportunity_Index", rev(seq(0.1, 1, length.out = 10)), shares = c(0, 0.2, 0.4, 0.6, 0.8))
  es <- ppr_index_tbl("Energy_Security_Index", seq(0.1, 1, length.out = 10), shares = 0.9)
  geo <- tibble::tibble(
    reporter_iso = "USA",
    partner_iso = c("JPN", "IND"),
    geo_alignment = c(1, 0.5),
    basis_reporter = "own_votes",
    basis_partner = c("own_votes", "neutral")
  )
  w <- list(imp_trade_index = 2, econ_opp_raw = 1, es_need = 2, eo_partner = 2, outbound_index = 2, geo_alignment = 2, chinese_parent_exposure = 1)
  dyads <- partnership_strength_build_friendshore_dyads(
    tibble::tibble(reporter_iso = "USA", partner_iso = "JPN", tech = "Solar", supply_chain = "Extraction", imp_trade_index = 0.9),
    eo, es,
    tibble::tibble(reporter_iso = "USA", partner_iso = "JPN", outbound_index = 0.6),
    tibble::tibble(tech = "Solar", ghg_index = 1),
    tibble::tibble(Country = "Japan", climate_policy_index = 0.75),
    ci,
    component_weights = w,
    geo_alignment = geo
  )
  cell <- function(r, p, s) dyads[dyads$reporter_iso == r & dyads$partner_iso == p & dyads$supply_chain == s, ]
  # Observed trade, outbound and votes; Japan's EO cell is 20% imputed. The reporter's ES (90%
  # imputed) does not count.
  expect_equal(cell("USA", "JPN", "Extraction")$imputed_weight_share, (2 * 0.2) / 8)
  # India: trade imputed, EO 60% imputed, no outbound position, neutral bloc.
  expect_equal(cell("USA", "IND", "Extraction")$imputed_weight_share, (2 + 2 * 0.6 + 2 + 2) / 8)
  # Deployment has no trade data at all (no_data): the trade component is left out.
  expect_equal(cell("USA", "IND", "Deployment")$trade_status, "no_data")
  expect_equal(cell("USA", "IND", "Deployment")$imputed_weight_share, (2 * 0.6 + 2 + 2) / 6)
  # A dyad the alignment table does not cover counts its geo weight as missing.
  expect_equal(cell("USA", "CHN", "Extraction")$imputed_weight_share, (2 + 2 * 0.4 + 2 + 2) / 8)

  # Without pillar shares (a pillar build before the rule) the EO component counts as observed.
  plain <- partnership_strength_build_friendshore_dyads(
    tibble::tibble(reporter_iso = "USA", partner_iso = "JPN", tech = "Solar", supply_chain = "Extraction", imp_trade_index = 0.9),
    dplyr::select(eo, -imputed_weight_share), dplyr::select(es, -imputed_weight_share),
    tibble::tibble(reporter_iso = "USA", partner_iso = "JPN", outbound_index = 0.6),
    tibble::tibble(tech = "Solar", ghg_index = 1),
    tibble::tibble(Country = "Japan", climate_policy_index = 0.75),
    ci,
    component_weights = w,
    geo_alignment = geo
  )
  expect_equal(plain$imputed_weight_share[plain$partner_iso == "JPN" & plain$supply_chain == "Extraction" & plain$reporter_iso == "USA"], 0)
  # The share does not touch the scores.
  expect_equal(dyads$fsi_raw, plain$fsi_raw[match(paste(dyads$reporter_iso, dyads$partner_iso, dyads$supply_chain),
                                                   paste(plain$reporter_iso, plain$partner_iso, plain$supply_chain))])
})

test_that("Opportunity cells count imputed trade and the market's ES share, not the reporter's EO", {
  ci <- ppr_country_info()
  eo <- ppr_index_tbl("Economic_Opportunity_Index", rev(seq(0.1, 1, length.out = 10)), shares = 0.9)
  es <- ppr_index_tbl("Energy_Security_Index", seq(0.1, 1, length.out = 10), shares = c(0, 0.3, 0.5, 0.7, 1))
  op <- partnership_strength_build_opportunity_dyads(
    tibble::tibble(reporter_iso = "USA", partner_iso = "JPN", tech = "Solar", supply_chain = "Extraction", trade_index = 0.8),
    eo, es,
    tibble::tibble(tech = "Solar", ghg_index = 0.9),
    tibble::tibble(Country = "Japan", climate_policy_index = 0.75),
    ci,
    component_weights = list(trade_index = 2, econ_opp_index = 2, partner_es_need = 1, climate_policy_weight = 0.2)
  )
  cell <- function(r, p, s) op[op$reporter_iso == r & op$partner_iso == p & op$supply_chain == s, ]
  expect_equal(cell("USA", "JPN", "Extraction")$imputed_weight_share, (1 * 0.3) / 3)
  expect_equal(cell("USA", "IND", "Extraction")$imputed_weight_share, (2 + 1 * 0.7) / 3)
  expect_equal(cell("USA", "IND", "Deployment")$imputed_weight_share, 0.7)
})

test_that("Development counts missing indicators in full and the ES / EO components at their pillar share", {
  ci <- ppr_country_info()
  es <- ppr_index_tbl("Energy_Security_Index", seq(0.1, 1, length.out = 10), shares = c(0, 0, 0.5, 0.2, 0.4))
  eo <- ppr_index_tbl("Economic_Opportunity_Index", rev(seq(0.1, 1, length.out = 10)), shares = c(0, 0, 0.1, 0.3, 0.6))
  wb_wdi <- tibble::tibble(
    Country.Name = ci$country,
    Country.Code = ci$iso3c,
    Series.Name = "GDP per capita (constant 2015 US$)",
    Series.Code = "NY.GDP.PCAP.KD",
    X2023 = c(61000, 35500, 11000, 2100, 3600)
  )
  # Viet Nam has no Doing Business score.
  wb_db <- tibble::tibble(
    Country.Name = ci$country[1:4],
    Country.Code = ci$iso3c[1:4],
    Series.Code = "IC.BUS.EASE.DFRN.XQ.DB1719",
    X2019 = c(84, 78, 77, 71)
  )
  # Only India receives aid (solar, Deployment). Its Extraction aid is then an observed zero; Viet Nam,
  # absent from the CRS, has no aid value.
  oecd <- tibble::tibble(
    year = 2022L, donor_iso3 = "USA", recipient_iso3 = "IND", purpose_code = 23230L,
    measure = "ODA", usd_commitment = 0, usd_disbursement = 10
  )
  crosswalk <- read.csv(file.path(ppr_root, "data", "reference", "oecd_crs_purpose_crosswalk.csv"))
  gdp_data <- tibble::tibble(iso3c = ci$iso3c, year = 2023, NY.GDP.MKTP.CD = c(27, 4, 18, 3.5, 0.4) * 1e12)
  w <- unlist(partnership_strength_development_default_weights())

  idx <- partnership_strength_build_development_index(es, eo, oecd, crosswalk, wb_wdi, wb_db, ci, gdp_data)
  share <- function(iso, stage) idx$imputed_weight_share[idx$iso3c == iso & idx$supply_chain == stage]
  # Missing for everyone: energy_use_index (no such WDI series here). India Deployment has aid.
  missing_ind_down <- w[["energy_use_index"]]
  expect_equal(share("IND", "Deployment"), (missing_ind_down + w[["energy_security"]] * 0.2 + w[["economic_opportunity"]] * 0.3) / sum(w))
  expect_equal(share("IND", "Extraction"), (missing_ind_down + w[["energy_security"]] * 0.2 + w[["economic_opportunity"]] * 0.3) / sum(w))
  # Viet Nam has no aid value and also lacks Doing Business and so the investment environment built on it.
  missing_vnm <- w[["energy_use_index"]] + w[["aid_index_supplychain"]] + w[["aidgdp_index_supplychain"]] +
    w[["doingbusiness_index"]] + w[["investment_enviro_index"]]
  expect_equal(share("VNM", "Extraction"), (missing_vnm + w[["energy_security"]] * 0.4 + w[["economic_opportunity"]] * 0.6) / sum(w))

  dev <- stronger_development(ci, gdp_data, eo, es, wb_wdi, wb_db, oecd, crosswalk)
  top <- dev$top5[dev$top5$variable == "Development Potential Index", ]
  expect_true(all(!is.na(top$imputed_weight_share)))
  expect_true(all(is.na(dev$top5$imputed_weight_share[dev$top5$variable != "Development Potential Index"])))
  vnm_cells <- idx[idx$iso3c == "VNM", ]
  expect_equal(top$imputed_weight_share[top$partner_iso == "VNM"], mean(vnm_cells$imputed_weight_share))
})

# Top-5 inputs for three partners (IND, VNM, MCO) rated by USA and JPN (coalition) and CHN.
ppr_top5 <- function(category, variable, values, shares) {
  tibble::tibble(
    reporter = NA_character_,
    partner = rep(c("India", "Viet Nam", "Monaco"), each = 3),
    reporter_iso = rep(c("USA", "JPN", "CHN"), 3),
    partner_iso = rep(c("IND", "VNM", "MCO"), each = 3),
    category = category,
    variable = variable,
    data_type = "index",
    value = values,
    imputed_weight_share = shares,
    Year = 2024L,
    source = "test",
    explanation = "test"
  )
}

ppr_inputs <- function() {
  list(
    fs = ppr_top5("friendshore", "Friendshore Index", c(0.8, 0.6, 0.1, 0.5, 0.5, 0.9, 0.9, 0.9, 0.2),
                  c(0.1, 0.3, 0.2, 0.2, 0.2, 0.9, 0.7, 0.5, 0.6)),
    op = ppr_top5("opportunity", "Opportunity Index", c(0.4, 0.6, 0.2, 0.3, 0.5, 1.0, 0.8, 0.8, 0.3),
                  c(0.2, 0.2, 0.2, 0.0, 0.4, 0.8, 0.9, 0.7, 0.6)),
    dv = tibble::tibble(
      partner = c("India", "Viet Nam"),
      partner_iso = c("IND", "VNM"),
      category = "development",
      variable = "Development Potential Index",
      data_type = "index",
      value = c(0.5, 0.7),
      imputed_weight_share = c(0.4, 0.6),
      Year = 2024L,
      source = "test",
      explanation = "test"
    )
  )
}

ppr_w <- list(friendshore = 0.4, opportunity = 0.4, development = 0.2)

test_that("the PSI share averages the rows the score averages, view by view", {
  x <- ppr_inputs()
  views <- build_partnership_strength_viewpoints(x$fs, x$op, x$dv, ppr_w, coalition_iso3 = c("USA", "JPN"))
  coal <- views$coalition$imputation
  world <- views$world$imputation
  ind_c <- coal[coal$Country == "India", ]
  expect_equal(ind_c$friendshore_imputed_share, 0.2)
  expect_equal(ind_c$opportunity_imputed_share, 0.2)
  expect_equal(ind_c$development_imputed_share, 0.4)
  expect_equal(ind_c$imputed_weight_share, 0.4 * 0.2 + 0.4 * 0.2 + 0.2 * 0.4)
  # The world view adds China's ratings.
  expect_equal(world$imputed_weight_share[world$Country == "Viet Nam"],
               0.4 * mean(c(0.2, 0.2, 0.9)) + 0.4 * mean(c(0, 0.4, 0.8)) + 0.2 * 0.6)
  # Monaco (high income) has no Development: the share renormalizes over FS and OP like the score.
  expect_equal(coal$imputed_weight_share[coal$Country == "Monaco"], (0.4 * 0.6 + 0.4 * 0.8) / 0.8)
  expect_true(is.na(coal$development_imputed_share[coal$Country == "Monaco"]))
  # The index itself is unchanged by the share.
  expect_equal(views$coalition$index$Partnership_Strength_Index[views$coalition$index$Country == "India"],
               0.4 * 0.7 + 0.4 * 0.5 + 0.2 * 0.5)

  # Inputs built before the rule carry no share: the partner's share is unknown.
  x$fs$imputed_weight_share <- NULL
  old <- build_partnership_strength_viewpoints(x$fs, x$op, x$dv, ppr_w, coalition_iso3 = "USA")
  expect_true(all(is.na(old$coalition$imputation$imputed_weight_share)))
})

test_that("a partner is ranked only when sovereign and its imputed share is at most tau", {
  psi <- tibble::tibble(
    Country = c("Cayman Islands", "United States", "Hong Kong SAR, China", "Japan", "India", "Monaco", "Viet Nam", "Taiwan"),
    partner_iso = c("CYM", "USA", "HKG", "JPN", "IND", "MCO", "VNM", "TWN"),
    value = c(0.95, 0.9, 0.85, 0.8, 0.8, 0.75, NA, 0.7),
    imputed_weight_share = c(0.1, 0.2, 0.7, 0.5 + 1e-12, 0.3, 0.6, 0.2, NA)
  )
  out <- psi_flag_rankable(psi, max_imputed_weight_share = 0.5)
  expect_equal(out$value, psi$value)
  expect_equal(out$rankable, c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(out$rank_exclusion, c(
    "not a sovereign economy",
    NA,
    "not a sovereign economy and imputed weight share above 0.5",
    NA,
    NA,
    "imputed weight share above 0.5",
    "not scored",
    "imputed weight share unknown"
  ))
  # Ties take the lowest rank; only rankable partners are ranked.
  expect_equal(out$rank, c(NA, 1L, NA, 2L, 2L, NA, NA, NA))
  # tau = 1 leaves the sovereignty condition alone.
  expect_equal(psi_flag_rankable(psi, max_imputed_weight_share = 1)$rankable, c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_error(psi_flag_rankable(dplyr::select(psi, -imputed_weight_share), max_imputed_weight_share = 0.5), "imputed_weight_share")

  # Without an argument tau comes from the option's psi entry, else the pillars' value.
  old <- options(opportunity_security.rankability = list(max_imputed_weight_share = 0.9, psi = list(max_imputed_weight_share = 0.25)))
  on.exit(options(old), add = TRUE)
  expect_equal(psi_resolve_max_imputed_weight_share(), 0.25)
  options(opportunity_security.rankability = list(max_imputed_weight_share = 0.9))
  expect_equal(psi_resolve_max_imputed_weight_share(), 0.9)
  expect_equal(psi_resolve_max_imputed_weight_share(0.4), 0.4)
})

test_that("sovereignty reads the ISO3 code when the partner has no name", {
  expect_equal(psi_is_sovereign(c("MCO", "Monaco", "HKG", "XKX", "TWN"), c("MCO", NA, "HKG", "XKX", "TWN")),
               c(TRUE, TRUE, FALSE, TRUE, TRUE))
  expect_equal(psi_is_sovereign(c("Monaco", "Hong Kong SAR, China")), c(TRUE, FALSE))
})

test_that("the dyadic PSI carries its imputed share over the categories the dyad has", {
  x <- ppr_inputs()
  d <- build_partnership_strength_dyads(x$fs, x$op, x$dv, ppr_w)
  usa_ind <- d[d$reporter_iso == "USA" & d$partner_iso == "IND", ]
  expect_equal(usa_ind$imputed_weight_share, 0.4 * 0.1 + 0.4 * 0.2 + 0.2 * 0.4)
  usa_mco <- d[d$reporter_iso == "USA" & d$partner_iso == "MCO", ]
  expect_equal(usa_mco$imputed_weight_share, (0.4 * 0.7 + 0.4 * 0.9) / 0.8)
  expect_equal(psi_weighted_share(list(1, NA), list(NA, 0.5), c(1, 1)), NA_real_)
  expect_equal(psi_weighted_share(list(NA, 1), list(NA, 0.5), c(1, 1)), 0.5)
})

test_that("config/rankability.yml gives the PSI the pillars' tau, and scripts/25 applies it", {
  rule <- yaml::read_yaml(file.path(ppr_root, "config", "rankability.yml"))
  expect_false(is.null(rule$psi$max_imputed_weight_share))
  # User decision (2026-10-08): the PSI takes the pillars' rule, threshold included.
  expect_equal(rule$psi$max_imputed_weight_share, rule$max_imputed_weight_share)

  s25 <- paste(sub("#.*$", "", readLines(file.path(ppr_root, "scripts", "25_build_partner_indices.R"), warn = FALSE)), collapse = "\n")
  expect_match(s25, "rankability.yml", fixed = TRUE)
  expect_match(s25, "psi_flag_rankable(max_imputed_weight_share = psi_tau)", fixed = TRUE)
  expect_match(s25, "dplyr::left_join(view$imputation, by = \"Country\")", fixed = TRUE)
  expect_match(s25, "rerun scripts/15_build_partner_themes.R", fixed = TRUE)
})
