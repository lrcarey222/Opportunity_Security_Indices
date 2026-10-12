repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "levels.R"))
source(file.path(repo_root, "R", "indices", "index_builder_core.R"))
source(file.path(repo_root, "R", "indices", "build_partnership_strength_index.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "prosperous_opportunity.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "safer_friendshore.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "stronger_development.R"))

# Small shared fixture: five economies, one of them high income, one tech in two stages.
ps_country_info <- function() {
  tibble::tibble(
    country = c("United States", "Japan", "China", "India", "Viet Nam"),
    iso3c = c("USA", "JPN", "CHN", "IND", "VNM"),
    income = c("High income", "High income", "Upper middle income", "Lower middle income", "Lower middle income")
  )
}

ps_index_tbl <- function(value_col, values) {
  countries <- c("United States", "Japan", "China", "India", "Viet Nam")
  tbl <- tidyr::expand_grid(
    Country = countries,
    tech = "Solar",
    supply_chain = c("Upstream", "Downstream")
  )
  tbl[[value_col]] <- values
  tbl
}

test_that("headline techs match the pillars' techs vector in scripts/20", {
  exprs <- parse(file.path(repo_root, "scripts", "20_build_indices.R"))
  is_techs <- vapply(exprs, function(e) {
    is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("techs"))
  }, logical(1))
  expect_true(any(is_techs))
  pillar_techs <- eval(exprs[[which(is_techs)[1]]][[3]])
  expect_setequal(partnership_strength_headline_techs(), pillar_techs)
})

test_that("row weighted mean drops missing components instead of scoring them 0", {
  out <- partnership_strength_row_weighted_mean(
    list(c(0.8, 0.8, NA), c(0.2, NA, NA)),
    c(1, 3)
  )
  expect_equal(out[1], (0.8 + 3 * 0.2) / 4)
  expect_equal(out[2], 0.8)
  expect_true(is.na(out[3]))
})

test_that("assert_unique stops on duplicate keys and passes unique tables through", {
  tbl <- tibble::tibble(a = c("x", "x", "y"), b = c(1, 1, 2))
  expect_error(partnership_strength_assert_unique(tbl, c("a", "b"), "toy"), "Duplicate rows in toy")
  expect_identical(partnership_strength_assert_unique(tbl[2:3, ], c("a", "b")), tbl[2:3, ])
})

test_that("country tables collapse spelling variants to one row per ISO3", {
  ci <- ps_country_info()
  es <- tibble::tibble(
    Country = c("Viet Nam", "Vietnam", "Taiwan, Province of China", "USSR", "Japan", "Japan"),
    tech = c("Solar", "Solar", "Solar", "Solar", "Solar", "Aluminium"),
    supply_chain = "Upstream",
    Energy_Security_Index = c(0.2, 0.9, 0.5, 0.7, 0.4, 0.3)
  )
  out <- partnership_strength_country_iso_table(
    es,
    "Energy_Security_Index",
    ci,
    techs = partnership_strength_headline_techs()
  )

  expect_false(anyDuplicated(out[c("iso3c", "tech", "supply_chain")]) > 0)
  # The canonical spelling wins over the variant; it is not averaged with it.
  expect_equal(out$Energy_Security_Index[out$iso3c == "VNM"], 0.2)
  # Taiwan keeps its own ISO3; USSR is a historical entity and is not read as Russia.
  expect_true("TWN" %in% out$iso3c)
  expect_false("RUS" %in% out$iso3c)
  # Non-headline technologies are filtered out when techs is given.
  expect_false("Aluminium" %in% out$tech)
})

test_that("ISO3 codes outside WDI get a name, so the PSI cannot pool them into NA", {
  ci <- ps_country_info()
  nm <- partnership_strength_iso_to_country(c("TWN", "USA", "AIA"), ci)
  expect_false(any(is.na(nm)))
  expect_equal(nm[1:2], c("Taiwan", "United States"))
  # The name maps back to the same code.
  expect_equal(partnership_strength_country_to_iso(nm, ci), c("TWN", "USA", "AIA"))
})

test_that("outbound index averages its three measures and omits dyads without a DIP position", {
  indicator <- "Outward Direct investment, Assets (gross), Debt instruments, All entities"
  fdi_raw <- tibble::tibble(
    COUNTRY = "United States",
    COUNTERPART_COUNTRY = c("Japan", "China, People's Republic of", "India", "Viet Nam", "Korea, Republic of", "World"),
    DV_TYPE = "Reported official data",
    INDICATOR = indicator,
    # Outside the window: ignored.
    X2019 = c(1000, 1000, 1000, 1000, 1000, 1000),
    X2020 = c(100, 40, 30, 0, NA, 500),
    X2021 = c(300, 60, NA, 0, NA, 500)
  )
  gdp_data <- tibble::tibble(
    iso3c = c("USA", "JPN", "CHN", "IND", "VNM"),
    year = 2023,
    NY.GDP.MKTP.CD = c(27e12, 4e12, 18e12, 3.5e12, 0.4e12)
  )

  out <- partnership_strength_build_outbound_edges(
    fdi_raw,
    ps_country_info(),
    gdp_data,
    gdp_year = 2023,
    years = 2020:2024
  )

  # IMF's spelling of China is matched; a row with no reported year (KOR) and the World
  # aggregate are absent rather than scored 0; a reported zero (VNM) is kept.
  expect_setequal(out$partner_iso, c("JPN", "CHN", "IND", "VNM"))
  expect_false(anyDuplicated(out[c("reporter_iso", "partner_iso")]) > 0)

  # Positions are the mean of the reported years in the window: JPN 200, CHN 50, IND 30, VNM 0.
  total <- c(JPN = 200, CHN = 50, IND = 30, VNM = 0)
  gdp_p <- c(JPN = 4e6, CHN = 18e6, IND = 3.5e6, VNM = 0.4e6)
  share <- total / gdp_p
  rel <- (total / 27e6) / gdp_p
  raw <- (median_scurve(total) + median_scurve(share) + median_scurve(rel)) / 3
  expected <- stats::setNames(median_scurve(raw), names(total))
  got <- stats::setNames(out$outbound_index, out$partner_iso)[names(total)]
  expect_equal(unname(got), unname(expected))
  # Not the total alone (the old sequence-operator result): CHN's position is small relative to
  # its GDP, so it ranks below IND once the share measures count.
  expect_false(isTRUE(all.equal(unname(got), median_scurve(unname(total)))))
  expect_gt(got[["IND"]], got[["CHN"]])
})

test_that("zero DIP positions form one tie block below the positive ones", {
  # Germany has no GDP row, so only its level measure is defined. Its zero used to rank apart
  # from the other zeros (their three-measure means differed in the last digits).
  ci <- tibble::tibble(
    country = c("United States", "Japan", "China", "India", "Viet Nam", "Germany", "France"),
    iso3c = c("USA", "JPN", "CHN", "IND", "VNM", "DEU", "FRA"),
    income = "High income"
  )
  fdi_raw <- tibble::tibble(
    COUNTRY = "United States",
    COUNTERPART_COUNTRY = c("Japan", "India", "China, People's Republic of", "Viet Nam", "Germany", "France"),
    DV_TYPE = "Reported official data",
    INDICATOR = "Outward Direct investment, Assets (gross), Debt instruments, All entities",
    X2022 = c(50, 10, 0, 0, 0, -2)
  )
  gdp_data <- tibble::tibble(
    iso3c = c("USA", "JPN", "CHN", "IND", "VNM", "FRA"),
    year = 2023,
    NY.GDP.MKTP.CD = c(27, 4, 18, 3.5, 0.4, 3) * 1e12
  )

  out <- partnership_strength_build_outbound_edges(fdi_raw, ci, gdp_data, gdp_year = 2023)
  got <- stats::setNames(out$outbound_index, out$partner_iso)

  zeros <- unname(got[c("CHN", "VNM", "DEU", "FRA")])
  # One block at its mid-rank among the reporter's six counterparts, below both positives.
  expect_equal(zeros, rep(median_scurve(c(2, 1, 0, 0, 0, 0))[3], 4))
  expect_gt(got[["IND"]], max(zeros))
  expect_equal(got[["JPN"]], 1)
})

test_that("friendshore dyads are unique, exclude self-dyads and drop a missing outbound component", {
  ci <- ps_country_info()
  # Grouped inputs and a variant spelling reproduce both causes of the old duplication.
  es <- ps_index_tbl("Energy_Security_Index", seq(0.1, 1, length.out = 10)) %>%
    dplyr::bind_rows(tibble::tibble(
      Country = "Vietnam",
      tech = "Solar",
      supply_chain = "Upstream",
      Energy_Security_Index = 0.99
    )) %>%
    dplyr::group_by(Country)
  eo <- ps_index_tbl("Economic_Opportunity_Index", rev(seq(0.1, 1, length.out = 10))) %>%
    dplyr::group_by(tech)
  import_indices <- tibble::tibble(
    reporter_iso = "USA",
    partner_iso = "JPN",
    tech = "Solar",
    supply_chain = "Upstream",
    imp_trade_index = 0.9
  )
  outbound_edges <- tibble::tibble(reporter_iso = "USA", partner_iso = "JPN", outbound_index = 0.6)
  policy <- tibble::tibble(Country = "Japan", climate_policy_index = 0.5)
  tech_ghg <- tibble::tibble(tech = "Solar", ghg_index = 1)
  # No alignment table: geo_alignment is NA for every dyad and drops out whatever its weight
  # (test-friendshore-geo-alignment.R covers the component).
  weights <- list(imp_trade_index = 2, econ_opp_raw = 0, es_need = 2, eo_partner = 2, outbound_index = 2, geo_alignment = 2, chinese_parent_exposure = 1)

  out <- partnership_strength_build_friendshore_dyads(
    import_indices, eo, es, outbound_edges, tech_ghg, policy, ci,
    component_weights = weights
  )

  expect_equal(nrow(out), 5 * 4 * 2)
  expect_false(anyDuplicated(out[c("reporter_iso", "partner_iso", "tech", "supply_chain")]) > 0)
  expect_false(any(out$reporter_iso == out$partner_iso))
  expect_equal(dplyr::n_distinct(out$reporter_iso), 5)

  # With ghg_index = 1 there is no penalty, so fsi_raw is the available-case weighted mean.
  no_dip <- out %>% dplyr::filter(reporter_iso == "USA", partner_iso == "CHN", supply_chain == "Upstream")
  expect_true(is.na(no_dip$outbound_index))
  expect_equal(no_dip$fsi_raw, (2 * no_dip$imp_trade_index + 2 * no_dip$es_need + 2 * no_dip$eo_partner) / 6)
  with_dip <- out %>% dplyr::filter(reporter_iso == "USA", partner_iso == "JPN", supply_chain == "Upstream")
  expect_equal(with_dip$outbound_index, 0.6)
  expect_equal(
    with_dip$fsi_raw,
    (2 * 0.9 + 2 * with_dip$es_need + 2 * with_dip$eo_partner + 2 * 0.6) / 8
  )
})

test_that("safer_friendshore and prosperous_opportunity write one row per dyad cell and variable", {
  ci <- ps_country_info()
  es <- ps_index_tbl("Energy_Security_Index", seq(0.1, 1, length.out = 10))
  eo <- ps_index_tbl("Economic_Opportunity_Index", rev(seq(0.1, 1, length.out = 10)))
  comtrade <- tibble::tibble(
    reporter_iso = "USA",
    partner_iso = c("JPN", "CHN", "JPN", "CHN"),
    flow_direction = c("Import", "Import", "Export", "Export"),
    period = c(2020, 2024, 2020, 2024),
    cmd_code = "854140",
    primary_value = c(10, 20, 5, 8)
  )
  subcat <- tibble::tibble(
    tech = c("Solar", "Solar", "Solar"),
    supply_chain = c("Downstream", "Downstream", "Downstream"),
    code = c("854140", "854140", "854140")
  )
  fdi_raw <- tibble::tibble(
    COUNTRY = "United States",
    COUNTERPART_COUNTRY = c("Japan", "India"),
    DV_TYPE = "Reported official data",
    INDICATOR = "Outward Direct investment, Assets (gross), Debt instruments, All entities",
    X2022 = c(100, 10)
  )
  gdp_data <- tibble::tibble(iso3c = ci$iso3c, year = 2023, NY.GDP.MKTP.CD = c(27, 4, 18, 3.5, 0.4) * 1e12)
  tech_ghg <- tibble::tibble(tech = "Solar", ghg_index = 0.9)
  policy <- tibble::tibble(Country = "Japan", climate_policy_index = 0.75)

  fs <- safer_friendshore(
    comtrade, subcat, eo, es, tech_ghg, policy, fdi_raw, ci, gdp_data,
    techs = partnership_strength_headline_techs()
  )
  op <- prosperous_opportunity(
    comtrade, subcat, eo, es, tech_ghg, policy, ci,
    techs = partnership_strength_headline_techs()
  )

  for (tbl in list(fs$variable, op$variable)) {
    expect_false(anyDuplicated(tbl[c("reporter_iso", "partner_iso", "tech", "supply_chain", "variable")]) > 0)
    expect_false(any(tbl$reporter_iso == tbl$partner_iso))
    expect_false(any(is.na(tbl$partner)))
  }
  expect_true("partner_es_need" %in% op$variable$variable)
  expect_false("energy_security_index" %in% op$variable$variable)

  # Component contributions are renormalized over the components a dyad has, so they add up to
  # a weighted mean on the index scale even where outbound is missing.
  contrib <- fs$contributions %>%
    dplyr::filter(variable != "Friendshore Index") %>%
    dplyr::group_by(reporter_iso, partner_iso, tech, supply_chain) %>%
    dplyr::summarise(total = sum(contribution), .groups = "drop")
  w <- unlist(partnership_strength_friendshore_default_weights())
  expected <- fs$variable %>%
    dplyr::filter(variable != "Friendshore Index", !is.na(value)) %>%
    dplyr::mutate(w = unname(w[variable])) %>%
    dplyr::group_by(reporter_iso, partner_iso, tech, supply_chain) %>%
    dplyr::summarise(fsi_raw = sum(w * value) / sum(w), .groups = "drop")
  both <- dplyr::inner_join(contrib, expected, by = c("reporter_iso", "partner_iso", "tech", "supply_chain"))
  expect_equal(nrow(both), nrow(expected))
  expect_equal(both$total, both$fsi_raw)

  # The partner roll-up is one row per partner cell, averaging over reporters.
  expect_false(anyDuplicated(fs$country[c("partner_iso", "tech", "supply_chain")]) > 0)
  jpn <- fs$variable %>%
    dplyr::filter(partner_iso == "JPN", variable == "Friendshore Index", supply_chain == "Upstream")
  expect_equal(
    fs$country$value[fs$country$partner_iso == "JPN" & fs$country$supply_chain == "Upstream"],
    mean(jpn$value)
  )
  expect_equal(fs$country$n_reporters[fs$country$partner_iso == "JPN" & fs$country$supply_chain == "Upstream"], 4)

  # top5 is one row per dyad and variable.
  expect_false(anyDuplicated(op$top5[c("reporter_iso", "partner_iso", "variable")]) > 0)
})

test_that("opportunity reads the renamed partner-need weight, and the legacy key with a warning", {
  cfg <- yaml::read_yaml(file.path(repo_root, "config", "weights.yml"))
  expect_true("partner_es_need" %in% names(cfg$partnership_opportunity_components))
  expect_false("energy_security_index" %in% names(cfg$partnership_opportunity_components))
  expect_silent(w <- partnership_strength_resolve_opportunity_weights(cfg$partnership_opportunity_components))
  expect_equal(w$partner_es_need, cfg$partnership_opportunity_components$partner_es_need)

  expect_warning(
    legacy <- partnership_strength_resolve_opportunity_weights(
      list(trade_index = 3, econ_opp_index = 3, energy_security_index = 1, climate_policy_weight = 0.05)
    ),
    "partner_es_need"
  )
  expect_equal(legacy$partner_es_need, 1)
  expect_null(legacy$energy_security_index)

  index_definition <- yaml::read_yaml(file.path(repo_root, "config", "index_definition.yml"))
  expect_true("partner_es_need" %in% index_definition$partnership_strength$opportunity_components)

  dev_w <- partnership_strength_resolve_weights(
    cfg$partnership_development_components,
    partnership_strength_development_default_weights(),
    "Development component"
  )
  expect_setequal(names(dev_w), names(partnership_strength_development_default_weights()))
})

test_that("development is partner-level: no reporter, no donor fan-out, non-high-income only", {
  ci <- ps_country_info()
  es <- ps_index_tbl("Energy_Security_Index", seq(0.1, 1, length.out = 10))
  eo <- ps_index_tbl("Economic_Opportunity_Index", rev(seq(0.1, 1, length.out = 10)))
  wb_wdi <- tibble::tibble(
    Country.Name = c(ci$country, "World"),
    Country.Code = c(ci$iso3c, "WLD"),
    Series.Name = "GDP per capita (constant 2015 US$)",
    Series.Code = "NY.GDP.PCAP.KD",
    X2022 = c(60000, 35000, 10000, 2000, 3500, 12000),
    X2023 = c(61000, 35500, 11000, 2100, 3600, 12500)
  )
  wb_db <- tibble::tibble(
    Country.Name = ci$country,
    Country.Code = ci$iso3c,
    Series.Code = "IC.BUS.EASE.DFRN.XQ.DB1719",
    X2019 = c(84, 78, 77, 71, 69)
  )
  # Two donors to India's energy sector, and Chinese mining aid that is almost all oil and gas.
  oecd <- tibble::tibble(
    DonorISO = c("USA", "JPN", "JPN", "USA", "USA", "USA", "USA"),
    RecipientISO = c("IND", "IND", "CHN", "CHN", "CHN", "IND", "LDC"),
    Recipient = c("India", "India", "China", "China", "China", "India", "Least developed countries"),
    Sector = c("Energy", "Energy", "Mineral resources and mining", "Oil and gas (upstream)", "Coal",
               "Mineral resources and mining", "Energy"),
    Year = 2022,
    Value = c(10, 5, 100, 80, 5, 20, 1000)
  )
  gdp_data <- tibble::tibble(iso3c = ci$iso3c, year = 2023, NY.GDP.MKTP.CD = c(27, 4, 18, 3.5, 0.4) * 1e12)

  up <- partnership_strength_build_oecd_supplychain(oecd, gdp_data, 2023, valid_iso = ci$iso3c)
  expect_false(anyDuplicated(up[c("iso3c", "supply_chain")]) > 0)
  expect_false("LDC" %in% up$iso3c)
  # Upstream aid net of fossil sub-rows: IND 20 > CHN 100 - 80 - 5 = 15.
  up_stage <- up %>% dplyr::filter(supply_chain == "Upstream")
  expect_gt(up_stage$aid_index_supplychain[up_stage$iso3c == "IND"], up_stage$aid_index_supplychain[up_stage$iso3c == "CHN"])

  dev <- stronger_development(ci, gdp_data, eo, es, wb_wdi, wb_db, oecd)

  expect_false("reporter_iso" %in% names(dev$variable))
  expect_false(anyDuplicated(dev$variable[c("partner_iso", "tech", "supply_chain", "variable")]) > 0)
  expect_setequal(unique(dev$variable$partner_iso), c("CHN", "IND", "VNM"))
  expect_false("us_economic_opportunity" %in% dev$variable$variable)
  idx <- dev$variable %>% dplyr::filter(variable == "Development Potential Index")
  expect_equal(nrow(idx), 3 * 2)
  expect_true(all(idx$value >= 0 & idx$value <= 1))
  expect_false(anyDuplicated(dev$top5[c("partner_iso", "variable")]) > 0)

  # Contributions of the components sum to the available-case weighted mean, and the index is
  # that mean S-curved across partners within (tech, stage).
  w <- unlist(partnership_strength_development_default_weights())
  raw <- dev$variable %>%
    dplyr::filter(variable != "Development Potential Index", !is.na(value)) %>%
    dplyr::mutate(w = unname(w[variable])) %>%
    dplyr::group_by(partner_iso, tech, supply_chain) %>%
    dplyr::summarise(raw = sum(w * value) / sum(w), .groups = "drop")
  sums <- dev$contributions %>%
    dplyr::filter(variable != "Development Potential Index") %>%
    dplyr::group_by(partner_iso, tech, supply_chain) %>%
    dplyr::summarise(total = sum(contribution), .groups = "drop") %>%
    dplyr::inner_join(raw, by = c("partner_iso", "tech", "supply_chain"))
  expect_equal(nrow(sums), nrow(raw))
  expect_equal(sums$total, sums$raw)
  ranked <- raw %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(expected = median_scurve(raw)) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(idx %>% dplyr::select(partner_iso, tech, supply_chain, value), by = c("partner_iso", "tech", "supply_chain"))
  expect_equal(ranked$value, ranked$expected)
})

test_that("WDI debt series score as burdens and non-comparable series are left out", {
  wb_wdi <- tibble::tibble(
    Country.Name = c("A", "A", "B", "B"),
    Country.Code = c("AAA", "AAA", "BBB", "BBB"),
    Series.Name = c("Central government debt", "CPI", "Central government debt", "CPI"),
    Series.Code = c("GC.DOD.TOTL.GD.ZS", "FP.CPI.TOTL", "GC.DOD.TOTL.GD.ZS", "FP.CPI.TOTL"),
    X2023 = c(120, 900, 30, 110)
  )
  wb_db <- tibble::tibble(
    Country.Name = c("A", "B"),
    Country.Code = c("AAA", "BBB"),
    Series.Code = "IC.BUS.EASE.DFRN.XQ.DB1719",
    X2019 = c(70, 70)
  )
  out <- partnership_strength_build_development_indicators(wb_wdi, wb_db)
  # A has the higher debt (and the higher CPI level, which must not count): B scores higher.
  expect_gt(out$development_index[out$iso3c == "BBB"], out$development_index[out$iso3c == "AAA"])
})

test_that("geothermal uses its own IPCC intensity", {
  ghg_raw <- tibble::tibble(
    Tech = c("Coal (pulverised)", "Solar PV - utility", "Geothermal", "Wind onshore"),
    ghg_intensity = c(820, 48, 38, 11)
  )
  out <- partnership_strength_clean_ghg(ghg_raw)
  expect_equal(sum(out$tech == "Geothermal"), 1)
  expect_equal(out$ghg_index[out$tech == "Geothermal"], (820 - 38) / (820 - 11))
})

test_that("PSI averages the pillar indices over the categories a partner has", {
  mk <- function(country, category, variable, value) {
    tibble::tibble(
      Country = country,
      tech = "All",
      supply_chain = "All",
      category = category,
      variable = variable,
      data_type = "index",
      value = value,
      Year = 2024L,
      source = "test",
      explanation = "test"
    )
  }
  theme_tables <- list(
    friendshore = dplyr::bind_rows(
      mk(c("India", "Japan"), "friendshore", "Friendshore Index", c(0.6, 0.8)),
      # A component row: must not enter the category score.
      mk(c("India", "Japan"), "friendshore", "es_need", c(0, 0))
    ),
    opportunity = mk(c("India", "Japan"), "opportunity", "Opportunity Index", c(0.4, 0.6)),
    # Development is defined for India only (Japan is high income).
    development = mk(c("India", "Japan"), "development", "Development Potential Index", c(0.5, NA))
  )
  out <- build_partnership_strength_index(theme_tables, list(friendshore = 0.4, opportunity = 0.4, development = 0.2))
  psi <- stats::setNames(out$index$Partnership_Strength_Index, out$index$Country)

  expect_equal(unname(psi["India"]), 0.4 * 0.6 + 0.4 * 0.4 + 0.2 * 0.5)
  # Not 0.4 * 0.8 + 0.4 * 0.6 = 0.56 (development counted as 0).
  expect_equal(unname(psi["Japan"]), (0.4 * 0.8 + 0.4 * 0.6) / 0.8)
})

test_that("dyadic PSI is one row per (reporter, partner) with the available-case composite", {
  top5 <- function(reporter, partner, variable, value) {
    # The real top5 tables carry a `category` column; it must not mask the builder's argument.
    tibble::tibble(reporter_iso = reporter, partner_iso = partner, category = "x", variable = variable, value = value)
  }
  fs <- top5(c("USA", "USA", "JPN"), c("IND", "JPN", "JPN"), "Friendshore Index", c(0.6, 0.8, 0.9))
  op <- top5(c("USA", "USA"), c("IND", "JPN"), "Opportunity Index", c(0.4, 0.6))
  dv <- tibble::tibble(partner_iso = "IND", category = "development", variable = "Development Potential Index", value = 0.5)

  out <- build_partnership_strength_dyads(fs, op, dv, list(friendshore = 0.4, opportunity = 0.4, development = 0.2))

  expect_equal(nrow(out), 2)
  expect_false(any(out$reporter_iso == out$partner_iso))
  ind <- out[out$partner_iso == "IND", ]
  jpn <- out[out$partner_iso == "JPN", ]
  expect_equal(ind$Partnership_Strength_Index, 0.4 * 0.6 + 0.4 * 0.4 + 0.2 * 0.5)
  expect_equal(jpn$Partnership_Strength_Index, (0.4 * 0.8 + 0.4 * 0.6) / 0.8)
  expect_equal(jpn$n_categories, 2)
})
