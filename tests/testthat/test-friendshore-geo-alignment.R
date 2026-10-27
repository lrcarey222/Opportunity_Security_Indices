# The geo_alignment component of the Friendshore theme (R/themes/partnership_strength/
# safer_friendshore.R): a weight of 0 leaves the index unchanged, a missing alignment drops out
# of the weighted mean instead of scoring 0, and the component reaches the contribution tables.

fsg_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(fsg_root, "R", "utils", "scurve.R"))
source(file.path(fsg_root, "R", "utils", "country.R"))
source(file.path(fsg_root, "R", "utils", "schema.R"))
source(file.path(fsg_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
# Trade cleaning and dyad series, shared with the Opportunity theme.
source(file.path(fsg_root, "R", "themes", "partnership_strength", "prosperous_opportunity.R"))
source(file.path(fsg_root, "R", "themes", "partnership_strength", "safer_friendshore.R"))

`%>%` <- dplyr::`%>%`

# Five economies, one tech in two stages (the fixture of test-partnership-strength.R).
fsg_country_info <- function() {
  tibble::tibble(
    country = c("United States", "Japan", "China", "India", "Viet Nam"),
    iso3c = c("USA", "JPN", "CHN", "IND", "VNM"),
    income = c("High income", "High income", "Upper middle income", "Lower middle income", "Lower middle income")
  )
}

fsg_index_tbl <- function(value_col, values) {
  tbl <- tidyr::expand_grid(
    Country = c("United States", "Japan", "China", "India", "Viet Nam"),
    tech = "Solar",
    supply_chain = c("Extraction", "Assembly")
  )
  tbl[[value_col]] <- values
  tbl
}

fsg_inputs <- function() {
  list(
    comtrade = tibble::tibble(
      reporter_iso = "USA",
      partner_iso = c("JPN", "CHN", "JPN", "CHN"),
      flow_direction = c("Import", "Import", "Export", "Export"),
      period = c(2020, 2024, 2020, 2024),
      cmd_code = "854140",
      primary_value = c(10, 20, 5, 8)
    ),
    subcat = tibble::tibble(tech = "Solar", supply_chain = "Assembly", code = "854140"),
    eo = fsg_index_tbl("Economic_Opportunity_Index", rev(seq(0.1, 1, length.out = 10))),
    es = fsg_index_tbl("Energy_Security_Index", seq(0.1, 1, length.out = 10)),
    tech_ghg = tibble::tibble(tech = "Solar", ghg_index = 0.9),
    policy = tibble::tibble(Country = "Japan", climate_policy_index = 0.75),
    fdi_raw = tibble::tibble(
      COUNTRY = "United States",
      COUNTERPART_COUNTRY = c("Japan", "India"),
      DV_TYPE = "Reported official data",
      INDICATOR = "Outward Direct investment, Assets (gross), Debt instruments, All entities",
      X2022 = c(100, 10)
    ),
    country_info = fsg_country_info(),
    gdp = tibble::tibble(
      iso3c = fsg_country_info()$iso3c, year = 2023, NY.GDP.MKTP.CD = c(27, 4, 18, 3.5, 0.4) * 1e12
    )
  )
}

# Alignment for every ordered pair of the five, except: USA-CHN and CHN-USA are NA (as for a
# country without a UN vote) and USA-VNM / VNM-USA are absent from the table.
fsg_alignment <- function() {
  iso <- fsg_country_info()$iso3c
  p <- c(USA = 1, JPN = 0.9, CHN = 0, IND = 0.5, VNM = 0.1)
  pairs <- expand.grid(reporter_iso = iso, partner_iso = iso, stringsAsFactors = FALSE)
  pairs <- pairs[pairs$reporter_iso != pairs$partner_iso, ]
  pi <- p[pairs$reporter_iso]
  pj <- p[pairs$partner_iso]
  pairs$geo_alignment <- unname(pi * pj + (1 - pi) * (1 - pj))
  us_cn <- (pairs$reporter_iso == "USA" & pairs$partner_iso == "CHN") |
    (pairs$reporter_iso == "CHN" & pairs$partner_iso == "USA")
  pairs$geo_alignment[us_cn] <- NA_real_
  us_vn <- (pairs$reporter_iso == "USA" & pairs$partner_iso == "VNM") |
    (pairs$reporter_iso == "VNM" & pairs$partner_iso == "USA")
  tibble::as_tibble(pairs[!us_vn, ])
}

fsg_weights <- function(geo) {
  # chinese_parent_exposure (CM-FDI) has no input here, so it is NA in every dyad and drops out.
  list(imp_trade_index = 2, econ_opp_raw = 0, es_need = 2, eo_partner = 2, outbound_index = 2, geo_alignment = geo,
       chinese_parent_exposure = 1)
}

fsg_run <- function(geo_weight, geo_alignment) {
  x <- fsg_inputs()
  suppressMessages(safer_friendshore(
    x$comtrade, x$subcat, x$eo, x$es, x$tech_ghg, x$policy, x$fdi_raw, x$country_info, x$gdp,
    component_weights = fsg_weights(geo_weight),
    techs = "Solar",
    geo_alignment = geo_alignment
  ))
}

fsg_dyads <- function(geo_weight, geo_alignment) {
  x <- fsg_inputs()
  partnership_strength_build_friendshore_dyads(
    tibble::tibble(reporter_iso = "USA", partner_iso = "JPN", tech = "Solar", supply_chain = "Extraction", imp_trade_index = 0.9),
    x$eo, x$es,
    tibble::tibble(reporter_iso = "USA", partner_iso = "JPN", outbound_index = 0.6),
    tibble::tibble(tech = "Solar", ghg_index = 1),
    x$policy, x$country_info,
    component_weights = fsg_weights(geo_weight),
    geo_alignment = geo_alignment
  )
}

test_that("geo_alignment weight 0 reproduces the Friendshore without the component exactly", {
  geo <- fsg_alignment()
  with0 <- fsg_dyads(0, geo)
  without <- fsg_dyads(0, NULL)
  keep <- setdiff(names(with0), "geo_alignment")
  expect_identical(with0[keep], without[keep])
  # The alignment values are carried but do not enter the score.
  expect_false(all(is.na(with0$geo_alignment)))
  # And the score is the five-component available-case mean of the code before the change.
  expect_equal(
    with0$fsi_raw,
    partnership_strength_row_weighted_mean(
      list(with0$imp_trade_index, with0$econ_opp_raw, with0$es_need, with0$eo_partner, with0$outbound_index),
      c(2, 0, 2, 2, 2)
    )
  )

  # Through safer_friendshore(): every table matches once the geo_alignment rows are set aside.
  fs0 <- fsg_run(0, geo)
  fs_none <- fsg_run(0, NULL)
  drop_geo <- function(tbl) dplyr::filter(tbl, variable != "geo_alignment")
  expect_identical(drop_geo(fs0$variable), drop_geo(fs_none$variable))
  expect_identical(drop_geo(fs0$contributions), drop_geo(fs_none$contributions))
  expect_identical(drop_geo(fs0$top5), drop_geo(fs_none$top5))
  expect_identical(fs0$country, fs_none$country)
})

test_that("a missing alignment drops out of the weighted mean instead of counting as 0", {
  geo <- fsg_alignment()
  with2 <- fsg_dyads(2, geo)
  without <- fsg_dyads(2, NULL)
  pick <- function(tbl, r, p, s = "Extraction") tbl[tbl$reporter_iso == r & tbl$partner_iso == p & tbl$supply_chain == s, ]

  # USA-JPN has an alignment: it enters with weight 2 next to the four other components.
  usa_jpn <- pick(with2, "USA", "JPN")
  expect_equal(usa_jpn$geo_alignment, 0.9)
  expect_equal(
    usa_jpn$fsi_raw,
    (2 * 0.9 + 2 * usa_jpn$es_need + 2 * usa_jpn$eo_partner + 2 * 0.6 + 2 * 0.9) / 10
  )

  # USA-CHN (NA in the table) and USA-VNM (not in the table): scored on the other components,
  # exactly as without an alignment table, and above the value a 0 would give.
  for (p in c("CHN", "VNM")) {
    got <- pick(with2, "USA", p)
    ref <- pick(without, "USA", p)
    expect_true(is.na(got$geo_alignment))
    expect_equal(got$fsi_raw, ref$fsi_raw)
    num <- 2 * got$imp_trade_index + 2 * got$es_need + 2 * got$eo_partner
    expect_equal(got$fsi_raw, num / 6)
    expect_gt(got$fsi_raw, num / 8)
  }

  # The component is the same for every (tech, stage) of a dyad.
  expect_equal(pick(with2, "JPN", "IND", "Extraction")$geo_alignment, pick(with2, "JPN", "IND", "Assembly")$geo_alignment)

  # Bad input is refused rather than joined.
  bad <- geo
  bad$geo_alignment[1] <- 1.5
  expect_error(fsg_dyads(2, bad), "\\[0, 1\\]")
  expect_error(fsg_dyads(2, rbind(geo, geo[1, ])), "Duplicate")
})

test_that("geo_alignment appears in the variable, contribution and inputs tables", {
  geo <- fsg_alignment()
  fs <- fsg_run(2, geo)

  expect_true("geo_alignment" %in% fs$variable$variable)
  expect_true("geo_alignment" %in% fs$contributions$variable)
  expect_true("geo_alignment" %in% fs$top5$variable)
  # NA alignments carry no contribution row.
  expect_false(any(fs$contributions$variable == "geo_alignment" &
                     fs$contributions$reporter_iso == "USA" & fs$contributions$partner_iso %in% c("CHN", "VNM")))

  # Contributions are weight / available weight x value and add up to fsi_raw.
  jpn <- fs$contributions %>%
    dplyr::filter(reporter_iso == "USA", partner_iso == "JPN", supply_chain == "Assembly")
  parts <- jpn %>% dplyr::filter(variable != "Friendshore Index")
  w <- unlist(fsg_weights(2))
  expect_equal(
    parts$contribution[parts$variable == "geo_alignment"],
    0.9 * 2 / sum(w[parts$variable])
  )
  values <- stats::setNames(parts$value, parts$variable)
  expect_equal(sum(parts$contribution), sum(w[names(values)] * values) / sum(w[names(values)]))

  # The unused top-3 inputs roll-up lists it too.
  dyads <- fsg_dyads(2, geo)
  inputs <- partnership_strength_build_friendshore_inputs_country(dyads, fsg_country_info(), fsg_weights(2))
  expect_true("geo_alignment" %in% inputs$variable)
  expect_equal(sum(unique(inputs[c("variable", "component_weight_share")])$component_weight_share), 1)

  # config/weights.yml gives it weight 2, like the other active components.
  cfg <- yaml::read_yaml(file.path(fsg_root, "config", "weights.yml"))
  expect_equal(cfg$partnership_friendshore_components$geo_alignment, 2)
  expect_setequal(names(cfg$partnership_friendshore_components), partnership_strength_friendshore_components())
})
