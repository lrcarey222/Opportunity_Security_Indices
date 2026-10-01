# Mineral-based Energy Security themes: foreign dependency (Mineral Supply), reserves,
# production and critical-minerals trade.
#
# Guards the failures these themes had: the IEA demand, IEA supply and EI reserve tables name
# graphite and rare earths differently, so every demand-weighted rollup silently dropped
# them; the supply stage recode sent refining to Upstream; and zero shares were ranked
# together with the suppliers, so a listed non-supplier scored mid-table while an unlisted
# one scored 0.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "levels.R"))
source(file.path(repo_root, "R", "utils", "iea_critical_minerals.R"))
source(file.path(repo_root, "R", "categories", "shared", "overall_index.R"))
source(file.path(repo_root, "R", "categories", "reserves", "reserves.R"))
source(file.path(repo_root, "R", "categories", "foreign_dependency", "foreign_dependency.R"))
source(file.path(repo_root, "R", "categories", "foreign_dependency", "critical_minerals_processing.R"))
source(file.path(repo_root, "R", "categories", "production", "critical_minerals_production.R"))
source(file.path(repo_root, "R", "categories", "minerals_trade", "critical_minerals_trade.R"))

# No index definition: the themes' own "Overall ..." rows are not what these tests check.
withr::local_options(list(opportunity_security.index_definition = NULL))

# A miniature IEA critical minerals frame, labelled the way the 2026 workbook labels it.
critical_fixture <- function() {
  demand <- tibble::tribble(
    ~Mineral,                 ~Sector.Country,        ~X2025, ~X2035,
    "Lithium",                "Electric vehicles",        80,     90,
    "Lithium",                "Grid battery storage",     20,     10,
    "Battery-grade graphite", "Electric vehicles",        60,     70,
    "Battery-grade graphite", "Grid battery storage",     40,     30,
    "Neodymium",              "Wind",                     50,     40,
    "Neodymium",              "Electric vehicles",        50,     60,
    "Copper",                 "Wind",                     10,     10,
    "Copper",                 "Electric vehicles",        30,     40,
    "Copper",                 "Electricity networks",     60,     50,
    "Copper",                 "Total copper",            100,    100
  ) %>%
    dplyr::mutate(Pillar = "3.1 Energy demand by tech", Scenario = "Stated Policies scenario")

  supply <- tibble::tribble(
    ~Mineral,                              ~Sector.Country, ~X2025, ~X2035,
    "Lithium - Mining",                    "Australia",         50,     45,
    "Lithium - Mining",                    "Chile",             30,     30,
    "Lithium - Mining",                    "Rest of world",     20,     25,
    "Lithium - Refining",                  "China",             70,     60,
    "Lithium - Refining",                  "Chile",             20,     25,
    "Lithium - Refining",                  "Rest of world",     10,     15,
    "Graphite - Mining (natural)",         "China",             80,     75,
    "Graphite - Mining (natural)",         "Mozambique",        10,     15,
    "Graphite - Mining (natural)",         "Rest of world",     10,     10,
    "Graphite - Battery grade",            "China",             95,     90,
    "Graphite - Battery grade",            "Japan",              5,     10,
    "Magnet rare earth elements - Mining", "China",             60,     55,
    "Magnet rare earth elements - Mining", "Myanmar",           20,     20,
    "Magnet rare earth elements - Mining", "Rest of world",     20,     25,
    "Copper - Mining",                     "Chile",             50,     45,
    "Copper - Mining",                     "Peru",              30,     30,
    "Copper - Mining",                     "Rest of world",     20,     25
  ) %>%
    dplyr::mutate(Pillar = "2 Total supply for key minerals", Scenario = "Base case")

  as.data.frame(dplyr::bind_rows(demand, supply))
}

roster_fixture <- function() {
  tibble::tibble(
    Country = c("Australia", "Chile", "China", "Mozambique", "Japan", "Myanmar", "Peru", "Germany")
  )
}

test_that("mineral names from every source meet on one canonical name", {
  expect_equal(
    harmonize_mineral_names(c(
      "Battery-grade graphite", "Natural Graphite", "Graphite (all grades: natural and synthetic)",
      "Neodymium", "Magnet rare earth elements", "Rare Earth metals", "rare-earth metals",
      "PGMs", "PGMs (other than iridum)", "platinum", "copper", " Lithium ", NA, ""
    )),
    c(
      "Graphite", "Graphite", "Graphite",
      "Rare Earths", "Rare Earths", "Rare Earths", "Rare Earths",
      "PGMs", "PGMs", "PGMs", "Copper", "Lithium", NA, NA
    )
  )
})

test_that("mining is Upstream and refining/processing is Midstream", {
  expect_equal(
    iea_critical_minerals_supply_stage(c(
      "Mining", "Refining", "Mining (natural)", "Battery grade", "Chemical", "Processing"
    )),
    c("Upstream", "Midstream", "Upstream", "Midstream", "Midstream", "Midstream")
  )
})

test_that("share index scores zero shares 0 and ranks positive shares above them", {
  idx <- iea_critical_minerals_share_index(c(0, 0, 0, 0.1, 0.3, 0.6, NA))

  expect_equal(idx[1:3], c(0, 0, 0))
  expect_true(all(idx[4:6] > 0))
  expect_true(idx[4] < idx[5] && idx[5] < idx[6])
  expect_equal(idx[6], 1)
  expect_true(is.na(idx[7]))
  expect_equal(iea_critical_minerals_share_index(c(0, 0)), c(0, 0))
})

test_that("demand shares carry the canonical mineral names", {
  demand <- reserves_build_mineral_demand_clean(critical_fixture())

  expect_setequal(unique(demand$Mineral), c("Lithium", "Graphite", "Rare Earths", "Copper"))
  expect_false(any(grepl("Total", demand$`Sector.Country`)))
  wind_ree <- demand %>% dplyr::filter(Mineral == "Rare Earths", tech == "Wind")
  expect_equal(wind_ree$share_24, 0.5)
})

test_that("Mineral Supply splits mining and refining and keeps graphite and rare earths", {
  critical <- critical_fixture()
  demand <- reserves_build_mineral_demand_clean(critical)

  out <- foreign_dependency_build_mineral_supply(
    critical = critical,
    mineral_demand_clean = demand,
    country_reference = roster_fixture()
  ) %>%
    dplyr::ungroup()

  expect_setequal(unique(out$supply_chain), c("Upstream", "Midstream"))
  value_of <- function(country, tech_, stage) {
    out$value[out$Country == country & out$tech == tech_ & out$supply_chain == stage]
  }

  # Rare earths reach Wind: Myanmar mines nothing but magnet rare earths.
  expect_gt(value_of("Myanmar", "Wind", "Upstream"), value_of("Germany", "Wind", "Upstream"))
  # Graphite reaches Batteries: Mozambique mines only natural graphite.
  expect_gt(value_of("Mozambique", "Batteries", "Upstream"), value_of("Germany", "Batteries", "Upstream"))
  # Refining is Midstream only: Japan refines battery-grade graphite and mines nothing.
  expect_gt(value_of("Japan", "Batteries", "Midstream"), value_of("Germany", "Batteries", "Midstream"))
  expect_equal(value_of("Japan", "Batteries", "Upstream"), value_of("Germany", "Batteries", "Upstream"))
  # China refines lithium: its Midstream score comes from refining, not ore.
  expect_gt(value_of("China", "Batteries", "Midstream"), value_of("Australia", "Batteries", "Midstream"))
})

test_that("non-suppliers score alike whether or not the IEA lists them, below every supplier", {
  critical <- critical_fixture()
  demand <- reserves_build_mineral_demand_clean(critical)

  out <- foreign_dependency_build_mineral_supply(
    critical = critical,
    mineral_demand_clean = demand,
    country_reference = roster_fixture()
  ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(tech == "Batteries", supply_chain == "Upstream")

  # Japan is listed (for refining) but mines nothing; Germany is not listed at all.
  japan <- out$value[out$Country == "Japan"]
  germany <- out$value[out$Country == "Germany"]
  expect_equal(japan, germany)
  expect_gt(germany, 0)

  miners <- out$value[out$Country %in% c("Australia", "Chile", "China", "Mozambique")]
  expect_true(all(miners > germany))
})

test_that("a listed supplier outside the roster is completed with zero supply too", {
  critical <- critical_fixture()
  demand <- reserves_build_mineral_demand_clean(critical)
  supply_of <- function(roster) {
    foreign_dependency_build_mineral_supply(
      critical = critical,
      mineral_demand_clean = demand,
      country_reference = roster
    ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(Country == "Myanmar", supply_chain == "Upstream") %>%
      dplyr::arrange(tech) %>%
      dplyr::pull(value)
  }

  # Myanmar mines only rare earths. Off the roster it used to be scored on rare earths alone
  # (no zero rows for copper), which put it above the same supplier on the roster.
  in_roster <- supply_of(roster_fixture())
  off_roster <- supply_of(dplyr::filter(roster_fixture(), Country != "Myanmar"))
  # Scored on every Upstream tech (Batteries, Electric Grid, EV, Wind), not only the two
  # that use rare earths.
  expect_length(off_roster, 4)
  expect_equal(off_roster, in_roster)
})

test_that("critical_minerals_processing scores the same way and completes its roster", {
  critical <- critical_fixture()
  demand <- reserves_build_mineral_demand_clean(critical)
  country_info <- tibble::tibble(country = c("Australia", "Chile", "China", "Japan", "Peru"))

  out <- critical_minerals_processing(
    critical = critical,
    mineral_demand_clean = demand,
    country_info = country_info,
    country_reference = roster_fixture()
  ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(tech == "Batteries", supply_chain == "Upstream")

  # Germany and Myanmar come from the roster only; neither mines a battery mineral.
  expect_equal(out$value[out$Country == "Germany"], out$value[out$Country == "Japan"])
  expect_gt(out$value[out$Country == "Germany"], 0)
  expect_gt(out$value[out$Country == "Australia"], out$value[out$Country == "Germany"])
})

test_that("foreign_dependency_ei_countries keeps one row per economy", {
  ei <- tibble::tibble(
    Country = c("Cyprus", "Cyprus", "US", "USSR", "Total Europe", "Germany", "Vietnam"),
    Year = 2025,
    Var = c("pop", "oil_tes_ej", "pop", "pop", "pop", "pop", "pop"),
    Value = 1,
    EU = c(0, 1, 0, 0, 0, 1, 0),
    SubRegion = c("Europe", "Europe", "North America", "CIS", "", "Europe", "Asia Pacific")
  )

  out <- foreign_dependency_ei_countries(ei, year = 2025)

  expect_setequal(out$Country, c("Cyprus", "United States", "Germany", "Viet Nam"))
  expect_equal(out$EU[out$Country == "Cyprus"], 1L)
  expect_equal(nrow(out), dplyr::n_distinct(out$Country))
})

# ---- Reserves ------------------------------------------------------------------------------

reserve_ei_fixture <- function() {
  tibble::tibble(
    Country = c("Chile", "China", "Australia", "Venezuela", "Russian Federation", "USSR", "Total World"),
    Year = 2025,
    ISO3166_alpha3 = c("CHL", "CHN", "AUS", "VEN", "RUS", "SUN", NA)
  )
}

reserve_inputs_fixture <- function() {
  list(
    list(
      sheet = "Oil - Proved reserves history", nm_col = "Thousand million barrels",
      val_col = "2020...42", tech_name = "Oil", unit_desc = "Thousand million barrels",
      data_year = 2020,
      data = tibble::tibble(
        `Thousand million barrels` = c(
          "Venezuela", "Venezuela: Orinoco Belt", "Russian Federation", "China",
          "Total World",
          "Gas Journal and an independent estimates of Russian reserves based on official data."
        ),
        `2020...42` = c(303.8, 261.8, 107.8, 26.0, 1732.4, NA)
      )
    ),
    list(
      sheet = "Copper P-R", nm_col = "Thousand tonnes", val_col = "At end of 2025",
      tech_name = "Copper", unit_desc = "Thousand tonnes",
      data = tibble::tibble(
        `Thousand tonnes` = c("Chile", "Australia", "China", "Rest of World", "Total World"),
        `At end of 2025` = c(190000, 100000, 41000, 500000, 831000)
      )
    ),
    list(
      sheet = "Rare Earth metals P-R", nm_col = "Thousand tonnes1", val_col = "At end of 2025",
      tech_name = "Rare Earths", unit_desc = "Thousand tonnes",
      data = tibble::tibble(
        `Thousand tonnes1` = c("China", "Australia", "Rest of World", "Total World"),
        `At end of 2025` = c(44000, 5700, 40000, 89700)
      )
    )
  )
}

test_that("reserve sub-rows and footnotes do not give a country a second row", {
  out <- suppressWarnings(reserves(
    ei = reserve_ei_fixture(),
    reserve_inputs = reserve_inputs_fixture(),
    mineral_demand_clean = reserves_build_mineral_demand_clean(critical_fixture())
  )) %>%
    dplyr::ungroup()

  oil_raw <- out %>% dplyr::filter(variable == "Oil Reserves", data_type == "raw", !is.na(value))
  expect_equal(nrow(oil_raw), dplyr::n_distinct(oil_raw$ISO3166_alpha3))
  expect_equal(oil_raw$value[oil_raw$Country == "Venezuela"], 303.8)
  expect_equal(oil_raw$value[oil_raw$Country == "Russia"], 107.8)
  # USSR is EI's CIS aggregate, not a reporting country.
  expect_false(any(out$Country == "USSR", na.rm = TRUE))
  # Fossil reserves stopped at end-2020; the explanation says so.
  expect_true(all(grepl("end-2020", oil_raw$explanation)))
})

test_that("tech reserves weight rare earths and count unlisted minerals as zero", {
  out <- suppressWarnings(reserves(
    ei = reserve_ei_fixture(),
    reserve_inputs = reserve_inputs_fixture(),
    mineral_demand_clean = reserves_build_mineral_demand_clean(critical_fixture())
  )) %>%
    dplyr::ungroup()

  expect_true(any(out$variable == "Rare Earths Reserves"))

  wind <- out %>%
    dplyr::filter(variable == "Wind Reserves", data_type == "index", !is.na(value))
  # Chile holds the most copper but no rare earths; with unlisted minerals dropped it used to
  # score its copper rank for the whole of Wind.
  expect_gt(wind$value[wind$Country == "China"], wind$value[wind$Country == "Chile"])
  # The smallest holder stays above the 0 of a country with no reserves.
  expect_true(all(wind$value > 0))
})

# ---- Production ----------------------------------------------------------------------------

production_inputs_fixture <- function() {
  sheet <- function(names_col, countries, values) {
    out <- tibble::tibble(countries, values * 0.9, values)
    names(out) <- c(names_col, "2024", "2025")
    out
  }
  list(
    list(
      sheet = "Copper P-R", skip = 2, nm_col = "Thousand tonnes", tech_name = "Copper",
      unit_desc = "Thousand tonnes",
      data = sheet("Thousand tonnes", c("Chile", "Peru", "China", "Rest of World"), c(5300, 2600, 1800, 9000))
    ),
    list(
      sheet = "Rare Earth metals P-R", skip = 2, nm_col = "Thousand tonnes1", tech_name = "Rare Earths",
      unit_desc = "Thousand tonnes",
      data = sheet("Thousand tonnes1", c("China", "Myanmar", "Rest of World"), c(270, 31, 60))
    )
  )
}

test_that("rare-earth production reaches Wind and producers stay above non-producers", {
  out <- critical_minerals_production(
    production_inputs = production_inputs_fixture(),
    mineral_demand_clean = reserves_build_mineral_demand_clean(critical_fixture()),
    country_reference = roster_fixture()
  ) %>%
    dplyr::ungroup()

  wind <- out %>% dplyr::filter(variable == "Wind Production", data_type == "index")
  value_of <- function(country) wind$value[wind$Country == country]

  # Myanmar produces only rare earths; before the harmonized join it scored 0 on Wind.
  expect_gt(value_of("Myanmar"), 0)
  expect_gt(value_of("China"), value_of("Chile"))
  expect_equal(value_of("Germany"), 0)
  expect_true(all(wind$value[wind$Country %in% c("Chile", "Peru", "China", "Myanmar")] > 0))
})

# ---- Critical-minerals trade ---------------------------------------------------------------

trade_fixture <- function() {
  flows <- function(reporters, codes, descs, values) {
    tibble::tibble(
      reporter_iso = reporters, reporter_desc = reporters, cmd_code = codes,
      cmd_desc = descs, primary_value = values, ref_year = 2024
    )
  }
  li <- "Lithium oxide and hydroxide"
  cu <- "Copper ores and concentrates"
  gr <- "Graphite; natural, in powder or in flakes"
  list(
    # Chile reports no imports of lithium at all: an import-led join dropped its exports.
    imports = flows(c("CHN", "CHN", "DEU", "DEU"), c("282520", "260300", "282520", "250410"),
                    c(li, cu, li, gr), c(50, 80, 20, 5)),
    exports = flows(c("CHL", "CHL", "CHN", "AUS"), c("282520", "260300", "250410", "282520"),
                    c(li, cu, gr, li), c(60, 90, 40, 30)),
    total = tibble::tibble(reporter_iso = c("CHL", "CHN", "AUS", "DEU"), primary_value = c(1000, 5000, 800, 3000)),
    country_info = tibble::tibble(
      iso3c = c("CHL", "CHN", "AUS", "DEU", "FRA"),
      country = c("Chile", "China", "Australia", "Germany", "France")
    )
  )
}

test_that("minerals trade keeps export-only flows and emits market share and HHI terms", {
  fx <- trade_fixture()

  out <- critical_minerals_trade(
    critmin_import = fx$imports,
    critmin_export = fx$exports,
    total_export = fx$total,
    mineral_demand_clean = reserves_build_mineral_demand_clean(critical_fixture()),
    country_info = fx$country_info
  ) %>%
    dplyr::ungroup()

  expect_equal(unique(out$Year), 2024L)
  expect_true(all(c("critmin_market_share", "critmin_hhi", "critmin_export_share") %in% out$variable))
  expect_false(any(out$variable == "critmin_trade"))

  ms <- out %>% dplyr::filter(variable == "critmin_market_share", data_type == "index", tech == "Electric Vehicles")
  # Chile's lithium and copper exports survive although it reports no lithium imports.
  expect_gt(ms$value[ms$Country == "Chile"], 0)
  # Germany imports but exports nothing: 0, the same as a country outside Comtrade.
  expect_equal(ms$value[ms$Country == "Germany"], 0)

  hhi <- out %>% dplyr::filter(variable == "critmin_hhi", data_type == "index")
  # A world-market term: one value per tech, emitted for every country (France reports nothing).
  expect_true("France" %in% hhi$Country)
  expect_equal(
    hhi %>% dplyr::group_by(tech) %>% dplyr::summarize(n = dplyr::n_distinct(value)) %>% dplyr::pull(n),
    rep(1L, dplyr::n_distinct(hhi$tech))
  )
})

test_that("demand weights are renormalized over the minerals that have trade data", {
  demand <- reserves_build_mineral_demand_clean(critical_fixture())

  weights <- critical_minerals_trade_build_weights(demand, traded_minerals = c("Lithium", "Copper", "Graphite"))

  # Rare earths have no HS codes in the pull; the EV weights still sum to one, for everyone.
  ev <- weights %>% dplyr::filter(tech == "Electric Vehicles")
  expect_false("Rare Earths" %in% ev$mineral)
  expect_equal(sum(ev$weight), 1)
})
