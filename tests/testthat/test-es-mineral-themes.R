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
source(file.path(repo_root, "R", "utils", "supply_chain_stages.R"))
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

test_that("mining is Extraction and refining/processing is Refining", {
  labels <- c("Mining", "Refining", "Mining (natural)", "Battery grade", "Chemical", "Processing")
  expect_equal(
    iea_critical_minerals_supply_stage(labels, legacy = FALSE),
    c("Extraction", "Refining", "Extraction", "Refining", "Refining", "Refining")
  )
  # The legacy labels R/indices/coalition.R still keys its mining and refining measures on:
  # the old classification (refining was Midstream), not the six-stage roll-up.
  expect_equal(
    iea_critical_minerals_supply_stage(labels),
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

  expect_setequal(unique(out$supply_chain), c("Extraction", "Refining"))
  value_of <- function(country, tech_, stage) {
    out$value[out$Country == country & out$tech == tech_ & out$supply_chain == stage]
  }

  # Rare earths reach Wind: Myanmar mines nothing but magnet rare earths.
  expect_gt(value_of("Myanmar", "Wind", "Extraction"), value_of("Germany", "Wind", "Extraction"))
  # Graphite reaches Batteries: Mozambique mines only natural graphite.
  expect_gt(value_of("Mozambique", "Batteries", "Extraction"), value_of("Germany", "Batteries", "Extraction"))
  # Refining is Refining only: Japan refines battery-grade graphite and mines nothing.
  expect_gt(value_of("Japan", "Batteries", "Refining"), value_of("Germany", "Batteries", "Refining"))
  expect_equal(value_of("Japan", "Batteries", "Extraction"), value_of("Germany", "Batteries", "Extraction"))
  # China refines lithium: its Refining score comes from refining, not ore.
  expect_gt(value_of("China", "Batteries", "Refining"), value_of("Australia", "Batteries", "Refining"))
  # The explanation says which supply the stage scores.
  expect_true(all(grepl("refined supply", out$explanation[out$supply_chain == "Refining"])))
  expect_true(all(grepl("mined supply", out$explanation[out$supply_chain == "Extraction"])))
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
    dplyr::filter(tech == "Batteries", supply_chain == "Extraction")

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
      dplyr::filter(Country == "Myanmar", supply_chain == "Extraction") %>%
      dplyr::arrange(tech) %>%
      dplyr::pull(value)
  }

  # Myanmar mines only rare earths. Off the roster it used to be scored on rare earths alone
  # (no zero rows for copper), which put it above the same supplier on the roster.
  in_roster <- supply_of(roster_fixture())
  off_roster <- supply_of(dplyr::filter(roster_fixture(), Country != "Myanmar"))
  # Scored on every Extraction tech (Batteries, Electric Grid, EV, Wind), not only the two
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
    dplyr::filter(tech == "Batteries", supply_chain == "Extraction")

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
  # Reserves are a resource endowment: Extraction, for the fuels and the mineral rollups alike.
  expect_setequal(unique(out$supply_chain), "Extraction")
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
  # Mine production only: every row is Extraction.
  expect_setequal(unique(out$supply_chain), "Extraction")
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
  ms_of <- function(country, stage) ms$value[ms$Country == country & ms$supply_chain == stage]
  # Chile's lithium hydroxide (Refining) and copper ore (Extraction) exports survive although
  # it reports no lithium imports.
  expect_gt(ms_of("Chile", "Refining"), 0)
  expect_gt(ms_of("Chile", "Extraction"), 0)
  # Germany imports but exports nothing: 0, the same as a country outside Comtrade.
  expect_equal(ms_of("Germany", "Refining"), 0)
  expect_equal(ms_of("Germany", "Extraction"), 0)
  expect_setequal(unique(out$supply_chain), c("Extraction", "Refining"))

  hhi <- out %>% dplyr::filter(variable == "critmin_hhi", data_type == "index")
  # A world-market term: one value per tech and stage, emitted for every country (France
  # reports nothing).
  expect_true("France" %in% hhi$Country)
  expect_equal(
    hhi %>% dplyr::group_by(tech, supply_chain) %>% dplyr::summarize(n = dplyr::n_distinct(value), .groups = "drop") %>% dplyr::pull(n),
    rep(1L, nrow(dplyr::distinct(hhi, tech, supply_chain)))
  )
})

test_that("each HS6 code is staged by what the good is", {
  codes <- tibble::tribble(
    ~cmd_code, ~cmd_desc,                                                                   ~stage,
    "250410",  "Graphite; natural, in powder or in flakes",                                  "Extraction",
    "260300",  "Copper ores and concentrates",                                               "Extraction",
    "261390",  "Molybdenum ores and concentrates; other than roasted",                       "Extraction",
    "282520",  "Lithium oxide and hydroxide",                                                "Refining",
    "283691",  "Carbonates; lithium carbonate",                                              "Refining",
    "380110",  "Graphite; artificial",                                                       "Refining",
    "720260",  "Ferro-alloys; ferro-nickel",                                                 "Refining",
    "740311",  "Copper; refined, unwrought, cathodes and sections of cathodes",              "Refining",
    "740811",  "Copper; wire, of refined copper, of which the maximum cross-sectional dimension exceeds 6mm", "Refining",
    "741011",  "Copper; foil, not backed, of a thickness not exceeding 0.15mm, of refined copper", "Refining",
    "750110",  "Nickel; nickel mattes",                                                      "Refining",
    "790500",  "Zinc; plates, sheets, strip and foil",                                       "Refining",
    "810520",  "Cobalt; mattes and other intermediate products of cobalt metallurgy, unwrought cobalt, powders", "Refining",
    # A combined heading that only includes scrap is mostly unwrought metal; "other than"
    # scrap is not scrap.
    "811100",  "Manganese; articles thereof, including waste and scrap",                     "Refining",
    "811229",  "Chromium and articles thereof; wrought other than waste and scrap",          "Refining",
    "262030",  "Slag, ash and residues; (not from the manufacture of iron or steel), containing mainly copper", "Recovery",
    "740400",  "Copper; waste and scrap",                                                    "Recovery",
    "750300",  "Nickel; waste and scrap",                                                    "Recovery",
    "790200",  "Zinc; waste and scrap",                                                      "Recovery",
    "810530",  "Cobalt; waste and scrap",                                                    "Recovery",
    "811222",  "Chromium; waste and scrap",                                                  "Recovery",
    # Outside the chapters the classifier knows: no guess.
    "854140",  "Photosensitive semiconductor devices",                                       NA_character_
  )
  expect_identical(critical_minerals_trade_stage(codes$cmd_code, codes$cmd_desc), codes$stage)

  # A matched code with no stage stops the theme rather than leaving its exports out.
  fx <- trade_fixture()
  odd <- dplyr::mutate(fx$exports[1, ], cmd_code = "854140", cmd_desc = "Copper-indium-gallium cells")
  expect_error(
    critical_minerals_trade_build_trade(
      critmin_import = fx$imports, critmin_export = dplyr::bind_rows(fx$exports, odd),
      country_info = fx$country_info,
      minerals_pattern = critical_minerals_trade_build_minerals_pattern(reserves_build_mineral_demand_clean(critical_fixture()))
    ),
    "no supply-chain stage for HS code\\(s\\) 854140"
  )
})

test_that("ores, refined copper and copper scrap are separate markets with their own weights", {
  flows <- function(reporters, codes, descs, values) {
    tibble::tibble(
      reporter_iso = reporters, reporter_desc = reporters, cmd_code = codes,
      cmd_desc = descs, primary_value = values, ref_year = 2024
    )
  }
  ore <- "Copper ores and concentrates"
  cathode <- "Copper; refined, unwrought, cathodes and sections of cathodes"
  wire <- "Copper; wire, of refined copper, of which the maximum cross-sectional dimension exceeds 6mm"
  scrap <- "Copper; waste and scrap"
  li <- "Lithium oxide and hydroxide"
  # Chile ships ore, China refined copper and wire, Germany scrap; Australia lithium hydroxide.
  exports <- flows(
    c("CHL", "CHL", "CHN", "CHN", "DEU", "AUS"),
    c("260300", "740311", "740311", "740811", "740400", "282520"),
    c(ore, cathode, cathode, wire, scrap, li),
    c(90, 10, 60, 30, 50, 40)
  )
  imports <- flows("CHN", "260300", ore, 100)
  total <- tibble::tibble(reporter_iso = c("CHL", "CHN", "AUS", "DEU"), primary_value = c(1000, 5000, 800, 3000))
  country_info <- trade_fixture()$country_info
  demand <- reserves_build_mineral_demand_clean(critical_fixture())

  fl <- critical_minerals_trade_flows(
    critmin_import = imports, critmin_export = exports, total_export = total,
    mineral_demand_clean = demand, country_info = country_info
  )
  cu <- fl %>% dplyr::filter(mineral == "Copper")
  share <- function(iso, stage) cu$market_share[cu$reporter_iso == iso & cu$supply_chain == stage]
  # One row per exporter, mineral and stage; each stage is its own world market.
  expect_equal(nrow(dplyr::count(fl, reporter_iso, mineral, supply_chain) %>% dplyr::filter(n > 1)), 0)
  expect_setequal(unique(cu$supply_chain), c("Extraction", "Refining", "Recovery"))
  expect_equal(share("CHL", "Extraction"), 1)
  expect_equal(share("CHL", "Refining"), 10 / 100)
  expect_equal(share("CHN", "Refining"), 90 / 100)
  expect_equal(share("DEU", "Recovery"), 1)
  expect_equal(share("DEU", "Refining"), 0)
  expect_equal(unique(cu$HHI[cu$supply_chain == "Refining"]), 0.1^2 + 0.9^2)
  # The HS6 codes behind each pair: cathodes and wire are two Refining codes.
  expect_equal(unique(cu$n_hs6[cu$supply_chain == "Refining"]), 2L)
  expect_equal(unique(cu$n_hs6[cu$supply_chain == "Extraction"]), 1L)

  out <- critical_minerals_trade(
    critmin_import = imports, critmin_export = exports, total_export = total,
    mineral_demand_clean = demand, country_info = country_info
  ) %>%
    dplyr::ungroup()
  raw <- out %>% dplyr::filter(variable == "critmin_market_share", data_type == "raw", tech == "Electric Vehicles")
  ev <- function(country, stage) raw$value[raw$Country == country & raw$supply_chain == stage]
  # EV demand: copper 30, lithium 80 (fixture). Only copper is traded as ore or scrap, so those
  # stages weight copper alone; Refining weights copper and lithium by their EV demand shares.
  w <- reserves_build_mineral_demand_clean(critical_fixture()) %>%
    dplyr::filter(tech == "Electric Vehicles", Mineral %in% c("Copper", "Lithium"))
  w_cu <- w$share_24[w$Mineral == "Copper"] / sum(w$share_24)
  expect_equal(ev("Chile", "Extraction"), 1)
  expect_equal(ev("Germany", "Recovery"), 1)
  expect_equal(ev("China", "Refining"), w_cu * 0.9)
  expect_equal(ev("Australia", "Refining"), (1 - w_cu) * 1)
  expect_equal(ev("Australia", "Extraction"), 0)
  expect_setequal(unique(out$supply_chain), c("Extraction", "Refining", "Recovery"))
  expect_match(
    unique(out$explanation[out$variable == "critmin_market_share" & out$data_type == "raw" &
                             out$tech == "Electric Vehicles" & out$supply_chain == "Refining"]),
    "Refining: refined chemicals.*\\(3 HS6 codes\\)"
  )
  expect_match(
    unique(out$explanation[out$variable == "critmin_market_share" & out$data_type == "raw" &
                             out$tech == "Electric Vehicles" & out$supply_chain == "Extraction"]),
    "Extraction: ores.*\\(1 HS6 code\\)$"
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

# ---- Critical-minerals trade: mirror exports for non-reporters -----------------------------

# Partner-level import declarations, shaped like critmin_mirror_import.csv. The DR Congo and the
# Republic of Congo report nothing; Chile reports its own exports.
mirror_fixture <- function() {
  cu <- "Copper ores and concentrates"
  tibble::tribble(
    ~ref_year, ~reporter_iso, ~partner_iso, ~cmd_code, ~cmd_desc,          ~cifvalue, ~fobvalue, ~primary_value, ~flow_code,
    2024,      "CHN",         "COD",        "260300",  cu,                       106,        NA,            106, "M",
    2024,      "DEU",         "COD",        "260300",  cu,                        12,        10,             12, "M",
    # The EU's declaration sums its members': counting it would count Germany twice.
    2024,      "EUR",         "COD",        "260300",  cu,                        12,        10,             12, "M",
    # Chile reports its own exports, so partners' imports from it are never added.
    2024,      "CHN",         "CHL",        "260300",  cu,                       500,        NA,            500, "M",
    # "Areas, n.e.s." is not an economy.
    2024,      "CHN",         "_X",         "260300",  cu,                        50,        NA,             50, "M",
    2024,      "CHN",         "COD",        "TOTAL",   "All Commodities",       1060,        NA,           1060, "M",
    2024,      "DEU",         "COD",        "TOTAL",   "All Commodities",        120,       100,            120, "M",
    # Another vintage than the pulls': ignored.
    2023,      "CHN",         "COG",        "260300",  cu,                        40,        NA,             40, "M"
  )
}

mirror_country_info <- function() {
  dplyr::bind_rows(
    trade_fixture()$country_info,
    tibble::tibble(iso3c = c("COD", "COG"), country = c("Democratic Republic of Congo", "Congo"))
  )
}

# Import declarations with both valuations: chapter 26 has enough of them for its own ratio,
# chapter 28 too few, so it takes the pooled one.
cif_fob_fixture <- function() {
  tibble::tibble(
    reporter_iso = "DEU",
    cmd_code = c(rep("260300", 30), rep("282520", 2)),
    cmd_desc = c(rep("Copper ores and concentrates", 30), rep("Lithium oxide and hydroxide", 2)),
    cifvalue = c(rep(110, 30), rep(102, 2)),
    fobvalue = c(rep(100, 30), rep(100, 2)),
    primary_value = 0,
    ref_year = 2024
  )
}

test_that("the CIF/FOB ratio is estimated by chapter and pooled where declarations are few", {
  ratio <- critical_minerals_trade_cif_fob_ratio(cif_fob_fixture())

  pooled <- (30 * 110 + 2 * 102) / (32 * 100)
  expect_equal(ratio$cif_fob[ratio$chapter == "26"], 1.10)
  expect_equal(ratio$cif_fob[ratio$chapter == "28"], pooled)
  expect_equal(ratio$cif_fob[ratio$chapter == "all"], pooled)
  # Without both valuations there is nothing to estimate from.
  expect_equal(nrow(critical_minerals_trade_cif_fob_ratio(trade_fixture()$imports)), 0)
})

test_that("mirror exports sum reporting importers at an FOB basis and skip aggregates", {
  fx <- trade_fixture()
  ratio <- critical_minerals_trade_cif_fob_ratio(cif_fob_fixture())

  mirror <- critical_minerals_trade_build_mirror(
    mirror_fixture(),
    reporters = critical_minerals_trade_reporters(fx$exports, fx$total),
    economies = mirror_country_info()$iso3c,
    cif_fob = ratio,
    year = 2024
  )

  cod <- mirror %>% dplyr::filter(reporter_iso == "COD", cmd_code == "260300")
  # Germany's own FOB value, China's CIF value over the chapter-26 ratio; the EU row is dropped.
  expect_equal(cod$exports, 10 + 106 / 1.10)
  expect_equal(cod$n_importers, 2L)
  expect_equal(cod$exports_cif, 106 + 12)
  # The total takes the pooled ratio.
  pooled <- ratio$cif_fob[ratio$chapter == "all"]
  expect_equal(mirror$exports[mirror$reporter_iso == "COD" & mirror$cmd_code == "TOTAL"], 100 + 1060 / pooled)
  # Chile reports; "_X" is no economy; the Congo row belongs to another year.
  expect_setequal(unique(mirror$reporter_iso), "COD")
})

test_that("non-reporters are scored from mirror exports and reporters keep their own figures", {
  fx <- trade_fixture()
  demand <- reserves_build_mineral_demand_clean(critical_fixture())
  imports <- dplyr::bind_rows(fx$imports, cif_fob_fixture())
  ci <- mirror_country_info()

  build <- function(mirror) {
    critical_minerals_trade_flows(
      critmin_import = imports, critmin_export = fx$exports, total_export = fx$total,
      mineral_demand_clean = demand, country_info = ci, mirror_import = mirror
    )
  }
  before <- build(NULL)
  after <- build(mirror_fixture())

  # The DR Congo now exports copper, from its partners' declarations.
  cod <- after %>% dplyr::filter(reporter_iso == "COD", mineral == "Copper")
  expect_equal(cod$export_source, "mirror")
  expect_equal(cod$exports, 10 + 106 / 1.10)
  expect_gt(cod$market_share, 0)
  expect_false("COD" %in% before$reporter_iso)
  # Its export share uses its mirror total exports.
  ratio <- critical_minerals_trade_cif_fob_ratio(imports)
  pooled <- ratio$cif_fob[ratio$chapter == "all"]
  expect_equal(cod$exp_share, cod$exports / (100 + 1060 / pooled))

  # Reporters' exports are untouched; only the world total behind their shares grows.
  key <- c("reporter_iso", "mineral", "supply_chain")
  both <- dplyr::inner_join(
    before %>% dplyr::select(dplyr::all_of(key), exports_before = exports, share_before = market_share),
    after %>% dplyr::select(dplyr::all_of(key), exports_after = exports, share_after = market_share, export_source),
    by = key
  )
  expect_equal(nrow(both), nrow(before))
  expect_equal(both$exports_after, both$exports_before)
  expect_true(all(both$export_source == "reported"))
  chl <- both %>% dplyr::filter(reporter_iso == "CHL", mineral == "Copper")
  expect_equal(chl$share_before, 1)
  expect_equal(chl$share_after, 90 / (90 + 10 + 106 / 1.10))

  # One provenance per economy, and the copper HHI now runs over both sources.
  expect_equal(nrow(dplyr::distinct(after, reporter_iso, export_source)), dplyr::n_distinct(after$reporter_iso))
  shares <- c(90, 10 + 106 / 1.10) / (90 + 10 + 106 / 1.10)
  expect_equal(unique(after$HHI[after$mineral == "Copper"]), sum(shares^2))
})

test_that("the theme marks provenance and scores a mirrored producer above zero", {
  fx <- trade_fixture()
  out <- critical_minerals_trade(
    critmin_import = dplyr::bind_rows(fx$imports, cif_fob_fixture()),
    critmin_export = fx$exports,
    total_export = fx$total,
    mineral_demand_clean = reserves_build_mineral_demand_clean(critical_fixture()),
    country_info = mirror_country_info(),
    mirror_import = mirror_fixture()
  ) %>%
    dplyr::ungroup()

  ms <- out %>% dplyr::filter(variable == "critmin_market_share", data_type == "index", tech == "Wind")
  expect_gt(ms$value[ms$Country == "Democratic Republic of Congo"], 0)

  prov <- dplyr::distinct(out, Country, provenance)
  expect_equal(prov$provenance[prov$Country == "Democratic Republic of Congo"], "mirror")
  expect_equal(prov$provenance[prov$Country == "Chile"], "reported")
  # France and the Congo have neither: only the world-market rows, which every country gets.
  expect_equal(prov$provenance[prov$Country == "France"], "none")
  expect_equal(nrow(prov), dplyr::n_distinct(out$Country))
  expect_false(any(is.na(out$provenance)))

  src <- out %>% dplyr::filter(Country == "Democratic Republic of Congo", variable == "critmin_market_share")
  expect_true(all(grepl("mirror", src$source)))
  expect_equal(unique(out$source[out$variable == "critmin_hhi"]), "UN Comtrade (reported and mirror exports)")
})

test_that("mirror data from another vintage leave the non-reporters at zero, with a warning", {
  fx <- trade_fixture()
  stale <- mirror_fixture() %>% dplyr::mutate(ref_year = 2023)

  expect_warning(
    flows <- critical_minerals_trade_flows(
      critmin_import = dplyr::bind_rows(fx$imports, cif_fob_fixture()),
      critmin_export = fx$exports,
      total_export = fx$total,
      mineral_demand_clean = reserves_build_mineral_demand_clean(critical_fixture()),
      country_info = mirror_country_info(),
      mirror_import = stale
    ),
    "no flows for 2024"
  )
  expect_false(any(flows$export_source == "mirror"))
})

test_that("an economy that declares imports but not exports is mirrored once", {
  fx <- trade_fixture()
  # The DR Congo declares copper imports (it is in the import pull) but no exports.
  imports <- dplyr::bind_rows(
    fx$imports, cif_fob_fixture(),
    tibble::tibble(
      reporter_iso = "COD", reporter_desc = "COD", cmd_code = "260300",
      cmd_desc = "Copper ores and concentrates", primary_value = 5, ref_year = 2024
    )
  )

  flows <- critical_minerals_trade_flows(
    critmin_import = imports,
    critmin_export = fx$exports,
    total_export = fx$total,
    mineral_demand_clean = reserves_build_mineral_demand_clean(critical_fixture()),
    country_info = mirror_country_info(),
    mirror_import = mirror_fixture()
  )

  cod <- flows %>% dplyr::filter(reporter_iso == "COD")
  expect_equal(unique(cod$export_source), "mirror")
  expect_equal(nrow(cod), nrow(dplyr::distinct(flows, mineral, supply_chain)))
  expect_equal(cod$exports[cod$mineral == "Copper"], 10 + 106 / 1.10)
})

test_that("a late reporter's own declarations replace its mirror, zeros included", {
  fx <- trade_fixture()
  cu <- "Copper ores and concentrates"
  # The DR Congo has since declared its 2024 exports: copper 150 and total 1500, to World. Its
  # partners still record 10 + 106 of copper from it, which must no longer count.
  late <- tibble::tribble(
    ~ref_year, ~flow_code, ~reporter_iso, ~partner_iso, ~partner_code, ~cmd_code, ~cmd_desc,             ~cifvalue, ~fobvalue, ~primary_value,
    2024,      "X",        "COD",         "W00",        0,             "260300",  "Copper ores, conc.",         NA,       150,            150,
    2024,      "X",        "COD",         "W00",        0,             "TOTAL",   "All Commodities",            NA,      1500,           1500,
    # A reporter in the staged pull is never replaced by a later declaration.
    2024,      "X",        "CHL",         "W00",        0,             "260300",  cu,                           NA,       999,            999,
    # Another year: ignored.
    2023,      "X",        "COG",         "W00",        0,             "260300",  cu,                           NA,        70,             70
  )
  mirror <- dplyr::bind_rows(mirror_fixture() %>% dplyr::mutate(partner_code = 1), late)
  imports <- dplyr::bind_rows(fx$imports, cif_fob_fixture())

  reported <- critical_minerals_trade_late_exports(
    mirror,
    reporters = critical_minerals_trade_reporters(fx$exports, fx$total),
    economies = mirror_country_info()$iso3c,
    year = 2024
  )
  expect_equal(reported$exports$reporter_iso, "COD")
  expect_equal(reported$exports$primary_value, 150)
  expect_equal(reported$totals$primary_value, 1500)

  flows <- critical_minerals_trade_flows(
    critmin_import = imports,
    critmin_export = fx$exports,
    total_export = fx$total,
    mineral_demand_clean = reserves_build_mineral_demand_clean(critical_fixture()),
    country_info = mirror_country_info(),
    mirror_import = mirror
  )

  cod <- flows %>% dplyr::filter(reporter_iso == "COD")
  expect_equal(unique(cod$export_source), "reported")
  # Its own copper, read through the pull's description for the code; zeros for the rest.
  expect_equal(cod$exports[cod$mineral == "Copper"], 150)
  expect_equal(sum(cod$exports[cod$mineral != "Copper"]), 0)
  expect_equal(cod$exp_share[cod$mineral == "Copper"], 150 / 1500)
  expect_equal(flows$exports[flows$reporter_iso == "CHL" & flows$mineral == "Copper"], 90)
  expect_false(any(flows$export_source == "mirror"))
  expect_equal(unique(flows$HHI[flows$mineral == "Copper"]), sum((c(90, 150) / 240)^2))
})

test_that("an economy with known total exports but none of the minerals scores zero, not 'none'", {
  fx <- trade_fixture()
  # The Congo has since declared its 2024 total exports but none of these minerals (as Iraq
  # did for 2024); China records other goods from France but none of these minerals.
  extra <- tibble::tribble(
    ~ref_year, ~flow_code, ~reporter_iso, ~partner_iso, ~partner_code, ~cmd_code, ~cmd_desc,         ~cifvalue, ~fobvalue, ~primary_value,
    2024,      "X",        "COG",         "W00",        0,             "TOTAL",   "All Commodities",        NA,       700,            700,
    2024,      "M",        "CHN",         "FRA",        251,           "TOTAL",   "All Commodities",       530,        NA,            530
  )
  mirror <- dplyr::bind_rows(mirror_fixture() %>% dplyr::mutate(partner_code = 1), extra)
  args <- list(
    critmin_import = dplyr::bind_rows(fx$imports, cif_fob_fixture()),
    critmin_export = fx$exports,
    total_export = fx$total,
    mineral_demand_clean = reserves_build_mineral_demand_clean(critical_fixture()),
    country_info = mirror_country_info(),
    mirror_import = mirror
  )

  flows <- do.call(critical_minerals_trade_flows, args)
  cog <- flows %>% dplyr::filter(reporter_iso == "COG")
  fra <- flows %>% dplyr::filter(reporter_iso == "FRA")
  # Zero rows for every mineral, like any reporter (the Congo) or mirrored economy (France).
  expect_equal(nrow(cog), nrow(dplyr::distinct(flows, mineral, supply_chain)))
  expect_equal(unique(cog$export_source), "reported")
  expect_equal(sum(cog$exports), 0)
  expect_equal(unique(cog$exp_share), 0)
  expect_equal(nrow(fra), nrow(dplyr::distinct(flows, mineral, supply_chain)))
  expect_equal(unique(fra$export_source), "mirror")
  expect_equal(sum(fra$exports), 0)
  # The DR Congo's mirror and the copper market are unchanged by the zero rows.
  expect_equal(flows$exports[flows$reporter_iso == "COD" & flows$mineral == "Copper"], 10 + 106 / 1.10)
  expect_equal(sum(flows$market_share[flows$mineral == "Copper"]), 1)

  out <- do.call(critical_minerals_trade, args) %>% dplyr::ungroup()
  prov <- dplyr::distinct(out, Country, provenance)
  expect_equal(prov$provenance[prov$Country == "Congo"], "reported")
  expect_equal(prov$provenance[prov$Country == "France"], "mirror")
  ms <- out %>%
    dplyr::filter(variable == "critmin_market_share", data_type == "index", Country %in% c("Congo", "France"))
  cells <- dplyr::distinct(out[out$variable == "critmin_market_share", ], tech, supply_chain)
  expect_equal(nrow(ms), 2 * nrow(cells))
  expect_true(all(ms$value == 0))
})

test_that("every mineral theme emits only the six stage names", {
  critical <- critical_fixture()
  demand <- reserves_build_mineral_demand_clean(critical)
  fx <- trade_fixture()

  themes <- list(
    mineral_supply = foreign_dependency_build_mineral_supply(critical, demand, roster_fixture()),
    processing = critical_minerals_processing(
      critical, demand, tibble::tibble(country = c("Australia", "Chile", "China")), roster_fixture()
    ),
    reserves = suppressWarnings(reserves(reserve_ei_fixture(), reserve_inputs_fixture(), demand)),
    production = critical_minerals_production(production_inputs_fixture(), demand, roster_fixture()),
    trade = critical_minerals_trade(fx$imports, fx$exports, fx$total, demand, fx$country_info)
  )
  for (name in names(themes)) {
    expect_silent(assert_supply_chain_stages(themes[[name]]$supply_chain, name))
    # Every Upstream measure here rolls up to Upstream, refining included.
    expect_setequal(unique(supply_chain_stage_rollup(themes[[name]]$supply_chain)), "Upstream")
  }
})
