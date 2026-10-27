# EI/BNEF/GIS-based themes: energy access, energy consumption, import dependence and the
# solar/wind resource potentials.
#
# Guards the failures these themes had: EI's "USSR" aggregate and raw spellings ("US",
# "Vietnam") were ranked as countries before names were standardized; BNEF wind was never
# recoded, so Wind had no Consumption; the BNEF growth index was ranked across technologies;
# and the East Asia & Pacific regional copy ran twice.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "levels.R"))
source(file.path(repo_root, "R", "utils", "eia_intl.R"))
source(file.path(repo_root, "R", "categories", "shared", "overall_index.R"))
source(file.path(repo_root, "R", "categories", "energy_access", "energy_access_consumption.R"))
source(file.path(repo_root, "R", "categories", "consumption", "energy_consumption.R"))
source(file.path(repo_root, "R", "categories", "energy_imports", "import_dependence.R"))
source(file.path(repo_root, "R", "categories", "energy_access", "solar_pv_potential.R"))
source(file.path(repo_root, "R", "categories", "energy_access", "wind_potential.R"))
source(file.path(repo_root, "R", "categories", "energy_access", "geothermal_potential.R"))
source(file.path(repo_root, "R", "indices", "index_builder_core.R"))

withr::local_options(list(opportunity_security.index_definition = NULL))

# EI long panel: two years, three real countries plus the aggregates EI publishes alongside.
ei_fixture <- function() {
  countries <- c("US", "Vietnam", "Germany", "USSR", "Total World", "Other Europe")
  vars <- c("pop", "coal_tes_ej", "oil_tes_ej", "gas_tes_ej", "solar_tes_ej", "wind_tes_ej", "nuclear_tes_ej")
  grid <- expand.grid(Country = countries, Year = c(2020, 2025), Var = vars, stringsAsFactors = FALSE)
  grid$Value <- seq_len(nrow(grid)) %% 17 + 1
  # USSR carries energy data but no population in the latest year, as in the EI file.
  grid$Value[grid$Country == "USSR" & grid$Var == "pop"] <- NA
  tibble::as_tibble(grid)
}

test_that("energy access ranks economies only, under their standardized names", {
  out <- energy_access_consumption(ei_fixture(), base_year = 2020, target_year = 2025) %>%
    dplyr::ungroup()

  expect_setequal(unique(out$Country), c("United States", "Viet Nam", "Germany"))
  per_capita <- out %>%
    dplyr::filter(variable == "Energy consumption per capita", data_type == "index", tech == "Oil")
  expect_equal(nrow(per_capita), 3)
})

test_that("the EI half of energy consumption drops USSR and standardizes names", {
  cleaned <- energy_consumption_clean_ei(ei_fixture(), 2025)

  expect_setequal(cleaned$Country, c("United States", "Viet Nam", "Germany"))
})

test_that("import dependence drops USSR and keeps one row per economy", {
  ei <- ei_fixture()
  extra <- expand.grid(
    Country = unique(ei$Country), Year = c(2020, 2025),
    Var = c("oilprod_kbd", "oilcons_kbd", "gasprod_ej", "coalprod_ej"),
    stringsAsFactors = FALSE
  )
  extra$Value <- seq_len(nrow(extra)) %% 11 + 1
  ei <- dplyr::bind_rows(ei, tibble::as_tibble(extra))

  cleaned <- import_dependence_clean_raw(ei, year = 2025)
  expect_false(any(cleaned$Country %in% c("USSR", "US", "Vietnam")))
  expect_equal(nrow(cleaned), dplyr::n_distinct(cleaned$Country))
})

# import_dependence_clean_raw() output for a net exporter, a partial producer and two
# economies that report zero production of every fuel (a reported 0, as EI gives for Spain coal
# in 2025, not an absent row). 100 * (0 - 0.007) / 0.007 is -100.00000000000001
# while 100 * (0 - 0.1) / 0.1 is -100, which is how the former "Import share" split the last
# two by rounding noise.
import_balance_fixture <- function() {
  tibble::tibble(
    Country = c("Exporter", "Partial", "None A", "None B"),
    Year = "2025",
    oil_tes_ej = c(1, 2, 0.1, 0.007),
    oilcons_kbd = c(500, 1000, 50, 3.5),
    oilprod_kbd = c(1500, 500, 0, 0),
    gas_tes_ej = c(1, 2, 0.1, 0.007),
    gasprod_ej = c(3, 1, 0, 0),
    coal_tes_ej = c(1, 2, 0.1, 0.007),
    coalprod_ej = c(3, 1, 0, 0)
  )
}

test_that("self-sufficiency is production over consumption and replaces the inverted 'Import share'", {
  out <- import_dependence_build_tidy(
    import_dependence_build_imports(import_balance_fixture()),
    year = 2025
  )

  expect_false("Import share" %in% out$variable)
  self_sufficiency <- function(country, fuel) {
    out$value[out$Country == country & out$tech == fuel & out$variable == "Self-sufficiency"]
  }
  expect_equal(self_sufficiency("Exporter", "Gas"), 300)
  expect_equal(self_sufficiency("Partial", "Gas"), 50)
  expect_equal(self_sufficiency("Partial", "Fossil"), 50)
  # Oil production in EJ is taken at the country's own EJ per barrel of consumption.
  expect_equal(self_sufficiency("Exporter", "Oil"), 300)
  expect_equal(self_sufficiency("None B", "Coal"), 0)

  # The index stays oriented higher = less import dependent.
  index <- out %>% dplyr::filter(data_type == "index", tech == "Gas")
  expect_equal(index$Country[which.max(index$value)], "Exporter")
})

test_that("economies that report zero production of a fuel tie exactly on self-sufficiency", {
  built <- import_dependence_build_imports(import_balance_fixture())
  none <- built$Country %in% c("None A", "None B")

  for (fuel in c("oil", "gas", "coal", "fossil")) {
    ss <- built[[paste0(fuel, "_self_sufficiency")]]
    expect_identical(ss[none], c(0, 0), info = fuel)
    ranked <- median_scurve(ss)
    expect_identical(ranked[none][[1]], ranked[none][[2]], info = fuel)
  }
})

# EI long panel for 2025 with the production gaps of the real file: EI lists fewer producers
# than consumers and folds the rest into "Other <region>" residuals. New Zealand has no gas
# or oil production row (it produces gas, inside "Other Asia Pacific"), Spain reports coal
# production of exactly 0, Singapore no production row at all, and Iceland no gas row of either
# kind. Until 2026-10-11 every absent production row was read as 0.
ei_production_gap_fixture <- function() {
  rows <- tibble::tribble(
    ~Country,             ~Var,          ~Value,
    "Norway",             "oil_tes_ej",  0.4,
    "Norway",             "oilcons_kbd", 200,
    "Norway",             "oilprod_kbd", 2000,
    "Norway",             "gas_tes_ej",  0.2,
    "Norway",             "gasprod_ej",  4,
    "Norway",             "coal_tes_ej", 0.03,
    "Norway",             "coalprod_ej", 0.01,
    "Netherlands",        "oil_tes_ej",  1.8,
    "Netherlands",        "oilcons_kbd", 900,
    "Netherlands",        "oilprod_kbd", 20,
    "Netherlands",        "gas_tes_ej",  1.1,
    "Netherlands",        "gasprod_ej",  0.4,
    "Netherlands",        "coal_tes_ej", 0.2,
    "New Zealand",        "oil_tes_ej",  0.32,
    "New Zealand",        "oilcons_kbd", 160,
    "New Zealand",        "gas_tes_ej",  0.1,
    "New Zealand",        "coal_tes_ej", 0.04,
    "New Zealand",        "coalprod_ej", 0.07,
    "Spain",              "oil_tes_ej",  2.5,
    "Spain",              "oilcons_kbd", 1250,
    "Spain",              "gas_tes_ej",  1.2,
    "Spain",              "coal_tes_ej", 0.1,
    "Spain",              "coalprod_ej", 0,
    "Singapore",          "oil_tes_ej",  3,
    "Singapore",          "oilcons_kbd", 1500,
    "Singapore",          "gas_tes_ej",  0.4,
    "Singapore",          "coal_tes_ej", 0.02,
    "Iceland",            "oil_tes_ej",  0.04,
    "Iceland",            "oilcons_kbd", 20,
    "Iceland",            "coal_tes_ej", 0.003,
    "Other Asia Pacific", "oil_tes_ej",  1,
    "Other Asia Pacific", "oilcons_kbd", 500,
    "Other Asia Pacific", "gasprod_ej",  0.7
  )
  dplyr::mutate(rows, Year = 2025L, .after = Country)
}

test_that("an absent EI production row is missing, not zero; a reported zero stays zero", {
  cleaned <- import_dependence_clean_raw(ei_production_gap_fixture(), year = 2025)
  value <- function(country, col) cleaned[[col]][cleaned$Country == country]

  expect_setequal(cleaned$Country, c("Norway", "Netherlands", "New Zealand", "Spain", "Singapore", "Iceland"))
  expect_true(is.na(value("New Zealand", "gasprod_ej")))
  expect_true(is.na(value("Singapore", "oilprod_kbd")))
  expect_true(is.na(value("Netherlands", "coalprod_ej")))
  expect_identical(value("Spain", "coalprod_ej"), 0)
  # Consumption is still completed with zero (EI omits it only where it omits production too).
  expect_identical(value("Iceland", "gas_tes_ej"), 0)
})

test_that("unreported production leaves balance, self-sufficiency and index NA, and says why", {
  # With the index definition loaded, so the theme's overall row is built as in scripts/10.
  out <- withr::with_options(
    list(opportunity_security.index_definition = yaml::read_yaml(file.path(repo_root, "config", "index_definition.yml"))),
    import_dependence(ei_production_gap_fixture(), year = 2025)
  )
  cell <- function(country, fuel, variable, data_type) {
    out[out$Country == country & out$tech == fuel & out$variable == variable & out$data_type == data_type, ]
  }

  # New Zealand gas, 0% self-sufficient under the zero fill, is not reported separately.
  nz_gas <- out[out$Country == "New Zealand" & out$tech == "Gas", ]
  expect_equal(nrow(nz_gas), 4L)
  expect_true(all(is.na(nz_gas$value)))
  expect_false(any(is.nan(nz_gas$value)))
  expect_true(all(grepl("Not reported separately: EI has no gas production row", nz_gas$explanation[nz_gas$variable != "Overall Energy Imports Index"])))
  # Iceland neither produces nor consumes gas in EI: the balance is missing, not 0.
  expect_true(is.na(cell("Iceland", "Gas", "Production surplus/deficit", "raw")$value))

  # Reported values are unaffected, and a reported zero is still a 0% observation.
  expect_equal(cell("New Zealand", "Coal", "Self-sufficiency", "raw")$value, 175)
  expect_false(grepl("Not reported", cell("New Zealand", "Coal", "Self-sufficiency", "raw")$explanation))
  expect_identical(cell("Spain", "Coal", "Self-sufficiency", "raw")$value, 0)
  expect_equal(cell("Spain", "Coal", "Production surplus/deficit", "raw")$value, -0.1)

  # The percent ranks run over the economies with a value: Coal over Norway, New Zealand and
  # Spain (Spain lowest), Gas over Norway and the Netherlands only.
  coal_index <- out[out$tech == "Coal" & out$variable == "Production surplus/deficit" & out$data_type == "index", ]
  expect_setequal(coal_index$Country[!is.na(coal_index$value)], c("Norway", "New Zealand", "Spain"))
  expect_equal(coal_index$Country[which.min(coal_index$value)], "Spain")
  gas_index <- out[out$tech == "Gas" & out$variable == "Production surplus/deficit" & out$data_type == "index", ]
  expect_equal(gas_index$value[gas_index$Country == "Norway"], 1)
  expect_equal(gas_index$value[gas_index$Country == "Netherlands"], 0)

  # Fossil sums the fuels EI reports (as the Sector Insights exporter does) and is NA only
  # when it reports none: New Zealand = coal production / all fossil consumption.
  expect_equal(cell("New Zealand", "Fossil", "Self-sufficiency", "raw")$value, 100 * 0.07 / (0.32 + 0.1 + 0.04))
  expect_true(all(is.na(out$value[out$Country == "Singapore" & out$tech == "Fossil"])))
  expect_true(grepl("no oil, gas or coal production row", cell("Singapore", "Fossil", "Self-sufficiency", "raw")$explanation))
})

test_that("an unreported fuel takes the pillar's missing-data rule and is flagged imputed", {
  rules <- yaml::read_yaml(file.path(repo_root, "config", "missing_data.yml"))$energy_security
  expect_identical(rules$import_dependence, "global_average")

  index_rows <- import_dependence(ei_production_gap_fixture(), year = 2025) %>%
    dplyr::filter(data_type == "index", variable == "Production surplus/deficit", tech == "Gas") %>%
    dplyr::mutate(theme = "import_dependence")
  filled <- apply_missing_policy(index_rows, parse_missing_policy(rules["import_dependence"], index_rows))

  nz <- filled[filled$Country == "New Zealand", ]
  expect_true(nz$imputed)
  expect_identical(nz$missing_rule_applied, "global_average")
  expect_equal(nz$value, mean(index_rows$value, na.rm = TRUE))
  expect_false(any(filled$imputed[filled$Country %in% c("Norway", "Netherlands")]))
})

test_that("the balance is consumption times self-sufficiency minus one for every fuel", {
  built <- import_dependence_build_imports(import_dependence_clean_raw(ei_production_gap_fixture(), year = 2025))
  for (f in c("oil", "gas", "coal", "fossil")) {
    ss <- built[[paste0(f, "_self_sufficiency")]]
    ok <- !is.na(ss)
    expect_true(any(ok), info = f)
    expect_equal(built[[paste0(f, "_surplus_ej")]][ok], (built[[paste0(f, "_tes_ej")]] * (ss / 100 - 1))[ok], info = f)
  }
})

# The 2025 production EIA supplies for gas and coal is its 2024 value (EIA publishes them a year
# after oil). Until fixes9 (2026-10-11) it was divided by EI's 2025 consumption, so a consumer whose
# use fell in 2025 read as self-sufficient: Slovenia coal 121% and a net exporter (EI's coal
# consumption fell 39%), New Zealand gas 122%. Values as in the 2026-10-11 inputs.
test_that("an EIA production value of an earlier year is set against consumption of that year", {
  ei <- tibble::tribble(
    ~Country,      ~Year, ~ISO3166_alpha3, ~Var,          ~Value,
    "Slovenia",    2025L, "SVN",           "oil_tes_ej",  0.2,
    "Slovenia",    2025L, "SVN",           "oilcons_kbd", 100,
    "Slovenia",    2025L, "SVN",           "oilprod_kbd", 0,
    "Slovenia",    2025L, "SVN",           "gas_tes_ej",  0.0326,
    "Slovenia",    2025L, "SVN",           "gasprod_ej",  0,
    "Slovenia",    2025L, "SVN",           "coal_tes_ej", 0.01971,
    "Slovenia",    2024L, "SVN",           "coal_tes_ej", 0.03254,
    "New Zealand", 2025L, "NZL",           "oil_tes_ej",  0.32,
    "New Zealand", 2025L, "NZL",           "oilcons_kbd", 160,
    "New Zealand", 2025L, "NZL",           "oilprod_kbd", 12,
    "New Zealand", 2025L, "NZL",           "gas_tes_ej",  0.09883,
    "New Zealand", 2024L, "NZL",           "gas_tes_ej",  0.10995,
    "New Zealand", 2025L, "NZL",           "coal_tes_ej", 0.04,
    "New Zealand", 2025L, "NZL",           "coalprod_ej", 0.07
  )
  eia_series <- function(iso3, fuel, component, unit, product, value) {
    tibble::tibble(
      iso3 = iso3, country = iso3, fuel = fuel, flow = "production", component = component, year = 2024L,
      value = value, unit = unit, series_id = paste0("INTL.", product, "-1-", iso3, "-", unit, ".A"), value_note = ""
    )
  }
  eia <- dplyr::bind_rows(
    eia_series("SVN", "coal", "coal", "TJ", 7L, 0.02393 / 0.95e-6),
    eia_series("NZL", "gas", "dry_gas", "BCM", 26L, 3.344)
  )
  out <- withr::with_options(
    list(opportunity_security.index_definition = yaml::read_yaml(file.path(repo_root, "config", "index_definition.yml"))),
    import_dependence(ei, year = 2025, eia = eia)
  )
  raw <- function(country, fuel, variable) {
    out$value[out$Country == country & out$tech == fuel & out$variable == variable & out$data_type == "raw"]
  }

  # Slovenia coal: 2024 production over 2024 consumption, a net importer.
  expect_equal(raw("Slovenia", "Coal", "Self-sufficiency"), 100 * 0.02393 / 0.03254)
  expect_equal(raw("Slovenia", "Coal", "Self-sufficiency"), 73.5, tolerance = 0.001)
  expect_equal(raw("Slovenia", "Coal", "Production surplus/deficit"), 0.01971 * (0.02393 / 0.03254 - 1))
  expect_lt(raw("Slovenia", "Coal", "Production surplus/deficit"), 0)
  # New Zealand gas: 109.5%, not 122%.
  expect_equal(raw("New Zealand", "Gas", "Self-sufficiency"), 100 * 3.344 * 0.036 / 0.10995)
  expect_equal(raw("New Zealand", "Gas", "Production surplus/deficit"), 0.09883 * (3.344 * 0.036 / 0.10995 - 1))
  # EI values of EI's year are untouched.
  expect_equal(raw("New Zealand", "Coal", "Self-sufficiency"), 175)
  expect_identical(raw("Slovenia", "Gas", "Self-sufficiency"), 0)
  ex <- out$explanation[out$Country == "Slovenia" & out$tech == "Coal" & out$variable == "Self-sufficiency"]
  expect_match(ex, "self-sufficiency pairs them in 2024")
})

# ---- BNEF New Energy Outlook ---------------------------------------------------------------

bnef_fixture <- function() {
  regions <- c("US", "Vietnam", "Other Southeast Asia", "Other Asia Pacific", "MENAT", "Global")
  pop <- tibble::tibble(
    Indicator = "Population", Macro.sector = "All sectors", Region = regions, Scenario = "ETS",
    Fuel.type = "", X2024 = c(330, 100, 400, 60, 500, 8000), X2035 = c(350, 105, 430, 62, 560, 8800)
  )
  fuels <- c("Onshore wind", "Offshore wind", "Utility-scale PV", "Small-scale PV", "Battery storage")
  cap <- expand.grid(Region = regions, Fuel.type = fuels, stringsAsFactors = FALSE) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      Indicator = "Installed electric capacity", Macro.sector = "Energy industry", Scenario = "ETS",
      X2024 = seq_len(dplyr::n()) %% 7 + 1,
      X2035 = X2024 * (1 + seq_len(dplyr::n()) %% 5)
    )
  dplyr::bind_rows(pop, cap)
}

bnef_country_info <- function() {
  tibble::tibble(
    iso3c = c("USA", "VNM", "THA", "NZL", "TUR", "EGY"),
    country = c("United States", "Viet Nam", "Thailand", "New Zealand", "Turkiye", "Egypt"),
    region = c(
      "North America", "East Asia & Pacific", "East Asia & Pacific", "East Asia & Pacific",
      "Europe & Central Asia", "Middle East & North Africa"
    )
  )
}

bnef_metrics <- function() {
  neo <- bnef_fixture()
  energy_consumption_build_bnef_metrics(
    neo,
    pop_tbl = energy_consumption_build_bnef_population(neo),
    techs = c("Wind", "Solar", "Batteries"),
    country_info = bnef_country_info()
  )
}

test_that("BNEF onshore and offshore wind become Wind Consumption", {
  out <- bnef_metrics()

  expect_true(all(c("Wind", "Solar", "Batteries") %in% out$tech))
  wind <- out %>% dplyr::filter(tech == "Wind", variable == "installed_cap_index")
  expect_true(nrow(wind) > 0)
  expect_equal(unique(out$Year), 2024L)
})

test_that("each country gets one BNEF row per tech and variable, from the right region", {
  out <- bnef_metrics()

  counts <- out %>% dplyr::count(Country, tech, variable)
  expect_true(all(counts$n == 1))

  value_of <- function(country, tech_ = "Solar", variable_ = "X2024_pc") {
    out$value[out$Country == country & out$tech == tech_ & out$variable == variable_]
  }
  neo <- bnef_fixture()
  solar_pc <- function(region) {
    pop <- neo$X2024[neo$Indicator == "Population" & neo$Region == region]
    sum(neo$X2024[neo$Region == region & neo$Fuel.type %in% c("Utility-scale PV", "Small-scale PV")]) / pop
  }

  # Thailand is in BNEF's "Other Southeast Asia", New Zealand in "Other Asia Pacific".
  expect_equal(value_of("Thailand"), solar_pc("Other Southeast Asia"))
  expect_equal(value_of("New Zealand"), solar_pc("Other Asia Pacific"))
  # Viet Nam is modelled on its own and is not also given the regional copy.
  expect_equal(value_of("Viet Nam"), solar_pc("Vietnam"))
  # Turkiye is the T in MENAT.
  expect_equal(value_of("Turkiye"), solar_pc("MENAT"))
  expect_false(any(out$Country %in% c("Vietnam", "US", "Other Southeast Asia")))
})

test_that("the BNEF growth index ranks countries within each technology", {
  out <- bnef_metrics() %>%
    dplyr::filter(variable == "elec_growth_index")

  top <- out %>% dplyr::group_by(tech) %>% dplyr::summarize(top = max(value), bottom = min(value))
  expect_true(all(top$top == 1))
  expect_true(all(top$bottom == 0))
})

test_that("Green Hydrogen consumption is labelled as all hydrogen, not capacity", {
  neo <- bnef_fixture()
  h2 <- tibble::tibble(
    Indicator = "Final energy consumption", Macro.sector = "All sectors",
    Region = c("US", "Vietnam", "MENAT"), Scenario = "ETS", Fuel.type = "Hydrogen",
    X2024 = c(1248.7, 30.4, 270.6), X2035 = c(1500, 40, 300)
  )
  neo <- dplyr::bind_rows(neo, h2)
  out <- energy_consumption_build_bnef_metrics(
    neo,
    pop_tbl = energy_consumption_build_bnef_population(neo),
    techs = c("Solar", "Green Hydrogen"),
    country_info = bnef_country_info()
  )

  h2_rows <- out %>% dplyr::filter(tech == "Green Hydrogen")
  expect_true(nrow(h2_rows) > 0)
  # Every hydrogen explanation says it is hydrogen consumption; none calls it capacity.
  expect_true(all(grepl("hydrogen", h2_rows$explanation, ignore.case = TRUE)))
  expect_false(any(grepl("capacity", h2_rows$explanation[h2_rows$variable != "share_24"])))
  expect_true(all(grepl("all hydrogen|mixed units", h2_rows$explanation)))
  # Values are unchanged by the relabelling: per-capita PJ from the Final energy consumption row.
  us <- h2_rows$value[h2_rows$Country == "United States" & h2_rows$variable == "X2024_pc"]
  expect_equal(us, 1248.7 / neo$X2024[neo$Indicator == "Population" & neo$Region == "US"])
  # Solar keeps the capacity wording.
  solar <- out %>% dplyr::filter(tech == "Solar", variable == "X2024_pc")
  expect_true(all(grepl("installed electric capacity", solar$explanation)))
})

# ---- Solar and wind potential --------------------------------------------------------------

test_that("resource potentials rank only the economies in the roster", {
  roster <- tibble::tibble(country = c("Cyprus", "Chile", "Taiwan"))

  pv_raw <- tibble::tibble(
    iso_a3 = c("CYP", "-99", "CHL", "TWN"),
    name_long = c("Cyprus", "Northern Cyprus", "Chile", "Taiwan"),
    pvout_total = c(10, 5, 900, 30),
    pvout_mean = c(4.8, 4.8, 5.9, 3.6)
  )
  pv <- solar_pv_potential_clean_raw(pv_raw, country_info = roster)
  expect_setequal(pv$Country, c("Cyprus", "Chile", "Taiwan"))
  expect_setequal(solar_pv_potential_clean_raw(pv_raw)$Country, c("Cyprus", "Northern Cyprus", "Chile", "Taiwan"))

  wind_raw <- tibble::tibble(
    iso_a3 = c("CYP", "-99", "CHL"),
    name_long = c("Cyprus", "Northern Cyprus", "Chile"),
    pd_total_MW_equiv = c(1, 1, 50),
    pd_mean_Wm2 = c(200, 210, 400),
    pd_share_over_threshold = c(0.1, 0.1, 0.5)
  )
  wind <- wind_potential_clean_raw(wind_raw, country_info = roster)
  expect_setequal(wind$Country, c("Cyprus", "Chile"))
})

# ---- Six-stage supply chain ----------------------------------------------------------------

test_that("consumption, energy access and installed capacity are Deployment rows", {
  index_definition <- yaml::read_yaml(file.path(repo_root, "config", "index_definition.yml"))
  withr::local_options(list(opportunity_security.index_definition = index_definition))

  access <- energy_access_consumption(ei_fixture(), base_year = 2020, target_year = 2025)
  expect_setequal(unique(access$supply_chain), "Deployment")
  # The category score is built in the Deployment cell, formerly Downstream.
  expect_true(any(access$variable == "Overall Energy Access Index" & access$supply_chain == "Deployment"))

  ei_level <- energy_consumption_build_ei_indices(energy_consumption_clean_ei(ei_fixture(), 2025), 2025)
  ei_growth <- energy_consumption_build_ei_growth(
    energy_consumption_build_ei_indices(energy_consumption_clean_ei(ei_fixture(), 2020), 2020),
    ei_level,
    2020,
    2025
  )
  expect_setequal(unique(c(ei_level$supply_chain, ei_growth$supply_chain)), "Deployment")
  # The growth join keys on supply_chain, so both years must carry the same stage.
  expect_true(nrow(ei_growth) > 0)
  expect_setequal(unique(bnef_metrics()$supply_chain), "Deployment")
})

test_that("solar, wind and geothermal potentials are site resource: Deployment, not Extraction", {
  pv <- solar_pv_potential(
    tibble::tibble(name_long = c("Chile", "Cyprus", "Germany"), pvout_total = c(900, 10, 300), pvout_mean = c(5.9, 4.8, 3.0)),
    year = 2023
  )
  wind <- wind_potential(
    tibble::tibble(
      name_long = c("Chile", "Cyprus", "Germany"),
      pd_total_MW_equiv = c(50, 1, 20),
      pd_mean_Wm2 = c(400, 200, 300),
      pd_share_over_threshold = c(0.5, 0.1, 0.3)
    ),
    year = 2023
  )
  geo <- geothermal_potential(
    tibble::tibble(
      iso3 = c("CHL", "CYP", "DEU"),
      lcoe_for_500mw = c(60, 90, 80), lcoe_for_5gw = c(70, NA, 95), total_mw = c(9000, 100, 2000),
      lcoe_500mw_index = c(1, 0, 0.3), lcoe_5gw_index = c(1, NA, 0), total_mw_index = c(1, 0, 0.5),
      geothermal_index = c(1, 0, 0.4)
    ),
    year = 2023
  )

  for (tbl in list(pv, wind, geo)) {
    expect_setequal(unique(tbl$supply_chain), "Deployment")
    expect_setequal(unique(tbl$category), "Reserves")
  }
})
