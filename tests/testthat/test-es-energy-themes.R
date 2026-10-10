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
source(file.path(repo_root, "R", "categories", "shared", "overall_index.R"))
source(file.path(repo_root, "R", "categories", "energy_access", "energy_access_consumption.R"))
source(file.path(repo_root, "R", "categories", "consumption", "energy_consumption.R"))
source(file.path(repo_root, "R", "categories", "energy_imports", "import_dependence.R"))
source(file.path(repo_root, "R", "categories", "energy_access", "solar_pv_potential.R"))
source(file.path(repo_root, "R", "categories", "energy_access", "wind_potential.R"))

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
# economies that produce none of any fuel. 100 * (0 - 0.007) / 0.007 is -100.00000000000001
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

test_that("economies that produce none of a fuel tie exactly on self-sufficiency", {
  built <- import_dependence_build_imports(import_balance_fixture())
  none <- built$Country %in% c("None A", "None B")

  for (fuel in c("oil", "gas", "coal", "fossil")) {
    ss <- built[[paste0(fuel, "_self_sufficiency")]]
    expect_identical(ss[none], c(0, 0), info = fuel)
    ranked <- median_scurve(ss)
    expect_identical(ranked[none][[1]], ranked[none][[2]], info = fuel)
  }
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
