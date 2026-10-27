# Guards the Technology Demand defects found before the paper build:
#   1. WEO low-emissions hydrogen summed its production inputs with its production output
#   2. BNEF NEO hydrogen ("Hydrogen") never met the pipeline's "Green Hydrogen" rows, and the
#      BNEF composite was re-ranked across every fuel and region at once
#   3. the overcapacity sheet's hydrogen block (labelled "H" + subscript two) was matched by
#      the mojibake "H???", battery cells were moved into Solar by component name, and the
#      copy to countries was keyed on the trade table's cells, so hydrogen could never land
#   4. the category listed each source's size and growth terms beside the composite built
#      from them
# and pins the six-stage layout: WEO and BNEF demand copied to all six stages, EV Outlook
# demand at Deployment, BCG segments copied to the stages they cover, and overcapacity split
# by BNEF component.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "supply_chain_stages.R"))
source(file.path(repo_root, "R", "categories", "technology_demand", "future_demand.R"))
source(file.path(repo_root, "R", "categories", "technology_demand", "overcapacity_premium.R"))

td_weo <- function() {
  tes <- expand.grid(
    PRODUCT = c("Solar", "Wind", "Coal", "Oil", "Natural gas", "Nuclear"),
    YEAR = c(2024L, 2035L),
    stringsAsFactors = FALSE
  )
  tes$FLOW <- "Total energy supply"
  tes$VALUE <- ifelse(tes$YEAR == 2035L, 100, 80) + seq_len(nrow(tes))

  h2 <- expand.grid(
    FLOW = c(
      "Hydrogen production inputs: offsite",
      "Hydrogen production: offsite",
      "Hydrogen-based fuels production inputs"
    ),
    YEAR = c(2024L, 2035L),
    stringsAsFactors = FALSE
  )
  h2$PRODUCT <- "Hydrogen: low-emissions"
  h2$VALUE <- c(0.017, 0.011, 0.001, 1.832, 1.234, 0.497)

  out <- rbind(tes[, c("PRODUCT", "FLOW", "YEAR", "VALUE")], h2[, c("PRODUCT", "FLOW", "YEAR", "VALUE")])
  out$SCENARIO <- ifelse(out$YEAR == 2024L, "Historical", "Stated Policies Scenario")
  out$CATEGORY <- "Energy"
  out
}

td_bnef <- function() {
  grid <- expand.grid(
    Region = c("US", "China", "India"),
    Fuel.type = c("Solar", "Coal", "Hydrogen"),
    stringsAsFactors = FALSE
  )
  grid$Indicator <- ifelse(grid$Fuel.type == "Hydrogen", "Final energy consumption", "Primary energy consumption")
  grid$Scenario <- "ETS"
  grid$Sector <- "All sectors"
  grid$Macro.sector <- "All"
  grid$X2024 <- c(10, 20, 5, 100, 400, 80, 0.1, 0.2, 0.05)
  grid$X2035 <- c(30, 50, 20, 60, 300, 90, 2, 1, 1)
  grid
}

td_overcapacity <- function() {
  data.frame(
    Sector = c("Battery", NA, NA, "Solar", NA, NA, "H\u2082"),
    Component = c("Separators", "Cells", "Anodes", "Polysilicon", "Cells", "Modules", "Electrolyzer stack"),
    `Overcapacity Ratio` = c(2, 3, 4, 1, 2, 3, 18),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

test_that("WEO hydrogen demand is its production, not production plus inputs", {
  weo <- future_demand_build_weo(td_weo(), country_reference = "India")
  h2 <- weo[weo$tech == "Green Hydrogen" & weo$supply_chain == "Assembly", ]

  expect_equal(h2$value[h2$variable == "2035"], 1.234)
  expect_equal(h2$value[h2$variable == "global_growth"], 1.234 - 0.011)
})

test_that("each country, cell and variable appears once when WEO and BNEF both cover a country", {
  # India and the United States are on every leg: WEO's global terms are copied to them and
  # BNEF reports them. Both legs used to name their terms "growth", "demand_size" and
  # "demand_growth", so each such key carried two rows from different sources (756 in the
  # 2026 build), and "growth" was an EJ change in one and a ratio in the other.
  ev_rows <- function(parameter, years, values, category) {
    data.frame(
      region_country = "United States", category = category, parameter = parameter,
      mode = "Cars", powertrain = "BEV", year = years, value = values, stringsAsFactors = FALSE
    )
  }
  iea_ev <- do.call(rbind, lapply(c("EV stock", "EV sales", "EV sales share"), function(p) {
    rbind(ev_rows(p, c(2022L, 2025L), c(10, 20), "Historical"), ev_rows(p, 2035L, 50, "Projection-STEPS"))
  }))
  bcg <- data.frame(
    `Energy source` = c("Solar", "Coal"), Upstream = c(925, 150), Midstream = c(2825, 300),
    Downstream = c(460, 200), check.names = FALSE
  )
  out <- future_demand(
    iea_weo = td_weo(),
    bnef_neo = td_bnef(),
    iea_ev = iea_ev,
    bcg = bcg,
    country_info = data.frame(iso3c = "USA", country = "United States"),
    country_reference = c("India", "United States", "Chile")
  )

  keys <- dplyr::count(out, Country, tech, supply_chain, variable)
  expect_equal(max(keys$n), 1)

  india_solar <- out[out$Country == "India" & out$tech == "Solar" & out$supply_chain == "Components", ]
  expect_equal(india_solar$source[india_solar$variable == "global_growth"], "IEA World Energy Outlook")
  expect_equal(india_solar$source[india_solar$variable == "growth"], "BNEF New Energy Outlook")
  # The composites the category scores keep their names, one per source.
  expect_setequal(
    india_solar$variable[india_solar$data_type == "index"],
    c("global_demand_size", "global_demand_growth", "Overall Global Demand",
      "demand_size", "demand_growth", "Overall Demand")
  )
})

test_that("BNEF hydrogen reaches Green Hydrogen and each composite is ranked within its fuel", {
  bnef <- future_demand_build_bnef(td_bnef())

  expect_false("Hydrogen" %in% bnef$tech)
  expect_true("Green Hydrogen" %in% bnef$tech)
  expect_setequal(unique(bnef$Country), c("United States", "China", "India"))

  overall <- bnef[bnef$variable == "Overall Demand" & bnef$supply_chain == "Deployment", ]
  # Within every fuel the three regions span the full 0-1 range.
  spans <- tapply(overall$value, overall$tech, function(v) c(min(v), max(v)))
  for (tech in names(spans)) {
    expect_equal(spans[[tech]], c(0, 1), info = tech)
  }
})

test_that("the overcapacity hydrogen block is read and copied to every trade country", {
  trade_tidy <- data.frame(
    Country = c("India", "India", "Chile"),
    tech = c("Solar", "Oil", "Oil"),
    supply_chain = "Assembly",
    stringsAsFactors = FALSE
  )
  out <- overcapacity_premium(td_overcapacity(), trade_tidy = trade_tidy)
  raw <- out[out$data_type == "raw" & out$Country == "India", ]

  expect_setequal(unique(out$tech), c("Batteries", "Solar", "Green Hydrogen"))
  # Battery cells stay in the battery block: mean(2, 3, 4), not mean(2, 4). Every battery
  # component is a part of the pack, so the block keeps a single Components cell.
  expect_equal(raw$value[raw$tech == "Batteries"], 3)
  expect_equal(raw$supply_chain[raw$tech == "Batteries"], "Components")
  # Solar cells (ratio 2) are the fixture's only solar component at Components.
  expect_equal(raw$value[raw$tech == "Solar" & raw$supply_chain == "Components"], 2)
  # Chile trades no solar or hydrogen goods; the global ratio still reaches it.
  expect_setequal(unique(out$tech[out$Country == "Chile"]), c("Batteries", "Solar", "Green Hydrogen"))
  h2 <- out[out$tech == "Green Hydrogen" & out$data_type == "index" & out$Country == "Chile", ]
  expect_equal(h2$value, 2^-18)
  expect_equal(h2$supply_chain, "Components")
})

test_that("overcapacity splits by component into refining, components and assembly", {
  # The BNEF sheet's order: wafers sit between polysilicon and cells, and the wind block
  # carries two nacelle rows.
  overcapacity <- data.frame(
    Sector = c("Battery", NA, NA, "Solar", NA, NA, NA, "Wind", NA, "H₂"),
    Component = c(
      "Separators", "Cells", "Anodes", "Polysilicon", "Wafers", "Cells", "Modules",
      "Offshore nacelles", "Onshore nacelles", "Electrolyzer stack"
    ),
    `Overcapacity Ratio` = c(2, 3, 4, 1, 4, 2, 3, 4, 2, 18),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  trade_tidy <- data.frame(Country = "India", tech = "Solar", stringsAsFactors = FALSE)
  out <- overcapacity_premium(overcapacity, trade_tidy = trade_tidy)
  raw <- out[out$data_type == "raw", ]
  value_of <- function(tech, stage) raw$value[raw$tech == tech & raw$supply_chain == stage]

  expect_true(all(out$supply_chain %in% SUPPLY_CHAIN_STAGES))
  expect_setequal(raw$supply_chain[raw$tech == "Solar"], c("Refining", "Components", "Assembly"))
  # Polysilicon alone at Refining, wafers and cells averaged at Components, modules at Assembly.
  expect_equal(value_of("Solar", "Refining"), 1)
  expect_equal(value_of("Solar", "Components"), mean(c(4, 2)))
  expect_equal(value_of("Solar", "Assembly"), 3)
  expect_equal(value_of("Wind", "Assembly"), mean(c(4, 2)))
  expect_equal(sum(raw$tech == "Wind"), 1L)
  idx <- out[out$data_type == "index" & out$tech == "Solar" & out$supply_chain == "Refining", ]
  expect_equal(idx$value, 2^-1)
  expect_match(
    raw$explanation[raw$tech == "Solar" & raw$supply_chain == "Components"],
    "components at this stage (Wafers, Cells)", fixed = TRUE
  )

  # A component the stage map does not know stops the build instead of being dropped.
  odd <- overcapacity
  odd$Component[[5]] <- "Ingots"
  expect_error(overcapacity_premium(odd, trade_tidy = trade_tidy), "no supply-chain stage for BNEF component", fixed = TRUE)
})

test_that("WEO and BNEF demand is copied to all six stages and says so", {
  weo <- future_demand_build_weo(td_weo(), country_reference = c("India", "Chile"))
  bnef <- future_demand_build_bnef(td_bnef())

  for (tbl in list(weo, bnef)) {
    expect_setequal(unique(tbl$supply_chain), SUPPLY_CHAIN_STAGES)
    per_key <- dplyr::summarize(
      dplyr::group_by(tbl, Country, tech, variable),
      n = dplyr::n(), n_values = dplyr::n_distinct(value), .groups = "drop"
    )
    expect_true(all(per_key$n == 6L))
    expect_true(all(per_key$n_values == 1L))
    expect_true(all(grepl("the same value at every supply-chain stage", tbl$explanation, fixed = TRUE)))
  }
})

test_that("the category scores one composite per source", {
  def <- yaml::read_yaml(file.path(repo_root, "config", "index_definition.yml"))
  components <- unlist(def$overall_variables[["Overall Technology Demand Index"]]$components)

  expect_setequal(
    components,
    c("Overall Global Demand", "Overall Demand", "Overall Addressable Market", "Overall Overcapacity")
  )
})

test_that("a BCG cell with no market size drops out instead of ranking as zero", {
  bcg <- data.frame(
    `Energy source` = c("Batteries", "Solar", "Oil"),
    Upstream = c(1220, 925, 1250),
    Midstream = c(4400, 2825, 2050),
    Downstream = c(0, 460, 3600),
    check.names = FALSE
  )
  out <- future_demand_build_bcg(bcg)

  expect_false(any(out$tech == "Batteries" & out$supply_chain == "Deployment"))
  idx <- out[out$variable == "Overall Addressable Market", ]
  # Eight sized source cells, each copied to the stages its segment covers: Batteries
  # Upstream (3 stages) + Midstream (2), Solar and Oil 3 + 2 + 1 each.
  expect_equal(nrow(idx), 17)
  # The smallest sized cell (Solar Downstream, now Deployment) takes the bottom rank.
  expect_equal(idx$value[idx$tech == "Solar" & idx$supply_chain == "Deployment"], 0)
})

test_that("each BCG segment is copied to the stages it covers and ranked once", {
  bcg <- data.frame(
    `Energy source` = c("Batteries", "Solar", "Oil"),
    Upstream = c(1220, 925, 1250),
    Midstream = c(4400, 2825, 2050),
    Downstream = c(0, 460, 3600),
    check.names = FALSE
  )
  out <- future_demand_build_bcg(bcg)
  sam <- out[out$variable == "SAM", ]
  value_of <- function(tech, stage) sam$value[sam$tech == tech & sam$supply_chain == stage]

  expect_true(all(out$supply_chain %in% SUPPLY_CHAIN_STAGES))
  for (stage in c("Extraction", "Refining", "Recovery")) {
    expect_equal(value_of("Solar", stage), 925, info = stage)
  }
  for (stage in c("Components", "Assembly")) {
    expect_equal(value_of("Solar", stage), 2825, info = stage)
  }
  expect_equal(value_of("Solar", "Deployment"), 460)
  expect_match(
    sam$explanation[sam$tech == "Solar" & sam$supply_chain == "Refining"],
    "copied to each stage it covers (Extraction, Refining, Recovery)", fixed = TRUE
  )

  # The index is ranked across the eight source cells before the copy, so each stage of a
  # segment carries the segment's rank: Batteries Midstream, the largest, is 1 at both stages.
  idx <- out[out$variable == "Overall Addressable Market", ]
  expect_equal(idx$value[idx$tech == "Batteries" & idx$supply_chain %in% c("Components", "Assembly")], c(1, 1))
  ranks <- median_scurve(c(1220, 925, 1250, 4400, 2825, 2050, 460, 3600), gamma = 0.5)
  expect_equal(idx$value[idx$tech == "Oil" & idx$supply_chain == "Recovery"], ranks[[3]])
})

test_that("the full theme carries only the six stages, with EV demand at Deployment", {
  ev_rows <- function(parameter, years, values, category) {
    data.frame(
      region_country = "United States", category = category, parameter = parameter,
      mode = "Cars", powertrain = "BEV", year = years, value = values, stringsAsFactors = FALSE
    )
  }
  iea_ev <- do.call(rbind, lapply(c("EV stock", "EV sales", "EV sales share"), function(p) {
    rbind(ev_rows(p, c(2022L, 2025L), c(10, 20), "Historical"), ev_rows(p, 2035L, 50, "Projection-STEPS"))
  }))
  bcg <- data.frame(
    `Energy source` = "Solar", Upstream = 925, Midstream = 2825, Downstream = 460, check.names = FALSE
  )
  out <- future_demand(
    iea_weo = td_weo(), bnef_neo = td_bnef(), iea_ev = iea_ev, bcg = bcg,
    country_info = data.frame(iso3c = "USA", country = "United States"),
    country_reference = c("India", "United States")
  )

  expect_true(all(out$supply_chain %in% SUPPLY_CHAIN_STAGES))
  ev <- out[out$source == "IEA Global EV Outlook 2026", ]
  expect_gt(nrow(ev), 0)
  expect_equal(unique(ev$supply_chain), "Deployment")
})
