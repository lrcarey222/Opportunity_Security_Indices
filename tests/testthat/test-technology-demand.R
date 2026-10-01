# Guards the Technology Demand defects found before the paper build:
#   1. WEO low-emissions hydrogen summed its production inputs with its production output
#   2. BNEF NEO hydrogen ("Hydrogen") never met the pipeline's "Green Hydrogen" rows, and the
#      BNEF composite was re-ranked across every fuel and region at once
#   3. the overcapacity sheet's hydrogen block (labelled "H" + subscript two) was matched by
#      the mojibake "H???", battery cells were moved into Solar by component name, and the
#      copy to countries was keyed on the trade table's cells, so hydrogen could never land
#   4. the category listed each source's size and growth terms beside the composite built
#      from them

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
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
  h2 <- weo[weo$tech == "Green Hydrogen" & weo$supply_chain == "Midstream", ]

  expect_equal(h2$value[h2$variable == "2035"], 1.234)
  expect_equal(h2$value[h2$variable == "growth"], 1.234 - 0.011)
})

test_that("BNEF hydrogen reaches Green Hydrogen and each composite is ranked within its fuel", {
  bnef <- future_demand_build_bnef(td_bnef())

  expect_false("Hydrogen" %in% bnef$tech)
  expect_true("Green Hydrogen" %in% bnef$tech)
  expect_setequal(unique(bnef$Country), c("United States", "China", "India"))

  overall <- bnef[bnef$variable == "Overall Demand" & bnef$supply_chain == "Midstream", ]
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
    supply_chain = "Midstream",
    stringsAsFactors = FALSE
  )
  out <- overcapacity_premium(td_overcapacity(), trade_tidy = trade_tidy)
  raw <- out[out$data_type == "raw" & out$Country == "India", ]

  expect_setequal(unique(out$tech), c("Batteries", "Solar", "Green Hydrogen"))
  # Battery cells stay in the battery block: mean(2, 3, 4), not mean(2, 4).
  expect_equal(raw$value[raw$tech == "Batteries"], 3)
  expect_equal(raw$value[raw$tech == "Solar"], 2)
  # Chile trades no solar or hydrogen goods; the global ratio still reaches it.
  expect_setequal(out$tech[out$Country == "Chile"], c("Batteries", "Solar", "Green Hydrogen"))
  h2 <- out$value[out$tech == "Green Hydrogen" & out$data_type == "index" & out$Country == "Chile"]
  expect_equal(h2, 2^-18)
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

  expect_false(any(out$tech == "Batteries" & out$supply_chain == "Downstream"))
  idx <- out[out$variable == "Overall Addressable Market", ]
  expect_equal(nrow(idx), 8)
  # The smallest sized cell (Solar Downstream) now takes the bottom rank.
  expect_equal(idx$value[idx$tech == "Solar" & idx$supply_chain == "Downstream"], 0)
})
