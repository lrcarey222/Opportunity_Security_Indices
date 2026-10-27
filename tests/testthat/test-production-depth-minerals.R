# Production depth and momentum, mineral Extraction and Refining cells (EO Production).
#
# The raw level of these cells used to be the mean of a country's mined kilotonnes over the
# technology's minerals, labelled as EI data, and the size index ranked every country-mineral
# row of the technology together before an unweighted mean. Copper tonnage swamped everything
# else, a country scored on how many of the minerals it mines, and a country mining none had
# an undefined growth that the index then filled with the cell average. These tests pin the
# replacement: each mineral as a share of world supply, combined with the technology's
# demand shares, and growth as the absolute change in that quantity.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "levels.R"))
source(file.path(repo_root, "R", "utils", "supply_chain_stages.R"))
source(file.path(repo_root, "R", "utils", "iea_critical_minerals.R"))
source(file.path(repo_root, "R", "categories", "shared", "overall_index.R"))
source(file.path(repo_root, "R", "categories", "reserves", "reserves.R"))
source(file.path(repo_root, "R", "categories", "production", "production_depth_momentum.R"))

withr::local_options(list(opportunity_security.index_definition = NULL))

# Electric vehicles use copper and lithium. EVs take a quarter of copper demand and three
# quarters of lithium demand, so the weights are 0.25 and 0.75. Copper is mined in tonnages
# fifty times lithium's. Japan appears only on a refining sheet, so it mines neither.
pdm_critical <- function() {
  demand <- tibble::tribble(
    ~Mineral,  ~`Sector.Country`,       ~X2025, ~X2035,
    "Copper",  "Electric vehicles",         10,     20,
    "Copper",  "Electricity networks",      30,     40,
    "Lithium", "Electric vehicles",         30,     60,
    "Lithium", "Grid battery storage",      10,     20
  )
  demand$Pillar <- "3.1 Energy demand by tech"

  supply <- tibble::tribble(
    ~Mineral,             ~`Sector.Country`, ~X2025, ~X2035,
    "Copper - Mining",    "Chile",             5000,   4000,
    "Copper - Mining",    "Peru",              3000,   3500,
    "Copper - Mining",    "Rest of world",     2000,   2000,
    "Copper - Mining",    "Total",            10000,   9500,
    "Lithium - Mining",   "Australia",          120,    200,
    "Lithium - Mining",   "Chile",               30,     60,
    "Lithium - Mining",   "Rest of world",       50,    100,
    "Lithium - Mining",   "Total",              200,    360,
    "Copper - Refining",  "Japan",             1500,   1500,
    "Copper - Refining",  "Chile",             2000,   2000,
    "Copper - Refining",  "Rest of world",     6500,   7000
  )
  supply$Pillar <- "2 Total supply for key minerals"

  as.data.frame(dplyr::bind_rows(demand, supply))
}

pdm_country_info <- function() {
  tibble::tibble(country = c("Chile", "Peru", "Australia", "Japan", "Norway"))
}

pdm_ei <- function() {
  tidyr::expand_grid(
    Country = c("Chile", "Australia"),
    Year = c(2015L, 2020L, 2025L),
    Var = "coalprod_ej"
  ) %>%
    dplyr::mutate(Value = c(1, 2, 3, 10, 12, 15))
}

pdm_value <- function(tbl, country, variable, tech = "Electric Vehicles", stage = "Extraction") {
  hit <- tbl[tbl$Country == country & tbl$tech == tech & tbl$supply_chain == stage & tbl$variable == variable, ]
  if (nrow(hit) != 1) stop("expected one row for ", country, "/", tech, "/", stage, "/", variable)
  hit$value
}

test_that("the raw level is the demand-weighted share of world mined supply, not a mean of kt", {
  critical <- pdm_critical()
  out <- production_depth_momentum(pdm_ei(), critical, reserves_build_mineral_demand_clean(critical), pdm_country_info())

  # 0.25 x Chile's 50% of copper + 0.75 x its 15% of lithium; the mean of its tonnages was
  # (5000 + 30) / 2 = 2515 kt, ahead of Australia's (0 + 120) / 2 = 60.
  expect_equal(pdm_value(out, "Chile", "production_latest"), 0.25 * 0.5 + 0.75 * 0.15)
  expect_equal(pdm_value(out, "Peru", "production_latest"), 0.25 * 0.3)
  expect_equal(pdm_value(out, "Australia", "production_latest"), 0.75 * 0.6)
  expect_equal(pdm_value(out, "Japan", "production_latest"), 0)

  # Australia mines 60% of the lithium EVs mostly need; Peru 30% of copper, a quarter of it.
  expect_gt(pdm_value(out, "Australia", "size"), pdm_value(out, "Chile", "size"))
  expect_gt(pdm_value(out, "Chile", "size"), pdm_value(out, "Peru", "size"))
  expect_gt(pdm_value(out, "Peru", "size"), pdm_value(out, "Japan", "size"))

  # A one-mineral technology is that mineral's world share.
  expect_equal(pdm_value(out, "Peru", "production_latest", tech = "Electric Grid"), 0.3)
})

test_that("growth is the absolute change against base-year world supply, defined for non-producers", {
  critical <- pdm_critical()
  out <- production_depth_momentum(pdm_ei(), critical, reserves_build_mineral_demand_clean(critical), pdm_country_info())
  ms <- production_depth_momentum_build_mineral_production(
    production_depth_momentum_build_mineral_supply(critical, reserves_build_mineral_demand_clean(critical), pdm_country_info())
  )
  ev <- ms[ms$tech == "Electric Vehicles" & ms$supply_chain == "Extraction", ]

  # Both years over the 2025 world totals, so a shrinking copper market is not read as a gain.
  expect_equal(ev$supply_growth[ev$Country == "Chile"], (0.25 * 4000 / 10000 + 0.75 * 60 / 200) - 0.2375)
  expect_equal(ev$supply_growth[ev$Country == "Australia"], 0.75 * (200 - 120) / 200)
  expect_equal(ev$supply_growth[ev$Country == "Japan"], 0)

  # Japan mines none of the minerals: observed, not left to the missing-data rule.
  japan <- out[out$Country == "Japan" & out$tech == "Electric Vehicles" & out$supply_chain == "Extraction", ]
  expect_setequal(japan$variable, c("production_latest", "size", "growth_abs", "Overall Production"))
  expect_false(anyNA(japan$value))
  expect_gt(pdm_value(out, "Australia", "growth_abs"), pdm_value(out, "Japan", "growth_abs"))
  # Chile's copper output falls, so in the copper-only cell it ranks below a country that
  # mines nothing, as a shrinking producer does in the EI cells.
  expect_lt(
    pdm_value(out, "Chile", "growth_abs", tech = "Electric Grid"),
    pdm_value(out, "Japan", "growth_abs", tech = "Electric Grid")
  )
})

test_that("mineral rows are labelled as IEA mined supply and the EI rows name the EI edition", {
  critical <- pdm_critical()
  out <- production_depth_momentum(pdm_ei(), critical, reserves_build_mineral_demand_clean(critical), pdm_country_info())

  minerals <- out[out$tech %in% c("Electric Vehicles", "Electric Grid", "Batteries") & out$supply_chain == "Extraction", ]
  expect_gt(nrow(minerals), 0)
  expect_equal(unique(minerals$source), "IEA Critical Minerals Database")
  raw <- minerals$explanation[minerals$variable == "production_latest" & minerals$tech == "Electric Vehicles"]
  expect_true(all(grepl("Share of world mined supply", raw)))
  growth <- minerals$explanation[minerals$variable == "growth_abs" & minerals$tech == "Electric Vehicles"]
  expect_true(all(grepl("2025-2035", growth)))

  coal <- out[out$tech == "Coal" & out$supply_chain == "Extraction", ]
  # The same edition label as the other EI themes (2025 data = the 2026 edition).
  expect_equal(unique(coal$source), "EI Statistical Review of World Energy (2026 edition)")
  expect_equal(pdm_value(out, "Australia", "production_latest", tech = "Coal"), 15)
})

test_that("refined supply is scored as Refining rows instead of being dropped", {
  critical <- pdm_critical()
  out <- production_depth_momentum(pdm_ei(), critical, reserves_build_mineral_demand_clean(critical), pdm_country_info())

  expect_silent(assert_supply_chain_stages(out$supply_chain, "production_depth_momentum"))
  expect_setequal(unique(out$supply_chain), c("Extraction", "Refining"))

  # Only copper has refining rows, so EV Refining is copper's refined supply alone: Japan
  # refines 15% of it and mines nothing, Chile refines 20%, Peru none.
  expect_equal(pdm_value(out, "Japan", "production_latest", stage = "Refining"), 1500 / 10000)
  expect_equal(pdm_value(out, "Chile", "production_latest", stage = "Refining"), 2000 / 10000)
  expect_equal(pdm_value(out, "Peru", "production_latest", stage = "Refining"), 0)
  expect_gt(pdm_value(out, "Japan", "size", stage = "Refining"), pdm_value(out, "Peru", "size", stage = "Refining"))
  expect_equal(pdm_value(out, "Japan", "production_latest"), 0)

  refining <- out[out$supply_chain == "Refining", ]
  expect_setequal(refining$variable, c("production_latest", "size", "growth_abs", "Overall Production"))
  expect_equal(unique(refining$source), "IEA Critical Minerals Database")
  raw <- refining$explanation[refining$variable == "production_latest"]
  expect_true(all(grepl("Share of world refined supply", raw)))
})
