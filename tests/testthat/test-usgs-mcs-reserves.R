# Critical-mineral reserves from USGS Mineral Commodity Summaries (R/categories/reserves/usgs_mcs.R)
# and the Reserves theme built from them (reserves() in R/categories/reserves/reserves.R).
#
# Guards the decisions of the 2026-10-09 switch from the EI mineral sheets to USGS MCS 2026: every
# USGS value code is read explicitly (a withheld or unquantified holder is missing, never a zero),
# the published erratum to Brazil's rare earths is applied to the data release, aggregates make
# the world denominator but are never holders, and holders the EI roster omits are kept.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "levels.R"))
source(file.path(repo_root, "R", "utils", "iea_critical_minerals.R"))
source(file.path(repo_root, "R", "categories", "shared", "overall_index.R"))
source(file.path(repo_root, "R", "categories", "reserves", "reserves.R"))
source(file.path(repo_root, "R", "categories", "reserves", "usgs_mcs.R"))

# No index definition: the theme's own "Overall ..." rows are not what these tests check.
withr::local_options(list(opportunity_security.index_definition = NULL))

# A miniature MCS 2026 commodity table, as the scripts read it (all text, "NA" kept as a code).
# Values are printed the USGS way; "—" is the em dash USGS prints for zero.
mcs_fixture <- function() {
  rows <- function(commodity, unit, country, value, detail = "Reserves", notes = "", statistics = "Reserves") {
    tibble::tibble(
      `MCS chapter` = toupper(commodity), Section = "World Mine Production and Reserves",
      Commodity = commodity, Country = country, Statistics = statistics,
      Statistics_detail = ifelse(country == "World total", paste0(detail, ": rounded"), detail),
      Unit = unit, Year = "2025", Value = value, Notes = notes
    )
  }
  dplyr::bind_rows(
    rows("Rare Earths", "metric tons",
         c("United States", "Brazil", "Burma", "China", "Tanzania", "Other countries", "World total"),
         c("1,900,000", "21,000,000", "NA", "44,000,000", "890,000", "NA", ">85,000,000")),
    # Production rows of the same chapter are not reserves.
    rows("Rare Earths", "metric tons", "China", "270,000", detail = "Mine production", statistics = "Production"),
    rows("Copper", "thousand metric tons",
         c("Chile", "Peru", "Germany", "Congo (Kinshasa)", "Other countries", "World total"),
         c("180,000", "85,000", "—", "80,000", "210,000", "550,000")),
    rows("Graphite (Natural)", "metric tons",
         c("China", "Brazil", "Ukraine", "Korea, Republic of", "World total"),
         c("100,000,000", "74,000,000", "s", "1,800,000", "310,000,000"),
         notes = c("", "", "Included in “World total.”", "", "")),
    rows("Lithium", "metric tons",
         c("United States", "Chile", "Australia", "Other countries", "World total"),
         c("W", "9,200,000", "8,400,000", "2,400,000", "37,000,000")),
    rows("Manganese", "thousand metric tons",
         c("Côte d’Ivoire", "South Africa", "Brazil", "Other countries", "World total"),
         c("NA", "550,000", "300,000", "Small", "1,700,000")),
    rows("Abrasives (Manufactured)", "metric tons", "United States", "10,000")
  )
}

mcs_specs_fixture <- function() {
  Filter(
    function(s) s$tech_name %in% c("Rare Earths", "Copper", "Graphite", "Lithium", "Manganese"),
    reserves_usgs_specs()
  )
}

row_of <- function(u, tech, iso3) u[u$tech == tech & u$ISO3166_alpha3 %in% iso3, , drop = FALSE]

test_that("the theme reads the nine minerals from USGS and only the fuels from EI", {
  expect_setequal(vapply(reserves_specs(), function(s) s$tech_name, character(1)), c("Oil", "Gas", "Coal"))
  specs <- reserves_usgs_specs()
  expect_setequal(vapply(specs, function(s) s$tech_name, character(1)), RESERVES_MINERALS)
  expect_equal(vapply(specs, function(s) s$commodity, character(1))[9], "Platinum-Group Metals")
  # Units of the EI sheets the theme read before: thousand tonnes, PGMs in kilograms.
  expect_setequal(vapply(specs, function(s) s$unit_desc, character(1)), c("Thousand tonnes", "Kilograms"))
})

test_that("every USGS value code is read explicitly", {
  p <- usgs_mcs_parse_value(
    c("1,900,000", ">9,100,000", "<200,000", "—", "NA", "W", "s", "Small", "Large", "XX", "", "Huge", "5900000"),
    notes = c(rep("", 6), "Included in “World total.”", rep("", 6))
  )
  expect_equal(
    p$status,
    c("reported", "lower bound", "upper bound", "zero", "not available", "withheld", "in world total",
      "small", "qualitative", "not applicable", "not available", NA, "reported")
  )
  expect_equal(p$value, c(1900000, 9100000, NA, 0, NA, NA, NA, NA, NA, NA, NA, NA, 5900000))
})

test_that("usgs_mcs_reserves keeps aggregates apart, maps USGS names and applies the erratum", {
  u <- usgs_mcs_reserves(mcs_fixture(), specs = mcs_specs_fixture())

  expect_setequal(unique(u$tech), c("Rare Earths", "Copper", "Graphite", "Lithium", "Manganese"))
  expect_equal(unique(u$data_year), 2025L)
  expect_equal(unique(u$edition), 2026L)
  # Reserves rows only: the production row and the other commodity are not read.
  expect_equal(nrow(u[u$tech == "Rare Earths", ]), 7)
  expect_false(any(u$commodity == "Abrasives (Manufactured)"))

  # MCS 2026 ver. 1.3 revised Brazil's rare earths from 21 Mt to 11 Mt and the world to >75 Mt;
  # the data release still prints the version 1.0 figures. Values in thousand tonnes.
  expect_equal(row_of(u, "Rare Earths", "BRA")$value, 11000)
  expect_match(row_of(u, "Rare Earths", "BRA")$notes, "Corrected from 21,000,000")
  world_ree <- u[u$tech == "Rare Earths" & u$row_type == "world", ]
  expect_equal(world_ree$value, 75000)
  expect_equal(world_ree$status, "lower bound")
  expect_equal(row_of(u, "Rare Earths", "CHN")$value, 44000)

  # USGS names to ISO3 and canonical names.
  expect_equal(row_of(u, "Rare Earths", "MMR")$label, "Burma")
  expect_equal(row_of(u, "Copper", "COD")$label, "Congo (Kinshasa)")
  expect_equal(row_of(u, "Graphite", "KOR")$label, "Korea, Republic of")
  expect_equal(nrow(row_of(u, "Manganese", "CIV")), 1)

  # "Other countries" and "World total" are aggregates, never holders.
  expect_true(all(is.na(u$ISO3166_alpha3[u$row_type != "country"])))
  expect_setequal(u$label[u$row_type == "other"], "Other countries")
  expect_equal(sum(u$row_type == "world"), 5)

  # Codes: an observed zero, and unquantified holders that must not become zeros.
  expect_equal(row_of(u, "Copper", "DEU")$value, 0)
  expect_equal(row_of(u, "Copper", "DEU")$status, "zero")
  expect_true(is.na(row_of(u, "Lithium", "USA")$value))
  expect_equal(row_of(u, "Lithium", "USA")$status, "withheld")
  expect_equal(row_of(u, "Graphite", "UKR")$status, "in world total")
  expect_equal(row_of(u, "Rare Earths", "MMR")$status, "not available")
  expect_equal(u$status[u$tech == "Manganese" & u$row_type == "other"], "small")
  unq <- usgs_mcs_unquantified(u)
  expect_setequal(paste(unq$tech, unq$ISO3166_alpha3),
                  c("Rare Earths MMR", "Graphite UKR", "Lithium USA", "Manganese CIV"))
})

test_that("the erratum is a no-op on a corrected release and stops on any other value", {
  fixed <- mcs_fixture()
  fixed$Value[fixed$Commodity == "Rare Earths" & fixed$Country == "Brazil"] <- "11,000,000"
  u <- usgs_mcs_reserves(fixed, specs = mcs_specs_fixture())
  expect_equal(row_of(u, "Rare Earths", "BRA")$value, 11000)
  expect_false(grepl("Corrected", row_of(u, "Rare Earths", "BRA")$notes))

  moved <- mcs_fixture()
  moved$Value[moved$Commodity == "Rare Earths" & moved$Country == "Brazil"] <- "15,000,000"
  expect_error(usgs_mcs_reserves(moved, specs = mcs_specs_fixture()), "neither the published")
})

test_that("usgs_mcs_reserves stops on what it cannot read safely", {
  specs <- mcs_specs_fixture()
  unknown <- mcs_fixture()
  unknown$Value[unknown$Commodity == "Copper" & unknown$Country == "Peru"] <- "Huge"
  expect_error(usgs_mcs_reserves(unknown, specs = specs), "unrecognised value code")

  restated <- mcs_fixture()
  restated$Unit[restated$Commodity == "Copper" & restated$Statistics == "Reserves"] <- "metric tons"
  expect_error(usgs_mcs_reserves(restated, specs = specs), "update reserves_usgs_specs")

  no_world <- mcs_fixture()
  no_world <- no_world[!(no_world$Commodity == "Copper" & no_world$Country == "World total"), ]
  expect_error(usgs_mcs_reserves(no_world, specs = specs), "World total")

  twice <- mcs_fixture()
  twice$Country[twice$Commodity == "Copper" & twice$Country == "Peru"] <- "Chile"
  expect_error(usgs_mcs_reserves(twice, specs = specs), "list one economy twice")

  atlantis <- mcs_fixture()
  atlantis$Country[atlantis$Commodity == "Copper" & atlantis$Country == "Peru"] <- "Atlantis"
  expect_error(usgs_mcs_reserves(atlantis, specs = specs), "no ISO3 code for Atlantis")

  expect_error(usgs_mcs_reserves(mcs_fixture()[, -9], specs = specs), "lacks column")
})

test_that("the world is the larger of the rounded World total and the listed rows", {
  w <- usgs_mcs_world_reserves(usgs_mcs_reserves(mcs_fixture(), specs = mcs_specs_fixture()))
  world <- function(t) w$world[w$tech == t]
  expect_equal(world("Copper"), 555000)       # listed 555,000 > rounded total 550,000
  expect_equal(world("Rare Earths"), 75000)   # ">75 Mt" > listed 57.79 Mt (Burma, India ... unquantified)
  expect_equal(world("Graphite"), 310000)     # Ukraine ("s") is inside the World total only
  expect_equal(world("Manganese"), 1700000)   # "Small" adds nothing to the listed sum
})

# EI roster: Tanzania, the DR Congo, South Africa, Korea, Ukraine and Myanmar are outside it.
usgs_ei_fixture <- function() {
  tibble::tibble(
    Country = c("Chile", "China", "Australia", "Peru", "Brazil", "United States", "Germany", "Venezuela", "Total World"),
    Year = 2025,
    ISO3166_alpha3 = c("CHL", "CHN", "AUS", "PER", "BRA", "USA", "DEU", "VEN", NA)
  )
}

usgs_oil_input <- function() {
  list(
    sheet = "Oil - Proved reserves history", nm_col = "Thousand million barrels",
    val_col = "2020...42", tech_name = "Oil", unit_desc = "Thousand million barrels", data_year = 2020,
    data = tibble::tibble(
      `Thousand million barrels` = c("Venezuela", "China", "United States", "Total World"),
      `2020...42` = c(303.8, 26.0, 68.8, 1732.4)
    )
  )
}

usgs_demand_fixture <- function() {
  demand <- tibble::tribble(
    ~Mineral,                 ~Sector.Country,        ~X2025, ~X2035,
    "Neodymium",              "Wind",                     50,     40,
    "Neodymium",              "Electric vehicles",        50,     60,
    "Copper",                 "Wind",                     10,     10,
    "Copper",                 "Electric vehicles",        30,     40,
    "Copper",                 "Electricity networks",     60,     50,
    "Battery-grade graphite", "Electric vehicles",        60,     70,
    "Battery-grade graphite", "Grid battery storage",     40,     30,
    "Lithium",                "Electric vehicles",        80,     90,
    "Lithium",                "Grid battery storage",     20,     10,
    "Manganese",              "Electric vehicles",        70,     70,
    "Manganese",              "Wind",                     30,     30
  ) %>%
    dplyr::mutate(Pillar = "3.1 Energy demand by tech")
  reserves_build_mineral_demand_clean(as.data.frame(demand))
}

usgs_theme_fixture <- function() {
  suppressWarnings(reserves(
    ei = usgs_ei_fixture(),
    reserve_inputs = list(usgs_oil_input()),
    mineral_demand_clean = usgs_demand_fixture(),
    usgs_reserves = usgs_mcs_reserves(mcs_fixture(), specs = mcs_specs_fixture())
  )) %>%
    dplyr::ungroup()
}

test_that("reserves() builds the mineral variables from USGS with the same shape and anchor", {
  out <- usgs_theme_fixture()
  val <- function(variable, iso3, type) {
    out$value[out$variable == variable & out$ISO3166_alpha3 == iso3 & out$data_type == type]
  }

  expect_true(all(c("Rare Earths Reserves", "Copper Reserves", "Graphite Reserves", "Lithium Reserves",
                    "Manganese Reserves", "Wind Reserves", "Electric Vehicles Reserves",
                    "Batteries Reserves", "Electric Grid Reserves", "Oil Reserves") %in% out$variable))
  key <- out %>% dplyr::count(ISO3166_alpha3, tech, supply_chain, variable, data_type) %>% dplyr::filter(n > 1)
  expect_equal(nrow(key), 0)

  # Raw values in thousand tonnes; the index is the zero-anchored S-curve of the positive holders.
  expect_equal(val("Rare Earths Reserves", "BRA", "raw"), 11000)
  ree <- out %>% dplyr::filter(variable == "Rare Earths Reserves", data_type == "index", !is.na(value))
  ree_by <- stats::setNames(ree$value, ree$ISO3166_alpha3)
  expect_equal(unname(ree_by["CHN"]), 1)
  expect_true(ree_by["CHN"] > ree_by["BRA"] && ree_by["BRA"] > ree_by["USA"] && ree_by["USA"] > ree_by["TZA"])
  expect_gt(unname(ree_by["TZA"]), 0)
  expect_equal(unname(ree_by), median_scurve(c(0, 1900, 11000, 44000, 890))[-1][match(names(ree_by), c("USA", "BRA", "CHN", "TZA"))])

  # Holders outside the EI roster are kept (an inner join on it used to drop them), with names.
  expect_equal(out$Country[out$ISO3166_alpha3 == "TZA"][1], "Tanzania")
  expect_gt(val("Copper Reserves", "COD", "index"), 0)
  expect_gt(val("Wind Reserves", "TZA", "index"), 0)

  # An observed zero scores 0; a holder USGS withholds ("W") or does not quantify stays missing.
  expect_equal(val("Copper Reserves", "DEU", "raw"), 0)
  expect_equal(val("Copper Reserves", "DEU", "index"), 0)
  expect_true(is.na(val("Lithium Reserves", "USA", "raw")))
  expect_true(is.na(val("Lithium Reserves", "USA", "index")))
  expect_true(is.na(val("Graphite Reserves", "UKR", "index")))
  expect_true(is.na(val("Copper Reserves", "VEN", "index"))) # on the roster, not listed by USGS

  # Provenance: the source and the vintage, unit and basis of the raw values.
  ree_raw <- out %>% dplyr::filter(variable == "Rare Earths Reserves", data_type == "raw")
  expect_equal(unique(ree_raw$source), "USGS Mineral Commodity Summaries 2026")
  expect_equal(length(unique(ree_raw$explanation)), 1)
  expect_match(unique(ree_raw$explanation), "Thousand tonnes, rare-earth-oxide equivalent) at end-2025")
  expect_equal(unique(out$source[out$variable == "Wind Reserves"]), "USGS Mineral Commodity Summaries 2026")

  # The fuels stay on EI.
  expect_equal(val("Oil Reserves", "VEN", "raw"), 303.8)
  expect_match(unique(out$source[out$variable == "Oil Reserves"]), "^EI Statistical Review")
})

test_that("technology rollups weight USGS minerals by demand and count unlisted minerals as zero", {
  out <- usgs_theme_fixture()
  wind <- out %>% dplyr::filter(variable == "Wind Reserves", data_type == "index", !is.na(value))
  wind_by <- stats::setNames(wind$value, wind$ISO3166_alpha3)
  # Brazil: rare earths and manganese; Chile: copper only.
  expect_gt(unname(wind_by["BRA"]), unname(wind_by["CHL"]))
  expect_true(all(wind$value > 0))
  # Germany's observed copper zero gives it no Wind reserves.
  expect_false("DEU" %in% names(wind_by))
  grid <- out %>% dplyr::filter(variable == "Electric Grid Reserves", data_type == "index", !is.na(value))
  expect_setequal(grid$ISO3166_alpha3, c("CHL", "PER", "COD"))
})

test_that("reserves() takes each mineral from one source", {
  ei_copper <- list(
    sheet = "Copper P-R", nm_col = "Thousand tonnes", val_col = "At end of 2025",
    tech_name = "Copper", unit_desc = "Thousand tonnes",
    data = tibble::tibble(`Thousand tonnes` = c("Chile", "Total World"), `At end of 2025` = c(190000, 831000))
  )
  expect_error(
    suppressWarnings(reserves(
      usgs_ei_fixture(), list(usgs_oil_input(), ei_copper), usgs_demand_fixture(),
      usgs_reserves = usgs_mcs_reserves(mcs_fixture(), specs = mcs_specs_fixture())
    )),
    "both an EI sheet and USGS"
  )
  expect_error(
    suppressWarnings(reserves(usgs_ei_fixture(), list(usgs_oil_input()), usgs_demand_fixture())),
    "no mineral reserves"
  )
})

test_that("scripts/10 and scripts/13 read the staged USGS file for the minerals", {
  for (script in c("10_build_themes.R", "13_build_coalition.R")) {
    text <- paste(readLines(file.path(repo_root, "scripts", script), warn = FALSE), collapse = "\n")
    expect_match(text, 'file.path(raw_data_path, "MCS2026_World_Data.csv")', fixed = TRUE, info = script)
    expect_match(text, "usgs_mcs_reserves(", fixed = TRUE, info = script)
    expect_match(text, "usgs_reserves = usgs_reserves", fixed = TRUE, info = script)
    expect_match(text, 'encoding = "windows-1252"', fixed = TRUE, info = script)
  }
})

test_that("the staged MCS 2026 release parses with every holder matched", {
  path <- file.path(repo_root, "data", "raw", "MCS2026_World_Data.csv")
  skip_if_not(file.exists(path), "data/raw/MCS2026_World_Data.csv is not staged")
  mcs <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()),
                         locale = readr::locale(encoding = "windows-1252"), na = character(), progress = FALSE)
  u <- usgs_mcs_reserves(mcs)
  expect_setequal(unique(u$tech), RESERVES_MINERALS)
  expect_equal(unique(u$edition), 2026L)
  expect_equal(row_of(u, "Rare Earths", "BRA")$value, 11000)  # the ver. 1.3 erratum
  expect_equal(row_of(u, "Manganese", "BRA")$value, 300000)   # not on EI's manganese sheet
  expect_equal(row_of(u, "Nickel", "BRA")$value, 16000)       # nor on its nickel sheet
  w <- usgs_mcs_world_reserves(u)
  expect_equal(w$world[w$tech == "Rare Earths"], 75290)
  expect_equal(w$world[w$tech == "Manganese"], 1798000)       # the release prints 1,700,000, the report 1,800,000
  expect_false(any(u$status == "withheld" & u$row_type == "country" & !is.na(u$value)))
})
