# Climate TRACE sector emissions (user request of 2026-10-17): the API parsers and asset aggregation
# (R/categories/climate/climate_trace.R), the technology x stage map (config/climate_trace_map.yml), the
# IPCC lifecycle rows behind tech_ghg_tbl, and the sector-climate chunk of the Sector bundle
# (R/dashboard/sector/sector_climate.R) with its contract checks and decoded parity. No network.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "categories", "climate", "climate_trace.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(repo_root, "R", "dashboard", "sector", "sector_files.R"))
for (f in sector_files()) source(file.path(repo_root, f))

map <- yaml::read_yaml(file.path(repo_root, "config", "climate_trace_map.yml"))
dict_cells <- vapply(yaml::read_yaml(file.path(repo_root, "config", "sector_dictionary.yml"))$cells, function(c) c$slug, character(1))

country_answer <- function(sub_world = 100) {
  em <- function(x) list(co2 = x, ch4 = 0, n2o = 0, co2e_100yr = x, co2e_20yr = x)
  list(
    list(country = "CHL", emissions = em(60), worldEmissions = em(sub_world)),
    list(country = "PER", emissions = em(30), worldEmissions = em(sub_world)),
    list(country = "ABW", emissions = list(co2 = NULL, ch4 = NULL, n2o = NULL, co2e_100yr = NULL, co2e_20yr = NULL), worldEmissions = em(sub_world))
  )
}

test_that("country answers parse to one row per country plus Climate TRACE's world total", {
  x <- climate_trace_parse_country(country_answer(), "copper-mining", "mineral-extraction", 2024)
  expect_identical(names(x), CLIMATE_TRACE_COUNTRY_COLUMNS)
  expect_identical(x$iso3, c("CHL", "PER", "ABW", "WLD"))
  expect_identical(x$co2e_100yr, c(60, 30, NA, 100))
  expect_true(all(x$year == 2024L) && all(x$subsector == "copper-mining"))
  bad <- country_answer()
  bad[[2]]$worldEmissions$co2e_100yr <- 99
  expect_error(climate_trace_parse_country(bad, "copper-mining", "mineral-extraction", 2024), "disagree")
  expect_error(climate_trace_parse_country(list(list(country = "all", emissions = list(), worldEmissions = list(co2e_100yr = 1))), "x", "y", 2024), "without countries")
})

test_that("asset groups: power by first-listed fuel, aluminium by asset type, otherwise all", {
  expect_identical(climate_trace_asset_group(c("coal, gas", "Gas", "", NA), "primary_fuel"), c("coal", "gas", "unknown", "unknown"))
  expect_identical(climate_trace_asset_group(c("Smelting", "Refinery"), "asset_type"), c("Smelting", "Refinery"))
  expect_identical(climate_trace_asset_group(c("x", "y"), "all"), c("all", "all"))
  expect_error(climate_trace_asset_group("x", "fuel"), "Unknown")
})

assets_page <- function() {
  es <- function(q, a, unit = "MWh") data.frame(Gas = c("co2", "co2e_100yr"), EmissionsQuantity = c(q * 0.9, q), Activity = c(a, a),
                                                ActivityUnits = unit, Capacity = 100, CapacityUnits = "MW")
  d <- data.frame(Id = c("1", "2", "3", "4"), Country = c("DEU", "DEU", "DEU", "POL"), AssetType = c("coal", "coal, gas", "gas", "coal"))
  d$EmissionsSummary <- list(es(10, 10), es(5, NA), es(2, 4), es(8, 8))
  d
}

test_that("tracked assets aggregate per country and group; intensity counts only assets with an activity", {
  r <- climate_trace_asset_rows(assets_page())
  expect_identical(r$co2e_100yr, c(10, 5, 2, 8))
  a <- climate_trace_aggregate_assets(r, "electricity-generation", 2024, "primary_fuel")
  expect_identical(names(a), CLIMATE_TRACE_ASSET_COLUMNS)
  deu <- a[a$iso3 == "DEU" & a$asset_group == "coal", ]
  expect_identical(c(deu$n_assets, deu$n_activity), c(2L, 1L))
  expect_identical(c(deu$co2e_100yr, deu$co2e_100yr_activity, deu$activity), c(15, 10, 10))
  expect_identical(unique(a$activity_unit), "MWh")
  it <- climate_trace_intensity_tbl(a)
  w <- it[it$iso3 == "WLD" & it$asset_group == "coal", ]
  expect_identical(c(w$co2e_100yr_activity, w$activity, w$intensity), c(18, 18, 1))
  expect_identical(w$intensity_unit, "t CO2e per MWh")
  expect_error(climate_trace_aggregate_assets(rbind(r, r), "electricity-generation", 2024, "primary_fuel"), "listed twice")
})

test_that("the map covers every cell, and match quality follows the best own source", {
  expect_identical(climate_trace_map_validate(map, dict_cells), character())
  # Keyed on the six-stage cells of the dictionary (66 headline cells and the Semiconductors stages).
  expect_setequal(names(map$cells), dict_cells)
  expect_identical(length(map$cells), 69L)
  expect_true(all(sector_headline_cell_slugs() %in% names(map$cells)))
  bad <- map
  bad$cells$`coal-extraction`$sources[[1]]$match <- NULL
  bad$cells$`solar-assembly`$sources[[1]]$match <- "proxy"
  bad$cells$`wind-deployment` <- NULL
  p <- climate_trace_map_validate(bad, dict_cells)
  expect_true(any(grepl("coal-extraction coal-mining: an own source needs", p)))
  expect_true(any(grepl("solar-assembly aluminum: a context source has no match", p)))
  expect_true(any(grepl("cells missing from the map: wind-deployment", p)))
  mt <- climate_trace_map_tbl(map)
  cells <- tibble::tibble(slug = dict_cells, tech = "x", stage = "y")
  ct <- climate_trace_cells_tbl(map, mt, cells)
  m <- stats::setNames(ct$match, ct$cell)
  expect_identical(unname(m[c("coal-extraction", "oil-refining", "gas-assembly", "wind-components", "solar-deployment")]),
                   c("direct", "partial", "proxy", "none", "none"))
  src <- climate_trace_sources_tbl(map, mt)
  expect_identical(src$basis[src$src_id == "electricity-generation:coal"], "assets")
  expect_identical(src$label[src$src_id == "electricity-generation:coal"], "Electricity generation, coal-fired plants")
  expect_false(anyDuplicated(src$src_id) > 0)
})

ipcc_raw <- data.frame(
  Tech = c("Coal (pulverised)", "Natural-gas combined cycle", "Oil-fired steam", "Biomass (dedicated)", "Solar PV – utility scale",
           "Solar PV – rooftop", "Concentrated solar power", "Geothermal", "Hydropower (typical reservoir)", "Wind – onshore",
           "Wind – offshore", "Nuclear (light-water)"),
  ghg_intensity = c(820, 490, 720, 230, 48, 41, 27, 38, 24, 11, 12, 12)
)

test_that("the IPCC rows reproduce tech_ghg_tbl, and the proxies are named", {
  tg <- partnership_strength_clean_ghg(ipcc_raw)
  lc <- climate_trace_lifecycle(ipcc_raw, tg, IPCC_OIL_LIFECYCLE_MEDIAN_G_PER_KWH)
  expect_identical(lc$tech, tg$tech)
  expect_identical(lc$g_min[lc$tech == "Oil"], 840)
  expect_identical(c(lc$g_min[lc$tech == "Solar"], lc$g_max[lc$tech == "Solar"]), c(27, 48))
  expect_identical(lc$basis[lc$tech == "Electric Grid"], "proxy")
  expect_identical(lc$proxy_of[lc$tech == "Electric Grid"], "Solar|Wind")
  expect_true(is.na(lc$g_min[lc$tech == "Batteries"]))
  tg2 <- tg
  tg2$ghg_index[tg2$tech == "Coal"] <- 0.5
  expect_error(climate_trace_lifecycle(ipcc_raw, tg2, IPCC_OIL_LIFECYCLE_MEDIAN_G_PER_KWH), "do not reproduce")
})

test_that("a repeated world total is flagged as carried forward", {
  ct <- tibble::tibble(iso3 = "WLD", subsector = rep(c("a", "b"), each = 2), year = rep(2024:2025, 2), co2e_100yr = c(5, 5, 5, 6))
  r <- climate_trace_repeats(ct)
  expect_identical(r$repeated, c(FALSE, TRUE, FALSE, FALSE))
})

# A small processed set: two cells, three sources, three economies (one outside dims.geo).
climate_processed <- function() {
  mt <- tibble::tibble(cell = c("coal-extraction", "coal-deployment", "wind-components"), src_id = c("coal-mining", "electricity-generation:coal", "iron-and-steel"),
                       role = c("own", "own", "context"), match = c("direct", "partial", NA), order = 1L)
  list(
    sources = tibble::tibble(src_id = c("coal-mining", "electricity-generation:coal", "iron-and-steel"), subsector = c("coal-mining", "electricity-generation", "iron-and-steel"),
                             sector = c("fossil-fuel-operations", "power", "manufacturing"), label = c("Coal mining", "Electricity generation, coal-fired plants", "Iron and steel"),
                             asset_group = c(NA, "coal", NA), basis = c("country", "assets", "country")),
    map = mt,
    cells = tibble::tibble(cell = c("coal-extraction", "coal-deployment", "wind-components"), tech = c("Coal", "Coal", "Wind"), stage = c("Extraction", "Deployment", "Components"),
                           match = c("direct", "partial", "none"), note = c("a", "b", "c")),
    lifecycle = tibble::tibble(tech = c("Coal", "Wind", "Batteries"), ghg_index = c(0.02, 1, 0.97), basis = c("ipcc", "ipcc", "proxy"),
                               g_min = c(820, 11, NA), g_max = c(820, 12, NA), rows = c("Coal (pulverised) 820", "Wind - onshore 11; Wind - offshore 12", NA),
                               proxy_of = c(NA, NA, "Solar")),
    emissions = tibble::tibble(src_id = rep(c("coal-mining", "electricity-generation:coal", "iron-and-steel"), each = 4),
                               iso3 = rep(c("CHN", "USA", "XXX", "WLD"), 3), year = 2025L, co2e_100yr = c(50, 20, 5, 75, 30, 10, 1, 41, 7, 2, 1, 10)),
    intensity = tibble::tibble(subsector = "electricity-generation", asset_group = "coal", iso3 = c("CHN", "WLD"), year = 2025L, n_activity = c(3L, 5L),
                               co2e_100yr_activity = c(30, 40), activity = c(30, 50), activity_unit = "MWh", intensity = c(1, 0.8), intensity_unit = "t CO2e per MWh"),
    country = tibble::tibble(iso3 = "WLD", subsector = c("coal-mining", "electricity-generation", "iron-and-steel"), year = 2025L, repeated = c(TRUE, FALSE, FALSE))
  )
}

test_that("sector-climate: part, chunk, contract checks and decoded parity", {
  p <- climate_processed()
  geo <- c("CHN", "USA", "WLD")
  part <- sector_climate_part(p, geo)
  expect_identical(part$year, 2025L)
  expect_identical(part$src$repeated, c(TRUE, FALSE, FALSE))
  expect_equal(part$outside$share_outside[part$outside$src_id == "coal-mining"], 5 / 75)
  items <- c("cell:coal-extraction", "cell:coal-deployment", "cell:wind-components")
  gi <- function(x) as.integer(match(x, geo) - 1L)
  ii <- function(x) as.integer(match(x, items) - 1L)
  ch <- sector_climate_chunk(part, gi, ii)
  sizes <- list(geo = length(geo), items = length(items), vars = 1L)
  bytes <- dash_osb_encode(ch, "sector-climate", "chunk")
  dec <- dash_osb_decode(bytes)$objects
  expect_identical(dec, dash_osb_normalize(ch))
  expect_identical(sector_check_chunk("sector-climate", dec, sizes), character())
  meta <- list(dims = list(geo = tibble::tibble(code = geo), items = tibble::tibble(axis = "cell", slug = sub("^cell:", "", items))))
  pr <- sector_climate_parity(dec, meta, p)
  expect_true(all(pr$status == "PASS"))
  expect_gte(nrow(pr), 6L)
  # A changed value fails the parity; a wrong cell match fails the checks.
  dec2 <- dec
  dec2$climate$em$value[1] <- dec2$climate$em$value[1] + 1e-9
  expect_true(any(sector_climate_parity(dec2, meta, p)$status == "FAIL"))
  dec3 <- dec
  dec3$climate$cells$match[1] <- "proxy"
  expect_match(sector_check_chunk("sector-climate", dec3, sizes), "best match", all = FALSE)
  dec4 <- dec
  dec4$climate$intensity$value[1] <- 2
  expect_match(sector_check_chunk("sector-climate", dec4, sizes), "emissions / activity", all = FALSE)
})
