# IEA scenario pathways (user request of 2026-10-17: IEA Net Zero data for the Climate section): the WEO
# Annex A and Critical Minerals parsers and the technology x stage map (R/categories/climate/iea_pathways.R,
# config/iea_pathways_map.yml), and the optional climate.nze_* tables of the sector-climate chunk
# (R/dashboard/sector/sector_climate.R) with their contract checks and decoded parity. Synthetic inputs,
# plus the real map against the sector dictionary.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "categories", "climate", "iea_pathways.R"))
source(file.path(repo_root, "R", "dashboard", "sector", "sector_files.R"))
for (f in sector_files()) source(file.path(repo_root, f))

real_map <- yaml::read_yaml(file.path(repo_root, "config", "iea_pathways_map.yml"))
dict_cells <- vapply(yaml::read_yaml(file.path(repo_root, "config", "sector_dictionary.yml"))$cells, function(c) c$slug, character(1))

# A two-series map over three cells.
toy_map <- function() {
  list(
    scenarios = list(
      weo = list(Historical = "historical", `Stated Policies Scenario` = "stated", `Current Policies Scenario` = "current", `Net Zero Emissions by 2050 Scenario` = "nze"),
      minerals = list(`Current Policies scenario` = "current", `Stated Policies scenario` = "stated", `High Demand Scenario` = "high_demand")
    ),
    series = list(
      list(id = "cap_solar", group = "deployment", category = "Capacity: installed", product = "Solar PV", flow = "Electrical capacity", label = "Solar PV capacity"),
      list(id = "co2_intensity", group = "emissions", category = "CO2 total intensity", product = "Electricity", flow = "Electricity generation", label = "CO2 intensity of electricity")
    ),
    cells = list(
      `solar-deployment` = list(series = list(list(id = "cap_solar", role = "own", match = "direct"), list(id = "co2_intensity", role = "context")), note = "a"),
      `solar-extraction` = list(series = list(list(id = "cap_solar", role = "demand", match = "proxy")), minerals = list("Solar PV"), note = "b"),
      `semiconductors-refining` = list(series = list(), note = "c")
    ),
    world_context = "co2_intensity"
  )
}

toy_weo <- function() {
  sc <- c("Historical", "Stated Policies Scenario", "Current Policies Scenario", "Net Zero Emissions by 2050 Scenario")
  rows <- list()
  for (k in 1:2) {
    cat_ <- c("Capacity: installed", "CO2 total intensity")[k]
    prod <- c("Solar PV", "Electricity")[k]
    flow <- c("Electrical capacity", "Electricity generation")[k]
    unit <- c("GW", "gCO2 per kWh")[k]
    rows[[length(rows) + 1]] <- data.frame(SCENARIO = sc[1], YEAR = c(2023L, 2024L), VALUE = c(1400, 1900) / k)
    for (j in 2:4) rows[[length(rows) + 1]] <- data.frame(SCENARIO = sc[j], YEAR = c(2035L, 2050L), VALUE = c(4000, 8000) * c(1, 0.9, 1.6)[j - 1] / k)
    for (i in (length(rows) - 3):length(rows)) rows[[i]] <- cbind(rows[[i]], CATEGORY = cat_, PRODUCT = prod, FLOW = flow, UNIT = unit)
  }
  out <- do.call(rbind, rows)
  out$REGION <- "World"
  out$PUBLICATION <- "World Energy Outlook 2025"
  # An unmapped row is ignored.
  rbind(out, data.frame(SCENARIO = "Historical", YEAR = 2024L, VALUE = 1, CATEGORY = "Energy", PRODUCT = "Total", FLOW = "Total energy supply", UNIT = "EJ", REGION = "World", PUBLICATION = "World Energy Outlook 2025"))
}

toy_critical <- function() {
  sc <- c("Current Policies scenario", "Stated Policies scenario", "High Demand Scenario")
  base <- data.frame(Pillar = "2 Total supply for key minerals", Mineral = "Copper - Mining", Sector.Country = "Chile", Scenario = "Base case", X2025 = 5, X2035 = 6)
  d <- expand.grid(Scenario = sc, Sector.Country = c("Solar PV", "Wind", "Total copper"), stringsAsFactors = FALSE)
  d$Pillar <- "3.1 Energy demand by tech"
  d$Mineral <- "Copper"
  d$X2025 <- ifelse(d$Sector.Country == "Total copper", 30, ifelse(d$Sector.Country == "Solar PV", 20, 10))
  d$X2035 <- d$X2025 * match(d$Scenario, sc)
  s1 <- data.frame(Pillar = "1 Total demand for key minerals", Mineral = "Copper", Sector.Country = c("Total demand", "Share of energy technologies in total demand"),
                   Scenario = "Stated Policies scenario", X2025 = c(300, 0.1), X2035 = c(350, 0.12))
  rbind(base, d[names(base)], s1)
}

test_that("the real map covers every cell and passes its validation", {
  expect_identical(iea_pathways_map_validate(real_map, dict_cells), character())
  # Keyed on the six-stage cells; mineral demand on the clean-technology Extraction and Refining cells only.
  expect_setequal(names(real_map$cells), dict_cells)
  with_minerals <- names(Filter(function(e) length(unlist(e$minerals)) > 0, real_map$cells))
  expect_true(all(grepl("-(extraction|refining)$", with_minerals)))
  expect_true(all(c("batteries-extraction", "batteries-refining", "solar-refining", "wind-extraction") %in% with_minerals))
  ids <- vapply(real_map$series, function(s) s$id, character(1))
  expect_true(real_map$world_context %in% ids)
  # Every own / demand row has a match; context rows none; the WEO rows are distinct.
  m <- iea_pathways_map_tbl(real_map)
  expect_identical(is.na(m$match), m$role == "context")
  expect_true(all(c("cap_solar", "cap_wind", "cap_nuclear", "cap_battery", "h2_low", "co2_intensity", "co2_steel", "tes_coal") %in% m$series))
})

test_that("map validation names unknown series, bad roles, context matches and missing cells", {
  mp <- toy_map()
  expect_match(iea_pathways_map_validate(mp, c("solar-deployment", "solar-extraction", "semiconductors-refining", "wind-deployment")), "cells missing from the map: wind-deployment", all = FALSE)
  mp$cells$`solar-deployment`$series[[2]]$match <- "direct"
  mp$cells$`solar-extraction`$series[[1]]$id <- "nope"
  mp$cells$`solar-extraction`$minerals <- list("Nuclear reactors")
  p <- iea_pathways_map_validate(mp, names(mp$cells), mineral_techs = c("Solar PV", "Wind"))
  expect_match(p, "a context series has no match quality", all = FALSE)
  expect_match(p, "unknown series", all = FALSE)
  expect_match(p, "unknown mineral technology Nuclear reactors", all = FALSE)
})

test_that("WEO rows of the mapped series parse to World pathways with their unit", {
  w <- iea_pathways_weo(toy_weo(), toy_map())
  expect_identical(w$series$id, c("cap_solar", "co2_intensity"))
  expect_identical(w$series$unit, c("GW", "gCO2 per kWh"))
  v <- w$values[w$values$series == "cap_solar", ]
  expect_identical(unique(v$scenario), c("historical", "stated", "current", "nze"))
  expect_identical(v$value[v$scenario == "nze" & v$year == 2035L], 6400)
  expect_identical(iea_pathways_weo_edition(toy_weo()), "World Energy Outlook 2025")
  # A renamed scenario and a missing series stop.
  bad <- toy_weo()
  bad$SCENARIO[bad$SCENARIO == "Current Policies Scenario"] <- "Current Policy Settings"
  expect_error(iea_pathways_weo(bad, toy_map()), "unknown scenario")
  bad2 <- toy_weo()
  bad2 <- bad2[bad2$PRODUCT != "Solar PV", ]
  expect_error(iea_pathways_weo(bad2, toy_map()), "no rows for series cap_solar")
})

test_that("cells take the best match of their own and demand series; none without one", {
  mp <- toy_map()
  m <- iea_pathways_map_tbl(mp)
  c <- iea_pathways_cells_tbl(mp, m, names(mp$cells))
  expect_identical(c$match, c("direct", "proxy", "none"))
  expect_identical(c$mineral_techs, c(NA, "Solar PV", NA))
})

test_that("mineral demand keeps every scenario and sheet as published, without the share rows", {
  mn <- iea_pathways_minerals(toy_critical(), toy_map())
  expect_identical(sort(unique(mn$sheet)), c("1", "3.1"))
  expect_false(any(grepl("^Share", mn$tech)))
  expect_identical(sort(unique(mn$scenario[mn$sheet == "3.1"])), c("current", "high_demand", "stated"))
  expect_identical(mn$kt[mn$sheet == "3.1" & mn$tech == "Solar PV" & mn$scenario == "high_demand" & mn$year == 2035L], 60)
  expect_identical(iea_pathways_mineral_techs(mn), c("Solar PV", "Wind"))
  bad <- toy_critical()
  bad$Scenario[bad$Scenario == "High Demand Scenario"] <- "Net Zero Emissions by 2050 Scenario"
  expect_error(iea_pathways_minerals(bad, toy_map()), "unknown scenario")
})

test_that("the vintage check compares the projection years both editions publish", {
  w <- iea_pathways_weo(toy_weo(), toy_map())
  old <- toy_weo()
  old$VALUE[old$SCENARIO == "Net Zero Emissions by 2050 Scenario"] <- old$VALUE[old$SCENARIO == "Net Zero Emissions by 2050 Scenario"] / 2
  v <- iea_pathways_vintage(w$values, old, toy_map())
  expect_identical(sort(unique(v$scenario)), c("nze", "stated"))
  expect_true(all(abs(v$rel[v$scenario == "nze"] - 1) < 1e-12))
  expect_true(all(v$rel[v$scenario == "stated"] == 0))
})

# The processed tables of the toy inputs, as scripts/17 writes them.
nze_processed <- function() {
  mp <- toy_map()
  w <- iea_pathways_weo(toy_weo(), mp)
  m <- iea_pathways_map_tbl(mp)
  list(series = w$series, values = w$values, map = m, cells = iea_pathways_cells_tbl(mp, m, names(mp$cells)),
       minerals = iea_pathways_minerals(toy_critical(), mp),
       meta = tibble::tibble(key = c("weo", "weo_previous", "minerals_base_year"), value = c("World Energy Outlook 2025", NA, "2025")))
}

# A minimal Climate TRACE part (the chunk's required tables).
climate_part_min <- function(items) {
  list(
    src = tibble::tibble(id = "coal-mining", subsector = "coal-mining", sector = "fossil-fuel-operations", label = "Coal mining", asset_group = NA_character_, basis = "country", repeated = FALSE),
    map = tibble::tibble(item_key = character(), src_id = character(), role = character(), match = character(), order = integer()),
    cells = tibble::tibble(item_key = items, match = "none", note = "n"),
    lifecycle = tibble::tibble(tech = "Solar", basis = "ipcc", g_min = 41, g_max = 48, rows = "Solar 41", proxy_of = NA_character_),
    em = tibble::tibble(src_id = "coal-mining", geo = "WLD", year = 2025L, value = 1),
    intensity = tibble::tibble(subsector = character(), asset_group = character(), geo = character(), year = integer(), n = integer(), emissions = double(),
                               activity = double(), value = double(), unit = character(), activity_unit = character())
  )
}

test_that("sector-climate with the nze tables: chunk, contract checks and decoded parity", {
  p <- nze_processed()
  items <- c("cell:solar-deployment", "cell:solar-extraction", "cell:semiconductors-refining")
  cl <- climate_part_min(items)
  cl$nze <- sector_nze_part(p)
  ii <- function(x) as.integer(match(x, items) - 1L)
  gi <- function(x) as.integer(match(x, "WLD") - 1L)
  ch <- sector_climate_chunk(cl, gi, ii)
  expect_true(all(c("nze_series", "nze_values", "nze_map", "nze_cells", "nze_minerals", "nze_meta") %in% names(ch$climate)))
  expect_identical(ch$climate$nze_minerals$slug[1], sector_mineral_slug("Copper"))
  sizes <- list(geo = 1L, items = length(items), vars = 1L)
  dec <- dash_osb_decode(dash_osb_encode(ch, "sector-climate", "chunk"))$objects
  expect_identical(dec, dash_osb_normalize(ch))
  expect_identical(sector_check_chunk("sector-climate", dec, sizes), character())
  meta <- list(dims = list(geo = tibble::tibble(code = "WLD"), items = tibble::tibble(axis = "cell", slug = sub("^cell:", "", items))))
  pr <- sector_nze_parity(dec, meta, p)
  expect_identical(nrow(pr), 6L)
  expect_true(all(pr$status == "PASS"))
  # A changed value fails the parity; a wrong cell match, a context row with a match and a missing table fail the checks.
  d2 <- dec
  d2$climate$nze_values$value[1] <- d2$climate$nze_values$value[1] * (1 + 1e-12)
  expect_true(any(sector_nze_parity(d2, meta, p)$status == "FAIL"))
  d3 <- dec
  d3$climate$nze_cells$match[1] <- "proxy"
  expect_match(sector_check_chunk("sector-climate", d3, sizes), "best match", all = FALSE)
  d4 <- dec
  d4$climate$nze_map$match[d4$climate$nze_map$role == "context"] <- "direct"
  expect_match(sector_check_chunk("sector-climate", d4, sizes), "exactly on own and demand rows", all = FALSE)
  d5 <- dec
  d5$climate$nze_meta <- NULL
  expect_match(sector_check_chunk("sector-climate", d5, sizes), "every nze table or none", all = FALSE)
  # Without the nze part the chunk carries no nze table and still passes.
  cl$nze <- NULL
  plain <- dash_osb_decode(dash_osb_encode(sector_climate_chunk(cl, gi, ii), "sector-climate", "chunk"))$objects
  expect_false(any(startsWith(names(plain$climate), "nze_")))
  expect_identical(sector_check_chunk("sector-climate", plain, sizes), character())
  expect_identical(nrow(sector_nze_parity(plain, meta, p)), 0L)
})

test_that("the processed tables, when built, are the exported ones (scripts/17 -> scripts/87)", {
  pd <- file.path(repo_root, "data", "processed")
  files <- file.path(pd, paste0(SECTOR_NZE_PROCESSED, ".rds"))
  skip_if_not(all(file.exists(files)), "iea_pathways_*_tbl.rds not built")
  p <- stats::setNames(lapply(files, readRDS), names(SECTOR_NZE_PROCESSED))
  expect_identical(sort(unique(p$values$scenario)), sort(c("historical", "stated", "current", "nze")))
  expect_identical(sort(unique(p$minerals$scenario)), sort(c("current", "stated", "high_demand")))
  expect_false("nze" %in% p$minerals$scenario)
  expect_identical(nrow(p$cells), length(dict_cells))
  # Spot values as published in WEO 2025 Annex A (World).
  if (identical(p$meta$value[p$meta$key == "weo"], "World Energy Outlook 2025")) {
    sid <- function(id, sc, y) p$values$value[p$values$series == id & p$values$scenario == sc & p$values$year == y]
    expect_length(sid("cap_solar", "nze", 2035L), 1L)
    expect_gt(sid("cap_solar", "nze", 2035L), sid("cap_solar", "stated", 2035L))
    expect_lt(sid("tes_coal", "nze", 2035L), sid("tes_coal", "stated", 2035L))
  }
})
