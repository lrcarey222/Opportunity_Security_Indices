# Climate sources (user request of 2026-10-17: seven files added to data/raw for the Climate section): the
# pure parsers and maps of R/categories/climate/climate_sources.R against config/climate_sources_map.yml,
# with synthetic inputs shaped like the published files, the stops on renamed sheets, titles, scenarios and
# rows, and the sector-climate-src chunk (R/dashboard/sector/sector_climate.R) with its contract checks and
# decoded parity.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "categories", "climate", "climate_sources.R"))
source(file.path(repo_root, "R", "dashboard", "sector", "sector_files.R"))
for (f in sector_files()) source(file.path(repo_root, f))

map <- yaml::read_yaml(file.path(repo_root, "config", "climate_sources_map.yml"))
dict_cells <- vapply(yaml::read_yaml(file.path(repo_root, "config", "sector_dictionary.yml"))$cells, function(c) c$slug, character(1))
weo_ids <- vapply(yaml::read_yaml(file.path(repo_root, "config", "iea_pathways_map.yml"))$series, function(s) s$id, character(1))
iso <- c(Japan = "JPN", Germany = "DEU", US = "USA", Brazil = "BRA")
to_iso3 <- function(x) unname(iso[x])

# A sheet as readxl returns it with col_names = FALSE (rows of cells; NA where empty).
sheet <- function(rows, ncol = max(lengths(rows))) {
  m <- t(vapply(rows, function(r) c(as.character(r), rep(NA_character_, ncol - length(r))), character(ncol)))
  as.data.frame(m, stringsAsFactors = FALSE)
}

gcp_sheet <- function(names, years, f) {
  sheet(c(list(c(NA, "Fossil CO2 emissions"), c(NA, "All values in million tonnes of carbon per year"), "MtC/yr", c(NA, toupper(names)), c(NA, names)),
          lapply(years, function(y) c(y, vapply(seq_along(names), function(j) f(y, j), numeric(1))))))
}
gcp_sheets <- function(names = c("Japan", "Germany", "OECD", "EU27", "World")) {
  list(territorial = gcp_sheet(names, 1989:2024, function(y, j) 100 * j + y - 2000),
       consumption = gcp_sheet(names, 1990:2023, function(y, j) 110 * j + y - 2000),
       transfers = gcp_sheet(names, 1990:2023, function(y, j) -10 * j))
}

ei_sheets <- function() {
  out <- list()
  for (x in map$ei$indicators) {
    out[[x$sheet]] <- sheet(list(x$title, "unit text", NULL, c("Country", 1990, 2017, 2024, "Growth Rate"), c("Japan", 1, 2, 3, 0.1), c("Germany", 2, 3, 4, 0.2),
                                 c("Brazil", 3, 4, 8, 0.3)))
  }
  pc <- names(map$ei$power$columns)
  out[[map$ei$power$sheet]] <- sheet(list(map$ei$power$title, "% Shares", c(NA, 2024), c("Country", pc), c("Japan", rep(1 / length(pc), length(pc))),
                                          c("Germany", rep(1 / length(pc), length(pc)))))
  out
}

weo_csv <- function() {
  regs <- names(map$weo_regions$regions)
  sc <- c(Historical = 2024, `Stated Policies Scenario` = 2035, `Current Policies Scenario` = 2035)
  rows <- list()
  for (r in regs) for (s in map$weo_regions$series) for (k in names(sc)) {
    rows[[length(rows) + 1L]] <- data.frame(PUBLICATION = "World Energy Outlook 2025", SCENARIO = k, CATEGORY = s$category, PRODUCT = s$product, FLOW = s$flow,
                                            UNIT = "TWh", REGION = r, YEAR = sc[[k]], VALUE = 100, stringsAsFactors = FALSE)
  }
  rows[[length(rows) + 1L]] <- data.frame(PUBLICATION = "World Energy Outlook 2025", SCENARIO = "Net Zero Emissions by 2050 Scenario", CATEGORY = "Energy",
                                          PRODUCT = "Total", FLOW = "Refinery runs", UNIT = "mb/d", REGION = "Japan and Korea", YEAR = 2035, VALUE = 1, stringsAsFactors = FALSE)
  do.call(rbind, rows)
}

nze_csv <- function() {
  yrs <- c(2019, 2020, 2030, 2040, 2050)
  do.call(rbind, lapply(map$nze2021$series, function(s) data.frame(Publication = "Net Zero by 2050", Scenario = "Net-Zero Emissions by 2050", Category = s$category,
                                                                    Product = s$product, Flow = s$flow, Unit = "GW", Year = yrs, Value = seq_along(yrs) * 10,
                                                                    stringsAsFactors = FALSE)))
}

wei_sheets <- function() {
  out <- list()
  for (reg in names(map$wei$regions)) {
    lab <- map$wei$region_labels[[reg]] %||% reg
    rows <- Filter(function(r) is.null(r$only) || reg %in% unlist(r$only), map$wei$rows)
    out[[reg]] <- sheet(c(list(c(NA, 47, 48), NULL, c(lab, 2015:2026)),
                          lapply(seq_along(rows), function(k) c(rows[[k]]$label, k * (1:12))), list("Note: a trailing note")))
  }
  out
}

test_that("the real map lists every cell and passes its validation", {
  expect_identical(climate_sources_map_validate(map, dict_cells, weo_ids), character())
  bad <- map
  bad$cells$`solar-downstream`$wei[[1]]$match <- "maybe"
  bad$cells$`wind-downstream` <- NULL
  bad$nze2021$series[[1]]$weo <- "no_such_series"
  bad$weo_regions$own$DEU <- "Germany"
  p <- climate_sources_map_validate(bad, dict_cells, weo_ids)
  expect_match(p, "match must be direct, partial or proxy", all = FALSE)
  expect_match(p, "cells missing from the map: wind-downstream", all = FALSE)
  expect_match(p, "is not a series of config/iea_pathways_map.yml", all = FALSE)
  expect_match(p, "own names a region that is not read", all = FALSE)
})

test_that("GCP national sheets parse to Mt CO2 from 1990, EU27 and World kept, other aggregates dropped", {
  g <- climate_sources_gcp_national(gcp_sheets(), map, to_iso3)
  expect_setequal(unique(g$iso3), c("JPN", "DEU", "EUU", "WLD"))
  expect_identical(min(g$year), 1990L)
  v <- g$value[g$iso3 == "JPN" & g$measure == "territorial" & g$year == 2024L]
  expect_identical(v, (100 + 24) * map$gcp$c_to_co2)
  expect_identical(max(g$year[g$measure == "consumption"]), 2023L)
  expect_error(climate_sources_gcp_national(gcp_sheets(c("Japan", "Atlantis", "World")), map, to_iso3), "no ISO3 for Atlantis")
  s <- gcp_sheets()
  s$consumption[3, 1] <- "GtC/yr"
  expect_error(climate_sources_gcp_national(s, map, to_iso3), "unit row")
  s <- gcp_sheets()
  s$transfers <- gcp_sheets(c("Japan", "World"))$transfers
  expect_error(climate_sources_gcp_national(s, map, to_iso3), "other columns")
})

test_that("GCP fuel sheet parses World CO2 by fuel and stops on a renamed column", {
  hdr <- c("Year", names(map$gcp$fuel_columns), "Per.Capita")
  d <- sheet(c(list(c(NA, "title"), "GtC/yr", hdr), lapply(1958:2024, function(y) c(y, rep(1, length(hdr) - 1)))))
  f <- climate_sources_gcp_fuel(d, map)
  expect_identical(min(f$year), as.integer(map$gcp$fuel_first_year))
  expect_setequal(unique(f$fuel), unname(unlist(map$gcp$fuel_columns)))
  expect_identical(f$value[f$fuel == "coal" & f$year == 2024L], map$gcp$c_to_co2)
  d[3, 3] <- "Coal (renamed)"
  expect_error(climate_sources_gcp_fuel(d, map), "missing column")
})

test_that("EI tracker sheets parse with the power mix; the median is over every listed economy; a renamed sheet stops", {
  e <- climate_sources_ei(ei_sheets(), map, to_iso3)
  expect_setequal(unique(e$values$year[e$values$ind == "ghg"]), c(1990L, 2017L, 2024L))
  expect_false(any(e$values$value == 0.1))
  expect_true(all(c("power_wind", "power_coal") %in% e$values$ind))
  m <- climate_sources_ei_median(e$values)
  expect_identical(m$value[m$ind == "ghg" & m$year == 2024L], 4)
  expect_identical(m$n[m$ind == "ghg" & m$year == 2024L], 3L)
  ind <- climate_sources_ei_indicators(map)
  expect_identical(sum(ind$low_carbon, na.rm = TRUE), length(map$ei$power$low_carbon))
  s <- ei_sheets()
  s[[map$ei$indicators[[1]]$sheet]][1, 1] <- "Something else"
  expect_error(climate_sources_ei(s, map, to_iso3), "expected")
  s <- ei_sheets()
  s[[map$ei$power$sheet]] <- NULL
  expect_error(climate_sources_ei(s, map, to_iso3), "no sheet")
})

test_that("WEO regions parse every mapped series; an unknown scenario or region stops", {
  w <- climate_sources_weo_regions(weo_csv(), map)
  expect_identical(nrow(w$series), length(map$weo_regions$series))
  expect_setequal(unique(w$values$region), names(map$weo_regions$regions))
  expect_false("nze" %in% w$values$scenario)
  x <- weo_csv()
  x$SCENARIO[1] <- "Announced Pledges Scenario"
  expect_error(climate_sources_weo_regions(x, map), "unknown scenario")
  x <- weo_csv()
  x$REGION[x$REGION == "Japan"] <- "Nippon"
  expect_error(climate_sources_weo_regions(x, map), "unknown region")
})

test_that("economies map to the most specific WEO region and say how", {
  sub <- function(x) unname(c(BDI = "Sub-Saharan Africa", PSE = "Western Asia", AND = "Southern Europe")[x])
  g <- climate_sources_weo_geo(c("JPN", "DEU", "IDN", "GBR", "HKG", "BDI", "PSE", "AND", "KOR"), map, sub)
  r <- stats::setNames(g$region, g$iso3)
  m <- stats::setNames(g$match, g$iso3)
  expect_identical(unname(r[c("JPN", "DEU", "IDN", "GBR", "HKG", "BDI", "KOR")]), c("Japan", "European Union", "Southeast Asia", "Europe", "China", "Africa", "Asia Pacific"))
  expect_identical(unname(m[c("JPN", "DEU", "BDI")]), c("own", "member", "by_subregion"))
  expect_false(any(c("PSE", "AND") %in% g$iso3))
})

test_that("NZE 2021 series pair with WEO series; another scenario stops", {
  n <- climate_sources_nze2021(nze_csv(), map)
  expect_identical(nrow(n$series), length(map$nze2021$series))
  expect_true(all(n$series$weo %in% weo_ids))
  expect_identical(nrow(n$values), 5L * nrow(n$series))
  x <- nze_csv()
  x$Scenario <- "Sustainable Development Scenario"
  expect_error(climate_sources_nze2021(x, map), "scenario")
  x <- nze_csv()
  expect_error(climate_sources_nze2021(x[-1, ], map), "one per year")
})

test_that("World Energy Investment sheets parse every row; World-only rows; a renamed row stops", {
  w <- climate_sources_wei(wei_sheets(), map)
  expect_identical(nrow(w$rows), length(map$wei$rows))
  expect_true(all(w$values$estimate == (w$values$year %in% unlist(map$wei$estimate_years))))
  expect_false(any(w$values$row == "memo_electricity" & w$values$region != "World"))
  expect_identical(w$regions$label[w$regions$region == "EMDE"], "Emerging market and developing economies")
  s <- wei_sheets()
  s$Europe[5, 1] <- "Fossil fuels (renamed)"
  expect_error(climate_sources_wei(s, map), "row labels differ")
  s <- wei_sheets()
  s$China <- NULL
  expect_error(climate_sources_wei(s, map), "no sheet 'China'")
})

# The processed tables from the synthetic inputs (as scripts/18 writes them).
src_processed <- function() {
  e <- climate_sources_ei(ei_sheets(), map, to_iso3)
  w <- climate_sources_weo_regions(weo_csv(), map)
  n <- climate_sources_nze2021(nze_csv(), map)
  i <- climate_sources_wei(wei_sheets(), map)
  mt <- climate_sources_map_tbl(map)
  hdr <- c("Year", names(map$gcp$fuel_columns))
  list(
    gcp_national = climate_sources_gcp_national(gcp_sheets(), map, to_iso3),
    gcp_fuel = climate_sources_gcp_fuel(sheet(c(list("GtC/yr", hdr), lapply(1960:2024, function(y) c(y, rep(1, length(hdr) - 1))))), map),
    ei = e$values, ei_ind = climate_sources_ei_indicators(map), ei_median = climate_sources_ei_median(e$values),
    weo_series = w$series, weo_values = w$values, weo_regions = w$regions,
    weo_geo = climate_sources_weo_geo(c("JPN", "DEU", "BRA"), map, function(x) rep(NA_character_, length(x))),
    nze21_series = n$series, nze21_values = n$values, wei_rows = i$rows, wei_values = i$values, wei_regions = i$regions,
    map = mt, cells = climate_sources_cells_tbl(map, mt, dict_cells),
    meta = tibble::tibble(key = c("gcp", "wei"), value = c("Global Carbon Budget 2025 v1.0", "World Energy Investment 2026"))
  )
}

test_that("cells take the best match of their WEI rows; GCP fuels are context on the fossil cells only", {
  p <- src_processed()
  expect_identical(nrow(p$cells), length(dict_cells))
  expect_identical(p$cells$match[p$cells$cell == "solar-downstream"], "direct")
  expect_identical(p$cells$match[p$cells$cell == "semiconductors-upstream"], "none")
  g <- p$map[p$map$src == "gcp", ]
  expect_true(all(g$role == "context") && all(is.na(g$match)))
  expect_true(all(sub("-.*$", "", g$cell) %in% c("coal", "oil", "gas")))
})

test_that("sector-climate-src: chunk, contract checks and decoded parity", {
  p <- src_processed()
  geo <- c("BRA", "DEU", "EUU", "JPN", "WLD")
  items <- paste0("cell:", dict_cells)
  part <- sector_src_part(p, geo)
  ii <- function(x) as.integer(match(x, items) - 1L)
  gi <- function(x) as.integer(match(x, geo) - 1L)
  ch <- sector_src_chunk(part, gi, ii)
  sizes <- list(geo = length(geo), items = length(items), vars = 1L)
  dec <- dash_osb_decode(dash_osb_encode(ch, "sector-climate-src", "chunk"))$objects
  expect_identical(dec, dash_osb_normalize(ch))
  expect_identical(sector_check_chunk("sector-climate-src", dec, sizes), character())
  meta <- list(dims = list(geo = tibble::tibble(code = geo), items = tibble::tibble(axis = "cell", slug = dict_cells)))
  pr <- sector_src_parity(dec, meta, p)
  expect_identical(nrow(pr), 18L)
  expect_true(all(pr$status == "PASS"))
  d2 <- dec
  d2$csrc$gcp$value[1] <- d2$csrc$gcp$value[1] * (1 + 1e-12)
  expect_true(any(sector_src_parity(d2, meta, p)$status == "FAIL"))
  d3 <- dec
  d3$csrc$cells$match[d3$csrc$cells$match == "direct"][1] <- "proxy"
  expect_match(sector_check_chunk("sector-climate-src", d3, sizes), "best match", all = FALSE)
  d4 <- dec
  d4$csrc$map$match[d4$csrc$map$src == "gcp"][1] <- "direct"
  expect_match(sector_check_chunk("sector-climate-src", d4, sizes), "context without one", all = FALSE)
})

test_that("the processed tables, when built, carry the published editions (scripts/18 -> scripts/87)", {
  pd <- file.path(repo_root, "data", "processed")
  files <- file.path(pd, paste0(SECTOR_SRC_PROCESSED, ".rds"))
  skip_if_not(all(file.exists(files)), "climate_src_*_tbl.rds not built")
  p <- stats::setNames(lapply(files, readRDS), names(SECTOR_SRC_PROCESSED))
  expect_identical(nrow(p$cells), length(dict_cells))
  expect_false("nze" %in% p$weo_values$scenario[p$weo_values$region != "World"])
  expect_true(all(c("JPN", "DEU", "BRA", "EUU", "WLD") %in% p$gcp_national$iso3))
  # Spot values as published (GCP 2025 v1.0: World 2024 fossil CO2 10.53 GtC; WEI 2026: World solar 2025 USD 407 bn).
  if (identical(p$meta$value[p$meta$key == "gcp"], "Global Carbon Budget 2025 v1.0")) {
    w <- p$gcp_fuel$value[p$gcp_fuel$fuel == "total" & p$gcp_fuel$year == 2024L]
    expect_equal(w / 3.664, 10.5345, tolerance = 1e-4)
  }
  if (identical(p$meta$value[p$meta$key == "wei"], "World Energy Investment 2026")) {
    expect_identical(p$wei_values$value[p$wei_values$row == "solar" & p$wei_values$region == "World" & p$wei_values$year == 2025L], 407)
  }
})
