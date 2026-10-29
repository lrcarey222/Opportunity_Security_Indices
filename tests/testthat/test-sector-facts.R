# Facts of the Sector Insights bundle from data/processed (R/dashboard/sector/sector_facts.R) on tiny
# hand-made processed tables: the raw-only filter, de-broadcasting, the supplier-table dedupe, value
# states (nsr vs zero, nq, na_growth) and the renamed processed measures shipped verbatim.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "dashboard", "sector", "sector_files.R"))
for (f in sector_files()) source(file.path(repo_root, f))

raw_tbl <- function(...) {
  t <- tibble::tibble(...)
  if (!"data_type" %in% names(t)) t$data_type <- "raw"
  t
}

test_that("only data_type raw rows are read; index, weight, contribution and Overall rows never", {
  tbl <- tibble::tibble(
    data_type = c("raw", "index", "weight", "contribution", "raw", "raw"),
    variable = c("exports", "exports", "exports", "exports", "Overall Trade Index", "market_share"),
    value = c(1, 2, 3, 4, 5, 6)
  )
  r <- sector_raw_rows(tbl)
  expect_identical(r$value, c(1, 6))
  expect_true(all(r$data_type == "raw"))
})

test_that("de-broadcast collapses identical copies and stops on a copy group with two values", {
  rows <- tibble::tibble(sub_sector = c("Oil_Brent", "Oil_Brent", "Oil_Brent", "Cobalt", "Cobalt"),
                         variable = "latest_price", iso3c = c("USA", "DEU", "CHN", "USA", "DEU"),
                         value = c(63.19, 63.19, 63.19, NA, NA))
  d <- sector_debroadcast(rows, c("sub_sector", "variable"), "prices")
  expect_identical(nrow(d), 2L)
  expect_identical(d$value, c(63.19, NA))
  rows$value[2] <- 63.2
  expect_error(sector_debroadcast(rows, c("sub_sector", "variable"), "prices"), "copies differ in value")
  rows$value[2] <- NA
  expect_error(sector_debroadcast(rows, c("sub_sector", "variable"), "prices"), "copies differ")
  # A second checked column (the observation year of wages).
  w <- tibble::tibble(iso3c = c("USA", "USA"), supply_chain = "Assembly", value = c(5000, 5000), Year = c(2024L, 2023L))
  expect_error(sector_debroadcast(w, c("iso3c", "supply_chain"), "wages", c("value", "Year")), "copies differ in Year")
})

test_that("world-level prices ship once per commodity (geo WLD), verbatim", {
  ep <- raw_tbl(
    iso3c = rep(c("USA", "DEU", "JPN"), each = 4),
    tech = rep(c("Oil", "Oil", "Batteries", "Batteries"), 3),
    sub_sector = rep(c("Oil_Brent", "Oil_Brent", "Cobalt", "Cobalt"), 3),
    variable = rep(c("latest_price", "price_volatility_5yr", "latest_price", "mean_price_volatility"), 3),
    value = rep(c(63.19, 0.32, 33000.5, 0.4), 3)
  )
  cm <- sector_commodity_items(unique(ep$sub_sector))
  x <- sector_facts_prices(ep, cm)
  f <- x$facts
  expect_setequal(paste(f$var, f$item_key), c("prices.latest commodity:oil-brent", "prices.vol_5y commodity:oil-brent",
                                               "prices.latest commodity:cobalt"))
  expect_true(all(f$geo == "WLD"))
  expect_identical(f$value[f$var == "prices.latest" & f$item_key == "commodity:oil-brent"], 63.19)
  expect_true(all(x$ledger$prov == "P"))
  expect_identical(x$ledger$source_value, f$value)
  expect_true(any(grepl("mean of the four windows", x$excluded$reason)))
})

test_that("the supplier table's technology copies collapse, asserting identical value and world_share", {
  mss <- tibble::tibble(
    measure = c("iea_mineral_refining", "iea_mineral_refining", "iea_mineral_refining", "atlas_export_share"),
    mineral = c("Lithium", "Lithium", "Lithium", NA), iso3 = c("CHN", "CHN", "CHL", "CHN"),
    tech = c("Batteries", "Electric Vehicles", "Batteries", "Solar"),
    value = c(235, 235, 30, 0.4), world_share = c(0.737, 0.737, 0.094, 0.4)
  )
  d <- sector_supplier_dedupe(mss)
  expect_identical(nrow(d), 2L)
  expect_setequal(d$iso3, c("CHN", "CHL"))
  mss$world_share[2] <- 0.74
  expect_error(sector_supplier_dedupe(mss), "copies differ in world_share")
})

test_that("EI zero-completions are nsr; zeros EI reports are zero; uranium zeros are true zeros", {
  pdm <- raw_tbl(
    iso3c = c("USA", "DEU", "FRA", "USA", "ESP", "KAZ", "NAM", "AUS"),
    tech = c("Oil", "Oil", "Oil", "Solar", "Solar", "Nuclear", "Nuclear", "Batteries"),
    supply_chain = c("Extraction", "Extraction", "Extraction", "Deployment", "Deployment", "Extraction", "Extraction", "Refining"),
    variable = "production_latest",
    value = c(20000, 0, 0, 300, 0, 21000, 0, 50)
  )
  ei_present <- tibble::tibble(ei_var = c("oilprod_kbd", "oilprod_kbd", "solar_twh", "solar_twh"), iso3 = c("USA", "DEU", "USA", "ESP"))
  x <- sector_facts_production(pdm, ei_present)
  st <- stats::setNames(x$facts$status, paste(x$facts$var, x$facts$geo))
  expect_identical(unname(st["production.fuels USA"]), "obs")
  expect_identical(unname(st["production.fuels DEU"]), "zero")
  expect_identical(unname(st["production.fuels FRA"]), "nsr")
  expect_identical(unname(st["production.generation ESP"]), "zero")
  expect_identical(unname(st["production.uranium NAM"]), "zero")
  expect_identical(x$facts$year[x$facts$var == "production.uranium"][1], 2024L)
  expect_identical(x$facts$item_key[x$facts$var == "production.fuels"][1], "fuel:oil")
  # The clean-tech mineral roll-up (Extraction and Refining cells) is never shipped (spec 9); uranium is
  # the Nuclear Extraction cell.
  expect_false("AUS" %in% x$facts$geo)
  expect_identical(x$excluded$n[grepl("roll-up", x$excluded$reason)], 1L)
  expect_identical(unique(x$facts$item_key[x$facts$var == "production.uranium"]), "cell:nuclear-extraction")
  expect_identical(unique(x$facts$item_key[x$facts$var == "production.generation"]), "fuel:solar")
  # A three-stage table (built before 2026-10-26) or a new cell stops instead of shipping nothing.
  old <- pdm
  old$supply_chain[1] <- "Upstream"
  expect_error(sector_facts_production(old, ei_present), "cells without a rule: Oil Upstream")
})

test_that("EIA-filled fuel production ships as observed with its EIA year and note; NA is nsr; Estonia's oil shale is noted", {
  src_ei <- "Energy Institute Statistical Review of World Energy 2026"
  src_eia <- paste0(src_ei, "; EIA International Energy Statistics")
  pdm <- raw_tbl(
    iso3c = c("USA", "JPN", "KOR", "EST", "NZL", "FRA"),
    tech = c("Oil", "Oil", "Gas", "Coal", "Gas", "Gas"), supply_chain = "Extraction", variable = "production_latest",
    value = c(20000, 7.37, 0, NA, 0.12, 0),
    source = c(src_ei, src_eia, src_eia, src_ei, src_eia, src_ei)
  )
  ei_present <- tibble::tibble(ei_var = "oilprod_kbd", iso3 = "USA")
  eia_cells <- tibble::tibble(iso3 = c("JPN", "KOR", "EST", "NZL", "FRA"), ei_var = c("oilprod_kbd", "gasprod_ej", "coalprod_ej", "gasprod_ej", "gasprod_ej"),
                              year = 2025L, value = c(7.37, 0, NA, 0.12, 0.0004), eia_year = c(2025L, 2024L, NA, 2024L, 2024L),
                              eia_basis = c("series", "series", "not_comparable", "series", "series"))
  x <- sector_facts_production(pdm, ei_present, eia_cells = eia_cells)
  f <- x$facts[x$facts$var == "production.fuels", ]
  st <- stats::setNames(f$status, f$geo)
  expect_identical(unname(st[c("USA", "JPN", "KOR", "EST", "NZL", "FRA")]), c("obs", "obs", "zero", "nsr", "obs", "nsr"))
  expect_true(is.nan(f$value[f$geo == "EST"]))
  # The EIA year where the value is EIA's (gas and coal a year behind); EI's own rows carry none.
  expect_identical(stats::setNames(f$obs_year, f$geo)[c("USA", "JPN", "KOR", "NZL", "FRA")], c(USA = NA, JPN = 2025L, KOR = 2024L, NZL = 2024L, FRA = NA))
  # FRA: a processed zero with neither an EI row nor EIA in its source is the zero-fill of an older table.
  expect_identical(f$value[f$geo == "FRA"], 0)
  nt <- stats::setNames(x$notes$note, x$notes$geo)
  expect_identical(unname(nt[c("JPN", "KOR", "NZL", "EST")]), c("eia_production", "eia_production", "eia_production", "ei_oil_shale"))
  expect_false("FRA" %in% x$notes$geo)
  expect_false("EST" %in% x$ledger$geo)
  ck <- sector_eia_check(x, eia_cells)
  expect_identical(c(ck$n, ck$n_missing), c(3L, 0L))
  expect_identical(ck$max_rel, 0)
  expect_identical(ck$not_comparable, "EST Coal")
  eia_cells$value[2] <- 0.5
  expect_gt(sector_eia_check(x, eia_cells)$max_rel, 0.1)
})

test_that("balance and self-sufficiency: EIA production observed, unreported production assumed 0 and said so (user rule)", {
  src_ei <- "EI 2026"
  src_eia <- "EI 2026; EIA International Energy Statistics"
  # EIA-filled table: NA where neither EI nor EIA reports the production (Estonia coal).
  idp <- raw_tbl(
    iso3c = rep(c("JPN", "EST", "AGO"), each = 8),
    tech = rep(rep(c("Oil", "Gas", "Coal", "Fossil"), each = 2), 3),
    variable = rep(c("Production surplus/deficit", "Self-sufficiency"), 12),
    value = c(-6.49, 0.22, -3.19, 2.09, -4.35, 0.26, -14.03, 0.67,
              -0.053, 0, -0.011, 0, NA, NA, -0.156, 0,
              2.14, 922, 0.17, 478, 0, NA, 2.30, 858),
    source = c(rep(src_eia, 4), rep(src_ei, 2), rep(src_eia, 2),
               rep(src_eia, 4), rep(src_ei, 2), rep(src_eia, 2),
               rep(src_eia, 8))
  )
  ei_present <- tibble::tibble(ei_var = c("coalprod_ej", "oil_tes_ej", "oil_tes_ej", "oilprod_kbd"), iso3 = c("JPN", "JPN", "EST", "AGO"))
  eia_cells <- tibble::tibble(iso3 = c("EST", "EST", "EST"), ei_var = c("oilprod_kbd", "gasprod_ej", "coalprod_ej"), year = 2025L,
                              value = c(0, 0, NA), eia_year = c(2025L, 2024L, NA), eia_basis = c("series", "series", "not_comparable"))
  cons <- tibble::tibble(iso3 = "EST", tech = c("Oil", "Gas", "Coal", "Fossil"), value = c(0.053, 0.011, 0.0918, 0.1558))
  x <- sector_facts_balance(idp, ei_present, eia_cells = eia_cells, consumption = cons)
  f <- x$facts
  key <- paste(f$var, f$item_key, f$geo)
  get <- function(v, item, geo, col) f[[col]][key == paste(v, paste0("fuel:", item), geo)]
  # Japan: oil and gas from EIA (observed), coal EI's.
  expect_identical(get("consumption.balance", "oil", "JPN", "status"), "obs")
  expect_identical(get("consumption.balance", "oil", "JPN", "value"), -6.49)
  # Estonia coal: assumed zero, derived (D) from EI consumption: minus consumption and 0%.
  expect_identical(get("consumption.balance", "coal", "EST", "status"), "nsr")
  expect_identical(get("consumption.balance", "coal", "EST", "value"), -0.0918)
  expect_identical(get("consumption.self_sufficiency", "coal", "EST", "value"), 0)
  expect_identical(get("consumption.self_sufficiency", "coal", "EST", "status"), "nsr")
  d <- x$ledger[x$ledger$prov == "D", ]
  expect_identical(nrow(d), 2L)
  expect_identical(d$formula_value[d$var == "consumption.balance"], -0.0918)
  # Estonia fossil: reported in part (oil and gas EIA zeros), coal counted as 0.
  expect_identical(get("consumption.balance", "fossil", "EST", "status"), "obs")
  # Angola: coal self-sufficiency NA (consumes none) is not shipped; the balance 0 is a zero.
  expect_length(get("consumption.self_sufficiency", "coal", "AGO", "value"), 0L)
  expect_identical(get("consumption.balance", "coal", "AGO", "status"), "zero")
  n <- x$notes
  nk <- function(v, item, geo) sort(n$note[n$var == v & n$item_key == paste0("fuel:", item) & n$geo == geo])
  expect_identical(nk("consumption.balance", "coal", "EST"), c("ei_oil_shale", "production_assumed_zero"))
  expect_identical(nk("consumption.balance", "fossil", "EST"), c("ei_oil_shale", "eia_production", "fossil_assumed_zero"))
  expect_identical(nk("consumption.balance", "oil", "JPN"), "eia_production")
  expect_identical(nk("consumption.balance", "coal", "JPN"), character())
  expect_identical(nk("consumption.balance", "oil", "AGO"), "eia_consumption")
  expect_identical(nk("consumption.balance", "gas", "AGO"), c("eia_consumption", "eia_production"))
  expect_identical(sum(x$excluded$n[grepl("D from EI consumption", x$excluded$reason)]), 2L)
  expect_identical(x$excluded$n[x$excluded$reason == "NA (no consumption)"], 1L)
  # Without a consumption figure the assumed-zero balance stays nsr NaN (counted).
  y <- sector_facts_balance(idp, ei_present, eia_cells = eia_cells)
  yb <- y$facts[y$facts$var == "consumption.balance" & y$facts$geo == "EST" & y$facts$item_key == "fuel:coal", ]
  expect_identical(yb$status, "nsr")
  expect_true(is.nan(yb$value))
  expect_true(any(grepl("no EI consumption figure", y$excluded$reason)))
})

test_that("the EIA lookup cells and EI consumption come from EI's own rows", {
  ei_values <- tibble::tibble(ei_var = c("oilprod_kbd", "oil_tes_ej", "gas_tes_ej", "oil_tes_ej", "coal_tes_ej", "gasprod_ej", "coalprod_ej"),
                              iso3 = c("USA", "USA", "USA", "JPN", "JPN", "USA", "USA"), value = c(20000, 38, 33, 6.5, 4.4, 37, 10))
  cells <- sector_eia_gap_cells(c("USA", "JPN", "JPN", NA), ei_values, 2025L)
  expect_identical(paste(cells$iso3, cells$ei_var), paste("JPN", c("oilprod_kbd", "gasprod_ej", "coalprod_ej")))
  expect_identical(cells$ei_consumption, c(6.5, NA, 4.4))
  cons <- sector_ei_consumption(ei_values)
  v <- stats::setNames(cons$value, paste(cons$iso3, cons$tech))
  expect_identical(unname(v[c("JPN Gas", "JPN Coal", "JPN Fossil", "USA Coal", "USA Fossil")]), c(0, 4.4, 10.9, 0, 71))
})

test_that("self-sufficiency and the EV net-export ratio ship verbatim from their renamed processed variables", {
  idp <- raw_tbl(
    iso3c = c("USA", "USA", "JPN", "JPN", "SGP"), tech = c("Gas", "Gas", "Gas", "Gas", "Gas"),
    variable = c("Production surplus/deficit", "Self-sufficiency", "Production surplus/deficit", "Self-sufficiency", "Self-sufficiency"),
    value = c(5.77, 131.6, -3.6, 0, NA)
  )
  x <- sector_facts_balance(idp)
  ss <- x$facts[x$facts$var == "consumption.self_sufficiency", ]
  expect_identical(ss$geo, c("USA", "JPN"))
  expect_identical(ss$value, c(131.6, 0))
  expect_identical(ss$status, c("obs", "zero"))
  expect_identical(x$ledger$variable[x$ledger$var == "consumption.self_sufficiency"], c("Self-sufficiency", "Self-sufficiency"))
  expect_identical(x$excluded$n, 1L)
  expect_identical(x$facts$value[x$facts$var == "consumption.balance" & x$facts$geo == "JPN"], -3.6)
  # Without an EI production row of the fuel (and no EIA value: a table built before the EIA fill),
  # production is assumed to be 0 and the fact says so (user rule 2026-10-11): the zero-filled balance
  # (minus consumption) and self-sufficiency (0%) ship verbatim as nsr with the note
  # production_assumed_zero.
  ep <- tibble::tibble(ei_var = "gasprod_ej", iso3 = "USA")
  y <- sector_facts_balance(idp, ep)
  ys <- y$facts[y$facts$var == "consumption.self_sufficiency", ]
  expect_identical(ys$status, c("obs", "nsr"))
  expect_identical(ys$value, c(131.6, 0))
  yb <- y$facts[y$facts$var == "consumption.balance", ]
  expect_identical(yb$status, c("obs", "nsr"))
  expect_identical(yb$value, c(5.77, -3.6))
  expect_true(all(y$ledger$prov == "P"))
  expect_identical(sort(y$ledger$geo[y$ledger$geo == "JPN"]), c("JPN", "JPN"))
  expect_identical(y$notes$geo, c("JPN", "JPN"))
  expect_true(all(y$notes$note == "production_assumed_zero"))
  expect_identical(sum(y$excluded$n[grepl("assumed 0", y$excluded$reason)]), 2L)

  # IEA EV regions: production and the net-export ratio at Assembly, sales at Deployment (the cells the
  # processed table files them at).
  msm <- raw_tbl(
    iso3c = c("CHN", "CHN", "DEU", "FRA", "DEU", "FRA", "CHN"), tech = "Electric Vehicles",
    supply_chain = c("Assembly", "Assembly", "Assembly", "Assembly", "Assembly", "Assembly", "Deployment"),
    variable = c("net_export_ratio", "Domestic.Production", "net_export_ratio", "net_export_ratio", "Domestic.Production", "Domestic.Production",
                 "Domestic.sales"),
    value = c(0.21, 12.4, -0.08, -0.08, 2.1, 2.1, 11.2),
    explanation = c("IEA region: China (EV Outlook)", "IEA region: China (EV Outlook)", rep("IEA region: European Union (EV Outlook)", 4),
                    "IEA region: China (EV Outlook)")
  )
  e <- sector_facts_ev_regions(msm)
  ner <- e$facts[e$facts$var == "manufacturing.ev_net_export_ratio", ]
  expect_identical(sort(ner$geo), c("R:iea_ev:china", "R:iea_ev:european-union"))
  expect_identical(ner$value[ner$geo == "R:iea_ev:european-union"], -0.08)
  cell_of <- stats::setNames(e$facts$item_key, e$facts$var)
  expect_identical(unname(cell_of[c("production.ev_region", "manufacturing.ev_net_export_ratio", "production.ev_region_sales")]),
                   c("cell:electric-vehicles-assembly", "cell:electric-vehicles-assembly", "cell:electric-vehicles-deployment"))
  # The series follow the same cells.
  ser <- sector_iea_ev_series(data.frame(Region = "China", Year = 2024L, `Domestic sales` = 11.2, `Domestic Production` = 12.4, check.names = FALSE))
  expect_identical(stats::setNames(ser$item_key, ser$var)[c("production.ev_region", "production.ev_region_sales")],
                   c(production.ev_region = "cell:electric-vehicles-assembly", production.ev_region_sales = "cell:electric-vehicles-deployment"))
  expect_true(all(e$ledger$prov == "P"))
  expect_identical(e$excluded$n, 2L)
  expect_identical(unname(e$region_of[c("DEU", "CHN")]), c("European Union", "China"))
  moved <- msm
  moved$supply_chain[7] <- "Assembly"
  expect_error(sector_facts_ev_regions(moved), "SECTOR_EV_REGION_CELLS expects")
  msm$value[4] <- -0.09
  expect_error(sector_facts_ev_regions(msm), "copies differ")
})

test_that("non-finite growth becomes na_growth with NaN, and the per-person table ships once", {
  ea <- raw_tbl(
    iso3c = c("USA", "DEU", "USA", "DEU", "TLS"), tech = c("Oil", "Oil", "Oil", "Oil", "Solar"),
    variable = c("Energy consumption per capita", "Energy consumption per capita",
                 "Energy consumption per capita growth", "Energy consumption per capita growth", "Energy consumption per capita growth"),
    value = c(0.105, 0.0, -0.02, NaN, Inf)
  )
  ec <- ea
  ei_present <- tibble::tibble(ei_var = "oil_tes_ej", iso3 = "USA")
  x <- sector_facts_consumption(ea, ec, ei_present)
  g <- x$facts[x$facts$var == "consumption.pc_change", ]
  expect_identical(g$status, c("obs", "na_growth", "na_growth"))
  expect_true(all(is.nan(g$value[g$status == "na_growth"])))
  expect_identical(x$facts$status[x$facts$var == "consumption.per_capita" & x$facts$geo == "DEU"], "nsr")
  expect_identical(x$excluded$n[grepl("Inf or NaN", x$excluded$reason)], 2L)
  ec$value[1] <- 0.106
  expect_error(sector_facts_consumption(ea, ec, ei_present), "ship-once")
})

test_that("RCA below USD 50 m of exports is 'too small to compare' (nq, NaN, not in the ledger)", {
  ef <- raw_tbl(
    iso3c = c("CHN", "CHN", "MLT", "MLT"), tech = "Batteries", supply_chain = "Assembly",
    variable = c("export_rca", "exports", "export_rca", "exports"), value = c(3.1, 8.9e10, 12.5, 2e7)
  )
  x <- sector_facts_rca(ef)
  expect_identical(x$facts$status, c("obs", "nq"))
  expect_true(is.nan(x$facts$value[2]))
  expect_identical(x$ledger$geo, "CHN")
  expect_identical(x$facts$item_key[1], "cell:batteries-assembly")
})

test_that("reserves: listed-without-a-figure holders are nq; the roster completion is not shipped", {
  res <- raw_tbl(
    iso3c = c("CHL", "AUS", "BOL", "SAU", "SAU", "KAZ"),
    variable = c("Lithium Reserves", "Lithium Reserves", "Lithium Reserves", "Oil Reserves", "Lithium Reserves", "Nuclear Reserves"),
    value = c(9300, 7000, NA, 267.2, NA, 815000)
  )
  unq <- tibble::tibble(tech = "Lithium", ISO3166_alpha3 = "BOL")
  x <- sector_facts_reserves(res, unq)
  f <- x$facts
  expect_identical(f$status[f$geo == "BOL"], "nq")
  expect_true(is.nan(f$value[f$geo == "BOL"]))
  expect_false("SAU" %in% f$geo[f$var == "resources.mineral_reserves"])
  expect_identical(f$var[f$geo == "KAZ"], "resources.uranium_resources")
  expect_identical(f$item_key[f$geo == "KAZ"], "cell:nuclear-extraction")
  expect_identical(f$year[f$var == "resources.fuel_reserves"], 2020L)
  expect_identical(f$year[f$geo == "CHL"], 2025L)
  # nq rows carry NaN, which is never a parity value: the ledger lists the shipped figures only.
  expect_false("BOL" %in% x$ledger$geo)
  expect_identical(x$ledger$source_value, x$facts$value[!is.na(x$facts$value)])
})

test_that("EI series: successor zeros before they exist and the USSR's zeros after it ends are nsr", {
  # Repair review 2026-10-11: EI fills the USSR successors with 0 before 1985 and the USSR with 0
  # after it; those rows are not observations.
  ei <- data.frame(
    Country = c("USSR", "USSR", "Ukraine", "Ukraine", "Japan", "Japan", "Total World", "Total World",
                "Other Europe", "Slovenia", "Slovenia", "USSR"),
    Year = c(1984L, 1985L, 1984L, 1985L, 1984L, 1985L, 1984L, 1985L, 1989L, 1989L, 1990L, 1983L),
    ISO3166_alpha3 = c("SUN", "SUN", "UKR", "UKR", "JPN", "JPN", "WLD", "WLD", "O-EUR", "SVN", "SVN", "SUN"),
    Var = "nuclear_tes_ej",
    Value = c(1.57, 0, 0, 0.58, 2.1, 0, 5, 5.2, 0.04, 0, 0.05, 1.4),
    stringsAsFactors = FALSE
  )
  vm <- tibble::tibble(var = "consumption.total_ej", item_key = "fuel:nuclear", ei_var = "nuclear_tes_ej")
  s <- sector_ei_series(ei, vm, geo_codes = c("SUN", "UKR", "JPN", "SVN"), years = 1983:1990)
  o <- s$obs
  st <- stats::setNames(o$status, paste(o$geo, o$year))
  expect_identical(unname(st[c("UKR 1984", "SUN 1985", "SVN 1989")]), rep("nsr", 3))
  expect_true(all(is.nan(o$value[o$status == "nsr"])))
  # True zeros and observations stay: Japan's 0 in 1985, the USSR in 1983-84, Ukraine from 1985.
  expect_identical(unname(st[c("JPN 1985", "SUN 1984", "SUN 1983", "UKR 1985", "SVN 1990")]), c("zero", "obs", "obs", "obs", "obs"))
  expect_identical(s$successor_nsr, 3L)
  # The EI residual is computed on the shipped values before the reclassification (zeros add nothing).
  expect_equal(o$value[o$geo == "RES:ei" & o$year == 1985L], 5.2 - 0.58, tolerance = 1e-12)
})

# ---- Six stages (schema 2, 2026-10-28) -------------------------------------------------------------

test_that("critical-mineral trade is keyed by mineral and stage, each stage its own basket and world total", {
  fl <- tibble::tibble(
    iso3c = c("CHN", "CHN", "CHN", "AUS", "AUS", "AUS"), mineral = "Lithium",
    supply_chain = c("Extraction", "Refining", "Recovery", "Extraction", "Refining", "Recovery"),
    export_source = "reported", exports = c(1, 30, 2, 9, 0, 0), exp_share = c(1e-5, 3e-4, 2e-5, 4e-4, 0, 0),
    market_share = c(0.1, 1, 1, 0.9, 0, 0), Year = 2024L
  )
  x <- sector_facts_critmin(fl)
  ex <- x$facts[x$facts$var == "trade.critmin_exports", ]
  # One fact per (economy, mineral, stage): the same economy and mineral at three stages are three items.
  expect_setequal(unique(ex$item_key), paste0("mineral_stage:lithium-", c("extraction", "refining", "recovery")))
  expect_false(anyDuplicated(paste(ex$item_key, ex$geo)) > 0)
  w <- stats::setNames(x$world$value, x$world$item_key)
  expect_identical(unname(w[paste0("mineral_stage:lithium-", c("extraction", "refining", "recovery"))]), c(10, 30, 2))
  expect_identical(x$check$max_rel, 0)
  expect_identical(x$pairs$stage, c("Extraction", "Refining", "Recovery"))
  expect_true(all(grepl("Lithium (Extraction|Refining|Recovery)$", x$ledger$key)))
  items <- sector_mineral_stage_items(x$pairs)
  expect_identical(items$slug, c("lithium-extraction", "lithium-refining", "lithium-recovery"))
  expect_identical(items$label, paste0("Lithium — ", c("Extraction", "Refining", "Recovery")))
  # A table built before the six-stage minerals trade (no stage) or with another stage stops.
  expect_error(sector_facts_critmin(fl[setdiff(names(fl), "supply_chain")]), "has no supply_chain")
  bad <- fl
  bad$supply_chain[1] <- "Assembly"
  expect_error(sector_facts_critmin(bad), "stages outside Extraction, Refining, Recovery")
})

test_that("mineral, mineral-stage, fuel and commodity items link to the cells of their stage", {
  cells <- sector_cell_items(character())
  ml <- tibble::tibble(mineral = c("Lithium", "Lithium"), tech = c("Batteries", "Electric Vehicles"), mineral_demand_share = c(0.2, 0.8))
  pt <- tibble::tibble(sub_sector = c("Oil_Brent", "Lithium"), tech = c("Oil", "Batteries"), supply_chain = c("Extraction", "Refining"))
  cm <- tibble::tibble(sub_sector = c("Oil_Brent", "Lithium"), slug = c("oil-brent", "lithium"))
  ms <- sector_mineral_stage_items(tibble::tibble(mineral = c("lithium", "lithium"), stage = c("Refining", "Recovery")))
  l <- sector_item_links(ml, pt, cm, cells, ms)
  of <- function(k) sort(l$cell[l$item_key == k])
  # Minerals: the Extraction and Refining cells of the technologies that use them (mining and reserves
  # show at Extraction, refining at Refining, by the measures' link_stage).
  expect_identical(of("mineral:lithium"), sort(c("batteries-extraction", "batteries-refining", "electric-vehicles-extraction", "electric-vehicles-refining")))
  expect_identical(of("mineral_stage:lithium-recovery"), c("batteries-recovery", "electric-vehicles-recovery"))
  expect_identical(l$weight[l$item_key == "mineral_stage:lithium-refining" & l$cell == "electric-vehicles-refining"], 0.8)
  expect_identical(of("fuel:oil"), c("oil-deployment", "oil-extraction"))
  expect_identical(of("fuel:solar"), "solar-deployment")
  expect_identical(of("commodity:oil-brent"), "oil-extraction")
  expect_identical(of("commodity:lithium"), "batteries-refining")
  # The ILO activities serve their stages; Critical Minerals policy covers the clean-tech material stages.
  expect_identical(sort(unique(sub("^.*-", "", of("activity:mining-utilities")))), c("extraction", "recovery"))
  expect_identical(sort(unique(sub("^.*-", "", of("activity:manufacturing")))), c("assembly", "components", "refining"))
  expect_identical(unique(sub("^.*-", "", of("activity:construction"))), "deployment")
  expect_setequal(unique(sub("^.*-", "", of("policy_tag:critical-minerals"))), c("extraction", "refining", "recovery"))
  expect_error(sector_item_links(ml, pt[c("sub_sector", "tech")], cm, cells, ms), "needs the processed prices' supply_chain")
})

test_that("IEA ETP shares ship on Components and Assembly with the note same_figure_pair", {
  mss <- tibble::tibble(measure = "iea_manufacturing_capacity", iso3 = c("CHN", "USA", "CHN"), tech = c("Batteries", "Batteries", "Solar"),
                        supply_chain = c("Components", "Components", "Assembly"), year = 2024L, world_share = c(0.8, 0.05, 0.8))
  fd <- raw_tbl(iso3c = c("CHN", "CHN", "USA", "USA", "DEU", "FRA", "DEU", "FRA"), tech = "Batteries",
                supply_chain = rep(c("Components", "Assembly"), 4), variable = "Market Share",
                value = c(0.7, 0.7, 0.1, 0.1, 0.12, 0.12, 0.12, 0.12),
                explanation = rep(c("IEA ETP pipeline", "IEA ETP pipeline; copied from Components: the source has no separate figure"), 4))
  x <- sector_facts_mfg(fd, mss, eu_members = c("DEU", "FRA"))
  now <- x$facts[x$facts$var == "manufacturing.share_now", ]
  expect_setequal(paste(now$item_key, now$geo), c(paste("cell:batteries-components", c("E:iea_etp:china", "E:iea_etp:us")),
                                                  paste("cell:batteries-assembly", c("E:iea_etp:china", "E:iea_etp:us")),
                                                  "cell:solar-assembly E:iea_etp:china", "cell:solar-components E:iea_etp:china"))
  expect_identical(now$value[now$item_key == "cell:batteries-assembly" & now$geo == "E:iea_etp:china"], 0.8)
  p35 <- x$facts[x$facts$var == "manufacturing.share_2035", ]
  expect_identical(sort(unique(p35$item_key)), c("cell:batteries-assembly", "cell:batteries-components"))
  expect_identical(nrow(p35), 6L)
  expect_true(all(x$notes$note == "same_figure_pair"))
  expect_identical(nrow(x$notes), nrow(x$facts))
  expect_identical(x$excluded$n[grepl("EU member copies", x$excluded$reason)], 2L)
  # A pair whose two stages differ stops.
  fd$value[2] <- 0.71
  expect_error(sector_facts_mfg(fd, mss, eu_members = c("DEU", "FRA")), "Components and Assembly differ")
})

test_that("manufacturing capex shows on Components and Assembly, critical minerals on Refining; investment@1 counts once", {
  im <- raw_tbl(
    iso3c = c("CHN", "CHN", "USA", "USA", "CHN", "CHN", "USA"), tech = c("Batteries", "Batteries", "Batteries", "Batteries", "Batteries", "Electric Vehicles", "Solar"),
    supply_chain = c("Components", "Assembly", "Components", "Assembly", "Refining", "Refining", "Deployment"),
    variable = "Annual Investment (USD bn, 2024$)", Year = 2025L, value = c(40, 40, 10, 10, 3, 4.5, 20),
    explanation = c(rep("Annual investment; manufacturing capex has no product detail, so the same amount is replicated onto Components and Assembly (do not sum across stages)", 4),
                    rep("Annual investment; critical-minerals (refining) capex, split between Batteries and Electric Vehicles rather than counted in both", 2),
                    "Annual investment")
  )
  x <- sector_facts_investment(im)
  st <- stats::setNames(x$facts$status, paste(x$facts$item_key, x$facts$geo))
  # 2025 critical-minerals (Refining) and solar rows are estimates; manufacturing rows are observed.
  expect_identical(unname(st[c("cell:batteries-refining CHN", "cell:solar-deployment USA", "cell:batteries-assembly CHN")]), c("est", "est", "obs"))
  n <- x$notes
  expect_identical(sort(unique(n$note)), c("cim_minerals_split", "same_figure_pair"))
  expect_identical(sort(n$item_key[n$note == "cim_minerals_split"]), c("cell:batteries-refining", "cell:electric-vehicles-refining"))
  expect_identical(sum(n$note == "same_figure_pair"), 4L)
  once <- sector_count_once(x$facts, n)
  expect_identical(sum(once$value), 40 + 10 + 3 + 4.5 + 20)
  expect_false(any(once$item_key == "cell:batteries-assembly"))
  # The headline sums count the copy once.
  geo <- tibble::tibble(code = c("CHN", "USA"), name = c("China", "United States"), kind = "country", group = c("china", "core"))
  vars <- tibble::tibble(id = "investment.annual", family = "investment", native_level = "country")
  h <- sector_headlines(sector_summary_empty(), x$facts, geo, tibble::tibble(axis = character(), slug = character(), label = character()),
                        vars, notes = n)
  inv <- Filter(function(z) z$id == "investment", h)[[1]]
  expect_identical(inv$values$total_bn, 77.5)
  expect_identical(inv$values$china, 47.5 / 77.5)
  # concentration@1 counts the pair as one measure too: the Assembly copy is not a second hit or row.
  expect_identical(sector_pair_copy_slices(x$facts, n), paste("investment.annual", "cell:batteries-assembly", 2025L, sep = "\r"))
  slugs <- c("batteries-refining", "batteries-components", "batteries-assembly", "electric-vehicles-refining", "solar-deployment")
  items <- tibble::tibble(axis = "cell", slug = slugs, label = slugs, stage = c("Refining", "Components", "Assembly", "Refining", "Deployment"),
                          headline = TRUE)
  sm <- sector_summary_empty()[rep(NA_integer_, length(slugs)), ]
  sm$var <- "investment.annual"
  sm$item_key <- paste0("cell:", slugs)
  sm$year <- 2025L
  sm$eu_mode <- "members"
  sm$top1_share <- c(1, 0.8, 0.8, 1, 0.4)
  h <- sector_headlines(sm, x$facts, geo, items, vars, notes = n)
  conc <- Filter(function(z) z$id == "concentration", h)[[1]]
  expect_identical(c(conc$values$n, conc$values$n_total), c(3L, 4L))
  h0 <- sector_headlines(sm, x$facts, geo, items, vars, notes = NULL)
  conc0 <- Filter(function(z) z$id == "concentration", h0)[[1]]
  expect_identical(c(conc0$values$n, conc0$values$n_total), c(4L, 5L))
  # Replicated capex that differs between the two stages stops.
  im$value[2] <- 41
  expect_error(sector_facts_investment(im), "replicated manufacturing capex differs")
})

test_that("wages: the six stages read three ILO activities, one value each", {
  cc <- raw_tbl(iso3c = "USA", tech = "Batteries", supply_chain = SUPPLY_CHAIN_STAGES, variable = "earnings_usd",
                value = c(8100, 7055, 7055, 7055, 6000, 8100), Year = 2024L)
  x <- sector_facts_costs(cc)
  w <- x$facts[x$facts$var == "prices.wages", ]
  expect_identical(stats::setNames(w$value, w$item_key)[c("activity:mining-utilities", "activity:manufacturing", "activity:construction")],
                   c(`activity:mining-utilities` = 8100, `activity:manufacturing` = 7055, `activity:construction` = 6000))
  cc$value[2] <- 7000
  expect_error(sector_facts_costs(cc), "copies differ")
})

test_that("resource potentials are the Deployment cells; a table filed elsewhere stops", {
  pv <- raw_tbl(iso3c = c("USA", "CHN"), tech = "Solar", supply_chain = "Deployment", variable = "Solar PV power potential per sq km", value = c(4.4, 4.0))
  x <- sector_facts_potential(pv, pv[0, ], pv[0, ])
  expect_identical(unique(x$facts$item_key), "cell:solar-deployment")
  pv$supply_chain <- "Extraction"
  expect_error(sector_facts_potential(pv, pv[0, ], pv[0, ]), "not Deployment")
})
