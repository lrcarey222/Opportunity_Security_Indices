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
  w <- tibble::tibble(iso3c = c("USA", "USA"), supply_chain = "Midstream", value = c(5000, 5000), Year = c(2024L, 2023L))
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
    supply_chain = c("Upstream", "Upstream", "Upstream", "Downstream", "Downstream", "Upstream", "Upstream", "Upstream"),
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
  # The mineral Upstream roll-up is never shipped (spec 9).
  expect_false("AUS" %in% x$facts$geo)
  expect_identical(x$excluded$n[grepl("roll-up", x$excluded$reason)], 1L)
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
  # Without an EI production row of the fuel, the pipeline's production is a zero-fill: both measures
  # ship as nsr with NaN and are counted as an nsr reclassification (repair review 2026-10-11).
  ep <- tibble::tibble(ei_var = "gasprod_ej", iso3 = "USA")
  y <- sector_facts_balance(idp, ep)
  ys <- y$facts[y$facts$var == "consumption.self_sufficiency", ]
  expect_identical(ys$status, c("obs", "nsr"))
  expect_true(is.nan(ys$value[2]))
  yb <- y$facts[y$facts$var == "consumption.balance", ]
  expect_identical(yb$status, c("obs", "nsr"))
  expect_false("JPN" %in% y$ledger$geo)
  expect_identical(sum(y$excluded$n[grepl("^nsr reclassification", y$excluded$reason)]), 2L)

  msm <- raw_tbl(
    iso3c = c("CHN", "CHN", "DEU", "FRA", "DEU", "FRA"), tech = "Electric Vehicles",
    variable = c("net_export_ratio", "Domestic.Production", "net_export_ratio", "net_export_ratio", "Domestic.Production", "Domestic.Production"),
    value = c(0.21, 12.4, -0.08, -0.08, 2.1, 2.1),
    explanation = c("IEA region: China (EV Outlook)", "IEA region: China (EV Outlook)", rep("IEA region: European Union (EV Outlook)", 4))
  )
  e <- sector_facts_ev_regions(msm)
  ner <- e$facts[e$facts$var == "manufacturing.ev_net_export_ratio", ]
  expect_identical(sort(ner$geo), c("R:iea_ev:china", "R:iea_ev:european-union"))
  expect_identical(ner$value[ner$geo == "R:iea_ev:european-union"], -0.08)
  expect_true(all(e$ledger$prov == "P"))
  expect_identical(e$excluded$n, 2L)
  expect_identical(unname(e$region_of[c("DEU", "CHN")]), c("European Union", "China"))
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
    iso3c = c("CHN", "CHN", "MLT", "MLT"), tech = "Batteries", supply_chain = "Midstream",
    variable = c("export_rca", "exports", "export_rca", "exports"), value = c(3.1, 8.9e10, 12.5, 2e7)
  )
  x <- sector_facts_rca(ef)
  expect_identical(x$facts$status, c("obs", "nq"))
  expect_true(is.nan(x$facts$value[2]))
  expect_identical(x$ledger$geo, "CHN")
  expect_identical(x$facts$item_key[1], "cell:batteries-midstream")
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
