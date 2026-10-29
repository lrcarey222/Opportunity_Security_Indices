# Development & finance in Sector Insights (R/dashboard/sector/sector_dev.R; contract.ts DEV_SPEC): the
# raw values behind Development Potential (WDI, Doing Business, OECD CRS) and Friendshore's outbound
# component (IMF DIP), parsed with the builders' own functions on small synthetic extracts, the parity
# with the builders' rankings, the conduit flags, and the sector-dev chunk with its contract checks.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "dashboard", "sector", "sector_files.R"))
for (f in sector_files()) source(file.path(repo_root, f))
for (f in c("R/utils/scurve.R", "R/utils/schema.R", "R/utils/country.R", "R/themes/partnership_strength/partnership_strength_helpers.R",
            "R/themes/partnership_strength/stronger_development.R", "R/themes/partnership_strength/safer_friendshore.R")) {
  source(file.path(repo_root, f))
}
suppressPackageStartupMessages(library(dplyr))

ci <- data.frame(iso3c = c("BRA", "IDN", "CHN", "DEU", "JPN", "USA", "HKG", "NLD"),
                 country = c("Brazil", "Indonesia", "China", "Germany", "Japan", "United States", "Hong Kong SAR, China", "Netherlands"))
gdp <- data.frame(iso3c = ci$iso3c, year = 2023L, NY.GDP.MKTP.CD = c(2.17e12, 1.37e12, 1.79e13, 4.5e12, 4.2e12, 2.7e13, 3.8e11, 1.1e12))

wdi_raw <- data.frame(
  Country.Name = c("Brazil", "Brazil", "Indonesia", "Indonesia", "World"),
  Country.Code = c("BRA", "BRA", "IDN", "IDN", "WLD"),
  Series.Name = c("GDP per capita", "CPI", "GDP per capita", "CPI", "GDP per capita"),
  Series.Code = c("NY.GDP.PCAP.KD", "FP.CPI.TOTL", "NY.GDP.PCAP.KD", "FP.CPI.TOTL", "NY.GDP.PCAP.KD"),
  X2022..YR2022. = c("9000", "300", "4200", "150", "12000"),
  X2023..YR2023. = c("9200", "320", "..", "160", "12100"),
  X = NA
)

crs_raw <- function() {
  rows <- expand.grid(donor = c("DEU", "JPN"), recipient = c("BRA", "IDN", "LDC"), sector = c("All sectors", "Energy", "Industry", "Mineral resources and mining", "Coal", "Oil and gas (upstream)"),
                      year = 2013:2014, stringsAsFactors = FALSE)
  v <- c(`All sectors` = 100, Energy = 20, Industry = 5, `Mineral resources and mining` = 4, Coal = 1, `Oil and gas (upstream)` = 2)
  n <- nrow(rows)
  m <- matrix(NA_character_, n, 36)
  m[, 5] <- rows$donor
  m[, 6] <- ifelse(rows$donor == "DEU", "Germany", "Japan")
  m[, 7] <- rows$recipient
  m[, 8] <- rows$recipient
  m[, 10] <- rows$sector
  m[, 12] <- "Official Development Assistance"
  m[, 18] <- "Disbursements"
  m[, 20] <- "Constant prices"
  m[, 26] <- "US dollar"
  m[, 27] <- as.character(rows$year)
  m[, 29] <- as.character(unname(v[rows$sector]) * ifelse(rows$recipient == "IDN", 2, 1) * ifelse(rows$donor == "JPN", 1.5, 1))
  m[, 31] <- "2023"
  m[, 36] <- "Millions"
  o <- as.data.frame(m, stringsAsFactors = FALSE)
  names(o) <- paste0("X", seq_len(36))
  o$X27 <- as.integer(o$X27)
  o$X29 <- as.numeric(o$X29)
  o
}

dip_raw <- data.frame(
  COUNTRY = c("Germany", "Germany", "Germany", "Japan", "Japan", "Netherlands", "Germany"),
  COUNTERPART_COUNTRY = c("China, P.R.: Mainland", "Netherlands, The", "United States", "United States", "World", "China, P.R.: Mainland", "Brazil"),
  DV_TYPE = "Reported official data",
  INDICATOR = SECTOR_DEV_DIP_INDICATOR,
  X2020 = c(100, 50, 400, 300, 900, 70, 0),
  X2021 = c(120, NA, 420, NA, 950, 80, 0),
  X2019 = c(1, 1, 1, 1, 1, 1, 1),
  stringsAsFactors = FALSE
)

test_that("WDI values are the Development Potential builder's, minus the series it leaves out; years stated", {
  w <- sector_dev_wdi(wdi_raw, ci$iso3c, dict_series = "NY.GDP.PCAP.KD")
  expect_identical(w$used, "NY.GDP.PCAP.KD")
  expect_identical(w$values$iso3, c("BRA", "IDN"))
  b <- partnership_strength_build_wdi_long(wdi_raw, valid_iso = ci$iso3c)
  expect_identical(w$values$value, b$value[b$Series.Code == "NY.GDP.PCAP.KD"][match(c("BRA", "IDN"), b$Country.Code[b$Series.Code == "NY.GDP.PCAP.KD"])])
  expect_identical(w$values$value[1], 9100)
  expect_identical(w$values$year_max, c(2023L, 2022L))
  expect_identical(w$values$n_years, c(2L, 1L))
  expect_error(sector_dev_wdi(wdi_raw, ci$iso3c, dict_series = character()), "not in config/sector_dictionary.yml dev_indicators")
})

test_that("CRS flows by donor, recipient, aid sector and year; the stage totals rank as the builder ranks them", {
  o <- crs_raw()
  crs <- sector_dev_crs(o, ci$iso3c)
  expect_identical(crs$meta$BaseYear, "2023")
  expect_false(any(crs$flows$recipient == "LDC"))
  mining <- crs$flows[crs$flows$item == "mining" & crs$flows$donor == "DEU" & crs$flows$recipient == "BRA" & crs$flows$year == 2013L, ]
  expect_identical(mining$value, 1)  # 4 - 1 - 2: mining net of coal and upstream oil and gas
  stage <- sector_dev_crs_stage(o, gdp, 2023, ci$iso3c)
  b <- partnership_strength_build_oecd_supplychain(o, gdp, 2023, valid_iso = ci$iso3c)
  pc <- sector_dev_crs_parity(stage, b, crs$flows)
  expect_identical(pc$status, rep("PASS", 3))
  expect_identical(stage$aid[stage$recipient == "IDN" & stage$supply_chain == "Downstream"], 2 * (20 + 30) * 2)
})

test_that("IMF DIP positions: the builder's steps, conduit centres flagged, parity with its outbound ranking", {
  cr <- partnership_strength_conduit_rule("conduits", "both")
  dip <- sector_dev_dip(dip_raw, ci, 2020:2021, partnership_strength_conduit_centres()$iso3)
  expect_setequal(paste(dip$reporter, dip$counterpart), c("DEU CHN", "DEU NLD", "DEU USA", "JPN USA", "NLD CHN", "DEU BRA"))
  expect_identical(dip$value[dip$reporter == "DEU" & dip$counterpart == "CHN"], 110)
  expect_identical(dip$n_years[dip$reporter == "JPN"], 1L)
  expect_identical(dip$value[dip$counterpart == "BRA"], 0)
  expect_identical(dip$conduit[dip$counterpart == "NLD"], "counterpart")
  expect_identical(dip$conduit[dip$reporter == "NLD"], "investor")
  b <- partnership_strength_build_outbound_edges(dip_raw, ci, gdp, gdp_year = 2023, years = 2020:2021, conduit_iso = cr$iso3, conduit_scope = cr$scope)
  expect_identical(sector_dev_dip_parity(dip, b, gdp, 2023, cr$scope, cr$iso3)$status, "PASS")
  lg <- partnership_strength_conduit_rule("legacy")
  b2 <- partnership_strength_build_outbound_edges(dip_raw, ci, gdp, gdp_year = 2023, years = 2020:2021, conduit_iso = lg$iso3, conduit_scope = lg$scope)
  expect_identical(sector_dev_dip_parity(dip, b2, gdp, 2023, lg$scope, lg$iso3)$status, "PASS")
})

test_that("the synthetic sector-dev chunk passes the contract checks and decodes identical()", {
  items <- sector_synthetic_items()
  dev <- sector_dev_synthetic(items[items$axis == "cell", , drop = FALSE])
  geo <- sector_synthetic_geo()
  all_items <- dplyr::bind_rows(items, dev$items[names(items)])
  gi <- function(x) sector_index(x, geo$code, "geo")
  ii <- function(x) sector_index(x, sector_item_key(all_items$axis, all_items$slug), "item")
  ch <- sector_dev_chunk(dev$part, gi, ii)
  sizes <- list(geo = nrow(geo), items = nrow(all_items), vars = 1L)
  expect_identical(sector_check_chunk("sector-dev", ch, sizes), character())
  bad <- ch
  bad$dev$dip$conduit[1] <- "both"
  expect_match(sector_check_chunk("sector-dev", bad, sizes), "conduit disagrees", all = FALSE)
  expect_true(all(dev$facts$var %in% SECTOR_DEV_VARS))
  expect_true(any(dev$part$oda$value < 0))
  links <- dev$links
  expect_setequal(unique(links$item_key), c("aid_sector:energy", "aid_sector:industry", "aid_sector:mining"))
  # The aid sectors and items carry no stage of their own; the links do.
  expect_true(all(is.na(dev$items$stage)))
})

test_that("aid sectors link to the cells the pipeline's six-stage purpose crosswalk files their codes under", {
  cw <- utils::read.csv(file.path(repo_root, "data", "reference", "oecd_crs_purpose_crosswalk.csv"), stringsAsFactors = FALSE)
  cells <- sector_cell_items(character())
  l <- sector_dev_aid_links(cw, cells)
  stage_of <- function(item) sort(unique(cells$stage[match(l$cell[l$item_key == item], cells$slug)]))
  # Energy (23x) at Deployment, industry (321x) at Refining, Components and Assembly, mining (322x less
  # "Coal" and "Oil and gas (upstream)") at Extraction; no CRS code funds recycling (Recovery).
  expect_identical(stage_of("aid_sector:energy"), "Deployment")
  expect_identical(stage_of("aid_sector:industry"), c("Assembly", "Components", "Refining"))
  expect_identical(stage_of("aid_sector:mining"), "Extraction")
  expect_false(any(endsWith(l$cell, "-recovery")))
  expect_false("aid_sector:all-sectors" %in% l$item_key)
  # Only the technologies the crosswalk names: no energy code funds Batteries or Green Hydrogen deployment.
  expect_false(any(c("batteries-deployment", "green-hydrogen-deployment") %in% l$cell[l$item_key == "aid_sector:energy"]))
  expect_true(all(c("solar-deployment", "electric-grid-deployment", "nuclear-deployment") %in% l$cell[l$item_key == "aid_sector:energy"]))
  # The netted mining codes alone link nothing; the synthetic crosswalk follows the same rule.
  expect_identical(nrow(sector_dev_aid_links(cw[cw$purpose_code %in% SECTOR_DEV_MINING_NET_CODES, ], cells)), 0L)
  syn <- sector_dev_aid_links(sector_dev_synthetic_crosswalk(), cells)
  expect_false("coal-extraction" %in% syn$cell)
  expect_identical(sector_dev_aid_sector_of(c("23230", "32169", "32220", "32261", "15110")), c("energy", "industry", "mining", NA, NA))
  bad <- cw
  bad$supply_chain[1] <- "Downstream"
  expect_error(sector_dev_aid_links(bad, cells), "not supply-chain stages")
})

test_that("the stored aid components are checked per tech and stage against the purpose-code builder", {
  aid <- tibble::tibble(
    iso3c = c("BRA", "BRA", "IDN", "IDN"), tech = c("Solar", "Wind", "Solar", "Wind"), supply_chain = "Deployment",
    aid_index_supplychain = c(0.9, 0.1, 0.1, 0.9), aidgdp_index_supplychain = c(0.8, 0.2, 0.2, 0.8)
  )
  indicators <- tibble::tibble(iso3c = c("BRA", "IDN"), development_index = c(0.4, 0.6), doingbusiness_index = c(0.5, 0.5), energy_use_index = c(0.3, 0.7))
  stored <- tidyr::pivot_longer(
    dplyr::left_join(aid, indicators, by = "iso3c"),
    c(aid_index_supplychain, aidgdp_index_supplychain, development_index, doingbusiness_index, energy_use_index),
    names_to = "variable", values_to = "value"
  ) %>%
    dplyr::transmute(partner_iso = iso3c, tech, supply_chain, variable, value, data_type = "index")
  pp <- sector_dev_processed_parity(stored, indicators, aid)
  expect_true(all(pp$status == "PASS"))
  # A stored aid value that differs by technology from the builder fails.
  stored$value[stored$partner_iso == "BRA" & stored$tech == "Wind" & stored$variable == "aid_index_supplychain"] <- 0.9
  pp <- sector_dev_processed_parity(stored, indicators, aid)
  expect_equal(pp$status[grepl("stored aid_index_supplychain", pp$check)], "FAIL")
})
