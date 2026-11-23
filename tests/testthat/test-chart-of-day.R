# Chart of the Day picker (R/chart_of_day/): labels, weights, cooldowns, the date-seeded draw, and the switch to the
# latest 12 months of monthly trade, on a small synthetic world (no processed data needed).

cod_root <- function() normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
for (f in list.files(file.path(cod_root(), "R", "chart_of_day"), pattern = "[.]R$", full.names = TRUE)) source(f, local = FALSE)
cod_cfg <- function() yaml::read_yaml(file.path(cod_root(), "config", "chart_of_day.yml"))

COD_FIX_ISO <- c("USA", "JPN", "KOR", "DEU", "CHN", "MEX", "VNM", "AUS", "IND", "CAN", "GBR", "FRA", "POL", "HUN", "ESP")

#' Raw tables shaped like data/processed, small enough to reason about.
cod_fixture_raw <- function() {
  iso <- COD_FIX_ISO
  set <- expand.grid(iso3c = iso, tech = c("Batteries", "Solar"), supply_chain = c("Extraction", "Refining", "Components", "Assembly"),
                     stringsAsFactors = FALSE)
  v <- seq_len(nrow(set))
  long <- function(variable, value) data.frame(set, sub_sector = "All", variable = variable, data_type = "raw", value = value, Year = 2024)
  tc <- rbind(long("exports", 1e9 * (v %% 7 + 1)), long("imports", 1e9 * (v %% 5 + 1)), long("market_share", (v %% 7 + 1) / 60),
              long("gdp", 1e12), long("deficit_gdp", 0.001))
  pc <- do.call(rbind, lapply(iso, function(r) data.frame(reporter_iso = r, partner_iso = setdiff(iso, r)[1:5], tech = "Batteries",
                                                           supply_chain = "Assembly", imports_usd = c(6e8, 2e8, 1e8, 5e7, 5e7))))
  ie <- data.frame(importer_iso = set$iso3c, tech = set$tech, supply_chain = set$supply_chain, imports_usd = 6e8, china_share = 0.5)
  im <- expand.grid(iso3c = iso, tech = "Solar", supply_chain = "Deployment", Year = 2018:2025, stringsAsFactors = FALSE)
  im$variable <- "Annual Investment (USD bn, 2024$)"; im$data_type <- "raw"; im$value <- im$Year - 2016
  cm <- data.frame(iso3c = iso, mineral = "Lithium", supply_chain = "Refining", exports = 1e8 * seq_along(iso),
                   market_share = seq_along(iso) / 120, Year = 2024)
  mp <- data.frame(iso3c = iso, variable = "Lithium Production", data_type = "raw", value = seq_along(iso), Year = 2025,
                   source = "EI", explanation = "Lithium production in 2025 (Thousand tonnes) from sheet 'Lithium P-R'")
  rs <- mp; rs$variable <- "Lithium Reserves"
  lc <- data.frame(iso3c = iso, tech = "Solar", variable = "lcoe_25", data_type = "raw", value = 40 + seq_along(iso), Year = 2025)
  co <- data.frame(iso3 = iso, country = iso, tier = "core", bloc = ifelse(iso == "CHN", "China-leaning", "U.S.-leaning"), gdp_usd = 1e12)
  list(trade_concentration = tc, import_partner_cells = pc, import_exposure = ie, investment_momentum = im,
       critical_minerals_trade_flows = cm, critical_minerals_production = mp, reserves = rs, lcoe_competitiveness = lc, coalition = co)
}

#' Latest-12-month cells for two monthly reporters (USA imports and exports, JPN imports).
cod_fixture_current <- function() {
  cells <- rbind(
    data.frame(reporter = "USA", partner = c("CHN", "MEX", "KOR"), flow = "import", tech = "Batteries", stage = "Assembly", usd = c(1e8, 6e8, 3e8)),
    data.frame(reporter = "USA", partner = c("MEX", "CAN"), flow = "export", tech = "Batteries", stage = "Assembly", usd = c(4e8, 1e8)),
    data.frame(reporter = "JPN", partner = c("CHN", "KOR"), flow = "import", tech = "Batteries", stage = "Assembly", usd = c(9e8, 1e8)))
  list(cells = cells, windows = data.frame(reporter = c("USA", "USA", "JPN"), flow = c("import", "export", "import"),
                                           window_end = c(202608L, 202608L, 202512L),
                                           period = c("12 months to Aug 2026", "12 months to Aug 2026", "12 months to Dec 2025")))
}

test_that("labels read as plain English", {
  expect_equal(cod_cell_phrase("Batteries", "Refining"), "processed materials for batteries")
  expect_equal(cod_cell_phrase("Batteries", "Assembly"), "battery cells and packs")
  expect_equal(cod_ordinal(c(1, 2, 3, 11, 12, 13, 21, 22)), c("1st", "2nd", "3rd", "11th", "12th", "13th", "21st", "22nd"))
  expect_equal(cod_usd(c(2.13e9, 4.5e8, -1.2e9)), c("$2.1bn", "$450m", "-$1.2bn"))
  expect_equal(cod_month_label(202608), "Aug 2026")
})

test_that("the draw is deterministic, never picks China as the focus, and respects cooldowns", {
  d <- cod_prepare(cod_fixture_raw()); cfg <- cod_cfg()
  cand <- cod_candidates(d, cfg)
  expect_true(all(c("suppliers", "customers", "leaderboard", "stage_balance", "investment_trend") %in% cand$template))
  w <- cod_weights(cand, d, cfg)
  expect_true(all(w[cand$iso3 == "CHN"] == 0))
  a <- cod_pick(cand, w, "2026-11-18"); b <- cod_pick(cand, w, "2026-11-18")
  expect_identical(a$key, b$key)
  hist <- data.frame(date = "2026-11-17", key = a$key, template = a$template, iso3 = a$iso3, tech = a$tech)
  w2 <- cod_weights(cand, d, cfg, hist, "2026-11-18")
  expect_equal(w2[cand$key == a$key], 0)
})

test_that("country weights act as odds, whatever the number of candidates a country has", {
  d <- cod_prepare(cod_fixture_raw()); cfg <- cod_cfg()
  cand <- cod_candidates(d, cfg)
  w <- cod_weights(cand, d, cfg)
  s <- cand$template == "suppliers"
  by <- tapply(w[s], cand$iso3[s], sum)
  named <- unlist(cfg$countries$named)
  expect_equal(unname(by["USA"] / by["JPN"]), unname(named["USA"] / named["JPN"]), tolerance = 1e-9)
})

test_that("a run of days builds specs that never repeat a chart and carry raw values only", {
  d <- cod_prepare(cod_fixture_raw()); cfg <- cod_cfg()
  specs <- cod_run(d, cfg, seq(as.Date("2026-11-18"), by = "day", length.out = 12))
  keys <- vapply(specs, `[[`, "", "key")
  expect_equal(length(unique(keys)), length(keys))
  for (s in specs) {
    expect_true(nchar(s$title) > 20)
    expect_false(grepl("NA|index|score", s$title))
    expect_true(s$chart$type %in% c("bar", "grouped_bar", "lines", "scatter", "tiles", "dumbbell", "stacked_bar"))
    dw <- cod_dw_spec(s)
    expect_true(is.data.frame(dw$data) && nrow(dw$data) > 0)
  }
})

test_that("latest-12-month trade maps HS6 to cells once per cell and labels the window", {
  master <- data.frame(tech = c("Batteries", "Batteries", "Solar"), stage = c("Assembly", "Assembly", "Components"),
                       hs6 = c(850760, 850760, 850760))
  dyads <- data.frame(reporter_iso = "USA", partner_iso = c("CHN", "MEX", "USA"), flow_direction = "import",
                      cmd_code = "850760", primary_value = c(30, 70, 5), window_end = 202608L)
  cc <- cod_current_cells(dyads, master)
  expect_equal(sum(cc$cells$usd[cc$cells$tech == "Batteries"]), 100)  # duplicate crosswalk row not double-counted; self-trade dropped
  expect_equal(sum(cc$cells$tech == "Solar"), 2)                        # a code in two technologies counts in each
  expect_equal(cc$windows$period, "12 months to Aug 2026")
})

test_that("a country's own trade uses its latest 12 months when it reports monthly, 2024 otherwise", {
  d <- cod_prepare(cod_fixture_raw(), cod_fixture_current()); cfg <- cod_cfg()
  a <- cod_partner_shares(d, "USA", "Batteries", "Assembly", "suppliers")
  expect_equal(attr(a, "period"), "12 months to Aug 2026")
  expect_equal(a$other[1], "MEX")
  expect_equal(attr(cod_partner_shares(d, "KOR", "Batteries", "Assembly", "suppliers"), "period"), "2024")
  e <- cod_partner_shares(d, "USA", "Batteries", "Assembly", "customers")
  expect_equal(attr(e, "basis"), "own")
  expect_equal(e$share[e$other == "MEX"], 0.8)
  expect_equal(attr(cod_partner_shares(d, "KOR", "Batteries", "Assembly", "customers"), "basis"), "mirror")
  cand <- cod_cand_suppliers(d, cfg)
  expect_true(all(cand$current[cand$iso3 == "USA"]))
  expect_false(any(cand$current[cand$iso3 == "KOR"]))
  # China's share of JPN battery cells: 5% in the 2024 fixture, 90% in the latest 12 months
  shift <- cod_china_shift_table(d, cfg)
  expect_equal(shift$change[shift$iso3 == "JPN"], 0.85)
})

test_that("non-trade sources: EI keeps countries plus WLD, policy counts each measure once, FDI drops cancelled", {
  ei <- data.frame(ISO3166_alpha3 = c("USA", "T-EU", "WLD", "O-AP", "JPN"), Year = 2025, Var = "solar_twh", Value = c(400, 300, 2800, 50, 100))
  expect_setequal(cod_ei_rows(ei)$iso3, c("USA", "WLD", "JPN"))
  measures <- data.frame(intervention_id = c("a", "b", "c"), iso3 = "USA", jurisdiction_name = "United States",
                         family = c("grant", "tariff", "grant"), support = c(1L, 0L, 1L),
                         announcement_date = as.Date(c("2024-01-01", "2023-05-01", "2019-01-01")),
                         counted = TRUE, is_sanction = c(FALSE, FALSE, TRUE))
  links <- data.frame(intervention_id = c("a", "a", "b", "c"), tech = c("Batteries", "Batteries", "Batteries", "Solar"))
  expect_equal(nrow(cod_nipo_rows(measures, links)), 2)  # duplicate link collapsed; sanction dropped
  fdi <- data.frame(project_id = 1:3, sector = "Battery", home_iso3 = c("KOR", "KOR", "USA"), destination_iso3 = "USA",
                    status = c("Operational", "Cancelled", "Announced"), year_announced = 2023, capital_usd_m = 100)
  expect_equal(nrow(cod_fdi_rows(fdi)), 1)               # cancelled and domestic projects dropped
  a <- cod_atlas_cells(data.frame(country_iso3_code = c("KOR", "CHN"), product_hs92_code = 850760, year = 2024, export_value = c(25, 75)),
                       data.frame(tech = "Batteries", stage = "Assembly", hs6 = 850760))
  expect_equal(a$share[a$iso3 == "KOR"], 0.25)
})

test_that("trend peers are neighbours or competitors, and small focus countries get an indexed chart", {
  cfg <- cod_cfg()
  size <- c(HRV = 400, SVN = 300, ITA = 5000, USA = 50000, IND = 40000, MEX = 30000, HKG = 350)
  p <- cod_pick_peers("HRV", size, cfg)
  expect_true(all(c("SVN", "ITA") %in% p))       # same sub-region (Southern Europe) beats far larger, far-away markets
  expect_false("HKG" %in% p)                     # re-export hubs never count
  lead <- c(AUS = 90, CHL = 60, CHN = 60, ARG = 20, BRA = 10, ZWE = 15)
  expect_true("AUS" %in% cod_pick_peers("CHL", lead, cfg))  # a leader's peers are the other leaders
  s <- expand.grid(iso3 = c("HRV", "USA"), x = 2018:2025, stringsAsFactors = FALSE)
  s$value <- ifelse(s$iso3 == "USA", 1e10, 1e8) * (s$x - 2016)
  d <- list(countries = data.frame(iso3 = c("HRV", "USA"), country = c("Croatia", "United States")))
  tr <- cod_trend_rows(s, "HRV", "USA", 2020L, d, cfg)
  expect_true(tr$indexed)
  expect_equal(tr$rows$value[tr$rows$iso3 == "HRV" & tr$rows$x == 2020], 100)
  expect_equal(tr$rows$level[tr$rows$iso3 == "HRV" & tr$rows$x == 2020], 4e8)
})
