# Trade of the Sector Insights bundle (R/dashboard/sector/sector_trade.R) on small matrices in the
# shape partnership_strength_build_trade_cells() returns: country totals, the zero-rebuild rule,
# intra-EU removal, the reconciliation table and the HS6 crosswalk guards.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "dashboard", "sector", "sector_files.R"))
for (f in sector_files()) source(file.path(repo_root, f))

# A three-economy matrix of one cell. 2024: USA declares imports, DEU declares exports only, CHN
# declares nothing (its imports are mirror, x1.06); 2020: everyone declares imports.
tiny_matrix <- function() {
  tibble::tribble(
    ~exporter_iso, ~importer_iso, ~tech, ~supply_chain, ~value_first, ~value_last, ~source_first, ~source_last,
    "CHN", "USA", "Batteries", "Assembly", 50, 100, "importer", "importer",
    "DEU", "USA", "Batteries", "Assembly", 10, 0, "importer", "importer",
    "DEU", "CHN", "Batteries", "Assembly", 5, 21.2, "importer", "mirror",
    "USA", "CHN", "Batteries", "Assembly", 0, NA, "importer", NA,
    "CHN", "DEU", "Batteries", "Assembly", 30, NA, "importer", NA,
    "USA", "DEU", "Batteries", "Assembly", 2, NA, "importer", NA
  )
}
tiny_reporters <- function() {
  tibble::tribble(
    ~reporter_iso, ~period, ~flow_direction, ~cell,
    "USA", 2020L, "import", "batteries-assembly", "CHN", 2020L, "import", "batteries-assembly",
    "DEU", 2020L, "import", "batteries-assembly", "USA", 2024L, "import", "batteries-assembly",
    "DEU", 2024L, "export", "batteries-assembly"
  )
}

test_that("country totals are sums over the matrix, with mirror and zero states", {
  tc <- tiny_matrix()
  dec <- sector_trade_declared(tiny_reporters())
  x <- sector_trade_country(tc, dec)
  get <- function(var, geo, y) x[x$var == var & x$geo == geo & x$year == y, , drop = FALSE]
  expect_identical(get("trade.exports", "CHN", 2024)$value, 100)
  expect_identical(get("trade.exports", "DEU", 2024)$value, 21.2)
  expect_identical(get("trade.exports", "CHN", 2020)$value, 80)
  expect_identical(get("trade.imports", "USA", 2024)$status, "obs")
  expect_identical(get("trade.imports", "CHN", 2024)$status, "mirror")
  expect_identical(get("trade.imports", "CHN", 2024)$value, 21.2)
  # USA's 2024 exports: its only observed pair is 0 (USA -> DEU and USA -> CHN unobserved, USA -> CHN 2020 zero).
  expect_identical(nrow(get("trade.exports", "USA", 2024)), 0L)
  expect_identical(get("trade.exports", "USA", 2020)$value, 2)
  # Net only where both totals exist; CHN did not declare its 2024 imports, so its net is mirror.
  expect_identical(get("trade.net", "CHN", 2024)$value, 100 - 21.2)
  expect_identical(get("trade.net", "CHN", 2024)$status, "mirror")
  expect_identical(nrow(get("trade.net", "USA", 2024)), 0L)
  w <- sector_trade_world(x)
  expect_equal(w$value[w$var == "trade.exports" & w$year == 2024], 121.2)
  expect_identical(w$basis[1], "sum_all")
})

test_that("flows keep positive pairs only; zeros are rebuilt from the declared table (contract rule)", {
  tc <- tiny_matrix()
  dec <- sector_trade_declared(tiny_reporters())
  fl <- sector_trade_flows(tc)
  expect_false(any(fl$exporter == "USA" & fl$importer == "CHN"))   # 0 in 2020, unobserved in 2024
  r <- fl[fl$exporter == "DEU" & fl$importer == "USA", ]
  expect_identical(r$v2020, 10)
  expect_true(is.nan(r$v2024))
  expect_identical(r$s2024, "none")
  # The client's rule: absent pair = zero when the importer declared imports that year, or failing
  # that when the exporter declared exports (a mirror zero); otherwise unobserved.
  state <- function(e, m, y) {
    hit <- fl[fl$exporter == e & fl$importer == m, , drop = FALSE]
    v <- if (nrow(hit)) hit[[paste0("v", y)]] else NaN
    if (!is.nan(v)) return("positive")
    d <- dec[dec$year == y, , drop = FALSE]
    if (any(d$geo == m & d$imports_declared) || any(d$geo == e & d$exports_declared)) "zero" else "unobserved"
  }
  expect_identical(state("DEU", "USA", 2024), "zero")
  expect_identical(state("USA", "CHN", 2020), "zero")
  expect_identical(state("USA", "CHN", 2024), "unobserved")
  expect_identical(state("CHN", "DEU", 2024), "unobserved")
  # Mirror zero: the importer did not declare, the exporter did.
  expect_identical(state("DEU", "CHN", 2024), "positive")
  # The same rule reproduces every row of the matrix.
  zr <- sector_trade_zero_rule_check(tc, dec)
  expect_identical(zr$bad, 0L)
  tc$value_last[2] <- NA
  expect_identical(sector_trade_zero_rule_check(tc, dec)$bad, 1L)
})

test_that("the zero rule holds on the synthetic matrix (non-reporter and re-export hub patterns)", {
  tc <- sector_synthetic_trade_cells()
  dec <- sector_trade_declared(sector_synthetic_reporters(tc))
  zr <- sector_trade_zero_rule_check(tc, dec)
  expect_identical(zr$bad, 0L)
  expect_gt(zr$n, 500)
})

test_that("intra-EU flows are removed before the EU is counted as one exporter", {
  tc <- tibble::tribble(
    ~exporter_iso, ~importer_iso, ~tech, ~supply_chain, ~value_first, ~value_last, ~source_first, ~source_last,
    "DEU", "POL", "Batteries", "Assembly", 1, 4, "importer", "importer",
    "POL", "DEU", "Batteries", "Assembly", 2, 6, "importer", "importer",
    "DEU", "USA", "Batteries", "Assembly", 3, 10, "importer", "importer",
    "CHN", "DEU", "Batteries", "Assembly", 20, 50, "importer", "importer",
    "CHN", "USA", "Batteries", "Assembly", 20, 30, "importer", "importer"
  )
  intra <- sector_eu_intra(tc, c("DEU", "POL"))
  expect_identical(intra$intra[intra$year == 2024], 10)
  expect_identical(intra$intra[intra$year == 2020], 3)
  rep <- expand.grid(reporter_iso = c("DEU", "POL", "USA", "CHN"), period = c(2020L, 2024L), flow_direction = "import",
                     cell = "batteries-assembly", stringsAsFactors = FALSE)
  country <- sector_trade_country(tc, sector_trade_declared(rep))
  facts <- country[country$var == "trade.exports", ]
  geo <- tibble::tibble(code = c("CHN", "DEU", "POL", "USA", "EUU", "WLD"), kind = c("country", "country", "country", "country", "eu", "world"),
                        group = c("china", "core", "core", "core", "core", "na"), eu_member = c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE))
  vars <- tibble::tibble(id = "trade.exports", agg = "sum", world_basis = "sum_all", native_level = "country")
  s <- sector_summary(facts, sector_trade_world(country), geo, vars, intra)
  m <- s[s$year == 2024 & s$eu_mode == "members", ]
  b <- s[s$year == 2024 & s$eu_mode == "bloc", ]
  expect_identical(m$W, 100)
  expect_identical(b$W, 90)                       # W - intra
  expect_identical(b$china, 80 / 90)              # CHN unchanged, the denominator shrinks
  expect_identical(b$core, (14 + 6 - 10) / 90)    # EUU = members - intra (DEU 14, POL 6)
  expect_identical(b$n, 2L)
  expect_identical(b$growth, 90 / (46 - 3) - 1)   # growth within the bloc mode (2020 intra 3)
  expect_identical(m$growth, 100 / 46 - 1)
})

test_that("the reconciliation compares the matrix with the exporter-reported basis and warns past 3 points", {
  summary <- tibble::tibble(var = "trade.exports", item_key = c("cell:batteries-assembly", "cell:batteries-assembly", "cell:oil-refining"),
                            year = c(2020L, 2024L, 2024L), eu_mode = "members", W = c(99.3e9, 202.7e9, 127e9),
                            china = c(0.40, 0.437, 0.10), hhi = c(0.19, 0.207, 0.05))
  reported <- tibble::tibble(item_key = c("cell:batteries-assembly", "cell:oil-refining"), year = 2024L, total = c(189.6e9, 120e9),
                             china_reported = c(0.434, 0.16), hhi_reported = c(0.206, 0.06), n_reporters = c(133L, 120L))
  r <- sector_trade_reconcile(summary, reported)
  expect_identical(nrow(r), 2L)                   # 2024 only: the reported basis has no 2020
  expect_identical(r$warn[r$item_key == "cell:batteries-assembly"], FALSE)
  expect_identical(r$warn[r$item_key == "cell:oil-refining"], TRUE)
  expect_identical(r$reported_hhi[r$item_key == "cell:batteries-assembly"], 0.206)
})

test_that("the HS6 crosswalk is deduplicated and guarded, keyed on the six-stage stage column", {
  # The master's former three-stage column (supply_chain) and its six-stage `stage`: cells are keyed on
  # `stage` (schema 2); supply_chain is kept only to match the reference master's excluded rows.
  hs6 <- tibble::tribble(
    ~tech, ~supply_chain, ~stage, ~sub_sector, ~hs6, ~note,
    "Batteries", "Midstream", "Assembly", "Cells", 850760, NA,
    "Batteries", "Midstream", "Assembly", "Cells", 850760, NA,                 # duplicate row
    "Electric Grid", "Midstream", "Components", "Converters", 850440, NA,
    "Batteries", "Midstream", "Assembly", "Battery management", 850440, "kept with caveat: general-purpose converters",
    "Solar", "Midstream", "Assembly", "Inverters", 850440, NA
  )
  b <- sector_trade_basket(hs6)
  expect_identical(nrow(b), 4L)
  expect_identical(b$hs6[1], "850760")
  expect_identical(b$cell, c("batteries-assembly", "electric-grid-components", "batteries-assembly", "solar-assembly"))
  expect_identical(b$supply_chain, c("Assembly", "Components", "Assembly", "Assembly"))
  expect_identical(unique(b$listed_as), "Midstream")
  expect_identical(b$shared_n[b$hs6 == "850440"], c(2L, 2L, 2L))
  expect_identical(b$shared_n[b$hs6 == "850760"], 0L)
  codes <- sector_trade_codes(b, c(`850760` = "Lithium-ion accumulators"))
  expect_identical(codes$caveat[codes$sub_sector == "Battery management"], "kept with caveat: general-purpose converters")
  expect_identical(codes$description[codes$hs6 == "850760"], "Lithium-ion accumulators")
  # A master staged before the six stages (no `stage`), or a stage that is not one of the six, stops.
  expect_error(sector_trade_basket(hs6[setdiff(names(hs6), "stage")]), "has no `stage` column")
  bad_stage <- hs6
  bad_stage$stage[1] <- "Midstream"
  expect_error(sector_trade_basket(bad_stage), "not supply-chain stages")
  # The caveat is kept wherever "kept with caveat" sits in the note (repair review 2026-10-11).
  mid <- tibble::tibble(cell = "batteries-extraction", sub_sector = "Ores", hs6 = c("260200", "253090", "260400"),
                        shared_n = 0L, note = c("moved from Refining: ores; kept with caveat: mostly steel use",
                                                "added (basket review 2026-09); kept with caveat: mineral substances n.e.c.", NA))
  mc <- sector_trade_codes(mid)
  expect_identical(mc$caveat, c("kept with caveat: mostly steel use", "kept with caveat: mineral substances n.e.c.", NA))
  # A code in two sub-sectors of one cell stops the export.
  bad <- rbind(hs6, tibble::tibble(tech = "Batteries", supply_chain = "Midstream", stage = "Assembly", sub_sector = "Packs", hs6 = 850760, note = NA))
  expect_error(sector_trade_basket(bad), "two sub-sectors of one cell")
  # A code excluded in data/reference but still where the reference master excluded it means scripts/04
  # was not rerun; excluded elsewhere only is fine (HS 851762 is out of Electric Grid only). The excluded
  # rows carry no six-stage stage, so they match on the master's former column.
  ref <- tibble::tibble(tech = c("Electric Grid", "Batteries"), supply_chain = "Midstream", stage = "", sub_sector = c("Converters", "Cells"),
                        hs6 = c(851762, 850680), excluded = c(TRUE, TRUE), note = c("excluded: telecom equipment", "excluded: primary cells"))
  expect_identical(sector_trade_excluded_in_basket(b, ref), character())
  ref$hs6[1] <- 850440
  expect_identical(sector_trade_excluded_in_basket(b, ref), "Electric Grid Midstream 850440")
  # Excluded codes ship at the technology level (user decision C6: no stage is invented for them).
  ex <- sector_trade_excluded(ref, "Batteries")
  expect_identical(names(ex), c("tech", "sub_sector", "hs6", "reason"))
  expect_identical(ex$tech, "Batteries")
  expect_identical(ex$hs6, "850680")
  expect_identical(ex$reason, "excluded: primary cells")
  expect_identical(nrow(sector_trade_excluded(ref, c("Batteries", "Electric Grid"))), 2L)
})

test_that("basket notes are reworded for the six stages; other old stage words are reported", {
  x <- c("excluded (basket review 2026-10): bauxite: feedstock for alumina. Earlier note: moved from Midstream: ore or concentrate (extraction stage)",
         "kept with caveat: steel is the larger use. 26.8% of the Comtrade 2024 Electric Vehicles Upstream basket (Atlas 2023: 20.6%)",
         "kept with caveat: 9.1% of the Comtrade 2024 Solar Midstream basket",
         "kept with caveat: parts of electric motors (excluded from Oil Midstream, where the use is incidental)",
         "kept with caveat: monazite. Excluded from Batteries Upstream, so the code is in one basket only",
         "kept with caveat: Refined chemicals and intermediates stay in Batteries Midstream, and copper is in Electric Grid Upstream only",
         "kept with caveat: the heading also holds uranium hexafluoride from conversion, a Midstream step with no code of its own",
         "kept with caveat: silver (7106.10 is a processed paste input, Midstream if anything)",
         "kept with caveat: something Downstream")
  y <- sector_six_stage_text(x)
  expect_identical(y[1], "excluded (basket review 2026-10): bauxite: feedstock for alumina.")
  expect_match(y[2], "of the Comtrade 2024 Electric Vehicles materials basket", fixed = TRUE)
  expect_match(y[3], "of the Comtrade 2024 Solar manufacturing basket", fixed = TRUE)
  expect_match(y[4], "excluded from the Oil manufacturing basket, where", fixed = TRUE)
  expect_match(y[5], "Excluded from the Batteries materials basket, so", fixed = TRUE)
  expect_match(y[6], "are in Batteries Refining, and copper is in the Electric Grid materials basket only", fixed = TRUE)
  expect_match(y[7], "a fuel-cycle step with no code of its own", fixed = TRUE)
  expect_match(y[8], "Components if anything", fixed = TRUE)
  expect_identical(sector_has_legacy_stage(y), c(rep(FALSE, 8), TRUE))
})

test_that("the declared table flags imports and exports per cell, economy and year", {
  d <- sector_trade_declared(tiny_reporters())
  k <- function(geo, y) d[d$geo == geo & d$year == y, , drop = FALSE]
  expect_identical(k("DEU", 2024)$imports_declared, FALSE)
  expect_identical(k("DEU", 2024)$exports_declared, TRUE)
  expect_identical(k("USA", 2024)$imports_declared, TRUE)
  expect_identical(nrow(k("CHN", 2024)), 0L)
  expect_true(all(d$item_key == "cell:batteries-assembly"))
})
