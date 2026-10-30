# Trade by sub-component and export feasibility in the Sector Insights bundle (sector_data change
# 2026-10-12): R/dashboard/sector/sector_trade.R (sector_trade_sub(), sector_trade_sub_table(),
# sector_trade_sub_check(), sector_trade_sub_summary()), sector_facts.R (sector_facts_feasibility(),
# sector_feasibility_year()), the dictionary's cell caveats and HHI reference thresholds, and the
# optional chunk tables (trade.cty_sub, trade.sub_intra, trade.sub_summary, dict.cell_caveats,
# dict.hhi_refs).

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "dashboard", "sector", "sector_files.R"))
for (f in sector_files()) source(file.path(repo_root, f))
source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
`%>%` <- dplyr::`%>%`

# Batteries Assembly has three sub-components (cells: 850760; parts: 850790; chemicals: 282520) and
# Batteries Refining one (lithium: 282520, the code it shares with Assembly). The crosswalk is staged as
# scripts/87 passes it: the six-stage `stage`, copied into supply_chain for the pipeline matrix.
sub_crosswalk <- function() {
  data.frame(
    tech = "Batteries", supply_chain = c("Assembly", "Assembly", "Assembly", "Refining"), stage = c("Assembly", "Assembly", "Assembly", "Refining"),
    sub_sector = c("Cells", "Parts", "Chemicals", "Lithium"), hs6 = c("850760", "850790", "282520", "282520"),
    note = NA_character_, stringsAsFactors = FALSE
  )
}
# 2024: USA declares its imports (and CHN's report of the same flow is ignored); DEU did not declare,
# so CHN's export to DEU is mirrored (x1.06); POL declares its imports of the shared code, DEU's
# report of that flow is ignored. 2020: DEU declares one import.
sub_dyads <- function() {
  tibble::tribble(
    ~reporter_iso, ~partner_iso, ~period, ~flow_direction, ~cmd_code, ~primary_value,
    "USA", "CHN", 2024L, "import", "850760", 100,
    "USA", "CHN", 2024L, "import", "850790", 20,
    "CHN", "USA", 2024L, "export", "850760", 90,
    "CHN", "DEU", 2024L, "export", "850760", 50,
    "POL", "DEU", 2024L, "import", "282520", 10,
    "DEU", "POL", 2024L, "export", "282520", 12,
    "DEU", "CHN", 2020L, "import", "850790", 5
  )
}
sub_fixture <- function() {
  hs6 <- sub_crosswalk()
  basket <- sector_trade_basket(hs6)
  td <- sub_dyads()
  std <- partnership_strength_standardize_comtrade(td, years = SECTOR_TRADE_YEARS)
  declared <- sector_trade_declared(sector_trade_reporters(std, basket))
  tc <- partnership_strength_build_trade_cells(td, hs6, years = SECTOR_TRADE_YEARS, cif_fob_factor = 1.06)
  country <- sector_trade_country(tc, declared)
  sub <- sector_trade_sub(std, basket, declared, eu_members = c("DEU", "POL"), cif_fob_factor = 1.06)
  list(basket = basket, declared = declared, tc = tc, country = country, world = sector_trade_world(country), sub = sub)
}

test_that("sub-components follow the cell's source rule (importer's declaration, else the exporter's x1.06)", {
  x <- sub_fixture()$sub
  get <- function(cell, sub, geo, y) x$country[x$country$item_key == paste0("cell:", cell) & x$country$sub_sector == sub &
                                                 x$country$geo == geo & x$country$year == y, , drop = FALSE]
  expect_equal(get("batteries-assembly", "Cells", "CHN", 2024L)$exports, 100 + 50 * 1.06)
  expect_identical(get("batteries-assembly", "Parts", "CHN", 2024L)$exports, 20)
  expect_identical(get("batteries-assembly", "Cells", "USA", 2024L)$imports, 100)
  expect_equal(get("batteries-assembly", "Cells", "DEU", 2024L)$imports, 53)
  # POL declared the shared code: its declaration counts in both cells, DEU's report of it in neither.
  expect_identical(get("batteries-assembly", "Chemicals", "DEU", 2024L)$exports, 10)
  expect_identical(get("batteries-refining", "Lithium", "POL", 2024L)$imports, 10)
  expect_true(is.na(get("batteries-assembly", "Cells", "CHN", 2024L)$imports))
  expect_identical(get("batteries-assembly", "Parts", "CHN", 2020L)$exports, 5)
  # Intra-EU flows (DEU -> POL) per sub-component; world totals per sub-component and year.
  expect_identical(x$intra$intra[x$intra$item_key == "cell:batteries-refining" & x$intra$year == 2024L], 10)
  expect_identical(nrow(x$intra[x$intra$item_key == "cell:batteries-assembly" & x$intra$sub_sector == "Cells", ]), 0L)
  w <- x$world[x$world$item_key == "cell:batteries-assembly" & x$world$year == 2024L, ]
  expect_equal(sum(w$total), 100 + 20 + 53 + 10)
})

test_that("sub-components add up to the matrix totals of partnership_strength_build_trade_cells()", {
  fx <- sub_fixture()
  cs <- sector_trade_sub_table(fx$sub, fx$country)
  chk <- sector_trade_sub_check(cs, fx$country, fx$world)
  expect_lte(chk$max_rel, 1e-12)
  expect_identical(chk$n_unmatched, 0L)
  expect_gt(chk$n, 0L)
  # Every (cell, economy, year) total of the matrix is the sum of its sub-component rows.
  ex <- fx$country[fx$country$var == "trade.exports", ]
  s <- tapply(cs$exports[cs$geo != "WLD"], paste(cs$item_key, cs$geo, cs$year)[cs$geo != "WLD"], sum, na.rm = TRUE)
  pos <- ex$value > 0
  expect_equal(as.vector(s[paste(ex$item_key, ex$geo, ex$year)[pos]]), ex$value[pos])
})

test_that("the missing side reads 0 where the cell total was observed and NaN where not; WLD rows hold the world", {
  fx <- sub_fixture()
  cs <- sector_trade_sub_table(fx$sub, fx$country)
  row <- function(cell, sub, geo, y) cs[cs$item_key == paste0("cell:", cell) & cs$sub_sector == sub & cs$geo == geo & cs$year == y, , drop = FALSE]
  # CHN's 2024 imports of the cell were observed (DEU's declared exports to CHN: none, a mirror zero).
  expect_identical(row("batteries-assembly", "Cells", "CHN", 2024L)$imports, 0)
  # In 2020 only DEU declared, so CHN's imports were not observed at all.
  expect_true(is.nan(row("batteries-assembly", "Parts", "CHN", 2020L)$imports))
  # No row where neither side is above 0 (USA's 2024 exports are an observed zero only).
  expect_false(any(cs$geo == "USA" & cs$year == 2024L & cs$sub_sector == "Parts" & !(cs$imports > 0)))
  expect_true(all((cs$exports > 0 & !is.na(cs$exports)) | (cs$imports > 0 & !is.na(cs$imports))))
  w <- row("batteries-assembly", "Cells", "WLD", 2024L)
  expect_equal(w$exports, 153)
  expect_identical(w$exports, w$imports)
  expect_identical(order(cs$item_key, cs$sub_sector, cs$geo, cs$year, method = "radix"), seq_len(nrow(cs)))
})

test_that("the sum check catches a sub-component that does not add up or has no cell total", {
  fx <- sub_fixture()
  cs <- sector_trade_sub_table(fx$sub, fx$country)
  bad <- cs
  i <- which(bad$geo == "CHN" & bad$sub_sector == "Cells" & bad$year == 2024L)
  bad$exports[i] <- bad$exports[i] * (1 + 1e-6)
  chk <- sector_trade_sub_check(bad, fx$country, fx$world)
  expect_gt(chk$max_rel, 1e-9)
  expect_match(paste(chk$examples, collapse = " "), "exports cell:batteries-assembly CHN 2024")
  extra <- dplyr::bind_rows(cs, tibble::tibble(item_key = "cell:batteries-assembly", sub_sector = "Cells", geo = "JPN", year = 2024L,
                                               exports = 1, imports = NaN))
  chk2 <- sector_trade_sub_check(extra, fx$country, fx$world)
  expect_identical(chk2$n_unmatched, 1L)
})

test_that("the per-sub-component summaries follow the meta.summary rules and add up to the cell's world total", {
  dict <- sector_dictionary_tables(yaml::read_yaml(file.path(repo_root, "config", "sector_dictionary.yml")), fill = sector_synthetic_fill())
  parts <- sector_synthetic_parts(dict)
  have <- unique(sector_data_keys(parts, parts$geo)$var)
  expect_true(all(c("trade.sub_exports", "trade.sub_imports", "trade.feasibility") %in% have))
  b <- sector_assemble(parts, dict, ship_vars = dict$vars$id[dict$vars$id %in% have])
  ss <- b$trade$batteries$sub_summary
  expect_false(is.null(ss))
  expect_setequal(unique(ss$var), c("trade.sub_exports", "trade.sub_imports"))
  expect_setequal(unique(ss$eu_mode), c("members", "bloc"))
  cs <- b$trade$batteries$cty_sub
  # One summary row per (cell, sub-component, measure, year, mode); W = the WLD row.
  expect_identical(anyDuplicated(ss[c("item_key", "sub_sector", "var", "year", "eu_mode")]), 0L)
  m <- ss[ss$var == "trade.sub_exports" & ss$eu_mode == "members", ]
  wld <- cs[cs$geo == "WLD", ]
  expect_equal(m$W, wld$exports[match(paste(m$item_key, m$sub_sector, m$year), paste(wld$item_key, wld$sub_sector, wld$year))])
  # Sub-component world totals add up to the cell's (meta.summary W of trade.exports).
  cell <- b$summary[b$summary$var == "trade.exports" & b$summary$eu_mode == "members" & b$summary$item_key %in% unique(m$item_key), ]
  sw <- tapply(m$W, paste(m$item_key, m$year), sum)
  expect_equal(as.vector(sw[paste(cell$item_key, cell$year)]), cell$W)
  # HHI and the effective number on a sub-component: 1 / HHI.
  ok <- is.finite(m$hhi)
  expect_equal(m$eff_n[ok], 1 / m$hhi[ok])
  expect_true(all(m$top3_share[ok] >= m$top1_share[ok]))
  # Growth since 2020 on the 2024 rows.
  expect_true(all(is.na(m$growth[m$year == 2020L])))
})

test_that("export feasibility ships the processed Atlas value verbatim with the Atlas data year", {
  ef <- tibble::tibble(
    iso3c = c("CHN", "DEU", "MLT", "CHN", "CHN"), tech = c("Batteries", "Batteries", "Batteries", "Solar", "Batteries"),
    supply_chain = c("Assembly", "Assembly", "Assembly", "Refining", "Assembly"),
    variable = c("feasibility", "feasibility", "feasibility", "feasibility", "feas"),
    data_type = c("raw", "raw", "raw", "raw", "index"), value = c(0.5132, 0.41, NA, 0.162, 0.97),
    Year = 2024L, explanation = c(rep("1 - mean Atlas product-space distance over the basket's HS4 headings [2023]", 4), "S-curve")
  )
  y <- sector_feasibility_year(ef)
  expect_identical(y, 2023L)
  x <- sector_facts_feasibility(ef, y)
  expect_identical(x$facts$var, rep("trade.feasibility", 3))
  expect_identical(x$facts$item_key, c("cell:batteries-assembly", "cell:batteries-assembly", "cell:solar-refining"))
  expect_identical(x$facts$value, c(0.5132, 0.41, 0.162))
  expect_identical(unique(x$facts$year), 2023L)
  expect_identical(x$ledger$prov, rep("P", 3))
  expect_identical(x$excluded$n[x$excluded$reason == "NA"], 1L)
  two <- ef
  two$explanation[2] <- "1 - mean Atlas product-space distance over the basket's HS4 headings [2022]"
  expect_error(sector_feasibility_year(two), "expected one bracketed data year")
  none <- ef
  none$explanation <- "no year"
  expect_error(sector_feasibility_year(none), "found none")
})

test_that("Atlas market share ships at the Atlas data year its explanation names", {
  tc <- tibble::tibble(
    iso3c = c("CHN", "DEU", "CHN"), tech = "Batteries", supply_chain = "Midstream",
    variable = "market_share", data_type = c("raw", "raw", "index"), value = c(0.41, 0, 0.97), Year = 2024L,
    explanation = c(rep("Share of world exports of the basket's HS6 codes (Atlas) [2024]", 2),
                    "S-curve of basket market share across countries within the cell [2024]")
  )
  y <- sector_atlas_share_year(tc)
  expect_identical(y, 2024L)
  x <- sector_facts_atlas(tc, y)
  expect_identical(unique(x$facts$year), 2024L)
  expect_identical(x$world$year, 2024L)
  two <- tc
  two$explanation[2] <- "Share of world exports of the basket's HS6 codes (Atlas) [2023]"
  expect_error(sector_atlas_share_year(two), "trade_concentration_tbl market_share: expected one bracketed data year")
})

test_that("the dictionary carries the feasibility, sub-component and HHI explainer entries", {
  d <- yaml::read_yaml(file.path(repo_root, "config", "sector_dictionary.yml"))
  v <- function(id) Filter(function(x) identical(x$id, id), d$vars)[[1]]
  fe <- v("trade.feasibility")
  expect_identical(fe$unit, "index01")
  expect_identical(fe$parity, list(table = "export_feasibility_tbl", variable = "feasibility"))
  expect_true(all(c("feasibility_reading", "atlas_source") %in% unlist(fe$caveats)))
  expect_identical(v("trade.sub_exports")$prov, "X")
  expect_true("sub_component_rule" %in% unlist(v("trade.sub_imports")$caveats))
  cav <- vapply(d$caveats, `[[`, "", "id")
  expect_true(all(c("hhi_reading", "hhi_refs_context", "feasibility_reading", "sub_component_rule") %in% cav))
  # The mineral and materials baskets of the 2026-10 review, at their six stages, ship exports as their
  # primary trade measure, except the fuel and uranium Extraction cells (production stays primary), and
  # each names its basket caveat.
  pf <- unlist(v("trade.exports")$primary_for)
  expect_true(all(c("electric-vehicles-extraction", "electric-vehicles-refining", "solar-refining", "wind-extraction", "wind-refining",
                    "semiconductors-refining", "batteries-recovery") %in% pf))
  expect_false(any(c("nuclear-extraction", "oil-extraction", "gas-extraction", "coal-extraction") %in% pf))
  expect_identical(unlist(v("production.uranium")$primary_for), "nuclear-extraction")
  cells <- stats::setNames(lapply(d$cells, function(c) unlist(c$caveats)), vapply(d$cells, `[[`, "", "slug"))
  for (cl in c("electric-vehicles-extraction", "electric-vehicles-refining", "electric-vehicles-components", "solar-refining", "wind-extraction",
               "wind-refining", "wind-components", "nuclear-extraction", "nuclear-refining", "semiconductors-refining")) {
    expect_true(length(cells[[cl]]) >= 1, info = cl)
  }
  # CM-FDI attribution names the changes (CC BY 4.0).
  src <- Filter(function(s) identical(s$id, "cmfdi"), d$sources)
  if (length(src)) expect_match(src[[1]]$attribution, "Changes: project counts and sums of disclosed capital computed by the OSI authors")
  # Competition-authority thresholds: on the 0-1 scale, points = value x 10,000.
  hr <- d$hhi_refs
  expect_gte(length(hr), 3L)
  for (h in hr) expect_equal(h$points, h$value * 10000, info = h$id)
})

test_that("the validator checks cell caveats and the HHI thresholds", {
  base <- yaml::read_yaml(file.path(repo_root, "config", "sector_dictionary.yml"))
  probs <- function(d) paste(sector_dictionary_validate(d), collapse = "\n")
  d <- base
  d$cells[[1]]$caveats <- list("no_such_caveat")
  expect_match(probs(d), "caveats not in caveats: no_such_caveat")
  d <- base
  d$hhi_refs[[1]]$points <- 999L
  expect_match(probs(d), "points must be value x 10,000")
  d <- base
  d$hhi_refs[[1]]$side <- "inside"
  expect_match(probs(d), "side must be above or below")
  d <- base
  d$hhi_refs[[2]]$id <- d$hhi_refs[[1]]$id
  expect_match(probs(d), "hhi_refs: duplicate id")
  d <- base
  d$hhi_refs[[1]]$value <- 1.5
  expect_match(probs(d), "value must be in \\(0, 1\\)")
})

test_that("the new optional tables round-trip, pass the contract checks and the checks catch breaks", {
  dict <- sector_dictionary_tables(yaml::read_yaml(file.path(repo_root, "config", "sector_dictionary.yml")), fill = sector_synthetic_fill())
  parts <- sector_synthetic_parts(dict)
  have <- unique(sector_data_keys(parts, parts$geo)$var)
  b <- sector_assemble(parts, dict, ship_vars = dict$vars$id[dict$vars$id %in% have])
  ch <- sector_chunks(b)
  meta <- dash_osb_decode(dash_osb_encode(ch$`sector-meta`, "m", "chunk"))$objects
  expect_true(nrow(meta$dict$cell_caveats) >= 6)
  expect_true(all(meta$dict$cell_caveats$caveat %in% meta$dict$caveats$id))
  expect_true(nrow(meta$dict$hhi_refs) >= 3)
  sizes <- list(geo = nrow(meta$dims$geo), items = nrow(meta$dims$items), vars = nrow(meta$dict$vars))
  tr <- dash_osb_decode(dash_osb_encode(ch$`sector-trade-batteries`, "t", "chunk"))$objects
  expect_true(all(c("cty_sub", "sub_intra", "sub_summary") %in% names(tr$trade)))
  expect_identical(sector_check_chunk("sector-trade-batteries", tr, sizes), character())
  # World rows reference dims.geo WLD; items are 0-based dims.items rows of trade cells.
  wld <- which(meta$dims$geo$code == "WLD") - 1L
  expect_true(any(tr$trade$cty_sub$geo == wld))
  bad <- tr
  bad$trade$cty_sub$exports[1] <- -1
  expect_match(paste(sector_check_chunk("sector-trade-batteries", bad, sizes), collapse = "\n"), "negative values")
  bad <- tr
  bad$trade$cty_sub$sub_sector[1] <- "Not a sub-sector"
  expect_match(paste(sector_check_chunk("sector-trade-batteries", bad, sizes), collapse = "\n"), "not a trade.codes sub-sector")
  bad <- tr
  bad$trade$cty_sub$exports[1] <- 0
  bad$trade$cty_sub$imports[1] <- NaN
  expect_match(paste(sector_check_chunk("sector-trade-batteries", bad, sizes), collapse = "\n"), "neither exports nor imports above 0")
  # The feasibility facts ship in facts with the index01 unit.
  facts <- dash_osb_decode(dash_osb_encode(ch$`sector-facts`, "f", "chunk"))$objects
  vid <- which(meta$dict$vars$id == "trade.feasibility") - 1L
  expect_true(any(facts$facts$obs$var == vid))
  expect_identical(meta$dict$vars$unit[vid + 1L], "index01")
})
