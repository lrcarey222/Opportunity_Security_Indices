# Guards the defects the two trade themes carried before this test existed:
#   1. trade_concentration (ES) and export_feasibility (EO) were the same table, so the ES
#      "Trade Risk" category was an exporter-strength score and deficit_gdp never existed
#   2. export size and Atlas feasibility were ranked within each country across its own
#      technology cells, not across countries within a cell
#   3. an HS4 heading counted once per child HS6 code in the feasibility mean
#   4. countries without Comtrade rows lost their observed Atlas variables and came through
#      as rows with tech = NA
# and pins the six-stage cells (2026-10-26): supply_chain is the crosswalk's Stage, so an old
# Upstream or Midstream basket splits into the stages of its own codes.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "levels.R"))
source(file.path(repo_root, "R", "utils", "supply_chain_stages.R"))
source(file.path(repo_root, "R", "utils", "hs6_crosswalk.R"))
source(file.path(repo_root, "R", "categories", "shared", "overall_index.R"))
source(file.path(repo_root, "R", "categories", "trade", "trade_core.R"))
source(file.path(repo_root, "R", "categories", "trade", "trade_concentration.R"))
source(file.path(repo_root, "R", "categories", "trade", "export_feasibility.R"))

# --- fixtures ---------------------------------------------------------------------------

# Shaped like hs6_view_bolstered(): the three-stage "Value Chain" and the six-stage Stage.
# The old Solar Midstream basket splits into Refining (polysilicon), Components (cells) and
# Assembly (modules and two converters); the old Oil Upstream basket into Extraction (crude),
# Refining (fuel oil) and Recovery (waste oil). Solar Assembly has two codes under the 8504
# heading and one under 8541, so a per-HS6 join would weigh 8504 twice. Solar Assembly and Oil
# Extraction carry the values the whole Solar Midstream and Oil Upstream cells had before.
tr_subcat <- function() {
  data.frame(
    Technology = c("Solar", "Solar", "Solar", "Solar", "Solar", "Oil", "Oil", "Oil"),
    `Value Chain` = c(rep("Midstream", 5), rep("Upstream", 3)),
    Stage = c("Assembly", "Assembly", "Assembly", "Components", "Refining", "Extraction", "Refining", "Recovery"),
    Sub.Sector = "x",
    HS6 = c("854143", "850431", "850440", "854142", "280461", "270900", "271019", "271099"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# CHN is the big solar exporter; SMA is a small economy whose only exports are solar goods
# (the within-country ranking used to score it 1 on export size); USA runs a large solar
# deficit; NOR exports oil and polysilicon and reports only to the Atlas, not to Comtrade.
tr_countries <- function() {
  data.frame(
    country = c("China", "Smallland", "United States", "Norway"),
    iso3c = c("CHN", "SMA", "USA", "NOR"),
    stringsAsFactors = FALSE
  )
}

tr_atlas6 <- function() {
  iso <- c("CHN", "SMA", "USA", "NOR")
  # Exports and imports by code, in `iso` order.
  solar <- list(exports = c(100, 2, 10, 0), imports = c(20, 1, 90, 5))
  flows <- list(
    "854143" = solar,
    "850431" = solar,
    "850440" = solar,
    "854142" = list(exports = c(40, 0, 5, 0), imports = c(5, 0, 30, 2)),
    "280461" = list(exports = c(50, 0, 30, 20), imports = c(60, 0, 5, 0)),
    "270900" = list(exports = c(0, 0, 20, 80), imports = c(90, 1, 30, 0)),
    "271019" = list(exports = c(10, 0, 60, 5), imports = c(30, 0, 10, 2)),
    "271099" = list(exports = c(1, 0, 2, 0), imports = c(0, 0, 1, 1))
  )
  out <- do.call(rbind, lapply(names(flows), function(code) {
    data.frame(
      country_iso3_code = iso,
      product_hs92_code = code,
      export_value = flows[[code]]$exports,
      import_value = flows[[code]]$imports,
      stringsAsFactors = FALSE
    )
  }))
  world <- tapply(out$export_value, out$product_hs92_code, sum)
  out$year <- 2023L
  out$global_market_share <- unname(out$export_value / world[out$product_hs92_code])
  out
}

# Distance by country and heading: 8541 near for CHN, far for SMA; 8504 the reverse.
tr_atlas4 <- function() {
  grid <- expand.grid(
    country_iso3_code = c("CHN", "SMA", "USA", "NOR"),
    product_hs92_code = c("8541", "8504", "2804", "2709", "2710"),
    stringsAsFactors = FALSE
  )
  d <- c(
    "CHN.8541" = 0.1, "CHN.8504" = 0.5, "CHN.2804" = 0.2, "CHN.2709" = 0.9, "CHN.2710" = 0.3,
    "SMA.8541" = 0.9, "SMA.8504" = 0.1, "SMA.2804" = 0.9, "SMA.2709" = 0.9, "SMA.2710" = 0.9,
    "USA.8541" = 0.5, "USA.8504" = 0.5, "USA.2804" = 0.4, "USA.2709" = 0.3, "USA.2710" = 0.2,
    "NOR.8541" = 0.8, "NOR.8504" = 0.8, "NOR.2804" = 0.3, "NOR.2709" = 0.1, "NOR.2710" = 0.2
  )
  data.frame(
    grid,
    year = 2023L,
    distance = unname(d[paste(grid$country_iso3_code, grid$product_hs92_code, sep = ".")]),
    stringsAsFactors = FALSE
  )
}

tr_comtrade <- function() {
  atlas <- tr_atlas6()
  atlas <- atlas[atlas$country_iso3_code != "NOR", ]
  rbind(
    data.frame(ref_year = 2024L, reporter_iso = atlas$country_iso3_code, partner_desc = "World",
               cmd_code = atlas$product_hs92_code, flow_direction = "Export",
               primary_value = atlas$export_value, stringsAsFactors = FALSE),
    data.frame(ref_year = 2024L, reporter_iso = atlas$country_iso3_code, partner_desc = "World",
               cmd_code = atlas$product_hs92_code, flow_direction = "Import",
               primary_value = atlas$import_value, stringsAsFactors = FALSE)
  )
}

tr_total_export <- function() {
  data.frame(
    ref_year = 2024L,
    reporter_iso = c("CHN", "SMA", "USA"),
    primary_value = c(3000, 7, 2000),
    stringsAsFactors = FALSE
  )
}

tr_gdp <- function() {
  data.frame(
    iso3c = c("CHN", "SMA", "USA", "NOR", "NOR"),
    year = c(2023L, 2023L, 2023L, 2022L, 2025L),
    NY.GDP.MKTP.CD = c(18000, 10, 27000, 500, 99999),
    stringsAsFactors = FALSE
  )
}

tr_build <- function(fun) {
  if (identical(fun, "trade_concentration")) {
    trade_concentration(
      subcat = tr_subcat(), aec_4_data = tr_atlas4(), aec_6_data = tr_atlas6(),
      comtrade_trade = tr_comtrade(), comtrade_total_export = tr_total_export(),
      country_info = tr_countries(), year_4 = 2023, year_6 = 2023, year_comtrade = 2024,
      gdp_data = tr_gdp()
    )
  } else {
    export_feasibility(
      aec_4_data = tr_atlas4(), aec_6_data = tr_atlas6(), subcat = tr_subcat(),
      country_info = tr_countries(), gdp_data = tr_gdp(),
      comtrade_trade = tr_comtrade(), comtrade_total_export = tr_total_export(),
      year_4 = 2023, year_6 = 2023, year_comtrade = 2024
    )
  }
}

# One cell's value. `stage` defaults to the cell that carries the old fixture's values (Solar
# Assembly, Oil Extraction); two matches stop, so a test cannot read another stage's row.
TR_HOME_STAGE <- c(Solar = "Assembly", Oil = "Extraction")
tr_value <- function(tbl, iso, tech, variable, data_type = "index", stage = TR_HOME_STAGE[[tech]]) {
  v <- tbl$value[tbl$iso3c == iso & tbl$tech == tech & tbl$supply_chain == stage &
    tbl$variable == variable & tbl$data_type == data_type]
  if (length(v) > 1) stop("tr_value(): ", length(v), " rows for ", iso, " ", tech, " ", stage, " ", variable)
  if (length(v) == 0) NA_real_ else v
}

# --- tests ------------------------------------------------------------------------------

test_that("the ES and EO trade themes emit their own components", {
  # The overall rows are only appended when an index definition is loaded.
  withr::local_options(list(
    opportunity_security.index_definition =
      yaml::read_yaml(file.path(repo_root, "config", "index_definition.yml"))
  ))
  es <- tr_build("trade_concentration")
  eo <- tr_build("export_feasibility")

  # Components only: apply_overall_definitions() appends every overall whose components are
  # present, and the pillar recomputes its score variable from these.
  components <- function(tbl) {
    sort(unique(tbl$variable[tbl$data_type == "index" & !startsWith(tbl$variable, "Overall ")]))
  }
  expect_setequal(components(es), c("HHI", "market_share", "deficit_gdp"))
  expect_setequal(components(eo), c("HHI", "market_share", "rca", "export_size", "feas"))
  expect_false(isTRUE(all.equal(es, eo)))

  # The ES score variable is the mean of its three components.
  usa <- function(v) tr_value(es, "USA", "Solar", v)
  expect_equal(
    usa("Overall Trade Risk Index"),
    mean(c(usa("HHI"), usa("market_share"), usa("deficit_gdp")))
  )
})

test_that("deficit_gdp is net imports over GDP and a surplus ranks as more secure", {
  es <- tr_build("trade_concentration")

  # USA solar: imports 3 x 90, exports 3 x 10, GDP 27000.
  expect_equal(tr_value(es, "USA", "Solar", "deficit_gdp", "raw"), (270 - 30) / 27000)
  expect_equal(tr_value(es, "CHN", "Solar", "deficit_gdp", "raw"), (60 - 300) / 18000)

  # China runs the surplus, the United States the deficit.
  expect_gt(tr_value(es, "CHN", "Solar", "deficit_gdp"), tr_value(es, "USA", "Solar", "deficit_gdp"))

  # GDP falls back to the latest year at or before the trade year, never a later one.
  expect_equal(tr_value(es, "NOR", "Oil", "gdp", "raw"), 500)
})

test_that("export size and feasibility are ranked across countries within a cell", {
  eo <- tr_build("export_feasibility")

  # Smallland's only exports are solar goods; ranked within the country it scored 1.
  expect_lt(tr_value(eo, "SMA", "Solar", "export_size"), tr_value(eo, "CHN", "Solar", "export_size"))
  expect_equal(tr_value(eo, "CHN", "Solar", "export_size"), 1)

  # Feasibility: China is closest to the basket, Smallland furthest on the heading average.
  expect_equal(tr_value(eo, "CHN", "Solar", "feas"), 1)
})

test_that("each HS4 heading counts once in the feasibility mean", {
  codes <- trade_core_build_energy_codes(tr_subcat())
  feas <- trade_core_build_aec_feasibility(tr_atlas4(), codes, year = 2023)

  # Smallland, Solar Assembly: 8541 distance 0.9 (one child code), 8504 distance 0.1 (two
  # child codes). Per heading: 1 - (0.9 + 0.1) / 2 = 0.5. Per child code it was
  # 1 - (0.9 + 0.1 * 2) / 3.
  sma <- feas$feasibility[feas$country_iso3_code == "SMA" & feas$tech == "Solar" & feas$supply_chain == "Assembly"]
  expect_equal(sma, 0.5)
  # 8541 is also the Components heading (cells): it counts in both cells, once in each.
  sma_cells <- feas$feasibility[feas$country_iso3_code == "SMA" & feas$tech == "Solar" & feas$supply_chain == "Components"]
  expect_equal(sma_cells, 1 - 0.9)
})

test_that("Atlas-only economies keep their Atlas variables and no row has tech NA", {
  es <- tr_build("trade_concentration")
  eo <- tr_build("export_feasibility")

  expect_false(any(is.na(es$tech)))
  expect_false(any(is.na(eo$tech)))

  # Norway reports no Comtrade flows but is in the Atlas.
  expect_false(is.na(tr_value(es, "NOR", "Oil", "market_share")))
  expect_false(is.na(tr_value(es, "NOR", "Oil", "deficit_gdp")))
  expect_true(is.na(tr_value(eo, "NOR", "Oil", "rca")))
})

test_that("HHI is one value per cell, the same for every country", {
  es <- tr_build("trade_concentration")
  hhi <- es[es$variable == "HHI" & es$data_type == "index", ]
  per_cell <- tapply(hhi$value, paste(hhi$tech, hhi$supply_chain), function(x) length(unique(x)))
  expect_length(per_cell, 6)
  expect_true(all(per_cell == 1))
})

test_that("the RCA world aggregates use the reporters present in both extracts", {
  codes <- trade_core_build_energy_codes(tr_subcat())
  trade <- trade_core_build_comtrade_trade(tr_comtrade(), codes, year = 2024)
  total <- rbind(tr_total_export(), data.frame(ref_year = 2024L, reporter_iso = "XXX", primary_value = 1e6))

  rca <- trade_core_build_comtrade_rca(trade, total, year = 2024)
  chn <- rca[rca$reporter_iso == "CHN" & rca$tech == "Solar" & rca$supply_chain == "Assembly", ]
  # World total over CHN + SMA + USA only; XXX reports no basket flows.
  solar_assembly <- trade$tech == "Solar" & trade$supply_chain == "Assembly"
  expect_equal(chn$world_share, sum(trade$exports[solar_assembly]) / 5007)
  expect_true("imports" %in% names(trade))
})

test_that("codes outside country_info take no rank", {
  # The Atlas lists "areas not specified" (ANS) and uninhabited territories. Give ANS the
  # largest solar market share: it must not push China off the top of the ranking.
  a6 <- tr_atlas6()
  ans <- a6[a6$country_iso3_code == "CHN", ]
  ans$country_iso3_code <- "ANS"
  ans$export_value <- 10 * ans$export_value
  eo <- export_feasibility(
    aec_4_data = tr_atlas4(), aec_6_data = rbind(a6, ans), subcat = tr_subcat(),
    country_info = tr_countries(), comtrade_trade = tr_comtrade(),
    comtrade_total_export = tr_total_export(), year_4 = 2023, year_6 = 2023, year_comtrade = 2024
  )

  expect_false("ANS" %in% eo$iso3c)
  expect_equal(tr_value(eo, "CHN", "Solar", "market_share"), 1)
})

test_that("market share is the country's share of world exports of the basket", {
  a6 <- tr_atlas6()
  # A niche exporter: Smallland ships all of the world's 850440. Its product share is 1, so
  # the old own-export-weighted mean of shares put it near 1 for the whole solar basket.
  extra <- data.frame(
    country_iso3_code = c("CHN", "SMA", "USA", "NOR"), product_hs92_code = "850440",
    year = 2023L, export_value = c(0, 50, 0, 0), import_value = 0, global_market_share = c(0, 1, 0, 0)
  )
  a6 <- rbind(a6[a6$product_hs92_code != "850440", ], extra)
  codes <- trade_core_build_energy_codes(tr_subcat())
  ms <- trade_core_build_aec_market_share(a6, codes, year = 2023)
  solar <- ms[ms$tech == "Solar" & ms$supply_chain == "Assembly", ]
  share <- function(iso) solar$market_share[solar$country_iso3_code == iso]

  # World Solar Assembly basket: 854143 and 850431 (112 each) + 850440 (50) = 274.
  expect_equal(share("CHN"), 200 / 274)
  expect_equal(share("SMA"), (4 + 50) / 274)
  # No basket exports is a zero share, not a missing one.
  expect_equal(share("NOR"), 0)
  expect_gt(share("CHN"), share("SMA"))
})

# --- six-stage cells (2026-10-26) ----------------------------------------------------------

test_that("the trade cells are the crosswalk's six stages, split from the basket's own codes", {
  codes <- trade_core_build_energy_codes(tr_subcat())
  expect_setequal(
    unique(paste(codes$tech, codes$supply_chain)),
    c("Solar Refining", "Solar Components", "Solar Assembly", "Oil Extraction", "Oil Refining", "Oil Recovery")
  )

  es <- tr_build("trade_concentration")
  eo <- tr_build("export_feasibility")
  for (tbl in list(es, eo)) {
    expect_true(assert_supply_chain_stages(tbl$supply_chain, label = "trade theme"))
    # No HS6 basket is a deployment good.
    expect_false("Deployment" %in% tbl$supply_chain)
  }

  # Refining and Recovery are cells of their own, each against its own world market.
  # Polysilicon (280461): world exports 100, Norway (Atlas only) 20.
  expect_equal(tr_value(es, "NOR", "Solar", "market_share", "raw", stage = "Refining"), 0.2)
  # Waste oil (271099): world exports 3, the United States 2.
  expect_equal(tr_value(es, "USA", "Oil", "market_share", "raw", stage = "Recovery"), 2 / 3)
  expect_equal(tr_value(eo, "USA", "Oil", "exports", "raw", stage = "Recovery"), 2)
  expect_equal(tr_value(eo, "USA", "Oil", "market_share", stage = "Recovery"), 1)
  # Smallland ships solar goods but no polysilicon: the United States outranks it there.
  expect_gt(
    tr_value(eo, "USA", "Solar", "market_share", stage = "Refining"),
    tr_value(eo, "SMA", "Solar", "market_share", stage = "Refining")
  )
})

test_that("the six stages split the old baskets without dropping or double-counting a code", {
  build <- function(fun, legacy) {
    do.call(fun, list(
      subcat = tr_subcat(), aec_4_data = tr_atlas4(), aec_6_data = tr_atlas6(),
      comtrade_trade = tr_comtrade(), comtrade_total_export = tr_total_export(),
      country_info = tr_countries(), year_4 = 2023, year_6 = 2023, year_comtrade = 2024,
      gdp_data = tr_gdp(), legacy_stages = legacy
    ))
  }
  raw_sum <- function(tbl, variable) {
    r <- tbl[tbl$variable == variable & tbl$data_type == "raw", ]
    tapply(r$value, paste(r$iso3c, r$tech), sum)
  }

  eo_legacy <- build(export_feasibility, TRUE)
  es_legacy <- build(trade_concentration, TRUE)
  expect_setequal(unique(eo_legacy$supply_chain), c("Upstream", "Midstream"))

  # Comtrade exports and imports, and the Atlas net imports, of each country's old basket are
  # the sums over the stages it split into: Solar Midstream = Refining + Components + Assembly,
  # Oil Upstream = Extraction + Refining + Recovery.
  eo <- build(export_feasibility, FALSE)
  es <- build(trade_concentration, FALSE)
  for (v in c("exports", "imports")) {
    expect_equal(raw_sum(eo, v), raw_sum(eo_legacy, v))
  }
  expect_equal(raw_sum(es, "deficit"), raw_sum(es_legacy, "deficit"))
  expect_equal(
    tr_value(eo_legacy, "CHN", "Solar", "exports", "raw", stage = "Midstream"),
    3 * 100 + 40 + 50
  )
})

test_that("Stage is read ahead of the three-stage columns, which are only a fallback", {
  sc <- tr_subcat()
  six <- trade_core_build_energy_codes(sc)
  expect_setequal(unique(six$supply_chain), c("Extraction", "Refining", "Components", "Assembly", "Recovery"))

  # The master's own shape: lower-case `stage` wins over its three-stage `supply_chain`.
  master_shape <- data.frame(
    tech = sc$Technology, supply_chain = sc$`Value Chain`, stage = sc$Stage,
    sub_sector = sc$Sub.Sector, HS6 = sc$HS6, stringsAsFactors = FALSE
  )
  expect_equal(trade_core_build_energy_codes(master_shape)$supply_chain, six$supply_chain)

  # legacy_stages builds the old baskets, quietly, for the readers not yet on six stages.
  expect_silent(legacy <- trade_core_build_energy_codes(sc, legacy_stages = TRUE))
  expect_setequal(unique(legacy$supply_chain), c("Midstream", "Upstream"))

  # A crosswalk staged before the Stage column existed still builds, and says so.
  expect_warning(
    fallback <- trade_core_build_energy_codes(sc[setdiff(names(sc), "Stage")]),
    "no Stage column"
  )
  expect_equal(fallback$supply_chain, legacy$supply_chain)

  # A code without a stage, or with an old stage name, stops rather than leaving the basket.
  blank <- sc
  blank$Stage[2] <- NA
  expect_error(trade_core_build_energy_codes(blank), "not supply-chain stages")
  old_name <- sc
  old_name$Stage[2] <- "Midstream"
  expect_error(trade_core_build_energy_codes(old_name), "Midstream")
})

test_that("every in-basket code of the HS6 master reaches one trade cell, in its own stage", {
  master <- utils::read.csv(
    file.path(repo_root, "data", "reference", "energy_hs6_master.csv"),
    colClasses = "character"
  )
  view <- hs6_build_views(master)[["hts_codes_categories_bolstered_final.csv"]]
  in_basket <- hs6_normalize_master(master)
  codes <- trade_core_build_energy_codes(view)

  expect_true(all(is_supply_chain_stage(codes$supply_chain)))
  expect_false("Deployment" %in% codes$supply_chain)
  expect_setequal(
    paste(codes$tech, codes$code6, codes$supply_chain),
    paste(in_basket$tech, in_basket$hs6, in_basket$stage)
  )
  # One stage per code within a technology, so no code counts twice in a technology's trade.
  expect_equal(anyDuplicated(paste(codes$tech, codes$code6)), 0L)
  # The cells are the master's tech x stage pairs.
  expect_setequal(
    unique(paste(codes$tech, codes$supply_chain)),
    unique(paste(in_basket$tech, in_basket$stage))
  )
})

# --- HHI ranking universe and re-export hubs (user decisions 2026-10-09) ------------------

test_that("the HHI index ranks the headline cells only and other cells cannot move them", {
  # Solar HHI 0.5, Oil 1 (one exporter), Wind 1/3; Semiconductors (unscored) varies.
  cells <- function(semis_iso, semis_x) {
    tibble::tibble(
      reporter_iso = c("A", "B", "A", "A", "B", "C", semis_iso),
      tech = c("Solar", "Solar", "Oil", "Wind", "Wind", "Wind", rep("Semiconductors", length(semis_iso))),
      supply_chain = c("Assembly", "Assembly", "Extraction", rep("Assembly", 3 + length(semis_iso))),
      sub_sector = "All",
      exports = c(1, 1, 5, 1, 1, 1, rep_len(semis_x, length(semis_iso)))
    )
  }
  monopoly <- cells("A", 7)
  flat <- cells(c("A", "B", "C", "D"), 1)
  headline <- c("Solar", "Oil", "Wind")
  idx <- function(ct, rank = headline) {
    h <- trade_core_build_comtrade_hhi(ct, rank_techs = rank)
    stats::setNames(h$HHI_index, h$tech)
  }

  # Headline cells: S(-HHI) over the three of them alone.
  expect_equal(unname(idx(monopoly)[c("Oil", "Solar", "Wind")]), c(0, 0.5, 1))
  for (ct in list(flat, monopoly[monopoly$tech != "Semiconductors", ])) {
    expect_equal(idx(ct)[c("Oil", "Solar", "Wind")], idx(monopoly)[c("Oil", "Solar", "Wind")])
  }
  # Semiconductors is placed against them: tied with Oil at the bottom (r = 0.5 / 3), or above all.
  s <- function(r) sqrt(r) / (sqrt(r) + sqrt(1 - r))
  expect_equal(unname(idx(monopoly)["Semiconductors"]), s(0.5 / 3))
  expect_equal(unname(idx(flat)["Semiconductors"]), 1)

  # Ranked across every cell (NULL, the former rule), the unscored cell moved the scored ones.
  expect_false(isTRUE(all.equal(idx(monopoly, NULL)[c("Oil", "Solar", "Wind")], idx(flat, NULL)[c("Oil", "Solar", "Wind")])))
  expect_warning(idx(monopoly, "Coal"), "no trade cell belongs")
})

# Hong Kong as a re-export hub: in Comtrade it ships five times China's solar goods; in the
# Atlas (and for its GDP) it looks like Smallland.
hk_countries <- function() rbind(tr_countries(), data.frame(country = "Hong Kong", iso3c = "HKG"))
hk_copy_of_sma <- function(tbl, col = "country_iso3_code") {
  h <- tbl[tbl[[col]] == "SMA", ]
  h[[col]] <- "HKG"
  rbind(tbl, h)
}
hk_comtrade <- function() {
  ct <- tr_comtrade()
  h <- ct[ct$reporter_iso == "CHN", ]
  h$reporter_iso <- "HKG"
  h$primary_value <- 5 * h$primary_value
  rbind(ct, h)
}
hk_total <- function() rbind(tr_total_export(), data.frame(ref_year = 2024L, reporter_iso = "HKG", primary_value = 9000))
hk_gdp <- function() rbind(tr_gdp(), data.frame(iso3c = "HKG", year = 2023L, NY.GDP.MKTP.CD = 400))
hk_build <- function(fun, hubs = NULL, comtrade = hk_comtrade(), total = hk_total()) {
  args <- list(
    subcat = tr_subcat(), aec_4_data = hk_copy_of_sma(tr_atlas4()), aec_6_data = hk_copy_of_sma(tr_atlas6()),
    comtrade_trade = comtrade, comtrade_total_export = total, country_info = hk_countries(),
    year_4 = 2023, year_6 = 2023, year_comtrade = 2024, gdp_data = hk_gdp(), reexport_hubs = hubs
  )
  do.call(fun, args)
}

test_that("re-export hubs leave the Comtrade measures and their world aggregates", {
  withr::local_options(list(
    opportunity_security.index_definition =
      yaml::read_yaml(file.path(repo_root, "config", "index_definition.yml"))
  ))
  # Without the rule Hong Kong's re-exports make it the largest solar exporter.
  eo_all <- hk_build(export_feasibility)
  expect_equal(tr_value(eo_all, "HKG", "Solar", "export_size"), 1)

  for (fun in c("trade_concentration", "export_feasibility")) {
    with_hubs <- hk_build(get(fun), hubs = "hkg")
    # The same as if Hong Kong had never reported to Comtrade: every other economy's exports,
    # RCA (world aggregates) and HHI (world shares), and Hong Kong's Atlas variables.
    never_reported <- hk_build(
      get(fun),
      comtrade = tr_comtrade(),
      total = tr_total_export()
    )
    # (The explanations differ only in naming the excluded hub.)
    key <- c("iso3c", "tech", "supply_chain", "variable", "data_type")
    tidy <- function(tbl) as.data.frame(dplyr::arrange(dplyr::select(tbl, -explanation), dplyr::across(dplyr::all_of(key))))
    expect_equal(tidy(with_hubs), tidy(never_reported), ignore_attr = TRUE)
    expect_true(any(grepl("re-export hubs (HKG) excluded", with_hubs$explanation, fixed = TRUE)))
    hk <- with_hubs[with_hubs$iso3c == "HKG", ]
    expect_false(any(hk$variable %in% c("exports", "imports", "export_rca", "rca", "export_size")))
    expect_true(all(c("market_share", "HHI") %in% hk$variable[hk$data_type == "index"]))
  }

  es <- hk_build(trade_concentration, hubs = "HKG")
  eo <- hk_build(export_feasibility, hubs = "HKG")
  expect_false(is.na(tr_value(es, "HKG", "Solar", "deficit_gdp")))
  expect_false(is.na(tr_value(eo, "HKG", "Solar", "feas")))
  # Solar HHI over China, Smallland and the United States (300, 6 and 30 of basket exports).
  expect_equal(tr_value(es, "CHN", "Solar", "HHI", "raw"), sum((c(300, 6, 30) / 336)^2))
  expect_equal(tr_value(eo, "CHN", "Solar", "export_size"), 1)
})

test_that("the pipeline ranks the HHI over the headline techs and drops the configured hubs", {
  # The hubs list is configuration, not code.
  idx_def <- yaml::read_yaml(file.path(repo_root, "config", "index_definition.yml"))
  expect_identical(toupper(unlist(idx_def$trade$re_export_hubs)), "HKG")

  # The ranking universe is config/coalition.yml headline_techs, the techs the pillars score.
  headline <- unlist(yaml::read_yaml(file.path(repo_root, "config", "coalition.yml"))$headline_techs)
  exprs <- parse(file.path(repo_root, "scripts", "20_build_indices.R"))
  is_techs <- vapply(exprs, function(e) {
    is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("techs"))
  }, logical(1))
  expect_setequal(headline, eval(exprs[[which(is_techs)[1]]][[3]]))
  # scripts/40 hands its own `techs` vector to the Atlas vintage build: it must be the same list.
  exprs40 <- parse(file.path(repo_root, "scripts", "40_build_index_vintages.R"))
  is_techs40 <- vapply(exprs40, function(e) {
    is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("techs"))
  }, logical(1))
  expect_true(any(is_techs40))
  expect_setequal(headline, eval(exprs40[[which(is_techs40)[1]]][[3]]))

  # scripts/10 passes both settings to both trade themes; scripts/40 the universe to its Atlas build.
  call_texts <- function(script, fn) {
    pd <- utils::getParseData(parse(file.path(repo_root, "scripts", script), keep.source = TRUE))
    name_expr <- pd$parent[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == fn]
    call_ids <- pd$parent[match(name_expr, pd$id)]
    vapply(call_ids, function(i) utils::getParseText(pd, i), character(1))
  }
  for (fn in c("trade_concentration", "export_feasibility")) {
    txt <- call_texts("10_build_themes.R", fn)
    expect_length(txt, 1)
    expect_match(txt, "hhi_rank_techs = trade_hhi_rank_techs", fixed = TRUE)
    expect_match(txt, "reexport_hubs = trade_reexport_hubs", fixed = TRUE)
  }
  expect_match(call_texts("40_build_index_vintages.R", "atlas_trade_theme"), "hhi_rank_techs = techs", fixed = TRUE)
})
