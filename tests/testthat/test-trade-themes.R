# Guards the defects the two trade themes carried before this test existed:
#   1. trade_concentration (ES) and export_feasibility (EO) were the same table, so the ES
#      "Trade Risk" category was an exporter-strength score and deficit_gdp never existed
#   2. export size and Atlas feasibility were ranked within each country across its own
#      technology cells, not across countries within a cell
#   3. an HS4 heading counted once per child HS6 code in the feasibility mean
#   4. countries without Comtrade rows lost their observed Atlas variables and came through
#      as rows with tech = NA

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "levels.R"))
source(file.path(repo_root, "R", "categories", "shared", "overall_index.R"))
source(file.path(repo_root, "R", "categories", "trade", "trade_core.R"))
source(file.path(repo_root, "R", "categories", "trade", "trade_concentration.R"))
source(file.path(repo_root, "R", "categories", "trade", "export_feasibility.R"))

# --- fixtures ---------------------------------------------------------------------------

# Solar Midstream = two HS6 codes under one HS4 heading plus one under another; Oil
# Upstream = one code. The 8541 heading has two children, so a per-HS6 join would weigh it
# twice.
tr_subcat <- function() {
  data.frame(
    Technology = c("Solar", "Solar", "Solar", "Oil"),
    `Value Chain` = c("Midstream", "Midstream", "Midstream", "Upstream"),
    Sub.Sector = "x",
    HS6 = c("854142", "854143", "850440", "270900"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# CHN is the big solar exporter; SMA is a small economy whose only exports are solar goods
# (the within-country ranking used to score it 1 on export size); USA runs a large solar
# deficit; NOR exports oil and reports only to the Atlas, not to Comtrade.
tr_countries <- function() {
  data.frame(
    country = c("China", "Smallland", "United States", "Norway"),
    iso3c = c("CHN", "SMA", "USA", "NOR"),
    stringsAsFactors = FALSE
  )
}

tr_atlas6 <- function() {
  grid <- expand.grid(
    country_iso3_code = c("CHN", "SMA", "USA", "NOR"),
    product_hs92_code = c("854142", "854143", "850440", "270900"),
    stringsAsFactors = FALSE
  )
  exports <- c(
    CHN = 100, SMA = 2, USA = 10, NOR = 0
  )[grid$country_iso3_code]
  imports <- c(CHN = 20, SMA = 1, USA = 90, NOR = 5)[grid$country_iso3_code]
  oil <- grid$product_hs92_code == "270900"
  exports[oil] <- c(CHN = 0, SMA = 0, USA = 20, NOR = 80)[grid$country_iso3_code[oil]]
  imports[oil] <- c(CHN = 90, SMA = 1, USA = 30, NOR = 0)[grid$country_iso3_code[oil]]

  world <- tapply(exports, grid$product_hs92_code, sum)
  data.frame(
    grid,
    year = 2023L,
    export_value = unname(exports),
    import_value = unname(imports),
    global_market_share = unname(exports / world[grid$product_hs92_code]),
    stringsAsFactors = FALSE
  )
}

# Distance by country and heading: 8541 near for CHN, far for SMA; 8504 the reverse.
tr_atlas4 <- function() {
  grid <- expand.grid(
    country_iso3_code = c("CHN", "SMA", "USA", "NOR"),
    product_hs92_code = c("8541", "8504", "2709"),
    stringsAsFactors = FALSE
  )
  d <- c(
    "CHN.8541" = 0.1, "CHN.8504" = 0.5, "CHN.2709" = 0.9,
    "SMA.8541" = 0.9, "SMA.8504" = 0.1, "SMA.2709" = 0.9,
    "USA.8541" = 0.5, "USA.8504" = 0.5, "USA.2709" = 0.3,
    "NOR.8541" = 0.8, "NOR.8504" = 0.8, "NOR.2709" = 0.1
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

tr_value <- function(tbl, iso, tech, variable, data_type = "index") {
  v <- tbl$value[tbl$iso3c == iso & tbl$tech == tech & tbl$variable == variable & tbl$data_type == data_type]
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

  # Smallland: 8541 distance 0.9 (two child codes), 8504 distance 0.1 (one child code).
  # Per heading: 1 - (0.9 + 0.1) / 2 = 0.5. Per child code it was 1 - (0.9 * 2 + 0.1) / 3.
  sma <- feas$feasibility[feas$country_iso3_code == "SMA" & feas$tech == "Solar"]
  expect_equal(sma, 0.5)
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
  per_cell <- tapply(hhi$value, hhi$tech, function(x) length(unique(x)))
  expect_true(all(per_cell == 1))
})

test_that("the RCA world aggregates use the reporters present in both extracts", {
  codes <- trade_core_build_energy_codes(tr_subcat())
  trade <- trade_core_build_comtrade_trade(tr_comtrade(), codes, year = 2024)
  total <- rbind(tr_total_export(), data.frame(ref_year = 2024L, reporter_iso = "XXX", primary_value = 1e6))

  rca <- trade_core_build_comtrade_rca(trade, total, year = 2024)
  chn <- rca[rca$reporter_iso == "CHN" & rca$tech == "Solar", ]
  # World total over CHN + SMA + USA only; XXX reports no basket flows.
  expect_equal(chn$world_share, sum(trade$exports[trade$tech == "Solar"]) / 5007)
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
  solar <- ms[ms$tech == "Solar", ]
  share <- function(iso) solar$market_share[solar$country_iso3_code == iso]

  # World solar basket: 854142 and 854143 (112 each) + 850440 (50) = 274.
  expect_equal(share("CHN"), 200 / 274)
  expect_equal(share("SMA"), (4 + 50) / 274)
  # No basket exports is a zero share, not a missing one.
  expect_equal(share("NOR"), 0)
  expect_gt(share("CHN"), share("SMA"))
})
