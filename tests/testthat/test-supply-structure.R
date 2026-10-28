# Supply structure of the strategic posture (R/strategic_position/supply_structure.R) and the coercive /
# trusted import shares of sp_import_exposure(): docs/strategic_position.md section 7, "Supply
# structure". Synthetic fixtures.

ss_repo_root <- function() {
  normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
}

suppressPackageStartupMessages(library(dplyr))
source(file.path(ss_repo_root(), "R", "utils", "supply_chain_stages.R"))
source(file.path(ss_repo_root(), "R", "strategic_position", "import_exposure.R"))
source(file.path(ss_repo_root(), "R", "strategic_position", "supply_structure.R"))

# A geo_alignment_tbl in miniature: geo_alignment follows the blocs as scripts/12 sets it (same
# non-neutral bloc 1, opposite blocs 0, anything with a Nonaligned economy 0.5).
ss_blocs <- c(
  USA = "U.S.-leaning", JPN = "U.S.-leaning", AUS = "U.S.-leaning",
  CHN = "China-leaning", HKG = "China-leaning", RUS = "China-leaning", IDN = "China-leaning",
  IND = "Nonaligned", CHL = "Nonaligned"
)
ss_alignment <- function(blocs = ss_blocs) {
  g <- expand.grid(reporter_iso = names(blocs), partner_iso = names(blocs), stringsAsFactors = FALSE)
  g <- g[g$reporter_iso != g$partner_iso, ]
  g$bloc_reporter <- unname(blocs[g$reporter_iso])
  g$bloc_partner <- unname(blocs[g$partner_iso])
  g$geo_alignment <- ifelse(g$bloc_reporter == "Nonaligned" | g$bloc_partner == "Nonaligned", 0.5,
                            ifelse(g$bloc_reporter == g$bloc_partner, 1, 0))
  tibble::as_tibble(g)
}
ss_pair <- function(cp, r, p) cp[cp$reporter_iso == r & cp$partner_iso == p, ]

# major_supplier_shares rows for one scope.
ss_rows <- function(tech, stage, measure, shares, mineral = NA_character_, demand = NA_real_, year = 2025L, aggregate = FALSE) {
  tibble::tibble(
    iso3 = names(shares), country = names(shares), tech = tech, supply_chain = stage, measure = measure,
    mineral = mineral, mineral_demand_share = demand, year = year, world_share = unname(shares),
    is_aggregate = rep_len(aggregate, length(shares))
  )
}
ss_cell <- function(res, iso, tech, stage) res$cells[res$cells$iso3 == iso & res$cells$tech == tech & res$cells$supply_chain == stage, ]

test_that("coercive pairs: opposite bloc, coercive powers outside their bloc, trusted same bloc", {
  cp <- sp_coercive_pairs(ss_alignment())
  # U.S.-leaning: the whole China-leaning bloc is coercive; China and Russia on both grounds.
  expect_true(ss_pair(cp, "USA", "CHN")$coercive)
  expect_equal(ss_pair(cp, "USA", "CHN")$coercive_basis, "opposite_bloc+power")
  expect_true(ss_pair(cp, "USA", "IDN")$coercive)
  expect_equal(ss_pair(cp, "USA", "IDN")$coercive_basis, "opposite_bloc")
  expect_true(ss_pair(cp, "USA", "JPN")$trusted)
  expect_false(ss_pair(cp, "USA", "JPN")$coercive)
  # Nonaligned: only the powers are coercive, and nothing is trusted (geo_alignment 0.5 with all).
  expect_true(ss_pair(cp, "IND", "CHN")$coercive)
  expect_true(ss_pair(cp, "IND", "HKG")$coercive)
  expect_equal(ss_pair(cp, "IND", "RUS")$coercive_basis, "power")
  expect_false(ss_pair(cp, "IND", "IDN")$coercive)
  expect_false(any(cp$trusted[cp$reporter_iso == "IND"]))
  # Same-bloc exemption: China is not coercive to Hong Kong or Russia, and they are trusted.
  expect_false(ss_pair(cp, "HKG", "CHN")$coercive)
  expect_true(ss_pair(cp, "HKG", "CHN")$trusted)
  expect_false(ss_pair(cp, "RUS", "CHN")$coercive)
  expect_true(ss_pair(cp, "CHN", "IDN")$trusted)
  # China's coercive suppliers are the U.S.-leaning bloc.
  expect_true(ss_pair(cp, "CHN", "AUS")$coercive)
  expect_false(ss_pair(cp, "CHN", "CHL")$coercive)
  expect_equal(nrow(cp), 9 * 8)

  # Sensitivity: the powers only.
  cp0 <- sp_coercive_pairs(ss_alignment(), opposite_bloc = FALSE)
  expect_false(ss_pair(cp0, "USA", "IDN")$coercive)
  expect_false(ss_pair(cp0, "USA", "IDN")$trusted)
  expect_true(ss_pair(cp0, "USA", "CHN")$coercive)
  expect_false(ss_pair(cp0, "CHN", "AUS")$coercive)
})

test_that("coercive pairs: missing alignment is NA unless the powers rule decides; unknown power bloc", {
  al <- ss_alignment()
  al$geo_alignment[al$reporter_iso == "JPN" & al$partner_iso %in% c("IDN", "CHN")] <- NA
  cp <- sp_coercive_pairs(al)
  expect_true(is.na(ss_pair(cp, "JPN", "IDN")$coercive))
  expect_true(is.na(ss_pair(cp, "JPN", "IDN")$trusted))
  expect_true(ss_pair(cp, "JPN", "CHN")$coercive)      # a power outside Japan's bloc
  expect_false(ss_pair(cp, "JPN", "CHN")$trusted)
  # A power the table gives no bloc for is coercive to every other economy, its would-be bloc included.
  al2 <- ss_alignment()
  al2$bloc_partner[al2$partner_iso == "RUS"] <- NA
  al2$bloc_reporter[al2$reporter_iso == "RUS"] <- NA
  expect_true(ss_pair(sp_coercive_pairs(al2), "CHN", "RUS")$coercive)
  expect_error(sp_coercive_pairs(dplyr::select(al, -bloc_partner)), "bloc_partner")
})

test_that("primary measure: mineral measures first where they exist, else the HS6 basket; none at Deployment", {
  mss <- dplyr::bind_rows(
    ss_rows("Batteries", "Refining", "iea_mineral_refining", c(CHN = 0.7, CHL = 0.2), "Lithium", 0.3),
    ss_rows("Batteries", "Refining", "atlas_export_share", c(CHN = 0.4, JPN = 0.6), year = 2023L),
    ss_rows("Solar", "Refining", "atlas_export_share", c(CHN = 0.3, USA = 0.7), year = 2023L),
    ss_rows("Coal", "Extraction", "ei_production", c(CHN = 0.5, AUS = 0.1, IDN = 0.3)),
    ss_rows("Coal", "Extraction", "atlas_export_share", c(AUS = 0.5, IDN = 0.5), year = 2023L),
    ss_rows("Nuclear", "Extraction", "uranium_mine_production", c(AUS = 0.4, CHL = 0.6), year = 2024L),
    ss_rows("Nuclear", "Extraction", "usgs_reserves", c(AUS = 1)),
    ss_rows("Batteries", "Components", "atlas_export_share", c(CHN = 0.5, JPN = 0.5), year = 2023L),
    ss_rows("Batteries", "Components", "iea_manufacturing_capacity", c(CHN = 0.8), year = 2024L),
    # A mineral measure whose minerals all fall below the demand floor gives way to the basket.
    ss_rows("Wind", "Refining", "iea_mineral_refining", c(CHN = 0.9), "Copper", 0.05),
    ss_rows("Wind", "Refining", "atlas_export_share", c(USA = 1), year = 2023L),
    # No HS6 basket: the IEA capacity is the fallback; the EU bloc row is an aggregate, not a supplier.
    ss_rows("Green Hydrogen", "Assembly", "iea_manufacturing_capacity", c(CHN = 0.4, USA = 0.1), year = 2024L),
    ss_rows("Green Hydrogen", "Assembly", "iea_manufacturing_capacity", c(EUU = 0.2), year = 2024L, aggregate = TRUE)
  )
  res <- sp_supply_structure(mss, sp_coercive_pairs(ss_alignment()), mineral_min_demand_share = 0.1)
  pm <- function(tech, stage) ss_cell(res, "USA", tech, stage)$primary_measure
  expect_equal(pm("Batteries", "Refining"), "iea_mineral_refining")
  expect_equal(pm("Solar", "Refining"), "atlas_export_share")
  expect_equal(pm("Coal", "Extraction"), "ei_production")
  expect_equal(pm("Nuclear", "Extraction"), "uranium_mine_production")
  expect_equal(pm("Batteries", "Components"), "atlas_export_share")
  expect_equal(pm("Wind", "Refining"), "atlas_export_share")
  expect_equal(pm("Green Hydrogen", "Assembly"), "iea_manufacturing_capacity")
  expect_equal(pm("Batteries", "Deployment"), "none")
  expect_equal(pm("Batteries", "Recovery"), "none")
  expect_equal(ss_cell(res, "USA", "Batteries", "Deployment")$home_status, "no_data")
  expect_equal(ss_cell(res, "USA", "Batteries", "Refining")$measure_year, 2025L)
  # The aggregate never becomes the world's top supplier; its share stays in the unlisted remainder.
  gh <- ss_cell(res, "USA", "Green Hydrogen", "Assembly")
  expect_equal(gh$world_top_iso3, "CHN")
  expect_equal(gh$world_unlisted_share, 0.5)
  # Every measure the cell has is summarised on its own, reserves excluded.
  m <- res$measures[res$measures$iso3 == "USA" & res$measures$tech == "Batteries" & res$measures$supply_chain == "Components", ]
  expect_setequal(m$measure, c("atlas_export_share", "iea_manufacturing_capacity"))
  expect_false("usgs_reserves" %in% res$measures$measure)
  # One row per assessed country x tech x six stages.
  expect_equal(nrow(res$cells), 9 * length(unique(mss$tech)) * 6)

  # A "<tech>/<stage>" entry overrides the stage's list for that technology only; an empty one
  # leaves the cell without a measure.
  pri <- c(SP_SUPPLY_MEASURE_PRIORITY, list(`Coal/Extraction` = "atlas_export_share", `Batteries/Components` = character()))
  res2 <- sp_supply_structure(mss, sp_coercive_pairs(ss_alignment()), measure_priority = pri)
  expect_equal(ss_cell(res2, "USA", "Coal", "Extraction")$primary_measure, "atlas_export_share")
  expect_equal(ss_cell(res2, "USA", "Coal", "Extraction")$world_top_share, 0.5)
  expect_equal(ss_cell(res2, "USA", "Nuclear", "Extraction")$primary_measure, "uranium_mine_production")
  expect_equal(ss_cell(res2, "USA", "Batteries", "Components")$primary_measure, "none")
  expect_error(sp_supply_structure(mss, sp_coercive_pairs(ss_alignment()), measure_priority = list(`Coal/Mining` = "x")), "unknown stage")
})

test_that("mineral cells: demand-weighted home share, the driving mineral is the largest coercive hold", {
  mss <- dplyr::bind_rows(
    ss_rows("Electric Vehicles", "Refining", "iea_mineral_refining", c(CHN = 0.6, AUS = 0.2, JPN = 0.1), "Lithium", 0.6),
    ss_rows("Electric Vehicles", "Refining", "iea_mineral_refining", c(IDN = 0.55, CHN = 0.1, AUS = 0.3), "Nickel", 0.3),
    # Below the demand floor: ignored, however concentrated.
    ss_rows("Electric Vehicles", "Refining", "iea_mineral_refining", c(CHN = 0.95, AUS = 0.05), "Copper", 0.05)
  )
  res <- sp_supply_structure(mss, sp_coercive_pairs(ss_alignment()), mineral_min_demand_share = 0.1)

  aus <- ss_cell(res, "AUS", "Electric Vehicles", "Refining")
  expect_equal(aus$home_share, (0.6 * 0.2 + 0.3 * 0.3) / 0.9)
  expect_equal(aus$home_status, "listed")
  expect_equal(aus$n_minerals, 2L)
  expect_equal(aus$minerals, "Lithium;Nickel")
  # Copper (95% China) is below the floor; lithium (China 60%) beats nickel (Indonesia 55%).
  expect_equal(aus$driving_mineral, "Lithium")
  expect_equal(aus$coercive_top_iso3, "CHN")
  expect_equal(aus$coercive_top_share, 0.6)
  expect_equal(aus$coercive_world_share, 0.6)
  expect_equal(aus$mineral_demand_share, 0.6)
  expect_equal(aus$driving_home_share, 0.2)
  expect_equal(aus$trusted_world_share, 0.2 + 0.1)  # home plus Japan, on lithium

  # The driving mineral is the country's own: for China the coercive hold is Australia's.
  chn <- ss_cell(res, "CHN", "Electric Vehicles", "Refining")
  expect_equal(chn$driving_mineral, "Nickel")
  expect_equal(chn$coercive_top_iso3, "AUS")
  expect_equal(chn$coercive_top_share, 0.3)
  expect_equal(chn$trusted_world_share, 0.1 + 0.55)  # home plus Indonesia, on nickel

  m <- res$minerals[res$minerals$iso3 == "AUS", ]
  expect_equal(m$weight[m$mineral == "Lithium"], 0.6 / 0.9)
  expect_equal(m$weight[m$mineral == "Copper"], 0)
  expect_false(m$eligible[m$mineral == "Copper"])
  expect_equal(m$mineral[m$driving], "Lithium")
})

test_that("mineral measures supplement each other: EI minerals join the IEA list at Extraction", {
  mss <- dplyr::bind_rows(
    ss_rows("Wind", "Extraction", "iea_mineral_mining", c(CHN = 0.6, AUS = 0.1), "Rare Earths", 0.5),
    ss_rows("Wind", "Extraction", "ei_mineral_mining", c(CHN = 0.3, AUS = 0.2), "Zinc", 0.9),
    ss_rows("Green Hydrogen", "Extraction", "ei_mineral_mining", c(CHL = 0.5, RUS = 0.4), "PGMs", 1)
  )
  res <- sp_supply_structure(mss, sp_coercive_pairs(ss_alignment()))
  w <- ss_cell(res, "AUS", "Wind", "Extraction")
  expect_equal(w$primary_measure, "iea_mineral_mining")
  expect_equal(w$mineral_measures, "iea_mineral_mining+ei_mineral_mining")
  expect_equal(w$home_share, (0.5 * 0.1 + 0.9 * 0.2) / 1.4)
  expect_equal(w$driving_mineral, "Rare Earths")
  # Without an IEA listing the EI sheet is the primary measure.
  expect_equal(ss_cell(res, "USA", "Green Hydrogen", "Extraction")$primary_measure, "ei_mineral_mining")
  # The per-measure table keeps each measure on its own.
  m <- res$measures[res$measures$iso3 == "AUS" & res$measures$tech == "Wind", ]
  expect_equal(m$home_share[m$measure == "iea_mineral_mining"], 0.1)
})

test_that("home share: absent from a complete listing is zero, from a partial listing NA and bracketed", {
  mss <- dplyr::bind_rows(
    # Atlas: every exporter is listed (shares sum to 1), so absence is a structural zero.
    ss_rows("Solar", "Components", "atlas_export_share", c(CHN = 0.8, USA = 0.2), year = 2023L),
    # IEA refining: the main producers plus an unlisted 'Rest of world' of 15%; a listed zero.
    ss_rows("Batteries", "Refining", "iea_mineral_refining", c(CHN = 0.7, CHL = 0.15, USA = 0), "Lithium", 0.5),
    ss_rows("Batteries", "Refining", "iea_mineral_refining", c(CHN = 0.9, JPN = 0.1), "Graphite", 0.5),
    # A residual row the upstream build normally drops is never a supplier.
    tibble::tibble(iso3 = "ROW", country = "Rest of world", tech = "Batteries", supply_chain = "Refining",
                   measure = "iea_mineral_refining", mineral = "Graphite", mineral_demand_share = 0.5,
                   year = 2025L, world_share = 0.5, is_aggregate = FALSE)
  )
  res <- sp_supply_structure(mss, sp_coercive_pairs(ss_alignment()))
  jpn_solar <- ss_cell(res, "JPN", "Solar", "Components")
  expect_equal(jpn_solar$home_share, 0)
  expect_equal(jpn_solar$home_status, "zero")
  expect_equal(jpn_solar$home_share_hi, 0)

  # Japan: unlisted in lithium (bounded by the 15% remainder), listed in graphite.
  jpn <- ss_cell(res, "JPN", "Batteries", "Refining")
  expect_true(is.na(jpn$home_share))
  expect_equal(jpn$home_status, "partial")
  expect_equal(jpn$home_share_lo, 0.5 * 0 + 0.5 * 0.1)
  expect_equal(jpn$home_share_hi, 0.5 * 0.15 + 0.5 * 0.1)
  # Australia is in neither listing: unlisted, not zero.
  aus <- ss_cell(res, "AUS", "Batteries", "Refining")
  expect_true(is.na(aus$home_share))
  expect_equal(aus$home_status, "unlisted")
  # The United States has a recorded zero in lithium but is unlisted in graphite.
  expect_equal(ss_cell(res, "USA", "Batteries", "Refining")$home_status, "partial")
  m <- res$minerals[res$minerals$iso3 == "USA" & res$minerals$mineral == "Lithium", ]
  expect_equal(m$home_status, "zero")
  expect_equal(m$home_share, 0)
  expect_equal(m$world_unlisted_share, 0.15)
  expect_false("ROW" %in% res$cells$world_top_iso3)
  expect_false("ROW" %in% res$cells$coercive_top_iso3)
})

test_that("coercive and trusted world shares follow the reporter's blocs; India trusts only itself", {
  mss <- ss_rows("Solar", "Assembly", "atlas_export_share",
                 c(CHN = 0.5, IDN = 0.1, JPN = 0.15, USA = 0.1, IND = 0.05, CHL = 0.05, RUS = 0.05), year = 2023L)
  res <- sp_supply_structure(mss, sp_coercive_pairs(ss_alignment()))
  usa <- ss_cell(res, "USA", "Solar", "Assembly")
  expect_equal(usa$coercive_world_share, 0.5 + 0.1 + 0.05)   # China, Indonesia, Russia
  expect_equal(usa$trusted_world_share, 0.1 + 0.15)          # home plus Japan
  expect_equal(usa$world_top_iso3, "CHN")
  expect_equal(usa$world_hhi, sum(c(0.5, 0.1, 0.15, 0.1, 0.05, 0.05, 0.05)^2))
  ind <- ss_cell(res, "IND", "Solar", "Assembly")
  expect_equal(ind$coercive_world_share, 0.5 + 0.05)          # the powers only
  expect_equal(ind$trusted_world_share, 0.05)                 # home
  chn <- ss_cell(res, "CHN", "Solar", "Assembly")
  expect_equal(chn$coercive_top_iso3, "JPN")
  expect_equal(chn$coercive_world_share, 0.15 + 0.1)          # Japan, the United States
  expect_equal(chn$trusted_world_share, 0.5 + 0.1 + 0.05)     # home, Indonesia, Russia
  # The powers-only sensitivity drops Indonesia from the U.S. coercive set.
  res0 <- sp_supply_structure(mss, sp_coercive_pairs(ss_alignment(), opposite_bloc = FALSE))
  expect_equal(ss_cell(res0, "USA", "Solar", "Assembly")$coercive_world_share, 0.5 + 0.05)
})

test_that("import exposure: coercive and trusted import shares from the coercive pairs", {
  tc <- tibble::tribble(
    ~exporter_iso, ~importer_iso, ~tech, ~supply_chain, ~value_last,
    "CHN", "JPN", "Solar", "Assembly", 50,
    "IDN", "JPN", "Solar", "Assembly", 10,
    "USA", "JPN", "Solar", "Assembly", 30,
    "XXX", "JPN", "Solar", "Assembly", 10,
    "CHN", "IND", "Solar", "Assembly", 40,
    "RUS", "IND", "Solar", "Assembly", 10,
    "IDN", "IND", "Solar", "Assembly", 50,
    "CHN", "HKG", "Solar", "Assembly", 80,
    "USA", "HKG", "Solar", "Assembly", 20,
    "CHN", "AUS", "Solar", "Assembly", 0,
    "CHN", "NZL", "Solar", "Assembly", 5
  )
  al <- ss_alignment()
  gdp <- tibble::tibble(iso3 = character(), gdp_usd = numeric())
  ie <- sp_import_exposure(tc, al, gdp, coercive_pairs = sp_coercive_pairs(al))
  row <- function(iso) ie[ie$importer_iso == iso, ]
  expect_equal(row("JPN")$coercive_import_share, 0.6)       # China and Indonesia; XXX is unaligned
  expect_equal(row("JPN")$trusted_import_share, 0.3)
  expect_equal(row("JPN")$coercive_import_share, row("JPN")$opposite_bloc_share)
  expect_equal(row("IND")$coercive_import_share, 0.5)       # China and Russia, not Indonesia
  expect_equal(row("IND")$trusted_import_share, 0)
  expect_equal(row("HKG")$coercive_import_share, 0.2)       # the United States, not China
  expect_equal(row("HKG")$trusted_import_share, 0.8)
  expect_equal(c(row("AUS")$coercive_import_share, row("AUS")$trusted_import_share), c(0, 0))
  expect_true(is.na(row("NZL")$coercive_import_share))      # no alignment for New Zealand
  # Without the pairs the table is unchanged.
  ie0 <- sp_import_exposure(tc, al, gdp)
  expect_false(any(c("coercive_import_share", "trusted_import_share") %in% names(ie0)))
  expect_equal(ie0, dplyr::select(ie, -coercive_import_share, -trusted_import_share))
})

test_that("cost premium: relative to the cheapest producer, broadcast regions flagged, unmeasured economies absent", {
  raw <- tibble::tribble(
    ~Product, ~Unit, ~Region, ~Year, ~Value,
    "Solar PV", "%", "China", 2023, 100,
    "Solar PV", "%", "United States", 2023, 114,
    "Solar PV", "%", "European Union", 2023, 140,
    "Solar PV", "%", "Korea", 2023, 126,
    "Solar PV", "%", "India", 2023, 123,
    "Solar PV", "%", "Other Southeast Asia", 2023, 114,
    "Ammonia", "%", "China", 2023, 100,
    "Ammonia", "%", "United States", 2023, 62,
    "Batteries", "%", "China", 2023, 120,
    "Batteries", "%", "Korea", 2023, 100
  )
  rmap <- sp_cost_region_map(eu_iso3 = c("DEU", "FRA"))
  p <- sp_cost_premium(raw, rmap)
  get <- function(iso, tech) p[p$iso3 == iso & p$tech == tech, ]
  expect_equal(get("USA", "Solar")$premium_pct, 14)
  expect_equal(get("CHN", "Solar")$premium_pct, 0)
  expect_equal(get("DEU", "Solar")$premium_pct, get("FRA", "Solar")$premium_pct)
  expect_true(get("DEU", "Solar")$region_broadcast)
  expect_false(get("USA", "Solar")$region_broadcast)
  expect_true(get("VNM", "Solar")$region_broadcast)
  expect_equal(get("USA", "Solar")$premium_basis, "iea_regional")
  expect_equal(get("USA", "Solar")$supply_chain, "Assembly")
  # The cheapest producer need not be China; batteries are cells (Components).
  expect_equal(get("CHN", "Batteries")$premium_pct, 20)
  expect_equal(get("CHN", "Batteries")$cheapest_region, "Korea")
  expect_equal(get("CHN", "Batteries")$supply_chain, "Components")
  # Ammonia is not a headline-technology product; Japan is in no region.
  expect_false(any(p$product == "Ammonia"))
  expect_false("JPN" %in% p$iso3)
  expect_error(sp_cost_premium(dplyr::bind_rows(raw, tibble::tibble(Product = "Solar PV", Unit = "%", Region = "Japan", Year = 2023, Value = 130)), rmap),
               "not in region_map")
})

test_that("the premium products and ASEAN list mirror the cost theme", {
  env <- new.env()
  sys.source(file.path(ss_repo_root(), "R", "categories", "economic opportunity", "cost_competitiveness.R"), envir = env)
  theme <- env$cost_competitiveness_iea_products()
  mine <- SP_COST_PREMIUM_PRODUCTS
  expect_equal(
    theme[match(mine$Product, theme$Product), c("tech", "supply_chain")],
    mine[, c("tech", "supply_chain")],
    ignore_attr = TRUE
  )
  expect_setequal(SP_COST_SOUTHEAST_ASIA_ISO3, env$COST_COMPETITIVENESS_SOUTHEAST_ASIA_ISO3)
})
