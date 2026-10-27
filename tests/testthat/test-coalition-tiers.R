# Two-tier coalition and EU node (R/indices/coalition.R, scripts/13_build_coalition.R).

coalition_test_root <- function() {
  normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
}

coalition_test_load <- function() {
  root <- coalition_test_root()
  source(file.path(root, "R", "utils", "supply_chain_stages.R"), local = FALSE)
  source(file.path(root, "R", "utils", "country.R"), local = FALSE)
  source(file.path(root, "R", "utils", "iea_critical_minerals.R"), local = FALSE)
  source(file.path(root, "R", "categories", "production", "critical_minerals_production.R"), local = FALSE)
  source(file.path(root, "R", "categories", "reserves", "reserves.R"), local = FALSE)
  source(file.path(root, "R", "categories", "reserves", "usgs_mcs.R"), local = FALSE)
  source(file.path(root, "R", "indices", "coalition.R"), local = FALSE)
}

# Fixtures: AAA..III are invented codes, so nothing depends on the real country roster.
coalition_test_blocs <- function() {
  data.frame(
    iso3 = c("AAA", "BBB", "CCC", "DDD", "EEE", "FFF", "GGG", "HHH", "III"),
    country = paste("Country", c("A", "B", "C", "D", "E", "F", "G", "H", "I")),
    bloc = c("U.S.-leaning", "U.S.-leaning", "Nonaligned", "China-leaning", "Nonaligned",
             "China-leaning", "U.S.-leaning", "U.S.-leaning", NA),
    alignment_basis = c("own_votes", "own_votes", "own_votes", "own_votes", "own_votes",
                        "own_votes", "inherited", "own_votes", "none"),
    stringsAsFactors = FALSE
  )
}

coalition_test_gdp <- function() {
  data.frame(
    iso3c = c("AAA", "AAA", "BBB", "CCC", "DDD", "EEE", "FFF", "GGG", "HHH", "III", "JJJ"),
    year = c(2024, 2025, 2025, 2025, 2025, 2025, 2025, 2025, 2025, 2025, 2025),
    `NY.GDP.MKTP.CD` = c(5e10, 2e11, 5e10, 3e11, 4e11, 1e10, 2e10, 5e11, 9e11, 1e9, 1e12),
    check.names = FALSE
  )
}

coalition_test_agreements <- function() {
  data.frame(
    iso3 = c("AAA", "BBB", "CCC", "DDD", "EEE", "KKK", "EUU", "JJJ"),
    agreement = c("NATO", "NATO", "IPEF", "IPEF", "MSP Forum", "U.S. FTA", "MSP", "MSP Forum"),
    stringsAsFactors = FALSE
  )
}

coalition_test_suppliers <- function() {
  tibble::tibble(
    iso3 = c("AAA", "CCC", "DDD", "EEE", "FFF", "III", "LLL", "EUU", "MMM"),
    country = paste("Supplier", 1:9),
    tech = "Batteries",
    supply_chain = c("Extraction", "Components", "Extraction", "Extraction", "Extraction", "Extraction", "Components", "Components", "Extraction"),
    measure = "atlas_export_share",
    mineral = NA_character_,
    world_share = c(0.10, 0.05, 0.30, 0.049, 0.20, 0.08, 0.06, 0.40, 0.07),
    is_aggregate = c(rep(FALSE, 7), TRUE, FALSE)
  )
}

coalition_test_tiers <- function(...) {
  derive_coalition_tiers(
    bloc_tbl = coalition_test_blocs(),
    gdp_tbl = coalition_test_gdp(),
    agreements_tbl = coalition_test_agreements(),
    supplier_tbl = coalition_test_suppliers(),
    ...
  )
}

test_that("a country qualifying for core is core, and every route is recorded", {
  coalition_test_load()
  out <- coalition_test_tiers()
  a <- out[out$iso3 == "AAA", ]
  expect_equal(a$tier, "core")
  expect_equal(a$routes, "core; agreement; supplier")
  expect_true(a$route_core && a$route_agreement && a$route_supplier)
  expect_equal(a$gdp_usd, 2e11) # the latest year, not the first row

  # CCC: nonaligned, big, IPEF and a major supplier at exactly the threshold -> partner by both routes.
  c <- out[out$iso3 == "CCC", ]
  expect_equal(c$tier, "partner")
  expect_equal(c$routes, "agreement; supplier")
  expect_equal(c$qualifying_agreements, "IPEF")

  # Tiers are core, partner or none; one row per economy, core first.
  expect_true(all(out$tier %in% COALITION_TIERS))
  expect_equal(anyDuplicated(out$iso3), 0)
  expect_equal(out$tier, out$tier[order(match(out$tier, COALITION_TIERS))])
})

test_that("the GDP floor applies to core and agreements but not to the supplier route", {
  coalition_test_load()
  out <- coalition_test_tiers(gdp_floor_usd = 1e11)
  tier <- stats::setNames(out$tier, out$iso3)
  routes <- stats::setNames(out$routes, out$iso3)

  # BBB: U.S.-leaning and NATO, but GDP below the floor -> neither core nor agreement partner.
  expect_equal(tier[["BBB"]], "none")
  expect_equal(routes[["BBB"]], "none")
  # MMM: not in the bloc table and without GDP, so no route at all (no alignment).
  expect_equal(tier[["MMM"]], "none")
  # LLL: a major supplier with no GDP and no bloc -> excluded while alignment is required.
  expect_equal(tier[["LLL"]], "none")
  expect_true(out$supplier_excluded[out$iso3 == "LLL"])

  # A small nonaligned supplier qualifies: with LLL in the bloc table as nonaligned and no GDP.
  blocs <- rbind(coalition_test_blocs(), data.frame(iso3 = "LLL", country = "Country L", bloc = "Nonaligned",
                                                    alignment_basis = "own_votes"))
  out2 <- derive_coalition_tiers(blocs, coalition_test_gdp(), coalition_test_agreements(), coalition_test_suppliers())
  expect_equal(out2$tier[out2$iso3 == "LLL"], "partner")
  expect_equal(out2$routes[out2$iso3 == "LLL"], "supplier")
  expect_true(is.na(out2$gdp_usd[out2$iso3 == "LLL"]))

  # Lowering the floor makes BBB core.
  low <- coalition_test_tiers(gdp_floor_usd = 1e10)
  expect_equal(low$tier[low$iso3 == "BBB"], "core")
})

test_that("China-leaning economies are excluded from the supplier route only", {
  coalition_test_load()
  out <- coalition_test_tiers()
  # FFF: China-leaning major supplier (20%), no agreement -> none, flagged.
  f <- out[out$iso3 == "FFF", ]
  expect_equal(f$tier, "none")
  expect_false(f$route_supplier)
  expect_true(f$supplier_excluded)
  expect_match(f$supplier_cells, "Batteries Extraction")
  # DDD: China-leaning major supplier in IPEF with GDP above the floor -> partner by agreement only.
  d <- out[out$iso3 == "DDD", ]
  expect_equal(d$tier, "partner")
  expect_equal(d$routes, "agreement")
  # III has no bloc: excluded while supplier_requires_alignment, a partner without it.
  expect_equal(out$tier[out$iso3 == "III"], "none")
  loose <- coalition_test_tiers(supplier_requires_alignment = FALSE)
  expect_equal(loose$tier[loose$iso3 == "III"], "partner")
  # EEE: nonaligned, 4.9% < 5% and only in the MSP Forum (not a partner route by default).
  expect_equal(out$tier[out$iso3 == "EEE"], "none")
  expect_equal(out$agreements[out$iso3 == "EEE"], "MSP Forum")
  expect_true(is.na(out$qualifying_agreements[out$iso3 == "EEE"]))
  # The supplier threshold is inclusive and configurable.
  strict <- coalition_test_tiers(supplier_share = 0.06)
  expect_equal(strict$routes[strict$iso3 == "CCC"], "agreement")
})

test_that("core needs the economy's own UN votes; the MSP Forum is a sensitivity", {
  coalition_test_load()
  out <- coalition_test_tiers()
  # GGG: U.S.-leaning by inheritance with a big GDP -> not core.
  expect_equal(out$tier[out$iso3 == "GGG"], "none")
  expect_false(out$own_vote[out$iso3 == "GGG"])
  any_vote <- coalition_test_tiers(core_requires_own_vote = FALSE)
  expect_equal(any_vote$tier[any_vote$iso3 == "GGG"], "core")
  # Earlier scripts/12 outputs mark inherited economies with proxy_of instead of alignment_basis.
  old <- coalition_test_blocs()
  old$proxy_of <- ifelse(old$alignment_basis == "inherited", "HHH", NA_character_)
  old$alignment_basis <- NULL
  expect_equal(coalition_own_vote(old), c(rep(TRUE, 6), FALSE, TRUE, FALSE)) # III: no bloc, no vote

  # JJJ: no bloc row, big GDP, MSP Forum only.
  expect_equal(out$tier[out$iso3 == "JJJ"], "none")
  forum <- coalition_test_tiers(partner_agreements = c("U.S. FTA", "IPEF", "MSP", "G7", "NATO", "MSP Forum"))
  expect_equal(forum$tier[forum$iso3 == "JJJ"], "partner")
  # EEE is in the Forum but below the GDP floor: the agreement route keeps its floor.
  expect_equal(forum$tier[forum$iso3 == "EEE"], "none")
  # The EU's own agreement rows (EUU) do not become a country row.
  expect_false("EUU" %in% out$iso3)
})

test_that("EU members map to one node with the configured tier and keep their own tier", {
  coalition_test_load()
  expect_equal(eu_node_map(c("aaa", "BBB", "CCC", NA), c("AAA", "BBB")), c("EUU", "EUU", "CCC", NA))
  expect_equal(eu_node_map("AAA", "AAA", eu_code = "EU"), "EU")

  out <- coalition_test_tiers(eu_members = c("AAA", "BBB", "NNN"))
  eu <- out[out$eu_member, ]
  expect_setequal(eu$iso3, c("AAA", "BBB", "NNN")) # a member absent from every input still gets a row
  expect_true(all(eu$network_node == "EUU"))
  expect_true(all(eu$node_tier == "core"))
  expect_equal(eu$tier[eu$iso3 == "BBB"], "none") # its own tier
  expect_equal(eu$tier[eu$iso3 == "AAA"], "core")
  non_eu <- out[!out$eu_member, ]
  expect_equal(non_eu$network_node, non_eu$iso3)
  expect_equal(non_eu$node_tier, non_eu$tier)
})

test_that("EU stage weights follow the basis priority and sum to one per (tech, stage)", {
  coalition_test_load()
  eu <- c("AAA", "BBB", "CCC")
  supplier <- tibble::tibble(
    iso3 = c("AAA", "BBB", "EUU", "XXX",   # Oil Extraction production (EI-like, with an EU total)
             "AAA", "BBB", "CCC",          # Oil Extraction exports (lower priority)
             "AAA", "CCC",                 # Solar Assembly exports
             "AAA", "BBB", "EUU",          # Gas Extraction production: members cover 30% of the EU total
             "AAA",                        # Nuclear Extraction: EU share below min_eu_world_share
             "AAA", "BBB"),                # Wind Assembly: a measure not in basis_priority
    tech = c("Oil", "Oil", "Oil", "Oil", "Oil", "Oil", "Oil", "Solar", "Solar", "Gas", "Gas", "Gas", "Nuclear", "Wind", "Wind"),
    supply_chain = c(rep("Extraction", 7), "Assembly", "Assembly", rep("Extraction", 4), "Assembly", "Assembly"),
    measure = c(rep("ei_production", 4), rep("atlas_export_share", 3), rep("atlas_export_share", 2),
                rep("ei_production", 3), "uranium_mine_production", "iea_manufacturing_capacity", "iea_manufacturing_capacity"),
    mineral = NA_character_,
    year = 2025L,
    world_share = c(0.03, 0.01, 0.05, 0.5, 0.2, 0.2, 0.6, 0.1, 0.3, 0.02, 0.01, 0.1, 0.0002, 0.1, 0.1),
    is_aggregate = c(FALSE, FALSE, TRUE, FALSE, rep(FALSE, 5), FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
  )
  gdp <- data.frame(iso3c = eu, year = 2025, `NY.GDP.MKTP.CD` = c(6e11, 3e11, 1e11), check.names = FALSE)
  techs <- c("Oil", "Solar", "Gas", "Nuclear", "Wind", "Geothermal")
  w <- eu_stage_weights(supplier, gdp, eu_members = eu, techs = techs, stages = c("Extraction", "Assembly"),
                        min_coverage = 0.5, min_eu_world_share = 0.001)

  expect_equal(nrow(w), length(eu) * length(techs) * 2)
  sums <- tapply(w$weight, paste(w$tech, w$supply_chain), sum)
  expect_true(all(abs(sums - 1) < 1e-12))
  expect_true(all(w$network_node == "EUU"))

  cell <- function(t, s) w[w$tech == t & w$supply_chain == s, ]
  get <- function(x, iso) x$weight[x$iso3 == iso]
  oil <- cell("Oil", "Extraction")
  expect_equal(unique(oil$basis), "production") # production beats exports
  expect_equal(get(oil, "AAA"), 0.75)
  expect_equal(get(oil, "CCC"), 0) # not listed by the production source
  expect_equal(unique(oil$coverage), 0.8)
  solar <- cell("Solar", "Assembly")
  expect_equal(unique(solar$basis), "exports")
  expect_equal(get(solar, "CCC"), 0.75)
  # Gas: members cover 30% < 50% of the EU total -> no production basis, no exports -> GDP.
  expect_equal(unique(cell("Gas", "Extraction")$basis), "gdp")
  expect_equal(get(cell("Gas", "Extraction"), "AAA"), 0.6)
  # Nuclear: the EU holds 0.02% of world output -> GDP.
  expect_equal(unique(cell("Nuclear", "Extraction")$basis), "gdp")
  # Wind: the manufacturing measure is not a weight basis -> GDP; Geothermal has no data -> GDP.
  expect_equal(unique(cell("Wind", "Assembly")$basis), "gdp")
  expect_equal(unique(cell("Geothermal", "Assembly")$basis), "gdp")
  expect_equal(unique(cell("Geothermal", "Assembly")$measures), "wdi_gdp")

  # By default every supply stage gets weights (the stages the allied network solves); a stage
  # without a usable measure (here Refining, Components, Recovery) falls to GDP.
  all5 <- eu_stage_weights(supplier, gdp, eu_members = eu, techs = techs)
  expect_setequal(unique(all5$supply_chain), SUPPLY_CHAIN_SUPPLY_STAGES)
  expect_equal(nrow(all5), length(eu) * length(techs) * 5)
  expect_equal(unique(all5$basis[all5$supply_chain %in% c("Refining", "Components", "Recovery")]), "gdp")
  # Three-stage labels on either side stop instead of putting every cell on GDP.
  expect_error(eu_stage_weights(supplier, gdp, eu_members = eu, techs = techs, stages = c("Upstream", "Midstream")),
               "not supply-chain stages: Upstream, Midstream")
  legacy <- supplier
  legacy$supply_chain[legacy$supply_chain == "Extraction"] <- "Upstream"
  expect_error(eu_stage_weights(legacy, gdp, eu_members = eu, techs = techs), "supplier_tbl\\$supply_chain")
})

test_that("every supplier measure carries the stage the themes give the same data", {
  coalition_test_load()
  expect_equal(unname(COALITION_CLEANTECH_STAGES[c("Batteries", "Solar", "Wind", "Green Hydrogen")]),
               c("Components", "Assembly", "Assembly", "Assembly"))
  expect_true(all(COALITION_CLEANTECH_STAGES %in% SUPPLY_CHAIN_MANUFACTURING_STAGES))
  # IEA manufacturing capacity: battery cell capacity is Components, the end products Assembly.
  cleantech <- data.frame(X = c("China", "Viet Nam", "Korea", "EU", "Other"),
                          `Solar - Current` = c(80, 5, 0, 0, 15), `Batteries - Current` = c(75, 0, 10, 5, 10),
                          `Wind - Current` = c(60, 0, 0, 20, 20), `Electrolyzers - Current` = c(50, 0, 0, 25, 25),
                          check.names = FALSE)
  ct <- coalition_shares_cleantech(cleantech)
  stage_of <- unique(ct[c("tech", "supply_chain")])
  expect_equal(stage_of$supply_chain[match(c("Solar", "Batteries", "Wind", "Green Hydrogen"), stage_of$tech)],
               c("Assembly", "Components", "Assembly", "Assembly"))
  expect_error(coalition_shares_cleantech(cleantech, tech_stages = c(Solar = "Assembly")), "no stage in tech_stages for Wind")

  # IEA mineral supply: mining Extraction, refining Refining; the measure follows the stage.
  critical <- data.frame(
    Pillar = "2 Total supply for key minerals",
    Mineral = c("Lithium - Mining", "Lithium - Mining", "Lithium - Chemical", "Lithium - Chemical"),
    Sector.Country = c("Australia", "Rest of world", "Chile", "Rest of world"), Scenario = "Base case",
    X2025 = c(40, 60, 30, 70), X2035 = c(40, 60, 30, 70), stringsAsFactors = FALSE
  )
  demand <- data.frame(Mineral = "Lithium", tech = "Batteries", share_24 = 0.9, stringsAsFactors = FALSE)
  m <- coalition_shares_iea_minerals(critical, demand, techs = "Batteries")
  expect_equal(m$supply_chain[match(c("AUS", "CHL"), m$iso3)], c("Extraction", "Refining"))
  expect_equal(m$measure[match(c("AUS", "CHL"), m$iso3)], c("iea_mineral_mining", "iea_mineral_refining"))

  # Fuel and uranium production are Extraction.
  ei <- data.frame(Country = c("Total World", "US"), Year = 2025, ISO3166_alpha3 = c("WLD", "USA"),
                   Var = "gasprod_ej", Value = c(100, 25), stringsAsFactors = FALSE)
  expect_equal(unique(coalition_shares_ei_production(ei, vars = c(Gas = "gasprod_ej"))$supply_chain), "Extraction")
  u <- data.frame(country = "Niger", iso3c = "NER", variable = "mine_production", value = 5, year = 2024)
  expect_equal(coalition_shares_uranium(u)$supply_chain, "Extraction")
})

test_that("export shares are read in the trade table's six-stage cells, and a three-stage table stops", {
  coalition_test_load()
  # One technology's old Midstream basket split into Refining, Components and Assembly cells: AAA
  # holds 6% of the Components basket though (say) under 5% of the whole.
  trade <- data.frame(
    Country = "A", iso3c = c("AAA", "AAA", "AAA", "BBB", "AAA"), tech = "Batteries",
    supply_chain = c("Refining", "Components", "Assembly", "Assembly", "Deployment"), sub_sector = "All",
    variable = "market_share", data_type = "raw", value = c(0.02, 0.06, 0.03, 0.30, 0.9), Year = 2024,
    stringsAsFactors = FALSE
  )
  s <- coalition_shares_trade(trade, techs = "Batteries")
  expect_setequal(s$supply_chain, c("Refining", "Components", "Assembly")) # Deployment is not a supply stage
  expect_equal(s$world_share[s$iso3 == "AAA" & s$supply_chain == "Components"], 0.06)
  t <- major_supplier_shares(trade_tbl = trade, techs = "Batteries")
  expect_equal(t$supply_chain[t$major], c("Assembly", "Components"))
  expect_equal(t$iso3[t$major], c("BBB", "AAA"))
  # A three-stage trade table (legacy_stages = TRUE, or a pre-2026-10-26 build) matches no stage:
  # it stops instead of returning no rows, and so do three-stage stages.
  legacy <- trade
  legacy$supply_chain <- c("Midstream", "Midstream", "Midstream", "Midstream", "Downstream")
  expect_error(coalition_shares_trade(legacy, techs = "Batteries"), "not supply-chain stages: Midstream, Downstream")
  expect_error(major_supplier_shares(trade_tbl = trade, techs = "Batteries", stages = c("Upstream", "Midstream")),
               "major_supplier_shares\\(\\): stages")
})

test_that("major_supplier_shares computes world shares from each source", {
  coalition_test_load()
  critical <- data.frame(
    Pillar = "2 Total supply for key minerals",
    Mineral = c(rep("Copper - Mining", 4), rep("Copper - Refining", 3)),
    Sector.Country = c("Chile", "Peru", "Rest of world", "Total", "China", "Japan", "Rest of world"),
    Scenario = "Base case",
    X2025 = c(25, 5, 70, 100, 50, 5, 45),
    X2035 = c(25, 5, 70, 100, 50, 5, 45),
    stringsAsFactors = FALSE
  )
  demand <- data.frame(Mineral = "Copper", tech = c("Electric Grid", "Green Hydrogen", "Heat Pumps"),
                       share_24 = c(0.6, 1e-6, 0.3), stringsAsFactors = FALSE)
  ei <- data.frame(
    Country = c("Total World", "Total EU", "US", "Italy", "Other Europe"),
    Year = 2025, ISO3166_alpha3 = c("WLD", "T-EU", "USA", "ITA", "O-EUR"),
    Var = "oilprod_kbd", Value = c(1000, 10, 200, 6, 4), stringsAsFactors = FALSE
  )
  cleantech <- data.frame(X = c("China", "Viet Nam", "EU", "US", "Other"),
                          `Solar - Current` = c(79, 5, 0, 0, 16), check.names = FALSE)
  trade <- data.frame(
    Country = c("Chile", "Chile", "Chile"), iso3c = "CHL", tech = c("Batteries", "Batteries", "Semiconductors"),
    supply_chain = c("Extraction", "Extraction", "Components"), sub_sector = "All",
    variable = c("market_share", "exports", "market_share"), data_type = "raw",
    value = c(0.14, 1e9, 0.2), Year = 2024, source = "Harvard Atlas of Economic Complexity",
    explanation = "Share of world exports of the basket's HS6 codes (Atlas) [2023]", stringsAsFactors = FALSE
  )
  techs <- c("Electric Grid", "Green Hydrogen", "Oil", "Solar", "Batteries")
  s <- major_supplier_shares(critical = critical, mineral_demand = demand, ei = ei,
                             cleantech_midstream = cleantech, trade_tbl = trade, techs = techs,
                             ei_vars = c(Oil = "oilprod_kbd"), cleantech_tech_map = c(Solar = "Solar"))

  share <- function(i, t, st, m) s$world_share[s$iso3 == i & s$tech == t & s$supply_chain == st & s$measure == m]
  expect_equal(share("CHL", "Electric Grid", "Extraction", "iea_mineral_mining"), 0.25)
  expect_equal(share("JPN", "Electric Grid", "Refining", "iea_mineral_refining"), 0.05)
  expect_false(any(s$tech == "Green Hydrogen")) # below mineral_min_demand_share
  expect_false(any(s$tech == "Heat Pumps"))     # not a headline technology
  expect_equal(share("USA", "Oil", "Extraction", "ei_production"), 0.2)
  expect_true(s$is_aggregate[s$iso3 == "EUU" & s$measure == "ei_production"])
  expect_equal(share("EUU", "Oil", "Extraction", "ei_production"), 0.01)
  expect_equal(share("VNM", "Solar", "Assembly", "iea_manufacturing_capacity"), 0.05)
  expect_true(s$is_aggregate[s$iso3 == "EUU" & s$measure == "iea_manufacturing_capacity"])
  expect_false(any(grepl("Other|Rest", s$country)))
  expect_equal(share("CHL", "Batteries", "Extraction", "atlas_export_share"), 0.14)
  expect_equal(s$year[s$measure == "atlas_export_share"], 2023L)
  expect_false(any(s$tech == "Semiconductors"))
  # The test passes at the threshold and never on an aggregate row.
  expect_true(s$major[s$iso3 == "VNM"])
  expect_true(s$major[s$iso3 == "JPN" & s$tech == "Electric Grid"])
  expect_false(any(s$major[s$is_aggregate]))
  expect_equal(anyDuplicated(s[c("iso3", "tech", "supply_chain", "measure", "mineral")]), 0)
})

test_that("EI mineral sheets test the minerals the IEA supply sheet does not cover", {
  coalition_test_load()
  # An EI "P-R" sheet as readxl reads it with skip = 2: the latest year appears twice (level, then
  # growth rate); the leftmost is the level.
  ei_sheet <- function(unit, labels, v2024, v2025) {
    d <- tibble::tibble(a = c(NA, labels, "Sources: includes data from US Geological Survey"),
                        b = c(NA, v2024, NA), c = c(NA, v2025, NA), d = c(NA, rep(0.01, length(labels)), NA))
    names(d) <- c(unit, "2024", "2025...3", "2025...4")
    d
  }
  sheets <- list(
    list(sheet = "Manganese P-R", mineral = "Manganese", nm_col = "Thousand tonnes",
         data = ei_sheet("Thousand tonnes", c("Australia", "South Africa", "Gabon", "Rest of World", "Total World"),
                         c(1, 1, 1, 1, 4), c(80, 380, 250, 290, 1000))),
    list(sheet = "PGM", mineral = "PGMs", nm_col = "Kilogram",
         data = ei_sheet("Kilogram", c("US", "South Africa", "Rest of World", "Total World"),
                         c(1, 1, 1, 3), c(20, 520, 460, 1000))),
    # Copper is on the IEA supply sheet below, so its EI sheet is not read.
    list(sheet = "Copper P-R", mineral = "Copper", nm_col = "Thousand tonnes",
         data = ei_sheet("Thousand tonnes", c("Zambia", "Total World"), c(1, 10), c(900, 1000)))
  )
  demand <- data.frame(
    Mineral = c("Manganese", "Manganese", "Manganese", "PGMs", "Copper"),
    tech = c("Wind", "Batteries", "Heat Pumps", "Green Hydrogen", "Electric Grid"),
    share_24 = c(0.6, 0.04, 0.3, 1, 0.6), stringsAsFactors = FALSE
  )
  critical <- data.frame(
    Pillar = "2 Total supply for key minerals", Mineral = "Copper - Mining",
    Sector.Country = c("Chile", "Rest of world"), Scenario = "Base case",
    X2025 = c(25, 75), X2035 = c(25, 75), stringsAsFactors = FALSE
  )
  techs <- c("Wind", "Batteries", "Green Hydrogen", "Electric Grid")
  s <- major_supplier_shares(critical = critical, mineral_demand = demand, ei_minerals = sheets, techs = techs)
  e <- s[s$measure == "ei_mineral_mining", ]
  share <- function(i, t, m) e$world_share[e$iso3 == i & e$tech == t & e$mineral == m]

  # Shares of the "Total World" row in the latest year; each linked technology, Extraction.
  expect_equal(share("ZAF", "Wind", "Manganese"), 0.38)
  expect_equal(share("GAB", "Batteries", "Manganese"), 0.25)
  expect_equal(share("ZAF", "Green Hydrogen", "PGMs"), 0.52)
  expect_equal(share("USA", "Green Hydrogen", "PGMs"), 0.02)
  expect_equal(unique(e$year), 2025L)
  expect_equal(unique(e$supply_chain), "Extraction")
  expect_false(any(e$tech == "Heat Pumps"))
  expect_false(any(grepl("Rest|Total|Sources", e$country)))
  # The value and its share stay on the same row.
  expect_equal(e$value[e$iso3 == "GAB" & e$tech == "Wind"], 250)
  # Copper comes from the IEA sheet only.
  expect_false(any(e$mineral == "Copper"))
  expect_false("ZMB" %in% s$iso3)
  expect_equal(s$world_share[s$iso3 == "CHL" & s$measure == "iea_mineral_mining"], 0.25)
  expect_true(all(e$major[e$iso3 %in% c("ZAF", "GAB", "AUS")]))
  expect_false(any(e$major[e$iso3 == "USA"]))

  # A Nonaligned manganese miner below the GDP floor becomes a partner through the supplier route.
  tiers <- derive_coalition_tiers(
    bloc_tbl = data.frame(iso3 = c("ZAF", "GAB", "USA"), bloc = c("Nonaligned", "Nonaligned", "U.S.-leaning"),
                          alignment_basis = "own_votes", stringsAsFactors = FALSE),
    gdp_tbl = data.frame(iso3c = c("ZAF", "GAB", "USA"), year = 2025, `NY.GDP.MKTP.CD` = c(4e11, 2e10, 3e13), check.names = FALSE),
    agreements_tbl = data.frame(iso3 = "USA", agreement = "NATO", stringsAsFactors = FALSE),
    supplier_tbl = s
  )
  expect_equal(tiers$tier[match(c("ZAF", "GAB", "USA"), tiers$iso3)], c("partner", "partner", "core"))
  expect_match(tiers$supplier_cells[tiers$iso3 == "GAB"], "ei_mineral_mining Manganese 25.0%", fixed = TRUE)

  # A sheet without a usable Total World row stops rather than inventing a denominator.
  bad <- sheets[1]
  bad[[1]]$data[[1]][bad[[1]]$data[[1]] %in% "Total World"] <- "World"
  expect_error(coalition_shares_ei_minerals(bad, demand, techs = techs), "Total World")
})

test_that("reserves rows are tested at supplier_reserves_share, every other measure at supplier_share", {
  coalition_test_load()
  expect_equal(coalition_supplier_threshold(c("usgs_reserves", "ei_reserves", "uranium_resources", "atlas_export_share", "ei_production")),
               c(0.10, 0.10, 0.10, 0.05, 0.05))
  expect_equal(coalition_supplier_threshold("ei_reserves", supplier_share = 0.05, supplier_reserves_share = 0.2), 0.2)
  expect_error(coalition_supplier_threshold("ei_reserves", supplier_reserves_share = 0), "supplier_reserves_share")

  blocs <- data.frame(
    iso3 = c("RRR", "SSS", "TTT", "UUU", "VVV"),
    bloc = c("Nonaligned", "Nonaligned", "Nonaligned", "China-leaning", "Nonaligned"),
    alignment_basis = "own_votes", stringsAsFactors = FALSE
  )
  gdp <- data.frame(iso3c = c("RRR", "SSS", "TTT", "UUU", "VVV"), year = 2025,
                    `NY.GDP.MKTP.CD` = c(2e12, 2e10, 3e10, 5e11, 1e10), check.names = FALSE)
  supplier <- tibble::tibble(
    iso3 = c("RRR", "RRR", "SSS", "TTT", "TTT", "UUU", "VVV"),
    tech = c("Wind", "Oil", "Batteries", "Nuclear", "Nuclear", "Gas", "Oil"),
    supply_chain = "Extraction",
    measure = c("usgs_reserves", "ei_production", "usgs_reserves", "uranium_resources", "uranium_mine_production",
                "ei_reserves", "atlas_export_share"),
    mineral = c("Rare Earths", NA, "Graphite", NA, NA, NA, NA),
    world_share = c(0.146, 0.039, 0.08, 0.10, 0.06, 0.20, 0.06),
    is_aggregate = FALSE
  )
  out <- derive_coalition_tiers(blocs, gdp, data.frame(iso3 = character(), agreement = character()), supplier)
  row <- function(i) out[out$iso3 == i, ]

  # RRR: a reserves holder (14.6%) with 3.9% of production -> partner by the reserves leg alone.
  expect_equal(row("RRR")$tier, "partner")
  expect_equal(row("RRR")$routes, "supplier: reserves")
  expect_true(row("RRR")$route_supplier && row("RRR")$route_supplier_reserves)
  expect_equal(row("RRR")$max_supplier_share, 0.039) # flow measures only, read against supplier_share
  expect_equal(row("RRR")$max_reserves_share, 0.146)
  expect_match(row("RRR")$supplier_cells, "Wind Extraction (usgs_reserves Rare Earths 14.6%)", fixed = TRUE)
  expect_match(row("RRR")$reason, "major reserves holder")
  # SSS: 8% of reserves would pass the flow bar but not the reserves bar.
  expect_equal(row("SSS")$tier, "none")
  expect_true(is.na(row("SSS")$supplier_cells))
  expect_false(row("SSS")$supplier_excluded)
  expect_true(is.na(row("SSS")$max_supplier_share))
  expect_equal(row("SSS")$max_reserves_share, 0.08)
  # TTT: at both bars (inclusive) -> both legs recorded.
  expect_equal(row("TTT")$routes, "supplier; supplier: reserves")
  expect_match(row("TTT")$reason, "major supplier and reserves holder")
  # UUU: China-leaning reserves holder -> excluded, flagged.
  expect_equal(row("UUU")$tier, "none")
  expect_false(row("UUU")$route_supplier_reserves)
  expect_true(row("UUU")$supplier_excluded)
  expect_match(row("UUU")$reason, "major reserves holder (excluded: China-leaning)", fixed = TRUE)
  # VVV: a flow measure keeps the 5% bar.
  expect_equal(row("VVV")$routes, "supplier")
  expect_false(row("VVV")$route_supplier_reserves)

  # Each bar moves on its own.
  low <- derive_coalition_tiers(blocs, gdp, data.frame(iso3 = character(), agreement = character()), supplier,
                                supplier_reserves_share = 0.08)
  expect_equal(low$routes[low$iso3 == "SSS"], "supplier: reserves")
  high <- derive_coalition_tiers(blocs, gdp, data.frame(iso3 = character(), agreement = character()), supplier,
                                 supplier_share = 0.07)
  expect_equal(high$routes[high$iso3 == "RRR"], "supplier: reserves")
  expect_equal(high$routes[high$iso3 == "TTT"], "supplier: reserves")
  expect_equal(high$tier[high$iso3 == "VVV"], "none")
  # One bar for everything (the pre-2026-10-09 rule) admits SSS at 5%.
  flat <- derive_coalition_tiers(blocs, gdp, data.frame(iso3 = character(), agreement = character()), supplier,
                                 supplier_reserves_share = 0.05)
  expect_equal(flat$tier[flat$iso3 == "SSS"], "partner")
})

# EI reserves sheets as readxl reads them with the spec's skip: a fuel sheet (proved reserves
# history, the value column "2020...42") and a mineral "P-R" sheet ("At end of <year>").
coalition_test_reserves_sheets <- function() {
  oil <- tibble::tibble(
    a = c(NA, "Canada", "US", "Total North America", "Venezuela", "Other S. & Cent. America",
          "Total World", "of which: OECD", "Non-OPEC", "European Union#", "Canadian Oil Sands: Total",
          "Venezuela: Orinoco Belt", "# Excludes Estonia."),
    b = c(NA, 160, 60, 220, 290, 20, 980, 250, 400, 4, 150, 250, NA),
    c = c(NA, 170, 70, 240, 300, 30, 1000, 260, 420, 5, 160, 260, NA)
  )
  names(oil) <- c("Thousand million barrels", "2019...41", "2020...42")
  graphite <- tibble::tibble(
    a = c(NA, "China", "Brazil1", "Madagascar", "Rest of World", "Total World", "Sources: USGS"),
    b = c(NA, 1, 1, 1, 1, 4, NA),
    c = c(NA, 100, 74, 27, 29, 400, NA)
  )
  names(graphite) <- c("Thousand tonnes", "2025", "At end of 2025")
  zinc <- graphite
  list(
    list(sheet = "Oil - Proved reserves history", resource = "Oil", nm_col = "Thousand million barrels",
         val_col = "2020...42", data_year = 2020, unit = "Thousand million barrels", data = oil),
    list(sheet = "Natural Graphite P-R", resource = "Graphite", nm_col = "Thousand tonnes",
         val_col = "At end of 2025", unit = "Thousand tonnes", data = graphite),
    # No demand link for zinc in the fixture and no Coal technology: both skipped.
    list(sheet = "Zinc P-R", resource = "Zinc", nm_col = "Thousand tonnes", val_col = "At end of 2025", data = zinc),
    list(sheet = "Coal - Reserves", resource = "Coal", nm_col = "Thousand million barrels", val_col = "2020...42",
         data_year = 2020, data = oil)
  )
}

test_that("EI reserves are shares of each sheet's Total World row, mapped like the mining measures", {
  coalition_test_load()
  demand <- data.frame(Mineral = c("Graphite", "Graphite", "Graphite"),
                       tech = c("Batteries", "Electric Vehicles", "Heat Pumps"),
                       share_24 = c(0.18, 0.82, 0.002), stringsAsFactors = FALSE)
  techs <- c("Batteries", "Electric Vehicles", "Oil")
  r <- coalition_shares_ei_reserves(coalition_test_reserves_sheets(), demand, techs = techs)
  share <- function(i, t) r$world_share[r$iso3 == i & r$tech == t]

  # Oil: country rows over Total World (1000). Regional totals, residuals, groups, the EU (whose
  # label carries a footnote "#") and the "<name>: <part>" component rows are dropped.
  oil <- r[r$tech == "Oil", ]
  expect_setequal(oil$iso3, c("CAN", "USA", "VEN"))
  expect_equal(share("VEN", "Oil"), 0.30)
  expect_equal(share("CAN", "Oil"), 0.17)
  expect_true(all(is.na(oil$mineral)))
  expect_equal(unique(oil$year), 2020L)
  # Graphite: "Brazil1" (a footnote digit) is Brazil; the denominator keeps the Rest of World.
  expect_equal(share("BRA", "Batteries"), 74 / 400)
  expect_equal(share("BRA", "Electric Vehicles"), 74 / 400)
  expect_equal(r$country[r$iso3 == "BRA"][1], "Brazil")
  expect_equal(unique(r$mineral[r$tech == "Batteries"]), "Graphite")
  expect_equal(unique(r$year[r$tech == "Batteries"]), 2025L)
  expect_false(any(r$tech == "Heat Pumps"))           # not a headline technology, and below the floor
  expect_false(any(r$tech %in% c("Coal", "Zinc")))   # Coal not in techs; zinc has no link
  expect_equal(unique(r$measure), "ei_reserves")
  expect_equal(unique(r$supply_chain), "Extraction")
  expect_false(any(grepl("Total|Rest|Other|Union|Sands|:", r$country)))
  expect_equal(anyDuplicated(r[c("iso3", "tech", "mineral")]), 0)

  # Without a Total World row, or when the country rows exceed it, the sheet stops.
  sheets <- coalition_test_reserves_sheets()[2]
  no_world <- sheets
  no_world[[1]]$data[[1]][no_world[[1]]$data[[1]] %in% "Total World"] <- "World total"
  expect_error(coalition_shares_ei_reserves(no_world, demand, techs = techs), "Total World")
  too_big <- sheets
  too_big[[1]]$data[[3]][too_big[[1]]$data[[1]] %in% "Total World"] <- 150
  expect_error(coalition_shares_ei_reserves(too_big, demand, techs = techs), "more than the Total World")
  unknown <- sheets
  unknown[[1]]$data[[1]][unknown[[1]]$data[[1]] %in% "Madagascar"] <- "Atlantis"
  expect_error(coalition_shares_ei_reserves(unknown, demand, techs = techs), "no ISO3 code: Atlantis")
})

test_that("uranium resources are shares of the listed countries in one cost category", {
  coalition_test_load()
  v260 <- "identified_recoverable_resources_lt_usd260_kgu"
  ur <- data.frame(
    country = c("Australia", "Niger", "Brazil", "Australia", "Niger", "Brazil", "Australia"),
    iso3c = c("AUS", "NER", "BRA", "AUS", "NER", "BRA", "AUS"),
    variable = c(rep(v260, 3), rep("identified_recoverable_resources_lt_usd130_kgu", 3), v260),
    value = c(600, 300, 100, 500, 100, 0, 999),
    unit = "tU", year = c(2025, 2025, 2025, 2025, 2025, 2025, 2023),
    stringsAsFactors = FALSE
  )
  u <- coalition_shares_uranium_resources(ur, variable = v260)
  expect_equal(u$world_share[match(c("AUS", "NER", "BRA"), u$iso3)], c(0.6, 0.3, 0.1)) # latest year only
  expect_equal(unique(u$tech), "Nuclear")
  expect_equal(unique(u$measure), "uranium_resources")
  expect_equal(unique(u$year), 2025L)
  expect_error(coalition_shares_uranium_resources(ur, variable = "nope"), "no nope rows")

  # In major_supplier_shares(), each row carries its threshold and the major flag follows it, and
  # derive_coalition_tiers() with the same bars agrees with that flag.
  demand <- data.frame(Mineral = "Graphite", tech = c("Batteries", "Electric Vehicles"),
                       share_24 = c(0.18, 0.82), stringsAsFactors = FALSE)
  s <- major_supplier_shares(
    mineral_demand = demand, ei_reserves = coalition_test_reserves_sheets(), uranium_resources = ur,
    uranium_production = data.frame(country = c("Niger", "Australia"), iso3c = c("NER", "AUS"),
                                     variable = "mine_production", value = c(6, 94), year = 2024),
    techs = c("Batteries", "Electric Vehicles", "Oil", "Nuclear"),
    uranium_resources_variable = v260
  )
  expect_equal(unique(s$threshold[s$measure %in% COALITION_RESERVES_MEASURES]), 0.10)
  expect_equal(unique(s$threshold[!s$measure %in% COALITION_RESERVES_MEASURES]), 0.05)
  major <- function(i, m) unique(s$major[s$iso3 == i & s$measure == m])
  expect_true(major("NER", "uranium_mine_production")) # 6% of production
  expect_true(major("NER", "uranium_resources"))       # 30% of resources
  expect_true(major("BRA", "uranium_resources"))       # exactly 10%: the bar is inclusive
  expect_false(major("MDG", "ei_reserves"))            # 6.75% of graphite reserves: above 5%, below 10%
  expect_true(major("BRA", "ei_reserves"))             # 18.5% of graphite reserves
  expect_false(major("USA", "ei_reserves"))            # 7% of oil reserves

  iso <- c("AUS", "NER", "BRA", "MDG", "USA", "CAN", "VEN", "CHN")
  tiers <- derive_coalition_tiers(
    bloc_tbl = data.frame(iso3 = iso, bloc = c("U.S.-leaning", rep("Nonaligned", 6), "China-leaning"),
                          alignment_basis = "own_votes", stringsAsFactors = FALSE),
    gdp_tbl = data.frame(iso3c = iso, year = 2025, `NY.GDP.MKTP.CD` = 1e9, check.names = FALSE),
    agreements_tbl = data.frame(iso3 = character(), agreement = character()),
    supplier_tbl = s
  )
  flagged <- unique(s$iso3[s$major])
  reserves_flagged <- unique(s$iso3[s$major & s$measure %in% COALITION_RESERVES_MEASURES])
  expect_equal(tiers$route_supplier | tiers$supplier_excluded, tiers$iso3 %in% flagged)
  expect_equal(tiers$route_supplier_reserves, tiers$iso3 %in% reserves_flagged & !tiers$supplier_excluded)
  expect_true(tiers$supplier_excluded[tiers$iso3 == "CHN"]) # 25% of graphite reserves, China-leaning
  expect_equal(tiers$routes[tiers$iso3 == "MDG"], "none")
  expect_equal(tiers$routes[tiers$iso3 == "NER"], "supplier; supplier: reserves")
  expect_equal(tiers$routes[tiers$iso3 == "BRA"], "supplier: reserves")
  expect_error(major_supplier_shares(uranium_resources = ur, techs = "Nuclear"), "uranium_resources_variable")
  expect_error(major_supplier_shares(ei_reserves = coalition_test_reserves_sheets(), techs = "Oil"), "mineral_demand")
})

# The MCS commodity table as scripts/13 reads it (all text), for two minerals.
coalition_test_mcs <- function() {
  rows <- function(commodity, unit, country, value) {
    data.frame(
      Commodity = commodity, Country = country, Statistics = "Reserves",
      Statistics_detail = ifelse(country == "World total", "Reserves: rounded", "Reserves"),
      Unit = unit, Year = "2025", Value = value, Notes = "", stringsAsFactors = FALSE
    )
  }
  rbind(
    rows("Graphite (Natural)", "metric tons",
         c("China", "Brazil", "Madagascar", "Ukraine", "Korea, Republic of", "World total"),
         c("100,000,000", "74,000,000", "27,000,000", "s", "1,800,000", "310,000,000")),
    rows("Nickel", "metric tons",
         c("Indonesia", "Brazil", "New Caledonia", "United States", "Other countries", "World total"),
         c("62,000,000", "16,000,000", "7,100,000", "W", ">9,100,000", ">140,000,000"))
  )
}

coalition_test_usgs <- function() {
  usgs_mcs_reserves(
    coalition_test_mcs(),
    specs = Filter(function(s) s$tech_name %in% c("Graphite", "Nickel"), reserves_usgs_specs())
  )
}

test_that("USGS reserves are shares of the world total, mapped like the mining measures", {
  coalition_test_load()
  demand <- data.frame(Mineral = c("Graphite", "Graphite", "Graphite", "Nickel", "Nickel"),
                       tech = c("Batteries", "Electric Vehicles", "Heat Pumps", "Electric Vehicles", "Wind"),
                       share_24 = c(0.18, 0.82, 0.002, 0.9, 0.005), stringsAsFactors = FALSE)
  techs <- c("Batteries", "Electric Vehicles", "Wind")
  r <- coalition_shares_usgs_reserves(coalition_test_usgs(), demand, techs = techs)
  share <- function(i, t, m) r$world_share[r$iso3 == i & r$tech == t & r$mineral == m]

  expect_equal(unique(r$measure), "usgs_reserves")
  expect_equal(unique(r$supply_chain), "Extraction")
  expect_equal(unique(r$year), 2025L)
  # Graphite: the World total (310 Mt) holds Ukraine's unquantified reserves, so it is the world.
  expect_equal(share("BRA", "Batteries", "Graphite"), 74 / 310)
  expect_equal(share("MDG", "Electric Vehicles", "Graphite"), 27 / 310)
  # Nickel: the listed rows (62 + 16 + 7.1 + 9.1 = 94.2 Mt) sit under the ">140 Mt" floor.
  expect_equal(share("BRA", "Electric Vehicles", "Nickel"), 16 / 140)
  expect_equal(share("NCL", "Electric Vehicles", "Nickel"), 7.1 / 140)
  # Holders USGS does not quantify have no share; aggregates are never holders.
  expect_false(any(r$iso3 %in% c("UKR", "USA")))
  expect_false(anyNA(r$iso3))
  # The demand floor (1%) drops Heat Pumps (0.2% of graphite) and Wind (0.5% of nickel).
  expect_false(any(r$tech == "Heat Pumps"))
  expect_false(any(r$tech == "Wind"))
  expect_equal(unique(r$unit), "Thousand tonnes")
  expect_match(r$measure_label[1], "^USGS Mineral Commodity Summaries 2026: (Graphite|Nickel) reserves at end-2025")
  expect_equal(anyDuplicated(r[c("iso3", "tech", "mineral")]), 0)

  # In major_supplier_shares(): fuels from EI, minerals from USGS, each at the reserves bar.
  fuels_only <- Filter(function(sh) sh$resource %in% c("Oil", "Gas", "Coal"), coalition_test_reserves_sheets())
  s <- major_supplier_shares(mineral_demand = demand, ei_reserves = fuels_only, usgs_reserves = coalition_test_usgs(),
                             techs = c(techs, "Oil"))
  major <- function(i, m) unique(s$major[s$iso3 == i & s$measure == m])
  expect_equal(unique(s$threshold), 0.10)
  expect_true(major("BRA", "usgs_reserves"))   # graphite 23.9%, nickel 11.4%
  expect_false(major("MDG", "usgs_reserves"))  # graphite 8.7%: below the reserves bar
  expect_false(major("NCL", "usgs_reserves"))  # nickel 5.1%
  expect_true(major("VEN", "ei_reserves"))     # oil 30%
  # The same mineral from both sources would be tested twice: stop.
  expect_error(
    major_supplier_shares(mineral_demand = demand, ei_reserves = coalition_test_reserves_sheets(),
                          usgs_reserves = coalition_test_usgs(), techs = techs),
    "both usgs_reserves and ei_reserves"
  )
  expect_error(major_supplier_shares(usgs_reserves = coalition_test_usgs(), techs = techs), "mineral_demand")
})

test_that("the agreements reference file is sourced and matches config/coalition.yml", {
  root <- coalition_test_root()
  agreements <- utils::read.csv(file.path(root, "data", "reference", "economic_security_agreements.csv"),
                                stringsAsFactors = FALSE, na.strings = "")
  expect_true(all(c("iso3", "country", "agreement", "membership_status", "since", "source", "url", "accessed") %in% names(agreements)))
  expect_false(anyNA(agreements[c("iso3", "country", "agreement", "membership_status", "source", "url", "accessed")]))
  expect_true(all(grepl("^[A-Z]{3}$", agreements$iso3)))
  expect_true(all(grepl("^https://", agreements$url)))
  expect_equal(anyDuplicated(agreements[c("iso3", "agreement")]), 0)
  expect_true("EUU" %in% agreements$iso3[agreements$agreement == "MSP"])
  expect_equal(sum(agreements$agreement == "U.S. FTA"), 20)
  expect_equal(sum(agreements$agreement == "NATO"), 32)
  # Official rosters re-checked 2026-10-08 (MTI Singapore; EC DG Trade, 2024-09-27; Global
  # Affairs Canada): IPEF 14 partners; MSP 14 countries and the EU; MSP Forum 8 (July 2024) + 7
  # (Sept 2024); G7 seven members and the EU.
  expect_equal(sum(agreements$agreement == "IPEF"), 14)
  expect_equal(sum(agreements$agreement == "MSP"), 15)
  expect_equal(sum(agreements$agreement == "MSP Forum"), 15)
  expect_equal(sort(agreements$iso3[agreements$agreement == "G7"]),
               c("CAN", "DEU", "EUU", "FRA", "GBR", "ITA", "JPN", "USA"))

  cfg <- yaml::read_yaml(file.path(root, "config", "coalition.yml"))
  expect_true(all(c(unlist(cfg$coalition$partner_agreements), unlist(cfg$coalition$sensitivity_agreements)) %in% agreements$agreement))
  expect_false("MSP Forum" %in% unlist(cfg$coalition$partner_agreements))
  # Reserves count at their own, higher bar (user decision, 2026-10-09).
  expect_equal(cfg$coalition$supplier_share, 0.05)
  expect_equal(cfg$coalition$supplier_reserves_share, 0.10)
  expect_true(isTRUE(cfg$supplier_measures$reserves))
  expect_length(unique(unlist(cfg$eu_node$members)), 27)
  expect_length(unlist(cfg$headline_techs), 11)
  # The supplier test reads the five supply stages (every stage but Deployment), in stage order.
  source(file.path(root, "R", "utils", "supply_chain_stages.R"), local = TRUE)
  expect_equal(unlist(cfg$stages), SUPPLY_CHAIN_SUPPLY_STAGES)
})
