# Two-tier coalition and EU node (R/indices/coalition.R, scripts/13_build_coalition.R).

coalition_test_root <- function() {
  normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
}

coalition_test_load <- function() {
  root <- coalition_test_root()
  source(file.path(root, "R", "utils", "country.R"), local = FALSE)
  source(file.path(root, "R", "utils", "iea_critical_minerals.R"), local = FALSE)
  source(file.path(root, "R", "categories", "production", "critical_minerals_production.R"), local = FALSE)
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
    supply_chain = c("Upstream", "Midstream", "Upstream", "Upstream", "Upstream", "Upstream", "Midstream", "Midstream", "Upstream"),
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
  expect_match(f$supplier_cells, "Batteries Upstream")
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
    iso3 = c("AAA", "BBB", "EUU", "XXX",   # Oil Upstream production (EI-like, with an EU total)
             "AAA", "BBB", "CCC",          # Oil Upstream exports (lower priority)
             "AAA", "CCC",                 # Solar Midstream exports
             "AAA", "BBB", "EUU",          # Gas Upstream production: members cover 30% of the EU total
             "AAA",                        # Nuclear Upstream: EU share below min_eu_world_share
             "AAA", "BBB"),                # Wind Midstream: a measure not in basis_priority
    tech = c("Oil", "Oil", "Oil", "Oil", "Oil", "Oil", "Oil", "Solar", "Solar", "Gas", "Gas", "Gas", "Nuclear", "Wind", "Wind"),
    supply_chain = c(rep("Upstream", 7), "Midstream", "Midstream", rep("Upstream", 4), "Midstream", "Midstream"),
    measure = c(rep("ei_production", 4), rep("atlas_export_share", 3), rep("atlas_export_share", 2),
                rep("ei_production", 3), "uranium_mine_production", "iea_manufacturing_capacity", "iea_manufacturing_capacity"),
    mineral = NA_character_,
    year = 2025L,
    world_share = c(0.03, 0.01, 0.05, 0.5, 0.2, 0.2, 0.6, 0.1, 0.3, 0.02, 0.01, 0.1, 0.0002, 0.1, 0.1),
    is_aggregate = c(FALSE, FALSE, TRUE, FALSE, rep(FALSE, 5), FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
  )
  gdp <- data.frame(iso3c = eu, year = 2025, `NY.GDP.MKTP.CD` = c(6e11, 3e11, 1e11), check.names = FALSE)
  techs <- c("Oil", "Solar", "Gas", "Nuclear", "Wind", "Geothermal")
  w <- eu_stage_weights(supplier, gdp, eu_members = eu, techs = techs, stages = c("Upstream", "Midstream"),
                        min_coverage = 0.5, min_eu_world_share = 0.001)

  expect_equal(nrow(w), length(eu) * length(techs) * 2)
  sums <- tapply(w$weight, paste(w$tech, w$supply_chain), sum)
  expect_true(all(abs(sums - 1) < 1e-12))
  expect_true(all(w$network_node == "EUU"))

  cell <- function(t, s) w[w$tech == t & w$supply_chain == s, ]
  get <- function(x, iso) x$weight[x$iso3 == iso]
  oil <- cell("Oil", "Upstream")
  expect_equal(unique(oil$basis), "production") # production beats exports
  expect_equal(get(oil, "AAA"), 0.75)
  expect_equal(get(oil, "CCC"), 0) # not listed by the production source
  expect_equal(unique(oil$coverage), 0.8)
  solar <- cell("Solar", "Midstream")
  expect_equal(unique(solar$basis), "exports")
  expect_equal(get(solar, "CCC"), 0.75)
  # Gas: members cover 30% < 50% of the EU total -> no production basis, no exports -> GDP.
  expect_equal(unique(cell("Gas", "Upstream")$basis), "gdp")
  expect_equal(get(cell("Gas", "Upstream"), "AAA"), 0.6)
  # Nuclear: the EU holds 0.02% of world output -> GDP.
  expect_equal(unique(cell("Nuclear", "Upstream")$basis), "gdp")
  # Wind: the manufacturing measure is not a weight basis -> GDP; Geothermal has no data -> GDP.
  expect_equal(unique(cell("Wind", "Midstream")$basis), "gdp")
  expect_equal(unique(cell("Geothermal", "Midstream")$basis), "gdp")
  expect_equal(unique(cell("Geothermal", "Midstream")$measures), "wdi_gdp")
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
    supply_chain = c("Upstream", "Upstream", "Midstream"), sub_sector = "All",
    variable = c("market_share", "exports", "market_share"), data_type = "raw",
    value = c(0.14, 1e9, 0.2), Year = 2024, source = "Harvard Atlas of Economic Complexity",
    explanation = "Share of world exports of the basket's HS6 codes (Atlas) [2023]", stringsAsFactors = FALSE
  )
  techs <- c("Electric Grid", "Green Hydrogen", "Oil", "Solar", "Batteries")
  s <- major_supplier_shares(critical = critical, mineral_demand = demand, ei = ei,
                             cleantech_midstream = cleantech, trade_tbl = trade, techs = techs,
                             ei_vars = c(Oil = "oilprod_kbd"), cleantech_tech_map = c(Solar = "Solar"))

  share <- function(i, t, st, m) s$world_share[s$iso3 == i & s$tech == t & s$supply_chain == st & s$measure == m]
  expect_equal(share("CHL", "Electric Grid", "Upstream", "iea_mineral_mining"), 0.25)
  expect_equal(share("JPN", "Electric Grid", "Midstream", "iea_mineral_refining"), 0.05)
  expect_false(any(s$tech == "Green Hydrogen")) # below mineral_min_demand_share
  expect_false(any(s$tech == "Heat Pumps"))     # not a headline technology
  expect_equal(share("USA", "Oil", "Upstream", "ei_production"), 0.2)
  expect_true(s$is_aggregate[s$iso3 == "EUU" & s$measure == "ei_production"])
  expect_equal(share("EUU", "Oil", "Upstream", "ei_production"), 0.01)
  expect_equal(share("VNM", "Solar", "Midstream", "iea_manufacturing_capacity"), 0.05)
  expect_true(s$is_aggregate[s$iso3 == "EUU" & s$measure == "iea_manufacturing_capacity"])
  expect_false(any(grepl("Other|Rest", s$country)))
  expect_equal(share("CHL", "Batteries", "Upstream", "atlas_export_share"), 0.14)
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

  # Shares of the "Total World" row in the latest year; each linked technology, Upstream.
  expect_equal(share("ZAF", "Wind", "Manganese"), 0.38)
  expect_equal(share("GAB", "Batteries", "Manganese"), 0.25)
  expect_equal(share("ZAF", "Green Hydrogen", "PGMs"), 0.52)
  expect_equal(share("USA", "Green Hydrogen", "PGMs"), 0.02)
  expect_equal(unique(e$year), 2025L)
  expect_equal(unique(e$supply_chain), "Upstream")
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
  expect_length(unique(unlist(cfg$eu_node$members)), 27)
  expect_length(unlist(cfg$headline_techs), 11)
})
