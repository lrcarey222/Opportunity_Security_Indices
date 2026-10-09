# Tests for the Strategic Index and the allied sector pick (R/indices/build_indices.R).
repo_root <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "indices", "build_indices.R"))

si_techs <- c("Solar", "Wind", "Coal", "Geothermal")
si_stages <- c("Upstream", "Midstream", "Downstream")

# Two countries x 4 techs x 3 stages with distinct, reproducible pillar values.
si_fixture <- function() {
  keys <- tidyr::crossing(Country = c("A", "B"), tech = si_techs, supply_chain = si_stages)
  n <- nrow(keys)
  list(
    eo = keys %>% dplyr::mutate(Economic_Opportunity_Index = seq(0.20, 0.80, length.out = n)),
    es = keys %>% dplyr::mutate(Energy_Security_Index = rev(seq(0.10, 0.90, length.out = n))),
    # 7 and 25 are coprime, so the 24 policy values are distinct (no rank ties).
    pol = keys %>% dplyr::mutate(value = ((seq_len(n) * 7) %% 25) / 25),
    ghg = tibble::tibble(tech = si_techs, ghg_index = c(0.95, 0.99, 0, 0.9))
  )
}

build_fixture_si <- function(fx = si_fixture(), ...) {
  build_strategic_index(
    economic_opportunity_index = fx$eo,
    energy_security_index = fx$es,
    policy_index = fx$pol,
    tech_ghg = fx$ghg,
    ...
  )
}

test_that("the SI is 0.2 pol + 0.3 EO + 0.3 (1 - ES) + 0.1 stage + 0.1 climate", {
  si <- build_fixture_si() %>% dplyr::filter(!is.na(.data$strategic_index))

  expected <- 0.2 * si$pol +
    0.3 * si$Economic_Opportunity_Index +
    0.3 * (1 - si$Energy_Security_Index) +
    0.1 * si$sc_weight +
    0.1 * si$tech_weight
  expect_equal(si$strategic_index, expected)
  expect_equal(unname(STRATEGIC_INDEX_WEIGHTS), c(0.2, 0.3, 0.3, 0.1, 0.1))
  expect_equal(sum(STRATEGIC_INDEX_WEIGHTS), 1)
  expect_equal(unname(si$sc_weight[si$supply_chain == "Midstream"][1]), 0.75)
  expect_equal(unname(si$sc_weight[si$supply_chain == "Upstream"][1]), 0.50)
  expect_equal(unname(si$sc_weight[si$supply_chain == "Downstream"][1]), 0.25)

  # The contributions reproduce the index exactly.
  contrib <- strategic_index_contributions(si)
  total <- contrib$contrib_policy + contrib$contrib_eo + contrib$contrib_es_need +
    contrib$contrib_sc + contrib$contrib_ghg
  expect_equal(total, si$strategic_index)
})

test_that("Energy_Security_Index keeps its security meaning; the SI uses the need", {
  fx <- si_fixture()
  si <- build_fixture_si(fx)
  joined <- si %>%
    dplyr::inner_join(
      fx$es %>% dplyr::rename(es_input = "Energy_Security_Index"),
      by = c("Country", "tech", "supply_chain")
    )

  expect_equal(joined$Energy_Security_Index, joined$es_input)
  expect_equal(joined$energy_security_need, 1 - joined$es_input)

  # Raising a cell's security, all else equal, lowers its SI by 0.3 per unit.
  bumped <- fx
  hit <- bumped$es$Country == "A" & bumped$es$tech == "Solar" & bumped$es$supply_chain == "Upstream"
  bumped$es$Energy_Security_Index[hit] <- bumped$es$Energy_Security_Index[hit] + 0.05
  si_bumped <- build_fixture_si(bumped)
  pick <- function(x) x$strategic_index[x$Country == "A" & x$tech == "Solar" & x$supply_chain == "Upstream"]
  expect_equal(pick(si_bumped) - pick(si), -0.3 * 0.05)

  # The within-country S-curve of the need is the mirror of the S-curve of security.
  a <- si %>% dplyr::filter(.data$Country == "A", !is.na(.data$strategic_index))
  expect_equal(a$es_need, 1 - median_scurve(a$Energy_Security_Index))
  expect_equal(a$es, a$es_need)
})

test_that("cells with a missing pillar or policy value are dropped, not imputed", {
  fx <- si_fixture()
  miss_es <- fx$es$Country == "A" & fx$es$tech == "Wind" & fx$es$supply_chain == "Upstream"
  fx$es$Energy_Security_Index[miss_es] <- NA
  fx$eo <- fx$eo %>%
    dplyr::filter(!(.data$Country == "A" & .data$tech == "Coal" & .data$supply_chain == "Midstream")) %>%
    dplyr::bind_rows(tibble::tibble(
      Country = "A", tech = "Coal", supply_chain = "Midstream", Economic_Opportunity_Index = NA_real_
    ))
  fx$pol <- fx$pol %>%
    dplyr::filter(!(.data$Country == "A" & .data$tech == "Solar" & .data$supply_chain == "Downstream"))

  si <- build_fixture_si(fx)
  cell <- function(tech, sc) si[si$Country == "A" & si$tech == tech & si$supply_chain == sc, ]

  expect_true(is.na(cell("Wind", "Upstream")$strategic_index))
  expect_equal(cell("Wind", "Upstream")$si_exclusion, "missing energy security")
  expect_true(is.na(cell("Coal", "Midstream")$strategic_index))
  expect_equal(cell("Coal", "Midstream")$si_exclusion, "missing economic opportunity")
  expect_true(is.na(cell("Solar", "Downstream")$strategic_index))
  expect_equal(cell("Solar", "Downstream")$si_exclusion, "missing policy index")

  # The within-country policy ranks are taken over the scored cells only.
  a <- si %>% dplyr::filter(.data$Country == "A", !is.na(.data$strategic_index))
  expect_equal(a$pol, median_scurve(a$value))
  expect_false(any(is.na(a$pol)))
})

test_that("countries without a Policy index get no SI and say why", {
  fx <- si_fixture()
  fx$pol <- fx$pol %>% dplyr::filter(.data$Country == "A")

  si <- build_fixture_si(fx)
  b <- si %>% dplyr::filter(.data$Country == "B")

  expect_true(all(is.na(b$strategic_index)))
  expect_true(all(b$si_exclusion == "country not policy-eligible"))

  # eo and es_need are not in the SI and feed the charts' EO vs ES-risk views, so they stay
  # defined (ranked over the country's observed pillar values) without a Policy index.
  expect_equal(b$eo, median_scurve(b$Economic_Opportunity_Index))
  expect_equal(b$es_need, median_scurve(b$energy_security_need))
  expect_true(all(is.na(b$pol)))
})

test_that("the policy basis is carried onto the SI rows and the allied picks, values unchanged", {
  fx <- si_fixture()
  plain <- build_fixture_si(fx)
  # A policy index without policy_basis (built before the PAMS-only fallback) adds no column.
  expect_false("policy_basis" %in% names(plain))

  fx$pol <- fx$pol %>%
    dplyr::mutate(policy_basis = dplyr::if_else(.data$Country == "A", "IEA PAMS + CAT", "IEA PAMS only"))
  si <- build_fixture_si(fx)
  expect_equal(setdiff(names(si), names(plain)), "policy_basis")
  expect_equal(which(names(si) == "policy_basis"), which(names(si) == "value") + 1L)
  expect_true(all(si$policy_basis[si$Country == "A"] == "IEA PAMS + CAT"))
  expect_true(all(si$policy_basis[si$Country == "B"] == "IEA PAMS only"))
  # the basis is a label: the index is the same with or without it
  expect_equal(dplyr::select(si, -"policy_basis"), plain)

  # a country without a Policy index has no basis
  fx$pol <- fx$pol %>% dplyr::filter(.data$Country == "A")
  si_a <- build_fixture_si(fx)
  expect_true(all(is.na(si_a$policy_basis[si_a$Country == "B"])))

  picks <- select_allied_strategic_sectors(si, ally_countries = c("A", "B"))
  expect_equal(sort(picks$policy_basis), c("IEA PAMS + CAT", "IEA PAMS only"))
})

test_that("eo and es_need are ranked over the observed pillar, pol over the scored cells", {
  fx <- si_fixture()
  miss_es <- fx$es$Country == "A" & fx$es$tech == "Wind" & fx$es$supply_chain == "Upstream"
  fx$es$Energy_Security_Index[miss_es] <- NA

  si <- build_fixture_si(fx)
  a <- si %>% dplyr::filter(.data$Country == "A")
  wind_up <- a$tech == "Wind" & a$supply_chain == "Upstream"

  expect_false(is.na(a$eo[wind_up]))
  expect_true(is.na(a$es_need[wind_up]))
  expect_true(is.na(a$pol[wind_up]))
  expect_equal(a$eo, median_scurve(a$Economic_Opportunity_Index))
  scored <- !is.na(a$strategic_index)
  expect_equal(a$pol[scored], median_scurve(a$value[scored]))
})

test_that("Geothermal - Midstream is removed before the within-country S-curves", {
  si <- build_fixture_si()

  expect_false("Geothermal - Midstream" %in% si$sector)
  a <- si %>% dplyr::filter(.data$Country == "A")
  # 4 techs x 3 stages minus one excluded sector, ranked among themselves.
  expect_equal(nrow(a), 11L)
  expect_equal(a$pol, median_scurve(a$value))
  expect_equal(range(a$pol), c(0, 1))
})

test_that("duplicated keys in an input stop the build instead of duplicating rows", {
  fx <- si_fixture()
  fx$es <- dplyr::bind_rows(fx$es, fx$es[1, ])

  expect_error(build_fixture_si(fx), "duplicated Country x tech x supply_chain")
})

test_that("a pillar that is constant across countries within a sector is flagged", {
  fx <- si_fixture()
  flat <- fx$es$tech == "Coal" & fx$es$supply_chain == "Upstream"
  fx$es$Energy_Security_Index[flat] <- 0

  deg <- strategic_index_degenerate_sectors(build_fixture_si(fx))

  expect_equal(deg$sector, "Coal - Upstream")
  expect_true(deg$es_constant)
  expect_equal(deg$es_value, 0)
  expect_false(deg$eo_constant)
  expect_equal(nrow(strategic_index_degenerate_sectors(build_fixture_si())), 0L)
})

test_that("a sector where many countries share one ES value is reported, not excluded", {
  # Three countries; in Coal - Upstream two of them sit at the same ES floor (a uranium-style
  # "no domestic resource" value), the third differs, so the sector is not degenerate.
  keys <- tidyr::crossing(Country = c("A", "B", "C"), tech = si_techs, supply_chain = si_stages)
  n <- nrow(keys)
  fx <- list(
    eo = keys %>% dplyr::mutate(Economic_Opportunity_Index = seq(0.20, 0.80, length.out = n)),
    es = keys %>% dplyr::mutate(Energy_Security_Index = rev(seq(0.10, 0.90, length.out = n))),
    pol = keys %>% dplyr::mutate(value = ((seq_len(n) * 7) %% 37) / 37),
    ghg = tibble::tibble(tech = si_techs, ghg_index = c(0.95, 0.99, 0, 0.9))
  )
  floor_cells <- fx$es$tech == "Coal" & fx$es$supply_chain == "Upstream" & fx$es$Country != "C"
  fx$es$Energy_Security_Index[floor_cells] <- 0.117

  si <- build_fixture_si(fx)
  fl <- strategic_index_es_floor_sectors(si, min_share = 0.5)

  expect_equal(fl$sector, "Coal - Upstream")
  expect_equal(fl$n_countries, 3L)
  expect_equal(fl$n_at_mode, 2L)
  expect_equal(fl$es_mode, 0.117)
  expect_equal(fl$share_at_mode, 2 / 3)
  # a diagnostic only: the sector stays in the SI and is not degenerate
  expect_equal(nrow(strategic_index_degenerate_sectors(si)), 0L)
  expect_true(all(!is.na(si$strategic_index[si$sector == "Coal - Upstream"])))
  # with distinct values everywhere nothing is reported at a majority threshold
  expect_equal(nrow(strategic_index_es_floor_sectors(build_fixture_si(), min_share = 0.75)), 0L)
})

test_that("the allied pick takes the top Upstream/Midstream sector outside Geothermal", {
  si <- tibble::tibble(
    Country = c("A", "A", "A", "A", "B"),
    tech = c("Geothermal", "Solar", "Wind", "Coal", "Wind"),
    supply_chain = c("Upstream", "Downstream", "Midstream", "Upstream", "Upstream"),
    strategic_index = c(0.9, 0.8, 0.6, 0.5, NA)
  )

  out <- select_allied_strategic_sectors(si, ally_countries = c("A", "B"))

  expect_equal(out$Country, "A")
  expect_equal(out$tech, "Wind")
  expect_equal(out$supply_chain, "Midstream")
})

geo_coalition_fixture <- function() {
  tibble::tibble(
    iso3 = c("USA", "jpn ", "CHN", "DEU", "IND"),
    country = c("United States", "Japan", "China", "Germany", "India"),
    member = c(TRUE, TRUE, FALSE, TRUE, FALSE),
    reason = c("member", "member", "other bloc", "member", "non-aligned")
  )
}

test_that("the coalition is the member == TRUE rows of geo_coalition_tbl, sorted", {
  expect_equal(allied_coalition_iso3(geo_coalition_fixture()), c("DEU", "JPN", "USA"))
  # The hand-picked 23-member list is gone from build_indices.R.
  expect_false(exists("STRATEGIC_ALLY_ISO3"))
})

test_that("a malformed coalition table stops instead of returning a partial coalition", {
  co <- geo_coalition_fixture()
  expect_error(allied_coalition_iso3(dplyr::select(co, -"member")), "member")
  expect_error(allied_coalition_iso3(dplyr::mutate(co, member = c(TRUE, NA, FALSE, TRUE, FALSE))), "no NA")
  expect_error(allied_coalition_iso3(dplyr::mutate(co, member = as.character(member))), "TRUE/FALSE")
  expect_error(allied_coalition_iso3(dplyr::mutate(co, iso3 = c("USA", "USA", "CHN", "DEU", "IND"))), "duplicate ISO3")
  expect_error(allied_coalition_iso3(dplyr::mutate(co, iso3 = c("USA", "", "CHN", "DEU", "IND"))), "missing or empty")
  expect_error(allied_coalition_iso3(dplyr::mutate(co, member = FALSE)), "no coalition members")
})

# coalition_tbl.rds as scripts/13_build_coalition.R writes it: one row per country, tier
# core/partner/none, the EU members sharing one network node.
coalition_tbl_fixture <- function() {
  tibble::tibble(
    iso3 = c("USA", "DEU", "FRA", "MEX", "CHN", "JPN", "ind"),
    country = c("United States", "Germany", "France", "Mexico", "China", "Japan", "India"),
    tier = c("core", "core", "Core", "partner", "none", "core", "partner"),
    routes = list("alignment", c("alignment", "NATO"), "alignment", "FTA", character(0), "alignment", c("IPEF", "MSP")),
    bloc = c("us", "us", "us", "non-aligned", "china", "us", "non-aligned"),
    gdp_usd = c(27e12, 4.5e12, 3e12, 1.8e12, 18e12, 4.2e12, 3.6e12),
    eu_member = c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    network_node = c("USA", "EUU", "EUU", "MEX", NA, NA, "IND"),
    node_tier = c("core", "core", "core", "partner", "none", "core", "partner")
  )
}

test_that("coalition_members() takes the core and partner tiers of coalition_tbl", {
  out <- expect_silent(coalition_members(coalition_tbl_fixture()))
  expect_equal(out$iso3, c("DEU", "FRA", "IND", "JPN", "MEX", "USA"))
  expect_equal(out$tier, c("core", "core", "partner", "core", "partner", "core"))
  # EU members keep the shared node; a member without one is its own node
  expect_equal(out$network_node, c("EUU", "EUU", "IND", "JPN", "MEX", "USA"))
  expect_true(all(out$source == "coalition_tbl"))
  expect_equal(out$country[out$iso3 == "FRA"], "France")
  # the descriptive columns are carried, a list column flattened
  expect_equal(out$routes[out$iso3 == "IND"], "IPEF; MSP")
  expect_equal(out$eu_member, c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(out$node_tier, c("core", "core", "partner", "core", "partner", "core"))
  expect_false("CHN" %in% out$iso3)
  # coalition_tbl wins over the fallback table, without a warning
  expect_silent(both <- coalition_members(coalition_tbl_fixture(), geo_coalition_fixture()))
  expect_equal(both, out)
  # The hand-picked 23-member list is gone from build_indices.R.
  expect_false(exists("STRATEGIC_ALLY_ISO3"))
})

test_that("coalition_members() falls back to geo_coalition_tbl with a warning", {
  expect_warning(
    out <- coalition_members(NULL, geo_coalition_tbl = geo_coalition_fixture()),
    "coalition_tbl.rds .*not available.*geo_coalition_tbl.rds"
  )
  expect_equal(out$iso3, c("DEU", "JPN", "USA"))
  expect_equal(out$tier, rep("core", 3))
  expect_equal(out$network_node, out$iso3)
  expect_equal(out$country, c("Germany", "Japan", "United States"))
  expect_true(all(out$source == "geo_coalition_tbl"))
  expect_error(coalition_members(NULL, NULL), "No coalition table")
  expect_error(coalition_members(NULL), "No coalition table")
})

test_that("a malformed coalition_tbl stops instead of returning a partial coalition", {
  co <- coalition_tbl_fixture()
  expect_error(coalition_members(dplyr::select(co, -"tier")), "tier")
  expect_error(coalition_members(dplyr::select(co, -"network_node")), "network_node")
  expect_error(coalition_members(dplyr::mutate(co, tier = replace(tier, 2, NA))), "core, partner or none")
  expect_error(coalition_members(dplyr::mutate(co, tier = replace(tier, 4, "associate"))), "associate")
  expect_error(coalition_members(dplyr::mutate(co, iso3 = replace(iso3, 3, "DEU"))), "duplicate ISO3")
  expect_error(coalition_members(dplyr::mutate(co, iso3 = replace(iso3, 3, ""))), "missing or empty")
  expect_error(coalition_members(dplyr::mutate(co, tier = "none")), "no coalition members")
})

test_that("the geo_coalition_tbl fallback is the member == TRUE rows, sorted", {
  expect_equal(allied_coalition_iso3(geo_coalition_fixture()), c("DEU", "JPN", "USA"))
})

test_that("a malformed geo_coalition_tbl stops instead of returning a partial coalition", {
  co <- geo_coalition_fixture()
  expect_error(allied_coalition_iso3(dplyr::select(co, -"member")), "member")
  expect_error(allied_coalition_iso3(dplyr::mutate(co, member = c(TRUE, NA, FALSE, TRUE, FALSE))), "no NA")
  expect_error(allied_coalition_iso3(dplyr::mutate(co, member = as.character(member))), "TRUE/FALSE")
  expect_error(allied_coalition_iso3(dplyr::mutate(co, iso3 = c("USA", "USA", "CHN", "DEU", "IND"))), "duplicate ISO3")
  expect_error(allied_coalition_iso3(dplyr::mutate(co, iso3 = c("USA", "", "CHN", "DEU", "IND"))), "missing or empty")
  expect_error(allied_coalition_iso3(dplyr::mutate(co, member = FALSE)), "no coalition members")
  # coalition_members() validates the fallback the same way, before it warns
  expect_error(coalition_members(NULL, dplyr::mutate(co, member = FALSE)), "no coalition members")
})

test_that("the allied pick is national and records each member's tier and network node", {
  si <- tibble::tibble(
    Country = c("Germany", "Germany", "France", "France", "Mexico", "China"),
    tech = c("Wind", "Solar", "Nuclear", "Wind", "Solar", "Solar"),
    supply_chain = c("Midstream", "Upstream", "Upstream", "Midstream", "Midstream", "Midstream"),
    strategic_index = c(0.7, 0.6, 0.8, 0.5, 0.4, 0.9)
  )
  members <- coalition_members(coalition_tbl_fixture())
  allies <- members %>%
    dplyr::filter(.data$iso3 %in% c("DEU", "FRA", "MEX", "USA")) %>%
    dplyr::transmute(Country = .data$country, iso3 = .data$iso3, tier = .data$tier, network_node = .data$network_node)

  out <- select_allied_strategic_sectors(si, ally_countries = allies) %>% dplyr::arrange(.data$Country)

  # Germany and France share the EU network node but each gets its own pick; China is not a member;
  # the United States has no scored sector.
  expect_equal(out$Country, c("France", "Germany", "Mexico"))
  expect_equal(out$tech, c("Nuclear", "Wind", "Solar"))
  expect_equal(out$tier, c("core", "core", "partner"))
  expect_equal(out$network_node, c("EUU", "EUU", "MEX"))
  expect_equal(out$iso3, c("FRA", "DEU", "MEX"))
  # A character vector of names still works and adds no columns.
  plain <- select_allied_strategic_sectors(si, ally_countries = allies$Country)
  expect_equal(names(plain), names(si))
  expect_equal(dplyr::arrange(plain, .data$Country)$tech, out$tech)

  expect_error(select_allied_strategic_sectors(si, dplyr::bind_rows(allies, allies[1, ])), "one row per Country")
  expect_error(select_allied_strategic_sectors(si, dplyr::mutate(allies, tech = "x")), "tech")
  expect_error(select_allied_strategic_sectors(si, dplyr::select(allies, -"Country")), "Country")
})

# The coalition block of a production script (from `coalition_path <-` to `coalition <-`),
# evaluated against a processed_dir holding the given tables.
run_script_coalition_block <- function(script, tables) {
  exprs <- as.list(parse(file.path(repo_root, "scripts", script), keep.source = FALSE))
  is_assign <- function(e, name) {
    is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name(name))
  }
  from <- which(vapply(exprs, is_assign, logical(1), name = "coalition_path"))
  to <- which(vapply(exprs, is_assign, logical(1), name = "coalition"))
  stopifnot(length(from) == 1, length(to) == 1, from < to)
  dir <- tempfile("coalition_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  for (nm in names(tables)) saveRDS(tables[[nm]], file.path(dir, paste0(nm, ".rds")))
  env <- new.env(parent = parent.frame())
  env$processed_dir <- dir
  for (e in exprs[from:to]) eval(e, envir = env)
  env$coalition
}

test_that("scripts/20 and scripts/30 read the coalition through coalition_members()", {
  for (script in c("20_build_indices.R", "30_build_allied_network_design.R")) {
    expect_error(
      run_script_coalition_block(script, list()),
      "neither coalition_tbl.rds .*nor geo_coalition_tbl.rds", info = script
    )
    expect_warning(
      geo <- run_script_coalition_block(script, list(geo_coalition_tbl = geo_coalition_fixture())),
      "not available", info = script
    )
    expect_equal(geo$iso3, c("DEU", "JPN", "USA"), info = script)
    expect_equal(unique(geo$tier), "core", info = script)
    expect_silent(two_tier <- run_script_coalition_block(
      script,
      list(coalition_tbl = coalition_tbl_fixture(), geo_coalition_tbl = geo_coalition_fixture())
    ))
    expect_equal(two_tier, coalition_members(coalition_tbl_fixture()), info = script)
    two_tier_only <- run_script_coalition_block(script, list(coalition_tbl = coalition_tbl_fixture()))
    expect_equal(two_tier_only, two_tier, info = script)
  }
})

test_that("no hand-written ally list is left in the production scripts", {
  old_only <- c("\"IND\"", "\"MEX\"", "\"BRA\"", "\"SAU\"", "\"ZAF\"", "\"IDN\"", "\"ARE\"", "\"VNM\"", "\"KEN\"", "\"ARG\"", "\"MAR\"", "\"CHL\"")
  for (script in c("20_build_indices.R", "30_build_allied_network_design.R")) {
    src <- readLines(file.path(repo_root, "scripts", script), warn = FALSE)
    code <- sub("#.*$", "", src)
    expect_true(any(grepl("coalition_members(", code, fixed = TRUE)), info = script)
    expect_false(any(grepl("allied_coalition_iso3(", code, fixed = TRUE)), info = script)
    expect_false(any(grepl("STRATEGIC_ALLY_ISO3|allied_network_default_iso3c\\(", code)), info = script)
    for (code_str in old_only) {
      expect_false(any(grepl(code_str, code, fixed = TRUE)), info = paste(script, code_str))
    }
  }
  # scripts/20 hands the member table (with tier) to the allied pick and saves it
  src20 <- sub("#.*$", "", readLines(file.path(repo_root, "scripts", "20_build_indices.R"), warn = FALSE))
  expect_true(any(grepl("select_allied_strategic_sectors(", src20, fixed = TRUE)))
  expect_true(any(grepl("ally_countries = allies", src20, fixed = TRUE)))
  expect_true(any(grepl("require_resource_base = si_resource_screen", src20, fixed = TRUE)))
  expect_true(any(grepl("coalition_members = coalition", src20, fixed = TRUE)))
  # scripts/30 passes tier and network_node through to the design
  src30 <- sub("#.*$", "", readLines(file.path(repo_root, "scripts", "30_build_allied_network_design.R"), warn = FALSE))
  expect_true(any(grepl("network_members = coalition", src30, fixed = TRUE)))
})

test_that("the derived coalition uses valid WDI ISO3 codes", {
  processed <- file.path(repo_root, "data", "processed")
  co_path <- file.path(processed, "coalition_tbl.rds")
  geo_path <- file.path(processed, "geo_coalition_tbl.rds")
  wdi <- file.path(repo_root, "data", "raw", "wdi_country_info.csv")
  skip_if_not(file.exists(co_path) || file.exists(geo_path), "no coalition table built")
  skip_if_not(file.exists(wdi), "staged WDI country file not available")
  coalition <- suppressWarnings(coalition_members(
    if (file.exists(co_path)) readRDS(co_path) else NULL,
    if (file.exists(geo_path)) readRDS(geo_path) else NULL
  ))
  iso <- toupper(utils::read.csv(wdi, stringsAsFactors = FALSE)$iso3c)
  expect_equal(setdiff(coalition$iso3, iso), character(0))
  expect_true(all(c("USA", "JPN", "DEU") %in% coalition$iso3))
  expect_true(all(coalition$tier %in% c("core", "partner")))
})

# Pillar tables as the v2 builders return them: every cell carries rankable and
# rank_exclusion (flag_rankable_cells()).
with_rankability <- function(tbl, value_col, not_rankable = NULL, reason = "imputed weight share above 0.5") {
  hit <- if (is.null(not_rankable)) rep(FALSE, nrow(tbl)) else not_rankable(tbl)
  tbl %>%
    dplyr::mutate(
      rankable = !is.na(.data[[value_col]]) & !hit,
      rank_exclusion = dplyr::case_when(
        is.na(.data[[value_col]]) ~ "not scored",
        hit ~ reason,
        TRUE ~ NA_character_
      )
    )
}

test_that("cells whose EO or ES is not rankable leave the SI, with the reason", {
  fx <- si_fixture()
  eo_hit <- function(t) t$Country == "A" & t$tech == "Solar" & t$supply_chain == "Upstream"
  es_hit <- function(t) t$Country == "A" & t$tech == "Wind" & t$supply_chain == "Midstream"
  fx$eo <- with_rankability(fx$eo, "Economic_Opportunity_Index", eo_hit)
  fx$es <- with_rankability(fx$es, "Energy_Security_Index", es_hit)

  si <- build_fixture_si(fx)
  cell <- function(tech, sc) si[si$Country == "A" & si$tech == tech & si$supply_chain == sc, ]

  expect_true(is.na(cell("Solar", "Upstream")$strategic_index))
  expect_equal(
    cell("Solar", "Upstream")$si_exclusion,
    "economic opportunity not rankable (imputed weight share above 0.5)"
  )
  expect_true(is.na(cell("Wind", "Midstream")$strategic_index))
  expect_equal(
    cell("Wind", "Midstream")$si_exclusion,
    "energy security not rankable (imputed weight share above 0.5)"
  )

  # The pillar values themselves are reported unchanged.
  pick <- function(tbl, col, hit) tbl[[col]][hit(tbl)]
  expect_equal(cell("Solar", "Upstream")$Economic_Opportunity_Index,
               pick(fx$eo, "Economic_Opportunity_Index", eo_hit))
  expect_equal(cell("Wind", "Midstream")$Energy_Security_Index,
               pick(fx$es, "Energy_Security_Index", es_hit))

  # Every other cell is untouched: same SI as with all cells rankable, apart from the
  # within-country policy rank, which is now taken over A's remaining sectors.
  b_base <- build_fixture_si() %>%
    dplyr::filter(.data$Country == "B") %>%
    dplyr::select("tech", "supply_chain", base_si = "strategic_index")
  b <- si %>%
    dplyr::filter(.data$Country == "B") %>%
    dplyr::inner_join(b_base, by = c("tech", "supply_chain"))
  expect_equal(nrow(b), 11L)
  expect_equal(b$strategic_index, b$base_si)
  a_in <- si %>% dplyr::filter(.data$Country == "A", !is.na(.data$strategic_index))
  expect_equal(nrow(a_in), 9L)
  expect_equal(a_in$pol, median_scurve(a_in$value))

  # The chart S-curves rank each pillar over its rankable cells only.
  a <- si %>% dplyr::filter(.data$Country == "A")
  solar_up <- a$tech == "Solar" & a$supply_chain == "Upstream"
  wind_mid <- a$tech == "Wind" & a$supply_chain == "Midstream"
  expect_true(is.na(a$eo[solar_up]))
  expect_false(is.na(a$es_need[solar_up]))
  expect_true(is.na(a$es_need[wind_mid]))
  expect_equal(a$eo[!solar_up], median_scurve(a$Economic_Opportunity_Index[!solar_up]))
})

test_that("a territory is excluded from the SI on both pillars", {
  fx <- si_fixture()
  territory <- function(t) t$Country == "B"
  fx$eo <- with_rankability(fx$eo, "Economic_Opportunity_Index", territory, "not a sovereign economy")
  fx$es <- with_rankability(fx$es, "Energy_Security_Index", territory, "not a sovereign economy")

  si <- build_fixture_si(fx)
  b <- si %>% dplyr::filter(.data$Country == "B")

  expect_true(all(is.na(b$strategic_index)))
  expect_true(all(b$si_exclusion == paste(
    "economic opportunity not rankable (not a sovereign economy);",
    "energy security not rankable (not a sovereign economy)"
  )))
  expect_true(all(is.na(b$eo)))
  expect_true(all(is.na(b$es_need)))
  expect_false(anyNA(b$Economic_Opportunity_Index))

  # A missing pillar is reported as missing, not also as not rankable.
  fx$es$Energy_Security_Index[fx$es$Country == "A" & fx$es$tech == "Coal" &
                                fx$es$supply_chain == "Upstream"] <- NA
  fx$es <- with_rankability(dplyr::select(fx$es, -"rankable", -"rank_exclusion"),
                            "Energy_Security_Index", territory, "not a sovereign economy")
  si <- build_fixture_si(fx)
  coal_up <- si$Country == "A" & si$tech == "Coal" & si$supply_chain == "Upstream"
  expect_equal(si$si_exclusion[coal_up], "missing energy security")
})

test_that("pillar tables without rankability columns count every scored cell as rankable", {
  fx <- si_fixture()
  plain <- build_fixture_si(fx)
  flagged <- build_fixture_si(list(
    eo = with_rankability(fx$eo, "Economic_Opportunity_Index"),
    es = with_rankability(fx$es, "Energy_Security_Index"),
    pol = fx$pol,
    ghg = fx$ghg
  ))

  expect_equal(flagged, plain)
  expect_true(all(is.na(plain$si_exclusion) | !grepl("rankable", plain$si_exclusion)))

  # A `rankable` column without rank_exclusion (or with an NA flag) still excludes the cell,
  # with a generic reason.
  solar_up <- function(t, country) {
    t$Country == country & t$tech == "Solar" & t$supply_chain == "Upstream"
  }
  eo_flag <- fx$eo %>% dplyr::mutate(rankable = !solar_up(fx$eo, "A"))
  eo_flag$rankable[solar_up(eo_flag, "B")] <- NA
  si <- build_fixture_si(list(eo = eo_flag, es = fx$es, pol = fx$pol, ghg = fx$ghg))
  hit <- si$tech == "Solar" & si$supply_chain == "Upstream"
  expect_true(all(is.na(si$strategic_index[hit])))
  expect_equal(si$si_exclusion[hit], rep("economic opportunity not rankable (flagged not rankable)", 2))
  expect_equal(sum(!is.na(si$strategic_index)), sum(!is.na(plain$strategic_index)) - 2L)
})
