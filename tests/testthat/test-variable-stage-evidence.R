# Guards the evidence-specificity metadata (data/reference/index_variable_stage_evidence.csv) and its
# helpers (R/utils/variable_stage_evidence.R): vocabulary, the lookup precedence, the cell measure of
# docs/strategic_position.md section 1, and the classifications the audits verified on the data.

vse_repo_root <- function() {
  normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
}

source(file.path(vse_repo_root(), "R", "utils", "supply_chain_stages.R"))
source(file.path(vse_repo_root(), "R", "utils", "variable_stage_evidence.R"))

vse_read_reference <- function(name) {
  utils::read.csv(file.path(vse_repo_root(), "data", "reference", name),
    stringsAsFactors = FALSE, check.names = FALSE, na.strings = ""
  )
}

vse_real_evidence <- function() {
  map <- vse_read_reference("index_variable_stage_map.csv")
  vse_read_check(vse_read_reference("index_variable_stage_evidence.csv"), map_ids = map$map_id)
}

# A small evidence table in the CSV's shape.
vse_fixture_evidence <- function(rows) {
  rows <- as.data.frame(rows, stringsAsFactors = FALSE)
  defaults <- list(map_id = "m1", pillar = "EO", category = "Trade", allocation_method = "test",
                   country_variation = "country", source = "test", notes = "")
  for (nm in names(defaults)) if (!nm %in% names(rows)) rows[[nm]] <- defaults[[nm]]
  rows$legacy_stage <- ifelse(rows$stage == "*", "*", supply_chain_stage_rollup(rows$stage))
  rows[VSE_COLUMNS]
}

headline_techs <- c("Batteries", "Coal", "Electric Grid", "Electric Vehicles", "Gas", "Geothermal",
                    "Green Hydrogen", "Nuclear", "Oil", "Solar", "Wind")

test_that("the evidence CSV passes the vocabulary checks and cites map rows that exist", {
  ev <- vse_real_evidence()
  expect_gt(nrow(ev), 0)
  expect_true(all(c("EO", "ES", "Policy", "SI", "PSI", "SN", "PP") %in% ev$pillar))
  expect_true(all(ev$evidence_status %in% VSE_STATUSES))
  expect_true(all(ev$country_variation %in% VSE_COUNTRY_VARIATION))
  expect_true(all(ev$stage %in% c(SUPPLY_CHAIN_STAGES, "*")))
  map <- vse_read_reference("index_variable_stage_map.csv")
  expect_equal(anyDuplicated(map$map_id), 0)
  expect_true(all(nzchar(map$map_id)))
})

test_that("every Economic Opportunity and Energy Security variable resolves where it is scored", {
  ev <- vse_real_evidence()
  # (pillar, category, variable, stages) of the 2026-10-27 contribution tables.
  scored <- list(
    list("EO", "Trade", c("market_share", "rca", "export_size", "feas", "HHI"), setdiff(SUPPLY_CHAIN_STAGES, "Deployment")),
    list("ES", "Trade", c("market_share", "deficit_gdp", "HHI"), setdiff(SUPPLY_CHAIN_STAGES, "Deployment")),
    list("EO", "Production", c("size", "growth_abs", "Overall Production"), c("Extraction", "Refining", "Deployment")),
    list("EO", "Cost Competitiveness", "Input Cost Index", SUPPLY_CHAIN_STAGES),
    list("EO", "Cost Competitiveness", "IEA Cost index", c("Components", "Assembly")),
    list("EO", "Technological Readiness", "TRL Index", SUPPLY_CHAIN_STAGES),
    list("EO", "Technology Demand", c("Overall Global Demand", "Overall Demand", "Overall Addressable Market"), SUPPLY_CHAIN_STAGES),
    list("EO", "Investment", c("Annual Investment Index", "Investment Momentum Index"), c("Refining", "Components", "Assembly", "Deployment")),
    list("ES", "Investment", c("Operating Capacity Index", "Pipeline Capacity Index"), c("Refining", "Components", "Assembly")),
    list("ES", "Minerals Trade", c("critmin_market_share", "critmin_hhi"), c("Extraction", "Refining", "Recovery")),
    list("ES", "Foreign Dependency", "Mineral Supply", c("Extraction", "Refining")),
    list("ES", "Energy Prices", "Overall Price Volatility Index", c("Extraction", "Refining")),
    list("EO", "Consumption", c("installed_cap_index", "elec_growth_index"), "Deployment"),
    list("ES", "Energy Access", c("Energy consumption per capita", "Energy consumption per capita growth"), "Deployment"),
    list("ES", "Reserves", c("Lithium Reserves", "Overall Solar PV Potential Index"), c("Extraction", "Deployment"))
  )
  rows <- do.call(rbind, lapply(scored, function(s) {
    expand.grid(pillar = s[[1]], category = s[[2]], variable = s[[3]], tech = headline_techs,
                supply_chain = s[[4]], stringsAsFactors = FALSE)
  }))
  # Reserves: each variable at its own stage only.
  rows <- rows[!(rows$variable == "Lithium Reserves" & rows$supply_chain == "Deployment") &
                 !(rows$variable == "Overall Solar PV Potential Index" & rows$supply_chain == "Extraction"), ]
  out <- vse_lookup(rows, ev)
  expect_false(any(is.na(out$evidence_status)),
               info = paste(unique(paste(out$variable, out$supply_chain)[is.na(out$evidence_status)]), collapse = "; "))
})

test_that("world constants are global and manufacturing-pair copies are shared at the copied stage", {
  ev <- vse_real_evidence()
  one <- function(pillar, category, variable, tech, stage) {
    vse_lookup(data.frame(pillar = pillar, category = category, variable = variable, tech = tech,
                          supply_chain = stage, stringsAsFactors = FALSE), ev)
  }
  for (v in list(c("EO", "Trade", "HHI"), c("EO", "Technological Readiness", "TRL Index"),
                 c("EO", "Energy Prices", "Overall Price Volatility Index"), c("EO", "Technology Demand", "Overall Overcapacity"),
                 c("ES", "Minerals Trade", "critmin_hhi"))) {
    stage <- if (v[2] == "Minerals Trade" || v[2] == "Energy Prices") "Refining" else "Components"
    tech <- if (v[3] == "Overall Overcapacity") "Solar" else "Batteries"
    expect_identical(one(v[1], v[2], v[3], tech, stage)$country_variation, "global", info = v[3])
  }
  # Batteries files the ETP share and IEA cost at Components; Solar, Wind and electrolysers at Assembly.
  expect_identical(one("ES", "Foreign Dependency", "Market Share", "Batteries", "Components")$evidence_status, "exact")
  expect_identical(one("ES", "Foreign Dependency", "Market Share", "Batteries", "Assembly")$evidence_status, "shared")
  expect_identical(one("EO", "Foreign Dependency", "Overall Market Share", "Solar", "Assembly")$evidence_status, "exact")
  expect_identical(one("EO", "Foreign Dependency", "Overall Market Share", "Solar", "Components")$evidence_status, "shared")
  expect_identical(one("EO", "Cost Competitiveness", "IEA Cost index", "Wind", "Components")$evidence_status, "shared")
  expect_identical(one("EO", "Technology Demand", "Overall Overcapacity", "Wind", "Components")$evidence_status, "shared")
  expect_identical(one("EO", "Technology Demand", "Overall Overcapacity", "Solar", "Components")$evidence_status, "split")
  expect_identical(one("EO", "Investment", "Annual Investment Index", "Solar", "Assembly")$evidence_status, "shared")
  # Trade has no basket at Deployment or for Geothermal; Strategic Need takes Assembly at Deployment.
  expect_identical(one("EO", "Trade", "market_share", "Solar", "Deployment")$evidence_status, "no_data")
  expect_identical(one("EO", "Trade", "market_share", "Geothermal", "Refining")$evidence_status, "no_data")
  expect_identical(one("EO", "Trade", "market_share", "Solar", "Extraction")$evidence_status, "no_data")
  expect_identical(one("SN", "Import Exposure", "import_hhi", "Solar", "Deployment")$evidence_status, "proxy")
  expect_identical(one("SN", "Import Exposure", "import_hhi", "Green Hydrogen", "Deployment")$evidence_status, "no_data")
  expect_identical(one("SN", "Import Exposure", "china_share", "Batteries", "Recovery")$evidence_status, "exact")
  expect_identical(one("SN", "Import Exposure", "opposite_bloc_share", "Coal", "Recovery")$evidence_status, "no_data")
  expect_identical(one("PP", "nipo", "nipo_attributed", "Wind", "Deployment")$evidence_status, "no_data")
  expect_identical(one("PP", "nipo", "nipo_attributed", "Wind", "Components")$evidence_status, "split")
  expect_identical(one("PP", "pams", "pams_tech_wide", "Wind", "Components")$evidence_status, "tech_wide")
})

test_that("the lookup prefers tech + stage, then tech, then stage, then the default", {
  ev <- vse_fixture_evidence(list(
    variable = "v", tech = c("*", "*", "Solar", "Solar"), stage = c("*", "Assembly", "*", "Assembly"),
    evidence_status = c("proxy", "shared", "tech_wide", "exact"), map_id = c("m_any", "m_stage", "m_tech", "m_both")
  ))
  ev <- vse_read_check(ev)
  rows <- data.frame(
    pillar = "EO", category = "Trade", variable = c("v", "v", "v", "v", "other", "v"),
    tech = c("Solar", "Solar", "Wind", "Wind", "Solar", "Wind"),
    supply_chain = c("Assembly", "Components", "Assembly", "Components", "Assembly", "Upstream"),
    stringsAsFactors = FALSE
  )
  out <- vse_lookup(rows, ev)
  expect_identical(out$evidence_status, c("exact", "tech_wide", "shared", "proxy", NA, "proxy"))
  expect_identical(out$map_id, c("m_both", "m_tech", "m_stage", "m_any", NA, "m_any"))
  expect_identical(out$evidence_match, c("tech+stage", "tech+*", "*+stage", "*+*", NA, "*+*"))
  expect_identical(out$variable, rows$variable)
})

test_that("vse_read_check() rejects bad vocabulary, inconsistent roll-ups and duplicate keys", {
  base <- list(variable = "v", tech = "*", stage = "Assembly", evidence_status = "exact")
  ok <- vse_fixture_evidence(base)
  expect_silent(vse_read_check(ok))
  bad <- ok; bad$evidence_status <- "inherited"
  expect_error(vse_read_check(bad), "unknown evidence_status")
  bad <- ok; bad$country_variation <- "world"
  expect_error(vse_read_check(bad), "unknown country_variation")
  bad <- ok; bad$stage <- "Midstream"; bad$legacy_stage <- "Midstream"
  expect_error(vse_read_check(bad), "neither a six-stage name")
  bad <- ok; bad$legacy_stage <- "Upstream"
  expect_error(vse_read_check(bad), "legacy_stage")
  expect_error(vse_read_check(rbind(ok, ok)), "duplicate")
  bad <- ok; bad$variable <- "*"
  expect_error(vse_read_check(bad), "only in tech and stage")
  expect_error(vse_read_check(ok, map_ids = "other"), "map_id")
  expect_error(vse_read_check(ok[setdiff(names(ok), "source")]), "missing column")
})

# Two cells of four inputs and one of a single input, weights summing to 1 per cell.
vse_fixture_inputs <- function() {
  data.frame(
    Country = c("A", "A", "A", "A", "B", "C"),
    tech = "Solar",
    supply_chain = c("Assembly", "Assembly", "Assembly", "Assembly", "Assembly", "Assembly"),
    pillar = "EO", category = "Trade",
    variable = c("v_exact", "v_split", "v_shared", "v_proxy", "v_tech", "v_exact"),
    weight = c(0.4, 0.2, 0.3, 0.1, 1, 1),
    observed = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
}
vse_fixture_statuses <- function() {
  vse_read_check(vse_fixture_evidence(list(
    variable = c("v_exact", "v_split", "v_shared", "v_proxy", "v_tech", "v_gone"), tech = "*", stage = "*",
    evidence_status = c("exact", "split", "shared", "proxy", "tech_wide", "no_data"),
    country_variation = c("country", "country", "global", "country", "country", "country")
  )))
}

test_that("specificity is the observed stage-specific weight over all weight; shares are over observed weight", {
  out <- vse_cell_specificity(vse_fixture_inputs(), vse_fixture_statuses())
  a <- out[out$Country == "A", ]
  expect_equal(a$evidence_specificity, 0.6)
  expect_equal(a$exact_evidence_share, 0.4 / 0.9)
  expect_equal(a$split_evidence_share, 0.2 / 0.9)
  expect_equal(a$shared_evidence_share, 0.3 / 0.9)
  expect_equal(a$proxy_evidence_share, 0)
  expect_equal(a$observed_share, 0.9)
  expect_equal(a$global_share, 0.3 / 0.9)
  expect_equal(a$no_data_share, 0)
  expect_equal(a$n_inputs, 4L)
  expect_identical(a$stage_evidence_quality, "medium")
  b <- out[out$Country == "B", ]
  expect_equal(b$evidence_specificity, 0)
  expect_equal(b$tech_wide_evidence_share, 1)
  expect_identical(b$stage_evidence_quality, "none")
  expect_identical(out$stage_evidence_quality[out$Country == "C"], "high")
  # A world constant at the right stage is specific by status; count_global = FALSE leaves it out.
  glob <- vse_fixture_inputs()
  glob$variable[glob$variable == "v_shared"] <- "v_exact_global"
  ev_g <- rbind(vse_fixture_statuses(), vse_fixture_evidence(list(variable = "v_exact_global", tech = "*", stage = "*",
                                                                  evidence_status = "exact", country_variation = "global")))
  a_all <- vse_cell_specificity(glob, ev_g)
  a_cty <- vse_cell_specificity(glob, ev_g, count_global = FALSE)
  expect_equal(a_all$evidence_specificity[a_all$Country == "A"], 0.9)
  expect_equal(a_cty$evidence_specificity[a_cty$Country == "A"], 0.6)
  expect_equal(a_cty$exact_evidence_share, a_all$exact_evidence_share)
  own <- data.frame(Country = "A", tech = "Solar", supply_chain = "Assembly", evidence_status = "exact",
                    weight = 1, observed = TRUE, stringsAsFactors = FALSE)
  expect_error(vse_cell_specificity(own, count_global = FALSE), "country_variation")
  expect_equal(nrow(vse_cell_specificity(own[0, ])), 0L)

  # Band edges: exactly 0.67 is high, exactly 0.33 medium, anything above 0 low.
  edge <- data.frame(Country = rep(c("H", "M", "L"), each = 2), tech = "Solar", supply_chain = "Assembly",
                     evidence_status = rep(c("exact", "shared"), 3), weight = c(0.67, 0.33, 0.33, 0.67, 0.01, 0.99),
                     observed = TRUE, stringsAsFactors = FALSE)
  q <- vse_cell_specificity(edge)
  expect_identical(q$stage_evidence_quality[match(c("H", "M", "L"), q$Country)], c("high", "medium", "low"))
  expect_identical(vse_cell_specificity(edge, bands = list(high = 0.9, medium = 0.5))$stage_evidence_quality[match("H", q$Country)], "medium")
})

test_that("a shared row counts as non-specific", {
  inputs <- data.frame(Country = c("A", "B"), tech = "Batteries", supply_chain = "Components", pillar = "EO",
                       category = "Trade", variable = c("v_exact", "v_shared"), weight = 1, observed = TRUE,
                       stringsAsFactors = FALSE)
  out <- vse_cell_specificity(inputs, vse_fixture_statuses())
  expect_equal(out$evidence_specificity, c(1, 0))
  expect_equal(out$shared_evidence_share, c(0, 1))
  # Counting shared as specific is a choice of the caller, not of the table.
  expect_equal(vse_cell_specificity(inputs, vse_fixture_statuses(), specific = c("exact", "split", "shared"))$evidence_specificity, c(1, 1))
})

test_that("an unobserved exact row lowers specificity but not the observed shares", {
  inputs <- data.frame(Country = rep(c("A", "B"), each = 2), tech = "Wind", supply_chain = "Assembly", pillar = "EO",
                       category = "Trade", variable = "v_exact", weight = 0.5, observed = c(TRUE, TRUE, TRUE, FALSE),
                       stringsAsFactors = FALSE)
  out <- vse_cell_specificity(inputs, vse_fixture_statuses())
  expect_equal(out$evidence_specificity, c(1, 0.5))
  expect_equal(out$exact_evidence_share, c(1, 1))
  expect_equal(out$observed_share, c(1, 0.5))
  # NA observed counts as not observed; zero-weight rows are not inputs.
  inputs$observed[4] <- NA
  inputs <- rbind(inputs, transform(inputs[1, ], variable = "v_shared", weight = 0))
  out2 <- vse_cell_specificity(inputs, vse_fixture_statuses())
  expect_equal(out2$evidence_specificity, c(1, 0.5))
  expect_equal(out2$n_inputs, c(2L, 2L))
})

test_that("no_data rows count in no_data_share; a module's own evidence_status is used and NA ones looked up", {
  inputs <- data.frame(Country = "A", tech = "Oil", supply_chain = "Deployment", pillar = "EO", category = "Trade",
                       variable = c("v_gone", "v_exact", "v_x"), weight = c(0.5, 0.25, 0.25), observed = c(FALSE, TRUE, TRUE),
                       evidence_status = c(NA, NA, "proxy"), stringsAsFactors = FALSE)
  out <- vse_cell_specificity(inputs, vse_fixture_statuses())
  expect_equal(out$no_data_share, 0.5)
  expect_equal(out$evidence_specificity, 0.25)
  expect_equal(out$proxy_evidence_share, 0.5)
  # Without evidence a status column suffices.
  own <- inputs; own$evidence_status <- c("no_data", "exact", "proxy")
  expect_equal(vse_cell_specificity(own[c("Country", "tech", "supply_chain", "weight", "observed", "evidence_status")])$evidence_specificity, 0.25)
})

test_that("an input variable with no evidence row stops with the variable named", {
  inputs <- vse_fixture_inputs()
  inputs$variable[2] <- "not_in_table"
  expect_error(vse_cell_specificity(inputs, vse_fixture_statuses()), "unmatched variables: EO / Trade / not_in_table")
  expect_error(vse_cell_specificity(inputs[c("Country", "tech", "supply_chain", "weight", "observed")]), "no evidence_status")
  expect_error(vse_cell_specificity(vse_fixture_inputs(), vse_fixture_statuses(), bands = list(high = 0.2, medium = 0.5)), "bands")
})

test_that("the coverage summary averages shares and counts quality bands by group", {
  cells <- vse_cell_specificity(vse_fixture_inputs(), vse_fixture_statuses())
  overall <- vse_coverage_summary(cells)
  expect_equal(overall$n_cells, 3L)
  expect_equal(overall$mean_evidence_specificity, mean(c(0.6, 0, 1)))
  expect_equal(c(overall$n_high, overall$n_medium, overall$n_low, overall$n_none), c(1L, 1L, 0L, 1L))
  by_country <- vse_coverage_summary(cells, by = "Country")
  expect_equal(nrow(by_country), 3L)
  expect_equal(by_country$mean_observed_share[by_country$Country == "A"], 0.9)
  # Suffixed columns, as in strategic_position_tbl.
  suffixed <- cells
  names(suffixed)[-(1:3)] <- paste0(names(suffixed)[-(1:3)], "_cp")
  expect_equal(vse_coverage_summary(suffixed, suffix = "_cp")$mean_evidence_specificity_cp, overall$mean_evidence_specificity)
})
