# Guards the clean PRIZE (R/strategic_position/industrial_opportunity.R), the Catch Up pathway
# (pathway.R) and supply_chain_need (supply_chain_need.R) of the canonical decision framework
# (docs/strategic_position.md; spec Parts IV-VI and XXII "Industrial Opportunity").

io_repo_root <- function() normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
suppressPackageStartupMessages(library(dplyr))
for (f in c("R/utils/supply_chain_stages.R", "R/strategic_position/sp_helpers.R", "R/strategic_position/posture_engine.R",
            "R/strategic_position/industrial_opportunity.R", "R/strategic_position/pathway.R",
            "R/strategic_position/supply_chain_need.R")) {
  source(file.path(io_repo_root(), f))
}
io_cfg <- function() yaml::read_yaml(file.path(io_repo_root(), "config", "strategic_position.yml"))$decision_framework

# A synthetic node-growth table: tech x six stages with a growth rate and outlook.
io_growth_tbl <- function(g = c(Solar = 2, Wind = 1, Coal = -0.2, Nuclear = 0.4)) {
  x <- expand.grid(tech = names(g), supply_chain = SUPPLY_CHAIN_STAGES, stringsAsFactors = FALSE)
  x$io_growth <- unname(g[x$tech])
  x$io_growth_current_policies <- x$io_growth
  x$io_growth_series <- paste0("cap_", tolower(x$tech))
  x$io_growth_basis <- "weo_world_stated"
  x$io_growth_match <- "exact"
  x$io_outlook <- sp_io_outlook(x$io_growth)
  x
}
io_cells <- function(iso3 = c("AAA", "BBB", "CCC"), techs = c("Solar", "Wind", "Coal", "Nuclear")) {
  expand.grid(iso3 = iso3, tech = techs, supply_chain = SUPPLY_CHAIN_STAGES, stringsAsFactors = FALSE)
}

test_that("no capability and no need variable can enter the prize", {
  expect_error(sp_io_check_inputs(c("io_growth_pct", "market_share")), "capability or need")
  expect_error(sp_io_check_inputs("rca"), "capability or need")
  expect_error(sp_io_check_inputs("import_hhi"), "capability or need")
  expect_error(sp_io_check_inputs("HHI"), "capability or need")
  expect_error(sp_io_check_inputs(c("china_share", "deficit_gdp")), "capability or need")
  expect_error(sp_io_check_inputs("competitive_position_percentile"), "capability or need")
  expect_true(sp_io_check_inputs(SP_IO_COMPONENTS))
  # the CP variables (EO Trade, Production, Investment capability, Reserves) and the need measures are listed
  for (v in c("market_share", "rca", "export_size", "size", "Operating Capacity Index", "Annual Investment Index",
              "Reserves", "HHI", "import_hhi", "china_share", "opposite_bloc_share", "supply_chain_need",
              "Input Cost Index", "feas")) {
    expect_true(v %in% SP_IO_FORBIDDEN, info = v)
  }
  expect_length(intersect(SP_IO_COMPONENTS, SP_IO_FORBIDDEN), 0)
})

test_that("industrial_opportunity does not move with capability or need columns", {
  np <- sp_io_node_prize(io_growth_tbl(), cfg = io_cfg()$industrial_opportunity)
  cells <- io_cells()
  a <- sp_industrial_opportunity(cells, np, cfg = io_cfg()$industrial_opportunity)
  b <- cells
  set.seed(3)
  b$market_share <- runif(nrow(b))
  b$competitive_position_percentile <- runif(nrow(b))
  b$supply_chain_need <- runif(nrow(b))
  b$import_hhi <- runif(nrow(b))
  b <- sp_industrial_opportunity(b, np, cfg = io_cfg()$industrial_opportunity)
  expect_identical(b$industrial_opportunity, a$industrial_opportunity)
})

test_that("country-invariant technology factors do not differentiate countries", {
  np <- sp_io_node_prize(io_growth_tbl(), maturity = data.frame(tech = "Solar", supply_chain = "Components", io_maturity = 9,
                                                                 io_maturity_class = "mature"),
                         cfg = io_cfg()$industrial_opportunity)
  out <- sp_industrial_opportunity(io_cells(), np, cfg = io_cfg()$industrial_opportunity)
  prod <- out[out$supply_chain != "Deployment", ]
  spread <- tapply(prod$industrial_opportunity, paste(prod$tech, prod$supply_chain), function(v) diff(range(v)))
  expect_true(all(spread == 0))
  expect_true(all(prod$io_basis == "node"))
  # maturity is context only: changing it leaves the prize unchanged
  np2 <- np
  np2$io_maturity[!is.na(np2$io_maturity)] <- 2
  expect_identical(sp_industrial_opportunity(io_cells(), np2, cfg = io_cfg()$industrial_opportunity)$industrial_opportunity,
                   out$industrial_opportunity)
  # Deployment carries no prize (its Need is deployment_need)
  expect_true(all(is.na(out$industrial_opportunity[out$supply_chain == "Deployment"])))
})

test_that("a national home market lifts the prize; a regional proxy is shown, never scored", {
  np <- sp_io_node_prize(io_growth_tbl(), cfg = io_cfg()$industrial_opportunity)
  home <- data.frame(iso3 = c("AAA", "BBB"), tech = "Wind", home_market = c(0.95, 0.95), hm_basis = c("national", "regional_proxy"))
  out <- sp_industrial_opportunity(io_cells(), np, home, io_cfg()$industrial_opportunity)
  w <- out[out$tech == "Wind" & out$supply_chain == "Components", ]
  node <- w$io_node_prize[1]
  expect_equal(w$industrial_opportunity[w$iso3 == "AAA"], (node + 0.95) / 2)
  expect_equal(w$io_basis[w$iso3 == "AAA"], "node_and_home_market")
  expect_equal(w$industrial_opportunity[w$iso3 == "BBB"], node)
  expect_equal(w$io_home_market_basis[w$iso3 == "BBB"], "regional_proxy")
  expect_false(w$io_home_market_scored[w$iso3 == "BBB"])
  expect_equal(w$industrial_opportunity[w$iso3 == "CCC"], node)
  expect_error(sp_io_home_market_std(data.frame(iso3 = "AAA", tech = "Wind", home_market = 1.4)), "\\[0, 1\\]")
  # a small home market never lowers a world-market prize (lift); the mean mode averages both ways
  low <- data.frame(iso3 = "CCC", tech = "Wind", io_home_market = 0.05, io_home_market_basis = "national")
  io_io <- io_cfg()$industrial_opportunity
  lift <- sp_industrial_opportunity(io_cells(), np, low, io_io)
  lc <- lift[lift$tech == "Wind" & lift$supply_chain == "Components" & lift$iso3 == "CCC", ]
  expect_equal(lc$industrial_opportunity, node)
  expect_false(lc$io_home_market_lift)
  expect_equal(lc$io_basis, "node")
  io_io$home_market_mode <- "mean"
  mean_mode <- sp_industrial_opportunity(io_cells(), np, low, io_io)
  expect_equal(mean_mode$industrial_opportunity[mean_mode$tech == "Wind" & mean_mode$supply_chain == "Components" & mean_mode$iso3 == "CCC"],
               (node + 0.05) / 2)
  io_io$home_market_mode <- "max"
  expect_error(sp_industrial_opportunity(io_cells(), np, low, io_io), "lift or mean")
})

test_that("a declining world market caps the prize, whatever the home market", {
  cfg <- io_cfg()$industrial_opportunity
  np <- sp_io_node_prize(io_growth_tbl(), cfg = cfg)
  coal <- np[np$tech == "Coal" & np$supply_chain != "Deployment", ]
  expect_true(all(coal$io_outlook == "declining"))
  expect_true(all(coal$io_node_prize <= cfg$caps$declining))
  home <- data.frame(iso3 = "AAA", tech = "Coal", home_market = 1, hm_basis = "national")
  out <- sp_industrial_opportunity(io_cells(), np, home, cfg)
  expect_true(all(out$industrial_opportunity[out$tech == "Coal" & out$supply_chain != "Deployment"] <= cfg$caps$declining))
  # an oversupplied node is capped only when listed (a shippable source)
  cfg_o <- cfg
  cfg_o$oversupplied_nodes <- list("Solar|Components")
  np_o <- sp_io_node_prize(io_growth_tbl(), cfg = cfg_o)
  s <- np_o[np_o$tech == "Solar" & np_o$supply_chain == "Components", ]
  expect_true(s$io_oversupplied)
  expect_lte(s$io_node_prize, cfg$caps$oversupplied)
})

test_that("missing components are reported, not filled", {
  g <- io_growth_tbl()
  g$io_growth[g$tech == "Nuclear" & g$supply_chain == "Recovery"] <- NA
  mom <- data.frame(tech = "Solar", supply_chain = "Components", io_investment_momentum = 0.3,
                    io_investment_latest_usd_bn = 10, io_investment_baseline_usd_bn = 7)
  np <- sp_io_node_prize(g, mom, cfg = io_cfg()$industrial_opportunity)
  nr <- np[np$tech == "Nuclear" & np$supply_chain == "Recovery", ]
  expect_true(is.na(nr$io_node_prize))
  expect_equal(nr$io_prize_basis, "not_assessed")
  sc <- np[np$tech == "Solar" & np$supply_chain == "Components", ]
  expect_equal(sc$io_prize_basis, "weo_world_stated+investment_momentum")
  sa <- np[np$tech == "Solar" & np$supply_chain == "Assembly", ]
  expect_equal(sa$io_prize_basis, "weo_world_stated")
  expect_true(is.na(sa$io_investment_momentum))
  expect_equal(sa$io_node_prize, sa$io_growth_pct)   # momentum not observed: the prize is the growth term alone
  out <- sp_industrial_opportunity(io_cells(), np, cfg = io_cfg()$industrial_opportunity)
  expect_true(all(out$io_basis[out$tech == "Nuclear" & out$supply_chain == "Recovery"] == "not_assessed"))
  # investment momentum: a node below the baseline floor is not observed (NA), never zero
  inv <- data.frame(tech = c("Solar", "Solar", "Wind", "Wind"), supply_chain = "Components", Year = c(2021L, 2025L, 2021L, 2025L),
                    value = c(10, 20, 0.1, 5))
  m <- sp_io_investment_momentum(inv, list(latest_years = 2025, baseline_years = 2021, min_baseline_usd_bn = 0.5))
  expect_equal(m$io_investment_momentum[m$tech == "Solar"], log(2))
  expect_true(is.na(m$io_investment_momentum[m$tech == "Wind"]))
})

test_that("world demand growth comes from the node series, minerals for mineral stages, deployment as the Recovery proxy", {
  map <- data.frame(cell = c("solar-components", "solar-deployment", "solar-extraction"), series = c("cap_solar", "cap_solar", "cap_solar"),
                    role = c("own", "own", "demand"), match = "exact", order = 1L, stringsAsFactors = FALSE)
  values <- data.frame(series = "cap_solar", scenario = c("historical", "stated", "current"), year = c(2024L, 2035L, 2035L),
                       value = c(100, 400, 300))
  cells_meta <- data.frame(cell = c("solar-components", "solar-extraction"), match = c("exact", "partial"),
                           mineral_techs = c(NA, "Solar PV"), stringsAsFactors = FALSE)
  minerals <- data.frame(sheet = "1", mineral = "Copper", tech = "Solar PV", scenario = "stated", year = c(2025L, 2035L), kt = c(100, 150))
  g <- sp_io_node_growth(map, values, cells_meta, minerals, techs = "Solar", cfg = io_cfg()$industrial_opportunity)
  expect_equal(g$io_growth[g$supply_chain == "Components"], 3)
  expect_equal(g$io_growth_current_policies[g$supply_chain == "Components"], 2)
  expect_equal(g$io_growth[g$supply_chain == "Extraction"], 0.5)
  expect_equal(g$io_growth_basis[g$supply_chain == "Extraction"], "iea_minerals_stated")
  expect_equal(g$io_growth_basis[g$supply_chain == "Recovery"], "proxy_deployment")
  expect_equal(g$io_growth[g$supply_chain == "Recovery"], 3)
  expect_equal(g$io_outlook[g$supply_chain == "Components"], "growing")
})

test_that("the technology outlook comes from data, with validated overrides", {
  g <- io_growth_tbl()
  o <- sp_io_tech_outlook(g)
  expect_equal(unname(o[c("Solar", "Coal")]), c("growing", "declining"))
  expect_equal(unname(sp_io_tech_outlook(g, list(Coal = "flat"))["Coal"]), "flat")
  expect_error(sp_io_tech_outlook(g, list(Coal = "shrinking")), "unknown outlook")
})

test_that("the validation tables report correlations, weights, movers and the disguised-capability check", {
  set.seed(11)
  tbl <- io_cells(iso3 = sprintf("C%02d", 1:20), techs = c("Solar", "Wind"))
  np <- sp_io_node_prize(io_growth_tbl(c(Solar = 2, Wind = 1)), cfg = io_cfg()$industrial_opportunity)
  tbl <- sp_industrial_opportunity(tbl, np, cfg = io_cfg()$industrial_opportunity)
  tbl$Country <- tbl$iso3
  tbl$competitive_position_percentile <- runif(nrow(tbl))
  tbl$supply_chain_need <- runif(nrow(tbl))
  tbl$legacy_eo_opportunity_pct <- runif(nrow(tbl))
  v <- sp_io_validation(tbl, n_movers = 5)
  expect_named(v, c("correlations", "weights", "movers", "disguise"))
  expect_equal(nrow(v$movers), 5)
  expect_equal(sum(v$weights$share_of_prize), 1)
  expect_true(all(is.na(v$disguise$spearman_with_cp)))   # no home market: the prize is constant within a node
})

test_that("with min_legs 1 the credible pathway fires on any evidence leg, and unknown is not yes", {
  x <- data.frame(iso3 = c("A", "B", "C", "D", "E", "F"), tech = "Solar",
                  supply_chain = c("Components", "Components", "Components", "Extraction", "Extraction", "Deployment"),
                  home_share = c(0.01, 0.01, NA, 0.01, 0.01, NA), cp_within_country_pct = c(0.2, 0.2, NA, 0.2, 0.2, NA),
                  feas_pct = c(0.9, 0.1, NA, 0.9, 0.9, 0.9), resource_base = c(NA, NA, NA, FALSE, TRUE, NA),
                  t3_learning_lo = c("none", "none", NA, "none", "none", "n_a"), stringsAsFactors = FALSE)
  one <- utils::modifyList(io_cfg()$pathway, list(min_legs = 1))
  out <- sp_pathway(x, cfg = one)
  expect_equal(out$pathway_credible, c("yes", "no", "unknown", "no", "yes", NA))
  expect_equal(out$pathway_basis[1], "feasibility")
  expect_equal(out$pathway_basis[3], "no_evidence")
  expect_equal(out$pathway_basis[4], "no_resource_base")
  pipe <- data.frame(iso3 = "B", tech = "Solar", supply_chain = "Components", pipeline_capacity = 5)
  cov <- data.frame(tech = "Solar", supply_chain = "Components")
  out2 <- sp_pathway(x, pipe, cov, cfg = one)
  expect_equal(out2$pathway_credible[2], "yes")
  expect_equal(out2$pathway_basis[2], "pipeline")
  expect_equal(out2$pw_pipeline[1], 0)   # covered by the monitor: absence is an observed zero
  # without min_legs the function keeps design D3's one leg
  expect_equal(sp_pathway(x, cfg = within(one, rm(min_legs)))$pathway_credible, out$pathway_credible)
})

test_that("the configured pathway needs two evidence legs; one leg is not enough, and too little evidence is unknown", {
  expect_equal(io_cfg()$pathway$min_legs, 2)
  # A: feasibility only, three legs observed -> no. B: feasibility + adjacent share -> yes. C: feasibility +
  # learning at an Extraction cell without a resource base -> no. D: feasibility + input cost at an Extraction
  # cell with the resource base unknown -> unknown. E: feasibility is the only observed leg and it fires ->
  # unknown (the data cannot reach two). F: nothing fires, two legs observed -> no. G: nothing observed.
  x <- data.frame(iso3 = c("A", "B", "C", "D", "E", "F", "G"), tech = "Solar",
                  supply_chain = c("Components", "Components", "Extraction", "Extraction", "Components", "Components", "Components"),
                  home_share = c(0.01, 0.2, 0.01, NA, NA, 0.01, NA), cp_within_country_pct = c(0.2, 0.2, 0.2, NA, NA, 0.2, NA),
                  feas_pct = c(0.9, 0.9, 0.9, 0.9, 0.9, 0.1, NA), resource_base = c(NA, NA, FALSE, NA, NA, NA, NA),
                  t3_learning_lo = c("none", "none", "strong", NA, NA, NA, NA), stringsAsFactors = FALSE)
  # the adjacent-stage leg reads the other stages of the same cell, so give B an established Refining stage
  ref <- x[2, ]
  ref$supply_chain <- "Refining"
  ref$home_share <- 0.2
  x$home_share[2] <- 0.01
  ic <- data.frame(iso3 = c("D", "X1", "X2", "X3"), tech = "Solar", supply_chain = "Extraction", input_cost_index = c(4, 1, 2, 3))
  out <- sp_pathway(rbind(x, ref), input_cost = ic, cfg = io_cfg()$pathway)[1:7, ]
  expect_equal(out$pathway_credible, c("no", "yes", "no", "unknown", "unknown", "no", "unknown"))
  expect_equal(out$pathway_basis, c("feasibility;too_few_legs", "feasibility;adjacent_stage", "no_resource_base",
                                    "feasibility;input_cost;resource_base_unknown", "feasibility;too_few_observed",
                                    "none_observed_fires", "no_evidence"))
  # an extra leg makes A credible
  pipe <- data.frame(iso3 = "A", tech = "Solar", supply_chain = "Components", pipeline_capacity = 5)
  cov <- data.frame(tech = "Solar", supply_chain = "Components")
  expect_equal(sp_pathway(rbind(x, ref), pipe, cov, cfg = io_cfg()$pathway)$pathway_basis[1], "feasibility;pipeline")
  # min_legs outside 1..5 is refused
  for (bad in list(0, 6, 1.5, "2", NA_real_, Inf, c(1, 2))) {
    expect_error(sp_pathway(x, cfg = utils::modifyList(io_cfg()$pathway, list(min_legs = bad))), "min_legs")
  }
})

test_that("supply_chain_need exists only at the producing stages and has no Deployment proxy", {
  x <- expand.grid(iso3 = sprintf("C%02d", 1:10), tech = "Solar", supply_chain = c("Components", "Deployment"), stringsAsFactors = FALSE)
  set.seed(5)
  n <- nrow(x)
  x$strategic_need_percentile <- runif(n)
  x$sn_import_dependence <- runif(n)
  x$sn_supplier_concentration <- runif(n)
  x$sn_risky_supplier_exposure <- runif(n)
  x$trusted_world_share <- runif(n)
  x$home_share <- c(0.2, rep(0.01, 9), rep(NA, 10))
  x$sn_import_material <- TRUE
  out <- sp_supply_chain_need(x, io_cfg())
  dep <- out$supply_chain == "Deployment"
  expect_true(all(is.na(out$supply_chain_need[dep])))
  expect_true(all(out$need_type[dep] == "deployment"))
  expect_true(all(out$need_type[!dep] == "supply_chain"))
  expect_true(all(out$supply_chain_need[!dep] >= 0 & out$supply_chain_need[!dep] <= 1))
  expect_true(out$sn_trusted_in_score[1])
  expect_equal(out$supply_chain_need_n_families[!dep][2], 4)
  # self-supplied (>= 10% of world supply): the import-composition families carry no need, so the score is
  # import dependence alone (over four families)
  expect_true(out$sn_self_supplied[1])
  expect_equal(out$supply_chain_need_score[1], out$sn_import_dependence[1] / 4)
  # not observed widely enough: trusted alternatives is a subcomponent only
  y <- x
  y$trusted_world_share[1:8] <- NA
  expect_false(sp_supply_chain_need(y, io_cfg())$sn_trusted_in_score[1])
  # a thin / excluded cell stays NA (never zero)
  z <- x
  z$strategic_need_percentile[2] <- NA
  expect_true(is.na(sp_supply_chain_need(z, io_cfg())$supply_chain_need[2]))
})
