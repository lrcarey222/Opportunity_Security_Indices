# Guards Competitive Position (R/strategic_position/competitive_position.R,
# docs/strategic_position.md section 2): fill semantics, family and score arithmetic, cross-country
# percentiles within a cell, thin cells, stage neutrality, context exclusion and orientation.

cp_repo_root <- function() {
  normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
}

suppressPackageStartupMessages(library(dplyr))
source(file.path(cp_repo_root(), "R", "utils", "supply_chain_stages.R"))
source(file.path(cp_repo_root(), "R", "strategic_position", "sp_helpers.R"))
source(file.path(cp_repo_root(), "R", "strategic_position", "competitive_position.R"))

# One contribution row per call; `imputed` marks a value the pillar builder filled in.
cp_rows <- function(Country, tech, supply_chain, category, variable, component_value, imputed = FALSE) {
  data.frame(
    Country = Country, tech = tech, supply_chain = supply_chain, category = category,
    score_variable = paste("Overall", category), variable = variable,
    component_value = component_value, component_weight_within_overall = 1, category_weight = 1,
    weighted_variable_contribution = component_value, missing_rule_applied = ifelse(imputed, "global_average", NA),
    imputed = imputed, stringsAsFactors = FALSE
  )
}

# A small config shaped like config/strategic_position.yml.
cp_cfg <- function(min_countries = 3, min_vars = 1) {
  list(
    min_observed_variables = min_vars,
    min_countries_per_cell = min_countries,
    families = list(
      export_position = list(
        stages = list("Extraction", "Refining", "Components", "Assembly", "Recovery"),
        variables = list(
          list(pillar = "EO", category = "Trade", variable = "market_share", fill = "na"),
          list(pillar = "EO", category = "Trade", variable = "rca", fill = "na")
        )
      ),
      production_position = list(
        stages = list("Extraction", "Refining"),
        variables = list(list(pillar = "EO", category = "Production", variable = "size", fill = "structural_zero"))
      ),
      manufacturing_capacity = list(
        stages = list("Refining", "Components", "Assembly"),
        variables = list(list(pillar = "EO", category = "Investment", variable = "Operating Capacity Index", fill = "structural_zero"))
      ),
      resource_endowment = list(
        stages = list("Extraction", "Deployment"),
        variables = list(list(pillar = "ES", category = "Reserves", variable_pattern = " Reserves$", fill = "structural_zero"))
      )
    ),
    context_variables = list(
      list(pillar = "EO", category = "Technology Demand", variable = "*", reason = "demand"),
      list(pillar = "EO", category = "Trade", variable = "HHI", reason = "world constant")
    )
  )
}

cp_empty_es <- function() cp_rows("X", "Background", "Extraction", "Trade", "market_share", 0)[0, ]

# Every configured variable exists somewhere in the real pillar tables; a background technology
# gives the fixtures the same property, so the absent-variable check passes. cp_run() drops it
# from the results.
cp_background <- function() list(
  eo = bind_rows(
    cp_rows("X", "Background", "Extraction", "Trade", c("market_share", "rca"), 0.5),
    cp_rows("X", "Background", "Extraction", "Production", "size", 0.5),
    cp_rows("X", "Background", "Refining", "Investment", "Operating Capacity Index", 0.5)
  ),
  es = cp_rows("X", "Background", "Extraction", "Reserves", "Background Reserves", 0.5)
)

cp_run <- function(eo, es = cp_empty_es(), cfg = cp_cfg()) {
  bg <- cp_background()
  res <- sp_competitive_position(bind_rows(eo, bg$eo), bind_rows(es, bg$es), cfg)
  res$cells <- res$cells[res$cells$tech != "Background", ]
  res$inputs <- res$inputs[res$inputs$tech != "Background", ]
  res
}

cells_of <- function(res, tech = NULL, stage = NULL) {
  out <- res$cells
  if (!is.null(tech)) out <- out[out$tech == tech, ]
  if (!is.null(stage)) out <- out[out$supply_chain == stage, ]
  out[order(out$Country), ]
}

test_that("an imputed value is NA under fill na and an observed 0 under structural_zero", {
  eo <- bind_rows(
    cp_rows(c("A", "B", "C"), "Oil", "Extraction", "Trade", "market_share", c(0.9, 0.4, 0.5), c(FALSE, FALSE, TRUE)),
    cp_rows(c("A", "B", "C"), "Oil", "Extraction", "Production", "size", c(0.8, 0.2, 0.5), c(FALSE, FALSE, TRUE))
  )
  res <- cp_run(eo)
  inp <- res$inputs[res$inputs$Country == "C", ]
  ms <- inp[inp$variable == "market_share", ]
  sz <- inp[inp$variable == "size", ]
  expect_true(is.na(ms$value))
  expect_false(ms$observed)
  expect_equal(ms$value_basis, "not_observed")
  expect_equal(sz$value, 0)
  expect_true(sz$observed)
  expect_equal(sz$value_basis, "structural_zero")

  c_cell <- cells_of(res, "Oil", "Extraction")
  expect_equal(c_cell$competitive_position_score[c_cell$Country == "C"], 0)
  expect_true(is.na(c_cell$cp_export_position[c_cell$Country == "C"]))
  expect_equal(c_cell$cp_n_structural_zeros[c_cell$Country == "C"], 1L)
})

test_that("a country the source leaves out of a covered cell is filled by the same rule", {
  eo <- bind_rows(
    cp_rows(c("A", "B", "C"), "Oil", "Extraction", "Trade", "market_share", c(0.9, 0.4, 0.3)),
    cp_rows(c("A", "B"), "Oil", "Extraction", "Production", "size", c(0.8, 0.2))
  )
  res <- cp_run(eo)
  c_size <- res$inputs[res$inputs$Country == "C" & res$inputs$variable == "size", ]
  expect_equal(nrow(c_size), 1)
  expect_equal(c_size$value, 0)
  expect_equal(c_size$value_basis, "structural_zero")
  expect_true(is.na(c_size$component_value))
})

test_that("family score is the mean of its variables and the score the mean of the families", {
  eo <- bind_rows(
    cp_rows("A", "Oil", "Extraction", "Trade", c("market_share", "rca"), c(0.8, 0.4)),
    cp_rows("A", "Oil", "Extraction", "Production", "size", 0.3)
  )
  es <- cp_rows("A", "Oil", "Extraction", "Reserves", "Oil Reserves", 0.9)
  res <- cp_run(eo, es, cp_cfg(min_countries = 1))
  a <- res$cells
  expect_equal(a$cp_export_position, 0.6)
  expect_equal(a$cp_production_position, 0.3)
  expect_equal(a$cp_resource_endowment, 0.9)
  expect_true(is.na(a$cp_manufacturing_capacity))
  # equal family weights: (0.6 + 0.3 + 0.9) / 3, not the mean of the four variables (0.6)
  expect_equal(a$competitive_position_score, 0.6)
  expect_equal(a$cp_n_families, 3L)
  expect_equal(a$cp_n_variables, 4L)
  expect_equal(a$cp_n_families_applicable, 3L)
  expect_equal(a$cp_family_coverage, 1)
  expect_equal(a$cp_basis, "industrial_position")

  # a missing variable drops out of its family's mean instead of counting as zero
  eo2 <- eo
  eo2$component_value[eo2$variable == "rca"] <- NA
  a2 <- cp_run(eo2, es, cp_cfg(min_countries = 1))$cells
  expect_equal(a2$cp_export_position, 0.8)
  expect_equal(a2$cp_n_variables, 3L)

  # intended weights: 1/3 per family, split equally over the family's variables, observed or not
  w <- cp_run(eo2, es, cp_cfg(min_countries = 1))$inputs
  expect_equal(sum(w$weight), 1)
  expect_equal(w$weight[w$variable == "rca"], 1 / 6)
  expect_equal(w$weight[w$variable == "size"], 1 / 3)
})

test_that("coverage counts the families applicable at the stage", {
  eo <- cp_rows(c("A", "B"), "Coal", "Components", "Trade", "market_share", c(0.7, 0.2))
  res <- cp_run(eo, cfg = cp_cfg(min_countries = 1))
  a <- cells_of(res)
  # Components: export_position and manufacturing_capacity apply; Coal has trade only
  expect_equal(a$cp_n_families_applicable, c(2L, 2L))
  expect_equal(a$cp_family_coverage, c(0.5, 0.5))
})

test_that("percentiles and ranks are taken across countries within each cell", {
  sc <- c(0.10, 0.50, 0.30, 0.90, 0.70)
  eo <- bind_rows(
    cp_rows(LETTERS[1:5], "Solar", "Assembly", "Trade", "market_share", sc),
    # another cell with different values and a different country set
    cp_rows(c("A", "B", "C", "Z"), "Wind", "Assembly", "Trade", "market_share", c(0.99, 0.98, 0.97, 0.01))
  )
  res <- cp_run(eo, cfg = cp_cfg(min_countries = 3))
  s <- cells_of(res, "Solar", "Assembly")
  expect_equal(s$competitive_position_percentile, (rank(sc) - 0.5) / 5)
  expect_equal(s$competitive_position_rank, as.integer(rank(-sc)))
  expect_equal(unique(s$cp_n_countries), 5L)
  w <- cells_of(res, "Wind", "Assembly")
  expect_equal(w$competitive_position_percentile, c(3.5, 2.5, 1.5, 0.5) / 4)
  expect_equal(w$competitive_position_rank[w$Country == "A"], 1L)

  # a country's percentile in one cell does not depend on another cell's values
  eo2 <- eo
  eo2$component_value[eo2$tech == "Wind"] <- rev(eo2$component_value[eo2$tech == "Wind"])
  s2 <- cells_of(cp_run(eo2, cfg = cp_cfg(min_countries = 3)), "Solar", "Assembly")
  expect_equal(s2$competitive_position_percentile, s$competitive_position_percentile)
})

test_that("a cell scored for too few countries gets no percentile or rank", {
  eo <- cp_rows(c("A", "B", "C"), "Gas", "Refining", "Trade", "market_share", c(0.2, 0.5, 0.8))
  res <- cp_run(eo, cfg = cp_cfg(min_countries = 5))
  a <- cells_of(res)
  expect_true(all(a$cp_thin_cell))
  expect_true(all(is.na(a$competitive_position_percentile)))
  expect_true(all(is.na(a$competitive_position_rank)))
  expect_equal(a$competitive_position_score, c(0.2, 0.5, 0.8))
  expect_equal(unique(a$cp_n_countries), 3L)

  # countries without a score do not count toward the threshold
  eo2 <- bind_rows(eo, cp_rows(c("D", "E"), "Gas", "Refining", "Trade", "market_share", 0.5, imputed = TRUE))
  a2 <- cells_of(cp_run(eo2, cfg = cp_cfg(min_countries = 4)))
  expect_true(all(a2$cp_thin_cell))
  expect_equal(nrow(a2), 5)
})

test_that("a singleton cell is thin by default and takes 0.5 when allowed", {
  eo <- cp_rows("A", "Nuclear", "Recovery", "Trade", "market_share", 0.4)
  thin <- cp_run(eo, cfg = cp_cfg(min_countries = 5))$cells
  expect_true(thin$cp_thin_cell)
  expect_true(is.na(thin$competitive_position_percentile))
  one <- cp_run(eo, cfg = cp_cfg(min_countries = 1))$cells
  expect_false(one$cp_thin_cell)
  expect_equal(one$competitive_position_percentile, 0.5)
  expect_equal(one$competitive_position_rank, 1L)
})

test_that("min_observed_variables withholds a score backed by too few variables", {
  eo <- bind_rows(
    cp_rows(c("A", "B"), "Oil", "Extraction", "Trade", "market_share", c(0.9, 0.4)),
    cp_rows("A", "Oil", "Extraction", "Trade", "rca", 0.5)
  )
  res <- cp_run(eo, cfg = cp_cfg(min_countries = 1, min_vars = 2))
  a <- cells_of(res)
  expect_equal(a$competitive_position_score, c(0.7, NA))
  expect_equal(a$cp_export_position, c(0.7, 0.4))
})

test_that("relabelling a cell's stage leaves the score unchanged (no stage prior)", {
  vals <- c(0.15, 0.35, 0.55, 0.75, 0.95, 0.05)
  ctry <- LETTERS[1:6]
  make <- function(stage) bind_rows(
    cp_rows(ctry, "Batteries", stage, "Trade", "market_share", vals),
    cp_rows(ctry, "Batteries", stage, "Trade", "rca", rev(vals)),
    cp_rows(ctry, "Batteries", stage, "Investment", "Operating Capacity Index", c(1, 0, 0.5, 0, 0.2, 0), c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE))
  )
  cols <- c("Country", "competitive_position_score", "competitive_position_percentile", "competitive_position_rank",
            "cp_family_coverage", "cp_n_variables")
  comp <- cells_of(cp_run(make("Components"), cfg = cp_cfg()))[, cols]
  asmb <- cells_of(cp_run(make("Assembly"), cfg = cp_cfg()))[, cols]
  expect_equal(as.data.frame(comp), as.data.frame(asmb), ignore_attr = TRUE)

  # with one family applying everywhere, identical values give identical results at all six stages
  cfg <- list(min_countries_per_cell = 3, families = list(trade = list(
    stages = as.list(SUPPLY_CHAIN_STAGES),
    variables = list(list(pillar = "EO", category = "Trade", variable = "market_share", fill = "na"))
  )))
  eo <- bind_rows(lapply(SUPPLY_CHAIN_STAGES, function(s) cp_rows(ctry, "Wind", s, "Trade", "market_share", vals)))
  res <- cp_run(eo, cfg = cfg)$cells
  by_stage <- split(res[order(res$Country), c("competitive_position_score", "competitive_position_percentile")], res$supply_chain[order(res$Country)])
  for (s in SUPPLY_CHAIN_STAGES) expect_equal(by_stage[[s]], by_stage[["Extraction"]], ignore_attr = TRUE, info = s)
  expect_equal(unique(res$cp_basis[res$supply_chain == "Deployment"]), "market_deployment_position")
})

test_that("context variables never change the score", {
  base <- bind_rows(
    cp_rows(LETTERS[1:5], "Solar", "Components", "Trade", "market_share", c(0.1, 0.4, 0.6, 0.8, 0.3)),
    cp_rows(LETTERS[1:5], "Solar", "Components", "Investment", "Operating Capacity Index", c(0.9, 0, 0.2, 0, 0.5))
  )
  ctx_rows <- function(v) bind_rows(
    cp_rows(LETTERS[1:5], "Solar", "Components", "Technology Demand", "Overall Global Demand", v),
    cp_rows(LETTERS[1:5], "Solar", "Components", "Trade", "HHI", v),
    cp_rows(LETTERS[1:5], "Solar", "Components", "Technological Readiness", "TRL Index", v)
  )
  cols <- c("Country", "competitive_position_score", "competitive_position_percentile")
  r0 <- cells_of(cp_run(base, cfg = cp_cfg()))[, cols]
  r1 <- cells_of(cp_run(bind_rows(base, ctx_rows(c(1, 0, 1, 0, 1)))))[, cols]
  r2 <- cells_of(cp_run(bind_rows(base, ctx_rows(c(0, 1, 0, 1, 0)))))[, cols]
  expect_equal(r1, r0, ignore_attr = TRUE)
  expect_equal(r2, r0, ignore_attr = TRUE)

  # the context table carries them, with their reasons
  ctx <- sp_cp_context(bind_rows(base, ctx_rows(rep(0.5, 5))), cp_empty_es(), cp_cfg())
  expect_setequal(unique(ctx$variable), c("Overall Global Demand", "HHI"))
  expect_equal(unique(ctx$reason[ctx$variable == "HHI"]), "world constant")

  # a family that names a context variable, or a demand / readiness / price category, is refused
  bad <- cp_cfg()
  bad$families$export_position$variables[[3]] <- list(pillar = "EO", category = "Trade", variable = "HHI", fill = "na")
  expect_error(cp_run(bind_rows(base, ctx_rows(rep(0.5, 5))), cfg = bad), "context variable")
  bad2 <- cp_cfg()
  bad2$families$export_position$variables[[3]] <- list(pillar = "EO", category = "Technology Demand", variable = "Overall Demand")
  expect_error(cp_run(base, cfg = bad2), "cannot enter Competitive Position")
})

test_that("a configured variable absent at every stage stops with a per-stage list", {
  eo <- cp_rows(c("A", "B"), "Oil", "Extraction", "Trade", "market_share", c(0.9, 0.4))
  cfg <- cp_cfg()
  cfg$families$production_position$variables[[1]]$variable <- "no_such_size"
  err <- tryCatch(cp_run(eo, cfg = cfg), error = function(e) conditionMessage(e))
  expect_match(err, "absent from the data at every stage")
  expect_match(err, "production_position: EO / Production / no_such_size")
  expect_match(err, "Extraction: .*no_such_size")
  expect_match(err, "Refining: .*no_such_size")

  # present at only some stages: no error, and the gap is reported
  res <- cp_run(
    bind_rows(eo, cp_rows("A", "Oil", "Extraction", "Production", "size", 0.4)),
    cp_rows("A", "Oil", "Extraction", "Reserves", "Oil Reserves", 0.2),
    cp_cfg(min_countries = 1)
  )
  cov <- res$variable_coverage
  expect_false(cov$present[cov$family == "production_position" & cov$supply_chain == "Refining"])
  expect_true(cov$present[cov$family == "production_position" & cov$supply_chain == "Extraction"])
})

test_that("orient invert flips observed values and keeps a structural zero at the floor", {
  eo <- cp_rows(c("A", "B", "C", "D"), "Wind", "Assembly", "Investment", "Operating Capacity Index",
                c(0.9, 0.6, 0.2, 0.4), c(FALSE, FALSE, FALSE, TRUE))
  cfg <- cp_cfg(min_countries = 3)
  as_is <- cells_of(cp_run(eo, cfg = cfg))
  cfg$families$manufacturing_capacity$variables[[1]]$orient <- "invert"
  inv_res <- cp_run(eo, cfg = cfg)
  inv <- cells_of(inv_res)
  expect_equal(as_is$competitive_position_score, c(0.9, 0.6, 0.2, 0))
  expect_equal(inv$competitive_position_score, c(0.1, 0.4, 0.8, 0))
  expect_equal(inv$competitive_position_rank, c(3L, 2L, 1L, 4L))
  expect_equal(unique(inv_res$inputs$orient), "invert")
  expect_error(
    { cfg$families$manufacturing_capacity$variables[[1]]$orient <- "upside_down"; cp_run(eo, cfg = cfg) },
    "orient must be one of"
  )
})

test_that("a variable-level stage list keeps a source out of the family's other stages", {
  eo <- bind_rows(
    cp_rows(c("A", "B"), "Solar", "Extraction", "Trade", "market_share", c(0.9, 0.1)),
    cp_rows(c("A", "B"), "Solar", "Refining", "Trade", "market_share", c(0.2, 0.8))
  )
  cfg <- cp_cfg(min_countries = 1)
  cfg$families$export_position$variables[[1]]$stages <- list("Refining")
  cfg$families$export_position$variables[[2]]$stages <- list("Refining")
  res <- cp_run(bind_rows(eo, cp_rows(c("A", "B"), "Solar", "Refining", "Trade", "rca", c(0.5, 0.5))), cfg = cfg)
  expect_false(any(res$cells$supply_chain == "Extraction"))
  cfg$families$export_position$variables[[1]]$stages <- list("Deployment")
  expect_error(cp_run(eo, cfg = cfg), "not stages of family")
})

test_that("a row selected by two families at one stage is refused", {
  eo <- cp_rows(c("A", "B"), "Oil", "Extraction", "Trade", "market_share", c(0.9, 0.4))
  cfg <- cp_cfg()
  cfg$families$production_position$variables[[2]] <- list(pillar = "EO", category = "Trade", variable = "market_share", fill = "na")
  expect_error(cp_run(eo, cfg = cfg), "selected twice")
})

test_that("the shipped config parses and keeps context variables out of the families", {
  cfg <- yaml::read_yaml(file.path(cp_repo_root(), "config", "strategic_position.yml"))$competitive_position
  specs <- sp_cp_specs(cfg)
  expect_true(all(specs$supply_chain %in% SUPPLY_CHAIN_STAGES))
  expect_true(all(specs$fill %in% CP_FILLS))
  ctx <- sp_cp_context_specs(cfg)
  named <- specs[!is.na(specs$variable_spec), ]
  expect_false(any(sp_cp_is_context(named$pillar, named$category, named$variable_spec, ctx)))
  expect_false(any(specs$category %in% CP_FORBIDDEN_CATEGORIES))
  # every stage but Recovery has at least one family; Recovery has none on purpose (its baskets are
  # scrap and spent material, and a large export share means the country ships it abroad for
  # processing, not that it recycles)
  expect_setequal(unique(specs$supply_chain), setdiff(SUPPLY_CHAIN_STAGES, "Recovery"))
})
