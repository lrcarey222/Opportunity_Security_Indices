repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "levels.R"))
source(file.path(repo_root, "R", "indices", "index_builder_core.R"))
source(file.path(repo_root, "R", "indices", "build_energy_security_index_v2.R"))
source(file.path(repo_root, "R", "indices", "build_economic_opportunity_index_v2.R"))

# A small pillar that exercises every aggregation rule at once:
#   Access    = mean(a1, a2, a3). a1 has a global_average rule (C is imputed), a2 has no rule
#               (B's a2 stays NA and drops out of the mean), a3 has a zero rule but no country
#               is observed, so it must stay NA rather than become 0 for everyone.
#   Supply    = mean(s1, s2), no rule: B has neither, so B's Supply category is NA.
#   Stability = a direct score variable, NA for C.
#   "Overall Decoy Index" is a second overall in the Access category that is not a score
#   variable (like ES building EO's "Overall Trade Index"); its components must not leak into
#   variable_contributions.
#   Wind-Upstream has two countries with nothing observed: the whole cell is unscored.
core_index_definition <- function() {
  categories <- list(
    Access = list(score_variable = "Overall Access Index"),
    Supply = list(score_variable = "Overall Supply Index"),
    Stability = list(score_variable = "Stability Score")
  )
  list(
    pillars = list(
      energy_security = list(categories = categories),
      economic_opportunity = list(categories = categories)
    ),
    overall_variables = list(
      `Overall Access Index` = list(category = "Access", components = c("a1", "a2", "a3"), aggregation = "mean"),
      `Overall Decoy Index` = list(category = "Access", components = c("a1", "a2"), aggregation = "mean"),
      `Overall Supply Index` = list(category = "Supply", components = c("s1", "s2"), aggregation = "mean")
    ),
    variable_levels = list()
  )
}

core_theme_rows <- function(Country, tech, category, variable, value) {
  tibble::tibble(
    Country = Country,
    tech = tech,
    supply_chain = "Upstream",
    category = category,
    variable = variable,
    value = value,
    Year = 2024,
    data_type = "index",
    source = "Test",
    explanation = "Example"
  )
}

core_theme_tables <- function() {
  abc <- c("A", "B", "C")
  list(
    t_access = core_theme_rows(abc, "Solar", "Access", "a1", c(0.2, 0.6, NA)),
    t_access2 = core_theme_rows(abc, "Solar", "Access", "a2", c(0.9, NA, 0.5)),
    t_access3 = core_theme_rows(abc, "Solar", "Access", "a3", c(NA_real_, NA_real_, NA_real_)),
    t_supply = dplyr::bind_rows(
      core_theme_rows(abc, "Solar", "Supply", "s1", c(0.7, NA, 0.3)),
      core_theme_rows(abc, "Solar", "Supply", "s2", c(0.1, NA, NA))
    ),
    t_stab = dplyr::bind_rows(
      core_theme_rows(abc, "Solar", "Stability", "Stability Score", c(0.5, 0.8, NA)),
      core_theme_rows(c("D", "E"), "Wind", "Stability", "Stability Score", c(NA_real_, NA_real_))
    )
  )
}

core_missing_data <- list(t_access = "global_average", t_access3 = "zero")
core_weights <- list(Access = 3, Supply = 2, Stability = 1)

build_core_es <- function(weights = core_weights) {
  options(opportunity_security.index_definition = core_index_definition())
  build_energy_security_index_v2(
    theme_tables = core_theme_tables(),
    weights = weights,
    missing_data = core_missing_data,
    allow_partial_categories = TRUE,
    include_sub_sector = FALSE
  )
}

key_cols <- c("Country", "tech", "supply_chain")

test_that("apply_missing_policy imputes only where the group observes at least one country", {
  tbl <- tibble::tibble(
    Country = c("A", "B", "A", "B"),
    tech = "Solar",
    supply_chain = "Upstream",
    sub_sector = "All",
    category = "Access",
    variable = c("x", "x", "y", "y"),
    theme = c("t_zero", "t_zero", "t_zero_empty", "t_zero_empty"),
    value = c(0.4, NA, NA, NA)
  )
  rules <- parse_missing_policy(list(t_zero = "zero", t_zero_empty = "zero"), tbl)

  out <- apply_missing_policy(tbl, rules)

  x_b <- dplyr::filter(out, variable == "x", Country == "B")
  expect_equal(x_b$value, 0)
  expect_true(x_b$imputed)
  expect_equal(x_b$missing_rule_applied, "zero")

  # No country observed in (Solar, Upstream, Access, y, t_zero_empty): nothing to impute from.
  y_rows <- dplyr::filter(out, variable == "y")
  expect_true(all(is.na(y_rows$value)))
  expect_false(any(y_rows$imputed))
  expect_true(all(is.na(y_rows$missing_rule_applied)))

  unobserved <- unobserved_component_groups(out)
  expect_equal(unobserved$variable, "y")
  expect_equal(unobserved$n_countries, 2L)
})

test_that("apply_missing_policy gives grid-created rows the 'All' sub_sector placeholder", {
  tbl <- tibble::tibble(
    Country = c("A", "B"),
    tech = "Solar",
    supply_chain = "Upstream",
    sub_sector = "All",
    category = c("Access", "Supply"),
    variable = c("x", "s"),
    theme = "t",
    value = c(0.4, 0.5)
  )

  out <- apply_missing_policy(tbl, parse_missing_policy(list(t = "global_average"), tbl))

  expect_equal(nrow(out), 4L)
  expect_true(all(out$sub_sector == "All"))
})

test_that("overall_component_weights reproduce mean and median overalls exactly", {
  x <- c(0.9, NA, 0.1, 0.4, 0.7)
  w_mean <- overall_component_weights(x, "mean")
  expect_equal(w_mean, c(0.25, 0, 0.25, 0.25, 0.25))
  expect_equal(sum(w_mean * dplyr::coalesce(x, 0)), mean(x, na.rm = TRUE), tolerance = 1e-15)

  w_median_even <- overall_component_weights(x, "median")
  expect_equal(sum(w_median_even * dplyr::coalesce(x, 0)), stats::median(x, na.rm = TRUE), tolerance = 1e-15)

  odd <- c(0.3, 0.8, 0.5)
  expect_equal(sum(overall_component_weights(odd, "median") * odd), 0.5)

  expect_equal(overall_component_weights(c(NA_real_, NA_real_), "mean"), c(0, 0))
})

test_that("compute_overall_scores builds each overall once and flags multi-row imputation", {
  tbl <- tibble::tibble(
    Country = c("A", "A", "B", "B"),
    tech = "Solar",
    supply_chain = "Upstream",
    sub_sector = "All",
    category = "Access",
    variable = c("a1", "a2", "a1", "a2"),
    theme = "t",
    value = c(0.2, 0.6, 0.4, NA),
    imputed = c(TRUE, FALSE, FALSE, FALSE),
    missing_rule_applied = c("global_average", NA, NA, NA)
  )
  index_definition <- list(
    overall_variables = list(
      `Overall Access Index` = list(category = "Access", components = c("a1", "a2")),
      `Overall Other Index` = list(category = "Access", components = "a1")
    )
  )

  out <- compute_overall_scores(tbl, index_definition)

  # Regression: binding the named list of per-overall tables used to collapse each overall
  # into one all-NA row.
  expect_false(anyNA(out$overall_scores$Country))
  expect_equal(nrow(out$overall_scores), 4L)

  access <- dplyr::filter(out$overall_scores, variable == "Overall Access Index")
  expect_equal(access$value[access$Country == "A"], 0.4)
  expect_equal(access$value[access$Country == "B"], 0.4)
  # A's overall has two rows and one imputed component; isTRUE() on that vector was FALSE.
  expect_true(access$imputed[access$Country == "A"])
  expect_false(access$imputed[access$Country == "B"])
  expect_equal(access$component_count[access$Country == "B"], 1L)

  components <- dplyr::filter(out$component_contributions, overall_variable == "Overall Access Index")
  b <- dplyr::filter(components, Country == "B")
  expect_equal(b$component_weight_within_overall[b$variable == "a1"], 1)
  expect_equal(b$component_weight_within_overall[b$variable == "a2"], 0)
})

test_that("an NA category drops out of the pillar with its weight (available-case)", {
  outputs <- suppressWarnings(build_core_es())
  index <- outputs$energy_security_index

  value_of <- function(country) index$Energy_Security_Index[index$Country == country]
  expect_equal(value_of("A"), (3 * 0.55 + 2 * 0.4 + 1 * 0.5) / 6, tolerance = 1e-12)
  # B has no Supply score: it is renormalized over Access and Stability, not scored 0.
  expect_equal(value_of("B"), (3 * 0.6 + 1 * 0.8) / 4, tolerance = 1e-12)
  # C's a1 is imputed (0.4), a2 observed, a3 unobserved everywhere (NA, not 0).
  expect_equal(value_of("C"), (3 * mean(c(0.4, 0.5)) + 2 * 0.3) / 5, tolerance = 1e-12)

  # Nothing observed in the Wind cell: NA, recorded, not invented.
  expect_true(all(is.na(index$Energy_Security_Index[index$tech == "Wind"])))
  expect_setequal(outputs$diagnostics$unscored_groups$Country, c("D", "E"))
  cells <- outputs$diagnostics$index_cells
  expect_equal(cells$cell_status[cells$tech == "Wind"], "unscored")
  expect_equal(cells$cell_status[cells$tech == "Solar"], "ok")

  weights <- outputs$category_contributions %>%
    dplyr::filter(Country == "B", category == "Supply")
  expect_true(is.na(weights$category_score))
  expect_equal(weights$category_weight, 0)
  expect_equal(weights$weighted_category_contribution, 0)

  expect_true("a3" %in% outputs$diagnostics$unobserved_components$variable)

  imputed_c <- outputs$category_scores %>%
    dplyr::filter(Country == "C", category == "Access")
  expect_true(imputed_c$imputed)
  expect_equal(imputed_c$missing_rule_applied, "global_average")

  # C's imputed a1 carries half of Access, which carries 3/5 of C's index.
  imputation <- outputs$diagnostics$index_imputation
  share_of <- function(country) imputation$imputed_weight_share[imputation$Country == country]
  expect_equal(share_of("C"), 0.5 * 3 / 5, tolerance = 1e-12)
  expect_equal(share_of("A"), 0)
  expect_true(is.na(share_of("D")))
})

test_that("compute_index_and_contributions applies the available-case rule to generic inputs", {
  # The partnership-strength builder calls it this way: one variable per category, no
  # score_variable column, and NaN for a category with nothing observed.
  category_scores <- tibble::tibble(
    Country = c("A", "A", "A", "B", "B", "C"),
    tech = "Solar",
    supply_chain = "Upstream",
    category = c("x", "y", "z", "x", "y", "x"),
    category_score = c(0.2, NaN, 0.8, NaN, 0.5, NaN)
  )
  variable_contributions <- category_scores %>%
    dplyr::mutate(
      variable = category,
      component_value = category_score,
      component_weight_within_overall = 1,
      missing_rule_applied = NA_character_,
      imputed = FALSE
    ) %>%
    dplyr::select(-category_score)
  weights_tbl <- tibble::tibble(category = c("x", "y", "z"), weight = c(1, 2, 3))

  out <- compute_index_and_contributions(category_scores, variable_contributions, weights_tbl)

  index <- out$index_tbl
  expect_equal(index$Index[index$Country == "A"], (1 * 0.2 + 3 * 0.8) / 4)
  expect_equal(index$Index[index$Country == "B"], 0.5)
  expect_true(is.na(index$Index[index$Country == "C"]))
  expect_equal(out$unscored_groups$Country, "C")

  a <- dplyr::filter(out$variable_contributions, Country == "A")
  expect_equal(sum(a$weighted_variable_contribution), index$Index[index$Country == "A"])
  expect_equal(a$category_weight[a$category == "y"], 0)

  # Strict mode: every group misses an observed category, so every index is NA.
  strict <- compute_index_and_contributions(
    category_scores, variable_contributions, weights_tbl,
    allow_partial_categories = FALSE
  )
  expect_true(all(is.na(strict$index_tbl$Index)))
})

test_that("category and variable contributions reconcile exactly to the index", {
  check_reconciles <- function(outputs, index_col) {
    index <- outputs[["index"]] %>% dplyr::filter(!is.na(.data[[index_col]]))

    category_sums <- outputs$category_contributions %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) %>%
      dplyr::summarize(total_category = sum(weighted_category_contribution), .groups = "drop")
    variable_sums <- outputs$variable_contributions %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) %>%
      dplyr::summarize(total_variable = sum(weighted_variable_contribution), .groups = "drop")

    combined <- index %>%
      dplyr::inner_join(category_sums, by = key_cols) %>%
      dplyr::inner_join(variable_sums, by = key_cols)

    expect_equal(nrow(combined), nrow(index))
    expect_lt(max(abs(combined$total_category - combined[[index_col]])), 1e-12)
    expect_lt(max(abs(combined$total_variable - combined[[index_col]])), 1e-12)

    # Only score-variable components, each once.
    vc <- outputs$variable_contributions
    expect_false("Overall Decoy Index" %in% vc$score_variable)
    expect_equal(anyDuplicated(vc[, c(key_cols, "category", "variable")]), 0L)
  }

  check_reconciles(suppressWarnings(build_core_es()), "Energy_Security_Index")

  eo_outputs <- suppressWarnings(build_economic_opportunity_index_v2(
    theme_tables = core_theme_tables(),
    weights = core_weights,
    missing_data = core_missing_data,
    allow_partial_categories = TRUE,
    include_sub_sector = FALSE,
    index_definition = core_index_definition()
  ))
  check_reconciles(eo_outputs, "Economic_Opportunity_Index")
})

test_that("a configured category whose score variable is never built warns once, by name", {
  index_definition <- core_index_definition()
  index_definition$pillars$energy_security$categories$`Energy Prices` <-
    list(score_variable = "Overall Energy Prices Index")
  index_definition$overall_variables$`Overall Energy Prices Index` <-
    list(category = "Energy Prices", components = "price_volatility", aggregation = "mean")
  options(opportunity_security.index_definition = index_definition)

  theme_tables <- core_theme_tables()
  theme_tables$energy_prices <- core_theme_rows(
    c("A", "B", "C"), "Solar", "Energy Prices", "Overall Price Volatility Index", 0.6
  )

  warnings <- character()
  outputs <- withCallingHandlers(
    build_energy_security_index_v2(
      theme_tables = theme_tables,
      weights = c(core_weights, list(`Energy Prices` = 2, Orphan = 4)),
      missing_data = core_missing_data,
      allow_partial_categories = TRUE,
      include_sub_sector = FALSE
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  prices <- grep("Energy Prices", warnings, value = TRUE)
  expect_length(prices, 1)
  expect_match(prices, "'Overall Energy Prices Index' was not built", fixed = TRUE)
  expect_match(prices, "Overall Price Volatility Index", fixed = TRUE)

  # The orphan weight (no category definition at all) is reported once by the index step.
  orphan <- grep("Orphan", warnings, value = TRUE)
  expect_length(orphan, 1)
  expect_match(orphan, "weight is dropped")

  expect_equal(outputs$diagnostics$categories_unproduced$category, "Energy Prices")
  expect_false("Energy Prices" %in% outputs$category_scores$category)
})

test_that("a theme composite standing in for an unbuilt configured overall is reported", {
  # Supply's configured components (s1, s2) are gone, but the theme still ships its own
  # "Overall Supply Index" rows - as most real themes do. They used to stand in for the
  # configured overall without a word, so the unproduced-score guard never saw the defect.
  theme_tables <- core_theme_tables()
  theme_tables$t_supply <- core_theme_rows(
    c("A", "B", "C"), "Solar", "Supply", "Overall Supply Index", c(0.9, 0.1, 0.5)
  )
  collect_warnings <- function(expr) {
    warnings <- character()
    value <- withCallingHandlers(expr, warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
    list(value = value, warnings = warnings)
  }

  options(opportunity_security.index_definition = core_index_definition())
  es <- collect_warnings(build_energy_security_index_v2(
    theme_tables = theme_tables,
    weights = core_weights,
    missing_data = core_missing_data,
    allow_partial_categories = TRUE,
    include_sub_sector = FALSE
  ))
  supply <- grep("Supply", es$warnings, value = TRUE)
  expect_length(supply, 1)
  expect_match(supply, "Supply ('Overall Supply Index')", fixed = TRUE)
  expect_equal(es$value$diagnostics$categories_substituted$category, "Supply")
  expect_equal(nrow(es$value$diagnostics$categories_unproduced), 0L)
  # The category still scores, from the theme rows; the warning is the only signal.
  scores <- dplyr::filter(es$value$category_scores, category == "Supply")
  expect_equal(scores$category_score[scores$Country == "A"], 0.9)

  eo <- collect_warnings(build_economic_opportunity_index_v2(
    theme_tables = theme_tables,
    weights = core_weights,
    missing_data = core_missing_data,
    allow_partial_categories = TRUE,
    include_sub_sector = FALSE,
    index_definition = core_index_definition()
  ))
  expect_length(grep("Overall Supply Index", eo$warnings, value = TRUE), 1)
  expect_equal(eo$value$diagnostics$categories_substituted$category, "Supply")

  # With its configured components present the overall is built and nothing is reported.
  expect_equal(nrow(suppressWarnings(build_core_es())$diagnostics$categories_substituted), 0L)
})

test_that("the repo configs score Energy Prices, fill Reserves and carry no orphan weights", {
  index_definition <- yaml::read_yaml(file.path(repo_root, "config", "index_definition.yml"))
  weights <- yaml::read_yaml(file.path(repo_root, "config", "weights.yml"))
  missing_data <- yaml::read_yaml(file.path(repo_root, "config", "missing_data.yml"))

  # The energy_prices theme emits "Overall Price Volatility Index" (R/categories/energy_prices).
  expect_true(
    "Overall Price Volatility Index" %in%
      index_definition$overall_variables$`Overall Energy Prices Index`$components
  )

  expect_equal(missing_data$energy_security$reserves, "zero")
  expect_null(missing_data$economic_opportunity$lcoe_competitiveness)

  # Every weighted category must be a configured category of its pillar, so the configured
  # weights are the ones the index can realise.
  for (pillar in c("energy_security", "economic_opportunity")) {
    configured <- names(index_definition$pillars[[pillar]]$categories)
    expect_setequal(names(weights[[pillar]]), configured)
  }
})
