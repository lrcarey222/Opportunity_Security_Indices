source(file.path("R", "utils", "scurve.R"))
source(file.path("R", "charts", "package_selection_viz.R"))
source(file.path("R", "charts", "package_selection_raw_highlights.R"))

test_that("build_top_variable_contrib_long returns top_k by sector", {
  var_tbl <- tibble::tibble(
    Country = c("Japan", "Japan", "Japan", "Japan"),
    tech = c("Solar", "Solar", "Wind", "Wind"),
    supply_chain = c("Midstream", "Midstream", "Midstream", "Midstream"),
    category = c("Trade", "Trade", "Energy Access", "Energy Access"),
    variable = c("var_a", "var_b", "var_c", "var_d"),
    weighted_variable_contribution = c(0.5, 0.2, -0.9, 0.1),
    component_value = c(1, 2, 3, 4),
    category_weight = c(0.3, 0.3, 0.7, 0.7),
    imputed = c(FALSE, FALSE, TRUE, FALSE),
    missing_rule_applied = c(FALSE, FALSE, TRUE, FALSE)
  )

  out <- build_top_variable_contrib_long(
    var_contrib_tbl = var_tbl,
    country = "Japan",
    selected_sector_labels = c("Solar - Midstream", "Wind - Midstream"),
    top_k = 1
  )

  expect_equal(nrow(out), 2)
  expect_true(all(out$sector_label %in% c("Solar - Midstream", "Wind - Midstream")))
  expect_true("weighted_variable_contribution" %in% names(out))
})

test_that("extract_raw_values_from_theme_tbls prefers raw rows and latest year", {
  top_long <- tibble::tibble(
    country = "Japan",
    tech = c("Solar", "Wind"),
    supply_chain = c("Midstream", "Midstream"),
    sector_label = c("Solar - Midstream", "Wind - Midstream"),
    category = c("Trade", "Energy Access"),
    variable = c("var_a", "var_c"),
    variable_label = c("Trade :: var_a", "Energy Access :: var_c"),
    weighted_variable_contribution = c(0.5, -0.9)
  )

  theme_tbl <- tibble::tibble(
    Country = c("Japan", "Japan", "Japan", "India", "India"),
    tech = c("Solar", "Solar", "Wind", "Solar", "Wind"),
    supply_chain = c("Midstream", "Midstream", "Midstream", "Midstream", "Midstream"),
    category = c("Trade", "Trade", "Energy Access", "Trade", "Energy Access"),
    variable = c("var_a", "var_a", "var_c", "var_a", "var_c"),
    data_type = c("raw", "index", "raw", "raw", "raw"),
    value = c(20, 0.9, 70, 40, 50),
    Year = c("FY2019", "FY2021", "2023", "2023", "2023"),
    source = "test",
    explanation = "explain"
  )

  out <- extract_raw_values_from_theme_tbls(
    theme_tbls = list(theme_tbl = theme_tbl),
    country = "Japan",
    top_long_tbl = top_long
  )

  expect_equal(out$country_value[out$variable == "var_a"], 20)
  expect_equal(out$country_value[out$variable == "var_c"], 70)
  expect_true(all(out$matched_table_file == "theme_tbl" | is.na(out$matched_table_file)))
})

test_that("build_raw_highlights_tbl computes global stats and joins country values", {
  top_long <- tibble::tibble(
    country = "Japan",
    tech = "Solar",
    supply_chain = "Midstream",
    sector_label = "Solar - Midstream",
    category = "Trade",
    variable = "var_a",
    variable_label = "Trade :: var_a",
    weighted_variable_contribution = 0.5
  )

  theme_tbl <- tibble::tibble(
    Country = c("Japan", "India", "Brazil", "USA"),
    tech = "Solar",
    supply_chain = "Midstream",
    category = "Trade",
    variable = "var_a",
    data_type = "raw",
    value = c(20, 40, 60, 80),
    Year = c("2023", "2023", "2023", "2023"),
    source = "test",
    explanation = "explain"
  )

  out <- build_raw_highlights_tbl(
    theme_tbls = list(theme_tbl = theme_tbl),
    country = "Japan",
    top_long_tbl = top_long
  )

  expect_equal(out$country_value, 20)
  expect_equal(out$global_p50, 50)
  expect_true(all(c("global_p25", "global_p75", "matched_table_file") %in% names(out)))
})

test_that("wide builder pivots sector columns", {
  long_tbl <- tibble::tibble(
    country = c("Japan", "Japan"),
    tech = c("Solar", "Wind"),
    supply_chain = c("Midstream", "Midstream"),
    sector_label = c("Solar - Midstream", "Wind - Midstream"),
    category = c("Trade", "Trade"),
    variable = c("var_a", "var_a"),
    variable_label = c("Trade :: var_a", "Trade :: var_a"),
    weighted_variable_contribution = c(0.4, 0.2)
  )

  wide <- build_top_variable_contrib_wide(long_tbl)

  expect_true(all(c("Solar - Midstream", "Wind - Midstream") %in% colnames(wide)))
  expect_false("sector_label" %in% colnames(wide))
})
