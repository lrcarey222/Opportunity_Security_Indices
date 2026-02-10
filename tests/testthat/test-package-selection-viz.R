source(file.path("R", "utils", "scurve.R"))
source(file.path("R", "charts", "package_selection_viz.R"))

test_that("build_country_strategic_tbl returns expected columns", {
  index_outputs <- list(
    economic_opportunity_index = tibble::tibble(
      Country = c("Japan", "Japan", "Japan"),
      tech = c("Solar", "Wind", "Batteries"),
      supply_chain = c("Upstream", "Midstream", "Downstream"),
      Economic_Opportunity_Index = c(0.7, 0.6, 0.8)
    ),
    energy_security_index = tibble::tibble(
      Country = c("Japan", "Japan", "Japan"),
      tech = c("Solar", "Wind", "Batteries"),
      supply_chain = c("Upstream", "Midstream", "Downstream"),
      Energy_Security_Index = c(0.5, 0.4, 0.9)
    ),
    policy_index = tibble::tibble(
      Country = c("Japan", "Japan", "Japan"),
      tech = c("Solar", "Wind", "Batteries"),
      supply_chain = c("Upstream", "Midstream", "Downstream"),
      value = c(0.3, 0.4, 0.5)
    )
  )

  out <- build_country_strategic_tbl(index_outputs, "Japan")

  expect_true(all(c(
    "Country", "tech", "supply_chain", "eo", "es_risk", "pol", "trl_index",
    "sc_weight", "tech_weight", "strategic_index", "sector_label"
  ) %in% names(out)))
  expect_equal(unique(out$Country), "Japan")
})

test_that("CSV builders return wide format", {
  strategic_tbl <- tibble::tibble(
    Country = "Japan",
    tech = c("Solar", "Wind", "Batteries"),
    supply_chain = c("Upstream", "Midstream", "Downstream"),
    eo = c(0.2, 0.5, 0.8),
    es_risk = c(0.7, 0.4, 0.3),
    pol = c(0.1, 0.4, 0.6),
    trl_index = c(0.5, 0.6, 0.7),
    sc_weight = c(0.5, 0.75, 0.25),
    tech_weight = c(0.5, 0.5, 0.5),
    Economic_Opportunity_Index = c(0.6, 0.7, 0.8),
    Energy_Security_Index = c(0.4, 0.6, 0.7),
    strategic_index = c(0.51, 0.62, 0.70),
    sector_label = c("Solar - Upstream", "Wind - Midstream", "Batteries - Downstream")
  )

  contrib_tbl <- tibble::tibble(
    Country = rep("Japan", 4),
    tech = c("Solar", "Solar", "Wind", "Wind"),
    supply_chain = c("Upstream", "Upstream", "Midstream", "Midstream"),
    category = c("Trade", "Policy", "Trade", "Policy"),
    weighted_contribution = c(0.2, 0.3, 0.25, 0.15),
    weight = c(0.5, 0.5, 0.5, 0.5)
  )

  heatmap_wide <- strategic_tbl %>%
    dplyr::select(.data$tech, .data$supply_chain, .data$strategic_index) %>%
    tidyr::pivot_wider(names_from = .data$supply_chain, values_from = .data$strategic_index)

  cat_wide <- build_category_contrib_wide(
    contrib_tbl,
    country = "Japan",
    selected_sector_labels = c("Solar - Upstream", "Wind - Midstream"),
    pillar = "ES"
  )

  expect_false(any(c("name", "key", "variable") %in% names(heatmap_wide)))
  expect_false(any(c("name", "key", "variable") %in% names(cat_wide)))
  expect_true(all(c("Solar - Upstream", "Wind - Midstream") %in% names(cat_wide)))
})

test_that("plotting functions return ggplot objects", {
  strategic_tbl <- tibble::tibble(
    Country = "Japan",
    tech = c("Solar", "Wind", "Batteries"),
    supply_chain = c("Upstream", "Midstream", "Downstream"),
    eo = c(0.2, 0.5, 0.8),
    es_risk = c(0.7, 0.4, 0.3),
    pol = c(0.1, 0.4, 0.6),
    trl_index = c(0.5, 0.6, 0.7),
    sc_weight = c(0.5, 0.75, 0.25),
    tech_weight = c(0.5, 0.5, 0.5),
    Economic_Opportunity_Index = c(0.6, 0.7, 0.8),
    Energy_Security_Index = c(0.4, 0.6, 0.7),
    strategic_index = c(0.51, 0.62, 0.70),
    sector_label = c("Solar - Upstream", "Wind - Midstream", "Batteries - Downstream")
  )

  topn <- build_topn_contrib_tbl(strategic_tbl, top_n = 2)
  topn_long <- attr(topn, "topn_long_tbl")

  cat_wide <- tibble::tibble(
    category = c("Trade", "Policy"),
    `Solar - Upstream` = c(0.2, 0.3),
    `Wind - Midstream` = c(0.25, 0.15),
    weight = c(0.5, 0.5)
  )

  expect_s3_class(plot_country_heatmap(strategic_tbl), "ggplot")
  expect_s3_class(plot_country_scatter(strategic_tbl), "ggplot")
  expect_s3_class(plot_topn_contrib(topn_long), "ggplot")
  expect_s3_class(plot_category_contrib(cat_wide, pillar = "ES"), "ggplot")
})
