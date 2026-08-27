# LCOE competitiveness theme builder functions.
#
# The theme scores two points on each BNEF LCOE curve: the release's current year and its
# 2050 forecast. Both are read from arguments rather than pinned, because the current year
# moves with every BNEF update (the 2025 release was quoted in 2024 real terms, the 2026
# release in 2025 real terms) and the emitted variable names carry that year -
# `lcoe_25`/`lcoe_50` for a 2025-vintage release. scripts/utils/bnef_lcoe.R reads the year
# off the workbook's "Key cost metrics (YYYY real)" banner and passes it in.

lcoe_competitiveness_year_column <- function(year) {
  paste0("X", as.integer(year))
}

# `lcoe_25` for 2025 - the two-digit form the index definition and README already use.
lcoe_competitiveness_variable <- function(year) {
  sprintf("lcoe_%02d", as.integer(year) %% 100L)
}

lcoe_competitiveness_resolve_years <- function(lcoe_bnef,
                                               current_year = NULL,
                                               horizon_year = 2050L) {
  if (is.null(current_year)) {
    current_year <- attr(lcoe_bnef, "reference_year")
  }
  current_year <- suppressWarnings(as.integer(current_year))

  if (length(current_year) != 1 || is.na(current_year)) {
    stop(
      "lcoe_competitiveness() needs the release's current year. It is normally read from ",
      "the workbook's \"Key cost metrics (YYYY real)\" banner by read_bnef_lcoe(); pass ",
      "current_year = explicitly when reading the sheet some other way."
    )
  }

  c(current = current_year, horizon = as.integer(horizon_year))
}

lcoe_competitiveness_clean_bnef <- function(lcoe_bnef, years) {
  year_cols <- vapply(years, lcoe_competitiveness_year_column, character(1), USE.NAMES = FALSE)

  require_columns(
    lcoe_bnef,
    c("Scenario", "Metric", "Technology", "Region", year_cols),
    label = "lcoe_bnef"
  )

  lcoe_bnef %>%
    dplyr::mutate(
      Technology = dplyr::recode(
        Technology,
        "CCGT" = "Gas",
        "Coal" = "Coal",
        "PV fixed-axis" = "Solar",
        "PV fixed-axis + storage" = "Solar",
        "Wind onshore" = "Wind",
        # BNEF retired the 1h utility-scale battery after the 2025 release and quotes a
        # 2h system instead; both are kept so either vintage maps to Batteries.
        "Utility-scale battery (1h)" = "Batteries",
        "Utility-scale battery (2h)" = "Batteries",
        "Utility-scale battery (4h)" = "Batteries",
        .default = NA_character_
      )
    ) %>%
    dplyr::filter(
      Scenario == "Mid",
      Metric == "LCOE",
      !is.na(Technology)
    ) %>%
    dplyr::select(Region, Technology, dplyr::all_of(year_cols))
}

lcoe_competitiveness_build_indices <- function(lcoe_clean, years, gamma = 0.5) {
  year_cols <- vapply(years, lcoe_competitiveness_year_column, character(1), USE.NAMES = FALSE)
  raw_cols <- paste0(vapply(years, lcoe_competitiveness_variable, character(1), USE.NAMES = FALSE), "_raw")
  index_cols <- sub("_raw$", "_index", raw_cols)

  require_columns(lcoe_clean, c("Region", "Technology", year_cols), label = "lcoe_clean")

  indices <- lcoe_clean %>%
    dplyr::group_by(Technology, Region) %>%
    dplyr::summarize(
      dplyr::across(
        dplyr::all_of(year_cols),
        ~ mean(suppressWarnings(as.numeric(.x)), na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    dplyr::rename_with(~ raw_cols, dplyr::all_of(year_cols)) %>%
    dplyr::group_by(Technology) %>%
    # Cheaper is better, so the s-curve is applied to the negated cost.
    dplyr::mutate(dplyr::across(
      dplyr::all_of(raw_cols),
      ~ median_scurve(-.x, gamma = gamma),
      .names = "{sub('_raw$', '_index', .col)}"
    )) %>%
    dplyr::ungroup()

  indices$overall_lcoe_index <- rowMeans(
    indices[, index_cols, drop = FALSE],
    na.rm = TRUE
  )

  indices %>%
    dplyr::select(
      Region,
      Technology,
      dplyr::all_of(raw_cols),
      dplyr::all_of(index_cols),
      overall_lcoe_index
    )
}

lcoe_competitiveness_build_table <- function(lcoe_indices,
                                             years,
                                             source_label = "BNEF LCOE Data Viewer") {
  lcoe_year <- as.integer(max(years))
  value_cols <- setdiff(names(lcoe_indices), c("Region", "Technology"))

  lcoe_indices %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(value_cols),
      names_to = c("variable", "data_type"),
      names_pattern = "(.*)_(raw|index)",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      variable = dplyr::case_when(
        variable == "overall_lcoe" ~ "Overall input cost index",
        TRUE ~ variable
      )
    ) %>%
    dplyr::transmute(
      Country = Region,
      tech = Technology,
      supply_chain = "Downstream",
      category = "Cost Competitiveness",
      variable,
      data_type,
      value,
      Year = lcoe_year,
      source = source_label,
      explanation = dplyr::case_when(
        data_type == "raw" ~ "Levelized cost of energy",
        data_type == "index" ~ "Percent-rank of LCOE across countries"
      )
    )
}

lcoe_competitiveness <- function(lcoe_bnef,
                                 current_year = NULL,
                                 horizon_year = 2050L,
                                 gamma = 0.5,
                                 source_label = "BNEF LCOE Data Viewer") {
  years <- lcoe_competitiveness_resolve_years(
    lcoe_bnef,
    current_year = current_year,
    horizon_year = horizon_year
  )

  lcoe_clean <- lcoe_competitiveness_clean_bnef(lcoe_bnef, years = years)
  lcoe_indices <- lcoe_competitiveness_build_indices(lcoe_clean, years = years, gamma = gamma)
  output <- lcoe_competitiveness_build_table(
    lcoe_indices,
    years = years,
    source_label = source_label
  )

  output <- standardize_theme_table(output)
  validate_schema(output)
  output
}
