# LCOE competitiveness theme builder functions.
#
# The theme scores two points on each BNEF LCOE curve: the release's current year and its
# 2050 forecast. Both are read from arguments rather than pinned, because the current year
# moves with every BNEF update (the 2025 release was quoted in 2024 real terms, the 2026
# release in 2025 real terms) and the emitted variable names carry that year -
# `lcoe_25`/`lcoe_50` for a 2025-vintage release. scripts/utils/bnef_lcoe.R reads the year
# off the workbook's "Key cost metrics (YYYY real)" banner and passes it in.
#
# The Cost Competitiveness category scores one LCOE variable, "LCOE Index" = the mean of the
# two point indices. The category used to list lcoe_<yy>, lcoe_50 and their mean (then named
# "Overall input cost index") beside the input-cost indices, so LCOE carried 3 of 4 votes
# wherever BNEF covers the market. lcoe_<yy> and lcoe_50 are still emitted for inspection.

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

# `economies` (ISO3 codes, normally country_info's) keeps the markets the index scores;
# NULL keeps every BNEF market.
lcoe_competitiveness_clean_bnef <- function(lcoe_bnef, years, economies = NULL) {
  year_cols <- vapply(years, lcoe_competitiveness_year_column, character(1), USE.NAMES = FALSE)

  require_columns(
    lcoe_bnef,
    c("Scenario", "Metric", "Technology", "Region", year_cols),
    label = "lcoe_bnef"
  )

  # One benchmark plant per technology, so every market is ranked on the same asset. The
  # regions are ranked against each other within a technology, and a variant that only some
  # of them report would move those regions alone: "PV fixed-axis + storage" (13 of the 29
  # fixed-axis markets in the 2026 release) used to be averaged into Solar, scoring those 13
  # on a costlier bundle. For the same reason offshore wind (12 markets) and the tracking
  # and "+ storage" variants are left out. Both battery durations are kept: every market
  # that quotes one quotes the other.
  lcoe_bnef %>%
    dplyr::mutate(
      Technology = dplyr::recode(
        Technology,
        "CCGT" = "Gas",
        "Coal" = "Coal",
        "PV fixed-axis" = "Solar",
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
    # Canonical names before the ranking ("China (Mainland)", "US", "UK", "Turkey" and
    # "Vietnam" in the 2026 release), and only markets the index scores take a rank: Taiwan
    # used to be ranked here and dropped when the table met country_info.
    dplyr::mutate(Region = standardize_country_names(Region)) %>%
    dplyr::filter(!is_aggregate_or_residual(Region)) %>%
    dplyr::filter(
      is.null(economies) | country_name_to_iso3(Region) %in% toupper(as.character(economies))
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
  # Every row carries the release's reference year: the 2050 value is that release's
  # projection, not a 2050 observation, and stamping the rows 2050 read as a data year.
  lcoe_year <- as.integer(years[["current"]])
  horizon_variable <- lcoe_competitiveness_variable(years[["horizon"]])
  value_cols <- setdiff(names(lcoe_indices), c("Region", "Technology"))

  lcoe_indices %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(value_cols),
      names_to = c("variable", "data_type"),
      names_pattern = "(.*)_(raw|index)",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      # The mean of the two point indices, and the one LCOE variable the Cost
      # Competitiveness category scores. It was named "Overall input cost index", which
      # read as the ILO/IMF Input Cost Index it sits beside.
      variable = dplyr::case_when(
        variable == "overall_lcoe" ~ "LCOE Index",
        TRUE ~ variable
      )
    ) %>%
    dplyr::transmute(
      Country = Region,
      tech = Technology,
      # The cost of generating from (or storing in) an installed plant: Deployment.
      supply_chain = "Deployment",
      category = "Cost Competitiveness",
      variable,
      data_type,
      value,
      Year = lcoe_year,
      source = source_label,
      explanation = dplyr::case_when(
        variable == "LCOE Index" ~
          "Mean of the current-year and 2050 LCOE indices",
        data_type == "raw" & variable == horizon_variable ~
          paste0("Levelized cost of electricity in ", years[["horizon"]], ", BNEF Mid scenario ($/MWh, real)"),
        data_type == "raw" ~
          paste0("Levelized cost of electricity in ", years[["current"]], ", BNEF Mid scenario ($/MWh, real)"),
        TRUE ~ "S-curve of -LCOE across BNEF markets within the technology (cheaper scores higher)"
      )
    )
}

# `country_info` (optional) restricts the ranking to its economies.
lcoe_competitiveness <- function(lcoe_bnef,
                                 current_year = NULL,
                                 horizon_year = 2050L,
                                 gamma = 0.5,
                                 source_label = "BNEF LCOE Data Viewer",
                                 country_info = NULL) {
  years <- lcoe_competitiveness_resolve_years(
    lcoe_bnef,
    current_year = current_year,
    horizon_year = horizon_year
  )

  lcoe_clean <- lcoe_competitiveness_clean_bnef(
    lcoe_bnef,
    years = years,
    economies = if (is.null(country_info)) NULL else country_info$iso3c
  )
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
