# Shared theme-table standardization helpers.
#
# These were previously defined inline in scripts/10_build_themes.R. They are pure
# transformations, so they live in R/ and are sourced by every script that has to bring a
# theme table onto the common schema - the annual pipeline and the vintage builder in
# scripts/40_build_index_vintages.R alike.
#
# standardize_theme_types() is also where a theme's country universe is settled: names are
# put on one canonical spelling per economy (R/utils/country.R), aggregate and residual rows
# are dropped, and keys that two spellings of one economy now share are collapsed to one.

rebuild_theme_overall_indices <- function(tbl) {
  if (is.null(tbl) || nrow(tbl) == 0 || !"variable" %in% names(tbl) || !"data_type" %in% names(tbl)) {
    return(tbl)
  }

  index_definition <- getOption("opportunity_security.index_definition")
  if (is.null(index_definition)) {
    return(tbl)
  }

  overall_defs <- index_definition$overall_variables
  overall_names <- names(overall_defs)

  if (is.null(overall_defs) || length(overall_names) == 0) {
    return(tbl)
  }

  include_sub_sector <- "sub_sector" %in% names(tbl)
  tbl_without_overall <- tbl %>%
    dplyr::filter(!(data_type == "index" & variable %in% overall_names))

  apply_overall_definitions(tbl_without_overall, include_sub_sector = include_sub_sector)
}

# Drop rows that are not a single present-day economy (world and bloc totals, regions, EI
# "Other ..." residuals, USSR, WDI aggregate codes). Left in, each is ranked and imputed as
# if it were a country, which inflates every S-curve denominator it sits in.
#
# A table made only of such rows passes through untouched: a technology-level table (the
# TRL theme is Country == "Global" throughout) has no country rows to keep, and its
# consumer decides how to broadcast it.
drop_aggregate_country_rows <- function(tbl) {
  if (is.null(tbl) || nrow(tbl) == 0 || !"Country" %in% names(tbl)) {
    return(tbl)
  }

  iso3c <- if ("iso3c" %in% names(tbl)) tbl$iso3c else NULL
  flagged <- is_aggregate_or_residual(tbl$Country, iso3c)

  if (!any(flagged) || all(flagged)) {
    return(tbl)
  }

  dropped <- sort(unique(tbl$Country[flagged]))
  message(
    "standardize_theme_types(): dropped ", sum(flagged), " aggregate/residual row(s) (",
    paste(utils::head(dropped, 8), collapse = ", "),
    if (length(dropped) > 8) paste0(", +", length(dropped) - 8, " more") else "",
    ")."
  )

  # Builders hand over grouped tibbles; filter ungrouped and put the groups back.
  regroup_theme_table(dplyr::ungroup(tbl)[!flagged, , drop = FALSE], dplyr::group_vars(tbl))
}

regroup_theme_table <- function(tbl, groups) {
  if (length(groups) == 0) {
    return(tbl)
  }
  dplyr::group_by(tbl, dplyr::across(dplyr::any_of(groups)))
}

# Resolve keys that two source spellings of one economy share once their names are merged
# ("Vietnam" and "Viet Nam" rows of the same tech x stage x variable x year).
#
# The choice is made per spelling, so rows that legitimately share a key within one
# spelling (several sources, several sub-sectors) are all kept. For each colliding key the
# spelling that is kept is, in order: one with a non-NA value; the canonical spelling (the
# one that already reads as the canonical name after the curated recodes); the spelling
# with more rows in the table; the alphabetically first. Two spellings with different
# non-NA values are a real conflict and raise a warning with counts.
collapse_country_spelling_duplicates <- function(tbl, spelling_col = ".country_input") {
  if (is.null(tbl) || nrow(tbl) == 0 || !spelling_col %in% names(tbl) || !"Country" %in% names(tbl)) {
    return(tbl)
  }

  key_cols <- intersect(
    c("Country", "tech", "supply_chain", "sub_sector", "category", "variable", "data_type", "Year"),
    names(tbl)
  )

  # Builders hand over grouped tibbles, which would leak into the counts and row ids below.
  groups <- dplyr::group_vars(tbl)
  keyed <- tbl %>%
    dplyr::ungroup() %>%
    dplyr::mutate(.spelling = .data[[spelling_col]], .row_id = dplyr::row_number())

  merged_countries <- keyed %>%
    dplyr::distinct(Country, .spelling) %>%
    dplyr::count(Country, name = "n_spellings") %>%
    dplyr::filter(n_spellings > 1)

  if (nrow(merged_countries) == 0) {
    return(tbl)
  }

  candidates <- keyed %>%
    dplyr::filter(Country %in% merged_countries$Country)

  per_spelling <- candidates %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(key_cols, ".spelling")))) %>%
    dplyr::summarise(
      .has_value = any(!is.na(value)),
      .signature = paste(sort(signif(value[!is.na(value)], 12)), collapse = "|"),
      .groups = "drop"
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup()

  if (nrow(per_spelling) == 0) {
    return(tbl)
  }

  prevalence <- candidates %>%
    dplyr::count(Country, .spelling, name = ".prevalence")

  ranked <- per_spelling %>%
    dplyr::left_join(prevalence, by = c("Country", ".spelling")) %>%
    dplyr::mutate(.canonical = dplyr::coalesce(country_clean_names(.spelling) == Country, FALSE)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) %>%
    dplyr::arrange(
      dplyr::desc(.has_value),
      dplyr::desc(.canonical),
      dplyr::desc(.prevalence),
      .spelling,
      .by_group = TRUE
    ) %>%
    dplyr::mutate(
      .winner = dplyr::row_number() == 1L,
      .conflict = dplyr::n_distinct(.signature[.has_value]) > 1L
    ) %>%
    dplyr::ungroup()

  losers <- ranked %>%
    dplyr::filter(!.winner) %>%
    dplyr::select(dplyr::all_of(c(key_cols, ".spelling")))

  out <- keyed %>%
    dplyr::anti_join(losers, by = c(key_cols, ".spelling")) %>%
    dplyr::arrange(.row_id) %>%
    dplyr::select(-".spelling", -".row_id")

  kept <- ranked %>% dplyr::filter(.winner)
  conflicts <- kept %>% dplyr::filter(.conflict)
  merged <- sort(unique(kept$Country))

  message(
    "standardize_theme_types(): ", nrow(kept), " key(s) carried more than one spelling of ",
    length(merged), " economy/economies (", paste(utils::head(merged, 8), collapse = ", "),
    if (length(merged) > 8) ", ..." else "", "); kept one spelling per key, ",
    nrow(tbl) - nrow(out), " row(s) dropped."
  )

  if (nrow(conflicts) > 0) {
    by_country <- conflicts %>% dplyr::count(Country, sort = TRUE)
    warning(
      "standardize_theme_types(): ", nrow(conflicts), " of ", nrow(kept),
      " merged key(s) had different non-NA values under different spellings; kept the ",
      "canonical (else the more common) spelling's value (",
      paste0(by_country$Country, " ", by_country$n, collapse = ", "),
      ").",
      call. = FALSE
    )
  }

  regroup_theme_table(out, groups)
}

# Country harmonisation shared by both standardize_theme_types() paths: drop aggregate
# rows, then resolve keys the merged spellings now share.
settle_theme_countries <- function(tbl) {
  tbl %>%
    drop_aggregate_country_rows() %>%
    collapse_country_spelling_duplicates()
}

standardize_theme_types <- function(tbl, country_info = NULL) {
  if (is.null(tbl)) {
    return(tbl)
  }

  core_cols <- c(
    "Country", "iso3c", "tech", "supply_chain", "sub_sector",
    "category", "variable", "data_type", "value", "Year", "source", "explanation"
  )

  keep_theme_schema <- function(x) {
    x %>%
      dplyr::select(dplyr::any_of(core_cols))
  }

  standardized <- tbl %>%
    dplyr::mutate(
      Country = as.character(Country),
      tech = as.character(tech),
      supply_chain = as.character(supply_chain),
      sub_sector = if ("sub_sector" %in% names(tbl)) as.character(sub_sector) else NULL,
      category = as.character(category),
      variable = as.character(variable),
      data_type = as.character(data_type),
      # First four-digit run, matching standardize_theme_table() and normalize_year(); the
      # end-anchored form could not parse an ISO date such as "2022-01-01".
      Year = suppressWarnings(as.integer(stringr::str_extract(as.character(Year), "\\d{4}"))),
      value = suppressWarnings(as.numeric(value)),
      source = as.character(source),
      explanation = as.character(explanation)
    ) %>%
    keep_theme_schema()

  # The source spelling of each row, carried until keys shared by two spellings of one
  # economy have been resolved.
  with_spelling <- standardized %>%
    dplyr::mutate(.country_input = stringr::str_squish(Country))

  if (is.null(country_info)) {
    # No WDI roster to join, so codes come from the names (ungrouped: one countrycode call);
    # aggregates stay uncoded.
    named <- standardize_country_table(with_spelling, country_info = NULL)
    harmonized <- dplyr::ungroup(named) %>%
      dplyr::mutate(iso3c = dplyr::coalesce(iso3c, country_name_to_iso3(Country))) %>%
      regroup_theme_table(dplyr::group_vars(named)) %>%
      settle_theme_countries() %>%
      keep_theme_schema()
    assert_country_iso3_consistent(harmonized, label = "standardize_theme_types()")
    return(rebuild_theme_overall_indices(harmonized))
  }

  standardized_with_country <- standardize_country_table(
    with_spelling,
    country_info = country_info
  )

  # Guard against full row-loss when country matching fails for a dataset.
  # In that case, preserve the standardized rows and allow downstream missing-data
  # handling to proceed rather than returning an empty table.
  if (nrow(standardized_with_country) == 0 && nrow(standardized) > 0) {
    warning(
      "Country standardization dropped all rows in standardize_theme_types(); ",
      "returning unfiltered standardized rows before rebuilding overall indices."
    )
    return(rebuild_theme_overall_indices(standardized))
  }

  # This path has never rebuilt the theme's overall rows (an earlier duplicate of the
  # standardize_country_table() call returned before the rebuild could run); it still does
  # not, so the processed theme tables keep their builders' own overall rows.
  settled <- standardized_with_country %>%
    settle_theme_countries() %>%
    keep_theme_schema()

  assert_country_iso3_consistent(settled, label = "standardize_theme_types()")
  settled
}
