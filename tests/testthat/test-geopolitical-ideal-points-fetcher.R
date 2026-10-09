# UN General Assembly ideal-point fetchers (scripts/fetchers/un_ideal_points.R): the all-votes
# series (un_ga_ideal_points_all, the primary input of the geopolitical alignment) and the
# final-passage series (un_ga_ideal_points). Offline: the Dataverse reads are injected stubs.

geo_fetch_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

geo_load_fetchers <- function() {
  source(file.path(geo_fetch_root, "scripts", "utils", "fetchers.R"), local = FALSE)
  source_fetcher_files(geo_fetch_root)
}

# The V39.0 (2026-07-30) listing: the 2026 file has final-passage estimates only; the July 2025
# file has both series; the June 2024 file is the legacy all-votes one.
geo_fixture_listing <- function() {
  data.frame(
    label = c(
      "AgreementScores.csv", "IdealPointDyads1946-2025.tab", "IdealpointestimatesAll_Jun2024.csv",
      "IdealpointestimatesFP_2026FP.csv", "IdealpointsJuly2025.tab"
    ),
    id = c("11837234", "13642051", "10295878", "14098429", "11824371"),
    creation_date = c("2025-07-31", "2026-04-02", "2024-06-24", "2026-07-26", "2025-07-28"),
    tabular = c(FALSE, TRUE, FALSE, FALSE, TRUE),
    description = "",
    stringsAsFactors = FALSE
  )
}

# Upstream layouts of the three country-year files.
geo_fixture_upstream <- function(label) {
  switch(
    label,
    "IdealpointestimatesFP_2026FP.csv" = data.frame(
      ccode = c(2L, 710L), session = c(80L, 80L), NVotesFP = c(58L, 61L), IdealPointFP = c(2.89, -0.37),
      `Q5%FP` = c(2.6, -0.5), `Q50%FP` = c(2.9, -0.37), `Q95%FP` = c(3.2, -0.2),
      iso3c = c("USA", "CHN"), Countryname = c("United States", "China"), year = c(2025L, 2025L),
      check.names = FALSE, stringsAsFactors = FALSE
    ),
    "IdealpointsJuly2025.tab" = data.frame(
      ccode = c(2L, 710L, 2L, 710L), iso3c = c("USA", "CHN", "USA", "CHN"),
      countryname = c("United States", "China", "United States", "China"),
      year = c(2023, 2023, 2024, 2024),
      idealpointfp = c(2.1, -0.8, 2.2, -0.9), idealpointall = c(2.3, -1.2, NA, NA),
      idealpointlegacy = c(2.2, -1.1, NA, NA),
      nvotesfp = c(60L, 61L, 62L, 63L), q5fp = c(1.9, -1.0, 2.0, -1.1), q50fp = c(2.1, -0.8, 2.2, -0.9),
      q95fp = c(2.4, -0.6, 2.5, -0.7),
      nvotesall = c(150L, 160L, NA, NA), q5all = c(2.1, -1.4, NA, NA), q50all = c(2.3, -1.2, NA, NA),
      q95all = c(2.5, -1.0, NA, NA),
      stringsAsFactors = FALSE
    ),
    "IdealpointestimatesAll_Jun2024.csv" = data.frame(
      ccode = 2L, session = 78L, IdealPointAll = 2.0, NVotesAll = 140L, iso3c = "USA",
      Countryname = "United States", year = 2023L, stringsAsFactors = FALSE
    ),
    stop("unexpected file ", label)
  )
}

test_that("candidate files are the country-year ideal-point files, newest first", {
  geo_load_fetchers()
  cand <- unga_candidate_ideal_point_files(geo_fixture_listing())
  expect_equal(cand$label, c(
    "IdealpointestimatesFP_2026FP.csv", "IdealpointsJuly2025.tab", "IdealpointestimatesAll_Jun2024.csv"
  ))
  expect_equal(unga_select_ideal_point_file(geo_fixture_listing())$label, "IdealpointestimatesFP_2026FP.csv")
  expect_error(unga_candidate_ideal_point_files(geo_fixture_listing()[1:2, ]), "No country-year ideal-point file")
})

test_that("the all-votes fetch takes the newest file that carries the all-votes series", {
  geo_load_fetchers()
  cand <- unga_candidate_ideal_point_files(geo_fixture_listing())
  read <- character()
  reader <- function(file) {
    read <<- c(read, file$label)
    geo_fixture_upstream(file$label)
  }
  out <- unga_pick_series_file(cand, reader, series = "All", dataset_version = "V39.0 (2026-07-30)")
  # The FP-only 2026 file is read and skipped; the July 2025 file wins; the legacy file is not read.
  expect_equal(read, c("IdealpointestimatesFP_2026FP.csv", "IdealpointsJuly2025.tab"))
  expect_equal(unique(out$estimate), "All")
  expect_equal(unique(out$source_file), "IdealpointsJuly2025.tab")
  expect_equal(unique(out$dataset_version), "V39.0 (2026-07-30)")
  # Only the years with an all-votes estimate (2023), the all-votes columns.
  expect_equal(out$year, c(2023L, 2023L))
  expect_equal(out$session, c(78L, 78L))
  expect_equal(out$ideal_point, c(2.3, -1.2))
  expect_equal(out$n_votes, c(150L, 160L))
  expect_equal(out$ideal_point_q05, c(2.1, -1.4))
  expect_equal(out$ideal_point_q95, c(2.5, -1.0))

  # Without a series the newest file is used in its preferred series (final passage).
  read <- character()
  fp <- unga_pick_series_file(cand, reader)
  expect_equal(read, "IdealpointestimatesFP_2026FP.csv")
  expect_equal(unique(fp$estimate), "FP")
  expect_equal(fp$ideal_point, c(2.89, -0.37))
  # The final-passage fetcher asks for FP by name: the newest file that carries it, never an
  # all-votes-only file even when that one comes first.
  read <- character()
  fp_named <- unga_pick_series_file(cand, reader, series = "FP")
  expect_equal(read, "IdealpointestimatesFP_2026FP.csv")
  expect_equal(fp_named, fp)
  read <- character()
  fp_older <- unga_pick_series_file(cand[c(3, 2), , drop = FALSE], reader, series = "FP")
  expect_equal(read, c("IdealpointestimatesAll_Jun2024.csv", "IdealpointsJuly2025.tab"))
  expect_equal(unique(fp_older$estimate), "FP")
  expect_equal(unique(fp_older$source_file), "IdealpointsJuly2025.tab")

  # No file with the series: an error naming what each file carries.
  expect_error(
    unga_pick_series_file(cand[1, , drop = FALSE], reader, series = "All"),
    "No country-year ideal-point file carries the All series; tried: IdealpointestimatesFP_2026FP.csv \\(FP\\)"
  )
})

test_that("the series argument picks the series columns and refuses a file without them", {
  geo_load_fetchers()
  jul <- geo_fixture_upstream("IdealpointsJuly2025.tab")
  expect_equal(unga_available_series(jul), c("FP", "All", "Legacy"))
  expect_equal(unique(unga_standardize_ideal_points(jul)$estimate), "FP")
  legacy <- unga_standardize_ideal_points(jul, series = "Legacy")
  expect_equal(legacy$ideal_point, c(2.2, -1.1))
  expect_true(all(is.na(legacy$n_votes)))
  fp_only <- geo_fixture_upstream("IdealpointestimatesFP_2026FP.csv")
  expect_error(unga_standardize_ideal_points(fp_only, series = "All"), "No All ideal-point column")
  expect_error(unga_standardize_ideal_points(jul, series = "all"), "series must be one of")
})

test_that("both fetchers are registered with contracts on the fixed schema", {
  geo_load_fetchers()
  for (id in c("un_ga_ideal_points", "un_ga_ideal_points_all")) {
    f <- get_fetcher(id)
    expect_false(is.null(f), info = id)
    expect_equal(f$contract$unique_key, c("ccode", "session"), info = id)
    expect_true(all(c("iso3c", "session", "ideal_point", "n_votes") %in% f$contract$required_columns), info = id)
    expect_gte(f$contract$min_rows, 10000)
  }
  expect_true("estimate" %in% get_fetcher("un_ga_ideal_points_all")$contract$required_columns)
  # Each fetcher names its series, so neither file can silently carry the other's.
  fn_src <- function(id) paste(deparse(body(get_fetcher(id)$fn)), collapse = " ")
  expect_match(fn_src("un_ga_ideal_points"), 'series = "FP"', fixed = TRUE)
  expect_match(fn_src("un_ga_ideal_points_all"), 'series = "All"', fixed = TRUE)

  manifest <- yaml::read_yaml(file.path(geo_fetch_root, "config", "raw_inputs_manifest.yml"))
  entries <- if (!is.null(manifest$inputs)) manifest$inputs else manifest
  ids <- vapply(entries, function(e) if (is.null(e$id)) NA_character_ else e$id, character(1))
  all_entry <- entries[[match("un_ga_ideal_points_all", ids)]]
  expect_equal(all_entry$path, "un_ga_ideal_points_all.csv")
  expect_equal(all_entry$fetch_policy, "prefer")
  expect_true("scripts/12_build_geopolitical_alignment.R" %in% unlist(all_entry$required_by))
})

test_that("the fetched all-votes file, when present, is one series with one row per country and year", {
  path <- file.path(geo_fetch_root, "data", "raw", "un_ga_ideal_points_all.csv")
  skip_if_not(file.exists(path), "un_ga_ideal_points_all.csv not fetched")
  x <- utils::read.csv(path, stringsAsFactors = FALSE)
  expect_equal(unique(x$estimate), "All")
  expect_false(anyDuplicated(x[c("ccode", "session")]) > 0)
  expect_true(all(x$session == x$year - 1945L))
  expect_gte(max(x$year), 2023L)
  expect_true(all(c("USA", "CHN") %in% x$iso3c[x$year == max(x$year)]))
})
