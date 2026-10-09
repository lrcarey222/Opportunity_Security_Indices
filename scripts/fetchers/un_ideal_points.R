# UN General Assembly ideal points: un_ga_ideal_points_all.csv (all-votes series, the primary
# input of the geopolitical alignment) and un_ga_ideal_points.csv (final-passage series, kept as
# a sensitivity).
#
# Source: Bailey, Strezhnev and Voeten (2017), "Estimating dynamic state preferences from United
# Nations voting data", Journal of Conflict Resolution 61(2); data published by Voeten et al. on
# Harvard Dataverse as "United Nations General Assembly Ideal Points", doi:10.7910/DVN/LEJUQZ
# (CC0 1.0). One estimate per country and year (UNGA session) from a dynamic one-dimensional IRT
# model with bridging votes; higher values are closer to the United States and Western positions.
#
# The releases carry two series (codebook of July 2025): "All" (idealpointall), estimated from all
# recorded votes, including votes on paragraphs and amendments ("based on more data and thus
# should be more precise"), and "FP" (idealpointfp), from final-passage votes only, which is
# updated first. The files change with each release (IdealpointestimatesAll_Jun2024.csv,
# IdealpointsJuly2025.tab with both series, IdealpointestimatesFP_2026FP.csv with FP only), so the
# fetchers read the latest published version's file list from the Dataverse API (public, no key)
# and write one series under a stable name in a fixed schema:
#   ccode, iso3c, country_name, session, year, ideal_point, ideal_point_q05, ideal_point_q50,
#   ideal_point_q95, n_votes, estimate, source_file, dataset_version
# `estimate` names the vote set behind ideal_point ("All" or "FP"). Rows without an estimate are
# dropped.
# - un_ga_ideal_points_all: the newest country-year file that carries the all-votes series. In
#   Dataverse V39.0 (2026-07-30) that is IdealpointsJuly2025.tab, whose all-votes column runs
#   1946-2023 (its 2024 rows carry final-passage estimates only); the 2026 file has final-passage
#   estimates only (1946-2025), so the all-votes series ends two years before the final-passage one.
# - un_ga_ideal_points: the newest country-year file that carries the final-passage series (the
#   2026 file in V39.0). Asking for the series by name keeps an all-votes-only file out of
#   un_ga_ideal_points.csv, which scripts/12 reads as the final-passage series.
# Consumer: scripts/12_build_geopolitical_alignment.R (config/geopolitical_distance.yml
# alignment$series picks the file).

UNGA_DATAVERSE_BASE <- "https://dataverse.harvard.edu"
UNGA_IDEAL_POINTS_DOI <- "doi:10.7910/DVN/LEJUQZ"

# Ideal-point series in order of preference, as normalized column suffixes.
UNGA_IDEAL_POINT_SERIES <- c(FP = "fp", All = "all", Default = "", Legacy = "legacy")

# File list of a Dataverse dataset version (the `files` element of the API's version object,
# parsed with simplifyVector = FALSE) as a data frame.
unga_dataverse_files <- function(version) {
  files <- version$files
  if (length(files) == 0) {
    return(data.frame(
      label = character(), id = character(), creation_date = character(),
      tabular = logical(), description = character(), stringsAsFactors = FALSE
    ))
  }
  pick <- function(x, default = NA_character_) if (is.null(x) || length(x) == 0) default else x
  data.frame(
    label = vapply(files, function(f) as.character(pick(f$label, pick(f$dataFile$filename))), character(1)),
    id = vapply(files, function(f) as.character(pick(f$dataFile$id)), character(1)),
    creation_date = vapply(files, function(f) as.character(pick(f$dataFile$creationDate)), character(1)),
    tabular = vapply(files, function(f) isTRUE(f$dataFile$tabularData), logical(1)),
    description = vapply(files, function(f) as.character(pick(f$description, "")), character(1)),
    stringsAsFactors = FALSE
  )
}

# The country-year ideal-point files, newest first: CSV or tab-delimited files whose name starts
# with "Idealpoint" and that are not the dyadic version. Newest = latest creation date, then
# highest file id.
unga_candidate_ideal_point_files <- function(files) {
  is_candidate <- grepl("^ideal ?points?", files$label, ignore.case = TRUE) &
    !grepl("dyad", files$label, ignore.case = TRUE) &
    grepl("\\.(csv|tab|tsv)$", files$label, ignore.case = TRUE)
  candidates <- files[is_candidate, , drop = FALSE]
  if (nrow(candidates) == 0) {
    stop("No country-year ideal-point file in the Dataverse dataset (files: ",
         paste(files$label, collapse = ", "), ").")
  }
  ord <- order(candidates$creation_date, suppressWarnings(as.numeric(candidates$id)), decreasing = TRUE)
  out <- candidates[ord, , drop = FALSE]
  rownames(out) <- NULL
  out
}

# The newest country-year ideal-point file.
unga_select_ideal_point_file <- function(files) {
  unga_candidate_ideal_point_files(files)[1, , drop = FALSE]
}

# Normalized column names of an upstream file (case and punctuation dropped).
unga_column_keys <- function(df) tolower(gsub("[^A-Za-z0-9]", "", names(df)))

# The series an upstream file carries, in order of preference (names of UNGA_IDEAL_POINT_SERIES).
unga_available_series <- function(df) {
  key <- unga_column_keys(df)
  names(UNGA_IDEAL_POINT_SERIES)[paste0("idealpoint", UNGA_IDEAL_POINT_SERIES) %in% key]
}

# One release in its upstream layout -> the fixed schema above. Column names differ across
# releases ("IdealPointFP" / "idealpointfp", "Q5%FP" / "q5fp", "Countryname" / "countryname"),
# so they are matched case- and punctuation-insensitively. `series` (a name of
# UNGA_IDEAL_POINT_SERIES, e.g. "All") requires that series; NULL takes the first the file
# carries in the order FP, All, Default, Legacy.
unga_standardize_ideal_points <- function(df, source_file = NA_character_, dataset_version = NA_character_,
                                          series = NULL) {
  if (!is.data.frame(df) || nrow(df) == 0) stop("Ideal-point file is empty.")
  key <- unga_column_keys(df)
  col <- function(name) {
    hit <- which(key == name)
    if (length(hit) == 0) NULL else df[[hit[1]]]
  }

  available <- unga_available_series(df)
  if (is.null(series)) {
    if (length(available) == 0) {
      stop("No ideal-point column found; columns are: ", paste(names(df), collapse = ", "))
    }
    series <- available[1]
  } else {
    if (length(series) != 1 || !series %in% names(UNGA_IDEAL_POINT_SERIES)) {
      stop("series must be one of: ", paste(names(UNGA_IDEAL_POINT_SERIES), collapse = ", "))
    }
    if (!series %in% available) {
      stop("No ", series, " ideal-point column (idealpoint", UNGA_IDEAL_POINT_SERIES[[series]],
           "); columns are: ", paste(names(df), collapse = ", "))
    }
  }
  suffix <- UNGA_IDEAL_POINT_SERIES[[series]]

  ccode <- col("ccode")
  iso3c <- col("iso3c")
  country_name <- col("countryname")
  year <- col("year")
  if (is.null(ccode) || is.null(iso3c) || is.null(country_name) || is.null(year)) {
    stop("Ideal-point file lacks ccode, iso3c, Countryname or year; columns are: ",
         paste(names(df), collapse = ", "))
  }
  year <- suppressWarnings(as.integer(year))

  # Session s of the General Assembly opens in September of year 1945 + s; the releases label
  # each session by that year. Files without a session column get it from the year.
  session <- col("session")
  if (is.null(session)) {
    session <- year - 1945L
  } else {
    session <- suppressWarnings(as.integer(session))
    bad <- !is.na(session) & !is.na(year) & session != year - 1945L
    if (any(bad)) {
      stop(sum(bad), " row(s) where session != year - 1945; the session-year convention changed.")
    }
  }

  num <- function(name) {
    x <- col(name)
    if (is.null(x)) rep(NA_real_, nrow(df)) else suppressWarnings(as.numeric(x))
  }
  n_votes <- col(paste0("nvotes", suffix))
  n_votes <- if (is.null(n_votes)) rep(NA_integer_, nrow(df)) else suppressWarnings(as.integer(n_votes))

  out <- data.frame(
    ccode = suppressWarnings(as.integer(ccode)),
    iso3c = toupper(trimws(as.character(iso3c))),
    country_name = trimws(as.character(country_name)),
    session = session,
    year = year,
    ideal_point = num(paste0("idealpoint", suffix)),
    ideal_point_q05 = num(paste0("q5", suffix)),
    ideal_point_q50 = num(paste0("q50", suffix)),
    ideal_point_q95 = num(paste0("q95", suffix)),
    n_votes = n_votes,
    estimate = series,
    source_file = as.character(source_file),
    dataset_version = as.character(dataset_version),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$ideal_point) & !is.na(out$session), , drop = FALSE]
  out <- out[order(out$ccode, out$session), , drop = FALSE]
  rownames(out) <- NULL
  out
}

unga_read_delimited <- function(text, tabular_or_tab) {
  if (tabular_or_tab) {
    utils::read.delim(text = text, check.names = FALSE, stringsAsFactors = FALSE, quote = "\"")
  } else {
    utils::read.csv(text = text, check.names = FALSE, stringsAsFactors = FALSE)
  }
}

# Release label of a Dataverse version object, e.g. "V39.0 (2026-07-30)".
unga_dataset_version <- function(version) {
  paste0(
    "V", version$versionNumber, ".", version$versionMinorNumber,
    if (!is.null(version$releaseTime)) paste0(" (", substr(version$releaseTime, 1, 10), ")") else ""
  )
}

# The newest file of `candidates` (unga_candidate_ideal_point_files() output) that carries
# `series`, standardized; series NULL takes the newest file in its preferred series.
# `read_file(file)` returns a candidate's upstream data frame (injected, so the choice can be
# tested offline). Files are read newest first; the first that carries the series wins.
unga_pick_series_file <- function(candidates, read_file, series = NULL, dataset_version = NA_character_) {
  if (nrow(candidates) == 0) stop("No candidate ideal-point files.")
  if (is.null(series)) candidates <- candidates[1, , drop = FALSE]
  tried <- character()
  for (k in seq_len(nrow(candidates))) {
    file <- candidates[k, , drop = FALSE]
    df <- read_file(file)
    if (is.null(series) || series %in% unga_available_series(df)) {
      return(unga_standardize_ideal_points(
        df,
        source_file = file$label, dataset_version = dataset_version, series = series
      ))
    }
    tried <- c(tried, paste0(file$label, " (", paste(unga_available_series(df), collapse = "/"), ")"))
  }
  stop("No country-year ideal-point file carries the ", series, " series; tried: ", paste(tried, collapse = ", "))
}

unga_fetch_ideal_points <- function(base = UNGA_DATAVERSE_BASE, doi = UNGA_IDEAL_POINTS_DOI, series = NULL) {
  meta <- opsi_http_get_json(paste0(base, "/api/datasets/:persistentId/?persistentId=", doi))
  version <- meta$data$latestVersion
  if (is.null(version)) stop("Dataverse returned no published version for ", doi)

  candidates <- unga_candidate_ideal_point_files(unga_dataverse_files(version))
  read_file <- function(file) {
    # Ingested (.tab) files are served as tab-delimited text by default; CSVs as stored.
    text <- opsi_http_get_text(paste0(base, "/api/access/datafile/", file$id))
    unga_read_delimited(text, file$tabular || grepl("\\.(tab|tsv)$", file$label, ignore.case = TRUE))
  }
  unga_pick_series_file(candidates, read_file, series = series, dataset_version = unga_dataset_version(version))
}

register_fetcher(
  id = "un_ga_ideal_points",
  description = "UN General Assembly ideal points, Bailey-Strezhnev-Voeten (Harvard Dataverse), final-passage series",
  contract = list(
    required_columns = c("ccode", "iso3c", "country_name", "session", "year", "ideal_point", "n_votes"),
    # The 1946-2025 release has 11,612 country-sessions; a much shorter file is a partial pull.
    min_rows = 10000,
    unique_key = c("ccode", "session")
  ),
  fn = function() unga_fetch_ideal_points(series = "FP")
)

register_fetcher(
  id = "un_ga_ideal_points_all",
  description = "UN General Assembly ideal points, Bailey-Strezhnev-Voeten (Harvard Dataverse), all-votes series",
  contract = list(
    required_columns = c("ccode", "iso3c", "country_name", "session", "year", "ideal_point", "n_votes", "estimate"),
    # The 1946-2023 all-votes series has 11,232 country-years; a much shorter file is a
    # partial pull.
    min_rows = 10000,
    unique_key = c("ccode", "session")
  ),
  fn = function() unga_fetch_ideal_points(series = "All")
)
