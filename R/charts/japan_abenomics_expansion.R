# Japan's fiscal, inflationary and growth expansion since Abenomics.
#
# Every series is indexed to 2012 = 100, the last full year before Abenomics. Shinzo
# Abe took office on 26 December 2012, the "three arrows" were announced in January
# 2013, and the Bank of Japan launched quantitative and qualitative easing (QQE) in
# April 2013, so 2012 is the clean pre-policy base.
#
# Metric selection follows the IMF's own Abenomics assessments:
#   * Botman, Danninger and Schiff (eds.), "Can Abenomics Succeed? Overcoming the
#     Legacy of Japan's Lost Decades", IMF, 2015 - ch. 1 frames the three arrows
#     (monetary easing, flexible fiscal policy, structural reform) and judges them on
#     escaping deflation, raising nominal and real growth, and stabilising public debt.
#   * Aoyagi, Ganelli and Murayama, "How Inclusive Is Abenomics?", IMF WP/15/54 -
#     employment and labour-force participation as the growth arrow's reach.
#   * IMF Japan Article IV staff reports (2013-2026) - track general government gross
#     debt, expenditure and revenue (the 2014 and 2019 consumption tax rises), CPI and
#     the GDP deflator as the two inflation gauges, and real and nominal GDP.
#
# Three pillars, plus the policy lever behind the inflation one:
#   fiscal     general government gross debt, expenditure, revenue      (IMF WEO)
#   inflation  CPI, GDP deflator                                        (IMF WEO)
#              BoJ monetary base - the QQE balance-sheet expansion      (BoJ)
#   growth     nominal GDP, real GDP, real GDP per capita, employment   (IMF WEO)
#
# Ratios (debt-to-GDP, primary balance, output gap) are deliberately not indexed:
# a primary balance crosses zero and an index of it is meaningless. Levels are
# indexed instead, so "debt up 38%" and "nominal GDP up 31%" read on one axis; the
# gap between them is the debt ratio. The ratios are kept in the long CSV for context.
#
# Sources
# -------
# IMF World Economic Outlook, latest vintage, via the IMF SDMX 2.1 API:
#   https://api.imf.org/external/sdmx/2.1/data/IMF.RES,WEO/JPN.<codes>.A
# (The IMF DataMapper API is blocked from this network; the SDMX feed carries the same
# WEO database.) WEO values beyond each indicator's LATEST_ACTUAL_ANNUAL_DATA year are
# IMF staff projections and are flagged as such - the fiscal series end a year earlier
# than the national accounts.
#
# Bank of Japan monetary base, average amounts outstanding (MD01 / MABS1AN11), via the
# BoJ Time-Series Data Search API:
#   https://www.stat-search.boj.or.jp/api/v1/getDataCode
# Monthly, averaged to calendar years; a year with fewer than 12 months is dropped.
#
# Outputs (data/processed/charts/):
#   japan_abenomics_expansion_<start>_<end>.csv             tidy long panel: raw values,
#                                                           index, context ratios, and
#                                                           projection years to JAE_END_YEAR
#                                                           (flagged), whatever <end> says
#   japan_abenomics_expansion_<start>_<end>_wide.csv        year x series, all pillars
#   japan_abenomics_expansion_<start>_<end>_wide_<pillar>.csv   one per pillar
# The wide files are Datawrapper-ready: first column `year`, one column per series,
# headers are the legend labels, blanks where a series has no value.
#
# Run:
#   Rscript R/charts/japan_abenomics_expansion.R
#
# Environment:
#   JAE_BASE_YEAR         year indexed to 100 (default 2012)
#   JAE_START_YEAR        first year written (default 2005)
#   JAE_PROJECTIONS       "true" to keep IMF projection years in the wide files
#                         (default false; the long file always has them, flagged)
#   JAE_END_YEAR          last year written when projections are on (default 2030)

source(local({
  resolve_bootstrap_path <- function() {
    candidate_starts <- character()

    sf <- tryCatch(sys.frame(1)$ofile, error = function(e) "")
    if (!is.null(sf) && nzchar(sf)) candidate_starts <- c(candidate_starts, dirname(sf))

    frame_ofiles <- vapply(sys.frames(), function(fr) {
      val <- tryCatch(fr$ofile, error = function(e) "")
      if (is.null(val) || !nzchar(val)) "" else dirname(val)
    }, character(1))
    candidate_starts <- c(candidate_starts, frame_ofiles[nzchar(frame_ofiles)])

    fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(fa) > 0) candidate_starts <- c(candidate_starts, dirname(sub("^--file=", "", fa[1])))

    candidate_starts <- unique(c(candidate_starts, getwd()))

    for (start in candidate_starts) {
      d <- normalizePath(start, winslash = "/", mustWork = FALSE)
      while (dirname(d) != d) {
        bootstrap <- file.path(d, "scripts", "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)

        bootstrap <- file.path(d, "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)

        d <- dirname(d)
      }
    }

    stop("Unable to resolve script path for bootstrap.")
  }

  resolve_bootstrap_path()
}))

## Configuration -------------------------------------------------------------

JAE_DEFAULT_BASE_YEAR <- 2012L
JAE_DEFAULT_START_YEAR <- 2005L
JAE_DEFAULT_END_YEAR <- 2030L

JAE_WEO_API <- "https://api.imf.org/external/sdmx/2.1/data/IMF.RES,WEO"
JAE_BOJ_API <- "https://www.stat-search.boj.or.jp/api/v1/getDataCode"

# One row per indexed line. `slug` fixes the long-file identifiers and column order;
# `label` is the wide-file header, which Datawrapper uses as the legend entry, so a
# relabel changes the chart and nothing else.
JAE_SERIES <- tibble::tribble(
  ~slug,               ~label,                              ~pillar,     ~source, ~code,
  "gg_gross_debt",     "Government gross debt",             "fiscal",    "weo",   "GGXWDG",
  "gg_expenditure",    "Government expenditure",            "fiscal",    "weo",   "GGX",
  "gg_revenue",        "Government revenue",                "fiscal",    "weo",   "GGR",
  "cpi",               "Consumer prices (CPI)",             "inflation", "weo",   "PCPI",
  "gdp_deflator",      "GDP deflator",                      "inflation", "weo",   "NGDP_D",
  "boj_monetary_base", "BoJ monetary base",                 "inflation", "boj",   "MD01:MABS1AN11",
  "nominal_gdp",       "Nominal GDP",                       "growth",    "weo",   "NGDP",
  "real_gdp",          "Real GDP",                          "growth",    "weo",   "NGDP_R",
  "real_gdp_pc",       "Real GDP per capita",               "growth",    "weo",   "NGDPRPC",
  "employment",        "Employment",                        "growth",    "weo",   "LE"
)

# Context ratios carried in the long file only, never indexed (see header).
JAE_CONTEXT <- tibble::tribble(
  ~slug,               ~label,                              ~pillar,     ~code,
  "gg_debt_pct_gdp",   "Government gross debt, % of GDP",   "fiscal",    "GGXWDG_NGDP",
  "gg_primary_bal_pct","Primary balance, % of GDP",         "fiscal",    "GGXONLB_NGDP",
  "cpi_inflation",     "CPI inflation, %",                  "inflation", "PCPIPCH",
  "real_gdp_growth",   "Real GDP growth, %",                "growth",    "NGDP_RPCH",
  "output_gap",        "Output gap, % of potential",        "growth",    "NGAP_NPGDP"
)

## Small helpers -------------------------------------------------------------

jae_http_get <- function(url, accept = "application/json",
                         retries = 3L, timeout_seconds = 180, backoff_seconds = 2) {
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("Package 'curl' is required to pull the source data.")
  }

  last_error <- "unknown error"

  for (attempt in seq_len(max(1L, retries))) {
    result <- tryCatch({
      handle <- curl::new_handle()
      curl::handle_setheaders(
        handle,
        "User-Agent" = "Mozilla/5.0 (compatible; OSI-pipeline/1.0)",
        "Accept" = accept
      )
      curl::handle_setopt(handle, timeout = timeout_seconds, connecttimeout = 60,
                          followlocation = TRUE)
      response <- curl::curl_fetch_memory(url, handle = handle)
      if (response$status_code >= 400L) stop("HTTP ", response$status_code, " for ", url)
      response$content
    }, error = function(e) {
      last_error <<- conditionMessage(e)
      NULL
    })

    if (!is.null(result)) return(result)
    if (attempt < retries) Sys.sleep(backoff_seconds * attempt)
  }

  stop("Request failed after ", retries, " attempts: ", last_error)
}

## IMF WEO pull --------------------------------------------------------------

jae_weo_url <- function(codes, start_year) {
  sprintf("%s/JPN.%s.A?startPeriod=%d", JAE_WEO_API, paste(codes, collapse = "+"), start_year)
}

# SDMX-CSV rows -> one row per indicator-year. `projection` is TRUE past the
# indicator's own latest actual year, which the WEO carries per series.
jae_parse_weo <- function(raw) {
  needed <- c("INDICATOR", "TIME_PERIOD", "OBS_VALUE", "LATEST_ACTUAL_ANNUAL_DATA")
  missing <- setdiff(needed, names(raw))
  if (length(missing) > 0) {
    stop("WEO response is missing column(s): ", paste(missing, collapse = ", "),
         ". The SDMX-CSV layout has changed.")
  }

  latest_actual <- suppressWarnings(as.integer(raw$LATEST_ACTUAL_ANNUAL_DATA))
  out <- data.frame(
    code = as.character(raw$INDICATOR),
    year = as.integer(raw$TIME_PERIOD),
    value = suppressWarnings(as.numeric(raw$OBS_VALUE)),
    projection = !is.na(latest_actual) & as.integer(raw$TIME_PERIOD) > latest_actual,
    stringsAsFactors = FALSE
  )
  vintage <- if ("PUBLICATION_DATE" %in% names(raw)) substr(raw$PUBLICATION_DATE, 1, 10) else NA
  out$vintage <- as.character(vintage)
  out[!is.na(out$value), , drop = FALSE]
}

jae_weo_panel <- function(codes, start_year) {
  body <- jae_http_get(
    jae_weo_url(codes, start_year),
    accept = "application/vnd.sdmx.data+csv;version=1.0.0"
  )
  raw <- readr::read_csv(
    I(rawToChar(body)),
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE
  )
  panel <- jae_parse_weo(raw)

  absent <- setdiff(codes, panel$code)
  if (length(absent) > 0) {
    stop("WEO returned no Japan data for: ", paste(absent, collapse = ", "), ".")
  }
  panel
}

## BoJ pull ------------------------------------------------------------------

jae_boj_monthly <- function(db, code, start_year) {
  url <- sprintf("%s?format=json&lang=en&db=%s&code=%s&startDate=%d01",
                 JAE_BOJ_API, db, code, start_year)
  parsed <- jsonlite::fromJSON(rawToChar(jae_http_get(url)), simplifyVector = FALSE)
  if (!identical(as.integer(parsed$STATUS), 200L) || length(parsed$RESULTSET) == 0) {
    stop("BoJ API returned no data for ", db, "/", code, ": ", parsed$MESSAGE)
  }

  values <- parsed$RESULTSET[[1]]$VALUES
  data.frame(
    period = as.integer(unlist(values$SURVEY_DATES)),
    value = vapply(values$VALUES, function(v) if (is.null(v)) NA_real_ else as.numeric(v),
                   numeric(1))
  )
}

# Calendar-year mean of a YYYYMM monthly series. Partial years are dropped so an
# eight-month 2026 average is never compared with a full-year 2012 base.
jae_annualise_monthly <- function(monthly, min_months = 12L) {
  monthly <- monthly[!is.na(monthly$value), , drop = FALSE]
  monthly$year <- monthly$period %/% 100L
  counts <- table(monthly$year)
  full_years <- as.integer(names(counts)[counts >= min_months])

  annual <- stats::aggregate(value ~ year, data = monthly[monthly$year %in% full_years, ],
                             FUN = mean)
  annual[order(annual$year), , drop = FALSE]
}

## Indexing ------------------------------------------------------------------

# 100 = the base year. The base must be an actual observation: indexing to a
# projection would move every later point if the IMF revised it.
jae_rebase <- function(df, base_year) {
  in_base <- df$year == base_year & !is.na(df$value)
  if (!any(in_base)) stop("No base-year (", base_year, ") observation to index to.")
  if (isTRUE(df$projection[in_base][1])) {
    stop("Base year ", base_year, " is an IMF projection for this series; pick an earlier base.")
  }
  df$index <- 100 * df$value / df$value[in_base][1]
  df
}

## Panel assembly ------------------------------------------------------------

japan_abenomics_panel <- function(base_year = JAE_DEFAULT_BASE_YEAR,
                                  start_year = JAE_DEFAULT_START_YEAR) {
  weo_codes <- c(JAE_SERIES$code[JAE_SERIES$source == "weo"], JAE_CONTEXT$code)
  message("Pulling IMF WEO series for Japan ...")
  weo <- jae_weo_panel(weo_codes, start_year)
  message("  WEO vintage published ", weo$vintage[1])

  message("Pulling BoJ monetary base ...")
  boj <- jae_annualise_monthly(jae_boj_monthly("MD01", "MABS1AN11", start_year))
  boj$code <- "MD01:MABS1AN11"
  boj$projection <- FALSE
  boj$vintage <- NA_character_

  source_rows <- rbind(weo, boj[, names(weo)])

  indexed <- lapply(seq_len(nrow(JAE_SERIES)), function(i) {
    spec <- JAE_SERIES[i, ]
    df <- source_rows[source_rows$code == spec$code, , drop = FALSE]
    df <- jae_rebase(df[order(df$year), , drop = FALSE], base_year)
    df$slug <- spec$slug
    df$kind <- "index"
    message(sprintf("  %-18s %d-%d  %s = %.1f", spec$slug, min(df$year), max(df$year),
                    max(df$year[!df$projection]), df$index[df$year == max(df$year[!df$projection])]))
    df
  })

  context <- lapply(seq_len(nrow(JAE_CONTEXT)), function(i) {
    df <- source_rows[source_rows$code == JAE_CONTEXT$code[i], , drop = FALSE]
    df$index <- NA_real_
    df$slug <- JAE_CONTEXT$slug[i]
    df$kind <- "context"
    df
  })

  specs <- rbind(JAE_SERIES[, c("slug", "label", "pillar")],
                 JAE_CONTEXT[, c("slug", "label", "pillar")])
  panel <- merge(do.call(rbind, c(indexed, context)), specs, by = "slug", all.x = TRUE)
  panel$slug <- factor(panel$slug, levels = specs$slug)
  panel <- panel[order(panel$slug, panel$year), , drop = FALSE]
  panel$slug <- as.character(panel$slug)
  rownames(panel) <- NULL
  panel[, c("slug", "label", "pillar", "kind", "code", "year",
            "value", "index", "projection", "vintage")]
}

# Year x series index table with the legend labels as headers - what Datawrapper
# wants pasted in. Projection years are blanked unless asked for, per series, so the
# fiscal lines stop at their last actual year while GDP carries on.
jae_wide <- function(panel, pillars = unique(JAE_SERIES$pillar),
                     include_projections = FALSE, digits = 1L) {
  specs <- JAE_SERIES[JAE_SERIES$pillar %in% pillars, , drop = FALSE]
  df <- panel[panel$kind == "index" & panel$slug %in% specs$slug, , drop = FALSE]
  if (!include_projections) df <- df[!df$projection, , drop = FALSE]

  wide <- tidyr::pivot_wider(
    df[, c("year", "slug", "index")],
    names_from = "slug", values_from = "index"
  )
  wide <- as.data.frame(wide[order(wide$year), c("year", intersect(specs$slug, names(wide)))])
  wide[-1] <- lapply(wide[-1], round, digits = digits)
  names(wide) <- c("year", specs$label[match(names(wide)[-1], specs$slug)])
  wide
}

## Runner --------------------------------------------------------------------

run_japan_abenomics_expansion <- function(repo_root,
                                          base_year = JAE_DEFAULT_BASE_YEAR,
                                          start_year = JAE_DEFAULT_START_YEAR,
                                          end_year = JAE_DEFAULT_END_YEAR,
                                          include_projections = FALSE,
                                          write_output = TRUE) {
  if (base_year < start_year) stop("Base year ", base_year, " is before start year ", start_year, ".")

  panel <- japan_abenomics_panel(base_year = base_year, start_year = start_year)
  panel <- panel[panel$year >= start_year & panel$year <= end_year, , drop = FALSE]
  if (!write_output) return(invisible(panel))

  out_dir <- file.path(repo_root, config$processed_dir, "charts")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  wide_all <- jae_wide(panel, include_projections = include_projections)
  stem <- sprintf("japan_abenomics_expansion_%d_%d", min(wide_all$year), max(wide_all$year))

  write_csv <- function(df, suffix) {
    path <- file.path(out_dir, paste0(stem, suffix, ".csv"))
    utils::write.csv(df, path, row.names = FALSE, na = "")
    message("Wrote ", path)
  }

  write_csv(panel, "")
  write_csv(wide_all, "_wide")
  for (pillar in unique(JAE_SERIES$pillar)) {
    write_csv(jae_wide(panel, pillar, include_projections), paste0("_wide_", pillar))
  }

  invisible(panel)
}

opsi_japan_abenomics_run_directly <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  length(file_arg) > 0 &&
    identical(basename(sub("^--file=", "", file_arg[1])), "japan_abenomics_expansion.R")
}

if (opsi_japan_abenomics_run_directly()) {
  invisible(run_japan_abenomics_expansion(
    repo_root = repo_root,
    base_year = as.integer(Sys.getenv("JAE_BASE_YEAR", as.character(JAE_DEFAULT_BASE_YEAR))),
    start_year = as.integer(Sys.getenv("JAE_START_YEAR", as.character(JAE_DEFAULT_START_YEAR))),
    end_year = as.integer(Sys.getenv("JAE_END_YEAR", as.character(JAE_DEFAULT_END_YEAR))),
    include_projections = tolower(Sys.getenv("JAE_PROJECTIONS", "false")) %in% c("1", "true", "yes")
  ))
}
