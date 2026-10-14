# China activity indicators, rebased to a common year.
#
# Replicates the JPMAM "China activity indicators" chart - exports by value,
# industrial production, retail sales, fixed asset investment and home sales, all
# rebased to a common index - and extends it with three cleantech export series
# (batteries, solar PV, EVs) from Ember's China cleantech export tracker.
#
# The point of the chart is the divergence: exports and industrial production keep
# compounding while property-linked demand (home sales, fixed asset investment)
# falls away, so China's growth is increasingly carried by what it sells abroad.
# The Ember series say what a growing share of that export growth now is.
#
# Sources
# -------
# Macro series are NBS (National Bureau of Statistics) and GACC (China customs)
# releases, pulled through East Money's public macro-data API:
#   https://datacenter-web.eastmoney.com/api/data/v1/get
# NBS's own portal (data.stats.gov.cn) is the primary source, but it is blocked
# from a lot of corporate networks and returns 403 rather than data, so this script
# uses the East Money mirror, which republishes the same NBS/GACC prints and is
# current to the latest monthly release. Series used:
#   RPT_ECONOMY_CUSTOMS       EXIT_BASE            exports, USD thousands   (GACC)
#   RPT_ECONOMY_INDUS_GROW    BASE_SAME            industrial value-added, % y/y
#   RPT_ECONOMY_TOTAL_RETAIL  RETAIL_TOTAL         retail sales, RMB 100mn
#   RPT_ECONOMY_ASSET_INVEST  BASE                 fixed asset investment, RMB 100mn
#   RPT_INDUSTRY_INDEX        EMI01523157          commercial building sales, RMB 10k
#
# Cleantech exports come from Ember's China cleantech export data explorer:
#   https://ember-energy.org/data/china-cleantech-exports-data-explorer/
#   https://files.ember-energy.org/public-downloads/customs/outputs/clean_tech_exports_full_release_monthly.csv
# World-total monthly export value in USD for Batteries, Solar PV and EVs. Ember's
# panel starts in 2018, so those three lines are shorter than the macro ones.
#
# Three things to know before reading the output
# ----------------------------------------------
#   * Industrial production is a VOLUME index; everything else is nominal value.
#     NBS publishes industrial value-added only as a growth rate, so the level
#     index here is built by chaining year-on-year growth off the base year
#     (base-year months seeded at 100, then propagated forwards and backwards).
#     The other series are indexed straight off their own base-year mean.
#   * NBS does not publish a separate January. Industrial production, retail sales
#     and fixed asset investment are released as a combined January-February print,
#     which is split evenly across the two months here; home sales are not published
#     cumulatively in this feed at all, so its January and February are linearly
#     interpolated. Every filled point is flagged in the `imputed` column. The
#     even split understates the February 2020 COVID collapse in particular.
#   * NBS publishes these series unadjusted, and the seasonality is not a detail:
#     fixed asset investment swings by a factor of three between February and
#     December of the same year. Every value series is therefore seasonally adjusted
#     here by STL on logs before indexing (`value_sa`), and the default
#     3-month centred moving average in `index_smooth` mops up the Chinese New Year
#     residual that no fixed monthly factor can track. `value` keeps the raw print.
#
# Outputs (data/processed/charts/):
#   china_activity_indicators_<start>_<end>.csv        tidy long panel
#   china_activity_indicators_<start>_<end>_wide.csv   date x series, smoothed index
#   china_activity_indicators_<start>_<end>_macro.png  the five JPMAM series
#   china_activity_indicators_<start>_<end>_cleantech.png  the three Ember series
#   china_activity_indicators_<start>_<end>_all.png    all eight together
#
# Run:
#   Rscript R/charts/china_activity_indicators.R
#
# Environment:
#   CAI_BASE_YEAR   year whose 12-month mean is 100 (default 2021)
#   CAI_START_YEAR  first year plotted (default 2016)
#   CAI_SMOOTH      centred moving-average window in months, odd (default 3; 1 = off)

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

CAI_DEFAULT_BASE_YEAR <- 2021L
CAI_DEFAULT_START_YEAR <- 2016L
CAI_DEFAULT_SMOOTH <- 3L

CAI_EM_API <- "https://datacenter-web.eastmoney.com/api/data/v1/get"
CAI_EM_REFERER <- "https://data.eastmoney.com/cjsj/"

CAI_EMBER_CSV <- paste0(
  "https://files.ember-energy.org/public-downloads/customs/outputs/",
  "clean_tech_exports_full_release_monthly.csv"
)

# One row per line on the chart. `slug` fixes the CSV column names and file-stem
# ordering so a relabelled series does not silently reshuffle the outputs. Colours
# follow the JPMAM original for the five macro series; the Ember lines take a
# separate, cooler ramp so the cleantech block reads as a group.
#
# `seasonal` says whether the series needs deseasonalising. Everything published as
# a monthly value does - Chinese New Year and the December construction push move
# fixed asset investment by a factor of three within a year, which buries the trend
# the chart is about. Industrial production is the exception: it is chained from
# year-on-year growth, so it never carries a seasonal pattern to remove.
CAI_SERIES <- tibble::tribble(
  ~slug,             ~label,                    ~group,      ~colour,     ~seasonal,
  "exports",         "Exports by value",        "macro",     "#D8232A",   TRUE,
  "industrial",      "Industrial production",   "macro",     "#5C5F2E",   FALSE,
  "retail",          "Retail sales",            "macro",     "#B8912F",   TRUE,
  "fai",             "Fixed asset investment",  "macro",     "#7B3F9D",   TRUE,
  "home_sales",      "Home sales",              "macro",     "#1F78B4",   TRUE,
  "ember_batteries", "Battery exports",         "cleantech", "#00857A",   TRUE,
  "ember_solar",     "Solar PV exports",        "cleantech", "#E8833A",   TRUE,
  "ember_evs",       "EV exports",              "cleantech", "#3B6DB5",   TRUE
)

## Small helpers -------------------------------------------------------------

cai_default <- function(x, fallback) if (is.null(x) || length(x) == 0) fallback else x

cai_num <- function(x) suppressWarnings(as.numeric(x))

cai_month_start <- function(x) as.Date(paste0(substr(as.character(x), 1, 7), "-01"))

# Every month between two dates, so a gap in a source feed shows up as an NA row
# rather than as a line segment drawn straight across the hole.
cai_month_grid <- function(from, to) {
  seq(cai_month_start(from), cai_month_start(to), by = "month")
}

cai_http_get <- function(url, accept = "application/json",
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
        # Both endpoints reject requests without a browser-ish agent; East Money
        # additionally checks the referer.
        "User-Agent" = "Mozilla/5.0 (compatible; OSI-pipeline/1.0)",
        "Accept" = accept,
        "Referer" = CAI_EM_REFERER
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

## East Money pull -----------------------------------------------------------

cai_em_url <- function(params) {
  encoded <- vapply(
    params,
    function(v) utils::URLencode(as.character(v), reserved = TRUE),
    character(1)
  )
  paste0(CAI_EM_API, "?", paste0(names(params), "=", encoded, collapse = "&"))
}

# One page of a report. The API answers 200 with success=false for a bad report or
# filter, so the body has to be checked rather than the status code.
cai_em_page <- function(report_name, columns, page, page_size, filter = NULL) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required to parse the East Money response.")
  }

  params <- list(
    reportName = report_name, columns = columns,
    pageSize = page_size, pageNumber = page,
    sortColumns = "REPORT_DATE", sortTypes = -1L,
    source = "WEB", client = "WEB"
  )
  if (!is.null(filter)) params$filter <- filter

  body <- rawToChar(cai_http_get(cai_em_url(params)))
  Encoding(body) <- "UTF-8"
  parsed <- jsonlite::fromJSON(body, simplifyVector = TRUE)

  if (!isTRUE(parsed$success)) {
    stop("East Money API rejected ", report_name, ": ",
         cai_default(parsed$message, "no message"))
  }
  parsed$result
}

# A whole report as one data frame, paged to exhaustion.
cai_em_report <- function(report_name, columns, filter = NULL,
                          page_size = 500L, pause = 0.2) {
  pages <- list()
  page <- 1L

  repeat {
    result <- cai_em_page(report_name, columns, page, page_size, filter)
    rows <- result$data
    if (is.null(rows) || !is.data.frame(rows) || nrow(rows) == 0) break

    pages[[length(pages) + 1L]] <- rows
    total_pages <- as.integer(cai_default(result$pages, 1L))
    if (page >= total_pages) break

    page <- page + 1L
    Sys.sleep(pause)
  }

  if (length(pages) == 0) {
    stop("East Money returned no rows for ", report_name, ".")
  }

  out <- dplyr::bind_rows(pages)
  out$date <- cai_month_start(out$REPORT_DATE)
  # The indicator store repeats rows across board/concept mappings; the report
  # tables do not, but deduplicating both is cheap and keeps one row per month.
  dplyr::distinct(out)
}

# A single series out of the generic indicator store, which is where East Money
# keeps the property numbers that have no dedicated macro report.
cai_em_indicator <- function(indicator_id) {
  raw <- cai_em_report(
    "RPT_INDUSTRY_INDEX",
    "INDICATOR_ID,INDICATOR_NAME,REPORT_DATE,INDICATOR_VALUE",
    filter = sprintf('(INDICATOR_ID="%s")', indicator_id)
  )
  out <- data.frame(
    date = raw$date,
    value = cai_num(raw$INDICATOR_VALUE),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$date), , drop = FALSE]
  out <- dplyr::distinct(out)
  out[order(out$date), , drop = FALSE]
}

## Jan-February repair -------------------------------------------------------

# NBS releases January and February together. In this feed the February row carries
# the combined figure in the accumulated column and an empty monthly column, and
# January has no row at all.
#
# `each` is what share of the combined figure each of the two months gets. For a
# value series that is half, which is right on the two-month total and wrong on the
# split within it - most visibly in 2020, when almost all of the collapse fell in
# February. For a growth rate the combined print is already the rate for the pair,
# so it carries over whole.
cai_split_jan_feb <- function(monthly, combined, each = 0.5) {
  stopifnot(all(c("date", "value") %in% names(monthly)))

  # A real monthly print always wins over the split, so a feed that starts
  # publishing January separately needs no change here.
  monthly <- monthly[!is.na(monthly$value), , drop = FALSE]
  monthly$imputed <- FALSE
  monthly <- monthly[, c("date", "value", "imputed"), drop = FALSE]

  combined <- combined[!is.na(combined$value) & as.integer(format(combined$date, "%m")) == 2L, ,
                       drop = FALSE]
  if (nrow(combined) == 0) return(monthly[order(monthly$date), , drop = FALSE])

  years <- as.integer(format(combined$date, "%Y"))
  filled <- data.frame(
    date = as.Date(c(sprintf("%d-01-01", years), sprintf("%d-02-01", years))),
    value = rep(combined$value * each, 2L),
    imputed = TRUE,
    stringsAsFactors = FALSE
  )
  filled <- filled[!(filled$date %in% monthly$date), , drop = FALSE]

  out <- rbind(monthly, filled)
  out[order(out$date), , drop = FALSE]
}

# Straight-line fill across a run of missing months, used where no cumulative print
# is published to split. Only interior gaps are filled - the series is never
# extrapolated past its own first or last observation.
cai_interpolate_gaps <- function(df, max_gap = 2L) {
  df <- df[order(df$date), , drop = FALSE]
  observed <- df[!is.na(df$value), , drop = FALSE]
  if (nrow(observed) < 2L) return(df)

  grid <- cai_month_grid(min(observed$date), max(observed$date))
  full <- data.frame(date = grid, stringsAsFactors = FALSE)
  full$value <- observed$value[match(full$date, observed$date)]
  full$imputed <- is.na(full$value)

  runs <- rle(full$imputed)
  too_long <- runs$values & runs$lengths > max_gap
  keep <- rep(!too_long, runs$lengths)

  interpolated <- stats::approx(
    x = as.numeric(observed$date), y = observed$value,
    xout = as.numeric(full$date), method = "linear", rule = 1
  )$y
  full$value[full$imputed & keep] <- interpolated[full$imputed & keep]
  full$value[full$imputed & !keep] <- NA_real_

  full
}

## Series builders -----------------------------------------------------------

# GACC monthly exports in USD thousands. This is the only macro series here that
# NBS/GACC publish for every month, January included.
cai_series_exports <- function() {
  raw <- cai_em_report("RPT_ECONOMY_CUSTOMS", "REPORT_DATE,EXIT_BASE")
  out <- data.frame(
    date = raw$date, value = cai_num(raw$EXIT_BASE),
    imputed = FALSE, stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$date) & !is.na(out$value), , drop = FALSE]
  out[order(out$date), , drop = FALSE]
}

# NBS retail sales of consumer goods, RMB 100mn.
cai_series_retail <- function() {
  raw <- cai_em_report("RPT_ECONOMY_TOTAL_RETAIL", "REPORT_DATE,RETAIL_TOTAL,RETAIL_TOTAL_ACCUMULATE")
  monthly <- data.frame(date = raw$date, value = cai_num(raw$RETAIL_TOTAL), stringsAsFactors = FALSE)
  combined <- data.frame(date = raw$date, value = cai_num(raw$RETAIL_TOTAL_ACCUMULATE), stringsAsFactors = FALSE)
  cai_split_jan_feb(monthly[!is.na(monthly$date), , drop = FALSE],
                    combined[!is.na(combined$date), , drop = FALSE])
}

# NBS investment actually completed in fixed assets, RMB 100mn.
cai_series_fai <- function() {
  raw <- cai_em_report("RPT_ECONOMY_ASSET_INVEST", "REPORT_DATE,BASE,BASE_ACCUMULATE")
  monthly <- data.frame(date = raw$date, value = cai_num(raw$BASE), stringsAsFactors = FALSE)
  combined <- data.frame(date = raw$date, value = cai_num(raw$BASE_ACCUMULATE), stringsAsFactors = FALSE)
  cai_split_jan_feb(monthly[!is.na(monthly$date), , drop = FALSE],
                    combined[!is.na(combined$date), , drop = FALSE])
}

# NBS sales value of commercialised buildings, RMB 10k. Published monthly with no
# January-February print in this feed, so the two-month hole is interpolated.
cai_series_home_sales <- function() {
  raw <- cai_em_indicator("EMI01523157")
  cai_interpolate_gaps(raw, max_gap = 2L)
}

# NBS industrial value-added. Only growth rates are published, so the level index is
# chained: base-year months are seeded at 100 and the year-on-year rates carry the
# index forwards and backwards from there. The seeding means the base year's mean is
# exactly 100 by construction and the index carries no seasonal pattern of its own -
# it is a pure cumulative-growth path.
cai_series_industrial <- function(base_year) {
  raw <- cai_em_report("RPT_ECONOMY_INDUS_GROW", "REPORT_DATE,BASE_SAME,BASE_ACCUMULATE")

  monthly <- data.frame(date = raw$date, value = cai_num(raw$BASE_SAME), stringsAsFactors = FALSE)
  combined <- data.frame(date = raw$date, value = cai_num(raw$BASE_ACCUMULATE), stringsAsFactors = FALSE)
  # The February accumulated figure is the combined January-February growth RATE, so
  # each month takes it whole rather than half of it.
  growth <- cai_split_jan_feb(monthly[!is.na(monthly$date), , drop = FALSE],
                              combined[!is.na(combined$date), , drop = FALSE],
                              each = 1)

  years <- sort(unique(as.integer(format(growth$date, "%Y"))))
  if (!base_year %in% years) {
    stop("Industrial production growth has no observations in base year ", base_year, ".")
  }

  grid <- cai_month_grid(min(growth$date), max(growth$date))
  slot <- match(grid, growth$date)
  g <- growth$value[slot] / 100
  imputed <- growth$imputed[slot]
  imputed[is.na(imputed)] <- TRUE
  index <- rep(NA_real_, length(grid))

  year_of <- as.integer(format(grid, "%Y"))
  index[year_of == base_year] <- 100

  # Forwards: this month's index is the same month a year ago grown by this month's
  # rate. Backwards: divide instead. A missing rate breaks the chain for that month
  # in that direction, which is why gaps are repaired above rather than tolerated.
  for (i in which(year_of > base_year)) {
    prev <- match(seq(grid[i], length = 2, by = "-12 months")[2], grid)
    if (!is.na(prev)) index[i] <- index[prev] * (1 + g[i])
  }
  for (i in rev(which(year_of < base_year))) {
    nxt <- match(seq(grid[i], length = 2, by = "12 months")[2], grid)
    if (!is.na(nxt) && !is.na(g[nxt]) && (1 + g[nxt]) != 0) index[i] <- index[nxt] / (1 + g[nxt])
  }

  data.frame(date = grid, value = index, imputed = imputed, stringsAsFactors = FALSE)
}

## Ember cleantech exports ---------------------------------------------------

# World-total monthly export value by technology. Ember carries both country rows
# and region rows, and the region rows overlap (G20, OECD, EU and so on), so the
# only safe world total is Ember's own "World" region rather than a sum of parts.
cai_ember_panel <- function() {
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required to read the Ember export panel.")
  }

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  writeBin(cai_http_get(CAI_EMBER_CSV, accept = "text/csv"), tmp)

  raw <- readr::read_csv(
    tmp,
    col_types = readr::cols(
      Area = readr::col_character(),
      Date = readr::col_date(format = ""),
      `Area type` = readr::col_character(),
      Region = readr::col_character(),
      `Commodity category` = readr::col_character(),
      `Amount (USD)` = readr::col_double(),
      `Amount 12 month rolling sum (USD)` = readr::col_double()
    ),
    progress = FALSE
  )

  world <- raw[raw$Area == "World" & raw$`Area type` == "Region", , drop = FALSE]
  if (nrow(world) == 0) {
    stop("Ember panel has no 'World' region rows; the file layout has changed.")
  }

  data.frame(
    category = world$`Commodity category`,
    date = cai_month_start(world$Date),
    value = as.numeric(world$`Amount (USD)`),
    stringsAsFactors = FALSE
  )
}

cai_series_ember <- function(panel, category) {
  out <- panel[panel$category == category, c("date", "value"), drop = FALSE]
  if (nrow(out) == 0) {
    stop("Ember panel has no rows for commodity category '", category, "'.")
  }
  out$imputed <- FALSE
  out <- out[!is.na(out$value), , drop = FALSE]
  out[order(out$date), , drop = FALSE]
}

## Seasonal adjustment -------------------------------------------------------

# STL on log values, which makes the decomposition multiplicative: the seasonal
# component comes out as a percentage swing rather than a fixed number of RMB, which
# is the right shape for series that have grown several-fold over the sample.
#
# `s.window` is deliberately finite rather than "periodic", so the seasonal pattern
# is allowed to drift. That matters here: NBS changed how it collects fixed asset
# investment in 2018, and China's January-February shutdown has been getting longer
# relative to the rest of the year, so a single fixed set of monthly factors leaves
# a large sawtooth behind. `robust = TRUE` stops the 2020 shutdown and the 2021
# property turn from being read as seasonal shape.
#
# Two things this does not fix. Chinese New Year moves between January and February,
# and a calendar-month seasonal cannot track it - that is what the light moving
# average afterwards is for. And the last few months of any STL seasonal are the
# least well determined, so recent readings can revise as data accrues.
CAI_SEASONAL_WINDOW <- 11L

cai_deseasonalise <- function(df, apply = TRUE, s_window = CAI_SEASONAL_WINDOW) {
  # STL needs at least two full periods to have anything to decompose.
  if (!apply || nrow(df) < 24L) {
    df$value_sa <- df$value
    return(df)
  }
  if (any(df$value <= 0, na.rm = TRUE)) {
    stop("Multiplicative seasonal adjustment needs strictly positive values.")
  }
  if (!identical(df$date, cai_month_grid(min(df$date), max(df$date)))) {
    stop("Seasonal adjustment needs a gap-free monthly series; got holes in the index.")
  }

  start <- c(as.integer(format(df$date[1], "%Y")), as.integer(format(df$date[1], "%m")))
  logged <- stats::ts(log(df$value), start = start, frequency = 12)
  fit <- stats::stl(logged, s.window = s_window, robust = TRUE)

  df$value_sa <- exp(as.numeric(logged) - as.numeric(fit$time.series[, "seasonal"]))
  df
}

## Indexing ------------------------------------------------------------------

# 100 = the base year's 12-month mean. A partial base year would move the level of
# every series against every other, so it is rejected rather than quietly used.
# Indexing runs off the seasonally adjusted values, so the base is a clean annual
# mean rather than whatever seasonal mix that particular year happened to carry.
cai_rebase <- function(df, base_year, min_base_months = 12L) {
  in_base <- as.integer(format(df$date, "%Y")) == base_year & !is.na(df$value_sa)
  n_base <- sum(in_base)
  if (n_base < min_base_months) {
    stop("Base year ", base_year, " has only ", n_base, " of ", min_base_months,
         " months of data.")
  }
  df$index <- 100 * df$value_sa / mean(df$value_sa[in_base])
  df
}

# Centred moving average. `sides = 2` with an odd window keeps turning points where
# they happened instead of shifting them half a window late.
cai_smooth <- function(x, window) {
  window <- as.integer(window)
  if (window <= 1L) return(x)
  if (window %% 2L == 0L) stop("Smoothing window must be odd; got ", window, ".")
  as.numeric(stats::filter(x, rep(1 / window, window), method = "convolution", sides = 2))
}

## Panel assembly ------------------------------------------------------------

china_activity_panel <- function(base_year = CAI_DEFAULT_BASE_YEAR,
                                 smooth_months = CAI_DEFAULT_SMOOTH,
                                 include_cleantech = TRUE) {
  message("Pulling NBS/GACC macro series from East Money ...")
  builders <- list(
    exports = cai_series_exports,
    industrial = function() cai_series_industrial(base_year),
    retail = cai_series_retail,
    fai = cai_series_fai,
    home_sales = cai_series_home_sales
  )

  if (include_cleantech) {
    message("Pulling Ember China cleantech export panel ...")
    ember <- cai_ember_panel()
    builders$ember_batteries <- function() cai_series_ember(ember, "Batteries")
    builders$ember_solar <- function() cai_series_ember(ember, "Solar PV")
    builders$ember_evs <- function() cai_series_ember(ember, "EVs")
  }

  parts <- lapply(names(builders), function(slug) {
    df <- builders[[slug]]()
    df <- df[!is.na(df$value), , drop = FALSE]
    df <- df[order(df$date), , drop = FALSE]
    df <- cai_deseasonalise(df, apply = CAI_SERIES$seasonal[match(slug, CAI_SERIES$slug)])
    df <- cai_rebase(df, base_year)
    df$index_smooth <- cai_smooth(df$index, smooth_months)
    df$slug <- slug
    message(sprintf(
      "  %-16s %3d months  %s to %s%s",
      slug, nrow(df), format(min(df$date), "%Y-%m"), format(max(df$date), "%Y-%m"),
      if (any(df$imputed)) sprintf("  (%d imputed)", sum(df$imputed)) else ""
    ))
    df
  })

  panel <- dplyr::bind_rows(parts)
  panel <- merge(panel, CAI_SERIES, by = "slug", all.x = TRUE)
  panel$label <- factor(panel$label, levels = CAI_SERIES$label)
  panel[order(panel$label, panel$date), c("slug", "label", "group", "date",
                                          "value", "value_sa", "imputed",
                                          "index", "index_smooth")]
}

## Charts --------------------------------------------------------------------

china_activity_plot <- function(panel, groups = c("macro", "cleantech"),
                                start_year = CAI_DEFAULT_START_YEAR,
                                base_year = CAI_DEFAULT_BASE_YEAR,
                                smooth_months = CAI_DEFAULT_SMOOTH,
                                title = "China activity indicators",
                                subtitle = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for the activity chart.")
  }

  df <- panel[panel$group %in% groups &
                as.integer(format(panel$date, "%Y")) >= start_year &
                !is.na(panel$index_smooth), , drop = FALSE]
  if (nrow(df) == 0) stop("Nothing to plot for group(s) ", paste(groups, collapse = ", "), ".")

  df$label <- droplevels(df$label)
  palette <- stats::setNames(CAI_SERIES$colour, CAI_SERIES$label)[levels(df$label)]

  smooth_note <- if (smooth_months > 1L) {
    sprintf("Seasonally adjusted monthly index, %d-month centred moving average. ", smooth_months)
  } else {
    "Seasonally adjusted monthly index. "
  }

  ggplot2::ggplot(df, ggplot2::aes(x = date, y = index_smooth, colour = label)) +
    ggplot2::geom_hline(yintercept = 100, colour = "grey70", linewidth = 0.3) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::scale_colour_manual(values = palette, breaks = levels(df$label)) +
    ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0.01, 0)) +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(n = 7)) +
    ggplot2::labs(
      title = title,
      subtitle = cai_default(subtitle, sprintf("Index (100 = %d average)", base_year)),
      x = NULL, y = sprintf("Index, %d = 100", base_year), colour = NULL,
      caption = paste0(
        smooth_note,
        "Industrial production is a volume index chained from year-on-year growth; the rest are nominal values.\n",
        "January and February are a combined NBS release and are split or interpolated across the two months.\n",
        "Source: NBS and GACC via East Money; Ember China cleantech export data explorer."
      )
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "top",
      legend.key.size = ggplot2::unit(0.9, "lines"),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(size = 8, hjust = 0, colour = "grey35"),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2, byrow = TRUE))
}

## Runner --------------------------------------------------------------------

run_china_activity_indicators <- function(repo_root,
                                          base_year = CAI_DEFAULT_BASE_YEAR,
                                          start_year = CAI_DEFAULT_START_YEAR,
                                          smooth_months = CAI_DEFAULT_SMOOTH,
                                          include_cleantech = TRUE,
                                          write_output = TRUE) {
  panel <- china_activity_panel(
    base_year = base_year, smooth_months = smooth_months,
    include_cleantech = include_cleantech
  )

  plotted <- panel[as.integer(format(panel$date, "%Y")) >= start_year, , drop = FALSE]
  if (nrow(plotted) == 0) stop("No observations from ", start_year, " onwards.")
  if (!write_output) return(invisible(panel))

  out_dir <- file.path(repo_root, config$processed_dir, "charts")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  stem <- sprintf(
    "china_activity_indicators_%s_%s",
    format(min(plotted$date), "%Y%m"), format(max(plotted$date), "%Y%m")
  )

  utils::write.csv(
    plotted, file.path(out_dir, paste0(stem, ".csv")), row.names = FALSE, na = ""
  )

  wide <- tidyr::pivot_wider(
    plotted[, c("date", "slug", "index_smooth")],
    names_from = "slug", values_from = "index_smooth"
  )
  wide <- wide[order(wide$date), c("date", intersect(CAI_SERIES$slug, names(wide)))]
  utils::write.csv(wide, file.path(out_dir, paste0(stem, "_wide.csv")),
                   row.names = FALSE, na = "")

  save_png <- function(plot, suffix, width = 10, height = 6) {
    path <- file.path(out_dir, paste0(stem, "_", suffix, ".png"))
    ggplot2::ggsave(
      path, plot, width = width, height = height, dpi = 200,
      device = if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else NULL
    )
    message("Wrote ", path)
  }

  save_png(
    china_activity_plot(panel, "macro", start_year, base_year, smooth_months),
    "macro"
  )

  if (include_cleantech) {
    save_png(
      china_activity_plot(
        panel, "cleantech", start_year, base_year, smooth_months,
        title = "China cleantech exports",
        subtitle = sprintf("Monthly export value, index (100 = %d average)", base_year)
      ),
      "cleantech"
    )
    save_png(
      china_activity_plot(
        panel, c("macro", "cleantech"), start_year, base_year, smooth_months,
        title = "China activity indicators and cleantech exports"
      ),
      "all"
    )
  }

  message("Wrote ", file.path(out_dir, paste0(stem, ".csv")))
  invisible(panel)
}

opsi_china_activity_run_directly <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  length(file_arg) > 0 &&
    identical(basename(sub("^--file=", "", file_arg[1])), "china_activity_indicators.R")
}

if (opsi_china_activity_run_directly()) {
  invisible(run_china_activity_indicators(
    repo_root = repo_root,
    base_year = as.integer(Sys.getenv("CAI_BASE_YEAR", as.character(CAI_DEFAULT_BASE_YEAR))),
    start_year = as.integer(Sys.getenv("CAI_START_YEAR", as.character(CAI_DEFAULT_START_YEAR))),
    smooth_months = as.integer(Sys.getenv("CAI_SMOOTH", as.character(CAI_DEFAULT_SMOOTH)))
  ))
}
