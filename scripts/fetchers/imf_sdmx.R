# IMF fetchers: imf_ppi.csv, imf_lending_rates.csv and imf_commodity_prices.csv,
# via the SDMX 3.0 API.
#
# Consumers (R/categories/economic opportunity/cost_competitiveness.R,
# R/categories/energy_prices/energy_prices.R) read these as IMF Data Explorer "wide"
# exports: metadata columns plus one column per period named "2024-M01" / "2024-Q1" /
# "2024", which read.csv turns into X2024.M01 etc. The API returns long SDMX-CSV, so
# we pivot.
#
# Two upstream quirks drive the implementation:
#   * startPeriod/endPeriod are ignored on these flows, so the whole flow is fetched
#     and filtered locally. Responses are large (PPI ~35MB, MFS_IR ~130MB, PCPS ~40MB),
#     so the body is streamed to a temp file and only the needed columns are parsed.
#   * The FREQUENCY column is mislabelled upstream (on PPI it echoes the PPI_ACTIVITY
#     codelist; on PCPS "M" is labelled "Mixed-type data" and "Q" "Constant prices"),
#     so frequency is derived from TIME_PERIOD instead.

IMF_SDMX_BASE <- "https://api.imf.org/external/sdmx/3.0"
IMF_SDMX_CSV_ACCEPT <- "application/vnd.sdmx.data+csv;version=2.0.0;labels=both"

# How far back to keep observations. The consumers take the latest value per country,
# so the window is really about coverage, not recency: countries whose series stopped
# reporting years ago drop out entirely if the window is too short. 1990 reproduces the
# country coverage of the staged Data Explorer extracts. Override with OPSI_IMF_START_YEAR.
imf_window_start_year <- function(default_start = 1990L) {
  configured <- suppressWarnings(as.integer(Sys.getenv("OPSI_IMF_START_YEAR", "")))
  if (!is.na(configured)) configured else default_start
}

# "USA: United States" -> "United States"; passes through values with no label.
imf_strip_code <- function(x) {
  x <- as.character(x)
  has_label <- grepl("^[^:]+: ", x)
  x[has_label] <- sub("^[^:]+: ", "", x[has_label])
  x
}

# Frequency implied by the period string, since the upstream column is unreliable.
imf_frequency_from_period <- function(period) {
  ifelse(grepl("-M\\d{2}$", period), "Monthly",
         ifelse(grepl("-Q[1-4]$", period), "Quarterly", "Annual"))
}

# Fetch a dataflow as SDMX-CSV and return only the requested columns.
imf_fetch_dataflow <- function(flow,
                               version,
                               columns,
                               agency = "IMF.STA",
                               timeout_seconds = 900) {
  url <- sprintf("%s/data/dataflow/%s/%s/%s/*", IMF_SDMX_BASE, agency, flow, version)

  body <- opsi_http_get(url, accept = IMF_SDMX_CSV_ACCEPT, retries = 2, timeout_seconds = timeout_seconds)

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  writeBin(body, tmp)
  rm(body)
  invisible(gc(verbose = FALSE))

  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required for the IMF fetchers.")
  }

  header <- names(readr::read_csv(tmp, n_max = 0, show_col_types = FALSE, progress = FALSE))

  # With labels=both the headers are "CODE: Label"; tolerate the plain form too.
  resolve <- function(want) {
    exact <- match(want, header)
    if (!is.na(exact)) return(header[exact])
    prefixed <- grep(paste0("^", want, "\\s*:"), header, value = TRUE)
    if (length(prefixed) > 0) return(prefixed[1])
    NA_character_
  }

  resolved <- vapply(columns, resolve, character(1))
  missing <- names(resolved)[is.na(resolved)]
  if (length(missing) > 0) {
    stop("IMF ", flow, ": response is missing expected column(s): ", paste(missing, collapse = ", "))
  }

  out <- readr::read_csv(
    tmp,
    col_select = dplyr::all_of(unname(resolved)),
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE,
    progress = FALSE
  )

  names(out) <- names(resolved)
  as.data.frame(out, stringsAsFactors = FALSE)
}

# Long SDMX rows -> the wide Data Explorer layout, restricted to a recent window.
imf_to_wide <- function(long_df, id_cols, start_year) {
  long_df$period <- opsi_normalize_period(long_df$TIME_PERIOD)
  long_df$year <- suppressWarnings(as.integer(substr(long_df$period, 1, 4)))

  long_df <- long_df[!is.na(long_df$year) & long_df$year >= start_year, , drop = FALSE]
  long_df <- long_df[!is.na(long_df$OBS_VALUE) & nzchar(long_df$OBS_VALUE), , drop = FALSE]

  if (nrow(long_df) == 0) stop("no observations remained after filtering to >= ", start_year)

  # Collapse duplicate (series, period) rows; keep the last reported observation.
  key <- do.call(paste, c(lapply(c(id_cols, "period"), function(cn) long_df[[cn]]), sep = "\r"))
  long_df <- long_df[!duplicated(key, fromLast = TRUE), , drop = FALSE]

  opsi_pivot_periods_wide(
    long_df,
    id_cols = id_cols,
    period_col = "period",
    value_col = "OBS_VALUE",
    period_order = opsi_order_periods(unique(long_df$period))
  )
}

## imf_ppi.csv --------------------------------------------------------------

register_fetcher(
  id = "imf_ppi",
  description = "IMF producer price indices (SDMX 3.0, wide Data Explorer layout)",
  contract = list(
    required_columns = c("COUNTRY", "INDICATOR", "TYPE_OF_TRANSFORMATION", "FREQUENCY"),
    min_rows = 50,
    unique_key = c("COUNTRY", "INDICATOR", "TYPE_OF_TRANSFORMATION", "FREQUENCY"),
    time_column_pattern = "^X?\\d{4}([.-][MQ]\\d{1,2})?$",
    min_time_columns = 4
  ),
  fn = function() {
    raw <- imf_fetch_dataflow(
      flow = "PPI", version = "3.0.0",
      columns = c(
        COUNTRY = "COUNTRY", INDICATOR = "INDICATOR",
        TYPE_OF_TRANSFORMATION = "TYPE_OF_TRANSFORMATION",
        TIME_PERIOD = "TIME_PERIOD", OBS_VALUE = "OBS_VALUE"
      )
    )

    raw$COUNTRY <- imf_strip_code(raw$COUNTRY)
    raw$INDICATOR <- imf_strip_code(raw$INDICATOR)
    raw$TYPE_OF_TRANSFORMATION <- imf_strip_code(raw$TYPE_OF_TRANSFORMATION)

    # cost_competitiveness_build_ppi() selects exactly this pair.
    raw <- raw[
      raw$INDICATOR == "Producer price index (PPI)" &
        raw$TYPE_OF_TRANSFORMATION == "Index", ,
      drop = FALSE
    ]
    if (nrow(raw) == 0) stop("no rows matched INDICATOR/TYPE_OF_TRANSFORMATION = PPI/Index")

    raw$FREQUENCY <- imf_frequency_from_period(opsi_normalize_period(raw$TIME_PERIOD))

    imf_to_wide(
      raw,
      id_cols = c("COUNTRY", "INDICATOR", "TYPE_OF_TRANSFORMATION", "FREQUENCY"),
      start_year = imf_window_start_year()
    )
  }
)

## imf_lending_rates.csv ----------------------------------------------------

# Rates cost_competitiveness_select_imf_rates() ranks, in its priority order.
IMF_RATE_INDICATOR_PATTERNS <- paste(
  "^Lending Rate, Percent per annum$",
  "Harmonized Euro.*Loans.*New Business.*Non-financial corporations",
  "Harmonized Euro.*Loans.*Outstanding.*Non-financial corporations",
  "^Money market Rate, Percent per annum$",
  "^Monetary policy-related, Rate",
  "^Deposit Rate, Percent per annum$",
  sep = "|"
)

register_fetcher(
  id = "imf_lending_rates",
  description = "IMF monetary and financial statistics interest rates (SDMX 3.0, wide layout)",
  contract = list(
    required_columns = c("COUNTRY", "INDICATOR", "FREQUENCY"),
    min_rows = 50,
    unique_key = c("COUNTRY", "INDICATOR", "FREQUENCY"),
    time_column_pattern = "^X?\\d{4}([.-][MQ]\\d{1,2})?$",
    min_time_columns = 4
  ),
  fn = function() {
    raw <- imf_fetch_dataflow(
      flow = "MFS_IR", version = "8.0.1",
      columns = c(
        COUNTRY = "COUNTRY", INDICATOR = "INDICATOR",
        TIME_PERIOD = "TIME_PERIOD", OBS_VALUE = "OBS_VALUE"
      )
    )

    raw$COUNTRY <- imf_strip_code(raw$COUNTRY)
    raw$INDICATOR <- imf_strip_code(raw$INDICATOR)

    # Keep only the rate types the theme scores; the flow carries many more.
    raw <- raw[grepl(IMF_RATE_INDICATOR_PATTERNS, raw$INDICATOR), , drop = FALSE]
    if (nrow(raw) == 0) stop("no rows matched the scored interest-rate indicators")

    raw$FREQUENCY <- imf_frequency_from_period(opsi_normalize_period(raw$TIME_PERIOD))

    imf_to_wide(
      raw,
      id_cols = c("COUNTRY", "INDICATOR", "FREQUENCY"),
      start_year = imf_window_start_year()
    )
  }
)

## imf_commodity_prices.csv -------------------------------------------------

# PCPS is published by IMF.RES, not IMF.STA like the two flows above.
IMF_PCPS_AGENCY <- "IMF.RES"
IMF_PCPS_VERSION <- "9.0.0"

# The API names an indicator with its short form ("Aluminum"); the Data Explorer export
# the Energy Prices theme was built against composes "<name>, <unit>, <measure>"
# ("Aluminum, US dollars per metric tonne, Unit prices"). The unit is not carried in the
# data message at all, so it is restored from config. See the file header there for why
# two downstream consumers need it.
imf_pcps_label_map <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)

    root <- getOption("opportunity_security.repo_root")
    if (is.null(root) || !nzchar(root)) {
      root <- if (requireNamespace("rprojroot", quietly = TRUE)) {
        tryCatch(rprojroot::find_root(rprojroot::is_git_root), error = function(e) getwd())
      } else {
        getwd()
      }
    }

    path <- file.path(root, "config", "imf_pcps_indicator_labels.yml")
    if (!file.exists(path)) {
      stop("IMF PCPS indicator label map not found: ", path)
    }

    cached <<- unlist(yaml::read_yaml(path))
    cached
  }
})

# "PALUM: Aluminum" -> "Aluminum, US dollars per metric tonne, Unit prices".
# Codes absent from the map keep their plain API label, so a commodity the IMF adds
# later degrades to a missing unit rather than a failed fetch.
imf_pcps_compose_indicator <- function(x) {
  x <- as.character(x)
  code <- sub("^([^:]+):.*$", "\\1", x)
  plain <- imf_strip_code(x)

  map <- imf_pcps_label_map()
  composed <- unname(map[code])
  ifelse(is.na(composed), plain, composed)
}

register_fetcher(
  id = "imf_commodity_prices",
  description = "IMF Primary Commodity Price System (SDMX 3.0, wide Data Explorer layout)",
  contract = list(
    required_columns = c("COUNTRY", "INDICATOR", "DATA_TRANSFORMATION", "FREQUENCY"),
    min_rows = 200,
    unique_key = c("COUNTRY", "INDICATOR", "DATA_TRANSFORMATION", "FREQUENCY"),
    time_column_pattern = "^X?\\d{4}([.-][MQ]\\d{1,2})?$",
    min_time_columns = 4
  ),
  fn = function() {
    raw <- imf_fetch_dataflow(
      flow = "PCPS", version = IMF_PCPS_VERSION, agency = IMF_PCPS_AGENCY,
      columns = c(
        COUNTRY = "COUNTRY", INDICATOR = "INDICATOR",
        DATA_TRANSFORMATION = "DATA_TRANSFORMATION",
        TIME_PERIOD = "TIME_PERIOD", OBS_VALUE = "OBS_VALUE"
      )
    )

    raw$COUNTRY <- imf_strip_code(raw$COUNTRY)
    raw$INDICATOR <- imf_pcps_compose_indicator(raw$INDICATOR)
    raw$DATA_TRANSFORMATION <- imf_strip_code(raw$DATA_TRANSFORMATION)

    # energy_prices_imf_monthly_usd_long() filters on the plural spelling the staged
    # export uses; the API returns the singular.
    raw$DATA_TRANSFORMATION[raw$DATA_TRANSFORMATION == "US dollar"] <- "US dollars"

    raw$FREQUENCY <- imf_frequency_from_period(opsi_normalize_period(raw$TIME_PERIOD))

    imf_to_wide(
      raw,
      id_cols = c("COUNTRY", "INDICATOR", "DATA_TRANSFORMATION", "FREQUENCY"),
      start_year = imf_window_start_year()
    )
  }
)
