# Daily equity prices for the EIS index families, by index.
#
# Pulls ~5 years of daily prices for the core listed companies behind the six EIS
# indices - A Materials & Inputs, B Electrotechnologies, C Grid, D Generation,
# E Electrified End Uses, F Industrial Transition - and collates them into a wide
# panel: one row per calendar date, one column per (index, company).
#
# Source: the public Yahoo Finance chart endpoint
#   https://query1.finance.yahoo.com/v8/finance/chart/<symbol>
# It needs no API key and covers the non-US listings in the universe (XETRA,
# Euronext, Borsa Italiana, SIX, LSE, Oslo, Helsinki, Stockholm, Copenhagen,
# Tokyo, Korea, Taiwan, Shenzhen, Hong Kong, Sydney, Toronto), which is why it is
# used in preference to a US-only free feed.
#
# Outputs (data/processed/charts/):
#   eis_equities_<start>_<end>_wide.csv      date x <index>__<ticker>, adjusted close
#   eis_equities_<start>_<end>_rebased.csv   same panel rebased to 100 at each
#                                            series' first observation
#   eis_equities_<start>_<end>_long.csv      tidy panel with OHLCV and index labels
#   eis_equities_<start>_<end>_registry.csv  one row per universe member: ticker,
#                                            index, currency, exchange, window, status
#   eis_equities_indexes.csv                 the six indices and their 39 sectors
#
# Two things to know before using the wide file:
#
#   * Prices are in each listing's LOCAL currency (USD, EUR, GBp, CHF, NOK, SEK,
#     DKK, JPY, KRW, TWD, CNY, HKD, AUD, CAD). Levels are not comparable across
#     columns; the rebased file, or returns computed per column, are. The currency
#     of every column is in the registry file.
#   * Exchanges keep different holiday calendars, so the date index is the union of
#     trading days and a column is NA on days its own market was shut. Gaps are left
#     as NA rather than carried forward, so no synthetic prices enter the panel.
#
# A company that anchors more than one index (GE Vernova and Siemens Energy in both
# Grid and Generation, Schneider in Grid and Electrified End Uses, Cummins and Bloom
# in Generation and Industrial Transition) is fetched once and written as one column
# per index, so slicing the panel by index never drops a name.
#
# Run:
#   Rscript R/charts/eis_equities.R
#
# Environment:
#   EIS_YEARS       lookback in years from the end date (default 5)
#   EIS_START_DATE  YYYY-MM-DD, overrides EIS_YEARS
#   EIS_END_DATE    YYYY-MM-DD (default today)
#   EIS_FIELD       adj_close | close  (default adj_close) - the field written wide
#   EIS_REFRESH     true to bypass the cached pull
#   EIS_PAUSE       seconds between symbol requests (default 0.4)

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

EIS_DEFAULT_YEARS <- 5L
EIS_DEFAULT_FIELD <- "adj_close"

EIS_CHART_API <- "https://query1.finance.yahoo.com/v8/finance/chart/"

# The six indices, in A-F order. `slug` is fixed here rather than derived from the
# title so the CSV column prefixes stay stable if an index is ever renamed.
EIS_INDEXES <- tibble::tribble(
  ~index_code, ~index, ~slug,
  "A", "EIS Materials & Inputs", "a_materials_inputs",
  "B", "EIS Electrotechnologies", "b_electrotechnologies",
  "C", "EIS Grid", "c_grid",
  "D", "EIS Generation", "d_generation",
  "E", "EIS Electrified End Uses", "e_electrified_end_uses",
  "F", "EIS Industrial Transition", "f_industrial_transition"
)

# The 39-sector coverage, one row per sector. Companies are not mapped to individual
# sectors - the source table assigns them at index level - so this stands as the
# reference definition of what each index spans.
EIS_INDEX_SECTORS <- tibble::tribble(
  ~index_code, ~sector,
  "A", "Critical minerals",
  "A", "Battery minerals/refining",
  "A", "Graphite",
  "A", "Rare earths",
  "A", "Copper/aluminum",
  "A", "Electrical steel",
  "A", "Polysilicon",
  "A", "Semiconductor materials",
  "A", "Uranium/fuel",

  "B", "Logic/memory/analog chips",
  "B", "Power semis",
  "B", "Power electronics",
  "B", "Controls/sensors",
  "B", "Cathodes",
  "B", "Battery materials",
  "B", "Cells/packs",
  "B", "Stationary storage",
  "B", "Magnets",
  "B", "Motors",

  "C", "Transformers",
  "C", "Switchgear/protection",
  "C", "Cables/conductors",
  "C", "HVDC",
  "C", "Digital grid/GETs",
  "C", "Microgrids/resilience",

  "D", "Solar",
  "D", "Onshore wind",
  "D", "Offshore wind",
  "D", "Nuclear reactor systems",
  "D", "Geothermal",
  "D", "Other firm/flexible power",

  "E", "EVs/drivetrains",
  "E", "Charging",
  "E", "Heat pumps/cooling",
  "E", "Industrial electrification/process heat",
  "E", "Data-center power/cooling",

  "F", "Electrolysers/H2 equipment",
  "F", "Hydrogen derivatives and low-emissions fuels",
  "F", "Near-zero materials and carbon management"
)

# The universe: the core listed companies for each index.
#
# `ticker` is the Yahoo symbol - for non-US listings the local code plus an exchange
# suffix. Primary listings are used in preference to ADRs (BHP.AX not BHP, RIO.L not
# RIO, MT.AS not MT) so the price series is the one the home market sets; the two
# exceptions are SQM and STMicroelectronics, whose NYSE lines are the reference
# quote in practice. Every symbol here was checked against the endpoint's own
# company name before being written down.
EIS_UNIVERSE <- tibble::tribble(
  ~index_code, ~company, ~ticker, ~note,

  # A. Materials & inputs
  "A", "BHP Group", "BHP.AX", "Primary ASX listing, AUD",
  "A", "Rio Tinto", "RIO.L", "Primary LSE listing, quoted in GBp (pence)",
  "A", "Freeport-McMoRan", "FCX", NA_character_,
  "A", "Albemarle", "ALB", NA_character_,
  "A", "SQM", "SQM", "NYSE ADR; the Santiago B line (SQM-B.SN) is the primary",
  "A", "MP Materials", "MP", NA_character_,
  "A", "Lynas Rare Earths", "LYC.AX", NA_character_,
  "A", "Norsk Hydro", "NHY.OL", NA_character_,
  "A", "POSCO Holdings", "005490.KS", NA_character_,
  "A", "Wacker Chemie", "WCH.DE", NA_character_,
  "A", "Shin-Etsu Chemical", "4063.T", NA_character_,
  "A", "Entegris", "ENTG", NA_character_,
  "A", "Cameco", "CCJ", NA_character_,
  "A", "Centrus Energy", "LEU", NA_character_,

  # B. Electrotechnologies
  "B", "TSMC", "2330.TW", "Primary Taiwan listing; the ADR is TSM",
  "B", "Samsung Electronics", "005930.KS", NA_character_,
  "B", "Texas Instruments", "TXN", NA_character_,
  "B", "Infineon Technologies", "IFX.DE", NA_character_,
  "B", "STMicroelectronics", "STM", "NYSE line; also trades in Paris as STMPA.PA",
  "B", "onsemi", "ON", NA_character_,
  "B", "Delta Electronics", "2308.TW", NA_character_,
  "B", "Rockwell Automation", "ROK", NA_character_,
  "B", "CATL", "300750.SZ", "Shenzhen A-share; also lists in Hong Kong (3750.HK)",
  "B", "LG Energy Solution", "373220.KS", "Listed January 2022; history starts then",
  "B", "Samsung SDI", "006400.KS", NA_character_,
  "B", "Panasonic Holdings", "6752.T", NA_character_,
  "B", "POSCO Future M", "003670.KS", NA_character_,
  "B", "Nidec", "6594.T", NA_character_,
  "B", "Regal Rexnord", "RRX", NA_character_,
  "B", "Neo Performance Materials", "NEO.TO", NA_character_,

  # C. Grid
  "C", "Eaton", "ETN", NA_character_,
  "C", "ABB", "ABBN.SW", NA_character_,
  "C", "Schneider Electric", "SU.PA", NA_character_,
  "C", "GE Vernova", "GEV", "Spun out of GE in April 2024; history starts then",
  "C", "Siemens Energy", "ENR.DE", NA_character_,
  "C", "Hitachi", "6501.T", "Hitachi Energy sits inside Hitachi Ltd",
  "C", "Hubbell", "HUBB", NA_character_,
  "C", "Prysmian", "PRY.MI", NA_character_,
  "C", "Nexans", "NEX.PA", NA_character_,
  "C", "HD Hyundai Electric", "267260.KS", NA_character_,
  "C", "LS Electric", "010120.KS", NA_character_,
  "C", "Itron", "ITRI", NA_character_,
  "C", "Landis+Gyr", "LAND.SW", "Moved its primary listing to SIX Swiss Exchange",
  "C", "Bentley Systems", "BSY", NA_character_,

  # D. Generation
  "D", "GE Vernova", "GEV", "Same listing as the Grid row",
  "D", "Siemens Energy", "ENR.DE", "Same listing as the Grid row",
  "D", "Mitsubishi Heavy Industries", "7011.T", NA_character_,
  "D", "Caterpillar", "CAT", NA_character_,
  "D", "Cummins", "CMI", NA_character_,
  "D", "First Solar", "FSLR", NA_character_,
  "D", "Nextracker", "NXT", "The table's 'Nextpower/NXT'",
  "D", "Vestas Wind Systems", "VWS.CO", NA_character_,
  "D", "Nordex", "NDX1.DE", NA_character_,
  "D", "BWX Technologies", "BWXT", NA_character_,
  "D", "Curtiss-Wright", "CW", NA_character_,
  "D", "Doosan Enerbility", "034020.KS", NA_character_,
  "D", "Ormat Technologies", "ORA", NA_character_,
  "D", "Bloom Energy", "BE", NA_character_,
  "D", "Wartsila", "WRT1V.HE", NA_character_,

  # E. Electrified end uses
  "E", "Tesla", "TSLA", NA_character_,
  "E", "BYD", "1211.HK", "Hong Kong line; the Shenzhen A-share is 002594.SZ",
  "E", "Hyundai Motor", "005380.KS", NA_character_,
  "E", "Aptiv", "APTV", NA_character_,
  "E", "BorgWarner", "BWA", NA_character_,
  "E", "Trane Technologies", "TT", NA_character_,
  "E", "Carrier Global", "CARR", NA_character_,
  "E", "Daikin Industries", "6367.T", NA_character_,
  "E", "Mitsubishi Electric", "6503.T", NA_character_,
  "E", "Schneider Electric", "SU.PA", "Same listing as the Grid row",
  "E", "Vertiv", "VRT", NA_character_,
  "E", "Modine Manufacturing", "MOD", NA_character_,
  "E", "Munters Group", "MTRS.ST", NA_character_,
  "E", "nVent Electric", "NVT", NA_character_,
  "E", "Legrand", "LR.PA", NA_character_,

  # F. Industrial transition
  "F", "thyssenkrupp nucera", "NCH2.DE", "Listed July 2023; history starts then",
  "F", "Nel", "NEL.OL", NA_character_,
  "F", "Bloom Energy", "BE", "Same listing as the Generation row",
  "F", "Cummins", "CMI", "Same listing as the Generation row",
  "F", "Linde", "LIN", NA_character_,
  "F", "Air Products", "APD", NA_character_,
  "F", "Yara International", "YAR.OL", NA_character_,
  "F", "CF Industries", "CF", NA_character_,
  "F", "Neste", "NESTE.HE", NA_character_,
  "F", "ArcelorMittal", "MT.AS", "Primary Amsterdam listing; the NYSE line is MT",
  "F", "Nucor", "NUE", NA_character_,
  "F", "Heidelberg Materials", "HEI.DE", NA_character_,
  "F", "Holcim", "HOLN.SW", NA_character_,
  "F", "Baker Hughes", "BKR", NA_character_,
  "F", "SLB", "SLB", NA_character_
)

## Universe ------------------------------------------------------------------

eis_slug <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_+|_+$", "", x)
}

# The universe with the derived fields every downstream step needs: index title and
# ordering, and the wide-panel column name for each (index, ticker) pair. Column
# names are slugged because a raw ticker like ENR.DE is mangled by read.csv()'s
# check.names; the registry file carries the mapping back to the real symbol.
eis_universe <- function(universe = EIS_UNIVERSE, indexes = EIS_INDEXES) {
  unknown <- setdiff(unique(universe$index_code), indexes$index_code)
  if (length(unknown) > 0) {
    stop("Universe references unknown indices: ", paste(unknown, collapse = ", "))
  }
  if (any(is.na(universe$ticker))) {
    stop("Every universe row needs a ticker; unlisted names do not belong here.")
  }

  idx <- match(universe$index_code, indexes$index_code)
  out <- universe
  out$index <- indexes$index[idx]
  out$index_order <- idx
  out$column <- paste0(indexes$slug[idx], "__", eis_slug(out$ticker))

  dup <- out$column[duplicated(out$column)]
  if (length(dup) > 0) {
    stop("Duplicate (index, ticker) pairs in the universe: ", paste(unique(dup), collapse = ", "))
  }

  out <- out[order(out$index_order, out$company), ]
  rownames(out) <- NULL
  out[, c("index_code", "index", "index_order", "company", "ticker", "column", "note")]
}

# The six indices with their sector coverage collapsed to one row each.
eis_index_table <- function(indexes = EIS_INDEXES, sectors = EIS_INDEX_SECTORS,
                            universe = eis_universe()) {
  split_sectors <- split(sectors$sector, sectors$index_code)

  out <- indexes
  out$n_sectors <- vapply(out$index_code, function(k) length(split_sectors[[k]]), integer(1))
  out$sectors <- vapply(
    out$index_code, function(k) paste(split_sectors[[k]], collapse = "; "), character(1)
  )
  out$n_companies <- vapply(
    out$index_code, function(k) sum(universe$index_code == k), integer(1)
  )
  out[, c("index_code", "index", "slug", "n_sectors", "n_companies", "sectors")]
}

eis_listed_tickers <- function(universe = eis_universe()) {
  sort(unique(universe$ticker))
}

## Yahoo chart client --------------------------------------------------------

eis_default <- function(x, fallback) {
  if (is.null(x) || length(x) == 0) fallback else x
}

# Yahoo returns JSON null for a missing observation, which parses to NULL inside the
# list, so the columns have to be walked element-by-element rather than unlisted.
eis_num_vec <- function(x, n) {
  if (is.null(x)) return(rep(NA_real_, n))
  out <- vapply(x, function(v) {
    if (is.null(v) || length(v) == 0) NA_real_ else suppressWarnings(as.numeric(v[[1]]))
  }, numeric(1))
  length(out) <- n
  out
}

eis_chart_request <- function(symbol, start_date, end_date, timeout_seconds = 60) {
  # period2 is exclusive of the instant, so push it a day out to keep the end date.
  period1 <- as.integer(as.POSIXct(paste0(start_date, " 00:00:00"), tz = "UTC"))
  period2 <- as.integer(as.POSIXct(paste0(end_date + 1L, " 00:00:00"), tz = "UTC"))

  url <- sprintf(
    "%s%s?period1=%d&period2=%d&interval=1d&includeAdjustedClose=true&includePrePost=false&events=div%%2Csplit",
    EIS_CHART_API, utils::URLencode(symbol, reserved = TRUE), period1, period2
  )

  handle <- curl::new_handle()
  curl::handle_setheaders(
    handle,
    # The endpoint rejects requests without a browser-ish agent.
    "User-Agent" = "Mozilla/5.0 (compatible; OSI-pipeline/1.0)",
    "Accept" = "application/json"
  )
  curl::handle_setopt(handle, timeout = timeout_seconds)

  curl::curl_fetch_memory(url, handle = handle)
}

# One symbol's daily bars, or NULL with a reason when the pull fails. Failures are
# returned rather than thrown so a single dead ticker cannot take the whole panel
# down; the reason lands in the registry file.
eis_fetch_symbol <- function(symbol, start_date, end_date,
                             retries = 3L, timeout_seconds = 60,
                             backoff_seconds = 2) {
  if (!requireNamespace("curl", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Packages 'curl' and 'jsonlite' are required to pull prices.")
  }

  last_reason <- "unknown error"

  for (attempt in seq_len(max(1L, retries))) {
    response <- tryCatch(
      eis_chart_request(symbol, start_date, end_date, timeout_seconds),
      error = function(e) {
        last_reason <<- paste0("request failed: ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(response)) {
      status <- response$status_code
      body <- rawToChar(response$content)

      # 404/422 mean the symbol does not exist on Yahoo - retrying cannot help.
      if (status %in% c(404L, 422L)) {
        parsed <- tryCatch(jsonlite::fromJSON(body, simplifyVector = FALSE), error = function(e) NULL)
        desc <- eis_default(parsed$chart$error$description, "symbol not found")
        return(list(data = NULL, reason = paste0("HTTP ", status, ": ", desc)))
      }

      if (status >= 400L) {
        last_reason <- paste0("HTTP ", status)
      } else {
        parsed <- tryCatch(jsonlite::fromJSON(body, simplifyVector = FALSE), error = function(e) NULL)
        if (is.null(parsed)) {
          last_reason <- "response was not valid JSON"
        } else if (!is.null(parsed$chart$error)) {
          return(list(
            data = NULL,
            reason = eis_default(parsed$chart$error$description, "chart API error")
          ))
        } else {
          result <- parsed$chart$result[[1]]
          timestamps <- eis_num_vec(result$timestamp, length(result$timestamp))

          if (length(timestamps) == 0) {
            return(list(data = NULL, reason = "no observations in the requested window"))
          }

          quote <- eis_default(result$indicators$quote[[1]], list())
          adj <- eis_default(result$indicators$adjclose[[1]]$adjclose, NULL)
          n <- length(timestamps)

          # Timestamps are the market-open instant in UTC; shifting by the exchange's
          # offset before truncating recovers the local trading date, which is what a
          # Tokyo or Shenzhen bar should be labelled with.
          gmtoffset <- suppressWarnings(as.numeric(eis_default(result$meta$gmtoffset, 0)))
          if (!is.finite(gmtoffset)) gmtoffset <- 0
          dates <- as.Date(as.POSIXct(timestamps + gmtoffset, origin = "1970-01-01", tz = "UTC"))

          close <- eis_num_vec(quote$close, n)
          adj_close <- eis_num_vec(adj, n)
          # Some listings come back without an adjusted series; close is the fallback.
          adj_close <- ifelse(is.na(adj_close), close, adj_close)

          out <- data.frame(
            ticker = symbol,
            date = dates,
            open = eis_num_vec(quote$open, n),
            high = eis_num_vec(quote$high, n),
            low = eis_num_vec(quote$low, n),
            close = close,
            adj_close = adj_close,
            volume = eis_num_vec(quote$volume, n),
            currency = as.character(eis_default(result$meta$currency, NA_character_)),
            exchange = as.character(eis_default(result$meta$fullExchangeName, NA_character_)),
            stringsAsFactors = FALSE
          )

          out <- out[!is.na(out$date) & out$date >= start_date & out$date <= end_date, ]
          out <- out[!is.na(out$close) | !is.na(out$adj_close), ]
          out <- out[!duplicated(out$date, fromLast = TRUE), ]
          out <- out[order(out$date), ]
          rownames(out) <- NULL

          if (nrow(out) == 0) {
            return(list(data = NULL, reason = "no observations in the requested window"))
          }
          return(list(data = out, reason = NA_character_))
        }
      }
    }

    if (attempt < retries) Sys.sleep(backoff_seconds * attempt)
  }

  list(data = NULL, reason = last_reason)
}

# Every ticker, fetched once. Returns the stacked long panel plus one status row per
# ticker so the caller can report what did and did not come back.
eis_fetch_panel <- function(tickers, start_date, end_date,
                            pause_seconds = 0.4, retries = 3L,
                            timeout_seconds = 60) {
  frames <- list()
  status <- list()

  for (i in seq_along(tickers)) {
    symbol <- tickers[[i]]
    message(sprintf("[%d/%d] %s", i, length(tickers), symbol))

    pull <- eis_fetch_symbol(
      symbol, start_date, end_date,
      retries = retries, timeout_seconds = timeout_seconds
    )

    if (is.null(pull$data)) {
      warning("No prices for ", symbol, ": ", pull$reason, call. = FALSE)
      status[[length(status) + 1L]] <- data.frame(
        ticker = symbol, status = "failed", reason = pull$reason,
        currency = NA_character_, exchange = NA_character_,
        first_date = as.Date(NA), last_date = as.Date(NA), n_obs = 0L,
        stringsAsFactors = FALSE
      )
    } else {
      frames[[length(frames) + 1L]] <- pull$data
      status[[length(status) + 1L]] <- data.frame(
        ticker = symbol, status = "ok", reason = NA_character_,
        currency = pull$data$currency[1], exchange = pull$data$exchange[1],
        first_date = min(pull$data$date), last_date = max(pull$data$date),
        n_obs = nrow(pull$data),
        stringsAsFactors = FALSE
      )
    }

    if (pause_seconds > 0 && i < length(tickers)) Sys.sleep(pause_seconds)
  }

  prices <- if (length(frames) > 0) do.call(rbind, frames) else NULL
  list(prices = prices, status = do.call(rbind, status))
}

## Panel assembly ------------------------------------------------------------

# Long panel: the raw pull joined to the universe, so a ticker used by two indices
# appears once per index. Sorted by index order, then company, then date.
eis_long_panel <- function(prices, universe = eis_universe()) {
  out <- merge(
    universe[, c("index_code", "index", "index_order", "company", "ticker", "column")],
    prices,
    by = "ticker"
  )
  out <- out[order(out$index_order, out$company, out$ticker, out$date), ]
  out <- out[, c("date", "index_code", "index", "index_order", "company", "ticker",
                 "column", "currency", "exchange",
                 "open", "high", "low", "close", "adj_close", "volume")]
  rownames(out) <- NULL
  out
}

# Wide panel: one row per calendar date on which any market traded, one column per
# (index, ticker). Columns are ordered by index, then company, so the CSV reads
# left-to-right A through F.
eis_wide_panel <- function(long, universe = eis_universe(),
                           field = EIS_DEFAULT_FIELD) {
  if (!field %in% c("adj_close", "close")) {
    stop("field must be 'adj_close' or 'close', got: ", field)
  }

  dates <- sort(unique(long$date))
  wide <- data.frame(date = dates, stringsAsFactors = FALSE)

  columns <- universe$column[universe$column %in% long$column]
  for (col in columns) {
    slice <- long[long$column == col, c("date", field)]
    names(slice) <- c("date", col)
    wide <- merge(wide, slice, by = "date", all.x = TRUE)
  }

  wide <- wide[order(wide$date), c("date", columns)]
  rownames(wide) <- NULL
  wide
}

# Each column divided by its own first observation, times 100. Series with a shorter
# history (GE Vernova's 2024 spin, LG Energy Solution's 2022 listing, thyssenkrupp
# nucera's 2023 IPO) start at 100 on their own first trading day, which is the only
# rebasing that keeps them in the panel at all.
eis_rebase <- function(wide, digits = 4) {
  out <- wide
  value_cols <- setdiff(names(out), "date")

  for (col in value_cols) {
    x <- out[[col]]
    first_idx <- which(!is.na(x) & x > 0)
    out[[col]] <- if (length(first_idx) == 0) NA_real_ else round(100 * x / x[first_idx[1]], digits)
  }

  out
}

# The universe with the pull outcome attached: one row per (index, company).
eis_registry <- function(universe = eis_universe(), status = NULL) {
  out <- universe

  if (is.null(status)) {
    out$status <- "not fetched"
    out$reason <- NA_character_
    out$currency <- NA_character_
    out$exchange <- NA_character_
    out$first_date <- as.Date(NA)
    out$last_date <- as.Date(NA)
    out$n_obs <- NA_integer_
  } else {
    idx <- match(out$ticker, status$ticker)
    out$status <- status$status[idx]
    out$reason <- status$reason[idx]
    out$currency <- status$currency[idx]
    out$exchange <- status$exchange[idx]
    out$first_date <- status$first_date[idx]
    out$last_date <- status$last_date[idx]
    out$n_obs <- status$n_obs[idx]
  }

  out <- out[order(out$index_order, out$company), ]
  rownames(out) <- NULL
  out
}

## Build ---------------------------------------------------------------------

eis_cache_path <- function(repo_root, start_date, end_date) {
  config <- getOption("opportunity_security.config")
  dir <- file.path(repo_root, config$raw_data_dir, "equities")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(dir, sprintf("eis_equities_%s_%s.rds", start_date, end_date))
}

eis_equities_build <- function(repo_root,
                               start_date,
                               end_date,
                               field = EIS_DEFAULT_FIELD,
                               refresh = FALSE,
                               pause_seconds = 0.4) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  if (start_date >= end_date) stop("start_date must be before end_date.")

  universe <- eis_universe()
  indexes <- eis_index_table(universe = universe)
  tickers <- eis_listed_tickers(universe)

  cache <- eis_cache_path(repo_root, start_date, end_date)
  if (!refresh && file.exists(cache)) {
    message("Using cached pull: ", cache)
    pull <- readRDS(cache)
  } else {
    message("Pulling ", length(tickers), " symbols from the Yahoo chart API, ",
            start_date, " to ", end_date, " ...")
    pull <- eis_fetch_panel(
      tickers, start_date = start_date, end_date = end_date,
      pause_seconds = pause_seconds
    )
    saveRDS(pull, cache)
    message("Cached raw pull to ", cache)
  }

  if (is.null(pull$prices)) stop("No prices returned for any symbol.")

  long <- eis_long_panel(pull$prices, universe)
  wide <- eis_wide_panel(long, universe, field = field)
  rebased <- eis_rebase(wide)
  registry <- eis_registry(universe, pull$status)

  out_dir <- file.path(repo_root, getOption("opportunity_security.config")$processed_dir, "charts")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  stem <- sprintf("eis_equities_%s_%s", start_date, end_date)
  paths <- list(
    wide = file.path(out_dir, paste0(stem, "_wide.csv")),
    rebased = file.path(out_dir, paste0(stem, "_rebased.csv")),
    long = file.path(out_dir, paste0(stem, "_long.csv")),
    registry = file.path(out_dir, paste0(stem, "_registry.csv")),
    indexes = file.path(out_dir, "eis_equities_indexes.csv")
  )

  utils::write.csv(wide, paths$wide, row.names = FALSE, na = "")
  utils::write.csv(rebased, paths$rebased, row.names = FALSE, na = "")
  utils::write.csv(long, paths$long, row.names = FALSE, na = "")
  utils::write.csv(registry, paths$registry, row.names = FALSE, na = "")
  utils::write.csv(indexes, paths$indexes, row.names = FALSE, na = "")

  for (p in paths) message("Wrote ", p)

  failed <- registry[registry$status %in% "failed", ]
  if (nrow(failed) > 0) {
    warning(
      "No prices for ", length(unique(failed$ticker)), " of ", length(tickers), " symbols: ",
      paste(sprintf("%s (%s)", failed$ticker, failed$reason), collapse = "; "),
      call. = FALSE
    )
  }

  message(sprintf(
    "Panel: %d dates x %d columns across %d indices, %s to %s, %d of %d symbols with data.",
    nrow(wide), ncol(wide) - 1L, nrow(indexes), min(wide$date), max(wide$date),
    sum(pull$status$status == "ok"), length(tickers)
  ))

  invisible(list(
    universe = universe, indexes = indexes, registry = registry,
    long = long, wide = wide, rebased = rebased, paths = paths
  ))
}

opsi_eis_equities_run_directly <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  length(file_arg) > 0 &&
    identical(basename(sub("^--file=", "", file_arg[1])), "eis_equities.R")
}

if (opsi_eis_equities_run_directly()) {
  end_date <- as.Date(Sys.getenv("EIS_END_DATE", as.character(Sys.Date())))
  years <- as.numeric(Sys.getenv("EIS_YEARS", as.character(EIS_DEFAULT_YEARS)))
  start_default <- seq(end_date, by = sprintf("-%d months", round(years * 12)), length.out = 2)[2]
  start_date <- as.Date(Sys.getenv("EIS_START_DATE", as.character(start_default)))

  invisible(eis_equities_build(
    repo_root = repo_root,
    start_date = start_date,
    end_date = end_date,
    field = Sys.getenv("EIS_FIELD", EIS_DEFAULT_FIELD),
    refresh = tolower(Sys.getenv("EIS_REFRESH", "false")) %in% c("1", "true", "yes"),
    pause_seconds = as.numeric(Sys.getenv("EIS_PAUSE", "0.4"))
  ))
}


eis<-read.csv("data/processed/charts/eis_equities_2021-08-18_2026-08-18_long.csv")

eis_asia <- eis %>%
  filter(exchange %in% c("KSE", "Tokyo")) %>%
  mutate(index = str_remove(index, "^EIS ")) %>%
  group_by(company) %>%
  mutate(
    base_price = adj_close[date == as.Date("2022-11-01")][1],
    indexed = (adj_close / base_price -1) * 100
  ) %>%
  ungroup()  %>%
  select(date,index,company,indexed) %>%
  filter(date=="2026-08-14")
  #pivot_wider(names_from="company",values_from="indexed")

write.csv(eis_asia,"data/processed/charts/eis_asia.csv")

         