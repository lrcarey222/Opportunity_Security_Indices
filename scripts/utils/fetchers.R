# Fetchers for raw inputs that have a public API.
#
# Design rules, in priority order:
#   1. Never destroy a good local file. Every fetch writes to a temp path, is checked
#      against a declared column/row contract, and only then atomically replaces the
#      target. A failed or malformed fetch leaves the staged copy untouched.
#   2. Emit the schema the pipeline already consumes. These fetchers reproduce the
#      DataBank / IMF Data Explorer export layouts, so no downstream code changes.
#   3. Fail soft by default. A network outage should not break a build that has usable
#      local data; set OPSI_REQUIRE_FETCH=true to make fetch failures fatal.

## Config -------------------------------------------------------------------

opsi_fetch_enabled <- function() {
  !(tolower(Sys.getenv("OPSI_SKIP_FETCH", "false")) %in% c("1", "true", "yes"))
}

opsi_fetch_required <- function() {
  tolower(Sys.getenv("OPSI_REQUIRE_FETCH", "false")) %in% c("1", "true", "yes")
}

# How old a fetched file may be before it is refreshed, by upstream cadence.
OPSI_FETCH_MAX_AGE_DAYS <- c(
  daily = 1, monthly = 25, quarterly = 80, semiannual = 150,
  annual = 300, irregular = 300, discontinued = Inf,
  continuous = 25, `ad-hoc` = Inf, `per-run` = 0, unknown = 90
)

opsi_fetch_max_age <- function(cadence) {
  age <- OPSI_FETCH_MAX_AGE_DAYS[[if (is.null(cadence) || is.na(cadence)) "unknown" else cadence]]
  if (is.null(age)) 90 else age
}

## HTTP ---------------------------------------------------------------------

opsi_http_get <- function(url,
                          accept = NULL,
                          retries = 3,
                          timeout_seconds = 300,
                          backoff_seconds = 2) {
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("Package 'curl' is required for API fetchers.")
  }

  headers <- list("User-Agent" = "OSI-pipeline/1.0 (+https://github.com/lrcarey222/Opportunity_Security_Indices)")
  if (!is.null(accept)) headers[["Accept"]] <- accept

  last_error <- NULL
  for (attempt in seq_len(retries)) {
    result <- tryCatch({
      handle <- curl::new_handle()
      curl::handle_setheaders(handle, .list = headers)
      curl::handle_setopt(handle, timeout = timeout_seconds, connecttimeout = 60, followlocation = TRUE)
      response <- curl::curl_fetch_memory(url, handle = handle)

      if (response$status_code >= 400) {
        stop("HTTP ", response$status_code, " for ", url)
      }
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

opsi_http_get_text <- function(url, ...) {
  txt <- rawToChar(opsi_http_get(url, ...))
  Encoding(txt) <- "UTF-8"
  txt
}

opsi_http_get_json <- function(url, ...) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required for API fetchers.")
  }
  jsonlite::fromJSON(opsi_http_get_text(url, ...), simplifyVector = FALSE)
}

## Contract validation ------------------------------------------------------

# A fetch result is only trusted if it looks like the file the pipeline expects.
opsi_validate_contract <- function(df, contract, label) {
  problems <- character()

  if (!is.data.frame(df)) {
    return(paste0(label, ": fetcher returned a ", class(df)[1], ", not a data frame"))
  }

  missing_cols <- setdiff(contract$required_columns, names(df))
  if (length(missing_cols) > 0) {
    problems <- c(problems, paste0("missing column(s): ", paste(missing_cols, collapse = ", ")))
  }

  if (!is.null(contract$min_rows) && nrow(df) < contract$min_rows) {
    problems <- c(problems, paste0("only ", nrow(df), " rows, expected at least ", contract$min_rows))
  }

  if (!is.null(contract$min_time_columns)) {
    n_time <- sum(grepl(contract$time_column_pattern, names(df)))
    if (n_time < contract$min_time_columns) {
      problems <- c(problems, paste0(
        "only ", n_time, " time column(s) matching ", contract$time_column_pattern,
        ", expected at least ", contract$min_time_columns
      ))
    }
  }

  # Duplicate keys are the failure mode that silently corrupts downstream joins:
  # an upstream API may return sub-national or aggregate rows that a curated export
  # would have filtered out.
  if (!is.null(contract$unique_key) && all(contract$unique_key %in% names(df))) {
    keys <- do.call(paste, c(lapply(contract$unique_key, function(cn) df[[cn]]), sep = "\r"))
    n_dupes <- sum(duplicated(keys))
    if (n_dupes > 0) {
      offenders <- unique(keys[duplicated(keys)])
      problems <- c(problems, paste0(
        n_dupes, " duplicate row(s) on key (", paste(contract$unique_key, collapse = ", "), "); e.g. ",
        paste(gsub("\r", "/", utils::head(offenders, 3)), collapse = "; ")
      ))
    }
  }

  if (length(problems) == 0) return(NULL)
  paste0(label, ": ", paste(problems, collapse = "; "))
}

# Write only after the contract passes, and swap into place atomically.
opsi_write_validated <- function(df, dest_path, contract, label) {
  problem <- opsi_validate_contract(df, contract, label)
  if (!is.null(problem)) stop(problem)

  dest_dir <- dirname(dest_path)
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

  tmp_path <- paste0(dest_path, ".tmp-fetch")
  on.exit(if (file.exists(tmp_path)) unlink(tmp_path), add = TRUE)

  utils::write.csv(df, tmp_path, row.names = FALSE, na = "")

  # Re-read to confirm the file round-trips into the shape consumers will see.
  reread <- utils::read.csv(tmp_path, nrows = 5, check.names = TRUE)
  if (ncol(reread) < length(contract$required_columns)) {
    stop(label, ": written file did not round-trip to the expected column count")
  }

  if (!file.rename(tmp_path, dest_path)) {
    if (!file.copy(tmp_path, dest_path, overwrite = TRUE)) {
      stop(label, ": could not move fetched file into place")
    }
  }
  invisible(dest_path)
}

## Registry -----------------------------------------------------------------

# Each fetcher: id (matches a manifest entry), fn returning a data frame, and the
# contract that output must satisfy.
OPSI_FETCHERS <- new.env(parent = emptyenv())

register_fetcher <- function(id, fn, contract, description = NULL) {
  assign(id, list(id = id, fn = fn, contract = contract, description = description), envir = OPSI_FETCHERS)
  invisible(id)
}

get_fetcher <- function(id) {
  if (!exists(id, envir = OPSI_FETCHERS, inherits = FALSE)) return(NULL)
  get(id, envir = OPSI_FETCHERS, inherits = FALSE)
}

list_fetchers <- function() sort(ls(envir = OPSI_FETCHERS))

# Run one fetcher. Returns "fetched", "fresh", "skipped", or "failed".
run_fetcher <- function(id,
                        dest_path,
                        cadence = "unknown",
                        force = FALSE,
                        quiet = FALSE) {
  fetcher <- get_fetcher(id)
  if (is.null(fetcher)) return("skipped")

  if (!opsi_fetch_enabled()) return("skipped")

  if (!force && file.exists(dest_path)) {
    age_days <- as.numeric(difftime(Sys.time(), file.info(dest_path)$mtime, units = "days"))
    if (age_days < opsi_fetch_max_age(cadence)) {
      if (!quiet) message("  ", id, ": local copy is ", round(age_days), "d old (", cadence, "); skipping fetch")
      return("fresh")
    }
  }

  outcome <- tryCatch({
    if (!quiet) message("  ", id, ": fetching...")
    df <- fetcher$fn()
    opsi_write_validated(df, dest_path, fetcher$contract, id)
    if (!quiet) message("  ", id, ": wrote ", nrow(df), " rows to ", basename(dest_path))
    "fetched"
  }, error = function(e) {
    msg <- paste0("  ", id, ": fetch failed - ", conditionMessage(e))
    if (opsi_fetch_required()) stop(msg, call. = FALSE)
    if (file.exists(dest_path)) {
      warning(msg, "\n    Keeping existing file: ", basename(dest_path), call. = FALSE)
    } else {
      warning(msg, "\n    No local fallback exists.", call. = FALSE)
    }
    "failed"
  })

  outcome
}

## Shared helpers -----------------------------------------------------------

# Pivot long observations into the wide, one-column-per-period layout used by the
# DataBank and IMF Data Explorer exports the pipeline was built against.
opsi_pivot_periods_wide <- function(long_df,
                                    id_cols,
                                    period_col = "period",
                                    value_col = "value",
                                    period_order = NULL) {
  stopifnot(all(c(id_cols, period_col, value_col) %in% names(long_df)))

  periods <- unique(long_df[[period_col]])
  periods <- periods[!is.na(periods)]
  periods <- if (is.null(period_order)) sort(periods) else period_order[period_order %in% periods]

  keys <- do.call(paste, c(lapply(id_cols, function(cn) long_df[[cn]]), sep = "\r"))
  unique_keys <- unique(keys)

  out <- long_df[match(unique_keys, keys), id_cols, drop = FALSE]
  rownames(out) <- NULL

  row_index <- match(keys, unique_keys)
  col_index <- match(long_df[[period_col]], periods)

  for (j in seq_along(periods)) {
    out[[periods[j]]] <- NA_real_
  }

  values <- suppressWarnings(as.numeric(long_df[[value_col]]))
  valid <- !is.na(row_index) & !is.na(col_index)

  # Fill column-wise; last write wins for duplicate (key, period) pairs.
  for (j in seq_along(periods)) {
    sel <- valid & col_index == j
    if (!any(sel)) next
    out[[periods[j]]][row_index[sel]] <- values[sel]
  }

  out
}

# Normalise an SDMX time period to the canonical spellings the parsers expect:
# "2024", "2024-Q1", "2024-M01". Upstream sometimes emits "2024-01" for monthly.
opsi_normalize_period <- function(x) {
  x <- trimws(as.character(x))

  quarterly <- grepl("^\\d{4}-?Q[1-4]$", x, ignore.case = TRUE)
  x[quarterly] <- sub("^(\\d{4})-?[Qq]([1-4])$", "\\1-Q\\2", x[quarterly])

  monthly_m <- grepl("^\\d{4}-?M\\d{1,2}$", x, ignore.case = TRUE)
  x[monthly_m] <- sprintf(
    "%s-M%02d",
    sub("^(\\d{4}).*$", "\\1", x[monthly_m]),
    as.integer(sub("^\\d{4}-?[Mm](\\d{1,2})$", "\\1", x[monthly_m]))
  )

  monthly_iso <- grepl("^\\d{4}-\\d{1,2}$", x)
  x[monthly_iso] <- sprintf(
    "%s-M%02d",
    sub("^(\\d{4})-.*$", "\\1", x[monthly_iso]),
    as.integer(sub("^\\d{4}-(\\d{1,2})$", "\\1", x[monthly_iso]))
  )

  x
}

# Order periods the way the IMF Data Explorer exports do: within each year the annual
# column comes first, then each quarter immediately followed by its three months
# (2021, 2021-Q1, 2021-M01, 2021-M02, 2021-M03, 2021-Q2, 2021-M04, ...).
# Ordering is cosmetic for the parsers, which read the period out of the column name,
# but matching the staged layout keeps fetched and staged files diffable.
opsi_order_periods <- function(periods) {
  year <- suppressWarnings(as.integer(substr(periods, 1, 4)))
  rank_within <- rep(0, length(periods))

  is_q <- grepl("-Q[1-4]$", periods)
  quarter_num <- as.integer(sub(".*-Q", "", periods[is_q]))
  rank_within[is_q] <- quarter_num * 10L

  is_m <- grepl("-M\\d{2}$", periods)
  month_num <- as.integer(sub(".*-M", "", periods[is_m]))
  month_quarter <- ((month_num - 1L) %/% 3L) + 1L
  rank_within[is_m] <- month_quarter * 10L + (month_num - 3L * (month_quarter - 1L))

  periods[order(year, rank_within)]
}

source_fetcher_files <- function(repo_root) {
  fetcher_dir <- file.path(repo_root, "scripts", "fetchers")
  if (!dir.exists(fetcher_dir)) return(invisible(character()))
  files <- sort(list.files(fetcher_dir, pattern = "\\.R$", full.names = TRUE))
  for (f in files) source(f, local = FALSE)
  invisible(files)
}
