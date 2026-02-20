source(local({
  # Prefer sys.frame(1)$ofile when sourced (e.g., from run_pipeline.R).
  sf <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  this_file <- if (!is.null(sf) && nzchar(sf)) sf else {
    ofiles <- vapply(sys.frames(), function(fr) {
      of <- tryCatch(fr$ofile, error = function(e) NULL)
      if (is.null(of) || !nzchar(of)) "" else as.character(of)
    }, character(1))
    ofiles <- ofiles[nzchar(ofiles)]
    if (length(ofiles) > 0) ofiles[[length(ofiles)]] else {
      # Fallback for direct Rscript execution of this script.
      fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
      if (length(fa) > 0) sub("^--file=", "", fa[1]) else ""
    }
  }
  if (!nzchar(this_file)) {
    candidate <- file.path(normalizePath(getwd(), winslash = "/", mustWork = FALSE), "scripts", "utils", "bootstrap.R")
    if (file.exists(candidate)) return(candidate)
    stop("Unable to resolve script path for bootstrap.")
  }
  file.path(dirname(normalizePath(this_file, winslash = "/", mustWork = FALSE)), "utils", "bootstrap.R")
}))

# Bulk UN Comtrade pull for HS92 trade and country totals.
#
# Downloads annual import/export data for all reporters from 2020:2025,
# plus monthly import/export for selected reporters from 2010:2025.

`%||%` <- function(x, y) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
}

parse_args <- function(args) {
  out <- list()
  for (arg in args) {
    if (!stringr::str_starts(arg, "--")) {
      next
    }
    split <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1]]
    key <- split[1]
    value <- if (length(split) > 1) split[2] else TRUE
    out[[key]] <- value
  }
  out
}

parse_years_arg <- function(years_arg, default_years) {
  if (is.null(years_arg) || !nzchar(years_arg)) {
    return(default_years)
  }

  years_arg <- gsub("\\s+", "", as.character(years_arg))
  if (grepl(":", years_arg, fixed = TRUE)) {
    bounds <- as.integer(strsplit(years_arg, ":", fixed = TRUE)[[1]])
    if (length(bounds) != 2 || any(is.na(bounds))) {
      stop("Invalid years range. Use format like 2020:2025")
    }
    return(seq.int(min(bounds), max(bounds)))
  }

  years <- as.integer(strsplit(years_arg, ",", fixed = TRUE)[[1]])
  years <- years[!is.na(years)]
  if (length(years) == 0) {
    stop("Invalid years list. Use format like 2020,2021,2022")
  }
  sort(unique(years))
}

resolve_hs_essentials_path <- function(raw_data_path, explicit_path = NULL) {
  if (!is.null(explicit_path) && nzchar(explicit_path)) {
    return(explicit_path)
  }

  candidates <- c(
    file.path(raw_data_path, "hs6_categories_with_essential.csv"),
    file.path(raw_data_path, "hts_codes_categories_bolstered_final.csv")
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) {
    stop(
      "Could not locate HS essentials file. Expected one of: ",
      paste(candidates, collapse = ", ")
    )
  }
  existing[[1]]
}

extract_hs92_codes <- function(hs_tbl) {
  candidate_cols <- c("HS92", "HS6", "hs92", "hs6", "code", "commodity_code")
  col <- candidate_cols[candidate_cols %in% names(hs_tbl)]
  if (length(col) == 0) {
    stop("HS essentials file must include one of columns: ", paste(candidate_cols, collapse = ", "))
  }

  codes <- hs_tbl[[col[[1]]]]
  codes <- as.character(codes)
  codes <- trimws(codes)
  codes <- stringr::str_extract(codes, "\\d+")
  codes <- codes[nzchar(codes)]
  codes <- stringr::str_pad(codes, width = 6, side = "left", pad = "0")
  codes <- codes[stringr::str_detect(codes, "^\\d{6}$")]
  sort(unique(codes))
}

extract_reporter_iso3 <- function(reporter_ref) {
  if (!is.data.frame(reporter_ref) || nrow(reporter_ref) == 0) {
    return(character())
  }

  candidate_cols <- c(
    "iso3_code", "reporter_iso", "reporterISO", "reporter_iso3",
    "ReporterISO", "reporterCodeIsoAlpha3", "reporter_code_iso_alpha_3"
  )
  match_cols <- candidate_cols[candidate_cols %in% names(reporter_ref)]

  if (length(match_cols) == 0) {
    # Fallback: detect likely 3-letter reporter code columns.
    likely_name <- grepl("iso|alpha", names(reporter_ref), ignore.case = TRUE)
    likely_vals <- vapply(
      reporter_ref,
      function(col) {
        vals <- as.character(col)
        vals <- vals[nzchar(vals)]
        if (length(vals) == 0) {
          return(FALSE)
        }
        mean(grepl("^[A-Z]{3}$", toupper(vals))) > 0.8
      },
      logical(1)
    )
    fallback_cols <- names(reporter_ref)[likely_name & likely_vals]
    if (length(fallback_cols) == 0) {
      stop(
        "Unable to detect reporter ISO3 column from ct_get_ref_table('reporter'). ",
        "Available columns: ",
        paste(names(reporter_ref), collapse = ", ")
      )
    }
    match_cols <- fallback_cols[[1]]
  }

  reporters <- as.character(reporter_ref[[match_cols[[1]]]])
  reporters <- trimws(reporters)
  reporters <- toupper(reporters)
  reporters <- reporters[nzchar(reporters)]
  reporters <- reporters[grepl("^[A-Z]{3}$", reporters)]
  sort(unique(reporters))
}

split_vec <- function(x, chunk_size) {
  if (length(x) == 0) {
    return(list())
  }
  idx <- ceiling(seq_along(x) / chunk_size)
  split(x, idx)
}

build_bulk_requests <- function(reporters,
                                hs_codes,
                                years,
                                frequency,
                                chunk_size,
                                year_chunk_size = 1,
                                flow_direction = c("import", "export"),
                                partner = "World") {
  code_chunks <- split_vec(hs_codes, chunk_size)
  year_chunks <- split_vec(sort(unique(as.integer(years))), max(1L, as.integer(year_chunk_size)))

  req <- expand.grid(
    reporter = reporters,
    flow_direction = flow_direction,
    year_chunk_id = seq_along(year_chunks),
    code_chunk_id = seq_along(code_chunks),
    stringsAsFactors = FALSE
  )

  req$partner <- partner
  req$start_date <- vapply(req$year_chunk_id, function(i) min(year_chunks[[i]]), integer(1))
  req$end_date <- vapply(req$year_chunk_id, function(i) max(year_chunks[[i]]), integer(1))
  req$commodity_code <- vapply(req$code_chunk_id, function(i) paste(code_chunks[[i]], collapse = ","), character(1))
  req$frequency <- frequency
  req$request_id <- seq_len(nrow(req))

  req[, c("request_id", "reporter", "partner", "commodity_code", "start_date", "end_date", "flow_direction", "frequency")]
}

build_total_requests <- function(reporters,
                                 years,
                                 frequency,
                                 year_chunk_size = 1,
                                 flow_direction = c("import", "export"),
                                 partner = "World") {
  year_chunks <- split_vec(sort(unique(as.integer(years))), max(1L, as.integer(year_chunk_size)))

  req <- expand.grid(
    reporter = reporters,
    flow_direction = flow_direction,
    year_chunk_id = seq_along(year_chunks),
    stringsAsFactors = FALSE
  )

  req$partner <- partner
  req$start_date <- vapply(req$year_chunk_id, function(i) min(year_chunks[[i]]), integer(1))
  req$end_date <- vapply(req$year_chunk_id, function(i) max(year_chunks[[i]]), integer(1))
  req$commodity_code <- "TOTAL"
  req$frequency <- frequency
  req$request_id <- seq_len(nrow(req))

  req[, c("request_id", "reporter", "partner", "commodity_code", "start_date", "end_date", "flow_direction", "frequency")]
}

run_bulk_comtrade_download <- function(
    annual_years = 2020:2025,
    monthly_years = 2010:2025,
    monthly_reporters = c("USA", "AUS", "CAN", "JPN", "VNM", "KOR", "IND", "CHN"),
    hs_chunk_size = 120,
    annual_year_chunk_size = length(annual_years),
    monthly_year_chunk_size = 1,
    output_dir = NULL,
    hs_essentials_path = NULL,
    retries = 3,
    sleep_seconds = 0.5,
    max_retry_sleep_seconds = 120,
    request_pause_seconds = 0.2,
    timeout_seconds = 120,
    show_progress = interactive()) {
  repo_root <- getOption("opportunity_security.repo_root")
  config <- getOption("opportunity_security.config")
  if (is.null(repo_root) || is.null(config) || is.null(config$raw_data_dir)) {
    stop("Config missing. Source scripts/00_setup.R before running.")
  }

  source(file.path(repo_root, "scripts", "utils", "comtrade_client.R"))

  raw_data_path <- file.path(repo_root, config$raw_data_dir)
  hs_path <- resolve_hs_essentials_path(raw_data_path, hs_essentials_path)
  hs_tbl <- utils::read.csv(hs_path, stringsAsFactors = FALSE)
  hs_codes <- extract_hs92_codes(hs_tbl)

  if (length(hs_codes) == 0) {
    stop("No HS92/HS6 codes found in file: ", hs_path)
  }

  reporter_ref <- comtradr::ct_get_ref_table("reporter")
  reporters <- extract_reporter_iso3(reporter_ref)

  monthly_reporters <- sort(unique(monthly_reporters))
  monthly_reporters <- monthly_reporters[monthly_reporters %in% reporters]
  if (length(monthly_reporters) == 0) {
    warning("None of the requested monthly reporters were found in Comtrade reporter table.")
  }

  message("HS code count: ", length(hs_codes))
  message("Annual reporter count: ", length(reporters))
  message("Monthly reporter count: ", length(monthly_reporters))

  if (is.null(output_dir) || !nzchar(output_dir)) {
    output_dir <- file.path(raw_data_path, "comtrade_bulk")
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  annual_hs_path <- file.path(output_dir, "comtrade_bulk_annual_hs92.csv")
  annual_total_path <- file.path(output_dir, "comtrade_bulk_annual_totals.csv")
  monthly_hs_path <- file.path(output_dir, "comtrade_bulk_monthly_hs92_selected_reporters.csv")
  monthly_total_path <- file.path(output_dir, "comtrade_bulk_monthly_totals_selected_reporters.csv")
  failed_path <- file.path(output_dir, "comtrade_bulk_failures.log")

  persist_stage_output <- function(stage_name, stage_out, output_path, failed_log_path) {
    # Ensure no inherited elapsed time limit affects large file writes.
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
    utils::write.csv(stage_out$data, output_path, row.names = FALSE)

    if (length(stage_out$failed) > 0) {
      failed_lines <- paste(stage_name, stage_out$failed, sep = ": ")
      write(
        x = failed_lines,
        file = failed_log_path,
        append = TRUE
      )
    }

    message("Wrote: ", output_path)
  }

  if (file.exists(failed_path)) {
    file.remove(failed_path)
  }

  comtrade_set_key_from_env()

  annual_hs_requests <- build_bulk_requests(
    reporters = reporters,
    hs_codes = hs_codes,
    years = annual_years,
    frequency = "A",
    chunk_size = hs_chunk_size,
    year_chunk_size = annual_year_chunk_size
  )
  annual_total_requests <- build_total_requests(
    reporters = reporters,
    years = annual_years,
    frequency = "A",
    year_chunk_size = annual_year_chunk_size
  )

  monthly_hs_requests <- if (length(monthly_reporters) > 0) {
    build_bulk_requests(
      reporters = monthly_reporters,
      hs_codes = hs_codes,
      years = monthly_years,
      frequency = "M",
      chunk_size = hs_chunk_size,
      year_chunk_size = monthly_year_chunk_size
    )
  } else {
    annual_hs_requests[0, ]
  }

  monthly_total_requests <- if (length(monthly_reporters) > 0) {
    build_total_requests(
      reporters = monthly_reporters,
      years = monthly_years,
      frequency = "M",
      year_chunk_size = monthly_year_chunk_size
    )
  } else {
    annual_total_requests[0, ]
  }

  message("Annual HS request count: ", nrow(annual_hs_requests))
  message("Annual totals request count: ", nrow(annual_total_requests))
  message("Monthly HS request count: ", nrow(monthly_hs_requests))
  message("Monthly totals request count: ", nrow(monthly_total_requests))

  annual_hs_out <- comtrade_fetch_requests(
    request_df = annual_hs_requests,
    retries = retries,
    sleep_seconds = sleep_seconds,
    max_sleep_seconds = max_retry_sleep_seconds,
    timeout_seconds = timeout_seconds,
    show_progress = show_progress,
    request_pause_seconds = request_pause_seconds
  )
  persist_stage_output("annual_hs", annual_hs_out, annual_hs_path, failed_path)

  annual_total_out <- comtrade_fetch_requests(
    request_df = annual_total_requests,
    retries = retries,
    sleep_seconds = sleep_seconds,
    max_sleep_seconds = max_retry_sleep_seconds,
    timeout_seconds = timeout_seconds,
    show_progress = show_progress,
    request_pause_seconds = request_pause_seconds
  )
  persist_stage_output("annual_totals", annual_total_out, annual_total_path, failed_path)

  monthly_hs_out <- comtrade_fetch_requests(
    request_df = monthly_hs_requests,
    retries = retries,
    sleep_seconds = sleep_seconds,
    max_sleep_seconds = max_retry_sleep_seconds,
    timeout_seconds = timeout_seconds,
    show_progress = show_progress,
    request_pause_seconds = request_pause_seconds
  )
  persist_stage_output("monthly_hs", monthly_hs_out, monthly_hs_path, failed_path)

  monthly_total_out <- comtrade_fetch_requests(
    request_df = monthly_total_requests,
    retries = retries,
    sleep_seconds = sleep_seconds,
    max_sleep_seconds = max_retry_sleep_seconds,
    timeout_seconds = timeout_seconds,
    show_progress = show_progress,
    request_pause_seconds = request_pause_seconds
  )
  persist_stage_output("monthly_totals", monthly_total_out, monthly_total_path, failed_path)

  list(
    annual_hs = annual_hs_out,
    annual_totals = annual_total_out,
    monthly_hs = monthly_hs_out,
    monthly_totals = monthly_total_out,
    output_dir = output_dir,
    hs_path = hs_path
  )
}

if (sys.nframe() == 0) {
  repo_root <- resolve_repo_root()

  args <- parse_args(commandArgs(trailingOnly = TRUE))

  annual_years <- parse_years_arg(args[["annual-years"]] %||% args[["annual_years"]], 2020:2025)
  monthly_years <- parse_years_arg(args[["monthly-years"]] %||% args[["monthly_years"]], 2010:2025)

  monthly_reporters <- args[["monthly-reporters"]] %||% args[["monthly_reporters"]]
  if (is.null(monthly_reporters) || !nzchar(monthly_reporters)) {
    monthly_reporters <- c("USA", "AUS", "CAN", "JPN", "VNM", "KOR", "IND", "CHN")
  } else {
    monthly_reporters <- stringr::str_split(monthly_reporters, ",", simplify = FALSE)[[1]]
    monthly_reporters <- trimws(monthly_reporters)
    monthly_reporters <- monthly_reporters[nzchar(monthly_reporters)]
  }

  run_bulk_comtrade_download(
    annual_years = annual_years,
    monthly_years = monthly_years,
    monthly_reporters = monthly_reporters,
    hs_chunk_size = as.integer(args[["hs-chunk-size"]] %||% args[["hs_chunk_size"]] %||% "120"),
    annual_year_chunk_size = as.integer(args[["annual-year-chunk-size"]] %||% args[["annual_year_chunk_size"]] %||% as.character(length(annual_years))),
    monthly_year_chunk_size = as.integer(args[["monthly-year-chunk-size"]] %||% args[["monthly_year_chunk_size"]] %||% "1"),
    output_dir = args[["output-dir"]] %||% args[["output_dir"]],
    hs_essentials_path = args[["hs-essentials-path"]] %||% args[["hs_essentials_path"]],
    retries = as.integer(args[["retries"]] %||% "3"),
    sleep_seconds = as.numeric(args[["sleep-seconds"]] %||% args[["sleep_seconds"]] %||% "0.5"),
    max_retry_sleep_seconds = as.numeric(args[["max-retry-sleep-seconds"]] %||% args[["max_retry_sleep_seconds"]] %||% "120"),
    request_pause_seconds = as.numeric(args[["request-pause-seconds"]] %||% args[["request_pause_seconds"]] %||% "0.2"),
    timeout_seconds = as.numeric(args[["timeout-seconds"]] %||% args[["timeout_seconds"]] %||% "120"),
    show_progress = TRUE
  )
}
