# Shared UN Comtrade client utilities for ingestion and timeseries pulls.

comtrade_set_key_from_env <- function() {
  comtrade_key <- Sys.getenv("COMTRADE_API_KEY")
  if (!nzchar(comtrade_key)) {
    stop(
      "COMTRADE_API_KEY is required. Set it in your environment before running Comtrade pulls.",
      call. = FALSE
    )
  }
  comtradr::set_primary_comtrade_key(comtrade_key)
  invisible(comtrade_key)
}

comtrade_detect_year_column <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(NA_character_)
  }
  candidates <- c("ref_year", "period", "year", "Year")
  hits <- candidates[candidates %in% names(df)]
  if (length(hits) == 0) {
    return(NA_character_)
  }
  hits[[1]]
}

comtrade_max_year <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(NA_integer_)
  }

  year_col <- comtrade_detect_year_column(df)
  if (is.na(year_col)) {
    return(NA_integer_)
  }

  year_vec <- suppressWarnings(as.integer(stringr::str_extract(as.character(df[[year_col]]), "\\d{4}")))
  year_vec <- year_vec[!is.na(year_vec)]
  if (length(year_vec) == 0) {
    return(NA_integer_)
  }

  max(year_vec)
}

comtrade_ct_get_data_with_timeout <- function(timeout_seconds = 120, ...) {
  if (.Platform$OS.type == "unix") {
    job <- parallel::mcparallel(comtradr::ct_get_data(...), silent = TRUE)
    result <- parallel::mccollect(job, wait = TRUE, timeout = timeout_seconds)
    if (is.null(result)) {
      tools::pskill(job$pid)
      stop("Comtrade request timed out after ", timeout_seconds, " seconds.")
    }
    value <- result[[1]]
    if (inherits(value, "try-error")) {
      stop(as.character(value))
    }
    return(value)
  }

  on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE), add = TRUE)
  setTimeLimit(cpu = Inf, elapsed = timeout_seconds, transient = TRUE)
  comtradr::ct_get_data(...)
}

comtrade_is_throttle_error <- function(err) {
  if (is.null(err)) {
    return(FALSE)
  }
  msg <- if (inherits(err, "error")) conditionMessage(err) else as.character(err)
  msg <- tolower(paste(msg, collapse = " "))
  grepl("429|throttl|rate\\s*limit|too\\s*many\\s*requests", msg)
}

comtrade_fetch_requests <- function(request_df,
                                    retries = 3,
                                    sleep_seconds = 0.5,
                                    timeout_seconds = 120,
                                    show_progress = interactive(),
                                    request_pause_seconds = 0) {
  required_cols <- c("reporter", "partner", "commodity_code", "start_date", "end_date", "flow_direction")
  missing_cols <- setdiff(required_cols, names(request_df))
  if (length(missing_cols) > 0) {
    stop("request_df is missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!"request_id" %in% names(request_df)) {
    request_df$request_id <- seq_len(nrow(request_df))
  }

  out <- vector("list", nrow(request_df))
  failed <- character()
  no_data <- character()

  pb <- NULL
  if (isTRUE(show_progress) && nrow(request_df) > 0) {
    pb <- progress::progress_bar$new(
      format = "Comtrade :current/:total [:bar] :percent eta=:eta rep=:rep flow=:flow yrs=:years",
      total = nrow(request_df),
      clear = FALSE,
      width = 90
    )
  }

  for (i in seq_len(nrow(request_df))) {
    req <- request_df[i, , drop = FALSE]

    data_chunk <- NULL
    last_err <- NULL
    throttle_multiplier <- 1

    for (attempt in seq_len(retries)) {
      attempt_out <- tryCatch(
        comtrade_ct_get_data_with_timeout(
          reporter = req$reporter[[1]],
          partner = req$partner[[1]],
          commodity_code = req$commodity_code[[1]],
          start_date = req$start_date[[1]],
          end_date = req$end_date[[1]],
          flow_direction = req$flow_direction[[1]],
          timeout_seconds = timeout_seconds
        ),
        error = function(e) e
      )

      if (!inherits(attempt_out, "error")) {
        data_chunk <- attempt_out
        break
      }

      last_err <- attempt_out
      if (comtrade_is_throttle_error(attempt_out)) {
        throttle_multiplier <- max(throttle_multiplier * 2, 2)
      }
      if (attempt < retries) {
        Sys.sleep((sleep_seconds * (2 ^ (attempt - 1))) * throttle_multiplier)
      }
    }

    req_label <- paste0(
      "id=", req$request_id[[1]],
      ", rep=", req$reporter[[1]],
      ", partner=", paste(req$partner[[1]], collapse = ","),
      ", flow=", req$flow_direction[[1]],
      ", years=", req$start_date[[1]], "-", req$end_date[[1]]
    )

    if (is.null(data_chunk)) {
      failed <- c(
        failed,
        paste0(
          req_label,
          " -> ",
          if (inherits(last_err, "error")) conditionMessage(last_err) else "unknown error"
        )
      )
    } else if (nrow(data_chunk) == 0) {
      no_data <- c(no_data, req_label)
    } else {
      out[[i]] <- dplyr::mutate(data_chunk, request_id = req$request_id[[1]])
    }

    if (!is.null(pb)) {
      pb$tick(tokens = list(
        rep = as.character(req$reporter[[1]]),
        flow = as.character(req$flow_direction[[1]]),
        years = paste0(req$start_date[[1]], "-", req$end_date[[1]])
      ))
    }

    if (request_pause_seconds > 0) {
      Sys.sleep(request_pause_seconds)
    }
  }

  list(
    data = dplyr::bind_rows(out) %>% dplyr::distinct(),
    failed = failed,
    no_data = no_data,
    request_count = nrow(request_df)
  )
}

comtrade_pick_latest_available_year <- function(probe_fn, candidate_year) {
  if (!is.function(probe_fn)) {
    stop("probe_fn must be a function that returns a data.frame for a year probe.")
  }

  years_to_try <- seq.int(candidate_year, candidate_year - 4L, by = -1L)
  for (yr in years_to_try) {
    probe <- tryCatch(probe_fn(yr), error = function(e) e)
    if (inherits(probe, "error")) {
      next
    }
    if (is.data.frame(probe) && nrow(probe) > 0) {
      return(as.integer(yr))
    }
  }

  warning(
    "No data returned for candidate year or previous 4 years; keeping requested year ",
    candidate_year,
    call. = FALSE
  )
  as.integer(candidate_year)
}
