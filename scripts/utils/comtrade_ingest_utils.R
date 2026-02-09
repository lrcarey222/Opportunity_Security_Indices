# Utility helpers for UN Comtrade ingestion.

resolve_comtrade_reporters <- function(wdi_country_info, reporter_ref) {
  reporter_iso_col <- intersect(c("iso_3", "iso3_code", "iso3c"), names(reporter_ref))
  if (length(reporter_iso_col) == 0) {
    stop("Unable to locate ISO3 reporter codes in comtradr reporter reference data.")
  }

  valid_reporters <- reporter_ref[[reporter_iso_col[1]]]
  valid_reporters <- as.character(valid_reporters)
  valid_reporters <- valid_reporters[!is.na(valid_reporters) & nzchar(valid_reporters)]
  valid_reporters <- unique(valid_reporters[nchar(valid_reporters) == 3])

  excluded_iso3 <- c("ASM", "CHI", "GUM", "IMN", "LIE", "MAF", "MCO", "PRI", "XKX")
  valid_reporters <- setdiff(valid_reporters, excluded_iso3)

  wdi_candidates <- character()
  if ("iso3c" %in% names(wdi_country_info)) {
    wdi_candidates <- as.character(wdi_country_info$iso3c)
    wdi_candidates <- wdi_candidates[!is.na(wdi_candidates) & nzchar(wdi_candidates)]
    wdi_candidates <- unique(wdi_candidates[nchar(wdi_candidates) == 3])
    wdi_candidates <- setdiff(wdi_candidates, excluded_iso3)
  }

  reporter_candidates <- intersect(wdi_candidates, valid_reporters)
  if (length(reporter_candidates) < 50) {
    reporter_candidates <- valid_reporters
  }
  if (length(reporter_candidates) == 0) {
    stop("No valid reporter codes remain after filtering against comtradr reference data.")
  }

  reporter_candidates
}

split_by_nchar <- function(x, max_chars = 2500) {
  chunks <- list()
  cur <- character()
  cur_len <- 0
  for (code in x) {
    add_len <- nchar(code) + ifelse(length(cur) == 0, 0, 1)
    if (cur_len + add_len > max_chars) {
      chunks[[length(chunks) + 1]] <- cur
      cur <- code
      cur_len <- nchar(code)
    } else {
      cur <- c(cur, code)
      cur_len <- cur_len + add_len
    }
  }
  if (length(cur)) {
    chunks[[length(chunks) + 1]] <- cur
  }
  chunks
}

split_vec <- function(x, chunk_size) {
  if (length(x) == 0) {
    return(list(character()))
  }
  split(x, ceiling(seq_along(x) / chunk_size))
}

ct_get_data_with_timeout <- function(timeout_seconds = 120, ...) {
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

fetch_comtrade_grid <- function(reporters,
                                partners,
                                code_chunks,
                                years,
                                flows,
                                partner_chunk_size = 50,
                                sleep_seconds = 0.5,
                                retries = 3,
                                request_timeout_seconds = 120,
                                chunk_index = 1L,
                                chunk_count = 1L) {
  if (length(reporters) == 0 || length(partners) == 0 || length(code_chunks) == 0) {
    return(data.frame())
  }

  partner_chunks <- split_vec(partners, chunk_size = partner_chunk_size)
  request_grid <- tidyr::expand_grid(
    rep = reporters,
    yr = years,
    dir = flows,
    cc = code_chunks,
    pch = partner_chunks
  )

  if (chunk_count > 1) {
    row_index <- seq_len(nrow(request_grid))
    selected_rows <- row_index[((row_index - 1) %% chunk_count) + 1 == chunk_index]
    request_grid <- request_grid[selected_rows, , drop = FALSE]
    message(
      "Running Comtrade request chunk ", chunk_index, "/", chunk_count,
      " with ", nrow(request_grid), " request(s)."
    )
  }

  output <- vector("list", nrow(request_grid))
  failed <- character()

  for (i in seq_len(nrow(request_grid))) {
    req <- request_grid[i, ]

    data_chunk <- NULL
    last_err <- NULL
    for (attempt in seq_len(retries)) {
      attempt_out <- tryCatch(
        ct_get_data_with_timeout(
          reporter = req$rep[[1]],
          partner = req$pch[[1]],
          commodity_code = req$cc[[1]],
          start_date = req$yr[[1]],
          end_date = req$yr[[1]],
          flow_direction = req$dir[[1]],
          timeout_seconds = request_timeout_seconds
        ),
        error = function(e) e
      )

      if (!inherits(attempt_out, "error")) {
        data_chunk <- attempt_out
        break
      }

      last_err <- attempt_out
      if (attempt < retries) {
        Sys.sleep(sleep_seconds * attempt)
      }
    }

    if (inherits(last_err, "error") && is.null(data_chunk)) {
      failed <- c(
        failed,
        paste0(
          "rep=", req$rep[[1]],
          ", yr=", req$yr[[1]],
          ", dir=", req$dir[[1]],
          ", codes=", paste(req$cc[[1]], collapse = ","),
          ", partners=", paste(req$pch[[1]], collapse = ","),
          " -> ", conditionMessage(last_err)
        )
      )
      next
    }

    if (!"flow_direction" %in% names(data_chunk)) {
      data_chunk <- dplyr::mutate(data_chunk, flow_direction = req$dir[[1]])
    }
    if ("trade_flow" %in% names(data_chunk)) {
      data_chunk <- dplyr::filter(data_chunk, tolower(trade_flow) == req$dir[[1]])
    }

    output[[i]] <- dplyr::mutate(
      data_chunk,
      reporter_req = req$rep[[1]],
      year_req = req$yr[[1]]
    )

    Sys.sleep(sleep_seconds)
  }

  if (length(failed) > 0) {
    stop(
      "UN Comtrade requests failed for ",
      length(failed),
      " request(s). Example failures:\n",
      paste(utils::head(failed, 5), collapse = "\n"),
      "\nHint: adjust COMTRADE_REQUEST_TIMEOUT_SECONDS / COMTRADE_MAX_RETRIES, ",
      "or run more chunks via COMTRADE_CHUNK_COUNT."
    )
  }

  dplyr::bind_rows(output) %>% dplyr::distinct()
}

stage_comtrade_output <- function(data, output_path, chunk_index = 1L, chunk_count = 1L) {
  if (chunk_count <= 1) {
    write.csv(data, output_path, row.names = FALSE)
    return(invisible(TRUE))
  }

  output_name <- tools::file_path_sans_ext(basename(output_path))
  chunk_dir <- file.path(dirname(output_path), "comtrade_chunks", output_name)
  if (!dir.exists(chunk_dir)) {
    dir.create(chunk_dir, recursive = TRUE)
  }

  chunk_path <- file.path(
    chunk_dir,
    sprintf("chunk_%03d_of_%03d.csv", as.integer(chunk_index), as.integer(chunk_count))
  )
  write.csv(data, chunk_path, row.names = FALSE)

  expected_paths <- file.path(
    chunk_dir,
    sprintf("chunk_%03d_of_%03d.csv", seq_len(chunk_count), rep(chunk_count, chunk_count))
  )

  if (!all(file.exists(expected_paths))) {
    message(
      "Staged Comtrade chunk ", chunk_index, "/", chunk_count,
      " for ", basename(output_path), ".",
      " Run remaining chunks to finalize."
    )
    return(invisible(FALSE))
  }

  combined <- dplyr::bind_rows(lapply(expected_paths, read.csv)) %>% dplyr::distinct()
  write.csv(combined, output_path, row.names = FALSE)
  message("Combined ", chunk_count, " chunk files into ", output_path)
  invisible(TRUE)
}
