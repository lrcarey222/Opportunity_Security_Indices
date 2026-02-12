# Pull a focused UN Comtrade timeseries for selected country/tech/supply_chain.

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

parse_bool_arg <- function(x, default = FALSE) {
  if (is.null(x) || !nzchar(x)) {
    return(default)
  }
  tolower(as.character(x)) %in% c("1", "true", "yes", "y")
}

parse_years_arg <- function(years_arg) {
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  if (is.null(years_arg) || !nzchar(years_arg)) {
    return((current_year - 5):(current_year - 1))
  }

  years_arg <- gsub("\\s+", "", years_arg)
  if (tolower(years_arg) == "latest5") {
    return((current_year - 5):(current_year - 1))
  }

  if (grepl(":", years_arg, fixed = TRUE)) {
    bounds <- as.integer(strsplit(years_arg, ":", fixed = TRUE)[[1]])
    if (length(bounds) != 2 || any(is.na(bounds))) {
      stop("Invalid --years range. Use format like --years=2021:2025")
    }
    return(seq.int(min(bounds), max(bounds)))
  }

  years <- as.integer(strsplit(years_arg, ",", fixed = TRUE)[[1]])
  years <- years[!is.na(years)]
  if (length(years) == 0) {
    stop("Invalid --years list. Use format like --years=2021,2022,2023")
  }
  sort(unique(years))
}

normalize_character_arg <- function(values, default = NULL) {
  if (is.null(values)) return(default)
  values <- as.character(values)
  values <- trimws(values)
  values <- values[nzchar(values)]
  if (length(values) == 0) return(default)
  if (length(values) == 1 && grepl(",", values, fixed = TRUE)) {
    values <- trimws(strsplit(values, ",", fixed = TRUE)[[1]])
    values <- values[nzchar(values)]
  }
  values <- unique(values)
  if (length(values) == 0) default else values
}

normalize_partners_arg <- function(partners) {
  normalize_character_arg(partners, default = "World")
}

normalize_frequency_arg <- function(frequency) {
  if (is.null(frequency) || !nzchar(as.character(frequency))) {
    return("A")
  }

  value <- tolower(trimws(as.character(frequency)[[1]]))
  mapping <- c(annual = "A", yearly = "A", year = "A", a = "A",
               monthly = "M", month = "M", m = "M")

  if (!value %in% names(mapping)) {
    stop("frequency must be one of: annual, monthly, A, or M")
  }

  mapping[[value]]
}

run_trade_timeseries_pull <- function(country,
                                      tech,
                                      supply_chain,
                                      partners,
                                      years,
                                      flow_direction = "export",
                                      frequency = "annual",
                                      catalog = NULL,
                                      hs6_catalog_path = NULL,
                                      output_path = NULL,
                                      write_output = TRUE,
                                      retries = 3,
                                      sleep_seconds = 0.5,
                                      max_code_chars = 2500,
                                      partner_chunk_size = 50,
                                      year_chunk_size = 12,
                                      request_pause_seconds = 0,
                                      timeout_seconds = 120,
                                      show_progress = interactive()) {
  country <- normalize_character_arg(country)
  tech <- normalize_character_arg(tech)
  partners <- normalize_partners_arg(partners)
  flow_direction <- tolower(normalize_character_arg(flow_direction, default = "export"))
  frequency <- normalize_frequency_arg(frequency)

  if (is.null(country) || is.null(tech) || is.null(supply_chain) || !nzchar(supply_chain)) {
    stop("country, tech, and supply_chain are required.")
  }

  repo_root <- getOption("opportunity_security.repo_root")
  config <- getOption("opportunity_security.config")
  if (is.null(repo_root) || is.null(config) || is.null(config$raw_data_dir)) {
    stop("Config missing. Source scripts/00_setup.R before running.")
  }

  source(file.path(repo_root, "scripts", "utils", "comtrade_client.R"))

  raw_data_path <- file.path(repo_root, config$raw_data_dir)
  if (is.null(catalog)) {
    if (is.null(hs6_catalog_path) || !nzchar(hs6_catalog_path)) {
      preferred_paths <- c(
        file.path(raw_data_path, "hs6_categories_with_essential.csv"),
        file.path(raw_data_path, "hts_codes_categories_bolstered_final.csv")
      )
      existing_path <- preferred_paths[file.exists(preferred_paths)]
      hs6_catalog_path <- if (length(existing_path) > 0) existing_path[[1]] else preferred_paths[[1]]
    }
    if (!file.exists(hs6_catalog_path)) {
      stop("HS6 catalog not found: ", hs6_catalog_path)
    }
    hs6_catalog <- utils::read.csv(hs6_catalog_path, stringsAsFactors = FALSE)
  } else {
    hs6_catalog <- as.data.frame(catalog, stringsAsFactors = FALSE)
  }

  if (is.null(output_path) || !nzchar(output_path)) {
    safe_country <- gsub("[^A-Za-z0-9]+", "_", paste(country, collapse = "_"))
    safe_tech <- gsub("[^A-Za-z0-9]+", "_", paste(tech, collapse = "_"))
    safe_chain <- gsub("[^A-Za-z0-9]+", "_", supply_chain)
    output_path <- file.path(raw_data_path, "trade_timeseries", paste0("trade_timeseries_", safe_country, "_", safe_tech, "_", safe_chain, ".csv"))
  }

  comtrade_set_key_from_env()
  request_grid <- build_trade_timeseries_request_grid(
    country = country,
    tech = tech,
    supply_chain = supply_chain,
    years = years,
    hs6_catalog = hs6_catalog,
    partner = partners,
    flow_direction = flow_direction,
    max_code_chars = max_code_chars,
    partner_chunk_size = partner_chunk_size,
    year_chunk_size = year_chunk_size
  )

  request_df <- dplyr::mutate(
    request_grid,
    request_id = dplyr::row_number(),
    reporter = rep,
    partner = pch,
    commodity_code = cc,
    start_date = ys,
    end_date = ye,
    flow_direction = dir,
    frequency = frequency
  ) %>%
    dplyr::select(request_id, reporter, partner, commodity_code, start_date, end_date, flow_direction, frequency)

  fetch_out <- comtrade_fetch_requests(
    request_df = request_df,
    retries = retries,
    sleep_seconds = sleep_seconds,
    timeout_seconds = timeout_seconds,
    show_progress = show_progress,
    request_pause_seconds = request_pause_seconds
  )

  if (length(fetch_out$failed) > 0) {
    warning("Some Comtrade requests failed (showing up to 5):\n", paste(utils::head(fetch_out$failed, 5), collapse = "\n"))
  }

  if (length(fetch_out$no_data) > 0) {
    message("Comtrade requests with no rows: ", length(fetch_out$no_data))
  }

  if (nrow(fetch_out$data) == 0) {
    trade_tbl <- fetch_out$data
  } else {
    request_lookup <- split(request_grid, seq_len(nrow(request_grid)))
    data_by_request <- split(fetch_out$data, fetch_out$data$request_id)
    tagged <- lapply(names(data_by_request), function(req_id) {
      req <- request_lookup[[as.integer(req_id)]]
      trade_tag_response_chunk(
        data_chunk = dplyr::select(data_by_request[[req_id]], -request_id),
        req = req,
        tech = req$tech[[1]],
        supply_chain = supply_chain
      )
    })
    trade_tbl <- dplyr::bind_rows(tagged) %>% dplyr::distinct()
  }

  requested_year_start <- min(years)
  requested_year_end <- max(years)
  actual_max_year <- comtrade_max_year(trade_tbl)
  message("Requested year range: ", requested_year_start, "-", requested_year_end)
  message("Max year actually returned: ", ifelse(is.na(actual_max_year), "NA", as.character(actual_max_year)))
  if (is.na(actual_max_year) || actual_max_year < requested_year_end) {
    warning("Requested end year ", requested_year_end, " not fully available in returned data.")
  }

  if (isTRUE(write_output)) {
    output_dir <- dirname(output_path)
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    utils::write.csv(trade_tbl, output_path, row.names = FALSE)
    message("Output: ", output_path)
  }

  trade_tbl
}


pull_trade_timeseries <- function(country,
                                  tech,
                                  supply_chain,
                                  partners = "World",
                                  years = (as.integer(format(Sys.Date(), "%Y")) - 5):(as.integer(format(Sys.Date(), "%Y")) - 1),
                                  flow = "export",
                                  frequency = "annual",
                                  catalog = NULL,
                                  hs6_catalog_path = NULL,
                                  output_path = NULL,
                                  write_output = FALSE,
                                  retries = 3,
                                  sleep_seconds = 0.5,
                                  max_code_chars = 2500,
                                  partner_chunk_size = 50,
                                  year_chunk_size = 12,
                                  request_pause_seconds = 0,
                                  timeout_seconds = 120,
                                  show_progress = interactive()) {
  years_parsed <- if (is.character(years) && length(years) == 1) parse_years_arg(years) else years
  run_trade_timeseries_pull(
    country = country,
    tech = tech,
    supply_chain = supply_chain,
    partners = partners,
    years = years_parsed,
    flow_direction = flow,
    frequency = frequency,
    catalog = catalog,
    hs6_catalog_path = hs6_catalog_path,
    output_path = output_path,
    write_output = write_output,
    retries = retries,
    sleep_seconds = sleep_seconds,
    max_code_chars = max_code_chars,
    partner_chunk_size = partner_chunk_size,
    year_chunk_size = year_chunk_size,
    request_pause_seconds = request_pause_seconds,
    timeout_seconds = timeout_seconds,
    show_progress = show_progress
  )
}

if (sys.nframe() == 0) {
  args_all <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args_all, value = TRUE)
  script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[1]) else file.path(getwd(), "scripts", "96_pull_trade_timeseries.R")
  repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)

  source(file.path(repo_root, "scripts", "00_setup.R"))
  source(file.path(getOption("opportunity_security.repo_root"), "R", "charts", "trade_timeseries.R"))

  args <- parse_args(commandArgs(trailingOnly = TRUE))
  years <- parse_years_arg(args[["years"]])
  partners <- normalize_partners_arg(args[["partners"]])

  refresh <- parse_bool_arg(args[["refresh"]], default = FALSE)
  output_path <- args[["output"]]
  if (!is.null(output_path) && file.exists(output_path) && !refresh) {
    message("Using existing output (refresh=false): ", output_path)
    print(utils::read.csv(output_path, stringsAsFactors = FALSE))
    quit(save = "no", status = 0)
  }

  run_trade_timeseries_pull(
    country = args[["country"]],
    tech = args[["tech"]],
    supply_chain = args[["supply-chain"]] %||% args[["supply_chain"]],
    partners = partners,
    years = years,
    flow_direction = args[["flow"]] %||% "export",
    frequency = args[["frequency"]] %||% "annual",
    hs6_catalog_path = args[["hs6-catalog"]] %||% args[["hs6_catalog"]],
    output_path = output_path,
    write_output = TRUE,
    retries = as.integer(args[["retries"]] %||% "3"),
    sleep_seconds = as.numeric(args[["sleep-seconds"]] %||% "0.5"),
    max_code_chars = as.integer(args[["max-code-chars"]] %||% "2500"),
    partner_chunk_size = as.integer(args[["partner-chunk-size"]] %||% "50"),
    year_chunk_size = as.integer(args[["year-chunk-size"]] %||% "12"),
    request_pause_seconds = as.numeric(args[["request-pause-seconds"]] %||% "0"),
    timeout_seconds = as.numeric(args[["timeout-seconds"]] %||% "120"),
    show_progress = TRUE
  )
}
