# Pull a focused UN Comtrade timeseries for selected country/tech/supply_chain.
#
# Example:
# COMTRADE_API_KEY="<your-key>" \
# Rscript R/charts/96_pull_trade_timeseries.R \
#   --country="USA" \
#   --tech="Batteries" \
#   --supply-chain="Midstream" \
#   --partners="CHN,FRA,DEU,ITA,ESP,NLD,BEL,SWE,POL,DNK,FIN,CZE,ROU,HUN,AUT,PRT,GRC,IRL,JPN,KOR,IND,VNM" \
#   --years="2021:2025" \
#   --flow="export"


`%||%` <- function(x, y) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) {
    return(y)
  }
  x
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

parse_years_arg <- function(years_arg) {
  if (is.null(years_arg) || !nzchar(years_arg)) {
    return(2021:2025)
  }

  years_arg <- gsub("\\s+", "", years_arg)
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

normalize_partners_arg <- function(partners) {
  if (is.null(partners)) {
    return("World")
  }

  partners <- as.character(partners)
  partners <- trimws(partners)
  partners <- partners[nzchar(partners)]

  if (length(partners) == 0) {
    return("World")
  }

  if (length(partners) == 1 && grepl(",", partners, fixed = TRUE)) {
    partners <- trimws(strsplit(partners, ",", fixed = TRUE)[[1]])
    partners <- partners[nzchar(partners)]
  }

  if (length(partners) == 0) {
    "World"
  } else {
    partners
  }
}


resolve_repo_root_local <- function() {
  repo_root <- getOption("opportunity_security.repo_root")
  if (!is.null(repo_root) && nzchar(repo_root)) {
    return(repo_root)
  }

  if (requireNamespace("rprojroot", quietly = TRUE)) {
    return(rprojroot::find_root(rprojroot::is_git_root))
  }

  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!file.exists(file.path(d, ".git")) && dirname(d) != d) {
    d <- dirname(d)
  }
  if (!file.exists(file.path(d, ".git"))) {
    stop("Unable to resolve repo root; run from the repository root.")
  }
  d
}

ensure_trade_timeseries_helpers <- function() {
  if (exists("build_trade_timeseries_request_grid", mode = "function") &&
      exists("trade_tag_response_chunk", mode = "function")) {
    return(invisible(TRUE))
  }

  repo_root <- resolve_repo_root_local()
  source(file.path(repo_root, "R", "charts", "trade_timeseries.R"))

  if (!exists("build_trade_timeseries_request_grid", mode = "function") ||
      !exists("trade_tag_response_chunk", mode = "function")) {
    stop("trade_timeseries helpers were not loaded. Check R/charts/trade_timeseries.R")
  }

  invisible(TRUE)
}


ensure_setup_loaded <- function() {
  config <- getOption("opportunity_security.config")
  if (!is.null(config) && !is.null(config$raw_data_dir)) {
    return(invisible(TRUE))
  }

  repo_root <- resolve_repo_root_local()
  source(file.path(repo_root, "scripts", "00_setup.R"))

  config <- getOption("opportunity_security.config")
  if (is.null(config) || is.null(config$raw_data_dir)) {
    stop("Failed to load config from scripts/00_setup.R")
  }

  invisible(TRUE)
}

run_trade_timeseries_pull <- function(country,
                                      tech,
                                      supply_chain,
                                      partners = NULL,
                                      partner = NULL,
                                      years = 2021:2025,
                                      flow_direction = "export",
                                      flow = NULL,
                                      hs6_catalog_path = NULL,
                                      output_path = NULL,
                                      retries = 3,
                                      sleep_seconds = 0.5,
                                      max_code_chars = 2500,
                                      partner_chunk_size = 50) {
  if (is.null(country) || !nzchar(country)) {
    stop("country is required.")
  }
  if (is.null(tech) || !nzchar(tech)) {
    stop("tech is required.")
  }
  if (is.null(supply_chain) || !nzchar(supply_chain)) {
    stop("supply_chain is required.")
  }

  if (is.null(flow) || !nzchar(as.character(flow))) {
    flow <- flow_direction
  }
  flow_direction <- as.character(flow)[1]

  if (is.null(partners) || length(partners) == 0) {
    partners <- partner
  }
  partners <- normalize_partners_arg(partners)

  if (!requireNamespace("comtradr", quietly = TRUE)) {
    stop("Package 'comtradr' is required.")
  }

  ensure_trade_timeseries_helpers()

  repo_root <- getOption("opportunity_security.repo_root")
  if (is.null(repo_root) || !nzchar(repo_root)) {
    if (requireNamespace("rprojroot", quietly = TRUE)) {
      repo_root <- rprojroot::find_root(rprojroot::is_git_root)
    } else {
      stop("Unable to resolve repo root. Run from the repository root.")
    }
  }

  ensure_setup_loaded()
  config <- getOption("opportunity_security.config")

  raw_data_path <- file.path(repo_root, config$raw_data_dir)
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

  if (is.null(output_path) || !nzchar(output_path)) {
    safe_country <- gsub("[^A-Za-z0-9]+", "_", country)
    safe_tech <- gsub("[^A-Za-z0-9]+", "_", tech)
    safe_chain <- gsub("[^A-Za-z0-9]+", "_", supply_chain)
    output_path <- file.path(
      raw_data_path,
      "trade_timeseries",
      paste0("trade_timeseries_", safe_country, "_", safe_tech, "_", safe_chain, ".csv")
    )
  }

  comtrade_key <- Sys.getenv("COMTRADE_API_KEY")
  if (comtrade_key == "") {
    stop("Set COMTRADE_API_KEY before running this script.")
  }
  comtradr::set_primary_comtrade_key(comtrade_key)

  hs6_catalog <- utils::read.csv(hs6_catalog_path, stringsAsFactors = FALSE)

  request_grid <- build_trade_timeseries_request_grid(
    country = country,
    tech = tech,
    supply_chain = supply_chain,
    years = years,
    hs6_catalog = hs6_catalog,
    partner = partners,
    flow_direction = flow_direction,
    max_code_chars = max_code_chars,
    partner_chunk_size = partner_chunk_size
  )

  output <- vector("list", nrow(request_grid))
  failed <- character()

  for (i in seq_len(nrow(request_grid))) {
    req <- request_grid[i, ]

    data_chunk <- NULL
    last_err <- NULL
    for (attempt in seq_len(retries)) {
      attempt_out <- tryCatch(
        comtradr::ct_get_data(
          reporter = req$rep[[1]],
          partner = req$pch[[1]],
          commodity_code = req$cc[[1]],
          start_date = req$yr[[1]],
          end_date = req$yr[[1]],
          flow_direction = req$dir[[1]]
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

    if (is.null(data_chunk)) {
      failed <- c(
        failed,
        paste0(
          "country=", req$rep[[1]],
          ", year=", req$yr[[1]],
          ", flow=", req$dir[[1]],
          " -> ",
          if (inherits(last_err, "error")) conditionMessage(last_err) else "unknown error"
        )
      )
      next
    }

    output[[i]] <- trade_tag_response_chunk(
      data_chunk = data_chunk,
      req = req,
      tech = tech,
      supply_chain = supply_chain
    )

    Sys.sleep(sleep_seconds)
  }

  trade_tbl <- dplyr::bind_rows(output) %>% dplyr::distinct()

  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  utils::write.csv(trade_tbl, output_path, row.names = FALSE)

  if (length(failed) > 0) {
    warning(
      "Some Comtrade requests failed (showing up to 5):\n",
      paste(utils::head(failed, 5), collapse = "\n")
    )
  }

  message("Rows written: ", nrow(trade_tbl))
  message("Output: ", output_path)

  invisible(list(
    data = trade_tbl,
    output_path = output_path,
    failed = failed
  ))
}

pull_trade_timeseries <- function(country,
                                  tech,
                                  supply_chain,
                                  partners = NULL,
                                  partner = NULL,
                                  years = 2021:2025,
                                  flow = "export",
                                  flow_direction = NULL,
                                  hs6_catalog_path = NULL,
                                  output_path = NULL,
                                  retries = 3,
                                  sleep_seconds = 0.5,
                                  max_code_chars = 2500,
                                  partner_chunk_size = 50) {
  years_parsed <- if (is.character(years) && length(years) == 1) {
    parse_years_arg(years)
  } else {
    years
  }

  if (is.null(partners) || length(partners) == 0) {
    partners <- partner
  }
  partners_parsed <- normalize_partners_arg(partners)

  if (is.null(flow_direction) || !nzchar(as.character(flow_direction))) {
    flow_direction <- flow
  }

  run_trade_timeseries_pull(
    country = country,
    tech = tech,
    supply_chain = supply_chain,
    partners = partners_parsed,
    years = years_parsed,
    flow_direction = flow_direction,
    hs6_catalog_path = hs6_catalog_path,
    output_path = output_path,
    retries = retries,
    sleep_seconds = sleep_seconds,
    max_code_chars = max_code_chars,
    partner_chunk_size = partner_chunk_size
  )
}

if (sys.nframe() == 0) {
  args_all <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args_all, value = TRUE)
  script_path <- if (length(file_arg) > 0) {
    sub("^--file=", "", file_arg[1])
  } else {
    file.path(getwd(), "R", "charts", "96_pull_trade_timeseries.R")
  }
  repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = FALSE)

  source(file.path(repo_root, "scripts", "00_setup.R"))
  source(file.path(getOption("opportunity_security.repo_root"), "R", "charts", "trade_timeseries.R"))

  args <- parse_args(commandArgs(trailingOnly = TRUE))

  years <- parse_years_arg(args[["years"]])
  partners <- normalize_partners_arg(args[["partners"]])

  run_trade_timeseries_pull(
    country = args[["country"]],
    tech = args[["tech"]],
    supply_chain = args[["supply-chain"]] %||% args[["supply_chain"]],
    partners = partners,
    years = years,
    flow_direction = args[["flow"]] %||% "export",
    hs6_catalog_path = args[["hs6-catalog"]] %||% args[["hs6_catalog"]],
    output_path = args[["output"]],
    retries = as.integer(args[["retries"]] %||% "3"),
    sleep_seconds = as.numeric(args[["sleep-seconds"]] %||% "0.5"),
    max_code_chars = as.integer(args[["max-code-chars"]] %||% "2500"),
    partner_chunk_size = as.integer(args[["partner-chunk-size"]] %||% "50")
  )
}
