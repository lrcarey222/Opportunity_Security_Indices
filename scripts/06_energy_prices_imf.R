# IMF Primary Commodity Price System (PCPS) energy price volatility helper.

imf_pcps_base_url <- "https://dataservices.imf.org/REST/SDMX_JSON.svc"

# IMF PCPS caching keeps DataStructure metadata local for resilience.
# Override behavior via options:
# - opportunity_security.imf_pcps_timeout (seconds)
# - opportunity_security.imf_pcps_retries (integer)
# - opportunity_security.imf_pcps_backoff (numeric vector, seconds)
# - opportunity_security.imf_pcps_use_cache (logical)
# - opportunity_security.imf_pcps_soft_fail (logical)
# - opportunity_security.processed_dir (cache root when set)

imf_pcps_cache_dir <- function() {
  processed_dir <- getOption("opportunity_security.processed_dir")
  if (!is.null(processed_dir) && nzchar(processed_dir)) {
    return(file.path(processed_dir, "cache", "imf_pcps"))
  }
  file.path(tempdir(), "imf_pcps_cache")
}

imf_pcps_cache_file <- function(endpoint) {
  safe_name <- gsub("[^A-Za-z0-9]+", "_", endpoint)
  file.path(imf_pcps_cache_dir(), paste0(safe_name, ".json"))
}

imf_pcps_fetch_options <- function() {
  list(
    timeout_sec = getOption("opportunity_security.imf_pcps_timeout", 300),
    retries = getOption("opportunity_security.imf_pcps_retries", 4),
    backoff = getOption("opportunity_security.imf_pcps_backoff", c(1, 2, 4, 8)),
    use_cache = getOption("opportunity_security.imf_pcps_use_cache", TRUE),
    soft_fail = getOption("opportunity_security.imf_pcps_soft_fail", FALSE)
  )
}

imf_pcps_fetch_json <- function(endpoint,
                                timeout_sec = 300,
                                retries = 4,
                                backoff = c(1, 2, 4, 8),
                                cache_path = NULL,
                                use_cache = TRUE,
                                soft_fail = FALSE) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required to ingest IMF PCPS data.")
  }
  url <- paste0(imf_pcps_base_url, "/", endpoint)
  retry_status <- c(429, 500, 502, 503, 504)
  max_attempts <- retries + 1
  last_error <- NULL

  if (is.null(cache_path) && use_cache && identical(endpoint, "DataStructure/IMF/PCPS")) {
    cache_path <- imf_pcps_cache_file(endpoint)
  }

  if (requireNamespace("curl", quietly = TRUE)) {
    for (attempt in seq_len(max_attempts)) {
      handle <- curl::new_handle()
      curl::handle_setopt(handle, timeout = timeout_sec, useragent = "OpportunitySecurityIndices/IMF-PCPS")
      result <- tryCatch(
        curl::curl_fetch_memory(url, handle = handle),
        error = function(err) err
      )
      if (inherits(result, "error")) {
        last_error <- result$message
      } else if (!is.null(result$status_code) && result$status_code %in% retry_status) {
        last_error <- sprintf("HTTP %s from IMF PCPS.", result$status_code)
      } else if (!is.null(result$status_code) && result$status_code >= 400) {
        stop(sprintf("IMF PCPS request failed for %s (HTTP %s).", endpoint, result$status_code))
      } else {
        content_text <- rawToChar(result$content)
        if (use_cache && !is.null(cache_path)) {
          cache_dir <- dirname(cache_path)
          if (!dir.exists(cache_dir)) {
            dir.create(cache_dir, recursive = TRUE)
          }
          try(writeLines(content_text, cache_path), silent = TRUE)
        }
        return(jsonlite::fromJSON(content_text, simplifyVector = TRUE))
      }

      if (attempt < max_attempts) {
        delay <- backoff[min(attempt, length(backoff))]
        Sys.sleep(delay)
      }
    }
  } else {
    old_timeout <- getOption("timeout")
    on.exit(options(timeout = old_timeout), add = TRUE)
    for (attempt in seq_len(max_attempts)) {
      options(timeout = timeout_sec)
      result <- tryCatch(readLines(url, warn = FALSE), error = function(err) err)
      if (!inherits(result, "error")) {
        content_text <- paste(result, collapse = "\n")
        if (use_cache && !is.null(cache_path)) {
          cache_dir <- dirname(cache_path)
          if (!dir.exists(cache_dir)) {
            dir.create(cache_dir, recursive = TRUE)
          }
          try(writeLines(content_text, cache_path), silent = TRUE)
        }
        return(jsonlite::fromJSON(content_text, simplifyVector = TRUE))
      }
      last_error <- result$message
      if (attempt < max_attempts) {
        delay <- backoff[min(attempt, length(backoff))]
        Sys.sleep(delay)
      }
    }
  }

  if (use_cache && !is.null(cache_path) && file.exists(cache_path)) {
    cached_text <- tryCatch(readLines(cache_path, warn = FALSE), error = function(err) err)
    if (!inherits(cached_text, "error")) {
      return(jsonlite::fromJSON(paste(cached_text, collapse = "\n"), simplifyVector = TRUE))
    }
  }

  if (soft_fail) {
    warning("IMF PCPS fetch failed; returning empty result.", call. = FALSE)
    return(NULL)
  }

  stop(sprintf(
    "IMF PCPS fetch failed for %s after %s attempts: %s",
    endpoint,
    max_attempts,
    ifelse(is.null(last_error), "unknown error", last_error)
  ))
}

imf_pcps_dimensions_df <- function(dimensions) {
  if (is.data.frame(dimensions)) {
    return(dimensions)
  }
  if (!is.list(dimensions)) {
    stop("Unexpected IMF PCPS dimensions format.")
  }
  dims <- lapply(dimensions, function(dim) {
    as.data.frame(dim, stringsAsFactors = FALSE)
  })
  do.call(rbind, dims)
}

imf_pcps_codelist_df <- function(codelist) {
  code <- codelist$Code
  if (is.null(code)) {
    return(data.frame(code = character(), label = character(), stringsAsFactors = FALSE))
  }
  if (is.data.frame(code)) {
    code_df <- code
  } else if (is.list(code)) {
    code_df <- do.call(
      rbind,
      lapply(code, function(item) as.data.frame(item, stringsAsFactors = FALSE))
    )
  } else {
    return(data.frame(code = character(), label = character(), stringsAsFactors = FALSE))
  }
  label <- character(nrow(code_df))
  if ("Description" %in% names(code_df)) {
    label <- vapply(code_df$Description, function(desc) {
      if (is.list(desc) && "#text" %in% names(desc)) {
        desc[["#text"]]
      } else if (is.character(desc)) {
        desc
      } else {
        NA_character_
      }
    }, character(1))
  }
  data.frame(
    code = code_df[["@value"]],
    label = label,
    stringsAsFactors = FALSE
  )
}

imf_pcps_get_commodity_codes <- function() {
  opts <- imf_pcps_fetch_options()
  dsd <- imf_pcps_fetch_json(
    "DataStructure/IMF/PCPS",
    timeout_sec = opts$timeout_sec,
    retries = opts$retries,
    backoff = opts$backoff,
    cache_path = imf_pcps_cache_file("DataStructure/IMF/PCPS"),
    use_cache = opts$use_cache,
    soft_fail = opts$soft_fail
  )
  if (is.null(dsd)) {
    warning("IMF PCPS metadata unavailable; returning empty commodity list.", call. = FALSE)
    return(data.frame(code = character(), label = character(), stringsAsFactors = FALSE))
  }
  dimensions <- dsd$Structure$KeyFamilies$KeyFamily$Components$Dimension
  dims_df <- imf_pcps_dimensions_df(dimensions)

  concept_cols <- c("@conceptRef", "@id")
  concept_vals <- dims_df[, concept_cols[concept_cols %in% names(dims_df)], drop = FALSE]
  concept_vals <- apply(concept_vals, 1, function(row) paste(row, collapse = " "))
  commodity_idx <- grepl("COMMODITY|ITEM|PRODUCT", toupper(concept_vals))
  if (!any(commodity_idx)) {
    stop("Unable to locate commodity dimension in IMF PCPS dataset.")
  }
  commodity_dim <- dims_df[commodity_idx, , drop = FALSE]
  codelist_id <- commodity_dim[["@codelist"]][1]
  codelists <- dsd$Structure$CodeLists$CodeList
  if (is.data.frame(codelists)) {
    codelist <- codelists[codelists[["@id"]] == codelist_id, , drop = FALSE]
  } else {
    idx <- which(vapply(codelists, function(cl) cl[["@id"]], character(1)) == codelist_id)
    if (length(idx) == 0) {
      stop("Unable to locate IMF PCPS commodity code list.")
    }
    codelist <- codelists[[idx[1]]]
  }
  imf_pcps_codelist_df(codelist)
}

imf_pcps_match_commodities <- function(commodity_df, patterns) {
  if (nrow(commodity_df) == 0) {
    return(data.frame(tech = character(), commodity_code = character(), commodity_label = character()))
  }
  labels <- tolower(commodity_df$label)
  matches <- lapply(names(patterns), function(tech) {
    pattern <- patterns[[tech]]
    idx <- grepl(pattern, labels)
    if (!any(idx)) {
      return(NULL)
    }
    data.frame(
      tech = tech,
      commodity_code = commodity_df$code[idx],
      commodity_label = commodity_df$label[idx],
      stringsAsFactors = FALSE
    )
  })
  matches <- matches[!vapply(matches, is.null, logical(1))]
  if (length(matches) == 0) {
    return(data.frame(tech = character(), commodity_code = character(), commodity_label = character()))
  }
  do.call(rbind, matches)
}

imf_pcps_build_series_key <- function(dimensions, commodity_code, freq_value = "M") {
  dims_df <- imf_pcps_dimensions_df(dimensions)
  dim_ids <- dims_df[["@id"]]
  dim_ids_upper <- toupper(dim_ids)
  values <- rep(".", length(dim_ids))
  freq_idx <- which(dim_ids_upper == "FREQ")
  if (length(freq_idx) == 1) {
    values[freq_idx] <- freq_value
  }
  concept_vals <- dims_df[, c("@conceptRef", "@id")[c("@conceptRef", "@id") %in% names(dims_df)], drop = FALSE]
  concept_vals <- apply(concept_vals, 1, function(row) paste(row, collapse = " "))
  commodity_idx <- which(grepl("COMMODITY|ITEM|PRODUCT", toupper(concept_vals)))
  if (length(commodity_idx) == 0) {
    stop("Unable to locate commodity dimension for IMF PCPS series key.")
  }
  values[commodity_idx[1]] <- commodity_code
  paste(values, collapse = ".")
}

imf_pcps_obs_to_df <- function(obs) {
  if (is.null(obs)) {
    return(data.frame(date = as.Date(character()), value = numeric()))
  }
  obs_df <- as.data.frame(obs, stringsAsFactors = FALSE)
  time_col <- if ("@TIME_PERIOD" %in% names(obs_df)) "@TIME_PERIOD" else "TIME_PERIOD"
  value_col <- if ("@OBS_VALUE" %in% names(obs_df)) "@OBS_VALUE" else "OBS_VALUE"
  data.frame(
    date = obs_df[[time_col]],
    value = as.numeric(obs_df[[value_col]]),
    stringsAsFactors = FALSE
  )
}

imf_pcps_fetch_series <- function(series_key, start_period, end_period) {
  endpoint <- sprintf(
    "CompactData/IMF/PCPS/%s?startPeriod=%s&endPeriod=%s",
    series_key,
    start_period,
    end_period
  )
  opts <- imf_pcps_fetch_options()
  data <- imf_pcps_fetch_json(
    endpoint,
    timeout_sec = opts$timeout_sec,
    retries = opts$retries,
    backoff = opts$backoff,
    use_cache = FALSE,
    soft_fail = opts$soft_fail
  )
  if (is.null(data)) {
    return(list())
  }
  series <- data$CompactData$DataSet$Series
  if (is.null(series)) {
    return(list())
  }
  if (is.data.frame(series)) {
    series <- list(series)
  } else if (is.list(series) && "Obs" %in% names(series)) {
    series <- list(series)
  }
  series
}

imf_pcps_prices_from_series <- function(series_list, tech, commodity_code, commodity_label) {
  if (length(series_list) == 0) {
    return(data.frame())
  }
  obs_tbls <- lapply(series_list, function(series) {
    obs_df <- imf_pcps_obs_to_df(series$Obs)
    if (nrow(obs_df) == 0) {
      return(NULL)
    }
    obs_df$tech <- tech
    obs_df$commodity_code <- commodity_code
    obs_df$commodity_label <- commodity_label
    obs_df
  })
  obs_tbls <- obs_tbls[!vapply(obs_tbls, is.null, logical(1))]
  if (length(obs_tbls) == 0) {
    return(data.frame())
  }
  do.call(rbind, obs_tbls)
}

imf_pcps_compute_volatility <- function(price_df) {
  if (nrow(price_df) == 0) {
    return(data.frame())
  }
  date_str <- as.character(price_df$date)
  date_str <- ifelse(
    nchar(date_str) == 7,
    paste0(date_str, "-01"),
    ifelse(nchar(date_str) == 4, paste0(date_str, "-01-01"), date_str)
  )
  price_df$date <- as.Date(date_str)
  split_keys <- interaction(price_df$tech, price_df$commodity_code, drop = TRUE)
  per_series <- lapply(split(price_df, split_keys), function(df) {
    df <- df[order(df$date), , drop = FALSE]
    df <- df[!is.na(df$value), , drop = FALSE]
    if (nrow(df) < 3) {
      return(NULL)
    }
    returns <- diff(log(df$value))
    volatility <- stats::sd(returns, na.rm = TRUE) * sqrt(12)
    data.frame(
      tech = df$tech[1],
      commodity_code = df$commodity_code[1],
      commodity_label = df$commodity_label[1],
      volatility_10y = volatility,
      stringsAsFactors = FALSE
    )
  })
  per_series <- do.call(rbind, per_series)
  if (is.null(per_series) || nrow(per_series) == 0) {
    return(data.frame())
  }
  tech_vol <- aggregate(
    volatility_10y ~ tech,
    data = per_series,
    FUN = mean,
    na.rm = TRUE
  )
  range_vals <- range(tech_vol$volatility_10y, na.rm = TRUE)
  if (diff(range_vals) == 0) {
    tech_vol$volatility_index <- NA_real_
  } else {
    tech_vol$volatility_index <- (tech_vol$volatility_10y - range_vals[1]) / diff(range_vals)
  }
  list(tech_vol = tech_vol, series_vol = per_series)
}

imf_pcps_energy_prices <- function(start_year, end_year) {
  opts <- imf_pcps_fetch_options()
  commodity_df <- imf_pcps_get_commodity_codes()
  if (nrow(commodity_df) == 0 && opts$soft_fail) {
    warning("IMF PCPS commodity list empty; returning empty energy price results.", call. = FALSE)
    return(list(
      prices = data.frame(
        date = as.Date(character()),
        value = numeric(),
        tech = character(),
        commodity_code = character(),
        commodity_label = character(),
        stringsAsFactors = FALSE
      ),
      tech_vol = data.frame(
        tech = character(),
        volatility_10y = numeric(),
        volatility_index = numeric(),
        start_year = integer(),
        end_year = integer(),
        stringsAsFactors = FALSE
      ),
      series_vol = data.frame(
        tech = character(),
        commodity_code = character(),
        commodity_label = character(),
        volatility_10y = numeric(),
        start_year = integer(),
        end_year = integer(),
        stringsAsFactors = FALSE
      )
    ))
  }
  target_patterns <- list(
    Coal = "coal",
    Oil = "crude oil|petroleum|oil",
    Gas = "natural gas|gas|lng",
    Cobalt = "cobalt",
    Lithium = "lithium",
    Graphite = "graphite",
    `Rare Earths` = "rare earth",
    Copper = "copper",
    Manganese = "manganese",
    Nickel = "nickel",
    Zinc = "zinc",
    PGMs = "platinum|palladium|pgm"
  )
  commodity_map <- imf_pcps_match_commodities(commodity_df, target_patterns)
  if (is.null(commodity_map) || nrow(commodity_map) == 0) {
    stop("No IMF PCPS commodities matched for coal, oil, gas, or critical minerals.")
  }

  dsd <- imf_pcps_fetch_json(
    "DataStructure/IMF/PCPS",
    timeout_sec = opts$timeout_sec,
    retries = opts$retries,
    backoff = opts$backoff,
    cache_path = imf_pcps_cache_file("DataStructure/IMF/PCPS"),
    use_cache = opts$use_cache,
    soft_fail = opts$soft_fail
  )
  if (is.null(dsd)) {
    warning("IMF PCPS metadata unavailable; returning empty energy price results.", call. = FALSE)
    return(list(
      prices = data.frame(
        date = as.Date(character()),
        value = numeric(),
        tech = character(),
        commodity_code = character(),
        commodity_label = character(),
        stringsAsFactors = FALSE
      ),
      tech_vol = data.frame(
        tech = character(),
        volatility_10y = numeric(),
        volatility_index = numeric(),
        start_year = integer(),
        end_year = integer(),
        stringsAsFactors = FALSE
      ),
      series_vol = data.frame(
        tech = character(),
        commodity_code = character(),
        commodity_label = character(),
        volatility_10y = numeric(),
        start_year = integer(),
        end_year = integer(),
        stringsAsFactors = FALSE
      )
    ))
  }
  dimensions <- dsd$Structure$KeyFamilies$KeyFamily$Components$Dimension

  price_tbls <- lapply(seq_len(nrow(commodity_map)), function(i) {
    tech <- commodity_map$tech[i]
    commodity_code <- commodity_map$commodity_code[i]
    commodity_label <- commodity_map$commodity_label[i]
    series_key <- imf_pcps_build_series_key(dimensions, commodity_code)
    series_list <- imf_pcps_fetch_series(series_key, start_year, end_year)
    imf_pcps_prices_from_series(series_list, tech, commodity_code, commodity_label)
  })
  prices <- do.call(rbind, price_tbls)
  if (is.null(prices) || nrow(prices) == 0) {
    stop("IMF PCPS returned no price observations for requested commodities.")
  }

  vol <- imf_pcps_compute_volatility(prices)
  tech_vol <- vol$tech_vol
  series_vol <- vol$series_vol
  tech_vol$start_year <- start_year
  tech_vol$end_year <- end_year
  series_vol$start_year <- start_year
  series_vol$end_year <- end_year

  list(prices = prices, tech_vol = tech_vol, series_vol = series_vol)
}
