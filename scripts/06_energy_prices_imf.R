# IMF Primary Commodity Price System (PCPS) energy price volatility helper.

imf_pcps_base_url <- "https://dataservices.imf.org/REST/SDMX_JSON.svc"

imf_pcps_fetch_json <- function(endpoint) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required to ingest IMF PCPS data.")
  }
  jsonlite::fromJSON(paste0(imf_pcps_base_url, "/", endpoint))
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
  dsd <- imf_pcps_fetch_json("DataStructure/IMF/PCPS")
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
  data <- imf_pcps_fetch_json(endpoint)
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
  commodity_df <- imf_pcps_get_commodity_codes()
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

  dsd <- imf_pcps_fetch_json("DataStructure/IMF/PCPS")
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
