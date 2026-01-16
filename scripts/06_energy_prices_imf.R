# IMF Primary Commodity Price System (PCPS) energy price volatility helper.
# Uses the local IMF_PCPS_all.xlsx snapshot instead of live API calls.

## Configuration helpers ----
imf_pcps_soft_fail_enabled <- function() {
  isTRUE(getOption("opportunity_security.imf_pcps_soft_fail", FALSE))
}

imf_pcps_resolve_repo_root <- function() {
  if (requireNamespace("rprojroot", quietly = TRUE)) {
    return(rprojroot::find_root(rprojroot::is_git_root))
  }
  start_dir <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  d <- start_dir
  while (!file.exists(file.path(d, ".git")) && dirname(d) != d) {
    d <- dirname(d)
  }
  if (!file.exists(file.path(d, ".git"))) {
    return(start_dir)
  }
  d
}

imf_pcps_resolve_snapshot_dir <- function() {
  snapshot_dir <- getOption("opportunity_security.raw_snapshot_dir")
  if (!is.null(snapshot_dir) && nzchar(snapshot_dir)) {
    return(snapshot_dir)
  }
  config <- getOption("opportunity_security.config")
  if (!is.null(config) && !is.null(config$raw_data_dir)) {
    repo_root <- imf_pcps_resolve_repo_root()
    raw_base_dir <- file.path(repo_root, config$raw_data_dir)
    snapshot_dirs <- list.dirs(raw_base_dir, recursive = FALSE, full.names = TRUE)
    if (length(snapshot_dirs) > 0) {
      snapshot_info <- file.info(snapshot_dirs)
      return(snapshot_dirs[order(snapshot_info$mtime, decreasing = TRUE)][1])
    }
  }
  NULL
}

imf_pcps_excel_path <- function() {
  snapshot_dir <- imf_pcps_resolve_snapshot_dir()
  if (is.null(snapshot_dir) || !nzchar(snapshot_dir)) {
    return(NULL)
  }
  file.path(snapshot_dir, "IMF_PCPS_all.xlsx")
}

## Empty result helpers ----
imf_pcps_empty_prices <- function() {
  data.frame(
    date = as.Date(character()),
    value = numeric(),
    tech = character(),
    commodity_code = character(),
    commodity_label = character(),
    stringsAsFactors = FALSE
  )
}

imf_pcps_empty_tech_vol <- function() {
  data.frame(
    tech = character(),
    volatility_10y = numeric(),
    volatility_index = numeric(),
    start_year = integer(),
    end_year = integer(),
    stringsAsFactors = FALSE
  )
}

imf_pcps_empty_series_vol <- function() {
  data.frame(
    tech = character(),
    commodity_code = character(),
    commodity_label = character(),
    volatility_10y = numeric(),
    start_year = integer(),
    end_year = integer(),
    stringsAsFactors = FALSE
  )
}

## Excel ingestion helpers ----
imf_pcps_find_column <- function(df, candidates) {
  col_names <- names(df)
  normalize_name <- function(x) {
    cleaned <- gsub("[^a-z0-9]+", "_", tolower(x))
    gsub("^_+|_+$", "", cleaned)
  }
  normalized_names <- normalize_name(col_names)
  normalized_candidates <- normalize_name(candidates)
  for (candidate in normalized_candidates) {
    idx <- match(candidate, normalized_names)
    if (!is.na(idx)) {
      return(col_names[[idx]])
    }
    partial_idx <- which(grepl(candidate, normalized_names, fixed = TRUE))
    if (length(partial_idx) > 0) {
      return(col_names[[partial_idx[1]]])
    }
  }
  NULL
}

imf_pcps_date_columns <- function(col_names, monthly_only = TRUE) {
  normalized <- gsub("^X", "", col_names)
  normalized_lower <- tolower(normalized)
  pattern <- if (monthly_only) {
    "^\\d{4}[-_]?m\\d{1,2}$"
  } else {
    "^\\d{4}([_-]?m\\d{1,2}|[_-]?q\\d{1}|[_-]?\\d{1,2})?$"
  }
  idx <- grepl(pattern, normalized_lower)
  list(original = col_names[idx], normalized = normalized[idx])
}

imf_pcps_normalize_date_label <- function(label) {
  out <- gsub("^X", "", label)
  out <- gsub("_", "-", out)
  out <- sub("^(\\d{4})M(\\d{1,2})$", "\\1-\\2", out, ignore.case = TRUE)
  out
}

imf_pcps_extract_year <- function(date_vals) {
  if (inherits(date_vals, "Date")) {
    return(as.integer(format(date_vals, "%Y")))
  }
  date_str <- as.character(date_vals)
  year_str <- sub("^(\\d{4}).*", "\\1", date_str)
  suppressWarnings(as.integer(year_str))
}

imf_pcps_select_sheet <- function(path) {
  sheets <- readxl::excel_sheets(path)
  best <- NULL
  best_sheet <- NULL
  best_score <- -Inf
  for (sheet in sheets) {
    df <- readxl::read_excel(
      path,
      sheet = sheet,
      n_max = 200,
      col_types = "text",
      guess_max = 50000
    )
    if (nrow(df) == 0) {
      next
    }
    time_col <- imf_pcps_find_column(df, c("time_period", "time", "date", "period"))
    value_col <- imf_pcps_find_column(df, c("obs_value", "value", "price", "index"))
    date_cols <- imf_pcps_date_columns(names(df), monthly_only = TRUE)
    score <- length(date_cols$original)
    numeric_like_count <- 0
    if (length(date_cols$original) > 0) {
      month_values <- unlist(df[date_cols$original], use.names = FALSE)
      month_values <- trimws(as.character(month_values))
      month_values <- month_values[nzchar(month_values)]
      if (length(month_values) > 0) {
        numeric_like <- suppressWarnings(as.numeric(gsub(",", "", month_values)))
        numeric_like_count <- sum(!is.na(numeric_like))
      }
    }
    score <- score + numeric_like_count
    if (!is.null(time_col) && !is.null(value_col)) {
      score <- score + 100
    }
    if (score > best_score) {
      best_score <- score
      best_sheet <- sheet
    }
  }
  if (is.null(best_sheet) || best_score <= 0) {
    stop("Unable to locate IMF PCPS data in IMF_PCPS_all.xlsx.")
  }
  best <- readxl::read_excel(
    path,
    sheet = best_sheet,
    col_types = "text",
    guess_max = 50000
  )
  list(sheet = best_sheet, data = best)
}

imf_pcps_long_from_excel <- function(raw_df) {
  parse_numeric <- function(x) {
    s <- trimws(as.character(x))
    s[s == ""] <- NA_character_
    
    # common "no data" tokens
    s[grepl("^(na|n/a|nd|\\.\\.|\\.|-|-|-)$", tolower(s))] <- NA_character_
    
    # normalize unicode dashes/minus
    s <- gsub("[\u2212\u2013\u2014]", "-", s)
    
    # remove thousands separators and spaces
    s <- gsub(",", "", s, fixed = TRUE)
    s <- gsub("\\s+", "", s)
    
    # strip trailing footnotes/suffixes but keep leading numeric portion
    # e.g. "123.4*" -> "123.4", "-56.7(est)" -> "-56.7"
    s <- sub("^(-?[0-9]*\\.?[0-9]+).*$", "\\1", s)
    
    suppressWarnings(as.numeric(s))
  }
  
  select_metadata_column <- function(df, columns, prefer_pattern = NULL) {
    if (length(columns) == 0) {
      return(NULL)
    }
    column_stats <- lapply(columns, function(col) {
      values <- trimws(as.character(df[[col]]))
      values <- values[nzchar(values)]
      numeric_vals <- suppressWarnings(as.numeric(gsub(",", "", values)))
      numeric_share <- if (length(values) == 0) {
        1
      } else {
        mean(!is.na(numeric_vals))
      }
      list(
        col = col,
        distinct = length(unique(values)),
        numeric_share = numeric_share
      )
    })
    stats_df <- data.frame(
      col = vapply(column_stats, function(x) x$col, character(1)),
      distinct = vapply(column_stats, function(x) x$distinct, integer(1)),
      numeric_share = vapply(column_stats, function(x) x$numeric_share, numeric(1)),
      stringsAsFactors = FALSE
    )
    candidates <- stats_df$col[stats_df$numeric_share < 0.5]
    if (!is.null(prefer_pattern)) {
      preferred <- candidates[grepl(prefer_pattern, candidates, ignore.case = TRUE)]
      if (length(preferred) > 0) {
        candidates <- preferred
      }
    }
    if (length(candidates) == 0) {
      candidates <- stats_df$col
    }
    stats_df <- stats_df[match(candidates, stats_df$col), , drop = FALSE]
    stats_df <- stats_df[order(-stats_df$distinct), , drop = FALSE]
    stats_df$col[1]
  }

  time_col <- imf_pcps_find_column(raw_df, c("time_period", "time", "date", "period"))
  value_col <- imf_pcps_find_column(raw_df, c("obs_value", "value", "price", "index"))
  code_col <- imf_pcps_find_column(raw_df, c("commodity_code", "commodity", "item", "product", "series", "code"))
  label_col <- imf_pcps_find_column(raw_df, c("commodity_label", "commodity_name", "commodity_description", "description", "label", "name"))

  if (!is.null(time_col) && !is.null(value_col)) {
    if (is.null(code_col)) {
      stop("Unable to identify commodity column in IMF_PCPS_all.xlsx.")
    }
    if (is.null(label_col)) {
      label_col <- code_col
    }
    return(data.frame(
      date = as.character(raw_df[[time_col]]),
      value = parse_numeric(raw_df[[value_col]]),
      commodity_code = as.character(raw_df[[code_col]]),
      commodity_label = as.character(raw_df[[label_col]]),
      stringsAsFactors = FALSE
    ))
  }

  date_all <- imf_pcps_date_columns(names(raw_df), monthly_only = FALSE)
  date_info <- imf_pcps_date_columns(names(raw_df), monthly_only = TRUE)
  if (length(date_info$original) == 0) {
    stop("Unable to locate time columns in IMF_PCPS_all.xlsx.")
  }
  metadata_cols <- setdiff(names(raw_df), date_all$original)
  if (is.null(code_col)) {
    code_col <- select_metadata_column(raw_df, metadata_cols)
  }
  if (is.null(code_col)) {
    stop("Unable to identify commodity column in IMF_PCPS_all.xlsx.")
  }
  if (is.null(label_col)) {
    label_col <- select_metadata_column(
      raw_df,
      metadata_cols,
      prefer_pattern = "name|label|description|commodity|item|product"
    )
    if (is.null(label_col)) {
      label_col <- code_col
    }
  }

  stacked <- stack(raw_df[date_info$original])
  meta_keep <- unique(c(code_col, label_col))
  metadata_rep <- raw_df[meta_keep][rep(seq_len(nrow(raw_df)), times = length(date_info$original)), , drop = FALSE]
  normalized_dates <- imf_pcps_normalize_date_label(date_info$normalized)
  date_labels <- normalized_dates[match(stacked$ind, date_info$original)]

  data.frame(
    date = as.character(date_labels),
    value = parse_numeric(stacked$values),
    commodity_code = as.character(metadata_rep[[code_col]]),
    commodity_label = as.character(metadata_rep[[label_col]]),
    stringsAsFactors = FALSE
  )
}

imf_pcps_data_env <- new.env(parent = emptyenv())

imf_pcps_load_pcps_data <- function() {
  if (exists("data", envir = imf_pcps_data_env, inherits = FALSE)) {
    return(get("data", envir = imf_pcps_data_env))
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required to ingest IMF PCPS data.")
  }
  excel_path <- imf_pcps_excel_path()
  if (is.null(excel_path) || !file.exists(excel_path)) {
    if (imf_pcps_soft_fail_enabled()) {
      warning("IMF PCPS Excel snapshot not found; returning empty result.", call. = FALSE)
      return(NULL)
    }
    stop("IMF PCPS Excel snapshot not found: ", excel_path)
  }
  selection <- imf_pcps_select_sheet(excel_path)
  raw_df <- selection$data
  data <- imf_pcps_long_from_excel(raw_df)
  data_before <- data
  data <- data[!is.na(data$value), , drop = FALSE]
  if (nrow(data) == 0) {
    monthly_cols <- imf_pcps_date_columns(names(raw_df), monthly_only = TRUE)
    stop(
      "IMF PCPS ingestion yielded no usable values. ",
      "Sheet: ", selection$sheet, ". ",
      "Columns: ", paste(utils::head(names(raw_df), 20), collapse = ", "), ". ",
      "Monthly columns detected: ", length(monthly_cols$original), ". ",
      "Example stacked values: ", paste(utils::head(data_before$value, 10), collapse = ", ")
    )
  }
  assign("data", data, envir = imf_pcps_data_env)
  data
}

imf_pcps_get_commodity_codes <- function() {
  data <- imf_pcps_load_pcps_data()
  if (is.null(data)) {
    warning("IMF PCPS data unavailable; returning empty commodity list.", call. = FALSE)
    return(data.frame(code = character(), label = character(), stringsAsFactors = FALSE))
  }
  commodity_df <- unique(data[, c("commodity_code", "commodity_label")])
  names(commodity_df) <- c("code", "label")
  commodity_df
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

imf_pcps_select_indicators <- function(commodity_df, indicators) {
  if (nrow(commodity_df) == 0 || length(indicators) == 0) {
    return(data.frame(tech = character(), commodity_code = character(), commodity_label = character()))
  }
  normalized_labels <- tolower(trimws(commodity_df$label))
  normalized_targets <- tolower(trimws(indicators))
  idx <- normalized_labels %in% normalized_targets
  if (!any(idx)) {
    return(data.frame(tech = character(), commodity_code = character(), commodity_label = character()))
  }
  data.frame(
    tech = commodity_df$label[idx],
    commodity_code = commodity_df$code[idx],
    commodity_label = commodity_df$label[idx],
    stringsAsFactors = FALSE
  )
}

imf_pcps_prices_from_data <- function(data, commodity_map) {
  if (nrow(data) == 0 || nrow(commodity_map) == 0) {
    return(data.frame())
  }
  merged <- merge(
    data,
    commodity_map,
    by = c("commodity_code", "commodity_label"),
    all.x = TRUE
  )
  merged <- merged[!is.na(merged$tech), , drop = FALSE]
  merged[, c("date", "value", "tech", "commodity_code", "commodity_label"), drop = FALSE]
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
  if (nrow(commodity_df) == 0 && imf_pcps_soft_fail_enabled()) {
    warning("IMF PCPS commodity list empty; returning empty energy price results.", call. = FALSE)
    return(list(
      prices = imf_pcps_empty_prices(),
      tech_vol = imf_pcps_empty_tech_vol(),
      series_vol = imf_pcps_empty_series_vol()
    ))
  }
  patterns <- list(
    Aluminum = "aluminum.*(unit prices|us dollars|usd)",
    Oil_APSP = "apsp.*crude oil|crude oil.*apsp",
    Oil_Brent = "brent.*crude|brent.*oil",
    Oil_WTI = "wti.*crude|wti.*oil",
    Chromium = "chromium",
    Coal = "coal",
    Cobalt = "cobalt",
    Copper = "copper",
    Lithium = "lithium",
    LNG = "lng|liquefied natural gas",
    Manganese = "manganese",
    Natural_Gas_Index = "natural gas index|commodity price index.*natural gas",
    Natural_Gas_EU = "natural gas.*eu",
    Natural_Gas_Henry_Hub = "henry hub|us henry hub",
    Nickel = "nickel",
    Rare_Earths = "rare earth",
    Silicon = "silicon",
    Uranium = "uranium",
    Zinc = "zinc"
  )
  commodity_map <- imf_pcps_match_commodities(commodity_df, patterns)
  if (is.null(commodity_map) || nrow(commodity_map) == 0) {
    sample_labels <- utils::head(commodity_df$label, 12)
    stop(
      "No IMF PCPS commodities matched requested patterns. ",
      "Check labels such as: ",
      paste(sample_labels, collapse = " | ")
    )
  }
  price_priority <- grepl("unit prices|us dollars|usd", tolower(commodity_map$commodity_label))
  commodity_map <- commodity_map[order(commodity_map$tech, !price_priority), , drop = FALSE]

  data <- imf_pcps_load_pcps_data()
  if (is.null(data)) {
    warning("IMF PCPS data unavailable; returning empty energy price results.", call. = FALSE)
    return(list(
      prices = imf_pcps_empty_prices(),
      tech_vol = imf_pcps_empty_tech_vol(),
      series_vol = imf_pcps_empty_series_vol()
    ))
  }

  data <- data[grepl("-M\\d{1,2}$", toupper(data$date)), , drop = FALSE]
  data_years <- imf_pcps_extract_year(data$date)
  if (!is.null(start_year) && !is.null(end_year)) {
    data <- data[data_years >= start_year & data_years <= end_year, , drop = FALSE]
  }

  prices <- imf_pcps_prices_from_data(data, commodity_map)
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
