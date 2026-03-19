# Trade timeseries helpers (UN Comtrade request planning)
#
# Keep this file pure: it only prepares request inputs and transforms returned
# data chunks. API calls and file IO should happen in scripts/.

trade_split_by_nchar <- function(x, max_chars = 2500) {
  x <- unique(stats::na.omit(as.character(x)))
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

  if (length(cur) > 0) {
    chunks[[length(chunks) + 1]] <- cur
  }

  chunks
}

trade_split_vec <- function(x, chunk_size) {
  if (length(x) == 0) {
    return(list(character()))
  }
  split(x, ceiling(seq_along(x) / chunk_size))
}

trade_prepare_partner_chunks <- function(partner, partner_chunk_size = 50) {
  partners <- unique(stats::na.omit(as.character(partner)))
  partners <- trimws(partners)
  partners <- partners[nzchar(partners)]

  if (length(partners) == 0) {
    return(list("World"))
  }

  world_idx <- tolower(partners) == "world"
  has_world <- any(world_idx)

  if (!has_world || length(partners) == 1) {
    return(trade_split_vec(partners, chunk_size = partner_chunk_size))
  }

  non_world <- partners[!world_idx]
  non_world_chunks <- trade_split_vec(non_world, chunk_size = partner_chunk_size)
  c(non_world_chunks, list("World"))
}

trade_normalize_years <- function(years) {
  year_values <- suppressWarnings(as.integer(years))
  year_values <- sort(unique(stats::na.omit(year_values)))

  if (length(year_values) == 0) {
    stop("years must include at least one valid year.")
  }

  year_values
}

trade_chunk_years <- function(years, year_chunk_size = 12L) {
  years <- trade_normalize_years(years)

  if (is.null(year_chunk_size) || is.na(year_chunk_size) || year_chunk_size <= 0) {
    stop("year_chunk_size must be a positive integer.")
  }

  chunk_size <- as.integer(year_chunk_size)
  start_idx <- seq(1L, length(years), by = chunk_size)
  end_idx <- pmin(start_idx + chunk_size - 1L, length(years))

  data.frame(
    ys = years[start_idx],
    ye = years[end_idx],
    stringsAsFactors = FALSE
  )
}

validate_trade_timeseries_requests <- function(request_df) {
  required_cols <- c(
    "reporter", "partner", "commodity_code", "start_date",
    "end_date", "flow_direction", "frequency"
  )
  missing_cols <- setdiff(required_cols, names(request_df))
  if (length(missing_cols) > 0) {
    stop("request_df is missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (nrow(request_df) == 0) {
    stop("request_df has no rows.")
  }

  start_year <- suppressWarnings(as.integer(request_df$start_date))
  end_year <- suppressWarnings(as.integer(request_df$end_date))
  frequency <- toupper(trimws(as.character(request_df$frequency)))

  if (any(is.na(start_year)) || any(is.na(end_year))) {
    stop("start_date and end_date must be valid years.")
  }

  bad_windows <- which(start_year > end_year)
  if (length(bad_windows) > 0) {
    stop("Invalid year windows found (start_date > end_date) at rows: ", paste(bad_windows, collapse = ", "))
  }

  bad_frequency <- which(!frequency %in% c("A", "M"))
  if (length(bad_frequency) > 0) {
    stop("frequency must be one of 'A' or 'M'. Invalid rows: ", paste(bad_frequency, collapse = ", "))
  }

  monthly_bad <- which(frequency == "M" & start_year != end_year)
  if (length(monthly_bad) > 0) {
    stop(
      "Monthly requests must have start_date == end_date. Invalid rows: ",
      paste(monthly_bad, collapse = ", ")
    )
  }

  check_non_empty <- function(values, label) {
    val <- trimws(as.character(values))
    bad <- which(is.na(val) | !nzchar(val))
    if (length(bad) > 0) {
      stop(label, " must be non-empty. Invalid rows: ", paste(bad, collapse = ", "))
    }
  }

  check_non_empty(request_df$reporter, "reporter")
  check_non_empty(request_df$partner, "partner")
  check_non_empty(request_df$commodity_code, "commodity_code")

  invisible(request_df)
}

trade_pick_column <- function(tbl, candidates, label) {
  hits <- candidates[candidates %in% names(tbl)]
  if (length(hits) == 0) {
    stop(
      "Catalog is missing ", label, " column. Tried: ",
      paste(candidates, collapse = ", ")
    )
  }
  hits[[1]]
}

trade_prepare_hs6_codes <- function(catalog,
                                    tech,
                                    supply_chain,
                                    hs_col = "HS6",
                                    tech_col = "tech",
                                    supply_chain_col = "supply_chain") {
  hs_col <- if (hs_col %in% names(catalog)) {
    hs_col
  } else {
    trade_pick_column(catalog, c("HS6", "hs6", "HS_6", "code_hs", "code"), "HS6")
  }
  tech_col <- if (tech_col %in% names(catalog)) {
    tech_col
  } else {
    trade_pick_column(catalog, c("tech", "Technology", "technology"), "tech")
  }
  supply_chain_col <- if (supply_chain_col %in% names(catalog)) {
    supply_chain_col
  } else {
    trade_pick_column(catalog, c("supply_chain", "Value.Chain", "value_chain", "Supply.Chain"), "supply_chain")
  }

  tech <- as.character(tech)
  tech <- trimws(tech)
  tech <- unique(tech[nzchar(tech)])
  if (length(tech) == 0) {
    stop("tech is required.")
  }

  selected <- catalog[
    catalog[[tech_col]] %in% tech & catalog[[supply_chain_col]] == supply_chain,
    ,
    drop = FALSE
  ]

  if (nrow(selected) == 0) {
    stop(
      "No HS codes found for tech='", paste(tech, collapse = ", "),
      "' and supply_chain='", supply_chain, "'."
    )
  }

  selected[[hs_col]] <- stringr::str_pad(
    stringr::str_replace_all(as.character(selected[[hs_col]]), "\\D", ""),
    width = 6,
    side = "left",
    pad = "0"
  )

  unique(stats::na.omit(selected[[hs_col]]))
}

build_trade_timeseries_request_grid <- function(country,
                                                tech,
                                                supply_chain,
                                                years,
                                                hs6_catalog,
                                                partner = "World",
                                                flow_direction = c("export", "import"),
                                                hs_col = "HS6",
                                                tech_col = "tech",
                                                supply_chain_col = "supply_chain",
                                                max_code_chars = 2500,
                                                partner_chunk_size = 50,
                                                year_chunk_size = 12L) {
  years <- trade_normalize_years(years)
  flows <- tolower(flow_direction)

  tech <- as.character(tech)
  tech <- trimws(tech)
  tech <- unique(tech[nzchar(tech)])
  if (length(tech) == 0) {
    stop("tech is required.")
  }

  partner_chunks <- trade_prepare_partner_chunks(partner, partner_chunk_size = partner_chunk_size)
  year_windows <- trade_chunk_years(years, year_chunk_size = year_chunk_size)

  request_blocks <- lapply(tech, function(tech_item) {
    hs6_codes <- trade_prepare_hs6_codes(
      catalog = hs6_catalog,
      tech = tech_item,
      supply_chain = supply_chain,
      hs_col = hs_col,
      tech_col = tech_col,
      supply_chain_col = supply_chain_col
    )

    code_chunks <- trade_split_by_nchar(hs6_codes, max_chars = max_code_chars)

    tidyr::expand_grid(
      rep = as.character(country),
      ys = year_windows$ys,
      ye = year_windows$ye,
      dir = flows,
      tech = tech_item,
      cc = code_chunks,
      pch = partner_chunks
    )
  })

  request_grid <- dplyr::bind_rows(request_blocks)
  if (nrow(request_grid) > 0 && any(request_grid$ys > request_grid$ye)) {
    stop("Invalid request grid generated: found ys > ye.")
  }

  request_grid
}

trade_tag_response_chunk <- function(data_chunk, req, tech, supply_chain) {
  if (!"flow_direction" %in% names(data_chunk)) {
    data_chunk <- dplyr::mutate(data_chunk, flow_direction = req$dir[[1]])
  }
  if ("trade_flow" %in% names(data_chunk)) {
    data_chunk <- dplyr::filter(data_chunk, tolower(trade_flow) == req$dir[[1]])
  }

  dplyr::mutate(
    data_chunk,
    country_req = req$rep[[1]],
    year_req = req$ys[[1]],
    year_req_end = req$ye[[1]],
    tech_req = if ("tech" %in% names(req)) req$tech[[1]] else tech,
    supply_chain_req = supply_chain
  )
}
