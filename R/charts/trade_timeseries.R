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

trade_normalize_years <- function(years) {
  if (length(years) == 2 && is.numeric(years)) {
    return(seq.int(min(years), max(years)))
  }
  sort(unique(as.integer(years)))
}


trade_chunk_years <- function(years, year_chunk_size = 12L) {
  years <- sort(unique(as.integer(years)))
  years <- years[!is.na(years)]

  if (length(years) == 0) {
    stop("years must include at least one valid year.")
  }

  if (is.null(year_chunk_size) || is.na(year_chunk_size) || year_chunk_size <= 0) {
    year_chunk_size <- 12L
  }

  year_chunk_size <- min(as.integer(year_chunk_size), 12L)

  latest_year <- max(years)
  base_years <- years[years < latest_year]

  chunk_starts <- integer()
  chunk_ends <- integer()

  if (length(base_years) > 0) {
    chunk_starts <- base_years[seq(1, length(base_years), by = year_chunk_size)]
    chunk_ends <- vapply(seq_along(chunk_starts), function(i) {
      base_years[min(i * year_chunk_size, length(base_years))]
    }, integer(1))
  }

  data.frame(
    ys = c(chunk_starts, latest_year),
    ye = c(chunk_ends, latest_year)
  )
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

  partner_chunks <- trade_split_vec(as.character(partner), chunk_size = partner_chunk_size)
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

  dplyr::bind_rows(request_blocks)
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
