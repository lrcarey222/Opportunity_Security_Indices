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

  selected <- catalog[
    catalog[[tech_col]] == tech & catalog[[supply_chain_col]] == supply_chain,
    ,
    drop = FALSE
  ]

  if (nrow(selected) == 0) {
    stop(
      "No HS codes found for tech='", tech,
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
                                                partner_chunk_size = 50) {
  years <- trade_normalize_years(years)
  flows <- tolower(flow_direction)

  hs6_codes <- trade_prepare_hs6_codes(
    catalog = hs6_catalog,
    tech = tech,
    supply_chain = supply_chain,
    hs_col = hs_col,
    tech_col = tech_col,
    supply_chain_col = supply_chain_col
  )

  code_chunks <- trade_split_by_nchar(hs6_codes, max_chars = max_code_chars)
  partner_chunks <- trade_split_vec(as.character(partner), chunk_size = partner_chunk_size)

  tidyr::expand_grid(
    rep = as.character(country),
    yr = years,
    dir = flows,
    cc = code_chunks,
    pch = partner_chunks
  )
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
    year_req = req$yr[[1]],
    tech_req = tech,
    supply_chain_req = supply_chain
  )
}
