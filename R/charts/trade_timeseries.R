# Trade timeseries pull helpers (UN Comtrade)
#
# This module reuses the request-grid/chunking approach from scripts/05_ingest_sources.R
# and exposes a focused API for pulling a country/tech/supply-chain timeseries.

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

trade_prepare_hs6_codes <- function(catalog,
                                    tech,
                                    supply_chain,
                                    hs_col = "HS6",
                                    tech_col = "tech",
                                    supply_chain_col = "supply_chain") {
  required_cols <- c(hs_col, tech_col, supply_chain_col)
  missing_cols <- setdiff(required_cols, names(catalog))
  if (length(missing_cols) > 0) {
    stop(
      "Catalog is missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }

  selected <- catalog[catalog[[tech_col]] == tech & catalog[[supply_chain_col]] == supply_chain, , drop = FALSE]

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

pull_trade_timeseries <- function(country = "Country",
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
                                  sleep_seconds = 0.5,
                                  retries = 3,
                                  ct_get_data_fn = comtradr::ct_get_data) {
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
  partner_chunks <- trade_split_vec(partner, chunk_size = partner_chunk_size)

  request_grid <- tidyr::expand_grid(
    rep = country,
    yr = years,
    dir = flows,
    cc = code_chunks,
    pch = partner_chunks
  )

  output <- vector("list", nrow(request_grid))
  failed <- character()

  for (i in seq_len(nrow(request_grid))) {
    req <- request_grid[i, ]

    data_chunk <- NULL
    last_err <- NULL

    for (attempt in seq_len(retries)) {
      attempt_out <- tryCatch(
        ct_get_data_fn(
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

    if (!"flow_direction" %in% names(data_chunk)) {
      data_chunk <- dplyr::mutate(data_chunk, flow_direction = req$dir[[1]])
    }
    if ("trade_flow" %in% names(data_chunk)) {
      data_chunk <- dplyr::filter(data_chunk, tolower(trade_flow) == req$dir[[1]])
    }

    output[[i]] <- dplyr::mutate(
      data_chunk,
      country_req = req$rep[[1]],
      year_req = req$yr[[1]],
      tech_req = tech,
      supply_chain_req = supply_chain
    )

    Sys.sleep(sleep_seconds)
  }

  if (length(failed) > 0) {
    warning(
      "Some Comtrade requests failed (showing up to 5):\n",
      paste(utils::head(failed, 5), collapse = "\n")
    )
  }

  dplyr::bind_rows(output) %>% dplyr::distinct()
}

#US Battery Midstream
COMTRADE_API_KEY="2940653b9bbe4671b3f7fde2846d14be"

us_batt_mid <- pull_trade_timeseries(
  country = "USA",
  tech = "Batteries",
  supply_chain = "Midstream",
  partners = c("CHN,FRA,DEU,ITA,ESP,NLD,BEL,SWE,POL,DNK,FIN,CZE,ROU,HUN,AUT,PRT,GRC,IRL,JPN,KOR,IND,VNM"),
  years = "2021:2025",
  flow = "export"
)
