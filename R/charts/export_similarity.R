# Export similarity between major energy-technology exporters.
#
# Finger-Kreinin export similarity index over the HS6 energy-technology basket:
#
#   ESI(a, b) = 100 * sum_p min(share_ap, share_bp)
#
# where share_ap is product p's share of country a's exports across the basket.
# 100 means identical export structures, 0 means no overlap.
#
# The product basket is data/reference/energy_hs6_master.csv, the single source of
# truth for the HS6 crosswalk, so this measures similarity of *energy-technology*
# export structure rather than of total exports. An overall index and a per-technology
# breakdown are produced.
#
# Data is pulled at HS6 and aggregated to `level` before the index is computed. The
# index is only comparable within a level, so every output row records its level.
#
# Run:
#   Rscript R/charts/export_similarity.R
#
# Environment:
#   ESI_LEVEL       hs6 | hs4 | hs2 | tech | sub_sector   (default hs4)
#   ESI_START_YEAR  default 2012
#   ESI_END_YEAR    default last calendar year
#   ESI_REFRESH     true to bypass the cached Comtrade pull
#
# Requires COMTRADE_API_KEY unless a cached pull is already present.

source(local({
  resolve_bootstrap_path <- function() {
    candidate_starts <- character()

    sf <- tryCatch(sys.frame(1)$ofile, error = function(e) "")
    if (!is.null(sf) && nzchar(sf)) candidate_starts <- c(candidate_starts, dirname(sf))

    frame_ofiles <- vapply(sys.frames(), function(fr) {
      val <- tryCatch(fr$ofile, error = function(e) "")
      if (is.null(val) || !nzchar(val)) "" else dirname(val)
    }, character(1))
    candidate_starts <- c(candidate_starts, frame_ofiles[nzchar(frame_ofiles)])

    fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(fa) > 0) candidate_starts <- c(candidate_starts, dirname(sub("^--file=", "", fa[1])))

    candidate_starts <- unique(c(candidate_starts, getwd()))

    for (start in candidate_starts) {
      d <- normalizePath(start, winslash = "/", mustWork = FALSE)
      while (dirname(d) != d) {
        bootstrap <- file.path(d, "scripts", "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)

        bootstrap <- file.path(d, "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)

        d <- dirname(d)
      }
    }

    stop("Unable to resolve script path for bootstrap.")
  }

  resolve_bootstrap_path()
}))

source(file.path(repo_root, "R", "utils", "hs6_crosswalk.R"))
source(file.path(repo_root, "scripts", "utils", "raw_inputs.R"))
source(file.path(repo_root, "scripts", "utils", "comtrade_ingest_utils.R"))
source(file.path(repo_root, "scripts", "utils", "comtrade_client.R"))

## Configuration -------------------------------------------------------------

EXPORT_SIMILARITY_COUNTRIES <- c(
  CHN = "China", DEU = "Germany", FRA = "France",
  JPN = "Japan", KOR = "Korea, Rep.", USA = "United States"
)

# HS6 codes are revision-specific. The master uses HS2022 codes (854142/854143 for
# solar cells and modules, which replaced the pre-2022 854140), so early years lose
# any code introduced by a later revision. products_compared and coverage_pct on each
# output row make that visible rather than silent.
#
# HS4 headings are considerably more stable across HS revisions than HS6 subheadings,
# so `level = "hs4"` can usually be run back further than HS6 can. Widen with
# ESI_START_YEAR and check coverage_pct before trusting the early years.
EXPORT_SIMILARITY_DEFAULT_START <- 2012L

# Product granularity the index is computed at.
#
# Finger-Kreinin rises mechanically with aggregation, because coarser buckets hide
# within-bucket composition differences. Measured on 2024 data for the six countries,
# mean ESI across the 15 pairs was: hs6 44.4, hs4 49.1, sub_sector 59.0, hs2 58.9,
# tech 70.7. Values are therefore only comparable within a level, which is why every
# output row records the level it was computed at.
#
# Pair *rankings* are more robust: Spearman against hs6 was 0.943 (hs4), 0.968
# (sub_sector), 0.882 (tech), 0.779 (hs2). hs2 is too coarse - it moved France-Korea
# from 9th to 2nd - so hs4 is the default: near-hs6 rankings, better revision
# stability, less product-level noise.
EXPORT_SIMILARITY_LEVELS <- c("hs6", "hs4", "hs2", "tech", "sub_sector")
EXPORT_SIMILARITY_DEFAULT_LEVEL <- "hs4"

## Pure computation ----------------------------------------------------------
# These take long-format trade data and are classification-agnostic, so the same code
# reproduces the historical SITC3 index as well as the HS6 one.

# trade_long: data.frame(year, iso, code, value)
export_similarity_pairs <- function(isos) {
  combos <- utils::combn(sort(unique(isos)), 2, simplify = FALSE)
  data.frame(
    country_a = vapply(combos, `[`, character(1), 1),
    country_b = vapply(combos, `[`, character(1), 2),
    stringsAsFactors = FALSE
  )
}

# Finger-Kreinin overlap for one pair-year. Products absent for a country count as
# zero exports, which is the standard treatment.
#
# min_products guards against a degenerate result: with a single product both shares
# are 1 and the index is trivially 100, which reads as "identical structures" when it
# actually means "no structure to compare". Such cells return NA.
EXPORT_SIMILARITY_MIN_PRODUCTS <- 5L

export_similarity_one <- function(trade_long, year, a, b,
                                  min_products = EXPORT_SIMILARITY_MIN_PRODUCTS) {
  x <- trade_long[trade_long$year == year & trade_long$iso %in% c(a, b), , drop = FALSE]
  if (nrow(x) == 0) {
    return(list(esi = NA_real_, products_compared = 0L))
  }

  totals <- stats::aggregate(value ~ iso + code, data = x, FUN = sum)
  va <- stats::setNames(totals$value[totals$iso == a], totals$code[totals$iso == a])
  vb <- stats::setNames(totals$value[totals$iso == b], totals$code[totals$iso == b])

  codes <- union(names(va), names(vb))
  if (length(codes) == 0) return(list(esi = NA_real_, products_compared = 0L))

  sa <- ifelse(is.na(va[codes]), 0, va[codes])
  sb <- ifelse(is.na(vb[codes]), 0, vb[codes])
  if (sum(sa) <= 0 || sum(sb) <= 0) {
    return(list(esi = NA_real_, products_compared = length(codes)))
  }

  # Too thin to describe an export structure; report the count, withhold the index.
  if (length(codes) < min_products) {
    return(list(esi = NA_real_, products_compared = length(codes)))
  }

  list(
    esi = 100 * sum(pmin(sa / sum(sa), sb / sum(sb))),
    products_compared = length(codes)
  )
}

# Sector decomposition of one pair-year's index.
#
# ESI = 100 * sum_p min(share_ap, share_bp) is a sum over products, so grouping the
# per-product terms gives sector contributions that add back to the total exactly.
# Shares stay defined over the whole basket - they are NOT renormalised within a
# sector - which is what makes the parts sum to the whole and lets a change in the
# total be attributed across sectors.
export_similarity_contributions <- function(trade_long, year, a, b, sector_of) {
  x <- trade_long[trade_long$year == year & trade_long$iso %in% c(a, b), , drop = FALSE]
  if (nrow(x) == 0) return(NULL)

  totals <- stats::aggregate(value ~ iso + code, data = x, FUN = sum)
  va <- stats::setNames(totals$value[totals$iso == a], totals$code[totals$iso == a])
  vb <- stats::setNames(totals$value[totals$iso == b], totals$code[totals$iso == b])

  codes <- union(names(va), names(vb))
  if (length(codes) == 0) return(NULL)

  sa <- ifelse(is.na(va[codes]), 0, va[codes])
  sb <- ifelse(is.na(vb[codes]), 0, vb[codes])
  if (sum(sa) <= 0 || sum(sb) <= 0) return(NULL)

  overlap <- 100 * pmin(sa / sum(sa), sb / sum(sb))
  sector <- sector_of[codes]
  sector[is.na(sector)] <- "Unclassified"

  agg <- stats::aggregate(
    list(contribution = as.numeric(overlap)),
    by = list(sector = as.character(sector)), FUN = sum
  )
  agg$year <- year
  agg$country_a <- a
  agg$country_b <- b
  agg[, c("year", "country_a", "country_b", "sector", "contribution")]
}

# Full pair x year panel. basket_size, when supplied, is the number of codes the
# basket defines, so coverage_pct shows how much of it the data actually reached.
# Re-key HS6 trade rows to a coarser product level. Values are not summed here;
# export_similarity_compute() aggregates by (iso, code) already.
#
# tech and sub_sector come from the master and are many-to-many: an HS6 code mapped to
# two technologies contributes to both, matching how the per-technology panels treat it.
export_similarity_aggregate <- function(trade_long,
                                        level = EXPORT_SIMILARITY_DEFAULT_LEVEL,
                                        master_long = NULL) {
  level <- match.arg(level, EXPORT_SIMILARITY_LEVELS)
  if (nrow(trade_long) == 0) return(trade_long)

  if (level == "hs6") return(trade_long)

  if (level %in% c("hs4", "hs2")) {
    width <- if (level == "hs4") 4L else 2L
    trade_long$code <- substr(trade_long$code, 1, width)
    return(trade_long)
  }

  if (is.null(master_long)) {
    stop("level '", level, "' needs master_long to map HS6 codes to ", level, ".")
  }

  map <- unique(master_long[, c("hs6", level)])
  names(map) <- c("code", "grouped")
  merged <- merge(trade_long, map, by = "code")
  if (nrow(merged) == 0) {
    stop("no HS6 codes in the trade data could be mapped to ", level, ".")
  }

  data.frame(
    year = merged$year, iso = merged$iso,
    code = as.character(merged$grouped), value = merged$value,
    stringsAsFactors = FALSE
  )
}

# Number of distinct products the basket defines at a given level, for coverage_pct.
export_similarity_basket_size <- function(master_long, level, codes = NULL) {
  level <- match.arg(level, EXPORT_SIMILARITY_LEVELS)
  keep <- if (is.null(codes)) master_long else master_long[master_long$hs6 %in% codes, , drop = FALSE]

  switch(
    level,
    hs6 = length(unique(keep$hs6)),
    hs4 = length(unique(substr(keep$hs6, 1, 4))),
    hs2 = length(unique(substr(keep$hs6, 1, 2))),
    tech = length(unique(keep$tech)),
    sub_sector = length(unique(keep$sub_sector))
  )
}

export_similarity_compute <- function(trade_long,
                                      basket_size = NA_integer_,
                                      group = NA_character_,
                                      level = NA_character_,
                                      min_products = EXPORT_SIMILARITY_MIN_PRODUCTS) {
  stopifnot(all(c("year", "iso", "code", "value") %in% names(trade_long)))

  trade_long <- trade_long[!is.na(trade_long$value) & trade_long$value > 0, , drop = FALSE]
  if (nrow(trade_long) == 0) return(export_similarity_empty())

  pairs <- export_similarity_pairs(trade_long$iso)
  years <- sort(unique(trade_long$year))

  rows <- list()
  for (y in years) {
    for (i in seq_len(nrow(pairs))) {
      a <- pairs$country_a[i]; b <- pairs$country_b[i]
      res <- export_similarity_one(trade_long, y, a, b, min_products = min_products)
      rows[[length(rows) + 1L]] <- data.frame(
        year = y,
        group = group,
        level = level,
        country_a = a,
        country_a_name = unname(EXPORT_SIMILARITY_COUNTRIES[a]),
        country_b = b,
        country_b_name = unname(EXPORT_SIMILARITY_COUNTRIES[b]),
        pair = paste(
          unname(EXPORT_SIMILARITY_COUNTRIES[a]), "-", unname(EXPORT_SIMILARITY_COUNTRIES[b])
        ),
        esi = res$esi,
        products_compared = res$products_compared,
        coverage_pct = if (is.na(basket_size) || basket_size == 0) {
          NA_real_
        } else {
          100 * res$products_compared / basket_size
        },
        stringsAsFactors = FALSE
      )
    }
  }

  out <- do.call(rbind, rows)
  out[order(out$year, out$country_a, out$country_b), , drop = FALSE]
}

export_similarity_empty <- function() {
  data.frame(
    year = integer(), group = character(), level = character(),
    country_a = character(), country_a_name = character(),
    country_b = character(), country_b_name = character(),
    pair = character(), esi = numeric(),
    products_compared = integer(), coverage_pct = numeric(),
    stringsAsFactors = FALSE
  )
}

# Overall index plus one panel per technology in the master, all at `level`.
#
# trade_long is expected at HS6; aggregation to `level` happens here so the
# per-technology subsets are taken on HS6 codes before being re-keyed.
export_similarity_by_tech <- function(trade_long,
                                      master_long,
                                      level = EXPORT_SIMILARITY_DEFAULT_LEVEL) {
  level <- match.arg(level, EXPORT_SIMILARITY_LEVELS)

  overall <- export_similarity_compute(
    export_similarity_aggregate(trade_long, level, master_long),
    basket_size = export_similarity_basket_size(master_long, level),
    group = "All technologies",
    level = level
  )

  # Keying a single-technology panel by technology leaves one product, which the
  # thin-basket guard would suppress anyway. Drop a level for those panels.
  tech_level <- if (level == "tech") "sub_sector" else level

  techs <- sort(unique(master_long$tech))
  per_tech <- lapply(techs, function(t) {
    codes <- unique(master_long$hs6[master_long$tech == t])
    sub <- trade_long[trade_long$code %in% codes, , drop = FALSE]
    if (nrow(sub) == 0) return(NULL)

    export_similarity_compute(
      export_similarity_aggregate(sub, tech_level, master_long),
      basket_size = export_similarity_basket_size(master_long, tech_level, codes = codes),
      group = t,
      level = tech_level
    )
  })

  do.call(rbind, c(list(overall), Filter(Negate(is.null), per_tech)))
}

## Data access ---------------------------------------------------------------

export_similarity_cache_path <- function(repo_root, start_year, end_year) {
  file.path(
    repo_root, "data", "raw",
    sprintf("comtrade_hs6_exports_%d_%d.rds", start_year, end_year)
  )
}

export_similarity_build_requests <- function(reporters, commodity_codes, years, frequency = "A") {
  tidyr::expand_grid(
    reporter = reporters,
    year = years,
    commodity_code = commodity_codes
  ) %>%
    dplyr::mutate(
      request_id = dplyr::row_number(),
      partner = "World",
      start_date = year,
      end_date = year,
      flow_direction = "export",
      frequency = frequency
    ) %>%
    dplyr::select(
      request_id, reporter, partner, commodity_code,
      start_date, end_date, flow_direction, frequency
    )
}

export_similarity_fetch <- function(repo_root,
                                    codes,
                                    start_year,
                                    end_year,
                                    refresh = FALSE) {
  cache <- export_similarity_cache_path(repo_root, start_year, end_year)
  if (file.exists(cache) && !refresh) {
    message("Using cached Comtrade pull: ", basename(cache))
    return(readRDS(cache))
  }

  comtrade_set_key_from_env()

  code_chunks <- split_by_nchar(codes, max_chars = 2500)
  requests <- export_similarity_build_requests(
    reporters = names(EXPORT_SIMILARITY_COUNTRIES),
    commodity_codes = code_chunks,
    years = seq.int(from = start_year, to = end_year)
  )

  message("Fetching ", nrow(requests), " Comtrade requests (",
          length(codes), " HS6 codes, ", start_year, "-", end_year, ")")

  out <- comtrade_fetch_requests(
    requests,
    retries = as.integer(Sys.getenv("COMTRADE_MAX_RETRIES", "3")),
    sleep_seconds = 0.5,
    timeout_seconds = as.numeric(Sys.getenv("COMTRADE_REQUEST_TIMEOUT_SECONDS", "120")),
    show_progress = TRUE
  )

  if (is.null(out$data) || nrow(out$data) == 0) {
    stop("Comtrade returned no data for the requested basket and years.")
  }

  if (!dir.exists(dirname(cache))) dir.create(dirname(cache), recursive = TRUE)
  saveRDS(out$data, cache)
  message("Cached raw pull to ", basename(cache))
  out$data
}

# Raw comtradr output -> the long shape the computation expects.
export_similarity_tidy <- function(raw, codes) {
  year_col <- if ("ref_year" %in% names(raw)) "ref_year" else "period"
  value_col <- if ("primary_value" %in% names(raw)) "primary_value" else "fobvalue"

  out <- data.frame(
    year = suppressWarnings(as.integer(raw[[year_col]])),
    iso = as.character(raw$reporter_iso),
    code = hs6_clean_code(raw$cmd_code),
    value = suppressWarnings(as.numeric(raw[[value_col]])),
    stringsAsFactors = FALSE
  )

  out <- out[!is.na(out$year) & !is.na(out$code) & !is.na(out$value), , drop = FALSE]
  out <- out[out$iso %in% names(EXPORT_SIMILARITY_COUNTRIES), , drop = FALSE]
  # Keep only the basket; a wildcard pull can return parent aggregates too.
  out[out$code %in% codes, , drop = FALSE]
}

## Runner --------------------------------------------------------------------

run_export_similarity <- function(repo_root,
                                  start_year = EXPORT_SIMILARITY_DEFAULT_START,
                                  end_year = as.integer(format(Sys.Date(), "%Y")) - 1L,
                                  level = EXPORT_SIMILARITY_DEFAULT_LEVEL,
                                  refresh = FALSE,
                                  write_output = TRUE) {
  level <- match.arg(level, EXPORT_SIMILARITY_LEVELS)

  master_path <- file.path(raw_inputs_reference_dir(repo_root), "energy_hs6_master.csv")
  master <- utils::read.csv(master_path, check.names = FALSE, stringsAsFactors = FALSE)
  master_long <- hs6_normalize_master(master)
  codes <- sort(unique(master_long$hs6))

  message("HS6 basket: ", length(codes), " codes across ",
          length(unique(master_long$tech)), " technologies")
  message("Computing at level '", level, "' (",
          export_similarity_basket_size(master_long, level), " products)")

  raw <- export_similarity_fetch(repo_root, codes, start_year, end_year, refresh = refresh)
  trade_long <- export_similarity_tidy(raw, codes)

  missing_isos <- setdiff(names(EXPORT_SIMILARITY_COUNTRIES), unique(trade_long$iso))
  if (length(missing_isos) > 0) {
    warning("No export data returned for: ", paste(missing_isos, collapse = ", "), call. = FALSE)
  }

  result <- export_similarity_by_tech(trade_long, master_long, level = level)

  overall <- result[result$group == "All technologies" & !is.na(result$esi), ]
  if (nrow(overall) > 0) {
    message(sprintf(
      "Overall index: mean %.1f across %d pair-years (median basket coverage %.0f%%)",
      mean(overall$esi), nrow(overall), stats::median(overall$coverage_pct, na.rm = TRUE)
    ))
  }
  suppressed <- unique(result$group[is.na(result$esi)])
  suppressed <- setdiff(suppressed, unique(result$group[!is.na(result$esi)]))
  if (length(suppressed) > 0) {
    message("Suppressed (fewer than ", EXPORT_SIMILARITY_MIN_PRODUCTS, " products): ",
            paste(suppressed, collapse = ", "))
  }

  if (write_output) {
    out_dir <- file.path(repo_root, config$processed_dir, "charts")
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    out_path <- file.path(
      out_dir, sprintf("export_similarity_%s_%d_%d.csv", level, start_year, end_year)
    )
    utils::write.csv(result, out_path, row.names = FALSE, na = "")
    message("Wrote ", out_path, " (", nrow(result), " rows)")
  }

  invisible(result)
}

## Alternative source: Atlas of Economic Complexity, HS92 4-digit ------------
#
# data/raw/hs92_country_product_year_4.csv is the Harvard Atlas HS92 4-digit panel
# (1995-2023, 1243 products, 242 economies). Two things make it worth using alongside
# the Comtrade path:
#
#   * it reaches back to 1995, where the HS6 basket cannot go, and
#   * it covers ALL traded products, so the index describes total export structure
#     rather than energy-technology export structure.
#
# Those are different measures. Values from this source are NOT comparable with the
# energy-basket index, which is why `group` records the basket and `level` the
# granularity on every row.

EXPORT_SIMILARITY_ATLAS_FILE <- "hs92_country_product_year_4.csv"
EXPORT_SIMILARITY_ATLAS_API <- "https://atlas.hks.harvard.edu/api/graphql"

# The staged panel stops before the newest release. The Growth Lab GraphQL API is
# public and unauthenticated, so years beyond the file are topped up from it.
# It returns Atlas product ids ("product-HS92-812"), not HS92 codes, so the id ->
# code mapping is taken from the staged file.
export_similarity_atlas_graphql <- function(query, timeout_seconds = 180) {
  if (!requireNamespace("curl", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Packages 'curl' and 'jsonlite' are required to query the Atlas API.")
  }

  handle <- curl::new_handle()
  curl::handle_setheaders(
    handle,
    "Content-Type" = "application/json",
    "User-Agent" = "OSI-pipeline/1.0"
  )
  curl::handle_setopt(
    handle,
    customrequest = "POST",
    timeout = timeout_seconds,
    postfields = jsonlite::toJSON(list(query = query), auto_unbox = TRUE)
  )

  response <- curl::curl_fetch_memory(EXPORT_SIMILARITY_ATLAS_API, handle = handle)
  if (response$status_code >= 400) {
    stop("Atlas API returned HTTP ", response$status_code)
  }

  parsed <- jsonlite::fromJSON(rawToChar(response$content))
  if (!is.null(parsed$errors)) {
    stop("Atlas API error: ", paste(parsed$errors$message, collapse = "; "))
  }

  # A year the Atlas has not released yet comes back as an empty list rather than an
  # error, so normalise to NULL and let callers treat it as "no data".
  out <- parsed$data$countryProductYear
  if (is.null(out) || !is.data.frame(out) || nrow(out) == 0) return(NULL)
  out
}

# One year of HS92 4-digit exports for the given economies, keyed by HS92 code.
export_similarity_atlas_api_year <- function(year, country_ids, product_map,
                                             pause_seconds = 0.6) {
  rows <- list()
  for (iso in names(country_ids)) {
    query <- sprintf(
      paste0("{ countryProductYear(countryId: %d, productClass: HS92, productLevel: 4, ",
             "yearMin: %d, yearMax: %d) { productId exportValue } }"),
      country_ids[[iso]], year, year
    )
    d <- export_similarity_atlas_graphql(query)
    if (is.null(d)) next

    rows[[length(rows) + 1L]] <- data.frame(
      year = year,
      iso = iso,
      product_id = suppressWarnings(as.integer(sub("^product-HS92-", "", d$productId))),
      value = suppressWarnings(as.numeric(d$exportValue)),
      stringsAsFactors = FALSE
    )
    Sys.sleep(pause_seconds)  # documented limit is 120 requests/minute
  }

  if (length(rows) == 0) return(NULL)
  out <- do.call(rbind, rows)
  out$code <- unname(product_map[as.character(out$product_id)])
  out <- out[!is.na(out$code) & !is.na(out$value) & out$value > 0, , drop = FALSE]
  out[, c("year", "iso", "code", "value"), drop = FALSE]
}

export_similarity_atlas_trade <- function(repo_root,
                                          start_year,
                                          end_year,
                                          isos = names(EXPORT_SIMILARITY_COUNTRIES),
                                          hs6_codes = NULL,
                                          allow_api = TRUE) {
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required to read the Atlas panel.")
  }

  path <- file.path(repo_root, config$raw_data_dir, EXPORT_SIMILARITY_ATLAS_FILE)
  if (!file.exists(path)) {
    stop("Atlas HS92 4-digit panel not found: ", path)
  }

  raw <- readr::read_csv(
    path,
    col_select = c("country_id", "country_iso3_code", "product_id",
                   "product_hs92_code", "year", "export_value"),
    col_types = readr::cols(
      country_id = readr::col_integer(),
      country_iso3_code = readr::col_character(),
      product_id = readr::col_integer(),
      product_hs92_code = readr::col_character(),
      year = readr::col_integer(),
      export_value = readr::col_double()
    ),
    progress = FALSE
  )

  out <- data.frame(
    year = raw$year,
    iso = raw$country_iso3_code,
    code = raw$product_hs92_code,
    value = raw$export_value,
    stringsAsFactors = FALSE
  )

  out <- out[out$iso %in% isos & out$year >= start_year & out$year <= end_year, , drop = FALSE]
  out <- out[!is.na(out$value) & out$value > 0, , drop = FALSE]

  # Top up any requested years the staged file does not reach.
  file_max <- suppressWarnings(max(raw$year, na.rm = TRUE))
  missing_years <- seq_len(0)
  if (is.finite(file_max) && end_year > file_max) {
    missing_years <- seq.int(from = file_max + 1L, to = end_year)
  }

  if (length(missing_years) > 0 && allow_api) {
    product_map <- stats::setNames(
      as.character(raw$product_hs92_code), as.character(raw$product_id)
    )
    product_map <- product_map[!duplicated(names(product_map))]

    country_ids <- tapply(raw$country_id, raw$country_iso3_code, function(x) x[1])
    country_ids <- as.list(country_ids[isos])
    country_ids <- country_ids[!vapply(country_ids, is.null, logical(1))]

    message("Staged panel ends at ", file_max, "; fetching ",
            paste(missing_years, collapse = ", "), " from the Atlas API")

    for (y in missing_years) {
      extra <- tryCatch(
        export_similarity_atlas_api_year(y, country_ids, product_map),
        error = function(e) {
          warning("Atlas API fetch failed for ", y, ": ", conditionMessage(e), call. = FALSE)
          NULL
        }
      )
      if (!is.null(extra) && nrow(extra) > 0) {
        message("  ", y, ": ", nrow(extra), " rows, ",
                length(unique(extra$iso)), " countries")
        out <- rbind(out, extra)
      }
    }
  } else if (length(missing_years) > 0) {
    warning("Requested years beyond ", file_max, " are unavailable (API disabled).", call. = FALSE)
  }

  # Optionally narrow to the energy basket. HS92 headings are matched on the first
  # four digits of the HS6 master codes, which is approximate across HS revisions.
  if (!is.null(hs6_codes)) {
    out <- out[out$code %in% unique(substr(hs6_codes, 1, 4)), , drop = FALSE]
  }

  out
}

# Full-trade-picture index from the Atlas panel, plus the partner chart.
# write_plot = FALSE writes the CSVs and returns the data without rendering a PNG,
# which is also the quickest path when only the series is wanted.
export_similarity_atlas_chart <- function(repo_root,
                                          start_year = 1995L,
                                          end_year = as.integer(format(Sys.Date(), "%Y")) - 1L,
                                          partner = "CHN",
                                          write_output = TRUE,
                                          write_plot = TRUE,
                                          allow_api = TRUE) {
  trade_long <- export_similarity_atlas_trade(
    repo_root, start_year, end_year, allow_api = allow_api
  )
  if (nrow(trade_long) == 0) {
    stop("No Atlas rows for ", start_year, "-", end_year, ".")
  }

  basket <- length(unique(trade_long$code))
  message(
    "Atlas HS92 4-digit: ", nrow(trade_long), " rows | ",
    length(unique(trade_long$iso)), " countries | ", basket, " products | ",
    start_year, "-", end_year
  )

  result <- export_similarity_compute(
    trade_long,
    basket_size = basket,
    group = "All products",
    level = "hs4"
  )

  series <- export_similarity_partner_series(result, partner = partner, group = "All products")
  partner_name <- unname(EXPORT_SIMILARITY_COUNTRIES[partner])

  if (write_output) {
    out_dir <- file.path(repo_root, config$processed_dir, "charts")
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    stem <- sprintf("export_similarity_atlas_hs4_%s_%d_%d", tolower(partner), start_year, end_year)

    # The partner series is the chart's data; the full panel is kept alongside it.
    series_path <- file.path(out_dir, paste0(stem, ".csv"))
    pairs_path <- file.path(out_dir, paste0(stem, "_all_pairs.csv"))

    utils::write.csv(
      series[, c("year", "level", "partner", "partner_name", "counterpart",
                 "counterpart_name", "pair", "esi", "products_compared", "coverage_pct")],
      series_path, row.names = FALSE, na = ""
    )
    utils::write.csv(result, pairs_path, row.names = FALSE, na = "")

    message("Wrote ", series_path, " (", nrow(series), " rows)")
    message("Wrote ", pairs_path, " (", nrow(result), " rows)")

    if (!write_plot) {
      return(invisible(list(
        result = result, series = series,
        series_csv = series_path, pairs_csv = pairs_path
      )))
    }

    years <- sort(unique(trade_long$year))
    plot <- export_similarity_plot_partner(
      series, partner_name = partner_name, level = "hs4",
      basket_label = "all traded products",
      source_label = "Harvard Growth Lab, Atlas of Economic Complexity (HS92, 4-digit)",
      basket_note = sprintf(
        "Total export structure across %d HS92 headings - not the energy-technology basket. Years %d-%d.",
        basket, min(years), max(years)
      )
    )
    png_path <- file.path(out_dir, paste0(stem, ".png"))
    ggplot2::ggsave(
      png_path, plot, width = 9, height = 5.5, dpi = 200,
      device = if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else NULL
    )
    message("Wrote ", png_path)
    return(invisible(list(
      result = result, series = series, plot = plot, png = png_path,
      series_csv = series_path, pairs_csv = pairs_path
    )))
  }

  invisible(list(result = result, series = series))
}

## Similarity with one country over time -------------------------------------

# Pull the pairs involving `partner` out of a result panel and label the other side.
# Pairs are stored with country_a < country_b, so the partner can be on either side.
export_similarity_partner_series <- function(result,
                                             partner = "CHN",
                                             group = "All technologies") {
  keep <- result$group == group &
    (result$country_a == partner | result$country_b == partner)
  out <- result[keep, , drop = FALSE]
  if (nrow(out) == 0) return(out)

  is_a <- out$country_a == partner
  out$counterpart <- ifelse(is_a, out$country_b, out$country_a)
  out$counterpart_name <- ifelse(is_a, out$country_b_name, out$country_a_name)
  out$partner <- partner
  out$partner_name <- ifelse(is_a, out$country_a_name, out$country_b_name)

  out <- out[!is.na(out$esi), , drop = FALSE]
  out[order(out$counterpart_name, out$year), , drop = FALSE]
}

# basket_label and source_label must describe the data actually passed in. They are
# parameters rather than constants because this chart serves two different measures:
# the energy-technology basket from Comtrade, and all traded products from the Atlas.
export_similarity_plot_partner <- function(series,
                                           partner_name = "China",
                                           level = EXPORT_SIMILARITY_DEFAULT_LEVEL,
                                           basket_note = NULL,
                                           basket_label = "the energy-technology export basket",
                                           source_label = paste(
                                             "UN Comtrade; basket from",
                                             "data/reference/energy_hs6_master.csv"
                                           )) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required to draw the export similarity chart.")
  }
  if (nrow(series) == 0) {
    stop("No similarity values to plot for ", partner_name, ".")
  }

  level_label <- switch(
    level,
    hs6 = "HS6 subheading", hs4 = "HS4 heading", hs2 = "HS2 chapter",
    tech = "technology", sub_sector = "sub-sector", level
  )

  subtitle <- paste0(
    "Finger–Kreinin index over ", basket_label, ", computed at ", level_label, " level"
  )
  caption <- paste0(
    "100 = identical export structures, 0 = no overlap. ",
    "Comparable only within a level: the index rises mechanically with aggregation.",
    if (!is.null(basket_note)) paste0("\n", basket_note) else "",
    "\nSource: ", source_label
  )

  years <- sort(unique(series$year))
  breaks <- if (length(years) > 12) scales::breaks_pretty(n = 10)(years) else years

  ggplot2::ggplot(
    series,
    ggplot2::aes(x = year, y = esi, colour = counterpart_name, group = counterpart_name)
  ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_x_continuous(breaks = breaks) +
    ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    ggplot2::labs(
      title = paste0("Export similarity with ", partner_name),
      subtitle = subtitle,
      x = NULL, y = "Export similarity index",
      colour = NULL, caption = caption
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(size = 8, hjust = 0, colour = "grey35"),
      panel.grid.minor = ggplot2::element_blank()
    )
}

# Run the index and write the China time-series chart plus its underlying series.
export_similarity_china_chart <- function(repo_root,
                                          start_year = EXPORT_SIMILARITY_DEFAULT_START,
                                          end_year = as.integer(format(Sys.Date(), "%Y")) - 1L,
                                          level = EXPORT_SIMILARITY_DEFAULT_LEVEL,
                                          refresh = FALSE) {
  result <- run_export_similarity(
    repo_root = repo_root,
    start_year = start_year, end_year = end_year,
    level = level, refresh = refresh, write_output = TRUE
  )

  series <- export_similarity_partner_series(result, partner = "CHN")
  if (nrow(series) == 0) {
    stop("No China pairs survived the index; nothing to chart.")
  }

  coverage <- stats::median(series$coverage_pct, na.rm = TRUE)
  basket_note <- sprintf(
    "Median basket coverage %.0f%% of %d products; years %d–%d.",
    coverage, max(series$products_compared, na.rm = TRUE),
    min(series$year), max(series$year)
  )

  out_dir <- file.path(repo_root, config$processed_dir, "charts")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  stem <- sprintf("export_similarity_china_%s_%d_%d", level, start_year, end_year)

  csv_path <- file.path(out_dir, paste0(stem, ".csv"))
  utils::write.csv(
    series[, c("year", "level", "partner", "partner_name", "counterpart",
               "counterpart_name", "pair", "esi", "products_compared", "coverage_pct")],
    csv_path, row.names = FALSE, na = ""
  )

  plot <- export_similarity_plot_partner(
    series, partner_name = "China", level = level, basket_note = basket_note
  )

  png_path <- file.path(out_dir, paste0(stem, ".png"))
  ggplot2::ggsave(
    png_path, plot, width = 9, height = 5.5, dpi = 200,
    device = if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else NULL
  )

  message("Wrote ", csv_path)
  message("Wrote ", png_path)
  if (length(unique(series$year)) < 2) {
    warning(
      "Only ", length(unique(series$year)),
      " year of data; the chart is a snapshot, not a time series. ",
      "Widen ESI_START_YEAR/ESI_END_YEAR and re-run with ESI_REFRESH=true.",
      call. = FALSE
    )
  }

  invisible(list(series = series, plot = plot, csv = csv_path, png = png_path))
}

opsi_export_similarity_run_directly <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  length(file_arg) > 0 &&
    identical(basename(sub("^--file=", "", file_arg[1])), "export_similarity.R")
}

if (opsi_export_similarity_run_directly()) {
  start_year <- as.integer(Sys.getenv("ESI_START_YEAR", EXPORT_SIMILARITY_DEFAULT_START))
  end_year <- as.integer(Sys.getenv(
    "ESI_END_YEAR", as.character(as.integer(format(Sys.Date(), "%Y")) - 1L)
  ))
  refresh <- tolower(Sys.getenv("ESI_REFRESH", "false")) %in% c("1", "true", "yes")
  level <- tolower(Sys.getenv("ESI_LEVEL", EXPORT_SIMILARITY_DEFAULT_LEVEL))

  # Builds the index, writes the full panel, then the China time-series chart.
  invisible(export_similarity_china_chart(
    repo_root = repo_root,
    start_year = start_year,
    end_year = end_year,
    level = level,
    refresh = refresh
  ))
}

esi_time <- read.csv('C:/Users/LCarey/OneDrive - RMI/Documents/GitHub/Opportunity_Security_Indices/data/processed/charts/export_similarity_atlas_hs4_chn_1995_2024.csv')

esi_time_wide<-esi_time %>%
  filter(partner=="CHN") %>%
  select(year,counterpart_name  ,esi) %>%
  pivot_wider(names_from="counterpart_name",values_from="esi")

write.csv(esi_time_wide,'C:/Users/LCarey/OneDrive - RMI/Documents/GitHub/Opportunity_Security_Indices/data/processed/charts/esi_time_wide.csv')
