# Structural trade benchmark for Competitive Position (docs/strategic_position.md, section 2.2): the
# export position of each technology x six-stage basket averaged over the benchmark years (2022-2024)
# instead of read from one year. Built from Comtrade annual exports to the World of the basket codes
# and total merchandise exports (scripts/94_pull_comtrade_trade_refresh.R, mode annual).
#
# For each reporter and year it reported its total: basket exports, the world export share (over the
# reporters of that year) and the Balassa RCA; each is then averaged over the years observed. The
# index values that replace EO's market_share and rca in Competitive Position are the same S-curve
# ranks across countries within the cell that the annual EO trade uses (median_scurve()).
#
# Pure: callers pass the tables. Needs R/utils/scurve.R and R/utils/supply_chain_stages.R.

SP_TB_TOTAL_CODE <- "TOTAL"

# annual: reporter_iso, period (year), cmd_code (HS6 or "TOTAL"), primary_value (exports to World).
# codes: tech, supply_chain, code6 (trade_core_build_energy_codes()); a code in several cells counts
# in each, as in the annual EO trade. exclude_reporters: re-export hubs, left out of every sum.
# min_years: fewer reported years than this leaves the reporter's averages NA.
#
# Returns one row per reporter_iso x tech x supply_chain with bench_exports (mean annual basket
# exports, USD), bench_share (mean yearly world share), bench_rca (mean yearly RCA), bench_n_years and
# bench_years (the years averaged, "2022;2023;2024").
sp_trade_benchmark <- function(annual, codes, years, exclude_reporters = character(), min_years = 2L) {
  need <- c("reporter_iso", "period", "cmd_code", "primary_value")
  miss <- setdiff(need, names(annual))
  if (length(miss)) stop("sp_trade_benchmark(): annual lacks column(s) ", paste(miss, collapse = ", "), ".")
  a <- data.frame(
    reporter_iso = toupper(as.character(annual$reporter_iso)),
    year = as.integer(substr(as.character(annual$period), 1, 4)),
    cmd_code = as.character(annual$cmd_code),
    value = as.numeric(annual$primary_value),
    stringsAsFactors = FALSE
  )
  a <- a[a$year %in% as.integer(years) & !a$reporter_iso %in% toupper(exclude_reporters) & !is.na(a$value), , drop = FALSE]
  is_total <- a$cmd_code == SP_TB_TOTAL_CODE
  totals <- stats::aggregate(value ~ reporter_iso + year, data = a[is_total, , drop = FALSE], FUN = sum)
  names(totals)[names(totals) == "value"] <- "total_exports"
  totals <- totals[totals$total_exports > 0, , drop = FALSE]
  if (!nrow(totals)) stop("sp_trade_benchmark(): no reporter has total exports in the benchmark years.")

  codes <- unique(data.frame(
    tech = dplyr::if_else(codes$tech == "Natural Gas", "Gas", as.character(codes$tech)),
    supply_chain = as.character(codes$supply_chain),
    code6 = stringr::str_pad(as.character(codes$code6), 6, side = "left", pad = "0"),
    stringsAsFactors = FALSE
  ))
  basket <- a[!is_total, , drop = FALSE]
  basket$code6 <- stringr::str_pad(basket$cmd_code, 6, side = "left", pad = "0")
  basket <- dplyr::inner_join(basket, codes, by = "code6", relationship = "many-to-many") %>%
    dplyr::group_by(.data$reporter_iso, .data$year, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(exports = sum(.data$value), .groups = "drop")

  # Every reporter-year with a total, in every cell: a reporter that reported its total and no basket
  # code exported none of the basket that year.
  cells <- unique(codes[, c("tech", "supply_chain")])
  yearly <- tidyr::crossing(totals, cells) %>%
    dplyr::left_join(basket, by = c("reporter_iso", "year", "tech", "supply_chain")) %>%
    dplyr::mutate(exports = dplyr::coalesce(.data$exports, 0)) %>%
    dplyr::group_by(.data$year, .data$tech, .data$supply_chain) %>%
    dplyr::mutate(world_exports = sum(.data$exports)) %>%
    dplyr::group_by(.data$year) %>%
    dplyr::mutate(world_total = sum(.data$total_exports[!duplicated(.data$reporter_iso)])) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      share = ifelse(.data$world_exports > 0, .data$exports / .data$world_exports, NA_real_),
      world_basket_share = .data$world_exports / .data$world_total,
      rca = ifelse(.data$world_basket_share > 0, (.data$exports / .data$total_exports) / .data$world_basket_share, NA_real_)
    )
  yearly %>%
    dplyr::group_by(.data$reporter_iso, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(
      bench_n_years = dplyr::n(),
      bench_years = paste(sort(.data$year), collapse = ";"),
      bench_exports = mean(.data$exports),
      bench_share = sp_mean_available(.data$share),
      bench_rca = sp_mean_available(.data$rca),
      .groups = "drop"
    ) %>%
    dplyr::mutate(dplyr::across(c("bench_exports", "bench_share", "bench_rca"),
                                ~ ifelse(.data$bench_n_years >= min_years, .x, NA_real_))) %>%
    dplyr::arrange(.data$tech, supply_chain_stage_factor(.data$supply_chain), .data$reporter_iso)
}

# The index values for Competitive Position. bench: sp_trade_benchmark(). economies: the ISO3 codes
# ranked (the index's economies; NULL = all in bench). atlas_share (optional): iso3, tech,
# supply_chain, market_share (the Atlas raw world share), the fallback share for economies that do not
# report to Comtrade, so they keep a world position instead of none. RCA has no fallback.
#
# Returns iso3 x tech x supply_chain with bench_share, bench_rca, bench_exports, bench_n_years,
# trade_benchmark_source ("comtrade" / "atlas_fallback"), market_share_index and rca_index (S-curve
# ranks across the economies within the cell).
sp_trade_benchmark_index <- function(bench, economies = NULL, atlas_share = NULL) {
  out <- dplyr::rename(bench, iso3 = "reporter_iso")
  out$trade_benchmark_source <- ifelse(is.na(out$bench_share), NA_character_, "comtrade")
  if (!is.null(atlas_share) && nrow(atlas_share)) {
    fb <- atlas_share %>%
      dplyr::transmute(iso3 = toupper(.data$iso3), .data$tech, .data$supply_chain, atlas_share = as.numeric(.data$market_share)) %>%
      dplyr::filter(!is.na(.data$atlas_share), !.data$iso3 %in% out$iso3[!is.na(out$bench_share)]) %>%
      dplyr::distinct(.data$iso3, .data$tech, .data$supply_chain, .keep_all = TRUE)
    out <- dplyr::full_join(out, fb, by = c("iso3", "tech", "supply_chain")) %>%
      dplyr::mutate(
        trade_benchmark_source = dplyr::if_else(is.na(.data$bench_share) & !is.na(.data$atlas_share), "atlas_fallback", .data$trade_benchmark_source),
        bench_share = dplyr::coalesce(.data$bench_share, .data$atlas_share)
      ) %>%
      dplyr::select(-"atlas_share")
  }
  if (!is.null(economies)) out <- out[out$iso3 %in% toupper(economies), , drop = FALSE]
  out %>%
    dplyr::group_by(.data$tech, .data$supply_chain) %>%
    dplyr::mutate(
      market_share_index = median_scurve(.data$bench_share),
      rca_index = median_scurve(.data$bench_rca)
    ) %>%
    dplyr::ungroup()
}

# EO contributions with market_share and rca replaced by the benchmark index values. eo_contrib keys
# countries by name; iso3_of maps those names to ISO3 (a function). A row with no benchmark value
# becomes not observed (component NA, imputed TRUE): Competitive Position reads `fill: na` for both.
# Rows of other variables are returned unchanged.
sp_apply_trade_benchmark <- function(eo_contrib, bench_idx, iso3_of) {
  hit <- eo_contrib$category == "Trade" & eo_contrib$variable %in% c("market_share", "rca")
  rows <- eo_contrib[hit, , drop = FALSE]
  rows$.iso3 <- iso3_of(rows$Country)
  long <- dplyr::bind_rows(
    data.frame(.iso3 = bench_idx$iso3, tech = bench_idx$tech, supply_chain = bench_idx$supply_chain,
               variable = "market_share", .bench = bench_idx$market_share_index, stringsAsFactors = FALSE),
    data.frame(.iso3 = bench_idx$iso3, tech = bench_idx$tech, supply_chain = bench_idx$supply_chain,
               variable = "rca", .bench = bench_idx$rca_index, stringsAsFactors = FALSE)
  )
  rows <- dplyr::left_join(rows, long, by = c(".iso3", "tech", "supply_chain", "variable"))
  rows$component_value <- rows$.bench
  rows$imputed <- is.na(rows$.bench)
  rows$.iso3 <- NULL
  rows$.bench <- NULL
  out <- eo_contrib
  out[hit, names(rows)] <- rows
  out
}
