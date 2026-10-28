# Current trade momentum for Competitive Position (docs/strategic_position.md, section 2.2): how a
# country's exports of each technology x six-stage basket moved over the latest twelve months against
# the twelve months before, from Comtrade monthly exports to the World
# (scripts/94_pull_comtrade_trade_refresh.R, mode monthly).
#
# One common cutoff month M for everyone: the latest month at which at least `min_share` of the
# reporters that publish monthly trade have all 24 months M-23..M. A reporter without the complete 24
# months gets NA, never 0; there is no partial-year comparison and no mirror (partner-reported) fill.
#
# Measures, per reporter x cell, on the reporters complete at M:
#   A  log export growth           log(X_current / X_previous)
#   B  specialization momentum     log of the change in the basket's share of total merchandise
#                                  exports = A - log(T_current / T_previous)
#   C  world-share momentum        log of the change in the reporter's share of the complete
#                                  reporters' basket exports; only where those reporters cover enough
#                                  of world benchmark exports. Within a cell C differs from A by a
#                                  constant, so it ranks countries exactly as A: diagnostic only.
# Guards: a small-base floor on the previous twelve months (a quantile of the cell's distribution,
# never below an absolute minimum), an HS-revision check (a reporter that changed HS edition between
# the windows, with much of the cell's value in headings present in one window only), a reporter
# check (monthly totals against the annual total of a benchmark year) and winsorisation of A and B
# across all cells before ranking.
# trade_momentum_score is the mean of the mid-rank percentiles of A and B across the valid reporters
# within the cell: a world price swing that moves every exporter alike does not move a rank.
#
# Pure: no IO. Needs R/strategic_position/sp_helpers.R and R/utils/supply_chain_stages.R.

SP_TM_QUALITY <- c("ok", "caution", "no_monthly_data", "incomplete_coverage", "small_base", "hs_break",
                   "reporter_mismatch", "thin_cell", "outside_universe", "no_basket")

# YYYYMM integer arithmetic: add k months.
sp_month_add <- function(ym, k) {
  ym <- as.integer(ym)
  idx <- (ym %/% 100L) * 12L + (ym %% 100L - 1L) + as.integer(k)
  as.integer((idx %/% 12L) * 100L + idx %% 12L + 1L)
}

# "Sep 2025" style label of a YYYYMM month.
sp_month_label <- function(ym) {
  ym <- as.integer(ym)
  ifelse(is.na(ym), NA_character_, paste(month.abb[ym %% 100L], ym %/% 100L))
}

# The months of each reporter with a positive monthly total. monthly: reporter_iso, period (YYYYMM),
# cmd_code, primary_value. Returns reporter_iso, period.
sp_tm_total_months <- function(monthly) {
  t <- monthly[monthly$cmd_code == SP_TB_TOTAL_CODE & !is.na(monthly$primary_value) & monthly$primary_value > 0, , drop = FALSE]
  unique(data.frame(reporter_iso = toupper(as.character(t$reporter_iso)), period = as.integer(t$period), stringsAsFactors = FALSE))
}

# The common cutoff. universe: the core analysis universe (ISO3). min_share: share of the universe's
# monthly reporters (members with any monthly total) that must have all `2 * window` months ending at
# the cutoff. Returns list(cutoff, current, previous (integer YYYYMM vectors, oldest first),
# complete (ISO3 complete at the cutoff), eligible (monthly reporters), no_monthly (members without
# any monthly data), coverage (one row per candidate month: n_complete, share_eligible,
# share_universe)). cutoff is NA when no month reaches min_share.
sp_tm_cutoff <- function(monthly, universe, window = 12L, min_share = 0.8) {
  universe <- sort(unique(toupper(universe)))
  tm <- sp_tm_total_months(monthly)
  tm <- tm[tm$reporter_iso %in% universe, , drop = FALSE]
  eligible <- sort(unique(tm$reporter_iso))
  n_need <- 2L * as.integer(window)
  months <- sort(unique(tm$period), decreasing = TRUE)
  by_rep <- split(tm$period, tm$reporter_iso)
  coverage <- dplyr::bind_rows(lapply(months, function(m) {
    need <- sp_month_add(m, seq.int(-(n_need - 1L), 0L))
    complete <- vapply(eligible, function(r) all(need %in% by_rep[[r]]), logical(1))
    data.frame(cutoff = m, n_eligible = length(eligible), n_complete = sum(complete),
               share_eligible = if (length(eligible)) mean(complete) else NA_real_,
               share_universe = sum(complete) / length(universe))
  }))
  ok <- coverage$cutoff[!is.na(coverage$share_eligible) & coverage$share_eligible >= min_share]
  cutoff <- if (length(ok)) max(ok) else NA_integer_
  complete <- character()
  if (!is.na(cutoff)) {
    need <- sp_month_add(cutoff, seq.int(-(n_need - 1L), 0L))
    complete <- eligible[vapply(eligible, function(r) all(need %in% by_rep[[r]]), logical(1))]
  }
  list(
    cutoff = cutoff,
    current = if (is.na(cutoff)) integer() else sp_month_add(cutoff, seq.int(-(window - 1L), 0L)),
    previous = if (is.na(cutoff)) integer() else sp_month_add(cutoff, seq.int(-(n_need - 1L), -window)),
    complete = complete, eligible = eligible, no_monthly = setdiff(universe, eligible),
    universe = universe, min_share = min_share, coverage = coverage
  )
}

# Winsorise at the given quantiles of the non-missing values.
sp_winsorize <- function(x, probs = c(0.05, 0.95)) {
  ok <- !is.na(x) & is.finite(x)
  if (sum(ok) < 3) return(x)
  q <- stats::quantile(x[ok], probs, names = FALSE, type = 7)
  pmin(pmax(x, q[1]), q[2])
}

# Momentum per reporter x cell.
#
# monthly: reporter_iso, period, cmd_code ("TOTAL" for the merchandise total), primary_value and
#   optionally classification_code (HS edition) and net_wgt. Exports to the World only.
# codes: tech, supply_chain, code6. cut: sp_tm_cutoff().
# bench: sp_trade_benchmark() (reporter_iso, tech, supply_chain, bench_exports), for the share
#   coverage of C. annual_totals: reporter_iso, year, total_exports (the annual merchandise total),
#   for the reporter check. cfg: the trade_momentum block of config/strategic_position.yml.
#
# Returns list(cells, reporters, floors):
#   cells: every universe reporter x cell with trade_momentum_period, _previous_period,
#     trade_months_observed, trade_momentum_quality (SP_TM_QUALITY), trade_momentum_coverage (share of
#     the cell's world benchmark exports held by the complete reporters), trade_momentum_raw (A,
#     winsorised), trade_momentum_specialization (B, winsorised), trade_momentum_share_change (C,
#     diagnostic), trade_momentum_score, trade_exports_current, trade_exports_previous,
#     trade_value_volume_gap (median log value growth minus median log weight growth of the cell: the
#     price component, diagnostic);
#   reporters: one row per universe reporter with months observed, the HS editions and the reporter
#     check;
#   floors: the small-base floor of each cell.
sp_trade_momentum <- function(monthly, codes, cut, bench = NULL, annual_totals = NULL, cfg = list()) {
  q_floor <- cfg$small_base_quantile %||% 0.25
  min_usd <- cfg$small_base_min_usd %||% 1e6
  wins <- unlist(cfg$winsorize %||% c(0.05, 0.95))
  hs_break <- cfg$hs_break_share %||% 0.25
  mism <- cfg$reporter_mismatch %||% list(caution = 0.10, exclude = 0.25)
  check_year <- as.integer(cfg$reporter_check_year %||% NA_integer_)
  min_rep <- as.integer(cfg$min_reporters_per_cell %||% 5L)
  share_cov <- cfg$share_coverage_min %||% 0.8

  codes <- unique(data.frame(
    tech = dplyr::if_else(codes$tech == "Natural Gas", "Gas", as.character(codes$tech)),
    supply_chain = as.character(codes$supply_chain),
    code6 = stringr::str_pad(as.character(codes$code6), 6, side = "left", pad = "0"),
    stringsAsFactors = FALSE
  ))
  cells <- unique(codes[, c("tech", "supply_chain")])
  m <- data.frame(
    reporter_iso = toupper(as.character(monthly$reporter_iso)),
    period = as.integer(monthly$period),
    cmd_code = as.character(monthly$cmd_code),
    value = as.numeric(monthly$primary_value),
    hs = if ("classification_code" %in% names(monthly)) as.character(monthly$classification_code) else NA_character_,
    wgt = if ("net_wgt" %in% names(monthly)) suppressWarnings(as.numeric(monthly$net_wgt)) else NA_real_,
    stringsAsFactors = FALSE
  )
  m <- m[m$reporter_iso %in% cut$universe & !is.na(m$value), , drop = FALSE]

  # Reporter table: months observed in the 24-month window, HS editions per window, monthly vs annual.
  tm <- sp_tm_total_months(monthly)
  win <- c(cut$previous, cut$current)
  rep_tbl <- data.frame(reporter_iso = cut$universe, stringsAsFactors = FALSE)
  rep_tbl$months_observed <- vapply(rep_tbl$reporter_iso, function(r) sum(win %in% tm$period[tm$reporter_iso == r]), integer(1))
  rep_tbl$complete <- rep_tbl$reporter_iso %in% cut$complete
  rep_tbl$has_monthly <- rep_tbl$reporter_iso %in% cut$eligible
  ed <- function(r, months) {
    e <- sort(unique(m$hs[m$reporter_iso == r & m$period %in% months & m$cmd_code != SP_TB_TOTAL_CODE]))
    paste(e[!is.na(e)], collapse = ";")
  }
  rep_tbl$hs_previous <- vapply(rep_tbl$reporter_iso, ed, character(1), months = cut$previous)
  rep_tbl$hs_current <- vapply(rep_tbl$reporter_iso, ed, character(1), months = cut$current)
  rep_tbl$hs_revision <- nzchar(rep_tbl$hs_previous) & nzchar(rep_tbl$hs_current) & rep_tbl$hs_previous != rep_tbl$hs_current
  rep_tbl$annual_gap <- NA_real_
  if (!is.na(check_year) && !is.null(annual_totals) && nrow(annual_totals)) {
    yr <- m[m$cmd_code == SP_TB_TOTAL_CODE & m$period %/% 100L == check_year, , drop = FALSE]
    n_mon <- tapply(yr$period, yr$reporter_iso, function(p) length(unique(p)))
    s_mon <- tapply(yr$value, yr$reporter_iso, sum)
    full <- names(n_mon)[n_mon == 12L]
    at <- annual_totals[as.integer(annual_totals$year) == check_year, , drop = FALSE]
    a_val <- stats::setNames(as.numeric(at$total_exports), toupper(at$reporter_iso))
    gap <- abs(s_mon[full] / a_val[full] - 1)
    rep_tbl$annual_gap <- unname(gap[rep_tbl$reporter_iso])
  }
  rep_tbl$reporter_quality <- dplyr::case_when(
    !rep_tbl$has_monthly ~ "no_monthly_data",
    !rep_tbl$complete ~ "incomplete_coverage",
    !is.na(rep_tbl$annual_gap) & rep_tbl$annual_gap > mism$exclude ~ "reporter_mismatch",
    !is.na(rep_tbl$annual_gap) & rep_tbl$annual_gap > mism$caution ~ "caution",
    TRUE ~ "ok"
  )

  grid <- tidyr::crossing(rep_tbl[, c("reporter_iso", "months_observed", "reporter_quality", "hs_revision")], cells)
  period_lab <- if (is.na(cut$cutoff)) NA_character_ else paste(sp_month_label(min(cut$current)), "-", sp_month_label(max(cut$current)))
  prev_lab <- if (is.na(cut$cutoff)) NA_character_ else paste(sp_month_label(min(cut$previous)), "-", sp_month_label(max(cut$previous)))
  if (is.na(cut$cutoff) || !length(cut$complete)) {
    out <- grid %>% dplyr::transmute(
      iso3 = .data$reporter_iso, .data$tech, .data$supply_chain, trade_momentum_period = period_lab,
      trade_momentum_previous_period = prev_lab, trade_months_observed = .data$months_observed,
      trade_momentum_quality = dplyr::if_else(.data$reporter_quality == "no_monthly_data", "no_monthly_data", "incomplete_coverage"),
      trade_momentum_coverage = NA_real_, trade_momentum_raw = NA_real_, trade_momentum_specialization = NA_real_,
      trade_momentum_share_change = NA_real_, trade_momentum_score = NA_real_,
      trade_exports_current = NA_real_, trade_exports_previous = NA_real_, trade_value_volume_gap = NA_real_
    )
    return(list(cells = out, reporters = rep_tbl, floors = data.frame()))
  }

  ok_rep <- rep_tbl$reporter_iso[rep_tbl$complete]
  mm <- m[m$reporter_iso %in% ok_rep & m$period %in% win, , drop = FALSE]
  mm$window <- ifelse(mm$period %in% cut$current, "cur", "prev")
  tot <- mm[mm$cmd_code == SP_TB_TOTAL_CODE, , drop = FALSE] %>%
    dplyr::group_by(.data$reporter_iso, .data$window) %>%
    dplyr::summarise(total = sum(.data$value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = "window", values_from = "total", names_prefix = "total_")
  bk <- mm[mm$cmd_code != SP_TB_TOTAL_CODE, , drop = FALSE]
  bk$code6 <- stringr::str_pad(bk$cmd_code, 6, side = "left", pad = "0")
  bk <- dplyr::inner_join(bk, codes, by = "code6", relationship = "many-to-many")
  by_cell <- bk %>%
    dplyr::group_by(.data$reporter_iso, .data$tech, .data$supply_chain, .data$window) %>%
    dplyr::summarise(x = sum(.data$value), w = sum(.data$wgt, na.rm = TRUE), w_n = sum(!is.na(.data$wgt)), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = "window", values_from = c("x", "w", "w_n"), values_fill = 0)
  for (col in c("x_cur", "x_prev", "w_cur", "w_prev")) if (!col %in% names(by_cell)) by_cell[[col]] <- 0
  # HS headings present in one window only, as a share of the cell's value over both windows.
  brk <- bk %>%
    dplyr::mutate(code4 = substr(.data$code6, 1, 4)) %>%
    dplyr::group_by(.data$reporter_iso, .data$tech, .data$supply_chain, .data$code4) %>%
    dplyr::summarise(one_side = dplyr::n_distinct(.data$window) == 1, v = sum(.data$value), .groups = "drop") %>%
    dplyr::group_by(.data$reporter_iso, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(break_share = sum(.data$v[.data$one_side]) / sum(.data$v), .groups = "drop")

  x <- tidyr::crossing(reporter_iso = ok_rep, cells) %>%
    dplyr::left_join(by_cell, by = c("reporter_iso", "tech", "supply_chain")) %>%
    dplyr::left_join(tot, by = "reporter_iso") %>%
    dplyr::left_join(brk, by = c("reporter_iso", "tech", "supply_chain")) %>%
    dplyr::left_join(rep_tbl[, c("reporter_iso", "reporter_quality", "hs_revision")], by = "reporter_iso") %>%
    dplyr::mutate(dplyr::across(c("x_cur", "x_prev"), ~ dplyr::coalesce(.x, 0)))
  floors <- x %>%
    dplyr::group_by(.data$tech, .data$supply_chain) %>%
    dplyr::summarise(
      small_base_floor = max(min_usd, if (any(.data$x_prev > 0)) stats::quantile(.data$x_prev[.data$x_prev > 0], q_floor, names = FALSE) else min_usd),
      .groups = "drop"
    )
  x <- x %>%
    dplyr::left_join(floors, by = c("tech", "supply_chain")) %>%
    dplyr::mutate(
      quality = dplyr::case_when(
        .data$reporter_quality == "reporter_mismatch" ~ "reporter_mismatch",
        .data$x_prev < .data$small_base_floor ~ "small_base",
        .data$hs_revision %in% TRUE & dplyr::coalesce(.data$break_share, 0) > hs_break ~ "hs_break",
        TRUE ~ .data$reporter_quality
      ),
      valid = .data$quality %in% c("ok", "caution"),
      # a collapse to (near) nothing is real momentum: the current value is floored at 1% of the base
      a = ifelse(.data$valid, log(pmax(.data$x_cur, 0.01 * .data$x_prev) / .data$x_prev), NA_real_),
      b = ifelse(.data$valid & .data$total_cur > 0 & .data$total_prev > 0, .data$a - log(.data$total_cur / .data$total_prev), NA_real_),
      wg = ifelse(.data$valid & .data$w_n_cur > 0 & .data$w_n_prev > 0 & .data$w_prev > 0 & .data$w_cur > 0, log(.data$w_cur / .data$w_prev), NA_real_)
    )
  x$a_w <- sp_winsorize(x$a, wins)
  x$b_w <- sp_winsorize(x$b, wins)
  # C: share of the complete reporters' basket exports, where they cover enough of world exports.
  cov <- if (!is.null(bench) && nrow(bench)) {
    bench %>%
      dplyr::group_by(.data$tech, .data$supply_chain) %>%
      dplyr::summarise(trade_momentum_coverage = ifelse(sum(.data$bench_exports, na.rm = TRUE) > 0,
        sum(.data$bench_exports[toupper(.data$reporter_iso) %in% ok_rep], na.rm = TRUE) / sum(.data$bench_exports, na.rm = TRUE), NA_real_),
        .groups = "drop")
  } else {
    cells %>% dplyr::mutate(trade_momentum_coverage = NA_real_)
  }
  x <- x %>%
    dplyr::left_join(cov, by = c("tech", "supply_chain")) %>%
    dplyr::group_by(.data$tech, .data$supply_chain) %>%
    dplyr::mutate(
      n_valid = sum(!is.na(.data$a_w)),
      quality = dplyr::if_else(.data$valid & .data$n_valid < min_rep, "thin_cell", .data$quality),
      p_a = if (dplyr::first(.data$n_valid) >= min_rep) sp_midrank_pct(.data$a_w) else NA_real_,
      p_b = if (dplyr::first(.data$n_valid) >= min_rep) sp_midrank_pct(.data$b_w) else NA_real_,
      c_share = {
        s_cur <- .data$x_cur / sum(.data$x_cur)
        s_prev <- .data$x_prev / sum(.data$x_prev)
        ifelse(.data$valid & !is.na(.data$trade_momentum_coverage) & .data$trade_momentum_coverage >= share_cov & s_cur > 0 & s_prev > 0,
               log(s_cur / s_prev), NA_real_)
      },
      value_volume_gap = stats::median(.data$a[.data$valid], na.rm = TRUE) - stats::median(.data$wg, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(score = ifelse(is.na(.data$p_a) | is.na(.data$p_b), ifelse(is.na(.data$p_a), NA_real_, .data$p_a), (.data$p_a + .data$p_b) / 2))

  grid %>%
    dplyr::left_join(cov, by = c("tech", "supply_chain")) %>%
    dplyr::left_join(
      x %>% dplyr::select("reporter_iso", "tech", "supply_chain", "quality", "a_w", "b_w", "c_share", "score",
                          "x_cur", "x_prev", "value_volume_gap"),
      by = c("reporter_iso", "tech", "supply_chain")
    ) %>%
    dplyr::transmute(
      iso3 = .data$reporter_iso, .data$tech, .data$supply_chain,
      trade_momentum_period = period_lab,
      trade_momentum_previous_period = prev_lab,
      trade_months_observed = .data$months_observed,
      trade_momentum_quality = dplyr::coalesce(.data$quality, .data$reporter_quality),
      .data$trade_momentum_coverage,
      trade_momentum_raw = .data$a_w,
      trade_momentum_specialization = .data$b_w,
      trade_momentum_share_change = .data$c_share,
      trade_momentum_score = .data$score,
      trade_exports_current = .data$x_cur,
      trade_exports_previous = .data$x_prev,
      trade_value_volume_gap = .data$value_volume_gap
    ) %>%
    (function(o) list(cells = o, reporters = rep_tbl, floors = floors))()
}

# Blend momentum into Competitive Position's export family and rescore. cells: sp_competitive_position()$
# cells; tm: iso3 x tech x supply_chain with trade_momentum_score (and the other momentum fields);
# iso3_of maps cells$Country to ISO3; families: names(competitive_position$families), the columns the
# score averages. weight: momentum's weight in the trade pillar where both exist
# (structural alone otherwise). Keeps the structural value as cp_<family>_structural. The score is the
# mean of the observed families, its percentile and rank across the countries within the cell, as in
# sp_competitive_position().
sp_cp_blend_momentum <- function(cells, tm, iso3_of, families, weight = 0.15, family = "export_position") {
  fam <- paste0("cp_", family)
  if (!fam %in% names(cells)) stop("sp_cp_blend_momentum(): cells has no ", fam, " column.")
  fam_cols <- intersect(paste0("cp_", families), names(cells))
  cells$iso3 <- iso3_of(cells$Country)
  if (!paste0(fam, "_structural") %in% names(cells)) cells[[paste0(fam, "_structural")]] <- cells[[fam]]
  s <- cells[[paste0(fam, "_structural")]]
  mom <- dplyr::left_join(cells[, c("iso3", "tech", "supply_chain")], tm[, c("iso3", "tech", "supply_chain", "trade_momentum_score")],
                          by = c("iso3", "tech", "supply_chain"))$trade_momentum_score
  blended <- !is.na(s) & !is.na(mom) & weight > 0
  cells[[fam]] <- ifelse(blended, (1 - weight) * s + weight * mom, s)
  cells$trade_momentum_weight <- ifelse(blended, weight, 0)
  had <- !is.na(cells$competitive_position_score)
  cells$competitive_position_score <- ifelse(had, sp_row_mean_available(cells[, fam_cols, drop = FALSE]), NA_real_)
  cells %>%
    dplyr::group_by(.data$tech, .data$supply_chain) %>%
    dplyr::mutate(
      competitive_position_percentile = if (dplyr::first(.data$cp_thin_cell)) NA_real_ else sp_midrank_pct(.data$competitive_position_score),
      competitive_position_rank = if (dplyr::first(.data$cp_thin_cell)) NA_integer_ else sp_rank_desc(.data$competitive_position_score)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"iso3")
}
