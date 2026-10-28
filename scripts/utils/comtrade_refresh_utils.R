# Pure helpers for the trade refresh pull (scripts/94_pull_comtrade_trade_refresh.R): the annual
# 2022-2024 benchmark and the monthly momentum window. No IO here. Needs
# scripts/utils/comtrade_monthly_utils.R (comtrade_month_add).

# The analysis universe for momentum: coalition members plus every economy whose latest GDP is at
# least `gdp_min` USD. `gdp` has iso3 and gdp_usd; aggregates must already be removed.
comtrade_refresh_universe <- function(coalition_iso3, gdp, gdp_min = 2e11) {
  big <- gdp$iso3[!is.na(gdp$gdp_usd) & gdp$gdp_usd >= gdp_min]
  sort(unique(c(as.character(coalition_iso3), as.character(big))))
}

# The months to request: the `n_months` calendar months ending at the latest month that at least
# `min_share` of the universe lists in the monthly data-availability listing (getDA). `da` has
# reporter_iso and period (YYYYMM). Asking for the same months for everyone keeps the windows
# comparable; reporters that publish later simply return fewer months.
comtrade_refresh_months <- function(da, universe, n_months = 38L, min_share = 0.25) {
  da <- da[da$reporter_iso %in% universe & !is.na(da$period), , drop = FALSE]
  share <- tapply(da$reporter_iso, da$period, function(r) length(unique(r))) / length(universe)
  ok <- as.integer(names(share)[share >= min_share])
  if (length(ok) == 0) stop("No month is listed by ", min_share * 100, "% of the universe.")
  last <- max(ok)
  comtrade_month_add(last, seq.int(-(n_months - 1L), 0L))
}

# Split periods into consecutive call windows of at most `max_periods` (the data API refuses more
# than 12 periods in one non-bulk call).
comtrade_refresh_period_windows <- function(periods, max_periods = 12L) {
  periods <- sort(unique(as.integer(periods)))
  split(periods, ceiling(seq_along(periods) / max_periods))
}

# One call per (period window, reporter batch, code group); `code_groups` is a list whose last
# element may be "TOTAL" (all commodities). Returns chunk_id, reporters (space-separated), periods
# (comma-separated), code_group.
comtrade_refresh_plan <- function(prefix, reporters, period_windows, n_code_groups, max_reporters = 15L) {
  reporters <- sort(unique(reporters))
  batches <- split(reporters, ceiling(seq_along(reporters) / max_reporters))
  rows <- list()
  for (w in seq_along(period_windows)) {
    for (b in seq_along(batches)) {
      for (g in seq_len(n_code_groups)) {
        rows[[length(rows) + 1L]] <- data.frame(
          chunk_id = sprintf("%s_w%02d_b%02d_g%d", prefix, w, b, g),
          reporters = paste(batches[[b]], collapse = " "),
          periods = paste(period_windows[[w]], collapse = ","),
          code_group = g, stringsAsFactors = FALSE
        )
      }
    }
  }
  do.call(rbind, rows)
}
