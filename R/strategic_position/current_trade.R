# Current trade: each reporter's latest 12 months of bilateral Comtrade data, summed to a trailing-12-month
# (T12M) dyad table that the annual rules (partnership_strength_build_trade_cells(), sp_import_exposure())
# can read unchanged, and the comparison of the resulting import exposure with the 2024 table.
#
# scripts/37_pull_comtrade_monthly_dyads.R pulls the months; scripts/38_build_current_import_exposure.R
# builds and compares. Windows are per reporter (each its own latest 12 months), so an importer's shares
# are internally consistent while different importers can end in different months; every row carries
# its window_end.
#
# Pure: no IO.

# Month arithmetic on YYYYMM integers.
ctm_month_add <- function(ym, k) {
  ym <- as.integer(ym)
  m0 <- (ym %/% 100L) * 12L + (ym %% 100L - 1L) + as.integer(k)
  (m0 %/% 12L) * 100L + (m0 %% 12L) + 1L
}

# The `n_months` YYYYMM months ending at `end`.
ctm_window_months <- function(end, n_months = 12L) ctm_month_add(end, seq.int(-(n_months - 1L), 0L))

# One window per reporter from the monthly data-availability listing (`da`: reporter_iso, period YYYYMM):
# the `n_months` months ending at the reporter's latest listed month, kept when that month is no earlier
# than `min_end` and at least `min_months` of the window are listed. Returns reporter_iso, window_start,
# window_end, months_listed; dropped reporters (stale or gappy) are in attr "dropped".
ctm_reporter_windows <- function(da, n_months = 12L, min_months = 10L, min_end = NULL) {
  if (!all(c("reporter_iso", "period") %in% names(da))) stop("ctm_reporter_windows(): da needs reporter_iso and period.")
  da <- unique(data.frame(reporter_iso = as.character(da$reporter_iso), period = as.integer(da$period)))
  da <- da[!is.na(da$period) & !is.na(da$reporter_iso), , drop = FALSE]
  rows <- lapply(split(da$period, da$reporter_iso), function(p) {
    end <- max(p)
    win <- ctm_window_months(end, n_months)
    data.frame(window_start = min(win), window_end = end, months_listed = sum(win %in% p))
  })
  out <- cbind(reporter_iso = names(rows), do.call(rbind, rows), stringsAsFactors = FALSE)
  rownames(out) <- NULL
  floor_end <- if (is.null(min_end)) -Inf else as.integer(min_end)
  stale <- out$window_end < floor_end
  ok <- !stale & out$months_listed >= min_months
  dropped <- out[!ok, , drop = FALSE]
  dropped$reason <- ifelse(stale[!ok], "stale", "gappy")
  out <- out[ok, , drop = FALSE]
  attr(out, "dropped") <- dropped
  out
}

# Per reporter x flow x month: commodity codes with data, and whether the month is complete (at least
# `min_code_share` of the reporter-flow's median month). Catches partial releases such as Japan's April
# 2026 imports (158 of ~372 codes). `monthly` has reporter_iso, flow_direction, period, cmd_code.
ctm_month_completeness <- function(monthly, min_code_share = 0.6) {
  k <- unique(monthly[c("reporter_iso", "flow_direction", "period", "cmd_code")])
  n <- stats::aggregate(cmd_code ~ reporter_iso + flow_direction + period, data = k, FUN = length)
  names(n)[4] <- "n_codes"
  med <- stats::ave(n$n_codes, n$reporter_iso, n$flow_direction, FUN = stats::median)
  n$complete <- n$n_codes >= min_code_share * med
  n[order(n$reporter_iso, n$flow_direction, n$period), ]
}

# T12M dyads: monthly rows (reporter_iso, partner_iso, flow_direction, period, cmd_code, primary_value)
# inside each reporter's window (`windows`: reporter_iso, window_end; window of `n_months`) and in a
# complete month (`completeness` from ctm_month_completeness()), summed per reporter x partner x flow x
# code and scaled by n_months / complete months. A reporter-flow with fewer than `min_months` complete
# months is dropped (attr "dropped"). Scaling keeps levels annual; within-importer shares do not depend
# on it. Returns those columns plus months_used, window_end, with period set to `period_label` so the
# table can sit beside an annual year in partnership_strength_build_trade_cells().
ctm_t12m_dyads <- function(monthly, windows, completeness, n_months = 12L, min_months = 10L, period_label = 9999L) {
  m <- merge(monthly, windows[c("reporter_iso", "window_end")], by = "reporter_iso")
  m <- m[m$period <= m$window_end & m$period >= ctm_month_add(m$window_end, -(n_months - 1L)), , drop = FALSE]
  ok <- completeness[completeness$complete, c("reporter_iso", "flow_direction", "period")]
  m <- merge(m, ok, by = c("reporter_iso", "flow_direction", "period"))
  used <- stats::aggregate(period ~ reporter_iso + flow_direction, data = unique(m[c("reporter_iso", "flow_direction", "period")]),
                           FUN = length)
  names(used)[3] <- "months_used"
  keep <- used[used$months_used >= min_months, , drop = FALSE]
  dropped <- used[used$months_used < min_months, , drop = FALSE]
  m <- merge(m, keep, by = c("reporter_iso", "flow_direction"))
  if (nrow(m) == 0) stop("ctm_t12m_dyads(): no reporter-flow has ", min_months, " complete months.")
  s <- stats::aggregate(primary_value ~ reporter_iso + partner_iso + flow_direction + cmd_code + months_used + window_end,
                        data = m, FUN = sum)
  s$primary_value <- s$primary_value * n_months / s$months_used
  s$period <- as.integer(period_label)
  s <- s[order(s$reporter_iso, s$flow_direction, s$partner_iso, s$cmd_code), ]
  rownames(s) <- NULL
  attr(s, "dropped") <- dropped
  s
}

# Current vs base import exposure per importer x tech x stage (both from sp_import_exposure()). Current
# rows count only where the importer reported its own monthly imports (import_source == "importer"):
# a mirror row is built from exporters' monthly reports, and China publishes none, so its China share
# would be missing the largest supplier. Columns: base and current imports, China share, top supplier
# and HHI, their changes, current_basis ("own_monthly" or "no_monthly_report"), and flag_china_move
# (|change in China share| >= `min_move` on at least `min_imports_usd` of current imports).
ctm_exposure_compare <- function(base, current, min_move = 0.10, min_imports_usd = 1e8) {
  keys <- c("importer_iso", "tech", "supply_chain")
  b <- base[base$observed %in% TRUE, c(keys, "imports_usd", "china_share", "top_supplier_iso3", "top_supplier_share", "import_hhi")]
  names(b)[-(1:3)] <- paste0(names(b)[-(1:3)], "_base")
  cur <- current[current$observed %in% TRUE, c(keys, "import_source", "imports_usd", "china_share", "top_supplier_iso3",
                                                "top_supplier_share", "import_hhi")]
  names(cur)[-(1:3)] <- paste0(names(cur)[-(1:3)], "_now")
  out <- merge(b, cur, by = keys, all = TRUE)
  own <- out$import_source_now %in% "importer"
  out$current_basis <- ifelse(own, "own_monthly", "no_monthly_report")
  for (v in c("imports_usd", "china_share", "top_supplier_iso3", "top_supplier_share", "import_hhi")) {
    out[[paste0(v, "_now")]][!own] <- NA
  }
  out$import_source_now <- NULL
  out$china_share_change <- out$china_share_now - out$china_share_base
  out$import_hhi_change <- out$import_hhi_now - out$import_hhi_base
  out$imports_growth <- ifelse(out$imports_usd_base > 0, out$imports_usd_now / out$imports_usd_base, NA_real_)
  out$top_supplier_changed <- own & !is.na(out$top_supplier_iso3_base) & !is.na(out$top_supplier_iso3_now) &
    out$top_supplier_iso3_base != out$top_supplier_iso3_now
  out$flag_china_move <- own & !is.na(out$china_share_change) & abs(out$china_share_change) >= min_move &
    out$imports_usd_now >= min_imports_usd
  out <- out[order(out$importer_iso, out$tech, out$supply_chain), ]
  rownames(out) <- NULL
  out
}
