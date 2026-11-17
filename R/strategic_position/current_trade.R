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

# The current_basis values that count as a current measurement: the importer's own monthly reports, and
# the Ember fill for importers without them (ctm_ember_fill()).
CTM_CURRENT_BASES <- c("own_monthly", "ember_fill")

# Current vs base import exposure per importer x tech x stage (both from sp_import_exposure()). Current
# rows count only where current_basis is in CTM_CURRENT_BASES (without the column: import_source ==
# "importer"): a plain mirror row is built from exporters' monthly reports, and China publishes none, so its
# China share would be missing the largest supplier. Columns: base and current imports, China share, top
# supplier and HHI, their changes, current_basis ("own_monthly", "ember_fill" or "no_monthly_report"), and flag_china_move
# (|change in China share| >= `min_move` on at least `min_imports_usd` of current imports).
ctm_exposure_compare <- function(base, current, min_move = 0.10, min_imports_usd = 1e8) {
  keys <- c("importer_iso", "tech", "supply_chain")
  b <- base[base$observed %in% TRUE, c(keys, "imports_usd", "china_share", "top_supplier_iso3", "top_supplier_share", "import_hhi")]
  names(b)[-(1:3)] <- paste0(names(b)[-(1:3)], "_base")
  if (!"current_basis" %in% names(current)) {
    current$current_basis <- ifelse(current$import_source %in% "importer", "own_monthly", "no_monthly_report")
  }
  cur <- current[current$observed %in% TRUE, c(keys, "current_basis", "imports_usd", "china_share", "top_supplier_iso3",
                                                "top_supplier_share", "import_hhi")]
  names(cur)[-(1:3)] <- paste0(names(cur)[-(1:3)], "_now")
  out <- merge(b, cur, by = keys, all = TRUE)
  own <- out$current_basis_now %in% CTM_CURRENT_BASES
  out$current_basis <- ifelse(own, out$current_basis_now, "no_monthly_report")
  out$current_basis_now <- NULL
  for (v in c("imports_usd", "china_share", "top_supplier_iso3", "top_supplier_share", "import_hhi")) {
    out[[paste0(v, "_now")]][!own] <- NA
  }
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

# The columns of an import exposure table that the blend moves: shares and HHI (weighted average of base
# and current), and the two levels (imports_usd, imports_to_gdp).
CTM_BLEND_COLS <- c("china_share", "opposite_bloc_share", "coercive_import_share", "trusted_import_share",
                    "import_hhi", "top_supplier_share", "imports_usd", "imports_to_gdp")

# Import exposure for Strategic Need and the posture, with the current (T12M) measurement blended into the
# base year where the importer reported its own monthly imports:
#   value = (1 - weight) * base + weight * current
# `base` and `current` are sp_import_exposure() tables; current needs current_basis (scripts/38). A cell
# with no own-monthly current value keeps its base row; a cell observed only now takes the current value.
# Shares stay in [0, 1] because each is a convex combination of two shares. Adds exposure_basis
# ("base", "blend", "current_only"), exposure_window_end, exposure_weight and <col>_base / <col>_t12m for
# china_share and coercive_import_share, so every changed value can be traced. weight = 0 returns the base
# values unchanged.
ctm_blend_exposure <- function(base, current, weight = 0.5, cols = CTM_BLEND_COLS) {
  if (!is.numeric(weight) || length(weight) != 1 || is.na(weight) || weight < 0 || weight > 1) {
    stop("ctm_blend_exposure(): weight must be one number in [0, 1].")
  }
  keys <- c("importer_iso", "tech", "supply_chain")
  if (!"current_basis" %in% names(current)) stop("ctm_blend_exposure(): current lacks current_basis (scripts/38).")
  cols <- intersect(cols, intersect(names(base), names(current)))
  cur <- current[current$current_basis %in% CTM_CURRENT_BASES & current$observed %in% TRUE,
                 c(keys, cols, "current_basis", intersect("window_end", names(current))), drop = FALSE]
  names(cur)[match(cols, names(cur))] <- paste0(cols, ".now")
  out <- merge(base, cur, by = keys, all.x = TRUE, sort = FALSE)
  has_now <- !is.na(out[[paste0(cols[1], ".now")]])
  base_obs <- out$observed %in% TRUE
  out$exposure_basis <- ifelse(has_now & base_obs, "blend", "base")
  out$exposure_current_source <- ifelse(has_now, out$current_basis, NA_character_)
  out$current_basis <- NULL
  for (v in intersect(c("china_share", "coercive_import_share"), cols)) {
    out[[paste0(v, "_base")]] <- out[[v]]
    out[[paste0(v, "_t12m")]] <- out[[paste0(v, ".now")]]
  }
  for (v in cols) {
    now <- out[[paste0(v, ".now")]]
    b <- out[[v]]
    blend <- has_now & base_obs & !is.na(b) & !is.na(now)
    out[[v]][blend] <- (1 - weight) * b[blend] + weight * now[blend]
    fill <- has_now & (!base_obs | is.na(b)) & !is.na(now)
    if (weight > 0) out[[v]][fill] <- now[fill]
    out[[paste0(v, ".now")]] <- NULL
  }
  only_now <- has_now & !base_obs & weight > 0
  out$observed[only_now] <- TRUE
  out$exposure_basis[only_now] <- "current_only"
  if (weight == 0) out$exposure_basis <- "base"
  out$exposure_weight <- ifelse(out$exposure_basis == "base", 0, weight)
  out$exposure_window_end <- if ("window_end" %in% names(out)) ifelse(out$exposure_basis == "base", NA, out$window_end) else NA
  out$window_end <- NULL
  out <- out[order(out$importer_iso, out$tech, out$supply_chain), ]
  rownames(out) <- NULL
  tibble::as_tibble(out)
}

# Per-cell trace of the blend for strategic_position_tbl (iso3, tech, supply_chain): exposure_basis,
# exposure_window_end, exposure_weight, china_share_base / _t12m, coercive_import_share_base / _t12m, and
# current_trade_flag ("china_up", "china_down" or NA): the latest 12 months put China's share at least
# `min_move` above or below the base year on at least `min_imports_usd` of blended imports. `blended` is
# ctm_blend_exposure()'s output.
ctm_trade_flags <- function(blended, min_move = 0.10, min_imports_usd = 1e8) {
  keep <- c("importer_iso", "tech", "supply_chain", "exposure_basis", "exposure_current_source", "exposure_window_end", "exposure_weight",
            intersect(c("china_share_base", "china_share_t12m", "coercive_import_share_base", "coercive_import_share_t12m"), names(blended)))
  out <- as.data.frame(blended)[, intersect(keep, names(blended)), drop = FALSE]
  names(out)[1] <- "iso3"
  move <- if (all(c("china_share_base", "china_share_t12m") %in% names(out))) out$china_share_t12m - out$china_share_base else NA_real_
  big <- !is.na(move) & abs(move) >= min_move & blended$imports_usd >= min_imports_usd & out$exposure_basis == "blend"
  out$current_trade_flag <- ifelse(big & move > 0, "china_up", ifelse(big & move < 0, "china_down", NA_character_))
  out
}

# ---- Supplier shares and import partners (partner recommendations) --------------------------------------

# Exporter shares of supply into a balanced panel of importers: those whose own declarations cover the cell
# in both the base year and the T12M window (source_first == source_last == "importer" in
# partnership_strength_build_trade_cells() output). China publishes no monthly data, but the panel's
# imports from China are recorded, so its share is measured. Returns exporter_iso, tech, supply_chain,
# panel_share_base, panel_share_now, panel_share_change, panel_n_importers, panel_usd_base, panel_usd_now.
ctm_panel_supplier_shares <- function(trade_cells) {
  need <- c("exporter_iso", "importer_iso", "tech", "supply_chain", "value_first", "value_last", "source_first", "source_last")
  miss <- setdiff(need, names(trade_cells))
  if (length(miss)) stop("ctm_panel_supplier_shares(): trade_cells lacks ", paste(miss, collapse = ", "), ".")
  p <- trade_cells[trade_cells$source_first %in% "importer" & trade_cells$source_last %in% "importer", need, drop = FALSE]
  p$value_first[is.na(p$value_first)] <- 0
  p$value_last[is.na(p$value_last)] <- 0
  key <- paste(p$tech, p$supply_chain, sep = "\r")
  n_imp <- tapply(p$importer_iso, key, function(x) length(unique(x)))
  ex <- stats::aggregate(cbind(value_first, value_last) ~ exporter_iso + tech + supply_chain, data = p, FUN = sum)
  tot_first <- tapply(ex$value_first, paste(ex$tech, ex$supply_chain, sep = "\r"), sum)
  tot_last <- tapply(ex$value_last, paste(ex$tech, ex$supply_chain, sep = "\r"), sum)
  k <- paste(ex$tech, ex$supply_chain, sep = "\r")
  out <- data.frame(
    exporter_iso = ex$exporter_iso, tech = ex$tech, supply_chain = ex$supply_chain,
    panel_share_base = ifelse(tot_first[k] > 0, ex$value_first / tot_first[k], NA_real_),
    panel_share_now = ifelse(tot_last[k] > 0, ex$value_last / tot_last[k], NA_real_),
    panel_n_importers = as.integer(n_imp[k]), panel_usd_base = as.numeric(tot_first[k]), panel_usd_now = as.numeric(tot_last[k]),
    stringsAsFactors = FALSE
  )
  out$panel_share_change <- out$panel_share_now - out$panel_share_base
  out <- out[order(out$tech, out$supply_chain, -out$panel_share_now), ]
  rownames(out) <- NULL
  out
}

# major_supplier_shares with the Atlas export shares moved by the panel's change in supply share:
#   share = max(0, atlas + weight * panel_share_change), rescaled so each cell's listed total is unchanged
# (the Atlas listing is complete, so shares keep summing to what they did). Additive, not a ratio, so a
# small base cannot produce an outsized move. Cells whose panel has fewer than `min_importers` importers
# or less than `min_panel_usd` of current imports keep their Atlas shares. Other measures pass through.
# Adds share_basis ("atlas" or "atlas_current") and atlas_share (the unmoved value).
ctm_update_export_shares <- function(mss, panel, weight = 0.5, measure = "atlas_export_share",
                                     min_importers = 10L, min_panel_usd = 1e8) {
  if (!is.numeric(weight) || length(weight) != 1 || is.na(weight) || weight < 0 || weight > 1) {
    stop("ctm_update_export_shares(): weight must be one number in [0, 1].")
  }
  mss$atlas_share <- ifelse(mss$measure == measure, mss$world_share, NA_real_)
  mss$share_basis <- ifelse(mss$measure == measure, "atlas", NA_character_)
  ok <- panel[panel$panel_n_importers >= min_importers & panel$panel_usd_now >= min_panel_usd, , drop = FALSE]
  if (weight == 0 || nrow(ok) == 0) return(mss)
  idx <- which(mss$measure == measure & !mss$is_aggregate %in% TRUE)
  key_m <- paste(mss$iso3[idx], mss$tech[idx], mss$supply_chain[idx], sep = "\r")
  key_p <- paste(ok$exporter_iso, ok$tech, ok$supply_chain, sep = "\r")
  chg <- ok$panel_share_change[match(key_m, key_p)]
  cell_ok <- paste(mss$tech[idx], mss$supply_chain[idx], sep = "\r") %in% unique(paste(ok$tech, ok$supply_chain, sep = "\r"))
  old <- mss$world_share[idx]
  new <- ifelse(cell_ok, pmax(0, old + weight * ifelse(is.na(chg), 0, chg)), old)
  cell <- paste(mss$tech[idx], mss$supply_chain[idx], mss$year[idx], sep = "\r")
  tot_old <- tapply(old, cell, sum, na.rm = TRUE)
  tot_new <- tapply(new, cell, sum, na.rm = TRUE)
  scale <- ifelse(tot_new[cell] > 0, tot_old[cell] / tot_new[cell], 1)
  new <- ifelse(cell_ok, new * scale, old)
  mss$world_share[idx] <- new
  if ("value" %in% names(mss)) mss$value[idx] <- ifelse(cell_ok, new, mss$value[idx])
  mss$share_basis[idx] <- ifelse(cell_ok, "atlas_current", "atlas")
  mss
}

# Import partners (reporter_iso, partner_iso, tech, supply_chain, imports_usd) with the T12M table blended in
# for reporters present in `now`: value = (1 - weight) * base + weight * now over the union of their partners
# (a partner missing on one side counts 0 there). Reporters absent from `now` keep their base rows.
ctm_blend_import_partners <- function(base, now, weight = 0.5) {
  keys <- c("reporter_iso", "partner_iso", "tech", "supply_chain")
  if (weight == 0 || is.null(now) || nrow(now) == 0) return(base)
  upd <- unique(now$reporter_iso)
  keep <- base[!base$reporter_iso %in% upd, , drop = FALSE]
  m <- merge(base[base$reporter_iso %in% upd, c(keys, "imports_usd")], now[c(keys, "imports_usd")], by = keys,
             all = TRUE, suffixes = c(".b", ".n"))
  m$imports_usd <- (1 - weight) * dplyr::coalesce(m$imports_usd.b, 0) + weight * dplyr::coalesce(m$imports_usd.n, 0)
  out <- rbind(as.data.frame(keep)[c(keys, "imports_usd")], m[c(keys, "imports_usd")])
  out <- out[out$imports_usd > 0, , drop = FALSE]
  tibble::as_tibble(out)
}

# ---- Ember fill for importers without monthly reports ---------------------------------------------------

# Current import exposure for importers that publish no monthly data, from two growth rates applied to their
# base-year cell imports (T = imports_usd, C = china_share x T):
#   gC  China's growth: Ember's latest-window / base-year exports to the importer over the cell's full-HS6
#       Ember codes (China's own customs data on both sides). Used when those codes carried at least
#       `min_china_cov` of China's base-year exports to the importer in the cell (`china_x24`, China's
#       annual Comtrade exports at HS6).
#   gO  the other suppliers' growth: monthly-reporting exporters' T12M / base-year exports to the importer in
#       the cell (exporter-declared both sides, `others`). Used when those exporters carried at least
#       `min_others_cov` of the base-year non-China imports (x24 x cif_fob / (T - C)).
#   A growth rate needs at least `min_base_usd` in its base year (a small base gives outsized ratios).
#   imports_now = C x gC + (T - C) x gO; china_share = C x gC / imports_now; for each other share s with
#   China in the group (`china_flags`: coercive, opposite, trusted per importer), s_now = ((s x T - C) x gO +
#   C x gC) / imports_now, else s x T x gO / imports_now. A cell where China supplied nothing in the base year
#   needs only gO. HHI and the top supplier are not re-estimated (NA).
# Inputs: base (sp_import_exposure()); targets (importer ISO3s to fill); code_cells (hs6, tech, supply_chain);
# ember (importer_iso, hs6, usd_latest, usd_base: full-HS6 codes only); china_x24 (importer_iso, hs6, usd);
# others (importer_iso, tech, supply_chain, x24, xnow); china_flags (importer_iso, coercive, opposite, trusted).
# Returns one row per filled cell with current_basis "ember_fill" and the fill diagnostics.
ctm_ember_fill <- function(base, targets, code_cells, ember, china_x24, others, china_flags,
                           min_china_cov = 0.5, min_others_cov = 0.5, cif_fob = 1, window_end = NA_integer_,
                           min_base_usd = 1e7) {
  keys <- c("importer_iso", "tech", "supply_chain")
  b <- as.data.frame(base)[base$importer_iso %in% targets & base$observed %in% TRUE & base$imports_usd > 0,
                           c(keys, "imports_usd", "china_share", intersect(c("coercive_import_share", "opposite_bloc_share",
                                                                             "trusted_import_share"), names(base)))]
  if (nrow(b) == 0) return(NULL)
  cc <- unique(code_cells[c("hs6", "tech", "supply_chain")])
  # China's base-year exports by cell, and the part on Ember codes.
  cx <- merge(china_x24, cc, by = "hs6")
  cx$covered <- cx$hs6 %in% ember$hs6
  cx_all <- stats::aggregate(usd ~ importer_iso + tech + supply_chain, data = cx, FUN = sum)
  cx_cov <- stats::aggregate(usd ~ importer_iso + tech + supply_chain, data = cx[cx$covered, , drop = FALSE], FUN = sum)
  names(cx_all)[4] <- "china_x24"
  names(cx_cov)[4] <- "china_x24_cov"
  # Ember growth by cell.
  em <- merge(ember, cc, by = "hs6")
  eg <- stats::aggregate(cbind(usd_latest, usd_base) ~ importer_iso + tech + supply_chain, data = em, FUN = sum)
  eg$china_growth <- ifelse(eg$usd_base >= min_base_usd, eg$usd_latest / eg$usd_base, NA_real_)
  o <- others
  o$others_growth <- ifelse(o$x24 >= min_base_usd, o$xnow / o$x24, NA_real_)
  j <- Reduce(function(x, y) merge(x, y, by = keys, all.x = TRUE),
              list(b, cx_all, cx_cov, eg[c(keys, "china_growth")], o[c(keys, "x24", "others_growth")]))
  j$china_x24_cov[is.na(j$china_x24_cov)] <- 0
  C <- j$china_share * j$imports_usd
  O <- j$imports_usd - C
  j$fill_china_cov <- ifelse(j$china_x24 > 0, j$china_x24_cov / j$china_x24, NA_real_)
  j$fill_others_cov <- ifelse(O > 0, pmin(1, j$x24 * cif_fob / O), NA_real_)
  china_ok <- C == 0 | (!is.na(j$fill_china_cov) & j$fill_china_cov >= min_china_cov & !is.na(j$china_growth))
  others_ok <- O == 0 | (!is.na(j$fill_others_cov) & j$fill_others_cov >= min_others_cov & !is.na(j$others_growth))
  ok <- (!is.na(C) & china_ok & others_ok) %in% TRUE
  gC <- ifelse(C == 0, 1, j$china_growth)
  gO <- ifelse(O == 0, 1, j$others_growth)
  j <- j[ok, , drop = FALSE]
  C <- C[ok]; O <- O[ok]; gC <- gC[ok]; gO <- gO[ok]
  if (nrow(j) == 0) return(NULL)
  now <- C * gC + O * gO
  out <- j[keys]
  out$observed <- TRUE
  out$current_basis <- "ember_fill"
  out$imports_usd <- now
  out$china_share <- ifelse(now > 0, C * gC / now, NA_real_)
  fl <- merge(j[keys], china_flags, by = "importer_iso", all.x = TRUE, sort = FALSE)
  fl <- fl[match(paste(j$importer_iso, j$tech, j$supply_chain), paste(fl$importer_iso, fl$tech, fl$supply_chain)), ]
  flag_of <- c(coercive_import_share = "coercive", opposite_bloc_share = "opposite", trusted_import_share = "trusted")
  for (s in names(flag_of)) {
    if (!s %in% names(j)) next
    f <- fl[[flag_of[[s]]]] %in% TRUE
    s_usd <- j[[s]] * j$imports_usd
    out[[s]] <- ifelse(now > 0, (ifelse(f, (s_usd - C) * gO + C * gC, s_usd * gO)) / now, NA_real_)
    out[[s]] <- pmin(1, pmax(0, out[[s]]))
  }
  out$import_hhi <- NA_real_
  out$top_supplier_iso3 <- NA_character_
  out$top_supplier_share <- NA_real_
  out$fill_china_growth <- gC
  out$fill_others_growth <- gO
  out$fill_china_cov <- j$fill_china_cov
  out$fill_others_cov <- j$fill_others_cov
  out$window_end <- as.integer(window_end)
  rownames(out) <- NULL
  out
}

# ---- Current dyads for the AIB trade network --------------------------------------------------------------

# A dyad table in the scripts/98 layout (reporter_iso, partner_iso, period, flow_direction, cmd_code,
# primary_value) whose `trade_year` rows are as current as the data allow, for scripts/31 (--dyads):
#   - a reporter-flow with T12M data (`t12m`, ctm_t12m_dyads()) takes it;
#   - any other reporter-flow carries its `trade_year` rows forward, and its China rows (China's own
#     exports, and a non-monthly importer's imports from China) are scaled by Ember's growth for that
#     destination and HS6 (`ember_growth`: importer_iso, hs6, growth), when Ember has the pair.
# `pulled_codes` (default: every code in `t12m`) are the codes the monthly pull asked for; a reporter-flow with T12M
# data still carries its other codes forward (the AIB dictionary reaches beyond the pulled basket).
# Rows of other years pass through. Adds current_source: "t12m", "carried" or "carried_ember".
ctm_current_dyads <- function(annual, t12m, ember_growth, trade_year = 2024L, pulled_codes = NULL) {
  cols <- c("reporter_iso", "partner_iso", "period", "flow_direction", "cmd_code", "primary_value")
  miss <- setdiff(cols, names(annual))
  if (length(miss)) stop("ctm_current_dyads(): annual lacks ", paste(miss, collapse = ", "), ".")
  annual <- as.data.frame(annual)[cols]
  annual$flow_direction <- tolower(annual$flow_direction)
  other <- annual[annual$period != trade_year, , drop = FALSE]
  other$current_source <- rep(NA_character_, nrow(other))
  base <- annual[annual$period == trade_year, , drop = FALSE]
  t <- as.data.frame(t12m)[c("reporter_iso", "partner_iso", "flow_direction", "cmd_code", "primary_value")]
  t$flow_direction <- tolower(t$flow_direction)
  t$period <- as.integer(trade_year)
  t$current_source <- rep("t12m", nrow(t))
  has_now <- unique(paste(t$reporter_iso, t$flow_direction))
  if (is.null(pulled_codes)) pulled_codes <- unique(t$cmd_code)
  carried <- base[!(paste(base$reporter_iso, base$flow_direction) %in% has_now & base$cmd_code %in% pulled_codes), , drop = FALSE]
  carried$current_source <- rep("carried", nrow(carried))
  dest <- ifelse(carried$reporter_iso == "CHN" & carried$flow_direction == "export", carried$partner_iso,
                 ifelse(carried$partner_iso == "CHN" & carried$flow_direction == "import", carried$reporter_iso, NA_character_))
  g <- ember_growth$growth[match(paste(dest, carried$cmd_code), paste(ember_growth$importer_iso, ember_growth$hs6))]
  scale <- !is.na(dest) & !is.na(g)
  carried$primary_value[scale] <- carried$primary_value[scale] * g[scale]
  carried$current_source[scale] <- "carried_ember"
  out <- rbind(other, carried, t[c(cols, "current_source")])
  rownames(out) <- NULL
  out
}
