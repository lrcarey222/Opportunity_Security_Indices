# US trade restrictiveness by technology x supply-chain stage (docs/us_trade_restrictiveness.md): the Trade War
# Tracker's Trade Restrictiveness Index (Waugh, "How Restrictive is U.S. Trade Policy?",
# github.com/tradewartracker/how-restrictive-us-trade) on the project's HS6 basket, with antidumping and
# countervailing duty deposits added, compared with the fence of the recommended posture (Step 4 of the
# decision chain: policy alignment; the measure never enters the posture engine).
#
# A good is one HS10 line from one partner. Its weight w is its import value in the weight year and its tariff
# tau (percent) the calculated duty over customs value in the data month:
#   TRI  = sqrt(sum(w * tau^2) / sum(w))   (the uniform tariff with the same welfare cost)
#   mean = sum(w * tau) / sum(w)
#
# Pure: no IO. scripts/28_build_us_trade_restrictiveness.R reads the tracker's files and writes the outputs.
# Partner groups come from sp_coercive_pairs() (R/strategic_position/supply_structure.R). Needs dplyr.

TRI_ROW_CODE <- "ROW"
TRI_GROUPS <- c("All suppliers", "China", "Coercive", "Trusted", "Nonaligned", "Unassigned (rest of world)")

# TRI and mean tariff of goods with weights `w` and tariffs `tau` (percent). Goods with no weight or no tariff
# are left out; with no weight left both are NA.
tri_index <- function(w, tau) {
  ok <- !is.na(w) & !is.na(tau) & w > 0
  sw <- sum(w[ok])
  if (sw <= 0) return(c(tri = NA_real_, mean = NA_real_))
  c(tri = sqrt(sum(w[ok] * tau[ok]^2) / sw), mean = sum(w[ok] * tau[ok]) / sw)
}

# The `n` months ending at `month` ("YYYY-MM"), oldest first, and the same calendar months of `year`.
tri_month_window <- function(month, n = 3) {
  d <- as.Date(paste0(month, "-01"))
  rev(format(seq(d, by = "-1 month", length.out = n), "%Y-%m"))
}
tri_same_months <- function(months, year) paste0(year, substr(months, 5, 7))

# One row per (cty_code, hs10) from the tracker's monthly entity files, stacked as `rows` (cty_code, hs10,
# time "YYYY-MM", value, duty; the all-country total under cty_code "TOTAL"): w and w_duty, the import value
# and duty of `weight_year`, and v_<m> / d_<m> for each of `months`. A rest-of-world row per line holds TOTAL
# less the named entities (floored at zero), so the panel covers every US import.
tri_build_panel <- function(rows, weight_year, months) {
  need <- c("cty_code", "hs10", "time", "value", "duty")
  miss <- setdiff(need, names(rows))
  if (length(miss) > 0) stop("tri_build_panel(): rows lack ", paste(miss, collapse = ", "), ".")
  rows <- rows[!is.na(rows$value), need]
  rows$duty[is.na(rows$duty)] <- 0
  out <- rows |>
    dplyr::filter(substr(.data$time, 1, 4) == as.character(weight_year)) |>
    dplyr::group_by(.data$cty_code, .data$hs10) |>
    dplyr::summarise(w = sum(.data$value), w_duty = sum(.data$duty), .groups = "drop")
  for (m in months) {
    a <- rows |>
      dplyr::filter(.data$time == m) |>
      dplyr::group_by(.data$cty_code, .data$hs10) |>
      dplyr::summarise(v = sum(.data$value), d = sum(.data$duty), .groups = "drop")
    names(a)[3:4] <- paste0(c("v_", "d_"), m)
    out <- dplyr::full_join(out, a, by = c("cty_code", "hs10"))
  }
  out <- as.data.frame(out)
  num <- setdiff(names(out), c("cty_code", "hs10"))
  out[num][is.na(out[num])] <- 0

  total <- out[out$cty_code == "TOTAL", ]
  named <- out[out$cty_code != "TOTAL", ]
  summed <- stats::aggregate(named[num], by = list(hs10 = named$hs10), FUN = sum)
  row <- total[c("hs10", num)]
  idx <- match(row$hs10, summed$hs10)
  for (col in num) row[[col]] <- pmax(0, row[[col]] - ifelse(is.na(idx), 0, summed[[col]][idx]))
  row$cty_code <- TRI_ROW_CODE
  out <- rbind(named, row[c("cty_code", "hs10", num)])
  out[order(out$cty_code, out$hs10), , drop = FALSE]
}

# Each panel good with weight (w > 0) and its tariffs (percent):
#   tau_collected  calculated duty / value in `month`. A line with no trade that month takes the partner's
#                  HS8 rate, then its HS6 rate, that month (HTS lines were re-coded after the weight year,
#                  e.g. lithium-ion cells 8507.60.0020); with nothing at HS6 it is NA ("absent").
#   match          "HS10", "HS8", "HS6" or "absent".
#   tau_universe   the tracker's convention: a line with no trade that month counts as tariff-free.
#   tau_base       the line's own weight-year rate.
tri_line_rates <- function(panel, month) {
  vm <- paste0("v_", month)
  dm <- paste0("d_", month)
  if (!all(c(vm, dm) %in% names(panel))) stop("tri_line_rates(): panel has no columns for ", month, ".")
  p <- panel
  p$hs8 <- substr(p$hs10, 1, 8)
  p$hs6 <- substr(p$hs10, 1, 6)
  rate_at <- function(level) {
    a <- data.frame(cty_code = p$cty_code, key = p[[level]], v = p[[vm]], d = p[[dm]]) |>
      dplyr::group_by(.data$cty_code, .data$key) |>
      dplyr::summarise(v = sum(.data$v), d = sum(.data$d), .groups = "drop") |>
      dplyr::filter(.data$v > 0)
    stats::setNames(100 * a$d / a$v, paste(a$cty_code, a$key))
  }
  g <- p[p$w > 0, , drop = FALSE]
  r10 <- unname(rate_at("hs10")[paste(g$cty_code, g$hs10)])
  r8 <- unname(rate_at("hs8")[paste(g$cty_code, g$hs8)])
  r6 <- unname(rate_at("hs6")[paste(g$cty_code, g$hs6)])
  g$match <- ifelse(!is.na(r10), "HS10", ifelse(!is.na(r8), "HS8", ifelse(!is.na(r6), "HS6", "absent")))
  g$tau_collected <- ifelse(!is.na(r10), r10, ifelse(!is.na(r8), r8, r6))
  g$tau_universe <- ifelse(g[[vm]] > 0, 100 * g[[dm]] / pmax(g[[vm]], 1e-300), 0)
  g$tau_base <- 100 * g$w_duty / g$w
  g
}

# Split partners out of the rest-of-world rows of `goods` (tri_line_rates()), from `carve`: cty_code, hs10,
# imports and duty in the weight year (the Trade War Tracker's own partner files). Each carved line takes its
# own weight-year rate (tau_base) and ROW's rates for the data month; ROW keeps what is left. Where the carved
# partners' imports of a line exceed ROW's weight they are scaled down to it, so total weight is unchanged.
tri_split_rest_of_world <- function(goods, carve) {
  carve <- carve[carve$imports > 0 & carve$hs10 %in% goods$hs10[goods$cty_code == TRI_ROW_CODE], , drop = FALSE]
  if (nrow(carve) == 0) return(goods)
  row_i <- which(goods$cty_code == TRI_ROW_CODE)
  row_w <- stats::setNames(goods$w[row_i], goods$hs10[row_i])
  tot <- tapply(carve$imports, carve$hs10, sum)
  scale <- stats::setNames(pmin(1, as.numeric(row_w[names(tot)]) / as.numeric(tot)), names(tot))
  carve$w <- carve$imports * unname(scale[carve$hs10])
  new <- goods[row_i[match(carve$hs10, goods$hs10[row_i])], , drop = FALSE]
  new$cty_code <- carve$cty_code
  new$w <- carve$w
  new$tau_base <- 100 * carve$duty / carve$imports
  taken <- tapply(carve$w, carve$hs10, sum)
  goods$w[row_i] <- goods$w[row_i] - ifelse(is.na(taken[goods$hs10[row_i]]), 0, as.numeric(taken[goods$hs10[row_i]]))
  out <- rbind(goods, new)
  out[out$w > 1e-6, , drop = FALSE]
}

# Supplier group of each partner for `reporter`, from sp_coercive_pairs() rows: "Coercive", "Trusted" or
# "Nonaligned"; the rest-of-world residual is "Unassigned (rest of world)". `blocs` names partner codes that
# are blocs rather than countries (the EU in the statutory panel) and the group to give them.
tri_partner_groups <- function(iso3, pairs, reporter = "USA", blocs = c(EU = "Trusted")) {
  p <- pairs[pairs$reporter_iso == reporter, , drop = FALSE]
  co <- stats::setNames(p$coercive, p$partner_iso)
  tr <- stats::setNames(p$trusted, p$partner_iso)
  out <- ifelse(co[iso3] %in% TRUE, "Coercive", ifelse(tr[iso3] %in% TRUE, "Trusted", "Nonaligned"))
  out[iso3 %in% names(blocs)] <- unname(blocs[iso3[iso3 %in% names(blocs)]])
  out[iso3 == TRI_ROW_CODE] <- "Unassigned (rest of world)"
  unname(out)
}

# TRUE where an HS10 code starts with one of `codes`; each code (4, 6, 8 or 10 digits, punctuation ignored)
# matches on its own length, the way the tracker's product masks do.
tri_prefix_match <- function(hs10, codes) {
  codes <- unique(gsub("[^0-9]", "", as.character(codes)))
  codes <- codes[nzchar(codes)]
  out <- logical(length(hs10))
  for (n in unique(nchar(codes))) out <- out | substr(hs10, 1, n) %in% codes[nchar(codes) == n]
  out
}

# Section 232 product set of each HS10 line, as the Trade War Tracker engine models them after the 6 April 2026
# metals restructuring, in its precedence: autos and parts (they keep their deal rates), then metals (annexes
# I-A, I-B and III less the annex II removals; copper sits in annex I-A), then copper, then timber.
# `lists`: named vectors of HTS codes (autos, metals_ia, metals_ib, metals_iii, metals_removed, copper, timber).
# Note the tracker applies the auto-parts list to every good in a listed provision, so all lithium-ion
# batteries (8507.60) and control boards (8537.10) carry the autos tag.
tri_section232_tags <- function(hs10, lists) {
  m <- function(k) tri_prefix_match(hs10, if (is.null(lists[[k]])) character() else lists[[k]])
  autos <- m("autos")
  metals <- (m("metals_ia") | m("metals_ib") | m("metals_iii")) & !m("metals_removed")
  ifelse(autos, "232 autos & parts", ifelse(metals, "232 metals", ifelse(m("copper"), "232 copper",
    ifelse(m("timber"), "232 timber", "none"))))
}

# AD/CVD deposit rate (percent) of each good from `orders` (data/reference/adcvd_orders.csv: family, iso3,
# in_force, hts (codes separated by ";"), ad_rate, cvd_rate): the AD plus CVD rate for exporters without their
# own rate. Only in-force rows count. Where several orders cover a line (China's 2012 and 2015 solar orders),
# the highest combined rate applies. Returns adcvd (0 where none) and adcvd_family.
tri_adcvd_rates <- function(hs10, iso3, orders) {
  o <- orders[toupper(as.character(orders$in_force)) == "TRUE", , drop = FALSE]
  rate <- numeric(length(hs10))
  family <- rep("", length(hs10))
  if (nrow(o) == 0) return(data.frame(adcvd = rate, adcvd_family = family))
  num <- function(x) { x <- suppressWarnings(as.numeric(x)); ifelse(is.na(x), 0, x) }
  exp <- do.call(rbind, lapply(seq_len(nrow(o)), function(i) {
    codes <- gsub("[^0-9]", "", strsplit(as.character(o$hts[i]), ";", fixed = TRUE)[[1]])
    codes <- codes[nzchar(codes)]
    if (length(codes) == 0) return(NULL)
    data.frame(iso3 = o$iso3[i], prefix = codes, family = o$family[i], rate = num(o$ad_rate[i]) + num(o$cvd_rate[i]))
  }))
  exp <- exp[order(-exp$rate), , drop = FALSE]
  exp <- exp[!duplicated(paste(exp$iso3, exp$prefix)), , drop = FALSE]
  for (n in sort(unique(nchar(exp$prefix)))) {
    e <- exp[nchar(exp$prefix) == n, , drop = FALSE]
    k <- match(paste(iso3, substr(hs10, 1, n)), paste(e$iso3, e$prefix))
    hit <- !is.na(k) & e$rate[k] > rate
    hit[is.na(hit)] <- FALSE
    rate[hit] <- e$rate[k[hit]]
    family[hit] <- e$family[k[hit]]
  }
  data.frame(adcvd = rate, adcvd_family = family)
}

# Line tariff with AD/CVD deposits, capped at `cap` (a prohibitive rate). A covered line with no trade in the
# data month is absent because of the order, so it enters at its weight-year rate plus the deposit.
tri_adcvd_tau <- function(tau_collected, tau_base, adcvd, cap = 100) {
  pmin(ifelse(is.na(tau_collected), tau_base, tau_collected) + adcvd, cap)
}

# Basket cells from energy_hs6_master.csv (tech, stage, hs6, sub_sector, excluded, legacy_hs): the in-basket
# rows of current HS (legacy bridge codes left out). Deployment has no traded goods of its own, so it takes
# the technology's Assembly basket, the goods the build-out imports. A code sits in one stage per technology.
tri_basket_cells <- function(master, techs, deployment_from = "Assembly") {
  is_true <- function(x) toupper(as.character(x)) %in% c("TRUE", "T", "1")
  m <- master[!is_true(master$excluded) & !is_true(master$legacy_hs) & master$tech %in% techs, , drop = FALSE]
  cells <- unique(m[c("tech", "stage", "hs6", "sub_sector")])
  cells <- cells[!duplicated(cells[c("tech", "stage", "hs6")]), , drop = FALSE]
  dup <- duplicated(cells[c("tech", "hs6")])
  if (any(dup)) {
    stop("tri_basket_cells(): code(s) in two stages of one technology: ",
         paste(utils::head(paste(cells$tech[dup], cells$hs6[dup]), 5), collapse = ", "), ".")
  }
  dep <- cells[cells$stage == deployment_from, , drop = FALSE]
  dep$stage <- rep("Deployment", nrow(dep))
  out <- rbind(cells, dep)
  rownames(out) <- NULL
  out
}

# TRI by basket cell and supplier group for tariff column `tau` of `goods` (columns hs6, iso3, group, w,
# s232, covered and `tau`). `keep` names a logical column of goods to use (the others still count in the
# cell's total weight, so weight_coverage shows how much is used). `cap` marks lines at the prohibitive cap.
# Groups: all suppliers, China, and each of goods$group.
tri_by_cell <- function(goods, cells, tau, measure, keep = NULL, cap = Inf) {
  g <- merge(goods, unique(cells[c("tech", "stage", "hs6")]), by = "hs6")
  subsets <- list(`All suppliers` = rep(TRUE, nrow(g)), China = g$iso3 %in% "CHN")
  for (grp in setdiff(TRI_GROUPS, c("All suppliers", "China"))) subsets[[grp]] <- g$group %in% grp
  out <- list()
  for (grp in names(subsets)) {
    s <- g[subsets[[grp]], , drop = FALSE]
    if (nrow(s) == 0) next
    key <- paste(s$tech, s$stage, sep = "\r")
    for (k in unique(key)) {
      d <- s[key == k, , drop = FALSE]
      used <- if (is.null(keep)) rep(TRUE, nrow(d)) else d[[keep]] %in% TRUE
      u <- d[used & !is.na(d[[tau]]), , drop = FALSE]
      w <- u$w
      t <- u[[tau]]
      sq <- w * t^2
      sw <- sum(w)
      is232 <- u$s232 != "none"
      cov <- u$covered %in% TRUE
      idx <- tri_index(w, t)
      out[[length(out) + 1]] <- data.frame(
        tech = d$tech[1], stage = d$stage[1], supplier_group = grp, measure = measure,
        tri = unname(idx["tri"]), mean = unname(idx["mean"]),
        w_used = sw, w_total = sum(d$w), weight_coverage = if (sum(d$w) > 0) sw / sum(d$w) else NA_real_,
        n_lines = nrow(u),
        s232_weight_share = if (sw > 0) sum(w[is232]) / sw else NA_real_,
        s232_tri_sq_share = if (sum(sq) > 0) sum(sq[is232]) / sum(sq) else NA_real_,
        tri_excl_232 = unname(tri_index(w[!is232], t[!is232])["tri"]),
        adcvd_weight_share = if (sw > 0) sum(w[cov]) / sw else NA_real_,
        adcvd_tri_sq_share = if (sum(sq) > 0) sum(sq[cov]) / sum(sq) else NA_real_,
        at_cap_weight_share = if (sw > 0) sum(w[t >= cap - 1e-9]) / sw else NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, out)
}

# Economy-wide TRI and mean of all goods by supplier group (same groups as tri_by_cell()).
tri_benchmarks <- function(goods, tau, measure, keep = NULL) {
  g <- if (is.null(keep)) goods else goods[goods[[keep]] %in% TRUE, , drop = FALSE]
  subsets <- list(`All suppliers` = rep(TRUE, nrow(g)), China = g$iso3 %in% "CHN")
  for (grp in setdiff(TRI_GROUPS, c("All suppliers", "China"))) subsets[[grp]] <- g$group %in% grp
  do.call(rbind, lapply(names(subsets), function(grp) {
    s <- g[subsets[[grp]], , drop = FALSE]
    idx <- tri_index(s$w, s[[tau]])
    data.frame(measure = measure, supplier_group = grp, tri = unname(idx["tri"]), mean = unname(idx["mean"]))
  }))
}

# Share of each cell's weight matched at HS10, HS8, HS6 or absent (all suppliers).
tri_match_shares <- function(goods, cells) {
  g <- merge(goods, unique(cells[c("tech", "stage", "hs6")]), by = "hs6")
  a <- stats::aggregate(w ~ tech + stage + match, data = g, FUN = sum)
  tot <- stats::aggregate(w ~ tech + stage, data = g, FUN = sum)
  names(tot)[3] <- "w_cell"
  a <- merge(a, tot, by = c("tech", "stage"))
  a$share <- a$w / a$w_cell
  w <- stats::reshape(a[c("tech", "stage", "match", "share")], idvar = c("tech", "stage"), timevar = "match", direction = "wide")
  names(w) <- sub("^share\\.", "match_share_", names(w))
  for (k in c("HS10", "HS8", "HS6", "absent")) {
    col <- paste0("match_share_", k)
    if (!col %in% names(w)) w[[col]] <- 0
    w[[col]][is.na(w[[col]])] <- 0
  }
  w
}

# Import volume by cell and group: the sum over `now` months over the same sum for `base` months (HS6 sums,
# so HTS re-coding does not matter). `panel` needs hs10, iso3, group and v_<month> columns.
tri_volume_ratios <- function(panel, cells, now, base) {
  p <- panel
  p$hs6 <- substr(p$hs10, 1, 6)
  p$v_now <- rowSums(as.matrix(p[paste0("v_", now)]))
  p$v_base <- rowSums(as.matrix(p[paste0("v_", base)]))
  g <- merge(p[c("hs6", "iso3", "group", "v_now", "v_base")], unique(cells[c("tech", "stage", "hs6")]), by = "hs6")
  subsets <- list(`All suppliers` = rep(TRUE, nrow(g)), China = g$iso3 %in% "CHN")
  for (grp in setdiff(TRI_GROUPS, c("All suppliers", "China"))) subsets[[grp]] <- g$group %in% grp
  do.call(rbind, lapply(names(subsets), function(grp) {
    s <- g[subsets[[grp]], , drop = FALSE]
    if (nrow(s) == 0) return(NULL)
    a <- stats::aggregate(cbind(v_now, v_base) ~ tech + stage, data = s, FUN = sum)
    a$supplier_group <- grp
    a$volume_ratio <- ifelse(a$v_base > 0, a$v_now / a$v_base, NA_real_)
    a
  }))
}

# The HS6 codes behind each cell's TRI (all suppliers): each code's pooled partner rate in `month`
# (HS6 x partner duty / value), weight-year weights, and its share of the cell's sum of w * tau^2.
# Codes under `min_share` of it are dropped; at most `top` per cell.
tri_cell_drivers <- function(panel, cells, month, s232_by_hs6 = NULL, min_share = 0.05, top = 3) {
  vm <- paste0("v_", month)
  dm <- paste0("d_", month)
  p <- panel
  p$hs6 <- substr(p$hs10, 1, 6)
  a <- stats::aggregate(cbind(w = p$w, v = p[[vm]], d = p[[dm]]), by = list(cty_code = p$cty_code, hs6 = p$hs6), FUN = sum)
  a <- a[a$w > 0 & a$v > 0, ]
  a$tau <- 100 * a$d / a$v
  a$sq <- a$w * a$tau^2
  h <- stats::aggregate(cbind(w, sq) ~ hs6, data = a, FUN = sum)
  h$tri_hs6 <- sqrt(h$sq / h$w)
  c6 <- merge(unique(cells[c("tech", "stage", "hs6", "sub_sector")]), h, by = "hs6")
  out <- do.call(rbind, lapply(split(c6, paste(c6$tech, c6$stage)), function(d) {
    d$w_share <- d$w / sum(d$w)
    d$sq_share <- if (sum(d$sq) > 0) d$sq / sum(d$sq) else NA_real_
    d <- d[order(-d$sq_share), , drop = FALSE]
    utils::head(d[!is.na(d$sq_share) & d$sq_share >= min_share, , drop = FALSE], top)
  }))
  if (!is.null(s232_by_hs6)) out$s232 <- unname(s232_by_hs6[out$hs6])
  rownames(out) <- NULL
  out[c("tech", "stage", "hs6", "sub_sector", "w_share", "tri_hs6", "sq_share", intersect("s232", names(out)))]
}

# Verdict of each cell against its recommended choice (config/us_trade_restrictiveness.yml thresholds):
#   secure                          coercive TRI >= high and trusted TRI < open ("In line"); coercive under
#                                   `open` (or none) is "No tariff fence", in between "Partial fence".
#   preserve, catch_up, partner,    the gate stays open to trusted partners: trusted TRI < open, else
#   accelerate                      "Taxes trusted partners".
#   watch                           a low fence: all-supplier TRI < open, else "Fence above recommendation".
#   anything else                   "Not assessed".
# A cell with no trusted-supplier imports counts as trusted TRI 0.
tri_posture_verdict <- function(choice, tri_all, tri_trusted, tri_coercive, open = 5, high = 15) {
  tr <- ifelse(is.na(tri_trusted), 0, tri_trusted)
  f1 <- function(x) formatC(x, format = "f", digits = 1)
  band <- function(x) ifelse(x < open, "low", ifelse(x < high, "moderate", "high"))
  verdict <- rep("Not assessed", length(choice))
  reason <- rep("No posture for this cell.", length(choice))
  sec <- choice %in% "secure"
  sec_ok <- sec & !is.na(tri_coercive) & tri_coercive >= high & tr < open
  sec_none <- sec & !sec_ok & (is.na(tri_coercive) | tri_coercive < open)
  sec_part <- sec & !sec_ok & !sec_none
  verdict[sec_ok] <- "In line"
  reason[sec_ok] <- "High tariff on the coercive supplier, low on trusted producers."
  verdict[sec_none] <- "No tariff fence"
  reason[sec_none] <- paste0("Coercive suppliers pay ", f1(tri_coercive[sec_none]), "%: the high fence is not a tariff fence.")
  verdict[sec_part] <- "Partial fence"
  reason[sec_part] <- paste0("Coercive suppliers pay ", f1(tri_coercive[sec_part]), "%, below the high band.")
  wat <- choice %in% "watch" & !is.na(tri_all)
  verdict[wat] <- ifelse(tri_all[wat] < open, "In line", "Fence above recommendation")
  reason[wat] <- ifelse(tri_all[wat] < open, "Low fence and low tariffs.",
    paste0("Low fence recommended; the cell's TRI is ", f1(tri_all[wat]), "% (", band(tri_all[wat]), ")."))
  gate <- choice %in% c("preserve", "catch_up", "partner", "accelerate") & !is.na(tri_all)
  verdict[gate] <- ifelse(tr[gate] < open, "In line", "Taxes trusted partners")
  reason[gate] <- ifelse(tr[gate] < open, paste0("Trusted partners pay ", f1(tr[gate]), "%; any fence falls on others."),
    paste0("Trusted partners pay ", f1(tr[gate]), "% (", band(tr[gate]), "); the posture keeps the gate open to them."))
  data.frame(verdict = verdict, verdict_reason = reason, stringsAsFactors = FALSE)
}
