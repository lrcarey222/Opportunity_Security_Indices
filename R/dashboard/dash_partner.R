# Dashboard L4: Friendshore (FS) and Opportunity (PO) dyad cells, their best-five summaries and
# the Partnership Strength Index (PSI) for any component weights and climate penalties (pure; no
# IO).
#
# The dyad cells are arrays [reporter, partner, cell] over the world economies U and the 33
# (technology, stage) cells. Exact port of partnership_strength_build_friendshore_dyads() and
# partnership_strength_build_opportunity_dyads() from their stored components:
#
#   FS (reporter r = importer rating partner p as a supplier):
#     raw   = weighted mean of imp_trade[r,p,c], econ_opp_raw[r,c], es_need[r,c], eo_partner[p,c],
#             outbound[r,p,c], geo_alignment[r,p] and 1 - chinese_parent_exposure[p,c] over the
#             components present (weights renormalised)
#     outbound[r,p,c] is the IMF direct-investment position outbound[r,p] in every cell, except in
#             the cells CM-FDI covers (outbound_source "cm_fdi": Solar, Wind, Batteries and Electric
#             Vehicles Midstream), where it is the CM-FDI investment tie outbound_cmfdi[r,p,k] of
#             the k-th such cell (NA where reporter r's parents have no project in the cell, so the
#             component drops out, as in partnership_strength_apply_cm_fdi())
#     chinese_parent_exposure[p,c] is the share of partner p's foreign-invested manufacturing in
#             the cell owned by China-headquartered parents (CM-FDI), a risk scored 1 - share;
#             NA (dropped, never 0) outside the CM-FDI cells and below two projects
#     adj   = max(0, raw - ((1 - g_c) cpi_r k + (1 - g_c) cpi_p k)),  k = 0.10 x greenness
#     index = median S-curve of adj over every world dyad of the cell
#   PO (reporter r = exporter rating market p):
#     raw   = weighted mean of trade[r,p,c], econ_opp_index[r,c], partner_es_need[p,c]
#     adj   = max(0, raw - (1 - g_c) cpi_p k'),  k' = 0.05 x greenness
#     index = median S-curve of adj over every world dyad of the cell
#
# The weighted mean accumulates the components in the pipeline's order with the same operations,
# so the arrays equal the pipeline's bit for bit. Best-five: mean of a dyad's five highest cells
# (over the non-fossil cells when fossil technologies are excluded). PSI (dyad and partner views):
# build_partnership_strength_dyads() / build_partnership_strength_viewpoints().
#
# bundle$partner (scripts/85): U (ISO3), names (partner display names by U), sovereign (by U),
# cells = tibble(cell_id, tech, supply_chain); fs = list(imp_trade [U,U,C], econ_opp_raw [U,C],
# es_need [U,C], eo_partner [U,C], outbound [U,U], outbound_source [C] ("imf_dip" / "cm_fdi"; absent
# = IMF everywhere), outbound_cmfdi [U,U,K] (the K "cm_fdi" cells in cell order), cpe [U,C]
# (chinese_parent_exposure by partner; absent = none), geo [U,U], mask [U,U,C], cpi [U], gh [C]);
# po = list(trade [U,U,C], econ_opp_index [U,C], partner_es_need [U,C], mask [U,U,C], cpi [U],
# gh [C]); dp = tibble(partner_iso, development, imputed_weight_share); top5_share = list(fs [U,U],
# po [U,U]) (the baseline best-five imputed share per dyad, NA when the build has none);
# psi = list(coalition_reporters, tau).

dash_bcast_r <- function(m, U) array(m[, rep(seq_len(ncol(m)), each = U)], c(U, U, ncol(m)))   # [r,c] -> [r,p,c]
dash_bcast_p <- function(m, U) array(rep(as.vector(m), each = U), c(U, U, ncol(m)))         # [p,c] -> [r,p,c]
dash_bcast_rp <- function(m, C) array(rep(as.vector(m), times = C), c(nrow(m), ncol(m), C))  # [r,p] -> [r,p,c]

# Weighted mean over components present, in list order (partnership_strength_row_weighted_mean()).
# Components with weight 0 add exactly 0 there and are skipped here.
dash_row_wmean <- function(values, weights) {
  num <- 0
  den <- 0
  for (k in seq_along(values)) {
    wk <- weights[[k]]
    if (is.null(values[[k]]) || is.null(wk) || is.na(wk) || wk == 0) next
    x <- values[[k]]
    ok <- !is.na(x)
    num <- num + ifelse(ok, x * wk, 0)
    den <- den + ok * wk
  }
  out <- num / den
  out[!(den > 0)] <- NA_real_
  out
}

# Median S-curve of each cell's slice over the dyads in mask.
dash_scurve_cells <- function(adj, mask) {
  out <- adj
  out[!mask] <- NA_real_
  for (c in seq_len(dim(adj)[3])) {
    v <- out[, , c]
    if (any(!is.na(v))) out[, , c] <- median_scurve(as.vector(v)) else out[, , c] <- NA_real_
  }
  out
}

# Which measure each cell's outbound component holds: "imf_dip" (the IMF position) or "cm_fdi" (the
# CM-FDI investment tie); a bundle without the vector predates CM-FDI (IMF everywhere).
dash_fs_outbound_source <- function(fs, C = length(fs$gh)) {
  if (is.null(fs$outbound_source)) rep("imf_dip", C) else as.character(fs$outbound_source)
}

# Friendshore's outbound-investment component as an array [r, p, c]: the IMF position outbound[r, p]
# in the "imf_dip" cells, the CM-FDI tie outbound_cmfdi[r, p, k] in the k-th "cm_fdi" cell. NULL
# when the bundle has neither.
dash_fs_outbound <- function(fs, U, C) {
  cm <- which(dash_fs_outbound_source(fs, C) == "cm_fdi")
  if (is.null(fs$outbound) && !length(cm)) return(NULL)
  out <- if (is.null(fs$outbound)) array(NA_real_, c(U, U, C)) else dash_bcast_rp(fs$outbound, C)
  if (length(cm)) out[, , cm] <- fs$outbound_cmfdi
  out
}

# The Friendshore score of a risk component (chinese_parent_exposure): 1 - share
# (partnership_strength_friendshore_score()).
dash_fs_risk_score <- function(x) 1 - x

dash_fs_adj <- function(fs, w, coef) {
  U <- length(fs$cpi)
  C <- length(fs$gh)
  comps <- list(
    imp_trade_index = fs$imp_trade,
    econ_opp_raw = if (isTRUE(w$econ_opp_raw > 0)) dash_bcast_r(fs$econ_opp_raw, U) else NULL,
    es_need = if (isTRUE(w$es_need > 0)) dash_bcast_r(fs$es_need, U) else NULL,
    eo_partner = if (isTRUE(w$eo_partner > 0)) dash_bcast_p(fs$eo_partner, U) else NULL,
    outbound_index = if (isTRUE(w$outbound_index > 0)) dash_fs_outbound(fs, U, C) else NULL,
    geo_alignment = if (isTRUE(w$geo_alignment > 0) && !is.null(fs$geo) && any(!is.na(fs$geo))) dash_bcast_rp(fs$geo, C) else NULL,
    chinese_parent_exposure = if (isTRUE(w$chinese_parent_exposure > 0) && !is.null(fs$cpe)) dash_fs_risk_score(dash_bcast_p(fs$cpe, U)) else NULL
  )
  wts <- lapply(names(comps), function(k) w[[k]])
  raw <- dash_row_wmean(comps, wts)
  rm(comps)
  one_g <- 1 - fs$gh
  pen_r <- array(outer(fs$cpi, one_g, function(cpi, g1) (g1 * cpi) * coef), c(U, 1, C))[, rep(1L, U), , drop = FALSE]
  pen_p <- array(rep(outer(fs$cpi, one_g, function(cpi, g1) (g1 * cpi) * coef), each = U), c(U, U, C))
  adj <- pmax(raw - (pen_r + pen_p), 0)
  list(raw = raw, adj = adj)
}

dash_po_adj <- function(po, w, coef) {
  U <- length(po$cpi)
  C <- length(po$gh)
  comps <- list(
    trade_index = po$trade,
    econ_opp_index = dash_bcast_r(po$econ_opp_index, U),
    partner_es_need = dash_bcast_p(po$partner_es_need, U)
  )
  wts <- lapply(names(comps), function(k) w[[k]])
  raw <- dash_row_wmean(comps, wts)
  rm(comps)
  pen <- array(rep(outer(po$cpi, 1 - po$gh, function(cpi, g1) (g1 * cpi) * coef), each = U), c(U, U, C))
  adj <- pmax(raw - pen, 0)
  list(raw = raw, adj = adj)
}

# Friendshore index array for the scenario.
dash_fs_cells <- function(bundle, params) {
  fs <- bundle$partner$fs
  a <- dash_fs_adj(fs, params$partner$fs_weights, params$partner$fs_penalty)
  idx <- dash_scurve_cells(a$adj, fs$mask)
  dimnames(idx) <- list(bundle$partner$U, bundle$partner$U, bundle$partner$cells$cell_id)
  idx
}

# Opportunity index array for the scenario.
dash_po_cells <- function(bundle, params) {
  po <- bundle$partner$po
  a <- dash_po_adj(po, params$partner$po_weights, params$partner$po_penalty)
  idx <- dash_scurve_cells(a$adj, po$mask)
  dimnames(idx) <- list(bundle$partner$U, bundle$partner$U, bundle$partner$cells$cell_id)
  idx
}

dash_partner_cells <- function(bundle, params, pillars = NULL) {
  list(fs_index = dash_fs_cells(bundle, params), po_index = dash_po_cells(bundle, params))
}

# Mean of the k largest non-missing values of each dyad over the cells kept; NA without any.
dash_topk_mean <- function(A, k, keep_cells) {
  U1 <- dim(A)[1]
  U2 <- dim(A)[2]
  M <- matrix(A[, , keep_cells, drop = FALSE], nrow = U1 * U2)
  n <- rowSums(!is.na(M))
  W <- M
  W[is.na(W)] <- -Inf
  top <- matrix(NA_real_, nrow(M), k)
  rows <- seq_len(nrow(W))
  for (j in seq_len(k)) {
    col <- max.col(W, ties.method = "first")
    idx <- cbind(rows, col)
    v <- W[idx]
    v[!is.finite(v)] <- NA_real_
    top[, j] <- v
    W[idx] <- -Inf
  }
  out <- rowMeans(top, na.rm = TRUE)
  out[n == 0] <- NA_real_
  matrix(out, U1, U2, dimnames = dimnames(A)[1:2])
}

# TRUE for the dyads [reporter, partner] with a row in any cell.
dash_any_cell <- function(mask) {
  d <- dim(mask)
  matrix(rowSums(matrix(mask, d[1] * d[2])) > 0, d[1], d[2], dimnames = dimnames(mask)[1:2])
}

dash_keep_cells <- function(bundle, params) {
  !bundle$partner$cells$tech %in% params$partner$exclude_techs
}

# Best-five summaries (matrices [reporter, partner]) and dyad existence.
dash_partner_top5 <- function(bundle, cells, params) {
  keep <- dash_keep_cells(bundle, params)
  k <- params$partner$top_k
  list(
    fs = dash_topk_mean(cells$fs_index, k, keep),
    po = dash_topk_mean(cells$po_index, k, keep),
    fs_exists = dash_any_cell(bundle$partner$fs$mask),
    po_exists = dash_any_cell(bundle$partner$po$mask)
  )
}

# Weighted share over the categories present (psi_weighted_share()).
dash_weighted_share <- function(values, shares, weights) {
  num <- 0
  den <- 0
  unknown <- FALSE
  for (k in seq_along(values)) {
    present <- !is.na(values[[k]]) & weights[[k]] > 0
    s <- shares[[k]]
    unknown <- unknown | (present & is.na(s))
    num <- num + ifelse(present, weights[[k]] * ifelse(is.na(s), 0, s), 0)
    den <- den + ifelse(present, weights[[k]], 0)
  }
  ifelse(den > 0 & !unknown, num / den, NA_real_)
}

# PSI rankability (psi_flag_rankable()); tau NA = no imputed-share condition (a build before the
# rule).
dash_psi_flag <- function(value, share, sovereign, tau) {
  scored <- !is.na(value)
  if (is.na(tau)) {
    reason <- ifelse(!scored, "not scored", ifelse(!sovereign, "not a sovereign economy", NA_character_))
  } else {
    share_known <- !is.na(share)
    over <- share_known & share > tau + 1e-9
    reason <- ifelse(
      !scored, "not scored",
      ifelse(!sovereign & over, paste0("not a sovereign economy and imputed weight share above ", format(tau)),
        ifelse(!sovereign, "not a sovereign economy",
          ifelse(!share_known, "imputed weight share unknown",
            ifelse(over, paste0("imputed weight share above ", format(tau)), NA_character_))))
    )
  }
  rankable <- is.na(reason)
  rank <- rep(NA_integer_, length(value))
  rank[rankable] <- as.integer(rank(-value[rankable], ties.method = "min"))
  list(rankable = rankable, rank_exclusion = reason, rank = rank)
}

# PSI: dyad table (all world dyads) and the partner-level view (coalition or world reporters).
dash_psi <- function(bundle, top5, params) {
  pb <- bundle$partner
  U <- pb$U
  w <- params$partner$psi_weights
  wts <- c(w[["friendshore"]], w[["opportunity"]], w[["development"]])
  dv <- pb$dp$development[match(U, pb$dp$partner_iso)]
  dv_share <- pb$dp$imputed_weight_share[match(U, pb$dp$partner_iso)]
  tau <- pb$psi$tau
  if (is.null(tau)) tau <- NA_real_

  # Dyads
  exists <- (top5$fs_exists | top5$po_exists)
  diag(exists) <- FALSE
  ij <- which(exists, arr.ind = TRUE)
  fs <- top5$fs[ij]
  fs[!top5$fs_exists[ij]] <- NA_real_
  op <- top5$po[ij]
  op[!top5$po_exists[ij]] <- NA_real_
  d <- dv[ij[, 2]]
  psi <- dash_row_wmean(list(fs, op, d), as.list(wts))
  share <- dash_weighted_share(
    list(fs, op, d),
    list(pb$top5_share$fs[ij], pb$top5_share$po[ij], dv_share[ij[, 2]]),
    wts
  )
  dyad <- tibble::tibble(
    reporter_iso = U[ij[, 1]],
    partner_iso = U[ij[, 2]],
    friendshore = fs,
    opportunity = op,
    development = d,
    n_categories = (!is.na(fs)) + (!is.na(op)) + (!is.na(d)),
    psi = psi,
    imputed_weight_share = share
  )
  dyad <- dyad[order(dyad$reporter_iso, dyad$partner_iso, method = "radix"), , drop = FALSE]
  fl <- dash_psi_flag(dyad$psi, dyad$imputed_weight_share, pb$sovereign[match(dyad$partner_iso, U)] %in% TRUE, tau)
  dyad$rankable <- fl$rankable
  dyad$rank_exclusion <- fl$rank_exclusion
  # rank among the reporter's rankable partners
  dyad$rank <- NA_integer_
  ok <- which(dyad$rankable)
  for (g in split(ok, dyad$reporter_iso[ok])) dyad$rank[g] <- as.integer(rank(-dyad$psi[g], ties.method = "min"))

  # Partner view
  reporters <- if (identical(params$partner$psi_view, "world")) U else intersect(pb$psi$coalition_reporters, U)
  rsel <- U %in% reporters
  colmean <- function(M, ex, share_m) {
    M <- M
    M[!ex] <- NA_real_
    diag(M) <- NA_real_
    M <- M[rsel, , drop = FALSE]
    n <- colSums(!is.na(M))
    s <- colSums(M, na.rm = TRUE)
    val <- ifelse(n > 0, s / n, NA_real_)
    sh <- share_m
    sh[!ex] <- NA_real_
    diag(sh) <- NA_real_
    sh <- sh[rsel, , drop = FALSE]
    sh[is.na(M)] <- NA_real_
    # mean of the rows' shares over the rows with a value (NA if any of them is unknown)
    sh_unknown <- colSums(!is.na(M) & is.na(sh)) > 0
    sh_mean <- ifelse(n > 0 & !sh_unknown, colSums(sh, na.rm = TRUE) / n, NA_real_)
    list(value = val, n = as.integer(n), share = sh_mean)
  }
  cf <- colmean(top5$fs, top5$fs_exists, pb$top5_share$fs)
  co <- colmean(top5$po, top5$po_exists, pb$top5_share$po)
  # categories in C-locale order: development, friendshore, opportunity
  Cm <- cbind(development = dv, friendshore = cf$value, opportunity = co$value)
  Wv <- c(development = w[["development"]], friendshore = w[["friendshore"]], opportunity = w[["opportunity"]])
  in_data <- colSums(!is.na(Cm)) > 0
  Wv[!in_data] <- NA_real_
  Wm <- matrix(Wv, nrow(Cm), 3, byrow = TRUE)
  obs <- !is.na(Cm) & !is.na(Wm)
  wsum <- rowSums(ifelse(obs, Wm, 0))
  scored <- wsum > 0
  cw <- ifelse(obs, Wm / wsum, 0)
  contrib <- ifelse(cw == 0, 0, Cm * cw)
  value <- rowSums(contrib)
  value[!scored] <- NA_real_
  Sm <- cbind(dv_share, cf$share, co$share)
  present_w <- obs & Wm > 0
  sh_unknown <- rowSums(present_w & is.na(Sm)) > 0
  iws <- rowSums(ifelse(present_w, Wm * ifelse(is.na(Sm), 0, Sm), 0)) / rowSums(ifelse(present_w, Wm, 0))
  iws[!scored | sh_unknown] <- NA_real_
  partner <- tibble::tibble(
    partner_iso = U,
    Country = pb$names,
    friendshore = cf$value,
    opportunity = co$value,
    development = dv,
    psi = value,
    n_reporters_friendshore = cf$n,
    n_reporters_opportunity = co$n,
    imputed_weight_share = iws
  )
  partner <- partner[scored, , drop = FALSE]
  fl <- dash_psi_flag(partner$psi, partner$imputed_weight_share, pb$sovereign[match(partner$partner_iso, U)] %in% TRUE, tau)
  partner$rankable <- fl$rankable
  partner$rank_exclusion <- fl$rank_exclusion
  partner$rank <- fl$rank
  partner <- partner[order(partner$Country, method = "radix"), , drop = FALSE]
  list(dyad = dyad, partner = partner, reporters = reporters, view = if (identical(params$partner$psi_view, "world")) "world" else "coalition")
}

# Per-cell decomposition of one dyad: FS and PO components, their weighted shares, penalties,
# raw and adjusted scores, the indices and whether the cell is among the dyad's best five.
dash_partner_decompose <- function(bundle, params, cells, reporter_iso, partner_iso) {
  pb <- bundle$partner
  r <- match(reporter_iso, pb$U)
  p <- match(partner_iso, pb$U)
  if (is.na(r) || is.na(p)) stop("Unknown reporter or partner ISO3.")
  fs <- pb$fs
  po <- pb$po
  wf <- params$partner$fs_weights
  wp <- params$partner$po_weights
  C <- nrow(pb$cells)
  ob_src <- dash_fs_outbound_source(fs, C)
  ob <- rep(if (is.null(fs$outbound)) NA_real_ else fs$outbound[r, p], C)
  cm <- which(ob_src == "cm_fdi")
  if (length(cm)) ob[cm] <- fs$outbound_cmfdi[r, p, ]
  # component values as stored (chinese_parent_exposure: the share); the scores entering the mean
  # (score_fs) differ only for the risk component (1 - share)
  comp_fs <- list(
    imp_trade_index = fs$imp_trade[r, p, ],
    econ_opp_raw = fs$econ_opp_raw[r, ],
    es_need = fs$es_need[r, ],
    eo_partner = fs$eo_partner[p, ],
    outbound_index = ob,
    geo_alignment = rep(if (is.null(fs$geo)) NA_real_ else fs$geo[r, p], C),
    chinese_parent_exposure = if (is.null(fs$cpe)) rep(NA_real_, C) else fs$cpe[p, ]
  )
  score_fs <- comp_fs
  score_fs$chinese_parent_exposure <- dash_fs_risk_score(comp_fs$chinese_parent_exposure)
  wfs <- lapply(names(comp_fs), function(k) if (is.null(wf[[k]])) 0 else wf[[k]])
  den_fs <- Reduce(`+`, Map(function(x, wk) (!is.na(x)) * wk, comp_fs, wfs))
  fs_raw <- dash_row_wmean(score_fs, wfs)
  pen_r <- ((1 - fs$gh) * fs$cpi[r]) * params$partner$fs_penalty
  pen_p <- ((1 - fs$gh) * fs$cpi[p]) * params$partner$fs_penalty
  fs_adj <- pmax(0, fs_raw - (pen_r + pen_p))
  fs_adj[!fs$mask[r, p, ]] <- NA_real_
  comp_po <- list(
    trade_index = po$trade[r, p, ],
    econ_opp_index = po$econ_opp_index[r, ],
    partner_es_need = po$partner_es_need[p, ]
  )
  wpo <- lapply(names(comp_po), function(k) wp[[k]])
  den_po <- Reduce(`+`, Map(function(x, wk) (!is.na(x)) * wk, comp_po, wpo))
  po_raw <- dash_row_wmean(comp_po, wpo)
  pen_po <- ((1 - po$gh) * po$cpi[p]) * params$partner$po_penalty
  po_adj <- pmax(0, po_raw - pen_po)
  po_adj[!po$mask[r, p, ]] <- NA_real_
  fsi <- cells$fs_index[r, p, ]
  opi <- cells$po_index[r, p, ]
  keep <- dash_keep_cells(bundle, params)
  topflag <- function(x) {
    x2 <- ifelse(keep, x, NA_real_)
    o <- order(-x2, na.last = NA)
    flag <- rep(FALSE, length(x))
    flag[utils::head(o, params$partner$top_k)] <- TRUE
    flag
  }
  out <- tibble::tibble(
    reporter_iso = reporter_iso,
    partner_iso = partner_iso,
    tech = pb$cells$tech,
    supply_chain = pb$cells$supply_chain
  )
  for (k in names(comp_fs)) {
    out[[paste0("fs_", k)]] <- comp_fs[[k]]
    out[[paste0("fs_", k, "_share")]] <- ifelse(is.na(comp_fs[[k]]) | den_fs == 0, NA_real_, score_fs[[k]] * wfs[[match(k, names(comp_fs))]] / den_fs)
  }
  out$fs_outbound_source <- ob_src
  out$fs_penalty_reporter <- pen_r
  out$fs_penalty_partner <- pen_p
  out$fsi_raw <- fs_raw
  out$fsi_adj <- fs_adj
  out$friendshore_index <- fsi
  for (k in names(comp_po)) {
    out[[paste0("po_", k)]] <- comp_po[[k]]
    out[[paste0("po_", k, "_share")]] <- ifelse(is.na(comp_po[[k]]) | den_po == 0, NA_real_, comp_po[[k]] * wpo[[match(k, names(comp_po))]] / den_po)
  }
  out$po_penalty <- pen_po
  out$opportunity_raw <- po_raw
  out$opportunity_adj <- po_adj
  out$opportunity_index <- opi
  out$in_top5_fs <- topflag(fsi)
  out$in_top5_po <- topflag(opi)
  out$excluded <- !keep
  out
}
