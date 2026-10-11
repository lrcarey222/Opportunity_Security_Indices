# Fast, arithmetically identical ports of the allied-network steps the dashboard repeats on every
# dial move (pure; no IO). allied_network_design.R's functions are written with dplyr, whose
# per-call overhead dominates when 22 stages are re-solved several times by the portfolio-cap loop;
# these base-R versions do the same floating-point operations in the same order, so their results
# are identical (tests compare them with the pipeline functions with identical()).
#
#   dash_extract_edges()       allied_network_extract_edges() for member-level dyads that are
#                              already one row per key (orientation reporter_is_consumer)
#   dash_solve_stage_greedy()  allied_network_solve_stage_greedy() with dash_ras_fast()
#   dash_supplier_stats()      allied_network_supplier_stats()

# Weighted mean over finite x with positive finite weights (allied_network_wmean()).
dash_wmean <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

# One edge component (producer reporter_iso, consumer partner_iso, tech, supply_chain, value),
# aggregated to node pairs (allied_network_aggregate_edge_component()).
dash_aggregate_edge_component <- function(comp, members, member_weights, node_trade) {
  node_of <- stats::setNames(members$network_node, members$iso3c)
  sizes <- table(members$network_node)
  multi_nodes <- names(sizes)[sizes > 1]
  p_node <- unname(node_of[comp$reporter_iso])
  c_node <- unname(node_of[comp$partner_iso])
  intra <- p_node == c_node
  comp <- comp[!intra, , drop = FALSE]
  p_node <- p_node[!intra]
  c_node <- c_node[!intra]
  is_multi <- p_node %in% multi_nodes | c_node %in% multi_nodes
  pass <- tibble::tibble(reporter_iso = p_node[!is_multi], partner_iso = c_node[!is_multi], tech = comp$tech[!is_multi],
                         supply_chain = comp$supply_chain[!is_multi], value = comp$value[!is_multi])
  if (!any(is_multi)) return(pass)
  a <- comp[is_multi, , drop = FALSE]
  a$p_node <- p_node[is_multi]
  a$c_node <- c_node[is_multi]
  a <- a[is.finite(a$value), , drop = FALSE]
  mw_key <- paste(member_weights$iso3c, member_weights$tech, member_weights$supply_chain, sep = "\r")
  wp <- member_weights$member_weight[match(paste(a$reporter_iso, a$tech, a$supply_chain, sep = "\r"), mw_key)]
  wc <- member_weights$member_weight[match(paste(a$partner_iso, a$tech, a$supply_chain, sep = "\r"), mw_key)]
  keep <- !is.na(wp) & !is.na(wc)
  a <- a[keep, , drop = FALSE]
  wp <- wp[keep]
  wc <- wc[keep]
  tr <- rep(NA_real_, nrow(a))
  if (!is.null(node_trade) && nrow(a)) {
    tk <- paste(node_trade$exporter_iso, node_trade$importer_iso, node_trade$tech, node_trade$supply_chain, sep = "\r")
    tr <- as.numeric(node_trade$value)[match(paste(a$reporter_iso, a$partner_iso, a$tech, a$supply_chain, sep = "\r"), tk)]
  }
  tr <- pmax(0, ifelse(is.na(tr), 0, tr))
  # rows in the pipeline's order: keys sorted, then producer and consumer member
  o <- order(a$p_node, a$c_node, a$tech, a$supply_chain, a$reporter_iso, a$partner_iso, method = "radix")
  a <- a[o, , drop = FALSE]
  wp <- wp[o]
  wc <- wc[o]
  tr <- tr[o]
  g <- paste(a$p_node, a$c_node, a$tech, a$supply_chain, sep = "\r")
  idx <- split(seq_len(nrow(a)), factor(g, levels = unique(g)))
  val <- vapply(idx, function(i) {
    if (sum(tr[i]) > 0) {
      dash_wmean(a$value[i], tr[i])
    } else if (sum(wp[i] * wc[i]) > 0) {
      dash_wmean(a$value[i], wp[i] * wc[i])
    } else {
      mean(a$value[i])
    }
  }, numeric(1), USE.NAMES = FALSE)
  first <- vapply(idx, `[`, integer(1), 1L, USE.NAMES = FALSE)
  agg <- tibble::tibble(reporter_iso = a$p_node[first], partner_iso = a$c_node[first], tech = a$tech[first],
                        supply_chain = a$supply_chain[first], value = val)
  dplyr::bind_rows(pass, agg)
}

# Node-pair edges with both components (missing component 0), as allied_network_extract_edges()
# returns them for producer = Friendshore partner = Opportunity reporter. fs_long / po_long: one
# row per member dyad and cell (reporter_iso, partner_iso, tech, supply_chain, value).
dash_extract_edges <- function(fs_long, po_long, members, aggregate, member_weights = NULL, node_trade = NULL) {
  mean_distinct <- function(v) ifelse(is.finite(v), v, NA_real_)
  fs <- tibble::tibble(reporter_iso = fs_long$partner_iso, partner_iso = fs_long$reporter_iso, tech = fs_long$tech,
                       supply_chain = fs_long$supply_chain, value = mean_distinct(fs_long$value))
  po <- tibble::tibble(reporter_iso = po_long$reporter_iso, partner_iso = po_long$partner_iso, tech = po_long$tech,
                       supply_chain = po_long$supply_chain, value = mean_distinct(po_long$value))
  if (anyDuplicated(paste(fs$reporter_iso, fs$partner_iso, fs$tech, fs$supply_chain)) ||
      anyDuplicated(paste(po$reporter_iso, po$partner_iso, po$tech, po$supply_chain))) {
    stop("dash_extract_edges: dyad rows must be unique per key.")
  }
  if (isTRUE(aggregate)) {
    fs <- dash_aggregate_edge_component(fs, members, member_weights, node_trade)
    po <- dash_aggregate_edge_component(po, members, member_weights, node_trade)
  }
  kf <- paste(fs$reporter_iso, fs$partner_iso, fs$tech, fs$supply_chain, sep = "\r")
  kp <- paste(po$reporter_iso, po$partner_iso, po$tech, po$supply_chain, sep = "\r")
  keys <- c(kf, kp[!kp %in% kf])
  src <- rbind(fs[c("reporter_iso", "partner_iso", "tech", "supply_chain")], po[!kp %in% kf, c("reporter_iso", "partner_iso", "tech", "supply_chain")])
  fsi <- fs$value[match(keys, kf)]
  opi <- po$value[match(keys, kp)]
  tibble::tibble(
    tech = src$tech, supply_chain = src$supply_chain, reporter_iso = src$reporter_iso, partner_iso = src$partner_iso,
    friendshore_index = ifelse(is.na(fsi), 0, fsi),
    opportunity_index = ifelse(is.na(opi), 0, opi)
  )
}

# Realized redundancy of a stage (allied_network_supplier_stats()).
dash_supplier_stats <- function(flows, specialization) {
  na_out <- list(min_suppliers = NA_integer_, min_supplier_share = NA_real_)
  if (is.null(flows) || !nrow(flows) || is.null(specialization)) return(na_out)
  dw <- specialization$demand_weight
  dpos <- ifelse(is.na(dw), 0, dw) > 0
  d_iso <- specialization$iso3c[dpos]
  d_w <- dw[dpos]
  f <- flows[flows$flow_share > 1e-12, , drop = FALSE]
  k <- match(f$partner_iso, d_iso)
  f <- f[!is.na(k), , drop = FALSE]
  k <- k[!is.na(k)]
  if (!nrow(f)) return(na_out)
  n_per <- tabulate(k, nbins = length(d_iso))
  rel <- f$flow_share / d_w[k]
  list(min_suppliers = as.integer(min(n_per)), min_supplier_share = min(rel))
}

# allied_network_solve_stage_greedy() in base R (see that function for the method). `ras` is the
# RAS kernel and `prune`, `prune_stepwise` the pruning functions (by default the pipeline's
# prunes in an environment with dash_ras_fast()).
dash_solve_stage_greedy <- function(nodes_stage, edges_stage, producer_forbidden = character(), min_producers = 3,
                                    max_share = 0.40, min_share = 0.05, min_suppliers_per_consumer = 2,
                                    epsilon_supplier_share = 0.10, w_edge = 0.5, w_node = 1.0, w_dev = 0.0,
                                    allow_self = TRUE, diversity_lambda = 0, solver_env = NULL) {
  if (is.null(solver_env)) solver_env <- dash_fast_solver_env()
  ras <- solver_env$allied_network_ras
  prune <- solver_env$allied_network_prune_small_flows
  prune_step <- solver_env$allied_network_prune_small_flows_stepwise
  diversity_lambda <- allied_network_check_lambda(diversity_lambda)
  iso <- as.character(nodes_stage$iso3c)
  if (anyDuplicated(iso)) stop("dash_solve_stage_greedy: nodes_stage must have one row per iso3c.")
  n <- length(iso)
  eligible_iso <- setdiff(iso, producer_forbidden)
  if (!length(eligible_iso)) eligible_iso <- iso
  min_required <- ceiling(1 / max_share)
  target_prod <- max(min_producers, min_required, min_suppliers_per_consumer)
  n_prod <- min(target_prod, length(eligible_iso))
  if (length(eligible_iso) < target_prod) warning("Producer forbiddance reduced feasible producers for stage; using eligible maximum.")

  # Completed edge matrix over the stage's nodes, in the sorted order tidyr::crossing() gives
  so <- sort(unique(iso), method = "radix")
  ri <- match(edges_stage$reporter_iso, so)
  pi <- match(edges_stage$partner_iso, so)
  in_st <- !is.na(ri) & !is.na(pi)
  if (anyDuplicated(paste(edges_stage$reporter_iso, edges_stage$partner_iso))) {
    stop("dash_solve_stage_greedy: duplicate (reporter_iso, partner_iso) edges; collapse edges first.")
  }
  Wn <- matrix(NA_real_, n, n)
  Wn[cbind(ri[in_st], pi[in_st])] <- edges_stage$edge_weight[in_st]
  FSm <- matrix(NA_real_, n, n)
  OPm <- matrix(NA_real_, n, n)
  has_fs <- "friendshore_index" %in% names(edges_stage)
  has_op <- "opportunity_index" %in% names(edges_stage)
  if (has_fs) FSm[cbind(ri[in_st], pi[in_st])] <- edges_stage$friendshore_index[in_st]
  if (has_op) OPm[cbind(ri[in_st], pi[in_st])] <- edges_stage$opportunity_index[in_st]
  med <- suppressWarnings(stats::median(as.vector(t(Wn)), na.rm = TRUE))
  if (!is.finite(med)) med <- 0.5
  E <- Wn
  na_e <- is.na(E)
  E[na_e] <- 0
  diag_na <- which(diag(na_e))
  if (length(diag_na)) E[cbind(diag_na, diag_na)] <- med

  # Connectivity: c_i = sum_j W_ij d_j over consumers in sorted order (long-double sums, like sum())
  d_sorted <- as.numeric(nodes_stage$demand_weight)[match(so, iso)]
  conn_sorted <- rowSums(E * rep(d_sorted, each = n), na.rm = TRUE)
  max_w <- max(c(0, E), na.rm = TRUE)
  if (any(conn_sorted > max_w * sum(nodes_stage$demand_weight, na.rm = TRUE) + 1e-9)) {
    stop("allied_network_connectivity_out: connectivity exceeds the largest edge weight; edges or demand are duplicated.")
  }
  conn <- conn_sorted[match(iso, so)]

  ps <- nodes_stage$producer_score
  dev <- nodes_stage$dev_potential
  conn0 <- ifelse(is.na(conn), 0, conn)
  has_score <- !is.na(ps)
  combined <- w_node * ifelse(is.na(ps), 0, ps) + w_edge * conn0 + w_dev * ifelse(is.na(dev), 0, dev)
  o <- order(!has_score, -combined, -ps, -conn0, iso, method = "radix", na.last = TRUE)
  cand_iso <- iso[o]

  dw <- suppressWarnings(as.numeric(nodes_stage$demand_weight))
  d_pos <- stats::setNames(ifelse(is.na(dw), 0, dw) > 0, iso)
  if (!any(d_pos)) d_pos[] <- TRUE
  dpos_sorted <- unname(d_pos[so])
  reach_m <- E > 0 & matrix(dpos_sorted, n, n, byrow = TRUE)
  if (!isTRUE(allow_self)) diag(reach_m) <- FALSE
  reach_iso <- so[rowSums(reach_m) > 0]
  pool <- unique(cand_iso[cand_iso %in% eligible_iso])
  pool <- c(pool[pool %in% reach_iso], pool[!pool %in% reach_iso])
  diversified <- NULL
  if (diversity_lambda > 0) {
    k <- match(pool, iso)
    diversified <- allied_network_diversified_shares(
      v = w_node * ifelse(is.na(ps[k]), 0, ps[k]) + w_edge * conn0[k],
      bonus = w_dev * ifelse(is.na(dev[k]), 0, dev[k]),
      lambda = diversity_lambda, min_share = min_share, max_share = max_share, n_min = n_prod
    )
  }
  if (!is.null(diversified)) {
    selected_iso <- pool[seq_len(diversified$k)]
    target <- stats::setNames(diversified$shares, selected_iso)
  } else {
    selected_iso <- utils::head(pool, n_prod)
    sel_order <- cand_iso[cand_iso %in% selected_iso]
    s <- combined[match(sel_order, iso)]
    raw <- exp(s - max(s))
    shares <- allied_network_cap_shares(raw / sum(raw), min_share = min_share, max_share = max_share)
    target <- stats::setNames(shares, sel_order)[selected_iso]
  }

  d <- dw
  d[!is.finite(d)] <- 0
  d <- if (sum(d) > 0) d / sum(d) else rep(1 / n, n)
  # K[selected, consumer in node order] = max(0, W)
  rsel <- match(selected_iso, so)
  K <- pmax(E[rsel, match(iso, so), drop = FALSE], 0)
  dimnames(K) <- list(selected_iso, iso)
  self_idx <- cbind(seq_along(selected_iso), match(selected_iso, iso))
  if (!isTRUE(allow_self)) K[self_idx] <- 0
  dead <- colSums(K) <= 0 & d > 0
  if (any(dead)) {
    K[, dead] <- 1
    if (!isTRUE(allow_self)) K[self_idx[dead[self_idx[, 2]], , drop = FALSE]] <- 0
  }
  tgt <- as.numeric(target)
  f <- ras(K, row_target = tgt, col_target = d)
  f <- prune(f, K, row_target = tgt, col_target = d, min_suppliers = min_suppliers_per_consumer, epsilon = epsilon_supplier_share)
  if (diversity_lambda > 0) {
    f <- prune_step(f, K, row_target = tgt, col_target = d, min_suppliers = min_suppliers_per_consumer, epsilon = epsilon_supplier_share)
  }
  if (max(abs(rowSums(f) - tgt)) > 1e-6) {
    warning("Greedy flow fit could not meet the target production shares for stage; realized shares are reported (see max_production_share).")
  }

  fv <- as.vector(f)
  keep <- fv > 0
  fr <- rep(selected_iso, times = n)[keep]
  fp <- rep(iso, each = length(selected_iso))[keep]
  ei <- cbind(match(fr, so), match(fp, so))
  flows <- tibble::tibble(
    reporter_iso = fr,
    partner_iso = fp,
    edge_weight = E[ei],
    friendshore_index = if (has_fs) FSm[ei] else NULL,
    opportunity_index = if (has_op) OPm[ei] else NULL,
    flow_share = fv[keep]
  )
  prod <- as.numeric(rowSums(f))
  sel <- iso[o] %in% selected_iso
  ord_nodes <- nodes_stage[o, , drop = FALSE]
  production_share <- prod[match(ord_nodes$iso3c, selected_iso)]
  production_share[is.na(production_share)] <- 0
  specialization <- tibble::tibble(
    iso3c = ord_nodes$iso3c,
    country = ord_nodes$country,
    producer_score = ord_nodes$producer_score,
    connectivity_out = conn0[o],
    dev_potential = ord_nodes$dev_potential,
    demand_weight = ord_nodes$demand_weight,
    selected = sel,
    production_share = production_share
  )
  for (col in intersect(allied_network_node_extra_cols(), names(nodes_stage))) specialization[[col]] <- ord_nodes[[col]]
  a_by_iso <- stats::setNames(ifelse(is.na(specialization$producer_score), 0, specialization$producer_score), specialization$iso3c)
  dev_by_iso <- stats::setNames(ifelse(is.na(specialization$dev_potential), 0, specialization$dev_potential), specialization$iso3c)
  objective <- sum(flows$flow_share * (w_node * a_by_iso[flows$reporter_iso] + w_edge * flows$edge_weight), na.rm = TRUE) +
    w_dev * sum(dev_by_iso[selected_iso])
  if (diversity_lambda > 0) objective <- objective - diversity_lambda * sum(prod^2)
  list(
    specialization = specialization,
    flows = flows,
    objective = objective,
    hhi = allied_network_hhi(prod),
    n_producers = length(selected_iso),
    hhi_tangent_max_error = NA_real_
  )
}
