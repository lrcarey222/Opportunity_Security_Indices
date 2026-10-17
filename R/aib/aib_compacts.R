# Allied Industrial Base (AIB): Sectoral Compact formation (docs/aib_methodology.md, 9-14).
#
# A Sectoral Compact is a variable-geometry coalition that coordinates trade, investment, demand
# and risk-management policies across a strategically coherent supply chain where joint action
# materially improves resilience relative to fragmented national or bilateral action. The
# bilateral trade and investment analysis says which relationships matter; this layer says which
# of them need to be coordinated together. Nodes are grouped only when they are (A) coherent in
# the supply chain, (B) served by overlapping or complementary partner networks and (C) in need of
# similar policy instruments. Nothing here forces one compact per technology, a compact on every
# technology, or every node of a technology into one compact.

aib_node_classes <- function() c("COORDINATE", "BUILD", "MANAGE SPECIALIZATION", "LOW INTERVENTION", "INSUFFICIENT DATA")

# Policy interpretation of each node's continuous results (thresholds in config/aib.yml
# classification). The label is not a score; the columns it reads stay in the output.
aib_classify_nodes <- function(node_res, cls) {
  nr <- node_res
  loss <- dplyr::coalesce(nr$coverage_loss_at_manage_cap, 0)
  ls <- dplyr::coalesce(nr$optimized_largest_share, 0)
  t3 <- if ("optimized_top3_share" %in% names(nr)) dplyr::coalesce(nr$optimized_top3_share, 0) else rep(0, nrow(nr))
  top3_bar <- if (is.null(cls$manage_top3_share)) Inf else cls$manage_top3_share
  ok <- dplyr::coalesce(nr$optimize, TRUE) & !is.na(nr$shock_exposure)
  concentrated <- ls >= cls$manage_largest_share | t3 >= top3_bar | loss >= cls$manage_coverage_loss
  nr$node_classification <- dplyr::case_when(
    !ok ~ "INSUFFICIENT DATA",
    nr$shock_exposure < cls$low_exposure ~ "LOW INTERVENTION",
    nr$residual_gap >= cls$build_gap ~ "BUILD",
    concentrated ~ "MANAGE SPECIALIZATION",
    nr$existing_coverage >= cls$adequate_coverage ~ "LOW INTERVENTION",
    nr$coordination_dividend >= cls$coordinate_dividend ~ "COORDINATE",
    TRUE ~ "LOW INTERVENTION"
  )
  pct <- function(x) sprintf("%.0f%%", 100 * x)
  nr$classification_reason <- dplyr::case_when(
    !ok ~ "data too thin for a bilateral optimization (see data-quality flags)",
    nr$shock_exposure < cls$low_exposure ~ paste0("low exposure: the shock removes ", pct(nr$shock_exposure), " of demand"),
    nr$residual_gap >= cls$build_gap ~ paste0("a ", pct(nr$residual_gap), " gap remains after optimized trade"),
    ls >= cls$manage_largest_share ~ paste0("resilient, but the efficient solution relies on ", nr$optimized_largest_supplier, " for ", pct(ls), " of supply"),
    t3 >= top3_bar ~ paste0("resilient, but three suppliers hold ", pct(t3), " of the optimized supply (largest ", nr$optimized_largest_supplier, ")"),
    loss >= cls$manage_coverage_loss ~ paste0("diversifying to a ", pct(cls$manage_cap), " supplier limit costs ", pct(loss), " of coverage"),
    nr$existing_coverage >= cls$adequate_coverage ~ paste0("existing relationships restore ", pct(nr$existing_coverage), " of demand"),
    nr$coordination_dividend >= cls$coordinate_dividend ~ paste0("new or expanded relationships add ", pct(nr$coordination_dividend), " of coverage over existing ones"),
    TRUE ~ paste0("existing trade restores ", pct(nr$existing_coverage), " and optimization adds little")
  )
  # Continuous policy-need profile (trade coordination, capacity, specialization management), the
  # basis of the policy-coherence test.
  nr$need_coordinate <- pmax(dplyr::coalesce(nr$coordination_dividend, 0), 0)
  nr$need_build <- pmax(dplyr::coalesce(nr$residual_gap, 0), 0)
  nr$need_manage <- pmax(ls - 0.25, 0) + pmax(t3 - 0.5, 0) + pmax(loss, 0)
  nr$need_manage_specialization <- nr$need_manage
  # Multi-label needs: a node can need coordination, capacity and specialization management at
  # once; the single class above is only the headline label.
  exposed <- ok & dplyr::coalesce(nr$shock_exposure, 0) >= cls$low_exposure
  nr$needs_coordinate <- exposed & dplyr::coalesce(nr$coordination_dividend, 0) >= cls$coordinate_dividend
  nr$needs_build <- exposed & dplyr::coalesce(nr$residual_gap, 0) >= cls$build_gap
  nr$needs_manage_specialization <- exposed & concentrated
  nr$policy_needs <- vapply(seq_len(nrow(nr)), function(i) {
    k <- c("COORDINATE", "BUILD", "MANAGE SPECIALIZATION")[c(nr$needs_coordinate[i], nr$needs_build[i], nr$needs_manage_specialization[i])]
    if (length(k)) paste(k, collapse = " + ") else "none"
  }, character(1))
  nr
}

aib_cosine <- function(a, b) {
  keys <- union(names(a), names(b))
  if (!length(keys)) return(0)
  x <- dplyr::coalesce(unname(a[keys]), 0)
  y <- dplyr::coalesce(unname(b[keys]), 0)
  d <- sqrt(sum(x^2)) * sqrt(sum(y^2))
  if (d <= 0) 0 else sum(x * y) / d
}

# Per node: who supplies the solution (`supply`: node_id, exporter_iso, w, the optimized supply
# shares summed over the shocks, so recurring suppliers weigh more; plus the leading investment
# hosts) and who needs it (lost supply by importer).
aib_node_partner_vectors <- function(supply, lost, hosts = NULL, top_hosts = 5) {
  out <- list()
  for (n in names(lost)) {
    sp <- supply[supply$node_id == n, , drop = FALSE]
    s <- if (nrow(sp)) stats::setNames(sp$w, sp$exporter_iso) else numeric()
    if (!is.null(hosts)) {
      h <- hosts[hosts$node_id == n & hosts$host_rank <= top_hosts, , drop = FALSE]
      if (nrow(h)) {
        hv <- tapply(pmax(dplyr::coalesce(h$marginal_resilience_gain_diversified, h$marginal_resilience_gain), 1e-6), h$host_iso, sum)
        hv <- hv / sum(hv) * max(sum(s), 1e-6) * 0.5
        s <- c(s, stats::setNames(as.numeric(hv), names(hv)))
        s <- tapply(s, names(s), sum)
      }
    }
    d <- lost[[n]]
    out[[n]] <- list(
      supply = if (length(s)) stats::setNames(as.numeric(s), names(s)) / max(sum(s), 1e-12) else numeric(),
      demand = if (length(d) && sum(d) > 0) d / sum(d) else numeric()
    )
  }
  out
}

# Group the intervention nodes into candidate compacts. Pairs must be coherent in the supply chain
# (coherence[i, j]), have policy-need cosine >= min_policy and partner similarity >= min_partner,
# where partner similarity is the larger of overlap (the same suppliers and hosts) and
# complementarity (one node's suppliers are the other's buyers). Complete linkage, so every pair
# inside a compact passes all three tests. Returns node_id -> group.
aib_group_nodes <- function(classified, vectors, coherence, min_partner = 0.35, min_policy = 0.5) {
  ids <- classified$node_id[classified$node_classification %in% c("COORDINATE", "BUILD", "MANAGE SPECIALIZATION")]
  ids <- ids[ids %in% names(vectors) & ids %in% rownames(coherence)]
  if (!length(ids)) return(tibble::tibble(node_id = character(), group = integer(), partner_similarity = numeric()))
  if (length(ids) == 1) return(tibble::tibble(node_id = ids, group = 1L))
  need <- as.matrix(classified[match(ids, classified$node_id), c("need_coordinate", "need_build", "need_manage")])
  n <- length(ids)
  dist <- matrix(0, n, n, dimnames = list(ids, ids))
  psim <- matrix(1, n, n, dimnames = list(ids, ids))
  for (i in seq_len(n - 1)) for (j in (i + 1):n) {
    a <- vectors[[ids[i]]]
    b <- vectors[[ids[j]]]
    sim <- max(aib_cosine(a$supply, b$supply), aib_cosine(a$supply, b$demand), aib_cosine(a$demand, b$supply))
    pol <- aib_cosine(stats::setNames(need[i, ], colnames(need)), stats::setNames(need[j, ], colnames(need)))
    ok <- isTRUE(coherence[ids[i], ids[j]]) && pol >= min_policy
    psim[i, j] <- psim[j, i] <- sim
    dist[i, j] <- dist[j, i] <- if (ok) 1 - sim else 10
  }
  hc <- stats::hclust(stats::as.dist(dist), method = "complete")
  g <- stats::cutree(hc, h = 1 - min_partner)
  tibble::tibble(node_id = ids, group = as.integer(g[ids]))
}

# The linear terms of the coalition test for one node: coverage of the anchors' demand when the
# members S coordinate is min(1, (F + sum_{c in S} g_c) / dA), where
#   dA   the anchors' demand,
#   F    what reaches them with nobody coordinating (non-candidate shipments, the shocked economy's
#        surviving supply and every candidate's current shipments to them),
#   g_c  what candidate c adds by coordinating: reallocation = its available supply (all observed
#        exports, bounded by physical capability where known) less what it already ships to the
#        anchors; conservative = the supply the shock frees for it, within its physical room;
#        plus any synthetic increment at c (host_extra),
#   x_c  every exporter's current shipments to the anchors (for the leave-one-out test).
# Linear in the membership, so the minimum coalition is an exact integer program.
aib_anchor_terms <- function(prob, anchors, scenario, host_extra = NULL, drop = character()) {
  if (is.null(prob)) return(NULL)
  A <- intersect(anchors, prob$scope)
  if (!length(A)) return(NULL)
  dA <- sum(prob$demand[A])
  if (!(dA > 0)) return(NULL)
  get0 <- function(v, k) {
    if (is.null(v) || !length(v)) return(rep(0, length(k)))
    a <- unname(v[k])
    a[is.na(a)] <- 0
    as.numeric(a)
  }
  lk <- prob$links[prob$links$m %in% A, , drop = FALSE]
  fl <- prob$fixed_links[prob$fixed_links$m %in% A, , drop = FALSE]
  x_c <- if (nrow(lk)) tapply(lk$x, lk$e, sum) else numeric()
  x_f <- if (nrow(fl)) tapply(fl$x, fl$e, sum) else numeric()
  cand <- prob$cand
  xA <- get0(x_c, cand)
  if (identical(scenario, "conservative")) {
    room <- if (is.null(prob$supply_cap)) rep(Inf, length(cand)) else pmax(get0(prob$supply_cap, cand) - get0(prob$to_scope, cand), 0)
    g <- pmin(get0(prob$freed, cand), room)
  } else {
    g <- pmax(aib_available_supply(prob, "reallocation", cand) - xA, 0)
  }
  names(g) <- cand
  if (!is.null(host_extra) && length(host_extra)) {
    hx <- host_extra[names(host_extra) %in% names(g)]
    g[names(hx)] <- g[names(hx)] + hx
    new <- host_extra[!names(host_extra) %in% names(g)]
    g <- c(g, new)
  }
  x_all <- c(stats::setNames(as.numeric(x_c), names(x_c)), stats::setNames(as.numeric(x_f), names(x_f)))
  x_all <- tapply(x_all, names(x_all), sum)
  out <- list(dA = dA, F = sum(prob$fixed_m[A]) + sum(xA), g = g, x = stats::setNames(as.numeric(x_all), names(x_all)))
  # `drop`: economies whose supply is lost entirely (current shipments and any addition).
  if (length(drop)) {
    out$F <- out$F - sum(out$x[names(out$x) %in% drop])
    out$g[names(out$g) %in% drop] <- 0
  }
  out
}

# Coverage of the anchors' demand in one node when the members in `S` coordinate (aib_anchor_terms()).
aib_anchor_coverage <- function(prob, anchors, S, scenario, host_extra = NULL) {
  t <- aib_anchor_terms(prob, anchors, scenario, host_extra)
  if (is.null(t)) return(NA_real_)
  min(t$dA, t$F + sum(t$g[names(t$g) %in% S])) / t$dA
}

# Exact minimum coalition: the fewest countries such that every node of the compact reaches
# `target` coverage of the anchors' demand, the anchors themselves always members (their demand is
# what the compact protects). A binary program solved with GLPK; among minimum-size coalitions the
# one adding the most supply is kept. Returns members, reached, per-node coverage, and, when the
# target cannot be reached even with every candidate, the best attainable minimum coverage.
aib_coalition_exact <- function(problems, node_ids, anchors, candidates, scenario, host_extra = list(), target = 0.95,
                                drop = character(), fixed = character(), tie_break = TRUE, time_limit = 5) {
  terms <- lapply(node_ids, function(n) aib_anchor_terms(problems[[n]], anchors, scenario, host_extra[[n]], drop = drop))
  names(terms) <- node_ids
  terms <- terms[!vapply(terms, is.null, logical(1))]
  cov_of <- function(S) vapply(terms, function(t) min(t$dA, t$F + sum(t$g[names(t$g) %in% S])) / t$dA, numeric(1))
  # `fixed`: members held in (e.g. the 95% coalition when solving for 99%), beside the anchors.
  base_set <- unique(c(anchors, setdiff(fixed, drop)))
  pool <- setdiff(unique(candidates), c(base_set, drop))
  # Nothing to choose when the held-in members already reach the target: optimal by construction.
  if (!length(terms)) return(list(members = sort(base_set), reached = TRUE, coverage = numeric(), size = length(base_set),
                                  optimal = TRUE, robust_optimal = TRUE))
  base <- cov_of(base_set)
  if (all(base >= target - 1e-9)) return(list(members = sort(base_set), reached = TRUE, coverage = base, size = length(base_set),
                                              optimal = TRUE, robust_optimal = TRUE))
  full <- cov_of(c(base_set, pool))
  if (any(full < target - 1e-9) || !length(pool)) {
    return(list(members = NA_character_, reached = FALSE, coverage = full, size = NA_integer_, best_min_coverage = min(full)))
  }
  coef <- do.call(rbind, lapply(terms, function(t) {
    v <- unname(t$g[pool]) / t$dA
    v[is.na(v)] <- 0
    v
  }))
  b0 <- vapply(terms, function(t) (t$F + sum(t$g[names(t$g) %in% base_set])) / t$dA, numeric(1))
  rhs <- target - b0
  need <- rhs > 1e-12
  mat <- coef[need, , drop = FALSE]
  np <- length(pool)
  # A solve that hits its time limit keeps its best feasible solution and is reported (optimal = FALSE).
  feasible <- function(r, m, d, b) {
    x <- r$solution
    if (is.null(x) || length(x) != ncol(m) || anyNA(x)) return(FALSE)
    lhs <- as.numeric(m %*% x)
    all(ifelse(d == ">=", lhs >= b - 1e-7, abs(lhs - b) <= 1e-7))
  }
  r1 <- Rglpk::Rglpk_solve_LP(obj = rep(1, np), mat = mat, dir = rep(">=", nrow(mat)), rhs = rhs[need] - 1e-12,
                              types = rep("B", np), max = FALSE, control = list(tm_limit = 1000 * 20 * time_limit))
  optimal <- r1$status == 0
  if (!optimal && !feasible(r1, mat, rep(">=", nrow(mat)), rhs[need] - 1e-12)) {
    return(list(members = NA_character_, reached = FALSE, coverage = full, size = NA_integer_, best_min_coverage = min(full)))
  }
  k <- round(sum(r1$solution))
  sel <- pool[r1$solution > 0.5]
  if (!isTRUE(tie_break)) {
    members <- sort(c(base_set, sel))
    return(list(members = members, reached = TRUE, coverage = cov_of(members), size = length(members), optimal = optimal,
                robust_optimal = NA))
  }
  # Among coalitions of that size, the most robust one: maximize the smallest margin z over the
  # target across the compact's nodes (at most 1 - target: coverage cannot exceed 1), so the
  # coalition does not sit exactly on the target, where losing any small supplier would tip a node
  # below it. Remaining ties go to the coalition adding the most supply.
  zmax <- 1 - target
  m2 <- rbind(cbind(coef, -1), c(rep(1, np), 0))
  d2 <- c(rep(">=", nrow(coef)), "==")
  ctl <- list(tm_limit = 1000 * time_limit)
  r2 <- Rglpk::Rglpk_solve_LP(obj = c(rep(0, np), 1), mat = m2, dir = d2, rhs = c(rhs, k),
                              types = c(rep("B", np), "C"), bounds = list(upper = list(ind = np + 1L, val = zmax)), max = TRUE,
                              control = ctl)
  robust <- r2$status == 0
  if (robust || feasible(r2, m2, d2, c(rhs, k))) {
    sel <- pool[r2$solution[seq_len(np)] > 0.5]
    z <- if (robust) r2$optimum else min(as.numeric(coef %*% r2$solution[seq_len(np)]) - rhs, zmax)
    r3 <- Rglpk::Rglpk_solve_LP(obj = c(colSums(coef), 0), mat = m2, dir = d2, rhs = c(rhs, k), types = c(rep("B", np), "C"),
                                bounds = list(lower = list(ind = np + 1L, val = max(z - 1e-9, 0)), upper = list(ind = np + 1L, val = zmax)),
                                max = TRUE, control = ctl)
    if (r3$status == 0 || feasible(r3, m2, d2, c(rhs, k))) sel <- pool[r3$solution[seq_len(np)] > 0.5]
  }
  members <- sort(c(base_set, sel))
  list(members = members, reached = TRUE, coverage = cov_of(members), size = length(members), optimal = optimal, robust_optimal = robust)
}

# Leave-one-country-out: for each economy supplying the anchors (members and non-members), the
# coverage of every node if that economy's supply were lost entirely.
#   critical      with the coalition fixed, the loss pushes a node below `target` (where the node
#                 is below it already, the loss lowers its coverage by `min_drop` or more). A
#                 non-member that is critical is a critical external dependency. (In a minimum
#                 coalition every non-anchor member is critical in this sense by construction.)
#   irreplaceable the loss pushes a node below `target` even if every other candidate joined
#                 (`candidates`; members only when NULL): a member that is irreplaceable is a
#                 critical supply partner.
aib_coalition_critical <- function(problems, node_ids, anchors, members, scenario, host_extra = list(), target = 0.95,
                                   candidates = NULL, min_drop = 0.01) {
  out <- list()
  open_set <- unique(c(members, candidates))
  for (n in node_ids) {
    t <- aib_anchor_terms(problems[[n]], anchors, scenario, host_extra[[n]])
    if (is.null(t)) next
    gm <- t$g[names(t$g) %in% members]
    go <- t$g[names(t$g) %in% open_set]
    with_all <- min(t$dA, t$F + sum(gm)) / t$dA
    who <- unique(c(names(t$x)[t$x > 1e-12], names(gm)[gm > 1e-12]))
    for (c in who) {
      xc <- if (c %in% names(t$x)) t$x[[c]] else 0
      gc <- if (c %in% names(gm)) gm[[c]] else 0
      go_c <- if (c %in% names(go)) go[[c]] else 0
      without <- min(t$dA, t$F - xc + sum(gm) - gc) / t$dA
      without_open <- min(t$dA, t$F - xc + sum(go) - go_c) / t$dA
      out[[length(out) + 1]] <- tibble::tibble(node_id = n, iso3 = c, member = c %in% members,
                                               supply_share = xc / t$dA, added_share = gc / t$dA,
                                               coverage_with = with_all, coverage_without = without,
                                               coverage_without_open = without_open,
                                               critical = without < target - 1e-9 &
                                                 (with_all >= target - 1e-9 | with_all - without >= min_drop),
                                               irreplaceable = without_open < target - 1e-9)
    }
  }
  dplyr::bind_rows(out)
}

# Substitutability of each member of a minimum coalition. With that member's supply lost (an anchor
# stays in for its demand): `needed` when the rest of the coalition no longer reaches the target;
# `single_substitutes` the non-members any one of which, added to the rest, restores it (most
# supply first). A member that is needed and has no single substitute is a critical supply partner;
# `substitutes_needed` is then the smallest number of additions that restores the target (exact
# program with the rest held in; NA when nothing can).
aib_coalition_substitution <- function(problems, node_ids, anchors, members, candidates, scenario, host_extra = list(),
                                       target = 0.95) {
  if (!length(members) || all(is.na(members))) return(tibble::tibble())
  pool <- setdiff(unique(candidates), members)
  dplyr::bind_rows(lapply(members, function(c) {
    terms <- lapply(node_ids, function(n) aib_anchor_terms(problems[[n]], anchors, scenario, host_extra[[n]], drop = c))
    terms <- terms[!vapply(terms, is.null, logical(1))]
    reach <- function(S) all(vapply(terms, function(t) min(t$dA, t$F + sum(t$g[names(t$g) %in% S])) / t$dA >= target - 1e-9, logical(1)))
    needed <- length(terms) > 0 && !reach(members)
    subs <- character()
    if (needed && length(pool)) {
      ok <- vapply(pool, function(x) reach(c(members, x)), logical(1))
      subs <- pool[ok]
      if (length(subs)) {
        w <- vapply(subs, function(x) sum(vapply(terms, function(t) aib_or(unname(t$g[x]), 0) / t$dA, numeric(1)), na.rm = TRUE), numeric(1))
        subs <- subs[order(-w)]
      }
    }
    n_needed <- if (!needed) 0L else if (length(subs)) 1L else {
      r <- aib_coalition_exact(problems, node_ids, anchors, pool, scenario, host_extra, target, drop = c, fixed = setdiff(members, c),
                               tie_break = FALSE)
      if (isTRUE(r$reached)) length(setdiff(r$members, members)) else NA_integer_
    }
    tibble::tibble(iso3 = c, anchor = c %in% anchors, needed = needed, n_single_substitutes = length(subs),
                   single_substitutes = paste(utils::head(subs, 5), collapse = " "), substitutes_needed = n_needed,
                   critical = needed && !length(subs))
  }))
}

# Curated physical / process adjacency (config/aib.yml supply_chain_links, upstream -> downstream,
# master sub-sector names, "Tech::Sub-sector" where a name recurs) as directed node edges.
aib_chain_edges <- function(node_cells_detail, links) {
  e <- dplyr::bind_rows(lapply(links, function(l) {
    l <- unlist(l)
    a <- aib_link_nodes(node_cells_detail, l[1])
    b <- aib_link_nodes(node_cells_detail, l[2])
    if (!length(a) || !length(b)) return(NULL)
    tidyr::expand_grid(from = a, to = b)
  }))
  if (!nrow(e)) return(tibble::tibble(from = character(), to = character()))
  dplyr::distinct(e[e$from != e$to, , drop = FALSE])
}

# Supply-chain coherence for compact grouping: two nodes are coherent when one is upstream of the
# other within `max_hops` links of the curated chain, or (allow_co_inputs) when both feed the same
# downstream node directly. Sitting in the same technology or stage is not enough on its own.
# `through`: the nodes a link may pass through (intermediate nodes and the shared downstream node of
# co-inputs); NULL = any node.
aib_chain_coherence <- function(chain_edges, node_ids, max_hops = 3, allow_co_inputs = TRUE, through = NULL) {
  n <- length(node_ids)
  adj <- matrix(0, n, n, dimnames = list(node_ids, node_ids))
  e <- chain_edges[chain_edges$from %in% node_ids & chain_edges$to %in% node_ids, , drop = FALSE]
  if (nrow(e)) adj[cbind(match(e$from, node_ids), match(e$to, node_ids))] <- 1
  pass <- diag(if (is.null(through)) rep(1, n) else as.numeric(node_ids %in% through), n)
  reach <- adj > 0
  step <- adj
  if (max_hops >= 2) for (h in 2:max_hops) {
    step <- (step %*% pass %*% adj > 0) * 1
    reach <- reach | step > 0
  }
  coh <- reach | t(reach)
  if (isTRUE(allow_co_inputs)) coh <- coh | (adj %*% pass %*% t(adj) > 0)
  diag(coh) <- TRUE
  dimnames(coh) <- list(node_ids, node_ids)
  coh
}

# A compact must be one connected piece of the physical chain: its nodes link directly, or through
# nodes that are in no compact or in this one. Complete linkage on k-hop coherence can join nodes
# whose only link runs through a node grouped elsewhere (natural graphite with battery packs, via
# anodes and cells in another compact); such a group is split into its connected parts.
aib_split_disconnected <- function(groups, chain_edges, node_ids, max_hops = 3, allow_co_inputs = TRUE) {
  if (!nrow(groups)) return(groups)
  grouped <- groups$node_id
  out <- list()
  k <- 0L
  for (g in sort(unique(groups$group))) {
    ids <- groups$node_id[groups$group == g]
    comp <- seq_along(ids)
    if (length(ids) > 1) {
      through <- setdiff(node_ids, setdiff(grouped, ids))
      coh <- aib_chain_coherence(chain_edges, node_ids, max_hops, allow_co_inputs, through = through)[ids, ids, drop = FALSE]
      comp <- rep(NA_integer_, length(ids))
      for (s0 in seq_along(ids)) {
        if (!is.na(comp[s0])) next
        comp[s0] <- s0
        queue <- s0
        while (length(queue)) {
          v <- queue[1]
          queue <- queue[-1]
          nb <- which(coh[v, ] & is.na(comp))
          comp[nb] <- s0
          queue <- c(queue, nb)
        }
      }
    }
    for (cc in unique(comp)) {
      k <- k + 1L
      out[[length(out) + 1]] <- tibble::tibble(node_id = ids[comp == cc], group = k)
    }
  }
  dplyr::bind_rows(out)
}

# Greedy sufficient coalition (a heuristic, reported as `greedy_sufficient_coalition` and used for
# the "coverage as countries join" path; the minimum coalition is aib_coalition_exact()): start
# from the demand anchors and add, one at a time, the economy that most reduces the shortfall
# against the highest target (summed over the compact's nodes, each capped at the target), until
# every node reaches it or nothing more helps. Returns the path with each node's coverage.
aib_coalition_path <- function(problems, node_ids, anchors, candidates, scenario, host_extra = list(),
                               target = 0.99, max_steps = 40) {
  cov_all <- function(S) vapply(node_ids, function(n) {
    v <- aib_anchor_coverage(problems[[n]], anchors, S, scenario, host_extra[[n]])
    if (is.na(v)) 1 else v
  }, numeric(1))
  S <- anchors
  cur <- cov_all(S)
  path <- list(tibble::tibble(step = 0L, added = NA_character_, members = paste(sort(S), collapse = " "),
                              n_members = length(S), min_coverage = min(cur), mean_coverage = mean(cur),
                              node_coverage = list(cur)))
  pool <- setdiff(candidates, S)
  step <- 0L
  while (min(cur) < target - 1e-9 && length(pool) && step < max_steps) {
    score <- vapply(pool, function(c) {
      nw <- cov_all(c(S, c))
      sum(pmin(nw, target)) - sum(pmin(cur, target)) + 1e-6 * (sum(nw) - sum(cur))
    }, numeric(1))
    if (max(score) <= 1e-7) break
    pick <- pool[order(-score, pool)][1]
    S <- c(S, pick)
    cur <- cov_all(S)
    pool <- setdiff(pool, pick)
    step <- step + 1L
    path[[length(path) + 1]] <- tibble::tibble(step = step, added = pick, members = paste(sort(S), collapse = " "),
                                               n_members = length(S), min_coverage = min(cur), mean_coverage = mean(cur),
                                               node_coverage = list(cur))
  }
  dplyr::bind_rows(path)
}

aib_coalition_at <- function(path, target) {
  k <- which(path$min_coverage >= target - 1e-9)
  if (!length(k)) return(list(members = NA_character_, reached = FALSE, coverage = max(path$min_coverage)))
  r <- path[k[1], ]
  list(members = r$members, reached = TRUE, coverage = r$min_coverage)
}

aib_archetype_tools <- function() {
  list(
    "Trade / Coordination Compact" = "market access; rules of origin; procurement; standards; offtake coordination",
    "Investment / Capacity Compact" = "coordinated finance; guarantees; investment pipeline; price support; demand aggregation; long-term offtake",
    "Specialization Compact" = "contingency planning; strategic inventories; supply guarantees; reciprocal access; targeted redundancy",
    "Integrated Compact" = "a mix of trade coordination, capacity finance and specialization safeguards by node"
  )
}

aib_class_treatment <- function(cls) {
  dplyr::case_when(
    cls == "COORDINATE" ~ "coordinate trade: market access, rules of origin, procurement and offtake with the listed partners",
    cls == "BUILD" ~ "build capacity: coordinated finance, guarantees and long-term offtake for the listed hosts",
    cls == "MANAGE SPECIALIZATION" ~ "manage specialization: inventories, supply guarantees and reciprocal access with the dominant suppliers",
    cls == "LOW INTERVENTION" ~ "monitor; no compact instrument needed",
    TRUE ~ "improve the data before choosing an instrument"
  )
}

# Archetype from the lost-supply-weighted mix of node classes.
aib_archetype <- function(classes, lost, dominance = 0.6) {
  map <- c("COORDINATE" = "Trade / Coordination Compact", "BUILD" = "Investment / Capacity Compact",
           "MANAGE SPECIALIZATION" = "Specialization Compact")
  w <- tapply(lost, classes, sum)
  w <- w[names(w) %in% names(map)]
  if (!length(w) || sum(w) <= 0) return("Integrated Compact")
  s <- w / sum(w)
  if (max(s) >= dominance) unname(map[names(s)[which.max(s)]]) else "Integrated Compact"
}

# Short compact name from its nodes.
aib_compact_name <- function(labels, techs) {
  t <- sort(unique(unlist(strsplit(techs, "; "))))
  l <- unique(sub(" \\+ .*$", "", labels))
  if (length(l) > 4) l <- c(l[1:3], paste0("and ", length(l) - 3, " more"))
  paste0(paste(t, collapse = "/"), ": ", paste(l, collapse = ", "))
}

# The node problem with only `members` coordinating: non-member candidates keep their surviving
# shipments into the universe (they move to the fixed flows) and cannot be redirected; members
# keep their supply pools. Used for the coverage a compact's own coalition achieves.
aib_restrict_problem <- function(prob, members) {
  keep <- intersect(prob$cand, members)
  drop <- setdiff(prob$cand, members)
  if (!length(drop)) return(prob)
  lk <- prob$links
  out_l <- lk[lk$e %in% drop, , drop = FALSE]
  add_m <- tapply(out_l$x, factor(out_l$m, levels = prob$scope), sum)
  add_m[is.na(add_m)] <- 0
  prob$fixed_m <- prob$fixed_m + add_m
  add_e <- tapply(out_l$x, out_l$e, sum)
  fe <- prob$fixed_by_exporter
  both <- union(names(fe), names(add_e))
  g <- function(v, k) { a <- unname(v[k]); a[is.na(a)] <- 0; a }
  prob$fixed_by_exporter <- stats::setNames(g(fe, both) + g(add_e, both), both)
  prob$fixed_total <- prob$fixed_total + sum(out_l$x)
  prob$links <- lk[!lk$e %in% drop, , drop = FALSE]
  prob$cand <- keep
  prob$S <- prob$S[keep]
  prob$to_scope <- prob$to_scope[keep]
  prob$freed <- prob$freed[keep]
  prob$outside_scope <- prob$outside_scope[keep]
  if (!is.null(prob$supply_cap)) prob$supply_cap <- prob$supply_cap[keep]
  if (!is.null(prob$physical_producer)) prob$physical_producer <- prob$physical_producer[keep]
  prob
}

# Coverage of the anchors' demand in one node under the existing-relationship response (state B
# flows, no coordination): the fragmented baseline a compact has to beat.
aib_anchor_existing_coverage <- function(prob, anchors, flows_existing) {
  A <- intersect(anchors, prob$scope)
  if (!length(A)) return(NA_real_)
  dA <- sum(prob$demand[A])
  if (dA <= 0) return(NA_real_)
  fb <- if (is.null(flows_existing) || !nrow(flows_existing)) 0 else sum(flows_existing$f[flows_existing$m %in% A])
  min(dA, sum(prob$fixed_m[A]) + fb) / dA
}

# Lost-supply shares by class, and the policy needs of every class holding at least `floor` of the
# compact's lost supply (a chain that spans archetypes keeps every archetype's instruments).
aib_class_mix <- function(classes, lost) {
  w <- tapply(lost, classes, sum)
  w <- w[names(w) %in% c("COORDINATE", "BUILD", "MANAGE SPECIALIZATION")]
  if (!length(w) || sum(w) <= 0) return(stats::setNames(numeric(), character()))
  sort(w / sum(w), decreasing = TRUE)
}
aib_policy_needs <- function(mix, floor = 0.2) {
  map <- c("COORDINATE" = "Trade / Coordination Compact", "BUILD" = "Investment / Capacity Compact",
           "MANAGE SPECIALIZATION" = "Specialization Compact")
  tools <- aib_archetype_tools()
  k <- names(mix)[mix >= floor]
  if (!length(k)) k <- names(mix)[1]
  if (!length(k)) return(tools[["Integrated Compact"]])
  paste(unique(unlist(strsplit(unlist(tools[unname(map[k])]), "; "))), collapse = "; ")
}
