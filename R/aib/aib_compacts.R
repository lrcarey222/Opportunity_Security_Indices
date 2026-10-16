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

# Coverage of the anchors' demand in one node when the members in `S` coordinate: non-member
# shipments to the anchors continue as they are; members pool the supply they can direct to the
# anchors (conservative: their shipments already headed there plus the supply the shock frees;
# reallocation: all their observed exports) plus a synthetic increment at member hosts.
aib_anchor_coverage <- function(prob, anchors, S, scenario, host_extra = NULL) {
  A <- intersect(anchors, prob$scope)
  if (!length(A)) return(NA_real_)
  dA <- sum(prob$demand[A])
  if (dA <= 0) return(NA_real_)
  mem <- intersect(S, prob$cand)
  lk <- prob$links
  to_A <- lk[lk$m %in% A, , drop = FALSE]
  fixed <- sum(prob$fixed_m[A]) + sum(to_A$x[!to_A$e %in% mem])
  pool <- if (identical(scenario, "conservative")) {
    sum(to_A$x[to_A$e %in% mem]) + sum(prob$freed[mem], na.rm = TRUE)
  } else {
    sum(prob$S[mem], na.rm = TRUE)
  }
  if (!is.null(host_extra)) pool <- pool + sum(host_extra[names(host_extra) %in% S])
  min(dA, fixed + pool) / dA
}

# Greedy minimum sufficient coalition: start from the demand anchors and add, one at a time, the
# economy that most reduces the shortfall against the highest target (summed over the compact's
# nodes, each capped at the target), until every node reaches it or nothing more helps. Country
# count is the coordination-complexity proxy. Returns the path (one row per step) with each
# node's coverage.
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
