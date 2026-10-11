# Dashboard L5: the Allied Industrial Base network for any dial setting (pure; no IO).
#
# Three steps, each cacheable on the parameters it reads (dash_params_key()):
#   dash_network_inputs()  member-level node inputs aggregated to network nodes (the EU bundle) and
#                          the node-pair Friendshore / Opportunity edge components, from the
#                          scenario's dyad cells (allied_network_aggregate_node_inputs(),
#                          allied_network_extract_edges(): the pipeline's own functions)
#   dash_network_score()   producer scores and demand (allied_network_score_nodes()), edge weights
#                          w_fs FS + w_op OP, and the home-bias self-supply edges
#   dash_network_solve()   per stage, the fast greedy (allied_network_solve_stage_greedy() with a
#                          faster but arithmetically identical RAS) or the exact MILP
#                          (allied_network_solve_stage_milp()), and the portfolio-cap loop of
#                          allied_network_design()
# The solve reproduces allied_network_design(method = "greedy") on the same inputs (tested).
#
# bundle$network (scripts/85): members (iso3c, country, tier, network_node, node_tier, node_only,
# in scripts/30's order), node_raw_member (allied_network_node_inputs(node_policy = TRUE): iso3c,
# country, tech, supply_chain, eo_raw, es_raw, policy_raw, dev_potential, gdp_usd, gdp_imputed),
# node_member_weights (eu_stage_weights_tbl), node_trade (member basket trade: exporter_iso,
# importer_iso, tech, supply_chain, value), country_size (iso3c, country, year, gdp_usd,
# gdp_imputed), node_labels, country_info (iso3c, country), country_map (Country spelling ->
# iso3c, is_canonical, for the policy term), stored, greedy_baseline, observed_hhi.

# ---- Fast RAS ---------------------------------------------------------------------------

# allied_network_ras() with .rowSums / .colSums and a recycled column scaling instead of sweep():
# the same floating-point operations in the same order, so the same result, about twice as fast.
dash_ras_fast <- function(K, row_target, col_target, max_iter = 2000, tol = 1e-12) {
  f <- K
  nr <- nrow(f)
  nc <- ncol(f)
  for (iter in seq_len(max_iter)) {
    rs <- .rowSums(f, nr, nc)
    v <- row_target / rs
    v[!(rs > 0)] <- 0
    f <- f * v
    cs <- .colSums(f, nr, nc)
    u <- col_target / cs
    u[!(cs > 0)] <- 0
    f <- f * rep(u, each = nr)
    if (max(abs(.rowSums(f, nr, nc) - row_target)) < tol) break
  }
  f
}

# An environment in which the pipeline's greedy stage solver (and its flow pruning) call
# dash_ras_fast() instead of allied_network_ras(). The pipeline functions are copied, not changed.
dash_fast_solver_env <- function(pipeline_env = NULL) {
  if (is.null(pipeline_env)) pipeline_env <- environment(allied_network_solve_stage_greedy)
  env <- new.env(parent = pipeline_env)
  env$allied_network_ras <- dash_ras_fast
  for (fn in c("allied_network_solve_stage_greedy", "allied_network_prune_small_flows",
               "allied_network_prune_small_flows_stepwise")) {
    f <- get(fn, envir = pipeline_env)
    environment(f) <- env
    assign(fn, f, envir = env)
  }
  env
}

# ---- Inputs ------------------------------------------------------------------------------

# Members checked as allied_network_check_members() returns them (scripts/30's order).
dash_network_members <- function(bundle) {
  m <- bundle$network$members
  allied_network_check_members(m, m$iso3c)
}

# The stages solved: the member stages in supply_chains, outside the excluded technologies.
dash_network_stages <- function(bundle, params) {
  raw <- bundle$network$node_raw_member
  np <- params$network
  st <- unique(raw[c("tech", "supply_chain")])
  keep <- tolower(st$supply_chain) %in% tolower(np$supply_chains) & !st$tech %in% np$exclude_techs
  st <- st[keep, , drop = FALSE]
  st <- st[order(st$tech, st$supply_chain, method = "radix"), , drop = FALSE]
  tibble::as_tibble(st)
}

# Scenario policy index collapsed to members (allied_network_collapse_country_index()'s rule: the
# canonical spelling first), for the producer score's policy term.
dash_network_policy_raw <- function(bundle, policy_index, raw) {
  cm <- bundle$network$country_map
  pol <- policy_index
  k <- match(pol$Country, cm$Country)
  pol$iso3c <- cm$iso3c[k]
  pol$is_canonical <- cm$is_canonical[k] %in% TRUE
  pol <- pol[!is.na(pol$iso3c) & pol$iso3c %in% raw$iso3c, , drop = FALSE]
  key <- paste(pol$iso3c, pol$tech, pol$supply_chain, sep = "\r")
  o <- order(key, !is.finite(pol$value), !pol$is_canonical, method = "radix")
  pol <- pol[o, , drop = FALSE]
  key <- key[o]
  pol <- pol[!duplicated(key), , drop = FALSE]
  pol$value[match(paste(raw$iso3c, raw$tech, raw$supply_chain, sep = "\r"),
                  paste(pol$iso3c, pol$tech, pol$supply_chain, sep = "\r"))]
}

# Member-pair rows of one dyad index (the rows the stored dyad table has, values possibly NA) in
# the long shape allied_network_extract_edges() reads.
dash_network_dyads <- function(arr, mask, bundle, iso3c_network, stages, category, variable) {
  U <- bundle$partner$U
  pc <- bundle$partner$cells
  ri <- match(iso3c_network, U)
  ri <- ri[!is.na(ri)]
  ci <- match(paste(stages$tech, stages$supply_chain), paste(pc$tech, pc$supply_chain))
  ci <- ci[!is.na(ci)]
  if (!length(ri) || !length(ci)) {
    return(tibble::tibble(reporter_iso = character(), partner_iso = character(), tech = character(),
                          supply_chain = character(), category = character(), variable = character(),
                          data_type = character(), value = numeric()))
  }
  sub <- arr[ri, ri, ci, drop = FALSE]
  msk <- mask[ri, ri, ci, drop = FALSE]
  idx <- which(msk, arr.ind = TRUE)
  tibble::tibble(
    reporter_iso = U[ri][idx[, 1]],
    partner_iso = U[ri][idx[, 2]],
    tech = pc$tech[ci][idx[, 3]],
    supply_chain = pc$supply_chain[ci][idx[, 3]],
    category = category,
    variable = variable,
    data_type = "index",
    value = sub[idx]
  )
}

# L5a. cells = dash_partner_cells(); policy = dash_policy() (read only when the scenario switches
# the producer score's policy term on).
dash_network_inputs <- function(bundle, params, cells, pillars = NULL, policy = NULL, edges_impl = c("fast", "pipeline")) {
  edges_impl <- match.arg(edges_impl)
  nb <- bundle$network
  np <- params$network
  members <- dash_network_members(bundle)
  iso3c_network <- members$iso3c
  stages <- dash_network_stages(bundle, params)
  if (!nrow(stages)) stop("No network stages to solve under these settings.")
  raw <- nb$node_raw_member
  raw <- raw[paste(raw$tech, raw$supply_chain) %in% paste(stages$tech, stages$supply_chain), , drop = FALSE]
  raw <- tibble::as_tibble(raw)
  notes <- character()
  if (isTRUE(np$node_policy) && !is.null(policy)) {
    raw$policy_raw <- dash_network_policy_raw(bundle, policy$index, raw)
    notes <- c(notes, "producer score includes the scenario Policy index")
  }
  aggregate <- allied_network_aggregates(members)
  member_weights <- NULL
  node_size <- NULL
  if (aggregate) {
    member_weights <- allied_network_member_weights(members, nb$node_member_weights, stages)
    node_size <- allied_network_node_size_tbl(nb$country_size, members)
    raw_nodes <- allied_network_aggregate_node_inputs(raw, members, member_weights, nb$node_labels, nb$country_info, node_size)
    nodes_vec <- allied_network_node_table(members)$network_node
  } else {
    raw_nodes <- raw
    nodes_vec <- iso3c_network
  }
  fs_long <- dash_network_dyads(cells$fs_index, bundle$partner$fs$mask, bundle, iso3c_network, stages, "friendshore", "Friendshore Index")
  po_long <- dash_network_dyads(cells$po_index, bundle$partner$po$mask, bundle, iso3c_network, stages, "opportunity", "Opportunity Index")
  node_trade <- if (aggregate && !is.null(nb$node_trade)) allied_network_member_trade(nb$node_trade, iso3c_network) else NULL
  edges <- if (edges_impl == "fast") {
    dash_extract_edges(fs_long, po_long, members, aggregate, member_weights, node_trade)
  } else {
    allied_network_extract_edges(
      fs_long, po_long,
      iso3c_network = iso3c_network,
      edge_weights = list(friendshore = 1, opportunity = 1),
      keep_components = TRUE,
      friendshore_orientation = "reporter_is_consumer",
      network_members = if (aggregate) members else NULL,
      member_weights = member_weights,
      node_trade_tbl = node_trade
    )
  }
  edges <- tibble::as_tibble(edges)[c("tech", "supply_chain", "reporter_iso", "partner_iso", "friendshore_index", "opportunity_index")]
  out <- list(
    raw_nodes = raw_nodes,
    edges = edges,
    member_weights = member_weights,
    stages = stages,
    members = members,
    nodes = nodes_vec,
    node_size = node_size,
    aggregate = aggregate
  )
  attr(out, "notes") <- notes
  out
}

# L5b. Producer scores and demand, edge weights and (populism != 50) the self-supply edges.
dash_network_score <- function(inputs, params) {
  np <- params$network
  nodes <- allied_network_score_nodes(
    inputs$raw_nodes,
    node_weights = np$node_weights,
    demand_mode = np$demand_mode,
    demand_weights = np$demand_weights,
    size_transform = np$size_transform,
    policy_missing = np$policy_missing,
    missing_components = np$missing_components,
    node_policy = np$node_policy
  )
  w <- allied_network_normalize_weights(c(friendshore = np$edge_weights$friendshore, opportunity = np$edge_weights$opportunity))
  if (any(is.na(w))) stop("Network edge weights must be positive.")
  edges <- inputs$edges
  edges$edge_weight <- w[["friendshore"]] * edges$friendshore_index + w[["opportunity"]] * edges$opportunity_index
  edges <- edges[c("tech", "supply_chain", "reporter_iso", "partner_iso", "edge_weight", "friendshore_index", "opportunity_index")]
  self_edges <- NULL
  if (!isTRUE(all.equal(np$self_multiplier, 1, tolerance = 0))) {
    # The solvers' default self-supply weight is the stage's median edge weight among its nodes;
    # home bias scales it (capped at 1, the largest index-valued weight)
    st <- inputs$stages
    self_edges <- lapply(seq_len(nrow(st)), function(i) {
      t <- st$tech[i]
      sc <- st$supply_chain[i]
      iso <- nodes$iso3c[nodes$tech == t & nodes$supply_chain == sc]
      e <- edges$edge_weight[edges$tech == t & edges$supply_chain == sc & edges$reporter_iso %in% iso & edges$partner_iso %in% iso]
      med <- suppressWarnings(stats::median(e, na.rm = TRUE))
      if (!is.finite(med)) med <- 0.5
      tibble::tibble(
        tech = t, supply_chain = sc, reporter_iso = iso, partner_iso = iso,
        edge_weight = min(1, np$self_multiplier * med),
        friendshore_index = NA_real_, opportunity_index = NA_real_
      )
    })
    self_edges <- dplyr::bind_rows(self_edges)
    edges <- dplyr::bind_rows(edges, self_edges)
  }
  list(
    nodes = nodes,
    edges = edges,
    self_edges = self_edges,
    stages = inputs$stages,
    members = inputs$members,
    nodes_vec = inputs$nodes,
    node_size = inputs$node_size,
    member_weights = inputs$member_weights,
    aggregate = inputs$aggregate
  )
}

# ---- Solve -------------------------------------------------------------------------------

dash_stage_key <- function(tech, supply_chain) paste(tech, supply_chain, sep = "||")

# allied_network_build_topk_tbl() with one change when share_rank_digits is a number: the stage
# ranks (rank_in_stage, topk_rank) use round(production_share, share_rank_digits), so producers
# whose target shares are equal (two at max_share, several water-filled to min_share) are ordered by
# the intended tie-breakers (producer score, then ISO3) and not by the ~1e-13 noise of the RAS fit,
# which no other implementation can reproduce. in_top_k still requires an unrounded share > 0, and
# the table keeps the unrounded shares. NULL calls the pipeline function itself.
dash_build_topk_tbl <- function(specialization_tbl, portfolio_top_k = 5, share_rank_digits = NULL) {
  if (is.null(share_rank_digits)) return(allied_network_build_topk_tbl(specialization_tbl, portfolio_top_k = portfolio_top_k))
  if (is.null(specialization_tbl) || !nrow(specialization_tbl)) return(tibble::tibble())
  strict_rank <- function(share, score, iso) {
    o <- order(-dplyr::coalesce(share, 0), -dplyr::coalesce(score, -Inf), iso)
    r <- integer(length(o))
    r[o] <- seq_along(o)
    r
  }
  specialization_tbl %>%
    dplyr::group_by(.data$tech, .data$supply_chain) %>%
    dplyr::mutate(
      .share_rank = round(.data$production_share, share_rank_digits),
      rank_in_stage = dplyr::dense_rank(dplyr::desc(dplyr::coalesce(.data$.share_rank, 0))),
      topk_rank = strict_rank(.data$.share_rank, .data$producer_score, .data$iso3c),
      in_top_k = .data$topk_rank <= portfolio_top_k & dplyr::coalesce(.data$production_share, 0) > 0
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select("tech", "supply_chain", "iso3c", "production_share", dplyr::any_of(c("production_usd", "producer_score")),
                  "rank_in_stage", "topk_rank", "in_top_k")
}

# L5c. Solves the stages (all, or `stages` = tibble(tech, supply_chain)) with the greedy solver or
# the MILP, and, for a greedy solve of all stages with caps on, runs allied_network_design()'s
# portfolio-cap loop. greedy_impl: "fast" (dash_solve_stage_greedy(), base R with the fast RAS),
# "pipeline_env" (allied_network_solve_stage_greedy() calling the fast RAS) or "pipeline" (the
# unmodified pipeline function); all three give identical results. `forbidden` (named list by
# "tech||supply_chain") seeds the producers barred per stage (the current greedy scenario's, when
# one stage is re-solved by MILP). `progress` is called with list(event, current, total, tech,
# supply_chain, phase, iter, method). share_rank_digits: NULL reproduces
# allied_network_design(method = "greedy") exactly; a number (the dashboard uses 9, in R and in the
# browser) ranks producers for the portfolio caps on their shares rounded to that many decimals
# (dash_build_topk_tbl()), so ties between equal target shares are not decided by RAS noise.
dash_network_solve <- function(scored, params, bundle = NULL, stages = NULL, solver = c("greedy", "milp"),
                               greedy_impl = c("fast", "pipeline_env", "pipeline"), forbidden = NULL, progress = NULL,
                               milp_time_limit = NULL, solver_env = NULL, share_rank_digits = NULL) {
  solver <- match.arg(solver)
  greedy_impl <- match.arg(greedy_impl)
  np <- params$network
  if (is.null(milp_time_limit)) milp_time_limit <- np$milp_time_limit
  t_start <- Sys.time()
  if (greedy_impl != "pipeline" && is.null(solver_env)) solver_env <- dash_fast_solver_env()
  greedy_fn <- switch(
    greedy_impl,
    fast = function(...) dash_solve_stage_greedy(..., solver_env = solver_env),
    pipeline_env = solver_env$allied_network_solve_stage_greedy,
    pipeline = allied_network_solve_stage_greedy
  )
  supplier_stats <- if (greedy_impl == "pipeline") allied_network_supplier_stats else dash_supplier_stats
  diversity_lambda <- allied_network_check_lambda(np$diversity_lambda)
  if (solver == "milp" && !allied_network_has_milp()) stop("The MILP solver packages (ompr, ompr.roi, ROI, ROI.plugin.glpk) are not installed.")

  all_stages <- scored$stages
  stage_keys_all <- dash_stage_key(all_stages$tech, all_stages$supply_chain)
  if (is.null(stages)) {
    stages <- all_stages
  } else {
    stages <- tibble::as_tibble(stages)[c("tech", "supply_chain")]
    bad <- setdiff(dash_stage_key(stages$tech, stages$supply_chain), stage_keys_all)
    if (length(bad)) stop("Stage(s) not in the scenario: ", paste(bad, collapse = ", "), ".")
    stages <- stages[order(stages$tech, stages$supply_chain, method = "radix"), , drop = FALSE]
  }
  full_run <- nrow(stages) == nrow(all_stages)
  stage_keys <- dash_stage_key(stages$tech, stages$supply_chain)
  stage_count <- length(stage_keys)

  nodes_selected <- scored$nodes[c("tech", "supply_chain", "iso3c", "country", "producer_score", "demand_weight", "dev_potential",
                                   intersect(allied_network_node_extra_cols(), names(scored$nodes)))]
  edges_selected <- scored$edges[c("tech", "supply_chain", "reporter_iso", "partner_iso", "edge_weight", "friendshore_index", "opportunity_index")]
  nodes_by_stage <- split(nodes_selected, dash_stage_key(nodes_selected$tech, nodes_selected$supply_chain), drop = TRUE)
  edges_by_stage <- split(edges_selected, dash_stage_key(edges_selected$tech, edges_selected$supply_chain), drop = TRUE)

  forbidden_by_stage <- stats::setNames(rep(list(character()), stage_count), stage_keys)
  if (!is.null(forbidden)) {
    for (k in intersect(names(forbidden), stage_keys)) forbidden_by_stage[[k]] <- as.character(forbidden[[k]])
  }

  iso3c_nodes <- scored$nodes_vec
  portfolio_enable <- isTRUE(np$portfolio$enable)
  pf <- np$portfolio
  cap_by_iso <- NULL
  caps_tbl <- NULL
  if (portfolio_enable) {
    size_tbl <- if (isTRUE(scored$aggregate)) scored$node_size else if (!is.null(bundle)) bundle$network$country_size else NULL
    if (is.null(size_tbl)) stop("Portfolio caps need the node sizes (pass the bundle).")
    cap_by_iso <- allied_network_compute_portfolio_caps(
      country_size_tbl = size_tbl, iso3c_network = iso3c_nodes,
      min_cap = pf$min_cap, max_cap = pf$max_cap, transform = pf$transform, clamp = TRUE
    )
    gdp_tbl <- dplyr::distinct(nodes_selected, .data$iso3c, .data$gdp_usd, .data$gdp_imputed)
    caps_tbl <- tibble::tibble(iso3c = iso3c_nodes) %>%
      dplyr::mutate(portfolio_cap = as.integer(cap_by_iso[.data$iso3c])) %>%
      dplyr::left_join(gdp_tbl, by = "iso3c")
  }

  has_milp <- allied_network_has_milp()
  solve_one_stage <- function(i) {
    t <- stages$tech[[i]]
    sc <- stages$supply_chain[[i]]
    key <- stage_keys[[i]]
    nodes_stage <- nodes_by_stage[[key]]
    skipped <- function(reason) {
      list(tech = t, supply_chain = sc, specialization = NULL, flows = NULL, objective = NA_real_, hhi = NA_real_,
           n_producers = NA_integer_, method_used = reason, fallback_reason = NA_character_, max_production_share = NA_real_)
    }
    n_stage_nodes <- if (is.null(nodes_stage)) 0L else length(unique(nodes_stage$iso3c))
    if (n_stage_nodes < 3) return(skipped("skipped_too_few_nodes"))
    if (!any(is.finite(nodes_stage$producer_score))) return(skipped("skipped_no_node_signal"))
    edges_stage <- edges_by_stage[[key]]
    if (is.null(edges_stage)) edges_stage <- edges_selected[0, , drop = FALSE]
    nodes_core <- nodes_stage[c("iso3c", "country", "producer_score", "demand_weight", "dev_potential",
                                intersect(allied_network_node_extra_cols(), names(nodes_stage)))]
    edges_core <- edges_stage[c("reporter_iso", "partner_iso", "edge_weight", "friendshore_index", "opportunity_index")]
    fb <- forbidden_by_stage[[key]]
    fallback_reason <- NA_character_
    stage_method <- solver
    run_greedy <- function() {
      greedy_fn(
        nodes_stage = nodes_core, edges_stage = edges_core,
        min_producers = np$min_producers, max_share = np$max_share, min_share = np$min_share,
        min_suppliers_per_consumer = np$min_suppliers_per_consumer,
        epsilon_supplier_share = np$epsilon_supplier_share,
        w_edge = np$w_edge, w_node = np$w_node, w_dev = np$w_dev, allow_self = np$allow_self,
        producer_forbidden = fb, diversity_lambda = diversity_lambda
      )
    }
    if (solver == "milp") {
      setTimeLimit(cpu = milp_time_limit, elapsed = milp_time_limit, transient = TRUE)
      sol <- tryCatch(
        allied_network_solve_stage_milp(
          nodes_stage = nodes_core, edges_stage = edges_core,
          min_producers = np$min_producers, max_share = np$max_share, min_share = np$min_share,
          min_suppliers_per_consumer = np$min_suppliers_per_consumer,
          epsilon_supplier_share = np$epsilon_supplier_share, w_edge = np$w_edge,
          w_node = np$w_node, w_dev = np$w_dev, allow_self = np$allow_self,
          producer_forbidden = fb, time_limit_sec = milp_time_limit,
          diversity_lambda = diversity_lambda, diversity_breakpoint_step = np$diversity_breakpoint_step
        ),
        error = function(e) {
          setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
          stage_method <<- "greedy"
          fallback_reason <<- conditionMessage(e)
          if (is.function(progress)) progress(list(event = "fallback_greedy", tech = t, supply_chain = sc, reason = fallback_reason))
          run_greedy()
        }
      )
      setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
    } else {
      sol <- run_greedy()
    }
    share_vec <- sol$specialization$production_share
    redundancy <- supplier_stats(sol$flows, sol$specialization)
    terms <- allied_network_objective_terms(sol$specialization, sol$flows, np$w_node, np$w_edge, np$w_dev, diversity_lambda)
    list(
      tech = t, supply_chain = sc,
      specialization = sol$specialization, flows = sol$flows,
      objective = sol$objective, hhi = sol$hhi, n_producers = sol$n_producers,
      method_used = stage_method, fallback_reason = fallback_reason,
      max_production_share = if (length(share_vec)) max(share_vec, na.rm = TRUE) else NA_real_,
      n_at_max_share = sum(share_vec >= np$max_share - 1e-6, na.rm = TRUE),
      objective_node = terms$node, objective_edge = terms$edge, objective_dev = terms$dev,
      diversity_penalty = terms$diversity_penalty,
      hhi_tangent_max_error = if (is.null(sol$hhi_tangent_max_error)) NA_real_ else sol$hhi_tangent_max_error,
      min_suppliers_realized = redundancy$min_suppliers,
      min_supplier_share_realized = redundancy$min_supplier_share,
      producers_forbidden = paste(sort(fb), collapse = " ")
    )
  }

  results <- vector("list", stage_count)
  for (i in seq_len(stage_count)) {
    if (is.function(progress)) progress(list(event = "start_stage", current = i, total = stage_count, tech = stages$tech[[i]],
                                             supply_chain = stages$supply_chain[[i]], phase = "initial", iter = 0L, method = solver))
    results[[i]] <- solve_one_stage(i)
    if (is.function(progress)) progress(list(event = "end_stage", current = i, total = stage_count, tech = stages$tech[[i]],
                                             supply_chain = stages$supply_chain[[i]], phase = "initial", iter = 0L, method = results[[i]]$method_used))
  }

  bind_spec <- function(res) {
    dplyr::bind_rows(lapply(res, function(x) {
      if (is.null(x$specialization)) return(NULL)
      s <- x$specialization
      s$tech <- x$tech
      s$supply_chain <- x$supply_chain
      dplyr::relocate(s, "tech", "supply_chain", .before = "iso3c")
    }))
  }

  portfolio_converged <- TRUE
  portfolio_iters <- 0
  portfolio_violations_remaining <- 0
  counts_tbl <- NULL
  topk_tbl <- tibble::tibble()
  run_caps <- portfolio_enable && full_run && solver == "greedy"
  if (run_caps) {
    for (iter in seq_len(pf$max_iters)) {
      portfolio_iters <- iter
      topk_tbl <- dash_build_topk_tbl(bind_spec(results), portfolio_top_k = pf$top_k, share_rank_digits = share_rank_digits)
      counts_tbl <- allied_network_count_topk(topk_tbl, iso3c_nodes, cap_by_iso)
      offenders <- counts_tbl[counts_tbl$over_by > 0, , drop = FALSE]
      if (!nrow(offenders)) {
        portfolio_converged <- TRUE
        break
      }
      portfolio_converged <- FALSE
      impacted <- integer(0)
      for (r in seq_len(nrow(offenders))) {
        offender_iso <- offenders$iso3c[[r]]
        over <- offenders$over_by[[r]]
        cand <- topk_tbl %>%
          dplyr::filter(.data$in_top_k, .data$iso3c == offender_iso) %>%
          dplyr::distinct(.data$tech, .data$supply_chain, .keep_all = TRUE)
        priority_base <- if ("production_usd" %in% names(cand)) dplyr::coalesce(cand$production_usd, cand$production_share) else cand$production_share
        cand <- cand %>%
          dplyr::mutate(producer_score = dplyr::coalesce(.data$producer_score, 1), drop_priority = priority_base * .data$producer_score) %>%
          dplyr::arrange(.data$drop_priority)
        if (!nrow(cand)) next
        drops <- utils::head(cand, over)
        for (k in seq_len(nrow(drops))) {
          sk <- dash_stage_key(drops$tech[[k]], drops$supply_chain[[k]])
          forbidden_by_stage[[sk]] <- unique(c(forbidden_by_stage[[sk]], offender_iso))
          impacted <- unique(c(impacted, match(sk, stage_keys)))
        }
      }
      if (!length(impacted)) break
      for (n_done in seq_along(impacted)) {
        idx <- impacted[[n_done]]
        if (is.function(progress)) progress(list(event = "start_stage", current = n_done, total = length(impacted), tech = stages$tech[[idx]],
                                                 supply_chain = stages$supply_chain[[idx]], phase = "portfolio", iter = iter, method = solver))
        results[[idx]] <- solve_one_stage(idx)
      }
    }
    topk_tbl <- dash_build_topk_tbl(bind_spec(results), portfolio_top_k = pf$top_k, share_rank_digits = share_rank_digits)
    counts_tbl <- allied_network_count_topk(topk_tbl, iso3c_nodes, cap_by_iso)
    portfolio_violations_remaining <- sum(counts_tbl$over_by > 0, na.rm = TRUE)
    if (portfolio_violations_remaining == 0) portfolio_converged <- TRUE
  } else if (portfolio_enable) {
    topk_tbl <- dash_build_topk_tbl(bind_spec(results), portfolio_top_k = pf$top_k, share_rank_digits = share_rank_digits)
    counts_tbl <- allied_network_count_topk(topk_tbl, iso3c_nodes, cap_by_iso)
  }

  specialization_tbl <- bind_spec(results)
  flows_tbl <- dplyr::bind_rows(lapply(results, function(x) {
    if (is.null(x$flows)) return(NULL)
    f <- x$flows
    f$tech <- x$tech
    f$supply_chain <- x$supply_chain
    dplyr::relocate(f, "tech", "supply_chain", .before = "reporter_iso")
  }))
  or_na <- function(v, na) if (is.null(v) || !length(v)) na else v
  diagnostics_tbl <- dplyr::bind_rows(lapply(results, function(x) {
    tibble::tibble(
      tech = x$tech, supply_chain = x$supply_chain,
      method = x$method_used, method_requested = solver,
      fallback_reason = or_na(x$fallback_reason, NA_character_),
      producers_forbidden = or_na(x$producers_forbidden, NA_character_),
      n_producers = x$n_producers, hhi = x$hhi, objective = x$objective,
      max_production_share = or_na(x$max_production_share, NA_real_),
      min_suppliers_realized = or_na(x$min_suppliers_realized, NA_integer_),
      min_supplier_share_realized = or_na(x$min_supplier_share_realized, NA_real_),
      diversity_lambda = diversity_lambda,
      node_policy = np$node_policy,
      effective_producers = 1 / x$hhi,
      n_at_max_share = or_na(x$n_at_max_share, NA_integer_),
      objective_node = or_na(x$objective_node, NA_real_),
      objective_edge = or_na(x$objective_edge, NA_real_),
      objective_dev = or_na(x$objective_dev, NA_real_),
      diversity_penalty = or_na(x$diversity_penalty, NA_real_),
      hhi_tangent_max_error = or_na(x$hhi_tangent_max_error, NA_real_)
    )
  }))
  stage_flags <- nodes_selected %>%
    dplyr::filter(dash_stage_key(.data$tech, .data$supply_chain) %in% stage_keys) %>%
    dplyr::group_by(.data$tech, .data$supply_chain) %>%
    dplyr::summarise(
      n_nodes = dplyr::n_distinct(.data$iso3c),
      n_nodes_no_score = sum(is.na(.data$producer_score)),
      n_nodes_partial_score = sum(dplyr::between(.data$node_components_n, 1L, 2L)),
      n_policy_imputed = sum(.data$policy_imputed),
      n_demand_need_missing = sum(.data$demand_need_missing),
      .groups = "drop"
    )
  diagnostics_tbl <- dplyr::left_join(diagnostics_tbl, stage_flags, by = c("tech", "supply_chain"))
  if (portfolio_enable && !is.null(specialization_tbl) && nrow(specialization_tbl)) {
    specialization_tbl <- specialization_tbl %>%
      dplyr::left_join(counts_tbl %>% dplyr::select("iso3c", portfolio_topk_count = "topk_count", "portfolio_cap"), by = "iso3c")
    diagnostics_tbl$portfolio_converged <- if (run_caps) portfolio_converged else NA
    diagnostics_tbl$portfolio_iters <- if (run_caps) portfolio_iters else NA_integer_
    diagnostics_tbl$portfolio_violations_remaining <- if (run_caps) portfolio_violations_remaining else NA_integer_
  }

  members <- scored$members
  member_shares <- NULL
  if (!is.null(members) && !is.null(specialization_tbl) && nrow(specialization_tbl)) {
    node_info <- if (isTRUE(scored$aggregate)) {
      allied_network_node_table(members) %>% dplyr::transmute(iso3c = .data$network_node, .data$tier, .data$network_node, .data$n_members)
    } else {
      members %>% dplyr::transmute(.data$iso3c, .data$tier, .data$network_node, n_members = 1L)
    }
    specialization_tbl <- specialization_tbl %>%
      dplyr::select(-dplyr::any_of(c("tier", "network_node", "n_members"))) %>%
      dplyr::left_join(node_info, by = "iso3c", relationship = "many-to-one") %>%
      dplyr::relocate("tier", "network_node", "n_members", .after = "iso3c")
    share_weights <- if (isTRUE(scored$aggregate)) scored$member_weights else allied_network_member_weights(members, NULL, stages)
    ci <- if (!is.null(bundle)) bundle$network$country_info else NULL
    member_shares <- allied_network_disaggregate_shares(specialization_tbl, members, share_weights, ci)
  }

  out <- list(
    specialization = specialization_tbl,
    flows = flows_tbl,
    diagnostics = diagnostics_tbl,
    portfolio = if (portfolio_enable) list(caps = caps_tbl, counts = counts_tbl, topk = topk_tbl,
                                           converged = if (run_caps) portfolio_converged else NA,
                                           iters = if (run_caps) portfolio_iters else NA_integer_,
                                           violations_remaining = if (run_caps) portfolio_violations_remaining else NA_integer_) else NULL,
    member_shares = member_shares,
    forbidden = forbidden_by_stage,
    solver_by_stage = tibble::tibble(tech = stages$tech, supply_chain = stages$supply_chain,
                                     solver = vapply(results, function(x) x$method_used, character(1))),
    stages = stages,
    elapsed_sec = as.numeric(difftime(Sys.time(), t_start, units = "secs"))
  )
  class(out) <- c("dash_network", "list")
  out
}

# Replace the stages of `stage_solution` (e.g. one stage re-solved by MILP) in `solution`.
dash_network_merge_stage <- function(solution, stage_solution) {
  keys_new <- dash_stage_key(stage_solution$stages$tech, stage_solution$stages$supply_chain)
  drop_rows <- function(tbl) {
    if (is.null(tbl) || !nrow(tbl)) return(tbl)
    tbl[!dash_stage_key(tbl$tech, tbl$supply_chain) %in% keys_new, , drop = FALSE]
  }
  out <- solution
  spec_new <- stage_solution$specialization
  if (!is.null(spec_new) && !is.null(solution$portfolio$counts)) {
    spec_new <- spec_new %>%
      dplyr::select(-dplyr::any_of(c("portfolio_topk_count", "portfolio_cap"))) %>%
      dplyr::left_join(solution$portfolio$counts %>% dplyr::select("iso3c", portfolio_topk_count = "topk_count", "portfolio_cap"), by = "iso3c")
  }
  out$specialization <- dplyr::bind_rows(drop_rows(solution$specialization), spec_new) %>% dplyr::arrange(.data$tech, .data$supply_chain)
  out$flows <- dplyr::bind_rows(drop_rows(solution$flows), stage_solution$flows) %>% dplyr::arrange(.data$tech, .data$supply_chain)
  diag_new <- stage_solution$diagnostics
  for (col in setdiff(names(solution$diagnostics), names(diag_new))) diag_new[[col]] <- solution$diagnostics[[col]][match(keys_new, dash_stage_key(solution$diagnostics$tech, solution$diagnostics$supply_chain))]
  out$diagnostics <- dplyr::bind_rows(drop_rows(solution$diagnostics), diag_new[names(solution$diagnostics)]) %>% dplyr::arrange(.data$tech, .data$supply_chain)
  out$member_shares <- dplyr::bind_rows(drop_rows(solution$member_shares), stage_solution$member_shares)
  sb <- solution$solver_by_stage
  sb$solver[dash_stage_key(sb$tech, sb$supply_chain) %in% keys_new] <- stage_solution$solver_by_stage$solver[match(
    dash_stage_key(sb$tech, sb$supply_chain)[dash_stage_key(sb$tech, sb$supply_chain) %in% keys_new], keys_new)]
  out$solver_by_stage <- sb
  out
}

# Producer sets, concentration and flow turnover of a solution against a reference (e.g. the
# baseline greedy solution): per stage and per node.
dash_network_compare <- function(scenario, reference) {
  key <- function(t) dash_stage_key(t$tech, t$supply_chain)
  s <- scenario$specialization
  r <- reference$specialization
  stages <- unique(rbind(scenario$diagnostics[c("tech", "supply_chain")], reference$diagnostics[c("tech", "supply_chain")]))
  stage_rows <- lapply(seq_len(nrow(stages)), function(i) {
    k <- dash_stage_key(stages$tech[i], stages$supply_chain[i])
    ss <- s[key(s) == k, , drop = FALSE]
    rr <- r[key(r) == k, , drop = FALSE]
    ps <- ss$iso3c[ss$production_share > 0]
    pr <- rr$iso3c[rr$production_share > 0]
    nodes <- union(ss$iso3c, rr$iso3c)
    sh_s <- ss$production_share[match(nodes, ss$iso3c)]
    sh_r <- rr$production_share[match(nodes, rr$iso3c)]
    sh_s[is.na(sh_s)] <- 0
    sh_r[is.na(sh_r)] <- 0
    fs <- scenario$flows[key(scenario$flows) == k, , drop = FALSE]
    fr <- reference$flows[key(reference$flows) == k, , drop = FALSE]
    pk <- union(paste(fs$reporter_iso, fs$partner_iso), paste(fr$reporter_iso, fr$partner_iso))
    vs <- fs$flow_share[match(pk, paste(fs$reporter_iso, fs$partner_iso))]
    vr <- fr$flow_share[match(pk, paste(fr$reporter_iso, fr$partner_iso))]
    vs[is.na(vs)] <- 0
    vr[is.na(vr)] <- 0
    hhi_s <- if (length(ps)) sum(sh_s^2) else NA_real_
    hhi_r <- if (length(pr)) sum(sh_r^2) else NA_real_
    u <- union(ps, pr)
    tibble::tibble(
      tech = stages$tech[i], supply_chain = stages$supply_chain[i],
      n_producers = length(ps), n_producers_ref = length(pr),
      producers_added = paste(sort(setdiff(ps, pr)), collapse = " "),
      producers_removed = paste(sort(setdiff(pr, ps)), collapse = " "),
      jaccard = if (length(u)) length(intersect(ps, pr)) / length(u) else NA_real_,
      hhi = hhi_s, hhi_ref = hhi_r, hhi_delta = hhi_s - hhi_r,
      max_abs_share_delta = if (length(nodes)) max(abs(sh_s - sh_r)) else NA_real_,
      flow_turnover = sum(abs(vs - vr)) / 2,
      changed = !setequal(ps, pr) || (length(nodes) && max(abs(sh_s - sh_r)) > 1e-9)
    )
  })
  stage_tbl <- dplyr::bind_rows(stage_rows)
  nodes_tbl <- dplyr::full_join(
    s %>% dplyr::select("tech", "supply_chain", "iso3c", production_share = "production_share"),
    r %>% dplyr::select("tech", "supply_chain", "iso3c", production_share_ref = "production_share"),
    by = c("tech", "supply_chain", "iso3c")
  ) %>%
    dplyr::mutate(
      production_share = dplyr::coalesce(.data$production_share, 0),
      production_share_ref = dplyr::coalesce(.data$production_share_ref, 0),
      share_delta = .data$production_share - .data$production_share_ref
    )
  node_summary <- nodes_tbl %>%
    dplyr::group_by(.data$iso3c) %>%
    dplyr::summarise(
      stages_producing = sum(.data$production_share > 0),
      stages_producing_ref = sum(.data$production_share_ref > 0),
      total_share = sum(.data$production_share),
      total_share_ref = sum(.data$production_share_ref),
      total_share_delta = sum(.data$share_delta),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(abs(.data$total_share_delta)))
  list(stage = stage_tbl, node = nodes_tbl, node_summary = node_summary)
}

# Designed concentration per stage against the reference design and the observed export HHI.
dash_network_concentration <- function(solution, bundle, reference = NULL) {
  d <- solution$diagnostics[c("tech", "supply_chain", "hhi", "effective_producers", "n_producers", "method")]
  names(d)[names(d) == "hhi"] <- "hhi_designed"
  if (!is.null(reference)) {
    rd <- reference$diagnostics[c("tech", "supply_chain", "hhi")]
    names(rd)[3] <- "hhi_baseline"
    d <- dplyr::left_join(d, rd, by = c("tech", "supply_chain"))
  } else {
    d$hhi_baseline <- NA_real_
  }
  obs <- bundle$network$observed_hhi
  if (!is.null(obs) && nrow(obs)) {
    o <- obs[c("tech", "supply_chain", "hhi", "effective_exporters", "top_node", "top_share")]
    names(o)[3] <- "hhi_observed"
    d <- dplyr::left_join(d, o, by = c("tech", "supply_chain"))
  } else {
    d$hhi_observed <- NA_real_
  }
  d
}
