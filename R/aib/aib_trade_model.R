# Allied Industrial Base (AIB): the common stress-test engine and the Trade Partnership Network
# (docs/aib_methodology.md, 5-6).
#
# For one node and one shock the engine builds the observed network into the candidate universe
# and computes three states:
#   A static shock               the shocked supply disappears; nothing adapts
#   B existing-network response  surviving candidate exporters reallocate only through bilateral
#                                relationships observed in the reference year (plus a country
#                                keeping its own exports at home)
#   C optimized partnerships     any candidate exporter may serve any importer in the universe
# under two supply scenarios kept apart everywhere:
#   conservative  every surviving shipment into the universe stays put; only supply the shock frees
#                 (exports that were headed to the shocked economy) can move
#   reallocation  an exporter can redirect any of its observed global exports toward the universe
# Observed exports are revealed tradable capability, not spare physical capacity: no state assumes
# anyone produces more than they exported. All quantities are shares of the universe's baseline
# demand for the node (its importers' observed imports), so coverage 0.8 means 80% of that demand
# is served after the shock.

aib_parse_shock_key <- function(key) {
  parts <- strsplit(key, ",", fixed = TRUE)[[1]]
  iso <- sub(":.*$", "", parts)
  frac <- as.numeric(sub("^.*:", "", parts))
  stats::setNames(frac, iso)
}

aib_shock_key <- function(iso, fraction) {
  o <- order(iso)
  paste0(iso[o], ":", format(fraction[o], trim = TRUE), collapse = ",")
}

# Shock targets per hierarchy row (tech, supply_chain, sub_sector) and shock. Country shocks hit
# every node; largest-supplier shocks hit the largest exporter of the node (level subsector), of
# the node's technology stage (stage) or of its technology (technology), with trade summed over
# that level's nodes. Re-export hubs inherit the shock of the origin they re-export for.
# Returns shock_id, shock_label, shock_type, shock_level, tech, supply_chain, sub_sector, node_id,
# target_iso (the named origin), shock_key ("ISO:fraction,..." with hubs).
aib_shock_targets <- function(shocks, node_cells, exporter_metrics, reexport_hubs = list()) {
  with_hubs <- function(iso) {
    hubs <- unlist(lapply(iso, function(x) reexport_hubs[[x]]))
    unique(c(iso, hubs))
  }
  top_by <- function(keys) {
    exporter_metrics %>%
      dplyr::inner_join(node_cells, by = "node_id", relationship = "many-to-many") %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(keys, "exporter_iso")))) %>%
      dplyr::summarise(exports = sum(exports), .groups = "drop") %>%
      dplyr::slice_max(exports, n = 1, with_ties = FALSE, by = dplyr::all_of(keys)) %>%
      dplyr::select(dplyr::all_of(keys), target_iso = exporter_iso)
  }
  out <- list()
  for (s in shocks) {
    frac <- as.numeric(aib_or(s$fraction, 1))
    rows <- node_cells
    if (identical(s$type, "country")) {
      rows$target_iso <- paste(s$countries, collapse = "+")
      iso <- with_hubs(unlist(s$countries))
      rows$shock_key <- aib_shock_key(iso, rep(frac, length(iso)))
      level <- "all"
    } else if (identical(s$type, "largest_supplier")) {
      level <- aib_or(s$level, "subsector")
      keys <- switch(level,
        subsector = "node_id",
        stage = c("tech", "supply_chain"),
        technology = "tech",
        stop("aib_shock_targets(): unknown level ", level)
      )
      rows <- rows %>% dplyr::inner_join(top_by(keys), by = keys)
      rows$shock_key <- vapply(rows$target_iso, function(t) {
        iso <- with_hubs(t)
        aib_shock_key(iso, rep(frac, length(iso)))
      }, character(1), USE.NAMES = FALSE)
    } else {
      stop("aib_shock_targets(): unknown shock type ", s$type)
    }
    rows$shock_id <- s$id
    rows$shock_label <- aib_or(s$label, s$id)
    rows$shock_type <- s$type
    rows$shock_level <- level
    rows$shock_fraction <- frac
    out[[length(out) + 1]] <- rows
  }
  dplyr::bind_rows(out) %>%
    dplyr::select(shock_id, shock_label, shock_type, shock_level, shock_fraction, tech, supply_chain, sub_sector, node_id, target_iso, shock_key)
}

aib_or <- function(a, b) if (is.null(a)) b else a

# Everything about one node and one shock that does not depend on the scenario or the state.
# flows: exporter_iso, importer_iso, value (USD, reference year). shocked: named fractions.
# universe: ISO3 codes of the candidate universe, NULL = every economy in the flows.
aib_node_problem <- function(flows, shocked, universe = NULL, shocked_demand = c("release", "hold"),
                             candidate_min_exports = 1e6, candidate_min_share = 5e-4,
                             allow_domestic = TRUE) {
  shocked_demand <- match.arg(shocked_demand)
  f <- flows[flows$value > 0, c("exporter_iso", "importer_iso", "value"), drop = FALSE]
  if (!nrow(f)) return(NULL)
  W <- sum(f$value)
  econ <- unique(c(f$exporter_iso, f$importer_iso))
  U <- if (is.null(universe)) econ else intersect(universe, econ)
  sh <- shocked[names(shocked) %in% econ]
  r_of <- function(iso) { r <- unname(sh[iso]); r[is.na(r)] <- 0; r }

  D_m <- tapply(f$value, f$importer_iso, sum)
  scope <- setdiff(names(D_m)[names(D_m) %in% U], names(sh))
  if (!length(scope)) return(NULL)
  D <- sum(D_m[scope])
  if (!(D > 0)) return(NULL)

  S_e <- tapply(f$value, f$exporter_iso, sum)
  cand <- names(S_e)[names(S_e) %in% U & !names(S_e) %in% names(sh) &
                       S_e >= max(candidate_min_exports, candidate_min_share * W)]

  fs <- f[f$importer_iso %in% scope, , drop = FALSE]
  r_e <- r_of(fs$exporter_iso)
  lost_row <- r_e * fs$value
  is_cand <- fs$exporter_iso %in% cand
  fixed_row <- ifelse(is_cand, 0, fs$value - lost_row)

  lost_m <- tapply(lost_row, factor(fs$importer_iso, levels = scope), sum)
  lost_m[is.na(lost_m)] <- 0
  fixed_m <- tapply(fixed_row, factor(fs$importer_iso, levels = scope), sum)
  fixed_m[is.na(fixed_m)] <- 0
  fixed_by_exp <- tapply(fixed_row, fs$exporter_iso, sum)
  fixed_by_exp <- fixed_by_exp[fixed_by_exp > 0]
  base_supply <- tapply(fs$value, fs$exporter_iso, sum)

  to_scope <- tapply(fs$value[is_cand], factor(fs$exporter_iso[is_cand], levels = cand), sum)
  to_scope[is.na(to_scope)] <- 0
  freed <- stats::setNames(rep(0, length(cand)), cand)
  if (identical(shocked_demand, "release") && length(sh)) {
    to_sh <- f[f$exporter_iso %in% cand & f$importer_iso %in% names(sh), , drop = FALSE]
    if (nrow(to_sh)) {
      fr <- tapply(to_sh$value * r_of(to_sh$importer_iso), factor(to_sh$exporter_iso, levels = cand), sum)
      fr[is.na(fr)] <- 0
      freed <- fr
    }
  }
  links <- fs[is_cand, , drop = FALSE]

  list(
    D = D, W = W, L = sum(lost_m), scope = scope, universe_size = length(U),
    demand = D_m[scope] / D,
    lost = lost_m / D,
    fixed_m = fixed_m / D,
    fixed_by_exporter = fixed_by_exp / D,
    fixed_total = sum(fixed_row) / D,
    cand = cand,
    S = S_e[cand] / D,
    to_scope = to_scope / D,
    freed = freed / D,
    links = tibble::tibble(e = links$exporter_iso, m = links$importer_iso, x = links$value / D),
    base_supply = base_supply / D,
    shocked = sh,
    allow_domestic = allow_domestic,
    shocked_demand = shocked_demand
  )
}

aib_supply_stats <- function(by_exporter) {
  by_exporter <- by_exporter[is.finite(by_exporter) & by_exporter > 1e-12]
  if (!length(by_exporter)) {
    return(list(hhi = NA_real_, largest = NA_character_, largest_share = NA_real_, top3_share = NA_real_, n_suppliers = 0L))
  }
  s <- by_exporter / sum(by_exporter)
  k <- which.max(s)
  list(hhi = sum(s^2), largest = names(s)[k], largest_share = unname(s[k]),
       top3_share = sum(utils::head(sort(s, decreasing = TRUE), 3)), n_suppliers = length(s))
}

# State A: the shocked supply disappears and nothing else moves.
aib_static_state <- function(prob) {
  r <- prob$shocked[names(prob$base_supply)]
  r[is.na(r)] <- 0
  sup <- prob$base_supply * (1 - r)
  st <- aib_supply_stats(sup)
  c(list(coverage = 1 - prob$L / prob$D), st)
}

# Baseline (pre-shock) supply into the universe.
aib_baseline_state <- function(prob) {
  c(list(coverage = 1), aib_supply_stats(prob$base_supply))
}

# State B or C under one scenario and one supplier-share limit. In state B each existing
# relationship may grow to at most (1 + existing_growth) times its observed size (Inf: unbounded);
# with `coordinating` (ISO3 codes) state B becomes the trade-compact state: those economies may
# also open or expand relationships with any importer, everyone else keeps the B response.
# The lexicographic sequence is
# (1) demand served, (1b) the smallest cut to surviving shipments, (2) specialization, (3) Export
# Opportunity, (4) compatibility; `steps` stops after that many (1 = coverage only, 2 = coverage
# and stability). `tolerances` are those of objectives 1, 2 and 3. `scores` is a list of functions
# spec(e), op(e, m), compat(e, m) returning numeric vectors. `extra` (named, shares of demand) adds
# synthetic capacity at hosts for the investment simulation; extra hosts that are not candidates
# join the program with that capacity only. Returns coverage, supply statistics, the flows and
# the lexicographic optima.
aib_solve_state <- function(prob, scenario = c("conservative", "reallocation"), state = c("B", "C"),
                            cap = 1, scores, tolerances = c(0.0005, 0.02, 0.02), neutral = 0.5,
                            extra = NULL, steps = 5, existing_growth = Inf, tolerance_stability = 1e-6,
                            coordinating = NULL) {
  scenario <- match.arg(scenario)
  state <- match.arg(state)
  cand <- prob$cand
  hosts <- if (is.null(extra)) character() else names(extra)
  cand_all <- union(cand, hosts)
  scope <- prob$scope
  if (!length(cand_all)) {
    return(c(list(coverage = 1 - prob$L / prob$D, flows = NULL, optima = rep(NA_real_, 4), status = 0L, n_vars = 0L),
             aib_supply_stats(prob$fixed_by_exporter)))
  }

  if (identical(state, "B")) {
    vars <- prob$links[, c("e", "m")]
    if (isTRUE(prob$allow_domestic)) {
      dom <- intersect(cand, scope)
      if (length(dom)) vars <- dplyr::bind_rows(vars, tibble::tibble(e = dom, m = dom))
    }
    # A coalition (`coordinating`) may also open or expand relationships with every importer, while
    # everyone else keeps the existing-network response: the "trade compact" state.
    co <- intersect(coordinating, cand_all)
    if (length(co)) vars <- dplyr::bind_rows(vars, tidyr::expand_grid(e = co, m = scope))
    vars <- dplyr::distinct(vars)
  } else {
    vars <- tidyr::expand_grid(e = cand_all, m = scope)
    if (!isTRUE(prob$allow_domestic)) vars <- vars[vars$e != vars$m, , drop = FALSE]
  }
  n <- nrow(vars)
  if (!n) {
    return(c(list(coverage = 1 - prob$L / prob$D, flows = NULL, optima = rep(NA_real_, 4), status = 0L, n_vars = 0L),
             aib_supply_stats(prob$fixed_by_exporter)))
  }
  base <- prob$links$x[match(paste(vars$e, vars$m), paste(prob$links$e, prob$links$m))]
  base[is.na(base)] <- 0
  lower <- if (identical(scenario, "conservative")) base else rep(0, n)
  upper <- rep(Inf, n)
  # State B: an existing relationship can grow only to (1 + existing_growth) times its observed size;
  # growing it further, or opening a new one, is what state C (coordination) adds.
  if (identical(state, "B") && is.finite(existing_growth)) {
    ex <- base > 0 & !(vars$e %in% coordinating)
    upper[ex] <- base[ex] * (1 + existing_growth)
  }

  get0 <- function(v, iso) {
    if (is.null(v) || !length(v)) return(rep(0, length(iso)))
    a <- unname(v[iso])
    a[is.na(a)] <- 0
    as.numeric(a)
  }
  add <- function(iso) get0(extra, iso)
  avail <- if (identical(scenario, "conservative")) {
    get0(prob$to_scope, cand_all) + get0(prob$freed, cand_all) + add(cand_all)
  } else {
    get0(prob$S, cand_all) + add(cand_all)
  }

  # Cuts to surviving shipments (reallocation only; conservative locks them): d_k >= base - f for
  # every surviving relationship, minimized right after coverage so the optimizer moves supply only
  # where coverage or the supplier-share limit needs it, never to reshuffle unaffected trade.
  cut_idx <- if (identical(scenario, "reallocation")) which(base > 0) else integer()
  n_cut <- length(cut_idx)
  n_all <- n + n_cut

  e_idx <- match(vars$e, cand_all)
  m_idx <- match(vars$m, scope)
  rows <- list(i = integer(), j = integer(), v = numeric(), dir = character(), rhs = numeric(), nrow = 0L)
  rows$i <- c(e_idx, length(cand_all) + m_idx)
  rows$j <- c(seq_len(n), seq_len(n))
  rows$v <- rep(1, 2 * n)
  rows$dir <- rep("<=", length(cand_all) + length(scope))
  rows$rhs <- c(pmax(avail, 0), pmax(unname(prob$demand - prob$fixed_m), 0))
  rows$nrow <- length(cand_all) + length(scope)
  if (n_cut) {
    r0 <- rows$nrow
    rows$i <- c(rows$i, r0 + seq_len(n_cut), r0 + seq_len(n_cut))
    rows$j <- c(rows$j, cut_idx, n + seq_len(n_cut))
    rows$v <- c(rows$v, rep(1, 2 * n_cut))
    rows$dir <- c(rows$dir, rep(">=", n_cut))
    rows$rhs <- c(rows$rhs, base[cut_idx])
    rows$nrow <- r0 + n_cut
  }

  if (cap < 1) {
    locked_e <- tapply(lower, factor(vars$e, levels = cand_all), sum)
    locked_e[is.na(locked_e)] <- 0
    served_floor <- prob$fixed_total + sum(lower)
    for (k in seq_along(cand_all)) {
      jj <- which(e_idx == k)
      if (!length(jj)) next
      if (locked_e[k] > cap * served_floor + 1e-12) {
        # Its shipments that may not move already exceed the limit: it may not expand.
        upper[jj] <- lower[jj]
        next
      }
      others <- setdiff(seq_len(n), jj)
      rows <- aib_rows_add(rows, c(jj, others), c(rep(1 - cap, length(jj)), rep(-cap, length(others))),
                           "<=", cap * prob$fixed_total)
    }
  }

  dom_link <- vars$e == vars$m
  pad <- function(v) c(v, rep(0, n_cut))
  obj <- list(
    pad(rep(1, n)),
    c(rep(0, n), rep(-1, n_cut)),
    pad(scores$spec(vars$e)),
    pad(ifelse(dom_link, neutral, scores$op(vars$e, vars$m))),
    pad(ifelse(dom_link, 1, scores$compat(vars$e, vars$m)))
  )
  tol <- c(tolerances[1], tolerance_stability, tolerances[-1])
  sol <- aib_lp_lexicographic(obj, rows, c(lower, rep(0, n_cut)), c(upper, base[cut_idx]), tol, steps = steps)
  if (is.null(sol$x)) {
    return(c(list(coverage = NA_real_, flows = NULL, optima = sol$optima, status = sol$status, n_vars = n),
             aib_supply_stats(numeric())))
  }
  x <- pmax(sol$x[seq_len(n)], 0)
  served <- prob$fixed_total + sum(x)
  by_e <- tapply(x, vars$e, sum)
  sup <- prob$fixed_by_exporter
  both <- union(names(sup), names(by_e))
  tot <- stats::setNames(get0(sup, both) + get0(by_e, both), both)
  flows <- tibble::tibble(e = vars$e, m = vars$m, f = x, base = base)
  flows <- flows[flows$f > 1e-9 | flows$base > 0, , drop = FALSE]
  c(list(coverage = served, flows = flows, optima = sol$optima, status = sol$status, n_vars = n,
         surviving_cut = if (n_cut) sum(pmax(base[cut_idx] - x[cut_idx], 0)) else 0,
         totals = stats::setNames(as.numeric(by_e), names(by_e)), supply = tot),
    aib_supply_stats(tot))
}

# State C solved in closed form at the exporter level. With every candidate free to serve every
# importer in the universe, the program decomposes: coverage is the largest total T with
#   T <= fixed + sum_e min(avail_e, cap * T)   and   T <= fixed + sum_m (demand_m - fixed_m)
# (found by bisection; T - RHS is concave in T), and the lexicographic order then fixes each
# exporter's total: surviving shipments first (the stability rule), then the most specialized
# exporters fill the rest (objective 2). Objectives 3 and 4 only decide which importer each unit
# goes to, which aib_solve_state() (the LP) does where the importer allocation is reported. Used for
# the frontier, the investment simulation and the coalition test; agrees with the LP on coverage
# and exporter totals (tests/testthat/test-aib-trade-model.R). Requires domestic retention to be
# allowed (a complete bipartite graph); otherwise call aib_solve_state().
aib_optimized_analytic <- function(prob, scenario = c("conservative", "reallocation"), cap = 1, scores, extra = NULL) {
  scenario <- match.arg(scenario)
  get0 <- function(v, iso) {
    if (is.null(v) || !length(v)) return(rep(0, length(iso)))
    a <- unname(v[iso])
    a[is.na(a)] <- 0
    as.numeric(a)
  }
  cand_all <- union(prob$cand, if (is.null(extra)) character() else names(extra))
  fixed <- prob$fixed_total
  dcap <- sum(pmax(as.numeric(prob$demand - prob$fixed_m), 0))
  if (!length(cand_all)) {
    return(c(list(coverage = fixed, totals = numeric(), status = 0L), aib_supply_stats(prob$fixed_by_exporter)))
  }
  avail <- if (identical(scenario, "conservative")) {
    get0(prob$to_scope, cand_all) + get0(prob$freed, cand_all)
  } else {
    get0(prob$S, cand_all)
  }
  avail <- pmax(avail + get0(extra, cand_all), 0)
  base_e <- get0(tapply(prob$links$x, prob$links$e, sum), cand_all)
  lower <- if (identical(scenario, "conservative")) base_e else rep(0, length(cand_all))
  if (cap >= 1) {
    upper <- avail
    total <- min(sum(avail), dcap)
  } else {
    floor_T <- fixed + sum(lower)
    locked <- lower > cap * floor_T + 1e-12
    contrib <- function(T) ifelse(locked, lower, pmin(avail, pmax(cap * T, lower)))
    h <- function(T) min(fixed + sum(contrib(T)), fixed + dcap)
    hi <- fixed + dcap
    if (h(hi) >= hi - 1e-12) {
      T_star <- hi
    } else {
      lo <- floor_T
      for (it in 1:80) {
        mid <- (lo + hi) / 2
        if (h(mid) >= mid - 1e-12) lo <- mid else hi <- mid
      }
      T_star <- lo
    }
    upper <- contrib(T_star)
    total <- max(T_star - fixed, sum(lower))
  }
  # Stability first (keep surviving shipments up to the limit), then specialization.
  t <- pmax(lower, pmin(base_e, upper))
  rem <- total - sum(t)
  if (rem > 1e-12) {
    ord <- order(-scores$spec(cand_all), cand_all)
    for (k in ord) {
      add <- min(upper[k] - t[k], rem)
      if (add > 0) {
        t[k] <- t[k] + add
        rem <- rem - add
      }
      if (rem <= 1e-12) break
    }
  }
  names(t) <- cand_all
  sup <- prob$fixed_by_exporter
  both <- union(names(sup), cand_all)
  tot <- stats::setNames(get0(sup, both) + get0(t, both), both)
  c(list(coverage = fixed + sum(t), totals = t, supply = tot, status = 0L), aib_supply_stats(tot))
}

# Supply-weighted objective-2 weight of the candidates' optimized supply (how much of the
# optimizer-controlled supply comes from specialized exporters), and coverage if the optimized
# allocation then lost its largest supplier too (the resilience a tighter supplier limit buys).
aib_specialization_score <- function(totals, scores) {
  totals <- totals[is.finite(totals) & totals > 1e-12]
  if (!length(totals)) return(NA_real_)
  sum(totals * scores$spec(names(totals))) / sum(totals)
}
aib_second_shock_coverage <- function(coverage, largest_share) {
  if (is.na(coverage) || is.na(largest_share)) return(NA_real_)
  coverage * (1 - largest_share)
}

# All trade-model outputs for one node and one shock: per scenario the static, existing-network
# and optimized states at the headline supplier-share limit, the frontier over every limit, and
# the optimized relationships. `scores_fun(prob)` returns the score functions for the node.
aib_run_node_shock <- function(prob, scenarios, caps, headline_cap, scores, tolerances, neutral = 0.5, existing_growth = Inf,
                               tolerance_stability = 1e-6) {
  A <- aib_static_state(prob)
  base <- aib_baseline_state(prob)
  caps <- sort(unique(c(caps, headline_cap)))
  summary <- list()
  frontier <- list()
  partnerships <- list()
  flows_opt <- list()
  flows_ex <- list()
  supply <- list()
  sup_rows <- function(v, state, sc) {
    v <- v[is.finite(v) & v > 1e-12]
    if (!length(v)) return(NULL)
    tibble::tibble(scenario = sc, state = state, exporter_iso = names(v), supply_usd = as.numeric(v) * prob$D)
  }
  r_sh <- prob$shocked[names(prob$base_supply)]
  r_sh[is.na(r_sh)] <- 0
  for (sc in scenarios) {
    B <- aib_solve_state(prob, sc, "B", cap = 1, scores = scores, tolerances = tolerances, neutral = neutral,
                         existing_growth = existing_growth, tolerance_stability = tolerance_stability)
    C <- aib_solve_state(prob, sc, "C", cap = headline_cap, scores = scores, tolerances = tolerances, neutral = neutral,
                         tolerance_stability = tolerance_stability)
    exact <- isTRUE(prob$allow_domestic)
    Cs <- lapply(caps, function(cp) {
      if (isTRUE(all.equal(cp, headline_cap))) return(C)
      if (exact) aib_optimized_analytic(prob, sc, cap = cp, scores = scores) else
        aib_solve_state(prob, sc, "C", cap = cp, scores = scores, tolerances = tolerances, neutral = neutral, steps = 3)
    })
    names(Cs) <- as.character(caps)
    unc <- Cs[[as.character(max(caps))]]
    if (is.na(unc$coverage) && exact) unc <- aib_optimized_analytic(prob, sc, cap = max(caps), scores = scores)
    supply[[sc]] <- dplyr::bind_rows(
      sup_rows(prob$base_supply, "baseline", sc),
      sup_rows(prob$base_supply * (1 - r_sh), "static", sc),
      sup_rows(B$supply, "existing", sc),
      sup_rows(C$supply, "optimized", sc)
    )
    if (!is.null(B$flows) && nrow(B$flows)) flows_ex[[sc]] <- dplyr::mutate(B$flows, scenario = sc)
    redirectable <- if (identical(sc, "conservative")) sum(prob$freed) else sum(prob$S) - sum(prob$to_scope)
    newl <- if (is.null(C$flows)) 0L else sum(C$flows$base == 0 & C$flows$e != C$flows$m & C$flows$f > 1e-6)
    expl <- if (is.null(C$flows)) 0L else sum(C$flows$base > 0 & C$flows$f > C$flows$base + 1e-6)
    summary[[sc]] <- tibble::tibble(
      scenario = sc,
      demand_usd = prob$D,
      lost_usd = prob$L,
      shock_exposure = prob$L / prob$D,
      static_coverage = A$coverage,
      existing_coverage = B$coverage,
      optimized_coverage = C$coverage,
      coordination_dividend = pmax(C$coverage - B$coverage, 0),
      residual_gap = max(1 - C$coverage, 0),
      residual_gap_usd = max(1 - C$coverage, 0) * prob$D,
      existing_recovery = if (prob$L > 0) (B$coverage - A$coverage) / (prob$L / prob$D) else NA_real_,
      optimized_recovery = if (prob$L > 0) (C$coverage - A$coverage) / (prob$L / prob$D) else NA_real_,
      redirectable_supply = redirectable,
      baseline_hhi = base$hhi,
      baseline_largest_supplier = base$largest,
      baseline_largest_share = base$largest_share,
      static_hhi = A$hhi,
      existing_hhi = B$hhi,
      optimized_hhi = C$hhi,
      optimized_largest_supplier = C$largest,
      optimized_largest_share = C$largest_share,
      optimized_top3_share = C$top3_share,
      baseline_top3_share = base$top3_share,
      optimized_n_suppliers = C$n_suppliers,
      n_candidates = length(prob$cand),
      n_universe_importers = length(prob$scope),
      n_new_links = newl,
      n_expanded_links = expl,
      max_supplier_share = headline_cap,
      status_existing = B$status,
      status_optimized = C$status
    )
    frontier[[sc]] <- dplyr::bind_rows(lapply(names(Cs), function(k) {
      x <- Cs[[k]]
      tibble::tibble(
        scenario = sc, max_supplier_share = as.numeric(k),
        optimized_coverage = x$coverage, existing_coverage = B$coverage, static_coverage = A$coverage,
        coverage_loss_vs_unconstrained = pmax(unc$coverage - x$coverage, 0),
        optimized_hhi = x$hhi, optimized_largest_supplier = x$largest, optimized_largest_share = x$largest_share,
        optimized_top3_share = x$top3_share, optimized_n_suppliers = x$n_suppliers,
        specialization_score = aib_specialization_score(x$totals, scores),
        second_shock_coverage = aib_second_shock_coverage(x$coverage, x$largest_share),
        status = x$status
      )
    }))
    if (!is.null(C$flows) && nrow(C$flows)) flows_opt[[sc]] <- dplyr::mutate(C$flows, scenario = sc)
    if (!is.null(C$flows) && nrow(C$flows)) {
      fl <- C$flows
      bflow <- if (is.null(B$flows)) 0 else B$flows$f[match(paste(fl$e, fl$m), paste(B$flows$e, B$flows$m))]
      bflow[is.na(bflow)] <- 0
      fl$existing_solution <- bflow
      fl <- fl[fl$f > fl$base + 1e-6, , drop = FALSE]
      if (nrow(fl)) {
        partnerships[[sc]] <- tibble::tibble(
          scenario = sc,
          exporter_iso = fl$e,
          importer_iso = fl$m,
          baseline_usd = fl$base * prob$D,
          optimized_usd = fl$f * prob$D,
          delta_usd = (fl$f - fl$base) * prob$D,
          delta_share_of_demand = fl$f - fl$base,
          relationship = dplyr::case_when(fl$e == fl$m ~ "domestic retention", fl$base > 0 ~ "expanded", TRUE ~ "new"),
          in_existing_network_solution = fl$existing_solution > fl$base + 1e-6,
          importer_lost_share = unname(prob$lost[fl$m]),
          share_of_importer_loss_restored = ifelse(prob$lost[fl$m] > 0, (fl$f - fl$base) / prob$lost[fl$m], NA_real_)
        )
      }
    }
  }
  list(summary = dplyr::bind_rows(summary), frontier = dplyr::bind_rows(frontier), partnerships = dplyr::bind_rows(partnerships),
       flows = dplyr::bind_rows(flows_opt), flows_existing = dplyr::bind_rows(flows_ex), supply = dplyr::bind_rows(supply))
}

# Two readings of the frontier per node:
#   tightest_costless_max_share   the tightest supplier-share limit whose coverage stays within
#                                 `tolerance` of the unconstrained optimum (diversifying that far
#                                 costs nothing against this shock);
#   min_required_diversification  the loosest limit at which coverage would still reach
#                                 `acceptable_coverage` if the optimized allocation then lost its
#                                 largest supplier too (second_shock_coverage): the least
#                                 diversification that buys acceptable resilience; NA when no
#                                 tested limit reaches it. Specialization retained there is the
#                                 specialization score at that limit over the unconstrained one.
aib_frontier_minimum <- function(frontier, tolerance = 0.02, acceptable_coverage = 0.8,
                                 keys = c("node_id", "shock_id", "scenario")) {
  frontier %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(
      unconstrained_coverage = optimized_coverage[which.max(max_supplier_share)],
      tightest_costless_max_share = {
        ok <- dplyr::coalesce(coverage_loss_vs_unconstrained <= tolerance + 1e-9, FALSE)
        if (any(ok)) min(max_supplier_share[ok]) else max(max_supplier_share)
      },
      coverage_at_tightest_costless = optimized_coverage[match(tightest_costless_max_share, max_supplier_share)],
      min_required_diversification = {
        ok <- dplyr::coalesce(second_shock_coverage >= acceptable_coverage - 1e-9, FALSE)
        if (any(ok)) max(max_supplier_share[ok]) else NA_real_
      },
      specialization_retained = {
        k <- match(min_required_diversification, max_supplier_share)
        u <- specialization_score[which.max(max_supplier_share)]
        if (is.na(k) || is.na(u) || u <= 0) NA_real_ else specialization_score[k] / u
      },
      .groups = "drop"
    )
}

# Concentration of a level's supply: exporter totals (USD) summed over the level's nodes for each
# state, then HHI, largest supplier and its share (supply rows: keys, shock_id, scenario, state,
# exporter_iso, supply_usd).
aib_aggregate_concentration <- function(supply_rows, keys) {
  supply_rows %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(keys, "shock_id", "scenario", "state", "exporter_iso")))) %>%
    dplyr::summarise(v = sum(supply_usd), .groups = "drop") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(keys, "shock_id", "scenario", "state")))) %>%
    dplyr::summarise(hhi = sum((v / sum(v))^2), largest = exporter_iso[which.max(v)], largest_share = max(v) / sum(v), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = state, values_from = c(hhi, largest, largest_share), names_glue = "{state}_{.value}") %>%
    dplyr::rename_with(~ sub("_largest$", "_largest_supplier", .x))
}

# Stage and technology aggregates of the node results: demand-weighted sums of the node rows of a
# hierarchy level (a node counts once per technology it serves, so a technology total is
# "technology-attributed" where nodes are shared). Coverage is recomputed from the summed USD.
aib_aggregate_levels <- function(node_rows) {
  agg <- function(keys, level) {
    node_rows %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(keys, "shock_id", "scenario")))) %>%
      dplyr::summarise(
        n_nodes = dplyr::n_distinct(node_id),
        static_served = sum(static_coverage * demand_usd),
        existing_served = sum(existing_coverage * demand_usd),
        optimized_served = sum(optimized_coverage * demand_usd),
        lost_usd = sum(lost_usd),
        demand_usd = sum(demand_usd),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        level = level,
        shock_exposure = lost_usd / demand_usd,
        static_coverage = static_served / demand_usd,
        existing_coverage = existing_served / demand_usd,
        optimized_coverage = optimized_served / demand_usd,
        coordination_dividend = optimized_coverage - existing_coverage,
        residual_gap = 1 - optimized_coverage,
        residual_gap_usd = demand_usd - optimized_served
      ) %>%
      dplyr::select(-static_served, -existing_served, -optimized_served)
  }
  dplyr::bind_rows(
    agg(c("tech", "supply_chain"), "stage"),
    agg("tech", "technology")
  )
}
