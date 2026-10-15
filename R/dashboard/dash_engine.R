# Dashboard orchestration: one scenario run (L1-L4), comparisons with the baseline, download
# tables and the bundle schema check (pure; no IO).

# A small memo cache for the engine layers (an environment; first in, first out). A script keeps one
# per R process; nothing is written to disk.
dash_cache_new <- function(max_items = 24L) {
  env <- new.env(parent = emptyenv())
  env$.keys <- character()
  env$.max <- as.integer(max_items)
  env
}

dash_cached <- function(cache, key, expr) {
  if (is.null(cache)) return(force(expr))
  if (exists(key, envir = cache, inherits = FALSE)) return(get(key, envir = cache, inherits = FALSE))
  val <- force(expr)
  assign(key, val, envir = cache)
  cache$.keys <- c(cache$.keys, key)
  while (length(cache$.keys) > cache$.max) {
    rm(list = cache$.keys[1], envir = cache)
    cache$.keys <- cache$.keys[-1]
  }
  val
}

# One scenario through L1-L4 (the network is solved separately: dash_network_*()). Returns a
# "dash_result": params, pillars, policy, si, allied (selection), partner (cells, top5, psi) and the
# layer keys. partner = FALSE skips L4.
dash_run <- function(bundle, params, cache = NULL, partner = TRUE) {
  keys <- list(
    pillars = dash_params_key(params, "pillars"),
    policy = dash_params_key(params, "policy"),
    si = dash_params_key(params, "si"),
    fs_cells = dash_params_key(params, "fs_cells"),
    po_cells = dash_params_key(params, "po_cells"),
    psi = dash_params_key(params, "psi")
  )
  keys$top5 <- paste0("top5:", keys$fs_cells, "|", keys$po_cells, "|",
                      paste(params$partner$exclude_techs, collapse = ","), "|", params$partner$top_k)
  pillars <- dash_cached(cache, keys$pillars, dash_pillars(bundle, params))
  policy <- dash_cached(cache, keys$policy, dash_policy(bundle, params))
  si <- dash_cached(cache, keys$si, dash_si(bundle, params, pillars, policy))
  allied <- dash_allied_selection(si, bundle, params)
  out <- list(params = params, pillars = pillars, policy = policy, si = si, allied = allied, partner = NULL, keys = keys)
  if (isTRUE(partner)) {
    fs <- dash_cached(cache, keys$fs_cells, dash_fs_cells(bundle, params))
    po <- dash_cached(cache, keys$po_cells, dash_po_cells(bundle, params))
    cells <- list(fs_index = fs, po_index = po)
    top5 <- dash_cached(cache, keys$top5, dash_partner_top5(bundle, cells, params))
    psi <- dash_cached(cache, keys$psi, dash_psi(bundle, top5, params))
    out$partner <- list(cells = cells, top5 = top5, psi = psi)
  }
  notes <- character()
  if (isTRUE(params$si$fossil_excluded)) notes <- c(notes, "fossil technologies excluded")
  if (isFALSE(params$si$require_resource_base)) notes <- c(notes, "endowment screen off")
  base_adv <- bundle$manifest$baseline$advanced
  if (!is.null(base_adv) && (!identical(params$pillars$es_weights, base_adv$es_weights) ||
                             !identical(params$pillars$eo_weights, base_adv$eo_weights))) {
    notes <- c(notes, "partner layer uses baseline pillars")
  }
  attr(out, "notes") <- notes
  class(out) <- c("dash_result", "list")
  out
}

# The full network for a scenario: inputs, scores and the solve (greedy by default).
dash_network_run <- function(bundle, params, result, solver = "greedy", progress = NULL, cache = NULL, solver_env = NULL) {
  k_in <- dash_params_key(params, "net_inputs")
  k_sc <- dash_params_key(params, "net_score")
  k_so <- paste0(dash_params_key(params, "net_solve"), "|", solver)
  inputs <- dash_cached(cache, k_in, dash_network_inputs(bundle, params, result$partner$cells, policy = result$policy))
  scored <- dash_cached(cache, k_sc, dash_network_score(inputs, params))
  dash_cached(cache, k_so, dash_network_solve(scored, params, bundle = bundle, solver = solver, progress = progress, solver_env = solver_env))
}

dash_join_base <- function(x, b, keys, cols, suffix = "_base") {
  bk <- do.call(paste, c(lapply(keys, function(k) b[[k]]), sep = "\r"))
  xk <- do.call(paste, c(lapply(keys, function(k) x[[k]]), sep = "\r"))
  m <- match(xk, bk)
  for (cl in cols) x[[paste0(cl, suffix)]] <- b[[cl]][m]
  x
}

# Scenario against the baseline result: deltas and rank changes for the SI, the pillars, the
# Policy index, the partner-level PSI and the allied selection.
dash_compare <- function(result, baseline_result) {
  k3 <- c("Country", "tech", "supply_chain")
  si <- dash_join_base(result$si$cells, baseline_result$si$cells, k3, c("strategic_index", "rank_in_country", "in_sample"))
  si$delta <- si$strategic_index - si$strategic_index_base
  si$rank_delta <- si$rank_in_country_base - si$rank_in_country   # positive = moved up
  es <- dash_join_base(result$pillars$es, baseline_result$pillars$es, k3, c("value", "rankable"))
  es$delta <- es$value - es$value_base
  eo <- dash_join_base(result$pillars$eo, baseline_result$pillars$eo, k3, c("value", "rankable"))
  eo$delta <- eo$value - eo$value_base
  pol <- dash_join_base(result$policy$index, baseline_result$policy$index, k3, "value")
  pol$delta <- pol$value - pol$value_base
  allied <- dash_join_base(result$allied, baseline_result$allied, "Country", c("sector", "strategic_index"))
  allied$changed <- is.na(allied$sector_base) | allied$sector != allied$sector_base
  out <- list(si = si, es = es, eo = eo, policy = pol, allied = allied)
  if (!is.null(result$partner) && !is.null(baseline_result$partner)) {
    pp <- dash_join_base(result$partner$psi$partner, baseline_result$partner$psi$partner, "partner_iso",
                         c("psi", "rank", "friendshore", "opportunity"))
    pp$delta <- pp$psi - pp$psi_base
    pp$rank_delta <- pp$rank_base - pp$rank
    out$psi_partner <- pp
  }
  out
}

# One reporter's dyads against the baseline (for the Partners view).
dash_compare_dyads <- function(result, baseline_result, reporter_iso) {
  d <- result$partner$psi$dyad
  d <- d[d$reporter_iso == reporter_iso, , drop = FALSE]
  b <- baseline_result$partner$psi$dyad
  b <- b[b$reporter_iso == reporter_iso, , drop = FALSE]
  d <- dash_join_base(d, b, "partner_iso", c("psi", "rank", "friendshore", "opportunity"))
  d$delta <- d$psi - d$psi_base
  d$rank_delta <- d$rank_base - d$rank
  d
}

# Parameters as a long table (section, parameter, value) for downloads.
dash_params_long <- function(params) {
  p <- unclass(params)
  flat <- unlist(p[c("dials", "m", "pillars", "policy", "si", "partner", "network")])
  tibble::tibble(
    section = sub("\\..*$", "", names(flat)),
    parameter = sub("^[^.]*\\.", "", names(flat)),
    value = as.character(flat)
  )
}

# Tidy tables of a scenario for download.
dash_scenario_tables <- function(result, network = NULL, compare = NULL, bundle = NULL) {
  out <- list(parameters = dash_params_long(result$params))
  if (!is.null(bundle)) {
    m <- bundle$manifest
    out$data_vintage <- tibble::tibble(
      item = c("bundle_created", "git_commit", "osi_branch", "notes"),
      value = c(format(m$created), dash_or(m$git_commit, NA_character_), dash_or(m$osi_branch, NA_character_),
                paste(m$notes, collapse = " | "))
    )
  }
  out$strategic_index <- if (!is.null(compare)) compare$si else result$si$cells
  out$strategic_excluded <- result$si$excluded
  out$allied_selection <- if (!is.null(compare)) compare$allied else result$allied
  out$energy_security <- if (!is.null(compare)) compare$es else result$pillars$es
  out$economic_opportunity <- if (!is.null(compare)) compare$eo else result$pillars$eo
  out$es_contributions <- result$pillars$es_contrib
  out$eo_contributions <- result$pillars$eo_contrib
  out$policy_index <- if (!is.null(compare)) compare$policy else result$policy$index
  if (!is.null(result$partner)) {
    out$psi_partner <- if (!is.null(compare$psi_partner)) compare$psi_partner else result$partner$psi$partner
    out$psi_dyad <- result$partner$psi$dyad
  }
  if (!is.null(network)) {
    out$network_specialization <- network$specialization
    out$network_flows <- network$flows
    out$network_diagnostics <- network$diagnostics
    if (!is.null(network$member_shares)) out$network_member_shares <- network$member_shares
  }
  lapply(out, tibble::as_tibble)
}

# Schema check of a bundle: stops naming what is missing or malformed.
dash_validate_bundle <- function(bundle) {
  need <- function(cond, msg) if (!isTRUE(cond)) stop("Invalid dashboard bundle: ", msg, call. = FALSE)
  need(is.list(bundle), "not a list")
  for (s in c("manifest", "dims", "pillars", "policy", "si", "partner", "network")) need(!is.null(bundle[[s]]), paste0("missing $", s))
  need(!is.null(bundle$manifest$baseline_inputs), "missing manifest$baseline_inputs")
  bi <- bundle$manifest$baseline_inputs
  for (k in c("es_weights", "eo_weights", "policy_weights", "si_weights", "stage_weights", "fs_weights", "po_weights",
              "psi_weights", "node_weights", "edge_weights", "demand_weights", "lambda", "portfolio", "supply_chains")) {
    need(!is.null(bi[[k]]), paste0("baseline_inputs$", k, " missing"))
  }
  need(!is.null(bundle$dims$techs) && length(bundle$dims$techs) > 0, "dims$techs empty")
  for (p in c("es", "eo")) {
    pb <- bundle$pillars[[p]]
    need(!is.null(pb), paste0("pillars$", p, " missing"))
    n <- nrow(pb$cells)
    k <- length(pb$categories)
    for (mname in c("score", "present", "cw_imp", "cw_tot")) {
      need(identical(dim(pb[[mname]]), c(n, k)), paste0("pillars$", p, "$", mname, " has the wrong shape"))
    }
  }
  need(identical(dim(bundle$policy$values), c(nrow(bundle$policy$cells), length(bundle$policy$variables))), "policy$values has the wrong shape")
  need(is.null(bundle$policy$components) || all(bundle$policy$components %in% bundle$policy$variables),
       "policy$components names a component the bundle lacks")
  rb <- bundle$si$resource_base
  if (!is.null(rb)) {
    need(is.data.frame(rb) && all(c("Country", "tech", "supply_chain", "resource_base") %in% names(rb)), "si$resource_base lacks columns")
    need(is.logical(rb$resource_base) && !anyNA(rb$resource_base), "si$resource_base$resource_base must be TRUE / FALSE")
    need(!anyDuplicated(paste(rb$Country, rb$tech, rb$supply_chain, sep = "\r")), "si$resource_base has duplicated cells")
  }
  pa <- bundle$partner
  U <- length(pa$U)
  C <- nrow(pa$cells)
  need(identical(dim(pa$fs$imp_trade), c(U, U, C)), "partner$fs$imp_trade has the wrong shape")
  need(identical(dim(pa$fs$mask), c(U, U, C)), "partner$fs$mask has the wrong shape")
  need(identical(dim(pa$po$trade), c(U, U, C)), "partner$po$trade has the wrong shape")
  need(identical(dim(pa$po$mask), c(U, U, C)), "partner$po$mask has the wrong shape")
  need(length(pa$fs$cpi) == U && length(pa$fs$gh) == C, "partner$fs cpi / gh lengths")
  # Friendshore's CM-FDI parts (dash_fs_outbound(), dash_fs_adj()): absent in a build before them
  src <- pa$fs$outbound_source
  if (!is.null(src)) {
    need(is.character(src) && length(src) == C && all(src %in% c("imf_dip", "cm_fdi")), "partner$fs$outbound_source must be imf_dip / cm_fdi by cell")
    K <- sum(src == "cm_fdi")
    need(if (K > 0) identical(dim(pa$fs$outbound_cmfdi), c(U, U, K)) else is.null(pa$fs$outbound_cmfdi),
         "partner$fs$outbound_cmfdi must be [U, U, number of cm_fdi cells]")
  } else {
    need(is.null(pa$fs$outbound_cmfdi), "partner$fs$outbound_cmfdi without outbound_source")
  }
  if (!is.null(pa$fs$cpe)) {
    need(identical(dim(pa$fs$cpe), c(U, C)), "partner$fs$cpe must be [U, C]")
    need(all(is.na(pa$fs$cpe) | (pa$fs$cpe >= 0 & pa$fs$cpe <= 1)), "partner$fs$cpe must lie in [0, 1]")
  }
  need(length(pa$po$cpi) == U && length(pa$po$gh) == C, "partner$po cpi / gh lengths")
  need(is.data.frame(bundle$network$members) && nrow(bundle$network$members) > 0, "network$members empty")
  need(is.data.frame(bundle$network$node_raw_member), "network$node_raw_member missing")
  invisible(TRUE)
}
