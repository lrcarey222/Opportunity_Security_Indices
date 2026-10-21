# Allied Industrial Base (AIB): the strategic layer (docs/aib_methodology.md, section 16).
#
# An interpretive layer ABOVE the AIB outputs of scripts/31-32, which it reads and never changes:
#   * Strategic Sectoral Compacts: persistent institutional frameworks around one physically coherent
#     value chain (config/aib_strategic_compacts.yml, curated, not discovered). The 33 compact candidates
#     of scripts/32 (AIB-Cxx) become the WORKSTREAMS inside them. A compact's id, nodes and steps are the
#     same in every scenario; a scenario sets only each node's status and the compact's portfolio
#     (class counts), missions, policy menu, coverage and critical partners.
#   * Strategic AIB Roles: seven roles a country can play in the whole system, each assigned by a
#     transparent two-gate percentile rule over named evidence (no composite score), at most 3 primary
#     and 2 secondary roles per country.
#   * Compact-specific country roles: what a country contributes inside each strategic compact, from
#     node-level evidence (the variable geometry).
#   * Template narratives: slot-filled from the same tables, with no country-specific text.
# Pure: every function takes tables and returns tables (scripts/33 does the IO). The method was
# calibrated on the 2026-10-20 run (role thresholds .90 / .75; config/aib_strategic_layer.yml).

aib_st_or <- function(a, b) if (is.null(a) || !length(a)) b else a

# ---- Vocabulary -----------------------------------------------------------------------------------------

aib_strategic_roles <- function() {
  c(RPA = "Resource & Processing Anchor", STM = "Specialized Technology & Manufacturing Anchor",
    SMI = "Scale Manufacturing & Integration Platform", DMA = "Demand & Market Anchor",
    CIA = "Capital & Investment Anchor", DSP = "Development & Scale-Up Platform", CBH = "Coordination / Bridge Hub")
}
aib_strategic_role_nouns <- function() {
  c(RPA = "resource & processing", STM = "specialized technology", SMI = "scale manufacturing", DMA = "demand & market",
    CIA = "capital & investment", DSP = "development & scale-up", CBH = "coordination / bridge")
}
# The roles a non-allied major supplier is assessed on (it appears as a supplier and a dependency, never as a member).
aib_strategic_supply_roles <- function() c("RPA", "STM", "SMI")
# Each role's two gate metrics: size first, then breadth / depth. Both must clear the bar; never averaged.
aib_strategic_gates <- function() {
  list(RPA = c("rp_positions", "rp_mean_share"), STM = c("spec_value_share", "spec_compacts"),
       SMI = c("mfg_value_share", "integration_compacts"), DMA = c("demand_mean_share", "net_import_end"),
       CIA = c("cmfdi_out_usd_bn", "fdi_out_usd_bn"), DSP = c("host_gap_usd_bn", "scaleup_rate"),
       CBH = c("ws_bridge_vertical", "sc_span"))
}
# Supporting evidence, reported only.
aib_strategic_support <- function() {
  list(RPA = "ws_resource", STM = c("spec_positions", "ws_specialized", "ws_critical"), SMI = "mfg_positions",
       DMA = c("demand_positions", "ws_demand_anchor", "net_import_mean", "net_import_mf", "net_import_rp"), CIA = "cmfdi_destinations",
       DSP = c("host_top_scaled", "momentum_nodes", "presence_nodes", "host_capable_gap_nodes"),
       CBH = c("ws_member", "ws_bridge", "sc_span_supply"))
}
aib_strategic_gate_labels <- function() {
  c(rp_positions = "resource & processing positions", rp_mean_share = "share of world resource & processing supply",
    spec_value_share = "value of specialist component positions", spec_compacts = "compacts with a specialist position",
    mfg_value_share = "share of world manufacturing exports in the mapped chains", integration_compacts = "integrated compacts",
    demand_mean_share = "share of allied demand", net_import_end = "net imports of end-use goods", net_import_mf = "net imports of manufactured goods",
    cmfdi_out_usd_bn = "outward cleantech-manufacturing FDI", fdi_out_usd_bn = "outward direct investment",
    host_gap_usd_bn = "unmet allied demand it can host", scaleup_rate = "scale-up rate",
    ws_bridge_vertical = "workstreams it bridges", sc_span = "strategic compacts it joins")
}
aib_strategic_functions <- function() c("resource", "processing", "components", "systems")
aib_strategic_function_labels <- function() {
  c(resource = "upstream resource", processing = "processing", components = "component, material and equipment",
    systems = "assembled system and end-product")
}
aib_strategic_mfg <- function() c("components", "systems")
# Compact-specific functions (task section 11), in the order they are tested.
aib_strategic_compact_functions <- function() {
  c(f_resource = "Resource supplier", f_processing = "Processing anchor", f_specialized = "Specialized technology supplier",
    f_scale = "Scale manufacturer", f_demand = "Demand anchor", f_investment = "Investment anchor",
    f_host = "Capacity-building host", f_bridge = "Bridge / coordination partner", f_divers = "Diversification partner")
}
aib_strategic_scenario_ids <- function() c("baseline", "managed", "partial", "decoupling")
# Node statuses on a spine: the five AIB classes plus the two scenario statuses that are not classes.
# NOT TESTED (2026-10-21): a Managed Interdependence node left alone only because its benchmark is concentrated too.
aib_strategic_statuses <- function() c(aib_node_classes(), "NOT STRESSED", "NOT TESTED", "NO SHOCK")
aib_strategic_intervention <- function() c("BUILD", "MANAGE SPECIALIZATION", "COORDINATE")

# Role and node bars (config/aib_strategic_layer.yml `roles` overrides any of them).
aib_strategic_default_params <- function() {
  list(p_primary = 0.90, p_secondary = 0.75, max_primary = 3L, max_secondary = 2L, lead_margin = 0.03,
       position_min = 0.10, spec_rca = 1.5, presence_min = 0.05, demand_node_min = 0.10,
       host_scale_min = 0.02, momentum_gain = 0.01, momentum_floor = 0.02, momentum_ratio = 1.25,
       major_supplier_single = 0.10, major_supplier_multi = 0.05, major_supplier_n = 3L)
}
aib_strategic_params <- function(cfg = NULL) {
  p <- aib_strategic_default_params()
  for (k in names(cfg)) p[[k]] <- cfg[[k]]
  p
}

# "a", "a and b", "a, b and c" (labels holding a comma are separated by semicolons).
aib_st_and <- function(x) {
  x <- as.character(x[!is.na(x) & nzchar(x)])
  if (length(x) <= 1) return(paste(x, collapse = ""))
  sep <- if (any(grepl(",", x, fixed = TRUE))) "; " else ", "
  paste0(paste(x[-length(x)], collapse = sep), " and ", x[length(x)])
}
aib_st_cap <- function(x) ifelse(is.na(x) | !nzchar(x), x, paste0(toupper(substr(x, 1, 1)), substring(x, 2)))
# Lower-case the first letter for use inside a sentence, except an acronym ("PV modules", "GOES").
aib_st_lcfirst <- function(x) ifelse(is.na(x) | !nzchar(x) | grepl("^([A-Z][A-Z0-9]|Li-ion)", x), x, paste0(tolower(substr(x, 1, 1)), substring(x, 2)))
# "a" or "an" before a word (by its first letter).
aib_st_article <- function(x) ifelse(grepl("^[aeiouAEIOU]", x), "an", "a")
aib_st_pct <- function(x) sprintf("%.0f%%", 100 * x)
# A share for prose: "<1%" for a positive share that rounds to 0%.
aib_st_share <- function(x) ifelse(!is.na(x) & x > 0 & x < 0.005, "<1%", aib_st_pct(x))

# ---- The compact map ------------------------------------------------------------------------------------

# One row per compact x step x node of config/aib_strategic_compacts.yml (core and extended scope).
aib_strategic_map <- function(y) {
  rows <- list()
  for (i in seq_along(y$compacts)) {
    k <- y$compacts[[i]]
    for (s in k$value_chain) for (scope in c("core", "extended")) {
      ids <- unlist(if (scope == "core") s$nodes else s$extended_nodes)
      if (!length(ids)) next
      rows[[length(rows) + 1]] <- tibble::tibble(
        strategic_compact_id = k$strategic_compact_id, strategic_compact_name = k$strategic_compact_name,
        family = aib_st_or(k$family, NA_character_), compact_order = i, value_chain_order = as.integer(s$value_chain_order),
        value_chain_step = as.character(s$step), step_description = aib_st_or(s$description, NA_character_),
        node_id = as.character(ids), scope = scope)
    }
  }
  if (!length(rows)) {
    return(tibble::tibble(strategic_compact_id = character(), strategic_compact_name = character(), family = character(),
                          compact_order = integer(), value_chain_order = integer(), value_chain_step = character(),
                          step_description = character(), node_id = character(), scope = character()))
  }
  dplyr::bind_rows(rows)
}

# One row per compact: id, name, family, purpose, order and the step labels of the whole chain.
aib_strategic_compact_defs <- function(y) {
  dplyr::bind_rows(lapply(seq_along(y$compacts), function(i) {
    k <- y$compacts[[i]]
    steps <- vapply(k$value_chain, function(s) as.character(s$step), "")
    tibble::tibble(strategic_compact_id = k$strategic_compact_id, strategic_compact_name = k$strategic_compact_name,
                   family = aib_st_or(k$family, NA_character_), purpose = aib_st_or(k$purpose, NA_character_),
                   compact_order = i, n_steps_configured = length(steps), value_chain_configured = paste(steps, collapse = " -> "))
  }))
}

# The map resolved for the nodes of a run, with the validation contract of the yml header.
#   run_nodes      the run's node ids (inputs$nodes$node_id)
#   aggregated     tibble(node_id = member sub-sector, aggregated_into = run node) of the data-quality merges
#   member_weight  named numeric (member sub-sector -> weight, e.g. world trade) for an aggregate the map
#                  does not list: it takes the compact and step of its heaviest mapped member
# Returns list(map = one row per mapped run node, unmapped = run nodes listed as unmapped, problems =
# errors (scripts/33 stops on them), notes = what was resolved by a fallback).
aib_strategic_validate_map <- function(y, run_nodes, aggregated = NULL, member_weight = NULL) {
  map <- aib_strategic_map(y)
  problems <- character()
  notes <- character()
  ids <- vapply(y$compacts, function(k) aib_st_or(k$strategic_compact_id, NA_character_), "")
  if (anyNA(ids) || any(!nzchar(ids))) problems <- c(problems, "a compact has no strategic_compact_id")
  if (any(duplicated(ids))) problems <- c(problems, paste("duplicate strategic_compact_id:", paste(unique(ids[duplicated(ids)]), collapse = ", ")))
  for (k in y$compacts) {
    o <- vapply(k$value_chain, function(s) as.numeric(aib_st_or(s$value_chain_order, NA)), 0)
    if (anyNA(o) || any(diff(o) <= 0)) problems <- c(problems, sprintf("%s: value_chain_order is not strictly increasing", k$strategic_compact_id))
  }
  dup <- unique(map$node_id[duplicated(map$node_id)])
  if (length(dup)) problems <- c(problems, paste("node mapped more than once:", paste(dup, collapse = ", ")))
  unm <- vapply(aib_st_or(y$unmapped, list()), function(u) as.character(u$node_id), "")
  both <- intersect(unm, map$node_id)
  if (length(both)) problems <- c(problems, paste("node both mapped and unmapped:", paste(both, collapse = ", ")))
  for (o in aib_st_or(y$workstream_overrides, list())) {
    bad <- setdiff(c(o$primary_compact, unlist(o$also_spans)), ids)
    if (length(bad)) problems <- c(problems, sprintf("workstream override %s names unknown compacts: %s", aib_st_or(o$workstream_id, "?"), paste(bad, collapse = ", ")))
  }
  run_map <- map[map$node_id %in% run_nodes, , drop = FALSE]
  run_map$resolution <- rep("listed", nrow(run_map))
  missing <- setdiff(run_nodes, c(run_map$node_id, unm))
  for (n in missing) {
    members <- if (is.null(aggregated)) character() else aggregated$node_id[aggregated$aggregated_into %in% n]
    m <- map[map$node_id %in% members, , drop = FALSE]
    if (grepl("--aggregated$", n) && nrow(m)) {
      w <- if (is.null(member_weight)) rep(0, nrow(m)) else dplyr::coalesce(as.numeric(member_weight[m$node_id]), 0)
      pick <- m[order(-w, m$node_id), , drop = FALSE][1, ]
      pick$node_id <- n
      pick$resolution <- paste0("aggregate: compact and step of its member ", m$node_id[order(-w, m$node_id)][1])
      run_map <- dplyr::bind_rows(run_map, pick)
      notes <- c(notes, sprintf("%s is not listed: placed with its member %s (%s, step %d)", n, m$node_id[order(-w, m$node_id)][1],
                                pick$strategic_compact_id, pick$value_chain_order))
    } else {
      problems <- c(problems, sprintf("run node %s is neither mapped to a compact nor listed as unmapped", n))
    }
  }
  listed_absent <- setdiff(map$node_id, run_nodes)
  if (length(listed_absent)) notes <- c(notes, sprintf("%d listed node ids are not nodes of this run (extended scope or merged sub-sectors)", length(listed_absent)))
  empty_steps <- dplyr::anti_join(dplyr::distinct(map, strategic_compact_id, value_chain_order, value_chain_step),
                                  dplyr::distinct(run_map, strategic_compact_id, value_chain_order), by = c("strategic_compact_id", "value_chain_order"))
  if (nrow(empty_steps)) notes <- c(notes, sprintf("%d steps have no node in this run and are left off the spine: %s", nrow(empty_steps),
                                                   paste(sprintf("%s/%s", empty_steps$strategic_compact_id, empty_steps$value_chain_step), collapse = ", ")))
  empty_compacts <- setdiff(ids, run_map$strategic_compact_id)
  if (length(empty_compacts)) notes <- c(notes, paste("compacts with no node in this run:", paste(empty_compacts, collapse = ", ")))
  run_map <- run_map[order(run_map$compact_order, run_map$value_chain_order, match(run_map$node_id, map$node_id)), , drop = FALSE]
  list(map = run_map, unmapped = intersect(unm, run_nodes), problems = problems, notes = notes)
}

# Node functions (config/aib_node_functions.yml): node_id -> resource / processing / components / systems,
# keyed by node id. A mapped node the file does not list falls back on its dictionary stage (with a note);
# a value outside the four is a problem. node_stage: named character (node_id -> Upstream / Midstream / Downstream).
aib_strategic_node_functions <- function(nf, map_run, node_stage = NULL) {
  vals <- unlist(aib_st_or(nf$nodes, list()))
  fb <- unlist(aib_st_or(nf$fallback_by_stage, list(Upstream = "resource", Midstream = "components", Downstream = "systems")))
  problems <- character()
  notes <- character()
  bad <- names(vals)[!vals %in% aib_strategic_functions()]
  if (length(bad)) problems <- c(problems, paste("node function outside resource / processing / components / systems:", paste(bad, collapse = ", ")))
  ids <- map_run$node_id
  fun <- unname(vals[ids])
  basis <- ifelse(is.na(fun), NA_character_, "config/aib_node_functions.yml")
  miss <- ids[is.na(fun)]
  for (n in miss) {
    st <- if (is.null(node_stage)) NA_character_ else unname(node_stage[n])
    f <- if (is.na(st)) NA_character_ else unname(fb[st])
    if (is.null(f) || is.na(f)) {
      problems <- c(problems, sprintf("mapped node %s has no function and no stage to fall back on", n))
    } else {
      fun[ids == n] <- f
      basis[ids == n] <- paste0("fallback: dictionary stage ", st)
      notes <- c(notes, sprintf("%s has no function in config/aib_node_functions.yml: %s from its stage (%s)", n, f, st))
    }
  }
  list(functions = tibble::tibble(node_id = ids, node_function = fun, function_basis = basis), problems = problems, notes = notes)
}

# ---- Workstreams ------------------------------------------------------------------------------------------

# Each compact candidate of scripts/32 (AIB-Cxx) becomes a workstream of exactly one strategic compact: by
# default the compact holding the largest share of its allied demand (classification demand_usd; node count
# when no node has demand); a `workstream_overrides` entry whose `nodes` equal the workstream's node set takes
# precedence, whatever the id (an override no workstream matches is stale and reported).
#   compact_nodes  res$compact_nodes (compact_id, node_id)
#   map_run        aib_strategic_validate_map()$map
#   node_demand    named numeric (node_id -> allied demand USD)
#   overrides      the yml's workstream_overrides
# Returns list(workstreams, stale).
aib_strategic_workstreams <- function(compact_nodes, map_run, node_demand, overrides = NULL, compact_order = NULL) {
  cn <- compact_nodes[!is.na(compact_nodes$compact_id) & nzchar(compact_nodes$compact_id), c("compact_id", "node_id")]
  cn <- cn[!duplicated(cn), , drop = FALSE]
  cn$sc_id <- map_run$strategic_compact_id[match(cn$node_id, map_run$node_id)]
  cn$demand <- dplyr::coalesce(as.numeric(node_demand[cn$node_id]), 0)
  if (is.null(compact_order)) compact_order <- unique(map_run$strategic_compact_id)
  ws_ids <- sort(unique(cn$compact_id))
  ov_sets <- lapply(aib_st_or(overrides, list()), function(o) sort(as.character(unlist(o$nodes))))
  used_ov <- rep(FALSE, length(ov_sets))
  rows <- lapply(ws_ids, function(w) {
    x <- cn[cn$compact_id == w, , drop = FALSE]
    nodes <- sort(unique(x$node_id))
    m <- x[!is.na(x$sc_id), , drop = FALSE]
    sp <- if (nrow(m)) {
      d <- tapply(m$demand, m$sc_id, sum)
      n <- tapply(m$demand, m$sc_id, length)
      sh <- if (sum(d) > 0) d / sum(d) else n / sum(n)
      sh[order(-sh, match(names(sh), compact_order))]
    } else stats::setNames(numeric(), character())
    hit <- which(vapply(ov_sets, function(s) identical(s, nodes), logical(1)))
    if (length(hit)) {
      o <- overrides[[hit[1]]]
      used_ov[hit[1]] <<- TRUE
      primary <- as.character(o$primary_compact)
      also <- union(as.character(unlist(o$also_spans)), setdiff(names(sp), primary))
      basis <- "override (config/aib_strategic_compacts.yml workstream_overrides)"
      reason <- aib_st_or(o$reason, NA_character_)
    } else {
      primary <- if (length(sp)) names(sp)[1] else NA_character_
      also <- setdiff(names(sp), primary)
      basis <- if (length(sp)) "largest share of allied demand" else "no node in a strategic compact"
      reason <- NA_character_
    }
    tibble::tibble(
      workstream_id = w, strategic_compact_id = primary,
      assignment_basis = basis,
      primary_demand_share = if (!is.na(primary) && primary %in% names(sp)) unname(sp[primary]) else NA_real_,
      demand_split = if (length(sp)) paste(sprintf("%s %s", names(sp), aib_st_pct(sp)), collapse = "; ") else NA_character_,
      also_spans = paste(also[order(match(also, compact_order))], collapse = " "),
      straddles = length(sp) > 1 || length(also) > 0,
      n_nodes = length(nodes), nodes = paste(nodes, collapse = " "),
      override_reason = reason)
  })
  ws <- dplyr::bind_rows(rows)
  stale <- if (length(ov_sets)) {
    k <- which(!used_ov)
    tibble::tibble(workstream_id = vapply(overrides[k], function(o) aib_st_or(o$workstream_id, NA_character_), ""),
                   nodes = vapply(ov_sets[k], paste, "", collapse = " "),
                   reason = rep("no workstream of this run has exactly these nodes: the demand rule applies", length(k)))
  } else tibble::tibble(workstream_id = character(), nodes = character(), reason = character())
  list(workstreams = ws, stale = stale)
}

# ---- Scenarios and node statuses ---------------------------------------------------------------------------

# The scenarios of the strategic layer: baseline, managed (when the run has the diversification scenario),
# partial (when the run classified the configured partial shock) and decoupling (the headline shock).
aib_strategic_scenario_defs <- function(res, cfg = NULL) {
  lab <- aib_st_or(cfg$labels, list())
  div <- if (!is.null(res$scenario_shock) && !is.na(res$scenario_shock)) res$scenario_shock else NA_character_
  part <- aib_st_or(cfg$partial_shock, NA_character_)
  has_part <- !is.na(part) && paste0("class_", part) %in% names(res$classification)
  yr <- aib_st_or(res$meta$trade_year, "the reference year")
  shock_label <- function(id) {
    s <- Filter(function(s) identical(s$id, id), aib_st_or(res$meta$config$shocks, list()))
    if (length(s)) aib_st_or(s[[1]]$label, id) else id
  }
  rows <- list(
    tibble::tibble(scenario = "baseline", label = aib_st_or(lab$baseline, "Baseline"), shock_id = NA_character_,
                   classes = FALSE, investment = FALSE, coalitions = FALSE,
                   words = sprintf("Trade as it is today (%s), before any shock.", yr)),
    if (!is.na(div) && !is.null(res$diversification)) tibble::tibble(
      scenario = "managed", label = aib_st_or(lab$managed, "Managed Interdependence"), shock_id = div, classes = TRUE,
      investment = TRUE, coalitions = !is.null(res$compact_members_managed),
      words = "No supplier is removed; a supplier above its node's concentration benchmark gives up the excess, which trade and new capacity replace."),
    if (has_part) tibble::tibble(
      scenario = "partial", label = aib_st_or(lab$partial, "Partial supplier disruption"), shock_id = part, classes = TRUE,
      investment = FALSE, coalitions = FALSE, words = paste0(shock_label(part), " (no investment simulation or coalitions under this scenario).")),
    tibble::tibble(scenario = "decoupling", label = aib_st_or(lab$decoupling, "Decoupling"), shock_id = res$meta$headline_shock,
                   classes = TRUE, investment = TRUE, coalitions = TRUE, words = paste0(shock_label(res$meta$headline_shock), "."))
  )
  dplyr::bind_rows(rows)
}

# The ranked investment hosts of every node under a shock at the headline increment (headline scenario,
# constraint and semantics), in the model's rank order.
aib_strategic_ranked_hosts <- function(res, shock) {
  iv <- res$investment
  inc <- as.numeric(aib_st_or(res$meta$config$investment$headline_increment, 0.1))
  if (is.null(iv) || !nrow(iv) || is.na(shock)) return(NULL)
  iv <- iv[iv$shock_id == shock & iv$scenario == res$meta$headline_scenario & abs(iv$increment - inc) < 1e-9 & !is.na(iv$host_rank), , drop = FALSE]
  if ("supply_constraint" %in% names(iv) && !is.null(res$meta$headline_constraint)) iv <- iv[iv$supply_constraint == res$meta$headline_constraint, , drop = FALSE]
  if ("shock_semantics" %in% names(iv) && !is.null(res$meta$headline_semantics)) iv <- iv[iv$shock_semantics == res$meta$headline_semantics, , drop = FALSE]
  iv[order(iv$node_id, iv$host_rank, iv$host_iso), , drop = FALSE]
}

# The model's tie group of the rank-1 host among one node's ranked hosts (rows in rank order): the hosts the
# model's own ranking (aib_rank_hosts()) cannot separate from rank 1 on its two findings, the simulated gain
# (diversified gain where present) within tie_resilience of the best, and the HHI change within tie_hhi of
# rank 1's; below that the order is its capability / Friendshore tie-break. Tolerances: config/aib.yml
# investment (res$meta$config$investment), 0.005 each by default.
aib_strategic_tie_group <- function(gain, gain_div, delta_hhi, tie_resilience = 0.005, tie_hhi = 0.005) {
  rg <- dplyr::coalesce(as.numeric(gain_div), as.numeric(gain))
  if (!length(rg) || all(is.na(rg))) return(rep(FALSE, length(rg)))
  gt <- floor((max(rg, na.rm = TRUE) - rg) / tie_resilience + 1e-9)
  dh <- dplyr::coalesce(as.numeric(delta_hhi), 0)
  !is.na(gt) & gt == gt[1] & abs(dh - dh[1]) < tie_hhi
}

# The best new-capacity host of each node under a shock at the headline increment. The simulated gain is
# min(increment, residual gap) for every host that can supply the gap, so the ranked hosts of most nodes tie
# with the model's rank 1 on gain and concentration (aib_strategic_tie_group()) and rank 1 is its capability
# tie-break, not a finding (2026-10-21 review). A host is named (top_host) only when no other host ties with
# it; otherwise top_host is NA and host_tie_n / host_tied give the size and members (model rank order) of the
# tie. host_gain is rank 1's gain either way; model_rank1 the model's own rank-1 host (evidence only).
aib_strategic_rank1 <- function(res, shock) {
  empty <- tibble::tibble(node_id = character(), top_host = character(), host_gain = numeric(), host_tie_n = integer(),
                          host_tied = character(), model_rank1 = character())
  iv <- aib_strategic_ranked_hosts(res, shock)
  if (is.null(iv) || !nrow(iv)) return(empty)
  ic <- res$meta$config$investment
  tr <- as.numeric(aib_st_or(ic$tie_resilience, 0.005))
  th <- as.numeric(aib_st_or(ic$tie_hhi, 0.005))
  col <- function(x, k) if (k %in% names(x)) x[[k]] else rep(NA_real_, nrow(x))
  dplyr::bind_rows(lapply(split(iv, iv$node_id), function(x) {
    x$tied <- aib_strategic_tie_group(x$marginal_resilience_gain, col(x, "marginal_resilience_gain_diversified"), col(x, "delta_hhi"), tr, th)
    t <- x$host_iso[x$tied]
    hg <- as.numeric(x$marginal_resilience_gain[1])
    tibble::tibble(node_id = x$node_id[1], top_host = if (length(t) == 1) t else NA_character_,
                   host_gain = hg,
                   host_tie_n = length(t), host_tied = paste(t, collapse = " "), model_rank1 = x$host_iso[1])
  }))
}

# Multi-label policy needs of scenario rows, with the classification rule itself (aib_classify_nodes()).
aib_strategic_needs <- function(rows, cls) {
  if (!nrow(rows)) return(tibble::tibble(needs_build = logical(), needs_coordinate = logical(), needs_manage = logical(),
                                          class = character(), reason = character()))
  need <- c("coverage_loss_at_manage_cap", "optimized_largest_share", "optimized_top3_share", "optimized_largest_supplier",
            "shock_exposure", "residual_gap", "existing_coverage", "coordination_dividend")
  for (k in need) if (!k %in% names(rows)) rows[[k]] <- NA
  if (!"optimize" %in% names(rows)) rows$optimize <- TRUE
  x <- aib_classify_nodes(rows, cls)
  tibble::tibble(needs_build = x$needs_build %in% TRUE, needs_coordinate = x$needs_coordinate %in% TRUE,
                 needs_manage = x$needs_manage_specialization %in% TRUE, class = x$node_classification, reason = x$classification_reason)
}

# Node x scenario statuses of every mapped run node (the spine): status (an AIB class, NOT STRESSED, NOT
# TESTED or NO SHOCK), the multi-label needs, demand, lost supply and the coverage states (existing
# relationships, coordinated trade = the optimized solution, post-investment = coordinated + the best host's
# simulated gain at the headline increment, capped at 1: by construction coordinated + min(increment,
# residual gap), since every host that can supply the gap gains the same). Baseline: NO SHOCK, coverage 1.
# Managed: the diversification rows where the scenario ran, NOT STRESSED (coverage 1) elsewhere, except NOT
# TESTED (no coverage credit) for a node that is itself concentrated (exporter HHI >= untested$node_hhi) and
# was left alone only because its benchmark heading is concentrated too (benchmark HHI >= untested$
# benchmark_hhi): being "as diversified as" a concentrated benchmark says nothing (2026-10-21 review).
# Partial: the stress rows of the partial shock and its recorded class (no post-investment coverage: not
# simulated). A node the trade model did not optimize is INSUFFICIENT DATA in every shocked scenario.
# The best host is named only when it beats every other host (aib_strategic_rank1()); host_tie_n and
# host_tied give the tie otherwise.
aib_strategic_node_status <- function(res, node_ids, scen, cls, untested = NULL) {
  thr_b <- as.numeric(aib_st_or(untested$benchmark_hhi, 0.18))
  thr_n <- as.numeric(aib_st_or(untested$node_hhi, 0.10))
  cl <- res$classification
  cl <- cl[match(node_ids, cl$node_id), , drop = FALSE]
  ids <- node_ids
  dem <- as.numeric(cl$demand_usd)
  opt <- dplyr::coalesce(as.logical(cl$optimize), FALSE)
  hs <- res$meta$headline_scenario
  base_tbl <- function(scn) tibble::tibble(node_id = ids, scenario = scn, demand_usd = dem, optimize = opt)
  out <- list()
  for (i in seq_len(nrow(scen))) {
    s <- scen$scenario[i]
    shock <- scen$shock_id[i]
    t <- base_tbl(s)
    if (s == "baseline") {
      t$node_status <- "NO SHOCK"; t$status_detail <- "no shock: trade as it is today"
      t$lost_usd <- 0; t$exposure <- 0
      t$coverage_static <- 1; t$coverage_existing <- 1; t$coverage_coordinated <- 1; t$coverage_post_investment <- 1
      t$residual_gap <- 0
      t$needs_build <- FALSE; t$needs_coordinate <- FALSE; t$needs_manage <- FALSE
      t$top_host <- NA_character_; t$host_gain <- NA_real_; t$host_tie_n <- NA_integer_; t$host_tied <- NA_character_; t$model_rank1 <- NA_character_
      t$largest_supplier <- cl$baseline_largest_supplier; t$largest_share <- as.numeric(cl$baseline_largest_share)
    } else {
      rows <- if (s == "decoupling") {
        cl
      } else if (s == "managed") {
        dv <- res$diversification
        d <- dv[!is.na(dv$scenario) & dv$scenario == hs & dv$status == "diversified to the benchmark", , drop = FALSE]
        d <- d[!duplicated(d$node_id), , drop = FALSE]
        d[match(ids, d$node_id), , drop = FALSE]
      } else {
        st <- res$stress
        st <- st[st$level == "subsector" & st$shock_id == shock & st$scenario == hs, , drop = FALSE]
        if ("shock_semantics" %in% names(st) && !is.null(res$meta$headline_semantics)) st <- st[st$shock_semantics == res$meta$headline_semantics, , drop = FALSE]
        if ("supply_constraint" %in% names(st) && !is.null(res$meta$headline_constraint)) st <- st[st$supply_constraint == res$meta$headline_constraint, , drop = FALSE]
        st <- st[!duplicated(st$node_id), , drop = FALSE]
        st[match(ids, st$node_id), , drop = FALSE]
      }
      ran <- !is.na(rows$node_id)
      cls_vec <- if (s == "decoupling") cl$node_classification else if (s == "managed") rows$diversify_class else cl[[paste0("class_", shock)]]
      status <- ifelse(!opt, "INSUFFICIENT DATA", ifelse(ran & !is.na(cls_vec), cls_vec, if (s == "managed") "NOT STRESSED" else "INSUFFICIENT DATA"))
      untested_note <- rep(NA_character_, length(ids))
      if (s == "managed") {
        dv_u <- res$diversification
        dv_u <- dv_u[is.na(dv_u$scenario) | dv_u$scenario == hs, , drop = FALSE]
        dv_u <- dv_u[!duplicated(dv_u$node_id), , drop = FALSE]
        dv_u <- dv_u[match(ids, dv_u$node_id), , drop = FALSE]
        hhi_n <- if ("current_hhi" %in% names(dv_u)) as.numeric(dv_u$current_hhi) else rep(NA_real_, length(ids))
        hhi_b <- if ("benchmark_hhi" %in% names(dv_u)) as.numeric(dv_u$benchmark_hhi) else rep(NA_real_, length(ids))
        un <- status == "NOT STRESSED" & dv_u$status %in% "already as diversified as its benchmark" &
          !is.na(hhi_b) & hhi_b >= thr_b & !is.na(hhi_n) & hhi_n >= thr_n
        status[un] <- "NOT TESTED"
        untested_note[un] <- sprintf("not tested under Managed Interdependence: the node is concentrated (exporter HHI %.2f) and its benchmark heading is too (HHI %.2f), so being as diversified as the benchmark says nothing; see its Decoupling class (%s)",
                                     hhi_n[un], hhi_b[un], dplyr::coalesce(cl$node_classification[un], "not classified"))
      }
      # Multi-label needs: the classification's own for Decoupling; for the other scenarios the classification
      # rule (aib_classify_nodes()) on the scenario's rows, whose reason is kept where its class is the recorded one.
      nd <- if (s == "decoupling") {
        tibble::tibble(needs_build = cl$needs_build %in% TRUE, needs_coordinate = cl$needs_coordinate %in% TRUE,
                       needs_manage = cl$needs_manage_specialization %in% TRUE, class = cl$node_classification, reason = cl$classification_reason)
      } else {
        x <- rows
        x$node_id <- ids
        x$optimize <- opt & ran
        aib_strategic_needs(x, cls)
      }
      reason <- ifelse(!is.na(nd$class) & nd$class == status, nd$reason, NA_character_)
      detail <- if (s == "managed") {
        dv_all <- res$diversification
        dv_all <- dv_all[is.na(dv_all$scenario) | dv_all$scenario == hs, , drop = FALSE]
        dv_all <- dv_all[!duplicated(dv_all$node_id), , drop = FALSE]
        st_txt <- dv_all$status[match(ids, dv_all$node_id)]
        ifelse(status == "NOT TESTED", untested_note,
               ifelse(status == "NOT STRESSED", dplyr::coalesce(st_txt, "not assessed"),
                      ifelse(status == "INSUFFICIENT DATA", "data too thin for a bilateral optimization",
                             ifelse(is.na(reason), "diversified to the benchmark", paste0("diversified to the benchmark; ", reason)))))
      } else ifelse(status == "INSUFFICIENT DATA", "data too thin for a bilateral optimization", reason)
      t$node_status <- status
      t$status_detail <- detail
      num <- function(k, fill) { v <- if (k %in% names(rows)) as.numeric(rows[[k]]) else rep(NA_real_, length(ids)); ifelse(ran, v, fill) }
      stressed <- ran & opt
      # NOT TESTED: no coverage credit and no lost supply (the scenario says nothing about the node)
      fill0 <- ifelse(opt & status != "NOT TESTED", 0, NA_real_)
      t$lost_usd <- ifelse(stressed, dplyr::coalesce(num("lost_usd", 0), 0), fill0)
      t$exposure <- ifelse(stressed, num("shock_exposure", NA_real_), fill0)
      fill1 <- ifelse(opt & status != "NOT TESTED", 1, NA_real_)
      t$coverage_static <- ifelse(stressed, num("static_coverage", NA_real_), fill1)
      t$coverage_existing <- ifelse(stressed, num("existing_coverage", NA_real_), fill1)
      t$coverage_coordinated <- ifelse(stressed, num("optimized_coverage", NA_real_), fill1)
      t$residual_gap <- ifelse(stressed, num("residual_gap", NA_real_), fill0)
      t$needs_build <- nd$needs_build & stressed
      t$needs_coordinate <- nd$needs_coordinate & stressed
      t$needs_manage <- nd$needs_manage & stressed
      r1 <- if (isTRUE(scen$investment[i])) aib_strategic_rank1(res, shock) else NULL
      k1 <- if (is.null(r1)) rep(NA_integer_, length(ids)) else match(ids, r1$node_id)
      t$top_host <- if (is.null(r1)) NA_character_ else r1$top_host[k1]
      t$host_gain <- if (is.null(r1)) NA_real_ else r1$host_gain[k1]
      t$host_tie_n <- if (is.null(r1)) NA_integer_ else r1$host_tie_n[k1]
      t$host_tied <- if (is.null(r1)) NA_character_ else r1$host_tied[k1]
      t$model_rank1 <- if (is.null(r1)) NA_character_ else r1$model_rank1[k1]
      t$top_host[!stressed] <- NA_character_
      t$host_gain[!stressed] <- NA_real_
      t$host_tie_n[!stressed] <- NA_integer_
      t$host_tied[!stressed] <- NA_character_
      t$model_rank1[!stressed] <- NA_character_
      t$largest_supplier <- ifelse(stressed, if ("optimized_largest_supplier" %in% names(rows)) rows$optimized_largest_supplier else NA_character_, cl$baseline_largest_supplier)
      t$largest_share <- ifelse(stressed, num("optimized_largest_share", NA_real_), as.numeric(cl$baseline_largest_share))
    }
    t$coverage_post_investment <- if (s == "baseline") 1 else if (isTRUE(scen$investment[i])) {
      ifelse(is.na(t$coverage_coordinated), NA_real_, pmin(1, t$coverage_coordinated + dplyr::coalesce(t$host_gain, 0)))
    } else NA_real_
    t$policy_needs <- vapply(seq_len(nrow(t)), function(k) {
      x <- c("BUILD", "COORDINATE", "MANAGE SPECIALIZATION")[c(t$needs_build[k], t$needs_coordinate[k], t$needs_manage[k])]
      if (length(x)) paste(x, collapse = " + ") else "none"
    }, "")
    out[[length(out) + 1]] <- t
  }
  dplyr::bind_rows(out)
}

# ---- Missions, policy menu and the compact portfolio -------------------------------------------------------

# Missions of one compact in one scenario. nd: its nodes (node_status, lost_usd, needs_*, value_chain_order,
# value_chain_step). Candidates: each intervention class present, ordered by its share of the compact's lost
# supply, then node count, then the configured class order; then each multi-label need not yet a candidate
# (a node needs it beside its headline class), ordered by the lost supply of the nodes that carry it. The
# first three are primary / secondary / supporting. No score: an ordering of named evidence.
# Materiality (2026-10-21 review): a mission is material when its nodes carry >= material_share of the
# compact's lost supply or >= material_usd; a compact whose primary mission is not material is led by the
# monitor template and its missions read as targeted actions (aib_strategic_portfolio()). {steps} names a
# step when all of the step's nodes share the mission's class, and otherwise the mission's own nodes in it
# ("solar water heaters (heating & cooling units)"), so a step whose main node is LOW is not announced as
# BUILD. lost_word: "lost supply" (shock scenarios) or "supply to shift" (Managed Interdependence).
aib_strategic_missions <- function(nd, cfg, lost_word = "lost supply") {
  classes <- unlist(aib_st_or(cfg$classes, aib_strategic_intervention()))
  tmpl <- aib_st_or(cfg$templates, list())
  max_steps <- as.integer(aib_st_or(cfg$max_steps, 3L))
  m_share <- as.numeric(aib_st_or(cfg$material_share, 0.25))
  m_usd <- as.numeric(aib_st_or(cfg$material_usd, 1e9))
  need_col <- c(BUILD = "needs_build", COORDINATE = "needs_coordinate", `MANAGE SPECIALIZATION` = "needs_manage")
  lost <- dplyr::coalesce(nd$lost_usd, 0)
  tot <- sum(lost)
  labels <- if ("node_label" %in% names(nd)) dplyr::coalesce(nd$node_label, nd$node_id) else nd$node_id
  cand <- list()
  for (k in classes) {
    idx <- nd$node_status %in% k
    if (any(idx)) cand[[length(cand) + 1]] <- list(cls = k, basis = "class", lost = sum(lost[idx]), n = sum(idx), idx = idx)
  }
  if (length(cand)) cand <- cand[order(-vapply(cand, `[[`, 0, "lost"), -vapply(cand, `[[`, 0, "n"), match(vapply(cand, `[[`, "", "cls"), classes))]
  extra <- list()
  for (k in setdiff(classes, vapply(cand, `[[`, "", "cls"))) {
    col <- need_col[[k]]
    idx <- if (col %in% names(nd)) nd[[col]] %in% TRUE else rep(FALSE, nrow(nd))
    if (any(idx)) extra[[length(extra) + 1]] <- list(cls = k, basis = "need", lost = sum(lost[idx]), n = sum(idx), idx = idx)
  }
  if (length(extra)) extra <- extra[order(-vapply(extra, `[[`, 0, "lost"), -vapply(extra, `[[`, 0, "n"), match(vapply(extra, `[[`, "", "cls"), classes))]
  all <- utils::head(c(cand, extra), 3)
  # a step whose nodes all carry the mission is named as the step; otherwise the mission's nodes in it are named
  steps_of <- function(idx) {
    x <- nd[idx, , drop = FALSE]
    if (!nrow(x)) return("")
    by <- stats::aggregate(list(lost = dplyr::coalesce(x$lost_usd, 0)), list(order = x$value_chain_order, step = x$value_chain_step), sum)
    by <- by[order(-by$lost, by$order), , drop = FALSE]
    by <- utils::head(by, max_steps)
    words <- vapply(seq_len(nrow(by)), function(j) {
      in_step <- nd$value_chain_order == by$order[j]
      mine <- in_step & idx
      if (all(mine[in_step])) return(aib_st_lcfirst(by$step[j]))
      nm <- aib_st_and(aib_st_lcfirst(labels[mine]))
      # the step in brackets, unless the node's own label already says it ("Li-ion cells & packs", step "Cells & packs")
      if (grepl(tolower(by$step[j]), tolower(nm), fixed = TRUE)) nm else sprintf("%s (%s)", nm, aib_st_lcfirst(by$step[j]))
    }, "")
    aib_st_and(words)
  }
  mission <- function(m) {
    if (is.null(m)) return(list(text = NA_character_, cls = NA_character_, basis = NA_character_, nodes = character(), material = NA))
    t <- aib_st_or(tmpl[[m$cls]], paste0(tolower(m$cls), " in {steps}"))
    material <- (tot > 0 && m$lost / tot >= m_share) || m$lost >= m_usd
    basis <- if (m$basis == "class") {
      sprintf("%s: %s of %s, %d node%s", m$cls, if (tot > 0) aib_st_share(m$lost / tot) else "0%", lost_word, m$n, if (m$n == 1) "" else "s")
    } else {
      sprintf("%s need beside the headline class on %d node%s", m$cls, m$n, if (m$n == 1) "" else "s")
    }
    if (!material) basis <- sprintf("%s; a targeted action (below the materiality floor of %s of %s or US$%sbn)", basis, aib_st_pct(m_share), lost_word, format(m_usd / 1e9))
    list(text = sub("{steps}", steps_of(m$idx), t, fixed = TRUE), cls = m$cls, basis = basis, nodes = nd$node_id[m$idx], material = material)
  }
  ms <- lapply(1:3, function(i) if (i <= length(all)) mission(all[[i]]) else mission(NULL))
  names(ms) <- c("primary", "secondary", "supporting")
  ms
}

# The compact's policy menu in a scenario: the instruments of every class (and multi-label need) present.
aib_strategic_policy_menu <- function(missions, menu) {
  cls <- stats::na.omit(vapply(missions, function(m) m$cls, ""))
  lapply(unname(cls), function(k) list(cls = k, instruments = as.character(unlist(menu[[k]])),
                               nodes = unname(unlist(lapply(missions, function(m) if (identical(m$cls, k)) m$nodes)))))
}

# One row per strategic compact x scenario: node counts by status, the lost-supply-weighted class mix,
# missions, policy mix and menu, demand-weighted coverage (existing, coordinated, post-investment; nodes
# without coverage, i.e. INSUFFICIENT DATA and NOT TESTED, are left out of the weights), the primary
# workstreams and those active in the scenario (a node in an intervention class) and the institutional
# slots of the scenario's workstream coalitions, restricted to the allied set (2026-10-21 review; `allied`,
# which includes the strategic layer's treat_as_allied economies):
#   coalition_members              allied members of the workstreams' minimum coalitions
#   critical_supply_partners       allied critical supply partners holding >= presence_min of world supply of
#                                  a node of the compact (`presence`: tibble(strategic_compact_id, iso3));
#                                  a re-exporter below that share is not a critical supplier
#   critical_external_dependencies non-allied economies a workstream depends on critically
#   allied_outside_core            allied economies a workstream lists as a critical external dependency
#                                  that are neither a minimum-coalition member nor a critical partner in any
#                                  workstream of the compact, nor one of its structural core countries (`core`:
#                                  a core country is not "outside the core")
#   non_allied_relied_on           non-allied members of the minimum coalitions (suppliers the coalition relies on)
#   build_hosts                    allied hosts that beat every other host on a BUILD node (a tie names nobody;
#                                  build_host_ties counts the BUILD nodes whose hosts tie at the full increment)
#   build_hosts_non_allied         the same, non-allied
# and a headline sentence (the verb follows the scenario: supply "has to shift" under Managed
# Interdependence, where no supplier is removed, and "is lost" otherwise; post-investment coverage is left
# out of it, since it is coordinated + min(increment, gap) on each node by construction). core_countries:
# the structural core (from the compact roles), the same in every scenario. node_labels: named character
# (node id -> label) for the missions.
aib_strategic_portfolio <- function(status, map_run, defs, scen, ws, members, cfg_missions, menu, core = NULL, country_name = NULL,
                                    allied = NULL, node_labels = NULL, presence = NULL) {
  cname <- function(x) if (is.null(country_name)) x else ifelse(is.na(country_name[x]), x, country_name[x])
  is_allied <- function(x) if (is.null(allied)) rep(TRUE, length(x)) else x %in% allied
  st <- dplyr::left_join(status, map_run[, c("node_id", "strategic_compact_id", "value_chain_order", "value_chain_step")], by = "node_id")
  st$node_label <- if (is.null(node_labels)) st$node_id else dplyr::coalesce(aib_strategic_prose_label(unname(node_labels[st$node_id])), st$node_id)
  m_share <- as.numeric(aib_st_or(cfg_missions$material_share, 0.25))
  m_usd <- as.numeric(aib_st_or(cfg_missions$material_usd, 1e9))
  rows <- list()
  menu_out <- list()
  for (k in seq_len(nrow(defs))) {
    sc <- defs$strategic_compact_id[k]
    ws_k <- ws[ws$strategic_compact_id %in% sc, , drop = FALSE]
    ws_also <- ws[!ws$strategic_compact_id %in% sc & vapply(strsplit(ws$also_spans, " ", fixed = TRUE), function(a) sc %in% a, logical(1)), , drop = FALSE]
    present <- if (is.null(presence)) NULL else presence$iso3[presence$strategic_compact_id == sc]
    for (i in seq_len(nrow(scen))) {
      s <- scen$scenario[i]
      mi <- s == "managed"
      nd <- st[st$strategic_compact_id %in% sc & st$scenario == s, , drop = FALSE]
      nd <- nd[order(nd$value_chain_order, match(nd$node_id, map_run$node_id)), , drop = FALSE]
      cnt <- function(x) sum(nd$node_status %in% x)
      dem <- dplyr::coalesce(nd$demand_usd, 0)
      wmean <- function(v) { ok <- !is.na(v) & dem > 0; if (any(ok)) sum(v[ok] * dem[ok]) / sum(dem[ok]) else NA_real_ }
      lost <- sum(nd$lost_usd, na.rm = TRUE)
      dem_cov <- sum(dem[!is.na(nd$coverage_existing)])
      mix <- if (nrow(nd) && lost > 0) aib_class_mix(nd$node_status, dplyr::coalesce(nd$lost_usd, 0), keep = aib_node_classes()) else NULL
      lost_word <- if (mi) "supply to shift" else "lost supply"
      ms <- if (s == "baseline") NULL else aib_strategic_missions(nd, cfg_missions, lost_word)
      no_need <- !is.null(ms) && is.na(ms$primary$cls)
      ws_nodes <- strsplit(ws_k$nodes, " ", fixed = TRUE)
      active <- ws_k$workstream_id[vapply(ws_nodes, function(ids) any(status$node_status[status$scenario == s & status$node_id %in% ids] %in% aib_strategic_intervention()), logical(1))]
      mem <- members[[s]]
      mem <- if (is.null(mem)) NULL else mem[mem$compact_id %in% ws_k$workstream_id, , drop = FALSE]
      count_codes <- function(x) { if (!length(x)) return(""); t <- table(x); paste(names(t)[order(-t, names(t))], collapse = " ") }
      slot <- function(x) if (is.null(mem)) NA_character_ else count_codes(x)
      core_ws <- if (is.null(mem)) character() else unique(mem$iso3[mem$in_minimum_coalition %in% TRUE | mem$coalition_tier %in% "critical supply partner"])
      ext <- if (is.null(mem)) character() else mem$iso3[mem$coalition_tier %in% c("critical external dependency", "critical external dependency (marginal)")]
      cp <- if (is.null(mem)) character() else mem$iso3[mem$coalition_tier %in% "critical supply partner"]
      cp <- cp[is_allied(cp) & (is.null(present) | cp %in% present)]
      cm <- if (is.null(mem)) character() else mem$iso3[mem$in_minimum_coalition %in% TRUE]
      crit <- slot(cp)
      crit_ext <- slot(ext[!is_allied(ext)])
      core_sc <- if (is.null(core)) character() else strsplit(aib_st_or(core$core_countries[core$strategic_compact_id == sc], ""), " ", fixed = TRUE)[[1]]
      outside <- slot(ext[is_allied(ext) & !ext %in% c(core_ws, core_sc)])
      coal <- slot(cm[is_allied(cm)])
      coal_non <- slot(cm[!is_allied(cm)])
      bh <- stats::na.omit(nd$top_host[nd$node_status %in% "BUILD"])
      build_hosts <- count_codes(bh[is_allied(bh)])
      build_hosts_non <- count_codes(bh[!is_allied(bh)])
      build_ties <- sum(nd$node_status %in% "BUILD" & is.na(nd$top_host) & dplyr::coalesce(nd$host_tie_n, 0L) > 1)
      cov_e <- wmean(nd$coverage_existing); cov_c <- wmean(nd$coverage_coordinated); cov_p <- wmean(nd$coverage_post_investment)
      exposure <- if (dem_cov > 0) lost / dem_cov else NA_real_
      n_untested <- cnt("NOT TESTED")
      untested_txt <- if (n_untested) sprintf(" %d concentrated node%s %s not tested under this scenario (see Decoupling).", n_untested, if (n_untested == 1) "" else "s", if (n_untested == 1) "is" else "are") else ""
      headline <- if (s == "baseline") {
        "No shock: allied demand is served as it is today (coverage 100%)."
      } else if (!nrow(nd) || lost <= 0) {
        paste0(sprintf("%s: no node of the compact %s.", scen$label[i], if (mi) "has supply to shift" else "loses supply"), untested_txt)
      } else {
        paste0(sprintf("%s: %s of the compact's allied demand (US$%.1fbn) %s; coverage %s with existing relationships, %s with coordinated trade.",
                       scen$label[i], aib_st_pct(exposure), lost / 1e9, if (mi) "has to shift away from suppliers above the cap" else "is lost",
                       aib_st_pct(cov_e), aib_st_pct(cov_c)), untested_txt)
      }
      # the statement: material missions lead; without one, the monitor template leads and the missions are targeted actions
      n_thin <- cnt("INSUFFICIENT DATA") + n_untested
      none_txt <- if (n_thin) {
        sub("{untested}", sprintf("%d node%s the model could not test", n_thin, if (n_thin == 1) "" else "s"),
            aib_st_or(cfg_missions$none_partial, "monitor: existing trade restores supply in every node the model could test ({untested})"), fixed = TRUE)
      } else aib_st_or(cfg_missions$none, "monitor")
      m_txt <- if (s == "baseline") aib_st_or(cfg_missions$no_shock, "no shock applied") else if (no_need) none_txt else {
        mm <- Filter(function(m) !is.na(m$cls), ms)
        mat <- vapply(mm, function(m) isTRUE(m$material), logical(1))
        lead <- if (isTRUE(ms$primary$material)) paste(vapply(mm[mat], `[[`, "", "text"), collapse = "; ") else
          sub("{floor}", sprintf("%s of the compact's %s or US$%sbn", aib_st_pct(m_share), lost_word, format(m_usd / 1e9)),
              aib_st_or(cfg_missions$monitor_lead, "monitor: no intervention class reaches the materiality floor ({floor})"), fixed = TRUE)
        tgt <- vapply(mm[!mat], `[[`, "", "text")
        if (length(tgt)) paste0(lead, "; targeted: ", paste(tgt, collapse = "; ")) else lead
      }
      pm <- if (is.null(ms)) list() else aib_strategic_policy_menu(ms, menu)
      menu_out[[paste(sc, s)]] <- pm
      g <- function(m, f) if (is.null(ms)) NA_character_ else ms[[m]][[f]]
      gm <- function(m) if (is.null(ms)) NA else ms[[m]]$material
      rows[[length(rows) + 1]] <- tibble::tibble(
        strategic_compact_id = sc, strategic_compact_name = defs$strategic_compact_name[k], scenario = s, scenario_label = scen$label[i],
        shock_id = scen$shock_id[i], technology_family = defs$family[k], purpose = defs$purpose[k],
        value_chain = paste(unique(nd$value_chain_step), collapse = " -> "),
        n_steps = length(unique(nd$value_chain_order)), n_nodes = nrow(nd), included_nodes = paste(nd$node_id, collapse = " "),
        workstream_count = nrow(ws_k), workstreams = paste(ws_k$workstream_id, collapse = " "),
        active_workstream_count = length(active), active_workstreams = paste(active, collapse = " "),
        also_touching_workstreams = paste(ws_also$workstream_id, collapse = " "),
        build_node_count = cnt("BUILD"), coordinate_node_count = cnt("COORDINATE"), manage_node_count = cnt("MANAGE SPECIALIZATION"),
        low_node_count = cnt("LOW INTERVENTION"), insufficient_node_count = cnt("INSUFFICIENT DATA"),
        not_stressed_node_count = cnt("NOT STRESSED"), not_tested_node_count = n_untested, no_shock_node_count = cnt("NO SHOCK"),
        needs_build_nodes = sum(nd$needs_build %in% TRUE), needs_coordinate_nodes = sum(nd$needs_coordinate %in% TRUE),
        needs_manage_nodes = sum(nd$needs_manage %in% TRUE),
        class_mix = if (length(mix)) aib_class_mix_text(mix) else NA_character_,
        mission_lead = if (s == "baseline") "no shock" else if (no_need) "monitor" else if (isTRUE(ms$primary$material)) "mission" else "monitor (targeted actions only)",
        primary_mission = g("primary", "text"), primary_mission_class = g("primary", "cls"), primary_mission_basis = g("primary", "basis"), primary_mission_material = gm("primary"),
        secondary_mission = g("secondary", "text"), secondary_mission_class = g("secondary", "cls"), secondary_mission_basis = g("secondary", "basis"), secondary_mission_material = gm("secondary"),
        supporting_mission = g("supporting", "text"), supporting_mission_class = g("supporting", "cls"), supporting_mission_basis = g("supporting", "basis"), supporting_mission_material = gm("supporting"),
        mission_statement = paste0(aib_st_cap(m_txt), "."),
        policy_mix = if (length(pm)) paste(vapply(pm, `[[`, "", "cls"), collapse = " + ") else if (s == "baseline") "NO SHOCK" else "LOW INTERVENTION",
        policy_menu = if (length(pm)) paste(vapply(pm, function(x) paste0(x$cls, ": ", paste(x$instruments, collapse = ", ")), ""), collapse = " | ") else NA_character_,
        demand_usd = sum(dem), lost_usd = lost, exposure = exposure,
        coverage_static = wmean(nd$coverage_static), coverage_existing = cov_e, coverage_coordinated = cov_c, coverage_post_investment = cov_p,
        coverage_basis = paste("demand-weighted over the compact's nodes with coverage (INSUFFICIENT DATA and NOT TESTED nodes left out); NOT STRESSED and NO SHOCK nodes count as 1;",
                               "post-investment = coordinated + the best host's simulated gain at the headline increment, capped at 1: by construction coordinated + min(increment, residual gap) on each node, since every host that can supply the gap gains the same"),
        headline_result = headline,
        core_countries = if (is.null(core)) NA_character_ else aib_st_or(core$core_countries[core$strategic_compact_id == sc], NA_character_),
        core_country_roles = if (is.null(core)) NA_character_ else aib_st_or(core$core_country_roles[core$strategic_compact_id == sc], NA_character_),
        critical_supply_partners = crit, critical_external_dependencies = crit_ext, allied_outside_core = outside,
        coalition_members = coal, non_allied_relied_on = coal_non,
        build_hosts = build_hosts, build_hosts_non_allied = build_hosts_non, build_host_ties = build_ties)
    }
  }
  list(compacts = dplyr::bind_rows(rows), menus = menu_out)
}

# ---- Strategic roles: evidence ---------------------------------------------------------------------------

# The node frame of the role rules: mapped run nodes with their compact, step, function, world trade and
# whether the trade model optimized them (thin-trade nodes say nothing about who demands them).
aib_strategic_node_frame <- function(inputs, map_run, node_fn) {
  nd <- inputs$nodes[, c("node_id", "sub_sector", "supply_chain")]
  nd <- dplyr::inner_join(nd, dplyr::transmute(map_run, node_id, sc_id = strategic_compact_id, sc_name = strategic_compact_name,
                                               vc_order = value_chain_order, vc_step = value_chain_step), by = "node_id")
  nd <- dplyr::left_join(nd, dplyr::select(node_fn, node_id, step_function = node_function), by = "node_id")
  nd <- dplyr::left_join(nd, inputs$node_summary[, c("node_id", "world_trade")], by = "node_id")
  q <- inputs$quality[is.na(inputs$quality$aggregated_into), c("node_id", "optimize")]
  nd <- dplyr::left_join(nd, q, by = "node_id")
  nd$optimize <- dplyr::coalesce(as.logical(nd$optimize), FALSE)
  nd
}

# Country x node evidence (reference year): exports, world share, RCA, production share, allied demand,
# net imports, the 2020 -> 2024 export-share change, investment-host ranks (the two institutional scenarios,
# headline increment) and the flags of the role rules (anchor, specialist, momentum, capable host).
#   allied  ISO3 codes of the demand scope; div_shock the Managed Interdependence shock (NA: none)
#   status  aib_strategic_node_status() (which nodes need new capacity under each scenario; NULL: every node)
#   hubs    re-export hubs (config/aib.yml trade.reexport_hubs): their exports are re-exports of another
#           economy's output, so they hold no anchor, specialist or scale position (reexport_hub)
aib_country_node_evidence <- function(inputs, res, nodes, allied, params, div_shock = res$scenario_shock, status = NULL, hubs = character()) {
  P <- params
  em <- inputs$exporter_metrics
  prod <- inputs$production |> dplyr::group_by(node_id, iso3) |>
    dplyr::summarise(production_share = max(production_share), .groups = "drop")
  fl <- inputs$flows[inputs$flows$node_id %in% nodes$node_id, , drop = FALSE]
  hist <- fl |> dplyr::group_by(node_id, iso3 = exporter_iso) |>
    dplyr::summarise(x_hist = sum(value_hist), x_now = sum(value), .groups = "drop") |>
    dplyr::group_by(node_id) |> dplyr::mutate(share_hist = x_hist / sum(x_hist), share_now = x_now / sum(x_now)) |> dplyr::ungroup()
  dem <- fl[fl$importer_iso %in% allied, , drop = FALSE] |> dplyr::group_by(node_id, iso3 = importer_iso) |>
    dplyr::summarise(imports = sum(value), .groups = "drop") |> dplyr::group_by(node_id) |>
    dplyr::mutate(scope_demand_usd = sum(imports), scope_demand_share = imports / scope_demand_usd) |> dplyr::ungroup()
  xall <- fl |> dplyr::group_by(node_id, iso3 = exporter_iso) |> dplyr::summarise(exports_all = sum(value), .groups = "drop")
  dem <- dem |> dplyr::left_join(xall, by = c("node_id", "iso3")) |>
    dplyr::mutate(net_import_share = pmax(imports - dplyr::coalesce(exports_all, 0), 0) / scope_demand_usd)
  # Investment hosts: every ranked host at the headline increment under Managed Interdependence and
  # Decoupling (headline scenario, constraint and semantics). The simulated gain is min(increment, residual
  # gap) for every host that can supply the gap, so most nodes' ranked hosts tie with rank 1 (aib_strategic_tie_group()) and the
  # model's rank inside a tie is its capability tie-break (2026-10-21 review). A host is CREDITED in a node
  # and scenario when the node needs new capacity there (BUILD, or a BUILD need beside its class; `status`,
  # aib_strategic_node_status()), the host is in rank 1's tie group, and it has capability at scale: the largest
  # of its world export / production share in the node and in any node joined to it by a material link
  # (>= host_scale_min). Its credit is the best gain x the node's allied demand, split equally among the
  # credited hosts (host_gap_usd: the larger of the two scenarios). best_host_rank: the model's best rank
  # among the scenarios where it is credited (evidence; NA when it is not credited).
  inv <- res$investment
  hs <- res$meta$headline_shock
  hscn <- res$meta$headline_scenario
  inc <- as.numeric(aib_st_or(res$meta$config$investment$headline_increment, 0.1))
  shocks <- stats::na.omit(c(hs, div_shock))
  pos0 <- em |> dplyr::select(node_id, iso3 = exporter_iso, world_share) |> dplyr::full_join(prod, by = c("node_id", "iso3")) |>
    dplyr::transmute(node_id, iso3, pos = pmax(dplyr::coalesce(world_share, 0), dplyr::coalesce(production_share, 0)))
  ml <- inputs$supply_chain_edges[grepl("material link", inputs$supply_chain_edges$basis), , drop = FALSE]
  links <- dplyr::bind_rows(tibble::tibble(node_id = ml$node_a, linked = ml$node_b), tibble::tibble(node_id = ml$node_b, linked = ml$node_a))
  hosts_all <- inv[inv$shock_id %in% shocks & inv$scenario == hscn & abs(inv$increment - inc) < 1e-9 & !is.na(inv$host_rank), , drop = FALSE]
  if ("supply_constraint" %in% names(hosts_all) && !is.null(res$meta$headline_constraint)) hosts_all <- hosts_all[hosts_all$supply_constraint == res$meta$headline_constraint, , drop = FALSE]
  if ("shock_semantics" %in% names(hosts_all) && !is.null(res$meta$headline_semantics)) hosts_all <- hosts_all[hosts_all$shock_semantics == res$meta$headline_semantics, , drop = FALSE]
  ic <- res$meta$config$investment
  tie_r <- as.numeric(aib_st_or(ic$tie_resilience, 0.005))
  tie_h <- as.numeric(aib_st_or(ic$tie_hhi, 0.005))
  if (!"marginal_resilience_gain_diversified" %in% names(hosts_all)) hosts_all$marginal_resilience_gain_diversified <- NA_real_
  if (!"delta_hhi" %in% names(hosts_all)) hosts_all$delta_hhi <- NA_real_
  hosts_all <- hosts_all |> dplyr::transmute(node_id, iso3 = host_iso, lens = ifelse(shock_id == hs, "decoupling", "managed"), host_rank,
                                             host_gain = as.numeric(marginal_resilience_gain),
                                             gain_div = as.numeric(marginal_resilience_gain_diversified), delta_hhi = as.numeric(delta_hhi),
                                             residual_gap_before,
                                             node_world_share = dplyr::coalesce(node_world_share, 0),
                                             host_production_share = dplyr::coalesce(production_share, 0))
  lk <- hosts_all |> dplyr::distinct(node_id, iso3) |> dplyr::inner_join(links, by = "node_id", relationship = "many-to-many") |>
    dplyr::left_join(dplyr::rename(pos0, linked = node_id), by = c("linked", "iso3")) |>
    dplyr::group_by(node_id, iso3) |> dplyr::summarise(linked_scale = max(dplyr::coalesce(pos, 0)), .groups = "drop")
  need <- if (is.null(status)) NULL else {
    x <- status[status$scenario %in% c("managed", "decoupling"), , drop = FALSE]
    tibble::tibble(node_id = x$node_id, lens = x$scenario, needs_capacity = x$node_status %in% "BUILD" | x$needs_build %in% TRUE)
  }
  node_dem <- stats::setNames(as.numeric(res$classification$demand_usd), res$classification$node_id)
  hosts_all <- hosts_all |> dplyr::left_join(lk, by = c("node_id", "iso3")) |>
    dplyr::mutate(host_scale = pmax(node_world_share, host_production_share, dplyr::coalesce(linked_scale, 0)),
                  host_scaled = host_scale >= P$host_scale_min) |>
    dplyr::arrange(node_id, lens, host_rank) |>
    dplyr::group_by(node_id, lens) |>
    dplyr::mutate(tied_best = aib_strategic_tie_group(host_gain, gain_div, delta_hhi, tie_r, tie_h),
                  best_gain = dplyr::first(host_gain)) |>
    dplyr::ungroup()
  hosts_all <- if (is.null(need)) dplyr::mutate(hosts_all, needs_capacity = TRUE) else
    dplyr::left_join(hosts_all, need, by = c("node_id", "lens")) |> dplyr::mutate(needs_capacity = dplyr::coalesce(needs_capacity, FALSE))
  hosts_all <- hosts_all |> dplyr::mutate(credited = tied_best & host_scaled & needs_capacity) |>
    dplyr::group_by(node_id, lens) |> dplyr::mutate(n_credited = sum(credited)) |> dplyr::ungroup() |>
    dplyr::mutate(credit_usd = ifelse(credited, dplyr::coalesce(best_gain, host_gain) * dplyr::coalesce(node_dem[node_id], 0) / pmax(n_credited, 1), 0))
  hosts <- hosts_all[hosts_all$credited, , drop = FALSE]
  capable <- hosts_all[hosts_all$host_scaled %in% TRUE, , drop = FALSE] |> dplyr::distinct(node_id, iso3) |> dplyr::mutate(capable_host = TRUE)
  host_sum <- hosts |> dplyr::group_by(node_id, iso3) |>
    dplyr::summarise(best_host_rank = min(host_rank), host_scaled = any(host_scaled), host_gain = max(host_gain),
                     host_gap_usd = max(credit_usd), host_tie_n = max(n_credited), host_lenses = paste(sort(unique(lens)), collapse = "+"), .groups = "drop")
  cn <- em |> dplyr::select(node_id, iso3 = exporter_iso, exports, world_share, export_rca) |>
    dplyr::full_join(prod, by = c("node_id", "iso3")) |>
    dplyr::full_join(dplyr::select(dem, node_id, iso3, imports, scope_demand_share, net_import_share), by = c("node_id", "iso3")) |>
    dplyr::left_join(dplyr::select(hist, node_id, iso3, share_hist, share_now), by = c("node_id", "iso3")) |>
    dplyr::left_join(host_sum, by = c("node_id", "iso3")) |>
    dplyr::left_join(capable, by = c("node_id", "iso3")) |>
    dplyr::inner_join(dplyr::select(nodes, node_id, sc_id, vc_order, vc_step, step_function, supply_chain, world_trade, optimize), by = "node_id")
  for (k in c("exports", "world_share", "imports", "scope_demand_share", "net_import_share")) cn[[k]] <- dplyr::coalesce(cn[[k]], 0)
  cn <- cn |> dplyr::mutate(
    position_share = pmax(world_share, dplyr::coalesce(production_share, 0)),
    reexport_hub = iso3 %in% hubs,
    anchor_position = !reexport_hub & position_share >= P$position_min & (dplyr::coalesce(export_rca, 0) >= 1 | dplyr::coalesce(production_share, 0) >= P$position_min),
    specialist_position = !reexport_hub & step_function == "components" & world_share >= P$position_min & dplyr::coalesce(export_rca, 0) >= P$spec_rca,
    momentum = !is.na(share_hist) & share_now - share_hist >= P$momentum_gain & share_now >= P$momentum_floor & share_now >= P$momentum_ratio * share_hist,
    host_scaled = dplyr::coalesce(host_scaled, FALSE), capable_host = dplyr::coalesce(capable_host, FALSE),
    world_import_share = ifelse(optimize & dplyr::coalesce(world_trade, 0) > 0, imports / world_trade, 0),
    supply_usd_eq = position_share * dplyr::coalesce(world_trade, 0))
  cn
}

# Each node's allied-scope imports (the demand scope's imports from every exporter), from the evidence.
aib_strategic_scope_demand <- function(cn) {
  cn |> dplyr::group_by(node_id) |> dplyr::summarise(scope_demand_usd = sum(imports), .groups = "drop")
}

# The workstream coalitions of both institutional scenarios (members, roles, bases, tiers), each workstream
# tagged with its strategic compact.
aib_strategic_ws_members <- function(res, ws) {
  mem <- dplyr::bind_rows(
    if (!is.null(res$compact_members)) dplyr::mutate(res$compact_members, lens = "decoupling"),
    if (!is.null(res$compact_members_managed)) dplyr::mutate(res$compact_members_managed, lens = "managed"))
  if (!nrow(mem)) return(mem)
  mem$ws_sc_id <- ws$strategic_compact_id[match(mem$compact_id, ws$workstream_id)]
  mem
}

# Per-country workstream evidence (either scenario).
aib_strategic_ws_country <- function(mem) {
  mem |> dplyr::group_by(iso3) |> dplyr::summarise(
    ws_member = dplyr::n_distinct(compact_id[in_minimum_coalition %in% TRUE]),
    sc_span = dplyr::n_distinct(stats::na.omit(ws_sc_id[in_minimum_coalition %in% TRUE])),
    ws_bridge = dplyr::n_distinct(compact_id[role == "Bridge partner"]),
    # the vertical basis only ("supplies one node of the compact and depends on another, or spans its
    # stages"); "member of several compacts' minimum coalitions" is excluded
    ws_bridge_vertical = dplyr::n_distinct(compact_id[role == "Bridge partner" & !grepl("several compacts", basis)]),
    sc_span_supply = dplyr::n_distinct(stats::na.omit(ws_sc_id[in_minimum_coalition %in% TRUE & role != "Demand anchor"])),
    ws_specialized = dplyr::n_distinct(compact_id[role %in% c("Specialized supplier", "Technology partner")]),
    ws_resource = dplyr::n_distinct(compact_id[role == "Resource supplier"]),
    ws_demand_anchor = dplyr::n_distinct(compact_id[role == "Demand anchor"]),
    ws_critical = dplyr::n_distinct(compact_id[critical_supplier %in% TRUE]),
    .groups = "drop")
}

# ---- Capital evidence -------------------------------------------------------------------------------------

# Outward direct-investment positions from IMF DIP rows (COUNTRY, COUNTERPART_COUNTRY, DV_TYPE, INDICATOR and
# year columns "2023" / "X2023"): the configured indicator and data type, the first configured year present,
# summed over counterpart economies that are not conduit centres; conduit reporters are dropped (not assessed).
# USD millions in, USD bn out. iso_of: function(names) -> ISO3 (default countrycode).
aib_capital_dip_outward <- function(dip, conduits, cfg = NULL, iso_of = NULL) {
  empty <- tibble::tibble(iso3 = character(), fdi_out_usd_bn = numeric(), fdi_year = integer())
  if (is.null(dip) || !nrow(dip)) return(empty)
  ind <- aib_st_or(cfg$dip_indicator, "Outward Direct investment, Net (assets less liabilities), All financial instruments, All entities")
  dv <- aib_st_or(cfg$dip_dv_type, "Reported official data")
  years <- as.character(unlist(aib_st_or(cfg$dip_years, list(2023, 2022))))
  d <- dip[dip$INDICATOR == ind & dip$DV_TYPE == dv, , drop = FALSE]
  if (!nrow(d)) return(empty)
  if (is.null(iso_of)) {
    iso_of <- function(x) suppressWarnings(countrycode::countrycode(x, "country.name", "iso3c", custom_match = c("Kosovo, Republic of" = "XKX")))
  }
  nm <- unique(c(d$COUNTRY, d$COUNTERPART_COUNTRY))
  iso <- stats::setNames(iso_of(nm), nm)
  rep_iso <- unname(iso[d$COUNTRY])
  par_iso <- unname(iso[d$COUNTERPART_COUNTRY])
  col <- function(y) { k <- intersect(c(y, paste0("X", y)), names(d)); if (length(k)) suppressWarnings(as.numeric(d[[k[1]]])) else rep(NA_real_, nrow(d)) }
  v <- rep(NA_real_, nrow(d))
  yr <- rep(NA_integer_, nrow(d))
  for (y in years) {
    x <- col(y)
    take <- is.na(v) & !is.na(x)
    v[take] <- x[take]
    yr[take] <- as.integer(y)
  }
  keep <- !is.na(rep_iso) & !is.na(par_iso) & rep_iso != par_iso & !par_iso %in% conduits & !is.na(v) & !rep_iso %in% conduits
  if (!any(keep)) return(empty)
  x <- tibble::tibble(iso3 = rep_iso[keep], v = v[keep], y = yr[keep])
  x |> dplyr::group_by(iso3) |> dplyr::summarise(fdi_out_usd_bn = sum(v) / 1000, fdi_year = max(y), .groups = "drop")
}

# Cross-border CM-FDI projects in the index sample (index_use, home != destination).
aib_capital_cmfdi_projects <- function(projects) {
  if (is.null(projects) || !nrow(projects)) return(NULL)
  p <- projects[projects$index_use %in% TRUE & !is.na(projects$home_iso3) & !is.na(projects$destination_iso3) &
                  projects$home_iso3 != projects$destination_iso3, , drop = FALSE]
  p$capital_w <- as.numeric(p$capital_usd_m) * as.numeric(p$attribution_weight)
  p
}

aib_capital_cmfdi_outward <- function(p) {
  if (is.null(p)) return(tibble::tibble(iso3 = character(), cmfdi_out_usd_bn = numeric(), cmfdi_destinations = integer()))
  p |> dplyr::group_by(iso3 = home_iso3) |>
    dplyr::summarise(cmfdi_out_usd_bn = sum(capital_w, na.rm = TRUE) / 1000, cmfdi_destinations = dplyr::n_distinct(destination_iso3), .groups = "drop")
}

# ---- Strategic roles: metrics and assignment -----------------------------------------------------------------

# Country metrics (docs/aib_methodology.md 16.4): one row per assessed economy, the allied demand scope plus
# the non-allied major suppliers (>= 10% of world supply of one mapped node, or >= 5% of three). capital:
# list(dip = aib_capital_dip_outward() or NULL, cmfdi = aib_capital_cmfdi_outward() or NULL); a missing input
# leaves its metric NA (the role is not assessed).
aib_country_role_metrics <- function(cn, nodes, ws_country, capital, allied, params) {
  P <- params
  MFG <- aib_strategic_mfg()
  rp_nodes <- nodes$node_id[nodes$step_function %in% c("resource", "processing")]
  mf_nodes <- nodes$node_id[nodes$step_function %in% MFG]
  # end-use goods: the nodes of each compact's last value-chain step (what a market buys rather than inputs
  # for its own production; 2026-10-21 review)
  end_nodes <- nodes$node_id[nodes$vc_order == stats::ave(nodes$vc_order, nodes$sc_id, FUN = max)]
  hub <- if ("reexport_hub" %in% names(cn)) cn$reexport_hub %in% TRUE else rep(FALSE, nrow(cn))
  # integration: >= presence_min of world exports in >= 2 manufacturing NODES of one compact (counting
  # nodes, not steps: an assembled-product chain has a single manufacturing step; 2026-10-21 review)
  integ <- cn[cn$step_function %in% MFG & cn$world_share >= P$presence_min & !hub, , drop = FALSE] |>
    dplyr::group_by(iso3, sc_id) |> dplyr::summarise(n_nodes = dplyr::n_distinct(node_id), .groups = "drop") |>
    dplyr::group_by(iso3) |> dplyr::summarise(integration_compacts = sum(n_nodes >= 2), .groups = "drop")
  mf_world <- sum(nodes$world_trade[nodes$node_id %in% mf_nodes], na.rm = TRUE)
  cm <- cn |> dplyr::group_by(iso3) |> dplyr::summarise(
    rp_positions = sum(anchor_position & node_id %in% rp_nodes),
    rp_mean_share = sum(position_share[node_id %in% rp_nodes]) / length(rp_nodes),
    spec_positions = sum(specialist_position),
    spec_compacts = dplyr::n_distinct(sc_id[specialist_position]),
    spec_value_share = sum(exports[specialist_position]) / mf_world,
    mfg_value_share = sum(exports[node_id %in% mf_nodes]) / mf_world,
    mfg_positions = sum(world_share >= P$presence_min & node_id %in% mf_nodes),
    demand_mean_share = sum(scope_demand_share) / nrow(nodes),
    demand_positions = sum(scope_demand_share >= P$demand_node_min),
    net_import_mean = sum(net_import_share) / nrow(nodes),
    net_import_mf = sum(net_import_share[node_id %in% mf_nodes]) / length(mf_nodes),
    net_import_end = sum(net_import_share[node_id %in% end_nodes]) / length(end_nodes),
    net_import_rp = sum(net_import_share[node_id %in% rp_nodes]) / length(rp_nodes),
    host_capable_gap_nodes = dplyr::n_distinct(node_id[capable_host]),
    host_top_scaled = dplyr::n_distinct(node_id[!is.na(best_host_rank) & host_scaled]),
    host_top_any = dplyr::n_distinct(node_id[!is.na(best_host_rank)]),
    host_gap_usd_bn = sum(host_gap_usd[!is.na(best_host_rank) & host_scaled], na.rm = TRUE) / 1e9,
    momentum_nodes = sum(momentum),
    presence_nodes = sum(position_share >= P$momentum_floor),
    .groups = "drop") |>
    dplyr::mutate(scaleup_rate = ifelse(presence_nodes > 0, momentum_nodes / presence_nodes, 0)) |>
    dplyr::left_join(integ, by = "iso3") |> dplyr::left_join(ws_country, by = "iso3")
  dip <- capital$dip
  cmf <- capital$cmfdi
  cty <- cm
  cty <- if (!is.null(dip)) dplyr::full_join(cty, dip, by = "iso3") else dplyr::mutate(cty, fdi_out_usd_bn = NA_real_, fdi_year = NA_integer_)
  cty <- if (!is.null(cmf)) dplyr::full_join(cty, cmf, by = "iso3") else dplyr::mutate(cty, cmfdi_out_usd_bn = NA_real_, cmfdi_destinations = NA_integer_)
  keep_na <- c("fdi_out_usd_bn", "fdi_year", if (is.null(cmf)) c("cmfdi_out_usd_bn", "cmfdi_destinations"))
  for (k in names(cty)) if (is.numeric(cty[[k]]) && !k %in% keep_na) cty[[k]] <- dplyr::coalesce(cty[[k]], 0)
  cty$allied <- cty$iso3 %in% allied
  major <- cn[!hub, , drop = FALSE] |> dplyr::group_by(iso3) |>
    dplyr::summarise(n10 = sum(position_share >= P$major_supplier_single), n5 = sum(position_share >= P$major_supplier_multi), .groups = "drop") |>
    dplyr::filter(n10 >= 1 | n5 >= P$major_supplier_n)
  cty <- cty[cty$allied | cty$iso3 %in% major$iso3, , drop = FALSE]
  cty$major_supplier <- cty$iso3 %in% major$iso3
  cty[order(cty$iso3), , drop = FALSE]
}

# Percentile among the eligible set (dplyr::percent_rank; 1 = the top; ties take the lower rank); NA (not
# assessed) stays NA.
aib_strategic_prank <- function(x, elig) {
  out <- rep(NA_real_, length(x))
  ok <- elig & !is.na(x)
  out[ok] <- dplyr::percent_rank(x[ok])
  out
}

# The seven roles (docs/aib_methodology.md 16.5): qualify (both gates > 0 and >= the bar), order (primary-
# qualifying first, role_strength in whole percentile points, the other gate, the fixed role order), cap
# (3 primary, a 4th demoted; 2 secondary; any further qualifying role "qualifies beyond the cap"). One row
# per assessed economy x role it is eligible for (allied: 7; non-allied supplier: the 3 supply roles).
aib_assign_strategic_roles <- function(cty, params) {
  P <- params
  ROLES <- aib_strategic_roles()
  GATES <- aib_strategic_gates()
  SUPPORT <- aib_strategic_support()
  long <- list()
  for (r in names(GATES)) {
    elig <- if (r %in% aib_strategic_supply_roles()) rep(TRUE, nrow(cty)) else cty$allied
    g <- GATES[[r]]
    pc <- vapply(g, function(m) aib_strategic_prank(cty[[m]], elig), numeric(nrow(cty)))
    val <- vapply(g, function(m) as.numeric(cty[[m]]), numeric(nrow(cty)))
    if (!is.matrix(pc)) { pc <- matrix(pc, ncol = 2); val <- matrix(val, ncol = 2) }
    pos <- val > 0
    pos[is.na(pos)] <- FALSE
    strength <- apply(pc, 1, min)
    other <- apply(pc, 1, max)
    allpos <- apply(pos, 1, all)
    sup <- c(g, SUPPORT[[r]])
    long[[r]] <- tibble::tibble(
      iso3 = cty$iso3, allied = cty$allied, role_code = r, strategic_role = ROLES[[r]], eligible = elig,
      gate1 = g[1], gate1_value = val[, 1], gate1_pct = pc[, 1], gate2 = g[2], gate2_value = val[, 2], gate2_pct = pc[, 2],
      all_positive = allpos, role_strength = strength, other_gate_pct = other,
      primary_ok = dplyr::coalesce(elig & allpos & strength >= P$p_primary, FALSE),
      secondary_ok = dplyr::coalesce(elig & allpos & strength >= P$p_secondary, FALSE),
      supporting_metrics = vapply(seq_len(nrow(cty)), function(i) paste(sprintf("%s=%s", sup, vapply(sup, function(m) {
        v <- cty[[m]][i]
        if (is.null(v) || is.na(v)) "NA" else format(signif(v, 3))
      }, "")), collapse = "; "), ""))
    long[[r]] <- long[[r]][long[[r]]$eligible, , drop = FALSE]
  }
  roles <- dplyr::bind_rows(long)
  roles <- roles[order(roles$iso3, -roles$primary_ok, -roles$secondary_ok, -round(dplyr::coalesce(roles$role_strength, -1), 2),
                       -dplyr::coalesce(roles$other_gate_pct, -1), match(roles$role_code, names(ROLES))), , drop = FALSE]
  roles <- roles |> dplyr::group_by(iso3) |>
    dplyr::mutate(n_prim = cumsum(primary_ok),
                  primary_or_secondary = dplyr::case_when(primary_ok & n_prim <= P$max_primary ~ "primary",
                                                          secondary_ok | primary_ok ~ "secondary", TRUE ~ NA_character_)) |>
    dplyr::mutate(n_sec = cumsum(primary_or_secondary %in% "secondary"),
                  primary_or_secondary = ifelse(primary_or_secondary %in% "secondary" & n_sec > P$max_secondary, "qualifies beyond the cap", primary_or_secondary),
                  role_rank = ifelse(is.na(primary_or_secondary), NA_integer_, cumsum(!is.na(primary_or_secondary)))) |>
    dplyr::ungroup() |> dplyr::select(-n_prim, -n_sec)
  roles
}

# ---- Compact-specific country roles -----------------------------------------------------------------------

# One row per country x strategic compact where the country performs at least one function
# (docs/aib_methodology.md 16.6), with the lead role and the evidence. The lead is the function behind the
# country's largest position: world supply share against world import share; positions within lead_margin
# of the largest tie, and a tie goes to a supply function over demand, then to the larger share (2026-10-21
# review: not to the larger US$ flow, which compares different nodes). Investment, host, bridge and
# diversification lead only without a supply or demand lead; demand alone leads as a last resort. Rules that
# keep the lead on what the country contributes (2026-10-21 validation and review):
#   * input demand does not lead: a demand position in a step before the country's last supply step (any
#     resource, processing, specialist or scale node) is input buying for its own downstream production;
#     only a demand position at or after that step competes for the lead (it stays among the functions);
#   * a demand position leads only with >= presence_min of world imports of its node;
#   * scale: >= presence_min of world exports of a manufacturing node that is not a specialist position
#     (node level: a step that bundles several products, e.g. network equipment, no longer hides a
#     transformer or meter maker); scale_steps10 / scale_steps5 keep the step-level evidence;
#   * a re-export hub (reexport_hub) holds no supply function.
# Also returns `steps`, the country x compact x step division of labour.
#   mem          aib_strategic_ws_members()
#   cmfdi        aib_capital_cmfdi_projects() (NULL: no investment function)
#   partnerships res$partnerships (resilience contribution of new and expanded relationships: US$ restored and
#                its share of the compact's allied demand, node_demand = named allied demand per node)
#   semantics, constraint  the headline shock semantics and supply constraint (filters, when present)
aib_compact_country_roles <- function(cn, nodes, mem, cmfdi, partnerships, allied, params, sector_map = NULL,
                                      shocks = c(managed = NA_character_, decoupling = NA_character_), scenario = NULL,
                                      node_demand = NULL, semantics = NULL, constraint = NULL) {
  if (!"reexport_hub" %in% names(cn)) cn$reexport_hub <- FALSE
  P <- params
  MFG <- aib_strategic_mfg()
  FUN_LABEL <- aib_strategic_compact_functions()
  scope_dem <- aib_strategic_scope_demand(cn)
  # investment contribution: the country's share of the outward CM-FDI capital in the compact's sectors
  cmf_sc <- if (is.null(cmfdi) || is.null(sector_map)) NULL else {
    sm <- unlist(sector_map)
    x <- cmfdi[cmfdi$sector %in% names(sm), , drop = FALSE]
    x$sc_id <- unname(sm[x$sector])
    x |> dplyr::group_by(sc_id) |> dplyr::mutate(tot = sum(capital_w, na.rm = TRUE)) |>
      dplyr::group_by(sc_id, iso3 = home_iso3) |> dplyr::summarise(cmfdi_sc_share = sum(capital_w, na.rm = TRUE) / dplyr::first(tot), .groups = "drop")
  }
  cmf_compacts <- if (is.null(sector_map)) character() else unique(unlist(sector_map))
  mem_sc <- if (!nrow(mem)) NULL else mem |> dplyr::group_by(iso3, sc_id = ws_sc_id) |> dplyr::summarise(
    ws_core = dplyr::n_distinct(compact_id[in_minimum_coalition %in% TRUE]),
    ws_any = dplyr::n_distinct(compact_id[coalition_tier %in% c("core coordinating member", "associate partner", "critical supply partner")]),
    bridge = any(role == "Bridge partner"), demand_anchor_ws = any(role == "Demand anchor"),
    critical = any(critical_supplier %in% TRUE & in_minimum_coalition %in% TRUE),
    critical_partner_tier = any(coalition_tier %in% "critical supply partner"),
    critical_external = any(coalition_tier %in% c("critical external dependency")),
    .groups = "drop")
  sc_world <- nodes |> dplyr::group_by(sc_id) |> dplyr::summarise(all_world = sum(world_trade, na.rm = TRUE), .groups = "drop")
  sc_dem <- scope_dem |> dplyr::inner_join(dplyr::select(nodes, node_id, sc_id), by = "node_id") |> dplyr::group_by(sc_id) |>
    dplyr::summarise(tot = sum(scope_demand_usd), .groups = "drop")
  imp_sc <- cn[cn$iso3 %in% allied, , drop = FALSE] |> dplyr::group_by(iso3, sc_id) |> dplyr::summarise(imports = sum(imports), .groups = "drop") |>
    dplyr::left_join(sc_dem, by = "sc_id") |> dplyr::transmute(iso3, sc_id, demand_contribution = imports / tot)
  # resilience contribution: the US$ its new and expanded relationships restore in the compact's nodes, and
  # that as a share of the compact's allied demand (2026-10-21 review: not a sum of node-level shares)
  pt <- partnerships[partnerships$relationship != "domestic retention" & partnerships$shock_id %in% stats::na.omit(shocks), , drop = FALSE]
  if (!is.null(scenario) && "scenario" %in% names(pt)) pt <- pt[pt$scenario == scenario, , drop = FALSE]
  if (!is.null(semantics) && "shock_semantics" %in% names(pt)) pt <- pt[pt$shock_semantics == semantics, , drop = FALSE]
  if (!is.null(constraint) && "supply_constraint" %in% names(pt)) pt <- pt[pt$supply_constraint == constraint, , drop = FALSE]
  ndem <- if (is.null(node_demand)) stats::setNames(scope_dem$scope_demand_usd, scope_dem$node_id) else node_demand
  if (!"delta_usd" %in% names(pt)) pt$delta_usd <- pt$delta_share_of_demand * dplyr::coalesce(as.numeric(ndem[pt$node_id]), 0)
  sc_ndem <- nodes |> dplyr::mutate(d = dplyr::coalesce(as.numeric(ndem[node_id]), 0)) |> dplyr::group_by(sc_id) |>
    dplyr::summarise(sc_demand_usd = sum(d), .groups = "drop")
  pt <- pt |> dplyr::inner_join(dplyr::select(nodes, node_id, sc_id), by = "node_id") |>
    dplyr::group_by(iso3 = exporter_iso, sc_id) |>
    dplyr::summarise(resilience_usd_managed = sum(pmax(delta_usd[shock_id %in% shocks[["managed"]]], 0)),
                     resilience_usd_decoupling = sum(pmax(delta_usd[shock_id %in% shocks[["decoupling"]]], 0)), .groups = "drop") |>
    dplyr::left_join(sc_ndem, by = "sc_id") |>
    dplyr::mutate(resilience_managed = ifelse(sc_demand_usd > 0, resilience_usd_managed / sc_demand_usd, NA_real_),
                  resilience_decoupling = ifelse(sc_demand_usd > 0, resilience_usd_decoupling / sc_demand_usd, NA_real_)) |>
    dplyr::select(-sc_demand_usd)
  # The division of labour by step (a step's nodes of one function together): supply share = the country's
  # world export (or production) share of the step weighted by world trade; demand share = its share of the
  # allied scope's imports of the step's optimized nodes (step level, every function).
  step_world <- nodes |> dplyr::group_by(sc_id, vc_order) |>
    dplyr::summarise(step_world = sum(world_trade, na.rm = TRUE), step_world_opt = sum(world_trade[optimize], na.rm = TRUE), .groups = "drop")
  step_dem <- scope_dem |> dplyr::inner_join(dplyr::select(nodes[nodes$optimize, ], node_id, sc_id, vc_order), by = "node_id") |>
    dplyr::group_by(sc_id, vc_order) |> dplyr::summarise(step_demand = sum(scope_demand_usd), .groups = "drop")
  steps <- cn |> dplyr::group_by(iso3, sc_id, vc_order, vc_step, step_function) |> dplyr::summarise(
    supply_usd_eq = sum(supply_usd_eq), imports_opt = sum(imports[optimize]),
    specialist = any(specialist_position), anchor = any(anchor_position), .groups = "drop") |>
    dplyr::left_join(step_world, by = c("sc_id", "vc_order")) |>
    dplyr::mutate(step_supply_share = supply_usd_eq / step_world)
  step_level <- steps |> dplyr::group_by(iso3, sc_id, vc_order, vc_step) |>
    dplyr::summarise(imports_opt = sum(imports_opt), .groups = "drop") |>
    dplyr::left_join(step_dem, by = c("sc_id", "vc_order")) |> dplyr::left_join(step_world, by = c("sc_id", "vc_order")) |>
    dplyr::mutate(step_demand_share = ifelse(iso3 %in% allied & !is.na(step_demand), imports_opt / step_demand, NA_real_),
                  step_world_demand_share = ifelse(iso3 %in% allied & step_world_opt > 0, imports_opt / step_world_opt, NA_real_))
  n_steps <- nodes |> dplyr::group_by(sc_id) |>
    dplyr::summarise(n_steps = dplyr::n_distinct(vc_order), n_mfg_steps = dplyr::n_distinct(vc_order[step_function %in% MFG]), .groups = "drop")
  sc_steps <- steps |> dplyr::group_by(iso3, sc_id) |> dplyr::summarise(
    scale_steps10 = sum(step_function %in% MFG & step_supply_share >= P$position_min),
    scale_steps5 = sum(step_function %in% MFG & step_supply_share >= P$presence_min),
    step_supply_sum = sum(step_supply_share, na.rm = TRUE), .groups = "drop") |>
    dplyr::left_join(step_level |> dplyr::group_by(iso3, sc_id) |> dplyr::summarise(
      demand_side = suppressWarnings(max(c(0, step_demand_share), na.rm = TRUE)),
      step_demand_sum = sum(step_world_demand_share, na.rm = TRUE), .groups = "drop"), by = c("iso3", "sc_id")) |>
    dplyr::left_join(n_steps, by = "sc_id") |>
    dplyr::mutate(mean_step_supply = step_supply_sum / n_steps, mean_step_demand = dplyr::coalesce(step_demand_sum, 0) / n_steps,
                  demand_side = dplyr::coalesce(demand_side, 0),
                  # step-level scale evidence (reported): >= 10% of one manufacturing step, or >= 5% of two (of
                  # every manufacturing step when the compact has fewer than two); the scale FUNCTION is node level (below)
                  f_scale_step = scale_steps10 >= 1 | scale_steps5 >= pmax(1, pmin(2, dplyr::coalesce(n_mfg_steps, 0L))))
  maxz <- function(x) if (length(x)) max(x) else 0
  top_node <- function(id, v, keep) if (any(keep)) id[keep][which.max(v[keep])] else NA_character_
  top_val <- function(x, v, keep) if (any(keep)) x[keep][which.max(v[keep])] else 0
  lab <- function(ids) paste(aib_strategic_prose_label(nodes$sub_sector[match(ids, nodes$node_id)]), collapse = "; ")
  lt <- cn |> dplyr::left_join(dplyr::select(step_level, iso3, sc_id, vc_order, step_demand_share), by = c("iso3", "sc_id", "vc_order")) |>
    dplyr::mutate(k_res = step_function == "resource" & anchor_position, k_pro = step_function == "processing" & anchor_position,
                  k_spe = specialist_position,
                  k_sca = step_function %in% MFG & !specialist_position & world_share >= P$presence_min & !(reexport_hub %in% TRUE),
                  k_dem = optimize & dplyr::coalesce(step_demand_share, 0) >= P$position_min,
                  k_host = (!is.na(best_host_rank) & host_scaled) | (capable_host & momentum)) |>
    # Input demand is not a market role: a demand position in a step BEFORE the country's last supply step
    # (any resource, processing, specialist or scale node) buys inputs for its own downstream production. It
    # stays a function, but only a demand position at or after that step (any step for a country that
    # supplies nothing in the compact) competes for the lead role.
    dplyr::group_by(iso3, sc_id) |>
    dplyr::mutate(last_supply_step = suppressWarnings(max(c(0L, vc_order[k_res | k_pro | k_spe | k_sca]))),
                  k_mkt = k_dem & vc_order >= last_supply_step) |>
    dplyr::ungroup()
  ccr <- lt |> dplyr::group_by(iso3, sc_id) |> dplyr::summarise(
    f_resource = any(k_res), f_processing = any(k_pro), f_specialized = any(k_spe), f_scale_node = any(k_sca),
    max_position = maxz(position_share[!(reexport_hub %in% TRUE)]),
    sh_resource = maxz(position_share[k_res]), nd_resource = top_node(node_id, position_share, k_res), us_resource = top_val(supply_usd_eq, position_share, k_res),
    sh_processing = maxz(position_share[k_pro]), nd_processing = top_node(node_id, position_share, k_pro), us_processing = top_val(supply_usd_eq, position_share, k_pro),
    sh_specialized = maxz(position_share[k_spe]), nd_specialized = top_node(node_id, position_share, k_spe), us_specialized = top_val(supply_usd_eq, position_share, k_spe),
    sh_scale = maxz(position_share[k_sca]), nd_scale = top_node(node_id, position_share, k_sca), us_scale = top_val(supply_usd_eq, position_share, k_sca),
    demand_lead_share = if (any(k_dem)) max(world_import_share[k_dem]) else maxz(world_import_share[optimize]),
    nd_demand = if (any(k_dem)) top_node(node_id, world_import_share, k_dem) else top_node(node_id, world_import_share, optimize),
    us_demand = if (any(k_dem)) top_val(imports, world_import_share, k_dem) else top_val(imports, world_import_share, optimize),
    last_supply_step = dplyr::first(last_supply_step),
    # the demand position that competes for the lead (NA: only input demand)
    market_lead_share = if (any(k_mkt)) max(world_import_share[k_mkt]) else if (dplyr::first(last_supply_step) == 0) demand_lead_share else NA_real_,
    nd_market = if (any(k_mkt)) top_node(node_id, world_import_share, k_mkt) else if (dplyr::first(last_supply_step) == 0) nd_demand else NA_character_,
    us_market = if (any(k_mkt)) top_val(imports, world_import_share, k_mkt) else if (dplyr::first(last_supply_step) == 0) us_demand else NA_real_,
    all_exports = sum(exports),
    host_scaled_nodes = dplyr::n_distinct(node_id[k_host]),
    key_nodes = lab(utils::head(node_id[order(-position_share)][position_share[order(-position_share)] >= P$presence_min], 3)),
    host_nodes = lab(unique(node_id[k_host])),
    .groups = "drop") |>
    dplyr::left_join(sc_steps, by = c("iso3", "sc_id")) |>
    dplyr::left_join(sc_world, by = "sc_id") |> dplyr::left_join(imp_sc, by = c("iso3", "sc_id")) |>
    dplyr::left_join(pt, by = c("iso3", "sc_id"))
  ccr <- if (is.null(cmf_sc)) dplyr::mutate(ccr, cmfdi_sc_share = NA_real_) else dplyr::left_join(ccr, cmf_sc, by = c("iso3", "sc_id"))
  ccr <- if (is.null(mem_sc)) dplyr::mutate(ccr, ws_core = 0L, ws_any = 0L, bridge = FALSE, demand_anchor_ws = FALSE, critical = FALSE,
                                            critical_partner_tier = FALSE, critical_external = FALSE) else
    dplyr::left_join(ccr, mem_sc, by = c("iso3", "sc_id"))
  ccr <- ccr |> dplyr::mutate(
    allied = iso3 %in% allied,
    supplier_contribution = all_exports / all_world, demand_contribution = dplyr::coalesce(demand_contribution, 0),
    investment_contribution = ifelse(sc_id %in% cmf_compacts & !is.null(cmf_sc), dplyr::coalesce(cmfdi_sc_share, 0), NA_real_),
    resilience_managed = dplyr::coalesce(resilience_managed, 0), resilience_decoupling = dplyr::coalesce(resilience_decoupling, 0),
    ws_core = dplyr::coalesce(as.integer(ws_core), 0L), ws_any = dplyr::coalesce(as.integer(ws_any), 0L),
    bridge = dplyr::coalesce(bridge, FALSE), demand_anchor_ws = dplyr::coalesce(demand_anchor_ws, FALSE),
    # a critical supplier must hold >= presence_min of world supply of a node of the compact (a re-exporter
    # below that share is not one; 2026-10-21 review)
    critical = dplyr::coalesce(critical, FALSE) & max_position >= P$presence_min,
    critical_partner_tier = dplyr::coalesce(critical_partner_tier, FALSE), critical_external = dplyr::coalesce(critical_external, FALSE),
    f_scale_step = dplyr::coalesce(f_scale_step, FALSE),
    f_scale = f_scale_node,
    f_demand = allied & (demand_side >= P$position_min | demand_anchor_ws),
    f_investment = allied & dplyr::coalesce(investment_contribution, 0) >= P$position_min,
    f_host = allied & host_scaled_nodes > 0,
    f_bridge = allied & (bridge | ws_core >= 2),
    f_divers = allied & ws_any > 0)
  SUPPLY_SH <- c(f_resource = "sh_resource", f_processing = "sh_processing", f_specialized = "sh_specialized", f_scale = "sh_scale")
  flags <- as.matrix(ccr[, names(FUN_LABEL)])
  ccr$compact_functions <- apply(flags, 1, function(z) paste(FUN_LABEL[names(FUN_LABEL)[as.logical(z)]], collapse = "; "))
  ccr$compact_role <- vapply(seq_len(nrow(ccr)), function(i) {
    r <- ccr[i, ]
    met <- names(FUN_LABEL)[flags[i, ]]
    if (!length(met)) return(NA_character_)
    sup <- intersect(names(SUPPLY_SH), met)
    mkt_ok <- "f_demand" %in% met && !is.na(r$market_lead_share) && r$market_lead_share >= P$presence_min
    cand <- c(sup, if (mkt_ok) "f_demand")
    sh <- vapply(cand, function(f) if (f == "f_demand") r$market_lead_share else r[[SUPPLY_SH[[f]]]], 0)
    if (length(sh) && max(sh) > 0) {
      tie <- cand[sh >= max(sh) - P$lead_margin]
      tie_sup <- setdiff(tie, "f_demand")
      # a supply function beats demand within the margin; among supply functions (the same measure, world
      # export share) the larger US$ position leads (a 17% chip position over an 18% gallium one)
      if (!length(tie_sup)) return(FUN_LABEL[["f_demand"]])
      us <- vapply(tie_sup, function(f) dplyr::coalesce(r[[sub("^sh_", "us_", SUPPLY_SH[[f]])]], 0), 0)
      return(FUN_LABEL[[tie_sup[order(-us, -sh[tie_sup], match(tie_sup, names(FUN_LABEL)))][1]]])
    }
    if (length(sup)) return(FUN_LABEL[[sup[1]]])
    rest <- intersect(c("f_investment", "f_host", "f_bridge", "f_divers"), met)
    if (length(rest)) return(FUN_LABEL[[rest[1]]])
    FUN_LABEL[["f_demand"]]
  }, "")
  ccr$compact_role_detail <- vapply(seq_len(nrow(ccr)), function(i) {
    r <- ccr[i, ]
    lead <- r$compact_role
    if (is.na(lead)) return(NA_character_)
    sup <- names(SUPPLY_SH)[flags[i, names(SUPPLY_SH)]]
    sup <- sup[order(-vapply(sup, function(f) r[[SUPPLY_SH[[f]]]], 0))]
    oth <- c(sup, intersect(c("f_investment", "f_demand", "f_host"), names(FUN_LABEL)[flags[i, ]]))
    oth <- setdiff(unname(FUN_LABEL[oth]), lead)
    paste(c(lead, tolower(utils::head(oth, 2))), collapse = " / ")
  }, "")
  lead_col <- function(role, d, r, p, s, c, default) dplyr::case_when(role == "Demand anchor" ~ d, role == "Resource supplier" ~ r,
                                                                       role == "Processing anchor" ~ p, role == "Specialized technology supplier" ~ s,
                                                                       role == "Scale manufacturer" ~ c, TRUE ~ default)
  ccr <- ccr |> dplyr::mutate(
    lead_function = names(FUN_LABEL)[match(compact_role, FUN_LABEL)],
    lead_node = lead_col(compact_role, nd_market, nd_resource, nd_processing, nd_specialized, nd_scale, NA_character_),
    lead_step = nodes$vc_step[match(lead_node, nodes$node_id)],
    lead_share = lead_col(compact_role, market_lead_share, sh_resource, sh_processing, sh_specialized, sh_scale, NA_real_),
    lead_share_basis = dplyr::case_when(compact_role == "Demand anchor" ~ "share of world imports", !is.na(lead_node) ~ "share of world exports or production", TRUE ~ NA_character_),
    # an allied economy is never an "external dependency": one that a workstream lists as a critical external
    # dependency and that is no minimum-coalition member or critical partner in any workstream of the compact
    # is an allied supplier outside the core coalition (2026-10-21 review)
    criticality = dplyr::case_when(allied & critical ~ "critical supply partner (no single substitute) in a workstream",
                                   critical_external & !allied ~ "critical external dependency",
                                   critical_external & allied & ws_core == 0 & !critical_partner_tier ~ "allied supplier outside the core coalition",
                                   TRUE ~ ""),
    compact_role = ifelse(!allied & !is.na(compact_role), paste0(compact_role, " (non-allied supplier)"), compact_role),
    # relevance: value-weighted (its share of the compact's world trade as a supplier, or of its allied demand as
    # a buyer, the larger); the unweighted step means stay as evidence (2026-10-21 review)
    compact_relevance = pmax(dplyr::coalesce(supplier_contribution, 0), dplyr::coalesce(demand_contribution, 0))) |>
    dplyr::filter(!is.na(compact_role))
  steps_out <- steps |> dplyr::left_join(dplyr::select(step_level, iso3, sc_id, vc_order, step_demand_share, step_world_demand_share),
                                        by = c("iso3", "sc_id", "vc_order"))
  list(compact_roles = ccr, steps = steps_out)
}

# The structural core of each compact (the same in every scenario): the major players and the upstream anchors
# of the chain. A value ranking alone filled the magnets core with motor makers, and one leader per function
# alone let niche leaders (quartz, monazite) push out the largest manufacturers (2026-10-21), so the core is:
# the `k_value` most relevant allied economies (value-weighted relevance); then, for each upstream function
# (resource supplier, processing anchor), the allied economy with the strongest lead position in it, if that
# position is at least `upstream_min` of world supply and the economy is not already in; then the next most
# relevant allied economies, up to k in all. Listed in chain (function) order. Non-allied lead suppliers apart.
# One row per compact x listed country (core: allied, rank 1..k; non-allied lead suppliers: rank 1..3 among
# those whose lead role is a supply role), with the lead role (without the non-allied suffix).
AIB_CORE_FUNCTION_ORDER <- c("Resource supplier", "Processing anchor", "Specialized technology supplier", "Scale manufacturer", "Demand anchor")
aib_strategic_core_long <- function(compact_roles, k = 6L, k_value = 3L, upstream_min = 0.10) {
  x <- compact_roles[order(compact_roles$sc_id, -compact_roles$compact_relevance, compact_roles$iso3), , drop = FALSE]
  fo <- AIB_CORE_FUNCTION_ORDER
  upstream <- fo[1:2]
  dplyr::bind_rows(lapply(split(x, x$sc_id), function(d) {
    al <- d[d$allied, , drop = FALSE]
    top <- utils::head(al, k_value)
    ls <- if ("lead_share" %in% names(al)) dplyr::coalesce(al$lead_share, 0) else rep(0, nrow(al))
    cand <- al[al$compact_role %in% upstream & ls >= upstream_min & !al$iso3 %in% top$iso3, , drop = FALSE]
    cand <- cand[order(match(cand$compact_role, fo), -dplyr::coalesce(cand$lead_share, 0), -cand$compact_relevance, cand$iso3), , drop = FALSE]
    up <- cand[!duplicated(cand$compact_role), , drop = FALSE]
    lead <- utils::head(dplyr::bind_rows(top, up), k)
    rest <- al[!al$iso3 %in% lead$iso3, , drop = FALSE]
    a <- dplyr::bind_rows(lead, utils::head(rest, max(0L, k - nrow(lead))))
    a <- a[order(dplyr::coalesce(match(a$compact_role, fo), length(fo) + 1L), -a$compact_relevance, a$iso3), , drop = FALSE]
    na <- utils::head(d[!d$allied & grepl("supplier|anchor|manufacturer", d$compact_role), , drop = FALSE], 3)
    b <- dplyr::bind_rows(a, na)
    tibble::tibble(strategic_compact_id = b$sc_id, iso3 = b$iso3, allied = b$allied,
                   rank = c(seq_len(nrow(a)), seq_len(nrow(na))),
                   compact_role = sub(" \\(non-allied supplier\\)$", "", b$compact_role), lead_step = b$lead_step,
                   compact_relevance = b$compact_relevance)
  }))
}

aib_strategic_core <- function(compact_roles, k = 6L, country_name = NULL) {
  cname <- function(x) if (is.null(country_name)) x else ifelse(is.na(country_name[x]), x, country_name[x])
  long <- aib_strategic_core_long(compact_roles, k)
  role_of <- function(r) paste0(tolower(r$compact_role), ifelse(is.na(r$lead_step), "", paste0(": ", tolower(r$lead_step))))
  dplyr::bind_rows(lapply(split(long, long$strategic_compact_id), function(d) {
    a <- d[d$allied, , drop = FALSE]
    na <- d[!d$allied, , drop = FALSE]
    tibble::tibble(strategic_compact_id = d$strategic_compact_id[1], core_countries = paste(a$iso3, collapse = " "),
                   core_country_roles = paste(sprintf("%s - %s", cname(a$iso3), role_of(a)), collapse = "; "),
                   non_allied_suppliers = paste(na$iso3, collapse = " "),
                   non_allied_supplier_roles = paste(sprintf("%s - %s", cname(na$iso3), role_of(na)), collapse = "; "))
  }))
}

# ---- Narratives -------------------------------------------------------------------------------------------

# A node label for prose: an aggregated node's generated label ("Uranium/fuel Upstream: aggregated low-coverage
# sub-sectors (Spent fuel & nuclear scrap, Uranium ores)") becomes its members ("Spent fuel & nuclear scrap and
# uranium ores (aggregated)"); every other label is unchanged.
aib_strategic_prose_label <- function(label) {
  m <- regmatches(label, regexec("^.*: aggregated low-coverage sub-sectors \\((.*)\\)$", label))
  vapply(seq_along(label), function(i) {
    if (is.na(label[i]) || length(m[[i]]) != 2) return(label[i])
    parts <- strsplit(m[[i]][2], ", ", fixed = TRUE)[[1]]
    low <- grepl("^[A-Z][a-z]", parts) & seq_along(parts) > 1
    parts[low] <- paste0(tolower(substr(parts[low], 1, 1)), substring(parts[low], 2))
    paste0(aib_st_and(parts), " (aggregated)")
  }, "")
}

# Template narratives (docs/aib_methodology.md 16.7): one row per allied economy and per non-allied supplier
# with a role, each slot a column. No text anywhere is written for a particular country. Beside each list slot,
# its items as codes (items_* columns, space-separated; *_share and *_usd_bn the matching numbers) so that a
# reader never has to split the prose back apart: items_first_on gate metrics, items_contributes, items_needs
# and items_capacity node ids, items_focus node functions, items_compacts strategic compact ids, items_partners
# ISO3 codes. List slots rank by value, not share (2026-10-21 review), and show the share: contributes by
# the US$ equivalent of the position (share x the node's world trade), needs by US$ net imports, capacity by
# the credited US$ (aib_country_node_evidence()), partners by the US$ that new and expanded trade between
# the two restores under Managed Interdependence and Decoupling (pair_usd: tibble(supplier_iso, buyer_iso,
# usd), aib_strategic_pair_usd(); investment edges are left out, since their hosts mostly tie).
aib_country_role_narratives <- function(roles, cty, cn, nodes, compact_roles, pair_usd, allied, params, country_name = NULL) {
  P <- params
  ROLES <- aib_strategic_roles()
  role_noun <- aib_strategic_role_nouns()
  GATES <- aib_strategic_gates()
  GATE_LABEL <- aib_strategic_gate_labels()
  cname <- function(x) if (is.null(country_name)) x else ifelse(is.na(country_name[x]), x, country_name[x])
  sub_label <- function(ids) aib_strategic_prose_label(nodes$sub_sector[match(ids, nodes$node_id)])
  num_items <- function(x) paste(format(signif(x, 4), scientific = FALSE, trim = TRUE), collapse = " ")
  item <- function(text = "", ids = character(), vals = NULL, total = NA_real_) {
    list(text = text, ids = paste(ids, collapse = " "), vals = if (is.null(vals)) "" else num_items(vals), total = total)
  }
  node_tier <- table(nodes$step_function)
  fn_focus <- function(iso) {
    x <- cn[cn$iso3 == iso, , drop = FALSE]
    if (!nrow(x)) return(item())
    tot <- tapply(x$position_share, x$step_function, function(v) sum(v >= P$presence_min))
    tot <- tot / as.numeric(node_tier[names(tot)])
    tot <- tot[!is.na(tot) & tot > 0]
    if (!length(tot)) return(item())
    tot <- tot[tot >= 0.5 * max(tot)]
    top <- names(sort(tot, decreasing = TRUE))[seq_len(min(2, length(tot)))]
    item(paste(aib_strategic_function_labels()[top], collapse = " and "), top)
  }
  sc_name <- function(id) nodes$sc_name[match(id, nodes$sc_id)]
  top_compacts <- function(iso, k = 3) {
    x <- compact_roles[compact_roles$iso3 == iso, , drop = FALSE]
    x <- utils::head(x[order(-x$compact_relevance, x$sc_id), , drop = FALSE], k)
    if (!nrow(x)) return(item())
    item(paste(sprintf("%s (%s%s)", sc_name(x$sc_id), tolower(x$compact_role), ifelse(is.na(x$lead_step), "", paste0(": ", tolower(x$lead_step)))), collapse = ", "),
         x$sc_id)
  }
  sdem <- aib_strategic_scope_demand(cn)
  sdem <- stats::setNames(sdem$scope_demand_usd, sdem$node_id)
  needs <- function(iso, k = 3) {
    x <- cn[cn$iso3 == iso & cn$optimize & cn$net_import_share >= 0.05, , drop = FALSE]
    x$net_usd <- x$net_import_share * dplyr::coalesce(as.numeric(sdem[x$node_id]), 0)
    x <- utils::head(x[order(-x$net_usd, x$node_id), , drop = FALSE], k)
    if (!nrow(x)) return(item())
    item(paste(sprintf("%s (%.0f%% of allied demand)", sub_label(x$node_id), 100 * x$net_import_share), collapse = ", "), x$node_id, x$net_import_share)
  }
  al <- cty[cty$allied, , drop = FALSE]
  dep_pct <- tibble::tibble(iso3 = al$iso3, p_rp = dplyr::percent_rank(al$net_import_rp), p_mf = dplyr::percent_rank(al$net_import_mf))
  dep_profile <- function(iso) {
    d <- dep_pct[dep_pct$iso3 == iso, , drop = FALSE]
    if (!nrow(d)) return("")
    k <- c(if (d$p_rp >= P$p_secondary) "upstream materials", if (d$p_mf >= P$p_secondary) "manufactured components and equipment")
    if (!length(k)) "" else paste0("a significant net buyer of ", paste(k, collapse = " and "), " (top quartile of allied net imports)")
  }
  e <- pair_usd
  partners <- function(iso, k = 3) {
    if (is.null(e) || !nrow(e)) return(item())
    x <- e[e$supplier_iso == iso | e$buyer_iso == iso, , drop = FALSE]
    if (!nrow(x)) return(item())
    w <- dplyr::coalesce(as.numeric(x$usd), 0)
    other <- ifelse(x$supplier_iso == iso, x$buyer_iso, x$supplier_iso)
    keep <- other %in% allied & other != iso & w > 0
    if (!any(keep)) return(item())
    s <- tapply(w[keep], other[keep], sum)
    s <- s[order(-s, names(s))]
    top <- utils::head(names(s), k)
    item(paste(cname(top), collapse = ", "), top)
  }
  gates_all <- unique(unlist(GATES))
  allied_rank <- vapply(gates_all, function(m) {
    v <- ifelse(cty$allied, cty[[m]], NA_real_)
    r <- rank(-v, ties.method = "min", na.last = "keep")
    ifelse(!is.na(v) & v > 0, r, NA_integer_)
  }, numeric(nrow(cty)))
  if (!is.matrix(allied_rank)) allied_rank <- matrix(allied_rank, nrow = nrow(cty), dimnames = list(NULL, gates_all))
  rownames(allied_rank) <- cty$iso3
  # the gates of its qualifying roles (primary, secondary and beyond the cap) on which it is first among allied economies
  first_on <- function(iso, codes) {
    if (!iso %in% rownames(allied_rank) || !length(codes)) return(item())
    g <- unique(unlist(GATES[codes]))
    g <- g[!is.na(allied_rank[iso, g]) & allied_rank[iso, g] == 1]
    if (!length(g)) item() else item(paste(GATE_LABEL[g], collapse = ", "), g)
  }
  key_positions <- function(iso, k = 3) {
    x <- cn[cn$iso3 == iso & (cn$anchor_position | cn$specialist_position), , drop = FALSE]
    x <- utils::head(x[order(-x$supply_usd_eq, -x$position_share, x$node_id), , drop = FALSE], k)
    if (!nrow(x)) return(item())
    item(paste(sprintf("%s (%.0f%%)", sub_label(x$node_id), 100 * x$position_share), collapse = ", "), x$node_id, x$position_share)
  }
  capacity_nodes <- function(iso, k = 3) {
    x <- cn[cn$iso3 == iso & !is.na(cn$best_host_rank) & cn$host_scaled, , drop = FALSE]
    x <- utils::head(x[order(-x$host_gap_usd, x$node_id), , drop = FALSE], k)
    if (!nrow(x)) return(item())
    tot <- sum(x$host_gap_usd) / 1e9
    item(paste0(paste(sub_label(x$node_id), collapse = ", "), sprintf(" (US$%.1fbn of unmet allied demand)", tot)), x$node_id, x$host_gap_usd / 1e9, tot)
  }
  role_list <- function(codes) {
    n <- role_noun[codes]
    if (length(n) <= 1) unname(n) else paste0(paste(n[-length(n)], collapse = ", "), " and ", n[length(n)])
  }
  plural <- function(n) if (n > 1) "s" else ""
  r <- roles
  r$pos <- dplyr::coalesce(r$primary_or_secondary, "")
  r <- r[order(r$iso3, r$role_rank), , drop = FALSE]
  isos <- sort(unique(r$iso3))
  out <- lapply(isos, function(iso) {
    x <- r[r$iso3 == iso, , drop = FALSE]
    prim <- x$role_code[x$pos == "primary"]
    sec <- x$role_code[x$pos == "secondary"]
    beyond <- x$role_code[x$pos == "qualifies beyond the cap"]
    is_allied <- iso %in% allied
    if (!is_allied && !length(prim) && !length(sec)) return(NULL)
    margin <- if (length(prim) >= 2) x$role_strength[x$pos == "primary"][1] - x$role_strength[x$pos == "primary"][2] else NA_real_
    headline <- if (!length(prim) && !length(sec)) "No strategic AIB role at the current thresholds; contributes through its compact-specific roles" else
      if (!length(prim)) "No primary strategic role at the current thresholds" else
        if (length(prim) == 1) ROLES[[prim]] else
          if (margin >= P$lead_margin) paste0(ROLES[[prim[1]]], " with ", role_list(prim[-1]), " role", plural(length(prim) - 1)) else
            paste0("Combines ", role_list(prim), " roles")
    fo <- first_on(iso, c(prim, sec, beyond))
    ct <- key_positions(iso)
    fc <- fn_focus(iso)
    nd <- if (is_allied) needs(iso) else item()
    cp <- top_compacts(iso)
    pt <- if (is_allied) partners(iso) else item()
    ca <- if (is_allied) capacity_nodes(iso) else item()
    tibble::tibble(
      iso3 = iso, country = cname(iso), allied = is_allied,
      primary_roles = paste(prim, collapse = " "), secondary_roles = paste(sec, collapse = " "), beyond_cap_roles = paste(beyond, collapse = " "),
      slot_headline = headline,
      slot_secondary = if (length(sec)) paste0("secondary ", role_list(sec), " role", plural(length(sec))) else "",
      slot_first_on = fo$text, slot_contributes = ct$text, slot_focus = fc$text, slot_dependency = if (is_allied) dep_profile(iso) else "",
      slot_needs = nd$text, slot_compacts = cp$text, slot_partners = pt$text, slot_capacity = ca$text,
      items_first_on = fo$ids, items_contributes = ct$ids, items_contributes_share = ct$vals, items_focus = fc$ids,
      items_needs = nd$ids, items_needs_share = nd$vals, items_compacts = cp$ids, items_partners = pt$ids,
      items_capacity = ca$ids, items_capacity_usd_bn = ca$vals, capacity_usd_bn = ca$total)
  })
  narr <- dplyr::bind_rows(out)
  narr$narrative <- with(narr, paste0(
    country, ifelse(allied, "", " (non-allied supplier, assessed on the supply roles only)"), ": ",
    slot_headline, ifelse(nzchar(slot_secondary), paste0("; ", slot_secondary), ""), ".",
    ifelse(nzchar(slot_first_on), paste0(" First among allied economies in ", slot_first_on, "."), ""),
    ifelse(nzchar(slot_contributes), paste0(" Contributes: ", slot_contributes, ifelse(nzchar(slot_focus), paste0("; strongest in ", slot_focus, " nodes"), ""), "."),
           ifelse(nzchar(slot_focus), paste0(" Strongest in ", slot_focus, " nodes."), "")),
    ifelse(nzchar(slot_needs), paste0(" Needs: ", slot_needs, ifelse(nzchar(slot_dependency), paste0("; ", slot_dependency), ""), "."),
           ifelse(nzchar(slot_dependency), paste0(" ", aib_st_cap(slot_dependency), "."), "")),
    ifelse(nzchar(slot_compacts), paste0(" Main compacts: ", slot_compacts, "."), ""),
    ifelse(nzchar(slot_partners), paste0(" Highest-value partners (US$ restored by new and expanded trade, Managed Interdependence and Decoupling): ", slot_partners, "."), ""),
    ifelse(nzchar(slot_capacity), paste0(" New capacity matters most in ", slot_capacity, "."), "")))
  narr
}

# ---- Bilateral relationship thesis (task section 19) -------------------------------------------------------

# For a country pair, from the compact roles and the Managed Interdependence / Decoupling networks. No score:
#   priority compacts  compacts where both have a compact role, complementary ones first (one side is the
#                      demand anchor where the other supplies), then those where their lead roles differ, then
#                      by the smaller of the two relevances; a compact where both are demand anchors is left
#                      out (two buyers are not a complementarity; 2026-10-21 review); at most 3
#   complementarity    each side's lead role in those compacts
#   logic              every label whose evidence holds, in a fixed order; the four compact-role labels are
#                      read on the priority compacts only (2026-10-21: read on every shared compact, large
#                      economies sharing up to nine compacts got nearly every label and the theses did not
#                      tell pairs apart):
#     trade                 a trade relationship between them in either network
#     investment            an investment relationship between them in either network
#     technology            in a priority compact one side is a specialized technology supplier and the other is not
#     development / capacity building  in a priority compact one side is a capacity-building host where the other
#                           is an investment anchor or holds the Capital & Investment Anchor role
#     market access         one side is the demand anchor of a priority compact where the other supplies
#     resilience / second sourcing  one side is a diversification or bridge partner in a priority compact where
#                           the other is a demand anchor
aib_relationship_theses <- function(pairs, compact_roles, edges, country_roles = NULL, sc_names = NULL, country_name = NULL) {
  cname <- function(x) if (is.null(country_name)) x else ifelse(is.na(country_name[x]), x, country_name[x])
  scn <- function(id) if (is.null(sc_names)) id else ifelse(is.na(sc_names[id]), id, sc_names[id])
  supply_roles <- c("Resource supplier", "Processing anchor", "Specialized technology supplier", "Scale manufacturer")
  cia <- if (is.null(country_roles)) character() else unique(country_roles$iso3[country_roles$role_code == "CIA" & country_roles$primary_or_secondary %in% c("primary", "secondary")])
  e <- edges
  ew <- if (is.null(e) || !nrow(e)) NULL else {
    key <- ifelse(e$supplier_iso < e$buyer_iso, paste(e$supplier_iso, e$buyer_iso), paste(e$buyer_iso, e$supplier_iso))
    tibble::tibble(key = key, trade = dplyr::coalesce(e$cumulative_trade_resilience_contribution, 0),
                   invest = dplyr::coalesce(e$cumulative_investment_resilience_contribution, 0)) |>
      dplyr::group_by(key) |> dplyr::summarise(trade = sum(trade), invest = sum(invest), .groups = "drop")
  }
  role_of <- function(r) sub(" \\(non-allied supplier\\)$", "", r)
  dplyr::bind_rows(lapply(seq_len(nrow(pairs)), function(i) {
    a <- pairs$a[i]
    b <- pairs$b[i]
    ra <- compact_roles[compact_roles$iso3 == a, , drop = FALSE]
    rb <- compact_roles[compact_roles$iso3 == b, , drop = FALSE]
    shared <- intersect(ra$sc_id, rb$sc_id)
    fa <- function(s, r) if (s %in% r$sc_id) r$compact_functions[r$sc_id == s] else ""
    la <- function(s, r) if (s %in% r$sc_id) role_of(r$compact_role[r$sc_id == s]) else NA_character_
    rel <- vapply(shared, function(s) min(ra$compact_relevance[ra$sc_id == s], rb$compact_relevance[rb$sc_id == s]), 0)
    lra <- vapply(shared, la, "", r = ra)
    lrb <- vapply(shared, la, "", r = rb)
    both_demand <- lra %in% "Demand anchor" & lrb %in% "Demand anchor"
    compl <- (lra %in% "Demand anchor" & lrb %in% supply_roles) | (lrb %in% "Demand anchor" & lra %in% supply_roles)
    differ <- lra != lrb
    ok <- !both_demand
    pri <- utils::head(shared[ok][order(-compl[ok], -differ[ok], -rel[ok], shared[ok])], 3)
    key <- if (a < b) paste(a, b) else paste(b, a)
    w <- if (is.null(ew)) NULL else ew[ew$key == key, , drop = FALSE]
    has <- function(s, r, f) grepl(f, fa(s, r), fixed = TRUE)
    logic <- c(
      trade = !is.null(w) && nrow(w) && w$trade > 0,
      investment = !is.null(w) && nrow(w) && w$invest > 0,
      technology = any(vapply(pri, function(s) xor(identical(la(s, ra), "Specialized technology supplier"), identical(la(s, rb), "Specialized technology supplier")), logical(1))),
      `development / capacity building` = any(vapply(pri, function(s) (has(s, ra, "Capacity-building host") && (has(s, rb, "Investment anchor") || b %in% cia)) ||
                                                        (has(s, rb, "Capacity-building host") && (has(s, ra, "Investment anchor") || a %in% cia)), logical(1))),
      `market access` = any(vapply(pri, function(s) (identical(la(s, ra), "Demand anchor") && la(s, rb) %in% supply_roles) ||
                                     (identical(la(s, rb), "Demand anchor") && la(s, ra) %in% supply_roles), logical(1))),
      `resilience / second sourcing` = any(vapply(pri, function(s) (identical(la(s, ra), "Demand anchor") && (has(s, rb, "Diversification partner") || has(s, rb, "Bridge / coordination partner"))) ||
                                                    (identical(la(s, rb), "Demand anchor") && (has(s, ra, "Diversification partner") || has(s, ra, "Bridge / coordination partner"))), logical(1))))
    comp <- if (length(pri)) paste(vapply(pri, function(s) sprintf("%s: %s %s, %s %s", scn(s), cname(a), tolower(la(s, ra)), cname(b), tolower(la(s, rb))), ""), collapse = "; ") else ""
    lg <- names(logic)[logic]
    tibble::tibble(a = a, b = b, priority_compacts = paste(pri, collapse = " "), shared_compacts = length(shared),
                   complementarity = comp, logic = paste(lg, collapse = "; "),
                   thesis = if (!length(pri) && !length(lg)) sprintf("%s and %s share no strategic compact and no modelled relationship.", cname(a), cname(b)) else
                     paste0(sprintf("%s - %s", cname(a), cname(b)),
                            if (length(lg)) paste0(": ", aib_st_article(lg[1]), " ", aib_st_and(lg), " relationship") else "",
                            if (length(pri)) paste0(". Priority compacts: ", comp) else "", "."))
  }))
}

# The relationships the theses are written for: every pair of allied economies with a modelled relationship
# (a trade or investment edge) in either institutional network (Decoupling: res$edges; Managed
# Interdependence: res$edges_managed), headline supply scenario. Returns list(pairs = tibble(a, b) with
# a < b, edges = those edges).
aib_strategic_thesis_pairs <- function(res, allied) {
  edges <- dplyr::bind_rows(res$edges, res$edges_managed)
  if (!nrow(edges)) return(list(pairs = tibble::tibble(a = character(), b = character()), edges = edges))
  if ("scenario" %in% names(edges)) edges <- edges[edges$scenario == res$meta$headline_scenario, , drop = FALSE]
  edges <- edges[edges$supplier_iso %in% allied & edges$buyer_iso %in% allied, , drop = FALSE]
  pairs <- unique(tibble::tibble(a = pmin(edges$supplier_iso, edges$buyer_iso), b = pmax(edges$supplier_iso, edges$buyer_iso)))
  pairs <- pairs[pairs$a != pairs$b, , drop = FALSE]
  list(pairs = pairs[order(pairs$a, pairs$b), , drop = FALSE], edges = edges)
}

# US$ that new and expanded trade restores from one allied economy to another (supplier -> buyer), summed
# over the nodes and the given shocks (the two institutional scenarios), headline scenario, semantics and
# constraint. Investment edges are left out: their hosts mostly tie at the full increment (aib_strategic_rank1()).
# node_demand (node -> allied demand) prices delta_share_of_demand when a run has no delta_usd.
aib_strategic_pair_usd <- function(res, shocks, allied, node_demand = NULL) {
  pt <- res$partnerships
  empty <- tibble::tibble(supplier_iso = character(), buyer_iso = character(), usd = numeric())
  if (is.null(pt) || !nrow(pt) || !all(c("exporter_iso", "importer_iso") %in% names(pt))) return(empty)
  pt <- pt[pt$relationship != "domestic retention" & pt$shock_id %in% stats::na.omit(shocks), , drop = FALSE]
  if ("scenario" %in% names(pt) && !is.null(res$meta$headline_scenario)) pt <- pt[pt$scenario == res$meta$headline_scenario, , drop = FALSE]
  if ("shock_semantics" %in% names(pt) && !is.null(res$meta$headline_semantics)) pt <- pt[pt$shock_semantics == res$meta$headline_semantics, , drop = FALSE]
  if ("supply_constraint" %in% names(pt) && !is.null(res$meta$headline_constraint)) pt <- pt[pt$supply_constraint == res$meta$headline_constraint, , drop = FALSE]
  if (!"delta_usd" %in% names(pt)) pt$delta_usd <- pt$delta_share_of_demand * dplyr::coalesce(as.numeric(node_demand[pt$node_id]), 0)
  pt <- pt[pt$exporter_iso %in% allied & pt$importer_iso %in% allied & pt$exporter_iso != pt$importer_iso, , drop = FALSE]
  if (!nrow(pt)) return(empty)
  pt |> dplyr::group_by(supplier_iso = exporter_iso, buyer_iso = importer_iso) |>
    dplyr::summarise(usd = sum(pmax(delta_usd, 0), na.rm = TRUE), .groups = "drop")
}

# Does the headline (Decoupling) shock bind on each compact? (2026-10-21 review.) The compact's leading
# non-allied supplier is the non-allied economy with the largest US$ supply equivalent over its nodes (re-
# export hubs counted with nobody). When that is not a target of the headline shock, the Decoupling shock is
# not the compact's binding stress, and the note gives the classes of its nodes under the largest-supplier
# shock (`alt_shock`, class_<alt_shock> of the classification) beside it. Returns one row per compact.
aib_strategic_stress_lens <- function(cn, res, allied, compact_ids, alt_shock = "largest_subsector", country_name = NULL) {
  cname <- function(x) if (is.null(country_name)) x else ifelse(is.na(country_name[x]), x, country_name[x])
  shock <- res$meta$headline_shock
  shocks_cfg <- aib_st_or(res$meta$config$shocks, list())
  sdef <- Filter(function(x) identical(x$id, shock), shocks_cfg)
  targets <- if (length(sdef)) as.character(unlist(sdef[[1]]$countries)) else character()
  label_of <- function(id) { d <- Filter(function(x) identical(x$id, id), shocks_cfg); if (length(d)) aib_st_or(d[[1]]$label, id) else id }
  alt_col <- paste0("class_", alt_shock)
  hub <- if ("reexport_hub" %in% names(cn)) cn$reexport_hub %in% TRUE else rep(FALSE, nrow(cn))
  x <- cn[!cn$iso3 %in% allied & !hub, , drop = FALSE]
  dplyr::bind_rows(lapply(compact_ids, function(sc) {
    y <- x[x$sc_id == sc, , drop = FALSE]
    by <- if (nrow(y)) tapply(y$supply_usd_eq, y$iso3, sum) else numeric()
    if (length(by)) by <- by[order(-by, names(by))]
    top <- if (length(by)) names(by)[1] else NA_character_
    t_usd <- if (length(targets)) sum(by[names(by) %in% targets]) else NA_real_
    binds <- if (!length(targets) || is.na(top)) NA else top %in% targets
    nodes_sc <- unique(cn$node_id[cn$sc_id == sc])
    cls <- if (alt_col %in% names(res$classification)) res$classification[[alt_col]][match(nodes_sc, res$classification$node_id)] else character()
    cls <- cls[!is.na(cls)]
    alt_txt <- if (length(cls)) {
      t <- table(factor(cls, levels = c("BUILD", "COORDINATE", "MANAGE SPECIALIZATION", "LOW INTERVENTION", "INSUFFICIENT DATA")))
      t <- t[t > 0]
      paste(sprintf("%d %s", as.integer(t), names(t)), collapse = ", ")
    } else NA_character_
    note <- if (isFALSE(binds)) {
      sprintf("The Decoupling shock (%s) does not bind hardest on this compact: its leading non-allied supplier is %s (US$%.1fbn of world supply in its nodes, against US$%.1fbn for %s).%s",
              label_of(shock), cname(top), by[[top]] / 1e9, t_usd / 1e9, paste(cname(targets), collapse = " and "),
              if (is.na(alt_txt)) "" else sprintf(" Under the alternative stress test (%s) its nodes are %s.", label_of(alt_shock), alt_txt))
    } else NA_character_
    tibble::tibble(strategic_compact_id = sc, decoupling_binds = binds, leading_non_allied = top,
                   leading_non_allied_usd = if (is.na(top)) NA_real_ else unname(by[[top]]), headline_target_usd = t_usd,
                   stress_lens_shock = alt_shock, stress_lens_classes = alt_txt, stress_note = note)
  }))
}

# ---- The whole layer ------------------------------------------------------------------------------------------

# Everything scripts/33 writes, from the AIB inputs and results, the three configs and the capital inputs.
#   compacts_yml  config/aib_strategic_compacts.yml (parsed); node_fn_yml config/aib_node_functions.yml;
#   layer_cfg     config/aib_strategic_layer.yml; capital list(dip = DIP rows or NULL, cmfdi = CM-FDI
#   projects or NULL); conduits ISO3 codes of the conduit centres.
# Returns a list of tables plus `problems` (scripts/33 stops on any) and `notes`.
aib_strategic_layer <- function(inputs, res, compacts_yml, node_fn_yml, layer_cfg, capital = list(), conduits = character(),
                                iso_of = NULL) {
  P <- aib_strategic_params(layer_cfg$roles)
  cnames <- stats::setNames(res$country_names$country, res$country_names$iso3)
  q <- inputs$quality
  agg <- if (!is.null(q) && "aggregated_into" %in% names(q)) q[!is.na(q$aggregated_into), c("node_id", "aggregated_into")] else NULL
  mw <- if (!is.null(q) && "world_trade" %in% names(q)) stats::setNames(as.numeric(q$world_trade), q$node_id) else NULL
  v <- aib_strategic_validate_map(compacts_yml, inputs$nodes$node_id, agg, mw)
  if (length(v$problems)) return(list(problems = v$problems, notes = v$notes))
  map_run <- v$map
  stage <- stats::setNames(inputs$nodes$supply_chain, inputs$nodes$node_id)
  fn <- aib_strategic_node_functions(node_fn_yml, map_run, stage)
  if (length(fn$problems)) return(list(problems = fn$problems, notes = c(v$notes, fn$notes)))
  map_run <- dplyr::left_join(map_run, fn$functions, by = "node_id")
  defs <- aib_strategic_compact_defs(compacts_yml)
  # a configured compact with no node in this run is noted by the validation and left out of every output
  defs$in_run <- defs$strategic_compact_id %in% map_run$strategic_compact_id
  node_demand <- stats::setNames(as.numeric(res$classification$demand_usd), res$classification$node_id)
  w <- aib_strategic_workstreams(res$compact_nodes, map_run, node_demand, compacts_yml$workstream_overrides, defs$strategic_compact_id)
  ws <- w$workstreams
  scen <- aib_strategic_scenario_defs(res, layer_cfg$scenarios)
  cls_thr <- res$meta$config$classification
  status <- aib_strategic_node_status(res, map_run$node_id, scen, cls_thr, untested = layer_cfg$scenarios$managed_untested)
  # two sets: the model's demand scope (coalition tiers; whose demand the stress tests protect) and the
  # strategic layer's allied set = the scope plus `allied: treat_as_allied` (config/aib_strategic_layer.yml):
  # economies outside the coalition tiers that the institutional slots treat as allies (role eligibility,
  # coalition members, hosts, critical partners, core countries); their own demand stays outside the scope
  scope <- inputs$coalition$iso3[inputs$coalition$tier != "none"]
  extra_allied <- setdiff(as.character(unlist(layer_cfg$allied$treat_as_allied)), scope)
  allied <- union(scope, extra_allied)
  hubs <- as.character(unlist(res$meta$config$trade$reexport_hubs))
  # roles: node evidence, workstream evidence, capital, metrics, assignment
  nodes <- aib_strategic_node_frame(inputs, map_run, fn$functions)
  div <- if (any(scen$scenario == "managed")) scen$shock_id[scen$scenario == "managed"] else NA_character_
  cn <- aib_country_node_evidence(inputs, res, nodes, scope, P, div_shock = div, status = status, hubs = hubs)
  mem <- aib_strategic_ws_members(res, ws)
  wsc <- aib_strategic_ws_country(mem)
  cmf_p <- aib_capital_cmfdi_projects(capital$cmfdi)
  cap <- list(dip = if (is.null(capital$dip)) NULL else aib_capital_dip_outward(capital$dip, conduits, layer_cfg$capital, iso_of),
              cmfdi = if (is.null(cmf_p)) NULL else aib_capital_cmfdi_outward(cmf_p))
  cty <- aib_country_role_metrics(cn, nodes, wsc, cap, allied, P)
  roles <- aib_assign_strategic_roles(cty, P)
  sm <- layer_cfg$capital$cmfdi_sectors
  cr <- aib_compact_country_roles(cn, nodes, mem, cmf_p, res$partnerships, allied, P, sector_map = sm,
                                  shocks = c(managed = div, decoupling = res$meta$headline_shock), scenario = res$meta$headline_scenario,
                                  node_demand = node_demand, semantics = res$meta$headline_semantics, constraint = res$meta$headline_constraint)
  core <- aib_strategic_core(cr$compact_roles, country_name = cnames)
  # a structural core country of the compact is never "outside the core coalition" (the portfolio's
  # allied_outside_core follows the same rule)
  core_l <- aib_strategic_core_long(cr$compact_roles)
  core_key <- paste(core_l$iso3[core_l$allied], core_l$strategic_compact_id[core_l$allied])
  oc <- cr$compact_roles$criticality %in% "allied supplier outside the core coalition" & paste(cr$compact_roles$iso3, cr$compact_roles$sc_id) %in% core_key
  cr$compact_roles$criticality[oc] <- ""
  members <- list(decoupling = res$compact_members, managed = res$compact_members_managed)
  presence <- cn[cn$position_share >= P$presence_min & !(cn$reexport_hub %in% TRUE), , drop = FALSE] |>
    dplyr::distinct(strategic_compact_id = sc_id, iso3)
  node_labels <- stats::setNames(res$classification$sub_sector, res$classification$node_id)
  pf <- aib_strategic_portfolio(status, map_run, defs[defs$in_run, , drop = FALSE], scen, ws, members, layer_cfg$missions, layer_cfg$policy_menu, core, cnames,
                                allied = allied, node_labels = node_labels, presence = presence)
  lens <- aib_strategic_stress_lens(cn, res, allied, defs$strategic_compact_id[defs$in_run], country_name = cnames)
  pf$compacts <- dplyr::left_join(pf$compacts, lens, by = "strategic_compact_id") |>
    dplyr::mutate(stress_note = ifelse(scenario %in% c("partial", "decoupling"), stress_note, NA_character_))
  # workstreams: scenario activity and the compact's name
  act <- function(s) vapply(strsplit(ws$nodes, " ", fixed = TRUE), function(ids) any(status$node_status[status$scenario == s & status$node_id %in% ids] %in% aib_strategic_intervention()), logical(1))
  for (s in scen$scenario) ws[[paste0("active_", s)]] <- act(s)
  ws$strategic_compact_name <- defs$strategic_compact_name[match(ws$strategic_compact_id, defs$strategic_compact_id)]
  cp <- res$compacts
  ws$workstream_name <- cp$compact_name[match(ws$workstream_id, cp$compact_id)]
  ws$decoupling_type <- cp$compact_type[match(ws$workstream_id, cp$compact_id)]
  ws$decoupling_archetype <- cp$compact_archetype[match(ws$workstream_id, cp$compact_id)]
  ws$decoupling_class_mix <- cp$class_mix[match(ws$workstream_id, cp$compact_id)]
  ws$decoupling_coalition <- cp$minimum_coalition[match(ws$workstream_id, cp$compact_id)]
  if (!is.null(res$compacts_managed)) {
    cm <- res$compacts_managed
    ws$managed_nodes_run <- cm$n_nodes_run[match(ws$workstream_id, cm$compact_id)]
    ws$managed_coalition <- cm$minimum_coalition[match(ws$workstream_id, cm$compact_id)]
  }
  ws$value_chain_steps <- vapply(strsplit(ws$nodes, " ", fixed = TRUE), function(ids) {
    m <- map_run[map_run$node_id %in% ids, , drop = FALSE]
    m <- m[order(m$compact_order, m$value_chain_order), , drop = FALSE]
    paste(unique(sprintf("%s: %s", m$strategic_compact_id, m$value_chain_step)), collapse = "; ")
  }, "")
  ws <- ws[order(match(ws$strategic_compact_id, defs$strategic_compact_id), ws$workstream_id), , drop = FALSE]
  pair_usd <- aib_strategic_pair_usd(res, c(div, res$meta$headline_shock), allied, node_demand)
  narr <- aib_country_role_narratives(roles, cty, cn, nodes, cr$compact_roles, pair_usd, allied, P, cnames)
  # spine (compact x step x node x scenario)
  ws_of <- res$compact_nodes[!is.na(res$compact_nodes$compact_id), c("node_id", "compact_id")]
  spine <- status |> dplyr::left_join(dplyr::select(map_run, node_id, strategic_compact_id, strategic_compact_name, compact_order, value_chain_order,
                                                    value_chain_step, step_description, scope, node_function), by = "node_id") |>
    dplyr::mutate(node_label = res$classification$sub_sector[match(node_id, res$classification$node_id)],
                  workstream_id = ws_of$compact_id[match(node_id, ws_of$node_id)],
                  workstream_compact = ws$strategic_compact_id[match(workstream_id, ws$workstream_id)]) |>
    dplyr::arrange(compact_order, value_chain_order, match(node_id, map_run$node_id), match(scenario, aib_strategic_scenario_ids())) |>
    dplyr::relocate(strategic_compact_id, strategic_compact_name, value_chain_order, value_chain_step, step_description, node_id, node_label,
                    node_function, scope, scenario, node_status, status_detail, policy_needs)
  # division of labour, node level (material cells only)
  dol <- cn[cn$position_share >= 0.01 | cn$scope_demand_share >= 0.01 | !is.na(cn$best_host_rank), , drop = FALSE] |>
    dplyr::transmute(strategic_compact_id = sc_id, iso3, allied = iso3 %in% allied, node_id, value_chain_order = vc_order, value_chain_step = vc_step,
                     node_function = step_function, position_share, world_share, production_share, export_rca, scope_demand_share, world_import_share,
                     net_import_share, best_host_rank, host_scaled, capable_host, host_gap_usd, host_tie_n, momentum, anchor_position, specialist_position,
                     reexport_hub) |>
    dplyr::arrange(match(strategic_compact_id, defs$strategic_compact_id), value_chain_order, node_id, dplyr::desc(position_share), iso3)
  cr_out <- cr$compact_roles |> dplyr::rename(strategic_compact_id = sc_id, resilience_contribution_managed = resilience_managed,
                                              resilience_contribution_decoupling = resilience_decoupling) |>
    dplyr::mutate(country = ifelse(is.na(cnames[iso3]), iso3, cnames[iso3]), resilience_contribution = resilience_contribution_managed,
                  strategic_compact_name = defs$strategic_compact_name[match(strategic_compact_id, defs$strategic_compact_id)]) |>
    dplyr::arrange(match(strategic_compact_id, defs$strategic_compact_id), dplyr::desc(compact_relevance), iso3) |>
    dplyr::relocate(strategic_compact_id, strategic_compact_name, iso3, country, allied, compact_role, compact_role_detail, compact_functions,
                    lead_function, lead_node, lead_step, lead_share, lead_share_basis, key_nodes, supplier_contribution, demand_contribution,
                    investment_contribution, resilience_contribution, resilience_contribution_managed, resilience_contribution_decoupling,
                    resilience_usd_managed, resilience_usd_decoupling, criticality, compact_relevance, mean_step_supply, mean_step_demand)
  roles_out <- roles |> dplyr::mutate(country = ifelse(is.na(cnames[iso3]), iso3, cnames[iso3]),
                                      primary_or_secondary = dplyr::coalesce(primary_or_secondary, "not assigned")) |>
    dplyr::arrange(iso3, is.na(role_rank), role_rank, match(role_code, names(aib_strategic_roles())))
  # bilateral relationship theses: every allied pair with a modelled relationship in either network
  tp <- aib_strategic_thesis_pairs(res, allied)
  theses <- aib_relationship_theses(tp$pairs, cr$compact_roles, tp$edges, roles_out,
                                    stats::setNames(defs$strategic_compact_name, defs$strategic_compact_id), cnames)
  list(
    params = P, scenarios = scen, compact_defs = defs, map = map_run, unmapped = v$unmapped, workstreams = ws, stale_overrides = w$stale,
    allied = allied, demand_scope = scope, treat_as_allied = extra_allied, stress_lens = lens, pair_usd = pair_usd,
    node_status = status, spine = spine, compacts = pf$compacts, policy_menus = pf$menus, core = core,
    core_long = aib_strategic_core_long(cr$compact_roles),
    metrics = cty, roles = roles_out, compact_roles = cr_out, steps = cr$steps, division = dol, narratives = narr, theses = theses,
    node_frame = nodes, evidence = cn, problems = character(), notes = c(v$notes, fn$notes,
      if (nrow(w$stale)) sprintf("stale workstream override %s (%s): the demand rule applies", w$stale$workstream_id, w$stale$nodes),
      if (length(extra_allied)) sprintf("treated as allied by the strategic layer (allied: treat_as_allied), outside the model's demand scope: %s", paste(extra_allied, collapse = ", ")),
      if (is.null(capital$dip)) "no IMF DIP input: outward direct investment not assessed (Capital & Investment Anchor not assessed)",
      if (is.null(capital$cmfdi)) "no CM-FDI input: outward cleantech-manufacturing FDI not assessed (Capital & Investment Anchor and the investment function not assessed)"))
}
