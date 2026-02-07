# --- FILE: R/indices/allied_network_design.R ---------------------------------
# Allied network design: choose a coalition-wide division of labor (specialization)
# and implied trade/relationship flows for each (tech x supply_chain) stage.

# Dependencies expected elsewhere in repo: dplyr, tidyr, tibble, purrr, stringr, rlang
# Optional solver deps for MILP: ompr, ompr.roi, ROI, ROI.plugin.glpk

if (!exists("median_scurve", mode = "function")) {
  source(file.path(dirname(sys.frame(1)$ofile), "..", "utils", "scurve.R"))
}
if (!exists("require_columns", mode = "function")) {
  source(file.path(dirname(sys.frame(1)$ofile), "..", "utils", "schema.R"))
}
if (!exists("partnership_strength_country_to_iso", mode = "function")) {
  source(file.path(
    dirname(sys.frame(1)$ofile),
    "..", "themes", "partnership_strength", "partnership_strength_helpers.R"
  ))
}

# ---- Defaults ---------------------------------------------------------------

allied_network_default_iso3c <- function() {
  c(
    "USA","CAN","JPN","AUS","IND","MEX","KOR","GBR","DEU","FRA","ITA","BRA","SAU",
    "ZAF","IDN","NOR","UAE","VNM","KEN","DNK","ARG","MAR","CHL"
  )
}

allied_network_normalize_weights <- function(w) {
  nm <- names(w)
  w <- suppressWarnings(as.numeric(w))
  if (!is.null(nm)) names(w) <- nm
  if (length(w) == 0 || all(is.na(w)) || sum(w, na.rm = TRUE) <= 0) {
    out <- rep(NA_real_, length(w))
    if (!is.null(nm)) names(out) <- nm
    return(out)
  }
  w / sum(w, na.rm = TRUE)
}

allied_network_hhi <- function(shares) {
  shares <- suppressWarnings(as.numeric(shares))
  shares <- shares[is.finite(shares)]
  if (!length(shares)) return(NA_real_)
  sum(shares^2)
}

# ---- Extract edges from partner dyad theme tables ----------------------------

# Expects the standardized dyad tables written by scripts/15_build_partner_themes.R:
# - partner_friendshore_tbl (category == "friendshore", variable == "Friendshore Index")
# - partner_opportunity_tbl (category == "opportunity", variable == "Opportunity Index")
allied_network_extract_edges <- function(partner_friendshore_tbl,
                                         partner_opportunity_tbl,
                                         iso3c_network = allied_network_default_iso3c(),
                                         edge_weights = list(friendshore = 0.5, opportunity = 0.5),
                                         keep_components = TRUE) {
  require_columns(
    partner_friendshore_tbl,
    c("reporter_iso", "partner_iso", "tech", "supply_chain", "category", "variable", "data_type", "value"),
    label = "partner_friendshore_tbl"
  )
  require_columns(
    partner_opportunity_tbl,
    c("reporter_iso", "partner_iso", "tech", "supply_chain", "category", "variable", "data_type", "value"),
    label = "partner_opportunity_tbl"
  )
  
  w <- c(friendshore = edge_weights$friendshore, opportunity = edge_weights$opportunity)
  w <- allied_network_normalize_weights(w)
  if (any(is.na(w))) {
    stop("edge_weights must be positive; got: ", paste(names(edge_weights), unlist(edge_weights), collapse = ", "))
  }
  
  fs <- partner_friendshore_tbl %>%
    dplyr::filter(
      category == "friendshore",
      variable == "Friendshore Index",
      data_type == "index"
    ) %>%
    dplyr::transmute(
      reporter_iso = as.character(reporter_iso),
      partner_iso  = as.character(partner_iso),
      tech         = as.character(tech),
      supply_chain = as.character(supply_chain),
      friendshore_index = suppressWarnings(as.numeric(value))
    )
  
  op <- partner_opportunity_tbl %>%
    dplyr::filter(
      category == "opportunity",
      variable == "Opportunity Index",
      data_type == "index"
    ) %>%
    dplyr::transmute(
      reporter_iso = as.character(reporter_iso),
      partner_iso  = as.character(partner_iso),
      tech         = as.character(tech),
      supply_chain = as.character(supply_chain),
      opportunity_index = suppressWarnings(as.numeric(value))
    )
  
  edges <- dplyr::full_join(fs, op, by = c("reporter_iso", "partner_iso", "tech", "supply_chain")) %>%
    dplyr::mutate(
      friendshore_index  = dplyr::coalesce(friendshore_index, 0),
      opportunity_index  = dplyr::coalesce(opportunity_index, 0),
      edge_weight = w[["friendshore"]] * friendshore_index + w[["opportunity"]] * opportunity_index
    ) %>%
    dplyr::filter(
      reporter_iso %in% iso3c_network,
      partner_iso %in% iso3c_network
    )
  
  if (!isTRUE(keep_components)) {
    edges <- edges %>%
      dplyr::select(reporter_iso, partner_iso, tech, supply_chain, edge_weight)
  }
  
  edges
}

allied_network_complete_edges_stage <- function(edges_stage,
                                                iso3c_vec,
                                                allow_self = TRUE,
                                                fill_missing = 0,
                                                self_weight = NULL) {
  require_columns(edges_stage, c("reporter_iso", "partner_iso", "edge_weight"), label = "edges_stage")
  
  base <- tidyr::crossing(
    reporter_iso = as.character(iso3c_vec),
    partner_iso  = as.character(iso3c_vec)
  )
  
  out <- base %>%
    dplyr::left_join(
      edges_stage %>% dplyr::select(reporter_iso, partner_iso, dplyr::any_of(c("edge_weight", "friendshore_index", "opportunity_index"))),
      by = c("reporter_iso", "partner_iso")
    )
  
  if (!isTRUE(allow_self)) {
    out <- out %>% dplyr::filter(reporter_iso != partner_iso)
  }
  
  if (is.null(self_weight)) {
    med <- suppressWarnings(stats::median(out$edge_weight, na.rm = TRUE))
    if (!is.finite(med)) med <- 0.5
    self_weight <- med
  }
  
  out <- out %>%
    dplyr::mutate(
      edge_weight = dplyr::case_when(
        !is.na(edge_weight) ~ edge_weight,
        reporter_iso == partner_iso ~ self_weight,
        TRUE ~ fill_missing
      )
    )
  
  out
}

# ---- Prepare coalition nodes (capability + demand weights) -------------------

# Inputs:
# - economic_opportunity_index: must contain Country, tech, supply_chain, Economic_Opportunity_Index
# - energy_security_index: must contain Country, tech, supply_chain, Energy_Security_Index
# - policy_index: must contain Country, tech, supply_chain, value  (your policy_index table)
# - partner_development_country_tbl (optional): standardized schema table with Development Potential Index
# - country_info: must contain iso3c, country (WDI country info standardized)
#
# Output: tibble with iso3c, country, tech, supply_chain, producer_score, demand_weight, dev_potential, etc.
allied_network_prepare_nodes <- function(economic_opportunity_index,
                                         energy_security_index,
                                         policy_index,
                                         country_info,
                                         iso3c_network = allied_network_default_iso3c(),
                                         partner_development_country_tbl = NULL,
                                         node_weights = list(eo = 0.5, policy = 0.3, resilience = 0.2),
                                         demand_from = c("need", "equal")) {
  demand_from <- match.arg(demand_from)
  
  require_columns(
    economic_opportunity_index,
    c("Country", "tech", "supply_chain", "Economic_Opportunity_Index"),
    label = "economic_opportunity_index"
  )
  require_columns(
    energy_security_index,
    c("Country", "tech", "supply_chain", "Energy_Security_Index"),
    label = "energy_security_index"
  )
  require_columns(
    policy_index,
    c("Country", "tech", "supply_chain", "value"),
    label = "policy_index"
  )
  require_columns(country_info, c("iso3c", "country"), label = "country_info")
  
  w_node <- c(
    eo = node_weights$eo,
    policy = node_weights$policy,
    resilience = node_weights$resilience
  )
  w_node <- allied_network_normalize_weights(w_node)
  if (any(is.na(w_node))) stop("node_weights must be positive (eo/policy/resilience).")
  
  # Map Country -> iso3c using your established helper
  eo <- economic_opportunity_index %>%
    dplyr::transmute(
      Country = partnership_strength_standardize_countries(Country),
      iso3c   = partnership_strength_country_to_iso(Country, country_info),
      country = Country,
      tech = as.character(tech),
      supply_chain = as.character(supply_chain),
      eo_raw = suppressWarnings(as.numeric(Economic_Opportunity_Index))
    )
  
  es <- energy_security_index %>%
    dplyr::transmute(
      Country = partnership_strength_standardize_countries(Country),
      iso3c   = partnership_strength_country_to_iso(Country, country_info),
      tech = as.character(tech),
      supply_chain = as.character(supply_chain),
      es_raw = suppressWarnings(as.numeric(Energy_Security_Index))
    )
  
  pol <- policy_index %>%
    dplyr::transmute(
      Country = partnership_strength_standardize_countries(Country),
      iso3c   = partnership_strength_country_to_iso(Country, country_info),
      tech = as.character(tech),
      supply_chain = as.character(supply_chain),
      policy_raw = suppressWarnings(as.numeric(value))
    )
  
  nodes <- eo %>%
    dplyr::left_join(es,  by = c("iso3c", "tech", "supply_chain")) %>%
    dplyr::left_join(pol, by = c("iso3c", "tech", "supply_chain")) %>%
    dplyr::filter(iso3c %in% iso3c_network)
  
  # Development potential (optional; only populated for EMEs in your current build)
  if (!is.null(partner_development_country_tbl)) {
    require_columns(
      partner_development_country_tbl,
      c("Country", "tech", "supply_chain", "category", "variable", "data_type", "value"),
      label = "partner_development_country_tbl"
    )
    dev <- partner_development_country_tbl %>%
      dplyr::filter(
        category == "development",
        variable == "Development Potential Index",
        data_type == "index"
      ) %>%
      dplyr::transmute(
        Country = partnership_strength_standardize_countries(Country),
        iso3c   = partnership_strength_country_to_iso(Country, country_info),
        tech = as.character(tech),
        supply_chain = as.character(supply_chain),
        dev_potential = suppressWarnings(as.numeric(value))
      )
    
    nodes <- nodes %>%
      dplyr::left_join(dev, by = c("iso3c", "tech", "supply_chain"))
  } else {
    nodes$dev_potential <- NA_real_
  }
  
  nodes <- nodes %>%
    dplyr::mutate(
      dev_potential = dplyr::coalesce(dev_potential, 0.5)
    ) %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(
      eo_idx  = median_scurve(eo_raw),
      es_idx  = median_scurve(es_raw),
      pol_idx = median_scurve(policy_raw),
      # "need" = vulnerability proxy (higher means more vulnerable / more demand for reliable supply)
      need_idx = 1 - es_idx,
      producer_score = w_node[["eo"]] * eo_idx + w_node[["policy"]] * pol_idx + w_node[["resilience"]] * es_idx
    ) %>%
    dplyr::ungroup()
  
  nodes <- nodes %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(
      demand_weight = dplyr::case_when(
        demand_from == "equal" ~ 1,
        TRUE ~ dplyr::coalesce(need_idx, 0)
      ),
      demand_weight = if (all(is.na(demand_weight)) || sum(demand_weight, na.rm = TRUE) <= 0) {
        rep(1, dplyr::n())
      } else {
        demand_weight
      },
      demand_weight = demand_weight / sum(demand_weight, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
  
  nodes
}

# ---- Solver: greedy fallback ------------------------------------------------

allied_network_cap_shares <- function(shares, min_share, max_share) {
  shares <- as.numeric(shares)
  n <- length(shares)
  if (n == 0) return(shares)
  
  # Feasibility guards
  min_share <- max(0, min(min_share, 1 / n))
  max_share <- max(min_share, min(max_share, 1))
  
  # Start normalized
  if (sum(shares, na.rm = TRUE) <= 0) shares <- rep(1 / n, n)
  shares <- shares / sum(shares)
  
  # Iteratively clip to max_share, then enforce min_share
  for (iter in 1:50) {
    over <- which(shares > max_share + 1e-10)
    if (!length(over)) break
    excess <- sum(shares[over] - max_share)
    shares[over] <- max_share
    under_idx <- setdiff(seq_len(n), over)
    if (!length(under_idx)) break
    shares[under_idx] <- shares[under_idx] + excess * (shares[under_idx] / sum(shares[under_idx]))
  }
  
  for (iter in 1:50) {
    under <- which(shares < min_share - 1e-10)
    if (!length(under)) break
    deficit <- sum(min_share - shares[under])
    shares[under] <- min_share
    over_idx <- setdiff(seq_len(n), under)
    if (!length(over_idx)) break
    shares[over_idx] <- pmax(0, shares[over_idx] - deficit * (shares[over_idx] / sum(shares[over_idx])))
    if (sum(shares, na.rm = TRUE) <= 0) {
      shares <- rep(1 / n, n)
      break
    }
    shares <- shares / sum(shares)
  }
  
  shares / sum(shares)
}

allied_network_solve_stage_greedy <- function(nodes_stage,
                                              edges_stage,
                                              min_producers = 3,
                                              max_share = 0.40,
                                              min_share = 0.05,
                                              w_edge = 0.5,
                                              w_node = 1.0,
                                              w_dev = 0.0,
                                              allow_self = TRUE) {
  require_columns(nodes_stage, c("iso3c", "producer_score", "demand_weight", "dev_potential"), label = "nodes_stage")
  require_columns(edges_stage, c("reporter_iso", "partner_iso", "edge_weight"), label = "edges_stage")
  
  iso <- nodes_stage$iso3c
  # Ensure feasibility: need enough producers so sum(max_share) can reach 1
  min_required <- ceiling(1 / max_share)
  n_prod <- max(min_producers, min_required)
  n_prod <- min(n_prod, length(iso))
  
  # Complete edges for this stage and (optionally) allow self edges
  edges_full <- allied_network_complete_edges_stage(
    edges_stage,
    iso3c_vec = iso,
    allow_self = TRUE,         # keep self in matrix; we can later drop flows if allow_self=FALSE
    fill_missing = 0,
    self_weight = NULL
  )
  
  # Weighted average outbound connectivity for each candidate producer i
  d_tbl <- nodes_stage %>% dplyr::select(iso3c, demand_weight)
  conn <- edges_full %>%
    dplyr::left_join(d_tbl, by = c("partner_iso" = "iso3c")) %>%
    dplyr::group_by(reporter_iso) %>%
    dplyr::summarize(connectivity_out = sum(edge_weight * demand_weight, na.rm = TRUE), .groups = "drop") %>%
    dplyr::rename(iso3c = reporter_iso)
  
  candidates <- nodes_stage %>%
    dplyr::left_join(conn, by = "iso3c") %>%
    dplyr::mutate(connectivity_out = dplyr::coalesce(connectivity_out, 0))
  
  candidates <- candidates %>%
    dplyr::mutate(
      combined_score = w_node * producer_score + w_edge * connectivity_out + w_dev * dev_potential
    ) %>%
    dplyr::arrange(dplyr::desc(combined_score))
  
  selected_iso <- utils::head(candidates$iso3c, n_prod)
  
  sel <- candidates %>%
    dplyr::mutate(selected = iso3c %in% selected_iso)
  
  # Shares (softmax on combined_score among selected), then cap
  sel_scores <- sel %>% dplyr::filter(selected)
  s <- sel_scores$combined_score
  s <- s - max(s, na.rm = TRUE)
  raw <- exp(s)
  shares <- raw / sum(raw)
  
  # enforce min/max share
  shares <- allied_network_cap_shares(shares, min_share = min_share, max_share = max_share)
  
  prod_tbl <- sel_scores %>%
    dplyr::mutate(production_share = shares) %>%
    dplyr::select(iso3c, production_share, producer_score, connectivity_out, dev_potential)
  
  # Build flows: allocate each consumer demand across selected producers proportional to edge*share
  edges_sel <- edges_full %>%
    dplyr::filter(reporter_iso %in% selected_iso, partner_iso %in% iso)
  
  flows <- edges_sel %>%
    dplyr::left_join(prod_tbl, by = c("reporter_iso" = "iso3c")) %>%
    dplyr::left_join(d_tbl, by = c("partner_iso" = "iso3c")) %>%
    dplyr::mutate(
      weight_raw = edge_weight * production_share,
      weight_raw = dplyr::coalesce(weight_raw, 0)
    ) %>%
    dplyr::group_by(partner_iso) %>%
    dplyr::mutate(
      denom = sum(weight_raw, na.rm = TRUE),
      denom = dplyr::if_else(is.na(denom) | denom <= 0, NA_real_, denom),
      flow_share = dplyr::if_else(
        is.na(denom),
        demand_weight * (production_share / sum(production_share, na.rm = TRUE)),
        demand_weight * (weight_raw / denom)
      )
    ) %>%
    dplyr::ungroup()
  
  if (!isTRUE(allow_self)) {
    flows <- flows %>% dplyr::filter(reporter_iso != partner_iso)
    # Re-normalize within each consumer to preserve demand_weight after removing self
    flows <- flows %>%
      dplyr::group_by(partner_iso) %>%
      dplyr::mutate(flow_share = flow_share / sum(flow_share, na.rm = TRUE) * dplyr::first(demand_weight)) %>%
      dplyr::ungroup()
  }
  
  # Attach non-selected producers with share 0 for completeness
  specialization <- sel %>%
    dplyr::left_join(prod_tbl %>% dplyr::select(iso3c, production_share), by = "iso3c") %>%
    dplyr::mutate(production_share = dplyr::coalesce(production_share, 0)) %>%
    dplyr::select(iso3c, country, producer_score, connectivity_out, dev_potential, demand_weight, selected, production_share)
  
  list(
    specialization = specialization,
    flows = flows %>%
      dplyr::select(reporter_iso, partner_iso, edge_weight, dplyr::any_of(c("friendshore_index", "opportunity_index")), flow_share),
    objective = sum(specialization$production_share * specialization$producer_score, na.rm = TRUE) +
      w_edge * sum(flows$flow_share * flows$edge_weight, na.rm = TRUE),
    hhi = allied_network_hhi(specialization$production_share),
    n_producers = sum(specialization$selected, na.rm = TRUE)
  )
}

# ---- Solver: MILP (optional) ------------------------------------------------

allied_network_has_milp <- function() {
  requireNamespace("ompr", quietly = TRUE) &&
    requireNamespace("ompr.roi", quietly = TRUE) &&
    requireNamespace("ROI", quietly = TRUE) &&
    requireNamespace("ROI.plugin.glpk", quietly = TRUE)
}

allied_network_solve_stage_milp <- function(nodes_stage,
                                            edges_stage,
                                            min_producers = 3,
                                            max_share = 0.40,
                                            min_share = 0.05,
                                            min_suppliers_per_consumer = 2,
                                            epsilon_supplier_share = 0.10,
                                            w_edge = 0.5,
                                            w_node = 1.0,
                                            w_dev = 0.0,
                                            allow_self = TRUE,
                                            solver = c("glpk")) {
  solver <- match.arg(solver)
  if (!allied_network_has_milp()) {
    stop("MILP solver packages not available. Install ompr, ompr.roi, ROI, ROI.plugin.glpk or use method='greedy'.")
  }
  
  require_columns(nodes_stage, c("iso3c", "producer_score", "demand_weight", "dev_potential"), label = "nodes_stage")
  require_columns(edges_stage, c("reporter_iso", "partner_iso", "edge_weight"), label = "edges_stage")
  
  iso <- as.character(nodes_stage$iso3c)
  n <- length(iso)
  if (n == 0) stop("No nodes in stage.")
  
  # Complete edge matrix for this stage
  edges_full <- allied_network_complete_edges_stage(
    edges_stage,
    iso3c_vec = iso,
    allow_self = TRUE,
    fill_missing = 0,
    self_weight = NULL
  )
  
  # Index mapping iso3c -> i
  idx <- stats::setNames(seq_len(n), iso)
  
  # Vectors/matrix (sanitize to avoid NA coefficients in MILP objective/constraints)
  a <- suppressWarnings(as.numeric(nodes_stage$producer_score))
  d <- suppressWarnings(as.numeric(nodes_stage$demand_weight))
  dev <- suppressWarnings(as.numeric(nodes_stage$dev_potential))

  a[!is.finite(a)] <- 0
  dev[!is.finite(dev)] <- 0
  d[!is.finite(d)] <- 0
  if (sum(d, na.rm = TRUE) <= 0) {
    d <- rep(1 / n, n)
  } else {
    d <- d / sum(d, na.rm = TRUE)
  }
  
  W <- matrix(0, nrow = n, ncol = n, dimnames = list(iso, iso))
  for (k in seq_len(nrow(edges_full))) {
    i <- edges_full$reporter_iso[[k]]
    j <- edges_full$partner_iso[[k]]
    if (!is.na(i) && !is.na(j) && i %in% iso && j %in% iso) {
      ew <- suppressWarnings(as.numeric(edges_full$edge_weight[[k]]))
      W[i, j] <- if (is.finite(ew)) ew else 0
    }
  }
  
  # ompr model
  # Variables:
  # f[i,j] flow share from i to j
  # z[i]   producer selected
  # y[i,j] supplier assignment (binary) for redundancy per consumer
  suppressPackageStartupMessages(library(ompr))
  suppressPackageStartupMessages(library(ompr.roi))
  suppressPackageStartupMessages(library(ROI))
  suppressPackageStartupMessages(library(ROI.plugin.glpk))
  
  model <- ompr::MIPModel() %>%
    ompr::add_variable(f[i, j], i = 1:n, j = 1:n, type = "continuous", lb = 0) %>%
    ompr::add_variable(z[i], i = 1:n, type = "binary") %>%
    ompr::add_variable(y[i, j], i = 1:n, j = 1:n, type = "binary") %>%
    # Objective: node capability + edge feasibility + dev "buildability" signal
    ompr::set_objective(
      ompr::sum_expr(f[i, j] * (w_node * a[i] + w_edge * W[iso[i], iso[j]]), i = 1:n, j = 1:n) +
        ompr::sum_expr(w_dev * dev[i] * z[i], i = 1:n),
      "max"
    ) %>%
    # Meet each consumer's demand weight
    ompr::add_constraint(ompr::sum_expr(f[i, j], i = 1:n) == d[j], j = 1:n) %>%
    # Production share caps and selection linkage
    ompr::add_constraint(ompr::sum_expr(f[i, j], j = 1:n) <= max_share * z[i], i = 1:n) %>%
    ompr::add_constraint(ompr::sum_expr(f[i, j], j = 1:n) >= min_share * z[i], i = 1:n) %>%
    ompr::add_constraint(ompr::sum_expr(z[i], i = 1:n) >= min_producers) %>%
    # Supplier assignment constraints (redundancy per consumer)
    ompr::add_constraint(f[i, j] <= d[j] * y[i, j], i = 1:n, j = 1:n) %>%
    ompr::add_constraint(f[i, j] >= epsilon_supplier_share * d[j] * y[i, j], i = 1:n, j = 1:n) %>%
    ompr::add_constraint(y[i, j] <= z[i], i = 1:n, j = 1:n) %>%
    ompr::add_constraint(ompr::sum_expr(y[i, j], i = 1:n) >= min_suppliers_per_consumer, j = 1:n)
  
  if (!isTRUE(allow_self)) {
    # Disallow self-supply by forcing y[i,i]=0 and f[i,i]=0
    model <- model %>%
      ompr::add_constraint(f[i, i] == 0, i = 1:n) %>%
      ompr::add_constraint(y[i, i] == 0, i = 1:n)
  }
  
  result <- ompr::solve_model(model, ompr.roi::with_ROI(solver = "glpk"))
  
  flows <- ompr::get_solution(result, f[i, j]) %>%
    dplyr::mutate(
      reporter_iso = iso[i],
      partner_iso = iso[j],
      flow_share = value
    ) %>%
    dplyr::select(reporter_iso, partner_iso, flow_share) %>%
    dplyr::filter(flow_share > 0)
  
  selected <- ompr::get_solution(result, z[i]) %>%
    dplyr::mutate(iso3c = iso[i], selected = as.integer(value) == 1) %>%
    dplyr::select(iso3c, selected)
  
  specialization <- nodes_stage %>%
    dplyr::left_join(selected, by = "iso3c") %>%
    dplyr::mutate(selected = dplyr::coalesce(selected, FALSE)) %>%
    dplyr::left_join(
      flows %>%
        dplyr::group_by(reporter_iso) %>%
        dplyr::summarize(production_share = sum(flow_share, na.rm = TRUE), .groups = "drop") %>%
        dplyr::rename(iso3c = reporter_iso),
      by = "iso3c"
    ) %>%
    dplyr::mutate(production_share = dplyr::coalesce(production_share, 0)) %>%
    dplyr::select(iso3c, country, producer_score, dev_potential, demand_weight, selected, production_share)
  
  flows <- flows %>%
    dplyr::left_join(edges_full, by = c("reporter_iso", "partner_iso"))
  
  list(
    specialization = specialization,
    flows = flows,
    objective = ompr::objective_value(result),
    hhi = allied_network_hhi(specialization$production_share),
    n_producers = sum(specialization$selected, na.rm = TRUE)
  )
}

# ---- Main orchestration ------------------------------------------------------

allied_network_design <- function(economic_opportunity_index,
                                  energy_security_index,
                                  policy_index,
                                  partner_friendshore_tbl,
                                  partner_opportunity_tbl,
                                  country_info,
                                  iso3c_network = allied_network_default_iso3c(),
                                  partner_development_country_tbl = NULL,
                                  techs = NULL,
                                  node_weights = list(eo = 0.5, policy = 0.3, resilience = 0.2),
                                  edge_weights = list(friendshore = 0.5, opportunity = 0.5),
                                  method = c("auto", "milp", "greedy"),
                                  # constraints / knobs
                                  min_producers = 3,
                                  max_share = 0.40,
                                  min_share = 0.05,
                                  min_suppliers_per_consumer = 2,
                                  epsilon_supplier_share = 0.10,
                                  allow_self = TRUE,
                                  # objective weights
                                  w_node = 1.0,
                                  w_edge = 0.5,
                                  w_dev = 0.0,
                                  progress_callback = NULL,
                                  auto_milp_max_nodes = 18,
                                  milp_stage_time_limit_sec = 120) {
  method <- match.arg(method)
  
  nodes <- allied_network_prepare_nodes(
    economic_opportunity_index = economic_opportunity_index,
    energy_security_index = energy_security_index,
    policy_index = policy_index,
    country_info = country_info,
    iso3c_network = iso3c_network,
    partner_development_country_tbl = partner_development_country_tbl,
    node_weights = node_weights,
    demand_from = "need"
  )
  
  if (!is.null(techs)) {
    nodes <- nodes %>% dplyr::filter(tech %in% techs)
  }
  
  edges <- allied_network_extract_edges(
    partner_friendshore_tbl = partner_friendshore_tbl,
    partner_opportunity_tbl = partner_opportunity_tbl,
    iso3c_network = iso3c_network,
    edge_weights = edge_weights,
    keep_components = TRUE
  )
  
  stages <- nodes %>%
    dplyr::distinct(tech, supply_chain) %>%
    dplyr::arrange(tech, supply_chain)
  
  if (nrow(stages) == 0) {
    stop("No (tech, supply_chain) stages available after filtering.")
  }
  
  has_milp <- allied_network_has_milp()
  use_method <- method
  if (method == "auto") {
    use_method <- if (has_milp) "milp" else "greedy"
  }
  
  nodes_selected <- nodes %>%
    dplyr::select(tech, supply_chain, iso3c, country, producer_score, demand_weight, dev_potential)
  edges_selected <- edges %>%
    dplyr::select(tech, supply_chain, reporter_iso, partner_iso, edge_weight, dplyr::any_of(c("friendshore_index", "opportunity_index")))

  stage_keys <- paste(stages$tech, stages$supply_chain, sep = "||")
  node_keys <- paste(nodes_selected$tech, nodes_selected$supply_chain, sep = "||")
  edge_keys <- paste(edges_selected$tech, edges_selected$supply_chain, sep = "||")

  nodes_by_stage <- split(nodes_selected, node_keys, drop = TRUE)
  edges_by_stage <- split(edges_selected, edge_keys, drop = TRUE)

  stage_count <- length(stage_keys)
  start_time <- Sys.time()
  results <- vector("list", stage_count)

  for (i in seq_len(stage_count)) {
    t <- stages$tech[[i]]
    sc <- stages$supply_chain[[i]]
    key <- stage_keys[[i]]

    if (is.function(progress_callback)) {
      progress_callback(list(
        event = "start_stage",
        current = i,
        total = stage_count,
        tech = t,
        supply_chain = sc,
        elapsed_sec = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
        eta_sec = if (i <= 1) NA_real_ else as.numeric(difftime(Sys.time(), start_time, units = "secs")) / (i - 1) * (stage_count - i + 1),
        pct_complete = (i - 1) / stage_count,
        pct_remaining = 1 - ((i - 1) / stage_count),
        method = use_method
      ))
    }

    nodes_stage <- nodes_by_stage[[key]]
    # skip tiny stages
    if (is.null(nodes_stage) || nrow(nodes_stage) < 3) {
      results[[i]] <- list(
        tech = t, supply_chain = sc,
        specialization = NULL, flows = NULL,
        objective = NA_real_, hhi = NA_real_, n_producers = NA_integer_, method_used = NA_character_
      )
      next
    }

    edges_stage <- edges_by_stage[[key]]
    if (is.null(edges_stage)) {
      edges_stage <- edges_selected[0, , drop = FALSE]
    }

    nodes_stage_core <- nodes_stage %>% dplyr::select(iso3c, country, producer_score, demand_weight, dev_potential)
    edges_stage_core <- edges_stage %>% dplyr::select(reporter_iso, partner_iso, edge_weight, dplyr::any_of(c("friendshore_index", "opportunity_index")))

    stage_method <- use_method
    if (method == "auto") {
      stage_method <- if (has_milp && nrow(nodes_stage_core) <= auto_milp_max_nodes) "milp" else "greedy"
    }

    solve_greedy <- function() {
      allied_network_solve_stage_greedy(
        nodes_stage = nodes_stage_core,
        edges_stage = edges_stage_core,
        min_producers = min_producers,
        max_share = max_share,
        min_share = min_share,
        w_edge = w_edge,
        w_node = w_node,
        w_dev = w_dev,
        allow_self = allow_self
      )
    }

    if (stage_method == "milp") {
      sol <- tryCatch({
        if (is.finite(milp_stage_time_limit_sec) && !is.na(milp_stage_time_limit_sec) && milp_stage_time_limit_sec > 0) {
          setTimeLimit(elapsed = milp_stage_time_limit_sec, transient = TRUE)
        }
        allied_network_solve_stage_milp(
          nodes_stage = nodes_stage_core,
          edges_stage = edges_stage_core,
          min_producers = min_producers,
          max_share = max_share,
          min_share = min_share,
          min_suppliers_per_consumer = min_suppliers_per_consumer,
          epsilon_supplier_share = epsilon_supplier_share,
          w_edge = w_edge,
          w_node = w_node,
          w_dev = w_dev,
          allow_self = allow_self
        )
      }, error = function(e) {
        if (is.function(progress_callback)) {
          progress_callback(list(
            event = "fallback_greedy",
            current = i,
            total = stage_count,
            tech = t,
            supply_chain = sc,
            elapsed_sec = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
            reason = conditionMessage(e),
            from_method = "milp",
            to_method = "greedy"
          ))
        }
        stage_method <<- "greedy"
        solve_greedy()
      })
      setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
    } else {
      sol <- solve_greedy()
    }

    elapsed_done <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    done_count <- i
    eta_done <- if (done_count <= 0) NA_real_ else (elapsed_done / done_count) * (stage_count - done_count)
    if (is.function(progress_callback)) {
      progress_callback(list(
        event = "end_stage",
        current = i,
        total = stage_count,
        tech = t,
        supply_chain = sc,
        elapsed_sec = elapsed_done,
        eta_sec = eta_done,
        pct_complete = done_count / stage_count,
        pct_remaining = 1 - (done_count / stage_count),
        method = stage_method
      ))
    }

    results[[i]] <- list(
      tech = t,
      supply_chain = sc,
      specialization = sol$specialization,
      flows = sol$flows,
      objective = sol$objective,
      hhi = sol$hhi,
      n_producers = sol$n_producers,
      method_used = stage_method
    )
  }
  
  specialization_tbl <- purrr::map_dfr(results, function(x) {
    if (is.null(x$specialization)) return(NULL)
    x$specialization %>%
      dplyr::mutate(tech = x$tech, supply_chain = x$supply_chain) %>%
      dplyr::relocate(tech, supply_chain, .before = iso3c)
  })
  
  flows_tbl <- purrr::map_dfr(results, function(x) {
    if (is.null(x$flows)) return(NULL)
    x$flows %>%
      dplyr::mutate(tech = x$tech, supply_chain = x$supply_chain) %>%
      dplyr::relocate(tech, supply_chain, .before = reporter_iso)
  })
  
  diagnostics_tbl <- purrr::map_dfr(results, function(x) {
    tibble::tibble(
      tech = x$tech,
      supply_chain = x$supply_chain,
      method = dplyr::coalesce(x$method_used, use_method),
      n_producers = x$n_producers,
      hhi = x$hhi,
      objective = x$objective
    )
  })
  
  # Build-candidates (optional): rank non-selected (or low-share) nodes by dev_potential * connectivity
  build_candidates_tbl <- NULL
  if (!is.null(partner_development_country_tbl)) {
    build_candidates_tbl <- specialization_tbl %>%
      dplyr::group_by(tech, supply_chain) %>%
      dplyr::mutate(
        build_priority = dev_potential * (0.6 * producer_score + 0.4 * dplyr::coalesce(connectivity_out, 0)),
        build_priority = dplyr::if_else(is.na(build_priority), 0, build_priority)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(dplyr::desc(build_priority)) %>%
      dplyr::select(tech, supply_chain, iso3c, country, dev_potential, producer_score, dplyr::any_of("connectivity_out"), selected, production_share, build_priority)
  }
  
  list(
    params = list(
      iso3c_network = iso3c_network,
      node_weights = node_weights,
      edge_weights = edge_weights,
      method = use_method,
      min_producers = min_producers,
      max_share = max_share,
      min_share = min_share,
      min_suppliers_per_consumer = min_suppliers_per_consumer,
      epsilon_supplier_share = epsilon_supplier_share,
      allow_self = allow_self,
      w_node = w_node,
      w_edge = w_edge,
      w_dev = w_dev,
      auto_milp_max_nodes = auto_milp_max_nodes,
      milp_stage_time_limit_sec = milp_stage_time_limit_sec
    ),
    specialization = specialization_tbl,
    flows = flows_tbl,
    diagnostics = diagnostics_tbl,
    build_candidates = build_candidates_tbl
  )
}
