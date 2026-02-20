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

allied_network_compute_portfolio_caps <- function(country_size_tbl,
                                                  iso3c_network,
                                                  min_cap = 2,
                                                  max_cap = 10,
                                                  transform = c("log", "linear"),
                                                  clamp = TRUE) {
  transform <- match.arg(transform)
  require_columns(country_size_tbl, c("iso3c", "gdp_usd"), label = "country_size_tbl")

  tbl <- tibble::tibble(iso3c = as.character(iso3c_network)) %>%
    dplyr::left_join(
      country_size_tbl %>%
        dplyr::transmute(
          iso3c = as.character(iso3c),
          gdp_usd = suppressWarnings(as.numeric(gdp_usd)),
          gdp_imputed = if ("gdp_imputed" %in% names(country_size_tbl)) dplyr::coalesce(as.logical(.data$gdp_imputed), FALSE) else FALSE
        ) %>%
        dplyr::group_by(iso3c) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup(),
      by = "iso3c"
    )

  med <- stats::median(tbl$gdp_usd[is.finite(tbl$gdp_usd)], na.rm = TRUE)
  if (!is.finite(med)) med <- 1
  tbl <- tbl %>%
    dplyr::mutate(
      gdp_usd = dplyr::if_else(is.finite(gdp_usd), gdp_usd, med),
      gdp_imputed = dplyr::coalesce(gdp_imputed, FALSE)
    )

  x <- if (transform == "log") log1p(tbl$gdp_usd) else tbl$gdp_usd
  rng <- range(x, na.rm = TRUE)
  denom <- rng[[2]] - rng[[1]]
  norm <- if (!is.finite(denom) || denom <= 0) rep(0, length(x)) else (x - rng[[1]]) / denom

  caps <- round(min_cap + norm * (max_cap - min_cap))
  if (isTRUE(clamp)) caps <- pmax(min_cap, pmin(max_cap, caps))
  if (length(caps) > 0) caps[which.max(tbl$gdp_usd)] <- max_cap
  stats::setNames(as.integer(caps), tbl$iso3c)
}

allied_network_get_stage_target <- function(stage_targets_tbl, tech, supply_chain) {
  if (is.null(stage_targets_tbl)) return(NA_real_)
  require_columns(stage_targets_tbl, c("tech", "supply_chain", "target_exports_usd"), label = "stage_targets_tbl")

  target_tbl <- stage_targets_tbl %>%
    dplyr::filter(tech == !!tech, supply_chain == !!supply_chain) %>%
    dplyr::mutate(
      Year = suppressWarnings(as.numeric(.data$Year)),
      target_exports_usd = suppressWarnings(as.numeric(target_exports_usd))
    )

  if (nrow(target_tbl) == 0) return(NA_real_)
  if ("Year" %in% names(target_tbl) && any(is.finite(target_tbl$Year))) {
    yr <- max(target_tbl$Year[is.finite(target_tbl$Year)], na.rm = TRUE)
    target_tbl <- target_tbl %>% dplyr::filter(.data$Year == yr)
  }

  out <- target_tbl$target_exports_usd[[1]]
  if (!is.finite(out)) return(NA_real_)
  out
}

allied_network_get_scale_caps <- function(scale_caps_tbl, tech, supply_chain, iso_vec) {
  iso_vec <- as.character(iso_vec)
  if (is.null(scale_caps_tbl)) {
    return(list(
      cap0 = stats::setNames(rep(0, length(iso_vec)), iso_vec),
      capMax = stats::setNames(rep(Inf, length(iso_vec)), iso_vec),
      build_max = stats::setNames(rep(Inf, length(iso_vec)), iso_vec)
    ))
  }

  require_columns(
    scale_caps_tbl,
    c("iso3c", "tech", "supply_chain", "cap0_exports_usd", "capMax_exports_usd", "build_max_usd"),
    label = "scale_caps_tbl"
  )

  caps <- scale_caps_tbl %>%
    dplyr::filter(tech == !!tech, supply_chain == !!supply_chain, iso3c %in% iso_vec) %>%
    dplyr::mutate(
      Year = suppressWarnings(as.numeric(.data$Year)),
      cap0_exports_usd = suppressWarnings(as.numeric(cap0_exports_usd)),
      capMax_exports_usd = suppressWarnings(as.numeric(capMax_exports_usd)),
      build_max_usd = suppressWarnings(as.numeric(build_max_usd))
    )

  if ("Year" %in% names(caps) && any(is.finite(caps$Year))) {
    caps <- caps %>%
      dplyr::group_by(iso3c) %>%
      dplyr::filter(.data$Year == max(.data$Year, na.rm = TRUE)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
  } else {
    caps <- caps %>% dplyr::group_by(iso3c) %>% dplyr::slice(1) %>% dplyr::ungroup()
  }

  caps <- tibble::tibble(iso3c = iso_vec) %>%
    dplyr::left_join(caps, by = "iso3c") %>%
    dplyr::mutate(
      cap0_exports_usd = dplyr::coalesce(cap0_exports_usd, 0),
      capMax_exports_usd = dplyr::coalesce(capMax_exports_usd, 0),
      build_max_usd = dplyr::coalesce(build_max_usd, 0)
    )

  list(
    cap0 = stats::setNames(caps$cap0_exports_usd, caps$iso3c),
    capMax = stats::setNames(caps$capMax_exports_usd, caps$iso3c),
    build_max = stats::setNames(caps$build_max_usd, caps$iso3c)
  )
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
                                         demand_from = NULL,
                                         country_size_tbl = NULL,
                                         demand_mode = c("need", "size", "mixed", "equal"),
                                         demand_weights = list(need = 0.5, size = 0.5),
                                         size_transform = c("log_scurve", "scurve", "rank")) {
  demand_mode <- match.arg(demand_mode)
  size_transform <- match.arg(size_transform)
  if (!is.null(demand_from)) {
    demand_from <- match.arg(demand_from, c("need", "equal"))
    demand_mode <- demand_from
  }
  
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

  normalize_country_col <- function(tbl) {
    if (!inherits(tbl, "data.frame") || "Country" %in% names(tbl)) {
      return(tbl)
    }

    if ("country" %in% names(tbl)) {
      return(dplyr::rename(tbl, Country = country))
    }

    if ("Partner" %in% names(tbl)) {
      return(dplyr::rename(tbl, Country = Partner))
    }

    if ("partner" %in% names(tbl)) {
      return(dplyr::rename(tbl, Country = partner))
    }

    tbl
  }
  
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
    partner_development_country_tbl <- normalize_country_col(partner_development_country_tbl)
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
  
  if (!is.null(country_size_tbl)) {
    require_columns(country_size_tbl, c("iso3c", "gdp_usd"), label = "country_size_tbl")
    nodes <- nodes %>%
      dplyr::left_join(
        country_size_tbl %>%
          dplyr::transmute(
            iso3c = as.character(iso3c),
            gdp_usd = suppressWarnings(as.numeric(gdp_usd)),
            gdp_imputed = if ("gdp_imputed" %in% names(country_size_tbl)) dplyr::coalesce(as.logical(.data$gdp_imputed), FALSE) else FALSE
          ) %>%
          dplyr::group_by(iso3c) %>%
          dplyr::slice(1) %>%
          dplyr::ungroup(),
        by = "iso3c"
      )
  } else {
    nodes <- nodes %>% dplyr::mutate(gdp_usd = NA_real_, gdp_imputed = FALSE)
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
      size_idx = dplyr::case_when(
        size_transform == "log_scurve" ~ median_scurve(log1p(gdp_usd)),
        size_transform == "scurve" ~ median_scurve(gdp_usd),
        size_transform == "rank" ~ dplyr::percent_rank(gdp_usd),
        TRUE ~ median_scurve(log1p(gdp_usd))
      ),
      size_idx = dplyr::coalesce(size_idx, 0),
      producer_score = w_node[["eo"]] * eo_idx + w_node[["policy"]] * pol_idx + w_node[["resilience"]] * es_idx
    ) %>%
    dplyr::ungroup()

  w_need <- suppressWarnings(as.numeric(demand_weights$need))
  w_size <- suppressWarnings(as.numeric(demand_weights$size))
  if (!is.finite(w_need)) w_need <- 0.5
  if (!is.finite(w_size)) w_size <- 0.5
  
  nodes <- nodes %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(
      demand_weight = dplyr::case_when(
        demand_mode == "equal" ~ 1,
        demand_mode == "need" ~ dplyr::coalesce(need_idx, 0),
        demand_mode == "size" ~ dplyr::coalesce(size_idx, 0),
        demand_mode == "mixed" ~ dplyr::coalesce(w_need * need_idx + w_size * size_idx, 0),
        TRUE ~ dplyr::coalesce(need_idx, 0)
      ),
      demand_need_component = dplyr::case_when(
        demand_mode == "mixed" ~ dplyr::coalesce(w_need * need_idx, 0),
        demand_mode == "need" ~ dplyr::coalesce(need_idx, 0),
        TRUE ~ 0
      ),
      demand_size_component = dplyr::case_when(
        demand_mode == "mixed" ~ dplyr::coalesce(w_size * size_idx, 0),
        demand_mode == "size" ~ dplyr::coalesce(size_idx, 0),
        TRUE ~ 0
      ),
      demand_weight = if (all(is.na(demand_weight)) || sum(demand_weight, na.rm = TRUE) <= 0) {
        rep(1, dplyr::n())
      } else {
        demand_weight
      },
      demand_need_component = if (sum(demand_need_component, na.rm = TRUE) > 0) demand_need_component / sum(demand_need_component, na.rm = TRUE) else 0,
      demand_size_component = if (sum(demand_size_component, na.rm = TRUE) > 0) demand_size_component / sum(demand_size_component, na.rm = TRUE) else 0,
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
                                              producer_forbidden = character(),
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
  eligible_iso <- setdiff(iso, producer_forbidden)
  if (!length(eligible_iso)) eligible_iso <- iso
  min_required <- ceiling(1 / max_share)
  target_prod <- max(min_producers, min_required)
  n_prod <- min(target_prod, length(eligible_iso))
  if (length(eligible_iso) < target_prod) warning("Producer forbiddance reduced feasible producers for stage; using eligible maximum.")
  
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
  
  selected_iso <- candidates %>% dplyr::filter(iso3c %in% eligible_iso) %>% dplyr::pull(iso3c) %>% utils::head(n_prod)
  
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
    dplyr::select(iso3c, country, producer_score, connectivity_out, dev_potential, demand_weight, selected, production_share, dplyr::any_of(c("gdp_usd", "gdp_imputed", "size_idx", "need_idx", "demand_need_component", "demand_size_component")))
  
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
                                            producer_forbidden = character(),
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
  eligible_iso <- setdiff(iso, producer_forbidden)
  if (!length(eligible_iso)) eligible_iso <- iso
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
    ompr::add_constraint(ompr::sum_expr(z[i], i = 1:n) >= min(min_producers, length(eligible_iso))) %>%
    # Supplier assignment constraints (redundancy per consumer)
    ompr::add_constraint(f[i, j] <= d[j] * y[i, j], i = 1:n, j = 1:n) %>%
    ompr::add_constraint(f[i, j] >= epsilon_supplier_share * d[j] * y[i, j], i = 1:n, j = 1:n) %>%
    ompr::add_constraint(y[i, j] <= z[i], i = 1:n, j = 1:n) %>%
    ompr::add_constraint(ompr::sum_expr(y[i, j], i = 1:n) >= min_suppliers_per_consumer, j = 1:n)

  if (length(setdiff(iso, eligible_iso)) > 0) {
    forbidden_idx <- which(iso %in% setdiff(iso, eligible_iso))
    model <- model %>% ompr::add_constraint(z[i] == 0, i = forbidden_idx)
  }

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
    dplyr::select(iso3c, country, producer_score, dev_potential, demand_weight, selected, production_share, dplyr::any_of(c("gdp_usd", "gdp_imputed", "size_idx", "need_idx", "demand_need_component", "demand_size_component")))
  
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

allied_network_build_cost <- function(dev_potential, build_cost_mode = c("none", "flat", "dev_potential")) {
  build_cost_mode <- match.arg(build_cost_mode)
  dev <- suppressWarnings(as.numeric(dev_potential))
  dev[!is.finite(dev)] <- 0.5
  dev <- pmax(0, pmin(1, dev))
  if (build_cost_mode == "none") return(rep(0, length(dev)))
  if (build_cost_mode == "flat") return(rep(1, length(dev)))
  1 - dev
}

allied_network_solve_stage_milp_scaled <- function(nodes_stage,
                                                   edges_stage,
                                                   target_total_usd,
                                                   producer_forbidden = character(),
                                                   cap0_usd,
                                                   capMax_usd,
                                                   build_max_usd,
                                                   build_cost_mode = c("none", "flat", "dev_potential"),
                                                   build_cost_weight = 0,
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
  build_cost_mode <- match.arg(build_cost_mode)
  if (!allied_network_has_milp()) {
    stop("MILP solver packages not available. Install ompr, ompr.roi, ROI, ROI.plugin.glpk or use method='greedy'.")
  }
  require_columns(nodes_stage, c("iso3c", "producer_score", "demand_weight", "dev_potential"), label = "nodes_stage")
  require_columns(edges_stage, c("reporter_iso", "partner_iso", "edge_weight"), label = "edges_stage")

  iso <- as.character(nodes_stage$iso3c)
  eligible_iso <- setdiff(iso, producer_forbidden)
  if (!length(eligible_iso)) eligible_iso <- iso
  n <- length(iso)
  if (n == 0) stop("No nodes in stage.")
  target_total_usd <- suppressWarnings(as.numeric(target_total_usd))
  if (!is.finite(target_total_usd) || target_total_usd <= 0) stop("target_total_usd must be positive.")

  cap0 <- suppressWarnings(as.numeric(cap0_usd[iso]))
  capMax <- suppressWarnings(as.numeric(capMax_usd[iso]))
  build_max <- suppressWarnings(as.numeric(build_max_usd[iso]))
  cap0[!is.finite(cap0)] <- 0
  capMax[!is.finite(capMax)] <- 0
  build_max[!is.finite(build_max)] <- 0
  cap0 <- pmax(0, cap0)
  capMax <- pmax(0, capMax)
  build_max <- pmax(0, build_max)

  edges_full <- allied_network_complete_edges_stage(edges_stage, iso3c_vec = iso, allow_self = TRUE, fill_missing = 0, self_weight = NULL)

  a <- suppressWarnings(as.numeric(nodes_stage$producer_score)); a[!is.finite(a)] <- 0
  d <- suppressWarnings(as.numeric(nodes_stage$demand_weight)); d[!is.finite(d)] <- 0
  dev <- suppressWarnings(as.numeric(nodes_stage$dev_potential)); dev[!is.finite(dev)] <- 0.5
  if (sum(d, na.rm = TRUE) <= 0) d <- rep(1 / n, n) else d <- d / sum(d, na.rm = TRUE)
  D <- d * target_total_usd
  min_lb <- pmin(min_share * target_total_usd, capMax)
  max_prod <- pmin(max_share * target_total_usd, capMax)
  build_cost <- allied_network_build_cost(dev, build_cost_mode = build_cost_mode)

  W <- matrix(0, nrow = n, ncol = n, dimnames = list(iso, iso))
  for (k in seq_len(nrow(edges_full))) {
    i <- edges_full$reporter_iso[[k]]
    j <- edges_full$partner_iso[[k]]
    ew <- suppressWarnings(as.numeric(edges_full$edge_weight[[k]]))
    if (!is.na(i) && !is.na(j) && i %in% iso && j %in% iso) W[i, j] <- if (is.finite(ew)) ew else 0
  }

  suppressPackageStartupMessages(library(ompr))
  suppressPackageStartupMessages(library(ompr.roi))
  suppressPackageStartupMessages(library(ROI))
  suppressPackageStartupMessages(library(ROI.plugin.glpk))

  model <- ompr::MIPModel() %>%
    ompr::add_variable(q[i, j], i = 1:n, j = 1:n, type = "continuous", lb = 0) %>%
    ompr::add_variable(z[i], i = 1:n, type = "binary") %>%
    ompr::add_variable(y[i, j], i = 1:n, j = 1:n, type = "binary") %>%
    ompr::add_variable(build[i], i = 1:n, type = "continuous", lb = 0) %>%
    ompr::set_objective(
      ompr::sum_expr(q[i, j] * (w_node * a[i] + w_edge * W[iso[i], iso[j]]), i = 1:n, j = 1:n) +
        ompr::sum_expr(w_dev * dev[i] * z[i], i = 1:n) -
        build_cost_weight * ompr::sum_expr(build[i] * build_cost[i], i = 1:n),
      "max"
    ) %>%
    ompr::add_constraint(ompr::sum_expr(q[i, j], i = 1:n) == D[j], j = 1:n) %>%
    ompr::add_constraint(ompr::sum_expr(q[i, j], j = 1:n) <= cap0[i] + build[i], i = 1:n) %>%
    ompr::add_constraint(build[i] <= build_max[i], i = 1:n) %>%
    ompr::add_constraint(ompr::sum_expr(q[i, j], j = 1:n) <= capMax[i], i = 1:n) %>%
    ompr::add_constraint(ompr::sum_expr(q[i, j], j = 1:n) <= max_prod[i] * z[i], i = 1:n) %>%
    ompr::add_constraint(ompr::sum_expr(q[i, j], j = 1:n) >= min_lb[i] * z[i], i = 1:n) %>%
    ompr::add_constraint(ompr::sum_expr(z[i], i = 1:n) >= min(min_producers, length(eligible_iso))) %>%
    ompr::add_constraint(q[i, j] <= D[j] * y[i, j], i = 1:n, j = 1:n) %>%
    ompr::add_constraint(q[i, j] >= epsilon_supplier_share * D[j] * y[i, j], i = 1:n, j = 1:n) %>%
    ompr::add_constraint(y[i, j] <= z[i], i = 1:n, j = 1:n) %>%
    ompr::add_constraint(ompr::sum_expr(y[i, j], i = 1:n) >= min_suppliers_per_consumer, j = 1:n)

  if (length(setdiff(iso, eligible_iso)) > 0) {
    forbidden_idx <- which(iso %in% setdiff(iso, eligible_iso))
    model <- model %>% ompr::add_constraint(z[i] == 0, i = forbidden_idx)
  }

  if (!isTRUE(allow_self)) {
    model <- model %>% ompr::add_constraint(q[i, i] == 0, i = 1:n) %>% ompr::add_constraint(y[i, i] == 0, i = 1:n)
  }

  result <- ompr::solve_model(model, ompr.roi::with_ROI(solver = solver))
  status <- ROI::solution(result, "status")$code
  if (!identical(status, 0L)) stop("Scaled MILP did not converge to optimal solution.")

  flows <- ompr::get_solution(result, q[i, j]) %>%
    dplyr::mutate(reporter_iso = iso[i], partner_iso = iso[j], flow_usd = value) %>%
    dplyr::select(reporter_iso, partner_iso, flow_usd) %>%
    dplyr::filter(flow_usd > 1e-9) %>%
    dplyr::mutate(flow_share = flow_usd / target_total_usd) %>%
    dplyr::left_join(edges_full, by = c("reporter_iso", "partner_iso"))

  selected <- ompr::get_solution(result, z[i]) %>%
    dplyr::mutate(iso3c = iso[i], selected = as.integer(value) == 1) %>%
    dplyr::select(iso3c, selected)

  build_tbl <- ompr::get_solution(result, build[i]) %>%
    dplyr::transmute(iso3c = iso[i], build_usd = value)

  specialization <- nodes_stage %>%
    dplyr::left_join(selected, by = "iso3c") %>%
    dplyr::mutate(selected = dplyr::coalesce(selected, FALSE)) %>%
    dplyr::left_join(
      flows %>% dplyr::group_by(reporter_iso) %>% dplyr::summarize(production_usd = sum(flow_usd, na.rm = TRUE), .groups = "drop") %>% dplyr::rename(iso3c = reporter_iso),
      by = "iso3c"
    ) %>%
    dplyr::left_join(build_tbl, by = "iso3c") %>%
    dplyr::mutate(
      production_usd = dplyr::coalesce(production_usd, 0),
      production_share = production_usd / target_total_usd,
      cap0_exports_usd = cap0,
      capMax_exports_usd = capMax,
      build_max_usd = build_max,
      build_needed_usd = pmax(0, production_usd - cap0_exports_usd),
      build_usd = dplyr::coalesce(build_usd, 0)
    ) %>%
    dplyr::select(iso3c, country, selected, production_usd, production_share, cap0_exports_usd, capMax_exports_usd, build_max_usd, build_needed_usd, demand_weight, producer_score, dev_potential, dplyr::any_of(c("connectivity_out", "gdp_usd", "gdp_imputed", "size_idx", "need_idx", "demand_need_component", "demand_size_component")))

  list(
    specialization = specialization,
    flows = flows %>% dplyr::select(reporter_iso, partner_iso, flow_usd, flow_share, edge_weight, dplyr::any_of(c("friendshore_index", "opportunity_index"))),
    objective = ompr::objective_value(result),
    hhi = allied_network_hhi(specialization$production_share),
    n_producers = sum(specialization$selected, na.rm = TRUE),
    unmet_demand_usd = 0
  )
}

allied_network_solve_stage_greedy_scaled <- function(nodes_stage,
                                                     edges_stage,
                                                     target_total_usd,
                                                     producer_forbidden = character(),
                                                     cap0_usd,
                                                     capMax_usd,
                                                     build_max_usd,
                                                     min_producers = 3,
                                                     max_share = 0.40,
                                                     min_share = 0.05,
                                                     min_suppliers_per_consumer = 2,
                                                     epsilon_supplier_share = 0.10,
                                                     w_edge = 0.5,
                                                     w_node = 1.0,
                                                     w_dev = 0.0,
                                                     allow_self = TRUE) {
  require_columns(nodes_stage, c("iso3c", "producer_score", "demand_weight", "dev_potential"), label = "nodes_stage")
  require_columns(edges_stage, c("reporter_iso", "partner_iso", "edge_weight"), label = "edges_stage")
  iso <- as.character(nodes_stage$iso3c)
  target_total_usd <- suppressWarnings(as.numeric(target_total_usd))
  if (!is.finite(target_total_usd) || target_total_usd <= 0) stop("target_total_usd must be positive.")

  edges_full <- allied_network_complete_edges_stage(edges_stage, iso3c_vec = iso, allow_self = TRUE, fill_missing = 0, self_weight = NULL)
  d_tbl <- nodes_stage %>% dplyr::select(iso3c, demand_weight)
  d <- suppressWarnings(as.numeric(d_tbl$demand_weight)); d[!is.finite(d)] <- 0
  if (sum(d) <= 0) d <- rep(1 / length(d), length(d)) else d <- d / sum(d)
  D <- stats::setNames(d * target_total_usd, d_tbl$iso3c)

  conn <- edges_full %>%
    dplyr::left_join(d_tbl, by = c("partner_iso" = "iso3c")) %>%
    dplyr::group_by(reporter_iso) %>%
    dplyr::summarize(connectivity_out = sum(edge_weight * demand_weight, na.rm = TRUE), .groups = "drop") %>%
    dplyr::rename(iso3c = reporter_iso)

  candidates <- nodes_stage %>%
    dplyr::left_join(conn, by = "iso3c") %>%
    dplyr::mutate(
      connectivity_out = dplyr::coalesce(connectivity_out, 0),
      combined_score = w_node * producer_score + w_edge * connectivity_out + w_dev * dev_potential
    ) %>%
    dplyr::arrange(dplyr::desc(combined_score))

  cap0 <- stats::setNames(pmax(0, suppressWarnings(as.numeric(cap0_usd[iso]))), iso); cap0[!is.finite(cap0)] <- 0
  capMax <- stats::setNames(pmax(0, suppressWarnings(as.numeric(capMax_usd[iso]))), iso); capMax[!is.finite(capMax)] <- 0
  build_max <- stats::setNames(pmax(0, suppressWarnings(as.numeric(build_max_usd[iso]))), iso); build_max[!is.finite(build_max)] <- 0
  remaining_cap <- capMax

  min_required <- max(min_producers, ceiling(1 / max_share))
  if (length(eligible_iso) < min_required) warning("Producer forbiddance reduced feasible producers for stage; using eligible maximum.")
  selected_iso <- candidates %>% dplyr::filter(iso3c %in% eligible_iso) %>% dplyr::pull(iso3c) %>% utils::head(min(min_required, length(eligible_iso)))

  allocate_once <- function(selected_iso, remaining_cap, unmet_by_consumer) {
    flow_rows <- list()
    for (j in names(unmet_by_consumer)) {
      remaining_j <- unmet_by_consumer[[j]]
      if (remaining_j <= 1e-9) next
      while (remaining_j > 1e-9) {
        cand <- selected_iso[remaining_cap[selected_iso] > 1e-9]
        if (!length(cand)) break
        wj <- edges_full %>%
          dplyr::filter(reporter_iso %in% cand, partner_iso == j) %>%
          dplyr::left_join(candidates %>% dplyr::select(iso3c, producer_score), by = c("reporter_iso" = "iso3c")) %>%
          dplyr::mutate(weight = pmax(0, edge_weight * producer_score)) %>%
          dplyr::select(reporter_iso, weight)
        if (!isTRUE(allow_self)) wj <- wj %>% dplyr::filter(reporter_iso != j)
        if (!nrow(wj)) break
        if (sum(wj$weight, na.rm = TRUE) <= 0) wj$weight <- 1
        wj <- wj %>% dplyr::mutate(weight = weight / sum(weight, na.rm = TRUE))
        allocated_any <- FALSE
        for (k in seq_len(nrow(wj))) {
          i <- wj$reporter_iso[[k]]
          alloc <- min(remaining_j * wj$weight[[k]], remaining_cap[[i]])
          if (alloc <= 1e-9) next
          flow_rows[[length(flow_rows) + 1]] <- tibble::tibble(reporter_iso = i, partner_iso = j, flow_usd = alloc)
          remaining_j <- remaining_j - alloc
          remaining_cap[[i]] <- remaining_cap[[i]] - alloc
          allocated_any <- TRUE
        }
        if (!allocated_any) break
      }
      unmet_by_consumer[[j]] <- remaining_j
    }
    list(flows = dplyr::bind_rows(flow_rows), remaining_cap = remaining_cap, unmet = unmet_by_consumer)
  }

  unmet <- D
  all_flows <- tibble::tibble(reporter_iso = character(), partner_iso = character(), flow_usd = numeric())
  cursor <- length(selected_iso)
  repeat {
    alloc <- allocate_once(selected_iso, remaining_cap, unmet)
    if (nrow(alloc$flows) > 0) all_flows <- dplyr::bind_rows(all_flows, alloc$flows)
    remaining_cap <- alloc$remaining_cap
    unmet <- alloc$unmet
    if (sum(unmet, na.rm = TRUE) <= 1e-6) break
    if (sum(remaining_cap, na.rm = TRUE) <= 1e-9 || cursor >= nrow(candidates)) break
    cursor <- cursor + 1
    next_iso <- candidates$iso3c[[cursor]]
    if (next_iso %in% eligible_iso) selected_iso <- unique(c(selected_iso, next_iso))
  }

  flows <- all_flows %>%
    dplyr::group_by(reporter_iso, partner_iso) %>%
    dplyr::summarize(flow_usd = sum(flow_usd, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(flow_share = flow_usd / target_total_usd) %>%
    dplyr::left_join(edges_full, by = c("reporter_iso", "partner_iso"))

  prod <- flows %>% dplyr::group_by(reporter_iso) %>% dplyr::summarize(production_usd = sum(flow_usd, na.rm = TRUE), .groups = "drop") %>% dplyr::rename(iso3c = reporter_iso)
  specialization <- candidates %>%
    dplyr::left_join(prod, by = "iso3c") %>%
    dplyr::mutate(
      selected = iso3c %in% selected_iso,
      production_usd = dplyr::coalesce(production_usd, 0),
      production_share = production_usd / target_total_usd,
      cap0_exports_usd = as.numeric(cap0[iso3c]),
      capMax_exports_usd = as.numeric(capMax[iso3c]),
      build_max_usd = as.numeric(build_max[iso3c]),
      build_needed_usd = pmax(0, production_usd - cap0_exports_usd)
    ) %>%
    dplyr::select(iso3c, country, selected, production_usd, production_share, cap0_exports_usd, capMax_exports_usd, build_max_usd, build_needed_usd, demand_weight, producer_score, dev_potential, connectivity_out, dplyr::any_of(c("gdp_usd", "gdp_imputed", "size_idx", "need_idx", "demand_need_component", "demand_size_component")))

  objective <- sum(flows$flow_usd * (w_node * specialization$producer_score[match(flows$reporter_iso, specialization$iso3c)] + w_edge * dplyr::coalesce(flows$edge_weight, 0)), na.rm = TRUE) +
    sum(w_dev * specialization$dev_potential * as.numeric(specialization$selected), na.rm = TRUE)

  list(
    specialization = specialization,
    flows = flows %>% dplyr::select(reporter_iso, partner_iso, flow_usd, flow_share, edge_weight, dplyr::any_of(c("friendshore_index", "opportunity_index"))),
    objective = objective,
    hhi = allied_network_hhi(specialization$production_share),
    n_producers = sum(specialization$selected, na.rm = TRUE),
    unmet_demand_usd = sum(unmet, na.rm = TRUE)
  )
}

# ---- Main orchestration ------------------------------------------------------

allied_network_build_topk_tbl <- function(specialization_tbl, portfolio_top_k = 5) {
  if (is.null(specialization_tbl) || !nrow(specialization_tbl)) return(tibble::tibble())
  specialization_tbl %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(
      rank_in_stage = dplyr::dense_rank(dplyr::desc(dplyr::coalesce(production_share, 0))),
      in_top_k = rank_in_stage <= portfolio_top_k & dplyr::coalesce(production_share, 0) > 0
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(tech, supply_chain, iso3c, production_share, dplyr::any_of('production_usd'), rank_in_stage, in_top_k)
}

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
                                  stage_targets_tbl = NULL,
                                  scale_caps_tbl = NULL,
                                  scale_mode = c("shares", "usd_target_match_china"),
                                  build_cost_mode = c("none", "flat", "dev_potential"),
                                  build_cost_weight = 0,
                                  country_size_tbl = NULL,
                                  demand_mode = c("need", "size", "mixed", "equal"),
                                  demand_weights = list(need = 0.5, size = 0.5),
                                  size_transform = c("log_scurve", "scurve", "rank"),
                                  portfolio_enable = FALSE,
                                  portfolio_top_k = 5,
                                  portfolio_min_cap = 2,
                                  portfolio_max_cap = 10,
                                  portfolio_transform = c("log", "linear"),
                                  portfolio_max_iters = 10,
                                  portfolio_verbose = FALSE,
                                  portfolio_caps_tbl = NULL,
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
  scale_mode <- match.arg(scale_mode)
  build_cost_mode <- match.arg(build_cost_mode)
  demand_mode <- match.arg(demand_mode)
  size_transform <- match.arg(size_transform)
  portfolio_transform <- match.arg(portfolio_transform)

  nodes <- allied_network_prepare_nodes(
    economic_opportunity_index = economic_opportunity_index,
    energy_security_index = energy_security_index,
    policy_index = policy_index,
    country_info = country_info,
    iso3c_network = iso3c_network,
    partner_development_country_tbl = partner_development_country_tbl,
    node_weights = node_weights,
    country_size_tbl = country_size_tbl,
    demand_mode = demand_mode,
    demand_weights = demand_weights,
    size_transform = size_transform
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

  if (nrow(stages) == 0) stop("No (tech, supply_chain) stages available after filtering.")

  has_milp <- allied_network_has_milp()
  use_method <- if (method == 'auto') if (has_milp) 'milp' else 'greedy' else method

  nodes_selected <- nodes %>%
    dplyr::select(
      tech, supply_chain, iso3c, country, producer_score, demand_weight, dev_potential,
      gdp_usd, gdp_imputed, size_idx, need_idx, demand_need_component, demand_size_component
    )
  edges_selected <- edges %>%
    dplyr::select(tech, supply_chain, reporter_iso, partner_iso, edge_weight, dplyr::any_of(c('friendshore_index', 'opportunity_index')))

  stage_keys <- paste(stages$tech, stages$supply_chain, sep = '||')
  names(stage_keys) <- stage_keys
  node_keys <- paste(nodes_selected$tech, nodes_selected$supply_chain, sep = '||')
  edge_keys <- paste(edges_selected$tech, edges_selected$supply_chain, sep = '||')
  nodes_by_stage <- split(nodes_selected, node_keys, drop = TRUE)
  edges_by_stage <- split(edges_selected, edge_keys, drop = TRUE)
  stage_count <- length(stage_keys)
  start_time <- Sys.time()

  producer_forbidden_by_stage <- stats::setNames(vector('list', length(stage_keys)), stage_keys)
  for (k in names(producer_forbidden_by_stage)) producer_forbidden_by_stage[[k]] <- character(0)

  cap_by_iso <- NULL
  caps_tbl <- NULL
  if (isTRUE(portfolio_enable)) {
    if (!is.null(portfolio_caps_tbl)) {
      require_columns(portfolio_caps_tbl, c('iso3c', 'portfolio_cap'), label = 'portfolio_caps_tbl')
      cap_by_iso <- portfolio_caps_tbl %>%
        dplyr::transmute(iso3c = as.character(iso3c), portfolio_cap = as.integer(portfolio_cap)) %>%
        dplyr::filter(iso3c %in% iso3c_network) %>%
        dplyr::distinct(iso3c, .keep_all = TRUE)
      cap_by_iso <- stats::setNames(cap_by_iso$portfolio_cap, cap_by_iso$iso3c)
    } else {
      if (is.null(country_size_tbl)) stop('portfolio_enable=TRUE requires country_size_tbl or portfolio_caps_tbl.')
      cap_by_iso <- allied_network_compute_portfolio_caps(
        country_size_tbl = country_size_tbl,
        iso3c_network = iso3c_network,
        min_cap = portfolio_min_cap,
        max_cap = portfolio_max_cap,
        transform = portfolio_transform,
        clamp = TRUE
      )
    }

    gdp_tbl <- nodes_selected %>%
      dplyr::distinct(iso3c, gdp_usd, gdp_imputed)
    caps_tbl <- tibble::tibble(iso3c = iso3c_network) %>%
      dplyr::mutate(portfolio_cap = as.integer(cap_by_iso[iso3c])) %>%
      dplyr::left_join(gdp_tbl, by = 'iso3c')
  }

  solve_one_stage <- function(stage_idx) {
    t <- stages$tech[[stage_idx]]
    sc <- stages$supply_chain[[stage_idx]]
    key <- stage_keys[[stage_idx]]

    nodes_stage <- nodes_by_stage[[key]]
    if (is.null(nodes_stage) || nrow(nodes_stage) < 3) {
      return(list(
        tech = t, supply_chain = sc,
        specialization = NULL, flows = NULL,
        objective = NA_real_, hhi = NA_real_, n_producers = NA_integer_, method_used = NA_character_,
        target_exports_usd = NA_real_, allies_cap0_usd = NA_real_, allies_capMax_usd = NA_real_,
        feasible_caps = NA, unmet_demand_usd = NA_real_
      ))
    }

    edges_stage <- edges_by_stage[[key]]
    if (is.null(edges_stage)) edges_stage <- edges_selected[0, , drop = FALSE]

    nodes_stage_core <- nodes_stage %>% dplyr::select(iso3c, country, producer_score, demand_weight, dev_potential, dplyr::any_of(c("gdp_usd", "gdp_imputed", "size_idx", "need_idx", "demand_need_component", "demand_size_component")))
    edges_stage_core <- edges_stage %>% dplyr::select(reporter_iso, partner_iso, edge_weight, dplyr::any_of(c('friendshore_index', 'opportunity_index')))

    stage_method <- use_method
    target_total_usd <- NA_real_
    allies_cap0_usd <- NA_real_
    allies_capMax_usd <- NA_real_
    feasible_caps <- NA

    scaled_enabled <- identical(scale_mode, 'usd_target_match_china') && !is.null(stage_targets_tbl) && !is.null(scale_caps_tbl)
    caps <- NULL
    if (scaled_enabled) {
      target_total_usd <- allied_network_get_stage_target(stage_targets_tbl, t, sc)
      caps <- allied_network_get_scale_caps(scale_caps_tbl, t, sc, nodes_stage_core$iso3c)
      allies_cap0_usd <- sum(caps$cap0, na.rm = TRUE)
      allies_capMax_usd <- sum(caps$capMax, na.rm = TRUE)
      feasible_caps <- allies_capMax_usd >= target_total_usd
      if (!is.finite(target_total_usd) || target_total_usd <= 0) {
        return(list(
          tech = t, supply_chain = sc,
          specialization = NULL, flows = NULL,
          objective = NA_real_, hhi = NA_real_, n_producers = NA_integer_, method_used = 'skipped_missing_target',
          target_exports_usd = target_total_usd, allies_cap0_usd = allies_cap0_usd,
          allies_capMax_usd = allies_capMax_usd, feasible_caps = feasible_caps, unmet_demand_usd = NA_real_
        ))
      }
    }

    if (method == 'auto') stage_method <- if (has_milp && nrow(nodes_stage_core) <= auto_milp_max_nodes) 'milp' else 'greedy'
    forbidden <- producer_forbidden_by_stage[[key]]

    solve_greedy <- function() {
      if (scaled_enabled) {
        allied_network_solve_stage_greedy_scaled(
          nodes_stage = nodes_stage_core, edges_stage = edges_stage_core,
          target_total_usd = target_total_usd, cap0_usd = caps$cap0, capMax_usd = caps$capMax,
          build_max_usd = caps$build_max, min_producers = min_producers, max_share = max_share,
          min_share = min_share, min_suppliers_per_consumer = min_suppliers_per_consumer,
          epsilon_supplier_share = epsilon_supplier_share, w_edge = w_edge, w_node = w_node,
          w_dev = w_dev, allow_self = allow_self, producer_forbidden = forbidden
        )
      } else {
        allied_network_solve_stage_greedy(
          nodes_stage = nodes_stage_core, edges_stage = edges_stage_core,
          min_producers = min_producers, max_share = max_share, min_share = min_share,
          w_edge = w_edge, w_node = w_node, w_dev = w_dev, allow_self = allow_self,
          producer_forbidden = forbidden
        )
      }
    }

    if (stage_method == 'milp') {
      setTimeLimit(cpu = milp_stage_time_limit_sec, elapsed = milp_stage_time_limit_sec, transient = TRUE)
      sol <- tryCatch({
        if (scaled_enabled) {
          allied_network_solve_stage_milp_scaled(
            nodes_stage = nodes_stage_core, edges_stage = edges_stage_core,
            target_total_usd = target_total_usd, cap0_usd = caps$cap0, capMax_usd = caps$capMax,
            build_max_usd = caps$build_max, build_cost_mode = build_cost_mode,
            build_cost_weight = build_cost_weight, min_producers = min_producers, max_share = max_share,
            min_share = min_share, min_suppliers_per_consumer = min_suppliers_per_consumer,
            epsilon_supplier_share = epsilon_supplier_share, w_edge = w_edge, w_node = w_node,
            w_dev = w_dev, allow_self = allow_self, producer_forbidden = forbidden
          )
        } else {
          allied_network_solve_stage_milp(
            nodes_stage = nodes_stage_core, edges_stage = edges_stage_core,
            min_producers = min_producers, max_share = max_share, min_share = min_share,
            min_suppliers_per_consumer = min_suppliers_per_consumer,
            epsilon_supplier_share = epsilon_supplier_share, w_edge = w_edge,
            w_node = w_node, w_dev = w_dev, allow_self = allow_self,
            producer_forbidden = forbidden
          )
        }
      }, error = function(e) {
        stage_method <<- 'greedy'
        solve_greedy()
      })
      setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
    } else {
      sol <- solve_greedy()
    }

    list(
      tech = t,
      supply_chain = sc,
      specialization = sol$specialization,
      flows = sol$flows,
      objective = sol$objective,
      hhi = sol$hhi,
      n_producers = sol$n_producers,
      method_used = stage_method,
      target_exports_usd = target_total_usd,
      allies_cap0_usd = allies_cap0_usd,
      allies_capMax_usd = allies_capMax_usd,
      feasible_caps = feasible_caps,
      unmet_demand_usd = dplyr::coalesce(sol$unmet_demand_usd, NA_real_)
    )
  }

  run_all_stages <- function(indices) {
    out <- vector('list', stage_count)
    if (is.null(indices)) indices <- seq_len(stage_count)
    for (i in indices) {
      if (is.function(progress_callback)) {
        progress_callback(list(event = 'start_stage', current = i, total = stage_count, tech = stages$tech[[i]], supply_chain = stages$supply_chain[[i]], method = use_method))
      }
      out[[i]] <- solve_one_stage(i)
      if (is.function(progress_callback)) {
        elapsed_done <- as.numeric(difftime(Sys.time(), start_time, units = 'secs'))
        done_count <- i
        eta_done <- if (done_count <= 0) NA_real_ else (elapsed_done / done_count) * (stage_count - done_count)
        progress_callback(list(event = 'end_stage', current = i, total = stage_count, tech = stages$tech[[i]], supply_chain = stages$supply_chain[[i]], elapsed_sec = elapsed_done, eta_sec = eta_done, pct_complete = done_count / stage_count, pct_remaining = 1 - (done_count / stage_count), method = out[[i]]$method_used))
      }
    }
    out
  }

  results <- run_all_stages(seq_len(stage_count))

  portfolio_converged <- TRUE
  portfolio_iters <- 0
  portfolio_violations_remaining <- 0
  counts_tbl <- NULL
  topk_tbl <- tibble::tibble()

  if (isTRUE(portfolio_enable)) {
    for (iter in seq_len(portfolio_max_iters)) {
      portfolio_iters <- iter
      specialization_tbl_iter <- purrr::map_dfr(results, function(x) {
        if (is.null(x$specialization)) return(NULL)
        x$specialization %>% dplyr::mutate(tech = x$tech, supply_chain = x$supply_chain) %>% dplyr::relocate(tech, supply_chain, .before = iso3c)
      })
      topk_tbl <- allied_network_build_topk_tbl(specialization_tbl_iter, portfolio_top_k = portfolio_top_k)

      counts_tbl <- topk_tbl %>%
        dplyr::filter(in_top_k) %>%
        dplyr::count(iso3c, name = 'topk_count') %>%
        dplyr::right_join(tibble::tibble(iso3c = iso3c_network), by = 'iso3c') %>%
        dplyr::mutate(topk_count = dplyr::coalesce(topk_count, 0L), portfolio_cap = as.integer(cap_by_iso[iso3c]), over_by = pmax(0L, topk_count - portfolio_cap))

      offenders <- counts_tbl %>% dplyr::filter(over_by > 0)
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
          dplyr::filter(in_top_k, iso3c == offender_iso) %>%
          dplyr::left_join(
            specialization_tbl_iter %>% dplyr::filter(iso3c == offender_iso) %>% dplyr::select(tech, supply_chain, producer_score, dplyr::any_of('production_usd')),
            by = c('tech', 'supply_chain')
          ) %>%
          dplyr::mutate(drop_priority = dplyr::coalesce(production_usd, production_share) * producer_score) %>%
          dplyr::arrange(drop_priority)
        if (!nrow(cand)) next
        drops <- utils::head(cand, over)
        for (k in seq_len(nrow(drops))) {
          stg_key <- paste(drops$tech[[k]], drops$supply_chain[[k]], sep = '||')
          producer_forbidden_by_stage[[stg_key]] <- unique(c(producer_forbidden_by_stage[[stg_key]], offender_iso))
          impacted <- unique(c(impacted, match(stg_key, stage_keys)))
        }
      }

      if (!length(impacted)) break
      for (idx in impacted) results[[idx]] <- solve_one_stage(idx)
      if (isTRUE(portfolio_verbose)) message('portfolio iteration ', iter, ': re-solved ', length(impacted), ' stages')
    }

    topk_tbl <- allied_network_build_topk_tbl(
      purrr::map_dfr(results, function(x) if (is.null(x$specialization)) NULL else x$specialization %>% dplyr::mutate(tech = x$tech, supply_chain = x$supply_chain) %>% dplyr::relocate(tech, supply_chain, .before = iso3c)),
      portfolio_top_k = portfolio_top_k
    )
    counts_tbl <- topk_tbl %>%
      dplyr::filter(in_top_k) %>%
      dplyr::count(iso3c, name = 'topk_count') %>%
      dplyr::right_join(tibble::tibble(iso3c = iso3c_network), by = 'iso3c') %>%
      dplyr::mutate(topk_count = dplyr::coalesce(topk_count, 0L), portfolio_cap = as.integer(cap_by_iso[iso3c]), over_by = pmax(0L, topk_count - portfolio_cap))
    portfolio_violations_remaining <- sum(counts_tbl$over_by > 0, na.rm = TRUE)
    if (portfolio_violations_remaining == 0) portfolio_converged <- TRUE
  }

  specialization_tbl <- purrr::map_dfr(results, function(x) {
    if (is.null(x$specialization)) return(NULL)
    x$specialization %>% dplyr::mutate(tech = x$tech, supply_chain = x$supply_chain) %>% dplyr::relocate(tech, supply_chain, .before = iso3c)
  })

  flows_tbl <- purrr::map_dfr(results, function(x) {
    if (is.null(x$flows)) return(NULL)
    x$flows %>% dplyr::mutate(tech = x$tech, supply_chain = x$supply_chain) %>% dplyr::relocate(tech, supply_chain, .before = reporter_iso)
  })

  diagnostics_tbl <- purrr::map_dfr(results, function(x) {
    out <- tibble::tibble(tech = x$tech, supply_chain = x$supply_chain, method = dplyr::coalesce(x$method_used, use_method), n_producers = x$n_producers, hhi = x$hhi, objective = x$objective)
    if (identical(scale_mode, 'usd_target_match_china')) {
      out <- out %>% dplyr::mutate(target_exports_usd = x$target_exports_usd, allies_cap0_usd = x$allies_cap0_usd, allies_capMax_usd = x$allies_capMax_usd, feasible_caps = x$feasible_caps, unmet_demand_usd = x$unmet_demand_usd)
    }
    out
  })

  if (isTRUE(portfolio_enable) && nrow(specialization_tbl)) {
    specialization_tbl <- specialization_tbl %>%
      dplyr::left_join(counts_tbl %>% dplyr::select(iso3c, portfolio_topk_count = topk_count, portfolio_cap), by = 'iso3c')
    diagnostics_tbl <- diagnostics_tbl %>%
      dplyr::mutate(
        portfolio_converged = portfolio_converged,
        portfolio_iters = portfolio_iters,
        portfolio_violations_remaining = portfolio_violations_remaining
      )
  }

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
      dplyr::select(tech, supply_chain, iso3c, country, dev_potential, producer_score, dplyr::any_of('connectivity_out'), selected, production_share, build_priority)
  }

  out <- list(
    params = list(
      iso3c_network = iso3c_network,
      node_weights = node_weights,
      edge_weights = edge_weights,
      method = use_method,
      scale_mode = scale_mode,
      build_cost_mode = build_cost_mode,
      build_cost_weight = build_cost_weight,
      demand_mode = demand_mode,
      demand_weights = demand_weights,
      size_transform = size_transform,
      portfolio_enable = portfolio_enable,
      portfolio_top_k = portfolio_top_k,
      portfolio_min_cap = portfolio_min_cap,
      portfolio_max_cap = portfolio_max_cap,
      portfolio_transform = portfolio_transform,
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

  if (isTRUE(portfolio_enable)) {
    out$portfolio <- list(
      caps = caps_tbl,
      counts = counts_tbl,
      topk = topk_tbl,
      converged = portfolio_converged,
      iters = portfolio_iters,
      violations_remaining = portfolio_violations_remaining
    )
  }

  out
}
