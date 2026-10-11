# Dashboard parameters: the four dials and the advanced settings mapped to the pipeline's weights
# (pure; no IO). Used by scripts/85 (the data export of the Allied Industrial Base Explorer), the tests
# and, ported to TypeScript, by the explorer itself (dashboard/src/engine/params.ts).
#
# Every dial runs 0-100, and 50 is the paper's baseline. A dial d becomes the multiplier
#
#   m(d) = 4^((d - 50) / 50)       m(0) = 0.25, m(25) = 0.5, m(50) = 1, m(75) = 2, m(100) = 4
#
# which scales the weights the dial owns (mH, mB, mP, mG below). Baseline values are never
# written here: they come from bundle$manifest$baseline_inputs, which scripts/85 reads off the
# pipeline outputs and config. Each formula returns the baseline value bit for bit when m = 1:
# weights are multiplied (x * 1 == x exactly), the Strategic Index weights are renormalised only
# when some multiplier differs from 1, and the stage-weight contrast is skipped at mP = 1.
#
# HAWKISHNESS (mH): SI weight on security need (1 - ES); Friendshore weight in the PSI; Friendshore
#   weight of network edges; geo_alignment weight inside Friendshore; network diversification
#   lambda; need weight of network demand; resilience (own ES) weight of the producer score.
# BULLISHNESS (mB): SI weight on EO; Opportunity weight in the PSI and of network edges; EO weight
#   of the producer score; size weight of network demand.
# POPULISM (mP): SI policy weight; IEA PAMS weight in the Policy composite; stage weights
#   lambda_s = lambda_max (lambda0_s / lambda_max)^mP (Midstream emphasis); the policy term of
#   the producer score, which the paper leaves out (weight 0.3 max(0, (P - 50) / 50), switched on
#   only above 50); a home-bias multiplier on the self-supply edge of every network stage.
# GREENNESS (mG): SI technology (GHG) weight; CAT weight in the Policy composite; the climate
#   penalties of Friendshore (per side) and Opportunity; fossil technologies (Coal, Oil, Gas) are
#   dropped from the Strategic Index, the partnership best-five and the network when G >= 90
#   (advanced setting fossil = "auto"; "never" / "always" override).
#
# No dial owns the endowment screen of the allied picks: the advanced setting require_resource_base
# (default TRUE, the paper's) bars an Upstream stage without recorded reserves or production from a
# member's allied pick (params$si$require_resource_base; dash_allied_selection()).
#
# Weight sets other than the Strategic Index are ratios inside the pipeline functions that use
# them (each divides by the sum of the weights present), so a dial scales one weight and the
# others' shares follow.

DASH_DIAL_NAMES <- c("hawkishness", "bullishness", "populism", "greenness")
DASH_DIAL_CODES <- c(hawkishness = "H", bullishness = "B", populism = "P", greenness = "G")

dash_or <- function(a, b) if (is.null(a)) b else a

# Multiplier of a dial setting (vectorised).
dash_dial_multiplier <- function(d) {
  d <- suppressWarnings(as.numeric(d))
  if (!length(d) || any(!is.finite(d)) || any(d < 0 | d > 100)) {
    stop("Dial settings must be numbers in [0, 100].")
  }
  4^((d - 50) / 50)
}

dash_default_dials <- function() {
  list(hawkishness = 50, bullishness = 50, populism = 50, greenness = 50)
}

# Dials as a named list of four doubles; missing dials take 50.
dash_check_dials <- function(dials) {
  if (is.null(dials)) dials <- list()
  if (!is.list(dials)) dials <- as.list(dials)
  unknown <- setdiff(names(dials), DASH_DIAL_NAMES)
  if (length(unknown)) stop("Unknown dial(s): ", paste(unknown, collapse = ", "), ".")
  out <- dash_default_dials()
  for (nm in names(dials)) {
    v <- suppressWarnings(as.numeric(dials[[nm]]))
    if (length(v) != 1 || !is.finite(v) || v < 0 || v > 100) {
      stop("Dial ", nm, " must be one number in [0, 100].")
    }
    out[[nm]] <- v
  }
  out[DASH_DIAL_NAMES]
}

# The advanced panel's defaults (the paper's settings).
dash_default_advanced <- function(bundle) {
  bi <- dash_baseline_inputs(bundle)
  list(
    es_weights = bi$es_weights,
    eo_weights = bi$eo_weights,
    tau = bi$tau,
    fossil = "auto",
    # the endowment screen of the allied picks (select_allied_strategic_sectors(require_resource_base));
    # on unless the build says otherwise
    require_resource_base = !isFALSE(bi$require_resource_base),
    psi_view = bi$psi_view,
    network = list(
      lambda = NA_real_,
      max_share = bi$max_share,
      min_producers = as.integer(bi$min_producers),
      portfolio_enable = isTRUE(bi$portfolio$enable)
    )
  )
}

dash_baseline_inputs <- function(bundle) {
  bi <- bundle$manifest$baseline_inputs
  if (is.null(bi)) stop("bundle$manifest$baseline_inputs is missing; rebuild the bundle with scripts/85.")
  bi
}

# Category weights from the advanced panel: named non-negative numbers over the baseline's
# categories, in the baseline's order (a category left out keeps its baseline weight).
dash_check_category_weights <- function(w, w0, label) {
  if (is.null(w)) return(w0)
  w <- unlist(w)
  if (is.null(names(w)) || any(!nzchar(names(w)))) stop(label, " must be a named numeric vector.")
  unknown <- setdiff(names(w), names(w0))
  if (length(unknown)) stop(label, ": unknown categories ", paste(unknown, collapse = ", "), ".")
  out <- w0
  v <- suppressWarnings(as.numeric(w))
  if (any(!is.finite(v)) || any(v < 0)) stop(label, " must be finite and >= 0.")
  out[names(w)] <- v
  if (sum(out) <= 0) stop(label, ": at least one category needs a positive weight.")
  storage.mode(out) <- "double"
  out
}

dash_check_advanced <- function(advanced, bundle) {
  bi <- dash_baseline_inputs(bundle)
  def <- dash_default_advanced(bundle)
  if (is.null(advanced)) return(def)
  if (!is.list(advanced)) stop("advanced must be a list (see dash_default_advanced()).")
  unknown <- setdiff(names(advanced), names(def))
  if (length(unknown)) stop("Unknown advanced setting(s): ", paste(unknown, collapse = ", "), ".")
  out <- def
  out$es_weights <- dash_check_category_weights(advanced$es_weights, bi$es_weights, "es_weights")
  out$eo_weights <- dash_check_category_weights(advanced$eo_weights, bi$eo_weights, "eo_weights")
  if ("tau" %in% names(advanced)) {
    tau <- advanced$tau
    if (is.null(tau) || (length(tau) == 1 && is.na(tau))) {
      out$tau <- NA_real_
    } else {
      tau <- suppressWarnings(as.numeric(tau))
      if (length(tau) != 1 || !is.finite(tau) || tau < 0 || tau > 1) stop("tau must be NA or one number in [0, 1].")
      out$tau <- tau
    }
  }
  if (!is.null(advanced$fossil)) {
    fossil <- as.character(advanced$fossil)
    if (length(fossil) != 1 || !fossil %in% c("auto", "never", "always")) stop("fossil must be 'auto', 'never' or 'always'.")
    out$fossil <- fossil
  }
  if (!is.null(advanced$require_resource_base)) {
    rrb <- advanced$require_resource_base
    if (length(rrb) != 1 || is.na(as.logical(rrb))) stop("require_resource_base must be TRUE or FALSE.")
    out$require_resource_base <- isTRUE(as.logical(rrb))
  }
  if (!is.null(advanced$psi_view)) {
    view <- as.character(advanced$psi_view)
    if (length(view) != 1 || !view %in% c("coalition", "world")) stop("psi_view must be 'coalition' or 'world'.")
    out$psi_view <- view
  }
  if (!is.null(advanced$network)) {
    net <- advanced$network
    unknown <- setdiff(names(net), names(def$network))
    if (length(unknown)) stop("Unknown network setting(s): ", paste(unknown, collapse = ", "), ".")
    if ("lambda" %in% names(net)) {
      lam <- net$lambda
      if (is.null(lam) || (length(lam) == 1 && is.na(lam))) {
        out$network$lambda <- NA_real_
      } else {
        lam <- suppressWarnings(as.numeric(lam))
        if (length(lam) != 1 || !is.finite(lam) || lam < 0) stop("network lambda must be NA or one number >= 0.")
        out$network$lambda <- lam
      }
    }
    if (!is.null(net$max_share)) {
      ms <- suppressWarnings(as.numeric(net$max_share))
      if (length(ms) != 1 || !is.finite(ms) || ms <= 0 || ms > 1) stop("max_share must be in (0, 1].")
      out$network$max_share <- ms
    }
    if (!is.null(net$min_producers)) {
      mp <- suppressWarnings(as.integer(round(as.numeric(net$min_producers))))
      if (length(mp) != 1 || is.na(mp) || mp < 1) stop("min_producers must be a positive integer.")
      out$network$min_producers <- mp
    }
    if (!is.null(net$portfolio_enable)) {
      out$network$portfolio_enable <- isTRUE(as.logical(net$portfolio_enable))
    }
  }
  out
}

# Strategic Index weights under the multipliers: w_k = w0_k m_k S0 / sum_j w0_j m_j, the baseline
# weights themselves when every multiplier is 1.
dash_si_weights <- function(w0, mult) {
  mult <- mult[names(w0)]
  if (all(mult == 1)) return(w0)
  raw <- w0 * mult
  raw * (sum(w0) / sum(raw))
}

# Stage weights under the populism multiplier: lambda_s = lambda_max (lambda0_s / lambda_max)^mP,
# which keeps the largest weight (Midstream) and widens (mP > 1) or narrows (mP < 1) the gaps.
dash_stage_weights <- function(sw0, mP) {
  if (mP == 1) return(sw0)
  lmax <- max(sw0)
  lmax * (sw0 / lmax)^mP
}

# The parameter object every engine function reads.
dash_params <- function(dials = dash_default_dials(), advanced = NULL, bundle) {
  if (missing(bundle) || is.null(bundle)) stop("dash_params() needs the bundle (its baseline_inputs).")
  bi <- dash_baseline_inputs(bundle)
  dials <- dash_check_dials(dials)
  adv <- dash_check_advanced(advanced, bundle)
  m <- dash_dial_multiplier(unlist(dials[DASH_DIAL_NAMES]))
  names(m) <- unname(DASH_DIAL_CODES[DASH_DIAL_NAMES])
  mH <- m[["H"]]; mB <- m[["B"]]; mP <- m[["P"]]; mG <- m[["G"]]

  fossil_excluded <- switch(
    adv$fossil,
    auto = dials$greenness >= bi$fossil_threshold,
    never = FALSE,
    always = TRUE
  )
  exclude_techs <- if (fossil_excluded) bi$fossil_techs else character()

  # Policy composite: IEA PAMS scaled by mP, CAT by mG; any other stored component keeps its weight
  pw <- bi$policy_weights
  if (bi$policy_iea_var %in% names(pw)) pw[[bi$policy_iea_var]] <- pw[[bi$policy_iea_var]] * mP
  if (bi$policy_cat_var %in% names(pw)) pw[[bi$policy_cat_var]] <- pw[[bi$policy_cat_var]] * mG

  si_mult <- c(policy = mP, economic_opportunity = mB, energy_security_need = mH, supply_chain = 1, technology = mG)
  si <- list(
    weights = dash_si_weights(bi$si_weights, si_mult),
    stage_weights = dash_stage_weights(bi$stage_weights, mP),
    excluded_sectors = bi$excluded_sectors,
    fossil_excluded = fossil_excluded,
    fossil_techs = bi$fossil_techs,
    require_resource_base = adv$require_resource_base
  )

  fs_weights <- bi$fs_weights
  if (!is.null(fs_weights$geo_alignment)) fs_weights$geo_alignment <- fs_weights$geo_alignment * mH
  psi_weights <- bi$psi_weights
  psi_weights[["friendshore"]] <- psi_weights[["friendshore"]] * mH
  psi_weights[["opportunity"]] <- psi_weights[["opportunity"]] * mB
  partner <- list(
    fs_weights = fs_weights,
    fs_penalty = bi$fs_penalty * mG,
    po_weights = bi$po_weights,
    po_penalty = bi$po_penalty * mG,
    psi_weights = psi_weights,
    psi_view = adv$psi_view,
    psi_tau = bi$psi_tau,
    top_k = as.integer(bi$top_k),
    exclude_techs = exclude_techs
  )

  # Producer score: EO (mB) and resilience (mH); the policy term ramps in above P = 50 when the
  # baseline leaves it out, else it scales with mP
  nw <- bi$node_weights
  node_policy <- isTRUE(bi$node_policy)
  ramp <- max(0, (dials$populism - 50) / 50)
  if (node_policy) {
    nw$policy <- nw$policy * mP
  } else if (ramp > 0) {
    node_policy <- TRUE
    nw$policy <- bi$node_policy_weight_max * ramp
  }
  nw$eo <- nw$eo * mB
  nw$resilience <- nw$resilience * mH

  lambda <- if (is.na(adv$network$lambda)) bi$lambda * mH else adv$network$lambda
  network <- list(
    node_weights = nw[c("eo", "policy", "resilience")],
    node_policy = node_policy,
    edge_weights = list(
      friendshore = bi$edge_weights$friendshore * mH,
      opportunity = bi$edge_weights$opportunity * mB
    ),
    demand_weights = list(
      need = bi$demand_weights$need * mH,
      size = bi$demand_weights$size * mB
    ),
    diversity_lambda = lambda,
    self_multiplier = mP,
    max_share = adv$network$max_share,
    min_share = bi$min_share,
    min_producers = adv$network$min_producers,
    min_suppliers_per_consumer = bi$min_suppliers_per_consumer,
    epsilon_supplier_share = bi$epsilon_supplier_share,
    w_node = bi$w_node,
    w_edge = bi$w_edge,
    w_dev = bi$w_dev,
    allow_self = bi$allow_self,
    portfolio = utils::modifyList(bi$portfolio, list(enable = adv$network$portfolio_enable)),
    diversity_breakpoint_step = bi$diversity_breakpoint_step,
    supply_chains = bi$supply_chains,
    exclude_techs = exclude_techs,
    milp_time_limit = bi$milp_time_limit,
    policy_missing = bi$policy_missing,
    missing_components = bi$missing_components,
    demand_mode = bi$demand_mode,
    size_transform = bi$size_transform
  )

  out <- list(
    dials = dials,
    m = m,
    advanced = adv,
    pillars = list(es_weights = adv$es_weights, eo_weights = adv$eo_weights, tau = adv$tau),
    policy = list(weights = pw),
    si = si,
    partner = partner,
    network = network
  )
  class(out) <- "dash_params"
  out
}

print.dash_params <- function(x, ...) {
  cat("<dash_params> dials: ", paste(names(x$dials), unlist(x$dials), sep = " = ", collapse = ", "), "\n", sep = "")
  cat("  SI weights: ", paste(names(x$si$weights), signif(x$si$weights, 3), sep = " = ", collapse = ", "), "\n", sep = "")
  invisible(x)
}

# TRUE when params are the paper's baseline (all dials at 50, advanced at defaults).
dash_is_baseline <- function(params, bundle) {
  base <- bundle$manifest$baseline
  if (is.null(base)) base <- dash_params(bundle = bundle)
  identical(unclass(params), unclass(base))
}

# The subset of the parameters a recompute layer reads.
dash_params_layer <- function(params, layer = c("pillars", "policy", "si", "partner_cells", "fs_cells", "po_cells",
                                                "psi", "net_inputs", "net_score", "net_solve", "all")) {
  layer <- match.arg(layer)
  p <- unclass(params)
  fs <- p$partner[c("fs_weights", "fs_penalty")]
  po <- p$partner[c("po_weights", "po_penalty")]
  net_inputs <- list(fs = fs, po = po, stages = p$network[c("supply_chains", "exclude_techs")])
  net_score <- c(net_inputs, list(
    policy = if (isTRUE(p$network$node_policy)) p$policy else NULL,
    score = p$network[c("node_weights", "node_policy", "edge_weights", "demand_weights", "self_multiplier",
                        "policy_missing", "missing_components", "demand_mode", "size_transform")]
  ))
  switch(
    layer,
    pillars = p$pillars,
    policy = p$policy,
    si = list(p$pillars, p$policy, p$si),
    fs_cells = fs,
    po_cells = po,
    partner_cells = list(fs = fs, po = po),
    psi = list(fs = fs, po = po, p$partner[c("psi_weights", "psi_view", "psi_tau", "top_k", "exclude_techs")]),
    net_inputs = net_inputs,
    net_score = net_score,
    net_solve = c(net_score, list(solve = p$network[setdiff(names(p$network), names(net_score$score))])),
    all = p
  )
}

# Cache key of a layer: a digest of the parameters it reads (digest::digest when installed, else
# the deparsed parameters themselves).
dash_params_key <- function(params, layer = "all") {
  x <- dash_params_layer(params, layer)
  if (requireNamespace("digest", quietly = TRUE)) {
    paste0(layer, ":", digest::digest(x, algo = "xxhash64"))
  } else {
    paste0(layer, ":", paste(deparse(x, control = c("keepNA", "digits17")), collapse = ""))
  }
}

# One row per (dial, parameter): the baseline value, the current value and their ratio, with a
# short text for the sidebar; `salient` marks the 2-3 rows worth showing under each slider.
dash_dial_readout <- function(params, bundle) {
  bi <- dash_baseline_inputs(bundle)
  base <- bundle$manifest$baseline
  if (is.null(base)) base <- dash_params(bundle = bundle)
  share <- function(w, k) {
    w <- unlist(w)
    if (is.null(w[k]) || is.na(w[k])) return(NA_real_)
    w[[k]] / sum(w, na.rm = TRUE)
  }
  eff_node <- function(p, k) {
    nw <- unlist(p$network$node_weights)
    if (!isTRUE(p$network$node_policy)) nw[["policy"]] <- 0
    nw[[k]] / sum(nw)
  }
  pol_share <- function(p, var) {
    w <- p$policy$weights
    if (!var %in% names(w)) return(NA_real_)
    w[[var]] / sum(w)
  }
  stage_ratio <- function(p, s) p$si$stage_weights[[s]] / p$si$stage_weights[["Midstream"]]
  rows <- list(
    list("hawkishness", "si_need", "SI weight on security need", function(p) p$si$weights[["energy_security_need"]], TRUE),
    list("hawkishness", "psi_fs_share", "Friendshore share of the PSI", function(p) share(p$partner$psi_weights, "friendshore"), TRUE),
    list("hawkishness", "edge_fs_share", "Friendshore share of network edges", function(p) share(p$network$edge_weights, "friendshore"), FALSE),
    list("hawkishness", "fs_geo_share", "Alignment share of Friendshore", function(p) share(p$partner$fs_weights, "geo_alignment"), FALSE),
    list("hawkishness", "lambda", "Network diversification lambda", function(p) p$network$diversity_lambda, TRUE),
    list("hawkishness", "demand_need_share", "Need share of network demand", function(p) share(p$network$demand_weights, "need"), FALSE),
    list("hawkishness", "node_resilience", "Resilience weight in producer score", function(p) eff_node(p, "resilience"), FALSE),
    list("bullishness", "si_eo", "SI weight on economic opportunity", function(p) p$si$weights[["economic_opportunity"]], TRUE),
    list("bullishness", "psi_po_share", "Opportunity share of the PSI", function(p) share(p$partner$psi_weights, "opportunity"), TRUE),
    list("bullishness", "edge_po_share", "Opportunity share of network edges", function(p) share(p$network$edge_weights, "opportunity"), TRUE),
    list("bullishness", "node_eo", "EO weight in producer score", function(p) eff_node(p, "eo"), FALSE),
    list("bullishness", "demand_size_share", "Size share of network demand", function(p) share(p$network$demand_weights, "size"), FALSE),
    list("populism", "si_policy", "SI weight on policy", function(p) p$si$weights[["policy"]], TRUE),
    list("populism", "policy_iea_share", "IEA PAMS share of the Policy index", function(p) pol_share(p, bi$policy_iea_var), FALSE),
    list("populism", "stage_up_ratio", "Upstream / Midstream stage weight", function(p) stage_ratio(p, "Upstream"), TRUE),
    list("populism", "stage_down_ratio", "Downstream / Midstream stage weight", function(p) stage_ratio(p, "Downstream"), FALSE),
    list("populism", "node_policy", "Policy weight in producer score", function(p) eff_node(p, "policy"), FALSE),
    list("populism", "home_bias", "Home-bias multiplier on self-supply", function(p) p$network$self_multiplier, TRUE),
    list("greenness", "si_tech", "SI weight on technology (GHG)", function(p) p$si$weights[["technology"]], TRUE),
    list("greenness", "policy_cat_share", "CAT share of the Policy index", function(p) pol_share(p, bi$policy_cat_var), FALSE),
    list("greenness", "fs_penalty", "Friendshore climate penalty (per side)", function(p) p$partner$fs_penalty, TRUE),
    list("greenness", "po_penalty", "Opportunity climate penalty", function(p) p$partner$po_penalty, FALSE),
    list("greenness", "fossil", "Fossil technologies excluded", function(p) as.numeric(isTRUE(p$si$fossil_excluded)), TRUE)
  )
  out <- lapply(rows, function(r) {
    b <- r[[4]](base)
    cur <- r[[4]](params)
    ratio <- if (is.finite(b) && b != 0) cur / b else NA_real_
    text <- if (r[[2]] == "fossil") {
      if (cur > 0) "Coal, Oil and Gas excluded" else "Fossil technologies included"
    } else if (r[[2]] == "home_bias") {
      sprintf("%s x%.2f", r[[3]], cur)
    } else if (is.finite(ratio) && abs(ratio - 1) > 1e-9) {
      sprintf("%s %.3g (x%.2f vs %.3g)", r[[3]], cur, ratio, b)
    } else if (!is.finite(ratio) && is.finite(cur) && cur != b) {
      sprintf("%s %.3g (paper: %.3g)", r[[3]], cur, b)
    } else {
      sprintf("%s %.3g (paper)", r[[3]], cur)
    }
    tibble::tibble(
      dial = r[[1]], parameter = r[[2]], label = r[[3]],
      baseline = b, current = cur, multiplier = ratio, text = text, salient = r[[5]]
    )
  })
  dplyr::bind_rows(out)
}

# Named dial presets.
dash_presets <- function() {
  base <- dash_default_dials()
  set <- function(...) utils::modifyList(base, list(...))
  list(
    "Paper baseline" = base,
    "Hawk" = set(hawkishness = 90),
    "Bull" = set(bullishness = 90),
    "Populist" = set(populism = 90),
    "Green" = set(greenness = 90),
    "Green hawk" = set(hawkishness = 80, greenness = 80)
  )
}

# The dials and advanced settings of a params object, for saving a scenario (e.g. as JSON).
dash_params_spec <- function(params) {
  list(dials = params$dials, advanced = params$advanced)
}
