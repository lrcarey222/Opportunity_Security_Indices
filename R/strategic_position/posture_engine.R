# Strategic posture engine: the Secure override of the canonical decision (docs/strategic_position.md,
# "Choice"), plus the engine's own Compete tests kept as diagnostics (engine_posture). The canonical
# choice outside Secure is decision_engine.R. It ports the NEIS Yards & Fences decision engine
# (dashboard/src/fence/engine: gates.ts, rules.ts, classify.ts) to R and applies it to every country x
# technology x six-stage node and to the sensitive layers inside a cell:
#
#   secure  = small yard   (acute consequence AND one of three routes: operational control through a
#                           command path or a dual-use node, a coercive physical chokepoint, or systemic
#                           resilience of an essential, import-dependent, slow-to-rebuild node)
#   compete = medium yard  (a capability case with a credible strategy that passes proportionality)
#   deploy  = large yard   (otherwise: open, affordable deployment is the strategic conclusion)
# The engine's Compete reading (Tests 3-5) is kept as a diagnostic (engine_posture, engine_variant): the
# recommended choice outside Secure comes from decision_engine.R (Need x Prize triage, Competitive
# Position decides the response; deployment_need at Deployment). Test 3's lower bound
# (t3_capability_lo) is the capability floor for Preserve there.
#
# Every test is tri-state (yes / no / unknown). Unknown inputs are bracketed, never filled: the
# recommendation is the strict reading (an unproven case for intervention is not met), the corners of
# the unknowns give `posture_possible`, and single-input resolution gives `pivotal_inputs`. There is no
# score anywhere in this file. Observed policy is not an input: Revealed Policy Priority is compared
# with the recommendation afterwards (sp_policy_alignment()).
#
# Pure functions: no IO. Needs R/utils/supply_chain_stages.R and R/strategic_position/sp_helpers.R.

# ---- Vocabulary ------------------------------------------------------------------------------------
# Ordered from the open (or weak) end to the security (or strong) end, as dashboard/src/fence/engine/
# types.ts DOMAINS.
SP_POSTURE_DOMAINS <- list(
  consequence = c("low", "moderate", "high", "severe"),
  time_horizon = c("years", "months", "weeks", "days"),
  digitally_active = c("no", "yes"),
  remote_control = c("no", "limited", "yes"),
  separability = c("separable", "partial", "inseparable"),
  substitutability = c("easy", "moderate", "hard", "impossible"),
  qualification_time = c("lt3m", "m3to12", "y1to3", "gt3y"),
  reconstitution_time = c("lt6m", "m6to12", "y1to3", "gt3y"),
  stockpile = c("gt12m", "m3to12", "lt3m", "none"),
  process_knowledge = c("no", "partly", "yes"),
  workforce_suppliers = c("no", "partly", "yes"),
  adjacent_innovation = c("no", "partly", "yes"),
  complementary = c("no", "partly", "yes"),
  beyond_novelty = c("no", "partly", "yes"),
  cost_perf_advantage = c("no", "partly", "yes"),
  incumbent_vulnerable = c("no", "partly", "yes"),
  scale_pathway = c("no", "partly", "yes")
)

# Inputs whose unknowns are bracketed on the security axis and on the capability axis
# (classify.ts SECURITY_IDS / CAPABILITY_IDS). The numeric ones are data.
SP_POSTURE_SECURITY_INPUTS <- c(
  "consequence", "time_horizon", "digitally_active", "remote_control", "substitutability",
  "qualification_time", "reconstitution_time", "stockpile", "exposed", "coercive_top_share",
  "coercive_import_share", "trusted_world_share"
)
SP_POSTURE_CAPABILITY_INPUTS <- c(
  "home_share", "process_knowledge", "workforce_suppliers", "adjacent_innovation", "complementary",
  "beyond_novelty", "cost_perf_advantage", "incumbent_vulnerable", "scale_pathway", "market_pct",
  "premium_pct", "adjacent_established", "cp_within_country_pct"
)
SP_POSTURE_NUMERIC <- c("coercive_top_share", "coercive_import_share", "trusted_world_share", "home_share", "market_pct", "premium_pct")
SP_POSTURE_LOGICAL <- c("exposed", "adjacent_established")

# Security routes (config/security_route_rules.csv, sp_security_rules()). Optional columns of the
# input frame: sr_rationale (the routes a node may take, ";"-separated; "" = none), dual_use,
# systemic_essentiality and sn_import_dependence. A node without a rule (sr_rationale NA) keeps the two
# original routes (the digital path and the physical chokepoint) and has no resilience route.
SP_SECURITY_RATIONALES <- c("operational_control", "coercive_chokepoint", "systemic_resilience")
SP_POSTURE_ROUTE_INPUTS <- c("systemic_essentiality", "dual_use", "sn_import_dependence")

SP_POSTURES <- c("secure", "compete", "deploy")
SP_POSTURE_VARIANTS <- c(
  "secure_control", "secure_integrated", "secure_whole", "secure_unclear", "secure_input", "secure_both",
  "secure_dual_use", "secure_resilience",
  "compete_preserve", "compete_adjacent_capability", "compete_catch_up", "compete_leap_ahead", "compete_costly",
  "compete_partner",
  "deploy", "deploy_floor", "deploy_diversify"
)
SP_COMPETE_STRATEGIES <- c("preserve", "adjacent_capability", "catch_up", "leap_ahead")

# Producing stages adjacent to each stage in the same technology (for complementary capabilities and
# the adjacent-capability strategy). Deployment is where a technology is used, so it is nobody's
# producing neighbour; Recovery feeds Refining.
SP_STAGE_ADJACENT <- list(
  Extraction = "Refining",
  Refining = c("Extraction", "Components", "Recovery"),
  Components = c("Refining", "Assembly"),
  Assembly = "Components",
  Deployment = character(),
  Recovery = "Refining"
)

# ---- Tri-state helpers -----------------------------------------------------------------------------
sp_tri <- function(x) {
  out <- ifelse(is.na(x), "unknown", ifelse(x, "yes", "no"))
  as.character(out)
}
sp_and3 <- function(...) {
  xs <- list(...)
  n <- max(lengths(xs))
  xs <- lapply(xs, rep_len, n)
  any_no <- Reduce(`|`, lapply(xs, function(x) x == "no"))
  all_yes <- Reduce(`&`, lapply(xs, function(x) x == "yes"))
  ifelse(any_no, "no", ifelse(all_yes, "yes", "unknown"))
}
sp_or3 <- function(...) {
  xs <- list(...)
  n <- max(lengths(xs))
  xs <- lapply(xs, rep_len, n)
  any_yes <- Reduce(`|`, lapply(xs, function(x) x == "yes"))
  all_no <- Reduce(`&`, lapply(xs, function(x) x == "no"))
  ifelse(any_yes, "yes", ifelse(all_no, "no", "unknown"))
}

# Categorical value to its position in the domain (1 = open/weak end); NA stays NA.
sp_level <- function(x, id) match(x, SP_POSTURE_DOMAINS[[id]])

# The value at the open/weak (`low`) or security/strong (`high`) end of an input's range.
sp_posture_extreme <- function(id, end = c("low", "high")) {
  end <- match.arg(end)
  if (id %in% names(SP_POSTURE_DOMAINS)) {
    d <- SP_POSTURE_DOMAINS[[id]]
    return(if (end == "high") d[length(d)] else d[1])
  }
  switch(id,
    # more coercive share and less trusted supply push toward Secure
    coercive_top_share = , coercive_import_share = , home_share = , market_pct = , cp_within_country_pct = if (end == "high") 1 else 0,
    trusted_world_share = if (end == "high") 0 else 1,
    premium_pct = if (end == "high") 0 else 75,
    exposed = , adjacent_established = end == "high",
    systemic_essentiality = if (end == "high") "high" else "low",
    dual_use = if (end == "high") "yes" else "no",
    sn_import_dependence = if (end == "high") 1 else 0,
    stop("sp_posture_extreme(): unknown input ", id)
  )
}

# ---- Input checks ----------------------------------------------------------------------------------
SP_POSTURE_REQUIRED <- c(
  "Country", "iso3", "tech", "supply_chain", "layer_id", "layer_type",
  names(SP_POSTURE_DOMAINS)[names(SP_POSTURE_DOMAINS) != "complementary"],
  "complementary", "maturity", "leap_candidate", "deployment_critical", "market_scope",
  SP_POSTURE_NUMERIC, SP_POSTURE_LOGICAL
)

# Normalises the engine input frame: "Unknown" and blanks to NA, vocabulary checks, typed numerics.
sp_posture_check_inputs <- function(x) {
  missing <- setdiff(SP_POSTURE_REQUIRED, names(x))
  if (length(missing) > 0) stop("sp_posture_check_inputs(): missing column(s) ", paste(missing, collapse = ", "))
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  for (id in names(SP_POSTURE_DOMAINS)) {
    v <- as.character(x[[id]])
    v[is.na(v) | v == "" | tolower(v) == "unknown"] <- NA_character_
    bad <- setdiff(unique(v[!is.na(v)]), SP_POSTURE_DOMAINS[[id]])
    if (length(bad) > 0) stop("sp_posture_check_inputs(): ", id, " has unknown value(s) ", paste(bad, collapse = ", "))
    x[[id]] <- v
  }
  for (id in c("maturity", "leap_candidate", "deployment_critical", "market_scope", "layer_type")) {
    v <- as.character(x[[id]])
    v[is.na(v) | v == "" | tolower(v) == "unknown"] <- NA_character_
    x[[id]] <- v
  }
  for (id in SP_POSTURE_NUMERIC) x[[id]] <- suppressWarnings(as.numeric(x[[id]]))
  for (id in SP_POSTURE_LOGICAL) x[[id]] <- as.logical(x[[id]])
  # optional route inputs (security_route_rules); a blank rationale means no route, NA means no rule
  route_vocab <- list(systemic_essentiality = c("low", "moderate", "high", "n_a"), dual_use = c("no", "yes"))
  for (id in names(route_vocab)) {
    v <- if (id %in% names(x)) as.character(x[[id]]) else rep(NA_character_, nrow(x))
    v[is.na(v) | v == "" | tolower(v) == "unknown"] <- NA_character_
    bad <- setdiff(unique(v[!is.na(v)]), route_vocab[[id]])
    if (length(bad) > 0) stop("sp_posture_check_inputs(): ", id, " has unknown value(s) ", paste(bad, collapse = ", "))
    x[[id]] <- v
  }
  x$sr_rationale <- if ("sr_rationale" %in% names(x)) as.character(x$sr_rationale) else NA_character_
  bad <- setdiff(unlist(strsplit(x$sr_rationale[!is.na(x$sr_rationale)], ";", fixed = TRUE)), SP_SECURITY_RATIONALES)
  if (length(bad) > 0) stop("sp_posture_check_inputs(): sr_rationale has unknown route(s) ", paste(unique(bad), collapse = ", "))
  x$sn_import_dependence <- if ("sn_import_dependence" %in% names(x)) suppressWarnings(as.numeric(x$sn_import_dependence)) else NA_real_
  assert_supply_chain_stages(x$supply_chain, label = "sp_posture_check_inputs() supply_chain")
  key <- paste(x$iso3, x$tech, x$supply_chain, x$layer_id, sep = "\r")
  if (anyDuplicated(key)) stop("sp_posture_check_inputs(): duplicate (iso3, tech, supply_chain, layer_id) rows.")
  x
}

# ---- Test 1: critical consequence ------------------------------------------------------------------
sp_test_consequence <- function(x, cfg) {
  acute_levels <- unlist(cfg$acute_consequence %||% c("high", "severe"))
  acute <- sp_tri(ifelse(is.na(x$consequence), NA, x$consequence %in% acute_levels))
  exposed <- sp_tri(x$exposed)
  state <- sp_and3(acute, exposed)
  detail <- dplyr::case_when(
    acute == "unknown" ~ "The consequence of disrupting this node has not been assessed.",
    acute == "no" ~ paste0("A ", x$consequence, " consequence does not reach the acute threshold, however concentrated supply is."),
    exposed == "no" ~ "The country neither imports, produces nor deploys this node materially, so its disruption has no acute consequence here.",
    exposed == "unknown" ~ paste0("Disruption would be ", x$consequence, " for a dependent country; whether this country depends on it is not established."),
    TRUE ~ paste0("Disruption would have a ", x$consequence, " consequence (", dplyr::coalesce(x$critical_function, "critical function"), ") and the country depends on the node.")
  )
  list(state = state, marginal = rep(FALSE, nrow(x)), detail = detail)
}

# ---- Test 2: exploitable vulnerability -------------------------------------------------------------
# Share against a yes line with a marginal band below it: answered yes at or above `yes`; marginal when
# within the band [marginal, yes + 0.05).
sp_share_test <- function(share, yes, marginal) {
  state <- sp_tri(ifelse(is.na(share), NA, share >= yes))
  m <- !is.na(share) & share >= marginal & share < yes + 0.05
  list(state = state, marginal = m)
}

sp_digital_applies <- function(x) {
  x$layer_type %in% "control" | (!x$layer_type %in% "stage" & x$digitally_active %in% "yes")
}

sp_test_digital <- function(x, cfg) {
  applies <- sp_digital_applies(x)
  horizon <- unlist(cfg$digital_horizon %||% c("days", "weeks", "months"))
  vend <- cfg$digital_vendor_share %||% list(threshold = 0.25, marginal = 0.15)
  da <- ifelse(x$layer_type %in% "control", "yes", sp_tri(ifelse(is.na(x$digitally_active), NA, x$digitally_active == "yes")))
  rc <- sp_tri(ifelse(is.na(x$remote_control), NA, x$remote_control != "no"))
  hz <- sp_tri(ifelse(is.na(x$time_horizon), NA, x$time_horizon %in% horizon))
  vs <- sp_share_test(x$coercive_import_share, vend$threshold, vend$marginal)
  # A country that is itself a major world supplier of the carrying hardware equips its installed
  # base mainly from its own vendors: its import mix does not describe its exposure.
  self <- sp_self_supplied(x, cfg)
  vs$state[self] <- "no"
  # the vendor mix of a few thousand dollars of imports says nothing about an installed base
  material <- sp_material_imports(x, cfg, gdp_line = FALSE)
  vs$state[material %in% FALSE & !self] <- "no"
  state <- ifelse(applies, sp_and3(da, rc, hz, vs$state), "no")
  detail <- dplyr::case_when(
    !applies ~ "Passive (or a whole-stage node): judged on the physical path; control layers are assessed separately.",
    self ~ paste0("The country holds ", sp_pct(x$home_share), " of world supply of the carrying hardware: its installed base comes mainly from its own vendors."),
    applies & material %in% FALSE ~ "The country's imports of the carrying hardware are immaterial (below the exposure floor).",
    state == "yes" ~ paste0(
      "An outside actor could ", ifelse(x$remote_control %in% "limited", "exert limited control over", "command, update or disable"),
      " this layer, and ", sp_pct(x$coercive_import_share), " of the country's supply of the carrying hardware comes from potentially coercive suppliers."
    ),
    rc == "no" ~ "No external command, update or data path has been identified.",
    hz == "no" ~ "Harm through the control layer would take years to become intolerable.",
    vs$state == "no" ~ paste0("Only ", sp_pct(x$coercive_import_share), " of the carrying hardware comes from potentially coercive suppliers: no foreign command path at scale."),
    TRUE ~ "Whether an outside actor could control this layer in this country is not yet established."
  )
  list(state = state, marginal = applies & (x$remote_control %in% "limited" | vs$marginal), detail = detail, applies = applies)
}

sp_months <- function(x, id, cfg) {
  tbl <- unlist(cfg$months[[id]])
  unname(tbl[x])
}

# Restore time minus (time to harm + stockpile cover), months, at the open (`low`) or security (`high`)
# end of every unknown input (gates.ts timingGap). Surge comes from data: trusted supply that could
# cover the loss.
sp_timing_gap <- function(x, cfg, bound = c("low", "high")) {
  bound <- match.arg(bound)
  pick <- function(id) {
    v <- sp_months(x[[id]], id, cfg)
    xs <- unlist(cfg$months[[id]])
    restore_like <- id %in% c("qualification_time", "reconstitution_time")
    sec <- if (restore_like) max(xs) else min(xs)
    open <- if (restore_like) min(xs) else max(xs)
    ifelse(is.na(v), if (bound == "high") sec else open, v)
  }
  surge <- cfg$surge_trusted_share %||% list(covers = 0.5, partial = 0.2)
  covers <- ifelse(is.na(x$trusted_world_share), bound == "low", x$trusted_world_share >= surge$covers)
  q <- pick("qualification_time")
  r <- pick("reconstitution_time")
  restore <- ifelse(covers, q, pmax(q, r))
  restore - (pick("time_horizon") + pick("stockpile"))
}

sp_test_physical <- function(x, cfg) {
  applies <- !x$layer_type %in% "control" & !(x$layer_type %in% "stage" & x$supply_chain == "Deployment")
  coer <- cfg$coercion %||% list(threshold = 0.5, marginal = 0.4)
  imp <- cfg$coercive_import_share %||% list(threshold = 0.25, marginal = 0.15)
  regional <- x$market_scope %in% "regional"
  # Regionally segmented markets (pipeline gas): the coercive suppliers' hold on the country's own
  # imports is the chokepoint; elsewhere it is the largest coercive supplier's hold on world supply.
  # The regional reading needs material imports (in value and against GDP): token imports are not
  # the country's supply.
  material <- sp_material_imports(x, cfg, gdp_line = FALSE)
  material_gdp <- sp_material_imports(x, cfg, gdp_line = TRUE)
  world_share <- ifelse(regional, ifelse(material_gdp %in% FALSE, 0, x$coercive_import_share), x$coercive_top_share)
  world <- sp_share_test(world_share, coer$threshold, coer$marginal)
  own <- sp_share_test(x$coercive_import_share, imp$threshold, imp$marginal)
  # A major world supplier of the node is not exposed through its (small) imports, and imports below
  # the exposure floor do not describe the country's supply. (Whether the country depends on the
  # node at all is Test 1's exposure condition.)
  self <- sp_self_supplied(x, cfg)
  own$state[self] <- "no"
  own$state[material %in% FALSE & !self] <- "no"
  coercion <- sp_and3(world$state, own$state)
  subst <- sp_tri(ifelse(is.na(x$substitutability), NA, x$substitutability %in% c("hard", "impossible")))
  lo <- sp_timing_gap(x, cfg, "low")
  hi <- sp_timing_gap(x, cfg, "high")
  timing <- ifelse(lo > 0, "yes", ifelse(hi <= 0, "no", "unknown"))
  timing_marginal <- timing != "unknown" & abs(lo) <= (cfg$timing_marginal_months %||% 6)
  state <- ifelse(applies, sp_and3(coercion, subst, timing), "no")
  who <- dplyr::coalesce(x$coercive_top_iso3, "the largest potentially coercive supplier")
  detail <- dplyr::case_when(
    !applies & x$layer_type %in% "control" ~ "A control layer is not withheld like a material; its risk is command and access.",
    !applies ~ "Deployment is where the technology is used: its supply vulnerability is assessed at the producing stages and in its control layers.",
    state == "yes" & regional ~ paste0(
      "Potentially coercive suppliers provide ", sp_pct(x$coercive_import_share), " of the country's own imports in a ",
      "regionally segmented market, substitution is ", x$substitutability,
      ", and trusted supply could not be restored before harm becomes intolerable."
    ),
    state == "yes" ~ paste0(
      who, " holds ", sp_pct(world_share), " of world supply, the country takes ", sp_pct(x$coercive_import_share),
      " of its imports from coercive suppliers, substitution is ", x$substitutability,
      ", and trusted supply could not be restored before harm becomes intolerable."
    ),
    self ~ paste0("The country itself holds ", sp_pct(x$home_share), " of world supply: it is not exposed through its imports."),
    regional & material_gdp %in% FALSE ~ "The country's imports in this regional market are immaterial: they are not its supply.",
    world$state == "no" & regional ~ paste0(
      "Potentially coercive suppliers provide ", sp_pct(world_share), " of the country's imports, under half: ",
      "concentration alone does not create coercive leverage."
    ),
    world$state == "no" ~ paste0(
      "No potentially coercive supplier holds half of world supply (largest ", sp_pct(world_share),
      "): concentration alone does not create coercive leverage."
    ),
    material %in% FALSE ~ "The country's imports of this node are below the exposure floor: they do not describe its supply.",
    own$state == "no" ~ paste0("Only ", sp_pct(x$coercive_import_share), " of the country's imports come from potentially coercive suppliers."),
    subst == "no" ~ paste0("Substitution is ", x$substitutability, ", so users have a way around a cutoff."),
    timing == "no" ~ "Trusted supply could be restored, or inventories bridge the gap, before harm becomes intolerable.",
    TRUE ~ "Coercive leverage cannot be confirmed or ruled out on the evidence available."
  )
  list(
    state = state, detail = detail, applies = applies,
    marginal = applies & ((world$marginal & world$state != "unknown") | (own$marginal & own$state != "unknown") |
      (x$substitutability %in% c("moderate", "hard")) | timing_marginal),
    parts = list(coercion = coercion, world = world$state, own = own$state, substitution = subst, timing = timing)
  )
}

sp_pct <- function(x) ifelse(is.na(x), "an unknown share", paste0(round(100 * x), "%"))

# ---- Test 2, by security route -----------------------------------------------------------------------
# Whether a node may take a route (its rule's rationale lists it). Without a rule the node keeps the two
# original routes and no resilience route.
sp_route_eligible <- function(x, route) {
  r <- if ("sr_rationale" %in% names(x)) x$sr_rationale else rep(NA_character_, nrow(x))
  legacy <- route %in% c("operational_control", "coercive_chokepoint")
  ifelse(is.na(r), legacy, grepl(route, r, fixed = TRUE))
}

# Route A, national-security control of a dual-use node (rule dual_use = yes): the bar is the
# coercive suppliers' share of the country's own material imports, not their hold on world supply.
sp_test_dual_use <- function(x, cfg) {
  dual <- if ("dual_use" %in% names(x)) x$dual_use else rep(NA_character_, nrow(x))
  applies <- sp_route_eligible(x, "operational_control") & dual %in% "yes" & !x$layer_type %in% "control"
  sh <- cfg$security_routes$dual_use_coercive_import_share %||% list(threshold = 0.25, marginal = 0.15)
  own <- sp_share_test(x$coercive_import_share, sh$threshold, sh$marginal)
  self <- sp_self_supplied(x, cfg)
  material <- sp_material_imports(x, cfg, gdp_line = FALSE)
  own$state[self] <- "no"
  own$state[material %in% FALSE & !self] <- "no"
  state <- ifelse(applies, own$state, "no")
  detail <- dplyr::case_when(
    !applies ~ "Not a dual-use node.",
    self ~ paste0("A dual-use node, but the country itself holds ", sp_pct(x$home_share), " of world supply."),
    material %in% FALSE ~ "A dual-use node, but the country's imports are below the exposure floor.",
    state == "yes" ~ paste0("A dual-use node with national-security end uses: ", sp_pct(x$coercive_import_share),
                            " of the country's imports come from potentially coercive suppliers."),
    state == "no" ~ paste0("A dual-use node; only ", sp_pct(x$coercive_import_share), " of the country's imports come from potentially coercive suppliers."),
    TRUE ~ "A dual-use node; the coercive share of the country's imports is not measured."
  )
  list(state = state, marginal = applies & own$marginal, detail = detail, applies = applies)
}

# Route C, systemic resilience: an essential function, substantial foreign dependency, a long lead
# time to rebuild supply and too little trusted supply to replace it. There need be no coercive
# supplier; the serious consequence is Test 1's acute consequence.
sp_test_resilience <- function(x, cfg) {
  rc <- cfg$security_routes$resilience %||% list()
  applies <- sp_route_eligible(x, "systemic_resilience") & !x$layer_type %in% "control" & x$supply_chain != "Deployment"
  ess <- if ("systemic_essentiality" %in% names(x)) x$systemic_essentiality else rep(NA_character_, nrow(x))
  essential <- sp_tri(ifelse(is.na(ess), NA, ess %in% unlist(rc$essentiality %||% "high")))
  dep_min <- rc$import_dependence_min %||% 0.5
  dep <- if ("sn_import_dependence" %in% names(x)) x$sn_import_dependence else rep(NA_real_, nrow(x))
  self <- sp_self_supplied(x, cfg)
  material <- sp_material_imports(x, cfg, gdp_line = TRUE)
  dependency <- ifelse(self, "no", sp_and3(sp_tri(material), sp_tri(ifelse(is.na(dep), NA, dep >= dep_min))))
  long_lead <- sp_tri(ifelse(is.na(x$reconstitution_time), NA, x$reconstitution_time %in% unlist(rc$long_lead_times %||% c("y1to3", "gt3y"))))
  alt_max <- rc$trusted_alternative_max %||% 0.5
  home <- dplyr::coalesce(if ("delivery_home_share" %in% names(x)) x$delivery_home_share else x$home_share, x$home_share, 0)
  partners <- pmax(x$trusted_world_share - home, 0)
  insufficient <- sp_tri(ifelse(is.na(x$trusted_world_share), NA, partners < alt_max))
  state <- ifelse(applies, sp_and3(essential, dependency, long_lead, insufficient), "no")
  detail <- dplyr::case_when(
    !applies ~ "Not a systemically essential node.",
    essential == "no" ~ "The node does not carry an essential function.",
    self ~ paste0("The country holds ", sp_pct(x$home_share), " of world supply: it is not dependent on others for it."),
    dependency == "no" ~ "The country's net import dependence is modest or its imports are immaterial.",
    long_lead == "no" ~ "Supply could be rebuilt within a year.",
    insufficient == "no" ~ paste0("Trusted partners hold ", sp_pct(partners), " of world supply: enough to replace a loss."),
    state == "yes" ~ paste0("An essential function on which the country depends heavily on imports, with supply that takes years to rebuild, ",
                            "and trusted partners hold only ", sp_pct(partners), " of world supply."),
    TRUE ~ "Whether trusted supply could replace a loss of this essential node is not established."
  )
  marginal <- applies & ((!is.na(partners) & !is.na(x$trusted_world_share) & abs(partners - alt_max) < 0.05) |
                           (!is.na(dep) & abs(dep - dep_min) < 0.05))
  list(state = state, marginal = marginal, detail = detail, applies = applies)
}

# The country's imports of the node are material: at least exposure_min_import_usd and, with
# gdp_line, exposure_material_imports_to_gdp of GDP. NA where imports are not measured.
sp_material_imports <- function(x, cfg, gdp_line = FALSE) {
  usd_min <- cfg$exposure_min_import_usd %||% 1e6
  itg_min <- cfg$exposure_material_imports_to_gdp %||% 0.0005
  usd <- if ("imports_usd" %in% names(x)) x$imports_usd else rep(NA_real_, nrow(x))
  itg <- if ("imports_to_gdp" %in% names(x)) x$imports_to_gdp else rep(NA_real_, nrow(x))
  ok <- ifelse(is.na(usd), NA, usd >= usd_min)
  if (gdp_line) ok <- ifelse(is.na(ok), NA, ok & !is.na(itg) & itg >= itg_min)
  ok
}

# The country is itself a major world supplier of the node (home share at or above self_supply_share).
sp_self_supplied <- function(x, cfg) {
  line <- cfg$self_supply_share %||% 0.10
  !is.na(x$home_share) & x$home_share >= line
}

# ---- Tests 3-5: capability case, strategy, proportionality ------------------------------------------
sp_learning_score <- function(x, bound) {
  s <- 0
  for (id in c("process_knowledge", "workforce_suppliers", "adjacent_innovation")) {
    v <- x[[id]]
    s <- s + ifelse(v %in% "yes", 1, ifelse(v %in% "partly", 0.5, ifelse(is.na(v) & bound == "high", 1, 0)))
  }
  s
}
sp_learning_level <- function(s) ifelse(s >= 2, "strong", ifelse(s >= 1, "moderate", "weak"))

sp_test_capability <- function(x, cfg) {
  stages <- unlist(cfg$compete_stages %||% setdiff(SUPPLY_CHAIN_STAGES, "Deployment"))
  applies <- x$supply_chain %in% stages
  pos <- cfg$position %||% list(strong = 0.10, moderate = 0.03)
  s <- x$home_share
  comp <- x$complementary

  # Present position (gates.ts positionPredicate): a share band, with the foothold band needing
  # complementary capabilities AND specialisation. A 3-10% world share is what a large diversified
  # exporter holds in many baskets without being specialised in them, so a foothold counts only in
  # the country's relatively stronger cells (within-country Competitive Position percentile).
  spec_min <- pos$foothold_within_country_pct %||% 0.5
  cpw <- if ("cp_within_country_pct" %in% names(x)) x$cp_within_country_pct else rep(1, nrow(x))
  specialised <- ifelse(is.na(cpw), NA, cpw >= spec_min)
  position <- dplyr::case_when(
    is.na(s) ~ "unknown",
    s >= pos$strong ~ "yes",
    s >= pos$moderate & (is.na(comp) | is.na(specialised)) & !(comp %in% "no" | specialised %in% FALSE) ~ "unknown",
    s >= pos$moderate & comp %in% c("yes", "partly") & specialised %in% TRUE ~ "yes",
    TRUE ~ "no"
  )
  position_marginal <- !is.na(s) & ((s >= pos$strong & s < pos$strong + 0.02) | (s >= pos$moderate & s < pos$strong & comp %in% "partly") |
    (s < pos$moderate & s >= pos$moderate - 0.01))

  # Industrial learning counts only where the country has a base to learn from: a present position
  # or foothold, or complementary capabilities. A node that carries learning in general is not a
  # reason for every country to make it (this departs from the reader-driven TS tool on purpose).
  base_lo <- position == "yes" | comp %in% c("yes", "partly") | (!is.na(s) & s >= pos$moderate)
  base_hi <- base_lo | position == "unknown" | is.na(comp)
  lv_lo <- ifelse(base_lo, sp_learning_level(sp_learning_score(x, "low")), "weak")
  lv_hi <- ifelse(base_hi, sp_learning_level(sp_learning_score(x, "high")), "weak")
  learning_level <- ifelse(lv_lo == lv_hi, lv_lo, "unknown")

  # Leap ahead (gates.ts leapCredible), only for curated leap candidates.
  leap_ok <- function(bound) {
    v <- function(id) {
      y <- x[[id]]
      ifelse(is.na(y), if (bound == "high") "yes" else "no", y)
    }
    core <- v("beyond_novelty") == "yes" & v("cost_perf_advantage") == "yes"
    support <- (v("incumbent_vulnerable") != "no") + (v("complementary") != "no") + (v("scale_pathway") != "no")
    x$leap_candidate %in% "yes" & core & support >= 2
  }
  leap_lo <- leap_ok("low")
  leap_hi <- leap_ok("high")
  leap <- ifelse(leap_lo, "yes", ifelse(!leap_hi, "no", "unknown"))

  strength_of <- function(p, lv, lp) {
    n <- p + (lv != "weak") + lp
    ifelse(lv == "strong" | n >= 2, "strong", ifelse(n == 1, "moderate", "none"))
  }
  st_lo <- strength_of(position == "yes", lv_lo, leap_lo)
  st_hi <- strength_of(position != "no", lv_hi, leap_hi)
  strength <- ifelse(!applies, "none", ifelse(st_lo == st_hi, st_lo, "unknown"))

  detail <- dplyr::case_when(
    !applies ~ "Deployment is where the technology is used, not produced: no industrial-capability case is tested.",
    strength == "strong" ~ "A strong capability case: established position, strong transferable learning on a domestic base, or two rationales together.",
    strength == "moderate" ~ "A moderate capability case resting on one rationale.",
    strength == "none" & position == "no" ~ paste0("Little present position (", sp_pct(s), " of world supply) and no base for a learning or leap case."),
    strength == "none" ~ "No position, learning or leap rationale is established.",
    TRUE ~ "The capability questions are not fully answered."
  )
  list(
    state = strength, applies = applies, position = position, position_marginal = position_marginal,
    learning = learning_level, learning_lo = lv_lo, learning_hi = lv_hi, leap = leap,
    strength_lo = ifelse(applies, st_lo, "none"), strength_hi = ifelse(applies, st_hi, "none"), detail = detail
  )
}

# Test 4: which strategy is credible (strict reading), with the strongest-first order preserve >
# adjacent_capability > catch_up > leap_ahead. Returns the primary strategy (NA when none) and all
# credible ones.
sp_test_strategy <- function(x, cap, cfg) {
  market_min <- cfg$catch_up_market_pct %||% 0.75
  learn_ok <- cap$learning_lo %in% c("strong", "moderate")
  learn_could <- cap$learning_hi %in% c("strong", "moderate")
  # Extraction needs a resource to build on: without recorded reserves or resource (resource_base
  # FALSE) no adjacent or catch-up case opens there, whatever the neighbouring stages hold.
  resource_ok <- if ("resource_base" %in% names(x)) !(x$supply_chain == "Extraction" & x$resource_base %in% FALSE) else rep(TRUE, nrow(x))
  preserve <- cap$applies & cap$position == "yes"
  adjacent <- cap$applies & cap$position != "yes" & x$adjacent_established %in% TRUE & learn_ok & resource_ok
  # a market to catch up into: large at home, and growing in the world (config market_outlook)
  outlooks <- unlist(cfg$catch_up_outlooks %||% "growing")
  growing <- if ("market_outlook" %in% names(x)) x$market_outlook %in% outlooks else rep(TRUE, nrow(x))
  market_ok <- !is.na(x$market_pct) & x$market_pct >= market_min
  catch_up <- cap$applies & cap$position != "yes" & x$complementary %in% "yes" & learn_ok & market_ok & growing & resource_ok
  leap <- cap$applies & cap$leap == "yes"
  m <- cbind(preserve = preserve, adjacent_capability = adjacent, catch_up = catch_up, leap_ahead = leap)
  primary <- apply(m, 1, function(r) if (any(r)) colnames(m)[which(r)[1]] else NA_character_)
  all <- apply(m, 1, function(r) if (any(r)) paste(colnames(m)[r], collapse = "+") else NA_character_)
  # Unknown: no strategy established yet, but an unknown input could still open one.
  adj_could <- !x$adjacent_established %in% FALSE & learn_could & resource_ok
  catch_could <- !x$complementary %in% c("no", "partly") & learn_could & (is.na(x$market_pct) | market_ok) & growing & resource_ok
  could <- cap$applies & (cap$position == "unknown" | cap$leap == "unknown" |
    (cap$position != "yes" & (adj_could | catch_could) &
      (is.na(x$adjacent_established) | is.na(x$complementary) | is.na(x$market_pct) | cap$learning == "unknown")))
  state <- ifelse(!cap$applies, "n_a", ifelse(!is.na(primary), "yes", ifelse(could, "unknown", "no")))
  detail <- dplyr::case_when(
    !cap$applies ~ "Not tested at Deployment.",
    primary == "preserve" ~ "Preserve: the country already holds a meaningful share of world supply that could be hollowed out.",
    primary == "adjacent_capability" ~ "Build adjacent capability: an established position in the same technology's neighbouring stage, and the node carries learning.",
    primary == "catch_up" ~ "Catch up: complementary capabilities, a learning node and a large domestic market make closing the gap plausible.",
    primary == "leap_ahead" ~ "Leap ahead: a successor technology offers a real advantage and the country has strengths to leap from.",
    state == "unknown" ~ "Whether a preserve, catch-up, adjacent or leap strategy is credible depends on inputs not yet known.",
    TRUE ~ "No credible path: no present position, no established adjacent stage, and no catch-up or leap case."
  )
  list(state = state, primary = primary, all = all, detail = detail)
}

# Test 5: proportionality of a capability-based intervention.
sp_test_proportionality <- function(x, cap, cfg) {
  prem <- cfg$premium %||% list(moderate = 10, high = 30)
  alt_min <- cfg$allied_alternative_share %||% 0.5
  p <- x$premium_pct
  band <- ifelse(is.na(p), NA, ifelse(p > prem$high, "high", ifelse(p >= prem$moderate, "moderate", "low")))
  # alternatives are partners' supply: trusted supply less the country's own share
  home <- dplyr::coalesce(if ("delivery_home_share" %in% names(x)) x$delivery_home_share else x$home_share, x$home_share, 0)
  partners <- pmax(x$trusted_world_share - home, 0)
  alternatives <- ifelse(is.na(x$trusted_world_share), NA, partners >= alt_min)
  dep_critical <- x$deployment_critical %in% "yes"
  # fail: a high premium; or, premium unknown, trusted alternatives exist and the good is
  # deployment-critical (protection would raise deployment costs when allies can supply).
  fail <- ifelse(!is.na(band), band == "high", ifelse(dep_critical, alternatives, FALSE))
  # climate cost: protecting or subsidising a high-emission technology (config climate_cost_max_ghg_index)
  climate <- if ("climate_cost" %in% names(x)) x$climate_cost %in% TRUE else rep(FALSE, nrow(x))
  fail <- ifelse(climate, TRUE, fail)
  state <- ifelse(!cap$applies, "n_a", ifelse(is.na(fail), "unknown", ifelse(fail, "fail", "pass")))
  detail <- dplyr::case_when(
    !cap$applies ~ "Not tested at Deployment.",
    climate ~ "A high-emission technology: building or protecting it has a climate cost that a moderate capability case does not justify.",
    !is.na(band) & band == "high" ~ paste0("A domestic premium of about ", round(p), "% exceeds what a moderate capability case justifies."),
    !is.na(band) ~ paste0("A ", band, " domestic premium (about ", round(p), "%)."),
    state == "fail" ~ paste0("Premium not measured; trusted partners hold ", sp_pct(partners), " of world supply and the good is deployment-critical, so protection would buy little at a real deployment cost."),
    state == "pass" ~ ifelse(dep_critical, paste0("Premium not measured; trusted partners hold only ", sp_pct(partners), " of world supply."), "Premium not measured; the good is not deployment-critical."),
    TRUE ~ "The cost of protection cannot be judged on the evidence available."
  )
  list(state = state, band = band, detail = detail)
}

# ---- Strict posture --------------------------------------------------------------------------------
sp_posture_gates <- function(x, cfg) {
  t1 <- sp_test_consequence(x, cfg)
  dig <- sp_test_digital(x, cfg)
  phy <- sp_test_physical(x, cfg)
  # each route counts only where the node's rule allows it
  dig$state <- ifelse(sp_route_eligible(x, "operational_control"), dig$state, "no")
  phy$state <- ifelse(sp_route_eligible(x, "coercive_chokepoint"), phy$state, "no")
  dual <- sp_test_dual_use(x, cfg)
  res <- sp_test_resilience(x, cfg)
  route_a <- sp_or3(dig$state, dual$state)
  t2 <- sp_or3(route_a, phy$state, res$state)
  secure <- sp_and3(t1$state, t2)
  cap <- sp_test_capability(x, cfg)
  strat <- sp_test_strategy(x, cap, cfg)
  prop <- sp_test_proportionality(x, cap, cfg)
  # Strict reading on the capability case's lower bound: a case proven at least moderate counts even
  # when the unknowns could make it strong.
  lo <- cap$strength_lo
  compete <- dplyr::case_when(
    !cap$applies ~ "no",
    strat$state %in% c("no", "n_a") ~ "no",
    lo == "strong" & strat$state == "yes" ~ "yes",
    cap$state == "none" ~ "no",
    lo == "moderate" & strat$state == "yes" & prop$state == "pass" ~ "yes",
    cap$state == "moderate" & prop$state == "fail" ~ "no",
    TRUE ~ "unknown"
  )
  list(t1 = t1, digital = dig, physical = phy, dual = dual, resilience = res, route_a = route_a, t2 = t2, secure = secure,
       cap = cap, strategy = strat, prop = prop, compete = compete)
}

# The Secure routes a node meets (";"-separated, in SP_SECURITY_RATIONALES order) and the primary
# rationale: operational control through a command path, else the physical chokepoint, else
# national-security control of a dual-use node, else systemic resilience.
sp_security_routes <- function(g) {
  met <- cbind(
    operational_control = g$route_a == "yes",
    coercive_chokepoint = g$physical$state == "yes",
    systemic_resilience = g$resilience$state == "yes"
  )
  routes <- apply(met, 1, function(r) if (any(r)) paste(colnames(met)[r], collapse = ";") else NA_character_)
  primary <- dplyr::case_when(
    g$digital$state == "yes" ~ "operational_control",
    g$physical$state == "yes" ~ "coercive_chokepoint",
    g$dual$state == "yes" ~ "operational_control",
    g$resilience$state == "yes" ~ "systemic_resilience",
    TRUE ~ NA_character_
  )
  list(routes = routes, primary = primary)
}

sp_posture_from_gates <- function(x, g, cfg) {
  watch <- cfg$concentration_watch %||% list(coercive_share = 0.4, import_hhi = 0.18)
  posture <- ifelse(g$secure == "yes", "secure", ifelse(g$compete == "yes", "compete", "deploy"))
  hhi <- if ("import_hhi" %in% names(x)) x$import_hhi else rep(NA_real_, nrow(x))
  scope <- dplyr::case_when(
    g$digital$state == "yes" & g$physical$state == "yes" ~ "secure_both",
    g$digital$state == "yes" & x$separability %in% "separable" ~ "secure_control",
    g$digital$state == "yes" & x$separability %in% "partial" ~ "secure_integrated",
    g$digital$state == "yes" & x$separability %in% "inseparable" ~ "secure_whole",
    g$digital$state == "yes" ~ "secure_unclear",
    g$physical$state == "yes" ~ "secure_input",
    g$dual$state == "yes" ~ "secure_dual_use",
    g$resilience$state == "yes" ~ "secure_resilience",
    TRUE ~ "secure_input"
  )
  costly <- g$cap$strength_lo == "strong" & g$prop$state == "fail"
  # the import HHI describes the country's supply only when its imports are material
  material <- sp_material_imports(x, cfg)
  variant <- dplyr::case_when(
    posture == "secure" ~ scope,
    posture == "compete" & costly ~ "compete_costly",
    posture == "compete" ~ paste0("compete_", g$strategy$primary),
    # a floor keeps a capability that has a credible strategy but fails on cost; no strategy, no floor
    g$cap$strength_lo == "moderate" & g$prop$state == "fail" & g$strategy$state == "yes" ~ "deploy_floor",
    (!is.na(x$coercive_top_share) & x$coercive_top_share >= watch$coercive_share) |
      (!is.na(hhi) & hhi >= watch$import_hhi & material %in% TRUE) ~ "deploy_diversify",
    TRUE ~ "deploy"
  )
  list(posture = posture, variant = variant)
}

# Resolve each unknown input of `ids` to its `end`.
sp_posture_resolve <- function(x, ids, end) {
  for (id in ids) {
    v <- x[[id]]
    miss <- is.na(v)
    if (any(miss)) {
      x[[id]][miss] <- sp_posture_extreme(id, end)
    }
  }
  x
}

# Every posture the unknowns allow: the strict posture at the four corners (security inputs at their
# open or security end x capability inputs at their weak or strong end), as classify.ts.
sp_posture_possible <- function(x, cfg) {
  corners <- list(c("low", "low"), c("low", "high"), c("high", "low"), c("high", "high"))
  sec_ids <- c(SP_POSTURE_SECURITY_INPUTS, intersect(SP_POSTURE_ROUTE_INPUTS, names(x)))
  out <- vapply(corners, function(cn) {
    y <- sp_posture_resolve(x, sec_ids, cn[1])
    y <- sp_posture_resolve(y, SP_POSTURE_CAPABILITY_INPUTS, cn[2])
    sp_posture_from_gates(y, sp_posture_gates(y, cfg), cfg)$posture
  }, character(nrow(x)))
  if (is.null(dim(out))) out <- matrix(out, nrow = nrow(x))
  apply(out, 1, function(r) paste(SP_POSTURES[SP_POSTURES %in% r], collapse = "|"))
}

# Unknown inputs that would narrow the answer (classify.ts missingEvidence `pivotal`): resolving the
# input alone, to either end of its range, changes the set of possible postures.
sp_posture_pivotal <- function(x, possible, cfg) {
  ids <- c(SP_POSTURE_SECURITY_INPUTS, intersect(SP_POSTURE_ROUTE_INPUTS, names(x)), SP_POSTURE_CAPABILITY_INPUTS)
  hits <- matrix(FALSE, nrow(x), length(ids), dimnames = list(NULL, ids))
  open <- grepl("\\|", possible)
  for (id in ids) {
    miss <- is.na(x[[id]]) & open
    if (!any(miss)) next
    sub <- x[miss, , drop = FALSE]
    for (end in c("low", "high")) {
      p <- sp_posture_possible(sp_posture_resolve(sub, id, end), cfg)
      hits[miss, id] <- hits[miss, id] | (p != possible[miss])
    }
  }
  apply(hits, 1, function(r) if (any(r)) paste(ids[r], collapse = ";") else NA_character_)
}

# Share of the inputs that can still matter which are known (classify.ts calculateEvidenceCompleteness):
# security inputs stop mattering once Secure is ruled out; capability inputs once Secure is certain
# or the node is at Deployment.
sp_posture_completeness <- function(x, possible) {
  secure_possible <- grepl("secure", possible)
  secure_certain <- possible == "secure"
  sec_ids <- SP_POSTURE_SECURITY_INPUTS
  cap_ids <- intersect(SP_POSTURE_CAPABILITY_INPUTS, names(x)) # optional inputs absent from the frame do not count
  known <- function(ids) Reduce(`+`, lapply(ids, function(id) !is.na(x[[id]])))
  dig <- sp_digital_applies(x)
  # inputs that do not apply to the node do not count
  sec_n <- length(sec_ids) - ifelse(dig, 0, 2) # digitally_active, remote_control only on the digital path
  sec_k <- known(sec_ids) - ifelse(dig, 0, (!is.na(x$digitally_active)) + (!is.na(x$remote_control)))
  leap_ids <- c("beyond_novelty", "cost_perf_advantage", "incumbent_vulnerable", "scale_pathway")
  leap_on <- x$leap_candidate %in% "yes"
  cap_n <- length(cap_ids) - ifelse(leap_on, 0, length(leap_ids))
  cap_k <- known(cap_ids) - ifelse(leap_on, 0, known(leap_ids))
  cap_applies <- x$supply_chain != "Deployment" & !secure_certain
  n <- ifelse(secure_possible, sec_n, 2) + ifelse(cap_applies, cap_n, 0)
  k <- ifelse(secure_possible, sec_k, (!is.na(x$consequence)) + (!is.na(x$exposed))) + ifelse(cap_applies, cap_k, 0)
  ifelse(n > 0, k / n, NA_real_)
}

# ---- Delivery mode ---------------------------------------------------------------------------------
sp_delivery_mode <- function(x, posture, variant, strategy, cfg) {
  d <- cfg$delivery %||% list(domestic_share = 0.10, foothold_share = 0.03, allied_share = 0.3, diversified_share = 0.5)
  # in mineral cells the trusted share is the driving mineral's, so compare like with like
  home <- dplyr::coalesce(if ("delivery_home_share" %in% names(x)) x$delivery_home_share else x$home_share, x$home_share, 0)
  allied <- pmax(dplyr::coalesce(x$trusted_world_share, 0) - home, 0)
  nonadv <- 1 - dplyr::coalesce(x$coercive_world_share, NA_real_)
  dplyr::case_when(
    posture == "secure" & home >= d$domestic_share ~ "domestic",
    posture == "secure" & home >= d$foothold_share & allied >= d$allied_share ~ "mixed",
    posture == "secure" & allied >= d$allied_share ~ "allied",
    posture == "secure" & !is.na(nonadv) & nonadv >= d$diversified_share ~ "diversified_global",
    posture == "secure" ~ "mixed",
    posture == "compete" & strategy %in% c("preserve", "leap_ahead") ~ "domestic",
    # Partner: the capability is built with trusted partners, not at home
    posture == "compete" & strategy %in% "partner" & allied >= d$allied_share ~ "allied",
    posture == "compete" & strategy %in% "partner" ~ "diversified_global",
    posture == "compete" & allied >= d$allied_share ~ "mixed",
    posture == "compete" ~ "domestic",
    variant == "deploy_diversify" & allied >= d$allied_share ~ "allied",
    TRUE ~ "diversified_global"
  )
}

# ---- Classification --------------------------------------------------------------------------------
# x: one row per node with the columns in SP_POSTURE_REQUIRED (see sp_posture_inputs()). cfg: the
# strategic_posture list of config/strategic_position.yml. Optional columns used when present:
# critical_function, coercive_top_iso3, coercive_world_share, import_hhi, attr_confidence (the curator's
# confidence), stage_evidence_quality_cp, strategic_need_percentile, exclusion (rows with a value are
# not classified).
sp_strategic_posture <- function(x, cfg = list()) {
  x <- sp_posture_check_inputs(x)
  for (col in c("critical_function", "coercive_top_iso3")) if (!col %in% names(x)) x[[col]] <- NA_character_
  for (col in c("coercive_world_share", "import_hhi", "strategic_need_percentile")) if (!col %in% names(x)) x[[col]] <- NA_real_
  if (!"exclusion" %in% names(x)) x$exclusion <- NA_character_

  g <- sp_posture_gates(x, cfg)
  pv <- sp_posture_from_gates(x, g, cfg)
  possible <- sp_posture_possible(x, cfg)
  pivotal <- sp_posture_pivotal(x, possible, cfg)
  completeness <- sp_posture_completeness(x, possible)

  # Decisive predicates and their marginal answers (classify.ts decisivePredicates/confidenceOf).
  marg_secure <- g$t1$marginal + g$digital$marginal * (g$digital$state == "yes") + g$physical$marginal * (g$physical$state == "yes") +
    (g$dual$marginal & g$dual$state == "yes" & g$digital$state != "yes" & g$physical$state != "yes") +
    (g$resilience$marginal & g$resilience$state == "yes" & g$route_a != "yes" & g$physical$state != "yes")
  marg_deploy_sec <- ifelse(g$t1$state == "no", 0, ifelse(g$digital$applies, g$digital$marginal, 0) + ifelse(g$physical$applies, g$physical$marginal, 0))
  marg_cap <- g$cap$position_marginal
  marginal_n <- dplyr::case_when(
    pv$posture == "secure" ~ as.numeric(marg_secure),
    pv$posture == "compete" ~ as.numeric(marg_cap) + as.numeric(marg_deploy_sec > 0),
    TRUE ~ as.numeric(marg_deploy_sec > 0) + as.numeric(marg_cap)
  )
  determinate <- !grepl("\\|", possible)
  low_assess <- if ("attr_confidence" %in% names(x)) x$attr_confidence %in% "low" else rep(FALSE, nrow(x))
  ev_low <- if ("stage_evidence_quality_cp" %in% names(x)) pv$posture == "compete" & x$stage_evidence_quality_cp %in% c("low", "none") else rep(FALSE, nrow(x))
  hi <- cfg$confidence$high_completeness %||% 0.75
  lo <- cfg$confidence$low_completeness %||% 0.4
  confidence <- dplyr::case_when(
    is.na(completeness) | completeness < lo | marginal_n >= 2 | (!determinate & completeness < hi) ~ "low",
    determinate & marginal_n == 0 & completeness >= hi & !low_assess & !ev_low ~ "high",
    TRUE ~ "moderate"
  )

  strategy <- ifelse(pv$posture == "compete", g$strategy$primary, NA_character_)
  delivery <- sp_delivery_mode(x, pv$posture, pv$variant, strategy, cfg)
  need_hi <- !is.na(x$strategic_need_percentile) & x$strategic_need_percentile >= 0.75
  secondary <- dplyr::case_when(
    pv$posture == "secure" & g$cap$state %in% c("strong", "moderate") ~ "capability",
    pv$posture == "compete" & (g$secure == "unknown" | need_hi) ~ "security",
    pv$posture == "deploy" & pv$variant == "deploy_diversify" ~ "diversification",
    pv$posture == "deploy" & pv$variant == "deploy_floor" ~ "capability",
    TRUE ~ NA_character_
  )
  t2_path <- dplyr::case_when(
    g$digital$state == "yes" & g$physical$state == "yes" ~ "both",
    g$digital$state == "yes" ~ "digital",
    g$physical$state == "yes" ~ "physical",
    g$dual$state == "yes" ~ "dual_use",
    g$resilience$state == "yes" ~ "resilience",
    g$t2 == "unknown" ~ "unknown",
    TRUE ~ "none"
  )
  routes <- sp_security_routes(g)

  out <- data.frame(
    strategic_posture = pv$posture,
    posture_variant = pv$variant,
    posture_confidence = confidence,
    evidence_completeness = round(completeness, 3),
    posture_possible = possible,
    pivotal_inputs = pivotal,
    posture_primary_rationale = c(secure = "security", compete = "capability", deploy = "openness")[pv$posture],
    posture_secondary_rationale = secondary,
    compete_strategy = strategy,
    compete_strategies_credible = ifelse(pv$posture == "compete", g$strategy$all, NA_character_),
    delivery_mode = delivery,
    t1_consequence = g$t1$state,
    t2_vulnerability = g$t2,
    t2_path = t2_path,
    t2_digital = g$digital$state,
    t2_physical = g$physical$state,
    t2_coercion = ifelse(g$physical$applies, g$physical$parts$coercion, "n_a"),
    t2_dual_use = ifelse(g$dual$applies, g$dual$state, "n_a"),
    t2_resilience = ifelse(g$resilience$applies, g$resilience$state, "n_a"),
    security_rationale = ifelse(pv$posture == "secure", routes$primary, NA_character_),
    security_routes_met = ifelse(g$t1$state == "yes", routes$routes, NA_character_),
    t3_capability = g$cap$state,
    # the strict (lower-bound) reading of the capability case and of learning: the canonical choice's
    # capability floor for Preserve and the learning leg of the Catch Up pathway (decision_engine.R)
    t3_capability_lo = ifelse(g$cap$applies, g$cap$strength_lo, "n_a"),
    t3_learning_lo = ifelse(g$cap$applies, g$cap$learning_lo, "n_a"),
    t3_position = ifelse(g$cap$applies, g$cap$position, "n_a"),
    t3_learning = ifelse(g$cap$applies, g$cap$learning, "n_a"),
    t3_leap = ifelse(g$cap$applies, g$cap$leap, "n_a"),
    t4_strategy = ifelse(g$cap$applies, ifelse(is.na(g$strategy$primary), g$strategy$state, g$strategy$primary), "n_a"),
    t5_proportionality = g$prop$state,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  out$decision_trace <- sp_decision_trace(x, g, pv, possible, pivotal)

  # Rows that are not classified (entrepots, no data at all) keep their reason and no posture.
  skip <- !is.na(x$exclusion)
  if (any(skip)) {
    keep <- c("t1_consequence")
    out[skip, setdiff(names(out), keep)] <- NA
    out$t1_consequence[skip] <- NA_character_
  }
  out$posture_exclusion <- x$exclusion
  cbind(x[, c("Country", "iso3", "tech", "supply_chain", "layer_id", "layer_type"), drop = FALSE], out)
}

# ---- Decision trace --------------------------------------------------------------------------------
sp_json_str <- function(x) {
  x <- ifelse(is.na(x), "null", paste0("\"", gsub("[\r\n]", " ", gsub("\"", "\\\\\"", gsub("\\\\", "\\\\\\\\", x))), "\""))
  x
}
sp_json_val <- function(v) {
  if (is.numeric(v)) ifelse(is.na(v), "null", format(round(v, 4), trim = TRUE, scientific = FALSE))
  else if (is.logical(v)) ifelse(is.na(v), "null", ifelse(v, "true", "false"))
  else sp_json_str(as.character(v))
}
# One input reference: {"id","value","provenance"}; data inputs are 'data', curated ones 'assessment',
# missing ones 'unknown'.
sp_json_input <- function(x, id, provenance) {
  v <- x[[id]]
  prov <- ifelse(is.na(v), "unknown", provenance)
  paste0("{\"id\":\"", id, "\",\"value\":", sp_json_val(v), ",\"provenance\":\"", prov, "\"}")
}
sp_json_test <- function(id, state, marginal, detail, inputs) {
  paste0(
    "{\"id\":\"", id, "\",\"state\":", sp_json_str(state), ",\"marginal\":", ifelse(marginal, "true", "false"),
    ",\"detail\":", sp_json_str(detail), ",\"inputs\":[", inputs, "]}"
  )
}

sp_decision_trace <- function(x, g, pv, possible, pivotal) {
  inp <- function(ids, prov) do.call(paste, c(lapply(ids, function(id) sp_json_input(x, id, prov)), sep = ","))
  t2_detail <- ifelse(g$digital$applies, g$digital$detail, g$physical$detail)
  t2_detail <- ifelse(g$digital$applies & g$physical$applies, paste(g$digital$detail, g$physical$detail), t2_detail)
  tests <- paste(
    sp_json_test("t1_consequence", g$t1$state, g$t1$marginal, g$t1$detail,
                 paste(inp(c("consequence"), "assessment"), inp("exposed", "data"), sep = ",")),
    sp_json_test("t2_vulnerability", g$t2, g$digital$marginal | g$physical$marginal, t2_detail,
                 paste(inp(c("remote_control", "time_horizon", "substitutability", "qualification_time", "reconstitution_time", "stockpile"), "assessment"),
                       inp(c("coercive_top_share", "coercive_import_share", "trusted_world_share"), "data"), sep = ",")),
    sp_json_test("t2_dual_use", g$dual$state, g$dual$marginal, g$dual$detail,
                 paste(inp("dual_use", "assessment"), inp("coercive_import_share", "data"), sep = ",")),
    sp_json_test("t2_resilience", g$resilience$state, g$resilience$marginal, g$resilience$detail,
                 paste(inp(c("systemic_essentiality", "reconstitution_time"), "assessment"),
                       inp(c("sn_import_dependence", "trusted_world_share"), "data"), sep = ",")),
    sp_json_test("t3_capability", g$cap$state, g$cap$position_marginal, g$cap$detail,
                 paste(inp(c("home_share", "complementary"), "data"), inp(c("process_knowledge", "workforce_suppliers", "adjacent_innovation"), "assessment"), sep = ",")),
    sp_json_test("t4_strategy", ifelse(is.na(g$strategy$primary), g$strategy$state, g$strategy$primary), FALSE, g$strategy$detail,
                 inp(c("adjacent_established", "market_pct"), "data")),
    sp_json_test("t5_proportionality", g$prop$state, FALSE, g$prop$detail,
                 paste(inp("premium_pct", "data"), inp("trusted_world_share", "data"), sep = ",")),
    sep = ","
  )
  poss <- vapply(strsplit(possible, "|", fixed = TRUE), function(p) paste0("[", paste0("\"", p, "\"", collapse = ","), "]"), character(1))
  piv <- vapply(strsplit(ifelse(is.na(pivotal), "", pivotal), ";", fixed = TRUE), function(p) {
    if (length(p) == 0) "[]" else paste0("[", paste0("\"", p, "\"", collapse = ","), "]")
  }, character(1))
  paste0("{\"tests\":[", tests, "],\"posture\":\"", pv$posture, "\",\"variant\":\"", pv$variant,
         "\",\"possible\":", poss, ",\"pivotal\":", piv, "}")
}

# ---- Policy alignment: recommended choice vs observed policy (spec X; design D5) ----------------------------
# Observed policy is downstream of the recommendation and never an input to it. Vocabulary
# (SP_POLICY_ALIGNMENTS): aligned | under_prioritized | over_prioritized | misaligned | deployment_gap |
# mixed | insufficient_data. Rules, in order:
#   not assessed, or no policy priority                      -> insufficient_data
#   Partner with protection / localisation in the top third  -> misaligned (wrong treatment: autarky
#                                                               where the choice asks for trusted partners)
#   Partner otherwise                                        -> aligned (domestic records cannot show
#                                                               partnership instruments)
#   Secure / Compete with NIPO and PAMS on opposite sides, or a fence without a build -> mixed
#   Secure / Compete with support >= matched_min             -> aligned, else under_prioritized
#     (Preserve and Catch Up are judged on the NIPO support stock where measured, i.e. capability-building
#     support; Secure on overall policy priority)
#   Deploy . Accelerate with deployment-policy priority below deployment_gap_max -> deployment_gap
#   Deploy . Watch: heavy protection / localisation (NIPO protection in the top third; at Deployment, the
#     technology's Assembly-equipment protection, i.e. local-content rules) -> over_prioritized; where the
#     protection split is not measured, total policy attention in the top third with an attributed record
#     at a producing stage -> over_prioritized (alignment_confidence low); an unsettled Watch (other
#     choices still possible) -> insufficient_data when the over test fires.
# tbl carries strategic_posture, compete_mode, deploy_mode (optional; absent: every Deploy reads as
# Watch), supply_chain, posture_possible and the Revealed Policy Priority columns (policy_priority_percentile,
# nipo_priority, pams_priority, policy_support_priority, policy_protection_priority, and at Deployment
# optionally deployment_localisation_priority); `has_record` says whether the cell has an attributed record.
SP_POLICY_ALIGNMENTS <- c("aligned", "under_prioritized", "over_prioritized", "misaligned", "deployment_gap", "mixed", "insufficient_data")

sp_policy_alignment_inputs <- function(tbl, cfg, has_record) {
  col <- function(n) if (n %in% names(tbl)) tbl[[n]] else rep(NA_real_, nrow(tbl))
  chr <- function(n) if (n %in% names(tbl)) as.character(tbl[[n]]) else rep(NA_character_, nrow(tbl))
  gap <- cfg$source_gap_mixed %||% 0.5
  fwb <- cfg$fence_without_build %||% list(protection_min = 0.67, support_max = 0.33)
  over <- cfg$over_min %||% 0.67
  posture <- tbl$strategic_posture
  mode <- chr("compete_mode")
  dmode <- if ("deploy_mode" %in% names(tbl)) chr("deploy_mode") else ifelse(posture %in% "deploy", "watch", NA_character_)
  pp <- col("policy_priority_percentile")
  nipo <- col("nipo_priority")
  pams <- col("pams_priority")
  sup <- col("policy_support_priority")
  pro <- col("policy_protection_priority")
  loc_dep <- col("deployment_localisation_priority")
  if (is.null(has_record)) has_record <- rep(TRUE, nrow(tbl))
  dep <- tbl$supply_chain == "Deployment"
  disagree <- !is.na(nipo) & !is.na(pams) & abs(nipo - pams) >= gap & ((nipo >= 0.5) != (pams >= 0.5))
  fence_no_build <- posture %in% "compete" & !mode %in% "partner" & !is.na(pro) & !is.na(sup) & pro >= fwb$protection_min & sup <= fwb$support_max
  autarkic <- mode %in% "partner" & !is.na(pro) & pro >= (cfg$partner_autarky$protection_min %||% 0.67)
  watch <- posture %in% "deploy" & dmode %in% "watch"
  # protection measure for Watch: the cell's NIPO protection, at Deployment the Assembly equipment's
  protect <- ifelse(dep, loc_dep, pro)
  over_protect <- watch & !is.na(protect) & protect >= over
  over_attention <- watch & !dep & is.na(protect) & !is.na(pp) & pp >= over & has_record
  list(posture = posture, mode = mode, dmode = dmode, pp = pp, sup = sup, pro = pro, dep = dep, disagree = disagree,
       fence_no_build = fence_no_build, autarkic = autarkic, watch = watch, protect = protect,
       over_protect = over_protect, over_attention = over_attention)
}

sp_policy_alignment <- function(tbl, cfg = list(), has_record = NULL) {
  matched <- cfg$matched_min %||% 0.5
  dep_gap <- cfg$deployment_gap_max %||% matched
  a <- sp_policy_alignment_inputs(tbl, cfg, has_record)
  settled <- if ("posture_possible" %in% names(tbl)) tbl$posture_possible %in% "deploy" else rep(TRUE, nrow(tbl))
  # Preserve and Catch Up need support for the capability: judged on the support stock where measured.
  support <- ifelse(a$mode %in% c("preserve", "catch_up") & !is.na(a$sup), a$sup, a$pp)
  dplyr::case_when(
    is.na(a$posture) ~ NA_character_,
    a$posture %in% "not_assessed" ~ "insufficient_data",
    is.na(a$pp) & !(a$watch & a$over_protect) ~ "insufficient_data",
    a$autarkic ~ "misaligned",
    a$mode %in% "partner" ~ "aligned",
    a$posture %in% c("secure", "compete") & (a$disagree | a$fence_no_build) ~ "mixed",
    a$posture %in% c("secure", "compete") & support >= matched ~ "aligned",
    a$posture %in% c("secure", "compete") ~ "under_prioritized",
    a$dmode %in% "accelerate" & a$pp < dep_gap ~ "deployment_gap",
    a$dmode %in% "accelerate" ~ "aligned",
    (a$over_protect | a$over_attention) & !settled ~ "insufficient_data",
    a$over_protect | a$over_attention ~ "over_prioritized",
    a$disagree ~ "mixed",
    TRUE ~ "aligned"
  )
}

# Why a cell's policy alignment is what it is, where a rule beyond the priority levels decides it:
# partner_autarkic (Partner with protection in the top third), fence_without_build (Compete with heavy
# protection and little support), watch_protection (Watch with heavy protection / localisation),
# watch_attention (Watch with heavy total attention, protection not measured), watch_localisation_deployment
# (a Watch deployment whose equipment carries heavy local-content protection), weak_deployment_policy
# (Accelerate with low deployment-policy priority), sources_disagree (NIPO and PAMS on opposite sides).
sp_policy_alignment_flag <- function(tbl, cfg = list(), has_record = NULL) {
  matched <- cfg$matched_min %||% 0.5
  dep_gap <- cfg$deployment_gap_max %||% matched
  a <- sp_policy_alignment_inputs(tbl, cfg, has_record)
  dplyr::case_when(
    is.na(a$posture) | a$posture %in% "not_assessed" ~ NA_character_,
    a$autarkic ~ "partner_autarkic",
    a$fence_no_build ~ "fence_without_build",
    a$over_protect & a$dep ~ "watch_localisation_deployment",
    a$over_protect ~ "watch_protection",
    a$over_attention ~ "watch_attention",
    a$dmode %in% "accelerate" & !is.na(a$pp) & a$pp < dep_gap ~ "weak_deployment_policy",
    a$disagree ~ "sources_disagree",
    TRUE ~ NA_character_
  )
}

# How far the alignment can be trusted (high / medium / low): two policy sources ranked = high, one =
# medium, none = low; one level lower where the rule needed the NIPO support / protection split and it
# is not measured (judged on total attention instead), and capped at medium at Deployment (one source,
# PAMS, by construction). insufficient_data is low.
sp_policy_alignment_confidence <- function(tbl, alignment, cfg = list(), has_record = NULL) {
  a <- sp_policy_alignment_inputs(tbl, cfg, has_record)
  nsrc <- if ("pp_n_sources" %in% names(tbl)) suppressWarnings(as.integer(tbl$pp_n_sources)) else
    as.integer(!is.na(if ("nipo_priority" %in% names(tbl)) tbl$nipo_priority else NA)) + as.integer(!is.na(if ("pams_priority" %in% names(tbl)) tbl$pams_priority else NA))
  nsrc <- dplyr::coalesce(nsrc, 0L)
  lvl <- ifelse(nsrc >= 2, 3L, ifelse(nsrc == 1, 2L, 1L))
  fallback <- (a$mode %in% c("preserve", "catch_up") & is.na(a$sup)) | (a$mode %in% "partner" & is.na(a$pro)) |
    a$over_attention | (a$watch & is.na(a$protect))
  lvl <- lvl - as.integer(fallback)
  lvl <- ifelse(a$dep, pmin(lvl, 2L), lvl)
  lvl <- pmax(lvl, 1L)
  out <- c("low", "medium", "high")[lvl]
  ifelse(is.na(alignment), NA_character_, ifelse(alignment == "insufficient_data", "low", out))
}

# The strict posture alone (no corners, pivotal inputs or trace): for sensitivity runs.
sp_posture_strict <- function(x, cfg = list()) {
  x <- sp_posture_check_inputs(x)
  if (!"import_hhi" %in% names(x)) x$import_hhi <- NA_real_
  p <- sp_posture_from_gates(x, sp_posture_gates(x, cfg), cfg)$posture
  if ("exclusion" %in% names(x)) p[!is.na(x$exclusion)] <- NA_character_
  p
}
