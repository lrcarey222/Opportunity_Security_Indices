# The six-stage posture bundle of the Explorer (dashboard/public/data/posture), schema 2: the canonical
# decision (scripts/26: Need -> Prize -> Position -> Choice -> Policy alignment) in the shape the Country,
# Sector and AIB views read (dashboard/src/posture/contract.ts, detail.ts). One core document with every
# country x technology x six-stage cell in columns, the decision thresholds, the stage display names, the
# node phrase table and the allied division of labour; and one detail document per country for the
# five-question cell drawer (NEED, PRIZE, POSITION, CHOICE, POLICY ALIGNMENT). A v1 core (Need x
# Opportunity quadrants) is refused by the app. Pure: scripts/91_export_posture_data.R writes the files.

POSTURE_SCHEMA <- "osi-posture"
POSTURE_SCHEMA_VERSION <- 2L
POSTURE_CODE <- c(secure = "S", compete = "C", deploy = "D", not_assessed = "N")
POSTURE_BUDGET_CORE <- 3 * 1024^2
POSTURE_BUDGET_DETAIL <- 128 * 1024
POSTURE_SCENARIO_LABELS <- list(current_policy = "Current policy", national_commitments = "National commitments",
                                climate_aligned = "Climate-aligned")

# Direction of current trade momentum from the log export growth: up above +band, down below -band.
posture_momentum_arrow <- function(raw, band = 0.1) {
  ifelse(is.na(raw), NA_character_, ifelse(raw > band, "up", ifelse(raw < -band, "down", "flat")))
}

# "Aug 2026" from a period label "Sep 2025 - Aug 2026".
posture_through <- function(period) {
  p <- stats::na.omit(unique(period))
  if (!length(p)) return(NA_character_)
  trimws(sub("^.* - ", "", p[1]))
}

posture_r3 <- function(x) ifelse(is.na(x), NA_real_, round(x, 3))

# Evidence words as the Explorer grammar reads them (spec XX): strong | moderate | weak. The engine
# writes high | medium | low | none, and the core carries those raw words (the TS contract normalises them
# once, normalizeEvidence() in dashboard/src/posture/contract.ts, and keeps the raw word as evidenceRaw);
# this is the R mirror of that mapping, for R-side readers and tests.
posture_evidence_word <- function(x) {
  unname(c(high = "strong", medium = "moderate", moderate = "moderate", strong = "strong", low = "weak", none = "weak",
           weak = "weak")[as.character(x)])
}

# The relevant Need of a cell: supply_chain_need at the producing stages, deployment_need at Deployment.
posture_need <- function(x) {
  dn <- if ("deployment_need" %in% names(x)) x$deployment_need else rep(NA_real_, nrow(x))
  ifelse(x$supply_chain == "Deployment", dn, x$supply_chain_need)
}

# Stop on a cell that breaks the framework's invariants (the app refuses such a core): Compete has a mode
# and no deploy mode, Deploy a deploy mode and no mode, Secure / not assessed neither; no Compete at
# Deployment; Accelerate only at Deployment.
posture_check_invariants <- function(x) {
  p <- x$strategic_posture
  m <- x$compete_mode
  dm <- x$deploy_mode
  dep <- x$supply_chain == "Deployment"
  bad <- c(
    compete_without_mode = sum(p %in% "compete" & (is.na(m) | !is.na(dm))),
    deploy_without_deploy_mode = sum(p %in% "deploy" & (is.na(dm) | !is.na(m))),
    secure_or_unassessed_with_mode = sum(p %in% c("secure", "not_assessed") & (!is.na(m) | !is.na(dm))),
    compete_at_deployment = sum(p %in% "compete" & dep),
    accelerate_off_deployment = sum(dm %in% "accelerate" & !dep),
    unknown_posture = sum(!p %in% names(POSTURE_CODE))
  )
  if (any(bad > 0)) stop("posture bundle: cells break the framework invariants: ",
                         paste(names(bad)[bad > 0], bad[bad > 0], sep = " ", collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

# The scenario each Deployment Need family is assessed on, from the cell table (deployment_need.R writes
# dn_<family>_scenario); defaults where the table carries none.
posture_family_scenarios <- function(x) {
  pick <- function(n, default) {
    v <- if (n %in% names(x)) stats::na.omit(unique(as.character(x[[n]]))) else character()
    if (length(v)) v[1] else default
  }
  list(demand = pick("dn_demand_scenario", "current_policy"), policy_gap = pick("dn_policy_gap_scenario", "national_commitments"),
       system = pick("dn_system_scenario", NA_character_))
}

# The core document. tbl: strategic_position_tbl (scripts/26). division: strategic_aib_division_tbl.
# countries: iso3, name, tier (coalition tier: core / partner / none). cfg: config/strategic_position.yml
# (headline_techs, decision_framework thresholds, node_phrases). stage_display: named character, stage id ->
# display name (config/supply_chain_stages.yml labels).
posture_core_doc <- function(tbl, division, countries, cfg, created, git_commit = NA_character_, momentum_band = 0.1,
                             stage_display = NULL) {
  techs <- unlist(cfg$headline_techs %||% sort(unique(tbl$tech)))
  stages <- SUPPLY_CHAIN_STAGES
  x <- tbl[!is.na(tbl$strategic_posture) & tbl$tech %in% techs, , drop = FALSE]
  x <- x[order(x$iso3, match(x$tech, techs), match(x$supply_chain, stages)), , drop = FALSE]
  posture_check_invariants(x)
  isos <- sort(unique(x$iso3))
  cty <- countries[match(isos, countries$iso3), , drop = FALSE]
  cty$iso3 <- isos
  cty$name <- dplyr::coalesce(cty$name, x$Country[match(isos, x$iso3)])
  cty$tier <- dplyr::coalesce(cty$tier, "none")
  df <- cfg$decision_framework %||% list()
  col <- function(n) if (n %in% names(x)) x[[n]] else rep(NA, nrow(x))
  cells <- list(
    c = match(x$iso3, isos) - 1L,
    t = match(x$tech, techs) - 1L,
    s = match(x$supply_chain, stages) - 1L,
    p = unname(POSTURE_CODE[x$strategic_posture]),
    m = col("compete_mode"),
    dm = col("deploy_mode"),
    need = posture_r3(posture_need(x)),
    prize = posture_r3(col("industrial_opportunity")),
    cp = posture_r3(col("cp_contender_pct")),
    v = col("posture_variant"),
    sec = col("security_rationale"),
    nt = col("need_type"),
    # the measured share (an economy a partial listing omits: its lower bound), not the engine's bracket
    share = posture_r3(dplyr::coalesce(col("home_share_exposure"), col("home_share"))),
    pw = col("pathway_credible"),
    bd = col("decision_borderline"),
    conf = col("posture_confidence"),
    fl = col("consistency_flag"),
    ev = col("stage_evidence_quality_cp"),   # raw high | medium | low | none (contract.ts normalises)
    al = col("policy_alignment"),
    alc = col("policy_alignment_confidence"),
    alf = col("policy_alignment_flag"),
    p1 = col("recommended_partner_1"),
    gap = col("partner_gap"),
    mom = posture_momentum_arrow(col("trade_momentum_raw"), momentum_band),
    mq = col("trade_momentum_quality")
  )
  cells <- lapply(cells, function(v) I(unname(v)))
  div <- division[division$tech %in% techs, , drop = FALSE]
  dcol <- function(n) if (n %in% names(div)) div[[n]] else rep(NA, nrow(div))
  members <- countries$iso3[countries$tier %in% c("core", "partner")]
  priority <- div$partner_demand + div$secure_demand > 0
  covered <- !div$aib_gap %in% c("no_trusted_supplier", "single_point_of_failure")
  sd <- stage_display %||% stats::setNames(stages, stages)
  phrases <- cfg$node_phrases %||% list()
  list(
    schema = POSTURE_SCHEMA,
    schema_version = POSTURE_SCHEMA_VERSION,
    created = created,
    git_commit = git_commit,
    meta = list(
      techs = I(techs), stages = I(stages),
      stage_display = as.list(sd[stages]),
      benchmark_period = unique(stats::na.omit(col("trade_benchmark_period")))[1],
      momentum_period = unique(stats::na.omit(col("trade_momentum_period")))[1],
      momentum_previous_period = unique(stats::na.omit(col("trade_momentum_previous_period")))[1],
      momentum_through = posture_through(col("trade_momentum_period")),
      momentum_band = momentum_band,
      thresholds = list(need = df$need_threshold %||% 0.67, prize = df$prize_threshold %||% 0.6, cp = df$cp_preserve_pct %||% 0.75,
                        deployment = df$deployment_threshold %||% 0.6, borderline_band = df$borderline_band %||% 0.05),
      scenario_labels = POSTURE_SCENARIO_LABELS,
      family_scenarios = posture_family_scenarios(x),
      phrases = phrases[intersect(names(phrases), paste(rep(techs, each = length(stages)), stages, sep = "|"))],
      core_members = sum(countries$tier %in% "core"),
      partner_members = sum(countries$tier %in% "partner")
    ),
    countries = list(iso3 = I(cty$iso3), name = I(cty$name), tier = I(cty$tier)),
    cells = cells,
    division = list(
      t = I(match(div$tech, techs) - 1L), s = I(match(div$supply_chain, stages) - 1L),
      primary = I(div$primary_holder), primary_share = I(posture_r3(div$primary_share)),
      backup_1 = I(div$backup_1), backup_1_share = I(posture_r3(div$backup_1_share)),
      backup_2 = I(div$backup_2), backup_2_share = I(posture_r3(div$backup_2_share)),
      redundancy = I(div$redundancy), allied_share = I(posture_r3(div$allied_share)), coercive_share = I(posture_r3(div$coercive_share)),
      allied_world_share = I(posture_r3(dcol("allied_world_share"))), coercive_world_share = I(posture_r3(dcol("coercive_world_share"))),
      leader = I(div$world_leader), leader_share = I(posture_r3(div$world_leader_share)),
      gap = I(div$aib_gap), gaps = I(div$aib_gaps), source = I(div$capability_source), aib_nodes = I(div$aib_n_nodes),
      partner_demand = I(div$partner_demand), secure_demand = I(div$secure_demand), partner_gaps = I(div$partner_gap_countries),
      member_partner_demand = I(div$partner_demand_members), secure_foreign_demand = I(dcol("secure_foreign_demand")),
      partner_aib_gaps = I(dcol("partner_aib_gaps"))
    ),
    aib_header = list(
      core_members = sum(countries$tier %in% "core"),
      members = length(members),
      priority_nodes = sum(priority),
      covered_share = if (any(priority)) round(mean(covered[priority]), 3) else NA_real_,
      unresolved_gaps = sum(priority & div$aib_gap != "none")
    ),
    design = posture_design_dict(x)
  )
}

# The policy design text, once per variant x delivery mode (it is the same for every country with that
# pair): keyed "variant|delivery", the drawer looks a cell's design up here.
posture_design_dict <- function(x) {
  need <- c("posture_variant", "delivery_mode", "fence_posture", "fence_level", "gate_posture", "build_posture", "benchmark", "exit_condition")
  if (!all(need %in% names(x))) return(list())
  d <- unique(x[!is.na(x$posture_variant), need, drop = FALSE])
  d <- d[!duplicated(paste(d$posture_variant, d$delivery_mode)), , drop = FALSE]
  stats::setNames(lapply(seq_len(nrow(d)), function(i) list(
    fence = d$fence_posture[i], fence_level = d$fence_level[i], gate = d$gate_posture[i], build = d$build_posture[i],
    benchmark = d$benchmark[i], exit = d$exit_condition[i]
  )), paste(d$posture_variant, d$delivery_mode, sep = "|"))
}

# The compact decision trace of a cell (decision_engine.R) as a list for the detail document.
posture_trace_steps <- function(trace) {
  if (is.na(trace) || !nzchar(trace)) return(NULL)
  j <- tryCatch(jsonlite::fromJSON(trace, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(j) || !length(j)) return(NULL)
  j
}

# The recommended mitigation of a Secure node: the first sentence of its build text (the full fence /
# gate / build design is in the core document's design dictionary).
posture_mitigation <- function(build) {
  if (is.null(build) || is.na(build) || !nzchar(build)) return(NULL)
  sub("^(.*?[.])\\s.*$", "\\1", build, perl = TRUE)
}

# The prize note: the world demand growth behind the node prize and the capex trend.
posture_prize_note <- function(r) {
  g <- function(n) if (n %in% names(r) && !is.na(r[[n]])) r[[n]] else NA
  growth <- g("io_growth")
  if (is.na(growth)) return(NULL)
  basis <- c(weo_world_stated = "WEO 2025 STEPS", iea_minerals_stated = "IEA minerals STEPS",
             proxy_deployment = "WEO 2025 STEPS, deployment proxy")[as.character(g("io_growth_basis"))]
  out <- sprintf("World demand %+.0f%% 2024-35 (%s)", 100 * growth, dplyr::coalesce(unname(basis), "IEA"))
  mom <- g("io_investment_momentum")
  if (!is.na(mom)) out <- paste0(out, sprintf("; capex %+.0f%% 2021-22 to 2024-25 (CIM)", 100 * (exp(mom) - 1)))
  if (isTRUE(g("io_node_prize_capped"))) out <- paste0(out, "; capped (declining market)")
  paste0(out, ".")
}

# The observed-policy sources of a cell as an array ("nipo+pams" -> ["nipo", "pams"]; "none" -> NULL).
posture_policy_sources <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(NULL)
  s <- setdiff(trimws(strsplit(x, "[+;]")[[1]]), c("", "none"))
  if (length(s)) I(s) else NULL
}

# Whether NIPO records any protection for a cell: TRUE where the observed stock is above 0, FALSE where it is
# observed and 0, NULL (left out of the document) where NIPO does not observe the cell.
posture_protection_recorded <- function(stock) {
  if (is.null(stock) || length(stock) != 1 || is.na(stock)) return(NULL)
  as.numeric(stock) > 0
}

# The bundle's commit stamp: the short sha, with "-dirty" where `git status --porcelain -- R scripts config`
# printed anything (the code that wrote the bundle is not the commit's). porcelain: its output lines.
posture_commit_stamp <- function(sha, porcelain = character()) {
  if (is.null(sha) || !length(sha) || is.na(sha[1]) || !nzchar(sha[1])) return(NA_character_)
  if (any(nzchar(trimws(porcelain)))) paste0(sha[1], "-dirty") else sha[1]
}

# One country's drawer document (schema 2): per cell, the five questions' evidence.
posture_detail_doc <- function(tbl, iso3, cfg, created) {
  techs <- unlist(cfg$headline_techs %||% sort(unique(tbl$tech)))
  x <- tbl[tbl$iso3 == iso3 & !is.na(tbl$strategic_posture) & tbl$tech %in% techs, , drop = FALSE]
  x <- x[order(match(x$tech, techs), match(x$supply_chain, SUPPLY_CHAIN_STAGES)), , drop = FALSE]
  g <- function(r, n) if (n %in% names(r) && length(r[[n]]) && !is.na(r[[n]])) r[[n]] else NULL
  num <- function(r, n) { v <- g(r, n); if (is.null(v)) NULL else signif(as.numeric(v), 3) }
  fam <- function(r, n) {
    v <- num(r, paste0(n, "_pct"))
    if (is.null(v)) v <- num(r, n)
    out <- Filter(Negate(is.null), list(value = v, basis = g(r, paste0(n, "_basis")), scenario = g(r, paste0(n, "_scenario")),
                                        note = g(r, paste0(n, "_note")) %||% g(r, paste0(n, "_measures"))))
    if (is.null(v) && is.null(out$basis)) NULL else out
  }
  cp_fams <- c("export_position", "export_position_structural", "production_position", "manufacturing_capacity", "investment_position",
               "cost_position", "resource_endowment", "deployment_scale")
  cells <- lapply(seq_len(nrow(x)), function(i) {
    r <- x[i, , drop = FALSE]
    dep <- r$supply_chain == "Deployment"
    posture <- r$strategic_posture
    partners <- Filter(Negate(is.null), lapply(1:3, function(k) {
      p <- g(r, paste0("recommended_partner_", k))
      if (is.null(p)) return(NULL)
      list(iso3 = p, capability = num(r, paste0("recommended_partner_", k, "_capability")),
           cp = num(r, paste0("recommended_partner_", k, "_cp")), relationship = num(r, paste0("recommended_partner_", k, "_relationship")),
           diversification = num(r, paste0("recommended_partner_", k, "_diversification")))
    }))
    cpf <- Filter(Negate(is.null), stats::setNames(lapply(cp_fams, function(f) num(r, paste0("cp_", f))), cp_fams))
    # the structural export position only where momentum moved it
    if (!is.null(cpf$export_position_structural) && identical(cpf$export_position_structural, cpf$export_position)) cpf$export_position_structural <- NULL
    need_block <- if (dep) list(
      dn_demand = fam(r, "dn_demand"), dn_policy_gap = fam(r, "dn_policy_gap"), dn_system = fam(r, "dn_system"),
      # the climate-aligned alternative: a World ratio (WEO NZE / Current Policies at 2035, minus 1), NOT a 0-1
      # score; shown only as a labelled alternative, never in the choice
      dn_climate_aligned = if (!is.null(num(r, "dn_climate_aligned"))) Filter(Negate(is.null), list(
        value = num(r, "dn_climate_aligned"), scenario = "climate_aligned", basis = g(r, "dn_climate_aligned_basis"),
        note = "World 2035, Net Zero vs Current Policies", unit = "ratio_vs_current_policies")) else NULL,
      dn_families_observed = num(r, "dn_families_observed"), dn_confidence = g(r, "dn_confidence"),
      deployment_trend = g(r, "deployment_trend")
    ) else list(
      import_dependence = num(r, "sn_import_dependence"), import_hhi = num(r, "sn_supplier_concentration"),
      china_bloc = num(r, "sn_risky_supplier_exposure"), trusted_alternatives = num(r, "sn_trusted_alternatives"),
      self_supplied = if (isTRUE(g(r, "sn_self_supplied"))) TRUE else NULL,
      top_supplier = g(r, "top_supplier_iso3"), top_supplier_share = num(r, "top_supplier_share"),
      china_share = num(r, "china_share"), opposite_bloc_share = num(r, "opposite_bloc_share"), imports_to_gdp = num(r, "imports_to_gdp"),
      trusted_share = num(r, "trusted_world_share"), coercive_top = g(r, "coercive_top_iso3"),
      coercive_top_share = num(r, "coercive_top_share"), coercive_import_share = num(r, "coercive_import_share"),
      substitutability = g(r, "substitutability"), reconstitution_time = g(r, "reconstitution_time"),
      stockpile = if (identical(g(r, "stockpile"), "Unknown")) NULL else g(r, "stockpile"),
      systemic_essentiality = g(r, "systemic_essentiality"), operational_control = g(r, "digitally_active")
    )
    prize <- if (dep) NULL else Filter(Negate(is.null), list(
      value = num(r, "industrial_opportunity"), node_prize = num(r, "io_node_prize"),
      investment_momentum = num(r, "io_investment_momentum_pct"), home_market = num(r, "io_home_market"),
      home_market_basis = g(r, "io_home_market_basis"), maturity = num(r, "io_maturity"),
      basis = g(r, "io_basis"), outlook = g(r, "io_outlook"), note = posture_prize_note(r)
    ))
    pol <- Filter(Negate(is.null), list(
      priority = num(r, "policy_priority_percentile"), support = num(r, "policy_support_priority"),
      protection = if (dep) num(r, "deployment_localisation_priority") else num(r, "policy_protection_priority"),
      # NIPO's observed protection stock above 0 (at Deployment the localisation stock, where the table has one)
      protection_recorded = posture_protection_recorded(if (dep) g(r, "deployment_localisation_stock") else g(r, "policy_protection_stock")),
      momentum = num(r, "policy_momentum"), alignment = g(r, "policy_alignment"),
      alignment_confidence = g(r, "policy_alignment_confidence"), flag = g(r, "policy_alignment_flag"),
      # the policy sources as a list (the engine writes "nipo+pams" / "pams" / "none"); I() keeps one item an array
      sources = posture_policy_sources(g(r, "policy_priority_sources")), n_sources = num(r, "pp_n_sources")
    ))
    out <- c(list(
      tech = r$tech, stage = r$supply_chain, posture = posture, variant = g(r, "posture_variant"),
      mode = g(r, "compete_mode"), deploy_mode = g(r, "deploy_mode"), confidence = g(r, "posture_confidence"),
      # the postures the evidence allows, only where there is more than one
      possible = if (grepl("|", dplyr::coalesce(g(r, "posture_possible"), ""), fixed = TRUE)) g(r, "posture_possible") else NULL,
      pivotal = g(r, "pivotal_inputs"),
      # the other choices the evidence allows, only where there are any (a borderline or an unknown pathway)
      choice_possible = if (grepl("|", dplyr::coalesce(g(r, "choice_possible"), ""), fixed = TRUE)) g(r, "choice_possible") else NULL,
      delivery = g(r, "delivery_mode"), security_rationale = g(r, "security_rationale"),
      security_routes = if (identical(posture, "secure")) g(r, "security_routes_met") else NULL,
      mitigation = if (identical(posture, "secure")) posture_mitigation(g(r, "build_posture")) else NULL,
      not_assessed_reason = if (identical(posture, "not_assessed")) g(r, "choice_reason") else NULL,
      need = if (dep) num(r, "deployment_need") else num(r, "supply_chain_need")),
      need_block,
      list(
        prize = if (length(prize)) prize else NULL,
        cp = num(r, "cp_contender_pct"),
        home_share = num(r, "home_share_exposure") %||% num(r, "home_share"), cp_families = if (length(cpf)) cpf else NULL,
        pathway_credible = if (dep) NULL else g(r, "pathway_credible"), pathway_basis = if (dep) NULL else g(r, "pathway_basis"),
        borderline = g(r, "decision_borderline"), consistency_flag = g(r, "consistency_flag"),
        trace = posture_trace_steps(g(r, "decision_trace") %||% NA_character_),
        policy = if (length(pol)) pol else NULL,
        partners = if (length(partners)) partners else NULL,
        partner_gap = g(r, "partner_gap"), partner_gap_reasons = g(r, "partner_gap_reasons"), partner_aib_status = g(r, "partner_aib_status"),
        partner_reason = g(r, "partner_reason"), partner_evidence = g(r, "partner_evidence_quality"),
        partner_source = g(r, "partner_capability_source"),
        trade = Filter(Negate(is.null), list(
          benchmark_source = g(r, "trade_benchmark_source"),
          benchmark_share = num(r, "trade_benchmark_share"), benchmark_rca = num(r, "trade_benchmark_rca"),
          benchmark_exports = num(r, "trade_benchmark_exports"),
          momentum_quality = g(r, "trade_momentum_quality"), momentum_raw = num(r, "trade_momentum_raw"),
          momentum_specialization = num(r, "trade_momentum_specialization"), momentum_score = num(r, "trade_momentum_score"),
          months_observed = num(r, "trade_months_observed"), exports_current = num(r, "trade_exports_current"),
          exports_previous = num(r, "trade_exports_previous")
        )),
        watch_reasons = g(r, "watch_reasons"),
        reason = if (identical(posture, "not_assessed")) NULL else g(r, "choice_reason")
      )
    )
    if (!length(out$trade)) out$trade <- NULL
    Filter(Negate(is.null), out)
  })
  list(schema = POSTURE_SCHEMA, schema_version = POSTURE_SCHEMA_VERSION, created = created, iso3 = iso3,
       country = x$Country[1], cells = cells)
}
