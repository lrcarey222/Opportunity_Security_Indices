# The six-stage posture bundle of the Explorer (dashboard/public/data/posture): the decision engine's
# output (scripts/26) in the shape the Country, Sector and AIB headline charts read
# (dashboard/src/posture/contract.ts). One core document with every country x technology x six-stage
# cell in columns and the allied division of labour, and one detail document per country for the cell
# drawer. Pure: scripts/91_export_posture_data.R writes the files.

POSTURE_SCHEMA <- "osi-posture"
POSTURE_SCHEMA_VERSION <- 1L
POSTURE_CODE <- c(secure = "S", compete = "C", deploy = "D")

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

# The core document. tbl: strategic_position_tbl (scripts/26). division: strategic_aib_division_tbl.
# countries: iso3, name, tier (coalition tier: core / partner / none). cfg: config/strategic_position.yml
# (headline_techs, strategic_posture.decision_quadrant).
posture_core_doc <- function(tbl, division, countries, cfg, created, git_commit = NA_character_, momentum_band = 0.1) {
  techs <- unlist(cfg$headline_techs %||% sort(unique(tbl$tech)))
  stages <- SUPPLY_CHAIN_STAGES
  x <- tbl[!is.na(tbl$strategic_posture), , drop = FALSE]
  x <- x[order(x$iso3, match(x$tech, techs), match(x$supply_chain, stages)), , drop = FALSE]
  isos <- sort(unique(x$iso3))
  cty <- countries[match(isos, countries$iso3), , drop = FALSE]
  cty$iso3 <- isos
  cty$name <- dplyr::coalesce(cty$name, x$Country[match(isos, x$iso3)])
  cty$tier <- dplyr::coalesce(cty$tier, "none")
  q <- cfg$strategic_posture$decision_quadrant %||% list()
  col <- function(n) if (n %in% names(x)) x[[n]] else rep(NA, nrow(x))
  cells <- list(
    c = match(x$iso3, isos) - 1L,
    t = match(x$tech, techs) - 1L,
    s = match(x$supply_chain, stages) - 1L,
    p = unname(POSTURE_CODE[x$strategic_posture]),
    v = x$posture_variant,
    m = col("compete_mode"),
    q = col("decision_quadrant"),
    need = posture_r3(col("strategic_need_percentile")),
    opp = posture_r3(col("opportunity_percentile")),
    cp = posture_r3(col("competitive_position_percentile")),
    share = posture_r3(col("home_share")),
    al = col("policy_alignment"),
    alf = col("policy_alignment_flag"),
    conf = col("posture_confidence"),
    ev = col("stage_evidence_quality_cp"),
    sec = col("security_rationale"),
    bd = col("quadrant_borderline"),
    fl = col("quadrant_consistency_flag"),
    p1 = col("recommended_partner_1"),
    gap = col("partner_gap"),
    mom = posture_momentum_arrow(col("trade_momentum_raw"), momentum_band),
    mq = col("trade_momentum_quality")
  )
  cells <- lapply(cells, function(v) I(unname(v)))
  div <- division[division$tech %in% techs, , drop = FALSE]
  members <- countries$iso3[countries$tier %in% c("core", "partner")]
  priority <- div$partner_demand + div$secure_demand > 0
  covered <- !div$aib_gap %in% c("no_trusted_supplier", "single_point_of_failure")
  list(
    schema = POSTURE_SCHEMA,
    schema_version = POSTURE_SCHEMA_VERSION,
    created = created,
    git_commit = git_commit,
    meta = list(
      techs = I(techs), stages = I(stages),
      benchmark_period = unique(stats::na.omit(col("trade_benchmark_period")))[1],
      momentum_period = unique(stats::na.omit(col("trade_momentum_period")))[1],
      momentum_previous_period = unique(stats::na.omit(col("trade_momentum_previous_period")))[1],
      momentum_through = posture_through(col("trade_momentum_period")),
      momentum_band = momentum_band,
      need_threshold = q$need_threshold %||% 0.67,
      opportunity_threshold = q$opportunity_threshold %||% 0.67,
      borderline_band = q$borderline_band %||% 0.05,
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
      leader = I(div$world_leader), leader_share = I(posture_r3(div$world_leader_share)),
      gap = I(div$aib_gap), gaps = I(div$aib_gaps), source = I(div$capability_source), aib_nodes = I(div$aib_n_nodes),
      partner_demand = I(div$partner_demand), secure_demand = I(div$secure_demand), partner_gaps = I(div$partner_gap_countries),
      member_partner_demand = I(div$partner_demand_members)
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

# The short summary of a decision trace's tests: id, state and detail.
posture_trace_tests <- function(trace) {
  if (is.na(trace) || !nzchar(trace)) return(NULL)
  j <- tryCatch(jsonlite::fromJSON(trace, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(j)) return(NULL)
  lapply(j$tests, function(t) list(id = t$id, state = t$state %||% NA, detail = t$detail %||% NA))
}

# The detail of the test that decided the posture: the route test for Secure, the quadrant otherwise.
posture_trace_reason <- function(trace, posture) {
  tests <- posture_trace_tests(trace)
  if (is.null(tests)) return(NULL)
  ids <- vapply(tests, function(t) t$id, "")
  st <- vapply(tests, function(t) as.character(t$state %||% NA), "")
  pick <- if (identical(posture, "secure")) which(ids %in% c("t2_vulnerability", "t2_dual_use", "t2_resilience") & st == "yes")[1] else which(ids == "quadrant")[1]
  if (is.na(pick)) return(NULL)
  tests[[pick]]$detail
}

# One country's drawer document: per cell, what the drawer shows for its posture. The policy design
# text is in the core document's design dictionary (keyed variant|delivery).
posture_detail_doc <- function(tbl, iso3, cfg, created) {
  techs <- unlist(cfg$headline_techs %||% sort(unique(tbl$tech)))
  x <- tbl[tbl$iso3 == iso3 & !is.na(tbl$strategic_posture), , drop = FALSE]
  x <- x[order(match(x$tech, techs), match(x$supply_chain, SUPPLY_CHAIN_STAGES)), , drop = FALSE]
  g <- function(r, n) if (n %in% names(r) && length(r[[n]]) && !is.na(r[[n]])) r[[n]] else NULL
  num <- function(r, n) { v <- g(r, n); if (is.null(v)) NULL else signif(as.numeric(v), 3) }
  cells <- lapply(seq_len(nrow(x)), function(i) {
    r <- x[i, , drop = FALSE]
    partners <- Filter(Negate(is.null), lapply(1:3, function(k) {
      p <- g(r, paste0("recommended_partner_", k))
      if (is.null(p)) return(NULL)
      list(iso3 = p, capability = num(r, paste0("recommended_partner_", k, "_capability")),
           cp = num(r, paste0("recommended_partner_", k, "_cp")), relationship = num(r, paste0("recommended_partner_", k, "_relationship")),
           diversification = num(r, paste0("recommended_partner_", k, "_diversification")))
    }))
    out <- list(
      tech = r$tech, stage = r$supply_chain, posture = r$strategic_posture, variant = r$posture_variant,
      mode = g(r, "compete_mode"), quadrant = g(r, "decision_quadrant"), confidence = g(r, "posture_confidence"),
      possible = g(r, "posture_possible"), pivotal = g(r, "pivotal_inputs"), delivery = g(r, "delivery_mode"),
      security_rationale = g(r, "security_rationale"), security_routes = g(r, "security_routes_met"),
      need = num(r, "strategic_need_percentile"), opportunity = num(r, "opportunity_percentile"),
      opportunity_basis = g(r, "opportunity_basis"), cp = num(r, "competitive_position_percentile"),
      home_share = num(r, "home_share"), trusted_share = num(r, "trusted_world_share"),
      coercive_top = g(r, "coercive_top_iso3"), coercive_top_share = num(r, "coercive_top_share"),
      coercive_import_share = num(r, "coercive_import_share"),
      borderline = g(r, "quadrant_borderline"), consistency_flag = g(r, "quadrant_consistency_flag"),
      alignment = g(r, "policy_alignment"), alignment_flag = g(r, "policy_alignment_flag"),
      policy_priority = num(r, "policy_priority_percentile"),
      partners = if (length(partners)) partners else NULL,
      partner_gap = g(r, "partner_gap"), partner_reason = g(r, "partner_reason"), partner_evidence = g(r, "partner_evidence_quality"),
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
      # the deciding test's words (Secure: Test 2; else the quadrant), not the whole trace
      reason = posture_trace_reason(r$decision_trace, r$strategic_posture)
    )
    Filter(Negate(is.null), out)
  })
  list(schema = POSTURE_SCHEMA, schema_version = POSTURE_SCHEMA_VERSION, created = created, iso3 = iso3,
       country = x$Country[1], cells = cells)
}
