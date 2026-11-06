# Partnership Memos: orchestration and the agenda tables (compacts, trade, investment, third-country, deal
# pipeline, MEA assessment, non-priorities, evidence register). docs/partnership_memos.md.
#
# Pure functions: every input is passed in by scripts/34_build_partnership_memo.R.

PM_ROLE_CONTRIBUTION <- c(CIA = "capital and export finance", STM = "specialised technology and components",
                          SMI = "equipment and manufacturing at scale", DMA = "demand and offtake", RPA = "resources and processing",
                          CBH = "coordination across compacts", DSP = "scale-up hosting")

# ---- Orchestration ----------------------------------------------------------------------------------------

# inputs: list(sp, frame, dol, trade, ev, mech, inst, devpart, invest_pairs, croles, sroles, compacts)
pm_assess_pair <- function(inputs, pair_cfg, cfg) {
  a <- pair_cfg$a; b <- pair_cfg$b
  prof <- pm_country_profiles(inputs$sp, inputs$frame, inputs$dol, a, b, cfg)
  trade <- pm_bilateral_trade(inputs$trade, a, b)
  d <- pm_build_dyad(prof, a, b, trade, cfg)
  d <- pm_classify_dyad(d, cfg)
  d$evidence_quality <- round(pmin(d$evidence_quality_a, d$evidence_quality_b, na.rm = TRUE), 2)
  # node <-> cell links
  fr <- inputs$frame
  links <- do.call(rbind, lapply(seq_len(nrow(fr)), function(i) {
    z <- trimws(strsplit(as.character(fr$score_cells[i]), ";")[[1]]); z <- z[nzchar(z)]
    if (!length(z)) return(NULL)
    data.frame(node_id = fr$node_id[i], compact = fr$strategic_compact_id[i], cell = z, stringsAsFactors = FALSE)
  }))
  d$nodes_of_cell <- lapply(seq_len(nrow(d)), function(i) if (d$unit_type[i] == "cell") unique(links$node_id[links$cell == d$unit_id[i]]) else character())
  d$compacts_of_cell <- vapply(seq_len(nrow(d)), function(i) {
    if (d$unit_type[i] == "node") return(d$compact[i])
    z <- sort(unique(links$compact[links$cell == d$unit_id[i]])); if (!length(z)) NA_character_ else paste(z, collapse = ";")
  }, character(1))
  d$primary_compact <- vapply(seq_len(nrow(d)), function(i) {
    if (d$unit_type[i] == "node") return(d$compact[i])
    z <- links$compact[links$cell == d$unit_id[i]]; z <- z[!is.na(z)]; if (!length(z)) NA_character_ else names(sort(table(z), decreasing = TRUE))[1]
  }, character(1))
  d$n_steps <- fr$n_steps[match(d$node_id, fr$node_id)]
  lab <- setNames(fr$node_label, fr$node_id)
  d$cell_nodes <- vapply(seq_len(nrow(d)), function(i) { z <- unname(lab[d$nodes_of_cell[[i]]]); paste(utils::head(z[!is.na(z)], 3), collapse = ", ") }, character(1))
  d$node_label <- ifelse(d$unit_type == "cell", ifelse(nzchar(d$cell_nodes), sprintf("%s %s (%s)", d$tech, tolower(d$stage), tolower(d$cell_nodes)),
                                                       sprintf("%s %s", d$tech, tolower(d$stage))), d$node_label)
  d$aib_third_hosts <- vapply(seq_len(nrow(d)), function(i) {
    nodes <- if (d$unit_type[i] == "node") d$node_id[i] else d$nodes_of_cell[[i]]
    h <- unlist(strsplit(paste(fr$priority_investment_hosts[fr$node_id %in% nodes], collapse = ";"), ";\\s*"))
    h <- setdiff(unique(trimws(h[nzchar(trimws(h)) & h != "NA"])), c(a, b))
    paste(utils::head(h, 3), collapse = ";")
  }, character(1))
  ev <- inputs$ev
  d$evidence_ids <- pm_match_evidence(d, ev, c(a, b))
  d$evidence_id_list <- vapply(d$evidence_ids, function(z) paste(z, collapse = ";"), character(1))
  dp <- pm_shared_devpart(inputs$devpart, a, b)
  key <- ifelse(d$unit_type == "cell", d$unit_id, paste(d$tech, d$stage, sep = "|"))
  cnt <- table(paste(dp$technology, dp$stage, sep = "|"))
  d$third_country_n <- as.integer(ifelse(key %in% names(cnt), cnt[key], 0L))
  d$buyer_protection <- ifelse(is.na(d$buyer), NA_real_, ifelse(d$buyer == a, d$policy_protection_priority_a, d$policy_protection_priority_b))
  d <- pm_constraint(d, cfg)
  d <- pm_instruments(d, inputs$inst, cfg)
  d <- pm_fields(d, cfg, ev, inputs$mech, NULL, inputs$invest_pairs, inputs$croles)
  d$treatment <- unname(PM_TREATMENT[d$relationship_state])
  d$state_label <- pm_state_label(d$relationship_state, pair_cfg$a_short, pair_cfg$b_short)
  list(profiles = prof, dyad = d, devpart = dp, links = links)
}

# Shared third-country steps: both countries are donors at the same recipient x technology x stage, on a
# development or commercial route (scripts/93 rules; review and excluded rows are not used).
pm_shared_devpart <- function(devpart, a, b) {
  if (is.null(devpart) || !nrow(devpart)) return(data.frame())
  x <- devpart[devpart$donor_iso3 %in% c(a, b) & devpart$route %in% c("development", "commercial"), , drop = FALSE]
  k <- paste(x$recipient_iso3, x$technology, x$stage)
  both <- k %in% intersect(k[x$donor_iso3 == a], k[x$donor_iso3 == b])
  x[both, , drop = FALSE]
}

# ---- Profiles in wide form (Section 2) --------------------------------------------------------------------

pm_profile_table <- function(d, a, b) {
  cols <- c("unit_type", "unit_id", "tech", "stage", "rollup", "node_label", "compact")
  per <- c("position_band", "competitive_position", "world_share", "export_share", "rca", "production_position",
           "investment_position", "strategic_need", "need_band", "need_basis", "coercive_import_share", "prize",
           "strategic_choice", "policy_support_priority", "policy_protection_priority", "policy_momentum",
           "policy_alignment_label", "evidence_specificity", "evidence_quality", "data_year")
  out <- d[, c(cols, paste0(per, "_a"), paste0(per, "_b"))]
  names(out) <- sub("_a$", paste0("_", tolower(a)), sub("_b$", paste0("_", tolower(b)), names(out)))
  out
}

# ---- Compacts (Section 7) ---------------------------------------------------------------------------------

pm_compact_candidates <- function(d, compacts, croles, mech, cfg, a, b, ev = NULL) {
  nd <- d[d$unit_type == "node" & !is.na(d$compact), , drop = FALSE]
  ids <- unique(nd$compact)
  role <- function(cm, iso) { r <- croles[croles$strategic_compact_id == cm & croles$iso3 == iso, , drop = FALSE]; if (nrow(r)) r$compact_role[1] else NA_character_ }
  lead <- function(cm, iso) { r <- croles[croles$strategic_compact_id == cm & croles$iso3 == iso, , drop = FALSE]; if (nrow(r)) r$lead_node[1] else NA_character_ }
  rel <- function(cm, iso) { r <- croles[croles$strategic_compact_id == cm & croles$iso3 == iso, , drop = FALSE]; if (nrow(r)) as.numeric(r$compact_relevance[1]) else 0 }
  out <- do.call(rbind, lapply(ids, function(cm) {
    x <- nd[nd$compact == cm, , drop = FALSE]
    act <- x[x$relationship_state %in% PM_ACTIVE_STATES, , drop = FALSE]
    cmeta <- compacts[compacts$strategic_compact_id == cm, , drop = FALSE]
    m <- mech[vapply(strsplit(mech$scope_compacts, "\\|"), function(z) cm %in% z, logical(1)), , drop = FALSE]
    st <- table(factor(x$relationship_state, levels = PM_STATES))
    best <- if (nrow(act)) act[order(act$priority_rank), ][1, ] else x[order(x$priority_rank), ][1, ]
    data.frame(
      strategic_compact_id = cm, strategic_compact_name = if (nrow(cmeta)) cmeta$strategic_compact_name[1] else cm,
      n_nodes = nrow(x), n_joint_bets = sum(x$priority_tier == 1), n_coordinate = sum(x$priority_tier == 2),
      n_manage_competition = sum(x$priority_tier == 3), n_leave_open = sum(x$priority_tier == 4),
      state_mix = paste(sprintf("%s %d", names(st)[st > 0], st[st > 0]), collapse = "; "),
      lead_node = best$node_id, lead_node_state = best$relationship_state,
      max_urgency = suppressWarnings(max(act$urgency, na.rm = TRUE)),
      importance_sum = round(sum(act$strategic_importance, na.rm = TRUE), 3),
      role_a = role(cm, a), role_b = role(cm, b), lead_node_a = lead(cm, a), lead_node_b = lead(cm, b),
      relevance_a = round(rel(cm, a), 3), relevance_b = round(rel(cm, b), 3),
      aib_primary_mission = if (nrow(cmeta)) cmeta$primary_mission[1] else NA_character_,
      aib_class_mix = if (nrow(cmeta)) cmeta$class_mix[1] else NA_character_,
      osi_need_in_compact = any(x$need_band_a %in% c("high", "demand") | x$need_band_b %in% c("high", "demand")) ||
        any(d$unit_type == "cell" & d$primary_compact %in% cm & (d$need_band_a == "high" | d$need_band_b == "high"), na.rm = TRUE),
      n_evidence_items = if (is.null(ev)) 0L else sum(ev$compact == cm & ev$item_type %in% c("project", "financing", "corporate") &
                                                       vapply(strsplit(ev$countries, "\\|"), function(z) all(c(a, b) %in% z), logical(1))),
      mechanisms = paste(m$mechanism_id, collapse = ";"),
      mechanism_with_finance = any(m$has_finance %in% c("TRUE", "partial") & m$status %in% c("in_force", "signed")),
      mechanism_with_pipeline = any(m$has_pipeline %in% "TRUE" & m$status %in% c("in_force", "signed")),
      stringsAsFactors = FALSE)
  }))
  out$max_urgency[!is.finite(out$max_urgency)] <- 0
  both_roles <- !is.na(out$role_a) & !is.na(out$role_b)
  osi_priority <- out$n_joint_bets >= 1 & (out$n_joint_bets + out$n_coordinate) >= 2 & both_roles
  evidence_led <- !osi_priority & out$osi_need_in_compact & (out$mechanism_with_pipeline | out$n_evidence_items >= cfg$evidence_led_min_items)
  out$compact_fit <- ifelse(osi_priority, "bilateral priority", ifelse(evidence_led, "evidence-led candidate",
                     ifelse(out$n_joint_bets + out$n_coordinate >= 1, "secondary", "not a bilateral priority")))
  out$recommendation_basis <- ifelse(osi_priority, "osi", ifelse(evidence_led, "external", NA_character_))
  lv <- c("bilateral priority", "evidence-led candidate", "secondary", "not a bilateral priority")
  out <- out[order(match(out$compact_fit, lv), -(out$max_urgency >= 0.67), -out$n_joint_bets, -out$n_evidence_items,
                   -out$mechanism_with_pipeline, -out$max_urgency, -out$importance_sum, out$strategic_compact_id), ]
  out$compact_rank <- seq_len(nrow(out))
  elig <- out$compact_fit %in% lv[1:2]
  out$recommended <- elig & cumsum(elig) <= cfg$priority$max_compacts
  out
}

# Six-stage architecture of one compact: the best node per stage, or the technology cell where no node sits.
pm_compact_architecture <- function(d, cm, links) {
  nd <- d[d$unit_type == "node" & d$compact %in% cm, , drop = FALSE]
  techs <- names(sort(table(links$cell[links$compact == cm]), decreasing = TRUE))
  techs <- unique(sub("\\|.*$", "", techs))
  stages <- c("Extraction", "Refining", "Components", "Assembly", "Deployment", "Recovery")
  do.call(rbind, lapply(stages, function(s) {
    x <- nd[nd$stage == s, , drop = FALSE]
    if (nrow(x)) {
      x <- x[order(!x$relationship_state %in% PM_ACTIVE_STATES, !nzchar(x$evidence_id_list), x$priority_rank), ]
      r <- x[1, ]
      others <- if (nrow(x) > 1) paste(x$node_id[-1], collapse = ", ") else ""
      return(data.frame(strategic_compact_id = cm, stage = s, unit = r$node_id, other_units = others, basis = "AIB node",
                        position_a = r$position_band_a, position_b = r$position_band_b, relationship_state = r$relationship_state,
                        treatment = r$treatment, main_instrument = r$main_instrument, stringsAsFactors = FALSE))
    }
    c1 <- d[d$unit_type == "cell" & d$stage == s & d$tech %in% utils::head(techs, 1), , drop = FALSE]
    if (!nrow(c1)) return(data.frame(strategic_compact_id = cm, stage = s, unit = NA, other_units = "", basis = "not covered",
                                     position_a = NA, position_b = NA, relationship_state = NA, treatment = "no intervention",
                                     main_instrument = NA, stringsAsFactors = FALSE))
    r <- c1[1, ]
    data.frame(strategic_compact_id = cm, stage = s, unit = r$unit_id, other_units = "", basis = "OSI cell (no AIB node at this stage)",
               position_a = r$position_band_a, position_b = r$position_band_b, relationship_state = r$relationship_state,
               treatment = r$treatment, main_instrument = r$main_instrument, stringsAsFactors = FALSE)
  }))
}

# ---- Trade agenda (Section 4) -----------------------------------------------------------------------------

pm_trade_agenda <- function(d, cfg, a, b, names) {
  nm <- function(iso) if (iso == a) names$a_short else names$b_short
  act <- d[d$relationship_state %in% PM_ACTIVE_STATES, , drop = FALSE]
  rows <- list()
  for (i in seq_len(nrow(act))) {
    r <- act[i, ]
    if (r$relationship_state %in% c("a_supplies_b_needs", "b_supplies_a_needs", "capacity_constrained_trusted_supply")) {
      prot <- isTRUE(r$buyer_protection >= 0.67)
      type <- if (prot) "tariff_risk" else if (r$relationship_state == "capacity_constrained_trusted_supply") "procurement_recognition" else "predictability"
      txt <- switch(type,
        tariff_risk = sprintf("Bind market access for %s %s into %s: %s's protection priority on this node is in its top third (%.2f), which taxes the trusted supplier it needs.", nm(r$supplier), r$node_label, nm(r$buyer), nm(r$buyer), r$buyer_protection),
        procurement_recognition = sprintf("Recognise %s output of %s as trusted domestic-equivalent supply in %s procurement and content rules; capacity, not access, is the constraint.", nm(r$supplier), r$node_label, nm(r$buyer)),
        predictability = sprintf("Keep %s exports of %s to %s tariff-free and predictable; the buyer's need is met by the trusted supplier.", nm(r$supplier), r$node_label, nm(r$buyer)))
      rows[[length(rows) + 1]] <- data.frame(unit_type = r$unit_type, unit_id = r$unit_id, direction = sprintf("%s -> %s", r$supplier, r$buyer),
        action_type = type, action = txt, relationship_state = r$relationship_state, priority_rank = r$priority_rank, stringsAsFactors = FALSE)
    } else if (r$relationship_state == "complementary_specialization") {
      rows[[length(rows) + 1]] <- data.frame(unit_type = r$unit_type, unit_id = r$unit_id, direction = "both", action_type = "mutual_recognition",
        action = sprintf("Mutual recognition of qualification and standards along the %s chain so each side's specialised step serves the other.", pm_or(r$compact, r$tech)),
        relationship_state = r$relationship_state, priority_rank = r$priority_rank, stringsAsFactors = FALSE)
    } else if (r$relationship_state == "shared_vulnerability") {
      cs <- pmin(r$coercive_import_share_a, r$coercive_import_share_b, na.rm = TRUE)
      type <- if (isTRUE(r$chokepoint) && isTRUE(cs >= 0.3)) "anti_circumvention" else "common_standards"
      rows[[length(rows) + 1]] <- data.frame(unit_type = r$unit_type, unit_id = r$unit_id, direction = "both", action_type = type,
        action = if (type == "anti_circumvention") sprintf("Common origin and anti-circumvention rules for %s: both import heavily from %s, so a measure one side takes alone leaks through the other.", r$node_label, r$coercive_top_iso3)
                 else sprintf("Common specifications for %s so a joint supplier base can serve both markets.", r$node_label),
        relationship_state = r$relationship_state, priority_rank = r$priority_rank, stringsAsFactors = FALSE)
    }
  }
  out <- if (length(rows)) do.call(rbind, rows) else data.frame()
  if (nrow(out)) out <- utils::head(out[order(out$priority_rank), ], cfg$priority$max_trade_actions)
  # open sourcing: material importers of nodes with adequate trusted supply
  open <- d[which(d$relationship_state == "low_strategic_case" & grepl("adequate_trusted_supply", d$state_reason) &
              (d$buyer_a %in% TRUE | d$buyer_b %in% TRUE)), , drop = FALSE]
  osz <- pmax(ifelse(is.na(open$net_import_share_a), 0, open$net_import_share_a), ifelse(is.na(open$net_import_share_b), 0, open$net_import_share_b))
  open <- utils::head(open[order(open$unit_type != "node", -osz, open$unit_id), ], 6)
  if (nrow(open)) out <- rbind(out, data.frame(unit_type = open$unit_type, unit_id = open$unit_id, direction = "open", action_type = "open_sourcing",
    action = sprintf("Leave %s open to global sourcing: trusted supply is adequate and diversified, so a bilateral preference adds cost without security.", open$node_label),
    relationship_state = open$relationship_state, priority_rank = open$priority_rank, stringsAsFactors = FALSE))
  comp <- d[d$relationship_state == "direct_competition", , drop = FALSE]
  if (nrow(comp)) out <- rbind(out, data.frame(unit_type = comp$unit_type, unit_id = comp$unit_id, direction = "both", action_type = "no_preference",
    action = sprintf("No bilateral preference for %s: both are strong suppliers; keep the market contestable.", comp$node_label),
    relationship_state = comp$relationship_state, priority_rank = comp$priority_rank, stringsAsFactors = FALSE))
  if (nrow(out)) { out$trade_rank <- seq_len(nrow(out)); out$rec_id <- sprintf("T%02d", out$trade_rank) }
  out
}

pm_or <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x)) y else x

# ---- Investment agenda (Section 5) ------------------------------------------------------------------------

pm_private_actors <- function(ids, ev) {
  vapply(ids, function(z) {
    e <- ev[ev$evidence_id %in% z & ev$item_type %in% c("project", "corporate", "financing"), , drop = FALSE]
    if (!nrow(e)) return("")
    nm <- unique(trimws(unlist(strsplit(e$institution, ";|/"))))
    nm <- nm[!grepl("^(US|Japan|Govt|Government|U\\.S\\.|DoD|DOE|METI|JBIC|NEXI|JOGMEC|DFC|EXIM|Commerce|NRC|TVA|DLA)", nm)]
    paste(utils::head(nm, 3), collapse = "; ")
  }, character(1))
}

pm_horizon <- function(speed) ifelse(is.na(speed), NA, ifelse(speed >= 0.67, "12-24 months", ifelse(speed >= 0.5, "2-3 years", "3-6 years")))

PM_OBSTACLE <- c(geopolitical_chokepoint = "Non-coercive producers cannot match the incumbent's prices through a price trough",
                 trusted_partner_capacity_constraint = "No buyer will sign long-term volumes before capacity exists, and no producer builds before volumes",
                 strategic_manufacturing_gap = "No base and a thin domestic market",
                 emerging_bottleneck = "Lead times and qualification of new suppliers",
                 no_material_intervention = "None")

pm_investment_agenda <- function(d, cfg, ev, inst, a, b) {
  x <- d[which(d$relationship_state %in% PM_ACTIVE_STATES & d$investment_fit >= 0.33 & !d$constraint_type %in% "no_material_intervention" &
                 (d$strategic_importance >= 0.4 | d$aib_class %in% "BUILD")), , drop = FALSE]
  if (!nrow(x)) return(data.frame())
  # resource stages: capital follows the buyer into the supplier's resource (offtake-linked equity); manufacturing:
  # the capability owner invests near the buyer's demand
  resource <- x$stage %in% c("Extraction", "Recovery") | (x$stage == "Refining" & x$tech %in% c("Coal", "Gas", "Oil"))
  inv_inst <- inst$instrument_id[inst$family %in% c("investment", "finance", "development")]
  lane <- ifelse(x$relationship_state == "shared_vulnerability", "joint",
          ifelse(x$relationship_state == "capacity_constrained_trusted_supply" & nzchar(x$aib_third_hosts) & x$chokepoint, "joint",
          ifelse(xor(!is.na(x$supplier) & x$supplier == a, resource), sprintf("%s -> %s", a, b), sprintf("%s -> %s", b, a))))
  owner <- ifelse(lane == "joint", paste(a, b, sep = "+"), ifelse(resource, x$buyer, x$supplier))
  dest <- ifelse(lane == "joint", ifelse(nzchar(x$aib_third_hosts), paste0("third countries (", x$aib_third_hosts, ") and either partner"), "either partner"),
                 ifelse(resource, x$supplier, x$buyer))
  dem <- ifelse(x$unit_type == "node",
                sprintf("%s takes %.0f%% of allied demand (%.0f%% of allied net imports); AIB class %s",
                        ifelse(is.na(x$buyer), "Both", x$buyer),
                        100 * ifelse(!is.na(x$buyer) & x$buyer == a, x$demand_share_a, x$demand_share_b),
                        100 * ifelse(!is.na(x$buyer) & x$buyer == a, x$net_import_share_a, x$net_import_share_b), x$aib_class),
                sprintf("supply-chain need %.2f (%s) / %.2f (%s)", x$strategic_need_a, a, x$strategic_need_b, b))
  pub <- vapply(strsplit(x$instruments, ";"), function(z) { z <- z[z %in% inv_inst]; if (length(z)) z[1] else NA_character_ }, character(1))
  out <- data.frame(lane = lane, unit_type = x$unit_type, unit_id = x$unit_id, node_label = x$node_label, stage = x$stage,
    capability_owner = owner, destination = dest, investment_logic = ifelse(lane == "joint", "joint capacity outside the pair or pooled",
      ifelse(resource, "offtake-linked equity: the buyer invests in the supplier's resource", "the capability owner builds near the buyer's demand")), demand_case = dem,
    private_actors = pm_private_actors(x$evidence_ids, ev),
    public_instrument = ifelse(is.na(pub), x$main_instrument, pub),
    additionality = x$additionality, additionality_label = x$additionality_label,
    additionality_rationale = sprintf("market failure %.2f; existing commitment %.1f; market already working %.1f", x$market_failure_score, x$existing_commitment, x$market_already_working),
    key_obstacle = unname(PM_OBSTACLE[x$constraint_type]), time_horizon = pm_horizon(pmin(x$execution_speed, 0.5, na.rm = TRUE)),
    investment_fit = x$investment_fit, priority_rank = x$priority_rank, evidence_ids = x$evidence_id_list, stringsAsFactors = FALSE)
  out <- out[order(match(out$lane, unique(c(sprintf("%s -> %s", b, a), sprintf("%s -> %s", a, b), "joint"))), out$priority_rank), ]
  out$rec_id <- sprintf("I%02d", seq_len(nrow(out)))
  out
}

# ---- Third-country cooperation (Section 6) ----------------------------------------------------------------

pm_third_country <- function(dp, sroles, d, compacts_sel, cfg, a, b) {
  if (!nrow(dp)) return(data.frame())
  rc <- function(iso) {
    r <- sroles[sroles$iso3 == iso & sroles$primary_or_secondary %in% c("primary", "secondary"), , drop = FALSE]
    r <- r[order(-as.numeric(r$role_strength)), ]
    paste(utils::head(unname(PM_ROLE_CONTRIBUTION[r$role_code]), 2), collapse = " and ")
  }
  ka <- dp[dp$donor_iso3 == a, ]; kb <- dp[dp$donor_iso3 == b, ]
  k <- function(x) paste(x$recipient_iso3, x$technology, x$stage)
  m <- merge(ka, kb[, c("recipient_iso3", "technology", "stage", "route", "donor_tier", "donor_choice", "rank_donor", "evidence_quality")],
             by = c("recipient_iso3", "technology", "stage"), suffixes = c("_a", "_b"))
  own <- function(t) !is.na(t) & as.numeric(t) <= 4
  cm <- vapply(strsplit(ifelse(is.na(m$compact), "", m$compact), ";"), function(z) any(z %in% compacts_sel), logical(1))
  evr <- function(q) match(q, c("strong", "moderate", "weak"))
  out <- data.frame(recipient_iso3 = m$recipient_iso3, recipient = m$recipient, technology = m$technology, stage = m$stage,
    compact = m$compact, in_priority_compact = cm, recipient_income_group = m$recipient_income_group,
    recipient_choice = m$recipient_choice, recipient_anchor = m$catch_up_anchor_type,
    a_need = ifelse(own(m$donor_tier_a), sprintf("own %s need (%s)", m$donor_choice_a, a), "allied-system need only"),
    b_need = ifelse(own(m$donor_tier_b), sprintf("own %s need (%s)", m$donor_choice_b, b), "allied-system need only"),
    a_contribution = rc(a), b_contribution = rc(b), route_a = m$route_a, route_b = m$route_b,
    tools = m$recommended_tool_family, evidence_quality = ifelse(evr(m$evidence_quality_a) >= evr(m$evidence_quality_b), m$evidence_quality_a, m$evidence_quality_b),
    rank_sum = as.numeric(m$rank_donor_a) + as.numeric(m$rank_donor_b), stringsAsFactors = FALSE)
  out$both_own_need <- own(m$donor_tier_a) & own(m$donor_tier_b)
  out <- out[order(!out$both_own_need, !out$in_priority_compact, evr(out$evidence_quality), out$rank_sum, out$recipient_iso3), ]
  out <- utils::head(out, cfg$priority$max_third_country)
  out$rec_id <- sprintf("X%02d", seq_len(nrow(out)))
  out
}

# ---- Deal pipeline (Section 9) ----------------------------------------------------------------------------

pm_deal_pipeline <- function(d, inst, institutions, ev, cfg, a, b, names) {
  x <- d[which(d$priority_tier %in% c(1L, 2L) & !is.na(d$main_instrument) & !d$constraint_type %in% "no_material_intervention" &
                 (d$strategic_importance >= 0.4 | d$aib_class %in% "BUILD")), , drop = FALSE]
  # a cell is dropped where an active node already covers the same technology x stage
  node_keys <- paste(x$tech[x$unit_type == "node"], x$stage[x$unit_type == "node"], sep = "|")
  x <- x[!(x$unit_type == "cell" & x$unit_id %in% node_keys), , drop = FALSE]
  if (!nrow(x)) return(data.frame())
  il <- setNames(inst$label, inst$instrument_id)
  inst_by <- function(iso, fam) {
    r <- institutions[institutions$iso3 == iso & vapply(strsplit(institutions$instrument_families, "\\|"), function(z) fam %in% z, logical(1)), ]
    paste(r$abbreviation, collapse = ", ")
  }
  resp <- vapply(seq_len(nrow(x)), function(i) {
    f <- x$main_instrument_family[i]
    paste(c(sprintf("%s: %s", a, inst_by(a, f)), sprintf("%s: %s", b, inst_by(b, f))), collapse = "; ")
  }, character(1))
  scale <- ifelse(x$unit_type == "node" & !is.na(x$node_demand_usd), sprintf("allied demand US$%.1fbn", x$node_demand_usd / 1e9),
                  ifelse(!is.na(x$imports_usd_a) | !is.na(x$imports_usd_b),
                         sprintf("imports US$%.1fbn (%s) / US$%.1fbn (%s)", ifelse(is.na(x$imports_usd_a), 0, x$imports_usd_a) / 1e9, a,
                                 ifelse(is.na(x$imports_usd_b), 0, x$imports_usd_b) / 1e9, b), NA_character_))
  m12 <- inst$milestone_12m[match(x$main_instrument, inst$instrument_id)]
  o36 <- inst$outcome_36m[match(x$main_instrument, inst$instrument_id)]
  who <- ifelse(is.na(x$supplier), "joint", paste(x$supplier, "->", x$buyer))
  tp <- inst$deal_template[match(x$main_instrument, inst$instrument_id)]
  sup_nm <- ifelse(is.na(x$supplier), "non-coercive", ifelse(x$supplier == a, names$a_short, names$b_short))
  buy_nm <- ifelse(is.na(x$buyer), sprintf("%s and %s", names$a_short, names$b_short), ifelse(x$buyer == a, names$a_short, names$b_short))
  hosts <- ifelse(nzchar(x$aib_third_hosts) & (is.na(x$supplier) | x$relationship_state == "capacity_constrained_trusted_supply"),
                  sprintf(" (candidate hosts: %s)", gsub(";", ", ", x$aib_third_hosts)), "")
  nodetxt <- ifelse(x$unit_type == "node", tolower(x$node_label), ifelse(nzchar(x$cell_nodes), tolower(sprintf("%s %s (%s)", x$tech, x$stage, x$cell_nodes)),
                    tolower(sprintf("%s %s", x$tech, x$stage))))
  txt <- vapply(seq_len(nrow(x)), function(i) {
    s <- tp[i]; s <- gsub("{supplier}", sup_nm[i], s, fixed = TRUE); s <- gsub("{buyer}", buy_nm[i], s, fixed = TRUE)
    s <- gsub("{node}", nodetxt[i], s, fixed = TRUE); gsub("{hosts}", hosts[i], s, fixed = TRUE) }, character(1))
  out <- data.frame(unit_type = x$unit_type, unit_id = x$unit_id,
    deal = txt, compact = ifelse(x$unit_type == "node", x$compact, x$primary_compact),
    stage = x$stage, problem_solved = x$market_failure, constraint_type = x$constraint_type, instrument = x$main_instrument,
    responsible_institutions = resp, private_actors = pm_private_actors(x$evidence_ids, ev), scale = scale,
    milestone_12m = m12, outcome_36m = o36, confidence = pm_band(x$evidence_quality),
    importance = pm_band(x$strategic_importance), urgency = pm_band(x$urgency), tractability = pm_band(x$tractability, 0.6, 0.4),
    execution_speed = x$execution_speed, additionality = x$additionality_label, interdependency = x$interdependency,
    evidence_quality = x$evidence_quality, priority_tier = x$priority_tier, evidence_ids = x$evidence_id_list, stringsAsFactors = FALSE)
  lv <- function(v) match(v, c("high", "medium", "low", "unknown"))
  out <- out[order(lv(out$importance), lv(out$additionality), lv(out$urgency), lv(out$tractability), -out$execution_speed,
                   -out$interdependency, -out$evidence_quality, out$unit_id), ]
  # at most two deals per compact (or technology) and per instrument within it, so the pipeline is a portfolio
  grp <- ifelse(is.na(out$compact), sub("\\|.*$", "", out$unit_id), out$compact)
  keep <- ave(seq_len(nrow(out)), grp, FUN = seq_along) <= 2 & !duplicated(paste(grp, out$instrument))
  out <- utils::head(out[keep, ], cfg$priority$max_deals)
  out$rank <- seq_len(nrow(out)); out$rec_id <- sprintf("D%02d", out$rank)
  out
}

# ---- MEA assessment (Section 8) ---------------------------------------------------------------------------

pm_mea <- function(sel, mech, cfg) {
  covers <- function(cm, need) {
    m <- mech[vapply(strsplit(mech$scope_compacts, "\\|"), function(z) cm %in% z, logical(1)) & mech$status %in% c("in_force", "signed"), , drop = FALSE]
    switch(need, any = nrow(m) > 0,
           finance_or_pipeline = any(m$has_finance %in% c("TRUE", "partial") | m$has_pipeline %in% c("TRUE", "partial")),
           both = any(m$has_finance %in% "TRUE" & m$has_pipeline %in% "TRUE"))
  }
  per <- data.frame(strategic_compact_id = sel,
                    covered_any = vapply(sel, covers, logical(1), need = "any"),
                    covered_finance_or_pipeline = vapply(sel, covers, logical(1), need = "finance_or_pipeline"),
                    covered_full = vapply(sel, covers, logical(1), need = "both"), stringsAsFactors = FALSE)
  cross <- mech[mech$cross_sector %in% "TRUE" & mech$status %in% c("in_force", "signed"), , drop = FALSE]
  cross_kpi <- any(cross$has_kpis %in% "TRUE" & cross$has_review %in% "TRUE")
  cross_pipe <- cross[cross$has_pipeline %in% "TRUE", , drop = FALSE]
  share <- if (length(sel)) mean(per$covered_finance_or_pipeline) else 0
  option <- if (length(sel) && all(per$covered_full) && cross_kpi) "existing"
            else if (share >= cfg$mea$upgrade_min_coverage || nrow(cross_pipe) > 0) "upgrade" else "new_mea"
  host <- if (nrow(cross_pipe)) cross_pipe$mechanism_id[1] else NA_character_
  gaps <- character()
  if (!cross_kpi) gaps <- c(gaps, "no cross-sector KPI dashboard or review cycle")
  if (any(!per$covered_any)) gaps <- c(gaps, paste("no bilateral mechanism covers", paste(per$strategic_compact_id[!per$covered_any], collapse = ", ")))
  if (nrow(cross_pipe) && any(cross_pipe$reciprocal %in% "FALSE")) gaps <- c(gaps, sprintf("%s has a pipeline and finance but no reciprocal commitments or sector strategy", host))
  list(option = option, host_mechanism = host, coverage_share = round(share, 2), per_compact = per, gaps = gaps,
       cross_sector_with_kpis = cross_kpi)
}

# ---- What not to do (Section 10) --------------------------------------------------------------------------

pm_non_priorities <- function(d, mea, cfg, names, a, b) {
  nm <- function(iso) if (iso == a) names$a_short else names$b_short
  rows <- list()
  comp <- d[d$relationship_state == "direct_competition", , drop = FALSE]
  for (i in seq_len(nrow(comp))) {
    r <- comp[i, ]
    rows[[length(rows) + 1]] <- data.frame(unit_id = r$unit_id, kind = "duplication",
      text = sprintf("Do not co-subsidise new %s capacity: both are strong suppliers (%s %s, %s %s). %s", r$node_label,
                     nm(a), pm_pct(r$world_share_a), nm(b), pm_pct(r$world_share_b),
                     if (isTRUE(r$chokepoint | r$allied_short)) "More trusted capacity is needed, so agree who specialises in what rather than racing." else "Trusted supply is adequate: this is commercial competition."),
      weight = 3 + r$strategic_importance, stringsAsFactors = FALSE)
  }
  done <- d[which(d$relationship_state %in% PM_ACTIVE_STATES & d$existing_commitment >= 1), , drop = FALSE]
  for (i in seq_len(nrow(done))) {
    r <- done[i, ]
    rows[[length(rows) + 1]] <- data.frame(unit_id = r$unit_id, kind = "already_committed",
      text = sprintf("Do not put new public money behind %s investment that is already committed (evidence %s); support is additional only beyond it.", r$node_label, r$evidence_id_list),
      weight = 2.5, stringsAsFactors = FALSE)
  }
  def <- d[which(d$relationship_state == "low_strategic_case" & (d$aib_low %in% TRUE | grepl("large_yard", d$state_reason)) &
             ((pmax(d$policy_protection_priority_a, d$policy_protection_priority_b, na.rm = TRUE) >= 0.67) %in% TRUE |
                d$policy_alignment_label_a %in% "over_prioritized" | d$policy_alignment_label_b %in% "over_prioritized")), , drop = FALSE]
  for (i in seq_len(nrow(def))) {
    r <- def[i, ]
    who <- if (isTRUE(r$policy_protection_priority_a >= 0.67) || r$policy_alignment_label_a %in% "over_prioritized") nm(a) else nm(b)
    rows[[length(rows) + 1]] <- data.frame(unit_id = r$unit_id, kind = "do_not_defend",
      text = sprintf("Do not defend %s as a bilateral priority: trusted supply is adequate (%s), yet %s's observed protection or attention is high.", r$node_label,
                     if (r$unit_type == "node") paste("AIB", r$aib_class) else "a large-yard node", who),
      weight = 2, stringsAsFactors = FALSE)
  }
  low_need <- d[which(d$unit_type == "node" & d$relationship_state == "low_strategic_case" & grepl("supplier_without_buyer_need", d$state_reason)), , drop = FALSE]
  for (i in seq_len(nrow(utils::head(low_need[order(-pmax(low_need$world_share_a, low_need$world_share_b)), ], 3)))) {
    r <- low_need[order(-pmax(low_need$world_share_a, low_need$world_share_b)), ][i, ]
    rows[[length(rows) + 1]] <- data.frame(unit_id = r$unit_id, kind = "no_bilateral_need",
      text = sprintf("Do not build a bilateral programme around %s: one side is strong but the other has no material need for it; trade it commercially.", r$node_label),
      weight = 1.5, stringsAsFactors = FALSE)
  }
  if (mea$option != "new_mea") rows[[length(rows) + 1]] <- data.frame(unit_id = NA_character_, kind = "institution",
      text = sprintf("Do not create a new bilateral energy MOU or a free-standing MEA institution: %s already carries a pipeline and finance; dock the compacts in it.", mea$host_mechanism),
      weight = 2.8, stringsAsFactors = FALSE)
  out <- do.call(rbind, rows)
  out <- out[order(-out$weight, out$unit_id), ]
  out <- out[!duplicated(out$text), ]
  out <- utils::head(out, max(cfg$priority$min_non_priorities, 6))
  out$rec_id <- sprintf("N%02d", seq_len(nrow(out)))
  out
}

pm_pct <- function(x, digits = 0) ifelse(is.na(x), "n/a", paste0(formatC(100 * x, format = "f", digits = digits), "%"))

# ---- Evidence register (Section 11) -----------------------------------------------------------------------

PM_OSI_METRICS <- list(
  cell = list(c("competitive_position", "Competitive Position (contender percentile)", "strategic_position_tbl (scripts/26)"),
              c("strategic_need", "Supply-chain or deployment need", "strategic_position_tbl (scripts/26)"),
              c("strategic_choice", "Strategic choice (engine)", "strategic_position_tbl (scripts/26)"),
              c("policy_priority", "Revealed policy priority (NIPO / IEA PAMS)", "strategic_position_tbl (scripts/26)")),
  node = list(c("world_share", "World export or production share", "aib_compact_division_of_labour (scripts/33)"),
              c("net_import_share", "Share of allied net imports", "aib_compact_division_of_labour (scripts/33)"),
              c("strategic_need", "Supply-chain need inherited from the node's cell", "strategic_position_tbl (scripts/26)"),
              c("aib_class", "AIB node class under the China shock", "aib_node_classification (scripts/32)")))

pm_evidence_register <- function(recs, d, ev, mech, a, b) {
  rows <- list()
  for (i in seq_len(nrow(recs))) {
    r <- recs[i, ]
    u <- if (!is.na(r$unit_id)) d[d$unit_id == r$unit_id, , drop = FALSE] else d[0, ]
    if (nrow(u)) {
      u <- u[1, ]
      met <- PM_OSI_METRICS[[u$unit_type]]
      for (m in met) {
        va <- u[[paste0(m[1], "_a")]]; vb <- u[[paste0(m[1], "_b")]]; v <- u[[m[1]]]
        val <- if (!is.null(va)) sprintf("%s %s; %s %s", a, format(va), b, format(vb)) else format(v)
        rows[[length(rows) + 1]] <- data.frame(claim_id = r$rec_id, rec_type = r$rec_type, country_a = a, country_b = b,
          technology = u$tech, stage = u$stage, unit = u$unit_id, source_metric = m[2], osi_variable = m[1], value = val,
          year = ifelse(u$unit_type == "node", "2024", ifelse(is.na(u$data_year_a), "", u$data_year_a)), source = m[3],
          evidence_specificity = ifelse(u$unit_type == "node" & m[1] == "strategic_need", "proxy", ifelse(u$unit_type == "node", "exact", ifelse(is.na(u$stage_evidence_quality_a), "unknown", u$stage_evidence_quality_a))),
          inference = u$state_reason, recommendation = r$text, confidence = pm_band(u$evidence_quality),
          basis = "osi", external_ref = "", stringsAsFactors = FALSE)
      }
    }
    ids <- unlist(strsplit(ifelse(is.na(r$evidence_ids), "", r$evidence_ids), ";"))
    for (e in ids[nzchar(ids)]) {
      x <- ev[ev$evidence_id == e, , drop = FALSE]; if (!nrow(x)) next
      rows[[length(rows) + 1]] <- data.frame(claim_id = r$rec_id, rec_type = r$rec_type, country_a = a, country_b = b,
        technology = x$technology, stage = x$stage, unit = ifelse(nrow(u), u$unit_id, ""), source_metric = x$name, osi_variable = "",
        value = x$claim, year = substr(x$publication_date, 1, 4), source = x$source, evidence_specificity = x$verification,
        inference = "", recommendation = r$text, confidence = x$verification, basis = "external",
        external_ref = sprintf("%s %s (accessed %s)", x$evidence_id, x$url, x$access_date), stringsAsFactors = FALSE)
    }
  }
  out <- do.call(rbind, rows)
  out
}
