# Partnership Memos: Compact memos (docs/partnership_memos.md, "Compact memos").
#
# A Compact memo asks the bilateral memo's question for a group: across the allied economies with a stake in one
# Strategic Sectoral Compact (or a cross-cutting compact such as critical minerals), where does coordinated
# industrial integration create strategic value, through which instruments, and where should the compact stay out?
#
# The grain is the AIB electro-industrial node. Each member's node profile is the bilateral memo's own
# (pm_node_profile(): position band, share, inherited need, demand). The node's compact state is then read across
# all members by an ordered rule list that mirrors the bilateral one:
#
#   1. not_assessed        no member has a measured position or need at the node.
#   2. member_supply / capacity_constrained_trusted_supply
#                          a capable member supplier outranks at least one member buyer (high need, or material
#                          demand where a chokepoint or an allied shortage exists); capacity-constrained when the
#                          AIB class is BUILD (allied capacity short of demand under the China shock).
#   3. shared_vulnerability no member supplier outranks a buyer, at least two members need the node (one with high
#                          need) and a coercive supplier holds >= the chokepoint share (or the buyers import >= 40%
#                          from coercive suppliers).
#   4. member_competition  two or more members are strong and no member needs what they make: managed competition
#                          (MANAGE SPECIALIZATION where more trusted capacity is needed).
#   5. low_strategic_case  everything else, with reasons.
#
# Constraint type and instrument reuse the bilateral functions through each state's bilateral analogue. Fields
# stay separate (importance, urgency, tractability, additionality, speed, interdependency, evidence); priority is
# lexicographic. Pure functions: no IO.

PM_COMPACT_STATES <- c("member_supply", "capacity_constrained_trusted_supply", "shared_vulnerability", "member_competition",
                       "low_strategic_case", "not_assessed")
PM_COMPACT_ACTIVE <- c("member_supply", "capacity_constrained_trusted_supply", "shared_vulnerability")
PM_COMPACT_RULE_STATE <- c(member_supply = "a_supplies_b_needs", capacity_constrained_trusted_supply = "capacity_constrained_trusted_supply",
                           shared_vulnerability = "shared_vulnerability", member_competition = "direct_competition",
                           low_strategic_case = "low_strategic_case", not_assessed = "not_assessed")
PM_COMPACT_TREATMENT <- c(member_supply = "COORDINATE", capacity_constrained_trusted_supply = "BUILD", shared_vulnerability = "BUILD",
                          member_competition = "MANAGE SPECIALIZATION", low_strategic_case = "LOW INTERVENTION", not_assessed = "INSUFFICIENT DATA")
PM_COMPACT_STATE_LABEL <- c(member_supply = "Member supply meets member need", capacity_constrained_trusted_supply = "Capacity-constrained trusted supply",
                            shared_vulnerability = "Shared vulnerability", member_competition = "Member competition",
                            low_strategic_case = "Low strategic case", not_assessed = "Not assessed")

pm_names_of <- function(iso, names) { v <- unname(names[iso]); ifelse(is.na(v), iso, v) }
# The parties of each node's lane: its member suppliers and buyers (the buyers alone where none supplies).
pm_lane_parties <- function(x) lapply(seq_len(nrow(x)), function(i) {
  z <- c(strsplit(x$lane_suppliers[i], ";")[[1]], strsplit(if (nzchar(x$lane_buyers[i])) x$lane_buyers[i] else x$buyers[i], ";")[[1]])
  unique(z[!is.na(z) & nzchar(z)]) })
# Evidence ids (per node) that name at least one of the node's parties; all ids where no party is known.
pm_ids_of_parties <- function(ids_list, ev, parties_list) lapply(seq_along(ids_list), function(i) {
  e <- ev[ev$evidence_id %in% ids_list[[i]], , drop = FALSE]; p <- parties_list[[i]]
  if (!length(p)) return(e$evidence_id)
  e$evidence_id[vapply(strsplit(e$countries, "\\|"), function(z) any(z %in% p), logical(1))] })
# lower-case a label's first letter for use inside a sentence, keeping acronyms (PV, EMD, PGMs)
pm_lc1 <- function(x) ifelse(grepl("^[A-Z][a-z]", x), paste0(tolower(substr(x, 1, 1)), substring(x, 2)), x)
pm_join <- function(x, n = 3, last = " and ") {
  x <- unique(x[!is.na(x) & nzchar(x)]); if (!length(x)) return("")
  k <- length(x) - n
  if (k > 0) return(sprintf("%s%s%d other%s", paste(utils::head(x, n), collapse = ", "), last, k, if (k > 1) "s" else ""))
  if (length(x) == 1) x else paste0(paste(x[-length(x)], collapse = ", "), last, x[length(x)])
}

# ---- Definition, members and profiles ---------------------------------------------------------------------

# The compact's node frame. `ccfg`: config compacts[[id]]: either source_compacts (an AIB compact, its own steps)
# or nodes + steps (a cross-cutting compact: steps[] = list(order, step, nodes)). The source AIB compact of each
# node is kept as `source_compact`.
pm_compact_frame <- function(frame, id, ccfg) {
  frame$source_compact <- frame$strategic_compact_id
  if (!is.null(ccfg$steps)) {
    st <- do.call(rbind, lapply(ccfg$steps, function(s) data.frame(node_id = unlist(s$nodes), value_chain_order = as.integer(s$order),
                                                                    value_chain_step = s$step, stringsAsFactors = FALSE)))
    fr <- frame[frame$node_id %in% st$node_id, , drop = FALSE]
    fr$value_chain_order <- st$value_chain_order[match(fr$node_id, st$node_id)]
    fr$value_chain_step <- st$value_chain_step[match(fr$node_id, st$node_id)]
    fr$n_steps <- length(unique(st$value_chain_order))
  } else {
    src <- unlist(if (is.null(ccfg$source_compacts)) id else ccfg$source_compacts)
    fr <- frame[frame$strategic_compact_id %in% src, , drop = FALSE]
  }
  fr$strategic_compact_id <- id
  fr[order(pm_num(fr$value_chain_order), fr$node_id), , drop = FALSE]
}

# Members: allied economies with a presence (>= presence share, anchor or specialist) or material demand
# (>= material net-import share, or >= material demand share with net imports) at any compact node, ranked by
# demand-weighted relevance (position plus net imports). `core`: the AIB core countries of the source compacts.
pm_compact_members <- function(frame_c, dol, croles, core, names, cfg) {
  th <- cfg$thresholds$node
  x <- dol[dol$node_id %in% frame_c$node_id & pm_lgl(dol$allied) %in% TRUE, , drop = FALSE]
  ps <- pm_num(x$position_share); ni <- pm_num(x$net_import_share); dm <- pm_num(x$scope_demand_share)
  ps[is.na(ps)] <- 0; ni[is.na(ni)] <- 0; dm[is.na(dm)] <- 0
  pres <- ps >= th$presence_share | pm_lgl(x$anchor_position) %in% TRUE | pm_lgl(x$specialist_position) %in% TRUE
  buy <- ni >= th$material_net_import_share | (dm >= th$material_demand_share & ni > 0)
  x <- x[pres | buy, , drop = FALSE]
  if (!nrow(x)) return(data.frame(iso3 = character(), country = character(), relevance = numeric(), stringsAsFactors = FALSE))
  nw <- stats::setNames(pm_num(frame_c$demand_usd), frame_c$node_id); nw[is.na(nw)] <- 0
  nw <- if (sum(nw) > 0) nw / sum(nw) else nw + 1 / length(nw)
  v <- (pmin(1, pm_num(x$position_share)) + pm_num(x$net_import_share)) * unname(nw[x$node_id])
  v[is.na(v)] <- 0
  rel <- tapply(v, x$iso3, sum)
  sup <- tapply(pm_num(x$position_share) >= th$presence_share | pm_lgl(x$anchor_position) %in% TRUE, x$iso3, sum)
  byr <- tapply(pm_num(x$net_import_share) >= th$material_net_import_share, x$iso3, sum)
  iso <- names(sort(rel, decreasing = TRUE))
  role <- vapply(iso, function(i) {
    r <- croles[croles$iso3 == i & croles$strategic_compact_id %in% unique(frame_c$source_compact), , drop = FALSE]
    if (!nrow(r)) return(NA_character_)
    r <- r[order(-pm_num(r$compact_relevance)), , drop = FALSE]
    if (length(unique(frame_c$source_compact)) > 1) sprintf("%s (%s)", r$compact_role[1], r$strategic_compact_id[1]) else r$compact_role[1]
  }, character(1))
  data.frame(iso3 = iso, country = pm_names_of(iso, names), relevance = round(as.numeric(rel[iso]), 4),
             n_presence_nodes = as.integer(sup[iso]), n_buyer_nodes = as.integer(byr[iso]), compact_role = unname(role),
             core = iso %in% core, stringsAsFactors = FALSE)
}

# Member x node profiles (the bilateral pm_node_profile per member) and the members' cell profiles.
pm_compact_profiles <- function(sp, frame_c, dol, members, cfg) {
  out <- list(); cells <- list()
  for (iso in members) {
    cp <- tryCatch(pm_cell_profile(sp, iso, cfg), error = function(e) NULL)
    if (is.null(cp)) next
    cells[[iso]] <- cp
    out[[iso]] <- pm_node_profile(frame_c, dol, iso, cp, cfg)
  }
  list(nodes = do.call(rbind, out), cells = do.call(rbind, cells))
}

# ---- Evidence --------------------------------------------------------------------------------------------

# Evidence ids per node: items naming at least one member (or the bloc a member belongs to); node items match that
# node only; compact-level items match the compact (or the node's source compact) at the node's stage.
pm_compact_match_evidence <- function(frame_c, ev, parties, id) {
  if (is.null(ev) || !nrow(ev)) return(replicate(nrow(frame_c), character(), simplify = FALSE))
  hit_party <- vapply(strsplit(ev$countries, "\\|"), function(z) any(z %in% parties), logical(1))
  lapply(seq_len(nrow(frame_c)), function(i) {
    hn <- nzchar(ev$aib_node) & ev$aib_node == frame_c$node_id[i]
    hc <- !nzchar(ev$aib_node) & ev$compact %in% c(id, frame_c$source_compact[i]) & ev$stage == frame_c$supply_chain[i]
    ev$evidence_id[hit_party & (hn | hc)]
  })
}

# The compact's own evidence (any node, the compact, a source compact at an upstream stage for a cross-cutting
# compact), for Section 1 and the evidence lists.
pm_compact_evidence_ids <- function(frame_c, ev, parties, id, cross_cutting = FALSE) {
  if (is.null(ev) || !nrow(ev)) return(character())
  hit_party <- vapply(strsplit(ev$countries, "\\|"), function(z) any(z %in% parties), logical(1))
  src <- unique(frame_c$source_compact)
  stg <- unique(frame_c$supply_chain)
  hit <- ev$compact == id | (nzchar(ev$aib_node) & ev$aib_node %in% frame_c$node_id) |
    (!nzchar(ev$aib_node) & ev$compact %in% src & (if (cross_cutting) ev$stage %in% stg else TRUE))
  ev$evidence_id[hit_party & hit]
}

# ---- Node assessment -------------------------------------------------------------------------------------

# prof: pm_compact_profiles()$nodes; cells: its $cells; flows: aib_inputs$flows; mech: active mechanisms covering
# the compact among members (pm_compact_mechanisms()); devpart: scripts/93 table (or NULL).
pm_compact_assess <- function(prof, cells, frame_c, flows, members, cfg, ev, mech, inst, devpart = NULL, parties = members) {
  th <- cfg$thresholds
  rk <- function(x) unname(PM_POSITION_RANK[ifelse(is.na(x), "not_measured", x)])
  mx <- function(v) { v <- v[!is.na(v)]; if (length(v)) max(v) else NA_real_ }
  mn <- function(v) { v <- v[!is.na(v)]; if (length(v)) min(v) else NA_real_ }
  ev_ids <- pm_compact_match_evidence(frame_c, ev, parties, frame_c$strategic_compact_id[1])
  hit_party <- vapply(strsplit(ev$countries, "\\|"), function(z) any(z %in% parties), logical(1))
  ev_node <- lapply(frame_c$node_id, function(n) ev$evidence_id[hit_party & nzchar(ev$aib_node) & ev$aib_node == n])
  dcap_of <- function(choke) ifelse(choke, 1, cfg$demand_importance_cap)
  # mechanisms: 1 an active mechanism with finance covers the node's compact, 0.5 one without
  mech_score <- function(src) {
    m <- mech[vapply(strsplit(mech$scope_compacts, "\\|"), function(z) any(z %in% c(frame_c$strategic_compact_id[1], src)), logical(1)), , drop = FALSE]
    if (!nrow(m)) 0 else if (any(m$has_finance %in% c("TRUE", "partial"))) 1 else 0.5
  }
  dp <- if (is.null(devpart) || !nrow(devpart)) NULL else devpart[devpart$donor_iso3 %in% members & devpart$route %in% c("development", "commercial"), , drop = FALSE]
  rows <- lapply(seq_len(nrow(frame_c)), function(i) {
    f <- frame_c[i, ]
    p <- prof[prof$node_id == f$node_id, , drop = FALSE]
    choke <- isTRUE(pm_num(f$coercive_world_share) >= th$chokepoint_share)
    severe <- isTRUE(pm_num(f$coercive_world_share) >= th$severe_chokepoint_share)
    short <- isTRUE(f$node_classification == "BUILD")
    more_cap <- choke || short
    named <- (p$largest_trusted %in% TRUE) | !is.na(p$host_rank)
    share_ok <- !is.na(p$world_share) & p$world_share >= th$node$presence_share
    # a material net importer with high need is short of the node itself: a buyer, never a supplier
    # a "moderate" supplier must also export the node: a producer for its home market does not supply another member
    # (2026-11-17: US wind turbine sets, 7% of world output, 1% of exports)
    exp_ok <- is.na(p$export_share) | p$export_share >= th$node$presence_share
    capable <- (p$position_band == "strong" | (p$position_band == "moderate" & named & share_ok & exp_ok)) & !p$need_band %in% "high"
    need_v <- p$need_band == "high" | (p$need_band == "demand" & more_cap)
    dem <- ifelse(is.na(p$demand_share), 0, p$demand_share)
    ns <- ifelse(p$need_band == "high", p$strategic_need, ifelse(p$need_band == "demand", pmin(dcap_of(choke), dem / th$node$demand_scale_share), 0))
    ns[is.na(ns)] <- 0
    p$need_score <- ns
    sup <- p[capable, , drop = FALSE]; sup <- sup[order(-rk(sup$position_band), -sup$world_share), , drop = FALSE]
    buy <- p[need_v & !capable & p$position_band != "strong", , drop = FALSE]; buy <- buy[order(-buy$need_score, -ifelse(is.na(buy$demand_share), 0, buy$demand_share)), , drop = FALSE]
    # lanes: a capable supplier that outranks a buyer
    lanes <- if (nrow(sup) && nrow(buy)) {
      g <- expand.grid(s = seq_len(nrow(sup)), b = seq_len(nrow(buy)))
      g <- g[sup$iso3[g$s] != buy$iso3[g$b] & rk(sup$position_band[g$s]) > rk(buy$position_band[g$b]), , drop = FALSE]
      g
    } else data.frame(s = integer(), b = integer())
    eff_sup <- sup[unique(lanes$s), , drop = FALSE]
    eff_buy <- buy[unique(lanes$b), , drop = FALSE]
    n_strong <- sum(p$position_band == "strong", na.rm = TRUE)
    measured <- any(p$position_band != "not_measured" | p$need_band != "not_measured", na.rm = TRUE)
    buy_cs <- if (nrow(buy)) { w <- ifelse(is.na(buy$demand_share) | buy$demand_share <= 0, 1, buy$demand_share); v <- buy$coercive_import_share; ok <- !is.na(v); if (any(ok)) sum(v[ok] * w[ok]) / sum(w[ok]) else NA_real_ } else NA_real_
    reason <- character()
    state <- if (!nrow(p) || !measured) { reason <- "no member has a measured position or need"; "not_assessed" }
      else if (nrow(lanes)) {
        if (short) { reason <- sprintf("%s supply %s, which allied capacity cannot cover under the China shock (AIB BUILD)", pm_join(eff_sup$iso3, 3), pm_join(eff_buy$iso3, 4)); "capacity_constrained_trusted_supply" }
        else { reason <- sprintf("%s can supply the need of %s", pm_join(eff_sup$iso3, 3), pm_join(eff_buy$iso3, 4)); "member_supply" }
      } else if (nrow(buy) >= 2 && any(buy$need_band == "high") && (choke || isTRUE(buy_cs >= th$both_dependent_coercive_share))) {
        reason <- if (choke) sprintf("%d members need the node, none supplies it, and %s holds %.0f%% of world supply", nrow(buy), f$coercive_top_iso3, 100 * pm_num(f$coercive_world_share))
                  else sprintf("%d members need the node, none supplies it, and the buyers import %.0f%% from coercive suppliers", nrow(buy), 100 * buy_cs)
        "shared_vulnerability"
      } else if (n_strong >= 2) {
        reason <- if (more_cap) sprintf("%d members are strong suppliers and no other member needs the node; more trusted capacity is needed, so coordinate specialisation", n_strong)
                  else sprintf("%d members are strong suppliers and trusted supply is adequate: managed competition", n_strong)
        "member_competition"
      } else {
        r <- character()
        if (!nrow(buy)) r <- c(r, "no_member_need")
        if (nrow(buy) && !nrow(sup)) r <- c(r, "need_without_member_capability")
        if (nrow(buy) && nrow(sup)) r <- c(r, "suppliers_do_not_outrank_buyers")
        if (!more_cap || isTRUE(f$node_classification == "LOW INTERVENTION")) r <- c(r, "adequate_trusted_supply")
        if (nrow(sup) && !nrow(buy)) r <- c(r, "supplier_without_member_need")
        reason <- paste(unique(r), collapse = ";"); "low_strategic_case"
      }
    # bilateral flows among members at the node: the buyers' imports from the effective suppliers
    fl <- flows[flows$node_id == f$node_id, , drop = FALSE]
    bi <- if (nrow(eff_buy)) sum(fl$value[fl$importer_iso %in% eff_buy$iso3]) else 0
    bs <- if (nrow(eff_buy) && nrow(eff_sup)) sum(fl$value[fl$importer_iso %in% eff_buy$iso3 & fl$exporter_iso %in% eff_sup$iso3]) else 0
    from_members <- if (bi > 0) bs / bi else NA_real_
    # deployment need of the node's technology among members (emerging-bottleneck test)
    dn <- cells[cells$stage == "Deployment" & cells$tech %in% f$primary_tech, , drop = FALSE]
    dn_max <- suppressWarnings(max(dn$strategic_need, na.rm = TRUE)); if (!is.finite(dn_max)) dn_max <- NA_real_
    dn_hi <- dn$iso3[!is.na(dn$strategic_need) & dn$strategic_need >= th$deployment_need_high]
    # third-country steps: recipients where >= 2 members are donors in the node's cells
    keys <- trimws(strsplit(as.character(f$score_cells), ";")[[1]]); keys <- keys[nzchar(keys) & keys != "NA"]
    n3 <- if (is.null(dp) || !length(keys)) 0L else {
      x <- dp[paste(dp$technology, dp$stage, sep = "|") %in% keys, , drop = FALSE]
      if (!nrow(x)) 0L else { k <- tapply(x$donor_iso3, x$recipient_iso3, function(z) length(unique(z))); sum(k >= 2) }
    }
    hosts <- trimws(strsplit(paste(f$priority_investment_hosts, collapse = ";"), ";")[[1]]); hosts <- hosts[nzchar(hosts) & hosts != "NA"]
    data.frame(
      node_id = f$node_id, node_label = f$node_label, compact = f$strategic_compact_id, source_compact = f$source_compact,
      step = f$value_chain_step, step_order = as.integer(pm_num(f$value_chain_order)), n_steps = as.integer(pm_num(f$n_steps)),
      stage = f$supply_chain, tech = f$primary_tech, unit_type = "node", unit_id = f$node_id,
      relationship_state = state, rule_state = unname(PM_COMPACT_RULE_STATE[state]), state_reason = paste(reason, collapse = ";"),
      suppliers = paste(sup$iso3, collapse = ";"), supplier_shares = paste(sprintf("%s %s", sup$iso3, pm_pct(sup$world_share, 0)), collapse = "; "),
      lane_suppliers = paste(eff_sup$iso3, collapse = ";"), buyers = paste(buy$iso3, collapse = ";"), lane_buyers = paste(eff_buy$iso3, collapse = ";"),
      n_suppliers = nrow(sup), n_buyers = nrow(buy), n_strong = n_strong, n_lanes = nrow(lanes),
      strong_members = paste(p$iso3[p$position_band == "strong"], collapse = ";"),
      member_share = round(sum(p$world_share, na.rm = TRUE), 3), trusted_member_share = round(sum(sup$world_share, na.rm = TRUE), 3),
      buyer_demand_share = round(sum(buy$demand_share, na.rm = TRUE), 3), buyer_net_import_share = round(sum(buy$net_import_share, na.rm = TRUE), 3),
      buyers_coercive_import_share = round(buy_cs, 3), buyers_from_member_suppliers = round(from_members, 3),
      chokepoint = choke, severe_chokepoint = severe, allied_short = short, coercive_top_iso3 = f$coercive_top_iso3,
      coercive_world_share = round(pm_num(f$coercive_world_share), 3), world_top_iso3 = f$largest_supplier, world_top_share = round(pm_num(f$largest_share), 3),
      aib_class = f$node_classification, residual_gap = round(pm_num(f$residual_gap), 3), node_demand_usd = pm_num(f$demand_usd),
      largest_trusted_supplier = f$optimized_largest_supplier, aib_hosts = paste(hosts, collapse = ";"),
      aib_third_hosts = paste(setdiff(hosts, sup$iso3), collapse = ";"), third_country_n = as.integer(n3),
      strategic_importance = round(if (state %in% PM_COMPACT_ACTIVE) max(c(0, buy$need_score)) else if (state == "member_competition") max(c(0, p$need_score)) else 0, 3),
      supply_fit = round(if (state %in% PM_COMPACT_ACTIVE && nrow(eff_sup)) min(max(eff_sup$competitive_position, na.rm = TRUE), max(eff_buy$need_score)) else 0, 3),
      member_need_max = round(max(c(0, p$need_score)), 3),
      buyer_protection = { v <- buy$policy_protection_priority; w <- ifelse(is.na(buy$demand_share) | buy$demand_share <= 0, 1e-6, buy$demand_share); ok <- !is.na(v); if (any(ok)) sum(v[ok] * w[ok]) / sum(w[ok]) else NA_real_ },
      supplier_policy = mean(utils::head(sup$policy_priority, 3), na.rm = TRUE), buyer_policy = mean(utils::head(buy$policy_priority, 3), na.rm = TRUE),
      deployment_need_a = dn_max, deployment_need_b = NA_real_, deployment_high = paste(dn_hi, collapse = ";"),
      prize_a = mx(p$prize), prize_b = NA_real_,
      mechanism_score = mech_score(f$source_compact),
      evidence_quality = round(mn(c(mx(sup$evidence_quality), mx(buy$evidence_quality))), 2),
      stringsAsFactors = FALSE)
  })
  d <- do.call(rbind, rows)
  for (k in c("buyer_protection", "prize_a", "supplier_policy", "buyer_policy", "evidence_quality")) d[[k]][!is.finite(d[[k]])] <- NA_real_
  d$evidence_ids <- ev_ids
  d$evidence_ids_node <- ev_node
  d$evidence_id_list <- vapply(ev_ids, function(z) paste(z, collapse = ";"), character(1))
  # constraint type and instruments through the bilateral analogue of each state
  tmp <- d; tmp$relationship_state <- d$rule_state
  tmp <- pm_constraint(tmp, cfg)
  tmp <- pm_instruments(tmp, inst, cfg)
  for (k in c("constraint_type", "constraint_secondary", "market_failure", "instruments", "main_instrument", "main_instrument_family", "heaviest_instrument_intensity")) d[[k]] <- tmp[[k]]
  pm_compact_fields(d, cfg, ev)
}

# The separate decision fields and the priority tier of a compact node.
pm_compact_fields <- function(d, cfg, ev) {
  th <- cfg$thresholds
  st <- d$relationship_state
  active <- st %in% PM_COMPACT_ACTIVE
  restr <- pm_restriction(d$evidence_ids, ev)
  d$active_restriction <- restr
  dn <- ifelse(!is.na(d$deployment_need_a) & d$deployment_need_a >= th$deployment_need_high, d$deployment_need_a, 0)
  urg <- pmax(ifelse(d$chokepoint, d$coercive_world_share, 0), ifelse(is.na(d$residual_gap), 0, d$residual_gap), ifelse(restr, 0.8, 0), dn, na.rm = TRUE)
  d$urgency <- round(ifelse(st == "not_assessed", NA, urg), 3)
  d$tractability <- round(rowMeans(cbind(d$supplier_policy, d$buyer_policy, d$mechanism_score), na.rm = TRUE), 3)
  # additionality: in a compact one committed project is partial coverage (0.5), an announced one 0.25
  commit <- vapply(d$evidence_ids_node, function(z) {
    s <- ev$status[ev$evidence_id %in% z & ev$item_type %in% c("project", "financing", "corporate")]
    if (any(s %in% c("under_construction", "operating", "signed", "in_force"))) 0.5 else if (any(s %in% c("announced", "proposed"))) 0.25 else 0
  }, numeric(1))
  mf <- ifelse(is.na(d$constraint_type), NA_real_, unname(PM_FAILURE_SCORE[d$constraint_type]))
  bw <- d$buyers_from_member_suppliers
  working <- ifelse(!st %in% c("member_supply", "capacity_constrained_trusted_supply") | is.na(bw), 0,
                    ifelse(bw >= th$bilateral_share_working, 1, ifelse(bw >= th$bilateral_share_partial, 0.5, 0)))
  d$market_failure_score <- mf; d$existing_commitment <- commit; d$market_already_working <- working
  d$additionality <- round(rowMeans(cbind(mf, 1 - commit, 1 - working)), 3)
  d$additionality_label <- ifelse(is.na(mf), "unknown", ifelse(mf == 0 | (working >= 1 & commit >= 0.5) | (working >= 1 & d$aib_class %in% "LOW INTERVENTION"), "low",
                           ifelse(mf >= 0.75 & commit < 0.5 & working < 0.5, "high", "medium")))
  sp <- cfg$speed; pen <- cfg$stage_speed_penalty
  d$execution_speed <- round(vapply(seq_len(nrow(d)), function(i) {
    f <- d$main_instrument_family[i]; if (is.na(f)) return(NA_real_)
    v <- if (is.null(sp[[f]])) 0.5 else as.numeric(sp[[f]])
    if (f %in% c("investment", "development") && !is.null(pen[[d$stage[i]]])) v <- min(v, as.numeric(pen[[d$stage[i]]]))
    v
  }, numeric(1)), 2)
  d$interdependency <- round(ifelse(!is.na(d$n_steps) & d$n_steps > 1, (d$n_steps - d$step_order) / (d$n_steps - 1), 0.5), 2)
  d$low_evidence <- is.na(d$evidence_quality) | d$evidence_quality < cfg$evidence$low_evidence_below
  d$treatment <- unname(PM_COMPACT_TREATMENT[st])
  d$state_label <- unname(PM_COMPACT_STATE_LABEL[st])
  tier <- ifelse(st == "not_assessed", 0L,
          ifelse(active & d$strategic_importance >= th$need_high & !d$low_evidence & d$additionality_label != "low", 1L,
          ifelse(active, 2L, ifelse(st == "member_competition", 3L, 4L))))
  d$priority_tier <- tier
  d$priority_label <- c("not assessed", "compact bet", "coordinate", "manage competition", "leave open")[tier + 1]
  lab <- function(x) match(x, c("high", "medium", "low", "unknown"))
  o <- order(d$priority_tier == 0, d$priority_tier, -d$urgency, lab(d$additionality_label), -d$strategic_importance,
             -d$tractability, -d$evidence_quality, d$step_order, d$node_id)
  d <- d[o, ]
  d$priority_rank <- seq_len(nrow(d))
  d
}

# Supplier -> buyer lanes at the active nodes: flow, the buyer's node imports and the supplier's share of them.
pm_compact_lanes <- function(na, prof, flows, max_per_node = 3) {
  act <- na[na$relationship_state %in% c("member_supply", "capacity_constrained_trusted_supply"), , drop = FALSE]
  if (!nrow(act)) return(data.frame())
  out <- do.call(rbind, lapply(seq_len(nrow(act)), function(i) {
    s <- strsplit(act$lane_suppliers[i], ";")[[1]]; b <- strsplit(act$lane_buyers[i], ";")[[1]]
    if (!length(s) || !length(b)) return(NULL)
    p <- prof[prof$node_id == act$node_id[i], , drop = FALSE]
    fl <- flows[flows$node_id == act$node_id[i], , drop = FALSE]
    g <- expand.grid(supplier = s, buyer = b, stringsAsFactors = FALSE)
    g <- g[g$supplier != g$buyer, , drop = FALSE]
    rk <- function(iso) unname(PM_POSITION_RANK[p$position_band[match(iso, p$iso3)]])
    g <- g[rk(g$supplier) > rk(g$buyer), , drop = FALSE]
    if (!nrow(g)) return(NULL)
    g$flow_usd <- vapply(seq_len(nrow(g)), function(k) sum(fl$value[fl$exporter_iso == g$supplier[k] & fl$importer_iso == g$buyer[k]]), numeric(1))
    g$buyer_imports_usd <- vapply(g$buyer, function(z) sum(fl$value[fl$importer_iso == z]), numeric(1))
    g$share_of_buyer_imports <- ifelse(g$buyer_imports_usd > 0, g$flow_usd / g$buyer_imports_usd, NA_real_)
    g$supplier_world_share <- p$world_share[match(g$supplier, p$iso3)]
    g$buyer_need <- p$need_band[match(g$buyer, p$iso3)]
    g$buyer_demand_share <- p$demand_share[match(g$buyer, p$iso3)]
    # the largest existing flows first; a lane with no trade yet is shown only where no lane has trade
    g <- g[order(-g$flow_usd, -g$buyer_demand_share, -g$supplier_world_share), , drop = FALSE]
    g <- utils::head(g[g$flow_usd > 0, , drop = FALSE], max_per_node)
    if (!nrow(g)) return(NULL)
    data.frame(node_id = act$node_id[i], node_label = act$node_label[i], step = act$step[i], relationship_state = act$relationship_state[i],
               priority_rank = act$priority_rank[i], g, stringsAsFactors = FALSE)
  }))
  if (is.null(out) || !nrow(out)) return(data.frame())
  out[order(out$priority_rank, -out$buyer_demand_share), , drop = FALSE]
}

# ---- Mechanisms and governance (Section 8) ----------------------------------------------------------------

# Active government mechanisms covering the compact with at least two member parties: plurilateral rows
# (plurilateral_mechanisms.csv) and the bilateral rows among members (bilateral_mechanisms.csv).
#   name_pattern: for a cross-cutting compact, a mechanism scoped only to a source compact counts when its name
#   matches this pattern (a battery-cell channel is not a critical-minerals channel)
#   upstream_scope: for an AIB compact with critical-mineral nodes, mechanisms scoped to the cross-cutting minerals
#   compact also count, flagged partial_scope (they cover the mineral steps only)
pm_compact_mechanisms <- function(mech, parties, id, src, name_pattern = NULL, upstream_scope = character()) {
  if (is.null(mech) || !nrow(mech)) return(mech)
  sc <- strsplit(mech$scope_compacts, "\\|")
  by_src <- vapply(sc, function(z) any(z %in% src), logical(1))
  if (!is.null(name_pattern)) by_src <- by_src & grepl(name_pattern, mech$name, ignore.case = TRUE)
  full <- vapply(sc, function(z) id %in% z, logical(1)) | by_src
  up <- if (length(upstream_scope)) vapply(sc, function(z) any(z %in% upstream_scope), logical(1)) else rep(FALSE, nrow(mech))
  mech$partial_scope <- !full & up
  covers <- full | up
  pz <- strsplit(mech$pair, "\\|")
  n_mem <- vapply(pz, function(z) sum(z %in% parties), integer(1))
  x <- mech[covers & n_mem >= 2, , drop = FALSE]
  x$n_parties <- vapply(strsplit(x$pair, "\\|"), length, integer(1))
  x$n_member_parties <- vapply(strsplit(x$pair, "\\|"), function(z) sum(z %in% parties), integer(1))
  # a bundle of separate bilateral MOUs (one government with many partners) is not a plurilateral body
  x$plurilateral <- x$n_parties >= 3 & !x$mechanism_type %in% "bilateral_bundle"
  x[order(!x$status %in% PM_ACTIVE_MECH, !x$plurilateral, -x$n_member_parties, x$mechanism_id), , drop = FALSE]
}

# Rule: existing = an active plurilateral mechanism covering the compact has a pipeline, finance, KPIs and a review
# cycle and includes at least `core_coverage_min` of the core members; upgrade = an active plurilateral mechanism
# covering the compact with finance, a pipeline or a framework mandate, including that share of the core; network =
# no such host, but at least two active bilateral mechanisms among members with finance or a pipeline cover it;
# new compact framework otherwise.
pm_compact_governance <- function(cm, core, cfg) {
  cov_min <- if (is.null(cfg$compact_memo$core_coverage_min)) 0.5 else cfg$compact_memo$core_coverage_min
  host_types <- if (is.null(cfg$compact_memo$host_types)) c("plurilateral", "framework", "finance_cooperation", "investment_vehicle") else unlist(cfg$compact_memo$host_types)
  act <- if (is.null(cm) || !nrow(cm)) cm else cm[cm$status %in% PM_ACTIVE_MECH, , drop = FALSE]
  if (is.null(act)) act <- data.frame()
  # a bloc party (the EU) covers its members
  expand <- function(z) unique(c(z, unlist(lapply(names(cfg$blocs), function(b) if (b %in% z) unlist(cfg$blocs[[b]]$members)))))
  cov <- function(pair) if (!length(core)) 0 else mean(core %in% expand(strsplit(pair, "\\|")[[1]]))
  pl <- if (nrow(act)) act[act$plurilateral, , drop = FALSE] else act
  if (nrow(pl)) pl$core_coverage <- vapply(pl$pair, cov, numeric(1))
  if (nrow(pl)) pl <- pl[pl$mechanism_type %in% host_types, , drop = FALSE]
  full <- if (nrow(pl)) pl[pl$has_pipeline %in% "TRUE" & pl$has_finance %in% "TRUE" & pl$has_kpis %in% "TRUE" & pl$has_review %in% "TRUE" & pl$core_coverage >= cov_min, , drop = FALSE] else pl
  hostable <- if (nrow(pl)) pl[pl$core_coverage >= cov_min, , drop = FALSE] else pl
  # the host that includes most of the core first, then whole-chain scope, then breadth, finance and pipeline
  if (nrow(hostable)) hostable <- hostable[order(-hostable$core_coverage, hostable$partial_scope %in% TRUE, -hostable$n_member_parties,
                                                 -(hostable$has_finance %in% "TRUE"), -(hostable$has_pipeline %in% "TRUE")), , drop = FALSE]
  bil <- if (nrow(act)) act[!act$plurilateral & act$mechanism_type %in% host_types & (act$has_finance %in% c("TRUE", "partial") | act$has_pipeline %in% c("TRUE", "partial")), , drop = FALSE] else act
  option <- if (nrow(full)) "existing" else if (nrow(hostable)) "upgrade" else if (nrow(bil) >= 2) "network" else "new_compact"
  host <- if (nrow(full)) full$mechanism_id[1] else if (nrow(hostable)) hostable$mechanism_id[1] else NA_character_
  hr <- if (!is.na(host)) act[act$mechanism_id == host, , drop = FALSE] else act[0, , drop = FALSE]
  gaps <- character()
  if (!nrow(pl)) gaps <- c(gaps, "no active plurilateral mechanism covers the compact")
  if (nrow(hr)) {
    if (!hr$has_pipeline[1] %in% "TRUE") gaps <- c(gaps, sprintf("%s has no published project pipeline", host))
    if (!hr$has_finance[1] %in% "TRUE") gaps <- c(gaps, sprintf("%s has no dedicated finance", host))
    if (!(hr$has_kpis[1] %in% "TRUE" && hr$has_review[1] %in% "TRUE")) gaps <- c(gaps, sprintf("%s has no KPI dashboard or review cycle", host))
    if (isTRUE(hr$partial_scope[1])) gaps <- c(gaps, sprintf("%s covers the critical-mineral steps only; the downstream steps need their own working group", host))
    miss <- setdiff(core, expand(strsplit(hr$pair[1], "\\|")[[1]]))
    if (length(miss)) gaps <- c(gaps, sprintf("core members outside %s: %s", host, paste(miss, collapse = ", ")))
  }
  feats <- if (!nrow(hr)) NA_character_ else {
    f <- c(if (hr$has_finance[1] %in% "TRUE") "finance" else if (hr$has_finance[1] %in% "partial") "partial finance", if (hr$has_pipeline[1] %in% "TRUE") "a project pipeline" else if (hr$has_pipeline[1] %in% "partial") "projects but no published list")
    if (length(f)) paste(f, collapse = " and ") else "a standing framework with no pipeline or finance yet"
  }
  # the closest candidate host when none qualifies: a plurilateral body of a host type that covers the compact,
  # whatever its status (announced, proposed) or core coverage
  cand <- if (!is.null(cm) && nrow(cm)) cm[cm$plurilateral & cm$mechanism_type %in% host_types & !cm$status %in% c("cancelled", "suspended"), , drop = FALSE] else data.frame()
  if (nrow(cand)) cand <- cand[order(-cand$n_member_parties), , drop = FALSE]
  list(option = option, host_mechanism = host, host_name = if (nrow(hr)) hr$name[1] else NA_character_, host_features = feats,
       host_partial = nrow(hr) > 0 && isTRUE(hr$partial_scope[1]),
       gaps = gaps, bilateral_channels = if (nrow(bil)) bil$mechanism_id else character(), mechanisms = cm,
       candidate = if (option %in% c("network", "new_compact") && nrow(cand)) sprintf("%s (%s, %s)", cand$name[1], cand$mechanism_id[1], cand$status[1]) else NA_character_)
}

# ---- Agendas ------------------------------------------------------------------------------------------------

pm_compact_trade <- function(na, cfg, names) {
  nm <- function(x) pm_join(pm_names_of(strsplit(x, ";")[[1]], names), 3)
  rows <- list()
  act <- na[na$relationship_state %in% PM_COMPACT_ACTIVE, , drop = FALSE]
  for (i in seq_len(nrow(act))) {
    r <- act[i, ]
    if (r$relationship_state %in% c("member_supply", "capacity_constrained_trusted_supply")) {
      type <- if (isTRUE(r$buyer_protection >= 0.67)) "tariff_risk" else if (r$relationship_state == "capacity_constrained_trusted_supply") "procurement_recognition" else "predictability"
      txt <- switch(type,
        tariff_risk = sprintf("Bind market access among members for %s from %s: the buyers' demand-weighted protection priority on this node is in their top third (%.2f), which taxes the trusted supply the compact needs.", pm_lc1(r$node_label), nm(r$lane_suppliers), r$buyer_protection),
        procurement_recognition = sprintf("Recognise member output of %s (%s) as trusted domestic-equivalent supply in the procurement and content rules of %s; capacity, not access, is the constraint.", pm_lc1(r$node_label), nm(r$lane_suppliers), nm(r$lane_buyers)),
        predictability = sprintf("Keep member exports of %s (%s) to %s tariff-free and predictable; member supply meets member need.", pm_lc1(r$node_label), nm(r$lane_suppliers), nm(r$lane_buyers)))
      rows[[length(rows) + 1]] <- data.frame(node_id = r$node_id, node_label = r$node_label, direction = sprintf("%s -> %s", gsub(";", ", ", r$lane_suppliers), gsub(";", ", ", r$lane_buyers)),
                                             action_type = type, action = txt, relationship_state = r$relationship_state, priority_rank = r$priority_rank, stringsAsFactors = FALSE)
    } else {
      anti <- isTRUE(r$chokepoint) && isTRUE(r$buyers_coercive_import_share >= 0.3)
      rows[[length(rows) + 1]] <- data.frame(node_id = r$node_id, node_label = r$node_label, direction = "all members", action_type = if (anti) "anti_circumvention" else "common_standards",
        action = if (anti) sprintf("Common origin and anti-circumvention rules for %s across %s: the buyers import %s from coercive suppliers (%s holds %s of world supply), so a measure one member takes alone leaks through another.",
                                   pm_lc1(r$node_label), nm(r$buyers), pm_pct(r$buyers_coercive_import_share), r$coercive_top_iso3, pm_pct(r$coercive_world_share))
                 else sprintf("Common specifications for %s so one non-coercive supplier base can serve every member market.", pm_lc1(r$node_label)),
        relationship_state = r$relationship_state, priority_rank = r$priority_rank, stringsAsFactors = FALSE)
    }
  }
  out <- if (length(rows)) do.call(rbind, rows) else data.frame()
  if (nrow(out)) out <- utils::head(out[order(out$priority_rank), ], cfg$priority$max_trade_actions)
  # never a chokepoint or a node the AIB cannot classify (2026-11-16, as in the pair memos)
  open <- na[which(na$relationship_state == "low_strategic_case" & grepl("adequate_trusted_supply", na$state_reason) & na$buyer_net_import_share > 0 &
               !na$node_id %in% names(cfg$trade_blind_spots) & !na$chokepoint %in% TRUE & !na$aib_class %in% "INSUFFICIENT DATA"), , drop = FALSE]
  open <- utils::head(open[order(-open$buyer_net_import_share), ], 4)
  if (nrow(open)) out <- rbind(out, data.frame(node_id = open$node_id, node_label = open$node_label, direction = "open", action_type = "open_sourcing",
    action = sprintf("Leave %s open to global sourcing: trusted supply is adequate and no coercive supplier is near the chokepoint line, so a compact preference adds cost without security.", pm_lc1(open$node_label)),
    relationship_state = open$relationship_state, priority_rank = open$priority_rank, stringsAsFactors = FALSE))
  comp <- na[na$relationship_state == "member_competition", , drop = FALSE]
  if (nrow(comp)) out <- rbind(out, data.frame(node_id = comp$node_id, node_label = comp$node_label, direction = "members", action_type = "no_preference",
    action = sprintf("No compact preference for %s: %s are strong suppliers and no other member needs more; keep the market contestable.", pm_lc1(comp$node_label), vapply(comp$strong_members, nm, character(1))),
    relationship_state = comp$relationship_state, priority_rank = comp$priority_rank, stringsAsFactors = FALSE))
  if (nrow(out)) { out$trade_rank <- seq_len(nrow(out)); out$rec_id <- sprintf("T%02d", out$trade_rank) }
  out
}

pm_compact_investment <- function(na, cfg, ev, inst, names, public = character()) {
  x <- na[na$relationship_state %in% PM_COMPACT_ACTIVE & !na$constraint_type %in% "no_material_intervention" &
            (na$strategic_importance >= 0.4 | na$aib_class %in% "BUILD"), , drop = FALSE]
  if (!nrow(x)) return(data.frame())
  nm <- function(s) pm_join(pm_names_of(strsplit(s, ";")[[1]], names), 3)
  resource <- x$stage %in% c("Extraction", "Recovery")
  inv_inst <- inst$instrument_id[inst$family %in% c("investment", "finance", "development")]
  hosts <- ifelse(nzchar(x$aib_hosts), vapply(x$aib_hosts, nm, character(1)), "no AIB-ranked host")
  owners <- ifelse(x$relationship_state == "shared_vulnerability", "pooled member capital", vapply(x$lane_suppliers, nm, character(1)))
  buyers <- vapply(ifelse(nzchar(x$lane_buyers), x$lane_buyers, x$buyers), nm, character(1))
  logic <- ifelse(x$relationship_state == "shared_vulnerability", sprintf("pooled finance and offtake from %s for non-coercive capacity", buyers),
           ifelse(resource, sprintf("offtake-linked equity: buyers (%s) take stakes in member or partner resources", buyers),
                  sprintf("capability owners build near member demand (%s) or in AIB-ranked hosts", buyers)))
  pub <- vapply(strsplit(x$instruments, ";"), function(z) { z <- z[z %in% inv_inst]; if (length(z)) z[1] else NA_character_ }, character(1))
  out <- data.frame(rec_id = NA_character_, node_id = x$node_id, node_label = x$node_label, step = x$step, stage = x$stage,
    capability_owner = owners, destination = hosts, investment_logic = logic,
    demand_case = sprintf("member buyers take %s of allied demand (%s of allied net imports); allied demand %s; AIB class %s", pm_pct(x$buyer_demand_share), pm_pct(x$buyer_net_import_share), pm_usd(x$node_demand_usd), x$aib_class),
    private_actors = pm_private_actors(pm_ids_of_parties(x$evidence_ids_node, ev, pm_lane_parties(x)), ev, public), public_instrument = ifelse(is.na(pub), x$main_instrument, pub),
    additionality = x$additionality, additionality_label = x$additionality_label,
    additionality_rationale = sprintf("market failure %.2f; existing commitment %.2f; market already working %.1f", x$market_failure_score, x$existing_commitment, x$market_already_working),
    key_obstacle = unname(PM_OBSTACLE[x$constraint_type]), time_horizon = pm_horizon(pmin(x$execution_speed, 0.5, na.rm = TRUE)),
    priority_rank = x$priority_rank, evidence_ids = x$evidence_id_list, stringsAsFactors = FALSE)
  out <- out[order(out$priority_rank), , drop = FALSE]
  out$rec_id <- sprintf("I%02d", seq_len(nrow(out)))
  out
}

# Third-country steps: recipient x technology x stage in the compact's cells where at least two members are donors
# (scripts/93 development or commercial route).
pm_compact_third <- function(devpart, frame_c, members, cfg, names) {
  if (is.null(devpart) || !nrow(devpart)) return(data.frame())
  keys <- unique(trimws(unlist(strsplit(paste(frame_c$score_cells, collapse = ";"), ";")))); keys <- keys[nzchar(keys) & keys != "NA"]
  x <- devpart[devpart$donor_iso3 %in% members & devpart$route %in% c("development", "commercial") &
                 paste(devpart$technology, devpart$stage, sep = "|") %in% keys & !devpart$recipient_iso3 %in% members, , drop = FALSE]
  if (!nrow(x)) return(data.frame())
  own <- function(t) !is.na(t) & pm_num(t) <= 4
  evr <- function(q) match(q, c("strong", "moderate", "weak"))
  g <- split(x, paste(x$recipient_iso3, x$technology, x$stage, sep = "|"))
  out <- do.call(rbind, lapply(g, function(z) {
    dn <- unique(z$donor_iso3)
    if (length(dn) < 2) return(NULL)
    tf <- sort(table(z$recommended_tool_family), decreasing = TRUE)
    data.frame(recipient_iso3 = z$recipient_iso3[1], recipient = z$recipient[1], technology = z$technology[1], stage = z$stage[1],
               donors = paste(dn, collapse = ";"), n_donors = length(dn), n_own_need = length(unique(z$donor_iso3[own(z$donor_tier)])),
               recipient_choice = z$recipient_choice[1], recipient_anchor = z$catch_up_anchor_type[1], recipient_income_group = z$recipient_income_group[1],
               tools = if (length(tf)) names(tf)[1] else NA_character_, evidence_quality = z$evidence_quality[order(evr(z$evidence_quality))][1],
               rank_sum = sum(pm_num(z$rank_donor), na.rm = TRUE) / nrow(z), stringsAsFactors = FALSE)
  }))
  if (is.null(out) || !nrow(out)) return(data.frame())
  out <- out[order(-out$n_own_need, -out$n_donors, evr(out$evidence_quality), out$rank_sum, out$recipient_iso3), , drop = FALSE]
  out <- utils::head(out, cfg$priority$max_third_country)
  out$donor_names <- vapply(out$donors, function(s) pm_join(pm_names_of(strsplit(s, ";")[[1]], names), 4), character(1))
  out$rec_id <- sprintf("X%02d", seq_len(nrow(out)))
  out
}

#   fta_pairs: "A|B" keys (sorted) of members with a trade agreement in force; a market-access deal between them holds
#   the agreement's terms instead of binding new access
pm_compact_deals <- function(na, inst, institutions, ev, cfg, names, public = character(), fta_pairs = character()) {
  x <- na[which(na$priority_tier %in% c(1L, 2L) & !is.na(na$main_instrument) & !na$constraint_type %in% "no_material_intervention" &
                  (na$strategic_importance >= 0.4 | na$aib_class %in% "BUILD")), , drop = FALSE]
  if (!nrow(x)) return(data.frame())
  nm <- function(s, n = 2) pm_join(pm_names_of(strsplit(s, ";")[[1]], names), n)
  # institutions in scope for the node's compact (institutions.csv scope_compacts; a nuclear regulator never signs a
  # battery standards deal)
  cm_of <- function(i) unique(stats::na.omit(c(if (!is.null(x$source_compact)) x$source_compact[i], if (!is.null(x$strategic_compact_id)) x$strategic_compact_id[i])))
  inst_by <- function(iso, fam, cm) {
    ok <- if (length(cm)) pm_inst_scope_ok(institutions, cm) else rep(TRUE, nrow(institutions))
    r <- institutions[institutions$iso3 == iso & vapply(strsplit(institutions$instrument_families, "\\|"), function(z) fam %in% z, logical(1)) & ok, , drop = FALSE]
    if (nrow(r)) paste(utils::head(r$abbreviation, 3), collapse = ", ") else "no institution listed"
  }
  lead_s <- vapply(strsplit(x$lane_suppliers, ";"), function(z) if (length(z)) z[1] else NA_character_, character(1))
  lead_b <- vapply(strsplit(ifelse(nzchar(x$lane_buyers), x$lane_buyers, x$buyers), ";"), function(z) if (length(z)) z[1] else NA_character_, character(1))
  resp <- vapply(seq_len(nrow(x)), function(i) {
    f <- x$main_instrument_family[i]
    isos <- unique(stats::na.omit(c(lead_s[i], lead_b[i])))
    paste(vapply(isos, function(z) sprintf("%s: %s", z, inst_by(z, f, cm_of(i))), character(1)), collapse = "; ")
  }, character(1))
  tp <- inst$deal_template[match(x$main_instrument, inst$instrument_id)]
  m12 <- inst$milestone_12m[match(x$main_instrument, inst$instrument_id)]; o36 <- inst$outcome_36m[match(x$main_instrument, inst$instrument_id)]
  fta <- x$main_instrument %in% "market_access" & vapply(seq_len(nrow(x)), function(i) paste(sort(c(lead_s[i], lead_b[i])), collapse = "|") %in% fta_pairs, logical(1))
  tp[fta] <- "Hold {buyer}'s trade-agreement terms for {supplier} {node} (no new tariff, safeguard or content rule)"
  m12[fta] <- "Written confirmation that the node stays at the trade agreement's terms, with no new safeguard or content rule"
  o36[fta] <- "The trusted supplier's exports of the node face no new barrier under the agreement"
  sup_nm <- ifelse(nzchar(x$lane_suppliers), vapply(x$lane_suppliers, nm, character(1)), "non-coercive")
  buy_nm <- vapply(ifelse(nzchar(x$lane_buyers), x$lane_buyers, x$buyers), nm, character(1), n = 3)
  # candidate hosts for pooled capacity only (a deal with a member supplier is built by that supplier)
  hosts <- ifelse(nzchar(x$aib_third_hosts) & !nzchar(x$lane_suppliers), sprintf(" (candidate hosts: %s)", vapply(x$aib_third_hosts, nm, character(1), n = 3)), "")
  txt <- vapply(seq_len(nrow(x)), function(i) {
    # several buyer economies read "buyers in A, B and 3 others", not "A, B and 3 others buyers"
    s <- gsub("{buyer} buyers", "buyers in {buyer}", tp[i], fixed = TRUE)
    s <- gsub("{supplier}", sup_nm[i], s, fixed = TRUE); s <- gsub("{buyer}", buy_nm[i], s, fixed = TRUE)
    s <- gsub("{node}", pm_lc1(x$node_label[i]), s, fixed = TRUE); gsub("{hosts}", hosts[i], s, fixed = TRUE) }, character(1))
  out <- data.frame(node_id = x$node_id, unit_id = x$node_id, deal = txt, step = x$step, stage = x$stage, problem_solved = x$market_failure,
    constraint_type = x$constraint_type, instrument = x$main_instrument, responsible_institutions = resp,
    private_actors = pm_private_actors(pm_ids_of_parties(x$evidence_ids_node, ev, pm_lane_parties(x)), ev, c(public, institutions$abbreviation, institutions$institution)),
    scale = ifelse(is.na(x$node_demand_usd), NA_character_, sprintf("allied demand %s", pm_usd(x$node_demand_usd))),
    milestone_12m = m12, outcome_36m = o36,
    confidence = pm_band(x$evidence_quality), importance = pm_band(x$strategic_importance), urgency = pm_band(x$urgency),
    tractability = pm_band(x$tractability, 0.6, 0.4), execution_speed = x$execution_speed, additionality = x$additionality_label,
    interdependency = x$interdependency, evidence_quality = x$evidence_quality, priority_tier = x$priority_tier, evidence_ids = x$evidence_id_list, stringsAsFactors = FALSE)
  lv <- function(v) match(v, c("high", "medium", "low", "unknown"))
  out <- out[order(lv(out$importance), lv(out$additionality), lv(out$urgency), lv(out$tractability), -out$execution_speed, -out$interdependency, -out$evidence_quality, out$node_id), ]
  # the headline bets (the first max_joint_bets of tier 1 in Section 3 order) each get a deal before the cap
  t1 <- na[na$priority_tier == 1, , drop = FALSE]
  head_ids <- utils::head(t1$node_id[order(t1$priority_rank)], cfg$priority$max_joint_bets)
  out <- out[order(!out$node_id %in% head_ids, out$priority_tier != 1), , drop = FALSE]
  keep <- out$priority_tier == 1 | (stats::ave(seq_len(nrow(out)), out$step, FUN = seq_along) <= 2 & !duplicated(paste(out$step, out$instrument)))
  out <- utils::head(out[keep, ], cfg$priority$max_deals)
  out$rank <- seq_len(nrow(out)); out$rec_id <- sprintf("D%02d", out$rank)
  out
}

pm_compact_non_priorities <- function(na, gov, cfg, names, ev) {
  nm <- function(s, n = 4) pm_join(pm_names_of(strsplit(s, ";")[[1]], names), n)
  rows <- list()
  add <- function(node_id, kind, text, weight) rows[[length(rows) + 1]] <<- data.frame(node_id = node_id, kind = kind, text = text, weight = weight, stringsAsFactors = FALSE)
  comp <- na[na$relationship_state == "member_competition", , drop = FALSE]
  for (i in seq_len(nrow(comp))) add(comp$node_id[i], "duplication",
    sprintf("Do not co-subsidise new %s capacity among members: %s are already strong suppliers. %s", pm_lc1(comp$node_label[i]), nm(comp$strong_members[i]),
            if (isTRUE(comp$chokepoint[i] | comp$allied_short[i])) "More trusted capacity is needed, so agree who specialises in what rather than racing." else "Trusted supply is adequate: this is commercial competition."),
    3 + comp$member_need_max[i])
  crowd <- na[na$relationship_state %in% PM_COMPACT_ACTIVE & na$n_strong >= 3, , drop = FALSE]
  for (i in seq_len(nrow(crowd))) add(crowd$node_id[i], "crowding",
    sprintf("Do not run parallel national subsidy races for %s: %d members (%s) are already strong suppliers; agree in the compact who expands where.", pm_lc1(crowd$node_label[i]), crowd$n_strong[i], nm(crowd$strong_members[i])), 2.9)
  done <- na[na$relationship_state %in% PM_COMPACT_ACTIVE & na$existing_commitment >= 0.5 & na$additionality_label == "low", , drop = FALSE]
  # cite only committed items that name the lane's own suppliers or buyers (a member's project elsewhere serves no one here)
  committed <- function(ids, parties) { e <- ev[ev$evidence_id %in% ids & ev$item_type %in% c("project", "financing", "corporate") & ev$status %in% c("under_construction", "operating", "signed", "in_force"), , drop = FALSE]
    e <- e[vapply(strsplit(e$countries, "\\|"), function(z) !length(parties) || any(z %in% parties), logical(1)), , drop = FALSE]; paste(e$evidence_id, collapse = ", ") }
  dpart <- pm_lane_parties(done)
  for (i in seq_len(nrow(done))) { cid <- committed(done$evidence_ids[[i]], dpart[[i]]); if (!nzchar(cid)) next
    add(done$node_id[i], "already_committed",
    sprintf("Do not put new public money behind %s projects that are already committed (evidence %s): member suppliers already serve member buyers, so compact support is additional only beyond them.", pm_lc1(done$node_label[i]), cid), 2.5) }
  low <- na[na$relationship_state == "low_strategic_case", , drop = FALSE]
  nw <- low[grepl("supplier_without_member_need", low$state_reason), , drop = FALSE]
  nw <- utils::head(nw[order(-nw$member_share), ], 3)
  for (i in seq_len(nrow(nw))) add(nw$node_id[i], "no_member_need",
    sprintf("Do not build a compact programme around %s: members supply it (%s) but none has a material need for more; trade it commercially.", pm_lc1(nw$node_label[i]), nm(nw$suppliers[i], 3)), 1.5)
  adq <- low[grepl("adequate_trusted_supply", low$state_reason) & low$aib_class %in% "LOW INTERVENTION" & !low$node_id %in% names(cfg$trade_blind_spots), , drop = FALSE]
  adq <- utils::head(adq[order(-adq$node_demand_usd), ], 3)
  for (i in seq_len(nrow(adq))) add(adq$node_id[i], "do_not_defend",
    sprintf("Do not treat %s as a compact priority: the AIB finds trusted supply adequate (LOW INTERVENTION) and no member supplier-buyer gap passes the rules.", pm_lc1(adq$node_label[i])), 2)
  if (gov$option %in% c("existing", "upgrade")) add(NA_character_, "institution",
    sprintf("Do not create a new compact secretariat or treaty body: %s already exists (%s); dock the compact in it.", gov$host_name, gov$host_features), 2.8)
  if (gov$option == "network") add(NA_character_, "institution",
    "Do not create a new treaty body: the members' bilateral frameworks already carry finance and projects; link them through a light compact secretariat.", 2.8)
  if (!length(rows)) return(data.frame(node_id = character(), kind = character(), text = character(), weight = numeric(), rec_id = character()))
  out <- do.call(rbind, rows)
  out <- out[order(-out$weight, out$node_id), , drop = FALSE]
  out <- out[!duplicated(out$text), , drop = FALSE]
  out <- out[stats::ave(seq_len(nrow(out)), out$kind, FUN = seq_along) <= 3, , drop = FALSE]
  out <- utils::head(out, max(cfg$priority$min_non_priorities, 7))
  out$rec_id <- sprintf("N%02d", seq_len(nrow(out)))
  out
}

# ---- Evidence register ----------------------------------------------------------------------------------

pm_compact_evidence_register <- function(recs, na, ev, id) {
  rows <- list()
  for (i in seq_len(nrow(recs))) {
    r <- recs[i, ]
    u <- if (!is.na(r$unit_id)) na[na$node_id == r$unit_id, , drop = FALSE] else na[0, ]
    if (nrow(u)) {
      u <- u[1, ]
      met <- list(c("trusted_member_share", "World supply held by capable member suppliers", sprintf("%s (%s)", pm_pct(u$trusted_member_share), u$supplier_shares), "aib_compact_division_of_labour (scripts/33)", "exact"),
                  c("buyer_net_import_share", "Allied net imports of the member buyers", sprintf("%s (%s)", pm_pct(u$buyer_net_import_share), gsub(";", ", ", u$buyers)), "aib_compact_division_of_labour (scripts/33)", "exact"),
                  c("strategic_importance", "Highest member buyer need (inherited from the cell)", format(u$strategic_importance), "strategic_position_tbl (scripts/26)", "proxy"),
                  c("aib_class", "AIB node class under the China shock", u$aib_class, "aib_node_classification (scripts/32)", "exact"))
      for (m in met) rows[[length(rows) + 1]] <- data.frame(claim_id = r$rec_id, rec_type = r$rec_type, compact = id, technology = u$tech, stage = u$stage,
        unit = u$node_id, source_metric = m[2], osi_variable = m[1], value = m[3], year = "2024", source = m[4], evidence_specificity = m[5],
        inference = u$state_reason, recommendation = r$text, confidence = pm_band(u$evidence_quality), basis = "osi", external_ref = "", stringsAsFactors = FALSE)
    }
    ids <- unlist(strsplit(ifelse(is.na(r$evidence_ids), "", r$evidence_ids), ";"))
    for (e in ids[nzchar(ids)]) {
      x <- ev[ev$evidence_id == e, , drop = FALSE]; if (!nrow(x)) next
      rows[[length(rows) + 1]] <- data.frame(claim_id = r$rec_id, rec_type = r$rec_type, compact = id, technology = x$technology, stage = x$stage,
        unit = if (nrow(u)) u$node_id else "", source_metric = x$name, osi_variable = "", value = x$claim, year = substr(x$publication_date, 1, 4),
        source = x$source, evidence_specificity = x$verification, inference = "", recommendation = r$text, confidence = x$verification,
        basis = "external", external_ref = sprintf("%s %s (accessed %s)", x$evidence_id, x$url, x$access_date), stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}
