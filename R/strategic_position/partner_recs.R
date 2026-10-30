# Partner recommendations and the allied division of labour (docs/strategic_position.md, "Allied
# strategy"). A node whose canonical choice is Compete . Partner (an important capability better met
# through allies), and a Secure node whose mitigation relies on trusted foreign capacity (delivery
# allied, mixed or diversified_global), is met with trusted suppliers, not at home. For each such
# country x technology x six-stage cell this file names up to three trusted partners and why, flags a
# partner gap where no credible trusted supplier exists, and gives the AIB status (covered | gap) with
# its reasons (no trusted supplier, a single credible supplier, weak capability evidence, insufficient
# allied capacity, excessive concentration). A genuine "no credible allied solution yet" is reported,
# not forced. The same capability measure gives the coalition-level division of labour the Explorer's
# AIB view draws.
#
# Capability of a supplier in a cell: the Allied Industrial Base position share (aib_strategic.rds
# evidence) averaged over the AIB nodes mapped to the cell, weighted by world trade x OSI coverage,
# where the AIB covers the cell; elsewhere its share of world supply on the cell's primary measure
# (sp_supply_structure()). capability_source says which.
# Trusted: the partner is trusted by the reporter (sp_coercive_pairs() / sp_nonaligned_trust(), the
# same rule as the engine's trusted_world_share). Credible: capability at least presence_min.
# Ranking: capability (relative to the best candidate) x relationship (Friendshore Index) x
# diversification (the fall in the reporter's import HHI if a step of its imports moved from its
# coercive suppliers, or else its largest supplier, to the candidate). No new score is reported: the
# product only orders the candidates.
#
# Pure: no IO. Needs sp_helpers.R and supply_chain_stages.R.

# AIB capability per cell. nodes: node_id, score_cells ("Tech|Stage; Tech|Stage"), osi_coverage.
# evidence: node_id, iso3, position_share, world_trade. quality: node_id, data_quality, stage_mixed.
# loo (optional): node_id, iso3, irreplaceable. techs: the OSI technologies kept.
# Returns list(capability: tech, supply_chain, iso3, aib_position_share; cells: tech, supply_chain,
# aib_n_nodes, aib_high_quality_share, aib_stage_mixed, aib_irreplaceable (ISO3s, ";")).
sp_aib_cell_capability <- function(nodes, evidence, quality, loo = NULL, techs = NULL) {
  map <- dplyr::bind_rows(lapply(seq_len(nrow(nodes)), function(i) {
    sc <- trimws(unlist(strsplit(dplyr::coalesce(as.character(nodes$score_cells[i]), ""), ";", fixed = TRUE)))
    sc <- sc[nzchar(sc)]
    if (!length(sc)) return(NULL)
    parts <- do.call(rbind, strsplit(sc, "|", fixed = TRUE))
    data.frame(node_id = nodes$node_id[i], tech = parts[, 1], supply_chain = parts[, 2],
               osi_coverage = as.numeric(nodes$osi_coverage[i]), stringsAsFactors = FALSE)
  }))
  empty <- list(capability = data.frame(tech = character(), supply_chain = character(), iso3 = character(), aib_position_share = numeric()),
                cells = data.frame(tech = character(), supply_chain = character(), aib_n_nodes = integer()))
  if (!nrow(map)) return(empty)
  if (!is.null(techs)) map <- map[map$tech %in% techs, , drop = FALSE]
  map <- map[is_supply_chain_stage(map$supply_chain), , drop = FALSE]
  wt <- evidence %>% dplyr::distinct(.data$node_id, .data$world_trade)
  wt <- wt[!duplicated(wt$node_id), , drop = FALSE]
  map <- map %>%
    dplyr::left_join(wt, by = "node_id") %>%
    dplyr::mutate(w = dplyr::coalesce(.data$world_trade, 0) * dplyr::coalesce(.data$osi_coverage, 1)) %>%
    dplyr::filter(.data$w > 0)
  if (!nrow(map)) return(empty)
  cap <- map %>%
    dplyr::inner_join(evidence %>% dplyr::select("node_id", "iso3", "position_share"), by = "node_id", relationship = "many-to-many") %>%
    dplyr::group_by(.data$tech, .data$supply_chain, .data$iso3) %>%
    dplyr::summarise(.num = sum(.data$w * dplyr::coalesce(.data$position_share, 0)), .groups = "drop") %>%
    dplyr::left_join(map %>% dplyr::group_by(.data$tech, .data$supply_chain) %>% dplyr::summarise(.den = sum(.data$w), .groups = "drop"),
                     by = c("tech", "supply_chain")) %>%
    dplyr::transmute(.data$tech, .data$supply_chain, .data$iso3, aib_position_share = .data$.num / .data$.den)
  q <- quality %>% dplyr::select("node_id", "data_quality", dplyr::any_of("stage_mixed"))
  if (!"stage_mixed" %in% names(q)) q$stage_mixed <- FALSE
  irr <- if (!is.null(loo) && nrow(loo)) {
    loo %>% dplyr::filter(.data$irreplaceable %in% TRUE) %>% dplyr::distinct(.data$node_id, .data$iso3)
  } else data.frame(node_id = character(), iso3 = character())
  cells <- map %>%
    dplyr::left_join(q, by = "node_id") %>%
    dplyr::group_by(.data$tech, .data$supply_chain) %>%
    dplyr::summarise(
      aib_n_nodes = dplyr::n_distinct(.data$node_id),
      aib_high_quality_share = sum(.data$w[.data$data_quality %in% "High confidence"]) / sum(.data$w),
      aib_stage_mixed = any(.data$stage_mixed %in% TRUE),
      aib_irreplaceable = {
        ids <- sort(unique(irr$iso3[irr$node_id %in% .data$node_id]))
        if (length(ids)) paste(ids, collapse = ";") else NA_character_
      },
      .groups = "drop"
    )
  list(capability = cap, cells = cells)
}

# Capability of every supplier in every cell: the AIB position share where the AIB covers the cell,
# else the world supply share. supply: iso3, tech, supply_chain, home_share (sp_supply_structure()$cells).
sp_cell_capability <- function(supply, aib = NULL) {
  w <- supply %>%
    dplyr::filter(!is.na(.data$home_share)) %>%
    dplyr::transmute(.data$tech, .data$supply_chain, .data$iso3, world_share = .data$home_share)
  if (is.null(aib) || !nrow(aib$capability)) {
    return(dplyr::mutate(w, capability = .data$world_share, capability_source = "world_share"))
  }
  covered <- unique(aib$capability[, c("tech", "supply_chain")])
  covered$.aib <- TRUE
  w %>%
    dplyr::full_join(aib$capability, by = c("tech", "supply_chain", "iso3")) %>%
    dplyr::left_join(covered, by = c("tech", "supply_chain")) %>%
    dplyr::mutate(
      .aib = dplyr::coalesce(.data$.aib, FALSE),
      capability = ifelse(.data$.aib, dplyr::coalesce(.data$aib_position_share, 0), .data$world_share),
      capability_source = ifelse(.data$.aib, "aib_position_share", "world_share")
    ) %>%
    dplyr::filter(!is.na(.data$capability)) %>%
    dplyr::select("tech", "supply_chain", "iso3", "world_share", "capability", "capability_source")
}

# The fall in the import HHI when `step` of the reporter's imports moves to `cand`, taken
# proportionally from its coercive suppliers (or, with none, from its largest supplier).
sp_hhi_drop <- function(shares, coercive, cand, step = 0.1) {
  if (!length(shares) || sum(shares) <= 0) return(NA_real_)
  s <- shares / sum(shares)
  h0 <- sum(s^2)
  src <- if (any(coercive & s > 0)) coercive & s > 0 else seq_along(s) == which.max(s)
  if (cand %in% names(s)) src[names(s) == cand] <- FALSE
  pool <- sum(s[src])
  if (pool <= 0) return(0)
  d <- min(step, pool)
  s[src] <- s[src] - d * s[src] / pool
  if (cand %in% names(s)) s[cand] <- s[cand] + d else s <- c(s, stats::setNames(d, cand))
  h0 - sum(s^2)
}

# Partner recommendations.
# cells: iso3, tech, supply_chain of the Partner nodes. capability: sp_cell_capability().
# pairs: sp_coercive_pairs() (reporter_iso, partner_iso, coercive, trusted). compat: reporter_iso,
# partner_iso, tech, supply_chain, friendshore. cp: iso3, tech, supply_chain,
# competitive_position_percentile. imports (optional): reporter_iso, partner_iso, tech, supply_chain,
# imports_usd. aib_cells (optional): sp_aib_cell_capability()$cells. members: coalition ISO3 codes.
# cfg: strategic_posture.partner_recs.
#
# cells may also carry coercive_top_share (the largest coercive supplier's share of world supply) for the
# excessive-concentration reason.
#
# Returns one row per cell with recommended_partner_1..3 (ISO3) and, for each, _capability, _cp
# (Competitive Position percentile), _relationship (Friendshore) and _diversification (HHI drop);
# partner_n_credible, partner_gap, partner_capability_source, partner_evidence_quality, partner_reason,
# partner_trusted_capacity (credible trusted partners' combined share of world supply),
# partner_gap_reasons (";"-list of PARTNER_GAP_REASONS) and partner_aib_status (covered | gap).
sp_partner_recs <- function(cells, capability, pairs, compat, cp, imports = NULL, aib_cells = NULL, members = character(), cfg = list()) {
  presence <- cfg$presence_min %||% 0.02
  step <- cfg$diversification_step %||% 0.1
  n_recs <- as.integer(cfg$n_recommendations %||% 3L)
  rel_default <- cfg$relationship_missing %||% 0.5
  if (!nrow(cells)) return(data.frame())
  trusted <- pairs %>% dplyr::filter(.data$trusted %in% TRUE, !.data$coercive %in% TRUE) %>%
    dplyr::select(iso3 = "reporter_iso", partner = "partner_iso")
  coer <- pairs %>% dplyr::filter(.data$coercive %in% TRUE) %>% dplyr::select(iso3 = "reporter_iso", partner = "partner_iso")
  cand <- cells %>%
    dplyr::select("iso3", "tech", "supply_chain") %>%
    dplyr::inner_join(trusted, by = "iso3", relationship = "many-to-many") %>%
    dplyr::filter(.data$partner != .data$iso3) %>%
    dplyr::inner_join(capability %>% dplyr::rename(partner = "iso3"), by = c("tech", "supply_chain", "partner")) %>%
    dplyr::filter(.data$capability > 0) %>%
    dplyr::left_join(compat %>% dplyr::rename(iso3 = "reporter_iso", partner = "partner_iso"), by = c("iso3", "partner", "tech", "supply_chain")) %>%
    dplyr::left_join(cp %>% dplyr::rename(partner = "iso3", partner_cp = "competitive_position_percentile"), by = c("partner", "tech", "supply_chain")) %>%
    dplyr::mutate(credible = .data$capability >= presence, relationship = dplyr::coalesce(.data$friendshore, rel_default),
                  relationship_measured = !is.na(.data$friendshore))
  # diversification
  if (!is.null(imports) && nrow(imports)) {
    imp <- imports %>% dplyr::filter(.data$imports_usd > 0) %>%
      dplyr::semi_join(cells, by = c("reporter_iso" = "iso3", "tech", "supply_chain"))
    coer_key <- paste(coer$iso3, coer$partner)
    keyed <- split(imp, paste(imp$reporter_iso, imp$tech, imp$supply_chain, sep = "|"))
    cand$diversification <- vapply(seq_len(nrow(cand)), function(i) {
      k <- paste(cand$iso3[i], cand$tech[i], cand$supply_chain[i], sep = "|")
      d <- keyed[[k]]
      if (is.null(d)) return(NA_real_)
      s <- stats::setNames(d$imports_usd, d$partner_iso)
      sp_hhi_drop(s, paste(cand$iso3[i], names(s)) %in% coer_key, cand$partner[i], step)
    }, numeric(1))
  } else {
    cand$diversification <- NA_real_
  }
  ranked <- cand %>%
    dplyr::filter(.data$credible) %>%
    dplyr::group_by(.data$iso3, .data$tech, .data$supply_chain) %>%
    dplyr::mutate(
      .cap = .data$capability / max(c(.data$capability, 0)),
      .div = {
        m <- suppressWarnings(max(abs(.data$diversification), na.rm = TRUE))
        ifelse(is.na(.data$diversification) | !is.finite(m) | m == 0, 0.5, 0.5 + 0.5 * .data$diversification / m)
      },
      .order = .data$.cap * .data$relationship * .data$.div
    ) %>%
    dplyr::arrange(dplyr::desc(.data$.order), dplyr::desc(.data$capability), .data$partner, .by_group = TRUE) %>%
    dplyr::mutate(.k = dplyr::row_number(), n_credible = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$.k <= n_recs)
  pct <- function(v) ifelse(is.na(v), "?", paste0(round(100 * v), "%"))
  out <- cells %>% dplyr::select("iso3", "tech", "supply_chain")
  for (k in seq_len(n_recs)) {
    rk <- ranked %>% dplyr::filter(.data$.k == k) %>%
      dplyr::transmute(.data$iso3, .data$tech, .data$supply_chain, p = .data$partner, cap = .data$capability, cpp = .data$partner_cp,
                       rel = ifelse(.data$relationship_measured, .data$relationship, NA_real_), div = .data$diversification)
    names(rk)[4:8] <- paste0("recommended_partner_", k, c("", "_capability", "_cp", "_relationship", "_diversification"))
    out <- dplyr::left_join(out, rk, by = c("iso3", "tech", "supply_chain"))
  }
  nc <- cand %>% dplyr::group_by(.data$iso3, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(partner_n_credible = sum(.data$credible), partner_n_trusted_present = dplyr::n(),
                     partner_capability_source = dplyr::first(.data$capability_source), .groups = "drop")
  out <- out %>% dplyr::left_join(nc, by = c("iso3", "tech", "supply_chain")) %>%
    dplyr::mutate(partner_n_credible = dplyr::coalesce(.data$partner_n_credible, 0L),
                  partner_gap = .data$partner_n_credible == 0)
  q <- if (!is.null(aib_cells) && nrow(aib_cells)) aib_cells[, c("tech", "supply_chain", "aib_high_quality_share", "aib_stage_mixed")] else NULL
  out <- if (is.null(q)) dplyr::mutate(out, aib_high_quality_share = NA_real_, aib_stage_mixed = NA) else dplyr::left_join(out, q, by = c("tech", "supply_chain"))
  out$partner_evidence_quality <- dplyr::case_when(
    is.na(out$partner_capability_source) ~ "none",
    out$partner_capability_source == "world_share" ~ "low",
    out$aib_stage_mixed %in% TRUE | dplyr::coalesce(out$aib_high_quality_share, 0) < 0.5 ~ "moderate",
    TRUE ~ "high"
  )
  src_word <- ifelse(out$partner_capability_source %in% "aib_position_share", "of allied supply (AIB)", "of world supply")
  name1 <- out$recommended_partner_1
  out$partner_reason <- dplyr::case_when(
    out$partner_gap ~ paste0("No trusted supplier holds ", round(100 * presence), "% or more ", ifelse(is.na(src_word), "of supply", src_word),
                             ": a partner gap. The need has to be met by diversifying, stockpiling or building with partners from a low base."),
    TRUE ~ paste0(
      name1, " holds ", pct(out$recommended_partner_1_capability), " ", src_word,
      ifelse(is.na(out$recommended_partner_1_relationship), "", paste0(", relationship ", format(round(out$recommended_partner_1_relationship, 2), nsmall = 2))),
      ifelse(!is.na(out$recommended_partner_1_diversification) & out$recommended_partner_1_diversification > 0,
             paste0(", and moving 10% of imports to it would cut import concentration (HHI) by ", format(round(out$recommended_partner_1_diversification, 3), nsmall = 3)), ""),
      ".", ifelse(!is.na(out$recommended_partner_2), paste0(" Backups: ", ifelse(is.na(out$recommended_partner_3), out$recommended_partner_2,
                                                                         paste(out$recommended_partner_2, "and", out$recommended_partner_3)), "."), ""),
      ifelse(out$partner_n_credible < n_recs, paste0(" Only ", out$partner_n_credible, " credible trusted supplier", ifelse(out$partner_n_credible == 1, "", "s"), "."), "")
    )
  )
  out$recommended_partner_1_member <- out$recommended_partner_1 %in% members
  # AIB status and its reasons. Capacity is judged on world supply shares (an AIB position share is a
  # share of allied supply and cannot say whether allies hold enough of the world's).
  cap_min <- cfg$allied_capacity_min %||% 0.3
  conc_min <- cfg$excessive_concentration_min %||% 0.5
  tc <- cand %>% dplyr::filter(.data$credible) %>% dplyr::group_by(.data$iso3, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(partner_trusted_capacity = if (all(is.na(.data$world_share))) NA_real_ else sum(.data$world_share, na.rm = TRUE),
                     .groups = "drop")
  out <- dplyr::left_join(out, tc, by = c("iso3", "tech", "supply_chain"))
  out$partner_trusted_capacity[out$partner_n_credible == 0] <- 0
  conc <- if ("coercive_top_share" %in% names(cells)) {
    cells$coercive_top_share[match(paste(out$iso3, out$tech, out$supply_chain), paste(cells$iso3, cells$tech, cells$supply_chain))]
  } else rep(NA_real_, nrow(out))
  r <- cbind(
    no_trusted_supplier = out$partner_n_credible == 0,
    single_credible_supplier = out$partner_n_credible == 1,
    weak_evidence = out$partner_n_credible > 0 & out$partner_evidence_quality %in% c("low", "none"),
    insufficient_allied_capacity = out$partner_n_credible > 0 & !is.na(out$partner_trusted_capacity) & out$partner_trusted_capacity < cap_min,
    excessive_concentration = !is.na(conc) & conc >= conc_min
  )
  out$partner_gap_reasons <- apply(r, 1, function(z) if (any(z)) paste(colnames(r)[z], collapse = ";") else NA_character_)
  out$partner_aib_status <- ifelse(r[, "no_trusted_supplier"] | r[, "single_credible_supplier"] | r[, "insufficient_allied_capacity"], "gap", "covered")
  dplyr::select(out, -"aib_high_quality_share", -"aib_stage_mixed")
}

PARTNER_GAP_REASONS <- c("no_trusted_supplier", "single_credible_supplier", "weak_evidence", "insufficient_allied_capacity",
                         "excessive_concentration")

# The allied division of labour per technology x six-stage node (the AIB view's matrix): the allied
# primary holder, up to two backups, redundancy and the gaps. capability: sp_cell_capability(). allied:
# the coalition ISO3 codes. coercive_powers: ISO3 codes. demand: tech, supply_chain, partner_demand,
# secure_demand (counts of countries). aib_cells: sp_aib_cell_capability()$cells.
# aib_gap (first that applies, all in aib_gaps): no_trusted_supplier (no allied holder at presence_min),
# single_point_of_failure (one allied holder has single_share or more of allied supply, or the AIB
# finds an irreplaceable member), insufficient_redundancy (fewer than redundancy_min allied holders),
# weak_evidence (no AIB node, or under half of the node weight high-confidence),
# insufficient_allied_capacity (allied holders together hold less than allied_capacity_min of world
# supply, judged on world supply shares where measured), excessive_concentration (the coercive powers
# hold excessive_concentration_min or more of world supply, so allied supply cannot dilute it), none.
# The two capacity reasons (spec XVI.1) come after the original four, so the primary aib_gap of a node is
# unchanged where one of those applies.
sp_aib_division <- function(capability, allied, coercive_powers, demand = NULL, aib_cells = NULL, cfg = list()) {
  presence <- cfg$presence_min %||% 0.02
  single <- cfg$single_point_share %||% 0.5
  red_min <- as.integer(cfg$redundancy_min %||% 3L)
  cap <- capability[capability$supply_chain != "Deployment", , drop = FALSE]
  out <- dplyr::bind_rows(lapply(split(cap, paste(cap$tech, cap$supply_chain, sep = "|")), function(d) {
    a <- d[d$iso3 %in% allied & d$capability >= presence, c("iso3", "capability"), drop = FALSE]
    a <- a[order(-a$capability, a$iso3), , drop = FALSE]
    at <- function(k, col) if (nrow(a) >= k) a[[col]][k] else NA
    data.frame(
      tech = d$tech[1], supply_chain = d$supply_chain[1], capability_source = d$capability_source[1],
      allied_share = sum(d$capability[d$iso3 %in% allied]),
      coercive_share = sum(d$capability[d$iso3 %in% coercive_powers]),
      world_leader = d$iso3[which.max(d$capability)], world_leader_share = max(d$capability),
      primary_holder = as.character(at(1, "iso3")), primary_share = as.numeric(at(1, "capability")),
      backup_1 = as.character(at(2, "iso3")), backup_1_share = as.numeric(at(2, "capability")),
      backup_2 = as.character(at(3, "iso3")), backup_2_share = as.numeric(at(3, "capability")),
      redundancy = nrow(a),
      allied_world_share = if (all(is.na(d$world_share))) NA_real_ else sum(d$world_share[d$iso3 %in% allied], na.rm = TRUE),
      coercive_world_share = if (all(is.na(d$world_share))) NA_real_ else sum(d$world_share[d$iso3 %in% coercive_powers], na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  if (!is.null(aib_cells) && nrow(aib_cells)) {
    out <- dplyr::left_join(out, aib_cells, by = c("tech", "supply_chain"))
  } else {
    out$aib_n_nodes <- NA_integer_
    out$aib_high_quality_share <- NA_real_
    out$aib_irreplaceable <- NA_character_
  }
  if (!is.null(demand)) out <- dplyr::left_join(out, demand, by = c("tech", "supply_chain"))
  top_of_allied <- ifelse(out$allied_share > 0, out$primary_share / out$allied_share, NA_real_)
  irr <- !is.na(out$aib_irreplaceable) & vapply(strsplit(dplyr::coalesce(out$aib_irreplaceable, ""), ";"), function(z) any(z %in% allied), logical(1))
  g_none <- out$redundancy == 0
  g_spof <- !g_none & ((!is.na(top_of_allied) & top_of_allied >= single) | irr)
  g_red <- !g_none & out$redundancy < red_min
  g_weak <- is.na(out$aib_n_nodes) | dplyr::coalesce(out$aib_high_quality_share, 0) < 0.5
  g_cap <- !g_none & !is.na(out$allied_world_share) & out$allied_world_share < (cfg$allied_capacity_min %||% 0.3)
  g_conc <- !is.na(out$coercive_world_share) & out$coercive_world_share >= (cfg$excessive_concentration_min %||% 0.5)
  out$primary_share_of_allied <- top_of_allied
  out$aib_gaps <- vapply(seq_len(nrow(out)), function(i) {
    g <- c(no_trusted_supplier = g_none[i], single_point_of_failure = g_spof[i], insufficient_redundancy = g_red[i], weak_evidence = g_weak[i],
           insufficient_allied_capacity = g_cap[i], excessive_concentration = g_conc[i])
    if (any(g)) paste(names(g)[g], collapse = ";") else NA_character_
  }, character(1))
  out$aib_gap <- ifelse(is.na(out$aib_gaps), "none", sub(";.*$", "", out$aib_gaps))
  out %>% dplyr::arrange(.data$tech, supply_chain_stage_factor(.data$supply_chain))
}
