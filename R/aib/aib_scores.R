# Allied Industrial Base (AIB): the measures the lexicographic tie-breaks read
# (docs/aib_methodology.md, 5). None is combined with another into a score; each is used on its own
# at its own priority level.
#   specialization  capped Balassa RCA of the exporter in the node (objective 2)
#   opportunity     Export Opportunity OP(exporter -> market, tech, stage), the partner theme's
#                   bilateral trade-feasibility index (objective 3)
#   friendshore     Friendshore FS(investor/buyer -> host, tech, stage), the bilateral
#                   investment-feasibility index (investment ordering, step 4)
#   compatibility   revealed policy/climate compatibility of two economies (objective 4)

# Objective-2 weight of an exporter: its revealed specialization, min(RCA, cap) / cap, or its
# production / capacity share where a proxy exists and is larger (trade misses production used at
# home: refining, polysilicon, wafers); a world-share term three orders of magnitude smaller only
# breaks ties. A missing RCA (no total exports on record) counts as neutral.
aib_specialization_weight <- function(rca, world_share, rca_cap = 4, neutral = 0.5, production_share = NULL) {
  base <- ifelse(is.na(rca), neutral, pmin(pmax(rca, 0), rca_cap) / rca_cap)
  if (!is.null(production_share)) base <- pmax(base, dplyr::coalesce(production_share, 0))
  base + 1e-3 * dplyr::coalesce(world_share, 0)
}

# Stop when a partner, development or policy table is still keyed by Upstream / Midstream / Downstream
# (built before the six-stage migration of 2026-10-26): no node stage would match it and every score
# would silently turn neutral.
aib_assert_six_stage_table <- function(stage, label) {
  old <- intersect(unique(as.character(stage)), SUPPLY_CHAIN_LEGACY_STAGES)
  if (length(old)) {
    stop(label, " is keyed by ", paste(old, collapse = " / "), ", not the six supply-chain stages (",
         paste(SUPPLY_CHAIN_STAGES, collapse = ", "), "): rebuild it before the AIB.", call. = FALSE)
  }
  invisible(TRUE)
}

# Long dyad index table (partner_opportunity_tbl / partner_friendshore_tbl rows of one index
# variable) reduced to reporter_iso, partner_iso, tech, supply_chain, value. `stages`: the supplying
# stages (every stage but Deployment), where the AIB's nodes sit.
aib_dyad_index <- function(tbl, variable, stages = SUPPLY_CHAIN_SUPPLY_STAGES) {
  require_columns(tbl, c("reporter_iso", "partner_iso", "tech", "supply_chain", "variable", "value"), label = "partner index table")
  keep <- tbl$variable == variable & tbl$supply_chain %in% stages
  tibble::tibble(
    reporter_iso = as.character(tbl$reporter_iso[keep]),
    partner_iso = as.character(tbl$partner_iso[keep]),
    tech = as.character(tbl$tech[keep]),
    supply_chain = as.character(tbl$supply_chain[keep]),
    value = as.numeric(tbl$value[keep])
  ) %>%
    dplyr::filter(!is.na(value))
}

# The OSI cells ("tech|stage") a node is scored in: `techs` with `supply_chain` either one stage for all
# of them or one per technology (the crosswalk's score_cells; stage is relative to the technology).
aib_score_cell_keys <- function(techs, supply_chain) {
  if (!length(techs)) return(character())
  if (!length(supply_chain) || !length(supply_chain) %in% c(1L, length(techs))) {
    stop("aib_score_cell_keys(): give one stage, or one per technology (", length(techs), ").")
  }
  paste(techs, rep_len(as.character(supply_chain), length(techs)), sep = "|")
}

# A dyad index for one node: the mean over the node's score cells (each technology at its own six-stage
# stage, aib_score_cell_keys()) per (reporter, partner). Returns a lookup environment keyed "REP|PAR"
# for fast access.
aib_node_dyad_lookup <- function(dyad_index, techs, supply_chain) {
  d <- dyad_index[dyad_index$tech %in% techs, , drop = FALSE]
  d <- d[paste(d$tech, d$supply_chain, sep = "|") %in% aib_score_cell_keys(techs, supply_chain), , drop = FALSE]
  if (!nrow(d)) return(new.env(hash = TRUE, size = 1L))
  d <- d %>%
    dplyr::group_by(reporter_iso, partner_iso) %>%
    dplyr::summarise(value = mean(value), .groups = "drop")
  env <- new.env(hash = TRUE, size = nrow(d))
  keys <- paste(d$reporter_iso, d$partner_iso, sep = "|")
  for (k in seq_along(keys)) assign(keys[k], d$value[k], envir = env)
  env
}

aib_lookup_pairs <- function(env, reporter, partner, neutral = 0.5) {
  keys <- paste(reporter, partner, sep = "|")
  out <- unlist(mget(keys, envir = env, ifnotfound = list(NA_real_)), use.names = FALSE)
  ifelse(is.na(out), neutral, out)
}

aib_lookup_pairs_raw <- function(env, reporter, partner) {
  keys <- paste(reporter, partner, sep = "|")
  unlist(mget(keys, envir = env, ifnotfound = list(NA_real_)), use.names = FALSE)
}

# IEA PAMS policy intensity per (iso3, tech, supply_chain): the rank S-curve of the log policy
# strength within the technology stage (policy_outputs.rds$policy_agg, six-stage since 2026-10-26).
# EU-level measures (iso3 EUR) are added to each member's own before ranking. Economies with no
# recorded policy get 0.
aib_pams_intensity <- function(policy_agg, eu_members = EU27_ISO3, eu_code = "EUR") {
  require_columns(policy_agg, c("iso3", "tech", "supply_chain", "strength_sum"), label = "policy_agg")
  pa <- tibble::tibble(
    iso3 = toupper(as.character(policy_agg$iso3)),
    tech = as.character(policy_agg$tech),
    supply_chain = as.character(policy_agg$supply_chain),
    strength = dplyr::coalesce(as.numeric(policy_agg$strength_sum), 0)
  )
  eu <- pa[pa$iso3 == eu_code, , drop = FALSE]
  if (nrow(eu)) {
    eu_rows <- tidyr::expand_grid(iso3 = eu_members, dplyr::select(eu, -iso3))
    pa <- dplyr::bind_rows(pa[pa$iso3 != eu_code, , drop = FALSE], eu_rows) %>%
      dplyr::group_by(iso3, tech, supply_chain) %>%
      dplyr::summarise(strength = sum(strength), .groups = "drop")
  }
  pa %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(intensity = median_scurve(log1p(pmax(strength, 0)))) %>%
    dplyr::ungroup() %>%
    dplyr::select(iso3, tech, supply_chain, intensity)
}

# Pairwise revealed compatibility for one node: the mean of the available components
#   pams  1 - |intensity_a - intensity_b| in the node's technologies and stage (absent = 0 policy)
#   cat   1 - |score_a - score_b| / 0.75 on the Climate Action Tracker overall rating
#         (0.25 critically insufficient ... 1 almost sufficient)
# Missing on both components is neutral. A country is fully compatible with itself.
aib_compatibility <- function(a, b, pams_node, cat_scores, components = c("pams", "cat"), neutral = 0.5) {
  n <- length(a)
  parts <- list()
  if ("pams" %in% components && !is.null(pams_node)) {
    ia <- pams_node$intensity[match(a, pams_node$iso3)]
    ib <- pams_node$intensity[match(b, pams_node$iso3)]
    ia[is.na(ia)] <- 0
    ib[is.na(ib)] <- 0
    has <- (a %in% pams_node$iso3) | (b %in% pams_node$iso3)
    parts$pams <- ifelse(has, 1 - abs(ia - ib), NA_real_)
  }
  if ("cat" %in% components && !is.null(cat_scores)) {
    ca <- cat_scores$cat_score[match(a, cat_scores$iso3)]
    cb <- cat_scores$cat_score[match(b, cat_scores$iso3)]
    parts$cat <- 1 - abs(ca - cb) / 0.75
  }
  if (!length(parts)) return(rep(neutral, n))
  m <- do.call(cbind, parts)
  out <- rowMeans(m, na.rm = TRUE)
  out[is.nan(out)] <- neutral
  out[a == b] <- 1
  out
}

# PAMS intensity of one node (mean over its score cells, aib_score_cell_keys()).
aib_pams_node <- function(pams, techs, supply_chain) {
  p <- pams[pams$tech %in% techs, , drop = FALSE]
  p <- p[paste(p$tech, p$supply_chain, sep = "|") %in% aib_score_cell_keys(techs, supply_chain), , drop = FALSE]
  if (!nrow(p)) return(NULL)
  p %>% dplyr::group_by(iso3) %>% dplyr::summarise(intensity = mean(intensity), .groups = "drop")
}

# The three score functions the optimizer reads for one node: spec(e), op(e, m), compat(a, b).
# An exporter absent from the node's trade has no revealed specialization (0); a missing RCA of
# an observed exporter is neutral; missing dyad scores are neutral.
aib_node_scores <- function(node_id, techs, supply_chain, exporter_metrics, op_index = NULL, pams = NULL,
                            cat_scores = NULL, rca_cap = 4, neutral = 0.5, components = c("pams", "cat")) {
  ex <- exporter_metrics[exporter_metrics$node_id == node_id, , drop = FALSE]
  prod <- if ("production_share" %in% names(ex)) ex$production_share else NULL
  spec_vals <- stats::setNames(aib_specialization_weight(ex$export_rca, ex$world_share, rca_cap, neutral, prod), ex$exporter_iso)
  op_env <- if (is.null(op_index)) new.env() else aib_node_dyad_lookup(op_index, techs, supply_chain)
  pams_node <- if (is.null(pams)) NULL else aib_pams_node(pams, techs, supply_chain)
  list(
    spec = function(e) { v <- unname(spec_vals[e]); v[is.na(v)] <- 0; v },
    op = function(e, m) aib_lookup_pairs(op_env, e, m, neutral),
    op_raw = function(e, m) aib_lookup_pairs_raw(op_env, e, m),
    compat = function(a, b) aib_compatibility(a, b, pams_node, cat_scores, components, neutral)
  )
}
