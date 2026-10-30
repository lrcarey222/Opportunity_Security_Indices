# The credible-pathway test for Catch Up (docs/strategic_position.md, "Choice"; design D3). A country
# with a high prize but no top-quarter position catches up only where there is evidence it can build
# the capability. Two rules (config decision_framework.pathway.rule):
#
# anchor (used since 2026-10-30, user spec "capability anchor + corroboration"): pathway_credible is yes
# when the country has at least one CAPABILITY ANCHOR - evidence of a real industrial base in or next
# to the node - and at least one more signal from a DIFFERENT evidence family (another anchor or a
# supporting signal). Two enabling signals without an anchor are never enough (high feasibility + low
# input costs is not Catch Up). The anchors, each an absolute measure (no Competitive Position score):
#
#   direct_supply_share    the node's MEASURED world supply share on its primary measure (mine, refinery
#                          or manufacturing output, or the Atlas export share where that is the measure;
#                          home_share_exposure, never a listing's residual) >= direct_share_min
#                                                                                         [family direct]
#   direct_export_share    the node basket's 2022-24 benchmark world export share
#                          (trade_benchmark_share) >= direct_share_min                    [family direct]
#   operating_capacity     GCIM operating capacity, share of world operating capacity of the node's most
#                          material product >= operating_capacity_share_min             [family capacity]
#   committed_pipeline     GCIM capacity under construction or built and not yet online, share of world
#                          operating capacity >= committed_pipeline_share_min           [family pipeline]
#   adjacent_stage_share   absolute adjacency: an adjacent stage of the same technology with measured
#                          world share >= adjacent_share                               [family adjacency]
#   resource_base          Extraction only: recorded reserves or resource above the cell floor
#                                                                                       [family resource]
#
# Supporting signals (corroborate an anchor, never qualify on their own):
#
#   feasibility            Atlas product-space feasibility in the top quarter (feas_pct >= feasibility_pct)
#   input_cost             Input Cost Index in the cheapest quarter (>= input_cost_pct)
#   adjacent_stage_relative  an adjacent stage among the country's own stronger cells (within-country CP
#                          percentile >= adjacent_cp_within_pct), whatever its world share [family adjacency]
#   learning               the posture engine's Test 3 learning lower bound moderate or strong: curated
#                          node learning inputs on a "base" that may be feasibility or relative adjacency
#                          alone, so context rather than capability
#   announced_pipeline     GCIM announced or paused capacity >= announced_pipeline_share_min of world
#                          operating capacity (speculative, so support only)          [family pipeline]
#
# A supporting signal in the same family as the anchor does not corroborate it (relative adjacency does
# not corroborate absolute adjacency of the same stages). Extraction also needs a resource base.
# no = an anchor family is observed and none fires (basis "...;no_anchor"), or the anchor is not
# corroborated though other evidence is observed ("...;anchor_uncorroborated"), or no resource base at
# Extraction; unknown = the evidence cannot decide (no anchor family observed: "anchor_unobserved"; an
# anchor with nothing else observed: "corroboration_unobserved"; nothing observed: "no_evidence"),
# read as not credible with lower confidence.
#
# legs (design D3 at min_legs 1, the 2026-10-30 two-leg rule at 2; kept for the sensitivity): yes when
# at least min_legs of feasibility, pipeline (raw GCIM pipeline > pipeline_min), adjacent_stage
# (absolute or relative), learning and input_cost fire; no / unknown and the too_few_legs /
# too_few_observed bases as before.
#
# Both rules write catch_up_anchor (TRUE / FALSE / NA when no anchor family is observed; NA at
# Deployment) and catch_up_anchor_type (the anchors that fired), so the legs rule can be compared.
#
# Pure functions: no IO. Needs posture_engine.R (SP_STAGE_ADJACENT) and sp_helpers.R.

SP_PATHWAY_EVIDENCE <- c("feasibility", "pipeline", "adjacent_stage", "learning", "input_cost")
SP_PATHWAY_ANCHORS <- c("direct_supply_share", "direct_export_share", "operating_capacity", "committed_pipeline",
                        "adjacent_stage_share", "resource_base")
SP_PATHWAY_SUPPORT <- c("feasibility", "input_cost", "adjacent_stage_relative", "learning", "announced_pipeline")
SP_PATHWAY_RULES <- c("anchor", "legs")
# the evidence family of each anchor and supporting signal (independence is between families)
SP_PATHWAY_FAMILY <- c(direct_supply_share = "direct", direct_export_share = "direct", operating_capacity = "capacity",
                       committed_pipeline = "pipeline", adjacent_stage_share = "adjacency", resource_base = "resource",
                       feasibility = "feasibility", input_cost = "input_cost", adjacent_stage_relative = "adjacency",
                       learning = "learning", announced_pipeline = "pipeline")

# The economy's MEASURED share of world supply: home_share_exposure where the table carries it, else
# home_share. home_share gives an economy that a partial listing omits the listing's residual ("Rest of
# world") as an upper bound - every unlisted economy held 0.87% of battery refining - so the anchors read
# the measured share (as contender status does: decision_engine.R sp_measured_share()).
sp_pathway_measured_share <- function(tbl) {
  h <- if ("home_share" %in% names(tbl)) tbl$home_share else rep(NA_real_, nrow(tbl))
  if ("home_share_exposure" %in% names(tbl)) dplyr::coalesce(tbl$home_share_exposure, h) else h
}

# Adjacent-stage evidence per cell: the best adjacent stage's world share (home_share, the legs rule's
# input, and the measured share, the anchor's) and within-country CP percentile. tbl: iso3, tech,
# supply_chain, home_share, cp_within_country_pct (home_share_exposure optional).
sp_pathway_adjacent <- function(tbl) {
  pairs <- do.call(rbind, lapply(names(SP_STAGE_ADJACENT), function(s) {
    adj <- SP_STAGE_ADJACENT[[s]]
    if (!length(adj)) return(NULL)
    data.frame(supply_chain = s, adjacent_stage = adj, stringsAsFactors = FALSE)
  }))
  vals <- data.frame(iso3 = tbl$iso3, tech = tbl$tech, adjacent_stage = tbl$supply_chain, adj_share = tbl$home_share,
                     adj_measured = sp_pathway_measured_share(tbl), adj_cp = tbl$cp_within_country_pct, stringsAsFactors = FALSE)
  top <- function(v) if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE)
  tbl[, c("iso3", "tech", "supply_chain")] %>%
    dplyr::inner_join(pairs, by = "supply_chain", relationship = "many-to-many") %>%
    dplyr::left_join(vals, by = c("iso3", "tech", "adjacent_stage")) %>%
    dplyr::group_by(.data$iso3, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(
      pw_adjacent_share = top(.data$adj_share),
      pw_adjacent_share_measured = top(.data$adj_measured),
      pw_adjacent_cp_within = top(.data$adj_cp),
      .groups = "drop"
    )
}

# Join a per-node table onto x: observed where the source covers the node (tech x supply_chain
# present in it; absence there is an observed zero), NA elsewhere.
sp_pathway_covered_value <- function(x, src, value_col) {
  out <- rep(NA_real_, nrow(x))
  if (is.null(src) || !nrow(src) || !value_col %in% names(src)) return(out)
  cov <- paste(x$tech, x$supply_chain) %in% paste(src$tech, src$supply_chain)
  out[cov] <- 0
  k <- match(paste(x$iso3, x$tech, x$supply_chain), paste(src$iso3, src$tech, src$supply_chain))
  hit <- cov & !is.na(k) & !is.na(src[[value_col]][k])
  out[hit] <- src[[value_col]][k[hit]]
  out
}

# ";"-joined names of the TRUE columns of a logical matrix, per row (NA where none).
sp_pathway_names <- function(m) {
  if (!ncol(m)) return(rep(NA_character_, nrow(m)))
  apply(m, 1, function(r) if (any(r)) paste(colnames(m)[r], collapse = ";") else NA_character_)
}

# tbl: the cell table (iso3, tech, supply_chain, home_share, trade_benchmark_share, cp_within_country_pct,
# feas_pct, resource_base, t3_learning_lo). pipeline: iso3, tech, supply_chain, pipeline_capacity (raw,
# the monitor's pipeline capacity; the legs rule) and covered: tech, supply_chain the monitor covers.
# input_cost: iso3, tech, supply_chain, input_cost_index (higher = cheaper). capacity: iso3, tech,
# supply_chain, operating_share, committed_share, announced_share (investment_capacity_shares(); the
# nodes it lists are covered, absence there = 0). cfg: decision_framework$pathway.
# Adds the pw_* evidence columns, catch_up_anchor, catch_up_anchor_type, pathway_support,
# pathway_credible (yes / no / unknown; NA at Deployment), pathway_basis and pathway_rule.
sp_pathway <- function(tbl, pipeline = NULL, covered = NULL, input_cost = NULL, cfg = list(), capacity = NULL) {
  rule <- if (is.null(cfg$rule)) "legs" else cfg$rule
  if (!is.character(rule) || length(rule) != 1 || !rule %in% SP_PATHWAY_RULES) {
    stop("decision_framework$pathway$rule must be one of ", paste(SP_PATHWAY_RULES, collapse = ", "), call. = FALSE)
  }
  f_min <- cfg$feasibility_pct %||% 0.75
  p_min <- cfg$pipeline_min %||% 0
  a_share <- cfg$adjacent_share %||% 0.05
  a_cp <- cfg$adjacent_cp_within_pct %||% 0.6
  l_lv <- unlist(cfg$learning_levels %||% c("moderate", "strong"))
  c_min <- cfg$input_cost_pct %||% 0.75
  d_min <- cfg$direct_share_min %||% 0.005
  o_min <- cfg$operating_capacity_share_min %||% 0.005
  cm_min <- cfg$committed_pipeline_share_min %||% 0.01
  an_min <- cfg$announced_pipeline_share_min %||% 0.02
  k_min <- if (is.null(cfg$min_legs)) 1 else cfg$min_legs   # not %||%: a NA setting must be refused, not defaulted
  if (!is.numeric(k_min) || length(k_min) != 1 || !is.finite(k_min) || k_min < 1 || k_min > length(SP_PATHWAY_EVIDENCE) || k_min != round(k_min)) {
    stop("decision_framework$pathway$min_legs must be a whole number from 1 to ", length(SP_PATHWAY_EVIDENCE), call. = FALSE)
  }
  col <- function(n, default = NA) if (n %in% names(tbl)) tbl[[n]] else rep(default, nrow(tbl))
  drop <- c("pw_feasibility_pct", "pw_pipeline", "pw_adjacent_share", "pw_adjacent_share_measured", "pw_adjacent_cp_within", "pw_learning",
            "pw_input_cost_pct", "pw_direct_share", "pw_export_share", "pw_operating_share", "pw_committed_share",
            "pw_announced_share", "catch_up_anchor", "catch_up_anchor_type", "pathway_support", "pathway_credible",
            "pathway_basis", "pathway_rule")
  x <- tbl[, setdiff(names(tbl), drop), drop = FALSE]
  x$.row <- seq_len(nrow(x))
  adj <- sp_pathway_adjacent(x)
  x <- dplyr::left_join(x, adj, by = c("iso3", "tech", "supply_chain"))
  x$pw_feasibility_pct <- col("feas_pct", NA_real_)[x$.row]
  # raw pipeline (legs rule): observed where the monitor covers the node (absence = 0), NA elsewhere
  x$pw_pipeline <- NA_real_
  if (!is.null(covered) && nrow(covered)) {
    cov <- paste(x$tech, x$supply_chain) %in% paste(covered$tech, covered$supply_chain)
    x$pw_pipeline[cov] <- 0
    if (!is.null(pipeline) && nrow(pipeline)) {
      k <- match(paste(x$iso3, x$tech, x$supply_chain), paste(pipeline$iso3, pipeline$tech, pipeline$supply_chain))
      hit <- cov & !is.na(k)
      x$pw_pipeline[hit] <- pipeline$pipeline_capacity[k[hit]]
    }
  }
  lv <- col("t3_learning_lo", NA_character_)[x$.row]
  x$pw_learning <- ifelse(is.na(lv) | lv %in% "n_a", NA_character_, lv)
  x$pw_input_cost_pct <- NA_real_
  if (!is.null(input_cost) && nrow(input_cost)) {
    ic <- input_cost[!is.na(input_cost$input_cost_index), c("iso3", "tech", "supply_chain", "input_cost_index")]
    ic <- ic[!duplicated(ic[, c("iso3", "tech", "supply_chain")]), , drop = FALSE]
    ic$pct <- stats::ave(ic$input_cost_index, paste(ic$tech, ic$supply_chain), FUN = sp_midrank_pct)
    k <- match(paste(x$iso3, x$tech, x$supply_chain), paste(ic$iso3, ic$tech, ic$supply_chain))
    x$pw_input_cost_pct <- ic$pct[k]
  }
  x$pw_direct_share <- sp_pathway_measured_share(tbl)[x$.row]
  x$pw_export_share <- col("trade_benchmark_share", NA_real_)[x$.row]
  x$pw_operating_share <- sp_pathway_covered_value(x, capacity, "operating_share")
  x$pw_committed_share <- sp_pathway_covered_value(x, capacity, "committed_share")
  x$pw_announced_share <- sp_pathway_covered_value(x, capacity, "announced_share")
  dep <- x$supply_chain == "Deployment"
  ext <- x$supply_chain == "Extraction"
  rb <- col("resource_base", NA)[x$.row]
  no_resource <- ext & rb %in% FALSE
  unknown_resource <- ext & is.na(rb)
  ge <- function(v, t) !is.na(v) & v >= t

  # ---- capability anchors and supporting signals (both rules report the anchor) ----
  anchors <- cbind(
    direct_supply_share = ge(x$pw_direct_share, d_min),
    direct_export_share = ge(x$pw_export_share, d_min),
    operating_capacity = ge(x$pw_operating_share, o_min),
    committed_pipeline = ge(x$pw_committed_share, cm_min),
    adjacent_stage_share = ge(x$pw_adjacent_share_measured, a_share),
    resource_base = ext & rb %in% TRUE
  )
  support <- cbind(
    feasibility = ge(x$pw_feasibility_pct, f_min),
    input_cost = ge(x$pw_input_cost_pct, c_min),
    adjacent_stage_relative = ge(x$pw_adjacent_cp_within, a_cp),
    learning = x$pw_learning %in% l_lv,
    announced_pipeline = ge(x$pw_announced_share, an_min)
  )
  fams <- unique(SP_PATHWAY_FAMILY)
  fam_obs <- cbind(
    direct = !is.na(x$pw_direct_share) | !is.na(x$pw_export_share),
    capacity = !is.na(x$pw_operating_share),
    pipeline = !is.na(x$pw_committed_share),
    adjacency = !is.na(x$pw_adjacent_share_measured) | !is.na(x$pw_adjacent_cp_within),
    resource = ext & !is.na(rb),
    feasibility = !is.na(x$pw_feasibility_pct),
    input_cost = !is.na(x$pw_input_cost_pct),
    learning = !is.na(x$pw_learning)
  )[, fams, drop = FALSE]
  fired <- cbind(anchors, support)
  fam_fired <- vapply(fams, function(f) rowSums(fired[, SP_PATHWAY_FAMILY[colnames(fired)] == f, drop = FALSE]) > 0, logical(nrow(x)))
  if (!is.matrix(fam_fired)) fam_fired <- matrix(fam_fired, nrow = nrow(x), dimnames = list(NULL, fams))
  anchor_fams <- c("direct", "capacity", "pipeline", "adjacency", "resource")
  any_anchor <- rowSums(anchors) > 0
  anchor_obs <- rowSums(fam_obs[, anchor_fams, drop = FALSE]) > 0
  n_fam_fired <- rowSums(fam_fired)
  n_fam_obs <- rowSums(fam_obs)
  # an anchor plus at least one signal from another family (anchor families are among the fired ones)
  corroborated <- any_anchor & n_fam_fired >= 2
  anchor_type <- sp_pathway_names(anchors)
  support_type <- sp_pathway_names(support)
  x$catch_up_anchor <- ifelse(dep, NA, ifelse(any_anchor, TRUE, ifelse(anchor_obs, FALSE, NA)))
  x$catch_up_anchor_type <- ifelse(dep, NA_character_, anchor_type)
  x$pathway_support <- ifelse(dep, NA_character_, support_type)
  x$pathway_rule <- ifelse(dep, NA_character_, rule)
  ev <- function(...) {   # the fired evidence (anchors first) and a status code
    parts <- cbind(...)
    apply(parts, 1, function(r) paste(stats::na.omit(r), collapse = ";"))
  }

  if (rule == "anchor") {
    fired_names <- ev(anchor_type, support_type)
    with_code <- function(code) ifelse(nzchar(fired_names), paste0(fired_names, ";", code), code)
    x$pathway_credible <- dplyr::case_when(
      dep ~ NA_character_,
      no_resource ~ "no",
      corroborated & unknown_resource ~ "unknown",
      corroborated ~ "yes",
      n_fam_obs == 0 ~ "unknown",
      !any_anchor & !anchor_obs ~ "unknown",
      !any_anchor ~ "no",
      n_fam_obs > n_fam_fired ~ "no",
      TRUE ~ "unknown"
    )
    x$pathway_basis <- dplyr::case_when(
      dep ~ NA_character_,
      no_resource ~ "no_resource_base",
      corroborated & unknown_resource ~ paste0(fired_names, ";resource_base_unknown"),
      corroborated ~ fired_names,
      n_fam_obs == 0 ~ "no_evidence",
      !any_anchor & !anchor_obs ~ with_code("anchor_unobserved"),
      !any_anchor ~ with_code("no_anchor"),
      n_fam_obs > n_fam_fired ~ with_code("anchor_uncorroborated"),
      TRUE ~ with_code("corroboration_unobserved")
    )
  } else {
    fires <- cbind(
      feasibility = support[, "feasibility"],
      pipeline = !is.na(x$pw_pipeline) & x$pw_pipeline > p_min,
      # the legs rule keeps its historical input (home_share, residuals included) so its counts reproduce
      adjacent_stage = ge(x$pw_adjacent_share, a_share) | support[, "adjacent_stage_relative"],
      learning = support[, "learning"],
      input_cost = support[, "input_cost"]
    )
    observed <- cbind(!is.na(x$pw_feasibility_pct), !is.na(x$pw_pipeline),
                      !is.na(x$pw_adjacent_share) | !is.na(x$pw_adjacent_cp_within), !is.na(x$pw_learning),
                      !is.na(x$pw_input_cost_pct))
    n_fire <- rowSums(fires)
    n_obs <- rowSums(observed)
    enough <- n_fire >= k_min
    # every observed leg fires, but too few are observed to reach min_legs: the data cannot decide
    undecided <- !enough & n_obs > 0 & n_fire == n_obs
    basis <- apply(fires, 1, function(r) if (any(r)) paste(SP_PATHWAY_EVIDENCE[r], collapse = ";") else NA_character_)
    x$pathway_credible <- dplyr::case_when(
      dep ~ NA_character_,
      no_resource ~ "no",
      enough & unknown_resource ~ "unknown",
      enough ~ "yes",
      undecided ~ "unknown",
      n_obs > 0 ~ "no",
      TRUE ~ "unknown"
    )
    x$pathway_basis <- dplyr::case_when(
      dep ~ NA_character_,
      no_resource ~ "no_resource_base",
      enough & unknown_resource ~ paste0(basis, ";resource_base_unknown"),
      enough ~ basis,
      undecided ~ paste0(basis, ";too_few_observed"),
      n_fire > 0 ~ paste0(basis, ";too_few_legs"),
      n_obs > 0 ~ "none_observed_fires",
      TRUE ~ "no_evidence"
    )
  }
  x <- x[order(x$.row), , drop = FALSE]
  x$.row <- NULL
  x
}
