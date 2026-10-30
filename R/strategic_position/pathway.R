# The credible-pathway test for Catch Up (docs/strategic_position.md, "Choice"; design D3). A country
# with a high prize but no top-quarter position catches up only where there is evidence it can build
# the capability. pathway_credible is yes when ANY of these fires:
#
#   feasibility     Atlas product-space feasibility of the node's HS basket in the top quarter of
#                   countries (feas_pct >= feasibility_pct)
#   pipeline        Clean Investment Monitor announced / under-construction capacity in the node > 0
#   adjacent_stage  an established adjacent stage of the same technology: world share there >=
#                   adjacent_share, or that stage among the country's stronger cells (within-country CP
#                   percentile >= adjacent_cp_within_pct)
#   learning        the curated learning inputs (process knowledge, workforce and suppliers, adjacent
#                   innovation) on a domestic base: the posture engine's Test 3 learning lower bound is
#                   moderate or strong (learning counts only where the country has a base to learn from)
#   input_cost      an input-cost advantage: Input Cost Index in the cheapest quarter (input_cost_pct)
#
# Extraction also needs a resource base (recorded reserves or resource above the cell floor). no = some
# evidence observed and none fires (or no resource base at Extraction); unknown = nothing observed,
# which the decision reads as not credible (with lower confidence). pathway_basis names what fired.
#
# Pure functions: no IO. Needs posture_engine.R (SP_STAGE_ADJACENT) and sp_helpers.R.

SP_PATHWAY_EVIDENCE <- c("feasibility", "pipeline", "adjacent_stage", "learning", "input_cost")

# Adjacent-stage evidence per cell: the best adjacent stage's world share and within-country CP
# percentile. tbl: iso3, tech, supply_chain, home_share, cp_within_country_pct.
sp_pathway_adjacent <- function(tbl) {
  pairs <- do.call(rbind, lapply(names(SP_STAGE_ADJACENT), function(s) {
    adj <- SP_STAGE_ADJACENT[[s]]
    if (!length(adj)) return(NULL)
    data.frame(supply_chain = s, adjacent_stage = adj, stringsAsFactors = FALSE)
  }))
  vals <- tbl[, c("iso3", "tech", "supply_chain", "home_share", "cp_within_country_pct")]
  names(vals) <- c("iso3", "tech", "adjacent_stage", "adj_share", "adj_cp")
  tbl[, c("iso3", "tech", "supply_chain")] %>%
    dplyr::inner_join(pairs, by = "supply_chain", relationship = "many-to-many") %>%
    dplyr::left_join(vals, by = c("iso3", "tech", "adjacent_stage")) %>%
    dplyr::group_by(.data$iso3, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(
      pw_adjacent_share = if (all(is.na(.data$adj_share))) NA_real_ else max(.data$adj_share, na.rm = TRUE),
      pw_adjacent_cp_within = if (all(is.na(.data$adj_cp))) NA_real_ else max(.data$adj_cp, na.rm = TRUE),
      .groups = "drop"
    )
}

# tbl: the cell table (iso3, tech, supply_chain, home_share, cp_within_country_pct, feas_pct,
# resource_base, t3_learning_lo). pipeline: iso3, tech, supply_chain, pipeline_capacity (raw, the
# monitor's announced + under-construction capacity; rows only where the monitor records any) and
# covered: tech, supply_chain the monitor covers (absence there is an observed zero). input_cost: iso3,
# tech, supply_chain, input_cost_index (higher = cheaper). cfg: decision_framework$pathway.
# Adds pw_feasibility_pct, pw_pipeline, pw_adjacent_share, pw_adjacent_cp_within, pw_learning,
# pw_input_cost_pct, pathway_credible (yes / no / unknown; NA at Deployment) and pathway_basis.
sp_pathway <- function(tbl, pipeline = NULL, covered = NULL, input_cost = NULL, cfg = list()) {
  f_min <- cfg$feasibility_pct %||% 0.75
  p_min <- cfg$pipeline_min %||% 0
  a_share <- cfg$adjacent_share %||% 0.05
  a_cp <- cfg$adjacent_cp_within_pct %||% 0.6
  l_lv <- unlist(cfg$learning_levels %||% c("moderate", "strong"))
  c_min <- cfg$input_cost_pct %||% 0.75
  col <- function(n, default = NA) if (n %in% names(tbl)) tbl[[n]] else rep(default, nrow(tbl))
  drop <- c("pw_feasibility_pct", "pw_pipeline", "pw_adjacent_share", "pw_adjacent_cp_within", "pw_learning",
            "pw_input_cost_pct", "pathway_credible", "pathway_basis")
  x <- tbl[, setdiff(names(tbl), drop), drop = FALSE]
  x$.row <- seq_len(nrow(x))
  adj <- sp_pathway_adjacent(x)
  x <- dplyr::left_join(x, adj, by = c("iso3", "tech", "supply_chain"))
  x$pw_feasibility_pct <- col("feas_pct", NA_real_)[x$.row]
  # pipeline: observed where the monitor covers the node (absence = 0), NA elsewhere
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
  fires <- cbind(
    feasibility = !is.na(x$pw_feasibility_pct) & x$pw_feasibility_pct >= f_min,
    pipeline = !is.na(x$pw_pipeline) & x$pw_pipeline > p_min,
    adjacent_stage = (!is.na(x$pw_adjacent_share) & x$pw_adjacent_share >= a_share) |
      (!is.na(x$pw_adjacent_cp_within) & x$pw_adjacent_cp_within >= a_cp),
    learning = x$pw_learning %in% l_lv,
    input_cost = !is.na(x$pw_input_cost_pct) & x$pw_input_cost_pct >= c_min
  )
  observed <- cbind(!is.na(x$pw_feasibility_pct), !is.na(x$pw_pipeline),
                    !is.na(x$pw_adjacent_share) | !is.na(x$pw_adjacent_cp_within), !is.na(x$pw_learning),
                    !is.na(x$pw_input_cost_pct))
  any_fire <- rowSums(fires) > 0
  any_obs <- rowSums(observed) > 0
  basis <- apply(fires, 1, function(r) if (any(r)) paste(SP_PATHWAY_EVIDENCE[r], collapse = ";") else NA_character_)
  rb <- col("resource_base", NA)[x$.row]
  no_resource <- x$supply_chain == "Extraction" & rb %in% FALSE
  unknown_resource <- x$supply_chain == "Extraction" & is.na(rb)
  x$pathway_credible <- dplyr::case_when(
    x$supply_chain == "Deployment" ~ NA_character_,
    no_resource ~ "no",
    any_fire & unknown_resource ~ "unknown",
    any_fire ~ "yes",
    any_obs ~ "no",
    TRUE ~ "unknown"
  )
  x$pathway_basis <- dplyr::case_when(
    x$supply_chain == "Deployment" ~ NA_character_,
    no_resource ~ "no_resource_base",
    any_fire & unknown_resource ~ paste0(basis, ";resource_base_unknown"),
    any_fire ~ basis,
    any_obs ~ "none_observed_fires",
    TRUE ~ "no_evidence"
  )
  x <- x[order(x$.row), , drop = FALSE]
  x$.row <- NULL
  x
}
