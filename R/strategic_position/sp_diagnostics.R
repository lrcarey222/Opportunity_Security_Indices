# Validation diagnostics for the strategic position framework (docs/strategic_position.md). Each
# function takes the canonical strategic_position_tbl (one row per Country x tech x supply_chain)
# and returns a small table that answers one of the validation questions. Pure functions: no IO.
# Needs R/strategic_position/sp_helpers.R.

sp_diag_check <- function(tbl, cols, label) {
  missing <- setdiff(cols, names(tbl))
  if (length(missing) > 0) stop(label, ": missing column(s) ", paste(missing, collapse = ", "))
  invisible(TRUE)
}

# Spearman correlation that returns NA (not an error or warning) with fewer than 3 complete pairs
# or a constant input.
sp_spearman <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  if (sum(ok) < 3 || length(unique(x[ok])) < 2 || length(unique(y[ok])) < 2) return(NA_real_)
  suppressWarnings(stats::cor(x[ok], y[ok], method = "spearman"))
}

# Q1-Q2. The strongest countries in each technology x stage cell, with the evidence behind them.
sp_diag_specialists <- function(tbl, top_n = 5) {
  sp_diag_check(tbl, c("Country", "tech", "supply_chain", "competitive_position_score",
                       "competitive_position_percentile", "stage_evidence_quality_cp"), "sp_diag_specialists()")
  tbl %>%
    dplyr::filter(!is.na(.data$competitive_position_percentile)) %>%
    dplyr::group_by(.data$tech, .data$supply_chain) %>%
    dplyr::arrange(dplyr::desc(.data$competitive_position_score), .by_group = TRUE) %>%
    dplyr::mutate(n_countries = dplyr::n(), position_rank = dplyr::row_number()) %>%
    dplyr::filter(.data$position_rank <= top_n) %>%
    dplyr::summarise(
      n_countries = dplyr::first(.data$n_countries),
      leaders = paste0(.data$Country, " (", sprintf("%.2f", .data$competitive_position_score), ")", collapse = "; "),
      leader_evidence = paste(unique(.data$stage_evidence_quality_cp), collapse = "/"),
      .groups = "drop"
    )
}

# Q3. Cells whose score rests mainly on shared, proxy or technology-wide evidence. `dimension` picks
# the evidence columns (cp, sn, pp).
sp_diag_weak_evidence <- function(tbl, dimension = "cp", max_specificity = 0.33) {
  spec <- paste0("evidence_specificity_", dimension)
  sp_diag_check(tbl, c("Country", "tech", "supply_chain", spec), "sp_diag_weak_evidence()")
  tbl %>%
    dplyr::filter(!is.na(.data[[spec]])) %>%
    dplyr::group_by(.data$tech, .data$supply_chain) %>%
    dplyr::summarise(
      n_cells = dplyr::n(),
      mean_specificity = mean(.data[[spec]]),
      share_weak = mean(.data[[spec]] < max_specificity),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$share_weak), .data$mean_specificity)
}

# Q5. What happens to each country's old Strategic Index top cells under the new dimensions.
# strategic_index_legacy must be on tbl. A legacy priority "disappears" when it is neither a
# demonstrated strength (position >= threshold) nor a high-need cell: what remains is policy (or
# the stage/technology priors).
sp_diag_legacy_priorities <- function(tbl, top_k = 5, threshold = 0.5) {
  sp_diag_check(tbl, c("Country", "tech", "supply_chain", "strategic_index_legacy",
                       "competitive_position_percentile", "strategic_need_percentile",
                       "policy_priority_percentile", "strategic_posture", "competitive_policy_alignment"),
                "sp_diag_legacy_priorities()")
  tbl %>%
    dplyr::filter(!is.na(.data$strategic_index_legacy)) %>%
    dplyr::group_by(.data$Country) %>%
    dplyr::arrange(dplyr::desc(.data$strategic_index_legacy), .by_group = TRUE) %>%
    dplyr::mutate(legacy_rank = dplyr::row_number()) %>%
    dplyr::filter(.data$legacy_rank <= top_k) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      survives_as = dplyr::case_when(
        !is.na(.data$competitive_position_percentile) & .data$competitive_position_percentile >= threshold ~ "demonstrated_strength",
        !is.na(.data$strategic_need_percentile) & .data$strategic_need_percentile >= threshold ~ "strategic_need_only",
        is.na(.data$competitive_position_percentile) ~ "position_unmeasured",
        TRUE ~ "disappears"
      )
    ) %>%
    dplyr::select(
      "Country", "legacy_rank", "tech", "supply_chain", "strategic_index_legacy",
      "competitive_position_percentile", "strategic_need_percentile", "policy_priority_percentile",
      "competitive_policy_alignment", "strategic_posture", "survives_as"
    )
}

# Q6. Latent advantages: demonstrated position well ahead of policy attention.
sp_diag_latent_advantages <- function(tbl, min_gap = 0.25) {
  sp_diag_check(tbl, c("Country", "tech", "supply_chain", "competitive_policy_alignment", "priority_gap"),
                "sp_diag_latent_advantages()")
  tbl %>%
    dplyr::filter(.data$competitive_policy_alignment == "latent_advantage",
                  !is.na(.data$priority_gap), .data$priority_gap >= min_gap) %>%
    dplyr::arrange(dplyr::desc(.data$priority_gap))
}

# Q7. Robust non-priorities: low on all three dimensions by a margin, with some stage-specific
# evidence behind the position (so a missing measure is not mistaken for a low one).
sp_diag_robust_low_priority <- function(tbl, ceiling = 0.4, min_specificity = 0.33) {
  sp_diag_check(tbl, c("competitive_position_percentile", "strategic_need_percentile",
                       "policy_priority_percentile", "evidence_specificity_cp"), "sp_diag_robust_low_priority()")
  tbl %>%
    dplyr::filter(
      .data$competitive_position_percentile < ceiling,
      .data$strategic_need_percentile < ceiling,
      .data$policy_priority_percentile < ceiling,
      .data$evidence_specificity_cp >= min_specificity
    )
}

# Q8, Q9, Q11, Q12. Within-country agreement between two policy measures over the country's cells:
# Spearman, and the share of cells where they disagree on high (>= 0.5) vs low.
sp_diag_policy_agreement <- function(tbl, x, y, label = paste(x, "vs", y)) {
  sp_diag_check(tbl, c("Country", x, y), "sp_diag_policy_agreement()")
  tbl %>%
    dplyr::group_by(.data$Country) %>%
    dplyr::summarise(
      comparison = label,
      n_cells = sum(!is.na(.data[[x]]) & !is.na(.data[[y]])),
      spearman = sp_spearman(.data[[x]], .data[[y]]),
      high_low_disagreement = {
        ok <- !is.na(.data[[x]]) & !is.na(.data[[y]])
        if (any(ok)) mean((.data[[x]][ok] >= 0.5) != (.data[[y]][ok] >= 0.5)) else NA_real_
      },
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$n_cells > 0)
}

# Q9. Which source carries each country's policy-priority signal.
sp_diag_policy_sources <- function(tbl) {
  sp_diag_check(tbl, c("Country", "policy_priority_sources", "policy_priority_percentile"), "sp_diag_policy_sources()")
  tbl %>%
    dplyr::filter(!is.na(.data$policy_priority_percentile)) %>%
    dplyr::count(.data$Country, .data$policy_priority_sources, name = "n_cells") %>%
    dplyr::group_by(.data$Country) %>%
    dplyr::mutate(share = .data$n_cells / sum(.data$n_cells)) %>%
    dplyr::ungroup()
}

# The priority-gap variants (percentile, z-score, rank) against each other, per country.
sp_diag_gap_variants <- function(tbl) {
  sp_diag_check(tbl, c("Country", "priority_gap", "priority_gap_z", "priority_gap_rank", "priority_gap_cross"),
                "sp_diag_gap_variants()")
  tbl %>%
    dplyr::group_by(.data$Country) %>%
    dplyr::summarise(
      n_cells = sum(!is.na(.data$priority_gap)),
      spearman_pct_z = sp_spearman(.data$priority_gap, .data$priority_gap_z),
      spearman_pct_rank = sp_spearman(.data$priority_gap, .data$priority_gap_rank),
      spearman_pct_cross = sp_spearman(.data$priority_gap, .data$priority_gap_cross),
      sign_agreement_z = {
        ok <- !is.na(.data$priority_gap) & !is.na(.data$priority_gap_z)
        if (any(ok)) mean(sign(.data$priority_gap[ok]) == sign(.data$priority_gap_z[ok])) else NA_real_
      },
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$n_cells > 0)
}

# Posture counts by country (the headline mix).
sp_diag_posture_mix <- function(tbl) {
  sp_diag_check(tbl, c("Country", "competitive_policy_alignment", "strategic_posture"), "sp_diag_posture_mix()")
  tbl %>%
    dplyr::count(.data$Country, .data$competitive_policy_alignment, .data$strategic_posture, name = "n_cells")
}

# ---- Strategic posture (Yards & Fences decision layer) ------------------------------------------------

# Share of classified cells in each posture, by any grouping (country, tech, stage).
sp_diag_posture_distribution <- function(tbl, by) {
  sp_diag_check(tbl, c(by, "strategic_posture"), "sp_diag_posture_distribution()")
  tbl %>%
    dplyr::filter(!is.na(.data$strategic_posture)) %>%
    dplyr::count(dplyr::across(dplyr::all_of(by)), .data$strategic_posture, name = "n_cells") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dplyr::mutate(share = .data$n_cells / sum(.data$n_cells)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = "strategic_posture", values_from = c("n_cells", "share"), values_fill = 0)
}

# Country divergence: for each technology x stage, the posture of each review country and the number
# of distinct postures among them (1 = they all agree).
sp_diag_country_divergence <- function(tbl, countries) {
  sp_diag_check(tbl, c("Country", "tech", "supply_chain", "strategic_posture", "compete_strategy"), "sp_diag_country_divergence()")
  tbl %>%
    dplyr::filter(.data$Country %in% countries, !is.na(.data$strategic_posture)) %>%
    dplyr::mutate(label = ifelse(is.na(.data$compete_strategy), .data$strategic_posture,
                                 paste0(.data$strategic_posture, ":", .data$compete_strategy))) %>%
    dplyr::group_by(.data$tech, .data$supply_chain) %>%
    dplyr::mutate(n_distinct_postures = dplyr::n_distinct(.data$strategic_posture)) %>%
    dplyr::ungroup() %>%
    dplyr::select("tech", "supply_chain", "n_distinct_postures", "Country", "label") %>%
    tidyr::pivot_wider(names_from = "Country", values_from = "label") %>%
    dplyr::arrange(dplyr::desc(.data$n_distinct_postures), .data$tech, .data$supply_chain)
}

# Layer precision: sensitive layers whose posture differs from their cell's.
sp_diag_layer_precision <- function(cells, layers) {
  sp_diag_check(layers, c("Country", "tech", "supply_chain", "layer_id", "strategic_posture"), "sp_diag_layer_precision()")
  cell_post <- cells %>% dplyr::select("Country", "tech", "supply_chain", cell_posture = "strategic_posture")
  layers %>%
    dplyr::filter(.data$layer_id != "stage") %>%
    dplyr::left_join(cell_post, by = c("Country", "tech", "supply_chain")) %>%
    dplyr::count(.data$tech, .data$supply_chain, .data$layer_id, layer_posture = .data$strategic_posture, .data$cell_posture, name = "n_countries")
}

# Threshold sensitivity: the share of classified nodes whose posture changes when one rule moves.
# `perturb` is a named list of functions cfg -> cfg.
sp_diag_threshold_sensitivity <- function(inputs, cfg, perturb, classify = sp_strategic_posture) {
  base <- classify(inputs, cfg)$strategic_posture
  ok <- !is.na(base)
  do.call(rbind, lapply(names(perturb), function(nm) {
    alt <- classify(inputs, perturb[[nm]](cfg))$strategic_posture
    ch <- ok & alt != base
    data.frame(
      perturbation = nm,
      n_nodes = sum(ok),
      share_changed = mean(ch[ok]),
      to_secure = sum(ch & alt == "secure"), to_compete = sum(ch & alt == "compete"), to_deploy = sum(ch & alt == "deploy"),
      stringsAsFactors = FALSE
    )
  }))
}

# Policy misalignment cases: under-prioritized Secure/Compete and over-prioritized Deploy.
sp_diag_policy_misalignment <- function(tbl) {
  sp_diag_check(tbl, c("Country", "tech", "supply_chain", "strategic_posture", "policy_alignment",
                       "policy_priority_percentile"), "sp_diag_policy_misalignment()")
  tbl %>%
    dplyr::filter(.data$policy_alignment %in% c("under_prioritized", "over_prioritized", "mixed")) %>%
    dplyr::arrange(.data$policy_alignment, dplyr::desc(abs(.data$policy_priority_percentile - 0.5)))
}
