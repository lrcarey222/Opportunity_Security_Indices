# Dashboard L1: Energy Security (ES) and Economic Opportunity (EO) pillars recomputed from the
# stored category scores for any category weights and rankability threshold (pure; no IO).
#
# Exact port of compute_index_and_contributions(), summarize_index_imputation() and
# flag_rankable_cells() (R/indices/index_builder_core.R) for the headline cells:
#
#   w_k             category weight; NA when the category is unweighted or scores nowhere in the
#                   pillar's full (all-technology) build (categories_in_data)
#   observed_k      the cell has a category row with a score and w_k is not NA
#   weight_sum      sum of w_k over the observed categories; the cell is scored when > 0
#   category_weight w_k / weight_sum (0 for an unobserved category; NA in an unscored cell)
#   contribution    category_score * category_weight (0 when the weight is 0)
#   value           sum of the contributions (NA in an unscored cell)
#   IWS             sum_k cw_k cw_imp_k / sum_k cw_k cw_tot_k, with cw_imp / cw_tot the summed
#                   within-category weights of the imputed / all variable rows
#   rankable        scored, sovereign and IWS <= tau (+1e-9); tau NA = the rule is off (a pillar
#                   build that predates it): every scored cell is rankable
#
# Sums run over the categories in C-locale order with rowSums() (long-double accumulation, like
# the pipeline's sum()), so the values match the pipeline's to the last bit.
#
# bundle$pillars$<es|eo> (written by scripts/85):
#   cells      tibble(Country, tech, supply_chain, iso3c, sovereign): one row per headline cell
#   categories chr, C-locale sorted; score, cw_imp, cw_tot (double) and present, imputed (logical)
#              matrices [cell x category]; missing_rule (character matrix, NA = none)
#   n_var      integer per cell: variable rows behind the cell (0 = no imputation record)
#   categories_in_data chr; allow_partial_categories lgl
#   var        tibble(cell, category, score_variable, variable, component_value, cwwo, imputed,
#              missing_rule_applied): the variable decomposition rows

DASH_PILLARS <- c(es = "Energy_Security_Index", eo = "Economic_Opportunity_Index")

dash_pillar_one <- function(pb, weights, tau, pillar) {
  cats <- pb$categories
  n <- nrow(pb$cells)
  w <- unname(weights[cats])
  w[!cats %in% pb$categories_in_data] <- NA_real_
  W <- matrix(w, nrow = n, ncol = length(cats), byrow = TRUE)
  obs <- pb$present & !is.na(pb$score) & !is.na(W)
  weight_sum <- rowSums(ifelse(obs, W, 0))
  scored <- weight_sum > 0
  cw <- ifelse(obs, W / weight_sum, 0)
  cw[!scored, ] <- NA_real_
  contrib <- ifelse(is.na(cw), NA_real_, ifelse(cw == 0, 0, pb$score * cw))
  contrib0 <- contrib
  contrib0[!pb$present] <- 0
  value <- rowSums(contrib0)
  value[!scored] <- NA_real_
  if (!isTRUE(pb$allow_partial_categories)) {
    expected <- intersect(cats[!is.na(w)], pb$categories_in_data)
    complete <- rowSums(obs[, cats %in% expected, drop = FALSE]) == length(expected)
    value[!complete] <- NA_real_
  }

  # Imputed weight share
  cwz <- cw
  cwz[!pb$present] <- 0
  num <- rowSums(cwz * pb$cw_imp)
  den <- rowSums(cwz * pb$cw_tot)
  iws <- num / den
  iws[!scored | pb$n_var == 0] <- NA_real_

  # Rankability
  if (is.na(tau)) {
    rankable <- scored & !is.na(value)
    reason <- ifelse(rankable, NA_character_, "not scored")
  } else {
    sovereign <- pb$cells$sovereign %in% TRUE
    share_known <- !is.na(iws)
    over <- share_known & iws > tau + 1e-9
    has_value <- !is.na(value)
    reason <- ifelse(
      !has_value, "not scored",
      ifelse(!sovereign & over, paste0("not a sovereign economy and imputed weight share above ", format(tau)),
        ifelse(!sovereign, "not a sovereign economy",
          ifelse(!share_known, "imputed weight share unknown",
            ifelse(over, paste0("imputed weight share above ", format(tau)), NA_character_))))
    )
    rankable <- is.na(reason)
  }

  index <- tibble::tibble(
    Country = pb$cells$Country,
    iso3c = pb$cells$iso3c,
    tech = pb$cells$tech,
    supply_chain = pb$cells$supply_chain,
    sector = paste(pb$cells$tech, pb$cells$supply_chain, sep = " - "),
    value = value,
    imputed_weight_share = iws,
    rankable = rankable,
    rank_exclusion = reason
  )
  list(index = index, category_weight = cw, contribution = contrib, weight_sum = weight_sum)
}

# Long contribution table of one pillar (rows that exist in the category table only).
dash_pillar_contrib_long <- function(pb, one) {
  idx <- which(pb$present, arr.ind = TRUE)
  idx <- idx[order(idx[, 1], idx[, 2]), , drop = FALSE]
  tibble::tibble(
    Country = pb$cells$Country[idx[, 1]],
    tech = pb$cells$tech[idx[, 1]],
    supply_chain = pb$cells$supply_chain[idx[, 1]],
    category = pb$categories[idx[, 2]],
    category_score = pb$score[idx],
    category_weight = one$category_weight[idx],
    contribution = one$contribution[idx],
    imputed = pb$imputed[idx],
    missing_rule_applied = pb$missing_rule[idx]
  )
}

# L1: both pillars for the scenario. Returns es and eo (index tables), es_contrib and eo_contrib
# (long category decompositions) and, for the variable drill-down, the category-weight matrices.
dash_pillars <- function(bundle, params, contributions = TRUE) {
  pp <- params$pillars
  tau <- if (is.null(pp$tau)) NA_real_ else pp$tau
  es <- dash_pillar_one(bundle$pillars$es, pp$es_weights, tau, "es")
  eo <- dash_pillar_one(bundle$pillars$eo, pp$eo_weights, tau, "eo")
  out <- list(
    es = es$index,
    eo = eo$index,
    es_category_weight = es$category_weight,
    eo_category_weight = eo$category_weight
  )
  if (isTRUE(contributions)) {
    out$es_contrib <- dash_pillar_contrib_long(bundle$pillars$es, es)
    out$eo_contrib <- dash_pillar_contrib_long(bundle$pillars$eo, eo)
  }
  out
}

# Variable rows of one pillar for a country (optionally one cell), with the scenario category
# weights: weighted_variable_contribution = component_value * cwwo * category_weight (0 when any
# factor is 0 or the value is missing; NA in an unscored cell), as compute_index_and_contributions()
# writes it. Sums to the cell's pillar value.
dash_variable_contrib <- function(bundle, pillars, pillar = c("es", "eo"), country, tech = NULL, stage = NULL) {
  pillar <- match.arg(pillar)
  pb <- bundle$pillars[[pillar]]
  cw <- pillars[[paste0(pillar, "_category_weight")]]
  keep_cell <- pb$cells$Country %in% country
  if (!is.null(tech)) keep_cell <- keep_cell & pb$cells$tech %in% tech
  if (!is.null(stage)) keep_cell <- keep_cell & pb$cells$supply_chain %in% stage
  cells <- which(keep_cell)
  v <- pb$var[pb$var$cell %in% cells, , drop = FALSE]
  k <- match(v$category, pb$categories)
  catw <- cw[cbind(v$cell, k)]
  wvc <- ifelse(
    is.na(catw), NA_real_,
    ifelse(is.na(v$component_value) | v$cwwo %in% 0 | catw == 0, 0, v$component_value * v$cwwo * catw)
  )
  tibble::tibble(
    Country = pb$cells$Country[v$cell],
    tech = pb$cells$tech[v$cell],
    supply_chain = pb$cells$supply_chain[v$cell],
    category = v$category,
    score_variable = v$score_variable,
    variable = v$variable,
    component_value = v$component_value,
    component_weight_within_overall = v$cwwo,
    category_weight = catw,
    weighted_variable_contribution = wvc,
    imputed = v$imputed,
    missing_rule_applied = v$missing_rule_applied
  )
}
