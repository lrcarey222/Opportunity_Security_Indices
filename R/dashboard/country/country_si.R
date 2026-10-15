# The Strategic Index of one economy's sectors (spec_country.md A9.3-A9.4): the five level terms, rank,
# margin to the next sector, the priority score without the policy term, policy activity ranks, thirds
# and the agreement classes. Pure.
#
# si_c: the economy's strategic_index rows with columns key, eo_level (Economic_Opportunity_Index),
# need (energy_security_need), pol, sc_weight, tech_weight, si (strategic_index, NA outside the
# sample), si_exclusion, resource_base, pick_exclusion. w: named weights c(pol, eo, need, sc, tech)
# (STRATEGIC_INDEX_WEIGHTS: 0.2, 0.3, 0.3, 0.1, 0.1).

COUNTRY_SI_WEIGHT_NAMES <- c("pol", "eo", "need", "sc", "tech")

# Rank descending; ties by average rank (tie_avg = TRUE) or by the Sector cell order.
country_rank_desc <- function(x, cell_order, tie_avg = FALSE) {
  if (tie_avg) return(rank(-x, ties.method = "average", na.last = "keep"))
  r <- rep(NA_real_, length(x))
  ok <- is.finite(x)
  o <- order(-x[ok], cell_order[ok])
  r[which(ok)[o]] <- seq_along(o)
  r
}

# SI facts per cell key (list keyed by cell key) and the in-sample order.
country_si <- function(si_c, w, cells = country_cells_tbl(), near_tie = 0.01) {
  stopifnot(all(COUNTRY_SI_WEIGHT_NAMES %in% names(w)))
  si_c <- si_c[match(cells$key, si_c$key), , drop = FALSE]
  si_c$key <- cells$key
  ins <- is.finite(si_c$si)
  n <- sum(ins)
  rank <- country_rank_desc(ifelse(ins, si_c$si, NA), cells$order)
  ord <- order(rank, na.last = NA)
  si_order <- cells$key[ord]
  margin <- rep(NA_real_, nrow(si_c))
  if (length(ord) > 1) margin[ord[-length(ord)]] <- si_c$si[ord[-length(ord)]] - si_c$si[ord[-1]]
  parts_v <- cbind(eo = w[["eo"]] * si_c$eo_level, need = w[["need"]] * si_c$need, pol = w[["pol"]] * si_c$pol,
                   stage = w[["sc"]] * si_c$sc_weight, tech = w[["tech"]] * si_c$tech_weight)
  facts <- lapply(seq_len(nrow(si_c)), function(i) {
    if (!ins[i]) {
      return(list(v = NULL, d = COUNTRY_DASH, in_sample = FALSE, rank = list(v = NULL, d = COUNTRY_DASH), n = country_fv(n, "count"),
                  exclusion = country_chr_or_null(si_c$si_exclusion[i]), margin_next = list(v = NULL, d = COUNTRY_DASH), top5 = FALSE,
                  parts = NULL, pick = FALSE, resource_base = if (is.na(si_c$resource_base[i])) NULL else isTRUE(si_c$resource_base[i]),
                  pick_exclusion = country_chr_or_null(si_c$pick_exclusion[i])))
    }
    p <- parts_v[i, ]
    list(
      v = si_c$si[i], d = country_fmt(si_c$si[i], "index"), in_sample = TRUE,
      rank = country_fv(rank[i], "rank"), n = country_fv(n, "count"), exclusion = NULL,
      margin_next = if (is.finite(margin[i])) list(v = round(margin[i], 6), d = country_fmt(margin[i], "index3")) else list(v = NULL, d = COUNTRY_DASH),
      top5 = rank[i] <= 5,
      parts = lapply(as.list(p), function(v) country_fv(round(v, 10), "index3")),
      pick = FALSE,
      resource_base = if (is.na(si_c$resource_base[i])) NULL else isTRUE(si_c$resource_base[i]),
      pick_exclusion = country_chr_or_null(si_c$pick_exclusion[i])
    )
  })
  names(facts) <- cells$key
  top <- if (length(ord)) {
    k1 <- ord[1]
    k2 <- if (length(ord) > 1) ord[2] else NA
    list(cell = cells$key[k1], label = cells$label[k1], si = country_fv(si_c$si[k1], "index"),
         margin = if (is.finite(margin[k1])) list(v = round(margin[k1], 6), d = country_fmt(margin[k1], "index3")) else list(v = NULL, d = COUNTRY_DASH),
         `next` = if (is.na(k2)) NULL else cells$key[k2], next_label = if (is.na(k2)) NULL else cells$label[k2],
         near_tie = is.finite(margin[k1]) && margin[k1] < near_tie)
  } else NULL
  list(facts = facts, si_order = si_order, n = n, top = top, parts = parts_v, rank = rank, ins = ins, pol = si_c$pol, si = si_c$si)
}

# Priority without the policy term and the policy-activity comparison (A9.3-A9.4).
# si: country_si() output; pol_index: the Policy index value per cell key (NA where none); records:
# per-cell PAMS counts (data frame key, records_specific, records_tech, records_all, latest_year).
country_priority_policy <- function(si, pol_index, records, cells = country_cells_tbl()) {
  ins <- si$ins
  n <- sum(ins)
  prio_v <- ifelse(ins, si$si - si$parts[, "pol"], NA_real_)
  # ranked among the in-sample sectors only (1..n): out-of-sample sectors stay NA rather than
  # taking the top ranks
  prio_rank <- country_rank_desc(ifelse(ins, prio_v, NA_real_), cells$order, tie_avg = TRUE)
  pol_rank <- country_rank_desc(ifelse(ins, si$pol, NA_real_), cells$order, tie_avg = TRUE)
  variation <- n > 1 && length(unique(si$pol[ins])) > 1
  prio_third <- country_thirds(prio_rank, n)
  pol_third <- if (variation) country_thirds(pol_rank, n) else rep(NA_character_, length(ins))
  cls <- if (variation) country_agreement_class(prio_third, pol_third) else rep(NA_character_, length(ins))
  rec <- records[match(cells$key, records$key), , drop = FALSE]
  cnt <- function(v) country_fv(if (is.na(v)) 0 else v, "count")
  priority <- lapply(seq_along(ins), function(i) {
    if (!ins[i]) return(NULL)
    list(v = country_fv(prio_v[i], "index", digits = 6), rank = country_fv(prio_rank[i], "rank"), third = prio_third[i])
  })
  policy <- lapply(seq_along(ins), function(i) {
    pv <- pol_index[[cells$key[i]]]
    if (is.null(pv)) pv <- NA_real_
    list(
      index = country_fv(pv, "index"),
      pol = country_fv(if (ins[i]) si$pol[i] else NA, "index"),
      rank = country_fv(if (ins[i]) pol_rank[i] else NA, "rank"),
      third = if (ins[i] && variation) pol_third[i] else NULL,
      class = if (ins[i] && variation) cls[i] else NULL,
      records_specific = cnt(rec$records_specific[i]),
      records_tech = cnt(rec$records_tech[i]),
      records_all = cnt(rec$records_all[i]),
      latest_year = country_fv(rec$latest_year[i], "year")
    )
  })
  names(priority) <- names(policy) <- cells$key
  by_prio <- order(prio_rank, cells$order, na.last = NA)
  keys_by_prio <- cells$key[by_prio]
  matrix <- list()
  for (p in c("high", "middle", "low")) for (q in c("high", "middle", "low")) {
    matrix[[paste0(p, "_", q)]] <- country_arr(if (variation) keys_by_prio[prio_third[by_prio] == p & pol_third[by_prio] == q] else character())
  }
  by_class <- list()
  for (k in c("aligned_high", "gap", "policy_ahead", "aligned_low", "partial")) {
    by_class[[k]] <- country_arr(if (variation) keys_by_prio[cls[by_prio] %in% k] else character())
  }
  agreement <- if (n > 0) list(variation = variation, n = n, k = round(n / 3), matrix = matrix, by_class = by_class) else NULL
  list(priority = priority, policy = policy, agreement = agreement)
}
