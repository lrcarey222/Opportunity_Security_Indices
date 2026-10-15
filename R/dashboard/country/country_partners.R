# Partners of one economy (spec_country.md A9.7): the Partnership Strength Index from the economy's own
# viewpoint (psi_dyad_tbl, reporter = the economy) beside its ties today. Pure.
#
# psi_c: psi_dyad rows of the reporter: partner_iso, friendshore, opportunity, development, psi,
# rankable, rank_exclusion, rank. best5: data frame partner_iso, side ("fs" | "po"), cells (cell keys
# joined by ";", highest first). ties: country_partner_trade()$ties. party_fn(iso3) -> list(name,
# group, tier, income_group).

country_partner_pct <- function(x, ok) {
  vals <- x[ok & is.finite(x)]
  vapply(x, function(v) if (!is.finite(v) || !length(vals)) NA_real_ else (sum(vals < v) + 0.5 * sum(vals == v)) / length(vals), 1)
}

# The main reason per partner: the component (fs, po, dp present) with the highest percentile among
# the reporter's rankable partners.
country_partner_reason <- function(psi_c) {
  ok <- psi_c$rankable %in% TRUE
  p <- cbind(fs = country_partner_pct(psi_c$friendshore, ok), po = country_partner_pct(psi_c$opportunity, ok),
             dp = country_partner_pct(psi_c$development, ok))
  apply(p, 1, function(r) {
    if (all(is.na(r))) return(NA_character_)
    names(r)[which.max(replace(r, is.na(r), -Inf))]
  })
}

country_partner_rows <- function(iso3, psi_c, best5, ties, fdi, agreements, align, party_fn, words, cfg, matrix_partners = character()) {
  n_top <- cfg$n_top_partners %||% 15
  n_list <- cfg$n_top_lists %||% 10
  n_order <- cfg$n_order %||% 25
  psi_c <- psi_c[psi_c$partner_iso != iso3, , drop = FALSE]
  psi_c$reason <- country_partner_reason(psi_c)
  ranked <- psi_c[psi_c$rankable %in% TRUE & is.finite(psi_c$rank), , drop = FALSE]
  ranked <- ranked[order(ranked$rank), , drop = FALSE]
  psi_order <- utils::head(ranked$partner_iso, n_order)
  ties <- ties[ties$partner != iso3, , drop = FALSE]
  tie_sorted <- ties[order(ties$tie_rank), , drop = FALSE]
  tie_order <- utils::head(tie_sorted$partner, n_order)
  keep_psi <- utils::head(ranked$partner_iso, n_top)
  keep_tie <- utils::head(tie_sorted$partner[is.finite(tie_sorted$tie) & tie_sorted$tie > 0], n_list)
  keep <- unique(c(keep_psi, keep_tie, matrix_partners))
  keep <- setdiff(keep, iso3)
  b5 <- function(j, side) {
    x <- best5$cells[best5$partner_iso == j & best5$side == side]
    if (!length(x) || is.na(x[1]) || !nzchar(x[1])) character() else strsplit(x[1], ";", fixed = TRUE)[[1]]
  }
  rows <- lapply(keep, function(j) {
    r <- psi_c[psi_c$partner_iso == j, , drop = FALSE]
    t <- ties[ties$partner == j, , drop = FALSE]
    p <- party_fn(j)
    has <- nrow(r) > 0
    rankable <- has && isTRUE(r$rankable[1]) && is.finite(r$rank[1])
    rank <- if (rankable) r$rank[1] else NA
    tie_rank <- if (nrow(t)) t$tie_rank[1] else NA
    dp_missing <- if (identical(p$income_group, "High income")) "not scored (high-income partner)" else "not scored (missing data)"
    al <- align[align$partner == j, , drop = FALSE]
    a <- if (nrow(al)) al$alignment[1] else NA
    in_psi <- isTRUE(rank <= n_list)
    in_tie <- isTRUE(tie_rank <= n_list)
    no_trade <- !nrow(t) || !is.finite(t$tie[1]) || t$tie[1] <= 0
    mt <- if (in_psi && in_tie) "priority_strong_ties"
             else if (in_psi && (no_trade || isTRUE(tie_rank > n_order))) "priority_weak_ties"
             else if (in_tie && (!rankable || isTRUE(rank > n_order))) "ties_lower_priority"
             else NULL
    reason <- if (has) country_chr_or_null(r$reason[1]) else NULL
    listed <- c(if (j %in% keep_psi) "psi", if (j %in% keep_tie) "tie", if (j %in% matrix_partners) "matrix")
    list(
      iso3 = j, name = p$name, tier = p$tier, group = p$group, income_group = country_chr_or_null(p$income_group),
      rank = country_fv(rank, "rank"), rankable = rankable,
      rank_reason = if (rankable) NULL else if (has) country_chr_or_null(paste0("not ranked: ", r$rank_exclusion[1] %||% "no score")) else "no Partnership Strength score",
      psi = country_fv(if (has) r$psi[1] else NA, "index"),
      fs = country_fv(if (has) r$friendshore[1] else NA, "index"),
      po = country_fv(if (has) r$opportunity[1] else NA, "index"),
      dp = country_fv(if (has) r$development[1] else NA, "index", missing = if (has) dp_missing else COUNTRY_DASH),
      reason = reason, reason_d = if (is.null(reason)) NULL else words$reason[[reason]],
      fs_best5 = country_arr(b5(j, "fs")), po_best5 = country_arr(b5(j, "po")),
      ties = list(
        imp_mean = country_fv(if (nrow(t)) round(t$imp_mean[1], 6) else 0, "share"),
        exp_mean = country_fv(if (nrow(t)) round(t$exp_mean[1], 6) else 0, "share"),
        tie = country_fv(if (nrow(t)) round(t$tie[1], 6) else 0, "share"),
        tie_rank = country_fv(tie_rank, "rank"),
        top3_import_cells = country_fv(if (nrow(t)) t$top3_import_cells[1] else 0L, "count"),
        n_import_cells = country_fv(if (nrow(t)) t$n_import_cells[1] else 0L, "count"),
        fdi_in = country_fv(fdi$partner_in(j), "count"),
        fdi_in_china_parent = j %in% COUNTRY_CHINA_HOMES,
        fdi_out = country_fv(fdi$partner_out(j), "count"),
        agreements = country_arr(country_agreements_shared(agreements, iso3, j)),
        alignment = country_fv(a, "index"),
        alignment_d = country_alignment_words(a, words$alignment),
        bloc = if (nrow(al)) country_chr_or_null(al$bloc[1]) else NULL,
        alignment_basis = if (nrow(al)) country_chr_or_null(al$basis[1]) else NULL
      ),
      in_psi_top = in_psi, in_tie_top = in_tie, match = mt,
      listed_for = country_arr(listed)
    )
  })
  # rows in PSI rank order, then the others by tie rank
  rk <- vapply(rows, function(r) r$rank$v %||% Inf, 1)
  tr <- vapply(rows, function(r) r$ties$tie_rank$v %||% Inf, 1)
  rows <- rows[order(rk, tr)]
  top <- if (nrow(ranked)) {
    j <- ranked$partner_iso[1]
    list(iso3 = j, name = party_fn(j)$name, psi = country_fv(ranked$psi[1], "index"), n_ranked = country_fv(nrow(ranked), "count"))
  } else NULL
  list(
    n_scored = country_fv(sum(is.finite(psi_c$psi)), "count"), n_ranked = country_fv(nrow(ranked), "count"),
    psi_order = country_arr(psi_order), tie_order = country_arr(tie_order), rows = rows, psi_1 = top
  )
}

# Best-five cells per (reporter, partner) from a dyad x cell index table (A9.7): the five highest
# values (ties by the Sector cell order) and their mean. cell_tbl: reporter_iso, partner_iso, key, value.
country_best5 <- function(cell_tbl, side, cells = country_cells_tbl()) {
  if (!nrow(cell_tbl)) return(data.frame(reporter_iso = character(), partner_iso = character(), side = character(), cells = character(), mean = numeric()))
  o <- order(cell_tbl$reporter_iso, cell_tbl$partner_iso, -cell_tbl$value, match(cell_tbl$key, cells$key))
  d <- cell_tbl[o, , drop = FALSE]
  g <- paste(d$reporter_iso, d$partner_iso, sep = "\r")
  pos <- stats::ave(seq_along(g), g, FUN = seq_along)
  d <- d[pos <= 5, , drop = FALSE]
  g <- g[pos <= 5]
  cells_s <- tapply(d$key, g, paste, collapse = ";")
  mean_v <- tapply(d$value, g, mean)
  first <- !duplicated(g)
  data.frame(reporter_iso = d$reporter_iso[first], partner_iso = d$partner_iso[first], side = side,
             cells = as.character(cells_s[g[first]]), mean = as.numeric(mean_v[g[first]]), stringsAsFactors = FALSE)
}
