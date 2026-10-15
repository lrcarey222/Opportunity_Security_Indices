# The R mirror of the TypeScript runtime checks of dashboard/src/country/contract.ts
# (checkFactSheet(), checkCountryManifest()): shapes, Fv values, enums, cell keys, quadrants re-derived
# from the plotted positions and the country's median sector (A9.2), SI parts summing to the SI, summary counts and order lists, partner
# ranks. Returns problem strings (empty when the sheet is valid). Pure. Works on the sheet as built in R
# or as read back with jsonlite::fromJSON(simplifyVector = FALSE).

COUNTRY_QUADRANT_IDS <- c("grow", "upgrade", "secure", "watch")
# gzip budgets of the export (spec A10; the same numbers as COUNTRY_BUDGET_GZ in dashboard/src/country/contract.ts
# and dashboard/scripts/check-budget.mjs). manifest: 15 kB until 2026-10-14, raised to 20 kB when the 31 memo
# entries (2.8 kB gz) took the published manifest to 16.9 kB.
COUNTRY_BUDGET_GZ <- c(manifest = 20480, sheet = 40960, sheet_target = 25600)
COUNTRY_CLASSES <- c("aligned_high", "gap", "policy_ahead", "aligned_low", "partial")
COUNTRY_CAVEAT_IDS <- c("relative_scores", "baseline", "rankability", "imputed_driver", "policy_pams_only", "no_si", "near_tie", "eu_node",
                        "inherited_alignment", "assumed_zero_production", "cmfdi_coverage", "trade_mirror", "few_economies", "cat_eu_bloc",
                        "intra_eu", "hkg_reexport", "pick_stability", "ownership_parent", "eu_policy_records", "nipo_context",
                        "eu_region_share")

country_is_fv <- function(x) {
  is.list(x) && "d" %in% names(x) && is.character(x$d) && length(x$d) == 1 &&
    (is.null(x$v) || (is.numeric(x$v) && length(x$v) == 1 && is.finite(x$v) && nzchar(x$d)))
}

country_check_sheet <- function(s) {
  p <- character()
  add <- function(...) p <<- c(p, paste0(...))
  fv <- function(x, path, min = NULL, max = NULL, int = FALSE) {
    if (!country_is_fv(x)) return(add(path, ": not an Fv {v, d}"))
    if (is.null(x$v)) return(invisible())
    if (!is.null(min) && x$v < min - 1e-9) add(path, ": ", x$v, " below ", min)
    if (!is.null(max) && x$v > max + 1e-9) add(path, ": ", x$v, " above ", max)
    if (int && x$v != round(x$v)) add(path, ": ", x$v, " is not an integer")
  }
  if (!identical(s$schema, "osi-country-facts")) add("schema: expected 'osi-country-facts'")
  m <- s$meta
  if (!is.list(m)) return(c(p, "meta: not an object"))
  if (!m$tier %in% c("core", "partner", "none")) add("meta.tier: '", m$tier, "'")
  if (!m$status %in% c("ok", "non_rankable", "no_data")) add("meta.status: '", m$status, "'")
  fv(m$rank_threshold, "meta.rank_threshold")
  fv(m$alignment_s, "meta.alignment_s")
  keys <- vapply(s$cells, function(c) c$key, "")
  all_keys <- country_cells_tbl()$key
  if (any(!keys %in% all_keys)) add("cells: unknown keys ", paste(setdiff(keys, all_keys), collapse = ", "))
  if (anyDuplicated(keys)) add("cells: duplicate keys")
  by <- stats::setNames(s$cells, keys)
  cmed <- country_median_sector(s$cells)
  for (c in s$cells) {
    at <- paste0("cells.", c$key)
    for (pl in c("es", "eo")) {
      x <- c[[pl]]
      if (is.null(x)) next
      fv(x, paste0(at, ".", pl), 0, 1)
      fv(x$pct, paste0(at, ".", pl, ".pct"), 0, 100)
      fv(x$med, paste0(at, ".", pl, ".med"), 0, 1)
      fv(x$n, paste0(at, ".", pl, ".n"), 0, int = TRUE)
      fv(x$order, paste0(at, ".", pl, ".order"), 1, int = TRUE)
    }
    if (c$key %in% COUNTRY_NO_ES_CELLS && !is.null(c$es)) add(at, ".es: the paper has no Energy Security score here")
    if (isTRUE(c$placed)) {
      if (is.null(c$es) || is.null(c$eo)) add(at, ": placed without both scores")
      else {
        if (!isTRUE(c$es$rankable) || !isTRUE(c$eo$rankable)) add(at, ": placed but a score is not rankable")
        if (is.null(c$es$pos) || is.null(c$eo$pos)) add(at, ": placed without a position on both axes")
        else {
          q <- country_quadrant(c$es$pos, c$eo$pos, cmed$es, cmed$eo)
          if (!identical(c$quadrant, q)) add(at, ".quadrant: '", c$quadrant %||% "null", "' but the positions and the country's median sector give '", q, "'")
        }
        for (pl in c("es", "eo")) {
          x <- c[[pl]]
          if (!is.null(x$pos) && ((x$pos >= 50) != (x$v >= x$med$v))) add(at, ".", pl, ".pos: on the wrong side of 50")
        }
      }
    } else if (!is.null(c$quadrant)) add(at, ".quadrant: set on a cell that is not placed")
    si <- c$si
    if (isTRUE(si$in_sample)) {
      if (is.null(si$v)) add(at, ".si: in sample without a value")
      else {
        sm <- sum(vapply(si$parts, function(x) x$v %||% 0, 1))
        if (abs(sm - si$v) > 1e-9) add(at, ".si.parts: sum ", sm, " differs from ", si$v)
      }
      fv(si$rank, paste0(at, ".si.rank"), 1, int = TRUE)
    } else if (!is.null(si$v)) add(at, ".si: a value outside the sample")
    if (!is.null(c$policy$class) && !c$policy$class %in% COUNTRY_CLASSES) add(at, ".policy.class: '", c$policy$class, "'")
    if (!is.null(c$trade) && isTRUE(c$trade$has_basket)) {
      for (side in c("sources", "markets")) {
        tot <- sum(vapply(c$trade[[side]], function(r) r$share$v %||% 0, 1))
        if (tot > 1 + 1e-9) add(at, ".trade.", side, ": shares sum to ", tot)
      }
    }
    for (pl in c("es", "eo")) {
      d <- c$drivers[[pl]]
      if (length(d)) {
        w <- sum(vapply(d, function(x) x$weight$v %||% 0, 1))
        if (abs(w - 1) > 1e-6) add(at, ".drivers.", pl, ": weights sum to ", w)
        for (x in d) if (isTRUE(x$top) && isTRUE(x$constant)) add(at, ".drivers.", pl, ".", x$key, ": constant but top")
      }
    }
  }
  sm <- s$summary
  placed <- Filter(function(c) isTRUE(c$placed), s$cells)
  if (!identical(sm$n_placed$v, as.numeric(length(placed))) && !identical(sm$n_placed$v, length(placed))) add("summary.n_placed: ", sm$n_placed$v, " but the cells give ", length(placed))
  for (pl in c("es", "eo")) {
    x <- sm$country_median[[pl]]
    if (!country_is_fv(x)) add("summary.country_median.", pl, ": not an Fv {v, d}")
    else if (!identical(is.null(x$v), !is.finite(cmed[[pl]])) || (!is.null(x$v) && abs(x$v - cmed[[pl]]) > 1e-9)) {
      add("summary.country_median.", pl, ": ", x$v %||% "null", " but the placed cells give ", cmed[[pl]])
    }
  }
  for (q in COUNTRY_QUADRANT_IDS) {
    n <- sum(vapply(placed, function(c) identical(c$quadrant, q), TRUE))
    if ((sm$quadrants[[q]]$v %||% -1) != n) add("summary.quadrants.", q, ": ", sm$quadrants[[q]]$v, " but the cells give ", n)
  }
  pk <- vapply(placed, function(c) c$key, "")
  for (nm in c("es_order", "eo_order")) {
    o <- unlist(sm[[nm]])
    if (!setequal(o, pk) || length(o) != length(pk)) add("summary.", nm, ": not the placed cells")
    pl <- substr(nm, 1, 2)
    for (i in seq_along(o)) if ((by[[o[i]]][[pl]]$order$v %||% -1) != i) add("cells.", o[i], ".", pl, ".order: not position ", i)
    v <- vapply(o, function(k) by[[k]][[pl]]$pct$v %||% NA_real_, 1)
    dirn <- if (pl == "es") 1 else -1
    if (length(v) > 1 && any(dirn * diff(v) < -1e-9, na.rm = TRUE)) add("summary.", nm, ": out of order")
  }
  sio <- unlist(sm$si_order)
  ins <- vapply(s$cells, function(c) isTRUE(c$si$in_sample), TRUE)
  if (length(sio) != sum(ins)) add("summary.si_order: not the in-sample cells")
  # priority and policy ranks lie within the in-sample sectors (1..n), thirds and classes follow from them, and
  # the 3 x 3 matrix and the classes hold each in-sample sector once (2026-10-14: the rank-offset bug)
  ins_keys <- keys[ins]
  n_in <- length(ins_keys)
  ag <- s$policy$agreement
  var_ok <- isTRUE(ag$variation)
  for (c in s$cells) {
    at <- paste0("cells.", c$key)
    if (c$key %in% ins_keys) {
      pr <- c$priority$rank$v
      po <- c$policy$rank$v
      if (is.null(pr) || pr < 1 || pr > n_in) {
        add(at, ".priority.rank: ", pr %||% "null", " outside 1..", n_in)
        next
      }
      if (!identical(c$priority$third, country_thirds(pr, n_in))) add(at, ".priority.third: '", c$priority$third %||% "null", "' but rank ", pr, " of ", n_in, " gives '", country_thirds(pr, n_in), "'")
      if (!is.null(po) && (po < 1 || po > n_in)) add(at, ".policy.rank: ", po, " outside 1..", n_in)
      else if (var_ok && !is.null(po)) {
        pt <- country_thirds(po, n_in)
        if (!identical(c$policy$third, pt)) add(at, ".policy.third: '", c$policy$third %||% "null", "' but rank ", po, " of ", n_in, " gives '", pt, "'")
        cl <- country_agreement_class(country_thirds(pr, n_in), pt)
        if (!identical(c$policy$class, cl)) add(at, ".policy.class: '", c$policy$class %||% "null", "' but the thirds give '", cl, "'")
      }
    } else {
      if (!is.null(c$priority)) add(at, ".priority: set outside the Strategic Index sample")
      if (!is.null(c$policy$rank$v)) add(at, ".policy.rank: set outside the Strategic Index sample")
    }
  }
  if (var_ok) {
    mk <- unlist(ag$matrix)
    if (length(mk) != n_in || !setequal(mk, ins_keys)) add("policy.agreement.matrix: ", length(mk), " entries, not the ", n_in, " in-sample sectors once each")
    for (nm in names(ag$matrix)) {
      pq <- strsplit(nm, "_", fixed = TRUE)[[1]]
      for (k in unlist(ag$matrix[[nm]])) if (!identical(by[[k]]$priority$third, pq[1]) || !identical(by[[k]]$policy$third, pq[2])) add("policy.agreement.matrix.", nm, ": ", k, " has other thirds")
    }
    bk <- unlist(ag$by_class)
    if (length(bk) != n_in) add("policy.agreement.by_class: ", length(bk), " entries, not the ", n_in, " in-sample sectors")
    for (cl in names(ag$by_class)) for (k in unlist(ag$by_class[[cl]])) if (!identical(by[[k]]$policy$class, cl)) add("policy.agreement.by_class.", cl, ": ", k, " has another class")
  }
  if (!all(unlist(s$priority_sectors) %in% keys)) add("priority_sectors: unknown keys")
  if (!s$priority_rule %in% c("upgrade", "top_si_fallback", "top_eo", "none")) add("priority_rule: '", s$priority_rule, "'")
  rows <- s$partners$rows
  rk <- unlist(lapply(rows, function(r) r$rank$v))
  if (anyDuplicated(rk)) add("partners.rows: duplicate ranks")
  if (any(vapply(rows, function(r) identical(r$iso3, s$iso3), TRUE))) add("partners.rows: the country is its own partner")
  po <- unlist(s$partners$psi_order)
  rb <- stats::setNames(rows, vapply(rows, function(r) r$iso3, ""))
  for (i in seq_along(po)) if (!is.null(rb[[po[i]]]) && (rb[[po[i]]]$rank$v %||% -1) != i) add("partners.psi_order: ", po[i], " at ", i)
  for (r in rows) {
    for (k in c("rank", "psi", "fs", "po", "dp")) fv(r[[k]], paste0("partners.", r$iso3, ".", k))
    if (!is.null(r$reason) && is.null(s$words$reason[[r$reason]])) add("partners.", r$iso3, ".reason: no word")
    if (!is.null(r$match) && is.null(s$words$match[[r$match]])) add("partners.", r$iso3, ".match: no word")
  }
  for (cv in s$caveats) if (!cv$id %in% COUNTRY_CAVEAT_IDS) add("caveats: unknown caveat id ", cv$id)
  if (is.null(s$checks$facts_sha256)) add("checks.facts_sha256: missing")
  p
}

country_check_manifest <- function(m) {
  p <- character()
  add <- function(...) p <<- c(p, paste0(...))
  if (!identical(m$schema, "osi-country")) add("manifest.schema")
  isos <- vapply(m$countries, function(c) c$iso3, "")
  if (anyDuplicated(isos)) add("manifest.countries: duplicate iso3")
  for (c in m$countries) {
    if (identical(c$status, "no_data") && !is.null(c$file)) add(c$iso3, ": no data but a file")
    if (!identical(c$status, "no_data") && !grepl("^facts-[A-Z0-9]{3}\\.[0-9a-f]{8}\\.json$", c$file %||% "")) add(c$iso3, ": bad file name")
    if (!is.null(c$memo) && !identical(c$tier, "core")) add(c$iso3, ": memo for a non-core economy")
    rn <- c$memo$rule_note %||% NULL
    if (!is.null(rn)) {
      if (!identical(rn, "quadrant_rule_2026_10_15")) add(c$iso3, ": memo.rule_note '", rn, "' is not a known rule note")
      if (!is.character(c$memo$rule_note_d) || !nzchar(c$memo$rule_note_d %||% "")) add(c$iso3, ": memo.rule_note without its words (rule_note_d)")
      if (isTRUE(c$memo$stale)) add(c$iso3, ": memo.rule_note on a stale memo")
    }
  }
  p
}
