# Energy Security and Economic Opportunity of one economy, sector by sector (spec_country.md A9.1-A9.2):
# percentiles among the other economies whose two scores are both rankable in the sector (S_c, the
# Space view's set), medians over S_c, the plotted position, the quadrant (against the country's own
# median sector, country_set_quadrants()), and the reasons a sector is not placed. Pure.
#
# Pillar tables (es, eo): one row per economy x cell with columns iso3, key (cell key), value,
# imputed_share, rankable, reason (the pipeline's rank_exclusion, NA when rankable).

# S_c of every cell: list(key -> list(iso3, es, eo, es_med, eo_med)).
country_cell_stats <- function(es, eo, keys = country_cells_tbl()$key) {
  b <- merge(es[, c("iso3", "key", "value", "rankable")], eo[, c("iso3", "key", "value", "rankable")],
             by = c("iso3", "key"), suffixes = c("_es", "_eo"))
  b <- b[b$rankable_es %in% TRUE & b$rankable_eo %in% TRUE & is.finite(b$value_es) & is.finite(b$value_eo), , drop = FALSE]
  sp <- split(b, factor(b$key, levels = keys))
  out <- lapply(keys, function(k) {
    s <- sp[[k]]
    if (is.null(s) || !nrow(s)) return(list(iso3 = character(), es = numeric(), eo = numeric(), es_med = NA_real_, eo_med = NA_real_))
    list(iso3 = s$iso3, es = s$value_es, eo = s$value_eo, es_med = stats::median(s$value_es), eo_med = stats::median(s$value_eo))
  })
  names(out) <- keys
  out
}

# Which pillars of a cell's S_c entry (country_cell_stats()) are constant across the economies compared:
# sd <= tol over two economies or more (strategic_index_degenerate_sectors()'s rule). A constant pillar is
# a sector constant, not a measurement: its percentiles do not rank the economies, so the sector is not
# placed. Returns c(es = , eo = ) logicals.
country_constant_pillars <- function(st, tol = 1e-12) {
  const <- function(v) length(v) >= 2 && isTRUE(stats::sd(v) <= tol)
  c(es = const(st$es), eo = const(st$eo))
}

# The cells with a pillar constant across the economies compared (country_constant_pillars()), in the order
# of `stats`: data.frame(key, pillar: "es" | "eo"), one row per constant pillar.
country_constant_cells <- function(stats, tol = 1e-12) {
  rows <- lapply(names(stats), function(k) {
    cp <- country_constant_pillars(stats[[k]], tol)
    if (!any(cp)) return(NULL)
    data.frame(key = k, pillar = names(cp)[cp], stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(data.frame(key = character(), pillar = character(), stringsAsFactors = FALSE)), rows))
  rownames(out) <- NULL
  out
}

# "imputed weight share above 0.5" / "not a sovereign economy ..." -> plain words.
country_rank_reason_words <- function(reason, pillar_label) {
  if (is.null(reason) || length(reason) == 0 || is.na(reason) || !nzchar(reason)) return(NA_character_)
  parts <- character()
  if (grepl("sovereign", reason, ignore.case = TRUE)) parts <- c(parts, "not a sovereign economy")
  if (grepl("imputed weight share", reason, ignore.case = TRUE)) parts <- c(parts, "more than half of its weight is filled-in data")
  if (!length(parts)) parts <- reason
  paste0(pillar_label, " not comparable: ", paste(parts, collapse = " and "))
}

# One pillar of one cell (PillarFacts) or NULL when the economy has no score there.
# row: the economy's row of the pillar table for the cell (one-row data frame or NULL); st: the cell's
# S_c entry; which: "es" | "eo".
country_pillar <- function(row, st, which, iso3) {
  if (is.null(row) || !nrow(row) || !is.finite(row$value[1])) return(NULL)
  x <- row$value[1]
  vals <- st[[which]]
  others <- vals[st$iso3 != iso3]
  med <- st[[paste0(which, "_med")]]
  pct <- country_percentile(others, x)
  pos <- country_pos(pct, x, med)
  label <- if (which == "es") "Energy Security" else "Economic Opportunity"
  list(
    v = x,
    d = country_fmt(x, "index"),
    rankable = isTRUE(row$rankable[1]),
    reason = if (isTRUE(row$rankable[1])) NULL else country_chr_or_null(row$reason[1]),
    reason_d = if (isTRUE(row$rankable[1])) NULL else country_chr_or_null(country_rank_reason_words(row$reason[1], label)),
    imputed_share = country_fv(row$imputed_share[1], "share", digits = 4),
    pct = country_fv(if (is.finite(pct)) round(pct, 2) else NA, "pct"),
    pos = if (is.finite(pos)) round(pos, 2) else NULL,
    med = country_fv(med, "index"),
    n = country_fv(length(vals), "count"),
    n_others = country_fv(length(others), "count"),
    order = list(v = NULL, d = COUNTRY_DASH),
    side = if (is.finite(med)) (if (x >= med) "above" else "below") else NULL
  )
}

# The cell list of one economy (without SI, policy, trade and drivers, which other helpers add).
# es_c, eo_c: the economy's rows of the pillar tables (any order); stats: country_cell_stats(); rule: one
# of COUNTRY_QUADRANT_RULES; no_es: the sectors without an Energy Security score for any economy
# (scripts/88 derives them from the data, country_no_es_cells(); COUNTRY_NO_ES_CELLS by default).
country_cells_base <- function(iso3, es_c, eo_c, stats, cells = country_cells_tbl(), rule = "country_median", no_es = COUNTRY_NO_ES_CELLS) {
  out <- lapply(seq_len(nrow(cells)), function(i) {
    k <- cells$key[i]
    st <- stats[[k]]
    er <- es_c[es_c$key == k, , drop = FALSE]
    orr <- eo_c[eo_c$key == k, , drop = FALSE]
    es <- if (k %in% no_es) NULL else country_pillar(er, st, "es", iso3)
    eo <- country_pillar(orr, st, "eo", iso3)
    # A pillar constant across the economies compared (country_constant_pillars()) ranks none of them: no
    # percentile, position or side on it, so the sector is not placed (the Strategic Index drops it too).
    cp <- country_constant_pillars(st)
    unrank <- function(x) {
      x$pct <- country_fv(NA, "pct")
      x["pos"] <- list(NULL)
      x["side"] <- list(NULL)
      x
    }
    if (cp[["es"]] && !is.null(es)) es <- unrank(es)
    if (cp[["eo"]] && !is.null(eo)) eo <- unrank(eo)
    # placed: both scores comparable and a position on both axes (at least one other economy to beat)
    placed <- !is.null(es) && !is.null(eo) && isTRUE(es$rankable) && isTRUE(eo$rankable) && iso3 %in% st$iso3 &&
      !is.null(es$pos) && !is.null(eo$pos)
    reasons <- character()
    if (is.null(es)) {
      reasons <- c(reasons, if (k %in% no_es) "no Energy Security score for any economy in this sector" else "no Energy Security score")
    } else if (!isTRUE(es$rankable)) {
      reasons <- c(reasons, es$reason_d %||% "Energy Security not comparable")
    } else if (cp[["es"]]) reasons <- c(reasons, "the same Energy Security score for every economy in this sector")
    if (is.null(eo)) reasons <- c(reasons, "no Economic Opportunity score")
    else if (!isTRUE(eo$rankable)) reasons <- c(reasons, eo$reason_d %||% "Economic Opportunity not comparable")
    else if (cp[["eo"]]) reasons <- c(reasons, "the same Economic Opportunity score for every economy in this sector")
    if (!placed && !length(reasons)) reasons <- "no other economy to compare with"
    if (!is.null(es)) es$reason_d <- NULL
    if (!is.null(eo)) eo$reason_d <- NULL
    list(
      key = k, tech = cells$tech[i], stage = cells$stage[i], label = cells$label[i], label_prose = cells$label_prose[i],
      es = es, eo = eo, placed = placed, legacy_corner = NULL,
      legacy_corner_reason = if (placed) NULL else paste(unique(reasons), collapse = "; ")
    )
  })
  country_set_quadrants(out, rule)$cells
}

# Fill es.order / eo.order and return the order lists (A8.2 summary.es_order, eo_order): placed cells
# by es.pct ascending (lowest first) and eo.pct descending; ties by the Sector cell order.
country_orders <- function(cells) {
  placed <- Filter(function(c) isTRUE(c$placed), cells)
  ord <- vapply(placed, function(c) match(c$key, country_cells_tbl()$key), 1L)
  key <- vapply(placed, function(c) c$key, "")
  es_p <- vapply(placed, function(c) c$es$pct$v %||% NA_real_, 1)
  eo_p <- vapply(placed, function(c) c$eo$pct$v %||% NA_real_, 1)
  es_order <- key[order(es_p, ord)]
  eo_order <- key[order(-eo_p, ord)]
  for (i in seq_along(cells)) {
    k <- cells[[i]]$key
    if (k %in% es_order) cells[[i]]$es$order <- country_fv(match(k, es_order), "order_low")
    if (k %in% eo_order) cells[[i]]$eo$order <- country_fv(match(k, eo_order), "order_high")
  }
  list(cells = cells, es_order = es_order, eo_order = eo_order)
}
