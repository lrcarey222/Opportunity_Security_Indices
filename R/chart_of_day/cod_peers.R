# Chart of the Day: comparison countries and indexed trends (pure).
#
# Trend charts compare the focus country with a few peers. Peers are neighbours or comparable competitors, not just
# the largest countries: each candidate scores a bonus for sharing the focus country's UN sub-region (or, less, its
# region), a bonus if it is one of the market's leaders within an order of magnitude of the focus country (a direct
# competitor), and loses a point per order of magnitude its size differs. Re-export hubs (Hong Kong, Macao) are never
# peers.
#
# When the countries shown are still of very different size, the chart is indexed (base year = 100), so a small
# country's trend is visible next to large ones; the levels stay in the data (and in the tooltip).

COD_REGION_FIX <- data.frame(iso3 = c("TWN", "XKX", "HKG", "MAC"), sub = c("Eastern Asia", "Southern Europe", "Eastern Asia", "Eastern Asia"),
                             reg = c("Asia", "Europe", "Asia", "Asia"), stringsAsFactors = FALSE)

#' UN sub-region and region for ISO3 codes.
cod_region <- function(iso3) {
  cl <- countrycode::codelist
  sub <- cl$un.regionsub.name[match(iso3, cl$iso3c)]
  reg <- cl$un.region.name[match(iso3, cl$iso3c)]
  i <- match(iso3, COD_REGION_FIX$iso3)
  sub[!is.na(i)] <- COD_REGION_FIX$sub[i[!is.na(i)]]
  reg[!is.na(i)] <- COD_REGION_FIX$reg[i[!is.na(i)]]
  data.frame(iso3 = iso3, sub = sub, reg = reg, stringsAsFactors = FALSE)
}

#' Pick `n` peers for `focus` from `size`, a named vector of positive sizes (names = ISO3; the focus may be in it).
#' Score = sub-region bonus (or region bonus) - |log10(size / focus size)|; ties broken by size.
cod_pick_peers <- function(focus, size, cfg, n = NULL) {
  pc <- cfg$peers
  if (is.null(n)) n <- if (is.null(pc$n)) 3 else pc$n
  size <- size[!is.na(size) & size > 0]
  fs <- size[focus]
  excl <- c("WLD", "EUU", unlist(pc$exclude))
  others <- size[names(size) != focus & !names(size) %in% excl]
  if (!length(others)) return(character())
  if (is.na(fs) || fs <= 0) return(utils::head(names(sort(others, decreasing = TRUE)), n))
  rg <- cod_region(c(focus, names(others)))
  f <- rg[1, ]; o <- rg[-1, ]
  bonus <- ifelse(!is.na(o$sub) & o$sub == f$sub, if (is.null(pc$subregion_bonus)) 1 else pc$subregion_bonus,
                  ifelse(!is.na(o$reg) & o$reg == f$reg, if (is.null(pc$region_bonus)) 0.5 else pc$region_bonus, 0))
  # market leaders within an order of magnitude are direct competitors
  lead_n <- if (is.null(pc$leader_top)) 5 else pc$leader_top
  leaders <- names(sort(size[!names(size) %in% excl], decreasing = TRUE))[seq_len(min(lead_n, length(size)))]
  # when the focus country is itself a leader, its peers are first of all the other leaders (its competitors)
  lb <- if (focus %in% leaders) (if (is.null(pc$leader_bonus_if_leader)) 2 else pc$leader_bonus_if_leader)
        else (if (is.null(pc$leader_bonus)) 0.5 else pc$leader_bonus)
  lead <- ifelse(names(others) %in% leaders & abs(log10(others / fs)) <= 1, lb, 0)
  score <- bonus + lead - abs(log10(others / fs))
  ord <- order(-score, -others)
  utils::head(names(others)[ord], n)
}

#' Size of each country in a long series (iso3, x, value): mean of its last three observations.
cod_series_size <- function(s) {
  s <- s[order(s$x), ]
  tapply(s$value, s$iso3, function(v) mean(utils::tail(v, 3)))
}

#' Rows for a trend chart of `focus` + `peers` from a long series (iso3, x, value). Indexes the chart (base = 100)
#' when the largest and smallest series differ by more than cfg$peers$index_ratio in size. Returns rows (series,
#' iso3, x, value, level, highlight), `indexed`, `base`, and the final index by country.
cod_trend_rows <- function(s, focus, peers, base_year, d, cfg) {
  # `peers` may hold spares: the first n that can be shown are kept (an indexed chart drops series with no base value)
  n <- if (is.null(cfg$peers$n)) 3 else cfg$peers$n
  keep <- c(focus, peers)
  s <- s[s$iso3 %in% keep, ]
  years <- sort(unique(s$x))
  grid <- expand.grid(iso3 = keep, x = years, stringsAsFactors = FALSE)
  grid$level <- s$value[match(paste(grid$iso3, grid$x), paste(s$iso3, s$x))]
  grid$level[is.na(grid$level)] <- 0
  sz <- cod_series_size(data.frame(iso3 = grid$iso3, x = grid$x, value = grid$level))
  sz <- sz[sz > 0]
  ratio <- if (length(sz) > 1) max(sz) / min(sz) else 1
  limit <- if (is.null(cfg$peers$index_ratio)) 4 else cfg$peers$index_ratio
  indexed <- ratio > limit
  base <- NA_integer_
  if (indexed) {
    # base: the configured year, or the first later year in which the focus country is at a meaningful level (10% of
    # its recent size: an index on a near-zero base reads as "5,000-fold") and at least one peer is positive
    fsz <- sz[focus]
    ok <- function(y) { v <- grid$level[grid$x == y]; names(v) <- grid$iso3[grid$x == y]
      !is.na(fsz) && v[focus] >= 0.1 * fsz && sum(v[peers] > 0) >= 1 }
    cands <- years[years >= base_year]
    base <- cands[vapply(cands, ok, logical(1))][1]
    if (is.na(base)) indexed <- FALSE
  }
  if (indexed) {
    b <- grid$level[grid$x == base]; names(b) <- grid$iso3[grid$x == base]
    drop <- names(b)[b <= 0]                      # a series that is zero in the base year cannot be indexed
    use <- c(focus, utils::head(setdiff(peers, drop), n))
    grid <- grid[grid$iso3 %in% use & grid$x >= base, ]
    grid$value <- 100 * grid$level / b[grid$iso3]
  } else {
    grid <- grid[grid$iso3 %in% c(focus, utils::head(peers, n)), ]
    grid$value <- grid$level
  }
  rows <- data.frame(series = cod_country_label(grid$iso3, d$countries), iso3 = grid$iso3, x = grid$x, value = grid$value,
                     level = grid$level, highlight = grid$iso3 == focus, stringsAsFactors = FALSE)
  last <- max(rows$x)
  fin <- stats::setNames(rows$value[rows$x == last], rows$iso3[rows$x == last])
  list(rows = rows, indexed = indexed, base = base, final = fin, shown = unique(rows$iso3))
}

#' "Italy, Slovenia and Hungary".
cod_and <- function(x) if (length(x) <= 1) paste(x) else paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])

#' Title for an indexed trend: how the focus country grew and how that compares with its peers.
cod_index_title <- function(subject, focus, tr, d) {
  last <- max(tr$rows$x)
  f <- tr$final[focus]; p <- tr$final[setdiff(names(tr$final), focus)]
  r <- f / 100
  move <- if (r >= 2) sprintf("grew %s-fold", if (r >= 10) round(r) else format(round(r, 1)))
          else if (r >= 1.05) sprintf("rose %s%%", round(100 * (r - 1)))
          else if (r <= 0.95) sprintf("fell %s%%", round(100 * (1 - r)))
          else "held steady"
  nm <- function(iso) cod_country_name(iso, d$countries)
  faster <- names(p)[p < f]; slower <- names(p)[p > f]
  cmp <- if (!length(slower) && length(faster)) sprintf(", faster than %s", cod_and(nm(faster)))
         else if (!length(faster) && length(slower)) sprintf(", slower than %s", cod_and(nm(slower)))
         else if (length(faster) && length(slower)) sprintf(", faster than %s but slower than %s", cod_and(nm(faster)), cod_and(nm(slower)))
         else ""
  sprintf("%s %s between %s and %s%s", subject, move, tr$base, last, cmp)
}
