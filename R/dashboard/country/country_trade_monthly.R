# Monthly trade of a technology cell by sub-component, for the Country pages (pure). One companion
# file per core member, trade-<ISO3>.<h8>.json, built from the monthly Comtrade pull
# (scripts/99_pull_comtrade_monthly_core.R -> data/raw/comtrade_monthly_core.csv): each reporter's
# latest 12 calendar months of imports from and exports to the World, as the reporter declared them
# (no mirror flows, not reconciled with the annual matrix), mapped to the in-basket HS6 master
# (tech, supply_chain, sub_sector). A code shared by two cells counts fully in each cell, as the
# annual cells do; inside a cell each code has one sub-component, so sub-components sum to the cell.
#
# Months: a month the reporter declared (any row in that flow) with no row for a code is a zero
# (Comtrade omits zero trade); a month it did not declare is null. The file carries values only; the
# client indexes them (indexTo100 in contract.ts: 100 * value / value in the first month, null for the
# whole series when the first month is zero or null), which keeps each file near 14 kB gz.

COUNTRY_TRADE_MONTHLY_SCHEMA <- "osi-country-trade-monthly"
COUNTRY_TRADE_MONTHLY_N <- 12L

country_month_label <- function(period) {
  p <- as.integer(period)
  sprintf("%04d-%02d", p %/% 100L, p %% 100L)
}

# YYYYMM months of a reporter's window.
country_month_seq <- function(first, n = COUNTRY_TRADE_MONTHLY_N) {
  first <- as.integer(first)
  idx <- (first %/% 100L) * 12L + (first %% 100L - 1L) + seq.int(0L, n - 1L)
  as.integer((idx %/% 12L) * 100L + idx %% 12L + 1L)
}

country_index_100 <- function(v) {
  v0 <- v[1]
  if (is.null(v0) || is.na(v0) || v0 <= 0) return(rep(NA_real_, length(v)))
  100 * v / v0
}

# Long table: iso3, month (YYYYMM), flow (import/export), cell, tech, stage, sub_sector, value_usd
# (NA for a month not reported, or reported with fewer than `min_coverage` x the reporter's median number of
# codes in that flow), index_100. `raw` = the pull (reporter_iso, period, flow_code M/X, cmd_code,
# primary_value); `basket` = sector_trade_basket() rows; `windows` = the pull's windows file
# (reporter_iso, first_month, last_month).
country_monthly_table <- function(raw, basket, windows, cells = country_cells_tbl(), min_coverage = 0.6) {
  basket <- basket[basket$cell %in% cells$key, , drop = FALSE]
  raw <- raw[raw$flow_code %in% c("M", "X"), , drop = FALSE]
  raw$hs6 <- formatC(as.integer(raw$cmd_code), width = 6, flag = "0")
  raw$period <- as.integer(raw$period)
  raw$flow <- ifelse(raw$flow_code == "M", "import", "export")
  w <- windows[!is.na(windows$first_month), , drop = FALSE]
  subs <- unique(basket[c("cell", "tech", "supply_chain", "sub_sector")])
  out <- list()
  for (i in seq_len(nrow(w))) {
    iso <- w$reporter_iso[i]
    months <- country_month_seq(w$first_month[i])
    r <- raw[raw$reporter_iso == iso & raw$period %in% months, , drop = FALSE]
    # a reporter with a window but no rows (a chunk not fetched yet) has no monthly data, not a null file
    if (!nrow(r)) next
    for (fl in c("import", "export")) {
      rf <- r[r$flow == fl, , drop = FALSE]
      # a month counts as reported when the reporter declared it with near-full code coverage: a partial
      # release (Japan's April 2026 imports: 158 codes against ~372 in its other months) would read as a
      # collapse of every sub-component, so it is treated as not (fully) reported
      n_codes <- tapply(rf$hs6, rf$period, function(z) length(unique(z)))
      full <- if (length(n_codes)) as.integer(names(n_codes)[n_codes >= min_coverage * stats::median(n_codes)]) else integer()
      declared <- months %in% full
      j <- merge(rf[c("hs6", "period", "primary_value")], basket[c("hs6", "cell", "sub_sector")], by = "hs6")
      agg <- if (nrow(j)) stats::aggregate(primary_value ~ cell + sub_sector + period, data = j, FUN = sum) else
        data.frame(cell = character(), sub_sector = character(), period = integer(), primary_value = numeric())
      key <- paste(agg$cell, agg$sub_sector, agg$period, sep = "|")
      g <- subs[rep(seq_len(nrow(subs)), each = length(months)), , drop = FALSE]
      g$month <- rep(months, times = nrow(subs))
      v <- agg$primary_value[match(paste(g$cell, g$sub_sector, g$month, sep = "|"), key)]
      v[is.na(v)] <- 0
      v[!declared[match(g$month, months)]] <- NA_real_
      g$value_usd <- v
      g$iso3 <- iso
      g$flow <- fl
      out[[length(out) + 1]] <- g
    }
  }
  if (!length(out)) return(data.frame())
  t <- do.call(rbind, out)
  names(t)[names(t) == "supply_chain"] <- "stage"
  t <- t[order(t$iso3, t$flow, t$cell, t$sub_sector, t$month), , drop = FALSE]
  t$index_100 <- stats::ave(t$value_usd, t$iso3, t$flow, t$cell, t$sub_sector, FUN = country_index_100)
  rownames(t) <- NULL
  t[c("iso3", "month", "flow", "cell", "tech", "stage", "sub_sector", "value_usd", "index_100")]
}

# The companion doc of one economy. Cells in the Sector cell order (every cell of the basket, traded
# or not); sub-components by their value over the window and flows (largest first); n_codes = basket
# codes of the sub-component; values whole USD.
country_monthly_doc <- function(tbl, iso3, window, basket, source, cells = country_cells_tbl()) {
  x <- tbl[tbl$iso3 == iso3, , drop = FALSE]
  months <- country_month_seq(window$first_month)
  num <- function(v) as.numeric(round(v))  # 12 values: always a JSON array, so no I() (AsIs skips exact doubles)
  cell_docs <- list()
  for (k in cells$key[cells$key %in% x$cell]) {
    xc <- x[x$cell == k, , drop = FALSE]
    ser <- function(flow, sub) {
      s <- xc[xc$flow == flow & xc$sub_sector == sub, , drop = FALSE]
      vapply(months, function(m) {
        z <- s$value_usd[s$month == m]
        if (!length(z) || all(is.na(z))) NA_real_ else sum(z)
      }, numeric(1))
    }
    names_sub <- unique(xc$sub_sector)
    tot <- vapply(names_sub, function(s) sum(xc$value_usd[xc$sub_sector == s], na.rm = TRUE), numeric(1))
    names_sub <- names_sub[order(-tot, names_sub)]
    sub_docs <- lapply(names_sub, function(s) {
      list(name = s, n_codes = sum(basket$cell == k & basket$sub_sector == s),
           imports = num(ser("import", s)), exports = num(ser("export", s)))
    })
    add_up <- function(fl) {
      m <- do.call(rbind, lapply(sub_docs, function(s) as.numeric(s[[fl]])))
      apply(m, 2, function(col) if (all(is.na(col))) NA_real_ else sum(col, na.rm = TRUE))
    }
    cell_docs[[length(cell_docs) + 1]] <- list(
      key = k, label = cells$label[cells$key == k],
      total = list(imports = num(add_up("imports")), exports = num(add_up("exports"))),
      subs = sub_docs
    )
  }
  declared <- function(fl) vapply(months, function(m) any(!is.na(x$value_usd[x$flow == fl & x$month == m])), logical(1))
  list(
    schema = COUNTRY_TRADE_MONTHLY_SCHEMA, schema_version = 1L, iso3 = iso3,
    window = list(first = country_month_label(months[1]), last = country_month_label(months[length(months)]),
                  months = country_arr(country_month_label(months)),
                  declared = list(imports = declared("import"), exports = declared("export"))),
    source = source,
    cells = cell_docs
  )
}

# The R mirror of checkTradeMonthlyDoc() in dashboard/src/country/contract.ts (the same rules and
# keywords). Works on the doc as built or as read back with jsonlite::fromJSON(simplifyVector = FALSE).
country_check_monthly_doc <- function(doc, cell_keys = country_cells_tbl()$key) {
  p <- character()
  add <- function(...) p <<- c(p, paste0(...))
  N <- COUNTRY_TRADE_MONTHLY_N
  is_num <- function(z) is.null(z) || (is.numeric(z) && length(z) == 1 && (is.na(z) || is.finite(z)))
  nums <- function(v) {
    if (!is.list(v) && !is.numeric(v) && !is.logical(v)) return(NULL)
    v <- as.list(v)
    if (!all(vapply(v, is_num, logical(1)) | vapply(v, function(z) length(z) == 1 && is.na(z), logical(1)))) return(NULL)
    vapply(v, function(z) if (is.null(z) || is.na(z)) NA_real_ else as.numeric(z), numeric(1))
  }
  is_str1 <- function(z) is.character(z) && length(z) == 1 && !is.na(z)
  if (!identical(doc$schema, COUNTRY_TRADE_MONTHLY_SCHEMA)) add("trade.schema")
  if (!is.numeric(doc$schema_version)) add("trade.schema_version: not a number")
  else if (floor(doc$schema_version) > 1) add("trade: schema_version ", doc$schema_version, " needs a newer app")
  if (!is_str1(doc$iso3) || !grepl("^[A-Z0-9]{3}$", doc$iso3)) add("trade.iso3: not an ISO3 code")
  months <- as.character(unlist(doc$window$months))
  if (length(months) != N) add("trade.window.months: expected 12 months")
  if (length(months) && !all(grepl("^[0-9]{4}-[0-9]{2}$", months))) add("trade.window.months: bad month")
  if (length(months) && (!identical(months[1], doc$window$first) || !identical(months[length(months)], doc$window$last))) add("trade.window: first/last")
  for (fl in c("imports", "exports")) {
    d <- doc$window$declared[[fl]]
    if (length(d) != N || !all(vapply(as.list(d), function(z) is.logical(z) && length(z) == 1 && !is.na(z), logical(1)))) add("trade.window.declared.", fl, ": expected 12 true/false")
  }
  src <- doc$source
  if (!is.list(src) || !is_str1(src$id) || !is_str1(src$label)) add("trade.source")
  else for (k in c("edition", "licence", "url")) if (!(is.null(src[[k]]) || (length(src[[k]]) == 1 && is.na(src[[k]])) || is_str1(src[[k]]))) add("trade.source.", k)
  if (!length(doc$cells)) add("trade.cells: empty")
  for (c in doc$cells) {
    k <- c$key %||% ""
    if (!k %in% cell_keys) add("trade.cells: unknown cell key '", k, "'")
    if (!is_str1(c$label)) add(k, ".label")
    if (!length(c$subs)) add(k, ": no sub-components")
    for (s in c$subs) if (!is_str1(s$name) || !nzchar(s$name)) add(k, ": sub-component without a name")
    for (fl in c("imports", "exports")) {
      tot <- nums(c$total[[fl]])
      if (length(tot) != N) { add(k, ".total.", fl, ": length"); next }
      sm <- rep(0, N)
      for (s in c$subs) {
        v <- nums(s[[fl]])
        if (length(v) != N) { add(k, "/", s$name %||% "?", ".", fl, ": length"); next }
        if (any(v[!is.na(v)] < 0)) add(k, "/", s$name, ".", fl, ": negative value")
        sm <- sm + ifelse(is.na(v), 0, v)
      }
      if (any(abs(ifelse(is.na(tot), 0, tot) - sm) > 0.5 + 1e-9 * abs(sm))) add(k, ".total.", fl, ": sub-components do not sum to the cell")
    }
  }
  p
}
