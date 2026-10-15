# Trade of one economy on the reconciled matrix (spec_country.md A9.8): imports and exports by cell,
# the largest sources and markets, and each partner's average share across the economy's baskets
# (never a sum across baskets, which overlap). Pure.
#
# flows: one row per cell x exporter x importer for the trade year: key, exporter, importer (ISO3),
# value (USD), status ("importer" | "mirror"). EU members appear as members.

# Per-cell trade facts and the share table of one economy.
# basket_keys: the cells that have a basket; party_fn(iso3) -> list(name, group).
country_trade_cells <- function(flows, iso3, basket_keys, year, party_fn, n_parties = 5, cells = country_cells_tbl()) {
  f <- flows[(flows$importer == iso3 | flows$exporter == iso3) & flows$exporter != flows$importer & is.finite(flows$value) & flows$value > 0, , drop = FALSE]
  shares <- list()
  facts <- lapply(cells$key, function(k) {
    if (!k %in% basket_keys) return(list(has_basket = FALSE, year = as.character(year), imports = list(v = NULL, d = COUNTRY_DASH),
                                         exports = list(v = NULL, d = COUNTRY_DASH), sources = list(), markets = list()))
    imp <- f[f$key == k & f$importer == iso3, , drop = FALSE]
    exp <- f[f$key == k & f$exporter == iso3, , drop = FALSE]
    ti <- sum(imp$value)
    te <- sum(exp$value)
    party <- function(d, who, total) {
      if (!nrow(d) || total <= 0) return(list())
      d <- d[order(-d$value, d[[who]]), , drop = FALSE]
      d <- utils::head(d, n_parties)
      lapply(seq_len(nrow(d)), function(i) {
        p <- party_fn(d[[who]][i])
        list(iso3 = d[[who]][i], name = p$name, group = p$group, share = country_fv(floor(d$value[i] / total * 1e6) / 1e6, "share"),
             value = country_fv(round(d$value[i]), "usd"), status = if (identical(d$status[i], "mirror")) "mirror" else "importer")
      })
    }
    if (ti > 0) shares[[length(shares) + 1]] <<- data.frame(key = k, side = "imp", partner = imp$exporter, value = imp$value, share = imp$value / ti,
                                                           status = imp$status, stringsAsFactors = FALSE)
    if (te > 0) shares[[length(shares) + 1]] <<- data.frame(key = k, side = "exp", partner = exp$importer, value = exp$value, share = exp$value / te,
                                                           status = exp$status, stringsAsFactors = FALSE)
    list(has_basket = TRUE, year = as.character(year), imports = country_fv(signif(ti, 12), "usd"), exports = country_fv(signif(te, 12), "usd"),
         sources = party(imp, "exporter", ti), markets = party(exp, "importer", te), .imports = ti, .exports = te)
  })
  names(facts) <- cells$key
  sh <- if (length(shares)) do.call(rbind, shares) else data.frame(key = character(), side = character(), partner = character(), value = numeric(),
                                                                    share = numeric(), status = character(), stringsAsFactors = FALSE)
  list(facts = facts, shares = sh)
}

# Each partner's average shares across the economy's import and export baskets (A9.8).
country_partner_trade <- function(shares) {
  imp_cells <- unique(shares$key[shares$side == "imp"])
  exp_cells <- unique(shares$key[shares$side == "exp"])
  partners <- sort(unique(shares$partner))
  if (!length(partners)) {
    return(list(ties = data.frame(partner = character(), imp_mean = numeric(), exp_mean = numeric(), tie = numeric(), tie_rank = integer(),
                                  top3_import_cells = integer(), n_import_cells = integer(), stringsAsFactors = FALSE),
                n_import_cells = 0L, n_export_cells = 0L))
  }
  imp <- shares[shares$side == "imp", , drop = FALSE]
  exp <- shares[shares$side == "exp", , drop = FALSE]
  imp_sum <- tapply(imp$share, factor(imp$partner, levels = partners), sum)
  exp_sum <- tapply(exp$share, factor(exp$partner, levels = partners), sum)
  imp_sum[is.na(imp_sum)] <- 0
  exp_sum[is.na(exp_sum)] <- 0
  imp_mean <- if (length(imp_cells)) as.numeric(imp_sum) / length(imp_cells) else rep(NA_real_, length(partners))
  exp_mean <- if (length(exp_cells)) as.numeric(exp_sum) / length(exp_cells) else rep(NA_real_, length(partners))
  tie <- ifelse(is.na(imp_mean), exp_mean, ifelse(is.na(exp_mean), imp_mean, (imp_mean + exp_mean) / 2))
  # top-3 source in how many import baskets
  top3 <- integer(length(partners))
  if (nrow(imp)) {
    r <- stats::ave(-imp$value, imp$key, FUN = function(v) rank(v, ties.method = "first"))
    t3 <- table(factor(imp$partner[r <= 3], levels = partners))
    top3 <- as.integer(t3)
  }
  nimp <- as.integer(table(factor(imp$partner, levels = partners)))
  o <- order(-tie, partners)
  tie_rank <- integer(length(partners))
  tie_rank[o] <- seq_along(o)
  list(
    ties = data.frame(partner = partners, imp_mean = imp_mean, exp_mean = exp_mean, tie = tie, tie_rank = tie_rank,
                      top3_import_cells = top3, n_import_cells = nimp, stringsAsFactors = FALSE),
    n_import_cells = length(imp_cells), n_export_cells = length(exp_cells)
  )
}

# The supplier matrix (A4.2 S5): rows = up to n_rows economies (the top-5 sources of each priority
# sector, then the top average suppliers), columns = cells with imports.
country_supplier_matrix <- function(shares, ties, priority_keys, n_rows = 12, cells = country_cells_tbl()) {
  imp <- shares[shares$side == "imp", , drop = FALSE]
  cols <- cells$key[cells$key %in% unique(imp$key)]
  rows <- character()
  for (k in priority_keys) {
    d <- imp[imp$key == k, , drop = FALSE]
    rows <- c(rows, utils::head(d$partner[order(-d$value, d$partner)], 5))
  }
  t <- ties[order(-ties$imp_mean, ties$partner), , drop = FALSE]
  rows <- unique(c(rows, t$partner[is.finite(t$imp_mean) & t$imp_mean > 0]))
  rows <- utils::head(rows, n_rows)
  get <- function(field) lapply(rows, function(p) lapply(cols, function(k) {
    v <- imp[[field]][imp$partner == p & imp$key == k]
    if (!length(v)) NULL else if (field == "share") round(v[1], 6) else v[1]
  }))
  list(partners = country_arr(rows), cells = country_arr(cols), share = get("share"), value = get("value"),
       status = lapply(rows, function(p) lapply(cols, function(k) {
         v <- imp$status[imp$partner == p & imp$key == k]
         if (!length(v)) NULL else if (identical(v[1], "mirror")) "mirror" else "importer"
       })))
}
