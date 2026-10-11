# Trade of the Sector Insights bundle (spec_sector.md 3.8, 8.6): the reconciled Comtrade matrix
# that partnership_strength_build_trade_cells() builds, reduced to the chunk tables. Pure.
#
#   trade.flows     positive flows in either year (v2020 / v2024 NaN and status "none" where the
#                   flow is zero or unobserved that year; zeros are rebuilt on the client)
#   trade.declared  who declared imports / exports of a cell in a year (rows with either flag)
#   trade.codes     the HS6 basket per cell (pipeline view data/raw/energy_hs6_master.csv)
#   trade.excluded  rows the reference master marks excluded
# and the country facts trade.exports / trade.imports / trade.net (sums over the matrix).
#
# Zero rebuild (client): a pair absent from trade.flows in a year is an observed zero when the
# importer declared imports of the cell that year, or, when it did not, when the exporter declared
# exports (a mirror zero); otherwise it is unobserved.

sector_cell_slug <- function(tech, stage) {
  tolower(paste0(gsub(" ", "-", tech), "-", stage))
}

# The basket as distinct (tech, supply_chain, sub_sector, hs6) rows; stops when a code maps to two
# sub-sectors inside one cell. Returns the rows plus `shared_n` (number of OTHER cells listing the
# code) and the cell slug.
sector_trade_basket <- function(hs6_raw) {
  b <- tibble::tibble(
    tech = as.character(hs6_raw$tech), supply_chain = as.character(hs6_raw$supply_chain),
    sub_sector = as.character(hs6_raw$sub_sector),
    hs6 = formatC(as.integer(hs6_raw$hs6), width = 6, flag = "0"),
    note = if ("note" %in% names(hs6_raw)) as.character(hs6_raw$note) else NA_character_
  )
  b <- b[!duplicated(b[c("tech", "supply_chain", "sub_sector", "hs6")]), , drop = FALSE]
  two <- b[duplicated(b[c("tech", "supply_chain", "hs6")]), , drop = FALSE]
  if (nrow(two)) {
    stop("sector_trade_basket(): HS6 codes in two sub-sectors of one cell: ",
         paste(utils::head(paste(two$tech, two$supply_chain, two$hs6), 5), collapse = "; "))
  }
  b$cell <- sector_cell_slug(b$tech, b$supply_chain)
  n_cells <- table(b$hs6)
  b$shared_n <- as.integer(n_cells[b$hs6]) - 1L
  b
}

# trade.codes rows of a basket; descriptions: named character vector hs6 -> text (NULL = none).
# caveat: the basket note from "kept with caveat" on, wherever the phrase sits in the note ("moved
# from Midstream: ...; kept with caveat: ..." keeps its caveat).
sector_trade_codes <- function(basket, descriptions = NULL) {
  hit <- !is.na(basket$note) & grepl("kept with caveat", basket$note, fixed = TRUE)
  caveat <- ifelse(hit, sub("^.*?kept with caveat", "kept with caveat", basket$note, perl = TRUE), NA_character_)
  tibble::tibble(
    item_key = paste0("cell:", basket$cell), sub_sector = basket$sub_sector, hs6 = basket$hs6,
    description = if (is.null(descriptions)) NA_character_ else unname(descriptions[basket$hs6]),
    shared_n = basket$shared_n, caveat = caveat
  )
}

# trade.excluded rows of the reference master (rows with excluded TRUE), for the cells in `cells`.
sector_trade_excluded <- function(hs6_ref, cells) {
  ex <- hs6_ref[as.logical(hs6_ref$excluded) %in% TRUE, , drop = FALSE]
  out <- tibble::tibble(
    item_key = paste0("cell:", sector_cell_slug(ex$tech, ex$supply_chain)),
    sub_sector = as.character(ex$sub_sector),
    hs6 = formatC(as.integer(ex$hs6), width = 6, flag = "0"),
    reason = dplyr::coalesce(as.character(ex$note), "excluded")
  )
  out <- out[sub("^cell:", "", out$item_key) %in% cells, , drop = FALSE]
  out[!duplicated(out[c("item_key", "hs6")]), , drop = FALSE]
}

# Codes the reference master marks excluded that are still in the pipeline basket of the same cell
# (scripts/04 was not rerun): returns them; the exporter stops on any.
sector_trade_excluded_in_basket <- function(basket, hs6_ref) {
  ex <- hs6_ref[as.logical(hs6_ref$excluded) %in% TRUE, , drop = FALSE]
  k_ex <- paste(sector_cell_slug(ex$tech, ex$supply_chain), formatC(as.integer(ex$hs6), width = 6, flag = "0"))
  k_b <- paste(basket$cell, basket$hs6)
  # A code excluded from one cell can legitimately stay in another; only the same cell counts.
  intersect(k_b, k_ex)
}

# Who reported which cell in which year, from the standardised dyads (as
# partnership_strength_standardize_comtrade() returns them) and the basket: distinct
# (reporter_iso, period, flow_direction, cell).
sector_trade_reporters <- function(std, basket) {
  map <- unique(basket[c("hs6", "cell")])
  j <- merge(unique(std[c("reporter_iso", "period", "flow_direction", "cmd_code")]), map,
             by.x = "cmd_code", by.y = "hs6")
  tibble::as_tibble(unique(j[c("reporter_iso", "period", "flow_direction", "cell")]))
}

# trade.declared from the reporters table: one row per (cell, economy, year) with either flag.
sector_trade_declared <- function(reporters, years = SECTOR_TRADE_YEARS) {
  r <- reporters[reporters$period %in% years, , drop = FALSE]
  key <- unique(r[c("cell", "reporter_iso", "period")])
  imp <- paste(r$cell, r$reporter_iso, r$period)[r$flow_direction == "import"]
  exp <- paste(r$cell, r$reporter_iso, r$period)[r$flow_direction == "export"]
  k <- paste(key$cell, key$reporter_iso, key$period)
  tibble::tibble(item_key = paste0("cell:", key$cell), geo = key$reporter_iso, year = as.integer(key$period),
                 imports_declared = k %in% imp, exports_declared = k %in% exp)
}

# The zero-rebuild rule of the contract checked against a matrix: a (pair, year) is observed exactly
# when its importer declared imports of the cell that year (source "importer") or, failing that, its
# exporter declared exports (source "mirror"); otherwise it is NA. Returns list(n = pair-years
# checked, bad = rows breaking the rule, example = up to 3 of them).
sector_trade_zero_rule_check <- function(trade_cells, declared, years = SECTOR_TRADE_YEARS) {
  cell <- paste0("cell:", sector_cell_slug(trade_cells$tech, trade_cells$supply_chain))
  dk <- paste(declared$item_key, declared$geo, declared$year)
  imp_keys <- dk[declared$imports_declared]
  exp_keys <- dk[declared$exports_declared]
  n <- 0L
  bad <- 0L
  example <- character()
  for (yi in seq_along(years)) {
    y <- years[yi]
    v <- if (yi == 1) trade_cells$value_first else trade_cells$value_last
    s <- if (yi == 1) trade_cells$source_first else trade_cells$source_last
    imp <- paste(cell, trade_cells$importer_iso, y) %in% imp_keys
    exp <- paste(cell, trade_cells$exporter_iso, y) %in% exp_keys
    observed <- !is.na(v)
    want_src <- ifelse(imp, "importer", "mirror")
    wrong <- observed != (imp | exp) | (observed & !(s %in% want_src))
    n <- n + length(v)
    bad <- bad + sum(wrong)
    if (any(wrong) && length(example) < 3) {
      w <- utils::head(which(wrong), 3 - length(example))
      example <- c(example, paste(cell[w], trade_cells$exporter_iso[w], "->", trade_cells$importer_iso[w], y))
    }
  }
  list(n = n, bad = bad, example = example)
}

# The matrix rows (exporter_iso, importer_iso, tech, supply_chain, value_first, value_last,
# source_first, source_last) as trade.flows: positive in either year only.
sector_trade_flows <- function(trade_cells) {
  v20 <- trade_cells$value_first
  v24 <- trade_cells$value_last
  pos20 <- !is.na(v20) & v20 > 0
  pos24 <- !is.na(v24) & v24 > 0
  keep <- pos20 | pos24
  tc <- trade_cells[keep, , drop = FALSE]
  tibble::tibble(
    item_key = paste0("cell:", sector_cell_slug(tc$tech, tc$supply_chain)),
    exporter = tc$exporter_iso, importer = tc$importer_iso,
    v2020 = ifelse(pos20[keep], v20[keep], NaN), v2024 = ifelse(pos24[keep], v24[keep], NaN),
    s2020 = ifelse(pos20[keep], tc$source_first, "none"), s2024 = ifelse(pos24[keep], tc$source_last, "none")
  )
}

# Country totals from the full matrix (observed pairs, zeros included): exports and imports per
# (cell, economy, year), and net = exports - imports where both exist. Status: imports "mirror" when
# the importer did not declare that year; a total of 0 is "zero". declared: trade.declared.
sector_trade_country <- function(trade_cells, declared, years = SECTOR_TRADE_YEARS) {
  cell <- sector_cell_slug(trade_cells$tech, trade_cells$supply_chain)
  out <- list()
  for (yi in seq_along(years)) {
    y <- years[yi]
    v <- if (yi == 1) trade_cells$value_first else trade_cells$value_last
    obs <- !is.na(v)
    ex <- stats::aggregate(list(value = v[obs]), list(cell = cell[obs], geo = trade_cells$exporter_iso[obs]), sum)
    im <- stats::aggregate(list(value = v[obs]), list(cell = cell[obs], geo = trade_cells$importer_iso[obs]), sum)
    dec <- paste(declared$item_key, declared$geo, declared$year)[declared$imports_declared]
    im_status <- ifelse(paste0("cell:", im$cell, " ", im$geo, " ", y) %in% dec, "obs", "mirror")
    ex_rows <- tibble::tibble(var = "trade.exports", item_key = paste0("cell:", ex$cell), geo = ex$geo, year = as.integer(y),
                              value = ex$value, status = ifelse(ex$value == 0, "zero", "obs"))
    im_rows <- tibble::tibble(var = "trade.imports", item_key = paste0("cell:", im$cell), geo = im$geo, year = as.integer(y),
                              value = im$value, status = ifelse(im$value == 0, "zero", im_status))
    both <- merge(ex, im, by = c("cell", "geo"), suffixes = c("_x", "_m"))
    net_status <- ifelse(paste0("cell:", both$cell, " ", both$geo, " ", y) %in% dec, "obs", "mirror")
    net <- both$value_x - both$value_m
    net_rows <- tibble::tibble(var = "trade.net", item_key = paste0("cell:", both$cell), geo = both$geo, year = as.integer(y),
                               value = net, status = ifelse(net == 0, "zero", net_status))
    out[[length(out) + 1]] <- dplyr::bind_rows(ex_rows, im_rows, net_rows)
  }
  dplyr::bind_rows(out)
}

# World totals of trade.exports and trade.imports: the sum of all flows of the cell and year
# (basis sum_all), equal to the sum of the country totals.
sector_trade_world <- function(country) {
  x <- country[country$var %in% c("trade.exports", "trade.imports"), , drop = FALSE]
  w <- stats::aggregate(list(value = x$value), list(var = x$var, item_key = x$item_key, year = x$year), sum)
  tibble::tibble(var = w$var, item_key = w$item_key, year = as.integer(w$year), value = w$value, basis = "sum_all")
}

# Intra-EU flows per (cell, year): the sum of observed flows with both sides EU members.
sector_eu_intra <- function(trade_cells, eu_members, years = SECTOR_TRADE_YEARS) {
  both <- trade_cells$exporter_iso %in% eu_members & trade_cells$importer_iso %in% eu_members
  tc <- trade_cells[both, , drop = FALSE]
  cell <- paste0("cell:", sector_cell_slug(tc$tech, tc$supply_chain))
  out <- list()
  for (yi in seq_along(years)) {
    v <- if (yi == 1) tc$value_first else tc$value_last
    ok <- !is.na(v)
    if (!any(ok)) next
    a <- stats::aggregate(list(intra = v[ok]), list(item_key = cell[ok]), sum)
    out[[yi]] <- tibble::tibble(item_key = a$item_key, year = as.integer(years[yi]), intra = a$intra)
  }
  dplyr::bind_rows(out)
}

# meta.reconcile: per trade cell, the matrix W, China share and HHI (members mode, from the
# summary) beside the exporter-reported totals of data/processed (exports summed over reporters,
# China's share, the processed HHI). warn = China shares differ by more than 3 points.
sector_trade_reconcile <- function(summary, reported, china_code = "CHN", geo_codes) {
  # reported: (item_key, year, total, china, hhi, n_reporters)
  # Only the years the exporter-reported basis covers (2024): a 2020 row would have one side only.
  m <- summary[summary$var == "trade.exports" & summary$eu_mode == "members" & summary$year %in% reported$year, , drop = FALSE]
  r <- merge(m[c("item_key", "year", "W", "china", "hhi")], reported, by = c("item_key", "year"), all.x = TRUE)
  tibble::tibble(
    item_key = r$item_key, year = as.integer(r$year), matrix_total = r$W, matrix_china = r$china, matrix_hhi = r$hhi,
    reported_total = r$total, reported_china = r$china_reported, reported_hhi = r$hhi_reported,
    n_reporters = as.integer(r$n_reporters),
    warn = !is.na(r$china_reported) & abs(r$china - r$china_reported) > 0.03
  )
}
