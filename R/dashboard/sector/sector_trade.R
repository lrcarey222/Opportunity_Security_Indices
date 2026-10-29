# Trade of the Sector Insights bundle (spec_sector.md 3.8, 8.6): the reconciled Comtrade matrix
# that partnership_strength_build_trade_cells() builds, reduced to the chunk tables. Pure.
#
#   trade.flows     positive flows in either year (v2020 / v2024 NaN and status "none" where the
#                   flow is zero or unobserved that year; zeros are rebuilt on the client)
#   trade.declared  who declared imports / exports of a cell in a year (rows with either flag)
#   trade.codes     the HS6 basket per cell (pipeline view data/raw/energy_hs6_master.csv)
#   trade.excluded  rows the reference master marks excluded, at the technology level (schema 2: the
#                   excluded rows carry no six-stage stage, so none is invented for them)
# and the country facts trade.exports / trade.imports / trade.net (sums over the matrix).
#
# Cells are six-stage (schema 2): each HS6 code sits in the master's `stage` column (scripts/04 stages
# it; the themes score trade on the same key), not in its former three-stage `supply_chain` column.
#
# Zero rebuild (client): a pair absent from trade.flows in a year is an observed zero when the
# importer declared imports of the cell that year, or, when it did not, when the exporter declared
# exports (a mirror zero); otherwise it is unobserved.

sector_cell_slug <- function(tech, stage) {
  tolower(paste0(gsub(" ", "-", tech), "-", stage))
}

# Basket notes of the master (trade.codes caveat, trade.excluded reason) as shipped: the six stages
# replace the old three (2026-10-28), so the stage names of the 2026-10 basket review are reworded.
# The review measured each code's share of the basket of the time; that basket was the technology's
# materials (the old Upstream: Extraction, Refining and Recovery) or manufacturing (the old Midstream)
# basket, which the reworded text names. An "Earlier note" about a move between the old stages is
# history and is dropped. Text naming an old stage in any other way is left as it is: scripts/87
# reports it (sector_has_legacy_stage()) so that the master's note can be reworded at the source.
sector_six_stage_text <- function(x) {
  x <- as.character(x)
  tl <- paste(gsub(" ", "[ ]", SECTOR_TECH_LABELS), collapse = "|")
  x <- sub("\\s*Earlier note: moved from (Upstream|Midstream|Downstream):.*$", "", x, perl = TRUE)
  x <- gsub("Refined chemicals and intermediates stay in Batteries Midstream", "Refined chemicals and intermediates are in Batteries Refining", x, fixed = TRUE)
  x <- gsub(", a Midstream step with no code of its own", ", a fuel-cycle step with no code of its own", x, fixed = TRUE)
  x <- gsub("Midstream if anything", "Components if anything", x, fixed = TRUE)
  x <- gsub(paste0("\\b(Excluded|excluded) from (", tl, ") Upstream\\b"), "\\1 from the \\2 materials basket", x, perl = TRUE)
  x <- gsub(paste0("\\b(Excluded|excluded) from (", tl, ") Midstream\\b"), "\\1 from the \\2 manufacturing basket", x, perl = TRUE)
  x <- gsub(paste0("\\bin (", tl, ") Upstream only\\b"), "in the \\1 materials basket only", x, perl = TRUE)
  x <- gsub(paste0("\\b(", tl, ") Upstream basket\\b"), "\\1 materials basket", x, perl = TRUE)
  x <- gsub(paste0("\\b(", tl, ") Midstream basket\\b"), "\\1 manufacturing basket", x, perl = TRUE)
  x
}

# The basket as distinct (tech, supply_chain, sub_sector, hs6) rows, supply_chain being the code's
# six-stage stage (hs6_raw[[stage_col]]; stops on a value that is not one of the six); stops when a
# code maps to two sub-sectors inside one cell. Returns the rows plus `shared_n` (number of OTHER cells
# listing the code), the cell slug and `listed_as` (the master's former supply_chain column, kept only
# to match the reference master's excluded rows, which are keyed on it; NA when stage_col is
# supply_chain itself).
sector_trade_basket <- function(hs6_raw, stage_col = "stage") {
  if (!stage_col %in% names(hs6_raw)) {
    stop("sector_trade_basket(): the HS6 master has no `", stage_col, "` column (staged before the six-stage master); rerun scripts/04_build_hs6_views.R.")
  }
  stage <- as.character(hs6_raw[[stage_col]])
  assert_supply_chain_stages(stage, label = paste0("HS6 master `", stage_col, "`"))
  b <- tibble::tibble(
    tech = as.character(hs6_raw$tech), supply_chain = stage,
    sub_sector = as.character(hs6_raw$sub_sector),
    hs6 = formatC(as.integer(hs6_raw$hs6), width = 6, flag = "0"),
    note = if ("note" %in% names(hs6_raw)) as.character(hs6_raw$note) else NA_character_,
    listed_as = if (stage_col != "supply_chain" && "supply_chain" %in% names(hs6_raw)) as.character(hs6_raw$supply_chain) else NA_character_
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
# caveat: the basket note from "kept with caveat" on, wherever the phrase sits in the note (a note
# that records a move before its "kept with caveat" keeps only its caveat), reworded for the six stages
# (sector_six_stage_text()).
sector_trade_codes <- function(basket, descriptions = NULL) {
  hit <- !is.na(basket$note) & grepl("kept with caveat", basket$note, fixed = TRUE)
  caveat <- ifelse(hit, sector_six_stage_text(sub("^.*?kept with caveat", "kept with caveat", basket$note, perl = TRUE)), NA_character_)
  tibble::tibble(
    item_key = paste0("cell:", basket$cell), sub_sector = basket$sub_sector, hs6 = basket$hs6,
    description = if (is.null(descriptions)) NA_character_ else unname(descriptions[basket$hs6]),
    shared_n = basket$shared_n, caveat = caveat
  )
}

# trade.excluded rows of the reference master (rows with excluded TRUE) of the technologies in `techs`
# (tech labels), at the technology level: tech, sub_sector, hs6, reason (the note, reworded for the
# six stages). The excluded rows have no six-stage `stage` (data/reference/energy_hs6_master.csv
# leaves it blank, and its serves_stage too), and the stages their former stage covered are several,
# so no stage is invented for them (user decision C6, 2026-10-28). One row per (tech, sub_sector, hs6).
sector_trade_excluded <- function(hs6_ref, techs) {
  ex <- hs6_ref[as.logical(hs6_ref$excluded) %in% TRUE, , drop = FALSE]
  out <- tibble::tibble(
    tech = as.character(ex$tech),
    sub_sector = as.character(ex$sub_sector),
    hs6 = formatC(as.integer(ex$hs6), width = 6, flag = "0"),
    reason = sector_six_stage_text(dplyr::coalesce(as.character(ex$note), "excluded"))
  )
  out <- out[out$tech %in% techs, , drop = FALSE]
  out <- out[!duplicated(out[c("tech", "sub_sector", "hs6")]), , drop = FALSE]
  out[order(match(out$tech, SECTOR_TECH_LABELS), out$hs6, out$sub_sector, method = "radix"), , drop = FALSE]
}

# Codes the reference master marks excluded that are still in the pipeline basket where the reference
# master excluded them (scripts/04 was not rerun): returns them; the exporter stops on any. The
# excluded rows are keyed on the master's former supply_chain column (they have no six-stage stage), so
# the basket's rows are matched on the same column (`listed_as`); a basket without it (staged by hand)
# is matched per technology.
sector_trade_excluded_in_basket <- function(basket, hs6_ref) {
  ex <- hs6_ref[as.logical(hs6_ref$excluded) %in% TRUE, , drop = FALSE]
  code <- formatC(as.integer(ex$hs6), width = 6, flag = "0")
  listed <- if ("listed_as" %in% names(basket)) basket$listed_as else rep(NA_character_, nrow(basket))
  by_cell <- !is.na(listed)
  k_b <- ifelse(by_cell, paste(basket$tech, listed, basket$hs6), paste(basket$tech, basket$hs6))
  k_ex <- c(paste(ex$tech, ex$supply_chain, code), paste(ex$tech, code))
  # A code excluded from one cell can legitimately stay in another; only the same cell counts.
  unique(k_b[k_b %in% k_ex])
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
                              value = ex$value, status = as.character(ifelse(ex$value == 0, "zero", "obs")))
    im_rows <- tibble::tibble(var = "trade.imports", item_key = paste0("cell:", im$cell), geo = im$geo, year = as.integer(y),
                              value = im$value, status = as.character(ifelse(im$value == 0, "zero", im_status)))
    both <- merge(ex, im, by = c("cell", "geo"), suffixes = c("_x", "_m"))
    net_status <- ifelse(paste0("cell:", both$cell, " ", both$geo, " ", y) %in% dec, "obs", "mirror")
    net <- both$value_x - both$value_m
    net_rows <- tibble::tibble(var = "trade.net", item_key = paste0("cell:", both$cell), geo = both$geo, year = as.integer(y),
                               value = net, status = as.character(ifelse(net == 0, "zero", net_status)))
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

# ---- Trade by sub-component (trade.cty_sub, trade.sub_intra, trade.sub_summary) ---------------------
#
# A cell's sub-components are the sub_sector groups of its HS6 basket (sector_trade_basket() stops
# when a code sits in two sub-sectors of one cell, so they partition the cell's codes). Their values
# follow the cell's own source rule, decided per (importer, cell, year) as in
# partnership_strength_build_trade_cells(): when the importer declared imports of the CELL that year
# (trade.declared), its declarations are used (summed over the sub-component's codes); otherwise the
# exporter's declarations x cif_fob_factor. So the sub-components of a cell add up to the cell's
# matrix value for every pair, and to its trade.exports / trade.imports facts for every economy.

# Pair-level sub-component flows aggregated to economies. std: the standardised dyads
# (partnership_strength_standardize_comtrade()); basket: sector_trade_basket(); declared:
# sector_trade_declared(). One cell at a time (only that cell's rows are copied, so memory stays near
# the size of the largest cell): the declarations of a (pair, year, sub-component) are summed first and
# the exporter's then multiplied by cif_fob_factor, as the matrix does for the cell. Returns
# list(country = (item_key, sub_sector, geo, year, exports, imports; NA where the economy has no flow on
# that side), intra = (item_key, sub_sector, year, intra), world = (item_key, sub_sector, year, total)).
sector_trade_sub <- function(std, basket, declared, eu_members = character(), cif_fob_factor = 1.06,
                             years = SECTOR_TRADE_YEARS) {
  map <- unique(tibble::tibble(cmd_code = basket$hs6, cell = basket$cell, sub_sector = basket$sub_sector))
  imp_keys <- paste(declared$item_key, declared$geo, declared$year)[declared$imports_declared]
  in_years <- std$period %in% years
  total <- function(x, by, name) {
    a <- dplyr::summarise(dplyr::group_by(x, dplyr::across(dplyr::all_of(by))), v = sum(value), .groups = "drop")
    names(a)[names(a) == "v"] <- name
    a
  }
  country <- list()
  intra <- list()
  world <- list()
  for (cl in sort(unique(map$cell))) {
    m <- map[map$cell == cl, , drop = FALSE]
    r <- which(in_years & std$cmd_code %in% m$cmd_code)
    if (!length(r)) next
    own <- std$flow_direction[r] == "import"
    d <- tibble::tibble(
      exporter = ifelse(own, std$partner_iso[r], std$reporter_iso[r]),
      importer = ifelse(own, std$reporter_iso[r], std$partner_iso[r]),
      year = as.integer(std$period[r]), sub_sector = m$sub_sector[match(std$cmd_code[r], m$cmd_code)],
      own = own, value = as.double(std$primary_value[r])
    )
    # The cell's source rule: the importer's declarations when it declared the cell that year, else
    # the exporter's declarations x cif_fob_factor.
    d <- d[d$own | !paste0("cell:", cl, " ", d$importer, " ", d$year) %in% imp_keys, , drop = FALSE]
    fl <- dplyr::summarise(dplyr::group_by(d, exporter, importer, year, sub_sector, own), value = sum(value), .groups = "drop")
    rm(d)
    fl$value <- ifelse(fl$own, fl$value, fl$value * cif_fob_factor)
    ex <- total(dplyr::rename(fl, geo = exporter), c("sub_sector", "geo", "year"), "exports")
    im <- total(dplyr::rename(fl, geo = importer), c("sub_sector", "geo", "year"), "imports")
    cty <- dplyr::full_join(ex, im, by = c("sub_sector", "geo", "year"))
    eu <- total(fl[fl$exporter %in% eu_members & fl$importer %in% eu_members, , drop = FALSE], c("sub_sector", "year"), "intra")
    w <- total(fl, c("sub_sector", "year"), "total")
    country[[cl]] <- tibble::tibble(item_key = paste0("cell:", cl), sub_sector = cty$sub_sector, geo = cty$geo,
                                    year = as.integer(cty$year), exports = as.double(cty$exports), imports = as.double(cty$imports))
    intra[[cl]] <- tibble::tibble(item_key = rep(paste0("cell:", cl), nrow(eu)), sub_sector = eu$sub_sector,
                                  year = as.integer(eu$year), intra = as.double(eu$intra))
    world[[cl]] <- tibble::tibble(item_key = paste0("cell:", cl), sub_sector = w$sub_sector, year = as.integer(w$year),
                                  total = as.double(w$total))
  }
  empty <- list(
    country = tibble::tibble(item_key = character(), sub_sector = character(), geo = character(), year = integer(),
                             exports = double(), imports = double()),
    intra = tibble::tibble(item_key = character(), sub_sector = character(), year = integer(), intra = double()),
    world = tibble::tibble(item_key = character(), sub_sector = character(), year = integer(), total = double())
  )
  list(country = dplyr::bind_rows(c(list(empty$country), unname(country))),
       intra = dplyr::bind_rows(c(list(empty$intra), unname(intra))),
       world = dplyr::bind_rows(c(list(empty$world), unname(world))))
}

# trade.cty_sub rows from sector_trade_sub(): the side an economy has no flow on reads 0 when its cell
# total on that side was observed that year (a trade.exports / trade.imports fact exists in
# `country_facts`, the rows of sector_trade_country()) and NaN otherwise; rows where neither side is
# above 0 are dropped; WLD rows carry the world total on both sides. Sorted by (item_key, sub_sector,
# geo, year).
sector_trade_sub_table <- function(sub, country_facts) {
  ck <- function(v) paste(country_facts$item_key, country_facts$geo, country_facts$year)[country_facts$var == v]
  k <- paste(sub$country$item_key, sub$country$geo, sub$country$year)
  x <- sub$country$exports
  m <- sub$country$imports
  x[is.na(x)] <- ifelse(k[is.na(x)] %in% ck("trade.exports"), 0, NaN)
  m[is.na(m)] <- ifelse(k[is.na(m)] %in% ck("trade.imports"), 0, NaN)
  keep <- (!is.na(x) & x > 0) | (!is.na(m) & m > 0)
  out <- tibble::tibble(item_key = sub$country$item_key[keep], sub_sector = sub$country$sub_sector[keep], geo = sub$country$geo[keep],
                        year = sub$country$year[keep], exports = x[keep], imports = m[keep])
  w <- sub$world[sub$world$total > 0, , drop = FALSE]
  out <- dplyr::bind_rows(out, tibble::tibble(item_key = w$item_key, sub_sector = w$sub_sector, geo = "WLD", year = w$year,
                                              exports = w$total, imports = w$total))
  out[order(out$item_key, out$sub_sector, out$geo, out$year, method = "radix"), , drop = FALSE]
}

# The sub-components add up to the cell: for every (cell, economy, year) and side, the sum over
# sub-components of trade.cty_sub equals the trade.exports / trade.imports fact (relative difference;
# a fact with no sub-component row must be 0, a sub-component row needs a fact), and the WLD rows add
# up to the cell's world total. Returns list(n, max_rel, n_unmatched, examples).
sector_trade_sub_check <- function(cty_sub, country_facts, world) {
  out <- list(n = 0L, max_rel = 0, n_unmatched = 0L, examples = character())
  note <- function(msg) if (length(out$examples) < 5) out$examples <<- c(out$examples, msg)
  cs <- cty_sub[cty_sub$geo != "WLD", , drop = FALSE]
  for (side in c("exports", "imports")) {
    v <- paste0("trade.", side)
    f <- country_facts[country_facts$var == v & country_facts$item_key %in% unique(cty_sub$item_key), , drop = FALSE]
    val <- cs[[side]]
    ok <- !is.na(val)
    s <- tapply(val[ok], paste(cs$item_key, cs$geo, cs$year)[ok], sum)
    fk <- paste(f$item_key, f$geo, f$year)
    got <- unname(s[fk])
    got[is.na(got)] <- 0
    # Relative to the fact; a zero fact must have no sub-component value at all.
    rel <- ifelse(f$value != 0, abs(got - f$value) / abs(f$value), abs(got))
    out$n <- out$n + length(rel)
    out$max_rel <- max(out$max_rel, rel)
    if (any(rel > 1e-9)) note(paste(side, fk[which.max(rel)]))
    extra <- setdiff(names(s)[s > 0], fk)
    out$n_unmatched <- out$n_unmatched + length(extra)
    if (length(extra)) note(paste(side, "without a fact:", extra[1]))
    w <- cty_sub[cty_sub$geo == "WLD", , drop = FALSE]
    ws <- tapply(w[[side]], paste(w$item_key, w$year), sum)
    wf <- world[world$var == v & world$item_key %in% unique(cty_sub$item_key), , drop = FALSE]
    wk <- paste(wf$item_key, wf$year)
    wgot <- unname(ws[wk])
    wgot[is.na(wgot)] <- 0
    wrel <- ifelse(wf$value != 0, abs(wgot - wf$value) / abs(wf$value), abs(wgot))
    out$n <- out$n + length(wrel)
    out$max_rel <- max(out$max_rel, wrel)
    if (any(wrel > 1e-9)) note(paste(side, "world", wk[which.max(wrel)]))
  }
  out
}

# trade.sub_summary: sector_summary() (the meta.summary rules) per sub-component, on the trade.cty_sub
# rows of the cells in `cty_sub` (measures trade.sub_exports / trade.sub_imports, W = the WLD row,
# intra-EU flows from `intra` in bloc mode). geo: dims.geo; vars: the dictionary vars.
sector_trade_sub_summary <- function(cty_sub, intra, geo, vars) {
  if (is.null(cty_sub) || !nrow(cty_sub)) return(NULL)
  key <- paste0(cty_sub$item_key, "\t", cty_sub$sub_sector)
  cty <- cty_sub$geo != "WLD"
  facts <- dplyr::bind_rows(lapply(c(exports = "trade.sub_exports", imports = "trade.sub_imports"), function(v) {
    side <- if (v == "trade.sub_exports") "exports" else "imports"
    x <- cty_sub[[side]]
    ok <- cty & !is.na(x)
    tibble::tibble(var = v, item_key = key[ok], geo = cty_sub$geo[ok], year = cty_sub$year[ok], value = x[ok], status = "obs")
  }))
  w <- !cty
  world <- dplyr::bind_rows(
    tibble::tibble(var = "trade.sub_exports", item_key = key[w], year = cty_sub$year[w], value = cty_sub$exports[w], basis = "sum_all"),
    tibble::tibble(var = "trade.sub_imports", item_key = key[w], year = cty_sub$year[w], value = cty_sub$imports[w], basis = "sum_all")
  )
  eu_intra <- if (is.null(intra) || !nrow(intra)) NULL else
    tibble::tibble(item_key = paste0(intra$item_key, "\t", intra$sub_sector), year = intra$year, intra = intra$intra)
  s <- sector_summary(facts, world, geo, vars, eu_intra)
  if (!nrow(s)) return(NULL)
  parts <- strsplit(s$item_key, "\t", fixed = TRUE)
  s$sub_sector <- vapply(parts, `[`, "", 2)
  s$item_key <- vapply(parts, `[`, "", 1)
  s[order(s$item_key, s$sub_sector, s$var, s$year, s$eu_mode, method = "radix"), , drop = FALSE]
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
