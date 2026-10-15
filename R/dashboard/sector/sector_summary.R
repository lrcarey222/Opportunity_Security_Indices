# Concentration summaries, coverage, availability and headlines of the Sector Insights bundle
# (spec_sector.md 3.6, 8.8). Pure. src/sector/derive/concentration.ts mirrors sector_concentration()
# and must agree with meta.summary within 1e-12.
#
# Rules (also the client's):
#   * Holders of a (var, item, year) are the facts rows whose geo kind matches the measure's native
#     level (country -> country, region -> region, entity -> entity) with a finite value and a status
#     other than nq / na_growth. Residual rows (kind residual) are never holders.
#   * EU bloc mode (country measures): when the slice has a row of kind eu (a source's own EU row,
#     EUU) it replaces the 27 members; otherwise EUU = the sum of the members' values. For trade
#     (trade.exports, trade.imports) the intra-EU flows are removed first: EUU = sum(members) - intra
#     and W = W - intra, where intra = the sum of flows between two EU members (trade chunk).
#   * W = facts.world (var, item, year) when present (basis from the row), else the sum of the
#     holders (basis sum_listed).
#   * share_i = x_i / W; unattributed = max(0, 1 - sum(share)); hhi = sum(share^2), a lower bound
#     ("at least") when unattributed > 0.05; eff_n = 1 / hhi; n = holders with x > 0; top-1 and top-3
#     by share, ties broken by the dims.geo row order; group split by dims.geo `group` (EUU: core).
#     A holder in group "na" (an IEA EV region whose members span groups) is ranked like any holder,
#     but its share counts as unattributed in the group split; hhi_lower_bound still reads only the
#     source's own residual (1 - sum(share)).
#   * growth (on the last year's row, same eu mode): pct_2020_2024 = W2024 / W2020 - 1 (trade);
#     cagr_2018_2025 = (W2025 / W2018)^(1/7) - 1 (CIM investment). NA when the base is zero or below
#     1% of the current value.

SECTOR_GROWTH_RULES <- tibble::tribble(
  ~var, ~growth_kind, ~y0, ~y1,
  "trade.exports", "pct_2020_2024", 2020L, 2024L,
  "trade.imports", "pct_2020_2024", 2020L, 2024L,
  "investment.annual", "cagr_2018_2025", 2018L, 2025L,
  # trade.sub_summary (sector_trade_sub_summary(): the same rules per sub-component of a cell)
  "trade.sub_exports", "pct_2020_2024", 2020L, 2024L,
  "trade.sub_imports", "pct_2020_2024", 2020L, 2024L
)
SECTOR_EU_TRADE_VARS <- c("trade.exports", "trade.imports", "trade.sub_exports", "trade.sub_imports")
SECTOR_SUMMARY_LEVEL_KIND <- c(country = "country", region = "region", entity = "entity")

# Concentration of one slice. x: holder values (>= 0, finite), in dims.geo order; geo_idx: their
# dims.geo row numbers (tie-break); groups: their groups; W: world total (NA -> sum(x)).
sector_concentration <- function(x, geo_idx, groups, W = NA_real_) {
  x <- as.double(x)
  basis_sum <- is.na(W)
  if (basis_sum) W <- sum(x)
  out <- list(W = W, n = sum(x > 0), top1 = NA_integer_, top1_share = NA_real_, top3_share = NA_real_,
              hhi = NA_real_, hhi_lower_bound = NA, eff_n = NA_real_, china = NA_real_, core = NA_real_,
              partner = NA_real_, rest = NA_real_, unattributed = NA_real_, clamped = 0)
  if (!length(x) || !is.finite(W) || W <= 0) return(out)
  share <- x / W
  ord <- order(-share, geo_idx)
  s_sorted <- share[ord]
  total <- sum(share)
  unatt <- 1 - total
  out$clamped <- if (unatt < 0) -unatt else 0
  unatt <- max(0, unatt)
  out$hhi <- sum(share^2)
  out$eff_n <- 1 / out$hhi
  out$hhi_lower_bound <- unatt > 0.05
  out$top1 <- as.integer(geo_idx[ord[1]])
  out$top1_share <- s_sorted[1]
  out$top3_share <- sum(s_sorted[seq_len(min(3, length(s_sorted)))])
  grp <- function(g) sum(share[groups == g])
  out$china <- grp("china")
  out$core <- grp("core")
  out$partner <- grp("partner")
  out$rest <- grp("rest")
  out$unattributed <- unatt + grp("na")
  out
}

# The holders of one slice for an eu mode. rows: facts of one (var, item, year) joined to geo
# (geo_idx, kind, group, eu_member); level: the measure's native level. eu_value: for trade in bloc
# mode, the EU value (sum of members - intra); NULL = sum of members. Returns a data frame
# (geo_idx, value, group) in dims.geo order, or NULL when the level has no summary.
sector_slice_holders <- function(rows, level, eu_mode, eu_idx, eu_value = NULL) {
  kind <- unname(SECTOR_SUMMARY_LEVEL_KIND[level])
  if (is.na(kind)) return(NULL)
  ok <- is.finite(rows$value) & !rows$status %in% SECTOR_NAN_STATUS
  h <- rows[ok & rows$kind == kind, c("geo_idx", "value", "group", "eu_member"), drop = FALSE]
  if (identical(kind, "country") && identical(eu_mode, "bloc")) {
    own <- rows[ok & rows$kind == "eu", , drop = FALSE]
    members <- h[h$eu_member %in% TRUE, , drop = FALSE]
    h <- h[!(h$eu_member %in% TRUE), , drop = FALSE]
    if (nrow(own)) {
      h <- rbind(h, data.frame(geo_idx = own$geo_idx[1], value = own$value[1], group = "core", eu_member = FALSE))
    } else if (nrow(members)) {
      v <- if (is.null(eu_value)) sum(members$value) else eu_value
      h <- rbind(h, data.frame(geo_idx = eu_idx, value = v, group = "core", eu_member = FALSE))
    }
  }
  h <- h[order(h$geo_idx), , drop = FALSE]
  h
}

# meta.summary. facts: (var, item_key, geo, year, value, status); world: (var, item_key, year, value,
# basis); geo: dims.geo (code, kind, group, eu_member) in its final order; vars: dictionary vars;
# eu_intra: (item_key, year, intra) for the trade cells (NULL = none). Returns one row per
# (item_key, var, year, eu_mode) of every additive measure with a world basis, plus `clamped`.
sector_summary <- function(facts, world, geo, vars, eu_intra = NULL) {
  geo_idx <- stats::setNames(seq_len(nrow(geo)), geo$code)
  eu_idx <- unname(geo_idx["EUU"])
  elig <- vars$id[vars$agg == "sum" & vars$world_basis != "none" & vars$native_level %in% names(SECTOR_SUMMARY_LEVEL_KIND)]
  f <- facts[facts$var %in% elig, , drop = FALSE]
  if (!nrow(f)) return(sector_summary_empty())
  gi <- geo_idx[f$geo]
  if (anyNA(gi)) stop("sector_summary(): facts geo not in dims.geo: ", paste(utils::head(unique(f$geo[is.na(gi)]), 5), collapse = ", "))
  f$geo_idx <- unname(gi)
  f$kind <- geo$kind[f$geo_idx]
  f$group <- geo$group[f$geo_idx]
  f$eu_member <- geo$eu_member[f$geo_idx]
  key <- paste(f$var, f$item_key, f$year, sep = "\r")
  wkey <- paste(world$var, world$item_key, world$year, sep = "\r")
  slices <- split(seq_len(nrow(f)), key)
  rows <- vector("list", length(slices) * 2L)
  k <- 0L
  for (s in names(slices)) {
    r <- f[slices[[s]], , drop = FALSE]
    v <- r$var[1]
    level <- vars$native_level[match(v, vars$id)]
    wi <- match(s, wkey)
    W <- if (is.na(wi)) NA_real_ else world$value[wi]
    basis <- if (is.na(wi)) "sum_listed" else world$basis[wi]
    for (mode in c("members", "bloc")) {
      W_m <- W
      eu_value <- NULL
      if (identical(mode, "bloc") && v %in% SECTOR_EU_TRADE_VARS && !is.null(eu_intra)) {
        ii <- which(eu_intra$item_key == r$item_key[1] & eu_intra$year == r$year[1])
        intra <- if (length(ii)) eu_intra$intra[ii[1]] else 0
        mem <- r$eu_member %in% TRUE & r$kind == "country" & is.finite(r$value)
        eu_value <- sum(r$value[mem]) - intra
        if (!is.na(W_m)) W_m <- W_m - intra
      }
      h <- sector_slice_holders(r, level, mode, eu_idx, eu_value)
      if (is.null(h)) next
      cc <- sector_concentration(h$value, h$geo_idx, h$group, W_m)
      k <- k + 1L
      rows[[k]] <- tibble::tibble(
        var = v, item_key = r$item_key[1], year = as.integer(r$year[1]), eu_mode = mode,
        W = cc$W, basis = basis, n = as.integer(cc$n), top1_geo = cc$top1, top1_share = cc$top1_share,
        top3_share = cc$top3_share, hhi = cc$hhi, hhi_lower_bound = cc$hhi_lower_bound, eff_n = cc$eff_n,
        china = cc$china, core = cc$core, partner = cc$partner, rest = cc$rest, unattributed = cc$unattributed,
        clamped = cc$clamped
      )
    }
  }
  out <- dplyr::bind_rows(rows[seq_len(k)])
  if (!nrow(out)) return(sector_summary_empty())
  out$growth <- NA_real_
  out$growth_kind <- NA_character_
  for (i in seq_len(nrow(SECTOR_GROWTH_RULES))) {
    g <- SECTOR_GROWTH_RULES[i, ]
    last <- which(out$var == g$var & out$year == g$y1)
    for (j in last) {
      b <- which(out$var == g$var & out$year == g$y0 & out$item_key == out$item_key[j] & out$eu_mode == out$eu_mode[j])
      out$growth_kind[j] <- g$growth_kind
      if (length(b)) {
        w0 <- out$W[b[1]]
        w1 <- out$W[j]
        if (is.finite(w0) && is.finite(w1) && w0 > 0 && w0 >= 0.01 * w1) {
          out$growth[j] <- if (startsWith(g$growth_kind, "cagr")) (w1 / w0)^(1 / (g$y1 - g$y0)) - 1 else w1 / w0 - 1
        }
      }
    }
  }
  out
}

sector_summary_empty <- function() {
  tibble::tibble(var = character(), item_key = character(), year = integer(), eu_mode = character(), W = double(),
                 basis = character(), n = integer(), top1_geo = integer(), top1_share = double(), top3_share = double(),
                 hhi = double(), hhi_lower_bound = logical(), eff_n = double(), china = double(), core = double(),
                 partner = double(), rest = double(), unattributed = double(), clamped = double(),
                 growth = double(), growth_kind = character())
}

# Which measures each cell shows (dict.cells.section_cards): every measure with item axis cell that
# has data for the cell, plus measures on other axes whose link_stage matches the cell's stage (or
# is "any") and that have data for an item linked to the cell. Ordered by the dictionary.
# Returns tibble(cell, var) .
sector_cell_vars <- function(cells, vars, data_keys, item_links) {
  # data_keys: distinct (var, item_key) with data in any chunk; item_links: (item_key, cell)
  out <- list()
  for (i in seq_len(nrow(cells))) {
    cs <- cells$slug[i]
    stage <- cells$stage[i]
    linked <- item_links$item_key[item_links$cell == cs]
    for (j in seq_len(nrow(vars))) {
      v <- vars$id[j]
      ax <- vars$item_axis[j]
      have <- data_keys$item_key[data_keys$var == v]
      hit <- if (ax == "cell") paste0("cell:", cs) %in% have
             else if (ax == "none") !is.na(vars$link_stage[j]) && length(have) > 0
             else {
               ls <- vars$link_stage[j]
               !is.na(ls) && (ls == "any" || ls == stage) && any(linked %in% have)
             }
      if (isTRUE(hit)) out[[length(out) + 1]] <- tibble::tibble(cell = cs, var = v)
    }
  }
  if (!length(out)) return(tibble::tibble(cell = character(), var = character()))
  dplyr::bind_rows(out)
}

# meta.coverage: one row per (item, family) for the cells and minerals. status: country (country
# rows), region (region / entity / eu rows only), global (world rows only), licensed, none.
# data: (var, item_key, geo_kind, geo) with data; licensed: (item_key, family, sources) of
# restricted measures not built. Returns tibble(item_key, family, status, n_geo, reason, sources).
sector_coverage <- function(items, vars, cell_vars, data, item_links, sources, licensed = NULL) {
  fams <- setdiff(SECTOR_FAMILIES, "development")
  rows <- list()
  src_short <- stats::setNames(sources$short, sources$id)
  for (i in seq_len(nrow(items))) {
    if (!items$axis[i] %in% c("cell", "mineral")) next
    ik <- items$item_key[i]
    if (items$axis[i] == "cell") {
      vs <- cell_vars$var[cell_vars$cell == items$slug[i]]
      keys <- c(ik, item_links$item_key[item_links$cell == items$slug[i]])
    } else {
      vs <- vars$id[vars$item_axis == "mineral"]
      keys <- ik
    }
    for (fam in fams) {
      fv <- vs[vars$family[match(vs, vars$id)] == fam]
      d <- data[data$var %in% fv & data$item_key %in% keys, , drop = FALSE]
      # mineral_level: the cell's data in this family exist only per linked mineral (clean-tech
      # Upstream); data through other links (fuels, commodities, policy tags) are the cell's own.
      mineral_level <- items$axis[i] == "cell" && nrow(d) > 0 && all(startsWith(d$item_key, "mineral:"))
      lic <- if (!is.null(licensed)) licensed[licensed$item_key == ik & licensed$family == fam, , drop = FALSE] else NULL
      if (any(d$geo_kind == "country")) {
        status <- "country"
        n_geo <- length(unique(d$geo[d$geo_kind == "country"]))
      } else if (any(d$geo_kind %in% c("region", "entity", "eu"))) {
        status <- "region"
        n_geo <- length(unique(d$geo[d$geo_kind %in% c("region", "entity", "eu")]))
      } else if (nrow(d)) {
        status <- "global"
        n_geo <- 0L
      } else if (!is.null(lic) && nrow(lic)) {
        status <- "licensed"
        n_geo <- 0L
      } else {
        status <- "none"
        n_geo <- 0L
      }
      reason <- if (status == "none") {
        if (fam == "exports" && items$axis[i] == "cell") "no_basket"
        else if (fam == "reserves" && items$axis[i] == "cell" && items$stage[i] %in% "Midstream") "not_applicable"
        else "no_source"
      } else if (status == "licensed") "licensed"
      else if (mineral_level) "mineral_level"
      else NA_character_
      srcs <- unique(src_short[vars$source[match(unique(d$var), vars$id)]])
      if (status == "licensed") srcs <- lic$sources
      rows[[length(rows) + 1]] <- tibble::tibble(item_key = ik, family = fam, status = status, n_geo = as.integer(n_geo),
                                                 reason = reason, sources = if (length(srcs)) paste(srcs, collapse = "|") else NA_character_)
    }
  }
  dplyr::bind_rows(rows)
}

# meta.avail: per (var, item_key, chunk) the year range and the number of country-kind economies.
# data: (var, item_key, chunk, year, geo, geo_kind).
sector_avail <- function(data) {
  if (!nrow(data)) {
    return(tibble::tibble(var = character(), item_key = character(), chunk = character(), year_min = integer(),
                          year_max = integer(), n_years = integer(), n_geo = integer()))
  }
  key <- paste(data$var, data$item_key, data$chunk, sep = "\r")
  parts <- split(seq_len(nrow(data)), key)
  dplyr::bind_rows(lapply(parts, function(ix) {
    y <- data$year[ix]
    y <- y[!is.na(y)]
    g <- data$geo[ix][data$geo_kind[ix] %in% "country"]
    tibble::tibble(
      var = data$var[ix[1]], item_key = data$item_key[ix[1]], chunk = data$chunk[ix[1]],
      year_min = if (length(y)) as.integer(min(y)) else NA_integer_,
      year_max = if (length(y)) as.integer(max(y)) else NA_integer_,
      n_years = as.integer(length(unique(y))),
      n_geo = as.integer(length(unique(g)))
    )
  }))
}

# The 33 headline cells' glyph: country_data = at least one lens family (SECTOR_LENS_FAMILIES) with
# country data. Policy and context are country-level for every cell and do not count (spec 3.3).
sector_coverage_glyph <- function(items, coverage) {
  cells <- items[items$axis == "cell" & items$headline, , drop = FALSE]
  lapply(seq_len(nrow(cells)), function(i) {
    cov <- coverage[coverage$item_key == cells$item_key[i] & coverage$family %in% SECTOR_LENS_FAMILIES, , drop = FALSE]
    list(tech = sub(paste0("-", tolower(cells$stage[i]), "$"), "", cells$slug[i]), stage = tolower(cells$stage[i]),
         country_data = any(cov$status == "country"))
  })
}

# ---- Headlines (manifest.headlines; contract.ts HEADLINE_RULES) -------------------------------------

# A label in running text: the first letter lower-cased unless the first word is an acronym
# ("Natural gas, EU (TTF)" -> "natural gas, EU (TTF)"; "PGMs" stays).
sector_lower_first <- function(x) {
  ifelse(!is.na(x) & grepl("^[A-Z][a-z]", x), paste0(tolower(substr(x, 1, 1)), substring(x, 2)), x)
}

# The running-text name of an item for a headline ("battery-grade graphite" for graphite refining).
sector_item_text <- function(items, item_key, context = NULL) {
  i <- match(item_key, sector_item_key(items$axis, items$slug))
  txt <- if ("text" %in% names(items)) items$text[i] else sector_lower_first(items$label[i])
  if (identical(context, "refining") && identical(items$slug[i], "graphite")) txt <- "battery-grade graphite"
  # The IEA's rare-earth blocks are the magnet rare earths only (Nd, Pr, Dy, Tb).
  if (isTRUE(context %in% c("refining", "mining")) && identical(items$slug[i], "rare-earths")) txt <- "magnet rare earths"
  txt
}

# manifest.headlines: a list of headline lists for the rules that find data (contract.ts
# HEADLINE_RULES: exactly these values / labels keys). summary: sector_summary() rows with item_key,
# var and top1_geo as a dims.geo row number (1-based); facts: long facts; geo: dims.geo; items:
# dims.items (+ text); vars: dictionary vars; pams: policy.pams (geo codes); src: named source
# strings by rule.
sector_headlines <- function(summary, facts, geo, items, vars, pams = NULL, src = list()) {
  out <- list()
  slug_of <- function(item_key) sub("^[a-z_]+:", "", item_key)
  geo_name <- function(code) geo$name[match(code, geo$code)]
  hl <- function(id, values, labels, source, year, href) {
    list(id = id, template = paste0(id, "@1"), values = values, labels = labels,
         source = if (is.null(source)) "" else source, year = year, href = href)
  }
  sm <- summary[summary$eu_mode == "members", , drop = FALSE]
  latest <- function(s) {
    if (!nrow(s)) return(s)
    mx <- stats::ave(s$year, paste(s$var, s$item_key), FUN = max)
    s[s$year == mx, , drop = FALSE]
  }

  # supply@1: IEA refining 2025, the mineral whose top-1 has the largest share of the IEA total.
  s <- sm[sm$var == "production.refining" & sm$year == 2025 & !is.na(sm$top1_share), , drop = FALSE]
  if (nrow(s)) {
    j <- order(-s$top1_share, s$item_key)[1]
    g <- geo$code[s$top1_geo[j]]
    f <- facts[facts$var == "production.refining" & facts$item_key == s$item_key[j] & facts$year == 2025 & facts$geo == g, , drop = FALSE]
    out[[length(out) + 1]] <- hl("supply",
      values = list(share = s$top1_share[j], value_kt = f$value[1], total_kt = s$W[j]),
      labels = list(geo = g, geo_name = geo_name(g), item = slug_of(s$item_key[j]),
                    item_text = sector_item_text(items, s$item_key[j], "refining")),
      source = src$supply, year = "2025", href = paste0("/sector/minerals/", slug_of(s$item_key[j])))
  }

  # concentration@1: cell items, latest year, each cell's primary var per family with country data.
  cells <- items[items$axis == "cell", , drop = FALSE]
  ck <- sector_item_key("cell", cells$slug)
  s <- latest(sm[sm$item_key %in% ck, , drop = FALSE])
  if (nrow(s)) {
    s$family <- vars$family[match(s$var, vars$id)]
    s$level <- vars$native_level[match(s$var, vars$id)]
    s <- s[s$level == "country", , drop = FALSE]
    s$ord <- match(s$var, vars$id)
    s <- s[order(s$item_key, s$family, s$ord), , drop = FALSE]
    s <- s[!duplicated(s[c("item_key", "family")]), , drop = FALSE]
    if (nrow(s)) {
      out[[length(out) + 1]] <- hl("concentration",
        values = list(n = sum(s$top1_share >= 0.5, na.rm = TRUE), n_total = nrow(s)), labels = structure(list(), names = character()),
        source = src$concentration, year = "latest", href = "/sector/concentration?lens=top1")
    }
  }

  # dependence@1: the headline Midstream trade cell with the largest 2024 W; China's share.
  mid <- cells$slug[cells$stage == "Midstream" & cells$headline]
  s <- sm[sm$var == "trade.exports" & sm$year == 2024 & sm$item_key %in% sector_item_key("cell", mid), , drop = FALSE]
  if (nrow(s)) {
    j <- order(-s$W, s$item_key)[1]
    cs <- slug_of(s$item_key[j])
    ci <- match(cs, cells$slug)
    out[[length(out) + 1]] <- hl("dependence",
      values = list(share = s$china[j], total_usd = s$W[j]),
      labels = list(cell = cs, cell_label = paste0(cells$tech[ci], " — ", cells$stage[ci])),
      source = src$dependence, year = "2024", href = paste0("/sector/", sub("-midstream$", "", cs), "/midstream#trade"))
  }

  # investment@1: investment.annual 2025 over every cell and economy.
  f <- facts[facts$var == "investment.annual" & facts$year == 2025, , drop = FALSE]
  f <- f[geo$kind[match(f$geo, geo$code)] == "country" & is.finite(f$value), , drop = FALSE]
  if (nrow(f) && sum(f$value) > 0) {
    grp <- geo$group[match(f$geo, geo$code)]
    tot <- sum(f$value)
    out[[length(out) + 1]] <- hl("investment",
      values = list(total_bn = tot, china = sum(f$value[grp == "china"]) / tot, coalition = sum(f$value[grp %in% c("core", "partner")]) / tot),
      labels = structure(list(), names = character()), source = src$investment, year = "2025", href = "/sector/explore/investment")
  }

  # reserves@1: USGS reserves, the mineral whose top-1 holder has the largest share of the world.
  s <- latest(sm[sm$var == "resources.mineral_reserves" & !is.na(sm$top1_share), , drop = FALSE])
  if (nrow(s)) {
    j <- order(-s$top1_share, s$item_key)[1]
    g <- geo$code[s$top1_geo[j]]
    it <- slug_of(s$item_key[j])
    out[[length(out) + 1]] <- hl("reserves",
      values = list(share = s$top1_share[j]),
      labels = list(geo = g, geo_name = geo_name(g), item = it, item_text = sector_item_text(items, s$item_key[j])),
      source = src$reserves, year = "end-2025",
      href = paste0("/sector/explore/resources?m=resources.mineral_reserves&item=", it))
  }

  # energy@1: consumption.balance, gas, 2025, countries: largest net exporter and importer.
  f <- facts[facts$var == "consumption.balance" & facts$item_key == "fuel:gas" & facts$year == 2025 & is.finite(facts$value), , drop = FALSE]
  f <- f[geo$kind[match(f$geo, geo$code)] == "country", , drop = FALSE]
  if (nrow(f) && any(f$value > 0) && any(f$value < 0)) {
    ex <- f[order(-f$value, f$geo)[1], ]
    im <- f[order(f$value, f$geo)[1], ]
    out[[length(out) + 1]] <- hl("energy",
      values = list(exporter_ej = ex$value, importer_ej = -im$value),
      labels = list(exporter = ex$geo, exporter_name = geo_name(ex$geo), importer = im$geo, importer_name = geo_name(im$geo)),
      source = src$energy, year = "2025", href = "/sector/explore/consumption?m=consumption.balance&item=gas")
  }

  # policy@1: distinct PAMS records with a technology tag whose PAMS year is 2026-2030. A record
  # listed for several jurisdictions (an EU directive copied to the 27 members and the EU) counts
  # once: `record` is the PAMS record before the country expansion (not shipped); without it, rows.
  if (!is.null(pams) && nrow(pams)) {
    sel <- pams$tech_mask != 0L & !is.na(pams$year) & pams$year >= 2026 & pams$year <= 2030
    n <- if ("record" %in% names(pams)) length(unique(pams$record[sel])) else sum(sel)
    if (n > 0) {
      out[[length(out) + 1]] <- hl("policy", values = list(n = n), labels = structure(list(), names = character()),
                                   source = src$policy, year = "2026-2030", href = "/sector/explore/policy")
    }
  }

  # prices@1: the commodity with the largest 5-year volatility.
  f <- facts[facts$var == "prices.vol_5y" & is.finite(facts$value), , drop = FALSE]
  if (nrow(f)) {
    j <- order(-f$value, f$item_key)[1]
    out[[length(out) + 1]] <- hl("prices", values = list(vol = f$value[j]),
      labels = list(item = slug_of(f$item_key[j]), item_text = sector_item_text(items, f$item_key[j])),
      source = src$prices, year = "2021-2025", href = "/sector/explore/prices")
  }
  out
}

# facts.world rows for every summarised slice that has none: sum_all and sum_listed measures get the
# sum of their holders (members mode), `one` measures get 1. A source_world measure without a world
# row falls back to the sum of its holders with basis sum_listed and is reported in `missing`.
# Returns list(world, missing).
sector_complete_world <- function(facts, world, vars, geo) {
  elig <- vars[vars$agg == "sum" & vars$world_basis != "none" & vars$native_level %in% names(SECTOR_SUMMARY_LEVEL_KIND), , drop = FALSE]
  f <- facts[facts$var %in% elig$id, , drop = FALSE]
  if (!nrow(f)) return(list(world = world, missing = character()))
  f$kind <- geo$kind[match(f$geo, geo$code)]
  f$level_kind <- unname(SECTOR_SUMMARY_LEVEL_KIND[elig$native_level[match(f$var, elig$id)]])
  ok <- f$kind == f$level_kind & is.finite(f$value) & !f$status %in% SECTOR_NAN_STATUS
  key <- paste(f$var, f$item_key, f$year, sep = "\r")
  wkey <- paste(world$var, world$item_key, world$year, sep = "\r")
  need <- unique(key[!key %in% wkey])
  if (!length(need)) return(list(world = world, missing = character()))
  add <- lapply(need, function(k) {
    i <- which(key == k)
    v <- f$var[i[1]]
    basis <- elig$world_basis[match(v, elig$id)]
    val <- if (basis == "one") 1 else sum(f$value[i][ok[i]])
    tibble::tibble(var = v, item_key = f$item_key[i[1]], year = as.integer(f$year[i[1]]), value = as.double(val),
                   basis = if (basis == "source_world") "sum_listed" else basis)
  })
  add <- dplyr::bind_rows(add)
  miss <- need[elig$world_basis[match(add$var, elig$id)] == "source_world"]
  list(world = dplyr::bind_rows(world, add), missing = gsub("\r", " ", miss))
}
