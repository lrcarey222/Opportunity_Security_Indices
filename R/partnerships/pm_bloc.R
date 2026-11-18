# Partnership Memos: a bloc (the European Union) as one side of a pair (docs/partnership_memos.md, "Blocs").
#
# The OSI tables are per economy. A bloc memo (United States-EU) needs the bloc as ONE economy, so these
# functions build synthetic inputs for it from its members, without recomputing the engine:
#
#   node grain (AIB division of labour): the members are merged in the bilateral node flows and intra-bloc
#     trade is dropped; every economy's world export share, allied-demand share and net-import share is then
#     re-based on world trade WITHOUT intra-bloc trade (the bloc is one economy, so its internal trade is not
#     world trade). Production shares add up across members. The bloc's RCA is the export-weighted mean of
#     its members' RCAs (its extra-bloc total exports are not in the inputs). Anchor and specialist flags
#     follow the AIB role rules (config/aib_strategic_layer.yml roles) on the re-based shares.
#   cell grain (six-stage strategic position): Competitive Position is the strongest member's (named);
#     supply-chain need is the extra-bloc-import-weighted mean of the members' needs; deployment need and
#     policy priority are size-weighted means (size = the member's imports over all cells); import exposure
#     (coercive share, China share, top supplier, HHI) is recomputed from the bloc's extra-bloc imports. The
#     engine's choice, evidence quality and posture fields are the lead member's (largest extra-bloc importer).
#   bilateral trade, development-partnership donors, compact and strategic roles, investment pairs and the
#     evidence layer's country lists are recoded from members to the bloc.
#
# Pure functions: data frames and config lists in, data frames out. No IO.

# Recode members to the bloc code in a "|" or ";"-separated string list (deduplicated, order kept).
pm_bloc_recode_list <- function(x, members, code, sep = "|") {
  vapply(as.character(x), function(s) {
    if (is.na(s) || !nzchar(s)) return(s)
    z <- trimws(strsplit(s, if (sep == "|") "\\|" else sep, fixed = FALSE)[[1]])
    z[z %in% members] <- code
    paste(unique(z[nzchar(z)]), collapse = if (sep == "|") "|" else paste0(sep, " "))
  }, character(1), USE.NAMES = FALSE)
}

# Evidence items naming a member also name the bloc (members are kept, so member-level facts stay visible).
pm_bloc_evidence <- function(ev, members, code) {
  if (is.null(ev) || !nrow(ev)) return(ev)
  ev$countries <- vapply(ev$countries, function(s) {
    z <- strsplit(s, "\\|")[[1]]
    if (any(z %in% members) && !code %in% z) z <- c(z, code)
    paste(z, collapse = "|")
  }, character(1), USE.NAMES = FALSE)
  ev
}

# Node flows with the members merged into one economy and intra-bloc trade dropped.
# flows: node_id, exporter_iso, importer_iso, value (and value_hist when present).
pm_bloc_flows <- function(flows, members, code) {
  f <- flows[, intersect(c("node_id", "exporter_iso", "importer_iso", "value", "value_hist"), names(flows)), drop = FALSE]
  f$exporter_iso[f$exporter_iso %in% members] <- code
  f$importer_iso[f$importer_iso %in% members] <- code
  f <- f[!(f$exporter_iso == code & f$importer_iso == code), , drop = FALSE]
  if (!"value_hist" %in% names(f)) f$value_hist <- NA_real_
  stats::aggregate(cbind(value, value_hist) ~ node_id + exporter_iso + importer_iso, data = f, FUN = sum, na.action = stats::na.pass)
}

# The division-of-labour table re-based on world trade without intra-bloc trade, with one row per node for the
# bloc in place of its members.
#   dol        aib_compact_division_of_labour.csv (all economies)
#   flows      aib_inputs$flows (original, per economy)
#   production aib_inputs$production (node_id, iso3, production_share)
#   rca        aib_inputs$exporter_metrics (node_id, exporter_iso, exports, export_rca)
#   roles      config/aib_strategic_layer.yml `roles` (position_min, spec_rca)
pm_bloc_dol <- function(dol, flows, production, rca, members, code, roles) {
  fl <- flows[flows$node_id %in% dol$node_id, , drop = FALSE]
  # the allied demand scope: economies with an allied-demand share; its size per node from any scope member
  imp0 <- stats::aggregate(value ~ node_id + importer_iso, data = fl, FUN = sum)
  names(imp0) <- c("node_id", "iso3", "imports0")
  sc <- merge(dol[dol$scope_demand_share > 0, c("node_id", "iso3", "scope_demand_share")], imp0, by = c("node_id", "iso3"))
  scope_usd <- tapply(sc$imports0 / sc$scope_demand_share, sc$node_id, stats::median)
  scope_members <- unique(dol$iso3[dol$scope_demand_share > 0])
  intra_scope <- tapply(fl$value * (fl$exporter_iso %in% members & fl$importer_iso %in% members & fl$importer_iso %in% scope_members), fl$node_id, sum)
  mf <- pm_bloc_flows(fl, members, code)
  world <- tapply(mf$value, mf$node_id, sum)
  ex <- stats::aggregate(value ~ node_id + exporter_iso, data = mf, FUN = sum); names(ex) <- c("node_id", "iso3", "exports")
  im <- stats::aggregate(value ~ node_id + importer_iso, data = mf, FUN = sum); names(im) <- c("node_id", "iso3", "imports")
  hist_ex <- stats::aggregate(value_hist ~ node_id + exporter_iso, data = mf, FUN = sum, na.action = stats::na.pass)
  names(hist_ex) <- c("node_id", "iso3", "exports_hist")
  # bloc rows: start from the members' rows
  keep <- dol[!dol$iso3 %in% members, , drop = FALSE]
  mem <- dol[dol$iso3 %in% members, , drop = FALSE]
  bl <- do.call(rbind, lapply(split(mem, mem$node_id), function(x) {
    r <- x[1, , drop = FALSE]
    r$bloc_lead_member <- x$iso3[which.max(pm_num(x$position_share))]
    r$iso3 <- code; r$allied <- TRUE
    pr <- production[production$node_id == r$node_id & production$iso3 %in% members, , drop = FALSE]
    r$production_share <- if (nrow(pr)) sum(pm_num(pr$production_share), na.rm = TRUE) else NA_real_
    rx <- rca[rca$node_id == r$node_id & rca$exporter_iso %in% members & !is.na(rca$export_rca), , drop = FALSE]
    r$export_rca <- if (nrow(rx) && sum(rx$exports) > 0) sum(rx$exports * rx$export_rca) / sum(rx$exports) else NA_real_
    hr <- pm_num(x$best_host_rank)
    r$best_host_rank <- if (all(is.na(hr))) NA_real_ else min(hr, na.rm = TRUE)
    r$capable_host <- any(pm_lgl(x$capable_host) %in% TRUE)
    if ("host_scaled" %in% names(r)) r$host_scaled <- any(pm_lgl(x$host_scaled) %in% TRUE)
    r$momentum <- any(pm_lgl(x$momentum) %in% TRUE)
    if ("reexport_hub" %in% names(r)) r$reexport_hub <- FALSE
    r
  }))
  keep$bloc_lead_member <- NA_character_
  out <- rbind(keep, bl)
  # re-base the trade shares on the merged flows
  k <- paste(out$node_id, out$iso3)
  exv <- stats::setNames(ex$exports, paste(ex$node_id, ex$iso3))[k]; exv[is.na(exv)] <- 0
  imv <- stats::setNames(im$imports, paste(im$node_id, im$iso3))[k]; imv[is.na(imv)] <- 0
  w <- as.numeric(world[out$node_id])
  out$world_share <- ifelse(is.na(w) | w <= 0, 0, unname(exv) / w)
  out$position_share <- pmax(out$world_share, ifelse(is.na(pm_num(out$production_share)), 0, pm_num(out$production_share)))
  s_usd <- as.numeric(scope_usd[out$node_id]) - ifelse(is.na(intra_scope[out$node_id]), 0, as.numeric(intra_scope[out$node_id]))
  in_scope <- out$iso3 %in% c(scope_members, code)
  out$scope_demand_share <- ifelse(in_scope & !is.na(s_usd) & s_usd > 0, unname(imv) / s_usd, 0)
  out$net_import_share <- ifelse(in_scope & !is.na(s_usd) & s_usd > 0, pmax(unname(imv) - unname(exv), 0) / s_usd, 0)
  out$world_import_share <- ifelse(is.na(w) | w <= 0, 0, unname(imv) / w)
  hub <- if ("reexport_hub" %in% names(out)) pm_lgl(out$reexport_hub) %in% TRUE else rep(FALSE, nrow(out))
  rc <- ifelse(is.na(pm_num(out$export_rca)), 0, pm_num(out$export_rca))
  ps <- ifelse(is.na(pm_num(out$production_share)), 0, pm_num(out$production_share))
  out$anchor_position <- !hub & out$position_share >= roles$position_min & (rc >= 1 | ps >= roles$position_min)
  out$specialist_position <- !hub & out$node_function %in% "components" & out$world_share >= roles$position_min & rc >= roles$spec_rca
  out[order(out$node_id, -out$position_share), , drop = FALSE]
}

# Cell rows (strategic_position_tbl) for the bloc, appended to the table.
#   imports: import_partner_cells_tbl (reporter_iso, partner_iso, tech, supply_chain, imports_usd)
pm_bloc_sp <- function(sp, imports, members, code, name, coercive, china_iso) {
  m <- sp[sp$iso3 %in% members, , drop = FALSE]
  if (!nrow(m)) stop("pm_bloc_sp: no strategic-position rows for the bloc members")
  x <- imports[imports$reporter_iso %in% members & !imports$partner_iso %in% members, , drop = FALSE]
  key <- function(t, s) paste(t, s, sep = "|")
  ext <- stats::aggregate(imports_usd ~ reporter_iso + tech + supply_chain, data = x, FUN = sum)
  ext_w <- stats::setNames(ext$imports_usd, paste(ext$reporter_iso, key(ext$tech, ext$supply_chain)))
  by_p <- stats::aggregate(imports_usd ~ partner_iso + tech + supply_chain, data = x, FUN = sum)
  size <- tapply(pm_num(m$imports_usd), m$iso3, sum, na.rm = TRUE)
  wmean <- function(v, w) { ok <- !is.na(v) & !is.na(w) & w > 0; if (!any(ok)) { v <- v[!is.na(v)]; if (length(v)) mean(v) else NA_real_ } else sum(v[ok] * w[ok]) / sum(w[ok]) }
  cells <- split(m, key(m$tech, m$supply_chain))
  rows <- lapply(names(cells), function(k) {
    g <- cells[[k]]
    w_ext <- unname(ext_w[paste(g$iso3, k)]); w_ext[is.na(w_ext)] <- 0
    w_size <- unname(size[g$iso3]); w_size[is.na(w_size)] <- 0
    lead <- g[order(-w_ext, -w_size), , drop = FALSE][1, , drop = FALSE]
    cp <- pm_num(g$cp_contender_pct); cpx <- pm_num(g$competitive_position_percentile)
    best <- if (all(is.na(cp))) which.max(ifelse(is.na(cpx), -1, cpx)) else which.max(ifelse(is.na(cp), -1, cp))
    r <- lead
    r$iso3 <- code; r$Country <- name
    r$cp_contender_pct <- if (all(is.na(cp))) NA_real_ else max(cp, na.rm = TRUE)
    r$competitive_position_percentile <- if (all(is.na(cpx))) NA_real_ else max(cpx, na.rm = TRUE)
    r$cp_high <- any(pm_lgl(g$cp_high) %in% TRUE)
    r$cp_is_contender <- any(pm_lgl(g$cp_is_contender) %in% TRUE)
    r$bloc_lead_member <- g$iso3[best]
    for (col in c("cp_production_position", "cp_manufacturing_capacity", "cp_investment_position", "strategic_industrial_value")) {
      v <- pm_num(g[[col]]); r[[col]] <- if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE)
    }
    r$supply_chain_need <- wmean(pm_num(g$supply_chain_need), w_ext)
    r$deployment_need <- wmean(pm_num(g$deployment_need), w_size)
    for (col in c("policy_priority_percentile", "policy_support_priority", "policy_protection_priority", "policy_momentum", "home_share_exposure", "trade_momentum_score"))
      r[[col]] <- wmean(pm_num(g[[col]]), w_size)
    r$trade_benchmark_rca <- wmean(pm_num(g$trade_benchmark_rca), pm_num(g$trade_benchmark_exports))
    r$trade_benchmark_share <- NA_real_
    r$sn_import_material <- sum(w_ext[pm_lgl(g$sn_import_material) %in% TRUE]) >= 0.5 * sum(w_ext) && sum(w_ext) > 0
    # extra-bloc import exposure
    p <- by_p[key(by_p$tech, by_p$supply_chain) == k, , drop = FALSE]
    tot <- sum(p$imports_usd)
    r$imports_usd <- tot; r$imports_to_gdp <- NA_real_
    if (tot > 0) {
      sh <- p$imports_usd / tot
      r$china_share <- sum(sh[p$partner_iso %in% china_iso]); r$coercive_import_share <- sum(sh[p$partner_iso %in% coercive])
      r$top_supplier_iso3 <- p$partner_iso[which.max(sh)]; r$top_supplier_share <- max(sh); r$import_hhi <- sum(sh^2)
    } else { r$china_share <- NA_real_; r$coercive_import_share <- NA_real_; r$top_supplier_iso3 <- NA_character_; r$top_supplier_share <- NA_real_; r$import_hhi <- NA_real_ }
    r$strategic_choice <- if (all(g$strategic_choice %in% "not_assessed")) "not_assessed" else lead$strategic_choice
    # partners the members name, ranked by the naming members' weight, members and the bloc excluded
    nm <- do.call(rbind, lapply(1:3, function(j) data.frame(p = g[[paste0("recommended_partner_", j)]], w = w_ext + 1e-9 * w_size, stringsAsFactors = FALSE)))
    nm <- nm[!is.na(nm$p) & nzchar(nm$p) & !nm$p %in% c(members, code), , drop = FALSE]
    rk <- if (nrow(nm)) names(sort(tapply(nm$w, nm$p, sum), decreasing = TRUE)) else character()
    for (j in 1:3) r[[paste0("recommended_partner_", j)]] <- if (length(rk) >= j) rk[j] else NA_character_
    for (col in c("world_top_iso3", "coercive_top_iso3")) if (col %in% names(r) && r[[col]] %in% members) r[[col]] <- code
    r
  })
  out <- do.call(rbind, rows)
  if (!"bloc_lead_member" %in% names(sp)) sp$bloc_lead_member <- NA_character_
  rbind(sp[, names(out)], out)
}

# Bilateral cell imports with the bloc as one reporter and partner; intra-bloc imports dropped.
pm_bloc_imports <- function(imports, members, code) {
  x <- imports
  x$reporter_iso[x$reporter_iso %in% members] <- code
  x$partner_iso[x$partner_iso %in% members] <- code
  x <- x[!(x$reporter_iso == code & x$partner_iso == code), , drop = FALSE]
  stats::aggregate(imports_usd ~ reporter_iso + partner_iso + tech + supply_chain, data = x, FUN = sum)
}

# Development-partnership donors: a member donor becomes the bloc (its best-ranked row per recipient step kept);
# recipients inside the bloc are dropped.
pm_bloc_devpart <- function(dp, members, code) {
  if (is.null(dp) || !nrow(dp)) return(dp)
  x <- dp[!dp$recipient_iso3 %in% members, , drop = FALSE]
  x$donor_iso3[x$donor_iso3 %in% members] <- code
  x <- x[order(x$donor_iso3, x$recipient_iso3, x$technology, x$stage, pm_num(x$rank_donor)), , drop = FALSE]
  x[!duplicated(paste(x$donor_iso3, x$recipient_iso3, x$technology, x$stage)), , drop = FALSE]
}

# Compact roles: the bloc takes its most relevant member's role in each compact ("<role> (via <member>)"),
# with relevance summed over the members.
pm_bloc_croles <- function(croles, members, code, names = NULL) {
  m <- croles[croles$iso3 %in% members, , drop = FALSE]
  if (!nrow(m)) return(croles)
  b <- do.call(rbind, lapply(split(m, m$strategic_compact_id), function(x) {
    x <- x[order(-pm_num(x$compact_relevance)), , drop = FALSE]
    r <- x[1, , drop = FALSE]
    via <- if (!is.null(names) && !is.na(names[r$iso3])) names[[r$iso3]] else r$iso3
    r$compact_role <- sprintf("%s (via %s)", r$compact_role, via)
    r$compact_relevance <- sum(pm_num(x$compact_relevance), na.rm = TRUE)
    r$iso3 <- code
    r
  }))
  rbind(croles[!croles$iso3 %in% members, , drop = FALSE], b)
}

# Strategic roles: the roles the largest members hold (primary or secondary), at the strongest member's strength.
#   size: named numeric, member -> economic size proxy
pm_bloc_sroles <- function(sroles, members, code, size, n_lead = 4) {
  lead <- utils::head(names(sort(size[names(size) %in% members], decreasing = TRUE)), n_lead)
  m <- sroles[sroles$iso3 %in% lead & sroles$primary_or_secondary %in% c("primary", "secondary"), , drop = FALSE]
  if (!nrow(m)) return(sroles[!sroles$iso3 %in% members, , drop = FALSE])
  b <- do.call(rbind, lapply(split(m, m$role_code), function(x) {
    r <- x[order(x$primary_or_secondary != "primary", -pm_num(x$role_strength)), , drop = FALSE][1, , drop = FALSE]
    r$iso3 <- code
    r
  }))
  rbind(sroles[!sroles$iso3 %in% members, , drop = FALSE], b)
}

# The memo's method note for a bloc pair (printed under the tags line).
#   name: sentence-initial name ("The European Union"); short: "EU"; ref: prose reference ("the EU");
#   other_poss: the other side's possessive ("the US's")
pm_bloc_note <- function(name, short, ref, n_members, other_poss) {
  sprintf("%s is treated as one economy (%d members). Trade among its members is excluded, so every share of world supply, allied demand and allied net imports in this memo is a share of world trade without intra-%s trade, and %s shares are higher than in its other memos. At cell level %s's Competitive Position is its strongest member's and its need is the members' need weighted by their imports from outside the %s; the member that holds %s's position is named in brackets.",
          name, n_members, short, other_poss, ref, short, ref)
}

# Node classification: the largest trusted supplier and the AIB host lists recoded to the bloc.
pm_bloc_nclass <- function(nclass, members, code) {
  nclass$optimized_largest_supplier[nclass$optimized_largest_supplier %in% members] <- code
  for (k in intersect(c("priority_investment_hosts", "specialty_hosts", "development_hosts"), names(nclass)))
    nclass[[k]] <- pm_bloc_recode_list(nclass[[k]], members, code, sep = ";")
  nclass
}

# The spine's world-largest supplier and its share, recomputed on the re-based division of labour.
pm_bloc_spine <- function(spine, dol_rebased) {
  top <- do.call(rbind, lapply(split(dol_rebased, dol_rebased$node_id), function(x) x[which.max(pm_num(x$position_share)), c("node_id", "iso3", "position_share")]))
  m <- match(spine$node_id, top$node_id)
  spine$largest_supplier <- ifelse(is.na(m), spine$largest_supplier, top$iso3[m])
  spine$largest_share <- ifelse(is.na(m), spine$largest_share, top$position_share[m])
  spine
}

# Investment pairs: investor and host recoded, intra-bloc pairs dropped, the best rank kept.
pm_bloc_invest_pairs <- function(ip, members, code) {
  if (is.null(ip) || !nrow(ip)) return(ip)
  ip$investor_iso[ip$investor_iso %in% members] <- code
  ip$host_iso[ip$host_iso %in% members] <- code
  ip <- ip[ip$investor_iso != ip$host_iso, , drop = FALSE]
  ip <- ip[order(ip$node_id, ip$investor_iso, ip$host_iso, ip$host_rank), , drop = FALSE]
  ip[!duplicated(paste(ip$node_id, ip$investor_iso, ip$host_iso)), , drop = FALSE]
}
