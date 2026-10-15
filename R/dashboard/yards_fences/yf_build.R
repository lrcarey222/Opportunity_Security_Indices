# Draw the Fence (Yards & Fences) data build: pure helpers. No IO; scripts/89 reads and writes.
#
# Inputs are the decoded, published Sector and Strategic bundles (scripts/87 and 85), the allied
# network CSVs, config/yards_fences_metric_map.yml and data/reference/yards_fences_assessments.csv.
# Outputs are plain lists that scripts/89 serialises to dashboard/public/data/yards-fences/ in the
# shape of dashboard/src/fence/data/contract.ts.

# Answer domains of the curated assessments (mirror of DOMAINS in dashboard/src/fence/engine/types.ts).
YF_DOMAINS <- list(
  consequence = c("low", "moderate", "high", "severe"),
  time_horizon = c("years", "months", "weeks", "days"),
  digitally_active = c("no", "yes"),
  remote_control = c("no", "limited", "yes"),
  separability = c("separable", "partial", "inseparable"),
  substitutability = c("easy", "moderate", "hard", "impossible"),
  qualification_time = c("lt3m", "m3to12", "y1to3", "gt3y"),
  reconstitution_time = c("lt6m", "m6to12", "y1to3", "gt3y"),
  stockpile = c("gt12m", "m3to12", "lt3m", "none"),
  surge_capacity = c("covers", "partial", "none"),
  process_knowledge = c("no", "partly", "yes"),
  workforce_suppliers = c("no", "partly", "yes"),
  adjacent_innovation = c("no", "partly", "yes"),
  complementary_capabilities = c("no", "partly", "yes"),
  maturity = c("mature", "scaling", "emerging")
)

# CSV column -> engine input id.
YF_INPUT_ID <- c(
  consequence = "consequence", time_horizon = "timeHorizon", digitally_active = "digitallyActive",
  remote_control = "remoteControl", separability = "separability", substitutability = "substitutability",
  qualification_time = "qualificationTime", reconstitution_time = "reconstitutionTime", stockpile = "stockpile",
  surge_capacity = "surgeCapacity", process_knowledge = "processKnowledge", workforce_suppliers = "workforceSuppliers",
  adjacent_innovation = "adjacentInnovation", complementary_capabilities = "complementaryCapabilities", maturity = "maturity"
)

YF_LAYERS <- c("control", "hardware", "material", "fuel", "structure")
YF_ENTITY_ISO <- c("E:iea_etp:china" = "CHN", "E:iea_etp:eu" = "EUU", "E:iea_etp:us" = "USA",
                   "E:iea_etp:india" = "IND", "E:iea_etp:viet-nam" = "VNM")

yf_is_unknown <- function(x) is.na(x) | !nzchar(trimws(x)) | tolower(trimws(x)) == "unknown"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

# Fill {key} placeholders; stops on any it cannot fill.
yf_fill <- function(template, values) {
  out <- template
  for (k in names(values)) out <- gsub(paste0("{", k, "}"), as.character(values[[k]]), out, fixed = TRUE)
  left <- regmatches(out, gregexpr("\\{[a-z]+\\}", out))[[1]]
  if (length(left)) stop("yf_fill(): unfilled placeholder(s) ", paste(left, collapse = ", "), " in: ", template)
  out
}

yf_fmt <- function(x, unit) {
  if (is.null(x) || is.na(x)) return("n/a")
  switch(unit,
    share = sprintf(if (x < 0.1) "%.1f%%" else "%.0f%%", 100 * x),
    hhi = sprintf("%.3f", x),
    usd = {
      a <- abs(x); s <- if (x < 0) "-" else ""
      if (a >= 1e9) sprintf("%sUSD %.1f bn", s, a / 1e9) else sprintf("%sUSD %.0f m", s, a / 1e6)
    },
    usd_bn = sprintf("USD %.1f bn", x),
    rca = sprintf("%.2f", x),
    index01 = sprintf("%.2f", x),
    pct_change = sprintf("%+.0f%%", 100 * x),
    pct_pa = sprintf("%+.1f%%", 100 * x),
    trl = sprintf("%.1f", x),
    trl_points = sprintf("%+.1f", x),
    format(x)
  )
}

# The home economy's status on one measure: "listed" (a positive value), "zero" (a recorded zero) or
# "unlisted" (absent, or a status such as not-quantified). `status` may be NULL (sub-component trade,
# where a row with 0 exports is a recorded zero).
yf_home_status <- function(geo_idx, value, status, home, lk) {
  iso <- lk$geo$iso[geo_idx + 1L]
  i <- which(iso == home)
  if (!length(i)) return("unlisted")
  v <- value[i[1]]
  s <- if (is.null(status)) "obs" else status[i[1]]
  if (!is.na(v) && v > 0) return("listed")
  if (!is.na(v) && v == 0 && s %in% c("obs", "zero")) return("zero")
  "unlisted"
}

# ── Lookups over the decoded Sector meta chunk ──────────────────────────────────────────────────

yf_lookup <- function(meta) {
  geo <- as.data.frame(meta$dims$geo, stringsAsFactors = FALSE)
  geo$iso <- ifelse(geo$code %in% names(YF_ENTITY_ISO), YF_ENTITY_ISO[geo$code], geo$code)
  ent_tier <- geo$tier[match(geo$iso, geo$code)]
  geo$tier2 <- ifelse(geo$kind == "entity", ifelse(is.na(ent_tier), "none", ent_tier), geo$tier)
  geo$tier2[is.na(geo$tier2)] <- "none"
  geo$display <- sub(" \\(IEA\\)$", "", geo$name)
  items <- as.data.frame(meta$dims$items, stringsAsFactors = FALSE)
  vars <- as.data.frame(meta$dict$vars, stringsAsFactors = FALSE)
  sources <- as.data.frame(meta$dict$sources, stringsAsFactors = FALSE)
  list(
    geo = geo, items = items, vars = vars, sources = sources,
    var_id = function(i) vars$id[i + 1L],
    item_slug = function(i) items$slug[i + 1L],
    geo_code = function(i) geo$code[i + 1L],
    var_index = function(id) match(id, vars$id) - 1L,
    item_index = function(slug) match(slug, items$slug) - 1L,
    source_of = function(var_id) {
      sid <- vars$source[match(var_id, vars$id)]
      s <- sources[match(sid, sources$id), ]
      list(id = sid, text = if (nrow(s) && !is.na(s$short)) paste0(s$short, if (!is.na(s$edition) && nzchar(s$edition)) paste0(" (", s$edition, ")") else "") else NA_character_)
    }
  )
}

# Per-geo shares of one measure for one scope, ranked; aggregates (EU bloc, world, regions,
# residuals) excluded so that ranks are over economies (EU members counted one by one).
yf_shares <- function(geo_idx, value, W, lk, basis_is_share = FALSE) {
  g <- lk$geo[geo_idx + 1L, ]
  keep <- g$kind %in% c("country", "entity") & !is.na(value) & value > 0
  if (!any(keep)) return(data.frame(code = character(), iso = character(), name = character(), tier = character(), share = numeric()))
  share <- if (basis_is_share) value[keep] else value[keep] / W
  out <- data.frame(code = g$code[keep], iso = g$iso[keep], name = g$display[keep], tier = g$tier2[keep], share = share, stringsAsFactors = FALSE)
  out[order(-out$share), , drop = FALSE]
}

# ── Evidence records ────────────────────────────────────────────────────────────────────────────

yf_record <- function(ctx, metric, label, raw, unit, extra = list()) {
  rec <- c(list(
    country = ctx$home, technology = ctx$technology, stage = ctx$stage, scope = ctx$scope,
    scopeId = ctx$scope_id, scopeLabel = ctx$scope_label, metricId = metric$id,
    measure = ctx$measure %||% NA_character_, concept = metric$concept, label = label,
    rawValue = raw, unit = unit, normalized = isTRUE(metric$normalized), evidenceType = "data"
  ), extra)
  rec
}

# The concentration family (top share, HHI, non-coalition top, China, coalition, home share) for one
# scope x measure. `summary_row` is the members-mode summary; `shares` the per-geo table.
yf_concentration_records <- function(ctx, mdef, summary_row, shares, metrics, hhi_pool, engine_ok, home_engine_ok) {
  out <- list()
  m <- function(id) metrics[[which(vapply(metrics, function(x) x$id == id, logical(1)))]]
  vals <- list(measure = mdef$label, year = summary_row$year, country = ctx$home_name)
  base <- list(year = summary_row$year, source = ctx$source, sourceId = ctx$source_id, caveats = ctx$caveats)
  lab <- function(md) yf_fill(md$label, list(measure = mdef$label, country = ctx$home_name))
  # top share
  top <- if (nrow(shares)) shares[1, ] else NULL
  md <- m("top_share")
  if (!is.null(top)) {
    out[[length(out) + 1]] <- yf_record(ctx, md, lab(md), summary_row$top1_share, "share", c(base, list(
      subject = top$name, engineInput = if (engine_ok) md$engine_input else NA,
      explanation = yf_fill(md$explanation, c(vals, list(subject = top$name, value = yf_fmt(summary_row$top1_share, "share")))))))
  }
  md <- m("hhi")
  pct <- round(100 * mean(hhi_pool < summary_row$hhi))
  out[[length(out) + 1]] <- yf_record(ctx, md, lab(md), summary_row$hhi, "hhi", c(base, list(
    percentile = pct, nEconomies = summary_row$n, engineInput = if (engine_ok) md$engine_input else NA,
    explanation = yf_fill(md$explanation, c(vals, list(value = yf_fmt(summary_row$hhi, "hhi"), pct = paste0(pct, "%")))))))
  nc <- shares[shares$tier == "none", , drop = FALSE]
  md <- m("non_coalition_top")
  if (nrow(nc)) {
    out[[length(out) + 1]] <- yf_record(ctx, md, lab(md), nc$share[1], "share", c(base, list(
      subject = nc$name[1], engineInput = if (engine_ok) md$engine_input else NA,
      explanation = yf_fill(md$explanation, c(vals, list(subject = nc$name[1], value = yf_fmt(nc$share[1], "share")))))))
  }
  md <- m("china_share")
  out[[length(out) + 1]] <- yf_record(ctx, md, lab(md), summary_row$china, "share", c(base, list(
    subject = "China", explanation = yf_fill(md$explanation, c(vals, list(value = yf_fmt(summary_row$china, "share")))))))
  md <- m("coalition_share")
  coal <- summary_row$core + summary_row$partner
  out[[length(out) + 1]] <- yf_record(ctx, md, lab(md), coal, "share", c(base, list(
    engineInput = if (engine_ok) md$engine_input else NA,
    explanation = yf_fill(md$explanation, c(vals, list(value = yf_fmt(coal, "share")))))))
  md <- m("home_share")
  hi <- which(shares$iso == ctx$home)
  n <- nrow(shares)
  if (length(hi)) {
    hs <- shares$share[hi[1]]; rk <- hi[1]
    concept <- md$concept_by_measure[[ctx$measure]] %||% md$concept
    md2 <- md; md2$concept <- concept
    out[[length(out) + 1]] <- yf_record(ctx, md2, lab(md), hs, "share", c(base, list(
      subject = ctx$home_name, rank = rk, nEconomies = n, percentile = if (n > 1) round(100 * (n - rk) / (n - 1)) else NA,
      engineInput = if (home_engine_ok) md$engine_input else NA,
      explanation = yf_fill(md$explanation, c(vals, list(value = yf_fmt(hs, "share"), rank = rk, n = n))))))
  } else {
    # Not in the ranked list: a recorded zero is data; anything else (not quantified, not listed)
    # stays unknown and never feeds the engine.
    concept <- md$concept_by_measure[[ctx$measure]] %||% md$concept
    md2 <- md; md2$concept <- concept
    zero <- identical(ctx$home_status, "zero")
    out[[length(out) + 1]] <- yf_record(ctx, md2, lab(md), if (zero) 0 else NA, "share", c(base, list(
      subject = ctx$home_name, rank = NA, nEconomies = n, percentile = if (zero) 0 else NA,
      engineInput = if (zero && home_engine_ok) md$engine_input else NA,
      explanation = if (zero) sprintf("%s records zero %s (%s).", ctx$home_name, mdef$label, summary_row$year)
                    else sprintf("%s is not listed separately in this source for %s (%s): not quantified, not zero.", ctx$home_name, mdef$label, summary_row$year))))
  }
  out
}

yf_rank_records <- function(ctx, md, label, home_value, all_values, unit, year, source, fill = list(), higher_better = TRUE) {
  all_values <- all_values[!is.na(all_values)]
  n <- length(all_values)
  rk <- if (is.na(home_value)) NA else if (higher_better) sum(all_values > home_value) + 1L else sum(all_values < home_value) + 1L
  pct <- if (is.na(rk) || n <= 1) NA else round(100 * (n - rk) / (n - 1))
  vals <- c(list(country = ctx$home_name, value = yf_fmt(home_value, unit), rank = rk, n = n, year = year), fill)
  yf_record(ctx, md, label, home_value, unit, list(
    year = year, source = source$text, sourceId = source$id, rank = rk, nEconomies = n, percentile = pct,
    caveats = ctx$caveats, explanation = yf_fill(md$explanation, vals)))
}

# ── Cells and sub-components ────────────────────────────────────────────────────────────────────

yf_cells <- function(meta, trade, lk) {
  cells <- lk$items[lk$items$axis == "cell", ]
  blurbs <- as.data.frame(meta$dict$cells, stringsAsFactors = FALSE)
  cc <- as.data.frame(meta$dict$cell_caveats, stringsAsFactors = FALSE)
  cav <- as.data.frame(meta$dict$caveats, stringsAsFactors = FALSE)
  codes <- do.call(rbind, lapply(trade, function(t) as.data.frame(t$trade$codes, stringsAsFactors = FALSE)))
  codes$slug <- lk$item_slug(codes$item)
  lapply(seq_len(nrow(cells)), function(i) {
    slug <- cells$slug[i]
    cd <- codes[codes$slug == slug, , drop = FALSE]
    subs <- lapply(unique(cd$sub_sector), function(s) {
      x <- cd[cd$sub_sector == s, ]
      list(id = paste0(slug, "::", s), label = s, hs6 = I(x$hs6), shared = any(x$shared_n > 0))
    })
    cav_ids <- cc$caveat[cc$slug == slug]
    list(
      id = slug, technology = cells$tech[i], stage = cells$stage[i], label = cells$label[i],
      blurb = blurbs$cell_blurb[match(slug, blurbs$slug)] %||% NA_character_,
      hasTrade = isTRUE(as.logical(cells$has_trade[i])), subcomponents = subs,
      caveats = I(unname(cav$text[match(cav_ids, cav$id)]))
    )
  })
}

# ── Normalised OSI components (Strategic bundle) ────────────────────────────────────────────────

yf_osi_component <- function(core, vars, pillar, category, variable, tech, stage) {
  cells <- as.data.frame(core$pillars[[pillar]]$cells, stringsAsFactors = FALSE)
  v <- as.data.frame(vars$pillars[[pillar]]$var, stringsAsFactors = FALSE)
  v <- v[v$category == category & v$variable == variable, , drop = FALSE]
  if (!nrow(v)) return(NULL)
  ci <- cells[v$cell, ]
  keep <- ci$tech == tech & ci$supply_chain == stage & !is.na(v$component_value) & as.logical(ci$sovereign %||% TRUE)
  data.frame(iso3 = ci$iso3c[keep], value = v$component_value[keep], imputed = v$imputed[keep], stringsAsFactors = FALSE)
}

# ── Supply views ────────────────────────────────────────────────────────────────────────────────

yf_supply_view <- function(scope_id, measure, mdef, summary_row, shares, source, n_top, home_imports = NULL) {
  top <- head(shares, n_top)
  list(
    scopeId = scope_id, measure = measure, measureLabel = mdef$label, year = summary_row$year, source = source,
    top = lapply(seq_len(nrow(top)), function(i) list(geo = top$iso[i], name = top$name[i], tier = top$tier[i], share = top$share[i])),
    hhi = summary_row$hhi, effN = summary_row$eff_n, china = summary_row$china, core = summary_row$core,
    partner = summary_row$partner, rest = summary_row$rest, homeImports = home_imports
  )
}

yf_home_imports <- function(flows, item_index, home, lk, n_top) {
  f <- flows[flows$item == item_index & lk$geo_code(flows$importer) == home & !is.na(flows$v2024) & flows$v2024 > 0, , drop = FALSE]
  if (!nrow(f)) return(NULL)
  tot <- sum(f$v2024)
  g <- lk$geo[f$exporter + 1L, ]
  sh <- data.frame(iso = g$iso, name = g$display, tier = g$tier2, share = f$v2024 / tot, stringsAsFactors = FALSE)
  sh <- stats::aggregate(share ~ iso + name + tier, data = sh, FUN = sum)
  sh <- sh[order(-sh$share), ]
  top <- head(sh, n_top)
  list(year = 2024L, total = tot, hhi = sum(sh$share^2),
       sources = lapply(seq_len(nrow(top)), function(i) list(geo = top$iso[i], name = top$name[i], tier = top$tier[i], share = top$share[i])))
}

# ── Allied network ──────────────────────────────────────────────────────────────────────────────

yf_network_views <- function(member_shares, diagnostics, observed, nodes, psi_home, home, built) {
  ms <- member_shares[as.logical(member_shares$selected), , drop = FALSE]
  lapply(seq_len(nrow(diagnostics)), function(i) {
    d <- diagnostics[i, ]
    cell <- paste0(gsub(" ", "-", tolower(d$tech)), "-", tolower(d$supply_chain))
    s <- ms[ms$tech == d$tech & ms$supply_chain == d$supply_chain, , drop = FALSE]
    nd <- unique(s[, c("network_node", "node_production_share")])
    nd <- nd[order(-nd$node_production_share), , drop = FALSE]
    o <- observed[observed$tech == d$tech & observed$supply_chain == d$supply_chain, , drop = FALSE]
    home_node <- unique(s$network_node[s$iso3c == home])
    list(
      cell = cell, method = d$method, lambda = suppressWarnings(as.numeric(d$diversity_lambda)), built = built,
      designedHhi = as.numeric(d$hhi), effectiveProducers = as.numeric(d$effective_producers),
      maxShare = as.numeric(d$max_production_share),
      observedHhi = if (nrow(o)) as.numeric(o$hhi[1]) else NA,
      observedTop = if (nrow(o)) list(node = o$top_node[1], share = as.numeric(o$top_share[1])) else NULL,
      observedYear = if (nrow(o)) as.integer(o$year[1]) else NA,
      nodes = lapply(seq_len(nrow(nd)), function(j) {
        nn <- nd$network_node[j]
        p <- psi_home[psi_home$partner_iso == nn, , drop = FALSE]
        list(node = nn, label = nodes$label[match(nn, nodes$network_node)] %||% nn,
             share = as.numeric(nd$node_production_share[j]),
             tier = nodes$tier[match(nn, nodes$network_node)] %||% "core",
             psi = if (nrow(p)) p$psi[1] else NA, psiRank = if (nrow(p)) p$rank[1] else NA)
      }),
      homeShare = if (length(home_node)) as.numeric(nd$node_production_share[nd$network_node == home_node[1]][1]) else NA,
      barred = I(if (is.na(d$producers_forbidden) || !nzchar(d$producers_forbidden)) character() else strsplit(trimws(d$producers_forbidden), "\\s+|;|,")[[1]])
    )
  })
}

# ── Curated components ──────────────────────────────────────────────────────────────────────────

yf_parse_scope <- function(x) {
  x <- trimws(x)
  if (yf_is_unknown(x)) return(NULL)
  p <- regmatches(x, regexpr(":", x), invert = TRUE)[[1]]
  list(scope = p[1], scopeId = paste(p[-1], collapse = ":"))
}

yf_components <- function(df) {
  lapply(seq_len(nrow(df)), function(i) {
    r <- df[i, ]
    ass <- list()
    for (col in names(YF_INPUT_ID)) {
      v <- trimws(r[[col]])
      if (!yf_is_unknown(v)) ass[[YF_INPUT_ID[[col]]]] <- v
    }
    ev <- if (r$evidence_scope %in% c("stage", "subcomponent", "mineral")) list(scope = r$evidence_scope, scopeId = r$evidence_scope_id) else NULL
    sec <- if (yf_is_unknown(r$secondary_scopes)) list() else Filter(Negate(is.null), lapply(strsplit(r$secondary_scopes, ";")[[1]], yf_parse_scope))
    ov <- suppressWarnings(as.numeric(r$non_allied_top_share_override))
    list(
      id = r$component_id, technology = r$technology, stage = r$stage, label = r$component, description = r$description,
      layer = r$layer, evidence = ev, secondary = sec, assessments = if (length(ass)) ass else structure(list(), names = character()),
      shareOverride = if (!is.na(ov)) list(value = ov, subject = r$override_subject) else NULL,
      criticalFunction = if (yf_is_unknown(r$critical_function)) NA else r$critical_function,
      paperPlacement = if (r$paper_placement %in% c("small", "medium", "large")) r$paper_placement else NA,
      notes = r$notes, source = r$source, confidence = r$confidence
    )
  })
}

# Problems with the curated table: unknown domain values, layers, confidences, duplicate ids.
yf_validate_assessments <- function(df) {
  bad <- character()
  if (anyDuplicated(df$component_id)) bad <- c(bad, paste("duplicate component_id:", paste(df$component_id[duplicated(df$component_id)], collapse = ", ")))
  for (col in names(YF_DOMAINS)) {
    v <- trimws(df[[col]])
    ok <- yf_is_unknown(v) | v %in% YF_DOMAINS[[col]]
    if (any(!ok)) bad <- c(bad, sprintf("%s: invalid value(s) %s (rows %s)", col, paste(unique(v[!ok]), collapse = ", "), paste(which(!ok), collapse = ",")))
  }
  if (any(!df$layer %in% YF_LAYERS)) bad <- c(bad, paste("invalid layer:", paste(unique(df$layer[!df$layer %in% YF_LAYERS]), collapse = ", ")))
  if (any(!df$confidence %in% c("low", "moderate", "high"))) bad <- c(bad, "invalid confidence")
  if (any(!df$stage %in% c("Upstream", "Midstream", "Downstream"))) bad <- c(bad, "invalid stage")
  if (any(yf_is_unknown(df$source))) bad <- c(bad, paste("missing source:", paste(df$component_id[yf_is_unknown(df$source)], collapse = ", ")))
  ov <- suppressWarnings(as.numeric(df$non_allied_top_share_override))
  if (any(!is.na(ov) & (ov < 0 | ov > 1))) bad <- c(bad, "share override outside 0-1")
  if (any(!is.na(ov) & yf_is_unknown(df$override_subject))) bad <- c(bad, "share override without a subject")
  bad
}

# Every validation check on the finished bundle. Returns data.frame(check, ok, detail).
yf_validate_bundle <- function(reference, bundle, sector_manifest, strategic_manifest) {
  scope_ids <- unique(vapply(bundle$records, function(r) r$scopeId, character(1)))
  comp_scopes <- unlist(lapply(reference$components, function(c) c(if (!is.null(c$evidence)) c$evidence$scopeId, vapply(c$secondary, function(s) s$scopeId, character(1)))))
  missing_scopes <- setdiff(comp_scopes, scope_ids)
  layers <- unlist(lapply(reference$stacks, function(s) s$layers))
  comp_ids <- vapply(reference$components, function(c) c$id, character(1))
  raw <- Filter(function(r) !isTRUE(r$normalized), bundle$records)
  no_meta <- Filter(function(r) is.null(r$source) || is.na(r$source) || is.null(r$year) || is.na(r$year), raw)
  norm_engine <- Filter(function(r) isTRUE(r$normalized) && !is.null(r$engineInput) && !is.na(r$engineInput), bundle$records)
  headline <- grepl("Energy Security Index|Economic Opportunity Index|Strategic Index|Partnership Strength Index",
                    vapply(bundle$records, function(r) r$label, character(1)))
  data.frame(
    check = c("sector bundle excludes restricted (BNEF/BCG) data", "strategic bundle is the one the sector bundle was built next to",
              "every component evidence scope has records", "every stack layer is a curated component",
              "every raw record carries a source and a year", "no normalised OSI component feeds the engine",
              "no headline index is used as evidence"),
    ok = c(identical(sector_manifest$flags$include_restricted, FALSE),
           identical(sector_manifest$strategic_bundle_id, strategic_manifest$bundle_id),
           length(missing_scopes) == 0, all(layers %in% comp_ids), length(no_meta) == 0, length(norm_engine) == 0, !any(headline)),
    detail = c(paste("include_restricted =", sector_manifest$flags$include_restricted),
               paste(sector_manifest$strategic_bundle_id, "vs", strategic_manifest$bundle_id),
               paste(missing_scopes, collapse = "; "), paste(setdiff(layers, comp_ids), collapse = "; "),
               paste(length(no_meta), "records"), paste(length(norm_engine), "records"), paste(sum(headline), "records")),
    stringsAsFactors = FALSE
  )
}
