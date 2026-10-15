# Assembly of one economy's fact sheet and of the Country Insights manifest (spec_country.md A8.1-A8.4).
# Pure: every input arrives in `ctx` (built by scripts/88 from data/processed and the published
# bundles, or by country_synthetic_ctx()). See scripts/88_export_country_data.R for the fields of ctx.

# The tables of one economy from a table keyed by iso3 (or a list pre-split by iso3).
country_of <- function(x, iso3, col = "iso3") {
  if (is.null(x)) return(NULL)
  if (!is.data.frame(x)) return(x[[iso3]] %||% NULL)
  x[x[[col]] == iso3, , drop = FALSE]
}

country_empty_df <- function(cols) {
  d <- as.data.frame(stats::setNames(replicate(length(cols), character(), simplify = FALSE), cols), stringsAsFactors = FALSE)
  d
}

# High-priority sectors (A9.5).
country_priority_sectors <- function(cells, si_order, eo_order, n = 3) {
  q <- vapply(cells, function(c) c$quadrant %||% "", "")
  names(q) <- vapply(cells, function(c) c$key, "")
  placed <- vapply(cells, function(c) isTRUE(c$placed), TRUE)
  names(placed) <- names(q)
  if (length(si_order)) {
    up <- si_order[q[si_order] == "upgrade"]
    if (length(up)) return(list(keys = utils::head(up, n), rule = "upgrade"))
    fb <- si_order[placed[si_order]]
    if (length(fb)) return(list(keys = utils::head(fb, n), rule = "top_si_fallback"))
  }
  if (length(eo_order)) return(list(keys = utils::head(eo_order, n), rule = "top_eo"))
  list(keys = character(), rule = "none")
}

# The caveats of a sheet (A8.4): every applicable id with its sentence.
country_caveats <- function(sheet, cfg, extra = list()) {
  m <- sheet$meta
  cells <- sheet$cells
  any_cell <- function(f) any(vapply(cells, function(c) isTRUE(f(c)), TRUE))
  drivers_any <- function(f) any_cell(function(c) any(vapply(c(c$drivers$es, c$drivers$eo), function(d) isTRUE(f(d)), TRUE)))
  trade_parties <- unlist(lapply(cells, function(c) if (is.null(c$trade)) list() else c(c$trade$sources, c$trade$markets)), recursive = FALSE)
  party_iso <- vapply(trade_parties, function(p) p$iso3, "")
  partner_iso <- vapply(sheet$partners$rows, function(r) r$iso3, "")
  has_trade <- any_cell(function(c) !is.null(c$trade) && isTRUE(c$trade$has_basket) && ((c$trade$imports$v %||% 0) > 0 || (c$trade$exports$v %||% 0) > 0))
  fdi_any <- any_cell(function(c) !is.null(c$fdi) && ((c$fdi$in_n$v %||% 0) + (c$fdi$out_n$v %||% 0)) > 0) ||
    any(vapply(sheet$partners$rows, function(r) ((r$ties$fdi_in$v %||% 0) + (r$ties$fdi_out$v %||% 0)) > 0, TRUE))
  china_parent <- any_cell(function(c) !is.null(c$fdi) && (c$fdi$in_china_n$v %||% 0) > 0) ||
    any(vapply(sheet$partners$rows, function(r) isTRUE(r$ties$fdi_in_china_parent) && (r$ties$fdi_in$v %||% 0) > 0, TRUE))
  # the top sector or the allied pick within 0.01 of its runner-up, or any two neighbouring sectors of the
  # Strategic Index ranking (the memo's machine trigger also fires on those, A8.4)
  near <- isTRUE(sheet$summary$top_si$near_tie) || isTRUE(sheet$network$pick$near_tie) ||
    any_cell(function(c) isTRUE(c$si$in_sample) && isTRUE((c$si$margin_next$v %||% 1) < (cfg$near_tie %||% 0.01)))
  on <- list(
    relative_scores = TRUE, baseline = TRUE,
    rankability = any_cell(function(c) (!is.null(c$es) && !isTRUE(c$es$rankable)) || (!is.null(c$eo) && !isTRUE(c$eo$rankable))),
    imputed_driver = drivers_any(function(d) isTRUE(d$top) && (isTRUE(d$imputed) || any(vapply(d$vars, function(v) isTRUE(v$imputed), TRUE)))),
    policy_pams_only = identical(m$policy_basis, "IEA PAMS only"),
    no_si = !isTRUE(m$has_si),
    near_tie = near,
    eu_node = isTRUE(m$eu_member) && isTRUE(sheet$network$member),
    inherited_alignment = identical(m$alignment_basis, "inherited"),
    assumed_zero_production = drivers_any(function(d) any(vapply(d$vars, function(v) identical(v$raw$note, "production_assumed_zero"), TRUE))),
    cmfdi_coverage = fdi_any,
    trade_mirror = any(vapply(trade_parties, function(p) identical(p$status, "mirror"), TRUE)),
    few_economies = any_cell(function(c) isTRUE(c$placed) && (c$es$n$v %||% 99) < (cfg$few_economies %||% 10)),
    cat_eu_bloc = identical(sheet$policy$cat$scope, "eu_bloc"),
    intra_eu = isTRUE(m$eu_member) && has_trade,
    hkg_reexport = "HKG" %in% c(party_iso, partner_iso),
    pick_stability = !is.null(sheet$network$pick),
    ownership_parent = china_parent,
    eu_policy_records = isTRUE(m$eu_member) && !is.null(sheet$policy$pams_eu_records),
    nipo_context = isTRUE(sheet$policy$nipo$covered),
    # a driver whose raw figure is the EU's regional value (raw note eu_region_share, 2026-10-14)
    eu_region_share = drivers_any(function(d) any(vapply(d$vars, function(v) identical(v$raw$note, "eu_region_share"), TRUE)))
  )
  # {country} and {sovereign} print as running-text names ("the Netherlands", "the United States'"), 2026-10-14
  vals <- c(list(country = m$country, as_of_label = m$as_of_label, few = cfg$few_economies %||% 10,
                 sovereign = extra$sovereign_name %||% "its sovereign", cmfdi_edition = m$cmfdi_edition,
                 share = extra$pick_share %||% COUNTRY_DASH, n = extra$eu_records %||% COUNTRY_DASH,
                 nipo_vintage = sheet$policy$nipo$vintage_label %||% "Global Trade Alert, New Industrial Policy Observatory"),
            extra[setdiff(names(extra), "sovereign_name")])
  ids <- names(on)[vapply(on, isTRUE, TRUE)]
  ids <- ids[ids %in% names(cfg$caveats)]
  lapply(ids, function(id) list(id = id, text = country_fill_caveat(cfg$caveats[[id]]$text, vals)))
}

# Sources of the sheet: the ids its src_map and raw figures use, labelled from the published Sector
# bundle's dictionary sources (sector_sources: the meta chunk's dict.sources, placeholders filled by
# scripts/87), else the Sector dictionary file (an edition with an unfilled {placeholder} is dropped), or
# config/country_insights.yml.
country_sources <- function(ids, dict, cfg, sector_sources = NULL) {
  ids <- unique(ids[!is.na(ids) & nzchar(ids)])
  filled <- function(x) { x <- country_chr_or_null(x); if (!is.null(x) && grepl("{", x, fixed = TRUE)) NULL else x }
  # config source_names: a spelled-out label for a Sector dictionary source that abbreviates its name (2026-10-14)
  lab <- function(id, x) cfg$source_names[[id]]$label %||% x
  out <- lapply(ids, function(id) {
    for (s in cfg$sources) if (identical(s$id, id)) return(list(id = id, label = s$label, edition = s$edition %||% NULL, licence = s$licence %||% NULL, url = s$url %||% NULL))
    if (!is.null(sector_sources) && id %in% sector_sources$id) {
      s <- sector_sources[match(id, sector_sources$id), , drop = FALSE]
      return(list(id = id, label = lab(id, s$citation %||% s$short %||% id), edition = filled(s$edition), licence = filled(s$licence), url = filled(s$url)))
    }
    for (s in dict$sources) if (identical(s$id, id)) {
      return(list(id = id, label = lab(id, s$citation %||% s$short %||% id), edition = filled(s$edition), licence = filled(s$licence), url = filled(s$url)))
    }
    list(id = id, label = lab(id, id), edition = NULL, licence = NULL, url = NULL)
  })
  out
}

# Remove internal fields (names starting with ".") recursively.
country_strip <- function(x) {
  if (!is.list(x) || inherits(x, "AsIs")) return(x)
  nm <- names(x)
  if (!is.null(nm)) {
    drop <- startsWith(nm, ".")
    if (any(drop)) x <- x[!drop]
  }
  for (i in seq_along(x)) if (is.list(x[[i]]) && !inherits(x[[i]], "AsIs")) x[[i]] <- country_strip(x[[i]])
  x
}

# One fact sheet. Returns the sheet (with checks) or NULL when the economy has no data.
country_fact_sheet <- function(iso3, ctx) {
  cfg <- ctx$cfg
  cells_tbl <- country_cells_tbl()
  ro <- ctx$roster[ctx$roster$iso3 == iso3, , drop = FALSE]
  if (!nrow(ro)) stop("country_fact_sheet(): ", iso3, " is not in the roster")
  country <- ro$name[1]
  es_c <- country_of(ctx$es_by, iso3) %||% country_empty_df(c("iso3", "key"))
  eo_c <- country_of(ctx$eo_by, iso3) %||% country_empty_df(c("iso3", "key"))
  psi_c <- country_of(ctx$psi_by, iso3)
  if (!nrow(es_c) && !nrow(eo_c) && (is.null(psi_c) || !nrow(psi_c))) return(NULL)

  # cells: scores, percentiles, quadrants (ctx$quadrant_rule: COUNTRY_QUADRANT_RULES; scripts/88 builds a
  # "sector_median" copy only to compare memos written under the earlier rule)
  cells <- country_cells_base(iso3, es_c, eo_c, ctx$stats, cells_tbl, rule = ctx$quadrant_rule %||% "country_median")
  oo <- country_orders(cells)
  cells <- oo$cells

  # Strategic Index, priority, policy
  si_rows <- country_of(ctx$si_by, iso3)
  if (is.null(si_rows) || !nrow(si_rows)) {
    si_rows <- data.frame(key = cells_tbl$key, tech = cells_tbl$tech, stage = cells_tbl$stage, eo_level = NA_real_, need = NA_real_, pol = NA_real_,
                          sc_weight = NA_real_, tech_weight = NA_real_, si = NA_real_, si_exclusion = NA_character_, resource_base = NA,
                          pick_exclusion = NA_character_, stringsAsFactors = FALSE)
  }
  si <- country_si(si_rows, ctx$si_w, cells_tbl, cfg$near_tie %||% 0.01)
  pams <- country_pams_counts(ctx$pams_counts, iso3, cells_tbl, eu_records = if (isTRUE(ro$eu_member[1])) ctx$eu_records else NULL)
  pol_index <- country_of(ctx$pol_index_by, iso3) %||% list()
  pp <- country_priority_policy(si, pol_index, pams$cells, cells_tbl)

  # trade, FDI
  tr <- country_trade_cells(ctx$flows, iso3, ctx$basket_keys, ctx$meta$trade_year, ctx$party_fn, cfg$n_trade_parties %||% 5, cells_tbl)
  pt <- country_partner_trade(tr$shares)
  fdi <- country_fdi(ctx$fdi, iso3, cells_tbl)

  # drivers
  above <- function(p) !is.null(p) && isTRUE(p$side == "above")
  for (i in seq_along(cells)) {
    k <- cells[[i]]$key
    cells[[i]]$si <- si$facts[[k]]
    cells[[i]]$priority <- pp$priority[[k]]
    cells[[i]]$policy <- pp$policy[[k]]
    cells[[i]]$trade <- tr$facts[[k]]
    cells[[i]]$fdi <- fdi$cells[[k]]
    dr <- list(es = list(), eo = list())
    for (pl in c("es", "eo")) {
      if (is.null(cells[[i]][[pl]]) || !isTRUE(cells[[i]]$placed)) next
      cat_c <- country_of(ctx$cat_by[[pl]], iso3)
      var_c <- country_of(ctx$var_by[[pl]], iso3)
      if (is.null(cat_c) || !nrow(cat_c)) next
      cat_c <- cat_c[cat_c$key == k, , drop = FALSE]
      var_c <- var_c[var_c$key == k, , drop = FALSE]
      rf <- country_raw_fn(iso3, cells_tbl[cells_tbl$key == k, ], pl, ctx$raw_rows, ctx$raw_idx, cfg$raw_tables, ctx$dict, cfg,
                           dup_fn = ctx$dup_fn %||% function(...) NULL, note_fn = ctx$note_fn %||% function(...) NA_character_,
                           eu_member = isTRUE(ro$eu_member[1]), eu_vals = ctx$eu_vals %||% numeric())
      dr[[pl]] <- country_cell_drivers(iso3, country, k, pl, cat_c, var_c, ctx$dstats[[pl]], above(cells[[i]][[pl]]), rf, cfg)
    }
    cells[[i]]$drivers <- dr
  }

  # priority sectors
  ps <- country_priority_sectors(cells, si$si_order, oo$eo_order, cfg$n_priority_sectors %||% 3)

  # partners
  sm <- country_supplier_matrix(tr$shares, pt$ties, ps$keys, cfg$n_supplier_rows %||% 12, cells_tbl)
  align <- country_alignment(ctx$geo_alignment, iso3)
  b5 <- ctx$best5[ctx$best5$reporter_iso == iso3, , drop = FALSE]
  partners <- country_partner_rows(iso3, if (is.null(psi_c)) data.frame(partner_iso = character(), friendshore = numeric(), opportunity = numeric(),
                                                                         development = numeric(), psi = numeric(), rankable = logical(),
                                                                         rank_exclusion = character(), rank = numeric()) else psi_c,
                                   b5, pt$ties, fdi, ctx$agreements, align, ctx$party_fn, cfg$words, cfg, as.character(sm$partners))
  psi_1 <- partners$psi_1
  partners$psi_1 <- NULL
  partners$n_import_cells <- country_fv(pt$n_import_cells, "count")
  partners$n_export_cells <- country_fv(pt$n_export_cells, "count")
  partners$supplier_matrix <- sm

  # network and allied pick
  allied <- if (is.null(ctx$allied)) NULL else ctx$allied[ctx$allied$iso3 == iso3, , drop = FALSE]
  net <- country_network_role(iso3, ro$tier[1], ro$eu_member[1], country_chr_or_null(ro$network_node[1]), ctx$member_shares, allied, si_rows,
                              cfg$near_tie %||% 0.01, cells_tbl, rule_words = country_pick_rule_words(cfg$pick_rule))
  if (!is.null(net$pick)) {
    for (i in seq_along(cells)) if (cells[[i]]$key == net$pick$cell) cells[[i]]$si$pick <- TRUE
    if (!is.null(ctx$pick_fn)) ctx$pick_fn(iso3, net$pick)
    net$pick$first <- NULL
  }

  # summary
  placed <- Filter(function(c) isTRUE(c$placed), cells)
  qn <- function(q) sum(vapply(placed, function(c) identical(c$quadrant, q), TRUE))
  quads <- list(grow = qn("grow"), upgrade = qn("upgrade"), secure = qn("secure"), watch = qn("watch"))
  cmed <- country_median_sector(cells)
  n_scored <- sum(vapply(cells, function(c) !is.null(c$es) && !is.null(c$eo), TRUE))
  ap <- if (!is.null(ctx$psi_partner)) ctx$psi_partner[ctx$psi_partner$partner_iso == iso3, , drop = FALSE] else NULL
  as_partner <- if (!is.null(ap) && nrow(ap)) {
    list(rank = country_fv(if (isTRUE(ap$rankable[1])) ap$rank[1] else NA, "rank"),
         n = country_fv(sum(ctx$psi_partner$rankable %in% TRUE), "count"), psi = country_fv(ap$psi[1], "index"),
         view = ctx$meta$coalition_view %||% "coalition core (31 members)")
  } else NULL
  summary <- list(
    n_cells = country_fv(length(cells), "count"), n_scored = country_fv(n_scored, "count"), n_placed = country_fv(length(placed), "count"),
    quadrants = lapply(quads, function(v) country_fv(v, "count")),
    # the dashed lines of the scatter: the country's median sector on each axis (A9.2)
    country_median = list(es = country_fv(cmed$es, "pct"), eo = country_fv(cmed$eo, "pct")),
    n_insecure = country_fv(quads$upgrade + quads$secure, "count"), n_high_eo = country_fv(quads$grow + quads$upgrade, "count"),
    es_order = country_arr(oo$es_order), eo_order = country_arr(oo$eo_order), si_order = country_arr(si$si_order),
    top_si = si$top, psi_1 = psi_1, as_partner = as_partner
  )

  # meta
  has_scores <- n_scored > 0
  status <- if (length(placed)) "ok" else if (nrow(es_c) || nrow(eo_c)) "non_rankable" else "no_data"
  status_reason <- if (status == "non_rankable") {
    rs <- unlist(lapply(cells, function(c) c(c$es$reason, c$eo$reason)))
    if (any(grepl("sovereign", rs))) "it is not a sovereign economy" else "most of its data are filled in"
  } else if (status == "no_data") "it has no Energy Security or Economic Opportunity scores" else NULL
  policy_basis <- country_chr_or_null(country_of(ctx$policy_basis, iso3))
  cat_r <- country_cat(ctx$cat_tbl, iso3, isTRUE(ro$eu_member[1]))
  nipo <- if (!is.null(ctx$nipo)) country_nipo_top(ctx$nipo, iso3, ctx$meta$as_of, cfg$nipo$n_top %||% 5, cells_tbl, ctx$meta$nipo,
                                                    rule = cfg$nipo$active_rule %||% "in_force",
                                                    broad_min = cfg$nipo$economy_wide_min_techs %||% 9L) else NULL
  meta <- list(
    country = country, tier = ro$tier[1], routes = country_arr(if (is.na(ro$routes[1])) character() else trimws(strsplit(ro$routes[1], ";", fixed = TRUE)[[1]])),
    region = country_chr_or_null(ro$region[1]), income_group = country_chr_or_null(ro$income_group[1]), sovereign = isTRUE(ro$sovereign[1]),
    eu_member = isTRUE(ro$eu_member[1]), network_node = country_chr_or_null(ro$network_node[1]),
    agreements = country_arr(country_agreements_of(ctx$agreements, iso3)),
    eu_agreements = country_arr(if (isTRUE(ro$eu_member[1])) country_agreements_of(ctx$agreements, "EUU") else character()),
    bloc = country_chr_or_null(ro$bloc[1]), alignment_s = country_fv(ro$s_axis[1], "index"),
    alignment_basis = country_chr_or_null(ro$alignment_basis[1]), alignment_from = country_chr_or_null(ro$alignment_from[1]),
    status = status, status_reason = status_reason, has_si = si$n > 0, policy_basis = policy_basis,
    as_of = ctx$meta$as_of, as_of_label = ctx$meta$as_of_label, created = ctx$meta$created,
    strategic_bundle_id = ctx$meta$strategic_bundle_id %||% NULL, sector_bundle_id = ctx$meta$sector_bundle_id %||% NULL,
    rank_threshold = list(v = ctx$meta$rank_threshold %||% 0.5, d = "half"),
    trade_year = as.character(ctx$meta$trade_year), un_window = ctx$meta$un_window, cmfdi_edition = ctx$meta$cmfdi_edition,
    memo_eligible = identical(ro$tier[1], "core")
  )

  sheet <- list(
    schema = "osi-country-facts", schema_version = COUNTRY_SCHEMA_VERSION, iso3 = iso3,
    meta = meta, words = cfg$words,
    src_map = lapply(cfg$src_map, function(e) list(prefix = e$prefix, src = e$src)),
    summary = summary,
    cells = cells,
    priority_sectors = country_arr(ps$keys), priority_rule = ps$rule,
    policy = c(list(cat = cat_r), pams$facts, list(agreement = pp$agreement, nipo = nipo)),
    partners = partners,
    network = net
  )
  extra <- list(pick_share = ctx$meta$pick_share_d %||% COUNTRY_DASH,
                eu_records = if (!is.null(ctx$eu_records)) country_fmt(ctx$eu_records, "count") else COUNTRY_DASH,
                sovereign_name = if (!is.na(ro$alignment_from[1])) ctx$party_fn(ro$alignment_from[1])$name else "its sovereign")
  sheet$caveats <- country_caveats(sheet, cfg, extra)
  raw_src <- unlist(lapply(cells, function(c) lapply(c(c$drivers$es, c$drivers$eo), function(d) lapply(d$vars, function(v) v$raw$source))))
  # a note naming another source (config raw_note_sources: eia_production -> eia_intl) lists that source too
  raw_note <- unique(unlist(lapply(cells, function(c) lapply(c(c$drivers$es, c$drivers$eo), function(d) lapply(d$vars, function(v) v$raw$note)))))
  raw_src <- c(raw_src, unlist(lapply(raw_note[!is.na(raw_note)], function(n) cfg$raw_note_sources[[n]])))
  sheet$sources <- country_sources(c(vapply(cfg$src_map, function(e) e$src, ""), raw_src, if (!is.null(nipo) && isTRUE(nipo$covered)) "gta_nipo"), ctx$dict, cfg,
                                   ctx$sector_sources)
  # Drivers in the sheet only for the cells a reader meets first and the memo discusses (priority
  # sectors, top five Strategic Index sectors, the four lowest Energy Security and highest Economic
  # Opportunity sectors, the allied pick); the others go to the companion drivers file (sheet size).
  n_keep <- cfg$drivers_in_sheet %||% list(si = 5, es = 4, eo = 4)
  keep <- unique(c(ps$keys, utils::head(si$si_order, n_keep$si %||% 5), utils::head(oo$es_order, n_keep$es %||% 4),
                   utils::head(oo$eo_order, n_keep$eo %||% 4), if (!is.null(net$pick)) net$pick$cell))
  extra <- list()
  for (i in seq_along(sheet$cells)) {
    k <- sheet$cells[[i]]$key
    has <- length(sheet$cells[[i]]$drivers$es) + length(sheet$cells[[i]]$drivers$eo) > 0
    if (has && !k %in% keep) {
      extra[[k]] <- sheet$cells[[i]]$drivers
      sheet$cells[[i]]$drivers <- list(es = list(), eo = list())
      sheet$cells[[i]]$drivers_in_sheet <- FALSE
    } else sheet$cells[[i]]$drivers_in_sheet <- TRUE
  }
  sheet <- country_strip(sheet)
  sheet$checks <- list(built_with = "scripts/88", facts_sha256 = country_sha256(as.character(country_json(sheet))))
  attr(sheet, "drivers_extra") <- country_strip(extra)
  sheet
}

# The companion drivers file of a sheet: the drivers of the cells not carried in the sheet.
country_drivers_doc <- function(sheet, extra) {
  list(schema = "osi-country-drivers", schema_version = COUNTRY_SCHEMA_VERSION, iso3 = sheet$iso3,
       facts_sha256 = sheet$checks$facts_sha256, cells = if (length(extra)) extra else country_obj())
}

# A sheet with the companion drivers merged back (the memo tooling and the memo writers' copies).
country_merge_drivers <- function(sheet, doc) {
  if (is.null(doc) || !length(doc$cells)) return(sheet)
  for (i in seq_along(sheet$cells)) {
    d <- doc$cells[[sheet$cells[[i]]$key]]
    if (!is.null(d)) {
      sheet$cells[[i]]$drivers <- d
      sheet$cells[[i]]$drivers_in_sheet <- TRUE
    }
  }
  sheet
}

# The manifest (A8.1). entries: one list per roster economy (iso3, name, tier, region, eu_member,
# status, file, bytes_gz, n_placed, quadrants, memo).
country_manifest <- function(entries, meta, sources, validation, counts) {
  list(
    schema = "osi-country", schema_version = COUNTRY_SCHEMA_VERSION, created = meta$created, git_commit = meta$git_commit %||% NULL,
    as_of = meta$as_of, as_of_label = meta$as_of_label, strategic_bundle_id = meta$strategic_bundle_id %||% NULL,
    sector_bundle_id = meta$sector_bundle_id %||% NULL, label = meta$label, counts = counts,
    validated = isTRUE(meta$validated), synthetic = isTRUE(meta$synthetic),
    nipo = if (is.null(meta$nipo)) NULL else list(vintage = meta$nipo$vintage %||% NULL, vintage_label = meta$nipo$vintage_label %||% NULL),
    countries = entries, sources = sources, validation = validation
  )
}

# The manifest entry of one economy.
country_manifest_entry <- function(ro, sheet, file, bytes_gz, memo = NULL) {
  q <- if (!is.null(sheet)) lapply(sheet$summary$quadrants, function(f) f$v) else NULL
  list(
    iso3 = ro$iso3, name = ro$name, tier = ro$tier, region = country_chr_or_null(ro$region), eu_member = isTRUE(ro$eu_member),
    iso_n3 = country_chr_or_null(ro$iso_n3),
    status = if (is.null(sheet)) "no_data" else sheet$meta$status, file = file, bytes_gz = bytes_gz,
    n_placed = if (is.null(sheet)) NULL else sheet$summary$n_placed$v, quadrants = q, memo = memo
  )
}
