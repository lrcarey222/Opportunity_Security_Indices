# Allied Industrial Base (AIB): the observed trade network at node grain and its baseline metrics
# (docs/aib_methodology.md, 3). Every metric is recomputed from flows summed to the node; no HS6-
# level score is averaged. Pure functions; scripts/31 reads the files.

aib_hhi <- function(shares) {
  shares <- shares[is.finite(shares) & shares > 0]
  if (!length(shares)) return(NA_real_)
  shares <- shares / sum(shares)
  sum(shares^2)
}

# Node flows from partnership_strength_build_trade_cells() run on aib_node_crosswalk() (tech is
# the node_id there). value is the reference year (value_last), value_hist the history year
# (value_first); a flow under `min_flow_usd` is treated as no relationship. Keeps a row when either
# year carries a relationship.
aib_node_flows <- function(trade_cells, min_flow_usd = 1e4) {
  require_columns(trade_cells, c("exporter_iso", "importer_iso", "tech", "value_first", "value_last"), label = "trade cells")
  v <- dplyr::coalesce(trade_cells$value_last, 0)
  h <- dplyr::coalesce(trade_cells$value_first, 0)
  keep <- v >= min_flow_usd | h >= min_flow_usd
  tibble::tibble(
    node_id = trade_cells$tech[keep],
    exporter_iso = trade_cells$exporter_iso[keep],
    importer_iso = trade_cells$importer_iso[keep],
    value = ifelse(v[keep] >= min_flow_usd, v[keep], 0),
    value_hist = ifelse(h[keep] >= min_flow_usd, h[keep], 0),
    source = if ("source_last" %in% names(trade_cells)) trade_cells$source_last[keep] else NA_character_,
    hist_observed = if ("source_first" %in% names(trade_cells)) !is.na(trade_cells$source_first[keep]) else NA
  )
}

# Total goods exports per economy for the Balassa RCA denominators: the reference-year Comtrade
# totals (comtrade_total_export.csv), filled for economies that did not report that year from the
# latest earlier year of the bulk totals (Viet Nam and the United Arab Emirates report 2023, not
# 2024). Rows are relabelled to `year` so trade_core_build_comtrade_rca() can read them; the
# actual year is kept in total_year.
aib_total_exports <- function(totals, bulk_totals = NULL, year = 2024) {
  pick <- function(tbl) {
    if (is.null(tbl) || !nrow(tbl)) return(NULL)
    yc <- intersect(c("period", "ref_year", "year"), names(tbl))[1]
    fc <- intersect(c("flow_code", "flow_direction"), names(tbl))[1]
    flow <- toupper(substr(as.character(tbl[[fc]]), 1, 1))
    partner_ok <- if ("partner_iso" %in% names(tbl)) tbl$partner_iso %in% c("W00", "World", "WLD") | is.na(tbl$partner_iso) else TRUE
    cmd_ok <- if ("cmd_code" %in% names(tbl)) toupper(as.character(tbl$cmd_code)) == "TOTAL" else TRUE
    tibble::tibble(
      reporter_iso = toupper(as.character(tbl$reporter_iso)),
      total_year = suppressWarnings(as.integer(tbl[[yc]])),
      primary_value = suppressWarnings(as.numeric(tbl$primary_value))
    )[flow == "X" & partner_ok & cmd_ok, , drop = FALSE] %>%
      dplyr::filter(!is.na(primary_value), primary_value > 0, !is.na(total_year), total_year <= year) %>%
      dplyr::group_by(reporter_iso, total_year) %>%
      dplyr::summarise(primary_value = sum(primary_value), .groups = "drop")
  }
  main <- pick(totals)
  main <- if (is.null(main)) NULL else main[main$total_year == year, , drop = FALSE]
  fill <- pick(bulk_totals)
  out <- main
  if (!is.null(fill)) {
    fill <- fill %>%
      dplyr::filter(!reporter_iso %in% main$reporter_iso) %>%
      dplyr::slice_max(total_year, n = 1, with_ties = FALSE, by = reporter_iso)
    out <- dplyr::bind_rows(main, fill)
  }
  if (is.null(out)) return(tibble::tibble(reporter_iso = character(), period = integer(), primary_value = numeric(), total_year = integer()))
  out %>% dplyr::mutate(period = as.integer(year)) %>% dplyr::select(reporter_iso, period, primary_value, total_year)
}

# Exporter metrics per node: observed exports (reference year, all markets), world export share
# (of the node's world trade), markets served and the Balassa RCA from the OSI trade theme's
# formula (trade_core_build_comtrade_rca(): node share of the economy's total exports over the
# world's, both world sums over economies present in both inputs). Production shares are joined
# later (aib_join_production()).
aib_exporter_metrics <- function(flows, total_exports, year = 2024) {
  require_columns(flows, c("node_id", "exporter_iso", "importer_iso", "value"), label = "node flows")
  ex <- flows %>%
    dplyr::filter(value > 0) %>%
    dplyr::group_by(node_id, exporter_iso) %>%
    dplyr::summarise(exports = sum(value), n_markets = dplyr::n_distinct(importer_iso), .groups = "drop") %>%
    dplyr::group_by(node_id) %>%
    dplyr::mutate(world_share = exports / sum(exports), export_rank = rank(-exports, ties.method = "first")) %>%
    dplyr::ungroup()
  ct <- tibble::tibble(
    reporter_iso = ex$exporter_iso, tech = ex$node_id, supply_chain = "node", sub_sector = "All",
    exports = ex$exports, imports = 0
  )
  rca <- trade_core_build_comtrade_rca(ct, total_exports, year = year) %>%
    dplyr::transmute(node_id = tech, exporter_iso = reporter_iso, total_exports, export_rca)
  ex %>%
    dplyr::left_join(rca, by = c("node_id", "exporter_iso")) %>%
    dplyr::mutate(rca_available = !is.na(export_rca))
}

# Importer metrics per node: observed imports (the demand proxy: trade-based, so it leaves out
# domestic production used at home), number of suppliers, the largest supplier and the import HHI.
aib_importer_metrics <- function(flows, china_iso = "CHN") {
  flows %>%
    dplyr::filter(value > 0) %>%
    dplyr::group_by(node_id, importer_iso) %>%
    dplyr::summarise(
      imports = sum(value),
      n_suppliers = dplyr::n_distinct(exporter_iso),
      top_supplier = exporter_iso[which.max(value)],
      top_supplier_share = max(value) / sum(value),
      china_share = sum(value[exporter_iso %in% china_iso]) / sum(value),
      import_hhi = sum((value / sum(value))^2),
      .groups = "drop"
    )
}

# Node summary: world trade, material exporters/importers, the largest exporter, export HHI,
# China's share, the share of value that rests on mirror (exporter-declared) flows, and the
# history-year value for the time-series check.
aib_node_summary <- function(flows, exporter_metrics, material_flow_usd = 1e6, china_iso = "CHN") {
  base <- flows %>%
    dplyr::group_by(node_id) %>%
    dplyr::summarise(
      world_trade = sum(value),
      world_trade_hist = sum(value_hist),
      mirror_share = if (sum(value) > 0) sum(value[source %in% "mirror"]) / sum(value) else NA_real_,
      n_relationships = sum(value > 0),
      .groups = "drop"
    )
  imp <- flows %>%
    dplyr::filter(value > 0) %>%
    dplyr::group_by(node_id, importer_iso) %>%
    dplyr::summarise(v = sum(value), .groups = "drop") %>%
    dplyr::group_by(node_id) %>%
    dplyr::summarise(n_importers = dplyr::n(), n_material_importers = sum(v >= material_flow_usd), .groups = "drop")
  ex <- exporter_metrics %>%
    dplyr::group_by(node_id) %>%
    dplyr::summarise(
      n_exporters = dplyr::n(),
      n_material_exporters = sum(exports >= material_flow_usd),
      top_exporter = exporter_iso[which.max(exports)],
      top_exporter_share = max(world_share),
      export_hhi = sum(world_share^2),
      china_export_share = sum(world_share[exporter_iso %in% china_iso]),
      missing_rca_share = sum(world_share[!rca_available]),
      n_rca_above_1 = sum(export_rca >= 1, na.rm = TRUE),
      .groups = "drop"
    )
  base %>%
    dplyr::left_join(ex, by = "node_id") %>%
    dplyr::left_join(imp, by = "node_id") %>%
    dplyr::mutate(growth = dplyr::if_else(world_trade_hist > 0, world_trade / world_trade_hist - 1, NA_real_))
}

# Production or capacity shares per (node, country) from the configured proxies
# (config/aib.yml production_proxies): `major_supplier_shares` rows (measure, mineral, world_share)
# and GCIM current operational capacity (technology, product, country, value). Returns node_id,
# iso3, production_share, production_source.
aib_production_shares <- function(proxies, node_cells, major_supplier_shares = NULL, gcim = NULL) {
  out <- list()
  for (p in proxies) {
    ids <- unique(node_cells$node_id[node_cells$sub_sector == p$sub_sector])
    if (!length(ids)) next
    rows <- NULL
    if (identical(p$source, "major_supplier_shares") && !is.null(major_supplier_shares)) {
      # A measure without minerals (uranium mine production) is matched on the measure alone.
      ms <- major_supplier_shares[major_supplier_shares$measure == p$measure &
                                    (if (is.null(p$mineral)) TRUE else dplyr::coalesce(tolower(major_supplier_shares$mineral) == tolower(p$mineral), FALSE)) &
                                    !dplyr::coalesce(as.logical(major_supplier_shares$is_aggregate), FALSE), , drop = FALSE]
      if (nrow(ms)) {
        ms <- ms[ms$year == max(ms$year, na.rm = TRUE), , drop = FALSE]
        rows <- ms %>%
          dplyr::group_by(iso3) %>%
          dplyr::summarise(production_share = max(world_share, na.rm = TRUE), .groups = "drop") %>%
          dplyr::mutate(production_source = paste0(p$measure, if (is.null(p$mineral)) "" else paste0(": ", p$mineral), " (", max(ms$year), ")"))
      }
    }
    if (identical(p$source, "gcim") && !is.null(gcim)) {
      g <- gcim[tolower(gcim$technology) == tolower(p$technology) & tolower(gcim$product) == tolower(p$product), , drop = FALSE]
      if (nrow(g) && sum(g$value, na.rm = TRUE) > 0) {
        rows <- g %>%
          dplyr::group_by(iso3) %>%
          dplyr::summarise(v = sum(value, na.rm = TRUE), .groups = "drop") %>%
          dplyr::mutate(production_share = v / sum(v), production_source = paste0("GCIM operational capacity: ", p$technology, " ", p$product)) %>%
          dplyr::select(-v)
      }
    }
    if (is.null(rows) || !nrow(rows)) next
    out[[length(out) + 1]] <- tidyr::expand_grid(node_id = ids, rows)
  }
  if (!length(out)) return(tibble::tibble(node_id = character(), iso3 = character(), production_share = numeric(), production_source = character()))
  dplyr::bind_rows(out) %>% dplyr::distinct(node_id, iso3, .keep_all = TRUE)
}

# Merge nodes into one aggregated node (data-quality fallback): their flows are summed under the
# new node_id and every metric is recomputed from the summed flows by the caller.
aib_merge_node_flows <- function(flows, merge_map) {
  if (is.null(merge_map) || !nrow(merge_map)) return(flows)
  idx <- match(flows$node_id, merge_map$node_id)
  flows$node_id[!is.na(idx)] <- merge_map$merged_id[idx[!is.na(idx)]]
  flows %>%
    dplyr::group_by(node_id, exporter_iso, importer_iso) %>%
    dplyr::summarise(
      source = source[which.max(value)],
      hist_observed = any(hist_observed),
      value_hist = sum(value_hist),
      value = sum(value),
      .groups = "drop"
    )
}
