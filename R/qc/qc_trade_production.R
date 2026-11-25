# Trade is not production (docs/QC_SYSTEM_DESIGN.md 6.2, 1.5 item 5): diagnostics for cells where export evidence
# (the Comtrade / Atlas trade benchmark and the export_position Competitive Position family) says an economy is a
# supplier while production or capacity evidence says otherwise, or says nothing.
#
# qc_production_evidence() gathers the production / capacity evidence the pipeline holds into one long table
# (never inventing a value: an economy a source does not list is "not listed", not zero); qc_trade_production()
# compares it with the trade evidence per country x tech x stage and returns one row per flagged cell with the
# evidence values and source names. Flags:
#   trade_without_production  high export share, the stage has production / capacity evidence, and this economy
#                             has none (measured zero, or not listed)                                    warn
#   export_exceeds_production export share at least `ratio` times the measured production share         warn
#   entrepot_signature        a re-export candidate: high exports and stage imports at least
#                             `reexport_ratio` x exports with no production evidence where the stage is
#                             measured, or a known re-export hub (cfg$hubs) with high exports and no
#                             production evidence                                          warn + human review
#   trade_driven_position     a top-quarter Competitive Position or a Preserve choice resting mainly on the
#                             export_position family, or a Catch Up whose pathway anchors are all trade
#                             measures, while production evidence is weak or absent        warn + human review
#   no_production_data_stage  a high export share in a stage no production / capacity source covers: the
#                             gap is made explicit, never filled                                         info
# production_status on every row: measured_positive | measured_zero | not_listed (the stage is measured, this
# economy is not) | stage_not_covered. A projection (e.g. IEA ETP 2035 pipeline shares) is reported
# (projected_share) but never counts as production.
#
# Pure functions: no IO. Needs dplyr, tibble and qc_core.R.

QC_TP_DEFAULTS <- list(
  export_share_high = 0.02,  # a world export share that marks a supplier
  export_share_min = 0.005,  # the floor below which trade evidence is not read at all
  rca_high = 2,              # revealed comparative advantage that marks a specialist (with export_share_min)
  ratio = 3,                 # export share / production share that is flagged
  reexport_ratio = 0.5,      # stage imports / benchmark exports at or above which imports are "high"
  trade_dominance = 0.5,     # export_position's share of the observed CP family sum that makes it the main driver
  cp_high = 0.75,            # the top quarter of contenders (decision_framework.cp_preserve_pct)
  min_stage_countries = 5,   # economies a source must measure in a tech x stage for the stage to count as covered
  hubs = c("HKG", "MAC", "SGP", "NLD", "BEL", "ARE", "PAN"),
  trade_anchors = c("direct_export_share")  # pathway anchors (pathway.R SP_PATHWAY_ANCHORS) that are trade measures
)

QC_CP_FAMILIES <- c("cp_export_position", "cp_production_position", "cp_manufacturing_capacity", "cp_investment_position",
                    "cp_cost_position", "cp_resource_endowment", "cp_deployment_scale", "cp_innovation_position")

# Source labels of the two Competitive Position families that measure production / capacity
# (config/strategic_position.yml competitive_position families).
QC_TP_FAMILY_SOURCE <- c(cp_production_position = "CP family production_position (Production/size: EI, IEA CM, Red Book)",
                         cp_manufacturing_capacity = "CP family manufacturing_capacity (GCIM operating capacity index)")

# Production / capacity evidence as one long table {iso3, tech, supply_chain, measure, kind, value, year, source}.
# kind: share (a measured share of world production or capacity), projected_share (a projection year), presence
# (a quantity: > 0 means the economy produces), family (a CP family percentile where structural zeros are 0),
# index (an index read as context only), resource (reserves: not production). tables: a named list of any of
# critical_minerals_production_tbl, production_depth_momentum_tbl, market_share_manufacturing_tbl,
# major_supplier_shares_tbl, cm_fdi_projects_tbl, critical_minerals_processing_tbl, and sp (the strategic position
# table, for its production_position and manufacturing_capacity families). as_of_year marks projections.
qc_production_evidence <- function(tables, as_of_year = NULL) {
  ev <- list()
  tidy <- function(x) as.data.frame(dplyr::ungroup(x))
  iso <- function(x) x[[qc_iso3_col(x) %||% "iso3c"]]
  add <- function(iso3, tech, stage, measure, kind, value, year, source) {
    if (!length(iso3)) return(invisible(NULL))
    ev[[length(ev) + 1L]] <<- tibble::tibble(iso3 = as.character(iso3), tech = as.character(tech), supply_chain = as.character(stage),
                                             measure = as.character(measure), kind = kind, value = as.numeric(value),
                                             year = as.integer(year), source = as.character(source))
  }
  proj <- function(y) if (is.null(as_of_year)) rep(FALSE, length(y)) else !is.na(y) & y > as_of_year
  if (!is.null(x <- tables[["critical_minerals_production_tbl"]])) {
    x <- tidy(x)
    r <- x[x$data_type %in% "raw" & grepl("Production$", x$variable) & !grepl("^Overall", x$variable), , drop = FALSE]
    add(iso(r), r$tech, r$supply_chain, paste0("critmin_production:", r$variable), "presence", r$value, r$Year, r$source)
  }
  if (!is.null(x <- tables[["production_depth_momentum_tbl"]])) {
    x <- tidy(x)
    r <- x[x$variable %in% "production_latest" & x$supply_chain != "Deployment", , drop = FALSE]
    if (nrow(r)) {
      tot <- stats::ave(dplyr::coalesce(r$value, 0), r$tech, r$supply_chain, FUN = function(v) sum(pmax(v, 0)))
      add(iso(r), r$tech, r$supply_chain, "production_latest", "presence", r$value, r$Year, r$source)
      add(iso(r), r$tech, r$supply_chain, "production_share_of_reporters", "share", ifelse(tot > 0, r$value / tot, NA_real_), r$Year, r$source)
    }
  }
  if (!is.null(x <- tables[["market_share_manufacturing_tbl"]])) {
    x <- tidy(x)
    r <- x[x$data_type %in% "raw" & x$variable %in% c("Overall Market Share", "market_share") & x$supply_chain != "Deployment", , drop = FALSE]
    add(iso(r), r$tech, r$supply_chain, paste0("manufacturing_share:", r$variable), ifelse(proj(r$Year), "projected_share", "share"), r$value, r$Year, r$source)
    r <- x[x$data_type %in% "raw" & x$variable %in% "production" & x$supply_chain != "Deployment", , drop = FALSE]
    add(iso(r), r$tech, r$supply_chain, "manufacturing_production", "presence", r$value, r$Year, r$source)
  }
  if (!is.null(x <- tables[["major_supplier_shares_tbl"]])) {
    x <- tidy(x)
    x <- x[!(x$is_aggregate %in% TRUE), , drop = FALSE]
    prod_m <- c("iea_mineral_mining", "iea_mineral_refining", "ei_production", "ei_mineral_mining", "uranium_mine_production",
                "iea_manufacturing_capacity")
    res_m <- c("usgs_reserves", "ei_reserves", "uranium_resources")
    lab <- function(r) paste0(r$measure, ifelse(is.na(r$mineral), "", paste0(":", r$mineral)))
    r <- x[x$measure %in% prod_m, , drop = FALSE]
    add(r$iso3, r$tech, r$supply_chain, lab(r), ifelse(proj(r$year), "projected_share", "share"), r$world_share, r$year, r$source)
    r <- x[x$measure %in% res_m, , drop = FALSE]
    add(r$iso3, r$tech, r$supply_chain, lab(r), "resource", r$world_share, r$year, r$source)
  }
  if (!is.null(x <- tables[["cm_fdi_projects_tbl"]])) {
    x <- tidy(x)
    x <- x[x$index_use %in% TRUE & !is.na(x$destination_iso3) & !is.na(x$stages), , drop = FALSE]
    if (nrow(x)) {
      st <- strsplit(x$stages, "|", fixed = TRUE)
      long <- tibble::tibble(iso3 = rep(x$destination_iso3, lengths(st)), tech = rep(x$tech, lengths(st)), supply_chain = unlist(st),
                             operational = rep(x$status %in% "Operational", lengths(st)))
      agg <- long |> dplyr::group_by(iso3, tech, supply_chain) |>
        dplyr::summarise(n_oper = sum(operational), n_all = dplyr::n(), .groups = "drop")
      add(agg$iso3, agg$tech, agg$supply_chain, "cm_fdi_operational_projects", "presence", agg$n_oper, NA, "CM-FDI (Net Zero Industrial Policy Lab)")
    }
  }
  if (!is.null(x <- tables[["critical_minerals_processing_tbl"]])) {
    x <- tidy(x)
    r <- x[x$variable %in% "Mineral Supply", , drop = FALSE]
    add(iso(r), r$tech, r$supply_chain, "critmin_mineral_supply_index", "index", r$value, r$Year, r$source)
  }
  if (!is.null(x <- tables[["sp"]])) {
    for (fam in names(QC_TP_FAMILY_SOURCE)) {
      if (!fam %in% names(x)) next
      r <- x[!is.na(x[[fam]]), , drop = FALSE]
      add(r$iso3, r$tech, r$supply_chain, fam, "family", r[[fam]], NA, QC_TP_FAMILY_SOURCE[[fam]])
    }
  }
  out <- dplyr::bind_rows(tibble::tibble(iso3 = character(), tech = character(), supply_chain = character(), measure = character(),
                                         kind = character(), value = numeric(), year = integer(), source = character()), ev)
  out[!is.na(out$iso3) & !is.na(out$value), ]
}

# Nodes (tech|stage keys) whose only measured supply share is the Atlas export share: there the pathway's
# direct_supply_share anchor (home_share_exposure) is itself a trade measure.
qc_trade_share_nodes <- function(major_supplier_shares) {
  if (is.null(major_supplier_shares) || !nrow(major_supplier_shares)) return(character())
  x <- as.data.frame(dplyr::ungroup(major_supplier_shares))
  res_m <- c("usgs_reserves", "ei_reserves", "uranium_resources")
  x <- x[!x$measure %in% res_m, c("tech", "supply_chain", "measure"), drop = FALSE]
  m <- tapply(x$measure, paste(x$tech, x$supply_chain, sep = "|"), function(v) all(unique(v) %in% "atlas_export_share"))
  names(m)[m %in% TRUE]
}

# Summarise the evidence per cell: measured (any production source lists the economy), positive (some production
# quantity, measured share, family or operational plant > 0; projections and indexes never count), the largest
# measured share and its measure, a short evidence string and the sources; and per tech x stage whether
# production evidence covers the stage.
qc_production_summary <- function(evidence, min_stage_countries = 5) {
  ev <- evidence[evidence$kind %in% c("share", "projected_share", "presence", "family"), ]
  real <- ev[ev$kind != "projected_share", ]
  cover <- real |> dplyr::group_by(tech, supply_chain) |>
    dplyr::summarise(stage_countries = length(unique(iso3)), stage_sources = paste(sort(unique(source)), collapse = "; "), .groups = "drop") |>
    dplyr::mutate(stage_covered = stage_countries >= min_stage_countries)
  fmt <- function(m, v) paste0(m, "=", format(signif(v, 3), trim = TRUE))
  cell <- ev |> dplyr::group_by(iso3, tech, supply_chain) |>
    dplyr::summarise(
      production_measured = any(kind != "projected_share"),
      production_positive = any(value > 0 & kind != "projected_share"),
      production_share = if (any(kind == "share")) max(value[kind == "share"]) else NA_real_,
      production_share_measure = if (any(kind == "share")) measure[kind == "share"][which.max(value[kind == "share"])] else NA_character_,
      projected_share = if (any(kind == "projected_share")) max(value[kind == "projected_share"]) else NA_real_,
      production_evidence = paste(utils::head(fmt(measure, value)[order(-value)], 4), collapse = "; "),
      production_sources = paste(sort(unique(source)), collapse = "; "),
      .groups = "drop")
  list(cover = cover, cell = cell)
}

# The trade-vs-production diagnostic. sp: the strategic position table; evidence: qc_production_evidence();
# trade_share_nodes: qc_trade_share_nodes() (optional). Returns one row per flagged cell (a cell can carry several
# flags, one row each), excluded rows (posture_exclusion) included, since their trade still enters contender sets.
qc_trade_production <- function(sp, evidence, cfg = QC_TP_DEFAULTS, trade_share_nodes = character()) {
  cfg <- utils::modifyList(QC_TP_DEFAULTS, cfg)
  s <- qc_production_summary(evidence, cfg$min_stage_countries)
  t <- sp[, intersect(c("iso3", "tech", "supply_chain", "strategic_choice", "posture_exclusion", "competitive_position_score",
                        "cp_contender_pct", "trade_benchmark_share", "trade_benchmark_rca", "trade_benchmark_exports",
                        "trade_benchmark_source", "trade_benchmark_period", "imports_usd", "catch_up_anchor_type", QC_CP_FAMILIES), names(sp)), drop = FALSE]
  t <- as.data.frame(t[t$supply_chain != "Deployment", , drop = FALSE])
  t <- dplyr::left_join(t, s$cover, by = c("tech", "supply_chain"))
  t <- dplyr::left_join(t, s$cell, by = c("iso3", "tech", "supply_chain"))
  t$stage_covered <- t$stage_covered %in% TRUE
  t$production_measured <- t$production_measured %in% TRUE
  t$production_positive <- t$production_positive %in% TRUE
  t$production_status <- ifelse(!t$stage_covered & !t$production_measured, "stage_not_covered",
                                ifelse(t$production_positive, "measured_positive", ifelse(t$production_measured, "measured_zero", "not_listed")))
  share <- qc_col(t, "trade_benchmark_share", NA_real_)
  rca <- qc_col(t, "trade_benchmark_rca", NA_real_)
  high_export <- !is.na(share) & (share >= cfg$export_share_high | (!is.na(rca) & rca >= cfg$rca_high & share >= cfg$export_share_min))
  fams <- intersect(QC_CP_FAMILIES, names(t))
  fam_m <- as.matrix(t[, fams, drop = FALSE])
  fam_sum <- rowSums(fam_m, na.rm = TRUE)
  n_fam <- rowSums(!is.na(fam_m))
  exp_f <- qc_col(t, "cp_export_position", NA_real_)
  t$n_cp_families <- n_fam
  t$export_weight <- ifelse(is.na(exp_f), NA_real_, ifelse(fam_sum > 0, exp_f / fam_sum, ifelse(n_fam == 1, 1, NA_real_)))
  trade_main <- !is.na(exp_f) & (n_fam == 1 | (!is.na(t$export_weight) & t$export_weight >= cfg$trade_dominance))
  ratio <- ifelse(!is.na(t$production_share) & t$production_share > 0, share / t$production_share, NA_real_)
  t$export_to_production <- ratio
  imp <- qc_col(t, "imports_usd", NA_real_)
  exps <- qc_col(t, "trade_benchmark_exports", NA_real_)
  t$import_export_ratio <- ifelse(!is.na(exps) & exps > 0, imp / exps, NA_real_)
  t$known_hub <- t$iso3 %in% cfg$hubs
  weak_prod <- !t$production_positive | (!is.na(ratio) & ratio >= cfg$ratio)
  cp_pct <- qc_col(t, "cp_contender_pct", NA_real_)
  choice <- qc_col(t, "strategic_choice", NA_character_)
  # Catch Up anchored only on trade measures (the export share, or a supply share that is the Atlas export share)
  anchors <- strsplit(dplyr::coalesce(as.character(qc_col(t, "catch_up_anchor_type", NA_character_)), ""), ";", fixed = TRUE)
  node <- paste(t$tech, t$supply_chain, sep = "|")
  trade_anchor <- vapply(seq_along(anchors), function(i) {
    a <- anchors[[i]]
    if (!length(a)) return(FALSE)
    ok <- a %in% cfg$trade_anchors | (a == "direct_supply_share" & node[i] %in% trade_share_nodes)
    all(ok)
  }, logical(1))
  t$catch_up_trade_anchored <- choice %in% "catch_up" & trade_anchor
  strong_pos <- (!is.na(cp_pct) & cp_pct >= cfg$cp_high) | choice %in% "preserve"
  reexport <- !is.na(t$import_export_ratio) & t$import_export_ratio >= cfg$reexport_ratio
  flags <- list(
    trade_without_production = high_export & t$stage_covered & !t$production_positive,
    export_exceeds_production = high_export & !is.na(ratio) & ratio >= cfg$ratio,
    entrepot_signature = high_export & !t$production_positive &
      ((t$stage_covered & reexport) | (t$known_hub & (reexport | t$stage_covered))),
    trade_driven_position = ((strong_pos & trade_main) | t$catch_up_trade_anchored) & weak_prod,
    no_production_data_stage = high_export & !t$stage_covered & !t$production_measured
  )
  status <- c(trade_without_production = "warn", export_exceeds_production = "warn", entrepot_signature = "warn",
              trade_driven_position = "warn", no_production_data_stage = "info")
  rows <- lapply(names(flags), function(f) {
    i <- which(flags[[f]])
    if (!length(i)) return(NULL)
    x <- t[i, , drop = FALSE]
    x$flag <- f
    x$status <- unname(status[f])
    x$needs_human_explanation <- f %in% c("trade_driven_position", "entrepot_signature") |
      (f == "trade_without_production" & choice[i] %in% c("preserve", "catch_up", "secure"))
    x$detail <- qc_tp_detail(f, x, cfg)
    x
  })
  out <- dplyr::bind_rows(rows)
  cols <- c("iso3", "tech", "supply_chain", "flag", "status", "needs_human_explanation", "strategic_choice", "posture_exclusion",
            "cp_contender_pct", "competitive_position_score", "n_cp_families", "cp_export_position", "cp_production_position",
            "cp_manufacturing_capacity", "export_weight", "catch_up_anchor_type", "catch_up_trade_anchored", "trade_benchmark_share",
            "trade_benchmark_rca", "trade_benchmark_source", "trade_benchmark_period", "trade_benchmark_exports", "imports_usd",
            "import_export_ratio", "known_hub", "production_status", "stage_covered", "stage_countries", "production_share",
            "production_share_measure", "projected_share", "export_to_production", "production_evidence", "production_sources",
            "stage_sources", "detail")
  if (is.null(out) || !nrow(out)) return(tibble::as_tibble(stats::setNames(replicate(length(cols), character(), simplify = FALSE), cols)))
  for (cn in setdiff(cols, names(out))) out[[cn]] <- NA
  out <- out[order(match(out$flag, names(flags)), -dplyr::coalesce(as.numeric(out$trade_benchmark_share), 0)), , drop = FALSE]
  tibble::as_tibble(out[, cols])
}

qc_tp_detail <- function(f, x, cfg) {
  pct <- function(v) ifelse(is.na(v), "NA", sprintf("%.1f%%", 100 * v))
  num <- function(v) ifelse(is.na(v), "NA", sprintf("%.2f", v))
  prod_txt <- c(measured_positive = "production measured", measured_zero = "production / capacity measured as zero",
                not_listed = "economy not listed by the production / capacity sources that cover this stage",
                stage_not_covered = "no production / capacity source covers this stage")[x$production_status]
  src <- ifelse(is.na(x$trade_benchmark_source), "source NA", x$trade_benchmark_source)
  switch(f,
    trade_without_production = sprintf("export share %s (%s) but %s (%d economies measured: %s)",
                                       pct(x$trade_benchmark_share), src, prod_txt, x$stage_countries, x$stage_sources),
    export_exceeds_production = sprintf("export share %s is %.1fx the measured production share %s (%s)", pct(x$trade_benchmark_share),
                                        x$export_to_production, pct(x$production_share), x$production_share_measure),
    entrepot_signature = sprintf("re-export candidate: export share %s (%s), stage imports %sx benchmark exports, %s%s",
                                 pct(x$trade_benchmark_share), src, num(x$import_export_ratio), prod_txt,
                                 ifelse(x$known_hub, "; a known re-export hub", "")),
    trade_driven_position = ifelse(x$catch_up_trade_anchored %in% TRUE,
      sprintf("Catch Up anchored only on trade (%s); export share %s; %s%s", x$catch_up_anchor_type, pct(x$trade_benchmark_share),
              ifelse(is.na(x$export_to_production), prod_txt, sprintf("export share %.1fx production share", x$export_to_production)),
              ifelse(x$supply_chain == "Recovery",
                     "; at Recovery the Competitive Position leaves export_position out because a large export share of scrap means it is shipped abroad for processing (config/strategic_position.yml), yet the pathway reads it as a capability anchor", "")),
      sprintf("choice %s, CP among contenders %s; export_position %s is %s of the %d observed CP families' sum; %s",
              x$strategic_choice, num(x$cp_contender_pct), num(x$cp_export_position), pct(x$export_weight), x$n_cp_families,
              ifelse(is.na(x$export_to_production), prod_txt, sprintf("export share %.1fx production share", x$export_to_production)))),
    no_production_data_stage = sprintf("export share %s (%s); no production or capacity source covers %s %s: no production evidence (not imputed)",
                                       pct(x$trade_benchmark_share), src, x$tech, x$supply_chain)
  )
}

# Summary check rows for the flags table (one per flag kind), so the trade-vs-production results appear in the
# main checks file too.
qc_trade_production_checks <- function(flags, table = "strategic_position_tbl x production evidence") {
  kinds <- c("trade_without_production", "export_exceeds_production", "entrepot_signature", "trade_driven_position", "no_production_data_stage")
  defaults <- c(trade_without_production = "warn", export_exceeds_production = "warn", entrepot_signature = "warn",
                trade_driven_position = "warn", no_production_data_stage = "info")
  out <- lapply(kinds, function(k) {
    x <- flags[flags$flag %in% k, , drop = FALSE]
    st <- if (!nrow(x)) "info" else unname(defaults[k])
    ch <- dplyr::coalesce(x$strategic_choice, "(excluded)")
    by_choice <- if (nrow(x)) { tt <- table(ch); paste(names(tt), tt, sep = "=", collapse = ", ") } else ""
    x <- x[order(-dplyr::coalesce(as.numeric(x$trade_benchmark_share), 0)), , drop = FALSE]
    qc_check(paste0("tp_", k), "trade_vs_production", table, st, nrow(x),
             sprintf("%d cells flagged %s%s", nrow(x), k, if (nzchar(by_choice)) paste0(" (choices: ", by_choice, ")") else ""),
             qc_examples(x, c("iso3", "tech", "supply_chain", "strategic_choice", "trade_benchmark_share", "cp_contender_pct", "production_status", "production_share")),
             needs_human_explanation = any(x$needs_human_explanation %in% TRUE))
  })
  qc_bind(out)
}
