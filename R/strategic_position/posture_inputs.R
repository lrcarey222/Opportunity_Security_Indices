# Assembles the decision engine's input frame (R/strategic_position/posture_engine.R) from the curated
# node attributes and the measured evidence (docs/strategic_position.md, section 7). One row per
# country x technology x six-stage x layer. Pure functions: no IO. Needs sp_helpers.R and
# R/utils/supply_chain_stages.R.
#
# What comes from where:
#   node attributes (assessment): consequence, timing, substitutability, learning, leap, market scope
#   supply structure (data):      home_share, coercive and trusted shares of world supply
#   import exposure (data):       the country's own imports from coercive suppliers, import HHI, value
#   Competitive Position (data):  complementary capabilities (adjacent stages), domestic market
#   EO feas (data):               HS4 export proximity, the cross-product side of complementarity
# Observed policy is not an input.

sp_posture_first_col <- function(tbl, candidates, label) {
  hit <- intersect(candidates, names(tbl))
  if (length(hit) == 0) stop(label, ": none of the columns ", paste(candidates, collapse = ", "))
  hit[1]
}

# Within-cell percentile of a measure across countries (e.g. EO feas component values).
sp_cell_pct <- function(tbl, value_col, by = c("tech", "supply_chain")) {
  tbl %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dplyr::mutate(.pct = sp_midrank_pct(.data[[value_col]])) %>%
    dplyr::ungroup() %>%
    dplyr::pull(".pct")
}

# Complementary capabilities and an established adjacent stage, per iso3 x tech x stage, from the
# country's position in the same technology's adjacent producing stages (SP_STAGE_ADJACENT) and its
# HS4 export proximity (feas percentile).
sp_posture_adjacency <- function(cells, cfg) {
  comp <- cfg$complementary %||% list(adjacent_share_yes = 0.05, adjacent_cp_pct_yes = 0.8, adjacent_cp_pct_partly = 0.6, feas_pct_partly = 0.75)
  strong <- (cfg$position %||% list(strong = 0.10))$strong
  pairs <- do.call(rbind, lapply(names(SP_STAGE_ADJACENT), function(s) {
    adj <- SP_STAGE_ADJACENT[[s]]
    if (length(adj) == 0) return(NULL)
    data.frame(supply_chain = s, adjacent_stage = adj, stringsAsFactors = FALSE)
  }))
  # the adjacent stage's strength relative to the country's other cells: across ~218 economies a large
  # economy is above the median almost everywhere, so only the within-country view separates them
  adj_vals <- cells %>%
    dplyr::group_by(.data$iso3) %>%
    dplyr::mutate(adj_cp = sp_midrank_pct(.data$competitive_position_percentile)) %>%
    dplyr::ungroup() %>%
    dplyr::select("iso3", "tech", adjacent_stage = "supply_chain", adj_share = "home_share", "adj_cp")
  agg <- cells %>%
    dplyr::select("iso3", "tech", "supply_chain") %>%
    dplyr::inner_join(pairs, by = "supply_chain", relationship = "many-to-many") %>%
    dplyr::left_join(adj_vals, by = c("iso3", "tech", "adjacent_stage")) %>%
    dplyr::group_by(.data$iso3, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(
      adj_any = any(!is.na(.data$adj_share) | !is.na(.data$adj_cp)),
      adj_yes = any(!is.na(.data$adj_share) & !is.na(.data$adj_cp) &
        .data$adj_share >= comp$adjacent_share_yes & .data$adj_cp >= comp$adjacent_cp_pct_yes),
      # partial complementarity still needs a real (if small) world presence in the adjacent stage
      adj_partly = any(!is.na(.data$adj_cp) & .data$adj_cp >= comp$adjacent_cp_pct_partly &
        !is.na(.data$adj_share) & .data$adj_share >= (comp$adjacent_share_partly %||% 0.005)),
      adjacent_established = if (all(is.na(.data$adj_share))) NA else any(.data$adj_share >= strong, na.rm = TRUE),
      adjacent_best_stage = {
        s <- .data$adj_share
        if (all(is.na(s))) NA_character_ else .data$adjacent_stage[which.max(dplyr::coalesce(s, -1))]
      },
      .groups = "drop"
    )
  out <- cells %>%
    dplyr::select("iso3", "tech", "supply_chain", "feas_pct") %>%
    dplyr::left_join(agg, by = c("iso3", "tech", "supply_chain")) %>%
    dplyr::mutate(
      feas_partly = !is.na(.data$feas_pct) & .data$feas_pct >= comp$feas_pct_partly,
      complementary = dplyr::case_when(
        .data$adj_yes %in% TRUE ~ "yes",
        .data$adj_partly %in% TRUE | .data$feas_partly ~ "partly",
        .data$adj_any %in% TRUE | !is.na(.data$feas_pct) ~ "no",
        TRUE ~ NA_character_
      )
    )
  out %>% dplyr::select("iso3", "tech", "supply_chain", "complementary", "adjacent_established", "adjacent_best_stage", "feas_pct")
}

# Builds the engine input frame.
#   nodes      resolved node attributes (posture_nodes.R pn_resolve()): iso3, tech, stage or
#              supply_chain, layer_id, layer_label, layer_type and the attribute columns
#   supply     supply structure per iso3 x tech x supply_chain (supply_structure.R)
#   supply_minerals optional per-mineral supply structure (iso3, tech, supply_chain, mineral, ...)
#   imports    import exposure (importer_iso or iso3, tech, supply_chain, imports_usd,
#              coercive_import_share, import_hhi)
#   cp_cells   Competitive Position cells (Country, tech, supply_chain, competitive_position_percentile,
#              cp_deployment_scale)
#   feas       Country, tech, supply_chain, feas (EO Trade feas component value), optional
#   premium    iso3, tech, supply_chain, premium_pct, optional
#   context    Country x tech x supply_chain columns carried for confidence and cards
#              (strategic_need_percentile, stage_evidence_quality_cp), optional
#   countries  Country, iso3 for the assessed countries
#   exclude_iso3 economies not classified (entrepots and ship registries)
#   supply_measures optional supply structure for every measure (iso3, tech, supply_chain, measure, ...)
#   coercion_override optional curated coercive shares (tech, supply_chain, layer_id,
#              coercive_top_share, coercive_top_iso3)
sp_posture_inputs <- function(nodes, supply, imports, cp_cells, countries, cfg = list(),
                              supply_minerals = NULL, supply_measures = NULL, feas = NULL, premium = NULL,
                              context = NULL, exclude_iso3 = character(), coercion_override = NULL,
                              tech_ghg = NULL) {
  stage_col <- sp_posture_first_col(nodes, c("supply_chain", "stage"), "sp_posture_inputs() nodes")
  nodes <- dplyr::rename(nodes, supply_chain = dplyr::all_of(stage_col))
  if (!"Country" %in% names(nodes)) nodes <- dplyr::left_join(nodes, countries, by = "iso3")
  imp_iso <- sp_posture_first_col(imports, c("iso3", "importer_iso"), "sp_posture_inputs() imports")
  imports <- dplyr::rename(imports, iso3 = dplyr::all_of(imp_iso))
  min_imp <- cfg$exposure_min_import_usd %||% 1e6

  cp <- cp_cells %>%
    dplyr::left_join(countries, by = "Country") %>%
    dplyr::select("iso3", "tech", "supply_chain", "competitive_position_percentile",
                  dplyr::any_of(c("cp_deployment_scale", "cp_resource_endowment")))
  # Resource base at Extraction: the country's resource-endowment family above the cell's floor
  # (reserves are a structural zero for non-holders).
  if ("cp_resource_endowment" %in% names(cp)) {
    cp <- cp %>%
      dplyr::group_by(.data$tech, .data$supply_chain) %>%
      dplyr::mutate(resource_base = ifelse(is.na(.data$cp_resource_endowment), NA,
        .data$cp_resource_endowment > suppressWarnings(min(.data$cp_resource_endowment, na.rm = TRUE)) + 1e-9)) %>%
      dplyr::ungroup()
  }

  # Domestic market and deployment: the country's Deployment position in the technology, and whether
  # it deploys the technology at all (generation above the cell's floor).
  dep <- cp %>%
    dplyr::filter(.data$supply_chain == "Deployment") %>%
    dplyr::group_by(.data$tech) %>%
    dplyr::mutate(
      .floor = if ("cp_deployment_scale" %in% names(.)) suppressWarnings(min(.data$cp_deployment_scale, na.rm = TRUE)) else NA_real_,
      deploys = if ("cp_deployment_scale" %in% names(.)) ifelse(is.na(.data$cp_deployment_scale), NA, .data$cp_deployment_scale > .data$.floor + 1e-9) else NA
    ) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(.data$iso3, .data$tech, market_pct = .data$competitive_position_percentile, .data$deploys)

  sup_cols <- intersect(c("home_share", "home_share_lo", "home_share_hi", "coercive_top_share", "coercive_top_iso3",
                          "coercive_world_share", "trusted_world_share", "world_top_iso3", "world_top_share",
                          "driving_mineral", "driving_home_share"), names(supply))
  cells <- supply %>%
    dplyr::select("iso3", "tech", "supply_chain", dplyr::all_of(sup_cols), cell_measure = dplyr::any_of("primary_measure")) %>%
    dplyr::full_join(cp, by = c("iso3", "tech", "supply_chain"))
  if (!is.null(feas)) {
    f <- feas %>%
      dplyr::left_join(countries, by = "Country") %>%
      dplyr::mutate(feas_pct = sp_cell_pct(., "feas")) %>%
      dplyr::select("iso3", "tech", "supply_chain", "feas_pct")
    cells <- dplyr::left_join(cells, f, by = c("iso3", "tech", "supply_chain"))
  } else {
    cells$feas_pct <- NA_real_
  }
  adj <- sp_posture_adjacency(cells, cfg)

  imp <- imports %>%
    dplyr::select("iso3", "tech", "supply_chain", dplyr::any_of(c("imports_usd", "imports_to_gdp", "coercive_import_share", "trusted_import_share", "import_hhi")))

  x <- nodes %>%
    dplyr::filter(.data$iso3 %in% countries$iso3) %>%
    dplyr::left_join(dplyr::select(cells, -dplyr::any_of(c("competitive_position_percentile", "cp_deployment_scale",
                                                            "cp_resource_endowment", "feas_pct"))),
                     by = c("iso3", "tech", "supply_chain")) %>%
    dplyr::left_join(adj, by = c("iso3", "tech", "supply_chain")) %>%
    dplyr::left_join(imp, by = c("iso3", "tech", "supply_chain")) %>%
    dplyr::left_join(dep, by = c("iso3", "tech"))

  # A control layer at Deployment rides on installed equipment: its vendor exposure and the
  # self-supply test use the technology's Assembly imports and Assembly world share (Deployment has
  # no traded basket).
  ctrl_dep <- x$layer_type %in% "control" & x$supply_chain == "Deployment"
  if (any(ctrl_dep)) {
    asm <- imp %>% dplyr::filter(.data$supply_chain == "Assembly") %>% dplyr::select(-"supply_chain")
    idx <- match(paste(x$iso3, x$tech)[ctrl_dep], paste(asm$iso3, asm$tech))
    for (col in setdiff(names(asm), c("iso3", "tech"))) x[[col]][ctrl_dep] <- asm[[col]][idx]
    asm_home <- cells %>% dplyr::filter(.data$supply_chain == "Assembly")
    idx <- match(paste(x$iso3, x$tech)[ctrl_dep], paste(asm_home$iso3, asm_home$tech))
    for (col in intersect(c("home_share", "home_share_lo", "home_share_hi"), names(asm_home))) {
      x[[col]][ctrl_dep] <- asm_home[[col]][idx]
    }
  }

  # Layers with their own world-supply measure: a material layer reads its mineral (at the mineral's
  # own stage, whatever stage the layer sits at: magnets are judged on rare-earth refining), any other
  # layer the measure named on its row when the cell has it. The largest supplier's share (world_top_share)
  # swaps with the rest, so the large-yard case's diversification reads the layer's own supply
  # (strategic_value.R); the Secure engine does not read it.
  sup_swap <- c("home_share", "home_share_lo", "home_share_hi", "coercive_top_share", "coercive_top_iso3",
                "coercive_world_share", "trusted_world_share", "world_top_iso3", "world_top_share")
  x$supply_basis <- "cell"
  swap_from <- function(x, src, keys, basis) {
    src <- src[!duplicated(src[keys]), c(keys, intersect(sup_swap, names(src))), drop = FALSE]
    names(src)[-seq_along(keys)] <- paste0(names(src)[-seq_along(keys)], ".src")
    y <- dplyr::left_join(x, src, by = keys)
    hit <- !is.na(y$coercive_top_share.src) | !is.na(y$home_share.src)
    for (col in intersect(sup_swap, names(x))) {
      sc <- paste0(col, ".src")
      if (sc %in% names(y)) y[[col]][hit] <- y[[sc]][hit]
    }
    y$supply_basis[hit] <- basis
    y[, !grepl("\\.src$", names(y))]
  }
  if (!is.null(supply_measures) && "primary_measure" %in% names(x)) {
    meas <- dplyr::rename(supply_measures, primary_measure = "measure")
    # whole-cell nodes keep the cell's measure (config supply_measure_priority); only layers swap
    own <- !x$layer_type %in% "stage" & !x$primary_measure %in% c(NA, "", "none") & (is.na(x$mineral) | x$mineral == "")
    if (any(own)) x[own, ] <- swap_from(x[own, ], meas, c("iso3", "tech", "supply_chain", "primary_measure"), "layer_measure")
  }
  if (!is.null(supply_minerals) && "mineral" %in% names(x)) {
    min_tbl <- supply_minerals %>%
      dplyr::mutate(mineral_measure = .data$measure) %>%
      dplyr::arrange(.data$iso3, .data$mineral, .data$measure)
    is_min <- !is.na(x$mineral) & x$mineral != ""
    if (any(is_min)) {
      # The named measure first, then any other listing of the same mineral (PGMs are only in EI).
      first <- swap_from(x[is_min, ], dplyr::rename(min_tbl, primary_measure = "mineral_measure"),
                         c("iso3", "primary_measure", "mineral"), "mineral")
      left <- first$supply_basis != "mineral"
      if (any(left)) first[left, ] <- swap_from(first[left, ], min_tbl, c("iso3", "mineral"), "mineral_other_measure")
      x[is_min, ] <- first
    }
  }
  # Curated coercive shares where trade codes cannot separate the layer (HALEU from LEU). With an
  # iso3 column the override applies only to the countries listed (the caller keeps those for which
  # the named supplier is coercive).
  if (!is.null(coercion_override)) {
    ov_keys <- intersect(c("iso3", "tech", "supply_chain", "layer_id"), names(coercion_override))
    o <- coercion_override[, c(ov_keys, "coercive_top_share", "coercive_top_iso3")]
    names(o)[length(ov_keys) + 1:2] <- c("ov_share", "ov_iso3")
    x <- dplyr::left_join(x, o, by = ov_keys)
    hit <- !is.na(x$ov_share)
    x$coercive_top_share[hit] <- x$ov_share[hit]
    x$coercive_top_iso3[hit] <- x$ov_iso3[hit]
    x$supply_basis[hit] <- "assessment_override"
    x$ov_share <- NULL
    x$ov_iso3 <- NULL
  }
  # A home share the listing does not quantify is bracketed by [home_share_lo, home_share_hi]; where
  # the whole bracket gives the same answer on the position test, the bound settles it.
  # The bound is for the position test only: exposure (below) reads the lower bound, so an unlisted
  # country does not become a producer at scale on the world residual.
  pos <- cfg$position %||% list(strong = 0.10, moderate = 0.03)
  x$home_share_exposure <- x$home_share
  if (all(c("home_share_lo", "home_share_hi") %in% names(x))) {
    unlisted <- is.na(x$home_share)
    x$home_share_exposure[unlisted] <- x$home_share_lo[unlisted]
    below <- unlisted & !is.na(x$home_share_hi) & x$home_share_hi < pos$moderate
    above <- unlisted & !is.na(x$home_share_lo) & x$home_share_lo >= pos$strong
    x$home_share[below] <- x$home_share_hi[below]
    x$home_share[above] <- x$home_share_lo[above]
  }
  # Delivery compares the home share with trusted supply of the same measure: in mineral cells the
  # trusted share is the driving mineral's, so the home share there is too; a layer with its own
  # measure or mineral already carries that measure's home share.
  x$delivery_home_share <- if ("driving_home_share" %in% names(x)) dplyr::coalesce(x$driving_home_share, x$home_share) else x$home_share
  swapped <- x$supply_basis != "cell"
  x$delivery_home_share[swapped] <- x$home_share[swapped]
  if (!is.null(premium)) {
    x <- dplyr::left_join(x, dplyr::select(premium, "iso3", "tech", "supply_chain", "premium_pct"), by = c("iso3", "tech", "supply_chain"))
  } else {
    x$premium_pct <- NA_real_
  }
  if (!is.null(context)) {
    x <- dplyr::left_join(x, context, by = c("Country", "tech", "supply_chain"))
  }

  # Exposure (Test 1): the country depends on the node at a scale where its disruption matters: its
  # imports are material relative to its economy, it produces a meaningful share of world supply, or it
  # is in the upper half of the technology's deployers. Fleet-scale consequences (a grid storage fleet,
  # a motor industry) do not arise from a million dollars of imports.
  itg_min <- cfg$exposure_material_imports_to_gdp %||% 0.0005
  home_min <- cfg$exposure_min_home_share %||% 0.005
  dep_pct <- cfg$exposure_deployment_pct %||% 0.5
  itg <- if ("imports_to_gdp" %in% names(x)) x$imports_to_gdp else rep(NA_real_, nrow(x))
  imports_known <- !is.na(x$imports_usd)
  material_imports <- imports_known & x$imports_usd >= min_imp & !is.na(itg) & itg >= itg_min
  produces <- !is.na(x$home_share_exposure) & x$home_share_exposure >= home_min
  # deployment: the Deployment position percentile (generation size is missing for some technologies,
  # e.g. EVs, so a missing size does not block it; a measured zero does)
  deploys_at_scale <- !x$deploys %in% FALSE & !is.na(x$market_pct) & x$market_pct >= dep_pct
  # downstream manufacturing: a country that makes the technology's components or assemblies
  # depends on its refined inputs and materials
  downstream <- cells %>%
    dplyr::filter(.data$supply_chain %in% SUPPLY_CHAIN_MANUFACTURING_STAGES) %>%
    dplyr::group_by(.data$iso3, .data$tech) %>%
    dplyr::summarise(downstream_home = suppressWarnings(max(.data$home_share, na.rm = TRUE)), .groups = "drop") %>%
    dplyr::mutate(downstream_home = ifelse(is.finite(.data$downstream_home), .data$downstream_home, NA_real_))
  x <- dplyr::left_join(x, downstream, by = c("iso3", "tech"))
  upstream_of_mfg <- x$supply_chain %in% c("Extraction", "Refining", "Components")
  produces <- produces | (upstream_of_mfg & !is.na(x$downstream_home) & x$downstream_home >= home_min)
  x$exposed <- dplyr::case_when(
    x$supply_chain == "Deployment" ~ ifelse(is.na(x$market_pct), NA, deploys_at_scale),
    material_imports | produces | deploys_at_scale ~ TRUE,
    imports_known ~ FALSE,
    TRUE ~ NA
  )
  x$exclusion <- dplyr::case_when(
    x$iso3 %in% exclude_iso3 ~ "entrepot",
    TRUE ~ NA_character_
  )
  if ("confidence" %in% names(x) && !"attr_confidence" %in% names(x)) x$attr_confidence <- x$confidence
  # Technology-level market outlook (catch-up) and climate cost (Test 5), from config and tech_ghg.
  outlook <- unlist(cfg$market_outlook %||% list())
  x$market_outlook <- if (length(outlook)) unname(outlook[x$tech]) else NA_character_
  if (!is.null(tech_ghg)) {
    line <- cfg$climate_cost_max_ghg_index %||% 0.5
    g <- stats::setNames(tech_ghg$ghg_index, tech_ghg$tech)
    x$climate_cost <- !is.na(g[x$tech]) & unname(g[x$tech]) < line
  }
  x
}
