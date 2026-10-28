# Allied Industrial Base (AIB): data-quality guardrails per sub-sector node
# (docs/aib_methodology.md, 4). Each node gets flags and one of three categories:
#   High confidence                every check passes
#   Usable with caution            the node can be optimized but a flag qualifies the reading
#   Insufficient for optimization  too little trade or too few traders to say anything bilateral
# Insufficient nodes of one technology stage are merged into an aggregated node instead of being
# optimized with false precision; an aggregate that still fails is reported, not solved.

aib_solar_revision_codes <- function() c("854140", "854142", "854143")

aib_data_quality <- function(nodes, node_codes, node_summary, cfg_dq, history_year = 2020) {
  hs2022 <- aib_hs2022_codes()
  codes <- node_codes %>%
    dplyr::group_by(node_id) %>%
    dplyr::summarise(
      n_hs6 = dplyr::n(),
      nonessential_share = mean(!essential),
      n_legacy = sum(legacy_hs),
      n_caveat = sum(grepl("kept with caveat", note, ignore.case = TRUE)),
      n_shared_codes = sum(n_nodes_sharing > 1),
      n_hs2022 = sum(hs6 %in% hs2022),
      solar_revision = any(hs6 %in% aib_solar_revision_codes()),
      vessel_codes = any(substr(hs6, 1, 2) == "89"),
      .groups = "drop"
    )
  q <- nodes %>%
    dplyr::select(node_id, sub_sector, supply_chain, dplyr::any_of("source_stage"), techs, stage_conflict,
                  dplyr::any_of(c("stage_mixed", "stage_mix"))) %>%
    dplyr::left_join(codes, by = "node_id") %>%
    dplyr::left_join(node_summary, by = "node_id")

  num <- function(x) dplyr::coalesce(as.numeric(x), 0)
  flags <- vector("list", nrow(q))
  insufficient <- logical(nrow(q))
  caution <- logical(nrow(q))
  for (i in seq_len(nrow(q))) {
    f <- character()
    r <- q[i, ]
    if (num(r$world_trade) < cfg_dq$min_world_trade_usd) {
      f <- c(f, sprintf("world trade %.0fm USD below %.0fm", num(r$world_trade) / 1e6, cfg_dq$min_world_trade_usd / 1e6))
      insufficient[i] <- TRUE
    }
    if (num(r$n_material_exporters) < cfg_dq$min_material_exporters) {
      f <- c(f, sprintf("%d material exporters (< %d)", as.integer(num(r$n_material_exporters)), cfg_dq$min_material_exporters))
      insufficient[i] <- TRUE
    }
    if (num(r$n_material_importers) < cfg_dq$min_material_importers) {
      f <- c(f, sprintf("%d material importers (< %d)", as.integer(num(r$n_material_importers)), cfg_dq$min_material_importers))
      insufficient[i] <- TRUE
    }
    if (num(r$mirror_share) >= cfg_dq$caution_mirror_share) {
      f <- c(f, sprintf("%.0f%% of value from exporter-declared mirror flows", 100 * num(r$mirror_share)))
      caution[i] <- TRUE
    }
    if (num(r$missing_rca_share) >= cfg_dq$caution_missing_rca_share) {
      f <- c(f, sprintf("specialization (RCA) missing for exporters holding %.0f%% of exports", 100 * num(r$missing_rca_share)))
      caution[i] <- TRUE
    }
    if (num(r$nonessential_share) >= cfg_dq$caution_nonessential_share) {
      f <- c(f, "most HS6 codes are not essential to the technology (broad headings)")
      caution[i] <- TRUE
    }
    if (num(r$n_caveat) > 0) {
      f <- c(f, sprintf("%d general-purpose code(s) kept with caveat: the technology is one use among several", as.integer(r$n_caveat)))
      caution[i] <- TRUE
    }
    if (num(r$n_shared_codes) > 0) {
      f <- c(f, sprintf("%d code(s) also counted in another node (multi-use: the value is not attributable to this technology alone)", as.integer(r$n_shared_codes)))
      caution[i] <- TRUE
    }
    if (num(r$n_legacy) > 0) {
      f <- c(f, sprintf("%d legacy-HS bridge code(s)", as.integer(r$n_legacy)))
      caution[i] <- TRUE
    }
    if (num(r$n_hs2022) > 0) {
      f <- c(f, sprintf("HS2022 revision: %d code(s) have no %d equivalent, so no comparable time series", as.integer(r$n_hs2022), history_year))
      caution[i] <- TRUE
    }
    if (isTRUE(r$solar_revision)) {
      f <- c(f, sprintf("solar HS revision: PV cells (8541.42) and modules (8541.43) split in HS2022; %d trade only as 8541.40 with LEDs, filed under cells", history_year))
      caution[i] <- TRUE
    }
    if (isTRUE(r$vessel_codes)) {
      f <- c(f, "vessel codes (HS 89): ship transfers are recorded by flag of registry, so open registries (Liberia, Marshall Islands, Panama, Bahamas) appear as traders")
      caution[i] <- TRUE
    }
    if (num(r$world_trade_hist) <= 0 && num(r$n_hs2022) == 0) {
      f <- c(f, sprintf("no %d trade observed", history_year))
      caution[i] <- TRUE
    }
    if (isTRUE(r$stage_conflict)) {
      f <- c(f, "technologies file the same codes in different stages")
      caution[i] <- TRUE
    }
    # The same idea at six stages: the node's codes carry several stage labels and the node is filed
    # under the one with the most world trade (aib_stage_by_trade()).
    if (isTRUE(r$stage_mixed)) {
      f <- c(f, sprintf("codes span several supply-chain stages (%s of trade), filed under %s", r$stage_mix, r$supply_chain))
      caution[i] <- TRUE
    }
    flags[[i]] <- f
  }
  q$flags <- vapply(flags, function(f) paste(f, collapse = "; "), character(1))
  q$data_quality <- dplyr::case_when(
    insufficient ~ "Insufficient for optimization",
    caution ~ "Usable with caution",
    TRUE ~ "High confidence"
  )
  q
}

# Merge map for insufficient nodes (aggregating upward, never inventing precision):
#   1. an insufficient node with a material-linked sibling in its technology stage (config/aib.yml
#      material_links: monazite -> rare-earth compounds, uranium ores -> uranium compounds) joins
#      that sibling (the largest by trade if several), which keeps its node_id and gets a "+" label;
#   2. the insufficient nodes left in one (primary technology, stage) are merged together when
#      there are at least two of them;
#   3. a node still alone stays as it is: reported, flagged, not optimized.
# "Stage" here is the source stage (the taxonomy's Upstream / Midstream / Downstream), so the merged
# ids ("<tech>-<source stage>--aggregated") are those of the three-stage build, which
# config/aib_strategic_compacts.yml and config/aib_node_functions.yml are keyed by. The label names the
# merged node's six-stage stage instead (since 2026-10-28; "Uranium/fuel Extraction: aggregated ..."): the
# members' six-stage label (`supply_chain` of `nodes`) holding the most of their world trade, a tie going
# to the earlier stage in chain order, which is the label scripts/31 then gives the merged node from its
# codes unless a member's codes are themselves mixed. Without `supply_chain` the source stage is named.
# Returns node_id, merged_id, merged_label; a sibling that absorbs others maps to itself.
aib_quality_merge_map <- function(quality, nodes, node_cells = NULL, edges = NULL) {
  empty <- tibble::tibble(node_id = character(), merged_id = character(), merged_label = character())
  bad_ids <- quality$node_id[quality$data_quality == "Insufficient for optimization"]
  if (!length(bad_ids)) return(empty)
  label_of <- function(id) nodes$sub_sector[match(id, nodes$node_id)]
  trade_of <- function(id) dplyr::coalesce(quality$world_trade[match(id, quality$node_id)], 0)
  out <- list()
  left <- bad_ids
  if (!is.null(node_cells) && !is.null(edges) && nrow(edges)) {
    mat <- edges[grepl("material link", edges$basis), , drop = FALSE]
    for (b in bad_ids) {
      stages <- dplyr::distinct(node_cells[node_cells$node_id == b, c("tech", "source_stage")])
      sibs <- unique(node_cells$node_id[paste(node_cells$tech, node_cells$source_stage) %in% paste(stages$tech, stages$source_stage)])
      sibs <- setdiff(sibs, bad_ids)
      linked <- intersect(sibs, c(mat$node_b[mat$node_a == b], mat$node_a[mat$node_b == b]))
      if (length(linked)) {
        target <- linked[which.max(trade_of(linked))]
        out[[length(out) + 1]] <- tibble::tibble(node_id = b, merged_id = target)
        left <- setdiff(left, b)
      }
    }
  }
  absorbed <- dplyr::bind_rows(out)
  if (nrow(absorbed)) {
    absorbed <- dplyr::bind_rows(absorbed, tibble::tibble(node_id = unique(absorbed$merged_id), merged_id = unique(absorbed$merged_id))) %>%
      dplyr::group_by(merged_id) %>%
      dplyr::mutate(merged_label = paste(c(label_of(merged_id[1]), sort(label_of(setdiff(node_id, merged_id[1])))), collapse = " + ")) %>%
      dplyr::ungroup()
  }
  rest <- nodes[nodes$node_id %in% left, intersect(c("node_id", "primary_tech", "source_stage", "supply_chain", "sub_sector"), names(nodes)), drop = FALSE]
  if (!"supply_chain" %in% names(rest)) rest$supply_chain <- rest$source_stage
  rest$trade <- if (nrow(rest)) rep_len(trade_of(rest$node_id), nrow(rest)) else numeric()
  # The six-stage label of a group of members: the stage with the most of their world trade (ties: chain order).
  label_stage <- function(stage, trade) {
    v <- tapply(trade, stage, sum)
    s <- names(v)[order(-as.numeric(v), match(names(v), SUPPLY_CHAIN_STAGES, nomatch = length(SUPPLY_CHAIN_STAGES) + 1L))]
    s[1]
  }
  grouped <- rest %>%
    dplyr::group_by(primary_tech, source_stage) %>%
    dplyr::filter(dplyr::n() >= 2) %>%
    dplyr::mutate(
      merged_id = paste0(aib_slug(primary_tech), "-", aib_slug(source_stage), "--aggregated"),
      merged_label = paste0(primary_tech, " ", label_stage(supply_chain, trade), ": aggregated low-coverage sub-sectors (", paste(sort(sub_sector), collapse = ", "), ")")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(node_id, merged_id, merged_label)
  res <- dplyr::bind_rows(absorbed, grouped)
  if (!nrow(res)) empty else dplyr::select(res, node_id, merged_id, merged_label)
}

# Where a production or capacity proxy exists, compare the largest producer's share with trade:
# when production is far more concentrated than exports (rare-earth refining, graphite anodes,
# polysilicon), trade-based exposure understates dependence and the node is flagged.
aib_production_flags <- function(quality, production, node_summary, gap = 0.25, china_iso = "CHN") {
  if (is.null(production) || !nrow(production)) return(quality)
  pr <- production %>%
    dplyr::group_by(node_id) %>%
    dplyr::summarise(
      production_top = iso3[which.max(production_share)],
      production_top_share = max(production_share),
      production_china_share = sum(production_share[iso3 == china_iso]),
      production_source = dplyr::first(production_source),
      .groups = "drop"
    )
  q <- quality %>% dplyr::select(-dplyr::any_of(c("production_top", "production_top_share", "production_china_share", "production_source"))) %>%
    dplyr::left_join(pr, by = "node_id")
  ce <- dplyr::coalesce(q$china_export_share, 0)
  hit <- !is.na(q$production_china_share) & (q$production_china_share - ce >= gap)
  msg <- sprintf("production more concentrated than trade: %s holds %.0f%% of %s vs %.0f%% of exports, so trade-based exposure understates dependence",
                 china_iso, 100 * q$production_china_share, q$production_source, 100 * ce)
  q$flags[hit] <- ifelse(nzchar(q$flags[hit]), paste(q$flags[hit], msg[hit], sep = "; "), msg[hit])
  q$data_quality[hit & q$data_quality == "High confidence"] <- "Usable with caution"
  q
}

# Physical validation of a node's class (config/aib.yml supply_constraint). `nodes` carries
# node_classification, shock_exposure, physical_data (complete / incomplete / none: how much of
# world production the proxy covers) and physically_divergent (physical and trade concentration
# differ by the threshold or more). In the production_constrained mode (`constrained`) a node with
# complete data had its suppliers bounded by physical capacity: that validates supplier capability,
# not exposure, which is still measured on trade. A LOW INTERVENTION or COORDINATE class is flagged
# PHYSICAL CAPABILITY NOT VALIDATED when the physical data are incomplete, or when they diverge from
# trade and either were not applied or the class rests on low trade exposure (a dependence that
# arrives embodied in downstream goods, as polysilicon does in wafers and cells).
aib_physical_validation <- function(nodes, constrained, low_exposure) {
  pd <- dplyr::coalesce(nodes$physical_data, "none")
  dv <- dplyr::coalesce(nodes$physically_divergent, FALSE)
  applied <- isTRUE(constrained) & pd == "complete"
  low_exp <- dplyr::coalesce(nodes$shock_exposure < low_exposure, FALSE)
  unl <- if ("trade_share_without_physical_data" %in% names(nodes)) nodes$trade_share_without_physical_data else rep(NA_real_, nrow(nodes))
  held <- ifelse(is.na(unl), "", sprintf(" (exporters with %.0f%% of world trade are absent from the physical data and held at their current shipments)", 100 * unl))
  nodes$physical_validation <- dplyr::case_when(
    applied & dv & low_exp ~ paste0("constrained by physical data", held, "; trade exposure understates physical concentration"),
    applied ~ paste0("constrained by physical data", held),
    pd == "incomplete" ~ "trade only; physical data incomplete",
    dv ~ "trade only; physical concentration diverges from trade",
    pd == "none" ~ "trade only; no physical data",
    TRUE ~ "trade only; physical data consistent with trade"
  )
  flag <- nodes$node_classification %in% c("LOW INTERVENTION", "COORDINATE") & (pd == "incomplete" | (dv & (!applied | low_exp)))
  nodes$classification_warning <- ifelse(flag, "PHYSICAL CAPABILITY NOT VALIDATED", NA_character_)
  nodes
}
