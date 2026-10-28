# Allied Industrial Base (AIB): sub-sector nodes of the trade network (docs/aib_methodology.md, 2).
#
# Hierarchy: Technology > Supply-chain stage > Sub-sector > HS6. The HS6 codes are the empirical
# trade data, the sub-sector is the unit the shocks and the optimizer work on, the stage frames
# policy and the technology summarizes. Nodes are the (tech, source_stage, sub_sector) cells of the
# taxonomy (the electro-industrial dictionary, or the reference HS6 master data/reference/
# energy_hs6_master.csv, in-basket rows only). A cell whose HS6 set is identical in several
# technologies (permanent magnets in EV and Wind, transformers in Grid and Solar, pipelines in Oil and
# Gas) is one physical node: its trade is counted once and the node keeps the list of technologies it
# serves. Two stage columns (since 2026-10-27): `source_stage` is the taxonomy's own Upstream /
# Midstream / Downstream (the dictionary's `stage`, the master's `supply_chain`) and defines the
# nodes (grouping, splits, node ids, the low-coverage aggregates, the same-stage edges), so the node
# set and its ids are those of the three-stage build; `supply_chain` is the six-stage label
# (R/utils/supply_chain_stages.R, which the caller sources) every analysis reads. Pure functions;
# scripts/31 does the IO.

aib_slug <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("&", "and", x, fixed = TRUE)
  x <- gsub("[^a-z0-9]+", "-", x)
  gsub("^-+|-+$", "", x)
}

aib_stage_levels <- function() SUPPLY_CHAIN_STAGES

# The stages of raw materials, mined or recovered: a member supplying one is a "Resource supplier"
# (scripts/32; in the dictionary these are exactly its Raw Material rows, its old Upstream).
aib_resource_stages <- function() c("Extraction", "Recovery")

# Distinct stage labels in chain order (unknown labels last, alphabetically).
aib_stage_sort <- function(x) {
  x <- unique(as.character(stats::na.omit(x)))
  x[order(match(x, aib_stage_levels(), nomatch = length(aib_stage_levels()) + 1L), x)]
}

# Recycled feed among the dictionary's raw materials: scrap, waste, spent batteries or fuel, residues
# and black mass, e-waste, used cooking oil (matched on the row's description words).
aib_recycled_feed_pattern <- function() "\\b(waste|scrap|spent|residues?|black mass|used cooking oil)\\b"

# The six-stage label of electro-industrial dictionary rows from their item type: Raw Material ->
# Extraction (Recovery when `text`, the row's HS description and segment, names recycled feed);
# Processed Material -> Refining; Component -> Components; Final Product and Process Equipment ->
# Assembly (capital goods stay in Assembly, a project decision). The dictionary's own stage is not
# read: its Downstream is final products and process equipment, not OSI deployment.
aib_dictionary_stage <- function(item_type, text = "") {
  it <- stringr::str_squish(as.character(item_type))
  map <- c(`Raw Material` = "Extraction", `Processed Material` = "Refining", Component = "Components",
           `Final Product` = "Assembly", `Process Equipment` = "Assembly")
  bad <- unique(it[!it %in% names(map)])
  if (length(bad)) stop("aib_dictionary_stage(): unknown item_type ", paste(bad, collapse = ", "), call. = FALSE)
  out <- unname(map[it])
  recycled <- grepl(aib_recycled_feed_pattern(), tolower(dplyr::coalesce(as.character(text), "")))
  out[it == "Raw Material" & recycled] <- "Recovery"
  out
}

# The six-stage label of groups of codes (a node, one technology's cell of a node): the stage holding the
# most world trade (code_trade: hs6, value; every code weighs at least 1 USD, so without trade the most
# codes win; a tie goes to the earlier stage). A code carrying several stages in one group (a shared
# node filed differently by its technologies) splits its trade among them. code_stages: the `keys`, hs6,
# supply_chain. Returns the keys, supply_chain, stage_mixed (the codes carry more than one stage) and
# stage_mix ("Refining 72%, Components 28%": shares of the group's trade, largest first, "<1%" below half a
# percent; NA when unmixed).
aib_stage_by_trade <- function(code_stages, keys = "node_id", code_trade = NULL) {
  require_columns(code_stages, c(keys, "hs6", "supply_chain"), label = "code stages")
  ct <- if (is.null(code_trade) || !nrow(code_trade)) tibble::tibble(hs6 = character(), value = numeric()) else
    code_trade %>% dplyr::transmute(hs6 = hs6_clean_code(hs6), value = as.numeric(value)) %>% dplyr::group_by(hs6) %>%
      dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
  cs <- tibble::as_tibble(code_stages) %>% dplyr::distinct(dplyr::across(dplyr::all_of(c(keys, "hs6", "supply_chain")))) %>%
    dplyr::left_join(ct, by = "hs6") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(keys, "hs6")))) %>%
    dplyr::mutate(value = pmax(dplyr::coalesce(value, 0), 1) / dplyr::n()) %>%
    dplyr::ungroup()
  cs %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(keys, "supply_chain")))) %>%
    dplyr::summarise(v = sum(value), .groups = "drop") %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(keys)), dplyr::desc(v), match(supply_chain, aib_stage_levels())) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(
      stage_mixed = dplyr::n() > 1,
      stage_mix = if (dplyr::n() > 1) paste(supply_chain, ifelse(v / sum(v) < 0.005, "<1%", sprintf("%.0f%%", 100 * v / sum(v))), collapse = ", ") else NA_character_,
      supply_chain = dplyr::first(supply_chain),
      .groups = "drop"
    ) %>%
    dplyr::select(dplyr::all_of(keys), supply_chain, stage_mixed, stage_mix)
}

# In-basket master rows, one per (tech, source_stage, sub_sector, hs6), with the code's six-stage label
# (`supply_chain`, from the master's `stage`) and the mapping-quality fields the data-quality flags read:
# essential (any), legacy_hs (any) and the first non-empty note. The master's own `supply_chain`
# (Upstream / Midstream / Downstream) becomes `source_stage`, the grouping of the nodes.
# `splits` (config/aib.yml nodes$splits) refines a sub-sector into finer nodes by HS6 code; every
# code of a split sub-sector must be assigned to exactly one of its parts.
aib_master_cells <- function(master, splits = list()) {
  require_columns(master, c("tech", "supply_chain", "stage", "sub_sector", "hs6"), label = "HS6 master")
  m <- tibble::as_tibble(master)
  if ("excluded" %in% names(m)) {
    excl <- toupper(as.character(m$excluded)) %in% c("TRUE", "T", "1", "YES")
    m <- m[!excl, , drop = FALSE]
  }
  flag <- function(x) if (is.null(x)) rep(FALSE, nrow(m)) else toupper(as.character(x)) %in% c("TRUE", "T", "1", "YES")
  cells <- tibble::tibble(
    tech = stringr::str_squish(as.character(m$tech)),
    source_stage = stringr::str_squish(as.character(m$supply_chain)),
    stage = stringr::str_squish(as.character(m$stage)),
    sub_sector = stringr::str_squish(as.character(m$sub_sector)),
    hs6 = hs6_clean_code(m$hs6),
    essential = flag(m$essential),
    legacy_hs = flag(m$legacy_hs),
    note = if ("note" %in% names(m)) dplyr::coalesce(as.character(m$note), "") else ""
  ) %>%
    dplyr::filter(!is.na(hs6), nzchar(hs6), !is.na(tech), !is.na(sub_sector)) %>%
    dplyr::group_by(tech, source_stage, sub_sector, hs6) %>%
    dplyr::summarise(
      supply_chain = dplyr::first(c(stage[!is.na(stage) & nzchar(stage)], NA_character_)),
      essential = any(essential),
      legacy_hs = any(legacy_hs),
      note = dplyr::first(c(note[nzchar(note)], "")),
      .groups = "drop"
    ) %>%
    dplyr::relocate(supply_chain, .after = source_stage)
  assert_supply_chain_stages(cells$supply_chain, "HS6 master stage")

  # A split matches its technology and source stage (the YAML key `supply_chain` holds the taxonomy's
  # Upstream / Midstream / Downstream), and its sub-sector when one is given. Codes no part lists go
  # to `rest` (a split with only `rest` renames the cell); without `rest` they are an error.
  for (sp in splits) {
    hit <- cells$tech == sp$tech & cells$source_stage == sp$supply_chain &
      (if (is.null(sp$sub_sector)) TRUE else cells$sub_sector == sp$sub_sector)
    label <- if (is.null(sp$sub_sector)) paste(sp$tech, sp$supply_chain, sep = " / ") else sp$sub_sector
    if (!any(hit)) {
      # `optional`: a split for a cell that only some scopes contain (the dictionary's extended rows).
      if (!isTRUE(sp$optional)) warning("aib_master_cells(): split target not in the master: ", sp$tech, " / ", sp$supply_chain, " / ", label, call. = FALSE)
      next
    }
    parts <- dplyr::bind_rows(lapply(sp$into, function(p) {
      tibble::tibble(part = as.character(p$sub_sector), hs6 = hs6_clean_code(unlist(p$hs6)))
    }))
    if (!nrow(parts)) parts <- tibble::tibble(part = character(), hs6 = character())
    if (anyDuplicated(parts$hs6)) stop("aib_master_cells(): a code is assigned to two parts of the split of ", label)
    codes <- cells$hs6[hit]
    missing <- setdiff(codes, parts$hs6)
    if (length(missing)) {
      if (is.null(sp$rest)) stop("aib_master_cells(): split of ", label, " leaves codes unassigned: ", paste(missing, collapse = ", "))
      parts <- dplyr::bind_rows(parts, tibble::tibble(part = as.character(sp$rest), hs6 = missing))
    }
    cells$sub_sector[hit] <- parts$part[match(codes, parts$hs6)]
  }

  dup <- cells %>% dplyr::count(tech, source_stage, hs6) %>% dplyr::filter(n > 1)
  if (nrow(dup)) {
    stop("aib_master_cells(): ", nrow(dup), " HS6 codes sit in two sub-sectors of one technology stage (e.g. ",
         dup$tech[1], " ", dup$source_stage[1], " ", dup$hs6[1], ").")
  }
  cells
}

# The electro-industrial HS6 dictionary (data/reference/electro_industrial_hs6_dictionary.csv) in the
# master's shape, so aib_master_cells() can build nodes from it: tech = EIS sector, supply_chain = the
# dictionary's stage (Upstream / Midstream / Downstream: the grouping), stage = the six-stage label
# (aib_dictionary_stage() from item_type, hs6_desc and segment), sub_sector = sector (refined by the
# taxonomy's splits). scope "core" keeps the core rows (the headline), "extended" every row;
# primary_only keeps each code's primary row, so every code sits in one node and sector totals do not
# double count (the dictionary's own aggregation rule). essential = specificity 1-2 (the core scope also
# keeps a few specificity-3 codes, justified in the dictionary). index_code / index_name carry the EIS
# index (6) above the sector (39).
aib_dictionary_master <- function(dict, scope = c("core", "extended"), primary_only = TRUE) {
  scope <- match.arg(scope)
  require_columns(dict, c("hs6", "index_code", "index_name", "sector", "stage", "item_type", "specificity", "scope", "primary"),
                  label = "HS6 dictionary")
  d <- tibble::as_tibble(dict)
  flag <- function(x) toupper(as.character(x)) %in% c("TRUE", "T", "1", "YES")
  if (isTRUE(primary_only)) d <- d[flag(d$primary), , drop = FALSE]
  if (identical(scope, "core")) d <- d[d$scope == "core", , drop = FALSE]
  spec <- suppressWarnings(as.integer(d$specificity))
  words <- paste(if ("hs6_desc" %in% names(d)) d$hs6_desc else "", if ("segment" %in% names(d)) d$segment else "")
  tibble::tibble(
    tech = stringr::str_squish(as.character(d$sector)),
    supply_chain = stringr::str_squish(as.character(d$stage)),
    stage = aib_dictionary_stage(d$item_type, words),
    sub_sector = stringr::str_squish(as.character(d$sector)),
    hs6 = hs6_clean_code(d$hs6),
    essential = !is.na(spec) & spec <= 2,
    legacy_hs = FALSE,
    note = if ("notes" %in% names(d)) dplyr::coalesce(as.character(d$notes), "") else "",
    index_code = as.character(d$index_code),
    index_name = as.character(d$index_name),
    segment = if ("segment" %in% names(d)) as.character(d$segment) else NA_character_,
    scope = as.character(d$scope)
  )
}

# Each node's place in the OSI basket (data/reference/energy_hs6_master.csv, excluded rows dropped):
# the OSI technology x stage cells its codes sit in, weighted by the codes' world trade (code_trade:
# hs6, value; a code in several OSI technologies counts in each). The stage is the master's six-stage
# `stage` (not its old `supply_chain`). The partner indices (Export Opportunity, Friendshore), the
# Development Potential Index and the policy compatibility are kept by OSI technology x six-stage
# cell, so a node built on another taxonomy reads them through this. Returns node_id, score_techs (OSI
# technologies holding at least `min_share` of the node's matched trade, largest first, "; "-joined; NA
# when no code is in the basket: neutral scores), score_stage (the OSI stage with the most matched
# trade), score_cells (each score technology at its own stage, "tech|stage; ...", in score_techs order:
# stage is relative to the technology, so HS 850760 is Batteries Assembly but Electric Vehicles
# Components, and a node of li-ion cells is scored in both of those cells, not in Electric Vehicles
# Assembly), osi_cells ("tech|stage" pairs, largest first), yf_cells and osi_subcomponents (the same
# cells and their OSI sub-sectors as Yards & Fences / Sector Profile ids, "<tech>-<stage>" and
# "<tech>-<stage>::<sub-sector>", e.g. "batteries-components", largest first) and osi_coverage (share of
# the node's trade in basket codes). Ties go to the earlier stage in chain order.
# `stage_col` picks the master's stage column: "stage" (the default: the six stages, asserted) or
# "supply_chain", the master's three-stage Upstream / Midstream / Downstream (asserted). The second builds
# the legacy crosswalk (scripts/84 since 2026-10-28: node.yfCellsLegacy / yfSubcomponentsLegacy, e.g.
# "batteries-midstream"), the cells of the three-stage Country, Sector and Strategic bundles; every output
# column then holds three-stage labels, ties going to the earlier of Upstream, Midstream, Downstream.
aib_osi_crosswalk <- function(node_codes, osi_master, code_trade = NULL, min_share = 0.2, stage_col = c("stage", "supply_chain")) {
  stage_col <- match.arg(stage_col)
  require_columns(osi_master, c("tech", stage_col, "sub_sector", "hs6"), label = "HS6 master")
  levels <- if (identical(stage_col, "stage")) SUPPLY_CHAIN_STAGES else SUPPLY_CHAIN_LEGACY_STAGES
  om <- tibble::as_tibble(osi_master)
  if ("excluded" %in% names(om)) om <- om[!(toupper(as.character(om$excluded)) %in% c("TRUE", "T", "1", "YES")), , drop = FALSE]
  om_sub <- om %>% dplyr::transmute(hs6 = hs6_clean_code(hs6), tech = stringr::str_squish(tech), supply_chain = stringr::str_squish(.data[[stage_col]]),
                                     sub_sector = stringr::str_squish(sub_sector)) %>% dplyr::distinct()
  if (identical(stage_col, "stage")) {
    assert_supply_chain_stages(om_sub$supply_chain, "HS6 master stage")
  } else {
    bad <- unique(om_sub$supply_chain[!om_sub$supply_chain %in% SUPPLY_CHAIN_LEGACY_STAGES])
    if (length(bad)) {
      stop("HS6 master supply_chain holds values that are not Upstream / Midstream / Downstream: ", paste(utils::head(bad, 10), collapse = ", "), ".")
    }
  }
  om <- om_sub %>% dplyr::distinct(hs6, tech, supply_chain)
  ct <- if (is.null(code_trade) || !nrow(code_trade)) tibble::tibble(hs6 = character(), value = numeric()) else
    code_trade %>% dplyr::transmute(hs6 = hs6_clean_code(hs6), value = as.numeric(value)) %>% dplyr::group_by(hs6) %>%
      dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
  # Every code weighs at least 1 USD, so a node without trade falls back to counting codes.
  nc <- node_codes %>% dplyr::distinct(node_id, hs6) %>% dplyr::left_join(ct, by = "hs6") %>%
    dplyr::mutate(value = pmax(dplyr::coalesce(value, 0), 1))
  tot <- nc %>% dplyr::group_by(node_id) %>% dplyr::summarise(total = sum(value), .groups = "drop")
  m <- nc %>% dplyr::inner_join(om, by = "hs6", relationship = "many-to-many")
  matched <- m %>% dplyr::distinct(node_id, hs6, value) %>% dplyr::group_by(node_id) %>% dplyr::summarise(mv = sum(value), .groups = "drop")
  by_tech <- m %>% dplyr::distinct(node_id, hs6, tech, value) %>% dplyr::group_by(node_id, tech) %>%
    dplyr::summarise(v = sum(value), .groups = "drop") %>% dplyr::left_join(matched, by = "node_id") %>%
    dplyr::mutate(share = v / mv) %>% dplyr::arrange(node_id, dplyr::desc(v))
  by_stage <- m %>% dplyr::distinct(node_id, hs6, supply_chain, value) %>% dplyr::group_by(node_id, supply_chain) %>%
    dplyr::summarise(v = sum(value), .groups = "drop") %>%
    dplyr::arrange(node_id, dplyr::desc(v), match(supply_chain, levels))
  by_cell <- m %>% dplyr::group_by(node_id, tech, supply_chain) %>% dplyr::summarise(v = sum(value), .groups = "drop") %>%
    dplyr::arrange(node_id, dplyr::desc(v), match(supply_chain, levels))
  kept <- by_tech %>% dplyr::group_by(node_id) %>% dplyr::filter(share >= min_share | dplyr::row_number() == 1) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>% dplyr::ungroup()
  techs <- kept %>% dplyr::group_by(node_id) %>% dplyr::summarise(score_techs = paste(tech, collapse = "; "), .groups = "drop")
  stage <- by_stage %>% dplyr::group_by(node_id) %>% dplyr::summarise(score_stage = dplyr::first(supply_chain), .groups = "drop")
  tech_stage <- by_cell %>% dplyr::group_by(node_id, tech) %>% dplyr::slice(1) %>% dplyr::ungroup() %>%
    dplyr::inner_join(dplyr::select(kept, node_id, tech, rank), by = c("node_id", "tech")) %>%
    dplyr::arrange(node_id, rank) %>% dplyr::group_by(node_id) %>%
    dplyr::summarise(score_cells = paste(tech, supply_chain, sep = "|", collapse = "; "), .groups = "drop")
  cells <- by_cell %>% dplyr::group_by(node_id) %>%
    dplyr::summarise(osi_cells = paste(tech, supply_chain, sep = "|", collapse = "; "),
                     yf_cells = paste(unique(paste0(aib_slug(tech), "-", aib_slug(supply_chain))), collapse = "; "), .groups = "drop")
  subs <- nc %>% dplyr::inner_join(om_sub, by = "hs6", relationship = "many-to-many") %>%
    dplyr::group_by(node_id, tech, supply_chain, sub_sector) %>% dplyr::summarise(v = sum(value), .groups = "drop") %>%
    dplyr::arrange(node_id, dplyr::desc(v)) %>% dplyr::group_by(node_id) %>%
    dplyr::summarise(osi_subcomponents = paste(unique(paste0(aib_slug(tech), "-", aib_slug(supply_chain), "::", sub_sector)), collapse = "; "),
                     .groups = "drop")
  tot %>% dplyr::left_join(matched, by = "node_id") %>% dplyr::left_join(techs, by = "node_id") %>%
    dplyr::left_join(stage, by = "node_id") %>% dplyr::left_join(tech_stage, by = "node_id") %>%
    dplyr::left_join(cells, by = "node_id") %>% dplyr::left_join(subs, by = "node_id") %>%
    dplyr::transmute(node_id, score_techs, score_stage, score_cells, osi_cells, yf_cells, osi_subcomponents,
                     osi_coverage = dplyr::coalesce(mv, 0) / total)
}

# Physical nodes from the master cells (aib_master_cells()). The grouping is the source stage, so the
# node set and ids do not depend on the six-stage labels; code_trade (hs6, value: world imports in the
# reference year) only weighs the six-stage label of each node and cell (aib_stage_by_trade()).
# Returns a list:
#   nodes        one row per node_id: sub_sector (label), supply_chain (six-stage), source_stage, techs
#                ("A; B"), n_techs, hs6 ("c1,c2"), n_hs6, primary_tech (first in alphabetical order),
#                stage_conflict (technologies file the same codes in different source stages),
#                stage_mixed and stage_mix (its codes carry several six-stage labels)
#   node_cells   the hierarchy rows (tech, supply_chain, source_stage, sub_sector) with their node_id;
#                supply_chain is the cell's own six-stage label (the stage is relative to the technology)
#   node_codes   node_id x hs6 with essential, legacy_hs, note and n_nodes_sharing (how many
#                physical nodes carry the code: > 1 means the code is also counted elsewhere)
#   code_stages  node_id x tech x hs6 with the code's six-stage label in that technology (supply_chain)
aib_build_nodes <- function(cells, code_trade = NULL) {
  require_columns(cells, c("tech", "source_stage", "supply_chain", "sub_sector", "hs6"), label = "master cells")
  sets <- cells %>%
    dplyr::group_by(tech, source_stage, sub_sector) %>%
    dplyr::summarise(hs6_key = paste(sort(unique(hs6)), collapse = ","), .groups = "drop")

  phys <- sets %>%
    dplyr::group_by(hs6_key) %>%
    dplyr::summarise(
      sub_sector = paste(sort(unique(sub_sector)), collapse = " / "),
      stage_conflict = dplyr::n_distinct(source_stage) > 1,
      source_stage = names(sort(table(source_stage), decreasing = TRUE))[1],
      techs = paste(sort(unique(tech)), collapse = "; "),
      n_techs = dplyr::n_distinct(tech),
      primary_tech = sort(unique(tech))[1],
      .groups = "drop"
    )
  # node_id: the sub-sector slug, made unique by source stage and then primary technology where two
  # different HS6 sets share a sub-sector name ("Ores & Concentrates").
  id <- aib_slug(phys$sub_sector)
  dup <- id %in% id[duplicated(id)]
  id[dup] <- paste(id[dup], aib_slug(phys$source_stage[dup]), sep = "--")
  dup <- id %in% id[duplicated(id)]
  id[dup] <- paste(id[dup], aib_slug(phys$primary_tech[dup]), sep = "--")
  if (anyDuplicated(id)) stop("aib_build_nodes(): node ids are not unique after disambiguation.")
  phys$node_id <- id
  phys$n_hs6 <- lengths(strsplit(phys$hs6_key, ",", fixed = TRUE))

  node_cells <- sets %>%
    dplyr::inner_join(dplyr::select(phys, hs6_key, node_id), by = "hs6_key") %>%
    dplyr::select(node_id, tech, source_stage, sub_sector)
  cell_codes <- cells %>% dplyr::inner_join(node_cells, by = c("tech", "source_stage", "sub_sector"))
  code_stages <- cell_codes %>% dplyr::distinct(node_id, tech, hs6, supply_chain)
  node_cells <- node_cells %>%
    dplyr::left_join(dplyr::select(aib_stage_by_trade(code_stages, c("node_id", "tech"), code_trade), node_id, tech, supply_chain),
                     by = c("node_id", "tech")) %>%
    dplyr::select(node_id, tech, supply_chain, source_stage, sub_sector)

  first_cells <- cell_codes %>%
    dplyr::group_by(node_id, hs6) %>%
    dplyr::summarise(
      essential = any(essential),
      legacy_hs = any(legacy_hs),
      note = dplyr::first(c(note[nzchar(note)], "")),
      .groups = "drop"
    )
  sharing <- first_cells %>% dplyr::count(hs6, name = "n_nodes_sharing")
  node_codes <- first_cells %>% dplyr::left_join(sharing, by = "hs6")

  # Rows in the three-stage build's order (primary technology, source stage, sub-sector).
  nodes <- phys %>%
    dplyr::left_join(aib_stage_by_trade(code_stages, "node_id", code_trade), by = "node_id") %>%
    dplyr::transmute(node_id, sub_sector, supply_chain, source_stage, techs, n_techs, primary_tech,
                     hs6 = hs6_key, n_hs6, stage_conflict, stage_mixed, stage_mix) %>%
    dplyr::arrange(primary_tech, match(source_stage, SUPPLY_CHAIN_LEGACY_STAGES), sub_sector)
  list(nodes = nodes, node_cells = node_cells, node_codes = node_codes, code_stages = code_stages)
}

# The (tech = node_id, supply_chain = "node", hs6) crosswalk that lets the partner themes' flow
# builder (partnership_strength_build_trade_cells(), importer's declaration first, else the
# exporter's at the CIF/FOB factor) reconcile flows at node grain without a second implementation.
aib_node_crosswalk <- function(node_codes, node_ids = NULL) {
  out <- node_codes
  if (!is.null(node_ids)) out <- out[out$node_id %in% node_ids, , drop = FALSE]
  tibble::tibble(tech = out$node_id, supply_chain = "node", sub_sector = "All", hs6 = out$hs6)
}

# HS6 codes created in the 2022 revision. A node made of them has no comparable earlier series
# (the 2020 dyads are HS2017 or older: PV cells and modules then traded together under 8541.40).
aib_hs2022_codes <- function() {
  c("854142", "854143", "850171", "850172", "850180", "870124", "870460",
    "854911", "854912", "854913", "854914", "854919", "854921", "854929", "854931", "854939", "854991", "854999")
}

# Node ids carrying a sub-sector name from a config link. "Tech::Sub-sector" restricts the name to
# one technology (several share names such as "Ores & Concentrates").
aib_link_nodes <- function(cells, name) {
  parts <- strsplit(name, "::", fixed = TRUE)[[1]]
  hit <- if (length(parts) == 2) cells$tech == parts[1] & cells$sub_sector == parts[2] else cells$sub_sector == name
  unique(cells$node_id[hit])
}

# The supply-chain coherence graph: nodes in the same source stage of one technology are adjacent
# (the taxonomy's Upstream / Midstream / Downstream, so the edges are those of the three-stage build),
# and config/aib.yml material_links adds the links that cross stages or technologies (sub-sector
# names, matched to every node carrying that name). Returns an edge list node_a, node_b, basis.
aib_supply_chain_edges <- function(node_cells, material_links = list()) {
  require_columns(node_cells, c("node_id", "tech", "source_stage", "sub_sector"), label = "node cells")
  same_stage <- node_cells %>%
    dplyr::distinct(node_id, tech, source_stage) %>%
    dplyr::inner_join(
      node_cells %>% dplyr::distinct(node_b = node_id, tech, source_stage),
      by = c("tech", "source_stage"), relationship = "many-to-many"
    ) %>%
    dplyr::filter(node_id < node_b) %>%
    dplyr::distinct(node_a = node_id, node_b) %>%
    dplyr::mutate(basis = "same technology stage")
  mat <- dplyr::bind_rows(lapply(material_links, function(l) {
    l <- unlist(l)
    a <- aib_link_nodes(node_cells, l[1])
    b <- aib_link_nodes(node_cells, l[2])
    if (!length(a) || !length(b)) return(NULL)
    tidyr::expand_grid(node_a = a, node_b = b)
  }))
  if (nrow(mat)) {
    mat <- mat %>%
      dplyr::filter(node_a != node_b) %>%
      dplyr::transmute(x = pmin(node_a, node_b), y = pmax(node_a, node_b)) %>%
      dplyr::distinct(node_a = x, node_b = y) %>%
      dplyr::mutate(basis = "material link")
  }
  dplyr::bind_rows(same_stage, mat) %>%
    dplyr::group_by(node_a, node_b) %>%
    dplyr::summarise(basis = paste(sort(unique(basis)), collapse = "; "), .groups = "drop")
}

# Coherent pairs: within `max_hops` steps on the supply-chain graph (2 by default, so a magnet
# node joins both its rare-earth feedstock and the motors that use it). Returns a symmetric
# logical matrix over `node_ids`.
aib_coherence_matrix <- function(edges, node_ids, max_hops = 2) {
  n <- length(node_ids)
  adj <- matrix(FALSE, n, n, dimnames = list(node_ids, node_ids))
  e <- edges[edges$node_a %in% node_ids & edges$node_b %in% node_ids, , drop = FALSE]
  if (nrow(e)) {
    adj[cbind(match(e$node_a, node_ids), match(e$node_b, node_ids))] <- TRUE
    adj[cbind(match(e$node_b, node_ids), match(e$node_a, node_ids))] <- TRUE
  }
  reach <- adj
  step <- adj
  if (max_hops >= 2) {
    for (h in 2:max_hops) {
      step <- (step %*% adj) > 0
      reach <- reach | step
    }
  }
  diag(reach) <- TRUE
  reach
}

# Supply-chain edges after the data-quality merge: node ids mapped to the nodes that absorbed them,
# self-edges dropped, bases combined.
aib_remap_edges <- function(edges, merge_map = NULL) {
  if (is.null(merge_map) || !nrow(merge_map) || !nrow(edges)) return(edges)
  re <- function(id) { k <- match(id, merge_map$node_id); ifelse(is.na(k), id, merge_map$merged_id[k]) }
  a <- re(edges$node_a)
  b <- re(edges$node_b)
  tibble::tibble(node_a = pmin(a, b), node_b = pmax(a, b), basis = edges$basis) %>%
    dplyr::filter(node_a != node_b) %>%
    dplyr::group_by(node_a, node_b) %>%
    dplyr::summarise(basis = paste(sort(unique(unlist(strsplit(basis, "; ")))), collapse = "; "), .groups = "drop")
}
