# Allied Industrial Base (AIB): sub-sector nodes of the trade network (docs/aib_methodology.md, 2).
#
# Hierarchy: Technology > Supply-chain stage > Sub-sector > HS6. The HS6 codes are the empirical
# trade data, the sub-sector is the unit the shocks and the optimizer work on, the stage frames
# policy and the technology summarizes. Nodes are the (tech, supply_chain, sub_sector) cells of the
# reference HS6 master (data/reference/energy_hs6_master.csv, in-basket rows only). A cell whose HS6
# set is identical in several technologies (permanent magnets in EV and Wind, transformers in Grid
# and Solar, pipelines in Oil and Gas) is one physical node: its trade is counted once and the node
# keeps the list of technologies it serves. Pure functions; scripts/31 does the IO.

aib_slug <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("&", "and", x, fixed = TRUE)
  x <- gsub("[^a-z0-9]+", "-", x)
  gsub("^-+|-+$", "", x)
}

aib_stage_levels <- function() c("Upstream", "Midstream", "Downstream")

# In-basket master rows, one per (tech, supply_chain, sub_sector, hs6), with the mapping-quality
# fields the data-quality flags read: essential (any), legacy_hs (any) and the first non-empty note.
# `splits` (config/aib.yml nodes$splits) refines a sub-sector into finer nodes by HS6 code; every
# code of a split sub-sector must be assigned to exactly one of its parts.
aib_master_cells <- function(master, splits = list()) {
  require_columns(master, c("tech", "supply_chain", "sub_sector", "hs6"), label = "HS6 master")
  m <- tibble::as_tibble(master)
  if ("excluded" %in% names(m)) {
    excl <- toupper(as.character(m$excluded)) %in% c("TRUE", "T", "1", "YES")
    m <- m[!excl, , drop = FALSE]
  }
  flag <- function(x) if (is.null(x)) rep(FALSE, nrow(m)) else toupper(as.character(x)) %in% c("TRUE", "T", "1", "YES")
  cells <- tibble::tibble(
    tech = stringr::str_squish(as.character(m$tech)),
    supply_chain = stringr::str_squish(as.character(m$supply_chain)),
    sub_sector = stringr::str_squish(as.character(m$sub_sector)),
    hs6 = hs6_clean_code(m$hs6),
    essential = flag(m$essential),
    legacy_hs = flag(m$legacy_hs),
    note = if ("note" %in% names(m)) dplyr::coalesce(as.character(m$note), "") else ""
  ) %>%
    dplyr::filter(!is.na(hs6), nzchar(hs6), !is.na(tech), !is.na(sub_sector)) %>%
    dplyr::group_by(tech, supply_chain, sub_sector, hs6) %>%
    dplyr::summarise(
      essential = any(essential),
      legacy_hs = any(legacy_hs),
      note = dplyr::first(c(note[nzchar(note)], "")),
      .groups = "drop"
    )

  # A split matches its technology and stage, and its sub-sector when one is given. Codes no part
  # lists go to `rest` (a split with only `rest` renames the cell); without `rest` they are an error.
  for (sp in splits) {
    hit <- cells$tech == sp$tech & cells$supply_chain == sp$supply_chain &
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

  dup <- cells %>% dplyr::count(tech, supply_chain, hs6) %>% dplyr::filter(n > 1)
  if (nrow(dup)) {
    stop("aib_master_cells(): ", nrow(dup), " HS6 codes sit in two sub-sectors of one technology stage (e.g. ",
         dup$tech[1], " ", dup$supply_chain[1], " ", dup$hs6[1], ").")
  }
  cells
}

# The electro-industrial HS6 dictionary (data/reference/electro_industrial_hs6_dictionary.csv) in the
# master's shape, so aib_master_cells() can build nodes from it: tech = EIS sector, supply_chain =
# stage, sub_sector = sector (refined by the taxonomy's splits). scope "core" keeps the core rows (the
# headline), "extended" every row; primary_only keeps each code's primary row, so every code sits in
# one node and sector totals do not double count (the dictionary's own aggregation rule). essential =
# specificity 1-2 (the core scope also keeps a few specificity-3 codes, justified in the dictionary). index_code / index_name carry the EIS index (6) above the sector (39).
aib_dictionary_master <- function(dict, scope = c("core", "extended"), primary_only = TRUE) {
  scope <- match.arg(scope)
  require_columns(dict, c("hs6", "index_code", "index_name", "sector", "stage", "specificity", "scope", "primary"),
                  label = "HS6 dictionary")
  d <- tibble::as_tibble(dict)
  flag <- function(x) toupper(as.character(x)) %in% c("TRUE", "T", "1", "YES")
  if (isTRUE(primary_only)) d <- d[flag(d$primary), , drop = FALSE]
  if (identical(scope, "core")) d <- d[d$scope == "core", , drop = FALSE]
  spec <- suppressWarnings(as.integer(d$specificity))
  tibble::tibble(
    tech = stringr::str_squish(as.character(d$sector)),
    supply_chain = stringr::str_squish(as.character(d$stage)),
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
# hs6, value; a code in several OSI technologies counts in each). The partner indices (Export
# Opportunity, Friendshore) and the policy compatibility are kept by OSI cell, so a node built on
# another taxonomy reads them through this. Returns node_id, score_techs (OSI technologies holding at
# least `min_share` of the node's matched trade, largest first, "; "-joined; NA when no code is in the
# basket: neutral scores), score_stage (the OSI stage with the most matched trade), osi_cells
# ("tech|stage" pairs, largest first), yf_cells and osi_subcomponents (the same cells and their OSI
# sub-sectors as Yards & Fences / Sector Profile ids, "<tech>-<stage>" and "<tech>-<stage>::<sub-sector>",
# largest first) and osi_coverage (share of the node's trade in basket codes).
aib_osi_crosswalk <- function(node_codes, osi_master, code_trade = NULL, min_share = 0.2) {
  om <- tibble::as_tibble(osi_master)
  if ("excluded" %in% names(om)) om <- om[!(toupper(as.character(om$excluded)) %in% c("TRUE", "T", "1", "YES")), , drop = FALSE]
  om_sub <- om %>% dplyr::transmute(hs6 = hs6_clean_code(hs6), tech = stringr::str_squish(tech), supply_chain = stringr::str_squish(supply_chain),
                                     sub_sector = stringr::str_squish(sub_sector)) %>% dplyr::distinct()
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
    dplyr::summarise(v = sum(value), .groups = "drop") %>% dplyr::arrange(node_id, dplyr::desc(v))
  by_cell <- m %>% dplyr::group_by(node_id, tech, supply_chain) %>% dplyr::summarise(v = sum(value), .groups = "drop") %>%
    dplyr::arrange(node_id, dplyr::desc(v))
  techs <- by_tech %>% dplyr::group_by(node_id) %>%
    dplyr::summarise(score_techs = paste(tech[share >= min_share | dplyr::row_number() == 1], collapse = "; "), .groups = "drop")
  stage <- by_stage %>% dplyr::group_by(node_id) %>% dplyr::summarise(score_stage = dplyr::first(supply_chain), .groups = "drop")
  cells <- by_cell %>% dplyr::group_by(node_id) %>%
    dplyr::summarise(osi_cells = paste(tech, supply_chain, sep = "|", collapse = "; "),
                     yf_cells = paste(unique(paste0(aib_slug(tech), "-", aib_slug(supply_chain))), collapse = "; "), .groups = "drop")
  subs <- nc %>% dplyr::inner_join(om_sub, by = "hs6", relationship = "many-to-many") %>%
    dplyr::group_by(node_id, tech, supply_chain, sub_sector) %>% dplyr::summarise(v = sum(value), .groups = "drop") %>%
    dplyr::arrange(node_id, dplyr::desc(v)) %>% dplyr::group_by(node_id) %>%
    dplyr::summarise(osi_subcomponents = paste(unique(paste0(aib_slug(tech), "-", aib_slug(supply_chain), "::", sub_sector)), collapse = "; "),
                     .groups = "drop")
  tot %>% dplyr::left_join(matched, by = "node_id") %>% dplyr::left_join(techs, by = "node_id") %>%
    dplyr::left_join(stage, by = "node_id") %>% dplyr::left_join(cells, by = "node_id") %>% dplyr::left_join(subs, by = "node_id") %>%
    dplyr::transmute(node_id, score_techs, score_stage, osi_cells, yf_cells, osi_subcomponents, osi_coverage = dplyr::coalesce(mv, 0) / total)
}

# Physical nodes from the master cells. Returns a list:
#   nodes       one row per node_id: sub_sector (label), supply_chain, techs ("A; B"), n_techs,
#               hs6 ("c1,c2"), n_hs6, primary_tech (first in alphabetical order)
#   node_cells  the hierarchy rows (tech, supply_chain, sub_sector) with their node_id
#   node_codes  node_id x hs6 with essential, legacy_hs, note and n_nodes_sharing (how many
#               physical nodes carry the code: > 1 means the code is also counted elsewhere)
aib_build_nodes <- function(cells) {
  require_columns(cells, c("tech", "supply_chain", "sub_sector", "hs6"), label = "master cells")
  sets <- cells %>%
    dplyr::group_by(tech, supply_chain, sub_sector) %>%
    dplyr::summarise(hs6_key = paste(sort(unique(hs6)), collapse = ","), .groups = "drop")

  phys <- sets %>%
    dplyr::group_by(hs6_key) %>%
    dplyr::summarise(
      sub_sector = paste(sort(unique(sub_sector)), collapse = " / "),
      stage_conflict = dplyr::n_distinct(supply_chain) > 1,
      supply_chain = names(sort(table(supply_chain), decreasing = TRUE))[1],
      techs = paste(sort(unique(tech)), collapse = "; "),
      n_techs = dplyr::n_distinct(tech),
      primary_tech = sort(unique(tech))[1],
      .groups = "drop"
    )
  # node_id: the sub-sector slug, made unique by stage and then primary technology where two
  # different HS6 sets share a sub-sector name ("Ores & Concentrates").
  id <- aib_slug(phys$sub_sector)
  dup <- id %in% id[duplicated(id)]
  id[dup] <- paste(id[dup], aib_slug(phys$supply_chain[dup]), sep = "--")
  dup <- id %in% id[duplicated(id)]
  id[dup] <- paste(id[dup], aib_slug(phys$primary_tech[dup]), sep = "--")
  if (anyDuplicated(id)) stop("aib_build_nodes(): node ids are not unique after disambiguation.")
  phys$node_id <- id
  phys$n_hs6 <- lengths(strsplit(phys$hs6_key, ",", fixed = TRUE))

  node_cells <- sets %>%
    dplyr::inner_join(dplyr::select(phys, hs6_key, node_id), by = "hs6_key") %>%
    dplyr::select(node_id, tech, supply_chain, sub_sector)

  first_cells <- cells %>%
    dplyr::inner_join(node_cells, by = c("tech", "supply_chain", "sub_sector")) %>%
    dplyr::group_by(node_id, hs6) %>%
    dplyr::summarise(
      essential = any(essential),
      legacy_hs = any(legacy_hs),
      note = dplyr::first(c(note[nzchar(note)], "")),
      .groups = "drop"
    )
  sharing <- first_cells %>% dplyr::count(hs6, name = "n_nodes_sharing")
  node_codes <- first_cells %>% dplyr::left_join(sharing, by = "hs6")

  nodes <- phys %>%
    dplyr::transmute(node_id, sub_sector, supply_chain, techs, n_techs, primary_tech,
                     hs6 = hs6_key, n_hs6, stage_conflict) %>%
    dplyr::arrange(primary_tech, match(supply_chain, aib_stage_levels()), sub_sector)
  list(nodes = nodes, node_cells = node_cells, node_codes = node_codes)
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

# The supply-chain coherence graph: nodes in the same stage of one technology are adjacent, and
# config/aib.yml material_links adds the links that cross stages or technologies (sub-sector
# names, matched to every node carrying that name). Returns an edge list node_a, node_b, basis.
aib_supply_chain_edges <- function(node_cells, material_links = list()) {
  same_stage <- node_cells %>%
    dplyr::distinct(node_id, tech, supply_chain) %>%
    dplyr::inner_join(
      node_cells %>% dplyr::distinct(node_b = node_id, tech, supply_chain),
      by = c("tech", "supply_chain"), relationship = "many-to-many"
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
