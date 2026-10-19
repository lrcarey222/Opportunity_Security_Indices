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

  for (sp in splits) {
    hit <- cells$tech == sp$tech & cells$supply_chain == sp$supply_chain & cells$sub_sector == sp$sub_sector
    if (!any(hit)) {
      warning("aib_master_cells(): split target not in the master: ", sp$tech, " / ", sp$supply_chain, " / ", sp$sub_sector, call. = FALSE)
      next
    }
    parts <- dplyr::bind_rows(lapply(sp$into, function(p) {
      tibble::tibble(part = as.character(p$sub_sector), hs6 = hs6_clean_code(unlist(p$hs6)))
    }))
    if (anyDuplicated(parts$hs6)) stop("aib_master_cells(): a code is assigned to two parts of the split of ", sp$sub_sector)
    codes <- cells$hs6[hit]
    missing <- setdiff(codes, parts$hs6)
    if (length(missing)) {
      stop("aib_master_cells(): split of ", sp$sub_sector, " leaves codes unassigned: ", paste(missing, collapse = ", "))
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
