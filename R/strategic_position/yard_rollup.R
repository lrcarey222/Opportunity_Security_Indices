# YARDS: capability layers as first-class yard units, rolled up to technology x six-stage for display
# (docs/strategic_position.md, "Layer-first yards and the roll-up"; user spec 2026-10-30, Yards & Fences item 1).
#
# Each unit takes its own choice (scripts/26: a cell from its own inputs; a sensitive layer from its own
# Secure tests, strategic industrial value and large-yard case, with its parent cell's need, Competitive
# Position and pathway), and the choice maps to a yard as the Yards & Fences paper's three yards ("Three Yards
# for Three Different Problems"):
#   small  (high fence)    Secure
#   medium (medium fence)  Preserve, Catch Up, Partner; Accelerate at Deployment & Use
#   large  (low fence)     Watch
# A cell's units are its stage row (the rest of the stage) and its layers. Where they agree the cell has that
# yard; where they do not it is mixed, named by the fences it spans: mixed_high_medium (small + medium),
# mixed_medium_low (medium + large), mixed_high_low (small + large) or mixed_high_medium_low (all three).
# Units not assessed carry no yard and do not enter the roll-up.
#
# The paper's own placements (yards_fences_assessments.csv paper_placement) are read only by
# sp_yf_reproduction(), the validation diagnostic, never by the classification.
#
# Pure functions: no IO. Needs strategic_value.R and decision_engine.R (sp_layer_choice()).

SP_YARDS <- c("small", "medium", "large")
SP_YARD_ROLLUPS <- c("small", "medium", "large", "mixed_high_medium", "mixed_medium_low", "mixed_high_low", "mixed_high_medium_low")
SP_YARD_FENCE <- c(small = "high", medium = "medium", large = "low")

# The yard of a choice verb at a stage (NA where not assessed).
sp_yard_of_choice <- function(verb, stage) {
  dplyr::case_when(
    verb %in% "secure" ~ "small",
    verb %in% c("preserve", "catch_up", "partner") ~ "medium",
    verb %in% "accelerate" & stage %in% "Deployment" ~ "medium",
    verb %in% "watch" ~ "large",
    TRUE ~ NA_character_
  )
}

# One roll-up value from the yards of a cell's units (NA ignored; all NA -> NA).
sp_yard_combine <- function(yards) {
  y <- SP_YARDS[SP_YARDS %in% yards]
  if (!length(y)) return(NA_character_)
  if (length(y) == 1) return(y)
  unname(c("small+medium" = "mixed_high_medium", "medium+large" = "mixed_medium_low", "small+large" = "mixed_high_low",
           "small+medium+large" = "mixed_high_medium_low")[paste(y, collapse = "+")])
}

# The compact JSON array of a cell's children [{layer_id, label, yard, choice, reason}] (missing keys left out;
# a single child stays a one-item array).
sp_yard_children_json <- function(layers) {
  if (!nrow(layers)) return(NA_character_)
  items <- lapply(seq_len(nrow(layers)), function(i) {
    it <- list(layer_id = layers$layer_id[i], label = layers$layer_label[i], yard = layers$yard[i],
               choice = layers$strategic_choice[i], reason = layers$choice_reason[i])
    Filter(function(v) !is.null(v) && length(v) == 1 && !is.na(v), it)
  })
  as.character(jsonlite::toJSON(I(items), auto_unbox = TRUE))
}

# Roll the layer yards up to their cells. cells: one row per (iso3, tech, supply_chain) with strategic_choice;
# layers: one row per (iso3, tech, supply_chain, layer_id) with strategic_choice and, optionally, layer_label
# and choice_reason. Adds to cells: yard (the stage row's own), yard_rollup, yard_children (JSON array, NA where
# the cell has no layers), yard_units (the stage row plus its layers) and yard_mixed; to layers: yard and the
# parent's yard_rollup. Returns list(cells, layers).
sp_yard_rollup <- function(cells, layers, keys = c("iso3", "tech", "supply_chain")) {
  cells$yard <- sp_yard_of_choice(cells$strategic_choice, cells$supply_chain)
  if (is.null(layers)) layers <- cells[0, intersect(c(keys, "strategic_choice"), names(cells)), drop = FALSE]
  layers <- as.data.frame(layers, stringsAsFactors = FALSE)
  if (!"layer_id" %in% names(layers)) layers$layer_id <- character(nrow(layers))
  for (cl in c("layer_label", "choice_reason")) if (!cl %in% names(layers)) layers[[cl]] <- rep(NA_character_, nrow(layers))
  layers$yard <- sp_yard_of_choice(layers$strategic_choice, layers$supply_chain)
  ck <- do.call(paste, c(cells[keys], sep = "|"))
  lk <- do.call(paste, c(layers[keys], sep = "|"))
  by_cell <- split(seq_len(nrow(layers)), factor(lk, levels = unique(c(ck, lk))))
  n <- nrow(cells)
  rollup <- character(n)
  children <- rep(NA_character_, n)
  units <- integer(n)
  mixed <- logical(n)
  for (i in seq_len(n)) {
    j <- by_cell[[ck[i]]]
    if (is.null(j)) j <- integer()
    y <- c(cells$yard[i], layers$yard[j])
    rollup[i] <- sp_yard_combine(y)
    units[i] <- 1L + length(j)
    mixed[i] <- sum(SP_YARDS %in% y) > 1
    if (length(j)) children[i] <- sp_yard_children_json(layers[j, , drop = FALSE])
  }
  cells$yard_rollup <- rollup
  cells$yard_children <- children
  cells$yard_units <- units
  cells$yard_mixed <- mixed
  layers$yard_rollup <- rollup[match(lk, ck)]
  list(cells = cells, layers = layers)
}

# ---- Layers as first-class units ----------------------------------------------------------------------------
# What a layer takes from its cell: the need, the market prize behind its strategic industrial value, the
# Competitive Position (with the capability floor) and the pathway (layers have no trade data of their own),
# the evidence quality, and the world supply structure the large-yard case reads.
SP_LAYER_PARENT_COLS <- c(
  "need_type", "supply_chain_need", "supply_chain_need_n_families", "deployment_need", "dn_families_observed",
  "industrial_opportunity", "io_node_prize", "io_home_market", "io_home_market_basis", "io_basis", "io_outlook", "io_growth",
  "io_growth_basis", "io_investment_momentum", "io_investment_momentum_pct", "io_node_prize_capped",
  "cp_contender_pct", "cp_contender_basis", "cp_n_contenders", "cp_is_contender", "t3_capability_lo",
  "pathway_credible", "pathway_basis", "pathway_rule", "pathway_support", "catch_up_anchor", "catch_up_anchor_type",
  "competitive_position_percentile", "strategic_need_percentile", "stage_evidence_quality_cp", "world_top_iso3", "world_top_share"
)

# The layer table with its cell's columns (SP_LAYER_PARENT_COLS present on cells replace the layer's own) and
# its own curated maturity as the option_value context.
sp_layer_inputs <- function(layers, cells, cols = SP_LAYER_PARENT_COLS, keys = c("iso3", "tech", "supply_chain")) {
  cols <- intersect(cols, names(cells))
  x <- as.data.frame(layers, stringsAsFactors = FALSE)
  x <- x[, setdiff(names(x), cols), drop = FALSE]
  x <- dplyr::left_join(x, as.data.frame(cells)[, c(keys, cols), drop = FALSE], by = keys)
  if ("maturity" %in% names(x)) x$io_maturity_class <- x$maturity
  x
}

# Each layer's OWN choice (user spec 2026-10-30, Yards & Fences item 1): Secure where the engine's own layer tests
# found it (strategic_posture secure, kept exactly); otherwise its own strategic industrial value (its own commons
# and option attributes with its cell's market prize, placed among the cells' scores) and its own large-yard case
# (its own attributes with its cell's world supply), with its cell's need, Competitive Position and pathway,
# through the same rule as the cells (decision_engine.R). layers: the engine's layer rows; cells: the decided cell
# table (after sp_strategic_value()); attrs: the resolved node attributes; cfg: decision_framework; post_cfg:
# strategic_posture. Needs strategic_value.R and decision_engine.R.
sp_layer_choice <- function(layers, cells, attrs, cfg = list(), post_cfg = list()) {
  x <- sp_layer_inputs(layers, cells)
  ref <- if ("siv_score" %in% names(cells)) cells$siv_score[cells$supply_chain %in% SP_IO_PRODUCING_STAGES] else NULL
  x <- sp_strategic_value(x, attrs, cfg, ref = ref)
  x <- sp_large_yard_case(x, attrs, cfg)
  x <- sp_decision_choice(x, cfg, post_cfg)
  x$choice_reason <- sp_choice_reason(x, cfg)
  x
}

# Counts of each roll-up by technology x stage (diagnostics yard_rollup_by_cell.csv).
sp_yard_rollup_counts <- function(cells) {
  x <- cells[!is.na(cells$strategic_choice), , drop = FALSE]
  r <- factor(dplyr::coalesce(x$yard_rollup, "not_assessed"), c(SP_YARD_ROLLUPS, "not_assessed"))
  g <- paste(x$tech, x$supply_chain, sep = "|")
  tab <- as.data.frame.matrix(table(g, r))
  out <- data.frame(tech = sub("\\|.*$", "", rownames(tab)), supply_chain = sub("^.*\\|", "", rownames(tab)), tab,
                    n_cells = rowSums(tab), n_layers_per_cell = as.integer(tapply(x$yard_units, g, max)[rownames(tab)]) - 1L,
                    check.names = FALSE, stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out[order(out$tech, match(out$supply_chain, SUPPLY_CHAIN_STAGES)), , drop = FALSE]
}

# Counts of each layer's choices and yards over countries (diagnostics yard_layers.csv).
sp_yard_layer_counts <- function(layers) {
  x <- layers[!is.na(layers$strategic_choice), , drop = FALSE]
  if (!nrow(x)) return(data.frame())
  key <- paste(x$tech, x$supply_chain, x$layer_id, sep = "|")
  out <- do.call(rbind, lapply(split(seq_len(nrow(x)), key), function(i) {
    ch <- table(factor(x$strategic_choice[i], c("secure", "preserve", "catch_up", "partner", "accelerate", "watch", "not_assessed")))
    yd <- table(factor(dplyr::coalesce(x$yard[i], "none"), c(SP_YARDS, "none")))
    data.frame(tech = x$tech[i[1]], supply_chain = x$supply_chain[i[1]], layer_id = x$layer_id[i[1]],
               layer_type = if ("layer_type" %in% names(x)) x$layer_type[i[1]] else NA_character_, n_countries = length(i),
               t(as.matrix(ch)), t(stats::setNames(as.vector(yd), paste0("yard_", names(yd)))),
               large_yard_case_true = if ("large_yard_case" %in% names(x)) sum(x$large_yard_case[i] %in% TRUE) else NA_integer_,
               check.names = FALSE, stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  out[order(out$tech, match(out$supply_chain, SUPPLY_CHAIN_STAGES), out$layer_id), , drop = FALSE]
}

# ---- Validation against the paper (diagnostic only) ---------------------------------------------------------
# For each curated Yards & Fences component, the unit the tool classifies (the layer linked to it by
# yf_component_id; with none, the stage rows of its technology at the six stages its three-stage label covers:
# Upstream = Extraction / Refining, Midstream = Components / Assembly, Downstream = Deployment), the derived
# yard for the review countries and the modal yard over all countries, the reasons, and the paper's own
# placement beside it with agree / differ. This is the only function that reads paper_placement.
# yf: yards_fences_assessments.csv (component_id, technology, stage, component, layer, paper_placement);
# units: cells and layers bound together (iso3, tech, supply_chain, layer_id, yf_component_id, yard,
# strategic_choice, choice_reason, large_yard_case, large_yard_reason, strategic_industrial_value).
SP_YF_STAGE_MAP <- list(Upstream = c("Extraction", "Refining"), Midstream = c("Components", "Assembly"), Downstream = "Deployment")

sp_yf_reproduction <- function(yf, units, countries = c("USA", "JPN", "DEU", "IND", "AUS")) {
  u <- as.data.frame(units, stringsAsFactors = FALSE)
  col <- function(d, nm) if (nm %in% names(d)) d[[nm]] else rep(NA, nrow(d))
  u$yf_component_id <- col(u, "yf_component_id")
  modal <- function(y) {
    y <- y[!is.na(y)]
    if (!length(y)) return(c(NA_character_, NA_character_))
    t <- table(factor(y, SP_YARDS))
    top <- names(t)[t == max(t)]
    c(paste(top, collapse = "|"), format(round(max(t) / length(y), 2), nsmall = 2))
  }
  rows <- list()
  for (k in seq_len(nrow(yf))) {
    id <- yf$component_id[k]
    hit <- u[u$yf_component_id %in% id, , drop = FALSE]
    kind <- "linked_layer"
    if (!nrow(hit)) {
      st <- SP_YF_STAGE_MAP[[as.character(yf$stage[k])]] %||% character()
      hit <- u[u$tech %in% yf$technology[k] & u$supply_chain %in% st & u$layer_id %in% "stage", , drop = FALSE]
      kind <- "stage_row"
    }
    if (!nrow(hit)) {
      rows[[length(rows) + 1]] <- data.frame(component_id = id, component = yf$component[k], unit_kind = "none",
                                             tech = NA_character_, supply_chain = NA_character_, layer_id = NA_character_,
                                             paper_placement = yf$paper_placement[k], agree = "no_unit", stringsAsFactors = FALSE)
      next
    }
    for (key in unique(paste(hit$tech, hit$supply_chain, hit$layer_id, sep = "|"))) {
      h <- hit[paste(hit$tech, hit$supply_chain, hit$layer_id, sep = "|") == key, , drop = FALSE]
      m <- modal(h$yard)
      per <- stats::setNames(lapply(countries, function(c3) { v <- h$yard[h$iso3 == c3]; if (length(v)) v[1] else NA_character_ }),
                             paste0("yard_", countries))
      ref <- h[h$iso3 %in% countries[1], , drop = FALSE]
      if (!nrow(ref)) ref <- h[1, , drop = FALSE]
      paper <- trimws(as.character(yf$paper_placement[k]))
      agree <- function(y) ifelse(is.na(paper) | !nzchar(paper), "no_paper_placement", ifelse(is.na(y), "not_assessed",
                                  ifelse(grepl(paste0("(^|\\|)", paper, "($|\\|)"), y), ifelse(grepl("|", y, fixed = TRUE), "tie_includes_paper", "agree"), "differ")))
      rows[[length(rows) + 1]] <- data.frame(
        component_id = id, component = yf$component[k], unit_kind = kind, tech = h$tech[1], supply_chain = h$supply_chain[1],
        layer_id = h$layer_id[1], per, modal_yard = m[1], modal_share = as.numeric(m[2]), n_countries = sum(!is.na(h$yard)),
        large_yard_case = paste0(sum(col(h, "large_yard_case") %in% TRUE), "/", nrow(h)),
        siv_median = if (all(is.na(col(h, "strategic_industrial_value")))) NA_real_ else round(stats::median(col(h, "strategic_industrial_value"), na.rm = TRUE), 3),
        reason = paste0(countries[1], ": ", dplyr::coalesce(col(ref, "choice_reason")[1], "not classified")),
        large_yard_reason = col(ref, "large_yard_reason")[1],
        paper_placement = paper, agree = agree(m[1]), agree_first_country = agree(per[[1]]),
        check.names = FALSE, stringsAsFactors = FALSE)
    }
  }
  dplyr::bind_rows(rows)
}
