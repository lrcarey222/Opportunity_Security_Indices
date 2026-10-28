# Strategic posture: curated node attributes and sensitive layers (docs/strategic_position.md, section 7,
# "Nodes and sensitive layers"). Pure helpers: no IO. scripts/ read
# data/reference/posture_node_attributes.csv and data/reference/yards_fences_assessments.csv and pass
# them in.
#
# The table holds inputs about each node, never a yard or a posture: the consequence of disrupting it
# for a country that depends on it, substitutability, qualification and reconstitution times, whether
# making it builds transferable learning, maturity, leap candidacy and deployment criticality. One row
# per (tech, stage, layer_id, country_iso3):
#   * layer_id "stage" is the whole six-stage cell, judged on the physical path only (by convention
#     digitally_active = no, remote_control = no, separability = Unknown);
#   * other layers are sensitive layers inside a cell (control software, hardware, a material, a fuel,
#     a structure). A layer linked to a curated Yards & Fences component (yf_component_id) inherits
#     that table's value wherever its own cell is blank;
#   * country_iso3 "*" applies to every country; an ISO3 row overrides the "*" row of the same
#     (tech, stage, layer_id) field by field, for documented country-specific facts (stockpiles).
# The literal "Unknown" marks a value the public record does not support; it resolves to NA.
#
# Needs R/utils/supply_chain_stages.R (SUPPLY_CHAIN_STAGES).

PN_UNKNOWN <- "Unknown"
PN_ANY_COUNTRY <- "*"
PN_STAGE_LAYER <- "stage"

# The contract table (docs/strategic_position.md) plus market_scope. Nothing else: a yard, posture
# or paper_placement column would let a placement leak into the inputs.
PN_COLUMNS <- c(
  "node_id", "tech", "stage", "layer_id", "layer_label", "layer_type", "country_iso3", "yf_component_id",
  "critical_function", "consequence", "time_horizon", "digitally_active", "remote_control", "separability",
  "substitutability", "qualification_time", "reconstitution_time", "stockpile",
  "process_knowledge", "workforce_suppliers", "adjacent_innovation", "maturity",
  "leap_candidate", "successor", "beyond_novelty", "cost_perf_advantage", "incumbent_vulnerable", "scale_pathway",
  "deployment_critical", "primary_measure", "mineral", "market_scope", "source", "confidence", "notes"
)
PN_KEY <- c("tech", "stage", "layer_id", "country_iso3")

# Default technology set; scripts pass config/strategic_position.yml `headline_techs`.
PN_HEADLINE_TECHS <- c(
  "Batteries", "Coal", "Electric Grid", "Electric Vehicles", "Gas", "Geothermal", "Green Hydrogen",
  "Nuclear", "Oil", "Solar", "Wind"
)

PN_LAYER_TYPES <- c("stage", "control", "hardware", "material", "fuel", "structure")

# Categorical attributes, ordered from the open (or weak) end to the security (or strong) end as
# DOMAINS in dashboard/src/fence/engine/types.ts; the last four are this table's own.
PN_DOMAINS <- list(
  consequence = c("low", "moderate", "high", "severe"),
  time_horizon = c("years", "months", "weeks", "days"),
  digitally_active = c("no", "yes"),
  remote_control = c("no", "limited", "yes"),
  separability = c("separable", "partial", "inseparable"),
  substitutability = c("easy", "moderate", "hard", "impossible"),
  qualification_time = c("lt3m", "m3to12", "y1to3", "gt3y"),
  reconstitution_time = c("lt6m", "m6to12", "y1to3", "gt3y"),
  stockpile = c("gt12m", "m3to12", "lt3m", "none"),
  process_knowledge = c("no", "partly", "yes"),
  workforce_suppliers = c("no", "partly", "yes"),
  adjacent_innovation = c("no", "partly", "yes"),
  maturity = c("mature", "scaling", "emerging"),
  leap_candidate = c("no", "yes"),
  beyond_novelty = c("no", "partly", "yes"),
  cost_perf_advantage = c("no", "partly", "yes"),
  incumbent_vulnerable = c("no", "partly", "yes"),
  scale_pathway = c("no", "partly", "yes"),
  deployment_critical = c("no", "yes"),
  market_scope = c("global", "regional")
)
# market_scope says which coercion share the engine reads (world supply, or the country's own imports
# where the market is segmented), so it must be stated: Unknown is not allowed there.
PN_UNKNOWN_ALLOWED <- setdiff(names(PN_DOMAINS), "market_scope")

PN_LEAP_TESTS <- c("beyond_novelty", "cost_perf_advantage", "incumbent_vulnerable", "scale_pathway")
PN_PRIMARY_MEASURES <- c(
  "atlas_export_share", "iea_mineral_refining", "iea_mineral_mining", "uranium_mine_production",
  "ei_production", "iea_manufacturing_capacity", "none"
)
PN_MINERAL_MEASURES <- c("iea_mineral_refining", "iea_mineral_mining")
PN_CONFIDENCE <- c("low", "moderate", "high")

# Y&F columns a linked layer inherits when its own cell is blank (same names in both tables).
# Not inherited: surge_capacity (the engine takes surge from trusted world supply, data) and
# complementary_capabilities (measured per country, not an attribute of the node).
PN_YF_INHERIT <- c(
  "critical_function", "consequence", "time_horizon", "digitally_active", "remote_control", "separability",
  "substitutability", "qualification_time", "reconstitution_time", "stockpile",
  "process_knowledge", "workforce_suppliers", "adjacent_innovation", "maturity"
)

# Fields a country row may override, and the fields that carry a provenance in the resolved output.
PN_ATTR_FIELDS <- c("critical_function", names(PN_DOMAINS), "successor", "primary_measure", "mineral")
PN_DECISIVE_FIELDS <- names(PN_DOMAINS)

pn_stop_values <- function(fn, what, bad) {
  bad <- unique(as.character(bad))
  stop(
    fn, ": ", what, ": ", paste(utils::head(bad, 12), collapse = ", "),
    if (length(bad) > 12) paste0(" (and ", length(bad) - 12, " more)") else "", ".",
    call. = FALSE
  )
}

pn_blank <- function(x) is.na(x) | x == ""

pn_row_label <- function(tbl, i) {
  paste0(tbl$node_id[i], " [", tbl$tech[i], " / ", tbl$stage[i], " / ", tbl$layer_id[i], " / ", tbl$country_iso3[i], "]")
}

pn_normalise <- function(tbl, cols) {
  tbl <- as.data.frame(tbl, stringsAsFactors = FALSE)
  for (col in cols) {
    v <- trimws(as.character(tbl[[col]]))
    v[is.na(v)] <- ""
    tbl[[col]] <- v
  }
  tbl
}

# Checks the curated node table and returns it with every column as trimmed character (blank = "").
# Stops on the first class of problem, naming the rows. `yf_components` (the Y&F assessments table)
# checks every yf_component_id and its layer type; `measures` (rows of major_supplier_shares_tbl:
# tech, supply_chain, measure, mineral) only warns, because measure coverage changes with the data.
pn_read_check <- function(tbl, yf_components = NULL, techs = PN_HEADLINE_TECHS, measures = NULL) {
  fn <- "pn_read_check()"
  missing <- setdiff(PN_COLUMNS, names(tbl))
  if (length(missing) > 0) pn_stop_values(fn, "missing column(s)", missing)
  extra <- setdiff(names(tbl), PN_COLUMNS)
  if (length(extra) > 0) {
    pn_stop_values(fn, "column(s) outside the contract (node attributes are inputs, not yard or posture labels)", extra)
  }
  tbl <- pn_normalise(tbl[, PN_COLUMNS, drop = FALSE], PN_COLUMNS)
  star <- tbl$country_iso3 == PN_ANY_COUNTRY

  # Identity and provenance: every row says what it is, where it comes from and what it rests on.
  for (col in c("node_id", "tech", "stage", "layer_id", "country_iso3", "source", "confidence", "notes")) {
    empty <- which(pn_blank(tbl[[col]]))
    if (length(empty)) pn_stop_values(fn, paste0("empty ", col, " in row(s)"), empty)
  }
  for (col in c("layer_label", "layer_type", "leap_candidate", "deployment_critical", "primary_measure", "market_scope")) {
    empty <- which(star & pn_blank(tbl[[col]]))
    if (length(empty)) pn_stop_values(fn, paste0("empty ", col, " on '*' row(s)"), pn_row_label(tbl, empty))
  }
  if (anyDuplicated(tbl$node_id)) pn_stop_values(fn, "duplicate node_id(s)", tbl$node_id[duplicated(tbl$node_id)])
  key <- do.call(paste, c(tbl[PN_KEY], sep = " | "))
  if (anyDuplicated(key)) pn_stop_values(fn, "duplicate (tech, stage, layer_id, country_iso3) key(s)", key[duplicated(key)])

  # Vocabularies.
  bad <- setdiff(tbl$tech, techs)
  if (length(bad)) pn_stop_values(fn, "tech outside the headline technologies", bad)
  bad <- setdiff(tbl$stage, SUPPLY_CHAIN_STAGES)
  if (length(bad)) pn_stop_values(fn, "stage outside the six stages", bad)
  bad <- setdiff(tbl$country_iso3, PN_ANY_COUNTRY)
  bad <- bad[!grepl("^[A-Z]{3}$", bad)]
  if (length(bad)) pn_stop_values(fn, "country_iso3 is neither '*' nor an ISO3 code", bad)
  bad <- setdiff(tbl$layer_type[!pn_blank(tbl$layer_type)], PN_LAYER_TYPES)
  if (length(bad)) pn_stop_values(fn, "unknown layer_type", bad)
  for (col in names(PN_DOMAINS)) {
    allowed <- c(PN_DOMAINS[[col]], if (col %in% PN_UNKNOWN_ALLOWED) PN_UNKNOWN)
    bad <- which(!pn_blank(tbl[[col]]) & !tbl[[col]] %in% allowed)
    if (length(bad)) {
      pn_stop_values(fn, paste0(col, " outside {", paste(allowed, collapse = ", "), "} in row(s)"),
                     paste0(pn_row_label(tbl, bad), " = ", tbl[[col]][bad]))
    }
  }
  bad <- which(!pn_blank(tbl$primary_measure) & !tbl$primary_measure %in% PN_PRIMARY_MEASURES)
  if (length(bad)) pn_stop_values(fn, "unknown primary_measure in row(s)", paste0(pn_row_label(tbl, bad), " = ", tbl$primary_measure[bad]))
  bad <- which(!tbl$confidence %in% PN_CONFIDENCE)
  if (length(bad)) pn_stop_values(fn, "confidence must be low, moderate or high in row(s)", pn_row_label(tbl, bad))

  # Layer identity: layer_id "stage" is exactly the whole-cell layer type.
  st <- tbl[star, , drop = FALSE]
  bad <- which((st$layer_id == PN_STAGE_LAYER) != (st$layer_type == PN_STAGE_LAYER))
  if (length(bad)) pn_stop_values(fn, "layer_id 'stage' must go with layer_type 'stage' (and only it) in row(s)", pn_row_label(st, bad))

  # Completeness: one whole-cell row for every headline technology x stage.
  cells <- expand.grid(tech = techs, stage = SUPPLY_CHAIN_STAGES, stringsAsFactors = FALSE)
  have <- paste(st$tech[st$layer_id == PN_STAGE_LAYER], st$stage[st$layer_id == PN_STAGE_LAYER], sep = " | ")
  absent <- setdiff(paste(cells$tech, cells$stage, sep = " | "), have)
  if (length(absent)) pn_stop_values(fn, "missing whole-cell (layer_id 'stage', country '*') row(s)", absent)

  # Whole cells are judged on the physical path only; control-layer risks live in control layers.
  sr <- st$layer_id == PN_STAGE_LAYER
  bad <- which(sr & (st$digitally_active != "no" | st$remote_control != "no" | st$separability != PN_UNKNOWN))
  if (length(bad)) {
    pn_stop_values(fn, "whole-cell rows must carry digitally_active = no, remote_control = no, separability = Unknown", pn_row_label(st, bad))
  }
  bad <- which(sr & !pn_blank(st$yf_component_id))
  if (length(bad)) pn_stop_values(fn, "whole-cell rows cannot link a Y&F component", pn_row_label(st, bad))
  # Deployment has no world-supply measure (it is use, not supply), and nor does software.
  bad <- which((st$layer_type == "control" | (sr & st$stage == "Deployment")) & st$primary_measure != "none")
  if (length(bad)) pn_stop_values(fn, "control layers and Deployment cells take primary_measure 'none'", pn_row_label(st, bad))
  bad <- which(st$layer_type == "control" & pn_blank(st$yf_component_id) & st$digitally_active != "yes")
  if (length(bad)) pn_stop_values(fn, "control layers are digitally active", pn_row_label(st, bad))

  # A '*' row without a Y&F link has nothing to inherit: every attribute is stated (Unknown counts).
  own <- star & pn_blank(tbl$yf_component_id)
  for (col in c("critical_function", setdiff(names(PN_DOMAINS), PN_LEAP_TESTS))) {
    bad <- which(own & pn_blank(tbl[[col]]))
    if (length(bad)) pn_stop_values(fn, paste0("empty ", col, " on a '*' row with no Y&F link (use Unknown)"), pn_row_label(tbl, bad))
  }

  # Leap tests describe a successor technology, so they exist only on leap candidates.
  cand <- star & tbl$leap_candidate == "yes"
  for (col in c("successor", PN_LEAP_TESTS)) {
    bad <- which(cand & pn_blank(tbl[[col]]))
    if (length(bad)) pn_stop_values(fn, paste0("leap candidate without ", col), pn_row_label(tbl, bad))
    bad <- which(star & tbl$leap_candidate == "no" & !pn_blank(tbl[[col]]))
    if (length(bad)) pn_stop_values(fn, paste0(col, " filled on a row that is not a leap candidate"), pn_row_label(tbl, bad))
  }

  # Minerals: only material layers name one, and a mineral measure needs it.
  bad <- which(star & tbl$layer_type != "material" & !pn_blank(tbl$mineral))
  if (length(bad)) pn_stop_values(fn, "mineral filled on a row that is not a material layer", pn_row_label(tbl, bad))
  bad <- which(star & tbl$layer_type == "material" & tbl$primary_measure %in% PN_MINERAL_MEASURES & pn_blank(tbl$mineral))
  if (length(bad)) pn_stop_values(fn, "material layer on a mineral measure without a mineral", pn_row_label(tbl, bad))

  # Y&F links.
  linked <- which(!pn_blank(tbl$yf_component_id))
  if (length(linked) && !is.null(yf_components)) {
    yf <- pn_normalise(yf_components, intersect(c("component_id", "layer", PN_YF_INHERIT), names(yf_components)))
    bad <- setdiff(tbl$yf_component_id[linked], yf$component_id)
    if (length(bad)) pn_stop_values(fn, "yf_component_id(s) not in yards_fences_assessments.csv", bad)
    lt <- yf$layer[match(tbl$yf_component_id, yf$component_id)]
    bad <- which(star & !pn_blank(tbl$yf_component_id) & tbl$layer_type != lt)
    if (length(bad)) pn_stop_values(fn, "layer_type differs from the linked Y&F component's layer", pn_row_label(tbl, bad))
  }

  # Country rows refine a node that exists; they never create one.
  cty <- which(!star)
  if (length(cty)) {
    star_key <- do.call(paste, c(tbl[star, c("tech", "stage", "layer_id")], sep = " | "))
    ck <- do.call(paste, c(tbl[cty, c("tech", "stage", "layer_id")], sep = " | "))
    bad <- cty[!ck %in% star_key]
    if (length(bad)) pn_stop_values(fn, "country row(s) without a '*' row for the same (tech, stage, layer_id)", pn_row_label(tbl, bad))
    ref <- match(ck, star_key) + 0L
    sr_rows <- which(star)[ref]
    for (col in c("layer_type", "yf_component_id")) {
      bad <- cty[!pn_blank(tbl[[col]][cty]) & tbl[[col]][cty] != tbl[[col]][sr_rows]]
      if (length(bad)) pn_stop_values(fn, paste0("country row ", col, " differs from its '*' row"), pn_row_label(tbl, bad))
    }
    filled <- rowSums(!as.matrix(as.data.frame(lapply(tbl[cty, PN_ATTR_FIELDS], pn_blank)))) > 0
    bad <- cty[!filled]
    if (length(bad)) pn_stop_values(fn, "country row(s) override nothing", pn_row_label(tbl, bad))
  }

  if (!is.null(measures)) {
    ms <- as.data.frame(measures, stringsAsFactors = FALSE)
    have <- unique(paste(ms$tech, ms$supply_chain, ms$measure, sep = " | "))
    use <- which(star & !tbl$primary_measure %in% c("none", ""))
    miss <- use[!paste(tbl$tech[use], tbl$stage[use], tbl$primary_measure[use], sep = " | ") %in% have]
    if (length(miss)) {
      warning(fn, ": primary_measure has no rows for the node's (tech, stage) in `measures`: ",
              paste(utils::head(paste0(pn_row_label(tbl, miss), " = ", tbl$primary_measure[miss]), 12), collapse = "; "),
              if (length(miss) > 12) paste0(" (and ", length(miss) - 12, " more)") else "", call. = FALSE)
    }
  }
  tbl
}

# One fully resolved row per (iso3, tech, stage, layer_id) for the given countries. Precedence, field
# by field: the country's own row > the '*' row > the linked Y&F component (for PN_YF_INHERIT fields
# left blank on the '*' row). "Unknown" and blanks resolve to NA. Leap tests on rows that are not leap
# candidates are NA and not applicable.
#
# Adds: attr_provenance ("field:assessment|unknown|n_a;..." over PN_DECISIVE_FIELDS: every value here is
# a curated assessment), attr_origin ("field:node|yf|country;..." for assessed fields), country_override,
# attr_source, attr_confidence (the lower of the '*' and country rows' confidences) and attr_notes.
pn_resolve <- function(nodes, yf_components = NULL, iso3) {
  fn <- "pn_resolve()"
  if (missing(iso3) || length(iso3) == 0) stop(fn, ": give at least one iso3.", call. = FALSE)
  iso3 <- unique(as.character(iso3))
  bad <- iso3[!grepl("^[A-Z]{3}$", iso3)]
  if (length(bad)) pn_stop_values(fn, "iso3 values that are not ISO3 codes", bad)
  techs <- unique(as.character(nodes$tech))
  nodes <- pn_read_check(nodes, yf_components, techs = techs)
  linked <- !pn_blank(nodes$yf_component_id)
  if (any(linked) && is.null(yf_components)) {
    stop(fn, ": the table links Y&F components; pass yf_components to resolve them.", call. = FALSE)
  }

  star <- nodes[nodes$country_iso3 == PN_ANY_COUNTRY, , drop = FALSE]
  cty <- nodes[nodes$country_iso3 != PN_ANY_COUNTRY, , drop = FALSE]
  fields <- PN_ATTR_FIELDS
  origin <- matrix(ifelse(as.matrix(as.data.frame(lapply(star[fields], pn_blank))), NA_character_, "node"),
                   nrow = nrow(star), dimnames = list(NULL, fields))

  # Y&F inheritance: blank cells of a linked layer take the curated component's value.
  if (!is.null(yf_components) && any(!pn_blank(star$yf_component_id))) {
    yf <- pn_normalise(yf_components, intersect(c("component_id", PN_YF_INHERIT), names(yf_components)))
    j <- match(star$yf_component_id, yf$component_id)
    for (col in PN_YF_INHERIT) {
      take <- !is.na(j) & pn_blank(star[[col]]) & !pn_blank(yf[[col]][j])
      star[[col]][take] <- yf[[col]][j[take]]
      origin[take, col] <- "yf"
    }
  }

  out <- vector("list", length(iso3))
  for (k in seq_along(iso3)) {
    r <- star
    org <- origin
    r$country_override <- FALSE
    r$country_source <- ""
    r$country_confidence <- ""
    r$country_notes <- ""
    mine <- cty[cty$country_iso3 == iso3[k], , drop = FALSE]
    if (nrow(mine)) {
      i <- match(paste(mine$tech, mine$stage, mine$layer_id), paste(r$tech, r$stage, r$layer_id))
      for (col in fields) {
        take <- !pn_blank(mine[[col]])
        r[[col]][i[take]] <- mine[[col]][take]
        org[i[take], col] <- "country"
      }
      r$country_override[i] <- TRUE
      r$country_source[i] <- mine$source
      r$country_confidence[i] <- mine$confidence
      r$country_notes[i] <- mine$notes
    }
    r$iso3 <- iso3[k]
    # Origin is reported only for values that were assessed (an inherited Unknown has none).
    assessed <- !is.na(org) & !as.matrix(as.data.frame(lapply(r[fields], function(v) v %in% c("", PN_UNKNOWN))))
    r$attr_origin <- vapply(seq_len(nrow(r)), function(n) {
      hit <- assessed[n, ]
      paste0(fields[hit], ":", org[n, hit], collapse = ";")
    }, character(1))
    out[[k]] <- r
  }
  res <- do.call(rbind, out)

  # Leap tests are not applicable unless the node is a leap candidate.
  non_cand <- res$leap_candidate != "yes"
  for (col in c("successor", PN_LEAP_TESTS)) res[[col]][non_cand] <- ""
  prov <- vapply(PN_DECISIVE_FIELDS, function(col) {
    p <- ifelse(res[[col]] %in% c("", PN_UNKNOWN), "unknown", "assessment")
    if (col %in% PN_LEAP_TESTS) p[non_cand] <- "n_a"
    paste0(col, ":", p)
  }, character(nrow(res)))
  if (is.null(dim(prov))) prov <- matrix(prov, nrow = 1)
  res$attr_provenance <- apply(prov, 1, paste, collapse = ";")

  for (col in fields) {
    v <- res[[col]]
    v[v %in% c("", PN_UNKNOWN)] <- NA_character_
    res[[col]] <- v
  }
  conf_rank <- function(x) match(x, PN_CONFIDENCE)
  res$attr_confidence <- ifelse(
    res$country_override & !pn_blank(res$country_confidence),
    PN_CONFIDENCE[pmin(conf_rank(res$confidence), conf_rank(res$country_confidence))],
    res$confidence
  )
  res$attr_source <- ifelse(res$country_override, paste0(res$source, " | ", res$country_source), res$source)
  res$attr_notes <- ifelse(res$country_override, paste0(res$notes, " | ", res$country_notes), res$notes)
  res$yf_component_id[pn_blank(res$yf_component_id)] <- NA_character_

  keep <- c(
    "iso3", "tech", "stage", "layer_id", "layer_label", "layer_type", "node_id", "yf_component_id",
    fields, "country_override", "attr_provenance", "attr_origin", "attr_source", "attr_confidence", "attr_notes"
  )
  res <- res[, keep, drop = FALSE]
  rownames(res) <- NULL
  res
}

# Share of resolved rows whose decisive attribute is unknown, per field (diagnostics and the readout).
# Leap tests count only on leap candidates.
pn_coverage <- function(resolved, by = character()) {
  rows <- lapply(PN_DECISIVE_FIELDS, function(col) {
    use <- if (col %in% PN_LEAP_TESTS) resolved$leap_candidate %in% "yes" else rep(TRUE, nrow(resolved))
    d <- resolved[use, c(by, col), drop = FALSE]
    if (!nrow(d)) return(NULL)
    d$unknown <- is.na(d[[col]])
    g <- if (length(by)) interaction(d[by], drop = TRUE, sep = " | ") else factor(rep("all", nrow(d)))
    data.frame(
      field = col, group = levels(g),
      n = as.integer(tapply(d$unknown, g, length)),
      n_unknown = as.integer(tapply(d$unknown, g, sum)),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out$share_unknown <- round(out$n_unknown / out$n, 3)
  rownames(out) <- NULL
  out
}
