# Competitive Position: how strong a country's demonstrated position is in a technology x
# six-stage cell, compared across countries within the cell (docs/strategic_position.md, section 2).
#
# The inputs are the EO and ES variable contributions from index_outputs.rds, whose
# component_value is already a within-cell index (higher = stronger) and whose `imputed` flag marks
# values the pillar builder filled in. Only the variables named under
# competitive_position.families enter, each family only at its own stages. There is no stage or
# technology prior and nothing from demand, technology readiness, world prices or policy: those are
# context (competitive_position.context_variables). Pure: callers pass the tables and the parsed
# config; nothing here reads files.

CP_FILLS <- c("na", "structural_zero")
CP_ORIENTS <- c("as_is", "invert")
CP_PILLARS <- c("EO", "ES")

# Categories that describe the technology or the world market rather than the country's own
# position. They are refused even if a config names them, so a config edit cannot quietly bring
# back the demand and readiness terms the Strategic Index mixed in.
CP_FORBIDDEN_CATEGORIES <- c("Technology Demand", "Technological Readiness", "Energy Prices")

CP_CONTRIB_COLUMNS <- c("Country", "tech", "supply_chain", "category", "variable", "component_value", "imputed")

sp_cp_or <- function(x, y) if (is.null(x)) y else x

sp_cp_spec_label <- function(pillar, category, variable, variable_pattern) {
  paste0(pillar, " / ", category, " / ", ifelse(is.na(variable), paste0("~", variable_pattern), variable))
}

# Flatten competitive_position.families into one row per (family, variable spec, stage) and check
# the vocabulary. A variable spec may carry its own `stages` (a subset of the family's), so one
# source can be kept out of a stage where it means something else; `orient: invert` flips a
# variable whose index runs the wrong way (value = 1 - component_value). `fill` defaults to `na`:
# missing is not zero unless the config says the source lists every holder.
sp_cp_specs <- function(cfg_cp) {
  fams <- cfg_cp[["families"]]
  if (!length(fams) || is.null(names(fams)) || any(!nzchar(names(fams)))) {
    stop("competitive_position.families must be a named, non-empty list.")
  }
  rows <- list()
  for (fam in names(fams)) {
    f <- fams[[fam]]
    where <- paste0("competitive_position.families.", fam)
    fam_stages <- as.character(unlist(f[["stages"]]))
    if (!length(fam_stages)) stop(where, " lists no stages.")
    assert_supply_chain_stages(fam_stages, paste0(where, ".stages"))
    if (!length(f[["variables"]])) stop(where, " lists no variables.")
    for (i in seq_along(f[["variables"]])) {
      v <- f[["variables"]][[i]]
      lab <- sprintf("%s.variables[%d]", where, i)
      pillar <- as.character(sp_cp_or(v[["pillar"]], NA_character_))
      if (!pillar %in% CP_PILLARS) stop(lab, ": pillar must be one of ", paste(CP_PILLARS, collapse = ", "), ".")
      category <- as.character(sp_cp_or(v[["category"]], NA_character_))
      if (is.na(category) || !nzchar(category)) stop(lab, " has no category.")
      if (category %in% CP_FORBIDDEN_CATEGORIES) {
        stop(lab, ": category '", category, "' measures demand, technology maturity or world prices, ",
             "not a country's own position, and cannot enter Competitive Position.")
      }
      if (is.null(v[["variable"]]) == is.null(v[["variable_pattern"]])) {
        stop(lab, " needs exactly one of `variable` or `variable_pattern`.")
      }
      fill <- as.character(sp_cp_or(v[["fill"]], "na"))
      if (!fill %in% CP_FILLS) stop(lab, ": fill must be one of ", paste(CP_FILLS, collapse = ", "), ".")
      orient <- as.character(sp_cp_or(v[["orient"]], "as_is"))
      if (!orient %in% CP_ORIENTS) stop(lab, ": orient must be one of ", paste(CP_ORIENTS, collapse = ", "), ".")
      v_stages <- if (is.null(v[["stages"]])) fam_stages else as.character(unlist(v[["stages"]]))
      outside <- setdiff(v_stages, fam_stages)
      if (length(outside)) {
        stop(lab, ": stages ", paste(outside, collapse = ", "), " are not stages of family ", fam, ".")
      }
      variable <- if (is.null(v[["variable"]])) NA_character_ else as.character(v[["variable"]])
      pattern <- if (is.null(v[["variable_pattern"]])) NA_character_ else as.character(v[["variable_pattern"]])
      rows[[length(rows) + 1]] <- data.frame(
        family = fam,
        spec = sp_cp_spec_label(pillar, category, variable, pattern),
        pillar = pillar, category = category, variable_spec = variable, variable_pattern = pattern,
        fill = fill, orient = orient, supply_chain = v_stages,
        stringsAsFactors = FALSE
      )
    }
  }
  out <- do.call(rbind, rows)
  dup <- duplicated(out[, c("family", "spec", "supply_chain")])
  if (any(dup)) stop("competitive_position.families names ", unique(out$spec[dup])[1], " twice in family ", unique(out$family[dup])[1], ".")
  out
}

# The context entries as a table (pillar, category, variable, reason); variable "*" = the whole
# category.
sp_cp_context_specs <- function(cfg_cp) {
  ctx <- cfg_cp[["context_variables"]]
  if (!length(ctx)) {
    return(data.frame(pillar = character(), category = character(), variable = character(), reason = character(),
                      stringsAsFactors = FALSE))
  }
  do.call(rbind, lapply(ctx, function(x) data.frame(
    pillar = as.character(sp_cp_or(x[["pillar"]], NA_character_)),
    category = as.character(sp_cp_or(x[["category"]], NA_character_)),
    variable = as.character(sp_cp_or(x[["variable"]], "*")),
    reason = as.character(sp_cp_or(x[["reason"]], NA_character_)),
    stringsAsFactors = FALSE
  )))
}

# TRUE for rows (pillar, category, variable) that a context entry names.
sp_cp_is_context <- function(pillar, category, variable, ctx) {
  hit <- rep(FALSE, length(pillar))
  for (i in seq_len(nrow(ctx))) {
    hit <- hit | (pillar == ctx$pillar[i] & category == ctx$category[i] &
                    (ctx$variable[i] == "*" | variable == ctx$variable[i]))
  }
  hit
}

sp_cp_check_contrib <- function(tbl, label) {
  missing <- setdiff(CP_CONTRIB_COLUMNS, names(tbl))
  if (length(missing)) stop(label, " lacks column(s) ", paste(missing, collapse = ", "), ".")
  assert_supply_chain_stages(tbl$supply_chain, label)
  invisible(TRUE)
}

# The rows of one pillar table that a spec selects, at the spec's stages.
sp_cp_select <- function(tbl, specs) {
  keys <- unique(specs[, c("family", "spec", "pillar", "category", "variable_spec", "variable_pattern", "fill", "orient")])
  out <- lapply(seq_len(nrow(keys)), function(i) {
    k <- keys[i, ]
    stages <- specs$supply_chain[specs$family == k$family & specs$spec == k$spec]
    var_hit <- if (is.na(k$variable_spec)) grepl(k$variable_pattern, tbl$variable) else tbl$variable == k$variable_spec
    keep <- tbl$category == k$category & tbl$supply_chain %in% stages & var_hit
    rows <- tbl[keep, CP_CONTRIB_COLUMNS, drop = FALSE]
    if (!nrow(rows)) return(NULL)
    cbind(k[rep(1, nrow(rows)), c("family", "spec", "pillar", "fill", "orient")], rows, row.names = NULL)
  })
  dplyr::bind_rows(out)
}

# Per (family, spec, stage): whether the data carries the variable at all, for how many
# technologies, and how many source-observed values it has. `n_constant_cells` counts technology
# cells in which every observed value (at least three countries) is the same: such a variable
# cannot tell countries apart there.
sp_cp_variable_coverage <- function(specs, sel) {
  per_cell <- if (nrow(sel)) {
    sel %>%
      dplyr::mutate(.obs = !dplyr::coalesce(as.logical(.data$imputed), FALSE) & !is.na(.data$component_value)) %>%
      dplyr::group_by(.data$family, .data$spec, .data$supply_chain, .data$tech) %>%
      dplyr::summarise(
        n_rows = dplyr::n(),
        n_source_observed = sum(.data$.obs),
        .constant = sum(.data$.obs) >= 3 & dplyr::n_distinct(.data$component_value[.data$.obs]) == 1,
        .groups = "drop"
      ) %>%
      dplyr::group_by(.data$family, .data$spec, .data$supply_chain) %>%
      dplyr::summarise(
        n_techs = dplyr::n(),
        techs = paste(sort(unique(.data$tech)), collapse = ", "),
        n_rows = sum(.data$n_rows),
        n_source_observed = sum(.data$n_source_observed),
        n_constant_cells = sum(.data$.constant),
        .groups = "drop"
      )
  } else {
    data.frame(family = character(), spec = character(), supply_chain = character(), n_techs = integer(),
               techs = character(), n_rows = integer(), n_source_observed = integer(), n_constant_cells = integer())
  }
  specs[, c("family", "spec", "pillar", "category", "variable_spec", "variable_pattern", "fill", "orient", "supply_chain")] %>%
    dplyr::left_join(per_cell, by = c("family", "spec", "supply_chain")) %>%
    dplyr::mutate(
      dplyr::across(c("n_techs", "n_rows", "n_source_observed", "n_constant_cells"), ~ dplyr::coalesce(as.integer(.x), 0L)),
      techs = dplyr::coalesce(.data$techs, ""),
      present = .data$n_rows > 0
    )
}

# Stop when a configured variable is absent from the data at every stage it applies to, naming per
# stage every configured variable the data lacks there.
sp_cp_stop_if_absent <- function(coverage) {
  by_spec <- tapply(coverage$present, paste(coverage$family, coverage$spec, sep = ": "), any)
  gone <- names(by_spec)[!by_spec]
  if (!length(gone)) return(invisible(TRUE))
  absent <- coverage[!coverage$present, ]
  absent$stage_f <- supply_chain_stage_factor(absent$supply_chain)
  per_stage <- vapply(split(absent, absent$stage_f, drop = TRUE), function(a) {
    paste0(as.character(a$stage_f[1]), ": ", paste(paste(a$family, a$spec, sep = " "), collapse = "; "))
  }, character(1))
  stop(
    "sp_competitive_position(): configured variable(s) absent from the data at every stage they apply to: ",
    paste(gone, collapse = "; "), ".\nConfigured variables absent, by stage:\n  ",
    paste(per_stage, collapse = "\n  "), call. = FALSE
  )
}

# Competitive Position per Country x tech x six-stage cell.
#
# eo_contrib / es_contrib: index_outputs.rds economic_opportunity_variable_contributions and
# energy_security_variable_contributions. cfg_cp: the parsed competitive_position block of
# config/strategic_position.yml. techs: optional technologies to keep (NULL = all in the data).
#
# A value the pillar builder imputed becomes NA under `fill: na` and 0 under
# `fill: structural_zero`; so does a country the source omits from a cell it covers (the inputs are
# completed to every country x variable of the technology x stage cell). A structural zero is the
# floor of the oriented scale (no holding = weakest position), and it counts as observed because
# the source lists every holder; value_basis tells it apart from a reported value.
#
# Returns list(
#   cells: one row per Country x tech x supply_chain with competitive_position_score (mean of the
#     observed family scores), _percentile (mid-rank across the countries scored in the cell; NA in
#     a thin cell), _rank (1 = strongest; NA in a thin cell), cp_<family> (mean of the family's
#     observed variables), cp_n_families, cp_n_families_applicable, cp_family_coverage,
#     cp_n_variables, cp_n_structural_zeros, cp_n_countries (scored in the cell), cp_thin_cell,
#     cp_basis;
#   inputs: one row per (Country, tech, supply_chain, family, variable) with pillar, category,
#     component_value, imputed, fill, orient, value, observed, value_basis and weight (intended
#     weight: equal over the families with data in the cell and over each family's variables in
#     it, observed or not; sums to 1 per cell);
#   variable_coverage: per family x variable spec x stage, whether the data carries it, for which
#     technologies, and how much of it is observed.
# )
sp_competitive_position <- function(eo_contrib, es_contrib, cfg_cp, techs = NULL) {
  sp_cp_check_contrib(eo_contrib, "eo_contrib")
  sp_cp_check_contrib(es_contrib, "es_contrib")
  specs <- sp_cp_specs(cfg_cp)
  ctx <- sp_cp_context_specs(cfg_cp)
  min_vars <- as.integer(sp_cp_or(cfg_cp[["min_observed_variables"]], 1L))
  min_countries <- as.integer(sp_cp_or(cfg_cp[["min_countries_per_cell"]], 5L))

  tables <- list(EO = eo_contrib, ES = es_contrib)
  sel <- dplyr::bind_rows(lapply(CP_PILLARS, function(p) {
    tbl <- as.data.frame(tables[[p]], stringsAsFactors = FALSE)
    if (!is.null(techs)) tbl <- tbl[tbl$tech %in% techs, , drop = FALSE]
    s <- specs[specs$pillar == p, , drop = FALSE]
    if (!nrow(s)) return(NULL)
    sp_cp_select(tbl, s)
  }))

  coverage <- sp_cp_variable_coverage(specs, sel)
  sp_cp_stop_if_absent(coverage)

  ctx_hit <- sp_cp_is_context(sel$pillar, sel$category, sel$variable, ctx)
  if (any(ctx_hit)) {
    bad <- unique(paste(sel$family[ctx_hit], sel$pillar[ctx_hit], sel$category[ctx_hit], sel$variable[ctx_hit], sep = " / "))
    stop("sp_competitive_position(): context variable(s) selected by a family: ", paste(bad, collapse = "; "),
         ". Context variables never enter the score.", call. = FALSE)
  }
  row_key <- c("pillar", "Country", "tech", "supply_chain", "category", "variable")
  dup <- duplicated(sel[, row_key])
  if (any(dup)) {
    d <- sel[dup, ][1, ]
    stop("sp_competitive_position(): ", d$pillar, " / ", d$category, " / ", d$variable, " at ", d$tech, " - ",
         d$supply_chain, " is selected twice (two families, or duplicate rows in the source).", call. = FALSE)
  }

  # Complete every technology x stage cell to all its countries x the variables the data carries
  # there, so a country the source leaves out is explicit (and every country's weights agree).
  var_keys <- c("family", "spec", "pillar", "fill", "orient", "category", "variable")
  cell_vars <- unique(sel[, c("tech", "supply_chain", var_keys)])
  cell_countries <- unique(sel[, c("tech", "supply_chain", "Country")])
  inputs <- cell_countries %>%
    dplyr::inner_join(cell_vars, by = c("tech", "supply_chain"), relationship = "many-to-many") %>%
    dplyr::left_join(
      dplyr::mutate(sel[, c(row_key, "component_value", "imputed")], .in_source = TRUE),
      by = row_key
    ) %>%
    dplyr::mutate(
      .in_source = dplyr::coalesce(.data$.in_source, FALSE),
      imputed = dplyr::coalesce(as.logical(.data$imputed), FALSE),
      .reported = .data$.in_source & !.data$imputed & !is.na(.data$component_value),
      value_basis = dplyr::case_when(
        .data$.reported ~ "observed",
        .data$fill == "structural_zero" ~ "structural_zero",
        TRUE ~ "not_observed"
      ),
      value = dplyr::case_when(
        .data$value_basis == "observed" & .data$orient == "invert" ~ 1 - .data$component_value,
        .data$value_basis == "observed" ~ .data$component_value,
        .data$value_basis == "structural_zero" ~ 0,
        TRUE ~ NA_real_
      ),
      observed = .data$value_basis != "not_observed"
    ) %>%
    dplyr::group_by(.data$Country, .data$tech, .data$supply_chain) %>%
    dplyr::mutate(.n_fam = dplyr::n_distinct(.data$family)) %>%
    dplyr::group_by(.data$family, .add = TRUE) %>%
    dplyr::mutate(weight = 1 / (.data$.n_fam * dplyr::n())) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      "Country", "tech", "supply_chain", "family", "pillar", "category", "variable",
      "component_value", "imputed", "fill", "orient", "value", "observed", "value_basis", "weight"
    ) %>%
    dplyr::arrange(.data$tech, supply_chain_stage_factor(.data$supply_chain), .data$Country, .data$family, .data$variable)

  fam_names <- names(cfg_cp[["families"]])
  fam_scores <- inputs %>%
    dplyr::group_by(.data$Country, .data$tech, .data$supply_chain, .data$family) %>%
    dplyr::summarise(
      score = sp_mean_available(.data$value),
      n_vars = sum(!is.na(.data$value)),
      n_zero = sum(.data$value_basis == "structural_zero"),
      .groups = "drop"
    )
  wide <- fam_scores %>%
    dplyr::select("Country", "tech", "supply_chain", "family", "score") %>%
    tidyr::pivot_wider(names_from = "family", values_from = "score", names_prefix = "cp_")
  for (f in paste0("cp_", fam_names)) if (!f %in% names(wide)) wide[[f]] <- NA_real_
  counts <- fam_scores %>%
    dplyr::group_by(.data$Country, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(
      cp_n_families = sum(!is.na(.data$score)),
      cp_n_variables = sum(.data$n_vars),
      cp_n_structural_zeros = sum(.data$n_zero),
      .groups = "drop"
    )
  applicable <- specs %>%
    dplyr::group_by(.data$supply_chain) %>%
    dplyr::summarise(cp_n_families_applicable = dplyr::n_distinct(.data$family), .groups = "drop")

  fam_cols <- paste0("cp_", fam_names)
  cells <- wide %>%
    dplyr::left_join(counts, by = c("Country", "tech", "supply_chain")) %>%
    dplyr::left_join(applicable, by = "supply_chain")
  # Equal weight per observed family: a family with more variables does not count for more.
  cells$competitive_position_score <- sp_row_mean_available(cells[, fam_cols, drop = FALSE])
  cells <- cells %>%
    dplyr::mutate(
      competitive_position_score = dplyr::if_else(.data$cp_n_variables >= min_vars, .data$competitive_position_score, NA_real_),
      cp_family_coverage = .data$cp_n_families / .data$cp_n_families_applicable
    ) %>%
    dplyr::group_by(.data$tech, .data$supply_chain) %>%
    dplyr::mutate(
      cp_n_countries = sum(!is.na(.data$competitive_position_score)),
      cp_thin_cell = .data$cp_n_countries < min_countries,
      competitive_position_percentile = if (dplyr::first(.data$cp_thin_cell)) NA_real_ else sp_midrank_pct(.data$competitive_position_score),
      competitive_position_rank = if (dplyr::first(.data$cp_thin_cell)) NA_integer_ else sp_rank_desc(.data$competitive_position_score)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cp_basis = dplyr::if_else(.data$supply_chain == "Deployment", "market_deployment_position", "industrial_position")) %>%
    dplyr::select(
      "Country", "tech", "supply_chain", "competitive_position_score", "competitive_position_percentile",
      "competitive_position_rank", dplyr::all_of(fam_cols), "cp_n_families", "cp_n_families_applicable",
      "cp_family_coverage", "cp_n_variables", "cp_n_structural_zeros", "cp_n_countries", "cp_thin_cell", "cp_basis"
    ) %>%
    dplyr::arrange(.data$tech, supply_chain_stage_factor(.data$supply_chain), dplyr::desc(.data$competitive_position_score), .data$Country)

  list(cells = cells, inputs = inputs, variable_coverage = coverage)
}

# The context variables of competitive_position.context_variables as a long table (Country, tech,
# supply_chain, pillar, category, variable, value = component_value, observed, reason), for
# carrying demand and market-size context next to the position. Never used in the score.
sp_cp_context <- function(eo_contrib, es_contrib, cfg_cp, techs = NULL) {
  ctx <- sp_cp_context_specs(cfg_cp)
  tables <- list(EO = eo_contrib, ES = es_contrib)
  out <- lapply(CP_PILLARS, function(p) {
    tbl <- as.data.frame(tables[[p]], stringsAsFactors = FALSE)
    if (!is.null(techs)) tbl <- tbl[tbl$tech %in% techs, , drop = FALSE]
    c_p <- ctx[ctx$pillar == p, , drop = FALSE]
    if (!nrow(c_p) || !nrow(tbl)) return(NULL)
    reason <- rep(NA_character_, nrow(tbl))
    # first matching entry wins, as in the config's order
    for (i in rev(seq_len(nrow(c_p)))) {
      hit <- tbl$category == c_p$category[i] & (c_p$variable[i] == "*" | tbl$variable == c_p$variable[i])
      reason[hit] <- c_p$reason[i]
    }
    rows <- tbl[!is.na(reason), , drop = FALSE]
    if (!nrow(rows)) return(NULL)
    data.frame(
      Country = rows$Country, tech = rows$tech, supply_chain = rows$supply_chain, pillar = p,
      category = rows$category, variable = rows$variable, value = rows$component_value,
      observed = !dplyr::coalesce(as.logical(rows$imputed), FALSE) & !is.na(rows$component_value),
      reason = reason[!is.na(reason)], stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(out)
}
