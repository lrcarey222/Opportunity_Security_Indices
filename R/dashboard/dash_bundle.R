# Bundle builders: turn pipeline-format tables into the dashboard bundle's compact components
# (pure; no IO). scripts/85_export_dashboard_data.R reads the pipeline outputs and calls these;
# dash_fixture_bundle() calls them on synthetic tables, so the tests exercise the same code.

DASH_STAGES <- c("Upstream", "Midstream", "Downstream")

# C-locale sort, as dplyr's group_by() / arrange() order character keys.
dash_sort <- function(x) sort(unique(x), method = "radix")

# One pillar from its category rows (Country, tech, supply_chain, category, category_score,
# imputed, missing_rule_applied) and variable rows (Country, tech, supply_chain, category,
# score_variable, variable, component_value, component_weight_within_overall, imputed,
# missing_rule_applied), headline technologies only. country_tbl: Country, iso3c, sovereign.
dash_bundle_pillar <- function(category_tbl, variable_tbl, categories_in_data, country_tbl,
                               allow_partial_categories = TRUE) {
  require_columns(category_tbl, c("Country", "tech", "supply_chain", "category", "category_score"), label = "category_tbl")
  require_columns(variable_tbl, c("Country", "tech", "supply_chain", "category", "variable", "component_value",
                                  "component_weight_within_overall"), label = "variable_tbl")
  category_tbl <- as.data.frame(category_tbl, stringsAsFactors = FALSE)
  key <- paste(category_tbl$Country, category_tbl$tech, category_tbl$supply_chain, sep = "\r")
  if (anyDuplicated(paste(key, category_tbl$category, sep = "\r"))) stop("category_tbl has duplicate (cell, category) rows.")
  cells <- unique(category_tbl[c("Country", "tech", "supply_chain")])
  cells <- cells[order(cells$Country, cells$tech, cells$supply_chain, method = "radix"), , drop = FALSE]
  cell_key <- paste(cells$Country, cells$tech, cells$supply_chain, sep = "\r")
  cats <- dash_sort(category_tbl$category)
  i <- match(key, cell_key)
  j <- match(category_tbl$category, cats)
  n <- nrow(cells)
  k <- length(cats)
  mk <- function(fill) matrix(fill, n, k, dimnames = list(NULL, cats))
  score <- mk(NA_real_)
  score[cbind(i, j)] <- as.numeric(category_tbl$category_score)
  present <- mk(FALSE)
  present[cbind(i, j)] <- TRUE
  imputed <- mk(NA)
  if ("imputed" %in% names(category_tbl)) imputed[cbind(i, j)] <- as.logical(category_tbl$imputed)
  missing_rule <- mk(NA_character_)
  if ("missing_rule_applied" %in% names(category_tbl)) missing_rule[cbind(i, j)] <- as.character(category_tbl$missing_rule_applied)

  variable_tbl <- as.data.frame(variable_tbl, stringsAsFactors = FALSE)
  vi <- match(paste(variable_tbl$Country, variable_tbl$tech, variable_tbl$supply_chain, sep = "\r"), cell_key)
  vj <- match(variable_tbl$category, cats)
  keep <- !is.na(vi) & !is.na(vj)
  # the pipeline joins variable rows to existing category rows only (inner join)
  keep <- keep & present[cbind(ifelse(is.na(vi), 1L, vi), ifelse(is.na(vj), 1L, vj))]
  variable_tbl <- variable_tbl[keep, , drop = FALSE]
  vi <- vi[keep]
  vj <- vj[keep]
  cwwo <- as.numeric(variable_tbl$component_weight_within_overall)
  imp <- if ("imputed" %in% names(variable_tbl)) as.logical(variable_tbl$imputed) else rep(FALSE, nrow(variable_tbl))
  grp <- (vj - 1L) * n + vi
  cw_tot <- mk(0)
  cw_imp <- mk(0)
  if (length(grp)) {
    s_tot <- tapply(ifelse(is.na(cwwo), 0, cwwo), grp, sum)
    s_imp <- tapply(ifelse(is.na(cwwo) | !(imp %in% TRUE), 0, cwwo), grp, sum)
    cw_tot[as.integer(names(s_tot))] <- as.numeric(s_tot)
    cw_imp[as.integer(names(s_imp))] <- as.numeric(s_imp)
  }
  n_var <- tabulate(vi, nbins = n)

  ct <- as.data.frame(country_tbl, stringsAsFactors = FALSE)
  cells_out <- tibble::tibble(
    Country = cells$Country,
    tech = cells$tech,
    supply_chain = cells$supply_chain,
    iso3c = ct$iso3c[match(cells$Country, ct$Country)],
    sovereign = ct$sovereign[match(cells$Country, ct$Country)] %in% TRUE
  )
  var_out <- tibble::tibble(
    cell = vi,
    category = as.character(variable_tbl$category),
    score_variable = if ("score_variable" %in% names(variable_tbl)) as.character(variable_tbl$score_variable) else NA_character_,
    variable = as.character(variable_tbl$variable),
    component_value = as.numeric(variable_tbl$component_value),
    cwwo = cwwo,
    imputed = imp,
    missing_rule_applied = if ("missing_rule_applied" %in% names(variable_tbl)) as.character(variable_tbl$missing_rule_applied) else NA_character_
  )
  list(
    cells = cells_out,
    categories = cats,
    score = score,
    present = present,
    imputed = imputed,
    missing_rule = missing_rule,
    cw_imp = cw_imp,
    cw_tot = cw_tot,
    n_var = n_var,
    categories_in_data = sort(as.character(categories_in_data), method = "radix"),
    allow_partial_categories = isTRUE(allow_partial_categories),
    var = var_out
  )
}

# Policy components (Country, tech, supply_chain, variable, value) for the variables given and
# the headline technologies: one value per cell and component (mean of duplicates, NA if all NA).
dash_bundle_policy <- function(policy_component_tbl, variables, techs) {
  require_columns(policy_component_tbl, c("Country", "tech", "supply_chain", "variable", "value"), label = "policy_component_tbl")
  x <- as.data.frame(policy_component_tbl, stringsAsFactors = FALSE)
  x <- x[x$variable %in% variables & x$tech %in% techs, c("Country", "tech", "supply_chain", "variable", "value"), drop = FALSE]
  x$value <- suppressWarnings(as.numeric(x$value))
  vars <- dash_sort(variables)
  key <- paste(x$Country, x$tech, x$supply_chain, sep = "\r")
  cells <- unique(x[c("Country", "tech", "supply_chain")])
  cells <- cells[order(cells$Country, cells$tech, cells$supply_chain, method = "radix"), , drop = FALSE]
  cell_key <- paste(cells$Country, cells$tech, cells$supply_chain, sep = "\r")
  g <- paste(key, x$variable, sep = "\r")
  agg <- tapply(x$value, g, function(v) if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE))
  first <- !duplicated(g)
  i <- match(key[first], cell_key)
  j <- match(x$variable[first], vars)
  values <- matrix(NA_real_, nrow(cells), length(vars), dimnames = list(NULL, vars))
  values[cbind(i, j)] <- as.numeric(agg[g[first]])
  list(
    cells = tibble::as_tibble(cells),
    variables = vars,
    values = values
  )
}

# Index cells of the partner arrays: (tech, stage) in the headline order x stages.
dash_partner_cells_tbl <- function(techs, stages = DASH_STAGES) {
  tibble::tibble(
    cell_id = seq_len(length(techs) * length(stages)),
    tech = rep(techs, each = length(stages)),
    supply_chain = rep(stages, times = length(techs))
  )
}

# Fill one variable of a long dyad table (reporter_iso, partner_iso, tech, supply_chain, value)
# into an array [U, U, C] / matrix [U, C] by reporter or partner / matrix [U, U]. For the reduced
# shapes it checks that the value is the same in every row of a key (`consistent`).
dash_fill_dyad <- function(r, p, ci, value, U_n, C_n, shape = c("rpc", "rc", "pc", "rp")) {
  shape <- match.arg(shape)
  if (shape == "rpc") {
    out <- array(NA_real_, c(U_n, U_n, C_n))
    out[r + (p - 1L) * U_n + (ci - 1L) * U_n * U_n] <- value
    return(list(value = out, consistent = TRUE))
  }
  idx <- switch(shape, rc = cbind(r, ci), pc = cbind(p, ci), rp = cbind(r, p))
  dims <- switch(shape, rc = c(U_n, C_n), pc = c(U_n, C_n), rp = c(U_n, U_n))
  out <- matrix(NA_real_, dims[1], dims[2])
  # a non-missing value wins over a missing one; then check every row against it
  ok <- !is.na(value)
  out[idx[!ok, , drop = FALSE]] <- NA_real_
  out[idx[ok, , drop = FALSE]] <- value[ok]
  back <- out[idx]
  same <- (is.na(back) & is.na(value)) | (!is.na(back) & !is.na(value) & back == value)
  list(value = out, consistent = all(same), n_conflicts = sum(!same))
}

# Friendshore or Opportunity long variable table -> arrays. `spec` maps component variables to
# shapes, e.g. c(imp_trade_index = "rpc", econ_opp_raw = "rc", ...); `index_variable` names the
# index rows, whose presence defines the dyad mask and whose values are kept for validation.
dash_bundle_dyads <- function(long_tbl, U, techs, spec, index_variable, stages = DASH_STAGES) {
  require_columns(long_tbl, c("reporter_iso", "partner_iso", "tech", "supply_chain", "variable", "value"), label = "dyad table")
  U_n <- length(U)
  C_n <- length(techs) * length(stages)
  r <- match(long_tbl$reporter_iso, U)
  p <- match(long_tbl$partner_iso, U)
  ci <- (match(long_tbl$tech, techs) - 1L) * length(stages) + match(long_tbl$supply_chain, stages)
  keep <- !is.na(r) & !is.na(p) & !is.na(ci)
  out <- list()
  checks <- list()
  for (v in c(names(spec), index_variable)) {
    sel <- which(keep & long_tbl$variable == v)
    shape <- if (v == index_variable) "rpc" else spec[[v]]
    if (!length(sel)) {
      out[[v]] <- NULL
      checks[[v]] <- tibble::tibble(variable = v, shape = shape, n_rows = 0L, consistent = NA, n_conflicts = NA_integer_)
      next
    }
    f <- dash_fill_dyad(r[sel], p[sel], ci[sel], as.numeric(long_tbl$value[sel]), U_n, C_n, shape)
    out[[v]] <- f$value
    checks[[v]] <- tibble::tibble(variable = v, shape = shape, n_rows = length(sel), consistent = f$consistent,
                                  n_conflicts = if (is.null(f$n_conflicts)) 0L else as.integer(f$n_conflicts))
    if (v == index_variable) {
      mask <- array(FALSE, c(U_n, U_n, C_n))
      mask[r[sel] + (p[sel] - 1L) * U_n + (ci[sel] - 1L) * U_n * U_n] <- TRUE
      out$mask <- mask
    }
  }
  out$checks <- dplyr::bind_rows(checks)
  out
}

# Friendshore's CM-FDI investment ties in its long table (commit 78be8b5, safer_friendshore.R): in
# the cells CM-FDI covers (Solar, Wind, Batteries and Electric Vehicles Midstream) every
# outbound_index row holds the CM-FDI tie of the reporter's parents to the partner, and its `source`
# names the Cleantech Manufacturing FDI Dataset ("(CM-FDI)"); every other cell keeps the IMF
# position. Relabels the outbound rows of those cells "outbound_index_cmfdi", so that the IMF rows
# stay one [U, U] matrix (shape "rp", constant over cells) and the ties fill an array [U, U, C].
# cmfdi_row: TRUE for the rows whose source is CM-FDI (default: read from the `source` column; FALSE
# everywhere without one). Returns list(long = the relabelled table without `source`, cells = the
# CM-FDI cell ids in cell order, outbound_source = "imf_dip" / "cm_fdi" by cell, n_rows = the
# CM-FDI outbound rows, n_mixed = outbound rows in a CM-FDI cell whose source is not CM-FDI, which
# a pipeline build never has).
dash_bundle_fs_cmfdi <- function(long_tbl, techs, stages = DASH_STAGES, cmfdi_row = NULL) {
  require_columns(long_tbl, c("tech", "supply_chain", "variable"), label = "Friendshore dyad table")
  ob <- which(long_tbl$variable == "outbound_index")
  if (is.null(cmfdi_row)) {
    cm_ob <- if ("source" %in% names(long_tbl)) grepl("(CM-FDI)", long_tbl$source[ob], fixed = TRUE) else rep(FALSE, length(ob))
  } else {
    cm_ob <- cmfdi_row[ob] %in% TRUE
  }
  ci <- (match(long_tbl$tech[ob], techs) - 1L) * length(stages) + match(long_tbl$supply_chain[ob], stages)
  cells <- sort(unique(ci[cm_ob & !is.na(ci)]))
  in_cell <- ci %in% cells
  outbound_source <- rep("imf_dip", length(techs) * length(stages))
  outbound_source[cells] <- "cm_fdi"
  long_tbl$variable[ob[in_cell]] <- "outbound_index_cmfdi"
  long_tbl$source <- NULL
  list(long = long_tbl, cells = as.integer(cells), outbound_source = outbound_source,
       n_rows = sum(cm_ob), n_mixed = sum(in_cell & !cm_ob))
}

# The bundle's CM-FDI parts of Friendshore from dash_bundle_dyads() output `fsb` (spec with
# outbound_index_cmfdi = "rpc" and chinese_parent_exposure = "pc") and dash_bundle_fs_cmfdi()
# output `cm`: outbound_source [C], outbound_cmfdi [U, U, K] (the K CM-FDI cells, NULL when none)
# and cpe [U, C] (chinese_parent_exposure by partner, NULL when the table has none).
dash_bundle_fs_cmfdi_parts <- function(fsb, cm) {
  K <- length(cm$cells)
  if (K > 0 && is.null(fsb$outbound_index_cmfdi)) stop("dash_bundle_fs_cmfdi_parts(): CM-FDI cells without outbound_index_cmfdi rows.")
  list(
    outbound_source = cm$outbound_source,
    outbound_cmfdi = if (K > 0) fsb$outbound_index_cmfdi[, , cm$cells, drop = FALSE] else NULL,
    cpe = fsb$chinese_parent_exposure
  )
}

# Best-five table (reporter_iso, partner_iso, variable, value [, imputed_weight_share]) -> value
# and share matrices [U, U] for the index variable.
dash_bundle_top5 <- function(top5_tbl, U, index_variable) {
  x <- top5_tbl[top5_tbl$variable == index_variable, , drop = FALSE]
  r <- match(x$reporter_iso, U)
  p <- match(x$partner_iso, U)
  ok <- !is.na(r) & !is.na(p)
  val <- matrix(NA_real_, length(U), length(U), dimnames = list(U, U))
  share <- val
  val[cbind(r[ok], p[ok])] <- as.numeric(x$value[ok])
  if ("imputed_weight_share" %in% names(x)) share[cbind(r[ok], p[ok])] <- as.numeric(x$imputed_weight_share[ok])
  exists <- matrix(FALSE, length(U), length(U), dimnames = list(U, U))
  exists[cbind(r[ok], p[ok])] <- TRUE
  list(value = val, share = share, exists = exists)
}

# Effective technology weight per cell for the dyad penalties: the table's ghg_index, a technology
# missing from it at the table's median (default_ghg in the dyad builders).
dash_bundle_gh <- function(tech_ghg, cells) {
  gh <- as.numeric(tech_ghg$ghg_index)
  names(gh) <- tech_ghg$tech
  default <- if (length(gh)) stats::median(gh, na.rm = TRUE) else 0.5
  v <- unname(gh[cells$tech])
  v[is.na(v)] <- default
  v
}

# Climate-policy index per economy for the dyad penalties: the mean per ISO3 of the cleaned CAT
# ratings (iso3c, climate_policy_index), an economy without one at their median (default_policy).
dash_bundle_cpi <- function(policy_iso, U) {
  cpi <- tapply(policy_iso$climate_policy_index, policy_iso$iso3c, function(v) mean(v, na.rm = TRUE))
  default <- if (length(cpi)) stats::median(cpi, na.rm = TRUE) else 0.5
  v <- unname(cpi[U])
  v[is.na(v)] <- default
  as.numeric(v)
}

# The bundle from its parts; manifest$baseline is computed from baseline_inputs.
dash_bundle_assemble <- function(manifest, dims, pillars, policy, si, partner) {
  bundle <- list(manifest = manifest, dims = dims, pillars = pillars, policy = policy, si = si, partner = partner)
  bundle$manifest$baseline <- NULL
  bundle$manifest$baseline <- dash_params(bundle = bundle)
  bundle
}
