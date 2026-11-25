# Pipeline-level data QC on the processed tables (docs/QC_SYSTEM_DESIGN.md 6.2): schema, missingness, numeric
# validity, trade consistency, classification logic and provenance completeness. Every function takes data
# frames / parsed lists and returns check rows (qc_core.R). Nothing here changes a value, a threshold or a
# classification: the classification checks re-apply the canonical rule (R/strategic_position/decision_engine.R
# sp_choice_rule, sp_choice_high_cp, sp_cp_contender_pct and their helpers, which the caller sources) to the
# stored inputs and report where the stored result is not reproduced.
#
# Vocabularies are read from the canonical constants: SUPPLY_CHAIN_STAGES (R/utils/supply_chain_stages.R),
# SP_CHOICES / SP_CHOICE_VERB_POSTURE (decision_engine.R). They are default arguments, evaluated only when used.
#
# Pure functions: no IO. Needs dplyr, tibble and qc_core.R.

# --------------------------------------------------------------------------------------------------------------
# Column expectations for strategic_position_tbl
# --------------------------------------------------------------------------------------------------------------

QC_SP_KEY <- c("iso3", "tech", "supply_chain")
QC_SP_REQUIRED <- c("Country", "iso3", "tech", "supply_chain", "strategic_choice", "strategic_posture", "engine_posture",
                    "posture_exclusion", "supply_chain_need", "strategic_industrial_value", "industrial_opportunity", "siv_cap",
                    "deployment_need", "cp_contender_pct", "cp_contender_basis", "cp_is_contender", "t3_capability_lo",
                    "pathway_credible", "large_yard_case", "compete_mode", "deploy_mode")
# Columns that hold a 0-1 share, score or percentile.
QC_SP_UNIT_COLS <- c("supply_chain_need", "supply_chain_need_uncapped", "strategic_industrial_value", "industrial_opportunity",
                     "deployment_need", "cp_contender_pct", "competitive_position_score", "competitive_position_percentile",
                     "strategic_need_percentile", "home_share", "home_share_exposure", "top_supplier_share", "china_share",
                     "opposite_bloc_share", "trusted_world_share", "coercive_top_share", "coercive_world_share",
                     "coercive_import_share", "world_top_share", "trade_benchmark_share", "trusted_alternatives_share",
                     "cp_export_position", "cp_production_position", "cp_manufacturing_capacity", "cp_investment_position",
                     "cp_cost_position", "cp_resource_endowment", "cp_deployment_scale", "cp_innovation_position",
                     "siv_market", "siv_commons", "siv_option")
QC_SP_HHI_COLS <- c("import_hhi")
QC_SP_NONNEG_COLS <- c("imports_usd", "imports_to_gdp", "trade_benchmark_exports", "trade_benchmark_rca", "n_suppliers")
QC_SP_RATIO_COLS <- c("trade_benchmark_rca", "imports_to_gdp")
# World shares: summed over the economies of a node (tech x stage) they cannot exceed 1. The value names the
# column that records each row's basis (NA: none), so an excess can be attributed to a basis.
QC_SP_WORLD_SHARE_COLS <- c(trade_benchmark_share = "trade_benchmark_source", home_share_exposure = NA)
# The decision inputs whose missingness is reported, with the stages where each applies.
QC_SP_MISSING_INPUTS <- list(
  supply_chain_need = "producing", strategic_industrial_value = "producing", cp_contender_pct = "producing",
  deployment_need = "Deployment"
)

# --------------------------------------------------------------------------------------------------------------
# Schema
# --------------------------------------------------------------------------------------------------------------

# Schema of the strategic position table: required columns, types, the six stages, the canonical choice keys,
# key uniqueness, ISO3 format, Country-iso3 one-to-one, and completeness of the country x tech x stage grid.
qc_schema_sp <- function(tbl, table = "strategic_position_tbl", stages = SUPPLY_CHAIN_STAGES, choices = SP_CHOICES,
                         required = QC_SP_REQUIRED, key = QC_SP_KEY, numeric_cols = c(QC_SP_UNIT_COLS, QC_SP_HHI_COLS, QC_SP_NONNEG_COLS)) {
  out <- list()
  miss <- setdiff(required, names(tbl))
  out$req <- qc_check("schema_required_columns", "schema", table, qc_status_if(length(miss), TRUE), length(miss),
                      if (length(miss)) "required columns missing" else sprintf("all %d required columns present", length(required)), miss)
  num <- intersect(numeric_cols, names(tbl))
  bad_type <- num[!vapply(num, function(cn) is.numeric(tbl[[cn]]), logical(1))]
  out$type <- qc_check("schema_numeric_types", "schema", table, qc_status_if(length(bad_type), TRUE), length(bad_type),
                       if (length(bad_type)) "numeric columns stored as another type" else sprintf("%d numeric columns are numeric", length(num)),
                       bad_type)
  if ("supply_chain" %in% names(tbl)) {
    bad <- tbl[!tbl$supply_chain %in% stages, , drop = FALSE]
    out$stage <- qc_check("schema_stage_vocabulary", "schema", table, qc_status_if(nrow(bad), TRUE), nrow(bad),
                          paste("supply_chain must be one of", paste(stages, collapse = ", ")),
                          qc_lab("supply_chain=", unique(as.character(bad$supply_chain))))
  }
  if (all(c("strategic_choice", "posture_exclusion") %in% names(tbl))) {
    excluded <- !is.na(tbl$posture_exclusion)
    bad <- tbl[!excluded & !(tbl$strategic_choice %in% choices), , drop = FALSE]
    out$choice <- qc_check("schema_choice_vocabulary", "schema", table, qc_status_if(nrow(bad), TRUE), nrow(bad),
                           paste("every non-excluded row carries a canonical choice key:", paste(choices, collapse = ", ")),
                           qc_examples(bad, c(key, "strategic_choice")))
    exc <- tbl[excluded, , drop = FALSE]
    out$excluded <- qc_check("schema_excluded_rows", "schema", table, "info", nrow(exc),
                             sprintf("rows excluded from the decision (posture_exclusion), economies: %s",
                                     paste(sort(unique(qc_col(exc, "iso3"))), collapse = ", ")),
                             qc_lab("posture_exclusion=", unique(exc$posture_exclusion)))
  }
  k <- intersect(key, names(tbl))
  if (length(k) == length(key)) {
    d <- duplicated(tbl[, key]) | duplicated(tbl[, key], fromLast = TRUE)
    out$key <- qc_check("schema_key_unique", "schema", table, qc_status_if(sum(d), TRUE), sum(d),
                        paste("rows unique on", paste(key, collapse = ", ")), qc_examples(tbl[d, , drop = FALSE], key))
  }
  ic <- qc_iso3_col(tbl)
  if (!is.null(ic)) {
    bad <- unique(tbl[[ic]][!qc_iso3_ok(tbl[[ic]])])
    out$iso <- qc_check("schema_iso3_format", "schema", table, qc_status_if(length(bad), TRUE), length(bad),
                        "ISO3 codes are three upper-case letters", qc_lab(paste0(ic, "="), ifelse(is.na(bad), "NA", bad)))
    if ("Country" %in% names(tbl)) out$iso_map <- qc_country_iso3_map(tbl, ic, table)
    if (all(c("tech", "supply_chain") %in% names(tbl))) {
      n_iso <- length(unique(tbl[[ic]]))
      grid <- n_iso * length(unique(tbl$tech)) * length(unique(tbl$supply_chain))
      gap <- grid - nrow(unique(tbl[, c(ic, "tech", "supply_chain")]))
      out$grid <- qc_check("schema_grid_complete", "schema", table, qc_status_if(gap), gap,
                           sprintf("%d economies x %d techs x %d stages = %d cells expected", n_iso, length(unique(tbl$tech)),
                                   length(unique(tbl$supply_chain)), grid))
    }
  }
  qc_bind(out)
}

# Country names and ISO3 codes one-to-one (the index tables key on Country, the partner code on ISO3).
qc_country_iso3_map <- function(tbl, ic = qc_iso3_col(tbl), table = "table") {
  pairs <- unique(tbl[!is.na(tbl[[ic]]) & !is.na(tbl$Country), c("Country", ic)])
  a <- unique(pairs$Country[duplicated(pairs$Country)])
  b <- unique(pairs[[ic]][duplicated(pairs[[ic]])])
  n <- length(a) + length(b)
  qc_check("schema_country_iso3_one_to_one", "schema", table, qc_status_if(n), n,
           "each Country string maps to one ISO3 code and back", c(qc_lab("Country=", a), qc_lab(paste0(ic, "="), b)))
}

# Schema of a processed table. Tidy theme tables (docs/data_dictionary.md: Country, tech, supply_chain, category,
# variable, data_type, value, Year, source, explanation) go through validate_schema() (R/utils/schema.R) when it
# is sourced; other tables (project-level, wide) are only checked for what they carry: the six-stage vocabulary
# (legacy Upstream / Midstream / Downstream flagged), ISO3 format and key uniqueness. stage_cols: the columns
# holding a stage label (a "|"-separated list is split).
qc_schema_theme <- function(tbl, table, stages = SUPPLY_CHAIN_STAGES, legacy = c("Upstream", "Midstream", "Downstream"),
                            stage_cols = c("supply_chain", "stages", "stage"), key = NULL) {
  out <- list()
  tbl <- as.data.frame(dplyr::ungroup(tbl))
  tidy <- all(c("variable", "data_type", "value") %in% names(tbl))
  if (tidy && exists("validate_schema", mode = "function")) {
    msg <- tryCatch({ validate_schema(tbl); NULL }, error = function(e) conditionMessage(e))
    out$vs <- qc_check("schema_theme_validate_schema", "schema", table, if (is.null(msg)) "info" else "warn", as.integer(!is.null(msg)),
                       msg %||% "validate_schema() passes", needs_human_explanation = !is.null(msg))
  } else if (!tidy) {
    out$vs <- qc_check("schema_theme_validate_schema", "schema", table, "info", 0L,
                       "not a tidy theme table (no variable / data_type / value): validate_schema() does not apply")
  }
  for (sc in intersect(stage_cols, names(tbl))) {
    v <- as.character(tbl[[sc]])
    parts <- unlist(strsplit(v[!is.na(v)], "|", fixed = TRUE))
    leg_rows <- sum(vapply(strsplit(dplyr::coalesce(v, ""), "|", fixed = TRUE), function(p) any(p %in% legacy), logical(1)))
    unk <- setdiff(unique(parts[!parts %in% c(stages, legacy)]), c(NA, ""))
    out[[paste0("stage_", sc)]] <- qc_check(paste0("schema_theme_stage_vocabulary", if (sc == "supply_chain") "" else paste0("_", sc)),
      "schema", table, qc_status_if(leg_rows + length(unk)), leg_rows + length(unk),
      sprintf("column %s: %d rows carry a legacy stage label (Upstream / Midstream / Downstream); %d unknown stage labels", sc, leg_rows, length(unk)),
      c(qc_lab("legacy=", unique(parts[parts %in% legacy])), qc_lab("unknown=", unk)), needs_human_explanation = leg_rows > 0)
  }
  ics <- unique(c(qc_iso3_col(tbl), grep("(^|_)iso3c?$", names(tbl), value = TRUE)))
  if (length(ics)) {
    bad <- unlist(lapply(ics, function(ic) qc_lab(paste0(ic, "="), unique(tbl[[ic]][!is.na(tbl[[ic]]) & !qc_iso3_ok(tbl[[ic]])]))))
    out$iso <- qc_check("schema_theme_iso3_format", "schema", table, qc_status_if(length(bad)), length(bad),
                        sprintf("ISO3 codes are three upper-case letters (NA allowed for aggregates); columns %s", paste(ics, collapse = ", ")), bad)
  }
  key <- key %||% intersect(c("Country", "iso3", "iso3c", "tech", "supply_chain", "sub_sector", "category", "variable", "data_type",
                              "Year", "measure", "mineral", "year"), names(tbl))
  if (length(key) >= 3) {
    d <- duplicated(tbl[, key]) | duplicated(tbl[, key], fromLast = TRUE)
    out$key <- qc_check("schema_theme_key_unique", "schema", table, qc_status_if(sum(d)), sum(d),
                        paste("rows unique on", paste(key, collapse = ", ")), qc_examples(tbl[d, , drop = FALSE], key), needs_human_explanation = any(d))
  }
  qc_bind(out)
}

# --------------------------------------------------------------------------------------------------------------
# Missingness
# --------------------------------------------------------------------------------------------------------------

# Missing share of each decision input per tech x stage (only the stages where the input applies; excluded rows
# left out). Returns a long tibble {input, tech, supply_chain, n, n_missing, missing_share}.
qc_missing_profile <- function(tbl, inputs = QC_SP_MISSING_INPUTS) {
  keep <- is.na(qc_col(tbl, "posture_exclusion", NA_character_))
  tbl <- tbl[keep, , drop = FALSE]
  rows <- lapply(names(inputs), function(inp) {
    where <- inputs[[inp]]
    sub <- if (identical(where, "producing")) tbl[tbl$supply_chain != "Deployment", , drop = FALSE] else tbl[tbl$supply_chain %in% where, , drop = FALSE]
    if (!nrow(sub)) return(NULL)
    v <- qc_col(sub, inp, NA_real_)
    tibble::tibble(input = inp, tech = sub$tech, supply_chain = sub$supply_chain, miss = is.na(v)) |>
      dplyr::group_by(input, tech, supply_chain) |>
      dplyr::summarise(n = dplyr::n(), n_missing = sum(miss), .groups = "drop") |>
      dplyr::mutate(missing_share = n_missing / n)
  })
  dplyr::bind_rows(rows)
}

# Missingness of the decision inputs: per input, the tech x stage nodes where it is not measured for any economy
# (a structural gap: info, made explicit), the nodes where it is missing for more than max_missing_share of
# economies (warn), economies missing most of their inputs, and (with a previous table) sudden increases per
# input x tech x stage.
qc_missingness_sp <- function(tbl, previous = NULL, table = "strategic_position_tbl", inputs = QC_SP_MISSING_INPUTS,
                              max_missing_share = 0.5, country_gap_share = 0.8, jump = 0.10) {
  prof <- qc_missing_profile(tbl, inputs)
  out <- list()
  for (inp in unique(prof$input)) {
    p <- prof[prof$input == inp, ]
    all_m <- p[p$missing_share >= 1, ]
    hi <- p[p$missing_share > max_missing_share & p$missing_share < 1, ]
    hi <- hi[order(-hi$missing_share), ]
    out[[paste0("s_", inp)]] <- qc_check(paste0("missing_", inp, "_structural"), "missingness", table, "info", nrow(all_m),
      sprintf("%s: not measured for any economy in %d of %d tech x stage nodes (every decided cell there is Not assessed or decided without it)",
              inp, nrow(all_m), nrow(p)), qc_lab("", paste(all_m$tech, all_m$supply_chain, sep = "/")))
    out[[paste0("m_", inp)]] <- qc_check(paste0("missing_", inp), "missingness", table, if (nrow(hi)) "warn" else "info", nrow(hi),
      sprintf("%s: %d of %d tech x stage nodes miss it for more than %.0f%% (but not all) of economies; overall %.1f%% missing",
              inp, nrow(hi), nrow(p), 100 * max_missing_share, 100 * sum(p$n_missing) / max(1, sum(p$n))),
      qc_examples(hi, c("tech", "supply_chain", "n_missing", "n")))
  }
  keep <- is.na(qc_col(tbl, "posture_exclusion", NA_character_))
  t2 <- tbl[keep, , drop = FALSE]
  dep <- t2$supply_chain == "Deployment"
  slots <- 0
  missing <- 0
  for (inp in names(inputs)) {
    applies <- if (identical(inputs[[inp]], "producing")) !dep else t2$supply_chain %in% inputs[[inp]]
    v <- qc_col(t2, inp, NA_real_)
    slots <- slots + applies
    missing <- missing + (applies & is.na(v))
  }
  by_c <- tibble::tibble(iso3 = t2$iso3, slots = slots, missing = missing) |>
    dplyr::group_by(iso3) |> dplyr::summarise(share = sum(missing) / max(1, sum(slots)), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(share))
  gap <- by_c[by_c$share > country_gap_share, ]
  out$country <- qc_check("missing_country_gaps", "missingness", table, if (nrow(gap)) "warn" else "info", nrow(gap),
    sprintf("economies missing more than %.0f%% of their applicable decision inputs (they are mostly Not assessed)", 100 * country_gap_share),
    qc_examples(gap, c("iso3", "share")))
  na_choice <- sum(qc_col(t2, "strategic_choice", NA_character_) %in% "not_assessed")
  out$na <- qc_check("missing_not_assessed_cells", "missingness", table, "info", na_choice,
                     sprintf("%d of %d decided cells are Not assessed (need, prize or deployment need missing)", na_choice, nrow(t2)))
  if (!is.null(previous)) {
    pp <- qc_missing_profile(previous, inputs)
    j <- dplyr::inner_join(prof, pp, by = c("input", "tech", "supply_chain"), suffix = c("", "_prev")) |>
      dplyr::mutate(increase = missing_share - missing_share_prev) |>
      dplyr::filter(increase > jump) |> dplyr::arrange(dplyr::desc(increase))
    out$jump <- qc_check("missing_increase_vs_previous", "missingness", table, if (nrow(j)) "warn" else "info", nrow(j),
      sprintf("input x tech x stage nodes whose missing share rose by more than %.0f points since the previous table", 100 * jump),
      qc_examples(j, c("input", "tech", "supply_chain", "missing_share_prev", "missing_share")), needs_human_explanation = nrow(j) > 0)
  }
  qc_bind(out)
}

# Missingness of a tidy theme table: NA values, and indicator x tech x stage cells observed for few economies.
qc_missingness_theme <- function(tbl, table, min_country_share = 0.25) {
  tbl <- as.data.frame(dplyr::ungroup(tbl))
  if (!"value" %in% names(tbl)) return(qc_empty())
  ic <- qc_iso3_col(tbl) %||% "Country"
  by <- intersect(c("variable", "measure", "tech", "supply_chain"), names(tbl))
  na_v <- sum(is.na(tbl$value))
  out <- list(qc_check("missing_theme_na_values", "missingness", table, "info", na_v,
                       sprintf("%d of %d rows have an NA value", na_v, nrow(tbl))))
  n_all <- length(unique(tbl[[ic]]))
  if (length(by) && n_all > 0) {
    g <- tibble::tibble(id = tbl[[ic]], ok = !is.na(tbl$value), tbl[, by, drop = FALSE]) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
      dplyr::summarise(n_countries = length(unique(id[ok])), .groups = "drop") |>
      dplyr::mutate(country_share = n_countries / n_all) |>
      dplyr::filter(country_share < min_country_share) |> dplyr::arrange(country_share)
    out$cov <- qc_check("missing_theme_sparse_cells", "missingness", table, "info", nrow(g),
      sprintf("indicator x tech x stage cells observed for fewer than %.0f%% of the table's %d economies", 100 * min_country_share, n_all),
      qc_examples(g, c(by, "n_countries")))
  }
  qc_bind(out)
}

# --------------------------------------------------------------------------------------------------------------
# Numeric validity
# --------------------------------------------------------------------------------------------------------------

# Values of x that are non-missing strings and do not parse as numbers (malformed numeric strings).
qc_malformed_numeric <- function(x) {
  if (is.numeric(x) || is.logical(x)) return(character())
  x <- as.character(x)
  s <- trimws(x)
  bad <- !is.na(s) & nzchar(s) & !toupper(s) %in% c("NA", "NAN", "NULL") & is.na(suppressWarnings(as.numeric(s)))
  unique(x[bad])
}

# Numeric validity of a wide table: Inf / NaN, malformed numeric strings, 0-1 columns outside [0, 1], HHI outside
# (0, 1], negatives where impossible, ratio division-by-zero signatures, and world shares summing over a node
# to more than 1 + sum_tol (attributed to the row basis where world_share_cols names a basis column).
# fail_inputs: columns whose violation is a fail (decision inputs); others warn.
qc_numeric_validity <- function(tbl, table, unit_cols = QC_SP_UNIT_COLS, hhi_cols = QC_SP_HHI_COLS, nonneg_cols = QC_SP_NONNEG_COLS,
                                ratio_cols = QC_SP_RATIO_COLS, world_share_cols = QC_SP_WORLD_SHARE_COLS, node_keys = c("tech", "supply_chain"),
                                key = QC_SP_KEY, tol = 1e-6, sum_tol = 0.02,
                                fail_inputs = c("supply_chain_need", "strategic_industrial_value", "deployment_need", "cp_contender_pct")) {
  out <- list()
  key <- intersect(key, names(tbl))
  ex <- function(idx, cn) qc_examples(tbl[idx, , drop = FALSE], c(key, cn))
  st <- function(n, cn) if (n == 0) "info" else if (cn %in% fail_inputs) "fail" else "warn"
  num_cols <- names(tbl)[vapply(tbl, is.numeric, logical(1))]
  inf <- vapply(num_cols, function(cn) sum(is.infinite(tbl[[cn]]) | is.nan(tbl[[cn]])), integer(1))
  bad <- names(inf)[inf > 0]
  out$inf <- qc_check("numeric_inf_nan", "numeric_validity", table, if (length(bad)) (if (any(bad %in% fail_inputs)) "fail" else "warn") else "info",
                      sum(inf), sprintf("Inf or NaN values in %d numeric columns", length(num_cols)), qc_lab("", bad, paste0(": ", inf[bad])))
  chr_expected <- intersect(c(unit_cols, hhi_cols, nonneg_cols), names(tbl))
  chr_expected <- chr_expected[!vapply(chr_expected, function(cn) is.numeric(tbl[[cn]]) || is.logical(tbl[[cn]]), logical(1))]
  mal <- unlist(lapply(chr_expected, function(cn) qc_lab(paste0(cn, "="), qc_malformed_numeric(tbl[[cn]]))))
  out$mal <- qc_check("numeric_malformed_strings", "numeric_validity", table, qc_status_if(length(mal)), length(mal),
                      sprintf("numeric columns stored as text with values that do not parse (%d columns stored as text)", length(chr_expected)), mal)
  as_num <- function(cn) suppressWarnings(as.numeric(tbl[[cn]]))
  n_unit <- 0L
  for (cn in intersect(unit_cols, names(tbl))) {
    v <- as_num(cn)
    i <- which(!is.na(v) & is.finite(v) & (v < -tol | v > 1 + tol))
    if (length(i)) {
      n_unit <- n_unit + 1L
      out[[paste0("u_", cn)]] <- qc_check(paste0("numeric_out_of_unit_", cn), "numeric_validity", table, st(length(i), cn),
                                          length(i), sprintf("%s outside [0, 1]", cn), ex(i, cn))
    }
  }
  if (!n_unit) out$u_ok <- qc_check("numeric_out_of_unit", "numeric_validity", table, "info", 0L,
                                     sprintf("%d share / score / percentile columns all within [0, 1]", length(intersect(unit_cols, names(tbl)))))
  for (cn in intersect(hhi_cols, names(tbl))) {
    v <- as_num(cn)
    i <- which(!is.na(v) & is.finite(v) & (v <= 0 | v > 1 + tol))
    zero <- sum(!is.na(v) & v == 0)
    out[[paste0("h_", cn)]] <- qc_check(paste0("numeric_hhi_range_", cn), "numeric_validity", table, qc_status_if(length(i)), length(i),
      sprintf("%s outside (0, 1]: %d exactly 0 (a no-imports signature stored as a number: a concentration of 0 reads as perfectly diversified, it should be NA)",
              cn, zero), ex(i, c(cn, "top_supplier_iso3", "top_supplier_share", "imports_usd")))
  }
  for (cn in intersect(nonneg_cols, names(tbl))) {
    v <- as_num(cn)
    i <- which(!is.na(v) & v < -tol)
    if (length(i)) out[[paste0("n_", cn)]] <- qc_check(paste0("numeric_negative_", cn), "numeric_validity", table, st(length(i), cn),
                                                     length(i), sprintf("%s negative where it cannot be", cn), ex(i, cn))
  }
  for (cn in intersect(ratio_cols, names(tbl))) {
    v <- as_num(cn)
    i <- which(is.infinite(v) | is.nan(v))
    out[[paste0("r_", cn)]] <- qc_check(paste0("numeric_division_by_zero_", cn), "numeric_validity", table, qc_status_if(length(i)), length(i),
                                        sprintf("%s Inf or NaN (a ratio over a zero denominator)", cn), ex(i, cn))
  }
  # RCA exactly 0 while the economy exports the basket: a zero numerator from a different basis than the share
  if (all(c("trade_benchmark_rca", "trade_benchmark_exports") %in% names(tbl))) {
    i <- which(tbl$trade_benchmark_rca %in% 0 & !is.na(tbl$trade_benchmark_exports) & tbl$trade_benchmark_exports > 0)
    out$rca0 <- qc_check("numeric_rca_zero_with_exports", "numeric_validity", table, qc_status_if(length(i)), length(i),
                         "RCA exactly 0 although benchmark exports are positive", ex(i, c("trade_benchmark_rca", "trade_benchmark_exports")))
  }
  nk <- intersect(node_keys, names(tbl))
  if (length(nk) == length(node_keys)) {
    for (cn in intersect(names(world_share_cols), names(tbl))) {
      bc <- world_share_cols[[cn]]
      basis <- if (!is.na(bc) && bc %in% names(tbl)) as.character(tbl[[bc]]) else rep("all", nrow(tbl))
      d <- tibble::tibble(tbl[, nk, drop = FALSE], v = as_num(cn), basis = dplyr::coalesce(basis, "(none)"))
      s <- d |> dplyr::group_by(dplyr::across(dplyr::all_of(nk))) |>
        dplyr::summarise(total = sum(v, na.rm = TRUE), n = sum(!is.na(v)),
                         by_basis = paste(sprintf("%s %.3f (%d)", sort(unique(basis[!is.na(v)])),
                                                  vapply(sort(unique(basis[!is.na(v)])), function(b) sum(v[basis == b], na.rm = TRUE), numeric(1)),
                                                  vapply(sort(unique(basis[!is.na(v)])), function(b) sum(!is.na(v[basis == b])), integer(1))),
                                          collapse = " + "), .groups = "drop") |>
        dplyr::filter(total > 1 + sum_tol) |> dplyr::arrange(dplyr::desc(total))
      out[[paste0("s_", cn)]] <- qc_check(paste0("numeric_share_sum_", cn), "numeric_validity", table, qc_status_if(nrow(s)), nrow(s),
        sprintf("%s summed over the economies of a node exceeds 1 + %.2f (double counting, or shares on different bases stacked)%s", cn, sum_tol,
                if (!is.na(bc)) paste0("; per-basis sums from ", bc, " show which basis adds the excess") else ""),
        qc_examples(s, c(nk, "total", "n", "by_basis")), needs_human_explanation = nrow(s) > 0)
    }
  }
  qc_bind(out)
}

# Numeric validity of a tidy theme table: value numeric (malformed strings if not), Inf/NaN, index rows outside
# [0, 1], negative raw values for variables that cannot be negative (nonneg_pattern, minus exempt_pattern), and
# world shares (major-supplier style tables: world_share by node and measure) summing to more than 1 + tol.
qc_numeric_theme <- function(tbl, table, nonneg_pattern = "share|production|capacity|export|import|reserve|size|supply|capex|usd",
                             exempt_pattern = "surplus|deficit|growth|change|momentum|net_export|balance|gap", sum_tol = 0.02) {
  tbl <- as.data.frame(dplyr::ungroup(tbl))
  out <- list()
  if (!"value" %in% names(tbl)) return(qc_empty())
  mal <- qc_malformed_numeric(tbl$value)
  out$mal <- qc_check("numeric_theme_malformed_strings", "numeric_validity", table, qc_status_if(length(mal)), length(mal),
                      if (is.numeric(tbl$value)) "value is numeric" else "value stored as text; values that do not parse", mal)
  v <- suppressWarnings(as.numeric(tbl$value))
  keys <- intersect(c(qc_iso3_col(tbl) %||% "Country", "tech", "supply_chain", "variable", "measure", "data_type"), names(tbl))
  i <- which(is.infinite(v) | is.nan(v))
  by_var <- if (length(i)) { vv <- if ("variable" %in% names(tbl)) tbl$variable[i] else qc_col(tbl, "measure", "")[i]; tt <- sort(table(vv), decreasing = TRUE); paste(names(tt), tt, sep = ": ", collapse = "; ") } else ""
  out$inf <- qc_check("numeric_theme_inf_nan", "numeric_validity", table, qc_status_if(length(i)), length(i),
                      if (length(i)) paste("Inf or NaN values (a division by zero upstream); by variable:", by_var) else "Inf or NaN values",
                      qc_examples(tbl[i, , drop = FALSE], c(keys, "value")), needs_human_explanation = length(i) > 0)
  if ("data_type" %in% names(tbl)) {
    i <- which(tbl$data_type %in% "index" & !is.na(v) & is.finite(v) & (v < -1e-9 | v > 1 + 1e-9))
    out$idx <- qc_check("numeric_theme_index_out_of_unit", "numeric_validity", table, qc_status_if(length(i)), length(i),
                        "index rows outside [0, 1]", qc_examples(tbl[i, , drop = FALSE], c(keys, "value")))
  }
  vn <- if ("variable" %in% names(tbl)) tbl$variable else qc_col(tbl, "measure", "")
  raw <- if ("data_type" %in% names(tbl)) tbl$data_type %in% "raw" else rep(TRUE, nrow(tbl))
  i <- which(raw & grepl(nonneg_pattern, vn, ignore.case = TRUE) & !grepl(exempt_pattern, vn, ignore.case = TRUE) & !is.na(v) & v < -1e-9)
  out$neg <- qc_check("numeric_theme_negative", "numeric_validity", table, qc_status_if(length(i)), length(i),
                      "negative raw values for quantities that cannot be negative", qc_examples(tbl[i, , drop = FALSE], c(keys, "value")))
  if (all(c("world_share", "measure", "tech", "supply_chain") %in% names(tbl))) {
    agg <- if ("is_aggregate" %in% names(tbl)) tbl$is_aggregate %in% TRUE else rep(FALSE, nrow(tbl))
    by <- intersect(c("tech", "supply_chain", "measure", "mineral", "year"), names(tbl))
    s <- tibble::tibble(tbl[!agg, by, drop = FALSE], w = tbl$world_share[!agg]) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
      dplyr::summarise(total = sum(w, na.rm = TRUE), n = sum(!is.na(w)), .groups = "drop") |>
      dplyr::filter(total > 1 + sum_tol) |> dplyr::arrange(dplyr::desc(total))
    out$sum <- qc_check("numeric_theme_share_sum", "numeric_validity", table, qc_status_if(nrow(s)), nrow(s),
                        sprintf("world_share summed over economies (aggregates left out) exceeds 1 + %.2f", sum_tol),
                        qc_examples(s, c(by, "total", "n")), needs_human_explanation = nrow(s) > 0)
  }
  qc_bind(out)
}

# Units: one unit per measure within its comparison group (by: measure, plus mineral / tech where present: oil in
# kb/d and gas in EJ under one measure is fine, two units for one measure x mineral x tech cannot be summed or
# ranked).
qc_units <- function(tbl, table, measure_col = "measure", unit_col = "unit", by = c("mineral", "tech")) {
  if (!all(c(measure_col, unit_col) %in% names(tbl))) return(qc_empty())
  tbl <- as.data.frame(dplyr::ungroup(tbl))
  g <- c(measure_col, intersect(by, names(tbl)))
  u <- unique(tbl[!is.na(tbl[[unit_col]]), c(g, unit_col), drop = FALSE])
  grp <- do.call(paste, c(u[g], sep = " / "))
  multi <- names(which(table(grp) > 1))
  ex <- vapply(multi, function(m) paste0(m, ": ", paste(sort(unique(u[[unit_col]][grp == m])), collapse = " | ")), character(1))
  qc_check("trade_units_consistent", "trade_consistency", table, qc_status_if(length(multi)), length(multi),
           sprintf("%s groups carrying more than one unit string", paste(g, collapse = " x ")), unname(ex), needs_human_explanation = length(multi) > 0)
}

# --------------------------------------------------------------------------------------------------------------
# Trade consistency
# --------------------------------------------------------------------------------------------------------------

# Bilateral trade rows (current_trade_dyads_tbl shape: reporter_iso, partner_iso, flow_direction export/import,
# cmd_code, primary_value, period, window_end, months_used): duplicate dyad rows; negative values; self dyads;
# months_used; and mirror vs reported: A's reported exports to B against B's reported imports from A, compared
# only where both declarations cover the same 12-month window (same window_end, months_used 12) and both reach
# min_value. CIF/FOB alone explains ~10%, so a ratio outside mirror_band is a reporting discrepancy. warn when
# more than max_off_value_share of the matched value is off.
qc_trade_dyads <- function(dyads, table = "current_trade_dyads_tbl", mirror_band = c(0.5, 2), min_value = 1e5,
                           max_off_value_share = 0.25) {
  out <- list()
  dyads <- as.data.frame(dyads)
  need <- c("reporter_iso", "partner_iso", "flow_direction", "cmd_code", "primary_value")
  if (!all(need %in% names(dyads))) {
    return(qc_check("trade_dyads_schema", "trade_consistency", table, "warn", 1L,
                    paste("dyad table lacks", paste(setdiff(need, names(dyads)), collapse = ", "))))
  }
  key <- intersect(c("reporter_iso", "partner_iso", "flow_direction", "cmd_code", "period", "window_end"), names(dyads))
  d <- duplicated(dyads[, key])
  out$dup <- qc_check("trade_duplicate_dyads", "trade_consistency", table, qc_status_if(sum(d), TRUE), sum(d),
                      paste("duplicate rows on", paste(key, collapse = ", "), "(each would be counted twice)"),
                      qc_examples(dyads[d, , drop = FALSE], c(key, "primary_value")))
  neg <- which(!is.na(dyads$primary_value) & dyads$primary_value < 0)
  out$neg <- qc_check("trade_negative_values", "trade_consistency", table, qc_status_if(length(neg), TRUE), length(neg),
                      "negative trade values", qc_examples(dyads[neg, , drop = FALSE], c(key, "primary_value")))
  self <- which(dyads$reporter_iso == dyads$partner_iso)
  out$self <- qc_check("trade_self_dyads", "trade_consistency", table, qc_status_if(length(self)), length(self),
                       "rows where reporter and partner are the same economy", qc_examples(dyads[self, , drop = FALSE], key))
  full <- rep(TRUE, nrow(dyads))
  if ("months_used" %in% names(dyads)) {
    m <- which(!is.na(dyads$months_used) & (dyads$months_used < 1 | dyads$months_used > 12))
    part <- !is.na(dyads$months_used) & dyads$months_used < 12 & dyads$months_used >= 1
    full <- dyads$months_used %in% 12
    out$months <- qc_check("trade_months_used", "trade_consistency", table, qc_status_if(length(m)), length(m),
                           sprintf("months_used outside 1-12; %d rows cover fewer than 12 months (a partial window, left out of the mirror comparison)", sum(part)),
                           qc_examples(dyads[m, , drop = FALSE], c(key, "months_used")))
  }
  win <- if ("window_end" %in% names(dyads)) as.character(dyads$window_end) else rep("", nrow(dyads))
  flow_sum <- function(dir) {
    k <- dyads$flow_direction == dir & !is.na(dyads$primary_value) & full
    tibble::tibble(reporter_iso = dyads$reporter_iso[k], partner_iso = dyads$partner_iso[k], cmd_code = dyads$cmd_code[k],
                   window = win[k], v = dyads$primary_value[k]) |>
      dplyr::group_by(reporter_iso, partner_iso, cmd_code, window) |>
      dplyr::summarise(v = sum(v), .groups = "drop")
  }
  ex <- flow_sum("export")
  names(ex)[names(ex) == "v"] <- "reported_export"
  im <- flow_sum("import")
  names(im) <- c("partner_iso", "reporter_iso", "cmd_code", "window", "mirror_import")
  j <- as.data.frame(dplyr::inner_join(ex, im, by = c("reporter_iso", "partner_iso", "cmd_code", "window")))
  j <- j[j$reported_export >= min_value & j$mirror_import >= min_value, , drop = FALSE]
  if (nrow(j)) {
    j$ratio <- j$mirror_import / j$reported_export
    off <- j[j$ratio < mirror_band[1] | j$ratio > mirror_band[2], , drop = FALSE]
    off <- off[order(-abs(log(off$ratio)) * log1p(pmax(off$reported_export, off$mirror_import))), , drop = FALSE]
    wshare <- sum(pmax(off$reported_export, off$mirror_import)) / sum(pmax(j$reported_export, j$mirror_import))
    out$mirror <- qc_check("trade_mirror_vs_reported", "trade_consistency", table,
      if (wshare > max_off_value_share) "warn" else "info", nrow(off),
      sprintf("%d of %d exporter-reported dyad x code flows (same 12-month window, both sides >= %s) differ from the partner's mirror imports by more than x%.1f; median mirror/reported %.3f; %.1f%% of matched value",
              nrow(off), nrow(j), format(min_value, scientific = FALSE), mirror_band[2], stats::median(j$ratio), 100 * wshare),
      qc_examples(off, c("reporter_iso", "partner_iso", "cmd_code", "window", "reported_export", "mirror_import", "ratio")),
      needs_human_explanation = wshare > max_off_value_share)
  } else {
    out$mirror <- qc_check("trade_mirror_vs_reported", "trade_consistency", table, "info", 0L,
                           "no dyad has both a reported export and a mirror import on the same full window")
  }
  qc_bind(out)
}

# Importer supplier shares reconcile within a cell: the top supplier's share is the largest single-supplier share,
# so China's share cannot exceed it, and where the top supplier is China the two agree. By construction they need
# not: china_share sums the China group (import_exposure.R SP_CHINA_ISO3: CHN, HKG, MAC) while top_supplier_share
# is one exporter, and the T12M blend (current_trade.R ctm_blend_exposure) moves both shares but keeps the base
# year's top_supplier_iso3. The check makes the gap visible, because the Explorer shows both numbers side by side
# ("China X%", "top supplier China Y%"). Rows are split by exposure_basis (base / blend / none) so a period mix can
# be told from the group definition. coercive_top_share is a world supply share (supply_structure.R), not an
# import share, and is not compared here.
qc_importer_shares <- function(tbl, table = "strategic_position_tbl", tol = 0.005) {
  out <- list()
  key <- intersect(QC_SP_KEY, names(tbl))
  top <- qc_col(tbl, "top_supplier_share", NA_real_)
  basis <- dplyr::coalesce(as.character(qc_col(tbl, "exposure_basis", NA_character_)), "(none)")
  split_txt <- function(i) { if (!length(i)) return(""); tt <- table(basis[i]); paste0(" (exposure_basis: ", paste(names(tt), tt, sep = "=", collapse = ", "), ")") }
  cs <- qc_col(tbl, "china_share", NA_real_)
  i <- which(!is.na(top) & !is.na(cs) & cs > top + tol)
  out$cs <- qc_check("trade_shares_china_share_le_top", "trade_consistency", table, qc_status_if(length(i)), length(i),
                     paste0("china_share exceeds the top single supplier's share", split_txt(i),
                            ": china_share counts CHN + HKG + MAC (SP_CHINA_ISO3), top_supplier_share one exporter; on blend rows both are T12M blends but top_supplier_iso3 is the base year's"),
                     qc_examples(tbl[i, , drop = FALSE], c(key, "exposure_basis", "top_supplier_iso3", "top_supplier_share", "china_share")),
                     needs_human_explanation = length(i) > 0)
  ch <- which(qc_col(tbl, "top_supplier_iso3", NA_character_) %in% "CHN" & !is.na(top) & !is.na(cs) & abs(top - cs) > tol)
  out$chn <- qc_check("trade_shares_china_top_agree", "trade_consistency", table, qc_status_if(length(ch)), length(ch),
                      paste0("the top supplier is China but china_share differs from top_supplier_share by more than ", tol, split_txt(ch)),
                      qc_examples(tbl[ch, , drop = FALSE], c(key, "exposure_basis", "top_supplier_share", "china_share")), needs_human_explanation = length(ch) > 0)
  qc_bind(out)
}

# The HS6 basket (data/reference/energy_hs6_master.csv shape: tech, sub_sector, hs6, stage, excluded). Codes in
# more than one row or cell are counted in each by design (sp_trade_benchmark: "a code in several cells counts in
# each"); they are counted here so the double counting is visible. Exact duplicate rows are a defect. Known
# misfilings (a list of {hs6, sub_sector_pattern, note}) are flagged wherever the code sits in a matching row.
qc_hs6_crosswalk <- function(master, table = "energy_hs6_master",
                             known_misfilings = list(list(hs6 = "281290", sub_sector_pattern = "cabl|cable|connector",
                                                          note = "HS 2812.90 is NF3 / SF6 (non-metal halides), not cabling"))) {
  m <- as.data.frame(master)
  m$hs6 <- gsub(" ", "0", sprintf("%6s", as.character(m$hs6)))
  live <- if ("excluded" %in% names(m)) !(toupper(as.character(m$excluded)) %in% "TRUE") else rep(TRUE, nrow(m))
  stage_col <- if ("stage" %in% names(m)) "stage" else "supply_chain"
  out <- list()
  dk <- intersect(c("tech", "sub_sector", "hs6", stage_col), names(m))
  d <- duplicated(m[, dk])
  out$dup <- qc_check("hs6_duplicate_rows", "trade_consistency", table, qc_status_if(sum(d)), sum(d),
                      paste("exact duplicate rows on", paste(dk, collapse = ", "), "(the code would count twice in one cell)"),
                      qc_examples(m[d, , drop = FALSE], dk), needs_human_explanation = any(d))
  ml <- m[live, , drop = FALSE]
  rows_per <- table(ml$hs6)
  multi <- names(rows_per)[rows_per > 1]
  cells <- unique(ml[, c("hs6", "tech", stage_col)])
  cells_per <- table(cells$hs6)
  multi_cell <- names(cells_per)[cells_per > 1]
  multi_cell <- multi_cell[order(-cells_per[multi_cell])]
  out$multi <- qc_check("hs6_codes_in_several_cells", "trade_consistency", table, "info", length(multi_cell),
                        sprintf("%d of %d live codes appear in more than one row and %d in more than one tech x stage cell: counted in every cell by design (trade summed across cells double counts)",
                                length(multi), length(rows_per), length(multi_cell)),
                        qc_lab("", utils::head(multi_cell, 10), paste0(" x", cells_per[utils::head(multi_cell, 10)])))
  for (k in known_misfilings) {
    hit <- ml[ml$hs6 == k$hs6 & grepl(k$sub_sector_pattern, qc_col(ml, "sub_sector", ""), ignore.case = TRUE), , drop = FALSE]
    out[[paste0("mis_", k$hs6)]] <- qc_check(paste0("hs6_known_misfiling_", k$hs6), "trade_consistency", table, qc_status_if(nrow(hit)),
                                             nrow(hit), paste0(k$note, "; live basket rows filing it there: ", nrow(hit)),
                                             qc_examples(hit, c("tech", "sub_sector", "hs6", stage_col)), needs_human_explanation = nrow(hit) > 0)
  }
  qc_bind(out)
}

# --------------------------------------------------------------------------------------------------------------
# Classification logic
# --------------------------------------------------------------------------------------------------------------

# Re-apply the canonical choice rule to the stored inputs. Needs decision_engine.R (sp_choice_thresholds,
# sp_choice_inputs, sp_choice_high_cp, sp_choice_rule) and strategic_value.R (sp_ly_cfg, sp_siv_cfg) sourced by
# the caller. cfg: the decision_framework list. Adds qc_choice (the recomputed choice; NA for excluded rows),
# qc_cp_high and qc_reproduced (TRUE / FALSE; NA for excluded rows). Secure is the engine's (engine_posture), as
# in sp_decision_choice(): the override is not re-derived here.
qc_choice_recompute <- function(tbl, cfg = list()) {
  need_fns <- c("sp_choice_thresholds", "sp_choice_inputs", "sp_choice_high_cp", "sp_choice_rule")
  miss <- need_fns[!vapply(need_fns, exists, logical(1), mode = "function")]
  if (length(miss)) stop("qc_choice_recompute(): source R/strategic_position/decision_engine.R first (missing ", paste(miss, collapse = ", "), ")", call. = FALSE)
  th <- sp_choice_thresholds(cfg)
  eng <- qc_col(tbl, "engine_posture", NA_character_)
  excluded <- !is.na(qc_col(tbl, "posture_exclusion", NA_character_)) | is.na(eng)
  inp <- sp_choice_inputs(tbl, cfg)
  cp_high <- sp_choice_high_cp(qc_col(tbl, "cp_contender_pct", NA_real_), qc_col(tbl, "cp_contender_basis", NA_character_),
                               qc_col(tbl, "cp_is_contender", NA), qc_col(tbl, "t3_capability_lo", NA_character_), th$cp, th$floor)
  verb <- sp_choice_rule(tbl$supply_chain, eng %in% "secure", inp$need, inp$prize, cp_high, qc_col(tbl, "pathway_credible", NA_character_),
                         inp$dn, th, inp$large, inp$capped)
  verb[excluded] <- NA_character_
  tbl$qc_choice <- verb
  tbl$qc_cp_high <- cp_high
  stored <- qc_col(tbl, "strategic_choice", NA_character_)
  tbl$qc_reproduced <- ifelse(excluded & is.na(stored), NA, dplyr::coalesce(verb == stored, FALSE))
  tbl
}

# The reproduction check: every stored choice must come back from the rule applied to the stored inputs, and the
# stored Competitive Position among contenders must come back from sp_cp_contender_pct() on the stored scores and
# shares. An irreproducible choice is a fail (the shipped answer cannot be derived from the shipped evidence).
qc_choice_reproduction <- function(tbl, cfg = list(), table = "strategic_position_tbl") {
  r <- qc_choice_recompute(tbl, cfg)
  bad <- r[r$qc_reproduced %in% FALSE, , drop = FALSE]
  cols <- c("iso3", "tech", "supply_chain", "strategic_choice", "qc_choice", "supply_chain_need", "strategic_industrial_value",
            "deployment_need", "cp_contender_pct", "large_yard_case", "pathway_credible")
  n_chk <- sum(!is.na(r$qc_reproduced))
  out <- list(qc_check("class_choice_reproduced", "classification", table, qc_status_if(nrow(bad), TRUE), nrow(bad),
                       sprintf("%d of %d decided rows: the stored strategic_choice is not what sp_choice_rule gives on the stored inputs (the engine's Secure taken as stored)",
                               nrow(bad), n_chk), qc_examples(bad, cols), needs_human_explanation = nrow(bad) > 0))
  if ("cp_high" %in% names(r)) {
    dep <- r$supply_chain == "Deployment"
    m <- which(!dep & !is.na(r$cp_high) & !is.na(r$qc_choice) & r$cp_high != r$qc_cp_high)
    out$cp <- qc_check("class_cp_high_reproduced", "classification", table, qc_status_if(length(m), TRUE), length(m),
                       "stored cp_high differs from the rule's high Competitive Position on the stored inputs",
                       qc_examples(r[m, , drop = FALSE], c("iso3", "tech", "supply_chain", "cp_high", "qc_cp_high", "cp_contender_pct", "cp_contender_basis",
                                                           "cp_is_contender", "t3_capability_lo")))
  }
  if (nrow(bad)) {
    tr <- sort(table(paste(bad$strategic_choice, "->", bad$qc_choice)), decreasing = TRUE)
    out$tr <- qc_check("class_choice_irreproducible_transitions", "classification", table, "fail", nrow(bad),
                       "stored -> recomputed choice among irreproducible rows", paste0(names(tr), ": ", tr))
  }
  if (exists("sp_cp_contender_pct", mode = "function") && all(c("competitive_position_score", "competitive_position_percentile") %in% names(tbl))) {
    out$cpc <- qc_cp_contender_reproduction(tbl, cfg, table)
  }
  qc_bind(out)
}

# Competitive Position among contenders recomputed from the stored score, measured share and cross-country
# percentile (sp_cp_contender_pct). A difference that changes the high-position test of a decided producing cell
# is a fail; any other difference a warn.
qc_cp_contender_reproduction <- function(tbl, cfg = list(), table = "strategic_position_tbl", tol = 1e-9) {
  base <- tbl[, setdiff(names(tbl), c("cp_contender_pct", "cp_contender_basis", "cp_n_contenders", "cp_is_contender")), drop = FALSE]
  r <- sp_cp_contender_pct(base, cfg)
  old <- qc_col(tbl, "cp_contender_pct", NA_real_)
  d <- abs(r$cp_contender_pct - old)
  diff <- (is.na(r$cp_contender_pct) != is.na(old)) | (!is.na(d) & d > tol) |
    (dplyr::coalesce(r$cp_contender_basis, "") != dplyr::coalesce(as.character(qc_col(tbl, "cp_contender_basis", NA_character_)), ""))
  th <- if (exists("sp_choice_thresholds", mode = "function")) sp_choice_thresholds(cfg) else list(cp = 0.75, floor = c("moderate", "strong"))
  hi_old <- sp_choice_high_cp(old, qc_col(tbl, "cp_contender_basis", NA_character_), qc_col(tbl, "cp_is_contender", NA), qc_col(tbl, "t3_capability_lo", NA_character_), th$cp, th$floor)
  hi_new <- sp_choice_high_cp(r$cp_contender_pct, r$cp_contender_basis, r$cp_is_contender, qc_col(tbl, "t3_capability_lo", NA_character_), th$cp, th$floor)
  decided <- !is.na(qc_col(tbl, "strategic_choice", NA_character_)) & tbl$supply_chain != "Deployment"
  material <- which(diff & decided & hi_old != hi_new)
  x <- tbl[which(diff), c("iso3", "tech", "supply_chain"), drop = FALSE]
  x$stored <- old[which(diff)]
  x$recomputed <- r$cp_contender_pct[which(diff)]
  qc_check("class_cp_contender_reproduced", "classification", table,
           if (length(material)) "fail" else if (any(diff)) "warn" else "info", sum(diff),
           sprintf("%d rows: stored cp_contender_pct / basis not reproduced by sp_cp_contender_pct() on the stored scores and shares; %d of them change the high-position test of a decided cell",
                   sum(diff), length(material)), qc_examples(x, names(x)), needs_human_explanation = any(diff))
}

# Rows excluded from the decision (posture_exclusion, e.g. entrepots) that still sit in a node's contender reference
# set: their Competitive Position score shifts every other economy's cp_contender_pct although their own cell is
# never decided (e.g. Hong Kong through the Atlas fallback of the trade benchmark).
qc_excluded_contenders <- function(tbl, table = "strategic_position_tbl") {
  ex <- !is.na(qc_col(tbl, "posture_exclusion", NA_character_)) & qc_col(tbl, "cp_is_contender", NA) %in% TRUE &
    qc_col(tbl, "cp_contender_basis", NA_character_) %in% "contenders"
  x <- tbl[ex, , drop = FALSE]
  qc_check("class_excluded_rows_in_contender_set", "classification", table, qc_status_if(nrow(x)), nrow(x),
           sprintf("excluded rows (posture_exclusion) counted as contenders in %d nodes: they set other economies' cp_contender_pct", nrow(unique(x[, c("tech", "supply_chain"), drop = FALSE]))),
           qc_examples(x, c("iso3", "tech", "supply_chain", "posture_exclusion", "home_share_exposure", "competitive_position_score", "trade_benchmark_source")),
           needs_human_explanation = nrow(x) > 0)
}

# Posture invariants on the stored table (posture_check_invariants, R/dashboard/posture/posture_export.R, and the
# bundle contract): the posture is the choice's posture; Compete carries its mode alone, Deploy its deploy mode
# alone, Secure and not assessed neither; no Compete at Deployment; Accelerate only at Deployment; Not assessed
# exactly where a decision input is missing (never Watch for a cell without need or prize).
qc_posture_invariants_sp <- function(tbl, table = "strategic_position_tbl", verb_posture = SP_CHOICE_VERB_POSTURE) {
  ch <- qc_col(tbl, "strategic_choice", NA_character_)
  ok <- !is.na(ch)
  t <- tbl[ok, , drop = FALSE]
  ch <- ch[ok]
  post <- qc_col(t, "strategic_posture", NA_character_)
  m <- qc_col(t, "compete_mode", NA_character_)
  dm <- qc_col(t, "deploy_mode", NA_character_)
  dep <- t$supply_chain == "Deployment"
  secure <- ch == "secure"
  need_na <- is.na(qc_col(t, "supply_chain_need", NA_real_)) | is.na(qc_col(t, "strategic_industrial_value", NA_real_))
  dn_na <- is.na(qc_col(t, "deployment_need", NA_real_))
  missing_input <- ifelse(dep, dn_na, need_na)
  rules <- list(
    posture_matches_choice = post != unname(verb_posture[ch]) | is.na(post),
    compete_mode_alone = post %in% "compete" & (is.na(m) | m != ch | !is.na(dm)),
    deploy_mode_alone = post %in% "deploy" & (is.na(dm) | dm != ch | !is.na(m)),
    secure_or_na_without_mode = post %in% c("secure", "not_assessed") & (!is.na(m) | !is.na(dm)),
    no_compete_at_deployment = dep & post %in% "compete",
    accelerate_only_at_deployment = ch %in% "accelerate" & !dep,
    not_assessed_iff_input_missing = !secure & (ch %in% "not_assessed") != missing_input
  )
  out <- lapply(names(rules), function(nm) {
    i <- which(rules[[nm]] %in% TRUE)
    qc_check(paste0("class_invariant_", nm), "classification", table, qc_status_if(length(i), TRUE), length(i),
             paste("posture invariant:", gsub("_", " ", nm)),
             qc_examples(t[i, , drop = FALSE], c("iso3", "tech", "supply_chain", "strategic_choice", "strategic_posture", "compete_mode", "deploy_mode")))
  })
  qc_bind(out)
}

# Threshold boundaries: cells sitting exactly on a decision threshold (inclusive rule: high), within the borderline
# band, the prize at the market-only cap value, and nodes whose prize is the same number for every economy within
# the band of the threshold (the whole node flips together on one calibration choice). Knife-edges are warn with
# needs_human_explanation: the answer there rests on a calibration choice, not on the evidence. eps: the distance
# that counts as "exactly" (the bundle rounds to 3 decimals, so 5e-4 matches what a reader sees).
qc_threshold_boundaries <- function(tbl, cfg = list(), table = "strategic_position_tbl", eps = 1e-9) {
  th <- list(need = cfg$need_threshold %||% 0.67, prize = cfg$prize_threshold %||% 0.6, cp = cfg$cp_preserve_pct %||% 0.75,
             deployment = cfg$deployment_threshold %||% 0.6, band = cfg$borderline_band %||% 0.05)
  cap_margin <- cfg$strategic_value$cap_margin %||% 0.01
  ch_all <- qc_col(tbl, "strategic_choice", NA_character_)
  t <- tbl[!is.na(ch_all) & !ch_all %in% c("secure", "not_assessed"), , drop = FALSE]
  dep <- t$supply_chain == "Deployment"
  capped <- qc_col(t, "siv_cap", NA_character_) %in% "market_only"
  vals <- list(
    need = list(v = ifelse(dep, NA, qc_col(t, "supply_chain_need", NA_real_)), th = th$need, col = "supply_chain_need"),
    prize = list(v = ifelse(dep | capped, NA, qc_col(t, "strategic_industrial_value", NA_real_)), th = th$prize, col = "strategic_industrial_value"),
    cp = list(v = ifelse(dep, NA, qc_col(t, "cp_contender_pct", NA_real_)), th = th$cp, col = "cp_contender_pct"),
    deployment = list(v = ifelse(dep, qc_col(t, "deployment_need", NA_real_), NA), th = th$deployment, col = "deployment_need")
  )
  out <- list()
  for (nm in names(vals)) {
    v <- vals[[nm]]$v
    at <- which(!is.na(v) & abs(v - vals[[nm]]$th) <= eps)
    near <- sum(!is.na(v) & abs(v - vals[[nm]]$th) < th$band)
    tt <- if (length(at)) { z <- table(t$strategic_choice[at]); paste0(" (choices: ", paste(names(z), z, sep = "=", collapse = ", "), ")") } else ""
    out[[nm]] <- qc_check(paste0("class_exactly_on_threshold_", nm), "classification", table, qc_status_if(length(at)), length(at),
      sprintf("%s exactly at its threshold %.2f (inclusive rule: counts as high)%s; %d decided cells within the borderline band %.2f",
              vals[[nm]]$col, vals[[nm]]$th, tt, near, th$band),
      qc_examples(t[at, , drop = FALSE], c("iso3", "tech", "supply_chain", vals[[nm]]$col, "strategic_choice")), needs_human_explanation = length(at) > 0)
  }
  cap_v <- th$prize - cap_margin
  pz <- qc_col(t, "strategic_industrial_value", NA_real_)
  at_cap <- sum(capped & !dep & !is.na(pz) & abs(pz - cap_v) < 1e-6)
  out$cap <- qc_check("class_prize_at_market_only_cap", "classification", table, "info", at_cap,
                      sprintf("prize stored exactly at the market-only cap %.2f (siv_cap market_only: never a high prize, a rule not a measurement); %d capped producing cells in all",
                              cap_v, sum(capped & !dep)))
  prod <- t[!dep & !capped & !is.na(pz), , drop = FALSE]
  if (nrow(prod)) {
    nodes <- prod |> dplyr::group_by(tech, supply_chain) |>
      dplyr::summarise(n = dplyr::n(), n_distinct = length(unique(round(strategic_industrial_value, 6))),
                       prize = stats::median(strategic_industrial_value), .groups = "drop") |>
      dplyr::filter(n >= 5, n_distinct == 1, abs(prize - th$prize) < th$band)
    out$node <- qc_check("class_node_constant_prize_near_threshold", "classification", table, qc_status_if(nrow(nodes)), nrow(nodes),
      sprintf("nodes where every economy carries the same prize within %.2f of the %.2f threshold: the whole node flips together", th$band, th$prize),
      qc_examples(nodes, c("tech", "supply_chain", "prize", "n")), needs_human_explanation = nrow(nodes) > 0)
  }
  qc_bind(out)
}

# The need-floor cap (supply_chain_need.R: a need score below the floor has its percentile capped under the
# threshold and its band, supply_chain_need_cap "score_below_floor"). The rule reads the capped value, so any
# threshold perturbation below the stored cap value flips these cells for an artefactual reason: counted here,
# by choice, with the uncapped percentile, so the threshold sensitivity can be read correctly.
qc_need_cap <- function(tbl, cfg = list(), table = "strategic_position_tbl") {
  capped <- qc_col(tbl, "supply_chain_need_cap", NA_character_) %in% "score_below_floor"
  t <- tbl[capped, , drop = FALSE]
  if (!nrow(t)) return(qc_check("class_need_floor_capped", "classification", table, "info", 0L, "no need-floor-capped cells"))
  by_choice <- table(dplyr::coalesce(t$strategic_choice, "excluded"))
  cap_vals <- sort(unique(round(qc_col(t, "supply_chain_need", NA_real_), 4)))
  th <- cfg$need_threshold %||% 0.67
  qc_check("class_need_floor_capped", "classification", table, "info", nrow(t),
           sprintf("%d cells have supply_chain_need capped (score_below_floor); stored need values %s; choices %s. A need threshold at or below the stored cap value flips them although their score is below the floor (threshold sensitivity must read supply_chain_need_cap); %d have an uncapped percentile >= %.2f",
                   nrow(t), paste(cap_vals, collapse = ", "), paste(names(by_choice), by_choice, sep = "=", collapse = ", "),
                   sum(qc_col(t, "supply_chain_need_uncapped", NA_real_) >= th, na.rm = TRUE), th),
           qc_examples(t, c("iso3", "tech", "supply_chain", "supply_chain_need_score", "supply_chain_need_uncapped", "supply_chain_need", "strategic_choice")),
           needs_human_explanation = TRUE)
}

# --------------------------------------------------------------------------------------------------------------
# Provenance completeness
# --------------------------------------------------------------------------------------------------------------

# A licence string's class: restricted (no redistribution), non_commercial (NC), attribution (CC BY / cite), open
# (CC0 / public domain), or unknown.
qc_licence_class <- function(x) {
  x <- dplyr::coalesce(as.character(x), "")
  dplyr::case_when(
    !nzchar(trimws(x)) ~ NA_character_,
    grepl("restricted|proprietary|subscription|terms of use|not for redistribution|all rights reserved", x, ignore.case = TRUE) ~ "restricted",
    grepl("\\bNC\\b|BY-NC|non-?commercial", x, ignore.case = TRUE) ~ "non_commercial",
    grepl("CC0|public domain|^open$|open data|open licen", x, ignore.case = TRUE) ~ "open",
    grepl("CC[- ]?BY|attribution|cite|citing", x, ignore.case = TRUE) ~ "attribution",
    TRUE ~ "unknown"
  )
}

# Licence classes that allow commercial reuse with at most attribution.
QC_LICENCE_PERMISSIVE <- c("open", "attribution")
QC_LICENCE_LIMITED <- c("non_commercial", "restricted")

qc_norm_url <- function(u) {
  u <- tolower(trimws(dplyr::coalesce(as.character(u), "")))
  u <- sub("^https?://", "", u)
  u <- sub("^www\\.", "", u)
  sub("/+$", "", u)
}

# A field of a parsed YAML entry as one string (NA when absent or blank). [[ ]], never $: `$` partial-matches.
qc_field <- function(e, f) {
  v <- e[[f]]
  if (is.null(v) || !length(v) || (length(v) == 1 && (is.na(v) || !nzchar(trimws(as.character(v)))))) NA_character_ else paste(as.character(unlist(v)), collapse = "; ")
}

# Provenance completeness of config/raw_inputs_manifest.yml (manifest: its parsed `inputs` list) against
# config/sector_dictionary.yml (dict_sources: its parsed `sources` list). repo_files: repo-relative paths that
# exist (required_by mappings are checked against them); raw_files: paths that exist under the raw directory
# (optional; NULL skips the staged-file check); as_of: a Date; links: optional data frame {dict_id, manifest_id}
# added to the URL-based linking of dictionary sources to manifest entries.
qc_provenance <- function(manifest, dict_sources = list(), repo_files = NULL, raw_files = NULL, as_of = NULL, stale_years = 2L,
                          links = NULL, table = "raw_inputs_manifest") {
  m <- tibble::tibble(
    id = vapply(manifest, qc_field, character(1), f = "id"), source_type = vapply(manifest, qc_field, character(1), f = "source_type"),
    path = vapply(manifest, qc_field, character(1), f = "path"), url = vapply(manifest, qc_field, character(1), f = "url"),
    vintage = vapply(manifest, qc_field, character(1), f = "vintage"), licence = vapply(manifest, qc_field, character(1), f = "licence"),
    optional = vapply(manifest, function(e) isTRUE(e[["optional"]]), logical(1)),
    required_by = lapply(manifest, function(e) as.character(unlist(e[["required_by"]])))
  )
  ext <- m$source_type %in% c("api", "manual")
  out <- list()
  for (f in c("url", "vintage", "licence")) {
    miss_ext <- m$id[ext & is.na(m[[f]])]
    miss_int <- m$id[!ext & is.na(m[[f]])]
    out[[f]] <- qc_check(paste0("prov_missing_", f), "provenance", table, qc_status_if(length(miss_ext)), length(miss_ext),
                         sprintf("%d of %d external (api/manual) inputs have no %s (%d derived/generated inputs also lack it); filled for %d of %d entries",
                                 length(miss_ext), sum(ext), f, length(miss_int), sum(!is.na(m[[f]])), nrow(m)),
                         miss_ext, needs_human_explanation = length(miss_ext) > 0 && f == "licence")
  }
  yrs <- lapply(m$vintage, function(v) if (is.na(v)) integer() else as.integer(regmatches(v, gregexpr("(?<![0-9])(19|20)[0-9]{2}(?![0-9])", v, perl = TRUE))[[1]]))
  last <- vapply(yrs, function(y) if (length(y)) max(y) else NA_integer_, integer(1))
  if (!is.null(as_of)) {
    cut <- as.integer(format(as.Date(as_of), "%Y")) - as.integer(stale_years)
    st <- which(!is.na(last) & last < cut)
    out$stale <- qc_check("prov_stale_vintage", "provenance", table, qc_status_if(length(st)), length(st),
                          sprintf("inputs whose vintage names no year after %d (as of %s, %d years)", cut, format(as.Date(as_of)), stale_years),
                          qc_lab("", m$id[st], paste0(" (", m$vintage[st], ")")))
  }
  unp <- which(!is.na(m$vintage) & is.na(last))
  out$unparsed <- qc_check("prov_vintage_without_year", "provenance", table, "info", length(unp),
                           "vintages that name no year (free text)", qc_lab("", m$id[unp], paste0(" (", m$vintage[unp], ")")))
  if (!is.null(repo_files)) {
    rb <- qc_unnest_required_by(m)
    bad <- rb[!rb$required_by %in% repo_files, , drop = FALSE]
    out$rb <- qc_check("prov_required_by_missing", "provenance", table, qc_status_if(nrow(bad)), nrow(bad),
                       sprintf("required_by entries naming a script or R file that does not exist in the repo (%d mappings checked)", nrow(rb)),
                       qc_lab("", bad$id, paste0(" -> ", bad$required_by)), needs_human_explanation = nrow(bad) > 0)
  }
  if (!is.null(raw_files)) {
    staged <- !is.na(m$path) & !m$optional & m$source_type %in% c("api", "manual")
    bad <- m[staged & !m$path %in% raw_files, , drop = FALSE]
    out$raw <- qc_check("prov_staged_file_missing", "provenance", table, qc_status_if(nrow(bad)), nrow(bad),
                        "non-optional external inputs whose file is not staged in the raw directory", qc_lab("", bad$id, paste0(" (", bad$path, ")")))
  }
  out$lic <- qc_licence_conflicts(m, dict_sources, links)
  qc_bind(out)
}

qc_unnest_required_by <- function(m) {
  rows <- lapply(seq_len(nrow(m)), function(i) if (length(m$required_by[[i]])) tibble::tibble(id = m$id[i], required_by = m$required_by[[i]]))
  dplyr::bind_rows(tibble::tibble(id = character(), required_by = character()), rows)
}

# Licence conflicts: a sector-dictionary source and a raw-manifest entry for the same dataset (same normalised URL,
# or an explicit link) whose licences disagree on reuse (one permissive, the other non-commercial or restricted);
# and a dictionary source whose own licence field says attribution / open while its attribution or issues text
# names a non-commercial licence.
qc_licence_conflicts <- function(m, dict_sources = list(), links = NULL, table = "sector_dictionary_vs_raw_manifest") {
  if (!length(dict_sources)) return(qc_check("prov_licence_conflict", "provenance", table, "info", 0L, "no dictionary sources given"))
  d <- tibble::tibble(dict_id = vapply(dict_sources, qc_field, character(1), f = "id"), dict_url = vapply(dict_sources, qc_field, character(1), f = "url"),
                      dict_licence = vapply(dict_sources, qc_field, character(1), f = "licence"),
                      dict_text = paste(vapply(dict_sources, qc_field, character(1), f = "attribution"), vapply(dict_sources, qc_field, character(1), f = "issues")))
  d$dict_class <- qc_licence_class(d$dict_licence)
  mm <- tibble::tibble(manifest_id = m$id, man_url = m$url, man_licence = m$licence, man_class = qc_licence_class(m$licence))
  d$k <- qc_norm_url(d$dict_url)
  mm$k <- qc_norm_url(mm$man_url)
  pairs <- dplyr::inner_join(d[nzchar(d$k), ], mm[nzchar(mm$k), ], by = "k", relationship = "many-to-many")
  if (!is.null(links) && nrow(links)) {
    extra <- dplyr::inner_join(dplyr::inner_join(as.data.frame(links), d, by = "dict_id"), mm, by = "manifest_id", suffix = c("", ".m"))
    pairs <- dplyr::bind_rows(pairs, extra[, intersect(names(pairs), names(extra))])
  }
  pairs <- unique(pairs[, c("dict_id", "dict_licence", "dict_class", "manifest_id", "man_licence", "man_class")])
  conf <- pairs[(pairs$dict_class %in% QC_LICENCE_PERMISSIVE & pairs$man_class %in% QC_LICENCE_LIMITED) |
                  (pairs$man_class %in% QC_LICENCE_PERMISSIVE & pairs$dict_class %in% QC_LICENCE_LIMITED), ]
  self <- d[d$dict_class %in% QC_LICENCE_PERMISSIVE & qc_licence_class(d$dict_text) %in% QC_LICENCE_LIMITED, ]
  trunc <- function(s) ifelse(nchar(s) > 60, paste0(substr(s, 1, 57), "..."), s)
  qc_bind(
    qc_check("prov_licence_conflict", "provenance", table, qc_status_if(nrow(conf)), nrow(conf),
             sprintf("dictionary source and raw-manifest entry for the same dataset disagree on reuse (%d linked pairs checked)", nrow(pairs)),
             qc_lab("", conf$dict_id, paste0(" '", conf$dict_licence, "' vs ", conf$manifest_id, " '", trunc(conf$man_licence), "'")),
             needs_human_explanation = nrow(conf) > 0),
    qc_check("prov_licence_self_conflict", "provenance", "sector_dictionary", qc_status_if(nrow(self)), nrow(self),
             "dictionary sources whose licence field is permissive while their attribution / issues text names a non-commercial or restricted licence",
             qc_lab("", self$dict_id, paste0(" licence '", self$dict_licence, "'")), needs_human_explanation = nrow(self) > 0)
  )
}
