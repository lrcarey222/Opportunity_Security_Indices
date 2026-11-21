# Golden cases on the pipeline table (docs/QC_SYSTEM_DESIGN.md 4.5, 6.1): evaluate config/qc/golden_cases.yml
# (schema GoldenCase in dashboard/src/review/model.ts: id, category, kind, key, expect {choice | choice_in, fence,
# numeric ranges}, exact, change_requires_review, rationale, approved) against a strategic_position table (or the
# shipped posture core converted to one, qc_posture_core_to_table) and, for aib_node cases, an AIB node table.
#
# The cases are pinned to a shipped bundle release; the processed table may be a newer run, so a failure here is
# drift evidence (the answer moved since the case was approved) unless the same case also fails on the shipped
# bundle, in which case the case itself is out of date or wrong. Every result says which table it was evaluated
# against.
#
# Status per case: pass, fail, missing (the key is not in the table, the row is excluded, or nothing the case
# expects can be observed), not_applicable (a kind or scenario the table cannot answer: compact cases; aib_node
# cases without a node table or on a scenario other than the node table's).
#
# Pure functions: no IO. Needs dplyr, tibble and qc_core.R. The fence is read from
# R/strategic_position/yard_rollup.R (sp_yard_of_choice, SP_YARD_FENCE) when sourced.

# Golden fact id -> column of the standardised table (qc_sp_standardize / qc_posture_core_to_table).
QC_GOLDEN_STD_FACTS <- c(need = "need", deployment_need = "dn", prize = "prize", market = "market", cp = "cp", home_share = "share")
# Golden fact id -> column of the raw strategic_position table, for facts the standard columns lack.
QC_GOLDEN_FACT_COLS <- c(
  import_hhi = "import_hhi", china_share = "china_share", top_supplier_share = "top_supplier_share",
  opposite_bloc_share = "opposite_bloc_share", trusted_share = "trusted_world_share", coercive_top_share = "coercive_top_share",
  imports_to_gdp = "imports_to_gdp", benchmark_share = "trade_benchmark_share", benchmark_rca = "trade_benchmark_rca",
  benchmark_exports = "trade_benchmark_exports", import_dependence = "sn_import_dependence",
  trusted_alternatives = "trusted_alternatives_share"
)
# AIB fact id -> column of the node table (outputs/aib/aib_node_classification.csv, renamed by the caller to
# node_id / class plus these).
QC_GOLDEN_AIB_FACTS <- c(exposure = "shock_exposure", static = "static_coverage", existing = "existing_coverage",
                         optimized = "optimized_coverage", dividend = "coordination_dividend", residual = "residual_gap",
                         after_largest_share = "optimized_largest_share", n1_reoptimized = "optimized_n1_reoptimized_coverage",
                         diversion = "optimized_diversion")

# The case list from a parsed YAML: a top-level list of cases, or a map holding one under cases / golden_cases.
qc_golden_cases <- function(parsed) {
  if (is.null(parsed)) return(list())
  if (!is.null(parsed[["cases"]])) return(parsed[["cases"]])
  if (!is.null(parsed[["golden_cases"]])) return(parsed[["golden_cases"]])
  if (!is.null(names(parsed)) && "id" %in% names(parsed)) return(list(parsed))
  parsed
}

qc_golden_fence <- function(choice, stage) {
  if (!exists("sp_yard_of_choice", mode = "function") || !exists("SP_YARD_FENCE")) return(rep(NA_character_, length(choice)))
  y <- sp_yard_of_choice(choice, stage)
  unname(SP_YARD_FENCE[y])
}

# Evaluate the cases. std: a standardised table (qc_sp_standardize(table) or qc_posture_core_to_table(core)); raw:
# the strategic_position table itself (numeric facts outside the standard columns; optional); aib_nodes: {node_id,
# class, + QC_GOLDEN_AIB_FACTS columns} (optional) for the scenario aib_scenario; table_label names what the cases
# ran against. tol: the slack on each numeric range. The cases were authored from the shipped bundle, which rounds
# scores to 3 decimals, so a range pinned at [0.467, 0.467] means "displays as 0.467": the default 5e-4 accepts
# exactly the unrounded values that display that way (an unrounded 0.4665 is not a change).
qc_golden_eval <- function(cases, std, raw = NULL, aib_nodes = NULL, table_label = "strategic_position_tbl",
                           aib_scenario = "decoupling", aib_label = "aib_node_classification", tol = 5e-4) {
  cases <- qc_golden_cases(cases)
  key_s <- function(k) paste(vapply(names(k), function(n) paste0(n, "=", paste(unlist(k[[n]]), collapse = ",")), character(1)), collapse = "; ")
  rng_s <- function(r) sprintf("[%s, %s]", format(unlist(r)[1]), format(unlist(r)[2]))
  fmtv <- function(v) ifelse(is.na(v), "NA", format(signif(v, 4)))
  rows <- lapply(cases, function(cs) {
    e <- cs[["expect"]] %||% list()
    kind <- cs[["kind"]] %||% "posture_cell"
    k <- cs[["key"]] %||% list()
    e_choice <- e[["choice"]]
    e_in <- unlist(e[["choice_in"]])
    e_num <- e[["numeric"]] %||% list()
    exp_parts <- c(if (!is.null(e_choice)) paste0("choice=", e_choice), if (length(e_in)) paste0("choice_in=", paste(e_in, collapse = "|")),
                   if (!is.null(e[["fence"]])) paste0("fence=", e[["fence"]]),
                   if (length(e_num)) paste0(names(e_num), "=", vapply(e_num, rng_s, character(1))))
    against <- if (kind == "aib_node") aib_label else table_label
    res <- function(status, observed = character(), failures = character(), note = "") {
      tibble::tibble(id = cs[["id"]] %||% NA_character_, category = cs[["category"]] %||% NA_character_, kind = kind, key = key_s(k),
                     status = status, expected = paste(exp_parts, collapse = "; "), observed = paste(observed, collapse = "; "),
                     failures = paste(failures, collapse = "; "), note = note, exact = isTRUE(cs[["exact"]]),
                     change_requires_review = isTRUE(cs[["change_requires_review"]]),
                     approved_release = as.character(cs[["approved"]][["release"]] %||% NA_character_), evaluated_against = against)
    }
    allowed_of <- function() if (isTRUE(cs[["exact"]]) && !is.null(e_choice)) e_choice else unique(c(e_choice, e_in))
    check_num <- function(getv, obs, fails, unobs) {
      evaluated <- 0L
      for (fid in names(e_num)) {
        rg <- as.numeric(unlist(e_num[[fid]]))
        v <- getv(fid)
        if (is.null(v)) { unobs <- c(unobs, fid); next }
        obs <- c(obs, sprintf("%s=%s", fid, fmtv(v)))
        evaluated <- evaluated + 1L
        if (is.na(v) || v < rg[1] - tol - 1e-12 || v > rg[2] + tol + 1e-12) fails <- c(fails, sprintf("%s %s outside [%s, %s]", fid, fmtv(v), rg[1], rg[2]))
      }
      list(obs = obs, fails = fails, unobs = unobs, evaluated = evaluated)
    }
    if (kind == "aib_node") {
      if (is.null(aib_nodes)) return(res("not_applicable", note = "no AIB node table given"))
      sc <- k[["scenario"]] %||% aib_scenario
      if (!identical(sc, aib_scenario)) return(res("not_applicable", note = paste0("scenario ", sc, "; the node table holds ", aib_scenario)))
      hit <- aib_nodes[aib_nodes$node_id %in% k[["node"]], , drop = FALSE]
      if (!nrow(hit)) return(res("missing", note = paste("node not in the AIB table:", k[["node"]] %||% "")))
      norm <- function(x) toupper(gsub("_", " ", trimws(as.character(x))))
      cls <- hit$class[1]
      obs <- paste0("class=", cls)
      fails <- character()
      evaluated <- 0L
      allowed <- norm(allowed_of())
      if (length(allowed)) {
        evaluated <- evaluated + 1L
        if (!norm(cls) %in% allowed) fails <- c(fails, paste0("class ", cls, " not in ", paste(allowed, collapse = "|")))
      }
      getv <- function(fid) {
        cn <- if (!is.na(QC_GOLDEN_AIB_FACTS[fid])) QC_GOLDEN_AIB_FACTS[[fid]] else fid
        if (cn %in% names(hit)) suppressWarnings(as.numeric(hit[[cn]][1])) else NULL
      }
      nr <- check_num(getv, obs, fails, character())
      evaluated <- evaluated + nr$evaluated
      note <- if (length(nr$unobs)) paste("not observable in this table:", paste(nr$unobs, collapse = ", ")) else ""
      if (!evaluated) return(res("missing", nr$obs, note = paste("nothing the case expects is observable;", note)))
      return(res(if (length(nr$fails)) "fail" else "pass", nr$obs, nr$fails, note))
    }
    if (kind != "posture_cell") return(res("not_applicable", note = paste("kind", kind, "is not evaluated on the pipeline table")))
    r <- std[std$iso3 %in% k[["iso3"]] & std$tech %in% k[["tech"]] & std$supply_chain %in% k[["stage"]], , drop = FALSE]
    if (!nrow(r)) return(res("missing", note = "key not in the table (iso3, tech display name, stage)"))
    r <- r[1, , drop = FALSE]
    if (is.na(r$choice)) return(res("missing", note = "row excluded from the decision (posture_exclusion)"))
    raw_r <- if (!is.null(raw)) raw[raw$iso3 %in% k[["iso3"]] & raw$tech %in% k[["tech"]] & raw$supply_chain %in% k[["stage"]], , drop = FALSE] else NULL
    obs <- paste0("choice=", r$choice)
    fails <- character()
    evaluated <- 0L
    allowed <- allowed_of()
    if (length(allowed)) {
      evaluated <- evaluated + 1L
      if (!r$choice %in% allowed) fails <- c(fails, sprintf("choice %s not in %s", r$choice, paste(allowed, collapse = "|")))
    }
    if (!is.null(e[["fence"]])) {
      f <- qc_golden_fence(r$choice, r$supply_chain)
      obs <- c(obs, paste0("fence=", f))
      if (!is.na(f)) {
        evaluated <- evaluated + 1L
        if (f != e[["fence"]]) fails <- c(fails, sprintf("fence %s, expected %s", f, e[["fence"]]))
      }
    }
    getv <- function(fid) {
      if (!is.na(QC_GOLDEN_STD_FACTS[fid]) && QC_GOLDEN_STD_FACTS[[fid]] %in% names(r)) return(as.numeric(r[[QC_GOLDEN_STD_FACTS[[fid]]]][1]))
      if (!is.null(raw_r) && nrow(raw_r)) {
        cn <- if (!is.na(QC_GOLDEN_FACT_COLS[fid])) QC_GOLDEN_FACT_COLS[[fid]] else fid
        if (cn %in% names(raw_r)) return(suppressWarnings(as.numeric(raw_r[[cn]][1])))
      }
      NULL
    }
    nr <- check_num(getv, obs, fails, character())
    evaluated <- evaluated + nr$evaluated
    note <- if (length(nr$unobs)) paste("not observable in this table:", paste(nr$unobs, collapse = ", ")) else ""
    if (!evaluated) return(res("missing", nr$obs, note = paste("nothing the case expects is observable;", note)))
    res(if (length(nr$fails)) "fail" else "pass", nr$obs, nr$fails, note)
  })
  out <- dplyr::bind_rows(rows)
  if (!nrow(out)) {
    return(tibble::tibble(id = character(), category = character(), kind = character(), key = character(), status = character(),
                          expected = character(), observed = character(), failures = character(), note = character(), exact = logical(),
                          change_requires_review = logical(), approved_release = character(), evaluated_against = character()))
  }
  out
}

# Check rows for the golden evaluation. A failure on another table than the pinned release is drift evidence against
# the approved (bundle-pinned) case: warn with needs_human_explanation, never fail here (the shipped-bundle layer gates CI).
# res may hold several evaluations (evaluated_against); one row set per evaluation.
qc_golden_checks <- function(res, table = "golden_cases") {
  if (!nrow(res)) return(qc_check("golden_cases", "golden", table, "info", 0L, "no golden cases evaluated (config/qc/golden_cases.yml absent or empty)"))
  out <- list()
  for (ag in unique(res$evaluated_against)) {
    r <- res[res$evaluated_against == ag, , drop = FALSE]
    cnt <- table(factor(r$status, levels = c("pass", "fail", "missing", "not_applicable")))
    f <- r[r$status == "fail", , drop = FALSE]
    m <- r[r$status == "missing", , drop = FALSE]
    tb <- paste(table, "vs", ag)
    out[[paste0("f_", ag)]] <- qc_check("golden_cases_fail", "golden", tb, qc_status_if(nrow(f)), nrow(f),
      sprintf("golden cases: %s. A fail on a table other than the release the case was pinned to is drift evidence between the two (release_alignment says which is newer); a fail on the pinned release means the case is out of date or wrong",
              paste(names(cnt), cnt, sep = "=", collapse = ", ")),
      qc_lab("", f$id, paste0(" ", f$key, ": ", f$failures)), needs_human_explanation = nrow(f) > 0)
    out[[paste0("m_", ag)]] <- qc_check("golden_cases_missing", "golden", tb, qc_status_if(nrow(m)), nrow(m),
      "golden cases that cannot be evaluated on this table", qc_lab("", m$id, paste0(" ", m$key, ": ", m$note)))
  }
  qc_bind(out)
}
