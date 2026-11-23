# Release drift (docs/QC_SYSTEM_DESIGN.md 6.2 "version drift"): compare two releases of the strategic position
# results, either two strategic_position tables or a table against the shipped posture bundle converted to a
# table (qc_posture_core_to_table), and, when given, two AIB node classifications and compact memberships.
#
# Reported: the classification transitions matrix, every cell whose choice changed (with the inputs that moved
# and the thresholds they crossed), large input moves, score distribution shifts (quantiles), rank changes per
# tech x stage (Spearman), AIB class and compact membership changes. Drift is never 'fail': a large change is
# warn with needs_human_explanation, a review list for a person, not a verdict.
#
# Pure functions: no IO. Needs dplyr, tibble and qc_core.R.

QC_DRIFT_KEY <- c("iso3", "tech", "supply_chain")
QC_DRIFT_INPUTS <- c("need", "dn", "prize", "market", "cp", "share")

# A strategic_position table in the drift's standard columns: iso3, tech, supply_chain, choice (NA for an excluded
# row), need (supply_chain_need, producing stages only), dn (deployment_need, Deployment only), prize (the prize the
# rule read), market (industrial_opportunity), cp (cp_contender_pct), share (measured share), borderline.
qc_sp_standardize <- function(tbl) {
  dep <- tbl$supply_chain == "Deployment"
  prize <- if ("choice_prize" %in% names(tbl)) dplyr::coalesce(tbl$choice_prize, ifelse(dep, NA_real_, qc_col(tbl, "strategic_industrial_value", NA_real_)))
           else ifelse(dep, NA_real_, qc_col(tbl, "strategic_industrial_value", NA_real_))
  tibble::tibble(
    iso3 = tbl$iso3, tech = tbl$tech, supply_chain = tbl$supply_chain,
    choice = qc_col(tbl, "strategic_choice", NA_character_),
    need = ifelse(dep, NA_real_, qc_col(tbl, "supply_chain_need", NA_real_)),
    dn = ifelse(dep, qc_col(tbl, "deployment_need", NA_real_), NA_real_),
    prize = prize,
    market = ifelse(dep, NA_real_, qc_col(tbl, "industrial_opportunity", NA_real_)),
    cp = ifelse(dep, NA_real_, qc_col(tbl, "cp_contender_pct", NA_real_)),
    share = dplyr::coalesce(qc_col(tbl, "home_share_exposure", NA_real_), qc_col(tbl, "home_share", NA_real_)),
    borderline = qc_col(tbl, "decision_borderline", NA_character_)
  )
}

# The shipped posture core (posture-core.*.json, parsed with jsonlite) as a table in the standard columns. The
# core is columnar: cells.c / t / s index countries.iso3 / meta.techs / meta.stages; p is the posture code (S, C,
# D, N), m the compete mode, dm the deploy mode; need, prize, io (market), cp, share are rounded to 3 decimals.
# cells.need holds the stage's relevant need (dashboard/src/posture/contract.ts needTypeOfStage): deployment need at
# Deployment, supply-chain need elsewhere; it is split into need / dn here as in qc_sp_standardize().
qc_posture_core_to_table <- function(core) {
  cells <- core$cells
  col <- function(nm) {
    x <- cells[[nm]]
    if (is.null(x)) return(rep(NA, length(cells$p)))
    if (is.list(x)) x <- vapply(x, function(z) if (is.null(z) || !length(z)) NA_character_ else as.character(z), character(1))
    x
  }
  num <- function(nm) suppressWarnings(as.numeric(col(nm)))
  iso <- as.character(unlist(core$countries$iso3))
  techs <- as.character(unlist(core$meta$techs))
  stages <- as.character(unlist(core$meta$stages))
  p <- as.character(col("p"))
  m <- as.character(col("m"))
  dm <- as.character(col("dm"))
  choice <- dplyr::case_when(p == "S" ~ "secure", p == "C" ~ m, p == "D" ~ dm, p == "N" ~ "not_assessed", TRUE ~ NA_character_)
  stage <- stages[as.integer(col("s")) + 1L]
  tibble::tibble(
    iso3 = iso[as.integer(col("c")) + 1L], tech = techs[as.integer(col("t")) + 1L], supply_chain = stage,
    choice = choice, need = ifelse(stage == "Deployment", NA_real_, num("need")), dn = ifelse(stage == "Deployment", num("need"), NA_real_),
    prize = ifelse(stage == "Deployment", NA_real_, num("prize")),
    market = ifelse(stage == "Deployment", NA_real_, num("io")), cp = ifelse(stage == "Deployment", NA_real_, num("cp")),
    share = num("share"), borderline = as.character(col("bd"))
  )
}

# Full join of two standardised releases. presence: both | only_prev | only_cur.
qc_drift_join <- function(prev, cur) {
  j <- dplyr::full_join(prev, cur, by = QC_DRIFT_KEY, suffix = c("_prev", "_cur"))
  inp <- QC_DRIFT_INPUTS[paste0(QC_DRIFT_INPUTS, "_prev") %in% names(j)]
  in_prev <- paste(j$iso3, j$tech, j$supply_chain) %in% paste(prev$iso3, prev$tech, prev$supply_chain)
  in_cur <- paste(j$iso3, j$tech, j$supply_chain) %in% paste(cur$iso3, cur$tech, cur$supply_chain)
  j$presence <- ifelse(in_prev & in_cur, "both", ifelse(in_prev, "only_prev", "only_cur"))
  for (v in inp) j[[paste0("d_", v)]] <- j[[paste0(v, "_cur")]] - j[[paste0(v, "_prev")]]
  j
}

# A cell present in one release only whose present side is excluded from the decision (choice NA): the shipped
# posture core leaves excluded rows (posture_exclusion, e.g. entrepots) out, so their absence there is expected.
qc_drift_expected_absence <- function(j) {
  (j$presence == "only_cur" & is.na(j$choice_cur)) | (j$presence == "only_prev" & is.na(j$choice_prev))
}

# The transitions matrix (long): from, to, n. An excluded or absent cell reads "(excluded)" / "(absent)".
qc_drift_transitions <- function(j) {
  lab <- function(ch, present) ifelse(!present, "(absent)", ifelse(is.na(ch), "(excluded)", ch))
  from <- lab(j$choice_prev, j$presence != "only_cur")
  to <- lab(j$choice_cur, j$presence != "only_prev")
  # an excluded row on one side and absent on the other is the shipped core omitting excluded rows: not a change
  tibble::tibble(from = from, to = to) |> dplyr::count(from, to, name = "n") |>
    dplyr::mutate(expected_absence = (from == "(absent)" & to == "(excluded)") | (from == "(excluded)" & to == "(absent)"),
                  changed = from != to & !expected_absence) |>
    dplyr::arrange(dplyr::desc(changed), dplyr::desc(n))
}

# Cells whose choice changed, or with an input move larger than `large`: the inputs that moved by more than tol and
# the decision thresholds they crossed (need: need_threshold; dn: deployment_threshold; prize; cp).
qc_drift_cells <- function(j, thresholds = list(need = 0.67, prize = 0.6, cp = 0.75, deployment = 0.6), tol = 0.001, large = 0.15) {
  inp <- QC_DRIFT_INPUTS[paste0("d_", QC_DRIFT_INPUTS) %in% names(j)]
  dmat <- as.matrix(j[, paste0("d_", inp), drop = FALSE])
  maxd <- apply(abs(dmat), 1, function(r) if (all(is.na(r))) NA_real_ else max(r, na.rm = TRUE))
  changed <- j$presence == "both" & dplyr::coalesce(j$choice_prev, "(excluded)") != dplyr::coalesce(j$choice_cur, "(excluded)")
  big <- !is.na(maxd) & maxd > large
  keep <- changed | big | (j$presence != "both" & !qc_drift_expected_absence(j))
  x <- j[keep, , drop = FALSE]
  dm <- dmat[keep, , drop = FALSE]
  moved <- apply(dm, 1, function(r) { h <- inp[!is.na(r) & abs(r) > tol]; if (length(h)) paste(h, collapse = ";") else NA_character_ })
  th_of <- function(v) switch(v, need = thresholds$need, dn = thresholds$deployment, prize = thresholds$prize, cp = thresholds$cp, NA_real_)
  crossed <- vapply(seq_len(nrow(x)), function(i) {
    h <- character()
    for (v in intersect(c("need", "dn", "prize", "cp"), inp)) {
      a <- x[[paste0(v, "_prev")]][i]
      b <- x[[paste0(v, "_cur")]][i]
      t <- th_of(v)
      if (!is.na(a) && !is.na(b) && !is.na(t) && ((a >= t) != (b >= t))) h <- c(h, sprintf("%s %.3f->%.3f (th %.2f)", v, a, b, t))
      if (xor(is.na(a), is.na(b))) h <- c(h, sprintf("%s %s->%s", v, ifelse(is.na(a), "NA", sprintf("%.3f", a)), ifelse(is.na(b), "NA", sprintf("%.3f", b))))
    }
    if (length(h)) paste(h, collapse = "; ") else NA_character_
  }, character(1))
  out <- tibble::tibble(x[, c(QC_DRIFT_KEY, "presence", "choice_prev", "choice_cur")],
                        choice_changed = changed[keep], max_abs_delta = maxd[keep], large_move = big[keep],
                        moved_inputs = moved, thresholds_crossed = crossed)
  for (v in inp) {
    out[[paste0(v, "_prev")]] <- x[[paste0(v, "_prev")]]
    out[[paste0(v, "_cur")]] <- x[[paste0(v, "_cur")]]
    out[[paste0("d_", v)]] <- x[[paste0("d_", v)]]
  }
  out$unexplained <- out$choice_changed & is.na(out$thresholds_crossed)
  out$status <- "warn"
  out$needs_human_explanation <- out$choice_changed | out$large_move
  out[order(!out$choice_changed, -dplyr::coalesce(out$max_abs_delta, 0)), ]
}

# Quantile shifts of each input between releases (cells present in both).
qc_drift_distribution <- function(j, probs = c(0.05, 0.25, 0.5, 0.75, 0.95)) {
  inp <- QC_DRIFT_INPUTS[paste0(QC_DRIFT_INPUTS, "_prev") %in% names(j)]
  b <- j[j$presence == "both", ]
  dplyr::bind_rows(lapply(inp, function(v) {
    q <- function(x) stats::quantile(x, probs, na.rm = TRUE, names = FALSE)
    a <- b[[paste0(v, "_prev")]]
    c2 <- b[[paste0(v, "_cur")]]
    if (all(is.na(a)) || all(is.na(c2))) return(NULL)
    tibble::tibble(input = v, stat = c(paste0("p", 100 * probs), "mean", "n"),
                   prev = c(q(a), mean(a, na.rm = TRUE), sum(!is.na(a))), cur = c(q(c2), mean(c2, na.rm = TRUE), sum(!is.na(c2)))) |>
      dplyr::mutate(delta = cur - prev)
  }))
}

# Spearman rank correlation of an input between releases per tech x stage (economies with both values, n >= min_n).
qc_drift_ranks <- function(j, inputs = c("cp", "need", "dn", "prize"), min_n = 5) {
  b <- j[j$presence == "both", ]
  dplyr::bind_rows(lapply(intersect(inputs, QC_DRIFT_INPUTS[paste0(QC_DRIFT_INPUTS, "_prev") %in% names(j)]), function(v) {
    b |> dplyr::group_by(tech, supply_chain) |>
      dplyr::summarise(input = v, n = sum(!is.na(.data[[paste0(v, "_prev")]]) & !is.na(.data[[paste0(v, "_cur")]])),
                       spearman = {
                         a <- .data[[paste0(v, "_prev")]]; c2 <- .data[[paste0(v, "_cur")]]; ok <- !is.na(a) & !is.na(c2)
                         if (sum(ok) >= min_n && stats::sd(a[ok]) > 0 && stats::sd(c2[ok]) > 0) suppressWarnings(stats::cor(a[ok], c2[ok], method = "spearman")) else NA_real_
                       }, .groups = "drop")
  }))
}

# Split a member / node list column: a list of character vectors, or strings separated by spaces, ; , or |.
qc_drift_split <- function(m) {
  if (is.list(m)) return(lapply(m, function(z) as.character(unlist(z))))
  lapply(strsplit(dplyr::coalesce(as.character(m), ""), "[ ;,|]+"), function(z) z[nzchar(z)])
}

# AIB drift: node class changes ({node_id, class} each) and compact changes. Compacts ({compact_id, nodes,
# members}: nodes and members as list columns or delimited strings) are matched on their node set, not their ID:
# AIB-C## IDs are positional and are reassigned when the compact list changes, so the same ID can name different
# compacts in two runs. Returns nodes {node_id, class_prev, class_cur, changed} and compacts {compact_key,
# compact_id_prev, compact_id_cur, presence, members_prev, members_cur, joined, left, changed}, plus id_reused: the
# IDs that name a different node set in the two releases.
qc_drift_aib <- function(prev_nodes, cur_nodes, prev_compacts = NULL, cur_compacts = NULL) {
  n <- dplyr::full_join(prev_nodes[, c("node_id", "class")], cur_nodes[, c("node_id", "class")], by = "node_id", suffix = c("_prev", "_cur"))
  n$changed <- dplyr::coalesce(n$class_prev, "(absent)") != dplyr::coalesce(n$class_cur, "(absent)")
  comp <- NULL
  reused <- character()
  if (!is.null(prev_compacts) && !is.null(cur_compacts)) {
    keyed <- function(cc) {
      nodes <- qc_drift_split(cc$nodes)
      tibble::tibble(compact_key = vapply(nodes, function(z) paste(sort(unique(z)), collapse = " "), character(1)),
                     compact_id = as.character(cc$compact_id),
                     members = vapply(qc_drift_split(cc$members), function(z) paste(sort(unique(z)), collapse = " "), character(1)))
    }
    p <- keyed(prev_compacts)
    c2 <- keyed(cur_compacts)
    comp <- dplyr::full_join(p, c2, by = "compact_key", suffix = c("_prev", "_cur"), relationship = "many-to-many")
    comp$presence <- ifelse(is.na(comp$compact_id_prev), "only_cur", ifelse(is.na(comp$compact_id_cur), "only_prev", "both"))
    sp_m <- function(s) strsplit(dplyr::coalesce(s, ""), " ", fixed = TRUE)
    mp <- sp_m(comp$members_prev)
    mc <- sp_m(comp$members_cur)
    comp$joined <- vapply(seq_along(mp), function(i) paste(setdiff(mc[[i]], mp[[i]]), collapse = " "), character(1))
    comp$left <- vapply(seq_along(mp), function(i) paste(setdiff(mp[[i]], mc[[i]]), collapse = " "), character(1))
    comp$changed <- comp$presence != "both" | dplyr::coalesce(comp$members_prev, "") != dplyr::coalesce(comp$members_cur, "")
    both_ids <- intersect(p$compact_id, c2$compact_id)
    reused <- both_ids[vapply(both_ids, function(i) p$compact_key[p$compact_id == i][1] != c2$compact_key[c2$compact_id == i][1], logical(1))]
  }
  list(nodes = n, compacts = comp, id_reused = reused)
}

# Check rows for a drift comparison. label names the comparison (e.g. "bundle 2026-10-31 -> pipeline 2026-11-17").
qc_drift_checks <- function(j, cells, dist = NULL, ranks = NULL, aib = NULL, label = "previous -> current", min_rho = 0.8, large = 0.15,
                            aib_label = label) {
  tbl <- paste("drift:", label)
  b <- j[j$presence == "both", ]
  chg <- cells[cells$choice_changed %in% TRUE, ]
  tr <- if (nrow(chg)) { x <- table(paste(chg$choice_prev, "->", chg$choice_cur)); x <- sort(x, decreasing = TRUE); paste0(names(x), ": ", x) } else character()
  expected <- qc_drift_expected_absence(j)
  odd <- j$presence != "both" & !expected
  out <- list(
    qc_check("drift_choice_changes", "drift", tbl, qc_status_if(nrow(chg)), nrow(chg),
             sprintf("%d of %d cells present in both releases change choice; %d of them with no input crossing its threshold (moved by a gate: large-yard, pathway, contender or capability)",
                     nrow(chg), nrow(b), sum(chg$unexplained %in% TRUE)), tr, needs_human_explanation = nrow(chg) > 0),
    qc_check("drift_presence", "drift", tbl, qc_status_if(sum(odd)), sum(odd),
             sprintf("%d decided cells only in the previous release, %d only in the current; %d more present on one side only as excluded rows (posture_exclusion: expected, the shipped core omits them)",
                     sum(odd & j$presence == "only_prev"), sum(odd & j$presence == "only_cur"), sum(expected)),
             unique(qc_lab("", j$iso3[odd], paste0(" (", j$presence[odd], ")"))), needs_human_explanation = any(odd))
  )
  dcols <- intersect(paste0("d_", QC_DRIFT_INPUTS), names(b))
  if (length(dcols) && nrow(b)) {
    mx <- vapply(dcols, function(cn) { v <- abs(b[[cn]]); if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE) }, numeric(1))
    na_flip <- vapply(sub("^d_", "", dcols), function(v) sum(xor(is.na(b[[paste0(v, "_prev")]]), is.na(b[[paste0(v, "_cur")]]))), integer(1))
    out$maxd <- qc_check("drift_max_input_delta", "drift", tbl, "info", sum(na_flip),
                         "largest absolute change of each input over cells in both releases, and cells where the input is measured on one side only (n)",
                         sprintf("%s: max |d| %s, measured on one side only %d", sub("^d_", "", dcols), ifelse(is.na(mx), "NA", format(signif(mx, 3))), na_flip))
  }
  big <- cells[cells$large_move %in% TRUE, ]
  out$big <- qc_check("drift_large_input_moves", "drift", tbl, qc_status_if(nrow(big)), nrow(big),
                      sprintf("cells with an input moving by more than %.2f", large),
                      qc_examples(big[order(-big$max_abs_delta), ], c("iso3", "tech", "supply_chain", "moved_inputs", "max_abs_delta", "choice_prev", "choice_cur")),
                      needs_human_explanation = nrow(big) > 0)
  if (!is.null(dist) && nrow(dist)) {
    med <- dist[dist$stat == "p50", ]
    out$dist <- qc_check("drift_distribution_shift", "drift", tbl, "info", nrow(med), "median of each input, previous -> current",
                         sprintf("%s: %.3f -> %.3f", med$input, med$prev, med$cur))
  }
  if (!is.null(ranks) && nrow(ranks)) {
    low <- ranks[!is.na(ranks$spearman) & ranks$spearman < min_rho, ]
    low <- low[order(low$spearman), ]
    out$rank <- qc_check("drift_rank_changes", "drift", tbl, qc_status_if(nrow(low)), nrow(low),
                         sprintf("input x tech x stage rankings with Spearman below %.2f between releases (%d rankings compared)", min_rho, sum(!is.na(ranks$spearman))),
                         qc_examples(low, c("input", "tech", "supply_chain", "spearman", "n")), needs_human_explanation = nrow(low) > 0)
  }
  if (!is.null(aib)) out$aib <- qc_drift_aib_checks(aib, aib_label)
  qc_bind(out)
}

# Check rows for an AIB drift comparison (qc_drift_aib()).
qc_drift_aib_checks <- function(aib, label = "previous -> current") {
  tbl <- paste("drift: AIB", label)
  nc <- aib$nodes[aib$nodes$changed, , drop = FALSE]
  out <- list(qc_check("drift_aib_class_changes", "drift", tbl, qc_status_if(nrow(nc)), nrow(nc),
                       sprintf("%d of %d AIB nodes change class (headline scenario)", nrow(nc), nrow(aib$nodes)),
                       qc_lab("", nc$node_id, paste0(": ", dplyr::coalesce(nc$class_prev, "(absent)"), " -> ", dplyr::coalesce(nc$class_cur, "(absent)"))),
                       needs_human_explanation = nrow(nc) > 0))
  if (!is.null(aib$compacts)) {
    cc <- aib$compacts
    both <- cc[cc$presence == "both", , drop = FALSE]
    mem <- both[both$changed, , drop = FALSE]
    gone <- cc[cc$presence != "both", , drop = FALSE]
    out$comp <- qc_check("drift_compact_membership_changes", "drift", tbl, qc_status_if(nrow(mem)), nrow(mem),
                         sprintf("%d of %d compacts with the same node set in both releases change their minimum coalition", nrow(mem), nrow(both)),
                         qc_lab("", mem$compact_id_cur, paste0(" (was ", mem$compact_id_prev, "): +[", mem$joined, "] -[", mem$left, "]")),
                         needs_human_explanation = nrow(mem) > 0)
    out$set <- qc_check("drift_compact_set_changes", "drift", tbl, qc_status_if(nrow(gone)), nrow(gone),
                        sprintf("%d compact node sets only in the previous release, %d only in the current (compacts formed, dissolved or re-scoped)",
                                sum(gone$presence == "only_prev"), sum(gone$presence == "only_cur")),
                        qc_lab("", ifelse(gone$presence == "only_prev", gone$compact_id_prev, gone$compact_id_cur), paste0(" (", gone$presence, "): ", gone$compact_key)),
                        needs_human_explanation = nrow(gone) > 0)
    reused <- aib$id_reused %||% character()
    out$ids <- qc_check("drift_compact_id_reused", "drift", tbl, qc_status_if(length(reused)), length(reused),
                        "compact IDs (AIB-C##) that name a different node set in the two releases: an ID is not a stable reference across runs",
                        reused, needs_human_explanation = length(reused) > 0)
  }
  qc_bind(out)
}

# Release alignment: can each shipped bundle be traced to the processed outputs being checked? x: one row per
# component {component, bundle_created, pipeline_time (both POSIXct, UTC), reproduced (TRUE / FALSE / NA: whether
# the comparison above found the bundle's results in the pipeline outputs), detail}. A pipeline output older than
# the bundle cannot be the bundle's source (the bundle was exported from another run, so it is not reproducible
# from this processed dir): warn with needs_human_explanation. A newer output that does not reproduce the bundle
# is drift (warn); a newer one that does is info.
qc_release_alignment <- function(x, table = "release_alignment") {
  if (is.null(x) || !nrow(x)) return(qc_empty())
  fmt <- function(t) ifelse(is.na(t), "NA", format(t, "%Y-%m-%d %H:%M UTC", tz = "UTC"))
  rows <- lapply(seq_len(nrow(x)), function(i) {
    b <- x$bundle_created[i]
    p <- x$pipeline_time[i]
    rep_ok <- x$reproduced[i]
    older <- !is.na(b) && !is.na(p) && p < b
    st <- if (older || (rep_ok %in% FALSE)) "warn" else "info"
    what <- if (is.na(b) || is.na(p)) "a timestamp is missing: alignment not checkable"
            else if (older) "the processed output predates the shipped bundle: the bundle was exported from another run and cannot be traced to this processed dir"
            else if (rep_ok %in% TRUE) "the processed output is newer than the bundle and reproduces its results"
            else if (rep_ok %in% FALSE) "the processed output is newer than the bundle and does not reproduce it (drift)"
            else "the processed output is newer than the bundle; reproduction not compared"
    qc_check(paste0("release_alignment_", x$component[i]), "provenance", table, st, as.integer(st == "warn"),
             sprintf("%s: bundle created %s, processed output %s; %s%s", x$component[i], fmt(b), fmt(p), what,
                     if (!is.null(x$detail) && !is.na(x$detail[i]) && nzchar(x$detail[i])) paste0(" (", x$detail[i], ")") else ""),
             needs_human_explanation = st == "warn")
  })
  qc_bind(rows)
}
