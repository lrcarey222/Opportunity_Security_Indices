# Plain-English explanation of a deterministic change record (docs/change-layer-architecture.md). A template
# filled from the structured fields: it never adds a fact the record does not hold. A later model pass may rephrase
# it, but the structured record stays the source of truth.
#
# Pure functions: no IO.

change_label <- function(cfg, group, x) {
  lab <- cfg$labels[[group]] %||% list()
  vapply(x, function(v) if (is.na(v)) "-" else (lab[[v]] %||% gsub("_", " ", v)), character(1), USE.NAMES = FALSE)
}

change_fmt <- function(field, v) {
  if (is.na(v)) return("n/a")
  if (field == "share") return(sprintf("%.1f%%", 100 * v))
  sprintf("%.2f", v)
}

# x: classified rows (posture_classify_cells output, meaningful rows) with attribution columns. One string per row.
posture_change_explain <- function(x, cfg) {
  bars_note <- function(cr, f) {
    if (is.na(cr)) return("")
    m <- regmatches(cr, regexpr(paste0(f, " [^;]*\\(bar ([0-9.]+)\\)"), cr))
    if (!length(m)) return("")
    sprintf(", crossing the %s bar", sub(".*\\(bar ([0-9.]+)\\).*", "\\1", m))
  }
  vapply(seq_len(nrow(x)), function(i) {
    r <- x[i, , drop = FALSE]
    s <- character()
    if (isTRUE(r$t_recommendation)) {
      a <- change_label(cfg, "choice", dplyr::coalesce(r$choice_prev, "not_assessed"))
      b <- change_label(cfg, "choice", dplyr::coalesce(r$choice_cur, "not_assessed"))
      s <- c(s, if (identical(r$subtype, "coverage") && b == change_label(cfg, "choice", "not_assessed")) sprintf("No longer assessed (was %s).", a)
                else if (identical(r$subtype, "coverage")) sprintf("Newly assessed: %s.", b)
                else sprintf("Recommendation changed from %s to %s.", a, b))
    }
    if (isTRUE(r$t_score)) {
      for (f in cfg$thresholds$score$fields) {
        a <- r[[paste0(f, "_prev")]]
        b <- r[[paste0(f, "_cur")]]
        d <- r[[paste0("d_", f)]]
        cr <- bars_note(r$crossed, f)
        big <- !is.na(d) && abs(d) >= cfg$thresholds$score$min_delta - 1e-9
        if (big || nzchar(cr) || xor(is.na(a), is.na(b))) {
          lab <- change_label(cfg, "field", f)
          if (f == "need" && identical(r$need_type_cur, "deployment")) lab <- "Deployment need"
          verb <- if (is.na(d)) "moved" else if (d > 0) "rose" else "fell"
          s <- c(s, sprintf("%s %s %s → %s%s.", lab, verb, change_fmt(f, a), change_fmt(f, b), cr))
        }
      }
    }
    if (isTRUE(r$t_data)) {
      for (f in names(cfg$thresholds$data)) {
        a <- r[[paste0(f, "_prev")]]
        b <- r[[paste0(f, "_cur")]]
        if (!is.na(a) && !is.na(b) && abs(b - a) > 0.001) s <- c(s, sprintf("%s moved %s → %s.", change_label(cfg, "field", f), change_fmt(f, a), change_fmt(f, b)))
      }
    }
    if (isTRUE(r$t_evidence)) {
      band_moved <- !identical(r$evidence_prev, r$evidence_cur) && !is.na(r$evidence_prev) && !is.na(r$evidence_cur)
      if (band_moved) s <- c(s, sprintf("Evidence %s from %s to %s.", r$evidence_direction, r$evidence_prev, r$evidence_cur))
      if (isTRUE(r$evidence_basis_changed)) {
        lab <- function(v) change_label(cfg, "evidence_status", v)
        s <- c(s, sprintf("Evidence basis changed from %s to %s.", lab(r$evidence_status_prev), lab(r$evidence_status_cur)))
      }
    }
    if (isTRUE(r$t_confidence)) s <- c(s, sprintf("Confidence %s from %s to %s.", ifelse(r$confidence_direction == "improved", "rose", "fell"), r$confidence_prev, r$confidence_cur))
    s <- c(s, switch(r$attribution,
      real_world = "The method was the same in both vintages, so this reflects a change in the data.",
      methodology = if (!is.na(r$method_types)) "A bridge run shows the method change alone produces this; the data did not change the result."
                    else "The method changed, the data is verifiably unchanged and this cell's measured inputs did not move: the change comes from the method, not the world.",
      mixed = "The method changed and the data changed too; without a bridge run the two cannot be separated.",
      unattributed = "The method changed between vintages. This cell's measured inputs did not move, but the raw-input vintages were not recorded, so a data change cannot be ruled out: not attributed.",
      both = sprintf("A bridge run splits this: the method change accounts for %s and new data for %s.",
                     gsub(";", ", ", r$method_types), gsub(";", ", ", r$world_types)),
      ""))
    if (isTRUE(r$method_unverified)) s <- c(s, "(The method fingerprint is unverified: one of the exports was built from uncommitted code.)")
    paste(s, collapse = " ")
  }, character(1))
}
