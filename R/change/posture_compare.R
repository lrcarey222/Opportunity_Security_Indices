# Deterministic change detection between two posture vintages (docs/change-layer-architecture.md 3-4).
# Types: recommendation, score, data, evidence, confidence; the pair carries methodology. Each change record is
# attributed real_world / methodology / mixed (or decomposed with a bridge vintage: new method on previous inputs).
# No model is involved: every rule reads config/change_layer.yml.
#
# Pure functions: no IO. Needs dplyr, tibble and R/change/posture_vintage.R.

CHANGE_KEY <- c("iso3", "tech", "stage")
CHANGE_TYPES <- c("recommendation", "score", "data", "evidence", "confidence")
CHANGE_ROUND_TOL <- 1e-9   # cores are rounded to 3 dp; compare deltas with a hair of slack

# Two vintages are comparable when they cover the same techs and stages.
posture_comparable <- function(prev_header, cur_header) {
  st <- setequal(unlist(prev_header$stages), unlist(cur_header$stages))
  te <- setequal(unlist(prev_header$techs), unlist(cur_header$techs))
  reason <- c(if (!st) "the stage taxonomy differs", if (!te) "the technology set differs")
  list(comparable = st && te, reason = if (length(reason)) paste(reason, collapse = "; ") else NA_character_)
}

change_rank <- function(x, scale) match(x, scale)

# Classify every cell of a joined pair. j: full join of prev and cur on CHANGE_KEY with _prev / _cur suffixes and a
# presence column. Returns j with logical type columns, severity, magnitude, crossed and directions.
posture_classify_cells <- function(j, cfg, bars_prev, bars_cur) {
  th <- cfg$thresholds
  sc <- th$score
  both <- j$presence == "both"
  ch_p <- dplyr::coalesce(j$choice_prev, "not_assessed")
  ch_c <- dplyr::coalesce(j$choice_cur, "not_assessed")
  fam_p <- posture_family_of(ch_p)
  fam_c <- posture_family_of(ch_c)
  # an absent side is not assessed; a cell absent on one side and not assessed on the other is no change
  rec <- ch_p != ch_c
  j$subtype <- ifelse(!rec, NA_character_,
                      ifelse(ch_p == "not_assessed" | ch_c == "not_assessed", "coverage",
                             ifelse(fam_p != fam_c, "posture", "mode")))
  j$t_recommendation <- rec

  bar_of <- function(field, need_type, bars) {
    if (field == "need") ifelse(need_type == "deployment", bars$deployment, bars$need) else rep(bars[[field]], length(need_type))
  }
  nt <- dplyr::coalesce(j$need_type_cur, j$need_type_prev, "supply_chain")
  score_hit <- rep(FALSE, nrow(j))
  moderate_hit <- rep(FALSE, nrow(j))
  crossed <- rep("", nrow(j))
  mag <- rep(NA_real_, nrow(j))
  for (f in sc$fields) {
    a <- j[[paste0(f, "_prev")]]
    b <- j[[paste0(f, "_cur")]]
    d <- b - a
    j[[paste0("d_", f)]] <- d
    bp <- bar_of(f, nt, bars_prev)
    bc <- bar_of(f, nt, bars_cur)
    cross <- !is.na(a) & !is.na(b) & ((a >= bp - CHANGE_ROUND_TOL) != (b >= bc - CHANGE_ROUND_TOL))
    big <- !is.na(d) & abs(d) >= sc$min_delta - CHANGE_ROUND_TOL
    appear <- xor(is.na(a), is.na(b)) & both
    hit <- both & (cross | big | appear)
    score_hit <- score_hit | hit
    moderate_hit <- moderate_hit | (both & (cross | (!is.na(d) & abs(d) >= sc$moderate_delta - CHANGE_ROUND_TOL)))
    tag <- ifelse(both & cross, sprintf("%s %.3f->%.3f (bar %.2f)", f, a, b, bc), "")
    crossed <- ifelse(tag == "", crossed, ifelse(crossed == "", tag, paste(crossed, tag, sep = "; ")))
    mag <- pmax(mag, ifelse(both, abs(d), NA_real_), na.rm = TRUE)
  }
  j$t_score <- score_hit
  j$crossed <- ifelse(crossed == "", NA_character_, crossed)
  j$magnitude <- mag

  data_hit <- rep(FALSE, nrow(j))
  for (f in names(th$data)) {
    r <- th$data[[f]]
    a <- j[[paste0(f, "_prev")]]
    b <- j[[paste0(f, "_cur")]]
    d <- abs(b - a)
    rel <- d / pmax(abs(a), 1e-9)
    data_hit <- data_hit | (both & !is.na(d) & d >= r$min_abs - CHANGE_ROUND_TOL & rel >= (r$min_rel %||% 0) - CHANGE_ROUND_TOL)
  }
  j$t_data <- data_hit
  # any measured-input move beyond rounding (attribution uses this, not the materiality rule)
  inputs_moved <- rep(FALSE, nrow(j))
  for (f in names(th$data)) {
    d <- abs(j[[paste0(f, "_cur")]] - j[[paste0(f, "_prev")]])
    inputs_moved <- inputs_moved | (both & !is.na(d) & d > 0.001 + CHANGE_ROUND_TOL)
  }
  j$inputs_moved <- inputs_moved

  ev_p <- change_rank(j$evidence_prev, th$evidence_scale)
  ev_c <- change_rank(j$evidence_cur, th$evidence_scale)
  ev_d <- ev_c - ev_p
  j$t_evidence <- both & !is.na(ev_d) & ev_d != 0
  j$evidence_direction <- ifelse(j$t_evidence, ifelse(ev_d > 0, "improved", "worsened"), NA_character_)
  cf_p <- change_rank(j$confidence_prev, th$confidence_scale)
  cf_c <- change_rank(j$confidence_cur, th$confidence_scale)
  cf_d <- cf_c - cf_p
  j$t_confidence <- both & !is.na(cf_d) & cf_d != 0
  j$confidence_direction <- ifelse(j$t_confidence, ifelse(cf_d > 0, "improved", "worsened"), NA_character_)

  sev <- pmax(ifelse(j$t_recommendation, 3L, 0L),
              ifelse(j$t_confidence, 2L, 0L),
              ifelse(j$t_score, ifelse(moderate_hit, 2L, 1L), 0L),
              ifelse(j$t_evidence, ifelse(abs(dplyr::coalesce(ev_d, 0L)) >= 2L, 2L, 1L), 0L),
              ifelse(j$t_data, 1L, 0L))
  j$severity <- c("none", "minor", "moderate", "major")[sev + 1L]
  tm <- as.matrix(j[, paste0("t_", CHANGE_TYPES)])
  j$types <- apply(tm, 1, function(r) paste(CHANGE_TYPES[r], collapse = ";"))
  j$meaningful <- rowSums(tm) > 0
  j
}

change_join <- function(prev, cur) {
  j <- dplyr::full_join(prev, cur, by = CHANGE_KEY, suffix = c("_prev", "_cur"))
  kp <- paste(prev$iso3, prev$tech, prev$stage)
  kc <- paste(cur$iso3, cur$tech, cur$stage)
  kj <- paste(j$iso3, j$tech, j$stage)
  j$presence <- ifelse(kj %in% kp & kj %in% kc, "both", ifelse(kj %in% kp, "only_prev", "only_cur"))
  j
}

# Compare two vintages. prev / cur: vintage tables; prev_header / cur_header: vintage headers (method$id,
# method$unverified, data$id, bars, techs, stages, vintage_id). bridge: optional vintage table built with the
# current method on the previous inputs, which splits a methodology pair exactly.
# Returns list(pair = <list>, changes = <tibble of meaningful change records>).
posture_compare <- function(prev, cur, prev_header, cur_header, cfg, bridge = NULL) {
  cmp <- posture_comparable(prev_header, cur_header)
  method_changed <- !identical(prev_header$method$id, cur_header$method$id)
  unverified <- isTRUE(prev_header$method$unverified) || isTRUE(cur_header$method$unverified)
  pair <- list(prev = prev_header$vintage_id, cur = cur_header$vintage_id, comparable = cmp$comparable,
               reason = cmp$reason, method_changed = method_changed, method_unverified = unverified,
               data_changed = !identical(prev_header$data$id, cur_header$data$id), bridge = !is.null(bridge))
  if (!cmp$comparable) {
    pair$counts <- list()
    return(list(pair = pair, changes = change_empty_records()))
  }
  j <- posture_classify_cells(change_join(prev, cur), cfg, prev_header$bars, cur_header$bars)
  pair$n_cells <- sum(j$presence == "both")
  pair$n_unchanged <- sum(!j$meaningful & j$presence == "both")
  x <- j[j$meaningful, , drop = FALSE]
  x$method_types <- NA_character_
  x$world_types <- NA_character_
  if (!method_changed) {
    x$attribution <- rep("real_world", nrow(x))
  } else if (!is.null(bridge)) {
    jm <- posture_classify_cells(change_join(prev, bridge), cfg, prev_header$bars, cur_header$bars)
    jw <- posture_classify_cells(change_join(bridge, cur), cfg, cur_header$bars, cur_header$bars)
    km <- match(paste(x$iso3, x$tech, x$stage), paste(jm$iso3, jm$tech, jm$stage))
    kw <- match(paste(x$iso3, x$tech, x$stage), paste(jw$iso3, jw$tech, jw$stage))
    m_any <- dplyr::coalesce(jm$meaningful[km], FALSE)
    w_any <- dplyr::coalesce(jw$meaningful[kw], FALSE)
    x$method_types <- ifelse(m_any, jm$types[km], NA_character_)
    x$world_types <- ifelse(w_any, jw$types[kw], NA_character_)
    x$attribution <- ifelse(m_any & w_any, "both", ifelse(m_any, "methodology", ifelse(w_any, "real_world", "methodology")))
  } else {
    x$attribution <- ifelse(x$inputs_moved, "mixed", "methodology")
  }
  x$method_changed <- rep(method_changed, nrow(x))
  x$method_unverified <- rep(unverified, nrow(x))
  x$prev_vintage <- rep(pair$prev, nrow(x))
  x$cur_vintage <- rep(pair$cur, nrow(x))
  x$id <- assessment_id(x$iso3, x$tech, x$stage, cfg$assessment_id_prefix %||% "aib:posture")
  x$sources <- change_sources(x, cfg)
  x$explanation <- posture_change_explain(x, cfg)
  rank <- c(none = 0, minor = 1, moderate = 2, major = 3)
  x <- x[order(-rank[x$severity], -dplyr::coalesce(x$magnitude, 0), x$iso3, x$tech, x$stage), , drop = FALSE]
  pair$counts <- c(as.list(stats::setNames(vapply(CHANGE_TYPES, function(t) sum(x[[paste0("t_", t)]]), integer(1)), CHANGE_TYPES)),
                   list(meaningful = nrow(x)),
                   as.list(table(factor(x$attribution, c("real_world", "methodology", "mixed", "both")))))
  list(pair = pair, changes = change_records(x))
}

# Provenance: the source ids behind the fields that changed in each record (";"-joined, sorted).
change_sources <- function(x, cfg) {
  src <- cfg$sources %||% list()
  ids <- function(k) unlist(src[[k]]$source_ids %||% list())
  vapply(seq_len(nrow(x)), function(i) {
    f <- character()
    if (x$t_score[i] || x$t_recommendation[i]) {
      for (s in cfg$thresholds$score$fields) {
        d <- x[[paste0("d_", s)]][i]
        if (!is.na(d) && abs(d) > 0.001) f <- c(f, if (s == "need" && identical(x$need_type_cur[i], "deployment")) "deployment_need" else s)
      }
    }
    if (x$t_data[i]) f <- c(f, names(cfg$thresholds$data))
    if (x$t_evidence[i]) f <- c(f, "evidence")
    out <- sort(unique(unlist(lapply(unique(f), ids))))
    if (length(out)) paste(out, collapse = ";") else NA_character_
  }, character(1))
}

CHANGE_RECORD_COLS <- c("id", "iso3", "tech", "stage", "prev_vintage", "cur_vintage", "types", "subtype", "severity",
                        "magnitude", "attribution", "method_changed", "method_unverified", "method_types", "world_types",
                        "crossed", "evidence_direction", "confidence_direction", "sources", "explanation")
CHANGE_SIDE_FIELDS <- c("choice", "posture", "need_type", "need", "prize", "cp", "market", "share", "confidence", "evidence")

change_records <- function(x) {
  out <- x[, intersect(CHANGE_RECORD_COLS, names(x)), drop = FALSE]
  for (f in CHANGE_SIDE_FIELDS) {
    out[[paste0("prev_", f)]] <- x[[paste0(f, "_prev")]]
    out[[paste0("cur_", f)]] <- x[[paste0(f, "_cur")]]
  }
  for (f in c("need", "prize", "cp")) out[[paste0("d_", f)]] <- x[[paste0("d_", f)]]
  out$impacts <- rep(NA_character_, nrow(out))
  tibble::as_tibble(out)
}

change_empty_records <- function() {
  cols <- c(CHANGE_RECORD_COLS, as.vector(outer(c("prev_", "cur_"), CHANGE_SIDE_FIELDS, paste0)), "d_need", "d_prize", "d_cp", "impacts")
  tibble::as_tibble(stats::setNames(lapply(cols, function(i) character()), cols))
}

# The previous comparable vintage of each vintage (headers ordered by created). Returns integer indices (NA none).
posture_previous_comparable <- function(headers) {
  vapply(seq_along(headers), function(i) {
    if (i == 1L) return(NA_integer_)
    for (k in rev(seq_len(i - 1L))) if (posture_comparable(headers[[k]], headers[[i]])$comparable) return(k)
    NA_integer_
  }, integer(1))
}
