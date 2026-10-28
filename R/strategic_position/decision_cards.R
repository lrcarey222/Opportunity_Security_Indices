# Decision cards for the strategic posture (docs/strategic_position.md, section 7). A card walks one
# node through the four steps of the framework: Strategic Position (the evidence) -> Strategic
# Posture (Secure, Compete or Deploy, from five inspectable tests) -> Policy Design (fence, gate,
# build) -> Evaluation (observed vs recommended policy, benchmark, exit condition).
#
# Cards read the canonical strategic_position_tbl columns that the posture engine and
# sp_policy_design() add. A line whose data are missing is left out rather than printed as "NA", so a
# card shows what the evidence holds and nothing more. No score or index is computed for the posture:
# the card shows the categorical answer and the test states behind it.
#
# Pure functions: return character; scripts write the file. Needs dplyr, tibble and jsonlite (the
# decision_trace column is a JSON string).

SP_CARD_TESTS <- data.frame(
  id = c("t1_consequence", "t2_vulnerability", "t3_capability", "t4_strategy", "t5_proportionality"),
  label = c("Critical consequence", "Exploitable vulnerability", "Competitive capability", "Credible strategy", "Proportionality"),
  stringsAsFactors = FALSE
)

# The example cases a review document shows, in display order. pick_order sets the order in which they
# are chosen: the specific cases first, so that the plain Secure / Compete / Deploy examples are other
# nodes where the table has them.
SP_CARD_CASES <- data.frame(
  case = c("secure", "compete", "deploy", "under_prioritized_secure", "under_supported_compete",
           "over_prioritized_deploy", "allied_high_need_low_position"),
  case_label = c("Secure", "Compete", "Deploy", "Under-prioritized Secure", "Latent or under-supported Compete",
                 "Over-prioritized Deploy", "High need, low position, allied delivery"),
  description = c(
    "A node where an acute consequence meets an exploitable vulnerability (Tests 1 and 2).",
    "A producing node with a capability case, a credible strategy and a proportionate cost (Tests 3 to 5).",
    "A node with neither a security case nor a capability case: openness is the recommendation.",
    "A Secure node whose observed policy priority is below the matched line.",
    "A Compete node whose observed policy priority is below the matched line.",
    "A Deploy node where observed intervention sits among the country's most targeted cells.",
    "High Strategic Need with a low domestic Competitive Position, delivered with allies: capacity is built with partners, not at home."
  ),
  pick_order = c(5L, 6L, 7L, 1L, 2L, 3L, 4L),
  stringsAsFactors = FALSE
)

# ---- Small formatting helpers -----------------------------------------------------------------------

# One field of a card row as a scalar; NA when absent, empty or a zero-length list cell.
dc_scalar <- function(r, name) {
  x <- r[[name]]
  if (is.null(x) || length(x) == 0) return(NA)
  if (is.list(x)) x <- x[[1]]
  if (length(x) == 0) return(NA)
  x <- x[[1]]
  if (is.character(x) && !nzchar(trimws(x))) return(NA)
  x
}

dc_has <- function(x) length(x) == 1 && !is.na(x)

# The first of several column names that holds a value: the canonical table names some fields
# differently from the posture contract (alignment_class for competitive_policy_alignment; the
# per-dimension stage_evidence_quality_cp the engine reads for stage_evidence_quality).
dc_first <- function(r, names) {
  for (n in names) {
    v <- dc_scalar(r, n)
    if (dc_has(v)) return(v)
  }
  NA
}
DC_EVIDENCE_COLS <- c("stage_evidence_quality", "stage_evidence_quality_cp")
DC_CPA_COLS <- c("competitive_policy_alignment", "alignment_class")

# Items of a multi-valued field: the engine writes posture_possible as "secure|compete" and
# pivotal_inputs as "a;b"; a list column is accepted too.
dc_split_items <- function(x) {
  if (is.null(x)) return(character())
  if (is.list(x)) x <- unlist(x)
  x <- as.character(x)
  x <- x[!is.na(x)]
  out <- trimws(unlist(strsplit(x, "[|;,]")))
  unique(out[nzchar(out)])
}

dc_cap <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))

dc_human <- function(x) {
  if (!dc_has(x)) return(NA_character_)
  x <- as.character(x)
  if (x == "n_a") return("Not applicable")
  dc_cap(gsub("_", " ", x, fixed = TRUE))
}

# A mid-rank percentile (0-1) as an ordinal, "91st". Shown on 1st-99th: a mid-rank percentile is never
# exactly 0 or 1, and "0th" or "100th" would read as an extreme the measure does not claim.
sp_ordinal_pct <- function(p) {
  if (!dc_has(p)) return(NA_character_)
  n <- min(99L, max(1L, as.integer(round(100 * as.numeric(p)))))
  suffix <- if (n %% 100 %in% 11:13) "th" else c("th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th")[n %% 10 + 1]
  paste0(n, suffix)
}

dc_share <- function(x) {
  if (!dc_has(x)) return(NA_character_)
  x <- as.numeric(x)
  sprintf(if (x < 0.1) "%.1f%%" else "%.0f%%", 100 * x)
}

dc_iso_label <- function(iso, iso3_names) {
  if (!dc_has(iso)) return(NA_character_)
  nm <- if (!is.null(iso3_names)) unname(iso3_names[iso]) else NA
  if (length(nm) == 1 && !is.na(nm)) paste0(nm, " (", iso, ")") else iso
}

dc_path_label <- function(path) {
  switch(as.character(path),
    digital = "digital path", physical = "physical path", both = "digital and physical paths",
    none = "no path", unknown = "path not established", gsub("_", " ", path, fixed = TRUE)
  )
}

# ---- Decision trace ---------------------------------------------------------------------------------

# Parses the decision_trace JSON ({"tests":[{"id","state","marginal","detail","inputs":[...]}],
# "posture","variant","possible","pivotal"}). NULL when missing or unparsable: the card then falls back
# to the test-state columns, without the one-line details.
sp_parse_decision_trace <- function(json) {
  if (!dc_has(json)) return(NULL)
  tr <- tryCatch(jsonlite::fromJSON(as.character(json), simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(tr)) NULL else tr
}

dc_trace_test <- function(tr, id) {
  for (t in tr$tests) if (identical(t$id, id)) return(t)
  NULL
}

# Pivotal entries may be plain ids or objects with an id. (No `%||%` here: yf_build.R defines its own
# with NA semantics, and a second definition would silently replace it in a shared session.)
dc_trace_items <- function(x) {
  if (is.null(x)) return(character())
  one <- function(e) {
    v <- if (is.list(e)) e$id else e
    if (is.null(v) || length(v) == 0) NA_character_ else as.character(v[[1]])
  }
  out <- vapply(x, one, character(1))
  out[!is.na(out) & nzchar(out)]
}

# ---- Card sections ----------------------------------------------------------------------------------

dc_title_line <- function(r, heading) {
  parts <- c(dc_scalar(r, "Country"), dc_scalar(r, "tech"), dc_scalar(r, "supply_chain"))
  layer <- dc_scalar(r, "layer_id")
  if (dc_has(layer) && layer != "stage") {
    lab <- dc_scalar(r, "layer_label")
    parts <- c(parts, if (dc_has(lab)) lab else layer)
  }
  paste(heading, paste(parts[!is.na(parts)], collapse = " - "))
}

dc_posture_line <- function(r) {
  posture <- dc_scalar(r, "strategic_posture")
  if (!dc_has(posture)) {
    why <- dc_scalar(r, "posture_exclusion")
    return(paste0("**Recommended posture: not classified**", if (dc_has(why)) paste0(" (", gsub("_", " ", why), ")") else ""))
  }
  variant <- dc_scalar(r, "posture_variant")
  conf <- dc_scalar(r, "posture_confidence")
  compl <- dc_scalar(r, "evidence_completeness")
  prim <- dc_scalar(r, "posture_primary_rationale")
  sec <- dc_scalar(r, "posture_secondary_rationale")
  strat <- dc_scalar(r, "compete_strategy")
  deliv <- dc_scalar(r, "delivery_mode")
  parts <- c(
    paste0("**Recommended posture: ", toupper(posture), "**", if (dc_has(variant)) paste0(" (", variant, ")") else ""),
    if (dc_has(conf)) paste0("Confidence: ", dc_cap(conf),
                             if (dc_has(compl)) paste0(" (", round(100 * as.numeric(compl)), "% of relevant inputs known)") else ""),
    if (dc_has(prim)) paste0("Primary rationale: ", dc_cap(prim), if (dc_has(sec)) paste0(" (secondary: ", sec, ")") else ""),
    # The strategy names the Compete case; on a Secure or Deploy node it would read as a recommendation.
    if (posture == "compete" && dc_has(strat)) paste0("Compete strategy: ", dc_human(strat)),
    if (dc_has(deliv)) paste0("Delivery: ", dc_human(deliv))
  )
  paste(parts, collapse = " | ")
}

dc_evidence_lines <- function(r, iso3_names) {
  cp <- sp_ordinal_pct(dc_scalar(r, "competitive_position_percentile"))
  sn <- sp_ordinal_pct(dc_scalar(r, "strategic_need_percentile"))
  pp <- sp_ordinal_pct(dc_scalar(r, "policy_priority_percentile"))
  q <- dc_first(r, DC_EVIDENCE_COLS)
  ev <- c(
    if (!is.na(cp)) paste0("Competitive Position ", cp, " percentile (across countries)"),
    if (!is.na(sn)) paste0("Strategic Need ", sn, " percentile (across countries)"),
    if (!is.na(pp)) paste0("Observed Policy Priority ", pp, " percentile (within the country)"),
    if (dc_has(q)) paste0("Evidence specificity: ", dc_cap(q))
  )
  home <- dc_share(dc_scalar(r, "home_share"))
  ciso <- dc_iso_label(dc_scalar(r, "coercive_top_iso3"), iso3_names)
  csh_raw <- dc_scalar(r, "coercive_top_share")
  csh <- dc_share(csh_raw)
  trusted <- dc_share(dc_scalar(r, "trusted_world_share"))
  coercive <- if (!is.na(ciso) && !is.na(csh)) {
    paste0("largest potentially coercive supplier ", ciso, " with ", csh, " of world supply")
  } else if (!is.na(ciso)) {
    paste0("largest potentially coercive supplier ", ciso)
  } else if (!is.na(csh) && as.numeric(csh_raw) == 0) {
    "no potentially coercive supplier in world supply"
  } else if (!is.na(csh)) {
    paste0("largest potentially coercive supplier holds ", csh, " of world supply")
  }
  sup <- c(
    if (!is.na(home)) paste0("world share ", home),
    coercive,
    if (!is.na(trusted)) paste0("trusted supply (home and same-bloc partners) ", trusted, " of world supply")
  )
  c(
    if (length(ev)) paste0("- **Evidence:** ", paste(ev, collapse = " | ")),
    if (length(sup)) paste0("- **Supply:** ", paste(sup, collapse = " | "))
  )
}

dc_test_lines <- function(r, tr) {
  out <- character()
  for (i in seq_len(nrow(SP_CARD_TESTS))) {
    id <- SP_CARD_TESTS$id[i]
    t <- dc_trace_test(tr, id)
    state <- dc_scalar(r, id)
    if (!dc_has(state) && !is.null(t$state)) state <- t$state
    detail <- if (!is.null(t$detail) && nzchar(t$detail)) t$detail else NA_character_
    if (!dc_has(state) && is.na(detail)) next
    label <- SP_CARD_TESTS$label[i]
    if (id == "t2_vulnerability") {
      path <- dc_scalar(r, "t2_path")
      if (dc_has(path)) label <- paste0(label, " (", dc_path_label(path), ")")
    }
    s <- if (dc_has(state)) paste0("**", dc_human(state), "**", if (isTRUE(t$marginal)) " (marginal)" else "") else "**not recorded**"
    out <- c(out, paste0("- ", label, ": ", s, if (!is.na(detail)) paste0(". ", detail) else ""))
  }
  if (length(out)) c("**Decision tests**", "", out) else character()
}

SP_CPA_LABELS <- c(
  aligned_strength = "Aligned strength (position and observed priority both high)",
  latent_advantage = "Latent advantage (position high, observed priority low)",
  policy_led_bet = "Policy-led bet (observed priority high, position low)",
  low_priority = "Low priority (position and observed priority both low)",
  insufficient_data = "Insufficient data"
)

dc_policy_alignment_text <- function(pa, posture, stage) {
  P <- if (dc_has(posture)) dc_cap(posture) else "the"
  switch(pa,
    aligned = if (identical(posture, "deploy") && identical(stage, "Deployment")) {
      "Aligned: support for deployment itself is what a Deploy recommendation asks for."
    } else if (identical(posture, "deploy")) {
      "Aligned: observed intervention is not concentrated here, and restraint is consistent with Deploy."
    } else {
      paste0("Aligned: observed policy priority matches the ", P, " recommendation.")
    },
    under_prioritized = paste0(
      "Under-prioritized: observed policy priority is below what a ", P, " recommendation implies. ",
      "This is low observed NIPO/PAMS intervention, not proof that the node has no priority."
    ),
    over_prioritized = "Over-prioritized: observed intervention sits among the country's most targeted cells although the recommendation is Deploy.",
    mixed = "Mixed: NIPO and PAMS point in opposite directions, or a Compete node is fenced without a build.",
    insufficient_data = "Insufficient data: no usable observed-policy record for this country or cell.",
    dc_human(pa)
  )
}

dc_alignment_lines <- function(r) {
  cpa <- dc_first(r, DC_CPA_COLS)
  pa <- dc_scalar(r, "policy_alignment")
  out <- c(
    if (dc_has(cpa)) paste0("- Competitive-policy alignment (diagnostic): ", if (cpa %in% names(SP_CPA_LABELS)) SP_CPA_LABELS[[cpa]] else dc_human(cpa)),
    if (dc_has(pa)) paste0("- Policy alignment (observed vs recommended): ",
                           dc_policy_alignment_text(pa, dc_scalar(r, "strategic_posture"), dc_scalar(r, "supply_chain")))
  )
  if (length(out)) c("**Alignment**", "", out) else character()
}

dc_design_lines <- function(r) {
  lvl <- function(...) {
    x <- vapply(list(...), function(v) if (dc_has(v)) gsub("_", " ", v, fixed = TRUE) else NA_character_, character(1))
    x <- x[!is.na(x)]
    if (length(x)) paste0(" (", paste(x, collapse = ", "), ")") else ""
  }
  inst <- function(name) {
    v <- dc_scalar(r, name)
    if (dc_has(v)) paste0(" Instruments: ", v, ".") else ""
  }
  fence <- dc_scalar(r, "fence_posture")
  gate <- dc_scalar(r, "gate_posture")
  build <- dc_scalar(r, "build_posture")
  out <- c(
    if (dc_has(fence)) paste0("- Fence", lvl(dc_scalar(r, "fence_level"), dc_scalar(r, "fence_scope")), ": ", fence, inst("fence_instruments")),
    if (dc_has(gate)) paste0("- Gate", lvl(dc_scalar(r, "gate_level")), ": ", gate, inst("gate_instruments")),
    if (dc_has(build)) paste0("- Build: ", build, inst("build_instruments"))
  )
  if (length(out)) c("**Policy design**", "", out) else character()
}

dc_evaluation_lines <- function(r) {
  bm <- dc_scalar(r, "benchmark")
  ex <- dc_scalar(r, "exit_condition")
  ri <- dc_scalar(r, "review_interval_years")
  # Metrics are a semicolon list whose items contain commas, so split on ";" only.
  sm <- dc_scalar(r, "success_metrics")
  sm <- if (dc_has(sm)) trimws(strsplit(as.character(sm), ";", fixed = TRUE)[[1]]) else character()
  sm <- sm[nzchar(sm)]
  out <- c(
    if (dc_has(bm)) paste0("- **Benchmark:** ", bm),
    if (dc_has(ex)) paste0("- **Exit condition:** ", ex),
    if (dc_has(ri)) paste0("- **Review:** every ", ri, if (as.numeric(ri) == 1) " year" else " years"),
    if (length(sm)) c("- **Success metrics:**", paste0("  - ", sm))
  )
  if (length(out)) c("**Evaluation**", "", out) else character()
}

# Sensitive layers of the cell. With `layers` (strategic_posture_layers_tbl) every layer and its
# posture is listed; otherwise the cell's sensitive_layers_secure ids are listed as Secure.
dc_layer_lines <- function(r, layers) {
  out <- character()
  if (!is.null(layers) && nrow(layers) > 0) {
    by_country <- "Country" %in% names(layers) && dc_has(dc_scalar(r, "Country"))
    if (!by_country && !"iso3" %in% names(layers)) stop("sp_decision_card(): layers needs a Country or iso3 column", call. = FALSE)
    same <- layers$tech == dc_scalar(r, "tech") & layers$supply_chain == dc_scalar(r, "supply_chain") &
      (if (by_country) layers$Country == dc_scalar(r, "Country") else layers$iso3 == dc_scalar(r, "iso3"))
    if ("layer_id" %in% names(layers)) same <- same & !layers$layer_id %in% "stage"
    l <- layers[which(same), , drop = FALSE]
    for (i in seq_len(nrow(l))) {
      li <- l[i, , drop = FALSE]
      lab <- dc_scalar(li, "layer_label")
      id <- dc_scalar(li, "layer_id")
      typ <- dc_scalar(li, "layer_type")
      pst <- dc_scalar(li, "strategic_posture")
      var <- dc_scalar(li, "posture_variant")
      out <- c(out, paste0(
        "- ", if (dc_has(lab)) lab else id, if (dc_has(typ)) paste0(" (", typ, ")") else "", ": ",
        if (dc_has(pst)) paste0(dc_cap(pst), if (dc_has(var)) paste0(" (", var, ")") else "") else "not classified"
      ))
    }
  } else {
    ids <- dc_split_items(dc_scalar(r, "sensitive_layers_secure"))
    if (length(ids)) out <- paste0("- ", ids, ": Secure")
  }
  if (!length(out)) return(character())
  c("**Sensitive layers in this cell**", "", out, "",
    "A layer's posture never changes the cell's posture: the layer gets its own fence.")
}

dc_change_lines <- function(r, tr) {
  possible <- dc_split_items(r[["posture_possible"]])
  if (!length(possible) && !is.null(tr$possible)) possible <- dc_trace_items(tr$possible)
  if (length(possible) <= 1) return(character())
  pivotal <- dc_split_items(r[["pivotal_inputs"]])
  if (!length(pivotal) && !is.null(tr$pivotal)) pivotal <- dc_trace_items(tr$pivotal)
  out <- paste0(
    "**What would change the answer:** the unknown inputs allow ",
    paste(dc_cap(possible), collapse = " or "),
    ". The recommendation is the strict reading: an unproven case for intervention is not met."
  )
  out <- c(out, if (length(pivotal)) {
    paste0("Pivotal inputs (each alone would decide it): ", paste(gsub("_", " ", pivotal, fixed = TRUE), collapse = ", "), ".")
  } else {
    "No single unknown input decides it on its own; several resolved together could."
  })
  out
}

# One card in Markdown. `row` is a one-row data frame or a list with the canonical columns.
# `layers` (optional) is the sensitive-layer table; `iso3_names` (optional) a named character vector
# ISO3 -> country name for the coercive supplier.
sp_decision_card <- function(row, layers = NULL, iso3_names = NULL, heading = "###") {
  if (is.data.frame(row)) {
    if (nrow(row) != 1) stop("sp_decision_card(): row must have exactly one row", call. = FALSE)
    r <- as.list(row)
  } else {
    r <- row
  }
  tr <- sp_parse_decision_trace(dc_scalar(r, "decision_trace"))
  blocks <- list(
    dc_title_line(r, heading),
    dc_posture_line(r),
    dc_evidence_lines(r, iso3_names),
    dc_test_lines(r, tr),
    dc_alignment_lines(r),
    dc_design_lines(r),
    dc_evaluation_lines(r),
    dc_layer_lines(r, layers),
    dc_change_lines(r, tr)
  )
  blocks <- Filter(length, blocks)
  paste(vapply(blocks, paste, character(1), collapse = "\n"), collapse = "\n\n")
}

# ---- Case selection ---------------------------------------------------------------------------------

dc_n_items <- function(x) {
  if (is.list(x)) return(vapply(x, function(v) length(dc_split_items(v)), integer(1)))
  vapply(as.character(x), function(v) length(dc_split_items(v)), integer(1), USE.NAMES = FALSE)
}

# Picks one clear example per case in SP_CARD_CASES from the canonical table (whole cells only).
# Preference, in order: a review country, higher posture_confidence, a settled posture, higher
# stage_evidence_quality, a country not yet used by another case, the case's own clarity (lower
# observed priority for under-prioritized cases, higher for over-prioritized, the need-position gap
# for the allied case), higher evidence_completeness; then alphabetical, so the pick is reproducible.
# Each case gets a different node where the table allows. Returns one row per case with `case`,
# `case_label`, `case_found`; an absent case has case_found = FALSE and no node columns.
# need_min / position_max: the high-need and low-position lines (config alignment thresholds).
# layer_cases: cases that may pick a sensitive-layer row (the table must then carry the layers with
# their cell's Strategic Position columns).
sp_card_cases <- function(tbl, countries = character(), need_min = 0.5, position_max = 0.5, foothold_share = 0.03,
                          layer_cases = character()) {
  tbl <- tibble::as_tibble(tbl[, setdiff(names(tbl), c("case", "case_label", "case_found")), drop = FALSE])
  n <- nrow(tbl)
  col <- function(name) if (name %in% names(tbl)) tbl[[name]] else rep(NA, n)
  num <- function(name) suppressWarnings(as.numeric(col(name)))
  cell <- if ("layer_id" %in% names(tbl)) is.na(tbl$layer_id) | tbl$layer_id == "stage" else rep(TRUE, n)
  posture <- col("strategic_posture")
  pa <- col("policy_alignment")
  pp <- num("policy_priority_percentile")
  sn <- num("strategic_need_percentile")
  cp <- num("competitive_position_percentile")

  # Low domestic position for the allied case: a world share below the foothold line where the table
  # has one (the cross-country percentile saturates for large economies), else the percentile.
  home <- num("home_share")
  low_position <- ifelse(!is.na(home), home < foothold_share, !is.na(cp) & cp < position_max)
  allied <- !is.na(sn) & sn >= need_min & low_position & col("delivery_mode") %in% "allied"
  ok <- list(
    secure = posture %in% "secure",
    compete = posture %in% "compete",
    deploy = posture %in% "deploy",
    under_prioritized_secure = posture %in% "secure" & pa %in% "under_prioritized",
    under_supported_compete = posture %in% "compete" & pa %in% "under_prioritized",
    over_prioritized_deploy = posture %in% "deploy" & pa %in% "over_prioritized",
    # a Secure node delivered with allies first; a diversifying Deploy node only if there is none
    allied_high_need_low_position = allied & (posture %in% "secure" | !any(allied & posture %in% "secure"))
  )
  zero <- rep(0, n)
  clarity <- list(
    under_prioritized_secure = -pp, under_supported_compete = -pp, over_prioritized_deploy = pp,
    allied_high_need_low_position = sn - dplyr::coalesce(home, cp)
  )

  review <- as.numeric(col("Country") %in% countries)
  conf <- match(col("posture_confidence"), c("low", "moderate", "high"))
  conf[is.na(conf)] <- 0
  ev_col <- intersect(DC_EVIDENCE_COLS, names(tbl))
  evq <- match(if (length(ev_col)) tbl[[ev_col[1]]] else rep(NA, n), c("none", "low", "medium", "high"))
  evq[is.na(evq)] <- 0
  settled <- if ("posture_possible" %in% names(tbl)) as.numeric(dc_n_items(tbl$posture_possible) <= 1) else zero
  compl <- num("evidence_completeness")
  compl[is.na(compl)] <- -1

  used <- integer()
  used_countries <- character()
  picked <- stats::setNames(rep(NA_integer_, nrow(SP_CARD_CASES)), SP_CARD_CASES$case)
  for (k in SP_CARD_CASES$case[order(SP_CARD_CASES$pick_order)]) {
    idx <- which(ok[[k]] & (cell | k %in% layer_cases) & !is.na(posture))
    if (!length(idx)) next
    fresh <- setdiff(idx, used)
    if (length(fresh)) idx <- fresh
    cl <- if (is.null(clarity[[k]])) zero else clarity[[k]]
    cl[is.na(cl)] <- -1e9
    new_country <- as.numeric(!tbl$Country[idx] %in% used_countries)
    o <- order(-review[idx], -conf[idx], -settled[idx], -evq[idx], -new_country, -cl[idx], -compl[idx],
               as.character(col("Country")[idx]), as.character(col("tech")[idx]), as.character(col("supply_chain")[idx]))
    pick <- idx[o[1]]
    picked[[k]] <- pick
    used <- c(used, pick)
    used_countries <- c(used_countries, as.character(tbl$Country[pick]))
  }

  rows <- lapply(seq_len(nrow(SP_CARD_CASES)), function(i) {
    k <- SP_CARD_CASES$case[i]
    head <- tibble::tibble(case = k, case_label = SP_CARD_CASES$case_label[i], case_found = !is.na(picked[[k]]))
    if (is.na(picked[[k]])) head else dplyr::bind_cols(head, tbl[picked[[k]], , drop = FALSE])
  })
  dplyr::bind_rows(rows)
}

# ---- Document ---------------------------------------------------------------------------------------

SP_CARDS_INTRO <- c(
  "Each card follows one node (a country x technology x supply-chain stage) through four steps:",
  "",
  "1. **Strategic Position**: what the evidence says. Competitive Position and Strategic Need are percentiles across countries in the cell; Revealed Policy Priority is a percentile across the country's own cells; evidence specificity says how much of the score rests on stage-specific observations.",
  "2. **Strategic Posture**: what kind of problem the node is. Secure (an acute consequence meets an exploitable vulnerability), Compete (a producing node with a capability case, a credible strategy and a proportionate cost) or Deploy (neither: openness, low cost and diversified supply). Five tri-state tests decide it; unknown inputs are bracketed, and the recommendation is the strict reading, so an unproven case for intervention is not met.",
  "3. **Policy Design**: the fence (what is restricted, and how narrowly), the gate (what stays open, and to whom) and the build (what capacity is created, and where), with the delivery mode.",
  "4. **Evaluation**: observed policy compared with the recommendation, and the benchmark, exit condition and review date that say when to reclassify.",
  "",
  "The posture is categorical: no score or index is computed for it. Observed policy never enters a test, and concentration alone never makes a node Secure.",
  "",
  paste(
    "**Caveat on observed policy.** Revealed Policy Priority rests on GTA NIPO and IEA PAMS records. A low observed policy",
    "priority is low observed NIPO/PAMS intervention, not proof that the government gives the node no priority: GTA does",
    "not record standards, technical regulation, most domestic regulation or agreed international measures, and NIPO has",
    "no Deployment evidence."
  )
)

# A Markdown document of the example cards. `tbl` is the canonical table (used for the posture counts);
# `cases` is the output of sp_card_cases(). Absent cases are listed as such rather than dropped.
sp_decision_cards_md <- function(tbl, cases, layers = NULL, iso3_names = NULL,
                                 title = "Strategic posture decision cards") {
  cell <- if ("layer_id" %in% names(tbl)) is.na(tbl$layer_id) | tbl$layer_id == "stage" else rep(TRUE, nrow(tbl))
  counts <- character()
  if ("strategic_posture" %in% names(tbl)) {
    p <- tbl$strategic_posture[cell]
    counts <- sprintf(
      "The table holds %d nodes: %d Secure, %d Compete, %d Deploy, %d not classified.",
      length(p), sum(p %in% "secure"), sum(p %in% "compete"), sum(p %in% "deploy"), sum(is.na(p))
    )
  }
  absent <- cases$case_label[!cases$case_found]
  out <- c(paste("#", title), "", SP_CARDS_INTRO, "", counts,
           if (length(absent)) c("", paste0("No node in the table meets: ", paste(absent, collapse = "; "), ".")))
  for (i in seq_len(nrow(cases))) {
    k <- cases$case[i]
    desc <- SP_CARD_CASES$description[match(k, SP_CARD_CASES$case)]
    body <- if (isTRUE(cases$case_found[i])) {
      sp_decision_card(cases[i, setdiff(names(cases), c("case", "case_label", "case_found")), drop = FALSE],
                       layers = layers, iso3_names = iso3_names, heading = "###")
    } else {
      "No node in the table meets this case."
    }
    out <- c(out, "", paste("##", cases$case_label[i]), "", if (!is.na(desc)) c(paste0("_", desc, "_"), ""), body)
  }
  paste(c(out, ""), collapse = "\n")
}
