# QC: canonical (pipeline-level) per-cell robustness of the strategic choice (docs/QC_SYSTEM_DESIGN.md 4.6, 6.3).
#
# Re-runs the canonical choice rule (R/strategic_position/decision_engine.R sp_choice_rule(), with
# sp_choice_inputs() and sp_choice_high_cp()) from the stored columns of strategic_position_tbl under
# alternative specifications, one at a time, and classifies every cell:
#   fragile       at least one LOCAL spec changes the choice (each decision threshold +/- the borderline band,
#                 0.05, one at a time; and the strict reading of cp >= 0.75)
#   sensitive     no local spec changes it, at least one STRUCTURAL spec does (large-yard min_signals 4 / 6,
#                 position_first off, market-only gate 0.33, Unknown attributes counted 0.5, need score floor
#                 0.05, prize threshold 0.65, Accelerate requiring >= 2 deployment-need families, need 0.60)
#   stable        no spec changes it
#   not_testable  the specs cannot exercise the cell (excluded, not assessed, the Secure override of the
#                 posture engine, which is not rerun here, or a stored choice the rule does not reproduce)
# fence_change says whether any flip moves the cell to another yard (yard_rollup.R sp_yard_of_choice()), which
# is more consequential than a change of mode within a fence.
#
# NEED-FLOOR CAP. supply_chain_need.R caps a need percentile that reaches need_threshold - band while the need
# score is below supply_chain_need.score_floor at need_threshold - band - 0.01 (0.61 at the configured 0.67).
# The engine's sp_choice_threshold_sensitivity() moves the threshold over the stored, capped value, so at need
# 0.60 every capped cell (0.61) reads as high need: an artifact. Every need spec here recomputes the need from
# supply_chain_need_uncapped and supply_chain_need_score with the cap rule at the spec's own threshold and floor
# (qc_sens_need()); the naive reading is kept as a diagnostic spec (need_060_naive) that never classifies.
#
# Specs that need the node attributes (the large-yard case and the strategic industrial value are recomputed
# with sp_large_yard_case() / sp_strategic_value(), as scripts/26 does) are compared against the same function
# rerun at the configured values, so a reproduction gap in the recomputation is never counted as a flip. They
# are skipped (status skipped, with the reason) when no node attributes are given.
#
# Pure functions: no IO. Needs R/strategic_position/{sp_helpers, industrial_opportunity, strategic_value,
# decision_engine, yard_rollup}.R and R/utils/supply_chain_stages.R sourced by the caller.

QC_SENS_CLASSES <- c("stable", "sensitive", "fragile", "not_testable")
QC_SENS_NEIS <- c("USA", "JPN", "KOR", "AUS", "IND", "DEU")
QC_SENS_KINDS <- c("local", "structural", "diagnostic")
# the SIV attributes an "Unknown counted 0.5" spec reads as partly
QC_SENS_HALF_FIELDS <- c("process_knowledge", "workforce_suppliers", "adjacent_innovation",
                         "beyond_novelty", "cost_perf_advantage", "incumbent_vulnerable", "scale_pathway")
# the input a spec perturbs (the driver named when it flips a cell)
QC_SENS_DRIVER <- c(
  need = "supply_chain_need", prize = "strategic_industrial_value", cp = "cp_contender_pct",
  deployment = "deployment_need", large_yard = "large_yard_min_signals", position_first = "large_yard_position_first",
  siv_gate = "siv_market_only_gate", siv_unknown = "siv_unknown_attributes", need_floor = "need_score_floor",
  dn_families = "deployment_need_families"
)



# The specifications, one row each: spec_id, kind (local | structural | diagnostic), driver (a QC_SENS_DRIVER
# key), parameter, value (character), label, needs (table | node_attributes). cfg: the decision_framework list.
qc_sens_specs <- function(cfg = list()) {
  th <- sp_choice_thresholds(cfg)
  b <- th$band
  r <- function(x) format(round(x, 4), nsmall = 2)
  floor0 <- (cfg$supply_chain_need %||% list())$score_floor %||% 0
  gate0 <- (cfg$strategic_value %||% list())$gate %||% 0.5
  row <- function(id, kind, driver, parameter, value, label, needs = "table") {
    data.frame(spec_id = id, kind = kind, driver = driver, parameter = parameter, value = as.character(value),
               label = label, needs = needs, stringsAsFactors = FALSE)
  }
  rbind(
    row("need_minus", "local", "need", "need_threshold", r(th$need - b), paste0("Need threshold ", r(th$need - b), " (cap-aware)")),
    row("need_plus", "local", "need", "need_threshold", r(th$need + b), paste0("Need threshold ", r(th$need + b), " (cap-aware)")),
    row("prize_minus", "local", "prize", "prize_threshold", r(th$prize - b), paste0("Prize threshold ", r(th$prize - b))),
    row("prize_plus", "local", "prize", "prize_threshold", r(th$prize + b), paste0("Prize threshold ", r(th$prize + b))),
    row("cp_minus", "local", "cp", "cp_preserve_pct", r(th$cp - b), paste0("Position threshold ", r(th$cp - b))),
    row("cp_plus", "local", "cp", "cp_preserve_pct", r(th$cp + b), paste0("Position threshold ", r(th$cp + b))),
    row("cp_strict", "local", "cp", "cp_rule", "strict", paste0("Position strictly above ", r(th$cp), " (configured: at or above)")),
    row("deployment_minus", "local", "deployment", "deployment_threshold", r(th$deployment - b),
        paste0("Deployment-need threshold ", r(th$deployment - b))),
    row("deployment_plus", "local", "deployment", "deployment_threshold", r(th$deployment + b),
        paste0("Deployment-need threshold ", r(th$deployment + b))),
    row("ly_min_signals_4", "structural", "large_yard", "large_yard.min_signals", "4", "Large-yard case on 4 commodity signals",
        "node_attributes"),
    row("ly_min_signals_6", "structural", "large_yard", "large_yard.min_signals", "6", "Large-yard case on 6 commodity signals",
        "node_attributes"),
    row("position_first_off", "structural", "position_first", "large_yard.position_first", "FALSE",
        "Large-yard case decides before Competitive Position"),
    row("siv_gate_033", "structural", "siv_gate", "strategic_value.gate", "0.33",
        paste0("Market-only gate 0.33 (configured ", gate0, ")"), "node_attributes"),
    row("siv_unknown_half", "structural", "siv_unknown", "strategic_value.unknown", "0.5",
        "Unknown commons / leap-test attributes counted 0.5 (configured 0)", "node_attributes"),
    row("need_floor_005", "structural", "need_floor", "supply_chain_need.score_floor", "0.05",
        paste0("Need score floor 0.05 (configured ", floor0, ")")),
    row("prize_065", "structural", "prize", "prize_threshold", "0.65", "Prize threshold 0.65"),
    row("dn_min_families_2", "structural", "dn_families", "deployment_need.min_families", "2",
        "Accelerate requires at least 2 deployment-need families"),
    row("need_060", "structural", "need", "need_threshold", "0.60", "Need threshold 0.60 (cap-aware: uncapped percentile and cap rule)"),
    row("need_060_naive", "diagnostic", "need", "need_threshold", "0.60",
        "Need threshold 0.60 over the stored capped need (the engine sensitivity's reading; artifact check, never classifies)")
  )
}

# Need at a threshold and score floor, cap-aware: the uncapped within-cell percentile with supply_chain_need.R's
# cap rule (a percentile at or above threshold - band with a score below the floor is capped at
# threshold - band - 0.01). Rows without an uncapped percentile keep the stored need. A table without the
# uncapped percentile or the score returns the stored need (and is flagged by qc_sens_reproduction()).
qc_sens_need <- function(tbl, need_threshold = 0.67, band = 0.05, floor = 0.10) {
  stored <- suppressWarnings(as.numeric(tbl$supply_chain_need))
  if (!all(c("supply_chain_need_uncapped", "supply_chain_need_score") %in% names(tbl))) return(stored)
  unc <- suppressWarnings(as.numeric(tbl$supply_chain_need_uncapped))
  score <- suppressWarnings(as.numeric(tbl$supply_chain_need_score))
  capped <- !is.na(unc) & unc >= need_threshold - band & !is.na(score) & score < floor
  out <- ifelse(capped, pmin(unc, need_threshold - band - 0.01), unc)
  out[is.na(unc)] <- stored[is.na(unc)]
  out
}

# The choice from the stored inputs with optional overrides (NULL = the stored input): need, prize and capped
# (the market-only cap) at the producing stages, large (the large-yard case), cp_strict (cp > threshold
# instead of >=), dn_min_families (Accelerate needs at least this many deployment-need families, else Watch).
# NA where the stored choice is NA (excluded rows).
qc_sens_choice <- function(tbl, cfg = list(), need = NULL, prize = NULL, capped = NULL, large = NULL, cp_strict = FALSE,
                           dn_min_families = NULL) {
  th <- sp_choice_thresholds(cfg)
  inp <- sp_choice_inputs(tbl, cfg)
  dep <- tbl$supply_chain == "Deployment"
  need_v <- if (is.null(need)) inp$need else ifelse(dep, NA_real_, need)
  prize_v <- if (is.null(prize)) inp$prize else ifelse(dep, NA_real_, prize)
  capped_v <- if (is.null(capped)) inp$capped else (!dep & capped %in% TRUE)
  large_v <- if (is.null(large)) inp$large else ifelse(dep, NA, as.logical(large))
  cp_t <- if (isTRUE(cp_strict)) th$cp + 1e-9 else th$cp
  ch <- sp_choice_high_cp(tbl$cp_contender_pct, tbl$cp_contender_basis, tbl$cp_is_contender, tbl$t3_capability_lo, cp_t, th$floor)
  v <- sp_choice_rule(tbl$supply_chain, tbl$engine_posture %in% "secure", need_v, prize_v, ch, tbl$pathway_credible, inp$dn, th,
                      large_v, capped_v)
  if (!is.null(dn_min_families)) {
    fam <- if ("dn_families_observed" %in% names(tbl)) suppressWarnings(as.integer(tbl$dn_families_observed)) else rep(NA_integer_, nrow(tbl))
    v[dep & v %in% "accelerate" & (is.na(fam) | fam < dn_min_families)] <- "watch"
  }
  v[is.na(tbl$strategic_choice)] <- NA_character_
  v
}

# Node attributes with every Unknown (NA, "" or "Unknown") commons and leap-test attribute read as partly (0.5).
qc_sens_unknown_half <- function(nodes) {
  for (f in intersect(QC_SENS_HALF_FIELDS, names(nodes))) {
    v <- trimws(as.character(nodes[[f]]))
    v[is.na(v) | v %in% c("", "Unknown")] <- "partly"
    nodes[[f]] <- v
  }
  nodes
}

qc_sens_set <- function(cfg, path, value) {
  if (length(path) == 1) {
    cfg[[path]] <- value
  } else {
    cfg[[path[1]]] <- qc_sens_set(cfg[[path[1]]] %||% list(), path[-1], value)
  }
  cfg
}

# How far the stored table can be reproduced from its own columns: the choice rule (rerun vs strategic_choice),
# the cap-aware need (vs supply_chain_need), and with nodes the strategic industrial value, the market-only cap
# and the large-yard case (vs the stored columns). One row per check: check, n_compared, n_mismatch,
# max_abs_diff, examples (up to 5 "ISO3|tech|stage stored->recomputed").
qc_sens_reproduction <- function(tbl, cfg = list(), nodes = NULL, tol = 1e-9) {
  key <- paste(tbl$iso3, tbl$tech, tbl$supply_chain, sep = "|")
  out <- list()
  add <- function(check, stored, recomputed, numeric = FALSE) {
    ok <- !(is.na(stored) & is.na(recomputed))
    if (numeric) {
      d <- abs(as.numeric(stored) - as.numeric(recomputed))
      bad <- ok & (is.na(d) | d > tol)
      mx <- if (any(ok & !is.na(d))) max(d[ok & !is.na(d)]) else NA_real_
    } else {
      bad <- ok & (is.na(stored) | is.na(recomputed) | as.character(stored) != as.character(recomputed))
      mx <- NA_real_
    }
    ex <- utils::head(which(bad), 5)
    out[[length(out) + 1]] <<- data.frame(
      check = check, n_compared = sum(ok), n_mismatch = sum(bad), max_abs_diff = mx,
      examples = if (length(ex)) paste0(key[ex], " ", stored[ex], "->", recomputed[ex], collapse = "; ") else NA_character_,
      stringsAsFactors = FALSE)
  }
  th <- sp_choice_thresholds(cfg)
  floor0 <- (cfg$supply_chain_need %||% list())$score_floor %||% 0
  need0 <- qc_sens_need(tbl, th$need, th$band, floor0)
  add("choice_rule_rerun", tbl$strategic_choice, qc_sens_choice(tbl, cfg, need = need0))
  add("choice_rule_rerun_stored_need", tbl$strategic_choice, sp_choice_rerun(tbl, cfg))
  prod <- tbl$supply_chain != "Deployment"
  add("need_cap_aware", ifelse(prod, tbl$supply_chain_need, NA), ifelse(prod, need0, NA), numeric = TRUE)
  # The cap-aware need at a moved threshold against the engine itself: sp_supply_chain_need() rerun from the stored
  # family columns with need_threshold 0.60 (only when supply_chain_need.R is sourced and the table carries its
  # inputs). This is what makes need_060 the reading the pipeline would give, unlike the naive one.
  eng_cols <- c("sn_import_dependence", "sn_supplier_concentration", "sn_risky_supplier_exposure", "strategic_need_percentile",
                "trusted_world_share", "home_share", "sn_import_material")
  if (exists("sp_supply_chain_need", mode = "function") && all(eng_cols %in% names(tbl))) {
    eng <- sp_supply_chain_need(tbl[, c("tech", "supply_chain", eng_cols), drop = FALSE], qc_sens_set(cfg, "need_threshold", 0.60))
    add("need_cap_aware_060_vs_engine", ifelse(prod, eng$supply_chain_need, NA),
        ifelse(prod, qc_sens_need(tbl, 0.60, th$band, floor0), NA), numeric = TRUE)
  }
  if (!is.null(nodes)) {
    sv <- sp_strategic_value(tbl, nodes, cfg)
    add("strategic_industrial_value", tbl$strategic_industrial_value, sv$strategic_industrial_value, numeric = TRUE)
    add("siv_cap", tbl$siv_cap, sv$siv_cap)
    ly <- sp_large_yard_case(tbl, nodes, cfg)
    add("large_yard_case", tbl$large_yard_case, ly$large_yard_case)
  }
  do.call(rbind, out)
}

# Run every spec. tbl: strategic_position_tbl (cells); cfg: decision_framework; nodes: the resolved node
# attributes (posture_nodes.R pn_resolve(), as scripts/26) or NULL. Returns list(specs (with status run |
# skipped and note), alt (spec_id -> choice vector), ref (spec_id -> the comparator choice vector: the stored
# choice for table specs, the recomputed configured choice for node-attribute specs), base (the stored choice)).
qc_sens_run <- function(tbl, cfg = list(), nodes = NULL) {
  specs <- qc_sens_specs(cfg)
  th <- sp_choice_thresholds(cfg)
  floor0 <- (cfg$supply_chain_need %||% list())$score_floor %||% 0
  has_unc <- all(c("supply_chain_need_uncapped", "supply_chain_need_score") %in% names(tbl))
  need_at <- function(t, f = floor0) qc_sens_need(tbl, t, th$band, f)
  need0 <- need_at(th$need)
  base <- as.character(tbl$strategic_choice)
  specs$status <- "run"
  specs$note <- NA_character_
  alt <- list()
  ref <- list()
  sv0 <- NULL
  ly0 <- NULL
  if (!is.null(nodes)) {
    sv0 <- sp_strategic_value(tbl, nodes, cfg)
    ly0 <- sp_large_yard_case(tbl, nodes, cfg)
  }
  for (i in seq_len(nrow(specs))) {
    s <- specs[i, ]
    if (s$needs == "node_attributes" && is.null(nodes)) {
      specs$status[i] <- "skipped"
      specs$note[i] <- "node attributes (data/reference/posture_node_attributes.csv) not given"
      next
    }
    if (s$driver %in% c("need", "need_floor") && !has_unc && s$spec_id != "need_060_naive") {
      specs$status[i] <- "skipped"
      specs$note[i] <- "supply_chain_need_uncapped / supply_chain_need_score not in the table: a cap-aware need cannot be computed"
      next
    }
    v <- suppressWarnings(as.numeric(s$value))
    id <- s$spec_id
    r <- base
    a <- switch(id,
      need_minus = , need_plus = , need_060 = {
        c2 <- qc_sens_set(cfg, "need_threshold", v)
        qc_sens_choice(tbl, c2, need = need_at(v))
      },
      need_060_naive = qc_sens_choice(tbl, qc_sens_set(cfg, "need_threshold", v)),
      prize_minus = , prize_plus = , prize_065 = qc_sens_choice(tbl, qc_sens_set(cfg, "prize_threshold", v), need = need0),
      cp_minus = , cp_plus = qc_sens_choice(tbl, qc_sens_set(cfg, "cp_preserve_pct", v), need = need0),
      cp_strict = qc_sens_choice(tbl, cfg, need = need0, cp_strict = TRUE),
      deployment_minus = , deployment_plus = qc_sens_choice(tbl, qc_sens_set(cfg, "deployment_threshold", v), need = need0),
      position_first_off = qc_sens_choice(tbl, qc_sens_set(cfg, c("large_yard", "position_first"), FALSE), need = need0),
      need_floor_005 = qc_sens_choice(tbl, cfg, need = need_at(th$need, v)),
      dn_min_families_2 = qc_sens_choice(tbl, cfg, need = need0, dn_min_families = as.integer(v)),
      ly_min_signals_4 = , ly_min_signals_6 = {
        c2 <- qc_sens_set(cfg, c("large_yard", "min_signals"), as.integer(v))
        r <- qc_sens_choice(tbl, cfg, need = need0, large = ly0$large_yard_case)
        qc_sens_choice(tbl, c2, need = need0, large = sp_large_yard_case(tbl, nodes, c2)$large_yard_case)
      },
      siv_gate_033 = {
        r <- qc_sens_choice(tbl, cfg, need = need0, prize = sv0$strategic_industrial_value, capped = sv0$siv_cap %in% "market_only")
        sv <- sp_strategic_value(tbl, nodes, qc_sens_set(cfg, c("strategic_value", "gate"), v))
        qc_sens_choice(tbl, cfg, need = need0, prize = sv$strategic_industrial_value, capped = sv$siv_cap %in% "market_only")
      },
      siv_unknown_half = {
        r <- qc_sens_choice(tbl, cfg, need = need0, prize = sv0$strategic_industrial_value, capped = sv0$siv_cap %in% "market_only")
        sv <- sp_strategic_value(tbl, qc_sens_unknown_half(nodes), cfg)
        qc_sens_choice(tbl, cfg, need = need0, prize = sv$strategic_industrial_value, capped = sv$siv_cap %in% "market_only")
      },
      stop("qc_sens_run(): no runner for spec ", id, call. = FALSE)
    )
    alt[[id]] <- a
    ref[[id]] <- r
  }
  # A structural spec that gives exactly the choices of a local one (prize 0.65 is also the local prize + band)
  # can never make a cell sensitive on its own: say so rather than let it look like independent evidence.
  changes <- function(id) any(!is.na(alt[[id]]) & !is.na(ref[[id]]) & alt[[id]] != ref[[id]])
  for (i in which(specs$kind == "structural" & specs$status == "run")) {
    if (!changes(specs$spec_id[i])) next
    for (j in which(specs$kind == "local" & specs$status == "run" & specs$driver == specs$driver[i])) {
      if (identical(alt[[specs$spec_id[i]]], alt[[specs$spec_id[j]]]) && identical(ref[[specs$spec_id[i]]], ref[[specs$spec_id[j]]])) {
        specs$note[i] <- paste0("gives the same choices as the local spec ", specs$spec_id[j], ": it cannot make a cell sensitive on its own")
        break
      }
    }
  }
  list(specs = specs, alt = alt, ref = ref, base = base)
}

# The need-floor-cap artifact (docs/qc_audit/audit_posture.md, problem 1): the cells the engine's own threshold
# sensitivity (sp_choice_threshold_sensitivity(), which moves the threshold over the stored, capped need) flips at
# need 0.60 but the cap-aware reading (the uncapped percentile with supply_chain_need.R's cap rule at 0.60) does
# not. run: qc_sens_run() (needs the need_060 and need_060_naive specs run). Returns case_key, iso3, tech, stage,
# stored need, uncapped percentile, need score, cap code, the stored choice, the naive and the cap-aware choice.
qc_sens_need_cap_artifacts <- function(tbl, run) {
  a <- run$alt$need_060_naive
  b <- run$alt$need_060
  if (is.null(a) || is.null(b)) return(NULL)
  i <- which(!is.na(a) & !is.na(b) & a != run$base & b == run$base)
  col <- function(n) if (n %in% names(tbl)) tbl[[n]][i] else rep(NA, length(i))
  data.frame(case_key = paste(tbl$iso3[i], tbl$tech[i], tbl$supply_chain[i], sep = "|"), iso3 = tbl$iso3[i], tech = tbl$tech[i],
             stage = tbl$supply_chain[i], supply_chain_need = col("supply_chain_need"), supply_chain_need_uncapped = col("supply_chain_need_uncapped"),
             supply_chain_need_score = col("supply_chain_need_score"), supply_chain_need_cap = col("supply_chain_need_cap"),
             stored_choice = run$base[i], naive_choice_at_060 = a[i], cap_aware_choice_at_060 = b[i], stringsAsFactors = FALSE)
}

# Per-cell robustness record (design 4.6). run: qc_sens_run(). Returns one row per cell: case_key, iso3,
# Country, tech, stage, baseline_choice, baseline_yard, specs_run, local_flips, structural_flips (";" lists of
# spec_id:alt_choice), fence_change, class, drivers (";" inputs), note (the not_testable reason). The
# diagnostic specs never classify.
qc_sens_classify <- function(tbl, run, cfg = list()) {
  n <- nrow(tbl)
  stage <- tbl$supply_chain
  base <- run$base
  specs <- run$specs[run$specs$status == "run", , drop = FALSE]
  cls_specs <- specs[specs$kind %in% c("local", "structural"), , drop = FALSE]
  base_yard <- sp_yard_of_choice(base, stage)
  th <- sp_choice_thresholds(cfg)
  floor0 <- (cfg$supply_chain_need %||% list())$score_floor %||% 0
  rerun0 <- qc_sens_choice(tbl, cfg, need = qc_sens_need(tbl, th$need, th$band, floor0))
  flips <- lapply(cls_specs$spec_id, function(id) {
    a <- run$alt[[id]]
    r <- run$ref[[id]]
    !is.na(a) & !is.na(r) & a != r
  })
  names(flips) <- cls_specs$spec_id
  listing <- function(i, kind) {
    ids <- cls_specs$spec_id[cls_specs$kind == kind]
    hit <- ids[vapply(ids, function(id) flips[[id]][i], logical(1))]
    if (!length(hit)) NA_character_ else paste0(hit, ":", vapply(hit, function(id) run$alt[[id]][i], character(1)), collapse = ";")
  }
  rec <- lapply(seq_len(n), function(i) {
    lf <- listing(i, "local")
    sf <- listing(i, "structural")
    hit <- cls_specs$spec_id[vapply(cls_specs$spec_id, function(id) flips[[id]][i], logical(1))]
    fence <- any(vapply(hit, function(id) !identical(sp_yard_of_choice(run$alt[[id]][i], stage[i]), base_yard[i]), logical(1)))
    drv <- unique(unname(QC_SENS_DRIVER[cls_specs$driver[match(hit, cls_specs$spec_id)]]))
    note <- NA_character_
    cl <- if (is.na(base[i])) {
      note <- paste0("excluded from the posture", if ("posture_exclusion" %in% names(tbl) && !is.na(tbl$posture_exclusion[i]))
        paste0(" (", tbl$posture_exclusion[i], ")") else "")
      "not_testable"
    } else if (base[i] == "not_assessed") {
      note <- if (stage[i] == "Deployment") "not assessed: deployment need missing" else "not assessed: supply-chain need or prize missing"
      "not_testable"
    } else if (base[i] == "secure") {
      note <- "Secure override of the posture engine (Tests 1-5): not rerun by this sensitivity"
      "not_testable"
    } else if (!identical(rerun0[i], base[i])) {
      note <- paste0("the choice rule from the stored inputs gives ", rerun0[i], ", not the stored ", base[i])
      "not_testable"
    } else if (!is.na(lf)) "fragile" else if (!is.na(sf)) "sensitive" else "stable"
    list(lf = lf, sf = sf, fence = cl %in% c("fragile", "sensitive") && fence, cl = cl,
         drv = if (cl %in% c("fragile", "sensitive") && length(drv)) paste(drv, collapse = ";") else NA_character_, note = note)
  })
  g <- function(f) vapply(rec, function(r) if (is.null(r[[f]])) NA else r[[f]], if (f == "fence") logical(1) else character(1))
  data.frame(
    case_key = paste(tbl$iso3, tbl$tech, stage, sep = "|"), iso3 = tbl$iso3,
    Country = if ("Country" %in% names(tbl)) tbl$Country else NA_character_, tech = tbl$tech, stage = stage,
    baseline_choice = base, baseline_yard = base_yard, specs_run = nrow(cls_specs),
    local_flips = g("lf"), structural_flips = g("sf"), fence_change = g("fence"), class = g("cl"), drivers = g("drv"),
    note = g("note"), stringsAsFactors = FALSE
  )
}

# The long results table (qc_sensitivity_pipeline.csv): one row per cell x spec that changes the choice
# (row_type flip; the diagnostic specs included, marked kind diagnostic) and one class row per cell (row_type
# cell_class). layer is pipeline; data_release names the table the run read.
qc_sens_long <- function(tbl, run, classes, data_release = NA_character_) {
  stage <- tbl$supply_chain
  sp <- run$specs[run$specs$status == "run", , drop = FALSE]
  flips <- do.call(rbind, lapply(seq_len(nrow(sp)), function(j) {
    id <- sp$spec_id[j]
    a <- run$alt[[id]]
    r <- run$ref[[id]]
    i <- which(!is.na(a) & !is.na(r) & a != r)
    if (!length(i)) return(NULL)
    ay <- sp_yard_of_choice(a[i], stage[i])
    by <- sp_yard_of_choice(r[i], stage[i])
    data.frame(row_type = "flip", case_key = classes$case_key[i], iso3 = tbl$iso3[i], tech = tbl$tech[i], stage = stage[i],
               baseline_choice = run$base[i], baseline_yard = sp_yard_of_choice(run$base[i], stage[i]), spec_id = id,
               spec_kind = sp$kind[j], spec_label = sp$label[j], parameter = sp$parameter[j], value = sp$value[j],
               comparator_choice = r[i], alt_choice = a[i], alt_yard = ay, fence_changed = !(by %in% ay & ay %in% by),
               class = classes$class[i], specs_run = classes$specs_run[i], local_flips = NA_character_,
               structural_flips = NA_character_, fence_change = NA, drivers = NA_character_, note = NA_character_,
               stringsAsFactors = FALSE)
  }))
  cells <- data.frame(row_type = "cell_class", case_key = classes$case_key, iso3 = classes$iso3, tech = classes$tech,
                      stage = classes$stage, baseline_choice = classes$baseline_choice, baseline_yard = classes$baseline_yard,
                      spec_id = NA_character_, spec_kind = NA_character_, spec_label = NA_character_, parameter = NA_character_,
                      value = NA_character_, comparator_choice = NA_character_, alt_choice = NA_character_, alt_yard = NA_character_,
                      fence_changed = NA, class = classes$class, specs_run = classes$specs_run, local_flips = classes$local_flips,
                      structural_flips = classes$structural_flips, fence_change = classes$fence_change,
                      drivers = classes$drivers, note = classes$note, stringsAsFactors = FALSE)
  out <- rbind(flips, cells)
  out <- cbind(layer = "pipeline", data_release = data_release, out, stringsAsFactors = FALSE)
  out[order(out$case_key, out$row_type != "cell_class", out$spec_id), , drop = FALSE]
}

# The committed form of the long table (qc/results/qc_sensitivity_pipeline.csv, kept under ~1 MB): every cell x
# spec flip row, and the cell_class rows of the fragile and sensitive cells only. Stable and not-testable cells are
# left out here; qc_sens_cell_classes() keeps one short row per cell for all of them. Pure.
qc_sens_trim <- function(long) {
  long[long$row_type != "cell_class" | long$class %in% c("fragile", "sensitive"), , drop = FALSE]
}

# One short row per cell (qc/results/pipeline_cell_classes.csv): iso3, tech, stage, baseline choice, class and
# whether a spec can change its fence. The per-cell record of every class, including stable and not testable (the
# pipeline release is not repeated on every row: it is in pipeline_robustness_summary.csv and qc/README.md). Pure.
qc_sens_cell_classes <- function(classes) {
  out <- data.frame(iso3 = classes$iso3, tech = classes$tech, stage = classes$stage,
                    baseline_choice = classes$baseline_choice, class = classes$class, fence_change = classes$fence_change,
                    stringsAsFactors = FALSE)
  out[order(out$iso3, out$tech, out$stage), , drop = FALSE]
}

# Class counts (pipeline_robustness_summary.csv): one row per group with n_cells, the four classes and
# fence_change, for group_type overall, tech_stage, choice and neis_economy (QC_SENS_NEIS); then one row per
# spec (group_type spec: kind, status, n_changed, n_fence_changed, note) and per reproduction check
# (group_type reproduction), and with artifacts (qc_sens_need_cap_artifacts()) one diagnostic row naming them.
qc_sens_summary <- function(classes, run = NULL, reproduction = NULL, neis = QC_SENS_NEIS, data_release = NA_character_,
                            artifacts = NULL) {
  count <- function(type, group, idx) {
    cl <- factor(classes$class[idx], QC_SENS_CLASSES)
    t <- table(cl)
    data.frame(group_type = type, group = group, n_cells = length(idx), stable = t[["stable"]], sensitive = t[["sensitive"]],
               fragile = t[["fragile"]], not_testable = t[["not_testable"]], fence_change = sum(classes$fence_change[idx] %in% TRUE),
               spec_kind = NA_character_, status = NA_character_, n_changed = NA_integer_, n_fence_changed = NA_integer_,
               note = NA_character_, stringsAsFactors = FALSE)
  }
  rows <- list(count("overall", "all", seq_len(nrow(classes))))
  ts <- paste(classes$tech, classes$stage, sep = "|")
  for (g in sort(unique(ts))) rows[[length(rows) + 1]] <- count("tech_stage", g, which(ts == g))
  ch <- dplyr::coalesce(classes$baseline_choice, "excluded")
  for (g in unique(c(intersect(SP_CHOICES, ch), setdiff(ch, SP_CHOICES)))) rows[[length(rows) + 1]] <- count("choice", g, which(ch == g))
  for (g in neis) rows[[length(rows) + 1]] <- count("neis_economy", g, which(classes$iso3 == g))
  out <- do.call(rbind, rows)
  blank <- out[0, ]
  if (!is.null(run)) {
    stage <- classes$stage
    sp <- run$specs
    rows <- lapply(seq_len(nrow(sp)), function(j) {
      id <- sp$spec_id[j]
      r <- blank[NA_integer_, ]
      r$group_type <- "spec"
      r$group <- id
      r$spec_kind <- sp$kind[j]
      r$status <- sp$status[j]
      r$note <- dplyr::coalesce(sp$note[j], sp$label[j])
      if (sp$status[j] == "run") {
        a <- run$alt[[id]]
        b <- run$ref[[id]]
        i <- which(!is.na(a) & !is.na(b) & a != b)
        r$n_changed <- length(i)
        r$n_fence_changed <- sum(sp_yard_of_choice(a[i], stage[i]) != sp_yard_of_choice(b[i], stage[i]) |
                                   xor(is.na(sp_yard_of_choice(a[i], stage[i])), is.na(sp_yard_of_choice(b[i], stage[i]))), na.rm = TRUE)
      }
      r
    })
    out <- rbind(out, do.call(rbind, rows))
  }
  if (!is.null(reproduction) && nrow(reproduction)) {
    rows <- lapply(seq_len(nrow(reproduction)), function(j) {
      r <- blank[NA_integer_, ]
      r$group_type <- "reproduction"
      r$group <- reproduction$check[j]
      r$n_cells <- reproduction$n_compared[j]
      r$status <- if (reproduction$n_mismatch[j] == 0) "reproduced" else "mismatch"
      r$n_changed <- reproduction$n_mismatch[j]
      r$note <- reproduction$examples[j]
      r
    })
    out <- rbind(out, do.call(rbind, rows))
  }
  if (!is.null(artifacts)) {
    r <- blank[NA_integer_, ]
    r$group_type <- "diagnostic"
    r$group <- "need_cap_artifact_at_060"
    r$n_changed <- nrow(artifacts)
    r$status <- if (nrow(artifacts)) "artifact_present" else "none"
    r$note <- paste0("cells the engine's own sensitivity flips at need 0.60 only because the capped need is stored as ",
                     "need - band - 0.01; the cap-aware reading keeps them",
                     if (nrow(artifacts)) paste0(": ", paste(artifacts$case_key, collapse = "; ")) else "")
    out <- rbind(out, r)
  }
  rownames(out) <- NULL
  cbind(layer = "pipeline", data_release = data_release, out, stringsAsFactors = FALSE)
}
