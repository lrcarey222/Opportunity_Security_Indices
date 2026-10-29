# Parity fixtures for the TypeScript engine of the Allied Industrial Base Explorer (pure; no IO).
# scripts/86_export_dashboard_fixtures.R runs the R reference engine (R/dashboard) on a bundle for
# the fixed scenarios below and encodes, per scenario, the output of every layer (spec_react.md
# section 5); dashboard/src/engine/__tests__/parity replays the same scenarios in TypeScript and
# compares layer by layer.
#
#   dash_fixture_scenarios(bundle)      the 17 scenario specs (dials and advanced settings; category
#                                       names, the PSI view and the endowment screen are taken from
#                                       the bundle)
#   dash_fixture_tie_bundle(bundle)     the synthetic bundle with inputs rounded to a few distinct
#                                       values, so the S-curves and ranks see heavy ties as the
#                                       real data do
#   dash_fixture_baseline(bundle)       the baseline result every scenario is compared with
#   dash_fixture_outputs(...)           one scenario's layer outputs (a nested list for
#                                       dash_osb_encode(kind = "fixture"))
#   dash_fixture_refs(...)              leaves identical to the reference scenario's -> reference
#                                       nodes (keeps the real set small)

# The original allied network design (L5: the greedy network and its scenarios S15 and S16's
# network settings) is retired (2026-10-17); S15 is gone and S16 keeps only its dial.

dash_fixture_dials <- function(H = 50, B = 50, P = 50, G = 50) {
  list(hawkishness = H, bullishness = B, populism = P, greenness = G)
}

# The scenario list of spec_react.md 5.2. Each element: id, label, dials (named list), advanced (a
# list for dash_params(); empty = the defaults), exercises (text).
dash_fixture_scenarios <- function(bundle) {
  bi <- dash_baseline_inputs(bundle)
  d <- dash_fixture_dials
  other_view <- if (identical(bi$psi_view, "world")) "coalition" else "world"
  # S13 flips the rankability rule against the baseline: on (tau 0.5) when the baseline has it off,
  # off (tau NA) when the baseline applies it, so both pillar paths are exercised on every bundle
  other_tau <- if (is.null(bi$tau) || (length(bi$tau) == 1 && is.na(bi$tau))) 0.5 else NA_real_
  s13_label <- if (is.na(other_tau)) "Other PSI view, rankability rule off" else "Other PSI view, tau 0.5"
  # S17 flips the endowment screen of the allied picks against the baseline (off when it is on)
  screen_other <- isFALSE(bi$require_resource_base)
  es <- bi$es_weights
  eo <- bi$eo_weights
  es_adv <- es[0]
  if (length(es) >= 1) es_adv[names(es)[1]] <- 0
  if (length(es) >= 2) es_adv[names(es)[2]] <- es[[2]] * 2
  eo_adv <- eo[0]
  if (length(eo) >= 3) eo_adv[names(eo)[3]] <- 0.5
  if (length(eo) >= 4) eo_adv[names(eo)[4]] <- 10 else if (length(eo) >= 1) eo_adv[names(eo)[1]] <- 10
  sc <- function(id, label, dials, advanced = list(), exercises = "") {
    list(id = id, label = label, dials = dials, advanced = advanced, exercises = exercises)
  }
  list(
    sc("S00", "NEIS baseline", d(),exercises = "the baseline; also checked against the shipped baseline tables"),
    sc("S01", "Hawkishness 0", d(H = 0), exercises = "SI need weight, PSI FS share, geo and Chinese-parent ownership weights (down)"),
    sc("S02", "Hawkishness 100", d(H = 100), exercises = "the same, up"),
    sc("S03", "Bullishness 0", d(B = 0), exercises = "SI EO weight, PSI PO share (down)"),
    sc("S04", "Bullishness 100", d(B = 100), exercises = "the same, up"),
    sc("S05", "Populism 0", d(P = 0), exercises = "SI policy weight, IEA share, stage contrast narrowed"),
    sc("S06", "Populism 100", d(P = 100), exercises = "SI policy weight and IEA share up, stage contrast widened"),
    sc("S07", "Greenness 0", d(G = 0), exercises = "SI technology weight, CAT share, climate penalties x0.25"),
    sc("S08", "Greenness 100", d(G = 100), exercises = "penalties x4, fossil exclusion (auto) in the SI and best-five"),
    sc("S09", "Greenness 85, fossil always", d(G = 85), list(fossil = "always"), "exclusion below the threshold"),
    sc("S10", "Greenness 100, fossil never", d(G = 100), list(fossil = "never"), "maximum penalties with fossil technologies kept"),
    sc("S11", "All four dials", d(80, 30, 70, 80), exercises = "all dials at once (no exclusion)"),
    sc("S12", "Off-grid dials", d(37, 63, 41, 59), exercises = "integer dials off the slider's 5-step (dial grid lookups)"),
    sc("S13", s13_label, d(), list(psi_view = other_view, tau = other_tau), "PSI viewpoint; the rankability rule flipped (on <-> off)"),
    sc("S14", "Category weights, tau 0.3", d(), list(es_weights = es_adv, eo_weights = eo_adv, tau = 0.3),
       "pillar recompute, IWS, rankable, SI resample, degenerate rebuild, partner layer uses baseline pillars"),
    sc("S16", "Hawkishness 70", d(H = 70), exercises = "an intermediate hawkishness multiplier (x1.74)"),
    sc("S17", if (screen_other) "Endowment screen on" else "Endowment screen off", d(), list(require_resource_base = screen_other),
       "the allied picks with the endowment screen flipped (Extraction stages without a resource base picked or barred)")
  )
}

# The synthetic bundle with its inputs rounded to `digits` decimals (pillar category scores, policy
# values, partner components, development), so ties are everywhere.
dash_fixture_tie_bundle <- function(bundle, digits = 1) {
  rnd <- function(x) if (is.null(x)) NULL else {
    y <- x
    y[] <- round(x, digits)
    y
  }
  for (p in c("es", "eo")) bundle$pillars[[p]]$score <- rnd(bundle$pillars[[p]]$score)
  bundle$policy$values <- rnd(bundle$policy$values)
  for (k in c("imp_trade", "econ_opp_raw", "es_need", "eo_partner", "outbound", "outbound_cmfdi", "cpe", "geo")) bundle$partner$fs[[k]] <- rnd(bundle$partner$fs[[k]])
  for (k in c("trade", "econ_opp_index", "partner_es_need")) bundle$partner$po[[k]] <- rnd(bundle$partner$po[[k]])
  bundle$partner$dp$development <- rnd(bundle$partner$dp$development)
  bundle$manifest$notes <- c(bundle$manifest$notes, paste0("Inputs rounded to ", digits, " decimal(s) (dash_fixture_tie_bundle()): heavy ties."))
  bundle
}

# The baseline every scenario is compared with: dash_run() at the baseline (partner layer on).
dash_fixture_baseline <- function(bundle) {
  params <- bundle$manifest$baseline
  if (is.null(params)) params <- dash_params(bundle = bundle)
  res <- dash_run(bundle, params)
  list(params = params, result = res)
}

# Per-cell statistics of a dyad index array [U, U, C] (sums with sum(), i.e. long double).
dash_fixture_cell_stats <- function(arr) {
  C <- dim(arr)[3]
  rows <- lapply(seq_len(C), function(c) {
    v <- arr[, , c]
    v <- v[!is.na(v)]
    tibble::tibble(
      cell_id = c, n_non_na = length(v),
      sum = if (length(v)) sum(v) else NA_real_,
      sum_sq = if (length(v)) sum(v^2) else NA_real_,
      min = if (length(v)) min(v) else NA_real_,
      max = if (length(v)) max(v) else NA_real_,
      n_distinct = length(unique(v))
    )
  })
  dplyr::bind_rows(rows)
}

# Values of a dyad index array at `n` positions drawn with set.seed(seed) among the mask-true cells
# (r, p, c 1-based).
dash_fixture_cell_sample <- function(arr, mask, n = 20000L, seed = 86L) {
  old <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) get(".Random.seed", envir = globalenv()) else NULL
  on.exit({
    if (is.null(old)) {
      if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
    } else {
      assign(".Random.seed", old, envir = globalenv())
    }
  }, add = TRUE)
  set.seed(seed)
  idx <- which(as.vector(mask))
  s <- if (length(idx) > n) sort(sample(idx, n)) else idx
  ai <- arrayInd(s, dim(arr))
  tibble::tibble(r = as.integer(ai[, 1]), p = as.integer(ai[, 2]), c = as.integer(ai[, 3]), value = as.vector(arr)[s])
}

# The dyads decomposed in L4.decompose: the first coalition reporter with its top-ranked and its
# lowest-ranked PSI partner and (when n_dyads > 2) a reporter outside the coalition with its top
# partner.
dash_fixture_decompose_dyads <- function(bundle, psi_dyad, n_dyads = 3L) {
  U <- bundle$partner$U
  coal <- intersect(bundle$partner$psi$coalition_reporters, U)
  pick <- function(rep, best = TRUE) {
    d <- psi_dyad[psi_dyad$reporter_iso == rep & !is.na(psi_dyad$psi), , drop = FALSE]
    if (!nrow(d)) return(NULL)
    o <- order(if (best) -d$psi else d$psi, d$partner_iso, method = "radix")
    c(rep, d$partner_iso[o[1]])
  }
  out <- list()
  r1 <- if (length(coal)) coal[1] else U[1]
  out[[1]] <- pick(r1, TRUE)
  out[[2]] <- pick(r1, FALSE)
  if (n_dyads > 2) {
    other <- setdiff(U, coal)
    other <- other[other %in% psi_dyad$reporter_iso]
    if (length(other)) out[[3]] <- pick(other[1], TRUE)
  }
  out <- Filter(Negate(is.null), out)
  tibble::tibble(reporter_iso = vapply(out, `[`, "", 1), partner_iso = vapply(out, `[`, "", 2))
}

# The reporters whose PSI dyads a fixture keeps when it stores a subset of the dyad table: about
# n/2 coalition reporters and n/2 others, evenly spaced in partner order, C-sorted.
dash_fixture_dyad_reporters <- function(bundle, n = 24L) {
  U <- bundle$partner$U
  coal <- intersect(bundle$partner$psi$coalition_reporters, U)
  other <- setdiff(U, coal)
  k1 <- min(length(coal), ceiling(n / 2))
  k2 <- min(length(other), n - k1)
  pick <- function(v, k) if (k <= 0 || !length(v)) character() else v[unique(round(seq(1, length(v), length.out = k)))]
  sort(unique(c(pick(coal, k1), pick(other, k2))), method = "radix")
}

# Leaves of a scenario's outputs that are identical() to the reference scenario's leaf at the same
# path become a reference node, json c(ref_scenario = <id>, ref_path = <dotted path>), which the
# TypeScript fixture reader resolves (spec_react.md 5.3, optional size saving). Only leaves of at
# least `min_bytes` are replaced.
dash_fixture_refs <- function(objects, ref_objects, ref_id, min_bytes = 4096, path = NULL) {
  if (is.null(ref_objects) || !is.list(objects)) return(objects)
  is_branch <- function(x) is.list(x) && !is.data.frame(x) && !inherits(x, "dash_osb_json") && length(x) > 0
  for (nm in names(objects)) {
    x <- objects[[nm]]
    y <- ref_objects[[nm]]
    if (is.null(x) || is.null(y)) next
    p <- if (is.null(path)) nm else paste(path, nm, sep = ".")
    if (is_branch(x) && is_branch(y)) {
      objects[[nm]] <- dash_fixture_refs(x, y, ref_id, min_bytes, p)
    } else if (!is_branch(x) && identical(x, y) && as.numeric(utils::object.size(x)) >= min_bytes) {
      objects[[nm]] <- dash_osb_json(c(ref_scenario = ref_id, ref_path = p))
    }
  }
  objects
}

# Number of reference nodes in a scenario's outputs.
dash_fixture_count_refs <- function(objects) {
  if (inherits(objects, "dash_osb_json")) return(as.integer(!is.null(names(objects$value)) && "ref_scenario" %in% names(objects$value)))
  if (!is.list(objects) || is.data.frame(objects)) return(0L)
  sum(vapply(objects, dash_fixture_count_refs, integer(1)))
}

# One scenario's outputs (spec_react.md 5.3). base = dash_fixture_baseline(bundle). opts:
# full_arrays (the FS / PO index arrays in full), contrib, var, sample (20,000 FS / PO cells)
# (logicals), n_decompose (dyads decomposed), dyad_reporters (NULL: the full PSI dyad table; n: the
# dyads of dash_fixture_dyad_reporters(bundle, n) only). Returns list(objects = <nested list>,
# timings = named numeric (seconds per layer)).
dash_fixture_outputs <- function(bundle, scenario, base, opts = list()) {
  o <- utils::modifyList(list(full_arrays = TRUE, contrib = TRUE, var = TRUE, sample = FALSE, n_decompose = 2L,
                              dyad_reporters = NULL), opts)
  timings <- numeric()
  tick <- function(name, expr) {
    t0 <- proc.time()[["elapsed"]]
    val <- force(expr)
    timings[name] <<- proc.time()[["elapsed"]] - t0
    val
  }
  params <- dash_params(scenario$dials, scenario$advanced, bundle = bundle)
  res <- tick("L1-L4", dash_run(bundle, params))
  out <- list(
    params = dash_osb_json(dash_params_json(params)),
    readout = dash_dial_readout(params, bundle),
    notes = dash_osb_json(I(as.character(attr(res, "notes"))))
  )

  # L1
  L1 <- list(es = res$pillars$es, eo = res$pillars$eo, es_cw = res$pillars$es_category_weight, eo_cw = res$pillars$eo_category_weight)
  if (isTRUE(o$contrib)) {
    L1$es_contrib <- res$pillars$es_contrib
    L1$eo_contrib <- res$pillars$eo_contrib
  }
  if (isTRUE(o$var)) {
    var_rows <- function(p) {
      cs <- sort(unique(bundle$pillars[[p]]$cells$Country), method = "radix")
      pick <- unique(cs[c(1, ceiling(length(cs) / 2), length(cs))])
      dplyr::bind_rows(lapply(pick, function(ctry) dash_variable_contrib(bundle, res$pillars, p, ctry)))
    }
    L1$var <- list(es = var_rows("es"), eo = var_rows("eo"))
  }
  out$L1 <- L1

  # L2, L3
  out$L2 <- list(index = res$policy$index, eligibility = res$policy$eligibility, weights = dash_osb_json(res$policy$weights))
  out$L3 <- list(cells = res$si$cells, excluded = res$si$excluded, degenerate = res$si$degenerate,
                 weights = dash_osb_json(res$si$weights), stage_weights = dash_osb_json(res$si$stage_weights), allied = res$allied)

  # L4
  cells <- res$partner$cells
  top5 <- res$partner$top5
  L4 <- list()
  if (isTRUE(o$full_arrays)) {
    L4$fs_index <- unname(cells$fs_index)
    L4$po_index <- unname(cells$po_index)
  }
  L4$fs_stats <- dash_fixture_cell_stats(cells$fs_index)
  L4$po_stats <- dash_fixture_cell_stats(cells$po_index)
  if (isTRUE(o$sample)) {
    L4$fs_sample <- dash_fixture_cell_sample(cells$fs_index, bundle$partner$fs$mask)
    L4$po_sample <- dash_fixture_cell_sample(cells$po_index, bundle$partner$po$mask)
  }
  L4$top5_fs <- unname(top5$fs)
  L4$top5_po <- unname(top5$po)
  L4$fs_exists <- unname(top5$fs_exists)
  L4$po_exists <- unname(top5$po_exists)
  L4$psi_dyad <- res$partner$psi$dyad
  if (!is.null(o$dyad_reporters)) {
    reps <- dash_fixture_dyad_reporters(bundle, o$dyad_reporters)
    L4$psi_dyad <- L4$psi_dyad[L4$psi_dyad$reporter_iso %in% reps, , drop = FALSE]
    L4$psi_dyad_reporters <- I(reps)
  }
  L4$psi_partner <- res$partner$psi$partner
  L4$psi_reporters <- I(as.character(res$partner$psi$reporters))
  L4$psi_view <- dash_osb_json(I(res$partner$psi$view))
  dy <- dash_fixture_decompose_dyads(bundle, res$partner$psi$dyad, o$n_decompose)
  L4$decompose_dyads <- dy
  L4$decompose <- dplyr::bind_rows(lapply(seq_len(nrow(dy)), function(i) {
    dash_partner_decompose(bundle, params, cells, dy$reporter_iso[i], dy$partner_iso[i])
  }))
  out$L4 <- L4

  # Compare with the baseline
  cmp <- dash_compare(res, base$result)
  out$compare <- cmp[intersect(c("si", "es", "eo", "policy", "allied", "psi_partner"), names(cmp))]
  list(objects = out, timings = timings, params = params)
}
