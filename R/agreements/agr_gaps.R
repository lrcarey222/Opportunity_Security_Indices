# Agreement gaps (docs/partnership_agreements.md, section 8). Pure.
#
#   agr_gaps(pairs, canonical, canonical_codes, position_tbl, compat_tbl, theses, cfg, alignment_tbl)
#     -> list(gaps, execution_gaps)
#
# Complementarity and coverage are evaluated per technology x stage cell, then reported per
# directed (need_iso3, supplier_iso3, tech):
#   - a cell is complementary when the supplier is a recommended partner of the need side in it, or
#     the need side's need percentile >= need_min while the supplier's competitive position
#     percentile >= capability_min in it; it counts only when the two economies' UN-vote alignment is
#     at least min_alignment (unknown alignment does not block);
#   - an agreement covers the cells of the technologies and stages its sectors map to (config
#     sectors: techs, stages; all six stages when a sector lists none), at the sector's grade
#     (specific or text, R/agreements/agr_pairs.R agr_agreement_grades());
#   - covered_grade is the best grade at the complementary stages (stages_high) when there are any,
#     else at any stage. A critical-minerals framework therefore covers battery refining, not
#     battery assembly.
# Rows: every (need, supplier, tech) with high complementarity, plus every one an agreement of the pair
# covers (any grade), where both sides are in the scored universe. complementarity is "not_assessed"
# (quadrant none) when the need side has no need or the supplier no capability measured. Every component is kept; the
# quadrant is a labelled rule over them, not a score.
# execution_gaps: per (iso3, tech), the priority stages (policy priority percentile >= priority_min
# within the economy, or a strategic choice in priority_choices) where the economy has
# high-complementarity suppliers and no agreement with any of them covers the stage at a specific
# grade.
#
# Inputs (data frames, only these columns are read):
#   pairs          agr_pair_profiles(): pair, a, b, n_agreements, depth_all, depth_ei, agreement_ids
#                  (via-EU agreements included).
#   canonical      canonical agreements: canonical_agreement_id, implementation_status; optional active
#                  and eligible flags (agr_agreement_flags()).
#   canonical_codes  canonical_agreement_id, dimension, code, basis, n_hits.
#   position_tbl   strategic_position_tbl: iso3, tech, supply_chain, supply_chain_need,
#                  deployment_need_pct (or deployment_need), competitive_position_percentile,
#                  recommended_partner_1..3, strategic_choice, policy_priority_percentile.
#   compat_tbl     partner_compat_cells_tbl: reporter_iso (importer, the need side), partner_iso
#                  (supplier), tech, supply_chain, friendshore. May be NULL.
#   theses         AIB relationship theses: a, b, priority_compacts. May be NULL.
#   alignment_tbl  geo_alignment_tbl: reporter_iso, partner_iso, geo_alignment. May be NULL.

AGR_GAP_COLUMNS <- c("need_iso3", "supplier_iso3", "tech", "pair", "need", "capability", "recommended",
                     "n_cells_high", "alignment", "alignment_blocked", "stages_high", "friendshore", "aib_shared",
                     "complementarity", "covered_grade", "stages_covered", "covering_agreements", "n_agreements",
                     "depth_all", "depth_ei", "architecture", "quadrant")

AGR_EXEC_GAP_COLUMNS <- c("iso3", "tech", "stages", "priority_basis", "priority", "n_suppliers",
                          "suppliers_without_coverage", "suppliers_text_coverage")

AGR_GAP_STAGES <- c("Extraction", "Refining", "Components", "Assembly", "Deployment", "Recovery")

# Max of a numeric vector by a key, NA when every value is NA (named by key).
agr_max_by <- function(x, key) {
  v <- suppressWarnings(tapply(x, key, max, na.rm = TRUE))
  v[!is.finite(v)] <- NA_real_
  v
}

# "|"-join stages in supply-chain order.
agr_join_stages <- function(x) {
  x <- unique(x[!is.na(x) & nzchar(x)])
  paste(x[order(match(x, AGR_GAP_STAGES))], collapse = "|")
}

# The per-cell need: supply_chain_need, or the deployment need percentile at Deployment.
agr_cell_need <- function(pos) {
  dep <- if ("deployment_need_pct" %in% names(pos)) pos$deployment_need_pct else rep(NA_real_, nrow(pos))
  if ("deployment_need" %in% names(pos)) dep <- ifelse(is.na(dep), pos$deployment_need, dep)
  scn <- if ("supply_chain_need" %in% names(pos)) pos$supply_chain_need else rep(NA_real_, nrow(pos))
  as.numeric(ifelse(pos$supply_chain == "Deployment", dep, scn))
}

# Complementary cells: data.frame(need_iso3, supplier_iso3, tech, stage, rec, hi), one row per cell.
agr_gap_cells <- function(pos, cfg) {
  gc <- cfg$gaps
  need_min <- as.numeric(gc$need_min)
  cap_min <- as.numeric(gc$capability_min)
  cell <- paste(pos$tech, pos$supply_chain, sep = "\r")
  hi <- lapply(split(seq_len(nrow(pos)), cell), function(ix) {
    p <- pos[ix, , drop = FALSE]
    n <- unique(p$iso3[!is.na(p$need) & p$need >= need_min])
    s <- unique(p$iso3[!is.na(p$capability) & p$capability >= cap_min])
    if (!length(n) || !length(s)) return(NULL)
    g <- expand.grid(need_iso3 = n, supplier_iso3 = s, stringsAsFactors = FALSE)
    g <- g[g$need_iso3 != g$supplier_iso3, , drop = FALSE]
    if (!nrow(g)) return(NULL)
    g$tech <- p$tech[1]
    g$stage <- p$supply_chain[1]
    g
  })
  hi <- do.call(rbind, hi[!vapply(hi, is.null, logical(1))])
  empty <- data.frame(need_iso3 = character(), supplier_iso3 = character(), tech = character(), stage = character(),
                      stringsAsFactors = FALSE)
  if (is.null(hi)) hi <- empty
  rp <- intersect(paste0("recommended_partner_", 1:3), names(pos))
  rec <- do.call(rbind, lapply(rp, function(col) {
    v <- as.character(pos[[col]])
    ok <- !is.na(v) & nzchar(v) & v != pos$iso3
    data.frame(need_iso3 = pos$iso3[ok], supplier_iso3 = v[ok], tech = pos$tech[ok], stage = pos$supply_chain[ok],
               stringsAsFactors = FALSE)
  }))
  if (is.null(rec)) rec <- empty
  k_hi <- paste(hi$need_iso3, hi$supplier_iso3, hi$tech, hi$stage, sep = "|")
  k_rec <- paste(rec$need_iso3, rec$supplier_iso3, rec$tech, rec$stage, sep = "|")
  keys <- unique(c(k_hi, k_rec))
  if (!length(keys)) return(cbind(empty, rec = logical(), hi = logical()))
  kk <- do.call(rbind, strsplit(keys, "|", fixed = TRUE))
  data.frame(need_iso3 = kk[, 1], supplier_iso3 = kk[, 2], tech = kk[, 3], stage = kk[, 4],
             rec = keys %in% k_rec, hi = keys %in% k_hi, stringsAsFactors = FALSE)
}

# Alignment of directed (a, b) from geo_alignment_tbl (symmetric; either orientation), NA if unknown.
agr_gap_alignment <- function(a, b, alignment_tbl) {
  if (is.null(alignment_tbl) || !nrow(alignment_tbl)) return(rep(NA_real_, length(a)))
  k <- paste(alignment_tbl$reporter_iso, alignment_tbl$partner_iso, sep = "|")
  v <- as.numeric(alignment_tbl$geo_alignment)
  out <- v[match(paste(a, b, sep = "|"), k)]
  miss <- is.na(out)
  out[miss] <- v[match(paste(b, a, sep = "|"), k)][miss]
  out
}

# Cell coverage: data.frame(pair, tech, stage, grade, agreements) with the best grade over the pair's
# live agreements (not cancelled / inactive; active and depth-eligible where flagged) and the
# agreements that cover the cell at that grade or any grade ("|"-joined, all grades).
agr_gap_cell_cover <- function(pairs, canonical, canonical_codes, cfg) {
  empty <- data.frame(pair = character(), tech = character(), stage = character(), grade = character(),
                      agreements = character(), stringsAsFactors = FALSE)
  if (!nrow(pairs) || !nrow(canonical) || !nrow(canonical_codes)) return(empty)
  ok <- !(canonical$implementation_status %in% c("cancelled", "inactive"))
  if ("active" %in% names(canonical)) ok <- ok & canonical$active %in% TRUE
  if ("eligible" %in% names(canonical)) ok <- ok & canonical$eligible %in% TRUE
  live <- canonical$canonical_agreement_id[ok]
  cc <- canonical_codes
  if (!"n_hits" %in% names(cc)) cc$n_hits <- 1L
  sec <- agr_agreement_grades(cc, cfg)$sectors
  sec <- sec[sec$canonical_agreement_id %in% live, , drop = FALSE]
  if (!nrow(sec)) return(empty)
  st <- agr_sector_table(cfg)
  cells <- do.call(rbind, lapply(seq_len(nrow(st)), function(i) {
    t <- agr_split(st$techs[i])[[1]]
    if (!length(t)) return(NULL)
    s <- agr_split(st$stages[i])[[1]]
    if (!length(s)) s <- AGR_GAP_STAGES
    expand.grid(sector = st$sector[i], tech = t, stage = s, stringsAsFactors = FALSE)
  }))
  ac <- merge(sec, cells, by = "sector")
  if (!nrow(ac)) return(empty)
  ids <- agr_split(pairs$agreement_ids)
  pa <- data.frame(pair = rep(pairs$pair, lengths(ids)), canonical_agreement_id = unlist(ids), stringsAsFactors = FALSE)
  pc <- merge(pa, ac[, c("canonical_agreement_id", "tech", "stage", "grade")], by = "canonical_agreement_id")
  if (!nrow(pc)) return(empty)
  key <- paste(pc$pair, pc$tech, pc$stage, sep = "\r")
  best <- tapply(pc$grade == "specific", key, any)
  ags <- tapply(pc$canonical_agreement_id, key, function(v) paste(sort(unique(v)), collapse = "|"))
  kk <- do.call(rbind, strsplit(names(best), "\r", fixed = TRUE))
  data.frame(pair = kk[, 1], tech = kk[, 2], stage = kk[, 3], grade = ifelse(as.logical(best), "specific", "text"),
             agreements = unname(ags[names(best)]), stringsAsFactors = FALSE)
}

agr_gaps <- function(pairs, canonical, canonical_codes, position_tbl, compat_tbl = NULL, theses = NULL, cfg,
                     alignment_tbl = NULL) {
  gc <- cfg$gaps
  min_al <- if (is.null(gc$min_alignment)) 0 else as.numeric(gc$min_alignment)
  pos <- as.data.frame(position_tbl, stringsAsFactors = FALSE)
  pos$iso3 <- as.character(pos$iso3)
  pos$tech <- as.character(pos$tech)
  pos$supply_chain <- as.character(pos$supply_chain)
  pos$need <- agr_cell_need(pos)
  pos$capability <- as.numeric(pos$competitive_position_percentile)
  universe <- sort(unique(pos$iso3))
  # Supply relationships only: Deployment is demand context (installing and using equipment), so a
  # deployment leader does not "supply" a deployer. supply_stages (config) bounds complementarity,
  # need and capability, and the execution gaps' priority cells.
  supply_stages <- unlist(gc$supply_stages %||% as.list(setdiff(AGR_GAP_STAGES, "Deployment")))
  pos <- pos[pos$supply_chain %in% supply_stages, , drop = FALSE]
  it <- paste(pos$iso3, pos$tech, sep = "|")
  need_by <- agr_max_by(pos$need, it)
  cap_by <- agr_max_by(pos$capability, it)

  cells <- agr_gap_cells(pos, cfg)
  cells$alignment <- agr_gap_alignment(cells$need_iso3, cells$supplier_iso3, alignment_tbl)
  cells$aligned <- is.na(cells$alignment) | cells$alignment >= min_al
  cells$k3 <- paste(cells$need_iso3, cells$supplier_iso3, cells$tech, sep = "|")
  cover <- agr_gap_cell_cover(pairs, canonical, canonical_codes, cfg)
  cov_grade <- stats::setNames(cover$grade, paste(cover$pair, cover$tech, cover$stage, sep = "\r"))
  cov_ags <- stats::setNames(cover$agreements, names(cov_grade))

  # Row keys: aligned complementarity, plus coverage (both directions) with one side in the universe.
  k_high <- unique(cells$k3[cells$aligned])
  pab <- strsplit(cover$pair, "|", fixed = TRUE)
  ca <- vapply(pab, `[`, "", 1)
  cb <- vapply(pab, `[`, "", 2)
  # Both sides scored: a pair with the EU (EUU) or an unscored territory has no need or capability.
  in_u <- ca %in% universe & cb %in% universe
  k_cov <- unique(c(paste(ca, cb, cover$tech, sep = "|")[in_u], paste(cb, ca, cover$tech, sep = "|")[in_u]))
  keys <- sort(unique(c(k_high, k_cov)), method = "radix")

  if (!length(keys)) {
    gaps <- as.data.frame(stats::setNames(replicate(length(AGR_GAP_COLUMNS), character(), simplify = FALSE), AGR_GAP_COLUMNS))
    return(list(gaps = gaps, execution_gaps = agr_execution_gaps(cells, cov_grade, pos, cfg)))
  }
  kk <- do.call(rbind, strsplit(keys, "|", fixed = TRUE))
  gaps <- data.frame(need_iso3 = kk[, 1], supplier_iso3 = kk[, 2], tech = kk[, 3], stringsAsFactors = FALSE)
  gaps$pair <- agr_pair_key(gaps$need_iso3, gaps$supplier_iso3)
  gaps$need <- round(unname(need_by[paste(gaps$need_iso3, gaps$tech, sep = "|")]), 4)
  gaps$capability <- round(unname(cap_by[paste(gaps$supplier_iso3, gaps$tech, sep = "|")]), 4)
  cnt <- function(flag) {
    v <- table(cells$k3[flag])
    r <- as.integer(v[keys]); r[is.na(r)] <- 0L; r
  }
  gaps$recommended <- cnt(cells$rec)
  gaps$n_cells_high <- cnt(cells$hi)
  gaps$alignment <- agr_gap_alignment(gaps$need_iso3, gaps$supplier_iso3, alignment_tbl)
  gaps$alignment_blocked <- (gaps$recommended + gaps$n_cells_high) > 0L & !is.na(gaps$alignment) & gaps$alignment < min_al
  hs <- tapply(cells$stage[cells$aligned], cells$k3[cells$aligned], agr_join_stages)
  sh <- unname(hs[keys]); sh[is.na(sh)] <- ""
  gaps$stages_high <- sh

  # Friendshore: max over the technology's cells of the directed dyad (importer = need side).
  gaps$friendshore <- NA_real_
  if (!is.null(compat_tbl) && nrow(compat_tbl)) {
    isos <- sort(unique(c(gaps$need_iso3, gaps$supplier_iso3)))
    techs <- sort(unique(gaps$tech))
    ia <- match(as.character(compat_tbl$reporter_iso), isos)
    ib <- match(as.character(compat_tbl$partner_iso), isos)
    itc <- match(as.character(compat_tbl$tech), techs)
    fs <- as.numeric(compat_tbl$friendshore)
    ok <- !is.na(ia) & !is.na(ib) & !is.na(itc) & !is.na(fs)
    nk <- (ia[ok] * (length(isos) + 1) + ib[ok]) * (length(techs) + 1) + itc[ok]
    fs <- fs[ok]
    o <- order(nk, -fs)
    nk <- nk[o]; fs <- fs[o]
    first <- !duplicated(nk)
    gk <- (match(gaps$need_iso3, isos) * (length(isos) + 1) + match(gaps$supplier_iso3, isos)) * (length(techs) + 1) +
      match(gaps$tech, techs)
    gaps$friendshore <- round(fs[first][match(gk, nk[first])], 4)
  }
  gaps$aib_shared <- ""
  if (!is.null(theses) && nrow(theses) && "priority_compacts" %in% names(theses)) {
    pc <- vapply(strsplit(agr_chr(theses$priority_compacts), "[ ,;|]+"), agr_join, character(1))
    tk <- agr_pair_key(theses$a, theses$b)
    s2 <- unname(pc[match(gaps$pair, tk)])
    s2[is.na(s2)] <- ""
    gaps$aib_shared <- s2
  }
  # high: aligned complementary stages; not_assessed: no need or no capability measured for the
  # technology (the rule cannot say "low"); low otherwise.
  gaps$complementarity <- ifelse(nzchar(gaps$stages_high), "high",
                                 ifelse(is.na(gaps$need) | is.na(gaps$capability), "not_assessed", "low"))

  # Coverage at the relevant stages: the complementary stages when there are any, else every supply stage.
  rel <- ifelse(nzchar(gaps$stages_high), gaps$stages_high, paste(supply_stages, collapse = "|"))
  rel_st <- strsplit(rel, "|", fixed = TRUE)
  grade_at <- function(i, stages) unname(cov_grade[paste(gaps$pair[i], gaps$tech[i], stages, sep = "\r")])
  gaps$covered_grade <- vapply(seq_len(nrow(gaps)), function(i) {
    g <- grade_at(i, rel_st[[i]])
    if (any(g == "specific", na.rm = TRUE)) "specific" else if (any(g == "text", na.rm = TRUE)) "text" else "none"
  }, character(1))
  gaps$stages_covered <- vapply(seq_len(nrow(gaps)), function(i) {
    g <- grade_at(i, supply_stages)
    agr_join_stages(supply_stages[!is.na(g) & g == "specific"])
  }, character(1))
  gaps$covering_agreements <- vapply(seq_len(nrow(gaps)), function(i) {
    a <- unname(cov_ags[paste(gaps$pair[i], gaps$tech[i], rel_st[[i]], sep = "\r")])
    agr_join(unlist(strsplit(a[!is.na(a)], "|", fixed = TRUE)))
  }, character(1))
  pi <- match(gaps$pair, pairs$pair)
  gaps$n_agreements <- as.integer(ifelse(is.na(pi), 0L, pairs$n_agreements[pi]))
  gaps$depth_all <- as.integer(ifelse(is.na(pi), 0L, pairs$depth_all[pi]))
  gaps$depth_ei <- as.integer(ifelse(is.na(pi), 0L, pairs$depth_ei[pi]))
  strong <- gaps$covered_grade == "specific"
  if (!is.null(gc$strong_depth)) strong <- strong | gaps$depth_ei >= as.integer(gc$strong_depth)
  gaps$architecture <- ifelse(strong, "strong", "weak")
  high <- gaps$complementarity == "high"
  low <- gaps$complementarity == "low"
  gaps$quadrant <- ifelse(high & strong, "established", ifelse(high, "opportunity", ifelse(low & strong, "legacy", "none")))
  gaps <- gaps[, AGR_GAP_COLUMNS]
  rownames(gaps) <- NULL
  list(gaps = gaps, execution_gaps = agr_execution_gaps(cells, cov_grade, pos, cfg))
}

# Execution gaps, stage by stage (see the header): priority cells with aligned high-complementarity
# suppliers, none of whose agreements with the economy covers the cell at a specific grade.
agr_execution_gaps <- function(cells, cov_grade, pos, cfg) {
  gc <- cfg$gaps
  empty <- data.frame(iso3 = character(), tech = character(), stages = character(), priority_basis = character(),
                      priority = numeric(), n_suppliers = integer(), suppliers_without_coverage = character(),
                      suppliers_text_coverage = character(), stringsAsFactors = FALSE)
  if (!nrow(cells)) return(empty)
  pp <- if ("policy_priority_percentile" %in% names(pos)) as.numeric(pos$policy_priority_percentile) else rep(NA_real_, nrow(pos))
  sc <- if ("strategic_choice" %in% names(pos)) as.character(pos$strategic_choice) else rep(NA_character_, nrow(pos))
  choices <- unlist(gc$priority_choices)
  by_pri <- !is.na(pp) & pp >= as.numeric(gc$priority_min)
  by_choice <- sc %in% choices
  pri_cells <- pos[by_pri | by_choice, c("iso3", "tech", "supply_chain"), drop = FALSE]
  if (!nrow(pri_cells)) return(empty)
  pri_cells$basis <- mapply(function(p, c) {
    b <- c(if (p) "policy_priority", if (nzchar(c)) paste0("choice_", c))
    paste(b, collapse = "|")
  }, by_pri[by_pri | by_choice], ifelse(by_choice, sc, "")[by_pri | by_choice])
  pri_by <- agr_max_by(pp, paste(pos$iso3, pos$tech, sep = "|"))
  hc <- cells[cells$aligned, , drop = FALSE]
  hc$cell <- paste(hc$need_iso3, hc$tech, hc$stage, sep = "|")
  pri_cells$cell <- paste(pri_cells$iso3, pri_cells$tech, pri_cells$supply_chain, sep = "|")
  hc <- hc[hc$cell %in% pri_cells$cell, , drop = FALSE]
  if (!nrow(hc)) return(empty)
  hc$grade <- unname(cov_grade[paste(agr_pair_key(hc$need_iso3, hc$supplier_iso3), hc$tech, hc$stage, sep = "\r")])
  hc$grade[is.na(hc$grade)] <- "none"
  covered_cells <- unique(hc$cell[hc$grade == "specific"])
  gapc <- hc[!(hc$cell %in% covered_cells), , drop = FALSE]
  if (!nrow(gapc)) return(empty)
  cap <- stats::setNames(as.numeric(pos$capability), paste(pos$iso3, pos$tech, pos$supply_chain, sep = "|"))
  gapc$cap <- unname(cap[paste(gapc$supplier_iso3, gapc$tech, gapc$stage, sep = "|")])
  gapc$k2 <- paste(gapc$need_iso3, gapc$tech, sep = "|")
  out <- lapply(split(gapc, factor(gapc$k2, levels = unique(gapc$k2))), function(d) {
    d <- d[order(!d$rec, -ifelse(is.na(d$cap), -1, d$cap), d$supplier_iso3, method = "radix"), , drop = FALSE]
    sup <- unique(d$supplier_iso3)
    pcs <- pri_cells[pri_cells$cell %in% unique(d$cell), , drop = FALSE]
    data.frame(iso3 = d$need_iso3[1], tech = d$tech[1], stages = agr_join_stages(d$stage),
               priority_basis = paste(unique(unlist(strsplit(pcs$basis, "|", fixed = TRUE))), collapse = "|"),
               priority = round(unname(pri_by[d$k2[1]]), 4), n_suppliers = length(sup),
               suppliers_without_coverage = paste(sup, collapse = "|"),
               suppliers_text_coverage = paste(unique(d$supplier_iso3[d$grade == "text"]), collapse = "|"),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out)
  out <- out[order(out$iso3, out$tech, method = "radix"), AGR_EXEC_GAP_COLUMNS, drop = FALSE]
  rownames(out) <- NULL
  out
}
