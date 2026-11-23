# Pair roll-up and partnership depth (docs/partnership_agreements.md, sections 6 and 7). Pure.
#
#   agr_agreement_flags(canonical, canonical_codes, cfg, as_of, desta_design)
#       one row per canonical agreement: activity, recency, depth eligibility, electro-industrial
#       flag, the evidence each depth rule reads, and the DESTA depth index of its DESTA member.
#   agr_agreement_grades(canonical_codes, cfg)
#       list(sectors = (agreement, sector, grade), instruments = (agreement, instrument, level,
#       group, operational), techs, segments): grade "specific" (a specific basis) or "text"
#       (text_body in at least min_body_sentences sentences of one document); instrument level 2
#       (established: source_coded, curated, type_implied) or 1 (mentioned: text_title, text_body).
#   agr_pair_agreements(canonical, cfg, eu_members, acceding)
#       one row per (unordered pair, agreement): every pair of an agreement's parties, plus member
#       state pairs through EUU (via_eu). Pairs of the EU with one of its member states are dropped.
#   agr_pair_profiles(canonical, canonical_codes, cfg, eu_members, as_of, desta_design)
#       one row per pair with an agreement (section 6), with depth_all / depth_ei and their rules.

AGR_ESTABLISHED_INSTRUMENT_BASES <- c("source_coded", "curated", "type_implied")
AGR_ACTIVE_STATUSES <- c("in_force", "implementation_underway", "operational")
AGR_DEPTH_GROUPS_FIMAN <- c("finance", "investment", "manufacturing")

# Sector -> dashboard technology, one row per pair (config sectors.*.techs).
agr_sector_tech_map <- function(cfg) {
  st <- agr_sector_table(cfg)
  t <- agr_split(st$techs)
  data.frame(sector = rep(st$sector, lengths(t)), tech = unlist(t), stringsAsFactors = FALSE)
}

# The latest of a set of normalised date strings per row ("" when none).
agr_latest_date <- function(...) {
  m <- cbind(...)
  apply(m, 1, function(v) {
    v <- v[nzchar(v)]
    if (!length(v)) return("")
    v[which.max(as.numeric(agr_date_value(v)))]
  })
}

agr_agreement_grades <- function(canonical_codes, cfg) {
  cc <- canonical_codes
  spec <- unlist(cfg$specific_bases)
  min_body <- as.integer(cfg$text_coding$min_body_sentences %||% 2L)
  sec <- cc[cc$dimension == "sector", , drop = FALSE]
  sec$grade <- ifelse(sec$basis %in% spec, "specific",
                      ifelse(sec$basis == "text_body" & sec$n_hits >= min_body, "text", ""))
  sec <- sec[nzchar(sec$grade), , drop = FALSE]
  sec <- sec[order(sec$canonical_agreement_id, sec$code, sec$grade != "specific", method = "radix"), , drop = FALSE]
  sec <- sec[!duplicated(paste(sec$canonical_agreement_id, sec$code)), c("canonical_agreement_id", "code", "grade"), drop = FALSE]
  names(sec)[2] <- "sector"
  it <- agr_instrument_table(cfg)
  ins <- cc[cc$dimension == "instrument_types", , drop = FALSE]
  ins$level <- ifelse(ins$basis %in% AGR_ESTABLISHED_INSTRUMENT_BASES, 2L, 1L)
  ins <- ins[order(ins$canonical_agreement_id, ins$code, -ins$level, method = "radix"), , drop = FALSE]
  ins <- ins[!duplicated(paste(ins$canonical_agreement_id, ins$code)), c("canonical_agreement_id", "code", "level", "basis"), drop = FALSE]
  names(ins)[2] <- "instrument"
  ins$group <- it$group[match(ins$instrument, it$instrument)]
  ins$operational <- it$operational[match(ins$instrument, it$instrument)]
  ins$group[is.na(ins$group)] <- "other"
  ins$operational[is.na(ins$operational)] <- FALSE
  sp <- function(dim) {
    x <- cc[cc$dimension == dim & cc$basis %in% spec, c("canonical_agreement_id", "code"), drop = FALSE]
    x[!duplicated(paste(x$canonical_agreement_id, x$code)), , drop = FALSE]
  }
  rownames(sec) <- NULL; rownames(ins) <- NULL
  list(sectors = sec, instruments = ins, techs = sp("technology"), segments = sp("supply_chain_segment"))
}

agr_agreement_flags <- function(canonical, canonical_codes, cfg, as_of, desta_design = NULL) {
  pc <- cfg$pairs
  dc <- cfg$depth
  as_of_d <- as.Date(as_of)
  st <- agr_sector_table(cfg)
  ei_sectors <- st$sector[st$electro_industrial]
  excl <- unlist(dc$sectoral_exclude)
  gr <- agr_agreement_grades(canonical_codes, cfg)
  id <- canonical$canonical_agreement_id
  status <- canonical$implementation_status
  delivery <- unlist(cfg$delivery_statuses)
  date_max <- agr_latest_date(canonical$announcement_date, canonical$signature_date, canonical$effective_date)
  dmax <- agr_date_value(date_max)
  # The same day y years earlier (29 February falls back to 28 February in a common year).
  years_back <- function(y) {
    d <- as.Date(sprintf("%04d-%s", as.integer(format(as_of_d, "%Y")) - y, format(as_of_d, "%m-%d")), format = "%Y-%m-%d")
    if (is.na(d)) d <- as.Date(sprintf("%04d-02-28", as.integer(format(as_of_d, "%Y")) - y))
    d
  }
  counted <- !(status %in% c("cancelled", "inactive"))
  active <- counted & (status %in% AGR_ACTIVE_STATUSES |
                         (!is.na(dmax) & dmax >= years_back(as.integer(pc$active_window_years))))
  recent <- !is.na(dmax) & dmax >= years_back(as.integer(pc$recent_years))
  eligible <- canonical$n_parties <= as.integer(pc$max_parties_for_depth)
  sp_labels <- unlist(cfg$strategic_partnership_labels)
  sp <- canonical$agreement_type == "strategic_partnership" | canonical$partnership_label %in% sp_labels
  framework <- canonical$agreement_type %in% unlist(dc$framework_types) | canonical$partnership_label %in% sp_labels
  spec_trade <- gr$sectors$canonical_agreement_id[gr$sectors$sector == "trade" & gr$sectors$grade == "specific"]
  trade_type <- canonical$agreement_type %in% c("fta", "epa") | (canonical$agreement_type == "treaty" & id %in% spec_trade)
  trade <- trade_type & status %in% c("signed", "in_force", delivery)
  ei <- id %in% gr$sectors$canonical_agreement_id[gr$sectors$sector %in% ei_sectors]
  sec3 <- gr$sectors[!(gr$sectors$sector %in% excl), , drop = FALSE]
  op <- gr$instruments[gr$instruments$operational & gr$instruments$level == 2L, , drop = FALSE]
  first_by <- function(df, col, fmt) {
    if (!nrow(df)) return(rep("", length(id)))
    df <- df[order(df$canonical_agreement_id, df[[col]], method = "radix"), , drop = FALSE]
    v <- tapply(seq_len(nrow(df)), df$canonical_agreement_id, function(ix) fmt(df[ix, , drop = FALSE]))
    out <- unname(v[id]); out[is.na(out)] <- ""; out
  }
  sec3_ex <- first_by(sec3, "sector", function(d) paste0(d$sector, ":", d$grade, collapse = ", "))
  op_ex <- first_by(op, "instrument", function(d) paste0(d$instrument, " [", d$basis, "]", collapse = ", "))
  op_groups <- first_by(op, "group", function(d) paste(sort(unique(d$group)), collapse = "|"))
  in_effect <- (canonical$legal_status == "binding" & status %in% AGR_ACTIVE_STATUSES) | status %in% delivery
  delivery_flag <- status %in% delivery
  tdi <- rep(NA_real_, length(id))
  if (!is.null(desta_design) && nrow(desta_design) && "depth_index" %in% names(desta_design)) {
    members <- agr_split(canonical$source_records)
    di <- stats::setNames(suppressWarnings(as.numeric(desta_design$depth_index)), desta_design$source_record_id)
    tdi <- vapply(members, function(m) {
      v <- di[m[m %in% names(di)]]
      v <- v[!is.na(v)]
      if (length(v)) max(v) else NA_real_
    }, numeric(1))
  }
  data.frame(canonical_agreement_id = id, n_parties = canonical$n_parties, date = canonical$primary_date,
             date_max = date_max, counted = counted, active = active, recent = recent, eligible = eligible,
             ei = ei, sp = sp, framework = framework, trade = trade, sec3 = nzchar(sec3_ex), sec3_example = sec3_ex,
             op4 = nzchar(op_ex), op_example = op_ex, op_groups = op_groups,
             op_trade = grepl("(^|\\|)trade(\\||$)", op_groups),
             op_fiman = grepl(paste0("(^|\\|)(", paste(AGR_DEPTH_GROUPS_FIMAN, collapse = "|"), ")(\\||$)"), op_groups),
             in_effect = in_effect, delivery = delivery_flag, trade_depth_index = tdi,
             stringsAsFactors = FALSE)
}

# Acceding parties of each canonical agreement that is an accession (DESTA desta_design$acceding of a
# member record), as a named list of character vectors (canonical_agreement_id -> acceders).
agr_canonical_acceding <- function(canonical, desta_design = NULL) {
  if (is.null(desta_design) || !nrow(desta_design) || !"acceding" %in% names(desta_design) ||
      !"source_records" %in% names(canonical)) return(list())
  acc <- stats::setNames(agr_chr(desta_design$acceding), desta_design$source_record_id)
  acc <- acc[nzchar(acc)]
  if (!length(acc)) return(list())
  mem <- agr_split(canonical$source_records)
  out <- lapply(mem, function(m) unique(unlist(agr_split(acc[intersect(m, names(acc))]))))
  names(out) <- canonical$canonical_agreement_id
  out[lengths(out) > 0]
}

# Pairs of each agreement. An accession record (DESTA lists the whole membership after the accession)
# makes pairs between the acceding countries and the members only: pairs of existing members already
# hold the agreement itself, and counting the accession again would credit them with a new agreement
# every time a country joins.
agr_pair_agreements <- function(canonical, cfg, eu_members, acceding = list()) {
  eu <- as.character(cfg$pairs$eu_code %||% "EUU")
  propagate <- !isFALSE(cfg$pairs$propagate_eu)
  eu_members <- sort(unique(agr_chr(eu_members)))
  parties <- agr_split(canonical$parties)
  # The EU's seat in a club (G7, the Minerals Security Partnership) does not make its member states
  # members: records of these categories are not propagated through the EU.
  no_prop_cat <- unlist(cfg$pairs$no_propagate_categories %||% list("plurilateral_club"))
  no_prop <- if ("relationship_category" %in% names(canonical)) canonical$relationship_category %in% no_prop_cat else rep(FALSE, nrow(canonical))
  out_a <- list(); out_b <- list(); out_id <- list(); out_v <- list()
  for (k in seq_along(parties)) {
    p <- parties[[k]]
    if (length(p) >= 2) {
      cm <- utils::combn(p, 2)
      keep <- !((cm[1, ] == eu & cm[2, ] %in% eu_members) | (cm[2, ] == eu & cm[1, ] %in% eu_members))
      acc <- acceding[[canonical$canonical_agreement_id[k]]]
      if (length(acc)) keep <- keep & (cm[1, ] %in% acc | cm[2, ] %in% acc)
      cm <- cm[, keep, drop = FALSE]
      if (ncol(cm)) {
        out_a[[length(out_a) + 1]] <- cm[1, ]; out_b[[length(out_b) + 1]] <- cm[2, ]
        out_id[[length(out_id) + 1]] <- rep(canonical$canonical_agreement_id[k], ncol(cm))
        out_v[[length(out_v) + 1]] <- rep(FALSE, ncol(cm))
      }
    }
    if (propagate && eu %in% p && !no_prop[k]) {
      others <- setdiff(p, c(eu, eu_members))
      acc <- acceding[[canonical$canonical_agreement_id[k]]]
      if (length(acc) && !(eu %in% acc)) others <- intersect(others, acc)
      ms <- setdiff(eu_members, p)
      if (length(others) && length(ms)) {
        g <- expand.grid(m = ms, o = others, stringsAsFactors = FALSE)
        out_a[[length(out_a) + 1]] <- g$m; out_b[[length(out_b) + 1]] <- g$o
        out_id[[length(out_id) + 1]] <- rep(canonical$canonical_agreement_id[k], nrow(g))
        out_v[[length(out_v) + 1]] <- rep(TRUE, nrow(g))
      }
    }
  }
  if (!length(out_a)) {
    return(data.frame(pair = character(), a = character(), b = character(), canonical_agreement_id = character(),
                      via_eu = logical(), stringsAsFactors = FALSE))
  }
  x <- unlist(out_a); y <- unlist(out_b)
  a <- ifelse(x <= y, x, y); b <- ifelse(x <= y, y, x)
  pa <- data.frame(pair = paste(a, b, sep = "|"), a = a, b = b, canonical_agreement_id = unlist(out_id),
                   via_eu = unlist(out_v), stringsAsFactors = FALSE)
  pa <- pa[pa$a != pa$b, , drop = FALSE]
  pa <- pa[order(pa$pair, pa$canonical_agreement_id, pa$via_eu, method = "radix"), , drop = FALSE]
  pa <- pa[!duplicated(paste(pa$pair, pa$canonical_agreement_id)), , drop = FALSE]
  rownames(pa) <- NULL
  pa
}

# Depth class and rule for each pair over the rows of `pf` (pair x agreement with flags) in scope.
agr_depth_classes <- function(pf, pairs, cfg, scope_label) {
  needs <- as.character(cfg$depth$integrated_needs %||% "in_force_or_delivery")
  labels <- unlist(cfg$depth$classes)
  pf <- pf[pf$active, , drop = FALSE]
  dnum <- as.numeric(agr_date_value(pf$date))
  dnum[is.na(dnum)] <- -1e9
  pf <- pf[order(pf$pair, -dnum, pf$canonical_agreement_id, method = "radix"), , drop = FALSE]
  el <- pf[pf$eligible, , drop = FALSE]
  pick <- function(rows) rows[!duplicated(rows$pair), , drop = FALSE]
  anyp <- function(cond) pairs %in% unique(el$pair[cond])
  ex <- function(cond) { r <- pick(el[cond, , drop = FALSE]); stats::setNames(r$canonical_agreement_id, r$pair) }
  c1 <- pairs %in% pf$pair
  c2 <- anyp(el$framework); c3 <- anyp(el$sec3); c4 <- anyp(el$op4)
  effect <- if (needs == "delivery") el$delivery else el$in_effect
  c5 <- c4 & c3 & anyp(el$op_trade) & anyp(el$op_fiman) & anyp(effect)
  cls <- ifelse(c5, 5L, ifelse(c4, 4L, ifelse(c3, 3L, ifelse(c2, 2L, ifelse(c1, 1L, 0L)))))
  rule <- rep("", length(pairs))
  e4 <- ex(el$op4); e3 <- ex(el$sec3); e2 <- ex(el$framework); et <- ex(el$op_trade); ef <- ex(el$op_fiman); ee <- ex(effect)
  e1 <- { r <- pick(pf); stats::setNames(r$canonical_agreement_id, r$pair) }
  op_txt <- stats::setNames(el$op_example, el$canonical_agreement_id)
  sec_txt <- stats::setNames(el$sec3_example, el$canonical_agreement_id)
  grp_txt <- stats::setNames(el$op_groups, el$canonical_agreement_id)
  fiman_only <- function(s) paste(intersect(strsplit(s, "|", fixed = TRUE)[[1]], AGR_DEPTH_GROUPS_FIMAN), collapse = "|")
  type_of <- stats::setNames(pf$agreement_type, pf$canonical_agreement_id)
  sp_of <- stats::setNames(pf$sp, pf$canonical_agreement_id)
  for (i in which(cls == 5L)) {
    p <- pairs[i]
    rule[i] <- sprintf("5 %s (%s): trade instrument in %s; finance, investment or manufacturing (%s) in %s; sectoral %s in %s; in effect: %s",
                       labels[["5"]], scope_label, et[[p]], fiman_only(grp_txt[[ef[[p]]]]), ef[[p]], sec_txt[[e3[[p]]]], e3[[p]], ee[[p]])
  }
  for (i in which(cls == 4L)) { p <- pairs[i]; rule[i] <- sprintf("4 %s (%s): %s in %s", labels[["4"]], scope_label, op_txt[[e4[[p]]]], e4[[p]]) }
  for (i in which(cls == 3L)) { p <- pairs[i]; rule[i] <- sprintf("3 %s (%s): %s in %s", labels[["3"]], scope_label, sec_txt[[e3[[p]]]], e3[[p]]) }
  for (i in which(cls == 2L)) {
    p <- pairs[i]; a <- e2[[p]]
    rule[i] <- sprintf("2 %s (%s): %s in %s", labels[["2"]], scope_label,
                       if (type_of[[a]] %in% unlist(cfg$depth$framework_types)) type_of[[a]] else "strategic partnership label", a)
  }
  for (i in which(cls == 1L)) {
    p <- pairs[i]; a <- e1[[p]]
    why <- if (!(p %in% el$pair)) "only plurilaterals above max_parties_for_depth" else type_of[[a]]
    rule[i] <- sprintf("1 %s (%s): %s in %s", labels[["1"]], scope_label, why, a)
  }
  rule[cls == 0L] <- sprintf("0 %s (%s)", labels[["0"]], scope_label)
  list(class = cls, rule = rule)
}

agr_pair_profiles <- function(canonical, canonical_codes, cfg, eu_members, as_of, desta_design = NULL) {
  flags <- agr_agreement_flags(canonical, canonical_codes, cfg, as_of, desta_design)
  pa <- agr_pair_agreements(canonical, cfg, eu_members, agr_canonical_acceding(canonical, desta_design))
  if (!nrow(pa)) return(data.frame())
  gr <- agr_agreement_grades(canonical_codes, cfg)
  st <- agr_sector_table(cfg)
  ci <- match(pa$canonical_agreement_id, canonical$canonical_agreement_id)
  fi <- match(pa$canonical_agreement_id, flags$canonical_agreement_id)
  pf <- cbind(pa, flags[fi, setdiff(names(flags), "canonical_agreement_id")])
  pf$agreement_type <- canonical$agreement_type[ci]
  pf$status <- canonical$implementation_status[ci]
  pf$confidence <- canonical$confidence[ci]
  pf$datasets <- canonical$source_datasets[ci]
  pairs <- sort(unique(pa$pair), method = "radix")
  g <- factor(pf$pair, levels = pairs)
  cnt <- function(cond) as.integer(tapply(cond, g, sum))
  anyg <- function(cond) as.logical(tapply(cond, g, any))
  collapse <- function(v, cond = rep(TRUE, length(v))) {
    out <- tapply(ifelse(cond, v, ""), g, function(x) agr_join(unlist(strsplit(x, "|", fixed = TRUE))))
    out <- as.character(out); out[is.na(out)] <- ""; out
  }
  dmin_str <- function(cond) {
    d <- ifelse(cond, pf$date, "")
    as.character(tapply(d, g, function(x) { x <- x[nzchar(x)]; if (!length(x)) "" else x[which.min(as.numeric(agr_date_value(x)))] }))
  }
  dmax_str <- function(d) as.character(tapply(d, g, function(x) { x <- x[nzchar(x)]; if (!length(x)) "" else x[which.max(as.numeric(agr_date_value(x)))] }))
  ab <- do.call(rbind, strsplit(pairs, "|", fixed = TRUE))
  out <- data.frame(pair = pairs, a = ab[, 1], b = ab[, 2], stringsAsFactors = FALSE)
  out$n_agreements <- as.integer(table(g))
  out$n_bilateral <- cnt(pf$n_parties == 2L)
  out$n_plurilateral <- cnt(pf$n_parties > 2L)
  out$n_via_eu <- cnt(pf$via_eu)
  out$n_active <- cnt(pf$active)
  out$n_recent <- cnt(pf$recent)
  out$oldest_active <- dmin_str(pf$active)
  out$newest <- dmax_str(pf$date)
  out$has_strategic_partnership <- anyg(pf$counted & pf$sp)
  out$has_trade_agreement <- anyg(pf$counted & pf$trade)

  # Sectors, technologies, segments and instruments over the agreements that are not inactive.
  live <- pf[pf$counted, c("pair", "canonical_agreement_id"), drop = FALSE]
  ps <- merge(live, gr$sectors, by = "canonical_agreement_id")
  ps <- ps[order(ps$pair, ps$sector, ps$grade != "specific", method = "radix"), , drop = FALSE]
  ps <- ps[!duplicated(paste(ps$pair, ps$sector)), , drop = FALSE]
  ps$ei <- st$electro_industrial[match(ps$sector, st$sector)]
  gs <- factor(ps$pair, levels = pairs)
  # "|"-join v by the factor f (one string per level, "" for a level without values; an empty input,
  # e.g. no electro-industrial sector anywhere in a single-source run, gives "" everywhere).
  join_g <- function(v, f) {
    if (!length(f)) return(rep("", length(levels(f))))
    x <- tapply(v, f, function(z) paste(sort(z), collapse = "|")); x <- as.character(x); x[is.na(x)] <- ""; x
  }
  out$has_sectoral <- pairs %in% ps$pair[ps$grade == "specific" & !(ps$sector %in% unlist(cfg$depth$sectoral_exclude))]
  out$sectors <- join_g(paste0(ps$sector, ":", ps$grade), gs)
  out$n_sectors <- as.integer(table(gs))
  pse <- ps[ps$ei, , drop = FALSE]
  out$ei_sectors <- join_g(paste0(pse$sector, ":", pse$grade), factor(pse$pair, levels = pairs))
  out$n_ei_sectors <- as.integer(table(factor(pse$pair, levels = pairs)))
  tj <- function(tbl) {
    x <- merge(live, tbl, by = "canonical_agreement_id")
    x <- x[!duplicated(paste(x$pair, x$code)), , drop = FALSE]
    join_g(x$code, factor(x$pair, levels = pairs))
  }
  out$technologies <- tj(gr$techs)
  out$segments <- tj(gr$segments)
  pi <- merge(live, gr$instruments, by = "canonical_agreement_id")
  for (grp in names(cfg$instrument_groups)) {
    sub <- pi[pi$group == grp, , drop = FALSE]
    lv <- tapply(sub$level, factor(sub$pair, levels = pairs), max)
    lv <- as.integer(lv); lv[is.na(lv)] <- 0L
    out[[paste0("inst_", grp)]] <- lv
  }
  out$n_announced <- cnt(pf$status == "announced")
  out$n_signed <- cnt(pf$status == "signed")
  out$n_in_force <- cnt(pf$status == "in_force")
  out$n_delivery <- cnt(pf$status %in% unlist(cfg$delivery_statuses))
  out$share_delivery <- round(out$n_delivery / out$n_agreements, 4)
  tdi <- ifelse(pf$trade & pf$status %in% AGR_ACTIVE_STATUSES, pf$trade_depth_index, NA_real_)
  td <- suppressWarnings(tapply(tdi, g, max, na.rm = TRUE))
  td <- as.numeric(td); td[!is.finite(td)] <- NA_real_
  out$trade_depth_index <- td
  da <- agr_depth_classes(pf, pairs, cfg, "all agreements")
  de <- agr_depth_classes(pf[pf$ei, , drop = FALSE], pairs, cfg, "electro-industrial agreements")
  out$depth_all <- da$class
  out$depth_all_rule <- da$rule
  out$depth_ei <- de$class
  out$depth_ei_rule <- de$rule
  out$sources <- collapse(pf$datasets)
  conf_rank <- c(low = 1L, medium = 2L, high = 3L)
  mc <- tapply(conf_rank[pf$confidence], g, max)
  out$max_confidence <- names(conf_rank)[as.integer(mc)]
  dnum <- as.numeric(agr_date_value(pf$date)); dnum[is.na(dnum)] <- -1e9
  o <- order(pf$pair, -dnum, pf$canonical_agreement_id, method = "radix")
  out$agreement_ids <- as.character(tapply(pf$canonical_agreement_id[o], factor(pf$pair[o], levels = pairs), paste, collapse = "|"))
  # Additive columns: agreement types, the electro-industrial count, and the technologies an active,
  # depth-eligible agreement covers through its sectors (best grade), which the gap join reads.
  out$types <- collapse(pf$agreement_type)
  out$n_ei <- cnt(pf$ei)
  cov <- pf[pf$active & pf$eligible, c("pair", "canonical_agreement_id"), drop = FALSE]
  cs <- merge(cov, gr$sectors, by = "canonical_agreement_id")
  tc <- merge(cs[, c("pair", "sector", "grade"), drop = FALSE], agr_sector_tech_map(cfg), by = "sector")
  tc <- tc[order(tc$pair, tc$tech, tc$grade != "specific", method = "radix"), , drop = FALSE]
  tc <- tc[!duplicated(paste(tc$pair, tc$tech)), , drop = FALSE]
  out$tech_cover <- join_g(paste0(tc$tech, ":", tc$grade), factor(tc$pair, levels = pairs))
  rownames(out) <- NULL
  out
}
