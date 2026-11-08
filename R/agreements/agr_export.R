# Dashboard export shaping (docs/partnership_agreements.md, section 9). Pure: builds R lists that
# jsonlite::toJSON(auto_unbox = TRUE, na = "null", null = "null") turns into the contract's JSON.
# Every array field is wrapped in I() (or is an unnamed list) so a one-item array stays an array;
# empty strings become NA (JSON null) in nullable fields.
#
#   agr_export_vocab(cfg, source_counts, source_updated)      manifest vocab
#   agr_export_agreements(canonical, canonical_codes, links, flags, delivery, cfg)
#                                                             named list of agreement objects
#   agr_country_agreement_ids(canonical, eu_members)          iso3 -> list(ids, via_eu)
#   agr_export_global(pairs, country_index, agreement_objs)   the global columnar file
#   agr_export_country(iso3, ...)                             one economy's file
#   agr_export_trim_gaps(gaps, iso3, top_candidates)          the economy's gap rows, trimmed

agr_na <- function(x) {
  x <- agr_chr(x)
  x[!nzchar(x)] <- NA_character_
  x
}

# A named list (id -> label) from a config map whose values are labels or lists with a label.
agr_label_map <- function(x) {
  if (is.null(x) || !length(x)) return(stats::setNames(list(), character()))
  lapply(x, function(v) if (is.list(v)) as.character(v$label %||% "") else as.character(v))
}

agr_export_vocab <- function(cfg, source_counts = integer(), source_updated = character(), compacts = NULL) {
  st <- agr_sector_table(cfg)
  it <- agr_instrument_table(cfg)
  srcs <- names(cfg$sources)
  list(
    agreement_types = agr_label_map(cfg$agreement_types),
    partnership_labels = agr_label_map(cfg$partnership_labels),
    legal_statuses = agr_label_map(cfg$legal_statuses),
    implementation_statuses = agr_label_map(cfg$implementation_statuses),
    delivery_statuses = I(as.character(unlist(cfg$delivery_statuses))),
    coding_bases = agr_label_map(cfg$coding_bases),
    confidence_levels = agr_label_map(cfg$confidence_levels),
    depth_classes = stats::setNames(lapply(cfg$depth$classes, as.character), as.character(names(cfg$depth$classes))),
    instrument_groups = agr_label_map(cfg$instrument_groups),
    sectors = unname(lapply(seq_len(nrow(st)), function(i) list(
      id = st$sector[i], label = st$label[i], ei = st$electro_industrial[i], techs = I(agr_split(st$techs[i])[[1]])))),
    instruments = unname(lapply(seq_len(nrow(it)), function(i) list(
      id = it$instrument[i], label = it$label[i], group = it$group[i], operational = it$operational[i]))),
    sources = unname(lapply(srcs, function(s) {
      x <- cfg$sources[[s]]
      n <- source_counts[s]
      u <- source_updated[s]
      list(id = s, label = as.character(x$label %||% s), short = as.character(x$short %||% s),
           citation = as.character(x$citation %||% ""), url = as.character(x$url %||% ""),
           licence = as.character(x$licence %||% ""),
           n_records = if (is.na(n)) 0L else as.integer(n),
           last_updated = if (is.na(u) || !nzchar(u)) NA_character_ else unname(u))
    })),
    specific_bases = I(as.character(unlist(cfg$specific_bases))),
    # Bases at which an instrument counts as established (a title keyword does not establish one:
    # R/agreements/agr_pairs.R AGR_ESTABLISHED_INSTRUMENT_BASES).
    established_instrument_bases = I(as.character(if (exists("AGR_ESTABLISHED_INSTRUMENT_BASES")) AGR_ESTABLISHED_INSTRUMENT_BASES
                                                  else c("source_coded", "curated", "type_implied"))),
    # Strategic compact id -> name (config/aib_strategic_compacts.yml), for the shared compacts of gap rows.
    compacts = if (length(compacts)) as.list(compacts) else stats::setNames(list(), character())
  )
}

# One code per (agreement, code) with its most specific basis (config coding_bases order).
agr_best_codes <- function(canonical_codes, dimension, cfg) {
  x <- canonical_codes[canonical_codes$dimension == dimension, c("canonical_agreement_id", "code", "basis"), drop = FALSE]
  if (!nrow(x)) return(x)
  rank <- match(x$basis, names(cfg$coding_bases))
  rank[is.na(rank)] <- 99L
  x <- x[order(x$canonical_agreement_id, x$code, rank, method = "radix"), , drop = FALSE]
  x[!duplicated(paste(x$canonical_agreement_id, x$code)), , drop = FALSE]
}

# Agreement objects keyed by canonical id. flags: agr_agreement_flags() (ei, trade_depth_index);
# delivery: data frame (canonical_agreement_id, text, status, url) or NULL. via_eu is set per
# economy by agr_export_country().
agr_export_agreements <- function(canonical, canonical_codes, links, flags, delivery = NULL, cfg) {
  ids <- canonical$canonical_agreement_id
  by_id <- function(df, col = "canonical_agreement_id") split(seq_len(nrow(df)), factor(df[[col]], levels = ids))
  sec <- agr_best_codes(canonical_codes, "sector", cfg)
  ins <- agr_best_codes(canonical_codes, "instrument_types", cfg)
  tec <- agr_best_codes(canonical_codes, "technology", cfg)
  seg <- agr_best_codes(canonical_codes, "supply_chain_segment", cfg)
  si <- by_id(sec); ii <- by_id(ins); ti <- by_id(tec); gi <- by_id(seg)
  li <- by_id(links)
  di <- if (!is.null(delivery) && nrow(delivery)) by_id(delivery) else NULL
  fi <- match(ids, flags$canonical_agreement_id)
  pairs_of <- function(df, ix) unname(lapply(ix, function(j) c(df$code[j], df$basis[j])))
  out <- lapply(seq_along(ids), function(k) {
    id <- ids[k]
    r <- canonical[k, , drop = FALSE]
    lk <- links[li[[id]], , drop = FALSE]
    lk <- lk[order(match(lk$source_dataset, AGR_SOURCE_PRECEDENCE), lk$source_record_id, method = "radix"), , drop = FALSE]
    dl <- if (is.null(di)) list() else {
      d <- delivery[di[[id]], , drop = FALSE]
      unname(lapply(seq_len(nrow(d)), function(j) list(text = agr_chr(d$text[j]), status = agr_chr(d$status[j]),
                                                         url = agr_na(d$url[j]))))
    }
    tdi <- if (is.na(fi[k])) NA_real_ else flags$trade_depth_index[fi[k]]
    list(
      id = id, title = agr_chr(r$title), parties = I(agr_split(r$parties)[[1]]),
      type = agr_chr(r$agreement_type), label = agr_na(r$partnership_label),
      legal = agr_chr(r$legal_status), legal_basis = agr_na(r$legal_status_basis),
      date = agr_na(r$primary_date), date_precision = agr_chr(r$date_precision),
      signature_date = agr_na(r$signature_date), effective_date = agr_na(r$effective_date),
      status = agr_chr(r$implementation_status), status_raw = agr_na(r$implementation_status_raw),
      category = agr_na(r$relationship_category),
      sectors = pairs_of(sec, si[[id]]), instruments = pairs_of(ins, ii[[id]]),
      techs = I(tec$code[ti[[id]]]), segments = I(seg$code[gi[[id]]]),
      ei = if (is.na(fi[k])) FALSE else isTRUE(flags$ei[fi[k]]), via_eu = FALSE,
      sources = unname(lapply(seq_len(nrow(lk)), function(j) list(
        dataset = lk$source_dataset[j], record_id = lk$source_record_id[j], url = agr_na(lk$source_url[j])))),
      primary_url = agr_na(r$primary_document_url), primary_available = isTRUE(as.logical(r$primary_source_available)),
      confidence = agr_chr(r$confidence), source_count = as.integer(r$source_count),
      delivery = dl,
      trade_depth_index = if (is.na(tdi)) NA_integer_ else as.integer(tdi)
    )
  })
  stats::setNames(out, ids)
}

# For each economy: the agreements it is a party to, and (for EU member states) the EU's own
# agreements it is not a party to (via_eu).
agr_country_agreement_ids <- function(canonical, eu_members, eu_code = "EUU") {
  pl <- agr_split(canonical$parties)
  ids <- canonical$canonical_agreement_id
  long <- data.frame(iso3 = unlist(pl), id = rep(ids, lengths(pl)), stringsAsFactors = FALSE)
  eu_ids <- ids[vapply(pl, function(p) eu_code %in% p, logical(1))]
  res <- split(long$id, long$iso3)
  out <- lapply(names(res), function(iso) list(ids = res[[iso]], via_eu = character()))
  names(out) <- names(res)
  for (m in eu_members) {
    own <- if (is.null(out[[m]])) character() else out[[m]]$ids
    via <- setdiff(eu_ids, own)
    if (!length(via) && !length(own)) next
    out[[m]] <- list(ids = own, via_eu = via)
  }
  out[order(names(out))]
}

agr_export_global <- function(pairs, country_index, agreement_objs) {
  strip_grade <- function(x) vapply(agr_split(x), function(v) paste(sub(":[^:]*$", "", v), collapse = "|"), character(1))
  p <- list(
    a = I(pairs$a), b = I(pairs$b), n = I(as.integer(pairs$n_agreements)), n_recent = I(as.integer(pairs$n_recent)),
    newest = I(agr_na(pairs$newest)), depth_all = I(as.integer(pairs$depth_all)), depth_ei = I(as.integer(pairs$depth_ei)),
    has_sp = I(as.logical(pairs$has_strategic_partnership)), has_trade = I(as.logical(pairs$has_trade_agreement)),
    has_sectoral = I(as.logical(pairs$has_sectoral)), n_delivery = I(as.integer(pairs$n_delivery)),
    ei_sectors = I(agr_na(strip_grade(pairs$ei_sectors))), types = I(agr_na(pairs$types)), sources = I(agr_na(pairs$sources))
  )
  # Per-pair attributes for the all-economies filters (a pair passes when any of its agreements has
  # the value): years of all / electro-industrial agreements, legal and implementation statuses,
  # instrument groups established in any agreement, stages covered at an established grade.
  ids <- agr_split(pairs$agreement_ids)
  field <- function(f, keep = NULL) vapply(ids, function(v) {
    o <- agreement_objs[intersect(v, names(agreement_objs))]
    if (!is.null(keep)) o <- Filter(keep, o)
    x <- vapply(o, function(z) { y <- z[[f]]; if (is.null(y) || is.na(y)) "" else as.character(y) }, character(1))
    if (f == "date") x <- substr(x, 1, 4)
    agr_join(x)
  }, character(1))
  p$years <- I(field("date"))
  p$years_ei <- I(field("date", function(z) isTRUE(z$ei)))
  p$legal <- I(field("legal"))
  p$status <- I(field("status"))
  groups <- grep("^inst_", names(pairs), value = TRUE)
  p$igroups <- I(vapply(seq_len(nrow(pairs)), function(i) {
    g <- groups[vapply(groups, function(k) isTRUE(as.integer(pairs[[k]][i]) == 2L), logical(1))]
    agr_join(sub("^inst_", "", g))
  }, character(1)))
  p$stages <- I(if ("segments" %in% names(pairs)) agr_chr(pairs$segments) else rep("", nrow(pairs)))
  isos <- names(country_index)
  pk <- c(pairs$a, pairs$b)
  dep <- c(pairs$depth_all, pairs$depth_all)
  n_partners <- vapply(isos, function(i) sum(pk == i), integer(1))
  n_d3 <- vapply(isos, function(i) sum(pk == i & dep >= 3L), integer(1))
  n_ag <- vapply(isos, function(i) length(country_index[[i]]$ids) + length(country_index[[i]]$via_eu), integer(1))
  n_ei <- vapply(isos, function(i) {
    a <- c(country_index[[i]]$ids, country_index[[i]]$via_eu)
    sum(vapply(agreement_objs[a], function(o) isTRUE(o$ei), logical(1)))
  }, integer(1))
  newest <- vapply(isos, function(i) {
    a <- c(country_index[[i]]$ids, country_index[[i]]$via_eu)
    d <- agr_chr(vapply(agreement_objs[a], function(o) if (is.na(o$date)) "" else o$date, character(1)))
    d <- d[nzchar(d)]
    if (!length(d)) NA_character_ else d[which.max(as.numeric(agr_date_value(d)))]
  }, character(1))
  list(pairs = p,
       countries = list(iso3 = I(unname(isos)), n_partners = I(unname(n_partners)), n_partners_d3 = I(unname(n_d3)),
                        n_agreements = I(unname(n_ag)), n_ei_agreements = I(unname(n_ei)), newest = I(unname(newest))))
}

# The economy's gap rows, from its side: per technology and side, the top_candidates partners
# (high complementarity first, then recommended, then need x capability) plus every partner with
# an agreement covering the technology.
agr_export_trim_gaps <- function(gaps, iso3, top_candidates = 5L) {
  g <- gaps[gaps$need_iso3 == iso3 | gaps$supplier_iso3 == iso3, , drop = FALSE]
  g <- g[g$need_iso3 != g$supplier_iso3, , drop = FALSE]
  if (!nrow(g)) return(g)
  g$side <- ifelse(g$need_iso3 == iso3, "needs", "supplies")
  g$partner <- ifelse(g$side == "needs", g$supplier_iso3, g$need_iso3)
  nc <- ifelse(is.na(g$need), 0, g$need) * ifelse(is.na(g$capability), 0, g$capability)
  g <- g[order(g$tech, g$side, g$complementarity != "high", -g$recommended, -nc, g$partner, method = "radix"), , drop = FALSE]
  rk <- stats::ave(seq_len(nrow(g)), g$tech, g$side, FUN = seq_along)
  g[rk <= top_candidates | g$covered_grade != "none", , drop = FALSE]
}

agr_export_country <- function(iso3, country_index, agreement_objs, pairs, gaps, execution_gaps, cfg) {
  ci <- country_index[[iso3]]
  ag <- c(lapply(agreement_objs[ci$ids], function(o) o),
          lapply(agreement_objs[ci$via_eu], function(o) { o$via_eu <- TRUE; o }))
  dn <- vapply(ag, function(o) if (is.na(o$date)) -1e9 else as.numeric(agr_date_value(o$date)), numeric(1))
  ag <- unname(ag[order(-dn, names(ag), method = "radix")])
  pr <- pairs[pairs$a == iso3 | pairs$b == iso3, , drop = FALSE]
  groups <- names(cfg$instrument_groups)
  partners <- lapply(seq_len(nrow(pr)), function(i) {
    r <- pr[i, , drop = FALSE]
    sc <- agr_split(r$sectors)[[1]]
    inst <- stats::setNames(lapply(groups, function(gp) { v <- r[[paste0("inst_", gp)]]; if (is.null(v) || is.na(v)) 0L else as.integer(v) }), groups)
    list(partner = if (r$a == iso3) r$b else r$a, n = as.integer(r$n_agreements), n_bilateral = as.integer(r$n_bilateral),
         n_plurilateral = as.integer(r$n_plurilateral), n_via_eu = as.integer(r$n_via_eu), n_active = as.integer(r$n_active),
         n_recent = as.integer(r$n_recent), oldest_active = agr_na(r$oldest_active), newest = agr_na(r$newest),
         has_sp = isTRUE(r$has_strategic_partnership), has_trade = isTRUE(r$has_trade_agreement),
         has_sectoral = isTRUE(r$has_sectoral),
         sectors = unname(lapply(sc, function(x) c(sub(":[^:]*$", "", x), sub("^.*:", "", x)))),
         ei_sectors = I(sub(":[^:]*$", "", agr_split(r$ei_sectors)[[1]])),
         techs = I(agr_split(r$technologies)[[1]]), segments = I(agr_split(r$segments)[[1]]),
         inst = inst, n_announced = as.integer(r$n_announced), n_signed = as.integer(r$n_signed),
         n_in_force = as.integer(r$n_in_force), n_delivery = as.integer(r$n_delivery),
         share_delivery = as.numeric(r$share_delivery),
         trade_depth_index = if (is.na(r$trade_depth_index)) NA_integer_ else as.integer(r$trade_depth_index),
         depth_all = as.integer(r$depth_all), depth_all_rule = agr_na(r$depth_all_rule),
         depth_ei = as.integer(r$depth_ei), depth_ei_rule = agr_na(r$depth_ei_rule),
         sources = I(agr_split(r$sources)[[1]]), max_confidence = agr_na(r$max_confidence),
         agreement_ids = I(agr_split_ordered(r$agreement_ids)))
  })
  dpth <- vapply(partners, function(p) p$depth_all, integer(1))
  nn <- vapply(partners, function(p) p$n, integer(1))
  pid <- vapply(partners, function(p) p$partner, character(1))
  partners <- partners[order(-dpth, -nn, pid, method = "radix")]
  tg <- agr_export_trim_gaps(gaps, iso3, as.integer(cfg$gaps$top_candidates))
  gl <- lapply(seq_len(nrow(tg)), function(i) {
    g <- tg[i, , drop = FALSE]
    list(partner = g$partner, tech = g$tech, side = g$side, need = if (is.na(g$need)) NA_real_ else g$need,
         capability = if (is.na(g$capability)) NA_real_ else g$capability, recommended = as.integer(g$recommended),
         friendshore = if (is.na(g$friendshore)) NA_real_ else g$friendshore,
         aib_shared = I(agr_split(g$aib_shared)[[1]]), complementarity = g$complementarity,
         covered_grade = g$covered_grade, architecture = g$architecture, quadrant = g$quadrant,
         alignment = if (is.null(g$alignment) || is.na(g$alignment)) NA_real_ else g$alignment,
         alignment_blocked = isTRUE(g$alignment_blocked),
         stages_high = I(agr_split_ordered(if (is.null(g$stages_high)) "" else g$stages_high)),
         stages_covered = I(agr_split_ordered(if (is.null(g$stages_covered)) "" else g$stages_covered)))
  })
  eg <- execution_gaps[execution_gaps$iso3 == iso3, , drop = FALSE]
  el <- lapply(seq_len(nrow(eg)), function(i) list(
    tech = eg$tech[i], stages = I(agr_split_ordered(if (is.null(eg$stages)) "" else eg$stages[i])),
    priority_basis = eg$priority_basis[i],
    priority = if (is.na(eg$priority[i])) NA_real_ else eg$priority[i],
    suppliers_without_coverage = I(agr_split_ordered(eg$suppliers_without_coverage[i]))))
  list(iso3 = iso3, agreements = ag, partners = partners, gaps = gl, execution_gaps = el)
}

# Split a "|"-joined field keeping its order (agr_split also keeps order; agr_join sorts).
agr_split_ordered <- function(x) {
  v <- strsplit(agr_chr(x), "|", fixed = TRUE)[[1]]
  v[nzchar(v)]
}

# Delivery evidence rows (canonical_agreement_id, text, status, url) from the extras of every
# source that records implementation evidence (pmemo implementation_evidence), mapped through links.
agr_export_delivery <- function(extras_by_source, links) {
  rows <- list()
  for (s in names(extras_by_source)) {
    ev <- extras_by_source[[s]]$implementation_evidence
    if (is.null(ev) || !is.data.frame(ev) || !nrow(ev)) next
    rows[[s]] <- data.frame(source_record_id = agr_chr(ev$source_record_id), text = agr_chr(ev$text),
                            status = agr_chr(ev$status), url = agr_chr(ev$url), stringsAsFactors = FALSE)
  }
  if (!length(rows)) return(NULL)
  d <- do.call(rbind, rows)
  d$canonical_agreement_id <- links$canonical_agreement_id[match(d$source_record_id, links$source_record_id)]
  d <- d[!is.na(d$canonical_agreement_id), c("canonical_agreement_id", "text", "status", "url"), drop = FALSE]
  rownames(d) <- NULL
  d
}
