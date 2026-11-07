# Linkage of source records into canonical agreements (docs/partnership_agreements.md, section 5).
#
# agr_link_records(records, codes, cfg, extras) takes the records and codes of every ingested
# source (common schema, R/agreements/agr_schema.R) and returns
#   canonical        one row per canonical agreement: the schema fields filled by field
#                    precedence, plus canonical_agreement_id, lead_record, source_records,
#                    source_datasets, source_count, n_records, link_rules, link_score_min,
#                    confidence;
#   links            one row per source record (none is dropped): the canonical id it belongs to,
#                    the rule and score of the link that attached it, and the record it linked to;
#   canonical_codes  the union of the members' codes, one row per (agreement, dimension, code,
#                    basis), n_hits the largest of one member (a document), n_records the members
#                    that carry it;
#   candidates       every candidate link considered, accepted or not, and why.
# Settings: cfg$linkage. Pure.
#
# Candidates: GSP documents on the same unordered pair within gsp_event_window_days (rule
# gsp_event); records of different sources (or both of a within_source_sources source) whose
# party sets have Jaccard >= party_jaccard_min and dates within date_window_days (year_window
# when either date is a year) that share a title (rule title: token Jaccard >= title_jaccard_min;
# rule title_containment: the shorter title inside the longer) or that are two trade agreements on
# one bilateral pair (rule trade_pair_year). Score = (title similarity + party Jaccard + date
# closeness) / 3, the trade rule counting as a full title match; gsp_event scores
# 1 - 0.25 x days / window. Links are accepted from the highest score down (ties by ids); a link
# below min_link_score is not made, nor one that would put two records of an exclusive source
# (DESTA, memberships, pmemo, IISD) into one agreement.

AGR_SOURCE_PRECEDENCE <- c("curated", "pmemo", "iisd", "desta", "memberships", "gsp")

# Within one source (a GSP event), the lead is the most formal document.
AGR_TYPE_RANK <- c("treaty", "fta", "epa", "framework", "strategic_partnership", "sectoral_partnership",
                   "financing_agreement", "mou", "joint_declaration", "joint_statement", "ministerial_initiative",
                   "plurilateral_forum", "other")

# Words dropped from titles besides config linkage$generic_title_words and country names:
# function words, months, administrative words and demonyms.
AGR_LINK_STOPWORDS <- c(
  "for", "with", "through", "among", "by", "from", "at", "as", "into", "under", "over", "this", "that",
  "these", "those", "co", "vs", "via", "upon", "toward", "towards", "be", "is", "are", "will", "new",
  "january", "february", "march", "april", "may", "june", "july", "august", "september", "october",
  "november", "december", "jan", "feb", "mar", "apr", "jun", "jul", "aug", "sep", "sept", "oct", "nov", "dec",
  "department", "ministry", "minister", "ministers", "ministerial", "prime", "president", "presidents",
  "office", "agency", "secretary", "council", "commission", "administration", "cabinet",
  "us", "usa", "uk", "eu", "prc", "rok", "dprk", "uae", "ussr", "u", "s",
  "american", "australian", "austrian", "belgian", "brazilian", "british", "canadian", "chilean", "chinese",
  "colombian", "czech", "danish", "dutch", "egyptian", "emirati", "estonian", "european", "finnish", "french",
  "german", "greek", "hungarian", "indian", "indonesian", "iranian", "iraqi", "irish", "israeli", "italian",
  "japanese", "jordanian", "kazakh", "kenyan", "korean", "kuwaiti", "latvian", "lithuanian", "malaysian",
  "mexican", "moroccan", "mongolian", "namibian", "nigerian", "norwegian", "pakistani", "peruvian",
  "philippine", "filipino", "polish", "portuguese", "qatari", "romanian", "russian", "saudi", "serbian",
  "singaporean", "slovak", "slovenian", "spanish", "swedish", "swiss", "thai", "turkish", "ukrainian",
  "uzbek", "vietnamese", "zambian", "argentine", "argentinian", "african", "asian", "arab", "emirates",
  "sino", "franco", "anglo", "turkiye", "czechia"
)

agr_link_settings <- function(cfg) {
  l <- cfg$linkage
  num <- function(x, d) if (is.null(x)) d else as.numeric(x)
  list(gsp_event_window_days = num(l$gsp_event_window_days, 7), party_jaccard_min = num(l$party_jaccard_min, 0.8),
       date_window_days = num(l$date_window_days, 45), year_window = num(l$year_window, 1),
       title_jaccard_min = num(l$title_jaccard_min, 0.5),
       title_containment_min = num(l$title_containment_min, 0.8),
       title_containment_min_tokens = num(l$title_containment_min_tokens, 3),
       trade_pair_year_rule = !isFALSE(l$trade_pair_year_rule), min_link_score = num(l$min_link_score, 0.6),
       trade_leg_rule = !isFALSE(l$trade_leg_rule), trade_leg_max_parties = num(l$trade_leg_max_parties, 8),
       trade_leg_year_window = num(l$trade_leg_year_window, 5),
       trade_leg_min_containment = num(l$trade_leg_min_containment, 0.5),
       within_source = unlist(l$within_source_sources %||% list("curated")),
       exclusive = unlist(l$exclusive_sources %||% list("desta", "memberships", "pmemo", "iisd")),
       generic = unlist(l$generic_title_words))
}

# --- titles ---------------------------------------------------------------------------------

# Country-name tokens (English names of countrycode, lower case).
agr_country_name_tokens <- function() {
  cl <- countrycode::codelist
  nm <- unique(c(cl$country.name.en, cl$iso.name.en, cl$un.name.en))
  nm <- tolower(nm[!is.na(nm)])
  tok <- unlist(strsplit(gsub("[^a-z]+", " ", iconv(nm, to = "ASCII//TRANSLIT", sub = "")), " +"))
  unique(tok[nchar(tok) >= 2])
}

agr_link_stopwords <- function(cfg) {
  unique(c(tolower(agr_link_settings(cfg)$generic), AGR_LINK_STOPWORDS, agr_country_name_tokens()))
}

# A light suffix stemmer (securing / security -> secur, minerals -> mineral).
agr_stem <- function(tok) {
  tok <- sub("ies$", "y", tok)
  tok <- ifelse(nchar(tok) > 5, sub("(ing|ity|ities|ation|ations|ment|ments)$", "", tok), tok)
  tok <- ifelse(nchar(tok) > 3 & !grepl("ss$", tok), sub("s$", "", tok), tok)
  tok
}

# Normalised title tokens: lower case, accents folded, punctuation and digits removed, stop words,
# generic words and country names dropped, light stemming. Returns a list of character vectors.
agr_title_tokens <- function(titles, stopwords) {
  t <- tolower(agr_chr(titles))
  t <- gsub("[’']s\\b", "", t, perl = TRUE)
  ascii <- iconv(t, to = "ASCII//TRANSLIT", sub = "")
  t <- ifelse(is.na(ascii) | (!nzchar(ascii) & nzchar(t)), t, ascii)
  t <- gsub("[^[:alpha:]]+", " ", t, perl = TRUE)
  lapply(strsplit(trimws(t), " +"), function(v) {
    v <- v[nchar(v) >= 2 & !(v %in% stopwords)]
    sort(unique(agr_stem(v)))
  })
}

agr_jaccard <- function(a, b) {
  u <- length(union(a, b))
  if (u == 0) return(0)
  length(intersect(a, b)) / u
}

agr_containment <- function(a, b) {
  m <- min(length(a), length(b))
  if (m == 0) return(0)
  length(intersect(a, b)) / m
}

# --- dates ----------------------------------------------------------------------------------

# Date closeness of two records over their announcement, signature and effective dates: the best
# pair of dates within the window (1 = same date, 0 = at the edge), NA when none is within it.
# Year-precision dates compare years (window year_window); others compare days. year_only: compare
# years whatever the precision (the trade rule).
agr_date_closeness <- function(di, dj, s, year_only = FALSE) {
  best <- NA_real_
  for (x in di) for (y in dj) {
    yr <- grepl("^\\d{4}$", x) || grepl("^\\d{4}$", y) || year_only
    if (yr) {
      g <- abs(agr_date_year(x) - agr_date_year(y))
      if (!is.na(g) && g <= s$year_window) best <- max(best, 1 - g / (s$year_window + 1), na.rm = TRUE)
    } else {
      g <- abs(as.numeric(agr_date_value(x) - agr_date_value(y)))
      if (!is.na(g) && g <= s$date_window_days) best <- max(best, 1 - g / s$date_window_days, na.rm = TRUE)
    }
  }
  best
}

agr_record_dates <- function(records) {
  lapply(seq_len(nrow(records)), function(i) {
    d <- c(records$announcement_date[i], records$signature_date[i], records$effective_date[i])
    unique(d[nzchar(d)])
  })
}

# TRUE for a trade agreement: fta / epa, or a treaty coded with the trade sector at a specific
# grade (a keyword in the body is not enough).
agr_is_trade_record <- function(records, codes, cfg) {
  spec <- unlist(cfg$specific_bases)
  tr <- codes$source_record_id[codes$dimension == "sector" & codes$code == "trade" & codes$basis %in% spec]
  records$agreement_type %in% c("fta", "epa") | (records$agreement_type == "treaty" & records$source_record_id %in% tr)
}

# --- ids --------------------------------------------------------------------------------------

# "agr-" + the first 10 hex characters of the MD5 of the sorted member ids ("|"-joined).
agr_canonical_id <- function(member_ids) {
  key <- paste(sort(unique(member_ids), method = "radix"), collapse = "|")
  paste0("agr-", substr(digest::digest(key, algo = "md5", serialize = FALSE), 1, 10))
}

# --- candidates -----------------------------------------------------------------------------

agr_link_candidates <- function(records, codes, cfg) {
  s <- agr_link_settings(cfg)
  n <- nrow(records)
  empty <- data.frame(from = character(), to = character(), rule = character(), score = numeric(),
                      title_jaccard = numeric(), title_containment = numeric(), party_jaccard = numeric(),
                      date_closeness = numeric(), stringsAsFactors = FALSE)
  if (n < 2) return(empty)
  parties <- agr_split(records$parties)
  dates <- agr_record_dates(records)
  tokens <- agr_title_tokens(records$title, agr_link_stopwords(cfg))
  trade <- agr_is_trade_record(records, codes, cfg)
  src <- records$source_dataset
  ids <- records$source_record_id
  out <- list()
  add <- function(i, j, rule, score, tj, tc, pj, dc) {
    out[[length(out) + 1L]] <<- data.frame(from = ids[i], to = ids[j], rule = rule, score = round(score, 4),
                                           title_jaccard = round(tj, 4), title_containment = round(tc, 4),
                                           party_jaccard = round(pj, 4), date_closeness = round(dc, 4),
                                           stringsAsFactors = FALSE)
  }
  consider <- function(i, j, pj) {
    if (src[i] == "gsp" && src[j] == "gsp") {
      # A treaty and a statement published the same week are different documents (the Japan-Australia
      # EPA and the leaders' joint statement): only documents of compatible types form one event.
      if ((records$agreement_type[i] == "treaty") != (records$agreement_type[j] == "treaty")) return(invisible())
      g <- suppressWarnings(min(abs(as.numeric(agr_date_value(records$primary_date[i]) - agr_date_value(records$primary_date[j])))))
      if (!is.na(g) && is.finite(g) && g <= s$gsp_event_window_days) {
        add(i, j, "gsp_event", 1 - 0.25 * g / s$gsp_event_window_days, agr_jaccard(tokens[[i]], tokens[[j]]),
            agr_containment(tokens[[i]], tokens[[j]]), pj, 1 - g / max(s$gsp_event_window_days, 1))
      }
      return(invisible())
    }
    if (src[i] == src[j] && !(src[i] %in% s$within_source)) return(invisible())
    dc <- agr_date_closeness(dates[[i]], dates[[j]], s)
    tj <- agr_jaccard(tokens[[i]], tokens[[j]])
    tc <- agr_containment(tokens[[i]], tokens[[j]])
    best <- NULL
    if (!is.na(dc)) {
      if (tj >= s$title_jaccard_min) {
        best <- list(rule = "title", score = (tj + pj + dc) / 3)
      } else if (tc >= s$title_containment_min && min(length(tokens[[i]]), length(tokens[[j]])) >= s$title_containment_min_tokens) {
        best <- list(rule = "title_containment", score = (0.75 * tc + pj + dc) / 3)
      }
    }
    if (s$trade_pair_year_rule && trade[i] && trade[j] && length(parties[[i]]) == 2 && pj == 1) {
      dy <- agr_date_closeness(dates[[i]], dates[[j]], s, year_only = TRUE)
      if (!is.na(dy)) {
        sc <- (1 + pj + dy) / 3
        if (is.null(best) || sc > best$score) { best <- list(rule = "trade_pair_year", score = sc); dc <- dy }
      }
    }
    if (!is.null(best)) add(i, j, best$rule, best$score, tj, tc, pj, dc)
    invisible()
  }
  # Bilateral records block on the pair; plurilaterals are compared with each other.
  bil <- which(records$n_parties == 2L)
  blocks <- split(bil, records$parties[bil])
  for (b in blocks) {
    if (length(b) < 2) next
    for (x in seq_len(length(b) - 1L)) for (y in (x + 1L):length(b)) consider(b[x], b[y], 1)
  }
  plu <- which(records$n_parties > 2L)
  if (length(plu) >= 2) {
    for (x in seq_len(length(plu) - 1L)) for (y in (x + 1L):length(plu)) {
      i <- plu[x]; j <- plu[y]
      if (src[i] == src[j] && !(src[i] %in% s$within_source)) next
      li <- length(parties[[i]]); lj <- length(parties[[j]])
      if (min(li, lj) / max(li, lj) < s$party_jaccard_min) next
      pj <- agr_jaccard(parties[[i]], parties[[j]])
      if (pj >= s$party_jaccard_min) consider(i, j, pj)
    }
  }
  # Trade legs (rule trade_leg): a bilateral trade agreement recorded by one source as the leg of a
  # small plurilateral trade agreement recorded by another (U.S. FTA "NAFTA / USMCA" with Canada vs
  # DESTA's NAFTA). The plurilateral must contain the pair, have at most trade_leg_max_parties
  # parties, share title tokens (containment >= trade_leg_min_containment) and have an entry into
  # force (else signature) within trade_leg_year_window years. Only the best plurilateral per
  # bilateral record is linked, so a leg never joins two successive agreements (NAFTA and USMCA).
  if (s$trade_leg_rule && length(plu) >= 1) {
    eff_year <- function(k) {
      y <- agr_date_year(records$effective_date[k])
      if (is.na(y)) y <- agr_date_year(records$primary_date[k])
      y
    }
    tp <- plu[trade[plu] & records$n_parties[plu] <= s$trade_leg_max_parties]
    for (i in bil[trade[bil]]) {
      best <- NULL
      for (j in tp) {
        if (src[i] == src[j]) next
        if (!all(parties[[i]] %in% parties[[j]])) next
        tc <- agr_containment(tokens[[i]], tokens[[j]])
        if (tc < s$trade_leg_min_containment) next
        g <- abs(eff_year(i) - eff_year(j))
        if (is.na(g) || g > s$trade_leg_year_window) next
        dc <- 1 - g / (s$trade_leg_year_window + 1)
        pj <- agr_jaccard(parties[[i]], parties[[j]])
        sc <- (tc + 1 + dc) / 3
        if (is.null(best) || sc > best$score) best <- list(j = j, score = sc, tc = tc, pj = pj, dc = dc)
      }
      if (!is.null(best)) add(i, best$j, "trade_leg", best$score, agr_jaccard(tokens[[i]], tokens[[best$j]]),
                              best$tc, best$pj, best$dc)
    }
  }
  if (length(out) == 0) return(empty)
  cand <- do.call(rbind, out)
  rownames(cand) <- NULL
  cand
}

# --- linkage --------------------------------------------------------------------------------

# Member order inside a canonical agreement: source precedence, then the most formal type, a named
# partnership, the earliest date and the id.
agr_member_order <- function(records) {
  sp <- match(records$source_dataset, AGR_SOURCE_PRECEDENCE)
  sp[is.na(sp)] <- length(AGR_SOURCE_PRECEDENCE) + 1L
  tr <- match(records$agreement_type, AGR_TYPE_RANK)
  tr[is.na(tr)] <- length(AGR_TYPE_RANK) + 1L
  dt <- agr_date_value(records$primary_date)
  dnum <- ifelse(is.na(dt), 99999, as.numeric(dt))
  order(sp, tr, !nzchar(records$partnership_label), dnum, records$source_record_id, method = "radix")
}

agr_link_verification <- function(extras) {
  frames <- list()
  walk <- function(x) {
    if (is.data.frame(x)) return(invisible())
    if (is.list(x)) {
      if (!is.null(x$curated_meta) && is.data.frame(x$curated_meta)) frames[[length(frames) + 1L]] <<- x$curated_meta
      for (el in x) if (is.list(el) && !is.data.frame(el)) walk(el)
    }
    invisible()
  }
  walk(extras)
  if (length(frames) == 0) return(stats::setNames(character(), character()))
  v <- do.call(rbind, lapply(frames, function(f) f[, c("source_record_id", "verification")]))
  stats::setNames(agr_chr(v$verification), v$source_record_id)
}

agr_link_records <- function(records, codes, cfg, extras = list()) {
  s <- agr_link_settings(cfg)
  records <- agr_as_records(records)
  codes <- agr_as_codes(codes)
  if (anyDuplicated(records$source_record_id)) stop("agr_link_records: duplicate source_record_id")
  n <- nrow(records)
  ids <- records$source_record_id
  cand <- agr_link_candidates(records, codes, cfg)
  cand$accepted <- rep(FALSE, nrow(cand))
  cand$reason <- rep("", nrow(cand))

  # Union-find over accepted links, from the highest score down.
  parent <- seq_len(n)
  names(parent) <- ids
  find <- function(i) { while (parent[i] != i) i <- parent[i]; i }
  comp_src <- lapply(seq_len(n), function(i) records$source_dataset[i])
  if (nrow(cand)) {
    o <- order(-cand$score, cand$from, cand$to, method = "radix")
    for (k in o) {
      if (cand$score[k] < s$min_link_score) { cand$reason[k] <- "below_min_link_score"; next }
      a <- find(match(cand$from[k], ids)); b <- find(match(cand$to[k], ids))
      if (a == b) { cand$accepted[k] <- TRUE; cand$reason[k] <- "same_cluster"; next }
      sa <- comp_src[[a]]; sb <- comp_src[[b]]
      clash <- intersect(intersect(sa, sb), s$exclusive)
      # Several legs of one plurilateral come from the same source (USA-CAN and USA-MEX of NAFTA):
      # a trade_leg link may join a cluster that already holds a leg of the leg's source.
      if (cand$rule[k] == "trade_leg") clash <- setdiff(clash, records$source_dataset[match(cand$from[k], ids)])
      if (length(clash)) { cand$reason[k] <- paste0("two_", clash[1], "_records"); next }
      parent[b] <- a
      comp_src[[a]] <- c(sa, sb)
      cand$accepted[k] <- TRUE
      cand$reason[k] <- "linked"
    }
  }
  root <- vapply(seq_len(n), find, integer(1))
  members <- split(seq_len(n), root)
  cid_of_root <- vapply(members, function(m) agr_canonical_id(ids[m]), character(1))
  cid <- unname(cid_of_root[as.character(root)])

  # Links: each member, the accepted link that attached it (the best-scoring accepted edge touching it
  # inside its cluster), or lead / singleton.
  acc <- cand[cand$accepted & cand$reason == "linked", , drop = FALSE]
  ord <- agr_member_order(records)
  rank_in <- integer(n); rank_in[ord] <- seq_len(n)
  lead_of <- vapply(members, function(m) m[which.min(rank_in[m])], integer(1))
  is_lead <- seq_len(n) %in% lead_of
  rule <- ifelse(lengths(members[as.character(root)]) == 1L, "singleton", "lead")
  score <- rep(NA_real_, n); linked_to <- rep("", n)
  for (i in which(!is_lead)) {
    e <- acc[acc$from == ids[i] | acc$to == ids[i], , drop = FALSE]
    if (!nrow(e)) next
    e <- e[order(-e$score, e$from, e$to, method = "radix"), , drop = FALSE][1, ]
    rule[i] <- e$rule
    score[i] <- e$score
    linked_to[i] <- if (e$from == ids[i]) e$to else e$from
  }
  links <- data.frame(canonical_agreement_id = cid, source_record_id = ids, source_dataset = records$source_dataset,
                      rule = rule, link_score = score, linked_to = linked_to, title = records$title,
                      source_url = records$source_url, stringsAsFactors = FALSE)
  links <- links[order(links$canonical_agreement_id, rank_in[match(links$source_record_id, ids)], method = "radix"), , drop = FALSE]
  rownames(links) <- NULL

  # Canonical codes: union over members, keeping the basis; n_hits of the best member document.
  cc <- codes[codes$source_record_id %in% ids, , drop = FALSE]
  cc$canonical_agreement_id <- cid[match(cc$source_record_id, ids)]
  if (nrow(cc)) {
    key <- paste(cc$canonical_agreement_id, cc$dimension, cc$code, cc$basis, sep = "\r")
    cc <- cc[order(key, -cc$n_hits, method = "radix"), , drop = FALSE]
    key <- paste(cc$canonical_agreement_id, cc$dimension, cc$code, cc$basis, sep = "\r")
    nrec <- tapply(cc$source_record_id, key, function(v) length(unique(v)))
    first <- !duplicated(key)
    canonical_codes <- data.frame(canonical_agreement_id = cc$canonical_agreement_id[first], dimension = cc$dimension[first],
                                  code = cc$code[first], basis = cc$basis[first], n_hits = cc$n_hits[first],
                                  n_records = as.integer(nrec[key[first]]), snippet = cc$snippet[first],
                                  source_record_id = cc$source_record_id[first], stringsAsFactors = FALSE)
  } else {
    canonical_codes <- data.frame(canonical_agreement_id = character(), dimension = character(), code = character(),
                                  basis = character(), n_hits = integer(), n_records = integer(), snippet = character(),
                                  source_record_id = character(), stringsAsFactors = FALSE)
  }
  rownames(canonical_codes) <- NULL

  # Canonical records by field precedence.
  verification <- agr_link_verification(extras)
  delivery <- unlist(cfg$delivery_statuses)
  first_val <- function(v, skip = "") { v <- v[nzchar(v) & !(v %in% skip)]; if (length(v)) v[1] else "" }
  rows <- lapply(seq_along(members), function(k) {
    m <- members[[k]]
    m <- m[order(rank_in[m])]
    r <- records[m, , drop = FALSE]
    lead <- r[1, , drop = FALSE]
    out <- lead
    out$title <- first_val(r$title)
    out$agreement_type <- first_val(r$agreement_type, "other")
    if (!nzchar(out$agreement_type)) out$agreement_type <- "other"
    out$agreement_type_raw <- first_val(r$agreement_type_raw)
    # The highest partnership tier any member names (config partnership_labels order), not the first.
    pl <- r$partnership_label[nzchar(r$partnership_label)]
    out$partnership_label <- if (length(pl)) pl[order(match(pl, names(cfg$partnership_labels)))][1] else ""
    li <- which(r$legal_status != "unknown")[1]
    if (!is.na(li)) { out$legal_status <- r$legal_status[li]; out$legal_status_basis <- r$legal_status_basis[li] }
    for (f in c("announcement_date", "signature_date", "effective_date", "expiry_date")) out[[f]] <- first_val(r[[f]])
    out$primary_date <- agr_primary_date(out$signature_date, out$announcement_date, out$effective_date)
    out$date_precision <- agr_date_precision(out$primary_date)
    out$relationship_category <- first_val(r$relationship_category, "other")
    if (!nzchar(out$relationship_category)) out$relationship_category <- "other"
    si <- which(r$implementation_status != "unknown")[1]
    if (is.na(si)) si <- 1L
    if (!(r$implementation_status[si] %in% c("inactive", "cancelled"))) {
      di <- which(r$implementation_status %in% delivery)[1]
      if (!is.na(di)) si <- di
    }
    out$implementation_status <- r$implementation_status[si]
    out$implementation_status_raw <- r$implementation_status_raw[si]
    off <- which(agr_is_official_url(r$source_url, unlist(cfg$official_hosts)))[1]
    out$source_url <- if (!is.na(off)) r$source_url[off] else first_val(r$source_url)
    out$primary_document_url <- first_val(r$primary_document_url)
    out$primary_source_available <- any(r$primary_source_available)
    lv <- r$last_verified[nzchar(r$last_verified)]
    out$last_verified <- if (length(lv)) max(lv) else ""
    su <- r$source_last_updated[nzchar(r$source_last_updated)]
    out$source_last_updated <- if (length(su)) max(su) else ""
    out$description <- first_val(r$description)
    ds <- unique(r$source_dataset)
    e <- acc[acc$from %in% r$source_record_id | acc$to %in% r$source_record_id, , drop = FALSE]
    score_min <- if (nrow(e)) min(e$score) else NA_real_
    # Matching uncertainty comes from title/date matches; the trade rules match on the pair or the
    # plurilateral's membership and an entry-into-force year, whose score only measures the date gap
    # (CAFTA-DR enters into force for Costa Rica three years after the others).
    fuzzy <- e[!(e$rule %in% c("trade_leg", "trade_pair_year")), , drop = FALSE]
    score_min_fuzzy <- if (nrow(fuzzy)) min(fuzzy$score) else NA_real_
    vf <- verification[r$source_record_id]
    vf <- vf[!is.na(vf)]
    all_unverified <- all(r$source_dataset %in% c("curated", "pmemo")) && length(vf) == nrow(r) && all(vf == "unverified")
    conf <- if (!is.na(score_min_fuzzy) && score_min_fuzzy < 0.75) "low"
            else if (out$primary_source_available || length(ds) >= 2) "high"
            else if (all_unverified) "low"
            else "medium"
    cbind(data.frame(canonical_agreement_id = cid[m[1]], stringsAsFactors = FALSE),
          out[, setdiff(AGR_RECORD_COLUMNS, c("source_dataset", "source_record_id")), drop = FALSE],
          data.frame(lead_record = lead$source_record_id, source_records = paste(r$source_record_id, collapse = "|"),
                     source_datasets = paste(ds, collapse = "|"), source_count = length(ds), n_records = nrow(r),
                     link_rules = paste(sort(unique(e$rule)), collapse = "|"), link_score_min = score_min,
                     confidence = conf, stringsAsFactors = FALSE))
  })
  canonical <- do.call(rbind, rows)
  rownames(canonical) <- NULL
  # Multi-valued fields from the union of codes.
  tmp <- canonical
  tmp$source_record_id <- tmp$canonical_agreement_id
  tmp <- agr_apply_codes(tmp, data.frame(source_record_id = canonical_codes$canonical_agreement_id,
                                         dimension = canonical_codes$dimension, code = canonical_codes$code,
                                         basis = canonical_codes$basis, n_hits = canonical_codes$n_hits,
                                         snippet = rep("", nrow(canonical_codes)), stringsAsFactors = FALSE))
  for (dim in AGR_CODE_DIMENSIONS) canonical[[dim]] <- tmp[[dim]]
  canonical <- canonical[order(canonical$canonical_agreement_id, method = "radix"), , drop = FALSE]
  rownames(canonical) <- NULL
  if (anyDuplicated(canonical$canonical_agreement_id)) stop("agr_link_records: canonical id collision")
  list(canonical = canonical, links = links, canonical_codes = canonical_codes, candidates = cand)
}
