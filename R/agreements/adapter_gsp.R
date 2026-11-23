# Global Strategic Partnership Dataset (GSP) adapter (docs/partnership_agreements.md, Sources).
#
# Input: the staged data/raw/gsp_partnership_documents.csv (scripts/fetchers/gsp_partnerships.R),
# or the upstream db_world*.csv, read with every column as character. Rows are directed
# documents: country1 is the government that published the document, country2 the partner.
# Output: list(records, codes, extras) in the common schema (R/agreements/agr_schema.R), one
# record per document. Pure: no IO. Needs agr_schema.R and agr_text_coding.R.
#
# Rules (the GSP type field is the only structured classification the source has):
#   agreement_type   Treaty -> treaty; Joint Statement -> joint_statement, or joint_declaration
#                    when the title says "Joint Declaration"; Action Plan -> framework; Joint
#                    Press Conference, News, Factsheet and blank -> other.
#   legal_status     Treaty -> binding; Joint Statement / Declaration, Joint Press Conference and
#                    Action Plan -> non_binding (basis source_document_type); News, Factsheet and
#                    blank -> unknown (basis none). Never read off the title.
#   dates            the source date (else year-month-day, else year) is the signature date of a
#                    treaty and the announcement date of everything else; status signed or
#                    announced accordingly.
#   partnership_label the partnership the title names, else the one body sentences name for the
#                    pair (conservative, see gsp_body_partnership_label()); basis in notes.
#   codes            keyword codes of title and full text (text_title / text_body) only: GSP
#                    codes no content, so there are no source_coded codes.
# Rows dropped (counted in extras$gsp_dropped): exact duplicates, a party that is not a state
# with an ISO3 code (GE-AB, Abkhazia), any other non-ISO3 code, and self-pairs.

GSP_V1_RELEASE_DATE <- "2025-09-23"

GSP_SOURCE_COLUMNS <- c(
  "doc_id", "country1_cown", "country1_iso3c", "country2_cown", "country2_iso3c",
  "year", "month", "day", "date", "title", "fulltext", "confidence_level1", "confidence_level2",
  "source", "title_sp", "factsheet_sp", "type"
)

# Upstream party codes that are not ISO3: mapped, or dropped as non-state entities.
GSP_PARTY_RECODE <- c(KOSOVO = "XKX")
GSP_PARTY_DROP <- c("GE-AB")

# Source type -> agreement type, legal status and its basis (keys lower case).
GSP_TYPE_RULES <- data.frame(
  type = c("treaty", "joint statement", "action plan", "joint press conference", "news", "factsheet", ""),
  agreement_type = c("treaty", "joint_statement", "framework", "other", "other", "other", "other"),
  legal_status = c("binding", "non_binding", "non_binding", "non_binding", "unknown", "unknown", "unknown"),
  stringsAsFactors = FALSE
)

# "Joint Declaration" in a title, in the languages GSP titles use.
GSP_JOINT_DECLARATION_PATTERN <- paste(
  "\\bjoint declarations?\\b", "déclaration (commune|conjointe)", "declaración conjunta",
  "declarați[ae] comună", "dichiarazione congiunta", "gemeinsame erklärung",
  "совместн[а-яё]* декларац",
  "联合宣言", "聯合宣言", "共同宣言", "공동선언",
  sep = "|"
)

# Partnership tiers, tested in this order (the first tier that matches a title wins; body counts
# are exclusive: a phrase counted for a higher tier is removed before lower tiers are counted).
# Case-insensitive PCRE. Cyrillic and Greek use explicit letter classes (\w is ASCII-only).
GSP_CYR <- "[а-яёєіїђћјњљџ]*"
# Words allowed between "strategic" and "partnership" ("Strategic, Security and Economic
# Partnership", "strategic cooperative partnership", "Strategic Cooperation and Partnership").
GSP_SP_MIDDLE <- "(?:,? (?:and )?(?:security|economic|cooperative|cooperation|coordination|political|defen[cs]e|global|trade|development)){0,3} (?:and )?partnership"
GSP_LABEL_PATTERNS <- list(
  comprehensive_strategic = c(
    paste0("comprehensive,? (and |global |strategic and )?strategic", GSP_SP_MIDDLE),
    "strategic and comprehensive partnership",
    "comprehensive partnership (and|of) strategic (coordination|cooperation|interaction)",
    "partenariat (stratégique global|global et stratégique|global stratégique)",
    "asociaci[oó]n estrat[eé]gica integral", "parceria estrat[eé]gica (global|abrangente)",
    paste0("всеобъемлющ", GSP_CYR, " стратегическ", GSP_CYR, " партн"),
    paste0("(всеобъемлющ|всесторонн)", GSP_CYR, " партн", GSP_CYR, " и стратегическ"),
    paste0("свеобухватн", GSP_CYR, " стратешк", GSP_CYR, " партн"),
    "全面战略(合作|协作)?伙伴关系",
    "全面戰略(合作|協作)?[夥伙]伴關係",
    "包括的戦略的パートナーシップ",
    "포괄적 ?전략적 ?(협력 ?)?동반자",
    "đối tác chiến lược toàn diện"
  ),
  special_strategic = c(
    "special (and privileged )?strategic (and global |global )?partnership",
    "privileged strategic partnership",
    "特別な?戦略的(グローバル・)?パートナーシップ",
    "특별 ?전략적 ?동반자",
    paste0("особо привилегированн", GSP_CYR, " стратегическ", GSP_CYR, " партн")
  ),
  global_strategic = c(
    "global strategic partnership",
    "strategic and global partnership",
    "戦略的グローバル・パートナーシップ",
    "グローバルな戦略的パートナーシップ"
  ),
  strategic = c(
    paste0("strategic", GSP_SP_MIDDLE),
    "partenariat stratégique", "asociaci[oó]n estrat[eé]gica", "parceria estrat[eé]gica",
    "partenariato strategico", "strategische[nr]? partnerschaft", "parteneriat\\p{L}* strategic",
    "strategick\\p{L}* partnerstv", "partnerstw\\p{L}* strategiczn", "strategiczn\\p{L}* partnerstw",
    paste0("стратегическ", GSP_CYR, " партн"),
    paste0("стратегічн", GSP_CYR, " партн"),
    paste0("стратешк", GSP_CYR, " партн"),
    "στρατηγικ\\p{Greek}* εταιρικ",
    "战略(合作|协作)?伙伴关系",
    "戰略(合作|協作)?[夥伙]伴關係",
    "戦略的パートナーシップ",
    "전략적 ?(협력 ?)?동반자",
    "đối tác chiến lược"
  ),
  comprehensive = c(
    "comprehensive (cooperative |cooperation )?partnership",
    "partenariat global", "asociaci[oó]n integral",
    paste0("(всеобъемлющ|всесторонн)", GSP_CYR, " партн"),
    "全面(合作)?伙伴关系", "全面(合作)?[夥伙]伴關係",
    "包括的パートナーシップ",
    "포괄적 ?(협력 ?)?동반자",
    "đối tác toàn diện"
  )
)

# Title only, after the tiers: a partnership with its own name ("Vuvale partnership", "Economic
# Partnership", "Partnership Agreement", "Friendship and Partnership"), or the word in another
# language.
GSP_OTHER_PARTNERSHIP_PATTERN <- paste(
  "\\b(?!(?:the|a|an|and|of|for|on|in|to|this|that|our|their|its|bilateral|into|with|by|at|as|strengthen|strengthening|enhance|enhancing|deepen|deepening|build|building|establish|establishing)\\b)[a-z][a-z-]+ partnerships?\\b",
  "\\bpartnerships? (agreement|treaty|charter|framework|declaration|programme|program|plan|council)\\b",
  "\\b(friendship|cooperation|co-operation|good-neighbourliness|good-neighborliness),? and partnership\\b",
  "partenariat", "asociaci[oó]n", "partenariato", "parceria", "partnerschaft", "parteneriat", "partnerstv", "partnerstw",
  paste0("партн", GSP_CYR, "ств"), "伙伴关系", "[夥伙]伴關係",
  "パートナーシップ", "동반자",
  sep = "|"
)

# A body sentence names the pair's own partnership only when it also speaks of the two sides
# ("the two countries", "bilateral", "our ... partnership", a "U.S.-India ... Partnership"
# compound, "双方") and names no regional grouping, whose partnership it may be.
GSP_BODY_PAIR_CUE <- paste(
  "\\b(two|both) (countries|sides|nations|leaders|governments|states|parties|prime ministers|presidents|ministers|peoples)\\b",
  "\\bbilateral\\b", "\\bour (two )?(countries|nations|relationship|relations|partnership)\\b",
  "\\bour( [a-z-]+){1,4} partnership\\b",
  "(?-i:\\b[A-Z][A-Za-z.]*[-\u2013][A-Z][A-Za-z.]*)( [A-Za-z-]+){0,4} partnership\\b",
  "\u4e2d\\p{Han}(\u5168\u9762|\u6218\u7565|\u6230\u7565)",
  "\\bbetween (the two|our|both)\\b", "\\b(elevat|upgrad|establish|launch|built|build)", "\\brelations(hip)?\\b",
  "双方|两国|雙方|兩國|両国|日[^。]{1,4}関係|양국",
  "deux pays|les deux parties|bilatéral", "ambos países|dos países|ambas partes|bilateral",
  paste0("двусторонн|обе сторон|двух стран|отношени"),
  sep = "|"
)
GSP_BODY_EXCLUDE <- "\\b(ASEAN|EU|European Union|African Union|NATO|Mercosur|CELAC|GCC|SCO|BRICS|Pacific Islands Forum|Quad|G7|G20|APEC)\\b|东盟|欧盟|東盟|歐盟"

gsp_rx <- function(patterns) paste0("(?:", paste(patterns, collapse = ")|(?:"), ")")

gsp_squish <- function(x) trimws(gsub("\\s+", " ", agr_chr(x)))

# Partnership tier a title names ("" when none): the first tier of GSP_LABEL_PATTERNS that
# matches, else other_partnership for GSP_OTHER_PARTNERSHIP_PATTERN.
gsp_title_partnership_label <- function(titles) {
  titles <- gsp_squish(titles)
  out <- character(length(titles))
  for (tier in names(GSP_LABEL_PATTERNS)) {
    hit <- !nzchar(out) & grepl(gsp_rx(GSP_LABEL_PATTERNS[[tier]]), titles, perl = TRUE, ignore.case = TRUE)
    out[hit] <- tier
  }
  other <- !nzchar(out) & grepl(GSP_OTHER_PARTNERSHIP_PATTERN, titles, perl = TRUE, ignore.case = TRUE)
  out[other] <- "other_partnership"
  out
}

# Partnership tier the body names for the pair: among sentences that name a tier, speak of the two
# sides (GSP_BODY_PAIR_CUE) and name no regional grouping (GSP_BODY_EXCLUDE), the tier named most
# often (each phrase counted once, for its highest tier); ties go to the higher tier. Returns
# data.frame(label, n_sentences) aligned with ids.
gsp_body_partnership_label <- function(ids, bodies, max_chars = 600L) {
  ids <- agr_chr(ids)
  out <- data.frame(label = character(length(ids)), n_sentences = integer(length(ids)), stringsAsFactors = FALSE)
  all_tiers <- gsp_rx(unlist(GSP_LABEL_PATTERNS, use.names = FALSE))
  bodies <- agr_chr(bodies)
  has <- nzchar(bodies) & grepl(all_tiers, bodies, perl = TRUE, ignore.case = TRUE)
  if (!any(has)) return(out)
  s <- agr_split_sentences(ids[has], bodies[has], max_chars)
  s <- s[grepl(all_tiers, s$sentence, perl = TRUE, ignore.case = TRUE), , drop = FALSE]
  s <- s[grepl(GSP_BODY_PAIR_CUE, s$sentence, perl = TRUE, ignore.case = TRUE) &
           !grepl(GSP_BODY_EXCLUDE, s$sentence, perl = TRUE), , drop = FALSE]
  if (nrow(s) == 0) return(out)
  txt <- s$sentence
  counts <- matrix(0L, nrow(s), length(GSP_LABEL_PATTERNS), dimnames = list(NULL, names(GSP_LABEL_PATTERNS)))
  for (tier in names(GSP_LABEL_PATTERNS)) {
    rx <- gsp_rx(GSP_LABEL_PATTERNS[[tier]])
    m <- gregexpr(rx, txt, perl = TRUE, ignore.case = TRUE)
    counts[, tier] <- vapply(m, function(v) sum(v > 0), integer(1))
    txt <- gsub(rx, " ", txt, perl = TRUE, ignore.case = TRUE)
  }
  by_doc <- rowsum(counts, s$doc, reorder = FALSE)
  n_sent <- table(s$doc)
  best <- apply(by_doc, 1, function(v) if (max(v) > 0) names(v)[which.max(v)] else "")
  idx <- match(rownames(by_doc), ids)
  out$label[idx] <- unname(best)
  out$n_sentences[idx] <- as.integer(n_sent[rownames(by_doc)])
  out
}

# Script of a text, from its first 4,000 characters: "cjk" when Han, kana or Hangul make up at
# least 30% of the letters, "latin" when Latin letters make up at least half, "other" (Cyrillic,
# Greek, Arabic, ...) otherwise, "none" without letters.
gsp_language_guess <- function(texts) {
  t <- substr(agr_chr(texts), 1, 4000)
  letters_n <- nchar(gsub("\\P{L}", "", t, perl = TRUE))
  latin <- nchar(gsub("\\P{Latin}", "", t, perl = TRUE))
  cjk <- nchar(gsub("[^\\p{Han}\\p{Hiragana}\\p{Katakana}\\p{Hangul}]", "", t, perl = TRUE))
  ifelse(letters_n == 0, "none",
         ifelse(cjk / pmax(letters_n, 1) >= 0.3, "cjk",
                ifelse(latin / pmax(letters_n, 1) >= 0.5, "latin", "other")))
}

# Source URL as a reader can open it: a PDF-viewer wrapper
# ("chrome-extension://<id>/https://www.mofa.go.jp/...") is unwrapped to the URL it wraps.
gsp_clean_url <- function(url) {
  url <- gsp_squish(url)
  sub("^chrome-extension://[A-Za-z0-9]+/(https?://)", "\\1", url)
}

gsp_logical <- function(x) {
  x <- toupper(agr_chr(x))
  ifelse(x %in% c("TRUE", "T", "1", "YES"), TRUE, ifelse(x %in% c("FALSE", "F", "0", "NO"), FALSE, NA))
}

# Party code -> ISO3 ("" when it is not one); GE-AB and other non-state codes are flagged.
gsp_party_iso3 <- function(x) {
  x <- agr_chr(x)
  up <- toupper(x)
  out <- ifelse(up %in% names(GSP_PARTY_RECODE), unname(GSP_PARTY_RECODE[up]), up)
  out[toupper(x) %in% toupper(GSP_PARTY_DROP)] <- ""
  out[!grepl("^[A-Z]{3}$", out)] <- ""
  out
}

# Date of each row: the source date, else year-month-day, else year-month, else year (normalised;
# an invalid day falls back to the month).
gsp_row_date <- function(date, year, month, day) {
  d <- agr_norm_date(date)
  y <- suppressWarnings(as.integer(agr_chr(year)))
  m <- suppressWarnings(as.integer(agr_chr(month)))
  dd <- suppressWarnings(as.integer(agr_chr(day)))
  built <- ifelse(is.na(y), "",
                  ifelse(is.na(m), sprintf("%04d", y),
                         ifelse(is.na(dd), sprintf("%04d-%02d", y, m), sprintf("%04d-%02d-%02d", y, m, dd))))
  built[is.na(y)] <- ""
  ifelse(nzchar(d), d, agr_norm_date(built))
}

# Country display names for descriptions (ISO3 when unknown).
gsp_country_names <- function(iso3) {
  iso3 <- unique(agr_chr(iso3))
  nm <- iso3
  if (requireNamespace("countrycode", quietly = TRUE)) {
    cc <- suppressWarnings(countrycode::countrycode(iso3, "iso3c", "country.name", custom_match = c(XKX = "Kosovo")))
    nm <- ifelse(is.na(cc) | !nzchar(cc), iso3, cc)
  }
  stats::setNames(nm, iso3)
}

# GSP rows -> list(records, codes, extras). `raw`: the staged file or the upstream CSV, columns
# as character (other types are coerced). `fetched_on`: the date the copy was fetched (last
# verified); "" takes the file's fetched_on column when present. `country_names`: optional named vector
# ISO3 -> display name for descriptions (default: countrycode, else the ISO3 code).
agr_adapt_gsp <- function(raw, cfg, fetched_on = "", country_names = NULL) {
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  names(raw) <- sub(paste0("^", intToUtf8(0xFEFF)), "", trimws(names(raw)))
  missing <- setdiff(c("doc_id", "country1_iso3c", "country2_iso3c", "title", "type"), names(raw))
  if (length(missing)) stop("GSP input lacks column(s): ", paste(missing, collapse = ", "))
  for (col in setdiff(c(GSP_SOURCE_COLUMNS, "dataset_version", "fetched_on"), names(raw))) raw[[col]] <- ""
  for (col in c(GSP_SOURCE_COLUMNS, "dataset_version", "fetched_on")) raw[[col]] <- agr_chr(raw[[col]])

  dropped <- list()
  drop <- function(keep, reason) {
    if (any(!keep)) {
      dropped[[length(dropped) + 1]] <<- data.frame(doc_id = raw$doc_id[!keep], country1_iso3c = raw$country1_iso3c[!keep],
                                                    country2_iso3c = raw$country2_iso3c[!keep], reason = reason,
                                                    stringsAsFactors = FALSE)
    }
    raw <<- raw[keep, , drop = FALSE]
  }

  drop(!duplicated(raw[, GSP_SOURCE_COLUMNS, drop = FALSE]), "exact_duplicate")
  non_state <- toupper(raw$country1_iso3c) %in% toupper(GSP_PARTY_DROP) | toupper(raw$country2_iso3c) %in% toupper(GSP_PARTY_DROP)
  drop(!non_state, "non_state_party")
  pub <- gsp_party_iso3(raw$country1_iso3c)
  par <- gsp_party_iso3(raw$country2_iso3c)
  drop(nzchar(pub) & nzchar(par), "invalid_iso3")
  pub <- gsp_party_iso3(raw$country1_iso3c)
  par <- gsp_party_iso3(raw$country2_iso3c)
  drop(pub != par, "self_pair")
  pub <- gsp_party_iso3(raw$country1_iso3c)
  par <- gsp_party_iso3(raw$country2_iso3c)
  dropped <- if (length(dropped)) do.call(rbind, dropped) else
    data.frame(doc_id = character(), country1_iso3c = character(), country2_iso3c = character(), reason = character(),
               stringsAsFactors = FALSE)

  if (nrow(raw) == 0) {
    return(list(records = agr_empty_records(), codes = agr_empty_codes(),
                extras = list(gsp_documents = data.frame(), gsp_dropped = dropped)))
  }

  # source_record_id: "gsp:" + doc_id (whitespace removed); repeated ids get "-2", "-3", ... in the
  # order (date, source, title, text length).
  doc <- gsub("\\s+", "", raw$doc_id)
  date <- gsp_row_date(raw$date, raw$year, raw$month, raw$day)
  doc <- ifelse(nzchar(doc), doc, paste0(pub, par, gsub("-", "", date)))
  base_id <- paste0("gsp:", doc)
  ord <- order(base_id, date, raw$source, raw$title, nchar(raw$fulltext), method = "radix")
  seq_in <- integer(length(base_id))
  seq_in[ord] <- stats::ave(seq_along(ord), base_id[ord], FUN = seq_along)
  ids <- ifelse(seq_in > 1L, paste0(base_id, "-", seq_in), base_id)

  type_raw <- gsp_squish(raw$type)
  rule <- match(tolower(type_raw), GSP_TYPE_RULES$type)
  agreement_type <- ifelse(is.na(rule), "other", GSP_TYPE_RULES$agreement_type[rule])
  legal_status <- ifelse(is.na(rule), "unknown", GSP_TYPE_RULES$legal_status[rule])
  title <- gsp_squish(raw$title)
  jd <- agreement_type == "joint_statement" & grepl(GSP_JOINT_DECLARATION_PATTERN, title, perl = TRUE, ignore.case = TRUE)
  agreement_type[jd] <- "joint_declaration"
  is_treaty <- agreement_type == "treaty"

  label_title <- gsp_title_partnership_label(title)
  max_chars <- if (is.null(cfg$text_coding$max_sentence_chars)) 600L else as.integer(cfg$text_coding$max_sentence_chars)
  need_body <- !nzchar(label_title)
  body <- gsp_body_partnership_label(ids[need_body], raw$fulltext[need_body], max_chars)
  label <- label_title
  label[need_body] <- body$label
  label_note <- ifelse(nzchar(label_title), "partnership label from title", "")
  label_note[need_body] <- ifelse(nzchar(body$label),
                                  sprintf("partnership label from body (%d sentence%s)", body$n_sentences,
                                          ifelse(body$n_sentences == 1L, "", "s")), "")

  source_url <- gsp_clean_url(raw$source)
  official <- agr_is_official_url(source_url, cfg$official_hosts)
  version_date <- regmatches(raw$dataset_version, regexpr("\\d{4}-\\d{2}-\\d{2}", raw$dataset_version))
  source_last_updated <- rep(GSP_V1_RELEASE_DATE, nrow(raw))
  has_v <- grepl("\\d{4}-\\d{2}-\\d{2}", raw$dataset_version)
  source_last_updated[has_v] <- version_date
  fetched_on <- agr_chr(fetched_on)
  last_verified <- if (length(fetched_on) == 1 && nzchar(fetched_on)) rep(fetched_on, nrow(raw)) else raw$fetched_on

  nm <- if (is.null(country_names)) gsp_country_names(pub) else country_names
  pub_name <- unname(nm[pub])
  pub_name[is.na(pub_name) | !nzchar(pub_name)] <- pub[is.na(pub_name) | !nzchar(pub_name)]
  doc_kind <- ifelse(nzchar(type_raw), type_raw, "Document")

  pf <- agr_party_fields(lapply(seq_len(nrow(raw)), function(i) c(pub[i], par[i])))
  records <- data.frame(
    source_dataset = "gsp",
    source_record_id = ids,
    title = title,
    pf,
    agreement_type = agreement_type,
    agreement_type_raw = type_raw,
    partnership_label = label,
    legal_status = legal_status,
    legal_status_basis = ifelse(legal_status == "unknown", "none", "source_document_type"),
    announcement_date = ifelse(is_treaty, "", date),
    signature_date = ifelse(is_treaty, date, ""),
    relationship_category = "overall_relationship",
    implementation_status = ifelse(is_treaty, "signed", "announced"),
    implementation_status_raw = type_raw,
    source_url = source_url,
    primary_document_url = ifelse(official, source_url, ""),
    primary_source_available = official,
    last_verified = last_verified,
    source_last_updated = source_last_updated,
    description = paste(doc_kind, "published by", pub_name),
    notes = paste0("publisher: ", pub, "; ", label_note),
    stringsAsFactors = FALSE
  )
  records$notes <- sub(";\\s*$", "", records$notes)
  records <- agr_as_records(records)

  codes <- agr_code_documents(ids, title, raw$fulltext, cfg)
  # A keyword in the headline of a news item or fact sheet (type other) is a mention, not the subject
  # of an agreement: its title codes count as body codes (one hit), so they never reach the specific
  # grade ("...exporter of the fuel of the future - green hydrogen, the President said").
  other_ids <- ids[records$agreement_type[match(ids, records$source_record_id)] == "other"]
  demote <- codes$basis == "text_title" & codes$source_record_id %in% other_ids
  if (any(demote)) {
    codes$basis[demote] <- "text_body"
    codes$n_hits[demote] <- 1L
    codes <- agr_as_codes(codes)
  }
  records <- agr_apply_codes(records, codes)

  documents <- data.frame(
    source_record_id = ids,
    doc_id = raw$doc_id,
    publisher_iso3 = pub,
    partner_iso3 = par,
    type_raw = type_raw,
    title_sp = gsp_logical(raw$title_sp),
    factsheet_sp = gsp_logical(raw$factsheet_sp),
    n_chars = nchar(raw$fulltext),
    language = gsp_language_guess(ifelse(nzchar(raw$fulltext), raw$fulltext, title)),
    label_basis = ifelse(nzchar(label_title), "title", ifelse(nzchar(label), "body", "")),
    stringsAsFactors = FALSE
  )
  documents <- documents[order(documents$source_record_id), , drop = FALSE]
  rownames(documents) <- NULL
  records <- records[order(records$source_record_id), , drop = FALSE]
  rownames(records) <- NULL

  list(records = records, codes = codes,
       extras = list(gsp_documents = documents, gsp_dropped = dropped))
}
