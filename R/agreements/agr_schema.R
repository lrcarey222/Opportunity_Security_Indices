# Common agreement schema (docs/partnership_agreements.md, "Common schema").
#
# Every source adapter returns a list(records = <data.frame>, codes = <data.frame>):
#   records: one row per source record, columns AGR_RECORD_COLUMNS (all character except
#            n_parties integer and primary_source_available logical). Multi-valued fields are
#            "|"-joined, sorted and de-duplicated (AGR_MULTI_FIELDS).
#   codes:   the evidence behind the multi-valued fields, one row per (record, dimension, code,
#            basis): AGR_CODE_COLUMNS. `sector`, `technology`, `supply_chain_segment` and
#            `instrument_types` in records are always the collapse of codes (agr_apply_codes()).
# The controlled vocabularies live in config/partnership_agreements.yml; scripts read the YAML
# and pass the parsed list (`cfg`) in. Pure: no IO.

AGR_SOURCES <- c("gsp", "desta", "memberships", "curated", "pmemo", "iisd")

AGR_RECORD_COLUMNS <- c(
  "source_dataset", "source_record_id", "title",
  "parties", "country_a", "country_b", "additional_parties", "n_parties",
  "bilateral_or_plurilateral",
  "agreement_type", "agreement_type_raw", "partnership_label", "legal_status", "legal_status_basis",
  "announcement_date", "signature_date", "effective_date", "expiry_date",
  "primary_date", "date_precision",
  "relationship_category", "sector", "technology", "supply_chain_segment", "instrument_types",
  "implementation_status", "implementation_status_raw",
  "source_url", "primary_document_url", "primary_source_available",
  "last_verified", "source_last_updated",
  "description", "notes"
)

AGR_MULTI_FIELDS <- c("parties", "additional_parties", "sector", "technology",
                      "supply_chain_segment", "instrument_types")

AGR_DATE_FIELDS <- c("announcement_date", "signature_date", "effective_date", "expiry_date",
                     "primary_date", "last_verified", "source_last_updated")

AGR_CODE_COLUMNS <- c("source_record_id", "dimension", "code", "basis", "n_hits", "snippet")

AGR_CODE_DIMENSIONS <- c("sector", "technology", "supply_chain_segment", "instrument_types")

# What the relationship is about, at the coarsest level (one value per record).
AGR_RELATIONSHIP_CATEGORIES <- c(
  overall_relationship = "Overall political relationship",
  trade_economic = "Trade and economic agreement",
  sectoral = "Sector-specific cooperation",
  investment_finance = "Investment or financing vehicle",
  plurilateral_club = "Plurilateral club or forum",
  other = "Other"
)

AGR_SEGMENT_CROSS <- "cross_stage"

# --- small helpers ------------------------------------------------------------------------

agr_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

# "|"-join a character vector into one sorted, de-duplicated field ("" when empty).
agr_join <- function(x) {
  x <- agr_chr(unlist(x, use.names = FALSE))
  x <- sort(unique(x[nzchar(x)]))
  paste(x, collapse = "|")
}

# Split "|"-joined fields into a list of character vectors.
agr_split <- function(x) {
  lapply(strsplit(agr_chr(x), "|", fixed = TRUE), function(v) v[nzchar(v)])
}

# Unordered pair key "AAA|BBB".
agr_pair_key <- function(a, b) {
  a <- agr_chr(a)
  b <- agr_chr(b)
  ifelse(a <= b, paste(a, b, sep = "|"), paste(b, a, sep = "|"))
}

# Party fields from a list of party vectors (ISO3, EUU allowed): sorted parties, country_a and
# country_b (the first two in sort order), additional_parties, n_parties and
# bilateral_or_plurilateral. A record keeps its parties in this canonical order so that the same
# agreement from two sources has the same party key.
agr_party_fields <- function(party_list) {
  party_list <- lapply(party_list, function(p) sort(unique(agr_chr(p)[nzchar(agr_chr(p))])))
  n <- vapply(party_list, length, integer(1))
  data.frame(
    parties = vapply(party_list, paste, character(1), collapse = "|"),
    country_a = vapply(party_list, function(p) if (length(p) >= 1) p[1] else "", character(1)),
    country_b = vapply(party_list, function(p) if (length(p) >= 2) p[2] else "", character(1)),
    additional_parties = vapply(party_list, function(p) if (length(p) > 2) paste(p[-(1:2)], collapse = "|") else "", character(1)),
    n_parties = n,
    bilateral_or_plurilateral = ifelse(n == 2L, "bilateral", "plurilateral"),
    stringsAsFactors = FALSE
  )
}

# Normalise a date to "YYYY-MM-DD", "YYYY-MM" or "YYYY" ("" when unusable). Accepts Date,
# "YYYY-MM-DD", "YYYY/MM/DD", "YYYY-MM", "YYYY", "DD/MM/YYYY" and numeric years. Invalid days
# (e.g. 2019-05-60) fall back to the month.
agr_norm_date <- function(x) {
  if (inherits(x, "Date")) return(ifelse(is.na(x), "", format(x, "%Y-%m-%d")))
  x <- agr_chr(x)
  out <- character(length(x))
  for (i in seq_along(x)) {
    v <- x[i]
    if (!nzchar(v)) next
    v <- sub("T.*$", "", v)
    if (grepl("^\\d{4}$", v)) {
      out[i] <- v
    } else if (grepl("^\\d{4}\\.0+$", v)) {
      out[i] <- sub("\\..*$", "", v)
    } else if (grepl("^\\d{4}[-/]\\d{1,2}$", v)) {
      p <- as.integer(strsplit(v, "[-/]")[[1]])
      out[i] <- if (p[2] >= 1 && p[2] <= 12) sprintf("%04d-%02d", p[1], p[2]) else sprintf("%04d", p[1])
    } else if (grepl("^\\d{4}[-/]\\d{1,2}[-/]\\d{1,2}$", v)) {
      p <- as.integer(strsplit(v, "[-/]")[[1]])
      d <- as.Date(sprintf("%04d-%02d-%02d", p[1], p[2], p[3]), format = "%Y-%m-%d")
      out[i] <- if (!is.na(d)) format(d, "%Y-%m-%d") else if (p[2] >= 1 && p[2] <= 12) sprintf("%04d-%02d", p[1], p[2]) else sprintf("%04d", p[1])
    } else if (grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", v)) {
      p <- as.integer(strsplit(v, "/")[[1]])
      d <- as.Date(sprintf("%04d-%02d-%02d", p[3], p[2], p[1]), format = "%Y-%m-%d")
      out[i] <- if (!is.na(d)) format(d, "%Y-%m-%d") else sprintf("%04d", p[3])
    }
  }
  out
}

agr_date_precision <- function(x) {
  x <- agr_chr(x)
  ifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", x), "day",
         ifelse(grepl("^\\d{4}-\\d{2}$", x), "month",
                ifelse(grepl("^\\d{4}$", x), "year", "none")))
}

# A normalised date as a Date (the first day of its month or year when less precise; NA if none).
agr_date_value <- function(x) {
  x <- agr_chr(x)
  full <- ifelse(grepl("^\\d{4}$", x), paste0(x, "-01-01"),
                 ifelse(grepl("^\\d{4}-\\d{2}$", x), paste0(x, "-01"), x))
  as.Date(ifelse(nzchar(full), full, NA_character_), format = "%Y-%m-%d")
}

agr_date_year <- function(x) {
  y <- suppressWarnings(as.integer(substr(agr_chr(x), 1, 4)))
  y
}

# primary_date: signature, else announcement, else effective date.
agr_primary_date <- function(signature_date, announcement_date, effective_date) {
  s <- agr_chr(signature_date)
  a <- agr_chr(announcement_date)
  e <- agr_chr(effective_date)
  ifelse(nzchar(s), s, ifelse(nzchar(a), a, e))
}

# Host of a URL, unwrapping web.archive.org links to the archived host.
agr_url_host <- function(url) {
  url <- agr_chr(url)
  archived <- sub("^https?://web\\.archive\\.org/web/[^/]+/", "", url)
  host <- sub("^[a-z]+://", "", tolower(archived))
  host <- sub("[/:?#].*$", "", host)
  sub("^www\\.", "", host)
}

# TRUE where a URL's host matches one of the official-host patterns (config official_hosts).
agr_is_official_url <- function(url, official_hosts) {
  host <- agr_url_host(url)
  pat <- paste0("(", paste(official_hosts, collapse = ")|("), ")")
  nzchar(host) & grepl(pat, host, perl = TRUE, ignore.case = TRUE)
}

# --- vocabularies -------------------------------------------------------------------------

# The controlled vocabularies of the parsed config.
agr_vocab <- function(cfg) {
  list(
    sources = names(cfg$sources),
    agreement_type = names(cfg$agreement_types),
    partnership_label = names(cfg$partnership_labels),
    legal_status = names(cfg$legal_statuses),
    legal_status_basis = names(cfg$legal_status_bases),
    implementation_status = names(cfg$implementation_statuses),
    coding_basis = names(cfg$coding_bases),
    confidence = names(cfg$confidence_levels),
    relationship_category = names(AGR_RELATIONSHIP_CATEGORIES),
    sector = names(cfg$sectors),
    instrument_types = names(cfg$instruments),
    supply_chain_segment = c("Extraction", "Refining", "Components", "Assembly", "Deployment",
                             "Recovery", AGR_SEGMENT_CROSS),
    technology = unique(c("Batteries", "Coal", "Electric Grid", "Electric Vehicles", "Gas",
                          "Geothermal", "Green Hydrogen", "Nuclear", "Oil", "Solar", "Wind",
                          "Semiconductors",
                          unlist(lapply(cfg$sectors, function(s) unlist(s$techs)), use.names = FALSE)))
  )
}

# Sector -> technologies / stages / electro-industrial flag, from the config.
agr_sector_table <- function(cfg) {
  ids <- names(cfg$sectors)
  data.frame(
    sector = ids,
    label = vapply(cfg$sectors, function(s) as.character(s$label), character(1)),
    electro_industrial = vapply(cfg$sectors, function(s) isTRUE(s$electro_industrial), logical(1)),
    techs = vapply(cfg$sectors, function(s) agr_join(s$techs), character(1)),
    stages = vapply(cfg$sectors, function(s) agr_join(s$stages), character(1)),
    compacts = vapply(cfg$sectors, function(s) agr_join(s$compacts), character(1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

agr_instrument_table <- function(cfg) {
  ids <- names(cfg$instruments)
  data.frame(
    instrument = ids,
    label = vapply(cfg$instruments, function(s) as.character(s$label), character(1)),
    group = vapply(cfg$instruments, function(s) as.character(s$group), character(1)),
    operational = vapply(cfg$instruments, function(s) isTRUE(s$operational), logical(1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

# --- empty frames and coercion ------------------------------------------------------------

agr_empty_records <- function(n = 0L) {
  df <- as.data.frame(setNames(replicate(length(AGR_RECORD_COLUMNS), character(n), simplify = FALSE),
                               AGR_RECORD_COLUMNS), stringsAsFactors = FALSE)
  df$n_parties <- integer(n)
  df$primary_source_available <- logical(n)
  df
}

agr_empty_codes <- function() {
  data.frame(source_record_id = character(), dimension = character(), code = character(),
             basis = character(), n_hits = integer(), snippet = character(),
             stringsAsFactors = FALSE)
}

# Fill missing schema columns with "" (or 0 / FALSE), coerce types, order columns, and
# normalise dates and multi-valued fields. Adapters call this last.
agr_as_records <- function(df) {
  if (nrow(df) == 0) return(agr_empty_records())
  for (col in setdiff(AGR_RECORD_COLUMNS, names(df))) df[[col]] <- ""
  for (col in setdiff(AGR_RECORD_COLUMNS, c("n_parties", "primary_source_available"))) df[[col]] <- agr_chr(df[[col]])
  for (col in setdiff(AGR_DATE_FIELDS, "primary_date")) df[[col]] <- agr_norm_date(df[[col]])
  df$primary_date <- agr_primary_date(df$signature_date, df$announcement_date, df$effective_date)
  df$date_precision <- agr_date_precision(df$primary_date)
  for (col in setdiff(AGR_MULTI_FIELDS, "parties")) {
    df[[col]] <- vapply(agr_split(df[[col]]), agr_join, character(1))
  }
  df$n_parties <- as.integer(df$n_parties)
  df$primary_source_available <- as.logical(df$primary_source_available)
  df$primary_source_available[is.na(df$primary_source_available)] <- FALSE
  df <- df[, AGR_RECORD_COLUMNS, drop = FALSE]
  rownames(df) <- NULL
  df
}

agr_as_codes <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(agr_empty_codes())
  for (col in setdiff(AGR_CODE_COLUMNS, names(df))) df[[col]] <- if (col == "n_hits") 1L else ""
  out <- data.frame(
    source_record_id = agr_chr(df$source_record_id),
    dimension = agr_chr(df$dimension),
    code = agr_chr(df$code),
    basis = agr_chr(df$basis),
    n_hits = as.integer(df$n_hits),
    snippet = agr_chr(df$snippet),
    stringsAsFactors = FALSE
  )
  out$n_hits[is.na(out$n_hits)] <- 1L
  # One row per (record, dimension, code, basis): hits summed, first snippet kept.
  key <- paste(out$source_record_id, out$dimension, out$code, out$basis, sep = "\r")
  if (anyDuplicated(key)) {
    hits <- tapply(out$n_hits, key, sum)
    out <- out[!duplicated(key), , drop = FALSE]
    out$n_hits <- as.integer(hits[paste(out$source_record_id, out$dimension, out$code, out$basis, sep = "\r")])
  }
  out <- out[order(out$source_record_id, out$dimension, out$code, out$basis), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Derive technology and segment codes from sector codes (sector -> techs and stages in the
# config), with the basis of the sector code. A sector without stages gives no segment code.
agr_derive_tech_codes <- function(codes, cfg) {
  codes <- agr_as_codes(codes)
  sec <- codes[codes$dimension == "sector", , drop = FALSE]
  if (nrow(sec) == 0) return(codes)
  st <- agr_sector_table(cfg)
  derived <- list()
  for (i in seq_len(nrow(sec))) {
    row <- st[st$sector == sec$code[i], , drop = FALSE]
    if (nrow(row) == 0) next
    for (t in agr_split(row$techs)[[1]]) {
      derived[[length(derived) + 1]] <- data.frame(source_record_id = sec$source_record_id[i], dimension = "technology",
                                                   code = t, basis = sec$basis[i], n_hits = sec$n_hits[i],
                                                   snippet = "", stringsAsFactors = FALSE)
    }
    for (s in agr_split(row$stages)[[1]]) {
      derived[[length(derived) + 1]] <- data.frame(source_record_id = sec$source_record_id[i], dimension = "supply_chain_segment",
                                                   code = s, basis = sec$basis[i], n_hits = sec$n_hits[i],
                                                   snippet = "", stringsAsFactors = FALSE)
    }
  }
  if (length(derived) == 0) return(codes)
  agr_as_codes(rbind(codes, do.call(rbind, derived)))
}

# Set the multi-valued fields of `records` from `codes` (every basis counts; the basis stays in
# the codes table). Records without codes get "".
agr_apply_codes <- function(records, codes) {
  codes <- agr_as_codes(codes)
  for (dim in AGR_CODE_DIMENSIONS) {
    sub <- codes[codes$dimension == dim, , drop = FALSE]
    joined <- if (nrow(sub) > 0) tapply(sub$code, sub$source_record_id, agr_join) else character()
    v <- unname(joined[records$source_record_id])
    v[is.na(v)] <- ""
    records[[dim]] <- v
  }
  records
}

# --- validation ---------------------------------------------------------------------------

# Problems with an adapter result, as a character vector (empty when valid). The scripts stop on
# any; tests call it on fixtures.
agr_validate <- function(result, cfg) {
  problems <- character()
  rec <- result$records
  codes <- result$codes
  vocab <- agr_vocab(cfg)
  missing <- setdiff(AGR_RECORD_COLUMNS, names(rec))
  if (length(missing)) return(paste("records lack columns:", paste(missing, collapse = ", ")))
  if (anyDuplicated(rec$source_record_id)) {
    problems <- c(problems, paste("duplicate source_record_id:",
                                  paste(head(unique(rec$source_record_id[duplicated(rec$source_record_id)]), 5), collapse = ", ")))
  }
  if (any(!nzchar(rec$source_record_id))) problems <- c(problems, "empty source_record_id")
  bad_src <- setdiff(unique(rec$source_dataset), vocab$sources)
  if (length(bad_src)) problems <- c(problems, paste("unknown source_dataset:", paste(bad_src, collapse = ", ")))
  check_single <- function(field, allowed, allow_empty = FALSE) {
    v <- rec[[field]]
    bad <- unique(v[!(v %in% allowed) & !(allow_empty & !nzchar(v))])
    if (length(bad)) problems <<- c(problems, paste0(field, " not in vocabulary: ", paste(head(bad, 8), collapse = ", ")))
  }
  check_single("agreement_type", vocab$agreement_type)
  check_single("partnership_label", vocab$partnership_label, allow_empty = TRUE)
  check_single("legal_status", vocab$legal_status)
  check_single("legal_status_basis", vocab$legal_status_basis)
  check_single("implementation_status", vocab$implementation_status)
  check_single("relationship_category", vocab$relationship_category)
  check_multi <- function(field, allowed) {
    v <- unique(unlist(agr_split(rec[[field]])))
    bad <- setdiff(v, allowed)
    if (length(bad)) problems <<- c(problems, paste0(field, " not in vocabulary: ", paste(head(bad, 8), collapse = ", ")))
  }
  check_multi("sector", vocab$sector)
  check_multi("instrument_types", vocab$instrument_types)
  check_multi("supply_chain_segment", vocab$supply_chain_segment)
  check_multi("technology", vocab$technology)
  parties <- agr_split(rec$parties)
  bad_iso <- unique(unlist(lapply(parties, function(p) p[!grepl("^[A-Z]{3}$", p)])))
  if (length(bad_iso)) problems <- c(problems, paste("parties not ISO3:", paste(head(bad_iso, 8), collapse = ", ")))
  n <- vapply(parties, length, integer(1))
  if (any(n < 2)) problems <- c(problems, paste(sum(n < 2), "records with fewer than two parties"))
  if (any(n != rec$n_parties)) problems <- c(problems, "n_parties does not match parties")
  sorted_ok <- vapply(parties, function(p) identical(p, sort(unique(p))), logical(1))
  if (!all(sorted_ok)) problems <- c(problems, "parties not sorted and unique")
  for (f in AGR_DATE_FIELDS) {
    v <- rec[[f]]
    bad <- v[nzchar(v) & !grepl("^\\d{4}(-\\d{2}(-\\d{2})?)?$", v)]
    if (length(bad)) problems <- c(problems, paste0(f, " malformed: ", paste(head(unique(bad), 5), collapse = ", ")))
  }
  if (!is.null(codes) && nrow(codes) > 0) {
    miss <- setdiff(AGR_CODE_COLUMNS, names(codes))
    if (length(miss)) {
      problems <- c(problems, paste("codes lack columns:", paste(miss, collapse = ", ")))
    } else {
      orphan <- setdiff(unique(codes$source_record_id), rec$source_record_id)
      if (length(orphan)) problems <- c(problems, paste(length(orphan), "codes rows refer to unknown records"))
      bad_dim <- setdiff(unique(codes$dimension), AGR_CODE_DIMENSIONS)
      if (length(bad_dim)) problems <- c(problems, paste("unknown code dimension:", paste(bad_dim, collapse = ", ")))
      bad_basis <- setdiff(unique(codes$basis), vocab$coding_basis)
      if (length(bad_basis)) problems <- c(problems, paste("unknown coding basis:", paste(bad_basis, collapse = ", ")))
      for (dim in AGR_CODE_DIMENSIONS) {
        allowed <- vocab[[dim]]
        bad <- setdiff(unique(codes$code[codes$dimension == dim]), allowed)
        if (length(bad)) problems <- c(problems, paste0("codes ", dim, " not in vocabulary: ", paste(head(bad, 8), collapse = ", ")))
      }
      # The wide fields must equal the collapse of codes.
      chk <- agr_apply_codes(rec, codes)
      for (dim in AGR_CODE_DIMENSIONS) {
        if (!identical(chk[[dim]], rec[[dim]])) problems <- c(problems, paste0(dim, " differs from the collapse of codes"))
      }
    }
  }
  problems
}
