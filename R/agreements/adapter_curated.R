# Curated adapter (docs/partnership_agreements.md, section 10 "Sources").
#
# data/reference/partnership_agreements/curated_agreements.csv holds agreements recorded by hand
# from an official source, one row per agreement (README in the same folder: column definitions
# and the evidence rules). Rows map 1:1 to records (source_record_id "curated:<curated_id>");
# sectors, instruments and supply-chain segments become codes with basis `curated`, and the
# technologies follow from the sectors. A row that breaks the schema stops the adapter with a
# message naming its curated_id. Pure: the caller reads the CSV and the config.

CURATED_COLUMNS <- c(
  "curated_id", "title", "parties", "agreement_type", "agreement_type_raw", "partnership_label",
  "legal_status", "legal_status_note", "announcement_date", "signature_date", "effective_date",
  "expiry_date", "relationship_category", "sectors", "instruments", "supply_chain_segments",
  "implementation_status", "implementation_status_raw", "implementation_evidence", "source_url",
  "primary_document_url", "publisher", "last_verified", "verification", "description", "notes"
)

CURATED_VERIFICATION <- c("verified", "partial", "unverified")

# Party codes that are valid without being ISO 3166 countries.
CURATED_EXTRA_PARTY_CODES <- c("EUU", "XKX")

# TRUE where x is an ISO3 country code (or EUU / XKX).
curated_valid_iso3 <- function(x) {
  x <- agr_chr(x)
  ok <- grepl("^[A-Z]{3}$", x)
  known <- suppressWarnings(countrycode::countrycode(x, "iso3c", "iso3c", warn = FALSE))
  ok & (!is.na(known) | x %in% CURATED_EXTRA_PARTY_CODES)
}

# TRUE where a non-empty date is exactly YYYY-MM-DD, YYYY-MM or YYYY and a valid calendar date.
curated_date_ok <- function(x) {
  x <- agr_chr(x)
  !nzchar(x) | (grepl("^\\d{4}(-\\d{2}(-\\d{2})?)?$", x) & agr_norm_date(x) == x)
}

curated_first_url <- function(x) {
  m <- regmatches(x, regexpr("https?://[^[:space:],;)]+", x))
  out <- character(length(x))
  out[grepl("https?://", x)] <- m
  sub("[.]$", "", out)
}

# Problems of a curated table, as "curated_id: problem" strings (empty when valid).
agr_curated_problems <- function(tbl, cfg) {
  vocab <- agr_vocab(cfg)
  ids <- tbl$curated_id
  label <- ifelse(nzchar(ids), ids, paste0("<row ", seq_along(ids), ">"))
  out <- character()
  add <- function(i, msg) out <<- c(out, paste0(label[i], ": ", msg))
  dup <- unique(ids[duplicated(ids) & nzchar(ids)])
  for (d in dup) out <- c(out, paste0(d, ": curated_id is not unique"))
  for (i in seq_len(nrow(tbl))) {
    r <- tbl[i, ]
    if (!nzchar(r$curated_id)) add(i, "curated_id is empty")
    else if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", r$curated_id)) add(i, paste0("curated_id '", r$curated_id, "' has characters other than letters, digits, '.', '_' and '-'"))
    if (!nzchar(r$title)) add(i, "title is empty")
    p <- agr_split(r$parties)[[1]]
    bad <- p[!curated_valid_iso3(p)]
    if (length(bad)) add(i, paste0("parties not ISO3 (use EUU for the EU): ", paste(bad, collapse = ", ")))
    if (length(unique(p)) < 2) add(i, "fewer than two parties")
    single <- list(agreement_type = vocab$agreement_type, legal_status = vocab$legal_status,
                   relationship_category = vocab$relationship_category,
                   implementation_status = vocab$implementation_status, verification = CURATED_VERIFICATION)
    for (f in names(single)) {
      if (!(r[[f]] %in% single[[f]])) add(i, paste0(f, " '", r[[f]], "' not in vocabulary (", paste(single[[f]], collapse = ", "), ")"))
    }
    if (nzchar(r$partnership_label) && !(r$partnership_label %in% vocab$partnership_label)) {
      add(i, paste0("partnership_label '", r$partnership_label, "' not in vocabulary"))
    }
    multi <- list(sectors = vocab$sector, instruments = vocab$instrument_types,
                  supply_chain_segments = vocab$supply_chain_segment)
    for (f in names(multi)) {
      bad <- setdiff(agr_split(r[[f]])[[1]], multi[[f]])
      if (length(bad)) add(i, paste0(f, " not in vocabulary: ", paste(bad, collapse = ", ")))
    }
    for (f in c("announcement_date", "signature_date", "effective_date", "expiry_date", "last_verified")) {
      if (!curated_date_ok(r[[f]])) add(i, paste0(f, " '", r[[f]], "' is not a valid YYYY-MM-DD, YYYY-MM or YYYY date"))
    }
    if (!nzchar(r$last_verified)) add(i, "last_verified is empty")
    if (!grepl("^https?://", r$source_url)) add(i, "source_url is missing or not an http(s) URL (an official source is required)")
    if (nzchar(r$primary_document_url) && !grepl("^https?://", r$primary_document_url)) add(i, "primary_document_url is not an http(s) URL")
    if (!nzchar(r$publisher)) add(i, "publisher is empty")
    if (r$legal_status %in% setdiff(vocab$legal_status, "unknown") && !nzchar(r$legal_status_note)) {
      add(i, paste0("legal_status '", r$legal_status, "' needs legal_status_note (the official wording behind it)"))
    }
    if (r$implementation_status %in% unlist(cfg$delivery_statuses) && !nzchar(r$implementation_evidence)) {
      add(i, paste0("implementation_status '", r$implementation_status, "' needs implementation_evidence"))
    }
  }
  out
}

# Derive technology codes from sectors; derive segment codes from sectors only for records
# without an explicit stage (cross_stage alone does not count as explicit).
curated_derive <- function(codes, cfg) {
  codes <- agr_as_codes(codes)
  derived <- agr_derive_tech_codes(codes, cfg)
  seg <- codes[codes$dimension == "supply_chain_segment" & codes$code != AGR_SEGMENT_CROSS, , drop = FALSE]
  explicit <- unique(seg$source_record_id)
  key_d <- paste(derived$source_record_id, derived$dimension, derived$code, derived$basis, sep = "\r")
  key_c <- paste(codes$source_record_id, codes$dimension, codes$code, codes$basis, sep = "\r")
  drop <- derived$dimension == "supply_chain_segment" & derived$source_record_id %in% explicit & !(key_d %in% key_c)
  agr_as_codes(derived[!drop, , drop = FALSE])
}

# tbl: the curated CSV as read (columns CURATED_COLUMNS, in any order; read all as character).
# Returns list(records, codes, extras); extras$implementation_evidence (source_record_id,
# evidence_id, item_type, text, claim, status, amount_usd, url, verification) and
# extras$curated_meta (source_record_id, verification, publisher).
agr_adapt_curated <- function(tbl, cfg) {
  miss <- setdiff(CURATED_COLUMNS, names(tbl))
  if (length(miss)) stop("curated: the table lacks columns: ", paste(miss, collapse = ", "))
  tbl <- as.data.frame(lapply(tbl[CURATED_COLUMNS], agr_chr), stringsAsFactors = FALSE)
  empty_extras <- list(
    implementation_evidence = data.frame(source_record_id = character(), evidence_id = character(), item_type = character(),
                                         text = character(), claim = character(), status = character(),
                                         amount_usd = numeric(), url = character(), verification = character(),
                                         stringsAsFactors = FALSE),
    curated_meta = data.frame(source_record_id = character(), verification = character(), publisher = character(),
                              stringsAsFactors = FALSE))
  if (nrow(tbl) == 0) return(list(records = agr_empty_records(), codes = agr_empty_codes(), extras = empty_extras))
  problems <- agr_curated_problems(tbl, cfg)
  if (length(problems)) {
    stop("curated: ", length(problems), " problem(s) in curated_agreements.csv:\n  ", paste(problems, collapse = "\n  "),
         call. = FALSE)
  }
  ids <- paste0("curated:", tbl$curated_id)
  party_list <- agr_split(tbl$parties)
  notes <- vapply(seq_len(nrow(tbl)), function(i) {
    parts <- c(if (nzchar(tbl$legal_status_note[i])) paste0("Legal status (official wording): ", tbl$legal_status_note[i]),
               paste0("Publisher: ", tbl$publisher[i]),
               paste0("Verification: ", tbl$verification[i]),
               if (nzchar(tbl$implementation_evidence[i])) paste0("Implementation evidence: ", tbl$implementation_evidence[i]),
               tbl$notes[i])
    parts <- parts[nzchar(parts)]
    paste(ifelse(grepl("[.!?]$", parts), parts, paste0(parts, ".")), collapse = " ")
  }, character(1))
  rec <- data.frame(
    source_dataset = "curated", source_record_id = ids, title = tbl$title,
    agr_party_fields(party_list),
    agreement_type = tbl$agreement_type, agreement_type_raw = tbl$agreement_type_raw,
    partnership_label = tbl$partnership_label, legal_status = tbl$legal_status,
    legal_status_basis = ifelse(tbl$legal_status == "unknown", "none", "curated"),
    announcement_date = tbl$announcement_date, signature_date = tbl$signature_date,
    effective_date = tbl$effective_date, expiry_date = tbl$expiry_date,
    relationship_category = tbl$relationship_category,
    implementation_status = tbl$implementation_status, implementation_status_raw = tbl$implementation_status_raw,
    source_url = tbl$source_url, primary_document_url = tbl$primary_document_url,
    last_verified = tbl$last_verified, source_last_updated = "",
    description = tbl$description, notes = notes,
    stringsAsFactors = FALSE)
  records <- agr_as_records(rec)
  records$primary_source_available <- agr_is_official_url(records$source_url, cfg$official_hosts) |
    agr_is_official_url(records$primary_document_url, cfg$official_hosts)

  code_rows <- list()
  dims <- c(sectors = "sector", instruments = "instrument_types", supply_chain_segments = "supply_chain_segment")
  for (f in names(dims)) {
    vals <- agr_split(tbl[[f]])
    n <- lengths(vals)
    if (sum(n) == 0) next
    code_rows[[length(code_rows) + 1]] <- data.frame(
      source_record_id = rep(ids, n), dimension = dims[[f]], code = unlist(vals), basis = "curated", n_hits = 1L,
      snippet = rep(paste0("curated (", tbl$verification, ") from ", tbl$source_url), n), stringsAsFactors = FALSE)
  }
  codes <- if (length(code_rows)) curated_derive(do.call(rbind, code_rows), cfg) else agr_empty_codes()
  records <- agr_apply_codes(records, codes)

  has_ev <- nzchar(tbl$implementation_evidence)
  impl <- data.frame(source_record_id = ids[has_ev], evidence_id = "", item_type = "",
                     text = tbl$implementation_evidence[has_ev], claim = tbl$implementation_evidence[has_ev],
                     status = tbl$implementation_status[has_ev], amount_usd = NA_real_,
                     url = curated_first_url(tbl$implementation_evidence[has_ev]),
                     verification = tbl$verification[has_ev], stringsAsFactors = FALSE)
  if (!any(has_ev)) impl <- empty_extras$implementation_evidence
  meta <- data.frame(source_record_id = ids, verification = tbl$verification, publisher = tbl$publisher,
                     stringsAsFactors = FALSE)
  list(records = records, codes = codes, extras = list(implementation_evidence = impl, curated_meta = meta))
}
