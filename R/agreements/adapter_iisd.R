# IISD Trade & Climate Tracker adapter (docs/partnership_agreements.md, section 10 "Sources").
#
# IISD's tracker site sits behind a bot challenge and its content is all rights reserved, so it is
# never fetched: this adapter reads an export IISD has shared, staged by hand as
# data/raw/iisd_trade_climate_tracker.csv. The expected columns follow IISD's methodology (1 July
# 2026): one row per agreement signed by two or more governments (January 2025 - June 2026),
# with its type (FTA, ECA, MoU / MoC, ART, CRM framework / agreement / strategic partnership,
# other), parties, signature date, whether the text is available, and per subtopic a coded
# summary of the operative provisions (Energy: Renewables; Energy: Cooperation and Systems;
# Climate: Emissions / Deforestation / Others; CRMs: Value Chain Stages; CRMs: Supply Chain
# Diversification / Security; Heavy Industry: Decarbonization / Circularity; Tariffs and Market
# Access). Column names are matched case- and punctuation-insensitively through IISD_COLUMN_ALIASES
# (README in data/reference/partnership_agreements/ lists them). A topic may also come as one
# long-form `topics` column ("Energy: Renewables; CRMs: Supply Chain Security").
# Topics IISD coded become sector codes with basis source_coded (IISD_TOPIC_CODES); the coded
# summaries are keyword-coded (agr_code_documents, basis text_body) for the specific sectors and
# instruments. Pure: the caller reads the CSV.

# Canonical column -> accepted names (compared after lower-casing, dropping "and"/"&" and every
# character that is not a letter or digit).
IISD_COLUMN_ALIASES <- list(
  agreement_id = c("agreement_id", "id", "record_id", "tracker_id", "agreement_code", "agreement_no"),
  title = c("title", "agreement", "agreement_name", "agreement_title", "name", "instrument_name"),
  parties = c("parties", "signatories", "parties_signatories", "countries", "members", "participants"),
  signature_date = c("signature_date", "date_of_signature", "signing_date", "date_signed", "signed", "signed_on", "date"),
  announcement_date = c("announcement_date", "date_announced", "announced"),
  effective_date = c("entry_into_force", "entry_into_force_date", "date_of_entry_into_force", "effective_date", "in_force_date"),
  agreement_type = c("agreement_type", "type", "instrument_type", "type_of_agreement", "type_of_instrument", "category"),
  legal_status = c("legal_status", "legally_binding", "binding", "legal_nature", "legal_form"),
  status = c("status", "agreement_status"),
  text_available = c("text_available", "full_text_available", "text_availability", "agreement_text_available", "text_public"),
  source_url = c("source_url", "tracker_url", "iisd_url", "url", "link", "source"),
  document_url = c("document_url", "text_url", "agreement_text_url", "full_text_url", "text_link", "agreement_text", "document", "legal_text"),
  topics = c("topics", "subtopics", "topic", "tags", "coded_topics"),
  summary = c("summary", "coded_summary", "provision_summary", "summaries"),
  last_updated = c("last_updated", "date_coded", "coded_on", "updated"),
  energy_renewables = c("energy_renewables", "renewables", "renewable_energy"),
  energy_cooperation_systems = c("energy_cooperation_and_systems", "energy_cooperation_systems", "energy_cooperation", "energy_systems"),
  climate_emissions = c("climate_emissions", "emissions"),
  climate_deforestation = c("climate_deforestation", "climate_deforestation_and_land_use", "deforestation", "deforestation_and_land_use"),
  climate_others = c("climate_others", "climate_other"),
  crm_value_chain = c("crms_value_chain_stages", "crm_value_chain_stages", "critical_minerals_value_chain_stages",
                      "crm_value_chain", "value_chain_stages", "critical_raw_minerals_value_chain_stages"),
  crm_supply_security = c("crms_supply_chain_diversification_security", "crms_supply_chain_diversification",
                          "crms_supply_chain_security", "crm_supply_chain_diversification_security", "crm_supply_security",
                          "crm_supply_chain_security", "critical_minerals_supply_chain_diversification",
                          "critical_minerals_supply_security", "crm_supply_security_diversification"),
  heavy_industry_decarbonization = c("heavy_industry_decarbonization", "heavy_industry_decarbonisation", "industrial_decarbonization"),
  heavy_industry_circularity = c("heavy_industry_circularity", "circularity"),
  tariffs_market_access = c("tariffs_market_access", "tariffs_and_market_access", "tariff_market_access", "market_access", "tariffs")
)

IISD_SUBTOPICS <- c("energy_renewables", "energy_cooperation_systems", "climate_emissions", "climate_deforestation",
                    "climate_others", "crm_value_chain", "crm_supply_security", "heavy_industry_decarbonization",
                    "heavy_industry_circularity", "tariffs_market_access")

IISD_SUBTOPIC_LABELS <- c(
  energy_renewables = "Energy: Renewables", energy_cooperation_systems = "Energy: Cooperation and Systems",
  climate_emissions = "Climate: Emissions", climate_deforestation = "Climate: Deforestation and Land Use",
  climate_others = "Climate: Others", crm_value_chain = "CRMs: Value Chain Stages",
  crm_supply_security = "CRMs: Supply Chain Diversification / Security",
  heavy_industry_decarbonization = "Heavy Industry: Decarbonization", heavy_industry_circularity = "Heavy Industry: Circularity",
  tariffs_market_access = "Tariffs and Market Access"
)

# Topic-level names in a long-form `topics` cell -> the subtopics they stand for.
IISD_TOPIC_LEVEL <- list(
  energy = c("energy_cooperation_systems"), climate = c("climate_others"), crms = c("crm_supply_security"),
  crm = c("crm_supply_security"), criticalminerals = c("crm_supply_security"),
  heavyindustry = c("heavy_industry_decarbonization")
)

# IISD subtopic -> codes (basis source_coded). Climate subtopics have no sector of their own.
IISD_TOPIC_CODES <- list(
  energy_renewables = list(sector = "general_clean_energy"),
  energy_cooperation_systems = list(sector = "general_clean_energy"),
  climate_emissions = list(), climate_deforestation = list(), climate_others = list(),
  crm_value_chain = list(sector = "critical_minerals"),
  crm_supply_security = list(sector = "critical_minerals"),
  heavy_industry_decarbonization = list(sector = "general_industrial"),
  heavy_industry_circularity = list(sector = "general_industrial"),
  tariffs_market_access = list(sector = "trade", instrument_types = c("tariff_reduction", "market_access"))
)

# Group names IISD may list as a party -> members (EU stays EUU). Membership as of 2026.
IISD_PARTY_GROUPS <- list(
  "european union" = "EUU", "eu" = "EUU", "euu" = "EUU",
  "efta" = c("CHE", "ISL", "LIE", "NOR"), "european free trade association" = c("CHE", "ISL", "LIE", "NOR"),
  "mercosur" = c("ARG", "BRA", "PRY", "URY"),
  "gcc" = c("ARE", "BHR", "KWT", "OMN", "QAT", "SAU"), "gulf cooperation council" = c("ARE", "BHR", "KWT", "OMN", "QAT", "SAU")
)

IISD_NO_VALUES <- c("", "no", "n", "0", "false", "none", "-", "n/a", "na", "not coded", "not applicable", "nil")
IISD_FLAG_VALUES <- c("yes", "y", "1", "true", "x", "coded", "\u2713", "\u2714")

iisd_key <- function(x) {
  x <- tolower(agr_chr(x))
  x <- gsub("&|\\band\\b", " ", x)
  gsub("[^a-z0-9]", "", x)
}

# Map the export's column names to canonical names. Returns a named character vector
# canonical -> original column name (first match wins).
iisd_match_columns <- function(cols) {
  keys <- iisd_key(cols)
  out <- character()
  for (canon in names(IISD_COLUMN_ALIASES)) {
    hit <- which(keys %in% iisd_key(IISD_COLUMN_ALIASES[[canon]]))
    hit <- setdiff(hit, match(out, cols))
    if (length(hit)) out[[canon]] <- cols[hit[1]]
  }
  out
}

# Text month dates ("15 March 2025", "March 15, 2025", "March 2025") -> ISO; others via agr_norm_date.
iisd_date <- function(x) {
  x <- agr_chr(x)
  months <- c(january = 1, february = 2, march = 3, april = 4, may = 5, june = 6, july = 7, august = 8,
              september = 9, october = 10, november = 11, december = 12)
  mon_of <- function(s) {
    k <- which(startsWith(names(months), tolower(substr(s, 1, 3))))
    if (length(k)) months[[k[1]]] else NA_integer_
  }
  out <- x
  for (i in seq_along(x)) {
    v <- x[i]
    if (grepl("^\\d{1,2} [A-Za-z]+\\.? \\d{4}$", v)) {
      p <- strsplit(v, " ")[[1]]
      mo <- mon_of(p[2])
      out[i] <- if (is.na(mo)) "" else sprintf("%s-%02d-%02d", p[3], mo, as.integer(p[1]))
    } else if (grepl("^[A-Za-z]+\\.? \\d{1,2}, \\d{4}$", v)) {
      p <- strsplit(sub(",", "", v), " ")[[1]]
      mo <- mon_of(p[1])
      out[i] <- if (is.na(mo)) "" else sprintf("%s-%02d-%02d", p[3], mo, as.integer(p[2]))
    } else if (grepl("^[A-Za-z]+\\.? \\d{4}$", v)) {
      p <- strsplit(v, " ")[[1]]
      mo <- mon_of(p[1])
      out[i] <- if (is.na(mo)) "" else sprintf("%s-%02d", p[2], mo)
    }
  }
  agr_norm_date(out)
}

# Parties cell -> ISO3 vector. Accepts ISO3 codes or names separated by ";", "|" or new lines (a
# single cell that does not resolve is retried split on "," and " and "). Returns
# list(iso3, unresolved).
iisd_parties <- function(cell, groups = IISD_PARTY_GROUPS) {
  resolve <- function(tok) {
    tok <- trimws(gsub("^the\\s+", "", trimws(tok), ignore.case = TRUE))
    if (!nzchar(tok)) return(character())
    low <- tolower(tok)
    if (low %in% names(groups)) return(groups[[low]])
    if (grepl("^[A-Z]{3}$", tok)) {
      ok <- tok %in% c("EUU", "XKX") || !is.na(suppressWarnings(countrycode::countrycode(tok, "iso3c", "iso3c", warn = FALSE)))
      return(if (ok) tok else NA_character_)
    }
    iso <- suppressWarnings(countrycode::countrycode(tok, "country.name", "iso3c", warn = FALSE,
                                                     custom_match = c("Kosovo" = "XKX", "European Union" = "EUU")))
    if (is.na(iso)) NA_character_ else iso
  }
  toks <- unlist(strsplit(agr_chr(cell), "\\s*[;|\n]\\s*"))
  toks <- toks[nzchar(toks)]
  # One cell of names separated by "," / " and " / "/": split when every piece resolves and the
  # pieces name two or more parties ("Japan, Republic of Korea"); otherwise the cell is one name
  # ("Korea, Republic of").
  if (length(toks) == 1 && grepl(",|\\s+and\\s+|/", toks)) {
    parts <- unlist(strsplit(toks, "\\s*,\\s*|\\s+and\\s+|\\s*/\\s*"))
    res <- lapply(parts[nzchar(parts)], resolve)
    ok <- !any(vapply(res, function(r) length(r) == 1 && is.na(r), logical(1)))
    if (ok && length(unique(unlist(res))) >= 2) {
      return(list(iso3 = sort(unique(unlist(res))), unresolved = character()))
    }
  }
  res <- lapply(toks, resolve)
  bad <- vapply(res, function(r) length(r) == 1 && is.na(r), logical(1))
  list(iso3 = sort(unique(unlist(res[!bad]))), unresolved = toks[bad])
}

# Raw agreement type -> agreement_type (IISD's categories, then common variants).
iisd_type <- function(raw) {
  r <- tolower(agr_chr(raw))
  out <- rep("other", length(r))
  set <- function(cond, val) { idx <- which(cond & out == "other"); out[idx] <<- val }
  set(grepl("strategic partnership", r), "strategic_partnership")
  set(grepl("\\bmou\\b|\\bmoc\\b|memorand", r), "mou")
  set(grepl("\\bepa\\b|economic partnership agreement", r), "epa")
  set(grepl("\\bfta\\b|free trade", r), "fta")
  set(grepl("\\bart\\b|reciprocal trade", r), "fta")
  set(grepl("\\beca\\b|economic cooperation agreement", r), "framework")
  set(grepl("\\bcrms?\\b|critical (raw )?minerals?", r) & grepl("framework|agreement", r), "framework")
  set(grepl("framework", r), "framework")
  out
}

iisd_label <- function(type, raw, title) {
  txt <- tolower(paste(raw, title))
  ifelse(type != "strategic_partnership", "",
         ifelse(grepl("comprehensive strategic", txt), "comprehensive_strategic",
                ifelse(grepl("global strategic", txt), "global_strategic",
                       ifelse(grepl("special strategic", txt), "special_strategic", "strategic"))))
}

iisd_legal <- function(x) {
  k <- tolower(agr_chr(x))
  ifelse(grepl("non[- ]?binding|^no$|political", k), "non_binding",
         ifelse(grepl("mixed|partly|partially", k), "mixed",
                ifelse(grepl("binding|^yes$|treaty", k), "binding", "unknown")))
}

# tbl: the staged IISD export (read all columns as character). cfg: parsed config.
# party_groups: group names -> members (extend for groups the export lists). strict: stop when a
# party name does not resolve (FALSE drops the name and reports it in extras$unresolved_parties;
# a record left with fewer than two parties is dropped and reported).
# Returns list(records, codes, extras) with extras$topics (source_record_id, subtopic, label,
# summary), extras$column_map and extras$unresolved_parties.
agr_adapt_iisd <- function(tbl, cfg, party_groups = IISD_PARTY_GROUPS, strict = TRUE, export_date = "") {
  cm <- iisd_match_columns(names(tbl))
  need <- c("title", "parties")
  if (!all(need %in% names(cm))) {
    stop("iisd: the export lacks required columns (", paste(setdiff(need, names(cm)), collapse = ", "),
         "); columns seen: ", paste(names(tbl), collapse = ", "), ". See IISD_COLUMN_ALIASES.")
  }
  sub_cols <- intersect(IISD_SUBTOPICS, names(cm))
  if (!length(sub_cols) && !("topics" %in% names(cm))) {
    stop("iisd: the export has neither subtopic columns nor a topics column; columns seen: ", paste(names(tbl), collapse = ", "))
  }
  col <- function(canon) if (canon %in% names(cm)) agr_chr(tbl[[cm[[canon]]]]) else rep("", nrow(tbl))
  n <- nrow(tbl)
  empty <- list(records = agr_empty_records(), codes = agr_empty_codes(),
                extras = list(topics = data.frame(source_record_id = character(), subtopic = character(), label = character(),
                                                  summary = character(), stringsAsFactors = FALSE),
                              column_map = data.frame(canonical = names(cm), column = unname(cm), stringsAsFactors = FALSE),
                              unresolved_parties = data.frame(source_record_id = character(), name = character(), stringsAsFactors = FALSE)))
  if (n == 0) return(empty)

  title <- col("title")
  sig <- iisd_date(col("signature_date"))
  ann <- iisd_date(col("announcement_date"))
  eff <- iisd_date(col("effective_date"))
  aid <- col("agreement_id")
  slug <- gsub("^-|-$", "", gsub("[^a-z0-9]+", "-", tolower(title)))
  aid <- ifelse(nzchar(aid), aid, paste0(substr(slug, 1, 60), ifelse(nzchar(sig), paste0("-", sig), "")))
  ids <- paste0("iisd:", aid)
  if (anyDuplicated(ids)) stop("iisd: duplicate record ids (agreement_id, or title + signature date): ",
                               paste(unique(ids[duplicated(ids)]), collapse = ", "))

  # Parties.
  pr <- lapply(col("parties"), iisd_parties, groups = party_groups)
  unresolved <- do.call(rbind, lapply(seq_len(n), function(i) {
    u <- pr[[i]]$unresolved
    if (length(u)) data.frame(source_record_id = ids[i], name = u, stringsAsFactors = FALSE) else NULL
  }))
  if (!is.null(unresolved) && strict) {
    stop("iisd: party names that do not resolve to ISO3 (add them to party_groups or use ISO3 codes): ",
         paste(unique(paste0(unresolved$name, " [", unresolved$source_record_id, "]")), collapse = "; "))
  }
  party_list <- lapply(pr, `[[`, "iso3")
  keep <- lengths(party_list) >= 2
  if (!all(keep)) {
    unresolved <- rbind(unresolved, data.frame(source_record_id = ids[!keep], name = "<fewer than two parties: record dropped>",
                                               stringsAsFactors = FALSE))
    if (strict) stop("iisd: records with fewer than two parties: ", paste(ids[!keep], collapse = ", "))
  }

  # Subtopics coded per record: wide columns, then a long-form topics column.
  topic_rows <- list()
  for (s in sub_cols) {
    v <- col(s)
    coded <- !(tolower(v) %in% IISD_NO_VALUES)
    if (!any(coded)) next
    summ <- ifelse(tolower(v) %in% IISD_FLAG_VALUES, "", v)
    topic_rows[[length(topic_rows) + 1]] <- data.frame(source_record_id = ids[coded], subtopic = s,
                                                       label = IISD_SUBTOPIC_LABELS[[s]], summary = summ[coded],
                                                       stringsAsFactors = FALSE)
  }
  if ("topics" %in% names(cm)) {
    alias_keys <- lapply(IISD_COLUMN_ALIASES[IISD_SUBTOPICS], iisd_key)
    tv <- col("topics")
    for (i in seq_len(n)) {
      for (tk in unlist(strsplit(tv[i], "\\s*[;|\n]\\s*"))) {
        k <- iisd_key(tk)
        if (!nzchar(k)) next
        hit <- names(alias_keys)[vapply(alias_keys, function(a) k %in% a, logical(1))]
        if (!length(hit) && k %in% names(IISD_TOPIC_LEVEL)) hit <- IISD_TOPIC_LEVEL[[k]]
        if (!length(hit)) next
        topic_rows[[length(topic_rows) + 1]] <- data.frame(source_record_id = ids[i], subtopic = hit,
                                                           label = unname(IISD_SUBTOPIC_LABELS[hit]), summary = "",
                                                           stringsAsFactors = FALSE)
      }
    }
  }
  topics <- if (length(topic_rows)) do.call(rbind, topic_rows) else empty$extras$topics
  topics <- topics[!duplicated(paste(topics$source_record_id, topics$subtopic)), , drop = FALSE]
  rownames(topics) <- NULL

  # Codes: IISD topics (source_coded), then the coded summaries (text_body).
  tc <- list()
  for (j in seq_len(nrow(topics))) {
    spec <- IISD_TOPIC_CODES[[topics$subtopic[j]]]
    snip <- paste0("IISD topic ", topics$label[j], if (nzchar(topics$summary[j])) paste0(": ", substr(gsub("\\s+", " ", topics$summary[j]), 1, 140)))
    for (dim in names(spec)) {
      tc[[length(tc) + 1]] <- data.frame(source_record_id = topics$source_record_id[j], dimension = dim,
                                         code = spec[[dim]], basis = "source_coded", n_hits = 1L, snippet = snip,
                                         stringsAsFactors = FALSE)
    }
  }
  src_codes <- agr_derive_tech_codes(if (length(tc)) do.call(rbind, tc) else agr_empty_codes(), cfg)
  bodies <- vapply(ids, function(id) {
    s <- topics$summary[topics$source_record_id == id & nzchar(topics$summary)]
    s <- c(s, col("summary")[ids == id])
    s <- s[nzchar(s)]
    paste(ifelse(grepl("[.!?]$", s), s, paste0(s, ".")), collapse = " ")
  }, character(1))
  txt_codes <- if (any(nzchar(bodies))) agr_code_documents(ids, rep("", n), bodies, cfg) else agr_empty_codes()

  type_raw <- col("agreement_type")
  type <- iisd_type(type_raw)
  legal <- if ("legal_status" %in% names(cm)) iisd_legal(col("legal_status")) else rep("unknown", n)
  st_raw <- col("status")
  status <- ifelse(grepl("in force", tolower(st_raw)), "in_force",
                   ifelse(nzchar(sig) | !nzchar(ann), "signed", "announced"))
  status_raw <- ifelse(nzchar(st_raw), st_raw,
                       ifelse(status == "signed", "signed (IISD includes agreements signed by two or more governments)", "announced"))
  src_url <- col("source_url")
  src_url[!grepl("^https?://", src_url)] <- ""
  src_url[!nzchar(src_url)] <- agr_chr(cfg$sources$iisd$url)
  doc_url <- col("document_url")
  doc_url[!grepl("^https?://", doc_url)] <- ""
  text_av <- col("text_available")
  all_codes <- agr_as_codes(rbind(src_codes, txt_codes))
  sec_by <- split(all_codes$code[all_codes$dimension == "sector"], all_codes$source_record_id[all_codes$dimension == "sector"])
  rel <- vapply(seq_len(n), function(i) {
    s <- setdiff(sec_by[[ids[i]]], "trade")
    if (type[i] %in% c("fta", "epa")) return("trade_economic")
    if (type[i] == "strategic_partnership") return("overall_relationship")
    if (type[i] == "framework" && grepl("\\beca\\b|economic cooperation", tolower(type_raw[i]))) return("trade_economic")
    if (length(s)) "sectoral" else "other"
  }, character(1))
  notes <- vapply(seq_len(n), function(i) {
    tp <- topics$label[topics$source_record_id == ids[i]]
    paste0("IISD type: ", if (nzchar(type_raw[i])) type_raw[i] else "not given", ". ",
           "IISD topics: ", if (length(tp)) paste(tp, collapse = "; ") else "none", ".",
           if (nzchar(text_av[i])) paste0(" Text available: ", text_av[i], ".") else "")
  }, character(1))

  rec <- data.frame(
    source_dataset = "iisd", source_record_id = ids, title = title,
    agr_party_fields(party_list),
    agreement_type = type, agreement_type_raw = type_raw, partnership_label = iisd_label(type, type_raw, title),
    legal_status = legal, legal_status_basis = ifelse(legal == "unknown", "none", "source_field"),
    announcement_date = ann, signature_date = sig, effective_date = eff, expiry_date = "",
    relationship_category = rel, implementation_status = status, implementation_status_raw = status_raw,
    source_url = src_url, primary_document_url = doc_url,
    last_verified = "", source_last_updated = if (nzchar(export_date)) export_date else iisd_date(col("last_updated")),
    description = paste0(ifelse(nzchar(type_raw), type_raw, "Agreement"), " signed by ",
                         vapply(party_list, paste, character(1), collapse = ", "), ", coded by the IISD Trade & Climate Tracker."),
    notes = notes, stringsAsFactors = FALSE)
  rec <- rec[keep, , drop = FALSE]
  records <- agr_as_records(rec)
  records$primary_source_available <- agr_is_official_url(records$primary_document_url, cfg$official_hosts) |
    agr_is_official_url(records$source_url, cfg$official_hosts)
  codes <- all_codes[all_codes$source_record_id %in% records$source_record_id, , drop = FALSE]
  records <- agr_apply_codes(records, codes)
  list(records = records, codes = agr_as_codes(codes), extras = list(
    topics = topics[topics$source_record_id %in% records$source_record_id, , drop = FALSE],
    column_map = empty$extras$column_map,
    unresolved_parties = if (is.null(unresolved)) empty$extras$unresolved_parties else unresolved))
}
