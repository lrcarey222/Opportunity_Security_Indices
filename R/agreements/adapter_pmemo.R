# Partnership-memo evidence adapter (docs/partnership_agreements.md, section 10 "Sources").
#
# Input: the Partnership Memos evidence layer (data/reference/partnership_evidence/):
#   bilateral_mechanisms.csv  one row per bilateral or plurilateral mechanism of a memo pair, with
#                             the evidence_ids behind it;
#   policy_evidence.csv       one row per agreement, institution, project, financing, dispute or
#                             market fact, with a verification grade.
# Records:
#   - every mechanism ("pmemo:<mechanism_id>");
#   - every agreement-like evidence row that no mechanism references ("pmemo:<evidence_id>"):
#     item_type agreement, framework, plurilateral or dialogue, or a policy row naming two or more
#     countries (a government-to-government measure). Disputes, market facts, export controls and
#     single-country rows are never records. Known duplicates across the two research passes
#     (PMEMO_DUPLICATES) are resolved in favour of the mechanism-linked item and reported.
#   - never an item of PMEMO_EXCLUDE: plurilateral items whose evidence lists only some of their
#     parties (FORGE, E37), which would otherwise read as a bilateral (extras$excluded).
# Project, financing, corporate and procurement rows linked to a mechanism become
# extras$implementation_evidence; delivery among them (under construction, operating, or a
# verified signed financing) moves the mechanism to implementation_underway. Sectors,
# instruments and stages come from explicit mapping tables below (basis curated). Pure.

PMEMO_CORE_TYPES <- c("agreement", "framework", "plurilateral", "dialogue", "institution", "policy")
PMEMO_DELIVERY_TYPES <- c("project", "financing", "corporate", "procurement")
PMEMO_RECORD_TYPES <- c("agreement", "framework", "plurilateral", "dialogue")
PMEMO_NEVER_TYPES <- c("dispute", "market_fact")

# Evidence or mechanism status -> implementation_status (raw kept in implementation_status_raw).
PMEMO_STATUS_MAP <- c(
  announced = "announced", proposed = "announced", signed = "signed", in_force = "in_force",
  under_construction = "implementation_underway", operating = "operational",
  paused = "unknown", suspended = "unknown", unknown = "unknown"
)

# mechanism_type -> agreement_type. plurilateral is decided by the name (PMEMO_PLURILATERAL_TYPE).
PMEMO_MECHANISM_TYPES <- c(
  trade_agreement = "fta", framework = "framework", investment_vehicle = "financing_agreement",
  dialogue = "ministerial_initiative", finance_cooperation = "financing_agreement",
  trade_policy = "other"   # an action plan (USJ-CMAP): no closer type in the vocabulary
)

# AIB strategic compact -> sectors. SC-SEMIS on a critical-minerals mechanism (its name names
# critical minerals or rare earths) means the compact's mineral inputs (gallium, germanium), so it
# maps to critical_minerals there; SC-MAGNETS adds evs / wind only when the text names motors /
# turbines.
PMEMO_COMPACT_SECTORS <- list(
  "SC-BATTERIES" = c("batteries", "critical_minerals"),
  "SC-MAGNETS" = "critical_minerals",
  "SC-SEMIS" = "semiconductors",
  "SC-NUCLEAR" = "nuclear",
  "SC-GRID" = "grid_equipment",
  "SC-SOLAR" = "solar",
  "SC-WIND" = "wind",
  "SC-EV" = "evs",
  "cross_sector" = character()
)

# Evidence `technology` (lower case) -> sectors ("" = deliberately no sector). power_generation is
# decided from the row's name and claim (pmemo_power_sector).
PMEMO_TECH_SECTORS <- c(
  "2nm logic" = "semiconductors",
  "ap1000 / smr" = "nuclear",
  "batteries" = "batteries",
  "battery recycling" = "batteries",
  "bwrx-300 smr" = "nuclear",
  "cathode materials" = "batteries",
  "critical minerals / infrastructure" = "critical_minerals",
  "critical minerals / rare earths" = "critical_minerals",
  "critical_minerals" = "critical_minerals",
  "cross_sector" = "",
  "dual-use items" = "",
  "dual-use items incl. rare earths" = "critical_minerals",
  "dy/tb oxides" = "critical_minerals",
  "energy infrastructure" = "",
  "energy_lng" = "fossil_fuels",
  "gallium" = "critical_minerals",
  "grain-oriented electrical steel" = "steel",
  "grid equipment" = "grid_equipment",
  "heavy forgings" = "",
  "heavy rare earth separation" = "critical_minerals",
  "hvdc cable" = "transmission",
  "large power transformers" = "transformers",
  "li-ion cells" = "batteries",
  "li-ion hybrid/ev batteries" = "batteries",
  "lithium" = "critical_minerals",
  "medium and heavy rare earths" = "critical_minerals",
  "natural gas power" = "fossil_fuels",
  "ndfeb magnets" = "critical_minerals",
  "ndpr/dy oxides" = "critical_minerals",
  "nuclear" = "nuclear",
  "nuclear / power / critical minerals" = "nuclear|critical_minerals",
  "power transformers" = "transformers",
  "rare earth refining" = "critical_minerals",
  "rare earth separation / samarium" = "critical_minerals",
  "rare earths and related items" = "critical_minerals",
  "rare_earths_magnets" = "critical_minerals",
  "semiconductors" = "semiconductors",
  "steel" = "steel",
  "switchgear / circuit breakers" = "grid_equipment",
  "synthetic_diamonds" = "general_industrial"
)

# Evidence `instrument` -> instrument code. loan is export_credit when JBIC, NEXI or EXIM is
# named, else public_finance; dialogue on an institution row (a body the agreement creates) is a
# working_group. other and export_control give no code.
PMEMO_INSTRUMENTS <- c(
  equity = "joint_investment_fund", offtake = "offtake", price_floor = "price_support",
  stockpile = "critical_mineral_supply", standards = "standards_cooperation",
  procurement = "government_procurement", dialogue = "strategic_dialogue", jv = "joint_venture",
  fdi = "investment_facilitation", investment_vehicle = "joint_investment_fund",
  tariff = "trade_preference", trade_agreement = "trade_preference", grant = "public_finance",
  other = "", export_control = ""
)

PMEMO_ECA_PATTERN <- "\\bJBIC\\b|\\bNEXI\\b|\\bEXIM\\b|Export-Import Bank"
PMEMO_DFI_PATTERN <- "\\bDFC\\b|\\bJICA\\b"

# Items the two research passes recorded twice. In each group the mechanism-linked item is kept.
PMEMO_DUPLICATES <- list(
  c("E12", "UJ-003"),
  c("E35", "UJ-047"),
  c("E28", "E29", "UJ-005"),
  c("E15", "E16", "E17", "E18", "E19", "E20", "E21", "UJ-004")
)

PMEMO_VERIFICATION_RANK <- c(verified = 3L, partial = 2L, unverified = 1L)

# Evidence rows or mechanisms that are never records, with the reason (extras$excluded). Only
# plurilateral items whose party list in the evidence is incomplete belong here: recorded with
# the parties the evidence names, they would show as a bilateral agreement between those two.
PMEMO_EXCLUDE <- c(
  E37 = paste("FORGE (Forum on Resource Geostrategic Engagement) is a plurilateral club of about 50",
              "participants with no published roster; the evidence names only USA and JPN, so a record",
              "would read as a US-Japan bilateral")
)

# --- helpers ------------------------------------------------------------------------------

pmemo_shorten <- function(x, n = 200L) {
  x <- gsub("\\s+", " ", agr_chr(x))
  long <- nchar(x) > n
  x[long] <- paste0(sub("\\s+\\S*$", "", substr(x[long], 1, n - 3L)), "...")
  x
}

# The first date written in a text: "4 Sep 2025" -> "2025-09-04", "Oct 2025" -> "2025-10",
# "2019" -> "2019" ("" when none). The match that starts first wins.
pmemo_text_date <- function(x) {
  x <- agr_chr(x)[1]
  if (is.na(x) || !nzchar(x)) return("")
  mon <- "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[a-z]*\\.?"
  pats <- list(
    dmy = list(p = paste0("\\b(\\d{1,2}) ", mon, " ((?:19|20)\\d{2})\\b"), d = 1, m = 2, y = 3),
    mdy = list(p = paste0("\\b", mon, " (\\d{1,2}), ((?:19|20)\\d{2})\\b"), d = 2, m = 1, y = 3),
    my = list(p = paste0("\\b", mon, " ((?:19|20)\\d{2})\\b"), d = 0, m = 1, y = 2),
    y = list(p = "\\b((?:19|20)\\d{2})\\b", d = 0, m = 0, y = 1)
  )
  best <- NULL
  for (nm in names(pats)) {
    m <- regexpr(pats[[nm]]$p, x, perl = TRUE)
    if (m < 0) next
    len <- attr(m, "match.length")
    if (is.null(best) || m < best$pos || (m == best$pos && len > best$len)) {
      best <- list(pos = m, len = len, nm = nm, text = regmatches(x, m))
    }
  }
  if (is.null(best)) return("")
  spec <- pats[[best$nm]]
  grp <- function(k) sub(spec$p, paste0("\\", k), best$text, perl = TRUE)
  months <- c(Jan = 1, Feb = 2, Mar = 3, Apr = 4, May = 5, Jun = 6, Jul = 7, Aug = 8, Sep = 9, Oct = 10, Nov = 11, Dec = 12)
  y <- grp(spec$y)
  if (spec$m == 0) return(y)
  mo <- months[[substr(grp(spec$m), 1, 3)]]
  if (spec$d == 0) return(sprintf("%s-%02d", y, mo))
  agr_norm_date(sprintf("%s-%02d-%02d", y, mo, as.integer(grp(spec$d))))
}

pmemo_earliest <- function(x) {
  x <- agr_norm_date(x)
  x <- x[nzchar(x)]
  if (!length(x)) return("")
  x[order(agr_date_value(x), -nchar(x))][1]
}

# Sector of a power_generation row from its own words.
pmemo_power_sector <- function(text) {
  if (grepl("\\bgas\\b|\\bLNG\\b|\\boil\\b|\\bcoal\\b", text, ignore.case = TRUE)) return("fossil_fuels")
  if (grepl("nuclear|\\bSMRs?\\b|reactor", text, ignore.case = TRUE)) return("nuclear")
  if (grepl("solar|wind|renewable|hydro|geotherm", text, ignore.case = TRUE)) return("general_clean_energy")
  NA_character_
}

pmemo_code <- function(id, dimension, code, snippet) {
  if (!length(code)) return(NULL)
  data.frame(source_record_id = id, dimension = dimension, code = code, basis = "curated", n_hits = 1L,
             snippet = snippet, stringsAsFactors = FALSE)
}

# Codes from one evidence row. Returns list(codes, unmapped).
pmemo_row_codes <- function(id, row, use_compact, minerals_context) {
  out <- list()
  unmapped <- list()
  text <- paste(row$name, row$claim)
  tech <- tolower(row$technology)
  if (nzchar(tech)) {
    if (tech == "power_generation") {
      s <- pmemo_power_sector(text)
      if (is.na(s)) unmapped[[length(unmapped) + 1]] <- c("technology", row$technology) else
        out[[length(out) + 1]] <- pmemo_code(id, "sector", s, paste0(row$evidence_id, " technology ", row$technology, " (", s, " from the item's description)"))
    } else if (tech %in% names(PMEMO_TECH_SECTORS)) {
      s <- agr_split(PMEMO_TECH_SECTORS[[tech]])[[1]]
      out[[length(out) + 1]] <- pmemo_code(id, "sector", s, paste0(row$evidence_id, " technology ", row$technology))
    } else {
      unmapped[[length(unmapped) + 1]] <- c("technology", row$technology)
    }
  }
  if (use_compact && nzchar(row$compact)) {
    s <- pmemo_compact_sectors(row$compact, text, minerals_context)
    if (is.null(s)) unmapped[[length(unmapped) + 1]] <- c("compact", row$compact) else
      out[[length(out) + 1]] <- pmemo_code(id, "sector", s, paste0(row$evidence_id, " compact ", row$compact))
  }
  ins <- pmemo_instrument(row$instrument, row$institution, row$item_type)
  if (is.na(ins)) {
    unmapped[[length(unmapped) + 1]] <- c("instrument", row$instrument)
  } else if (nzchar(ins)) {
    out[[length(out) + 1]] <- pmemo_code(id, "instrument_types", ins,
                                         paste0(row$evidence_id, " instrument ", row$instrument, if (nzchar(row$institution)) paste0(" (", row$institution, ")")))
  }
  if (nzchar(row$stage)) {
    out[[length(out) + 1]] <- pmemo_code(id, "supply_chain_segment", row$stage, paste0(row$evidence_id, " stage ", row$stage))
  }
  list(codes = do.call(rbind, out), unmapped = unmapped)
}

# Sectors of one compact (NULL when the compact is not in the mapping).
pmemo_compact_sectors <- function(compact, text, minerals_context) {
  if (!(compact %in% names(PMEMO_COMPACT_SECTORS))) return(NULL)
  s <- PMEMO_COMPACT_SECTORS[[compact]]
  if (compact == "SC-SEMIS" && minerals_context) s <- "critical_minerals"
  if (compact == "SC-MAGNETS") {
    if (grepl("\\bmotors?\\b", text, ignore.case = TRUE)) s <- c(s, "evs")
    if (grepl("\\bturbines?\\b", text, ignore.case = TRUE)) s <- c(s, "wind")
  }
  s
}

# Instrument code of an evidence instrument ("" none, NA unmapped).
pmemo_instrument <- function(instrument, institution, item_type) {
  instrument <- agr_chr(instrument)
  if (!nzchar(instrument)) return("")
  if (instrument == "loan") {
    return(if (grepl(PMEMO_ECA_PATTERN, institution)) "export_credit" else "public_finance")
  }
  if (instrument == "dialogue" && identical(item_type, "institution")) return("working_group")
  if (instrument %in% names(PMEMO_INSTRUMENTS)) return(PMEMO_INSTRUMENTS[[instrument]])
  NA_character_
}

pmemo_relationship <- function(agreement_type, raw_type, name, sectors) {
  specific <- setdiff(sectors, "trade")
  if (agreement_type == "financing_agreement") return("investment_finance")
  if (agreement_type == "plurilateral_forum") return("plurilateral_club")
  if (agreement_type %in% c("fta", "epa")) return(if (length(specific)) "sectoral" else "trade_economic")
  if (raw_type == "dialogue" || agreement_type == "ministerial_initiative") {
    if (length(specific)) return("sectoral")
    return(if (grepl("econom|trade", name, ignore.case = TRUE)) "trade_economic" else "overall_relationship")
  }
  if (length(specific)) "sectoral" else "other"
}

# Derive technology codes; derive stage codes only for records with no specific stage of their
# own, and drop cross_stage from records that have a specific one.
pmemo_derive <- function(codes, cfg) {
  codes <- agr_as_codes(codes)
  seg <- codes[codes$dimension == "supply_chain_segment" & codes$code != AGR_SEGMENT_CROSS, , drop = FALSE]
  explicit <- unique(seg$source_record_id)
  codes <- codes[!(codes$dimension == "supply_chain_segment" & codes$code == AGR_SEGMENT_CROSS &
                     codes$source_record_id %in% explicit), , drop = FALSE]
  derived <- agr_derive_tech_codes(codes, cfg)
  key_d <- paste(derived$source_record_id, derived$dimension, derived$code, sep = "\r")
  key_c <- paste(codes$source_record_id, codes$dimension, codes$code, sep = "\r")
  drop <- derived$dimension == "supply_chain_segment" & derived$source_record_id %in% explicit & !(key_d %in% key_c)
  agr_as_codes(derived[!drop, , drop = FALSE])
}

# The row whose url stands for a record: best verification, then an official host, then order.
pmemo_pick_source <- function(rows, cfg) {
  rows <- rows[nzchar(rows$url), , drop = FALSE]
  if (!nrow(rows)) return(NULL)
  rank <- PMEMO_VERIFICATION_RANK[rows$verification]
  rank[is.na(rank)] <- 0L
  off <- agr_is_official_url(rows$url, cfg$official_hosts)
  rows[order(-rank, -off, seq_len(nrow(rows)))[1], , drop = FALSE]
}

pmemo_nonbinding <- function(text) any(grepl("\\bnon-?binding\\b", text, ignore.case = TRUE))

# --- adapter ------------------------------------------------------------------------------

# mechanisms: bilateral_mechanisms.csv; evidence: policy_evidence.csv (read as character).
# cfg: parsed config/partnership_agreements.yml. duplicates: groups of evidence ids that record
# the same item. financing_signed_counts: a verified financing row with status signed or in_force
# linked to a mechanism counts as delivery evidence (besides under_construction / operating).
# exclude: named character vector, evidence or mechanism id -> reason; those are never records.
# Returns list(records, codes, extras) with extras implementation_evidence, curated_meta,
# mechanism_assessment, duplicates, unmapped, unresolved_links and excluded.
agr_adapt_pmemo <- function(mechanisms, evidence, cfg, duplicates = PMEMO_DUPLICATES,
                            financing_signed_counts = TRUE, exclude = PMEMO_EXCLUDE) {
  need_m <- c("mechanism_id", "pair", "name", "mechanism_type", "status", "evidence_ids", "scope_compacts",
              "has_finance", "notes")
  need_e <- c("evidence_id", "item_type", "name", "institution", "countries", "status", "publication_date",
              "access_date", "technology", "compact", "stage", "instrument", "amount_usd", "source", "url",
              "claim", "verification")
  miss <- c(setdiff(need_m, names(mechanisms)), setdiff(need_e, names(evidence)))
  if (length(miss)) stop("pmemo: input tables lack columns: ", paste(miss, collapse = ", "))
  mechanisms <- as.data.frame(lapply(mechanisms, agr_chr), stringsAsFactors = FALSE)
  amount <- suppressWarnings(as.numeric(agr_chr(evidence$amount_usd)))
  evidence <- as.data.frame(lapply(evidence, agr_chr), stringsAsFactors = FALSE)
  evidence$amount_num <- amount
  ev_ctry <- agr_split(evidence$countries)
  evidence$n_countries <- lengths(lapply(ev_ctry, unique))
  links <- agr_split(mechanisms$evidence_ids)
  names(links) <- mechanisms$mechanism_id
  ref_ids <- unique(unlist(links))
  link_owner <- unlist(lapply(names(links), function(m) setNames(rep(m, length(links[[m]])), links[[m]])))
  type_labels <- unlist(cfg$agreement_types)

  recs <- list(); codes <- list(); impl <- list(); meta <- list(); assess <- list(); unmapped <- list()
  unresolved <- list()

  add_unmapped <- function(id, um) {
    for (u in um) unmapped[[length(unmapped) + 1]] <<- data.frame(source_record_id = id, field = u[1], value = u[2], stringsAsFactors = FALSE)
  }

  exclude <- if (length(exclude)) exclude else character()
  excluded <- data.frame(id = character(), kind = character(), reason = character(), stringsAsFactors = FALSE)
  if (length(exclude)) {
    hit_m <- intersect(names(exclude), mechanisms$mechanism_id)
    hit_e <- intersect(setdiff(names(exclude), hit_m), evidence$evidence_id)
    excluded <- data.frame(id = c(hit_m, hit_e), kind = c(rep("mechanism", length(hit_m)), rep("evidence", length(hit_e))),
                           reason = unname(exclude[c(hit_m, hit_e)]), stringsAsFactors = FALSE)
  }

  # Mechanisms.
  for (i in seq_len(nrow(mechanisms))) {
    m <- mechanisms[i, ]
    if (m$mechanism_id %in% names(exclude)) next
    id <- paste0("pmemo:", m$mechanism_id)
    ids <- links[[i]]
    missing <- setdiff(ids, evidence$evidence_id)
    if (length(missing)) unresolved[[length(unresolved) + 1]] <- data.frame(source_record_id = id, evidence_id = missing, stringsAsFactors = FALSE)
    rows <- evidence[match(intersect(ids, evidence$evidence_id), evidence$evidence_id), , drop = FALSE]
    usable <- rows[!(rows$item_type %in% PMEMO_NEVER_TYPES) & rows$instrument != "export_control", , drop = FALSE]
    core <- usable[usable$item_type %in% PMEMO_CORE_TYPES, , drop = FALSE]
    delivery <- usable[usable$item_type %in% PMEMO_DELIVERY_TYPES, , drop = FALSE]
    contributing <- usable[usable$verification != "unverified" &
                             usable$item_type %in% c(PMEMO_CORE_TYPES, PMEMO_DELIVERY_TYPES), , drop = FALSE]
    mtext <- paste(m$name, m$notes)
    minerals <- grepl("critical mineral|rare earth", m$name, ignore.case = TRUE)

    agreement_type <- if (m$mechanism_type == "plurilateral") {
      if (grepl("framework", m$name, ignore.case = TRUE)) "framework" else "plurilateral_forum"
    } else if (m$mechanism_type %in% names(PMEMO_MECHANISM_TYPES)) PMEMO_MECHANISM_TYPES[[m$mechanism_type]] else "other"

    # Codes: scope compacts, then the technology, instrument and stage of contributing rows.
    cl <- list()
    for (cp in agr_split(m$scope_compacts)[[1]]) {
      s <- pmemo_compact_sectors(cp, mtext, minerals)
      if (is.null(s)) add_unmapped(id, list(c("scope_compacts", cp))) else
        cl[[length(cl) + 1]] <- pmemo_code(id, "sector", s, paste0(m$mechanism_id, " scope compact ", cp))
    }
    for (k in seq_len(nrow(contributing))) {
      rc <- pmemo_row_codes(id, contributing[k, ], use_compact = FALSE, minerals_context = minerals)
      cl[[length(cl) + 1]] <- rc$codes
      add_unmapped(id, rc$unmapped)
    }
    if (m$has_finance %in% c("TRUE", "partial")) {
      ftext <- paste(mtext, paste(contributing$institution, collapse = " "))
      fin <- c(if (grepl(PMEMO_ECA_PATTERN, ftext)) "export_credit", if (grepl(PMEMO_DFI_PATTERN, ftext)) "development_finance")
      if (!length(fin)) fin <- "public_finance"
      cl[[length(cl) + 1]] <- pmemo_code(id, "instrument_types", fin, paste0(m$mechanism_id, " has_finance ", m$has_finance))
    }
    if (agreement_type == "fta") {
      cl[[length(cl) + 1]] <- data.frame(source_record_id = id, dimension = "sector", code = "trade", basis = "type_implied",
                                         n_hits = 1L, snippet = paste0(m$mechanism_id, " mechanism_type trade_agreement"),
                                         stringsAsFactors = FALSE)
    }
    rec_codes <- do.call(rbind, cl)
    codes[[length(codes) + 1]] <- rec_codes
    sectors <- unique(rec_codes$code[rec_codes$dimension == "sector"])

    # Status: the mechanism's own, then delivery evidence among its linked rows.
    base <- if (m$status %in% names(PMEMO_STATUS_MAP)) PMEMO_STATUS_MAP[[m$status]] else "unknown"
    raw <- m$status
    built <- delivery$evidence_id[delivery$status %in% c("under_construction", "operating")]
    fin_ids <- if (financing_signed_counts) {
      delivery$evidence_id[delivery$item_type == "financing" & delivery$status %in% c("signed", "in_force") &
                             delivery$verification == "verified"]
    } else character()
    status <- base
    if (length(built) || length(fin_ids)) {
      operating_itself <- m$status == "operating" || grepl("\\boperat(ing|ional)\\b", m$notes, ignore.case = TRUE)
      status <- if (operating_itself) "operational" else "implementation_underway"
      if (length(built)) raw <- paste0(raw, "; ", length(built), " linked project(s) under construction or operating (", paste(built, collapse = ", "), ")")
      if (length(fin_ids)) raw <- paste0(raw, "; ", length(fin_ids), " linked financing(s) signed, verified (", paste(fin_ids, collapse = ", "), ")")
    }

    # Date: a date in the mechanism's name (made precise by a core row of the same month or
    # year), else "launched <date>" in its notes, else the earliest core-row publication date.
    paren <- paste(regmatches(m$name, gregexpr("\\([^)]*\\)", m$name))[[1]], collapse = " ")
    name_date <- pmemo_text_date(paren)
    core_date <- pmemo_earliest(core$publication_date)
    launch <- regmatches(m$notes, regexpr("launched (in )?[^;.]*", m$notes))
    notes_date <- if (length(launch)) pmemo_text_date(launch) else ""
    date <- if (nzchar(name_date)) {
      if (nzchar(core_date) && startsWith(core_date, name_date)) core_date else name_date
    } else if (nzchar(notes_date)) notes_date else core_date
    date_field <- if (base == "announced" || agreement_type %in% c("ministerial_initiative", "plurilateral_forum")) "announcement_date" else "signature_date"

    src <- pmemo_pick_source(core, cfg)
    if (is.null(src)) src <- pmemo_pick_source(usable, cfg)
    legal_nb <- pmemo_nonbinding(c(m$notes, core$claim))
    parties <- agr_split(m$pair)[[1]]
    rec <- data.frame(
      source_dataset = "pmemo", source_record_id = id, title = m$name,
      agr_party_fields(list(parties)),
      agreement_type = agreement_type, agreement_type_raw = m$mechanism_type, partnership_label = "",
      legal_status = if (legal_nb) "non_binding" else "unknown",
      legal_status_basis = if (legal_nb) "curated" else "none",
      announcement_date = "", signature_date = "", effective_date = "", expiry_date = "",
      relationship_category = pmemo_relationship(agreement_type, m$mechanism_type, m$name, sectors),
      implementation_status = status, implementation_status_raw = raw,
      source_url = if (is.null(src)) "" else src$url, primary_document_url = "",
      last_verified = if ("assessed_on" %in% names(m)) m$assessed_on else "", source_last_updated = "",
      description = paste0(type_labels[[agreement_type]], " between ", paste(sort(unique(parties)), collapse = ", "),
                           ", assessed in the partnership-memo evidence (", length(ids), " evidence items linked)."),
      notes = paste0(m$notes, " Evidence: ", paste(ids, collapse = ", "), ".",
                     if (legal_nb) " Legal status: the memo evidence describes it as non-binding." else "",
                     if (!is.null(src)) paste0(" Source: ", src$source, " (", src$evidence_id, ", ", src$verification, ").") else ""),
      stringsAsFactors = FALSE)
    rec[[date_field]] <- date
    recs[[length(recs) + 1]] <- rec

    dl <- delivery[nzchar(delivery$url), , drop = FALSE]
    if (nrow(dl)) {
      impl[[length(impl) + 1]] <- data.frame(
        source_record_id = id, evidence_id = dl$evidence_id, item_type = dl$item_type,
        text = pmemo_shorten(dl$name), claim = pmemo_shorten(dl$claim), status = dl$status,
        amount_usd = dl$amount_num, url = dl$url, verification = dl$verification, stringsAsFactors = FALSE)
    }
    meta[[length(meta) + 1]] <- data.frame(source_record_id = id,
                                           verification = if (is.null(src)) "unverified" else src$verification,
                                           publisher = if (is.null(src)) "" else src$source, stringsAsFactors = FALSE)
    flag_cols <- intersect(c("cross_sector", "has_pipeline", "has_finance", "has_kpis", "has_review", "reciprocal",
                             "sunset", "assessed_on"), names(m))
    assess[[length(assess) + 1]] <- data.frame(source_record_id = id, mechanism_id = m$mechanism_id,
                                               m[, flag_cols, drop = FALSE], stringsAsFactors = FALSE, row.names = NULL)
  }

  # Evidence rows that are agreements in their own right.
  g2g_policy <- evidence$item_type == "policy" & evidence$n_countries >= 2
  candidate <- (evidence$item_type %in% PMEMO_RECORD_TYPES | g2g_policy) &
    !(evidence$item_type %in% PMEMO_NEVER_TYPES) & evidence$instrument != "export_control" &
    evidence$n_countries >= 2 & !(evidence$evidence_id %in% ref_ids) & !(evidence$evidence_id %in% names(exclude))
  dup_rows <- list()
  for (g in seq_along(duplicates)) {
    grp <- intersect(duplicates[[g]], evidence$evidence_id)
    linked <- grp[grp %in% ref_ids]
    cand <- grp[grp %in% evidence$evidence_id[candidate]]
    if (length(linked)) {
      keep_desc <- paste0(linked, " (", link_owner[linked], ")")
      for (e in grp) {
        action <- if (e %in% linked) paste0("kept: linked to ", link_owner[[e]])
          else if (e %in% cand) paste0("dropped: duplicate of ", paste(keep_desc, collapse = ", "))
          else paste0("not a record (", evidence$item_type[evidence$evidence_id == e], "); duplicate of ", paste(keep_desc, collapse = ", "))
        dup_rows[[length(dup_rows) + 1]] <- data.frame(duplicate_group = g, evidence_id = e, action = action, stringsAsFactors = FALSE)
      }
      candidate[evidence$evidence_id %in% cand] <- FALSE
    } else {
      keep <- if (length(cand)) cand[1] else character()
      for (e in grp) {
        action <- if (e %in% keep) "kept: record of its own"
          else if (e %in% cand) paste0("dropped: duplicate of ", keep)
          else paste0("not a record (", evidence$item_type[evidence$evidence_id == e], "); no member is linked to a mechanism")
        dup_rows[[length(dup_rows) + 1]] <- data.frame(duplicate_group = g, evidence_id = e, action = action, stringsAsFactors = FALSE)
      }
      candidate[evidence$evidence_id %in% setdiff(cand, keep)] <- FALSE
    }
  }
  for (k in which(candidate)) {
    e <- evidence[k, ]
    id <- paste0("pmemo:", e$evidence_id)
    minerals <- grepl("critical mineral|rare earth", e$name, ignore.case = TRUE)
    rc <- pmemo_row_codes(id, e, use_compact = TRUE, minerals_context = minerals)
    add_unmapped(id, rc$unmapped)
    agreement_type <- switch(e$item_type,
      agreement = if (e$instrument %in% c("trade_agreement", "tariff")) "fta"
                  else if (grepl("\\bMOU\\b|memorandum", e$name, ignore.case = TRUE)) "mou"
                  else if (grepl("framework", e$name, ignore.case = TRUE)) "framework" else "other",
      framework = "framework",
      plurilateral = if (grepl("framework", e$name, ignore.case = TRUE)) "framework" else "plurilateral_forum",
      dialogue = "ministerial_initiative",
      policy = if (grepl("framework", e$name, ignore.case = TRUE)) "framework"
               else if (grepl("\\bMOU\\b|memorandum", e$name, ignore.case = TRUE)) "mou" else "other",
      "other")
    rec_codes <- rc$codes
    if (agreement_type == "fta") {
      rec_codes <- rbind(rec_codes, data.frame(source_record_id = id, dimension = "sector", code = "trade", basis = "type_implied",
                                               n_hits = 1L, snippet = paste0(e$evidence_id, " instrument ", e$instrument),
                                               stringsAsFactors = FALSE))
    }
    codes[[length(codes) + 1]] <- rec_codes
    sectors <- unique(rec_codes$code[rec_codes$dimension == "sector"])
    base <- if (e$status %in% names(PMEMO_STATUS_MAP)) PMEMO_STATUS_MAP[[e$status]] else "unknown"
    date_field <- if (base == "announced" || agreement_type %in% c("ministerial_initiative", "plurilateral_forum")) "announcement_date" else "signature_date"
    legal_nb <- pmemo_nonbinding(e$claim)
    parties <- unique(agr_split(e$countries)[[1]])
    rec <- data.frame(
      source_dataset = "pmemo", source_record_id = id, title = e$name,
      agr_party_fields(list(parties)),
      agreement_type = agreement_type, agreement_type_raw = e$item_type, partnership_label = "",
      legal_status = if (legal_nb) "non_binding" else "unknown",
      legal_status_basis = if (legal_nb) "curated" else "none",
      announcement_date = "", signature_date = "", effective_date = "", expiry_date = "",
      relationship_category = pmemo_relationship(agreement_type, e$item_type, e$name, sectors),
      implementation_status = base, implementation_status_raw = e$status,
      source_url = e$url, primary_document_url = "",
      last_verified = e$access_date, source_last_updated = "",
      description = paste0(type_labels[[agreement_type]], " between ", paste(sort(parties), collapse = ", "),
                           ", recorded as a single evidence item (", e$evidence_id, ") in the partnership-memo evidence."),
      notes = paste0(e$claim, " Source: ", e$source, " (", e$verification, ").",
                     if (e$item_type == "plurilateral") " Only the countries the evidence row names are listed as parties." else ""),
      stringsAsFactors = FALSE)
    rec[[date_field]] <- e$publication_date
    recs[[length(recs) + 1]] <- rec
    meta[[length(meta) + 1]] <- data.frame(source_record_id = id, verification = e$verification, publisher = e$source,
                                           stringsAsFactors = FALSE)
  }

  records <- agr_as_records(do.call(rbind, recs))
  records$primary_source_available <- agr_is_official_url(records$source_url, cfg$official_hosts)
  codes <- pmemo_derive(do.call(rbind, codes), cfg)
  records <- agr_apply_codes(records, codes)
  bind <- function(x, empty) if (length(x)) { out <- do.call(rbind, x); rownames(out) <- NULL; out } else empty
  list(records = records, codes = codes, extras = list(
    implementation_evidence = bind(impl, data.frame(source_record_id = character(), evidence_id = character(),
                                                    item_type = character(), text = character(), claim = character(),
                                                    status = character(), amount_usd = numeric(), url = character(),
                                                    verification = character(), stringsAsFactors = FALSE)),
    curated_meta = bind(meta, data.frame(source_record_id = character(), verification = character(),
                                         publisher = character(), stringsAsFactors = FALSE)),
    mechanism_assessment = bind(assess, data.frame(source_record_id = character(), mechanism_id = character(),
                                                   stringsAsFactors = FALSE)),
    duplicates = bind(dup_rows, data.frame(duplicate_group = integer(), evidence_id = character(), action = character(),
                                           stringsAsFactors = FALSE)),
    unmapped = bind(unmapped, data.frame(source_record_id = character(), field = character(), value = character(),
                                         stringsAsFactors = FALSE)),
    unresolved_links = bind(unresolved, data.frame(source_record_id = character(), evidence_id = character(),
                                                   stringsAsFactors = FALSE)),
    excluded = excluded
  ))
}
