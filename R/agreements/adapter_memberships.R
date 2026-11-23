# Memberships adapter (docs/partnership_agreements.md, section 10 "Sources").
#
# data/reference/economic_security_agreements.csv has one row per iso3 x agreement for six
# codes. This adapter turns them into agreement records in the common schema:
#   U.S. FTA   one bilateral USA-X record per row. The USA has no row: every U.S. FTA is with
#              the USA. Plurilateral FTAs (CAFTA-DR, NAFTA/USMCA) stay one USA-X leg per row,
#              and the notes name the other partners the CSV lists under the same agreement.
#   IPEF, MSP, MSP Forum, G7, NATO   one plurilateral record each, with every listed member.
#              The MSP Forum rows list Forum-only members; their note says MSP partners are
#              automatically Forum members, so the MSP partners are added to the Forum record
#              (flagged in extras$members). No other party is added.
# Type, legal status and scope are recorded by hand per club (MEMBERSHIP_SPECS) with the
# official page they rest on. The reference CSV and its other consumers are unchanged.
# Pure: the caller reads the CSV and the config and passes them in.

# Official pages checked on 2026-11-06 for the IPEF codes (commerce.gov/ipef returned HTTP 403).
MEMBERSHIP_IPEF_LAUNCH_URL <- "https://bidenwhitehouse.archives.gov/briefing-room/statements-releases/2022/05/23/statement-on-indo-pacific-economic-framework-for-prosperity/"
MEMBERSHIP_IPEF_SCA_URL <- "https://www.meti.gov.sg/trade-international-economic-relations/regional-and-international-platforms/indo-pacific-economic-framework/ipef-pillars-and-agreements/supply-chain-agreement"

# One entry per `agreement` value of the CSV. `date_field` says which record date the earliest
# `since` fills: the CSV's U.S. FTA dates are entry-into-force (CBP "Implementation Date"), the
# NATO founding date is the treaty signature (row notes), and a club's earliest date is its launch.
MEMBERSHIP_SPECS <- list(
  "U.S. FTA" = list(
    key = "us_fta", title = NA_character_, agreement_type = "fta",
    legal_status = "binding",
    legal_status_note = "Comprehensive free trade agreement in force with the United States (USTR list of FTAs in force); a binding international agreement.",
    relationship_category = "trade_economic", date_field = "effective_date",
    codes = list(
      list(dimension = "sector", code = "trade", basis = "type_implied", snippet = "U.S. FTA: a free trade agreement in force"),
      list(dimension = "instrument_types", code = "trade_preference", basis = "type_implied", snippet = "U.S. FTA: preferential tariff treatment between the parties"),
      list(dimension = "instrument_types", code = "tariff_reduction", basis = "type_implied", snippet = "U.S. FTA: tariff elimination schedules")
    )),
  "IPEF" = list(
    key = "ipef", title = "Indo-Pacific Economic Framework for Prosperity (IPEF)", agreement_type = "framework",
    legal_status = "mixed",
    legal_status_note = paste(
      "Mixed: the framework itself is a non-binding negotiating framework launched by a leaders' statement (2022-05-23);",
      "its pillar agreements bind the partners that ratify them (the Supply Chain Agreement was signed by all 14 partners",
      "on 2023-11-14 and entered into force on 2024-02-24, Singapore Ministry of Trade and Industry IPEF page)."),
    relationship_category = "plurilateral_club", date_field = "announcement_date",
    status = "in_force",
    status_raw = "partner; Supply Chain Agreement in force since 2024-02-24 (Singapore MTI IPEF page, checked 2026-11-06)",
    codes = list(
      list(dimension = "sector", code = "trade", basis = "curated",
           snippet = paste0("IPEF pillar 'Trade' (White House launch statement 2022-05-23, ", MEMBERSHIP_IPEF_LAUNCH_URL, ")")),
      list(dimension = "sector", code = "general_industrial", basis = "curated",
           snippet = paste0("IPEF pillar 'Supply Chains' (White House launch statement 2022-05-23, ", MEMBERSHIP_IPEF_LAUNCH_URL, ")")),
      list(dimension = "sector", code = "general_clean_energy", basis = "curated",
           snippet = paste0("IPEF pillar 'Clean Energy, Decarbonization, and Infrastructure' (White House launch statement 2022-05-23, ", MEMBERSHIP_IPEF_LAUNCH_URL, ")")),
      list(dimension = "instrument_types", code = "supply_chain_monitoring", basis = "curated",
           snippet = paste0("IPEF Supply Chain Agreement (in force 2024-02-24) establishes a Supply Chain Council to develop sector-specific action plans for resilience in critical sectors (", MEMBERSHIP_IPEF_SCA_URL, ")")),
      list(dimension = "instrument_types", code = "early_warning", basis = "curated",
           snippet = paste0("IPEF Supply Chain Agreement establishes a Crisis Response Network for emergency communications and coordination in supply-chain disruptions (", MEMBERSHIP_IPEF_SCA_URL, ")"))
    )),
  "MSP" = list(
    key = "msp", title = "Minerals Security Partnership (MSP)", agreement_type = "ministerial_initiative",
    legal_status = "non_binding",
    legal_status_note = "Non-binding: a partnership of governments announced by the U.S. Department of State (June 2022), with no treaty text.",
    relationship_category = "plurilateral_club", date_field = "announcement_date",
    codes = list(
      list(dimension = "sector", code = "critical_minerals", basis = "curated",
           snippet = "Minerals Security Partnership: the club's stated scope is critical-mineral supply chains (European Commission DG Trade, 2024-09-27)")
    )),
  "MSP Forum" = list(
    key = "msp_forum", title = "Minerals Security Partnership Forum (MSP Forum)", agreement_type = "plurilateral_forum",
    legal_status = "non_binding",
    legal_status_note = "Non-binding: a forum of MSP partners and Forum members convened by the MSP (July 2024), with no treaty text.",
    relationship_category = "plurilateral_club", date_field = "announcement_date",
    add_parties_from = "MSP",
    codes = list(
      list(dimension = "sector", code = "critical_minerals", basis = "curated",
           snippet = "MSP Forum: a forum of the Minerals Security Partnership on critical-mineral supply chains (European Commission DG Trade, 2024-09-27)")
    )),
  "G7" = list(
    key = "g7", title = "Group of Seven (G7)", agreement_type = "plurilateral_forum",
    legal_status = "non_binding",
    legal_status_note = "Non-binding: an informal forum of leaders that meets at annual summits; it has no founding treaty.",
    relationship_category = "plurilateral_club", date_field = "announcement_date",
    codes = list()),
  "NATO" = list(
    key = "nato", title = "North Atlantic Treaty Organization (NATO)", agreement_type = "treaty",
    legal_status = "binding",
    legal_status_note = "Binding: the North Atlantic Treaty, signed 1949-04-04 (row notes); members accede to the treaty.",
    relationship_category = "plurilateral_club", date_field = "signature_date",
    codes = list())
)

# End each note part with a full stop and join them.
membership_join_notes <- function(parts) {
  parts <- agr_chr(parts)
  parts <- parts[nzchar(parts)]
  parts <- ifelse(grepl("[.!?]$", parts), parts, paste0(parts, "."))
  paste(parts, collapse = " ")
}

# First URL of a "; "-separated url cell.
membership_first_url <- function(x) {
  x <- agr_chr(x)
  vapply(strsplit(x, "\\s*;\\s*"), function(u) if (length(u)) u[1] else "", character(1))
}

# Earliest of a vector of normalised dates (mixed precision compared by their first day); "" if none.
membership_earliest <- function(x) {
  x <- agr_norm_date(x)
  x <- x[nzchar(x)]
  if (!length(x)) return("")
  v <- agr_date_value(x)
  x[order(v, -nchar(x))][1]
}

# Implementation status of a club from its membership_status values and row notes.
membership_status <- function(spec, sub) {
  raw <- paste(sort(unique(agr_chr(sub$membership_status))), collapse = "; ")
  notes <- paste(agr_chr(sub$note), collapse = " ")
  if (!is.null(spec$status)) return(list(status = spec$status, raw = spec$status_raw))
  if (grepl("succeeded by FORGE", notes, fixed = TRUE)) {
    return(list(status = "inactive",
                raw = paste0(raw, "; succeeded by FORGE (Forum on Resource Geostrategic Engagement), February 2026, per row notes")))
  }
  current <- c("in_force", "member", "participant", "partner", "forum_member")
  st <- if (all(agr_chr(sub$membership_status) %in% current)) "in_force" else "unknown"
  list(status = st, raw = raw)
}

membership_codes <- function(id, spec) {
  if (!length(spec$codes)) return(agr_empty_codes())
  do.call(rbind, lapply(spec$codes, function(cd) {
    data.frame(source_record_id = id, dimension = cd$dimension, code = cd$code, basis = cd$basis,
               n_hits = 1L, snippet = cd$snippet, stringsAsFactors = FALSE)
  }))
}

# tbl: the reference CSV as read (all columns character is fine). cfg: the parsed
# config/partnership_agreements.yml. Returns list(records, codes, extras) where extras$members
# is one row per record x member (source_record_id, iso3, country, membership_status, since,
# added_from_note).
agr_adapt_memberships <- function(tbl, cfg) {
  need <- c("iso3", "country", "agreement", "membership_status", "since", "source", "url", "accessed", "note")
  miss <- setdiff(need, names(tbl))
  if (length(miss)) stop("memberships: the reference table lacks columns: ", paste(miss, collapse = ", "))
  tbl <- as.data.frame(lapply(tbl[need], agr_chr), stringsAsFactors = FALSE)
  unknown <- setdiff(unique(tbl$agreement), names(MEMBERSHIP_SPECS))
  if (length(unknown)) {
    stop("memberships: no MEMBERSHIP_SPECS entry for agreement(s): ", paste(unknown, collapse = ", "),
         " (add one in R/agreements/adapter_memberships.R)")
  }
  bad_iso <- unique(tbl$iso3[!grepl("^[A-Z]{3}$", tbl$iso3)])
  if (length(bad_iso)) stop("memberships: iso3 values are not ISO3 codes: ", paste(bad_iso, collapse = ", "))

  recs <- list()
  codes <- list()
  members <- list()

  # U.S. FTA: one bilateral record per row.
  fta <- tbl[tbl$agreement == "U.S. FTA", , drop = FALSE]
  spec <- MEMBERSHIP_SPECS[["U.S. FTA"]]
  for (i in seq_len(nrow(fta))) {
    r <- fta[i, ]
    id <- paste0("memberships:", spec$key, ":", r$iso3)
    title <- if (nzchar(r$note)) r$note else paste0("U.S.-", r$country, " FTA")
    note_parts <- c(paste0("Reference row note: ", r$note), paste0("Source: ", r$source),
                    "The USA has no row in the reference table; every U.S. FTA is with the USA.")
    same <- setdiff(fta$iso3[fta$note == r$note & nzchar(r$note)], r$iso3)
    if (length(same)) {
      note_parts <- c(note_parts, paste0(
        "The reference table lists the same agreement for ", paste(sort(same), collapse = ", "),
        "; this record is the USA-", r$iso3, " leg only."))
    }
    if (grepl("replaced by USMCA", r$note, fixed = TRUE)) {
      title <- "NAFTA / USMCA"
      note_parts <- c(note_parts, paste(
        "NAFTA was implemented on 1994-01-01 (the effective date here) and replaced by USMCA on 2020-07-01;",
        "the record stands for the U.S. FTA relationship, in force under USMCA since 2020-07-01."))
    }
    recs[[length(recs) + 1]] <- data.frame(
      source_dataset = "memberships", source_record_id = id, title = title,
      agr_party_fields(list(c("USA", r$iso3))),
      agreement_type = spec$agreement_type, agreement_type_raw = "U.S. FTA", partnership_label = "",
      legal_status = spec$legal_status, legal_status_basis = "curated",
      announcement_date = "", signature_date = "", effective_date = r$since, expiry_date = "",
      relationship_category = spec$relationship_category,
      implementation_status = if (r$membership_status == "in_force") "in_force" else "unknown",
      implementation_status_raw = r$membership_status,
      source_url = membership_first_url(r$url), primary_document_url = "",
      last_verified = r$accessed, source_last_updated = "",
      description = paste0("Free trade agreement between the United States and ", r$country, " (", title,
                           "), in force since ", agr_norm_date(r$since), " according to the USTR list of U.S. FTAs."),
      notes = membership_join_notes(c(paste0("Legal status: ", spec$legal_status_note), note_parts)),
      stringsAsFactors = FALSE)
    codes[[length(codes) + 1]] <- membership_codes(id, spec)
    members[[length(members) + 1]] <- data.frame(
      source_record_id = id, iso3 = c("USA", r$iso3), country = c("United States", r$country),
      membership_status = c("party (implicit)", r$membership_status), since = c("", agr_norm_date(r$since)),
      added_from_note = c(TRUE, FALSE), stringsAsFactors = FALSE)
  }

  # Clubs: one plurilateral record each.
  for (ag in setdiff(unique(tbl$agreement), "U.S. FTA")) {
    spec <- MEMBERSHIP_SPECS[[ag]]
    sub <- tbl[tbl$agreement == ag, , drop = FALSE]
    id <- paste0("memberships:", spec$key)
    mem <- data.frame(source_record_id = id, iso3 = sub$iso3, country = sub$country,
                      membership_status = sub$membership_status, since = agr_norm_date(sub$since),
                      added_from_note = FALSE, stringsAsFactors = FALSE)
    extra_note <- character()
    if (!is.null(spec$add_parties_from)) {
      says_so <- any(grepl("partners are automatically Forum members", sub$note, fixed = TRUE))
      donor <- tbl[tbl$agreement == spec$add_parties_from, , drop = FALSE]
      add <- donor[!(donor$iso3 %in% sub$iso3), , drop = FALSE]
      if (says_so && nrow(add)) {
        mem <- rbind(mem, data.frame(
          source_record_id = id, iso3 = add$iso3, country = add$country,
          membership_status = paste0(spec$add_parties_from, " partner (automatic Forum member per the reference note)"),
          since = "", added_from_note = TRUE, stringsAsFactors = FALSE))
        extra_note <- paste0(nrow(sub), " Forum-only members are listed in the reference table; the ", nrow(add), " ",
                             spec$add_parties_from, " partners (", paste(sort(add$iso3), collapse = ", "),
                             ") are added because the rows note that MSP partners are automatically Forum members.")
      }
    }
    date <- membership_earliest(sub$since)
    st <- membership_status(spec, sub)
    urls <- unique(membership_first_url(sub$url))
    row_notes <- unique(sub$note[nzchar(sub$note)])
    common <- if (length(row_notes)) {
      # Notes repeated on most rows (e.g. the FORGE succession) are reported once.
      tab <- table(sub$note[nzchar(sub$note)])
      names(tab)[tab == max(tab)][1]
    } else ""
    rec <- data.frame(
      source_dataset = "memberships", source_record_id = id, title = spec$title,
      agr_party_fields(list(mem$iso3)),
      agreement_type = spec$agreement_type, agreement_type_raw = ag, partnership_label = "",
      legal_status = spec$legal_status, legal_status_basis = "curated",
      announcement_date = "", signature_date = "", effective_date = "", expiry_date = "",
      relationship_category = spec$relationship_category,
      implementation_status = st$status, implementation_status_raw = st$raw,
      source_url = urls[1], primary_document_url = "",
      last_verified = membership_earliest(sub$accessed), source_last_updated = "",
      description = paste0(spec$title, ": ", length(unique(mem$iso3)), " members listed in the project reference table, earliest membership ",
                           if (nzchar(date)) date else "undated", "."),
      notes = membership_join_notes(c(
        paste0("Legal status: ", spec$legal_status_note),
        paste0("Date: the earliest membership date in the reference table (", spec$date_field, ")"),
        extra_note,
        if (nzchar(common)) paste0("Reference row note (most rows): ", common),
        paste0("Sources: ", paste(unique(sub$source), collapse = " | ")))),
      stringsAsFactors = FALSE)
    rec[[spec$date_field]] <- date
    recs[[length(recs) + 1]] <- rec
    codes[[length(codes) + 1]] <- membership_codes(id, spec)
    members[[length(members) + 1]] <- mem
  }

  records <- agr_as_records(do.call(rbind, recs))
  records$primary_source_available <- agr_is_official_url(records$source_url, cfg$official_hosts)
  codes <- agr_derive_tech_codes(do.call(rbind, codes), cfg)
  records <- agr_apply_codes(records, codes)
  members <- do.call(rbind, members)
  rownames(members) <- NULL
  list(records = records, codes = codes, extras = list(members = members))
}
