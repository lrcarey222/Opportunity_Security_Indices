# DESTA (Design of Trade Agreements) adapter (docs/partnership_agreements.md, "Sources").
#
# Input: the DESTA v2.x list of treaties (one row per entry: base treaties, accessions,
# withdrawals, protocols or amendments, consolidated versions and negotiations; parties in
# c1..c91 as numeric ISO 3166-1 codes with membership roles in mc1..mc91), the content coding
# (provisions, one row per coded entry), the indices (depth_index and its seven components,
# depth_rasch, flexibility, enforcement) and the dyadic withdrawals, as data frames read from
# data/raw/desta_*.csv (scripts/fetchers/desta.R). Pure: no IO.
#
# Entries are keyed by (number, base_treaty): `number` is not unique on its own (1142 is both a
# protocol of 1095 and a negotiation). The rules, in order:
#
#  * Records. One record per base treaty and per accession entry. A protocol or amendment is a
#    record of its own only when its party set differs from its parent's parties at the protocol's
#    year (the parent's c-list, plus accessions, minus withdrawals filed against it up to that year):
#    DESTA files the UK's 2019-2021 continuity agreements (Korea UK, Canada UK, Switzerland UK, ...),
#    the ALADI bilaterals and a few re-founding protocols (SADC 1996, CEFTA 2006) as protocols of the
#    agreement they replace, with their own parties. Protocols with the parent's parties amend it and
#    are not records. Consolidated entries are never records; negotiations are skipped (counted).
#  * Parties come from c1..c91 of the list file, never from the dyads file (which omits dyads by
#    membership role: RCEP lists no JPN-KOR dyad). Numeric ISO -> ISO3 with countrycode; 900 is
#    Kosovo (XKX); 530 (Netherlands Antilles, absent from v2.3) and other unmapped codes are dropped
#    and noted.
#  * Accessions. An accession record's parties are the parties of its agreement just after the
#    accession (base c-list, plus every earlier and this accession, minus earlier withdrawals); the
#    acceding countries are the ones not already parties (DESTA role 1 for typememb 5, role 2 for
#    typememb 6 when the difference is empty). Titles read "<agreement> (accession of <names>)".
#  * Withdrawals (DESTA role 1 = withdrawing country; the dyadic withdrawal file is the fallback)
#    remove the withdrawing country from the agreement they are filed against and from its
#    accession records ("GBR withdrew 2020" in notes). A record whose remaining parties are fewer
#    than two, or an accession record whose acceding countries all withdrew, is `inactive` and
#    keeps its parties. Withdrawals are not applied to protocol records of the same family (the
#    UK withdrew from EC Korea but is the party of Korea UK).
#  * EU collapse. DESTA lists EC/EU agreements under the member states. An agreement is an EU
#    agreement when its name names the EC/EU (DESTA writes "EC" throughout) or, for a two-sided
#    agreement (typememb 3 or 4), its parties include every EU member state of its signature year
#    (Lome, Cotonou, Arusha). Its EU side - the parties that were EU members in the record's year -
#    is replaced by EUU when at least one non-EU party remains; the pair roll-up propagates EUU to
#    current member states (config pairs.propagate_eu). Accessions of new EU member states to an EU
#    agreement add no party under EUU and are not records (counted). Agreements among EU members
#    only (the EC treaties) stay country lists. Other blocs (EFTA, ASEAN, MERCOSUR) stay country
#    lists.
#  * Types. agreement_type `epa` when the name or WTO name says Economic Partnership or EPA, or
#    when Japan is a party of a bilateral or third-country agreement without the United States
#    (Japan designates all its trade agreements except the 2019 US agreements as EPAs); `framework`
#    for DESTA mar_typedepth 5 (framework agreement, no specific provisions); `fta` otherwise
#    (customs unions, partial-scope and services agreements included). agreement_type_raw keeps
#    DESTA's own words (mar_typedepth; typememb).
#  * Status. in_force when entryforceyear <= as_of_year, signed otherwise; inactive as above.
#    DESTA does not code supersession, so no agreement is made inactive for being replaced.
#  * Coding. A record takes the provisions and indices of the newest consolidated entry of its
#    agreement (consolidated "Xa" consolidates protocol X with its agreement) when one exists,
#    else of the agreement's own entry; accession records take their agreement's. depth_index
#    comes from the indices file, or is recomputed from the provisions with the published rule
#    (agr_desta_depth_components(); it reproduces all 692 published values of v2.3).

AGR_DESTA_SOURCE_URL <- "https://www.designoftradeagreements.org/downloads/"
AGR_DESTA_RELEASE <- "2025-09"

# Numeric codes countrycode does not map, or maps differently from DESTA's use.
AGR_DESTA_SPECIAL_ISO <- c(`900` = "XKX", `729` = "SDN", `158` = "TWN", `344` = "HKG", `446` = "MAC")
# Dropped (no current ISO3; the Netherlands Antilles dissolved in 2010).
AGR_DESTA_DROP_CODES <- c(530L)

# EU membership history, for finding the EU side of an agreement in its year. States in
# `eu_members` that are missing here count as members from as_of_year.
AGR_DESTA_EU_JOIN_YEAR <- c(
  BEL = 1958L, DEU = 1958L, FRA = 1958L, ITA = 1958L, LUX = 1958L, NLD = 1958L,
  DNK = 1973L, IRL = 1973L, GBR = 1973L, GRC = 1981L, ESP = 1986L, PRT = 1986L,
  AUT = 1995L, FIN = 1995L, SWE = 1995L,
  CYP = 2004L, CZE = 2004L, EST = 2004L, HUN = 2004L, LVA = 2004L, LTU = 2004L, MLT = 2004L,
  POL = 2004L, SVK = 2004L, SVN = 2004L, BGR = 2007L, ROU = 2007L, HRV = 2013L
)
AGR_DESTA_EU_EXIT_YEAR <- c(GBR = 2020L)

AGR_DESTA_EU_NAME_PATTERN <- "(^|[^A-Za-z])(EC|EU|EEC|European Communit(y|ies)|European Economic Community|European Union)([^A-Za-z]|$)"
AGR_DESTA_EPA_PATTERN <- "Economic Partnership|(^|[^A-Za-z])(EPA|CEPA)([^A-Za-z]|$)"

AGR_DESTA_TYPEDEPTH <- c(`1` = "partial scope agreement", `2` = "free trade agreement", `3` = "customs union",
                         `4` = "services agreement", `5` = "framework agreement (no specific provisions)")
AGR_DESTA_TYPEMEMB <- c(`1` = "bilateral", `2` = "plurilateral", `3` = "plurilateral and third country",
                        `4` = "region-region", `5` = "accession", `6` = "accession through regional membership",
                        `7` = "withdrawal")

AGR_DESTA_DEPTH_COMPONENTS <- c("full_fta", "standards", "investments", "services", "procurement",
                                "competition", "iprs")

# --- helpers ------------------------------------------------------------------------------

agr_desta_int <- function(x) {
  if (is.null(x)) return(NULL)
  suppressWarnings(as.integer(trimws(as.character(x))))
}

agr_desta_flag <- function(df, var, value = 1L) {
  v <- agr_desta_int(df[[var]])
  if (is.null(v)) return(rep(FALSE, nrow(df)))
  !is.na(v) & v == value
}

# One integer column of a frame (NA when the column is missing).
agr_desta_col <- function(df, var) {
  v <- agr_desta_int(df[[var]])
  if (is.null(v)) rep(NA_integer_, nrow(df)) else v
}

# Numeric ISO 3166-1 -> ISO3, with DESTA's special codes. NA where unmapped or dropped.
agr_desta_iso3 <- function(codes) {
  codes <- agr_desta_int(codes)
  out <- rep(NA_character_, length(codes))
  ok <- !is.na(codes)
  if (any(ok)) {
    u <- sort(unique(codes[ok]))
    m <- suppressWarnings(countrycode::countrycode(u, "iso3n", "iso3c", warn = FALSE))
    special <- as.character(u) %in% names(AGR_DESTA_SPECIAL_ISO)
    m[special] <- AGR_DESTA_SPECIAL_ISO[as.character(u[special])]
    m[u %in% AGR_DESTA_DROP_CODES] <- NA_character_
    out[ok] <- m[match(codes[ok], u)]
  }
  out
}

agr_desta_country_names <- function(iso3) {
  nm <- suppressWarnings(countrycode::countrycode(iso3, "iso3c", "country.name", warn = FALSE,
                                                  custom_match = c(XKX = "Kosovo", EUU = "European Union")))
  ifelse(is.na(nm), iso3, nm)
}

# The seven depth components and depth_index from a provisions frame, with the rule of the DESTA
# indices note (missing values count as 0): full_fta = mar_typedepth 2 or 3; standards = sps_prov
# or tbt_prov; investments = inv_chapter, inv_reference_service or inv_reference_bit; services =
# ser_specific; procurement = proc_prov 2; competition = comp_chap; iprs = ipr_nt, ipr_mfn,
# ipr_scope_substantial_dummy, ipr_specific_enforcement_dummy or ipr_specific_prov.
agr_desta_depth_components <- function(provisions) {
  f <- function(...) {
    vars <- c(...)
    Reduce(`|`, lapply(vars, function(v) agr_desta_flag(provisions, v)), rep(FALSE, nrow(provisions)))
  }
  td <- agr_desta_col(provisions, "mar_typedepth")
  out <- data.frame(
    full_fta = as.integer(!is.na(td) & td %in% c(2L, 3L)),
    standards = as.integer(f("sps_prov", "tbt_prov")),
    investments = as.integer(f("inv_chapter", "inv_reference_service", "inv_reference_bit")),
    services = as.integer(f("ser_specific")),
    procurement = as.integer(agr_desta_flag(provisions, "proc_prov", 2L)),
    competition = as.integer(f("comp_chap")),
    iprs = as.integer(f("ipr_nt", "ipr_mfn", "ipr_scope_substantial_dummy",
                        "ipr_specific_enforcement_dummy", "ipr_specific_prov")),
    stringsAsFactors = FALSE
  )
  out$depth_index <- rowSums(out[, AGR_DESTA_DEPTH_COMPONENTS])
  out
}

# Recomputed depth_index against the published one, on every entry that has both: the number
# compared and the keys that differ.
agr_desta_check_depth <- function(provisions, indices) {
  if (is.null(provisions) || is.null(indices) || nrow(provisions) == 0 || nrow(indices) == 0) {
    return(list(n_compared = 0L, mismatches = character()))
  }
  pk <- paste(agr_chr(provisions$number), agr_chr(provisions$base_treaty), sep = "\r")
  ik <- paste(agr_chr(indices$number), agr_chr(indices$base_treaty), sep = "\r")
  both <- intersect(pk, ik)
  re <- agr_desta_depth_components(provisions)$depth_index[match(both, pk)]
  pub <- agr_desta_col(indices, "depth_index")[match(both, ik)]
  bad <- !is.na(pub) & re != pub
  list(n_compared = sum(!is.na(pub)), mismatches = sub("\r.*$", "", both[bad]))
}

# Instrument codes from one provisions row (a one-row data frame) or, without provisions, from
# the depth components of an indices row. Only instruments a DESTA variable codes explicitly.
# DESTA has no mutual-recognition variable, so mutual_recognition is never coded here.
agr_desta_instruments <- function(prov, idx) {
  if (!is.null(prov) && nrow(prov) == 1) {
    td <- agr_desta_col(prov, "mar_typedepth")
    fl <- function(v, value = 1L) isTRUE(agr_desta_flag(prov, v, value))
    any_of <- function(...) any(vapply(c(...), fl, logical(1)))
    out <- character()
    if (is.na(td) || td != 5L) out <- c(out, "trade_preference")
    if (!is.na(td) && td %in% c(2L, 3L)) out <- c(out, "tariff_reduction")
    if (fl("ser_specific")) out <- c(out, "market_access")
    if (fl("inv_chapter")) out <- c(out, "fdi_framework")
    if (any_of("inv_pre_est_oper", "inv_nt", "inv_mfn")) out <- c(out, "investment_facilitation")
    if (fl("proc_prov", 2L)) out <- c(out, "government_procurement")
    if (any_of("tbt_prov", "sps_prov")) out <- c(out, "standards_cooperation")
    if (any_of("regulation_cooperation_chapter", "tbt_coop")) out <- c(out, "regulatory_cooperation")
    if (fl("regulation_information_exchange")) out <- c(out, "data_sharing")
    if (any_of("comp_specific_body", "gvc_cooperation_specific")) out <- c(out, "working_group")
    return(list(codes = out, basis = "source_coded"))
  }
  if (!is.null(idx) && nrow(idx) == 1) {
    fl <- function(v) isTRUE(agr_desta_flag(idx, v))
    out <- "trade_preference"
    if (fl("full_fta")) out <- c(out, "tariff_reduction")
    if (fl("services")) out <- c(out, "market_access")
    if (fl("procurement")) out <- c(out, "government_procurement")
    if (fl("standards")) out <- c(out, "standards_cooperation")
    return(list(codes = out, basis = "source_coded"))
  }
  # Not content-coded: DESTA lists only agreements that liberalise some trade.
  list(codes = "trade_preference", basis = "type_implied")
}

agr_desta_eu_member_at <- function(iso3, year, eu_ever, join, exit, tolerance = 0L) {
  j <- join[iso3]
  e <- exit[iso3]
  iso3 %in% eu_ever & !is.na(year) & !is.na(j) & j <= year + tolerance & (is.na(e) | e > year)
}

# --- adapter ------------------------------------------------------------------------------

# Records, codes and extras for DESTA. `eu_members`: ISO3 of the current EU member states
# (former members are added from AGR_DESTA_EU_EXIT_YEAR). `fetched_on`: the fetch date
# (last_verified; defaults to the list file's fetched_on column when there is one).
# `protocol_records = FALSE` restores "protocols are never records".
agr_adapt_desta <- function(list_of_treaties, provisions, indices, withdrawals, cfg, eu_members,
                            fetched_on = "", as_of_year = 2026, protocol_records = TRUE) {
  lst <- as.data.frame(list_of_treaties, stringsAsFactors = FALSE)
  if (!nzchar(agr_chr(fetched_on)[1]) && "fetched_on" %in% names(lst)) {
    fo <- agr_chr(lst$fetched_on)
    fetched_on <- if (any(nzchar(fo))) max(fo[nzchar(fo)]) else ""
  }
  release <- if ("source_release" %in% names(lst) && any(nzchar(agr_chr(lst$source_release)))) {
    agr_chr(lst$source_release)[nzchar(agr_chr(lst$source_release))][1]
  } else AGR_DESTA_RELEASE
  as_of_year <- as.integer(as_of_year)

  lst$number <- agr_chr(lst$number)
  lst$base_treaty <- agr_chr(lst$base_treaty)
  lst$entry_type <- agr_chr(lst$entry_type)
  lst$name <- trimws(gsub("[[:space:] ]+", " ", agr_chr(lst$name)))
  lst$wto_name <- if ("wto_name" %in% names(lst)) trimws(gsub("[[:space:] ]+", " ", agr_chr(lst$wto_name))) else ""
  lst$regioncon <- if ("regioncon" %in% names(lst)) agr_chr(lst$regioncon) else ""
  lst$year_i <- agr_desta_int(lst$year)
  lst$eif_i <- agr_desta_int(lst$entryforceyear)
  lst$typememb_i <- agr_desta_int(lst$typememb)
  lst$key <- paste(lst$number, lst$base_treaty, sep = "\r")
  if (anyDuplicated(lst$key)) stop("DESTA list of treaties: duplicate (number, base_treaty) keys")

  # Parties and roles from c1..c91 / mc1..mc91.
  ccols <- grep("^c[0-9]+$", names(lst), value = TRUE)
  ccols <- ccols[order(as.integer(sub("^c", "", ccols)))]
  mcols <- sub("^c", "mc", ccols)
  cmat <- vapply(ccols, function(cc) agr_desta_int(lst[[cc]]), integer(nrow(lst)))
  if (!is.matrix(cmat)) cmat <- matrix(cmat, nrow = nrow(lst))
  mmat <- vapply(mcols, function(cc) if (cc %in% names(lst)) agr_desta_int(lst[[cc]]) else rep(NA_integer_, nrow(lst)),
                 integer(nrow(lst)))
  if (!is.matrix(mmat)) mmat <- matrix(mmat, nrow = nrow(lst))
  iso_mat <- matrix(agr_desta_iso3(as.vector(cmat)), nrow = nrow(cmat))
  parties <- lapply(seq_len(nrow(lst)), function(i) sort(unique(iso_mat[i, !is.na(iso_mat[i, ])])))
  unmapped <- lapply(seq_len(nrow(lst)), function(i) sort(unique(cmat[i, !is.na(cmat[i, ]) & is.na(iso_mat[i, ])])))
  role_of <- function(i, role) sort(unique(iso_mat[i, !is.na(iso_mat[i, ]) & !is.na(mmat[i, ]) & mmat[i, ] == role]))

  live <- lst$entry_type != "negotiation"
  skipped <- list()
  skip <- function(i, reason, anchor = "") {
    skipped[[length(skipped) + 1L]] <<- data.frame(
      desta_number = lst$number[i], base_treaty = lst$base_treaty[i], entry_type = lst$entry_type[i],
      name = lst$name[i], reason = reason, anchor_record_id = anchor, stringsAsFactors = FALSE)
  }
  for (i in which(!live)) skip(i, "negotiation")

  # Parent of an entry: the base treaty or protocol whose number is the entry's base_treaty.
  parent_pool <- which(live & lst$entry_type %in% c("base_treaty", "protocol or amendment"))
  parent_of <- function(i) {
    cand <- parent_pool[lst$number[parent_pool] == lst$base_treaty[i] & parent_pool != i]
    if (length(cand) == 0) return(NA_integer_)
    if (length(cand) > 1) cand <- cand[order(lst$entry_type[cand] != "base_treaty")]
    cand[1]
  }
  parent <- rep(NA_integer_, nrow(lst))
  for (i in which(live & lst$entry_type != "base_treaty")) parent[i] <- parent_of(i)

  # Withdrawing countries (role 1; the dyadic withdrawal file as fallback).
  withdrawers <- vector("list", nrow(lst))
  wd <- if (is.null(withdrawals)) NULL else as.data.frame(withdrawals, stringsAsFactors = FALSE)
  for (i in which(live & lst$entry_type == "withdrawal")) {
    w <- role_of(i, 1L)
    if (length(w) == 0 && !is.null(wd) && nrow(wd) > 0 && all(c("number", "iso1") %in% names(wd))) {
      rows <- agr_chr(wd$number) == lst$number[i]
      if ("base_treaty" %in% names(wd)) rows <- rows & agr_chr(wd$base_treaty) == lst$base_treaty[i]
      w <- sort(unique(stats::na.omit(agr_desta_iso3(wd$iso1[rows]))))
    }
    withdrawers[[i]] <- w
  }

  # Acceding countries by DESTA role: 1 for a direct accession (typememb 5), 2 for an accession
  # through regional membership (typememb 6). The c-list of a typememb-6 entry holds only the
  # counterpart and the acceding countries, and the counterpart can be miscoded (Syria for Turkey
  # in 357+3 and 357+4), so the roles take precedence over the c-list.
  acc_role <- vector("list", nrow(lst))
  for (i in which(live & lst$entry_type == "accession")) {
    acc_role[[i]] <- role_of(i, if (isTRUE(lst$typememb_i[i] == 6L)) 2L else 1L)
  }
  acceding_of <- function(e, state) {
    diff <- setdiff(parties[[e]], state)
    by_role <- intersect(acc_role[[e]], diff)
    if (length(by_role)) by_role else if (length(acc_role[[e]]) && length(diff) == 0) setdiff(acc_role[[e]], state) else diff
  }

  # Parties of entry q at year y: its c-list, plus accessions filed against q, minus withdrawals
  # filed against q, up to y.
  events_against <- function(q, types) which(live & lst$entry_type %in% types & parent == q)
  state_at <- function(q, y) {
    s <- parties[[q]]
    ev <- events_against(q, c("accession", "withdrawal"))
    ev <- ev[!is.na(lst$year_i[ev]) & lst$year_i[ev] <= y]
    ev <- ev[order(lst$year_i[ev], lst$entry_type[ev] == "withdrawal", ev)]
    for (e in ev) {
      if (lst$entry_type[e] == "accession") s <- union(s, acceding_of(e, s)) else s <- setdiff(s, withdrawers[[e]])
    }
    sort(unique(s))
  }

  # Which protocols are records of their own.
  is_record <- rep(FALSE, nrow(lst))
  is_record[live & lst$entry_type == "base_treaty"] <- TRUE
  if (isTRUE(protocol_records)) {
    for (p in which(live & lst$entry_type == "protocol or amendment")) {
      q <- parent[p]
      if (length(parties[[p]]) < 2) next
      if (is.na(q)) { is_record[p] <- TRUE; next }
      ref <- state_at(q, if (is.na(lst$year_i[p])) as_of_year else lst$year_i[p])
      if (!setequal(parties[[p]], ref)) is_record[p] <- TRUE
    }
  }
  anchor_of <- function(i) {
    cur <- i
    seen <- integer()
    while (!is.na(cur) && !(cur %in% seen)) {
      if (is_record[cur]) return(cur)
      seen <- c(seen, cur)
      cur <- parent[cur]
    }
    NA_integer_
  }
  anchor <- rep(NA_integer_, nrow(lst))
  for (i in which(live)) {
    if (lst$entry_type[i] == "consolidated") {
      # "Xa" consolidates protocol (or treaty) X with its agreement.
      target <- which(live & lst$number == sub("a$", "", lst$number[i]) &
                        lst$entry_type %in% c("protocol or amendment", "base_treaty"))
      anchor[i] <- if (length(target)) anchor_of(target[1]) else anchor_of(parent[i])
    } else if (is_record[i]) {
      anchor[i] <- i
    } else {
      anchor[i] <- anchor_of(parent[i])
    }
  }

  # EU membership tables.
  eu_members <- sort(unique(agr_chr(eu_members)))
  eu_ever <- sort(unique(c(eu_members, names(AGR_DESTA_EU_EXIT_YEAR))))
  join <- setNames(rep(as_of_year, length(eu_ever)), eu_ever)
  known <- intersect(eu_ever, names(AGR_DESTA_EU_JOIN_YEAR))
  join[known] <- AGR_DESTA_EU_JOIN_YEAR[known]
  exit <- setNames(rep(NA_integer_, length(eu_ever)), eu_ever)
  ex <- intersect(eu_ever, names(AGR_DESTA_EU_EXIT_YEAR))
  exit[ex] <- AGR_DESTA_EU_EXIT_YEAR[ex]
  eu_at <- function(y) eu_ever[agr_desta_eu_member_at(eu_ever, y, eu_ever, join, exit)]
  # An EU agreement: named EC/EU, or two-sided with every EU member of its year among the parties.
  eu_agreement <- function(i) {
    y <- lst$year_i[i]
    if (is.na(y)) return(FALSE)
    members <- eu_at(y)
    if (length(members) == 0) return(FALSE)
    named <- grepl(AGR_DESTA_EU_NAME_PATTERN, lst$name[i], perl = TRUE)
    complete <- all(members %in% parties[[i]])
    has_other <- length(setdiff(parties[[i]], eu_ever)) > 0 ||
      any(!agr_desta_eu_member_at(parties[[i]], y, eu_ever, join, exit, tolerance = 1L))
    (named || (complete && isTRUE(lst$typememb_i[i] %in% c(3L, 4L)))) && has_other
  }
  # EU side of a party set in year y (members of that year; one year of tolerance for an
  # agreement signed in the year before an enlargement, as DESTA codes Greece in EC Yugoslavia 1980).
  eu_side <- function(p, y) p[agr_desta_eu_member_at(p, y, eu_ever, join, exit, tolerance = 1L)]

  # Coding entries: provisions and indices keyed by (number, base_treaty).
  prov <- if (is.null(provisions)) data.frame() else as.data.frame(provisions, stringsAsFactors = FALSE)
  idx <- if (is.null(indices)) data.frame() else as.data.frame(indices, stringsAsFactors = FALSE)
  prov_key <- if (nrow(prov)) paste(agr_chr(prov$number), agr_chr(prov$base_treaty), sep = "\r") else character()
  idx_key <- if (nrow(idx)) paste(agr_chr(idx$number), agr_chr(idx$base_treaty), sep = "\r") else character()
  prov_depth <- if (nrow(prov)) agr_desta_depth_components(prov) else NULL

  coding_for_anchor <- function(a) {
    cons <- which(live & lst$entry_type == "consolidated" & anchor == a)
    cons <- cons[order(-ifelse(is.na(lst$year_i[cons]), -Inf, lst$year_i[cons]), -cons)]
    cand <- c(cons, a)
    pick_p <- cand[lst$key[cand] %in% prov_key]
    if (length(pick_p)) {
      e <- pick_p[1]
      pr <- prov[match(lst$key[e], prov_key), , drop = FALSE]
      pd <- prov_depth[match(lst$key[e], prov_key), , drop = FALSE]
      ir <- if (lst$key[e] %in% idx_key) idx[match(lst$key[e], idx_key), , drop = FALSE] else NULL
      return(list(entry = e, prov = pr, prov_depth = pd, idx = ir, source = if (is.null(ir)) "recomputed" else "indices"))
    }
    pick_i <- cand[lst$key[cand] %in% idx_key]
    if (length(pick_i)) {
      e <- pick_i[1]
      return(list(entry = e, prov = NULL, prov_depth = NULL, idx = idx[match(lst$key[e], idx_key), , drop = FALSE], source = "indices"))
    }
    list(entry = NA_integer_, prov = NULL, prov_depth = NULL, idx = NULL, source = "none")
  }

  record_id <- function(i) {
    id <- paste0("desta:", lst$number[i])
    if (sum(lst$number[live & (is_record | lst$entry_type == "accession")] == lst$number[i]) > 1) {
      id <- paste0(id, "~", lst$base_treaty[i])
    }
    id
  }

  recs <- list()
  design <- list()
  code_rows <- list()
  n_eu <- 0L

  add_record <- function(i, a, p_final, acceding, withdrawn_note, status, status_raw, eu_collapsed, notes_extra, title) {
    cod <- coding_for_anchor(a)
    sid <- record_id(i)
    pr <- cod$prov
    ir <- cod$idx
    td <- if (!is.null(pr)) agr_desta_col(pr, "mar_typedepth") else NA_integer_
    tm <- lst$typememb_i[i]
    raw_type <- paste0(if (!is.na(td)) AGR_DESTA_TYPEDEPTH[as.character(td)] else "type not coded",
                       "; ", if (!is.na(tm) && as.character(tm) %in% names(AGR_DESTA_TYPEMEMB)) AGR_DESTA_TYPEMEMB[as.character(tm)] else "membership not coded")
    anchor_tm <- lst$typememb_i[a]
    epa <- grepl(AGR_DESTA_EPA_PATTERN, lst$name[a], perl = TRUE) || grepl(AGR_DESTA_EPA_PATTERN, lst$wto_name[a], perl = TRUE) ||
      ("JPN" %in% p_final && !("USA" %in% p_final) && isTRUE(anchor_tm %in% c(1L, 3L)))
    atype <- if (!is.na(td) && td == 5L) "framework" else if (epa) "epa" else "fta"
    comps <- if (!is.null(ir)) {
      vapply(AGR_DESTA_DEPTH_COMPONENTS, function(v) agr_desta_col(ir, v)[1], integer(1))
    } else if (!is.null(cod$prov_depth)) {
      vapply(AGR_DESTA_DEPTH_COMPONENTS, function(v) as.integer(cod$prov_depth[[v]][1]), integer(1))
    } else setNames(rep(NA_integer_, 7), AGR_DESTA_DEPTH_COMPONENTS)
    depth <- if (!is.null(ir)) agr_desta_col(ir, "depth_index")[1] else if (!is.null(cod$prov_depth)) as.integer(cod$prov_depth$depth_index[1]) else NA_integer_
    num_or_na <- function(df, v) if (!is.null(df) && v %in% names(df)) suppressWarnings(as.numeric(df[[v]][1])) else NA_real_
    flag_or_na <- function(vars) {
      if (is.null(pr)) return(NA_integer_)
      vals <- vapply(vars, function(v) agr_desta_col(pr, v)[1], integer(1))
      if (all(is.na(vals))) return(NA_integer_)
      as.integer(any(vals %in% 1L))
    }
    coding_note <- if (!is.na(cod$entry) && cod$entry != a) paste0("coding from DESTA consolidated entry ", lst$number[cod$entry])
                   else if (!is.na(cod$entry) && cod$entry != i) paste0("coding from DESTA entry ", lst$number[cod$entry])
                   else if (is.na(cod$entry)) "not content-coded by DESTA" else ""
    unm <- unmapped[[i]]
    notes <- c(withdrawn_note,
               if (eu_collapsed) "EU member states collapsed to EUU",
               if (lst$entry_type[i] == "protocol or amendment") paste0("DESTA files this as a protocol of ", lst$name[parent[i]], " (", lst$number[parent[i]], ") with its own parties"),
               if (length(unm)) paste0("unmapped DESTA country codes dropped: ", paste(unm, collapse = ", ")),
               coding_note, notes_extra)
    notes <- notes[!is.na(notes) & nzchar(notes)]
    wto <- if (nzchar(lst$wto_name[a])) paste0("; WTO RTA list: ", lst$wto_name[a]) else ""
    desc <- paste0("DESTA ", lst$entry_type[i], " ", lst$number[i],
                   if (lst$entry_type[i] != "base_treaty") paste0(" (agreement ", lst$number[a], ")") else "",
                   ": ", raw_type,
                   if (nzchar(lst$regioncon[i])) paste0("; region ", lst$regioncon[i]) else "",
                   if (!is.na(depth)) paste0("; depth index ", depth, "/7") else "", wto)
    pf <- agr_party_fields(list(p_final))
    recs[[length(recs) + 1L]] <<- data.frame(
      source_dataset = "desta", source_record_id = sid, title = title, pf,
      agreement_type = atype, agreement_type_raw = raw_type, partnership_label = "",
      legal_status = "binding", legal_status_basis = "source_document_type",
      announcement_date = "", signature_date = if (is.na(lst$year_i[i])) "" else as.character(lst$year_i[i]),
      effective_date = if (is.na(lst$eif_i[i])) "" else as.character(lst$eif_i[i]), expiry_date = "",
      relationship_category = "trade_economic",
      implementation_status = status, implementation_status_raw = status_raw,
      source_url = AGR_DESTA_SOURCE_URL, primary_document_url = "", primary_source_available = FALSE,
      last_verified = agr_chr(fetched_on)[1], source_last_updated = release,
      description = desc, notes = paste(notes, collapse = "; "),
      stringsAsFactors = FALSE)
    ins <- agr_desta_instruments(pr, if (is.null(pr)) ir else NULL)
    ins_snippet <- paste0(if (ins$basis == "source_coded") "DESTA content coding" else "DESTA list of treaties (not content-coded)",
                          if (!is.na(cod$entry)) paste0(", entry ", lst$number[cod$entry]) else "")
    code_rows[[length(code_rows) + 1L]] <<- data.frame(
      source_record_id = sid, dimension = c("sector", rep("instrument_types", length(ins$codes))),
      code = c("trade", ins$codes), basis = c("source_coded", rep(ins$basis, length(ins$codes))),
      n_hits = 1L, snippet = c("DESTA trade agreement", rep(ins_snippet, length(ins$codes))),
      stringsAsFactors = FALSE)
    design[[length(design) + 1L]] <<- data.frame(
      source_record_id = sid, desta_number = lst$number[i], base_treaty = lst$base_treaty[i],
      entry_type = lst$entry_type[i], agreement_number = lst$number[a], typememb = tm,
      acceding = paste(acceding, collapse = "|"),
      coding_entry = if (is.na(cod$entry)) "" else lst$number[cod$entry],
      depth_index = depth, depth_index_source = cod$source,
      depth_rasch = num_or_na(ir, "depth_rasch"),
      full_fta = comps[["full_fta"]], standards = comps[["standards"]], investments = comps[["investments"]],
      services = comps[["services"]], procurement = comps[["procurement"]], competition = comps[["competition"]],
      iprs = comps[["iprs"]],
      state_aid = flag_or_na("comp_state_aid"), subsidies = flag_or_na(c("comp_subsidies", "tr_sub_prov")),
      soe = flag_or_na("comp_soe"), investor_state = flag_or_na("inv_ctry_dsm"),
      enforce = num_or_na(ir, "enforce"), flexrigid = num_or_na(ir, "flexrigid"),
      entry_force_year = lst$eif_i[i],
      withdrawn = attr(withdrawn_note, "iso3") %||% "",
      eu_collapsed = eu_collapsed,
      stringsAsFactors = FALSE)
  }

  status_of <- function(i) {
    eif <- lst$eif_i[i]
    if (!is.na(eif) && eif <= as_of_year) list("in_force", paste0("in force since ", eif, " (DESTA entryforceyear)"))
    else if (!is.na(eif)) list("signed", paste0("signed ", lst$year_i[i], "; entry into force ", eif, " is after ", as_of_year))
    else list("signed", paste0("signed ", if (is.na(lst$year_i[i])) "(year unknown)" else lst$year_i[i],
                                "; no entry into force recorded by DESTA"))
  }
  wd_note <- function(ws) {
    if (length(ws) == 0) return(structure("", iso3 = ""))
    txt <- vapply(ws, function(w) paste0(w$iso3, " withdrew ", w$year), character(1))
    structure(paste(txt, collapse = "; "), iso3 = paste(sort(unique(vapply(ws, function(w) w$iso3, character(1)))), collapse = "|"))
  }

  for (a in which(live & is_record)) {
    ev <- events_against(a, c("accession", "withdrawal"))
    # Accessions and withdrawals filed against non-record protocols of this agreement.
    ev <- union(ev, which(live & lst$entry_type %in% c("accession", "withdrawal") & anchor == a))
    ev <- ev[order(ifelse(is.na(lst$year_i[ev]), as_of_year, lst$year_i[ev]), lst$entry_type[ev] == "withdrawal", ev)]
    wd_ev <- ev[lst$entry_type[ev] == "withdrawal"]
    w_list <- list()
    for (e in wd_ev) for (w in withdrawers[[e]]) w_list[[length(w_list) + 1L]] <- list(iso3 = w, year = lst$year_i[e], idx = e)
    eu_ag <- eu_agreement(a)
    base_parties <- parties[[a]]
    title_a <- lst$name[a]

    # The agreement's own record.
    removed <- Filter(function(w) w$iso3 %in% base_parties, w_list)
    p <- setdiff(base_parties, vapply(removed, function(w) w$iso3, character(1)))
    st <- status_of(a)
    if (length(p) < 2) {
      p <- base_parties
      st <- list("inactive", paste0("inactive: fewer than two parties remain after withdrawals (", wd_note(removed), "); ", st[[2]]))
    }
    collapsed <- FALSE
    if (eu_ag) {
      side <- eu_side(p, lst$year_i[a])
      if (length(side) && length(setdiff(p, side))) { p <- sort(c(setdiff(p, side), "EUU")); collapsed <- TRUE }
    }
    if (length(base_parties) < 2) {
      skip(a, "fewer_than_two_parties")
    } else {
      add_record(a, a, p, character(), wd_note(removed), st[[1]], st[[2]], collapsed, "", title_a)
      if (collapsed) n_eu <- n_eu + 1L
    }

    # Accession records.
    state <- base_parties
    for (k in seq_along(ev)) {
      e <- ev[k]
      if (lst$entry_type[e] == "withdrawal") { state <- setdiff(state, withdrawers[[e]]); next }
      acc <- acceding_of(e, state)
      if (length(acc) == 0) { skip(e, "accession_adds_no_party", record_id(a)); next }
      stray <- setdiff(setdiff(parties[[e]], state), acc)
      stray_note <- if (length(stray)) paste0("DESTA's entry also lists ", paste(stray, collapse = ", "),
                                              ", not a party of the agreement (ignored)") else ""
      state <- sort(unique(c(state, acc)))
      if (eu_ag && all(acc %in% eu_ever)) { skip(e, "eu_enlargement_absorbed_by_EUU", record_id(a)); next }
      later <- Filter(function(w) w$iso3 %in% state & (is.na(w$year) | is.na(lst$year_i[e]) | w$year >= lst$year_i[e]) & w$idx %in% ev[seq_along(ev) > k], w_list)
      later_iso <- vapply(later, function(w) w$iso3, character(1))
      st <- status_of(e)
      p <- state
      if (length(acc) && all(acc %in% later_iso)) {
        st <- list("inactive", paste0("inactive: the acceding countries withdrew (", wd_note(Filter(function(w) w$iso3 %in% acc, later)), "); ", st[[2]]))
      } else {
        p <- setdiff(state, later_iso)
        if (length(p) < 2) { p <- state; st <- list("inactive", paste0("inactive: fewer than two parties remain after withdrawals; ", st[[2]])) }
      }
      collapsed <- FALSE
      if (eu_ag) {
        side <- eu_side(p, lst$year_i[e])
        if (length(side) && length(setdiff(p, side))) { p <- sort(c(setdiff(p, side), "EUU")); collapsed <- TRUE }
      }
      title <- paste0(title_a, " (accession of ", paste(agr_desta_country_names(acc), collapse = ", "), ")")
      add_record(e, a, p, acc, wd_note(Filter(function(w) w$iso3 %in% state, later)), st[[1]], st[[2]], collapsed, stray_note, title)
      if (collapsed) n_eu <- n_eu + 1L
    }
  }
  for (i in which(live & lst$entry_type == "protocol or amendment" & !is_record)) skip(i, "protocol_amends_parent", if (!is.na(anchor[i])) record_id(anchor[i]) else "")
  for (i in which(live & lst$entry_type == "consolidated")) skip(i, "consolidated_coding_only", if (!is.na(anchor[i])) record_id(anchor[i]) else "")
  for (i in which(live & lst$entry_type == "withdrawal")) skip(i, "withdrawal_applied", if (!is.na(anchor[i])) record_id(anchor[i]) else "")
  for (i in which(live & lst$entry_type == "accession" & is.na(anchor))) skip(i, "accession_without_agreement")

  records <- if (length(recs)) agr_as_records(do.call(rbind, recs)) else agr_empty_records()
  codes <- if (length(code_rows)) agr_as_codes(do.call(rbind, code_rows)) else agr_empty_codes()
  codes <- agr_derive_tech_codes(codes, cfg)
  records <- agr_apply_codes(records, codes)
  design_df <- if (length(design)) do.call(rbind, design) else data.frame()
  rownames(design_df) <- NULL
  skipped_df <- if (length(skipped)) do.call(rbind, skipped) else data.frame()
  rownames(skipped_df) <- NULL
  depth_check <- agr_desta_check_depth(if (nrow(prov)) prov else NULL, if (nrow(idx)) idx else NULL)
  list(records = records, codes = codes,
       extras = list(desta_design = design_df, desta_skipped = skipped_df,
                     desta_summary = list(n_entries = nrow(lst), n_negotiation = sum(!live),
                                          n_records = nrow(records), n_eu_collapsed = n_eu,
                                          depth_compared = depth_check$n_compared,
                                          depth_mismatches = depth_check$mismatches,
                                          release = release, as_of_year = as_of_year)))
}
