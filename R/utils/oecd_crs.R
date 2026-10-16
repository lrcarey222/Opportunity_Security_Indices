# OECD Creditor Reporting System (CRS): pure helpers behind data/raw/oecd_crs_purpose.csv, the
# bilateral official-finance extract the Development Potential index reads by purpose code
# (fetcher: scripts/fetchers/oecd_crs.R; consumer: partnership_strength_build_oecd_tech() in
# R/themes/partnership_strength/stronger_development.R).
#
# Source: the CRS bulk file the OECD publishes with the DF_CRS dataflow (CRS-reduced.parquet, one
# row per reported activity and year, numeric DAC codes) and the DAC-CRS code lists
# (DAC-CRS-CODES.xlsx), which map donor and recipient codes to ISO3. The SDMX data endpoint of
# DF_CRS returned HTTP 500 for every query on 2026-10-16, and its sector dimension is the purpose
# code hierarchy (3-digit totals next to their 5-digit children), so the bulk file is the source.
#
# Scope of the extract:
#   - purpose codes of Energy (23xxx), Industry (321xx) and Mineral resources and mining (322xx);
#   - official flows: ODA (category 10), non-export-credit Other Official Flows (21) and private
#     sector instruments (60). Officially supported export credits (22) and private flows (30, 36,
#     37) are left out;
#   - bilateral flows of country donors (DAC members and the non-DAC countries that report): the
#     donor's Data Explorer code is an ISO3 code. Multilateral agencies, EU institutions and
#     private foundations have none, and multilateral inflows and outflows (bi_multi 2, 4) and
#     private outflows (6) are dropped;
#   - recipient countries only: regional and unspecified recipients carry no ISO3 code;
#   - complete years only (oecd_crs_complete_years()): the bulk file carries the first reports of
#     the year after the last complete one (2025: 12,448 activities against 370,248 for 2024 in
#     the October 2026 file).
# Amounts are USD millions at constant prices (the OECD DAC deflator of each donor), summed over
# the activities of each (year, donor, recipient, purpose code, measure).

# CRS category code -> measure.
OECD_CRS_MEASURES <- c("10" = "ODA", "21" = "OOF", "60" = "PSI")

# Bi_Multi codes that are not a country donor's own bilateral flow.
OECD_CRS_DROP_BI_MULTI <- c(2L, 4L, 6L)

# Purpose codes of the extract: Energy (230-236 and children), Industry (321) and Mineral resources
# and mining (322).
oecd_crs_in_scope_purpose <- function(purpose_code) {
  code <- suppressWarnings(as.integer(purpose_code))
  !is.na(code) & ((code >= 23000L & code < 24000L) | (code >= 32100L & code < 32300L))
}

# The DAC-CRS code list's "Donor" sheet, read without column names: side-by-side blocks (DAC
# members, multilateral donors, non-DAC donors, private donors), each a header row with
# "DonorCode" / "Donor code", then "Data Explorer code" and the English name. Returns the country
# donors (Data Explorer code of three capital letters): donor_code, donor_iso3, donor.
oecd_crs_donor_codes <- function(sheet) {
  cells <- as.data.frame(sheet, stringsAsFactors = FALSE)
  cells[] <- lapply(cells, function(col) trimws(as.character(col)))
  hits <- which(vapply(cells, function(col) any(grepl("^donor ?code$", tolower(col))), logical(1)))
  if (!length(hits)) stop("DAC-CRS code list: no 'DonorCode' header in the Donor sheet.")
  blocks <- lapply(hits, function(j) {
    header_row <- which(grepl("^donor ?code$", tolower(cells[[j]])))[1]
    if (j + 2 > ncol(cells) || !grepl("data explorer code", tolower(cells[[j + 1]][header_row]))) {
      stop("DAC-CRS code list: the Donor sheet block at column ", j, " has no 'Data Explorer code' column next to its code.")
    }
    rows <- seq.int(header_row + 1L, nrow(cells))
    tibble::tibble(
      donor_code = suppressWarnings(as.integer(cells[[j]][rows])),
      donor_iso3 = cells[[j + 1]][rows],
      donor = cells[[j + 2]][rows]
    )
  })
  out <- dplyr::bind_rows(blocks)
  out <- out[!is.na(out$donor_code) & !is.na(out$donor_iso3) & grepl("^[A-Z]{3}$", out$donor_iso3), , drop = FALSE]
  if (anyDuplicated(out$donor_code)) {
    stop("DAC-CRS code list: duplicate donor codes ", paste(unique(out$donor_code[duplicated(out$donor_code)]), collapse = ", "))
  }
  out
}

# The DAC-CRS code list's "Recipient" sheet (read with column names). Returns the recipient
# countries (ISOcode of three capital letters): recipient_code, recipient_iso3, recipient.
oecd_crs_recipient_codes <- function(sheet) {
  nm <- tolower(gsub("[^a-z]", "", tolower(names(sheet))))
  pick <- function(pattern, label) {
    j <- which(grepl(pattern, nm))[1]
    if (is.na(j)) stop("DAC-CRS code list: the Recipient sheet has no ", label, " column.")
    trimws(as.character(sheet[[j]]))
  }
  out <- tibble::tibble(
    recipient_code = suppressWarnings(as.integer(pick("^recipientcode$", "'Recipient code'"))),
    recipient_iso3 = pick("^isocode$", "'ISOcode'"),
    recipient = pick("^recipientnameen$", "'Recipient name (EN)'")
  )
  out <- out[!is.na(out$recipient_code) & !is.na(out$recipient_iso3) & grepl("^[A-Z]{3}$", out$recipient_iso3), , drop = FALSE]
  if (anyDuplicated(out$recipient_code)) {
    stop("DAC-CRS code list: duplicate recipient codes ", paste(unique(out$recipient_code[duplicated(out$recipient_code)]), collapse = ", "))
  }
  out
}

# Years the bulk file covers in full, from its activity count per year (every purpose and flow):
# a year is complete when it has at least `min_share` of the median count of the three years
# before it. The first years (no three predecessors) count as complete.
oecd_crs_complete_years <- function(year, n, min_share = 0.5) {
  ord <- order(year)
  year <- as.integer(year[ord])
  n <- as.numeric(n[ord])
  ok <- vapply(seq_along(year), function(i) {
    prior <- n[year < year[i] & year >= year[i] - 3L]
    length(prior) < 3L || n[i] >= min_share * stats::median(prior)
  }, logical(1))
  year[ok]
}

# The price base year of the deflated amounts: the year in which the deflated and the current
# amounts agree (the DAC deflator is 100 there for every donor) for at least `min_share` of the
# rows with a positive current amount. NA when no year qualifies.
oecd_crs_price_base_year <- function(year, nominal, deflated, min_share = 0.95, tol = 1e-6) {
  pos <- !is.na(nominal) & !is.na(deflated) & nominal > 0
  if (!any(pos)) return(NA_integer_)
  same <- abs(deflated[pos] / nominal[pos] - 1) < tol
  share <- tapply(same, year[pos], mean)
  base <- as.integer(names(share)[share >= min_share])
  if (length(base) == 1L) base else NA_integer_
}

# The extract from CRS activity rows: `activities` has year, donor_code, recipient_code, category,
# bi_multi, purpose_code, usd_commitment_defl and usd_disbursement_defl; `donors` and `recipients`
# are oecd_crs_donor_codes() and oecd_crs_recipient_codes(). Keeps the scope above for the
# `years` given and sums each (year, donor, recipient, purpose code, measure). Returns year,
# donor_iso3, donor, recipient_iso3, recipient, purpose_code, measure, usd_commitment,
# usd_disbursement (USD millions, constant prices).
oecd_crs_aggregate <- function(activities, donors, recipients, years) {
  need <- c("year", "donor_code", "recipient_code", "category", "bi_multi", "purpose_code",
            "usd_commitment_defl", "usd_disbursement_defl")
  missing <- setdiff(need, names(activities))
  if (length(missing)) stop("CRS activities missing column(s): ", paste(missing, collapse = ", "))

  a <- tibble::as_tibble(activities[need])
  a <- a[a$year %in% years &
           as.character(a$category) %in% names(OECD_CRS_MEASURES) &
           !a$bi_multi %in% OECD_CRS_DROP_BI_MULTI &
           oecd_crs_in_scope_purpose(a$purpose_code), , drop = FALSE]
  a$measure <- unname(OECD_CRS_MEASURES[as.character(a$category)])

  a <- dplyr::inner_join(a, donors, by = "donor_code")
  a <- dplyr::inner_join(a, recipients, by = "recipient_code")
  a$purpose_code <- as.integer(a$purpose_code)
  a$year <- as.integer(a$year)
  out <- dplyr::summarise(
    dplyr::group_by(a, year, donor_iso3, donor, recipient_iso3, recipient, purpose_code, measure),
    usd_commitment = sum(usd_commitment_defl, na.rm = TRUE),
    usd_disbursement = sum(usd_disbursement_defl, na.rm = TRUE),
    .groups = "drop"
  )
  dplyr::arrange(out, year, donor_iso3, recipient_iso3, purpose_code, measure)
}
