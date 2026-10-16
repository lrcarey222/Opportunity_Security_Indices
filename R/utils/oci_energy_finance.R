# Oil Change International, Public Finance for Energy Database (energyfinance.org): pure helpers
# behind data/raw/oci_energy_finance.csv (fetcher: scripts/fetchers/oci_energy_finance.R;
# consumer: partnership_strength_oci_amounts() in R/themes/partnership_strength/stronger_development.R).
#
# Why: the OECD CRS has no activity-level data from the export credit agencies (no officially
# supported export credits at all) or from several G20 development lenders. Japan reports no OOF
# activities, so JBIC is absent; so are NEXI, JOGMEC, US EXIM, Hermes, KfW IPEX-Bank, UKEF, Bpifrance,
# SACE and EDC. OCI tracks their energy transactions project by project (G20 export credit agencies,
# development finance institutions, national development banks and the major MDBs).
#
# The database publishes one row per transaction: institution (name, abbreviation, country group,
# kind), mechanism (loan, guarantee, equity, grant, insurance, ...), the recipient country (a name,
# or several separated by commas), category / sector / value-chain stage, the amount in US dollars
# at the transaction's exchange rate (current prices) and the institution's fiscal year.
# Licence: Creative Commons Attribution-NonCommercial 4.0 International (attribution to Oil Change
# International, Public Finance for Energy Database).

# The database's columns this pipeline keeps, by its header names.
OCI_ENERGY_FINANCE_COLUMNS <- c(
  date = "date", fy = "FY", amount_usd = "amountUSD", mechanism = "mechanism",
  institution = "institution", institution_abbr = "institutionAbbr", institution_group = "institutionGroup",
  institution_kind = "institutionKind", project = "project", recipient = "country", category = "category",
  sector = "sector", stage = "stage", scope = "International or Domestic", source = "source"
)

oci_squish <- function(x) gsub("\\s+", " ", trimws(as.character(x)))

# ISO3 codes of a recipient string. One country name gives one code; a list ("Kenya,Namibia,
# Seychelles") gives one code per name when every name is a country. A list with a regional
# label in it ("Kenya,Southern Africa") gives none: the deal's share in the named country is
# unknown, and matching the whole string would hand all of it to the first country named.
# Returns a character vector (empty when nothing matches).
oci_recipient_iso3 <- function(recipient) {
  one <- function(name) {
    suppressWarnings(countrycode::countrycode(name, "country.name", "iso3c", warn = FALSE,
                                              custom_match = c(Kosovo = "XKX", Micronesia = "FSM")))
  }
  r <- oci_squish(recipient)
  if (!nzchar(r)) return(character())
  parts <- oci_squish(strsplit(r, ",", fixed = TRUE)[[1]])
  parts <- parts[nzchar(parts)]
  codes <- vapply(parts, one, character(1))
  if (anyNA(codes)) character() else unique(unname(codes))
}

# The tidy extract from the database's CSV as read (read.csv(check.names = FALSE)): international
# transactions only, one row per (transaction, recipient country) with the amount split equally
# across the countries of a multi-country transaction. Rows whose recipient matches no country are
# dropped. KfW rows whose institution is KfW IPEX-Bank get the abbreviation "KfW-IPEX" (the
# database files some of them under "KfW"). Returns fy, date, institution, institution_abbr,
# institution_group, institution_kind, mechanism, recipient, recipient_iso3, recipient_share,
# category, sector, stage, amount_usd, project, source.
oci_energy_finance_clean <- function(raw) {
  missing <- setdiff(OCI_ENERGY_FINANCE_COLUMNS, names(raw))
  if (length(missing)) stop("OCI Public Finance for Energy data missing column(s): ", paste(missing, collapse = ", "))
  x <- raw[OCI_ENERGY_FINANCE_COLUMNS]
  names(x) <- names(OCI_ENERGY_FINANCE_COLUMNS)
  x[] <- lapply(x, oci_squish)
  x <- x[tolower(x$scope) == "international", setdiff(names(x), "scope"), drop = FALSE]
  x$fy <- suppressWarnings(as.integer(x$fy))
  x$amount_usd <- suppressWarnings(as.numeric(gsub("[$,]", "", x$amount_usd)))
  ipex <- grepl("ipex", x$institution, ignore.case = TRUE)
  x$institution_abbr[ipex] <- "KfW-IPEX"

  names_u <- unique(x$recipient)
  iso_u <- lapply(names_u, oci_recipient_iso3)
  iso <- iso_u[match(x$recipient, names_u)]
  n <- lengths(iso)
  out <- x[rep(seq_len(nrow(x)), n), , drop = FALSE]
  out$recipient_iso3 <- unlist(iso, use.names = FALSE)
  out$recipient_share <- rep(1 / n[n > 0], n[n > 0])
  rownames(out) <- NULL
  out[c("fy", "date", "institution", "institution_abbr", "institution_group", "institution_kind", "mechanism",
        "recipient", "recipient_iso3", "recipient_share", "category", "sector", "stage", "amount_usd", "project", "source")]
}
