# Oil Change International, Public Finance for Energy Database: oci_energy_finance.csv.
#
# Why: the export credit agencies and several G20 development lenders (JBIC, NEXI, JOGMEC, US EXIM,
# Hermes, KfW IPEX-Bank, UKEF, Bpifrance, SACE, EDC, KDB, ...) report no activities to the OECD CRS,
# so their energy finance was missing from Development Potential. R/utils/oci_energy_finance.R has
# the cleaning rules.
#
# Source: https://energyfinance.org/dataset.csv, the transaction-level file behind the database's
# "download the dataset" link (about 20 MB, one row per transaction). Public, no key; licence
# CC BY-NC 4.0 (attribution: Oil Change International, Public Finance for Energy Database).
#
# The fetch downloads the file into a new, empty temporary directory, reads it as data only and
# removes the directory afterwards. It writes the international transactions, one row per
# (transaction, recipient country):
#   fy, date, institution, institution_abbr, institution_group, institution_kind, mechanism,
#   recipient, recipient_iso3, recipient_share, category, sector, stage, amount_usd, project,
#   source, fetched_on
# (amount_usd in current US dollars, the full transaction amount; recipient_share splits a
# multi-country transaction equally).
#
# Consumers: scripts/15_build_partner_themes.R (Development Potential) and
# scripts/87_export_sector_data.R (its parity check). scripts/05_ingest_sources.R refreshes it when
# the copy is older than the annual cadence allows (manifest id oci_energy_finance, fetch_policy
# prefer). To refetch:
#   Rscript scripts/fetchers/oci_energy_finance.R

OCI_ENERGY_FINANCE_URL <- "https://energyfinance.org/dataset.csv"

# Run as a script, the fetcher framework (register_fetcher(), ...) is not loaded yet; scripts/05
# and the tests source scripts/utils/fetchers.R before this file.
if (sys.nframe() == 0L && !exists("register_fetcher", mode = "function")) {
  local({
    script <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1])
    root <- normalizePath(file.path(dirname(script), "..", ".."), winslash = "/", mustWork = TRUE)
    options(opportunity_security.repo_root = root)
    source(file.path(root, "scripts", "utils", "fetchers.R"), local = FALSE)
  })
}

# Repo root for the pure helpers: the option the scripts set, else the `repo_root` scripts/05
# defines, else the directory holding R/utils/oci_energy_finance.R above the working directory.
oci_energy_finance_repo_root <- function() {
  for (root in list(getOption("opportunity_security.repo_root"), get0("repo_root", envir = globalenv(), inherits = FALSE))) {
    if (is.character(root) && length(root) == 1 && nzchar(root) && file.exists(file.path(root, "R", "utils", "oci_energy_finance.R"))) {
      return(root)
    }
  }
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (dirname(d) != d) {
    if (file.exists(file.path(d, "R", "utils", "oci_energy_finance.R"))) return(d)
    d <- dirname(d)
  }
  stop("Cannot find R/utils/oci_energy_finance.R above ", getwd(), "; set options(opportunity_security.repo_root).")
}

oci_energy_finance_fetch <- function(url = OCI_ENERGY_FINANCE_URL, work_dir = tempfile("oci_energy_finance_")) {
  if (!exists("oci_energy_finance_clean", mode = "function")) {
    source(file.path(oci_energy_finance_repo_root(), "R", "utils", "oci_energy_finance.R"), local = FALSE)
  }
  if (!requireNamespace("countrycode", quietly = TRUE)) stop("Package 'countrycode' is required for the OCI fetcher.")
  # Checked before the cleanup is registered: an existing directory is someone else's.
  if (dir.exists(work_dir)) stop("OCI download directory already exists: ", work_dir)
  dir.create(work_dir, recursive = TRUE)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  path <- file.path(work_dir, "dataset.csv")
  status <- utils::download.file(url, path, method = "libcurl", mode = "wb", quiet = TRUE,
                                 headers = c("User-Agent" = "OSI-pipeline/1.0 (+https://github.com/lrcarey222/Opportunity_Security_Indices)"))
  if (!identical(as.integer(status), 0L) || !file.exists(path) || file.size(path) == 0) stop("OCI download failed: ", url)

  raw <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8")
  raw <- raw[, nzchar(names(raw)) & !is.na(names(raw)), drop = FALSE]
  out <- oci_energy_finance_clean(raw)
  message(sprintf("  oci_energy_finance: %d international transactions -> %d (transaction, country) rows; FY %d-%d",
                  sum(tolower(raw[["International or Domestic"]]) == "international", na.rm = TRUE), nrow(out),
                  min(out$fy, na.rm = TRUE), max(out$fy, na.rm = TRUE)))
  out$fetched_on <- format(Sys.Date())
  as.data.frame(out)
}

register_fetcher(
  id = "oci_energy_finance",
  description = "Oil Change International Public Finance for Energy Database: international energy transactions of G20 public finance institutions",
  contract = list(
    required_columns = c("fy", "institution_abbr", "institution_group", "institution_kind", "mechanism",
                         "recipient_iso3", "recipient_share", "sector", "stage", "amount_usd"),
    # About 11,000 international transactions in the October 2026 file; far fewer is a broken download.
    min_rows = 5000
  ),
  fn = function() oci_energy_finance_fetch()
)

# Standalone: Rscript scripts/fetchers/oci_energy_finance.R refetches and replaces the staged file.
if (sys.nframe() == 0L) {
  local({
    root <- oci_energy_finance_repo_root()
    config_path <- file.path(root, "config", "config.yml")
    config <- if (file.exists(config_path)) yaml::read_yaml(config_path) else list()
    raw_dir <- file.path(root, if (is.null(config$raw_data_dir)) "data/raw" else config$raw_data_dir)
    fetcher <- get_fetcher("oci_energy_finance")
    dest <- file.path(raw_dir, "oci_energy_finance.csv")
    df <- fetcher$fn()
    opsi_write_validated(df, dest, fetcher$contract, fetcher$id)
    message("Wrote ", nrow(df), " rows (", length(unique(df$recipient_iso3)), " recipients, ",
            length(unique(df$institution_abbr)), " institutions) to ", dest)
  })
}
