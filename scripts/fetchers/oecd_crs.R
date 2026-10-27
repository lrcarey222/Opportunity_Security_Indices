# OECD Creditor Reporting System, bilateral official finance by purpose code: oecd_crs_purpose.csv.
#
# Why: the Development Potential index scores the aid a partner receives per technology and stage.
# The staged SDMX extract (oecd_crs_api.csv, DF_CRS 1.4) has only the 3-digit sectors (Energy,
# Industry, Mineral resources and mining), ODA only, and 2024 as a 108-row stub; the 5-digit
# purpose codes (solar, wind, grids, nuclear, non-ferrous metals, ...) are what tell technologies
# apart. R/utils/oecd_crs.R has the scope rules.
#
# Source: OECD, Creditor Reporting System, the bulk file published with the DF_CRS dataflow
# (https://webfs-dcd.oecd.org/files/dotStat/DSD_CRS/CRS-reduced.parquet, about 290 MB, one row per
# activity and year; the dataflow's annotations on sdmx.oecd.org link it), and the DAC-CRS code
# lists (https://webfs.oecd.org/oda/DataCollection/Resources/DAC-CRS-CODES.xlsx) for the donor
# and recipient ISO3 codes. Public, no API key; OECD data are CC BY 4.0.
#
# The fetch downloads both files into a new, empty temporary directory, reads them as data only
# (nothing in the download is run or sourced) and removes the directory afterwards. The parquet is
# read with arrow, filtered to the in-scope purpose codes before it is collected (about 190,000
# activity rows), and written as
#   year, donor_iso3, donor, recipient_iso3, recipient, purpose_code, measure, usd_commitment,
#   usd_disbursement, price_base_year, source_updated, fetched_on
# (USD millions at constant prices of price_base_year; source_updated is the bulk file's
# Last-Modified date).
#
# Consumers: scripts/15_build_partner_themes.R (Development Potential) and
# scripts/87_export_sector_data.R (its parity check of the stored components).
# scripts/05_ingest_sources.R refreshes it when the copy is older than the annual cadence allows
# (manifest id oecd_crs_purpose, fetch_policy prefer). Needs the arrow and readxl packages. To
# refetch:
#   Rscript scripts/fetchers/oecd_crs.R

OECD_CRS_BULK_URL <- "https://webfs-dcd.oecd.org/files/dotStat/DSD_CRS/CRS-reduced.parquet"
OECD_CRS_CODES_URL <- "https://webfs.oecd.org/oda/DataCollection/Resources/DAC-CRS-CODES.xlsx"
OECD_CRS_MIN_YEAR <- 2013L

# Run as a script, the fetcher framework (register_fetcher(), opsi_http_get(), ...) is not loaded
# yet; scripts/05 and the tests source scripts/utils/fetchers.R before this file.
if (sys.nframe() == 0L && !exists("register_fetcher", mode = "function")) {
  local({
    script <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1])
    root <- normalizePath(file.path(dirname(script), "..", ".."), winslash = "/", mustWork = TRUE)
    options(opportunity_security.repo_root = root)
    source(file.path(root, "scripts", "utils", "fetchers.R"), local = FALSE)
  })
}

# Repo root for the pure helpers: the option the scripts set, else the `repo_root` scripts/05
# defines, else the directory holding R/utils/oecd_crs.R above the working directory.
oecd_crs_repo_root <- function() {
  for (root in list(getOption("opportunity_security.repo_root"), get0("repo_root", envir = globalenv(), inherits = FALSE))) {
    if (is.character(root) && length(root) == 1 && nzchar(root) && file.exists(file.path(root, "R", "utils", "oecd_crs.R"))) {
      return(root)
    }
  }
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (dirname(d) != d) {
    if (file.exists(file.path(d, "R", "utils", "oecd_crs.R"))) return(d)
    d <- dirname(d)
  }
  stop("Cannot find R/utils/oecd_crs.R above ", getwd(), "; set options(opportunity_security.repo_root).")
}

oecd_crs_load_helpers <- function(root = oecd_crs_repo_root()) {
  source(file.path(root, "R", "utils", "oecd_crs.R"), local = FALSE)
  invisible(root)
}

# Streams `url` to `dest` (the bulk file is too large to hold in memory twice) and returns the
# server's Last-Modified date ("YYYY-MM-DD"; NA when absent). Base R's libcurl is used, not the
# curl package (opsi_http_get()): on 2026-10-16 webfs-dcd.oecd.org answered the curl package's
# libcurl 8.3.0 with a Cloudflare challenge (HTTP 403, "Just a moment...") whatever the headers,
# while download.file(method = "libcurl") and Git's curl got the file.
oecd_crs_download <- function(url, dest, retries = 3, timeout_seconds = 1800) {
  old_timeout <- options(timeout = max(timeout_seconds, getOption("timeout")))
  on.exit(options(old_timeout), add = TRUE)
  headers <- c("User-Agent" = "OSI-pipeline/1.0 (+https://github.com/lrcarey222/Opportunity_Security_Indices)")
  last_error <- NULL
  for (attempt in seq_len(retries)) {
    ok <- tryCatch({
      status <- utils::download.file(url, dest, method = "libcurl", mode = "wb", quiet = TRUE, headers = headers)
      if (!identical(as.integer(status), 0L) || !file.exists(dest) || file.size(dest) == 0) stop("download.file() status ", status)
      TRUE
    }, error = function(e) {
      last_error <<- conditionMessage(e)
      FALSE
    }, warning = function(w) {
      last_error <<- conditionMessage(w)
      FALSE
    })
    if (ok) {
      head <- tryCatch(curlGetHeaders(url), error = function(e) character())
      modified <- sub("^[^:]+:\\s*", "", grep("^last-modified:", head, ignore.case = TRUE, value = TRUE)[1])
      parsed <- if (is.na(modified)) NA else as.Date(strptime(trimws(modified), "%a, %d %b %Y %H:%M:%S", tz = "GMT"))
      return(if (is.na(parsed)) NA_character_ else format(parsed))
    }
    if (attempt < retries) Sys.sleep(2 * attempt)
  }
  stop("Download of ", url, " failed after ", retries, " attempts: ", last_error)
}

oecd_crs_fetch <- function(bulk_url = OECD_CRS_BULK_URL,
                           codes_url = OECD_CRS_CODES_URL,
                           min_year = OECD_CRS_MIN_YEAR,
                           work_dir = tempfile("oecd_crs_")) {
  if (!exists("oecd_crs_aggregate", mode = "function")) oecd_crs_load_helpers()
  for (pkg in c("arrow", "readxl", "dplyr")) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop("Package '", pkg, "' is required for the OECD CRS fetcher.")
  }
  # Checked before the cleanup is registered: an existing directory is someone else's.
  if (dir.exists(work_dir)) stop("OECD CRS download directory already exists: ", work_dir)
  dir.create(work_dir, recursive = TRUE)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  parquet_path <- file.path(work_dir, "CRS-reduced.parquet")
  codes_path <- file.path(work_dir, "DAC-CRS-CODES.xlsx")
  updated <- oecd_crs_download(bulk_url, parquet_path)
  oecd_crs_download(codes_url, codes_path)

  `%>%` <- dplyr::`%>%`
  ds <- arrow::open_dataset(parquet_path)
  counts <- ds %>% dplyr::count(year) %>% dplyr::collect()
  complete <- oecd_crs_complete_years(counts$year, counts$n)
  years <- complete[complete >= min_year]
  stub <- setdiff(counts$year[counts$year >= min_year], years)
  if (length(stub)) {
    message("  oecd_crs_purpose: leaving out incomplete year(s) ", paste(stub, collapse = ", "),
            " (", paste(counts$n[counts$year %in% stub], collapse = ", "), " activities)")
  }

  activities <- ds %>%
    dplyr::filter(
      year >= min_year,
      (purpose_code >= 23000L & purpose_code < 24000L) | (purpose_code >= 32100L & purpose_code < 32300L)
    ) %>%
    dplyr::select(year, donor_code, recipient_code, category, bi_multi, purpose_code,
                  usd_commitment, usd_disbursement, usd_commitment_defl, usd_disbursement_defl) %>%
    dplyr::collect()

  base_year <- oecd_crs_price_base_year(activities$year, activities$usd_disbursement, activities$usd_disbursement_defl)
  if (is.na(base_year)) stop("OECD CRS: no year in which deflated and current amounts agree; cannot name the price base.")

  donors <- oecd_crs_donor_codes(readxl::read_excel(codes_path, sheet = "Donor", col_names = FALSE, .name_repair = "minimal"))
  recipients <- oecd_crs_recipient_codes(readxl::read_excel(codes_path, sheet = "Recipient", .name_repair = "minimal"))

  out <- oecd_crs_aggregate(activities, donors, recipients, years)

  # What the scope leaves out of the official in-scope flows of the kept years, for the log.
  official <- activities[activities$year %in% years & as.character(activities$category) %in% names(OECD_CRS_MEASURES), , drop = FALSE]
  kept_share <- sum(out$usd_disbursement) / sum(official$usd_disbursement_defl, na.rm = TRUE)
  message(sprintf("  oecd_crs_purpose: %d-%d, price base %d; bilateral country-to-country rows keep %.0f%% of official in-scope disbursements",
                  min(years), max(years), base_year, 100 * kept_share))

  out$price_base_year <- base_year
  out$source_updated <- updated
  out$fetched_on <- format(Sys.Date())
  as.data.frame(out)
}

register_fetcher(
  id = "oecd_crs_purpose",
  description = "OECD CRS bulk file: bilateral ODA, OOF and PSI by donor, recipient, purpose code and year",
  contract = list(
    required_columns = c("year", "donor_iso3", "recipient_iso3", "purpose_code", "measure",
                         "usd_commitment", "usd_disbursement", "price_base_year"),
    # 41,448 rows for 2013-2024 in the October 2026 file; far fewer is a broken download.
    min_rows = 20000,
    unique_key = c("year", "donor_iso3", "recipient_iso3", "purpose_code", "measure")
  ),
  fn = function() oecd_crs_fetch()
)

# Standalone: Rscript scripts/fetchers/oecd_crs.R refetches and replaces the staged file.
if (sys.nframe() == 0L) {
  local({
    root <- oecd_crs_repo_root()
    config_path <- file.path(root, "config", "config.yml")
    config <- if (file.exists(config_path)) yaml::read_yaml(config_path) else list()
    raw_dir <- file.path(root, if (is.null(config$raw_data_dir)) "data/raw" else config$raw_data_dir)
    oecd_crs_load_helpers(root)
    fetcher <- get_fetcher("oecd_crs_purpose")
    dest <- file.path(raw_dir, "oecd_crs_purpose.csv")
    df <- fetcher$fn()
    opsi_write_validated(df, dest, fetcher$contract, fetcher$id)
    message("Wrote ", nrow(df), " rows (", length(unique(df$recipient_iso3)), " recipients, ",
            length(unique(df$donor_iso3)), " donors, ", min(df$year), "-", max(df$year), ") to ", dest)
  })
}
