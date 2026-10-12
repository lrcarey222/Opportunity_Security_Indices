# EIA International Energy Statistics, fossil fuel production and consumption:
# eia_intl_fossil_production.csv.
#
# Why: EI lists fewer producers than consumers and folds the rest into "Other <region>"
# residuals. Where EI has no production row for an economy and fuel, the import dependence and
# production themes take the EIA value of the same year in EI's units (user decision of
# 2026-10-11; R/utils/eia_intl.R has the rules and the unit conversions).
#
# Source: U.S. Energy Information Administration, International Energy Statistics, bulk download
# https://www.eia.gov/opendata/bulk/INTL.zip - public, no API key; a work of the US government,
# in the public domain. The zip holds one file, INTL.txt, with one JSON object per series
# (about 105,000 lines and 130 MB in October 2026).
#
# The fetch downloads the zip into a new, empty temporary directory and reads INTL.txt from it as
# data only (nothing in the download is run or sourced). eia_intl_parse_lines() keeps the series
# in eia_intl_series_spec() - production of crude oil incl. lease condensate, NGPL, dry natural
# gas and coal; consumption of petroleum and other liquids, dry natural gas and coal - for
# three-letter (ISO3) geographies and years from EIA_INTL_MIN_YEAR, and the result is written as
#   iso3, country, fuel, flow, component, year, value, unit, series_id, value_note, last_updated
# (value_note holds EIA's code where it gives no number: "--", "NA", "ie", "w"). The temporary
# directory is removed afterwards.
#
# Consumers: scripts/10_build_themes.R and scripts/40_build_index_vintages.R.
# scripts/05_ingest_sources.R runs it when data/raw has no copy (manifest id
# eia_intl_fossil_production, fetch_policy fallback: the staged copy is kept until it is
# refetched on purpose, because EIA revises the series every month). To refetch:
#   Rscript scripts/fetchers/eia_intl.R

EIA_INTL_MIN_YEAR <- 2000L

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
# defines (it sources this file from any working directory), else the directory holding
# R/utils/eia_intl.R above the working directory.
eia_intl_repo_root <- function() {
  for (root in list(getOption("opportunity_security.repo_root"), get0("repo_root", envir = globalenv(), inherits = FALSE))) {
    if (is.character(root) && length(root) == 1 && nzchar(root) && file.exists(file.path(root, "R", "utils", "eia_intl.R"))) {
      return(root)
    }
  }
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (dirname(d) != d) {
    if (file.exists(file.path(d, "R", "utils", "eia_intl.R"))) return(d)
    d <- dirname(d)
  }
  stop("Cannot find R/utils/eia_intl.R above ", getwd(), "; set options(opportunity_security.repo_root).")
}

eia_intl_load_helpers <- function(root = eia_intl_repo_root()) {
  for (f in c("schema.R", "eia_intl.R")) {
    source(file.path(root, "R", "utils", f), local = FALSE)
  }
  invisible(root)
}

# Downloads INTL.zip into `work_dir` (created; must not exist yet) and returns the zip's path.
eia_intl_download <- function(work_dir, url = EIA_INTL_BULK_URL) {
  if (dir.exists(work_dir)) {
    stop("EIA download directory already exists: ", work_dir)
  }
  dir.create(work_dir, recursive = TRUE)
  zip_path <- file.path(work_dir, "INTL.zip")
  writeBin(opsi_http_get(url, timeout_seconds = 900), zip_path)
  zip_path
}

# The tidy extract from a downloaded INTL.zip. Only the member INTL.txt is read, through a
# connection to the zip, so no other file in the archive is written to disk.
eia_intl_read_zip <- function(zip_path, min_year = EIA_INTL_MIN_YEAR) {
  members <- utils::unzip(zip_path, list = TRUE)
  if (!"INTL.txt" %in% members$Name) {
    stop("INTL.zip has no INTL.txt (members: ", paste(utils::head(members$Name, 5), collapse = ", "), ")")
  }
  con <- unz(zip_path, "INTL.txt")
  open(con, "rb")
  on.exit(close(con), add = TRUE)
  lines <- readLines(con, warn = FALSE, encoding = "UTF-8")
  eia_intl_parse_lines(lines, min_year = min_year)
}

eia_intl_fetch <- function(url = NULL, work_dir = tempfile("eia_intl_")) {
  if (!exists("eia_intl_parse_lines", mode = "function")) eia_intl_load_helpers()
  if (is.null(url)) url <- EIA_INTL_BULK_URL
  # Checked before the cleanup is registered: an existing directory is someone else's, and the
  # stop() in eia_intl_download() must not trigger its deletion.
  if (dir.exists(work_dir)) {
    stop("EIA download directory already exists: ", work_dir)
  }
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  out <- eia_intl_read_zip(eia_intl_download(work_dir, url = url))
  out$fetched_on <- format(Sys.Date())
  as.data.frame(out)
}

register_fetcher(
  id = "eia_intl_fossil_production",
  description = "EIA International Energy Statistics bulk file: fossil fuel production and consumption by country",
  contract = list(
    required_columns = c("iso3", "country", "fuel", "flow", "component", "year", "value", "unit", "series_id"),
    # About 8 series x 220 economies x 26 years (2000 on); far fewer rows is a broken download.
    min_rows = 20000,
    unique_key = c("series_id", "year")
  ),
  fn = function() eia_intl_fetch()
)

# Standalone: Rscript scripts/fetchers/eia_intl.R refetches and replaces the staged file.
if (sys.nframe() == 0L) {
  local({
    root <- eia_intl_repo_root()
    config_path <- file.path(root, "config", "config.yml")
    config <- if (file.exists(config_path)) yaml::read_yaml(config_path) else list()
    raw_dir <- file.path(root, if (is.null(config$raw_data_dir)) "data/raw" else config$raw_data_dir)
    eia_intl_load_helpers(root)
    fetcher <- get_fetcher("eia_intl_fossil_production")
    dest <- file.path(raw_dir, "eia_intl_fossil_production.csv")
    df <- fetcher$fn()
    opsi_write_validated(df, dest, fetcher$contract, fetcher$id)
    message("Wrote ", nrow(df), " rows (", length(unique(df$iso3)), " economies, ",
            min(df$year), "-", max(df$year), ") to ", dest)
  })
}
