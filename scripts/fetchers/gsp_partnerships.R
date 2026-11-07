# Global Strategic Partnership Dataset (GSP): gsp_partnership_documents.csv.
#
# Why: the partnership-agreements layer (docs/partnership_agreements.md) needs individual
# partnership documents between governments. GSP collects the strategic-partnership documents
# that governments published from 1990 to 2023 (joint statements, treaties, action plans, press
# conferences, news items and fact sheets), one row per document, with the full text.
#
# Source: Chang-Liao, N. and Fu, L. (2025), Global Strategic Partnership Dataset, V1.0, Harvard
# Dataverse doi:10.7910/DVN/UJD7LW (CC0 1.0). The fetch reads the latest released version's file
# list from the Dataverse API (public, no key), takes the newest db_world*.csv (V1.0, 2025-09-23:
# db_world_Dataverse_20250923.csv, 1,949 rows), downloads it into a new, empty temporary
# directory, reads it as data only (a real CSV parser: fulltext has embedded newlines) and
# removes the directory afterwards. Output, in a fixed schema:
#   row_key, doc_id, country1_cown, country1_iso3c, country2_cown, country2_iso3c, year, month,
#   day, date, title, fulltext, confidence_level1, confidence_level2, source, title_sp,
#   factsheet_sp, type, source_file, dataset_version, fetched_on
# Rows are directed: country1 published the document, country2 is the partner. Values are kept
# as published (codes such as "Kosovo" and "GE-AB", blank dates, an invalid day); only exact
# duplicate rows are dropped. R/agreements/adapter_gsp.R normalises the rest. row_key is
# doc_id (whitespace removed) + "-" + the first 8 hex characters of the MD5 of the source URL,
# with "-2", "-3", ... appended in a fixed order when two rows still share it.
#
# Consumer: scripts/35_ingest_agreements.R --source gsp. Manifest id gsp_partnership_documents
# (fetch_policy prefer). To refetch:
#   Rscript scripts/fetchers/gsp_partnerships.R [--dest=<path>]
# (default destination: <raw_data_dir>/gsp_partnership_documents.csv, raw_data_dir from the
# config named by OPSI_CONFIG, else config/config.yml, else data/raw).
#
# dataverse.harvard.edu sometimes fails the TLS handshake (schannel SEC_E_WRONG_PRINCIPAL); the
# requests are retried. Certificate checks are never disabled.

GSP_DATAVERSE_BASE <- "https://dataverse.harvard.edu"
GSP_DOI <- "doi:10.7910/DVN/UJD7LW"
GSP_FILE_PATTERN <- "^db_world.*\\.(csv|tab)$"

# Upstream columns, in the published order.
GSP_UPSTREAM_COLUMNS <- c(
  "doc_id", "country1_cown", "country1_iso3c", "country2_cown", "country2_iso3c",
  "year", "month", "day", "date", "title", "fulltext", "confidence_level1", "confidence_level2",
  "source", "title_sp", "factsheet_sp", "type"
)
# Without these the file cannot be normalised; the others are filled with "" when absent.
GSP_CORE_COLUMNS <- c("doc_id", "country1_iso3c", "country2_iso3c", "year", "title", "fulltext",
                      "source", "type")
GSP_OUTPUT_COLUMNS <- c("row_key", GSP_UPSTREAM_COLUMNS, "source_file", "dataset_version", "fetched_on")

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

# File list of a Dataverse dataset version (the API's version object, parsed with
# simplifyVector = FALSE) as a data frame.
gsp_dataverse_files <- function(version) {
  files <- version$files
  if (length(files) == 0) {
    return(data.frame(label = character(), id = character(), creation_date = character(),
                      tabular = logical(), stringsAsFactors = FALSE))
  }
  pick <- function(x, default = NA_character_) if (is.null(x) || length(x) == 0) default else x
  data.frame(
    label = vapply(files, function(f) as.character(pick(f$label, pick(f$dataFile$filename))), character(1)),
    id = vapply(files, function(f) as.character(pick(f$dataFile$id)), character(1)),
    creation_date = vapply(files, function(f) as.character(pick(f$dataFile$creationDate)), character(1)),
    tabular = vapply(files, function(f) isTRUE(f$dataFile$tabularData), logical(1)),
    stringsAsFactors = FALSE
  )
}

# The newest world file: db_world*.csv (or the .tab an ingest would make of it), latest creation
# date, then highest file id.
gsp_select_world_file <- function(files) {
  cand <- files[grepl(GSP_FILE_PATTERN, files$label, ignore.case = TRUE), , drop = FALSE]
  if (nrow(cand) == 0) {
    stop("No db_world*.csv file in the GSP Dataverse dataset (files: ", paste(files$label, collapse = ", "), ").")
  }
  ord <- order(cand$creation_date, suppressWarnings(as.numeric(cand$id)), decreasing = TRUE)
  out <- cand[ord[1], , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Release label of a Dataverse version object, e.g. "V1.0 (2025-09-23)".
gsp_dataset_version <- function(version) {
  paste0(
    "V", version$versionNumber, ".", version$versionMinorNumber,
    if (!is.null(version$releaseTime)) paste0(" (", substr(version$releaseTime, 1, 10), ")") else ""
  )
}

gsp_md5 <- function(x) {
  x <- enc2utf8(as.character(x))
  if (requireNamespace("digest", quietly = TRUE)) {
    return(vapply(x, function(s) digest::digest(s, algo = "md5", serialize = FALSE), character(1), USE.NAMES = FALSE))
  }
  # Fallback without digest: MD5 of each string written to a temporary file.
  vapply(x, function(s) {
    tmp <- tempfile("gsp_md5_")
    on.exit(unlink(tmp), add = TRUE)
    writeBin(charToRaw(s), tmp)
    unname(tools::md5sum(tmp))
  }, character(1), USE.NAMES = FALSE)
}

# Stable row key: doc_id (whitespace removed) + "-" + 8 hex characters of the MD5 of the source
# URL. Rows that still share a key get "-2", "-3", ... in the order (date, title, fulltext MD5).
gsp_row_keys <- function(df) {
  doc <- gsub("\\s+", "", df$doc_id)
  key <- paste0(doc, "-", substr(gsp_md5(df$source), 1, 8))
  if (anyDuplicated(key)) {
    ord <- order(key, df$date, df$title, gsp_md5(df$fulltext), method = "radix")
    seq_in <- integer(length(key))
    seq_in[ord] <- stats::ave(seq_along(ord), key[ord], FUN = seq_along)
    key <- ifelse(seq_in > 1L, paste0(key, "-", seq_in), key)
  }
  key
}

# An upstream data frame (all columns read as character) -> the fixed schema above.
gsp_standardize_documents <- function(raw, source_file = "", dataset_version = "", fetched_on = format(Sys.Date())) {
  if (!is.data.frame(raw) || nrow(raw) == 0) stop("GSP file is empty.")
  names(raw) <- sub(paste0("^", intToUtf8(0xFEFF)), "", trimws(names(raw)))
  missing <- setdiff(GSP_CORE_COLUMNS, names(raw))
  if (length(missing)) {
    stop("GSP file lacks column(s) ", paste(missing, collapse = ", "), "; columns are: ", paste(names(raw), collapse = ", "))
  }
  for (col in setdiff(GSP_UPSTREAM_COLUMNS, names(raw))) raw[[col]] <- ""
  out <- raw[, GSP_UPSTREAM_COLUMNS, drop = FALSE]
  for (col in GSP_UPSTREAM_COLUMNS) {
    v <- as.character(out[[col]])
    v[is.na(v)] <- ""
    out[[col]] <- v
  }
  dup <- duplicated(out)
  if (any(dup)) {
    message(sprintf("  gsp_partnership_documents: dropped %d exact duplicate row(s): %s", sum(dup),
                    paste(utils::head(unique(out$doc_id[dup]), 5), collapse = ", ")))
    out <- out[!dup, , drop = FALSE]
  }
  out$row_key <- gsp_row_keys(out)
  out$source_file <- as.character(source_file)
  out$dataset_version <- as.character(dataset_version)
  out$fetched_on <- as.character(fetched_on)
  out <- out[, GSP_OUTPUT_COLUMNS, drop = FALSE]
  rownames(out) <- NULL
  out
}

# Read a downloaded GSP CSV as data: every column character, "NA" kept as text.
gsp_read_csv <- function(path) {
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8",
                  colClasses = "character", na.strings = character())
}

gsp_fetch_documents <- function(base = GSP_DATAVERSE_BASE, doi = GSP_DOI, work_dir = tempfile("gsp_partnerships_"),
                                retries = 5) {
  meta <- opsi_http_get_json(paste0(base, "/api/datasets/:persistentId/?persistentId=", doi), retries = retries)
  version <- meta$data$latestVersion
  if (is.null(version)) stop("Dataverse returned no published version for ", doi)
  file <- gsp_select_world_file(gsp_dataverse_files(version))
  dataset_version <- gsp_dataset_version(version)

  # Checked before the cleanup is registered: an existing directory is someone else's.
  if (dir.exists(work_dir)) stop("GSP download directory already exists: ", work_dir)
  dir.create(work_dir, recursive = TRUE)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  # An ingested (.tab) file is served as tab-delimited text by default; ask for the original CSV.
  url <- paste0(base, "/api/access/datafile/", file$id, if (isTRUE(file$tabular)) "?format=original" else "")
  bytes <- opsi_http_get(url, retries = retries)
  path <- file.path(work_dir, "gsp_world.csv")
  writeBin(bytes, path)
  if (!file.exists(path) || file.size(path) == 0) stop("GSP download failed: ", url)

  out <- gsp_standardize_documents(gsp_read_csv(path), source_file = file$label,
                                   dataset_version = dataset_version, fetched_on = format(Sys.Date()))
  message(sprintf("  gsp_partnership_documents: %s %s, %d documents, %d publishers",
                  dataset_version, file$label, nrow(out), length(unique(out$country1_iso3c))))
  out
}

register_fetcher(
  id = "gsp_partnership_documents",
  description = "Global Strategic Partnership Dataset (Chang-Liao and Fu, Harvard Dataverse): partnership documents with full text",
  contract = list(
    required_columns = c("row_key", "doc_id", "country1_iso3c", "country2_iso3c", "year", "month", "day",
                         "date", "title", "fulltext", "source", "type", "dataset_version", "fetched_on"),
    # V1.0 has 1,948 distinct documents; far fewer is a broken download.
    min_rows = 1500,
    unique_key = "row_key"
  ),
  fn = function() gsp_fetch_documents()
)

# Standalone: Rscript scripts/fetchers/gsp_partnerships.R [--dest=<path>] refetches and replaces
# the staged file.
if (sys.nframe() == 0L) {
  local({
    root <- getOption("opportunity_security.repo_root")
    args <- commandArgs(trailingOnly = TRUE)
    dest_arg <- sub("^--dest=", "", grep("^--dest=", args, value = TRUE))
    config_path <- Sys.getenv("OPSI_CONFIG", file.path(root, "config", "config.yml"))
    if (!grepl("^([A-Za-z]:)?[/\\\\]", config_path)) config_path <- file.path(root, config_path)
    config <- if (file.exists(config_path)) yaml::read_yaml(config_path) else list()
    raw_dir <- if (is.null(config$raw_data_dir)) "data/raw" else config$raw_data_dir
    if (!grepl("^([A-Za-z]:)?[/\\\\]", raw_dir)) raw_dir <- file.path(root, raw_dir)
    dest <- if (length(dest_arg) == 1 && nzchar(dest_arg)) dest_arg else file.path(raw_dir, "gsp_partnership_documents.csv")
    fetcher <- get_fetcher("gsp_partnership_documents")
    df <- fetcher$fn()
    opsi_write_validated(df, dest, fetcher$contract, fetcher$id)
    message("Wrote ", nrow(df), " documents (", length(unique(df$country1_iso3c)), " publishers, ",
            df$dataset_version[1], ") to ", dest)
  })
}
