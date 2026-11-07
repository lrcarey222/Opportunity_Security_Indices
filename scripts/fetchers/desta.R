# Design of Trade Agreements (DESTA): five files into data/raw (docs/partnership_agreements.md).
#
# Why: DESTA is the structured source of the partnership-agreements layer for trade agreements -
# every preferential trade agreement since 1948 with its parties, signature and entry-into-force
# years, accessions and withdrawals, and a content coding of about 500 provisions (services,
# investment, procurement, standards, competition, IPRs) with the derived depth index.
# R/agreements/adapter_desta.R turns it into agreement records.
#
# Source: https://www.designoftradeagreements.org/downloads/ (version 2.3, "Current version:
# September 2025"). No licence is published; the authors ask users to cite Dür, Baccini and Elsig
# (2014), The Design of International Trade Agreements: Introducing a New Database, Review of
# International Organizations 9(3): 353-375. Only derived facts are redistributed.
#
# The fetch reads the downloads page and takes the newest version of each file it links (so a new
# release is picked up), falling back to the v2.3 URLs below when the page cannot be read or no
# longer links a file. Each file is downloaded into a new, empty temporary directory, read as data
# only, converted to UTF-8 (the files are Latin-1 / Windows-1252; non-breaking spaces become plain
# spaces) and written with every column as published plus
#   source_version (e.g. "2.3"), source_release ("YYYY-MM", from the page), fetched_on
# Files (manifest ids = file names):
#   desta_list_of_treaties   one row per entry (base treaty, accession, withdrawal, protocol,
#                            consolidated, negotiation); parties c1..c91, roles mc1..mc91
#   desta_dyads              the dyadic list (omits dyads by membership role: do not build parties
#                            from it)
#   desta_withdrawals        dyadic withdrawals (iso1 = the withdrawing country)
#   desta_provisions         the content coding (desta_version_*.csv)
#   desta_indices            depth_index and its components, depth_rasch, flexibility, enforcement
#
# Consumers: scripts/35_ingest_agreements.R --source desta. scripts/05_ingest_sources.R refreshes
# the files when they are older than the annual cadence allows (fetch_policy prefer). To refetch:
#   Rscript scripts/fetchers/desta.R [--raw-dir data/raw] [--only desta_indices,desta_provisions]

DESTA_DOWNLOADS_URL <- "https://www.designoftradeagreements.org/downloads/"
DESTA_SITE_URL <- "https://www.designoftradeagreements.org"
DESTA_FALLBACK_VERSION <- "02_03"
DESTA_FALLBACK_RELEASE <- "2025-09"

# File id -> the published file-name pattern (version captured as two groups) and the v2.3 path.
DESTA_FILES <- list(
  desta_list_of_treaties = list(
    pattern = "desta_list_of_treaties_([0-9]+)_([0-9]+)\\.csv",
    fallback = "/media/filer_public/5e/11/5e11943b-8871-4e8b-bcb2-8188aa83969c/desta_list_of_treaties_02_03.csv"),
  desta_dyads = list(
    pattern = "desta_list_of_treaties_([0-9]+)_([0-9]+)_dyads\\.csv",
    fallback = "/media/filer_public/6a/45/6a454835-eef1-44c4-8af2-5557a9552167/desta_list_of_treaties_02_03_dyads.csv"),
  desta_withdrawals = list(
    pattern = "desta_dyadic_withdrawal_([0-9]+)_([0-9]+)\\.csv",
    fallback = "/media/filer_public/38/c6/38c6e92f-a513-443e-8907-77b570fd2973/desta_dyadic_withdrawal_02_03.csv"),
  desta_provisions = list(
    pattern = "desta_version_([0-9]+)_([0-9]+)\\.csv",
    fallback = "/media/filer_public/53/af/53af422c-29b0-48f9-bf1c-3537f5c028d4/desta_version_02_03.csv"),
  desta_indices = list(
    pattern = "desta_indices_version_([0-9]+)_([0-9]+)\\.csv",
    fallback = "/media/filer_public/0c/64/0c64ec71-5728-409f-91d8-64c324bf4400/desta_indices_version_02_03.csv")
)

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

# Absolute URL of an href on the DESTA site.
desta_absolute_url <- function(href) {
  ifelse(grepl("^https?://", href), href, paste0(DESTA_SITE_URL, ifelse(startsWith(href, "/"), "", "/"), href))
}

# The newest version of each file the downloads page links, and the release month the page states
# ("Current version: September 2025."). Pure: `html` is the page text. Files the page does not link
# keep their v2.3 fallback (`discovered` FALSE).
desta_discover_files <- function(html) {
  html <- if (is.null(html) || is.na(html)) "" else html
  hrefs <- regmatches(html, gregexpr("href=\"[^\"]+\"", html))[[1]]
  hrefs <- sub("^href=\"", "", sub("\"$", "", hrefs))
  rows <- lapply(names(DESTA_FILES), function(id) {
    spec <- DESTA_FILES[[id]]
    pat <- paste0("(^|/)", spec$pattern, "$")
    hit <- hrefs[grepl(pat, hrefs, perl = TRUE)]
    if (length(hit) == 0) {
      return(data.frame(id = id, url = desta_absolute_url(spec$fallback), version = DESTA_FALLBACK_VERSION,
                        discovered = FALSE, stringsAsFactors = FALSE))
    }
    m <- regmatches(basename(hit), regexec(spec$pattern, basename(hit), perl = TRUE))
    major <- vapply(m, function(x) as.integer(x[2]), integer(1))
    minor <- vapply(m, function(x) as.integer(x[3]), integer(1))
    best <- order(-major, -minor)[1]
    data.frame(id = id, url = desta_absolute_url(hit[best]), version = sprintf("%02d_%02d", major[best], minor[best]),
               discovered = TRUE, stringsAsFactors = FALSE)
  })
  files <- do.call(rbind, rows)
  text <- gsub("<[^>]+>", " ", html)
  rel <- regmatches(text, regexpr("Current version:\\s*[A-Z][a-z]+\\s+[0-9]{4}", text, perl = TRUE))
  release <- DESTA_FALLBACK_RELEASE
  if (length(rel) == 1) {
    parts <- strsplit(trimws(sub("^Current version:\\s*", "", rel)), "\\s+")[[1]]
    mon <- match(tolower(parts[1]), tolower(month.name))
    if (!is.na(mon)) release <- sprintf("%s-%02d", parts[2], mon)
  }
  attr(files, "release") <- release
  files
}

# Decode a downloaded CSV to UTF-8 text: UTF-8 when valid, else Windows-1252 (Latin-1 for bytes
# Windows-1252 leaves undefined); non-breaking spaces become spaces; a BOM is dropped.
desta_decode_text <- function(bytes) {
  txt <- rawToChar(bytes)
  if (!validUTF8(txt)) {
    conv <- iconv(txt, from = "CP1252", to = "UTF-8")
    if (is.na(conv)) conv <- iconv(txt, from = "latin1", to = "UTF-8")
    txt <- conv
  }
  Encoding(txt) <- "UTF-8"
  txt <- gsub(" ", " ", txt, fixed = TRUE)
  sub("^﻿", "", txt)
}

desta_read_csv_text <- function(txt) {
  df <- utils::read.csv(text = txt, colClasses = "character", check.names = FALSE, stringsAsFactors = FALSE,
                        encoding = "UTF-8", na.strings = c("", "NA"))
  df <- df[, nzchar(names(df)) & !is.na(names(df)), drop = FALSE]
  for (cn in names(df)) {
    v <- df[[cn]]
    Encoding(v) <- "UTF-8"
    df[[cn]] <- v
  }
  df
}

# The downloads page and the discovered file table, fetched once per R session.
DESTA_FETCH_CACHE <- new.env(parent = emptyenv())

desta_files <- function(refresh = FALSE) {
  if (!refresh && exists("files", envir = DESTA_FETCH_CACHE, inherits = FALSE)) {
    return(get("files", envir = DESTA_FETCH_CACHE, inherits = FALSE))
  }
  html <- tryCatch(opsi_http_get_text(DESTA_DOWNLOADS_URL, retries = 2), error = function(e) {
    message("  desta: downloads page unavailable (", conditionMessage(e), "); using the v2.3 file URLs")
    ""
  })
  files <- desta_discover_files(html)
  assign("files", files, envir = DESTA_FETCH_CACHE)
  files
}

desta_fetch <- function(id, work_dir = tempfile(paste0(id, "_"))) {
  files <- desta_files()
  row <- files[files$id == id, , drop = FALSE]
  if (nrow(row) != 1) stop("Unknown DESTA file id: ", id)
  # Checked before the cleanup is registered: an existing directory is someone else's.
  if (dir.exists(work_dir)) stop("DESTA download directory already exists: ", work_dir)
  dir.create(work_dir, recursive = TRUE)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  bytes <- opsi_http_get(row$url, accept = "text/csv,*/*")
  path <- file.path(work_dir, "download.csv")
  writeBin(bytes, path)
  if (!file.exists(path) || file.size(path) == 0) stop("DESTA download is empty: ", row$url)
  df <- desta_read_csv_text(desta_decode_text(readBin(path, "raw", file.size(path))))

  df$source_version <- sub("^0", "", sub("_0?", ".", row$version))
  df$source_release <- attr(files, "release")
  df$fetched_on <- format(Sys.Date())
  message(sprintf("  %s: %d rows x %d columns from %s (version %s%s)", id, nrow(df), ncol(df), basename(row$url),
                  df$source_version[1], if (isTRUE(row$discovered)) "" else ", fallback URL"))
  df
}

register_fetcher(
  id = "desta_list_of_treaties",
  description = "DESTA list of treaties: one row per entry with parties (c1..c91) and membership roles (mc1..mc91)",
  contract = list(
    required_columns = c("number", "base_treaty", "name", "entry_type", "consolidated", "year", "entryforceyear",
                         "typememb", "c1", "c2", "mc1", "mc2"),
    # 1,410 entries in v2.3 (September 2025).
    min_rows = 1000,
    unique_key = c("number", "base_treaty")
  ),
  fn = function() desta_fetch("desta_list_of_treaties")
)

register_fetcher(
  id = "desta_dyads",
  description = "DESTA list of treaties, dyadic version (omits dyads by membership role)",
  contract = list(
    required_columns = c("country1", "country2", "iso1", "iso2", "number", "base_treaty", "name", "entry_type",
                         "year", "entryforceyear"),
    # 20,569 dyads in v2.3.
    min_rows = 10000,
    unique_key = c("iso1", "iso2", "number", "base_treaty")
  ),
  fn = function() desta_fetch("desta_dyads")
)

register_fetcher(
  id = "desta_withdrawals",
  description = "DESTA dyadic withdrawals (iso1 is the withdrawing country)",
  contract = list(
    required_columns = c("country1", "country2", "iso1", "iso2", "number", "base_treaty", "name", "year"),
    # 1,650 withdrawal dyads (72 withdrawal entries) in v2.3.
    min_rows = 500,
    unique_key = c("iso1", "iso2", "number", "base_treaty")
  ),
  fn = function() desta_fetch("desta_withdrawals")
)

register_fetcher(
  id = "desta_provisions",
  description = "DESTA content coding (desta_version_*): one row per coded entry, about 500 provision variables",
  contract = list(
    required_columns = c("number", "base_treaty", "name", "entry_type", "year", "mar_typedepth", "ser_specific",
                         "inv_chapter", "proc_prov", "tbt_prov", "sps_prov", "comp_chap", "ipr_nt"),
    # 781 coded entries in v2.3.
    min_rows = 600,
    unique_key = c("number", "base_treaty")
  ),
  fn = function() desta_fetch("desta_provisions")
)

register_fetcher(
  id = "desta_indices",
  description = "DESTA indices: depth_index and its seven components, depth_rasch, flexibility and enforcement",
  contract = list(
    required_columns = c("number", "base_treaty", "name", "year", "entry_type", "full_fta", "standards", "investments",
                         "services", "procurement", "competition", "iprs", "depth_index", "depth_rasch"),
    # 692 entries in v2.3.
    min_rows = 500,
    unique_key = c("number", "base_treaty")
  ),
  fn = function() desta_fetch("desta_indices")
)

# Standalone: Rscript scripts/fetchers/desta.R [--raw-dir DIR] [--only id1,id2] refetches and
# replaces the staged files (each validated by its contract before it replaces the old copy).
if (sys.nframe() == 0L) {
  local({
    args <- commandArgs(trailingOnly = TRUE)
    flag <- function(name, default = NULL) {
      i <- match(paste0("--", name), args)
      if (!is.na(i) && i < length(args)) return(args[i + 1])
      hit <- grep(paste0("^--", name, "="), args, value = TRUE)
      if (length(hit)) return(sub(paste0("^--", name, "="), "", hit[1]))
      default
    }
    root <- getOption("opportunity_security.repo_root", normalizePath(".", winslash = "/"))
    config_path <- Sys.getenv("OPSI_CONFIG", file.path(root, "config", "config.yml"))
    if (!grepl("^([A-Za-z]:)?[/\\\\]", config_path)) config_path <- file.path(root, config_path)
    config <- if (file.exists(config_path)) yaml::read_yaml(config_path) else list()
    raw_dir <- flag("raw-dir", if (is.null(config$raw_data_dir)) "data/raw" else config$raw_data_dir)
    if (!grepl("^([A-Za-z]:)?[/\\\\]", raw_dir)) raw_dir <- file.path(root, raw_dir)
    ids <- names(DESTA_FILES)
    only <- flag("only")
    if (!is.null(only)) ids <- intersect(ids, trimws(strsplit(only, ",")[[1]]))
    failed <- character()
    for (id in ids) {
      fetcher <- get_fetcher(id)
      dest <- file.path(raw_dir, paste0(id, ".csv"))
      ok <- tryCatch({
        df <- fetcher$fn()
        opsi_write_validated(df, dest, fetcher$contract, fetcher$id)
        message("Wrote ", nrow(df), " rows to ", dest)
        TRUE
      }, error = function(e) {
        message("FAILED ", id, ": ", conditionMessage(e))
        FALSE
      })
      if (!ok) failed <- c(failed, id)
    }
    if (length(failed)) stop("DESTA fetch failed for: ", paste(failed, collapse = ", "))
  })
}
