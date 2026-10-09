# UN Comtrade partner-reported imports from the economies that report no exports to Comtrade:
# critmin_mirror_import.csv.
#
# Why: the critical-minerals trade theme (R/categories/minerals_trade/critical_minerals_trade.R)
# measures world export market shares from reporter-side Comtrade exports. The staged pulls
# (critmin_import_<year>.csv, critmin_export_<year>.csv) are reporter x World only, so an economy
# that does not report to Comtrade - the DR Congo, Russia, Guinea, Gabon, New Caledonia,
# Mongolia, Lao PDR, Viet Nam, ... - has no exports at all and scores 0 on market share. Mirror
# statistics fill that gap: what every reporting economy says it imported from them.
#
# Source: UN Comtrade public preview API, which needs no subscription key but returns at most
# 500 records per call and meters calls (past the quota it answers 403 "Quota Exceeded" with a
# Retry-After header, which is honoured):
#   https://comtradeapi.un.org/public/v1/preview/C/A/HS?period=<year>&partnerCode=<m49,...>
#     &partner2Code=0&flowCode=M&customsCode=C00&motCode=0&cmdCode=<hs6,...>&includeDesc=true
# reporterCode is left empty, so one call returns every reporter's imports of the listed codes
# from the listed partners. A call that comes back full is discarded and split - by partner
# first, then by commodity code - until none is, which keeps the call count near
# records / 500. The same calls with cmdCode=TOTAL give each partner's total mirror exports, the
# denominator of the theme's export-share term. Answered chunks can be cached on disk
# (`cache_dir`), so a fetch cut off by the quota resumes where it stopped.
#
# Late reporters: some of these economies have declared that year's exports to Comtrade since
# the critmin pull was staged (in October 2026 for 2024: the DR Congo, Jordan, Oman, Algeria,
# Iraq, Ghana, ...). The same calls with reporterCode=<partners>, partnerCode=0 (World) and
# flowCode=X fetch those own declarations, commodity codes and TOTAL, so the theme can use an
# economy's own exports whenever it reports them and fall back to the mirror only when it does
# not. (A partner's M49 code is its reporter code for every current economy.)
#
# Scope, chosen from the staged critmin files so the two always describe one Comtrade vintage:
#   year     the newest critmin_export_<year>.csv that clears the manifest's reporter floor;
#   codes    every HS6 code in that vintage's critmin import and export files;
#   partners every WDI economy (wdi_country_info.csv) that reports neither critical-minerals
#            exports nor total exports in that vintage. World, the "areas n.e.s." codes,
#            bunkers, free zones and special categories are never partners here.
# Output: Comtrade's own columns in the snake_case of the staged critmin files, one row per
# (flow, reporter, partner, commodity), plus `fetched_on`: flow M rows are the partners'
# imports from the non-reporters, flow X rows the late reporters' own exports to World. Values
# are as reported: imports are mostly CIF, and the theme moves them to an FOB basis.

CRITMIN_MIRROR_ENDPOINT <- "https://comtradeapi.un.org/public/v1/preview/C/A/HS"
CRITMIN_MIRROR_MAX_RECORDS <- 500L

# WDI codes Comtrade spells differently: the Channel Islands and Kosovo carry Comtrade-only
# ISO codes (_CI, _KS) in the partner reference table.
CRITMIN_MIRROR_PARTNER_OVERRIDES <- c(CHI = 830L, XKX = 412L)

# WDI ISO3 -> Comtrade partner code, from comtradr's bundled partner reference table. Codes of
# entities that no longer exist (Sudan before 2011, Ethiopia before 1993, ...) are dropped so
# each economy maps to its current code.
critmin_mirror_partner_codes <- function(iso3c, partner_ref) {
  ref <- partner_ref[is.na(partner_ref$exit_year), , drop = FALSE]
  ref <- ref[!duplicated(ref$iso_3), , drop = FALSE]
  code <- ref$id[match(iso3c, ref$iso_3)]
  override <- CRITMIN_MIRROR_PARTNER_OVERRIDES[iso3c]
  code <- ifelse(is.na(override), code, override)
  data.frame(iso3c = iso3c, partner_code = as.integer(code), stringsAsFactors = FALSE)
}

# flow "M": every reporter's imports from the economies in `partner_code`; flow "X": those
# economies' own exports to World.
critmin_mirror_url <- function(partner_code, codes, year, flow = c("M", "X")) {
  flow <- match.arg(flow)
  economies <- paste(partner_code, collapse = ",")
  paste0(
    CRITMIN_MIRROR_ENDPOINT,
    "?period=", year,
    if (flow == "M") paste0("&partnerCode=", economies) else paste0("&reporterCode=", economies, "&partnerCode=0"),
    "&partner2Code=0&flowCode=", flow, "&customsCode=C00&motCode=0",
    "&cmdCode=", paste(codes, collapse = ","),
    "&includeDesc=true"
  )
}

# One API response -> the staged-file columns.
critmin_mirror_parse <- function(json) {
  if (!is.null(json$error) && nzchar(as.character(json$error))) {
    stop("Comtrade returned an error: ", json$error)
  }
  rows <- json$data
  if (is.null(rows) || length(rows) == 0 || (is.data.frame(rows) && nrow(rows) == 0)) {
    return(NULL)
  }
  rows <- as.data.frame(rows, stringsAsFactors = FALSE)
  col <- function(name, type = as.character) {
    if (name %in% names(rows)) type(rows[[name]]) else type(rep(NA, nrow(rows)))
  }
  data.frame(
    ref_year = col("refYear", as.integer),
    reporter_code = col("reporterCode", as.integer),
    reporter_iso = col("reporterISO"),
    reporter_desc = col("reporterDesc"),
    flow_code = col("flowCode"),
    partner_code = col("partnerCode", as.integer),
    partner_iso = col("partnerISO"),
    partner_desc = col("partnerDesc"),
    partner2_code = col("partner2Code", as.integer),
    classification_code = col("classificationCode"),
    cmd_code = col("cmdCode"),
    cmd_desc = col("cmdDesc"),
    customs_code = col("customsCode"),
    mot_code = col("motCode", as.integer),
    qty = col("qty", as.numeric),
    net_wgt = col("netWgt", as.numeric),
    cifvalue = col("cifvalue", as.numeric),
    fobvalue = col("fobvalue", as.numeric),
    primary_value = col("primaryValue", as.numeric),
    is_reported = col("isReported", as.logical),
    is_aggregate = col("isAggregate", as.logical),
    stringsAsFactors = FALSE
  )
}

# One GET. A metered-out call (403 "Quota Exceeded", 429, 503) waits for the Retry-After the
# endpoint sends - up to `max_wait_seconds` - and is repeated; other failures back off briefly.
opsi_http_get_json_simple <- function(url, retries = 5, max_wait_seconds = 3700, quiet = FALSE) {
  handle <- curl::new_handle()
  curl::handle_setheaders(handle, "User-Agent" = "OSI-pipeline/1.0 (+https://github.com/lrcarey222/Opportunity_Security_Indices)")
  curl::handle_setopt(handle, timeout = 180, connecttimeout = 60, followlocation = TRUE)

  last_error <- NULL
  for (attempt in seq_len(retries)) {
    res <- tryCatch(curl::curl_fetch_memory(url, handle = handle), error = function(e) e)
    if (inherits(res, "error")) {
      last_error <- conditionMessage(res)
      Sys.sleep(10 * attempt)
      next
    }
    if (res$status_code == 200) {
      txt <- rawToChar(res$content)
      Encoding(txt) <- "UTF-8"
      return(jsonlite::fromJSON(txt, simplifyVector = TRUE))
    }
    last_error <- paste0("HTTP ", res$status_code)
    if (res$status_code %in% c(403, 429, 503)) {
      wait <- suppressWarnings(as.numeric(curl::parse_headers_list(res$headers)[["retry-after"]]))
      if (length(wait) == 0 || is.na(wait)) wait <- 60 * attempt
      if (wait > max_wait_seconds) break
      if (!quiet) message("      Comtrade quota: waiting ", round(wait), " s (", last_error, ")")
      Sys.sleep(wait + 5)
      next
    }
    break
  }
  stop("Request failed after ", retries, " attempts: ", last_error, " for ", url)
}

# Every reporter's imports of `codes` from `partner_code` (one or more partners), or with
# flow "X" those partners' own exports. A response at the record cap may be truncated, so it is
# discarded and the request split in two - partners first, then codes - until each call fits.
# With `cache_dir`, every answered call is kept and reused.
critmin_mirror_fetch_partner <- function(partner_code, codes, year, pause_seconds = 1, cache_dir = NULL,
                                         flow = "M") {
  url <- critmin_mirror_url(partner_code, codes, year, flow = flow)
  cache_file <- if (is.null(cache_dir)) NULL else file.path(cache_dir, paste0(rlang::hash(url), ".rds"))

  if (!is.null(cache_file) && file.exists(cache_file)) {
    json <- readRDS(cache_file)
  } else {
    json <- opsi_http_get_json_simple(url)
    Sys.sleep(pause_seconds)
    if (!is.null(cache_file)) saveRDS(json, cache_file)
  }
  n <- if (is.null(json$count)) 0L else as.integer(json$count)

  if (n >= CRITMIN_MIRROR_MAX_RECORDS) {
    split_half <- function(x) list(x[seq_len(ceiling(length(x) / 2))], x[-seq_len(ceiling(length(x) / 2))])
    if (length(partner_code) > 1) {
      parts <- split_half(partner_code)
      return(rbind(
        critmin_mirror_fetch_partner(parts[[1]], codes, year, pause_seconds, cache_dir, flow),
        critmin_mirror_fetch_partner(parts[[2]], codes, year, pause_seconds, cache_dir, flow)
      ))
    }
    if (length(codes) == 1) {
      stop("Partner ", partner_code, ", code ", codes, ": ", n, " records hit the preview cap.")
    }
    parts <- split_half(codes)
    return(rbind(
      critmin_mirror_fetch_partner(partner_code, parts[[1]], year, pause_seconds, cache_dir, flow),
      critmin_mirror_fetch_partner(partner_code, parts[[2]], year, pause_seconds, cache_dir, flow)
    ))
  }
  critmin_mirror_parse(json)
}

# All partners' commodity flows and totals, then the same partners' own exports (late
# reporters). A pass that fails (the quota outlasting Retry-After, an outage) is retried once
# after `retry_pause_seconds`; chunks already answered come from the cache. If it fails again the
# whole fetch fails, so a pull missing an economy never replaces a complete one.
critmin_mirror_fetch <- function(partners, codes, year, pause_seconds = 1, retry_pause_seconds = 60,
                                 cache_dir = NULL, quiet = FALSE) {
  if (!is.null(cache_dir)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  pass <- function(cmd, label, flow) {
    if (!quiet) message("    ", label, ": ", nrow(partners), " partners")
    tryCatch(
      critmin_mirror_fetch_partner(partners$partner_code, cmd, year, pause_seconds, cache_dir, flow),
      error = function(e) e
    )
  }
  run <- function(cmd, label, flow = "M") {
    out <- pass(cmd, label, flow)
    if (inherits(out, "error")) {
      if (!quiet) message("    ", label, " failed (", conditionMessage(out), "); retrying once")
      Sys.sleep(retry_pause_seconds)
      out <- pass(cmd, label, flow)
    }
    if (inherits(out, "error")) stop("Mirror fetch failed (", label, "): ", conditionMessage(out))
    out
  }

  df <- rbind(run(codes, "commodity flows"), run("TOTAL", "total trade"))
  if (is.null(df)) stop("No mirror records returned for ", nrow(partners), " partners.")
  missing <- setdiff(partners$partner_code, df$partner_code)
  if (length(missing) > 0 && !quiet) {
    message("    no importer reports trade with: ", paste(partners$iso3c[partners$partner_code %in% missing], collapse = ", "))
  }

  own <- rbind(run(codes, "own exports (late reporters)", "X"), run("TOTAL", "own total exports (late reporters)", "X"))
  if (!is.null(own) && !quiet) {
    message("    late reporters with own exports: ", paste(sort(unique(own$reporter_iso)), collapse = ", "))
  }
  df <- rbind(df, own)
  df$fetched_on <- as.character(Sys.Date())
  df[order(df$flow_code, df$partner_code, df$cmd_code, df$reporter_code), , drop = FALSE]
}

# Year, codes and non-reporting partners from the staged critmin files (see header).
critmin_mirror_scope <- function(raw_dir, manifest = NULL, partner_ref = NULL) {
  resolve <- function(kind, fallback) {
    resolve_versioned_raw_input(
      raw_dir,
      pattern = paste0("^critmin_", kind, "_\\d{4}\\.csv$"),
      fallback = fallback,
      label = paste("Comtrade critical minerals", kind),
      quiet = TRUE,
      manifest = manifest
    )
  }
  export_path <- resolve("export", "critmin_export_2024.csv")
  year <- as.integer(sub("^.*_(\\d{4})\\.csv$", "\\1", basename(export_path)))
  import_path <- file.path(raw_dir, sprintf("critmin_import_%d.csv", year))
  total_path <- file.path(raw_dir, sprintf("critmin_total_export_%d.csv", year))

  read_cols <- function(path) {
    utils::read.csv(path, colClasses = "character")[, c("reporter_iso", "cmd_code"), drop = FALSE]
  }
  exports <- read_cols(export_path)
  imports <- read_cols(import_path)
  totals <- read_cols(total_path)

  # The economies the indices score: WDI rows that are not regional or income aggregates.
  country_info <- utils::read.csv(file.path(raw_dir, "wdi_country_info.csv"), stringsAsFactors = FALSE)
  if ("region" %in% names(country_info)) {
    country_info <- country_info[is.na(country_info$region) | country_info$region != "Aggregates", , drop = FALSE]
  }
  economies <- unique(toupper(trimws(country_info$iso3c)))
  economies <- economies[!is.na(economies) & nzchar(economies)]
  reporters <- unique(c(exports$reporter_iso, totals$reporter_iso))
  non_reporters <- sort(setdiff(economies, reporters))

  if (is.null(partner_ref)) partner_ref <- comtradr::ct_get_ref_table("partner")
  partners <- critmin_mirror_partner_codes(non_reporters, partner_ref)
  if (any(is.na(partners$partner_code))) {
    warning("No Comtrade partner code for: ", paste(partners$iso3c[is.na(partners$partner_code)], collapse = ", "))
  }

  list(
    year = year,
    codes = sort(unique(c(exports$cmd_code, imports$cmd_code))),
    partners = partners[!is.na(partners$partner_code), , drop = FALSE]
  )
}

critmin_mirror_default_fetch <- function() {
  root <- getOption("opportunity_security.repo_root")
  if (is.null(root) || !nzchar(root)) {
    root <- if (requireNamespace("rprojroot", quietly = TRUE)) {
      tryCatch(rprojroot::find_root(rprojroot::is_git_root), error = function(e) getwd())
    } else {
      getwd()
    }
  }
  config <- getOption("opportunity_security.config")
  raw_dir <- file.path(root, if (is.null(config$raw_data_dir)) "data/raw" else config$raw_data_dir)
  if (!exists("resolve_versioned_raw_input", mode = "function")) {
    source(file.path(root, "scripts", "utils", "raw_inputs.R"), local = FALSE)
  }
  manifest <- read_raw_inputs_manifest(raw_inputs_manifest_path(root))

  scope <- critmin_mirror_scope(raw_dir, manifest = manifest)
  # data/raw_cache is git-ignored scratch space beside data/raw.
  cache_dir <- file.path(dirname(raw_dir), "raw_cache", paste0("critmin_mirror_", scope$year))
  critmin_mirror_fetch(scope$partners, scope$codes, scope$year, cache_dir = cache_dir)
}

register_fetcher(
  id = "critmin_mirror_import",
  description = "UN Comtrade partner-reported imports from non-reporting economies, and their late own exports (public preview API)",
  contract = list(
    required_columns = c(
      "ref_year", "reporter_code", "reporter_iso", "flow_code", "partner_code", "partner_iso",
      "cmd_code", "cifvalue", "fobvalue", "primary_value", "fetched_on"
    ),
    # Several thousand rows over the ~85 non-reporting economies; a few hundred means a
    # broken pull.
    min_rows = 1000,
    unique_key = c("ref_year", "flow_code", "reporter_code", "partner_code", "cmd_code")
  ),
  fn = function() critmin_mirror_default_fetch()
)
