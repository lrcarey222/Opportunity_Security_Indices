# BNEF LCOE Data Viewer reader.
#
# BNEF changed the delivery format between releases: the 2025 update was staged as a CSV
# export of the "Raw LCOE data" sheet ("2025-03-24 - 2025 LCOE Data Viewer Tool.csv"),
# the 2026 update as the whole macro workbook in Excel's binary container
# ("2026-08-11 - LCOE Data.xlsb"). The sheet layout is identical in both: six banner rows,
# a "Key cost metrics (YYYY real)" line, then a header row of Metric/Region/Technology/
# Scenario/Unit followed by one column per year.
#
# No maintained R package reads .xlsb - readxlsb was archived from CRAN - so a workbook is
# flattened to CSV once by scripts/utils/convert_xlsb_sheet.ps1 (Excel automation) and the
# result cached under data/raw_cache. The cache is keyed on the source file's mtime, so a
# newly staged release reconverts and an unchanged one is read straight from cache.
#
# read_bnef_lcoe() returns the sheet in the same shape read.csv(skip = 8) always gave, with
# the release's reference year attached as the "reference_year" attribute. That year is what
# the LCOE theme scores as "current", and it moves with the release (2024 -> 2025), which is
# why it is read from the file rather than hard-coded.

BNEF_LCOE_SHEET <- "Raw LCOE data"

# Rows above the Metric/Region/Technology header, i.e. the read.csv `skip` value.
BNEF_LCOE_SKIP_ROWS <- 8L

# 1-based row holding "Key cost metrics (YYYY real)".
BNEF_LCOE_REFERENCE_ROW <- 8L

bnef_lcoe_cache_dir <- function(root_dir) {
  file.path(root_dir, "data", "raw_cache", "bnef_lcoe")
}

# Deterministic cache name that cannot collide with the staged inputs the manifest
# resolver scans for - the "__" sheet suffix keeps it outside the release patterns.
bnef_lcoe_cache_path <- function(path, cache_dir) {
  stem <- sub("\\.[A-Za-z0-9]+$", "", basename(path))
  file.path(cache_dir, paste0(stem, "__", BNEF_LCOE_SHEET, ".csv"))
}

bnef_lcoe_convert_workbook <- function(path, dest, converter) {
  if (!identical(.Platform$OS.type, "windows")) {
    stop(
      "Cannot read ", basename(path), ": flattening an .xlsb needs Excel automation, ",
      "which is Windows-only. Export the '", BNEF_LCOE_SHEET, "' sheet to CSV as\n  ",
      dest, "\nand rerun."
    )
  }
  if (!file.exists(converter)) {
    stop("BNEF LCOE converter not found: ", converter)
  }

  dest_dir <- dirname(dest)
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  message("Flattening ", basename(path), " sheet '", BNEF_LCOE_SHEET, "' (Excel, ~1 min)")
  status <- system2(
    "powershell",
    c(
      "-NoProfile", "-ExecutionPolicy", "Bypass", "-File", shQuote(converter),
      "-Path", shQuote(normalizePath(path, winslash = "\\", mustWork = TRUE)),
      "-Sheet", shQuote(BNEF_LCOE_SHEET),
      "-Out", shQuote(normalizePath(dest, winslash = "\\", mustWork = FALSE))
    ),
    stdout = TRUE,
    stderr = TRUE
  )

  if (!is.null(attr(status, "status")) && attr(status, "status") != 0 || !file.exists(dest)) {
    stop(
      "Failed to flatten ", basename(path), " via Excel:\n",
      paste(status, collapse = "\n")
    )
  }

  invisible(dest)
}

# Path to a CSV holding the raw LCOE sheet, converting and caching an .xlsb release first.
bnef_lcoe_resolve_csv <- function(path,
                                  cache_dir,
                                  converter,
                                  force = FALSE) {
  if (!file.exists(path)) {
    stop("BNEF LCOE input not found: ", path)
  }

  ext <- tolower(sub(".*\\.", "", basename(path)))
  if (identical(ext, "csv")) {
    return(path)
  }
  if (!ext %in% c("xlsb", "xlsx", "xlsm")) {
    stop("Unsupported BNEF LCOE input format '", ext, "': ", path)
  }

  cached <- bnef_lcoe_cache_path(path, cache_dir)
  cache_is_fresh <- file.exists(cached) &&
    as.numeric(file.info(cached)$mtime) >= as.numeric(file.info(path)$mtime)

  if (!isTRUE(force) && cache_is_fresh) {
    return(cached)
  }

  bnef_lcoe_convert_workbook(path, cached, converter = converter)
  cached
}

# Reference ("current") year of a release, from the "Key cost metrics (YYYY real)" banner.
bnef_lcoe_reference_year <- function(csv_path) {
  banner <- tryCatch(
    readLines(csv_path, n = BNEF_LCOE_REFERENCE_ROW, warn = FALSE),
    error = function(e) character()
  )
  if (length(banner) < BNEF_LCOE_REFERENCE_ROW) {
    return(NA_integer_)
  }

  year <- stringr::str_match(
    banner[[BNEF_LCOE_REFERENCE_ROW]],
    "Key cost metrics\\s*\\((\\d{4}) real\\)"
  )[1, 2]

  suppressWarnings(as.integer(year))
}

read_bnef_lcoe <- function(path,
                           root_dir,
                           cache_dir = bnef_lcoe_cache_dir(root_dir),
                           converter = file.path(root_dir, "scripts", "utils", "convert_xlsb_sheet.ps1"),
                           force = FALSE) {
  csv_path <- bnef_lcoe_resolve_csv(path, cache_dir = cache_dir, converter = converter, force = force)

  lcoe <- utils::read.csv(csv_path, skip = BNEF_LCOE_SKIP_ROWS, check.names = TRUE)
  attr(lcoe, "reference_year") <- bnef_lcoe_reference_year(csv_path)
  attr(lcoe, "source_file") <- basename(path)
  lcoe
}
