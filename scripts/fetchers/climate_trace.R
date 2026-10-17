# Climate TRACE sector emissions: climate_trace_country_emissions.csv and
# climate_trace_asset_activity.csv.
#
# Why: the Sector Profile (Climate lens and section) and the Country Profile (Policy & climate) show
# sector emissions by country (user request of 2026-10-17). config/climate_trace_map.yml lists what is
# fetched and maps it to technology x stage; R/categories/climate/climate_trace.R has the parsers.
#
# Source: Climate TRACE, public API https://api.climatetrace.org/v6 (no key), CC BY 4.0 - cite
# "Climate TRACE" and link https://climatetrace.org wherever the figures are shown.
#   country totals  /v6/country/emissions?since=<y>&to=<y>&subsectors=<s>&countries=<every country>
#                   one call per (subsector, year) for the countries of /v6/definitions/countries;
#                   a WLD row holds the answer's worldEmissions. Columns: iso3, sector, subsector,
#                   year, co2e_100yr, co2, ch4, n2o, co2e_20yr (tonnes), fetched_on.
#   tracked assets  /v6/assets?subsectors=<s>&year=<y>&limit=10000&offset=<k>, paged until a page is
#                   short; the assets are aggregated per (country, asset group) at once and not kept.
#                   Columns: iso3, subsector, asset_group, year, n_assets, n_activity, co2e_100yr,
#                   co2e_100yr_activity, activity, activity_unit, capacity, capacity_unit, fetched_on.
#
# Every answered call is cached as a small CSV under data/raw_cache/climate_trace/ (git-ignored), so
# a fetch the API cuts off resumes where it stopped. When calls still fail, the fetch stops and names
# each missing (subsector, year): nothing is filled in. CLIMATE_TRACE_REFRESH=true ignores the cache.
#
# Consumer: scripts/16_build_climate_trace.R. scripts/05_ingest_sources.R runs both fetchers when
# data/raw has no copy (manifest ids climate_trace_country_emissions and climate_trace_asset_activity,
# fetch_policy fallback: the staged copy is kept until refetched on purpose, because Climate TRACE
# revises its history with each release). To refetch:
#   Rscript scripts/fetchers/climate_trace.R [country|assets]

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

# Repo root: the option the scripts set, else scripts/05's `repo_root`, else the directory holding
# config/climate_trace_map.yml above the working directory.
climate_trace_repo_root <- function() {
  for (root in list(getOption("opportunity_security.repo_root"), get0("repo_root", envir = globalenv(), inherits = FALSE))) {
    if (is.character(root) && length(root) == 1 && nzchar(root) && file.exists(file.path(root, "config", "climate_trace_map.yml"))) {
      return(root)
    }
  }
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (dirname(d) != d) {
    if (file.exists(file.path(d, "config", "climate_trace_map.yml"))) return(d)
    d <- dirname(d)
  }
  stop("Cannot find config/climate_trace_map.yml above ", getwd(), "; set options(opportunity_security.repo_root).")
}

climate_trace_load_helpers <- function(root = climate_trace_repo_root()) {
  source(file.path(root, "R", "utils", "schema.R"), local = FALSE)
  source(file.path(root, "R", "categories", "climate", "climate_trace.R"), local = FALSE)
  invisible(root)
}

climate_trace_dirs <- function(root = climate_trace_repo_root()) {
  config_path <- file.path(root, "config", "config.yml")
  config <- if (file.exists(config_path)) yaml::read_yaml(config_path) else list()
  raw_dir <- file.path(root, if (is.null(config$raw_data_dir)) "data/raw" else config$raw_data_dir)
  # data/raw_cache is git-ignored scratch space beside data/raw.
  list(raw = raw_dir, cache = file.path(dirname(raw_dir), "raw_cache", "climate_trace"))
}

climate_trace_get_json <- function(url, simplify = FALSE) {
  jsonlite::fromJSON(opsi_http_get_text(url, accept = "application/json", timeout_seconds = 600), simplifyVector = simplify)
}

# Runs `calls` (a data frame with a `key` column) through `one(row)`, caching each answer as
# <cache_dir>/<key>.csv. Returns the rows of every answered call; stops naming the calls that failed.
climate_trace_cached_calls <- function(calls, one, cache_dir, what) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  refresh <- tolower(Sys.getenv("CLIMATE_TRACE_REFRESH", "false")) %in% c("1", "true", "yes")
  out <- vector("list", nrow(calls))
  failed <- character()
  for (i in seq_len(nrow(calls))) {
    path <- file.path(cache_dir, paste0(calls$key[i], ".csv"))
    if (!refresh && file.exists(path)) {
      out[[i]] <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
      next
    }
    res <- tryCatch(one(calls[i, , drop = FALSE]), error = function(e) {
      message("  climate_trace ", what, " ", calls$key[i], ": ", conditionMessage(e))
      NULL
    })
    if (is.null(res)) {
      failed <- c(failed, calls$key[i])
      next
    }
    tmp <- paste0(path, ".tmp")
    utils::write.csv(res, tmp, row.names = FALSE, na = "")
    file.rename(tmp, path)
    out[[i]] <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
    message("  climate_trace ", what, " ", calls$key[i], ": ", nrow(res), " rows")
  }
  if (length(failed)) {
    stop("Climate TRACE ", what, ": ", length(failed), " of ", nrow(calls), " calls failed (cached answers kept in ", cache_dir,
         "; rerun to resume). Missing: ", paste(failed, collapse = ", "))
  }
  dplyr::bind_rows(out)
}

climate_trace_read_map <- function(root = climate_trace_repo_root()) {
  yaml::read_yaml(file.path(root, "config", "climate_trace_map.yml"))
}

climate_trace_fetch_country <- function(root = climate_trace_repo_root()) {
  climate_trace_load_helpers(root)
  map <- climate_trace_read_map(root)
  api <- map$fetch$api %||% CLIMATE_TRACE_API
  countries <- climate_trace_get_json(paste0(api, "/definitions/countries"), simplify = TRUE)
  iso <- sort(unique(countries$alpha3[nzchar(countries$alpha3)]))
  if (length(iso) < 200) stop("Climate TRACE lists only ", length(iso), " countries")
  subs <- map$fetch$country_subsectors
  calls <- expand.grid(i = seq_along(subs), year = as.integer(unlist(map$fetch$years)), KEEP.OUT.ATTRS = FALSE)
  calls$subsector <- vapply(subs[calls$i], function(s) s$subsector, character(1))
  calls$sector <- vapply(subs[calls$i], function(s) s$sector, character(1))
  calls$key <- paste0("country_", calls$subsector, "_", calls$year)
  one <- function(r) {
    x <- climate_trace_get_json(climate_trace_country_url(r$subsector, r$year, iso, api = api))
    climate_trace_parse_country(x, r$subsector, r$sector, r$year)
  }
  out <- climate_trace_cached_calls(calls, one, climate_trace_dirs(root)$cache, "country")
  out$fetched_on <- format(Sys.Date())
  as.data.frame(out[c(CLIMATE_TRACE_COUNTRY_COLUMNS, "fetched_on")])
}

climate_trace_fetch_assets <- function(root = climate_trace_repo_root()) {
  climate_trace_load_helpers(root)
  map <- climate_trace_read_map(root)
  api <- map$fetch$api %||% CLIMATE_TRACE_API
  subs <- map$fetch$asset_subsectors
  calls <- expand.grid(i = seq_along(subs), year = as.integer(unlist(map$fetch$years)), KEEP.OUT.ATTRS = FALSE)
  calls$subsector <- vapply(subs[calls$i], function(s) s$subsector, character(1))
  calls$rule <- vapply(subs[calls$i], function(s) s$group, character(1))
  calls$key <- paste0("assets_", calls$subsector, "_", calls$year)
  one <- function(r) {
    pages <- list()
    offset <- 0L
    repeat {
      x <- climate_trace_get_json(climate_trace_assets_url(r$subsector, r$year, offset = offset, api = api), simplify = TRUE)
      rows <- climate_trace_asset_rows(x$assets)
      pages[[length(pages) + 1L]] <- rows
      if (nrow(rows) < CLIMATE_TRACE_ASSET_PAGE) break
      offset <- offset + CLIMATE_TRACE_ASSET_PAGE
      if (length(pages) > 50L) stop("more than 50 pages of assets")
    }
    climate_trace_aggregate_assets(dplyr::bind_rows(pages), r$subsector, r$year, r$rule)
  }
  out <- climate_trace_cached_calls(calls, one, climate_trace_dirs(root)$cache, "assets")
  out$fetched_on <- format(Sys.Date())
  as.data.frame(out[c(CLIMATE_TRACE_ASSET_COLUMNS, "fetched_on")])
}

register_fetcher(
  id = "climate_trace_country_emissions",
  description = "Climate TRACE country emissions by subsector and year (CO2e 100-year, CC BY 4.0)",
  contract = list(
    required_columns = c("iso3", "sector", "subsector", "year", "co2e_100yr", "co2", "ch4", "n2o", "co2e_20yr"),
    # 14 subsectors x 5 years x ~250 countries (+ WLD); far fewer rows is a broken pull.
    min_rows = 10000,
    unique_key = c("iso3", "subsector", "year")
  ),
  fn = function() climate_trace_fetch_country()
)

register_fetcher(
  id = "climate_trace_asset_activity",
  description = "Climate TRACE tracked assets aggregated by country, subsector, asset group and year: emissions and activity",
  contract = list(
    required_columns = c("iso3", "subsector", "asset_group", "year", "n_assets", "co2e_100yr", "activity", "activity_unit"),
    min_rows = 2000,
    unique_key = c("iso3", "subsector", "asset_group", "year")
  ),
  fn = function() climate_trace_fetch_assets()
)

# Standalone: Rscript scripts/fetchers/climate_trace.R [country|assets] refetches and replaces the
# staged file(s).
if (sys.nframe() == 0L) {
  local({
    root <- climate_trace_repo_root()
    which <- commandArgs(trailingOnly = TRUE)
    if (!length(which)) which <- c("country", "assets")
    raw_dir <- climate_trace_dirs(root)$raw
    for (w in which) {
      id <- switch(w, country = "climate_trace_country_emissions", assets = "climate_trace_asset_activity", stop("unknown part ", w))
      fetcher <- get_fetcher(id)
      dest <- file.path(raw_dir, paste0(id, ".csv"))
      df <- fetcher$fn()
      opsi_write_validated(df, dest, fetcher$contract, fetcher$id)
      message("Wrote ", nrow(df), " rows (", length(unique(df$iso3)), " geographies, ", min(df$year), "-", max(df$year), ") to ", dest)
    }
  })
}
