# US patent grants by technology and inventor country: uspto_y02_patents.csv.
#
# Why: the index has no country-level innovation measure (Technological Readiness is the IEA's
# maturity rating of each technology, the same for every country). Climate-mitigation patents by
# technology are the measure CFR's Global Energy Innovation Index uses (its K4 indicator, from OECD
# triadic families); the OECD SDMX API only publishes them at domain level (energy, transport), so
# this prototype counts US grants by CPC group instead.
#
# Source: USPTO Open Data Portal, patent file wrapper search
# (https://api.uspto.gov/api/v1/patent/applications/search). Needs a free ODP API key in the
# environment variable USPTO_KEY (put it in your user .Renviron, never in the repo). US public data.
#
# For each technology in config/patent_tech_cpc.yml and grant year, it pages through every granted
# patent carrying one of the technology's CPC groups (100 per request) and counts inventor countries
# whole and fractionally (R/utils/uspto_patents.R has the rules). For the denominator of revealed
# technological advantage it then counts all utility grants per inventor country and year with one
# count-only query each, for every country that has a grant in any technology. Writes
#   grant_year, tech, inventor_iso2, patents_whole, patents_fractional, fetched_on
# with inventor_iso2 "WLD" for the number of grants and tech "_ALL" for all utility grants.
#
# About 1,100 record pages plus ~700 count queries for 2016-2025: allow 30-45 minutes. To refetch:
#   Rscript scripts/fetchers/uspto_patents.R

USPTO_ODP_SEARCH_URL <- "https://api.uspto.gov/api/v1/patent/applications/search"
USPTO_ODP_PAGE <- 100L
# The search refuses offsets past this; a query with more hits is split into months.
USPTO_ODP_MAX_WINDOW <- 9900L

if (sys.nframe() == 0L && !exists("register_fetcher", mode = "function")) {
  local({
    script <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1])
    root <- normalizePath(file.path(dirname(script), "..", ".."), winslash = "/", mustWork = TRUE)
    options(opportunity_security.repo_root = root)
    source(file.path(root, "scripts", "utils", "fetchers.R"), local = FALSE)
  })
}

uspto_patents_repo_root <- function() {
  for (root in list(getOption("opportunity_security.repo_root"), get0("repo_root", envir = globalenv(), inherits = FALSE))) {
    if (is.character(root) && length(root) == 1 && nzchar(root) && file.exists(file.path(root, "R", "utils", "uspto_patents.R"))) {
      return(root)
    }
  }
  normalizePath(".", winslash = "/", mustWork = TRUE)
}

uspto_patents_load_helpers <- function(root = uspto_patents_repo_root()) {
  source(file.path(root, "R", "utils", "uspto_patents.R"), local = FALSE)
  invisible(root)
}

uspto_odp_key <- function() {
  key <- gsub("^['\"]|['\"]$", "", trimws(Sys.getenv("USPTO_KEY")))
  if (!nzchar(key)) stop("USPTO_KEY is not set. Get a free key at https://data.uspto.gov and add USPTO_KEY=... to your user .Renviron.")
  key
}

# One search request. Returns the parsed body; a 404 is the API's "no matching records" and comes
# back as count 0. Retries 429 and 5xx with backoff.
uspto_odp_post <- function(body, key = uspto_odp_key(), retries = 5) {
  for (pkg in c("curl", "jsonlite")) if (!requireNamespace(pkg, quietly = TRUE)) stop("Package '", pkg, "' is required.")
  payload <- jsonlite::toJSON(body, auto_unbox = TRUE)
  last <- NULL
  for (attempt in seq_len(retries)) {
    h <- curl::new_handle()
    curl::handle_setheaders(h, "X-API-KEY" = key, "Content-Type" = "application/json", "Accept" = "application/json")
    curl::handle_setopt(h, post = TRUE, postfields = as.character(payload), timeout = 180, connecttimeout = 60)
    res <- tryCatch(curl::curl_fetch_memory(USPTO_ODP_SEARCH_URL, handle = h), error = function(e) e)
    if (inherits(res, "error")) {
      last <- conditionMessage(res)
    } else if (res$status_code == 200) {
      return(jsonlite::fromJSON(rawToChar(res$content), simplifyVector = FALSE))
    } else if (res$status_code == 404) {
      return(list(count = 0L, patentFileWrapperDataBag = list()))
    } else {
      last <- paste("HTTP", res$status_code, substr(rawToChar(res$content), 1, 200))
      if (res$status_code < 429 && res$status_code != 408) break
    }
    Sys.sleep(min(60, 5 * 2^(attempt - 1)))
  }
  stop("USPTO ODP request failed: ", last)
}

uspto_odp_count <- function(q, key = uspto_odp_key()) {
  as.integer(uspto_odp_post(list(q = q, pagination = list(offset = 0L, limit = 1L),
                                 fields = list("applicationNumberText")), key)$count)
}

# Every record matching q (only the fields the counts need), splitting into months when a year has
# more hits than the search will page through.
uspto_odp_records <- function(q, year, key = uspto_odp_key(), pause = 0.2) {
  fields <- list("applicationNumberText", "applicationMetaData.inventorBag")
  windows <- list(c(sprintf("%d-01-01", year), sprintf("%d-12-31", year)))
  first <- uspto_odp_count(paste(q, "AND", uspto_grant_clause(windows[[1]][1], windows[[1]][2])), key)
  if (first > USPTO_ODP_MAX_WINDOW) {
    starts <- seq(as.Date(sprintf("%d-01-01", year)), by = "month", length.out = 12)
    ends <- c(starts[-1] - 1, as.Date(sprintf("%d-12-31", year)))
    windows <- Map(function(a, b) c(format(a), format(b)), starts, ends)
  }
  out <- list()
  for (w in windows) {
    wq <- paste(q, "AND", uspto_grant_clause(w[1], w[2]))
    offset <- 0L
    repeat {
      page <- uspto_odp_post(list(q = wq, pagination = list(offset = offset, limit = USPTO_ODP_PAGE), fields = fields), key)
      bag <- page$patentFileWrapperDataBag
      if (length(bag) == 0) break
      out <- c(out, bag)
      offset <- offset + USPTO_ODP_PAGE
      if (offset >= as.integer(page$count)) break
      if (offset > USPTO_ODP_MAX_WINDOW) stop("Query has more hits than the search pages through: ", wq)
      Sys.sleep(pause)
    }
  }
  # A grant can appear in two monthly windows only if its grant date moved; keep one.
  ids <- vapply(out, function(r) if (is.null(r$applicationNumberText)) NA_character_ else r$applicationNumberText, character(1))
  out[!duplicated(ids) | is.na(ids)]
}

uspto_patents_fetch <- function(config_path = file.path(uspto_patents_repo_root(), "config", "patent_tech_cpc.yml"),
                                years = NULL) {
  if (!exists("uspto_cpc_clause", mode = "function")) uspto_patents_load_helpers()
  cfg <- yaml::read_yaml(config_path)
  if (is.null(years)) years <- seq.int(cfg$source$first_grant_year, cfg$source$last_grant_year)
  key <- uspto_odp_key()

  counts <- list()
  for (tech in names(cfg$techs)) {
    tq <- paste(uspto_cpc_clause(unlist(cfg$techs[[tech]]$cpc)), "AND", uspto_utility_clause())
    for (yr in years) {
      recs <- uspto_odp_records(tq, yr, key)
      w <- uspto_records_to_country_weights(recs)
      counts[[length(counts) + 1]] <- uspto_aggregate_counts(w, tech, yr)
      message(sprintf("  uspto_y02_patents: %s %d: %d grants", tech, yr, length(recs)))
    }
  }
  counts <- do.call(rbind, counts)

  # Denominator: all utility grants per inventor country and year (whole counts), and the world.
  countries <- sort(setdiff(unique(counts$inventor_iso2), USPTO_PATENTS_WORLD))
  totals <- list()
  for (yr in years) {
    base <- paste(uspto_utility_clause(), "AND", uspto_year_clause(yr))
    world <- uspto_odp_count(base, key)
    by_country <- vapply(countries, function(cc) {
      Sys.sleep(0.2)
      uspto_odp_count(paste(base, "AND", uspto_inventor_country_clause(cc)), key)
    }, integer(1))
    totals[[length(totals) + 1]] <- data.frame(
      grant_year = as.integer(yr), tech = USPTO_PATENTS_ALL, inventor_iso2 = c(countries, USPTO_PATENTS_WORLD),
      patents_whole = c(by_country, world), patents_fractional = NA_real_, stringsAsFactors = FALSE)
    message(sprintf("  uspto_y02_patents: all utility grants %d: %d (%d countries)", yr, world, length(countries)))
  }
  out <- rbind(counts, do.call(rbind, totals))
  out$fetched_on <- format(Sys.Date())
  out[order(out$tech, out$grant_year, out$inventor_iso2), , drop = FALSE]
}

register_fetcher(
  id = "uspto_y02_patents",
  description = "USPTO ODP: granted US utility patents by Y02/Y04S technology, grant year and inventor country",
  contract = list(
    required_columns = c("grant_year", "tech", "inventor_iso2", "patents_whole", "patents_fractional"),
    min_rows = 1000,
    unique_key = c("grant_year", "tech", "inventor_iso2")
  ),
  fn = function() uspto_patents_fetch()
)

if (sys.nframe() == 0L) {
  local({
    root <- uspto_patents_repo_root()
    config_path <- file.path(root, "config", "config.yml")
    config <- if (file.exists(config_path)) yaml::read_yaml(config_path) else list()
    raw_dir <- file.path(root, if (is.null(config$raw_data_dir)) "data/raw" else config$raw_data_dir)
    uspto_patents_load_helpers(root)
    fetcher <- get_fetcher("uspto_y02_patents")
    dest <- file.path(raw_dir, "uspto_y02_patents.csv")
    df <- fetcher$fn()
    opsi_write_validated(df, dest, fetcher$contract, fetcher$id)
    message("Wrote ", nrow(df), " rows (", length(unique(df$inventor_iso2)), " countries, ",
            min(df$grant_year), "-", max(df$grant_year), ") to ", dest)
  })
}
