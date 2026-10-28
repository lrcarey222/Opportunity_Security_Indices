source(local({
  resolve_bootstrap_path <- function() {
    candidate_starts <- character()
    fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(fa) > 0) candidate_starts <- c(candidate_starts, dirname(sub("^--file=", "", fa[1])))
    sf <- tryCatch(sys.frame(1)$ofile, error = function(e) "")
    if (!is.null(sf) && nzchar(sf)) candidate_starts <- c(candidate_starts, dirname(sf))
    candidate_starts <- unique(c(candidate_starts, getwd()))
    for (start in candidate_starts) {
      d <- normalizePath(start, winslash = "/", mustWork = FALSE)
      while (dirname(d) != d) {
        bootstrap <- file.path(d, "scripts", "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)
        d <- dirname(d)
      }
    }
    stop("Unable to resolve script path for bootstrap.")
  }
  resolve_bootstrap_path()
}))

# Trade refresh pull for Competitive Position's trade evidence (docs/strategic_position.md):
#   annual  : exports to the World of the energy basket and total merchandise exports, 2022-2024, for
#             every reporter of comtrade_total_export.csv -> data/raw/comtrade_benchmark_annual.csv.
#             The structural trade benchmark averages the three years on one basket.
#   monthly : the same exports and monthly totals for the analysis universe (coalition members plus
#             economies with GDP of at least COMTRADE_REFRESH_GDP_MIN), over the same calendar months
#             for everyone -> data/raw/comtrade_momentum_monthly.csv. The momentum module picks a
#             common cutoff from what reporters actually returned.
# Exports only (flow X), partner World, all customs procedures and transport modes, HS as reported.
# Codes = every cmd_code of data/raw/comtrade_energy_trade.csv (the basket the annual EO trade uses),
# plus TOTAL as its own code group.
#
# Calls follow scripts/99: one keyed data-API call per chunk, the raw response kept as a chunk file so
# an interrupted run resumes, a capped or too-long request is split, calls counted against
# COMTRADE_REFRESH_MAX_CALLS per UTC day. The key travels in a header and never reaches a log line.
#
# Env knobs (all optional except COMTRADE_API_KEY for the fetch):
#   COMTRADE_REFRESH_MODE          annual | monthly | both (default both)
#   COMTRADE_REFRESH_STAGE         plan | fetch | combine | all (default all)
#   COMTRADE_REFRESH_YEARS         default 2022,2023,2024
#   COMTRADE_REFRESH_MONTHS        months requested in monthly mode, default 38
#   COMTRADE_REFRESH_GDP_MIN       USD, default 2e11
#   COMTRADE_REFRESH_MAX_CALLS     keyed calls allowed per UTC day, default 250
#   COMTRADE_REFRESH_PAUSE_SECONDS pause before every call, default 2
#   COMTRADE_REFRESH_CODE_GROUPS   basket code groups per call window (query length), default 3
#   COMTRADE_REFRESH_MAX_REPORTERS reporters per call, default 15

config <- getOption("opportunity_security.config")
raw_data_path <- file.path(repo_root, config$raw_data_dir)
processed_dir <- file.path(repo_root, config$processed_dir)

source(file.path(repo_root, "scripts", "utils", "comtrade_client.R"))
source(file.path(repo_root, "scripts", "utils", "comtrade_dyads_utils.R"))
source(file.path(repo_root, "scripts", "utils", "comtrade_monthly_utils.R"))
source(file.path(repo_root, "scripts", "utils", "comtrade_refresh_utils.R"))

env_chr <- function(name, default) {
  v <- Sys.getenv(name, "")
  if (nzchar(v)) v else default
}
mode <- tolower(env_chr("COMTRADE_REFRESH_MODE", "both"))
stage <- tolower(env_chr("COMTRADE_REFRESH_STAGE", "all"))
years <- as.integer(strsplit(env_chr("COMTRADE_REFRESH_YEARS", "2022,2023,2024"), ",")[[1]])
n_months <- as.integer(env_chr("COMTRADE_REFRESH_MONTHS", "38"))
gdp_min <- as.numeric(env_chr("COMTRADE_REFRESH_GDP_MIN", "2e11"))
max_calls_per_day <- as.integer(env_chr("COMTRADE_REFRESH_MAX_CALLS", "250"))
pause_seconds <- as.numeric(env_chr("COMTRADE_REFRESH_PAUSE_SECONDS", "2"))
n_code_groups <- as.integer(env_chr("COMTRADE_REFRESH_CODE_GROUPS", "3"))
max_reporters <- as.integer(env_chr("COMTRADE_REFRESH_MAX_REPORTERS", "15"))
if (!mode %in% c("annual", "monthly", "both")) stop("COMTRADE_REFRESH_MODE must be annual, monthly or both.")
if (!stage %in% c("plan", "fetch", "combine", "all")) stop("COMTRADE_REFRESH_STAGE must be plan, fetch, combine or all.")
retries <- 5L
timeout_seconds <- 600
backoff_seconds <- 15
max_wait_seconds <- 600

chunk_root <- file.path(raw_data_path, "comtrade_chunks", "comtrade_trade_refresh")
dir.create(chunk_root, recursive = TRUE, showWarnings = FALSE)
log_path <- file.path(chunk_root, "log.md")
calls_path <- file.path(chunk_root, "calls.csv")

utc_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
utc_today <- function() format(Sys.time(), "%Y-%m-%d", tz = "UTC")
redact <- function(x) {
  x <- as.character(x)
  key <- Sys.getenv("COMTRADE_API_KEY")
  if (nzchar(key)) x <- gsub(key, "<redacted>", x, fixed = TRUE)
  gsub("[\r\n]+", " ", gsub("[0-9A-Fa-f]{32}", "<redacted>", x))
}
log_line <- function(...) {
  line <- redact(paste0(...))
  cat(line, "\n", file = log_path, append = TRUE, sep = "")
  message(line)
}
calls_log <- function() {
  if (!file.exists(calls_path)) return(data.frame(utc_date = character()))
  utils::read.csv(calls_path, stringsAsFactors = FALSE)
}
record_call <- function(id, attempt, status, rows = NA_integer_) {
  row <- data.frame(utc_time = utc_now(), utc_date = utc_today(), chunk_id = id, attempt = attempt,
                    status = redact(status), rows = rows)
  utils::write.table(row, calls_path, sep = ",", row.names = FALSE,
                     col.names = !file.exists(calls_path), append = file.exists(calls_path))
}
calls_today <- function() sum(calls_log()$utc_date == utc_today())

# --- Basket and reporters ---------------------------------------------------------------------
energy_trade <- data.table::fread(file.path(raw_data_path, "comtrade_energy_trade.csv"),
                                  select = c("cmd_code", "flow_code"), colClasses = list(character = "cmd_code"))
basket <- sort(unique(energy_trade$cmd_code[energy_trade$flow_code == "X"]))
basket <- basket[nchar(basket) == 6]
basket_groups <- comtrade_dyads_code_groups(basket, n_code_groups)
code_groups <- c(basket_groups, list("TOTAL"))
totals <- data.table::fread(file.path(raw_data_path, "comtrade_total_export.csv"), select = "reporter_iso")
annual_reporters <- sort(unique(as.character(totals$reporter_iso)))
reporter_ref <- comtradr::ct_get_ref_table("reporter")

read_da_monthly <- function(path) {
  if (!file.exists(path)) {
    this_year <- as.integer(format(Sys.Date(), "%Y"))
    periods <- as.vector(outer((this_year - 3L):this_year, 1:12, function(y, m) y * 100L + m))
    batches <- split(sort(periods), ceiling(seq_along(periods) / 12))
    parts <- lapply(batches, function(p) {
      req <- httr2::req_timeout(httr2::req_url_query(
        httr2::request("https://comtradeapi.un.org/public/v1/getDA/C/M/HS"), period = paste(p, collapse = ",")), 120)
      jsonlite::fromJSON(httr2::resp_body_string(httr2::req_perform(req)))$data
    })
    jsonlite::write_json(do.call(rbind, parts[vapply(parts, NROW, 1L) > 0]), path, digits = NA)
  }
  da <- jsonlite::fromJSON(path)
  data.frame(reporter_iso = as.character(da$reporterISO), period = as.integer(da$period), stringsAsFactors = FALSE)
}

plans <- list()
if (mode %in% c("annual", "both")) {
  dir <- file.path(chunk_root, "annual")
  dir.create(dir, showWarnings = FALSE)
  plan_path <- file.path(dir, "plan.csv")
  if (!file.exists(plan_path)) {
    plan <- comtrade_refresh_plan("annual", annual_reporters, list(years), length(code_groups), max_reporters)
    utils::write.csv(plan, plan_path, row.names = FALSE)
    log_line("## Annual plan (", utc_now(), "): ", length(annual_reporters), " reporters, years ",
             paste(years, collapse = ","), ", ", length(basket), " codes + TOTAL, ", nrow(plan), " calls")
  }
  plans$annual <- list(dir = dir, plan = utils::read.csv(plan_path, stringsAsFactors = FALSE), freq = "A")
}
if (mode %in% c("monthly", "both")) {
  dir <- file.path(chunk_root, "monthly")
  dir.create(dir, showWarnings = FALSE)
  plan_path <- file.path(dir, "plan.csv")
  if (!file.exists(plan_path)) {
    coalition <- readRDS(file.path(processed_dir, "coalition_tbl.rds"))
    gdp_raw <- utils::read.csv(file.path(raw_data_path, "wdi_gdp.csv"), stringsAsFactors = FALSE)
    gdp <- gdp_raw[!is.na(gdp_raw$NY.GDP.MKTP.CD), ]
    gdp <- gdp[order(gdp$iso3c, -gdp$year), ]
    gdp <- gdp[!duplicated(gdp$iso3c), ]
    gdp <- data.frame(iso3 = gdp$iso3c, gdp_usd = gdp$NY.GDP.MKTP.CD)
    gdp <- gdp[gdp$iso3 %in% reporter_ref$iso_3, ]  # drops WDI aggregates
    universe <- comtrade_refresh_universe(coalition$iso3[coalition$tier %in% c("core", "partner")], gdp, gdp_min)
    da <- read_da_monthly(file.path(dir, "getDA_monthly.json"))
    months <- comtrade_refresh_months(da, universe, n_months)
    plan <- comtrade_refresh_plan("monthly", universe, comtrade_refresh_period_windows(months, 12L),
                                  length(code_groups), max_reporters)
    utils::write.csv(plan, plan_path, row.names = FALSE)
    utils::write.csv(data.frame(iso3 = universe), file.path(dir, "universe.csv"), row.names = FALSE)
    log_line("## Monthly plan (", utc_now(), "): ", length(universe), " reporters, months ", min(months), "-",
             max(months), ", ", length(basket), " codes + TOTAL, ", nrow(plan), " calls")
  }
  plans$monthly <- list(dir = dir, plan = utils::read.csv(plan_path, stringsAsFactors = FALSE), freq = "M")
}

# --- Fetch ------------------------------------------------------------------------------------
period_str <- function(p, freq) if (freq == "A") as.character(p) else sprintf("%04d-%02d", p %/% 100L, p %% 100L)

# Comtrade lists Taiwan as "Other Asia, nes" (reporter 490, S19); the trade files call it TWN.
COMTRADE_ISO_ALIAS <- c(TWN = "S19")
to_comtrade_iso <- function(iso) ifelse(iso %in% names(COMTRADE_ISO_ALIAS), COMTRADE_ISO_ALIAS[iso], iso)

refresh_call <- function(reporters, chunk_codes, periods, freq) {
  params <- comtradr:::ct_check_params(
    type = "goods", frequency = freq, commodity_classification = "HS", commodity_code = chunk_codes,
    flow_direction = "export", reporter = unname(to_comtrade_iso(reporters)), partner = "World",
    start_date = period_str(min(periods), freq), end_date = period_str(max(periods), freq),
    mode_of_transport = "TOTAL modes of transport", partner_2 = "World", customs_code = "C00",
    update = FALSE, verbose = FALSE, extra_params = NULL, bulk = FALSE
  )
  req <- comtradr:::ct_build_request(params, primary_token = comtradr::get_primary_comtrade_key(),
                                     verbose = FALSE, bulk = FALSE)
  req <- httr2::req_error(httr2::req_timeout(req, timeout_seconds), is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req)
  status <- as.integer(httr2::resp_status(resp))
  if (status == 200L) {
    return(list(status = status, data = suppressMessages(
      comtradr:::ct_process_response(resp, verbose = FALSE, tidy_cols = TRUE, bulk = FALSE))))
  }
  retry_after <- suppressWarnings(as.numeric(httr2::resp_header(resp, "Retry-After")))
  body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
  list(status = status, retry_after = if (length(retry_after) == 1) retry_after else NA_real_,
       message = substr(gsub("\\s+", " ", body), 1, 300))
}

fetch_chunk <- function(dir, id, reporters, chunk_codes, periods, freq) {
  f <- function(ext) file.path(dir, paste0(id, ext))
  if (file.exists(f(".csv.gz")) || file.exists(f(".empty"))) return("done")
  split_fetch <- function(reason) {
    log_line("- ", utc_now(), " | ", id, " | ", reason, ", splitting")
    h <- comtrade_dyads_split(reporters, chunk_codes)
    a <- fetch_chunk(dir, paste0(id, "a"), h$a$reporters, h$a$codes, periods, freq)
    b <- fetch_chunk(dir, paste0(id, "b"), h$b$reporters, h$b$codes, periods, freq)
    if (all(c(a, b) == "done")) "done" else if ("budget" %in% c(a, b)) "budget" else "failed"
  }
  ids <- reporter_ref$id[reporter_ref$iso_3 %in% to_comtrade_iso(reporters)]
  if (comtrade_dyads_query_chars(ids, chunk_codes) > COMTRADE_DYADS_MAX_QUERY_CHARS) return(split_fetch("query string too long"))
  out <- NULL
  for (attempt in seq_len(retries)) {
    if (calls_today() >= max_calls_per_day) {
      log_line("- ", utc_now(), " | budget of ", max_calls_per_day, " calls reached before ", id)
      return("budget")
    }
    Sys.sleep(pause_seconds)
    res <- tryCatch(refresh_call(reporters, chunk_codes, periods, freq), error = function(e) e)
    if (!inherits(res, "error") && identical(res$status, 200L)) {
      record_call(id, attempt, "ok", if (comtrade_dyads_is_empty(res$data)) 0L else nrow(res$data))
      out <- res$data
      break
    }
    status <- if (inherits(res, "error")) NA_integer_ else res$status
    msg <- if (inherits(res, "error")) comtrade_error_message(res) else paste0("HTTP ", status, ": ", res$message)
    record_call(id, attempt, substr(msg, 1, 200))
    log_line("- ", utc_now(), " | ", id, " attempt ", attempt, " failed: ", substr(msg, 1, 200))
    if (grepl("quota", msg, ignore.case = TRUE)) return("budget")
    if (identical(status, 401L) || identical(status, 403L)) stop("Comtrade refused the request; stopping.")
    if (identical(status, 414L)) return(split_fetch("HTTP 414"))
    if (!is.na(status) && status >= 400 && status < 500 && status != 429L) break
    if (identical(status, 429L) && !is.na(res$retry_after) && res$retry_after > max_wait_seconds) return("budget")
    if (attempt < retries) {
      wait <- if (identical(status, 429L) && !is.na(res$retry_after)) res$retry_after + 1 else backoff_seconds * 2^(attempt - 1)
      Sys.sleep(min(wait, max_wait_seconds))
    }
  }
  if (is.null(out)) {
    log_line("- ", utc_now(), " | ", id, " FAILED; rerun to retry")
    return("failed")
  }
  if (comtrade_dyads_is_empty(out)) {
    writeLines(utc_now(), f(".empty"))
    return("done")
  }
  if (comtrade_dyads_is_truncated(nrow(out))) return(split_fetch("hit the record cap"))
  tmp <- f(".tmp.csv.gz")
  data.table::fwrite(out, tmp)
  file.rename(tmp, f(".csv.gz"))
  log_line("- ", utc_now(), " | call ", calls_today(), " today | ", id, " | ", length(reporters), " reporters, ",
           length(chunk_codes), " codes | ", format(nrow(out), big.mark = ","), " rows")
  "done"
}

if (stage %in% c("fetch", "all")) {
  comtrade_set_key_from_env()
  log_line("## Fetch session ", utc_now(), " (", calls_today(), " calls already used today, budget ", max_calls_per_day, ")")
  for (nm in names(plans)) {
    p <- plans[[nm]]
    states <- character(nrow(p$plan))
    for (i in seq_len(nrow(p$plan))) {
      periods <- as.integer(strsplit(p$plan$periods[i], ",")[[1]])
      states[i] <- fetch_chunk(p$dir, p$plan$chunk_id[i], strsplit(p$plan$reporters[i], " ", fixed = TRUE)[[1]],
                               code_groups[[p$plan$code_group[i]]], periods, p$freq)
      if (states[i] == "budget") break
    }
    log_line("- ", nm, ": ", sum(states == "done"), "/", nrow(p$plan), " planned chunks done")
    if (any(states == "budget")) break
  }
}

# --- Combine ----------------------------------------------------------------------------------
chunk_files <- function(dir, id) {
  for (ext in c(".csv.gz", ".empty")) if (file.exists(file.path(dir, paste0(id, ext)))) return(file.path(dir, paste0(id, ext)))
  a <- chunk_files(dir, paste0(id, "a"))
  b <- chunk_files(dir, paste0(id, "b"))
  if (is.null(a) || is.null(b)) NULL else c(a, b)
}
if (stage %in% c("combine", "all")) {
  outputs <- c(annual = "comtrade_benchmark_annual.csv", monthly = "comtrade_momentum_monthly.csv")
  for (nm in names(plans)) {
    p <- plans[[nm]]
    leaves <- lapply(p$plan$chunk_id, function(id) chunk_files(p$dir, id))
    if (any(vapply(leaves, is.null, logical(1)))) {
      log_line("- ", nm, ": not all chunks fetched yet; rerun the fetch")
      next
    }
    files <- grep("\\.csv\\.gz$", unlist(leaves), value = TRUE)
    keep <- c("period", "ref_year", "reporter_iso", "flow_code", "partner_code", "classification_code", "cmd_code", "primary_value", "net_wgt")
    raw <- data.table::rbindlist(lapply(files, function(f) {
      d <- data.table::fread(f, colClasses = list(character = c("cmd_code", "period")))
      d[, intersect(keep, names(d)), with = FALSE]
    }), fill = TRUE)
    raw <- raw[raw$partner_code == 0 & raw$flow_code == "X", ]
    for (a in names(COMTRADE_ISO_ALIAS)) raw$reporter_iso[raw$reporter_iso == COMTRADE_ISO_ALIAS[[a]]] <- a
    key <- paste(raw$reporter_iso, raw$period, raw$cmd_code)
    if (anyDuplicated(key)) stop(nm, ": duplicate (reporter, period, code) rows: ", sum(duplicated(key)))
    out_path <- file.path(raw_data_path, outputs[[nm]])
    data.table::fwrite(raw[order(raw$reporter_iso, raw$cmd_code, raw$period), ], out_path)
    log_line("- ", nm, ": wrote ", out_path, " (", format(nrow(raw), big.mark = ","), " rows, ",
             length(unique(raw$reporter_iso)), " reporters)")
  }
}
