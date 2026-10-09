source(local({
  resolve_bootstrap_path <- function() {
    candidate_starts <- character()

    sf <- tryCatch(sys.frame(1)$ofile, error = function(e) "")
    if (!is.null(sf) && nzchar(sf)) candidate_starts <- c(candidate_starts, dirname(sf))

    frame_ofiles <- vapply(sys.frames(), function(fr) {
      val <- tryCatch(fr$ofile, error = function(e) "")
      if (is.null(val) || !nzchar(val)) "" else dirname(val)
    }, character(1))
    candidate_starts <- c(candidate_starts, frame_ofiles[nzchar(frame_ofiles)])

    fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(fa) > 0) candidate_starts <- c(candidate_starts, dirname(sub("^--file=", "", fa[1])))

    candidate_starts <- unique(c(candidate_starts, getwd()))

    for (start in candidate_starts) {
      d <- normalizePath(start, winslash = "/", mustWork = FALSE)
      while (dirname(d) != d) {
        bootstrap <- file.path(d, "scripts", "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)

        bootstrap <- file.path(d, "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)

        d <- dirname(d)
      }
    }

    stop("Unable to resolve script path for bootstrap.")
  }

  resolve_bootstrap_path()
}))

# All-reporter UN Comtrade pull of the energy HS6 basket by partner:
# data/raw/comtrade_energy_dyads_all.csv.
#
# Why: the partnership themes (scripts/15_build_partner_themes.R) score reporter x partner trade
# by technology and supply-chain stage. allied_comtrade_energy_data.csv has 22 reporters, so every
# other dyad's trade term was imputed. This pull asks Comtrade for every reporter it has in the
# theme years, all partners, imports and exports, so most dyads are observed directly or as a
# mirror (j's reported exports to i for i's imports from j).
#
# Shape: the allied file's columns (comtradr tidy names; API totals over customs procedure, mode
# of transport and second partner) plus `classification_requested` ("HS" = each reporter's HS
# edition as reported, H3-H6 in classification_code) and `cmd_code_legacy_hs` (basket bridge code).
# Partners and reporters are WDI economies Comtrade knows (scripts/utils/comtrade_dyads_utils.R)
# plus Taiwan, which Comtrade codes S19 ("Other Asia, nes") and the combine step recodes to TWN;
# World, the other "nes" areas, bunkers and free zones are dropped.
#
# Calls: one data-API call per batch of reporters x year x flow, all basket codes, all partners,
# batches packed to stay under the free tier's 100,000-record cap (a capped response is split and
# repeated). Data availability (which reporters have each year) comes from the keyless public
# getDA endpoint. Each call's raw response is kept as one chunk file, so an interrupted run
# resumes without repeating calls; every attempt is counted against COMTRADE_DYADS_MAX_CALLS per
# UTC day.
#
# Codes added to the basket after the main pull are fetched as a supplementary pull of just those
# codes (COMTRADE_DYADS_CODES) into a chunk directory of its own, with the main pull's getDA files
# copied in so the reporters are the same, and combined with the main directory
# (COMTRADE_DYADS_COMBINE_DIRS). The supplementary chunks also carry each reporter's World row,
# which the energy_trade stage adds to comtrade_energy_trade.csv for the codes that file lacks
# (the Electric Grid Upstream copper and aluminium codes were added this way on 2026-10-09).
#
# Env knobs (all optional except COMTRADE_API_KEY):
#   COMTRADE_DYADS_STAGE           plan | fetch | combine | all (default all), or energy_trade:
#                                  add the World totals in this chunk directory to
#                                  comtrade_energy_trade.csv for codes it does not hold (its year
#                                  and, per flow, its reporters; no API calls)
#   COMTRADE_DYADS_CODES           comma-separated HS6 codes of the basket: plan a supplementary
#                                  pull of only these codes (new chunk directory), default all
#   COMTRADE_DYADS_COMBINE_DIRS    comma-separated further chunk directories (each with its own
#                                  plan.csv and codes.txt, codes disjoint) combined with this one;
#                                  together they must hold every basket code, or combine stops
#   COMTRADE_DYADS_OUTPUT          combined file, default <raw_data_dir>/comtrade_energy_dyads_all.csv
#   COMTRADE_DYADS_YEARS           default 2020,2024 (the first and last year the themes use)
#   COMTRADE_DYADS_FLOWS           default import,export (imports are fetched first)
#   COMTRADE_DYADS_CHUNK_DIR       default <raw_data_dir>/comtrade_chunks/comtrade_energy_dyads_all
#   COMTRADE_DYADS_LOG             progress log (markdown), default <chunk dir>/log.md
#   COMTRADE_DYADS_MAX_CALLS       keyed calls allowed per day, default 350 (the free tier allows
#                                  ~500 and the key may be shared); counted per UTC day and per
#                                  local calendar day, whichever is higher
#   COMTRADE_DYADS_PAUSE_SECONDS   pause before every call, default 2
#   COMTRADE_DYADS_TARGET_RECORDS  estimated records per call, default 85000
#   COMTRADE_DYADS_MAX_REPORTERS   reporters per call (query length), default 50
#   COMTRADE_DYADS_CODE_GROUPS     basket code groups (query length), default 2
#   COMTRADE_DYADS_REPLAN          true: re-pack the calls not yet made with the current estimate
#                                  (fetched chunks are kept), default false
#   COMTRADE_DYADS_ALLOW_PARTIAL   true: combine the chunks fetched so far even if some are
#                                  missing, or basket codes no chunk directory holds (the log
#                                  lists them), default false
#   COMTRADE_DYADS_TAIWAN          TWN | drop: Comtrade's "Other Asia, nes" (S19, Taiwan) as a
#                                  reporter and partner, recoded to TWN or dropped; default TWN
#                                  (WDI has no Taiwan, so dropping it loses Taiwan's flows)
#   COMTRADE_DYADS_MAX_WAIT_SECONDS longest HTTP 429 Retry-After honoured before the session
#                                  stops (also caps the backoff), default 600
#   COMTRADE_MAX_RETRIES, COMTRADE_REQUEST_TIMEOUT_SECONDS, COMTRADE_SLEEP_SECONDS (backoff base)

config <- getOption("opportunity_security.config")
raw_data_path <- file.path(repo_root, config$raw_data_dir)
if (!dir.exists(raw_data_path)) dir.create(raw_data_path, recursive = TRUE)

source(file.path(repo_root, "scripts", "utils", "raw_inputs.R"))
source(file.path(repo_root, "scripts", "utils", "comtrade_client.R"))
source(file.path(repo_root, "scripts", "utils", "comtrade_ingest_utils.R"))
source(file.path(repo_root, "scripts", "utils", "comtrade_dyads_utils.R"))

env_chr <- function(name, default) {
  v <- Sys.getenv(name, "")
  if (nzchar(v)) v else default
}
env_list <- function(name, default) trimws(strsplit(env_chr(name, default), ",", fixed = TRUE)[[1]])

stage <- tolower(env_chr("COMTRADE_DYADS_STAGE", "all"))
if (!stage %in% c("plan", "fetch", "combine", "all", "energy_trade")) {
  stop("COMTRADE_DYADS_STAGE must be plan, fetch, combine, all or energy_trade.")
}
dyad_years <- as.integer(env_list("COMTRADE_DYADS_YEARS", "2020,2024"))
dyad_flows <- tolower(env_list("COMTRADE_DYADS_FLOWS", "import,export"))
if (!all(dyad_flows %in% c("import", "export"))) stop("COMTRADE_DYADS_FLOWS must list import and/or export.")
chunk_dir <- env_chr("COMTRADE_DYADS_CHUNK_DIR", file.path(raw_data_path, "comtrade_chunks", "comtrade_energy_dyads_all"))
log_path <- env_chr("COMTRADE_DYADS_LOG", file.path(chunk_dir, "log.md"))
max_calls_per_day <- as.integer(env_chr("COMTRADE_DYADS_MAX_CALLS", "350"))
pause_seconds <- as.numeric(env_chr("COMTRADE_DYADS_PAUSE_SECONDS", "2"))
target_records <- as.numeric(env_chr("COMTRADE_DYADS_TARGET_RECORDS", "85000"))
max_reporters <- as.integer(env_chr("COMTRADE_DYADS_MAX_REPORTERS", "50"))
n_code_groups <- as.integer(env_chr("COMTRADE_DYADS_CODE_GROUPS", "2"))
replan <- toupper(env_chr("COMTRADE_DYADS_REPLAN", "false")) %in% c("TRUE", "1", "YES")
allow_partial <- toupper(env_chr("COMTRADE_DYADS_ALLOW_PARTIAL", "false")) %in% c("TRUE", "1", "YES")
taiwan_mode <- env_chr("COMTRADE_DYADS_TAIWAN", "TWN")
if (!taiwan_mode %in% c("TWN", "drop")) stop("COMTRADE_DYADS_TAIWAN must be TWN or drop.")
retries <- max(1L, as.integer(env_chr("COMTRADE_MAX_RETRIES", "5")))
timeout_seconds <- as.numeric(env_chr("COMTRADE_REQUEST_TIMEOUT_SECONDS", "600"))
backoff_seconds <- as.numeric(env_chr("COMTRADE_SLEEP_SECONDS", "15"))
max_wait_seconds <- as.numeric(env_chr("COMTRADE_DYADS_MAX_WAIT_SECONDS", "600"))

dyads_output_path <- env_chr("COMTRADE_DYADS_OUTPUT", file.path(raw_data_path, "comtrade_energy_dyads_all.csv"))
combine_dirs <- setdiff(env_list("COMTRADE_DYADS_COMBINE_DIRS", ""), "")
energy_trade_path <- file.path(raw_data_path, "comtrade_energy_trade.csv")
dir.create(chunk_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)

utc_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
utc_today <- function() format(Sys.time(), "%Y-%m-%d", tz = "UTC")

# Never let the key reach a log line: drop it and anything shaped like a Comtrade key.
redact <- function(x) {
  x <- as.character(x)
  key <- Sys.getenv("COMTRADE_API_KEY")
  if (nzchar(key)) x <- gsub(key, "<redacted>", x, fixed = TRUE)
  x <- gsub("[0-9A-Fa-f]{32}", "<redacted>", x)
  gsub("[\r\n]+", " ", x)
}

log_line <- function(...) {
  line <- redact(paste0(...))
  cat(line, "\n", file = log_path, append = TRUE, sep = "")
  message(line)
}

# --- Inputs ---------------------------------------------------------------------------------
country_info <- utils::read.csv(file.path(raw_data_path, "wdi_country_info.csv"), stringsAsFactors = FALSE)
partner_ref <- comtradr::ct_get_ref_table("partner")
reporter_ref <- comtradr::ct_get_ref_table("reporter")
economies <- comtrade_dyads_economies(country_info, partner_ref)

reference_master_path <- file.path(raw_inputs_reference_dir(repo_root), "energy_hs6_master.csv")
hs6_view_path <- file.path(raw_data_path, "energy_hs6_master.csv")
basket <- comtrade_dyads_basket(list(
  if (file.exists(reference_master_path)) utils::read.csv(reference_master_path, colClasses = "character"),
  if (file.exists(hs6_view_path)) utils::read.csv(hs6_view_path, colClasses = "character")
))
if (length(basket$codes) == 0) stop("No HS6 codes found in the energy HS6 master.")
pull_codes <- comtrade_dyads_select_codes(basket$codes, env_list("COMTRADE_DYADS_CODES", ""))

# --- Call accounting ------------------------------------------------------------------------
plan_path <- file.path(chunk_dir, "plan.csv")
codes_path <- file.path(chunk_dir, "codes.txt")
calls_path <- file.path(chunk_dir, "calls.csv")

calls_log <- function() {
  if (!file.exists(calls_path)) {
    return(data.frame(utc_time = character(), utc_date = character(), chunk_id = character(),
                      attempt = integer(), status = character(), rows = integer()))
  }
  utils::read.csv(calls_path, stringsAsFactors = FALSE)
}
record_call <- function(id, attempt, status, rows = NA_integer_) {
  row <- data.frame(utc_time = utc_now(), utc_date = utc_today(), chunk_id = id,
                    attempt = attempt, status = redact(status), rows = rows)
  utils::write.table(row, calls_path, sep = ",", row.names = FALSE,
                     col.names = !file.exists(calls_path), append = file.exists(calls_path))
}
# Calls made "today": the higher of the UTC-day count (Comtrade's quota day) and the local
# calendar-day count, so a budget shared with another session holds on either reading.
calls_today <- function() {
  log <- calls_log()
  if (nrow(log) == 0) return(0L)
  local_date <- format(as.POSIXct(log$utc_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "%Y-%m-%d", tz = "")
  max(sum(log$utc_date == utc_today()), sum(local_date == format(Sys.time(), "%Y-%m-%d")))
}

# --- Plan -----------------------------------------------------------------------------------
# Data availability: the keyless public endpoint first. It has its own small quota (HTTP 403
# "Quota Exceeded"), so on refusal the keyed endpoint is used and counted as a call.
da_public_endpoint <- "https://comtradeapi.un.org/public/v1/getDA/C/A/HS"
da_keyed_endpoint <- "https://comtradeapi.un.org/data/v1/getDA/C/A/HS"

read_da <- function(year) {
  path <- file.path(chunk_dir, sprintf("getDA_%d.json", year))
  if (!file.exists(path)) {
    req <- httr2::req_timeout(httr2::req_url_query(httr2::request(da_public_endpoint), period = year), 120)
    resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
    if (inherits(resp, "error")) {
      key <- Sys.getenv("COMTRADE_API_KEY")
      if (!nzchar(key)) stop("Public getDA refused (", redact(conditionMessage(resp)), ") and COMTRADE_API_KEY is not set.")
      keyed <- httr2::req_headers(
        httr2::req_timeout(httr2::req_url_query(httr2::request(da_keyed_endpoint), period = year), 120),
        `Ocp-Apim-Subscription-Key` = key
      )
      resp <- tryCatch(httr2::req_perform(keyed), error = function(e) e)
      record_call(sprintf("getDA_%d", year), 1L, if (inherits(resp, "error")) "request_error" else "ok")
      if (inherits(resp, "error")) stop("getDA failed for ", year, ": ", redact(conditionMessage(resp)))
    }
    writeBin(httr2::resp_body_raw(resp), path)
  }
  da <- jsonlite::fromJSON(path)$data
  ext_flag <- function(col) if (col %in% names(da)) as.logical(da[[col]]) else rep(FALSE, nrow(da))
  extended <- ext_flag("isExtendedPartner2Code") | ext_flag("isExtendedCustomsCode") | ext_flag("isExtendedMotCode")
  data.frame(
    year = as.integer(da$period),
    reporter_code = as.integer(da$reporterCode),
    reporter_iso = as.character(da$reporterISO),
    classification_code = as.character(da$classificationCode),
    total_records = as.numeric(da$totalRecords),
    extended = !is.na(extended) & extended,
    last_released = as.character(da$lastReleased),
    stringsAsFactors = FALSE
  )
}

plan_estimates <- function() {
  da <- do.call(rbind, lapply(dyad_years, read_da))
  reporters <- comtrade_dyads_reporters(da, economies, reporter_ref$iso_3, include_taiwan = TRUE)
  utils::write.csv(reporters, file.path(chunk_dir, "reporters.csv"), row.names = FALSE)
  estimates <- comtrade_dyads_estimate(reporters, dyad_flows)
  # The estimate is for the whole basket; a supplementary pull of a few codes scales it by their
  # share of the codes (the record cap still splits a call that comes back full).
  estimates$est <- estimates$est * length(pull_codes) / length(basket$codes)
  list(da = da, reporters = reporters, estimates = estimates)
}

log_plan <- function(plan, heading) {
  log_line("")
  log_line("## ", heading, " (", utc_now(), ")")
  log_line("- ", nrow(plan), " planned calls, ", format(sum(plan$est_records), big.mark = ","),
           " estimated records (near upper bound), target ", format(target_records, big.mark = ","), " per call")
  by <- stats::aggregate(est_records ~ year + flow, data = plan, FUN = length)
  log_line("- calls by year and flow: ", paste(paste0(by$year, " ", by$flow, " ", by$est_records), collapse = "; "))
}

if (!file.exists(plan_path)) {
  pe <- plan_estimates()
  plan <- comtrade_dyads_plan(pe$estimates, target_records = target_records, max_reporters = max_reporters,
                              n_groups = n_code_groups)
  utils::write.csv(plan, plan_path, row.names = FALSE)
  writeLines(pull_codes, codes_path)
  dropped <- setdiff(unique(pe$da$reporter_iso), pe$reporters$reporter_iso)
  log_plan(plan, "Plan")
  log_line("- years ", paste(dyad_years, collapse = ", "), "; flows ", paste(dyad_flows, collapse = ", "),
           "; ", length(pull_codes), " HS6 codes (", sum(pull_codes %in% basket$legacy), " legacy bridge codes)",
           if (length(pull_codes) < length(basket$codes)) paste0(" of the ", length(basket$codes), "-code basket: ",
                                                                 paste(pull_codes, collapse = " ")),
           "; ", length(economies), " WDI economies as partners")
  for (y in dyad_years) {
    log_line("- ", y, ": ", sum(pe$reporters$year == y), " reporters with data (getDA)")
  }
  log_line("- getDA reporters left out (aggregates or outside WDI): ", paste(sort(dropped), collapse = " "))
} else if (replan) {
  old <- utils::read.csv(plan_path, stringsAsFactors = FALSE)
  if (!identical(readLines(codes_path), pull_codes)) {
    stop("The HS6 basket changed since the plan; pull the new codes into a new chunk directory (COMTRADE_DYADS_CODES).")
  }
  started <- vapply(old$chunk_id, function(id) {
    any(file.exists(file.path(chunk_dir, paste0(id, c(".csv.gz", ".empty", ".split")))))
  }, logical(1))
  pe <- plan_estimates()
  tags <- setdiff(letters, c("a", "b", unique(substr(sub("^.*_g[0-9]+_", "", old$chunk_id), 1, 1))))
  plan <- comtrade_dyads_replan(old, started, pe$estimates, id_tag = tags[1], target_records = target_records,
                                max_reporters = max_reporters, n_groups = n_code_groups)
  file.copy(plan_path, file.path(chunk_dir, paste0("plan_superseded_", format(Sys.time(), "%Y%m%dT%H%M%S"), ".csv")))
  utils::write.csv(plan, plan_path, row.names = FALSE)
  log_plan(plan, paste0("Re-plan, ", sum(started), " started chunk(s) kept, new ids tagged '", tags[1], "'"))
}
plan <- utils::read.csv(plan_path, stringsAsFactors = FALSE)
codes <- readLines(codes_path)
code_groups <- comtrade_dyads_code_groups(codes, max(plan$code_group))
plan_reporters <- function(i) strsplit(plan$reporters[i], " ", fixed = TRUE)[[1]]
plan_codes <- function(i) code_groups[[plan$code_group[i]]]

# --- Fetch ----------------------------------------------------------------------------------
chunk_file <- function(id, ext, dir = chunk_dir) file.path(dir, paste0(id, ext))

# Leaf status of a chunk: done (data or empty), split (children), or pending.
chunk_state <- function(id, dir = chunk_dir) {
  if (file.exists(chunk_file(id, ".csv.gz", dir)) || file.exists(chunk_file(id, ".empty", dir))) return("done")
  if (file.exists(chunk_file(id, ".split", dir))) return("split")
  "pending"
}

# One data-API request with no hidden retry. comtradr::ct_get_data retries a 403/429 once after
# the Retry-After header, which for a spent allowance is hours away, and sleeps through it inside
# the call (an earlier session stalled 26 minutes there). Here each HTTP attempt returns to the
# loop in fetch_chunk, which counts it and honours Retry-After itself. The request is built and
# the response parsed by comtradr's own functions, so the rows match ct_get_data's (totals over
# customs procedure, mode of transport and second partner; tidy column names). The key travels
# in a request header, never in the URL.
dyads_call <- function(reporters, chunk_codes, year, flow) {
  params <- comtradr:::ct_check_params(
    type = "goods", frequency = "A", commodity_classification = "HS", commodity_code = chunk_codes,
    flow_direction = flow, reporter = reporters, partner = "everything", start_date = year, end_date = year,
    mode_of_transport = "TOTAL modes of transport", partner_2 = "World", customs_code = "C00",
    update = FALSE, verbose = FALSE, extra_params = NULL, bulk = FALSE
  )
  req <- comtradr:::ct_build_request(params, primary_token = comtradr::get_primary_comtrade_key(),
                                     verbose = FALSE, bulk = FALSE)
  req <- httr2::req_error(httr2::req_timeout(req, timeout_seconds), is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req)
  status <- as.integer(httr2::resp_status(resp))
  if (status == 200L) {
    data <- suppressMessages(comtradr:::ct_process_response(resp, verbose = FALSE, tidy_cols = TRUE, bulk = FALSE))
    return(list(status = status, data = data))
  }
  retry_after <- suppressWarnings(as.numeric(httr2::resp_header(resp, "Retry-After")))
  body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
  list(status = status, retry_after = if (length(retry_after) == 1) retry_after else NA_real_,
       message = substr(gsub("\\s+", " ", body), 1, 300))
}

fetch_chunk <- function(id, year, flow, reporters, chunk_codes) {
  state <- chunk_state(id)
  if (state == "done") return("done")
  if (state == "split") {
    halves <- comtrade_dyads_split(reporters, chunk_codes)
    a <- fetch_chunk(paste0(id, "a"), year, flow, halves$a$reporters, halves$a$codes)
    b <- fetch_chunk(paste0(id, "b"), year, flow, halves$b$reporters, halves$b$codes)
    return(if (all(c(a, b) == "done")) "done" else if ("budget" %in% c(a, b)) "budget" else "failed")
  }

  split_and_fetch <- function(reason) {
    writeLines(paste(utc_now(), reason), chunk_file(id, ".split"))
    log_line("- ", utc_now(), " | ", id, " | ", length(reporters), " reporters, ", length(chunk_codes),
             " codes | ", reason, ", splitting")
    fetch_chunk(id, year, flow, reporters, chunk_codes)
  }

  # Split before calling when the query string would exceed the gateway's limit (HTTP 414).
  reporter_ids <- reporter_ref$id[reporter_ref$iso_3 %in% reporters]
  if (comtrade_dyads_query_chars(reporter_ids, chunk_codes) > COMTRADE_DYADS_MAX_QUERY_CHARS) {
    return(split_and_fetch("query string too long"))
  }

  out <- NULL
  n_server_timeouts <- 0L
  for (attempt in seq_len(retries)) {
    if (calls_today() >= max_calls_per_day) {
      log_line("- ", utc_now(), " | budget of ", max_calls_per_day, " calls for ", utc_today(), " reached before ", id)
      return("budget")
    }
    Sys.sleep(pause_seconds)
    t0 <- Sys.time()
    res <- tryCatch(dyads_call(reporters, chunk_codes, year, flow), error = function(e) e)
    secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
    if (!inherits(res, "error") && identical(res$status, 200L)) {
      rows <- if (comtrade_dyads_is_empty(res$data)) 0L else nrow(res$data)
      record_call(id, attempt, "ok", rows)
      out <- res$data
      break
    }
    status <- if (inherits(res, "error")) NA_integer_ else res$status
    msg <- if (inherits(res, "error")) comtrade_error_message(res) else paste0("HTTP ", status, ": ", res$message)
    kind <- if (inherits(res, "error")) comtrade_classify_error(res) else paste0("http_", status)
    retry_after <- if (inherits(res, "error")) NA_real_ else res$retry_after
    record_call(id, attempt, paste(kind, substr(msg, 1, 200)))
    log_line("- ", utc_now(), " | ", id, " attempt ", attempt, " failed after ", secs, "s (", kind,
             if (!is.na(retry_after)) paste0(", Retry-After ", retry_after, "s"), "): ", substr(msg, 1, 200))
    # The daily-quota refusal is an HTTP 403 (with a Retry-After of hours), so test for it before
    # treating 401/403 as a bad key.
    if (grepl("quota", msg, ignore.case = TRUE)) {
      log_line("- ", utc_now(), " | Comtrade reports the quota exhausted; stopping this session")
      return("budget")
    }
    if (identical(status, 401L) || identical(status, 403L) || identical(kind, "auth")) {
      stop("Comtrade refused the request (", kind, "); stopping without further calls.")
    }
    if (identical(status, 414L) || grepl("\\b414\\b", msg)) return(split_and_fetch("HTTP 414 (query too long)"))
    if (!is.na(status) && status >= 400 && status < 500 && status != 429L) {
      log_line("- ", utc_now(), " | ", id, " was refused with HTTP ", status, "; not retrying")
      break
    }
    # The server gives up on a heavy query after ~230 s ("500 - The request timed out"); a retry
    # usually succeeds, but a second timeout means the query is too heavy, so split it.
    if (!is.na(status) && status >= 500 && grepl("timed out", msg, ignore.case = TRUE)) {
      n_server_timeouts <- n_server_timeouts + 1L
      if (n_server_timeouts >= 2L && (length(reporters) > 1 || length(chunk_codes) > 1)) {
        return(split_and_fetch("server timed out twice"))
      }
    }
    # HTTP 429: wait exactly what the server asks; a wait beyond the session's patience means the
    # key's allowance is spent for now, so stop and leave the rest for a later session.
    if (identical(status, 429L) && !is.na(retry_after) && retry_after > max_wait_seconds) {
      log_line("- ", utc_now(), " | Retry-After of ", retry_after, "s exceeds ", max_wait_seconds, "s; stopping this session")
      return("budget")
    }
    if (attempt < retries) {
      wait <- if (identical(status, 429L) && !is.na(retry_after)) retry_after + 1 else backoff_seconds * 2^(attempt - 1)
      Sys.sleep(min(wait, max_wait_seconds))
    }
  }
  if (is.null(out)) {
    log_line("- ", utc_now(), " | ", id, " FAILED after ", retries, " attempts; rerun to retry")
    return("failed")
  }

  n_today <- paste0(calls_today(), " today, ", nrow(calls_log()), " total")
  if (comtrade_dyads_is_empty(out)) {
    writeLines(utc_now(), chunk_file(id, ".empty"))
    log_line("- ", utc_now(), " | call ", n_today, " | ", id, " | ", length(reporters), " reporters | no data")
    return("done")
  }
  if (comtrade_dyads_is_truncated(nrow(out))) {
    return(split_and_fetch(paste0("call ", n_today, ": hit the ", COMTRADE_DYADS_RECORD_CAP, "-record cap")))
  }
  tmp <- chunk_file(id, ".tmp.csv.gz")
  data.table::fwrite(out, tmp)
  file.rename(tmp, chunk_file(id, ".csv.gz"))
  log_line("- ", utc_now(), " | call ", n_today, " | ", id, " | ", length(reporters), " reporters, ",
           length(chunk_codes), " codes | ", format(nrow(out), big.mark = ","), " rows | ",
           length(unique(out$reporter_iso)), " reporters returned")
  "done"
}

# Leaf chunks of the plan in `dir` (a split chunk is replaced by its halves): ids with their state
# and directory, so combine reads exactly the plans' files and can list what is still missing.
# Another chunk directory (COMTRADE_DYADS_COMBINE_DIRS) is walked from its own plan.csv and
# codes.txt.
plan_leaves <- function(dir = chunk_dir) {
  p <- plan
  groups <- code_groups
  if (!identical(dir, chunk_dir)) {
    p <- utils::read.csv(file.path(dir, "plan.csv"), stringsAsFactors = FALSE)
    groups <- comtrade_dyads_code_groups(readLines(file.path(dir, "codes.txt")), max(p$code_group))
  }
  walk <- function(id, reporters, chunk_codes) {
    s <- chunk_state(id, dir)
    if (s != "split") return(data.frame(chunk_id = id, state = s, dir = dir, stringsAsFactors = FALSE))
    h <- comtrade_dyads_split(reporters, chunk_codes)
    rbind(walk(paste0(id, "a"), h$a$reporters, h$a$codes), walk(paste0(id, "b"), h$b$reporters, h$b$codes))
  }
  do.call(rbind, lapply(seq_len(nrow(p)), function(i) {
    walk(p$chunk_id[i], strsplit(p$reporters[i], " ", fixed = TRUE)[[1]], groups[[p$code_group[i]]])
  }))
}
all_chunks_done <- function() all(plan_leaves()$state == "done")

# Text columns of the raw chunks (codes keep their leading zeros).
char_cols <- c("reporter_iso", "partner_iso", "partner2iso", "cmd_code", "customs_code", "mos_code",
               "flow_code", "period", "classification_code", "qty_unit_abbr", "alt_qty_unit_abbr")
read_chunk <- function(f) {
  # Values and weights are read as double: a chunk whose weights are all whole and some above
  # 2^31 reads as integer64 by default (fread's integer64 = "double" did not prevent it), and
  # rbindlist would then truncate the decimals of every other chunk's values.
  as.data.frame(data.table::fread(f, colClasses = list(character = char_cols, numeric = COMTRADE_DYADS_VALUE_COLUMNS),
                                  showProgress = FALSE))
}

if (stage %in% c("fetch", "all")) {
  comtrade_set_key_from_env()
  log_line("")
  log_line("## Fetch session ", utc_now(), " (", calls_today(), " calls already used today, budget ", max_calls_per_day, ")")
  statuses <- character(nrow(plan))
  for (i in seq_len(nrow(plan))) {
    statuses[i] <- fetch_chunk(
      plan$chunk_id[i], plan$year[i], plan$flow[i],
      plan_reporters(i), plan_codes(i)
    )
    if (statuses[i] == "budget") break
    if (i %% 5 == 0 || i == nrow(plan)) {
      log_line("- progress: ", sum(statuses[seq_len(i)] == "done"), "/", nrow(plan), " planned chunks done; ",
               calls_today(), " calls today")
    }
  }
  n_done <- sum(statuses == "done")
  log_line("- session end ", utc_now(), ": ", n_done, "/", nrow(plan), " planned chunks done; ",
           calls_today(), " calls used today; ", nrow(calls_log()), " calls in total")
  if (any(statuses %in% c("failed", "budget"))) {
    pending <- plan$chunk_id[statuses != "done"]
    log_line("- not yet fetched: ", paste(pending, collapse = " "))
  }
}

# --- Combine --------------------------------------------------------------------------------
if (stage %in% c("combine", "all")) {
  dirs <- unique(c(chunk_dir, combine_dirs))
  dir_codes <- lapply(dirs, function(d) readLines(file.path(d, "codes.txt")))
  shared <- comtrade_dyads_shared_codes(dir_codes)
  if (length(shared) > 0) {
    stop("The chunk directories to combine share code(s) ", paste(shared, collapse = " "),
         "; each code must come from one pull.")
  }
  uncovered <- comtrade_dyads_uncovered_codes(basket$codes, dir_codes)
  if (length(uncovered) > 0 && !allow_partial) {
    stop("No chunk directory to combine holds the basket code(s) ", paste(uncovered, collapse = " "),
         "; name the directory of their supplementary pull in COMTRADE_DYADS_COMBINE_DIRS (or pull them with ",
         "COMTRADE_DYADS_CODES), or combine without them with COMTRADE_DYADS_ALLOW_PARTIAL=true.")
  }
  leaves <- do.call(rbind, lapply(dirs, plan_leaves))
  missing_chunks <- leaves$chunk_id[leaves$state != "done"]
  if (length(missing_chunks) > 0 && !allow_partial) {
    stop("Not every planned chunk is fetched yet; rerun with COMTRADE_DYADS_STAGE=fetch, or combine what is ",
         "there with COMTRADE_DYADS_ALLOW_PARTIAL=true (see ", log_path, ").")
  }
  files <- chunk_file(leaves$chunk_id, ".csv.gz", leaves$dir)
  files <- files[file.exists(files)]
  dropped <- c(placeholder = 0, non_total = 0, aggregate_or_outside = 0)
  economies_out <- if (identical(taiwan_mode, "TWN")) union(economies, COMTRADE_DYADS_TAIWAN_ISO3) else economies
  parts <- lapply(files, function(f) {
    std <- comtrade_dyads_standardize(read_chunk(f), economies, legacy_codes = basket$legacy,
                                      taiwan = taiwan_mode)
    dropped <<- dropped + attr(std, "dropped")
    std
  })
  combined <- as.data.frame(data.table::rbindlist(parts, use.names = TRUE, fill = TRUE))
  rm(parts)
  invisible(gc())
  combined <- comtrade_dyads_collapse_duplicates(combined)
  n_collapsed <- attr(combined, "n_collapsed")
  n_identical <- if (is.null(attr(combined, "n_identical"))) 0L else attr(combined, "n_identical")
  combined <- combined[order(combined$ref_year, combined$flow_code, combined$reporter_iso,
                             combined$partner_iso, combined$cmd_code), , drop = FALSE]

  problems <- comtrade_dyads_validate(combined, economies_out)
  if (length(problems) > 0) stop("Combined Comtrade dyads failed validation:\n- ", paste(problems, collapse = "\n- "))

  tmp <- paste0(dyads_output_path, ".tmp")
  data.table::fwrite(combined, tmp)
  file.rename(tmp, dyads_output_path)

  log_line("")
  log_line("## Combine ", utc_now())
  log_line("- wrote ", dyads_output_path, ": ", format(nrow(combined), big.mark = ","), " rows from ",
           length(files), " chunk files",
           if (length(dirs) > 1) paste0(" in ", length(dirs), " chunk directories (", paste(dirs, collapse = ", "), ")"),
           "; dropped ", dropped[["aggregate_or_outside"]],
           " World/area/non-WDI rows, ", dropped[["non_total"]], " non-total rows, ",
           dropped[["placeholder"]], " placeholders; collapsed ", n_collapsed, " duplicate rows (",
           n_identical, " identical); kept ", sum(combined$reporter_iso == combined$partner_iso),
           " self-dyad (re-import) rows, as the allied file does")
  if (identical(taiwan_mode, "TWN")) {
    twn_r <- combined$reporter_iso == COMTRADE_DYADS_TAIWAN_ISO3
    twn_p <- combined$partner_iso == COMTRADE_DYADS_TAIWAN_ISO3
    log_line("- Taiwan (Comtrade S19) recoded to TWN: ", format(sum(twn_r | twn_p), big.mark = ","), " rows (",
             format(sum(twn_r), big.mark = ","), " as reporter, ", format(sum(twn_p), big.mark = ","), " as partner)")
  } else {
    log_line("- Taiwan (Comtrade S19) dropped (COMTRADE_DYADS_TAIWAN=drop)")
  }
  if (length(missing_chunks) > 0) {
    log_line("- PARTIAL: ", length(missing_chunks), " planned chunk(s) not fetched: ", paste(missing_chunks, collapse = " "))
  }
  if (length(uncovered) > 0) {
    log_line("- PARTIAL: ", length(uncovered), " basket code(s) in no chunk directory: ", paste(uncovered, collapse = " "))
  }
  for (y in sort(unique(combined$ref_year))) {
    for (f in c("import", "export")) {
      s <- combined[combined$ref_year == y & combined$flow_direction == f, , drop = FALSE]
      log_line("- ", y, " ", f, ": ", length(unique(s$reporter_iso)), " reporters, ",
               length(unique(s$partner_iso)), " partners, ", length(unique(s$cmd_code)), " codes, ",
               format(nrow(s), big.mark = ","), " rows")
    }
  }
}

# --- Energy-trade World totals --------------------------------------------------------------
# The World rows of this chunk directory, added to comtrade_energy_trade.csv for the codes that
# file lacks (comtrade_energy_trade_add_codes(): the file's year and reporters; rows it already
# has are rewritten unchanged). Run it on a supplementary pull of codes new to the basket.
if (stage == "energy_trade") {
  leaves <- plan_leaves()
  if (any(leaves$state != "done")) {
    stop("Not every planned chunk of ", chunk_dir, " is fetched yet; the World totals would be incomplete.")
  }
  files <- chunk_file(leaves$chunk_id, ".csv.gz")
  files <- files[file.exists(files)]
  world <- as.data.frame(data.table::rbindlist(lapply(files, function(f) comtrade_dyads_world_totals(read_chunk(f))),
                                               use.names = TRUE, fill = TRUE))
  text_cols <- COMTRADE_ENERGY_TRADE_TEXT_COLUMNS
  existing <- utils::read.csv(energy_trade_path, stringsAsFactors = FALSE,
                              colClasses = stats::setNames(rep("character", length(text_cols)), text_cols))
  merged <- comtrade_energy_trade_add_codes(existing, world)
  added <- attr(merged, "added")

  log_line("")
  log_line("## Energy-trade World totals ", utc_now())
  if (nrow(added) == 0) {
    log_line("- nothing added: ", energy_trade_path, " already holds every code with World rows in its year")
  } else {
    tmp <- paste0(energy_trade_path, ".tmp")
    utils::write.csv(merged, tmp, row.names = FALSE)
    file.rename(tmp, energy_trade_path)
    log_line("- wrote ", energy_trade_path, ": ", format(nrow(existing), big.mark = ","), " rows kept, ",
             format(sum(added$rows), big.mark = ","), " added (year ", paste(sort(unique(existing$ref_year)), collapse = ", "),
             "; reporters as in the file, flow by flow)")
    for (cc in sort(unique(added$cmd_code))) {
      a <- added[added$cmd_code == cc, , drop = FALSE]
      log_line("- ", cc, ": ", paste(paste0(a$flow_code, " ", a$rows, " reporters"), collapse = ", "))
    }
  }
}
