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

# Monthly UN Comtrade pull for the coalition's core members: data/raw/comtrade_monthly_core.csv.
#
# Why: the Country pages chart each technology cell's sub-components month by month, indexed to the
# first of the last 12 months. The annual pulls (scripts/98) cannot show that; this pull asks for
# the latest 12 calendar months each core member has reported, flows to and from the World.
#
# Scope: reporters = tier "core" of data/processed/coalition_tbl.rds (scripts/13), one call group per
# window end. Window = the 12 calendar months ending at the reporter's latest month in Comtrade's
# keyless monthly data-availability listing (getDA, freq M), so reporters that publish later keep
# their own window; the windows file records each one and the members with no monthly data at all.
# Codes = every non-legacy code of data/reference/energy_hs6_master.csv plus any code lists named in
# COMTRADE_MONTHLY_EXTRA_CODES (e.g. a proposed basket), partner World, all customs procedures and
# transport modes (the API totals), HS as reported (H6 for every core member in 2024-26).
#
# Calls follow scripts/98: one keyed data-API call per chunk, raw response kept as a chunk file so an
# interrupted run resumes, a capped or too-long request is split, calls counted against
# COMTRADE_MONTHLY_MAX_CALLS per UTC day. The key travels in a header and never reaches a log line.
#
# Env knobs (all optional except COMTRADE_API_KEY for the fetch):
#   COMTRADE_MONTHLY_STAGE         plan | fetch | combine | all (default all)
#   COMTRADE_MONTHLY_PROCESSED_DIR where coalition_tbl.rds is read (default <repo>/<processed_dir>)
#   COMTRADE_MONTHLY_EXTRA_CODES   comma-separated CSV paths with an hs6 column, added to the basket
#   COMTRADE_MONTHLY_CHUNK_DIR     default <raw_data_dir>/comtrade_chunks/comtrade_monthly_core
#   COMTRADE_MONTHLY_OUTPUT        default <raw_data_dir>/comtrade_monthly_core.csv (+ _windows.csv)
#   COMTRADE_MONTHLY_MAX_CALLS     keyed calls allowed per UTC day, default 150
#   COMTRADE_MONTHLY_PAUSE_SECONDS pause before every call, default 2
#   COMTRADE_MONTHLY_CODE_GROUPS   code groups per window (query length), default 3
#   COMTRADE_MONTHLY_MAX_REPORTERS reporters per call, default 15

config <- getOption("opportunity_security.config")
raw_data_path <- file.path(repo_root, config$raw_data_dir)
dir.create(raw_data_path, recursive = TRUE, showWarnings = FALSE)

source(file.path(repo_root, "scripts", "utils", "raw_inputs.R"))
source(file.path(repo_root, "scripts", "utils", "comtrade_client.R"))
source(file.path(repo_root, "scripts", "utils", "comtrade_dyads_utils.R"))
source(file.path(repo_root, "scripts", "utils", "comtrade_monthly_utils.R"))

env_chr <- function(name, default) {
  v <- Sys.getenv(name, "")
  if (nzchar(v)) v else default
}
env_list <- function(name) setdiff(trimws(strsplit(env_chr(name, ""), ",", fixed = TRUE)[[1]]), "")

stage <- tolower(env_chr("COMTRADE_MONTHLY_STAGE", "all"))
if (!stage %in% c("plan", "fetch", "combine", "all")) stop("COMTRADE_MONTHLY_STAGE must be plan, fetch, combine or all.")
processed_dir <- env_chr("COMTRADE_MONTHLY_PROCESSED_DIR", file.path(repo_root, config$processed_dir))
chunk_dir <- env_chr("COMTRADE_MONTHLY_CHUNK_DIR", file.path(raw_data_path, "comtrade_chunks", "comtrade_monthly_core"))
output_path <- env_chr("COMTRADE_MONTHLY_OUTPUT", file.path(raw_data_path, "comtrade_monthly_core.csv"))
windows_path <- sub("\\.csv$", "_windows.csv", output_path)
max_calls_per_day <- as.integer(env_chr("COMTRADE_MONTHLY_MAX_CALLS", "150"))
pause_seconds <- as.numeric(env_chr("COMTRADE_MONTHLY_PAUSE_SECONDS", "2"))
n_code_groups <- as.integer(env_chr("COMTRADE_MONTHLY_CODE_GROUPS", "3"))
max_reporters <- as.integer(env_chr("COMTRADE_MONTHLY_MAX_REPORTERS", "15"))
retries <- 5L
timeout_seconds <- 600
backoff_seconds <- 15
max_wait_seconds <- 600
dir.create(chunk_dir, recursive = TRUE, showWarnings = FALSE)
log_path <- file.path(chunk_dir, "log.md")

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

# --- Basket, reporters, windows --------------------------------------------------------------
master <- utils::read.csv(file.path(raw_inputs_reference_dir(repo_root), "energy_hs6_master.csv"), colClasses = "character")
extras <- lapply(env_list("COMTRADE_MONTHLY_EXTRA_CODES"), utils::read.csv, colClasses = "character")
codes <- comtrade_monthly_basket(c(list(master), extras))

coalition <- readRDS(file.path(processed_dir, "coalition_tbl.rds"))
core <- sort(unique(as.character(coalition$iso3[coalition$tier == "core"])))
reporter_ref <- comtradr::ct_get_ref_table("reporter")

plan_path <- file.path(chunk_dir, "plan.csv")
codes_path <- file.path(chunk_dir, "codes.txt")
calls_path <- file.path(chunk_dir, "calls.csv")
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

# Keyless monthly availability for the months a 12-month window can reach (two calendar years back).
read_da_monthly <- function() {
  path <- file.path(chunk_dir, "getDA_monthly.json")
  if (!file.exists(path)) {
    this_year <- as.integer(format(Sys.Date(), "%Y"))
    periods <- as.vector(outer((this_year - 2L):this_year, 1:12, function(y, m) y * 100L + m))
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

if (!file.exists(plan_path)) {
  windows <- comtrade_monthly_windows(read_da_monthly(), core)
  plan <- comtrade_monthly_plan(windows, n_code_groups = n_code_groups, max_reporters = max_reporters)
  utils::write.csv(windows, file.path(chunk_dir, "windows.csv"), row.names = FALSE)
  utils::write.csv(plan, plan_path, row.names = FALSE)
  writeLines(codes, codes_path)
  log_line("")
  log_line("## Plan (", utc_now(), ")")
  log_line("- ", length(core), " core members; ", sum(!is.na(windows$last_month)), " with monthly data; none for: ",
           paste(windows$reporter_iso[is.na(windows$last_month)], collapse = " "))
  for (l in sort(unique(windows$last_month[!is.na(windows$last_month)]))) {
    log_line("- window ending ", l, ": ", paste(windows$reporter_iso[windows$last_month %in% l], collapse = " "))
  }
  log_line("- ", length(codes), " HS6 codes in ", n_code_groups, " groups; ", nrow(plan), " planned calls")
}
windows <- utils::read.csv(file.path(chunk_dir, "windows.csv"), stringsAsFactors = FALSE)
plan <- utils::read.csv(plan_path, stringsAsFactors = FALSE)
codes <- readLines(codes_path)
code_groups <- comtrade_dyads_code_groups(codes, max(plan$code_group))

# --- Fetch ------------------------------------------------------------------------------------
chunk_file <- function(id, ext) file.path(chunk_dir, paste0(id, ext))
# The files a planned chunk resolved to: its own (data or empty) or, when it was split, both halves'
# (recursively). NULL while any branch is still missing, so a half-fetched split is not "done".
chunk_leaves <- function(id) {
  for (ext in c(".csv.gz", ".empty")) if (file.exists(chunk_file(id, ext))) return(chunk_file(id, ext))
  if (!file.exists(chunk_file(paste0(id, "a"), ".csv.gz")) && !file.exists(chunk_file(paste0(id, "a"), ".empty")) &&
      !length(list.files(chunk_dir, pattern = paste0("^", id, "a")))) return(NULL)
  a <- chunk_leaves(paste0(id, "a"))
  b <- chunk_leaves(paste0(id, "b"))
  if (is.null(a) || is.null(b)) NULL else c(a, b)
}
month_str <- function(p) sprintf("%04d-%02d", p %/% 100L, p %% 100L)

monthly_call <- function(reporters, chunk_codes, first, last, flow) {
  params <- comtradr:::ct_check_params(
    type = "goods", frequency = "M", commodity_classification = "HS", commodity_code = chunk_codes,
    flow_direction = flow, reporter = reporters, partner = "World", start_date = month_str(first),
    end_date = month_str(last), mode_of_transport = "TOTAL modes of transport", partner_2 = "World",
    customs_code = "C00", update = FALSE, verbose = FALSE, extra_params = NULL, bulk = FALSE
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

fetch_chunk <- function(id, reporters, chunk_codes, first, last, flow) {
  if (file.exists(chunk_file(id, ".csv.gz")) || file.exists(chunk_file(id, ".empty"))) return("done")
  split_fetch <- function(reason) {
    log_line("- ", utc_now(), " | ", id, " | ", reason, ", splitting")
    h <- comtrade_dyads_split(reporters, chunk_codes)
    a <- fetch_chunk(paste0(id, "a"), h$a$reporters, h$a$codes, first, last, flow)
    b <- fetch_chunk(paste0(id, "b"), h$b$reporters, h$b$codes, first, last, flow)
    if (all(c(a, b) == "done")) "done" else if ("budget" %in% c(a, b)) "budget" else "failed"
  }
  ids <- reporter_ref$id[reporter_ref$iso_3 %in% reporters]
  if (comtrade_dyads_query_chars(ids, chunk_codes) > COMTRADE_DYADS_MAX_QUERY_CHARS) {
    return(split_fetch("query string too long"))
  }
  out <- NULL
  for (attempt in seq_len(retries)) {
    if (calls_today() >= max_calls_per_day) {
      log_line("- ", utc_now(), " | budget of ", max_calls_per_day, " calls reached before ", id)
      return("budget")
    }
    Sys.sleep(pause_seconds)
    res <- tryCatch(monthly_call(reporters, chunk_codes, first, last, flow), error = function(e) e)
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
    writeLines(utc_now(), chunk_file(id, ".empty"))
    log_line("- ", utc_now(), " | call ", calls_today(), " today | ", id, " | no data")
    return("done")
  }
  if (comtrade_dyads_is_truncated(nrow(out))) return(split_fetch("hit the record cap"))
  tmp <- chunk_file(id, ".tmp.csv.gz")
  data.table::fwrite(out, tmp)
  file.rename(tmp, chunk_file(id, ".csv.gz"))
  log_line("- ", utc_now(), " | call ", calls_today(), " today | ", id, " | ", length(reporters), " reporters, ",
           length(chunk_codes), " codes | ", format(nrow(out), big.mark = ","), " rows | ",
           length(unique(out$reporter_iso)), " reporters returned")
  "done"
}

if (stage %in% c("fetch", "all")) {
  comtrade_set_key_from_env()
  log_line("")
  log_line("## Fetch session ", utc_now(), " (", calls_today(), " calls already used today, budget ", max_calls_per_day, ")")
  states <- character(nrow(plan))
  for (i in seq_len(nrow(plan))) {
    states[i] <- fetch_chunk(plan$chunk_id[i], strsplit(plan$reporters[i], " ", fixed = TRUE)[[1]],
                             code_groups[[plan$code_group[i]]], plan$first_month[i], plan$last_month[i], plan$flow[i])
    if (states[i] == "budget") break
  }
  log_line("- session end ", utc_now(), ": ", sum(states == "done"), "/", nrow(plan), " planned chunks done")
}

# --- Combine ----------------------------------------------------------------------------------
if (stage %in% c("combine", "all")) {
  # only the current plan's chunks: files of an earlier plan (a refresh moves the windows) overlap it
  leaves <- lapply(plan$chunk_id, chunk_leaves)
  done <- !vapply(leaves, is.null, logical(1))
  if (!all(done)) stop("Chunks not fetched yet: ", paste(plan$chunk_id[!done], collapse = ", "))
  files <- grep("\\.csv\\.gz$", unlist(leaves), value = TRUE)
  keep <- c("period", "reporter_iso", "reporter_desc", "flow_code", "partner_code", "classification_code",
            "cmd_code", "cmd_desc", "primary_value", "fobvalue", "cifvalue", "net_wgt", "qty", "qty_unit_abbr")
  raw <- data.table::rbindlist(lapply(files, function(f) {
    d <- data.table::fread(f, colClasses = list(character = c("cmd_code", "period")))
    d[, intersect(keep, names(d)), with = FALSE]
  }), fill = TRUE)
  raw <- as.data.frame(raw[raw$partner_code == 0, ])
  key <- paste(raw$reporter_iso, raw$period, raw$flow_code, raw$cmd_code)
  if (anyDuplicated(key)) stop("Duplicate (reporter, month, flow, code) rows across chunks: ", sum(duplicated(key)))
  out <- comtrade_monthly_trim(raw, windows)
  out$reporter_iso <- as.character(out$reporter_iso)
  out <- out[order(out$reporter_iso, out$flow_code, out$cmd_code, out$period), ]
  data.table::fwrite(out, output_path)
  utils::write.csv(windows, windows_path, row.names = FALSE)
  log_line("")
  log_line("## Combine ", utc_now())
  log_line("- wrote ", output_path, ": ", format(nrow(out), big.mark = ","), " rows; ",
           length(unique(out$reporter_iso)), " reporters; ", length(unique(out$cmd_code)), " codes; ",
           nrow(raw) - nrow(out), " rows outside a reporter's window dropped")
}
