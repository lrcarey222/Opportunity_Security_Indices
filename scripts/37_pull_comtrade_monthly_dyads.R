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


# Monthly bilateral Comtrade pull: each monthly reporter's latest 12 months of basket imports and exports,
# by partner (the T12M input of scripts/38_build_current_import_exposure.R).
#
#   Rscript scripts/37_pull_comtrade_monthly_dyads.R
#
# Resumable: one chunk file per call under data/raw/comtrade_chunks/comtrade_monthly_dyads/; rerun to
# continue after a budget stop or failure. Stages (COMTRADE_MD_STAGE): plan, fetch, combine, all.
# Settings (environment):
#   COMTRADE_API_KEY             required for fetch
#   COMTRADE_MD_MONTHS           window length (12)
#   COMTRADE_MD_MIN_MONTHS       months a window must list to be kept (10)
#   COMTRADE_MD_MAX_STALENESS    months a reporter's latest month may trail the newest listed month (9)
#   COMTRADE_MD_MAX_CALLS        daily call budget shared with no other pull (300)
#   COMTRADE_MD_TARGET_RECORDS   records per planned call (85000; the API caps a call at 100000)
#   COMTRADE_MD_REPLAN           "true" to rebuild the plan (only before any chunk is fetched)
#
# Inputs: data/reference/energy_hs6_master.csv (basket: non-excluded, current-HS codes);
# data/raw/comtrade_energy_dyads_all.csv (scripts/98; 2024 rows per reporter size the plan, optional).
# Output: data/raw/comtrade_monthly_dyads.csv.gz (reporter_iso, partner_iso, flow_direction, period YYYYMM,
# cmd_code, primary_value) and data/raw/comtrade_monthly_dyads_windows.csv (reporter_iso, window_start,
# window_end, months_listed).

for (f in c("scripts/utils/comtrade_client.R", "scripts/utils/comtrade_dyads_utils.R", "R/strategic_position/current_trade.R")) {
  source(file.path(repo_root, f))
}
config <- getOption("opportunity_security.config")
if (is.null(config)) stop("Config not loaded; run scripts/00_setup.R first.")
raw_data_path <- file.path(repo_root, config$raw_data_dir)

env_chr <- function(name, default) {
  v <- Sys.getenv(name, "")
  if (nzchar(v)) v else default
}
stage <- tolower(env_chr("COMTRADE_MD_STAGE", "all"))
if (!stage %in% c("plan", "fetch", "combine", "all")) stop("COMTRADE_MD_STAGE must be plan, fetch, combine or all.")
n_months <- as.integer(env_chr("COMTRADE_MD_MONTHS", "12"))
min_months <- as.integer(env_chr("COMTRADE_MD_MIN_MONTHS", "10"))
max_staleness <- as.integer(env_chr("COMTRADE_MD_MAX_STALENESS", "9"))
max_calls_per_day <- as.integer(env_chr("COMTRADE_MD_MAX_CALLS", "300"))
target_records <- as.numeric(env_chr("COMTRADE_MD_TARGET_RECORDS", "85000"))
replan <- tolower(env_chr("COMTRADE_MD_REPLAN", "false")) %in% c("1", "true", "yes")
pause_seconds <- 2
retries <- 5L
timeout_seconds <- 600
backoff_seconds <- 15
max_wait_seconds <- 600
# Monthly basket rows per reporter-flow over 12 months as a multiple of its 2024 annual rows (Germany,
# 2025-06 imports: ~4,000 rows a month for 244 codes against ~9,300 a year), and the fallback for a
# reporter without annual rows.
monthly_row_factor <- 5.5
fallback_rows <- 6000

# Code set (COMTRADE_MD_CODE_SET): "basket" (the OSI basket; default) or "dictionary_extra" (the AIB
# dictionary's core codes outside the basket, which scripts/31 reads; written to its own chunk folder and file,
# on the basket pull's reporter windows so both sets cover the same months).
code_set <- tolower(env_chr("COMTRADE_MD_CODE_SET", "basket"))
if (!code_set %in% c("basket", "dictionary_extra")) stop("COMTRADE_MD_CODE_SET must be basket or dictionary_extra.")
suffix <- if (code_set == "basket") "" else paste0("_", code_set)
basket_chunk_dir <- file.path(raw_data_path, "comtrade_chunks", "comtrade_monthly_dyads")
chunk_dir <- paste0(basket_chunk_dir, suffix)
dir.create(chunk_dir, recursive = TRUE, showWarnings = FALSE)
log_path <- file.path(chunk_dir, "log.md")
calls_path <- file.path(chunk_dir, "calls.csv")
plan_path <- file.path(chunk_dir, "plan.csv")
windows_path <- file.path(chunk_dir, "windows.csv")
out_path <- file.path(raw_data_path, paste0("comtrade_monthly_dyads", suffix, ".csv.gz"))
out_windows_path <- file.path(raw_data_path, "comtrade_monthly_dyads_windows.csv")

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
record_call <- function(id, attempt, status, rows = NA_integer_) {
  row <- data.frame(utc_time = utc_now(), utc_date = utc_today(), chunk_id = id, attempt = attempt, status = redact(status), rows = rows)
  utils::write.table(row, calls_path, sep = ",", row.names = FALSE, col.names = !file.exists(calls_path), append = file.exists(calls_path))
}
calls_today <- function() {
  if (!file.exists(calls_path)) return(0L)
  sum(utils::read.csv(calls_path, stringsAsFactors = FALSE)$utc_date == utc_today())
}

master <- utils::read.csv(file.path(repo_root, "data", "reference", "energy_hs6_master.csv"), colClasses = "character")
is_true <- function(x) toupper(x) %in% c("TRUE", "T", "1")
master <- master[!is_true(master$excluded) & !is_true(master$legacy_hs), ]
basket <- sort(unique(formatC(as.integer(master$hs6), width = 6, flag = "0")))
codes <- basket
if (code_set == "dictionary_extra") {
  for (f in c("R/utils/schema.R", "R/utils/hs6_crosswalk.R", "R/aib/aib_nodes.R")) source(file.path(repo_root, f))
  aib_nodes_cfg <- yaml::read_yaml(file.path(repo_root, "config", "aib.yml"))$nodes$dictionary
  dict <- utils::read.csv(file.path(repo_root, "data", "reference", aib_nodes_cfg$file %||% "electro_industrial_hs6_dictionary.csv"),
                          colClasses = c(hs6 = "character", hs1992_join = "character"), stringsAsFactors = FALSE)
  dm <- aib_dictionary_master(dict, aib_nodes_cfg$scope %||% "core", primary_only = !identical(aib_nodes_cfg$primary_only, FALSE))
  codes <- setdiff(sort(unique(formatC(as.integer(dm$hs6), width = 6, flag = "0"))), basket)
  message("Dictionary codes outside the basket: ", length(codes), ".")
}
n_code_groups <- if (length(codes) > 150) 2L else 1L
code_groups <- comtrade_dyads_code_groups(codes, n_code_groups)
reporter_ref <- comtradr::ct_get_ref_table("reporter")
to_comtrade_iso <- function(iso) ifelse(iso == "TWN", COMTRADE_DYADS_TAIWAN_ISO, iso)

# ---- Plan -----------------------------------------------------------------------------------------
if (replan || !file.exists(plan_path)) {
  if (replan && length(list.files(chunk_dir, pattern = "\\.(csv\\.gz|empty)$"))) {
    stop("Chunks already fetched in ", chunk_dir, "; move them aside before COMTRADE_MD_REPLAN.")
  }
  if (code_set != "basket") {
    # the basket pull's windows, so both code sets cover the same months per reporter
    basket_windows <- file.path(basket_chunk_dir, "windows.csv")
    if (!file.exists(basket_windows)) stop(basket_windows, " not found; plan the basket pull first.")
    all_w <- utils::read.csv(basket_windows, stringsAsFactors = FALSE)
    utils::write.csv(all_w, windows_path, row.names = FALSE)
    windows <- all_w[all_w$reason == "kept", c("reporter_iso", "window_start", "window_end", "months_listed")]
    log_line("## Plan (", utc_now(), "): ", code_set, ", ", length(codes), " codes on the basket pull's windows (", nrow(windows), " reporters).")
  } else {
    this_year <- as.integer(format(Sys.Date(), "%Y"))
    periods <- as.vector(outer((this_year - 2L):this_year, 1:12, function(y, m) y * 100L + m))
    da <- do.call(rbind, lapply(split(sort(periods), ceiling(seq_along(periods) / 12)), function(p) {
      req <- httr2::req_timeout(httr2::req_url_query(
        httr2::request("https://comtradeapi.un.org/public/v1/getDA/C/M/HS"), period = paste(p, collapse = ",")), 120)
      d <- jsonlite::fromJSON(httr2::resp_body_string(httr2::req_perform(req)))$data
      if (NROW(d) == 0) NULL else data.frame(reporter_iso = as.character(d$reporterISO), period = as.integer(d$period))
    }))
    da$reporter_iso <- ifelse(da$reporter_iso == COMTRADE_DYADS_TAIWAN_ISO, "TWN", da$reporter_iso)
    economies <- reporter_ref$iso_3[!as.logical(reporter_ref$group)]
    da <- da[da$reporter_iso %in% c(economies, "TWN"), , drop = FALSE]
    newest <- max(da$period)
    windows <- ctm_reporter_windows(da, n_months, min_months, min_end = ctm_month_add(newest, -max_staleness))
    dropped <- attr(windows, "dropped")
    utils::write.csv(rbind(cbind(windows, reason = "kept"), dropped), windows_path, row.names = FALSE)
    log_line("## Plan (", utc_now(), "): newest month ", newest, "; ", nrow(windows), " reporters kept, ",
             sum(dropped$reason == "stale"), " stale (", paste(utils::head(dropped$reporter_iso[dropped$reason == "stale"], 40), collapse = " "),
             "), ", sum(dropped$reason == "gappy"), " gappy.")
  }

  annual_rows <- NULL
  dyad_path <- file.path(raw_data_path, "comtrade_energy_dyads_all.csv")
  if (file.exists(dyad_path)) {
    a <- data.table::fread(dyad_path, select = c("period", "reporter_iso", "flow_code"))
    a <- a[a$period == max(a$period), ]
    annual_rows <- as.data.frame(a[, .N, by = .(reporter_iso, flow_code)])
  }
  est <- do.call(rbind, lapply(c("import", "export"), function(fl) {
    code <- if (fl == "import") "M" else "X"
    n <- if (is.null(annual_rows)) integer() else stats::setNames(annual_rows$N[annual_rows$flow_code == code],
                                                                 annual_rows$reporter_iso[annual_rows$flow_code == code])
    rows <- unname(n[windows$reporter_iso])
    data.frame(year = windows$window_end, reporter_iso = windows$reporter_iso, flow = fl,
               est = ifelse(is.na(rows), fallback_rows, rows * monthly_row_factor) * length(codes) / length(basket))
  }))
  plan <- comtrade_dyads_plan(est, target_records = target_records, max_reporters = 50, n_groups = n_code_groups)
  utils::write.csv(plan, plan_path, row.names = FALSE)
  log_line("- ", nrow(plan), " calls planned, ~", format(round(sum(plan$est_records)), big.mark = ","), " records.")
}
plan <- utils::read.csv(plan_path, stringsAsFactors = FALSE)
windows <- utils::read.csv(windows_path, stringsAsFactors = FALSE)
windows <- windows[windows$reason == "kept", c("reporter_iso", "window_start", "window_end", "months_listed")]
if (stage == "plan") {
  message(nrow(plan), " calls planned; see ", plan_path)
  quit(save = "no")
}

# ---- Fetch ----------------------------------------------------------------------------------------
md_call <- function(reporters, chunk_codes, end, flow) {
  start <- ctm_month_add(end, -(n_months - 1L))
  params <- comtradr:::ct_check_params(
    type = "goods", frequency = "M", commodity_classification = "HS", commodity_code = chunk_codes,
    flow_direction = flow, reporter = unname(to_comtrade_iso(reporters)), partner = "everything",
    start_date = sprintf("%04d-%02d", start %/% 100L, start %% 100L), end_date = sprintf("%04d-%02d", end %/% 100L, end %% 100L),
    mode_of_transport = "TOTAL modes of transport", partner_2 = "World", customs_code = "C00",
    update = FALSE, verbose = FALSE, extra_params = NULL, bulk = FALSE
  )
  req <- comtradr:::ct_build_request(params, primary_token = comtradr::get_primary_comtrade_key(), verbose = FALSE, bulk = FALSE)
  req <- httr2::req_error(httr2::req_timeout(req, timeout_seconds), is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req)
  status <- as.integer(httr2::resp_status(resp))
  if (status == 200L) {
    return(list(status = status, data = suppressMessages(comtradr:::ct_process_response(resp, verbose = FALSE, tidy_cols = TRUE, bulk = FALSE))))
  }
  retry_after <- suppressWarnings(as.numeric(httr2::resp_header(resp, "Retry-After")))
  body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
  list(status = status, retry_after = if (length(retry_after) == 1) retry_after else NA_real_, message = substr(gsub("\\s+", " ", body), 1, 300))
}

fetch_chunk <- function(id, reporters, chunk_codes, end, flow) {
  f <- function(ext) file.path(chunk_dir, paste0(id, ext))
  if (file.exists(f(".csv.gz")) || file.exists(f(".empty"))) return("done")
  split_fetch <- function(reason) {
    log_line("- ", utc_now(), " | ", id, " | ", reason, ", splitting")
    h <- comtrade_dyads_split(reporters, chunk_codes)
    a <- fetch_chunk(paste0(id, "a"), h$a$reporters, h$a$codes, end, flow)
    b <- fetch_chunk(paste0(id, "b"), h$b$reporters, h$b$codes, end, flow)
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
    res <- tryCatch(md_call(reporters, chunk_codes, end, flow), error = function(e) e)
    if (!inherits(res, "error") && identical(res$status, 200L)) {
      record_call(id, attempt, "ok", if (comtrade_dyads_is_empty(res$data)) 0L else nrow(res$data))
      out <- res$data
      break
    }
    status <- if (inherits(res, "error")) NA_integer_ else res$status
    msg <- if (inherits(res, "error")) conditionMessage(res) else paste0("HTTP ", status, ": ", res$message)
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
  keep <- intersect(c("period", "reporter_iso", "partner_iso", "flow_code", "cmd_code", "primary_value"), names(out))
  tmp <- f(".tmp.csv.gz")
  data.table::fwrite(out[, keep], tmp)
  file.rename(tmp, f(".csv.gz"))
  log_line("- ", utc_now(), " | call ", calls_today(), " today | ", id, " | ", length(reporters), " reporters, ",
           length(chunk_codes), " codes | ", format(nrow(out), big.mark = ","), " rows")
  "done"
}

if (stage %in% c("fetch", "all")) {
  comtrade_set_key_from_env()
  log_line("## Fetch session ", utc_now(), " (", calls_today(), " calls used today, budget ", max_calls_per_day, ")")
  states <- character(nrow(plan))
  for (i in seq_len(nrow(plan))) {
    states[i] <- fetch_chunk(plan$chunk_id[i], strsplit(plan$reporters[i], " ", fixed = TRUE)[[1]],
                             code_groups[[plan$code_group[i]]], plan$year[i], plan$flow[i])
    if (states[i] == "budget") break
  }
  log_line("- ", sum(states == "done"), "/", nrow(plan), " planned chunks done")
  if (any(states != "done")) {
    message("Not every chunk is done; rerun to continue.")
    quit(save = "no", status = if (any(states == "failed")) 1L else 0L)
  }
}

# ---- Combine --------------------------------------------------------------------------------------
if (stage %in% c("combine", "all")) {
  chunk_paths <- function(id) {
    for (ext in c(".csv.gz", ".empty")) if (file.exists(file.path(chunk_dir, paste0(id, ext)))) return(file.path(chunk_dir, paste0(id, ext)))
    a <- chunk_paths(paste0(id, "a"))
    b <- chunk_paths(paste0(id, "b"))
    if (is.null(a) || is.null(b)) NULL else c(a, b)
  }
  paths <- lapply(plan$chunk_id, chunk_paths)
  if (any(vapply(paths, is.null, TRUE))) stop("Chunks missing for: ", paste(plan$chunk_id[vapply(paths, is.null, TRUE)], collapse = ", "))
  files <- grep("\\.csv\\.gz$", unlist(paths), value = TRUE)
  d <- data.table::rbindlist(lapply(files, data.table::fread, colClasses = list(character = c("cmd_code", "period"))), fill = TRUE)
  d <- d[d$partner_iso != "W00" & !is.na(d$partner_iso) & nzchar(d$partner_iso), ]
  d$reporter_iso <- ifelse(d$reporter_iso == COMTRADE_DYADS_TAIWAN_ISO, "TWN", d$reporter_iso)
  d$partner_iso <- ifelse(d$partner_iso == COMTRADE_DYADS_TAIWAN_ISO, "TWN", d$partner_iso)
  d <- d[d$reporter_iso != d$partner_iso, ]
  out <- data.table::data.table(
    reporter_iso = d$reporter_iso, partner_iso = d$partner_iso,
    flow_direction = ifelse(d$flow_code == "M", "import", ifelse(d$flow_code == "X", "export", NA)),
    period = as.integer(d$period), cmd_code = formatC(as.integer(d$cmd_code), width = 6, flag = "0"),
    primary_value = as.numeric(d$primary_value)
  )
  out <- out[!is.na(out$flow_direction) & !is.na(out$primary_value), ]
  out <- out[, .(primary_value = sum(primary_value)), by = .(reporter_iso, partner_iso, flow_direction, period, cmd_code)]
  data.table::fwrite(out, out_path)
  utils::write.csv(windows, out_windows_path, row.names = FALSE)
  log_line("## Combined ", utc_now(), ": ", format(nrow(out), big.mark = ","), " rows, ", length(unique(out$reporter_iso)),
           " reporters, months ", min(out$period), "-", max(out$period), " -> ", basename(out_path))
}
