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

config <- getOption("opportunity_security.config")
sharepoint_raw_dir <- config$sharepoint_raw_dir
raw_data_path <- file.path(repo_root, config$raw_data_dir)
if (!dir.exists(raw_data_path)) dir.create(raw_data_path, recursive = TRUE)

is_skip_data_downloads <- function() tolower(Sys.getenv("SKIP_DATA_DOWNLOADS")) %in% c("1", "true", "yes")
skip_data_downloads <- is_skip_data_downloads()

source(file.path(repo_root, "scripts", "utils", "raw_inputs.R"))
source(file.path(repo_root, "scripts", "utils", "fetchers.R"))
source_fetcher_files(repo_root)
force_refresh <- opsi_force_refresh()
prefer_fetch <- tolower(Sys.getenv("OPSI_PREFER_FETCH", "false")) %in% c("1", "true", "yes")

copy_raw_file <- function(source_path, dest_path) {
  sync_raw_file(source_path, dest_path, force = force_refresh) != "missing"
}

source(file.path(repo_root, "scripts", "utils", "comtrade_ingest_utils.R"))
source(file.path(repo_root, "scripts", "utils", "comtrade_client.R"))

# Sync staged raw inputs. This compares size and mtime rather than skipping any file
# that already exists locally, so a newer vintage in the staging area actually lands.
# Set OPSI_FORCE_REFRESH=true to recopy everything regardless.
manifest_path <- raw_inputs_manifest_path(repo_root)
manifest <- read_raw_inputs_manifest(manifest_path)
staged_entries <- raw_inputs_staged_entries(manifest)

missing <- character()
sync_counts <- c(copied = 0L, current = 0L)
fetch_counts <- c(fetched = 0L, fresh = 0L, failed = 0L, skipped = 0L)

reference_dir <- raw_inputs_reference_dir(repo_root)

# Regenerate the HS6 crosswalk views from the single master in data/reference before
# anything reads them. Historically three differently-shaped crosswalks were staged
# independently and had drifted apart; they are now derived from one table.
source(file.path(repo_root, "scripts", "04_build_hs6_views.R"))
build_hs6_views(repo_root = repo_root, raw_data_path = raw_data_path)

for (entry in staged_entries) {
  status <- sync_raw_input_entry(
    entry, sharepoint_raw_dir, raw_data_path,
    force = force_refresh, reference_dir = reference_dir
  )
  if (status != "missing") sync_counts[[status]] <- sync_counts[[status]] + 1L

  has_local <- raw_input_present_locally(entry, raw_data_path)
  fetcher_available <- !is.null(get_fetcher(entry$id)) && !identical(entry$fetch_policy, "never")

  # A registered fetcher runs when the API is the declared authority for this input
  # (fetch_policy: prefer), or when there is no local copy to fall back on. Curated
  # staged files otherwise win, because for some sources the API's country coverage
  # differs from the extract the indices were built against.
  should_fetch <- fetcher_available &&
    (prefer_fetch || identical(entry$fetch_policy, "prefer") || !has_local)

  if (should_fetch) {
    outcome <- run_fetcher(
      entry$id,
      dest_path = file.path(raw_data_path, entry$path),
      cadence = entry$cadence,
      force = force_refresh || (!has_local)
    )
    fetch_counts[[outcome]] <- fetch_counts[[outcome]] + 1L
    if (outcome %in% c("fetched", "fresh")) has_local <- TRUE
  }

  if (!has_local && !isTRUE(entry$optional)) {
    missing <- c(missing, entry$path)
  }
}

if (length(missing) > 0) {
  stop(
    "Missing required raw inputs.\n",
    "Staged inputs are expected in sharepoint_raw_dir (", sharepoint_raw_dir, ");\n",
    "project-authored crosswalks are expected in ", reference_dir, ".\n",
    paste0("- ", missing, collapse = "\n")
  )
}
message(
  "Raw inputs synced in ", raw_data_path,
  " (", sync_counts[["copied"]], " updated, ", sync_counts[["current"]], " already current",
  if (force_refresh) ", forced" else "", ")"
)
if (sum(fetch_counts) > 0) {
  message(
    "API fetchers: ", fetch_counts[["fetched"]], " fetched, ", fetch_counts[["fresh"]],
    " already fresh, ", fetch_counts[["failed"]], " failed"
  )
}

wdi_gdp_path <- file.path(raw_data_path, "wdi_gdp.csv")
wdi_country_path <- file.path(raw_data_path, "wdi_country_info.csv")
copy_raw_file(file.path(sharepoint_raw_dir, "wdi_gdp.csv"), wdi_gdp_path)
copy_raw_file(file.path(sharepoint_raw_dir, "wdi_country_info.csv"), wdi_country_path)
wdi_needs_pull <- !file.exists(wdi_gdp_path) || !file.exists(wdi_country_path) || force_refresh
if (wdi_needs_pull && !skip_data_downloads) {
  wdi_gdp <- WDI::WDI(indicator = "NY.GDP.MKTP.CD", start = 2007, end = as.integer(format(Sys.Date(), "%Y")) - 1)
  write.csv(wdi_gdp, wdi_gdp_path, row.names = FALSE)
  write.csv(WDI::WDI_data$country, wdi_country_path, row.names = FALSE)
}

# --- Source: UN Comtrade (shared client) ---------------------------------
comtrade_chunk_count <- max(1L, as.integer(Sys.getenv("COMTRADE_CHUNK_COUNT", "1")))
comtrade_chunk_index <- max(1L, as.integer(Sys.getenv("COMTRADE_CHUNK_INDEX", "1")))
if (comtrade_chunk_index > comtrade_chunk_count) stop("COMTRADE_CHUNK_INDEX cannot exceed COMTRADE_CHUNK_COUNT")

candidate_target_year <- as.integer(Sys.getenv("COMTRADE_TARGET_YEAR", as.character(as.integer(format(Sys.Date(), "%Y")) - 1L)))
comtrade_start_year <- as.integer(Sys.getenv("COMTRADE_START_YEAR", as.character(candidate_target_year - 4L)))
# seq.int() with a single length>1 argument returns seq_along(), which silently
# requested years 1..5 instead of the calendar window. Build the range explicitly.
comtrade_years <- seq.int(from = comtrade_start_year, to = candidate_target_year)
comtrade_retries <- max(1L, as.integer(Sys.getenv("COMTRADE_MAX_RETRIES", "3")))
comtrade_sleep_seconds <- as.numeric(Sys.getenv("COMTRADE_SLEEP_SECONDS", "0.5"))
comtrade_pause_seconds <- as.numeric(Sys.getenv("COMTRADE_REQUEST_PAUSE_SECONDS", "0.2"))
comtrade_timeout_seconds <- as.numeric(Sys.getenv("COMTRADE_REQUEST_TIMEOUT_SECONDS", "120"))

subset_request_chunk <- function(request_df, chunk_index, chunk_count) {
  if (chunk_count <= 1 || nrow(request_df) == 0) return(request_df)
  idx <- seq_len(nrow(request_df))
  request_df[idx[((idx - 1) %% chunk_count) + 1 == chunk_index], , drop = FALSE]
}

# Build the request grid in the shape scripts/utils/comtrade_client.R validates:
# request_id, reporter, partner, commodity_code, start_date, end_date, flow_direction,
# frequency. request_id is assigned before chunking so ids stay stable and unique
# across chunked runs (COMTRADE_CHUNK_INDEX).
build_requests <- function(reporters, partners, commodity_codes, years, flows,
                           partner_chunk_size = 50, frequency = "A") {
  partner_chunks <- split_vec(partners, chunk_size = partner_chunk_size)
  # Expand over a single year column. Crossing start_date and end_date separately
  # would emit every (start, end) pair - including inverted ranges - which is 5x the
  # requests for the multi-year allied pull and wrong for all but the diagonal.
  tidyr::expand_grid(
    reporter = reporters,
    year = years,
    flow_direction = flows,
    commodity_code = commodity_codes,
    partner = partner_chunks
  ) %>%
    dplyr::mutate(
      request_id = dplyr::row_number(),
      start_date = year,
      end_date = year,
      frequency = frequency
    ) %>%
    dplyr::select(
      request_id, reporter, partner, commodity_code,
      start_date, end_date, flow_direction, frequency
    )
}

if (!skip_data_downloads) {
  comtrade_set_key_from_env()
}

critical_minerals_path <- resolve_versioned_raw_input(
  raw_data_path,
  # Top-level alternation, no capture group: the manifest scanner in scripts/utils
  # reads this call as source text and stops at the first closing bracket it finds.
  pattern = "^IEA Critical Minerals Dataset \\d{4}\\.xlsx$|^iea_criticalminerals_\\d{2}\\.csv$",
  fallback = "IEA Critical Minerals Dataset 2026.xlsx",
  label = "IEA Critical Minerals Dataset"
)
critical_minerals_hs_path <- file.path(raw_data_path, "Columbia University Critical Minerals Dashboard", "unique_comtrade.csv")
hs6_category_path <- file.path(raw_data_path, "hts_codes_categories_bolstered_final.csv")
allies_path <- file.path(raw_data_path, "allies.csv")
country_info <- read.csv(wdi_country_path)
reporter_ref <- if (!skip_data_downloads) comtradr::ct_get_ref_table("reporter") else data.frame(iso3_code = character())
reporter_candidates <- if (!skip_data_downloads) resolve_comtrade_reporters(country_info, reporter_ref) else character()

probe_year <- function(request_builder, year) {
  req <- request_builder(year)
  req <- req[1, , drop = FALSE]
  probe <- comtrade_fetch_requests(req, retries = 1, sleep_seconds = 0, timeout_seconds = 30, show_progress = FALSE)
  probe$data
}

actual_years <- list()

# Critical minerals outputs. The year suffix must match the year Comtrade actually
# served, otherwise downstream readers resolve a different (older) file than the one
# this step just wrote.
critmin_output_paths <- function(year) {
  list(
    import = file.path(raw_data_path, sprintf("critmin_import_%d.csv", year)),
    export = file.path(raw_data_path, sprintf("critmin_export_%d.csv", year)),
    total_export = file.path(raw_data_path, sprintf("critmin_total_export_%d.csv", year))
  )
}

critmin_paths <- critmin_output_paths(candidate_target_year)
critmin_import_path <- critmin_paths$import
critmin_export_path <- critmin_paths$export
critmin_total_export_path <- critmin_paths$total_export

if (!skip_data_downloads) {
  source(file.path(repo_root, "R", "utils", "iea_critical_minerals.R"))
  source(file.path(repo_root, "R", "categories", "minerals_trade", "critical_minerals_trade.R"))
  source(file.path(repo_root, "R", "categories", "reserves", "reserves.R"))
  critical <- read_iea_critical_minerals(critical_minerals_path)
  mineral_demand_clean <- reserves_build_mineral_demand_clean(critical)
  crit_hs <- read.csv(critical_minerals_hs_path)
  crit_codes <- critical_minerals_trade_filter_hs(crit_hs, mineral_demand_clean)$hscode %>%
    as.character() %>% stringr::str_replace_all("\\D", "") %>% stringr::str_pad(width = 6, side = "left", pad = "0") %>%
    stats::na.omit() %>% unique()
  crit_code_chunks <- split_by_nchar(crit_codes, max_chars = 2500)

  crit_probe_builder <- function(y) build_requests(reporter_candidates[1], "World", crit_code_chunks[1], y, "import", partner_chunk_size = 1)
  crit_target_year <- comtrade_pick_latest_available_year(function(y) probe_year(crit_probe_builder, y), candidate_target_year)
  actual_years$critmin <- crit_target_year

  critmin_paths <- critmin_output_paths(crit_target_year)
  critmin_import_path <- critmin_paths$import
  critmin_export_path <- critmin_paths$export
  critmin_total_export_path <- critmin_paths$total_export

  crit_import_req <- build_requests(reporter_candidates, "World", crit_code_chunks, crit_target_year, "import", partner_chunk_size = 1)
  crit_export_req <- build_requests(reporter_candidates, "World", crit_code_chunks, crit_target_year, "export", partner_chunk_size = 1)
  crit_total_req <- build_requests(reporter_candidates, "World", list("TOTAL"), crit_target_year, "export", partner_chunk_size = 1)

  crit_import_out <- comtrade_fetch_requests(subset_request_chunk(crit_import_req, comtrade_chunk_index, comtrade_chunk_count), retries = comtrade_retries, sleep_seconds = comtrade_sleep_seconds, timeout_seconds = comtrade_timeout_seconds, show_progress = TRUE, request_pause_seconds = comtrade_pause_seconds)
  crit_export_out <- comtrade_fetch_requests(subset_request_chunk(crit_export_req, comtrade_chunk_index, comtrade_chunk_count), retries = comtrade_retries, sleep_seconds = comtrade_sleep_seconds, timeout_seconds = comtrade_timeout_seconds, show_progress = TRUE, request_pause_seconds = comtrade_pause_seconds)
  crit_total_out <- comtrade_fetch_requests(subset_request_chunk(crit_total_req, comtrade_chunk_index, comtrade_chunk_count), retries = comtrade_retries, sleep_seconds = comtrade_sleep_seconds, timeout_seconds = comtrade_timeout_seconds, show_progress = TRUE, request_pause_seconds = comtrade_pause_seconds)

  stage_comtrade_output(crit_import_out$data, critmin_import_path, chunk_index = comtrade_chunk_index, chunk_count = comtrade_chunk_count)
  stage_comtrade_output(crit_export_out$data, critmin_export_path, chunk_index = comtrade_chunk_index, chunk_count = comtrade_chunk_count)
  stage_comtrade_output(crit_total_out$data, critmin_total_export_path, chunk_index = comtrade_chunk_index, chunk_count = comtrade_chunk_count)
}

# Energy trade outputs
comtrade_energy_trade_path <- file.path(raw_data_path, "comtrade_energy_trade.csv")
comtrade_total_export_path <- file.path(raw_data_path, "comtrade_total_export.csv")
allied_comtrade_energy_path <- file.path(raw_data_path, "allied_comtrade_energy_data.csv")

if (!skip_data_downloads) {
  subcat <- readr::read_csv(hs6_category_path, show_col_types = FALSE)
  energy_codes <- subcat$HS6 %>% as.character() %>% stringr::str_replace_all("\\D", "") %>% stringr::str_pad(width = 6, side = "left", pad = "0") %>%
    stats::na.omit() %>% unique()
  code_chunks <- split_by_nchar(energy_codes, max_chars = 2500)
  valid_countries <- setdiff(unique(country_info$iso3c), c("ASM", "CHI", "GUM", "IMN", "LIE", "MAF", "MCO", "PRI", "XKX"))
  allies <- if (file.exists(allies_path)) read.csv(allies_path) else data.frame(iso3c = valid_countries)

  energy_probe_builder <- function(y) build_requests(reporter_candidates[1], "World", code_chunks[1], y, "export", partner_chunk_size = 1)
  energy_year <- comtrade_pick_latest_available_year(function(y) probe_year(energy_probe_builder, y), candidate_target_year)
  actual_years$energy <- energy_year

  energy_req <- build_requests(reporter_candidates, "World", code_chunks, energy_year, c("export", "import"), partner_chunk_size = 1)
  total_req <- build_requests(reporter_candidates, "World", list("TOTAL"), energy_year, "export", partner_chunk_size = 1)
  allied_req <- build_requests(allies$iso3c, valid_countries, code_chunks, comtrade_years, c("export", "import"), partner_chunk_size = 50)

  energy_out <- comtrade_fetch_requests(subset_request_chunk(energy_req, comtrade_chunk_index, comtrade_chunk_count), retries = comtrade_retries, sleep_seconds = comtrade_sleep_seconds, timeout_seconds = comtrade_timeout_seconds, show_progress = TRUE, request_pause_seconds = comtrade_pause_seconds)
  total_out <- comtrade_fetch_requests(subset_request_chunk(total_req, comtrade_chunk_index, comtrade_chunk_count), retries = comtrade_retries, sleep_seconds = comtrade_sleep_seconds, timeout_seconds = comtrade_timeout_seconds, show_progress = TRUE, request_pause_seconds = comtrade_pause_seconds)
  allied_out <- comtrade_fetch_requests(subset_request_chunk(allied_req, comtrade_chunk_index, comtrade_chunk_count), retries = comtrade_retries, sleep_seconds = comtrade_sleep_seconds, timeout_seconds = comtrade_timeout_seconds, show_progress = TRUE, request_pause_seconds = comtrade_pause_seconds)

  stage_comtrade_output(energy_out$data, comtrade_energy_trade_path, chunk_index = comtrade_chunk_index, chunk_count = comtrade_chunk_count)
  stage_comtrade_output(total_out$data, comtrade_total_export_path, chunk_index = comtrade_chunk_index, chunk_count = comtrade_chunk_count)
  stage_comtrade_output(allied_out$data, allied_comtrade_energy_path, chunk_index = comtrade_chunk_index, chunk_count = comtrade_chunk_count)

  vintage_path <- file.path(raw_data_path, "comtrade_vintage.yml")
  vintage <- list(
    retrieval_timestamp_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    year_start = comtrade_start_year,
    year_end = candidate_target_year,
    actual_year_end_used = if (length(actual_years) > 0) max(unlist(actual_years)) else NA_integer_,
    actual_year_by_dataset = actual_years
  )
  yaml::write_yaml(vintage, vintage_path)
}

# --- Source: IMF Primary Commodity Price System (PCPS) ------------------
imf_pcps_excel_path <- file.path(raw_data_path, "IMF_PCPS_all.xlsx")
imf_pcps_excel_status <- sync_raw_file(
  file.path(sharepoint_raw_dir, "IMF_PCPS_all.xlsx"),
  imf_pcps_excel_path,
  force = force_refresh
)

imf_pcps_prices_path <- file.path(raw_data_path, "imf_pcps_prices.csv")
imf_pcps_volatility_path <- file.path(raw_data_path, "imf_pcps_price_volatility.csv")
imf_pcps_series_volatility_path <- file.path(raw_data_path, "imf_pcps_price_volatility_series.csv")

# Recompute when a derived file is missing, when the snapshot workbook moved ahead of
# the derived outputs, or on a forced refresh.
imf_pcps_outputs <- c(imf_pcps_prices_path, imf_pcps_volatility_path, imf_pcps_series_volatility_path)
imf_pcps_stale <- identical(imf_pcps_excel_status, "copied") ||
  (file.exists(imf_pcps_excel_path) && any(vapply(
    imf_pcps_outputs,
    function(p) !file.exists(p) || as.numeric(file.info(imf_pcps_excel_path)$mtime) > as.numeric(file.info(p)$mtime),
    logical(1)
  )))
needs_imf_pcps <- force_refresh || !all(file.exists(imf_pcps_outputs)) || imf_pcps_stale

# The derivation reads the workbook directly, so without it there is nothing to derive.
# This is not fatal: 10_build_themes.R falls back to imf_commodity_prices.csv for the
# Energy Prices theme (see OPSI_ENERGY_PRICES_SOURCE).
if (needs_imf_pcps && !file.exists(imf_pcps_excel_path)) {
  message(
    "IMF PCPS: ", basename(imf_pcps_excel_path), " not found in ", raw_data_path,
    " or sharepoint_raw_dir; skipping the PCPS derivation.\n",
    "  Energy Prices will read imf_commodity_prices.csv instead."
  )
  needs_imf_pcps <- FALSE
}

if (needs_imf_pcps && !skip_data_downloads) {
  old_snapshot_option <- getOption("opportunity_security.raw_snapshot_dir")
  options(opportunity_security.raw_snapshot_dir = raw_data_path)
  source(file.path(repo_root, "scripts", "06_energy_prices_imf.R"))
  end_year <- as.integer(format(Sys.Date(), "%Y"))
  imf_pcps_data <- imf_pcps_energy_prices(start_year = end_year - 9, end_year = end_year)
  write.csv(imf_pcps_data$prices, imf_pcps_prices_path, row.names = FALSE)
  write.csv(imf_pcps_data$tech_vol, imf_pcps_volatility_path, row.names = FALSE)
  write.csv(imf_pcps_data$series_vol, imf_pcps_series_volatility_path, row.names = FALSE)
  options(opportunity_security.raw_snapshot_dir = old_snapshot_option)
}
