# Ingest raw sources from SharePoint into the configured raw data folder.
resolve_repo_root <- function() {
  if (requireNamespace("rprojroot", quietly = TRUE)) {
    return(rprojroot::find_root(rprojroot::is_git_root))
  }
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  start <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[1]) else normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  d <- dirname(normalizePath(start, winslash = "/", mustWork = FALSE))
  while (!file.exists(file.path(d, ".git")) && dirname(d) != d) d <- dirname(d)
  if (!file.exists(file.path(d, ".git"))) stop("Could not locate repo root.")
  d
}

repo_root <- resolve_repo_root()
config_path <- Sys.getenv("OPSI_CONFIG", file.path(repo_root, "config", "config.yml"))
config <- getOption("opportunity_security.config")
if (is.null(config)) {
  if (!file.exists(config_path)) stop("Config file not found: ", config_path)
  config <- yaml::read_yaml(config_path)
}

sharepoint_raw_dir <- config$sharepoint_raw_dir
raw_data_path <- file.path(repo_root, config$raw_data_dir)
if (!dir.exists(raw_data_path)) dir.create(raw_data_path, recursive = TRUE)

is_skip_data_downloads <- function() tolower(Sys.getenv("SKIP_DATA_DOWNLOADS")) %in% c("1", "true", "yes")
skip_data_downloads <- is_skip_data_downloads()

copy_raw_file <- function(source_path, dest_path) {
  if (!file.exists(source_path)) return(FALSE)
  if (!dir.exists(dirname(dest_path))) dir.create(dirname(dest_path), recursive = TRUE)
  file.copy(source_path, dest_path, overwrite = TRUE)
}

source(file.path(repo_root, "scripts", "utils", "comtrade_ingest_utils.R"))
source(file.path(repo_root, "scripts", "utils", "comtrade_client.R"))

manifest_path <- file.path(repo_root, "config", "raw_inputs_manifest.yml")
manifest <- yaml::read_yaml(manifest_path)
missing <- character()
for (entry in manifest) {
  if (is.null(entry$path) || !nzchar(entry$path)) next
  source_path <- file.path(sharepoint_raw_dir, entry$path)
  dest_path <- file.path(raw_data_path, entry$path)
  if (file.exists(dest_path)) next
  if (!file.exists(source_path)) {
    if (!isTRUE(entry$optional)) missing <- c(missing, entry$path)
    next
  }
  if (!dir.exists(dirname(dest_path))) dir.create(dirname(dest_path), recursive = TRUE)
  if (!file.copy(source_path, dest_path, overwrite = TRUE)) stop("Failed to copy raw input: ", entry$path)
}
if (length(missing) > 0) stop("Missing required raw inputs in sharepoint_raw_dir:\n", paste0("- ", missing, collapse = "\n"))
message("Raw inputs refreshed in: ", raw_data_path)

wdi_gdp_path <- file.path(raw_data_path, "wdi_gdp.csv")
wdi_country_path <- file.path(raw_data_path, "wdi_country_info.csv")
if (!file.exists(wdi_gdp_path)) copy_raw_file(file.path(sharepoint_raw_dir, "wdi_gdp.csv"), wdi_gdp_path)
if (!file.exists(wdi_country_path)) copy_raw_file(file.path(sharepoint_raw_dir, "wdi_country_info.csv"), wdi_country_path)
if ((!file.exists(wdi_gdp_path) || !file.exists(wdi_country_path)) && !skip_data_downloads) {
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
comtrade_years <- seq.int(comtrade_start_year, candidate_target_year)
comtrade_retries <- max(1L, as.integer(Sys.getenv("COMTRADE_MAX_RETRIES", "3")))
comtrade_sleep_seconds <- as.numeric(Sys.getenv("COMTRADE_SLEEP_SECONDS", "0.5"))
comtrade_pause_seconds <- as.numeric(Sys.getenv("COMTRADE_REQUEST_PAUSE_SECONDS", "0.2"))
comtrade_timeout_seconds <- as.numeric(Sys.getenv("COMTRADE_REQUEST_TIMEOUT_SECONDS", "120"))

subset_request_chunk <- function(request_df, chunk_index, chunk_count) {
  if (chunk_count <= 1 || nrow(request_df) == 0) return(request_df)
  idx <- seq_len(nrow(request_df))
  request_df[idx[((idx - 1) %% chunk_count) + 1 == chunk_index], , drop = FALSE]
}

build_requests <- function(reporters, partners, commodity_codes, years, flows, partner_chunk_size = 50) {
  partner_chunks <- split_vec(partners, chunk_size = partner_chunk_size)
  tidyr::expand_grid(
    reporter = reporters,
    start_date = years,
    end_date = years,
    flow_direction = flows,
    commodity_code = commodity_codes,
    partner = partner_chunks
  )
}

if (!skip_data_downloads) {
  comtrade_set_key_from_env()
}

critical_minerals_path <- file.path(raw_data_path, "iea_criticalminerals_25.csv")
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

# Critical minerals outputs (backward compatible filenames)
critmin_import_path <- file.path(raw_data_path, "critmin_import_2025.csv")
critmin_export_path <- file.path(raw_data_path, "critmin_export_2025.csv")
critmin_total_export_path <- file.path(raw_data_path, "critmin_total_export_2025.csv")

if (!skip_data_downloads) {
  source(file.path(repo_root, "R", "categories", "minerals_trade", "critical_minerals_trade.R"))
  source(file.path(repo_root, "R", "categories", "reserves", "reserves.R"))
  critical <- read.csv(critical_minerals_path)
  mineral_demand_clean <- reserves_build_mineral_demand_clean(critical)
  crit_hs <- read.csv(critical_minerals_hs_path)
  crit_codes <- critical_minerals_trade_filter_hs(crit_hs, mineral_demand_clean)$hscode %>%
    as.character() %>% stringr::str_replace_all("\\D", "") %>% stringr::str_pad(width = 6, side = "left", pad = "0") %>%
    stats::na.omit() %>% unique()
  crit_code_chunks <- split_by_nchar(crit_codes, max_chars = 2500)

  crit_probe_builder <- function(y) build_requests(reporter_candidates[1], "World", crit_code_chunks[1], y, "import", partner_chunk_size = 1)
  crit_target_year <- comtrade_pick_latest_available_year(function(y) probe_year(crit_probe_builder, y), candidate_target_year)
  actual_years$critmin <- crit_target_year

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
if (!file.exists(imf_pcps_excel_path)) {
  candidates <- c(file.path(sharepoint_raw_dir, "IMF_PCPS_all.xlsx"), file.path(raw_data_path, "IMF_PCPS_all.xlsx"))
  for (candidate in candidates) {
    if (file.exists(candidate)) {
      copy_raw_file(candidate, imf_pcps_excel_path)
      break
    }
  }
}

imf_pcps_prices_path <- file.path(raw_data_path, "imf_pcps_prices.csv")
imf_pcps_volatility_path <- file.path(raw_data_path, "imf_pcps_price_volatility.csv")
imf_pcps_series_volatility_path <- file.path(raw_data_path, "imf_pcps_price_volatility_series.csv")
needs_imf_pcps <- !(file.exists(imf_pcps_prices_path) && file.exists(imf_pcps_volatility_path) && file.exists(imf_pcps_series_volatility_path))
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
