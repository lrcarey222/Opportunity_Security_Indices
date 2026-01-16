# Ingest raw sources from SharePoint into a snapshot folder.
resolve_repo_root <- function() {
  # Prefer rprojroot if available (most robust)
  if (requireNamespace("rprojroot", quietly = TRUE)) {
    return(rprojroot::find_root(rprojroot::is_git_root))
  }
  
  # Fallback: start from script path if we have it, otherwise from getwd()
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  
  start <- if (length(file_arg) > 0) {
    sub("^--file=", "", file_arg[1])
  } else if (!is.null(sys.frame(1)$ofile)) {
    sys.frame(1)$ofile
  } else {
    ""
  }
  
  d <- if (nzchar(start)) {
    dirname(normalizePath(start, winslash = "/", mustWork = FALSE))
  } else {
    normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  }
  
  # Walk up until we find a .git directory
  while (!file.exists(file.path(d, ".git")) && dirname(d) != d) {
    d <- dirname(d)
  }
  
  if (!file.exists(file.path(d, ".git"))) {
    stop("Could not locate repo root (no .git found). Run from the repo directory or set OPSI_CONFIG/OPSI_WEIGHTS.")
  }
  
  d
}


repo_root <- resolve_repo_root()
config_path <- Sys.getenv("OPSI_CONFIG", file.path(repo_root, "config", "config.yml"))
weights_path <- Sys.getenv("OPSI_WEIGHTS", file.path(repo_root, "config", "weights.yml"))

config <- getOption("opportunity_security.config")
if (is.null(config)) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required to load config files.")
  }
  if (!file.exists(config_path)) {
    stop("Config file not found: ", config_path)
  }
  config <- yaml::read_yaml(config_path)
}

sharepoint_raw_dir <- config$sharepoint_raw_dir
raw_data_dir <- file.path(repo_root, config$raw_data_dir)
is_skip_data_downloads <- function() {
  tolower(Sys.getenv("SKIP_DATA_DOWNLOADS")) %in% c("1", "true", "yes")
}
skip_data_downloads <- is_skip_data_downloads()

manifest_path <- file.path(repo_root, "config", "raw_inputs_manifest.yml")
if (!file.exists(manifest_path)) {
  stop("Raw inputs manifest not found: ", manifest_path)
}

manifest <- yaml::read_yaml(manifest_path)
if (length(manifest) == 0) {
  stop("Raw inputs manifest is empty: ", manifest_path)
}

snapshot_date <- format(Sys.Date(), "%Y-%m-%d")
snapshot_dir <- file.path(raw_data_dir, snapshot_date)
if (!dir.exists(snapshot_dir)) {
  dir.create(snapshot_dir, recursive = TRUE)
}

missing <- character()

# --- Source: SharePoint raw inputs (config/raw_inputs_manifest.yml) ---
for (entry in manifest) {
  if (is.null(entry$path) || !nzchar(entry$path)) {
    next
  }
  is_optional <- isTRUE(entry$optional)
  source_path <- file.path(sharepoint_raw_dir, entry$path)
  dest_path <- file.path(snapshot_dir, entry$path)

  if (file.exists(dest_path)) {
    next
  }

  if (!file.exists(source_path)) {
    if (!is_optional) {
      missing <- c(missing, entry$path)
    }
    next
  }

  dest_dir <- dirname(dest_path)
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  copied <- file.copy(source_path, dest_path, overwrite = TRUE)
  if (!copied) {
    stop("Failed to copy raw input: ", source_path, " -> ", dest_path)
  }
}

if (length(missing) > 0) {
  missing_list <- paste(paste0("- ", missing), collapse = "\n")
  stop("Missing required raw inputs in sharepoint_raw_dir:\n", missing_list)
}

message("Raw inputs snapshot created at: ", snapshot_dir)

# --- Supplemental API pulls ---
# These API pulls are kept in the ingest stage so downstream steps only read
# local snapshot files. This keeps theme builders focused on transformations.

# --- Source: World Bank WDI (GDP + country info) ---
wdi_gdp_path <- file.path(snapshot_dir, "wdi_gdp.csv")
wdi_country_path <- file.path(snapshot_dir, "wdi_country_info.csv")

copy_snapshot_file <- function(source_path, dest_path) {
  if (!file.exists(source_path)) {
    return(FALSE)
  }
  dest_dir <- dirname(dest_path)
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }
  file.copy(source_path, dest_path, overwrite = TRUE)
}

if (!file.exists(wdi_gdp_path)) {
  copy_snapshot_file(file.path(sharepoint_raw_dir, "wdi_gdp.csv"), wdi_gdp_path)
}
if (!file.exists(wdi_country_path)) {
  copy_snapshot_file(file.path(sharepoint_raw_dir, "wdi_country_info.csv"), wdi_country_path)
}

if (!file.exists(wdi_gdp_path) || !file.exists(wdi_country_path)) {
  if (skip_data_downloads) {
    message("Skipping WDI download; missing WDI outputs in snapshot.")
  } else {
    if (!requireNamespace("WDI", quietly = TRUE)) {
      stop("Package 'WDI' is required to ingest World Bank GDP and country data.")
    }
    wdi_gdp <- WDI::WDI(indicator = "NY.GDP.MKTP.CD", start = 2007, end = 2024)
    write.csv(wdi_gdp, wdi_gdp_path, row.names = FALSE)

    wdi_country_info <- WDI::WDI_data$country
    write.csv(wdi_country_info, wdi_country_path, row.names = FALSE)
  }
}

critical_minerals_path <- file.path(snapshot_dir, "iea_criticalminerals_25.csv")
critical_minerals_hs_path <- file.path(
  snapshot_dir,
  "Columbia University Critical Minerals Dashboard",
  "unique_comtrade.csv"
)
energy_trade_codes_path <- file.path(snapshot_dir, "consolidated_hs6_energy_tech_long.csv")

# --- Source: UN Comtrade (critical minerals trade) ---
critmin_import_path <- file.path(snapshot_dir, "critmin_import_2024.csv")
critmin_export_path <- file.path(snapshot_dir, "critmin_export_2024.csv")
critmin_total_export_path <- file.path(snapshot_dir, "critmin_total_export_2024.csv")

if (!file.exists(critmin_import_path)) {
  copy_snapshot_file(file.path(sharepoint_raw_dir, "critmin_import_2024.csv"), critmin_import_path)
}
if (!file.exists(critmin_export_path)) {
  copy_snapshot_file(file.path(sharepoint_raw_dir, "critmin_export_2024.csv"), critmin_export_path)
}
if (!file.exists(critmin_total_export_path)) {
  copy_snapshot_file(file.path(sharepoint_raw_dir, "critmin_total_export_2024.csv"), critmin_total_export_path)
}

needs_comtrade <- !(
  file.exists(critmin_import_path) &&
    file.exists(critmin_export_path) &&
    file.exists(critmin_total_export_path)
)

if (needs_comtrade) {
  if (skip_data_downloads) {
    message("Skipping comtrade download; missing critical minerals trade outputs in snapshot.")
  } else {
    if (!requireNamespace("comtradr", quietly = TRUE)) {
      stop("Package 'comtradr' is required to ingest critical minerals trade data.")
    }

    comtrade_key <- Sys.getenv("COMTRADE_API_KEY")
    if (comtrade_key == "") {
      stop("COMTRADE_API_KEY environment variable must be set to ingest critical minerals trade data.")
    }
    comtradr::set_primary_comtrade_key(comtrade_key)

    if (!file.exists(critical_minerals_path)) {
      stop("Critical minerals dataset missing from snapshot: ", critical_minerals_path)
    }
    if (!file.exists(critical_minerals_hs_path)) {
      stop("Critical minerals HS dataset missing from snapshot: ", critical_minerals_hs_path)
    }
    if (!file.exists(wdi_country_path)) {
      stop("WDI country data missing from snapshot: ", wdi_country_path)
    }

    source(file.path(repo_root, "R", "categories", "minerals_trade", "critical_minerals_trade.R"))
    source(file.path(repo_root, "R", "categories", "reserves", "reserves.R"))

    critical <- read.csv(critical_minerals_path)
    mineral_demand_clean <- reserves_build_mineral_demand_clean(critical)
    crit_hs <- read.csv(critical_minerals_hs_path)
    crit_hs_filtered <- critical_minerals_trade_filter_hs(crit_hs, mineral_demand_clean)
    wdi_country_info <- read.csv(wdi_country_path)

    reporter_candidates <- wdi_country_info$iso3c
    reporter_candidates <- reporter_candidates[!is.na(reporter_candidates) & nzchar(reporter_candidates)]
    reporter_candidates <- unique(reporter_candidates)

    reporter_ref <- comtradr::ct_get_ref_table("reporter")
    reporter_iso_col <- intersect(c("iso_3", "iso3_code", "iso3c"), names(reporter_ref))
    if (length(reporter_iso_col) == 0) {
      stop("Unable to locate ISO3 reporter codes in comtradr reporter reference data.")
    }
    valid_reporters <- reporter_ref[[reporter_iso_col[1]]]
    valid_reporters <- unique(valid_reporters[!is.na(valid_reporters) & nzchar(valid_reporters)])
    reporter_candidates <- intersect(reporter_candidates, valid_reporters)
    if (length(reporter_candidates) == 0) {
      stop("No valid reporter codes remain after filtering against comtradr reference data.")
    }

    critmin_import <- comtradr::ct_get_data(
      reporter = reporter_candidates,
      partner = "World",
      commodity_code = crit_hs_filtered$hscode,
      start_date = 2024,
      end_date = 2024,
      flow_direction = "import"
    )
    critmin_export <- comtradr::ct_get_data(
      reporter = reporter_candidates,
      partner = "World",
      commodity_code = crit_hs_filtered$hscode,
      start_date = 2024,
      end_date = 2024,
      flow_direction = "export"
    )
    total_export <- comtradr::ct_get_data(
      reporter = reporter_candidates,
      partner = "World",
      commodity_code = "TOTAL",
      start_date = 2024,
      end_date = 2024,
      flow_direction = "export"
    )

    write.csv(critmin_import, critmin_import_path, row.names = FALSE)
    write.csv(critmin_export, critmin_export_path, row.names = FALSE)
    write.csv(total_export, critmin_total_export_path, row.names = FALSE)
  }
}

# --- Source: UN Comtrade (energy trade) ---
comtrade_energy_trade_path <- file.path(snapshot_dir, "comtrade_energy_trade.csv")
comtrade_total_export_path <- file.path(snapshot_dir, "comtrade_total_export.csv")

comtrade_target_year <- as.integer(format(Sys.Date(), "%Y")) - 1

comtrade_has_year <- function(path, year) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  data <- read.csv(path)
  year_col <- intersect(c("period", "ref_year", "year", "Year"), names(data))
  if (length(year_col) == 0) {
    return(FALSE)
  }
  max_year <- suppressWarnings(max(as.integer(data[[year_col[[1]]]]), na.rm = TRUE))
  !is.na(max_year) && max_year >= year
}

needs_energy_comtrade <- !(
  comtrade_has_year(comtrade_energy_trade_path, comtrade_target_year) &&
    comtrade_has_year(comtrade_total_export_path, comtrade_target_year)
)

if (needs_energy_comtrade) {
  if (skip_data_downloads) {
    message("Skipping comtrade download; missing energy trade outputs in snapshot.")
  } else {
    if (!requireNamespace("comtradr", quietly = TRUE)) {
      stop("Package 'comtradr' is required to ingest energy trade data.")
    }

    comtrade_key <- Sys.getenv("COMTRADE_API_KEY")
    if (comtrade_key == "") {
      stop("COMTRADE_API_KEY environment variable must be set to ingest energy trade data.")
    }
    comtradr::set_primary_comtrade_key(comtrade_key)

    if (!file.exists(energy_trade_codes_path)) {
      stop("Energy trade HS6 codes missing from snapshot: ", energy_trade_codes_path)
    }
    if (!file.exists(wdi_country_path)) {
      stop("WDI country data missing from snapshot: ", wdi_country_path)
    }

    energy_trade_codes <- read.csv(energy_trade_codes_path)
    energy_codes <- energy_trade_codes$HS6 %>%
      as.character() %>%
      stringr::str_replace_all("\\D", "") %>%
      stringr::str_pad(width = 6, side = "left", pad = "0") %>%
      stats::na.omit() %>%
      unique()

    split_by_nchar <- function(x, max_chars = 2500) {
      chunks <- list()
      cur <- character()
      cur_len <- 0
      for (code in x) {
        add_len <- nchar(code) + ifelse(length(cur) == 0, 0, 1)
        if (cur_len + add_len > max_chars) {
          chunks[[length(chunks) + 1]] <- cur
          cur <- code
          cur_len <- nchar(code)
        } else {
          cur <- c(cur, code)
          cur_len <- cur_len + add_len
        }
      }
      if (length(cur)) {
        chunks[[length(chunks) + 1]] <- cur
      }
      chunks
    }

    code_chunks <- split_by_nchar(energy_codes, max_chars = 2500)

    wdi_country_info <- read.csv(wdi_country_path)
    reporter_candidates <- wdi_country_info$iso3c
    reporter_candidates <- reporter_candidates[!is.na(reporter_candidates) & nzchar(reporter_candidates)]
    reporter_candidates <- unique(reporter_candidates)

    reporter_ref <- comtradr::ct_get_ref_table("reporter")
    reporter_iso_col <- intersect(c("iso_3", "iso3_code", "iso3c"), names(reporter_ref))
    if (length(reporter_iso_col) == 0) {
      stop("Unable to locate ISO3 reporter codes in comtradr reporter reference data.")
    }
    valid_reporters <- reporter_ref[[reporter_iso_col[1]]]
    valid_reporters <- unique(valid_reporters[!is.na(valid_reporters) & nzchar(valid_reporters)])
    reporter_candidates <- intersect(reporter_candidates, valid_reporters)
    if (length(reporter_candidates) == 0) {
      stop("No valid reporter codes remain after filtering against comtradr reference data.")
    }

    safe_ct <- purrr::possibly(comtradr::ct_get_data, otherwise = NULL)
    trade_flows <- c("export", "import")

    energy_trade_list <- list()
    idx <- 1
    for (flow in trade_flows) {
      for (chunk in code_chunks) {
        data_chunk <- safe_ct(
          reporter = reporter_candidates,
          partner = "World",
          commodity_code = chunk,
          start_date = comtrade_target_year,
          end_date = comtrade_target_year,
          flow_direction = flow
        )
        if (!is.null(data_chunk)) {
          if (!"flow_direction" %in% names(data_chunk)) {
            data_chunk <- dplyr::mutate(data_chunk, flow_direction = flow)
          }
          energy_trade_list[[idx]] <- data_chunk
          idx <- idx + 1
        }
      }
    }

    energy_trade <- dplyr::bind_rows(energy_trade_list) %>% dplyr::distinct()

    total_export <- comtradr::ct_get_data(
      reporter = reporter_candidates,
      partner = "World",
      commodity_code = "TOTAL",
      start_date = comtrade_target_year,
      end_date = comtrade_target_year,
      flow_direction = "export"
    )

    write.csv(energy_trade, comtrade_energy_trade_path, row.names = FALSE)
    write.csv(total_export, comtrade_total_export_path, row.names = FALSE)
  }
}

# --- Source: IMF Primary Commodity Price System (PCPS) ---
imf_pcps_excel_path <- file.path(snapshot_dir, "IMF_PCPS_all.xlsx")
if (!file.exists(imf_pcps_excel_path)) {
  imf_pcps_candidates <- c(
    file.path(sharepoint_raw_dir, "IMF_PCPS_all.xlsx"),
    file.path(raw_data_dir, "IMF_PCPS_all.xlsx")
  )
  for (candidate in imf_pcps_candidates) {
    if (file.exists(candidate)) {
      copy_snapshot_file(candidate, imf_pcps_excel_path)
      break
    }
  }
}
if (!file.exists(imf_pcps_excel_path)) {
  if (skip_data_downloads) {
    message("Skipping IMF PCPS snapshot lookup; missing file: ", imf_pcps_excel_path)
  } else {
    stop("IMF PCPS Excel snapshot missing: ", imf_pcps_excel_path)
  }
}

imf_pcps_prices_path <- file.path(snapshot_dir, "imf_pcps_prices.csv")
imf_pcps_volatility_path <- file.path(snapshot_dir, "imf_pcps_price_volatility.csv")
imf_pcps_series_volatility_path <- file.path(snapshot_dir, "imf_pcps_price_volatility_series.csv")

needs_imf_pcps <- !(
  file.exists(imf_pcps_prices_path) &&
    file.exists(imf_pcps_volatility_path) &&
    file.exists(imf_pcps_series_volatility_path)
)

if (needs_imf_pcps) {
  if (skip_data_downloads) {
    message("Skipping IMF PCPS processing because SKIP_DATA_DOWNLOADS is enabled.")
    needs_imf_pcps <- FALSE
  }
}

if (needs_imf_pcps) {
  old_snapshot_option <- getOption("opportunity_security.raw_snapshot_dir")
  options(opportunity_security.raw_snapshot_dir = snapshot_dir)

  source(file.path(repo_root, "scripts", "06_energy_prices_imf.R"))
  end_year <- as.integer(format(Sys.Date(), "%Y"))
  start_year <- end_year - 9
  imf_pcps_data <- imf_pcps_energy_prices(start_year = start_year, end_year = end_year)

  write.csv(imf_pcps_data$prices, imf_pcps_prices_path, row.names = FALSE)
  write.csv(imf_pcps_data$tech_vol, imf_pcps_volatility_path, row.names = FALSE)
  write.csv(imf_pcps_data$series_vol, imf_pcps_series_volatility_path, row.names = FALSE)

  if (is.null(old_snapshot_option)) {
    options(opportunity_security.raw_snapshot_dir = NULL)
  } else {
    options(opportunity_security.raw_snapshot_dir = old_snapshot_option)
  }
}
