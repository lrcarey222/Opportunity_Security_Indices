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


resolve_comtrade_reporters <- function(wdi_country_info, reporter_ref) {
  reporter_iso_col <- intersect(c("iso_3", "iso3_code", "iso3c"), names(reporter_ref))
  if (length(reporter_iso_col) == 0) {
    stop("Unable to locate ISO3 reporter codes in comtradr reporter reference data.")
  }

  valid_reporters <- reporter_ref[[reporter_iso_col[1]]]
  valid_reporters <- as.character(valid_reporters)
  valid_reporters <- valid_reporters[!is.na(valid_reporters) & nzchar(valid_reporters)]
  valid_reporters <- unique(valid_reporters[nchar(valid_reporters) == 3])

  excluded_iso3 <- c("ASM", "CHI", "GUM", "IMN", "LIE", "MAF", "MCO", "PRI", "XKX")
  valid_reporters <- setdiff(valid_reporters, excluded_iso3)

  wdi_candidates <- character()
  if ("iso3c" %in% names(wdi_country_info)) {
    wdi_candidates <- as.character(wdi_country_info$iso3c)
    wdi_candidates <- wdi_candidates[!is.na(wdi_candidates) & nzchar(wdi_candidates)]
    wdi_candidates <- unique(wdi_candidates[nchar(wdi_candidates) == 3])
    wdi_candidates <- setdiff(wdi_candidates, excluded_iso3)
  }

  reporter_candidates <- intersect(wdi_candidates, valid_reporters)

  # If a malformed or partial WDI file is present, fall back to Comtrade reporter reference
  # so API pulls still run for the full country set.
  if (length(reporter_candidates) < 50) {
    reporter_candidates <- valid_reporters
  }

  if (length(reporter_candidates) == 0) {
    stop("No valid reporter codes remain after filtering against comtradr reference data.")
  }

  reporter_candidates
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

# --- Source: OECD CRS (development assistance) ---
oecd_api_path <- file.path(snapshot_dir, "oecd_crs_api.csv")

if (!file.exists(oecd_api_path)) {
  copy_snapshot_file(file.path(sharepoint_raw_dir, "oecd_crs_api.csv"), oecd_api_path)
}

if (!file.exists(oecd_api_path)) {
  if (skip_data_downloads) {
    message("Skipping OECD CRS API download; missing OECD CRS output in snapshot.")
  } else {
    if (!requireNamespace("httr", quietly = TRUE) ||
        !requireNamespace("readr", quietly = TRUE) ||
        !requireNamespace("glue", quietly = TRUE) ||
        !requireNamespace("purrr", quietly = TRUE)) {
      stop("Packages 'httr', 'readr', 'glue', and 'purrr' are required to ingest OECD CRS data.")
    }
    if (!file.exists(wdi_country_path)) {
      stop("WDI country data missing from snapshot: ", wdi_country_path)
    }

    wdi_country_info <- read.csv(wdi_country_path)
    if (!"iso3c" %in% names(wdi_country_info)) {
      stop("WDI country data missing iso3c column: ", wdi_country_path)
    }

    iso_vec <- wdi_country_info$iso3c
    if ("income" %in% names(wdi_country_info)) {
      iso_vec <- iso_vec[wdi_country_info$income != "High income"]
    }
    iso_vec <- iso_vec[!is.na(iso_vec) & nzchar(iso_vec)]
    
    iso_vec_high <- wdi_country_info$iso3c
    if ("income" %in% names(wdi_country_info)) {
      iso_vec_high <- iso_vec_high[wdi_country_info$income == "High income"]
    }
    iso_vec_high <- iso_vec_high[!is.na(iso_vec_high) & nzchar(iso_vec_high)]

    chunk_size <- 132
    iso_chunks <- split(iso_vec, ceiling(seq_along(iso_vec) / chunk_size))
    iso_chunks_high <- split(iso_vec_high, ceiling(seq_along(iso_vec_high) / chunk_size))

    fetch_chunk <- function(donors, recipients) {
      dons   <- paste(donors, collapse = "+")
      recips <- paste(recipients, collapse = "+")

      url <- glue::glue(
        "https://sdmx.oecd.org/dcd-public/rest/data/",
        "OECD.DCD.FSD,DSD_CRS@DF_CRS,1.4/{dons}.{recips}.",
        "32262+32261+322+321+230+1000.100._T._T.D.Q._T..",
        "?startPeriod=2013",
        "&dimensionAtObservation=AllDimensions",
        "&format=csvfilewithlabels"
      )

      tmp <- tempfile(fileext = ".csv")

      httr::RETRY(
        "GET",
        url,
        httr::user_agent("opportunity-security-indices/1.0"),
        httr::write_disk(tmp, overwrite = TRUE),
        times = 6,
        terminate_on = c(404)
      )

      readr::read_csv(
        tmp,
        col_names = FALSE,
        skip = 1,
        col_types = readr::cols(.default = "c"),
        show_col_types = FALSE
      )
    }

    all_oecd <- purrr::map_dfr(iso_chunks, function(chunk) {
      dat <- fetch_chunk(donors=iso_vec_high, recipients=chunk)
      Sys.sleep(12)
      dat
    })

    readr::write_csv(all_oecd, oecd_api_path)
  }
}

critical_minerals_path <- file.path(snapshot_dir, "iea_criticalminerals_25.csv")
critical_minerals_hs_path <- file.path(
  snapshot_dir,
  "Columbia University Critical Minerals Dashboard",
  "unique_comtrade.csv"
)
energy_trade_codes_path <- file.path(snapshot_dir, "consolidated_hs6_energy_tech_long.csv")

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

split_vec <- function(x, chunk_size) {
  if (length(x) == 0) {
    return(list(character()))
  }
  split(x, ceiling(seq_along(x) / chunk_size))
}

fetch_comtrade_grid <- function(reporters,
                                partners,
                                code_chunks,
                                years,
                                flows,
                                partner_chunk_size = 50,
                                sleep_seconds = 0.5,
                                retries = 5) {
  if (length(reporters) == 0 || length(partners) == 0 || length(code_chunks) == 0) {
    return(data.frame())
  }

  partner_chunks <- split_vec(partners, chunk_size = partner_chunk_size)
  request_grid <- tidyr::expand_grid(
    rep = reporters,
    yr = years,
    dir = flows,
    cc = code_chunks,
    pch = partner_chunks
  )

  output <- vector("list", nrow(request_grid))
  failed <- character()

  for (i in seq_len(nrow(request_grid))) {
    req <- request_grid[i, ]

    data_chunk <- NULL
    last_err <- NULL
    for (attempt in seq_len(retries)) {
      attempt_out <- tryCatch(
        comtradr::ct_get_data(
          reporter = req$rep[[1]],
          partner = req$pch[[1]],
          commodity_code = req$cc[[1]],
          start_date = req$yr[[1]],
          end_date = req$yr[[1]],
          flow_direction = req$dir[[1]]
        ),
        error = function(e) e
      )

      if (!inherits(attempt_out, "error")) {
        data_chunk <- attempt_out
        break
      }

      last_err <- attempt_out
      if (attempt < retries) {
        Sys.sleep(sleep_seconds * attempt)
      }
    }

    if (inherits(last_err, "error") && is.null(data_chunk)) {
      failed <- c(
        failed,
        paste0(
          "rep=", req$rep[[1]],
          ", yr=", req$yr[[1]],
          ", dir=", req$dir[[1]],
          ", codes=", paste(req$cc[[1]], collapse = ","),
          ", partners=", paste(req$pch[[1]], collapse = ","),
          " -> ", conditionMessage(last_err)
        )
      )
      next
    }

    if (!"flow_direction" %in% names(data_chunk)) {
      data_chunk <- dplyr::mutate(data_chunk, flow_direction = req$dir[[1]])
    }
    if ("trade_flow" %in% names(data_chunk)) {
      data_chunk <- dplyr::filter(data_chunk, tolower(trade_flow) == req$dir[[1]])
    }

    output[[i]] <- dplyr::mutate(
      data_chunk,
      reporter_req = req$rep[[1]],
      year_req = req$yr[[1]]
    )

    Sys.sleep(sleep_seconds)
  }

  if (length(failed) > 0) {
    stop(
      "UN Comtrade requests failed for ",
      length(failed),
      " request(s). Example failures:
",
      paste(utils::head(failed, 5), collapse = "
")
    )
  }

  dplyr::bind_rows(output) %>% dplyr::distinct()
}

# --- Source: UN Comtrade (critical minerals trade) ---
critmin_import_path <- file.path(snapshot_dir, "critmin_import_2025.csv")
critmin_export_path <- file.path(snapshot_dir, "critmin_export_2025.csv")
critmin_total_export_path <- file.path(snapshot_dir, "critmin_total_export_2025.csv")

if (!file.exists(critmin_import_path)) {
  copy_snapshot_file(file.path(sharepoint_raw_dir, "critmin_import_2025.csv"), critmin_import_path)
}
if (!file.exists(critmin_export_path)) {
  copy_snapshot_file(file.path(sharepoint_raw_dir, "critmin_export_2025.csv"), critmin_export_path)
}
if (!file.exists(critmin_total_export_path)) {
  copy_snapshot_file(file.path(sharepoint_raw_dir, "critmin_total_export_2025.csv"), critmin_total_export_path)
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
    crit_codes <- crit_hs_filtered$hscode %>%
      as.character() %>%
      stringr::str_replace_all("\\D", "") %>%
      stringr::str_pad(width = 6, side = "left", pad = "0") %>%
      stats::na.omit() %>%
      unique()

    wdi_country_info <- read.csv(wdi_country_path)
    reporter_ref <- comtradr::ct_get_ref_table("reporter")
    reporter_candidates <- resolve_comtrade_reporters(wdi_country_info, reporter_ref)
    crit_code_chunks <- split_by_nchar(crit_codes, max_chars = 2500)

    critmin_import <- fetch_comtrade_grid(
      reporters = reporter_candidates,
      partners = "World",
      code_chunks = crit_code_chunks,
      years = 2025,
      flows = "import",
      partner_chunk_size = 1
    )

    critmin_export <- fetch_comtrade_grid(
      reporters = reporter_candidates,
      partners = "World",
      code_chunks = crit_code_chunks,
      years = 2025,
      flows = "export",
      partner_chunk_size = 1
    )

    total_export <- fetch_comtrade_grid(
      reporters = reporter_candidates,
      partners = "World",
      code_chunks = list("TOTAL"),
      years = 2025,
      flows = "export",
      partner_chunk_size = 1
    )

    write.csv(critmin_import, critmin_import_path, row.names = FALSE)
    write.csv(critmin_export, critmin_export_path, row.names = FALSE)
    write.csv(total_export, critmin_total_export_path, row.names = FALSE)
  }
}

# --- Source: UN Comtrade (energy trade) ---
comtrade_energy_trade_path <- file.path(snapshot_dir, "comtrade_energy_trade.csv")
comtrade_total_export_path <- file.path(snapshot_dir, "comtrade_total_export.csv")
allied_comtrade_energy_path <- file.path(snapshot_dir, "allied_comtrade_energy_data.csv")

comtrade_target_year_env <- suppressWarnings(as.integer(Sys.getenv("COMTRADE_TARGET_YEAR", "")))
comtrade_target_year <- if (!is.na(comtrade_target_year_env)) {
  comtrade_target_year_env
} else {
  as.integer(format(Sys.Date(), "%Y")) - 1
}

comtrade_start_year_env <- suppressWarnings(as.integer(Sys.getenv("COMTRADE_START_YEAR", "")))
comtrade_start_year <- if (!is.na(comtrade_start_year_env)) {
  comtrade_start_year_env
} else {
  comtrade_target_year - 4
}

if (comtrade_start_year > comtrade_target_year) {
  stop("COMTRADE_START_YEAR cannot be greater than COMTRADE_TARGET_YEAR.")
}

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
    comtrade_has_year(comtrade_total_export_path, comtrade_target_year) &&
    comtrade_has_year(allied_comtrade_energy_path, comtrade_target_year)
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
    code_chunks <- split_by_nchar(energy_codes, max_chars = 2500)

    wdi_country_info <- read.csv(wdi_country_path)
    reporter_ref <- comtradr::ct_get_ref_table("reporter")
    reporter_candidates <- resolve_comtrade_reporters(wdi_country_info, reporter_ref)

    ally_reporters <- c(
      "USA", "CAN", "JPN", "AUS", "IND", "MEX", "KOR", "GBR", "DEU", "FRA",
      "ITA", "BRA", "SAU", "ZAF", "IDN", "NOR", "ARE", "VNM", "KEN", "DNK",
      "ARG", "MAR", "CHL"
    )
    ally_reporters <- intersect(ally_reporters, reporter_candidates)

    trade_flows <- c("export", "import")

    energy_trade <- fetch_comtrade_grid(
      reporters = reporter_candidates,
      partners = "World",
      code_chunks = code_chunks,
      years = comtrade_start_year:comtrade_target_year,
      flows = trade_flows,
      partner_chunk_size = 1
    )

    allied_comtrade_energy <- fetch_comtrade_grid(
      reporters = ally_reporters,
      partners = reporter_candidates,
      code_chunks = code_chunks,
      years = comtrade_start_year:comtrade_target_year,
      flows = trade_flows,
      partner_chunk_size = 50
    )

    total_export <- fetch_comtrade_grid(
      reporters = reporter_candidates,
      partners = "World",
      code_chunks = list("TOTAL"),
      years = comtrade_start_year:comtrade_target_year,
      flows = "export",
      partner_chunk_size = 1
    )

    write.csv(energy_trade, comtrade_energy_trade_path, row.names = FALSE)
    write.csv(total_export, comtrade_total_export_path, row.names = FALSE)
    write.csv(allied_comtrade_energy, allied_comtrade_energy_path, row.names = FALSE)
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
