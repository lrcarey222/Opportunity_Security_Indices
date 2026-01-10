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
skip_data_downloads <- Sys.getenv("SKIP_DATA_DOWNLOADS") == "1"

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

    source(file.path(repo_root, "R", "themes", "energy_security", "critical_minerals_trade.R"))
    source(file.path(repo_root, "R", "themes", "energy_security", "reserves.R"))

    critical <- read.csv(critical_minerals_path)
    mineral_demand_clean <- reserves_build_mineral_demand_clean(critical)
    crit_hs <- read.csv(critical_minerals_hs_path)
    crit_hs_filtered <- critical_minerals_trade_filter_hs(crit_hs, mineral_demand_clean)
    wdi_country_info <- read.csv(wdi_country_path)

    critmin_import <- comtradr::ct_get_data(
      reporter = wdi_country_info$iso3c,
      partner = "World",
      commodity_code = crit_hs_filtered$hscode,
      start_date = 2024,
      end_date = 2024,
      flow_direction = "import"
    )
    critmin_export <- comtradr::ct_get_data(
      reporter = wdi_country_info$iso3c,
      partner = "World",
      commodity_code = crit_hs_filtered$hscode,
      start_date = 2024,
      end_date = 2024,
      flow_direction = "export"
    )
    total_export <- comtradr::ct_get_data(
      reporter = wdi_country_info$iso3c,
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
