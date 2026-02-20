# --- FILE: scripts/30_build_allied_network_design.R --------------------------
# Runs the allied network design module using existing pipeline outputs
# and writes CSVs to outputs_dir.

resolve_repo_root_from_script <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  script_path <- if (length(file_arg) > 0) {
    sub("^--file=", "", file_arg[1])
  } else if (!is.null(sys.frame(1)$ofile)) {
    sys.frame(1)$ofile
  } else {
    ""
  }

  start_dir <- if (nzchar(script_path)) {
    normalizePath(dirname(script_path), winslash = "/", mustWork = FALSE)
  } else {
    normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  }

  d <- start_dir
  while (!file.exists(file.path(d, ".git")) && dirname(d) != d) {
    d <- dirname(d)
  }
  if (!file.exists(file.path(d, ".git"))) {
    stop("Could not locate repo root (no .git found).")
  }
  d
}

if (!exists("repo_root") || !nzchar(repo_root)) {
  repo_root <- resolve_repo_root_from_script()
}

if (is.null(getOption("opportunity_security.config"))) {
  source(file.path(repo_root, "scripts", "00_setup.R"))
}

ensure_cran_repo <- function() {
  repos <- getOption("repos")
  cran <- if (is.null(repos) || is.null(repos[["CRAN"]])) "" else repos[["CRAN"]]
  if (is.null(repos) || identical(cran, "@CRAN@") || is.na(cran) || cran == "") {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }
}

ensure_user_lib <- function() {
  user_lib <- Sys.getenv("R_LIBS_USER")
  if (!nzchar(user_lib)) {
    user_lib <- file.path(
      Sys.getenv("USERPROFILE"),
      "Documents",
      "R",
      "win-library",
      paste0(getRversion()[1, 1:2], collapse = ".")
    )
  }
  if (!dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
  .libPaths(c(user_lib, .libPaths()))
  invisible(user_lib)
}

ensure_milp_pkgs <- function(install_if_missing = TRUE, require_milp = FALSE) {
  milp_pkgs <- c("ompr", "ompr.roi", "ROI", "ROI.plugin.glpk")
  missing <- milp_pkgs[!vapply(milp_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) == 0) return(TRUE)

  if (!install_if_missing) {
    if (require_milp) stop("MILP required but missing packages: ", paste(missing, collapse = ", "))
    return(FALSE)
  }

  ensure_cran_repo()
  ensure_user_lib()
  message("MILP packages missing; attempting install on Windows: ", paste(missing, collapse = ", "))
  ok <- TRUE
  tryCatch({
    install.packages(missing, dependencies = TRUE)
  }, error = function(e) {
    ok <<- FALSE
    message("Automatic install failed: ", conditionMessage(e))
  })

  missing2 <- milp_pkgs[!vapply(milp_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing2) == 0) return(TRUE)

  msg <- paste0(
    "MILP packages still missing after install attempt: ", paste(missing2, collapse = ", "), "\n",
    "On Windows this usually means you need permissions to write to your R library or a build toolchain.\n",
    "Try running R/RStudio as Administrator, or set a user library (R_LIBS_USER), or install Rtools if compilation is required."
  )
  if (require_milp) stop(msg) else message(msg)
  FALSE
}

install_if_missing <- tolower(Sys.getenv("OPSI_INSTALL_MILP_PKGS", "true")) %in% c("1", "true", "yes")
require_milp <- tolower(Sys.getenv("OPSI_REQUIRE_MILP", "false")) %in% c("1", "true", "yes")
milp_available <- ensure_milp_pkgs(install_if_missing = install_if_missing, require_milp = require_milp)

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(repo_root, "R", "indices", "allied_network_design.R"))

config <- getOption("opportunity_security.config")
if (is.null(config)) stop("Config not loaded; run scripts/00_setup.R first.")
processed_dir <- file.path(repo_root, config$processed_dir)
if (!dir.exists(processed_dir)) stop("Processed data directory not found: ", processed_dir)

read_processed_tbl <- function(name) {
  path <- file.path(processed_dir, paste0(name, ".rds"))
  if (!file.exists(path)) stop("Processed table not found: ", path)
  readRDS(path)
}

# Index outputs path
outputs_rds_path <- Sys.getenv("OPSI_OUTPUTS_RDS", "")
if (!nzchar(outputs_rds_path)) {
  outputs_rds_path <- if (!is.null(config$outputs_rds) && nzchar(config$outputs_rds)) {
    file.path(repo_root, config$outputs_rds)
  } else {
    file.path(processed_dir, "outputs", "index_outputs.rds")
  }
}
if (!file.exists(outputs_rds_path)) stop("Index outputs not found: ", outputs_rds_path)
index_outputs <- readRDS(outputs_rds_path)

economic_opportunity_index <- index_outputs$economic_opportunity_index
energy_security_index      <- index_outputs$energy_security_index
policy_index               <- index_outputs$policy_index

if (is.null(economic_opportunity_index) || is.null(energy_security_index) || is.null(policy_index)) {
  stop("Index outputs missing one of: economic_opportunity_index, energy_security_index, policy_index")
}

# Partner dyad tables (from scripts/15_build_partner_themes.R)
partner_friendshore_tbl <- read_processed_tbl("partner_friendshore_tbl")
partner_opportunity_tbl <- read_processed_tbl("partner_opportunity_tbl")
partner_development_country_tbl <- NULL
dev_path <- file.path(processed_dir, "partner_development_country_tbl.rds")
if (file.exists(dev_path)) {
  partner_development_country_tbl <- readRDS(dev_path)
}

# Load country_info from the configured raw data directory.
is_skip_data_downloads <- function() {
  tolower(Sys.getenv("SKIP_DATA_DOWNLOADS")) %in% c("1", "true", "yes")
}
skip_data_downloads <- is_skip_data_downloads()

resolve_raw_data_dir <- function(root_dir, raw_data_dir, skip_data_downloads = FALSE) {
  raw_base_dir <- file.path(root_dir, raw_data_dir)
  if (!dir.exists(raw_base_dir)) {
    if (skip_data_downloads) return(NULL)
    stop("Raw data directory not found: ", raw_base_dir)
  }
  raw_base_dir
}

raw_data_path <- resolve_raw_data_dir(repo_root, config$raw_data_dir, skip_data_downloads)
if (is.null(raw_data_path)) stop("Raw data directory not found (and SKIP_DATA_DOWNLOADS is set).")

wdi_country_path <- file.path(raw_data_path, "wdi_country_info.csv")
country_info <- read.csv(wdi_country_path)
country_info <- standardize_country_info(country_info)

load_country_size_tbl <- function(raw_data_path, country_info, iso3c_network) {
  gdp_path <- file.path(raw_data_path, "wdi_gdp.csv")
  if (!file.exists(gdp_path)) stop("GDP file not found: ", gdp_path)
  gdp_raw <- read.csv(gdp_path, stringsAsFactors = FALSE)

  lower_names <- tolower(names(gdp_raw))
  year_col <- names(gdp_raw)[match(TRUE, lower_names %in% c("year"), nomatch = 0)]
  country_col <- names(gdp_raw)[match(TRUE, lower_names %in% c("country", "country_name"), nomatch = 0)]
  iso_col <- names(gdp_raw)[match(TRUE, lower_names %in% c("iso3c", "iso3", "iso_code"), nomatch = 0)]

  gdp_col <- if ("NY.GDP.MKTP.CD" %in% names(gdp_raw)) {
    "NY.GDP.MKTP.CD"
  } else {
    candidate <- names(gdp_raw)[vapply(gdp_raw, is.numeric, logical(1))]
    candidate <- setdiff(candidate, c(year_col, "iso2c", "country", "Country", "iso3c"))
    if (!length(candidate)) stop("No usable GDP numeric column found in wdi_gdp.csv")
    candidate[[1]]
  }

  if (is.null(country_col) || !nzchar(country_col)) stop("Could not identify country column in wdi_gdp.csv")

  gdp_tbl <- gdp_raw %>%
    dplyr::transmute(
      country = standardize_country_names(.data[[country_col]]),
      iso3c = if (!is.null(iso_col) && nzchar(iso_col)) toupper(as.character(.data[[iso_col]])) else NA_character_,
      year = if (!is.null(year_col) && nzchar(year_col)) suppressWarnings(as.numeric(.data[[year_col]])) else NA_real_,
      gdp_usd = suppressWarnings(as.numeric(.data[[gdp_col]]))
    ) %>%
    dplyr::left_join(country_info %>% dplyr::transmute(country = standardize_country_names(country), iso3c_ref = toupper(as.character(iso3c))), by = "country") %>%
    dplyr::mutate(iso3c = dplyr::coalesce(iso3c, iso3c_ref)) %>%
    dplyr::select(-iso3c_ref) %>%
    dplyr::filter(iso3c %in% iso3c_network) %>%
    dplyr::group_by(iso3c) %>%
    dplyr::arrange(dplyr::desc(year), .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  med <- stats::median(gdp_tbl$gdp_usd[is.finite(gdp_tbl$gdp_usd)], na.rm = TRUE)
  if (!is.finite(med)) med <- 1

  tibble::tibble(iso3c = iso3c_network) %>%
    dplyr::left_join(gdp_tbl, by = "iso3c") %>%
    dplyr::left_join(country_info %>% dplyr::transmute(iso3c = toupper(as.character(iso3c)), country_ref = country), by = "iso3c") %>%
    dplyr::mutate(
      country = dplyr::coalesce(country, country_ref),
      year = suppressWarnings(as.numeric(year)),
      gdp_imputed = !is.finite(gdp_usd),
      gdp_usd = dplyr::if_else(is.finite(gdp_usd), gdp_usd, med)
    ) %>%
    dplyr::select(iso3c, country, year, gdp_usd, gdp_imputed)
}


# Coalition definition (your list)
iso3c_network <- c(
  "USA","CAN","JPN","AUS","IND","MEX","KOR","GBR","DEU","FRA","ITA","BRA","SAU",
  "ZAF","IDN","NOR","UAE","VNM","KEN","DNK","ARG","MAR","CHL"
)

# Run
country_size_tbl <- load_country_size_tbl(raw_data_path, country_info, iso3c_network)
fmt_time <- function(seconds) {
  if (!is.finite(seconds) || is.na(seconds)) return("--:--")
  seconds <- max(0, round(seconds))
  mins <- seconds %/% 60
  secs <- seconds %% 60
  sprintf("%02d:%02d", mins, secs)
}

progress_bar <- NULL
on.exit({
  if (!is.null(progress_bar)) close(progress_bar)
}, add = TRUE)

progress_callback <- function(info) {
  if (!is.list(info) || is.null(info$event)) return(invisible(NULL))
  if (info$event == "start_stage") {
    if (is.null(progress_bar)) {
      progress_bar <<- utils::txtProgressBar(min = 0, max = info$total, style = 3)
    }
    return(invisible(NULL))
  }
  if (info$event == "fallback_greedy") {
    cat(sprintf("  fallback to greedy for %s / %s: %s\n", info$tech, info$supply_chain, info$reason))
    return(invisible(NULL))
  }
  if (info$event == "end_stage" && !is.null(progress_bar)) {
    utils::setTxtProgressBar(progress_bar, info$current)
    pct_remaining <- max(0, min(100, 100 * info$pct_remaining))
    msg <- sprintf(
      "  %s/%s complete | %0.1f%% remaining | ETA %s | method: %s | stage: %s / %s",
      info$current,
      info$total,
      pct_remaining,
      fmt_time(info$eta_sec),
      info$method,
      info$tech,
      info$supply_chain
    )
    cat(msg, "\n")
  }
  invisible(NULL)
}


if (milp_available) {
  message("MILP solver stack detected (ompr/ROI/GLPK). method='auto' will use MILP (unless stage too large or solver errors).")
} else {
  message("MILP solver stack not detected. method='auto' will use greedy fallback.")
}

res <- allied_network_design(
  economic_opportunity_index = economic_opportunity_index,
  energy_security_index = energy_security_index,
  policy_index = policy_index,
  partner_friendshore_tbl = partner_friendshore_tbl,
  partner_opportunity_tbl = partner_opportunity_tbl,
  partner_development_country_tbl = partner_development_country_tbl,
  country_info = country_info,
  iso3c_network = iso3c_network,
  method = "auto",        # uses MILP if installed, otherwise greedy
  min_producers = 3,
  max_share = 0.40,
  min_share = 0.05,
  min_suppliers_per_consumer = 2,
  epsilon_supplier_share = 0.10,
  allow_self = TRUE,
  w_node = 1.0,
  w_edge = 0.5,
  w_dev = 0.0,
  progress_callback = progress_callback,
  auto_milp_max_nodes = 18,
  milp_stage_time_limit_sec = 120,
  country_size_tbl = country_size_tbl,
  demand_mode = "mixed",
  demand_weights = list(need = 0.5, size = 0.5),
  portfolio_enable = TRUE,
  portfolio_top_k = 5,
  portfolio_min_cap = 2,
  portfolio_max_cap = 10,
  portfolio_transform = "log"
)

outputs_dir <- if (!is.null(config$outputs_dir) && nzchar(config$outputs_dir)) {
  file.path(repo_root, config$outputs_dir)
} else {
  file.path(repo_root, config$processed_dir, "outputs")
}
if (!dir.exists(outputs_dir)) dir.create(outputs_dir, recursive = TRUE)

utils::write.csv(res$specialization, file.path(outputs_dir, "allied_network_specialization.csv"), row.names = FALSE)
utils::write.csv(res$flows,          file.path(outputs_dir, "allied_network_flows.csv"),         row.names = FALSE)
utils::write.csv(res$diagnostics,    file.path(outputs_dir, "allied_network_diagnostics.csv"),   row.names = FALSE)
if (!is.null(res$build_candidates)) {
  utils::write.csv(res$build_candidates, file.path(outputs_dir, "allied_network_build_candidates.csv"), row.names = FALSE)
}

if (!is.null(res$portfolio)) {
  utils::write.csv(res$portfolio$caps, file.path(outputs_dir, "allied_network_portfolio_caps.csv"), row.names = FALSE)
  utils::write.csv(res$portfolio$counts, file.path(outputs_dir, "allied_network_portfolio_counts.csv"), row.names = FALSE)
  utils::write.csv(res$portfolio$topk, file.path(outputs_dir, "allied_network_topk_by_stage.csv"), row.names = FALSE)
}

message("Wrote allied network outputs to: ", outputs_dir)
