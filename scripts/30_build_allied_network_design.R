# --- FILE: scripts/30_build_allied_network_design.R --------------------------
# Runs the allied network design module using existing pipeline outputs
# and writes CSVs to outputs_dir.

if (!exists("repo_root")) {
  repo_root <- resolve_repo_root()
}

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

# Load country_info from the latest raw snapshot (same approach as scripts/15_build_partner_themes.R)
is_skip_data_downloads <- function() {
  tolower(Sys.getenv("SKIP_DATA_DOWNLOADS")) %in% c("1", "true", "yes")
}
skip_data_downloads <- is_skip_data_downloads()

latest_raw_snapshot <- function(root_dir, raw_data_dir, skip_data_downloads = FALSE) {
  raw_base_dir <- file.path(root_dir, raw_data_dir)
  if (!dir.exists(raw_base_dir)) {
    if (skip_data_downloads) return(NULL)
    stop("Raw data directory not found: ", raw_base_dir)
  }
  snapshot_dirs <- list.dirs(raw_base_dir, recursive = FALSE, full.names = TRUE)
  if (length(snapshot_dirs) == 0) {
    if (skip_data_downloads) return(NULL)
    stop("No raw data snapshots found in: ", raw_base_dir)
  }
  snapshot_info <- file.info(snapshot_dirs)
  snapshot_dirs[order(snapshot_info$mtime, decreasing = TRUE)][1]
}

latest_snapshot <- latest_raw_snapshot(repo_root, config$raw_data_dir, skip_data_downloads)
if (is.null(latest_snapshot)) stop("No raw snapshots found (and SKIP_DATA_DOWNLOADS is set).")

country_info <- standardize_country_info(country_info)

# Coalition definition (your list)
iso3c_network <- c(
  "USA","CAN","JPN","AUS","IND","MEX","KOR","GBR","DEU","FRA","ITA","BRA","SAU",
  "ZAF","IDN","NOR","UAE","VNM","KEN","DNK","ARG","MAR","CHL"
)

# Run
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
  milp_stage_time_limit_sec = 120
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

message("Wrote allied network outputs to: ", outputs_dir)
