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

# Coalition definition. ARE is the ISO3 code for the United Arab Emirates ("UAE" matched no table).
iso3c_network <- allied_network_default_iso3c()

# Partner dyad tables (from scripts/15_build_partner_themes.R). The friendshore table has ~17M rows;
# keep only the coalition's index rows as each table is read, so the full tables are never held
# together. allied_network_design() collapses whatever duplicates remain.
partner_tbl_cols <- c("reporter_iso", "partner_iso", "tech", "supply_chain", "category", "variable", "data_type", "value", "Year")
read_coalition_dyads <- function(name, variable_name) {
  tbl <- read_processed_tbl(name)
  out <- tbl %>%
    dplyr::filter(
      variable == variable_name,
      reporter_iso %in% iso3c_network,
      partner_iso %in% iso3c_network
    ) %>%
    dplyr::select(dplyr::any_of(partner_tbl_cols)) %>%
    tibble::as_tibble()
  rm(tbl)
  invisible(gc())
  out
}
partner_friendshore_tbl <- read_coalition_dyads("partner_friendshore_tbl", "Friendshore Index")
partner_opportunity_tbl <- read_coalition_dyads("partner_opportunity_tbl", "Opportunity Index")
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

# Coalition GDP: latest finite WDI year per member (see allied_network_country_size_tbl)
gdp_path <- file.path(raw_data_path, "wdi_gdp.csv")
if (!file.exists(gdp_path)) stop("GDP file not found: ", gdp_path)
country_size_tbl <- allied_network_country_size_tbl(
  read.csv(gdp_path, stringsAsFactors = FALSE),
  country_info,
  iso3c_network
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


auto_milp_max_nodes <- 30
if (milp_available) {
  message("MILP solver stack detected (ompr/ROI/GLPK). method='auto' uses MILP for stages with up to ", auto_milp_max_nodes, " members; greedy if a stage is larger or the solver fails.")
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
  method = "auto",        # MILP per stage when installed and the stage is small enough, else greedy
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
  auto_milp_max_nodes = auto_milp_max_nodes,
  milp_stage_time_limit_sec = 120,
  country_size_tbl = country_size_tbl,
  demand_mode = "mixed",
  demand_weights = list(need = 0.5, size = 0.5),
  portfolio_enable = TRUE,
  portfolio_top_k = 5,
  portfolio_min_cap = 2,
  portfolio_max_cap = 10,
  portfolio_transform = "log",
  # missing-data rules and edge orientation (the function defaults, stated for the record)
  policy_missing = "coalition_median",
  missing_components = "renormalize",
  friendshore_orientation = "reporter_is_consumer"
)

diag_tbl <- res$diagnostics
message(
  "Allied network: ", nrow(diag_tbl), " stages; method per stage: ",
  paste(names(table(diag_tbl$method)), table(diag_tbl$method), sep = " = ", collapse = ", "),
  "; policy-imputed members selected in ", sum(dplyr::coalesce(diag_tbl$n_selected_policy_imputed, 0L) > 0), " stages",
  if (!is.null(res$portfolio)) paste0("; portfolio converged: ", res$portfolio$converged, " after ", res$portfolio$iters, " iterations") else ""
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
utils::write.csv(res$input_checks,   file.path(outputs_dir, "allied_network_input_checks.csv"),  row.names = FALSE)
if (!is.null(res$build_candidates)) {
  utils::write.csv(res$build_candidates, file.path(outputs_dir, "allied_network_build_candidates.csv"), row.names = FALSE)
}

if (!is.null(res$portfolio)) {
  utils::write.csv(res$portfolio$caps, file.path(outputs_dir, "allied_network_portfolio_caps.csv"), row.names = FALSE)
  utils::write.csv(res$portfolio$counts, file.path(outputs_dir, "allied_network_portfolio_counts.csv"), row.names = FALSE)
  utils::write.csv(res$portfolio$topk, file.path(outputs_dir, "allied_network_topk_by_stage.csv"), row.names = FALSE)
}

message("Wrote allied network outputs to: ", outputs_dir)
