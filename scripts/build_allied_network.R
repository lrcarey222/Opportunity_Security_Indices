# User-facing entrypoint for building allied network outputs only.

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
  tryCatch({
    install.packages(missing, dependencies = TRUE)
  }, error = function(e) {
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

is_true <- function(x) {
  tolower(x) %in% c("1", "true", "yes")
}

repo_root <- resolve_repo_root_from_script()
source(file.path(repo_root, "scripts", "00_setup.R"))

config <- getOption("opportunity_security.config")
if (is.null(config)) stop("Config not loaded after setup.")

install_if_missing <- is_true(Sys.getenv("OPSI_INSTALL_MILP_PKGS", "true"))
require_milp <- is_true(Sys.getenv("OPSI_REQUIRE_MILP", "false"))
milp_available <- ensure_milp_pkgs(install_if_missing = install_if_missing, require_milp = require_milp)
if (milp_available) {
  message("MILP solver stack detected (ompr/ROI/GLPK). method='auto' will use MILP (unless stage too large or solver errors).")
} else {
  message("MILP solver stack not detected. method='auto' will use greedy fallback.")
}

required_paths <- function(config, repo_root) {
  processed_dir <- file.path(repo_root, config$processed_dir)
  outputs_rds_path <- Sys.getenv("OPSI_OUTPUTS_RDS", "")
  if (!nzchar(outputs_rds_path)) {
    outputs_rds_path <- if (!is.null(config$outputs_rds) && nzchar(config$outputs_rds)) {
      file.path(repo_root, config$outputs_rds)
    } else {
      file.path(processed_dir, "outputs", "index_outputs.rds")
    }
  }

  list(
    outputs_rds = outputs_rds_path,
    partner_friendshore = file.path(processed_dir, "partner_friendshore_tbl.rds"),
    partner_opportunity = file.path(processed_dir, "partner_opportunity_tbl.rds"),
    partner_development_optional = file.path(processed_dir, "partner_development_country_tbl.rds"),
    wdi_country_info = file.path(repo_root, config$raw_data_dir, "wdi_country_info.csv")
  )
}

missing_required <- function(paths) {
  required <- c(paths$outputs_rds, paths$partner_friendshore, paths$partner_opportunity, paths$wdi_country_info)
  required[!file.exists(required)]
}

paths <- required_paths(config, repo_root)
missing <- missing_required(paths)
auto_prereqs <- is_true(Sys.getenv("OPSI_AUTO_PREREQS", "false"))

if (length(missing) > 0 && auto_prereqs) {
  prereq_scripts <- c(
    "scripts/05_ingest_sources.R",
    "scripts/07_process_data.R",
    "scripts/10_build_themes.R",
    "scripts/20_build_indices.R",
    "scripts/15_build_partner_themes.R"
  )

  for (script_rel in prereq_scripts) {
    if (length(missing) == 0) break
    message("Missing prerequisites detected; sourcing ", script_rel)
    source(file.path(repo_root, script_rel))
    paths <- required_paths(config, repo_root)
    missing <- missing_required(paths)
  }
}

if (length(missing) > 0) {
  stop(
    paste0(
      "Missing required files:\n - ", paste(missing, collapse = "\n - "), "\n",
      "Run: Rscript run_pipeline.R (or set OPSI_AUTO_PREREQS=true and rerun)."
    )
  )
}

source(file.path(repo_root, "scripts", "30_build_allied_network_design.R"))

outputs_dir <- if (!is.null(config$outputs_dir) && nzchar(config$outputs_dir)) {
  file.path(repo_root, config$outputs_dir)
} else {
  file.path(repo_root, config$processed_dir, "outputs")
}
message("Allied network build completed successfully. Outputs directory: ", outputs_dir)
