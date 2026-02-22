opsi_bootstrap_verbose <- function() {
  tolower(Sys.getenv("OPSI_BOOTSTRAP_VERBOSE", "false")) %in% c("1", "true", "yes")
}

opsi_bootstrap_message <- function(...) {
  if (opsi_bootstrap_verbose()) {
    message(...)
  }
}

ensure_cran_repo <- function() {
  repos <- getOption("repos")
  cran <- if (is.null(repos) || is.null(repos[["CRAN"]])) "" else repos[["CRAN"]]
  if (is.null(repos) || identical(cran, "@CRAN@") || is.na(cran) || cran == "") {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }
}

ensure_user_lib <- function() {
  user_lib <- Sys.getenv("R_LIBS_USER", "")
  if (!nzchar(user_lib)) {
    if (.Platform$OS.type == "windows") {
      home_dir <- Sys.getenv("USERPROFILE", path.expand("~"))
      user_lib <- file.path(
        home_dir,
        "Documents",
        "R",
        "win-library",
        paste0(getRversion()[1, 1:2], collapse = ".")
      )
    } else {
      user_lib <- path.expand(file.path("~", "R", paste0("x86_64-pc-linux-gnu-library/", getRversion()[1, 1:2])))
    }
  }

  if (!dir.exists(user_lib)) {
    dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
  }

  .libPaths(c(user_lib, .libPaths()))
  invisible(user_lib)
}

ensure_packages <- function(pkgs, install_if_missing = FALSE, quiet = TRUE) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    if (install_if_missing && .Platform$OS.type == "windows") {
      ensure_cran_repo()
      ensure_user_lib()
      opsi_bootstrap_message("Installing missing packages: ", paste(missing, collapse = ", "))
      install.packages(missing, dependencies = TRUE)
      missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
    }

    if (length(missing) > 0) {
      stop(
        "Missing required packages: ", paste(missing, collapse = ", "), "\n",
        "Install them manually or set OPSI_AUTO_INSTALL_PACKAGES=true on Windows."
      )
    }
  }

  invisible(lapply(pkgs, function(pkg) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE, quietly = quiet, warn.conflicts = FALSE))
  }))
  invisible(TRUE)
}

find_repo_root_from_bootstrap <- function() {
  bootstrap_path <- sys.frame(1)$ofile
  if (is.null(bootstrap_path) || !nzchar(bootstrap_path)) {
    stop("bootstrap.R could not determine its own source path via sys.frame(1)$ofile.")
  }

  start_dir <- dirname(normalizePath(bootstrap_path, winslash = "/", mustWork = FALSE))
  d <- start_dir
  while (!file.exists(file.path(d, ".git")) && dirname(d) != d) {
    d <- dirname(d)
  }

  if (!file.exists(file.path(d, ".git"))) {
    stop("Could not locate repo root from bootstrap path (no .git found).")
  }

  d
}

resolve_repo_root <- function() {
  repo_root <- getOption("opportunity_security.repo_root")
  if (!is.null(repo_root) && nzchar(repo_root) && dir.exists(repo_root)) {
    return(normalizePath(repo_root, winslash = "/", mustWork = FALSE))
  }

  find_repo_root_from_bootstrap()
}

read_processed_tbl <- function(name) {
  config <- getOption("opportunity_security.config")
  repo_root <- resolve_repo_root()
  path <- file.path(repo_root, config$processed_dir, paste0(name, ".rds"))
  if (!file.exists(path)) {
    stop("Processed table not found: ", path)
  }
  readRDS(path)
}

is_skip_data_downloads <- function() {
  tolower(Sys.getenv("SKIP_DATA_DOWNLOADS", "false")) %in% c("1", "true", "yes")
}

resolve_raw_data_dir <- function() {
  config <- getOption("opportunity_security.config")
  repo_root <- resolve_repo_root()
  raw_base_dir <- file.path(repo_root, config$raw_data_dir)
  if (!dir.exists(raw_base_dir)) {
    if (is_skip_data_downloads()) {
      opsi_bootstrap_message("Raw data directory not found and downloads are skipped: ", raw_base_dir)
      return(NULL)
    }
    stop("Raw data directory not found: ", raw_base_dir)
  }
  raw_base_dir
}

opsi_bootstrap <- function() {
  repo_root <- getOption("opportunity_security.repo_root")
  if (is.null(repo_root) || !nzchar(repo_root) || !dir.exists(repo_root)) {
    repo_root <- find_repo_root_from_bootstrap()
    options(opportunity_security.repo_root = repo_root)
  }
  repo_root <- normalizePath(repo_root, winslash = "/", mustWork = FALSE)

  core_packages <- c(
    "dplyr",
    "tidyr",
    "stringr",
    "readr",
    "readxl",
    "tibble",
    "purrr",
    "glue",
    "rlang",
    "yaml",
    "rprojroot",
    "countrycode",
    "WDI",
    "comtradr",
    "magrittr",
    "progress",
    "lubridate"
  )

  auto_install_packages <- tolower(Sys.getenv("OPSI_AUTO_INSTALL_PACKAGES", "false")) %in% c("1", "true", "yes")
  ensure_packages(core_packages, install_if_missing = auto_install_packages)

  config_loaded <- !is.null(getOption("opportunity_security.config")) &&
    !is.null(getOption("opportunity_security.weights")) &&
    !is.null(getOption("opportunity_security.missing_data")) &&
    !is.null(getOption("opportunity_security.index_definition"))

  if (!config_loaded) {
    config_path <- Sys.getenv("OPSI_CONFIG", file.path(repo_root, "config", "config.yml"))
    weights_path <- Sys.getenv("OPSI_WEIGHTS", file.path(repo_root, "config", "weights.yml"))
    missing_data_path <- Sys.getenv("OPSI_MISSING_DATA", file.path(repo_root, "config", "missing_data.yml"))
    index_definition_path <- Sys.getenv("OPSI_INDEX_DEFINITION", file.path(repo_root, "config", "index_definition.yml"))

    if (!file.exists(config_path)) stop("Config file not found: ", config_path)
    if (!file.exists(weights_path)) stop("Weights file not found: ", weights_path)
    if (!file.exists(missing_data_path)) stop("Missing data config file not found: ", missing_data_path)
    if (!file.exists(index_definition_path)) stop("Index definition config file not found: ", index_definition_path)

    options(
      opportunity_security.config = yaml::read_yaml(config_path),
      opportunity_security.weights = yaml::read_yaml(weights_path),
      opportunity_security.missing_data = yaml::read_yaml(missing_data_path),
      opportunity_security.index_definition = yaml::read_yaml(index_definition_path),
      opportunity_security.repo_root = repo_root
    )
  }

  assign("repo_root", repo_root, envir = .GlobalEnv)
  assign("config", getOption("opportunity_security.config"), envir = .GlobalEnv)
  options(opportunity_security.bootstrapped = TRUE)
  opsi_bootstrap_message("Bootstrap complete. repo_root=", repo_root)
  invisible(TRUE)
}

opsi_bootstrap()
