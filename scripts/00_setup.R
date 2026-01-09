# Pipeline setup: load config, verify env vars, set options.
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

if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("Package 'yaml' is required to load config files.")
}

if (Sys.which("Rscript") == "") {
  stop("Rscript not found on PATH. Install R to enable testing.")
}

if (!file.exists(config_path)) {
  stop("Config file not found: ", config_path)
}
if (!file.exists(weights_path)) {
  stop("Weights file not found: ", weights_path)
}

config <- yaml::read_yaml(config_path)
weights <- yaml::read_yaml(weights_path)

required_env_vars <- c()
missing_env <- required_env_vars[Sys.getenv(required_env_vars) == ""]
if (length(missing_env) > 0) {
  stop("Missing required env vars: ", paste(missing_env, collapse = ", "))
}

if (is.null(config$raw_data_dir)) {
  stop("Config is missing raw_data_dir.")
}

options(
  opportunity_security.config = config,
  opportunity_security.weights = weights,
  opportunity_security.repo_root = repo_root
)
# setup (placeholder).
# TODO: implement.
setup_stub <- function() {
  NULL
}
