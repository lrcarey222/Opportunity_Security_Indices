# Pipeline setup: load config, verify env vars, set options.
resolve_repo_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  script_path <- if (length(file_arg) > 0) {
    sub("^--file=", "", file_arg[1])
  } else if (!is.null(sys.frame(1)$ofile)) {
    sys.frame(1)$ofile
  } else {
    ""
  }
  dirname(normalizePath(script_path, winslash = "/", mustWork = FALSE))
}

repo_root <- resolve_repo_root()
config_path <- Sys.getenv("OPSI_CONFIG", file.path(repo_root, "config", "config.yml"))
weights_path <- Sys.getenv("OPSI_WEIGHTS", file.path(repo_root, "config", "weights.yml"))

if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("Package 'yaml' is required to load config files.")
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

options(
  opportunity_security.config = config,
  opportunity_security.weights = weights
)
# setup (placeholder).
# TODO: implement.
setup_stub <- function() {
  NULL
}
