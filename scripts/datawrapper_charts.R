if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("Package 'dplyr' is required for Datawrapper exports.")
}

resolve_datawrapper_repo_root <- function() {
  if (requireNamespace("rprojroot", quietly = TRUE)) {
    return(rprojroot::find_root(rprojroot::is_git_root))
  }

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

  while (!file.exists(file.path(d, ".git")) && dirname(d) != d) {
    d <- dirname(d)
  }

  if (!file.exists(file.path(d, ".git"))) {
    stop("Could not locate repo root (no .git found).")
  }

  d
}

`%||%` <- function(x, y) {
  if (!is.null(x)) {
    x
  } else {
    y
  }
}

load_datawrapper_outputs <- function(outputs_rds_path) {
  if (is.null(outputs_rds_path) || !nzchar(outputs_rds_path)) {
    stop("OPSI_OUTPUTS_RDS must point to an RDS file with index outputs.")
  }

  if (!file.exists(outputs_rds_path)) {
    stop("Outputs RDS file not found: ", outputs_rds_path)
  }

  readRDS(outputs_rds_path)
}

build_datawrapper_dataset_map <- function(outputs) {
  energy_security_outputs <- outputs$energy_security_outputs %||% list(
    category_scores = outputs$energy_security_category_scores,
    category_contributions = outputs$energy_security_category_contributions,
    variable_contributions = outputs$energy_security_variable_contributions,
    index = outputs$energy_security_index
  )

  economic_opportunity_outputs <- outputs$economic_opportunity_outputs %||% list(
    category_scores = outputs$economic_opportunity_category_scores,
    category_contributions = outputs$economic_opportunity_category_contributions,
    variable_contributions = outputs$economic_opportunity_variable_contributions,
    index = outputs$economic_opportunity_index
  )

  list(
    energy_security_category_scores = energy_security_outputs$category_scores,
    energy_security_category_contributions = energy_security_outputs$category_contributions,
    energy_security_variable_contributions = energy_security_outputs$variable_contributions,
    energy_security_index = energy_security_outputs$index %||%
      energy_security_outputs$energy_security_index,
    economic_opportunity_category_scores = economic_opportunity_outputs$category_scores,
    economic_opportunity_category_contributions = economic_opportunity_outputs$category_contributions,
    economic_opportunity_variable_contributions = economic_opportunity_outputs$variable_contributions,
    economic_opportunity_index = economic_opportunity_outputs$index %||%
      economic_opportunity_outputs$economic_opportunity_index
  )
}

run_datawrapper_charts <- function() {
  repo_root <- resolve_datawrapper_repo_root()
  source(file.path(repo_root, "R", "charts", "datawrapper_filters.R"))

  token <- Sys.getenv("DATAWRAPPER_API_TOKEN")
  chart_id <- Sys.getenv("OPSI_DW_CHART_ID")
  dataset_name <- Sys.getenv("OPSI_DW_DATASET")
  outputs_rds_path <- Sys.getenv("OPSI_OUTPUTS_RDS")

  filters <- list(
    countries = parse_filter_list(Sys.getenv("OPSI_DW_COUNTRIES")),
    tech = parse_filter_list(Sys.getenv("OPSI_DW_TECH")),
    supply_chain = parse_filter_list(Sys.getenv("OPSI_DW_SUPPLY_CHAIN")),
    categories = parse_filter_list(Sys.getenv("OPSI_DW_CATEGORIES")),
    variables = parse_filter_list(Sys.getenv("OPSI_DW_VARIABLES"))
  )

  publish <- tolower(Sys.getenv("OPSI_DW_PUBLISH", "true")) %in% c("1", "true", "yes")

  outputs <- load_datawrapper_outputs(outputs_rds_path)
  dataset_map <- build_datawrapper_dataset_map(outputs)
  dataset <- resolve_datawrapper_dataset(dataset_name, dataset_map)

  if (is.null(dataset)) {
    stop("Requested dataset is NULL; check outputs contents and dataset name.")
  }

  filtered_dataset <- filter_datawrapper_dataset(
    dataset,
    countries = filters$countries,
    tech = filters$tech,
    supply_chain = filters$supply_chain,
    categories = filters$categories,
    variables = filters$variables
  )

  if (nrow(filtered_dataset) == 0) {
    stop("Filtered dataset is empty; adjust filter inputs or dataset selection.")
  }

  datawrapper_update_chart(
    token = token,
    chart_id = chart_id,
    data = filtered_dataset,
    publish = publish
  )

  message("Datawrapper chart updated: ", chart_id)
}
