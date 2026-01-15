# Debug helper: reverse-engineer energy security index values.
#
# Example usage:
#   Rscript scripts/95_debug_energy_security_breakdown.R \
#     --country="United States" \
#     --tech="Oil" \
#     --supply_chain="Upstream" \
#     --output-dir="output/debug"
#
# In an R session:
#   source("scripts/95_debug_energy_security_breakdown.R")
#   run_energy_security_breakdown(
#     country = "United States",
#     tech = "Oil",
#     supply_chain = "Upstream"
#   )
#
# Country summary for stacked bar chart data:
#   Rscript scripts/95_debug_energy_security_breakdown.R \
#     --country="United States" \
#     --mode="country" \
#     --output-dir="output/debug"

resolve_repo_root_local <- function() {
  repo_root <- getOption("opportunity_security.repo_root")
  if (!is.null(repo_root) && nzchar(repo_root)) {
    return(repo_root)
  }

  if (requireNamespace("rprojroot", quietly = TRUE)) {
    return(rprojroot::find_root(rprojroot::is_git_root))
  }

  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!file.exists(file.path(d, ".git")) && dirname(d) != d) {
    d <- dirname(d)
  }

  if (!file.exists(file.path(d, ".git"))) {
    stop("Unable to determine repo root; set working directory to the repo.")
  }

  d
}

parse_args <- function(args) {
  out <- list()
  for (arg in args) {
    if (!stringr::str_starts(arg, "--")) {
      next
    }
    split <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1]]
    key <- split[1]
    value <- if (length(split) > 1) split[2] else TRUE
    out[[key]] <- value
  }
  out
}

as_bool <- function(value, default = FALSE) {
  if (is.null(value)) {
    return(default)
  }
  if (is.logical(value)) {
    return(value)
  }
  value <- tolower(as.character(value))
  value %in% c("1", "true", "yes", "y")
}

run_energy_security_breakdown <- function(country,
                                          tech = NULL,
                                          supply_chain = NULL,
                                          mode = c("single", "country"),
                                          output_dir = NULL,
                                          run_ingest = FALSE,
                                          run_process = FALSE) {
  if (is.null(country) || !nzchar(country)) {
    stop("country is required (e.g., country = \"United States\").")
  }

  mode <- match.arg(mode)

  if (mode == "single" && (is.null(tech) || is.null(supply_chain))) {
    stop("tech and supply_chain are required when mode = \"single\".")
  }

  repo_root <- resolve_repo_root_local()

  source(file.path(repo_root, "scripts", "00_setup.R"))

  if (isTRUE(run_ingest)) {
    source(file.path(repo_root, "scripts", "05_ingest_sources.R"))
  }

  if (isTRUE(run_process)) {
    source(file.path(repo_root, "scripts", "07_process_data.R"))
  }

  source(file.path(repo_root, "scripts", "10_build_categories.R"))
  source(file.path(repo_root, "scripts", "20_build_indices.R"))

  weights <- getOption("opportunity_security.weights")
  if (is.null(weights) || is.null(weights$energy_security)) {
    stop("Energy security weights not loaded; ensure scripts/00_setup.R ran.")
  }

  weights_tbl <- tibble::tibble(
    category = names(weights$energy_security),
    weight = as.numeric(unlist(weights$energy_security, use.names = FALSE))
  )

  energy_security_inputs <- list(
    energy_access_tbl = energy_access_tbl,
    import_dependence_tbl = import_dependence_tbl,
    reserves_tbl = reserves_tbl,
    foreign_dependency_tbl = foreign_dependency_tbl,
    critical_minerals_processing_tbl = critical_minerals_processing_tbl,
    critical_minerals_production_tbl = critical_minerals_production_tbl,
    critical_minerals_trade_tbl = critical_minerals_trade_tbl,
    energy_consumption_tbl = energy_consumption_tbl,
    trade_concentration_tbl = trade_concentration_tbl,
    energy_prices_tbl = energy_prices_tbl
  )

  energy_security_data <- standardize_energy_security_inputs(energy_security_inputs)

  category_scores <- energy_security_category_scores %>%
    dplyr::filter(Country == country)

  if (nrow(category_scores) == 0) {
    stop("No category scores found for country: ", country)
  }

  latest_year <- category_scores %>%
    dplyr::filter(!is.na(Year)) %>%
    dplyr::summarize(latest_year = max(Year, na.rm = TRUE)) %>%
    dplyr::pull(latest_year)

  category_scores <- category_scores %>%
    dplyr::filter(Year == latest_year)

  weights_sum <- weights_tbl %>%
    dplyr::summarize(weight_sum = sum(weight, na.rm = TRUE)) %>%
    dplyr::pull(weight_sum)

  category_scores <- category_scores %>%
    dplyr::left_join(weights_tbl, by = "category") %>%
    dplyr::mutate(
      weighted_contribution = category_score * weight / weights_sum
    )

  if (mode == "single") {
    energy_index_row <- energy_security_index %>%
      dplyr::filter(
        Country == country,
        .data$tech == tech,
        .data$supply_chain == supply_chain,
        Year == latest_year
      )

    if (nrow(energy_index_row) == 0) {
      stop("No energy security index found for specified country/tech/supply_chain.")
    }

    message(
      "Energy security index (", country, " | ", tech, " | ", supply_chain, ") = ",
      round(energy_index_row$energy_security_index[1], 3)
    )

    category_breakdown <- category_scores %>%
      dplyr::filter(.data$tech == tech, .data$supply_chain == supply_chain) %>%
      dplyr::arrange(dplyr::desc(weighted_contribution))

    if (nrow(category_breakdown) == 0) {
      stop("No category breakdown found for specified tech/supply_chain.")
    }

    print(category_breakdown)

    variable_breakdown <- energy_security_data %>%
      dplyr::filter(
        Country == country,
        .data$tech == tech,
        .data$supply_chain == supply_chain,
        data_type == "index",
        !grepl("Overall", variable),
        Year == latest_year
      ) %>%
      dplyr::select(category, variable, value, source, explanation) %>%
      dplyr::arrange(category, dplyr::desc(value))

    if (nrow(variable_breakdown) == 0) {
      warning("No variable-level breakdown found for specified filters.")
    } else {
      print(variable_breakdown)
    }

    if (!is.null(output_dir) && nzchar(output_dir)) {
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }

      utils::write.csv(
        category_breakdown,
        file = file.path(output_dir, "energy_security_category_breakdown.csv"),
        row.names = FALSE
      )

      utils::write.csv(
        variable_breakdown,
        file = file.path(output_dir, "energy_security_variable_breakdown.csv"),
        row.names = FALSE
      )

      message("Breakdown outputs written to: ", output_dir)
    }

    return(list(
      energy_security_index = energy_index_row,
      category_breakdown = category_breakdown,
      variable_breakdown = variable_breakdown
    ))
  }

  country_summary <- category_scores %>%
    dplyr::arrange(tech, supply_chain, dplyr::desc(weighted_contribution))

  print(country_summary)

  if (!is.null(output_dir) && nzchar(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    utils::write.csv(
      country_summary,
      file = file.path(output_dir, "energy_security_country_category_contributions.csv"),
      row.names = FALSE
    )

    message("Country summary written to: ", output_dir)
  }

  list(
    energy_security_index = energy_security_index %>%
      dplyr::filter(Country == country, Year == latest_year),
    category_breakdown = country_summary
  )
}

args <- parse_args(commandArgs(trailingOnly = TRUE))

if (length(args) > 0) {
  mode <- if (!is.null(args$mode)) {
    args$mode
  } else if (!is.null(args$tech) && !is.null(args$supply_chain)) {
    "single"
  } else {
    "country"
  }

  run_energy_security_breakdown(
    country = args$country,
    tech = args$tech,
    supply_chain = args$supply_chain,
    mode = mode,
    output_dir = args$`output-dir`,
    run_ingest = as_bool(args$`run-ingest`, default = FALSE),
    run_process = as_bool(args$`run-process`, default = FALSE)
  )
}
