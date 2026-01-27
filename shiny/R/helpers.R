resolve_app_dir <- function() {
  frame_path <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(frame_path) && nzchar(frame_path)) {
    return(normalizePath(dirname(frame_path), winslash = "/", mustWork = FALSE))
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

resolve_repo_root <- function(app_dir) {
  candidates <- c(app_dir, dirname(app_dir))
  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "Opportunity_Security_Indices.Rproj")) ||
        dir.exists(file.path(candidate, "config"))) {
      return(normalizePath(candidate, winslash = "/", mustWork = FALSE))
    }
  }
  normalizePath(dirname(app_dir), winslash = "/", mustWork = FALSE)
}

find_outputs_rds <- function(repo_root) {
  env_path <- Sys.getenv("OPSI_OUTPUTS_RDS", "")
  candidates <- c(
    env_path,
    file.path(repo_root, "data", "processed", "outputs", "index_outputs.rds"),
    file.path(repo_root, "data", "processed", "index_outputs.rds"),
    file.path(repo_root, "output", "index_outputs.rds"),
    file.path(repo_root, "outputs", "index_outputs.rds")
  )
  candidates <- candidates[nzchar(candidates)]
  for (candidate in candidates) {
    if (file.exists(candidate)) {
      return(candidate)
    }
  }
  NULL
}

load_sample_indices <- function(app_dir) {
  sample_path <- file.path(app_dir, "inst", "extdata", "sample_indices.csv")
  utils::read.csv(sample_path, stringsAsFactors = FALSE)
}

standardize_index_tbl <- function(tbl, metric_col, metric_label) {
  if (!all(c("Country", "tech", "supply_chain") %in% names(tbl))) {
    return(NULL)
  }
  iso3_col <- rep(NA_character_, nrow(tbl))
  if ("iso3" %in% names(tbl)) {
    iso3_col <- tbl$iso3
  } else if ("ISO3" %in% names(tbl)) {
    iso3_col <- tbl$ISO3
  }
  data.frame(
    country = tbl$Country,
    iso3 = iso3_col,
    tech = tbl$tech,
    supply_chain = tbl$supply_chain,
    metric = metric_label,
    value = tbl[[metric_col]],
    stringsAsFactors = FALSE
  )
}

resolve_iso3 <- function(country, iso3_col = NULL) {
  if (!is.null(iso3_col)) {
    return(iso3_col)
  }
  if (!requireNamespace("countrycode", quietly = TRUE)) {
    return(rep(NA_character_, length(country)))
  }
  overrides <- c(
    "Korea" = "KOR",
    "South Korea" = "KOR",
    "Viet Nam" = "VNM",
    "Iran" = "IRN",
    "Russia" = "RUS",
    "Bolivia" = "BOL",
    "Venezuela" = "VEN",
    "United States" = "USA",
    "United States of America" = "USA",
    "Czechia" = "CZE",
    "Congo (Democratic Republic of the)" = "COD",
    "Democratic Republic of the Congo" = "COD"
  )
  countrycode::countrycode(country,
                           origin = "country.name",
                           destination = "iso3c",
                           custom_match = overrides)
}

load_pipeline_indices <- function(outputs_path) {
  outputs <- readRDS(outputs_path)
  energy_tbl <- outputs$energy_security_index
  econ_tbl <- outputs$economic_opportunity_index
  metric_rows <- list()
  if (!is.null(energy_tbl) && "Energy_Security_Index" %in% names(energy_tbl)) {
    metric_rows[["Energy Security Index"]] <- standardize_index_tbl(
      energy_tbl,
      "Energy_Security_Index",
      "Energy Security Index"
    )
  }
  if (!is.null(econ_tbl) && "Economic_Opportunity_Index" %in% names(econ_tbl)) {
    metric_rows[["Economic Opportunity Index"]] <- standardize_index_tbl(
      econ_tbl,
      "Economic_Opportunity_Index",
      "Economic Opportunity Index"
    )
  }
  data <- do.call(rbind, metric_rows)
  if (is.null(data)) {
    return(NULL)
  }
  data
}

load_index_data <- function(app_dir, repo_root) {
  outputs_path <- find_outputs_rds(repo_root)
  if (!is.null(outputs_path)) {
    pipeline_data <- load_pipeline_indices(outputs_path)
    if (!is.null(pipeline_data) && nrow(pipeline_data) > 0) {
      if (!("iso3" %in% names(pipeline_data)) || all(is.na(pipeline_data$iso3))) {
        pipeline_data$iso3 <- resolve_iso3(pipeline_data$country)
      }
      return(list(
        data = pipeline_data,
        source = "Pipeline outputs",
        detail = outputs_path
      ))
    }
  }
  sample <- load_sample_indices(app_dir)
  sample$iso3 <- resolve_iso3(sample$country, iso3_col = sample$iso3)
  sample_data <- data.frame(
    country = sample$country,
    iso3 = sample$iso3,
    tech = sample$tech,
    supply_chain = sample$supply_chain,
    metric = rep("Energy Security Index", nrow(sample)),
    value = sample$Energy_Security_Index,
    stringsAsFactors = FALSE
  )
  sample_data <- rbind(
    sample_data,
    data.frame(
      country = sample$country,
      iso3 = sample$iso3,
      tech = sample$tech,
      supply_chain = sample$supply_chain,
      metric = rep("Economic Opportunity Index", nrow(sample)),
      value = sample$Economic_Opportunity_Index,
      stringsAsFactors = FALSE
    ),
    data.frame(
      country = sample$country,
      iso3 = sample$iso3,
      tech = sample$tech,
      supply_chain = sample$supply_chain,
      metric = rep("Strategic Index (Sample)", nrow(sample)),
      value = sample$Strategic_Index,
      stringsAsFactors = FALSE
    )
  )
  list(
    data = sample_data,
    source = "Sample data",
    detail = "shiny/inst/extdata/sample_indices.csv"
  )
}

summarize_for_map <- function(data, metric, tech, supply_chain) {
  filtered <- data[data$metric == metric, , drop = FALSE]
  if (!is.null(tech)) {
    filtered <- filtered[filtered$tech == tech, , drop = FALSE]
  }
  if (!is.null(supply_chain)) {
    filtered <- filtered[filtered$supply_chain == supply_chain, , drop = FALSE]
  }
  if (nrow(filtered) == 0) {
    return(data.frame(
      country = character(),
      iso3 = character(),
      metric = character(),
      value = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  aggregate(value ~ country + iso3 + metric, data = filtered, FUN = function(x) {
    if (all(is.na(x))) {
      NA_real_
    } else {
      mean(x, na.rm = TRUE)
    }
  })
}
