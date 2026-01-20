parse_filter_list <- function(raw_value) {
  if (is.null(raw_value)) {
    return(NULL)
  }

  value <- trimws(as.character(raw_value))
  if (!nzchar(value)) {
    return(NULL)
  }

  if (tolower(value) %in% c("all", "*", "any")) {
    return(NULL)
  }

  parts <- unlist(strsplit(value, ","))
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]

  if (length(parts) == 0) {
    return(NULL)
  }

  unique(parts)
}

filter_datawrapper_dataset <- function(data,
                                       countries = NULL,
                                       tech = NULL,
                                       supply_chain = NULL,
                                       categories = NULL,
                                       variables = NULL) {
  if (!is.null(countries) && "Country" %in% names(data)) {
    data <- dplyr::filter(data, Country %in% countries)
  }
  if (!is.null(tech) && "tech" %in% names(data)) {
    data <- dplyr::filter(data, tech %in% tech)
  }
  if (!is.null(supply_chain) && "supply_chain" %in% names(data)) {
    data <- dplyr::filter(data, supply_chain %in% supply_chain)
  }
  if (!is.null(categories) && "category" %in% names(data)) {
    data <- dplyr::filter(data, category %in% categories)
  }
  if (!is.null(variables) && "variable" %in% names(data)) {
    data <- dplyr::filter(data, variable %in% variables)
  }

  data
}

resolve_datawrapper_dataset <- function(dataset_name, datasets) {
  if (is.null(dataset_name) || !nzchar(dataset_name)) {
    stop("Dataset name is required for Datawrapper export.")
  }

  if (!dataset_name %in% names(datasets)) {
    stop(
      "Unknown dataset: ", dataset_name,
      ". Available datasets: ",
      paste(names(datasets), collapse = ", ")
    )
  }

  datasets[[dataset_name]]
}
