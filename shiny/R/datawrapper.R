dw_api_base <- function() {
  "https://api.datawrapper.de/v3"
}

dw_build_headers <- function(api_key) {
  httr::add_headers(
    Authorization = paste("Bearer", api_key)
  )
}

dw_parse_json <- function(response) {
  jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
}

dw_create_chart <- function(title,
                            type = "d3-maps-choropleth",
                            folder_id = NULL,
                            theme = NULL,
                            api_key = Sys.getenv("DATAWRAPPER_API_KEY", "")) {
  if (!nzchar(api_key)) {
    stop("DATAWRAPPER_API_KEY is not set.")
  }
  body <- list(
    title = title,
    type = type
  )
  if (!is.null(folder_id)) {
    body$folderId <- folder_id
  }
  if (!is.null(theme)) {
    body$theme <- theme
  }
  response <- httr::POST(
    url = paste0(dw_api_base(), "/charts"),
    body = body,
    encode = "json",
    dw_build_headers(api_key)
  )
  httr::stop_for_status(response)
  payload <- dw_parse_json(response)
  payload$id
}

dw_upload_csv <- function(chart_id,
                          csv_string,
                          api_key = Sys.getenv("DATAWRAPPER_API_KEY", "")) {
  if (!nzchar(api_key)) {
    stop("DATAWRAPPER_API_KEY is not set.")
  }
  response <- httr::PUT(
    url = paste0(dw_api_base(), "/charts/", chart_id, "/data"),
    body = csv_string,
    encode = "raw",
    dw_build_headers(api_key),
    httr::add_headers(`Content-Type` = "text/csv; charset=UTF-8")
  )
  httr::stop_for_status(response)
  invisible(TRUE)
}

dw_patch_metadata <- function(chart_id,
                              metadata,
                              title = NULL,
                              api_key = Sys.getenv("DATAWRAPPER_API_KEY", "")) {
  if (!nzchar(api_key)) {
    stop("DATAWRAPPER_API_KEY is not set.")
  }
  body <- list(metadata = metadata)
  if (!is.null(title)) {
    body$title <- title
  }
  response <- httr::PATCH(
    url = paste0(dw_api_base(), "/charts/", chart_id),
    body = body,
    encode = "json",
    dw_build_headers(api_key)
  )
  httr::stop_for_status(response)
  invisible(TRUE)
}

dw_publish <- function(chart_id,
                       api_key = Sys.getenv("DATAWRAPPER_API_KEY", "")) {
  if (!nzchar(api_key)) {
    stop("DATAWRAPPER_API_KEY is not set.")
  }
  response <- httr::POST(
    url = paste0(dw_api_base(), "/charts/", chart_id, "/publish"),
    dw_build_headers(api_key)
  )
  httr::stop_for_status(response)
  dw_parse_json(response)
}

dw_get_iframe_src <- function(publish_response) {
  iframe <- publish_response$embedCodes$iframe
  if (!is.null(iframe)) {
    match <- regexpr("src=\"([^\"]+)\"", iframe, perl = TRUE)
    if (match[1] != -1) {
      src <- regmatches(iframe, match)
      return(sub("^src=\"|\"$", "", src))
    }
  }
  publish_response$publicUrl
}
