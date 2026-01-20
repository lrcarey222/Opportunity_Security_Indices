if (!requireNamespace("httr2", quietly = TRUE)) {
  stop("Package 'httr2' is required for Datawrapper API calls.")
}
if (!requireNamespace("readr", quietly = TRUE)) {
  stop("Package 'readr' is required for Datawrapper exports.")
}

datawrapper_api_request <- function(token,
                                     method,
                                     path,
                                     body_raw = NULL,
                                     content_type = NULL) {
  if (is.null(token) || !nzchar(token)) {
    stop("Datawrapper API token is required.")
  }

  request <- httr2::request(paste0("https://api.datawrapper.de/v3", path))
  request <- httr2::req_headers(
    request,
    Authorization = paste("Bearer", token)
  )
  request <- httr2::req_method(request, method)

  if (!is.null(body_raw)) {
    request <- httr2::req_body_raw(request, body_raw, type = content_type)
  }

  response <- httr2::req_perform(request)

  if (httr2::resp_status(response) >= 300) {
    message("Datawrapper API error: ", httr2::resp_status_desc(response))
    stop(
      "Datawrapper API request failed with status ",
      httr2::resp_status(response),
      "."
    )
  }

  response
}

datawrapper_upload_chart_data <- function(token, chart_id, data) {
  if (is.null(chart_id) || !nzchar(chart_id)) {
    stop("Datawrapper chart ID is required.")
  }

  csv_text <- readr::format_csv(data, na = "")

  datawrapper_api_request(
    token,
    method = "PUT",
    path = paste0("/charts/", chart_id, "/data"),
    body_raw = csv_text,
    content_type = "text/csv"
  )
}

datawrapper_publish_chart <- function(token, chart_id) {
  if (is.null(chart_id) || !nzchar(chart_id)) {
    stop("Datawrapper chart ID is required.")
  }

  datawrapper_api_request(
    token,
    method = "POST",
    path = paste0("/charts/", chart_id, "/publish")
  )
}

datawrapper_update_chart <- function(token, chart_id, data, publish = TRUE) {
  datawrapper_upload_chart_data(token, chart_id, data)

  if (isTRUE(publish)) {
    datawrapper_publish_chart(token, chart_id)
  } else {
    NULL
  }
}
