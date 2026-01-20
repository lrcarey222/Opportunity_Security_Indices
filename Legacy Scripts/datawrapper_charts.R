#Datawrapper Charts
library(dplyr)
library(purrr)
library(httr)
library(jsonlite)
library(DatawRappr)
library(rdwd)
datawrapper_auth(api_key = "D9oxBZAhXR7y9CWWWmF5iGmK7Y53cyPTnWj2QwiYENkbsv8ZL7yWKMYbybErNi2p", overwrite = TRUE)
Sys.setenv(DATAWRAPPER_API_KEY = DatawRappr::dw_get_api_key())

publish_datawrapper_chart <- function(
    chart_id,
    api_key = DatawRappr::dw_get_api_key()
) {
  response <- httr::POST(
    url = paste0("https://api.datawrapper.de/v3/charts/", chart_id, "/publish"),
    httr::add_headers(Authorization = paste("Bearer", api_key))
  )
  httr::stop_for_status(response)
  httr::content(response, as = "parsed")
}

df_to_csv_string <- function(df) {
  paste(
    capture.output(
      write.csv(df, row.names = FALSE, na = "", quote = TRUE)
    ),
    collapse = "\n"
  )
}

#Export Size-------------

library(dplyr)
library(tibble)
library(DatawRappr)

create_export_chart_for_country <- function(
    reporter_iso_code,
    template_chart_id = "uIwjm",
    folder_id = NULL
) {
  # 1) Copy the template chart (keeps design & axes)
  copied   <- DatawRappr::dw_copy_chart(copy_from = template_chart_id)
  chart_id <- copied$content$id
  
  # 2) Build & upload data for this reporter
  df       <- build_export_size_df(reporter_iso_code)
  csv_data <- df_to_csv_string(df)
  upload_data_to_chart(chart_id, csv_data)
  
  # 3) Derive a nice country name
  reporter_name <- country_info %>%
    filter(iso3c == reporter_iso_code) %>%
    distinct(country) %>%
    pull(country) %>%
    .[1]
  
  if (is.na(reporter_name) || is.null(reporter_name)) {
    reporter_name <- reporter_iso_code
  }
  
  intro_txt <- paste0("Top 12 partner economies for ", reporter_name,
                      " by new energy exports")
  
  # 4) Update title + intro
  edit_datawrapper_chart(
    api_key  = DatawRappr::dw_get_api_key(),
    chart_id = chart_id,
    title    = paste0(reporter_name,
                      " new energy export partners"),
    metadata = list(
      describe = list(intro = intro_txt)
    )
  )
  
  # 5) Publish
  #publish_datawrapper_chart(chart_id)
  
  # 6) Return a one-row tibble with useful info
  tibble(
    reporter_iso = reporter_iso_code,
    chart_id     = chart_id,
    url          = paste0("https://datawrapper.dwcdn.net/", chart_id, "/")
  )
}



reporter_list <- c("JPN", "KOR", "USA", "VNM", "AUS","IND")

export_chart_index <- map_dfr(
  reporter_list,
  ~ create_export_chart_for_country(
    reporter_iso_code   = .x,
    template_chart_id   = "uIwjm",
    folder_id           = "280831"  # or NULL if you don't care about folders
  )
)

export_chart_index
# Optionally save:
# write.csv(export_chart_index, "Downloads/export_chart_index.csv", row.names = FALSE)


