#Datawrapper
library(DatawRappr)
library(rdwd)

Sys.setenv(DATAWRAPPER_TOKEN = "FARkk5iDkLAsKNRn9jZDf2ZPRJQvFX4CsxvFd5VBfxLaFk0VNvkOZrH2ZTv0tEYN")

# set the token
datawrapper_auth <- function(api_key, overwrite = FALSE) {
  
  # Access global environment file:
  filename <- paste0(Sys.getenv("HOME"), "/.Renviron")
  
  if (!file.exists(filename)) { # create .Renviron, if it doesn't exist
    file.create(filename)
  }
  
  # check if key already exists - if yes, check for overwrite = TRUE, else: write new key
  if (Sys.getenv("DW_KEY") != "") {
    
    if (overwrite == TRUE) {
      
      
      # old solution:
      # delete DW_KEY from .Renviron:
      # command <- paste0("sed -i '' '/^DW_KEY/d' ", filename)
      # system(command = command)
      #
      # # write new Key to to environment file:
      # new_key <- paste0('DW_KEY = ', api_key)
      # write(new_key, file = filename, append = TRUE)
      
      # base R-solution:
      txt_vector <- readLines(filename, n = -1)
      lines_delete <- which(grepl("^DW_KEY.*$", txt_vector))
      output_txt <- txt_vector[-lines_delete]
      
      # add new key:
      new_key <- paste0('DW_KEY = ', api_key)
      output_txt <- c(output_txt, new_key)
      writeLines(output_txt, filename)
      
      # reload Renviron after changing it:
      readRenviron(filename)
      
    } else if (overwrite == FALSE) { # if key exists, but overwrite is FALSE: throw warning and end function
      
      warning(paste0("A Datawrapper-key ", Sys.getenv("DW_KEY"), " already exists on this system.\nSet `overwrite = TRUE` to delete it."), immediate. = TRUE)
      
    }
    
    
  } else {
    
    # write new Key to to environment file
    new_key <- paste0('DW_KEY = ', api_key)
    write(new_key, file = filename, append = TRUE)
    readRenviron(filename)
    print("New key was saved!")
    
  }
  
}

datawrapper_auth(api_key = Sys.getenv("DATAWRAPPER_TOKEN"), overwrite = TRUE)
Sys.setenv(DATAWRAPPER_API_KEY = "FARkk5iDkLAsKNRn9jZDf2ZPRJQvFX4CsxvFd5VBfxLaFk0VNvkOZrH2ZTv0tEYN")
api_key = "FARkk5iDkLAsKNRn9jZDf2ZPRJQvFX4CsxvFd5VBfxLaFk0VNvkOZrH2ZTv0tEYN"

# Helper function to create a Datawrapper chart
create_datawrapper_chart <- function(title, type, folder_id, theme) {
  response <- POST(
    url = "https://api.datawrapper.de/v3/charts",
    add_headers(Authorization = paste("Bearer", Sys.getenv("DATAWRAPPER_API_KEY"))),
    body = toJSON(list(
      title = title,
      type = type,
      folderId = folder_id,
      theme = theme
    ), auto_unbox = TRUE),
    encode = "json"
  )
  stop_for_status(response)
  content(response)
}

# Helper function to upload data to a Datawrapper chart
upload_data_to_chart <- function(chart_id, data) {
  url <- paste0("https://api.datawrapper.de/v3/charts/", chart_id, "/data")
  response <- httr::PUT(
    url = url,
    add_headers(
      Authorization = paste("Bearer", Sys.getenv("DATAWRAPPER_API_KEY")),
      `Content-Type` = "text/csv"
    ),
    body = data
  )
  response
}

# Helper function to update chart properties

edit_datawrapper_chart <- function(api_key, chart_id, metadata) {
  url <- paste0("https://api.datawrapper.de/v3/charts/", chart_id)
  
  response <- httr::PATCH(
    url = url,
    httr::add_headers(Authorization = paste("Bearer", api_key)),
    body = jsonlite::toJSON(list(metadata = metadata), auto_unbox = TRUE),
    encode = "json"
  )
  
  if (httr::status_code(response) >= 400) {
    stop("Error editing Datawrapper chart: ", httr::content(response, as = "text"))
  }
  
  message("Chart metadata successfully updated for chart ID: ", chart_id)
}

get_chart_metadata <- function(api_key, chart_id) {
  url <- paste0("https://api.datawrapper.de/v3/charts/", chart_id)
  response <- httr::GET(
    url,
    httr::add_headers(Authorization = paste("Bearer", api_key))
  )
  httr::content(response, as = "parsed")
}

# Helper function to publish a Datawrapper chart
publish_datawrapper_chart <- function(chart_id) {
  response <- POST(
    url = paste0("https://api.datawrapper.de/v3/charts/", chart_id, "/publish"),
    add_headers(Authorization = paste("Bearer", Sys.getenv("DATAWRAPPER_API_KEY")))
  )
  stop_for_status(response)
  content(response)
}


#MAPS!!!
url <- "https://api.datawrapper.de/plugin/basemaps"

# Make the GET request
response <- GET(url)

# Check the status code to ensure the request was successful
if (status_code(response) == 200) {
  # Parse the response (assuming JSON content)
  data_text <- content(response, "text", encoding = "UTF-8")
  data_parsed <- fromJSON(data_text)
  
  # Print or further process the parsed data
  #print(data_parsed)
} else {
  stop("Request failed with status code: ", status_code(response))
}

basemaps<-as.data.frame(data_parsed$data)
create_choropleth_map <- function(title, folder_id, theme = "default") {
  response <- POST(
    url = "https://api.datawrapper.de/v3/charts",
    add_headers(Authorization = paste("Bearer", Sys.getenv("DATAWRAPPER_API_KEY"))),
    body = toJSON(list(
      title = title,
      type = "d3-maps-choropleth",  # Specify map type
      folderId = folder_id,
      theme = theme
    ), auto_unbox = TRUE),
    encode = "json"
  )
  
  stop_for_status(response)
  chart <- content(response)
  
  message("Map created with ID: ", chart$id)
  return(chart$id)
}

edit_map_metadata <- function(chart_id, basemap_id, basemap_value, keys_col, values_col, tooltip = list()) {
  metadata <- list(
    axes = list(
      keys = keys_col,
      values = values_col
    ),
    visualize = list(
      basemap = basemap_id,
      `map-key-attr` = basemap_value,
      tooltip = tooltip
    )
  )
  
  url <- paste0("https://api.datawrapper.de/v3/charts/", chart_id)
  
  response <- PATCH(
    url = url,
    add_headers(Authorization = paste("Bearer", Sys.getenv("DATAWRAPPER_API_KEY"))),
    body = toJSON(list(metadata = metadata), auto_unbox = TRUE),
    encode = "json"
  )
  
  stop_for_status(response)
  message("Metadata updated for map with ID: ", chart_id)
}


upload_data_to_map <- function(chart_id, data) {
  url <- paste0("https://api.datawrapper.de/v3/charts/", chart_id, "/data")
  
  response <- PUT(
    url = url,
    add_headers(
      Authorization = paste("Bearer", Sys.getenv("DATAWRAPPER_API_KEY")),
      `Content-Type` = "text/csv"
    ),
    body = data
  )
  
  stop_for_status(response)
  message("Data uploaded to map with ID: ", chart_id)
}

publish_map <- function(chart_id) {
  response <- POST(
    url = paste0("https://api.datawrapper.de/v3/charts/", chart_id, "/publish"),
    add_headers(Authorization = paste("Bearer", Sys.getenv("DATAWRAPPER_API_KEY")))
  )
  
  stop_for_status(response)
  message("Map published with ID: ", chart_id)
}

state_gdp_growth<-county_gdp_ind %>%
  filter(geo=="Metro Area")
state_map<-create_choropleth_map(title="Metro Growth",
                                 folder_id = "280831")


edit_map_metadata(
  chart_id = state_map,
  basemap_id = "usa-cbsa-2019",
  basemap_value = "Name",
  keys_col = "geo_name",
  values_col = "total_gdp_1yr",
  tooltip = list(
    title = "{{geo_name}}",
    body = "GDP Growth: {{total_gdp_1yr}}%",
    fields = c("geo_name", "total_gdp_1yr")
  )
)
csv_data <- paste(
  capture.output(
    write.csv(state_gdp_growth, row.names = FALSE, quote = FALSE)
  ),
  collapse = "\n"
)
upload_data_to_map(state_map,csv_data)


#Example Workflow:-------------------
# Create a chart
chart_id <- create_datawrapper_chart(api_key, title = "Test Chart", type = "bar-chart")

# Upload data
data <- data.frame(Category = c("A", "B"), Value = c(100, 200))
upload_data_to_chart(api_key, chart_id, data)

# Edit metadata
metadata <- list(visualize = list(`color-category` = list(map = list(A = "#FF0000", B = "#00FF00"))))
edit_datawrapper_chart(api_key, chart_id, metadata)

# Publish the chart
publish_datawrapper_chart(api_key, chart_id)

#Stacked Investment Chart
chart<-create_datawrapper_chart(title="AZ Investment",
                                type="stacked-column-chart",
                                folder_id = "280831",
                                theme = "RMI")
data <- investment_state_quarter %>% filter(State_Name == "Arizona") %>% select(-State_Name)
upload_data_to_chart(api_key, chart$id, data)

metadata <- list(
  `describe`= list(
  `intro` = "US Population, 1950-2019.",
  `byline` = "Lisa Charlotte Rost, Datawrapper",
  `source-url` = "http://data.un.org/",
  `source-name` = "UN Data"),
  visualize = list(
    `color-by-column`=TRUE,
    `categoryLabels`=list(
      position="color legend"
    ),
    `show-color-key`=TRUE,
    `color-category` = list(
      map = list(Buildings = "#0BD0D9", Power = "#0989B1",Transport="#003A61","Clean Tech Manufacturing"="#FFCA08",Industry="#F8931D")
      )
    ),
  publish=list(
    blocks=list(
      `logo`=list(
        enabled=TRUE
      ),
      `get-the-data`=TRUE
    )
  )
  )
edit_datawrapper_chart(api_key, chart$id, metadata)
publish_datawrapper_chart(api_key, chart$id)
published_chart <- publish_datawrapper_chart(api_key, chart$id)
values$iframeCode <- published_chart$data$metadata$publish$`embed-codes`$`embed-method-iframe`



rmi_palette<-c("#0BD0D9",
               "#0989B1",
               "#003A61",
               "#FFCA08",
               "#F8931D",
               "#548538",
               "#7F7F7F")

# Adding distinct colors manually
additional_colors <- c("#E63946", "#F4A261", "#2A9D8F", "#264653", "#E76F51", "#8D99AE", "#9C6644","#6A4C93",  # Purple
                       "#F77F00",  # Vivid orange
                       "#80B918",  # Lime green
                       "#005F73",  # Dark cyan
                       "#9D4EDD",  # Lavender
                       "#EF233C")   # Crimson