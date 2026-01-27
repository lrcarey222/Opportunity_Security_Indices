frame_path <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
cwd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
if (!is.null(frame_path) && nzchar(frame_path)) {
  app_dir <- normalizePath(dirname(frame_path), winslash = "/", mustWork = FALSE)
} else if (file.exists(file.path(cwd, "app.R"))) {
  app_dir <- cwd
} else if (file.exists(file.path(cwd, "shiny", "app.R"))) {
  app_dir <- normalizePath(file.path(cwd, "shiny"), winslash = "/", mustWork = FALSE)
} else {
  app_dir <- cwd
}
source(file.path(app_dir, "R", "helpers.R"))

app_dir <- resolve_app_dir()
repo_root <- resolve_repo_root(app_dir)

required_packages <- c(
  "shiny",
  "bslib",
  "dplyr",
  "scales",
  "countrycode",
  "leaflet",
  "sf",
  "rnaturalearth",
  "rnaturalearthdata"
)
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

app_data <- load_index_data(app_dir, repo_root)
index_data <- app_data$data

metric_choices <- sort(unique(index_data$metric))
tech_choices <- sort(unique(index_data$tech))
if (length(tech_choices) == 0) {
  tech_choices <- "All"
}
tech_choices <- tech_choices[!is.na(tech_choices)]

supply_choices <- sort(unique(index_data$supply_chain))
if (length(supply_choices) == 0) {
  supply_choices <- "All"
}
supply_choices <- supply_choices[!is.na(supply_choices)]

ui <- bslib::page_sidebar(
  title = "Opportunity & Security Indices",
  theme = bslib::bs_theme(bootswatch = "flatly"),
  sidebar = bslib::sidebar(
    shiny::h4("Map controls"),
    shiny::selectInput("metric", "Index", choices = metric_choices, selected = metric_choices[1]),
    shiny::selectInput("tech", "Technology", choices = tech_choices, selected = tech_choices[1]),
    shiny::selectInput("supply_chain", "Supply chain", choices = supply_choices, selected = supply_choices[1]),
    shiny::hr(),
    shiny::h5("Data source"),
    shiny::verbatimTextOutput("data_source", placeholder = TRUE),
    shiny::h5("Dependencies"),
    shiny::uiOutput("dependency_notice")
  ),
  shiny::uiOutput("map_ui")
)

server <- function(input, output, session) {
  output$data_source <- shiny::renderText({
    paste0(app_data$source, " (", app_data$detail, ")")
  })

  output$dependency_notice <- shiny::renderUI({
    if (length(missing_packages) == 0) {
      shiny::tags$span("All required packages available.")
    } else {
      shiny::tags$div(
        shiny::tags$p("Missing packages:"),
        shiny::tags$code(paste(missing_packages, collapse = ", ")),
        shiny::tags$p("Install the packages above to enable the interactive map.")
      )
    }
  })

  output$map_ui <- shiny::renderUI({
    if (length(missing_packages) > 0) {
      return(shiny::tags$div(
        class = "p-4",
        shiny::tags$h4("Interactive map unavailable"),
        shiny::tags$p("Install the missing packages listed in the sidebar to render the map.")
      ))
    }
    leaflet::leafletOutput("map", height = "650px")
  })

  map_data <- shiny::reactive({
    if (length(missing_packages) > 0) {
      return(NULL)
    }
    filtered <- summarize_for_map(
      index_data,
      input$metric,
      input$tech,
      input$supply_chain
    )
    world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
    world <- sf::st_transform(world, 4326)
    dplyr::left_join(world, filtered, by = c("iso_a3" = "iso3"))
  })

  output$map <- leaflet::renderLeaflet({
    map_tbl <- map_data()
    if (is.null(map_tbl)) {
      return(NULL)
    }
    map_tbl_data <- map_tbl[!is.na(map_tbl$value), , drop = FALSE]
    if (nrow(map_tbl_data) == 0) {
      map_tbl_data <- map_tbl
    }
    pal <- leaflet::colorNumeric(
      palette = "YlGnBu",
      domain = map_tbl$value,
      na.color = "#e0e0e0"
    )
    label_text <- paste0(
      map_tbl_data$name,
      ": ",
      ifelse(is.na(map_tbl_data$value), "No data", scales::number(map_tbl_data$value, accuracy = 0.01))
    )

    bbox <- sf::st_bbox(map_tbl_data)
    leaflet::leaflet(map_tbl_data, options = leaflet::leafletOptions(worldCopyJump = FALSE)) %>%
      leaflet::addProviderTiles(
        "CartoDB.Positron",
        options = leaflet::providerTileOptions(noWrap = TRUE)
      ) %>%
      leaflet::addPolygons(
        fillColor = ~pal(value),
        color = "#666666",
        weight = 0.5,
        fillOpacity = 0.7,
        label = label_text,
        highlightOptions = leaflet::highlightOptions(weight = 2, color = "#000000", bringToFront = TRUE)
      ) %>%
      leaflet::fitBounds(
        lng1 = bbox[["xmin"]],
        lat1 = bbox[["ymin"]],
        lng2 = bbox[["xmax"]],
        lat2 = bbox[["ymax"]]
      ) %>%
      leaflet::addLegend(
        position = "bottomright",
        pal = pal,
        values = ~value,
        title = input$metric
      )
  })
}

app <- shiny::shinyApp(ui = ui, server = server)
app
