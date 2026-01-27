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
source(file.path(app_dir, "R", "datawrapper.R"))

app_dir <- resolve_app_dir()
repo_root <- resolve_repo_root(app_dir)

required_packages <- c(
  "shiny",
  "bslib",
  "dplyr",
  "scales",
  "countrycode"
)
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
datawrapper_packages <- c("httr", "jsonlite")
missing_dw_packages <- datawrapper_packages[
  !vapply(datawrapper_packages, requireNamespace, logical(1), quietly = TRUE)
]

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
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::textInput("dw_chart_id", "Datawrapper chart ID (optional)", Sys.getenv("DATAWRAPPER_CHART_ID_WORLD", "")),
      shiny::selectInput(
        "dw_map_key_attr",
        "Basemap key attribute",
        choices = c("ISO3", "Name"),
        selected = "ISO3"
      ),
      shiny::tags$p(
        class = "text-muted",
        "Use ISO3 for ISO-3 country codes. If joins fail, switch to Name and",
        "ensure the country names match Datawrapper's world basemap."
      ),
      shiny::actionButton("dw_update", "Update Datawrapper map"),
      shiny::verbatimTextOutput("dw_status")
    ),
    shiny::uiOutput("dw_iframe")
  )
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
    shiny::uiOutput("map")
  })

  dw_status <- shiny::reactiveVal("Datawrapper disabled. Set DATAWRAPPER_API_KEY to enable publishing.")
  dw_iframe_src <- shiny::reactiveVal(NULL)
  dw_chart_cache <- file.path(
    path.expand("~"),
    ".config",
    "opportunity_security_indices",
    "datawrapper_chart_world.txt"
  )

  output$dw_status <- shiny::renderText({
    dw_status()
  })

  output$dw_iframe <- shiny::renderUI({
    src <- dw_iframe_src()
    if (is.null(src) || !nzchar(src)) {
      shiny::tags$p(class = "text-muted", "No Datawrapper chart embedded yet.")
    } else {
      shiny::tags$iframe(
        src = src,
        width = "100%",
        height = "800px",
        frameborder = 0
      )
    }
  })

  shiny::observeEvent(input$dw_update, {
    api_key <- Sys.getenv("DATAWRAPPER_API_KEY", "")
    if (!nzchar(api_key)) {
      dw_status("Datawrapper disabled. Set DATAWRAPPER_API_KEY to enable publishing.")
      dw_iframe_src(NULL)
      return(NULL)
    }
    if (length(missing_dw_packages) > 0) {
      dw_status(paste(
        "Missing packages for Datawrapper:",
        paste(missing_dw_packages, collapse = ", ")
      ))
      dw_iframe_src(NULL)
      return(NULL)
    }

    metric_unit <- NULL
    if ("unit" %in% names(index_data)) {
      metric_unit <- unique(index_data$unit[index_data$metric == input$metric])
      metric_unit <- metric_unit[!is.na(metric_unit) & nzchar(metric_unit)]
      if (length(metric_unit) != 1) {
        metric_unit <- NULL
      }
    }
    legend_title <- if (!is.null(metric_unit)) {
      paste0(input$metric, " (", metric_unit, ")")
    } else {
      input$metric
    }

    key_column <- if (identical(input$dw_map_key_attr, "Name")) {
      "country"
    } else {
      "iso3"
    }

    filtered <- summarize_for_map(
      index_data,
      input$metric,
      input$tech,
      input$supply_chain
    )
    filtered <- filtered[!is.na(filtered$value), , drop = FALSE]
    if (nrow(filtered) == 0) {
      dw_status("No data available for the selected filters.")
      dw_iframe_src(NULL)
      return(NULL)
    }

    dw_data <- filtered[, c("iso3", "country", "value"), drop = FALSE]
    csv_data <- paste(
      capture.output(utils::write.csv(dw_data, row.names = FALSE, quote = TRUE)),
      collapse = "\n"
    )

    metadata <- list(
      axes = list(
        keys = key_column,
        values = "value"
      ),
      describe = list(
        intro = paste("Opportunity & Security Indices:", input$metric)
      ),
      visualize = list(
        basemap = "world",
        `map-key-attr` = input$dw_map_key_attr,
        "tooltip-title" = "{{country}}",
        "tooltip-body" = "{{ ROUND(value, 2) }}",
        "color-scale" = list(
          colors = c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b"),
          mode = "linear",
          nullColor = "#e0e0e0"
        ),
        "legend-title" = legend_title
      )
    )

    chart_id <- NULL
    chart_id_env <- Sys.getenv("DATAWRAPPER_CHART_ID_WORLD", "")
    chart_id_input <- input$dw_chart_id
    if (nzchar(chart_id_input)) {
      chart_id <- chart_id_input
    } else if (nzchar(chart_id_env)) {
      chart_id <- chart_id_env
    } else if (file.exists(dw_chart_cache)) {
      chart_id <- trimws(readLines(dw_chart_cache, warn = FALSE))
      if (!nzchar(chart_id)) {
        chart_id <- NULL
      }
    }

    result <- tryCatch({
      if (is.null(chart_id)) {
        chart_id <- dw_create_chart(title = paste("Opportunity & Security Indices:", input$metric))
        dir.create(dirname(dw_chart_cache), recursive = TRUE, showWarnings = FALSE)
        writeLines(chart_id, dw_chart_cache)
      }
      dw_upload_csv(chart_id, csv_data)
      dw_patch_metadata(chart_id, metadata, title = paste("Opportunity & Security Indices:", input$metric))
      publish_response <- dw_publish(chart_id)
      iframe_src <- dw_get_iframe_src(publish_response)
      list(
        status = paste("Published Datawrapper chart", chart_id),
        iframe_src = iframe_src
      )
    }, error = function(err) {
      list(
        status = paste("Datawrapper update failed:", conditionMessage(err)),
        iframe_src = NULL
      )
    })

    dw_status(result$status)
    dw_iframe_src(result$iframe_src)
  })

  output$map <- shiny::renderUI({
    src <- dw_iframe_src()
    if (is.null(src) || !nzchar(src)) {
      shiny::tags$div(
        class = "p-4",
        shiny::tags$h4("Datawrapper map unavailable"),
        shiny::tags$p("Use the Datawrapper tab to publish a map and embed it here.")
      )
    } else {
      shiny::tags$iframe(
        src = src,
        width = "100%",
        height = "800px",
        frameborder = 0
      )
    }
  })
}

app <- shiny::shinyApp(ui = ui, server = server)
app
