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
index_data <- app_data$data %>%
  filter(tech %in% techs)

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
    shiny::actionButton("dw_update", "Update Map"),
    shiny::hr(),
    shiny::h5("Data source"),
    shiny::verbatimTextOutput("data_source", placeholder = TRUE),
    shiny::h5("Dependencies"),
    shiny::uiOutput("dependency_notice")
  ),
  shiny::tags$script(
    shiny::HTML(paste0(
      "(function(){\n",
      "  function parseMessage(eventData) {\n",
      "    var data = eventData;\n",
      "    if (!data) return null;\n",
      "    if (typeof data === 'string') {\n",
      "      try { data = JSON.parse(data); } catch (e) { return null; }\n",
      "    }\n",
      "    return data;\n",
      "  }\n",
      "  window.addEventListener('message', function(event) {\n",
      "    var iframe = document.getElementById('dw_map_iframe');\n",
      "    if (!iframe || event.source !== iframe.contentWindow) return;\n",
      "    var data = parseMessage(event.data);\n",
      "    if (!data || data['datawrapper-height']) return;\n",
      "    var payload = data.data || data.payload || data;\n",
      "    var country = payload.country || payload.name || payload.label || null;\n",
      "    var iso3 = payload.iso3 || payload.ISO3 || payload.key || null;\n",
      "    if (country || iso3) {\n",
      "      if (window.Shiny) {\n",
      "        Shiny.setInputValue('dw_selected_country', {\n",
      "          country: country,\n",
      "          iso3: iso3,\n",
      "          nonce: Date.now()\n",
      "        }, {priority: 'event'});\n",
      "      }\n",
      "    }\n",
      "  }, false);\n",
      "})();\n"
    ))
  ),
  shiny::verbatimTextOutput("dw_status"),
  shiny::uiOutput("dw_iframe"),
  shiny::uiOutput("metric_note"),
  shiny::tags$h5("Country comparison"),
  shiny::tags$p(class = "text-muted", "Click a country in the map to view all tech × supply-chain combinations."),
  shiny::plotOutput("country_scatter", height = "1000px")
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
        id = "dw_map_iframe",
        src = src,
        width = "100%",
        height = "800px",
        frameborder = 0
      )
    }
  })

  output$metric_note <- shiny::renderUI({
    if (identical(input$metric, "Energy Security Index")) {
      shiny::tags$div(
        class = "text-muted mt-2",
        shiny::tags$strong("Energy Security Index:"),
        "Measures resilience and security across energy supply chains.",
        "Raw metrics are normalized to 0–1 with a median-centered S-curve, then",
        "category scores are aggregated into a weighted mean within each",
        "country × tech × supply-chain group (weights in config/weights.yml)."
      )
    } else if (identical(input$metric, "Economic Opportunity Index")) {
      shiny::tags$div(
        class = "text-muted mt-2",
        shiny::tags$strong("Economic Opportunity Index:"),
        "Captures opportunity for growth and competitiveness in energy-related markets.",
        "Raw metrics are normalized to 0–1 with a median-centered S-curve, then",
        "category scores are aggregated into a weighted mean within each",
        "country × tech × supply-chain group (weights in config/weights.yml)."
      )
    } else {
      NULL
    }
  })

  selected_country <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$dw_selected_country, {
    selected_country(input$dw_selected_country)
  })

  output$country_scatter <- shiny::renderPlot({
    selected <- selected_country()
    if (is.null(selected)) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, "Select a country on the map to view its position.")
      return()
    }

    es_tbl <- index_data[index_data$metric == "Energy Security Index", , drop = FALSE]
    eo_tbl <- index_data[index_data$metric == "Economic Opportunity Index", , drop = FALSE]

    if (!is.null(selected$iso3) && nzchar(selected$iso3)) {
      es_tbl <- es_tbl[es_tbl$iso3 == selected$iso3, , drop = FALSE]
      eo_tbl <- eo_tbl[eo_tbl$iso3 == selected$iso3, , drop = FALSE]
    } else if (!is.null(selected$country) && nzchar(selected$country)) {
      es_tbl <- es_tbl[es_tbl$country == selected$country, , drop = FALSE]
      eo_tbl <- eo_tbl[eo_tbl$country == selected$country, , drop = FALSE]
    }

    merged <- merge(
      es_tbl[, c("country", "iso3", "tech", "supply_chain", "value")],
      eo_tbl[, c("country", "iso3", "tech", "supply_chain", "value")],
      by = c("iso3", "tech", "supply_chain"),
      suffixes = c("_es", "_eo"),
      all = FALSE
    )

    if (nrow(merged) == 0) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, "No index data available for the selected country.")
      return()
    }

    x_min <- min(merged$value_eo, na.rm = TRUE)
    x_max <- max(merged$value_eo, na.rm = TRUE)
    y_min <- min(merged$value_es, na.rm = TRUE)
    y_max <- max(merged$value_es, na.rm = TRUE)

    graphics::plot(
      merged$value_eo,
      merged$value_es,
      xlab = "Economic Opportunity Index",
      ylab = "Energy Security Index",
      xlim = c(x_min, x_max),
      ylim = c(y_min, y_max),
      pch = 19,
      col = "#1f78b4",
      asp = 1
    )

    graphics::text(
      merged$value_eo,
      merged$value_es,
      labels = paste(merged$tech, merged$supply_chain, sep = " · "),
      pos = 3,
      cex = 0.7
    )
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

    key_column <- "iso3"

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
        `map-key-attr` = "ISO3",
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
    if (nzchar(chart_id_env)) {
      chart_id <- chart_id_env
    } else if (file.exists(dw_chart_cache)) {
      chart_id <- trimws(readLines(dw_chart_cache, warn = FALSE))
      if (!nzchar(chart_id)) {
        chart_id <- NULL
      }
    }

    result <- tryCatch({
      if (is.null(chart_id)) {
        chart_id <- dw_create_chart(title = paste(input$tech, input$supply_chain, input$metric))
        dir.create(dirname(dw_chart_cache), recursive = TRUE, showWarnings = FALSE)
        writeLines(chart_id, dw_chart_cache)
      }
      dw_upload_csv(chart_id, csv_data)
      dw_patch_metadata(chart_id, metadata, title = paste(input$tech, input$supply_chain, input$metric))
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

}

app <- shiny::shinyApp(ui = ui, server = server)
app
