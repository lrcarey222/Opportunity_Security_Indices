# Chart of the Day -> Datawrapper chart (pure): type, CSV-ready data frame and metadata in the NEIS theme.
# The script sends it through the API (scripts/chart_of_day/01_chart_of_day.R).

COD_DW_COL <- list(navy = "#001f3e", orange = "#f58229", blue = "#1189b1", green = "#a3b53a", teal = "#55c3c4", grey = "#646e77",
                   light = "#c9cfd4")
COD_BLOC_COL <- c(`U.S.-leaning` = "#1189b1", `China-leaning` = "#f58229", Nonaligned = "#646e77", Others = "#c9cfd4")

cod_dw_number_format <- function(fmt) switch(fmt, pct = "0%", pct2 = "0.00%", usd = "$0.[0]a", usd_mwh = "$0", num = "0,0.[0]", count = "0,0", "0,0")

cod_dw_spec <- function(spec, byline = "NEIS / RMI") {
  ch <- spec$chart; r <- spec$rows
  fmt <- ch$format
  vis <- list(); axes <- NULL
  if (ch$type == "bar") {
    type <- "d3-bars"
    data <- data.frame(Country = r$label, Value = r$value, check.names = FALSE, stringsAsFactors = FALSE)
    names(data)[2] <- if (fmt %in% c("pct", "pct2")) "Share" else "Value"
    pal <- if (!is.null(ch$colors)) unlist(ch$colors) else COD_BLOC_COL
    cols <- if (!is.null(r$group)) unname(pal[r$group]) else ifelse(r$highlight, COD_DW_COL$orange, COD_DW_COL$navy)
    cols[is.na(cols)] <- COD_DW_COL$grey
    vis <- list(`custom-colors` = as.list(stats::setNames(cols, r$label)), `value-label-format` = cod_dw_number_format(fmt),
                `show-value-labels` = TRUE, sort = list(enabled = FALSE))
  } else if (ch$type == "grouped_bar") {
    type <- "d3-bars-grouped"
    cats <- unique(r$category)
    data <- data.frame(Step = cats, Exports = r$value[r$series == "Exports"][match(cats, r$category[r$series == "Exports"])],
                       Imports = r$value[r$series == "Imports"][match(cats, r$category[r$series == "Imports"])], check.names = FALSE)
    vis <- list(`custom-colors` = list(Exports = COD_DW_COL$green, Imports = COD_DW_COL$blue), `value-label-format` = cod_dw_number_format(fmt))
  } else if (ch$type == "lines") {
    type <- "d3-lines"
    s <- unique(r$series)
    data <- data.frame(Year = sort(unique(r$x)))
    for (k in s) data[[k]] <- r$value[r$series == k][match(data$Year, r$x[r$series == k])]
    data$Year <- as.character(data$Year)
    focus <- r$series[r$highlight][1]
    lc <- stats::setNames(rep(COD_DW_COL$light, length(s)), s); lc[focus] <- COD_DW_COL$orange
    vis <- list(`custom-colors` = as.list(lc), lines = stats::setNames(lapply(s, function(k) list(width = if (k == focus) "style3" else "style1")), s),
                `label-colors` = TRUE, `y-grid-format` = cod_dw_number_format(fmt))
  } else if (ch$type == "scatter") {
    type <- "d3-scatter-plot"
    data <- data.frame(Country = r$label, Imports = r$x, Exports = r$y, Group = ifelse(r$highlight, spec$focus$name, "Other countries"),
                       Label = ifelse(r$show_label, r$label, ""), check.names = FALSE)
    axes <- list(x = "Imports", y = "Exports", color = "Group", labels = "Label")
    vis <- list(`x-log` = TRUE, `y-log` = TRUE, `color-category` = list(map = stats::setNames(list(COD_DW_COL$orange, COD_DW_COL$light),
                c(spec$focus$name, "Other countries"))), `show-color-key` = FALSE, `regression` = FALSE)
  } else if (ch$type == "stacked_bar") {
    type <- "stacked-column-chart"
    ser <- unlist(ch$series)
    data <- data.frame(Year = sort(unique(r$x)))
    for (k in ser) data[[k]] <- r$value[r$series == k][match(data$Year, r$x[r$series == k])]
    data$Year <- as.character(data$Year)
    vis <- list(`custom-colors` = ch$colors, `value-label-format` = cod_dw_number_format(fmt))
  } else if (ch$type == "dumbbell") {
    type <- "d3-arrow-plot"
    data <- data.frame(Step = r$label, a = round(100 * r$from, 1), b = round(100 * r$to, 1), check.names = FALSE)
    names(data)[2:3] <- c(ch$from_label, ch$to_label)
    axes <- list(start = ch$from_label, end = ch$to_label, labels = "Step")
    vis <- list(`range-value-labels` = "both", `value-label-format` = "0", `color-by` = "direction",
                `custom-range` = list(0, 100), `sort-by` = "end", `sort-range` = list(enabled = FALSE),
                `color-category` = list(map = list(increase = COD_DW_COL$orange, decrease = COD_DW_COL$blue)))
  } else if (ch$type == "tiles") {
    type <- "tables"
    rows_order <- unlist(ch$rows_order); cols_order <- unlist(ch$cols_order)
    cols_order <- cols_order[cols_order %in% r$col]
    data <- data.frame(Technology = rows_order, check.names = FALSE)
    for (k in cols_order) data[[k]] <- round(100 * r$value[r$col == k][match(rows_order, r$row[r$col == k])])
    vis <- list(heatmap = list(enabled = TRUE, mode = "continuous", stops = "equidistant", rangeMin = "0", rangeMax = "100",
                               colors = list(list(color = "#ecf1f4", position = 0), list(color = "#f9c79a", position = 0.4),
                                             list(color = "#f58229", position = 0.7), list(color = "#9a4a0c", position = 1))),
                columns = stats::setNames(lapply(cols_order, function(x) list(heatmap = TRUE, `number-append` = "%")), cols_order))
  } else stop("unknown chart type ", ch$type)
  meta <- list(describe = list(intro = spec$subtitle, `source-name` = spec$source, `source-url` = "", byline = byline),
               annotate = list(notes = paste(c(spec$notes, sprintf("Chart of the Day, %s.", spec$date)), collapse = " ")),
               visualize = vis)
  if (!is.null(axes)) meta$axes <- axes
  list(key = spec$date, title = spec$title, type = type, data = data, metadata = meta)
}
