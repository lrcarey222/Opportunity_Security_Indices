#!/usr/bin/env Rscript

source(local({
  # Prefer sys.frame(1)$ofile when sourced (e.g., from run_pipeline.R).
  sf <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  this_file <- if (!is.null(sf) && nzchar(sf)) sf else {
    ofiles <- vapply(sys.frames(), function(fr) {
      of <- tryCatch(fr$ofile, error = function(e) NULL)
      if (is.null(of) || !nzchar(of)) "" else as.character(of)
    }, character(1))
    ofiles <- ofiles[nzchar(ofiles)]
    if (length(ofiles) > 0) ofiles[[length(ofiles)]] else {
      # Fallback for direct Rscript execution of this script.
      fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
      if (length(fa) > 0) sub("^--file=", "", fa[1]) else ""
    }
  }
  if (!nzchar(this_file)) {
    candidate <- file.path(normalizePath(getwd(), winslash = "/", mustWork = FALSE), "scripts", "utils", "bootstrap.R")
    if (file.exists(candidate)) return(candidate)
    stop("Unable to resolve script path for bootstrap.")
  }
  file.path(dirname(normalizePath(this_file, winslash = "/", mustWork = FALSE)), "utils", "bootstrap.R")
}))

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "charts", "package_selection_viz.R"))
source(file.path(repo_root, "R", "charts", "package_selection_raw_highlights.R"))

DEFAULT_COUNTRY <- "United States"

parse_args <- function(args) {
  out <- list(country = NULL, out_dir = NULL, top_n = 10L, selected = NULL, top_k_vars = 5L, include_raw_highlights = TRUE)
  for (arg in args) {
    if (startsWith(arg, "--country=")) out$country <- sub("^--country=", "", arg)
    if (startsWith(arg, "--out-dir=")) out$out_dir <- sub("^--out-dir=", "", arg)
    if (startsWith(arg, "--top-n=")) out$top_n <- as.integer(sub("^--top-n=", "", arg))
    if (startsWith(arg, "--selected=")) out$selected <- sub("^--selected=", "", arg)
    if (startsWith(arg, "--top-k-vars=")) out$top_k_vars <- as.integer(sub("^--top-k-vars=", "", arg))
    if (startsWith(arg, "--include-raw-highlights=")) {
      out$include_raw_highlights <- tolower(sub("^--include-raw-highlights=", "", arg)) %in% c("true", "1", "yes", "y")
    }
  }
  out
}

split_selected <- function(x) {
  if (is.null(x) || !nzchar(x)) return(character())
  stringr::str_split(x, pattern = ";", simplify = FALSE)[[1]] %>%
    stringr::str_trim() %>%
    .[nzchar(.)]
}

resolve_outputs_rds <- function(repo_root, config) {
  candidates <- c(
    Sys.getenv("OPSI_OUTPUTS_RDS", ""),
    file.path(repo_root, config$processed_dir %||% "data/processed", "outputs", "index_outputs.rds"),
    file.path(repo_root, "outputs", "index_outputs.rds")
  )
  candidates <- candidates[nzchar(candidates)]
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0) {
    stop("Could not locate index_outputs.rds. Checked: ", paste(candidates, collapse = ", "))
  }
  found[[1]]
}

safe_chart <- function(expr) {
  tryCatch(expr, error = function(e) {
    message("Skipping chart due to error: ", e$message)
    NULL
  })
}

slugify <- function(x) {
  tolower(gsub("(^_+|_+$)", "", gsub("[^a-zA-Z0-9]+", "_", x)))
}

category_theme_file_map <- function() {
  list(
    "Trade" = c("trade_concentration_tbl.rds", "export_feasibility_tbl.rds"),
    "Foreign Dependency" = c("foreign_dependency_tbl.rds", "market_share_manufacturing_tbl.rds", "critical_minerals_processing_tbl.rds"),
    "Minerals Trade" = c("critical_minerals_trade_tbl.rds"),
    "Production" = c("production_depth_momentum_tbl.rds", "critical_minerals_production_tbl.rds"),
    "Reserves" = c("reserves_tbl.rds", "solar_pv_potential_tbl.rds", "wind_potential_tbl.rds", "geothermal_potential_tbl.rds"),
    "Energy Imports" = c("import_dependence_tbl.rds"),
    "Energy Access" = c("energy_access_tbl.rds"),
    "Consumption" = c("energy_consumption_tbl.rds"),
    "Energy Prices" = c("energy_prices_tbl.rds", "lcoe_competitiveness_tbl.rds"),
    "Technology Demand" = c("future_demand_tbl.rds", "overcapacity_premium_tbl.rds"),
    "Cost Competitiveness" = c("cost_competitiveness_tbl.rds"),
    "Technological Readiness" = c("technological_readiness_tbl.rds")
  )
}

load_theme_tbls_for_categories <- function(processed_dir, categories = character()) {
  all_candidates <- list.files(processed_dir, pattern = "_tbl\\.rds$", full.names = TRUE)
  if (length(all_candidates) == 0) {
    return(list())
  }

  file_map <- category_theme_file_map()
  mapped <- unlist(file_map[categories], use.names = FALSE)
  mapped_paths <- file.path(processed_dir, mapped)
  mapped_paths <- mapped_paths[file.exists(mapped_paths)]

  fallback_paths <- setdiff(all_candidates, mapped_paths)
  load_paths <- unique(c(mapped_paths, fallback_paths))

  tbls <- purrr::map(load_paths, ~ tryCatch(readRDS(.x), error = function(e) NULL))
  names(tbls) <- basename(load_paths)
  tbls[!vapply(tbls, is.null, logical(1))]
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
country <- args$country %||% DEFAULT_COUNTRY
if (!nzchar(country)) {
  stop("Country is empty. Set DEFAULT_COUNTRY in this script or pass --country=\"India\".")
}

config <- getOption("opportunity_security.config")
outputs_dir_name <- args$out_dir %||% config$outputs_dir %||% "outputs"
country_slug <- tolower(gsub("[^a-zA-Z0-9]+", "_", country))
base_out_dir <- file.path(repo_root, outputs_dir_name, "package_selection_viz", country_slug)
plots_dir <- file.path(base_out_dir, "plots")
data_dir <- file.path(base_out_dir, "datawrapper")
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

outputs_rds <- resolve_outputs_rds(repo_root, config)
index_outputs <- readRDS(outputs_rds)
selected_sector_labels <- split_selected(args$selected)
message("Building package-selection outputs for country: ", country)

strategic_tbl <- build_country_strategic_tbl(index_outputs = index_outputs, country = country)
if (length(selected_sector_labels) == 0) {
  selected_sector_labels <- strategic_tbl %>%
    dplyr::arrange(dplyr::desc(.data$strategic_index)) %>%
    dplyr::slice_head(n = 5) %>%
    dplyr::pull(.data$sector_label)
}

manifest <- tibble::tibble(
  chart_key = character(),
  title = character(),
  plot_path = character(),
  csv_path = character(),
  datawrapper_type = character(),
  notes = character()
)

append_manifest <- function(chart_key, title, plot_path, csv_path, datawrapper_type, notes = "") {
  manifest <<- dplyr::bind_rows(
    manifest,
    tibble::tibble(
      chart_key = chart_key,
      title = title,
      plot_path = plot_path,
      csv_path = csv_path,
      datawrapper_type = datawrapper_type,
      notes = notes
    )
  )
}

# (1) heatmap
heatmap_plot <- plot_country_heatmap(strategic_tbl, selected_sector_labels)
heatmap_plot_path <- file.path(plots_dir, "country_strategic_heatmap.png")
ggsave(heatmap_plot_path, heatmap_plot, width = 9, height = 6, dpi = 300, bg = "white")

heatmap_csv <- strategic_tbl %>%
  dplyr::select("tech", "supply_chain", "strategic_index") %>%
  tidyr::pivot_wider(names_from = .data$supply_chain, values_from = .data$strategic_index)
heatmap_csv_path <- file.path(data_dir, "country_strategic_heatmap_wide.csv")
readr::write_csv(heatmap_csv, heatmap_csv_path)
append_manifest("country_strategic_heatmap", "Country strategic-index heatmap", heatmap_plot_path, heatmap_csv_path, "d3-heatmap")

# (2) scatter
scatter_plot <- plot_country_scatter(strategic_tbl, selected_sector_labels)
scatter_plot_path <- file.path(plots_dir, "country_eo_vs_es_risk_scatter.png")
ggsave(scatter_plot_path, scatter_plot, width = 9, height = 6, dpi = 300, bg = "white")

scatter_csv <- strategic_tbl %>%
  dplyr::mutate(selected = .data$sector_label %in% selected_sector_labels) %>%
  dplyr::select("sector_label", "tech", "supply_chain", "eo", "es_risk", "pol", "trl_index", "strategic_index", "selected")
scatter_csv_path <- file.path(data_dir, "country_eo_vs_es_risk_scatter.csv")
readr::write_csv(scatter_csv, scatter_csv_path)
append_manifest("country_eo_vs_es_risk_scatter", "EO vs ES-risk scatter", scatter_plot_path, scatter_csv_path, "d3-scatter-plot")

# (3) top-N decomposition
topn_tbl <- build_topn_contrib_tbl(strategic_tbl, top_n = args$top_n)
topn_long <- attr(topn_tbl, "topn_long_tbl")

topn_plot <- plot_topn_contrib(topn_long)
topn_plot_path <- file.path(plots_dir, "topn_sector_decomposition.png")
ggsave(topn_plot_path, topn_plot, width = 10, height = 7, dpi = 300, bg = "white")

topn_csv_path <- file.path(data_dir, "topn_sector_decomposition.csv")
readr::write_csv(topn_tbl %>% dplyr::select(-dplyr::all_of("Country")), topn_csv_path)
append_manifest("topn_sector_decomposition", "Top-N sectors decomposition", topn_plot_path, topn_csv_path, "d3-bars")

# (4) ES category contributions (selected)
es_wide <- build_category_contrib_wide(
  contrib_tbl = index_outputs$energy_security_category_contributions,
  country = country,
  selected_sector_labels = selected_sector_labels,
  pillar = "ES"
)
if (nrow(es_wide) > 0) {
  es_plot <- plot_category_contrib(es_wide, pillar = "ES")
  es_plot_path <- file.path(plots_dir, "es_category_contributions_selected.png")
  ggsave(es_plot_path, es_plot, width = 10, height = 7, dpi = 300, bg = "white")

  es_csv_path <- file.path(data_dir, "es_category_contributions_selected_wide.csv")
  readr::write_csv(es_wide, es_csv_path)
  append_manifest("es_category_contributions_selected", "ES category contributions (selected)", es_plot_path, es_csv_path, "d3-bars")
} else {
  message("Skipping ES selected category chart; no matching data.")
}

# (5) EO category contributions (selected)
eo_wide <- build_category_contrib_wide(
  contrib_tbl = index_outputs$economic_opportunity_category_contributions,
  country = country,
  selected_sector_labels = selected_sector_labels,
  pillar = "EO"
)
if (nrow(eo_wide) > 0) {
  eo_plot <- plot_category_contrib(eo_wide, pillar = "EO")
  eo_plot_path <- file.path(plots_dir, "eo_category_contributions_selected.png")
  ggsave(eo_plot_path, eo_plot, width = 10, height = 7, dpi = 300, bg = "white")

  eo_csv_path <- file.path(data_dir, "eo_category_contributions_selected_wide.csv")
  readr::write_csv(eo_wide, eo_csv_path)
  append_manifest("eo_category_contributions_selected", "EO category contributions (selected)", eo_plot_path, eo_csv_path, "d3-bars")
} else {
  message("Skipping EO selected category chart; no matching data.")
}

# (6) top variable contributions and raw highlights for selected sectors
for (pillar in c("EO", "ES")) {
  var_contrib_tbl <- resolve_variable_contrib_tbl(index_outputs, pillar = pillar)
  if (nrow(var_contrib_tbl) == 0) {
    message("Skipping ", pillar, " variable outputs; variable contribution table not found.")
    next
  }

  top_var_long <- build_top_variable_contrib_long(
    var_contrib_tbl = var_contrib_tbl,
    country = country,
    selected_sector_labels = selected_sector_labels,
    top_k = args$top_k_vars
  )

  if (nrow(top_var_long) == 0) {
    message("Skipping ", pillar, " variable outputs; no matching selected sectors.")
    next
  }

  top_var_wide <- build_top_variable_contrib_wide(top_var_long)
  top_var_wide_path <- file.path(data_dir, paste0(tolower(pillar), "_top_variable_contributions_selected_wide.csv"))
  top_var_long_path <- file.path(data_dir, paste0(tolower(pillar), "_top_variable_contributions_selected_long.csv"))
  readr::write_csv(top_var_wide, top_var_wide_path)
  readr::write_csv(top_var_long, top_var_long_path)

  top_var_plot <- safe_chart(plot_top_variable_contrib(top_var_long, pillar = pillar))
  top_var_plot_path <- file.path(plots_dir, paste0(tolower(pillar), "_top_variable_contributions_selected.png"))
  if (!is.null(top_var_plot)) {
    ggsave(top_var_plot_path, top_var_plot, width = 11, height = 8, dpi = 300, bg = "white")
  }

  append_manifest(
    chart_key = paste0(tolower(pillar), "_top_variable_contributions_selected"),
    title = paste0(pillar, " top variable contributions (selected sectors)"),
    plot_path = top_var_plot_path,
    csv_path = top_var_wide_path,
    datawrapper_type = "d3-bars",
    notes = paste("Audit long CSV:", basename(top_var_long_path))
  )

  if (isTRUE(args$include_raw_highlights)) {
    processed_dir <- file.path(repo_root, config$processed_dir %||% "data/processed")
    if (!dir.exists(processed_dir)) {
      message("Skipping ", pillar, " raw highlights; processed_dir not found: ", processed_dir)
      next
    }

    theme_tbls <- load_theme_tbls_for_categories(processed_dir, categories = unique(top_var_long$category))
    if (length(theme_tbls) == 0) {
      message("Skipping ", pillar, " raw highlights; no *_tbl.rds files found in ", processed_dir)
      next
    }

    raw_highlights <- build_raw_highlights_tbl(theme_tbls = theme_tbls, country = country, top_long_tbl = top_var_long)

    raw_long_path <- file.path(data_dir, paste0(tolower(pillar), "_raw_highlights_selected_long.csv"))
    readr::write_csv(raw_highlights, raw_long_path)

    for (sector in unique(raw_highlights$sector_label)) {
      sector_slug <- slugify(sector)
      sector_tbl <- raw_highlights %>%
        dplyr::filter(.data$sector_label == sector) %>%
        dplyr::select(
          .data$variable_label,
          country_value = .data$country_value,
          .data$global_p25,
          .data$global_p50,
          .data$global_p75,
          .data$Year,
          .data$source
        )
      sector_path <- file.path(data_dir, paste0(tolower(pillar), "_raw_highlights_", sector_slug, ".csv"))
      readr::write_csv(sector_tbl, sector_path)
      append_manifest(
        chart_key = paste0(tolower(pillar), "_raw_highlights_", sector_slug),
        title = paste0(pillar, " raw highlights: ", sector),
        plot_path = file.path(plots_dir, paste0(tolower(pillar), "_raw_highlights_selected.png")),
        csv_path = sector_path,
        datawrapper_type = "d3-range-plot",
        notes = "Per-sector raw indicator highlights CSV."
      )
    }

    raw_plot_tbl <- raw_highlights %>% dplyr::filter(!is.na(.data$country_value))
    if (nrow(raw_plot_tbl) > 0) {
      raw_plot <- safe_chart(plot_raw_highlights(raw_plot_tbl, pillar = pillar))
      raw_plot_path <- file.path(plots_dir, paste0(tolower(pillar), "_raw_highlights_selected.png"))
      if (!is.null(raw_plot)) {
        ggsave(raw_plot_path, raw_plot, width = 12, height = 8, dpi = 300, bg = "white")
      }
      append_manifest(
        chart_key = paste0(tolower(pillar), "_raw_highlights_selected"),
        title = paste0(pillar, " raw indicator highlights (selected sectors)"),
        plot_path = raw_plot_path,
        csv_path = raw_long_path,
        datawrapper_type = "d3-range-plot",
        notes = "Long audit CSV with country value, global quartiles, and table match metadata."
      )
    } else {
      message("Skipping ", pillar, " raw highlight plot; no matched raw values.")
    }
  }
}

# (7) Optional partner shortlist
processed_dir <- file.path(repo_root, config$processed_dir %||% "data/processed")
friendshore_path <- file.path(processed_dir, "partner_friendshore_tbl.rds")
opportunity_path <- file.path(processed_dir, "partner_opportunity_tbl.rds")

if (file.exists(friendshore_path) || file.exists(opportunity_path)) {
  friendshore_tbl <- if (file.exists(friendshore_path)) readRDS(friendshore_path) else tibble::tibble()
  opportunity_tbl <- if (file.exists(opportunity_path)) readRDS(opportunity_path) else tibble::tibble()

  required_friendshore <- c("tech", "supply_chain")
  if (all(required_friendshore %in% colnames(friendshore_tbl)) || all(required_friendshore %in% colnames(opportunity_tbl))) {
    message("Partner tables detected; generating optional shortlist only if expected columns are available.")
    if (all(c("reporter_name", "partner_name", "friendshore_index", "tech", "supply_chain") %in% colnames(friendshore_tbl))) {
      partner_tbl <- friendshore_tbl %>%
        dplyr::filter(tolower(.data$reporter_name) == tolower(country)) %>%
        dplyr::mutate(sector_label = paste(.data$tech, .data$supply_chain, sep = " - ")) %>%
        dplyr::filter(.data$sector_label %in% selected_sector_labels) %>%
        dplyr::group_by(.data$sector_label) %>%
        dplyr::slice_max(order_by = .data$friendshore_index, n = args$top_n, with_ties = FALSE) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(opportunity_index = NA_real_) %>%
        dplyr::select("sector_label", partner_name = "partner_name", "friendshore_index", "opportunity_index")

      if (nrow(partner_tbl) > 0) {
        partner_csv_path <- file.path(data_dir, "partner_shortlist_selected.csv")
        readr::write_csv(partner_tbl, partner_csv_path)
        partner_plot <- ggplot2::ggplot(partner_tbl, ggplot2::aes(x = reorder(.data$partner_name, .data$friendshore_index), y = .data$friendshore_index)) +
          ggplot2::geom_col(fill = "#2c7fb8") +
          ggplot2::coord_flip() +
          ggplot2::facet_wrap(~sector_label, scales = "free_y") +
          ggplot2::labs(x = NULL, y = "Friendshore index", title = "Partner shortlist by selected sector") +
          ggplot2::theme_minimal(base_size = 11)
        partner_plot_path <- file.path(plots_dir, "partner_shortlist_selected.png")
        ggsave(partner_plot_path, partner_plot, width = 11, height = 8, dpi = 300, bg = "white")
        append_manifest("partner_shortlist_selected", "Partner shortlist by selected sector", partner_plot_path, partner_csv_path, "d3-bars", notes = "Optional output; generated when partner tables exist.")
      }
    } else {
      message("Partner tables present but required columns not found; skipping optional partner shortlist.")
    }
  }
} else {
  message("Partner tables not found; skipping optional partner shortlist outputs.")
}

manifest_path <- file.path(base_out_dir, "manifest.csv")
readr::write_csv(manifest, manifest_path)

message("Package selection visualization outputs written to: ", base_out_dir)
