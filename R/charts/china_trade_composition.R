# Sectoral composition of China's goods trade.
#
# Replicates the disaggregation in the Federal Reserve FEDS Note "The Sectoral
# Evolution of China's Trade" (28 Feb 2025), which tracks how the sector shares of
# China's exports and imports have shifted - most visibly the rise of road vehicles on
# the export side and their collapse on the import side.
#
# Source here is the Harvard Growth Lab Atlas panel (HS92, 4-digit) rather than raw
# Comtrade, topped up from the Atlas API for years beyond the staged file. Atlas/BACI
# reconciles mirror flows, so shares differ marginally from Comtrade-based figures.
#
# Validation against the Fed note (their SITC 78 vs HS chapter 87 here):
#   export share  2013 -> 2023   Fed 2.9% -> 5.9%   this 2.6% -> 5.6%
#   import share  2013 -> 2023   Fed 4.5% -> 2.7%   this 4.6% -> 2.9%
#
# Run:
#   Rscript R/charts/china_trade_composition.R

source(local({
  resolve_bootstrap_path <- function() {
    candidate_starts <- character()

    sf <- tryCatch(sys.frame(1)$ofile, error = function(e) "")
    if (!is.null(sf) && nzchar(sf)) candidate_starts <- c(candidate_starts, dirname(sf))

    frame_ofiles <- vapply(sys.frames(), function(fr) {
      val <- tryCatch(fr$ofile, error = function(e) "")
      if (is.null(val) || !nzchar(val)) "" else dirname(val)
    }, character(1))
    candidate_starts <- c(candidate_starts, frame_ofiles[nzchar(frame_ofiles)])

    fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(fa) > 0) candidate_starts <- c(candidate_starts, dirname(sub("^--file=", "", fa[1])))

    candidate_starts <- unique(c(candidate_starts, getwd()))

    for (start in candidate_starts) {
      d <- normalizePath(start, winslash = "/", mustWork = FALSE)
      while (dirname(d) != d) {
        bootstrap <- file.path(d, "scripts", "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)

        bootstrap <- file.path(d, "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)

        d <- dirname(d)
      }
    }

    stop("Unable to resolve script path for bootstrap.")
  }

  resolve_bootstrap_path()
}))

# Reuses the Atlas API helpers so the API contract lives in one place.
source(file.path(repo_root, "R", "charts", "export_similarity.R"))

## Sector definition ---------------------------------------------------------
#
# HS2 chapters grouped into readable sectors. Road vehicles (87) is deliberately its
# own sector rather than folded into transport, because it is the Fed note's headline
# and the single largest compositional shift in the period.

CHINA_TRADE_SECTORS <- list(
  `Electronics & electrical`  = "85",
  `Machinery`                 = "84",
  `Road vehicles`             = "87",
  `Other transport`           = c("86", "88", "89"),
  `Precision instruments`     = c("90", "91", "92"),
  `Chemicals, plastics & rubber` = sprintf("%02d", c(28:40)),
  `Base metals & articles`    = sprintf("%02d", 72:83),
  `Textiles, apparel & footwear` = sprintf("%02d", 50:67),
  `Furniture, toys & misc`    = c("94", "95", "96", "97"),
  `Agriculture & food`        = sprintf("%02d", 1:24),
  `Mineral fuels`             = "27",
  `Ores & other minerals`     = c("25", "26", "68", "69", "70", "71"),
  `Wood, paper & leather`     = sprintf("%02d", c(41:49))
)

# Order sectors for stacking: manufactures first, primary goods last.
CHINA_TRADE_SECTOR_ORDER <- c(
  "Electronics & electrical", "Machinery", "Road vehicles", "Other transport",
  "Precision instruments", "Chemicals, plastics & rubber", "Base metals & articles",
  "Textiles, apparel & footwear", "Furniture, toys & misc", "Wood, paper & leather",
  "Agriculture & food", "Ores & other minerals", "Mineral fuels", "Arms & other"
)

# The Atlas carries unallocated trade under XXXX and 9999. This is not a sector: it is
# trade the source could not assign to a product. For China it was 14.8% of exports in
# 1995 and 2.3% by 2023, so leaving it in the denominator would make every sector's
# share drift upward over time for purely measurement reasons. Shares are therefore
# computed over allocated trade only, and the unallocated share is reported alongside.
CHINA_TRADE_UNSPECIFIED_CODES <- c("XXXX", "9999")

china_trade_sector_lookup <- function() {
  chapters <- unlist(CHINA_TRADE_SECTORS, use.names = FALSE)
  labels <- rep(names(CHINA_TRADE_SECTORS), lengths(CHINA_TRADE_SECTORS))
  dup <- chapters[duplicated(chapters)]
  if (length(dup) > 0) {
    stop("Chapter(s) assigned to more than one sector: ", paste(unique(dup), collapse = ", "))
  }
  stats::setNames(labels, chapters)
}

china_trade_assign_sector <- function(hs_codes) {
  lookup <- china_trade_sector_lookup()
  chapter <- substr(hs_codes, 1, 2)
  out <- unname(lookup[chapter])
  out[is.na(out)] <- "Arms & other"
  out[hs_codes %in% CHINA_TRADE_UNSPECIFIED_CODES] <- "Unspecified"
  out
}

## Data ----------------------------------------------------------------------

# Both flows for one economy, from the staged Atlas panel plus an API top-up.
china_trade_panel <- function(repo_root,
                              iso = "CHN",
                              start_year = 1995L,
                              end_year = as.integer(format(Sys.Date(), "%Y")) - 1L,
                              allow_api = TRUE) {
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required to read the Atlas panel.")
  }

  path <- file.path(repo_root, config$raw_data_dir, EXPORT_SIMILARITY_ATLAS_FILE)
  if (!file.exists(path)) stop("Atlas HS92 4-digit panel not found: ", path)

  raw <- readr::read_csv(
    path,
    col_select = c("country_id", "country_iso3_code", "product_id",
                   "product_hs92_code", "year", "export_value", "import_value"),
    col_types = readr::cols(
      country_id = readr::col_integer(),
      country_iso3_code = readr::col_character(),
      product_id = readr::col_integer(),
      product_hs92_code = readr::col_character(),
      year = readr::col_integer(),
      export_value = readr::col_double(),
      import_value = readr::col_double()
    ),
    progress = FALSE
  )

  panel <- raw[raw$country_iso3_code == iso, , drop = FALSE]
  out <- data.frame(
    year = panel$year,
    code = panel$product_hs92_code,
    exports = panel$export_value,
    imports = panel$import_value,
    stringsAsFactors = FALSE
  )
  out <- out[out$year >= start_year & out$year <= end_year, , drop = FALSE]

  file_max <- suppressWarnings(max(panel$year, na.rm = TRUE))
  if (is.finite(file_max) && end_year > file_max && allow_api) {
    product_map <- stats::setNames(
      as.character(raw$product_hs92_code), as.character(raw$product_id)
    )
    product_map <- product_map[!duplicated(names(product_map))]
    country_id <- panel$country_id[1]

    for (y in seq.int(from = file_max + 1L, to = end_year)) {
      extra <- tryCatch(
        china_trade_api_year(y, country_id, product_map),
        error = function(e) {
          warning("Atlas API fetch failed for ", y, ": ", conditionMessage(e), call. = FALSE)
          NULL
        }
      )
      if (!is.null(extra) && nrow(extra) > 0) {
        message("  API top-up ", y, ": ", nrow(extra), " products")
        out <- rbind(out, extra)
      } else {
        message("  ", y, ": not yet released by the Atlas; stopping there")
        break
      }
    }
  }

  out[!is.na(out$code), , drop = FALSE]
}

china_trade_api_year <- function(year, country_id, product_map) {
  query <- sprintf(
    paste0("{ countryProductYear(countryId: %d, productClass: HS92, productLevel: 4, ",
           "yearMin: %d, yearMax: %d) { productId exportValue importValue } }"),
    country_id, year, year
  )
  d <- export_similarity_atlas_graphql(query)
  if (is.null(d)) return(NULL)

  data.frame(
    year = year,
    code = unname(product_map[as.character(sub("^product-HS92-", "", d$productId))]),
    exports = suppressWarnings(as.numeric(d$exportValue)),
    imports = suppressWarnings(as.numeric(d$importValue)),
    stringsAsFactors = FALSE
  )
}

# Year x sector shares of total exports and imports.
china_trade_composition <- function(panel) {
  panel$sector <- china_trade_assign_sector(panel$code)

  agg <- stats::aggregate(
    cbind(exports, imports) ~ year + sector, data = panel, FUN = sum, na.rm = TRUE
  )

  # Denominator is allocated trade, so shares are comparable across years.
  allocated <- panel[panel$sector != "Unspecified", , drop = FALSE]
  totals <- stats::aggregate(
    cbind(exports, imports) ~ year, data = allocated, FUN = sum, na.rm = TRUE
  )
  names(totals)[2:3] <- c("total_exports", "total_imports")

  gross <- stats::aggregate(
    cbind(exports, imports) ~ year, data = panel, FUN = sum, na.rm = TRUE
  )
  names(gross)[2:3] <- c("gross_exports", "gross_imports")

  out <- merge(merge(agg, totals, by = "year"), gross, by = "year")
  out$export_share <- 100 * out$exports / out$total_exports
  out$import_share <- 100 * out$imports / out$total_imports

  # Carry the measurement caveat in the data rather than only in a comment.
  out$unallocated_export_pct <- 100 * (1 - out$total_exports / out$gross_exports)
  out$unallocated_import_pct <- 100 * (1 - out$total_imports / out$gross_imports)

  levels_present <- CHINA_TRADE_SECTOR_ORDER[CHINA_TRADE_SECTOR_ORDER %in% out$sector]
  out$sector <- factor(out$sector, levels = c(levels_present, "Unspecified"))

  out[order(out$year, out$sector), , drop = FALSE]
}

# Sectors only, with the unallocated row removed; this is what the charts stack.
china_trade_allocated <- function(composition) {
  out <- composition[composition$sector != "Unspecified", , drop = FALSE]
  out$sector <- droplevels(out$sector)
  out
}

## Charts --------------------------------------------------------------------

# 14 visually distinct fills; recycling would give two sectors the same colour.
china_trade_palette <- function(n) {
  base <- c(
    "#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#b15928", "#a6cee3",
    "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6", "#dede00", "#8dd3c7", "#7f7f7f"
  )
  if (n > length(base)) {
    stop("china_trade_palette(): ", n, " sectors but only ", length(base), " distinct colours.")
  }
  base[seq_len(n)]
}

china_trade_plot_composition <- function(composition, flow = c("export", "import")) {
  flow <- match.arg(flow)
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for the composition chart.")
  }

  composition <- china_trade_allocated(composition)
  share_col <- paste0(flow, "_share")
  composition$share <- composition[[share_col]]
  sectors <- levels(composition$sector)

  years <- sort(unique(composition$year))
  flow_label <- if (flow == "export") "exports" else "imports"

  unalloc <- composition[[paste0("unallocated_", flow, "_pct")]]
  unalloc_note <- sprintf(
    "Shares are of allocated trade; unallocated (Atlas XXXX/9999) ran %.1f%%-%.1f%% of the gross total.",
    min(unalloc, na.rm = TRUE), max(unalloc, na.rm = TRUE)
  )

  ggplot2::ggplot(
    composition,
    ggplot2::aes(x = year, y = share, fill = sector)
  ) +
    ggplot2::geom_area(colour = "white", linewidth = 0.15) +
    ggplot2::scale_fill_manual(values = china_trade_palette(length(sectors)), breaks = sectors) +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_pretty(n = 8)(years), expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 100),
                                breaks = seq(0, 100, 20)) +
    ggplot2::labs(
      title = paste0("Sectoral composition of China's ", flow_label),
      subtitle = paste0(
        "Share of allocated goods ", flow_label, ", HS92 chapters grouped into sectors, ",
        min(years), "-", max(years)
      ),
      x = NULL, y = paste0("% of allocated ", flow_label), fill = NULL,
      caption = paste0(
        "Sectors are HS2 chapter groupings; road vehicles (HS 87) is shown separately.\n",
        unalloc_note,
        "\nSource: Harvard Growth Lab, Atlas of Economic Complexity (HS92, 4-digit)."
      )
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "right",
      legend.key.size = ggplot2::unit(0.8, "lines"),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(size = 8, hjust = 0, colour = "grey35"),
      panel.grid.minor = ggplot2::element_blank()
    )
}

# The sectors that moved most, exports and imports side by side.
china_trade_plot_shifts <- function(composition, from_year, to_year, top_n = 8) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for the shift chart.")
  }

  composition <- china_trade_allocated(composition)
  a <- composition[composition$year == from_year, ]
  b <- composition[composition$year == to_year, ]
  m <- merge(
    a[, c("sector", "export_share", "import_share")],
    b[, c("sector", "export_share", "import_share")],
    by = "sector", suffixes = c("_from", "_to")
  )
  m$Exports <- m$export_share_to - m$export_share_from
  m$Imports <- m$import_share_to - m$import_share_from

  long <- rbind(
    data.frame(sector = m$sector, flow = "Exports", change = m$Exports, stringsAsFactors = FALSE),
    data.frame(sector = m$sector, flow = "Imports", change = m$Imports, stringsAsFactors = FALSE)
  )

  keep <- utils::head(
    m$sector[order(-pmax(abs(m$Exports), abs(m$Imports)))], top_n
  )
  long <- long[long$sector %in% keep, ]
  long$sector <- factor(long$sector, levels = rev(as.character(keep)))

  ggplot2::ggplot(long, ggplot2::aes(x = change, y = sector, fill = flow)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.7), width = 0.65) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.4) +
    ggplot2::scale_fill_manual(values = c(Exports = "#1f78b4", Imports = "#e31a1c")) +
    ggplot2::labs(
      title = sprintf("Where China's trade composition shifted, %d to %d", from_year, to_year),
      subtitle = "Change in each sector's share of allocated goods trade, percentage points",
      x = "Change in share (pp)", y = NULL, fill = NULL,
      caption = paste(
        "Shares are of allocated trade, excluding the Atlas unallocated codes.",
        "\nSource: Harvard Growth Lab, Atlas of Economic Complexity (HS92, 4-digit)."
      )
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(size = 8, hjust = 0, colour = "grey35"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank()
    )
}

## Runner --------------------------------------------------------------------

run_china_trade_composition <- function(repo_root,
                                        start_year = 1995L,
                                        end_year = as.integer(format(Sys.Date(), "%Y")) - 1L,
                                        iso = "CHN",
                                        write_output = TRUE) {
  panel <- china_trade_panel(repo_root, iso = iso,
                             start_year = start_year, end_year = end_year)
  if (nrow(panel) == 0) stop("No Atlas rows for ", iso, " ", start_year, "-", end_year, ".")

  composition <- china_trade_composition(panel)
  years <- sort(unique(composition$year))
  message(
    iso, " composition: ", length(years), " years (", min(years), "-", max(years), "), ",
    nlevels(composition$sector), " sectors"
  )

  if (!write_output) return(invisible(composition))

  out_dir <- file.path(repo_root, config$processed_dir, "charts")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  stem <- sprintf("china_trade_composition_%d_%d", min(years), max(years))

  utils::write.csv(
    composition[, c("year", "sector", "exports", "imports",
                    "total_exports", "total_imports", "export_share", "import_share",
                    "unallocated_export_pct", "unallocated_import_pct")],
    file.path(out_dir, paste0(stem, ".csv")), row.names = FALSE, na = ""
  )

  save_png <- function(plot, suffix, width = 10, height = 6) {
    p <- file.path(out_dir, paste0(stem, "_", suffix, ".png"))
    ggplot2::ggsave(
      p, plot, width = width, height = height, dpi = 200,
      device = if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else NULL
    )
    message("Wrote ", p)
    p
  }

  save_png(china_trade_plot_composition(composition, "export"), "exports")
  save_png(china_trade_plot_composition(composition, "import"), "imports")
  save_png(
    china_trade_plot_shifts(composition, min(years), max(years)),
    "shifts", width = 9, height = 6
  )

  message("Wrote ", file.path(out_dir, paste0(stem, ".csv")))
  invisible(composition)
}

opsi_china_trade_run_directly <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  length(file_arg) > 0 &&
    identical(basename(sub("^--file=", "", file_arg[1])), "china_trade_composition.R")
}

if (opsi_china_trade_run_directly()) {
  invisible(run_china_trade_composition(
    repo_root = repo_root,
    start_year = as.integer(Sys.getenv("CTC_START_YEAR", "1995")),
    end_year = as.integer(Sys.getenv(
      "CTC_END_YEAR", as.character(as.integer(format(Sys.Date(), "%Y")) - 1L)
    )),
    iso = Sys.getenv("CTC_ISO", "CHN")
  ))
}
