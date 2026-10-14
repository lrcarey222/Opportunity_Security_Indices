# Export similarity with China, and its sectoral decomposition.
#
# Replicates the two-panel figure from the Federal Reserve FEDS Note "The Sectoral
# Evolution of China's Trade" (28 Feb 2025):
#
#   left   ESI between China and selected advanced economies, over time
#   right  change in each economy's sectoral ESI contributions between two years,
#          stacked to the total change
#
# Comparators here are Japan, Korea, the euro area and the United States.
#
# The decomposition is exact, not an approximation. ESI is a sum over products of
# min(share_a, share_b), so grouping those per-product terms by sector gives
# contributions that add back to the total index, and differencing them across two
# years attributes the change in the total to sectors.
#
# Run:
#   Rscript R/charts/china_esi_sectoral.R
#
# Environment:
#   CESI_FROM_YEAR  first year of the change decomposition (default 2010)
#   CESI_TO_YEAR    last year, also the end of the time series (default latest)
#   CESI_LEVEL      hs4 (default) or hs2/hs6 - see export_similarity.R on levels

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

source(file.path(repo_root, "R", "charts", "export_similarity.R"))

## Comparators ---------------------------------------------------------------

CESI_EURO_AREA <- c(
  "AUT", "BEL", "CYP", "DEU", "EST", "ESP", "FIN", "FRA", "GRC", "HRV",
  "IRL", "ITA", "LTU", "LUX", "LVA", "MLT", "NLD", "PRT", "SVK", "SVN"
)

# Bloc code -> label. "EUZ" is a synthetic reporter built by summing members.
CESI_COMPARATORS <- c(JPN = "Japan", KOR = "Korea", EUZ = "Euro area", USA = "United States")
CESI_BASE <- c(CHN = "China")

## SITC-section approximation ------------------------------------------------
#
# The Fed note groups by SITC section. The Atlas panel is HS92, and no official
# HS92->SITC concordance ships with this repo, so sections are approximated at the HS
# chapter level. The grouping is deliberately coarse - it only needs to separate the
# five buckets the figure uses - but it is an approximation, not a concordance, and
# chapter-level assignment cannot split chapters that straddle two SITC sections.

CESI_SITC_GROUPS <- list(
  `Sectors 0-4, 9: Primary & unclassified` = c(
    sprintf("%02d", 1:27), "41", "43", "44", "47", "50", "51", "52", "53"
  ),
  `Sector 5: Chemicals` = sprintf("%02d", 28:38),
  `Sector 6: Manufactured goods by material` = c(
    "39", "40", "45", "46", "48", "49",
    sprintf("%02d", 54:60), "63", sprintf("%02d", 65:71), sprintf("%02d", 72:83)
  ),
  `Sector 7: Machinery & transport equipment` = sprintf("%02d", 84:89),
  `Sector 8: Misc manufactured articles` = c(
    "42", "61", "62", "64", sprintf("%02d", 90:97)
  )
)

CESI_SITC_ORDER <- c(
  "Sectors 0-4, 9: Primary & unclassified",
  "Sector 5: Chemicals",
  "Sector 6: Manufactured goods by material",
  "Sector 7: Machinery & transport equipment",
  "Sector 8: Misc manufactured articles"
)

CESI_SITC_COLOURS <- c(
  "Sectors 0-4, 9: Primary & unclassified"    = "#1b6b3a",
  "Sector 5: Chemicals"                       = "#e8d58a",
  "Sector 6: Manufactured goods by material"  = "#a6a6a6",
  "Sector 7: Machinery & transport equipment" = "#4a72c4",
  "Sector 8: Misc manufactured articles"      = "#c0392b"
)

cesi_sector_lookup <- function() {
  chapters <- unlist(CESI_SITC_GROUPS, use.names = FALSE)
  dup <- chapters[duplicated(chapters)]
  if (length(dup) > 0) {
    stop("Chapter(s) in more than one SITC group: ", paste(unique(dup), collapse = ", "))
  }
  stats::setNames(rep(names(CESI_SITC_GROUPS), lengths(CESI_SITC_GROUPS)), chapters)
}

# code -> sector, for whatever product codes the level produced.
cesi_sector_of <- function(codes) {
  lookup <- cesi_sector_lookup()
  out <- unname(lookup[substr(codes, 1, 2)])
  out[is.na(out)] <- "Sectors 0-4, 9: Primary & unclassified"
  stats::setNames(out, codes)
}

## Data ----------------------------------------------------------------------

# China plus the comparators, with euro area members collapsed into one reporter.
cesi_trade_panel <- function(repo_root, start_year, end_year, level = "hs4") {
  isos <- c(names(CESI_BASE), setdiff(names(CESI_COMPARATORS), "EUZ"), CESI_EURO_AREA)

  trade <- export_similarity_atlas_trade(
    repo_root, start_year = start_year, end_year = end_year, isos = unique(isos)
  )

  # Euro area as a bloc. The panel carries no bilateral detail, so members' gross
  # exports are summed and intra-euro-area trade cannot be netted out; see the note
  # in the chart caption.
  ea <- trade[trade$iso %in% CESI_EURO_AREA, , drop = FALSE]
  if (nrow(ea) > 0) {
    ea_agg <- stats::aggregate(value ~ year + code, data = ea, FUN = sum, na.rm = TRUE)
    ea_agg$iso <- "EUZ"
    trade <- rbind(
      trade[!trade$iso %in% CESI_EURO_AREA, , drop = FALSE],
      ea_agg[, c("year", "iso", "code", "value")]
    )
  }

  export_similarity_aggregate(trade, level = level)
}

# ESI series and sector contributions for China against each comparator.
cesi_compute <- function(trade, from_year, to_year) {
  years <- sort(unique(trade$year))
  sector_of <- cesi_sector_of(unique(trade$code))

  series <- list()
  contributions <- list()

  for (iso in names(CESI_COMPARATORS)) {
    for (y in years) {
      res <- export_similarity_one(trade, y, "CHN", iso, min_products = 5)
      series[[length(series) + 1L]] <- data.frame(
        year = y, iso = iso, name = unname(CESI_COMPARATORS[iso]),
        esi = res$esi, products_compared = res$products_compared,
        stringsAsFactors = FALSE
      )
    }
    for (y in c(from_year, to_year)) {
      cont <- export_similarity_contributions(trade, y, "CHN", iso, sector_of)
      if (is.null(cont)) next
      cont$iso <- iso
      cont$name <- unname(CESI_COMPARATORS[iso])
      contributions[[length(contributions) + 1L]] <- cont
    }
  }

  list(
    series = do.call(rbind, series),
    contributions = do.call(rbind, contributions)
  )
}

# Sector contributions differenced between the two years.
cesi_changes <- function(contributions, from_year, to_year) {
  a <- contributions[contributions$year == from_year, c("iso", "name", "sector", "contribution")]
  b <- contributions[contributions$year == to_year, c("iso", "name", "sector", "contribution")]
  m <- merge(a, b, by = c("iso", "name", "sector"), suffixes = c("_from", "_to"), all = TRUE)
  m$contribution_from[is.na(m$contribution_from)] <- 0
  m$contribution_to[is.na(m$contribution_to)] <- 0
  m$change <- m$contribution_to - m$contribution_from

  m$sector <- factor(m$sector, levels = CESI_SITC_ORDER)
  m$name <- factor(m$name, levels = unname(CESI_COMPARATORS))
  m[order(m$name, m$sector), , drop = FALSE]
}

## Charts --------------------------------------------------------------------

cesi_plot_series <- function(series, from_year, to_year) {
  series <- series[!is.na(series$esi), , drop = FALSE]
  series$name <- factor(series$name, levels = unname(CESI_COMPARATORS))

  ggplot2::ggplot(
    series, ggplot2::aes(x = year, y = esi, colour = name, linetype = name)
  ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::scale_colour_manual(values = c(
      Japan = "#000000", Korea = "#8e44ad",
      `Euro area` = "#2c62c4", `United States` = "#1e8449"
    )) +
    ggplot2::scale_linetype_manual(values = c(
      Japan = "dashed", Korea = "dotted",
      `Euro area` = "dotdash", `United States` = "solid"
    )) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 6)) +
    ggplot2::labs(
      title = "Export Similarity Index between\nChina and selected economies",
      x = NULL, y = "Percent", colour = NULL, linetype = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      # legend.position.inside replaced numeric legend.position in ggplot2 3.5.0.
      legend.position = "inside",
      legend.position.inside = c(0.02, 0.98),
      legend.justification = c(0, 1),
      legend.background = ggplot2::element_rect(fill = "white", colour = NA),
      legend.key.width = ggplot2::unit(1.6, "lines"),
      plot.title = ggplot2::element_text(face = "bold", size = 12),
      panel.grid.minor = ggplot2::element_blank()
    )
}

cesi_plot_changes <- function(changes, from_year, to_year) {
  totals <- stats::aggregate(change ~ name, data = changes, FUN = sum)

  ggplot2::ggplot(changes, ggplot2::aes(x = name, y = change, fill = sector)) +
    ggplot2::geom_col(width = 0.62) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.5, colour = "black") +
    ggplot2::geom_point(
      data = totals, ggplot2::aes(x = name, y = change),
      inherit.aes = FALSE, shape = 21, size = 4.2, fill = "white", stroke = 1.1
    ) +
    ggplot2::scale_fill_manual(values = CESI_SITC_COLOURS, drop = FALSE) +
    ggplot2::labs(
      title = sprintf("Change in sectoral ESI between\n%d and %d", from_year, to_year),
      x = NULL, y = "Percentage points", fill = NULL
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1)) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 8),
      legend.key.size = ggplot2::unit(0.8, "lines"),
      plot.title = ggplot2::element_text(face = "bold", size = 12),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank()
    )
}

## Runner --------------------------------------------------------------------

run_china_esi_sectoral <- function(repo_root,
                                   start_year = 2010L,
                                   from_year = 2010L,
                                   to_year = as.integer(format(Sys.Date(), "%Y")) - 1L,
                                   level = "hs4",
                                   write_output = TRUE) {
  trade <- cesi_trade_panel(repo_root, start_year, to_year, level = level)
  available <- sort(unique(trade$year))
  if (!to_year %in% available) {
    to_year <- max(available)
    message("Requested end year unavailable; using ", to_year)
  }
  message("Panel: ", nrow(trade), " rows | ", length(unique(trade$iso)), " reporters | ",
          length(unique(trade$code)), " products (", level, ") | ",
          min(available), "-", max(available))

  out <- cesi_compute(trade, from_year, to_year)
  changes <- cesi_changes(out$contributions, from_year, to_year)

  # The decomposition must reconstruct the index, not merely resemble it.
  check <- merge(
    stats::aggregate(contribution ~ iso + year, data = out$contributions, FUN = sum),
    out$series[, c("iso", "year", "esi")], by = c("iso", "year")
  )
  worst <- max(abs(check$contribution - check$esi), na.rm = TRUE)
  message(sprintf("Decomposition check: max |sum(sectors) - ESI| = %.2e", worst))
  if (worst > 1e-8) warning("Sector contributions do not sum to the index.", call. = FALSE)

  if (!write_output) return(invisible(list(series = out$series, changes = changes)))

  out_dir <- file.path(repo_root, config$processed_dir, "charts")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  stem <- sprintf("china_esi_sectoral_%s_%d_%d", level, from_year, to_year)

  utils::write.csv(out$series, file.path(out_dir, paste0(stem, "_series.csv")),
                   row.names = FALSE, na = "")
  utils::write.csv(changes, file.path(out_dir, paste0(stem, "_sector_changes.csv")),
                   row.names = FALSE, na = "")

  left <- cesi_plot_series(out$series, from_year, to_year)
  right <- cesi_plot_changes(changes, from_year, to_year)

  caption <- paste(
    sprintf(
      "Finger-Kreinin index over all traded products at %s level; sector contributions sum to the index.",
      toupper(level)
    ),
    "\nSITC sections are approximated from HS92 chapters. Euro area is the sum of 20 members' gross exports,",
    "\nso intra-euro-area trade is included. Source: Harvard Growth Lab, Atlas of Economic Complexity."
  )

  combined <- cowplot::plot_grid(left, right, nrow = 1, rel_widths = c(1, 1.05))
  combined <- cowplot::add_sub(combined, caption, x = 0, hjust = 0, size = 8, colour = "grey35")
  combined <- cowplot::ggdraw(combined)

  png_path <- file.path(out_dir, paste0(stem, ".png"))
  ggplot2::ggsave(
    png_path, combined, width = 12, height = 6.2, dpi = 200,
    device = if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else NULL
  )

  message("Wrote ", png_path)
  message("Wrote ", file.path(out_dir, paste0(stem, "_series.csv")))
  message("Wrote ", file.path(out_dir, paste0(stem, "_sector_changes.csv")))

  invisible(list(series = out$series, changes = changes, plot = combined, png = png_path))
}

opsi_cesi_run_directly <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  length(file_arg) > 0 &&
    identical(basename(sub("^--file=", "", file_arg[1])), "china_esi_sectoral.R")
}

if (opsi_cesi_run_directly()) {
  from_year <- as.integer(Sys.getenv("CESI_FROM_YEAR", "2010"))
  to_year <- as.integer(Sys.getenv(
    "CESI_TO_YEAR", as.character(as.integer(format(Sys.Date(), "%Y")) - 1L)
  ))
  invisible(run_china_esi_sectoral(
    repo_root = repo_root,
    start_year = from_year, from_year = from_year, to_year = to_year,
    level = tolower(Sys.getenv("CESI_LEVEL", "hs4"))
  ))
}

esi<-read.csv('C:/Users/LCarey/OneDrive - RMI/Documents/GitHub/Opportunity_Security_Indices/data/processed/charts/china_esi_sectoral_hs4_2010_2024_sector_changes.csv')

esi_sector_wide<-esi %>%
  select(name,sector,change) %>%
  pivot_wider(names_from="name",values_from="change")

write.csv(esi_sector_wide,"C:/Users/LCarey/OneDrive - RMI/Documents/GitHub/Opportunity_Security_Indices/data/processed/charts/esi_sector_wide.csv")
