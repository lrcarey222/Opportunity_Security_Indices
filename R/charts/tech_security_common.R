# Shared machinery for the per-technology security chart scripts.
#
# japan_nuclear_security.R and japan_grid_components_security.R ask the same three
# questions of two different technologies - where is the economic opportunity, where is
# the security risk, and is policy moving - so the parts that do not depend on which
# technology it is live here: the commodity universe, the two trade panels, the
# concentration maths, the policy series, and the house look.
#
# Source it after the bootstrap has run; it expects `repo_root` and `config` to exist.
#
# Not sourced by japan_critical_minerals_security.R, which predates it and carries its
# own copies. Migrating that script onto these helpers would remove the overlap but
# would also churn a file that is already working, so it is left alone.

## Look -----------------------------------------------------------------------

# The repo's house palette (scripts/20_build_indices.R).
TS_PALETTE <- c(
  cyan = "#0BD0D9", blue = "#0989B1", navy = "#003A61",
  yellow = "#FFCA08", orange = "#F8931D", green = "#548538", grey = "#7F7F7F"
)

TS_FOCUS_COLOUR <- unname(TS_PALETTE["orange"])
TS_CONTEXT_COLOUR <- "grey55"

TS_THEME_BASE_SIZE <- 11

ts_theme <- function(base_size = TS_THEME_BASE_SIZE) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 2),
      plot.subtitle = ggplot2::element_text(colour = "grey30", size = base_size - 1),
      plot.caption = ggplot2::element_text(colour = "grey40", size = base_size - 3, hjust = 0),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = base_size - 1),
      legend.key.size = ggplot2::unit(0.9, "lines")
    )
}

## Commodity universe ---------------------------------------------------------

# Six-character HS code from whatever type the CSV reader inferred.
#
# read.csv types a code column as integer when every value in it is numeric, which strips
# a leading zero. Padding is done by hand rather than with formatC's "0" flag, which
# silently pads with spaces once its input is a character vector.
ts_hs6 <- function(x) {
  s <- as.character(x)
  short <- !is.na(s) & nchar(s) < 6
  s[short] <- paste0(strrep("0", 6 - nchar(s[short])), s[short])
  s
}

# HS6 commodity codes for one technology, from the repo's energy-tech crosswalk.
#
# Two properties of that file matter and are handled here rather than at each call site.
#
# It carries exact-duplicate rows - 482 rows collapse to 466 - so anything that joins it
# to a trade panel without deduplicating counts those commodities twice. The dedupe is
# checked, not assumed: if a code ever maps to two different sub-sectors within one
# technology the split would be arbitrary, so that stops the run.
#
# And its technology assignment is many-to-many by design: a static converter (850440)
# is grid equipment, but it is also solar, wind and EV equipment. `exclusive` flags the
# codes that belong to this technology and no other, which is the only subset that can
# be described as the technology's own trade. For nuclear that flag isolates the fuel
# cycle and reactor hardware from the steam plant it shares with coal and gas.
ts_tech_codes <- function(repo_root, tech, exclude_codes = character()) {
  path <- file.path(repo_root, config$raw_data_dir, "hs6_categories_with_essential.csv")
  if (!file.exists(path)) {
    stop("Energy-tech HS6 crosswalk not found: ", path)
  }
  cw <- utils::read.csv(path, stringsAsFactors = FALSE)

  required <- c("Technology", "Value.Chain", "Sub.Sector", "HS6")
  missing <- setdiff(required, names(cw))
  if (length(missing) > 0) {
    stop("HS6 crosswalk is missing column(s): ", paste(missing, collapse = ", "))
  }
  cw$hs6 <- ts_hs6(cw$HS6)

  tech_pairs <- unique(cw[, c("Technology", "hs6")])
  shared <- table(tech_pairs$hs6)

  out <- unique(cw[cw$Technology == tech, c("hs6", "Sub.Sector", "Value.Chain")])
  if (nrow(out) == 0) {
    stop("No HS6 codes in the crosswalk for technology '", tech, "'.")
  }

  clash <- names(which(table(out$hs6) > 1))
  if (length(clash) > 0) {
    stop(
      "HS6 code(s) mapped to more than one sub-sector within '", tech, "': ",
      paste(clash, collapse = ", ")
    )
  }

  out <- out[!out$hs6 %in% exclude_codes, , drop = FALSE]
  out$exclusive <- unname(shared[out$hs6]) == 1L
  names(out)[names(out) == "Sub.Sector"] <- "sub_sector"
  names(out)[names(out) == "Value.Chain"] <- "value_chain"
  out[order(out$sub_sector, out$hs6), , drop = FALSE]
}

## Trade panels ---------------------------------------------------------------

# Cross-sectional panel: every reporter, both flows, one year.
#
# comtrade_energy_trade.csv is the pipeline's own energy-tech pull. Partner is World, so
# it carries no bilateral detail - there is nothing in this repo that does, for these
# commodities - which is what forces the concentration measures below to be built from
# global export structure rather than from realised supplier mixes.
ts_comtrade_energy <- function(repo_root) {
  path <- file.path(repo_root, config$raw_data_dir, "comtrade_energy_trade.csv")
  if (!file.exists(path)) {
    stop("Comtrade energy-tech pull not found: ", path,
         "\nRun scripts/05_ingest_sources.R first.")
  }
  raw <- utils::read.csv(path, stringsAsFactors = FALSE)

  required <- c("reporter_iso", "flow_desc", "cmd_code", "cmd_desc", "primary_value")
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0) {
    stop(basename(path), " is missing column(s): ", paste(missing, collapse = ", "))
  }

  year_col <- intersect(c("ref_year", "period", "year", "Year"), names(raw))
  out <- data.frame(
    year = if (length(year_col) > 0) suppressWarnings(as.integer(raw[[year_col[1]]])) else NA_integer_,
    reporter_iso = raw$reporter_iso,
    flow = raw$flow_desc,
    hs6 = ts_hs6(raw$cmd_code),
    cmd_desc = raw$cmd_desc,
    value = suppressWarnings(as.numeric(raw$primary_value)),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$value) & out$value > 0 & out$flow %in% c("Import", "Export"), , drop = FALSE]
  out$flow <- ifelse(out$flow == "Import", "Imports", "Exports")
  out
}

# Long panel: every reporter, both flows, 1995 to the Atlas release year.
#
# The Atlas file is ~900MB, so only the five columns the charts need are read; that takes
# a few seconds rather than a few minutes. Returns NULL when the file is not staged, so a
# tree without it still produces every other figure.
#
# The Atlas is HS92. The crosswalk is built on a recent HS revision, so codes introduced
# after 1992 simply are not there - electrical steel and the current steam-turbine
# headings among them. The count of matched codes is reported by the caller rather than
# silently absorbed, because a series built on 54 of 68 codes is not the same series.
ts_atlas_trade <- function(repo_root, codes, quiet = FALSE) {
  path <- file.path(repo_root, config$raw_data_dir, "hs92_country_product_year_6.csv")
  if (!file.exists(path)) {
    message("Atlas HS6 panel not staged (", basename(path), "); skipping the long series.")
    return(NULL)
  }

  if (!quiet) message("Reading the Atlas HS6 panel (large file, a few seconds)...")
  atlas <- readr::read_csv(
    path,
    col_select = c("country_iso3_code", "product_hs92_code", "year", "export_value", "import_value"),
    col_types = readr::cols(
      country_iso3_code = readr::col_character(),
      product_hs92_code = readr::col_character(),
      year = readr::col_integer(),
      export_value = readr::col_double(),
      import_value = readr::col_double()
    ),
    progress = FALSE
  )

  atlas$hs6 <- ts_hs6(atlas$product_hs92_code)
  out <- as.data.frame(atlas[atlas$hs6 %in% codes, , drop = FALSE])
  out$product_hs92_code <- NULL
  names(out)[names(out) == "country_iso3_code"] <- "reporter_iso"
  out
}

## Concentration --------------------------------------------------------------

# How few countries export each commodity.
#
# An HHI over reporters' shares of world exports, per commodity. Shares are gross, so a
# re-exporter is credited as a source - which matters here: the Netherlands ranks as a
# major uranium exporter because Urenco enriches there, and Kazakh material moves through
# several hands before it is a fuel assembly.
ts_commodity_concentration <- function(exports) {
  by_code <- stats::aggregate(value ~ hs6 + reporter_iso, data = exports, FUN = sum)
  world <- stats::aggregate(value ~ hs6, data = by_code, FUN = sum)
  names(world)[2] <- "world_exports"

  by_code <- merge(by_code, world, by = "hs6")
  by_code$share <- by_code$value / by_code$world_exports

  agg <- do.call(rbind, lapply(split(by_code, by_code$hs6), function(d) {
    d <- d[order(-d$share), , drop = FALSE]
    data.frame(
      hs6 = d$hs6[[1]],
      world_exports = d$world_exports[[1]],
      supplier_hhi = sum(d$share^2),
      top_supplier_share = d$share[[1]],
      top_supplier_iso = d$reporter_iso[[1]],
      top3_share = sum(utils::head(d$share, 3)),
      n_suppliers = nrow(d),
      stringsAsFactors = FALSE
    )
  }))
  rownames(agg) <- NULL
  agg[order(-agg$world_exports), , drop = FALSE]
}

# One exposure number per importer: the concentration of the basket it actually buys.
#
# Each commodity's export-market HHI weighted by the importer's own spend on it. This
# measures how thin the markets an importer depends on are; it is not that importer's
# realised supplier mix, which the World-partner pull cannot show.
ts_supplier_exposure <- function(imports, concentration) {
  imp <- merge(
    imports[, c("reporter_iso", "hs6", "value")],
    concentration[, c("hs6", "supplier_hhi", "top_supplier_share")],
    by = "hs6"
  )
  if (nrow(imp) == 0) {
    stop("No importer rows survived the join to commodity concentration.")
  }

  out <- do.call(rbind, lapply(split(imp, imp$reporter_iso), function(d) {
    data.frame(
      reporter_iso = d$reporter_iso[[1]],
      import_bill_usd = sum(d$value),
      weighted_supplier_hhi = stats::weighted.mean(d$supplier_hhi, d$value),
      weighted_top_supplier_share = stats::weighted.mean(d$top_supplier_share, d$value),
      n_commodities = nrow(d),
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  out[order(-out$import_bill_usd), , drop = FALSE]
}

## Policy ---------------------------------------------------------------------

# Peers for the policy figures. NIPO covers 22 reporters; these are the advanced
# economies that run comparable programmes in both technologies.
TS_POLICY_PEERS <- c(
  JPN = "Japan", KOR = "Korea", DEU = "Germany",
  FRA = "France", USA = "United States"
)

ts_policy_colours <- function(focus_name) {
  base <- c(
    Japan = unname(TS_PALETTE["orange"]),
    Korea = unname(TS_PALETTE["blue"]),
    Germany = unname(TS_PALETTE["green"]),
    France = unname(TS_PALETTE["cyan"]),
    `United States` = unname(TS_PALETTE["grey"])
  )
  base[focus_name] <- TS_FOCUS_COLOUR
  base
}

# Active domestic interventions per year, from the NIPO annual panel.
#
# n_policies_window counts distinct policies active in a year, so the series is a stock,
# not a flow of announcements: it falls only as policies expire or are removed. It is a
# count rather than one of the intervention-strength scores deliberately - counts compare
# across countries and supply-chain stages without depending on the strength scaling,
# which is itself under revision.
ts_policy_activity <- function(repo_root, tech, peers = TS_POLICY_PEERS, from_year = 2015L) {
  path <- file.path(repo_root, config$processed_dir, "nipo_tech_year.rds")
  if (!file.exists(path)) {
    stop("NIPO annual panel not found: ", path, "\nRun scripts/10_build_themes.R first.")
  }
  panel <- as.data.frame(readRDS(path))

  d <- panel[panel$tech == tech & panel$iso3 %in% names(peers), , drop = FALSE]
  if (nrow(d) == 0) {
    stop("No NIPO rows for tech '", tech, "' and the peer set.")
  }

  # The panel runs into the current calendar year, which is partial - GTA keeps recording
  # into it - so the series stops at the last complete year.
  last_complete <- min(
    max(d$announce_year, na.rm = TRUE),
    as.integer(format(Sys.Date(), "%Y")) - 1L
  )
  d <- d[d$announce_year >= from_year & d$announce_year <= last_complete, , drop = FALSE]

  out <- data.frame(
    iso3 = d$iso3,
    name = unname(peers[d$iso3]),
    supply_chain = d$supply_chain,
    year = as.integer(d$announce_year),
    n_active_policies = as.integer(d$n_policies_window),
    stringsAsFactors = FALSE
  )
  out$name <- factor(out$name, levels = unname(peers))
  out$supply_chain <- factor(out$supply_chain, levels = c("Upstream", "Midstream"))

  # A peer with no policy in a year is a zero, not a gap; without this the lines jump
  # over the 2020-21 trough instead of showing it.
  grid <- expand.grid(
    iso3 = names(peers), year = seq(from_year, last_complete),
    supply_chain = levels(out$supply_chain), stringsAsFactors = FALSE
  )
  grid$name <- factor(unname(peers[grid$iso3]), levels = unname(peers))
  grid$supply_chain <- factor(grid$supply_chain, levels = levels(out$supply_chain))
  out <- merge(grid, out, by = c("iso3", "name", "year", "supply_chain"), all.x = TRUE)
  out$n_active_policies[is.na(out$n_active_policies)] <- 0L

  attr(out, "last_complete_year") <- last_complete
  out[order(out$supply_chain, out$name, out$year), , drop = FALSE]
}

# Shared policy figure: the same chart for every technology, with the focal country's
# latest count printed on each panel because the free y scales otherwise hide the level
# gap between the two supply-chain stages, which is usually the point.
ts_plot_policy <- function(policy, focus_name, tech_label, stage_labels) {
  d <- policy
  levels(d$supply_chain) <- stage_labels

  focal <- d[d$name == focus_name, , drop = FALSE]
  ends <- do.call(rbind, lapply(split(focal, focal$supply_chain, drop = TRUE), function(x) {
    x[which.max(x$year), , drop = FALSE]
  }))

  ggplot2::ggplot(d, ggplot2::aes(x = year, y = n_active_policies, colour = name)) +
    ggplot2::geom_line(ggplot2::aes(linewidth = name == focus_name)) +
    ggplot2::geom_point(data = focal, size = 1.5) +
    ggplot2::geom_label(
      data = ends,
      ggplot2::aes(label = sprintf("%s: %d", focus_name, n_active_policies)),
      hjust = 1.05, vjust = -0.45, size = 3, fontface = "bold",
      label.size = 0, fill = "white", show.legend = FALSE
    ) +
    ggplot2::facet_wrap(~supply_chain, nrow = 1, scales = "free_y") +
    ggplot2::scale_colour_manual(values = ts_policy_colours(focus_name)) +
    ggplot2::scale_linewidth_manual(values = c(`TRUE` = 1.3, `FALSE` = 0.6), guide = "none") +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 5)) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(
      subtitle = sprintf(
        "Distinct domestic industrial-policy interventions active in each year, %s supply chain",
        tech_label
      ),
      x = NULL, y = "Active interventions", colour = NULL
    ) +
    ts_theme() +
    ggplot2::theme(legend.position = "bottom")
}

ts_policy_caption <- function(policy) {
  sprintf(
    paste0(
      "Source: RMI New Industrial Policy Observatory, built on Global Trade Alert. Counts distinct domestic interventions active in\n",
      "each year within the index's rolling window, so the level falls only as policies expire. Series stops at %d, the last complete year."
    ),
    attr(policy, "last_complete_year")
  )
}

## Shared risk figure ---------------------------------------------------------

# Importer exposure scatter: spend against the thinness of the markets that supply it.
ts_plot_exposure <- function(by_importer, focus_iso, top_n, trade_year, title, subtitle) {
  d <- utils::head(by_importer, top_n)
  d$bill_bn <- d$import_bill_usd / 1e9
  d$is_focus <- d$reporter_iso == focus_iso

  p <- ggplot2::ggplot(d, ggplot2::aes(x = bill_bn, y = weighted_supplier_hhi)) +
    ggplot2::geom_hline(
      yintercept = stats::median(d$weighted_supplier_hhi), linetype = "dashed",
      colour = "grey60", linewidth = 0.4
    ) +
    ggplot2::geom_point(ggplot2::aes(colour = is_focus, size = is_focus)) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = TS_FOCUS_COLOUR, `FALSE` = TS_CONTEXT_COLOUR)) +
    ggplot2::scale_size_manual(values = c(`TRUE` = 4.2, `FALSE` = 2.4)) +
    ggplot2::scale_x_log10(
      labels = scales::label_dollar(suffix = "bn", accuracy = 0.1),
      expand = ggplot2::expansion(mult = 0.09)
    ) +
    # Precision from the spread, not a fixed two decimals: grid HHIs sit between 0.095
    # and 0.125, where rounding to 0.01 prints "0.12" against three different gridlines.
    ggplot2::scale_y_continuous(
      labels = scales::label_number(
        accuracy = if (diff(range(d$weighted_supplier_hhi)) < 0.06) 0.001 else 0.01
      )
    ) +
    ggplot2::labs(
      title = title, subtitle = subtitle,
      x = sprintf("%d imports (log scale)", trade_year),
      y = "Import-weighted supplier HHI"
    ) +
    ts_theme() +
    ggplot2::theme(legend.position = "none")

  if (requireNamespace("ggrepel", quietly = TRUE)) {
    p + ggrepel::geom_text_repel(
      ggplot2::aes(label = reporter_iso, colour = is_focus,
                   fontface = ifelse(is_focus, "bold", "plain")),
      size = 3, seed = 42, min.segment.length = 0.2, segment.colour = "grey70",
      max.overlaps = Inf, show.legend = FALSE
    )
  } else {
    p + ggplot2::geom_text(
      ggplot2::aes(label = reporter_iso, colour = is_focus), size = 3,
      vjust = -0.9, show.legend = FALSE
    )
  }
}

ts_exposure_caption <- function(trade_year) {
  paste0(
    sprintf("Source: UN Comtrade %d (energy-tech HS6 pull), reporter against partner World.\n", trade_year),
    "Supplier HHI is computed over reporters' shares of world exports of each commodity, then weighted by the importer's own spend on it:\n",
    "it measures how thin the markets an importer buys from are, not that importer's realised supplier mix. Exporter shares are gross, so\n",
    "material that is re-exported or processed en route is credited to the last shipper."
  )
}

## Output ---------------------------------------------------------------------

ts_write <- function(obj, out_dir, stem) {
  path <- file.path(out_dir, paste0(stem, ".csv"))
  utils::write.csv(obj, path, row.names = FALSE, na = "")
  message("Wrote ", path)
  invisible(path)
}

ts_save <- function(plot, out_dir, stem, width, height, caption) {
  framed <- plot + ggplot2::labs(caption = caption)
  path <- file.path(out_dir, paste0(stem, ".png"))
  ggplot2::ggsave(
    path, framed, width = width, height = height, dpi = 200, bg = "white",
    device = if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else NULL
  )
  message("Wrote ", path)
  invisible(path)
}

ts_out_dir <- function(repo_root) {
  out_dir <- file.path(repo_root, config$processed_dir, "charts")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  out_dir
}

ts_focus_name <- function(focus_iso) {
  nm <- suppressWarnings(
    countrycode::countrycode(focus_iso, "iso3c", "country.name", warn = FALSE)
  )
  if (is.na(nm)) focus_iso else nm
}
