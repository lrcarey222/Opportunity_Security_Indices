# Japan's nuclear economic opportunity, supply security, and policy momentum.
#
# The argument in one line: Japan's nuclear industrial base has been shrinking into a
# growing world market since Fukushima, its remaining export strength is in the steam
# plant it shares with fossil generation rather than in anything nuclear-specific, the
# fuel cycle it depends on is one of the most concentrated markets in energy trade, and
# both money and policy have only turned back up in the last three years.
#
#   fig 1  position       Who exports nuclear technology, split into the fuel cycle,
#                         reactor hardware, and the shared steam plant
#   fig 2  chokepoints    Concentration of every nuclear commodity against its size,
#                         separating nuclear-specific codes from shared ones
#   fig 3  exposure       Import-bill-weighted supplier concentration, Japan against
#                         the other large importers
#   fig 4  momentum       Japan's nuclear exports and world share since 1995, beside
#                         annual nuclear investment since 2018
#   fig 5  policy         Active domestic interventions, Japan against four peers
#
# A note on what "nuclear" means here. The repo's HS6 crosswalk puts 18 commodities under
# Nuclear, but only seven of them - uranium, spent fuel, reactors, isotopic separation
# machinery, fuel elements, reactor parts - are nuclear and nothing else. The other
# eleven are boilers, condensers and steam turbines, which a coal or geothermal plant
# buys too, and which the crosswalk also files under those technologies. Every figure
# keeps the two apart rather than reporting one "nuclear trade" number that is 30% steam
# plant. See the `exclusive` column in the CSVs.
#
# Run:
#   Rscript R/charts/japan_nuclear_security.R
#
# Environment:
#   JNS_FOCUS_ISO     focal reporter ISO3 (default JPN)
#   JNS_POLICY_FROM   first announcement year on figure 5 (default 2015)
#   JNS_TREND_FROM    first year of the long trade series on figure 4 (default 1995)
#   JNS_TOP_IMPORTERS how many importers to plot on figure 3 (default 20)

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

source(file.path(repo_root, "R", "charts", "tech_security_common.R"))

## Constants -----------------------------------------------------------------

JNS_TECH <- "Nuclear"

# Sub-sectors, outermost first: what only a reactor needs, then the reactor itself, then
# the power island any thermal plant needs.
JNS_SUBSECTOR_LEVELS <- c(
  "Nuclear Fuel Cycle", "Reactor Components", "Steam Cycle & Balance of Plant"
)

JNS_SUBSECTOR_LABELS <- c(
  "Nuclear Fuel Cycle" = "Fuel cycle",
  "Reactor Components" = "Reactor hardware",
  "Steam Cycle & Balance of Plant" = "Steam plant (shared with fossil)"
)

JNS_SUBSECTOR_COLOURS <- c(
  "Fuel cycle" = unname(TS_PALETTE["navy"]),
  "Reactor hardware" = unname(TS_PALETTE["blue"]),
  "Steam plant (shared with fossil)" = "#C9CFD4"
)

jns_label_subsector <- function(x) {
  out <- unname(JNS_SUBSECTOR_LABELS[x])
  out[is.na(out)] <- x[is.na(out)]
  factor(out, levels = unname(JNS_SUBSECTOR_LABELS))
}

## Data ----------------------------------------------------------------------

# Nuclear commodity trade for one year, tagged with sub-sector and exclusivity.
jns_trade <- function(repo_root) {
  codes <- ts_tech_codes(repo_root, JNS_TECH)
  panel <- ts_comtrade_energy(repo_root)

  trade <- merge(panel, codes, by = "hs6")
  if (nrow(trade) == 0) {
    stop("No nuclear commodities found in the Comtrade energy-tech pull.")
  }
  trade$sub_sector <- factor(trade$sub_sector, levels = JNS_SUBSECTOR_LEVELS)
  if (any(is.na(trade$sub_sector))) {
    stop("Unexpected nuclear sub-sector(s): ",
         paste(setdiff(unique(merge(panel, codes, by = "hs6")$sub_sector),
                       JNS_SUBSECTOR_LEVELS), collapse = ", "))
  }

  missing <- setdiff(codes$hs6, unique(trade$hs6))
  if (length(missing) > 0) {
    message("Nuclear: ", length(missing), " of ", nrow(codes),
            " crosswalk codes absent from the Comtrade pull (",
            paste(missing, collapse = ", "), ").")
  }
  attr(trade, "codes") <- codes
  trade
}

# Figure 1: top exporters, with each one's exports split by sub-sector.
jns_fig1_data <- function(trade, focus_iso, top_n = 12L) {
  exports <- trade[trade$flow == "Exports", , drop = FALSE]
  world <- sum(exports$value)

  by_reporter <- stats::aggregate(value ~ reporter_iso, data = exports, FUN = sum)
  by_reporter <- by_reporter[order(-by_reporter$value), , drop = FALSE]
  by_reporter$share <- by_reporter$value / world
  by_reporter$rank <- seq_len(nrow(by_reporter))

  keep <- utils::head(by_reporter$reporter_iso, top_n)
  # The focal country is always on the chart, even when it does not make the cut.
  keep <- unique(c(keep, focus_iso))

  d <- exports[exports$reporter_iso %in% keep, , drop = FALSE]
  agg <- stats::aggregate(value ~ reporter_iso + sub_sector, data = d, FUN = sum)
  agg$share_of_world <- agg$value / world
  agg <- merge(agg, by_reporter[, c("reporter_iso", "rank")], by = "reporter_iso")

  agg$sub_sector <- jns_label_subsector(as.character(agg$sub_sector))
  # Ascending levels so the largest exporter sits at the top of a horizontal bar chart.
  shown <- by_reporter[by_reporter$reporter_iso %in% keep, , drop = FALSE]
  agg$reporter_iso <- factor(
    agg$reporter_iso, levels = shown$reporter_iso[order(shown$value)]
  )
  attr(agg, "world_exports") <- world
  attr(agg, "focus_rank") <- by_reporter$rank[by_reporter$reporter_iso == focus_iso]
  agg[order(agg$reporter_iso, agg$sub_sector), , drop = FALSE]
}

# Figure 2: concentration against size, per commodity.
jns_fig2_data <- function(trade) {
  exports <- trade[trade$flow == "Exports", , drop = FALSE]
  conc <- ts_commodity_concentration(exports)

  meta <- unique(trade[, c("hs6", "sub_sector", "exclusive")])
  desc <- unique(trade[, c("hs6", "cmd_desc")])
  desc <- desc[!duplicated(desc$hs6), , drop = FALSE]

  out <- merge(merge(conc, meta, by = "hs6"), desc, by = "hs6")
  out$kind <- ifelse(out$exclusive, "Nuclear-specific", "Shared with other thermal plant")
  out$label <- jns_short_label(out$hs6, out$cmd_desc)
  out[order(-out$world_exports), , drop = FALSE]
}

# Commodity names short enough to sit next to a point. The Comtrade descriptions run to
# 200 characters for the uranium headings, which no amount of wrapping will fit.
JNS_SHORT_NAMES <- c(
  "284410" = "Natural uranium",
  "284420" = "Enriched uranium & plutonium",
  "284450" = "Spent fuel elements",
  "840110" = "Nuclear reactors",
  "840120" = "Isotopic separation machinery",
  "840130" = "Fuel elements (fresh)",
  "840140" = "Reactor parts"
)

jns_short_label <- function(hs6, cmd_desc) {
  out <- unname(JNS_SHORT_NAMES[hs6])
  fallback <- sub(";.*$", "", cmd_desc)
  fallback <- substr(fallback, 1, 34)
  out[is.na(out)] <- fallback[is.na(out)]
  out
}

# Figure 4, left: the focal country's exports and world share over the Atlas history.
jns_trend <- function(repo_root, focus_iso, from_year = 1995L) {
  codes <- ts_tech_codes(repo_root, JNS_TECH)
  atlas <- ts_atlas_trade(repo_root, codes$hs6)
  if (is.null(atlas)) return(NULL)

  matched <- length(intersect(codes$hs6, unique(atlas$hs6)))
  message(
    "Nuclear long series: ", matched, " of ", nrow(codes),
    " codes exist in the Atlas HS92 nomenclature; ",
    nrow(codes) - matched, " were introduced by a later HS revision."
  )

  atlas <- atlas[atlas$year >= from_year, , drop = FALSE]
  world <- stats::aggregate(export_value ~ year, data = atlas, FUN = sum)
  names(world)[2] <- "world_exports"

  focal <- atlas[atlas$reporter_iso == focus_iso, , drop = FALSE]
  if (nrow(focal) == 0) {
    message("Atlas panel has no rows for ", focus_iso, "; skipping the long series.")
    return(NULL)
  }
  f <- stats::aggregate(cbind(exports = export_value, imports = import_value) ~ year,
                        data = focal, FUN = sum)

  out <- merge(f, world, by = "year")
  out$world_share <- out$exports / out$world_exports
  attr(out, "codes_matched") <- matched
  attr(out, "codes_total") <- nrow(codes)
  out[order(out$year), , drop = FALSE]
}

# Figure 4, right: annual nuclear investment, focal country against the peer set.
jns_investment <- function(repo_root, focus_iso, focus_name, peers = TS_POLICY_PEERS) {
  path <- file.path(repo_root, config$processed_dir, "investment_momentum_tbl.rds")
  if (!file.exists(path)) {
    message("investment_momentum_tbl not found; skipping the investment panel.")
    return(NULL)
  }
  tbl <- as.data.frame(readRDS(path))

  d <- tbl[
    tbl$tech == JNS_TECH &
      tbl$variable == "Annual Investment (USD bn, 2024$)" &
      tbl$data_type == "raw",
    , drop = FALSE
  ]
  if (nrow(d) == 0) {
    message("No nuclear annual-investment rows; skipping the investment panel.")
    return(NULL)
  }

  # Peers are matched on ISO3, not on the country name. This table spells Korea
  # "South Korea" while the peer set and the policy figures use countrycode's "Korea",
  # and matching on the name silently drops it rather than failing.
  if (!"iso3c" %in% names(d)) {
    stop("investment_momentum_tbl has no iso3c column to match peers on.")
  }
  wanted_iso <- unique(c(focus_iso, names(peers)))
  d <- d[d$iso3c %in% wanted_iso, , drop = FALSE]
  if (nrow(d) == 0) {
    message("No nuclear investment rows for the peer set; skipping the investment panel.")
    return(NULL)
  }

  labels <- c(peers, stats::setNames(focus_name, focus_iso))
  out <- data.frame(
    iso3 = d$iso3c, country = unname(labels[d$iso3c]),
    year = as.integer(d$Year), investment_bn = as.numeric(d$value),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$year) & !is.na(out$investment_bn), , drop = FALSE]
  out <- stats::aggregate(investment_bn ~ iso3 + country + year, data = out, FUN = sum)

  present <- unique(out$country)
  missing <- setdiff(unname(labels[wanted_iso]), present)
  if (length(missing) > 0) {
    message("Nuclear investment: no rows for ", paste(missing, collapse = ", "),
            " (absent from the investment theme, not zeroed).")
  }
  out$country <- factor(out$country, levels = c(focus_name, setdiff(present, focus_name)))
  out[order(out$country, out$year), , drop = FALSE]
}

## Figures -------------------------------------------------------------------

jns_plot_fig1 <- function(fig1, focus_iso, focus_name, trade_year) {
  world_bn <- attr(fig1, "world_exports") / 1e9
  focus_rank <- attr(fig1, "focus_rank")

  totals <- stats::aggregate(share_of_world ~ reporter_iso, data = fig1, FUN = sum)
  totals$label <- sprintf("%.1f%%", 100 * totals$share_of_world)
  totals$is_focus <- as.character(totals$reporter_iso) == focus_iso

  ggplot2::ggplot(fig1, ggplot2::aes(x = share_of_world, y = reporter_iso, fill = sub_sector)) +
    # reverse = TRUE anchors the fuel cycle at the left edge of every bar, so the segment
    # the title is about starts from a common baseline and can be compared by eye.
    ggplot2::geom_col(width = 0.7, position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_text(
      data = totals,
      ggplot2::aes(x = share_of_world, y = reporter_iso, label = label,
                   fontface = ifelse(is_focus, "bold", "plain"),
                   colour = is_focus),
      inherit.aes = FALSE, hjust = -0.18, size = 3, show.legend = FALSE
    ) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = TS_FOCUS_COLOUR, `FALSE` = "grey35")) +
    ggplot2::scale_fill_manual(values = JNS_SUBSECTOR_COLOURS, drop = FALSE) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0, 0.13))
    ) +
    ggplot2::labs(
      title = sprintf(
        "%s is the world's %s-largest nuclear exporter, on the strength of its steam plant",
        focus_name, scales::label_ordinal()(focus_rank)
      ),
      subtitle = sprintf(
        "Share of the $%.0fbn world market in nuclear-tech commodities, %d",
        world_bn, trade_year
      ),
      x = NULL, y = NULL, fill = NULL
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1)) +
    ts_theme() +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.major.y = ggplot2::element_blank()
    )
}

jns_plot_fig2 <- function(fig2, trade_year) {
  fig2$size_bn <- fig2$world_exports / 1e9
  # Only the commodities worth naming; the tail is boiler parts at a few hundred million.
  named <- fig2[fig2$exclusive | fig2$size_bn > 1.5, , drop = FALSE]

  p <- ggplot2::ggplot(fig2, ggplot2::aes(x = size_bn, y = top3_share)) +
    ggplot2::geom_point(ggplot2::aes(colour = kind, size = size_bn), alpha = 0.85) +
    ggplot2::scale_colour_manual(values = c(
      `Nuclear-specific` = unname(TS_PALETTE["navy"]),
      `Shared with other thermal plant` = "grey65"
    )) +
    ggplot2::scale_size_area(max_size = 9, guide = "none") +
    ggplot2::scale_x_log10(
      # These commodities span five orders of magnitude - spent fuel trades in single
      # millions, natural uranium in billions - so a fixed accuracy rounds the small end
      # to $0.00bn. Short-scale cuts label each decade in its own unit.
      labels = scales::label_dollar(scale_cut = scales::cut_short_scale()),
      expand = ggplot2::expansion(mult = 0.16)
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 1), expand = ggplot2::expansion(mult = 0.03)
    ) +
    ggplot2::labs(
      title = "The nuclear-specific commodities are the concentrated ones",
      subtitle = sprintf(
        "Every commodity in the nuclear crosswalk, %d: world export value against the share held by its top three suppliers",
        trade_year
      ),
      x = "World exports (log scale)", y = "Top-3 supplier share", colour = NULL
    ) +
    ts_theme() +
    ggplot2::theme(legend.position = "bottom")

  if (requireNamespace("ggrepel", quietly = TRUE)) {
    p + ggrepel::geom_text_repel(
      data = named,
      ggplot2::aes(label = sprintf("%s (%s)", label, top_supplier_iso), colour = kind),
      size = 2.8, seed = 7, min.segment.length = 0.2, segment.colour = "grey70",
      max.overlaps = Inf, box.padding = 0.45, show.legend = FALSE
    )
  } else {
    p + ggplot2::geom_text(
      data = named, ggplot2::aes(label = label, colour = kind),
      size = 2.8, vjust = -1, show.legend = FALSE
    )
  }
}

# Figure 3: importer exposure, coloured by how much of the bill is fuel cycle.
#
# The plain exposure scatter used by the other technology scripts would put Japan below
# the median and leave the reader to conclude it is comparatively safe. It is not that
# simple: measured exposure here is almost entirely a function of what fraction of an
# importer's bill is fuel cycle, which is the concentrated part. Colouring by that
# fraction shows the mechanism instead of hiding it, and makes Japan's position legible
# as "buys little fuel" rather than "buys from many sellers".
jns_fig3_data <- function(trade, exposure) {
  imports <- trade[trade$flow == "Imports", , drop = FALSE]
  fuel <- imports[imports$sub_sector == "Nuclear Fuel Cycle", , drop = FALSE]

  bill <- stats::aggregate(value ~ reporter_iso, data = imports, FUN = sum)
  names(bill)[2] <- "bill"
  fc <- stats::aggregate(value ~ reporter_iso, data = fuel, FUN = sum)
  names(fc)[2] <- "fuel_cycle"

  d <- merge(bill, fc, by = "reporter_iso", all.x = TRUE)
  d$fuel_cycle[is.na(d$fuel_cycle)] <- 0
  d$fuel_cycle_share <- d$fuel_cycle / d$bill

  out <- merge(exposure, d[, c("reporter_iso", "fuel_cycle_share")], by = "reporter_iso")
  out[order(-out$import_bill_usd), , drop = FALSE]
}

jns_plot_fig3 <- function(fig3, focus_iso, focus_name, top_n, trade_year) {
  d <- utils::head(fig3, top_n)
  d$bill_bn <- d$import_bill_usd / 1e9
  d$is_focus <- d$reporter_iso == focus_iso
  r <- stats::cor(d$fuel_cycle_share, d$weighted_supplier_hhi)
  focal <- d[d$is_focus, , drop = FALSE]

  p <- ggplot2::ggplot(d, ggplot2::aes(x = bill_bn, y = weighted_supplier_hhi)) +
    ggplot2::geom_hline(
      yintercept = stats::median(d$weighted_supplier_hhi), linetype = "dashed",
      colour = "grey60", linewidth = 0.4
    ) +
    ggplot2::geom_point(ggplot2::aes(fill = fuel_cycle_share, size = is_focus),
                        shape = 21, colour = "grey30", stroke = 0.5) +
    ggplot2::scale_fill_gradient(
      low = "#EAF1F5", high = unname(TS_PALETTE["navy"]),
      labels = scales::percent_format(accuracy = 1), limits = c(0, 1)
    ) +
    ggplot2::scale_size_manual(values = c(`TRUE` = 6, `FALSE` = 3.6), guide = "none") +
    ggplot2::scale_x_log10(
      labels = scales::label_dollar(suffix = "bn", accuracy = 0.1),
      expand = ggplot2::expansion(mult = 0.10)
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
    ggplot2::labs(
      title = "Exposure is a question of how much fuel you buy",
      subtitle = sprintf(
        paste0(
          "%d nuclear-tech imports against the concentration of the markets they come from, %d largest importers.\n",
          "Fill is the fuel cycle's share of each importer's bill; it explains almost all of the spread (r = %.2f)."
        ),
        trade_year, nrow(d), r
      ),
      x = sprintf("%d imports (log scale)", trade_year),
      y = "Import-weighted supplier HHI",
      fill = "Fuel cycle\nshare of bill"
    ) +
    ts_theme() +
    ggplot2::theme(legend.position = "right")

  lab <- if (requireNamespace("ggrepel", quietly = TRUE)) {
    ggrepel::geom_text_repel(
      ggplot2::aes(label = reporter_iso, fontface = ifelse(is_focus, "bold", "plain")),
      size = 3, seed = 42, min.segment.length = 0.2, segment.colour = "grey70",
      max.overlaps = Inf, colour = "grey25"
    )
  } else {
    ggplot2::geom_text(ggplot2::aes(label = reporter_iso), size = 3, vjust = -1.1,
                       colour = "grey25")
  }

  p <- p + lab
  if (nrow(focal) == 1) {
    p <- p + ggplot2::annotate(
      "point", x = focal$bill_bn, y = focal$weighted_supplier_hhi,
      shape = 21, size = 9, colour = TS_FOCUS_COLOUR, fill = NA, stroke = 1.2
    )
  }
  p
}

jns_plot_fig4_left <- function(trend, focus_name) {
  # Two units on one panel: exports in dollars as bars, world share as a line on a
  # secondary axis. The scaling factor maps share onto the dollar axis.
  scale_factor <- max(trend$exports) / max(trend$world_share)

  ggplot2::ggplot(trend, ggplot2::aes(x = year)) +
    ggplot2::geom_col(
      ggplot2::aes(y = exports / 1e9), fill = "#C9CFD4", width = 0.8
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = world_share * scale_factor / 1e9),
      colour = TS_FOCUS_COLOUR, linewidth = 1.2
    ) +
    ggplot2::scale_y_continuous(
      name = sprintf("%s exports ($bn, bars)", focus_name),
      labels = scales::label_dollar(suffix = "bn", accuracy = 0.5),
      sec.axis = ggplot2::sec_axis(
        ~ . * 1e9 / scale_factor,
        name = "Share of world exports (line)",
        labels = scales::percent_format(accuracy = 1)
      ),
      expand = ggplot2::expansion(mult = c(0, 0.06))
    ) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 6)) +
    ggplot2::labs(
      title = "Shrinking into a growing market",
      subtitle = sprintf(
        "%s's nuclear-tech exports and its share of the world total, %d-%d",
        focus_name, min(trend$year), max(trend$year)
      ),
      x = NULL
    ) +
    ts_theme() +
    ggplot2::theme(
      axis.title.y.right = ggplot2::element_text(colour = TS_FOCUS_COLOUR),
      axis.text.y.right = ggplot2::element_text(colour = TS_FOCUS_COLOUR)
    )
}

jns_plot_fig4_right <- function(investment, focus_name) {
  focal <- investment[investment$country == focus_name, , drop = FALSE]

  ggplot2::ggplot(investment, ggplot2::aes(x = year, y = investment_bn, colour = country)) +
    ggplot2::geom_line(ggplot2::aes(linewidth = country == focus_name)) +
    ggplot2::geom_point(data = focal, size = 1.6) +
    ggplot2::scale_colour_manual(values = ts_policy_colours(focus_name)) +
    ggplot2::scale_linewidth_manual(values = c(`TRUE` = 1.3, `FALSE` = 0.6), guide = "none") +
    ggplot2::scale_y_continuous(
      labels = scales::label_dollar(suffix = "bn", accuracy = 1)
    ) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 5)) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(
      title = "...but the money has turned back up",
      subtitle = sprintf(
        "Annual nuclear investment, constant 2024 dollars, %d-%d",
        min(investment$year), max(investment$year)
      ),
      x = NULL, y = NULL, colour = NULL
    ) +
    ts_theme() +
    ggplot2::theme(legend.position = "bottom")
}

## Runner --------------------------------------------------------------------

run_japan_nuclear <- function(repo_root,
                              focus_iso = "JPN",
                              policy_from = 2015L,
                              trend_from = 1995L,
                              top_importers = 20L,
                              write_output = TRUE) {
  ensure_packages(c("ggplot2", "scales"))

  focus_name <- ts_focus_name(focus_iso)
  trade <- jns_trade(repo_root)
  trade_year <- suppressWarnings(max(trade$year, na.rm = TRUE))
  if (!is.finite(trade_year)) trade_year <- NA_integer_

  exclusive_share <- sum(trade$value[trade$flow == "Exports" & trade$exclusive]) /
    sum(trade$value[trade$flow == "Exports"])
  message(sprintf(
    "Nuclear trade panel: %d commodities, %d reporters, year %s; %.0f%% of world exports are nuclear-specific codes.",
    length(unique(trade$hs6)), length(unique(trade$reporter_iso)),
    trade_year, 100 * exclusive_share
  ))

  fig1 <- jns_fig1_data(trade, focus_iso = focus_iso)
  fig2 <- jns_fig2_data(trade)

  imports <- trade[trade$flow == "Imports", , drop = FALSE]
  exposure <- ts_supplier_exposure(imports, fig2[, c("hs6", "supplier_hhi", "top_supplier_share")])
  exposure <- jns_fig3_data(trade, exposure)

  focus_row <- exposure[exposure$reporter_iso == focus_iso, , drop = FALSE]
  if (nrow(focus_row) == 1) {
    rank <- sum(exposure$weighted_supplier_hhi >= focus_row$weighted_supplier_hhi)
    message(sprintf(
      "%s: $%.2fbn nuclear-tech imports, weighted supplier HHI %.3f (rank %d of %d importers); fuel cycle is %.0f%% of the bill.",
      focus_name, focus_row$import_bill_usd / 1e9, focus_row$weighted_supplier_hhi,
      rank, nrow(exposure), 100 * focus_row$fuel_cycle_share
    ))
  }

  trend <- jns_trend(repo_root, focus_iso = focus_iso, from_year = trend_from)
  if (!is.null(trend)) {
    first <- trend[which.min(trend$year), ]
    last <- trend[which.max(trend$year), ]
    message(sprintf(
      "%s nuclear exports %d->%d: $%.2fbn -> $%.2fbn; world share %.1f%% -> %.1f%%.",
      focus_name, first$year, last$year, first$exports / 1e9, last$exports / 1e9,
      100 * first$world_share, 100 * last$world_share
    ))
  }
  investment <- jns_investment(repo_root, focus_iso = focus_iso, focus_name = focus_name)
  policy <- ts_policy_activity(repo_root, JNS_TECH, from_year = policy_from)

  result <- list(
    codes = attr(trade, "codes"), position = fig1, chokepoints = fig2,
    exposure = exposure, trend = trend, investment = investment, policy = policy,
    meta = list(focus_iso = focus_iso, focus_name = focus_name, trade_year = trade_year,
                exclusive_export_share = exclusive_share)
  )
  if (!write_output) return(invisible(result))

  out_dir <- ts_out_dir(repo_root)
  prefix <- sprintf("%s_nuclear", tolower(focus_iso))

  trade_cap <- paste0(
    sprintf("Source: UN Comtrade %d (energy-tech HS6 pull), reporter against partner World, on the repo's HS6 technology crosswalk.\n", trade_year),
    "Eleven of the eighteen commodities - boilers, condensers, steam turbines - are shared with fossil and geothermal plant and are\n",
    "counted under those technologies too, so this is nuclear-attributed trade, not nuclear-exclusive trade."
  )

  ts_write(attr(trade, "codes"), out_dir, paste0(prefix, "_commodity_codes"))
  ts_write(fig1, out_dir, paste0(prefix, "_export_position"))
  ts_write(fig2, out_dir, paste0(prefix, "_chokepoints"))
  ts_write(exposure, out_dir, paste0(prefix, "_supplier_exposure"))
  if (!is.null(trend)) ts_write(trend, out_dir, paste0(prefix, "_export_trend"))
  if (!is.null(investment)) ts_write(investment, out_dir, paste0(prefix, "_investment"))
  ts_write(policy, out_dir, paste0(prefix, "_policy_activity"))

  pngs <- c(
    ts_save(
      jns_plot_fig1(fig1, focus_iso, focus_name, trade_year), out_dir,
      paste0(prefix, "_01_export_position"), 10, 6, trade_cap
    ),
    ts_save(
      jns_plot_fig2(fig2, trade_year), out_dir,
      paste0(prefix, "_02_chokepoints"), 10, 6.0,
      paste0(
        trade_cap,
        "\nThree-letter codes name each commodity's largest exporter. Shares are gross, so enrichment and conversion services show up in the",
        "\ncountry that ships the material on - the Netherlands for Urenco output, not the country the uranium was mined in."
      )
    ),
    ts_save(
      jns_plot_fig3(exposure, focus_iso, focus_name, top_importers, trade_year),
      out_dir, paste0(prefix, "_03_supplier_exposure"), 10, 6.4,
      paste0(
        ts_exposure_caption(trade_year),
        sprintf(
          "\nA low score here means a small fuel bill, not a diversified one: %s's reactor fleet has run well below its pre-2011 output, so\nthe fuel cycle is a smaller share of a much smaller bill. Dashed line is the median of the importers shown.",
          focus_name
        )
      )
    ),
    ts_save(
      ts_plot_policy(
        policy, focus_name, "nuclear",
        c("Upstream (fuel & raw material)", "Midstream (reactors & components)")
      ) + ggplot2::labs(title = "Policy has come back, at the reactor end"),
      out_dir, paste0(prefix, "_05_policy_activity"), 10, 5.2, ts_policy_caption(policy)
    )
  )

  # Figure 4 needs at least one of its two panels.
  panels <- list()
  if (!is.null(trend)) panels <- c(panels, list(jns_plot_fig4_left(trend, focus_name)))
  if (!is.null(investment)) panels <- c(panels, list(jns_plot_fig4_right(investment, focus_name)))
  if (length(panels) > 0) {
    fig4 <- if (length(panels) == 1) {
      panels[[1]]
    } else {
      cowplot::ggdraw(cowplot::plot_grid(plotlist = panels, nrow = 1, rel_widths = c(1.05, 1)))
    }
    trend_cap <- paste0(
      "Sources: Harvard Atlas of Economic Complexity (HS92, left) and the repo's investment-momentum theme, built on BNEF (right).\n",
      "The Atlas is HS92, so commodities introduced by later HS revisions are absent from the left panel; the run log reports how many.\n",
      "The two panels come from different sources and different vintages and should not be read as one continuous series."
    )
    pngs <- c(pngs, ts_save(
      fig4, out_dir, paste0(prefix, "_04_momentum"),
      if (length(panels) == 1) 7 else 12, 5.4, trend_cap
    ))
  }

  result$png <- pngs
  invisible(result)
}

jns_run_directly <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  length(file_arg) > 0 &&
    identical(basename(sub("^--file=", "", file_arg[1])), "japan_nuclear_security.R")
}

if (jns_run_directly()) {
  invisible(run_japan_nuclear(
    repo_root = repo_root,
    focus_iso = toupper(Sys.getenv("JNS_FOCUS_ISO", "JPN")),
    policy_from = as.integer(Sys.getenv("JNS_POLICY_FROM", "2015")),
    trend_from = as.integer(Sys.getenv("JNS_TREND_FROM", "1995")),
    top_importers = as.integer(Sys.getenv("JNS_TOP_IMPORTERS", "20"))
  ))
}
