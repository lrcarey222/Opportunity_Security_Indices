# Japan's grid-equipment economic opportunity, supply security, and policy momentum.
#
# The argument in one line: grid equipment is the largest energy-technology market in this
# repo and it is growing fast, Japan's exports into it have grown in absolute terms and
# collapsed in relative terms, the components that matter most for grid build-out are
# concentrated in a handful of suppliers, and Japan's policy response only turned up
# after 2021.
#
#   fig 1  position      Who exports grid equipment, split by component family
#   fig 2  momentum      Japan's exports and world share since 1995, and who took the
#                        share it lost
#   fig 3  chokepoints   Concentration of every component against its size
#   fig 4  exposure      Import-bill-weighted supplier concentration, Japan against the
#                        other large importers
#   fig 5  policy        Active domestic interventions, Japan against four peers
#
# Two things about the commodity universe are worth knowing before reading any of it.
#
# Almost nothing here is grid-exclusive. Forty-three of the crosswalk's fifty grid codes
# are also filed under solar, wind or EVs - a static converter is grid equipment and also
# inverter equipment - so these are grid-attributed aggregates that overlap other clean
# technologies, not a self-contained sector. Where nuclear had a clean exclusive core to
# separate out, grid does not.
#
# And two groups of codes are dropped by default, both recorded in the commodity-codes
# CSV. Cross-border electricity itself (271600, $57bn) is a traded commodity rather than
# a component. Chapter 44 softwood ($6bn) is in the crosswalk as pole timber, but the
# headings are generic pine and fir logs that mostly become construction lumber; left in,
# New Zealand pine ranks as the single most concentrated "grid component" in the world.
# Set JGS_KEEP_ALL_CODES=true to put both back.
#
# Run:
#   Rscript R/charts/japan_grid_components_security.R
#
# Environment:
#   JGS_FOCUS_ISO       focal reporter ISO3 (default JPN)
#   JGS_POLICY_FROM     first announcement year on figure 5 (default 2015)
#   JGS_TREND_FROM      first year of the long trade series on figure 2 (default 1995)
#   JGS_TOP_IMPORTERS   how many importers to plot on figure 4 (default 20)
#   JGS_KEEP_ALL_CODES  keep electricity and softwood in the universe (default false)

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

JGS_TECH <- "Electric Grid"

# Cross-border electricity: a traded commodity, not a piece of equipment. It is the
# crosswalk's only Upstream grid row, which is how it is identified rather than by code.
JGS_ELECTRICITY_CODE <- "271600"

# Chapter 44 softwood. See the header note.
JGS_TIMBER_PREFIX <- "44"

# The crosswalk's twelve equipment sub-sectors, collapsed to five families a legend can
# hold. Anything unmapped would fail loudly in jgs_family() rather than vanish.
JGS_FAMILIES <- list(
  "Cables & conductors" = "Grid Cables & Accessories",
  "Transformers" = "Transformers",
  "Switchgear & protection" = c(
    "Circuit Breakers", "Switchgear", "Surge Arresters",
    "Other High-Voltage Apparatus", "Power Capacitors & Fuses"
  ),
  "Converters & power electronics" = "Power Converters & Parts",
  "Control, metering & automation" = c(
    "Grid Control Panels", "Grid Metering Equipment", "Grid Automation & Control Equipment"
  ),
  "Electrical steel (GOES)" = "Electrical Steel (GOES)"
)

JGS_FAMILY_LEVELS <- names(JGS_FAMILIES)

JGS_FAMILY_COLOURS <- c(
  "Cables & conductors" = unname(TS_PALETTE["navy"]),
  "Transformers" = unname(TS_PALETTE["blue"]),
  "Switchgear & protection" = unname(TS_PALETTE["cyan"]),
  "Converters & power electronics" = unname(TS_PALETTE["green"]),
  "Control, metering & automation" = unname(TS_PALETTE["yellow"]),
  "Electrical steel (GOES)" = "#C9CFD4"
)

jgs_family <- function(sub_sector) {
  lookup <- stats::setNames(
    rep(names(JGS_FAMILIES), lengths(JGS_FAMILIES)),
    unlist(JGS_FAMILIES, use.names = FALSE)
  )
  out <- unname(lookup[sub_sector])
  if (any(is.na(out))) {
    stop("Grid sub-sector(s) not mapped to a component family: ",
         paste(unique(sub_sector[is.na(out)]), collapse = ", "))
  }
  factor(out, levels = JGS_FAMILY_LEVELS)
}

# Peers for the share-of-market panel: the incumbent, the challenger that took the share,
# and the two other advanced exporters.
JGS_TREND_PEERS <- c(
  JPN = "Japan", CHN = "China", DEU = "Germany", USA = "United States", KOR = "Korea"
)

# China is on this figure but not in the policy peer set, so the trend panel carries its
# own palette rather than borrowing ts_policy_colours(), which would leave China unmapped
# and drawn in the same grey as the United States.
JGS_TREND_COLOURS <- c(
  Japan = TS_FOCUS_COLOUR,
  China = unname(TS_PALETTE["navy"]),
  Germany = unname(TS_PALETTE["green"]),
  `United States` = unname(TS_PALETTE["grey"]),
  Korea = unname(TS_PALETTE["blue"])
)

## Data ----------------------------------------------------------------------

jgs_codes <- function(repo_root, keep_all = FALSE) {
  codes <- ts_tech_codes(repo_root, JGS_TECH)

  codes$dropped_reason <- NA_character_
  codes$dropped_reason[codes$value_chain != "Midstream"] <- "cross-border electricity, not equipment"
  codes$dropped_reason[substr(codes$hs6, 1, 2) == JGS_TIMBER_PREFIX] <-
    "generic softwood heading, not a grid-specific component"

  if (keep_all) {
    codes$dropped_reason <- NA_character_
  }
  codes$in_universe <- is.na(codes$dropped_reason)
  codes
}

jgs_trade <- function(repo_root, keep_all = FALSE) {
  codes <- jgs_codes(repo_root, keep_all = keep_all)
  panel <- ts_comtrade_energy(repo_root)

  universe <- codes[codes$in_universe, , drop = FALSE]
  trade <- merge(panel, universe[, c("hs6", "sub_sector")], by = "hs6")
  if (nrow(trade) == 0) {
    stop("No grid commodities found in the Comtrade energy-tech pull.")
  }
  trade$family <- jgs_family(trade$sub_sector)

  missing <- setdiff(universe$hs6, unique(trade$hs6))
  if (length(missing) > 0) {
    absent <- universe[universe$hs6 %in% missing, ]
    message(
      "Grid: ", length(missing), " of ", nrow(universe),
      " commodities absent from the Comtrade pull, including all of ",
      paste(sort(unique(absent$sub_sector[absent$sub_sector == "Electrical Steel (GOES)"])),
            collapse = ", "),
      " (", paste(missing, collapse = ", "), ")."
    )
  }

  attr(trade, "codes") <- codes
  attr(trade, "absent") <- missing
  trade
}

# Figure 1: top exporters, split by component family.
jgs_fig1_data <- function(trade, focus_iso, top_n = 14L) {
  exports <- trade[trade$flow == "Exports", , drop = FALSE]
  world <- sum(exports$value)

  by_reporter <- stats::aggregate(value ~ reporter_iso, data = exports, FUN = sum)
  by_reporter <- by_reporter[order(-by_reporter$value), , drop = FALSE]
  by_reporter$share <- by_reporter$value / world
  by_reporter$rank <- seq_len(nrow(by_reporter))

  keep <- unique(c(utils::head(by_reporter$reporter_iso, top_n), focus_iso))
  d <- exports[exports$reporter_iso %in% keep, , drop = FALSE]

  agg <- stats::aggregate(value ~ reporter_iso + family, data = d, FUN = sum)
  agg$share_of_world <- agg$value / world

  shown <- by_reporter[by_reporter$reporter_iso %in% keep, , drop = FALSE]
  agg$reporter_iso <- factor(agg$reporter_iso, levels = shown$reporter_iso[order(shown$value)])

  attr(agg, "world_exports") <- world
  attr(agg, "focus_rank") <- by_reporter$rank[by_reporter$reporter_iso == focus_iso]
  attr(agg, "focus_share") <- by_reporter$share[by_reporter$reporter_iso == focus_iso]
  agg[order(agg$reporter_iso, agg$family), , drop = FALSE]
}

# Figure 3: concentration against size, per component.
jgs_fig3_data <- function(trade) {
  exports <- trade[trade$flow == "Exports", , drop = FALSE]
  conc <- ts_commodity_concentration(exports)

  meta <- unique(trade[, c("hs6", "sub_sector", "family")])
  # Comtrade carries more than one description string for some codes - the same softwood
  # heading appears as both "n.e.c. in heading" and "n.e.c in headings" - so the first
  # per code is taken rather than letting the join fan out.
  desc <- unique(trade[, c("hs6", "cmd_desc")])
  desc <- desc[!duplicated(desc$hs6), , drop = FALSE]

  out <- merge(merge(conc, meta, by = "hs6"), desc, by = "hs6")
  out$label <- jgs_short_label(out$hs6, out$cmd_desc)
  out[order(-out$world_exports), , drop = FALSE]
}

# Comtrade descriptions are written for customs officers, not for axes, and their first
# clause is not distinguishing: all seven transformer headings begin "Electrical
# transformers", all six cable headings begin "Insulated electric conductors". Labelling
# from the description alone produces a chart with "Electrical transformers (CHN)" on it
# four times. These are the ratings and types that actually separate them.
JGS_SHORT_NAMES <- c(
  "850440" = "Static converters",
  "853710" = "Control panels, <1kV",
  "854449" = "Insulated conductors, other",
  "854442" = "Conductors, fitted with connectors",
  "740811" = "Copper wire, >6mm",
  "850490" = "Transformer & converter parts",
  "854460" = "Conductors, >1kV",
  "854470" = "Optical fibre cable",
  "850423" = "Liquid-dielectric transformers, >10MVA",
  "853720" = "Control panels, >1kV",
  "854420" = "Coaxial cable",
  "731210" = "Steel stranded wire",
  "850431" = "Transformers, <=1kVA",
  "853590" = "High-voltage apparatus, other",
  "850422" = "Liquid-dielectric transformers, 0.65-10MVA",
  "740819" = "Copper wire, <=6mm",
  "902830" = "Electricity meters",
  "850421" = "Liquid-dielectric transformers, <=650kVA",
  "853530" = "Isolating switches, >1kV",
  "902890" = "Meter parts",
  "741300" = "Copper stranded wire",
  "850434" = "Transformers, >500kVA",
  "850433" = "Transformers, 16-500kVA",
  "853521" = "Circuit breakers, <72.5kV",
  "853529" = "Circuit breakers, >=72.5kV",
  "761410" = "Aluminium cable, steel-cored",
  "761490" = "Aluminium cable, other",
  "850432" = "Transformers, 1-16kVA",
  "854620" = "Ceramic insulators",
  "853540" = "Surge arresters",
  "853510" = "Fuses, >1kV",
  "853210" = "Power capacitors",
  "281290" = "Halides (incl. SF6)",
  "854610" = "Glass insulators",
  "681510" = "Carbon & graphite articles"
)

jgs_short_label <- function(hs6, cmd_desc) {
  out <- unname(JGS_SHORT_NAMES[hs6])
  fallback <- trimws(sub(";.*$", "", cmd_desc))
  fallback <- ifelse(nchar(fallback) > 34, paste0(substr(fallback, 1, 33), "…"), fallback)
  out[is.na(out)] <- fallback[is.na(out)]
  out
}

# Figure 2: the focal country and its peers' shares of world exports, over the Atlas run.
jgs_trend <- function(repo_root, focus_iso, keep_all = FALSE, from_year = 1995L,
                      peers = JGS_TREND_PEERS) {
  codes <- jgs_codes(repo_root, keep_all = keep_all)
  universe <- codes$hs6[codes$in_universe]

  atlas <- ts_atlas_trade(repo_root, universe)
  if (is.null(atlas)) return(NULL)

  matched <- length(intersect(universe, unique(atlas$hs6)))
  message(
    "Grid long series: ", matched, " of ", length(universe),
    " codes exist in the Atlas HS92 nomenclature; ",
    length(universe) - matched, " were introduced by a later HS revision."
  )

  atlas <- atlas[atlas$year >= from_year, , drop = FALSE]
  world <- stats::aggregate(export_value ~ year, data = atlas, FUN = sum)
  names(world)[2] <- "world_exports"

  wanted <- unique(c(focus_iso, names(peers)))
  d <- atlas[atlas$reporter_iso %in% wanted, , drop = FALSE]
  if (nrow(d) == 0) {
    message("Atlas panel has no rows for the trend countries; skipping the long series.")
    return(NULL)
  }
  agg <- stats::aggregate(cbind(exports = export_value, imports = import_value) ~
                            reporter_iso + year, data = d, FUN = sum)

  out <- merge(agg, world, by = "year")
  out$world_share <- out$exports / out$world_exports
  labels <- c(peers, stats::setNames(ts_focus_name(focus_iso), focus_iso))
  out$name <- factor(unname(labels[out$reporter_iso]),
                     levels = unname(labels[wanted]))
  attr(out, "codes_matched") <- matched
  attr(out, "codes_total") <- length(universe)
  out[order(out$name, out$year), , drop = FALSE]
}

## Figures -------------------------------------------------------------------

jgs_plot_fig1 <- function(fig1, focus_iso, focus_name, trade_year) {
  world_bn <- attr(fig1, "world_exports") / 1e9
  focus_rank <- attr(fig1, "focus_rank")

  totals <- stats::aggregate(share_of_world ~ reporter_iso, data = fig1, FUN = sum)
  totals$label <- sprintf("%.1f%%", 100 * totals$share_of_world)
  totals$is_focus <- as.character(totals$reporter_iso) == focus_iso

  ggplot2::ggplot(fig1, ggplot2::aes(x = share_of_world, y = reporter_iso, fill = family)) +
    ggplot2::geom_col(width = 0.7, position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_text(
      data = totals,
      ggplot2::aes(x = share_of_world, y = reporter_iso, label = label,
                   colour = is_focus, fontface = ifelse(is_focus, "bold", "plain")),
      inherit.aes = FALSE, hjust = -0.18, size = 3, show.legend = FALSE
    ) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = TS_FOCUS_COLOUR, `FALSE` = "grey35")) +
    # drop = TRUE so electrical steel, which the Comtrade pull does not carry at all,
    # does not sit in the legend as a swatch with nothing behind it.
    ggplot2::scale_fill_manual(values = JGS_FAMILY_COLOURS, drop = TRUE) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0, 0.13))
    ) +
    ggplot2::labs(
      title = sprintf(
        "%s is the %s-largest exporter into the biggest energy-tech market there is",
        focus_name, scales::label_ordinal()(focus_rank)
      ),
      subtitle = sprintf(
        "Share of the $%.0fbn world market in grid equipment, %d", world_bn, trade_year
      ),
      x = NULL, y = NULL, fill = NULL
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2)) +
    ts_theme() +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.major.y = ggplot2::element_blank()
    )
}

jgs_plot_fig2_left <- function(trend, focus_name) {
  d <- trend[trend$name == focus_name, , drop = FALSE]
  scale_factor <- max(d$exports) / max(d$world_share)

  ggplot2::ggplot(d, ggplot2::aes(x = year)) +
    ggplot2::geom_col(ggplot2::aes(y = exports / 1e9), fill = "#C9CFD4", width = 0.8) +
    ggplot2::geom_line(
      ggplot2::aes(y = world_share * scale_factor / 1e9),
      colour = TS_FOCUS_COLOUR, linewidth = 1.2
    ) +
    ggplot2::scale_y_continuous(
      name = sprintf("%s exports ($bn, bars)", focus_name),
      labels = scales::label_dollar(suffix = "bn", accuracy = 1),
      sec.axis = ggplot2::sec_axis(
        ~ . * 1e9 / scale_factor, name = "Share of world exports (line)",
        labels = scales::percent_format(accuracy = 1)
      ),
      expand = ggplot2::expansion(mult = c(0, 0.06))
    ) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 6)) +
    ggplot2::labs(
      title = "Bigger every decade, and smaller every decade",
      subtitle = sprintf(
        "%s's grid-equipment exports and its share of the world total, %d-%d",
        focus_name, min(d$year), max(d$year)
      ),
      x = NULL
    ) +
    ts_theme() +
    ggplot2::theme(
      axis.title.y.right = ggplot2::element_text(colour = TS_FOCUS_COLOUR),
      axis.text.y.right = ggplot2::element_text(colour = TS_FOCUS_COLOUR)
    )
}

jgs_plot_fig2_right <- function(trend, focus_name) {
  ends <- do.call(rbind, lapply(split(trend, trend$name, drop = TRUE), function(d) {
    d[which.max(d$year), , drop = FALSE]
  }))

  ggplot2::ggplot(trend, ggplot2::aes(x = year, y = world_share, colour = name)) +
    ggplot2::geom_line(ggplot2::aes(linewidth = name == focus_name)) +
    ggplot2::geom_text(
      data = ends,
      ggplot2::aes(label = sprintf(" %s %.1f%%", name, 100 * world_share)),
      hjust = 0, size = 2.9, show.legend = FALSE
    ) +
    ggplot2::scale_colour_manual(values = JGS_TREND_COLOURS) +
    ggplot2::scale_linewidth_manual(values = c(`TRUE` = 1.4, `FALSE` = 0.7), guide = "none") +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_pretty(n = 5),
      expand = ggplot2::expansion(mult = c(0.02, 0.22))
    ) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(
      title = "Where the share went",
      subtitle = "Share of world grid-equipment exports",
      x = NULL, y = NULL, colour = NULL
    ) +
    ts_theme() +
    ggplot2::theme(legend.position = "none")
}

# Ranked lollipop rather than a size-against-concentration scatter.
#
# The scatter that works for nuclear - eighteen commodities spanning five orders of
# magnitude, with a clean exclusive/shared split - does not work here. Thirty-five
# components bunched into two decades of value and a narrow concentration band overlap
# into an unreadable mass, and the one thing a reader wants from the figure, which
# specific component is tight and who holds it, is exactly what gets lost. Ranking the
# largest components by concentration puts that in the axis order.
jgs_plot_fig3 <- function(fig3, trade_year, top_n = 18L) {
  d <- utils::head(fig3[order(-fig3$world_exports), , drop = FALSE], top_n)
  covered <- sum(d$world_exports) / sum(fig3$world_exports)

  d$label <- factor(d$label, levels = d$label[order(d$top3_share)])
  d$annot <- sprintf(
    "  %s  %s", d$top_supplier_iso,
    scales::label_dollar(scale_cut = scales::cut_short_scale())(d$world_exports)
  )

  # The headline is counted off the data rather than written by hand, so it cannot drift
  # away from the chart when the pull is refreshed.
  lead <- sort(table(d$top_supplier_iso), decreasing = TRUE)
  lead_iso <- names(lead)[[1]]
  lead_name <- ts_focus_name(lead_iso)
  headline <- sprintf(
    "No single chokepoint, but %s leads %d of the %d biggest components",
    lead_name, lead[[1]], nrow(d)
  )

  ggplot2::ggplot(d, ggplot2::aes(x = top3_share, y = label, colour = family)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = top3_share, yend = label), linewidth = 0.8, alpha = 0.75
    ) +
    ggplot2::geom_point(ggplot2::aes(size = world_exports)) +
    ggplot2::geom_text(
      ggplot2::aes(label = annot), hjust = 0, size = 2.8, colour = "grey30"
    ) +
    ggplot2::scale_colour_manual(values = JGS_FAMILY_COLOURS, drop = TRUE) +
    ggplot2::scale_size_area(max_size = 5, guide = "none") +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 1.18), breaks = c(0, 0.25, 0.5, 0.75, 1), expand = c(0, 0)
    ) +
    ggplot2::labs(
      title = headline,
      subtitle = sprintf(
        "Share of world exports held by the top three suppliers, %d. The %d largest components, %.0f%% of the market;\ndot size is export value, and the labels give the largest supplier and that value.",
        trade_year, nrow(d), 100 * covered
      ),
      x = "Top-3 supplier share", y = NULL, colour = NULL
    ) +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2, override.aes = list(size = 3))) +
    ts_theme() +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.major.y = ggplot2::element_blank()
    )
}

## Runner --------------------------------------------------------------------

run_japan_grid_components <- function(repo_root,
                                      focus_iso = "JPN",
                                      policy_from = 2015L,
                                      trend_from = 1995L,
                                      top_importers = 20L,
                                      keep_all_codes = FALSE,
                                      write_output = TRUE) {
  ensure_packages(c("ggplot2", "scales"))

  focus_name <- ts_focus_name(focus_iso)
  trade <- jgs_trade(repo_root, keep_all = keep_all_codes)
  codes <- attr(trade, "codes")
  trade_year <- suppressWarnings(max(trade$year, na.rm = TRUE))
  if (!is.finite(trade_year)) trade_year <- NA_integer_

  dropped <- codes[!codes$in_universe, , drop = FALSE]
  if (nrow(dropped) > 0) {
    message("Grid universe: dropped ", nrow(dropped), " code(s) - ",
            paste(unique(dropped$dropped_reason), collapse = "; "),
            ". Set keep_all_codes = TRUE to retain them.")
  }
  message(sprintf(
    "Grid trade panel: %d commodities, %d reporters, year %s; $%.0fbn world exports.",
    length(unique(trade$hs6)), length(unique(trade$reporter_iso)), trade_year,
    sum(trade$value[trade$flow == "Exports"]) / 1e9
  ))

  fig1 <- jgs_fig1_data(trade, focus_iso = focus_iso)
  fig3 <- jgs_fig3_data(trade)

  imports <- trade[trade$flow == "Imports", , drop = FALSE]
  exposure <- ts_supplier_exposure(imports, fig3[, c("hs6", "supplier_hhi", "top_supplier_share")])

  focus_row <- exposure[exposure$reporter_iso == focus_iso, , drop = FALSE]
  if (nrow(focus_row) == 1) {
    rank <- sum(exposure$weighted_supplier_hhi >= focus_row$weighted_supplier_hhi)
    message(sprintf(
      "%s: $%.1fbn grid-equipment imports, weighted supplier HHI %.3f (rank %d of %d importers).",
      focus_name, focus_row$import_bill_usd / 1e9, focus_row$weighted_supplier_hhi,
      rank, nrow(exposure)
    ))
  }

  trend <- jgs_trend(repo_root, focus_iso = focus_iso, keep_all = keep_all_codes,
                     from_year = trend_from)
  if (!is.null(trend)) {
    f <- trend[trend$name == focus_name, , drop = FALSE]
    first <- f[which.min(f$year), ]
    last <- f[which.max(f$year), ]
    message(sprintf(
      "%s grid exports %d->%d: $%.1fbn -> $%.1fbn; world share %.1f%% -> %.1f%% of a market that grew $%.0fbn -> $%.0fbn.",
      focus_name, first$year, last$year, first$exports / 1e9, last$exports / 1e9,
      100 * first$world_share, 100 * last$world_share,
      first$world_exports / 1e9, last$world_exports / 1e9
    ))
  }
  policy <- ts_policy_activity(repo_root, JGS_TECH, from_year = policy_from)

  result <- list(
    codes = codes, position = fig1, chokepoints = fig3, exposure = exposure,
    trend = trend, policy = policy,
    meta = list(focus_iso = focus_iso, focus_name = focus_name, trade_year = trade_year,
                absent_codes = attr(trade, "absent"))
  )
  if (!write_output) return(invisible(result))

  out_dir <- ts_out_dir(repo_root)
  prefix <- sprintf("%s_grid_components", tolower(focus_iso))

  trade_cap <- paste0(
    sprintf("Source: UN Comtrade %d (energy-tech HS6 pull), reporter against partner World, on the repo's HS6 technology crosswalk.\n", trade_year),
    "Forty-three of the fifty grid codes are also filed under solar, wind or EVs, so this is grid-attributed trade that overlaps other\n",
    "clean technologies rather than a self-contained sector. Cross-border electricity and generic softwood headings are excluded; the\n",
    "grain-oriented electrical steel codes are in the crosswalk but absent from this pull, so the GOES chokepoint is not measured here."
  )

  ts_write(codes, out_dir, paste0(prefix, "_commodity_codes"))
  ts_write(fig1, out_dir, paste0(prefix, "_export_position"))
  ts_write(fig3, out_dir, paste0(prefix, "_chokepoints"))
  ts_write(exposure, out_dir, paste0(prefix, "_supplier_exposure"))
  if (!is.null(trend)) ts_write(trend, out_dir, paste0(prefix, "_export_trend"))
  ts_write(policy, out_dir, paste0(prefix, "_policy_activity"))

  pngs <- c(
    ts_save(
      jgs_plot_fig1(fig1, focus_iso, focus_name, trade_year), out_dir,
      paste0(prefix, "_01_export_position"), 10.5, 6.6, trade_cap
    ),
    ts_save(
      jgs_plot_fig3(fig3, trade_year), out_dir,
      paste0(prefix, "_03_chokepoints"), 10.5, 6.6,
      paste0(trade_cap, "\nThree-letter codes name each component's largest exporter.")
    ),
    ts_save(
      ts_plot_exposure(
        exposure, focus_iso, top_importers, trade_year,
        title = "Broad supply overall, and Japan buys at the narrow end of it",
        subtitle = sprintf(
          paste0(
            "%d grid-equipment imports against the concentration of the export markets they come from;\n",
            "dashed line is the median of the %d largest importers. Every score here is low - an HHI of 0.12 is\n",
            "about what eight equal suppliers would give - so the spread is between degrees of breadth, not chokeholds."
          ),
          trade_year, min(top_importers, nrow(exposure))
        )
      ),
      out_dir, paste0(prefix, "_04_supplier_exposure"), 9.4, 6.8,
      ts_exposure_caption(trade_year)
    ),
    ts_save(
      ts_plot_policy(
        policy, focus_name, "grid",
        c("Upstream (electricity supply)", "Midstream (equipment & components)")
      ) + ggplot2::labs(title = "The policy response arrived after 2021"),
      out_dir, paste0(prefix, "_05_policy_activity"), 10, 5.2, ts_policy_caption(policy)
    )
  )

  if (!is.null(trend)) {
    fig2 <- cowplot::ggdraw(cowplot::plot_grid(
      jgs_plot_fig2_left(trend, focus_name), jgs_plot_fig2_right(trend, focus_name),
      nrow = 1, rel_widths = c(1.05, 1)
    ))
    pngs <- c(pngs, ts_save(
      fig2, out_dir, paste0(prefix, "_02_momentum"), 12, 5.6,
      paste0(
        "Source: Harvard Atlas of Economic Complexity (HS92). The Atlas is HS92, so commodities introduced by later HS revisions are\n",
        "absent; the run log reports how many matched. Atlas values are not directly comparable with the Comtrade figures on the other\n",
        "panels, which are a different source and a later year."
      )
    ))
  }

  result$png <- pngs
  invisible(result)
}

jgs_run_directly <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  length(file_arg) > 0 &&
    identical(basename(sub("^--file=", "", file_arg[1])), "japan_grid_components_security.R")
}

if (jgs_run_directly()) {
  invisible(run_japan_grid_components(
    repo_root = repo_root,
    focus_iso = toupper(Sys.getenv("JGS_FOCUS_ISO", "JPN")),
    policy_from = as.integer(Sys.getenv("JGS_POLICY_FROM", "2015")),
    trend_from = as.integer(Sys.getenv("JGS_TREND_FROM", "1995")),
    top_importers = as.integer(Sys.getenv("JGS_TOP_IMPORTERS", "20")),
    keep_all_codes = tolower(Sys.getenv("JGS_KEEP_ALL_CODES", "false")) %in% c("1", "true", "yes")
  ))
}
