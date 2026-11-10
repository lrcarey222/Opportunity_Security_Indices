# Charts of the NEIS trade-fence explainer (outputs/trade_fence_explainer/README.md). Pure: every function takes
# chart-ready data frames (outputs/trade_fence_explainer/data/) and returns a ggplot; nothing is read or written
# here. scripts/trade_fence_explainer/02_draw_charts.R saves them. Needs ggplot2, scales, ggrepel.
#
# Visual system (one for all eight charts):
# - NEIS ink and surface: white background, ink #111827 / #374151 / #6b7280, hairline grid #e5e7eb, Roboto.
# - Supplier blocs: Trusted NEIS blue #1889b0, Nonaligned neutral grey #9ca3af (deliberately grey: the
#   "neither" group; always direct-labelled), Coercive NEIS orange #f58229 (China drawn in the same orange,
#   named). The dataviz validator passes CVD and normal-vision separation; grey fails its chroma floor by
#   design and grey/orange sit under 3:1 on white, so every bloc mark carries a direct label.
# - Fence height: one NEIS sequential blue ramp, light to dark (Low / Medium / High; TRI bins on chart 2).
# - Yards & Fences verdicts: green / purple / amber / red with glyphs (validator: all checks pass, amber
#   below 3:1 so cells carry glyph and text). Kept apart from the bloc colours on purpose.
# - The six choices are written as words in the cells, never coloured, so the page has one colour system per
#   job.

TFE_INK <- c(primary = "#111827", secondary = "#374151", muted = "#6b7280", grid = "#e5e7eb", faint = "#f3f4f6")
TFE_BLOC <- c(Trusted = "#1889b0", Nonaligned = "#9ca3af", Coercive = "#f58229", China = "#f58229",
              `All suppliers` = "#374151", `Coercive excl. China` = "#f58229")
TFE_VERDICT_COL <- c(Aligned = "#0e8a3a", `Taxes trusted partners` = "#7b4fbf", `Fence above recommendation` = "#c98500",
                     `Fence below recommendation / missing` = "#d03b3b")
TFE_VERDICT_GLYPH <- c(Aligned = "✓", `Taxes trusted partners` = "◆", `Fence above recommendation` = "▲",
                       `Fence below recommendation / missing` = "▽")
TFE_BAND_COL <- c(Low = "#e3f3fb", Medium = "#5cbfe8", High = "#016787")
TFE_TRI_BINS <- c(`Under 5%` = "#e3f3fb", `5-15%` = "#b4e7fe", `15-30%` = "#43a8d0", `30% and over` = "#016787")
TFE_STAGES <- c("Extraction", "Refining", "Components", "Assembly", "Deployment", "Recovery")
TFE_STAGE_AXIS <- c(Extraction = "Extraction", Refining = "Refining", Components = "Compo-\nnents", Assembly = "Assembly",
                    Deployment = "Deploy-\nment", Recovery = "Recovery")
TFE_TECH_LABEL <- c(Batteries = "Batteries", `Electric Vehicles` = "Electric vehicles", Solar = "Solar", Wind = "Wind",
                    `Electric Grid` = "Electric grid", Nuclear = "Nuclear", Gas = "Gas", Oil = "Oil", Coal = "Coal")

tfe_wrap <- function(x, width) paste(strwrap(x, width), collapse = "\n")
tfe_pct <- function(x, d = 1) ifelse(is.na(x), "", paste0(formatC(x, format = "f", digits = d), "%"))
tfe_bn <- function(x, d = 0) paste0("$", formatC(x / 1e9, format = "f", digits = d, big.mark = ","), "bn")

# Spread label positions `y` (one axis's units) so neighbours are at least `gap` apart, keeping their order.
tfe_spread <- function(y, gap) {
  o <- order(y)
  z <- y[o]
  for (i in seq_along(z)[-1]) z[i] <- max(z[i], z[i - 1] + gap)
  out <- y
  out[o] <- z
  out
}

# The house theme. `base` is the body size in points at the 6.5-inch output width.
tfe_theme <- function(base = 9.5, family = "") {
  ggplot2::theme_minimal(base_size = base, base_family = family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.5), colour = TFE_INK[["primary"]], lineheight = 1.05,
                                         margin = ggplot2::margin(b = 5)),
      plot.subtitle = ggplot2::element_text(size = ggplot2::rel(1.02), colour = TFE_INK[["secondary"]], lineheight = 1.2,
                                            margin = ggplot2::margin(b = 10)),
      plot.caption = ggplot2::element_text(size = ggplot2::rel(0.76), colour = TFE_INK[["muted"]], hjust = 0, lineheight = 1.22,
                                           margin = ggplot2::margin(t = 12)),
      plot.title.position = "plot", plot.caption.position = "plot",
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = TFE_INK[["grid"]], linewidth = 0.3),
      axis.text = ggplot2::element_text(colour = TFE_INK[["secondary"]]),
      axis.title = ggplot2::element_text(colour = TFE_INK[["secondary"]], size = ggplot2::rel(0.92)),
      strip.text = ggplot2::element_text(face = "bold", colour = TFE_INK[["primary"]], hjust = 0, size = ggplot2::rel(1.05)),
      legend.position = "none",
      plot.background = ggplot2::element_rect(fill = "#ffffff", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#ffffff", colour = NA),
      plot.margin = ggplot2::margin(14, 16, 10, 14)
    )
}

tfe_labs <- function(title, subtitle, caption, wt = 58, ws = 92, wc = 128) {
  ggplot2::labs(title = tfe_wrap(title, wt), subtitle = if (is.null(subtitle)) NULL else tfe_wrap(subtitle, ws),
                caption = paste(vapply(caption, tfe_wrap, character(1), width = wc), collapse = "\n"))
}

# ---- chart 1: the fence rising ---------------------------------------------------------------------------------
tfe_chart_01 <- function(d, family = "") {
  rows <- c("China", "Coercive", "Nonaligned", "Trusted", "All suppliers")
  lab <- c(China = "China", Coercive = "Coercive suppliers", Nonaligned = "Nonaligned suppliers", Trusted = "Trusted suppliers",
           `All suppliers` = "All suppliers")
  d <- d[d$supplier_group %in% rows, ]
  d$row <- factor(d$supplier_group, rev(rows))
  d$col <- unname(TFE_BLOC[d$supplier_group])
  d$right <- sprintf("%s  ×%.1f", tfe_pct(d$tri_latest), d$tri_latest / d$tri_2024)
  ggplot2::ggplot(d, ggplot2::aes(y = row)) +
    ggplot2::geom_segment(ggplot2::aes(x = tri_2024, xend = tri_latest, yend = row), colour = d$col, linewidth = 1.1, alpha = 0.55) +
    ggplot2::geom_point(ggplot2::aes(x = tri_2024), shape = 21, size = 3.4, stroke = 1, fill = "#ffffff", colour = d$col) +
    ggplot2::geom_point(ggplot2::aes(x = tri_latest), shape = 21, size = 3.8, stroke = 0.6, fill = d$col, colour = "#ffffff") +
    ggplot2::geom_text(ggplot2::aes(x = tri_2024 - 0.7, label = tfe_pct(tri_2024)), hjust = 1, size = 3.0, colour = TFE_INK[["muted"]], family = family) +
    ggplot2::geom_text(ggplot2::aes(x = tri_latest + 0.8, label = right), hjust = 0, size = 3.2, fontface = "bold",
                       colour = TFE_INK[["primary"]], family = family) +
    ggplot2::scale_y_discrete(labels = lab) +
    ggplot2::scale_x_continuous(limits = c(-4, 36), breaks = seq(0, 30, 10), labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
    ggplot2::labs(x = "Trade Restrictiveness Index, all US goods imports", y = NULL) +
    tfe_theme(family = family) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_text(size = 10, colour = TFE_INK[["primary"]]))
}

# ---- chart 2: tariff map ------------------------------------------------------------------------------------------
tfe_tri_bin <- function(x) {
  out <- ifelse(x < 5, names(TFE_TRI_BINS)[1], ifelse(x < 15, names(TFE_TRI_BINS)[2], ifelse(x < 30, names(TFE_TRI_BINS)[3], names(TFE_TRI_BINS)[4])))
  factor(out, names(TFE_TRI_BINS))
}
tfe_chart_02 <- function(d, techs, family = "") {
  d$in_basket <- d$in_basket %in% TRUE & !is.na(d$tri_all)
  d$thin <- d$thin %in% TRUE & d$in_basket
  d$tech_f <- factor(d$tech, rev(techs))
  d$stage_f <- factor(d$stage, TFE_STAGES)
  d$bin <- tfe_tri_bin(d$tri_all)
  d$txt <- ifelse(d$in_basket, formatC(d$tri_all, format = "f", digits = 0), "")
  d$ink <- ifelse(d$in_basket & d$tri_all >= 15, "#ffffff", TFE_INK[["primary"]])
  dd <- d[d$in_basket, ]
  na <- d[!d$in_basket, ]
  th <- dd[dd$thin, ]
  ggplot2::ggplot(dd, ggplot2::aes(x = stage_f, y = tech_f)) +
    ggplot2::geom_tile(data = na, fill = TFE_INK[["faint"]], colour = "#ffffff", linewidth = 1.2) +
    ggplot2::geom_text(data = na, label = "–", colour = "#9ca3af", size = 3.2, family = family) +
    ggplot2::geom_tile(ggplot2::aes(fill = bin), colour = "#ffffff", linewidth = 1.2) +
    ggplot2::geom_text(ggplot2::aes(label = txt, colour = ink, fontface = ifelse(thin, "italic", "bold")), size = 3.5, family = family) +
    ggplot2::geom_point(data = th, shape = 16, size = 1.1, position = ggplot2::position_nudge(x = 0.36, y = 0.28),
                        colour = ifelse(th$tri_all >= 15, "#ffffff", TFE_INK[["muted"]])) +
    ggplot2::annotate("rect", xmin = 4.5, xmax = 5.5, ymin = 0.5, ymax = length(techs) + 0.5, fill = NA, colour = TFE_INK[["muted"]],
                      linewidth = 0.35, linetype = "22") +
    ggplot2::scale_fill_manual(values = TFE_TRI_BINS, drop = FALSE, name = "Trade Restrictiveness Index (TRI)") +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_discrete(limits = TFE_STAGES, position = "top", labels = c(TFE_STAGE_AXIS[1:4], Deployment = "Deploy-\nment*", Recovery = "Recovery")) +
    ggplot2::scale_y_discrete(limits = rev(techs), labels = TFE_TECH_LABEL) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, title.position = "top", keywidth = grid::unit(1.5, "lines"),
                                                 keyheight = grid::unit(0.75, "lines"))) +
    tfe_theme(family = family) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.text.x.top = ggplot2::element_text(size = 9.2, colour = TFE_INK[["primary"]], lineheight = 0.9),
                   axis.text.y = ggplot2::element_text(size = 10, colour = TFE_INK[["primary"]]), legend.position = "top",
                   legend.justification = "left", legend.title = ggplot2::element_text(size = 8.5, colour = TFE_INK[["secondary"]]),
                   legend.text = ggplot2::element_text(size = 8.5, colour = TFE_INK[["secondary"]]), legend.margin = ggplot2::margin(0, 0, 4, 0))
}

# ---- chart 3: who gets through the gate --------------------------------------------------------------------------
tfe_chart_03 <- function(d, groups, family = "") {
  d <- d[d$plotted %in% TRUE, ]
  d$col <- unname(TFE_BLOC[d$group])
  d$size_bn <- d$two_way_trade_2024_usd / 1e9
  lab <- d[d$label %in% TRUE, ]
  g <- groups[groups$group %in% c("Trusted", "Nonaligned", "Coercive", "Coercive excl. China"), ]
  g$col <- unname(TFE_BLOC[g$group])
  g$txt <- paste0(c(Trusted = "Trusted", Nonaligned = "Nonaligned", Coercive = "Coercive", `Coercive excl. China` = "Excl. China")[g$group],
                  " ", tfe_pct(g$effective_rate))
  g$lty <- ifelse(g$group == "Coercive excl. China", "dotted", "dashed")
  ymax <- max(d$effective_rate, na.rm = TRUE) * 1.08
  g$ylab <- tfe_spread(g$effective_rate, ymax * 0.045)
  ggplot2::ggplot(d, ggplot2::aes(x = s_partner, y = effective_rate)) +
    ggplot2::geom_hline(data = g, ggplot2::aes(yintercept = effective_rate), colour = g$col, linewidth = 0.45, linetype = g$lty) +
    ggplot2::geom_text(data = g, ggplot2::aes(x = 1.03, y = ylab, label = txt), colour = TFE_INK[["secondary"]], hjust = 0,
                       size = 2.6, lineheight = 0.85, family = family) +
    ggplot2::geom_point(ggplot2::aes(size = size_bn), shape = 21, fill = d$col, colour = "#ffffff", stroke = 0.5, alpha = 0.85) +
    ggrepel::geom_text_repel(data = lab, ggplot2::aes(label = name), size = 3.1, family = family, colour = TFE_INK[["primary"]],
                             min.segment.length = 0.2, segment.colour = TFE_INK[["muted"]], segment.size = 0.25, box.padding = 0.4,
                             point.padding = 0.3, seed = 7, max.overlaps = 50) +
    ggplot2::scale_size_area(max_size = 15, guide = "none") +
    ggplot2::scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                                labels = c("0\nvotes with\nChina", "0.25", "0.5", "0.75", "1\nvotes with\nthe US")) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
    ggplot2::coord_cartesian(xlim = c(-0.02, 1.0), ylim = c(0, ymax), clip = "off") +
    ggplot2::labs(x = "Position on the US-China axis of UN General Assembly votes", y = "Effective tariff paid, June-August 2026") +
    tfe_theme(family = family) +
    ggplot2::theme(plot.margin = ggplot2::margin(14, 70, 10, 14))
}

# ---- chart 4: multi-tool fence -------------------------------------------------------------------------------------
TFE_TOOL_AXIS <- c(Tariffs = "Tariffs", `AD/CVD` = "AD/\nCVD", `Import bans` = "Import\nbans", `Quotas / licensing` = "Quotas,\nlicences",
                   `Local-content rules` = "Local-\ncontent\nrules", `Local-content incentives` = "Local-\ncontent\nincentives",
                   `Public purchasing` = "Public\npurch-\nasing", `Investment restrictions` = "Invest-\nment\nlimits",
                   `Export controls` = "Export\ncontrols")
tfe_chart_04 <- function(d, family = "", dec4_flags = NULL) {
  tools <- names(TFE_TOOL_AXIS)
  levels_y <- rev(c(" ", "Extraction", "Refining", "Components", "Assembly / Deployment", "Recovery"))
  d$tool_f <- factor(d$tool, tools)
  d$stage_f <- factor(d$stage, levels_y)
  techs <- c("Electric grid", "Solar", "Batteries")
  d$tech_f <- factor(c(`Electric Grid` = "Electric grid", Solar = "Solar", Batteries = "Batteries")[d$tech], techs)
  d$band_f <- factor(d$band, c("Low", "Medium", "High"))
  d$txt <- paste0(substr(d$band, 1, 1), ifelse(d$single_measure %in% TRUE, "†", ""))
  d$ink <- ifelse(d$band %in% "High", "#ffffff", TFE_INK[["primary"]])
  hdr <- data.frame(tech_f = factor("Electric grid", techs), stage_f = factor(" ", levels_y), x = c(2.5, 6, 8.5),
                    label = c("At the border", "In the domestic market", "Capital, technology"))
  hdr_tick <- data.frame(tech_f = factor("Electric grid", techs), stage_f = factor(" ", levels_y), tool_f = factor(tools, tools))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = tool_f, y = stage_f)) +
    ggplot2::geom_blank(data = hdr_tick) +
    ggplot2::geom_tile(ggplot2::aes(fill = band_f), colour = "#ffffff", linewidth = 1.1) +
    ggplot2::geom_text(ggplot2::aes(label = txt, colour = ink), size = 3.0, fontface = "bold", family = family) +
    ggplot2::geom_text(data = hdr, ggplot2::aes(x = x, y = stage_f, label = label), inherit.aes = FALSE, size = 2.9, fontface = "bold",
                       colour = TFE_INK[["secondary"]], family = family, vjust = 0.3, lineheight = 0.85)
  if (!is.null(dec4_flags) && nrow(dec4_flags)) {
    f <- merge(d[c("tech", "stage", "tool", "tool_f", "stage_f", "tech_f")], dec4_flags, by = c("tech", "stage", "tool"))
    p <- p + ggplot2::geom_point(data = f, shape = 17, size = 1.7, colour = "#d03b3b", position = ggplot2::position_nudge(x = 0.33, y = 0.26))
  }
  p +
    ggplot2::facet_wrap(~tech_f, ncol = 1, scales = "free_y") +
    ggplot2::geom_vline(xintercept = c(4.5, 7.5), colour = TFE_INK[["secondary"]], linewidth = 0.4) +
    ggplot2::scale_fill_manual(values = TFE_BAND_COL, drop = FALSE, name = "Fence height against the same tool across all US imports",
                               na.translate = FALSE) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_discrete(position = "top", labels = TFE_TOOL_AXIS, drop = FALSE) +
    ggplot2::scale_y_discrete(limits = function(x) levels_y[levels_y %in% x]) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, title.position = "top", keywidth = grid::unit(1.4, "lines"),
                                                 keyheight = grid::unit(0.75, "lines"))) +
    tfe_theme(family = family) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.text.x.top = ggplot2::element_text(size = 8, lineheight = 0.88, colour = TFE_INK[["primary"]], vjust = 0),
                   axis.text.y = ggplot2::element_text(size = 8.8, colour = TFE_INK[["primary"]]), legend.position = "top", legend.justification = "left",
                   legend.title = ggplot2::element_text(size = 8.3), legend.text = ggplot2::element_text(size = 8.3),
                   strip.text = ggplot2::element_text(margin = ggplot2::margin(t = 6, b = 3)), panel.spacing.y = grid::unit(0.5, "lines"))
}

# ---- chart 5: Yards & Fences audit -----------------------------------------------------------------------------------
tfe_chart_05 <- function(d, techs, family = "", symbol_family = family) {
  d$tech_f <- factor(d$tech, rev(techs))
  d$stage_f <- factor(d$stage, TFE_STAGES)
  ok <- !is.na(d$verdict4) & nzchar(d$verdict4)
  dd <- d[ok, ]
  na <- d[!ok, ]
  na$txt <- ifelse(na$in_basket %in% TRUE | na$strategic_choice %in% "not_assessed", "not\nassessed", "–")
  dd$glyph <- unname(TFE_VERDICT_GLYPH[dd$verdict4])
  ch <- if ("verdict_changed_dec4" %in% names(dd)) dd[dd$verdict_changed_dec4 %in% TRUE, ] else dd[0, ]
  p <- ggplot2::ggplot(dd, ggplot2::aes(x = stage_f, y = tech_f)) +
    ggplot2::geom_tile(data = na, fill = TFE_INK[["faint"]], colour = "#ffffff", linewidth = 1.2) +
    ggplot2::geom_text(data = na, ggplot2::aes(label = txt), colour = "#9ca3af", size = 2.3, lineheight = 0.85, family = family) +
    ggplot2::geom_tile(ggplot2::aes(fill = verdict4), colour = "#ffffff", linewidth = 1.2) +
    ggplot2::geom_text(ggplot2::aes(label = glyph), colour = "#ffffff", size = 3.0, nudge_y = 0.17, family = symbol_family) +
    ggplot2::geom_text(ggplot2::aes(label = choice_label), colour = "#ffffff", size = 2.45, nudge_y = -0.17, family = family, fontface = "bold")
  if (nrow(ch)) p <- p + ggplot2::geom_tile(data = ch, fill = NA, colour = TFE_INK[["primary"]], linewidth = 0.9, width = 0.9, height = 0.88)
  p +
    ggplot2::scale_fill_manual(values = TFE_VERDICT_COL, breaks = names(TFE_VERDICT_COL), drop = FALSE,
                               labels = paste(TFE_VERDICT_GLYPH[names(TFE_VERDICT_COL)], names(TFE_VERDICT_COL))) +
    ggplot2::scale_x_discrete(limits = TFE_STAGES, position = "top", labels = TFE_STAGE_AXIS) +
    ggplot2::scale_y_discrete(limits = rev(techs), labels = TFE_TECH_LABEL) +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 2, byrow = TRUE, keywidth = grid::unit(0.9, "lines"), keyheight = grid::unit(0.8, "lines"))) +
    tfe_theme(family = family) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.text.x.top = ggplot2::element_text(size = 9.2, colour = TFE_INK[["primary"]], lineheight = 0.9),
                   axis.text.y = ggplot2::element_text(size = 10, colour = TFE_INK[["primary"]]), legend.position = "top", legend.justification = "left",
                   legend.text = ggplot2::element_text(size = 8.6, colour = TFE_INK[["secondary"]], family = symbol_family), legend.margin = ggplot2::margin(0, 0, 4, 0))
}

tfe_chart_05b <- function(d, family = "", symbol_family = family, col = "cells_observed") {
  d$n <- d[[col]]
  d$v <- factor(d$verdict, rev(names(TFE_VERDICT_COL)))
  d$txt <- paste0(TFE_VERDICT_GLYPH[d$verdict], "  ", d$verdict)
  ggplot2::ggplot(d, ggplot2::aes(y = v, x = n)) +
    ggplot2::geom_col(ggplot2::aes(fill = verdict), width = 0.55) +
    ggplot2::geom_text(ggplot2::aes(label = n), hjust = -0.35, size = 5.2, fontface = "bold", colour = TFE_INK[["primary"]], family = family) +
    ggplot2::geom_text(ggplot2::aes(x = 0, label = txt), hjust = 0, vjust = -1.6, size = 3.6, colour = TFE_INK[["primary"]], family = symbol_family) +
    ggplot2::scale_fill_manual(values = TFE_VERDICT_COL) +
    ggplot2::scale_x_continuous(limits = c(0, max(d$n) * 1.18), expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = c(0.4, 0.9))) +
    ggplot2::labs(x = NULL, y = NULL) +
    tfe_theme(base = 10.5, family = family) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.text = ggplot2::element_blank())
}

# ---- chart 6: dollar-weighted --------------------------------------------------------------------------------------
tfe_chart_06 <- function(d, family = "") {
  bars <- c(`All suppliers` = "All US imports in the assessed basket", `Trusted partners` = "Imports from trusted partners",
            `All suppliers (excl. oil, gas, coal)` = "All imports, clean-energy and grid technologies only (excl. oil, gas, coal)")
  d <- d[d$suppliers %in% names(bars), ]
  d$panel <- factor(paste0(bars[d$suppliers], ", 2024: ", tfe_bn(d$basis_total_usd)), unique(paste0(bars[d$suppliers], ", 2024: ", tfe_bn(d$basis_total_usd))))
  d$v <- factor(d$verdict, names(TFE_VERDICT_COL))
  d <- d[order(d$panel, d$v), ]
  d$lab <- ifelse(d$share >= 0.08, paste0(round(100 * d$share), "%\n", tfe_bn(d$imports_usd)), ifelse(d$share >= 0.02, paste0(round(100 * d$share), "%"), ""))
  d$ink <- ifelse(d$verdict == "Fence above recommendation", TFE_INK[["primary"]], "#ffffff")
  d$bar <- factor("bar")
  ggplot2::ggplot(d, ggplot2::aes(y = bar, x = share, fill = v, group = v)) +
    ggplot2::geom_col(width = 0.8, colour = "#ffffff", linewidth = 0.6, orientation = "y", position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_text(ggplot2::aes(label = lab, colour = ink), position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE), size = 2.9,
                       lineheight = 0.95, family = family, fontface = "bold") +
    ggplot2::facet_wrap(~panel, ncol = 1) +
    ggplot2::scale_fill_manual(values = TFE_VERDICT_COL, breaks = names(TFE_VERDICT_COL), drop = FALSE,
                               labels = paste(TFE_VERDICT_GLYPH[names(TFE_VERDICT_COL)], names(TFE_VERDICT_COL))) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0, 1.001)) +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 2, byrow = TRUE, keywidth = grid::unit(0.9, "lines"), keyheight = grid::unit(0.8, "lines"))) +
    tfe_theme(family = family) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.text = ggplot2::element_blank(), legend.position = "top", legend.justification = "left",
                   legend.text = ggplot2::element_text(size = 8.6), strip.text = ggplot2::element_text(face = "plain", size = 9.4, margin = ggplot2::margin(t = 8, b = 3)))
}

# ---- chart 7: why allies pay -----------------------------------------------------------------------------------------
TFE_SCENARIO_STYLE <- data.frame(
  scenario = c("Dec 4 statutory (all measures)", "Without Section 232", "Without Section 301", "Without Section 338", "Without AD/CVD",
               "2024 rates (pre-2025 baseline)"),
  short = c("All measures, 4 Dec 2026", "Without §232 (metals, autos, polysilicon)", "Without §301", "Without §338 (Canada)",
            "Without AD/CVD", "2024 duty rates"),
  shape = c(21, 21, 22, 23, 24, 21), fill = c("#111827", "#ffffff", "#ffffff", "#ffffff", "#ffffff", "#d1d5db"),
  colour = c("#111827", "#f58229", "#1889b0", "#6b7280", "#7b4fbf", "#9ca3af"),
  offset = c(0, -0.2, 0.12, 0.2, -0.12, 0), stringsAsFactors = FALSE)
tfe_chart_07 <- function(d, family = "") {
  d$cell <- paste(TFE_TECH_LABEL[d$tech], tolower(d$stage))
  ord <- unique(d$cell)
  d$cell_f <- factor(d$cell, rev(ord))
  s <- TFE_SCENARIO_STYLE[TFE_SCENARIO_STYLE$scenario %in% d$scenario, ]
  d$sc_f <- factor(d$scenario, s$scenario)
  d$y <- as.numeric(d$cell_f) + s$offset[match(d$scenario, s$scenario)]
  rng <- stats::aggregate(tri ~ cell_f, data = d, FUN = range)
  rng <- data.frame(y = as.numeric(rng$cell_f), lo = rng$tri[, 1], hi = rng$tri[, 2])
  full <- d[d$scenario == s$scenario[1], ]
  obs <- unique(d[c("cell_f", "observed_aug2026_trusted_tri")])
  ggplot2::ggplot(d) +
    ggplot2::geom_segment(data = rng, ggplot2::aes(x = lo, xend = hi, y = y, yend = y), colour = TFE_INK[["grid"]], linewidth = 2.4) +
    ggplot2::geom_point(data = obs, ggplot2::aes(x = observed_aug2026_trusted_tri, y = as.numeric(cell_f)), shape = 124, size = 6, colour = TFE_INK[["muted"]]) +
    ggplot2::geom_point(ggplot2::aes(x = tri, y = y, shape = sc_f, fill = sc_f, colour = sc_f), size = 2.9, stroke = 1.05) +
    ggplot2::geom_text(data = full, ggplot2::aes(x = rng$hi[match(as.numeric(cell_f), rng$y)] + 0.8, y = as.numeric(cell_f), label = tfe_pct(tri)),
                       hjust = 0, size = 2.9, fontface = "bold", colour = TFE_INK[["primary"]], family = family) +
    ggplot2::scale_shape_manual(values = stats::setNames(s$shape, s$scenario), labels = s$short, name = NULL) +
    ggplot2::scale_fill_manual(values = stats::setNames(s$fill, s$scenario), labels = s$short, name = NULL) +
    ggplot2::scale_colour_manual(values = stats::setNames(s$colour, s$scenario), labels = s$short, name = NULL) +
    ggplot2::scale_y_continuous(breaks = seq_along(levels(d$cell_f)), labels = levels(d$cell_f), expand = ggplot2::expansion(add = 0.5)) +
    ggplot2::scale_x_continuous(limits = c(0, max(d$tri) * 1.13), labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
    ggplot2::labs(x = "Trade Restrictiveness Index on trusted-partner imports", y = NULL) +
    ggplot2::guides(shape = ggplot2::guide_legend(ncol = 2, byrow = TRUE)) +
    tfe_theme(family = family) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_text(size = 9.6, colour = TFE_INK[["primary"]]),
                   legend.position = "top", legend.justification = "left", legend.text = ggplot2::element_text(size = 8.4),
                   legend.key.height = grid::unit(0.9, "lines"))
}

# ---- chart 8: trade response ---------------------------------------------------------------------------------------
TFE_MODULE_ORIGINS <- c(Vietnam = "#016787", Thailand = "#2792ba", Malaysia = "#5cbfe8", Cambodia = "#b4e7fe",
                        India = "#c2410c", Indonesia = "#f58229", Laos = "#fbbf8a", Philippines = "#55c4c5", Africa = "#a3b63b",
                        China = "#7f1d1d", `All other` = "#d1d5db")
tfe_chart_08a <- function(m, events, family = "", end_date = as.Date("2026-12-31")) {
  m$date <- as.Date(paste0(m$time, "-01"))
  pal <- TFE_MODULE_ORIGINS[names(TFE_MODULE_ORIGINS) %in% m$origin]
  m$origin_f <- factor(m$origin, names(pal))
  ev <- events
  ev$date <- as.Date(ev$date)
  ymax <- max(stats::aggregate(value ~ date, data = m, FUN = sum)$value) / 1e9 * 1.12
  ev$n <- seq_len(nrow(ev))
  ggplot2::ggplot(m, ggplot2::aes(x = date, y = value / 1e9)) +
    ggplot2::geom_col(ggplot2::aes(fill = origin_f), width = 26, colour = "#ffffff", linewidth = 0.12) +
    ggplot2::geom_vline(data = ev, ggplot2::aes(xintercept = date), colour = TFE_INK[["secondary"]], linewidth = 0.35,
                        linetype = ifelse(ev$future %in% TRUE, "22", "solid")) +
    ggplot2::geom_label(data = ev, ggplot2::aes(x = date, y = ymax * 0.97, label = n), size = 2.6, family = family, fontface = "bold",
                        label.size = 0.25, label.r = grid::unit(0.5, "lines"), fill = "#ffffff", colour = TFE_INK[["primary"]],
                        label.padding = grid::unit(0.12, "lines")) +
    ggplot2::scale_fill_manual(values = pal, name = NULL) +
    ggplot2::scale_x_date(limits = c(as.Date("2023-12-10"), end_date), date_breaks = "6 months", date_labels = "%b\n%Y", expand = c(0, 0)) +
    ggplot2::scale_y_continuous(labels = function(x) paste0("$", x, "bn"), expand = c(0, 0), limits = c(0, ymax)) +
    ggplot2::labs(x = NULL, y = "US imports per month") +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 3, byrow = TRUE, keywidth = grid::unit(0.7, "lines"), keyheight = grid::unit(0.6, "lines"))) +
    tfe_theme(family = family) +
    ggplot2::theme(legend.position = "top", legend.justification = "left", legend.text = ggplot2::element_text(size = 7.8),
                   panel.grid.major.x = ggplot2::element_blank(), legend.margin = ggplot2::margin(0, 0, 0, 0))
}

tfe_chart_08b <- function(d, family = "") {
  ord <- c("China", "Korea", "Japan", "Malaysia", "Thailand", "Vietnam", "All other")
  d <- d[d$origin %in% ord, ]
  long <- rbind(data.frame(origin = d$origin, period = "Jan-Aug 2024", value = d$value_jan_aug_2024, share = d$share_2024),
                data.frame(origin = d$origin, period = "Jan-Aug 2026", value = d$value_jan_aug_2026, share = d$share_2026))
  long$origin_f <- factor(long$origin, rev(ord))
  long$period_f <- factor(long$period, c("Jan-Aug 2026", "Jan-Aug 2024"))
  pal <- c(`Jan-Aug 2024` = "#b4e7fe", `Jan-Aug 2026` = "#016787")
  long$lab <- tfe_bn(long$value, 1)
  sh <- data.frame(origin_f = factor(d$origin, rev(ord)), txt = sprintf("%s%% → %s%%", round(100 * d$share_2024), round(100 * d$share_2026)))
  xmax <- max(long$value) / 1e9
  ggplot2::ggplot(long, ggplot2::aes(y = origin_f, x = value / 1e9, fill = period_f)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.74) +
    ggplot2::geom_text(ggplot2::aes(label = lab), position = ggplot2::position_dodge(width = 0.8), hjust = -0.1, size = 2.4,
                       colour = TFE_INK[["secondary"]], family = family) +
    ggplot2::geom_text(data = sh, ggplot2::aes(y = origin_f, x = xmax * 1.32, label = txt), inherit.aes = FALSE, hjust = 0, size = 2.8,
                       fontface = "bold", colour = TFE_INK[["primary"]], family = family) +
    ggplot2::annotate("text", x = xmax * 1.32, y = length(ord) + 0.62, label = "Share of total", hjust = 0, size = 2.6, colour = TFE_INK[["muted"]], family = family) +
    ggplot2::scale_fill_manual(values = pal, name = NULL, breaks = c("Jan-Aug 2024", "Jan-Aug 2026")) +
    ggplot2::scale_x_continuous(labels = function(x) paste0("$", x, "bn"), limits = c(0, xmax * 1.62), breaks = pretty(c(0, xmax), 4),
                                expand = c(0, 0)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = "US lithium-ion imports, January-August", y = NULL) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1)) +
    tfe_theme(family = family) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), legend.position = "top", legend.justification = "left",
                   axis.text.y = ggplot2::element_text(size = 9.4, colour = TFE_INK[["primary"]]))
}

# Small companion panel: US module factories' operating capacity, from the reference file.
tfe_chart_08_domestic <- function(d, family = "") {
  d$period_f <- factor(d$label, d$label)
  ggplot2::ggplot(d, ggplot2::aes(x = period_f, y = value)) +
    ggplot2::geom_col(fill = "#1889b0", width = 0.62) +
    ggplot2::geom_text(ggplot2::aes(label = formatC(value, format = "f", digits = 0)), vjust = -0.4, size = 2.8, family = family, colour = TFE_INK[["primary"]]) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(x = NULL, y = NULL) +
    tfe_theme(family = family) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_text(size = 7.6, lineheight = 0.9),
                   plot.title = ggplot2::element_text(size = 9.6, face = "bold"), plot.subtitle = ggplot2::element_text(size = 7.8))
}
