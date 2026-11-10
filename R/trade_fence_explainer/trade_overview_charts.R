# Charts of the opening section of the trade-fence explainer (00a trade overview, 00b rewiring, 00c policy
# timeline, analysis/fence_change_vs_diversification). Pure: data frames in, ggplots out. Uses the visual system of
# explainer_charts.R (TFE_INK, tfe_theme, tfe_labs, tfe_bn, TFE_TECH_LABEL).

TFE_FLOW_COL <- c(Imports = "#016787", Exports = "#a3b63b")
TFE_CAUSAL_NOTE <- "Policy events are shown for context; changes in trade flows cannot be attributed to individual measures from these descriptive charts alone."

tfe_bn_short <- function(x) ifelse(abs(x) >= 1e10, sprintf("$%.0fbn", x / 1e9),
                             ifelse(abs(x) >= 1e9, sprintf("$%.1fbn", x / 1e9), sprintf("$%.0fm", x / 1e6)))

# Panel A: imports and exports by technology (latest period), clean energy and grid apart from fuels, with the
# net balance written at the right of each row.
tfe_chart_00a_tech <- function(bt, family = "") {
  d <- bt[!is.na(bt$imports_2026), ]
  ord <- d$tech[order(d$group, d$imports_2026)]
  long <- rbind(data.frame(tech = d$tech, group = d$group, flow = "Imports", value = d$imports_2026),
                data.frame(tech = d$tech, group = d$group, flow = "Exports", value = d$exports_2026))
  long$tech_f <- factor(long$tech, ord)
  long$flow_f <- factor(long$flow, c("Exports", "Imports"))
  mx <- stats::ave(pmax(d$imports_2026, d$exports_2026), d$group, FUN = max)
  net <- data.frame(tech_f = factor(d$tech, ord), group = d$group, x = mx * 1.36,
                    lab = paste0(ifelse(d$balance_2026 < 0, "−", "+"), tfe_bn_short(abs(d$balance_2026))))
  long$lab <- tfe_bn_short(long$value)
  ggplot2::ggplot(long, ggplot2::aes(y = tech_f, x = value / 1e9, fill = flow_f)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.78), width = 0.72) +
    ggplot2::geom_text(ggplot2::aes(label = lab), position = ggplot2::position_dodge(width = 0.78), hjust = -0.12, size = 2.45,
                       colour = TFE_INK[["secondary"]], family = family) +
    ggplot2::geom_text(data = net, ggplot2::aes(y = tech_f, x = x / 1e9, label = lab), inherit.aes = FALSE, hjust = 0, size = 2.7,
                       fontface = "bold", colour = TFE_INK[["primary"]], family = family) +
    ggplot2::facet_wrap(~group, ncol = 1, scales = "free") +
    ggplot2::scale_fill_manual(values = TFE_FLOW_COL, breaks = c("Imports", "Exports"), name = NULL) +
    ggplot2::scale_y_discrete(labels = TFE_TECH_LABEL) +
    ggplot2::scale_x_continuous(labels = function(x) paste0("$", x, "bn"), expand = ggplot2::expansion(mult = c(0, 0.55))) +
    ggplot2::labs(x = NULL, y = NULL) +
    tfe_theme(family = family) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), legend.position = "top", legend.justification = "left",
                   strip.text = ggplot2::element_text(hjust = 0, face = "bold", size = 8.8, margin = ggplot2::margin(t = 4, b = 2)),
                   axis.text.y = ggplot2::element_text(size = 9.2, colour = TFE_INK[["primary"]]), panel.spacing.y = grid::unit(0.8, "lines"))
}

# Panel B: imports and exports by stage, all technologies and clean energy and grid only.
tfe_chart_00a_stage <- function(bs, family = "") {
  st <- rev(c("Extraction", "Refining", "Components", "Assembly", "Recovery"))
  long <- rbind(data.frame(basket = bs$basket, stage = bs$stage, flow = "Imports", value = bs$imports_2026),
                data.frame(basket = bs$basket, stage = bs$stage, flow = "Exports", value = bs$exports_2026))
  long$stage_f <- factor(long$stage, st)
  long$flow_f <- factor(long$flow, c("Exports", "Imports"))
  long$basket_f <- factor(sub(" \\(excl. oil, gas, coal\\)", "\n(excl. oil, gas, coal)", long$basket),
                          unique(sub(" \\(excl. oil, gas, coal\\)", "\n(excl. oil, gas, coal)", bs$basket)))
  long$lab <- tfe_bn_short(long$value)
  ggplot2::ggplot(long, ggplot2::aes(y = stage_f, x = value / 1e9, fill = flow_f)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.78), width = 0.72) +
    ggplot2::geom_text(ggplot2::aes(label = lab), position = ggplot2::position_dodge(width = 0.78), hjust = -0.12, size = 2.35,
                       colour = TFE_INK[["secondary"]], family = family) +
    ggplot2::facet_wrap(~basket_f, nrow = 1, scales = "free_x") +
    ggplot2::scale_fill_manual(values = TFE_FLOW_COL, breaks = c("Imports", "Exports"), name = NULL, guide = "none") +
    ggplot2::scale_x_continuous(labels = function(x) paste0("$", x, "bn"), expand = ggplot2::expansion(mult = c(0, 0.3)), n.breaks = 3) +
    ggplot2::labs(x = NULL, y = NULL) +
    tfe_theme(family = family) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), strip.text = ggplot2::element_text(size = 8.4, face = "bold", lineheight = 0.95),
                   axis.text.y = ggplot2::element_text(size = 9, colour = TFE_INK[["primary"]]), panel.spacing.x = grid::unit(1.2, "lines"))
}

# Chart 00b: import growth x change in China's share, bubbles sized by latest-period imports.
tfe_chart_00b <- function(b, family = "") {
  b$label <- ifelse(is.na(b$label), b$tech, b$label)
  b$size <- b$imports_2026 / 1e9
  xl <- range(c(b$imports_change_pct, 0), na.rm = TRUE) + c(-15, 15)
  yl <- c(min(b$china_share_change_pts, 0, na.rm = TRUE) - 9, max(b$china_share_change_pts, 0, na.rm = TRUE) + 10)
  q <- data.frame(x = c(xl[2], xl[1], xl[2], xl[1]), y = c(yl[1], yl[1], yl[2], yl[2]), hj = c(1, 0, 1, 0), vj = c(0, 0, 1, 1),
                  lab = c("Diversification during expansion", "Diversification through\ncontraction / substitution",
                          "Deepening dependence", "Concentration despite contraction"))
  ggplot2::ggplot(b, ggplot2::aes(x = imports_change_pct, y = china_share_change_pts)) +
    ggplot2::geom_hline(yintercept = 0, colour = TFE_INK[["muted"]], linewidth = 0.4) +
    ggplot2::geom_vline(xintercept = 0, colour = TFE_INK[["muted"]], linewidth = 0.4) +
    ggplot2::geom_text(data = q, ggplot2::aes(x = x, y = y, label = lab, hjust = hj, vjust = vj), inherit.aes = FALSE, size = 2.6,
                       colour = "#9ca3af", fontface = "italic", lineheight = 0.9, family = family) +
    ggplot2::geom_point(ggplot2::aes(size = size, fill = kind, shape = kind), colour = "#ffffff", stroke = 0.5, alpha = 0.88) +
    ggrepel::geom_text_repel(ggplot2::aes(label = label), size = 2.9, family = family, colour = TFE_INK[["primary"]], seed = 3,
                             box.padding = 0.45, point.padding = 0.3, min.segment.length = 0.2, segment.colour = TFE_INK[["muted"]], segment.size = 0.25) +
    ggplot2::scale_size_area(max_size = 13, guide = "none") +
    ggplot2::scale_fill_manual(values = c(Technology = "#1889b0", `Product group` = "#55c4c5"), name = NULL) +
    ggplot2::scale_shape_manual(values = c(Technology = 21, `Product group` = 22), name = NULL) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "%"), limits = xl, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x, " pts"), limits = yl, expand = c(0, 0)) +
    ggplot2::labs(x = "Change in US import value, January-August 2024 to 2026", y = "Change in China's share of US imports") +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(size = 4)), shape = ggplot2::guide_legend(override.aes = list(size = 4))) +
    tfe_theme(family = family) +
    ggplot2::theme(legend.position = "top", legend.justification = "left")
}

# Chart 00c: a slim timeline of the key measures, with the two observed windows shaded.
tfe_chart_00c <- function(tl, windows, family = "") {
  k <- tl[toupper(tl$key_event) == "TRUE", ]
  k$date <- as.Date(k$date)
  k <- k[order(k$date), ]
  k$side <- rep(c(1, -1), length.out = nrow(k))
  k$level <- k$side * (0.55 + 0.42 * ((seq_len(nrow(k)) - 1) %/% 2 %% 2))
  k$future <- k$date > as.Date(windows$latest)
  w <- data.frame(xmin = as.Date(windows$start), xmax = as.Date(windows$end), lab = windows$label)
  ggplot2::ggplot(k) +
    ggplot2::annotate("rect", xmin = w$xmin, xmax = w$xmax, ymin = -1.35, ymax = 1.35, fill = "#eef7fc") +
    ggplot2::annotate("text", x = w$xmin + (w$xmax - w$xmin) / 2, y = -1.5, label = w$lab, size = 2.5, colour = TFE_INK[["muted"]], family = family) +
    ggplot2::geom_hline(yintercept = 0, colour = TFE_INK[["secondary"]], linewidth = 0.5) +
    ggplot2::geom_segment(ggplot2::aes(x = date, xend = date, y = 0, yend = level), colour = TFE_INK[["muted"]], linewidth = 0.3,
                          linetype = ifelse(k$future, "22", "solid")) +
    ggplot2::geom_point(ggplot2::aes(x = date, y = 0), size = 1.8, colour = TFE_INK[["primary"]]) +
    ggplot2::geom_label(ggplot2::aes(x = date, y = level, label = gsub(": ", ":\n", short_label)), size = 2.3, family = family,
                        label.size = 0, fill = "#ffffffee", colour = TFE_INK[["primary"]], lineheight = 0.88, label.padding = grid::unit(0.12, "lines")) +
    ggplot2::scale_x_date(limits = as.Date(c("2024-01-01", "2027-01-31")), date_breaks = "6 months", date_labels = "%b %Y", expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(-1.65, 1.35)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = NULL, y = NULL) +
    tfe_theme(family = family) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(), axis.text.x = ggplot2::element_text(size = 8),
                   plot.margin = ggplot2::margin(14, 30, 10, 18))
}

# Analysis: change in all-supplier TRI against change in coercive-supplier share, by technology x stage.
tfe_chart_fence_vs_div <- function(d, fence_line = 10, dep_line = -5, family = "") {
  d$cell <- paste(TFE_TECH_LABEL[d$tech], tolower(d$stage))
  d$size <- d$imports_2024 / 1e9
  lab <- d[d$imports_2024 >= 5e9 | abs(d$coercive_share_change_pts) >= 15, ]
  ggplot2::ggplot(d, ggplot2::aes(x = tri_change_pts, y = coercive_share_change_pts)) +
    ggplot2::geom_vline(xintercept = fence_line, linetype = "22", colour = TFE_INK[["muted"]], linewidth = 0.35) +
    ggplot2::geom_hline(yintercept = dep_line, linetype = "22", colour = TFE_INK[["muted"]], linewidth = 0.35) +
    ggplot2::geom_hline(yintercept = 0, colour = TFE_INK[["grid"]], linewidth = 0.4) +
    ggplot2::geom_point(ggplot2::aes(size = size), shape = 21, fill = "#1889b0", colour = "#ffffff", alpha = 0.8) +
    ggrepel::geom_text_repel(data = lab, ggplot2::aes(label = cell), size = 2.6, family = family, colour = TFE_INK[["primary"]], seed = 5,
                             max.overlaps = 40, min.segment.length = 0.2, segment.colour = TFE_INK[["muted"]], segment.size = 0.25) +
    ggplot2::scale_size_area(max_size = 14, guide = "none") +
    ggplot2::scale_x_continuous(labels = function(x) paste0("+", x, " pts")) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x, " pts")) +
    ggplot2::labs(x = "Change in Trade Restrictiveness Index, all suppliers (2024 rates to August 2026, Census duty)",
                  y = "Change in coercive-supplier share of imports\n(January-August 2024 to 2026)") +
    tfe_theme(family = family)
}
