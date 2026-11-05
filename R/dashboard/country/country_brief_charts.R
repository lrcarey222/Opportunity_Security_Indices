# The two required figures of a country strategy brief, as ggplot objects (pure: tables in, plots out;
# scripts/92_export_country_briefs.R saves them as SVG for the screen and the PDF, PNG for Word):
#
#   strategy_map    the technology x six-stage Strategy Map of the Country page, simplified for print: each
#                   step in its choice's colour with the choice written in, a corner flag in the colour of
#                   the other fence where the step's parts sit behind more than one ("Mixed High/Medium"),
#                   and a legend grouped by fence (High: Secure; Medium: Preserve, Catch Up, Partner,
#                   Accelerate; Low: Watch).
#   value_position  Strategic Industrial Value (x) against Competitive Position (y) for the producing steps
#                   with both measures, marked by choice, Secure (or holding a Secure layer) ringed in orange,
#                   the prize (60) and position (75) bars dashed, the most important steps labelled.
# Review cases (country_brief_review(), 2026-11-03: low confidence, a review flag, a thin anchor, a data
# artifact, a single-indicator deployment need) are marked on every figure and never take a headline mark: a
# dotted outline on the map, a hollow, unlabelled marker on the scatter, a separate grey group on the
# deployment chart.

COUNTRY_BRIEF_CHOICE_LABEL <- c(secure = "Secure", preserve = "Preserve", catch_up = "Catch Up", partner = "Partner", accelerate = "Accelerate",
                                watch = "Watch", not_assessed = "Not assessed")
COUNTRY_BRIEF_MIXED_SHORT <- c(mixed_high_medium = "Mixed H/M", mixed_medium_low = "Mixed M/L", mixed_high_low = "Mixed H/L", mixed_high_medium_low = "Mixed H/M/L")

# The yards of a step's parts other than its own choice's (the corner-flag colours), from its layers.
country_brief_other_yards <- function(node, own_choice, layers) {
  own <- unname(COUNTRY_BRIEF_YARD[own_choice])
  y <- unique(stats::na.omit(layers$yard[layers$node == node]))
  setdiff(intersect(c("small", "medium", "large"), y), own)
}

# The Strategy Map. `cells`, `layers`: country_brief_chart_data(); `stage_display`: meta.stage_display.
country_brief_map_plot <- function(cells, layers, cfg, techs, stages, stage_display, base_family = "sans") {
  cc <- cfg$charts
  col <- unlist(cc$colors)
  txt <- unlist(cc$text_on)
  ycol <- unlist(cc$yard_colors)
  d <- cells
  d$x <- match(d$stage, stages)
  d$y <- length(techs) + 1 - match(d$tech, techs)
  d$fill <- unname(col[d$choice])
  d$tcol <- unname(txt[d$choice])
  d$label <- unname(COUNTRY_BRIEF_CHOICE_LABEL[d$choice])
  d$label[d$choice == "not_assessed"] <- "n.a."
  d$mix <- ifelse(d$mixed, unname(COUNTRY_BRIEF_MIXED_SHORT[d$yard_rollup]), NA_character_)
  w <- 0.47
  h <- 0.44
  flags <- list()
  for (i in which(d$mixed)) {
    oth <- country_brief_other_yards(d$node[i], d$choice[i], layers)
    if (!length(oth)) next
    x1 <- d$x[i] + w; y0 <- d$y[i] - h
    fw <- 0.34; fh <- 0.5
    # one triangle in the lower-right corner, split in two when two other fences are present
    if (length(oth) == 1) {
      flags[[length(flags) + 1]] <- data.frame(id = paste0(i, "a"), x = c(x1 - fw, x1, x1), y = c(y0, y0, y0 + fh), fill = unname(ycol[oth[1]]))
    } else {
      flags[[length(flags) + 1]] <- data.frame(id = paste0(i, "a"), x = c(x1 - fw, x1, x1 - fw / 2), y = c(y0, y0, y0 + fh / 2), fill = unname(ycol[oth[1]]))
      flags[[length(flags) + 1]] <- data.frame(id = paste0(i, "b"), x = c(x1 - fw / 2, x1, x1), y = c(y0 + fh / 2, y0, y0 + fh), fill = unname(ycol[oth[2]]))
    }
  }
  fl <- if (length(flags)) do.call(rbind, flags) else data.frame(id = character(), x = numeric(), y = numeric(), fill = character())
  na <- d[d$choice == "not_assessed", , drop = FALSE]
  as <- d[d$choice != "not_assessed", , drop = FALSE]
  rv <- if (is.null(d$review)) d[0, , drop = FALSE] else d[!is.na(d$review), , drop = FALSE]
  n_t <- length(techs)
  heads <- data.frame(x = seq_along(stages), y = n_t + 0.85, label = vapply(stages, function(s) {
    lab <- unname(stage_display[[s]] %||% s)
    paste(strwrap(lab, 15), collapse = "\n")
  }, ""))
  bands <- data.frame(x = c(2.5, 5, 6), y = n_t + 1.55, label = c("Supply-chain strategy", "Energy system", "Circularity"))
  # legend rows under the grid, grouped by fence
  leg <- data.frame(
    key = c("secure", "preserve", "catch_up", "partner", "accelerate", "watch", "not_assessed"),
    lab = c("Secure", "Preserve", "Catch Up", "Partner", "Accelerate", "Watch", "Not assessed"),
    x = c(0.62, 1.82, 2.82, 3.82, 4.82, 5.72, 5.72),
    y = c(-0.35, -0.35, -0.35, -0.35, -0.35, -0.35, -1.05))
  leg$fill <- unname(col[leg$key])
  fence_lab <- data.frame(x = c(0.52, 1.72, 5.62), y = 0.25, label = c("High fence", "Medium fence", "Low fence"))
  counts <- table(factor(d$choice, levels = names(COUNTRY_BRIEF_CHOICE_LABEL)))
  leg$lab <- sprintf("%s (%d)", leg$lab, as.integer(counts[leg$key]))
  mix_leg <- data.frame(id = c("m1", "m1", "m1"), x = c(0.62 + 0.2 - 0.12, 0.62 + 0.2, 0.62 + 0.2), y = c(-1.05 - 0.17, -1.05 - 0.17, -1.05 + 0.05))
  ggplot2::ggplot() +
    ggplot2::geom_tile(data = as, ggplot2::aes(x = x, y = y), fill = as$fill, colour = "white", linewidth = 0.6, width = 2 * w + 0.04, height = 2 * h + 0.08) +
    ggplot2::geom_tile(data = na, ggplot2::aes(x = x, y = y), fill = "white", colour = cc$muted, linewidth = 0.3, linetype = "dashed", width = 2 * w - 0.02, height = 2 * h) +
    ggplot2::geom_polygon(data = fl, ggplot2::aes(x = x, y = y, group = id), fill = fl$fill, colour = "white", linewidth = 0.35) +
    ggplot2::geom_tile(data = rv, ggplot2::aes(x = x, y = y), fill = NA, colour = cc$ink, linewidth = 0.45, linetype = "11", width = 2 * w - 0.08, height = 2 * h - 0.06) +
    ggplot2::geom_text(data = d, ggplot2::aes(x = x - 0.04, y = y + ifelse(is.na(mix), 0, 0.12), label = label), colour = d$tcol, size = 2.35,
                       family = base_family, fontface = ifelse(d$choice %in% c("secure", "preserve", "catch_up"), "bold", "plain")) +
    ggplot2::geom_text(data = d[!is.na(d$mix), , drop = FALSE], ggplot2::aes(x = x - 0.1, y = y - 0.2, label = mix), colour = d$tcol[!is.na(d$mix)], size = 2.05,
                       family = base_family) +
    ggplot2::geom_text(data = heads, ggplot2::aes(x = x, y = y, label = label), size = 2.3, colour = cc$ink, family = base_family, lineheight = 0.9, fontface = "bold") +
    ggplot2::geom_text(data = bands, ggplot2::aes(x = x, y = y, label = label), size = 2.2, colour = cc$muted, family = base_family, fontface = "italic") +
    ggplot2::geom_segment(ggplot2::aes(x = c(4.5, 5.5), xend = c(4.5, 5.5), y = 0.52, yend = n_t + 1.75), colour = cc$muted, linewidth = 0.35, linetype = "22") +
    ggplot2::geom_text(data = fence_lab, ggplot2::aes(x = x, y = y, label = label), hjust = 0, size = 2.2, colour = cc$ink, family = base_family, fontface = "bold") +
    ggplot2::geom_tile(data = leg[leg$key != "not_assessed", ], ggplot2::aes(x = x, y = y), fill = leg$fill[leg$key != "not_assessed"], width = 0.2, height = 0.36) +
    ggplot2::geom_tile(data = leg[leg$key == "not_assessed", ], ggplot2::aes(x = x, y = y), fill = "white", colour = cc$muted, linetype = "dashed", linewidth = 0.3,
                       width = 0.2, height = 0.36) +
    ggplot2::geom_text(data = leg, ggplot2::aes(x = x + 0.15, y = y, label = lab), hjust = 0, size = 2.4, colour = cc$ink, family = base_family) +
    ggplot2::geom_tile(ggplot2::aes(x = 0.62, y = -1.05), fill = col[["partner"]], width = 0.2, height = 0.36) +
    ggplot2::geom_polygon(data = mix_leg, ggplot2::aes(x = x, y = y, group = id), fill = ycol[["small"]], colour = "white", linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(x = 0.92, y = -1.05, label = "Mixed step: a corner flag shows the fence of its other parts, High (orange), Medium (violet) or Low (grey)"),
                       hjust = 0, size = 2.4, colour = cc$ink, family = base_family) +
    ggplot2::geom_tile(ggplot2::aes(x = 0.62, y = -1.7), fill = "white", colour = cc$ink, linewidth = 0.45, linetype = "11", width = 0.2, height = 0.36) +
    ggplot2::geom_text(ggplot2::aes(x = 0.77, y = -1.7, label = "Review / insufficient evidence: low confidence, a review flag, a thin anchor or a known data artifact (not a headline call)"),
                       hjust = 0, size = 2.4, colour = cc$ink, family = base_family) +
    ggplot2::scale_x_continuous(limits = c(0.45, 6.6), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = seq_along(techs), labels = rev(techs), limits = c(-2.0, n_t + 1.85), expand = c(0, 0)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void(base_family = base_family) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7, colour = cc$ink, hjust = 1, margin = ggplot2::margin(r = 3)),
                   plot.margin = ggplot2::margin(4, 6, 4, 4), plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}

# The points of the value-position chart: producing steps with both measures (Recovery has no position
# measure), steps on the same spot merged into one marker ("EV drivetrain components / EV and charger
# manufacturing"), and the labels:
# Secure, Preserve and Catch Up first, then a strong position left open, then Partner by Strategic
# Industrial Value, up to `n`.
country_brief_scatter_points <- function(cells, n) {
  d <- cells[cells$stage %in% setdiff(COUNTRY_BRIEF_PRODUCING, "Recovery") & cells$choice != "not_assessed" & !is.na(cells$siv) & !is.na(cells$cp), , drop = FALSE]
  d$x <- round(100 * d$siv, 1)
  d$y <- round(100 * d$cp, 1)
  key <- paste(d$x, d$y, d$choice)
  m <- do.call(rbind, lapply(split(seq_len(nrow(d)), factor(key, levels = unique(key))), function(i) {
    r <- d[i[1], , drop = FALSE]
    # one technology on the same spot reads by its stages ("wind components and assembly")
    r$name <- if (length(i) > 1 && length(unique(d$tech[i])) == 1) {
      paste(COUNTRY_TECH_PROSE[[d$tech[i[1]]]] %||% tolower(d$tech[i[1]]), paste(tolower(d$stage[i]), collapse = " and "))
    } else paste(d$name[i], collapse = " / ")
    r$n_merged <- length(i)
    r
  }))
  m <- m[order(m$x), , drop = FALSE]
  # every marker sits at its own value (no offsets, which would read as negative positions)
  m$yp <- m$y
  # a strong position or a very high value left open (Watch in the top quarter near the value bar, or a value
  # of 90 and more) is labelled before Partner
  pri <- c(secure = 1, preserve = 2, catch_up = 3, partner = 5, watch = 6)[m$choice]
  pri[m$choice == "watch" & ((m$y >= 75 & m$x >= 50) | m$x >= 90)] <- 4
  # a review case never takes a label (a hollow marker shows it; a label would promote it on a major chart)
  m$is_review <- if (is.null(m$review)) FALSE else !is.na(m$review)
  pri[m$is_review] <- 9
  o <- order(pri, -m$x)
  keep <- m$node[o][pri[o] <= 5]
  m$lab <- ifelse(m$node %in% utils::head(keep, n), m$name, NA_character_)
  m
}

# The value-position chart. `cells`: country_brief_chart_data().
country_brief_scatter_plot <- function(cells, layers, cfg, thresholds, base_family = "sans") {
  cc <- cfg$charts
  col <- unlist(cc$colors)
  d <- country_brief_scatter_points(cells, cc$label_max %||% 9)
  d$shape <- c(secure = 23, preserve = 21, catch_up = 24, partner = 22, watch = 21)[d$choice]
  d$size <- ifelse(d$choice == "watch", 1.7, 2.9)
  d$fillc <- ifelse(d$is_review, "white", unname(col[d$choice]))
  px <- 100 * (thresholds$prize %||% 0.6)
  py <- 100 * (thresholds$cp %||% 0.75)
  leg <- data.frame(k = c("secure", "preserve", "catch_up", "partner", "watch"), lab = c("Secure", "Preserve", "Catch Up", "Partner", "Watch"),
                    shape = c(23, 21, 24, 22, 21))
  leg$x <- seq(2, by = 14, length.out = nrow(leg))
  leg$y <- 111
  # labels of the steps at or near position 0 go into a band under the axis
  d$ny <- 0
  low <- which(d$yp < 8 & !is.na(d$lab))
  low <- low[order(d$x[low])]
  # two staggered rows, alternating along the value axis, so neighbors never share a line
  d$ny[low] <- rep(c(-9, -20), length.out = length(low))
  lo <- -30
  ggplot2::ggplot(d, ggplot2::aes(x = x, y = yp)) +
    ggplot2::annotate("rect", xmin = px, xmax = 102, ymin = py, ymax = 102, fill = "#eef6fa", colour = NA) +
    ggplot2::annotate("segment", x = px, xend = px, y = -4, yend = 102, linetype = "22", colour = cc$muted, linewidth = 0.35) +
    ggplot2::geom_hline(yintercept = py, linetype = "22", colour = cc$muted, linewidth = 0.35) +
    ggplot2::annotate("text", x = 101, y = 101, label = "High value, top-quarter position: Preserve", hjust = 1, vjust = 1, size = 2.45, colour = cc$muted, family = base_family) +
    ggplot2::annotate("text", x = 101, y = 44, label = "High value, weaker today: Catch Up where there is an anchor;\nelse Partner where need is high, otherwise Watch", hjust = 1, vjust = 1, size = 2.45,
                      colour = cc$muted, family = base_family, lineheight = 0.95) +
    ggplot2::annotate("text", x = 1, y = 101, label = "Left of the value bar: Watch, unless supply-chain need is high", hjust = 0, vjust = 1, size = 2.45,
                      colour = cc$muted, family = base_family, lineheight = 0.95) +
    ggplot2::annotate("text", x = 0.8, y = py + 0.8, label = "top quarter 75", hjust = 0, vjust = 0, size = 1.9, colour = cc$muted, family = base_family) +
    ggplot2::geom_point(data = d[d$choice == "secure", , drop = FALSE], shape = 21, size = 5.4, stroke = 1.1, colour = col[["secure"]], fill = NA) +
    ggplot2::geom_point(shape = d$shape, size = d$size, fill = d$fillc, colour = cc$ink, stroke = 0.45) +
    ggrepel::geom_text_repel(ggplot2::aes(label = lab), size = 2.7, family = base_family, colour = cc$ink, min.segment.length = 0.1, segment.colour = cc$muted,
                             segment.size = 0.25, box.padding = 0.35, point.padding = 0.3, max.overlaps = Inf, seed = 7, na.rm = TRUE,
                             force = 2, nudge_y = d$ny, ylim = c(lo + 1, 98)) +
    ggplot2::geom_point(data = leg, ggplot2::aes(x = x, y = y), shape = leg$shape, size = 2.6, fill = unname(col[leg$k]), colour = cc$ink, stroke = 0.45, inherit.aes = FALSE) +
    ggplot2::geom_text(data = leg, ggplot2::aes(x = x + 2.2, y = y, label = lab), hjust = 0, size = 2.6, family = base_family, colour = cc$ink, inherit.aes = FALSE) +
    ggplot2::annotate("point", x = 74, y = 111, shape = 21, size = 2.6, fill = "white", colour = cc$ink, stroke = 0.45) +
    ggplot2::annotate("text", x = 76.2, y = 111, label = "Review case (hollow)", hjust = 0, size = 2.6, family = base_family, colour = cc$ink) +
    ggplot2::scale_x_continuous("Strategic Industrial Value: how much having this productive capability matters (0-100)", limits = c(0, 102), breaks = seq(0, 100, 20),
                                expand = c(0, 0)) +
    ggplot2::scale_y_continuous("Competitive Position: how strong the country is today (0-100)", limits = c(lo, 114), breaks = seq(0, 100, 25), expand = c(0, 0)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal(base_family = base_family, base_size = 8.5) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), panel.grid.major = ggplot2::element_line(colour = "#eef0f3", linewidth = 0.3),
                   axis.title = ggplot2::element_text(size = 8, colour = cc$ink), axis.text = ggplot2::element_text(size = 7.5, colour = cc$muted),
                   plot.margin = ggplot2::margin(4, 8, 4, 4), plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}

# The alternative text of the two figures (the screen's <img alt>, Word's description).
country_brief_alt <- function(id, cells, country) {
  if (id == "aib_roles") return(sprintf("Matrix of %s's roles in the strategic compacts of the Allied Industrial Base: where it is essential, holds a substitutable role, or relies on trusted partners.", country))
  if (id == "deployment") return(sprintf("Bar chart of %s's deployment need by technology, marking Accelerate and Watch and the main driver of each.", country))
  if (id == "strategy_map") {
    n <- table(factor(cells$choice, levels = names(COUNTRY_BRIEF_CHOICE_LABEL)))
    sprintf("Strategy Map of %s: %d technologies by six supply-chain stages. %d Secure, %d Preserve, %d Catch Up, %d Partner, %d Accelerate, %d Watch and %d not assessed; %d steps mixed.",
            country, length(unique(cells$tech)), n[["secure"]], n[["preserve"]], n[["catch_up"]], n[["partner"]], n[["accelerate"]], n[["watch"]],
            n[["not_assessed"]], sum(cells$mixed))
  } else {
    sprintf("Scatter of %s's producing steps by Strategic Industrial Value and Competitive Position, marked by strategic choice.", country)
  }
}

# Figure 3: the economy's role in the Allied Industrial Base, one row per strategic compact in which it
# holds a role or relies on partners, one column per role group (config aib_matrix). Each cell: its level
# (essential, a substitutable role, relies on partners, or relies where allied supply falls short) and a few
# words (the lead step and share, the host rank, the partners).
# The layout is in inches (y) and columns (x): each row is as tall as its most-wrapped cell or label, so no
# text leaves its tile; the heads and the one-row legend get their own bands. height_in (the figure's height,
# never below charts$matrix_height_in) is what scripts/92 renders at.
COUNTRY_BRIEF_MATRIX_TEXT <- list(cell = 2.1, head = 2.5, legend = 2.3, label_pt = 7, lineheight = 0.9, margin_pt = 4, wrap = 24, pad_in = 0.08)
country_brief_matrix_layout <- function(mx, cfg) {
  ts <- COUNTRY_BRIEF_MATRIX_TEXT
  cols <- names(cfg$aib_matrix$columns)
  labs <- vapply(cfg$aib_matrix$columns, function(x) x$label, "")
  wrap <- function(t, w) vapply(t, function(s) paste(strwrap(s, w), collapse = "\n"), "", USE.NAMES = FALSE)
  nl <- function(t) lengths(gregexpr("\n", t, fixed = TRUE)) * grepl("\n", t, fixed = TRUE) + 1L
  line <- function(mm) ts$lineheight * mm * ggplot2::.pt / 72
  rows <- unique(mx[order(mx$row_order), c("compact", "name")])
  rows$label <- wrap(rows$name, 24)
  d <- mx
  d$x <- match(d$col, cols)
  # "core seat" stays on one line (a no-break space, drawn as a space)
  d$wrapped <- gsub("\u00a0", " ", wrap(gsub("core seat", "core\u00a0seat", d$text, fixed = TRUE), ts$wrap), fixed = TRUE)
  need <- vapply(seq_len(nrow(rows)), function(i) max(nl(d$wrapped[d$compact == rows$compact[i]]) * line(ts$cell),
                                                      nl(rows$label[i]) * ts$lineheight * ts$label_pt / 72), 0)
  rows$h <- pmax(2 * line(ts$cell), need) + ts$pad_in
  heads <- data.frame(x = seq_along(cols), label = wrap(labs, 14))
  head_h <- max(nl(heads$label)) * line(ts$head) + 0.06
  leg_h <- 0.17
  extra <- max(0, cfg$charts$matrix_height_in - 2 * ts$margin_pt / 72 - sum(rows$h) - head_h - leg_h)
  rows$h <- rows$h + extra / max(1, nrow(rows))
  total <- sum(rows$h) + head_h + leg_h
  rows$y <- total - head_h - cumsum(rows$h) + rows$h / 2
  d$y <- rows$y[match(d$compact, rows$compact)]
  d$h <- rows$h[match(d$compact, rows$compact)] - 0.04
  heads$y <- total - head_h / 2
  # legend keys spaced by their labels' widths (Arial at the legend size, in columns: 0.62, 0.99, 0.92, 1.07)
  leg <- data.frame(level = c("essential", "role", "relies", "gap"), x = c(0.62, 1.56, 2.87, 4.11), y = leg_h / 2,
                    lab = c("Essential to allies", "Holds the role, substitutable", "Relies on trusted partners", "Relies; allied supply falls short"))
  list(d = d, rows = rows, heads = heads, leg = leg, total = total, n_cols = length(cols), height_in = total + 2 * ts$margin_pt / 72)
}

country_brief_matrix_plot <- function(mx, cfg, base_family = "sans") {
  cc <- cfg$charts
  ts <- COUNTRY_BRIEF_MATRIX_TEXT
  fill <- c(essential = "#1f4e79", role = "#a8c8e6", relies = "#e9f6f6", gap = "#fde3cf")
  ink <- c(essential = "#ffffff", role = "#111827", relies = "#111827", gap = "#111827")
  edge <- c(essential = "#1f4e79", role = "#a8c8e6", relies = "#55c4c5", gap = "#f58229")
  lo <- country_brief_matrix_layout(mx, cfg)
  d <- lo$d
  leg <- lo$leg
  ggplot2::ggplot() +
    ggplot2::geom_tile(data = d, ggplot2::aes(x = x, y = y, height = h), fill = unname(fill[d$level]), colour = unname(edge[d$level]), linewidth = 0.6,
                       linetype = ifelse(d$level %in% c("relies", "gap"), "22", "solid"), width = 0.94) +
    ggplot2::geom_text(data = d, ggplot2::aes(x = x, y = y, label = wrapped), colour = unname(ink[d$level]), size = ts$cell, lineheight = ts$lineheight,
                       family = base_family) +
    ggplot2::geom_text(data = lo$heads, ggplot2::aes(x = x, y = y, label = label), size = ts$head, fontface = "bold", lineheight = ts$lineheight,
                       colour = cc$ink, family = base_family) +
    ggplot2::geom_tile(data = leg, ggplot2::aes(x = x, y = y), fill = unname(fill[leg$level]), colour = unname(edge[leg$level]),
                       linetype = ifelse(leg$level %in% c("relies", "gap"), "22", "solid"), width = 0.16, height = 0.1) +
    ggplot2::geom_text(data = leg, ggplot2::aes(x = x + 0.12, y = y, label = lab), hjust = 0, size = ts$legend, colour = cc$ink, family = base_family) +
    ggplot2::scale_x_continuous(limits = c(0.5, lo$n_cols + 0.5), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = lo$rows$y, labels = lo$rows$label, limits = c(0, lo$total), expand = c(0, 0)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void(base_family = base_family) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7, colour = cc$ink, hjust = 1, lineheight = 0.9, margin = ggplot2::margin(r = 4)),
                   plot.margin = ggplot2::margin(4, 6, 4, 4), plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}

# Figure 4: deployment need by technology, Accelerate or Watch, with the main driver and a lighter bar where
# the measure rests on a single indicator or low confidence.
country_brief_deployment_plot <- function(dep, cfg, thresholds, base_family = "sans") {
  cc <- cfg$charts
  col <- unlist(cc$colors)
  d <- dep
  d$is_review <- if (is.null(d$review)) FALSE else !is.na(d$review)
  # the headline bars by need, then the review cases apart (17B: a single-indicator Accelerate is a warning, not a priority)
  d <- d[order(d$is_review, -d$dn), , drop = FALSE]
  n_ok <- sum(!d$is_review)
  d$y <- rev(seq_len(nrow(d))) + ifelse(d$is_review, 0, 0.8 * any(d$is_review))
  d$v <- 100 * d$dn
  d$weak <- d$confidence %in% "low" | (!is.na(d$families) & d$families < 2)
  d$fill <- ifelse(d$is_review, "#e5e7eb", unname(col[ifelse(d$choice == "accelerate", "accelerate", "watch")]))
  d$alpha <- 1
  d$lab <- ifelse(d$is_review, sprintf("%d  review: single low-confidence indicator", as.integer(country_brief_round(d$v))),
                  sprintf("%d  %s", as.integer(country_brief_round(d$v)), d$driver %||% ""))
  bar <- 100 * (thresholds$deployment %||% 0.6)
  rv_head <- if (any(d$is_review)) data.frame(x = 0.5, y = max(d$y[d$is_review]) + 0.6, label = "Review / insufficient evidence (not a deployment priority)") else data.frame(x = numeric(), y = numeric(), label = character())
  ggplot2::ggplot(d) +
    ggplot2::geom_col(ggplot2::aes(x = v, y = y), orientation = "y", fill = d$fill, alpha = d$alpha, width = 0.7) +
    ggplot2::geom_vline(xintercept = bar, linetype = "22", colour = cc$muted, linewidth = 0.4) +
    ggplot2::annotate("text", x = bar + 0.8, y = max(d$y) + 0.75, label = sprintf("Accelerate at %d", as.integer(bar)), hjust = 0, size = 2.4,
                      colour = cc$muted, family = base_family) +
    ggplot2::geom_text(ggplot2::aes(x = v + 1.2, y = y, label = lab), hjust = 0, size = 2.5, colour = cc$ink, family = base_family) +
    ggplot2::geom_text(data = rv_head, ggplot2::aes(x = x, y = y, label = label), hjust = 0, size = 2.4, colour = cc$muted, family = base_family, fontface = "italic") +
    ggplot2::scale_y_continuous(breaks = d$y, labels = country_brief_cap_vec(d$name), expand = c(0.02, 0.02), limits = c(0.4, max(d$y) + 1.1)) +
    ggplot2::scale_x_continuous("Deployment need (0-100)", limits = c(0, 140), breaks = seq(0, 100, 20), expand = c(0, 0)) +
    ggplot2::theme_minimal(base_family = base_family, base_size = 8.5) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(colour = "#eef0f3", linewidth = 0.3), axis.title.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = 7.5, colour = cc$ink), axis.title.x = ggplot2::element_text(size = 7.5, colour = cc$ink, hjust = 0.35),
                   axis.text.x = ggplot2::element_text(size = 7, colour = cc$muted), plot.margin = ggplot2::margin(4, 8, 4, 4),
                   plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}

country_brief_cap_vec <- function(x) vapply(x, country_brief_cap1, "", USE.NAMES = FALSE)
