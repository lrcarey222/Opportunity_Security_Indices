# Chart of the Day "lines" animated for the social video (scripts/chart_of_day/03_animate.R). Pure.
#
# A trend of the focus country against two or three peers, as the Explorer draws it (ChartOfDayFigure.tsx): the focus
# line thick in NEIS orange, the peers thin in grey, each series named at its line's end. Indexed charts
# (y_label "Index, <year> = 100") get a subtle reference rule at 100; share charts (format "pct") get percentage ticks.
# The y label sits above the panel's top left, as the Explorer's "up-arrow" label does. Every position comes from the
# final data (axes, ticks, end-label slots), so nothing rescales or moves except the lines drawing left to right with a
# dot riding on each head and the focus value counting along just ahead of its head.
#
# Clock (t from 0 to 1): axes, grid, the y label and a dot at each series' first point are there from the start;
# 0.04-0.80 one moving year sweeps from the first year to the last (cubic in-out), every line revealed up to it with
# the last segment interpolated, the focus value riding ahead of the focus head; 0.78-0.88 the series names fade in
# at the line ends (nudged apart so they never collide) while the riding value fades into the focus label
# "<name> <final value>". Final from 0.88.
cod_anim_lines <- function(s, t, family = "") {
  t <- cod_clamp01(t)
  fmt <- if (is.null(s$chart$format)) "num" else s$chart$format
  y_lab <- if (is.null(s$chart$y_label)) "Investment, 2024 dollars" else s$chart$y_label
  indexed <- grepl("^Index", y_lab)

  r <- s$rows
  r <- r[is.finite(r$x) & is.finite(r$value), ]
  r <- r[order(r$series, r$x), ]
  hl <- tapply(as.logical(r$highlight), r$series, any)
  ser <- names(hl)
  ser <- c(ser[!hl], ser[hl])                      # focus drawn last, on top
  is_focus <- unname(hl[ser])
  ns <- length(ser)

  # --- layout: data units; the body is about 5.4 in (389 pt) wide and 4.0-4.6 in tall ------------------------------
  lab_pt <- 8.6; val_pt <- 7.8; tick_pt <- 7.8; ylab_pt <- 8.2
  W <- 330; H <- 250                               # approximate panel size in points
  x0 <- min(r$x); x1 <- max(r$x)
  last_v <- vapply(ser, function(k) { d <- r[r$series == k, ]; d$value[which.max(d$x)] }, numeric(1))
  end_lab <- ifelse(is_focus, paste(ser, cod_fmt(last_v, fmt)), ser)
  room <- max(nchar(end_lab) * ifelse(is_focus, 0.56, 0.52) * lab_pt) + 10   # pt after the last year for end labels
  ux <- (x1 - x0) / (W - room)                     # years per point
  x_hi <- x1 + room * ux

  v_lo <- min(0, r$value); v_hi <- max(r$value)
  br <- pretty(c(v_lo, v_hi), n = 5)
  y_lo <- min(br); y_top <- max(br)
  y_hi <- y_top + 0.10 * (y_top - y_lo)            # headroom for the y label
  uy <- (y_hi - y_lo) / H                          # value units per point

  span <- x1 - x0
  step <- if (span <= 7) 1 else if (span <= 12) 2 else 5
  xb <- seq(x0, x1, by = step)
  if (x1 - max(xb) >= step / 2) xb <- c(xb, x1)
  tick_lab <- if (fmt %in% c("pct", "pct2")) paste0(format(100 * br, trim = TRUE, drop0trailing = TRUE), "%") else
    format(br, big.mark = ",", trim = TRUE, scientific = FALSE)

  # --- timing ---------------------------------------------------------------------------------------------------------
  p_draw <- cod_ease(cod_phase(t, 0.04, 0.80))
  xh <- x0 + p_draw * span                         # the moving year
  a_lab <- cod_ease(cod_phase(t, 0.78, 0.88))
  a_tick <- 1 - a_lab

  # --- marks ----------------------------------------------------------------------------------------------------------
  path <- do.call(rbind, lapply(seq_len(ns), function(i) {
    d <- r[r$series == ser[i], ]
    keep <- d[d$x <= xh, c("x", "value")]
    if (xh > min(d$x) && !(xh %in% d$x) && xh <= max(d$x)) keep <- rbind(keep, data.frame(x = xh, value = stats::approx(d$x, d$value, xh)$y))
    data.frame(series = ser[i], x = keep$x, y = keep$value, focus = is_focus[i], ord = i)
  }))
  heads <- do.call(rbind, lapply(split(path, path$ord), function(d) d[which.max(d$x), ]))

  # end-label slots from the final values, nudged apart (at least one label height) and kept inside the panel
  gap <- 1.25 * lab_pt * uy
  o <- order(last_v); want <- last_v[o]; y <- want
  for (k in seq_along(y)[-1]) y[k] <- max(y[k], y[k - 1] + gap)
  y <- y - mean(y - want)                          # centre the block on where the labels want to be
  for (k in seq_along(y)[-1]) y[k] <- max(y[k], y[k - 1] + gap)
  y <- y + max(0, y_lo + 0.4 * gap - min(y)) - max(0, max(y) - (y_top + 0.3 * gap))
  slot <- numeric(ns); slot[o] <- y
  labs_df <- data.frame(x = x1 + 7 * ux, y = slot, lab = end_lab, a = a_lab,
                        col = ifelse(is_focus, COD_ANIM_COL[["focus"]], COD_INK[["secondary"]]),
                        face = ifelse(is_focus, "bold", "plain"))
  fh <- heads[heads$focus, ]
  ticker <- data.frame(x = fh$x + 7 * ux, y = fh$y, lab = cod_fmt(fh$y, fmt),
                       col = grDevices::adjustcolor(COD_ANIM_COL[["focus"]], alpha.f = a_tick),
                       fill = grDevices::adjustcolor(COD_INK[["surface"]], alpha.f = a_tick))
  ticker <- ticker[a_tick > 0.01, ]
  path_ln <- path[path$series %in% names(which(table(path$series) > 1)), ]

  grid_df <- data.frame(y = br[br != 0 | !indexed])
  base_df <- data.frame(y = if (y_lo <= 0 && y_top >= 0) 0 else numeric(0))
  ref_df <- data.frame(y = if (indexed) 100 else numeric(0))
  yl_df <- data.frame(x = x0, y = y_hi, lab = y_lab)

  ggplot2::ggplot() +
    ggplot2::geom_segment(data = grid_df, ggplot2::aes(x = x0, xend = x1, y = y, yend = y),
                          colour = COD_INK[["grid"]], linewidth = 0.3) +
    ggplot2::geom_segment(data = base_df, ggplot2::aes(x = x0, xend = x1, y = y, yend = y),
                          colour = COD_INK[["hairline"]], linewidth = 0.4) +
    ggplot2::geom_segment(data = ref_df, ggplot2::aes(x = x0, xend = x1, y = y, yend = y),
                          colour = COD_INK[["muted"]], linewidth = 0.35, linetype = "22") +
    ggplot2::geom_text(data = yl_df, ggplot2::aes(x = x, y = y, label = lab), hjust = 0, vjust = 1, size = cod_pt(ylab_pt),
                       colour = COD_INK[["muted"]], family = family) +
    ggplot2::geom_path(data = path_ln[!path_ln$focus, ], ggplot2::aes(x = x, y = y, group = series),
                       colour = COD_ANIM_COL[["other_line"]], linewidth = 0.75, lineend = "round", linejoin = "round") +
    ggplot2::geom_path(data = path_ln[path_ln$focus, ], ggplot2::aes(x = x, y = y, group = series),
                       colour = COD_ANIM_COL[["focus"]], linewidth = 1.5, lineend = "round", linejoin = "round") +
    ggplot2::geom_point(data = heads[!heads$focus, ], ggplot2::aes(x = x, y = y), colour = COD_ANIM_COL[["other_line"]], size = 1.7) +
    ggplot2::geom_point(data = heads[heads$focus, ], ggplot2::aes(x = x, y = y), colour = COD_ANIM_COL[["focus"]], size = 2.9) +
    ggplot2::geom_label(data = ticker, ggplot2::aes(x = x, y = y, label = lab, colour = col, fill = fill), hjust = 0, size = cod_pt(val_pt),
                        fontface = "bold", label.size = 0,
                        label.padding = grid::unit(1.2, "pt"), family = family) +
    ggplot2::geom_text(data = labs_df, ggplot2::aes(x = x, y = y, label = lab, alpha = a, colour = col, fontface = face),
                       hjust = 0, size = cod_pt(lab_pt), family = family) +
    ggplot2::scale_alpha_identity() + ggplot2::scale_colour_identity() + ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(breaks = xb, labels = as.character(xb)) +
    ggplot2::scale_y_continuous(breaks = br, labels = tick_lab) +
    ggplot2::coord_cartesian(xlim = c(x0, x_hi), ylim = c(y_lo, y_hi), expand = FALSE, clip = "off") +
    ggplot2::labs(x = NULL, y = NULL) +
    cod_theme(family = family) + cod_no_grid() +
    ggplot2::theme(axis.text = ggplot2::element_text(colour = COD_INK[["secondary"]], size = tick_pt),
                   axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 4)),
                   axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 4)))
}
