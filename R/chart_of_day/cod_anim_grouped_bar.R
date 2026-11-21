# Chart of the Day "grouped_bar" animated (scripts/chart_of_day/03_animate.R). Pure.
#
# A country's exports and imports at each supply-chain step (the stage balance). One horizontal pair of bars per step,
# steps top to bottom in supply-chain order (the spec's row order), exports above imports, on one $ scale. Values are
# written at the bar ends, so there is no x axis; the net balance (exports minus imports, "+$1.2bn" surplus in the
# exports colour, "-$3.4bn" deficit in the imports colour) sits in a right-hand column, since the titles are about
# surpluses and deficits. The top line carries the focus country's name in orange and the Exports/Imports key. Every
# position comes from the final data, so nothing moves except the bars growing and their values riding on them.
#
# Clock (t from 0 to 1): the country name, key, step labels and baseline are there from the start; 0.03-0.53 the bars
# grow step by step, exports just ahead of imports, values counting up and fading in over the first quarter of their
# bar; 0.48-0.55 the "Net balance" header; 0.52-0.80 the net balances, step by step. Final from 0.80.
cod_anim_grouped_bar <- function(s, t, family = "") {
  t <- cod_clamp01(t)
  r <- s$rows
  cats <- unique(r$category)
  n <- length(cats)
  val <- function(series) {
    v <- vapply(cats, function(k) sum(r$value[r$category == k & r$series == series], na.rm = TRUE), numeric(1))
    unname(v)
  }
  ex <- val("Exports"); im <- val("Imports"); net <- ex - im
  fmt <- if (is.null(s$chart$format)) "usd" else s$chart$format
  col_ex <- COD_ANIM_COL[["exports"]]; col_im <- COD_ANIM_COL[["imports"]]
  if (!is.null(s$chart$colors)) {
    if (!is.null(s$chart$colors[["Exports"]])) col_ex <- s$chart$colors[["Exports"]]
    if (!is.null(s$chart$colors[["Imports"]])) col_im <- s$chart$colors[["Imports"]]
  }

  # --- layout: y runs up, one unit per step; x in $bn ------------------------------------------------------------------
  bar_h <- 0.34; off <- 0.19
  y_h <- 0
  y_r <- y_h - 1.05 - seq(0, n - 1)
  ylim <- c(min(y_r) - 0.6, y_h + 0.45)
  big <- max(c(ex, im, 1)) / 1e9
  x_max <- 1.52 * big          # room after the longest bar for its value and the net-balance column
  u <- x_max / 280             # $bn per point of panel width (about 280 pt of panel in the 5.4 in body)
  gap <- 4 * u                 # step label to bar
  lab_pt <- 8.8; val_pt <- 7.8; key_pt <- 7.8; net_pt <- 8.8; head_pt <- 9.2

  # --- timing ----------------------------------------------------------------------------------------------------------
  grow <- function(a, b) cod_ease(cod_stagger(t, seq_len(n), n, a, b, 0.45))
  p_ex <- grow(0.03, 0.50); p_im <- grow(0.06, 0.53)
  a_net_h <- cod_ease(cod_phase(t, 0.48, 0.55))
  a_net <- cod_ease(cod_stagger(t, seq_len(n), n, 0.52, 0.80, 0.4))
  lab_alpha <- function(p) cod_clamp01(p / 0.25)   # a value fades in over the first quarter of its bar

  # --- marks -----------------------------------------------------------------------------------------------------------
  bars <- function(v, p, dy) data.frame(xmin = 0, xmax = v / 1e9 * p, ymin = y_r + dy - bar_h / 2, ymax = y_r + dy + bar_h / 2)
  vals <- function(v, p, dy) data.frame(x = v / 1e9 * p + 3 * u, y = y_r + dy, lab = cod_fmt(v * p, fmt), a = lab_alpha(p))
  b_ex <- bars(ex, p_ex, off); b_im <- bars(im, p_im, -off)
  vl <- rbind(vals(ex, p_ex, off), vals(im, p_im, -off))
  rows <- data.frame(y = y_r, lab = cats)
  nets <- data.frame(y = y_r, a = a_net, col = ifelse(net >= 0, col_ex, col_im),
                     lab = paste0(ifelse(net < 0, "−", "+"), cod_fmt(abs(net), fmt)))
  focus <- if (is.null(s$focus$name)) "" else s$focus$name
  heads <- data.frame(x = c(-gap, x_max), y = y_h, a = c(1, a_net_h), lab = c(focus, "Net balance"), hjust = c(1, 1),
                      face = c("bold", "plain"), size = cod_pt(c(head_pt, key_pt)),
                      col = c(COD_ANIM_COL[["focus"]], COD_INK[["muted"]]))
  kw <- 6.4 * u; ks <- 52 * u
  keys <- data.frame(xmin = c(0, ks), xmax = c(0, ks) + kw, ymin = y_h - bar_h / 2, ymax = y_h + bar_h / 2,
                     fill = c(col_ex, col_im))
  keys_lab <- data.frame(x = keys$xmax + 3 * u, y = y_h, lab = c("Exports", "Imports"))
  base <- data.frame(y = max(y_r) + 0.45, yend = min(y_r) - 0.45)
  # faint separators between steps
  seps <- data.frame(y = utils::head(y_r, -1) - 0.5)

  ggplot2::ggplot() +
    ggplot2::geom_segment(data = seps, ggplot2::aes(x = -gap - 70 * u, xend = x_max, y = y, yend = y),
                          colour = COD_INK[["grid"]], linewidth = 0.3) +
    ggplot2::geom_segment(data = base, ggplot2::aes(x = 0, xend = 0, y = y, yend = yend),
                          colour = COD_INK[["hairline"]], linewidth = 0.35) +
    ggplot2::geom_rect(data = b_ex, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = col_ex, colour = NA) +
    ggplot2::geom_rect(data = b_im, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = col_im, colour = NA) +
    ggplot2::geom_text(data = vl, ggplot2::aes(x = x, y = y, label = lab, alpha = a), hjust = 0, size = cod_pt(val_pt),
                       colour = COD_INK[["secondary"]], family = family) +
    ggplot2::geom_text(data = rows, ggplot2::aes(x = -gap, y = y, label = lab), hjust = 1, size = cod_pt(lab_pt),
                       colour = COD_INK[["primary"]], family = family) +
    ggplot2::geom_text(data = nets, ggplot2::aes(x = x_max, y = y, label = lab, alpha = a, colour = col), hjust = 1,
                       size = cod_pt(net_pt), fontface = "bold", family = family) +
    ggplot2::geom_text(data = heads, ggplot2::aes(x = x, y = y, label = lab, alpha = a, hjust = hjust, fontface = face,
                                                  size = size, colour = col), family = family) +
    ggplot2::geom_rect(data = keys, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), colour = NA) +
    ggplot2::geom_text(data = keys_lab, ggplot2::aes(x = x, y = y, label = lab), hjust = 0, size = cod_pt(key_pt),
                       colour = COD_INK[["secondary"]], family = family) +
    ggplot2::scale_alpha_identity() + ggplot2::scale_colour_identity() + ggplot2::scale_fill_identity() +
    ggplot2::scale_size_identity() +
    ggplot2::scale_x_continuous(breaks = NULL) +
    # the step labels as transparent axis text, only to reserve their width (the visible ones are the text layer)
    ggplot2::scale_y_continuous(breaks = rows$y, labels = rows$lab) +
    ggplot2::coord_cartesian(xlim = c(0, x_max), ylim = ylim, expand = FALSE, clip = "off") +
    ggplot2::labs(x = NULL, y = NULL, title = NULL, subtitle = NULL, caption = NULL) +
    cod_theme(family = family) + cod_no_grid() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(colour = "transparent", size = lab_pt, margin = ggplot2::margin(r = 4)))
}
