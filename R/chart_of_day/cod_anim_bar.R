# Chart of the Day "bar" charts animated for the social card (scripts/chart_of_day/03_animate.R). Pure.
#
# Horizontal ranked bars in the row order of s$rows (already ranked; a focus row may be appended last). Three colourings,
# as in the Explorer (ChartOfDayFigure.tsx): leaderboards, with the focus country (highlight) in NEIS orange and the
# rest in the receding grey; s$chart$color_by == "group", with bars coloured by UN-voting bloc (U.S.-leaning,
# China-leaning, Nonaligned, Others); and s$chart$colors, the spec's own named colours (e.g. Support / Protection),
# which override the bloc colours. When colour carries meaning a key sits above the bars (the card has no legend area).
# Values are written at the bar ends in s$chart$format (cod_fmt), so there is no x axis. A focus row appended after a
# gap in rank (e.g. "15. Sweden" after the top 10) sits a half row below the others. Every position comes from the
# final data, so nothing moves except the bars growing and their values riding on them.
#
# Clock (t from 0 to 1): the row labels and the baseline are there from the start; 0.00-0.10 the key fades in;
# 0.04-0.80 the bars grow from 0 one after another, top to bottom, each value counting up at its bar end and fading in
# over the first quarter of its bar. The focus row's label (and its value, as it appears) is orange and bold
# throughout, so the eye has somewhere to go from the first frame. Final from 0.80.
cod_anim_bar <- function(s, t, family = "") {
  t <- cod_clamp01(t)
  rows <- as.data.frame(s$rows, stringsAsFactors = FALSE)
  n <- nrow(rows)
  fmt <- if (is.null(s$chart$format)) "" else s$chart$format
  val <- as.numeric(rows$value); val[is.na(val)] <- 0
  hl <- if (is.null(rows$highlight)) rep(FALSE, n) else (!is.na(rows$highlight) & as.logical(rows$highlight))
  grp <- if (is.null(rows$group)) rep("Others", n) else ifelse(is.na(rows$group) | !nzchar(rows$group), "Others", rows$group)
  by_group <- identical(s$chart$color_by, "group")
  own <- s$chart$colors
  own <- if (is.null(own) || !length(own)) NULL else unlist(own)

  # --- colours ---------------------------------------------------------------------------------------------------------
  bloc <- c("U.S.-leaning" = COD_ANIM_COL[["us_leaning"]], "China-leaning" = COD_ANIM_COL[["china_leaning"]],
            "Nonaligned" = COD_ANIM_COL[["nonaligned"]], "Others" = COD_ANIM_COL[["others"]])
  if (by_group) {
    pal <- if (!is.null(own)) own else bloc
    fill <- unname(ifelse(grp %in% names(pal), pal[grp], COD_ANIM_COL[["other"]]))
  } else {
    fill <- ifelse(hl, COD_ANIM_COL[["focus"]], COD_ANIM_COL[["other"]])
  }

  # --- layout: y runs down, one unit per row; x in value units ----------------------------------------------------------
  rank <- suppressWarnings(as.numeric(sub("^\\s*(\\d+)\\..*$", "\\1", rows$label)))
  rank[!grepl("^\\s*\\d+\\.", rows$label)] <- NA
  gap_last <- n >= 2 && hl[n] && !is.na(rank[n]) && !is.na(rank[n - 1]) && rank[n] > n && rank[n] - rank[n - 1] > 1
  y <- -(seq_len(n) - 1) - ifelse(gap_last & seq_len(n) == n, 0.5, 0)
  bar_h <- 0.62
  lab_pt <- 8.8; val_pt <- 7.8; key_pt <- 7.8

  # panel width in points (5.4 in body less the card body's margins and the row labels' reserved width)
  lab_w <- max(nchar(rows$label)) * 0.53 * lab_pt + 6
  panel_pt <- 5.4 * 72 - 26 - lab_w
  big <- max(val, 0)
  if (big <= 0) big <- 1
  val_w <- max(nchar(cod_fmt(val, fmt))) * 0.56 * val_pt + 10   # room after the longest bar for its value
  x_max <- big / (1 - val_w / panel_pt)
  u <- x_max / panel_pt                                          # value units per point of panel width
  gap <- 5 * u                                                   # row label to bar

  # the key: present groups, one line above the bars (two lines when it would not fit)
  key <- NULL
  if (by_group) {
    if (!is.null(own)) {
      kg <- names(own)[names(own) %in% grp]
      klab <- kg; kcol <- unname(own[kg])
    } else {
      kg <- c("U.S.-leaning", "Nonaligned", "China-leaning", "Others")
      kg <- kg[kg %in% grp]
      klab <- ifelse(kg == "Others", "All other countries", kg); kcol <- unname(bloc[kg])
    }
    if (length(kg)) {
      sw <- 7; sp <- 4; between <- 12                                # swatch, swatch-to-text, item gap (pt)
      w_item <- sw + sp + nchar(klab) * 0.48 * key_pt + between
      x0_pt <- 0                                                      # start at the bars' baseline
      avail <- panel_pt - x0_pt
      line <- integer(length(kg)); xs <- numeric(length(kg)); cur <- 0; ln <- 1
      for (k in seq_along(kg)) {
        if (cur > 0 && cur + w_item[k] - between > avail) { ln <- ln + 1; cur <- 0 }
        line[k] <- ln; xs[k] <- cur; cur <- cur + w_item[k]
      }
      key <- data.frame(x = (x0_pt + xs) * u, line = line, lab = klab, col = kcol, stringsAsFactors = FALSE)
    }
  }
  n_key_lines <- if (is.null(key)) 0 else max(key$line)
  key_y <- if (n_key_lines) 0.95 + (n_key_lines - seq_len(n_key_lines)) * 0.75 else numeric()
  ylim <- c(min(y) - 0.55, if (n_key_lines) max(key_y) + 0.45 else 0.55)

  # --- timing ----------------------------------------------------------------------------------------------------------
  p <- cod_ease(cod_stagger(t, seq_len(n), n, 0.04, 0.80, if (n > 1) 0.4 else 1))
  a_key <- cod_ease(cod_phase(t, 0.00, 0.10))
  lab_alpha <- function(q) cod_clamp01(q / 0.25)     # a value fades in over the first quarter of its bar

  # --- marks -----------------------------------------------------------------------------------------------------------
  bars <- data.frame(xmin = 0, xmax = val * p, ymin = y - bar_h / 2, ymax = y + bar_h / 2, fill = fill,
                     stringsAsFactors = FALSE)
  # the focus row (highlight) is bold from the start: orange on leaderboards, primary ink on bloc-coloured charts
  hl_col <- if (by_group) COD_INK[["primary"]] else COD_ANIM_COL[["focus"]]
  vals <- data.frame(x = pmax(val * p, 0) + 3 * u, y = y, lab = cod_fmt(val * p, fmt), a = lab_alpha(p),
                     col = ifelse(hl, hl_col, COD_INK[["secondary"]]), face = ifelse(hl, "bold", "plain"),
                     stringsAsFactors = FALSE)
  rl <- data.frame(y = y, lab = rows$label, col = ifelse(hl, hl_col, COD_INK[["primary"]]),
                   face = ifelse(hl, "bold", "plain"), stringsAsFactors = FALSE)
  base <- data.frame(y = max(y) + 0.45, yend = min(y) - 0.45)

  g <- ggplot2::ggplot() +
    ggplot2::geom_segment(data = base, ggplot2::aes(x = 0, xend = 0, y = y, yend = yend),
                          colour = COD_INK[["hairline"]], linewidth = 0.35) +
    ggplot2::geom_rect(data = bars, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), colour = NA) +
    ggplot2::geom_text(data = vals, ggplot2::aes(x = x, y = y, label = lab, alpha = a, colour = col, fontface = face),
                       hjust = 0, size = cod_pt(val_pt), family = family) +
    ggplot2::geom_text(data = rl, ggplot2::aes(x = -gap, y = y, label = lab, colour = col, fontface = face), hjust = 1,
                       size = cod_pt(lab_pt), family = family)
  if (!is.null(key)) {
    ky <- key_y[key$line]
    sw_u <- 7 * u
    keys <- data.frame(xmin = key$x, xmax = key$x + sw_u, ymin = ky - 0.2, ymax = ky + 0.2, fill = key$col, a = a_key,
                       stringsAsFactors = FALSE)
    klab <- data.frame(x = key$x + sw_u + 4 * u, y = ky, lab = key$lab, a = a_key, stringsAsFactors = FALSE)
    g <- g +
      ggplot2::geom_rect(data = keys, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill, alpha = a),
                         colour = NA) +
      ggplot2::geom_text(data = klab, ggplot2::aes(x = x, y = y, label = lab, alpha = a), hjust = 0, size = cod_pt(key_pt),
                         colour = COD_INK[["secondary"]], family = family)
  }
  g +
    ggplot2::scale_alpha_identity() + ggplot2::scale_fill_identity() + ggplot2::scale_colour_identity() +
    ggplot2::scale_x_continuous(breaks = NULL) +
    # the row labels as transparent axis text, only to reserve their width (the visible ones are the text layers)
    ggplot2::scale_y_continuous(breaks = y, labels = rows$label) +
    ggplot2::coord_cartesian(xlim = c(0, x_max), ylim = ylim, expand = FALSE, clip = "off") +
    ggplot2::labs(x = NULL, y = NULL, title = NULL, subtitle = NULL, caption = NULL) +
    cod_theme(family = family) + cod_no_grid() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(colour = "transparent", size = lab_pt, face = "bold",
                                                       margin = ggplot2::margin(r = 5)))
}
