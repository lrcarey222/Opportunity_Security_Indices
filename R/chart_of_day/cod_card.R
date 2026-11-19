# Chart of the Day social card (pure): the NEIS card frame for animated posts, ported from the trade-fence
# explainer's neis_card.R (osi-worktrees/tri, R/trade_fence_explainer/neis_card.R; names prefixed cod_ so the two can
# sit on one branch). The look is the Explorer's export card (dashboard/src/chartOfDay/ExportCard.tsx): an orange bar
# along the top, the NEIS Center logo (vector; the wordmark takes the ground's ink), an uppercase grey eyebrow and a
# label at top right, the 85 x 6 px orange rule, a large headline (Roboto Medium), a grey subtitle, the chart, a hairline
# and the footer (notes and "Source: .... Chart: NEIS Center."). Text is wrapped by measured width, so register the
# fonts and open a ragg device before calling cod_card().

COD_CARD_SIGNAL <- "#f58229"
# Layout in inches for a 6.5-inch card (the explainer's chart width, so the chart typography is unchanged): the
# Explorer card's proportions (1200 px wide, 56 px side padding, 8 px top bar, 48 px logo, 85 x 6 px rule) scaled to
# 6.5 inches, with the type sizes raised to sit above the charts' 9.5 pt body text. Line heights are multiples of the
# font's own line height (Roboto: 1.17 em), as ragg sets them, so 1.02 / 1.2 / 1.22 match the card's CSS line-heights
# 1.18 / 1.4 / 1.5 for the headline, subtitle and footer. Text blocks are measured on the device, not estimated.
COD_CARD <- list(width = 6.5, pad_x = 0.2, top_bar = 0.045, pad_top = 0.19, logo_h = 0.29, pad_rule = 0.15,
                 rule_w = 0.46, rule_h = 0.034, pad_title = 0.17, title_pt = 15, title_lead = 1.02, pad_sub = 0.10,
                 sub_pt = 9.6, sub_lead = 1.2, pad_chart = 0.12, pad_footer = 0.10, pad_hair = 0.09, foot_pt = 6.8,
                 foot_lead = 1.22, foot_gap = 0.07, pad_bottom = 0.19, eyebrow_pt = 6.6, label_pt = 7.2)

# NEIS Center logo: path data copied from the Explorer's Logo component (dashboard/src/components/Logo.tsx, itself a
# copy of NEIS/src/components/Logo.tsx). fill NA = the wordmark (takes the ground's ink). viewBox about 3.2:1.
COD_LOGO_VIEWBOX <- c(x = 44.0, y = 306.8, w = 524.0, h = 164.4)
COD_LOGO_PATHS <- data.frame(
  fill = c("#f58229", "#1889b0", "#a3b63b", "#55c4c5", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  d = c(
    "M344.3,369.7c-4.3,41.8-73.7,67.5-138.2-1.8-18-19.3-61.7-81-119.3-52.6-18.5,9.1-37.7,50.2,13.6,67.5-11.5-19.4-6.6-35.3,6.1-39.9,24.8-9,49.6,6.7,90.5,44.1,68.5,62.4,147.3,32.1,147.3-17.2",
    "M47.2,331.8c0,49.4,76.4,100.5,76.4,27,0,0,28.8,19.1,23.2,44-13.1,57.9-122.8,47.9-99.6-71",
    "M141.6,361.1c44,25.1,40.9,67.6,35.3,78.8-8.3,16.7-28.8,8.1-21.7-7.5,5.7-14.2,13.8-42-13.6-71.2",
    "M186.4,387.1c14.1,58.1,70,75.1,84.5,72.4,21.6-3.9,18.6-30-1.5-27-17.9,1.9-58.8-4.8-83-45.3",
    "M265,378.6c0,3.9,0,19.8.4,22.8-1,1.4-2.8,1.9-4.4,1.8-2.1-2.7-5.8-7.1-15.1-17.7l-30.3-34.5c-8.3-9.6-12.6-14.4-14.8-16.3-.4,0-.4,1.4-.4,8.3v25.2c0,9.7.1,21.6,2.3,25.5,1.2,1.9,3.4,2.8,6.6,3.2l2.8.4c1.2.9,1,3.4-.3,3.8-4.8-.3-9.8-.4-14.8-.4s-8.9.1-13.5.4c-1-.6-1.3-2.8-.3-3.8l2.8-.6c2.5-.5,4.8-.9,5.7-3,1.7-4,1.7-15.3,1.7-25.5v-30.7c0-10.1.1-12.2-4.3-15.9-1.4-1.2-5-2.2-6.9-2.6l-1.9-.4c-.9-.8-.8-3.4.5-3.8,4.9.5,12,.4,15.3.4s5.8-.1,8.9-.4c2.2,5.6,16.3,21.9,20.8,26.8l12.9,14c5.4,6,17.5,19.7,18.8,20.4.4-.4.4-1,.4-3.1v-25.2c0-9.7-.1-21.6-2.5-25.5-1.2-1.9-3.2-2.8-6.5-3.2l-3-.4c-1.2-.9-1-3.4.3-3.8,5,.3,9.8.4,14.9.4s8.9-.1,13.6-.4c1,.6,1.3,2.8.3,3.8l-3,.6c-2.5.5-4.7.9-5.6,3-1.8,4-1.8,15.3-1.8,25.5v30.9Z",
    "M291.4,333.8c0-13.7-1.2-14.2-7.2-15l-2.1-.3c-.9-.9-.8-3.2.3-3.8,4.9.3,11,.4,16.7.4h24.7c8.3,0,15.8-.1,17.5-.4.5,2.2,1.3,12.4,2.1,18-.5,1-3.1,1.3-4.1.5-2.1-7.5-4-10.6-8.2-11.8-3-.8-6.9-1.2-10.4-1.2h-8.7c-5.6,0-5.6.3-5.6,7.6v21.9c0,3.4.1,3.5,3.1,3.5h6.2c11.3,0,13.2-.1,14.4-5.7l.9-4.4c1-.9,3.5-.9,4.1.1-.1,3.5-.4,8.2-.4,12.9s.3,9.4.4,13.6c-.6.9-3.1.9-4.1.1l-.9-5.2c-.9-5.4-3.1-5.7-14.4-5.7h-6.2c-3,0-3.1.3-3.1,3.5v16.4c0,6.6.3,11.3,2.2,13.5,1.6,1.8,3.8,2.7,14.6,2.8,16.4.1,19.3-3.5,23.8-14,1.2-.9,3.4-.5,4.1.8-.8,5.4-3.6,15.3-5.4,18.8-5.7-.3-16.8-.4-32-.4h-14.8c-5.7,0-13.6.1-19.4.4-1-.5-1.2-2.8-.3-3.8l4.8-.5c6.1-.6,7.2-1,7.2-14.8v-48.1Z",
    "M381.2,382c0,13.7,1.2,14.1,7.2,14.9l2.8.4c.9.9.8,3.2-.3,3.8-5.7-.3-11.5-.4-17.2-.4s-11.8.1-17.5.4c-1-.5-1.2-2.8-.3-3.8l2.8-.4c6.1-.8,7.2-1.2,7.2-14.9v-48.1c0-13.7-1.2-14.1-7.2-14.9l-2.8-.4c-.9-.9-.8-3.2.3-3.8,5.7.3,11.5.4,17.5.4s11.5-.1,17.2-.4c1,.5,1.2,2.8.3,3.8l-2.8.4c-6.1.8-7.2,1.2-7.2,14.9v48.1Z",
    "M402.5,397.2c-2.3-2.7-4-11.6-3.9-19.7.8-1.3,3-1.4,4-.5,2.3,6.6,9.3,20.4,22,20.4s15.4-6,15.4-13.8-3.5-13.3-13.2-18.8l-5.6-3.1c-9.7-5.4-18.2-13.3-18.2-25s9.6-23.7,28.5-23.7,11.6,1.6,17.7,2.7c1.4,2.8,2.7,11.4,2.7,17.5-.6,1.2-3,1.3-4.1.4-1.9-6.7-6.5-15.4-17.5-15.4s-14.6,6.7-14.6,13.6,3.8,11.3,12.3,15.8l8,4.3c8.4,4.4,17.7,12.4,17.7,25.5s-11.3,25.2-30,25.2-18.6-3.9-21.2-5.4Z",
    "M347.8,429.7c-.5.5-1.6.6-2.1.1-1.1-4.1-4-9.4-12.6-9.4s-16.5,5.8-16.5,19.1,8.6,20.5,17,20.5,11.7-5.4,13.5-9.6c.6-.6,1.6-.3,1.9.3-.5,4.3-2.2,8.6-3.1,9.8-.8.1-1.6.4-2.3.6-1.4.5-5.9,1.6-10.1,1.6-6.4,0-11.9-1.3-16.6-4.3-5.2-3.4-9.2-9.3-9.2-17.8s3.4-13.5,8.2-17.1c4.8-3.6,11.3-5.5,17.9-5.5s8.2.6,11.2,1.4c.8.2,1.2.3,1.8.4.1,1.5.3,5.3.9,10.1Z",
    "M358.6,428.1c0-6.9-.6-7.1-3.6-7.5h-1c-.5-.6-.4-1.7.1-2,2.5.1,5.5.2,8.3.2h12.4c4.1,0,7.9,0,8.7-.2.3,1.1.6,6.2,1,9-.3.5-1.6.6-2.1.3-1-3.8-2-5.3-4.1-5.9-1.5-.4-3.4-.6-5.2-.6h-4.3c-2.8,0-2.8.1-2.8,3.8v10.9c0,1.7,0,1.7,1.6,1.7h3.1c5.6,0,6.6,0,7.2-2.8l.5-2.2c.5-.5,1.7-.5,2.1,0,0,1.7-.2,4.1-.2,6.5s.1,4.7.2,6.8c-.3.5-1.6.5-2.1,0l-.5-2.6c-.5-2.7-1.6-2.8-7.2-2.8h-3.1c-1.5,0-1.6.1-1.6,1.7v8.2c0,3.3.1,5.6,1.1,6.7.8.9,1.9,1.4,7.3,1.4,8.2,0,9.6-1.7,11.9-7,.6-.5,1.7-.3,2.1.4-.4,2.7-1.8,7.6-2.7,9.4-2.8-.1-8.4-.2-16-.2h-7.4c-2.8,0-6.8,0-9.7.2-.5-.3-.6-1.4-.1-1.9l2.4-.3c3-.3,3.6-.5,3.6-7.4v-24.1Z",
    "M432.8,450.5c0,1.9,0,9.9.2,11.4-.5.7-1.4,1-2.2.9-1-1.4-2.9-3.6-7.6-8.9l-15.1-17.3c-4.1-4.8-6.3-7.2-7.4-8.2-.2,0-.2.7-.2,4.1v12.6c0,4.9,0,10.8,1.2,12.7.6,1,1.7,1.4,3.3,1.6l1.4.2c.6.5.5,1.7-.1,1.9-2.4-.1-4.9-.2-7.4-.2s-4.5,0-6.7.2c-.5-.3-.6-1.4-.1-1.9l1.4-.3c1.2-.3,2.4-.5,2.8-1.5.8-2,.8-7.6.8-12.7v-15.3c0-5,0-6.1-2.1-8-.7-.6-2.5-1.1-3.4-1.3l-1-.2c-.5-.4-.4-1.7.3-1.9,2.5.3,6,.2,7.6.2s2.9,0,4.5-.2c1.1,2.8,8.2,10.9,10.4,13.4l6.5,7c2.7,3,8.7,9.8,9.4,10.2.2-.2.2-.5.2-1.6v-12.6c0-4.9,0-10.8-1.2-12.7-.6-1-1.6-1.4-3.2-1.6l-1.5-.2c-.6-.5-.5-1.7.1-1.9,2.5.1,4.9.2,7.4.2s4.5,0,6.8-.2c.5.3.6,1.4.1,1.9l-1.5.3c-1.2.3-2.3.5-2.8,1.5-.9,2-.9,7.6-.9,12.7v15.5Z",
    "M459.5,424.2c0-2.7,0-2.8-1.7-2.8h-3.6c-6,0-7.7.7-10.1,6-.5.4-1.8.2-2.1-.5,1.1-3.5,2.1-7.5,2.5-9.8.1-.2.4-.3.7-.3s.5,0,.6.3c.3,1.6,1.2,1.7,5.6,1.7h26.4c3.2,0,4.1-.2,4.9-1.7.3-.1.5-.3.8-.3.4,0,.7.1.8.3-.6,2.7-1.2,8.2-1,10.3-.3.5-1.4.6-2,.2-.8-5-2.1-6.3-9-6.3h-3.7c-1.7,0-1.7.1-1.7,2.8v27.9c0,6.9.6,7.1,3.6,7.4l2.3.3c.5.5.4,1.6-.1,1.9-3.8-.1-6.7-.2-9.5-.2s-5.9,0-10,.2c-.5-.3-.6-1.6-.1-1.9l2.7-.3c3-.4,3.6-.5,3.6-7.3v-27.9Z",
    "M494,428.1c0-6.9-.6-7.1-3.6-7.5h-1c-.5-.6-.4-1.7.1-2,2.5.1,5.5.2,8.3.2h12.4c4.1,0,7.9,0,8.7-.2.3,1.1.6,6.2,1,9-.3.5-1.6.6-2.1.3-1-3.8-2-5.3-4.1-5.9-1.5-.4-3.4-.6-5.2-.6h-4.3c-2.8,0-2.8.1-2.8,3.8v10.9c0,1.7,0,1.7,1.6,1.7h3.1c5.6,0,6.6,0,7.2-2.8l.5-2.2c.5-.5,1.7-.5,2.1,0,0,1.7-.2,4.1-.2,6.5s.1,4.7.2,6.8c-.3.5-1.6.5-2.1,0l-.5-2.6c-.5-2.7-1.6-2.8-7.2-2.8h-3.1c-1.5,0-1.6.1-1.6,1.7v8.2c0,3.3.1,5.6,1.1,6.7.8.9,1.9,1.4,7.3,1.4,8.2,0,9.6-1.7,11.9-7,.6-.5,1.7-.3,2.1.4-.4,2.7-1.8,7.6-2.7,9.4-2.8-.1-8.4-.2-16-.2h-7.4c-2.8,0-6.8,0-9.7.2-.5-.3-.6-1.4-.1-1.9l2.4-.3c3-.3,3.6-.5,3.6-7.4v-24.1Z",
    "M532.7,428.5c0-6.2,0-7-3.1-7.3l-1.7-.2c-.5-.5-.5-1.5,0-1.8,4.1-.4,9.6-.6,14.9-.6s9.4.6,12.3,2.2c3.3,1.7,5.7,4.8,5.7,9.4s-4.6,8.9-7.8,10.2c-.4.2-.3.7,0,1.1,5.2,8.7,8.4,14.2,11.5,16.8,1.8,1.6,3.7,1.9,4.5,2,.3.3.3,1,.1,1.3-.8.2-2,.4-4.3.4-6.2,0-9.6-2.5-13.5-8.5-1.7-2.7-3.6-6.3-5.1-8.8-1.2-1.9-1.9-2.2-4-2.2s-2,0-2,1.4v8.3c0,6.9.6,7.1,3.6,7.4l1.4.2c.5.5.4,1.6-.1,1.9-2.8-.1-5.8-.2-8.6-.2s-5.8,0-8.7.2c-.5-.3-.7-1.4-.1-1.9l1.4-.2c3-.4,3.6-.6,3.6-7.4v-23.7ZM540.2,437.8c0,2.1,0,2.2,3.1,2.2,6.3,0,9.3-3.1,9.3-9.3s-3.3-9.6-8.9-9.6-3.4.2-3.4,3.2v13.5Z"
  ), stringsAsFactors = FALSE)

# SVG path data -> list of subpaths (two-column matrices x, y in SVG coordinates). Handles M L H V C S Q T Z, absolute
# and relative, with implicit repeats; curves are flattened to `n` segments.
cod_svg_path <- function(d, n = 12) {
  tok <- regmatches(d, gregexpr("[MmLlHhVvCcSsQqTtZz]|-?(?:\\d+\\.?\\d*|\\.\\d+)(?:[eE][-+]?\\d+)?", d, perl = TRUE))[[1]]
  out <- list(); pts <- NULL; cur <- c(0, 0); start <- c(0, 0); ctrl <- NULL; cmd <- NULL; i <- 1
  num <- function(k) { v <- as.numeric(tok[i:(i + k - 1)]); i <<- i + k; v }
  cubic <- function(p0, p1, p2, p3) { t <- seq(0, 1, length.out = n + 1)[-1]
    cbind((1 - t)^3 * p0[1] + 3 * (1 - t)^2 * t * p1[1] + 3 * (1 - t) * t^2 * p2[1] + t^3 * p3[1],
          (1 - t)^3 * p0[2] + 3 * (1 - t)^2 * t * p1[2] + 3 * (1 - t) * t^2 * p2[2] + t^3 * p3[2]) }
  flush <- function() if (!is.null(pts) && nrow(pts) > 1) out[[length(out) + 1]] <<- pts
  while (i <= length(tok)) {
    if (grepl("^[A-Za-z]$", tok[i])) { cmd <- tok[i]; i <- i + 1 }
    rel <- cmd == tolower(cmd); C <- toupper(cmd); o <- if (rel) cur else c(0, 0)
    if (C == "Z") { if (!is.null(pts)) pts <- rbind(pts, start); flush(); pts <- NULL; cur <- start; ctrl <- NULL; next }
    if (C == "M") { flush(); cur <- o + num(2); start <- cur; pts <- matrix(cur, 1); ctrl <- NULL
      cmd <- if (rel) "l" else "L"; next }
    if (C == "L") { cur <- o + num(2); pts <- rbind(pts, cur); ctrl <- NULL; next }
    if (C == "H") { v <- num(1); cur <- c(if (rel) cur[1] + v else v, cur[2]); pts <- rbind(pts, cur); ctrl <- NULL; next }
    if (C == "V") { v <- num(1); cur <- c(cur[1], if (rel) cur[2] + v else v); pts <- rbind(pts, cur); ctrl <- NULL; next }
    if (C == "C") { v <- num(6); p1 <- o + v[1:2]; p2 <- o + v[3:4]; p3 <- o + v[5:6]
      pts <- rbind(pts, cubic(cur, p1, p2, p3)); ctrl <- p2; cur <- p3; next }
    if (C == "S") { v <- num(4); p1 <- if (is.null(ctrl)) cur else 2 * cur - ctrl; p2 <- o + v[1:2]; p3 <- o + v[3:4]
      pts <- rbind(pts, cubic(cur, p1, p2, p3)); ctrl <- p2; cur <- p3; next }
    if (C == "Q") { v <- num(4); q <- o + v[1:2]; p3 <- o + v[3:4]
      pts <- rbind(pts, cubic(cur, cur + 2 / 3 * (q - cur), p3 + 2 / 3 * (q - p3), p3)); ctrl <- q; cur <- p3; next }
    if (C == "T") { v <- num(2); q <- if (is.null(ctrl)) cur else 2 * cur - ctrl; p3 <- o + v
      pts <- rbind(pts, cubic(cur, cur + 2 / 3 * (q - cur), p3 + 2 / 3 * (q - p3), p3)); ctrl <- q; cur <- p3; next }
    stop("Unsupported SVG path command: ", cmd)
  }
  flush()
  out
}

# The NEIS Center logo as a grob, `height` inches tall with its top-left corner at (x, y) (grid units); the wordmark
# takes `ink`.
cod_card_logo <- function(x, y, height, ink) {
  vb <- COD_LOGO_VIEWBOX
  vp <- grid::viewport(x = x, y = y, width = grid::unit(height * vb[["w"]] / vb[["h"]], "in"), height = grid::unit(height, "in"),
                       just = c("left", "top"), xscale = c(vb[["x"]], vb[["x"]] + vb[["w"]]), yscale = c(vb[["y"]] + vb[["h"]], vb[["y"]]))
  kids <- lapply(seq_len(nrow(COD_LOGO_PATHS)), function(k) {
    sp <- cod_svg_path(COD_LOGO_PATHS$d[k])
    xy <- do.call(rbind, sp); id <- rep(seq_along(sp), vapply(sp, nrow, integer(1)))
    fill <- if (is.na(COD_LOGO_PATHS$fill[k])) ink else COD_LOGO_PATHS$fill[k]
    grid::pathGrob(xy[, 1], xy[, 2], id = id, rule = "winding", default.units = "native", gp = grid::gpar(fill = fill, col = NA))
  })
  grid::gTree(children = do.call(grid::gList, kids), vp = vp, name = "neis-logo")
}

# Greedy word wrap by measured width (needs an open device). Existing line breaks are paragraph breaks.
cod_card_wrap <- function(text, width_in, size, family, bold = FALSE) {
  if (is.null(text) || !length(text) || !nzchar(paste(text, collapse = ""))) return(character())
  w_pt <- width_in * 72
  # measured on the current device, the same engine that draws the text (systemfonts::string_width ran ~4% narrow)
  sw <- function(s) grid::convertWidth(grid::grobWidth(grid::textGrob(s, gp = grid::gpar(fontfamily = family, fontsize = size,
                                                                                       fontface = if (bold) "bold" else "plain"))), "pt", valueOnly = TRUE)
  unlist(lapply(strsplit(paste(text, collapse = "\n"), "\n", fixed = TRUE)[[1]], function(par) {
    words <- strsplit(trimws(par), "[ \t]+")[[1]]
    if (!length(words)) return(character())
    lines <- character(); cur <- ""
    for (w in words) {
      cand <- if (nzchar(cur)) paste(cur, w) else w
      if (!nzchar(cur) || sw(cand) <= w_pt) cur <- cand else { lines <- c(lines, cur); cur <- w }
    }
    c(lines, cur)
  }))
}

# Footer paragraphs: the chart's notes with "Chart: NEIS Center." appended to the source line (the last paragraph that
# contains "Source:"/"Sources:"), or as its own line when there is none. A no-break space keeps "NEIS Center" together.
cod_card_notes <- function(notes, credit = "Chart: NEIS\u00a0Center.") {
  notes <- trimws(unlist(strsplit(paste(notes, collapse = "\n"), "\n", fixed = TRUE)))
  notes <- notes[nzchar(notes)]
  k <- utils::tail(grep("(^|[.;:] )Sources?:", notes), 1)
  if (length(k)) notes[k] <- paste(sub("([^.])$", "\\1.", notes[k]), credit) else notes <- c(notes, credit)
  notes
}

# A ggplot as a card body: title, subtitle and caption removed (the card sets them), top and bottom margins trimmed and
# the chart's own side margins kept (some charts reserve room for labels outside the panel), with the panel area of
# the chart as published at `h` inches. `p_ref`: the chart with its published (wrapped) labels, for that measurement;
# by default `p` itself. Measures on the current device.
cod_card_body <- function(p, h, p_ref = p, spec = COD_CARD) {
  fixed <- function(g) { hs <- g$heights; sum(grid::convertHeight(hs[grid::unitType(hs) != "null"], "in", valueOnly = TRUE)) }
  g0 <- ggplot2::ggplotGrob(p_ref)
  side <- grid::convertWidth(g0$widths[c(1, length(g0$widths))], "pt", valueOnly = TRUE)
  p2 <- p + ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL) +
    ggplot2::theme(plot.margin = ggplot2::margin(6, side[2], 4, side[1]), plot.background = ggplot2::element_rect(fill = NA, colour = NA))
  g2 <- ggplot2::ggplotGrob(p2)
  list(grob = g2, height = h - fixed(g0) + fixed(g2), title = p$labels$title, subtitle = p$labels$subtitle,
       caption = p$labels$caption)
}

# The card (measures its text on the current device: open a ragg device first). `body`: a grob drawn full width, `body_h` inches tall. `ink`: cod_ink(ground). `family_medium`: the headline
# and eyebrow face (Roboto Medium). Returns list(grob, width, height) in inches.
cod_card <- function(body, body_h, title, subtitle = NULL, notes = character(), ink, family = "", family_medium = family,
                     eyebrow = "", label = "", spec = COD_CARD) {
  W <- spec$width; px <- spec$pad_x; tw <- W - 2 * px
  # a text block advances by whole line pitches, measured on the device as the height of two lines minus one (a
  # textGrob's height leaves out the last line's descent and leading, which made paragraphs touch)
  gh <- function(x, pt, lead, fam) grid::convertHeight(grid::grobHeight(grid::textGrob(x, gp = grid::gpar(fontfamily = fam, fontsize = pt,
                                                                                                         lineheight = lead))), "in", valueOnly = TRUE)
  block_h <- function(lines, pt, lead, fam) if (!length(lines)) 0 else length(lines) * (gh("Hg\nHg", pt, lead, fam) - gh("Hg", pt, lead, fam))
  one <- function(x) paste(gsub("\\s*\n\\s*", " ", x), collapse = " ")
  t_lines <- cod_card_wrap(one(title), tw, spec$title_pt, family_medium)
  s_lines <- cod_card_wrap(if (length(subtitle)) one(subtitle) else NULL, tw, spec$sub_pt, family)
  paras <- lapply(cod_card_notes(notes), cod_card_wrap, width_in = tw, size = spec$foot_pt, family = family)
  paras <- paras[lengths(paras) > 0]
  # vertical positions, inches from the top
  y_logo <- spec$top_bar + spec$pad_top
  y_rule <- y_logo + spec$logo_h + spec$pad_rule
  y_title <- y_rule + spec$rule_h + spec$pad_title
  y <- y_title + block_h(t_lines, spec$title_pt, spec$title_lead, family_medium)
  y_sub <- y + spec$pad_sub
  if (length(s_lines)) y <- y_sub + block_h(s_lines, spec$sub_pt, spec$sub_lead, family)
  y_body <- y + spec$pad_chart
  y_hair <- y_body + body_h + spec$pad_footer
  y_par <- numeric(length(paras)); y <- y_hair + spec$pad_hair
  for (k in seq_along(paras)) { y_par[k] <- y; y <- y + block_h(paras[[k]], spec$foot_pt, spec$foot_lead, family) + spec$foot_gap }
  H <- y - spec$foot_gap + spec$pad_bottom
  top <- function(off) grid::unit(1, "npc") - grid::unit(off, "in")
  txt <- function(lines, off, pt, lead, col, fam, x = grid::unit(px, "in"), just = c("left", "top"))
    grid::textGrob(paste(lines, collapse = "\n"), x = x, y = top(off), just = just,
                   gp = grid::gpar(fontfamily = fam, fontsize = pt, lineheight = lead, col = col))
  kids <- list(
    grid::rectGrob(gp = grid::gpar(fill = ink[["surface"]], col = NA), name = "ground"),
    grid::rectGrob(x = 0, y = grid::unit(1, "npc"), width = grid::unit(1, "npc"), height = grid::unit(spec$top_bar, "in"),
                   just = c("left", "top"), gp = grid::gpar(fill = COD_CARD_SIGNAL, col = NA), name = "top-bar"),
    cod_card_logo(grid::unit(px, "in"), top(y_logo), spec$logo_h, ink[["primary"]]),
    txt(toupper(eyebrow), y_logo + 0.035, spec$eyebrow_pt, 1, ink[["muted"]], family_medium,
        x = grid::unit(1, "npc") - grid::unit(px, "in"), just = c("right", "top")),
    txt(label, y_logo + 0.035 + spec$eyebrow_pt * 1.75 / 72, spec$label_pt, 1, ink[["muted"]], family,
        x = grid::unit(1, "npc") - grid::unit(px, "in"), just = c("right", "top")),
    grid::rectGrob(x = grid::unit(px, "in"), y = top(y_rule), width = grid::unit(spec$rule_w, "in"), height = grid::unit(spec$rule_h, "in"),
                   just = c("left", "top"), gp = grid::gpar(fill = COD_CARD_SIGNAL, col = NA), name = "rule"),
    txt(t_lines, y_title, spec$title_pt, spec$title_lead, ink[["primary"]], family_medium))
  if (length(s_lines)) kids <- c(kids, list(txt(s_lines, y_sub, spec$sub_pt, spec$sub_lead, ink[["secondary"]], family)))
  kids <- c(kids, list(
    grid::gTree(children = grid::gList(body), name = "chart",
                vp = grid::viewport(x = 0, y = top(y_body), width = grid::unit(1, "npc"), height = grid::unit(body_h, "in"), just = c("left", "top"))),
    grid::segmentsGrob(x0 = grid::unit(px, "in"), x1 = grid::unit(1, "npc") - grid::unit(px, "in"), y0 = top(y_hair), y1 = top(y_hair),
                       gp = grid::gpar(col = ink[["hairline"]], lwd = 0.8), name = "hairline")))
  for (k in seq_along(paras)) kids <- c(kids, list(txt(paras[[k]], y_par[k], spec$foot_pt, spec$foot_lead, ink[["muted"]], family)))
  list(grob = grid::gTree(children = do.call(grid::gList, kids), name = "neis-card"), width = W, height = H)
}

# Card label from the chart registry title ("3. The fence ..." -> "Chart 3"; "Sidebar: ..." -> "Sidebar").
cod_card_label <- function(title) {
  n <- regmatches(title, regexpr("^[0-9]+[a-z]?(?=\\.)", title, perl = TRUE))
  if (length(n)) return(paste("Chart", n))
  k <- regmatches(title, regexpr("^[A-Za-z]+(?=:)", title, perl = TRUE))
  names_k <- c(Social = "Social graphic", Sidebar = "Sidebar", Analysis = "Analysis")
  if (!length(k)) "" else if (k %in% names(names_k)) names_k[[k]] else k
}
