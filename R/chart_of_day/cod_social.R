# Chart of the Day animated posts (pure): the 1080 x 1350 px (4:5) social frame, the two grounds (NEIS navy "blue" and
# "white"), easing and timing helpers, and the house ggplot theme. The method is the trade-fence explainer's LinkedIn
# carousel (osi-worktrees/tri: R/trade_fence_explainer/neis_animation.R, scripts/trade_fence_explainer/
# 15_animate_carousel.R): each chart type has a frame function cod_anim_<type>(spec, t, family) in
# cod_anim_<type>.R, where t runs from 0 (start) to 1 (the finished chart) and the same t always gives the same frame;
# scripts/chart_of_day/03_animate.R draws the frames inside cod_social_card() and encodes MP4, GIF and a still.
#
# Grounds: chart functions take every colour from COD_INK (text and marks on the ground) and COD_ANIM_COL (series),
# which the script sets for a ground with list2env(cod_anim_palette(ground), globalenv()).

COD_SOCIAL <- list(width = 5.4, height = 6.75, dpi = 200, fps = 24, anim_seconds = 6, hold_seconds = 2.5,
                   pad_x = 0.2, top_bar = 0.045, pad_top = 0.19, logo_h = 0.29, pad_rule = 0.15, rule_w = 0.46, rule_h = 0.034,
                   pad_title = 0.17, title_pt = 15.5, title_lead = 1.02, pad_sub = 0.10, sub_pt = 9.4, sub_lead = 1.2,
                   pad_chart = 0.12, pad_footer = 0.10, pad_hair = 0.09, foot_pt = 6.6, foot_lead = 1.22, foot_gap = 0.07,
                   pad_bottom = 0.19, eyebrow_pt = 6.6, label_pt = 7.2)

# Ink and surface roles for a ground (the explainer's tfe_ink(): white is the NEIS ink scale, blue the Chart of the Day
# navy ground with white type).
cod_ink <- function(ground = c("blue", "white")) {
  switch(match.arg(ground),
         white = c(primary = "#111827", secondary = "#374151", muted = "#6b7280", grid = "#e5e7eb", faint = "#f3f4f6",
                   surface = "#ffffff", hairline = "#d1d5db", placeholder = "#9ca3af"),
         blue = c(primary = "#ffffff", secondary = "#d1d5db", muted = "#9ca3af", grid = "#14314d", faint = "#0e2a47",
                  surface = "#001f3e", hairline = "#33506d", placeholder = "#7d8b99"))
}

# Every colour constant the animations use, for a ground. Series colours are the Explorer's (ChartOfDayFigure.tsx):
# the focus country in NEIS orange, the rest in a grey that recedes on the ground; exports green, imports blue (a
# lighter blue on navy, as in the explainer's blue palette); UN-voting blocs as on the dashboard.
cod_anim_palette <- function(ground = c("blue", "white")) {
  ground <- match.arg(ground)
  pick <- function(white, blue) if (ground == "white") white else blue
  list(
    COD_INK = cod_ink(ground),
    COD_TEXT_BASE = pick("black", "#ffffff"),
    COD_ANIM_COL = c(focus = "#f58229", other = pick("#9ca3af", "#5d7189"), other_line = pick("#9ca3af", "#7d8b99"),
                     exports = "#a3b63b", imports = pick("#016787", "#3fa5cd"),
                     us_leaning = "#1889b0", china_leaning = "#c96f7a", nonaligned = pick("#6b7280", "#9ca3af"),
                     others = pick("#d1d5db", "#33506d")))
}
list2env(cod_anim_palette("blue"), environment())

# Easing and timing (vectorised over t).
cod_clamp01 <- function(x) pmin(pmax(x, 0), 1)
cod_ease <- function(t) { t <- cod_clamp01(t); ifelse(t < 0.5, 4 * t^3, 1 - (-2 * t + 2)^3 / 2) }  # cubic in-out
cod_ease_out <- function(t) { t <- cod_clamp01(t); 1 - (1 - t)^3 }
# progress of a step that runs from a to b on the 0-1 clock
cod_phase <- function(t, a, b) cod_clamp01((t - a) / (b - a))
# progress of item i of n when the items start one after another, each taking `d` of the window [a, b]
cod_stagger <- function(t, i, n, a = 0, b = 1, d = 0.35) {
  start <- a + (i - 1) / max(n - 1, 1) * (b - a) * (1 - d)
  cod_phase(t, start, start + (b - a) * d)
}

# Text size in points to ggplot2 mm.
cod_pt <- function(x) x / ggplot2::.pt

# The house theme (the explainer's tfe_theme()), sized for the 5.4-inch social body.
cod_theme <- function(base = 9.5, family = "") {
  ggplot2::theme_minimal(base_size = base, base_family = family) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = COD_INK[["grid"]], linewidth = 0.3),
      axis.text = ggplot2::element_text(colour = COD_INK[["secondary"]]),
      axis.title = ggplot2::element_text(colour = COD_INK[["secondary"]], size = ggplot2::rel(0.92)),
      legend.position = "none",
      text = ggplot2::element_text(colour = COD_TEXT_BASE),
      plot.background = ggplot2::element_rect(fill = NA, colour = NA),
      panel.background = ggplot2::element_rect(fill = NA, colour = NA),
      plot.margin = ggplot2::margin(4, 14, 2, 12))
}

# No grid and no ticks (bar charts). cod_theme() sets panel.grid.major itself, so blank it by name.
cod_no_grid <- function() {
  ggplot2::theme(panel.grid = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank())
}

# Money for labels: "$12bn", "$1.5bn", "$430m".
cod_usd_short <- function(x) ifelse(abs(x) >= 1e10, sprintf("$%.0fbn", x / 1e9),
                             ifelse(abs(x) >= 1e9, sprintf("$%.1fbn", x / 1e9), sprintf("$%.0fm", x / 1e6)))

# A value in a spec's format, for labels (matches the Explorer's fmtValue()).
cod_fmt <- function(v, fmt) {
  switch(fmt,
         pct = ifelse(v > 0 & v < 0.01, sprintf("%.1f%%", 100 * v), sprintf("%.0f%%", 100 * v)),
         pct2 = sprintf("%.2f%%", 100 * v),
         usd = cod_usd_short(v),
         usd_mwh = sprintf("$%.0f/MWh", v),
         count = format(round(v), big.mark = ","),
         ifelse(abs(v) >= 100, format(round(v), big.mark = ","), format(signif(v, 2))))
}

# The first n sentences of a text (a sentence ends with a full stop before a capital), for shorter social footers.
cod_first_sentences <- function(x, n) {
  if (is.null(x) || !nzchar(x)) return(x)
  s <- strsplit(x, "(?<=\\.)\\s+(?=[A-Z])", perl = TRUE)[[1]]
  paste(utils::head(s, n), collapse = " ")
}

# A chart frame as the card body: a ggplot with title/subtitle/caption removed and a transparent outer background, so
# the card's ground shows.
cod_social_body <- function(x) {
  if (!inherits(x, "ggplot")) return(x)
  ggplot2::ggplotGrob(x + ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL) +
                        ggplot2::theme(plot.margin = ggplot2::margin(4, 14, 2, 12),
                                       plot.background = ggplot2::element_rect(fill = NA, colour = NA)))
}

# The social card at the fixed size: cod_card()'s frame with the chart given whatever height the text leaves. Measures
# text on the current device (open a ragg device first). Returns list(grob, width, height, body_h).
cod_social_card <- function(body, title, subtitle, notes, ink, family = "", family_medium = family, eyebrow = "", label = "",
                            spec = COD_SOCIAL) {
  chrome <- cod_card(grid::nullGrob(), 0, title, subtitle, notes, ink = ink, family = family, family_medium = family_medium,
                     eyebrow = eyebrow, label = label, spec = spec)
  body_h <- spec$height - chrome$height
  cd <- cod_card(cod_social_body(body), body_h, title, subtitle, notes, ink = ink, family = family, family_medium = family_medium,
                 eyebrow = eyebrow, label = label, spec = spec)
  cd$body_h <- body_h
  cd
}
