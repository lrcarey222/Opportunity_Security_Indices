# Figure: the median-centred S-curve used to normalise every OSI component.
#
# Panel (a) plots phi_gamma(r) = r^gamma / (r^gamma + (1 - r)^gamma) against the percent rank
# r for gamma in {0.25, 0.5, 1, 2}; gamma = 0.5 is the value the pipeline uses (the default of
# median_scurve() in R/utils/scurve.R and of every theme builder that calls it).
# Panel (b) takes a simulated right-skewed (lognormal) raw variable and shows each observation
# under three normalisations - a min-max rescaling of the raw value, the percent rank r, and
# the S-curved index produced by median_scurve() itself - with the same observation joined
# across rows. The bottom and top tenths of the sample are highlighted, so the figure shows
# the S-curve stretching the tails and compressing the middle relative to the percent rank.
#
# No pipeline data are read: the figure depends only on the function and a fixed-seed draw.
# Output: paper/figures/fig_scurve.pdf (+ png twin) and paper/results/scurve.json.

find_repo_root <- function() {
  d <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  while (!file.exists(file.path(d, "scripts", "paper", "_paper_theme.R"))) {
    if (dirname(d) == d) stop("Run from inside the repository.")
    d <- dirname(d)
  }
  d
}

repo_root <- find_repo_root()
source(file.path(repo_root, "scripts", "paper", "_paper_theme.R"))
source(file.path(repo_root, "R", "utils", "scurve.R"))

suppressPackageStartupMessages(library(cowplot))

# Analytic form, checked against the pipeline function on a grid of ranks.
phi <- function(r, gamma) r^gamma / (r^gamma + (1 - r)^gamma)

grid_check <- (0:20) / 20
stopifnot(isTRUE(all.equal(median_scurve(grid_check), phi(grid_check, 0.5))))
# Properties stated in Section 4: symmetry, logit-linearity, slope gamma at the median.
stopifnot(isTRUE(all.equal(phi(1 - grid_check, 0.5), 1 - phi(grid_check, 0.5))))
inner <- grid_check[grid_check > 0 & grid_check < 1]
stopifnot(isTRUE(all.equal(qlogis(phi(inner, 0.5)), 0.5 * qlogis(inner))))
h <- 1e-6
stopifnot(abs((phi(0.5 + h, 0.5) - phi(0.5 - h, 0.5)) / (2 * h) - 0.5) < 1e-6)
stopifnot(isTRUE(all.equal(phi(c(0.1, 0.9), 0.5), c(0.25, 0.75))))

hl <- paper_highlight()

# ---------------------------------------------------------------------------
# Panel (a): the S-curve family
# ---------------------------------------------------------------------------
gammas <- c(0.25, 0.5, 1, 2)
r_grid <- seq(0, 1, length.out = 501)

gamma_levels <- c("0.25", "0.5", "1", "2")
# plotmath labels: the pdf() device cannot be relied on to carry a Unicode gamma.
gamma_labels <- c(
  expression(gamma == 0.25),
  expression(gamma == 0.5 ~ "(OSI)"),
  expression(gamma == 1 ~ "(percent rank)"),
  expression(gamma == 2)
)

curves <- do.call(rbind, lapply(gammas, function(g) {
  data.frame(r = r_grid, value = phi(r_grid, g), gamma = format(g))
}))
curves$gamma <- factor(trimws(curves$gamma), levels = gamma_levels)
# Draw the highlighted curve last so it sits on top.
curves <- curves[order(curves$gamma == "0.5"), ]

ref_points <- data.frame(r = c(0.1, 0.5, 0.9), value = phi(c(0.1, 0.5, 0.9), 0.5))

panel_a <- ggplot(curves, aes(r, value, group = gamma, colour = gamma, linetype = gamma,
                              linewidth = gamma)) +
  geom_segment(data = ref_points, aes(x = r, xend = r, y = 0, yend = value),
               inherit.aes = FALSE, colour = paper_ink$muted, linewidth = 0.25,
               linetype = "dotted") +
  geom_segment(data = ref_points, aes(x = 0, xend = r, y = value, yend = value),
               inherit.aes = FALSE, colour = paper_ink$muted, linewidth = 0.25,
               linetype = "dotted") +
  geom_line() +
  geom_point(data = ref_points, aes(r, value), inherit.aes = FALSE,
             colour = hl[["highlight"]], size = 1.6) +
  scale_colour_manual(
    values = c("0.25" = paper_ink$muted, "0.5" = hl[["highlight"]],
               "1" = paper_ink$muted, "2" = paper_ink$muted),
    labels = gamma_labels, name = NULL
  ) +
  scale_linetype_manual(
    values = c("0.25" = "longdash", "0.5" = "solid", "1" = "solid", "2" = "dashed"),
    labels = gamma_labels, name = NULL
  ) +
  scale_linewidth_manual(
    values = c("0.25" = 0.5, "0.5" = 1.1, "1" = 0.35, "2" = 0.5),
    labels = gamma_labels, name = NULL
  ) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE),
         linetype = guide_legend(nrow = 2, byrow = TRUE),
         linewidth = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_x_continuous(breaks = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1),
                     labels = c("0", "0.1", "0.25", "0.5", "0.75", "0.9", "1"),
                     expand = expansion(mult = 0.01)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), expand = expansion(mult = 0.01)) +
  coord_equal() +
  labs(x = "Percent rank r", y = expression(phi[gamma](r))) +
  theme_paper(grid = "xy") +
  theme(legend.key.width = unit(0.9, "cm"), legend.margin = margin(t = 0),
        plot.margin = margin(16, 8, 6, 6))

# ---------------------------------------------------------------------------
# Panel (b): one skewed raw variable under three normalisations
# ---------------------------------------------------------------------------
set.seed(20260930)
n_obs <- 40
raw <- sort(stats::rlnorm(n_obs, meanlog = 0, sdlog = 1))

mapped <- data.frame(
  id = seq_len(n_obs),
  raw = raw,
  minmax = (raw - min(raw)) / (max(raw) - min(raw)),
  rank = (rank(raw, ties.method = "average") - 1) / (n_obs - 1),
  scurve = median_scurve(raw)
)
stopifnot(isTRUE(all.equal(mapped$scurve, phi(mapped$rank, 0.5))))

n_tail <- n_obs / 10
mapped$group <- ifelse(mapped$id <= n_tail | mapped$id > n_obs - n_tail, "tail", "middle")

# Rows from top to bottom: min-max (y = 3), percent rank (y = 2), S-curve index (y = 1).
row_y <- c(minmax = 3, rank = 2, scurve = 1)
row_labels <- c(
  expression(atop("S-curve", phi[0.5](r))),
  expression(atop("Percent", "rank " * r)),
  expression(atop("Min-max", "of raw"))
)

points_long <- do.call(rbind, lapply(names(row_y), function(v) {
  data.frame(id = mapped$id, group = mapped$group, x = mapped[[v]], y = row_y[[v]])
}))
links <- rbind(
  data.frame(id = mapped$id, group = mapped$group, x = mapped$minmax, xend = mapped$rank,
             y = row_y[["minmax"]], yend = row_y[["rank"]]),
  data.frame(id = mapped$id, group = mapped$group, x = mapped$rank, xend = mapped$scurve,
             y = row_y[["rank"]], yend = row_y[["scurve"]])
)
# Draw the highlighted tails on top of the gray middle.
points_long <- points_long[order(points_long$group == "tail"), ]
links <- links[order(links$group == "tail"), ]

group_cols <- c(tail = hl[["highlight"]], middle = hl[["other"]])

gap <- function(v) diff(range(v))
lower_tail <- mapped[mapped$id <= n_tail, ]
upper_tail <- mapped[mapped$id > n_obs - n_tail, ]
middle <- mapped[mapped$id %in% ((n_obs / 2 - 1):(n_obs / 2 + 2)), ]

# Brackets under the S-curve row marking the span of the bottom and top tenths.
span_tbl <- data.frame(
  x = c(min(lower_tail$scurve), min(upper_tail$scurve)),
  xend = c(max(lower_tail$scurve), max(upper_tail$scurve)),
  y = 0.66
)
span_tbl$label <- sprintf("span %.2f", span_tbl$xend - span_tbl$x)
ticks_tbl <- data.frame(x = c(span_tbl$x, span_tbl$xend), y = 0.66)

panel_b <- ggplot() +
  geom_segment(data = links, aes(x = x, xend = xend, y = y, yend = yend, colour = group),
               linewidth = 0.3, alpha = 0.8) +
  geom_point(data = points_long, aes(x = x, y = y, colour = group), size = 1.3) +
  geom_segment(data = span_tbl, aes(x = x, xend = xend, y = y, yend = y),
               colour = paper_ink$secondary, linewidth = 0.35) +
  geom_segment(data = ticks_tbl, aes(x = x, xend = x, y = y, yend = y + 0.12),
               colour = paper_ink$secondary, linewidth = 0.35) +
  geom_text(data = span_tbl, aes(x = (x + xend) / 2, y = y - 0.16, label = label),
            size = 2.5, colour = paper_ink$secondary) +
  scale_colour_manual(values = group_cols,
                      labels = c(tail = "Bottom and top tenths", middle = "Middle 80%"),
                      breaks = c("tail", "middle"), name = NULL) +
  scale_y_continuous(breaks = c(1, 2, 3), labels = row_labels,
                     limits = c(0.4, 3.15), expand = expansion(mult = 0)) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(0, 1),
                     expand = expansion(mult = 0.02)) +
  labs(x = "Normalised score", y = NULL) +
  theme_paper(grid = "x") +
  theme(axis.text.y = element_text(hjust = 0.5, lineheight = 0.9),
        axis.line.x = element_blank(),
        legend.margin = margin(t = 0),
        plot.margin = margin(16, 8, 6, 6))

# Panel letters only; the caption in the paper carries the titles.
fig <- cowplot::plot_grid(panel_a, panel_b, nrow = 1, rel_widths = c(1, 1.15), align = "h",
                          axis = "bt", labels = c("(a)", "(b)"), label_size = 9,
                          label_fontface = "bold", hjust = 0, label_x = 0)

save_paper_fig(fig, "fig_scurve", width = paper_width_full, height = 3.4)

# ---------------------------------------------------------------------------
# Numbers quoted in Section 4 (all analytic or from the fixed-seed draw).
# ---------------------------------------------------------------------------
write_paper_results(
  list(
    gamma = 0.5,
    phi_0.5 = list(
      r_0.01 = phi(0.01, 0.5), r_0.05 = phi(0.05, 0.5), r_0.10 = phi(0.10, 0.5),
      r_0.25 = phi(0.25, 0.5), r_0.50 = phi(0.50, 0.5), r_0.60 = phi(0.60, 0.5),
      r_0.75 = phi(0.75, 0.5), r_0.90 = phi(0.90, 0.5), r_0.95 = phi(0.95, 0.5),
      r_0.99 = phi(0.99, 0.5)
    ),
    interquartile_span_gamma_0.5 = phi(0.75, 0.5) - phi(0.25, 0.5),
    phi_2 = list(r_0.10 = phi(0.10, 2), r_0.25 = phi(0.25, 2)),
    slope_at_median_gamma_0.5 = 0.5,
    # Dispersion of an untied S-curved variable: the population standard deviation of the
    # grid phi((0:(n-1))/(n-1)), its limit sd(phi(U)) with U ~ Uniform(0, 1), and the
    # smallest score above zero, phi(1/(n-1)).
    dispersion = list(
      grid_sd_gamma_0.5 = stats::setNames(
        lapply(c(5, 10, 20, 40), function(n) {
          v <- phi((0:(n - 1)) / (n - 1), 0.5)
          sqrt(mean((v - mean(v))^2))
        }),
        paste0("n_", c(5, 10, 20, 40))
      ),
      limit_sd = stats::setNames(
        lapply(gammas, function(g) {
          sqrt(stats::integrate(function(u) (phi(u, g) - 0.5)^2, 0, 1)$value)
        }),
        paste0("gamma_", gammas)
      ),
      smallest_positive_step_gamma_0.5 = stats::setNames(
        lapply(c(5, 10, 20, 40), function(n) phi(1 / (n - 1), 0.5)),
        paste0("n_", c(5, 10, 20, 40))
      )
    ),
    edge_cases = list(
      single_observation = median_scurve(5),
      two_observations = median_scurve(c(1, 2)),
      all_tied = median_scurve(c(1, 1, 1)),
      tied_zeros = median_scurve(c(0, 0, 0, 5)),
      zero_anchor_positive = median_scurve(c(0, 2, 5, 9))[-1]
    ),
    simulated_panel_b = list(
      seed = 20260930, n = n_obs, meanlog = 0, sdlog = 1,
      share_below_0.1_minmax = mean(mapped$minmax < 0.1),
      bottom4_spread = list(minmax = gap(lower_tail$minmax), rank = gap(lower_tail$rank),
                            scurve = gap(lower_tail$scurve)),
      middle4_spread = list(minmax = gap(middle$minmax), rank = gap(middle$rank),
                            scurve = gap(middle$scurve)),
      top4_spread = list(minmax = gap(upper_tail$minmax), rank = gap(upper_tail$rank),
                         scurve = gap(upper_tail$scurve))
    )
  ),
  "scurve"
)

message("Wrote ", file.path(paper_dir("figures"), "fig_scurve.pdf"))
