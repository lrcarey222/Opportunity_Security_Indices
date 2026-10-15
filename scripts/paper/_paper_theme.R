# Shared look for every figure in paper/.
#
# Sourced by the scripts/paper/fig_*.R builders so the paper reads as one
# system: one sans face, hairline recessive grid, a fixed categorical order,
# single-hue sequential ramps and a blue <-> gray <-> red diverging scale.
# Figures are sized for a 6.5in text block and written as vector PDF.

suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
})

paper_ink <- list(
  primary   = "#0b0b0b",
  secondary = "#52514e",
  muted     = "#898781",
  grid      = "#e1e0d9",
  axis      = "#c3c2b7",
  surface   = "#ffffff",
  neutral   = "#f0efec"
)

# Categorical slots in fixed order. Never cycle past eight: fold the tail into
# "Other", facet, or highlight one series against gray (paper_highlight()).
paper_cat <- c(
  blue    = "#2a78d6",
  orange  = "#eb6834",
  aqua    = "#1baf7a",
  yellow  = "#eda100",
  magenta = "#e87ba4",
  green   = "#008300",
  violet  = "#4a3aa7",
  red     = "#e34948"
)

# Supply-chain stages always wear the same three slots (the three that
# validate against every pair, so they are safe in scatters and maps too).
paper_stage_cols <- c(
  Upstream   = unname(paper_cat["blue"]),
  Midstream  = unname(paper_cat["orange"]),
  Downstream = unname(paper_cat["aqua"])
)

# Pillars / index families: fixed identity across every figure.
paper_pillar_cols <- c(
  "Energy Security"      = unname(paper_cat["blue"]),
  "Economic Opportunity" = unname(paper_cat["orange"]),
  "Policy"               = unname(paper_cat["aqua"]),
  "Supply-chain weight"  = unname(paper_cat["yellow"]),
  "Technology weight"    = unname(paper_cat["violet"])
)

paper_seq_blue <- c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5",
                    "#256abf", "#184f95", "#0d366b")
paper_seq_orange <- c("#fde3d6", "#f8bfa3", "#f39a70", "#eb6834",
                      "#c9521f", "#9c3e16", "#6e2a0e")
paper_div <- c(low = "#256abf", mid = "#f0efec", high = "#d03b3b")

paper_highlight <- function(highlight_col = paper_cat[["blue"]]) {
  c(highlight = highlight_col, other = "#c3c2b7")
}

theme_paper <- function(base_size = 9, base_family = "sans", grid = "y") {
  t <- theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      text              = element_text(colour = paper_ink$primary),
      plot.title        = element_text(size = base_size + 1, face = "bold",
                                       hjust = 0, margin = margin(b = 4)),
      plot.subtitle     = element_text(size = base_size, colour = paper_ink$secondary,
                                       hjust = 0, margin = margin(b = 6)),
      plot.caption      = element_text(size = base_size - 1.5, colour = paper_ink$muted,
                                       hjust = 0, margin = margin(t = 6)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      axis.title        = element_text(size = base_size, colour = paper_ink$secondary),
      axis.text         = element_text(size = base_size - 1, colour = paper_ink$secondary),
      axis.line.x       = element_line(colour = paper_ink$axis, linewidth = 0.3),
      axis.ticks        = element_blank(),
      panel.grid.major  = element_line(colour = paper_ink$grid, linewidth = 0.25),
      panel.grid.minor  = element_blank(),
      strip.text        = element_text(size = base_size - 0.5, face = "bold",
                                       colour = paper_ink$primary, hjust = 0),
      legend.title      = element_text(size = base_size - 1, colour = paper_ink$secondary),
      legend.text       = element_text(size = base_size - 1, colour = paper_ink$primary),
      legend.position   = "bottom",
      legend.key.size   = unit(0.35, "cm"),
      plot.background   = element_rect(fill = paper_ink$surface, colour = NA),
      panel.background  = element_rect(fill = paper_ink$surface, colour = NA),
      plot.margin       = margin(6, 8, 6, 6)
    )
  if (identical(grid, "y")) {
    t <- t + theme(panel.grid.major.x = element_blank())
  } else if (identical(grid, "x")) {
    t <- t + theme(panel.grid.major.y = element_blank())
  } else if (identical(grid, "none")) {
    t <- t + theme(panel.grid.major = element_blank())
  }
  t
}

scale_fill_stage <- function(...) scale_fill_manual(values = paper_stage_cols, ...)
scale_colour_stage <- function(...) scale_colour_manual(values = paper_stage_cols, ...)
scale_fill_seq <- function(..., palette = paper_seq_blue) {
  scale_fill_gradientn(colours = palette, ...)
}
scale_fill_div <- function(..., midpoint = 0.5) {
  scale_fill_gradient2(low = paper_div[["low"]], mid = paper_div[["mid"]],
                       high = paper_div[["high"]], midpoint = midpoint, ...)
}

# Full text width is 6.5in; a half-width panel is 3.1in.
paper_width_full <- 6.5
paper_width_half <- 3.1

paper_repo_root <- function() {
  d <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  while (dirname(d) != d) {
    if (file.exists(file.path(d, "scripts", "paper", "_paper_theme.R"))) return(d)
    d <- dirname(d)
  }
  stop("Run from inside the repository: could not find scripts/paper/_paper_theme.R")
}

paper_dir <- function(...) file.path(paper_repo_root(), "paper", ...)

save_paper_fig <- function(plot, name, width = paper_width_full, height = 4) {
  out_dir <- paper_dir("figures")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  path <- file.path(out_dir, paste0(name, ".pdf"))
  ggsave(path, plot, width = width, height = height, units = "in",
         device = grDevices::pdf, useDingbats = FALSE)
  # A PNG twin makes the figure easy to eyeball during review.
  png_dir <- file.path(out_dir, "png")
  if (!dir.exists(png_dir)) dir.create(png_dir, recursive = TRUE)
  ggsave(file.path(png_dir, paste0(name, ".png")), plot, width = width,
         height = height, units = "in", dpi = 150, bg = "white")
  invisible(path)
}

# Numbers quoted in the text are written here, one JSON file per theme, so the
# prose can be checked against what the figures were drawn from.
write_paper_results <- function(x, name) {
  out_dir <- paper_dir("results")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  jsonlite::write_json(x, file.path(out_dir, paste0(name, ".json")),
                       auto_unbox = TRUE, pretty = TRUE, digits = 6)
}
