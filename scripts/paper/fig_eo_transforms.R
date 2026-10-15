# Figure: the three cardinal transformations of the Economic Opportunity pillar, with the
# inputs of the final build placed on each curve (paper Section 6, fig:eo_transforms).
#
# Panel (a): the TRL level score b(x) of trl_bell_hard() (R/categories/technological_readiness/
#   technological_readiness.R), a raised-cosine bell that is 0 at and below TRL 2 and at and
#   above TRL 11 and peaks at TRL 6. Circles sit on the curve at each end-year TRL, with area
#   proportional to the number of IEA Clean Energy Technology Guide items mapped to a headline
#   technology at that level. The 2026 Guide stops at TRL 9 and grades TRL 9 items by a
#   post-TRL-9 adoption stage, which technological_readiness_extended_trl() reads back onto the
#   1-11 scale (Awaiting adoption 9, Building momentum 10, Commercial in many markets 11).
# Panel (b): the TRL momentum score psi(delta) = (min(max(delta, 0), cap) / cap)^gamma_m, with
#   circles for the items' TRL change over the release's window (items without a start-year
#   TRL have no momentum score and are not drawn).
# Panel (c): the overcapacity index 2^(-rho) of overcapacity_premium()
#   (R/categories/technology_demand/overcapacity_premium.R), where rho is the ratio of
#   midstream manufacturing capacity to forecast demand, with the four technologies the
#   BloombergNEF supply-chain data cover marked at their values in the final build.
#
# Parameters are read from the theme builders' own function definitions and defaults. The item
# table is rebuilt with the theme's own pure functions from data/raw (the Guide and the mapping
# rules in config/iea_clean_tech_guide_tech_map.yml), and the script stops unless the
# technology-stage TRL indices it implies equal those of the processed theme table
# (data/processed/technological_readiness_tbl.rds). Overcapacity is read from
# data/processed/overcapacity_premium_tbl.rds.
# Output: paper/figures/fig_eo_transforms.pdf (+ png twin) and paper/results/eo_transforms.json.
#
# Run from the repository root:
#   Rscript scripts/paper/fig_eo_transforms.R

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
source(file.path(repo_root, "R", "utils", "schema.R"))

suppressPackageStartupMessages({
  library(dplyr)
  library(cowplot)
})

config <- yaml::read_yaml(Sys.getenv("OPSI_CONFIG", file.path(repo_root, "config", "config.yml")))
raw_dir <- file.path(repo_root, config$raw_data_dir)
processed_dir <- file.path(repo_root, "data", "processed")

trl_file <- file.path(repo_root, "R", "categories", "technological_readiness", "technological_readiness.R")
overcap_file <- file.path(repo_root, "R", "categories", "technology_demand", "overcapacity_premium.R")

# The TRL builder file only defines functions and constants, so sourcing it is side-effect free.
trl_env <- new.env()
sys.source(trl_file, envir = trl_env)

defaults <- formals(trl_env$technological_readiness_build_tech)
par_min <- eval(defaults$min_trl)
par_mu <- eval(defaults$mu)
par_max <- eval(defaults$max_trl)
par_w <- eval(defaults$momentum_weight)
par_cap <- eval(defaults$delta_cap)
par_gm <- eval(defaults$gamma_momentum)
post_scale <- trl_env$IEA_POST_TRL9_SCALE
headline_techs <- eval(defaults$techs)

# The paper states these values; stop if the builder has moved away from them.
stopifnot(
  par_min == 2, par_mu == 6, par_max == 11,
  par_w == 0.3, par_cap == 3, par_gm == 0.5,
  identical(unname(post_scale), c(9, 10, 11))
)

bell <- function(x) trl_env$trl_bell_hard(x, min_trl = par_min, mu = par_mu, max_trl = par_max)
# Same expression as the momentum term inside technological_readiness_build_tech().
momentum <- function(delta) (pmin(pmax(0, delta), par_cap) / par_cap)^par_gm

# The overcapacity transform is inline in overcapacity_premium(); check it is still 2^(-ratio).
overcap_src <- readLines(overcap_file, warn = FALSE)
stopifnot(any(grepl("2\\^\\(-Overcapacity\\)", overcap_src)))
overcap <- function(rho) 2^(-rho)

# ---------------------------------------------------------------------------
# Item table of the final build, rebuilt with the theme's own functions
# ---------------------------------------------------------------------------
guide_path <- file.path(raw_dir, "IEACleanTechGuidepublicdataset.csv")
guide_raw <- utils::read.csv(guide_path, fileEncoding = "UTF-8-BOM")
guide <- trl_env$technological_readiness_clean(guide_raw)
year_end <- trl_env$technological_readiness_resolve_year_end(guide)
year_start <- trl_env$technological_readiness_resolve_year_start(guide)
map_rules <- trl_env$read_iea_tech_map_rules(
  file.path(repo_root, "config", "iea_clean_tech_guide_tech_map.yml")
)
assigned <- trl_env$technological_readiness_assign_tech(guide, map_rules = map_rules)
rm(guide_raw)

end_col <- paste0("trl", year_end)
post_col <- paste0("post_trl9_stage_", year_end)
items_cells <- assigned %>%
  dplyr::filter(.data$tech %in% headline_techs) %>%
  dplyr::mutate(
    trl_reported = .data[[end_col]],
    post_stage = .data[[post_col]],
    trl_end = trl_env$technological_readiness_extended_trl(.data[[end_col]], .data[[post_col]]),
    trl_start = trl_env$find_trl_start(dplyr::pick(dplyr::everything())),
    trl_delta = trl_env$technological_readiness_series_trl(.data[[end_col]], .data[[post_col]]) -
      .data$trl_start
  )
# One row per Guide item (an item mapped to several technology-stage cells counts once).
items <- items_cells[!duplicated(items_cells[, c("name", "sector_raw")]), ]

# The cell indices these items imply must be the ones the pipeline wrote.
rebuilt <- trl_env$technological_readiness_build_tech(assigned, techs = headline_techs)
trl_tbl <- readRDS(file.path(processed_dir, "technological_readiness_tbl.rds"))
cell_tbl <- attr(trl_tbl, "mapping_diagnostics")$tech_stage_table
check <- dplyr::inner_join(rebuilt, cell_tbl, by = c("tech", "supply_chain"), suffix = c("", "_tbl"))
stopifnot(nrow(check) == 33, max(abs(check$trl_index - check$trl_index_tbl)) < 1e-12)
cell_index <- trl_tbl %>%
  dplyr::filter(.data$variable == "TRL Index") %>%
  dplyr::distinct(.data$tech, .data$supply_chain, .data$value)
stopifnot(nrow(cell_index) == 33,
          max(abs(cell_index$value[order(cell_index$tech, cell_index$supply_chain)] -
                    cell_tbl$trl_index[order(cell_tbl$tech, cell_tbl$supply_chain)])) < 1e-12)
rm(trl_tbl, assigned)
invisible(gc())

level_counts <- items %>%
  dplyr::filter(!is.na(.data$trl_end)) %>%
  dplyr::count(trl_end, name = "n") %>% dplyr::rename(x = trl_end) %>%
  dplyr::mutate(y = bell(.data$x))
stopifnot(all(level_counts$x == round(level_counts$x)))
delta_counts <- items %>%
  dplyr::filter(!is.na(.data$trl_delta)) %>%
  dplyr::count(trl_delta, name = "n") %>% dplyr::rename(d = trl_delta) %>%
  dplyr::mutate(y = momentum(.data$d))

oc <- readRDS(file.path(processed_dir, "overcapacity_premium_tbl.rds")) %>%
  dplyr::distinct(.data$tech, .data$supply_chain, .data$variable, .data$value) %>%
  tidyr::pivot_wider(names_from = "variable", values_from = "value")
stopifnot(nrow(oc) == 4, all(oc$supply_chain == "Midstream"),
          max(abs(oc$`Overall Overcapacity` - overcap(oc$Overcapacity))) < 1e-12)

# ---------------------------------------------------------------------------
# Figure
# ---------------------------------------------------------------------------
hl <- paper_highlight()
line_col <- hl[["highlight"]]
size_scale <- scale_size_area(
  name = "Guide items", max_size = 6.5,
  limits = c(0, max(c(level_counts$n, delta_counts$n))),
  breaks = c(10, 50, 100)
)
bubble <- function(data, mapping) {
  geom_point(data = data, mapping = mapping, shape = 21, fill = line_col,
             colour = paper_ink$surface, stroke = 0.5, alpha = 0.9)
}

x_grid <- seq(1, 11, length.out = 1001)
bell_df <- data.frame(x = x_grid, y = bell(x_grid))
# Labels for the three post-TRL-9 adoption stages, placed above their circles.
stage_lab <- level_counts %>%
  dplyr::filter(.data$x %in% unname(post_scale)) %>%
  dplyr::mutate(lab = c("AA", "BM", "CM")[match(.data$x, unname(post_scale))])

panel_a <- ggplot() +
  geom_vline(xintercept = c(par_min, par_mu, par_max), colour = paper_ink$grid, linewidth = 0.3) +
  geom_line(data = bell_df, aes(x, y), colour = line_col, linewidth = 0.7) +
  bubble(level_counts, aes(x = x, y = y, size = n)) +
  geom_text(data = stage_lab, aes(x = x - 0.42, y = y + 0.02, label = lab), size = 2.3,
            hjust = 1, colour = paper_ink$secondary) +
  size_scale +
  scale_x_continuous(breaks = 1:11, limits = c(0.7, 11.3), expand = expansion(mult = 0)) +
  scale_y_continuous(limits = c(-0.06, 1.1), breaks = seq(0, 1, 0.25)) +
  labs(x = sprintf("End-year TRL (%d)", year_end), y = expression("Level score " * b(x))) +
  theme_paper(grid = "y") +
  theme(plot.margin = margin(14, 6, 4, 4))

d_max <- max(delta_counts$d)
d_grid <- seq(-0.5, d_max + 0.3, length.out = 1001)
mom_df <- data.frame(d = d_grid, y = momentum(d_grid))

panel_b <- ggplot() +
  geom_vline(xintercept = c(0, par_cap), colour = paper_ink$grid, linewidth = 0.3) +
  geom_line(data = mom_df, aes(d, y), colour = line_col, linewidth = 0.7) +
  bubble(delta_counts, aes(x = d, y = y, size = n)) +
  size_scale +
  scale_x_continuous(breaks = 0:d_max, expand = expansion(mult = 0.02)) +
  scale_y_continuous(limits = c(-0.06, 1.1), breaks = seq(0, 1, 0.25)) +
  labs(x = sprintf("TRL change, %d to %d", year_start, year_end),
       y = expression("Momentum score " * psi(Delta))) +
  theme_paper(grid = "y") +
  theme(plot.margin = margin(14, 6, 4, 4))

rho_max <- 5
r_grid <- seq(0, rho_max, length.out = 1001)
oc_df <- data.frame(rho = r_grid, y = overcap(r_grid))
oc_on <- oc %>% dplyr::filter(.data$Overcapacity <= rho_max)
oc_off <- oc %>% dplyr::filter(.data$Overcapacity > rho_max)
oc_lab <- oc_on %>%
  dplyr::arrange(.data$Overcapacity) %>%
  dplyr::mutate(grp = cumsum(c(TRUE, diff(.data$Overcapacity) > 0.15))) %>%
  dplyr::group_by(.data$grp) %>%
  dplyr::summarise(
    rho = mean(.data$Overcapacity), y = mean(.data$`Overall Overcapacity`),
    lab = paste(sort(dplyr::if_else(.data$tech == "Batteries", "batteries", .data$tech)),
                collapse = ",\n"),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    lab = paste0(toupper(substr(.data$lab, 1, 1)), substring(.data$lab, 2)),
    # Alternate sides so neighbouring labels do not collide.
    hj = ifelse(seq_along(.data$lab) %% 2 == 1, 1, 0),
    nudge = ifelse(.data$hj == 1, -0.1, 0.1)
  )
off_lab <- if (nrow(oc_off) > 0) {
  sprintf("%s (off scale):\nratio %.1f, index %.0e", ifelse(oc_off$tech == "Green Hydrogen",
                                                  "Green hydrogen", oc_off$tech),
          oc_off$Overcapacity, oc_off$`Overall Overcapacity`)
} else character(0)

panel_c <- ggplot() +
  geom_vline(xintercept = 1, colour = paper_ink$grid, linewidth = 0.3) +
  geom_line(data = oc_df, aes(rho, y), colour = line_col, linewidth = 0.7) +
  geom_point(data = oc_on, aes(Overcapacity, `Overall Overcapacity`), shape = 21,
             fill = line_col, colour = paper_ink$surface, stroke = 0.5, size = 2.2) +
  geom_text(data = oc_lab, aes(x = rho + nudge, y = y + 0.07, label = lab, hjust = hj),
            size = 2.3, colour = paper_ink$secondary, lineheight = 0.9, vjust = 0) +
  annotate("text", x = rho_max, y = 0.62, label = paste(off_lab, collapse = "\n"),
           hjust = 1, size = 2.3, colour = paper_ink$secondary, lineheight = 0.9) +
  scale_x_continuous(breaks = 0:rho_max, limits = c(0, rho_max),
                     expand = expansion(mult = c(0.02, 0.04))) +
  scale_y_continuous(limits = c(-0.06, 1.1), breaks = seq(0, 1, 0.25)) +
  labs(x = expression("Capacity / demand ratio, " * rho),
       y = expression("Overcapacity index " * 2^{-rho})) +
  theme_paper(grid = "y") +
  theme(plot.margin = margin(14, 6, 4, 4))

legend <- cowplot::get_plot_component(
  panel_a + guides(size = guide_legend(nrow = 1, override.aes = list(fill = line_col))) +
    theme(legend.position = "bottom"),
  "guide-box-bottom", return_all = TRUE
)
if (is.list(legend) && !inherits(legend, "grob")) legend <- legend[[1]]
row <- cowplot::plot_grid(
  panel_a + theme(legend.position = "none"),
  panel_b + theme(legend.position = "none"),
  panel_c,
  nrow = 1, align = "h", axis = "tb", rel_widths = c(1.2, 1, 1),
  labels = c("(a)", "(b)", "(c)"), label_size = 9, label_fontface = "bold",
  label_x = 0, hjust = 0
)
fig <- cowplot::plot_grid(row, legend, ncol = 1, rel_heights = c(1, 0.12))

save_paper_fig(fig, "fig_eo_transforms", width = paper_width_full, height = 2.9)

# ---------------------------------------------------------------------------
# Numbers
# ---------------------------------------------------------------------------
cells <- cell_tbl %>%
  dplyr::transmute(tech, supply_chain, trl_end = .data[[end_col]], trl_level_index,
                   trl_momentum_index, trl_index, n_items, basis, momentum_basis)
n_items <- nrow(items)
adoption <- items %>%
  dplyr::filter(!is.na(.data$post_stage)) %>%
  dplyr::count(post_stage, trl_end)

key_values <- list(
  bell = setNames(as.list(bell(2:11)), paste0("trl_", 2:11)),
  momentum = setNames(as.list(momentum(c(0, 1, 2, 3, 4))), paste0("delta_", 0:4)),
  overcapacity = setNames(as.list(overcap(c(0, 0.5, 1, 2, 3))), paste0("rho_", c(0, 0.5, 1, 2, 3))),
  parameters = list(
    min_trl = par_min, peak_trl = par_mu, max_trl = par_max,
    momentum_weight = par_w, delta_cap = par_cap, gamma_momentum = par_gm,
    post_trl9_scale = as.list(post_scale)
  ),
  guide = list(
    file = "data/raw/IEACleanTechGuidepublicdataset.csv",
    year_end = year_end, year_start = year_start,
    items_in_release = nrow(guide),
    items_mapped_to_headline_techs = n_items,
    item_cell_rows = nrow(items_cells),
    items_in_more_than_one_cell = n_items - sum(!duplicated(items_cells[, c("name", "sector_raw")]) &
                                                   !duplicated(items_cells[, c("name", "sector_raw")], fromLast = TRUE)),
    items_by_end_trl = stats::setNames(as.list(level_counts$n), level_counts$x),
    items_with_adoption_stage = sum(!is.na(items$post_stage)),
    items_by_adoption_stage = stats::setNames(
      as.list(as.integer(table(items$post_stage))), names(table(items$post_stage))
    ),
    adoption_stage_by_end_trl = adoption,
    items_level_zero = sum(bell(items$trl_end) == 0, na.rm = TRUE),
    items_level_zero_at_trl11 = sum(items$trl_end == par_max, na.rm = TRUE),
    share_items_trl9_or_above = mean(items$trl_end >= 9, na.rm = TRUE),
    items_without_momentum = sum(is.na(items$trl_delta)),
    items_by_delta = stats::setNames(as.list(delta_counts$n), delta_counts$d),
    share_items_delta_zero = mean(items$trl_delta[!is.na(items$trl_delta)] <= 0)
  ),
  cells = list(
    n = nrow(cells),
    stage_basis = sum(cells$basis == "stage"),
    technology_basis = sum(cells$basis == "technology"),
    technology_basis_cells = paste(cells$tech, cells$supply_chain)[cells$basis == "technology"],
    trl_index_min = min(cells$trl_index), trl_index_max = max(cells$trl_index),
    trl_index_median = stats::median(cells$trl_index),
    cell_min = paste(cells$tech, cells$supply_chain)[which.min(cells$trl_index)],
    cell_max = paste(cells$tech, cells$supply_chain)[which.max(cells$trl_index)],
    trl_end_min = min(cells$trl_end), trl_end_max = max(cells$trl_end),
    table = cells
  ),
  overcapacity_build = oc %>%
    dplyr::transmute(tech, supply_chain, rho = Overcapacity, index = `Overall Overcapacity`)
)
stopifnot(abs(key_values$bell$trl_9 - cos(3 * pi / 10)) < 1e-12)
stopifnot(abs(key_values$bell$trl_3 - cos(3 * pi / 8)) < 1e-12)
stopifnot(key_values$overcapacity$rho_1 == 0.5)

write_paper_results(key_values, "eo_transforms")
message("Wrote fig_eo_transforms.pdf and eo_transforms.json")
