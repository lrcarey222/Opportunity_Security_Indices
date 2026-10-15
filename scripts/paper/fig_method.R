# Figures and numbers on the structure of the pillar indices (paper Sections 2-4 and Appendix A,
# theme "method").
#
# Reads the final index outputs (data/processed/outputs/index_outputs.rds, final rerun of
# 2026-10-12), the October 2026 NIPO component of the robustness variant, the HS6 master (data/reference/energy_hs6_master.csv), the all-reporter Comtrade
# dyad file (data/raw/comtrade_energy_dyads_all.csv), the Comtrade energy-trade file rebuilt from
# its World rows (data/raw/comtrade_energy_trade.csv) and a few small reference files, and writes
#   method_weights        nominal category weight shares (config/weights.yml) against the
#                         effective weights the available-case rule realises, by stage;
#   method_category_sets  which categories are observed in each of the 33 headline
#                         technology x stage pairs, and whether each is constant within the pair;
#   method_imputation     mean imputed share by category and stage, and for the pillar;
#   method_coverage       economies scored in each pair against economies with country-specific
#                         observed data, with the pair status (ok / constant / unscored);
#   method_rankability    share of the scored headline cells that are rankable under
#                         config/rankability.yml (sovereign and imputed weight share <= tau), and
#                         why the others are not (territory / imputed weight share above tau);
# plus paper/results/method.json (every number quoted in paper/results/text/method.tex, and in
# $tbd_fills the LaTeX that replaces the \tbd{} placeholders of Sections 2-4 and 8 this theme
# answers) and the tables paper/tables/method_imputation.tex, method_rankability.tex,
# method_policy.tex and method_hs6_baskets.tex.
#
# Definitions used throughout (the key names in method.json repeat them):
#   pair            a technology x stage combination (t, s); "headline" = the eleven
#                   technologies of the reported ES/EO indices (scripts/20), 33 pairs.
#   cell            a country x technology x stage combination (i, t, s) with an index value.
#   observed        a category enters a cell when it has a category score, i.e. a row in
#                   category_contributions with category_weight > 0 (the available-case rule).
#   constant        within a pair, a quantity whose range across the pair's cells is <= 1e-12.
#   country-specific a component (variable) of a pair whose non-imputed values take at least two
#                   distinct values across the pair's countries; a cell "has country-specific
#                   data" when at least one of its non-imputed, positively weighted components is
#                   country-specific. World-level series copied to every economy (commodity-price
#                   volatility, world export concentration, technology readiness, global demand)
#                   are therefore not country-specific.
#   rankable        the pillar table's own flag (flag_rankable_cells(), R/indices/
#                   index_builder_core.R): the economy is sovereign (a Correlates of War state code)
#                   and imputed_weight_share <= max_imputed_weight_share of config/rankability.yml.
#                   Reasons are counted exclusively here: a territory is a territory whatever its
#                   imputed share, and "imputed" means a sovereign economy over the threshold.
#
# Run from anywhere inside the repository:
#   Rscript scripts/paper/fig_method.R

local({
  find_theme <- function() {
    d <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
    while (dirname(d) != d) {
      f <- file.path(d, "scripts", "paper", "_paper_theme.R")
      if (file.exists(f)) return(f)
      d <- dirname(d)
    }
    stop("Run from inside the repository.")
  }
  source(find_theme(), local = FALSE)
})

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

root <- paper_repo_root()
source(file.path(root, "R", "utils", "country.R"))
source(file.path(root, "R", "utils", "schema.R"))
source(file.path(root, "R", "utils", "hs6_crosswalk.R"))
# Pure helpers of the bilateral themes: the reconciled trade matrix and the conduit list.
source(file.path(root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(root, "R", "themes", "partnership_strength", "safer_friendshore.R"))

config <- yaml::read_yaml(Sys.getenv("OPSI_CONFIG", file.path(root, "config", "config.yml")))
weights_cfg <- yaml::read_yaml(Sys.getenv("OPSI_WEIGHTS", file.path(root, "config", "weights.yml")))
rank_cfg <- yaml::read_yaml(file.path(root, "config", "rankability.yml"))
partnership_cfg <- yaml::read_yaml(file.path(root, "config", "partnership.yml"))
processed_dir <- file.path(root, config$processed_dir)
raw_dir <- file.path(root, config$raw_data_dir)
tau <- as.numeric(rank_cfg$max_imputed_weight_share)

stage_levels <- c("Upstream", "Midstream", "Downstream")
pillar_levels <- c("Energy Security", "Economic Opportunity")
tol <- 1e-12

# ---------------------------------------------------------------------------
# Inputs
# ---------------------------------------------------------------------------
idx_all <- readRDS(file.path(processed_dir, "outputs", "index_outputs.rds"))

# Headline technologies: those of the reported pillar tables (scripts/20 filters both to the
# same eleven), so the list is not repeated here.
headline_techs <- sort(union(unique(idx_all$energy_security_index$tech),
                             unique(idx_all$economic_opportunity_index$tech)))
stopifnot(length(headline_techs) == 11)

keep_pillar <- function(o, index_col) {
  list(
    index = o$index %>% dplyr::rename(value = dplyr::all_of(index_col)),
    cc = o$category_contributions,
    vc = o$variable_contributions,
    cells = o$diagnostics$index_cells,
    imputation = o$diagnostics$index_imputation,
    years = o$diagnostics$year_provenance,
    unscored_groups = o$diagnostics$unscored_groups
  )
}
pillars <- list(
  "Energy Security" = keep_pillar(idx_all$energy_security_outputs, "Energy_Security_Index"),
  "Economic Opportunity" = keep_pillar(idx_all$economic_opportunity_outputs,
                                       "Economic_Opportunity_Index")
)
si <- idx_all$strategic_index
policy_index <- idx_all$policy_index
policy_components <- idx_all$policy_component_tbl
policy_eligibility <- idx_all$policy_eligibility
policy_provenance <- idx_all$policy_index_provenance
coalition_members <- idx_all$coalition_members
rm(idx_all)
invisible(gc())

nominal <- dplyr::bind_rows(
  tibble::tibble(pillar = "Energy Security", category = names(weights_cfg$energy_security),
                 w = unlist(weights_cfg$energy_security)),
  tibble::tibble(pillar = "Economic Opportunity", category = names(weights_cfg$economic_opportunity),
                 w = unlist(weights_cfg$economic_opportunity))
) %>%
  dplyr::group_by(pillar) %>%
  dplyr::mutate(cfg_order = dplyr::row_number(), nominal_share = w / sum(w)) %>%
  # Table tab:category_weights order: weight descending, ties in configuration order.
  dplyr::arrange(dplyr::desc(w), cfg_order, .by_group = TRUE) %>%
  dplyr::mutate(cat_rank = dplyr::row_number()) %>%
  dplyr::ungroup()

# ---------------------------------------------------------------------------
# Local helpers (pure)
# ---------------------------------------------------------------------------
key_cols <- c("Country", "tech", "supply_chain")

headline <- function(df) dplyr::filter(df, .data$tech %in% headline_techs)

stack_pillars <- function(fun) {
  dplyr::bind_rows(lapply(names(pillars), function(p) {
    out <- fun(pillars[[p]], p)
    if (nrow(out) == 0) return(NULL)
    dplyr::mutate(out, pillar = p, .before = 1)
  }))
}

# Every scored headline cell x every configured category of the pillar, with the realised
# weight (0 where the category is not observed in the cell).
effective_weight_grid <- function(pl, p) {
  cats <- nominal$category[nominal$pillar == p]
  cells <- headline(pl$index) %>% dplyr::filter(!is.na(.data$value)) %>% dplyr::select(dplyr::all_of(key_cols))
  obs <- headline(pl$cc) %>%
    dplyr::filter(!is.na(.data$category_score), .data$category_weight > 0) %>%
    dplyr::select(dplyr::all_of(key_cols), "category", "category_weight", "category_score")
  tidyr::expand_grid(cells, category = cats) %>%
    dplyr::left_join(obs, by = c(key_cols, "category")) %>%
    dplyr::mutate(observed = !is.na(.data$category_weight),
                  category_weight = dplyr::coalesce(.data$category_weight, 0))
}

fmt <- function(x, d = 2) formatC(x, format = "f", digits = d)

# Unique y keys for facets whose categories differ by pillar (reorder-within).
cat_key <- function(pillar, category) paste(pillar, category, sep = "___")
strip_key <- function(x) sub("^.*___", "", x)

# ---------------------------------------------------------------------------
# 1. Nominal vs effective weights
# ---------------------------------------------------------------------------
ew <- stack_pillars(effective_weight_grid)

eff_by_stage_cells <- ew %>%
  dplyr::group_by(pillar, supply_chain, category) %>%
  dplyr::summarise(effective_cells = mean(category_weight), n_cells = dplyr::n(), .groups = "drop")
# Within a pair every country has the same category set (checked below), so the realised
# weight is a pair-level quantity; the pair-weighted mean is reported alongside.
eff_by_stage_pairs <- ew %>%
  dplyr::group_by(pillar, tech, supply_chain, category) %>%
  dplyr::summarise(w_pair = mean(category_weight), .groups = "drop") %>%
  dplyr::group_by(pillar, supply_chain, category) %>%
  dplyr::summarise(effective_pairs = mean(w_pair), n_pairs = dplyr::n(), .groups = "drop")
eff <- eff_by_stage_cells %>%
  dplyr::left_join(eff_by_stage_pairs, by = c("pillar", "supply_chain", "category")) %>%
  dplyr::left_join(nominal %>% dplyr::select(pillar, category, w, nominal_share, cat_rank),
                   by = c("pillar", "category"))
eff_all_stages <- ew %>%
  dplyr::group_by(pillar, category) %>%
  dplyr::summarise(effective_all_cells = mean(category_weight), .groups = "drop")

plot_w <- eff %>%
  dplyr::mutate(pillar = factor(pillar, levels = pillar_levels))
nom_w <- nominal %>%
  dplyr::mutate(pillar = factor(pillar, levels = pillar_levels))

w_series <- c("Nominal share", stage_levels)
w_cols <- c("Nominal share" = paper_ink$primary, paper_stage_cols)

# The two panels list different categories, so each pillar's categories get their own block
# of y positions (free y scales then show each panel only its own ten labels). Within a row
# the three stage dots sit Upstream on top, Downstream at the bottom.
plot_w$ykey <- cat_key(plot_w$pillar, plot_w$category)
nom_w$ykey <- cat_key(nom_w$pillar, nom_w$category)
y_levels <- rev(cat_key(nominal$pillar, nominal$category))
pos_map <- c(Upstream = 0.24, Midstream = 0, Downstream = -0.24)
plot_w$ynum <- match(plot_w$ykey, y_levels)
nom_w$ynum <- match(nom_w$ykey, y_levels)

p_weights <- ggplot() +
  geom_segment(data = nom_w,
               aes(x = nominal_share, xend = nominal_share, y = ynum - 0.42, yend = ynum + 0.42,
                   colour = "Nominal share"),
               linewidth = 0.55, key_glyph = "vpath") +
  geom_point(data = plot_w,
             aes(x = effective_cells, y = ynum + pos_map[supply_chain], colour = supply_chain),
             size = 1.6, stroke = 0, shape = 16) +
  facet_wrap(~pillar, ncol = 2, scales = "free_y") +
  scale_y_continuous(
    breaks = seq_along(y_levels), labels = strip_key(y_levels),
    expand = expansion(add = 0.3)
  ) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0.01, 0.04))) +
  scale_colour_manual(values = w_cols, breaks = w_series, name = NULL,
                      labels = c("Nominal share", "Effective: Upstream",
                                 "Effective: Midstream", "Effective: Downstream")) +
  guides(colour = guide_legend(nrow = 1, override.aes = list(
    linetype = c(1, 0, 0, 0), shape = c(NA, 16, 16, 16), size = c(NA, 1.8, 1.8, 1.8)))) +
  labs(x = "Weight share of the category in the pillar", y = NULL) +
  theme_paper(grid = "x") +
  theme(panel.spacing.x = unit(1.2, "lines"))

save_paper_fig(p_weights, "method_weights", width = paper_width_full, height = 3.3)

# ---------------------------------------------------------------------------
# 2. Category sets of the 33 headline pairs
# ---------------------------------------------------------------------------
all_pairs <- tidyr::expand_grid(tech = headline_techs, supply_chain = stage_levels)

cat_pair <- ew %>%
  dplyr::group_by(pillar, tech, supply_chain, category) %>%
  dplyr::summarise(
    n_cells = dplyr::n(),
    n_observed = sum(observed),
    score_range = if (any(observed)) diff(range(category_score[observed])) else NA_real_,
    .groups = "drop"
  ) %>%
  dplyr::mutate(status = dplyr::case_when(
    n_observed == 0 ~ "Not observed",
    n_observed < n_cells ~ "Observed for some economies only",
    n_cells >= 2 & score_range <= tol ~ "Observed, identical for every economy",
    TRUE ~ "Observed, varies across economies"
  ))

pair_sets <- ew %>%
  dplyr::filter(observed) %>%
  dplyr::group_by(pillar, Country, tech, supply_chain) %>%
  dplyr::summarise(set = paste(sort(category), collapse = "|"), n_cat = dplyr::n(), .groups = "drop") %>%
  dplyr::group_by(pillar, tech, supply_chain) %>%
  dplyr::summarise(n_sets = dplyr::n_distinct(set), n_cat_min = min(n_cat), n_cat_max = max(n_cat),
                   n_cells = dplyr::n(), .groups = "drop")

set_status_levels <- c("Observed, varies across economies", "Observed, identical for every economy",
                       "Observed for some economies only", "Not observed")
set_cols <- c(
  "Observed, varies across economies" = paper_seq_blue[[5]],
  "Observed, identical for every economy" = paper_seq_blue[[2]],
  "Observed for some economies only" = unname(paper_cat["yellow"]),
  "Not observed" = paper_ink$neutral
)
present_status <- set_status_levels[set_status_levels %in% c(cat_pair$status, "Not observed")]

count_row <- "Categories observed"
set_levels <- unlist(lapply(pillar_levels, function(p) {
  cats <- nominal$category[nominal$pillar == p]
  cat_key(p, c(rev(cats), count_row))
}))
tile_df <- cat_pair %>%
  dplyr::mutate(ykey = cat_key(pillar, category))
count_df <- pair_sets %>%
  dplyr::mutate(ykey = cat_key(pillar, count_row), label = as.character(n_cat_min))
unscored_df <- tidyr::expand_grid(pillar = pillar_levels, all_pairs) %>%
  dplyr::anti_join(pair_sets, by = c("pillar", "tech", "supply_chain")) %>%
  dplyr::mutate(y_mid = vapply(pillar, function(p) {
    mean(match(cat_key(p, nominal$category[nominal$pillar == p]), set_levels))
  }, numeric(1)))

finish_axes <- function(df) {
  df %>% dplyr::mutate(
    pillar = factor(pillar, levels = pillar_levels),
    stage = factor(supply_chain, levels = stage_levels),
    tech = factor(tech, levels = headline_techs)
  )
}
tile_df <- finish_axes(tile_df) %>% dplyr::mutate(ynum = match(ykey, set_levels),
                                                  status = factor(status, levels = present_status))
count_df <- finish_axes(count_df) %>% dplyr::mutate(ynum = match(ykey, set_levels))
unscored_df <- finish_axes(unscored_df)

p_sets <- ggplot() +
  geom_tile(data = tile_df, aes(x = tech, y = ynum, fill = status),
            width = 0.9, height = 0.86) +
  geom_text(data = count_df, aes(x = tech, y = ynum, label = label),
            size = 2.8, colour = paper_ink$secondary) +
  geom_text(data = unscored_df, aes(x = tech, y = y_mid), label = "pair not scored",
            angle = 90, size = 2.8, colour = paper_ink$muted) +
  facet_grid(pillar ~ stage, scales = "free", space = "free") +
  scale_y_continuous(breaks = seq_along(set_levels), labels = strip_key(set_levels),
                     expand = expansion(add = 0.1)) +
  scale_x_discrete(expand = expansion(add = 0.1), drop = FALSE) +
  scale_fill_manual(values = set_cols[present_status], breaks = present_status, name = NULL,
                    drop = FALSE) +
  labs(x = NULL, y = NULL) +
  theme_paper(grid = "none") +
  theme(
    axis.line.x = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    strip.text.y = element_text(angle = -90, hjust = 0.5),
    panel.spacing = unit(0.5, "lines")
  )

save_paper_fig(p_sets, "method_category_sets", width = paper_width_full, height = 5.2)

# ---------------------------------------------------------------------------
# 3. Imputation burden by category and stage
# ---------------------------------------------------------------------------
# Imputed share of a category in a cell: the share of the category score carried by imputed
# components, sum_v lambda_v 1[imputed]. Weighted by the realised category weight and summed
# over categories it is the cell's imputed weight share iota of eq. (imputed_share), which the
# builder records in diagnostics$index_imputation (checked below).
category_imputation <- function(pl, p) {
  headline(pl$vc) %>%
    dplyr::filter(.data$category_weight > 0) %>%
    dplyr::group_by(Country, tech, supply_chain, category) %>%
    dplyr::summarise(
      lambda_sum = sum(component_weight_within_overall),
      imp_share = sum(component_weight_within_overall * (imputed %in% TRUE)),
      omega = dplyr::first(category_weight),
      .groups = "drop"
    )
}
cat_imp <- stack_pillars(category_imputation)
stopifnot(max(abs(cat_imp$lambda_sum - 1)) < 1e-9)

cell_imp <- stack_pillars(function(pl, p) headline(pl$imputation))
cell_imp_check <- cat_imp %>%
  dplyr::group_by(pillar, Country, tech, supply_chain) %>%
  dplyr::summarise(iota = sum(omega * imp_share), .groups = "drop") %>%
  dplyr::inner_join(cell_imp, by = c("pillar", key_cols))
stopifnot(nrow(cell_imp_check) == nrow(cell_imp))
iota_reproduction_error <- max(abs(cell_imp_check$iota - cell_imp_check$imputed_weight_share))
stopifnot(iota_reproduction_error < 1e-9)

imp_cat_stage <- cat_imp %>%
  dplyr::group_by(pillar, supply_chain, category) %>%
  dplyr::summarise(mean_imputed_share = mean(imp_share), n_cells = dplyr::n(),
                   share_fully_imputed = mean(imp_share >= 1 - tol), .groups = "drop")
imp_pillar_stage <- cell_imp %>%
  dplyr::group_by(pillar, supply_chain) %>%
  dplyr::summarise(mean_imputed_share = mean(imputed_weight_share), n_cells = dplyr::n(),
                   n_fully_imputed = sum(imputed_weight_share >= 1 - tol),
                   n_no_imputation = sum(imputed_weight_share <= tol),
                   .groups = "drop")
imp_pillar_all <- cell_imp %>%
  dplyr::group_by(pillar) %>%
  dplyr::summarise(mean_imputed_share = mean(imputed_weight_share),
                   median_imputed_share = stats::median(imputed_weight_share),
                   n_cells = dplyr::n(),
                   n_fully_imputed = sum(imputed_weight_share >= 1 - tol),
                   n_no_imputation = sum(imputed_weight_share <= tol),
                   share_above_half = mean(imputed_weight_share > 0.5),
                   .groups = "drop")
# Contribution of each category to the mean imputed weight share of the stage (sums to the
# pillar row over categories).
imp_contrib <- ew %>%
  dplyr::select(pillar, dplyr::all_of(key_cols), category) %>%
  dplyr::left_join(cat_imp, by = c("pillar", key_cols, "category")) %>%
  dplyr::mutate(term = dplyr::coalesce(omega * imp_share, 0)) %>%
  dplyr::group_by(pillar, supply_chain, category) %>%
  dplyr::summarise(contribution_to_mean_iota = mean(term), .groups = "drop")
contrib_check <- imp_contrib %>%
  dplyr::group_by(pillar, supply_chain) %>%
  dplyr::summarise(s = sum(contribution_to_mean_iota), .groups = "drop") %>%
  dplyr::inner_join(imp_pillar_stage, by = c("pillar", "supply_chain"))
stopifnot(max(abs(contrib_check$s - contrib_check$mean_imputed_share)) < 1e-9)

fully_imputed_cells <- cell_imp %>%
  dplyr::filter(imputed_weight_share >= 1 - tol) %>%
  dplyr::arrange(pillar, supply_chain, tech, Country)

pillar_row <- "Pillar"
imp_levels <- unlist(lapply(pillar_levels, function(p) {
  cats <- nominal$category[nominal$pillar == p]
  cat_key(p, c(rev(cats), pillar_row))
}))
# The pillar row sits slightly apart from the category rows.
imp_pos <- seq_along(imp_levels) + ifelse(strip_key(imp_levels) == pillar_row, 0.3, 0)
imp_tiles <- dplyr::bind_rows(
  tidyr::expand_grid(nominal %>% dplyr::select(pillar, category), supply_chain = stage_levels) %>%
    dplyr::left_join(imp_cat_stage, by = c("pillar", "supply_chain", "category")),
  imp_pillar_stage %>% dplyr::mutate(category = pillar_row)
) %>%
  dplyr::mutate(
    ykey = cat_key(pillar, category),
    ynum = imp_pos[match(ykey, imp_levels)],
    pillar = factor(pillar, levels = pillar_levels),
    stage = factor(supply_chain, levels = stage_levels),
    label = ifelse(category == pillar_row, fmt(mean_imputed_share), NA_character_),
    label_col = ifelse(!is.na(mean_imputed_share) & mean_imputed_share > 0.55, "light", "dark")
  )

stage_abbrev <- c(Upstream = "Up", Midstream = "Mid", Downstream = "Down")
imp_tiles$stage_lab <- factor(stage_abbrev[as.character(imp_tiles$stage)], levels = stage_abbrev)

# Unobserved category-stage combinations get no tile (a dash), so they cannot be read as a
# zero imputed share, which is the light end of the ramp.
p_imp <- ggplot(imp_tiles, aes(x = stage_lab, y = ynum)) +
  geom_tile(data = dplyr::filter(imp_tiles, !is.na(mean_imputed_share)),
            aes(fill = mean_imputed_share), width = 0.94, height = 0.9) +
  geom_text(data = dplyr::filter(imp_tiles, is.na(mean_imputed_share)),
            label = "–", size = 2.8, colour = paper_ink$muted) +
  geom_text(aes(label = label, colour = label_col), size = 2.8, na.rm = TRUE,
            show.legend = FALSE) +
  facet_wrap(~pillar, ncol = 2, scales = "free_y") +
  scale_y_continuous(breaks = imp_pos, labels = strip_key(imp_levels),
                     expand = expansion(add = 0.1)) +
  scale_x_discrete(position = "top", drop = FALSE) +
  scale_fill_gradientn(colours = paper_seq_blue, limits = c(0, 1),
                       breaks = seq(0, 1, 0.25), name = "Mean imputed share") +
  scale_colour_manual(values = c(light = "#ffffff", dark = paper_ink$primary)) +
  guides(fill = guide_colourbar(barwidth = unit(4.5, "cm"), barheight = unit(0.25, "cm"),
                                title.vjust = 0.9)) +
  labs(x = NULL, y = NULL) +
  theme_paper(grid = "none") +
  theme(axis.line.x = element_blank(), panel.spacing.x = unit(1.5, "lines"),
        strip.placement = "outside",
        axis.text.x.top = element_text(colour = paper_ink$secondary, size = 8))

save_paper_fig(p_imp, "method_imputation", width = paper_width_full, height = 3.6)

# Table: every value of the heatmap.
all_cats <- sort(unique(nominal$category))
tab_val <- function(p, cat, st) {
  if (!cat %in% nominal$category[nominal$pillar == p]) return("{n.a.}")
  v <- imp_cat_stage$mean_imputed_share[imp_cat_stage$pillar == p & imp_cat_stage$category == cat &
                                          imp_cat_stage$supply_chain == st]
  if (length(v) == 0) "{--}" else fmt(v)
}
stage_vals <- function(fun) {
  c(vapply(stage_levels, function(st) fun("Energy Security", st), character(1)),
    vapply(stage_levels, function(st) fun("Economic Opportunity", st), character(1)))
}
tab_rows <- vapply(all_cats, function(cat) {
  vals <- stage_vals(function(p, st) tab_val(p, cat, st))
  paste0("      ", cat, " & ", paste(vals, collapse = " & "), " \\\\")
}, character(1))
pil_vals <- stage_vals(function(p, st) fmt(imp_pillar_stage$mean_imputed_share[
  imp_pillar_stage$pillar == p & imp_pillar_stage$supply_chain == st]))
n_vals <- stage_vals(function(p, st) format(imp_pillar_stage$n_cells[
  imp_pillar_stage$pillar == p & imp_pillar_stage$supply_chain == st], big.mark = ","))
tab_tex <- c(
  "% Generated by scripts/paper/fig_method.R; do not edit by hand.",
  "\\begin{table}[tbp]",
  "  \\centering",
  "  \\small",
  "  \\begin{threeparttable}",
  "    \\caption{Mean imputed share by category, stage and pillar, headline technologies}\\label{tab:method_imputation}",
  "    \\begin{tabular}{@{}l *{6}{S[table-format=1.2]}@{}}",
  "      \\toprule",
  "      & \\multicolumn{3}{c}{Energy Security} & \\multicolumn{3}{c@{}}{Economic Opportunity}\\\\",
  "      \\cmidrule(lr){2-4}\\cmidrule(l){5-7}",
  "      {Category} & {Up} & {Mid} & {Down} & {Up} & {Mid} & {Down} \\\\",
  "      \\midrule",
  tab_rows,
  "      \\midrule",
  paste0("      Pillar, $\\bar\\iota$ & ", paste(pil_vals, collapse = " & "), " \\\\"),
  paste0("      Cells & ", paste(paste0("{", n_vals, "}"), collapse = " & "), " \\\\"),
  "      \\bottomrule",
  "    \\end{tabular}",
  "    \\begin{tablenotes}[flushleft]\\footnotesize",
  "      \\item \\emph{Notes:} Category rows give the mean, over the country-technology-stage cells",
  "      of the eleven headline technologies in which the category is observed, of the share of the",
  "      category score carried by imputed components,",
  "      $\\sum_v\\lambda_{vkits}\\1[\\tilde x_{itsv}\\text{ imputed}]$. The pillar row gives the mean",
  "      over all scored cells of the stage of the imputed weight share $\\iota_{its}$ of",
  "      equation~\\eqref{eq:imputed_share}. --: category not observed at that stage; n.a.: category",
  "      not part of the pillar. Source: \\texttt{data/processed/outputs/index\\_outputs.rds}",
  "      (variable contributions and the pillar diagnostics); \\texttt{scripts/paper/fig\\_method.R}.",
  "    \\end{tablenotes}",
  "  \\end{threeparttable}",
  "\\end{table}"
)
tab_dir <- paper_dir("tables")
if (!dir.exists(tab_dir)) dir.create(tab_dir, recursive = TRUE)
writeLines(tab_tex, file.path(tab_dir, "method_imputation.tex"), useBytes = TRUE)

# ---------------------------------------------------------------------------
# 4. Coverage: economies scored per pair, economies with country-specific data, pair status
# ---------------------------------------------------------------------------
# A component is country-specific in a pair when its non-imputed values take at least two
# distinct values across the pair's economies (world-level series copied to every economy
# are not). A cell has country-specific data when at least one of its non-imputed, positively
# weighted components is country-specific.
country_specific_cells <- function(pl, p) {
  vc <- headline(pl$vc)
  var_class <- vc %>%
    dplyr::filter(!(imputed %in% TRUE), !is.na(component_value)) %>%
    dplyr::group_by(tech, supply_chain, category, variable) %>%
    dplyr::summarise(n_obs = dplyr::n(), obs_range = diff(range(component_value)), .groups = "drop") %>%
    dplyr::mutate(country_specific = obs_range > tol)
  vc %>%
    dplyr::left_join(var_class, by = c("tech", "supply_chain", "category", "variable")) %>%
    dplyr::group_by(Country, tech, supply_chain) %>%
    dplyr::summarise(
      has_country_specific = any(!(imputed %in% TRUE) & country_specific %in% TRUE &
                                   category_weight * component_weight_within_overall > 0),
      .groups = "drop"
    )
}
common_components <- function(pl, p) {
  headline(pl$vc) %>%
    dplyr::filter(!(imputed %in% TRUE), !is.na(component_value)) %>%
    dplyr::group_by(tech, supply_chain, category, variable) %>%
    dplyr::summarise(n_obs = dplyr::n(), obs_range = diff(range(component_value)), .groups = "drop") %>%
    dplyr::filter(obs_range <= tol, n_obs >= 2) %>%
    dplyr::count(category, variable, name = "n_pairs")
}
cs_cells <- stack_pillars(country_specific_cells)
cell_common_components <- stack_pillars(common_components)

pair_status <- stack_pillars(function(pl, p) headline(pl$cells))
coverage <- stack_pillars(function(pl, p) {
  headline(pl$index) %>% dplyr::filter(!is.na(.data$value)) %>%
    dplyr::count(tech, supply_chain, name = "n_scored")
}) %>%
  dplyr::left_join(cs_cells %>% dplyr::group_by(pillar, tech, supply_chain) %>%
                     dplyr::summarise(n_country_specific = sum(has_country_specific), .groups = "drop"),
                   by = c("pillar", "tech", "supply_chain")) %>%
  dplyr::left_join(cell_imp %>% dplyr::group_by(pillar, tech, supply_chain) %>%
                     dplyr::summarise(n_fully_imputed = sum(imputed_weight_share >= 1 - tol),
                                      .groups = "drop"),
                   by = c("pillar", "tech", "supply_chain")) %>%
  dplyr::left_join(pair_status %>% dplyr::select(pillar, tech, supply_chain, n_countries, n_unscored,
                                                 score_range, cell_status),
                   by = c("pillar", "tech", "supply_chain"))
stopifnot(all(coverage$n_scored == coverage$n_countries - coverage$n_unscored))

cov_plot <- coverage %>%
  dplyr::mutate(
    pillar = factor(pillar, levels = pillar_levels),
    stage = factor(supply_chain, levels = stage_levels),
    y = match(tech, rev(headline_techs)) + ifelse(pillar == "Energy Security", 0.17, -0.17)
  )
cov_long <- cov_plot %>%
  dplyr::select(pillar, stage, tech, y, n_scored, n_country_specific) %>%
  tidyr::pivot_longer(c(n_scored, n_country_specific), names_to = "measure", values_to = "n") %>%
  dplyr::mutate(measure = factor(measure, levels = c("n_scored", "n_country_specific"),
                                 labels = c("Scored", "With country-specific data")))
cov_missing <- tidyr::expand_grid(pillar = pillar_levels, all_pairs) %>%
  dplyr::anti_join(coverage, by = c("pillar", "tech", "supply_chain")) %>%
  dplyr::mutate(
    stage = factor(supply_chain, levels = stage_levels),
    y = match(tech, rev(headline_techs)) + ifelse(pillar == "Energy Security", 0.17, -0.17),
    label = paste0("no ", ifelse(pillar == "Energy Security", "ES", "EO"), " score")
  )
cov_flag <- cov_plot %>%
  dplyr::filter(cell_status != "ok") %>%
  dplyr::mutate(label = paste0(ifelse(pillar == "Energy Security", "ES", "EO"), " ", cell_status))
cov_xmin <- 100
stopifnot(min(c(coverage$n_scored, coverage$n_country_specific)) >= cov_xmin + 3)

# Mass points: within a pair, the largest group of scored economies sharing one pillar value
# (a floor or ceiling the missing-data rules or zero records create). Country-specific data do
# not exclude a mass point: an observed zero is country-specific when others are positive.
mass_points <- stack_pillars(function(pl, p) {
  headline(pl$index) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::mutate(v = round(.data$value, 10)) %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(n_scored = dplyr::n(), v_min = min(v), v_max = max(v)) %>%
    dplyr::count(tech, supply_chain, n_scored, v_min, v_max, v, name = "n_tied") %>%
    dplyr::slice_max(n_tied, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(share_tied = n_tied / n_scored,
                  position = dplyr::case_when(v == v_min ~ "minimum", v == v_max ~ "maximum",
                                              TRUE ~ "interior")) %>%
    dplyr::select(-v_min, -v_max)
}) %>%
  dplyr::rename(tied_value = v)
mass_points_25 <- mass_points %>% dplyr::filter(share_tied >= 0.25) %>%
  dplyr::arrange(pillar, dplyr::desc(share_tied))

# Hollow ring drawn first and larger, filled dot on top: where every scored economy has
# country-specific data the two marks read as one ringed dot.
p_cov <- ggplot(cov_plot) +
  geom_segment(aes(x = n_country_specific, xend = n_scored, y = y, yend = y, colour = pillar),
               linewidth = 0.4, show.legend = FALSE) +
  geom_point(data = dplyr::filter(cov_long, measure != "Scored"),
             aes(x = n, y = y, colour = pillar, shape = measure),
             size = 2.1, stroke = 0.55, fill = paper_ink$surface) +
  geom_point(data = dplyr::filter(cov_long, measure == "Scored"),
             aes(x = n, y = y, colour = pillar, shape = measure), size = 1.3) +
  geom_text(data = cov_missing, aes(x = cov_xmin + 3, y = y, label = label), hjust = 0,
            size = 2.8, colour = paper_ink$muted) +
  geom_text(data = cov_flag, aes(x = cov_xmin + 3, y = y + 0.33, label = label), hjust = 0,
            size = 2.8, colour = paper_ink$secondary) +
  facet_wrap(~stage, nrow = 1) +
  scale_y_continuous(breaks = seq_along(headline_techs), labels = rev(headline_techs),
                     expand = expansion(add = 0.5)) +
  # Every count lies between 100 and the 219 economies of the universe, so the axis starts at
  # 100 (the marks are dots, not bars, so no length is read from the baseline); a gap of a few
  # economies would be invisible on a 0-219 axis.
  scale_x_continuous(limits = c(cov_xmin, 225), breaks = seq(cov_xmin, 200, 25),
                     expand = expansion(mult = c(0.02, 0.02))) +
  scale_colour_manual(values = paper_pillar_cols[pillar_levels], name = NULL) +
  scale_shape_manual(values = c("Scored" = 16, "With country-specific data" = 21),
                     breaks = c("Scored", "With country-specific data"), name = NULL) +
  guides(colour = guide_legend(order = 1, override.aes = list(shape = 16, size = 1.8)),
         shape = guide_legend(order = 2, override.aes = list(size = c(1.5, 2.1)))) +
  labs(x = "Number of economies", y = NULL) +
  theme_paper(grid = "x") +
  theme(panel.spacing.x = unit(1, "lines"), legend.box = "horizontal",
        legend.box.spacing = unit(0.1, "cm"), legend.spacing.x = unit(0.4, "cm"))

save_paper_fig(p_cov, "method_coverage", width = paper_width_full, height = 3.9)

# ---------------------------------------------------------------------------
# 4b. Rankability (config/rankability.yml): which scored headline cells may be ranked
# ---------------------------------------------------------------------------
# The pillar tables carry the builder's flag; the reason is classified exclusively (a territory
# counts as a territory whatever its imputed share; "imputed" is a sovereign cell over tau).
rank_cells <- stack_pillars(function(pl, p) {
  headline(pl$index) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::select(dplyr::all_of(key_cols), value, imputed_weight_share, rankable, rank_exclusion)
}) %>%
  dplyr::mutate(
    territory = grepl("not a sovereign economy", rank_exclusion),
    over_tau = imputed_weight_share > tau + 1e-9,
    reason = dplyr::case_when(
      rankable ~ "Rankable",
      territory ~ "Territory",
      over_tau ~ "Imputed",
      TRUE ~ "Other"
    )
  )
# The flag must agree with the rule as configured.
stopifnot(!any(rank_cells$reason == "Other"),
          all(rank_cells$rankable == (!rank_cells$territory & !rank_cells$over_tau)))

rank_levels <- c("Rankable", "Imputed", "Territory")
rank_labels <- c("Rankable" = "Rankable",
                 "Imputed" = sprintf("Sovereign, imputed weight share above %s", format(tau)),
                 "Territory" = "Territory (not ranked)")
rank_cols <- c("Rankable" = paper_seq_blue[[5]], "Imputed" = paper_seq_blue[[2]],
               "Territory" = paper_ink$axis)
all_row <- "All technologies"

rank_pair <- rank_cells %>%
  dplyr::count(pillar, tech, supply_chain, reason, name = "n") %>%
  dplyr::group_by(pillar, tech, supply_chain) %>%
  dplyr::mutate(n_scored = sum(n), share = n / n_scored) %>%
  dplyr::ungroup()
rank_stage <- rank_cells %>%
  dplyr::count(pillar, supply_chain, reason, name = "n") %>%
  dplyr::group_by(pillar, supply_chain) %>%
  dplyr::mutate(n_scored = sum(n), share = n / n_scored) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(tech = all_row)
rank_y_levels <- c(rev(headline_techs), all_row)
rank_y <- stats::setNames(c(seq_along(headline_techs), length(headline_techs) + 1.4), rank_y_levels)
rank_plot <- dplyr::bind_rows(rank_pair, rank_stage) %>%
  dplyr::mutate(
    pillar = factor(pillar, levels = pillar_levels),
    stage = factor(supply_chain, levels = stage_levels),
    reason = factor(reason, levels = rank_levels),
    y = rank_y[tech]
  )
rank_all_lab <- rank_plot %>%
  dplyr::filter(tech == all_row, reason == "Rankable") %>%
  dplyr::mutate(label = paste0(sprintf("%.0f", 100 * share), "%"))
rank_missing <- tidyr::expand_grid(pillar = pillar_levels, all_pairs) %>%
  dplyr::anti_join(rank_pair, by = c("pillar", "tech", "supply_chain")) %>%
  dplyr::mutate(pillar = factor(pillar, levels = pillar_levels),
                stage = factor(supply_chain, levels = stage_levels),
                y = rank_y[tech])

p_rank <- ggplot(rank_plot) +
  geom_col(aes(x = share, y = y, fill = reason), orientation = "y", width = 0.78,
           position = position_stack(reverse = TRUE), colour = paper_ink$surface, linewidth = 0.25) +
  geom_text(data = rank_all_lab, aes(x = 0.03, y = y, label = label), hjust = 0, size = 2.7,
            colour = "#ffffff", fontface = "bold") +
  geom_text(data = rank_missing, aes(x = 0.02, y = y), label = "no ES score", hjust = 0,
            size = 2.5, colour = paper_ink$muted) +
  facet_grid(pillar ~ stage) +
  scale_y_continuous(breaks = unname(rank_y), labels = names(rank_y),
                     expand = expansion(add = 0.5)) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1"),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = rank_cols, breaks = rank_levels, labels = rank_labels[rank_levels],
                    name = NULL) +
  labs(x = "Share of the scored cells of the pair", y = NULL) +
  theme_paper(grid = "none") +
  theme(panel.spacing.x = unit(0.9, "lines"), panel.spacing.y = unit(0.7, "lines"),
        strip.text.y = element_text(angle = -90, hjust = 0.5))

save_paper_fig(p_rank, "method_rankability", width = paper_width_full, height = 4.6)

# Summary by pillar and stage (and the pillar total), for Table tab:method_rankability.
rank_summary_of <- function(df) {
  df %>% dplyr::summarise(
    n_scored = dplyr::n(),
    n_rankable = sum(rankable),
    n_territory = sum(reason == "Territory"),
    n_territory_also_over_tau = sum(reason == "Territory" & over_tau),
    n_sovereign_over_tau = sum(reason == "Imputed"),
    share_rankable = mean(rankable),
    share_sovereign_cells_rankable = sum(rankable) / sum(!territory),
    n_economies_scored = dplyr::n_distinct(Country),
    n_economies_with_rankable_cell = dplyr::n_distinct(Country[rankable]),
    n_sovereign_economies_without_rankable_cell =
      dplyr::n_distinct(Country[!territory]) - dplyr::n_distinct(Country[rankable]),
    .groups = "drop"
  )
}
rank_by_stage <- rank_cells %>% dplyr::group_by(pillar, supply_chain) %>% rank_summary_of()
rank_by_pillar <- rank_cells %>% dplyr::group_by(pillar) %>% rank_summary_of()
rank_by_pair <- rank_pair %>%
  dplyr::select(pillar, tech, supply_chain, reason, n, n_scored) %>%
  tidyr::pivot_wider(names_from = reason, values_from = n, values_fill = 0) %>%
  dplyr::mutate(share_rankable = Rankable / n_scored)
rank_pair_extremes <- rank_by_pair %>%
  dplyr::group_by(pillar) %>%
  dplyr::summarise(
    min_share = min(share_rankable),
    min_pair = paste(paste(tech, supply_chain)[share_rankable == min(share_rankable)], collapse = "; "),
    max_share = max(share_rankable),
    max_pair = paste(paste(tech, supply_chain)[share_rankable == max(share_rankable)], collapse = "; "),
    min_rankable_cells = min(Rankable),
    min_rankable_cells_pair = paste(paste(tech, supply_chain)[Rankable == min(Rankable)], collapse = "; "),
    .groups = "drop"
  )
# Mean pillar value of rankable cells against sovereign cells over the threshold (does the rule
# select on the level of the index?).
rank_value_gap <- rank_cells %>%
  dplyr::filter(reason != "Territory") %>%
  dplyr::group_by(pillar, supply_chain) %>%
  dplyr::summarise(mean_value_rankable = mean(value[rankable]),
                   mean_value_sovereign_over_tau = mean(value[!rankable]),
                   .groups = "drop")
# How many cells of each mass point (Section 4 above) the rule keeps.
mass_points_25 <- mass_points_25 %>%
  dplyr::left_join(
    rank_cells %>%
      dplyr::mutate(v = round(value, 10)) %>%
      dplyr::group_by(pillar, tech, supply_chain, v) %>%
      dplyr::summarise(n_tied_rankable = sum(rankable), .groups = "drop"),
    by = c("pillar", "tech", "supply_chain", "tied_value" = "v")
  )
# Economies with no rankable cell in a pillar, sovereign ones listed.
rank_lost_economies <- rank_cells %>%
  dplyr::group_by(pillar, Country) %>%
  dplyr::summarise(territory = dplyr::first(territory), any_rankable = any(rankable), .groups = "drop") %>%
  dplyr::filter(!territory, !any_rankable) %>%
  dplyr::group_by(pillar) %>%
  dplyr::summarise(n = dplyr::n(), economies = paste(sort(Country), collapse = "; "), .groups = "drop")

n_fmt <- function(x) format(x, big.mark = ",", trim = TRUE)
rank_row <- function(r, label) {
  paste0("      ", label, " & ", n_fmt(r$n_scored), " & ", n_fmt(r$n_rankable), " & ",
         n_fmt(r$n_territory), " & ", n_fmt(r$n_sovereign_over_tau), " & ",
         fmt(r$share_rankable), " & ", r$n_economies_with_rankable_cell, " \\\\")
}
rank_tab_rows <- unlist(lapply(pillar_levels, function(p) {
  st <- rank_by_stage %>% dplyr::filter(pillar == p) %>% dplyr::arrange(match(supply_chain, stage_levels))
  tot <- rank_by_pillar %>% dplyr::filter(pillar == p)
  c(paste0("      \\multicolumn{7}{@{}l}{\\emph{", p, "}} \\\\"),
    vapply(seq_len(nrow(st)), function(k) rank_row(st[k, ], paste0("\\quad ", st$supply_chain[k])),
           character(1)),
    rank_row(tot, "\\quad All stages"))
}))
rank_tab <- c(
  "% Generated by scripts/paper/fig_method.R; do not edit by hand.",
  "\\begin{table}[tbp]",
  "  \\centering",
  "  \\small",
  "  \\begin{threeparttable}",
  "    \\caption{Rankable cells by pillar and stage, headline technologies}\\label{tab:method_rankability}",
  "    \\begin{tabular}{@{}l *{4}{r} S[table-format=1.2] r@{}}",
  "      \\toprule",
  "      & {Scored} & {Rankable} & \\multicolumn{2}{c}{Not rankable} & {Share} & {Economies} \\\\",
  "      \\cmidrule(lr){4-5}",
  sprintf("      & {cells} & {cells} & {Territory} & {$\\iota_{its}>%s$} & {rankable} & {ranked} \\\\", format(tau)),
  "      \\midrule",
  rank_tab_rows,
  "      \\bottomrule",
  "    \\end{tabular}",
  "    \\begin{tablenotes}[flushleft]\\footnotesize",
  "      \\item \\emph{Notes:} Country--technology--stage cells of the eleven headline technologies",
  "      with a pillar value. A cell is rankable when the economy is a sovereign state (a Correlates",
  sprintf(paste0("      of War state-system code) and at most $\\tau=%s$ of its index weight rests on imputed ",
                 "components, $\\iota_{its}\\le\\tau$"), format(tau)),
  "      (\\texttt{config/rankability.yml}). Reasons are exclusive: a territory is counted as a territory",
  "      whatever its imputed share; the next column counts sovereign cells over the threshold.",
  "      Economies ranked: economies with at least one rankable cell. Source:",
  "      \\texttt{data/processed/outputs/index\\_outputs.rds}; \\texttt{scripts/paper/fig\\_method.R}.",
  "    \\end{tablenotes}",
  "  \\end{threeparttable}",
  "\\end{table}"
)
writeLines(rank_tab, file.path(paper_dir("tables"), "method_rankability.tex"), useBytes = TRUE)

# ---------------------------------------------------------------------------
# 5. Numbers for Sections 2-6 (paper/results/method.json)
# ---------------------------------------------------------------------------
as_records <- function(df) {
  df <- as.data.frame(df)
  for (j in seq_along(df)) if (is.factor(df[[j]])) df[[j]] <- as.character(df[[j]])
  df
}

# -- Country universe after harmonisation --------------------------------------------------
# Sovereign states: economies with a Correlates of War (COW) state-system code in
# countrycode's codelist. The codelist leaves Serbia (COW 345) and Kosovo (COW 347) without a
# code, so both are supplied. The COW list is the 193 UN member states plus the Holy See,
# Kosovo and Taiwan; every other code is a territory, special administrative region or other
# non-sovereign economy with an ISO3 (or World Bank) code, West Bank and Gaza included.
non_un_states <- c("TWN", "XKX", "VAT")
universe_of <- function(countries) {
  countries <- sort(unique(countries))
  iso <- country_name_to_iso3(countries)
  cown <- suppressWarnings(countrycode::countrycode(
    iso, origin = "iso3c", destination = "cown",
    custom_match = c(XKX = "347", SRB = "345"), warn = FALSE
  ))
  tibble::tibble(Country = countries, iso3c = iso, sovereign = !is.na(cown),
                 un_member = !is.na(cown) & !iso %in% non_un_states)
}
summarise_universe <- function(u) {
  list(
    n_economies = nrow(u),
    n_iso3 = dplyr::n_distinct(u$iso3c[!is.na(u$iso3c)]),
    n_without_iso3 = sum(is.na(u$iso3c)),
    n_sovereign = sum(u$sovereign),
    n_un_members = sum(u$un_member),
    sovereign_not_un = sort(u$Country[u$sovereign & !u$un_member]),
    n_non_sovereign = sum(!u$sovereign),
    non_sovereign = sort(u$Country[!u$sovereign])
  )
}
scored_countries <- function(pl, techs = NULL) {
  ix <- pl$index
  if (!is.null(techs)) ix <- ix[ix$tech %in% techs, ]
  unique(ix$Country[!is.na(ix$value)])
}
u_es <- universe_of(scored_countries(pillars[["Energy Security"]], headline_techs))
u_eo <- universe_of(scored_countries(pillars[["Economic Opportunity"]], headline_techs))
u_both <- universe_of(union(u_es$Country, u_eo$Country))
u_es_all <- universe_of(scored_countries(pillars[["Energy Security"]]))
u_eo_all <- universe_of(scored_countries(pillars[["Economic Opportunity"]]))
si_in <- si %>% dplyr::filter(is.na(si_exclusion), !is.na(strategic_index))
u_si <- universe_of(si_in$Country)
stopifnot(!any(duplicated(u_both$iso3c[!is.na(u_both$iso3c)])))

# Imputation burden of sovereign states against territories (mean iota over headline cells).
imp_by_sovereignty <- cell_imp %>%
  dplyr::left_join(u_both %>% dplyr::select(Country, sovereign), by = "Country") %>%
  dplyr::group_by(pillar, sovereign) %>%
  dplyr::summarise(n_economies = dplyr::n_distinct(Country), n_cells = dplyr::n(),
                   mean_imputed_share = mean(imputed_weight_share),
                   median_imputed_share = stats::median(imputed_weight_share),
                   n_fully_imputed = sum(imputed_weight_share >= 1 - tol), .groups = "drop")
# Economies ranked by mean iota over their headline cells: how many of the top 20 are territories.
imp_by_economy <- cell_imp %>%
  dplyr::group_by(pillar, Country) %>%
  dplyr::summarise(mean_imputed_share = mean(imputed_weight_share), .groups = "drop") %>%
  dplyr::left_join(u_both %>% dplyr::select(Country, sovereign), by = "Country") %>%
  dplyr::group_by(pillar) %>%
  dplyr::arrange(dplyr::desc(mean_imputed_share), .by_group = TRUE) %>%
  dplyr::mutate(rank = dplyr::row_number()) %>%
  dplyr::ungroup()
imp_top20 <- imp_by_economy %>%
  dplyr::filter(rank <= 20) %>%
  dplyr::group_by(pillar) %>%
  dplyr::summarise(top20_territories = sum(!sovereign), top20_sovereign = sum(sovereign),
                   top10 = paste(Country[rank <= 10], collapse = "; "), .groups = "drop")

grid_by_stage <- stack_pillars(function(pl, p) {
  headline(pl$index) %>% dplyr::filter(!is.na(.data$value)) %>%
    dplyr::group_by(supply_chain) %>%
    dplyr::summarise(n_economies = dplyr::n_distinct(Country), n_cells = dplyr::n(), .groups = "drop")
})
upstream_no_cs <- cs_cells %>%
  dplyr::filter(supply_chain == "Upstream") %>%
  dplyr::group_by(pillar, Country) %>%
  dplyr::summarise(any_cs = any(has_country_specific), .groups = "drop") %>%
  dplyr::group_by(pillar) %>%
  dplyr::summarise(n_upstream_grid = dplyr::n(), n_without_country_specific = sum(!any_cs),
                   economies = paste(sort(Country[!any_cs]), collapse = "; "), .groups = "drop")

# -- Pairs and pair status -----------------------------------------------------------------
status_counts <- function(cells) {
  tab <- table(factor(cells$cell_status, levels = c("ok", "constant", "unscored")))
  as.list(stats::setNames(as.integer(tab), names(tab)))
}
non_ok <- function(cells) {
  cells %>% dplyr::filter(cell_status != "ok") %>%
    dplyr::transmute(tech, supply_chain, cell_status, n_countries, score_range)
}
pair_summary <- lapply(pillar_levels, function(p) {
  cells_all <- pillars[[p]]$cells
  cells_h <- headline(cells_all)
  list(
    headline_pairs_scored = nrow(cells_h),
    headline_pairs_possible = length(headline_techs) * length(stage_levels),
    headline_pairs_without_score = as_records(all_pairs %>%
      dplyr::anti_join(cells_h, by = c("tech", "supply_chain"))),
    headline_status = status_counts(cells_h),
    headline_non_ok = as_records(non_ok(cells_h)),
    all_tech_pairs = nrow(cells_all),
    all_tech_status = status_counts(cells_all),
    all_tech_non_ok = as_records(non_ok(cells_all)),
    headline_country_cells = sum(!is.na(headline(pillars[[p]]$index)$value)),
    all_tech_country_cells = sum(!is.na(pillars[[p]]$index$value)),
    unscored_country_cells = nrow(pillars[[p]]$unscored_groups)
  )
})
names(pair_summary) <- pillar_levels

# -- Category sets -------------------------------------------------------------------------
cats_per_pair <- pair_sets %>%
  dplyr::group_by(pillar, supply_chain) %>%
  dplyr::summarise(min = min(n_cat_min), median = stats::median(n_cat_min), max = max(n_cat_max),
                   n_pairs = dplyr::n(), .groups = "drop")
cats_per_pair_all <- pair_sets %>%
  dplyr::group_by(pillar) %>%
  dplyr::summarise(min = min(n_cat_min), median = stats::median(n_cat_min), max = max(n_cat_max),
                   share_pairs_one_category_set = mean(n_sets == 1),
                   n_pairs_single_category = sum(n_cat_max == 1), .groups = "drop")
cats_distribution_pairs <- pair_sets %>% dplyr::count(pillar, n_categories = n_cat_min, name = "n_pairs")
cats_per_cell <- ew %>%
  dplyr::group_by(pillar, Country, tech, supply_chain) %>%
  dplyr::summarise(n_categories = sum(observed), .groups = "drop")
cats_distribution_cells <- cats_per_cell %>%
  dplyr::count(pillar, n_categories, name = "n_cells") %>%
  dplyr::group_by(pillar) %>%
  dplyr::mutate(share_cells = n_cells / sum(n_cells)) %>%
  dplyr::ungroup()
cats_per_cell_summary <- cats_per_cell %>%
  dplyr::group_by(pillar) %>%
  dplyr::summarise(n_cells = dplyr::n(), min = min(n_categories),
                   median = stats::median(n_categories), mean = mean(n_categories),
                   max = max(n_categories),
                   n_cells_single_category = sum(n_categories == 1),
                   share_cells_single_category = mean(n_categories == 1), .groups = "drop")
distinct_sets <- ew %>%
  dplyr::filter(observed) %>%
  dplyr::group_by(pillar, tech, supply_chain) %>%
  dplyr::summarise(set = paste(sort(unique(category)), collapse = "|"), .groups = "drop") %>%
  dplyr::group_by(pillar) %>%
  dplyr::summarise(n_distinct_category_sets = dplyr::n_distinct(set), .groups = "drop")
constant_categories <- cat_pair %>%
  dplyr::filter(status == "Observed, identical for every economy") %>%
  dplyr::count(pillar, category, name = "n_pairs_constant")
# Categories of the pillar never observed at a stage (the structural zeros of Figure 1).
never_at_stage <- eff %>% dplyr::filter(effective_cells == 0) %>%
  dplyr::select(pillar, supply_chain, category)

# -- Effective weights ---------------------------------------------------------------------
eff_out <- eff %>%
  dplyr::select(pillar, supply_chain, category, nominal_share, effective_cells, effective_pairs,
                n_cells, n_pairs) %>%
  dplyr::arrange(pillar, supply_chain, dplyr::desc(effective_cells))
eff_gap <- eff %>%
  dplyr::mutate(gap = effective_cells - nominal_share) %>%
  dplyr::group_by(pillar) %>%
  dplyr::summarise(
    max_effective = max(effective_cells),
    max_effective_at = paste(category[which.max(effective_cells)],
                             supply_chain[which.max(effective_cells)], sep = " / "),
    max_abs_gap = max(abs(gap)),
    mean_abs_gap = mean(abs(gap)),
    max_cells_vs_pairs_difference = max(abs(effective_cells - effective_pairs)),
    .groups = "drop"
  )
# Realised weights in single pairs (a pair scored on one category gives it weight 1).
eff_pair_max <- ew %>% dplyr::group_by(pillar) %>%
  dplyr::summarise(max_pair_weight = max(category_weight),
                   min_positive_pair_weight = min(category_weight[category_weight > 0]),
                   .groups = "drop")

# Effective weight of inputs that are constant within the pair: (a) whole categories that are
# identical for every economy of the pair; (b) components whose values (observed or imputed)
# are identical for every economy of the pair, weighted omega_k * lambda_v.
const_weight_cat <- ew %>%
  dplyr::left_join(cat_pair %>% dplyr::select(pillar, tech, supply_chain, category, status),
                   by = c("pillar", "tech", "supply_chain", "category")) %>%
  dplyr::group_by(pillar, Country, tech, supply_chain) %>%
  dplyr::summarise(w_const = sum(category_weight[status == "Observed, identical for every economy"]),
                   .groups = "drop") %>%
  dplyr::group_by(pillar, supply_chain) %>%
  dplyr::summarise(mean_weight_constant_categories = mean(w_const), .groups = "drop")
const_weight_comp <- stack_pillars(function(pl, p) {
  vc <- headline(pl$vc) %>% dplyr::filter(.data$category_weight > 0)
  vclass <- vc %>%
    dplyr::group_by(tech, supply_chain, category, variable) %>%
    dplyr::summarise(n = dplyr::n(), rng = diff(range(component_value, na.rm = TRUE)),
                     .groups = "drop") %>%
    dplyr::mutate(constant = n >= 2 & rng <= tol)
  vc %>%
    dplyr::left_join(vclass, by = c("tech", "supply_chain", "category", "variable")) %>%
    dplyr::group_by(Country, tech, supply_chain) %>%
    dplyr::summarise(w_const = sum(category_weight * component_weight_within_overall * constant),
                     .groups = "drop")
}) %>%
  dplyr::group_by(pillar, supply_chain) %>%
  dplyr::summarise(mean_weight_constant_components = mean(w_const), .groups = "drop")
# The components of (b) that sit inside categories which vary within the pair (the ones the
# text names), with the number of pairs by stage and their mean weight omega_k * lambda_v over
# the scored cells of the stage.
const_components_in_varying_cats <- stack_pillars(function(pl, p) {
  vc <- headline(pl$vc) %>% dplyr::filter(.data$category_weight > 0)
  varying <- cat_pair %>%
    dplyr::filter(.data$pillar == p, .data$status == "Observed, varies across economies") %>%
    dplyr::select(tech, supply_chain, category)
  vclass <- vc %>%
    dplyr::group_by(tech, supply_chain, category, variable) %>%
    dplyr::summarise(n = dplyr::n(), rng = diff(range(component_value, na.rm = TRUE)),
                     .groups = "drop") %>%
    dplyr::filter(n >= 2, rng <= tol) %>%
    dplyr::semi_join(varying, by = c("tech", "supply_chain", "category"))
  n_stage <- headline(pl$index) %>% dplyr::filter(!is.na(.data$value)) %>%
    dplyr::count(supply_chain, name = "n_stage_cells")
  vc %>%
    dplyr::semi_join(vclass, by = c("tech", "supply_chain", "category", "variable")) %>%
    dplyr::group_by(supply_chain, category, variable) %>%
    dplyr::summarise(n_pairs = dplyr::n_distinct(tech),
                     w_sum = sum(category_weight * component_weight_within_overall),
                     .groups = "drop") %>%
    dplyr::left_join(n_stage, by = "supply_chain") %>%
    dplyr::mutate(mean_weight_over_stage_cells = w_sum / n_stage_cells) %>%
    dplyr::select(-w_sum, -n_stage_cells)
})
eo_named_constant <- ew %>%
  dplyr::filter(pillar == "Economic Opportunity",
                category %in% c("Technology Demand", "Technological Readiness", "Energy Prices")) %>%
  dplyr::group_by(Country, tech, supply_chain) %>%
  dplyr::summarise(w = sum(category_weight), .groups = "drop") %>%
  dplyr::group_by(supply_chain) %>%
  dplyr::summarise(mean_weight_techdemand_trl_prices = mean(w), .groups = "drop")

# -- Latest-year selection: distinct years per cell ----------------------------------------
# Components that enter the index (positive weight in the variable decomposition) joined to
# the year the latest-year rule selected for them. Most imputed components have no recorded
# year and so add none; the few that do (e.g. some Input Cost Index fills in EO) are counted
# with that year. Years after the run year are projection horizons of outlook series (2030, 2035), not
# data vintages; the share is reported with and without them.
run_year <- 2026L
year_mix <- stack_pillars(function(pl, p) {
  comps <- headline(pl$vc) %>%
    dplyr::filter(.data$category_weight * .data$component_weight_within_overall > 0) %>%
    dplyr::select(dplyr::all_of(key_cols), category, variable)
  yrs <- headline(pl$years) %>%
    dplyr::semi_join(comps, by = c(key_cols, "category", "variable"))
  cells <- headline(pl$index) %>% dplyr::filter(!is.na(.data$value)) %>%
    dplyr::select(dplyr::all_of(key_cols))
  per <- yrs %>%
    dplyr::group_by(Country, tech, supply_chain) %>%
    dplyr::summarise(
      n_years = dplyr::n_distinct(Year_selected[Year_selected > 0]),
      n_vintage_years = dplyr::n_distinct(Year_selected[Year_selected > 0 & Year_selected <= run_year]),
      min_year = suppressWarnings(min(Year_selected[Year_selected > 0])),
      max_vintage_year = suppressWarnings(max(Year_selected[Year_selected > 0 &
                                                              Year_selected <= run_year])),
      .groups = "drop"
    )
  cells %>% dplyr::left_join(per, by = key_cols) %>%
    dplyr::mutate(dplyr::across(c(n_years, n_vintage_years), ~ dplyr::coalesce(.x, 0L)))
})
years_summary <- year_mix %>%
  dplyr::mutate(span = ifelse(n_vintage_years > 1, max_vintage_year - min_year, NA_real_)) %>%
  dplyr::group_by(pillar) %>%
  dplyr::summarise(
    n_cells = dplyr::n(),
    share_cells_more_than_one_year = mean(n_years > 1),
    max_distinct_years = max(n_years),
    share_cells_more_than_one_vintage_year = mean(n_vintage_years > 1),
    max_distinct_vintage_years = max(n_vintage_years),
    median_vintage_span_if_mixed = stats::median(span, na.rm = TRUE),
    max_vintage_span = max(span, na.rm = TRUE),
    n_cells_without_dated_component = sum(n_years == 0),
    .groups = "drop"
  )
years_dist <- year_mix %>% dplyr::count(pillar, n_years, name = "n_cells")
years_selected <- stack_pillars(function(pl, p) {
  headline(pl$years) %>% dplyr::count(Year_selected, name = "n_component_rows")
})

# -- Exact decompositions ------------------------------------------------------------------
decomposition <- lapply(pillar_levels, function(p) {
  pl <- pillars[[p]]
  ix <- pl$index %>% dplyr::filter(!is.na(.data$value))
  cat_sum <- pl$cc %>% dplyr::group_by(Country, tech, supply_chain) %>%
    dplyr::summarise(s_cat = sum(weighted_category_contribution), w_cat = sum(category_weight),
                     .groups = "drop")
  var_sum <- pl$vc %>% dplyr::group_by(Country, tech, supply_chain) %>%
    dplyr::summarise(s_var = sum(weighted_variable_contribution),
                     w_var = sum(category_weight * component_weight_within_overall), .groups = "drop")
  d <- ix %>% dplyr::left_join(cat_sum, by = key_cols) %>% dplyr::left_join(var_sum, by = key_cols)
  stopifnot(!anyNA(d$s_cat), !anyNA(d$s_var))
  dh <- d[d$tech %in% headline_techs, ]
  list(
    n_cells_checked = nrow(d),
    max_abs_index_minus_category_sum = max(abs(d$value - d$s_cat)),
    max_abs_index_minus_variable_sum = max(abs(d$value - d$s_var)),
    max_abs_category_weights_minus_one = max(abs(d$w_cat - 1)),
    max_abs_variable_weights_minus_one = max(abs(d$w_var - 1)),
    headline_n_cells_checked = nrow(dh),
    headline_max_abs_index_minus_category_sum = max(abs(dh$value - dh$s_cat)),
    headline_max_abs_index_minus_variable_sum = max(abs(dh$value - dh$s_var))
  )
})
names(decomposition) <- pillar_levels

# -- HS6 baskets after the 2026-10 basket review (same normalisation as the trade themes) --
# The master keeps every row it ever carried; rows the review took out of the baskets have
# excluded = TRUE and the reason in `note`. hs6_normalize_master() keeps the basket rows only.
master_raw <- utils::read.csv(
  file.path(root, "data", "reference", "energy_hs6_master.csv"),
  colClasses = c(hs6 = "character"), stringsAsFactors = FALSE, check.names = FALSE
)
master_raw$excluded <- as.logical(master_raw$excluded) %in% TRUE
master_raw$legacy_hs <- as.logical(master_raw$legacy_hs) %in% TRUE
master <- hs6_normalize_master(master_raw)
hs6_pairs <- unique(master[, c("hs6", "tech", "supply_chain", "legacy_hs", "essential", "sub_sector")])
code_n_baskets <- hs6_pairs %>% dplyr::distinct(hs6, tech, supply_chain) %>%
  dplyr::count(hs6, name = "n_baskets")
hs6_by_pair <- hs6_pairs %>%
  dplyr::left_join(code_n_baskets, by = "hs6") %>%
  dplyr::group_by(tech, supply_chain) %>%
  dplyr::summarise(codes_total = dplyr::n_distinct(hs6),
                   codes_current = dplyr::n_distinct(hs6[!legacy_hs]),
                   codes_legacy = dplyr::n_distinct(hs6[legacy_hs]),
                   codes_shared = dplyr::n_distinct(hs6[n_baskets > 1]),
                   codes_essential = dplyr::n_distinct(hs6[essential]),
                   sub_sectors = dplyr::n_distinct(sub_sector), .groups = "drop")
review_rows <- master_raw %>%
  dplyr::filter(excluded) %>%
  dplyr::mutate(hs6 = hs6_clean_code(hs6))
review_by_pair <- review_rows %>%
  dplyr::group_by(tech, supply_chain) %>%
  dplyr::summarise(rows_removed = dplyr::n(), codes_removed = dplyr::n_distinct(hs6), .groups = "drop")
hs6_by_pair <- hs6_by_pair %>%
  dplyr::full_join(review_by_pair, by = c("tech", "supply_chain")) %>%
  dplyr::mutate(dplyr::across(c(codes_total, codes_current, codes_legacy, codes_shared,
                                codes_essential, sub_sectors, rows_removed, codes_removed),
                              ~ dplyr::coalesce(as.integer(.x), 0L))) %>%
  dplyr::arrange(tech, match(supply_chain, stage_levels))
basket_codes <- unique(master$hs6)
all_codes <- unique(hs6_clean_code(master_raw$hs6))
codes_gone <- setdiff(unique(review_rows$hs6), basket_codes)
electricity_row <- review_rows %>% dplyr::filter(hs6 == "271600")
caveat_rows <- master_raw %>% dplyr::filter(!excluded, grepl("kept with caveat", note))
basket_pairs <- hs6_by_pair %>% dplyr::filter(codes_total > 0)
up_mid <- all_pairs %>% dplyr::filter(supply_chain != "Downstream")
hs6_review <- list(
  master_rows = nrow(master_raw),
  master_distinct_codes = length(all_codes),
  rows_excluded = nrow(review_rows),
  rows_in_baskets = sum(!master_raw$excluded),
  basket_rows_after_dedup = nrow(hs6_pairs),
  basket_distinct_codes = length(basket_codes),
  basket_distinct_codes_current = length(unique(master$hs6[!master$legacy_hs])),
  basket_distinct_codes_legacy = length(unique(master$hs6[master$legacy_hs])),
  basket_distinct_codes_essential = length(unique(master$hs6[master$essential])),
  basket_codes_in_two_or_more_baskets = sum(code_n_baskets$n_baskets > 1),
  codes_removed_from_every_basket = length(codes_gone),
  codes_removed_but_kept_in_another_basket = length(setdiff(unique(review_rows$hs6), codes_gone)),
  rows_kept_with_caveat = nrow(caveat_rows),
  baskets_with_codes = nrow(basket_pairs),
  scored_baskets_with_codes = sum(basket_pairs$tech %in% headline_techs),
  scored_baskets = as_records(basket_pairs %>% dplyr::filter(tech %in% headline_techs) %>%
                                dplyr::select(tech, supply_chain, codes_total)),
  electricity_removed_from = paste(electricity_row$tech, electricity_row$supply_chain),
  electricity_note = electricity_row$note,
  grid_upstream_basket = as_records(master_raw %>%
    dplyr::filter(!excluded, tech == "Electric Grid", supply_chain == "Upstream") %>%
    dplyr::transmute(hs6 = hs6_clean_code(hs6), sub_sector)),
  grid_midstream_telecom_removed = as_records(review_rows %>%
    dplyr::filter(tech == "Electric Grid", supply_chain == "Midstream") %>%
    dplyr::transmute(hs6, note = substr(note, 1, 120))),
  rows_removed_by_pair = as_records(review_by_pair),
  scored_upstream_midstream_pairs_without_basket = sort(setdiff(
    paste(up_mid$tech, up_mid$supply_chain),
    paste(basket_pairs$tech, basket_pairs$supply_chain)))
)

hs6_tab_rows <- unlist(lapply(seq_len(nrow(hs6_by_pair)), function(k) {
  r <- hs6_by_pair[k, ]
  first <- k == 1 || hs6_by_pair$tech[k - 1] != r$tech
  sprintf("      %s & %s & %d & %d & %d & %d & %d & %d \\\\",
          if (first) paste0(r$tech, if (!r$tech %in% headline_techs) "\\tnote{a}" else "") else "",
          r$supply_chain, r$codes_total, r$codes_legacy, r$codes_shared, r$codes_essential,
          r$sub_sectors, r$rows_removed)
}))
hs6_tab <- c(
  "% Generated by scripts/paper/fig_method.R; do not edit by hand.",
  "\\begin{table}[tbp]",
  "  \\centering",
  "  \\footnotesize",
  "  \\begin{threeparttable}",
  "    \\caption{HS6 baskets by technology and stage after the basket review}\\label{tab:method_hs6_baskets}",
  "    \\begin{tabular}{@{}ll*{6}{r}@{}}",
  "      \\toprule",
  "      Technology & Stage & Codes & of which & Shared with & Essential & Sub-sectors & Rows removed \\\\",
  "      & & & legacy & other baskets & & & by the review \\\\",
  "      \\midrule",
  hs6_tab_rows,
  "      \\midrule",
  sprintf("      Distinct codes & & %d & %d & %d & %d & & %d \\\\",
          hs6_review$basket_distinct_codes, hs6_review$basket_distinct_codes_legacy,
          hs6_review$basket_codes_in_two_or_more_baskets,
          hs6_review$basket_distinct_codes_essential, hs6_review$rows_excluded),
  "      \\bottomrule",
  "    \\end{tabular}",
  "    \\begin{tablenotes}[flushleft]\\footnotesize",
  "      \\item \\emph{Notes:} Distinct HS6 codes in each technology--stage basket of",
  "      \\texttt{data/reference/energy\\_hs6\\_master.csv} after the basket review of October 2026",
  sprintf(paste0("      (rows flagged \\texttt{excluded} are in no basket). Legacy: bridge codes of older ",
                 "HS revisions. Shared: codes that also belong to at least one other basket. Essential: ",
                 "codes flagged as diagnostic of the technology. Rows removed: master rows of the pair ",
                 "that the review took out of the basket, each with its reason in the master's ",
                 "\\texttt{note} column (%d rows in all; %d of the codes concerned leave every basket, ",
                 "the others stay in another technology's basket). A pair with no remaining code has no ",
                 "traded-goods basket: of the scored Upstream and Midstream pairs, %s have none, and no ",
                 "basket exists for the Downstream stage. The last row counts distinct codes over all ",
                 "baskets; its final entry is the total of removed rows."),
          hs6_review$rows_excluded, hs6_review$codes_removed_from_every_basket,
          paste(hs6_review$scored_upstream_midstream_pairs_without_basket, collapse = ", ")),
  "      \\item[a] Carried in the master; not a scored technology.",
  "    \\end{tablenotes}",
  "  \\end{threeparttable}",
  "\\end{table}"
)
writeLines(hs6_tab, file.path(paper_dir("tables"), "method_hs6_baskets.tex"), useBytes = TRUE)

# -- Bilateral trade: all-reporter Comtrade dyads, reconciled flow matrix -------------------
# Same reader columns and rule as scripts/15 (importer's declaration, else the exporter's mirror
# at the CIF/FOB factor of config/partnership.yml), so the counts reproduce its log line.
trade_years <- 2020:2024
cif_fob <- if (is.null(partnership_cfg$trade$cif_fob_factor)) {
  PARTNERSHIP_STRENGTH_CIF_FOB_FACTOR
} else {
  as.numeric(partnership_cfg$trade$cif_fob_factor)
}
dyads <- as.data.frame(data.table::fread(
  file.path(raw_dir, "comtrade_energy_dyads_all.csv"),
  select = c("reporter_iso", "partner_iso", "period", "flow_direction", "cmd_code", "primary_value"),
  colClasses = list(character = c("reporter_iso", "partner_iso", "flow_direction", "cmd_code")),
  na.strings = c("", "NA"), showProgress = FALSE
))
reporters_by <- dyads %>%
  dplyr::filter(period %in% range(trade_years)) %>%
  dplyr::group_by(period, flow_direction) %>%
  dplyr::summarise(n_reporters = dplyr::n_distinct(reporter_iso),
                   n_partners = dplyr::n_distinct(partner_iso), rows = dplyr::n(), .groups = "drop")
imp_rep <- function(y) unique(dyads$reporter_iso[dyads$period == y & dyads$flow_direction == "import"])
lapsed_importers <- sort(setdiff(imp_rep(min(trade_years)), imp_rep(max(trade_years))))
new_importers <- sort(setdiff(imp_rep(max(trade_years)), imp_rep(min(trade_years))))
dyad_summary <- list(
  rows = nrow(dyads),
  reporters_any_year = dplyr::n_distinct(dyads$reporter_iso),
  economies_any_role = dplyr::n_distinct(c(dyads$reporter_iso, dyads$partner_iso)),
  taiwan_reporter = "TWN" %in% dyads$reporter_iso,
  taiwan_partner = "TWN" %in% dyads$partner_iso
)
subcat_basket <- utils::read.csv(file.path(raw_dir, "energy_hs6_master.csv"))
trade_cells <- partnership_strength_build_trade_cells(dyads, subcat_basket, years = trade_years,
                                                      cif_fob_factor = cif_fob)
rm(dyads)
invisible(gc())
last_obs <- trade_cells[!is.na(trade_cells$source_last), ]
first_obs <- trade_cells[!is.na(trade_cells$source_first), ]
trade_matrix <- c(
  list(file = "data/raw/comtrade_energy_dyads_all.csv", years = range(trade_years)),
  dyad_summary,
  list(
    reporters_by_year_flow = as_records(reporters_by),
    importers_reporting_first_not_last = lapsed_importers,
    importers_reporting_last_not_first = new_importers,
    taiwan_cells = sum(trade_cells$exporter_iso == "TWN" | trade_cells$importer_iso == "TWN"),
    cif_fob_factor = cif_fob,
    observed_cells = nrow(trade_cells),
    tech_stage_pairs = dplyr::n_distinct(paste(trade_cells$tech, trade_cells$supply_chain)),
    cells_observed_last_year = nrow(last_obs),
    mirror_share_last_year_cells = mean(last_obs$source_last == "mirror"),
    mirror_share_last_year_value = sum(last_obs$value_last[last_obs$source_last == "mirror"]) /
      sum(last_obs$value_last),
    positive_cells_last_year = sum(last_obs$value_last > 0),
    mirror_share_last_year_positive_cells =
      mean(last_obs$source_last[last_obs$value_last > 0] == "mirror"),
    cells_observed_first_year = nrow(first_obs),
    mirror_share_first_year_cells = mean(first_obs$source_first == "mirror"),
    importers_mirror_only_last_year =
      dplyr::n_distinct(last_obs$importer_iso[last_obs$source_last == "mirror"]),
    importers_last_year = dplyr::n_distinct(last_obs$importer_iso),
    # Importers whose every last-year cell comes from partners' mirror declarations (economies
    # that did not report imports in the last year enter the matrix only this way).
    importers_all_cells_mirror_last_year = sum(tapply(last_obs$source_last == "mirror",
                                                      last_obs$importer_iso, all)),
    exporters_last_year = dplyr::n_distinct(last_obs$exporter_iso)
  )
)
rm(trade_cells, last_obs, first_obs)
invisible(gc())

# -- Comtrade energy-trade file (exports and imports by reporter and code, partner World) ----
# Rebuilt from the World rows of the same all-reporter pull; it feeds the trade themes of the
# pillars (export size, RCA, export HHI, import dependence). Re-export hubs of
# config/index_definition.yml are dropped from the Comtrade export measures by scripts/10.
energy_trade <- as.data.frame(data.table::fread(
  file.path(raw_dir, "comtrade_energy_trade.csv"),
  select = c("reporter_iso", "partner_iso", "period", "flow_direction", "cmd_code", "primary_value"),
  colClasses = list(character = c("reporter_iso", "partner_iso", "flow_direction", "cmd_code")),
  na.strings = c("", "NA"), showProgress = FALSE
))
energy_trade$cmd_code <- hs6_clean_code(energy_trade$cmd_code)
et_by <- energy_trade %>%
  dplyr::group_by(period, flow_direction) %>%
  dplyr::summarise(n_reporters = dplyr::n_distinct(reporter_iso),
                   n_codes = dplyr::n_distinct(cmd_code), rows = dplyr::n(), .groups = "drop")
index_def <- yaml::read_yaml(file.path(root, "config", "index_definition.yml"))
re_export_hubs <- unlist(index_def$trade$re_export_hubs)
energy_trade_file <- list(
  file = "data/raw/comtrade_energy_trade.csv",
  partners = sort(unique(energy_trade$partner_iso)),
  years = sort(unique(energy_trade$period)),
  by_year_flow = as_records(et_by),
  reporters_any = dplyr::n_distinct(energy_trade$reporter_iso),
  basket_codes = length(basket_codes),
  basket_codes_present = length(intersect(basket_codes, unique(energy_trade$cmd_code))),
  basket_codes_absent = sort(setdiff(basket_codes, unique(energy_trade$cmd_code))),
  basket_codes_current_hs = length(unique(master$hs6[!master$legacy_hs])),
  basket_codes_current_hs_present = length(intersect(unique(master$hs6[!master$legacy_hs]),
                                                     unique(energy_trade$cmd_code))),
  absent_codes_all_legacy = all(setdiff(basket_codes, unique(energy_trade$cmd_code)) %in%
                                  unique(master$hs6[master$legacy_hs])),
  qatar_lng_exports_2024_usd = sum(energy_trade$primary_value[
    energy_trade$reporter_iso == "QAT" & energy_trade$cmd_code == "271111" &
      energy_trade$flow_direction == "export" & energy_trade$period == 2024], na.rm = TRUE),
  taiwan_reporter = "TWN" %in% energy_trade$reporter_iso,
  re_export_hubs = re_export_hubs,
  re_export_hubs_reporting = sort(intersect(re_export_hubs, unique(energy_trade$reporter_iso)))
)
rm(energy_trade)
invisible(gc())

# -- Conduit financial centres excluded from outbound direct-investment positions -----------
conduit_rule_name <- if (is.null(partnership_cfg$outbound_fdi$conduit_rule)) "conduits" else
  partnership_cfg$outbound_fdi$conduit_rule
conduit_rule <- partnership_strength_conduit_rule(
  rule = conduit_rule_name,
  scope = if (is.null(partnership_cfg$outbound_fdi$conduit_scope)) "both" else
    partnership_cfg$outbound_fdi$conduit_scope
)
conduit_tbl <- partnership_strength_conduit_centres() %>%
  dplyr::filter(iso3 %in% conduit_rule$iso3)
conduits <- list(
  rule = conduit_rule_name,
  scope = conduit_rule$scope,
  n = nrow(conduit_tbl),
  n_dej_top10 = sum(conduit_tbl$dej_top10),
  n_offshore_added = sum(!conduit_tbl$dej_top10),
  dej_top10 = conduit_tbl$name[conduit_tbl$dej_top10],
  offshore_added = conduit_tbl$name[!conduit_tbl$dej_top10],
  iso3 = conduit_tbl$iso3,
  n_previously_excluded = sum(conduit_tbl$previous_exclusion),
  new_relative_to_previous_rule = conduit_tbl$name[!conduit_tbl$previous_exclusion]
)

# -- Policy index: IEA PAMS + CAT ----------------------------------------------------------
pol_cov <- policy_components %>%
  dplyr::filter(.data$tech %in% headline_techs) %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(
    n_economies = dplyr::n_distinct(Country), n_cells = dplyr::n(),
    max_within_pair_range = max(tapply(value, paste(tech, supply_chain),
                                       function(v) diff(range(v, na.rm = TRUE))), na.rm = TRUE),
    .groups = "drop")
pol_w <- policy_provenance$weights
pol_eligible <- sort(policy_eligibility$Country[policy_eligibility$eligible])
pol_basis <- policy_eligibility %>% dplyr::filter(eligible) %>% dplyr::select(Country, policy_basis)
pol_both <- sort(pol_basis$Country[pol_basis$policy_basis == "IEA PAMS + CAT"])
cat_bloc <- policy_provenance$component_build$cat_eu_bloc_members
cat_countries <- unique(policy_components$Country[policy_components$variable == "CAT Policy Index"])
pams_countries <- unique(policy_components$Country[policy_components$variable == "IEA PAMS Policy Index"])
coal_pol <- coalition_members %>%
  dplyr::mutate(policy_eligible = country %in% pol_eligible) %>%
  dplyr::left_join(pol_basis, by = c("country" = "Country"))

# Does the basis matter? For the economies scored on both components, the within-country
# Spearman correlation between the IEA PAMS component alone and the composite, over (a) every
# headline policy cell of the economy and (b) the economy's Strategic Index cells (the sectors
# over which the SI's within-country policy term is computed). Ties are averaged (Spearman on
# average ranks); an economy whose PAMS or composite is constant over its cells has no value.
pams_cells <- policy_components %>%
  dplyr::filter(.data$variable == "IEA PAMS Policy Index", .data$tech %in% headline_techs,
                .data$Country %in% pol_both) %>%
  dplyr::select(Country, tech, supply_chain, pams = value)
pol_vs_pams <- policy_index %>%
  dplyr::filter(.data$tech %in% headline_techs, .data$Country %in% pol_both) %>%
  dplyr::select(Country, tech, supply_chain, composite = value) %>%
  dplyr::inner_join(pams_cells, by = c("Country", "tech", "supply_chain"))
within_spearman <- function(df) {
  df %>%
    dplyr::group_by(Country) %>%
    dplyr::summarise(
      n_cells = dplyr::n(),
      rho = if (dplyr::n() >= 3 && stats::sd(pams) > 0 && stats::sd(composite) > 0) {
        stats::cor(pams, composite, method = "spearman")
      } else {
        NA_real_
      },
      .groups = "drop"
    )
}
rho_all_cells <- within_spearman(pol_vs_pams)
rho_si_cells <- within_spearman(pol_vs_pams %>%
  dplyr::semi_join(si %>% dplyr::filter(is.na(.data$si_exclusion), !is.na(.data$strategic_index)),
                   by = c("Country", "tech", "supply_chain")))
rho_summary <- function(r) {
  q <- stats::quantile(r$rho, c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE)
  list(n_economies = sum(!is.na(r$rho)), median = q[2], p25 = q[1], p75 = q[3],
       min = min(r$rho, na.rm = TRUE), max = max(r$rho, na.rm = TRUE),
       n_below_half = sum(r$rho < 0.5, na.rm = TRUE),
       median_cells = stats::median(r$n_cells))
}
# Cell-level correlation of the two components among the economies that have both.
cat_cells <- policy_components %>%
  dplyr::filter(.data$variable == "CAT Policy Index", .data$tech %in% headline_techs,
                .data$Country %in% pol_both) %>%
  dplyr::select(Country, tech, supply_chain, cat = value)
pams_cat <- pams_cells %>% dplyr::inner_join(cat_cells, by = c("Country", "tech", "supply_chain"))

# NIPO robustness variant ("NIPO added back"): the component built from the user's October 2026
# NIPO run (GTA NIPO - October 2026.xlsx, interventions in force on 5 October 2026, refreshed on
# 13 October with the debiased strength of PR #333) exactly as scripts/10 builds the NIPO rows of
# policy_component_tbl. The NIPO rows stored in policy_component_tbl are the February 2026 ones.
nipo_oct_path <- Sys.getenv("OPSI_NIPO_OCT_COMPONENT",
                            file.path(dirname(root), "scratch_final", "nipo_oct",
                                      "nipo_component_oct2026.rds"))
stopifnot(file.exists(nipo_oct_path))
nipo_oct <- readRDS(nipo_oct_path)
nipo_oct_summary <- list(
  file = basename(nipo_oct_path),
  rows = nrow(nipo_oct),
  jurisdictions = dplyr::n_distinct(nipo_oct$Country),
  headline_cells = sum(nipo_oct$tech %in% headline_techs),
  headline_jurisdictions = dplyr::n_distinct(nipo_oct$Country[nipo_oct$tech %in% headline_techs]),
  as_of = "2026-10-05 (refreshed 2026-10-13)",
  source = unique(nipo_oct$source)
)

policy_summary <- list(
  components = names(pol_w),
  weights = as.list(pol_w),
  required_components = policy_provenance$required_components,
  excluded_components = as.list(policy_provenance$excluded_components),
  eligibility_rule = policy_provenance$eligibility_rule,
  coverage = as_records(pol_cov),
  n_eligible = length(pol_eligible),
  n_both_components = length(pol_both),
  n_pams_only = sum(pol_basis$policy_basis == "IEA PAMS only"),
  basis_counts = as.list(table(pol_basis$policy_basis)),
  n_with_pams = length(pams_countries),
  n_with_cat = length(cat_countries),
  n_with_pams_not_eligible = length(setdiff(pams_countries, pol_eligible)),
  cat_eu_bloc_members = length(cat_bloc),
  cat_own_rating = length(setdiff(cat_countries, cat_bloc)),
  cat_without_pams = sort(setdiff(cat_countries, pams_countries)),
  nipo_jurisdictions_stored = policy_provenance$component_build$nipo_jurisdictions,
  nipo_october_2026 = nipo_oct_summary,
  index_cells = nrow(policy_index),
  index_cells_by_basis = as.list(table(policy_index$policy_basis)),
  index_economies = dplyr::n_distinct(policy_index$Country),
  coalition_members = nrow(coal_pol),
  coalition_members_eligible = sum(coal_pol$policy_eligible),
  coalition_by_basis = as.list(table(coal_pol$policy_basis, useNA = "ifany")),
  coalition_pams_only = as_records(coal_pol %>% dplyr::filter(policy_basis == "IEA PAMS only") %>%
                                     dplyr::select(iso3, country, tier)),
  coalition_by_tier = as_records(coal_pol %>% dplyr::group_by(tier) %>%
                                   dplyr::summarise(n = dplyr::n(), eligible = sum(policy_eligible),
                                                    pams_only = sum(policy_basis == "IEA PAMS only",
                                                                    na.rm = TRUE),
                                                    .groups = "drop")),
  coalition_without_policy = as_records(coal_pol %>% dplyr::filter(!policy_eligible) %>%
                                          dplyr::select(iso3, country, tier)),
  both_components_economies = pol_both,
  pams_vs_composite_within_country_spearman_all_cells = rho_summary(rho_all_cells),
  pams_vs_composite_within_country_spearman_si_cells = rho_summary(rho_si_cells),
  pams_cat_cell_spearman_both_economies = stats::cor(pams_cat$pams, pams_cat$cat, method = "spearman"),
  pams_cat_cells = nrow(pams_cat),
  robustness_variant_both_required = list(env = "OPSI_POLICY_REQUIRE_ALL=1",
                                          n_eligible = length(pol_both))
)
stopifnot(policy_summary$n_eligible == policy_provenance$n_countries_eligible,
          policy_summary$index_economies == policy_summary$n_eligible,
          policy_summary$n_both_components == policy_provenance$n_countries_every_component,
          policy_summary$n_both_components + policy_summary$n_pams_only == policy_summary$n_eligible)

pol_row <- function(var, label, w, status) {
  r <- pol_cov[pol_cov$variable == var, ]
  sprintf("      %s & %s & %s & %s \\\\", label, w, if (nrow(r)) n_fmt(r$n_economies) else "--", status)
}
rho_a <- policy_summary$pams_vs_composite_within_country_spearman_all_cells
rho_s <- policy_summary$pams_vs_composite_within_country_spearman_si_cells
pol_tab <- c(
  "% Generated by scripts/paper/fig_method.R; do not edit by hand.",
  "\\begin{table}[tbp]",
  "  \\centering",
  "  \\small",
  "  \\begin{threeparttable}",
  "    \\caption{Components of the Policy index}\\label{tab:method_policy}",
  "    \\begin{tabular}{@{}llrl@{}}",
  "      \\toprule",
  "      Component & Weight & Economies & In the composite \\\\",
  "      \\midrule",
  pol_row("IEA PAMS Policy Index", "IEA Policies and Measures (PAMS)",
          format(pol_w[["IEA PAMS Policy Index"]]), "yes, required"),
  pol_row("CAT Policy Index", "Climate Action Tracker rating", format(pol_w[["CAT Policy Index"]]),
          sprintf("where rated (%d own, %d via the EU)", policy_summary$cat_own_rating,
                  policy_summary$cat_eu_bloc_members)),
  sprintf("      %s & -- & %s & %s \\\\", "GTA New Industrial Policy Observatory",
          n_fmt(nipo_oct_summary$jurisdictions), "no (robustness variant, October 2026)"),
  pol_row("Dual-use policy score", "Dual-use policy score", "--", "no (constant within pairs)"),
  "      \\midrule",
  sprintf("      Economies with a Policy index & & %d & \\\\", policy_summary$n_eligible),
  sprintf("      \\quad on IEA PAMS and CAT & & %d & \\\\", policy_summary$n_both_components),
  sprintf("      \\quad on IEA PAMS only & & %d & CAT weight renormalised away \\\\",
          policy_summary$n_pams_only),
  sprintf("      Coalition members with a Policy index & & %d & of %d (%d on IEA PAMS only) \\\\",
          policy_summary$coalition_members_eligible, policy_summary$coalition_members,
          nrow(policy_summary$coalition_pams_only)),
  "      \\bottomrule",
  "    \\end{tabular}",
  "    \\begin{tablenotes}[flushleft]\\footnotesize",
  "      \\item \\emph{Notes:} Economies: economies with at least one value of the component in the",
  "      eleven headline technologies. The Policy index of a cell is the weighted mean of the",
  "      composite components present in it, with the weights renormalised over those components.",
  "      IEA PAMS is required: an economy is scored when it has an IEA PAMS value in at least one",
  "      headline cell, and the Climate Action Tracker rating is added where the economy has one.",
  "      EU member states without a rating of their own take the EU's rating. Requiring both",
  sprintf(paste0("      components (\\texttt{OPSI\\_POLICY\\_REQUIRE\\_ALL=1}, a robustness variant) leaves the ",
                 "%d economies scored on both. For those %d, the within-country Spearman correlation ",
                 "between the IEA PAMS component alone and the composite over their headline cells has ",
                 "median %s (interquartile range %s--%s), so the basis on which an economy is scored ",
                 "affects the ordering of its sectors."),
          policy_summary$n_both_components, rho_a$n_economies, fmt(rho_a$median),
          fmt(rho_a$p25), fmt(rho_a$p75)),
  sprintf(paste0("      The NIPO component enters only a robustness variant, built from the October 2026 NIPO ",
                 "release (interventions in force on 5 October 2026; %d jurisdictions, EU members ",
                 "individually); the dual-use score takes one value for every economy of a ",
                 "technology--stage pair, so it cannot rank economies."),
          policy_summary$nipo_october_2026$jurisdictions),
  "      Source: \\texttt{policy\\_component\\_tbl}, \\texttt{policy\\_eligibility}, \\texttt{policy\\_index} and",
  "      \\texttt{coalition\\_members} in \\texttt{data/processed/outputs/index\\_outputs.rds};",
  "      \\texttt{scripts/paper/fig\\_method.R}.",
  "    \\end{tablenotes}",
  "  \\end{threeparttable}",
  "\\end{table}"
)
writeLines(pol_tab, file.path(paper_dir("tables"), "method_policy.tex"), useBytes = TRUE)

# -- Comtrade critical-minerals vintage ----------------------------------------------------
resolved <- yaml::read_yaml(file.path(raw_dir, "raw_inputs_resolved.yml"))
manifest <- yaml::read_yaml(file.path(root, "config", "raw_inputs_manifest.yml"))
manifest_entries <- if (!is.null(manifest$inputs)) manifest$inputs else manifest
critmin_ids <- c("critmin_import", "critmin_export", "critmin_total_export")
critmin <- lapply(critmin_ids, function(id) {
  r <- Filter(function(e) identical(e$id, id), resolved$inputs)[[1]]
  m <- Filter(function(e) is.list(e) && identical(e$id, id), manifest_entries)
  m <- if (length(m) > 0) m[[1]] else list()
  pattern <- if (!is.null(m$pattern)) m$pattern else paste0("^", id, "_\\d{4}\\.csv$")
  reporter_col <- if (!is.null(m$reporter_col)) m$reporter_col else "reporter_iso"
  candidates <- sort(list.files(raw_dir, pattern = pattern), decreasing = TRUE)
  vintages <- lapply(candidates, function(f) {
    rep <- data.table::fread(file.path(raw_dir, f), select = reporter_col,
                             colClasses = "character", showProgress = FALSE)[[1]]
    list(file = f, reporters = dplyr::n_distinct(rep[!is.na(rep) & nzchar(rep)]))
  })
  list(id = id, file_used = r$file, vintage_key = r$vintage_key, reporters_used = r$reporters,
       min_reporters = m$min_reporters, resolve = m$resolve, candidates = vintages)
})
names(critmin) <- critmin_ids
resolved_at <- resolved$resolved_at_utc

# -- Strategic Index sample ----------------------------------------------------------------
sectors_all <- paste(all_pairs$tech, all_pairs$supply_chain, sep = " - ")
# What the rankability rule removes from the SI: cells of policy-eligible economies in the
# reported sectors with both pillars observed (the sample the SI would have without the rule).
si_candidates <- si %>%
  dplyr::filter(.data$Country %in% pol_eligible, !is.na(.data$Economic_Opportunity_Index),
                !is.na(.data$Energy_Security_Index), .data$sector %in% unique(si_in$sector)) %>%
  dplyr::mutate(kept = !is.na(.data$strategic_index) & is.na(.data$si_exclusion))
si_rank_effect <- si_candidates %>%
  dplyr::group_by(kept) %>%
  dplyr::summarise(n_cells = dplyr::n(), mean_es_need = mean(energy_security_need),
                   mean_eo = mean(Economic_Opportunity_Index), .groups = "drop")
si_kept_by_economy <- si_candidates %>%
  dplyr::group_by(Country) %>%
  dplyr::summarise(n = dplyr::n(), kept = sum(kept), .groups = "drop") %>%
  dplyr::mutate(share_kept = kept / n) %>%
  dplyr::arrange(share_kept)
si_summary <- list(
  rows = nrow(si),
  in_sample_cells = nrow(si_in),
  in_sample_economies = dplyr::n_distinct(si_in$Country),
  in_sample_coalition_members = sum(coalition_members$country %in% si_in$Country),
  in_sample_cells_by_policy_basis = as.list(table(si_in$policy_basis)),
  in_sample_economies_by_policy_basis = as.list(tapply(si_in$Country, si_in$policy_basis,
                                                       dplyr::n_distinct)),
  sectors_in_sample = dplyr::n_distinct(si_in$sector),
  headline_sectors_not_in_sample = sort(setdiff(sectors_all, unique(si_in$sector))),
  headline_sectors_absent_from_table = sort(setdiff(sectors_all, unique(si$sector))),
  exclusion_reasons = as.list(table(si$si_exclusion)),
  candidate_cells_without_rankability_rule = nrow(si_candidates),
  share_candidate_cells_kept = mean(si_candidates$kept),
  candidate_economies = dplyr::n_distinct(si_candidates$Country),
  candidate_economies_with_a_kept_cell = dplyr::n_distinct(si_candidates$Country[si_candidates$kept]),
  candidate_economies_losing_every_cell = sort(setdiff(unique(si_candidates$Country),
                                                       unique(si_candidates$Country[si_candidates$kept]))),
  rankability_effect = as_records(si_rank_effect),
  economies_keeping_under_half_of_cells = as_records(si_kept_by_economy %>% dplyr::filter(share_kept < 0.5))
)
stopifnot(sum(si_candidates$kept) == nrow(si_in))

# -- Pairs scored on a single category, and effective weights over the ok pairs only --------
single_category_pairs <- ew %>%
  dplyr::filter(observed) %>%
  dplyr::distinct(pillar, tech, supply_chain, category) %>%
  dplyr::group_by(pillar, tech, supply_chain) %>%
  dplyr::filter(dplyr::n() == 1) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(pillar, supply_chain, tech)
ok_pairs <- stack_pillars(function(pl, p) {
  headline(pl$cells) %>% dplyr::filter(.data$cell_status == "ok") %>% dplyr::select(tech, supply_chain)
})
eff_ok <- ew %>%
  dplyr::semi_join(ok_pairs, by = c("pillar", "tech", "supply_chain")) %>%
  dplyr::group_by(pillar, supply_chain, category) %>%
  dplyr::summarise(effective_cells_ok_pairs = mean(category_weight), .groups = "drop")
eff_out <- eff_out %>% dplyr::left_join(eff_ok, by = c("pillar", "supply_chain", "category"))

# -- Category set of each pair, constant categories by pair, cells with both pillars -------
pair_set_names <- ew %>%
  dplyr::filter(observed) %>%
  dplyr::distinct(pillar, tech, supply_chain, category) %>%
  dplyr::left_join(nominal %>% dplyr::select(pillar, category, cat_rank), by = c("pillar", "category")) %>%
  dplyr::arrange(pillar, supply_chain, tech, cat_rank) %>%
  dplyr::group_by(pillar, supply_chain, tech) %>%
  dplyr::summarise(categories = paste(category, collapse = "; "), n_categories = dplyr::n(),
                   .groups = "drop")
constant_category_pairs <- cat_pair %>%
  dplyr::filter(status == "Observed, identical for every economy") %>%
  dplyr::select(pillar, category, supply_chain, tech) %>%
  dplyr::arrange(pillar, category, supply_chain, tech)
both_pillars <- headline(pillars[["Energy Security"]]$index) %>%
  dplyr::filter(!is.na(.data$value)) %>%
  dplyr::inner_join(headline(pillars[["Economic Opportunity"]]$index) %>%
                      dplyr::filter(!is.na(.data$value)),
                    by = key_cols)
# Range of the selected years over positively weighted components, and the economies that
# hold the oldest one.
oldest_component <- stack_pillars(function(pl, p) {
  comps <- headline(pl$vc) %>%
    dplyr::filter(.data$category_weight * .data$component_weight_within_overall > 0) %>%
    dplyr::select(dplyr::all_of(key_cols), category, variable)
  yrs <- headline(pl$years) %>%
    dplyr::semi_join(comps, by = c(key_cols, "category", "variable")) %>%
    dplyr::filter(.data$Year_selected > 0)
  oldest <- yrs %>% dplyr::filter(.data$Year_selected == min(.data$Year_selected))
  tibble::tibble(
    min_year = min(yrs$Year_selected),
    max_year = max(yrs$Year_selected),
    max_year_at_or_before_run_year = max(yrs$Year_selected[yrs$Year_selected <= run_year]),
    oldest_year_n_economies = dplyr::n_distinct(oldest$Country),
    oldest_year_economies = if (dplyr::n_distinct(oldest$Country) <= 10) {
      paste(sort(unique(oldest$Country)), collapse = "; ")
    } else {
      NA_character_
    },
    oldest_year_variables = paste(sort(unique(oldest$variable)), collapse = "; "),
    oldest_year_cells = dplyr::n_distinct(paste(oldest$Country, oldest$tech, oldest$supply_chain))
  )
})

# -- Coverage gap (scored minus country-specific economies) by pillar and stage -------------
gap_by_stage <- coverage %>%
  dplyr::mutate(gap = n_scored - n_country_specific) %>%
  dplyr::group_by(pillar, supply_chain) %>%
  dplyr::summarise(
    n_pairs = dplyr::n(),
    n_pairs_no_gap = sum(gap == 0),
    min_gap = min(gap), techs_at_min_gap = paste(sort(tech[gap == min(gap)]), collapse = "; "),
    max_gap = max(gap), techs_at_max_gap = paste(sort(tech[gap == max(gap)]), collapse = "; "),
    max_gap_excl_constant_pairs = max(gap[cell_status == "ok"]),
    techs_at_max_gap_excl_constant = paste(sort(tech[cell_status == "ok" &
                                                       gap == max(gap[cell_status == "ok"])]),
                                           collapse = "; "),
    .groups = "drop"
  )

# -- Technology Demand (EO): which components are imputed --------------------------------------
# share_cells_imputed: share of the cells in which the category is observed where the component
# is imputed; mean_imputed_weight_in_category: mean of lambda_v * 1[imputed] over those cells
# (summing over components gives the category row of Table tab:method_imputation);
# observed_economies_median: median over pairs of the economies with a non-imputed value.
td_components <- headline(pillars[["Economic Opportunity"]]$vc) %>%
  dplyr::filter(.data$category == "Technology Demand", .data$category_weight > 0) %>%
  dplyr::group_by(supply_chain) %>%
  dplyr::mutate(n_td_cells = dplyr::n_distinct(paste(Country, tech))) %>%
  dplyr::group_by(supply_chain, variable) %>%
  dplyr::summarise(
    n_cells = dplyr::n(),
    n_pairs = dplyr::n_distinct(tech),
    share_cells_imputed = mean(imputed %in% TRUE),
    mean_imputed_weight_in_category = sum(component_weight_within_overall * (imputed %in% TRUE)) /
      dplyr::first(n_td_cells),
    observed_economies_median = stats::median(tapply(!(imputed %in% TRUE), tech, sum)),
    .groups = "drop"
  )

# -- Fills for the \tbd{} placeholders of Sections 2-4 that this theme answers -----------------
# Each fill is LaTeX that replaces the whole \tbd{...} at the named place; the anchor is the
# opening text of the placeholder.
n_txt <- function(x) format(x, big.mark = ",", trim = TRUE)
pct <- function(x) sprintf("%.0f", 100 * x)
g_es <- grid_by_stage %>% dplyr::filter(pillar == "Energy Security") %>%
  dplyr::arrange(match(supply_chain, stage_levels))
g_eo <- grid_by_stage %>% dplyr::filter(pillar == "Economic Opportunity") %>%
  dplyr::arrange(match(supply_chain, stage_levels))
ys_es <- years_summary[years_summary$pillar == "Energy Security", ]
ys_eo <- years_summary[years_summary$pillar == "Economic Opportunity", ]
cpc_es <- cats_per_cell_summary[cats_per_cell_summary$pillar == "Energy Security", ]
cpc_eo <- cats_per_cell_summary[cats_per_cell_summary$pillar == "Economic Opportunity", ]
cpp <- cats_per_pair_all %>% dplyr::left_join(distinct_sets, by = "pillar")
ips <- function(p) {
  v <- imp_pillar_stage %>% dplyr::filter(pillar == p) %>%
    dplyr::arrange(match(supply_chain, stage_levels))
  paste0(paste(fmt(v$mean_imputed_share[1:2]), collapse = ", "), " and ",
         fmt(v$mean_imputed_share[3]))
}
fi_eo <- fully_imputed_cells %>% dplyr::filter(pillar == "Economic Opportunity") %>%
  dplyr::count(Country, sort = TRUE)
ue <- upstream_no_cs[upstream_no_cs$pillar == "Energy Security", ]
uo <- upstream_no_cs[upstream_no_cs$pillar == "Economic Opportunity", ]
stopifnot(identical(ue$economies, uo$economies))
ps_es <- pair_summary[["Energy Security"]]
ps_eo <- pair_summary[["Economic Opportunity"]]
non_ok_es <- ps_es$headline_non_ok
non_ok_eo <- ps_eo$headline_non_ok
no_es <- ps_es$headline_pairs_without_score
# Final run: every scored pair is ok in both pillars (Nuclear Upstream, constant before the
# Red Book uranium data, now varies), and three pairs have no ES category.
stopifnot(nrow(non_ok_es) == 0, nrow(non_ok_eo) == 0, nrow(no_es) == 3,
          ps_eo$headline_status$ok == ps_eo$headline_pairs_scored)
d_es <- decomposition[["Energy Security"]]
d_eo <- decomposition[["Economic Opportunity"]]
stopifnot(d_es$max_abs_index_minus_category_sum == 0, d_eo$max_abs_index_minus_category_sum == 0)
var_err <- max(d_es$max_abs_index_minus_variable_sum, d_eo$max_abs_index_minus_variable_sum)
var_err_tex <- sub("e-0*(\\d+)$", "\\\\times10^{-\\1}", formatC(var_err, format = "e", digits = 1))
latex_names <- function(x) {
  x <- gsub("Virgin Islands \\(U\\.S\\.\\)", "U.S.\\\\ Virgin Islands", x)
  gsub("St\\. ", "St.\\\\ ", x)
}
n_full <- function(p) imp_pillar_all$n_fully_imputed[imp_pillar_all$pillar == p]
one_set <- function(p) {
  s <- cpp$share_pairs_one_category_set[cpp$pillar == p]
  if (s == 1) "all" else paste0(pct(s), " percent")
}
tbd_fills <- list(
  list(file = "sections/02_framework.tex",
       placeholder = "number of technology--stage pairs with an ES score",
       fill = as.character(ps_es$headline_pairs_scored)),
  list(file = "sections/02_framework.tex",
       placeholder = "number of technology--stage pairs with an EO score",
       fill = as.character(ps_eo$headline_pairs_scored)),
  list(file = "sections/02_framework.tex",
       placeholder = "number of economies with at least one ES score",
       fill = as.character(nrow(u_es))),
  list(file = "sections/02_framework.tex",
       placeholder = "number of economies with at least one EO score",
       fill = as.character(nrow(u_eo))),
  list(file = "sections/02_framework.tex",
       placeholder = "number of the 23 coalition members with a Policy index",
       fill = sprintf("all %d (%d of them on IEA PAMS alone, for want of a Climate Action Tracker rating)",
                      policy_summary$coalition_members_eligible,
                      nrow(policy_summary$coalition_pams_only))),
  list(file = "sections/02_framework.tex",
       placeholder = "technology--stage pairs excluded from the SI as constant in ES or EO, if any",
       fill = sprintf(paste0("no pair: every scored pair varies across economies in both pillars. ",
                             "Geothermal--Midstream is left out by rule (it also has no $\\ES$ value), and ",
                             "%s have no $\\ES$ value"),
                      paste(paste0(no_es$tech, "--", no_es$supply_chain)[
                        paste(no_es$tech, no_es$supply_chain) != "Geothermal Midstream"],
                        collapse = " and "))),
  list(file = "sections/03_data.tex",
       placeholder = paste("number of economies (ISO3) with at least one ES score, and of these the",
                           "number of sovereign states and of territories"),
       fill = sprintf(paste0("%d economies, each with its own ISO3 code, of which %d are sovereign ",
                             "states (the %d members of the United Nations, Kosovo and Taiwan) and %d ",
                             "are territories, special administrative regions or the West Bank and Gaza"),
                      nrow(u_es), sum(u_es$sovereign), sum(u_es$un_member), sum(!u_es$sovereign))),
  list(file = "sections/03_data.tex",
       placeholder = "number of economies with at least one EO score, sovereign states and territories",
       fill = sprintf("the same %d economies (%d sovereign states and %d territories)",
                      nrow(u_eo), sum(u_eo$sovereign), sum(!u_eo$sovereign))),
  list(file = "sections/04_normalization.tex",
       placeholder = paste("share of ES and EO country-cells that combine components from more than",
                           "one year, and the maximum number of distinct years in a cell"),
       fill = sprintf(paste0("%s percent of the $\\ES$ cells and %s percent of the $\\EO$ cells of the ",
                             "eleven headline technologies combine components from more than one year, ",
                             "with at most %d distinct years in an $\\ES$ cell and %d in an $\\EO$ cell ",
                             "(%d once the projection horizons after %d of the outlook series are set aside)"),
                      pct(ys_es$share_cells_more_than_one_year), pct(ys_eo$share_cells_more_than_one_year),
                      ys_es$max_distinct_years, ys_eo$max_distinct_years,
                      ys_eo$max_distinct_vintage_years, run_year)),
  list(file = "sections/04_normalization.tex",
       placeholder = paste("number of countries in the ES and EO grids by stage, and the number of",
                           "upstream grid countries covered by no country-specific upstream source"),
       fill = sprintf(paste0("the $\\ES$ grids of the headline technologies span %d, %d and %d economies ",
                             "at the upstream, midstream and downstream stages and the $\\EO$ grids %d, %d ",
                             "and %d; %s"),
                      g_es$n_economies[1], g_es$n_economies[2], g_es$n_economies[3],
                      g_eo$n_economies[1], g_eo$n_economies[2], g_eo$n_economies[3],
                      if (ue$n_without_country_specific == 0) {
                        sprintf(paste0("every one of the %d economies in the upstream grids has a ",
                                       "country-specific upstream component in at least one upstream ",
                                       "pair of each pillar"), ue$n_upstream_grid)
                      } else {
                        sprintf(paste0("%d of the %d economies in the upstream grids (%s) have no ",
                                       "country-specific upstream component in either pillar"),
                                ue$n_without_country_specific, ue$n_upstream_grid,
                                latex_names(gsub("; ", ", ", ue$economies)))
                      })),
  list(file = "sections/04_normalization.tex",
       placeholder = paste("share of ES and EO technology-stage pairs in which all scored countries share",
                           "one category set, and the distribution of the number of observed categories",
                           "per cell"),
       fill = sprintf(paste0("all scored economies share one category set in %s of the %d $\\ES$ pairs ",
                             "and %s of the %d $\\EO$ pairs, and a cell has between %d and %d observed ",
                             "categories in $\\ES$ (median %g; %s percent of cells have a single ",
                             "category) and between %d and %d in $\\EO$ (median %g)"),
                      one_set("Energy Security"), ps_es$headline_pairs_scored,
                      one_set("Economic Opportunity"), ps_eo$headline_pairs_scored,
                      cpc_es$min, cpc_es$max, cpc_es$median, pct(cpc_es$share_cells_single_category),
                      cpc_eo$min, cpc_eo$max, cpc_eo$median)),
  list(file = "sections/04_normalization.tex",
       placeholder = paste("maximum absolute difference between the index and the sum of its category",
                           "contributions, and between the index and the sum of its variable",
                           "contributions, ES and EO"),
       fill = sprintf(paste0("exactly zero for the category decomposition and $%s$ for the variable ",
                             "decomposition, in both pillars (%s $\\ES$ and %s $\\EO$ cells, all ",
                             "technologies)"),
                      var_err_tex, n_txt(d_es$n_cells_checked), n_txt(d_eo$n_cells_checked))),
  list(file = "sections/04_normalization.tex",
       placeholder = paste("number of ES and EO country-cells among the eleven headline technologies",
                           "with imputed weight share equal to one, and the mean imputed weight share",
                           "by stage"),
       fill = sprintf(paste0("%s $\\ES$ cell and %d $\\EO$ cells of the eleven headline technologies ",
                             "have $\\iota_{its}=1$ (%s), and the mean imputed weight share is %s in ",
                             "$\\ES$ and %s in $\\EO$ at the upstream, midstream and downstream stages"),
                      if (n_full("Energy Security") == 0) "no" else as.character(n_full("Energy Security")),
                      n_full("Economic Opportunity"),
                      paste(sprintf("%s %d", latex_names(fi_eo$Country), fi_eo$n), collapse = ", "),
                      ips("Energy Security"), ips("Economic Opportunity"))),
  list(file = "sections/04_normalization.tex",
       placeholder = paste("number of ES and EO technology-stage pairs with status ok, constant and",
                           "unscored, and the list of non-ok pairs"),
       fill = sprintf(paste0("all %d headline pairs with an $\\ES$ score and all %d with an $\\EO$ score ",
                             "have status ok, none is constant and none is unscored, while %s have no ",
                             "observed $\\ES$ category. Over every technology, %d of the %d $\\ES$ pairs ",
                             "and %d of the %d $\\EO$ pairs are ok"),
                      ps_es$headline_status$ok, ps_eo$headline_status$ok,
                      paste0("three pairs (", paste(paste0(no_es$tech, "--", no_es$supply_chain),
                                                     collapse = ", "), ")"),
                      ps_es$all_tech_status$ok, ps_es$all_tech_pairs,
                      ps_eo$all_tech_status$ok, ps_eo$all_tech_pairs)),
  list(file = "sections/08_strategic.tex",
       placeholder = "number of countries with each component and number of policy-eligible countries",
       fill = sprintf(paste0("IEA PAMS %d economies, CAT %d (%d with their own rating, %d EU members ",
                             "through the EU's), NIPO %d in the October 2026 release (robustness variant only), dual-use %d; %d ",
                             "economies have a Policy index, %d on both components and %d on IEA PAMS alone"),
                      policy_summary$n_with_pams, policy_summary$n_with_cat,
                      policy_summary$cat_own_rating, policy_summary$cat_eu_bloc_members,
                      policy_summary$nipo_october_2026$jurisdictions,
                      pol_cov$n_economies[pol_cov$variable == "Dual-use policy score"],
                      policy_summary$n_eligible, policy_summary$n_both_components,
                      policy_summary$n_pams_only)),
  list(file = "sections/08_strategic.tex",
       placeholder = "number of policy-eligible countries and any coalition member that fails, with the missing",
       fill = sprintf(paste0("%d economies, every coalition member among them; %d members have no Climate ",
                             "Action Tracker rating and are scored on IEA PAMS alone (%s)"),
                      policy_summary$n_eligible, nrow(policy_summary$coalition_pams_only),
                      paste(policy_summary$coalition_pams_only$iso3, collapse = ", "))),
  list(file = "sections/08_strategic.tex",
       placeholder = "number of policy-eligible countries (Results paragraph 'Coverage of the Policy index')",
       fill = as.character(policy_summary$n_eligible)),
  list(file = "sections/08_strategic.tex",
       placeholder = paste("number of coalition members among them; names of any member that fails and",
                           "the missing component"),
       fill = sprintf(paste0("all %d coalition members among them, none failing; %d members have no ",
                             "Climate Action Tracker rating and are scored on IEA PAMS alone (%s)"),
                      policy_summary$coalition_members, nrow(policy_summary$coalition_pams_only),
                      paste(policy_summary$coalition_pams_only$iso3, collapse = ", ")))
)

# -- Assemble ------------------------------------------------------------------------------
rankability <- list(
  rule = list(config = "config/rankability.yml", max_imputed_weight_share = tau,
              sovereign = "Correlates of War state-system code (Serbia 345 and Kosovo 347 supplied)"),
  by_pillar = as_records(rank_by_pillar),
  by_stage = as_records(rank_by_stage),
  pair_extremes = as_records(rank_pair_extremes),
  by_pair = as_records(rank_by_pair %>% dplyr::arrange(pillar, supply_chain, tech)),
  value_gap_rankable_vs_sovereign_over_tau = as_records(rank_value_gap),
  sovereign_economies_without_rankable_cell = as_records(rank_lost_economies),
  economies_ranked_in_both_pillars = length(intersect(
    unique(rank_cells$Country[rank_cells$pillar == "Energy Security" & rank_cells$rankable]),
    unique(rank_cells$Country[rank_cells$pillar == "Economic Opportunity" & rank_cells$rankable]))),
  cells_rankable_in_both_pillars = nrow(
    rank_cells %>% dplyr::filter(pillar == "Energy Security", rankable) %>%
      dplyr::semi_join(rank_cells %>% dplyr::filter(pillar == "Economic Opportunity", rankable),
                       by = key_cols)),
  taiwan = as_records(rank_cells %>% dplyr::filter(Country == "Taiwan") %>%
                        dplyr::group_by(pillar) %>%
                        dplyr::summarise(n_scored = dplyr::n(), n_rankable = sum(rankable),
                                         .groups = "drop"))
)

results <- list(
  source = paste("data/processed/outputs/index_outputs.rds (final rerun of 2026-10-12:",
                 "scripts 04 10 12 13 20 15 25, then 30 at lambda 0.78 and 40)"),
  definitions = list(
    pair = "technology x stage (t,s); headline = the 11 technologies of the reported ES/EO tables",
    cell = "country x technology x stage (i,t,s) with an index value",
    observed_category = "category with a category score in the cell (category_weight > 0)",
    constant = "range across the economies of the pair <= 1e-12",
    country_specific_component = "non-imputed values take >= 2 distinct values across the economies of the pair",
    effective_weight = "category_weight of category_contributions, 0 where unobserved; mean over scored headline cells (effective_cells) or over pairs (effective_pairs)",
    category_imputed_share = "sum over the components of the category of lambda_v * 1[imputed]; mean over cells in which the category is observed",
    pillar_imputed_share = "diagnostics$index_imputation$imputed_weight_share (iota)",
    sovereign = "economy with a Correlates of War state-system code in countrycode::codelist (includes Kosovo and Taiwan)",
    years = "distinct Year_selected > 0 over positively weighted components of the cell; vintage years exclude projection horizons > run_year"
  ),
  headline_techs = headline_techs,
  run_year = run_year,
  pairs = pair_summary,
  universe = list(
    es_headline = summarise_universe(u_es),
    eo_headline = summarise_universe(u_eo),
    es_or_eo_headline = summarise_universe(u_both),
    es_and_eo_headline = length(intersect(u_es$Country, u_eo$Country)),
    es_only_headline = sort(setdiff(u_es$Country, u_eo$Country)),
    eo_only_headline = sort(setdiff(u_eo$Country, u_es$Country)),
    es_all_techs = summarise_universe(u_es_all)[c("n_economies", "n_iso3", "n_sovereign", "n_non_sovereign")],
    eo_all_techs = summarise_universe(u_eo_all)[c("n_economies", "n_iso3", "n_sovereign", "n_non_sovereign")],
    strategic_index = summarise_universe(u_si)[c("n_economies", "n_iso3", "n_sovereign", "n_non_sovereign")],
    grid_by_stage = as_records(grid_by_stage),
    upstream_grid_without_country_specific_data = as_records(upstream_no_cs)
  ),
  category_sets = list(
    per_pillar = as_records(cats_per_pair_all %>% dplyr::left_join(distinct_sets, by = "pillar")),
    per_stage = as_records(cats_per_pair),
    distribution_pairs = as_records(cats_distribution_pairs),
    distribution_cells = as_records(cats_distribution_cells),
    per_cell_summary = as_records(cats_per_cell_summary),
    by_pair = as_records(pair_sets %>% dplyr::arrange(pillar, supply_chain, tech)),
    categories_constant_within_pair = as_records(constant_categories),
    categories_never_observed_at_stage = as_records(never_at_stage),
    single_category_pairs = as_records(single_category_pairs),
    pair_category_sets = as_records(pair_set_names),
    constant_category_pairs = as_records(constant_category_pairs)
  ),
  effective_weights = list(
    nominal = as_records(nominal %>% dplyr::select(pillar, category, w, nominal_share)),
    by_stage = as_records(eff_out),
    all_stages = as_records(eff_all_stages),
    summary = as_records(eff_gap %>% dplyr::left_join(eff_pair_max, by = "pillar")),
    weight_on_constant_categories_by_stage = as_records(const_weight_cat),
    weight_on_constant_components_by_stage = as_records(const_weight_comp),
    constant_components_in_varying_categories = as_records(const_components_in_varying_cats),
    eo_weight_on_technology_demand_trl_prices_by_stage = as_records(eo_named_constant)
  ),
  imputation = list(
    pillar_by_stage = as_records(imp_pillar_stage),
    pillar_all = as_records(imp_pillar_all),
    category_by_stage = as_records(imp_cat_stage),
    category_contribution_to_mean_iota = as_records(
      imp_contrib %>% dplyr::filter(contribution_to_mean_iota > 0)),
    fully_imputed_cells = as_records(fully_imputed_cells),
    by_sovereignty = as_records(imp_by_sovereignty),
    top20_economies_by_mean_iota = as_records(imp_top20),
    technology_demand_components = as_records(td_components),
    iota_reproduction_max_abs_error = iota_reproduction_error
  ),
  coverage = list(
    by_pair = as_records(coverage %>% dplyr::arrange(pillar, supply_chain, tech)),
    components_common_within_pair = as_records(cell_common_components),
    pairs_with_all_scored_country_specific = as_records(
      coverage %>% dplyr::group_by(pillar) %>%
        dplyr::summarise(n_pairs = dplyr::n(), n_all = sum(n_country_specific == n_scored),
                         min_share = min(n_country_specific / n_scored), .groups = "drop")),
    gap_by_stage = as_records(gap_by_stage),
    mass_points_share_25 = as_records(mass_points_25)
  ),
  years = list(summary = as_records(years_summary), distribution = as_records(years_dist),
               selected_year_rows = as_records(years_selected),
               extreme_years_of_weighted_components = as_records(oldest_component)),
  headline_cells_with_both_pillars = nrow(both_pillars),
  policy_index_economies = dplyr::n_distinct(policy_index$Country),
  decomposition = decomposition,
  hs6_by_pair = as_records(hs6_by_pair),
  hs6_totals = list(distinct_codes = length(unique(master$hs6)),
                    current_codes = length(unique(master$hs6[!master$legacy_hs])),
                    legacy_codes = length(unique(master$hs6[master$legacy_hs]))),
  comtrade_critmin = list(resolved_at_utc = resolved_at, inputs = critmin),
  strategic_index_sample = si_summary,
  rankability = rankability,
  hs6_review = hs6_review,
  trade_matrix = trade_matrix,
  energy_trade_file = energy_trade_file,
  conduits = conduits,
  policy = policy_summary,
  tbd_fills = tbd_fills
)
write_paper_results(results, "method")
