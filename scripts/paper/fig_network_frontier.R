# Figure and numbers: the diversification frontier of the allied network design and the calibration
# of its weight lambda (paper Section 10 and Appendix C; fig:network_frontier,
# results/network_frontier.json).
#
# The design maximises, stage by stage, its linear score (node and edge terms) minus
# lambda * HHI of the designed production shares. lambda is set by a rule: the smallest value on a
# grid at which the median designed HHI over the 22 Upstream and Midstream stages is at most the
# median observed export HHI of the network's own nodes (2024 reconciled basket trade, the EU-27 as
# one exporter counting extra-EU exports only, the stages with an HS6 basket: 18 since the Upstream
# baskets of 2026-10-11, 14 before; the script checks the count against the master). The grid study
# (0 to 1.5, step 0.01 between 0.70 and 1.0) was run outside the repository on the inputs of the
# final pipeline run of 2026-10-12; this script reads its summaries and runs and draws
#   (a) the median designed HHI against lambda over the whole grid (caps on, the production
#       configuration), with the observed target and the no-caps run at the pick (0.78);
#   (b) the neighbourhood of the pick (0.70-0.90), marking the steps where the median rises;
#   (c) the score given up: the summed linear score of the 22 stage programs (node + edge terms,
#       without the HHI penalty) relative to lambda = 0.
# It re-applies the rule to the grid and checks that the study's run at the picked lambda is the
# stored production design (data/processed/outputs/allied_network_*.csv), so the study's inputs
# and settings are those of scripts/30.
#
# Reads: <study dir>/lambda_study_g37.csv (caps on), lambda_study_nc37.csv (caps off),
# runs/g37_l*.rds, runs/nc37_l*.rds, observed_target.rds, with study dir = OPSI_NETWORK_STUDY_DIR
# (default ../scratch_final/network next to the repository root); and
# data/processed/outputs/allied_network_{diagnostics,specialization,observed_hhi}.csv.
# Writes: paper/figures/network_frontier.pdf (+ png) and paper/results/network_frontier.json.
#
# Run from the repository root (under a minute):
#   Rscript scripts/paper/fig_network_frontier.R

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
  library(purrr)
})

root <- paper_repo_root()
config <- yaml::read_yaml(Sys.getenv("OPSI_CONFIG", file.path(root, "config", "config.yml")))
outputs_dir <- file.path(root, config$processed_dir, "outputs")
study_dir <- Sys.getenv("OPSI_NETWORK_STUDY_DIR", file.path(dirname(root), "scratch_final", "network"))

stored_diag <- utils::read.csv(file.path(outputs_dir, "allied_network_diagnostics.csv"), stringsAsFactors = FALSE)
stored_spec <- utils::read.csv(file.path(outputs_dir, "allied_network_specialization.csv"), stringsAsFactors = FALSE)
obs_hhi <- utils::read.csv(file.path(outputs_dir, "allied_network_observed_hhi.csv"), stringsAsFactors = FALSE)
lambda_prod <- unique(stored_diag$diversity_lambda)
n_nodes <- dplyr::n_distinct(stored_spec$iso3c)
stopifnot(length(lambda_prod) == 1, nrow(stored_diag) == 22)

tag_caps <- sprintf("g%d", n_nodes)
tag_nocaps <- sprintf("nc%d", n_nodes)
grid <- utils::read.csv(file.path(study_dir, sprintf("lambda_study_%s.csv", tag_caps)), stringsAsFactors = FALSE) %>%
  dplyr::arrange(lambda)
nocaps <- utils::read.csv(file.path(study_dir, sprintf("lambda_study_%s.csv", tag_nocaps)), stringsAsFactors = FALSE) %>%
  dplyr::arrange(lambda)
target_study <- readRDS(file.path(study_dir, "observed_target.rds"))$target
target <- stats::median(obs_hhi$hhi)
# The network stages with an HS6 basket, from the master (14 until the Upstream baskets of
# Electric Vehicles, Solar, Wind and Nuclear of 2026-10-11, 18 since).
source(file.path(root, "R", "utils", "schema.R"))
source(file.path(root, "R", "utils", "hs6_crosswalk.R"))
headline_techs <- unlist(yaml::read_yaml(file.path(root, "config", "coalition.yml"))$headline_techs)
basket <- hs6_normalize_master(utils::read.csv(
  file.path(root, "data", "reference", "energy_hs6_master.csv"),
  colClasses = c(hs6 = "character"), stringsAsFactors = FALSE, check.names = FALSE
))
n_basket_stages <- nrow(unique(basket[basket$tech %in% headline_techs &
                                        basket$supply_chain %in% c("Upstream", "Midstream"),
                                      c("tech", "supply_chain")]))
# The study's target must be the one scripts/30 wrote for the final run
stopifnot(abs(target - target_study) < 1e-9, nrow(obs_hhi) == n_basket_stages)

# ---------------------------------------------------------------------------
# Runs: objective decomposition per lambda (caps on), checks against the stored design
read_run <- function(tag, lam) readRDS(file.path(study_dir, "runs", sprintf("%s_l%s.rds", tag, format(lam))))
run_stats <- purrr::map_dfr(grid$lambda, function(lam) {
  r <- read_run(tag_caps, lam)
  d <- r$diagnostics
  stopifnot(isTRUE(all.equal(r$params$diversity_lambda, lam)), isTRUE(r$params$portfolio_enable),
            identical(r$params$node_policy, FALSE), nrow(d) == 22)
  tibble::tibble(
    lambda = lam,
    linear_score_sum = sum(d$objective_node + d$objective_edge),
    node_score_sum = sum(d$objective_node), edge_score_sum = sum(d$objective_edge),
    hhi_mean = mean(d$hhi), hhi_median_check = stats::median(d$hhi),
    n_stages_milp = sum(d$method == "milp")
  )
})
grid <- grid %>% dplyr::left_join(run_stats, by = "lambda")
# Every stage is solved in every run; a run may end one stage on the greedy fallback (MILP time limit)
stopifnot(max(abs(grid$median_hhi - grid$hhi_median_check)) < 1e-12, all(grid$n_stages_milp == grid$milp_final),
          all(grid$n_stages_milp >= 21))
fallback_runs <- grid %>% dplyr::filter(n_stages_milp < 22) %>%
  dplyr::transmute(lambda, n_stages_milp, fallback_stages = final_greedy)

# The study's run at the production lambda is the stored production design
prod_run <- read_run(tag_caps, lambda_prod)
repro <- stored_spec %>%
  dplyr::select(tech, supply_chain, iso3c, a = production_share) %>%
  dplyr::full_join(prod_run$specialization %>% dplyr::select(tech, supply_chain, iso3c, b = production_share),
                   by = c("tech", "supply_chain", "iso3c"))
repro_diff <- max(abs(repro$a - repro$b))
stopifnot(!anyNA(repro$a), !anyNA(repro$b), repro_diff < 1e-9)

nocaps_stats <- purrr::map_dfr(nocaps$lambda, function(lam) {
  r <- read_run(tag_nocaps, lam)
  stopifnot(isFALSE(r$params$portfolio_enable), isTRUE(all.equal(r$params$diversity_lambda, lam)))
  tibble::tibble(lambda = lam, linear_score_sum = sum(r$diagnostics$objective_node + r$diagnostics$objective_edge))
})
nocaps <- nocaps %>% dplyr::left_join(nocaps_stats, by = "lambda")

# ---------------------------------------------------------------------------
# The rule, non-monotone steps, score cost
grid <- grid %>% dplyr::mutate(meets = median_hhi <= target + 1e-12)
pick <- min(grid$lambda[grid$meets])
stopifnot(isTRUE(all.equal(pick, lambda_prod)))
pick_row <- grid[grid$lambda == pick, ]
stopifnot(pick_row$n_stages_milp == 22)
# The grid's fine-step range: the points spaced 0.01 apart
fine <- grid$lambda[abs(c(diff(grid$lambda), NA) - 0.01) < 1e-9 | abs(c(NA, diff(grid$lambda)) - 0.01) < 1e-9]
fine_range <- range(fine, na.rm = TRUE)
below <- grid %>% dplyr::filter(lambda < pick) %>% dplyr::slice_max(lambda, n = 2)
above <- grid %>% dplyr::filter(lambda > pick) %>% dplyr::slice_min(lambda, n = 1)
# Steps between consecutive grid points at which the median designed HHI rises (or stays flat)
steps <- grid %>%
  dplyr::transmute(lambda_from = dplyr::lag(lambda), lambda_to = lambda,
                   hhi_from = dplyr::lag(median_hhi), hhi_to = median_hhi) %>%
  dplyr::filter(!is.na(lambda_from)) %>%
  dplyr::mutate(change = hhi_to - hhi_from)
rises <- steps %>% dplyr::filter(change > 1e-9)
flats <- steps %>% dplyr::filter(abs(change) <= 1e-9)
# Grid points at or above the pick whose median is above the target (none: the rule is a first
# crossing, but the median could cross back)
recross <- grid %>% dplyr::filter(lambda > pick, !meets)
L0 <- grid$linear_score_sum[grid$lambda == 0]
grid <- grid %>% dplyr::mutate(score_change_pct = 100 * (linear_score_sum - L0) / L0)
nocaps <- nocaps %>% dplyr::mutate(score_change_pct = 100 * (linear_score_sum - L0) / L0)
nocaps_pick <- nocaps[abs(nocaps$lambda - pick) < 1e-9, ]
pick_row <- grid[abs(grid$lambda - pick) < 1e-9, ]

results <- list(
  description = paste(
    "Calibration of the diversification weight lambda of the allied network design: lambda study on the final",
    "node set (", n_nodes, "nodes; caps on = production configuration of scripts/30; Upstream and Midstream; MILP for",
    "every stage; 300 s stage limit; 30 cap passes). median_hhi = median over the 22 stages of the designed HHI;",
    "target = median over the", nrow(obs_hhi), "basket stages of the nodes' observed 2024 export HHI (EU-27 extra-EU),",
    "allied_network_observed_hhi.csv. Rule: the smallest grid lambda with median_hhi <= target. linear score =",
    "sum over the 22 stages of the node and edge terms of the stage objective (without the HHI penalty);",
    "score_change_pct relative to lambda = 0 with caps."),
  study_files = list(caps = sprintf("lambda_study_%s.csv", tag_caps), nocaps = sprintf("lambda_study_%s.csv", tag_nocaps),
                     runs = sprintf("runs/%s_l*.rds, runs/%s_l*.rds", tag_caps, tag_nocaps)),
  target = target, target_equivalent_exporters = 1 / target, n_basket_stages = nrow(obs_hhi),
  pick = pick, production_lambda = lambda_prod,
  study_run_at_pick_equals_production_max_abs_share_diff = repro_diff,
  at_pick = list(median_hhi = pick_row$median_hhi, effective_producers = pick_row$median_eff,
                 median_n_producers = pick_row$median_n_prod, max_share = pick_row$max_share,
                 min_hhi = pick_row$min_hhi, max_hhi = pick_row$max_hhi,
                 margin_to_target = target - pick_row$median_hhi,
                 iters = pick_row$iters, converged = pick_row$converged,
                 score_change_pct_vs_lambda0 = pick_row$score_change_pct),
  below_pick = below %>% dplyr::select(lambda, median_hhi),
  above_pick = above %>% dplyr::select(lambda, median_hhi),
  grid = grid %>% dplyr::select(lambda, median_hhi, min_hhi, max_hhi, median_eff, median_n_prod, max_share,
                                slots_at_040, pct_at_005, milp_final, fallbacks_all_passes, converged, iters,
                                violations, eu_stages_selected, eu_topk, meets, linear_score_sum, hhi_mean,
                                score_change_pct, runtime_min),
  n_grid_points = nrow(grid), grid_range = I(range(grid$lambda)),
  fine_step_range = I(fine_range),
  all_runs_milp_22 = all(grid$milp_final == 22), all_runs_no_fallback = all(grid$fallbacks_all_passes == 0),
  runs_with_greedy_fallback = fallback_runs,
  max_lambda_all_milp_from_zero = max(grid$lambda[cumprod(grid$n_stages_milp == 22) == 1]),
  all_runs_converged = all(grid$converged), iters_range = I(range(grid$iters)),
  at_lambda0 = list(median_hhi = grid$median_hhi[grid$lambda == 0], median_n_producers = grid$median_n_prod[grid$lambda == 0],
                    slots_at_040 = grid$slots_at_040[grid$lambda == 0], producer_slots = grid$producer_slots[grid$lambda == 0]),
  at_lambda_max = list(lambda = max(grid$lambda), median_hhi = grid$median_hhi[which.max(grid$lambda)],
                       median_eff = grid$median_eff[which.max(grid$lambda)],
                       score_change_pct = grid$score_change_pct[which.max(grid$lambda)]),
  non_monotone_rises = rises, flat_steps = flats,
  n_rises = nrow(rises), n_steps = nrow(steps),
  recrossings_above_pick = nrow(recross),
  nocaps = nocaps %>% dplyr::select(lambda, median_hhi, median_eff, median_n_prod, max_share, milp_final,
                                    fallbacks_all_passes, linear_score_sum, score_change_pct),
  nocaps_at_pick_median_hhi = nocaps_pick$median_hhi,
  runtime_min_range = I(range(grid$runtime_min)),
  runtime_note = "runtime_min: wall-clock minutes per grid run with up to 12 runs in parallel on a shared workstation."
)
write_paper_results(results, "network_frontier")

# ---------------------------------------------------------------------------
# Figure
col_caps <- paper_cat[["blue"]]
col_rise <- paper_cat[["orange"]]
col_pick <- paper_ink$primary
base_theme <- theme_paper(base_size = 8.5, grid = "y") +
  theme(plot.title = element_text(size = 8.5, face = "bold", hjust = 0, margin = margin(b = 3)),
        legend.position = "none", plot.margin = margin(4, 8, 4, 4))
target_lab <- sprintf("Target %.4f", target)
fmt_l <- function(x) sub("\\.?0+$", "", formatC(x, format = "f", digits = 2))

# (a) whole grid
zl <- c(0.70, 0.90)
zoom <- zl + c(-0.005, 0.005)
p_a <- ggplot(grid, aes(x = lambda, y = median_hhi)) +
  annotate("rect", xmin = zoom[1], xmax = zoom[2], ymin = -Inf, ymax = Inf, fill = paper_ink$neutral) +
  geom_hline(yintercept = target, colour = paper_ink$secondary, linewidth = 0.4) +
  geom_line(colour = col_caps, linewidth = 0.5) +
  geom_point(colour = col_caps, size = 1.1) +
  geom_point(data = nocaps, shape = 21, fill = "white", colour = paper_ink$secondary, size = 1.4, stroke = 0.5) +
  geom_point(data = pick_row, shape = 21, fill = col_caps, colour = col_pick, size = 2.2, stroke = 0.7) +
  annotate("text", x = 1.5, y = target, label = target_lab, hjust = 1, vjust = -0.6, size = 2.4,
           colour = paper_ink$secondary) +
  annotate("text", x = 0.06, y = grid$median_hhi[grid$lambda == 0], label = "two producers at the\n0.40 cap per stage",
           hjust = 0, vjust = 0.5, size = 2.3, colour = paper_ink$secondary, lineheight = 0.9) +
  scale_x_continuous(breaks = seq(0, 1.5, 0.25), labels = c("0", "0.25", "0.5", "0.75", "1", "1.25", "1.5"),
                     expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(limits = c(0.09, 0.37), breaks = seq(0.1, 0.35, 0.05)) +
  labs(title = "(a) Median designed HHI", x = expression("Diversification weight " * lambda), y = "Median HHI over 22 stages") +
  base_theme

# (b) neighbourhood of the pick
gz <- grid %>% dplyr::filter(lambda >= zl[1], lambda <= zl[2])
rise_seg <- rises %>% dplyr::filter(lambda_from >= zl[1], lambda_to <= zl[2])
# label the first rise above the pick
rise_lab <- rise_seg %>% dplyr::filter(lambda_from >= pick) %>% dplyr::slice_min(lambda_from, n = 1)
nocaps_z <- nocaps %>% dplyr::filter(lambda >= zl[1], lambda <= zl[2])
p_b <- ggplot(gz, aes(x = lambda, y = median_hhi)) +
  geom_hline(yintercept = target, colour = paper_ink$secondary, linewidth = 0.4) +
  geom_line(colour = col_caps, linewidth = 0.5) +
  geom_segment(data = rise_seg, aes(x = lambda_from, xend = lambda_to, y = hhi_from, yend = hhi_to),
               colour = col_rise, linewidth = 1.1, inherit.aes = FALSE) +
  geom_point(aes(fill = meets), shape = 21, colour = col_caps, size = 1.5, stroke = 0.5) +
  scale_fill_manual(values = c(`TRUE` = col_caps, `FALSE` = "white")) +
  geom_point(data = nocaps_z, shape = 21, fill = "white", colour = paper_ink$secondary, size = 1.4, stroke = 0.5) +
  geom_point(data = pick_row, shape = 21, fill = col_caps, colour = col_pick, size = 2.4, stroke = 0.8) +
  annotate("text", x = pick, y = pick_row$median_hhi, label = sprintf("lambda == %s", fmt_l(pick)), parse = TRUE,
           hjust = 1.25, vjust = 1.4, size = 2.5, colour = col_pick) +
  annotate("text", x = zl[2], y = target, label = target_lab, hjust = 1, vjust = -0.6, size = 2.4,
           colour = paper_ink$secondary) +
  annotate("text", x = nocaps_z$lambda + 0.007, y = nocaps_z$median_hhi, label = "no caps", hjust = 0, vjust = 0.5,
           size = 2.3, colour = paper_ink$secondary) +
  annotate("text", x = rise_lab$lambda_to, y = max(rise_lab$hhi_to, rise_lab$hhi_from), label = "median rises",
           hjust = 0, vjust = -0.9, size = 2.3, colour = col_rise) +
  scale_x_continuous(breaks = seq(zl[1], zl[2], 0.05), labels = fmt_l,
                     expand = expansion(mult = c(0.03, 0.03))) +
  scale_y_continuous(breaks = seq(0.135, 0.17, 0.005), labels = function(x) sprintf("%.3f", x)) +
  labs(title = "(b) Around the pick", x = expression("Diversification weight " * lambda), y = NULL) +
  base_theme

# (c) linear score given up
p_c <- ggplot(grid, aes(x = lambda, y = score_change_pct)) +
  geom_hline(yintercept = 0, colour = paper_ink$axis, linewidth = 0.3) +
  geom_line(colour = col_caps, linewidth = 0.5) +
  geom_point(colour = col_caps, size = 1.1) +
  geom_point(data = nocaps, shape = 21, fill = "white", colour = paper_ink$secondary, size = 1.4, stroke = 0.5) +
  geom_point(data = pick_row, shape = 21, fill = col_caps, colour = col_pick, size = 2.2, stroke = 0.7) +
  annotate("text", x = pick, y = pick_row$score_change_pct,
           label = sprintf("%.1f%%", pick_row$score_change_pct), hjust = 1.3, vjust = 1.3, size = 2.4, colour = col_pick) +
  scale_x_continuous(breaks = seq(0, 1.5, 0.25), labels = c("0", "0.25", "0.5", "0.75", "1", "1.25", "1.5"),
                     expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "(c) Linear score vs. lambda = 0", x = expression("Diversification weight " * lambda),
       y = "Change in summed node + edge score") +
  base_theme

p_all <- cowplot::plot_grid(p_a, p_b, p_c, nrow = 1, rel_widths = c(1.05, 1, 1), align = "h", axis = "bt")
save_paper_fig(p_all, "network_frontier", width = paper_width_full, height = 2.7)
message(sprintf("Done: network_frontier.json and figures/network_frontier (pick %s, target %.4f, design %.4f).",
                format(pick), target, pick_row$median_hhi))

# ---------------------------------------------------------------------------
# Table: the calibration grid around the pick, with anchor points and the no-caps run
tab_pts <- grid %>%
  dplyr::filter(lambda %in% c(0, 0.5, 1, 1.5) | (lambda >= pick - 0.04 - 1e-9 & lambda <= pick + 0.04 + 1e-9))
f4 <- function(x) sprintf("%.4f", x)
row_of <- function(r, label = fmt_l(r$lambda), caps = "on") {
  sprintf("%s & %s & %s & %s & %s & %s & %d & %s \\\\", label, caps, f4(r$median_hhi), sprintf("%.2f", r$median_eff),
          sprintf("%.3f", r$max_share), sprintf("%+.4f", target - r$median_hhi), as.integer(r$milp_final),
          ifelse(r$median_hhi <= target + 1e-12, "yes", "no"))
}
grid_rows <- purrr::map_chr(seq_len(nrow(tab_pts)), function(i) {
  r <- tab_pts[i, ]
  lab <- if (isTRUE(all.equal(r$lambda, pick))) sprintf("\\textbf{%s}", fmt_l(r$lambda)) else fmt_l(r$lambda)
  row_of(r, lab)
})
nc_rows <- purrr::map_chr(seq_len(nrow(nocaps)), function(i) {
  r <- nocaps[i, ]
  r$milp_final <- as.integer(r$milp_final)
  row_of(r, fmt_l(r$lambda), "off")
})
frontier_tab <- c(
  "% Generated by scripts/paper/fig_network_frontier.R",
  "\\begin{table}[htbp]",
  "\\centering",
  "\\begin{threeparttable}",
  "\\caption{Calibration of the diversification weight: the grid around the pick}",
  "\\label{tab:network_frontier}",
  "\\footnotesize",
  "\\begin{tabular}{@{}l c r r r r r c@{}}",
  "\\toprule",
  "$\\lambda$ & Caps & Median $\\HHI^{\\mathrm{D}}$ & Eff.\\ producers & Max.\\ share & Target $-$ median & MILP stages & Meets \\\\",
  "\\midrule",
  grid_rows,
  "\\addlinespace",
  nc_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("\\item \\emph{Notes:} Production configuration of the network design (", n_nodes, " nodes, the 22 Upstream ",
         "and Midstream stages, node policy term off, MILP with a 300-second stage limit and the greedy fallback, ",
         "GDP-log portfolio caps) re-solved at each grid value of $\\lambda$. Median $\\HHI^{\\mathrm{D}}$: median over ",
         "the 22 stages of the Herfindahl index of designed production shares; Eff.\\ producers: median over ",
         "the stages of its inverse. Target: median over the ", nrow(obs_hhi), " stages with an HS6 basket of the nodes' observed 2024 ",
         "export Herfindahl index (the EU-27 node counting extra-EU exports only), ", f4(target), " (",
         sprintf("%.2f", 1 / target), " equivalent exporters). Meets: median $\\HHI^{\\mathrm{D}}\\le$ target; the rule ",
         "picks the smallest grid value that meets it (bold). The grid runs from 0 to 1.5 with a step of 0.01 between ",
         fmt_l(fine_range[1]), " and ", fmt_l(fine_range[2]), "; the table shows the points within 0.04 of the pick, ",
         "the anchors 0, 0.5, 1 and 1.5, and the run without portfolio caps at the pick. ",
         "Source: lambda study of the final inputs (2026-10-12)."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)
tab_dir <- paper_dir("tables")
if (!dir.exists(tab_dir)) dir.create(tab_dir, recursive = TRUE)
writeLines(frontier_tab, file.path(tab_dir, "network_frontier.tex"), useBytes = TRUE)
