# Figure, table and numbers: sensitivity of the Allied Industrial Base analysis (paper Section 11,
# subsec:robust_network; fig:aib_classes, tab:aib_variants, results/aib_robustness.json).
#
# Every variant is already in the outputs of scripts/32_build_aib_partnerships.R; nothing is re-solved
# here. Reads from <processed_dir>/outputs/aib/ (override OPSI_AIB_DIR) and the global-scope
# sensitivity run from <processed_dir>/outputs/aib_global/ (override OPSI_AIB_GLOBAL_DIR; demand scope
# and candidate universe both global):
#   aib_node_classification.csv     the class under the headline and every other variant
#   allied_trade_stress_test.csv    exposure, coverage and residual gap per shock, semantics, supply
#                                   constraint and scenario
#   allied_trade_growth_sensitivity.csv  state B under growth bounds 1.25x, 2x, 4x and unlimited
#   allied_trade_frontier.csv       maximum supplier share 25/40/60/100% and the true N-1 test
#   aib_compact_candidates.csv      compacts under each supply-constraint mode
# Headline: China removed, hold, production-constrained, reallocation, allied demand scope, global
# candidate partners; classes counted over the electro-industrial strategic scope (70 nodes).
#
# Writes paper/figures/aib_classes.pdf (+ png), paper/tables/aib_variants.tex and
# paper/results/aib_robustness.json.
#
# Run from the repository root (seconds):  Rscript scripts/paper/fig_aib_robustness.R

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
  library(readr)
})

root <- paper_repo_root()
config <- yaml::read_yaml(Sys.getenv("OPSI_CONFIG", file.path(root, "config", "config.yml")))
processed_dir <- file.path(root, config$processed_dir)
aib_dir <- Sys.getenv("OPSI_AIB_DIR", file.path(processed_dir, "outputs", "aib"))
glob_dir <- Sys.getenv("OPSI_AIB_GLOBAL_DIR", file.path(processed_dir, "outputs", "aib_global"))
rd <- function(d, f) read_csv(file.path(d, f), show_col_types = FALSE, guess_max = 1e5)
r3 <- function(x) round(x, 3)

nc <- rd(aib_dir, "aib_node_classification.csv")
st <- rd(aib_dir, "allied_trade_stress_test.csv")
gs <- rd(aib_dir, "allied_trade_growth_sensitivity.csv")
fr <- rd(aib_dir, "allied_trade_frontier.csv")
cc <- rd(aib_dir, "aib_compact_candidates.csv")
gnc <- rd(glob_dir, "aib_node_classification.csv")
gst <- rd(glob_dir, "allied_trade_stress_test.csv")
gcc <- rd(glob_dir, "aib_compact_candidates.csv")
stopifnot(all(gst$demand_scope == "global"), all(nc$demand_scope == "coalition"))

sc <- nc %>% filter(in_strategic_scope)
gsc <- gnc %>% filter(in_strategic_scope)

# ---------------------------------------------------------------------------
# Classes under every variant (strategic scope)
# ---------------------------------------------------------------------------
variants <- tribble(
  ~group, ~variant, ~cls,
  "Headline", "Headline: China removed, hold, production-constrained, reallocation", sc$node_classification,
  "Shock semantics", "Release (decoupling)", sc$class_release,
  "Physical constraint", "Trade-only", sc$class_trade_only,
  "Supply scenario", "Conservative", sc$class_conservative,
  "Shock", "China 50%", sc$class_china_50,
  "Shock", "Largest supplier, sub-sector", sc$class_largest_subsector,
  "Shock", "Largest supplier, stage", sc$class_largest_stage,
  "Shock", "Largest supplier, technology", sc$class_largest_technology,
  "Growth bound (state B)", "Existing links may grow 1.25x", sc$`class_growth_1.25x`,
  "Growth bound (state B)", "Existing links may grow 4x", sc$class_growth_4x,
  "Growth bound (state B)", "Existing links unbounded", sc$class_growth_unlimited,
  "Demand scope", "Global demand, hold", gsc$node_classification,
  "Demand scope", "Global demand, release", gsc$class_release
)
counts <- variants %>% rowwise() %>%
  reframe(group, variant, k = names(table(cls)), n = as.integer(table(cls))) %>% rename(cls = k) %>%
  mutate(variant = factor(variant, levels = rev(variants$variant)),
         cls = factor(cls, levels = c("BUILD", "COORDINATE", "MANAGE SPECIALIZATION", "LOW INTERVENTION", "INSUFFICIENT DATA")))
cls_cols <- c("BUILD" = paper_cat[["orange"]], "COORDINATE" = paper_cat[["blue"]],
              "MANAGE SPECIALIZATION" = paper_cat[["violet"]], "LOW INTERVENTION" = "#c3c2b7",
              "INSUFFICIENT DATA" = paper_ink$neutral)
p <- ggplot(counts %>% filter(cls != "INSUFFICIENT DATA"), aes(n, variant, fill = cls)) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.2) +
  geom_text(aes(label = ifelse(n >= 3, n, "")), position = position_stack(vjust = 0.5), size = 2.3,
            colour = ifelse(counts$cls[counts$cls != "INSUFFICIENT DATA"] %in% c("BUILD", "COORDINATE", "MANAGE SPECIALIZATION"),
                            "white", paper_ink$primary)) +
  scale_fill_manual(values = cls_cols, name = NULL, drop = TRUE) +
  scale_x_continuous("Nodes in the electro-industrial scope", expand = expansion(mult = c(0, 0.02))) +
  labs(y = NULL) + theme_paper(grid = "x") + guides(fill = guide_legend(nrow = 2))
save_paper_fig(p, "aib_classes", height = 3.6)

cnt <- function(v) as.list(table(v))
changed <- function(v) sc$sub_sector[v != sc$node_classification]
cls_by_variant <- setNames(lapply(variants$cls, cnt), variants$variant)

# Nodes whose class changes, by variant
release_to_low <- sc %>% filter(node_classification == "BUILD", class_release == "LOW INTERVENTION") %>% pull(sub_sector)
release_manage <- sc %>% filter(class_release == "MANAGE SPECIALIZATION") %>% pull(sub_sector)
to_changes <- sc %>% filter(class_trade_only != node_classification) %>%
  transmute(sub_sector, headline = node_classification, trade_only = class_trade_only,
            warning = classification_warning_trade_only)
coord_other <- sc %>% filter(class_largest_subsector == "COORDINATE" | class_largest_stage == "COORDINATE") %>%
  transmute(sub_sector, largest_subsector = class_largest_subsector, largest_stage = class_largest_stage)
growth_changes <- sum(sc$`class_growth_1.25x` != sc$node_classification | sc$class_growth_4x != sc$node_classification |
                        sc$class_growth_unlimited != sc$node_classification | sc$class_growth_2x != sc$node_classification)

# ---------------------------------------------------------------------------
# Technology residual gaps under the variants (table)
# ---------------------------------------------------------------------------
tech_var <- function(s, shock = "china_100", sem = "hold", con = "production_constrained", scen = "reallocation") {
  s %>% filter(level == "technology", shock_id == shock, shock_semantics == sem, supply_constraint == con, scenario == scen) %>%
    select(tech, shock_exposure, residual_gap, in_strategic_scope)
}
tv <- list(
  headline = tech_var(st), release = tech_var(st, sem = "release"), trade_only = tech_var(st, con = "trade_only"),
  conservative = tech_var(st, scen = "conservative"), china_50 = tech_var(st, shock = "china_50"),
  global = tech_var(gst), global_release = tech_var(gst, sem = "release"))
tt <- tv$headline %>% select(tech, in_strategic_scope, exposure = shock_exposure, headline = residual_gap)
for (k in setdiff(names(tv), "headline")) tt[[k]] <- tv[[k]]$residual_gap[match(tt$tech, tv[[k]]$tech)]
tt$global_exposure <- tv$global$shock_exposure[match(tt$tech, tv$global$tech)]
tt <- tt %>% arrange(desc(in_strategic_scope), desc(exposure))
pc <- function(x) sprintf("%.1f", 100 * pmax(x, 0))
bcount <- function(v) sum(v == "BUILD")
tab <- c(
  "% Generated by scripts/paper/fig_aib_robustness.R; do not edit by hand.",
  "\\begin{table}[tbp]", "\\centering", "\\small",
  "\\caption{Residual gap after optimized trade by technology under each variant (percent of the demand scope's",
  "traded demand), with the number of BUILD nodes in the electro-industrial scope. One setting changes at a time",
  "from the headline (China removed, hold, production-constrained, reallocation, allied demand scope). Global: every",
  "importer's demand is protected (demand scope and candidate partners global); its exposure differs from the",
  "headline's because the scope differs. Source: \\texttt{allied\\_trade\\_stress\\_test.csv},",
  "\\texttt{aib\\_node\\_classification.csv} (\\texttt{outputs/aib} and \\texttt{outputs/aib\\_global});",
  "\\texttt{scripts/paper/fig\\_aib\\_robustness.R}.}",
  "\\label{tab:aib_variants}",
  "\\begin{tabular}{@{}lrrrrrrrrr@{}}", "\\toprule",
  "& \\multicolumn{2}{c}{Headline} & Release & Trade- & Conser- & China & \\multicolumn{3}{c}{Global demand} \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(l){8-10}",
  "Technology & Exposure & Gap & & only & vative & 50\\% & Exposure & Gap & Release \\\\", "\\midrule",
  sprintf("%s & %s & %s & %s & %s & %s & %s & %s & %s & %s \\\\", tt$tech, pc(tt$exposure), pc(tt$headline), pc(tt$release),
          pc(tt$trade_only), pc(tt$conservative), pc(tt$china_50), pc(tt$global_exposure), pc(tt$global), pc(tt$global_release)),
  "\\midrule",
  sprintf("BUILD nodes (of %d) & & %d & %d & %d & %d & %d & & %d & %d \\\\", nrow(sc), bcount(sc$node_classification),
          bcount(sc$class_release), bcount(sc$class_trade_only), bcount(sc$class_conservative), bcount(sc$class_china_50),
          bcount(gsc$node_classification), bcount(gsc$class_release)),
  "\\bottomrule", "\\end{tabular}", "\\end{table}")
first_fossil <- which(grepl("^(Coal|Oil|Gas) &", tab))[1]
tab <- append(tab, "\\addlinespace", after = first_fossil - 1)
writeLines(tab, paper_dir("tables", "aib_variants.tex"))

# ---------------------------------------------------------------------------
# Growth bound, N-1 and the frontier
# ---------------------------------------------------------------------------
gr <- gs %>% filter(scenario == "reallocation") %>% group_by(existing_growth_label) %>%
  summarise(existing_coverage_mean = r3(mean(existing_coverage, na.rm = TRUE)),
            dividend_median_pt = round(100 * median(coordination_dividend, na.rm = TRUE), 3),
            dividend_max_pt = round(100 * max(coordination_dividend, na.rm = TRUE), 2),
            max_node = node_label[which.max(coordination_dividend)],
            at_least_5pt = sum(coordination_dividend >= 0.05, na.rm = TRUE), .groups = "drop")
fh <- fr %>% filter(level == "subsector", shock_id == "china_100", scenario == "reallocation", shock_semantics == "hold",
                    supply_constraint == "production_constrained")
f1 <- fh %>% filter(max_supplier_share == 1)
fl <- fh %>% group_by(max_supplier_share) %>%
  summarise(coverage_median = r3(median(optimized_coverage, na.rm = TRUE)),
            n1_reoptimized_median = r3(median(n1_reoptimized_coverage, na.rm = TRUE)),
            n1_instantaneous_median = r3(median(instantaneous_n1_retained_coverage, na.rm = TRUE)),
            n1_at_least_80 = sum(n1_reoptimized_coverage >= 0.8, na.rm = TRUE),
            largest_share_median = r3(median(optimized_largest_share, na.rm = TRUE)), .groups = "drop")
frontier <- list(
  n_nodes = nrow(f1),
  min_required_diversification = as.list(table(ifelse(is.na(f1$min_required_diversification), "none", f1$min_required_diversification))),
  tightest_costless = as.list(table(f1$tightest_costless_max_share, useNA = "ifany")),
  by_limit = lapply(split(fl, fl$max_supplier_share), function(r) as.list(r[, -1])),
  n1_drop_median_pt = round(100 * median(f1$optimized_coverage - f1$n1_reoptimized_coverage, na.rm = TRUE), 1))

# Compacts under trade-only and global demand
cc_pc <- cc %>% filter(supply_constraint == "production_constrained")
cc_to <- cc %>% filter(supply_constraint == "trade_only")
same_nodes <- sum(cc_to$included_subsectors %in% cc_pc$included_subsectors)
gcc_pc <- gcc %>% filter(supply_constraint == "production_constrained")

res <- list(
  generated = format(Sys.time(), "%Y-%m-%d %H:%M"),
  n_scope = nrow(sc),
  classes = cls_by_variant,
  release = list(build_to_low = release_to_low, n_build_to_low = length(release_to_low), manage = release_manage),
  trade_only = list(changes = to_changes, warnings = sc$sub_sector[!is.na(sc$classification_warning_trade_only)]),
  conservative_extra_build = changed(sc$class_conservative),
  china_50 = list(changes = length(changed(sc$class_china_50))),
  coordinate_other_shocks = coord_other,
  growth = list(class_changes = growth_changes, by_bound = lapply(split(gr, gr$existing_growth_label), function(r) as.list(r[, -1])),
                sensitive = sum(nc$coordinate_sensitive_to_growth, na.rm = TRUE)),
  frontier = frontier,
  technology = lapply(split(tt, tt$tech), function(r) lapply(as.list(r[, -1]), function(x) if (is.numeric(x)) r3(x) else x)),
  global = list(classes = cnt(gsc$node_classification), release = cnt(gsc$class_release),
                compacts_pc = nrow(gcc_pc), compacts_types = as.list(table(gcc_pc$compact_type))),
  compacts = list(production_constrained = nrow(cc_pc), trade_only = nrow(cc_to), same_node_sets = same_nodes)
)
write_paper_results(res, "aib_robustness")
message("fig_aib_robustness: wrote aib_classes, tables/aib_variants.tex and results/aib_robustness.json")
