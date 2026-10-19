# Figures, tables and numbers: the Allied Industrial Base (paper Section 10, sec:network; Appendix C,
# app:algorithm; Online Appendix oa:network).
#
# Reads the outputs of scripts/31_build_aib_trade_network.R and scripts/32_build_aib_partnerships.R
# (the run after the AIB credibility pass, commit ba38e2b) from <processed_dir>/outputs/aib/
# (override with the environment variable OPSI_AIB_DIR):
#   allied_trade_stress_test.csv, allied_investment_partnerships.csv, allied_investment_pairs.csv,
#   emergent_aib_edges.csv, emergent_aib_country_summary.csv, aib_node_classification.csv,
#   aib_compact_candidates.csv, aib_compact_members.csv, aib_compact_nodes.csv,
#   allied_subsector_data_quality.csv
# and data/processed/coalition_tbl.rds (tiers, for colouring economies).
#
# Headline variant throughout (docs/aib_methodology.md, 5 and 14): China removed (china_100), hold
# semantics, production-constrained supply, reallocation scenario, allied demand scope (core and
# partner tiers), global candidate partners, electro-industrial strategic scope for compacts and
# network edges. The investment step is the headline increment (10% of the node's demand-scope
# demand) at the first-ranked host.
#
# Builds:
#   aib_ladder     coverage of the allied scope's traded demand by technology: shock (state A),
#                  existing relationships (B), optimized trade (C), and C plus one 10% increment at
#                  the leading host of every node with a residual gap of 5% or more
#   aib_nodes      per node: shock exposure against the residual gap and against the coordination
#                  dividend, coloured by class
#   aib_flows      investor -> host flows: supply from the new capacity at the leading hosts
#   aib_roles      economies' roles in the emergent network (trade supplier, trade buyer,
#                  investment host, investor), kept apart
#   aib_compacts   the 22 Sectoral Compacts: coverage ladder of the compact's nodes and the coalition
#                  tiers
#   tables/aib_compacts.tex    the compacts, their minimum coalitions and tiers
#   tables/aib_validation.tex  the four validation chains, production-constrained and trade-only
# and writes every number the text quotes to paper/results/aib.json.
#
# Run from the repository root (about a minute):  Rscript scripts/paper/fig_aib.R

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
  library(stringr)
  library(ggrepel)
  library(cowplot)
})

root <- paper_repo_root()
config <- yaml::read_yaml(Sys.getenv("OPSI_CONFIG", file.path(root, "config", "config.yml")))
processed_dir <- file.path(root, config$processed_dir)
aib_dir <- Sys.getenv("OPSI_AIB_DIR", file.path(processed_dir, "outputs", "aib"))
rd <- function(f) read_csv(file.path(aib_dir, f), show_col_types = FALSE, guess_max = 1e5)

HL <- list(shock = "china_100", scenario = "reallocation", semantics = "hold",
           constraint = "production_constrained", increment = 0.10)
hl <- function(x) {
  if ("shock_id" %in% names(x)) x <- x[x$shock_id == HL$shock, , drop = FALSE]
  if ("scenario" %in% names(x)) x <- x[x$scenario == HL$scenario, , drop = FALSE]
  if ("shock_semantics" %in% names(x)) x <- x[x$shock_semantics == HL$semantics, , drop = FALSE]
  if ("supply_constraint" %in% names(x)) x <- x[x$supply_constraint == HL$constraint, , drop = FALSE]
  x
}
r3 <- function(x) round(x, 3)
split_codes <- function(x) if (is.na(x) || !nzchar(x)) character() else strsplit(x, " ")[[1]]
tex_esc <- function(x) gsub("&", "\\\\&", x)

st <- rd("allied_trade_stress_test.csv")
nc <- rd("aib_node_classification.csv")
ipp <- rd("allied_investment_partnerships.csv")
ip <- rd("allied_investment_pairs.csv")
cs <- rd("emergent_aib_country_summary.csv")
ed <- rd("emergent_aib_edges.csv")
cc <- rd("aib_compact_candidates.csv")
cn <- rd("aib_compact_nodes.csv")
dq <- rd("allied_subsector_data_quality.csv")
coal <- readRDS(file.path(processed_dir, "coalition_tbl.rds"))
tier_of <- function(iso) {
  t <- coal$tier[match(iso, coal$iso3)]
  t[is.na(t) | t == "none"] <- "outside"
  t
}
stopifnot(all(nc$supply_constraint == HL$constraint))

# ---------------------------------------------------------------------------
# Nodes, scope and data quality
# ---------------------------------------------------------------------------
n_nodes <- nrow(nc)
quality <- as.list(table(nc$data_quality))
n_opt <- sum(nc$node_classification != "INSUFFICIENT DATA")
n_scope <- sum(nc$in_strategic_scope)
n_phys <- sum(nc$physical_data == "complete")
phys_nodes <- nc$sub_sector[nc$physical_data == "complete"]
shared <- nc %>% filter(str_detect(techs, ";")) %>% pull(sub_sector)
n_core <- sum(coal$tier == "core"); n_partner <- sum(coal$tier == "partner")

# Largest world exporter by technology stage (2024 node exports summed over the stage's nodes; a shared
# node counts in each of its technologies), for the nine headline technologies with a trade basket.
inputs <- readRDS(file.path(aib_dir, "aib_inputs.rds"))
stage_top <- inputs$exporter_metrics %>%
  inner_join(inputs$node_cells, by = "node_id", relationship = "many-to-many") %>%
  group_by(tech, supply_chain, exporter_iso) %>% summarise(v = sum(exports, na.rm = TRUE), .groups = "drop") %>%
  group_by(tech, supply_chain) %>% mutate(share = v / sum(v)) %>% slice_max(share, n = 1, with_ties = FALSE) %>%
  ungroup() %>% filter(tech != "Semiconductors")
rm(inputs)
china_top <- stage_top %>% filter(exporter_iso == "CHN")

st_h <- hl(st)
sub <- st_h %>% filter(level == "subsector")
cand <- sub %>% distinct(node_id, n_candidates)

# ---------------------------------------------------------------------------
# Coverage ladder by technology
# ---------------------------------------------------------------------------
inv_h <- ipp %>% filter(shock_id == HL$shock, scenario == HL$scenario, supply_constraint == HL$constraint,
                        increment == HL$increment)
inv1 <- inv_h %>% filter(host_rank == 1) %>%
  group_by(node_id) %>% slice_max(coverage_after - coverage_before, n = 1, with_ties = FALSE) %>% ungroup() %>%
  select(node_id, host_iso, coverage_before, coverage_after, marginal_resilience_gain, residual_gap_before,
         residual_gap_after, additional_demand_served_usd)
chk <- sub %>% inner_join(inv1, by = "node_id")
stopifnot(max(abs(chk$coverage_before - chk$optimized_coverage)) < 1e-6)

sub_inv <- sub %>% left_join(inv1 %>% select(node_id, coverage_after), by = "node_id") %>%
  mutate(investment_coverage = coalesce(coverage_after, optimized_coverage))
tech_rows <- st_h %>% filter(level == "technology") %>%
  select(tech, demand_usd, lost_usd, shock_exposure, static_coverage, existing_coverage, optimized_coverage,
         coordination_dividend, residual_gap, in_strategic_scope)
tech_inv <- sub_inv %>% group_by(tech) %>%
  summarise(optimized_check = sum(demand_usd * optimized_coverage) / sum(demand_usd),
            investment_coverage = sum(demand_usd * investment_coverage) / sum(demand_usd),
            n_nodes = n_distinct(node_id), n_build_sim = sum(!is.na(coverage_after)), .groups = "drop")
tech_rows <- tech_rows %>% left_join(tech_inv, by = "tech")
stopifnot(max(abs(tech_rows$optimized_check - tech_rows$optimized_coverage)) < 1e-6)
tech_rows <- tech_rows %>% arrange(desc(in_strategic_scope), desc(shock_exposure))

# The allied scope as a whole, each physical node once (the run with the larger exposure where a
# shared node has two).
scope_total <- function(rows) {
  n <- rows %>% group_by(node_id) %>% slice_max(shock_exposure, n = 1, with_ties = FALSE) %>% ungroup()
  D <- sum(n$demand_usd)
  list(n_nodes = nrow(n), demand_usd_bn = r3(D / 1e9), lost_usd_bn = r3(sum(n$lost_usd) / 1e9),
       static = r3(1 - sum(n$lost_usd) / D),
       existing = r3(sum(n$demand_usd * n$existing_coverage) / D),
       optimized = r3(sum(n$demand_usd * n$optimized_coverage) / D),
       investment = r3(sum(n$demand_usd * n$investment_coverage) / D),
       diversion_usd_bn = r3(sum(n$demand_usd * coalesce(n$optimized_diversion, 0)) / 1e9),
       recovery_usd_bn = r3(sum(n$demand_usd * (n$optimized_coverage - (1 - n$lost_usd / n$demand_usd))) / 1e9),
       new_links = sum(n$n_new_links, na.rm = TRUE), expanded_links = sum(n$n_expanded_links, na.rm = TRUE))
}
tot_all <- scope_total(sub_inv %>% filter(!is.na(optimized_coverage)))
tot_scope <- scope_total(sub_inv %>% filter(!is.na(optimized_coverage), in_strategic_scope))

lad <- tech_rows %>%
  transmute(tech, scope = ifelse(in_strategic_scope, "Electro-industrial (strategic scope)", "Coal, oil and gas"),
            `Shock (A)` = static_coverage, `Existing relationships (B)` = existing_coverage,
            `Optimized trade (C)` = optimized_coverage, `C + 10% capacity increment` = investment_coverage) %>%
  pivot_longer(-c(tech, scope), names_to = "state", values_to = "coverage") %>%
  mutate(state = factor(state, levels = c("Shock (A)", "Existing relationships (B)", "Optimized trade (C)",
                                          "C + 10% capacity increment")),
         tech = factor(tech, levels = rev(tech_rows$tech)),
         scope = factor(scope, levels = c("Electro-industrial (strategic scope)", "Coal, oil and gas")))
state_cols <- c("Shock (A)" = paper_ink$muted, "Existing relationships (B)" = paper_cat[["blue"]],
                "Optimized trade (C)" = paper_cat[["orange"]], "C + 10% capacity increment" = paper_cat[["aqua"]])
p_lad <- ggplot(lad, aes(coverage * 100, tech)) +
  geom_line(aes(group = tech), colour = paper_ink$axis, linewidth = 0.5) +
  geom_point(aes(colour = state, shape = state), size = 2.1, stroke = 0.7) +
  scale_colour_manual(values = state_cols, name = NULL) +
  scale_shape_manual(values = c(1, 16, 17, 15), name = NULL) +
  facet_grid(scope ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous("Share of the allied scope's 2024 traded demand served (%)", limits = c(50, 100),
                     breaks = seq(50, 100, 10)) +
  labs(y = NULL) + theme_paper(grid = "x") +
  theme(strip.text.y = element_text(angle = 0, hjust = 0), legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 1))
save_paper_fig(p_lad, "aib_ladder", height = 3.4)

# ---------------------------------------------------------------------------
# Node classes, exposure, residual gap and the coordination dividend
# ---------------------------------------------------------------------------
cls_cols <- c("BUILD" = paper_cat[["orange"]], "COORDINATE" = paper_cat[["blue"]],
              "MANAGE SPECIALIZATION" = paper_cat[["violet"]], "LOW INTERVENTION" = paper_ink$muted)
nd <- nc %>% filter(node_classification != "INSUFFICIENT DATA") %>%
  mutate(cls = factor(node_classification, levels = names(cls_cols)),
         scope = ifelse(in_strategic_scope, "Strategic scope", "Coal, oil and gas"))
lab_ids <- c("permanent-magnets-and-magnet-parts", "rare-earth-compounds", "anode-materials", "wafers",
             "polysilicon", "lithium-chemicals", "wind-towers", "battery-cells", "natural-graphite")
nd <- nd %>% mutate(lab = sub(" [+] .*", "", sub_sector))
nd_lab <- nd %>% filter(node_id %in% lab_ids)
base_nodes <- function(p) {
  p + geom_point(aes(colour = cls, shape = scope), size = 1.7, stroke = 0.6, alpha = 0.9) +
    scale_colour_manual(values = cls_cols, name = NULL, drop = TRUE) +
    scale_shape_manual(values = c("Strategic scope" = 16, "Coal, oil and gas" = 1), name = NULL) +
    theme_paper(grid = "xy") + theme(legend.box = "horizontal")
}
pa <- base_nodes(ggplot(nd, aes(shock_exposure * 100, residual_gap * 100))) +
  geom_abline(slope = 1, intercept = 0, colour = paper_ink$axis, linetype = "dashed", linewidth = 0.3) +
  geom_hline(yintercept = 10, colour = paper_ink$axis, linewidth = 0.3) +
  geom_text_repel(data = nd_lab, aes(label = lab), size = 2.2, colour = paper_ink$secondary,
                  min.segment.length = 0, segment.colour = paper_ink$axis, seed = 1, max.overlaps = 30) +
  labs(x = "Shock exposure (% of demand lost)", y = "Residual gap after optimized trade (%)",
       title = "(a) Exposure and the residual gap")
pb <- base_nodes(ggplot(nd, aes(shock_exposure * 100, coordination_dividend * 100))) +
  geom_hline(yintercept = 5, colour = paper_ink$axis, linetype = "dashed", linewidth = 0.3) +
  geom_text_repel(data = nd %>% filter(coordination_dividend >= 0.015), aes(label = lab), size = 2.2,
                  colour = paper_ink$secondary, min.segment.length = 0, segment.colour = paper_ink$axis, seed = 2) +
  labs(x = "Shock exposure (% of demand lost)", y = "Coordination dividend (points)",
       title = "(b) Exposure and the coordination dividend")
leg <- cowplot::get_plot_component(pa + theme(legend.position = "bottom"), "guide-box-bottom", return_all = TRUE)
p_nodes <- cowplot::plot_grid(
  cowplot::plot_grid(pa + theme(legend.position = "none"), pb + theme(legend.position = "none"), nrow = 1),
  leg, ncol = 1, rel_heights = c(1, 0.09))
save_paper_fig(p_nodes, "aib_nodes", height = 3.6)

cls_all <- as.list(table(nc$node_classification))
cls_scope <- as.list(table(nc$node_classification[nc$in_strategic_scope]))
div <- nd$coordination_dividend
top_div <- nd %>% arrange(desc(coordination_dividend)) %>% slice(1)
needs <- nc %>% filter(policy_needs != "none", policy_needs != node_classification) %>%
  transmute(sub_sector, node_classification, policy_needs)
warn <- nc %>% filter(!is.na(classification_warning)) %>% transmute(sub_sector, node_classification, shock_exposure = r3(shock_exposure))
warn_trade_only <- nc %>% filter(!is.na(classification_warning_trade_only)) %>% pull(sub_sector)
build_scope <- nd %>% filter(in_strategic_scope, node_classification == "BUILD")

# ---------------------------------------------------------------------------
# Marginal investment: hosts and investor -> host flows
# ---------------------------------------------------------------------------
inv_scope <- inv_h %>% filter(in_strategic_scope)
hosts_per_node <- inv_scope %>% group_by(node_id) %>% summarise(n = n_distinct(host_iso), .groups = "drop")
lead <- inv_scope %>% filter(host_rank == 1) %>% distinct(node_id, host_iso, capability_basis, marginal_resilience_gain,
                                                          marginal_resilience_gain_diversified, residual_gap_before,
                                                          residual_gap_after)
lead_count <- lead %>% count(host_iso, sort = TRUE)
pairs <- ip %>% filter(shock_id == HL$shock, scenario == HL$scenario, supply_constraint == HL$constraint,
                       host_rank == 1, increment == HL$increment) %>%
  left_join(nc %>% select(node_id, in_strategic_scope), by = "node_id") %>% filter(in_strategic_scope)
pairs_x <- pairs %>% filter(relationship != "domestic")
flows <- pairs_x %>% group_by(investor_iso, host_iso) %>%
  summarise(usd = sum(supply_from_new_capacity_usd, na.rm = TRUE), n_nodes = n_distinct(node_id),
            friendshore = mean(friendshore, na.rm = TRUE), .groups = "drop") %>% arrange(desc(usd))
host_tot <- pairs_x %>% group_by(host_iso) %>%
  summarise(usd = sum(supply_from_new_capacity_usd, na.rm = TRUE), n_nodes = n_distinct(node_id),
            n_investors = n_distinct(investor_iso), .groups = "drop") %>% arrange(desc(usd))
inv_tot <- pairs_x %>% group_by(investor_iso) %>%
  summarise(usd = sum(supply_from_new_capacity_usd, na.rm = TRUE), n_nodes = n_distinct(node_id),
            n_hosts = n_distinct(host_iso), .groups = "drop") %>% arrange(desc(usd))
fs_w <- with(pairs_x[!is.na(pairs_x$friendshore), ], sum(friendshore * supply_from_new_capacity_usd) / sum(supply_from_new_capacity_usd))
usd_x <- sum(pairs_x$supply_from_new_capacity_usd, na.rm = TRUE)
usd_dom <- sum(pairs$supply_from_new_capacity_usd[pairs$relationship == "domestic"], na.rm = TRUE)
host_tier_share <- pairs_x %>% mutate(t = tier_of(host_iso)) %>% group_by(t) %>%
  summarise(s = sum(supply_from_new_capacity_usd, na.rm = TRUE) / usd_x, .groups = "drop")

top_h <- head(host_tot$host_iso, 10)
top_i <- head(inv_tot$investor_iso, 12)
fm <- flows %>% filter(host_iso %in% top_h, investor_iso %in% top_i) %>%
  mutate(host_iso = factor(host_iso, levels = rev(top_h)), investor_iso = factor(investor_iso, levels = top_i))
share_shown <- sum(fm$usd) / usd_x
p_flows <- ggplot(fm, aes(investor_iso, host_iso, fill = usd / 1e9)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = ifelse(usd >= 1e9, sprintf("%.1f", usd / 1e9), "")), size = 2.3,
            colour = ifelse(fm$usd >= 4e9, "white", paper_ink$primary)) +
  scale_fill_gradientn(colours = paper_seq_blue, name = "Supply from the new capacity (USD bn)",
                       trans = "sqrt", breaks = c(0.5, 2, 5, 10)) +
  labs(x = "Investor (buyer whose lost supply the new capacity serves)", y = "Host") +
  theme_paper(grid = "none") + theme(legend.key.width = unit(1.2, "cm"))
save_paper_fig(p_flows, "aib_flows", height = 3.5)

# ---------------------------------------------------------------------------
# Country roles in the emergent network
# ---------------------------------------------------------------------------
cs_h <- cs %>% filter(scenario == HL$scenario)
ed_h <- ed %>% filter(scenario == HL$scenario)
roles <- cs_h %>% select(iso3, country, supplier_headline_volume_usd, buyer_headline_volume_usd,
                         supplier_n_partners, buyer_n_partners, investment_host_n_partners, investor_n_partners) %>%
  left_join(host_tot %>% select(iso3 = host_iso, host_usd = usd), by = "iso3") %>%
  left_join(inv_tot %>% select(iso3 = investor_iso, investor_usd = usd), by = "iso3") %>%
  mutate(across(c(host_usd, investor_usd, supplier_headline_volume_usd, buyer_headline_volume_usd), ~ coalesce(.x, 0)),
         total = supplier_headline_volume_usd + buyer_headline_volume_usd + host_usd + investor_usd,
         tier = tier_of(iso3)) %>% arrange(desc(total))
top_r <- head(roles, 24)
rl <- top_r %>% select(iso3, tier, `Trade supplier` = supplier_headline_volume_usd, `Trade buyer` = buyer_headline_volume_usd,
                       `Investment host` = host_usd, `Investor` = investor_usd) %>%
  pivot_longer(-c(iso3, tier), names_to = "role", values_to = "usd") %>%
  mutate(role = factor(role, levels = c("Trade supplier", "Trade buyer", "Investment host", "Investor")),
         iso3 = factor(iso3, levels = rev(top_r$iso3)),
         tier = factor(tier, levels = c("core", "partner", "outside"),
                       labels = c("Core member", "Partner", "Outside the coalition"))) %>% filter(usd > 0)
p_roles <- ggplot(rl, aes(role, iso3)) +
  geom_point(aes(size = usd / 1e9, colour = tier), alpha = 0.85) +
  scale_size_area(max_size = 7, name = "USD bn (headline)", breaks = c(1, 5, 10, 20)) +
  scale_colour_manual(values = c("Core member" = paper_cat[["blue"]], "Partner" = paper_cat[["aqua"]],
                                 "Outside the coalition" = paper_cat[["orange"]]), name = NULL, drop = FALSE) +
  scale_x_discrete(position = "top") + labs(x = NULL, y = NULL) +
  theme_paper(grid = "xy") + theme(legend.box = "vertical")
save_paper_fig(p_roles, "aib_roles", width = 4.6, height = 5.2)

role_counts <- list(
  supplier = sum(cs_h$supplier_n_partners > 0), buyer = sum(cs_h$buyer_n_partners > 0),
  investment_host = sum(cs_h$investment_host_n_partners > 0), investor = sum(cs_h$investor_n_partners > 0),
  all_four = sum(cs_h$supplier_n_partners > 0 & cs_h$buyer_n_partners > 0 & cs_h$investment_host_n_partners > 0 &
                   cs_h$investor_n_partners > 0),
  supplier_only = sum(cs_h$roles == "supplier", na.rm = TRUE),
  supplier_only_outside = sum(cs_h$roles == "supplier" & tier_of(cs_h$iso3) == "outside", na.rm = TRUE))

# ---------------------------------------------------------------------------
# Sectoral Compacts
# ---------------------------------------------------------------------------
cc_h <- cc %>% filter(supply_constraint == HL$constraint)
short_name <- function(s) {
  p <- sub(" [+] .*", "", strsplit(s, "; ")[[1]])
  if (length(p) <= 2) paste(p, collapse = " + ") else paste0(paste(p[1:2], collapse = " + "), " +", length(p) - 2)
}
cc_h <- cc_h %>% rowwise() %>%
  mutate(short = short_name(included_subsectors),
         n_members = length(split_codes(minimum_coalition)),
         n_critical = length(split_codes(critical_supply_partners)),
         n_associate = length(split_codes(associate_partners)),
         n_crit_ext = length(split_codes(critical_external_dependencies)),
         same_as_greedy = setequal(split_codes(minimum_coalition), split_codes(greedy_sufficient_coalition)),
         same_size_greedy = length(split_codes(greedy_sufficient_coalition)) == n_members) %>% ungroup() %>%
  arrange(desc(lost_usd)) %>% mutate(label = paste0(sub("AIB-", "", compact_id), " ", short))
cl <- cc_h %>% select(label, `Shock (A)` = coverage_static, `Existing relationships (B)` = coverage_without_coordination,
                      `Optimized trade (C)` = coverage_with_trade_coordination,
                      `C + 10% capacity increment` = coverage_after_investment_simulation) %>%
  pivot_longer(-label, names_to = "state", values_to = "coverage") %>%
  mutate(state = factor(state, levels = names(state_cols)), label = factor(label, levels = rev(cc_h$label)))
p_ca <- ggplot(cl, aes(coverage * 100, label)) +
  geom_line(aes(group = label), colour = paper_ink$axis, linewidth = 0.5) +
  geom_point(aes(colour = state, shape = state), size = 1.8, stroke = 0.6) +
  scale_colour_manual(values = state_cols, name = NULL,
                      labels = c("Shock", "Existing relationships", "Trade compact", "+ investment")) +
  scale_shape_manual(values = c(1, 16, 17, 15), name = NULL,
                     labels = c("Shock", "Existing relationships", "Trade compact", "+ investment")) +
  scale_x_continuous("Coverage (%)", limits = c(40, 100)) + labs(y = NULL,
  title = "(a) Coverage") + theme_paper(grid = "x") +
  theme(legend.position = "bottom", axis.text.y = element_text(size = 6.5)) +
  guides(colour = guide_legend(nrow = 1), shape = guide_legend(nrow = 1))
tiers <- cc_h %>% transmute(label, `Core member` = n_members - n_critical, `Critical supply partner` = n_critical,
                            `Associate partner` = n_associate) %>%
  pivot_longer(-label, names_to = "tier", values_to = "n") %>%
  mutate(label = factor(label, levels = rev(cc_h$label)),
         tier = factor(tier, levels = rev(c("Core member", "Critical supply partner", "Associate partner"))))
ext <- cc_h %>% transmute(label = factor(label, levels = rev(cc_h$label)), x = n_members + n_associate,
                          txt = ifelse(n_crit_ext > 0, paste0("+", n_crit_ext, " ext."), ""))
p_cb <- ggplot(tiers, aes(n, label, fill = tier)) + geom_col(width = 0.7) +
  geom_text(data = ext, aes(x = x + 0.3, y = label, label = txt), inherit.aes = FALSE, hjust = 0, size = 2.1,
            colour = paper_cat[["red"]]) +
  scale_fill_manual(values = c("Core member" = paper_cat[["blue"]], "Critical supply partner" = paper_cat[["orange"]],
                               "Associate partner" = paper_seq_blue[2]), name = NULL, breaks = c("Core member", "Critical supply partner", "Associate partner")) +
  scale_x_continuous("Countries in the coalition", limits = c(0, 12), breaks = seq(0, 12, 3)) +
  labs(y = NULL, title = "(b) Coalition tiers") + theme_paper(grid = "x") +
  theme(axis.text.y = element_blank(), legend.position = "bottom") + guides(fill = guide_legend(nrow = 1))
leg_a <- cowplot::get_plot_component(p_ca, "guide-box-bottom", return_all = TRUE)
leg_b <- cowplot::get_plot_component(p_cb, "guide-box-bottom", return_all = TRUE)
p_comp <- cowplot::plot_grid(
  cowplot::plot_grid(p_ca + theme(legend.position = "none"), p_cb + theme(legend.position = "none"),
                     nrow = 1, rel_widths = c(2.1, 1), align = "h", axis = "tb"),
  cowplot::plot_grid(leg_a, leg_b, ncol = 1), ncol = 1, rel_heights = c(1, 0.1))
save_paper_fig(p_comp, "aib_compacts", height = 4.6)

member_freq <- cc_h %>% rowwise() %>% reframe(iso3 = split_codes(minimum_coalition)) %>% count(iso3, sort = TRUE)
comp_needs <- table(unlist(strsplit(cc_h$key_policy_needs, "; ")))

# Compact table
tab_rows <- cc_h %>% mutate(
  id = sub("AIB-", "", compact_id),
  subs = tex_esc(gsub("; ", ", ", included_subsectors)),
  members = vapply(minimum_coalition, function(m) {
    v <- split_codes(m); crit <- split_codes(critical_supply_partners[match(m, minimum_coalition)])
    paste(ifelse(v %in% crit, paste0("\\textbf{", v, "}"), v), collapse = " ")
  }, character(1)),
  assoc = ifelse(is.na(associate_partners), "", associate_partners),
  ext = ifelse(is.na(critical_external_dependencies), "", critical_external_dependencies),
  cov = sprintf("%.0f/%.0f/%.0f/%.0f", 100 * coverage_static, 100 * coverage_without_coordination,
                100 * coverage_with_trade_coordination, 100 * coverage_after_investment_simulation),
  lost = sprintf("%.1f", lost_usd / 1e9))
tab_lines <- c(
  "% Generated by scripts/paper/fig_aib.R; do not edit by hand.",
  "\\begin{table}[tbp]", "\\centering", "\\scriptsize",
  "\\caption{The Sectoral Compacts of the headline run. Each row is a candidate compact: connected nodes of the",
  "electro-industrial chains that share partners and policy needs. Coalition: the exact minimum coalition at 95\\% of",
  "the demand anchors' traded demand in every node, critical supply partners (no single other economy can replace",
  "them) in bold; associates: the fewest additions reaching 99\\%; critical external: non-members whose loss pushes a",
  "node below 95\\% and that supply at least 1\\% of the anchors' demand. Coverage of the nodes' allied demand",
  "(\\%): shock / existing relationships / trade compact / trade compact plus one 10\\% increment at up to two member",
  "hosts per BUILD node. Lost: allied demand lost to the shock, USD bn. China removed, hold, production-constrained,",
  "reallocation. Source: \\texttt{aib\\_compact\\_candidates.csv}; \\texttt{scripts/paper/fig\\_aib.R}.}",
  "\\label{tab:aib_compacts}",
  "\\setlength{\\tabcolsep}{3pt}",
  "\\begin{tabularx}{\\textwidth}{@{}l>{\\raggedright\\arraybackslash}Xr>{\\raggedright\\arraybackslash}p{3.1cm}l>{\\raggedright\\arraybackslash}p{2.1cm}l@{}}",
  "\\toprule",
  "& Nodes & Lost & Coalition (95\\%) & Assoc. & Critical external & Coverage \\\\",
  "\\midrule",
  sprintf("%s & %s & %s & %s & %s & %s & %s \\\\", tab_rows$id, tab_rows$subs, tab_rows$lost, tab_rows$members,
          tab_rows$assoc, tab_rows$ext, tab_rows$cov),
  "\\bottomrule", "\\end{tabularx}", "\\end{table}")
writeLines(tab_lines, paper_dir("tables", "aib_compacts.tex"))

# ---------------------------------------------------------------------------
# Validation chains (Online Appendix)
# ---------------------------------------------------------------------------
val_ids <- c("permanent-magnets-and-magnet-parts", "rare-earth-compounds", "anode-materials", "wafers")
cc_all <- cc %>% select(compact_id, supply_constraint, minimum_coalition, critical_supply_partners)
val_row <- function(id, mode) {
  s <- st %>% filter(level == "subsector", node_id == id, shock_id == HL$shock, scenario == HL$scenario,
                     shock_semantics == HL$semantics, supply_constraint == mode) %>% slice_max(shock_exposure, n = 1, with_ties = FALSE)
  k <- nc %>% filter(node_id == id)
  cls <- if (mode == HL$constraint) k$node_classification else k$class_trade_only
  w <- if (mode == HL$constraint) k$classification_warning else k$classification_warning_trade_only
  comp <- cn %>% filter(node_id == id, supply_constraint == mode) %>% distinct(compact_id) %>% pull(compact_id)
  m <- cc_all %>% filter(compact_id %in% comp, supply_constraint == mode)
  tibble(node_id = id, sub_sector = k$sub_sector, mode = mode, exposure = r3(s$shock_exposure),
         existing = r3(s$existing_coverage), optimized = r3(s$optimized_coverage), dividend = r3(s$coordination_dividend),
         gap = r3(s$residual_gap), class = cls, warning = ifelse(is.na(w), "", w),
         compact = if (nrow(m)) paste(sub("AIB-", "", m$compact_id), collapse = ", ") else "",
         coalition = if (nrow(m)) m$minimum_coalition[1] else "",
         critical = if (nrow(m) && !is.na(m$critical_supply_partners[1])) m$critical_supply_partners[1] else "")
}
val <- bind_rows(lapply(val_ids, function(i) bind_rows(val_row(i, "production_constrained"), val_row(i, "trade_only"))))
pc <- function(x) sprintf("%.1f", 100 * x)
val_lines <- c(
  "% Generated by scripts/paper/fig_aib.R; do not edit by hand.",
  "\\begin{table}[tbp]", "\\centering", "\\footnotesize",
  "\\caption{Validation chains for four nodes, China removed, hold, reallocation. Mode: production-constrained (PC,",
  "the headline) or trade-only (TO). Exposure, existing (state B) and optimized (state C) coverage and the residual",
  "gap in percent of the allied scope's traded demand; dividend in points. Compact: the compact the node belongs to",
  "and its exact 95\\% coalition, critical supply partners in bold. The cases report what the model produced; nothing",
  "was tuned to them. Source: \\texttt{allied\\_trade\\_stress\\_test.csv}, \\texttt{aib\\_node\\_classification.csv},",
  "\\texttt{aib\\_compact\\_candidates.csv}; \\texttt{scripts/paper/fig\\_aib.R}.}",
  "\\label{tab:aib_validation}",
  "\\setlength{\\tabcolsep}{3pt}",
  "\\begin{tabularx}{\\textwidth}{@{}>{\\raggedright\\arraybackslash}p{2.3cm}lrrrrrl>{\\raggedright\\arraybackslash}X@{}}",
  "\\toprule",
  "Node & Mode & Exposure & Existing & Optimized & Dividend & Gap & Class & Compact and coalition \\\\",
  "\\midrule",
  sprintf("%s & %s & %s & %s & %s & %s & %s & %s%s & %s \\\\",
          ifelse(val$mode == "production_constrained", tex_esc(sub(" [+] .*", "", val$sub_sector)), ""),
          ifelse(val$mode == "production_constrained", "PC", "TO"),
          pc(val$exposure), pc(val$existing), pc(val$optimized), pc(val$dividend), pc(val$gap),
          ifelse(val$class == "LOW INTERVENTION", "LOW", val$class),
          ifelse(nzchar(val$warning), "$^{\\dagger}$", ""),
          ifelse(nzchar(val$compact), paste0(val$compact, ": ", vapply(seq_len(nrow(val)), function(i) {
            v <- split_codes(val$coalition[i]); crit <- split_codes(val$critical[i])
            paste(ifelse(v %in% crit, paste0("\\textbf{", v, "}"), v), collapse = " ")
          }, character(1))), "none")),
  "\\bottomrule",
  "\\multicolumn{9}{@{}p{\\textwidth}@{}}{\\footnotesize $^{\\dagger}$ PHYSICAL CAPABILITY NOT VALIDATED: the class rests on trade data that diverge from the physical data.}\\\\",
  "\\end{tabularx}", "\\end{table}")
writeLines(val_lines, paper_dir("tables", "aib_validation.tex"))

# ---------------------------------------------------------------------------
# Numbers for the text
# ---------------------------------------------------------------------------
tech_list <- lapply(split(tech_rows, tech_rows$tech), function(r) list(
  in_scope = r$in_strategic_scope, demand_usd_bn = r3(r$demand_usd / 1e9), lost_usd_bn = r3(r$lost_usd / 1e9),
  exposure = r3(r$shock_exposure), static = r3(r$static_coverage), existing = r3(r$existing_coverage),
  optimized = r3(r$optimized_coverage), dividend = round(r$coordination_dividend, 4), residual_gap = r3(r$residual_gap),
  investment = r3(r$investment_coverage), n_nodes = r$n_nodes, n_nodes_simulated = r$n_build_sim))
validation <- lapply(split(val, paste(val$node_id, val$mode)), as.list)

res <- list(
  generated = format(Sys.time(), "%Y-%m-%d %H:%M"),
  source_dir = "data/processed/outputs/aib (scripts/31-32 after the credibility pass, ba38e2b)",
  headline = HL,
  nodes = list(n = n_nodes, quality = quality, optimized = n_opt, in_strategic_scope = n_scope,
               physical_data = n_phys, physical_nodes = phys_nodes, shared_nodes = shared,
               candidates_per_node = list(median = median(cand$n_candidates), min = min(cand$n_candidates),
                                          max = max(cand$n_candidates)),
               coalition = list(core = n_core, partner = n_partner, demand_scope = n_core + n_partner)),
  china_largest_exporter = list(n_stages = nrow(stage_top), n_china = nrow(china_top),
                                n_china_midstream = sum(china_top$supply_chain == "Midstream"),
                                share_min = r3(min(china_top$share)), share_max = r3(max(china_top$share)),
                                stages = paste(china_top$tech, china_top$supply_chain),
                                others = setNames(as.list(stage_top$exporter_iso[stage_top$exporter_iso != "CHN"]),
                                                  paste(stage_top$tech, stage_top$supply_chain)[stage_top$exporter_iso != "CHN"])),
  scope_total = list(all_nodes = tot_all, strategic_scope = tot_scope),
  technology = tech_list,
  dividend = list(n = length(div), below_1pt = sum(div < 0.01), at_least_5pt = sum(div >= 0.05),
                  median_pt = round(100 * median(div), 3), max_node = top_div$sub_sector,
                  max_pt = round(100 * top_div$coordination_dividend, 2), max_existing = r3(top_div$existing_coverage)),
  high_exposure_nodes = nd %>% filter(shock_exposure >= 0.5) %>% arrange(desc(shock_exposure)) %>%
    transmute(sub_sector, exposure = r3(shock_exposure), residual_gap = r3(residual_gap), class = node_classification),
  classes = list(all = cls_all, strategic_scope = cls_scope, multi_label = needs, warnings = warn,
                 warnings_trade_only = warn_trade_only,
                 build_scope_median_gap = r3(median(build_scope$residual_gap)),
                 build_scope_median_exposure = r3(median(build_scope$shock_exposure))),
  investment = list(
    nodes_simulated_all = n_distinct(inv_h$node_id), nodes_simulated_scope = n_distinct(inv_scope$node_id),
    eligible_hosts = n_distinct(inv_scope$host_iso),
    hosts_per_node = list(median = median(hosts_per_node$n), min = min(hosts_per_node$n), max = max(hosts_per_node$n)),
    lead_gain_median = r3(median(lead$marginal_resilience_gain)), lead_gain_min = r3(min(lead$marginal_resilience_gain)),
    lead_gap_before_median = r3(median(lead$residual_gap_before)), lead_gap_after_median = r3(median(lead$residual_gap_after)),
    lead_gap_closed = sum(lead$residual_gap_after < 0.005),
    lead_capability_basis = as.list(table(lead$capability_basis)),
    lead_hosts = setNames(as.list(lead_count$n), lead_count$host_iso),
    n_lead_hosts = nrow(lead_count),
    lead_nodes = lapply(setNames(head(lead_count$host_iso, 3), head(lead_count$host_iso, 3)), function(h)
      nc$sub_sector[match(lead$node_id[lead$host_iso == h], nc$node_id)]),
    top_pair_nodes = lapply(seq_len(5), function(i) unique(pairs_x$node_label[pairs_x$investor_iso == flows$investor_iso[i] &
                                                                                   pairs_x$host_iso == flows$host_iso[i]])),
    flows_usd_bn = r3(usd_x / 1e9), domestic_usd_bn = r3(usd_dom / 1e9), n_pairs = nrow(flows),
    n_investors = n_distinct(pairs_x$investor_iso), n_hosts = n_distinct(pairs_x$host_iso),
    friendshore_usd_weighted = r3(fs_w),
    host_tier_share = setNames(as.list(r3(host_tier_share$s)), host_tier_share$t),
    top_hosts = lapply(seq_len(8), function(i) list(iso = host_tot$host_iso[i], usd_bn = r3(host_tot$usd[i] / 1e9),
                                                     nodes = host_tot$n_nodes[i], investors = host_tot$n_investors[i],
                                                     tier = tier_of(host_tot$host_iso[i]))),
    top_investors = lapply(seq_len(8), function(i) list(iso = inv_tot$investor_iso[i], usd_bn = r3(inv_tot$usd[i] / 1e9),
                                                         nodes = inv_tot$n_nodes[i], hosts = inv_tot$n_hosts[i])),
    top_pairs = lapply(seq_len(8), function(i) list(investor = flows$investor_iso[i], host = flows$host_iso[i],
                                                     usd_bn = r3(flows$usd[i] / 1e9), nodes = flows$n_nodes[i],
                                                     friendshore = r3(flows$friendshore[i]))),
    figure_share_shown = r3(share_shown)),
  network = list(n_edges = nrow(ed_h), n_trade_edges = sum(ed_h$n_trade_selected > 0),
                 n_investment_edges = sum(ed_h$n_investment_selected > 0),
                 n_both = sum(ed_h$n_trade_selected > 0 & ed_h$n_investment_selected > 0),
                 n_economies = nrow(cs_h), roles = role_counts,
                 top_suppliers = lapply(seq_len(6), function(i) {
                   r <- cs_h %>% arrange(desc(supplier_headline_volume_usd)) %>% slice(i)
                   list(iso = r$iso3, usd_bn = r3(r$supplier_headline_volume_usd / 1e9), partners = r$supplier_n_partners,
                        tier = tier_of(r$iso3))
                 }),
                 top_buyers = lapply(seq_len(6), function(i) {
                   r <- cs_h %>% arrange(desc(buyer_headline_volume_usd)) %>% slice(i)
                   list(iso = r$iso3, usd_bn = r3(r$buyer_headline_volume_usd / 1e9), partners = r$buyer_n_partners)
                 }),
                 supplier_volume_usd_bn = r3(sum(cs_h$supplier_headline_volume_usd) / 1e9),
                 supplier_volume_outside_share = r3(sum(cs_h$supplier_headline_volume_usd[tier_of(cs_h$iso3) == "outside"]) /
                                                      sum(cs_h$supplier_headline_volume_usd)),
                 top_edges = lapply(seq_len(5), function(i) {
                   r <- ed_h %>% arrange(desc(headline_trade_volume_usd)) %>% slice(i)
                   list(supplier = r$supplier_iso, buyer = r$buyer_iso, usd_bn = r3(r$headline_trade_volume_usd / 1e9),
                        subsectors = r$n_subsectors)
                 })),
  compacts = list(
    n = nrow(cc_h), types = as.list(table(cc_h$compact_type)), archetypes = as.list(table(cc_h$compact_archetype)),
    size = list(min = min(cc_h$n_members), median = median(cc_h$n_members), max = max(cc_h$n_members)),
    sizes = as.list(table(cc_h$n_members)),
    n_with_critical = sum(cc_h$n_critical > 0),
    critical = setNames(as.list(cc_h$critical_supply_partners[cc_h$n_critical > 0]), cc_h$compact_id[cc_h$n_critical > 0]),
    n_with_associates = sum(cc_h$n_associate > 0), n_with_crit_ext = sum(cc_h$n_crit_ext > 0),
    n_target_reached_95 = sum(cc_h$target_reached_95), n_robust_solved = sum(cc_h$robust_tie_break_solved),
    n_associates_reach_99 = sum(cc_h$associates_reach_99),
    exact_equals_greedy = sum(cc_h$same_as_greedy), exact_size_equals_greedy = sum(cc_h$same_size_greedy),
    n_subsectors = sum(cc_h$n_subsectors), n_multi_node = sum(cc_h$n_subsectors > 1),
    lost_usd_bn = r3(sum(cc_h$lost_usd) / 1e9),
    max_dividend_pt = round(100 * max(cc_h$coordination_dividend), 3),
    max_trade_minus_existing_pt = round(100 * max(cc_h$coverage_with_trade_coordination - cc_h$coverage_without_coordination), 3),
    anchor_gain_median = r3(median(cc_h$compact_gain_over_fragmented)),
    anchor_gain_range = r3(range(cc_h$compact_gain_over_fragmented)),
    investment_step_median_pt = round(100 * median(cc_h$coverage_after_investment_simulation - cc_h$coverage_with_trade_coordination), 2),
    after_investment_at_least_95 = sum(cc_h$coverage_after_investment_simulation >= 0.95),
    member_freq = setNames(as.list(head(member_freq$n, 10)), head(member_freq$iso3, 10)),
    n_member_economies = nrow(member_freq),
    key_policy_needs = as.list(comp_needs),
    rows = lapply(seq_len(nrow(cc_h)), function(i) with(cc_h[i, ], list(
      id = compact_id, nodes = included_subsectors, technology = technology, members = minimum_coalition,
      critical = critical_supply_partners, associates = associate_partners, critical_external = critical_external_dependencies,
      lost_usd_bn = r3(lost_usd / 1e9), static = r3(coverage_static), existing = r3(coverage_without_coordination),
      trade_compact = r3(coverage_with_trade_coordination), investment = r3(coverage_after_investment_simulation),
      anchor_fragmented = r3(anchor_coverage_fragmented), anchor_compact = r3(anchor_coverage_compact))))),
  validation = validation
)
write_paper_results(res, "aib")
message("fig_aib: wrote 5 figures, 2 tables and results/aib.json")
