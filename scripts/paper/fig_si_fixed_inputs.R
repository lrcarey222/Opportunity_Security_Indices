# Figure: the country-invariant inputs of the Strategic Index (paper Section 8,
# fig:si_fixed_inputs).
#
# Panel (a) plots the CAT policy component x = kappa * g + (1 - kappa) * (1 - g) against the
# technology GHG weight g for the four CAT rating scores kappa in {0.25, 0.5, 0.75, 1}, with the
# eleven headline technologies marked on each line. The values come from the pipeline's own
# functions: partnership_strength_clean_ghg() (g_t from data/raw/ipcc_ghg_intensity.csv, with
# the oil row replaced by the SRREN lifecycle median inside that function) and
# cat_policy_index() (the formula). The legend counts the countries CAT rates in each category
# in the final build, all of them and the allied-coalition members among them, read from the
# CAT rows of policy_component_tbl in data/processed/outputs/index_outputs.rds.
# Panel (b) shows the sector prior 0.1 * lambda_s + 0.1 * g_t that the stage and technology
# weights add to every country's Strategic Index, using STRATEGIC_SUPPLY_CHAIN_WEIGHTS and
# STRATEGIC_INDEX_WEIGHTS from R/indices/build_indices.R. Geothermal - Midstream is not a
# reported sector (STRATEGIC_EXCLUDED_SECTORS) and is greyed out.
#
# The script stops unless the stage and technology weights it computes are the ones carried by
# the Strategic Index of the final build (sc_weight, tech_weight in index_outputs.rds).
# Output: paper/figures/fig_si_fixed_inputs.pdf (+ png twin) and
# paper/results/si_fixed_inputs.json.
#
# Run from anywhere inside the repository:
#   Rscript scripts/paper/fig_si_fixed_inputs.R

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
  library(cowplot)
})

root <- paper_repo_root()
source(file.path(root, "R", "utils", "schema.R"))
source(file.path(root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(root, "R", "categories", "policy", "cat_policy_index.R"))
source(file.path(root, "R", "indices", "build_indices.R"))

config <- yaml::read_yaml(Sys.getenv("OPSI_CONFIG", file.path(root, "config", "config.yml")))
raw_dir <- file.path(root, config$raw_data_dir)

tech_ghg_raw <- readr::read_csv(file.path(raw_dir, "ipcc_ghg_intensity.csv"),
                                show_col_types = FALSE)
tech_ghg <- partnership_strength_clean_ghg(tech_ghg_raw)
g_code <- stats::setNames(tech_ghg$ghg_index, tech_ghg$tech)
stopifnot(length(g_code) == 11, !anyNA(g_code))

stopifnot(identical(unname(STRATEGIC_SUPPLY_CHAIN_WEIGHTS[c("Upstream", "Midstream", "Downstream")]),
                    c(0.50, 0.75, 0.25)))
stopifnot(isTRUE(all.equal(unname(STRATEGIC_INDEX_WEIGHTS),
                           c(0.2, 0.3, 0.3, 0.1, 0.1))))

# ---------------------------------------------------------------------------
# Final build: the weights the Strategic Index used, and the CAT ratings
# ---------------------------------------------------------------------------
io <- readRDS(file.path(root, "data", "processed", "outputs", "index_outputs.rds"))
si <- io$strategic_index
policy_component <- io$policy_component_tbl
coalition <- io$coalition_members
policy_provenance <- io$policy_index_provenance
rm(io)
invisible(gc())

si_weights <- si %>% dplyr::distinct(.data$tech, .data$supply_chain, .data$sc_weight, .data$tech_weight)
stopifnot(
  all(abs(si_weights$sc_weight - unname(STRATEGIC_SUPPLY_CHAIN_WEIGHTS[si_weights$supply_chain])) < 1e-12),
  all(abs(si_weights$tech_weight - unname(g_code[si_weights$tech])) < 1e-12),
  !any(paste(si$tech, si$supply_chain, sep = " - ") %in% STRATEGIC_EXCLUDED_SECTORS)
)
si_in <- si[!is.na(si$strategic_index), ]
si_cells <- nrow(si_in)
si_countries <- length(unique(si_in$Country))
rm(si)

# CAT is constant across stages and its value for a technology with g = 0 is 1 - kappa.
g0_tech <- names(g_code)[g_code == 0]
stopifnot(length(g0_tech) == 1)
cat_rows <- policy_component %>%
  dplyr::filter(.data$variable == "CAT Policy Index", .data$tech == g0_tech) %>%
  dplyr::distinct(.data$Country, .data$iso3c, .data$value) %>%
  dplyr::mutate(kappa = round(1 - .data$value, 6))
stopifnot(!anyDuplicated(cat_rows$Country),
          all(cat_rows$kappa %in% c(0.25, 0.5, 0.75, 1)))
# The composite rows keep only the cells the policy index scores: check the formula on them.
cat_all <- policy_component %>%
  dplyr::filter(.data$variable == "CAT Policy Index") %>%
  dplyr::left_join(dplyr::select(cat_rows, "Country", "kappa"), by = "Country") %>%
  dplyr::mutate(g = unname(g_code[.data$tech]))
stopifnot(max(abs(cat_all$value - (cat_all$kappa * cat_all$g + (1 - cat_all$kappa) * (1 - cat_all$g)))) < 1e-9)
rm(policy_component, cat_all)
cat_rows$member <- cat_rows$iso3c %in% coalition$iso3

# ---------------------------------------------------------------------------
# Panel (a): CAT alignment lines, computed by cat_policy_index()
# ---------------------------------------------------------------------------
ratings <- tibble::tibble(
  Country = c("Critically insufficient", "Highly insufficient", "Insufficient",
              "Almost sufficient"),
  climate_policy_index = c(0.25, 0.50, 0.75, 1.00)
)
cat_tbl <- cat_policy_index(tech_ghg, ratings, supply_chain_universe = "Midstream") %>%
  dplyr::left_join(tech_ghg, by = "tech") %>%
  dplyr::left_join(ratings, by = "Country")
# The component is the formula stated in the paper.
stopifnot(isTRUE(all.equal(
  cat_tbl$value,
  cat_tbl$climate_policy_index * cat_tbl$ghg_index +
    (1 - cat_tbl$climate_policy_index) * (1 - cat_tbl$ghg_index)
)))

rating_counts <- ratings %>%
  dplyr::mutate(
    rated = vapply(.data$climate_policy_index, function(k) sum(cat_rows$kappa == k), integer(1)),
    members = vapply(.data$climate_policy_index, function(k) sum(cat_rows$kappa == k & cat_rows$member),
                     integer(1))
  )
stopifnot(sum(rating_counts$rated) == nrow(cat_rows))

rating_levels <- ratings$Country
# plotmath labels: the pdf() device cannot be relied on to carry a Unicode kappa.
rating_labels <- lapply(seq_len(nrow(rating_counts)), function(j) {
  bquote(.(rating_counts$Country[j]) ~ "(" * kappa[i] == .(rating_counts$climate_policy_index[j]) * "):" ~
           .(rating_counts$rated[j]) ~ "rated," ~ .(rating_counts$members[j]) ~ "members")
})
cat_tbl$rating <- factor(cat_tbl$Country, levels = rating_levels)

lines_tbl <- tidyr::crossing(ratings, g = seq(0, 1, length.out = 101)) %>%
  dplyr::mutate(
    value = climate_policy_index * g + (1 - climate_policy_index) * (1 - g),
    rating = factor(Country, levels = rating_levels)
  )

# The slope in g_t is 2 kappa - 1, a signed quantity: red for the falling line, gray for the
# flat one, blue (light to dark) for the rising ones.
rating_cols <- stats::setNames(
  c(paper_div[["high"]], paper_ink$muted, paper_seq_blue[3], paper_seq_blue[6]),
  rating_levels
)

# Technology labels along the top: the spread-out technologies individually (the two with g
# near 0 together) and the six low-carbon technologies (g between 0.966 and 0.999) as one group.
fossil_pair <- names(sort(g_code))[1:2]
spread_techs <- c("Gas", "Electric Vehicles", "Green Hydrogen")
tech_labels <- tibble::tibble(
  g = c(0, unname(g_code[spread_techs])),
  label = c(paste0(fossil_pair[1], ", ", tolower(fossil_pair[2])), "Gas", "EVs", "Green H2"),
  y = c(1.07, 1.07, 1.07, 1.13),
  hjust = c(0, 0.5, 0.5, 0.5)
)
cluster_techs <- setdiff(names(g_code), c(fossil_pair, spread_techs))
stopifnot(length(cluster_techs) == 6)
cluster_range <- range(g_code[cluster_techs])

panel_a <- ggplot() +
  geom_vline(data = tibble::tibble(g = g_code), aes(xintercept = g),
             colour = paper_ink$grid, linewidth = 0.25) +
  geom_line(data = lines_tbl, aes(g, value, colour = rating), linewidth = 0.7) +
  geom_point(data = cat_tbl, aes(ghg_index, value, colour = rating), size = 1.4) +
  geom_text(data = tech_labels, aes(x = g, y = y, label = label, hjust = hjust), size = 2.4,
            colour = paper_ink$secondary) +
  annotate("text", x = cluster_range[2], y = 1.07, label = "6 low-carbon",
           size = 2.4, colour = paper_ink$secondary, hjust = 1) +
  scale_colour_manual(values = rating_cols, labels = rating_labels, name = NULL) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.02, 1.02),
                     expand = expansion(mult = 0)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(0, 1.17),
                     expand = expansion(mult = 0)) +
  guides(colour = guide_legend(ncol = 1)) +
  labs(x = expression("Technology weight " * g[t] * " (1 = cleanest)"),
       y = expression("CAT component " * x[its]^{CAT})) +
  theme_paper(grid = "y") +
  theme(legend.position = "bottom", legend.margin = margin(t = 0),
        legend.text = element_text(size = 7),
        plot.margin = margin(14, 8, 6, 6))

# ---------------------------------------------------------------------------
# Panel (b): sector prior 0.1 * lambda_s + 0.1 * g_t
# ---------------------------------------------------------------------------
w <- STRATEGIC_INDEX_WEIGHTS
prior_tbl <- tidyr::crossing(
  tech_ghg,
  supply_chain = names(STRATEGIC_SUPPLY_CHAIN_WEIGHTS)
) %>%
  dplyr::mutate(
    lambda = unname(STRATEGIC_SUPPLY_CHAIN_WEIGHTS[supply_chain]),
    prior = w[["supply_chain"]] * lambda + w[["technology"]] * ghg_index,
    sector = paste(tech, supply_chain, sep = " - "),
    reported = !sector %in% STRATEGIC_EXCLUDED_SECTORS,
    supply_chain = factor(supply_chain, levels = c("Upstream", "Midstream", "Downstream")),
    tech = factor(tech, levels = names(sort(g_code, decreasing = TRUE)))
  )
stopifnot(nrow(prior_tbl) == 33, sum(prior_tbl$reported) == 32)
prior_max <- max(prior_tbl$prior)

panel_b <- ggplot(prior_tbl, aes(supply_chain, tech)) +
  geom_tile(data = dplyr::filter(prior_tbl, reported), aes(fill = prior),
            colour = "white", linewidth = 0.6) +
  geom_tile(data = dplyr::filter(prior_tbl, !reported), fill = paper_ink$neutral,
            colour = "white", linewidth = 0.6) +
  geom_text(data = dplyr::filter(prior_tbl, reported),
            aes(label = sprintf("%.3f", prior),
                colour = prior > 0.11),
            size = 2.4, show.legend = FALSE) +
  geom_text(data = dplyr::filter(prior_tbl, !reported), label = "not reported",
            size = 2.1, colour = paper_ink$muted) +
  scale_fill_seq(name = expression(0.1 * lambda[s] + 0.1 * g[t]),
                 limits = c(0, 0.175), breaks = c(0, 0.05, 0.10, 0.15),
                 guide = guide_colourbar(title.position = "top")) +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = paper_ink$primary)) +
  scale_x_discrete(position = "top", expand = expansion(add = 0)) +
  scale_y_discrete(limits = rev, expand = expansion(add = 0)) +
  labs(x = NULL, y = NULL) +
  theme_paper(grid = "none") +
  theme(axis.line.x = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(0.9, "cm"),
        legend.margin = margin(t = 0),
        plot.margin = margin(14, 8, 6, 6))
stopifnot(prior_max <= 0.175)

fig <- cowplot::plot_grid(panel_a, panel_b, nrow = 1, rel_widths = c(1.1, 1),
                          align = "h", axis = "t",
                          labels = c("(a)", "(b)"), label_size = 9, label_fontface = "bold",
                          label_x = 0, hjust = 0)

save_paper_fig(fig, "fig_si_fixed_inputs", width = paper_width_full, height = 3.8)

ipcc <- tech_ghg_raw
write_paper_results(
  list(
    ghg_index = as.list(g_code[order(g_code)]),
    ghg_intensity_inputs_g_per_kwh = stats::setNames(as.list(ipcc$ghg_intensity), ipcc$Tech),
    oil_intensity_used_g_per_kwh = IPCC_OIL_LIFECYCLE_MEDIAN_G_PER_KWH,
    oil_intensity_in_input_file = ipcc$ghg_intensity[grepl("^Oil", ipcc$Tech)],
    supply_chain_weights = as.list(STRATEGIC_SUPPLY_CHAIN_WEIGHTS),
    strategic_index_weights = as.list(STRATEGIC_INDEX_WEIGHTS),
    excluded_sectors = STRATEGIC_EXCLUDED_SECTORS,
    sector_prior = stats::setNames(
      as.list(prior_tbl$prior[prior_tbl$reported]),
      prior_tbl$sector[prior_tbl$reported]
    ),
    sector_prior_range = range(prior_tbl$prior[prior_tbl$reported]),
    sector_prior_min_sector = prior_tbl$sector[prior_tbl$reported][which.min(prior_tbl$prior[prior_tbl$reported])],
    sector_prior_max_sector = prior_tbl$sector[prior_tbl$reported][which.max(prior_tbl$prior[prior_tbl$reported])],
    technology_term_gap_oil_wind = w[["technology"]] * (max(g_code) - min(g_code)),
    stage_term_gap_mid_down = w[["supply_chain"]] *
      (STRATEGIC_SUPPLY_CHAIN_WEIGHTS[["Midstream"]] - STRATEGIC_SUPPLY_CHAIN_WEIGHTS[["Downstream"]]),
    # Lead of Nuclear Midstream's prior over the fossil Midstream sectors: the whole of the gap
    # comes from the technology term, since the four share the stage weight. It is the margin a
    # country whose only rankable sectors are these four Midstream pairs starts from.
    nuclear_midstream_prior_lead = stats::setNames(
      as.list(w[["technology"]] * (g_code[["Nuclear"]] - g_code[c("Gas", "Coal", "Oil")])),
      c("over_gas", "over_coal", "over_oil")
    ),
    si_bounds = c(
      lower = w[["supply_chain"]] * min(STRATEGIC_SUPPLY_CHAIN_WEIGHTS) + w[["technology"]] * min(g_code),
      upper = w[["policy"]] + w[["economic_opportunity"]] + w[["energy_security_need"]] +
        w[["supply_chain"]] * max(STRATEGIC_SUPPLY_CHAIN_WEIGHTS) + w[["technology"]] * max(g_code)
    ),
    cat_slope_by_rating = stats::setNames(as.list(2 * ratings$climate_policy_index - 1),
                                          ratings$Country),
    cat_ratings_final_build = list(
      countries_rated = nrow(cat_rows),
      coalition_members_rated = sum(cat_rows$member),
      coalition_members = nrow(coalition),
      by_rating = rating_counts %>% dplyr::rename(rating = Country, kappa = climate_policy_index),
      members_by_rating = lapply(
        stats::setNames(ratings$climate_policy_index, ratings$Country),
        function(k) sort(cat_rows$iso3c[cat_rows$kappa == k & cat_rows$member])
      ),
      members_without_cat = sort(setdiff(coalition$iso3, cat_rows$iso3c)),
      eu_members_given_bloc_rating = length(policy_provenance$component_build$cat_eu_bloc_members)
    ),
    si_final_build = list(cells = si_cells, countries = si_countries)
  ),
  "si_fixed_inputs"
)

message("Wrote ", file.path(paper_dir("figures"), "fig_si_fixed_inputs.pdf"))
