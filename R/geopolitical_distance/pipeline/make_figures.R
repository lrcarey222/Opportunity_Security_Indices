repo_root <- getwd()
for (pkg in c("readr", "yaml", "dplyr", "ggplot2", "tidyr")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, repos = "https://cloud.r-project.org")
}
library(readr); library(yaml); library(dplyr); library(ggplot2); library(tidyr)

cfg <- read_yaml(file.path(repo_root, "config", "geopolitical_distance.yml"))
seg <- read_csv(file.path(repo_root, cfg$outputs$seg), show_col_types = FALSE)
ipd <- read_csv(file.path(repo_root, cfg$outputs$ipd), show_col_types = FALSE)

dir.create(file.path(repo_root, "figures", "geopolitical_distance"), showWarnings = FALSE, recursive = TRUE)

base_2021 <- seg %>% filter(spec_name == "baseline_all_2021") %>% select(iso3, seg_base = seg)
all_2023 <- seg %>% filter(spec_name == "all_2023") %>% select(iso3, seg_2023 = seg)
econ_2021 <- seg %>% filter(spec_name == "economic_2021") %>% select(iso3, seg_econ = seg)
post_2021 <- seg %>% filter(spec_name == "post1990_all_2021") %>% select(iso3, seg_post = seg)

p1 <- inner_join(base_2021, all_2023, by = "iso3") %>%
  ggplot(aes(seg_base, seg_2023)) + geom_point(alpha = 0.6) + geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Baseline 2021 seg", y = "All-votes 2023 seg", title = "Seg comparison: baseline vs 2023") + theme_minimal()

p2 <- inner_join(base_2021, econ_2021, by = "iso3") %>%
  ggplot(aes(seg_base, seg_econ)) + geom_point(alpha = 0.6) + geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Baseline 2021 seg", y = "Economic-votes 2021 seg", title = "Seg comparison: baseline vs economic") + theme_minimal()

p3 <- inner_join(base_2021, post_2021, by = "iso3") %>%
  ggplot(aes(seg_base, seg_post)) + geom_point(alpha = 0.6) + geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Baseline 2021 seg", y = "Post-1990 2021 seg", title = "Seg comparison: baseline vs post-1990") + theme_minimal()

ggsave(file.path(repo_root, "figures", "geopolitical_distance", "seg_scatter_baseline_vs_2023.png"), p1, width = 7, height = 5)
ggsave(file.path(repo_root, "figures", "geopolitical_distance", "seg_scatter_baseline_vs_economic.png"), p2, width = 7, height = 5)
ggsave(file.path(repo_root, "figures", "geopolitical_distance", "seg_scatter_baseline_vs_post1990.png"), p3, width = 7, height = 5)

ph <- ggplot(seg, aes(seg, fill = spec_name)) + geom_histogram(alpha = 0.5, bins = 30, position = "identity") +
  facet_wrap(~spec_name, scales = "free_y") + theme_minimal() + labs(title = "Distribution of seg scores")
ggsave(file.path(repo_root, "figures", "geopolitical_distance", "seg_histograms.png"), ph, width = 10, height = 7)

majors <- c("USA", "CHN", "JPN", "KOR", "IND", "VNM", "DEU", "FRA", "RUS")
hm <- ipd %>% filter(spec_name == "baseline_all_2021", iso3_i %in% majors, iso3_j %in% majors) %>%
  ggplot(aes(iso3_i, iso3_j, fill = ipd)) + geom_tile() + scale_fill_viridis_c() + theme_minimal() +
  labs(title = "Bilateral IPD heatmap (baseline 2021)")
ggsave(file.path(repo_root, "figures", "geopolitical_distance", "ipd_heatmap_majors.png"), hm, width = 7, height = 6)
