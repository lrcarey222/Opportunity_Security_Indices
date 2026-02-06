

processed_dir <- file.path(repo_root, config$processed_dir)
if (!dir.exists(processed_dir)) {
  stop("Processed data directory not found: ", processed_dir)
}

read_processed_tbl <- function(name, processed_dir) {
  path <- file.path(processed_dir, paste0(name, ".rds"))
  if (!file.exists(path)) {
    stop("Processed table not found: ", path)
  }
  readRDS(path)
}

nipo_policy_index_tbl <- read_processed_tbl("nipo_policy_index_tbl", processed_dir)

#Asia Trip NIPO Comparison

asia_nipo <- nipo_policy_index_tbl %>%
  filter(iso3 %in% c("JPN",
                    "KOR",
                    "VNM",
                    "IND")) %>%
  mutate(industry=paste0(tech,"-",supply_chain)) %>%
  select(country, industry, domestic_stock_sum) %>%
  pivot_wider(names_from="country", 
              values_from="domestic_stock_sum") 

write.csv(asia_nipo,paste0(processed_dir,"/charts/asia_nipo.csv"))
  