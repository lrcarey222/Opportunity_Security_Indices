

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

#Policy Heatmap

index_outputs <- read_processed_tbl("outputs/index_outputs", processed_dir)
policy_index<-index_outputs$policy_index

iso <- "JPN"

nipo_heatmap <- nipo_policy_index_tbl %>%
  filter(iso3 == iso,
         supply_chain != "Downstream",
         !tech  %in% c("Electric Motors", "Magnets")) %>%
  select(country, tech, supply_chain, domestic_intervention_index) %>%
  arrange(desc(domestic_intervention_index)) %>%
  pivot_wider(names_from="supply_chain", 
              values_from="domestic_intervention_index") 

write.csv(nipo_heatmap,paste0(processed_dir,"/charts/nipo_heatmap_",iso,".csv"))

chart_country <-"Japan"

#Composite Policy Heatmap
policy_heatmap <- policy_index %>%
  filter(Country == chart_country,
         !tech  %in% c("Electric Motors", "Magnets")) %>%
  select(Country, tech, supply_chain, value) %>%
  arrange(desc(value)) %>%
  pivot_wider(names_from="supply_chain", 
              values_from="value") 

write.csv(policy_heatmap,paste0(processed_dir,"/charts/policy_heatmap_",iso,".csv"))

#NIPO Time Series Chart

nipo_tech_year <- read_processed_tbl("nipo_tech_year_tbl", processed_dir)

nipo_top<-nipo_policy_index_tbl %>%
  filter(iso3 == iso,
         supply_chain != "Downstream",
         !tech  %in% c("Electric Motors", "Magnets")) %>%
  arrange(desc(domestic_intervention_index)) %>%
  slice_max(n=5,order_by=domestic_intervention_index) 
  

nipo_time_plot<- nipo_tech_year %>%
  filter(iso3 == iso,
         supply_chain != "Downstream",
         !tech  %in% c("Electric Motors", "Magnets")) %>%
  inner_join(nipo_top,by=c("tech","supply_chain")) %>%
  mutate(industry=paste(tech,supply_chain)) %>%
  select(industry,announce_year,domestic_strength_balanced) %>%
  pivot_wider(names_from=industry,values_from=domestic_strength_balanced) %>%
  arrange(announce_year)
  
write.csv(nipo_time_plot,paste0(processed_dir,"/charts/nipo_time_plot_",iso,".csv"))

