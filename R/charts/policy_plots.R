

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

#nipo_policy_index_tbl <- read_processed_tbl("nipo_policy_index_tbl", processed_dir)

#Asia Trip NIPO Comparison

asia_nipo <- nipo_tech_year %>%
  filter(iso3 %in% c("JPN",
                    "KOR",
                    #"VNM",
                    "IND"),
         announce_year=="2026") %>%
  mutate(industry=paste0(tech,"-",supply_chain)) %>%
  select(country, industry, domestic_intervention_index_xs) %>%
  pivot_wider(names_from="country", 
              values_from="domestic_intervention_index_xs") 

write.csv(asia_nipo,paste0(processed_dir,"/charts/asia_nipo.csv"))

#Policy Heatmap

index_outputs <- read_processed_tbl("outputs/index_outputs", processed_dir)
policy_index<-index_outputs$policy_index

iso <- "JPN"

ally_iso3 <- c(
  "USA", "CAN", "JPN", "AUS", "IND", "MEX", "KOR", "GBR", "DEU", "FRA", "ITA", "BRA", "SAU",
  "ZAF", "IDN", "NOR", "ARE", "VNM", "KEN", "DNK", "ARG", "MAR", "CHL"
)

nipo_heatmap <- nipo_policy_index_tbl %>%
  filter(iso3 == iso,
         supply_chain != "Downstream",
         !tech  %in% c("Electric Motors", "Magnets")) %>%
  select(country, tech, supply_chain, domestic_intervention_index) %>%
  arrange(desc(domestic_intervention_index)) %>%
  pivot_wider(names_from="supply_chain", 
              values_from="domestic_intervention_index") 

write.csv(nipo_heatmap,paste0(processed_dir,"/charts/nipo_heatmap_",iso,".csv"))

allied_heatmap <- nipo_policy_index_tbl %>%
  filter(
    iso3 %in% c("USA", "CAN", "JPN", "AUS", "IND", "KOR", "GBR", "DEU", "FRA", "ITA", "DNK") |
      country %in% c("Austria","Belgium","Switzerland","Greece","Netherlands","Poland","Portugal","Spain","Sweden"),
    supply_chain != "Downstream",
    !tech %in% c("Electric Motors", "Magnets")
  ) %>%
  mutate(
    country2 = case_when(
      iso3 %in% c("USA", "CAN", "JPN", "AUS", "IND", "KOR", "GBR") ~ country,
      TRUE ~ "EU"
    ),
    industry = paste(tech, supply_chain)) %>%
  filter(!industry %in% c("Solar Upstream","Wind Upstream","Electric Grid Upstream")) %>%
  mutate(
    industry = case_when(
      tech %in% c("Batteries","Solar","Wind","Semiconductors","Electric Vehicles","Electric Grid") &
        supply_chain=="Upstream" ~ "Critical Minerals",
      TRUE ~ tech
    ),
  ) %>%
  group_by(country2, industry) %>%
  summarize(
    domestic_intervention_index = mean(domestic_intervention_index, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(domestic_intervention_index)) %>%
  pivot_wider(
    names_from = country2,
    values_from = domestic_intervention_index
  )


write.csv(allied_heatmap,paste0(processed_dir,"/charts/nipo_heatmap_allies.csv"))


chart_country <-"India"

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

nipo_top<-nipo_tech_year %>%
  filter(iso3 == iso,
         supply_chain != "Downstream",
         !tech  %in% c("Electric Motors", "Magnets"),
         announce_year=="2026") %>%
  arrange(desc(domestic_intervention_index_xs_within_country)) %>%
  slice_max(n=5,order_by=domestic_intervention_index_xs_within_country) %>%
  mutate(industry=paste(tech,supply_chain)) 
  
  

nipo_time_plot<- nipo_tech_year %>%
  mutate(industry=paste(tech,supply_chain)) %>%
    filter(iso3 == iso,
         supply_chain != "Downstream",
         !tech  %in% c("Electric Motors", "Magnets"),
         industry %in% unique(nipo_top$industry)) %>%
  #inner_join(nipo_top,by=c("iso3","country","tech","supply_chain","announce_year")) %>%
  select(industry,announce_year,domestic_intervention_index_xs_within_country) %>%
  pivot_wider(names_from=industry,values_from=domestic_intervention_index_xs_within_country) %>%
  arrange(announce_year)
  
write.csv(nipo_time_plot,paste0(processed_dir,"/charts/nipo_time_plot_",iso,".csv"))



#Equity intervention

allies <- country_info %>%
  filter(iso3c %in% ally_iso3)

equity <- nipo_raw %>%
  dplyr::mutate(
    `Implementing Jurisdiction` = standardize_country_names(`Implementing Jurisdiction`)
  ) %>%
  left_join(subcat %>%
              mutate(HS6=as.character(hs6)),by=c("Product: HS 6-digit (2022)"="HS6")) %>%
  filter(`GTA Intervention Type`=="Equity stake",
         `Implementing Jurisdiction` %in% allies$country,
         URL != "https://www.globaltradealert.org/intervention/153898") %>%
  select(-`Sector: CPC 3-digit (v2.1)`) %>%
  mutate(year=substr(`Announcement Date`,1,4)) %>%
  #filter(year>2020) %>%
  separate_rows(Product, sep = ";\\s*") %>%
  
  # Reshape Sector columns
  pivot_longer(
    cols = starts_with("Sector:"),
    names_to = "Sector",
    values_to = "sector_flag"
  ) %>%
  filter(sector_flag == TRUE) %>%
  
  # Reshape Motive columns
  pivot_longer(
    cols = starts_with("Motive:"),
    names_to = "Motive",
    values_to = "motive_flag"
  ) %>%
  filter(motive_flag == TRUE) %>%
  
  # Clean labels
  mutate(
    Sector = str_remove(Sector, "^Sector: "),
    Motive = str_remove(Motive, "^Motive: "),
    Product = str_trim(Product)
  ) %>%
  
  # Count policies
  group_by(`Implementing Jurisdiction`,year) %>%
  summarise(
    #n_policies = n_distinct(`Entry ID`),
    sub=sum(`Size of Subsidy (USD Million)`,na.rm=T ),
    .groups = "drop"
  ) %>%
  arrange(year,desc(sub)) %>%
  pivot_wider(names_from=year,values_from=sub)

write.csv(equity,"data/processed/charts/equity_policies.csv")


#Japan Investment Roadmap

jpn_rdmp <- read.csv("C:/Users/LCarey/Downloads/japan_growth_strategy_product_breakdown_usd.csv")

jpn_rdmp <- jpn_rdmp %>%
  select(Sector,Product.technology,Investment.USD.bn) %>%
  pivot_wider(names_from="Product.technology",values_from="Investment.USD.bn")

write.csv(jpn_rdmp,"data/processed/charts/jpn_roadmap_products.csv")
