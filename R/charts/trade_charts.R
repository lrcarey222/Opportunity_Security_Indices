Sys.setenv(COMTRADE_API_KEY = "aca1f47164e348fe978af0b93bfa6af4")
primary_key <- "5927e0b514da47d98f869ba5ca186485"
secondary_key<- "1ffe814c2cd048f0b1da30cc401cbac2"
tertiary_key <- "2940653b9bbe4671b3f7fde2846d14be"
additional_key <- "aca1f47164e348fe978af0b93bfa6af4"
key4<-"a709c13423c9424898a4292b383edc7a"
key5 <- "4967b706a00e49ee8819eee592f4fbb9"
premium_key <- "fd178c714d644e1ab8adbc16026faaaf"


library(lubridate)
library(slider)
library(countrycode)

source("scripts/96_pull_trade_timeseries.R")

#Trade Timeseries plot-------------

iso_rep<-"VNM"
iso_partner<-c("CHN","FRA","DEU","ITA","ESP","NLD","BEL","SWE","POL","DNK","FIN","CZE","ROU","HUN","AUT","PRT","GRC","IRL","JPN","USA","IND","VNM")

codelist_iso3 <- countrycode::codelist |>
  as_tibble() |>
  filter(!is.na(eu28)) |>
  pull(iso3c)


hs6_category_path <- file.path(
  raw_data_path,"energy_hs6_master.csv")

subcat <- readr::read_csv(hs6_category_path, show_col_types = FALSE)  
subcat_wind <- subcat %>%filter(essential==T)
subcat2<-subcat %>%
  mutate(HS6=as.character(HS6)) %>%
  filter(HS6=="854140")

oil_producers<- ei %>%
  filter(Var=="oilprod_kbd",
         Year=="2024",
         !is.na(ISO3166_numeric)) %>%
  arrange(desc(Value)) %>%
  slice_max(Value,n=25)

producer_iso <-distinct(oil_producers$ISO3166_alpha3)

res_wind <- pull_trade_timeseries(
  catalog = subcat_wind,
  country = country_info$iso3c,
  tech = c("Wind"),
  supply_chain = "Midstream",
  partners = "World",
  years = c("2025"),
  flow = c("import"),
  frequency="annual"
)


res<-bind_rows(res2,res3,res4)

base_year  <- 2019
base_month <- 1

library(dplyr)
library(tidyr)
library(lubridate)
library(slider)

plot_tot <- res_irn %>%
  #filter(flow_desc == "Export") %>%
  left_join(
    country_info %>% select(iso3c, region),
    by = c("partner_iso" = "iso3c")
  ) %>%
  left_join(
    subcat %>% mutate(code6 = as.character(hs6)),
    by = c("cmd_code" = "code6"),
    relationship = "many-to-many"
  ) %>%
  group_by(tech, reporter_desc,partner_desc, ref_year, ref_month) %>%
  summarise(imports = sum(primary_value, na.rm = TRUE), .groups = "drop") %>%
  mutate(date = ymd(sprintf("%d-%02d-01", ref_year, ref_month))) %>%
  select(tech, reporter_desc,partner_desc, date, imports) %>%
  
  # create missing months for every tech-partner combo
  group_by(tech, reporter_desc,partner_desc) %>%
  # 3-month rolling average
  mutate(
    imports_roll3m = slide_dbl(
      imports,
      ~ mean(.x, na.rm = TRUE),
      .before = 2,
      .complete = TRUE
    )
  ) %>%
  ungroup() 

iran_oil <- plot_tot %>%
  group_by(reporter_desc,partner_desc) %>%
  summarize(imports=sum(imports,na.rm=T)) %>%
  ungroup() %>%
  mutate(share=imports/sum(imports)*100) %>%
  arrange(desc(share))
  

plot_df_top <- plot_tot %>%
  mutate(year=substr(date,1,4)) %>%
  group_by(year,partner_desc) %>%
  summarize(imports=sum(imports,na.rm=T)) %>%
  filter(year=="2025") %>%
  arrange(desc(imports)) %>%
  slice_max(n=10,order_by=imports)
  

write.csv(plot_tot%>%
            filter(partner_desc %in% plot_df_top$partner_desc) %>%
            select(date, tech, partner_desc, imports_roll3m) %>%
            pivot_wider(
              names_from = partner_desc,
              values_from = imports_roll3m,
              values_fill = 0
            ),paste0("data/processed/charts/india_oil_country_monthly.csv"))



write.csv(plot_tot %>%
            slice_max(n=15,order_by=World),paste0(processed_dir,"/charts/us_electroindustrial_agg_imports_china.csv"))


#Atlas Economic COmplexity Data

world<-read.csv("data/raw/hs92_product_year_6.csv")
world<- read.csv("C:/Users/LCarey/Downloads/hs92_country_product_year_6.csv")

base_year=2016

world_plot<-world %>%
  inner_join(hs6_categories_essential %>% mutate(code6 = as.character(HS6)),
            by = c("product_hs92_code" = "code6"),
            relationship = "many-to-many") %>%
  group_by(Technology,Value.Chain, year) %>%
  summarise(exports = sum(export_value, na.rm = TRUE), .groups = "drop") %>%
  group_by(Technology,Value.Chain) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    base_exports = first(exports[year == base_year]),
    export_index = (exports / base_exports) * 100
  ) %>%
  ungroup() %>%
  mutate(industry=paste(Technology,Value.Chain)) 

write.csv(world_plot %>%
            filter(Technology %in% c("Semiconductors","Batteries","Solar","Gas","Oil")) %>%
            select(year,industry,export_index) %>%
            pivot_wider(names_from=industry,values_from=export_index),paste0(processed_dir,"/charts/electro_world_ind.csv"))


clean_electro_fossil <- world_plot %>%
  mutate(
    sector = case_when(
      Technology %in% c("Semiconductors","Batteries","Magnets","Electric Motors","Electric Grid") ~ "Electro-Industrial",
      Technology %in% c("Oil","Coal","Gas") ~ "Fossil",
      Technology %in% c("Solar","Wind","Green Hydrogen") ~ "Clean Power",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(sector)) %>%
  group_by(sector, year) %>%
  summarise(exports = sum(exports, na.rm = TRUE), .groups = "drop") %>%
  group_by(sector) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    base_exports = first(exports[year == base_year]),
    export_index = if_else(is.na(base_exports) | base_exports == 0, NA_real_, (exports / base_exports) * 100)
  ) %>%
  ungroup()
write.csv(clean_electro_fossil %>%
            select(year,sector,export_index) %>%
            pivot_wider(names_from=sector,values_from=export_index),paste0(processed_dir,"/charts/electro_world.csv"))


library(ggplot2)
ggplot(data=plot_tot,
       aes(x=ref_year,y=china_share,color=Sub.Sector))+
  geom_line()+
  theme_minimal()

write.csv(plot_tot,paste0(processed_dir,"/charts/trade_time_plot_","KOR",".csv"))

#India Monthly Exports
bulk_month<-read.csv("data/raw/comtrade_bulk/comtrade_bulk_monthly_hs92_selected_reporters.csv")
bulk_annual<-read.csv("data/raw/comtrade_bulk/comtrade_bulk_annual_hs92.csv")


india_month <- bulk_month %>%
  filter(reporter_desc=="India",
         partner_desc=="World",
         flow_desc=="Export") %>%
  left_join(subcat,by=c("cmd_code"="HS6")) %>%
  group_by(reporter_code,tech,supply_chain,`Sub.Sector`,ref_year,ref_month) %>%
  summarize(value=sum(primary_value,na.rm=T))

wind_export_ind_ann <- india_month %>%
  filter(tech=="Wind") %>%
  group_by(Sub.Sector,ref_year) %>%
  summarize(export=sum(value,na.rm=T)) %>%
  arrange(Sub.Sector,ref_year)


ggplot(data=wind_export_ind_ann,aes(x=ref_year,y=export,color=Sub.Sector))+geom_line()+theme_minimal()


india_import <- comtrade_energy_trade %>%
  filter(reporter_desc=="India") %>%
  left_join(subcat %>%
              mutate(hs6=as.character(HS6)),by=c("cmd_code"="HS6")) %>%
  filter(tech=="Wind") %>%
  group_by(reporter_desc,partner_desc,tech,supply_chain,`Sub.Sector`,ref_year) %>%
  summarize(value=sum(primary_value,na.rm=T)) %>%
  group_by(tech,supply_chain,`Sub.Sector`,ref_year) %>%
  mutate(share=value/value[[partner_desc=="World"]])


#China Fossil Import v Renewable Exports
plot_chn<-bulk_annual %>%
  filter(reporter_iso=="CHN",
         ref_year=="2024") %>%
  left_join(hts_codes_categories_bolstered_final %>%
              mutate(hs_code=as.character(HS6)),
            by=c("cmd_code"="HS6")) %>%
  group_by(Technology,flow_desc) %>%
  summarize(total=sum(primary_value,na.rm=T)) %>%
  pivot_wider(names_from=flow_desc,values_from=total)
write.csv(plot_chn,"data/processed/charts/china_importsexports_tech.csv")


#India Manufacturing Exports
ind_man_plot <- aec_6_data %>%
  filter(country_iso3_code=="IND") %>%
  inner_join(subcat %>%
              mutate(HS6=as.character(hs6)),by=c("product_hs92_code"="HS6"), relationship = "many-to-many") %>%
  filter(tech %in% c("Wind","Solar","Batteries"),
         supply_chain=="Midstream") %>%
  group_by(tech,supply_chain,year) %>%
  summarize(export=sum(export_value,na.rm=T)) %>%
  group_by(year) %>%
  mutate(share=export/sum(export,na.rm=T)) 

write.csv(ind_man_plot%>%
            select(year,tech,export) %>%
            pivot_wider(names_from=year,values_from=export),"data/processed/charts/ind_man_exports.csv")
ggplot(data=ind_man_plot,aes(x=year,y=export,fill=tech))+geom_col()+theme_minimal()

#India Solar Trade Concentration

ind_solar_concentration <- trade_concentration_tbl %>%
  filter(iso3c=="IND",tech=="Solar",data_type=="index") %>%
  select(sub_sector,variable,value) %>%
  pivot_wider(names_from=sub_sector,values_from=value) %>%
  write.csv("data/processed/charts/ind_solar_concentration.csv")


#Japanese Nuclear
jpn_nuc_concentration_bubble <- trade_concentration_tbl %>%
  filter(iso3c=="JPN",tech=="Nuclear",data_type=="index") %>%
  select(sub_sector,variable,value) %>%
  pivot_wider(names_from=variable,values_from=value) 

hs6_names<-comtrade_energy_trade %>%
  inner_join(subcat,by=c("cmd_code"="hs6")) %>%
  distinct(tech,supply_chain,sub_sector,cmd_code,cmd_desc)

jpn_nuclear_exports<-comtrade_energy_trade %>%
  inner_join(subcat,by=c("cmd_code"="hs6")) %>%
  filter(tech=="Nuclear",
         reporter_iso=="JPN") %>%
  arrange(desc(primary_value)) %>%
  distinct(reporter_iso,tech,supply_chain,sub_sector,cmd_code,cmd_desc,ref_year,flow_direction,primary_value) %>%
  pivot_wider(names_from=flow_direction,values_from=primary_value)

  write.csv(jpn_nuc_concentration_bubble,"data/processed/charts/jpn_nuc_concentration_bubble.csv")

  hs92_tbl <- ct_get_ref_table("H0")
  
  jap_nuc_4 <- aec_4_data %>%
    left_join(hs6_names %>%
                mutate(hs4=substr(cmd_code,1,4)),by=c("product_hs92_code"="hs4")) %>%
    filter(tech=="Nuclear",
           country_iso3_code=="JPN") %>%
    distinct(year,product_hs92_code,export_value,import_value,export_rca,pci,distance) %>%
    left_join(hs92_tbl,by=c("product_hs92_code"="id")) %>%
    mutate(text_clean = str_remove(text, "^[^-]+\\s*-\\s*"))
  
  
  
  jap_feas_bubble <- jap_nuc_4 %>%
    filter(year=="2023") %>%
    distinct(product_hs92_code,text_clean, export_value,export_rca,pci,distance)
  
  write.csv(jap_feas_bubble,"data/processed/charts/jap_feas_nuclear.csv")

  jea_nuc_trade <- jap_nuc_4 %>%
    mutate(net_exports=export_value-import_value) %>%
    select(year,text_clean,net_exports) %>%
    pivot_wider(names_from=text_clean,values_from=net_exports)
  
  write.csv(jea_nuc_trade,"data/processed/charts/jea_nuc_trade.csv")
  
#Grid Export Market
  
  grid_export <- trade_concentration_tbl %>%
    filter(tech=="Electric Grid",
           Year=="2024",
           variable=="export_size")%>%
    group_by(sub_sector) %>%
    summarize(export_size=sum(value,na.rm=T)) %>%
    arrange(desc(export_size))

  cables_export <- trade_concentration_tbl %>%
    filter(tech=="Electric Grid",
           Year=="2024",
           grepl("Cables",sub_sector))

#Korea Annual Nuclear Exports
  korea_nuc_exp<-res_kor %>%
    inner_join(subcat %>%
                 filter(tech=="Nuclear") %>%
                 mutate(cmd_code=as.character(hs6)),by=c("cmd_code")) %>%
    group_by(partner_desc,ref_year,tech) %>%
    summarize(exports=sum(primary_value,na.rm=T)) %>%
    pivot_wider(names_from=partner_desc,values_from=exports)

  write.csv(korea_nuc_exp,"data/processed/charts/korea_nuclear_exports_ts.csv")  
  
  wind_scatter <- res_eu_wind %>%
    inner_join(subcat %>%
                mutate(cmd_code=as.character(hs6)) %>%
                filter(essential==T),by=c("cmd_code"))
  
  #Wind Scatter
  library(WDI)
  gdp_pc <- WDI(
    country = "all",
    indicator = c("NY.GDP.PCAP.CD",
                  "NY.GDP.MKTP.CD"),
    start = 2023,
    end = 2023
  )
  
  wind_scatter <- res_wind %>%
    left_join(subcat %>%
      filter(tech=="Wind") %>%
      mutate(cmd_code=as.character(hs6)),by=c("cmd_code")) %>%
    left_join(codelist_iso3 %>%
                select(iso3c, region23,eu28),by=c("reporter_iso"="iso3c")) %>%
    filter(sub_sector %in% c("Wind Towers",
                             "Wind Blades",
                             "Nacelles & Drivetrains",
                             "Wind Turbines & Generators",
                             "Offshore Substructures")) %>%
    left_join(gdp_pc %>% 
                select(iso3c,`NY.GDP.PCAP.CD`,
                       `NY.GDP.MKTP.CD`),by=c("reporter_iso"="iso3c")) %>%
    mutate(region23=ifelse(grepl("Europe",region23),"Other Europe",region23)) %>%
    mutate(region=ifelse(reporter_iso=="USA","USA",
                         ifelse(!is.na(eu28),"EU",
                                ifelse(reporter_iso=="CHN","CHN",region23)))) %>%
    group_by(region) %>% 
    summarize(val=sum(primary_value,na.rm=T),
              gdp_cap=weighted.mean(x=NY.GDP.PCAP.CD,w=NY.GDP.MKTP.CD,na.rm=T)) %>%
    arrange(desc(val))
  
  # install.packages(c("unvotes", "tidyverse", "lubridate"))
  
  library(unvotes)
  library(tidyverse)
  library(lubridate)
  
  # Parameters
  start_year <- 2014
  end_year   <- 2024
  
  # Combine UN votes with vote dates
  votes <- un_votes %>%
    left_join(un_roll_calls, by = "rcid") %>%
    mutate(year = year(date)) %>%
    filter(year >= start_year, year <= end_year)
  
  # Pull China's vote on each roll call
  china_votes <- votes %>%
    filter(country_code == "CN") %>%
    select(rcid, china_vote = vote)
  
  # Country-year voting similarity with China
  china_alignment <- votes %>%
    inner_join(china_votes, by = "rcid") %>%
    filter(country_code != "CN") %>%
    mutate(
      same_as_china = vote == china_vote
    ) %>%
    group_by(country, country_code) %>%
    summarise(
      votes_compared = n(),
      china_similarity = mean(same_as_china, na.rm = TRUE),
      china_distance = 1 - china_similarity,
      .groups = "drop"
    )
  
  china_alignment
 

  china_alignment <- china_alignment %>%
    select(country_code,china_distance) %>%
  left_join(codelist_iso3 %>%
              select(iso2c,iso3c, region23,eu28),by=c("country_code"="iso2c")) %>%
    left_join(gdp_pc %>% 
                select(iso3c,`NY.GDP.PCAP.CD`,
                       `NY.GDP.MKTP.CD`),by=c("iso3c")) %>%
    mutate(region23=ifelse(grepl("Europe",region23),"Other Europe",region23)) %>%
    mutate(region=ifelse(iso3c=="USA","USA",
                         ifelse(!is.na(eu28),"EU",
                                ifelse(iso3c=="CHN","CHN",region23)))) %>%
    group_by(region) %>% 
    summarize(china_index=weighted.mean(china_distance,
                                        w=NY.GDP.MKTP.CD,
                                        na.rm=T))

  wind_scatter<-wind_scatter %>%
    left_join(china_alignment,by=c("region"))
  
  write.csv(wind_scatter,"data/processed/charts/wind_scatter.csv")
  