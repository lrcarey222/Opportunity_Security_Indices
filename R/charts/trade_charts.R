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

source("scripts/96_pull_trade_timeseries.R")

#Trade Timeseries plot-------------

iso_rep<-"VNM"
iso_partner<-c("CHN","FRA","DEU","ITA","ESP","NLD","BEL","SWE","POL","DNK","FIN","CZE","ROU","HUN","AUT","PRT","GRC","IRL","JPN","USA","IND","VNM")

subcat2<-subcat %>%
  mutate(HS6=as.character(HS6)) %>%
  filter(HS6=="854140")

res_vnm <- pull_trade_timeseries(
  catalog = subcat2,
  country = "VNM",
  tech = c("Solar"),
  supply_chain = "Midstream",
  partners = "USA",
  years = c("2022:2025"),
  flow = c("export"),
  frequency="annual"
)

res<-bind_rows(res2,res3,res4)

base_year  <- 2019
base_month <- 1

plot_tot <- res %>%
  filter(flow_desc == "Import") %>%
  left_join(country_info %>% select(iso3c, region),
            by = c("partner_iso" = "iso3c")) %>%
  left_join(hs6_categories_essential %>% mutate(code6 = as.character(HS6)),
            by = c("cmd_code" = "code6"),
            relationship = "many-to-many") %>%
  filter(Technology %in% c("Semiconductors","Batteries","Solar","Magnets","Electric Motors")) %>%
  group_by(Technology,Sub.Sector, partner_desc, ref_year, ref_month) %>%
  summarise(imports = sum(primary_value, na.rm = TRUE), .groups = "drop") %>%
  mutate(date = ymd(sprintf("%d-%02d-01", ref_year, ref_month))) %>%
  group_by(Technology, Sub.Sector,partner_desc) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(imports_roll3m = slide_dbl(
    imports,
    ~ mean(.x, na.rm = TRUE),
    .before = 11,
    .complete = TRUE
  )) %>%
  ungroup() %>%
  filter(partner_desc == "World") %>%
  group_by(Technology,Sub.Sector) %>%
  mutate(
    base_imports = first(imports_roll3m[ref_year == base_year & ref_month == base_month]),
    import_index = (imports_roll3m / base_imports) * 100
  ) %>%
  ungroup() %>%
  mutate(industry=paste(Technology,Sub.Sector)) %>%
  select(industry, date, import_index) %>%
  filter(industry %in% plot_df_top$industry) %>%
  pivot_wider(names_from = industry, values_from = import_index) %>%
  arrange(date)

plot_df_top <- plot_tot %>%
  filter(date=="2024-12-01") %>%
  arrange(desc(import_index)) %>%
  slice_max(n=10,order_by=import_index)
  

write.csv(plot_tot,paste0(processed_dir,"/charts/us_electro_monthly_sub.csv"))



%>%
  pivot_wider(names_from=partner_desc,values_from=exports) %>%
  mutate(china_share=China/World*100,
         non_china=World-China) %>%
  group_by(Technology,Sub.Sector) %>%
  mutate(base_exports = World[ref_year == base_year][1]) %>%  # take the first if duplicates
  mutate(export_index = (World / base_exports-1)*100) %>%
  ungroup() %>%
  filter(ref_year=="2024") %>%
  #select(Sub.Sector, ref_year, export_index) %>%
  #pivot_wider(names_from = Sub.Sector, values_from = export_index) %>%
  mutate(industry=paste(Technology)) %>%
  arrange(desc(export_index))

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


%>%
  select(industry, date, import_index) %>%
  filter(industry %in% plot_df_top$industry) %>%
  pivot_wider(names_from = industry, values_from = import_index) %>%
  arrange(date)
  

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

