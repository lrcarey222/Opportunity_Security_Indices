#Trade----------------------------------------

#Harvard Atlas of Economic Complexity, UN Comtrade, and US Census Bureau Trade Data

#Energy-related product Codes------------------------

##Harvard Greenplexity data -> NB: 4-digit product codes-----------------
greenplexity <- read.csv(paste0(raw_data,"/greenplexity_scrape_full.csv")) %>%
  mutate(gross_export=as.numeric(str_remove_all(gross_export, "[$, ]")),
         Gross.Import=as.numeric(str_remove_all(Gross.Import, "[$, ]")),
         sector=ifelse(sector=="Stone","Minerals",sector)) 

#Convert to standardized tech and supply chain terms
green_codes<-greenplexity %>%
  mutate(phase=ifelse(sector %in% c("Agriculture",
                                    "Minerals",
                                    "Metals"),
                      "Upstream",
                      ifelse(sector %in% c("Chemicals",
                                           "Machinery",
                                           "Electronics"),
                             "Midstream",
                             ifelse(sector=="Vehicles","Midstream",NA))),
         supplyChain=ifelse(grepl("Solar",supplyChain),"Solar",
                            ifelse(grepl("Wind",supplyChain),"Wind",supplyChain)),
         industry=paste(supplyChain,phase)) %>%
  select(supplyChain,nameShortEn,sector,industry,code) %>% distinct()

##Dept of Commerce - Energy Trade Dashboard - 10-digit product codes-------------------
#https://www.trade.gov/data-visualization/us-energy-trade-dashboard
hs_codes<-read.csv('OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/hts_codes_categories.csv')
hs_codes<-hs_codes %>%
  mutate(code_10=substr(HTS.Code,1,10),
         code_6=substr(HTS.Code,1,6),
         code_4=substr(HTS.Code,1,4)) %>%
  select(-Start.Year.Valid,-End.Year.Valid)

#Convert to standardized tech and supply chain terms
sectors <- hs_codes %>%
  mutate(
    Sector_clean = str_squish(Sector),            # "Electricity Infrastructure"
    Sub.Sector   = str_squish(Sub.Sector),
    code_4       = str_trim(code_4),
    code_6       = str_trim(code_6)
  ) %>%
  mutate(
    industry = case_when(
      # --- specific overrides go first ---
      Sector_clean == "Electricity Infrastructure" & code_4 == "2710" ~ "Oil Upstream",
      code_6 == "271019"                                              ~ "Oil Upstream",
      
      # batteries / solar / wind, etc.
      Sub.Sector == "Wind"                         ~ "Wind Midstream",
      str_detect(Sub.Sector, "Solar")              ~ "Solar Midstream",
      str_detect(Sub.Sector, "Battery Inputs")     ~ "Batteries Upstream",
      str_detect(Sub.Sector, "Batteries and Parts")~ "Batteries Midstream",
      
      Sub.Sector %in% c(
        "Lubricating Oils","Quenching Oils","Greases","Mineral Oils",
        "Petroleum Oils: Other","Waste Oils","Fuel Oils: No. 2 & 3",
        "Fuel Oils: Light Oils","Fuel Oils: No. 4","Fuel Oils: Heavy Oils",
        "Kerosene","Petroleum Solids","Gasoline","Naphtha",
        "Motor Fuel","Fuel Oils: Other"
      ) ~ "Oil Upstream",
      
      Sector_clean == "Thermal Power" & Sub.Sector %in% c("Internal Combustion Generating Sets","Generators") ~ "Oil Midstream",
      Sector_clean == "Thermal Power" & Sub.Sector %in% c("Gas","Steam (Coal, Nuclear, Geothermal, Combined Cycle Gas, Etc.)")
      ~ "Coal Midstream; Gas Midstream; Nuclear Midstream",
      
      Sub.Sector == "Crude Oils"                  ~ "Oil Upstream",
      Sub.Sector %in% c("Gas","Ethylene","Propylene","Butylene","Butadiene") ~ "Gas Midstream",
      Sub.Sector %in% c("Propane","Butane","Ethane","Petroleum Gases","Natural Gas") ~ "Gas Upstream",
      str_detect(Sector_clean, "Coal")            ~ "Coal Upstream",
      Sub.Sector == "Nuclear Fuel"                ~ "Nuclear Upstream",
      
      # --- general Electricity Infrastructure rule AFTER overrides ---
      Sector_clean == "Electricity Infrastructure" ~ "Electric Grid Midstream",
      
      Sub.Sector == "Equipment"                   ~ "Nuclear Midstream",
      Sector_clean == "Fossil Energy: Equipment"  ~ "Oil Midstream; Gas Midstream",
      
      TRUE ~ NA_character_
    )
  ) %>%
  separate_rows(industry, sep = "\\s*;\\s*") %>%
  mutate(industry = str_squish(industry))



ev_codes <- tibble(
  HTS.Code = c(
    "870380--CARS BEVS",
    "870360--CARS PHEVS",
    "870370--CARS PHEVS",
    "871160--TWO WHEELERS BEVS",
    "870240--BUSES BEVS",
    "870460--TRUCKS BEVS",
    "870124--TRACTORS BEVS"
  ),
  Sector = "Electric Vehicles",
  Sub.Sector = "EV Midstream",
  code_10 = NA_character_,
  code_6 = c("870380", "870360", "870370", "871160", "870240", "870460", "870124"),
  code_4 = substr(c("870380", "870360", "870370", "871160", "870240", "870460", "870124"), 1, 4),
  industry = "Electric Vehicles Midstream"
)


sectors <- bind_rows(sectors, ev_codes)
# Inspect the result
glimpse(sectors)


##MIT HS Codes for Batteries------------
batt_mit <- read.csv(
  paste0(raw_data, "battery_hscodes_mit.csv"),
  fileEncoding = "latin1",
  stringsAsFactors = FALSE,
  check.names = TRUE
)

batt_codes <- batt_mit %>%
  mutate(tech="Electric Vehicles",
         supply_chain = ifelse(
           substr(as.character(HS.code.), 1, 1)=="2",
           "Upstream","Midstream"
         ),
         industry=paste(tech,supply_chain),
         product=Tag,
         code=HS.code.) %>%
  select(industry,code,product)

#String-based approach with all critical minerals listed in Columbia Universtiy Dashboard
crit_hs <- read.csv(paste0(raw_data,"Columbia University Critical Minerals Dashboard/unique_comtrade.csv"))
critical<-read.csv(paste0(raw_data,"iea_criticalminerals_25.csv")) 

critical_minerals<-critical %>%
  filter(Pillar=="3.1 Cleantech demand by tech",
         !grepl("Other|Total", `Sector.Country`))  %>%
  distinct(Mineral,Sector.Country) %>%
  mutate(tech = case_when(
    str_detect(Sector.Country, regex("^Solar", ignore_case=TRUE))            ~ "Solar",
    str_detect(Sector.Country, regex("^Wind", ignore_case=TRUE))             ~ "Wind",
    str_detect(Sector.Country, regex("Electric vehicles?", ignore_case=TRUE))~ "Electric Vehicles",
    str_detect(Sector.Country, regex("Grid battery storage", ignore_case=TRUE)) ~ "Batteries",
    str_detect(Sector.Country, regex("Electricity networks?", ignore_case=TRUE)) ~ "Electric Grid",
    str_detect(Sector.Country, regex("Hydrogen", ignore_case=TRUE))          ~ "Green Hydrogen",
    str_detect(Sector.Country, regex("Heat pumps?", ignore_case=TRUE))       ~ "Heat Pumps",
    str_detect(Sector.Country, regex("^Coal$", ignore_case=TRUE))            ~ "Coal",
    str_detect(Sector.Country, regex("^Nuclear$", ignore_case=TRUE))         ~ "Nuclear",
    str_detect(Sector.Country, regex("^Oil$", ignore_case=TRUE))             ~ "Oil",
    str_detect(Sector.Country, regex("^Gas$", ignore_case=TRUE))             ~ "Gas",
    str_detect(Sector.Country, regex("Hydroelectric", ignore_case=TRUE))     ~ "Hydroelectric Power",
    str_detect(Sector.Country, regex("Geothermal", ignore_case=TRUE))        ~ "Geothermal",
    TRUE                                                                   ~ NA_character_
  )) %>%
  mutate(Mineral=ifelse(grepl("Graphite",Mineral),"Graphite",Mineral))

minerals_pattern <- paste0(paste0(critical_minerals$Mineral, collapse = "|"),"|","Graphite")

crit_hs_filtered <- crit_hs %>%
  mutate(
    mineral = str_to_sentence(str_extract(
      comtradeDescription,
      regex(minerals_pattern, ignore_case = TRUE)
    )
    )) %>%
  inner_join(critical_minerals,by=c("mineral"="Mineral")) %>%
  mutate(supply_chain=ifelse(str_starts(hscode, "2"),"Upstream","Midstream"),
         industry=paste(tech,supply_chain)) %>%
  select(industry,
         code=hscode,
         product=comtradeDescription)

#Bind together for full list of 4 digit codes
energy_codes_4 <-rbind(sectors %>%
                      rename(code=code_4,
                             product=HTS.Code) %>%
                      select(industry,code,product),
                    green_codes %>%
                      rename(product=nameShortEn) %>%
                      select(industry,code,product)) %>%
  distinct() %>%
  mutate(industry=str_replace_all(industry," NA"," Upstream"),
         industry=str_replace_all(industry,"Battery","Batteries"),
         industry=str_replace_all(industry,"Nuclear Power","Nuclear"),
         industry=str_replace_all(industry,"Critical Minerals","Batteries"))


#Bind together for full list of 6 digit codes
energy_codes_6 <-rbind(sectors %>%
                      rename(code=code_6,
                             product=HTS.Code) %>%
                      select(industry,code,product),
                    batt_codes,
                    crit_hs_filtered)  %>%
  distinct(industry,code,product) %>%
  mutate(industry=str_replace_all(industry," NA"," Upstream"),
         industry=str_replace_all(industry,"Battery","Batteries"),
         industry=str_replace_all(industry,"Nuclear Power","Nuclear"),
         industry=str_replace_all(industry,"Critical Minerals","Batteries"))

write.csv(energy_codes_6,"Downloads/energy_codes_6.csv")


subcat<-read.csv("Downloads/hts_codes_categories_bolstered_final.csv")

check<-energy_codes_6 %>%
  mutate(code_hs=as.numeric(code)) %>%
  left_join(subcat,by=c("code_hs"="HS6"))

#UN Comtrade--------------
library(comtradr)
set_primary_comtrade_key('2940653b9bbe4671b3f7fde2846d14be')

#All Energy Trade Data Import from Comtrade-----------------------------------
country_info_iso <- country_info %>%
  filter(!iso3c %in% c("ASM", "CHI", "GUM", "IMN", "LIE", "MAF", "MCO", "PRI", "XKX"))

subcat<-read.csv(paste0(raw_data,"hts_codes_categories_bolstered_final.csv")) %>%
  mutate(code=as.character(HS6))

# 1) Clean & prep the HS6 codes
codes <- subcat$code %>%
  as.character() %>%
  str_replace_all("\\D", "") %>%        # keep digits only, just in case
  str_pad(width = 6, side = "left", pad = "0") %>%
  na.omit() %>%
  unique()

# 2) Split into chunks by max characters allowed in the commodity_code param
split_by_nchar <- function(x, max_chars = 2500) {
  chunks <- list(); cur <- character(); cur_len <- 0
  for (code in x) {
    add_len <- nchar(code) + ifelse(length(cur) == 0, 0, 1) # comma if not first
    if (cur_len + add_len > max_chars) {
      chunks[[length(chunks) + 1]] <- cur
      cur <- code
      cur_len <- nchar(code)
    } else {
      cur <- c(cur, code)
      cur_len <- cur_len + add_len
    }
  }
  if (length(cur)) chunks[[length(chunks) + 1]] <- cur
  chunks
}

code_chunks <- split_by_nchar(codes, max_chars = 2500)  # conservative margin under 4096

split_vec <- function(x, chunk_size) {
  if (length(x) == 0) return(list(character()))
  split(x, ceiling(seq_along(x) / chunk_size))
}

safe_ct <- purrr::possibly(ct_get_data, otherwise = NULL)

# reporter / partner / code inputs you already have
reporters <- country_info_iso$iso3c
partners  <- "World"
codes     <- code_chunks                 # from your split_by_nchar(...)
years     <- c(2020,2024)
dirs      <- c("export","import")

# ALSO chunk the partner list to keep rows per call below 100k
# (tune chunk_size if you still hit the cap; larger == fewer calls, smaller == safer)
partner_chunks <- split_vec(partners, chunk_size = 50)

# Cartesian product: one reporter × one year × one flow × one code-chunk × one partner-chunk
grid <- tidyr::expand_grid(
  rep  = reporters,
  yr   = years,
  dir  = dirs,
  cc   = codes,
  pch  = partner_chunks
)

library(progress)
pb <- progress_bar$new(
  format = "  :current/:total [:bar] :percent | ETA: :eta | rep=:rep yr=:yr dir=:dir partners=:pn codes=:cn",
  total  = nrow(grid),
  clear  = FALSE, width = 90
)

# Run the queries
res_list <- purrr::pmap(
  grid,
  function(rep, yr, dir, cc, pch) {
    pb$tick(tokens = list(rep = rep, yr = yr, dir = dir,
                          pn = length(pch), cn = length(cc)))
    Sys.sleep(0.5)  # increase if rate-limited
    out <- safe_ct(
      reporter       = rep,
      partner        = pch,
      commodity_code = cc,
      start_date     = yr,
      end_date       = yr,
      flow_direction = dir
    )
    if (is.null(out)) return(NULL)
    
    # Tag the direction if the API payload doesn't include it
    if (!"flow_direction" %in% names(out)) {
      out <- dplyr::mutate(out, flow_direction = dir)
    }
    # (Optional) if there's a 'trade_flow' column, keep only the requested flow
    if ("trade_flow" %in% names(out)) {
      out <- dplyr::filter(out, tolower(trade_flow) == dir)
    }
    
    # Stamp keys to help debugging / dedup if needed
    dplyr::mutate(out, reporter_req = rep, year_req = yr)
  }
)

# Bind and dedupe
res_all <- dplyr::bind_rows(res_list) %>% dplyr::distinct()
write.csv(res,paste0(raw_data,"allied_comtrade_energy_data.csv"))


##All Energy Sectors for the US--------------------

energy_import <- ct_get_data(
  reporter = "USA",
  partner = "World",
  commodity_code = energy_codes_6$code,
  start_date = 2014,
  end_date = 2024,
  flow_direction = 'import'
)


energy_import_clean <-energy_import %>%
  select(reporter_iso,reporter_desc,period,cmd_code,cmd_desc,primary_value) %>%
  rename(imports=primary_value,
         year=period) %>%
  left_join(energy_codes_6,by=c("cmd_code"="code")) %>%
  group_by(year,industry) %>%
  summarize(imports=sum(imports,na.rm=T))

energy_import_change <-energy_import_clean %>%
  pivot_wider(names_from=year,values_from=imports) %>%
  mutate(perc_change=`2024`/`2014`-1) %>%
  select(industry,perc_change) %>%
  arrange(desc(perc_change))

write.csv(energy_import_change,"Downloads/energy_imports.csv")

##All Critical Minerals------------------------------

critmin_import <- ct_get_data(
  reporter = country_info$iso3c,
  partner = "World",
  commodity_code = crit_hs_filtered$hscode,
  start_date = 2024,
  end_date = 2024,
  flow_direction = 'import'
)

critmin_export <- ct_get_data(
  reporter = country_info$iso3c,
  partner = "World",
  commodity_code = crit_hs_filtered$hscode,
  start_date = 2024,
  end_date = 2024,
  flow_direction = 'export'
)

total_import <- ct_get_data(
  reporter = country_info$iso3c,
  partner = "World",
  commodity_code = 'TOTAL',
  start_date = 2024,
  end_date = 2024,
  flow_direction = 'import'
)

total_export <- ct_get_data(
  reporter = country_info$iso3c,
  partner = "World",
  commodity_code = 'TOTAL',
  start_date = 2024,
  end_date = 2024,
  flow_direction = 'export'
)

critmin_trade <-critmin_import %>%
  select(reporter_iso,reporter_desc,cmd_code,cmd_desc,primary_value) %>%
  rename(imports=primary_value) %>%
  left_join(critmin_export %>%
              select(reporter_iso,reporter_desc,cmd_code,cmd_desc,primary_value) %>%
              rename(exports=primary_value),by=c("reporter_iso","reporter_desc","cmd_code","cmd_desc")) %>%
  left_join(country_info %>% select(iso3c,country),by=c("reporter_iso"="iso3c")) %>%
  select(-reporter_desc) %>%
  mutate(imports=ifelse(is.na(imports),0,imports),
         exports=ifelse(is.na(exports),0,exports),
         trade_balance=(exports-imports)/(exports+imports)) %>%
  mutate(
    mineral = str_to_sentence(str_extract(
      cmd_desc,
      regex(minerals_pattern, ignore_case = TRUE)
    )
    )) %>%
  mutate(supply_chain=ifelse(str_starts(cmd_code, "2"),"Upstream","Midstream"))


critmin_hhi <- critmin_trade %>%
  group_by(reporter_iso,country,mineral,supply_chain) %>%
  summarize(exports=sum(exports,na.rm=T)) %>%
  left_join(total_export %>%
              select(reporter_iso,
                     total_exports=primary_value),by=c("reporter_iso")) %>%
  mutate(exp_share=exports/total_exports) %>%
  group_by(mineral,supply_chain) %>%
  mutate(market_share=exports/sum(exports,na.rm=T)) %>%
  mutate(
    # convert percent ??? fraction
    share_frac_24 = exports/sum(exports,na.rm=T),
    # HHI = sum of squared shares
    HHI_24 = sum(share_frac_24^2, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(hhi_index=median_scurve(-HHI_24)
  )




#US v China
#All Energy Trade
country_info_iso <- country_info %>%
  filter(!iso3c %in% c("ASM", "CHI", "GUM", "IMN", "LIE", "MAF", "MCO", "PRI", "XKX"))

# 1) Clean & prep the HS6 codes
codes <- sectors$code_6 %>%
  as.character() %>%
  str_replace_all("\\D", "") %>%        # keep digits only, just in case
  str_pad(width = 6, side = "left", pad = "0") %>%
  na.omit() %>%
  unique()

# 2) Split into chunks by max characters allowed in the commodity_code param
split_by_nchar <- function(x, max_chars = 2500) {
  chunks <- list(); cur <- character(); cur_len <- 0
  for (code in x) {
    add_len <- nchar(code) + ifelse(length(cur) == 0, 0, 1) # comma if not first
    if (cur_len + add_len > max_chars) {
      chunks[[length(chunks) + 1]] <- cur
      cur <- code
      cur_len <- nchar(code)
    } else {
      cur <- c(cur, code)
      cur_len <- cur_len + add_len
    }
  }
  if (length(cur)) chunks[[length(chunks) + 1]] <- cur
  chunks
}

code_chunks <- split_by_nchar(codes, max_chars = 2500)  # conservative margin under 4096

# 3) Pull each chunk (add a tiny pause to be polite to the API)
safe_ct <- purrr::possibly(ct_get_data, otherwise = NULL)

#Trade
#China
res_list_chn<- purrr::map(code_chunks, ~{
  Sys.sleep(0.4)  # adjust if you hit rate limits
  safe_ct(
    reporter       = "CHN",
    partner        = "World",
    commodity_code = .x,
    start_date     = 2014,
    end_date       = 2024,
    flow_direction = "export"
  )
})
chn_energy_exports<-res_list_chn %>%
  discard(is.null) %>%
  dplyr::bind_rows()

chn_exports <- chn_energy_exports %>%
  left_join(energy_codes,by=c("cmd_code"="code6")) %>%
  distinct(reporter_iso,partner_iso,partner_desc,period,industry,cmd_desc,primary_value) %>%
  group_by(reporter_iso,partner_iso,industry,period) %>%
  summarize(exports=sum(primary_value,na.rm=T))

chn_green_exports <- chn_exports %>%
  separate(
    col    = industry,
    into   = c("tech", "supply_chain"),
    sep    = " (?=Downstream|Midstream|Upstream$)"
  ) %>%
  filter(tech %in% c("Solar","Batteries","Wind","Nuclear","Electric Vehicles")) %>%
  group_by(period) %>%
  summarize(exports_chn_green=sum(exports,na.rm=T))


#USA
res_list_usa<- purrr::map(code_chunks, ~{
  Sys.sleep(0.4)  # adjust if you hit rate limits
  safe_ct(
    reporter       = "USA",
    partner        = "World",
    commodity_code = .x,
    start_date     = 2014,
    end_date       = 2024,
    flow_direction = "export"
  )
})

usa_energy_exports<-res_list_usa %>%
  discard(is.null) %>%
  dplyr::bind_rows()

usa_exports <- usa_energy_exports %>%
  left_join(energy_codes,by=c("cmd_code"="code6")) %>%
  distinct(reporter_iso,partner_iso,partner_desc,period,industry,cmd_code,cmd_desc,primary_value) %>%
  group_by(reporter_iso,partner_iso,industry,period) %>%
  summarize(exports=sum(primary_value,na.rm=T)) %>%
  filter(!is.na(industry))

usa_green_exports <- usa_exports %>%
  separate(
    col    = industry,
    into   = c("tech", "supply_chain"),
    sep    = " (?=Downstream|Midstream|Upstream$)"
  ) %>%
  filter(tech %in% c("Solar","Batteries","Wind","Nuclear","Electric Vehicles")) %>%
  group_by(period) %>%
  summarize(exports_usa_green=sum(exports,na.rm=T))

usa_fossil_exports <- usa_exports %>%
  separate(
    col    = industry,
    into   = c("tech", "supply_chain"),
    sep    = " (?=Downstream|Midstream|Upstream$)"
  ) %>%
  filter(tech %in% c("Gas","Oil"),
         supply_chain=="Upstream") %>%
  group_by(period) %>%
  summarize(exports_usa_fossil=sum(exports,na.rm=T)) %>%
  left_join(chn_green_exports,by=c("period")) %>%
  left_join(usa_green_exports,by=c("period"))
write.csv(usa_fossil_exports,"Downloads/exports.csv")

exports<-usa_exports %>%
  filter(period=="2024") %>%
  left_join(chn_exports %>% filter(period=="2024"),by=c("industry")) %>%
  arrange(desc(exports.y)) %>%
  select(industry,exports.x,exports.y) %>%
  write.csv("Downloads/exports_24.csv")
