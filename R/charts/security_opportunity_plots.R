# security_opportunity_plots 
country1="India"
group1 <- c("India","Viet Nam","South Korea","Japan")


#Energy Access------

#Energy consumption per capita

plot1<-energy_access_tbl %>%
  left_join(country_info %>%
              select(iso3c,region)) %>%
  filter(region %in% c("South Asia",
                       "East Asia & Pacific")) %>%
  filter(variable=="Energy consumption per capita",
         data_type=="raw") %>%
  left_join(country_info %>%
              select(iso3c,region)) %>%
  filter(region %in% c("South Asia",
                       "East Asia & Pacific")) %>%
  arrange(desc(value)) %>%
  select(Country,tech,value) %>%
  pivot_wider(names_from=Country,values_from=value)

write.csv(plot1,"data/processed/charts/energy_access_asia.csv")

#Reserves

plot2<-reserves_tbl %>%
  left_join(country_info %>%
              select(iso3c,region),by=c("ISO3166_alpha3"="iso3c")) %>%
  filter(region %in% c("South Asia",
                       "East Asia & Pacific")) %>%
  filter(data_type=="index") %>%
  left_join(country_info %>%
              select(iso3c,region)) %>%
  filter(region %in% c("South Asia",
                       "East Asia & Pacific")) %>%
  distinct(Country,tech,value) %>%
  filter(!is.na(value),
         tech %in% techs) %>%
  arrange(desc(value)) %>%
  select(Country,tech,value) %>%
  pivot_wider(names_from=tech,values_from=value)

write.csv(plot2,"data/processed/charts/reserves_asia.csv")


#Commodity Price Volatility
clean_sub_sector <- function(x) {
  x %>%
    str_replace_all("_", " ") %>%
    str_squish() %>%
    stringi::stri_trans_totitle() %>%
    # fixes for acronyms / known tokens
    str_replace_all("\\bLng\\b", "LNG") %>%
    str_replace_all("\\bEu\\b", "EU") %>%
    str_replace_all("\\bWti\\b", "WTI") %>%
    str_replace_all("\\bApsp\\b", "APSP") %>%
    str_replace_all("\\bHenry Hub\\b", "Henry Hub") %>%
    str_replace_all("_", " ")
}

plot_prices <- energy_prices_tbl %>%
  filter(Country=="Australia", data_type=="raw") %>% 
  select(-explanation) %>%
  pivot_wider(names_from=variable,values_from=value) %>%
  group_by(sub_sector) %>%
  summarise(
    tech  = str_c(sort(unique(tech)), collapse = ", "),
    price_volatility = first(mean_price_volatility),
    latest_price = first(latest_price),
    yoy_price_change_pct = first(yoy_price_change_pct),
    .groups = "drop"
  ) %>% 
  distinct(sub_sector,price_volatility,latest_price,yoy_price_change_pct,tech)%>% 
  arrange(desc(price_volatility))

build_commodity_use_map <- function() {
  tribble(
    ~sub_sector, ~civilian_uses, ~defense_uses, ~notes,
    
    # Fossil fuels / energy carriers
    "Oil_APSP",  "Transport fuels; petrochemical feedstocks (plastics, solvents); industrial heat", "Military aviation/ground/naval fuels; base power backup; feedstocks for polymers/composites", "Oil series variants treated separately; same use profile.",
    "Oil_Brent", "Transport fuels; petrochemical feedstocks; industrial heat", "Military fuels; base power backup; feedstocks for materials", "Benchmark crude.",
    "Oil_WTI",   "Transport fuels; petrochemical feedstocks; industrial heat", "Military fuels; base power backup; feedstocks for materials", "US benchmark crude.",
    "Dubai_Crude","Transport fuels; petrochemical feedstocks; industrial heat","Military fuels; base power backup; feedstocks for materials","Middle East benchmark crude.",
    
    "Natural_Gas_Index","Electricity + heat generation; industrial process heat; fertilizer/ammonia and chemicals", "Base power generation; industrial supply chains for chemicals/materials; energy security for domestic production", "Index series.",
    "Natural_Gas_EU",   "Electricity + heat; industrial process heat; chemicals", "Base power resilience; industrial chemicals/materials supply", "EU price series.",
    "Natural_Gas_Henry_Hub","Electricity + heat; industrial process heat; chemicals", "Base power resilience; industrial chemicals/materials supply", "US price series.",
    "LNG",        "Global traded gas for power/heat; industrial energy supply", "Logistics fuel diversification; base power resilience in import-dependent theaters", "Asia LNG series in PCPS.",
    "Propane",    "Space heating; industrial energy; petrochemical feedstock", "Portable/remote energy supply; contingency heating/power; logistics flexibility", "Often co-moves with NGL markets.",
    "Coal",       "Power generation (where used); metallurgical coke/steel chain", "Steel supply chain resilience for vehicles/ships/industrial base", "Coal is more about industrial base/steel than frontline fuel.",
    
    # Nuclear fuel input
    "Uranium",    "Nuclear power fuel; medical isotopes supply chain (indirect)", "Naval nuclear propulsion (where applicable); strategic deterrence enterprise (high-level)", "Keep wording high-level; avoid operational detail.",
    
    # Battery / transition minerals and related inputs
    "Lithium",    "Li-ion batteries (EVs, consumer electronics, grid storage)", "Portable power for communications/unmanned systems; energy-dense storage for deployed ops (high-level)", "Key for Li-based chemistries.",
    "Cobalt",     "Battery cathodes; high-temp superalloys; catalysts", "High-temp alloys for aerospace/propulsion; high-performance components (high-level)", "Often a superalloy + cathode metal.",
    "Nickel",     "Stainless steel; batteries (high-Ni cathodes); superalloys", "Aerospace/propulsion alloys; corrosion-resistant components (high-level)", "Ni is both structural + battery.",
    "Manganese",  "Steel alloys; some battery cathodes (NMC/LMFP)", "Steel/alloying for vehicles/shipbuilding; industrial base inputs (high-level)", "Broad structural relevance.",
    "Copper",     "Power grid wiring; motors; electronics; plumbing", "Electronics and power distribution; vehicles/ships wiring; munitions-related alloys (high-level)", "Critical electrical conductor.",
    "Aluminum",   "Lightweight structures; vehicles; aircraft; packaging; power lines", "Airframes/vehicles; lightweight structures; power systems (high-level)", "Structural + conductivity.",
    "Chromium",   "Stainless steel; corrosion-resistant coatings; alloying", "Armor/vehicle steels; corrosion-resistant components (high-level)", "Primarily alloying/plating.",
    "Rare_Earths", "Permanent magnets for motors/wind; electronics; catalysts", "Precision guidance/motors/actuators; radar/electronics components (high-level)", "Think NdPr magnets (civil + defense).",
    "Silicon",    "Solar PV; electronics; (some) semiconductor feedstocks; silicones", "Microelectronics and sensors supply chains (high-level); PV for expeditionary power (indirect)", "PCPS silicon is usually metallurgical-grade; still useful proxy.",
    "Zinc",       "Galvanizing steel; alloys; batteries (limited)", "Corrosion protection for vehicles/structures; industrial base inputs (high-level)", "More about corrosion control than energy tech.",
    
    # New additions from your PCPS snapshot / expanded coverage
    "Iron_Ore",   "Steelmaking feedstock; construction and manufacturing", "Ships/vehicles/industrial base steel; infrastructure (high-level)", "Useful proxy for steel intensity across many techs.",
    "Vanadium",   "Steel strengthening alloys; vanadium redox flow batteries", "High-strength steels for vehicles/structures (high-level)", "Good proxy for flow batteries + specialty steels.",
    "Molybdenum", "High-strength steels; catalysts; industrial equipment", "High-strength alloys for aerospace/vehicles; industrial base (high-level)", "Often alloying for toughness/heat tolerance.",
    "Lead",       "Lead-acid batteries; radiation shielding (industrial)", "Legacy batteries; shielding applications (high-level); some munitions relevance (high-level)", "Keep general; avoid specifics.",
    "Tin",        "Solder for electronics; plating; alloys", "Electronics assembly/repair; supply chain for ruggedized systems (high-level)", "Solder is the main channel.",
    
    # Phosphate / fertilizer-linked inputs (use as proxies; keep conservative)
    "Diammonium_Phosphate", "Fertilizer; phosphate chemical chain (proxy for phosphate availability)", "Indirect relevance via industrial chemicals; food security / logistics (high-level)", "Use as a proxy for phosphate inputs (e.g., LFP) only if you choose to.",
    "Urea",                 "Fertilizer; chemical manufacturing", "Indirect relevance via industrial chemical supply chains (high-level)", "Avoid detailing energetic-material pathways.",
    "Potassium_Fertilizer", "Fertilizer; agricultural inputs", "Indirect relevance via food security/logistics (high-level)", "Not a direct defense material."
  )
}

commodity_use_map <- build_commodity_use_map()

units<-tibble::tribble(
  ~sub_sector,                  ~unit_description,
  "Lithium",                    "USD per metric tonne",
  "Cobalt",                     "USD per metric tonne",
  "Molybdenum",                 "USD per metric tonne",
  "Nickel",                     "USD per metric tonne",
  "Chromium",                   "USD per metric tonne",
  "Vanadium",                   "USD per metric tonne",
  "Copper",                     "USD per metric tonne",
  "Rare Earths",                "USD per metric tonne",
  "Propane",                    "US cents per gallon",
  "Zinc",                       "USD per metric tonne",
  "Manganese",                  "USD per metric tonne",
  "Silicon",                    "USD per metric tonne",
  "Natural Gas Index",          "Indicator-native IMF unit",
  "Diammonium Phosphate",       "USD per metric tonne",
  "Natural Gas EU",             "USD per MMBtu",
  "Natural Gas Henry Hub",      "USD per MMBtu",
  "Iron Ore",                   "USD per metric tonne",
  "LNG",                        "USD per MMBtu",
  "Uranium",                    "USD per metric tonne",
  "Coal",                       "USD per metric tonne",
  "Oil Brent",                  "USD per barrel",
  "Oil WTI",                    "USD per barrel",
  "Oil APSP",                   "USD per barrel",
  "Dubai Crude",                "USD per barrel"
)


plot_prices_clean <- plot_prices %>%
  left_join(commodity_use_map, by = "sub_sector") %>%
  mutate(sub_sector = clean_sub_sector(sub_sector))%>%
  left_join(units,by=c("sub_sector")) %>%
  mutate(price=paste0(round(latest_price,1)," (",unit_description,")")) %>%
  select(sub_sector,price_volatility,yoy_price_change_pct,price,tech,civilian_uses,defense_uses)


write.csv(plot_prices_clean,"data/processed/charts/commodity_price_volatility.csv")

plot_volatility <- energy_prices_tbl %>%
  filter(Country=="Australia", data_type=="raw",
         grepl("price_volatility_",variable)) %>% 
  select(-explanation) %>%
  distinct(sub_sector,variable,value) %>%
  arrange(desc(value)) %>%
  pivot_wider(names_from=sub_sector,values_from=value) 

write.csv(plot_volatility,"data/processed/charts/commodity_price_volatility_years.csv")
