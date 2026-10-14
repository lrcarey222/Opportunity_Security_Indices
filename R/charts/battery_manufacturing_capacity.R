library(readr)
library(readxl)
library(dplyr)
library(purrr)
library(tools)
library(janitor)

#BNEF Data

folder <- "C:/Users/LCarey/OneDrive - RMI/Documents - US Program/6_Projects/Clean Regional Economic Development/ACRE/Data/Raw Data/BNEF/Global Battery Supply Chain Facility-Level Production Capacity - March 2025/"

files <- list.files(
  folder,
  pattern = "\\.(csv|xlsx|xls)$",
  full.names = TRUE,
  ignore.case = TRUE
)

read_any_as_text <- function(path) {
  ext <- tolower(file_ext(path))
  
  df <- if (ext == "csv") {
    read_csv(path, col_types = cols(.default = col_character()))
  } else if (ext %in% c("xlsx", "xls")) {
    read_excel(path, col_types = "text")
  } else {
    stop(paste("Unsupported file type:", path))
  }
  
  clean_names(df)
}

all_bnef <- map_dfr(files, read_any_as_text, .id = "file_id") %>%
  mutate(source_file = basename(files[file_id])) %>%
  select(source_file, everything(), -file_id)

capacity<-all_bnef %>%
  group_by(country) %>%
  summarize(battery_capacity=sum(as.numeric(battery_capacity_m_wh),na.rm=T),
            battery_material_capacity=sum(as.numeric(battery_material_capacity_t),na.rm=T),
            separator_capacity=sum(as.numeric(separator_capacity_m_sq_m),na.rm=T))

capacity_subsector <-all_bnef %>%
  filter(country %in% c("Japan",
                        "India",
                        "South Korea",
                        "Vietnam",
                        "United States")) %>%
  group_by(country,plant_type,battery_plant_chemistry) %>%
  summarize(battery_capacity=sum(as.numeric(battery_capacity_m_wh),na.rm=T),
            battery_material_capacity=sum(as.numeric(battery_material_capacity_t),na.rm=T),
            separator_capacity=sum(as.numeric(separator_capacity_m_sq_m),na.rm=T)) %>%
  mutate(capacity=ifelse(battery_capacity>0,battery_capacity,
                         ifelse(battery_material_capacity>0,battery_material_capacity,separator_capacity))) %>%
  group_by(country,plant_type) %>%
  mutate(share=round(capacity/sum(capacity)*100,1))

capacity_chemistry_plot<-capacity_subsector %>%
  ungroup() %>%
  arrange(desc(capacity)) %>%
  filter(plant_type=="CELLS",
       !is.na(battery_plant_chemistry)) %>%
  select(country,battery_plant_chemistry,capacity) %>%
  pivot_wider(names_from=country,values_from=capacity)

write.csv(capacity_chemistry_plot,"data/processed/charts/capacity_chemistry_plot.csv")

cell_chemistry <- capacity_subsector %>%
  filter(plant_type=="CELLS") %>%
  select(country,battery_plant_chemistry,capacity) %>%
  pivot_wider(names_from=battery_plant_chemistry,values_from=capacity)

material_capacity=all_bnef %>%
  mutate(year=substr(announced_date,1,4)) %>%
  filter(!is.na(battery_material_capacity_t )) %>%
  group_by(country,year,status,battery_plant_subsector) %>%
  summarize(battery_material_capacity=sum(as.numeric(battery_material_capacity_t),na.rm=T))
  
india_batt_bnef <- all_bnef %>%
  filter(country=="India",
         !is.na(battery_capacity_m_wh)) %>%
  group_by(state, status) %>%
  summarize(battery_capacity_m_wh=sum(as.numeric(battery_capacity_m_wh),na.rm=T)) %>%
  arrange(desc(battery_capacity_m_wh))

india_batt_owners <- all_bnef %>%
  filter(country=="India",
         !is.na(battery_capacity_m_wh)) %>%
  group_by(owners,state) %>%
  summarize(battery_capacity_m_wh=sum(as.numeric(battery_capacity_m_wh),na.rm=T))

ownership_batt <- all_bnef %>%
  #filter(country %in% c("India","Vietnam","South Korea", "Japan")) %>%
  distinct(country,status,owners)

write.csv(ownership_batt,"C:/Users/LCarey/Downloads/ownership.csv")
ownership_companies<-read_excel("C:/Users/LCarey/Downloads/ownership_with_owner_headquarters.xlsx",1)

ownership_clean <- ownership_companies %>%
  clean_names() %>%
  select(country, status, owners, owner_headquarters) %>%
  distinct()

bnef_clean <- all_bnef %>%
  clean_names() %>%
  mutate(
    plant_country = country,
    investment_status = status,
    battery_capacity_mwh = parse_number(battery_capacity_m_wh),
    battery_capacity_gwh = battery_capacity_mwh / 1000
  )

joined_bnef <- bnef_clean %>%
  left_join(
    ownership_clean,
    by = c("country", "status", "owners")
  )

capacity_by_owner_hq <- joined_bnef %>%
  filter(!is.na(battery_capacity_mwh), !is.na(owner_headquarters)) %>%
  select(
    id,
    name,
    plant_country,
    investment_status,
    owners,
    owner_headquarters,
    battery_capacity_mwh,
    battery_capacity_gwh
  ) %>%
  separate_rows(owners, owner_headquarters, sep = "\\s*\\|\\s*") %>%
  mutate(
    owner_share_pct_raw = parse_number(str_extract(owners, "\\d+(?:\\.\\d+)?%")),
    owner_name = str_trim(str_remove(owners, "\\s*\\d+(?:\\.\\d+)?%$")),
    owner_hq_country = str_trim(str_replace(owner_headquarters, "^.*?[---]\\s*", ""))
  ) %>%
  group_by(id) %>%
  mutate(
    owner_share_pct = if (all(is.na(owner_share_pct_raw))) {
      rep(100 / n(), n())
    } else {
      owner_share_pct_raw
    },
    owner_share_pct = owner_share_pct / sum(owner_share_pct, na.rm = TRUE) * 100,
    allocated_capacity_mwh = battery_capacity_mwh * owner_share_pct / 100,
    allocated_capacity_gwh = battery_capacity_gwh * owner_share_pct / 100
  ) %>%
  ungroup()

# --- 3) Final summary: capacity by plant country, investment status, owner HQ country ---

capacity_summary <- capacity_by_owner_hq %>%
  group_by(plant_country, investment_status, owner_hq_country) %>%
  summarise(
    facilities = n_distinct(id),
    allocated_capacity_mwh = sum(allocated_capacity_mwh, na.rm = TRUE),
    allocated_capacity_gwh = sum(allocated_capacity_gwh, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(allocated_capacity_gwh))

owner_country<-capacity_by_owner_hq %>%
  group_by(owner_hq_country) %>%
  summarise(
    facilities = n_distinct(id),
    allocated_capacity_mwh = sum(allocated_capacity_mwh, na.rm = TRUE),
    allocated_capacity_gwh = sum(allocated_capacity_gwh, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  mutate(share=allocated_capacity_gwh/sum(allocated_capacity_gwh)*100) %>%
  arrange(desc(allocated_capacity_gwh))

write.csv(capacity_summary %>%
            filter(plant_country=="India") %>%
            select(investment_status,
                   owner_hq_country,
                   allocated_capacity_gwh) %>%
            pivot_wider(names_from=owner_hq_country,
                        values_from=allocated_capacity_gwh),
          "data/processed/charts/batt_ownership_india.csv")

write.csv(capacity_summary %>%
            group_by(plant_country, owner_hq_country) %>%
            summarise(
              allocated_capacity_mwh = sum(allocated_capacity_mwh, na.rm = TRUE),
              allocated_capacity_gwh = sum(allocated_capacity_gwh, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            select(plant_country,
                   owner_hq_country,
                   allocated_capacity_gwh) %>%
            pivot_wider(names_from=plant_country,
                        values_from=allocated_capacity_gwh),
          "data/processed/charts/batt_ownership.csv")

#GCIM Data

gcim_fac <- read_excel("data/raw/GCIM_Investment_Capacity.xlsx",3,skip=4)

india_batt<-gcim_fac %>%
  filter(Country=="India",
         Technology=="Batteries",
         Segment=="Manufacturing") %>%
  group_by(Battery_type) %>%
  summarize(Capacity=sum(Capacity,na.rm=T)) %>%
  ungroup() %>%
  mutate(share=Capacity/sum(Capacity))

write.csv(india_batt,
          "data/processed/charts/india_state_batt_capacity_enduse.csv")




viet_ev<-gcim_fac %>%
  filter(Country=="Viet Nam",
         Technology=="Zero Emission Vehicles",
         Segment=="Manufacturing") %>%
  group_by(Investment_Status) %>%
  summarize(Capacity=sum(Capacity,na.rm=T))
