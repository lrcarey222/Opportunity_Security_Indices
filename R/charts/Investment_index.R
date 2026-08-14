investment_monitor_path <- file.path(
  raw_data_path, "GCIM_Investment_Capacity_aggregated.xlsx")

gcim <- read_excel(investment_monitor_path,2, skip=2)

investment_index<-readRDS(paste0(processed_dir,"/investment_momentum_tbl.rds"))

library(dplyr)
library(ggplot2)
library(forcats)
library(scales)

countries <- c("India", "Viet Nam", "South Korea", "Japan")
var_to_plot <- "Investment Momentum Index" 

df_plot <- investment_index %>%
  filter(Country %in% countries, variable == var_to_plot) %>%
  group_by(Country, iso3c, tech, supply_chain) %>%
  filter(Year == max(Year, na.rm = TRUE)) %>%   # latest year per Country-tech-supply_chain
  ungroup() %>%
  mutate(
    tech = fct_reorder(tech, value, .fun = sum, .desc = TRUE),
    supply_chain = fct_inorder(supply_chain)
  )

df_plot_wide<-df_plot %>%
  select(Country,tech,supply_chain,value) %>%
  pivot_wider(names_from=supply_chain,values_from=value) 
write.csv(df_plot_wide %>%
            filter(Country=="Japan"),"data/processed/charts/investment_index.csv")


ggplot(df_plot, aes(x = supply_chain, y = tech, fill = value)) +
  geom_tile(color = "white", linewidth = 0.3) +
  facet_wrap(~ Country, nrow = 1) +
  scale_fill_continuous(labels = label_number()) +
  labs(
    title = var_to_plot,
    subtitle = "Latest year shown for each Country × tech × supply_chain",
    x = NULL, y = NULL, fill = "Value"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

ggplot(df_plot, aes(x = supply_chain, y = tech, fill = value)) +
  geom_tile(color = "white", linewidth = 0.3) +
  facet_wrap(~ Country, nrow = 1) +
  scale_fill_continuous(trans = "log1p", labels = label_number()) +
  labs(title = paste0(var_to_plot, " (log1p scale)"), x = NULL, y = NULL, fill = "USD bn") +
  theme_minimal() +
  theme(panel.grid = element_blank())



library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

countries <- c("India","Viet Nam","South Korea","Japan")
var_to_plot <- "Annual Investment Index"   # <-- radar works best with an index

df <- investment_index %>%
  filter(Country %in% countries, variable == var_to_plot) %>%
  group_by(Country, iso3c, tech, supply_chain) %>%
  filter(Year == max(Year, na.rm = TRUE)) %>%
  ungroup()

# Ensure all spokes exist per Country-tech (important for radar closure)
df_complete <- df %>%
  group_by(Country, iso3c, tech) %>%
  complete(supply_chain, fill = list(value = 0)) %>%
  ungroup()

# Close the polygon by repeating the first supply_chain level at the end
sc_levels <- df_complete %>% distinct(supply_chain) %>% pull(supply_chain)
df_closed <- df_complete %>%
  mutate(supply_chain = factor(supply_chain, levels = sc_levels)) %>%
  group_by(Country, iso3c, tech) %>%
  arrange(supply_chain) 

ggplot(df_closed, aes(x = supply_chain, y = value, group = tech, color=tech)) +
  #geom_polygon(alpha = 0.15) +
  geom_line(linewidth = 0.6) +
  coord_polar() +
  facet_wrap(~ Country, nrow = 1) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = paste0(var_to_plot, " (latest year)"),
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )

library(dplyr)
library(ggplot2)
library(scales)

countries <- c("India","Vietnam","South Korea","Japan")
var_to_plot <- "Annual Investment (USD bn, 2024$)"

df_radial <- investment_index %>%
  filter(Country %in% countries, variable == var_to_plot) %>%
  group_by(Country, iso3c, tech, supply_chain) %>%
  filter(Year == max(Year, na.rm = TRUE)) %>%
  ungroup()

ggplot(df_radial, aes(x = supply_chain, y = tech, fill = value)) +
  geom_tile(color = "white", linewidth = 0.25) +
  coord_polar() +
  facet_wrap(~ Country, nrow = 1) +
  scale_fill_continuous(trans = "log1p", labels = label_number()) +
  labs(
    title = paste0(var_to_plot, " (latest year, log1p)"),
    x = NULL, y = NULL, fill = "USD bn"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())


#India Wind Investment
india_wind <- gcim %>%
  filter(Country=="India",
         Technology=="Wind")

library(dplyr)
library(tidyr)

wind_manu_index <- gcim %>%
  filter(Technology == "Wind") %>%
  mutate(
    Country = recode(Country, "USA" = "United States")
  ) %>%
  group_by(Country, Year, Segment) %>%
  summarise(
    investment_usd_bn = sum(Investment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Country, Segment) %>%
  mutate(
    investment_2022 = investment_usd_bn[Year == 2022][1],
    index_2022_100 = 100 * investment_usd_bn / investment_2022
  ) %>%
  ungroup() %>%
  select(Country, Year, Segment, investment_usd_bn, index_2022_100) %>%
  pivot_wider(
    names_from = Segment,
    values_from = c(investment_usd_bn, index_2022_100),
    values_fill = NA
  ) %>%
  mutate(
    man_power_ratio =
      investment_usd_bn_Manufacturing / `investment_usd_bn_Electric Power`,
    man_power_index_ratio =
      index_2022_100_Manufacturing / `index_2022_100_Electric Power`
  )

ggplot(wind_manu_index %>%
         filter(!is.na(man_power_ratio)), aes(x = Year, y = man_power_ratio, group = Country, color=Country)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Wind Manufacturing Investment",
    subtitle = "Indexed to 2022 = 100",
    x = NULL,
    y = "Index (2022 = 100)"
  ) +
  theme_minimal()

ggplot(wind_manu_index%>%
         filter(!is.na(man_power_ratio)), aes(x = Year, y = `investment_usd_bn_Electric Power`, group = Country, fill=Country)) +
  geom_col() +
 
  labs(
    title = "Wind Manufacturing Investment",
    subtitle = "Indexed to 2022 = 100",
    x = NULL,
    y = "Index (2022 = 100)"
  ) +
  theme_minimal()


#India Cleantech Manufacturing Chart

india_man_plot<-investment_index %>%
  filter(Country=="India",
         supply_chain=="Midstream",
         data_type=="raw",
         variable=="Annual Investment (USD bn, 2024$)") %>%
  group_by(Year) %>%
  mutate(share=value/sum(value,na.rm=T))

write.csv(india_man_plot,"data/processed/charts/india_man_year.csv")

#Global Clean Investment and Interest rates
world_plot<-investment_index %>%
  filter(supply_chain=="Midstream",
         data_type=="raw",
         variable=="Annual Investment (USD bn, 2024$)") %>%
  group_by(Year) %>%
  summarize(inv=sum(value,na.rm=T)) %>%
  ungroup() %>%
  mutate(index=inv/inv[Year=="2022"]*100)
write.csv(world_plot,"data/processed/charts/world_man.csv")

#Korean Battery and EV Manufacturing investment

kor_ev_batt<- investment_index %>%
  filter(tech %in% c("Electric Vehicles","Batteries"),
         supply_chain=="Midstream",
         data_type=="raw",
         grepl("Annual Investment",variable)) %>%
  group_by(Year,Country) %>%
  summarize(inv=sum(value,na.rm=T)) %>%
  group_by(Year) %>%
  mutate(share=inv/sum(inv)*100) %>%
  arrange(desc(share))
