library(forcats)

library(dplyr)
library(tidyr)
library(countrycode)

build_surplus_vs_tes_share_ratio <- function(ei, year = 2024, verbose = TRUE) {
  
  # ---------- helper: standard ISO map from EI ----------
  iso_map <- ei %>%
    select(Country, ISO3166_alpha3) %>%
    distinct() %>%
    filter(!is.na(ISO3166_alpha3), ISO3166_alpha3 != "")
  
  # ---------- 1) Consumption side (keyed by ISO) ----------
  cons_wide <- ei %>%
    select(ISO3166_alpha3, Country, Year, Var, Value) %>%
    filter(
      Year == as.character(year),
      Var %in% c("pop", "tes_ej", "tes_gj_pc", "oilcons_ej", "gascons_ej", "coalcons_ej"),
      !is.na(ISO3166_alpha3), ISO3166_alpha3 != ""
    ) %>%
    distinct() %>%
    pivot_wider(
      id_cols = c(ISO3166_alpha3, Country, Year),
      names_from = Var,
      values_from = Value
    ) %>%
    mutate(
      pop = as.numeric(pop),
      tes_ej = as.numeric(tes_ej),
      tes_gj_pc = as.numeric(tes_gj_pc),
      oilcons_ej = as.numeric(coalesce(oilcons_ej, 0)),
      gascons_ej = as.numeric(coalesce(gascons_ej, 0)),
      coalcons_ej = as.numeric(coalesce(coalcons_ej, 0))
    ) %>%
    mutate(
      total_energy_ej = case_when(
        !is.na(tes_ej) ~ tes_ej,
        is.na(tes_ej) & !is.na(tes_gj_pc) & !is.na(pop) ~ (tes_gj_pc * pop) / 1e9, # GJ -> EJ
        TRUE ~ NA_real_
      ),
      fossil_cons_ej = oilcons_ej + gascons_ej + coalcons_ej
    )
  
  if (all(is.na(cons_wide$total_energy_ej))) {
    stop("Couldn't build total_energy_ej. Need tes_ej OR (tes_gj_pc + pop) in `ei`.")
  }
  
  cons_tbl <- cons_wide %>%
    select(ISO3166_alpha3, Country, pop, total_energy_ej,
           oilcons_ej, gascons_ej, coalcons_ej, fossil_cons_ej) %>%
    pivot_longer(
      cols = c(oilcons_ej, gascons_ej, coalcons_ej, fossil_cons_ej),
      names_to = "tech_raw",
      values_to = "tech_consumption_ej"
    ) %>%
    mutate(
      tech = case_when(
        tech_raw == "oilcons_ej" ~ "Oil",
        tech_raw == "gascons_ej" ~ "Gas",
        tech_raw == "coalcons_ej" ~ "Coal",
        tech_raw == "fossil_cons_ej" ~ "Fossil",
        TRUE ~ NA_character_
      ),
      energy_consumption_per_capita_ej = if_else(!is.na(pop) & pop > 0,
                                                 tech_consumption_ej / pop,
                                                 NA_real_),
      tech_consumption_share = if_else(!is.na(total_energy_ej) & total_energy_ej > 0,
                                       tech_consumption_ej / total_energy_ej,
                                       NA_real_),
      tech_consumption_share_pct = 100 * tech_consumption_share
    ) %>%
    select(
      ISO3166_alpha3, Country, tech,
      tech_consumption_ej,
      energy_consumption_per_capita_ej,
      total_energy_ej,
      tech_consumption_share, tech_consumption_share_pct
    )
  
  # ---------- 2) Surplus/deficit side (get ISO reliably, then key by ISO) ----------
  surplus_raw <- import_dependence(ei, year = year) %>%
    filter(
      category == "Energy Imports",
      variable == "Production surplus/deficit",
      data_type == "raw",
      tech %in% c("Oil", "Gas", "Coal", "Fossil")
    ) %>%
    transmute(
      Country,
      tech,
      production_surplus_deficit_ej = as.numeric(value)
    )
  
  # First attempt: map ISO using EI's own country labels
  surplus_tbl <- surplus_raw %>%
    left_join(iso_map, by = "Country") %>%
    mutate(
      # Fallback ONLY where EI-label join failed:
      ISO3166_alpha3 = if_else(
        is.na(ISO3166_alpha3) | ISO3166_alpha3 == "",
        countrycode(Country, origin = "country.name", destination = "iso3c"),
        ISO3166_alpha3
      )
    ) %>%
    filter(!is.na(ISO3166_alpha3), ISO3166_alpha3 != "") %>%
    select(ISO3166_alpha3, tech, production_surplus_deficit_ej)
  
  # ---------- 3) Join on ISO + tech (NOT on Country) ----------
  out <- cons_tbl %>%
    inner_join(surplus_tbl, by = c("ISO3166_alpha3", "tech")) %>%
    mutate(
      surplus_over_tech_consumption = if_else(tech_consumption_ej > 0,
                                              production_surplus_deficit_ej / tech_consumption_ej,
                                              NA_real_),
      surplus_vs_share = if_else(!is.na(tech_consumption_share) & tech_consumption_share > 0,
                                       surplus_over_tech_consumption * tech_consumption_share,
                                       NA_real_)
    ) %>%
    select(
      ISO3166_alpha3, Country, tech,
      production_surplus_deficit_ej,
      tech_consumption_ej,
      total_energy_ej,
      energy_consumption_per_capita_ej,
      tech_consumption_share, tech_consumption_share_pct,
      surplus_over_tech_consumption,
      surplus_vs_share
    )
  
  # ---------- 4) Diagnostics so you KNOW what's being dropped ----------
  if (isTRUE(verbose)) {
    cons_isos <- cons_tbl %>% distinct(ISO3166_alpha3)
    surplus_isos <- surplus_tbl %>% distinct(ISO3166_alpha3)
    out_isos <- out %>% distinct(ISO3166_alpha3)
    
    dropped_surplus <- surplus_isos %>% anti_join(out_isos, by = "ISO3166_alpha3")
    dropped_cons <- cons_isos %>% anti_join(out_isos, by = "ISO3166_alpha3")
    
    if ("USA" %in% cons_tbl$ISO3166_alpha3 && !"USA" %in% out$ISO3166_alpha3) {
      message("?????? USA is in consumption table but missing after join. This means surplus_tbl has no USA rows (likely country-name mismatch upstream).")
    }
    
    message("Join coverage (unique ISO3): ",
            "cons=", nrow(cons_isos),
            " surplus=", nrow(surplus_isos),
            " out=", nrow(out_isos),
            " | dropped_from_surplus=", nrow(dropped_surplus),
            " dropped_from_cons=", nrow(dropped_cons))
    
    if (nrow(dropped_surplus) > 0) {
      message("ISO3 present in surplus but not in output (first 10): ",
              paste(head(dropped_surplus$ISO3166_alpha3, 10), collapse = ", "))
    }
    if (nrow(dropped_cons) > 0) {
      message("ISO3 present in consumption but not in output (first 10): ",
              paste(head(dropped_cons$ISO3166_alpha3, 10), collapse = ", "))
    }
  }
  
  out
}

# Example:
df_compare <- build_surplus_vs_tes_share_ratio(ei, year = 2024, verbose = TRUE)
# df_compare %>% filter(ISO3166_alpha3 == "USA")

df_fossil <- df_compare %>%
  inner_join(gdp_data %>%
              filter(year=="2024") %>%
              select(iso3c,NY.GDP.MKTP.CD) %>%
              rename(gdp="NY.GDP.MKTP.CD") %>%
               slice_max(order_by=gdp,n=75),by=c("ISO3166_alpha3"="iso3c")) %>%
  filter(tech=="Fossil") %>%
  arrange(surplus_vs_share)

library(dplyr)
library(ggplot2)
library(forcats)

# df is your output from build_surplus_vs_tes_share_ratio(), filtered to tech == "Fossil"
df_fossil <- df_compare %>%
  inner_join(gdp_data %>%
               filter(year=="2024") %>%
               select(iso3c,NY.GDP.MKTP.CD) %>%
               rename(gdp="NY.GDP.MKTP.CD") %>%
               slice_max(order_by=gdp,n=75),by=c("ISO3166_alpha3"="iso3c")) %>%
  #filter(tech == "Gas") %>%
  mutate(
    import_dependence = pmax(0, -production_surplus_deficit_ej / tech_consumption_ej),
    import_dependence = pmin(import_dependence, 1),  # optional cap at 1
    fossil_share = tech_consumption_share,           # already fossil_cons/tes
    exposure = import_dependence * fossil_share
  )

write.csv(df_fossil,"data/processed/charts/gas_exposure.csv")

# If you have region/income groups, join them in; otherwise just use exposure deciles
df_plot <- df_fossil %>%
  mutate(
    exposure_decile = ntile(exposure, 10),
    exposure_decile = factor(exposure_decile, levels = 1:10, labels = paste0("D", 1:10))
  )

ggplot(df_plot, aes(x = exposure_decile, y = fct_reorder(ISO3166_alpha3, exposure), fill = exposure)) +
  geom_tile() +
  labs(
    x = "Exposure decile",
    y = "Countries (ISO3, ordered by exposure)",
    fill = "Fossil import exposure\n(import dependence × fossil share)"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

top_n <- 40

df_top <- df_fossil %>%
  arrange(desc(exposure)) %>%
  slice_head(n = top_n) %>%
  mutate(Country = forcats::fct_reorder(Country, exposure))

ggplot(df_top, aes(x = Country)) +
  geom_col(aes(y = exposure)) +
  geom_point(aes(y = import_dependence), size = 2) +
  geom_point(aes(y = fossil_share), size = 2) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Value (0-1)",
    title = "Fossil import exposure",
    subtitle = "Bar = import dependence × fossil share; dots = components"
  ) +
  theme_minimal()


library(dplyr)
library(tidyr)
library(httr)
library(jsonlite)
library(stringr)

target_year <- 2024

oil_shock  <- 0.30
gas_shock  <- 0.40
coal_shock <- 0.20

# -----------------------------
# 1) Helper: World Bank API pull
# -----------------------------
pull_wb_indicator <- function(indicator, year, per_page = 20000) {
  url <- paste0(
    "https://api.worldbank.org/v2/country/all/indicator/", indicator,
    "?format=json",
    "&per_page=", per_page,
    "&date=", year
  )
  
  res <- httr::RETRY(
    "GET",
    url,
    httr::timeout(60),
    times = 5,
    pause_base = 2,
    pause_cap = 20
  )
  httr::stop_for_status(res)
  
  txt <- httr::content(res, as = "text", encoding = "UTF-8")
  js <- jsonlite::fromJSON(txt, flatten = TRUE)
  
  if (length(js) < 2 || is.null(js[[2]])) {
    return(tibble())
  }
  
  js[[2]] %>%
    transmute(
      iso3c = countryiso3code,
      country = country.value,
      Year = as.integer(date),
      value = as.numeric(value)
    )
}

# -----------------------------
# 2) Build fossil import bill from repo trade data
# -----------------------------
energy_codes <- trade_core_build_energy_codes(subcat, include_sub_sector = FALSE)

trade_energy <- trade_core_build_comtrade_trade(
  comtrade_data = comtrade_energy_trade,
  energy_codes  = energy_codes,
  year          = target_year
)

fossil_bill <- trade_energy %>%
  filter(tech %in% c("Oil", "Gas", "Coal")) %>%
  group_by(reporter_iso, tech) %>%
  summarise(import_bill_usd = sum(import, na.rm = TRUE), .groups = "drop") %>%
  mutate(import_bill_usd = pmax(import_bill_usd, 0))

fossil_bill_wide <- fossil_bill %>%
  mutate(tech = str_to_lower(tech)) %>%
  pivot_wider(
    names_from = tech,
    values_from = import_bill_usd,
    values_fill = 0,
    names_prefix = "import_bill_"
  ) %>%
  mutate(
    annual_fossil_import_bill_usd =
      import_bill_oil + import_bill_gas + import_bill_coal,
    shocked_increment_bill_usd =
      import_bill_oil  * oil_shock +
      import_bill_gas  * gas_shock +
      import_bill_coal * coal_shock
  ) %>%
  rename(iso3c = reporter_iso)

# -----------------------------
# 3) Pull macro variables from World Bank
# -----------------------------
wb_reserves <- pull_wb_indicator("FI.RES.TOTL.CD", target_year) %>%
  transmute(iso3c, fx_reserves_usd = value)

wb_exports <- pull_wb_indicator("NE.EXP.GNFS.CD", target_year) %>%
  transmute(iso3c, exports_usd = value)

wb_current_account <- pull_wb_indicator("BN.CAB.XOKA.CD", target_year) %>%
  transmute(iso3c, current_account_balance_usd = value)

# -----------------------------
# 4) Assemble metrics
# -----------------------------
fossil_vulnerability_metrics <- fossil_bill_wide %>%
  left_join(wb_reserves, by = "iso3c") %>%
  left_join(wb_exports, by = "iso3c") %>%
  left_join(wb_current_account, by = "iso3c") %>%
  left_join(country_info %>% select(iso3c, country) %>% distinct(), by = "iso3c") %>%
  mutate(
    fossil_bill_to_reserves =
      annual_fossil_import_bill_usd / fx_reserves_usd,
    
    fossil_bill_to_exports =
      annual_fossil_import_bill_usd / exports_usd,
    
    # Not receipts, but still useful macro pressure metric:
    fossil_bill_to_abs_current_account =
      annual_fossil_import_bill_usd / abs(current_account_balance_usd),
    
    shock_bill_to_reserves =
      shocked_increment_bill_usd / fx_reserves_usd
  ) %>%
  select(
    iso3c, country,
    import_bill_oil, import_bill_gas, import_bill_coal,
    annual_fossil_import_bill_usd,
    shocked_increment_bill_usd,
    fx_reserves_usd,
    exports_usd,
    current_account_balance_usd,
    fossil_bill_to_reserves,
    fossil_bill_to_exports,
    fossil_bill_to_abs_current_account,
    shock_bill_to_reserves
  )

# -----------------------------
# 5) Diagnostics
# -----------------------------
missing_macro <- fossil_vulnerability_metrics %>%
  filter(
    is.na(fx_reserves_usd) |
      is.na(exports_usd) |
      is.na(current_account_balance_usd)
  ) %>%
  select(iso3c, country, fx_reserves_usd, exports_usd, current_account_balance_usd)

fossil_vulnerability_metrics
missing_macro