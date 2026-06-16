res_kor <- pull_trade_timeseries(
  catalog = subcat,
  country = c("CHN","KOR"),
  tech = c("Batteries","Electric Vehicles","Electric Grid"),
  supply_chain = "Midstream",
  partners = "World",
  years = c("2008:2025"),
  flow = c("export"),
  frequency="annual"
)

kor_qual <- res_kor %>%
  left_join(subcat %>%
              mutate(cmd_code=as.character(hs6)),by=c("cmd_code"))

library(dplyr)
library(tidyr)
library(stringr)

uv_gap <- kor_qual %>%
  filter(
    ref_year >= 2008,
    ref_year <= 2025,
    reporter_iso %in% c("KOR", "CHN"),
    flow_direction == "export"
  ) %>%
  filter(
    !is.na(fobvalue),
    !is.na(qty),
    qty > 0,
    fobvalue > 0
  ) %>%
  filter(tech %in% c("Batteries","Electric Vehicles","Electric Grid")) %>%
  group_by(
    ref_year,
    reporter_iso,
    reporter_desc,
    tech,
    supply_chain
  ) %>%
  summarise(
    export_value_usd = sum(fobvalue, na.rm = TRUE),
    export_qty_kg = sum(qty, na.rm = TRUE),
    unit_value_usd_per_kg = export_value_usd / export_qty_kg,
    n_hs6 = n_distinct(cmd_code),
    .groups = "drop"
  ) %>%
  select(
    ref_year,
    reporter_iso,
    tech,
    supply_chain,
    export_value_usd,
    export_qty_kg,
    unit_value_usd_per_kg,
    n_hs6
  ) %>%
  pivot_wider(
    names_from = reporter_iso,
    values_from = c(
      export_value_usd,
      export_qty_kg,
      unit_value_usd_per_kg,
      n_hs6
    )
  ) %>%
  mutate(
    uv_gap_kor_minus_chn =
      unit_value_usd_per_kg_KOR - unit_value_usd_per_kg_CHN,
    
    uv_ratio_kor_to_chn =
      unit_value_usd_per_kg_KOR / unit_value_usd_per_kg_CHN,
    
    uv_log_gap_kor_minus_chn =
      log(unit_value_usd_per_kg_KOR) - log(unit_value_usd_per_kg_CHN),
    
    uv_pct_gap_kor_vs_chn =
      100 * (unit_value_usd_per_kg_KOR / unit_value_usd_per_kg_CHN - 1)
  ) %>%
  arrange(
    tech,
    supply_chain,
    ref_year
  )
