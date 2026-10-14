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

uv_gap_hs6_weighted <- kor_qual %>%
  filter(tech %in% c("Batteries","Electric Vehicles","Electric Grid")) %>%
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
  group_by(
    ref_year,
    reporter_iso,
    tech,
    supply_chain,
    sub_sector,
    hs6
  ) %>%
  summarise(
    export_value_usd = sum(fobvalue, na.rm = TRUE),
    export_qty_kg = sum(qty, na.rm = TRUE),
    unit_value_usd_per_kg = export_value_usd / export_qty_kg,
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = reporter_iso,
    values_from = c(export_value_usd, export_qty_kg, unit_value_usd_per_kg)
  ) %>%
  filter(
    !is.na(unit_value_usd_per_kg_KOR),
    !is.na(unit_value_usd_per_kg_CHN),
    unit_value_usd_per_kg_KOR > 0,
    unit_value_usd_per_kg_CHN > 0
  ) %>%
  mutate(
    hs6_log_gap_kor_minus_chn =
      log(unit_value_usd_per_kg_KOR) - log(unit_value_usd_per_kg_CHN),
    
    combined_export_value =
      coalesce(export_value_usd_KOR, 0) + coalesce(export_value_usd_CHN, 0)
  ) %>%
  group_by(ref_year, tech, supply_chain, 
           sub_sector
           ) %>%
  summarise(
    weighted_log_gap_kor_minus_chn =
      weighted.mean(hs6_log_gap_kor_minus_chn, combined_export_value, na.rm = TRUE),
    
    weighted_pct_gap_kor_vs_chn =
      100 * (exp(weighted_log_gap_kor_minus_chn) - 1),
    
    n_matched_hs6 = n_distinct(hs6),
    
    kor_exports_usd = sum(export_value_usd_KOR, na.rm = TRUE),
    chn_exports_usd = sum(export_value_usd_CHN, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  arrange(tech, supply_chain,
          sub_sector, 
          ref_year)

uv_gap_hs6_weighted_smoothed <- uv_gap_hs6_weighted %>%
  group_by(tech, supply_chain, 
           sub_sector
           ) %>%
  arrange(ref_year, .by_group = TRUE) %>%
  mutate(
    weighted_log_gap_kor_minus_chn_3yr =
      slide_dbl(
        weighted_log_gap_kor_minus_chn,
        mean,
        .before = 1,
        .after = 1,
        .complete = FALSE,
        na.rm = TRUE
      ),
    
    weighted_pct_gap_kor_vs_chn_3yr =
      100 * (exp(weighted_log_gap_kor_minus_chn_3yr) - 1)
  ) %>%
  ungroup()

ggplot(data=uv_gap_hs6_weighted_smoothed %>%
         filter(sub_sector %in% c("Rare Earth Separation & Compounds",
                                  "Battery Packs, Parts & BMS",
                                  "Electric Drive Units & Motors" ,
                                  "Transformers" ,
                                  "Grid Automation & Control Equipment" )),aes(x=ref_year,y=weighted_pct_gap_kor_vs_chn_3yr,color=sub_sector))+geom_line()+theme_minimal()
#+theme(legend.position="none")


# ============================================================================
# Korea vs China - export unit-value gap, time series 2008-2025
# Grouped by tech × supply_chain × sub_sector
#
# Input : kor_qual  (Comtrade-style tibble already in your environment)
# Output: kor_chn_unit_value_gap.csv          (tidy gap series - main deliverable)
#         kor_chn_unit_value_gap.png          (faceted quick-look)
#         kor_chn_unit_value_gap_matched.csv  (composition-controlled variant)
#         kor_chn_gap_summary.csv             ("where is China catching up fastest")
# ============================================================================

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

stopifnot(exists("kor_qual"))

# ---- 0. Parameters ---------------------------------------------------------
group_vars     <- c("tech", "supply_chain", "sub_sector")  # drop sub_sector to roll up
yr_min         <- 2008
yr_max         <- 2025
essential_only <- FALSE     # TRUE = restrict to essential == TRUE
min_value_usd  <- 0         # optional FOB floor per HS6 line to cut noise (e.g. 1e5)
ratio_clip     <- c(0.02, 50)  # trim absurd line ratios in the matched variant only

# ---- 1. Filter to comparable "export ??? World" records ----------------------
base <- kor_qual %>%
  filter(
    reporter_iso %in% c("KOR", "CHN"),
    flow_code   == "X",        # exports
    partner_iso == "W00",      # to World (partner_code 0)
    ref_year >= yr_min, ref_year <= yr_max
  ) %>%
  mutate(
    reporter = if_else(reporter_iso == "KOR", "Korea", "China"),
    # consistent quantity in kg: prefer net_wgt, fall back to qty when unit is kg
    qty_kg = case_when(
      !is.na(net_wgt) & net_wgt > 0                 ~ net_wgt,
      qty_unit_abbr == "kg" & !is.na(qty) & qty > 0 ~ qty,
      TRUE                                          ~ NA_real_
    ),
    value_usd = primary_value
  ) %>%
  filter(!is.na(qty_kg), qty_kg > 0,
         !is.na(value_usd), value_usd > min_value_usd)

if (essential_only) base <- base %>% filter(essential %in% TRUE)

# ---- 1b. De-duplicate -------------------------------------------------------
# Your data carries duplicate rows where the SAME (reporter, year, hs6, sub_sector)
# appears twice differing only in `essential` (e.g. 253090, 262019). Summing those
# would double-count value AND weight. distinct() on the analytic columns collapses
# them while preserving any HS6 that legitimately maps to >1 sub_sector.
n_before <- nrow(base)
records <- base %>%
  distinct(reporter, ref_year, hs6,
           !!!rlang::syms(group_vars), value_usd, qty_kg)
message("De-dup: removed ", n_before - nrow(records),
        " duplicate rows (", nrow(records), " analytic records kept).")

# ---- 2. Group-level unit value (volume-weighted USD/kg) --------------------
# UV = ??Value / ??Weight within group-year. Volume-weighting makes the group UV
# robust to per-line outliers (a 1-kg misreport gets ~zero weight).
grp_uv <- records %>%
  group_by(across(all_of(group_vars)), ref_year, reporter) %>%
  summarise(
    value_usd = sum(value_usd, na.rm = TRUE),
    qty_kg    = sum(qty_kg,    na.rm = TRUE),
    n_hs6     = n_distinct(hs6),
    .groups = "drop"
  ) %>%
  mutate(uv_usd_per_kg = value_usd / qty_kg)

# ---- 3. Pivot to Korea vs China and compute the gap ------------------------
gap <- grp_uv %>%
  pivot_wider(
    id_cols     = all_of(c(group_vars, "ref_year")),
    names_from  = reporter,
    values_from = c(uv_usd_per_kg, value_usd, n_hs6)
  ) %>%
  filter(!is.na(uv_usd_per_kg_Korea), !is.na(uv_usd_per_kg_China),
         uv_usd_per_kg_China > 0) %>%            # keep group-years both report
  mutate(
    gap_ratio      = uv_usd_per_kg_Korea / uv_usd_per_kg_China,  # >1 = Korea premium
    gap_diff_usdkg = uv_usd_per_kg_Korea - uv_usd_per_kg_China,
    kr_premium_pct = (gap_ratio - 1) * 100,
    log_gap        = log(gap_ratio)
  ) %>%
  arrange(across(all_of(group_vars)), ref_year)

write_csv(gap, "kor_chn_unit_value_gap.csv")
message("Wrote ", nrow(gap), " group-year rows. Year coverage: ",
        paste(range(gap$ref_year), collapse = "-"))

# ---- 4. Quick-look chart (facet on the finest grouping var) ----------------
facet_var <- tail(group_vars, 1)
p <- ggplot(gap, aes(ref_year, gap_ratio)) +
  geom_hline(yintercept = 1, linewidth = .3, colour = "grey60") +
  geom_line(colour = "#ED9218", linewidth = .7) +
  geom_point(colour = "#ED9218", size = 1) +
  facet_wrap(vars(.data[[facet_var]]), scales = "free_y") +
  scale_x_continuous(breaks = seq(yr_min, yr_max, 4)) +
  labs(
    title    = "Korea's export unit-value premium over China",
    subtitle = "Korean \u00f7 Chinese export unit value (USD/kg). 1.0 = parity; a falling line = China catching up.",
    x = NULL, y = "Korea \u00f7 China unit value",
    caption  = "Source: UN Comtrade via kor_qual. Volume-weighted USD/kg."
  ) +
  theme_minimal(base_size = 11)
ggsave("kor_chn_unit_value_gap.png", p, width = 12, height = 8, dpi = 150)

# ============================================================================
# 5. OPTIONAL - composition-controlled gap (matched HS6 lines)
#    Compares only HS6 lines BOTH countries export in a given group-year, then
#    takes a trade-weighted geometric mean of the line-level KR/CN ratios. This
#    strips out the "Korea just exports a richer mix within the sub-sector"
#    effect - the critique a sharp reader will raise about the simple version.
# ============================================================================
matched <- records %>%
  mutate(uv = value_usd / qty_kg) %>%
  select(all_of(group_vars), hs6, ref_year, reporter, uv, value_usd) %>%
  pivot_wider(names_from = reporter, values_from = c(uv, value_usd)) %>%
  filter(!is.na(uv_Korea), !is.na(uv_China), uv_Korea > 0, uv_China > 0) %>%
  mutate(line_ratio = uv_Korea / uv_China,
         w          = value_usd_Korea + value_usd_China) %>%
  filter(line_ratio >= ratio_clip[1], line_ratio <= ratio_clip[2]) %>%
  group_by(across(all_of(group_vars)), ref_year) %>%
  summarise(
    gap_ratio_matched = exp(weighted.mean(log(line_ratio), w, na.rm = TRUE)),
    n_matched_hs6     = n(),
    .groups = "drop"
  ) %>%
  arrange(across(all_of(group_vars)), ref_year)

write_csv(matched, "kor_chn_unit_value_gap_matched.csv")

# ============================================================================
# 6. OPTIONAL - "where is China catching up fastest?"  (deck-picking aid)
#    First vs latest year per group: starting premium, latest premium, and the
#    average annual change in the ratio. Most-negative slope = fastest erosion.
# ============================================================================
summary_tbl <- gap %>%
  group_by(across(all_of(group_vars))) %>%
  filter(n() >= 2) %>%
  summarise(
    yr_first      = min(ref_year),
    yr_last       = max(ref_year),
    ratio_first   = gap_ratio[which.min(ref_year)],
    ratio_last    = gap_ratio[which.max(ref_year)],
    ratio_chg_pa  = (ratio_last - ratio_first) / (yr_last - yr_first),
    kr_value_last = value_usd_Korea[which.max(ref_year)],
    .groups = "drop"
  ) %>%
  arrange(ratio_chg_pa)            # fastest-closing gaps at the top

write_csv(summary_tbl, "kor_chn_gap_summary.csv")
message("Top sub-sectors where Korea's premium is eroding fastest:")
print(utils::head(summary_tbl, 10))
