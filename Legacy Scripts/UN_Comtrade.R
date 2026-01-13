
#All Energy Trade Data Import from Comtrade-----------------------------------
country_info_iso <- country_info %>%
  filter(!iso3c %in% c("ASM", "CHI", "GUM", "IMN", "LIE", "MAF", "MCO", "PRI", "XKX"))

subcat<-read.csv(paste0(raw_data,"consolidated_hs6_energy_tech_long")) %>%
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
#write.csv(res,paste0(raw_data,"allied_comtrade_energy_data.csv"))


#RCA Values
tot <- ct_get_data(
  reporter = "all_countries",
  partner  = "World",      # aggregate partner (default)
  commodity_code = "TOTAL",# all commodities (default)
  flow_direction = c("Export"),
  start_date = 2020,
  end_date   = 2025
)

export_subcomponent_world <-res_all %>%
  left_join(subcat,by=c("cmd_code"="code")) %>%
  filter(flow_direction == "export",
         period=="2024",
         !is.na(tech), !is.na(supply_chain), !is.na(sub_sector),
         tech !="") %>%
  group_by(tech,supply_chain,sub_sector, period) %>%
  summarise(val_world = sum(primary_value, na.rm = TRUE), .groups = "drop_last") 


export_subcomponent <-res_all %>%
  left_join(subcat,by=c("cmd_code"="code")) %>%
  filter(flow_direction == "export", 
         !is.na(tech), !is.na(supply_chain), !is.na(sub_sector),
         tech !="") %>%
  group_by(reporter_iso, partner_iso, tech,supply_chain,sub_sector, period) %>%
  summarise(val = sum(primary_value, na.rm = TRUE), .groups = "drop_last") %>%
  complete(period = as.character(years), fill = list(val = 0)) %>%
  ungroup() %>%
  pivot_wider(names_from = period, values_from = val, names_prefix = "y", values_fill = 0) %>%
  transmute(
    reporter_iso, partner_iso, tech,supply_chain,sub_sector,
    level_last  = .data[[paste0("y", max(years))]],
    level_first = .data[[paste0("y", min(years))]],
    growth      = dplyr::if_else(level_first > 0, (level_last / level_first) - 1, NA_real_)
  ) %>%
  inner_join(tot %>%
               filter(ref_year=="2024") %>%
               select(reporter_iso,total=primary_value),by=c("reporter_iso"="reporter_iso")) %>%
  inner_join(export_subcomponent_world,by=c("tech","supply_chain","sub_sector")) %>%
  cbind(tot %>%
          filter(ref_year=="2024") %>%
          summarize(primary_value=sum(primary_value)) %>%
          select(total_world=primary_value))%>%
  mutate(share=level_last/total,
         world_share=val_world/total_world,
         rca=share/world_share) %>%
  group_by(tech,supply_chain,sub_sector) %>%
  mutate(
    export_index        = safe_scurve(level_last),     # level in last year
    export_growth_index = safe_scurve(growth),         # growth from first -> last
    rca_index=safe_scurve(rca),
    # NA-safe weighted blend: 2 * level + 1 * growth, then re-scaled in (tech, chain)
    ti_num =
      2 * coalesce(export_index,        0) +
      1 * coalesce(export_growth_index, 0) +
      1 * coalesce(rca_index,            0),
    ti_den = 2*(!is.na(export_index)) + 1*(!is.na(export_growth_index))+ 1*(!is.na(rca_index)),
    trade_index_raw = if_else(ti_den > 0, ti_num / ti_den, NA_real_)
  ) %>%
  mutate(trade_index = safe_scurve(trade_index_raw)) %>%
  ungroup() 

subcomponents <-res_all %>%
  filter(reporter_iso=="USA") %>%
  left_join(subcat,by=c("cmd_code"="code")) %>%
  distinct(reporter_iso,reporter_desc,cmd_code,cmd_desc,tech,supply_chain,sub_sector)