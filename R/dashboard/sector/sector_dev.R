# Development & finance of Sector Insights (theme `development`; chunk sector-dev, contract DEV_SPEC):
# the raw data behind the Development Potential index and behind the outbound-investment component of
# Friendshore, as raw values with units and years, never as indices. Pure: the raw tables arrive as
# read by scripts/87 (read.csv / fread, the same calls as scripts/15_build_partner_themes.R).
#
#   * World Bank WDI series (wb_wdi.csv): partnership_strength_build_wdi_long() of the pipeline
#     (R/themes/partnership_strength/stronger_development.R) is called as is; its `value` (the mean
#     of the extract's years each economy reports) is the shipped value. The series are the ones the
#     builder ranks: every series of the extract except the three it leaves out (CPI level, net
#     foreign assets in local currency, net goods trade); energy use per person enters Development
#     Potential on its own (energy_use_index). A series without any value is not shipped.
#   * Doing Business (wb_doingbusiness.csv): partnership_strength_build_doing_business(), its
#     `doingbusiness` (ease of doing business score, DB17-20 methodology, 2019).
#   * OECD CRS (oecd_crs_api.csv): official development assistance, gross disbursements, constant
#     2023 US dollars, millions, by donor, recipient, CRS sector and year. The stage rule is
#     partnership_strength_build_oecd_supplychain()'s, applied here row by row: Energy -> Downstream,
#     Industry -> Midstream, Mineral resources and mining less its coal and oil-and-gas (upstream)
#     sub-rows -> Upstream; recipients restricted to the WDI countries. Its stage totals (2013-2024
#     over every donor per recipient and stage, and that total / 2023 GDP) are rebuilt with the same
#     steps by sector_dev_crs_stage() and checked against that builder (sector_dev_crs_parity()).
#     Since October 2026 Development Potential no longer ranks these totals: its aid components come
#     from oecd_crs_purpose.csv by purpose code, per (tech, stage)
#     (partnership_strength_build_oecd_tech()), and sector_dev_processed_parity() checks the stored
#     components against that builder.
#   * IMF Direct Investment Positions (imf_dip.csv, column-selected): the outward position of the
#     reporter in each counterpart, the same indicator, reporting basis, year window, name matching
#     and duplicate rule as partnership_strength_build_outbound_edges() (safer_friendshore.R), i.e.
#     the position behind Friendshore's outbound_index. Positions with a conduit financial centre
#     (partnership_strength_conduit_centres(); config/partnership.yml outbound_fdi) are kept and
#     flagged: the index treats them as unobserved. sector_dev_dip_parity() re-ranks the shipped
#     positions with the builder's rule and must reproduce its outbound_index exactly.

# The measures of Development & finance built here (scripts/87 --no-dev leaves them out).
SECTOR_DEV_VARS <- c("development.wdi", "development.doing_business", "development.oda_received", "development.oda_given",
                     "development.oda", "development.oda_received_total", "development.oda_received_gdp", "development.fdi_out")

# CRS sector items (dims.items axis `aid_sector`) and the stage each maps to (the builder's rule).
SECTOR_DEV_AID_ITEMS <- tibble::tribble(
  ~slug, ~label, ~text, ~stage,
  "all-sectors", "All sectors", "all sectors", NA_character_,
  "energy", "Energy", "energy", "Downstream",
  "industry", "Industry", "industry", "Midstream",
  "mining", "Mineral resources and mining (less coal, oil and gas)", "mineral resources and mining (less coal, oil and gas)", "Upstream"
)
# The CRS sector names of the extract (lower case, squished) -> item; sign -1 for the fossil sub-rows
# netted out of mining, as in partnership_strength_build_oecd_supplychain().
SECTOR_DEV_CRS_MAP <- tibble::tribble(
  ~sector, ~item, ~sign,
  "all sectors", "all-sectors", 1,
  "energy", "energy", 1,
  "industry", "industry", 1,
  "mineral resources and mining", "mining", 1,
  "coal", "mining", -1,
  "oil and gas (upstream)", "mining", -1
)
# Regional aggregates and unallocated positions of the IMF DIP (partnership_strength_build_outbound_edges()).
SECTOR_DEV_DIP_AGGREGATES <- c(
  "World", "Not Specified (including Confidential)", "Europe", "North and Central America", "Oceania and Polar Regions",
  "Other Near and Middle East Economies", "Economies of Persian Gulf", "Central and South Asia", "East Asia", "North Africa",
  "North Atlantic and Caribbean", "South America", "Sub-Saharan Africa", "US Pacific Islands"
)
SECTOR_DEV_DIP_INDICATOR <- "Outward Direct investment, Assets (gross), Debt instruments, All entities"
SECTOR_DEV_DIP_BASIS <- "Reported official data"
SECTOR_DEV_CONDUIT <- c("none", "counterpart", "investor", "both")
# WDI series the builder leaves out of the development basket (stronger_development.R
# excluded_codes) other than energy use, which enters Development Potential on its own.
SECTOR_DEV_WDI_EXCLUDED <- c("FP.CPI.TOTL", "FM.AST.NFRG.CN", "BN.GSR.MRCH.CD")
SECTOR_DEV_DB_SERIES <- "IC.BUS.EASE.DFRN.XQ.DB1719"

# The IMF DIP years the outbound builder averages (scripts/15 partner_trade_years) and its GDP year
# (partnership_strength_build_outbound_edges() / stronger_development() default).
SECTOR_DEV_DIP_YEARS <- 2020:2024
SECTOR_DEV_GDP_YEAR <- 2023

# The dictionary's dev_indicators (config/sector_dictionary.yml) as tibble(code, label, unit), in
# display order; stops on a duplicate code or a unit that is not in `units`.
sector_dev_indicators <- function(dict_raw) {
  ind <- dict_raw$dev_indicators
  if (!length(ind)) stop("config/sector_dictionary.yml: no dev_indicators")
  get <- function(k) vapply(ind, function(e) if (is.null(e[[k]])) NA_character_ else as.character(e[[k]]), character(1))
  t <- tibble::tibble(code = get("code"), label = get("label"), unit = get("unit"))
  if (anyNA(t)) stop("dev_indicators: every entry needs code, label and unit")
  if (anyDuplicated(t$code)) stop("dev_indicators: duplicate code ", paste(unique(t$code[duplicated(t$code)]), collapse = ", "))
  units <- vapply(dict_raw$units, function(u) as.character(u$id), character(1))
  bad <- setdiff(t$unit, units)
  if (length(bad)) stop("dev_indicators: units not in `units`: ", paste(bad, collapse = ", "))
  t
}

# The Development Potential components stored in data/processed (partner_development_tbl: one row per
# partner, tech, stage and variable) against the builders run on the raw inputs of this export
# (partnership_strength_build_development_indicators() for the partner-level components and
# partnership_strength_build_oecd_tech() for the aid components, per tech and stage): the stored value
# must be identical() wherever the builder has one. Shows that the raw values shipped are the ones
# behind the stored indices. Returns tibble(check, status, n, detail).
sector_dev_processed_parity <- function(processed, indicators, aid) {
  out <- list()
  p <- processed[processed$data_type == "index", c("partner_iso", "tech", "supply_chain", "variable", "value"), drop = FALSE]
  for (v in c("development_index", "doingbusiness_index", "energy_use_index", "aid_index_supplychain", "aidgdp_index_supplychain")) {
    pv <- p[p$variable == v, , drop = FALSE]
    by_cell <- v %in% c("aid_index_supplychain", "aidgdp_index_supplychain")
    ref_tbl <- if (by_cell) aid else indicators
    rk <- if (by_cell) paste(ref_tbl$iso3c, ref_tbl$tech, ref_tbl$supply_chain) else ref_tbl$iso3c
    pk <- if (by_cell) paste(pv$partner_iso, pv$tech, pv$supply_chain) else pv$partner_iso
    ref <- ref_tbl[[v]][match(pk, rk)]
    hit <- !is.na(ref) & !is.na(pv$value)
    # one stored value per key (the table repeats partner-level values for every tech and stage)
    one <- !duplicated(pk) & hit
    ok <- identical(pv$value[hit], ref[hit])
    out[[length(out) + 1]] <- tibble::tibble(
      check = paste0("parity X: the builder on this export's raw inputs reproduces the stored ", v, " (data/processed partner_development_tbl) identical()"),
      status = if (ok && sum(one) > 0) "PASS" else "FAIL", n = as.integer(sum(one)),
      detail = sprintf("%d stored rows compared; %d stored values without a builder value", sum(hit), sum(is.na(ref) & !is.na(pv$value))))
  }
  dplyr::bind_rows(out)
}

# "EG.ELC.ACCS.ZS" -> "eg-elc-accs-zs" (dims.items slug of axis `indicator`).
sector_dev_indicator_slug <- function(code) gsub("[^a-z0-9]+", "-", tolower(code))

# The years of the value columns of a World Bank extract ("X2022..YR2022." -> 2022); NA for the
# empty trailing columns read.csv names X, X.1, ...
sector_dev_wb_years <- function(cols) {
  y <- suppressWarnings(as.integer(sub("^.*YR([0-9]{4}).*$", "\\1", cols)))
  y[!grepl("YR[0-9]{4}", cols)] <- NA_integer_
  y
}

# Per (country, series) of a World Bank extract: the first and last year with a value and the
# number of years. Stops when a column without a year carries a value (the builder's mean would
# then use a value whose year is unknown).
sector_dev_wb_coverage <- function(wb, valid_iso, series = NULL) {
  keys <- c("Country.Name", "Country.Code", "Series.Name", "Series.Code")
  if (!all(keys %in% names(wb))) stop("sector_dev_wb_coverage(): expected columns ", paste(keys, collapse = ", "))
  wb <- wb[wb$Country.Code %in% valid_iso, , drop = FALSE]
  if (!is.null(series)) wb <- wb[wb$Series.Code %in% series, , drop = FALSE]
  vc <- setdiff(names(wb), keys)
  yrs <- sector_dev_wb_years(vc)
  num <- vapply(vc, function(cl) suppressWarnings(as.numeric(wb[[cl]])), numeric(nrow(wb)))
  num <- matrix(num, nrow = nrow(wb))
  if (any(!is.na(num[, is.na(yrs), drop = FALSE]))) stop("sector_dev_wb_coverage(): values in columns without a year: ", paste(vc[is.na(yrs)], collapse = ", "))
  num <- num[, !is.na(yrs), drop = FALSE]
  yrs <- yrs[!is.na(yrs)]
  has <- !is.na(num)
  first <- apply(has, 1, function(h) if (any(h)) min(yrs[h]) else NA_integer_)
  last <- apply(has, 1, function(h) if (any(h)) max(yrs[h]) else NA_integer_)
  tibble::tibble(iso3 = wb$Country.Code, code = wb$Series.Code, year_min = as.integer(first), year_max = as.integer(last),
                 n_years = as.integer(rowSums(has)))
}

# WDI values behind Development Potential: one row per (economy, series) with a finite value.
# `value` is partnership_strength_build_wdi_long()$value verbatim (the mean of the years reported).
# dict_series: the dictionary's dev_indicators codes; a builder series missing from it stops.
sector_dev_wdi <- function(wb_wdi, valid_iso, dict_series) {
  long <- partnership_strength_build_wdi_long(wb_wdi, valid_iso = valid_iso)
  used <- sort(setdiff(unique(long$Series.Code), SECTOR_DEV_WDI_EXCLUDED))
  missing <- setdiff(used, dict_series)
  if (length(missing)) stop("WDI series the Development Potential builder uses are not in config/sector_dictionary.yml dev_indicators: ", paste(missing, collapse = ", "))
  long <- long[long$Series.Code %in% used & is.finite(long$value), , drop = FALSE]
  cov <- sector_dev_wb_coverage(wb_wdi, valid_iso, used)
  cov <- cov[!duplicated(cov[c("iso3", "code")]) | cov$n_years > 0, , drop = FALSE]
  cov <- dplyr::summarise(dplyr::group_by(cov, iso3, code), year_min = suppressWarnings(min(year_min, na.rm = TRUE)),
                          year_max = suppressWarnings(max(year_max, na.rm = TRUE)), n_years = max(n_years), .groups = "drop")
  out <- tibble::tibble(iso3 = as.character(long$Country.Code), code = as.character(long$Series.Code), value = long$value)
  m <- match(paste(out$iso3, out$code), paste(cov$iso3, cov$code))
  out$year_min <- as.integer(cov$year_min[m])
  out$year_max <- as.integer(cov$year_max[m])
  out$n_years <- as.integer(cov$n_years[m])
  if (anyNA(out$year_max)) stop("sector_dev_wdi(): a value without a reported year")
  list(values = out[order(out$code, out$iso3), , drop = FALSE], used = used,
       empty = sort(setdiff(used, unique(out$code))))
}

# Doing Business: partnership_strength_build_doing_business()$doingbusiness verbatim, finite only.
sector_dev_doing_business <- function(wb_db, valid_iso) {
  db <- partnership_strength_build_doing_business(wb_db, valid_iso = valid_iso)
  db <- db[is.finite(db$doingbusiness), , drop = FALSE]
  cov <- sector_dev_wb_coverage(wb_db, valid_iso, SECTOR_DEV_DB_SERIES)
  yrs <- sort(unique(stats::na.omit(cov$year_max)))
  if (length(yrs) != 1L) stop("sector_dev_doing_business(): expected one data year, found ", paste(yrs, collapse = ", "))
  tibble::tibble(iso3 = as.character(db$Country.Code), value = db$doingbusiness, year = yrs)
}

# The CRS extract with the builder's column names (partnership_strength_build_oecd_supplychain():
# positions 5, 7, 8, 10, 27, 29 of the API download when the named columns are absent), plus the
# donor name and the unit, price-base and flow columns the captions state.
sector_dev_crs_columns <- function(oecd_api_raw) {
  o <- oecd_api_raw
  req <- c("DonorISO", "RecipientISO", "Recipient", "Sector", "Year", "Value")
  if (!all(req %in% names(o))) {
    if (ncol(o) < 36) stop("OECD CRS extract: missing expected columns.")
    names(o)[c(5, 7, 8, 10, 27, 29)] <- req
    names(o)[c(6, 12, 18, 20, 26, 31, 36)] <- c("Donor", "Measure", "FlowType", "PriceBase", "Currency", "BaseYear", "UnitMult")
  }
  o
}

# CRS rows by (donor, recipient, aid-sector item, year): value = sum of sign x Value (USD m, constant
# prices) over the extract's rows, recipients in valid_iso. Rows with a missing Value are dropped
# (counted in n_na). Also returns the measure, flow type, price base, base year and unit multiplier
# found in the extract (one each, or the export stops).
sector_dev_crs <- function(oecd_api_raw, valid_iso) {
  o <- sector_dev_crs_columns(oecd_api_raw)
  meta <- list()
  for (k in c("Measure", "FlowType", "PriceBase", "Currency", "BaseYear", "UnitMult")) {
    u <- unique(as.character(o[[k]]))
    if (length(u) != 1L) stop("OECD CRS extract: expected one ", k, ", found ", paste(u, collapse = " | "))
    meta[[k]] <- u
  }
  sector <- stringr::str_to_lower(stringr::str_squish(as.character(o$Sector)))
  unknown <- setdiff(unique(sector), SECTOR_DEV_CRS_MAP$sector)
  if (length(unknown)) stop("OECD CRS extract: CRS sectors without a rule: ", paste(unknown, collapse = ", "))
  m <- match(sector, SECTOR_DEV_CRS_MAP$sector)
  value <- suppressWarnings(as.numeric(o$Value))
  keep <- as.character(o$RecipientISO) %in% valid_iso & !is.na(value)
  t <- tibble::tibble(donor = as.character(o$DonorISO), recipient = as.character(o$RecipientISO),
                      item = SECTOR_DEV_CRS_MAP$item[m], year = as.integer(o$Year), x = SECTOR_DEV_CRS_MAP$sign[m] * value)[keep, , drop = FALSE]
  flows <- dplyr::summarise(dplyr::group_by(t, item, year, donor, recipient), value = sum(x), .groups = "drop")
  flows <- flows[order(match(flows$item, SECTOR_DEV_AID_ITEMS$slug), flows$year, flows$donor, flows$recipient, method = "radix"), , drop = FALSE]
  donors <- unique(tibble::tibble(code = as.character(o$DonorISO), name = as.character(o$Donor)))
  list(flows = flows, meta = meta, n_na = sum(as.character(o$RecipientISO) %in% valid_iso & is.na(value)),
       donors = donors, n_recipients_dropped = length(setdiff(unique(as.character(o$RecipientISO)), valid_iso)))
}

# Years of the CRS extract most donors had not reported when it was taken: a year with fewer than half
# the median number of donors reporting (any aid sector). Development Potential counts them in its
# 2013-2024 totals (so do the shipped totals); the yearly measures and flows leave them out.
# Returns tibble(year, n_donors, n_max).
sector_dev_crs_incomplete <- function(flows) {
  n <- tapply(flows$donor, flows$year, function(d) length(unique(d)))
  bad <- as.integer(names(n)[n < 0.5 * stats::median(n)])
  tibble::tibble(year = bad, n_donors = as.integer(n[as.character(bad)]), n_max = rep(as.integer(max(n)), length(bad)))
}

# The CRS part shipped as years: `crs` without the incomplete years (sector_dev_crs_incomplete()).
sector_dev_crs_ship <- function(crs, incomplete) {
  crs$flows <- crs$flows[!crs$flows$year %in% incomplete$year, , drop = FALSE]
  crs
}

# The builder's quantity, rebuilt with its steps (partnership_strength_build_oecd_supplychain()):
# per (recipient, stage) the total over every donor and year, floored at 0, and that total / GDP of
# gdp_year (USD m / USD, as the builder divides). Returns recipient, supply_chain, aid, aid_gdp.
sector_dev_crs_stage <- function(oecd_api_raw, gdp_data, gdp_year = 2023, valid_iso = NULL) {
  o <- sector_dev_crs_columns(oecd_api_raw)
  o <- dplyr::select(o, dplyr::all_of(c("DonorISO", "RecipientISO", "Recipient", "Sector", "Year", "Value")))
  gdp_tbl <- gdp_data[gdp_data$year == gdp_year, , drop = FALSE]
  gdp_tbl <- tibble::tibble(iso3c = gdp_tbl$iso3c, gdp = suppressWarnings(as.numeric(gdp_tbl$NY.GDP.MKTP.CD)))
  gdp_tbl <- gdp_tbl[!duplicated(gdp_tbl$iso3c), , drop = FALSE]
  sector <- stringr::str_to_lower(stringr::str_squish(as.character(o$Sector)))
  o %>%
    dplyr::mutate(
      supply_chain = dplyr::case_when(
        sector == "energy" ~ "Downstream",
        sector == "industry" ~ "Midstream",
        sector %in% c("mineral resources and mining", "coal", "oil and gas (upstream)") ~ "Upstream",
        TRUE ~ NA_character_
      ),
      sign = dplyr::if_else(sector %in% c("coal", "oil and gas (upstream)"), -1, 1),
      Value = suppressWarnings(as.numeric(Value))
    ) %>%
    dplyr::filter(!is.na(supply_chain)) %>%
    dplyr::filter(if (is.null(valid_iso)) TRUE else RecipientISO %in% valid_iso) %>%
    dplyr::group_by(RecipientISO, supply_chain) %>%
    dplyr::summarize(aid = pmax(0, sum(sign * Value, na.rm = TRUE)), .groups = "drop") %>%
    dplyr::left_join(gdp_tbl, by = c("RecipientISO" = "iso3c")) %>%
    dplyr::mutate(aid_gdp = dplyr::if_else(!is.na(gdp) & gdp > 0, aid / gdp, NA_real_)) %>%
    dplyr::transmute(recipient = as.character(RecipientISO), supply_chain, aid, aid_gdp)
}

# Parity of the rebuilt stage totals with the builder: ranked within each stage as the builder ranks
# them, they must reproduce partnership_strength_build_oecd_supplychain()'s aid_index_supplychain and
# aidgdp_index_supplychain identical(); the dyad rows of `flows` must add up to the totals (1e-9
# relative). Returns tibble(check, status, n, detail).
sector_dev_crs_parity <- function(stage, builder, flows) {
  out <- list()
  add <- function(check, ok, n, detail = NA_character_) {
    out[[length(out) + 1]] <<- tibble::tibble(check = check, status = if (ok) "PASS" else "FAIL", n = as.integer(n), detail = detail)
  }
  s <- stage %>%
    dplyr::group_by(supply_chain) %>%
    dplyr::mutate(a = partnership_strength_safe_scurve(aid), g = partnership_strength_safe_scurve(aid_gdp)) %>%
    dplyr::ungroup()
  k_s <- paste(s$recipient, s$supply_chain)
  k_b <- paste(builder$iso3c, builder$supply_chain)
  same_keys <- setequal(k_s, k_b) && !anyDuplicated(k_s)
  m <- match(k_b, k_s)
  add("parity X: CRS aid received 2013-2024 by stage, ranked as the Development Potential builder ranks it, reproduces its aid_index_supplychain identical()",
      same_keys && identical(s$a[m], builder$aid_index_supplychain), nrow(builder))
  add("parity X: CRS aid received / GDP by stage, ranked as the builder ranks it, reproduces its aidgdp_index_supplychain identical()",
      same_keys && identical(s$g[m], builder$aidgdp_index_supplychain), nrow(builder))
  st <- SECTOR_DEV_AID_ITEMS$stage[match(flows$item, SECTOR_DEV_AID_ITEMS$slug)]
  f <- flows[!is.na(st), , drop = FALSE]
  f$supply_chain <- st[!is.na(st)]
  sums <- dplyr::summarise(dplyr::group_by(f, recipient, supply_chain), v = pmax(0, sum(value)), .groups = "drop")
  mm <- match(paste(stage$recipient, stage$supply_chain), paste(sums$recipient, sums$supply_chain))
  ref <- stage$aid
  got <- sums$v[mm]
  got[is.na(mm)] <- 0
  rel <- abs(got - ref) / pmax(abs(ref), 1e-9)
  add("CRS dyads (donor -> recipient, sector, year) add up to the builder's stage totals (1e-9 relative)", max(c(0, rel)) <= 1e-9, nrow(stage),
      sprintf("max relative difference %.3g", max(c(0, rel))))
  dplyr::bind_rows(out)
}

# IMF DIP positions behind Friendshore's outbound component: one row per (reporter, counterpart)
# with a reported position in `years`, the same steps as partnership_strength_build_outbound_edges()
# up to its conduit filter: rows of SECTOR_DEV_DIP_BASIS and SECTOR_DEV_DIP_INDICATOR, the mean of
# the reported annual positions (USD m), IMF names -> ISO3 through the pipeline's mapping,
# aggregates dropped, two spellings of one economy averaged. Conduit positions are kept and flagged
# (`conduit`: which side is a conduit centre). Also: first and last year reported and their count.
sector_dev_dip <- function(fdi_raw, country_info, years, conduit_iso) {
  require_columns(fdi_raw, c("COUNTRY", "COUNTERPART_COUNTRY", "DV_TYPE", "INDICATOR"), label = "fdi_raw")
  year_cols <- grep("^X?[0-9]{4}$", names(fdi_raw), value = TRUE)
  year_cols <- year_cols[as.integer(sub("^X", "", year_cols)) %in% years]
  if (!length(year_cols)) stop("sector_dev_dip(): no year columns for ", paste(range(years), collapse = "-"))
  fdi <- fdi_raw[fdi_raw$DV_TYPE == SECTOR_DEV_DIP_BASIS & fdi_raw$INDICATOR == SECTOR_DEV_DIP_INDICATOR, , drop = FALSE]
  positions <- vapply(year_cols, function(col) suppressWarnings(as.numeric(fdi[[col]])), numeric(nrow(fdi)))
  positions <- matrix(positions, nrow = nrow(fdi))
  n_reported <- rowSums(!is.na(positions))
  total <- ifelse(n_reported > 0, rowSums(positions, na.rm = TRUE) / pmax(n_reported, 1), NA_real_)
  yr <- as.integer(sub("^X", "", year_cols))
  first <- apply(!is.na(positions), 1, function(h) if (any(h)) min(yr[h]) else NA_integer_)
  last <- apply(!is.na(positions), 1, function(h) if (any(h)) max(yr[h]) else NA_integer_)
  imf_names <- unique(c(fdi$COUNTRY, fdi$COUNTERPART_COUNTRY))
  imf_clean <- partnership_strength_clean_outbound_countries(imf_names)
  imf_iso <- partnership_strength_country_to_iso(imf_clean, country_info)
  imf_iso[imf_clean %in% SECTOR_DEV_DIP_AGGREGATES] <- NA_character_
  t <- tibble::tibble(reporter = imf_iso[match(fdi$COUNTRY, imf_names)], counterpart = imf_iso[match(fdi$COUNTERPART_COUNTRY, imf_names)],
                      total = total, first = as.integer(first), last = as.integer(last), n = as.integer(n_reported))
  t <- t[!is.na(t$reporter) & !is.na(t$counterpart) & !is.na(t$total) & t$reporter != t$counterpart, , drop = FALSE]
  out <- dplyr::summarise(dplyr::group_by(t, reporter, counterpart), value = mean(total), year_min = min(first), year_max = max(last),
                          n_years = max(n), .groups = "drop")
  ci <- out$counterpart %in% conduit_iso
  ri <- out$reporter %in% conduit_iso
  out$conduit <- ifelse(ci & ri, "both", ifelse(ci, "counterpart", ifelse(ri, "investor", "none")))
  out[order(out$reporter, out$counterpart, method = "radix"), , drop = FALSE]
}

# Whether Friendshore drops a position under the configured conduit rule (partnership_strength_conduit_rule():
# its centres `conduit_iso`; scope "both": a centre on either side, "counterpart": a centre as counterpart).
sector_dev_dip_excluded <- function(reporter, counterpart, conduit_iso, scope) {
  counterpart %in% conduit_iso | (!identical(scope, "counterpart") & reporter %in% conduit_iso)
}

# The builder's ranking of the positions it keeps (partnership_strength_build_outbound_edges(), after
# its conduit filter), applied to the shipped positions: returns reporter_iso, partner_iso,
# outbound_index, to be compared identical() with the builder's output.
sector_dev_dip_rank <- function(dip, gdp_data, gdp_year, scope, conduit_iso) {
  gdp_latest <- gdp_data %>%
    dplyr::filter(as.character(year) == as.character(gdp_year)) %>%
    dplyr::transmute(iso3c = as.character(iso3c), GDP = suppressWarnings(as.numeric(NY.GDP.MKTP.CD))) %>%
    dplyr::filter(!is.na(GDP), GDP > 0) %>%
    dplyr::distinct(iso3c, .keep_all = TRUE)
  dip[!sector_dev_dip_excluded(dip$reporter, dip$counterpart, conduit_iso, scope), , drop = FALSE] %>%
    dplyr::transmute(reporter_iso = reporter, partner_iso = counterpart, total = value) %>%
    dplyr::left_join(dplyr::rename(gdp_latest, GDP_reporter = GDP), by = c("reporter_iso" = "iso3c")) %>%
    dplyr::left_join(dplyr::rename(gdp_latest, GDP_partner = GDP), by = c("partner_iso" = "iso3c")) %>%
    dplyr::mutate(GDP_cp = GDP_partner / 1e6, GDP = GDP_reporter / 1e6, total_counterpart_share = total / GDP_cp,
                  outbound_relative = (total / GDP) / GDP_cp) %>%
    dplyr::mutate(
      total_index = partnership_strength_safe_scurve(total),
      total_share_index = partnership_strength_safe_scurve(total_counterpart_share),
      outbound_relative_index = partnership_strength_safe_scurve(outbound_relative),
      outbound_index = partnership_strength_row_weighted_mean(list(total_index, total_share_index, outbound_relative_index), c(1, 1, 1)),
      outbound_index = dplyr::if_else(total <= 0, 0, outbound_index)
    ) %>%
    dplyr::group_by(reporter_iso) %>%
    dplyr::mutate(outbound_index = partnership_strength_safe_scurve(outbound_index)) %>%
    dplyr::ungroup() %>%
    dplyr::select(reporter_iso, partner_iso, outbound_index)
}

# Parity of the shipped positions with the builder's outbound_index (`builder`: the output of
# partnership_strength_build_outbound_edges() on the same raw input and settings).
sector_dev_dip_parity <- function(dip, builder, gdp_data, gdp_year, scope, conduit_iso) {
  mine <- sector_dev_dip_rank(dip, gdp_data, gdp_year, scope, conduit_iso)
  mine <- mine[order(mine$reporter_iso, mine$partner_iso, method = "radix"), , drop = FALSE]
  b <- builder[order(builder$reporter_iso, builder$partner_iso, method = "radix"), , drop = FALSE]
  same_keys <- identical(mine$reporter_iso, b$reporter_iso) && identical(mine$partner_iso, b$partner_iso)
  tibble::tibble(
    check = "parity X: IMF DIP positions (conduit positions dropped as the index drops them), ranked as Friendshore's outbound builder ranks them, reproduce its outbound_index identical()",
    status = if (same_keys && identical(mine$outbound_index, b$outbound_index)) "PASS" else "FAIL",
    n = nrow(b), detail = sprintf("%d positions shipped, %d of them with a conduit centre", nrow(dip), sum(dip$conduit != "none"))
  )
}

# dims.items rows of the two development axes (indicator: the dictionary's dev_indicators order;
# aid_sector: SECTOR_DEV_AID_ITEMS) and dims.item_links (aid sectors -> every cell of their stage).
sector_dev_items <- function(indicators, cells) {
  ind <- tibble::tibble(axis = "indicator", slug = sector_dev_indicator_slug(indicators$code), label = indicators$label,
                        text = sector_lower_first(indicators$label))
  aid <- tibble::tibble(axis = "aid_sector", slug = SECTOR_DEV_AID_ITEMS$slug, label = SECTOR_DEV_AID_ITEMS$label,
                        text = SECTOR_DEV_AID_ITEMS$text)
  items <- dplyr::bind_rows(ind, aid)
  items$tech <- NA_character_
  items$stage <- NA_character_
  items$headline <- FALSE
  items$has_trade <- FALSE
  items$sort <- stats::ave(seq_len(nrow(items)), items$axis, FUN = seq_along)
  st <- SECTOR_DEV_AID_ITEMS[!is.na(SECTOR_DEV_AID_ITEMS$stage), , drop = FALSE]
  links <- dplyr::bind_rows(lapply(seq_len(nrow(st)), function(i) {
    cs <- cells$slug[cells$stage == st$stage[i]]
    tibble::tibble(item_key = paste0("aid_sector:", st$slug[i]), cell = cs, weight = 1)
  }))
  list(items = items, links = links)
}

# Item units of the development measures: each WDI series in its dictionary unit.
sector_dev_item_units <- function(indicators) {
  tibble::tibble(var = "development.wdi", item_key = paste0("indicator:", sector_dev_indicator_slug(indicators$code)), unit = indicators$unit)
}

# Facts (sector_fx() rows, prov X, no formula_value: scripts/87 checks the decoded values identical()
# to the builders', sector_dev_facts_parity()) of the development measures. wdi: sector_dev_wdi()$values;
# db: sector_dev_doing_business(); crs: sector_dev_crs_ship() (the years shown); stage:
# sector_dev_crs_stage() (every year, as the builder sums them); wdi_year: the nominal year of the WDI
# window (its last year); total_year: the last year of the CRS extract (the totals' nominal year).
sector_dev_facts <- function(wdi, db, crs, stage, wdi_year, total_year) {
  w <- sector_fx("development.wdi", paste0("indicator:", sector_dev_indicator_slug(wdi$code)), wdi$iso3, wdi_year, wdi$value, "obs",
                 table = "wb_wdi.csv", variable = wdi$code, prov = "X", obs_year = wdi$year_max)
  # each value is the mean of the years reported: obs_from..obs_year is the span averaged
  w$facts$obs_from <- as.integer(wdi$year_min)
  d <- sector_fx("development.doing_business", "none:none", db$iso3, db$year, db$value, "obs", table = "wb_doingbusiness.csv",
                 variable = SECTOR_DEV_DB_SERIES, prov = "X")
  fl <- crs$flows
  rec <- dplyr::summarise(dplyr::group_by(fl, item, year, recipient), value = sum(value), .groups = "drop")
  don <- dplyr::summarise(dplyr::group_by(fl, item, year, donor), value = sum(value), .groups = "drop")
  r <- sector_fx("development.oda_received", paste0("aid_sector:", rec$item), rec$recipient, rec$year, rec$value, "obs",
                 table = "oecd_crs_api.csv", variable = rec$item, prov = "X")
  g <- sector_fx("development.oda_given", paste0("aid_sector:", don$item), don$donor, don$year, don$value, "obs",
                 table = "oecd_crs_api.csv", variable = don$item, prov = "X")
  item <- SECTOR_DEV_AID_ITEMS$slug[match(stage$supply_chain, SECTOR_DEV_AID_ITEMS$stage)]
  yr <- total_year
  t <- sector_fx("development.oda_received_total", paste0("aid_sector:", item), stage$recipient, yr, stage$aid, "obs",
                 table = "oecd_crs_api.csv", variable = "aid", prov = "X")
  okg <- !is.na(stage$aid_gdp)
  gg <- sector_fx("development.oda_received_gdp", paste0("aid_sector:", item[okg]), stage$recipient[okg], yr, stage$aid_gdp[okg], "obs",
                  table = "oecd_crs_api.csv", variable = "aid_gdp", prov = "X")
  parts <- list(w, d, r, g, t, gg)
  list(facts = dplyr::bind_rows(lapply(parts, `[[`, "facts")), ledger = dplyr::bind_rows(lapply(parts, `[[`, "ledger")))
}

# The text-keyed part of sector-dev: aid flows, positions and the conduit centres.
sector_dev_part <- function(crs, dip, conduits) {
  list(
    oda = tibble::tibble(item_key = paste0("aid_sector:", crs$flows$item), year = crs$flows$year, donor = crs$flows$donor,
                         recipient = crs$flows$recipient, value = crs$flows$value),
    dip = tibble::tibble(reporter = dip$reporter, counterpart = dip$counterpart, value = dip$value, year_min = dip$year_min,
                         year_max = dip$year_max, n_years = dip$n_years, conduit = dip$conduit),
    conduits = tibble::tibble(geo = conduits$iso3, dej_top10 = as.logical(conduits$dej_top10))
  )
}

# Data keys of sector-dev for the availability, roster and coverage tables (sector_data_keys()).
# development.fdi_out is dated by the last year of its window (`dip_year`).
sector_dev_data_keys <- function(dev, kind_of, dip_year) {
  o <- dev$oda
  d <- dev$dip
  dplyr::bind_rows(
    if (nrow(o)) tibble::tibble(var = "development.oda", item_key = o$item_key, chunk = "dev", year = as.integer(o$year), geo = o$recipient,
                                geo_kind = kind_of(o$recipient)),
    if (nrow(d)) tibble::tibble(var = "development.fdi_out", item_key = "none:none", chunk = "dev", year = as.integer(dip_year), geo = d$reporter,
                                geo_kind = kind_of(d$reporter))
  )
}

# The sector-dev chunk (contract DEV_SPEC) from the text-keyed part. gi / ii: geo / item code ->
# 0-based index.
sector_dev_chunk <- function(dev, gi, ii) {
  o <- dev$oda
  oda <- tibble::tibble(item = ii(o$item_key), year = as.integer(o$year), donor = gi(o$donor), recipient = gi(o$recipient), value = as.double(o$value))
  oda <- oda[order(oda$item, oda$year, oda$donor, oda$recipient, method = "radix"), , drop = FALSE]
  d <- dev$dip
  dip <- tibble::tibble(reporter = gi(d$reporter), counterpart = gi(d$counterpart), value = as.double(d$value), year_min = as.integer(d$year_min),
                        year_max = as.integer(d$year_max), n_years = as.integer(d$n_years), conduit = as.character(d$conduit))
  dip <- dip[order(dip$reporter, dip$counterpart, method = "radix"), , drop = FALSE]
  cd <- tibble::tibble(geo = gi(dev$conduits$geo), dej_top10 = as.logical(dev$conduits$dej_top10))
  list(dev = list(oda = oda, dip = dip, conduits = cd[order(cd$geo), , drop = FALSE]))
}

# Cross-table rules of sector-dev (contract checkDev()): finite values, no self pairs, conduit flags
# agree with dev.conduits, positions dated inside their window. (That dev.oda items are aid_sector
# items is checked by the decoded parity, sector_dev_parity().)
sector_check_dev <- function(d) {
  e <- character()
  o <- d$oda
  if (any(!is.finite(o$value))) e <- c(e, "dev.oda.value has non-finite values")
  if (any(o$donor == o$recipient)) e <- c(e, "dev.oda: a donor funding itself")
  p <- d$dip
  if (any(!is.finite(p$value))) e <- c(e, "dev.dip.value has non-finite values")
  if (any(p$reporter == p$counterpart)) e <- c(e, "dev.dip: a position in the reporter itself")
  if (any(p$year_min > p$year_max) || any(p$n_years < 1L) || any(p$n_years > p$year_max - p$year_min + 1L)) e <- c(e, "dev.dip: years out of order")
  cg <- d$conduits$geo
  want <- ifelse(p$counterpart %in% cg & p$reporter %in% cg, "both", ifelse(p$counterpart %in% cg, "counterpart", ifelse(p$reporter %in% cg, "investor", "none")))
  if (!identical(want, p$conduit)) e <- c(e, "dev.dip.conduit disagrees with dev.conduits")
  e
}

# Decoded facts of the development measures against the builders' own values (scripts/87 post-decode).
# dv: decoded facts (var, item_key, geo, year, value) of development.*; ref: list(wdi = (key "ISO3 CODE",
# value) of partnership_strength_build_wdi_long(), db = (key ISO3, value) of
# partnership_strength_build_doing_business()$doingbusiness, stage = sector_dev_crs_stage()).
sector_dev_facts_parity <- function(dv, ref) {
  out <- list()
  add <- function(check, ok, n) out[[length(out) + 1]] <<- tibble::tibble(check = check, status = if (ok && n > 0) "PASS" else "FAIL", n = as.integer(n), detail = NA_character_)
  w <- dv[dv$var == "development.wdi", , drop = FALSE]
  code <- toupper(gsub("-", ".", sub("^indicator:", "", w$item_key)))
  add("parity X: decoded development.wdi identical() to partnership_strength_build_wdi_long()$value on wb_wdi.csv",
      identical(w$value, ref$wdi$value[match(paste(w$geo, code), ref$wdi$key)]), nrow(w))
  add("decoded development.wdi: every value carries the span of years it averages (obs_from <= obs_year)",
      "obs_from" %in% names(w) && !anyNA(w$obs_from) && !anyNA(w$obs_year) && all(w$obs_from <= w$obs_year), nrow(w))
  d <- dv[dv$var == "development.doing_business", , drop = FALSE]
  add("parity X: decoded development.doing_business identical() to partnership_strength_build_doing_business()$doingbusiness on wb_doingbusiness.csv",
      identical(d$value, ref$db$value[match(d$geo, ref$db$key)]), nrow(d))
  st <- ref$stage
  item <- SECTOR_DEV_AID_ITEMS$slug[match(st$supply_chain, SECTOR_DEV_AID_ITEMS$stage)]
  sk <- paste0("aid_sector:", item, " ", st$recipient)
  t <- dv[dv$var == "development.oda_received_total", , drop = FALSE]
  add("parity X: decoded development.oda_received_total identical() to the builder's aid per recipient and stage (oecd_crs_api.csv)",
      identical(t$value, st$aid[match(paste(t$item_key, t$geo), sk)]) && nrow(t) == nrow(st), nrow(t))
  g <- dv[dv$var == "development.oda_received_gdp", , drop = FALSE]
  add("parity X: decoded development.oda_received_gdp identical() to the builder's aid / GDP per recipient and stage",
      identical(g$value, st$aid_gdp[match(paste(g$item_key, g$geo), sk)]) && nrow(g) == sum(!is.na(st$aid_gdp)), nrow(g))
  dplyr::bind_rows(out)
}

# Dictionary placeholders filled from the development data (captions and sources).
# crs: sector_dev_crs() (every year); incomplete: sector_dev_crs_incomplete().
sector_dev_fill <- function(wdi, db, crs, dip, dip_years, conduits, conduit_scope, incomplete) {
  ft <- range(crs$flows$year)
  fy <- range(crs$flows$year[!crs$flows$year %in% incomplete$year])
  list(
    wdi_years = paste0(min(wdi$values$year_min), "-", max(wdi$values$year_max)),
    wdi_n_series = length(unique(wdi$values$code)),
    wdi_empty = if (length(wdi$empty)) paste(wdi$empty, collapse = ", ") else "none",
    db_year = db$year[1],
    crs_years = paste0(fy[1], "-", fy[2]),
    crs_years_total = paste0(ft[1], "-", ft[2]),
    crs_incomplete = if (nrow(incomplete)) paste(sprintf("%d (%d of %d donors had reported)", incomplete$year, incomplete$n_donors, incomplete$n_max), collapse = "; ") else "none",
    crs_year_last = fy[2],
    crs_base_year = crs$meta$BaseYear,
    crs_n_donors = length(unique(crs$flows$donor)),
    crs_n_recipients = length(unique(crs$flows$recipient)),
    # the years with a reported position (the window is dip_years; a year nobody has reported yet is not named)
    dip_years = paste0(min(dip$year_min), "-", max(dip$year_max)),
    dip_n = format(nrow(dip), big.mark = ","),
    dip_n_conduit = format(sum(dip$conduit != "none"), big.mark = ","),
    dip_conduits = paste(conduits$name, collapse = ", "),
    dip_conduit_scope = if (identical(conduit_scope, "counterpart")) "held in one of them" else "with one of them on either side"
  )
}

# The synthetic fixture's development data (sector_synthetic_parts()): a few WDI series, Doing
# Business, CRS aid flows (three donors, four recipients, 2022-2024) and IMF DIP positions with a
# conduit centre (HKG), built with the same part / facts / items helpers as the real export.
# `cells`: the fixture's cell items. Returns list(part, facts, ledger, items, links, item_units, fill).
sector_dev_synthetic <- function(cells) {
  ind <- tibble::tibble(code = c("NY.GDP.PCAP.KD", "EG.ELC.ACCS.ZS", "HD.HCI.OVRL"),
                        label = c("GDP per capita (constant 2015 US$)", "Access to electricity (% of population)", "Human Capital Index (World Bank, 0-1)"),
                        unit = c("usd_pc_2015", "pct_pop", "scale01"))
  econ <- c("BRA", "CHN", "DEU", "IDN", "JPN", "POL", "USA")
  wdi <- tibble::tibble(iso3 = rep(econ, 2), code = rep(c("NY.GDP.PCAP.KD", "EG.ELC.ACCS.ZS"), each = length(econ)),
                        value = c(9100, 12600, 43200, 4300, 36000, 17800, 65000, 100, 100, 100, 99.2, 100, 100, 100),
                        year_min = c(rep(2022L, 10), 2022L, 2023L, 2023L, 2023L), year_max = c(rep(2023L, length(econ)), rep(2023L, 3), 2022L, rep(2023L, 3)),
                        n_years = c(rep(2L, 10), 1L, 1L, 1L, 1L))
  db <- tibble::tibble(iso3 = c("BRA", "CHN", "DEU", "IDN", "JPN", "POL", "USA"), value = c(59.1, 77.9, 79.7, 69.6, 78.0, 76.4, 84.0), year = 2019L)
  grid <- expand.grid(item = c("all-sectors", "energy", "industry", "mining"), year = 2022:2024, donor = c("DEU", "JPN", "USA"),
                      recipient = c("BRA", "IDN", "CHN", "EST"), stringsAsFactors = FALSE)
  grid <- grid[!(grid$recipient == "CHN" & grid$donor == "USA") & !(grid$recipient == "EST"), , drop = FALSE]
  base <- c(`all-sectors` = 400, energy = 60, industry = 12, mining = 3)
  rec <- c(BRA = 1, IDN = 1.6, CHN = 0.4)
  don <- c(DEU = 1.2, JPN = 1.5, USA = 0.9)
  grid$value <- unname(base[grid$item] * rec[grid$recipient] * don[grid$donor] * (1 + 0.1 * (grid$year - 2022)))
  grid$value[grid$item == "mining" & grid$donor == "JPN" & grid$recipient == "CHN" & grid$year == 2023] <- -0.5
  flows <- tibble::as_tibble(grid[order(match(grid$item, SECTOR_DEV_AID_ITEMS$slug), grid$year, grid$donor, grid$recipient, method = "radix"), c("item", "year", "donor", "recipient", "value")])
  flows$year <- as.integer(flows$year)
  crs <- list(flows = flows, meta = list(BaseYear = "2023"))
  st <- flows[!is.na(SECTOR_DEV_AID_ITEMS$stage[match(flows$item, SECTOR_DEV_AID_ITEMS$slug)]), , drop = FALSE]
  st$supply_chain <- SECTOR_DEV_AID_ITEMS$stage[match(st$item, SECTOR_DEV_AID_ITEMS$slug)]
  stage <- dplyr::summarise(dplyr::group_by(st, recipient, supply_chain), aid = pmax(0, sum(value)), .groups = "drop")
  gdp <- c(BRA = 2.17e12, IDN = 1.37e12, CHN = 1.79e13)
  stage$aid_gdp <- stage$aid / unname(gdp[stage$recipient])
  dip <- tibble::tribble(
    ~reporter, ~counterpart, ~value, ~year_min, ~year_max, ~n_years,
    "DEU", "CHN", 21000, 2020L, 2024L, 5L,
    "DEU", "HKG", 3500, 2020L, 2024L, 5L,
    "DEU", "POL", 14000, 2020L, 2024L, 5L,
    "DEU", "USA", 98000, 2020L, 2024L, 5L,
    "JPN", "CHN", 30500, 2020L, 2023L, 4L,
    "JPN", "IDN", 0, 2021L, 2024L, 4L,
    "JPN", "USA", 120000, 2020L, 2024L, 5L,
    "USA", "BRA", 26000, 2020L, 2024L, 5L,
    "USA", "DEU", 64000, 2020L, 2024L, 5L,
    "USA", "JPN", 41000, 2022L, 2024L, 3L,
    "HKG", "CHN", 160000, 2020L, 2024L, 5L
  )
  centres <- tibble::tibble(iso3 = "HKG", name = "Hong Kong SAR, China", dej_top10 = TRUE)
  dip$conduit <- ifelse(dip$counterpart %in% centres$iso3 & dip$reporter %in% centres$iso3, "both",
                        ifelse(dip$counterpart %in% centres$iso3, "counterpart", ifelse(dip$reporter %in% centres$iso3, "investor", "none")))
  fx <- sector_dev_facts(wdi, db, crs, stage, 2024L, 2024L)
  it <- sector_dev_items(ind, cells)
  list(part = sector_dev_part(crs, dip, centres), facts = fx$facts, ledger = fx$ledger, items = it$items, links = it$links,
       item_units = sector_dev_item_units(ind),
       fill = sector_dev_fill(list(values = wdi, empty = "HD.HCI.OVRL"), db, crs, dip, 2020:2024, centres, "both", sector_dev_crs_incomplete(crs$flows)))
}

# The same placeholders for a build without the development data.
sector_dev_fill_none <- function() {
  list(wdi_years = "not in this build", wdi_n_series = 0, wdi_empty = "none", db_year = "not in this build", crs_years = "not in this build",
       crs_years_total = "not in this build", crs_incomplete = "none",
       crs_year_last = "not in this build", crs_base_year = "not in this build", crs_n_donors = 0, crs_n_recipients = 0,
       dip_years = "not in this build", dip_n = 0, dip_n_conduit = 0, dip_conduits = "none", dip_conduit_scope = "with one of them on either side")
}

# Decoded-chunk parity of sector-dev (scripts/87 post-decode): the decoded aid flows and positions are
# identical() to the part that was encoded (text keys restored through the decoded meta).
sector_dev_parity <- function(decoded, meta, dev) {
  geo <- meta$dims$geo$code
  it <- meta$dims$items
  ik <- paste0(it$axis, ":", it$slug)
  o <- decoded$dev$oda
  od <- tibble::tibble(item_key = ik[o$item + 1L], year = o$year, donor = geo[o$donor + 1L], recipient = geo[o$recipient + 1L], value = o$value)
  ref <- dev$oda[order(match(dev$oda$item_key, ik), dev$oda$year, match(dev$oda$donor, geo), match(dev$oda$recipient, geo), method = "radix"), , drop = FALSE]
  p <- decoded$dev$dip
  pd <- tibble::tibble(reporter = geo[p$reporter + 1L], counterpart = geo[p$counterpart + 1L], value = p$value, year_min = p$year_min,
                       year_max = p$year_max, n_years = p$n_years, conduit = p$conduit)
  rp <- dev$dip[order(match(dev$dip$reporter, geo), match(dev$dip$counterpart, geo), method = "radix"), , drop = FALSE]
  tibble::tibble(
    check = c("parity X: decoded aid flows (development.oda) identical() to the CRS rows as summed", "parity X: decoded positions (development.fdi_out) identical() to the IMF DIP positions as parsed"),
    status = c(if (identical(as.data.frame(od), as.data.frame(ref[names(od)]))) "PASS" else "FAIL",
               if (identical(as.data.frame(pd), as.data.frame(rp[names(pd)]))) "PASS" else "FAIL"),
    n = c(nrow(od), nrow(pd)), detail = NA_character_
  )
}
