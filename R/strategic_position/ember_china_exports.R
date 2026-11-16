# Ember China Cleantech Export Data (CC BY 4.0) as a recent read on China's clean-tech shipments to each
# destination, set against the Comtrade view the AIB already uses.
#
# Ember publishes China's customs exports, monthly from 2018, by destination country and commodity code:
# https://ember-energy.org/data/china-cleantech-export-data/ (file clean_tech_exports_full_release_monthly_lower.csv).
# Codes are either full HS6 headings (850760 lithium-ion batteries, 854143 PV modules, 870380 BEV cars) or
# Chinese 8-digit sub-headings (85044030 PV inverters, 85030030 wind nacelles, 38180015/19 wafers). Such a code
# is only part of its HS6 heading, so it is flagged `partial_hs6` and never compared with HS6 trade. An 8-digit
# code ending "00" (85414200 cells, 85414300 modules) is China's undivided HS6 heading and counts as full.
#
# Three uses:
#   ember_cte_cells()          China's exports to each destination per basket cell (tech x stage) over the
#                              latest 12 months and a base calendar year, and their growth.
#   ember_cte_compare()        Ember 2024 exports vs Comtrade 2024 imports-from-China, code by code, for full
#                              HS6 codes: how far the two sources agree.
#   ember_cte_nowcast_share()  China's share of an importer's imports of the Ember-covered codes in a cell,
#                              rolled forward from the Comtrade year with Ember's growth (indicative).
#
# A code the basket files under several technologies counts in each (technology-attributed trade, as in
# import_exposure.R). Pure: no IO.

EMBER_CTE_COLS <- c(area = "Area", date = "Date", area_type = "Area type", code = "Commodity code",
                    category = "Commodity category", sub_category = "Commodity sub-category", usd = "Amount (USD)")

# Ember area names countrycode does not resolve. Netherlands Antilles (dissolved 2010) stays NA.
EMBER_CTE_ISO3_FIXES <- c(Micronesia = "FSM", Kosovo = "XKX")

# Raw Ember file -> one row per area x month x code: area, iso3 (NA for regions and unresolved names),
# is_region, date (first of month), code, hs6, partial_hs6, category, sub_category, usd.
ember_cte_prepare <- function(raw) {
  miss <- setdiff(EMBER_CTE_COLS, names(raw))
  if (length(miss) > 0) stop("ember_cte_prepare(): Ember file lacks column(s) ", paste(miss, collapse = ", "),
                             "; the layout has changed.")
  code <- gsub("[^0-9]", "", as.character(raw[[EMBER_CTE_COLS[["code"]]]]))
  code <- ifelse(nchar(code) == 5L | nchar(code) == 7L, paste0("0", code), code)
  if (any(nchar(code) < 6L)) stop("ember_cte_prepare(): commodity code(s) shorter than six digits: ",
                                  paste(utils::head(unique(code[nchar(code) < 6L]), 5), collapse = ", "), ".")
  area <- trimws(as.character(raw[[EMBER_CTE_COLS[["area"]]]]))
  is_region <- !trimws(as.character(raw[[EMBER_CTE_COLS[["area_type"]]]])) %in% "Country or economy"
  iso3 <- rep(NA_character_, length(area))
  iso3[!is_region] <- countrycode::countrycode(area[!is_region], "country.name", "iso3c", warn = FALSE,
                                               custom_match = EMBER_CTE_ISO3_FIXES)
  date <- as.Date(substr(as.character(raw[[EMBER_CTE_COLS[["date"]]]]), 1, 10))
  if (anyNA(date)) stop("ember_cte_prepare(): unparseable Date value(s).")
  data.frame(
    area = area, iso3 = iso3, is_region = is_region, date = date, code = code, hs6 = substr(code, 1, 6),
    partial_hs6 = nchar(code) > 6L & substr(code, 7, 8) != "00",
    category = as.character(raw[[EMBER_CTE_COLS[["category"]]]]),
    sub_category = as.character(raw[[EMBER_CTE_COLS[["sub_category"]]]]),
    usd = suppressWarnings(as.numeric(raw[[EMBER_CTE_COLS[["usd"]]]])),
    stringsAsFactors = FALSE
  )
}

# Ember codes x basket cells. `cells` has tech, stage, hs6 (tri_basket_cells() rows; any Deployment rows are
# dropped, since Deployment re-uses the Assembly codes). Returns one row per Ember code x (tech, stage) with
# category, sub_category, partial_hs6 and n_cells (how many cells the code is attributed to). Ember codes with
# no basket cell are returned in attr "unmatched" (code, category, sub_category).
ember_cte_code_cells <- function(prepared, cells) {
  codes <- unique(prepared[c("code", "hs6", "partial_hs6", "category", "sub_category")])
  if (anyDuplicated(codes$code)) stop("ember_cte_code_cells(): a code carries two categories.")
  cells <- unique(cells[!cells$stage %in% "Deployment", c("tech", "stage", "hs6")])
  out <- merge(codes, cells, by = "hs6")
  out$n_cells <- as.integer(stats::ave(seq_len(nrow(out)), out$code, FUN = length))
  out <- out[order(out$code, out$tech, out$stage), c("code", "hs6", "partial_hs6", "category", "sub_category",
                                                     "tech", "stage", "n_cells")]
  rownames(out) <- NULL
  attr(out, "unmatched") <- codes[!codes$code %in% out$code, c("code", "category", "sub_category")]
  out
}

# The `months` calendar months ending at `end` (a Date or "YYYY-MM"), as first-of-month Dates.
ember_cte_window <- function(end, months = 12L) {
  end <- as.Date(paste0(substr(as.character(end), 1, 7), "-01"))
  rev(seq(end, by = "-1 month", length.out = months))
}

# China's exports per destination country x code over the latest `months` months up to `end` (default: the
# file's last month) and over calendar `base_year`. A destination with no row in a window has 0 there only if
# the file has that code for some destination in the window (Ember omits zero rows). Columns: iso3, code,
# usd_latest, usd_base, latest_end, latest_months.
ember_cte_code_windows <- function(prepared, base_year = 2024L, end = NULL, months = 12L) {
  d <- prepared[!prepared$is_region & !is.na(prepared$iso3), , drop = FALSE]
  if (is.null(end)) end <- max(d$date)
  win <- ember_cte_window(end, months)
  missing_months <- setdiff(as.character(win), as.character(unique(d$date)))
  if (length(missing_months) > 0) stop("ember_cte_code_windows(): no data for ", paste(missing_months, collapse = ", "), ".")
  sum_by <- function(x) {
    if (nrow(x) == 0) return(data.frame(iso3 = character(), code = character(), usd = numeric()))
    stats::aggregate(usd ~ iso3 + code, data = x, FUN = sum, na.rm = TRUE, na.action = stats::na.pass)
  }
  latest <- sum_by(d[d$date %in% win, , drop = FALSE])
  base <- sum_by(d[format(d$date, "%Y") == as.character(base_year), , drop = FALSE])
  names(latest)[3] <- "usd_latest"
  names(base)[3] <- "usd_base"
  out <- merge(latest, base, by = c("iso3", "code"), all = TRUE)
  out$usd_latest[is.na(out$usd_latest)] <- 0
  out$usd_base[is.na(out$usd_base)] <- 0
  out$latest_end <- format(max(win), "%Y-%m")
  out$latest_months <- as.integer(months)
  out
}

# Per destination x basket cell: China's exports over the latest window and the base year, summed over the
# Ember codes in the cell, with growth (latest / base; NA when base is 0), the codes used and whether any is a
# partial HS6 code. `windows` from ember_cte_code_windows(), `code_cells` from ember_cte_code_cells().
ember_cte_cells <- function(windows, code_cells) {
  j <- merge(windows, code_cells[c("code", "partial_hs6", "tech", "stage")], by = "code")
  if (nrow(j) == 0) stop("ember_cte_cells(): no Ember code maps to a basket cell.")
  key <- paste(j$iso3, j$tech, j$stage, sep = "\r")
  idx <- split(seq_len(nrow(j)), key)
  out <- do.call(rbind, lapply(idx, function(i) {
    data.frame(iso3 = j$iso3[i[1]], tech = j$tech[i[1]], stage = j$stage[i[1]],
               usd_latest = sum(j$usd_latest[i]), usd_base = sum(j$usd_base[i]),
               codes = paste(sort(unique(j$code[i])), collapse = " "), any_partial_hs6 = any(j$partial_hs6[i]),
               latest_end = j$latest_end[i[1]], stringsAsFactors = FALSE)
  }))
  out$growth <- ifelse(out$usd_base > 0, out$usd_latest / out$usd_base, NA_real_)
  out <- out[order(out$iso3, out$tech, out$stage), ]
  rownames(out) <- NULL
  out
}

# Ember base-year exports vs Comtrade imports-from-China for full HS6 codes. `comtrade` has reporter_iso, hs6,
# imports_from_china (the reporter's declared imports from CHN in the same year). Returns one row per
# destination x code where either side is positive, with ratio = ember / comtrade (CIF/FOB valuation, transit
# through third countries and timing make about 0.8-1.0 normal). Partial (8-digit) Ember codes are left out.
ember_cte_compare <- function(windows, code_cells, comtrade) {
  full <- unique(code_cells[!code_cells$partial_hs6, c("code", "hs6")])
  e <- merge(windows[c("iso3", "code", "usd_base")], full, by = "code")
  if (nrow(e) == 0) {
    e <- data.frame(iso3 = character(), hs6 = character(), ember_usd = numeric())
  } else {
    e <- stats::aggregate(usd_base ~ iso3 + hs6, data = e, FUN = sum)
    names(e)[3] <- "ember_usd"
  }
  ct <- data.frame(iso3 = comtrade$reporter_iso, hs6 = comtrade$hs6, comtrade_usd = comtrade$imports_from_china)
  ct <- ct[ct$hs6 %in% full$hs6, , drop = FALSE]
  out <- merge(e, ct, by = c("iso3", "hs6"), all = TRUE)
  out$comtrade_reported <- !is.na(out$comtrade_usd)
  out$ember_usd[is.na(out$ember_usd)] <- 0
  out <- out[out$ember_usd > 0 | (out$comtrade_reported & out$comtrade_usd > 0), , drop = FALSE]
  out$ratio <- ifelse(out$comtrade_usd > 0, out$ember_usd / out$comtrade_usd, NA_real_)
  out <- out[order(out$iso3, out$hs6), ]
  rownames(out) <- NULL
  out
}

# Summary of ember_cte_compare() per code: matched reporters, total value on each side, value-weighted ratio
# and the median and inter-quartile range of reporter ratios (reporters with both sides positive).
ember_cte_compare_summary <- function(cmp) {
  m <- cmp[cmp$comtrade_reported & cmp$comtrade_usd > 0 & cmp$ember_usd > 0, , drop = FALSE]
  do.call(rbind, lapply(split(m, m$hs6), function(x) {
    q <- stats::quantile(x$ratio, c(0.25, 0.5, 0.75), names = FALSE)
    data.frame(hs6 = x$hs6[1], n_reporters = nrow(x), ember_usd = sum(x$ember_usd), comtrade_usd = sum(x$comtrade_usd),
               ratio_total = sum(x$ember_usd) / sum(x$comtrade_usd), ratio_p25 = q[1], ratio_median = q[2], ratio_p75 = q[3])
  }))
}

# Indicative China share of an importer's imports of the Ember-covered codes in a cell, rolled forward from the
# Comtrade year. `base` has iso3, tech, stage, china_usd and total_usd (Comtrade base-year imports over the
# cell's full-HS6 Ember codes); `cells` from ember_cte_cells() (built from the same codes). The China flow is
# scaled by Ember's growth and every other supplier is held flat:
#   share_now = C * g / (C * g + T - C)
# Holding the rest flat overstates the swing when the whole market moves; treat the result as a direction and
# order of magnitude, not a replacement for the Comtrade share. Cells with no growth (base 0) get NA.
ember_cte_nowcast_share <- function(base, cells) {
  j <- merge(base, cells[c("iso3", "tech", "stage", "growth", "latest_end")], by = c("iso3", "tech", "stage"))
  j$share_base <- ifelse(j$total_usd > 0, j$china_usd / j$total_usd, NA_real_)
  scaled <- j$china_usd * j$growth
  j$share_now <- ifelse(j$total_usd > 0 & !is.na(j$growth), scaled / (scaled + j$total_usd - j$china_usd), NA_real_)
  j$share_change <- j$share_now - j$share_base
  j <- j[order(j$iso3, j$tech, j$stage), ]
  rownames(j) <- NULL
  j
}
