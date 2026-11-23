# Chart of the Day: non-trade sources (pure). The script reads the raw files and passes them here:
#   ei      Energy Institute Statistical Review, long CSV (Country, ISO3166_alpha3, Year, Var, Value), 1965-2025
#   gcim    Rhodium/MIT Clean Investment Monitor (GCIM_Investment_Capacity_aggregated.xlsx): annual_actual_investment
#           (Country, Sector, Technology, Year, Investment $bn 2024) and electric_power_capacity (..., Year, Value GW)
#   atlas   Harvard Growth Lab Atlas HS92 country x product x year exports, already cut to the basket's HS6 codes
#   nipo    GTA-based industrial-policy measures (nipo_stage_measures_tbl) and their tech links (nipo_stage_links_tbl)
#   fdi     clean-tech manufacturing FDI projects (cm_fdi_projects_tbl: battery, EV, solar, wind factories)

COD_EI_VARS <- c("electbyfuel_coal", "electbyfuel_gas", "electbyfuel_oil", "electbyfuel_nuclear", "electbyfuel_hydro",
                 "electbyfuel_other", "electbyfuel_total", "wind_twh", "solar_twh", "biogeo_twh", "nuclear_twh",
                 "lithium_kt", "cobalt_kt", "graphite_kt", "rareearths_kt", "coalprod_mt", "gasprod_bcm", "oilprod_kbd",
                 "co2_mtco2")

#' EI rows for the variables the templates use, one row per country x year x variable. EI also carries regional
#' totals and "Other ..." remainders under ISO-like codes (T-EU, O-AP, ROW, USSR): only real ISO3 codes are kept as
#' countries, plus WLD ("Total World"), which the templates use as the world figure.
cod_ei_rows <- function(ei) {
  x <- data.frame(iso3 = ei$ISO3166_alpha3, year = as.integer(ei$Year), var = ei$Var, value = as.numeric(ei$Value),
                  stringsAsFactors = FALSE)
  ok <- x$iso3 %in% c(countrycode::codelist$iso3c, "WLD")
  x[ok & x$var %in% COD_EI_VARS & !is.na(x$value), ]
}

#' GCIM country names to ISO3 (countrycode, with the monitor's own short names).
cod_gcim_iso3 <- function(country) {
  fix <- c(US = "USA", UK = "GBR", Bosnia = "BIH", Kosovo = "XKX")
  out <- suppressWarnings(countrycode::countrycode(country, "country.name", "iso3c", custom_match = fix))
  out
}

cod_gcim_rows <- function(inv, power) {
  inv <- as.data.frame(inv); power <- as.data.frame(power)
  i <- data.frame(iso3 = cod_gcim_iso3(inv$Country), sector = inv$Sector, technology = inv$Technology,
                  year = as.integer(inv$Year), usd = as.numeric(inv$Investment) * 1e9, stringsAsFactors = FALSE)
  i <- i[!is.na(i$iso3) & !is.na(i$usd), ]
  i <- stats::aggregate(usd ~ iso3 + sector + technology + year, i, sum)
  p <- data.frame(iso3 = cod_gcim_iso3(power$Country), technology = power$Technology, year = as.integer(power$Year),
                  gw = as.numeric(power$Value), stringsAsFactors = FALSE)
  p <- p[!is.na(p$iso3) & !is.na(p$gw), ]
  p <- stats::aggregate(gw ~ iso3 + technology + year, p, sum)
  list(invest = i, power = p)
}

#' Atlas exports by country x tech x stage x year and each country's share of the world total.
#' `atlas`: country_iso3_code, product_hs92_code, year, export_value (already cut to basket codes).
cod_atlas_cells <- function(atlas, master) {
  m <- unique(data.frame(hs6 = as.integer(master$hs6), tech = as.character(master$tech), stage = as.character(master$stage),
                         stringsAsFactors = FALSE))
  m <- m[!is.na(m$stage) & nzchar(m$stage), ]
  a <- data.frame(iso3 = atlas$country_iso3_code, hs6 = as.integer(atlas$product_hs92_code), year = as.integer(atlas$year),
                  usd = as.numeric(atlas$export_value), stringsAsFactors = FALSE)
  a <- a[!is.na(a$usd) & a$usd > 0, ]
  j <- merge(a, m, by = "hs6")
  key <- paste(j$iso3, j$tech, j$stage, j$year, sep = "|")
  s <- rowsum(j$usd, key)
  parts <- do.call(rbind, strsplit(rownames(s), "|", fixed = TRUE))
  out <- data.frame(iso3 = parts[, 1], tech = parts[, 2], stage = parts[, 3], year = as.integer(parts[, 4]), exports = as.numeric(s),
                    stringsAsFactors = FALSE)
  world <- stats::aggregate(exports ~ tech + stage + year, out, sum)
  out$share <- out$exports / world$exports[match(paste(out$tech, out$stage, out$year), paste(world$tech, world$stage, world$year))]
  attr(out, "codes_matched") <- length(intersect(unique(a$hs6), m$hs6))
  attr(out, "codes_basket") <- length(unique(m$hs6))
  out
}

#' Policy measures counted by the NIPO pipeline, linked to technologies (one row per measure x tech).
cod_nipo_rows <- function(measures, links) {
  m <- as.data.frame(measures); l <- as.data.frame(links)
  m <- m[m$counted %in% TRUE & !(m$is_sanction %in% TRUE), ]
  l <- unique(l[c("intervention_id", "tech")])
  j <- merge(m[c("intervention_id", "iso3", "jurisdiction_name", "family", "support", "announcement_date")], l, by = "intervention_id")
  j$year <- as.integer(format(j$announcement_date, "%Y"))
  j[!is.na(j$year), c("intervention_id", "iso3", "tech", "family", "support", "year")]
}

#' Clean-tech factory FDI projects (any status except cancelled or closed).
cod_fdi_rows <- function(fdi) {
  f <- as.data.frame(fdi)
  f <- f[!f$status %in% c("Cancelled", "Closed") & f$home_iso3 != f$destination_iso3, ]
  data.frame(project = f$project_id, sector = f$sector, home = f$home_iso3, host = f$destination_iso3,
             year = suppressWarnings(as.integer(f$year_announced)), status = f$status, usd = suppressWarnings(as.numeric(f$capital_usd_m)) * 1e6,
             stringsAsFactors = FALSE)
}

#' Add the non-trade sources to cod_prepare() output. Any element of `more` may be NULL (its templates then
#' produce no candidates).
cod_prepare_more <- function(d, more) {
  if (!is.null(more$ei)) d$ei <- cod_ei_rows(more$ei)
  if (!is.null(more$gcim_invest) && !is.null(more$gcim_power)) {
    g <- cod_gcim_rows(more$gcim_invest, more$gcim_power); d$gcim_invest <- g$invest; d$gcim_power <- g$power
  }
  if (!is.null(more$atlas)) d$atlas <- more$atlas
  if (!is.null(more$nipo_measures) && !is.null(more$nipo_links)) d$nipo <- cod_nipo_rows(more$nipo_measures, more$nipo_links)
  if (!is.null(more$fdi)) d$fdi <- cod_fdi_rows(more$fdi)
  d
}
