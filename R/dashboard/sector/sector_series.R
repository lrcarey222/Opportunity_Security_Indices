# The sector-series chunk (time-series scope, user decision 2026-10-10): EI production, generation
# and consumption 1965-2025, IEA EV regions 2021-2024, CIM power capacity additions 2018-2025, IMF
# monthly prices and CIM manufacturing capacity by product. Pure.

# EI long CSV (Country, Year, ISO3166_alpha3, Var, Value) -> series rows. var_map: (var, item_key,
# ei_var). Country rows keep their ISO3; "WLD" is the world (series.world); "T-EU" is EI's own EU
# total, shipped as EUU; every other aggregate (T-*, O-*, regions) is dropped. RES:ei = world - the
# sum of the country rows (EI's residual: economies it does not list separately). geo_codes: the
# country codes of dims.geo (others are dropped and reported).
sector_ei_series <- function(ei, var_map, geo_codes, years = 1965:2025) {
  x <- ei[ei$Var %in% var_map$ei_var & ei$Year %in% years & !is.na(ei$Value), , drop = FALSE]
  code <- x$ISO3166_alpha3
  is_world <- code %in% "WLD" | (is.na(code) & grepl("^Total World$", x$Country))
  is_eu <- code %in% "T-EU"
  is_cty <- grepl("^[A-Z]{3}$", code) & !is_world & code %in% geo_codes
  dropped_codes <- unique(code[grepl("^[A-Z]{3}$", code) & !is_world & !code %in% geo_codes])
  m <- match(x$Var, var_map$ei_var)
  mk <- function(sel, geo) {
    tibble::tibble(var = var_map$var[m[sel]], item_key = var_map$item_key[m[sel]], geo = geo, year = as.integer(x$Year[sel]),
                   value = as.double(x$Value[sel]), status = as.character(ifelse(x$Value[sel] == 0, "zero", "obs")), obs_year = NA_integer_)
  }
  cty <- mk(is_cty, code[is_cty])
  eu <- mk(is_eu, "EUU")
  w <- mk(is_world, "WLD")
  world <- tibble::tibble(var = w$var, item_key = w$item_key, year = w$year, value = w$value, basis = "source_world")
  if (anyDuplicated(cty[c("var", "item_key", "geo", "year")])) stop("EI series: duplicate country rows")
  if (anyDuplicated(world[c("var", "item_key", "year")])) stop("EI series: duplicate world rows")
  listed <- stats::aggregate(list(listed = cty$value), list(var = cty$var, item_key = cty$item_key, year = cty$year), sum)
  res <- merge(world, listed, by = c("var", "item_key", "year"))
  r <- res$value - res$listed
  # Floating-point dust of EI's nine-decimal figures is a zero residual. A residual that is still
  # negative (the listed economies exceed EI's own world total) is not a quantity: it is dropped
  # and returned in `negative_residual` for the validation report.
  r[abs(r) <= pmax(1e-6 * abs(res$value), 1e-8)] <- 0
  neg <- r < 0
  resid <- tibble::tibble(var = res$var[!neg], item_key = res$item_key[!neg], geo = "RES:ei", year = as.integer(res$year[!neg]),
                          value = r[!neg], status = as.character(ifelse(r[!neg] == 0, "zero", "obs")), obs_year = NA_integer_)
  negative <- tibble::tibble(var = res$var[neg], item_key = res$item_key[neg], year = as.integer(res$year[neg]),
                             residual = r[neg], world = res$value[neg])
  # Successor and predecessor fill (after the residual, which these zeros do not change).
  succ <- sector_ei_successor_nsr(cty, x[is_cty | x$ISO3166_alpha3 %in% SECTOR_EI_PREDECESSORS$pred, , drop = FALSE], var_map)
  list(obs = dplyr::bind_rows(succ$obs, eu, resid), world = world, dropped = dropped_codes, negative_residual = negative,
       successor_nsr = succ$n)
}

# EI fills the successor states of the USSR and Yugoslavia with 0 before they exist (and the USSR
# with 0 after it ends). Those zeros are not observations: the quantity sits in the predecessor's
# row (USSR, "SUN") or in EI's "Other Europe" ("O-EUR"). pred: the predecessor's code in the EI CSV;
# members: its successors; first_year: the members' first year of their own.
SECTOR_EI_PREDECESSORS <- tibble::tibble(
  pred = c("SUN", "O-EUR"),
  members = list(c("ARM", "AZE", "BLR", "EST", "GEO", "KAZ", "KGZ", "LVA", "LTU", "MDA", "RUS", "TJK", "TKM", "UKR", "UZB"),
                 c("SVN", "HRV", "MKD", "BIH", "SRB", "MNE", "SCG", "XKX")),
  first_year = c(1985L, 1990L),
  own_row = c(TRUE, FALSE)
)

# rows: the country series rows of sector_ei_series() (var, item_key, geo, year, value, status);
# ei: the EI rows of the countries and predecessors (Country, Year, ISO3166_alpha3, Var, Value);
# var_map as for sector_ei_series(). Returns list(obs, n): rows with
#   * a successor's 0 before its first year, where the predecessor is non-zero that year -> nsr, NaN;
#   * a predecessor with its own row (SUN): a 0 from the successors' first year on, or after its
#     last non-zero year -> nsr, NaN;
# and n, the number of rows changed (for the validation report).
sector_ei_successor_nsr <- function(rows, ei, var_map) {
  pk <- paste(ei$ISO3166_alpha3, ei$Var, ei$Year)
  ei_var <- var_map$ei_var[match(paste(rows$var, rows$item_key), paste(var_map$var, var_map$item_key))]
  hit <- rep(FALSE, nrow(rows))
  for (i in seq_len(nrow(SECTOR_EI_PREDECESSORS))) {
    p <- SECTOR_EI_PREDECESSORS[i, ]
    pv <- ei$Value[match(paste(p$pred, ei_var, rows$year), pk)]
    hit <- hit | (rows$geo %in% p$members[[1]] & rows$year < p$first_year & rows$value == 0 & !is.na(pv) & pv != 0)
    if (p$own_row) {
      own <- rows$geo == p$pred
      if (any(own)) {
        nz_year <- ifelse(rows$value[own] != 0, rows$year[own], NA_integer_)
        last_nz <- suppressWarnings(tapply(nz_year, paste(rows$var, rows$item_key)[own], max, na.rm = TRUE))
        lk <- suppressWarnings(as.integer(last_nz[paste(rows$var, rows$item_key)]))
        after <- rows$year >= p$first_year | (!is.na(lk) & rows$year > lk)
        hit <- hit | (own & rows$value == 0 & after)
      }
    }
  }
  rows$value[hit] <- NaN
  rows$status[hit] <- "nsr"
  list(obs = rows, n = sum(hit))
}

# The EI variables of the series (and of the processed cross-sections).
sector_ei_var_map <- function() {
  tibble::tribble(
    ~var, ~item_key, ~ei_var,
    "production.fuels", "fuel:oil", "oilprod_kbd",
    "production.fuels", "fuel:gas", "gasprod_ej",
    "production.fuels", "fuel:coal", "coalprod_ej",
    "production.generation", "fuel:solar", "solar_twh",
    "production.generation", "fuel:wind", "wind_twh",
    "production.generation", "fuel:nuclear", "nuclear_twh",
    "production.generation", "fuel:hydro", "hydro_twh",
    "production.generation", "fuel:coal", "electbyfuel_coal",
    "production.generation", "fuel:gas", "electbyfuel_gas",
    "production.generation", "fuel:oil", "electbyfuel_oil",
    "consumption.total_ej", "fuel:oil", "oil_tes_ej",
    "consumption.total_ej", "fuel:gas", "gas_tes_ej",
    "consumption.total_ej", "fuel:coal", "coal_tes_ej",
    "consumption.total_ej", "fuel:nuclear", "nuclear_tes_ej",
    "consumption.total_ej", "fuel:solar", "solar_tes_ej",
    "consumption.total_ej", "fuel:wind", "wind_tes_ej",
    "consumption.total_ej", "fuel:hydro", "hydro_tes_ej"
  )
}

# IEA EV regions 2021-2024 (ev_Midstream_capacity.csv: Region, Year, Domestic sales, Domestic
# Production) -> production.ev_region and production.ev_region_sales.
sector_iea_ev_series <- function(ev) {
  nm <- names(ev)
  prod <- nm[grepl("^Domestic.?Production$", nm)]
  sales <- nm[grepl("^Domestic.?sales$", nm)]
  if (length(prod) != 1 || length(sales) != 1) stop("ev_Midstream_capacity.csv: production / sales columns not found")
  reg <- SECTOR_EV_REGIONS[trimws(ev$Region)]
  if (anyNA(reg)) stop("ev_Midstream_capacity.csv: unknown regions ", paste(unique(ev$Region[is.na(reg)]), collapse = ", "))
  geo <- paste0("R:iea_ev:", unname(reg))
  mk <- function(var, v) tibble::tibble(var = var, item_key = "cell:electric-vehicles-midstream", geo = geo, year = as.integer(ev$Year),
                                        value = as.double(v), status = ifelse(v == 0, "zero", "obs"), obs_year = NA_integer_)
  dplyr::bind_rows(mk("production.ev_region", ev[[prod]]), mk("production.ev_region_sales", ev[[sales]]))
}

# CIM electric power capacity additions (GW) -> investment.power_additions per cell. Nuclear and
# Advanced Nuclear add up (both GW, disjoint); Other Renewables is not a cell and is dropped.
# country_iso3: names -> ISO3.
sector_cim_power <- function(pw, country_iso3) {
  cell <- c(Solar = "solar-downstream", Wind = "wind-downstream", Nuclear = "nuclear-downstream",
            `Advanced Nuclear` = "nuclear-downstream", `Enhanced Geothermal` = "geothermal-downstream")[pw$Technology]
  x <- pw[!is.na(cell), , drop = FALSE]
  x$cell <- cell[!is.na(cell)]
  x$iso3 <- country_iso3(x$Country)
  if (anyNA(x$iso3)) stop("CIM power additions: no ISO3 for ", paste(unique(x$Country[is.na(x$iso3)]), collapse = ", "))
  a <- stats::aggregate(list(value = as.numeric(x$Value)), list(cell = x$cell, geo = x$iso3, year = as.integer(x$Year)), sum)
  tibble::tibble(var = "investment.power_additions", item_key = paste0("cell:", a$cell), geo = a$geo, year = a$year,
                 value = a$value, status = ifelse(a$value == 0, "zero", "obs"), obs_year = NA_integer_)
}

# CIM manufacturing capacity by product and status (native units, never summed across products),
# one snapshot (year 2025). Status (contract vocabulary): operational = CIM's "current operational"
# plus "operational yet to come online" (built and in operation, still ramping up: the same product
# and unit, so the two add); construction / announced / paused = capacity CIM anticipates by 2030.
sector_cim_capacity <- function(cap, country_iso3, year = 2025L) {
  cell <- c(Batteries = "batteries-midstream", Solar = "solar-midstream", Wind = "wind-midstream",
            `Zero Emission Vehicles` = "electric-vehicles-midstream", `Critical Minerals` = "batteries-upstream")[cap$Technology]
  x <- cap[!is.na(cell) & !is.na(cap$Value), , drop = FALSE]
  x$cell <- cell[!is.na(cell) & !is.na(cap$Value)]
  cat <- tolower(x$Category)
  x$cap_status <- ifelse(grepl("^current operational", cat) | grepl("yet to come online", cat), "operational",
                  ifelse(grepl("under construction", cat), "construction", ifelse(grepl("^announced", cat), "announced",
                  ifelse(grepl("^paused", cat), "paused", NA_character_))))
  if (anyNA(x$cap_status)) stop("CIM capacity: unknown categories ", paste(unique(x$Category[is.na(x$cap_status)]), collapse = ", "))
  x$year <- as.integer(year)
  unit <- ifelse(x$Technology %in% c("Solar", "Wind"), "GW",
          ifelse(x$Technology == "Zero Emission Vehicles", "vehicles",
          ifelse(x$Technology == "Critical Minerals", "t/yr",
          ifelse(x$Technology == "Batteries" & x$Product == "EAM", "kt", "GWh"))))
  x$iso3 <- country_iso3(x$Country)
  if (anyNA(x$iso3)) stop("CIM capacity: no ISO3 for ", paste(unique(x$Country[is.na(x$iso3)]), collapse = ", "))
  a <- stats::aggregate(list(value = as.numeric(x$Value)),
                        list(cell = x$cell, product = x$Product, unit = unit, geo = x$iso3, year = x$year, cap_status = x$cap_status), sum)
  tibble::tibble(item_key = paste0("cell:", a$cell), product = a$product, unit = a$unit, geo = a$geo, year = as.integer(a$year),
                 cap_status = a$cap_status, value = a$value, status = ifelse(a$value == 0, "zero", "obs"))
}

# IMF monthly prices of the commodities the processed prices table carries: the mean over a
# commodity's series per month (coal: Australia and South Africa). monthly: the output of
# energy_prices_imf_clean() (INDICATOR, date, value, clean); commodities: sector_commodity_items().
sector_imf_monthly <- function(monthly, commodities, from_year = 2003L) {
  m <- monthly[monthly$clean %in% commodities$sub_sector & !is.na(monthly$value), , drop = FALSE]
  y <- as.integer(format(m$date, "%Y"))
  mo <- as.integer(format(m$date, "%m"))
  keep <- y >= from_year
  m <- m[keep, , drop = FALSE]
  a <- stats::aggregate(list(value = m$value), list(clean = m$clean, year = y[keep], month = mo[keep]), mean)
  tibble::tibble(var = "prices.monthly", item_key = paste0("commodity:", commodities$slug[match(a$clean, commodities$sub_sector)]),
                 geo = "WLD", year = as.integer(a$year), month = as.integer(a$month), value = a$value, status = "obs")
}
