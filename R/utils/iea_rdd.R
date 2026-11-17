# IEA Energy Technology RD&D Budgets: pure helpers (no IO).
#
# Used by scripts/29_build_innovation_position.R. Technology codes live in config/iea_rdd_tech_map.yml.
#
# Measure: public sector (central government plus state), research + development + demonstration,
# USD millions at constant prices and exchange rates (UNIT USD_R). GDP intensity uses the IEA's own
# "units of GDP" series for the country total, so no outside GDP series is needed: intensity of a
# technology = (technology / country total) x (country total per unit of GDP).

IEA_RDD_COUNTRY_ISO3 <- c(
  AUSTRALIA = "AUS", AUSTRIA = "AUT", BELGIUM = "BEL", BRAZIL = "BRA", CANADA = "CAN", CHILE = "CHL",
  CZECH = "CZE", DENMARK = "DNK", ESTONIA = "EST", FINLAND = "FIN", FRANCE = "FRA", GERMANY = "DEU",
  GREECE = "GRC", HUNGARY = "HUN", IRELAND = "IRL", ITALY = "ITA", JAPAN = "JPN", KOREA = "KOR",
  LATVIA = "LVA", LITHUANIA = "LTU", LUXEMBOURG = "LUX", MEXICO = "MEX", NETHERLANDS = "NLD",
  NEWZEALAND = "NZL", NORWAY = "NOR", POLAND = "POL", PORTUGAL = "PRT", SLOVAKIA = "SVK", SPAIN = "ESP",
  SWEDEN = "SWE", SWITZERLAND = "CHE", TURKIYE = "TUR", UK = "GBR", USA = "USA"
)

# Raw RDDPUBLIC rows -> country (ISO3) x code x year values for the public RD&D measure.
# Returns columns iso3, code, year, usd_m (USD_R) and, for code TOTAL only, gdp_units (U_GDP).
iea_rdd_public_values <- function(raw) {
  keep <- raw$RDD_SECTOR == "PUBLIC" & raw$RDD_TYPE == "RDD" & raw$UNIT %in% c("USD_R", "U_GDP")
  d <- raw[keep, , drop = FALSE]
  d$iso3 <- unname(IEA_RDD_COUNTRY_ISO3[d$COUNTRY])
  d <- d[!is.na(d$iso3), , drop = FALSE]
  d$year <- as.integer(d$TIME_PERIOD)
  d$value <- suppressWarnings(as.numeric(d$OBS_VALUE))
  usd <- d[d$UNIT == "USD_R", c("iso3", "RDD_TECH", "year", "value")]
  names(usd) <- c("iso3", "code", "year", "usd_m")
  gdp <- d[d$UNIT == "U_GDP" & d$RDD_TECH == "TOTAL", c("iso3", "year", "value")]
  names(gdp) <- c("iso3", "year", "gdp_units")
  out <- merge(usd, gdp, by = c("iso3", "year"), all.x = TRUE)
  out$gdp_units[out$code != "TOTAL"] <- NA_real_
  out[order(out$iso3, out$code, out$year), , drop = FALSE]
}

# Parent of each code in the IEA tree: the longest other code that is a proper prefix of it; one-letter
# codes and "UN" hang off TOTAL. "<parent>UN" codes are the parent's unallocated remainder.
iea_rdd_parent <- function(codes) {
  codes <- unique(codes)
  allocated <- setdiff(codes[!grepl("UN$", codes)], "TOTAL")
  vapply(codes, function(c) {
    if (c == "TOTAL") return(NA_character_)
    if (grepl("UN$", c)) {
      p <- sub("UN$", "", c)
      return(if (nzchar(p)) p else "TOTAL")
    }
    cand <- allocated[startsWith(c, allocated) & allocated != c]
    if (length(cand)) cand[which.max(nchar(cand))] else "TOTAL"
  }, character(1))
}

# Push every parent's budget down to its allocated children, so the "unallocated" remainders follow
# the allocated split instead of vanishing from a leaf-code sum. Top-down from TOTAL: a child gets
#   parent_adjusted x child / sum(allocated children)
# using the country-year's own split; when the country allocated nothing below that parent, the split
# of all reporters that year; when nobody did, the children keep their own values. Adds usd_adj.
# Assumption, stated in the outputs: unallocated money is spent like allocated money.
iea_rdd_allocate <- function(values) {
  values$usd_adj <- values$usd_m
  codes <- unique(values$code)
  parent <- iea_rdd_parent(codes)
  child_codes <- split(names(parent)[!is.na(parent) & !grepl("UN$", names(parent))],
                       parent[!is.na(parent) & !grepl("UN$", names(parent))])
  depth <- function(p) if (p == "TOTAL") 0L else nchar(p)
  parents <- names(child_codes)[order(vapply(names(child_codes), depth, integer(1)))]

  key <- paste(values$iso3, values$year, sep = "|")
  for (p in parents) {
    kids <- child_codes[[p]]
    is_p <- values$code == p
    is_k <- values$code %in% kids
    if (!any(is_p) || !any(is_k)) next
    pk <- key[is_p]
    p_adj <- stats::setNames(values$usd_adj[is_p], pk)
    kid_raw <- values$usd_m[is_k]
    kid_raw[is.na(kid_raw)] <- 0
    kid_key <- key[is_k]
    own_sum <- tapply(kid_raw, kid_key, sum)
    # Reporter-wide weights by year and child, for country-years with nothing allocated.
    kid_year <- values$year[is_k]
    pool <- tapply(kid_raw, paste(kid_year, values$code[is_k]), sum)
    pool_sum <- tapply(kid_raw, kid_year, sum)
    w_own <- ifelse(own_sum[kid_key] > 0, kid_raw / own_sum[kid_key], NA_real_)
    w_pool <- pool[paste(kid_year, values$code[is_k])] / pool_sum[as.character(kid_year)]
    w <- ifelse(is.na(w_own), ifelse(is.finite(w_pool), w_pool, NA_real_), w_own)
    target <- p_adj[kid_key]
    new <- ifelse(!is.na(target) & !is.na(w), target * w, values$usd_adj[is_k])
    values$usd_adj[is_k] <- unname(new)
  }
  values
}

# Codes nest by prefix (C1 > C12 > C121); a code list must not hold a code and one of its children.
iea_rdd_codes_nested <- function(codes) {
  codes <- unique(codes)
  any(vapply(codes, function(a) any(startsWith(setdiff(codes, a), a)), logical(1)))
}

# Sum each technology's codes: tech x country x year. `tech_codes` is a named list of code vectors.
# A country-year with a reported TOTAL but no row for a code counts that code as 0.
iea_rdd_tech_values <- function(values, tech_codes) {
  for (t in names(tech_codes)) {
    if (iea_rdd_codes_nested(tech_codes[[t]])) stop("Nested IEA RD&D codes for ", t, ": ", paste(tech_codes[[t]], collapse = ", "))
  }
  # Sum the allocated values (iea_rdd_allocate()) when present, else the reported ones.
  if (!is.null(values$usd_adj)) values$usd_m <- ifelse(values$code == "TOTAL", values$usd_m, values$usd_adj)
  totals <- values[values$code == "TOTAL" & !is.na(values$usd_m), c("iso3", "year", "usd_m", "gdp_units")]
  names(totals)[3] <- "total_usd_m"
  rows <- lapply(names(tech_codes), function(t) {
    sub <- values[values$code %in% tech_codes[[t]] & !is.na(values$usd_m), , drop = FALSE]
    agg <- if (nrow(sub)) stats::aggregate(usd_m ~ iso3 + year, data = sub, FUN = sum) else
      data.frame(iso3 = character(), year = integer(), usd_m = numeric())
    m <- merge(totals, agg, by = c("iso3", "year"), all.x = TRUE)
    m$usd_m[is.na(m$usd_m)] <- 0
    m$tech <- t
    m
  })
  out <- do.call(rbind, rows)
  out[, c("tech", "iso3", "year", "usd_m", "total_usd_m", "gdp_units")]
}

# Indicators by tech and country over a window, with growth against an earlier window. Each country
# is averaged over the years it reported in the window (budgets are reported with gaps and lags).
#   usd_m           mean annual public RD&D, USD m constant
#   share_reporting share of the reporting countries' total (only IEA reporters: no US, China, India)
#   intensity       per unit of GDP (scaled by the IEA's GDP units; comparable across countries)
#   specialisation  (tech / country total) / (sum of tech / sum of totals) over reporters
#   growth          log(recent / earlier) when both windows exceed `min_usd_m`
iea_rdd_indicators <- function(tech_values, recent_years, earlier_years, min_usd_m = 1) {
  win_mean <- function(years) {
    sub <- tech_values[tech_values$year %in% years, , drop = FALSE]
    sub$intensity <- ifelse(sub$total_usd_m > 0, sub$usd_m / sub$total_usd_m * sub$gdp_units, NA_real_)
    stats::aggregate(cbind(usd_m, total_usd_m, intensity) ~ tech + iso3, data = sub, FUN = mean, na.action = stats::na.pass)
  }
  recent <- win_mean(recent_years)
  earlier <- win_mean(earlier_years)
  names(earlier)[names(earlier) == "usd_m"] <- "usd_m_earlier"
  out <- merge(recent, earlier[, c("tech", "iso3", "usd_m_earlier")], by = c("tech", "iso3"), all.x = TRUE)

  tech_sum <- tapply(out$usd_m, out$tech, sum)
  tot_sum <- tapply(out$total_usd_m, out$tech, sum)
  out$share_reporting <- out$usd_m / tech_sum[out$tech]
  base <- tech_sum[out$tech] / tot_sum[out$tech]
  out$specialisation <- ifelse(out$total_usd_m > 0 & base > 0, (out$usd_m / out$total_usd_m) / base, NA_real_)
  ok <- !is.na(out$usd_m_earlier) & out$usd_m > min_usd_m & out$usd_m_earlier > min_usd_m
  out$growth <- ifelse(ok, log(out$usd_m / out$usd_m_earlier), NA_real_)
  out[order(out$tech, -out$usd_m), , drop = FALSE]
}

# Score within each tech, CFR-style: 0.8 x mean(scurve(share), scurve(intensity)) + 0.2 x scurve(growth).
iea_rdd_scores <- function(indicators, level_weight = 0.8) {
  if (!exists("median_scurve", mode = "function")) stop("median_scurve() is required (R/utils/scurve.R).")
  out <- lapply(split(indicators, indicators$tech), function(d) {
    s_share <- median_scurve(d$share_reporting)
    s_int <- median_scurve(d$intensity)
    s_growth <- median_scurve(d$growth)
    level <- ifelse(is.na(s_int), s_share, (s_share + s_int) / 2)
    d$share_score <- s_share
    d$intensity_score <- s_int
    d$growth_score <- s_growth
    d$rdd_index <- ifelse(is.na(s_growth), level, level_weight * level + (1 - level_weight) * s_growth)
    d
  })
  do.call(rbind, unname(out))
}

iea_rdd_theme_table <- function(scores, year, window_label, supply_chain = "All") {
  sc <- if (!is.null(scores$supply_chain)) scores$supply_chain else supply_chain
  long <- function(col, variable, data_type, explanation) {
    data.frame(
      Country = scores$Country, tech = scores$tech, supply_chain = sc, category = "Innovation",
      variable = variable, data_type = data_type, value = as.numeric(scores[[col]]), Year = as.integer(year),
      source = "IEA Energy Technology RD&D Budgets (public sector, USD constant)",
      explanation = paste0(explanation, " Window ", window_label, "."), stringsAsFactors = FALSE)
  }
  out <- rbind(
    long("usd_m", "Public RD&D (USD m, constant)", "raw", "Mean annual public RD&D on the technology's IEA codes."),
    long("share_reporting", "Share of reporting countries' public RD&D", "raw", "IEA reporters only: no US after 2015, no China or India."),
    long("intensity", "Public RD&D per unit of GDP", "raw", "Technology share of the country's RD&D x its RD&D per unit of GDP (IEA units)."),
    long("specialisation", "Public RD&D specialisation", "raw", "(Tech / country total) / (all reporters' tech / all reporters' total)."),
    long("growth", "Public RD&D growth (log, recent vs five years earlier)", "raw", "NA when either window is under USD 1m."),
    long("rdd_index", "Public RD&D Index", "index", "0.8 x mean(median_scurve(share), median_scurve(intensity)) + 0.2 x median_scurve(growth), within technology.")
  )
  out[!is.na(out$Country), , drop = FALSE]
}
