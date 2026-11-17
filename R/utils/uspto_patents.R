# US patent grants by technology and inventor country: pure helpers (no IO).
#
# Used by scripts/fetchers/uspto_patents.R (builds the queries, parses the API records) and by
# scripts/29_build_innovation_position.R (turns the counts into the Innovation indicators).
# The technology classes live in config/patent_tech_cpc.yml.
#
# Counting rules
#   * Country = inventor country (OECD convention). An inventor's residence address wins over the
#     postal one.
#   * Whole counts: a grant counts 1 for every country with at least one inventor on it. Used for
#     revealed technological advantage, whose denominator (all utility grants by country) can only be
#     counted whole through the API.
#   * Fractional counts: each inventor carries 1 / (number of inventors), summed by country. Shares
#     of world patenting use these, so a country's shares add to its contribution and the world total
#     equals the number of grants.
#   * Rows with inventor_iso2 "WLD" hold the number of grants (the world total) for each tech/year.
#   * tech "_ALL" rows are all utility grants, whole-counted (fractional is NA).

USPTO_PATENTS_WORLD <- "WLD"
USPTO_PATENTS_ALL <- "_ALL"

# "Y02E10/5*" -> the ODP stored form "Y02E  10/5*" (main group right-aligned in four characters),
# with the spaces and slash escaped for the Lucene query parser.
uspto_cpc_pattern <- function(code) {
  code <- toupper(gsub("\\s+", "", code))
  m <- regmatches(code, regexec("^([A-HY][0-9]{2}[A-Z])([0-9]{1,4})?(/([0-9*]+))?(\\*)?$", code))[[1]]
  if (length(m) == 0) stop("Not a CPC pattern: ", code)
  subclass <- m[2]
  main <- m[3]
  sub <- m[5]
  star <- m[6]
  if (!nzchar(main)) return(paste0(subclass, "*"))
  stored <- paste0(subclass, formatC(main, width = 4), if (nzchar(sub)) paste0("/", sub) else "", star)
  stored <- gsub(" ", "\\\\ ", stored)
  gsub("/", "\\\\/", stored)
}

uspto_cpc_clause <- function(codes) {
  parts <- paste0("applicationMetaData.cpcClassificationBag:", vapply(codes, uspto_cpc_pattern, character(1)))
  paste0("(", paste(parts, collapse = " OR "), ")")
}

uspto_grant_clause <- function(from, to) {
  sprintf("applicationMetaData.grantDate:[%s TO %s]", format(as.Date(from)), format(as.Date(to)))
}

uspto_year_clause <- function(year) {
  uspto_grant_clause(sprintf("%d-01-01", year), sprintf("%d-12-31", year))
}

uspto_utility_clause <- function() "applicationMetaData.applicationTypeCode:UTL"

uspto_inventor_country_clause <- function(iso2) {
  sprintf("applicationMetaData.inventorBag.correspondenceAddressBag.countryCode:\"%s\"", toupper(iso2))
}

# Inventor countries of one API record (a parsed JSON list): one ISO2 code per inventor, NA when an
# inventor has no address.
uspto_record_inventor_countries <- function(record) {
  inventors <- record$applicationMetaData$inventorBag
  if (is.null(inventors) || length(inventors) == 0) return(character())
  vapply(inventors, function(inv) {
    addrs <- inv$correspondenceAddressBag
    if (is.null(addrs) || length(addrs) == 0) return(NA_character_)
    kind <- vapply(addrs, function(a) if (is.null(a$postalAddressCategory)) "" else a$postalAddressCategory, character(1))
    code <- vapply(addrs, function(a) if (is.null(a$countryCode)) NA_character_ else toupper(a$countryCode), character(1))
    pick <- c(code[kind == "residence"], code)
    pick <- pick[!is.na(pick) & nzchar(pick)]
    if (length(pick)) pick[[1]] else NA_character_
  }, character(1))
}

# Parsed API records -> one row per (application, country) with whole and fractional weights.
# Inventors without a country are dropped from the fraction's numerator but kept in its denominator,
# so a grant with an unknown inventor contributes less than 1 in total.
uspto_records_to_country_weights <- function(records) {
  rows <- lapply(records, function(rec) {
    app <- rec$applicationNumberText
    if (is.null(app)) app <- NA_character_
    countries <- uspto_record_inventor_countries(rec)
    if (length(countries) == 0) {
      return(data.frame(application = app, inventor_iso2 = NA_character_, whole = 0, fractional = 0,
                        stringsAsFactors = FALSE))
    }
    known <- countries[!is.na(countries)]
    if (length(known) == 0) {
      return(data.frame(application = app, inventor_iso2 = NA_character_, whole = 0, fractional = 0,
                        stringsAsFactors = FALSE))
    }
    frac <- tapply(rep(1 / length(countries), length(known)), known, sum)
    data.frame(application = app, inventor_iso2 = names(frac), whole = 1, fractional = as.numeric(frac),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# Country weights of one tech/year -> counts by country, plus the WLD row (number of grants).
uspto_aggregate_counts <- function(weights, tech, grant_year) {
  apps <- unique(weights$application)
  w <- weights[!is.na(weights$inventor_iso2), , drop = FALSE]
  if (nrow(w)) {
    agg <- stats::aggregate(cbind(whole, fractional) ~ inventor_iso2, data = w, FUN = sum)
  } else {
    agg <- data.frame(inventor_iso2 = character(), whole = numeric(), fractional = numeric())
  }
  world <- data.frame(inventor_iso2 = USPTO_PATENTS_WORLD, whole = length(apps), fractional = length(apps))
  out <- rbind(agg, world)
  data.frame(grant_year = as.integer(grant_year), tech = tech, inventor_iso2 = out$inventor_iso2,
             patents_whole = out$whole, patents_fractional = out$fractional, stringsAsFactors = FALSE)
}

# Sum counts over a window of grant years.
uspto_window_sum <- function(counts, years, value_col) {
  sub <- counts[counts$grant_year %in% years, , drop = FALSE]
  if (!nrow(sub)) return(data.frame(tech = character(), inventor_iso2 = character(), value = numeric()))
  agg <- stats::aggregate(sub[[value_col]], by = list(tech = sub$tech, inventor_iso2 = sub$inventor_iso2), FUN = sum)
  names(agg)[3] <- "value"
  agg
}

# Patent indicators by country and tech over a recent window, with growth against the window before.
#   share_world   fractional grants / world grants in the tech (absolute weight)
#   rta           revealed technological advantage on whole counts:
#                 (country tech / country all) / (world tech / world all)
#   growth        log(recent / earlier) of fractional counts, NA unless both windows have at least
#                 `min_patents` whole grants (a tiny base makes growth meaningless)
#   rta is NA when the country has fewer than `min_patents` whole grants in the tech in the window.
uspto_patent_indicators <- function(counts, recent_years, earlier_years, min_patents = 5) {
  techs <- setdiff(unique(counts$tech), USPTO_PATENTS_ALL)
  frac_recent <- uspto_window_sum(counts, recent_years, "patents_fractional")
  frac_earlier <- uspto_window_sum(counts, earlier_years, "patents_fractional")
  whole_recent <- uspto_window_sum(counts, recent_years, "patents_whole")
  whole_earlier <- uspto_window_sum(counts, earlier_years, "patents_whole")

  key <- function(t, c) paste(t, c, sep = "\r")
  lookup <- function(tbl, t, c) {
    v <- tbl$value[match(key(t, c), key(tbl$tech, tbl$inventor_iso2))]
    ifelse(is.na(v), 0, v)
  }

  countries <- setdiff(unique(counts$inventor_iso2[counts$tech %in% techs]), USPTO_PATENTS_WORLD)
  grid <- expand.grid(tech = techs, inventor_iso2 = countries, stringsAsFactors = FALSE)

  world_tech <- lookup(frac_recent, grid$tech, USPTO_PATENTS_WORLD)
  world_all <- lookup(whole_recent, USPTO_PATENTS_ALL, USPTO_PATENTS_WORLD)
  country_all <- lookup(whole_recent, USPTO_PATENTS_ALL, grid$inventor_iso2)

  grid$patents_fractional <- lookup(frac_recent, grid$tech, grid$inventor_iso2)
  grid$patents_whole <- lookup(whole_recent, grid$tech, grid$inventor_iso2)
  grid$patents_fractional_earlier <- lookup(frac_earlier, grid$tech, grid$inventor_iso2)
  grid$patents_whole_earlier <- lookup(whole_earlier, grid$tech, grid$inventor_iso2)
  grid$country_all_whole <- country_all

  grid$share_world <- ifelse(world_tech > 0, grid$patents_fractional / world_tech, NA_real_)
  ok_rta <- grid$patents_whole >= min_patents & country_all > 0 & world_tech > 0 & world_all > 0
  world_tech_whole <- lookup(whole_recent, grid$tech, USPTO_PATENTS_WORLD)
  grid$rta <- ifelse(ok_rta, (grid$patents_whole / country_all) / (world_tech_whole / world_all), NA_real_)
  ok_growth <- grid$patents_whole >= min_patents & grid$patents_whole_earlier >= min_patents
  grid$growth <- ifelse(ok_growth, log(grid$patents_fractional / grid$patents_fractional_earlier), NA_real_)
  grid[order(grid$tech, -grid$share_world), , drop = FALSE]
}

# Score the indicators within each tech, CFR-style: 80% level, 20% change.
#   level = mean of median_scurve(share_world) and median_scurve(rta) (rta missing -> scored as 0)
#   index = 0.8 * level + 0.2 * median_scurve(growth) (growth missing -> level alone)
# Scored over the countries passed in; pass the pipeline's country set, with zeros for countries
# that have no grants, so the curve's reference is the same as the rest of the index.
uspto_patent_scores <- function(indicators, level_weight = 0.8) {
  if (!exists("median_scurve", mode = "function")) stop("median_scurve() is required (R/utils/scurve.R).")
  out <- split(indicators, indicators$tech)
  out <- lapply(out, function(d) {
    s_share <- median_scurve(d$share_world)
    # Too few grants for an RTA (NA) earns no specialisation credit: scored as RTA 0, so a country
    # with one or two grants is not ranked on its share alone above countries with real portfolios.
    s_rta <- median_scurve(ifelse(is.na(d$rta), 0, d$rta))
    s_growth <- median_scurve(d$growth)
    level <- (s_share + s_rta) / 2
    d$share_score <- s_share
    d$rta_score <- s_rta
    d$growth_score <- s_growth
    d$innovation_index <- ifelse(is.na(s_growth), level, level_weight * level + (1 - level_weight) * s_growth)
    d
  })
  do.call(rbind, unname(out))
}

# Countries that patent enough in the US to be compared: at least `min_total` whole utility grants
# in the recent window. Small filers get NA, not a low score.
uspto_patent_universe <- function(indicators, min_total = 100) {
  unique(indicators$inventor_iso2[indicators$country_all_whole >= min_total])
}

# Scores -> the standard theme table (R/utils/schema.R). Patents describe a technology, not a stage,
# so supply_chain is "All"; which stages the Innovation category feeds is a wiring decision.
uspto_patent_theme_table <- function(scores, year, window_label) {
  long <- function(col, variable, data_type, explanation) {
    data.frame(
      Country = scores$Country, tech = scores$tech, supply_chain = "All", category = "Innovation",
      variable = variable, data_type = data_type, value = as.numeric(scores[[col]]), Year = as.integer(year),
      source = "USPTO Open Data Portal, granted utility patents (CPC Y02/Y04S)",
      explanation = paste0(explanation, " Grants ", window_label, "."), stringsAsFactors = FALSE)
  }
  out <- rbind(
    long("patents_fractional", "US patent grants (fractional, inventor country)", "raw",
         "Granted US utility patents in the technology's CPC groups, fractional count by inventor country."),
    long("share_world", "Share of world US patent grants", "raw",
         "Fractional grants / all grants in the technology."),
    long("rta", "Revealed technological advantage (US grants)", "raw",
         "(Country grants in tech / country grants in all fields) / (world tech / world all), whole counts; NA under 5 grants."),
    long("growth", "Patent growth (log, recent vs five years earlier)", "raw",
         "log of fractional grants in the recent window over the window five years before; NA under 5 grants in either."),
    long("innovation_index", "Patent Innovation Index", "index",
         "0.8 x mean(median_scurve(share), median_scurve(RTA, NA as 0)) + 0.2 x median_scurve(growth), within technology.")
  )
  out[!is.na(out$Country), , drop = FALSE]
}
