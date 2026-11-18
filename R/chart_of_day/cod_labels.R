# Chart of the Day: plain-language labels and number formatting (pure).

COD_STAGES <- c("Extraction", "Refining", "Components", "Assembly", "Deployment", "Recovery")
COD_TRADE_STAGES <- c("Extraction", "Refining", "Components", "Assembly", "Recovery")

COD_TECH_NOUN <- c(Batteries = "battery", Solar = "solar", `Electric Vehicles` = "electric-vehicle", `Electric Grid` = "grid",
                   Wind = "wind", Nuclear = "nuclear", Semiconductors = "semiconductor", Gas = "gas", Oil = "oil",
                   Coal = "coal", `Green Hydrogen` = "hydrogen", Geothermal = "geothermal")

# What each technology's chain feeds, for the materials steps ("raw materials for batteries").
COD_TECH_OBJECT <- c(Batteries = "batteries", Solar = "solar power", `Electric Vehicles` = "electric vehicles", `Electric Grid` = "the grid",
                     Wind = "wind power", Nuclear = "nuclear power", Semiconductors = "semiconductors", `Green Hydrogen` = "hydrogen",
                     Geothermal = "geothermal power", Gas = "gas", Oil = "oil", Coal = "coal")

# The cells whose generic phrase would mislead or read badly.
COD_CELL_OVERRIDE <- c(`Batteries|Assembly` = "battery cells and packs", `Electric Vehicles|Assembly` = "electric vehicles",
                       `Electric Vehicles|Components` = "electric-vehicle parts",
                       `Oil|Extraction` = "crude oil", `Gas|Extraction` = "natural gas", `Coal|Extraction` = "coal",
                       `Oil|Refining` = "refined oil products", `Gas|Refining` = "processed gas", `Coal|Refining` = "processed coal",
                       `Nuclear|Extraction` = "uranium ore", `Nuclear|Refining` = "nuclear fuel materials",
                       `Solar|Assembly` = "solar modules and inverters", `Wind|Assembly` = "wind turbines and towers",
                       `Semiconductors|Assembly` = "finished chips", `Semiconductors|Components` = "chip components",
                       `Electric Grid|Assembly` = "finished grid equipment")

#' Plain phrase for a technology x stage cell, e.g. "processed materials for batteries".
cod_cell_phrase <- function(tech, stage) {
  key <- paste(tech, stage, sep = "|")
  if (!is.na(COD_CELL_OVERRIDE[key])) return(unname(COD_CELL_OVERRIDE[key]))
  n <- if (!is.na(COD_TECH_NOUN[tech])) unname(COD_TECH_NOUN[tech]) else tolower(tech)
  o <- if (!is.na(COD_TECH_OBJECT[tech])) unname(COD_TECH_OBJECT[tech]) else tolower(tech)
  switch(stage,
    Extraction = sprintf("raw materials for %s", o),
    Refining = sprintf("processed materials for %s", o),
    Components = sprintf("%s components", n),
    Assembly = sprintf("finished %s equipment", n),
    Recovery = sprintf("%s scrap and waste", n),
    Deployment = sprintf("new %s projects", n),
    Manufacturing = sprintf("%s manufacturing", n),
    sprintf("%s (%s)", n, tolower(stage)))
}

#' Short stage label for axes.
cod_stage_label <- function(stage, tech = NULL) {
  lab <- c(Extraction = "Raw materials", Refining = "Refining", Components = "Components", Assembly = "Final products",
           Recovery = "Scrap & recycling", Deployment = "Deployment", Manufacturing = "Manufacturing")
  out <- unname(lab[stage])
  out[is.na(out)] <- stage[is.na(out)]
  out
}

cod_ordinal <- function(k) {
  k <- as.integer(k)
  suf <- ifelse(k %% 100 %in% 11:13, "th", c("th", "st", "nd", "rd", rep("th", 6))[k %% 10 + 1])
  paste0(k, suf)
}

#' "$12.3bn", "$450m", "$3.1k".
cod_usd <- function(x, digits = 1) {
  vapply(x, function(v) {
    if (is.na(v)) return("n/a")
    a <- abs(v); s <- if (v < 0) "-" else ""
    if (a >= 1e12) sprintf("%s$%strn", s, formatC(a / 1e12, format = "f", digits = digits))
    else if (a >= 1e9) sprintf("%s$%sbn", s, formatC(a / 1e9, format = "f", digits = digits))
    else if (a >= 1e6) sprintf("%s$%sm", s, formatC(a / 1e6, format = "f", digits = 0))
    else sprintf("%s$%s", s, formatC(a, format = "d", big.mark = ","))
  }, character(1))
}

cod_pct <- function(x, digits = 0) ifelse(is.na(x), "n/a", paste0(formatC(100 * x, format = "f", digits = digits), "%"))

#' Short display name for a country (keeps titles tight).
cod_country_name <- function(iso3, countries) {
  short <- c(USA = "the US", GBR = "the UK", KOR = "South Korea", ARE = "the UAE", NLD = "the Netherlands",
             PHL = "the Philippines", CZE = "Czechia", RUS = "Russia", TWN = "Taiwan", EUU = "the EU",
             VNM = "Vietnam", TUR = "T\u00fcrkiye", IRN = "Iran", VEN = "Venezuela", BOL = "Bolivia", TZA = "Tanzania",
             COD = "DR Congo", LAO = "Laos")
  out <- countries$country[match(iso3, countries$iso3)]
  out[is.na(out)] <- iso3[is.na(out)]
  s <- unname(short[iso3])
  out[!is.na(s)] <- s[!is.na(s)]
  out
}

#' Country name for chart labels (no article).
cod_country_label <- function(iso3, countries) {
  x <- cod_country_name(iso3, countries)
  x <- sub("^the ", "", x)
  ifelse(x == "US", "United States", ifelse(x == "UK", "United Kingdom", ifelse(x == "UAE", "United Arab Emirates", x)))
}

cod_cap1 <- function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))

#' Possessive of a country name ("the US's" -> "US").
cod_poss <- function(name) paste0(name, if (grepl("s$", name)) "'" else "'s")
