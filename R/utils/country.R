# Country standardization helpers.
#
# Sources spell economies differently (EI "Vietnam" and "Slovakia", WDI "Viet Nam" and
# "Slovak Republic", IEA "Taiwan, Province of China"). The index tables key on the Country
# string, so every spelling that survived became a separate pseudo-country with its own
# imputed profile: the 2026-09 build carried 270+ strings for about 200 economies.
# standardize_country_names() maps every spelling of an economy to one canonical name, the
# name country_info (WDI) already carries after country_recode_map(), so joins against
# country_info keep working:
#
#   1. trim, collapse whitespace, strip footnote digits, apply country_recode_map();
#   2. keep names that are already canonical;
#   3. leave aggregates, residual rows and historical entities alone - theme builders still
#      need "World" and "European Union" rows - and let standardize_theme_types() drop them
#      at the end of each theme (is_aggregate_or_residual());
#   4. match everything else to an ISO3 code with countrycode, plus
#      country_iso3_custom_match() for the names its regexes miss or conflate, and rename it
#      to that code's canonical name.
#
# Names that match nothing come back cleaned but otherwise unchanged. Pass country columns
# only: countrycode's regexes are loose enough to read "Solar cooking" as the Cook Islands,
# so technology, product or sector labels must not go through here.

country_recode_map <- function() {
  c(
    "United States of America" = "United States",
    "US" = "United States",
    "USA" = "United States",
    "UK" = "United Kingdom",
    "United kingdom" = "United Kingdom",
    "Korea" = "South Korea",
    "Korea, Rep." = "South Korea",
    "Republic of Korea" = "South Korea",
    "Korea, Dem. People's Rep." = "North Korea",
    # WDI country_info names it "Viet Nam"; without this, tables standardized without a
    # country_info join keep "Vietnam" and never meet the rest of the pipeline.
    "Vietnam" = "Viet Nam",
    "Iran, Islamic Rep." = "Iran",
    "Turkey" = "Turkiye",
    # The EI mineral sheets spell it with the diaeresis; the EI and WDI country rosters
    # do not, so the two never matched.
    "Türkiye" = "Turkiye",
    "Curacao" = "Curaçao",
    "Saudi arabia" = "Saudi Arabia",
    "Russian Federation" = "Russia",
    "Czechia" = "Czech Republic",
    "Yemen, Rep." = "Yemen",
    "Venezuela, RB" = "Venezuela",
    "Egypt, Arab Rep." = "Egypt",
    "Gambia, The" = "Gambia",
    "Bahamas, The" = "Bahamas",
    "Hong Kong SAR, China" = "Hong Kong",
    "Macao SAR, China" = "Macau",
    "Micronesia, Fed. Sts." = "Micronesia",
    "Congo, Dem. Rep." = "Democratic Republic of Congo",
    "Congo, Rep." = "Congo",
    # The IEA critical minerals workbook spells this out in full; WDI, the country
    # reference, calls it "Lao PDR".
    "Lao People's Democratic Republic" = "Lao PDR"
  )
}

# Canonical display name per ISO3 code: the WDI economy roster (the non-aggregate rows of
# data/raw/wdi_country_info.csv) after country_recode_map(), i.e. the name country_info
# already carries through the pipeline, plus Taiwan, which WDI does not list. Codes outside
# this table fall back to countrycode's English name. tests/testthat/test-country-harmonization.R
# checks the table against the staged WDI file, so a WDI rename shows up as a test failure.
country_canonical_names <- function() {
  c(
    ABW = "Aruba",
    AFG = "Afghanistan",
    AGO = "Angola",
    ALB = "Albania",
    AND = "Andorra",
    ARE = "United Arab Emirates",
    ARG = "Argentina",
    ARM = "Armenia",
    ASM = "American Samoa",
    ATG = "Antigua and Barbuda",
    AUS = "Australia",
    AUT = "Austria",
    AZE = "Azerbaijan",
    BDI = "Burundi",
    BEL = "Belgium",
    BEN = "Benin",
    BFA = "Burkina Faso",
    BGD = "Bangladesh",
    BGR = "Bulgaria",
    BHR = "Bahrain",
    BHS = "Bahamas",
    BIH = "Bosnia and Herzegovina",
    BLR = "Belarus",
    BLZ = "Belize",
    BMU = "Bermuda",
    BOL = "Bolivia",
    BRA = "Brazil",
    BRB = "Barbados",
    BRN = "Brunei Darussalam",
    BTN = "Bhutan",
    BWA = "Botswana",
    CAF = "Central African Republic",
    CAN = "Canada",
    CHE = "Switzerland",
    CHI = "Channel Islands",
    CHL = "Chile",
    CHN = "China",
    CIV = "Cote d'Ivoire",
    CMR = "Cameroon",
    COD = "Democratic Republic of Congo",
    COG = "Congo",
    COL = "Colombia",
    COM = "Comoros",
    CPV = "Cabo Verde",
    CRI = "Costa Rica",
    CUB = "Cuba",
    CUW = "Curaçao",
    CYM = "Cayman Islands",
    CYP = "Cyprus",
    CZE = "Czech Republic",
    DEU = "Germany",
    DJI = "Djibouti",
    DMA = "Dominica",
    DNK = "Denmark",
    DOM = "Dominican Republic",
    DZA = "Algeria",
    ECU = "Ecuador",
    EGY = "Egypt",
    ERI = "Eritrea",
    ESP = "Spain",
    EST = "Estonia",
    ETH = "Ethiopia",
    FIN = "Finland",
    FJI = "Fiji",
    FRA = "France",
    FRO = "Faroe Islands",
    FSM = "Micronesia",
    GAB = "Gabon",
    GBR = "United Kingdom",
    GEO = "Georgia",
    GHA = "Ghana",
    GIB = "Gibraltar",
    GIN = "Guinea",
    GMB = "Gambia",
    GNB = "Guinea-Bissau",
    GNQ = "Equatorial Guinea",
    GRC = "Greece",
    GRD = "Grenada",
    GRL = "Greenland",
    GTM = "Guatemala",
    GUM = "Guam",
    GUY = "Guyana",
    HKG = "Hong Kong",
    HND = "Honduras",
    HRV = "Croatia",
    HTI = "Haiti",
    HUN = "Hungary",
    IDN = "Indonesia",
    IMN = "Isle of Man",
    IND = "India",
    IRL = "Ireland",
    IRN = "Iran",
    IRQ = "Iraq",
    ISL = "Iceland",
    ISR = "Israel",
    ITA = "Italy",
    JAM = "Jamaica",
    JOR = "Jordan",
    JPN = "Japan",
    KAZ = "Kazakhstan",
    KEN = "Kenya",
    KGZ = "Kyrgyz Republic",
    KHM = "Cambodia",
    KIR = "Kiribati",
    KNA = "St. Kitts and Nevis",
    KOR = "South Korea",
    KWT = "Kuwait",
    LAO = "Lao PDR",
    LBN = "Lebanon",
    LBR = "Liberia",
    LBY = "Libya",
    LCA = "St. Lucia",
    LIE = "Liechtenstein",
    LKA = "Sri Lanka",
    LSO = "Lesotho",
    LTU = "Lithuania",
    LUX = "Luxembourg",
    LVA = "Latvia",
    MAC = "Macau",
    MAF = "St. Martin (French part)",
    MAR = "Morocco",
    MCO = "Monaco",
    MDA = "Moldova",
    MDG = "Madagascar",
    MDV = "Maldives",
    MEX = "Mexico",
    MHL = "Marshall Islands",
    MKD = "North Macedonia",
    MLI = "Mali",
    MLT = "Malta",
    MMR = "Myanmar",
    MNE = "Montenegro",
    MNG = "Mongolia",
    MNP = "Northern Mariana Islands",
    MOZ = "Mozambique",
    MRT = "Mauritania",
    MUS = "Mauritius",
    MWI = "Malawi",
    MYS = "Malaysia",
    NAM = "Namibia",
    NCL = "New Caledonia",
    NER = "Niger",
    NGA = "Nigeria",
    NIC = "Nicaragua",
    NLD = "Netherlands",
    NOR = "Norway",
    NPL = "Nepal",
    NRU = "Nauru",
    NZL = "New Zealand",
    OMN = "Oman",
    PAK = "Pakistan",
    PAN = "Panama",
    PER = "Peru",
    PHL = "Philippines",
    PLW = "Palau",
    PNG = "Papua New Guinea",
    POL = "Poland",
    PRI = "Puerto Rico",
    PRK = "North Korea",
    PRT = "Portugal",
    PRY = "Paraguay",
    PSE = "West Bank and Gaza",
    PYF = "French Polynesia",
    QAT = "Qatar",
    ROU = "Romania",
    RUS = "Russia",
    RWA = "Rwanda",
    SAU = "Saudi Arabia",
    SDN = "Sudan",
    SEN = "Senegal",
    SGP = "Singapore",
    SLB = "Solomon Islands",
    SLE = "Sierra Leone",
    SLV = "El Salvador",
    SMR = "San Marino",
    SOM = "Somalia",
    SRB = "Serbia",
    SSD = "South Sudan",
    STP = "Sao Tome and Principe",
    SUR = "Suriname",
    SVK = "Slovak Republic",
    SVN = "Slovenia",
    SWE = "Sweden",
    SWZ = "Eswatini",
    SXM = "Sint Maarten (Dutch part)",
    SYC = "Seychelles",
    SYR = "Syrian Arab Republic",
    TCA = "Turks and Caicos Islands",
    TCD = "Chad",
    TGO = "Togo",
    THA = "Thailand",
    TJK = "Tajikistan",
    TKM = "Turkmenistan",
    TLS = "Timor-Leste",
    TON = "Tonga",
    TTO = "Trinidad and Tobago",
    TUN = "Tunisia",
    TUR = "Turkiye",
    TUV = "Tuvalu",
    # Not in WDI. countrycode already reads "Taiwan", "Chinese Taipei" and "Taiwan, Province
    # of China" as TWN; pinning the name here keeps it stable across countrycode releases.
    TWN = "Taiwan",
    TZA = "Tanzania",
    UGA = "Uganda",
    UKR = "Ukraine",
    URY = "Uruguay",
    USA = "United States",
    UZB = "Uzbekistan",
    VCT = "St. Vincent and the Grenadines",
    VEN = "Venezuela",
    VGB = "British Virgin Islands",
    VIR = "Virgin Islands (U.S.)",
    VNM = "Viet Nam",
    VUT = "Vanuatu",
    WSM = "Samoa",
    XKX = "Kosovo",
    YEM = "Yemen",
    ZAF = "South Africa",
    ZMB = "Zambia",
    ZWE = "Zimbabwe"
  )
}

# Exact-name ISO3 codes for spellings countrycode's regexes miss, and for the hard cases
# whose code should not depend on a countrycode release. Canonical names never reach this
# table (they resolve through country_canonical_names(), which is how the bare
# "Micronesia", "Kosovo" and "Channel Islands" get FSM, XKX and CHI). Entities that must
# never be merged into another economy (USSR -> RUS, Northern Cyprus -> CYP) are not
# listed: is_aggregate_or_residual() stops them before countrycode is consulted.
country_iso3_custom_match <- function() {
  c(
    "China (Mainland)" = "CHN",
    "Mainland China" = "CHN",
    "Chinese Taipei" = "TWN",
    "Taiwan, Province of China" = "TWN",
    "Taiwan, China" = "TWN",
    # countrycode knows Kosovo but issues no iso3c for it; XKX is the code WDI uses.
    "Republic of Kosovo" = "XKX",
    "Kosovo (under UNSCR 1244)" = "XKX",
    # Island-level label for French Polynesia in the solar and investment sources.
    "Tahiti" = "PYF",
    # Comtrade partner labels.
    "Br. Virgin Isds" = "VGB",
    "FS Micronesia" = "FSM",
    # IMF long forms; countrycode sees two countries in each and returns nothing.
    "Kosovo, Republic of" = "XKX",
    "Hong Kong Special Administrative Region, People's Republic of China" = "HKG",
    "Macao Special Administrative Region, People's Republic of China" = "MAC",
    "Anguilla, United Kingdom-British Overseas Territory" = "AIA",
    "Montserrat, United Kingdom-British Overseas Territory" = "MSR",
    # Nauru's own name, as it appears in the staged WDI GDP extract.
    "Naoero" = "NRU"
  )
}

# WDI aggregate codes (region == "Aggregates" in wdi_country_info.csv). A table that
# carries WDI codes can carry "WLD" or "EUU" rows even when their names are unfamiliar.
country_aggregate_iso3 <- function() {
  c(
    "AFE", "AFR", "AFW", "ARB", "BEA", "BEC", "BHI", "BLA", "BMN", "BSS", "CAA", "CEA",
    "CEB", "CEU", "CLA", "CME", "CSA", "CSS", "DEA", "DEC", "DLA", "DMN", "DNS", "DSA",
    "DSF", "DSS", "EAP", "EAR", "EAS", "ECA", "ECS", "EMU", "EUU", "FCS", "FXS", "HIC",
    "HPC", "IBB", "IBD", "IBT", "IDA", "IDB", "IDX", "INX", "LAC", "LCN", "LDC", "LIC",
    "LMC", "LMY", "LTE", "MDE", "MEA", "MIC", "MNA", "NAC", "NAF", "NRS", "NXS", "OED",
    "OSS", "PRE", "PSS", "PST", "RRS", "SAS", "SSA", "SSF", "SST", "SXZ", "TEA", "TEC",
    "TLA", "TMN", "TSA", "TSS", "UMC", "WLD", "XZN"
  )
}

# Lower-case names that are not a single present-day economy. Matched exactly, because a
# substring rule on region words would also catch "South Africa" or "American Samoa".
country_aggregate_names <- function() {
  c(
    # World, blocs and groupings.
    "world", "global", "total", "eu", "eu27", "eu-27", "eu 27", "eu28", "eu-28",
    "european union", "euro area", "eurozone", "g7", "g8", "g20", "brics", "gcc",
    "gulf cooperation council", "asean", "apec", "cis", "commonwealth of independent states",
    "mena", "menat", "aggregates", "international", "bunkers", "free zones",
    "special categories", "unspecified", "unknown", "not specified", "not classified",
    "other", "others", "row", "advanced economies", "developed economies",
    "developing economies", "emerging markets", "emerging market and developing economies",
    "developing asia", "arab world", "arab states", "middle east and caspian",
    # Greater China is China plus Hong Kong, Macau and Taiwan; countrycode reads it as China.
    "greater china",
    # Continents and regions.
    "africa", "asia", "europe", "oceania", "americas", "america", "north america",
    "northern america", "south america", "central america", "latin america",
    "latin america and the caribbean", "latin america & caribbean", "caribbean",
    "middle east", "near east", "asia pacific", "asia-pacific", "asia and pacific",
    "eurasia", "sub-saharan africa", "southeast asia", "south-east asia", "south east asia",
    "south-eastern asia", "south asia", "southern asia", "east asia", "eastern asia",
    "central asia", "western asia", "west asia", "western europe", "eastern europe",
    "northern europe", "southern europe", "central europe", "north africa",
    "northern africa", "southern africa", "west africa", "western africa", "east africa",
    "eastern africa", "middle africa", "central africa", "central and south america",
    "central & south america", "south and central america", "s. & cent. america",
    "melanesia", "polynesia", "australia and new zealand", "middle east and north africa",
    "middle east & north africa", "europe & central asia", "east asia & pacific",
    "africa eastern and southern", "africa western and central",
    "central europe and the baltics", "middle east, north africa, afghanistan & pakistan",
    # Historical entities: their rows are pre-dissolution series and must not be read as
    # the successor state (countrycode maps "USSR" to RUS).
    "ussr", "u.s.s.r.", "soviet union", "former soviet union", "yugoslavia",
    "former yugoslavia", "sfr yugoslavia", "czechoslovakia", "serbia and montenegro",
    "netherlands antilles", "east germany", "west germany", "german democratic republic",
    # Entities without an ISO3 code of their own; countrycode folds Northern Cyprus into
    # Cyprus, whose WDI series exclude it.
    "northern cyprus", "north cyprus", "turkish republic of northern cyprus", "somaliland",
    "abkhazia", "south ossetia", "transnistria"
  )
}

# Shapes of aggregate and residual labels: "Other Asia Pacific", "Total World",
# "Rest of World", "Non-OECD", "Areas, nes", "Sub-Saharan Africa excluding South Africa",
# "Asia Pacific (ex China)", WDI income and lending groups, currency and customs unions, and
# pre-split series such as "Sudan (former)". Region-plus-country labels such as "China
# Region" are caught too, because countrycode would otherwise read them as the country.
country_aggregate_pattern <- function() {
  paste(
    c(
      "^(other|rest of|total|of which|non-|euro area)",
      "\\bworld\\b",
      "\\b(oecd|opec|hipc|ida|ibrd|ifc)\\b",
      # "ex China" but not "(ex-Zaire)", which some sources append to a country's own name.
      "\\bexcl(\\.|uding)?\\b|\\bexcept\\b|\\bex\\.?\\s",
      "\\(former\\)",
      "\\bcurrency\\b|\\bmonetary\\b|\\bcustoms\\b|\\beconomic community\\b",
      "(,|\\s)n\\.?e\\.?s\\.?$",
      "\\bincome\\b|\\bcountries\\b|\\beconomies\\b|\\bsmall states\\b|\\bdividend\\b",
      "\\bdeveloping\\b",
      "\\bsituations\\b|\\bclassification\\b|\\bmembers\\b",
      "(?<!administrative) region\\b"
    ),
    collapse = "|"
  )
}

# TRUE for rows that are not a single present-day economy: world and bloc totals, regions,
# residual "Other ..." rows, historical entities and the few territories without an ISO3
# code of their own (Northern Cyprus, Somaliland). `iso3c`, when given, also flags WDI
# aggregate codes. Canonical country names are never flagged, whatever words they contain.
# Names that are none of these but match no country are not flagged; they simply get no code.
is_aggregate_or_residual <- function(country, iso3c = NULL) {
  if (length(country) == 0) {
    return(logical())
  }

  raw <- stringr::str_squish(as.character(country))

  # The regex runs once per distinct name; theme and dyad tables repeat names heavily.
  keys <- unique(raw)
  key_lower <- tolower(keys)
  key_flagged <- key_lower %in% country_aggregate_names() |
    stringr::str_detect(key_lower, stringr::regex(country_aggregate_pattern()))
  key_flagged[is.na(keys) | !nzchar(keys)] <- FALSE
  flagged <- key_flagged[match(raw, keys)]

  if (!is.null(iso3c)) {
    flagged <- flagged | (normalize_iso3c(iso3c) %in% country_aggregate_iso3())
  }

  flagged & !(raw %in% country_canonical_names())
}

# Step 1 of standardize_country_names(): whitespace, footnote markers and the curated
# recodes, without any ISO3 matching.
country_clean_names <- function(country) {
  cleaned <- stringr::str_trim(as.character(country))
  cleaned <- stringr::str_replace_all(cleaned, "\\s+", " ")

  # Statistical-source tables mark footnotes by appending a digit to the country name
  # ("Brazil1", "India2" on the EI graphite sheet), which silently drops that country from
  # every name-based join. No entry in the EI or WDI country rosters legitimately ends in a
  # letter followed by a digit, so removing the marker can only turn a failed match into a
  # match. Anchored on a letter so numbers elsewhere in a cell are left alone.
  cleaned <- stringr::str_replace(cleaned, "([[:alpha:]])[0-9]+$", "\\1")

  dplyr::recode(cleaned, !!!country_recode_map(), .default = cleaned)
}

# Clean, recode and resolve a vector of names in one pass over its unique values. Returns a
# list with the canonical name and ISO3 code (NA for aggregates and unmatched names) for
# every element.
country_resolve <- function(country) {
  # Clean the distinct inputs only and map back at the end; callers pass whole columns.
  input <- as.character(country)
  distinct_input <- unique(input)
  cleaned <- country_clean_names(distinct_input)

  keys <- unique(cleaned[!is.na(cleaned) & nzchar(cleaned)])
  canonical <- country_canonical_names()

  iso <- names(canonical)[match(keys, canonical)]
  lookup <- is.na(iso) & !is_aggregate_or_residual(keys)
  if (any(lookup)) {
    iso[lookup] <- suppressWarnings(countrycode::countrycode(
      keys[lookup],
      origin = "country.name",
      destination = "iso3c",
      custom_match = country_iso3_custom_match(),
      warn = FALSE
    ))
  }

  resolved <- country_name_from_iso3(iso)
  resolved <- dplyr::coalesce(resolved, keys)

  idx <- match(cleaned, keys)
  pos <- match(input, distinct_input)
  list(
    name = dplyr::if_else(is.na(idx), cleaned, resolved[idx])[pos],
    iso3c = iso[idx][pos]
  )
}

standardize_country_names <- function(country) {
  if (is.null(country)) {
    return(country)
  }

  country_resolve(country)$name
}

# ISO3 code for each name, NA for aggregates, residual rows and names nothing matches.
country_name_to_iso3 <- function(country) {
  if (is.null(country) || length(country) == 0) {
    return(character())
  }

  country_resolve(country)$iso3c
}

# Canonical name for each ISO3 code: country_canonical_names(), then countrycode's English
# name for codes outside the WDI roster (territories such as Anguilla or Pitcairn).
country_name_from_iso3 <- function(iso3c) {
  if (length(iso3c) == 0) {
    return(character())
  }

  iso <- normalize_iso3c(iso3c)
  canonical <- country_canonical_names()
  out <- unname(canonical[iso])

  fallback <- is.na(out) & !is.na(iso)
  if (any(fallback)) {
    out[fallback] <- suppressWarnings(countrycode::countrycode(
      iso[fallback],
      origin = "iso3c",
      destination = "country.name.en",
      warn = FALSE
    ))
  }

  out
}

normalize_iso3c <- function(x) {
  cleaned <- toupper(stringr::str_trim(as.character(x)))
  cleaned[cleaned %in% c("", "NA", "N/A", "NULL", "<NA>")] <- NA_character_
  cleaned
}

standardize_country_info <- function(country_info) {
  if (is.null(country_info)) {
    return(country_info)
  }
  if (!"country" %in% names(country_info)) {
    stop("country_info must include a 'country' column.")
  }

  standardized <- country_info %>%
    dplyr::mutate(
      country = standardize_country_names(country),
      iso3c = normalize_iso3c(iso3c)
    ) %>%
    dplyr::filter(!is.na(iso3c), nzchar(iso3c))

  if ("region" %in% names(standardized)) {
    standardized <- standardized %>% dplyr::filter(region != "Aggregates")
  }

  standardized %>%
    dplyr::distinct(iso3c, .keep_all = TRUE)
}

# country_info plus the economies the indices score that WDI does not list (Taiwan), each
# under its canonical name and with the WDI attributes NA. Builders that inner-join their rows
# to country_info (the trade themes, trade_core_build_indices()) dropped Taiwan otherwise.
# Codes already in country_info are left as they are.
add_unlisted_economies <- function(country_info, iso3 = "TWN") {
  if (is.null(country_info)) {
    return(country_info)
  }
  require_cols <- c("country", "iso3c")
  if (!all(require_cols %in% names(country_info))) {
    stop("country_info must include columns: ", paste(require_cols, collapse = ", "), ".")
  }
  iso3 <- normalize_iso3c(iso3)
  missing <- setdiff(iso3[!is.na(iso3)], country_info$iso3c)
  if (length(missing) == 0) {
    return(country_info)
  }
  canonical <- country_canonical_names()
  unnamed <- setdiff(missing, names(canonical))
  if (length(unnamed) > 0) {
    stop("No canonical name for: ", paste(unnamed, collapse = ", "), ".")
  }
  dplyr::bind_rows(
    country_info,
    tibble::tibble(country = unname(canonical[missing]), iso3c = missing)
  )
}

# Put a table's Country column on the canonical names and fill iso3c.
#
# Rows that carry an iso3c take the country_info name for it (or the canonical name when the
# code is outside country_info, e.g. TWN); rows without one are matched by name. Either way a
# row that ends up with an ISO3 code ends up with that code's one canonical name, so two
# spellings of an economy can no longer survive side by side. A row whose name is an
# aggregate or historical entity loses any code it carried ("USSR" tagged RUS upstream is
# still not Russia). Rows left without a code (aggregates, residual rows, unmatched names)
# are dropped unless that would drop everything.
standardize_country_table <- function(tbl, country_info = NULL) {
  if (is.null(tbl) || !"Country" %in% names(tbl)) {
    return(tbl)
  }

  # Builders hand over grouped tibbles. Resolving names group by group would call
  # countrycode once per group, so work ungrouped and restore the groups on the way out.
  groups <- dplyr::group_vars(tbl)
  regroup <- function(x) {
    if (length(groups) == 0) x else dplyr::group_by(x, dplyr::across(dplyr::any_of(groups)))
  }

  standardized <- tbl %>%
    dplyr::ungroup() %>%
    dplyr::select(-dplyr::any_of(c(
      "country",
      "country_std",
      "country_std.x",
      "country_std.y",
      "iso3c.x",
      "iso3c.y"
    ))) %>%
    dplyr::mutate(
      Country = standardize_country_names(Country),
      iso3c = if ("iso3c" %in% names(tbl)) normalize_iso3c(iso3c) else NA_character_,
      iso3c = dplyr::if_else(is_aggregate_or_residual(Country), NA_character_, iso3c)
    )

  if (is.null(country_info)) {
    return(regroup(standardized))
  }

  country_ref <- country_info %>%
    dplyr::transmute(
      ref_iso3c = normalize_iso3c(iso3c),
      ref_country = as.character(country),
      ref_country_std = standardize_country_names(country)
    ) %>%
    dplyr::distinct(ref_iso3c, .keep_all = TRUE)

  with_iso <- standardized %>%
    dplyr::filter(!is.na(iso3c), nzchar(iso3c)) %>%
    dplyr::left_join(country_ref, by = c("iso3c" = "ref_iso3c"))

  without_iso <- standardized %>%
    dplyr::filter(is.na(iso3c) | !nzchar(iso3c)) %>%
    dplyr::left_join(country_ref, by = c("Country" = "ref_country_std")) %>%
    dplyr::mutate(iso3c = dplyr::coalesce(iso3c, ref_iso3c))

  standardized_with_ref <- dplyr::bind_rows(with_iso, without_iso) %>%
    dplyr::mutate(
      iso3c = normalize_iso3c(iso3c),
      # Name-based fill for what country_info does not list (Taiwan, territories).
      # country_name_to_iso3() returns NA for aggregates and historical entities, where a
      # raw countrycode call would have read "USSR" as Russia.
      iso3c = dplyr::coalesce(iso3c, country_name_to_iso3(Country)),
      Country = dplyr::coalesce(ref_country, country_name_from_iso3(iso3c), Country)
    ) %>%
    dplyr::select(-dplyr::any_of(c("ref_iso3c", "ref_country", "ref_country_std")))

  standardized_filtered <- standardized_with_ref %>%
    dplyr::filter(!is.na(iso3c), nzchar(iso3c))

  if (nrow(standardized_filtered) == 0 && nrow(standardized_with_ref) > 0) {
    warning(
      "Country standardization dropped all rows in standardize_country_table(); ",
      "returning rows without iso3c filtering."
    )
    return(regroup(standardized_with_ref))
  }

  regroup(standardized_filtered)
}
