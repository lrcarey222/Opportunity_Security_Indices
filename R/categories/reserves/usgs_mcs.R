# USGS Mineral Commodity Summaries (MCS): country reserves of the critical minerals, for the
# Reserves theme (reserves() in reserves.R) and the reserves leg of the coalition supplier
# route (coalition_shares_usgs_reserves() in R/indices/coalition.R).
#
# User decision (2026-10-09): the nine critical-mineral reserves come from USGS MCS 2026
# (reserves at end-2025); oil, gas and coal stay on the EI Statistical Review (proved reserves
# at end-2020) and uranium on the OECD-NEA/IAEA Red Book (uranium.R). EI's mineral reserve
# sheets are copies of older USGS data - the 2026 workbook carries Brazil's rare earths at
# 21 Mt, the MCS 2025 figure that MCS 2026 revised to 11 Mt - and list only the leading
# holders, folding the rest into "Rest of World" (Brazil's manganese, 17% of world reserves,
# and nickel, 11%, were not on EI's sheets).
#
# Input: the commodity table of an MCS data release (2026: ScienceBase "Mineral Commodity
# Summaries 2026 Data Release - Commodity Salient U.S. and World Statistics",
# MCS2026_Commodities_Data.csv, staged as data/raw/MCS2026_World_Data.csv), read by the
# scripts with every column as character, "NA" kept as text and the file's Windows-1252
# encoding. It is one long table for every chapter: Commodity, Country, Statistics
# ("Reserves"), Statistics_detail, Unit, Year, Value (as printed: "1,900,000", "NA", "W",
# ">76,000,000"), Notes.
#
# Pure functions; they use R/utils/country.R (country_name_to_iso3, standardize_country_names,
# is_aggregate_or_residual).

# The nine minerals of the Reserves theme and the USGS rows that hold their reserves.
#
# `scale` converts the USGS unit to `unit_desc`, the unit of the EI sheets the theme read
# until the switch (thousand tonnes; kilograms for PGMs), so raw values stay comparable across
# it. The index and the coalition use ranks and shares within a mineral, which no unit changes;
# what has to hold is one basis per mineral across countries, and each USGS column has one:
# `basis` records it. Manganese is the chapter's default, gross weight (the table gives mine
# production in manganese content but the reserves column no basis of its own), as EI's copy was.
# `unit` is the USGS unit the rows must carry: a new edition that restates a mineral in other
# units stops here rather than being scaled wrongly.
reserves_usgs_specs <- function() {
  spec <- function(tech_name, commodity, unit, scale, unit_desc, basis, detail = "Reserves") {
    list(
      tech_name = tech_name, commodity = commodity, detail = detail, unit = unit,
      scale = scale, unit_desc = unit_desc, basis = basis
    )
  }
  list(
    spec("Cobalt", "Cobalt", "metric tons", 1e-3, "Thousand tonnes", "cobalt content"),
    spec("Lithium", "Lithium", "metric tons", 1e-3, "Thousand tonnes", "lithium content"),
    spec("Graphite", "Graphite (Natural)", "metric tons", 1e-3, "Thousand tonnes", "natural graphite"),
    spec("Rare Earths", "Rare Earths", "metric tons", 1e-3, "Thousand tonnes", "rare-earth-oxide equivalent"),
    spec("Copper", "Copper", "thousand metric tons", 1, "Thousand tonnes", "copper content"),
    spec("Manganese", "Manganese", "thousand metric tons", 1, "Thousand tonnes", "gross weight"),
    spec("Nickel", "Nickel", "metric tons", 1e-3, "Thousand tonnes", "nickel content"),
    spec("Zinc", "Zinc", "thousand metric tons", 1, "Thousand tonnes", "zinc content"),
    # Platinum-group metals combined (platinum, palladium, rhodium, ...): USGS reports PGM
    # reserves only as one total, which is also what the IEA demand sheet maps ("PGMs").
    spec("PGMs", "Platinum-Group Metals", "kilograms", 1, "Kilograms", "platinum-group-metal content",
         detail = "PGM reserves")
  )
}

# Errata to an edition's data release: corrections USGS published in the report after the
# release's CSV was uploaded. MCS 2026 version 1.3 (2026-05-27; pubs.usgs.gov/publication/mcs2026,
# version history) revised Brazil's rare-earth reserves "from 21,000,000 to 11,000,000" and the
# world total "from >85,000,000 to >75,000,000" ("Reserves for Australia, Brazil, and Malaysia
# were revised based on Government reports"); the ScienceBase CSV (uploaded 2026-02-06) still
# carries the version 1.0 values. `published` is the value the release prints, `corrected` the
# report's; usgs_mcs_apply_errata() stops on any third value, so a re-uploaded release is noticed.
usgs_mcs_errata <- function() {
  ref <- "USGS, Mineral Commodity Summaries 2026, ver. 1.3 (2026-05-27), p. 153, version history"
  tibble::tibble(
    edition = 2026L,
    commodity = "Rare Earths",
    country = c("Brazil", "World total"),
    statistics = "Reserves",
    published = c("21,000,000", ">85,000,000"),
    corrected = c("11,000,000", ">75,000,000"),
    reference = ref
  )
}

# Value codes of the USGS tables, read explicitly (MCS "Explanation of symbols" and the
# chapter footnotes):
#   "1,900,000"  reported                    -> the number
#   ">9,100,000" lower bound ("more than")   -> the number, as a floor
#   "<200,000"   upper bound                 -> NA (only that it is below the number)
#   "—"          zero                        -> 0 (an observed zero)
#   "NA"         not available               -> NA
#   "W"          withheld (company data)     -> NA: the holder has reserves USGS does not
#                                               publish; 0 would call it a non-holder
#   "s"          small / in the World total  -> NA. The release's metadata defines "s" as "small";
#                                               the graphite chapter prints these cells (United
#                                               States, Austria, Germany, Ukraine) as footnote
#                                               "Included in World total". Either way a holder
#                                               USGS does not quantify, not a non-holder
#   "Small", "Very small"                    -> NA (positive, unquantified)
#   "Large", "Moderate", "Moderate to large" -> NA (qualitative)
#   "XX"         not applicable              -> NA
# Any other code returns status NA, which usgs_mcs_reserves() stops on: a new symbol has to be
# read before it is scored.
usgs_mcs_parse_value <- function(value, notes = NULL) {
  raw <- stringr::str_squish(as.character(value))
  notes <- if (is.null(notes)) rep(NA_character_, length(raw)) else as.character(notes)
  number <- suppressWarnings(as.numeric(gsub(",", "", sub("^[<>]\\s*", "", raw))))
  lower <- tolower(raw)

  status <- dplyr::case_when(
    is.na(raw) | !nzchar(raw) | raw == "NA" ~ "not available",
    grepl("^[0-9][0-9,]*(\\.[0-9]+)?$", raw) & !is.na(number) ~ "reported",
    grepl("^>", raw) & !is.na(number) ~ "lower bound",
    grepl("^<", raw) & !is.na(number) ~ "upper bound",
    grepl("^[—–-]+$", raw) ~ "zero",
    raw == "W" ~ "withheld",
    raw == "s" | grepl("included in .{0,2}world total", notes, ignore.case = TRUE) ~ "in world total",
    lower %in% c("small", "very small") ~ "small",
    lower %in% c("large", "moderate", "moderate to large") ~ "qualitative",
    raw == "XX" ~ "not applicable",
    TRUE ~ NA_character_
  )

  tibble::tibble(
    value = dplyr::case_when(
      status %in% c("reported", "lower bound") ~ number,
      status == "zero" ~ 0,
      TRUE ~ NA_real_
    ),
    status = status
  )
}

# Apply usgs_mcs_errata() rows of `edition` to the reserves rows (Commodity, Country,
# Statistics, Value). Each erratum must find exactly one row, holding either the published
# value (replaced) or already the corrected one (a fixed re-upload: left alone).
usgs_mcs_apply_errata <- function(rows, errata, edition) {
  errata <- errata[errata$edition == edition, , drop = FALSE]
  for (i in seq_len(nrow(errata))) {
    hit <- which(
      rows$Commodity == errata$commodity[i] &
        rows$Country == errata$country[i] &
        rows$Statistics == errata$statistics[i]
    )
    if (length(hit) == 0) {
      next # the erratum's mineral is not among the requested ones
    }
    label <- paste0(errata$commodity[i], " / ", errata$country[i], " (MCS ", edition, ")")
    if (length(hit) > 1) {
      stop("USGS erratum ", label, " matches ", length(hit), " rows.")
    }
    current <- stringr::str_squish(rows$Value[hit])
    if (identical(current, errata$corrected[i])) {
      next
    }
    if (!identical(current, errata$published[i])) {
      stop(
        "USGS erratum ", label, ": the release has '", current, "', neither the published '",
        errata$published[i], "' nor the corrected '", errata$corrected[i], "'. Check the release."
      )
    }
    rows$Value[hit] <- errata$corrected[i]
    rows$Notes[hit] <- paste0(
      dplyr::coalesce(rows$Notes[hit], ""), if (nzchar(dplyr::coalesce(rows$Notes[hit], ""))) " -- ",
      "Corrected from ", errata$published[i], ": ", errata$reference[i], "."
    )
  }
  rows
}

# Reserves of the minerals in `specs` from an MCS commodity table, one row per mineral and
# USGS row: tech (the theme's mineral name), commodity, label (USGS's country label), Country
# (canonical name, NA for aggregates), ISO3166_alpha3, row_type ("country", "other" for
# "Other countries", "world" for "World total"), value (in `unit_desc`), status (see
# usgs_mcs_parse_value()), code (the printed value), unit, basis, data_year (the reserves'
# year-end), edition (data_year + 1, the MCS edition that publishes it) and notes.
#
# The aggregate rows stay in the table, flagged, because they make the world denominator
# (usgs_mcs_world_reserves()); the theme and the coalition keep only the country rows as holders.
usgs_mcs_reserves <- function(mcs, specs = reserves_usgs_specs(), errata = usgs_mcs_errata()) {
  needed <- c("Commodity", "Country", "Statistics", "Statistics_detail", "Unit", "Year", "Value", "Notes")
  if (!inherits(mcs, "data.frame")) {
    stop("mcs must be a data.frame (the MCS commodity table).")
  }
  missing_cols <- setdiff(needed, names(mcs))
  if (length(missing_cols) > 0) {
    stop("The MCS table lacks column(s): ", paste(missing_cols, collapse = ", "),
         ". It must be the long commodity table of the MCS 2026 data release layout.")
  }

  out <- lapply(specs, function(spec) {
    detail_pattern <- paste0("^", spec$detail, "(: rounded)?$")
    rows <- mcs[
      mcs$Commodity == spec$commodity & mcs$Statistics == "Reserves" &
        grepl(detail_pattern, mcs$Statistics_detail),
      needed,
      drop = FALSE
    ]
    if (nrow(rows) == 0) {
      stop("The MCS table has no '", spec$detail, "' rows for ", spec$commodity, ".")
    }
    units <- unique(rows$Unit)
    if (!identical(units, spec$unit)) {
      stop("MCS ", spec$commodity, " reserves are in '", paste(units, collapse = "', '"),
           "', not '", spec$unit, "'; update reserves_usgs_specs() before scaling them.")
    }
    data_year <- unique(suppressWarnings(as.integer(rows$Year)))
    if (length(data_year) != 1 || is.na(data_year)) {
      stop("MCS ", spec$commodity, " reserves carry more than one year: ", paste(unique(rows$Year), collapse = ", "))
    }
    edition <- data_year + 1L
    rows <- usgs_mcs_apply_errata(rows, errata, edition)

    parsed <- usgs_mcs_parse_value(rows$Value, rows$Notes)
    unknown <- is.na(parsed$status)
    if (any(unknown)) {
      stop("MCS ", spec$commodity, " reserves: unrecognised value code(s) ",
           paste0("'", unique(rows$Value[unknown]), "'", collapse = ", "), " (", paste(rows$Country[unknown], collapse = ", "), ").")
    }

    label <- stringr::str_squish(rows$Country)
    row_type <- dplyr::case_when(
      grepl("^world total", label, ignore.case = TRUE) ~ "world",
      is_aggregate_or_residual(label) ~ "other",
      TRUE ~ "country"
    )
    if (sum(row_type == "world") != 1) {
      stop("MCS ", spec$commodity, " reserves need exactly one 'World total' row; found ", sum(row_type == "world"), ".")
    }
    iso3 <- rep(NA_character_, length(label))
    iso3[row_type == "country"] <- country_name_to_iso3(label[row_type == "country"])
    unmatched <- row_type == "country" & is.na(iso3)
    if (any(unmatched)) {
      stop("MCS ", spec$commodity, " reserves: no ISO3 code for ", paste(label[unmatched], collapse = ", "), ".")
    }
    dupes <- iso3[row_type == "country"]
    if (anyDuplicated(dupes)) {
      stop("MCS ", spec$commodity, " reserves list one economy twice: ", paste(unique(dupes[duplicated(dupes)]), collapse = ", "))
    }

    tibble::tibble(
      tech = spec$tech_name,
      commodity = spec$commodity,
      label = label,
      Country = ifelse(row_type == "country", standardize_country_names(label), NA_character_),
      ISO3166_alpha3 = iso3,
      row_type = row_type,
      value = parsed$value * spec$scale,
      status = parsed$status,
      code = stringr::str_squish(rows$Value),
      unit = spec$unit_desc,
      basis = spec$basis,
      data_year = data_year,
      edition = edition,
      notes = rows$Notes
    )
  })

  out <- dplyr::bind_rows(out)
  if (dplyr::n_distinct(out$edition) != 1) {
    stop("The MCS reserves mix editions: ", paste(sort(unique(out$edition)), collapse = ", "))
  }
  out
}

# World reserves per mineral: the denominator of a country's share. USGS's "World total" is
# rounded (two significant figures) and can be a floor (">75,000,000"), so the listed rows
# (countries plus "Other countries") can add to more than it - rare earths 75.3 Mt against
# ">75 Mt", zinc 240.8 Mt against 240 Mt, and the MCS 2026 release's manganese total of
# 1,700,000 (the report prints 1,800,000) against 1,798,000 listed. The world is the larger of
# the two, so every share is a share of a total at least as large as its parts. Unquantified
# holders ("NA", "W", "s") are inside the World total, not the listed sum.
usgs_mcs_world_reserves <- function(usgs_reserves) {
  out <- dplyr::summarise(
    dplyr::group_by(usgs_reserves, .data$tech),
    world_reported = sum(.data$value[.data$row_type == "world"]),
    world_status = .data$status[.data$row_type == "world"][1],
    listed = sum(.data$value[.data$row_type != "world"], na.rm = TRUE),
    .groups = "drop"
  )
  out$world <- pmax(out$world_reported, out$listed, na.rm = TRUE)
  out
}

# The country rows whose reserves are not quantified, for the scripts' log: they enter the
# theme as missing (the reserves missing-data rule then scores them like an unlisted economy and
# counts them as imputed), never as an observed zero.
usgs_mcs_unquantified <- function(usgs_reserves) {
  usgs_reserves[
    usgs_reserves$row_type == "country" & is.na(usgs_reserves$value),
    c("tech", "Country", "ISO3166_alpha3", "status", "code"),
    drop = FALSE
  ]
}
