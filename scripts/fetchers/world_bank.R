# World Bank fetchers: wb_wdi.csv and wb_doingbusiness.csv.
#
# Both files are DataBank "wide" exports. The consumers
# (R/themes/partnership_strength/stronger_development.R) key off:
#   Country Name, Country Code, Series Name, Series Code, and one column per year
#   named "2023 [YR2023]".
# The v2 API returns long JSON, so we pivot back into that layout rather than change
# any downstream parsing.

WB_API_BASE <- "https://api.worldbank.org/v2"

# The /country endpoint is the authority on what counts as an economy. The indicator
# endpoint also returns regional aggregates and, for Doing Business, sub-national city
# entries (Beijing, Delhi, Lagos) that reuse their parent ISO3 code. DataBank exports
# exclude both, and leaving them in produces duplicate country keys that corrupt the
# downstream one-to-one joins in stronger_development.R.
wb_economies_cache <- new.env(parent = emptyenv())

wb_economies <- function() {
  if (exists("ids", envir = wb_economies_cache, inherits = FALSE)) {
    return(get("ids", envir = wb_economies_cache, inherits = FALSE))
  }

  payload <- opsi_http_get_json(
    paste0(WB_API_BASE, "/country?format=json&per_page=400")
  )
  rows <- if (length(payload) >= 2) payload[[2]] else list()

  keep <- vapply(rows, function(r) {
    region_id <- tryCatch(r$region$id, error = function(e) NULL)
    # Aggregates carry region id "NA"; real economies carry a region.
    !is.null(region_id) && !identical(as.character(region_id), "NA")
  }, logical(1))

  # Return official name keyed by ISO3. ISO3 is the stable filter (it survives the
  # Turkey/Turkiye and Czech Republic/Czechia renames); the name is used only to pick
  # the national row when a sub-national entry shares the same ISO3.
  iso3 <- vapply(rows[keep], function(r) as.character(r$id), character(1))
  official <- vapply(rows[keep], function(r) as.character(r$name), character(1))

  valid <- !is.na(iso3) & nzchar(iso3)
  lookup <- stats::setNames(official[valid], iso3[valid])

  assign("ids", lookup, envir = wb_economies_cache)
  lookup
}

# One indicator, all countries, all years -> long data frame.
wb_fetch_indicator <- function(indicator, start_year = NULL, end_year = NULL, per_page = 20000) {
  date_param <- if (!is.null(start_year) && !is.null(end_year)) {
    paste0("&date=", start_year, ":", end_year)
  } else {
    ""
  }

  collected <- list()
  page <- 1L
  repeat {
    url <- sprintf(
      "%s/country/all/indicator/%s?format=json&per_page=%d&page=%d%s",
      WB_API_BASE, utils::URLencode(indicator, reserved = TRUE), per_page, page, date_param
    )
    payload <- opsi_http_get_json(url)

    if (length(payload) < 2 || is.null(payload[[2]])) break

    header <- payload[[1]]
    rows <- payload[[2]]
    if (length(rows) == 0) break

    collected[[length(collected) + 1L]] <- rows

    total_pages <- if (!is.null(header$pages)) as.integer(header$pages) else 1L
    if (page >= total_pages) break
    page <- page + 1L
  }

  rows <- unlist(collected, recursive = FALSE)
  if (length(rows) == 0) {
    return(data.frame(
      country_id = character(), country_name = character(), country_code = character(),
      series_name = character(), series_code = character(),
      year = integer(), value = numeric(), stringsAsFactors = FALSE
    ))
  }

  pluck <- function(row, path, default = NA_character_) {
    node <- row
    for (key in path) {
      node <- node[[key]]
      if (is.null(node)) return(default)
    }
    if (length(node) == 0) default else node
  }

  data.frame(
    country_id = vapply(rows, function(r) as.character(pluck(r, c("country", "id"))), character(1)),
    country_name = vapply(rows, function(r) as.character(pluck(r, c("country", "value"))), character(1)),
    country_code = vapply(rows, function(r) as.character(pluck(r, "countryiso3code")), character(1)),
    series_name = vapply(rows, function(r) as.character(pluck(r, c("indicator", "value"))), character(1)),
    series_code = vapply(rows, function(r) as.character(pluck(r, c("indicator", "id"))), character(1)),
    year = vapply(rows, function(r) suppressWarnings(as.integer(pluck(r, "date", NA))), integer(1)),
    value = vapply(rows, function(r) {
      v <- pluck(r, "value", NA)
      if (is.null(v) || length(v) == 0) NA_real_ else suppressWarnings(as.numeric(v))
    }, numeric(1)),
    stringsAsFactors = FALSE
  )
}

# Long rows -> DataBank wide layout, including the "YYYY [YRYYYY]" column naming and
# the ".." placeholder DataBank uses for missing observations.
wb_to_databank_wide <- function(long_df, economies = wb_economies()) {
  long_df <- long_df[!is.na(long_df$country_code) & nzchar(long_df$country_code), , drop = FALSE]
  long_df <- long_df[!is.na(long_df$year), , drop = FALSE]

  # Drop aggregates, then collapse any sub-national rows sharing an ISO3 down to the
  # national one, so that one row means one economy.
  if (length(economies) > 0) {
    long_df <- long_df[long_df$country_code %in% names(economies), , drop = FALSE]

    if (nrow(long_df) > 0) {
      official <- unname(economies[long_df$country_code])
      is_official <- !is.na(official) & long_df$country_name == official

      key <- paste(long_df$country_code, long_df$series_code, long_df$year, sep = "\r")
      # Prefer the row whose name matches the official economy name; when naming has
      # drifted and none match, fall back to the first row so no economy is lost.
      ord <- order(key, !is_official)
      long_df <- long_df[ord, , drop = FALSE]
      long_df <- long_df[!duplicated(key[ord]), , drop = FALSE]

      # Report the canonical name so the export is stable across upstream renames.
      long_df$country_name <- ifelse(
        is.na(unname(economies[long_df$country_code])),
        long_df$country_name,
        unname(economies[long_df$country_code])
      )
    }
  }

  if (nrow(long_df) == 0) {
    return(data.frame(
      `Country Name` = character(), `Country Code` = character(),
      `Series Name` = character(), `Series Code` = character(),
      check.names = FALSE, stringsAsFactors = FALSE
    ))
  }

  long_df$period <- sprintf("%d [YR%d]", long_df$year, long_df$year)

  wide <- opsi_pivot_periods_wide(
    long_df,
    id_cols = c("country_name", "country_code", "series_name", "series_code"),
    period_col = "period",
    value_col = "value",
    period_order = sprintf("%d [YR%d]", sort(unique(long_df$year)), sort(unique(long_df$year)))
  )

  names(wide)[1:4] <- c("Country Name", "Country Code", "Series Name", "Series Code")

  # DataBank writes ".." rather than an empty cell for missing values, and the
  # consumers coerce with as.numeric(), so either parses to NA. Match the export.
  period_cols <- setdiff(names(wide), c("Country Name", "Country Code", "Series Name", "Series Code"))
  for (cn in period_cols) {
    v <- wide[[cn]]
    wide[[cn]] <- ifelse(is.na(v), "..", format(v, trim = TRUE, scientific = FALSE))
  }

  wide[order(wide$`Country Name`, wide$`Series Code`), , drop = FALSE]
}

wb_fetch_series_set <- function(indicators, start_year, end_year) {
  frames <- lapply(indicators, function(ind) {
    wb_fetch_indicator(ind, start_year = start_year, end_year = end_year)
  })
  frames <- Filter(function(d) nrow(d) > 0, frames)
  if (length(frames) == 0) stop("no observations returned for any requested indicator")
  wb_to_databank_wide(do.call(rbind, frames))
}

## wb_wdi.csv ---------------------------------------------------------------

# The exact 44 Series Codes present in the staged DataBank extract. Keep this list in
# sync with what stronger_development.R scores; it names the series explicitly rather
# than pulling a whole DataBank source so the output stays stable and reviewable.
WB_WDI_INDICATORS <- c(
  # Governance (Worldwide Governance Indicators percentile ranks)
  "CC.PER.RNK", "GE.PER.RNK", "PV.PER.RNK", "RL.PER.RNK",
  # Debt and aid
  "DC.DAC.USAL.CD", "DT.DOD.DECT.GN.ZS", "DT.ODA.ODAT.PC.ZS",
  "DT.TDS.DECT.GN.ZS", "DT.TDS.DPPF.XP.ZS",
  # Energy
  "EG.ELC.ACCS.ZS", "EG.IMP.CONS.ZS", "EG.USE.PCAP.KG.OE", "IC.ELC.OUTG.ZS",
  # Financial sector
  "FB.BNK.CAPA.ZS", "FD.AST.PRVT.GD.ZS", "FD.RES.LIQU.AS.ZS", "FM.AST.NFRG.CN",
  "FP.CPI.TOTL", "FR.INR.DPST", "FS.AST.PRVT.GD.ZS",
  # Fiscal
  "GC.DOD.TOTL.GD.ZS", "GC.TAX.TOTL.GD.ZS",
  # Human capital
  "HD.HCI.OVRL",
  # Business Ready (B-READY) topic scores
  "IC.BRE.BE.OS", "IC.BRE.BI.OS", "IC.BRE.BL.OS", "IC.BRE.DR.OS", "IC.BRE.FS.OS",
  "IC.BRE.IT.OS", "IC.BRE.LB.OS", "IC.BRE.MC.OS", "IC.BRE.TX.OS", "IC.BRE.US.OS",
  # Macro and trade
  "BN.GSR.MRCH.CD", "BX.KLT.DINV.WD.GD.ZS", "NE.GDI.TOTL.ZS", "NE.RSB.GNFS.ZS",
  "NE.TRD.GNFS.ZS", "NV.IND.TOTL.KD.ZG", "NY.GDP.MKTP.KD.ZG", "NY.GDP.PCAP.KD",
  "TM.TAX.MANF.WM.AR.ZS", "TM.TAX.MRCH.WM.AR.ZS", "TX.VAL.TECH.MF.ZS"
)

register_fetcher(
  id = "wb_wdi",
  description = "World Bank WDI development indicator panel (DataBank wide layout)",
  contract = list(
    required_columns = c("Country Name", "Country Code", "Series Name", "Series Code"),
    min_rows = 500,
    unique_key = c("Country Code", "Series Code"),
    time_column_pattern = "^X?\\d{4}\\.*\\s*\\[?YR",
    min_time_columns = 2
  ),
  fn = function() {
    end_year <- as.integer(format(Sys.Date(), "%Y")) - 1L
    wb_fetch_series_set(WB_WDI_INDICATORS, start_year = end_year - 2L, end_year = end_year)
  }
)

## wb_doingbusiness.csv -----------------------------------------------------

# Doing Business was discontinued after 2020, so these series are static; the fetcher
# exists to make the file reproducible, not to track new releases.
# stronger_development.R selects IC.BUS.EASE.DFRN.XQ.DB1719.
WB_DOINGBUSINESS_INDICATORS <- c(
  "IC.BUS.EASE.DFRN.DB1014",
  "IC.BUS.EASE.DFRN.DB16",
  "IC.BUS.EASE.DFRN.XQ.DB1719",
  "IC.BUS.EASE.XQ"
)

register_fetcher(
  id = "wb_doingbusiness",
  description = "World Bank Doing Business indicators (archived series, DataBank wide layout)",
  contract = list(
    required_columns = c("Country Name", "Country Code", "Series Name", "Series Code"),
    min_rows = 100,
    unique_key = c("Country Code", "Series Code"),
    time_column_pattern = "^X?\\d{4}\\.*\\s*\\[?YR",
    min_time_columns = 1
  ),
  fn = function() {
    wb_fetch_series_set(WB_DOINGBUSINESS_INDICATORS, start_year = 2019L, end_year = 2019L)
  }
)
