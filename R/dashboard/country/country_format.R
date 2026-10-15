# Display strings of Country Insights (spec_country.md A8.3): the single formatter. Every number a
# fact sheet shows or a memo cites is an Fv list(v = <number or NA>, d = <display string>); d is
# computed here once, so the screen, the PDF and the Word file print the same text. Pure.
#
#   index         2 decimals                                  "0.35"
#   signed_index  sign + 2 decimals                           "+0.12", "-0.12"
#   pct           integer percent of economies beaten         "31%" (1-99 unless all or none)
#   share         1 decimal percent, "<0.1%" for tiny values  "37.8%"
#   rank          ordinal with digits                         "1st", "24th"
#   order_low     position in a lowest-first list             "lowest", "3rd lowest"
#   order_high    position in a highest-first list            "highest", "2nd highest"
#   count         thousands separator                         "1,042"
#   usd           the Sector rule                             "USD 6.1 bn", "USD 840 m"
#   year          the year as written                         "2024"
#   date          "12 October 2026"
#   raw           a value in a Sector unit (unit_kind, unit text, display scale): 3 significant
#                 figures, grouped thousands, ASCII hyphen-minus for negatives ("-6.49 EJ")
#
# NA prints as an en dash in d (callers decide whether a missing value is shown).

COUNTRY_DASH <- "–"

country_ordinal <- function(n) {
  n <- as.integer(round(n))
  suf <- ifelse(n %% 100L %in% 11:13, "th", c("th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th")[n %% 10L + 1L])
  paste0(n, suf)
}

# Round to `sig` significant figures.
country_round_sig <- function(x, sig = 3) {
  out <- x
  ok <- is.finite(x) & x != 0
  d <- ceiling(log10(abs(x[ok])))
  out[ok] <- round(x[ok] * 10^(sig - d)) / 10^(sig - d)
  out
}

# Grouped thousands with at most `max_frac` decimals (trailing zeros dropped), ASCII minus.
country_grouped <- function(x, max_frac = 0) {
  vapply(x, function(v) {
    if (!is.finite(v)) return(COUNTRY_DASH)
    s <- formatC(v, format = "f", digits = max_frac, big.mark = ",")
    if (max_frac > 0) s <- sub("\\.?0+$", "", s)
    if (s %in% c("-0", "-0.0")) s <- "0"
    s
  }, character(1), USE.NAMES = FALSE)
}

# 3 significant figures with grouping: 21034 -> "21,000", 1.2345 -> "1.23", 0.01234 -> "0.0123".
country_sig <- function(x, sig = 3) {
  vapply(x, function(v) {
    if (!is.finite(v)) return(COUNTRY_DASH)
    if (v == 0) return("0")
    r <- country_round_sig(v, sig)
    d <- ceiling(log10(abs(r)))
    frac <- max(0, sig - d)
    country_grouped(r, min(frac, 10))
  }, character(1), USE.NAMES = FALSE)
}

country_usd <- function(x) {
  vapply(x, function(v) {
    if (!is.finite(v)) return(COUNTRY_DASH)
    a <- abs(v)
    steps <- c(tn = 1e12, bn = 1e9, m = 1e6, k = 1e3)
    hit <- which(a >= steps)
    if (length(hit)) {
      s <- steps[hit[1]]
      paste0("USD ", country_sig(v / s, 3), " ", names(s))
    } else paste0("USD ", country_sig(v, 3))
  }, character(1), USE.NAMES = FALSE)
}

country_share_text <- function(x, digits = 1) {
  vapply(x, function(v) {
    if (!is.finite(v)) return(COUNTRY_DASH)
    lim <- 0.5 * 10^(-digits) / 100
    if (v > 0 && v < lim) return(paste0("<", formatC(10^(-digits), format = "f", digits = digits), "%"))
    s <- formatC(v * 100, format = "f", digits = digits)
    if (grepl("^-0\\.?0*$", s)) s <- formatC(0, format = "f", digits = digits)
    paste0(s, "%")
  }, character(1), USE.NAMES = FALSE)
}

country_date_label <- function(iso_date) {
  d <- as.Date(iso_date)
  months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  ifelse(is.na(d), COUNTRY_DASH, paste(as.integer(format(d, "%d")), months[as.integer(format(d, "%m"))], format(d, "%Y")))
}

# A raw value in a Sector unit: unit_kind decides the number format, `unit` is the text after it.
country_raw_text <- function(x, unit_kind, unit = "", scale = 1) {
  v <- x * scale
  unit <- if (is.null(unit) || is.na(unit)) "" else unit
  suffix <- if (nzchar(unit) && !grepl("^\\d+\\s*[-–]\\s*\\d+$", unit)) paste0(" ", unit) else ""
  if (!is.finite(v)) return(COUNTRY_DASH)
  switch(
    unit_kind,
    usd = country_usd(v),
    usd_m = country_usd(v * 1e6),
    usd_bn_real = paste0(country_usd(v * 1e9), " (2024$)"),
    share = paste0(country_share_text(v), if (nzchar(unit) && !unit %in% c("share")) paste0(" ", unit) else ""),
    pct = paste0(formatC(v, format = "f", digits = 1), "%", if (nzchar(unit) && !unit %in% c("%")) sub("^%", "", paste0(" ", sub("^% ?", "", unit))) else ""),
    hhi = formatC(v, format = "f", digits = 3),
    index01 = formatC(v, format = "f", digits = 2),
    count = country_grouped(round(v), 0),
    ratio = paste0(formatC(v, format = "f", digits = 2), if (nzchar(unit) && !unit %in% c("ratio", "RCA")) suffix else ""),
    trl = paste0("TRL ", formatC(v, format = "f", digits = 1)),
    gj_pc = paste0(country_sig(v, 3), " GJ per person"),
    paste0(country_sig(v, 3), suffix)
  )
}

# Display strings for a vector of values of one kind.
country_fmt <- function(x, kind, ...) {
  x <- as.numeric(x)
  out <- switch(
    kind,
    index = ifelse(is.finite(x), formatC(x, format = "f", digits = 2), COUNTRY_DASH),
    index3 = ifelse(is.finite(x), formatC(x, format = "f", digits = 3), COUNTRY_DASH),
    signed_index = ifelse(is.finite(x), paste0(ifelse(round(x, 2) > 0, "+", ""), formatC(x, format = "f", digits = 2)), COUNTRY_DASH),
    # half away from zero ("62.5" -> "63%", as a reader and JavaScript's Math.round expect; R's round() is half-even);
    # "100%" and "0%" only when literally every or no other economy is beaten (99.6 -> "99%", 0.4 -> "1%")
    pct = ifelse(is.finite(x), paste0(formatC(pmin(ifelse(x < 100, 99, 100), pmax(ifelse(x > 0, 1, 0), floor(round(x, 6) + 0.5))), format = "d"), "%"), COUNTRY_DASH),
    share = country_share_text(x),
    rank = ifelse(is.finite(x), country_ordinal(ifelse(is.finite(x), x, 0)), COUNTRY_DASH),
    order_low = ifelse(is.finite(x), ifelse(x == 1, "lowest", paste(country_ordinal(ifelse(is.finite(x), x, 0)), "lowest")), COUNTRY_DASH),
    order_high = ifelse(is.finite(x), ifelse(x == 1, "highest", paste(country_ordinal(ifelse(is.finite(x), x, 0)), "highest")), COUNTRY_DASH),
    count = country_grouped(round(x), 0),
    usd = country_usd(x),
    year = ifelse(is.finite(x), formatC(x, format = "d"), COUNTRY_DASH),
    number = country_sig(x, 3),
    stop("country_fmt(): unknown kind ", kind)
  )
  out <- gsub("−", "-", out, fixed = TRUE)
  # "-0.00" and "+0.00" read as zero
  out[out %in% c("-0.00", "+0.00", "-0.000")] <- sub("^[-+]", "", out[out %in% c("-0.00", "+0.00", "-0.000")])
  unname(out)
}

# One Fv: list(v, d). NA / NULL / non-finite -> v NULL (JSON null) with d = `missing` (default: dash).
country_fv <- function(x, kind, missing = COUNTRY_DASH, digits = NULL) {
  if (is.null(x) || length(x) == 0 || is.na(x[1]) || !is.finite(as.numeric(x[1]))) return(list(v = NULL, d = missing))
  v <- as.numeric(x[1])
  if (!is.null(digits)) v <- round(v, digits)
  list(v = v, d = country_fmt(v, kind))
}

# An Fv with an explicit display string.
country_fv_d <- function(v, d) {
  if (is.null(v) || length(v) == 0 || is.na(v[1])) return(list(v = NULL, d = d))
  list(v = as.numeric(v[1]), d = d)
}

# A raw Fv with unit metadata (A8.2 RawValue).
country_fv_raw <- function(x, unit_kind, unit = "", scale = 1, year = NA, source = NA, source_label = NA, note = NA, note_d = NA) {
  ok <- !(is.null(x) || length(x) == 0 || is.na(x[1]) || !is.finite(as.numeric(x[1])))
  na_null <- function(s) if (is.null(s) || length(s) == 0 || is.na(s)) NULL else as.character(s)
  out <- list(
    v = if (ok) as.numeric(x[1]) else NULL,
    d = if (ok) country_raw_text(as.numeric(x[1]), unit_kind, unit, scale) else COUNTRY_DASH,
    unit = na_null(unit),
    year = na_null(year),
    source = na_null(source),
    source_label = na_null(source_label),
    note = na_null(note)
  )
  # the note in plain words (additive 2026-10-14); omitted when there is none (sheet size)
  if (!is.null(na_null(note_d))) out$note_d <- as.character(note_d)
  out
}
