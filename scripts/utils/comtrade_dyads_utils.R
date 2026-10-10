# Helpers for the all-reporter UN Comtrade energy-dyad pull (scripts/98_pull_comtrade_energy_dyads.R).
#
# Everything here is pure: the script reads the inputs, calls the API and writes the chunk files.
#
# The pull replaces the 22-reporter allied_comtrade_energy_data.csv as the bilateral trade source
# of the partnership themes. It keeps that file's shape (comtradr's tidy columns, API totals over
# customs procedure, mode of transport and second partner) so the partner builders can read it
# with a path switch, and adds two flags: the classification requested and whether the HS6 code
# is a legacy bridge code from the basket.

# Free-tier record cap of the Comtrade data API. A response of exactly this many rows is
# truncated, so the request is split and repeated.
COMTRADE_DYADS_RECORD_CAP <- 100000L

# The API gateway answers HTTP 414 above a ~2,048-character query string (comtradr only guards
# the 4,096-character URL), and commas are sent percent-encoded. The full 298-code basket alone
# is ~2,700 characters, so the basket is requested in code groups.
COMTRADE_DYADS_MAX_QUERY_CHARS <- 1950L

# Approximate query-string length of a data-API request: the fixed parameters (flow, period,
# customs, transport, second partner, includeDesc) plus the encoded code and reporter lists.
comtrade_dyads_query_chars <- function(reporter_ids, codes) {
  enc <- function(x) if (length(x) == 0) 0L else sum(nchar(as.character(x))) + 3L * (length(x) - 1L)
  110L + enc(codes) + enc(reporter_ids)
}

# Interleaved code groups (every n-th code of the sorted basket), so each group mixes chapters
# and carries a similar share of the records.
comtrade_dyads_code_groups <- function(codes, n_groups) {
  codes <- sort(unique(codes))
  lapply(seq_len(n_groups), function(g) codes[seq.int(g, length(codes), by = n_groups)])
}

# WDI economies the original allied pull left out of its partner list (scripts/05_ingest_sources.R
# `valid_countries`); kept out here so both files describe the same partner universe.
COMTRADE_DYADS_EXCLUDED_ISO3 <- c("ASM", "CHI", "GUM", "IMN", "LIE", "MAF", "MCO", "PRI", "XKX")

# Comtrade's code for the statistics of Taiwan ("Other Asia, nes", numeric code 490), as a reporter
# and as a partner. WDI has no Taiwan, so a plain WDI filter drops it; the combine step recodes it
# to TWN, the code the rest of the pipeline uses for Taiwan.
COMTRADE_DYADS_TAIWAN_ISO <- "S19"
COMTRADE_DYADS_TAIWAN_ISO3 <- "TWN"

# Recode Comtrade's Taiwan code (S19) to TWN in a vector of reporter or partner codes.
comtrade_dyads_recode_taiwan <- function(iso) {
  iso <- as.character(iso)
  iso[!is.na(iso) & iso == COMTRADE_DYADS_TAIWAN_ISO] <- COMTRADE_DYADS_TAIWAN_ISO3
  iso
}

# Column order of allied_comtrade_energy_data.csv without its unnamed row-index column, then the
# two flags this pull adds.
COMTRADE_DYADS_ALLIED_COLUMNS <- c(
  "type_code", "freq_code", "ref_period_id", "ref_year", "ref_month", "period",
  "reporter_code", "reporter_iso", "reporter_desc", "flow_code", "flow_desc",
  "partner_code", "partner_iso", "partner_desc", "partner2code", "partner2iso", "partner2desc",
  "classification_code", "classification_search_code", "is_original_classification",
  "cmd_code", "cmd_desc", "aggr_level", "is_leaf", "customs_code", "customs_desc",
  "mos_code", "mot_code", "mot_desc", "qty_unit_code", "qty_unit_abbr", "qty",
  "is_qty_estimated", "alt_qty_unit_code", "alt_qty_unit_abbr", "alt_qty",
  "is_alt_qty_estimated", "net_wgt", "is_net_wgt_estimated", "gross_wgt",
  "is_gross_wgt_estimated", "cifvalue", "fobvalue", "primary_value",
  "legacy_estimation_flag", "is_reported", "is_aggregate",
  "flow_direction", "reporter_req", "year_req", "count"
)
COMTRADE_DYADS_FLAG_COLUMNS <- c("classification_requested", "cmd_code_legacy_hs")

# Identifies one record. customs, mode of transport and second partner are requested as totals
# (C00, 0, 0), so the key reduces to reporter x partner x year x flow x commodity.
COMTRADE_DYADS_KEY <- c(
  "reporter_iso", "partner_iso", "ref_year", "flow_code", "cmd_code",
  "customs_code", "mot_code", "partner2code"
)

COMTRADE_DYADS_VALUE_COLUMNS <- c("qty", "alt_qty", "net_wgt", "gross_wgt", "cifvalue", "fobvalue", "primary_value")

comtrade_dyads_pad_hs6 <- function(x) {
  x <- gsub("\\D", "", as.character(x))
  x[!nzchar(x)] <- NA_character_
  # formatC(flag = "0") pads character input with spaces, so pad by hand.
  ifelse(is.na(x), NA_character_, paste0(strrep("0", pmax(0L, 6L - nchar(x))), x))
}

# HS6 basket: every distinct code in the master crosswalk(s), and the legacy bridge codes among
# them. `masters` is a list of data frames with an hs6 (or HS6) column and optionally legacy_hs.
comtrade_dyads_basket <- function(masters) {
  codes <- character()
  legacy <- character()
  for (m in masters) {
    if (is.null(m) || nrow(m) == 0) next
    code_col <- intersect(c("hs6", "HS6", "code", "cmd_code"), names(m))[1]
    if (is.na(code_col)) stop("HS6 basket has no code column.")
    hs6 <- comtrade_dyads_pad_hs6(m[[code_col]])
    codes <- c(codes, hs6)
    if ("legacy_hs" %in% names(m)) {
      flag <- toupper(as.character(m$legacy_hs)) %in% c("TRUE", "T", "1", "YES")
      legacy <- c(legacy, hs6[flag])
    }
  }
  codes <- sort(unique(codes[!is.na(codes)]))
  list(codes = codes, legacy = sort(unique(legacy[!is.na(legacy) & legacy %in% codes])))
}

# Codes of a supplementary pull (COMTRADE_DYADS_CODES): codes added to the master after the main
# pull are fetched into a chunk directory of their own and combined with it, rather than
# re-pulling the whole basket. A code outside the basket is an error, not a silent drop, and so is
# an entry that is not an HS6 code (dropping it could leave nothing requested, which means the
# whole basket). With no codes requested the pull is the whole basket.
comtrade_dyads_select_codes <- function(basket_codes, requested = character()) {
  requested <- trimws(requested)
  requested <- requested[!is.na(requested) & nzchar(requested)]
  if (length(requested) == 0) return(basket_codes)
  padded <- comtrade_dyads_pad_hs6(requested)
  if (anyNA(padded)) {
    stop("COMTRADE_DYADS_CODES lists entries that are not HS6 codes: ", paste(requested[is.na(padded)], collapse = ", "))
  }
  requested <- padded
  outside <- setdiff(requested, basket_codes)
  if (length(outside) > 0) {
    stop("COMTRADE_DYADS_CODES lists codes outside the HS6 basket: ", paste(outside, collapse = ", "))
  }
  sort(unique(requested))
}

# Chunk directories combined into one file must not share a code: the same reporter, partner and
# code would be fetched twice, and collapse_duplicates() would sum the two if a revision between
# the pulls made them differ. `codes_by_dir` is a named list of each directory's codes.txt.
comtrade_dyads_shared_codes <- function(codes_by_dir) {
  all_codes <- unlist(codes_by_dir, use.names = FALSE)
  sort(unique(all_codes[duplicated(all_codes)]))
}

# Basket codes that no chunk directory fetched. Once codes join the basket in a supplementary
# pull, a combine of the main directory alone (the rebuild command from before) would write a
# dyad file without them, and every cell they feed would lose its bilateral trade silently; a
# supplementary pull combined on its own would overwrite the file with just its few codes.
comtrade_dyads_uncovered_codes <- function(basket_codes, codes_by_dir) {
  sort(setdiff(basket_codes, unlist(codes_by_dir, use.names = FALSE)))
}

# The economies a dyad may connect: WDI countries (no regional or income aggregates) that
# Comtrade knows as a single partner, less the territories the allied pull excluded.
comtrade_dyads_economies <- function(country_info, partner_ref, excluded = COMTRADE_DYADS_EXCLUDED_ISO3) {
  iso <- toupper(trimws(as.character(country_info$iso3c)))
  if ("region" %in% names(country_info)) {
    region <- as.character(country_info$region)
    iso <- iso[is.na(region) | region != "Aggregates"]
  }
  iso <- unique(iso[!is.na(iso) & grepl("^[A-Z]{3}$", iso)])
  ref_iso <- as.character(partner_ref$iso_3[!as.logical(partner_ref$group)])
  sort(setdiff(intersect(iso, ref_iso), excluded))
}

# Data-availability rows (Comtrade getDA) -> one row per (year, reporter) in the pull.
# `da` needs year, reporter_iso and total_records. Reporters outside `economies` (the EU and
# ASEAN aggregates, Montserrat, ...) are dropped; Taiwan's "S19" is kept when include_taiwan.
comtrade_dyads_reporters <- function(da, economies, reporter_ref_iso, include_taiwan = TRUE) {
  keep_iso <- c(economies, if (isTRUE(include_taiwan)) COMTRADE_DYADS_TAIWAN_ISO)
  out <- da[da$reporter_iso %in% keep_iso & da$reporter_iso %in% reporter_ref_iso, , drop = FALSE]
  out <- out[!duplicated(out[, c("year", "reporter_iso")]), , drop = FALSE]
  out[order(out$year, out$reporter_iso), , drop = FALSE]
}

# Near-upper-bound record estimate per (year, reporter, flow) for the whole basket.
# Calibrated on the allied pull (22 reporters, 2020 and 2024, scaled from 212 to 298 codes) and
# the first all-partner chunk: basket rows per flow were at most 3.1% of a reporter's getDA
# totalRecords, and at most 1.1% for reporters that publish second-partner, customs or transport
# breakdowns (`extended`), whose totals are inflated by those rows. The caps are the largest
# reporters seen (Germany: ~9,300 import and ~20,600 export rows) with headroom. Without an
# `extended` column every reporter gets the standard (higher) rate.
comtrade_dyads_estimate <- function(reporters, flows, rate = c(standard = 0.031, extended = 0.011),
                                    cap = c(import = 10500, export = 25000), floor = 200,
                                    headroom = 1.1) {
  extended <- if ("extended" %in% names(reporters)) as.logical(reporters$extended) else rep(FALSE, nrow(reporters))
  extended[is.na(extended)] <- FALSE
  if (length(rate) == 1) rate <- c(standard = rate, extended = rate)
  r <- ifelse(extended, rate[["extended"]], rate[["standard"]])
  rows <- lapply(flows, function(f) {
    cap_f <- if (f %in% names(cap)) cap[[f]] else max(cap)
    data.frame(
      year = reporters$year,
      reporter_iso = reporters$reporter_iso,
      flow = f,
      est = pmax(floor, pmin(cap_f, headroom * r * as.numeric(reporters$total_records))),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# First-fit-decreasing bin packing: bin index for each element of `est` such that no bin's sum
# exceeds `target` (an element larger than target gets a bin of its own) and no bin holds more
# than `max_items` elements.
comtrade_dyads_pack <- function(est, target, max_items) {
  stopifnot(is.numeric(est), target > 0, max_items >= 1)
  bin <- integer(length(est))
  load <- numeric()
  n <- integer()
  for (i in order(-est, seq_along(est))) {
    fit <- which(load + est[i] <= target & n < max_items)
    if (length(fit) == 0) {
      load <- c(load, est[i])
      n <- c(n, 1L)
      bin[i] <- length(load)
    } else {
      b <- fit[1]
      load[b] <- load[b] + est[i]
      n[b] <- n[b] + 1L
      bin[i] <- b
    }
  }
  bin
}

# Request plan: one row per API call before any splitting, flows in `flow_order` first (imports
# first, so a short budget still yields import data for every reporter, exports derivable as
# mirrors), then year, then code group. Each reporter's estimate is shared equally across the
# `n_groups` code groups. reporters are space-separated ISO3 codes. `id_tag` is the letter before
# the batch number in chunk ids ("2020_import_g1_b01"); a re-plan uses another tag so its ids
# never collide with chunk files of an earlier plan. `exclude` (year, flow, code_group,
# reporter_iso) lists reporters already covered for that year, flow and code group.
comtrade_dyads_plan <- function(estimates, target_records = 70000, max_reporters = 50,
                                n_groups = 2L, flow_order = c("import", "export"), id_tag = "b",
                                exclude = NULL) {
  flows <- c(intersect(flow_order, unique(estimates$flow)), setdiff(unique(estimates$flow), flow_order))
  out <- list()
  for (f in flows) {
    for (y in sort(unique(estimates$year[estimates$flow == f]))) {
      e_all <- estimates[estimates$flow == f & estimates$year == y, , drop = FALSE]
      e_all$est <- e_all$est / n_groups
      for (g in seq_len(n_groups)) {
        e <- e_all
        if (!is.null(exclude) && nrow(exclude) > 0) {
          covered <- exclude$reporter_iso[exclude$year == y & exclude$flow == f & exclude$code_group == g]
          e <- e[!e$reporter_iso %in% covered, , drop = FALSE]
        }
        if (nrow(e) == 0) next
        bins <- comtrade_dyads_pack(e$est, target_records, max_reporters)
        for (b in sort(unique(bins))) {
          members <- e[bins == b, , drop = FALSE]
          members <- members[order(-members$est, members$reporter_iso), , drop = FALSE]
          out[[length(out) + 1]] <- data.frame(
            chunk_id = sprintf("%d_%s_g%d_%s%02d", y, f, g, id_tag, b),
            year = y,
            flow = f,
            code_group = g,
            n_reporters = nrow(members),
            est_records = round(sum(members$est)),
            reporters = paste(members$reporter_iso, collapse = " "),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  if (length(out) == 0) {
    return(data.frame(chunk_id = character(), year = integer(), flow = character(), code_group = integer(),
                      n_reporters = integer(), est_records = numeric(), reporters = character()))
  }
  do.call(rbind, out)
}

# Re-plan the calls not yet made: rows of `plan` flagged in `started` (fetched, or split into
# halves) are kept as they are, and every other (year, flow, code group, reporter) is packed
# afresh from `estimates` under ids tagged `id_tag`, so no reporter is fetched twice.
comtrade_dyads_replan <- function(plan, started, estimates, id_tag = "r", ...) {
  stopifnot(length(started) == nrow(plan))
  kept <- plan[started, , drop = FALSE]
  covered <- if (nrow(kept) == 0) {
    data.frame(year = integer(), flow = character(), code_group = integer(), reporter_iso = character())
  } else {
    do.call(rbind, lapply(seq_len(nrow(kept)), function(i) {
      data.frame(year = kept$year[i], flow = kept$flow[i], code_group = kept$code_group[i],
                 reporter_iso = strsplit(kept$reporters[i], " ", fixed = TRUE)[[1]],
                 stringsAsFactors = FALSE)
    }))
  }
  fresh <- comtrade_dyads_plan(estimates, id_tag = id_tag, exclude = covered, ...)
  if (any(fresh$chunk_id %in% plan$chunk_id)) stop("Re-plan chunk ids collide with the earlier plan; use another id_tag.")
  rbind(kept, fresh)
}

# A truncated request is split in two: by reporter while it has several, else by commodity.
comtrade_dyads_split <- function(reporters, codes) {
  if (length(reporters) > 1) {
    h <- ceiling(length(reporters) / 2)
    return(list(
      a = list(reporters = reporters[seq_len(h)], codes = codes),
      b = list(reporters = reporters[-seq_len(h)], codes = codes)
    ))
  }
  if (length(codes) > 1) {
    h <- ceiling(length(codes) / 2)
    return(list(
      a = list(reporters = reporters, codes = codes[seq_len(h)]),
      b = list(reporters = reporters, codes = codes[-seq_len(h)])
    ))
  }
  stop("A single reporter and commodity exceed the Comtrade record cap; cannot split further.")
}

comtrade_dyads_is_truncated <- function(n_rows, cap = COMTRADE_DYADS_RECORD_CAP) {
  n_rows >= cap
}

# comtradr returns data.frame(count = 0) when a request matches no records.
comtrade_dyads_is_empty <- function(df) {
  is.null(df) || !is.data.frame(df) || nrow(df) == 0 ||
    (identical(names(df), "count") && all(df$count %in% 0))
}

# One raw chunk (comtradr tidy columns) -> rows of the output file.
# - drops comtradr's no-data placeholder;
# - keeps the API totals (customs C00, mode of transport 0, second partner World), the only
#   rows requested; anything else is counted in attr(, "dropped") rather than kept;
# - keeps reporters and partners in `economies` (World, the "nes" areas, bunkers, free zones and
#   territories outside WDI go), with Taiwan's S19 recoded to TWN (the default) or dropped per
#   `taiwan`; TWN is then in scope whether or not `economies` lists it;
# - adds the allied file's bookkeeping columns and the two flags.
comtrade_dyads_standardize <- function(raw, economies, legacy_codes = character(),
                                       taiwan = c("TWN", "drop"),
                                       classification_requested = "HS") {
  taiwan <- match.arg(taiwan)
  empty <- as.data.frame(
    stats::setNames(replicate(length(c(COMTRADE_DYADS_ALLIED_COLUMNS, COMTRADE_DYADS_FLAG_COLUMNS)), logical(), simplify = FALSE),
                    c(COMTRADE_DYADS_ALLIED_COLUMNS, COMTRADE_DYADS_FLAG_COLUMNS))
  )
  if (comtrade_dyads_is_empty(raw)) {
    attr(empty, "dropped") <- c(placeholder = 0L, non_total = 0L, aggregate_or_outside = 0L)
    return(empty)
  }
  df <- as.data.frame(raw, stringsAsFactors = FALSE)
  n0 <- nrow(df)

  df <- df[!is.na(df$reporter_iso) & !is.na(df$cmd_code), , drop = FALSE]
  n_placeholder <- n0 - nrow(df)

  is_total <- as.character(df$customs_code) == "C00" &
    suppressWarnings(as.numeric(df$mot_code)) == 0 &
    suppressWarnings(as.numeric(df$partner2code)) == 0
  is_total[is.na(is_total)] <- FALSE
  n_non_total <- sum(!is_total)
  df <- df[is_total, , drop = FALSE]

  df$reporter_iso <- as.character(df$reporter_iso)
  df$partner_iso <- as.character(df$partner_iso)
  if (identical(taiwan, "TWN")) {
    df$reporter_iso <- comtrade_dyads_recode_taiwan(df$reporter_iso)
    df$partner_iso <- comtrade_dyads_recode_taiwan(df$partner_iso)
    economies <- union(economies, COMTRADE_DYADS_TAIWAN_ISO3)
  }
  in_scope <- df$reporter_iso %in% economies & df$partner_iso %in% economies
  n_outside <- sum(!in_scope)
  df <- df[in_scope, , drop = FALSE]

  df$cmd_code <- comtrade_dyads_pad_hs6(df$cmd_code)
  df$flow_direction <- unname(c(M = "import", X = "export")[as.character(df$flow_code)])
  df$reporter_req <- df$reporter_iso
  df$year_req <- as.integer(df$ref_year)
  df$count <- rep(NA_integer_, nrow(df))
  df$classification_requested <- rep(classification_requested, nrow(df))
  df$cmd_code_legacy_hs <- df$cmd_code %in% legacy_codes

  for (col in setdiff(COMTRADE_DYADS_ALLIED_COLUMNS, names(df))) df[[col]] <- rep(NA, nrow(df))
  out <- df[, c(COMTRADE_DYADS_ALLIED_COLUMNS, COMTRADE_DYADS_FLAG_COLUMNS), drop = FALSE]
  rownames(out) <- NULL
  attr(out, "dropped") <- c(placeholder = n_placeholder, non_total = n_non_total, aggregate_or_outside = n_outside)
  out
}

# Rows that share COMTRADE_DYADS_KEY. The API returns one total per key, so a duplicate means a
# reporter was fetched twice (identical rows: one copy is kept) or two partner codes map to one
# ISO3 code (different rows: the value and quantity columns are summed, first value of
# everything else).
comtrade_dyads_collapse_duplicates <- function(df) {
  key <- do.call(paste, c(lapply(COMTRADE_DYADS_KEY, function(k) as.character(df[[k]])), sep = "\r"))
  dup <- duplicated(key) | duplicated(key, fromLast = TRUE)
  if (!any(dup)) {
    attr(df, "n_collapsed") <- 0L
    return(df)
  }
  keep <- df[!dup, , drop = FALSE]
  d <- df[dup, , drop = FALSE]
  k <- key[dup]
  same <- duplicated(d)
  d <- d[!same, , drop = FALSE]
  k <- k[!same]
  first <- d[!duplicated(k), , drop = FALSE]
  for (col in intersect(COMTRADE_DYADS_VALUE_COLUMNS, names(d))) {
    sums <- tapply(suppressWarnings(as.numeric(d[[col]])), k, sum, na.rm = TRUE)
    first[[col]] <- as.numeric(sums[k[!duplicated(k)]])
  }
  out <- rbind(keep, first)
  attr(out, "n_collapsed") <- sum(dup) - nrow(first)
  attr(out, "n_identical") <- sum(same)
  out
}

# Validation of the combined file; returns a character vector of problems (empty when clean).
# Self-dyads (a reporter's re-imports from itself) are allowed by default: the allied file keeps
# them as reported, and the partnership builders drop reporter_iso == partner_iso themselves.
comtrade_dyads_validate <- function(df, economies, allow_self = TRUE) {
  problems <- character()
  missing <- setdiff(c(COMTRADE_DYADS_ALLIED_COLUMNS, COMTRADE_DYADS_FLAG_COLUMNS), names(df))
  if (length(missing) > 0) problems <- c(problems, paste("missing columns:", paste(missing, collapse = ", ")))
  if (nrow(df) == 0) return(c(problems, "no rows"))
  key <- do.call(paste, c(lapply(COMTRADE_DYADS_KEY, function(k) as.character(df[[k]])), sep = "\r"))
  if (anyDuplicated(key) > 0) problems <- c(problems, paste(sum(duplicated(key)), "duplicate keys"))
  if (any(!df$partner_iso %in% economies)) {
    problems <- c(problems, paste("partners outside the economy list:", paste(utils::head(unique(df$partner_iso[!df$partner_iso %in% economies]), 10), collapse = ", ")))
  }
  if (any(!df$reporter_iso %in% economies)) {
    problems <- c(problems, paste("reporters outside the economy list:", paste(utils::head(unique(df$reporter_iso[!df$reporter_iso %in% economies]), 10), collapse = ", ")))
  }
  if (!isTRUE(allow_self) && any(df$reporter_iso == df$partner_iso)) problems <- c(problems, "self-dyads present")
  if (any(!df$flow_direction %in% c("import", "export"))) problems <- c(problems, "flow_direction outside import/export")
  if (any(as.character(df$customs_code) != "C00") || any(as.numeric(df$mot_code) != 0) || any(as.numeric(df$partner2code) != 0)) {
    problems <- c(problems, "non-total customs / transport / second-partner rows")
  }
  if (any(is.na(df$primary_value))) problems <- c(problems, paste(sum(is.na(df$primary_value)), "rows with NA primary_value"))
  if (any(!grepl("^[0-9]{6}$", df$cmd_code))) problems <- c(problems, "cmd_code not 6 digits")
  problems
}

## Reporter-World totals for comtrade_energy_trade.csv -----------------------------------------
#
# comtrade_energy_trade.csv holds each reporter's trade with the World in the basket codes; the
# trade themes (scripts/10) take basket exports, Balassa RCA and the export HHI from it, with
# comtrade_total_export.csv (each reporter's all-product exports) as the RCA denominator.
# scripts/05_ingest_sources.R pulls it one call per reporter and flow (~400 calls) on the product
# list of its day: the file staged 2026-09-30 lacked 60 of the 256 basket codes and 13 of the
# economies with 2024 data (DR Congo among them). A partner = "everything" request of this pull
# returns the same World record (partner code 0) beside the partner rows, so the energy_trade
# stage derives the file from those World rows instead: every code and every reporter of the pull,
# no further data calls. On the keys both pulls held for 2024 the two agreed to the dollar on 87% of
# rows; the rest are Comtrade's revisions between the pulls (median 1%), and the newer release is
# the one kept.

# Columns comtrade_energy_trade.csv and comtrade_total_export.csv carry as text although they look
# numeric (write.csv quoted them); read them as character so a rewrite leaves the rows it keeps
# unchanged.
COMTRADE_ENERGY_TRADE_TEXT_COLUMNS <- c("cmd_code", "period", "mos_code")

# `df` with the columns of `template`, in its order and with its column classes (a column `df`
# lacks is NA), so a rewritten file reads back as before. The value and weight columns are always
# double: an extract whose weights happen to be small whole numbers reads as integer, and a larger
# value would then become NA.
comtrade_energy_trade_conform <- function(df, template) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  like <- function(x, proto) {
    switch(class(proto)[1],
      character = as.character(x),
      integer = as.integer(x),
      numeric = as.numeric(x),
      logical = as.logical(x),
      x
    )
  }
  cols <- lapply(names(template), function(col) {
    value <- if (col %in% names(df)) df[[col]] else rep(NA, nrow(df))
    if (col %in% COMTRADE_DYADS_VALUE_COLUMNS) as.numeric(value) else like(value, template[[col]])
  })
  as.data.frame(stats::setNames(cols, names(template)), stringsAsFactors = FALSE, optional = TRUE)
}

# Equal trade values, up to the 15 significant digits write.csv keeps (a value read back from the
# staged file differs from the API's in the last bits). NA equals NA only.
comtrade_energy_trade_same_value <- function(a, b) {
  a <- as.numeric(a)
  b <- as.numeric(b)
  same <- abs(a - b) <= 1e-12 * pmax(abs(a), abs(b))
  same[is.na(same)] <- FALSE
  same | (is.na(a) & is.na(b))
}

# The World rows of one raw chunk (comtradr tidy columns), as API totals over customs procedure,
# mode of transport and second partner, with the flow_direction column the energy-trade file has.
comtrade_dyads_world_totals <- function(raw) {
  if (comtrade_dyads_is_empty(raw)) return(NULL)
  df <- as.data.frame(raw, stringsAsFactors = FALSE)
  num <- function(x) suppressWarnings(as.numeric(x))
  keep <- !is.na(df$reporter_iso) & !is.na(df$cmd_code) & num(df$partner_code) == 0 &
    as.character(df$customs_code) == "C00" & num(df$mot_code) == 0 & num(df$partner2code) == 0
  keep[is.na(keep)] <- FALSE
  df <- df[keep, , drop = FALSE]
  df$cmd_code <- comtrade_dyads_pad_hs6(df$cmd_code)
  df$flow_direction <- unname(c(M = "import", X = "export")[as.character(df$flow_code)])
  rownames(df) <- NULL
  df
}

# The energy-trade table derived from the World rows of the dyad pull, in the table's year(s).
# `pulled_codes` are the codes the chunk directories requested (their codes.txt): for each of them
# every reporter-flow the World rows hold comes from the World rows, so a code and a reporter are
# never mixed from two releases (a reporter that re-filed in a newer HS edition, as Georgia did for
# 2024, would otherwise hold both editions' codes for the same goods). A key the table held that
# the newer release no longer has is dropped with the rest of that reporter-flow's old rows. Rows
# of codes the pull did not request, and of reporter-flows the World rows lack entirely, are kept
# as they are, so no reporter is lost. Taiwan's Comtrade code S19 becomes TWN, as in the dyad file
# (or is dropped). Columns and column types follow `existing`.
#
# attr(, "changes"): `keys` counts the table's keys by fate (unchanged / revised value / new /
# dropped, and kept rows of codes outside the pull or of reporter-flows the pull lacks); `by_flow`
# gives reporters, codes and rows per flow before and after.
comtrade_energy_trade_from_world <- function(existing, world, pulled_codes, taiwan = c("TWN", "drop")) {
  taiwan <- match.arg(taiwan)
  required <- c("reporter_iso", "ref_year", "flow_code", "cmd_code", "primary_value")
  if (is.null(world) || nrow(world) == 0) stop("No World rows to derive the energy-trade table from.")
  missing <- setdiff(required, intersect(names(existing), names(world)))
  if (length(missing) > 0) stop("Energy-trade derivation needs column(s): ", paste(missing, collapse = ", "))

  years <- unique(as.integer(existing$ref_year))
  pulled_codes <- unique(comtrade_dyads_pad_hs6(pulled_codes))

  new <- as.data.frame(world, stringsAsFactors = FALSE)
  new$cmd_code <- comtrade_dyads_pad_hs6(new$cmd_code)
  new$reporter_iso <- as.character(new$reporter_iso)
  if (identical(taiwan, "TWN")) {
    new$reporter_iso <- comtrade_dyads_recode_taiwan(new$reporter_iso)
  } else {
    new <- new[new$reporter_iso != COMTRADE_DYADS_TAIWAN_ISO, , drop = FALSE]
  }
  new <- new[as.integer(new$ref_year) %in% years & new$cmd_code %in% pulled_codes, , drop = FALSE]
  if (nrow(new) == 0) {
    stop("The World rows hold no requested code in the energy-trade year(s) ", paste(sort(years), collapse = ", "), ".")
  }
  if (!"flow_direction" %in% names(new)) {
    new$flow_direction <- unname(c(M = "import", X = "export")[as.character(new$flow_code)])
  }
  key_of <- function(df) paste(df$reporter_iso, as.integer(df$ref_year), df$flow_code, comtrade_dyads_pad_hs6(df$cmd_code))
  new_key <- key_of(new)
  if (anyDuplicated(new_key) > 0) {
    stop(sum(duplicated(new_key)), " duplicate reporter x year x flow x code World rows; were chunks fetched twice?")
  }

  flow_of <- function(df) paste(as.integer(df$ref_year), df$flow_code, df$reporter_iso)
  ex_key <- key_of(existing)
  in_pull <- comtrade_dyads_pad_hs6(existing$cmd_code) %in% pulled_codes
  in_world <- flow_of(existing) %in% flow_of(new)
  replaced <- in_pull & in_world

  out <- rbind(
    comtrade_energy_trade_conform(existing[!replaced, , drop = FALSE], existing),
    comtrade_energy_trade_conform(new, existing)
  )
  out <- out[order(as.integer(out$ref_year), out$flow_code, out$reporter_iso, comtrade_dyads_pad_hs6(out$cmd_code)), , drop = FALSE]
  rownames(out) <- NULL

  old_value <- stats::setNames(as.numeric(existing$primary_value[replaced]), ex_key[replaced])
  both <- intersect(names(old_value), new_key)
  same <- comtrade_energy_trade_same_value(old_value[both], new$primary_value[match(both, new_key)])
  keys <- c(
    unchanged = sum(same),
    revised = sum(!same),
    new = sum(!new_key %in% ex_key),
    dropped = sum(!ex_key[replaced] %in% new_key),
    kept_outside_pull = sum(!in_pull),
    kept_reporter_flows = sum(in_pull & !in_world)
  )
  per_flow <- function(df, label) {
    flows <- sort(unique(as.character(df$flow_code)))
    counts <- data.frame(
      flow_code = flows,
      reporters = vapply(flows, function(f) length(unique(df$reporter_iso[df$flow_code == f])), integer(1)),
      codes = vapply(flows, function(f) length(unique(comtrade_dyads_pad_hs6(df$cmd_code[df$flow_code == f]))), integer(1)),
      rows = vapply(flows, function(f) sum(df$flow_code == f), integer(1)),
      stringsAsFactors = FALSE, row.names = NULL
    )
    stats::setNames(counts, c("flow_code", paste0(c("reporters", "codes", "rows"), "_", label)))
  }
  by_flow <- merge(per_flow(existing, "before"), per_flow(out, "after"), by = "flow_code", all = TRUE)
  attr(out, "changes") <- list(keys = keys, by_flow = by_flow)
  out
}

# The all-product (TOTAL) export rows of a fetch (comtradr tidy columns, one World record per
# reporter and year) in place of `existing`'s rows of comtrade_total_export.csv for the same
# reporters and years; reporters the fetch did not return keep their rows. The table is the
# denominator of the trade themes' RCA, so it should hold every export reporter of the
# energy-trade table, from the same release. Taiwan's S19 becomes TWN (or is dropped). Columns
# and column types follow `existing`. attr(, "changes"): reporters refreshed (of them with a
# revised value), added and kept.
comtrade_total_export_refresh <- function(existing, fetched, years, taiwan = c("TWN", "drop")) {
  taiwan <- match.arg(taiwan)
  required <- c("reporter_iso", "ref_year", "flow_code", "cmd_code", "partner_code", "customs_code",
                "mot_code", "partner2code", "primary_value")
  new <- if (is.null(fetched)) existing[0, , drop = FALSE] else as.data.frame(fetched, stringsAsFactors = FALSE)
  missing <- setdiff(required, intersect(names(existing), names(new)))
  if (length(missing) > 0) stop("Total-export refresh needs column(s): ", paste(missing, collapse = ", "))
  years <- as.integer(years)

  # Only the API totals (customs procedure, mode of transport and second partner), as the file holds.
  num <- function(x) suppressWarnings(as.numeric(x))
  total <- !is.na(new$reporter_iso) & num(new$partner_code) == 0 & as.character(new$flow_code) == "X" &
    toupper(as.character(new$cmd_code)) == "TOTAL" & as.integer(new$ref_year) %in% years &
    as.character(new$customs_code) == "C00" & num(new$mot_code) == 0 & num(new$partner2code) == 0
  total[is.na(total)] <- FALSE
  new <- new[total, , drop = FALSE]
  new$reporter_iso <- as.character(new$reporter_iso)
  if (identical(taiwan, "TWN")) {
    new$reporter_iso <- comtrade_dyads_recode_taiwan(new$reporter_iso)
  } else {
    new <- new[new$reporter_iso != COMTRADE_DYADS_TAIWAN_ISO, , drop = FALSE]
  }
  key_of <- function(df) paste(df$reporter_iso, as.integer(df$ref_year))
  new_key <- key_of(new)
  if (anyDuplicated(new_key) > 0) {
    stop(sum(duplicated(new_key)), " duplicate reporter x year TOTAL export rows in the fetch.")
  }

  ex_key <- key_of(existing)
  replaced <- ex_key %in% new_key
  out <- rbind(
    comtrade_energy_trade_conform(existing[!replaced, , drop = FALSE], existing),
    comtrade_energy_trade_conform(new, existing)
  )
  out <- out[order(as.integer(out$ref_year), out$reporter_iso), , drop = FALSE]
  rownames(out) <- NULL

  revised <- !comtrade_energy_trade_same_value(existing$primary_value[replaced],
                                               new$primary_value[match(ex_key[replaced], new_key)])
  attr(out, "changes") <- c(
    refreshed = sum(replaced),
    revised = sum(revised),
    added = sum(!new_key %in% ex_key),
    kept = sum(!replaced)
  )
  out
}
