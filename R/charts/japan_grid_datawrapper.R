# Japan's position in grid equipment: four Datawrapper-ready tables.
#
#   1  specialisation   Where is Japan specialised within grid equipment? Japan's RCA by
#                       grid component, latest year, with its share of world exports.
#   2  trend            Is it holding that position? World export share 2000-2023 for
#                       five strategic components: Japan, China, South Korea, Germany, US.
#   3  vulnerability    How concentrated are Japan's own grid-equipment imports? Supplier
#                       HHI per component, largest supplier, China's share, trade balance.
#   4  partners         Which partner markets best complement Japan? Partner markets for
#                       Japanese grid manufacturing ranked by a dyad partnership score.
#
# Components
# ----------
# One list of eleven components (JGD_COMPONENTS) serves charts 1-3, built from the
# repo's energy-tech crosswalk (Electric Grid, Midstream) but split finer where the
# HS codes allow it - large power transformers (>10 MVA) apart from the rest, static
# converters apart from power semiconductors - and with the crosswalk's two traps left
# out: cross-border electricity (2716.00) and chapter 44 softwood logs.
# Electrical steel is the 7225.11/19 headings, which in HS92 are one code (7225.10)
# that does not separate grain-oriented from non-oriented steel.
#
# Sources
# -------
#   Chart 1  UN Comtrade 2024 (comtrade_energy_trade.csv + comtrade_total_export.csv),
#            RCA defined as in the index pipeline (trade_core_build_comtrade_rca()).
#            Components whose HS2022 codes the 2024 pull lacks - power semiconductors,
#            grid automation, electrical steel - use the Atlas of Economic Complexity
#            HS92 panel (cleaned UN Comtrade) for its latest year instead. Each row's
#            Source and Year columns say which; a row is never mixed across sources.
#   Chart 2  Atlas HS92 panel, 2000 to its latest year (2023). One source end to end,
#            so no classification break inside the series.
#   Chart 3  UN Comtrade, Japan's 2024 imports by partner, from the Comtrade API (keyed
#            if COMTRADE_API_KEY is set, the free preview endpoint otherwise), with
#            Japan's exports to World for the trade balance. The repo's bilateral pull
#            (allied_comtrade_energy_data.csv) lacks converters, power semiconductors,
#            automation, electrical steel and some transformer codes, so it is not used.
#            Cached under <processed_dir>/charts/cache/; JGD_REFRESH=true re-pulls.
#   Chart 4  The partnership index outputs (partnership_index_outputs.rds): the
#            friendshore, opportunity and development indices for each Japan-partner
#            dyad in Electric Grid / Midstream. The pipeline's own PSI is a country-level
#            composite, not a dyad score, so the dyad score here applies the same PSI
#            weights (config/weights.yml: psi) to the three dyad indices. Development
#            potential is only built for emerging economies; where it is missing the
#            other two weights are rescaled to sum to one, and the `Development` column
#            is blank. Development rows vary by sub-sector and are averaged per partner,
#            which is what build_partnership_strength_index() does with them.
#            The indices are not scaled by market size, so partners are screened to
#            those importing at least JGD_MIN_MARKET_BN of the chart 1 grid codes in
#            the 2024 Comtrade pull; that also drops partners that do not report.
#            The Japan grid subset is cached; the full file is ~185MB and loading it
#            under Git Bash on Windows has segfaulted - run from PowerShell if it does.
#
# Outputs (data/processed/charts/), all Datawrapper-ready:
#   japan_grid_1_specialisation_<year>.csv     bar/dot chart; reference line at RCA = 1
#   japan_grid_2_trend_<start>_<end>_wide.csv   year x "<component>: <country>"
#   japan_grid_2_trend_<start>_<end>_<slug>.csv year x country, one per component
#   japan_grid_3_import_concentration_<year>.csv
#   japan_grid_4_partners_<year>.csv            stacked bar of weighted contributions
#
# Run:
#   Rscript R/charts/japan_grid_datawrapper.R
#
# Environment:
#   JGD_TREND_FROM    first year of chart 2 (default 2000)
#   JGD_TOP_PARTNERS  partners in chart 4 (default 15)
#   JGD_MIN_MARKET_BN chart 4 market screen: partner's 2024 grid-equipment imports,
#                     USD bn (default 1)
#   JGD_REFRESH       "true" to re-pull Comtrade and re-extract the PSI subset

source(local({
  resolve_bootstrap_path <- function() {
    candidate_starts <- character()

    sf <- tryCatch(sys.frame(1)$ofile, error = function(e) "")
    if (!is.null(sf) && nzchar(sf)) candidate_starts <- c(candidate_starts, dirname(sf))

    frame_ofiles <- vapply(sys.frames(), function(fr) {
      val <- tryCatch(fr$ofile, error = function(e) "")
      if (is.null(val) || !nzchar(val)) "" else dirname(val)
    }, character(1))
    candidate_starts <- c(candidate_starts, frame_ofiles[nzchar(frame_ofiles)])

    fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (length(fa) > 0) candidate_starts <- c(candidate_starts, dirname(sub("^--file=", "", fa[1])))

    candidate_starts <- unique(c(candidate_starts, getwd()))

    for (start in candidate_starts) {
      d <- normalizePath(start, winslash = "/", mustWork = FALSE)
      while (dirname(d) != d) {
        bootstrap <- file.path(d, "scripts", "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)

        bootstrap <- file.path(d, "utils", "bootstrap.R")
        if (file.exists(bootstrap)) return(bootstrap)

        d <- dirname(d)
      }
    }

    stop("Unable to resolve script path for bootstrap.")
  }

  resolve_bootstrap_path()
}))

source(file.path(repo_root, "R", "charts", "tech_security_common.R"))

## Configuration -------------------------------------------------------------

JGD_FOCUS_ISO <- "JPN"
JGD_REPORTER_CODE <- 392L     # Japan, Comtrade numeric
JGD_CHINA_CODE <- 156L
JGD_DEFAULT_TREND_FROM <- 2000L
JGD_DEFAULT_TOP_PARTNERS <- 15L
JGD_DEFAULT_MIN_MARKET_BN <- 1

JGD_TREND_COUNTRIES <- c(JPN = "Japan", CHN = "China", KOR = "South Korea",
                         DEU = "Germany", USA = "United States")

# `hs`: HS2022 codes, looked up in the 2024 Comtrade data. `hs92`: Atlas codes, used
# for chart 2 and for chart 1 rows the 2024 pull cannot cover. `strategic`: the five
# components followed over time in chart 2 - the four where Japan's position is worth
# defending (electrical steel, grid automation and power semiconductors, where its RCA
# is well above 1, and static converters, the HVDC and renewables interface where it
# is close to 1) plus large power transformers, the build-out bottleneck item.
JGD_COMPONENTS <- tibble::tribble(
  ~slug,          ~label,                                          ~hs,                                                ~hs92,                                     ~strategic,
  "lpt",          "Large power transformers (>10 MVA)",            "850423",                                           "850423",                                  TRUE,
  "transformers", "Distribution & other transformers",             "850421|850422|850431|850432|850433|850434",        "850421|850422|850431|850432|850433|850434", FALSE,
  "tx_parts",     "Transformer & converter parts",                 "850490",                                           "850490",                                  FALSE,
  "switchgear",   "HV switchgear & circuit breakers (>1 kV)",      "853521|853529|853530|853540|853590",               "853521|853529|853530|853540|853590",      FALSE,
  "capacitors",   "Power capacitors & HV fuses",                   "853210|853510",                                    "853210|853510",                           FALSE,
  "cables",       "HV cables (>1 kV)",                             "854460",                                           "854460",                                  FALSE,
  "panels",       "Control panels & switchboards",                 "853710|853720",                                    "853710|853720",                           FALSE,
  "automation",   "Grid automation & metering",                    "903289|902830|902890",                             "903289|902830|902890",                    TRUE,
  "converters",   "Static converters (inverters, rectifiers)",     "850440",                                           "850440",                                  TRUE,
  "power_semis",  "Power semiconductors (transistors, thyristors)", "854129|854130",                                   "854129|854130",                           TRUE,
  "elec_steel",   "Electrical steel (silicon, flat-rolled)",       "722511|722519",                                    "722510",                                  TRUE
)

JGD_PREVIEW_API <- "https://comtradeapi.un.org/public/v1/preview/C/A/HS"
JGD_KEYED_API <- "https://comtradeapi.un.org/data/v1/get/C/A/HS"
JGD_PARTNERS_REF <- "https://comtradeapi.un.org/files/v1/app/reference/partnerAreas.json"
JGD_BATCH <- 4L
JGD_ROW_LIMIT <- 500L

## Helpers -------------------------------------------------------------------

jgd_split <- function(x) {
  out <- strsplit(x, "|", fixed = TRUE)[[1]]
  out[nzchar(out)]
}

# Long code map: one row per component x code, for one code column.
jgd_code_map <- function(components, column = "hs") {
  do.call(rbind, lapply(seq_len(nrow(components)), function(i) {
    data.frame(slug = components$slug[i], hs6 = jgd_split(components[[column]][i]),
               stringsAsFactors = FALSE)
  }))
}

# TRUE where every HS2022 code of a component is in the 2024 pull.
jgd_covered <- function(components, pull_codes) {
  vapply(components$hs, function(h) all(jgd_split(h) %in% pull_codes), logical(1),
         USE.NAMES = FALSE)
}

# Herfindahl-Hirschman index on the 0-10,000 scale.
jgd_hhi <- function(values) {
  values <- values[!is.na(values) & values > 0]
  if (length(values) == 0) return(NA_real_)
  sum((100 * values / sum(values))^2)
}

## RCA (chart 1) -------------------------------------------------------------

# Balassa RCA and world export share for every reporter x component in one source.
# World is the reporters present in both inputs, so the two halves of the ratio
# cover the same countries; a reporter exporting none of a component gets 0.
jgd_rca <- function(exports, totals, code_map) {
  reporters <- intersect(unique(exports$reporter_iso), totals$reporter_iso[totals$total > 0])
  exports <- exports[exports$reporter_iso %in% reporters, , drop = FALSE]
  totals <- totals[totals$reporter_iso %in% reporters, , drop = FALSE]

  joined <- merge(exports, code_map, by = "hs6")
  by_row <- if (nrow(joined) > 0) {
    stats::aggregate(value ~ slug + reporter_iso, data = joined, FUN = sum)
  } else {
    data.frame(slug = character(), reporter_iso = character(), value = numeric())
  }

  grid <- expand.grid(slug = unique(code_map$slug), reporter_iso = reporters,
                      stringsAsFactors = FALSE)
  out <- merge(grid, by_row, by = c("slug", "reporter_iso"), all.x = TRUE)
  out$value[is.na(out$value)] <- 0
  out <- merge(out, totals, by = "reporter_iso")

  world <- stats::aggregate(value ~ slug, data = out, FUN = sum)
  names(world)[2] <- "world_value"
  out <- merge(out, world, by = "slug")

  out$world_share <- out$value / out$world_value
  out$rca <- (out$value / out$total) / (out$world_value / sum(totals$total))
  out
}

# Global exporter HHI per component (context for chart 3): how concentrated world
# supply is, independent of where Japan happens to buy.
jgd_global_hhi <- function(rca) {
  out <- lapply(split(rca, rca$slug), function(d) data.frame(slug = d$slug[1], global_hhi = jgd_hhi(d$value)))
  do.call(rbind, out)
}

jgd_specialisation_table <- function(rca_focus, components = JGD_COMPONENTS) {
  d <- merge(components[, c("slug", "label")], rca_focus, by = "slug")
  d <- d[order(-d$rca), , drop = FALSE]
  out <- data.frame(
    Component = d$label,
    `Japan RCA` = round(d$rca, 2),
    `Japan share of world exports (%)` = round(100 * d$world_share, 1),
    `Japan exports (USD m)` = round(d$value / 1e6, 1),
    Source = d$source_label,
    Year = d$year,
    `HS6 codes` = d$hs_used,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

## Atlas (charts 1 and 2) ----------------------------------------------------

# One chunked pass over the ~900MB Atlas panel: exports of the requested codes from
# `from_year`, and every country's total exports per year (needed for RCA).
jgd_atlas <- function(repo_root, codes, from_year) {
  path <- file.path(repo_root, config$raw_data_dir, "hs92_country_product_year_6.csv")
  if (!file.exists(path)) stop("Atlas HS6 panel not found: ", path)

  selected <- list()
  totals <- list()
  message("Reading the Atlas HS92 panel (large file, chunked) ...")
  readr::read_csv_chunked(
    path,
    readr::SideEffectChunkCallback$new(function(chunk, pos) {
      chunk <- chunk[chunk$year >= from_year & !is.na(chunk$export_value) &
                       chunk$export_value > 0, , drop = FALSE]
      if (nrow(chunk) == 0) return(invisible())
      chunk$hs6 <- ts_hs6(chunk$product_hs92_code)
      totals[[length(totals) + 1]] <<- stats::aggregate(
        export_value ~ country_iso3_code + year, data = chunk, FUN = sum
      )
      hit <- chunk[chunk$hs6 %in% codes, c("country_iso3_code", "hs6", "year", "export_value")]
      if (nrow(hit) > 0) selected[[length(selected) + 1]] <<- as.data.frame(hit)
    }),
    col_types = readr::cols_only(
      country_iso3_code = readr::col_character(), product_hs92_code = readr::col_character(),
      year = readr::col_integer(), export_value = readr::col_double()
    ),
    chunk_size = 2e6, progress = FALSE
  )

  exports <- do.call(rbind, selected)
  names(exports) <- c("reporter_iso", "hs6", "year", "value")
  tot <- stats::aggregate(export_value ~ country_iso3_code + year, data = do.call(rbind, totals),
                          FUN = sum)
  names(tot) <- c("reporter_iso", "year", "total")

  absent <- setdiff(codes, exports$hs6)
  if (length(absent) > 0) stop("Atlas has no exports under: ", paste(absent, collapse = ", "))
  list(exports = exports, totals = tot, latest_year = max(exports$year))
}

## Trend (chart 2) -----------------------------------------------------------

jgd_trend <- function(atlas_exports, code_map, countries = JGD_TREND_COUNTRIES) {
  d <- merge(atlas_exports, code_map, by = "hs6")
  world <- stats::aggregate(value ~ slug + year, data = d, FUN = sum)
  names(world)[3] <- "world_value"

  focus <- d[d$reporter_iso %in% names(countries), , drop = FALSE]
  by_country <- stats::aggregate(value ~ slug + reporter_iso + year, data = focus, FUN = sum)

  grid <- merge(world[, c("slug", "year")],
                data.frame(reporter_iso = names(countries), stringsAsFactors = FALSE), by = NULL)
  out <- merge(grid, by_country, by = c("slug", "reporter_iso", "year"), all.x = TRUE)
  out$value[is.na(out$value)] <- 0
  out <- merge(out, world, by = c("slug", "year"))
  out$share_pct <- 100 * out$value / out$world_value
  out$country <- unname(countries[out$reporter_iso])
  out
}

# Year x columns. One component: columns are countries. Several: "<label>: <country>".
jgd_trend_wide <- function(trend, slugs, labels, countries = JGD_TREND_COUNTRIES, digits = 2L) {
  d <- trend[trend$slug %in% slugs, , drop = FALSE]
  d$column <- if (length(slugs) == 1) d$country else paste0(unname(labels[d$slug]), ": ", d$country)
  col_order <- if (length(slugs) == 1) unname(countries) else
    as.vector(t(outer(unname(labels[slugs]), unname(countries), paste, sep = ": ")))

  wide <- tidyr::pivot_wider(d[, c("year", "column", "share_pct")],
                             names_from = "column", values_from = "share_pct")
  wide <- as.data.frame(wide)[order(wide$year), c("year", intersect(col_order, names(wide)))]
  wide[-1] <- lapply(wide[-1], round, digits = digits)
  rownames(wide) <- NULL
  wide
}

## Comtrade bilateral (chart 3) ----------------------------------------------

jgd_http_get <- function(url, headers = character(), retries = 3L, backoff_seconds = 5) {
  if (!requireNamespace("curl", quietly = TRUE)) stop("Package 'curl' is required.")
  last_error <- "unknown error"
  for (attempt in seq_len(retries)) {
    result <- tryCatch({
      h <- curl::new_handle()
      curl::handle_setheaders(h, .list = as.list(c("User-Agent" = "OSI-pipeline/1.0", headers)))
      curl::handle_setopt(h, timeout = 120, connecttimeout = 60, followlocation = TRUE)
      r <- curl::curl_fetch_memory(url, handle = h)
      if (r$status_code >= 400L) stop("HTTP ", r$status_code)
      rawToChar(r$content)
    }, error = function(e) {
      last_error <<- conditionMessage(e)
      NULL
    })
    if (!is.null(result)) return(result)
    if (attempt < retries) Sys.sleep(backoff_seconds * attempt)
  }
  stop("Comtrade request failed after ", retries, " attempts: ", last_error, "\n  ", url)
}

jgd_pull_batch <- function(codes, year, flow, world_only, key = "") {
  base <- if (nzchar(key)) JGD_KEYED_API else JGD_PREVIEW_API
  url <- sprintf(
    "%s?reporterCode=%d&period=%d&cmdCode=%s&flowCode=%s&partner2Code=0&customsCode=C00&motCode=0%s",
    base, JGD_REPORTER_CODE, year, paste(codes, collapse = ","), flow,
    if (world_only) "&partnerCode=0" else ""
  )
  headers <- if (nzchar(key)) c("Ocp-Apim-Subscription-Key" = key) else character()
  parsed <- jsonlite::fromJSON(jgd_http_get(url, headers), simplifyVector = TRUE)
  if (!is.null(parsed$error) && nzchar(parsed$error)) stop("Comtrade error: ", parsed$error)

  data <- parsed$data
  if (!nzchar(key) && NROW(data) >= JGD_ROW_LIMIT) {
    stop("Preview call hit its ", JGD_ROW_LIMIT, "-row cap for ", paste(codes, collapse = ","), ".")
  }
  if (NROW(data) == 0) {
    return(data.frame(flow = character(), cmd_code = character(), partner_code = integer(),
                      value = numeric()))
  }
  data.frame(flow = flow, cmd_code = as.character(data$cmdCode),
             partner_code = as.integer(data$partnerCode), value = as.numeric(data$primaryValue),
             stringsAsFactors = FALSE)
}

# Japan's imports by partner and exports to World, every HS2022 component code.
jgd_bilateral <- function(repo_root, year, codes, refresh = FALSE) {
  cache_dir <- file.path(repo_root, config$processed_dir, "charts", "cache")
  cache <- file.path(cache_dir, sprintf("comtrade_jpn_grid_bilateral_%d.csv", year))
  if (file.exists(cache) && !refresh) {
    out <- utils::read.csv(cache, colClasses = c(cmd_code = "character"), stringsAsFactors = FALSE)
    if (all(codes %in% out$cmd_code[out$flow == "M"])) {
      message("Using cached Comtrade pull: ", cache)
      return(out)
    }
  }

  key <- Sys.getenv("COMTRADE_API_KEY", "")
  message("Pulling Japan ", year, " grid trade from Comtrade (",
          if (nzchar(key)) "keyed" else "preview", " endpoint) ...")
  batches <- split(codes, ceiling(seq_along(codes) / JGD_BATCH))
  parts <- list()
  for (i in seq_along(batches)) {
    if (!nzchar(key) && i > 1) Sys.sleep(2)
    parts[[length(parts) + 1]] <- jgd_pull_batch(batches[[i]], year, "M", FALSE, key)
  }
  if (!nzchar(key)) Sys.sleep(2)
  parts[[length(parts) + 1]] <- jgd_pull_batch(codes, year, "X", TRUE, key)
  out <- do.call(rbind, parts)

  ref <- jsonlite::fromJSON(jgd_http_get(JGD_PARTNERS_REF), simplifyVector = TRUE)$results
  names_tbl <- data.frame(partner_code = as.integer(ref$PartnerCode), partner = ref$PartnerDesc,
                          stringsAsFactors = FALSE)
  out <- merge(out, names_tbl, by = "partner_code", all.x = TRUE)
  out$partner[is.na(out$partner)] <- paste0("Partner ", out$partner_code[is.na(out$partner)])
  out <- out[order(out$flow, out$cmd_code, -out$value),
             c("flow", "cmd_code", "partner_code", "partner", "value")]

  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  utils::write.csv(out, cache, row.names = FALSE)
  message("Cached ", nrow(out), " rows to ", cache)
  out
}

# One row per component: supplier HHI of Japan's imports, largest supplier, China's
# share, import and export value. Shares are against the reported World total.
jgd_import_concentration <- function(bilateral, code_map, china = JGD_CHINA_CODE) {
  rows <- lapply(unique(code_map$slug), function(s) {
    codes <- code_map$hs6[code_map$slug == s]
    m <- bilateral[bilateral$flow == "M" & bilateral$cmd_code %in% codes, , drop = FALSE]
    x <- bilateral[bilateral$flow == "X" & bilateral$cmd_code %in% codes &
                     bilateral$partner_code == 0L, , drop = FALSE]
    world_m <- sum(m$value[m$partner_code == 0L])
    if (world_m <= 0) stop("No Japan imports reported for component '", s, "'.")

    partners <- m[m$partner_code != 0L, , drop = FALSE]
    by_partner <- stats::aggregate(value ~ partner_code + partner, data = partners, FUN = sum)
    by_partner <- by_partner[order(-by_partner$value), , drop = FALSE]

    data.frame(
      slug = s,
      supplier_hhi = jgd_hhi(by_partner$value),
      top_supplier = by_partner$partner[1],
      top_supplier_share = 100 * by_partner$value[1] / world_m,
      china_share = 100 * sum(by_partner$value[by_partner$partner_code == china]) / world_m,
      imports = world_m,
      exports = sum(x$value),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

jgd_concentration_table <- function(conc, global_hhi, components = JGD_COMPONENTS) {
  d <- merge(components[, c("slug", "label")], conc, by = "slug")
  d <- merge(d, global_hhi, by = "slug", all.x = TRUE)
  d <- d[order(-d$supplier_hhi), , drop = FALSE]
  out <- data.frame(
    Component = d$label,
    `Supplier HHI (Japan imports)` = round(d$supplier_hhi),
    `Largest supplier` = d$top_supplier,
    `Largest supplier share (%)` = round(d$top_supplier_share, 1),
    `China share (%)` = round(d$china_share, 1),
    `Global exporter HHI` = round(d$global_hhi),
    `Japan imports (USD m)` = round(d$imports / 1e6, 1),
    `Japan exports (USD m)` = round(d$exports / 1e6, 1),
    `Trade balance (USD m)` = round((d$exports - d$imports) / 1e6, 1),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

## Partners (chart 4) --------------------------------------------------------

# Japan x Electric Grid x Midstream rows of the three dyad index tables, cached. The
# full RDS is ~185MB in memory as several GB; each table is subset and dropped in turn.
jgd_psi_dyads <- function(repo_root, refresh = FALSE, tech = "Electric Grid",
                          stage = "Midstream") {
  cache_dir <- file.path(repo_root, config$processed_dir, "charts", "cache")
  cache <- file.path(cache_dir, "psi_jpn_grid_midstream.rds")
  if (file.exists(cache) && !refresh) {
    message("Using cached PSI dyads: ", cache)
    return(readRDS(cache))
  }

  path <- file.path(repo_root, config$processed_dir, "outputs", "partnership_index_outputs.rds")
  if (!file.exists(path)) stop("Partnership index outputs not found: ", path,
                               "\nRun scripts/25_build_partner_indices.R first.")
  message("Reading partnership index outputs (large) ...")
  x <- readRDS(path)
  tables <- c(friendshore = "partner_friendshore_variable_tbl",
              opportunity = "partner_opportunity_variable_tbl",
              development = "partner_development_variable_tbl")
  x <- x[unname(tables)]
  invisible(gc())

  out <- list()
  for (k in names(tables)) {
    y <- x[[tables[[k]]]]
    if (is.null(y)) stop("Partnership outputs have no '", tables[[k]], "' table.")
    idx <- which(y$reporter_iso == JGD_FOCUS_ISO & y$tech == tech & y$supply_chain == stage)
    out[[k]] <- as.data.frame(lapply(y[c("partner", "partner_iso", "variable", "value", "Year")],
                                     function(col) col[idx]), stringsAsFactors = FALSE)
    x[[tables[[k]]]] <- NULL
    rm(y)
    invisible(gc())
  }

  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  saveRDS(out, cache)
  out
}

# Dyad partnership score per partner. Friendshore rows arrive duplicated and
# development rows once per sub-sector, so each index is averaged per partner first.
jgd_partner_scores <- function(dyads, weights) {
  pick <- function(tbl, variable, name) {
    d <- tbl[tbl$variable == variable & !is.na(tbl$value), , drop = FALSE]
    if (nrow(d) == 0) return(data.frame(partner_iso = character(), partner = character()))
    a <- stats::aggregate(value ~ partner_iso + partner, data = d, FUN = mean)
    names(a)[3] <- name
    a
  }
  f <- pick(dyads$friendshore, "Friendshore Index", "friendshore")
  o <- pick(dyads$opportunity, "Opportunity Index", "opportunity")
  d <- pick(dyads$development, "Development Potential Index", "development")

  out <- merge(f, o, by = c("partner_iso", "partner"))
  out <- merge(out, d, by = c("partner_iso", "partner"), all.x = TRUE)

  w <- unlist(weights[c("friendshore", "opportunity", "development")])
  if (any(is.na(w))) stop("PSI weights need friendshore, opportunity and development.")
  has_dev <- !is.na(out$development)
  denom <- w[["friendshore"]] + w[["opportunity"]] + ifelse(has_dev, w[["development"]], 0)

  out$friendshore_w <- w[["friendshore"]] * out$friendshore / denom
  out$opportunity_w <- w[["opportunity"]] * out$opportunity / denom
  out$development_w <- ifelse(has_dev, w[["development"]] * out$development / denom, 0)
  out$score <- out$friendshore_w + out$opportunity_w + out$development_w
  out[order(-out$score), , drop = FALSE]
}

# Keep partners whose grid-equipment imports clear `min_bn`. The partnership indices
# are not scaled by market size, so unscreened the top of the list includes small
# island economies; a grid manufacturing partner has to be a market first.
jgd_screen_partners <- function(scores, market, min_bn) {
  out <- merge(scores, market, by.x = "partner_iso", by.y = "reporter_iso")
  dropped <- nrow(scores) - sum(out$grid_imports >= min_bn * 1e9)
  out <- out[out$grid_imports >= min_bn * 1e9, , drop = FALSE]
  message(sprintf("  Market screen: %d of %d partners import >= $%.1fbn of grid equipment",
                  nrow(out), nrow(scores), min_bn))
  attr(out, "dropped") <- dropped
  out[order(-out$score), , drop = FALSE]
}

jgd_partner_table <- function(scores, top_n) {
  d <- utils::head(scores, top_n)
  out <- data.frame(
    Partner = d$partner,
    `Friendshore (weighted)` = round(d$friendshore_w, 3),
    `Opportunity (weighted)` = round(d$opportunity_w, 3),
    `Development (weighted)` = round(d$development_w, 3),
    `Partnership score` = round(d$score, 3),
    Friendshore = round(d$friendshore, 3),
    Opportunity = round(d$opportunity, 3),
    Development = round(d$development, 3),
    `Grid equipment imports (USD bn)` = round(d$grid_imports / 1e9, 1),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

## Runner --------------------------------------------------------------------

run_japan_grid_datawrapper <- function(repo_root, trend_from = JGD_DEFAULT_TREND_FROM,
                                       top_partners = JGD_DEFAULT_TOP_PARTNERS,
                                       min_market_bn = JGD_DEFAULT_MIN_MARKET_BN,
                                       refresh = FALSE, write_output = TRUE) {
  out_dir <- ts_out_dir(repo_root)
  map22 <- jgd_code_map(JGD_COMPONENTS, "hs")
  map92 <- jgd_code_map(JGD_COMPONENTS, "hs92")

  # Chart 1 ------------------------------------------------------------------
  message("Chart 1: RCA by component")
  ct <- ts_comtrade_energy(repo_root)
  ct <- ct[ct$flow == "Exports", , drop = FALSE]
  ct_year <- unique(ct$year)
  if (length(ct_year) != 1) stop("Expected one year in the Comtrade pull; got ", paste(ct_year, collapse = ", "))

  tot_path <- file.path(repo_root, config$raw_data_dir, "comtrade_total_export.csv")
  tot <- utils::read.csv(tot_path, stringsAsFactors = FALSE)
  tot <- tot[tot$flow_code == "X" & tot$ref_year == ct_year, , drop = FALSE]
  ct_totals <- stats::aggregate(primary_value ~ reporter_iso, data = tot, FUN = sum)
  names(ct_totals)[2] <- "total"

  covered <- jgd_covered(JGD_COMPONENTS, unique(ct$hs6))
  ct_slugs <- JGD_COMPONENTS$slug[covered]
  atlas_slugs <- JGD_COMPONENTS$slug[!covered]

  atlas <- jgd_atlas(repo_root, unique(map92$hs6), trend_from)
  atlas_year <- atlas$latest_year

  rca_ct <- jgd_rca(ct[, c("reporter_iso", "hs6", "value")], ct_totals,
                    map22[map22$slug %in% ct_slugs, ])
  rca_ct$year <- ct_year
  rca_ct$source_label <- "UN Comtrade"
  rca_at <- jgd_rca(atlas$exports[atlas$exports$year == atlas_year, c("reporter_iso", "hs6", "value")],
                    atlas$totals[atlas$totals$year == atlas_year, c("reporter_iso", "total")],
                    map92[map92$slug %in% atlas_slugs, ])
  rca_at$year <- atlas_year
  rca_at$source_label <- "UN Comtrade via Atlas (HS92)"
  rca <- rbind(rca_ct, rca_at)

  hs_used <- c(
    stats::setNames(gsub("|", " ", JGD_COMPONENTS$hs[covered], fixed = TRUE), ct_slugs),
    stats::setNames(gsub("|", " ", JGD_COMPONENTS$hs92[!covered], fixed = TRUE), atlas_slugs)
  )
  focus <- rca[rca$reporter_iso == JGD_FOCUS_ISO, , drop = FALSE]
  if (nrow(focus) != nrow(JGD_COMPONENTS)) stop("Japan is missing from the RCA data for some components.")
  focus$hs_used <- unname(hs_used[focus$slug])
  chart1 <- jgd_specialisation_table(focus)
  print(chart1[, 1:4], row.names = FALSE)

  # Chart 2 ------------------------------------------------------------------
  message("Chart 2: world export share over time")
  strategic <- JGD_COMPONENTS$slug[JGD_COMPONENTS$strategic]
  labels <- stats::setNames(JGD_COMPONENTS$label, JGD_COMPONENTS$slug)
  trend <- jgd_trend(atlas$exports, map92[map92$slug %in% strategic, ])
  jp_first <- trend[trend$reporter_iso == JGD_FOCUS_ISO & trend$year == min(trend$year), ]
  jp_last <- trend[trend$reporter_iso == JGD_FOCUS_ISO & trend$year == max(trend$year), ]
  for (s in strategic) {
    message(sprintf("  Japan, %-45s %5.1f%% -> %5.1f%%", labels[[s]],
                    jp_first$share_pct[jp_first$slug == s], jp_last$share_pct[jp_last$slug == s]))
  }

  # Chart 3 ------------------------------------------------------------------
  message("Chart 3: supplier concentration of Japan's imports")
  bilateral <- jgd_bilateral(repo_root, ct_year, unique(map22$hs6), refresh)
  conc <- jgd_import_concentration(bilateral, map22)
  chart3 <- jgd_concentration_table(conc, jgd_global_hhi(rca))
  print(chart3[, 1:5], row.names = FALSE)

  # Chart 4 ------------------------------------------------------------------
  message("Chart 4: partner markets")
  weights <- yaml::read_yaml(file.path(repo_root, "config", "weights.yml"))$psi
  dyads <- jgd_psi_dyads(repo_root, refresh)
  psi_year <- max(dyads$friendshore$Year, na.rm = TRUE)
  ct_all <- ts_comtrade_energy(repo_root)
  grid_m <- ct_all[ct_all$flow == "Imports" & ct_all$hs6 %in% map22$hs6, , drop = FALSE]
  market <- stats::aggregate(value ~ reporter_iso, data = grid_m, FUN = sum)
  names(market)[2] <- "grid_imports"
  scores <- jgd_screen_partners(jgd_partner_scores(dyads, weights), market, min_market_bn)
  chart4 <- jgd_partner_table(scores, top_partners)
  print(chart4[, c(1, 5)], row.names = FALSE)

  if (!write_output) return(invisible(list(chart1 = chart1, trend = trend, chart3 = chart3,
                                           chart4 = chart4)))

  ts_write(chart1, out_dir, sprintf("japan_grid_1_specialisation_%d", ct_year))
  stem2 <- sprintf("japan_grid_2_trend_%d_%d", min(trend$year), max(trend$year))
  ts_write(jgd_trend_wide(trend, strategic, labels), out_dir, paste0(stem2, "_wide"))
  for (s in strategic) ts_write(jgd_trend_wide(trend, s, labels), out_dir, paste0(stem2, "_", s))
  ts_write(chart3, out_dir, sprintf("japan_grid_3_import_concentration_%d", ct_year))
  ts_write(chart4, out_dir, sprintf("japan_grid_4_partners_%d", psi_year))
  invisible(list(chart1 = chart1, trend = trend, chart3 = chart3, chart4 = chart4))
}

opsi_japan_grid_dw_run_directly <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  length(file_arg) > 0 &&
    identical(basename(sub("^--file=", "", file_arg[1])), "japan_grid_datawrapper.R")
}

if (opsi_japan_grid_dw_run_directly()) {
  invisible(run_japan_grid_datawrapper(
    repo_root = repo_root,
    trend_from = as.integer(Sys.getenv("JGD_TREND_FROM", as.character(JGD_DEFAULT_TREND_FROM))),
    top_partners = as.integer(Sys.getenv("JGD_TOP_PARTNERS", as.character(JGD_DEFAULT_TOP_PARTNERS))),
    min_market_bn = as.numeric(Sys.getenv("JGD_MIN_MARKET_BN", as.character(JGD_DEFAULT_MIN_MARKET_BN))),
    refresh = tolower(Sys.getenv("JGD_REFRESH", "false")) %in% c("1", "true", "yes")
  ))
}
