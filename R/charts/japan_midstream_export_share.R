# Japan's share of world exports in four cleantech midstreams since 2000, against
# China, the US, South Korea and Germany.
#
# Four panels - Nuclear, Electric Grid, Batteries and Solar midstream - each with five
# lines: the country's exports of that midstream as a share of world exports of it.
#
# What "midstream" means
# ----------------------
# The repo's energy-tech crosswalk (hs6_categories_with_essential.csv), every code
# tagged `Value Chain == "Midstream"` for the technology, with three exclusions:
#   * Chapter 44 softwood logs. The crosswalk files them under grid and solar cabling
#     (wooden poles); left in, New Zealand pine is the largest "grid" exporter.
#   * Chapters 25 and 26 ores and concentrates (except 2620 metal-bearing residues,
#     which are recycling feedstock). The crosswalk tags battery-mineral ores as
#     midstream; left in, copper ore alone is most of the battery series and Chile and
#     Peru lead it. Ores are mining, not midstream processing.
#   * Nothing else. Generic headings the crosswalk includes stay in - polyolefin resin
#     and film, copper and aluminium foil - so the battery series is
#     "battery-attributed trade", broader than battery parts. (The 3824.99 chemical-
#     preparations catch-all and 2853 would stay too, but the Atlas publishes neither
#     heading, so they drop out; see the run log.)
# A code can belong to several technologies (a static converter is grid and solar),
# so the four panels overlap and must not be summed.
#
# Source
# ------
# Atlas of Economic Complexity HS92 panel (hs92_country_product_year_6.csv): Harvard
# Growth Lab's cleaned, mirror-reconciled UN Comtrade data, 1995-2023. It is the only
# Comtrade series in the repo that reaches back to 2000 (the bulk Comtrade pulls start
# in 2020), and being HS92 throughout it has no classification breaks inside the run.
# World = every exporter in the panel.
#
# The cost of HS92 is that codes created by later revisions do not exist. The crosswalk
# codes that matter are mapped to the HS92 heading that contained them (JMS_HS92_MAP),
# notably solar cells and modules (8541.42/43) to 8541.40, which also holds LEDs and
# photodiodes, so the solar series is a proxy whose non-PV share is largest in the
# early years. Codes with no clean HS92 parent are dropped and counted in the run log
# and in the long CSV.
#
# Outputs (data/processed/charts/):
#   japan_midstream_export_share_<start>_<end>_wide.csv
#       year x (sector: country). One file for a Datawrapper small-multiples chart, or
#       select one sector's five columns for a single line chart.
#   japan_midstream_export_share_<start>_<end>_wide_<sector>.csv
#       year x country, one per sector - the simplest input for a Datawrapper line chart.
#   japan_midstream_export_share_<start>_<end>.csv
#       long panel with exports, world exports and share.
# Shares are percent of world exports.
#
# Run:
#   Rscript R/charts/japan_midstream_export_share.R
#
# Environment:
#   JMS_START_YEAR   first year (default 2000)

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

JMS_DEFAULT_START_YEAR <- 2000L

# Crosswalk technology -> panel label and file slug, in panel order.
JMS_SECTORS <- tibble::tribble(
  ~tech,            ~label,                 ~slug,
  "Nuclear",        "Nuclear midstream",    "nuclear",
  "Electric Grid",  "Grid midstream",       "grid",
  "Batteries",      "Batteries midstream",  "batteries",
  "Solar",          "Solar midstream",      "solar"
)

# Focus first; the order is the column order in every wide file.
JMS_COUNTRIES <- c(JPN = "Japan", CHN = "China", USA = "United States",
                   KOR = "South Korea", DEU = "Germany")

# Later-HS crosswalk codes -> the HS92 heading that contained them. Only codes whose
# HS92 parent is mostly the same goods are mapped; the rest are dropped and counted.
JMS_HS92_MAP <- c(
  "854142" = "854140", "854143" = "854140",   # PV cells, modules -> photosensitive devices
  "850760" = "850780",                        # Li-ion -> other accumulators
  "722511" = "722510", "722519" = "722510",   # electrical steel, >=600mm
  "722611" = "722610", "722619" = "722610",   # electrical steel, <600mm
  "840681" = "840619", "840682" = "840619",   # steam turbines -> other steam turbines
  "810520" = "810510", "810530" = "810510",   # cobalt unwrought / waste
  "711291" = "711210", "711292" = "711220",   # gold, platinum waste and scrap
  "711230" = "711290", "711299" = "711290",   # precious-metal ash, other scrap
  "262099" = "262090",                        # other metal-bearing residues
  "854442" = "854441"                         # low-voltage conductors with connectors
)

## Commodity universe --------------------------------------------------------

# TRUE for codes kept in the midstream universe; see the header for why.
jms_keep_code <- function(hs6) {
  ch <- substr(hs6, 1, 2)
  !(ch == "44" | (ch %in% c("25", "26") & substr(hs6, 1, 4) != "2620"))
}

# Crosswalk midstream codes for one technology, translated to HS92.
#
# Returns the crosswalk code, the HS92 code it is read as, and whether that HS92 code
# exists in the Atlas. Translation can collapse several codes onto one HS92 heading
# (8541.42 and 8541.43 both become 8541.40); the caller dedupes on `hs92` so the
# heading is counted once.
jms_universe <- function(codes, atlas_codes) {
  codes <- codes[codes$value_chain == "Midstream" & jms_keep_code(codes$hs6), , drop = FALSE]
  mapped <- unname(JMS_HS92_MAP[codes$hs6])
  codes$hs92 <- ifelse(codes$hs6 %in% atlas_codes, codes$hs6, mapped)
  codes$in_atlas <- !is.na(codes$hs92) & codes$hs92 %in% atlas_codes
  codes
}

## Atlas ---------------------------------------------------------------------

# Every exporter's exports of the requested codes from `start_year`, read in chunks
# so the ~900MB panel never sits in memory whole. Also returns the set of HS92 codes
# the panel carries, which the universe needs to know what translates.
jms_atlas <- function(repo_root, start_year) {
  path <- file.path(repo_root, config$raw_data_dir, "hs92_country_product_year_6.csv")
  if (!file.exists(path)) stop("Atlas HS6 panel not found: ", path)

  message("Reading the Atlas HS92 panel from ", start_year, " (large file, chunked) ...")
  readr::read_csv_chunked(
    path,
    readr::DataFrameCallback$new(function(chunk, pos) {
      chunk[chunk$year >= start_year & !is.na(chunk$export_value), , drop = FALSE]
    }),
    col_types = readr::cols_only(
      country_iso3_code = readr::col_character(), product_hs92_code = readr::col_character(),
      year = readr::col_integer(), export_value = readr::col_double()
    ),
    chunk_size = 2e6, progress = FALSE
  )
}

## Shares --------------------------------------------------------------------

# Long panel: one row per sector x country x year. `atlas` has reporter_iso, hs6,
# year, value. Countries with no exports in a year get 0 rather than a gap.
jms_shares <- function(atlas, universes, countries = JMS_COUNTRIES) {
  out <- lapply(names(universes), function(slug) {
    codes <- unique(universes[[slug]]$hs92[universes[[slug]]$in_atlas])
    d <- atlas[atlas$hs6 %in% codes, , drop = FALSE]
    if (nrow(d) == 0) stop("No Atlas exports for sector '", slug, "'.")

    world <- stats::aggregate(value ~ year, data = d, FUN = sum)
    names(world)[2] <- "world_exports"

    focus <- d[d$reporter_iso %in% names(countries), , drop = FALSE]
    by_country <- if (nrow(focus) > 0) {
      stats::aggregate(value ~ reporter_iso + year, data = focus, FUN = sum)
    } else {
      data.frame(reporter_iso = character(), year = integer(), value = numeric())
    }

    grid <- expand.grid(reporter_iso = names(countries), year = world$year,
                        stringsAsFactors = FALSE)
    s <- merge(grid, by_country, by = c("reporter_iso", "year"), all.x = TRUE)
    s$value[is.na(s$value)] <- 0
    s <- merge(s, world, by = "year")
    s$share_pct <- 100 * s$value / s$world_exports
    s$sector <- slug
    s$country <- unname(countries[s$reporter_iso])
    s$codes_used <- length(codes)
    s
  })
  out <- do.call(rbind, out)
  out$sector <- factor(out$sector, levels = names(universes))
  out$country <- factor(out$country, levels = unname(countries))
  out <- out[order(out$sector, out$country, out$year), , drop = FALSE]
  out$sector <- as.character(out$sector)
  out$country <- as.character(out$country)
  rownames(out) <- NULL
  out[, c("sector", "country", "reporter_iso", "year", "value", "world_exports",
          "share_pct", "codes_used")]
}

# Year x country for the given sectors. With more than one sector, headers are
# "<sector label>: <country>" so a combined file still reads in a Datawrapper legend.
jms_wide <- function(shares, sectors = unique(shares$sector), labels = NULL, digits = 2L) {
  d <- shares[shares$sector %in% sectors, , drop = FALSE]
  if (length(sectors) > 1) {
    lab <- if (is.null(labels)) stats::setNames(sectors, sectors) else labels
    d$column <- paste0(unname(lab[d$sector]), ": ", d$country)
  } else {
    d$column <- d$country
  }
  order_cols <- unique(d$column)

  wide <- tidyr::pivot_wider(d[, c("year", "column", "share_pct")],
                             names_from = "column", values_from = "share_pct")
  wide <- as.data.frame(wide)[order(wide$year), c("year", order_cols)]
  wide[-1] <- lapply(wide[-1], round, digits = digits)
  rownames(wide) <- NULL
  wide
}

## Runner --------------------------------------------------------------------

run_japan_midstream_export_share <- function(repo_root, start_year = JMS_DEFAULT_START_YEAR,
                                             write_output = TRUE) {
  atlas <- jms_atlas(repo_root, start_year)
  atlas$hs6 <- ts_hs6(atlas$product_hs92_code)
  atlas <- data.frame(reporter_iso = atlas$country_iso3_code, hs6 = atlas$hs6,
                      year = atlas$year, value = atlas$export_value, stringsAsFactors = FALSE)
  atlas_codes <- unique(atlas$hs6)

  universes <- list()
  for (i in seq_len(nrow(JMS_SECTORS))) {
    u <- jms_universe(ts_tech_codes(repo_root, JMS_SECTORS$tech[i]), atlas_codes)
    dropped <- u$hs6[!u$in_atlas]
    message(sprintf(
      "  %-20s %d midstream codes, %d read as %d HS92 headings%s",
      JMS_SECTORS$label[i], nrow(u), sum(u$in_atlas), length(unique(u$hs92[u$in_atlas])),
      if (length(dropped) > 0) paste0("; dropped (no HS92 parent): ", paste(dropped, collapse = " ")) else ""
    ))
    universes[[JMS_SECTORS$slug[i]]] <- u
  }

  shares <- jms_shares(atlas, universes)
  last <- shares[shares$year == max(shares$year) & shares$reporter_iso == "JPN", ]
  first <- shares[shares$year == min(shares$year) & shares$reporter_iso == "JPN", ]
  for (s in JMS_SECTORS$slug) {
    message(sprintf("  Japan, %-10s %4.1f%% (%d) -> %4.1f%% (%d)", s,
                    first$share_pct[first$sector == s], min(shares$year),
                    last$share_pct[last$sector == s], max(shares$year)))
  }
  if (!write_output) return(invisible(shares))

  out_dir <- ts_out_dir(repo_root)
  stem <- sprintf("japan_midstream_export_share_%d_%d", min(shares$year), max(shares$year))
  labels <- stats::setNames(JMS_SECTORS$label, JMS_SECTORS$slug)

  ts_write(shares, out_dir, stem)
  ts_write(jms_wide(shares, JMS_SECTORS$slug, labels), out_dir, paste0(stem, "_wide"))
  for (s in JMS_SECTORS$slug) {
    ts_write(jms_wide(shares, s), out_dir, paste0(stem, "_wide_", s))
  }
  invisible(shares)
}

opsi_japan_midstream_share_run_directly <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  length(file_arg) > 0 &&
    identical(basename(sub("^--file=", "", file_arg[1])), "japan_midstream_export_share.R")
}

if (opsi_japan_midstream_share_run_directly()) {
  invisible(run_japan_midstream_export_share(
    repo_root = repo_root,
    start_year = as.integer(Sys.getenv("JMS_START_YEAR", as.character(JMS_DEFAULT_START_YEAR)))
  ))
}
