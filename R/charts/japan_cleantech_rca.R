# Japan's export specialisation across cleantech subcomponents.
#
# Evidence for one claim: that Japan's industrial strengths in critical minerals and
# advanced materials, nuclear and grids sit in specific subcomponents - refined metals,
# magnets, battery chemicals, reactor hardware, forgings, transformers, switchgear,
# power electronics, cables - rather than across the board. The chart is one bar per
# subcomponent showing Japan's revealed comparative advantage (RCA), grouped into the
# three sectors, with four peers alongside for context.
#
# RCA is the Balassa index, defined exactly as the index pipeline defines it
# (`trade_core_build_comtrade_rca()` in R/categories/trade/trade_core.R):
#
#   RCA = (Japan's exports of the subcomponent / Japan's total exports)
#       / (world exports of the subcomponent / world total exports)
#
# Above 1, the subcomponent is a larger share of Japan's exports than of world trade;
# that is specialisation, not market share. A country can have an RCA of 3 in something
# it exports little of in absolute terms, so Japan's world export share is carried as a
# tooltip column to keep the two apart. "World" is the reporters present in both the
# subcomponent and the total-export data, so the two halves of the ratio cover the
# same countries.
#
# Subcomponents and their HS6 codes
# ---------------------------------
# Rows are built from the repo's energy-tech crosswalk (hs6_categories_with_essential.csv)
# but curated rather than taken sub-sector by sub-sector, because the paragraph names
# processing stages and the crosswalk sub-sectors mix them:
#   * Mining is dropped from the refining and cathode rows (chapter 26 ores are not
#     "refining and processing know-how").
#   * Separators use the polyolefin film headings (3920.10/3920.20) that separator film
#     is classified under, not the crosswalk's raw resins. Both headings also cover
#     film for every other use, so this is a proxy.
#   * Electrolytes keep 3824.99, the catch-all for chemical preparations that prepared
#     electrolyte solutions fall under; it is far broader than electrolytes.
#   * Forgings and castings are not in the crosswalk. 7326.19 (forged or stamped steel
#     articles) and 7325.99 (other cast steel articles) are added as an all-uses proxy -
#     there is no HS code for nuclear-grade forgings, which ship as these headings or as
#     reactor and boiler parts.
#   * The steam plant (8402, 8404, 8406) is kept and labelled as shared with fossil
#     generation, so the nuclear group is not read as nuclear-only trade.
# Quality control, supply-security institutions, fuel *services*, supplier networks and
# system integration are not traded goods and cannot be measured from trade data.
#
# Sources
# -------
# 1. UN Comtrade 2024, the pipeline's own energy-tech pull (comtrade_energy_trade.csv,
#    HS2022, partner World) with comtrade_total_export.csv for the denominators. This is
#    the default for every row.
# 2. The Atlas of Economic Complexity HS92 panel (hs92_country_product_year_6.csv), which
#    is Harvard Growth Lab's cleaned, mirror-reconciled UN Comtrade data, latest year.
#    Used only for rows whose codes the 2024 pull does not carry: permanent magnets,
#    rare earths, power semiconductors, grid automation, forgings and castings,
#    electrical steel (HS92 7225.10 does not split grain-oriented from non-oriented),
#    and the precious-metal and battery scrap headings of recycling. Totals there are the sum
#    over every HS6 product. Each row's `Source` and `Data year` columns say which
#    applies; RCA is a ratio within one source, so a row is never mixed across both.
#
# Output (data/processed/charts/):
#   japan_cleantech_rca_<year>_wide.csv
# Datawrapper-ready for one bar chart (or dot plot with the peers): `Subcomponent`,
# `Sector` (use as the bar group), `Japan`, then one column per peer, then
# tooltip-only columns. Rows are ordered by sector as in the paragraph, then by
# Japan's RCA, highest first.
#
# Run:
#   Rscript R/charts/japan_cleantech_rca.R
#
# Environment:
#   JCR_PEERS       comma-separated ISO3 peers (default CHN,DEU,KOR,USA)
#   JCR_ATLAS_YEAR  Atlas fallback year (default: latest in the file)

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

JCR_FOCUS_ISO <- "JPN"
JCR_DEFAULT_PEERS <- c("CHN", "DEU", "KOR", "USA")
JCR_SECTORS <- c("Critical minerals & materials", "Nuclear", "Grids")

# One row per bar. `hs` are the HS2022 codes looked up in the 2024 Comtrade pull;
# `hs92` are the Atlas codes used when any of `hs` is missing from that pull. A row
# with no `hs92` must be fully covered by the pull, and the run stops if it is not.
JCR_ROWS <- tibble::tribble(
  ~sector,                           ~label,                                      ~hs,                                                             ~hs92,
  "Critical minerals & materials",   "Refined nickel & cobalt",                   "282200|282540|283324|750210|750400|810520",                     "",
  "Critical minerals & materials",   "Lithium chemicals",                         "282520|283691",                                                 "",
  "Critical minerals & materials",   "Rare-earth metals & compounds",             "280530|284690",                                                 "280530|284690",
  "Critical minerals & materials",   "Permanent magnets",                         "850511|850519",                                                 "850511|850519",
  "Critical minerals & materials",   "Cathode & precursor chemicals",             "281700|282010|282090|282530|282690|283529|283699|284290|285390|750110", "",
  "Critical minerals & materials",   "Separator film (polyolefin)",               "392010|392020",                                                 "",
  "Critical minerals & materials",   "Electrolyte chemicals",                     "282739|292090|382499",                                          "",
  "Critical minerals & materials",   "Precious-metal & nickel scrap (recycling)", "711230|711291|711292|711299|750300",                           "711220|711290|750300",
  "Nuclear",                         "Reactors & reactor internals",              "840110|840140",                                                 "",
  "Nuclear",                         "Nuclear fuel & fuel-cycle materials",       "840130|284410|284420|284450",                                   "",
  "Nuclear",                         "Steel forgings & castings (all uses)",      "732619|732599",                                                 "732619|732599",
  "Nuclear",                         "Steam generators & turbines (shared)",      "840211|840212|840219|840220|840290|840410|840420|840490|840681|840682|840690", "",
  "Grids",                           "Transformers",                              "850421|850422|850423|850431|850432|850433|850434",              "",
  "Grids",                           "Electrical steel (silicon, flat-rolled)",   "722511|722519",                                                 "722510",
  "Grids",                           "HV switchgear & circuit breakers",          "853521|853529|853530|853540|853590",                            "",
  "Grids",                           "Power electronics (converters, power semis)", "850440|854129|854130",                                        "850440|854129|854130",
  "Grids",                           "HV cables",                                 "854460",                                                        "",
  "Grids",                           "Control panels & automation",               "853710|853720|903289",                                          "853710|853720|903289"
)

## Helpers -------------------------------------------------------------------

jcr_split <- function(x) {
  out <- strsplit(x, "|", fixed = TRUE)[[1]]
  out[nzchar(out)]
}

# Which source each row is computed from, and with which codes.
jcr_assign_sources <- function(rows, pull_codes) {
  out <- lapply(seq_len(nrow(rows)), function(i) {
    hs <- jcr_split(rows$hs[i])
    if (all(hs %in% pull_codes)) {
      return(data.frame(label = rows$label[i], source = "comtrade", hs6 = hs,
                        stringsAsFactors = FALSE))
    }
    hs92 <- jcr_split(rows$hs92[i])
    if (length(hs92) == 0) {
      stop("Row '", rows$label[i], "' needs code(s) the 2024 Comtrade pull lacks (",
           paste(setdiff(hs, pull_codes), collapse = ", "), ") and has no HS92 fallback.")
    }
    data.frame(label = rows$label[i], source = "atlas", hs6 = hs92, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# Balassa RCA for every reporter and row in one source.
#
# `exports` has reporter_iso, hs6, value; `totals` has reporter_iso, total. World is the
# reporters present in both, so numerator and denominator cover the same countries.
jcr_rca <- function(exports, totals, code_map) {
  reporters <- intersect(unique(exports$reporter_iso), totals$reporter_iso[totals$total > 0])
  exports <- exports[exports$reporter_iso %in% reporters, , drop = FALSE]
  totals <- totals[totals$reporter_iso %in% reporters, , drop = FALSE]

  joined <- merge(exports, code_map[, c("label", "hs6")], by = "hs6")
  by_row <- stats::aggregate(value ~ label + reporter_iso, data = joined, FUN = sum)

  # Every reporter gets every row, so a country that exports none of a subcomponent
  # shows RCA 0 rather than vanishing from the chart.
  grid <- expand.grid(label = unique(code_map$label), reporter_iso = reporters,
                      stringsAsFactors = FALSE)
  out <- merge(grid, by_row, by = c("label", "reporter_iso"), all.x = TRUE)
  out$value[is.na(out$value)] <- 0
  out <- merge(out, totals, by = "reporter_iso")

  world_row <- stats::aggregate(value ~ label, data = out, FUN = sum)
  names(world_row)[2] <- "world_value"
  out <- merge(out, world_row, by = "label")
  world_total <- sum(totals$total)

  out$world_share <- out$value / out$world_value
  out$rca <- (out$value / out$total) / (out$world_value / world_total)
  out$n_reporters <- length(reporters)
  out
}

## Sources -------------------------------------------------------------------

jcr_comtrade <- function(repo_root) {
  trade <- ts_comtrade_energy(repo_root)
  trade <- trade[trade$flow == "Exports", c("year", "reporter_iso", "hs6", "value")]

  path <- file.path(repo_root, config$raw_data_dir, "comtrade_total_export.csv")
  if (!file.exists(path)) stop("Comtrade total exports not found: ", path)
  tot <- utils::read.csv(path, stringsAsFactors = FALSE)
  tot <- tot[tot$flow_code == "X", , drop = FALSE]

  years <- unique(c(trade$year, tot$ref_year))
  if (length(years) != 1) {
    stop("Expected one year across the Comtrade pulls; got ", paste(sort(years), collapse = ", "), ".")
  }
  totals <- stats::aggregate(primary_value ~ reporter_iso, data = tot, FUN = sum)
  names(totals)[2] <- "total"
  list(year = years, exports = trade[, c("reporter_iso", "hs6", "value")], totals = totals)
}

# Atlas panel for one year: totals over every HS6, and the requested codes. Read in
# chunks filtered to the year, so the ~900MB file never sits in memory whole.
jcr_atlas <- function(repo_root, codes, year = NA_integer_) {
  path <- file.path(repo_root, config$raw_data_dir, "hs92_country_product_year_6.csv")
  if (!file.exists(path)) stop("Atlas HS6 panel not found: ", path)

  types <- readr::cols_only(
    country_iso3_code = readr::col_character(), product_hs92_code = readr::col_character(),
    year = readr::col_integer(), export_value = readr::col_double()
  )
  if (is.na(year)) {
    message("Finding the latest Atlas year ...")
    yrs <- readr::read_csv(path, col_types = readr::cols_only(year = readr::col_integer()),
                           progress = FALSE)
    year <- max(yrs$year, na.rm = TRUE)
  }

  message("Reading Atlas ", year, " (large file, chunked) ...")
  atlas <- readr::read_csv_chunked(
    path,
    readr::DataFrameCallback$new(function(chunk, pos) {
      chunk[chunk$year == year & chunk$export_value > 0, , drop = FALSE]
    }),
    col_types = types, chunk_size = 2e6, progress = FALSE
  )
  atlas$hs6 <- ts_hs6(atlas$product_hs92_code)
  names(atlas)[names(atlas) == "country_iso3_code"] <- "reporter_iso"

  totals <- stats::aggregate(export_value ~ reporter_iso, data = atlas, FUN = sum)
  names(totals)[2] <- "total"
  sel <- as.data.frame(atlas[atlas$hs6 %in% codes, c("reporter_iso", "hs6", "export_value")])
  names(sel)[3] <- "value"

  absent <- setdiff(codes, sel$hs6)
  if (length(absent) > 0) stop("Atlas ", year, " has no exports under: ", paste(absent, collapse = ", "))
  list(year = year, exports = sel, totals = totals)
}

## Panel ---------------------------------------------------------------------

# Long panel: one row per subcomponent x country (focus + peers).
jcr_panel <- function(rca, rows, countries, source_years) {
  keep <- rca[rca$reporter_iso %in% countries, , drop = FALSE]
  missing <- setdiff(countries, keep$reporter_iso)
  if (length(missing) > 0) stop("No trade data for: ", paste(missing, collapse = ", "), ".")

  keep <- merge(keep, rows[, c("sector", "label")], by = "label")
  keep$data_year <- unname(source_years[keep$source])
  keep
}

# Wide Datawrapper table: subcomponents down, countries across.
jcr_wide <- function(panel, rows, focus = JCR_FOCUS_ISO, peers = JCR_DEFAULT_PEERS,
                     country_names = NULL, digits = 2L) {
  isos <- c(focus, peers)
  if (is.null(country_names)) country_names <- stats::setNames(isos, isos)

  rca <- tidyr::pivot_wider(panel[, c("label", "reporter_iso", "rca")],
                            names_from = "reporter_iso", values_from = "rca")
  rca <- as.data.frame(rca)[, c("label", isos)]
  rca[isos] <- lapply(rca[isos], round, digits = digits)
  names(rca) <- c("label", unname(country_names[isos]))

  jp <- panel[panel$reporter_iso == focus, , drop = FALSE]
  tips <- data.frame(
    label = jp$label,
    world_share = round(100 * jp$world_share, 1),
    exports_bn = round(jp$value / 1e9, 2),
    source = ifelse(jp$source == "comtrade", "UN Comtrade", "UN Comtrade via Atlas (HS92)"),
    data_year = jp$data_year,
    hs = vapply(jp$label, function(l) {
      codes <- unique(panel$hs_codes[panel$label == l])
      if (length(codes) == 0) "" else codes[1]
    }, character(1)),
    stringsAsFactors = FALSE
  )

  out <- merge(rows[, c("sector", "label")], rca, by = "label")
  out <- merge(out, tips, by = "label")
  out$sector <- factor(out$sector, levels = JCR_SECTORS)
  out <- out[order(out$sector, -out[[unname(country_names[focus])]]), , drop = FALSE]
  out$sector <- as.character(out$sector)

  out <- out[, c("label", "sector", unname(country_names[isos]),
                 "world_share", "exports_bn", "source", "data_year", "hs")]
  names(out) <- c("Subcomponent", "Sector", unname(country_names[isos]),
                  paste0(unname(country_names[focus]), " share of world exports (%)"),
                  paste0(unname(country_names[focus]), " exports (USD bn)"),
                  "Source", "Data year", "HS6 codes")
  rownames(out) <- NULL
  out
}

## Runner --------------------------------------------------------------------

run_japan_cleantech_rca <- function(repo_root, peers = JCR_DEFAULT_PEERS,
                                    atlas_year = NA_integer_, write_output = TRUE) {
  message("Reading the 2024 Comtrade energy-tech pull ...")
  ct <- jcr_comtrade(repo_root)
  code_map <- jcr_assign_sources(JCR_ROWS, unique(ct$exports$hs6))

  parts <- list(jcr_rca(ct$exports, ct$totals, code_map[code_map$source == "comtrade", ]))
  parts[[1]]$source <- "comtrade"
  years <- c(comtrade = ct$year)

  atlas_map <- code_map[code_map$source == "atlas", , drop = FALSE]
  if (nrow(atlas_map) > 0) {
    at <- jcr_atlas(repo_root, unique(atlas_map$hs6), atlas_year)
    atlas_rca <- jcr_rca(at$exports, at$totals, atlas_map)
    atlas_rca$source <- "atlas"
    parts <- c(parts, list(atlas_rca))
    years["atlas"] <- at$year
  }

  rca <- do.call(rbind, parts)
  hs_codes <- tapply(code_map$hs6, code_map$label, paste, collapse = " ")
  rca$hs_codes <- unname(hs_codes[rca$label])

  isos <- c(JCR_FOCUS_ISO, peers)
  panel <- jcr_panel(rca, JCR_ROWS, isos, years)
  names_map <- stats::setNames(vapply(isos, ts_focus_name, character(1)), isos)
  names_map[names_map == "Korea, Republic of"] <- "South Korea"
  wide <- jcr_wide(panel, JCR_ROWS, JCR_FOCUS_ISO, peers, names_map)

  for (i in seq_len(nrow(wide))) {
    message(sprintf("  %-46s %-30s RCA %5.2f  (%s %s)", wide$Subcomponent[i], wide$Sector[i],
                    wide[[names_map[JCR_FOCUS_ISO]]][i], wide$Source[i], wide$`Data year`[i]))
  }
  if (!write_output) return(invisible(wide))

  out_dir <- ts_out_dir(repo_root)
  ts_write(wide, out_dir, sprintf("japan_cleantech_rca_%d_wide", ct$year))
  invisible(wide)
}

opsi_japan_cleantech_rca_run_directly <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  length(file_arg) > 0 &&
    identical(basename(sub("^--file=", "", file_arg[1])), "japan_cleantech_rca.R")
}

if (opsi_japan_cleantech_rca_run_directly()) {
  peers_env <- Sys.getenv("JCR_PEERS", paste(JCR_DEFAULT_PEERS, collapse = ","))
  invisible(run_japan_cleantech_rca(
    repo_root = repo_root,
    peers = trimws(strsplit(peers_env, ",", fixed = TRUE)[[1]]),
    atlas_year = as.integer(Sys.getenv("JCR_ATLAS_YEAR", NA_character_))
  ))
}
