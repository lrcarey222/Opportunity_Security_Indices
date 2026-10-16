# Japan's critical mineral and mineral-processing economic security.
#
# Five figures on one argument: Japan's position is structurally precarious - it mines
# nothing, refines a sliver, and buys its raw material from the world's most concentrated
# supplier markets - but it is improving on three fronts at once: the supplier markets
# Japan buys from are projected to deconcentrate, Japan's own refining output is projected
# to grow where it has a foothold, and Japan's policy intervention in the midstream has
# risen roughly tenfold since 2020.
#
#   fig 1  where_japan_sits        Japan's share of 2025 global mine and refinery supply,
#                                  against China's, for six minerals at two stages
#   fig 2  concentration_outlook   Top-3 supplier share 2025 -> 2040, and Japan's own
#                                  refined output over the same horizon (IEA base case)
#   fig 3  supplier_exposure       Import-bill-weighted supplier concentration of each
#                                  large importer's critical mineral basket
#   fig 4  value_added_ladder      Japan's critical mineral trade by processing stage:
#                                  ore in, refined metal out
#   fig 5  policy_momentum         Active domestic interventions in battery upstream and
#                                  midstream, Japan against three peers
#
# Every figure is written as both a PNG and the CSV behind it, so the numbers quoted in
# any write-up can be traced back to a row.
#
# Run:
#   Rscript R/charts/japan_critical_minerals_security.R
#
# Environment:
#   JCM_FOCUS_ISO     focal reporter ISO3 (default JPN) - the figures are built for Japan
#                     but nothing here is Japan-specific, so a peer can be swapped in
#   JCM_HORIZON_YEAR  end of the IEA projection horizon (default 2040)
#   JCM_POLICY_FROM   first announcement year on figure 5 (default 2015)
#   JCM_TOP_IMPORTERS how many importers to plot on figure 3 (default 20)
#   JCM_MIN_REPORTERS reporters a Comtrade vintage needs before it is used (default 50)

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

source(file.path(repo_root, "scripts", "utils", "raw_inputs.R"))
source(file.path(repo_root, "R", "utils", "iea_critical_minerals.R"))

## Constants -----------------------------------------------------------------

# The repo's house palette (scripts/20_build_indices.R).
JCM_PALETTE <- c(
  cyan = "#0BD0D9", blue = "#0989B1", navy = "#003A61",
  yellow = "#FFCA08", orange = "#F8931D", green = "#548538", grey = "#7F7F7F"
)

JCM_FOCUS_COLOUR <- unname(JCM_PALETTE["orange"])
JCM_CHINA_COLOUR <- unname(JCM_PALETTE["navy"])
JCM_OTHER_COLOUR <- unname(JCM_PALETTE["blue"])
JCM_ROW_COLOUR <- "#C9CFD4"

# The IEA supply sheet carries three summary rows alongside the country rows. They are
# aggregates, not producers, so they never enter a share denominator as a country.
JCM_IEA_SUMMARY_ROWS <- c("Total", "Top 3 share", "Total clean technologies")

# Peers for the policy figure. NIPO covers 22 reporters; these are the three that share
# Japan's problem - large battery manufacturing, no domestic mine base to speak of.
JCM_POLICY_PEERS <- c(JPN = "Japan", KOR = "Korea", DEU = "Germany", USA = "United States")

JCM_POLICY_COLOURS <- c(
  Japan = JCM_FOCUS_COLOUR,
  Korea = unname(JCM_PALETTE["blue"]),
  Germany = unname(JCM_PALETTE["green"]),
  `United States` = unname(JCM_PALETTE["grey"])
)

# Processing stage from the HS chapter of a critical mineral commodity code.
#
# This is a chapter-level rule, not a concordance: HS chapters separate mineral ores
# (25-26) from inorganic chemistry (28-29, 38-39) from base metals (71-81), which is
# exactly the raw / chemical / metal ladder figure 4 needs. It cannot resolve stage
# *within* a chapter - chapter 75 holds both nickel matte, an intermediate, and refined
# unwrought nickel - so figure 4 reads as a ladder, not as a capacity measure.
JCM_STAGE_LEVELS <- c(
  "Ores, concentrates & raw mineral",
  "Refined chemicals & compounds",
  "Refined metals & alloys"
)

jcm_processing_stage <- function(hs6) {
  chapter <- substr(hs6, 1, 2)
  out <- rep(NA_character_, length(chapter))
  out[chapter %in% c("25", "26")] <- JCM_STAGE_LEVELS[1]
  out[chapter %in% c("28", "29", "38", "39")] <- JCM_STAGE_LEVELS[2]
  out[chapter %in% c("71", "72", "73", "74", "75", "76", "78", "79", "80", "81")] <- JCM_STAGE_LEVELS[3]
  out
}

# Six-character HS code from whatever type the CSV reader inferred.
#
# read.csv types cmd_code as integer when every value in the pull is numeric, which
# strips a leading zero. Padding is done by hand rather than with formatC's "0" flag,
# which silently pads with spaces once its input is a character vector.
jcm_hs6 <- function(x) {
  s <- as.character(x)
  short <- !is.na(s) & nchar(s) < 6
  s[short] <- paste0(strrep("0", 6 - nchar(s[short])), s[short])
  s
}

## Inputs --------------------------------------------------------------------

# The IEA release is resolved through the manifest's versioned pattern rather than a
# pinned filename, so a new release flows in without a code edit. The pattern is copied
# from scripts/10_build_themes.R, which owns the contract.
jcm_iea_path <- function(raw_data_path) {
  resolve_versioned_raw_input(
    raw_data_path,
    pattern = "^IEA Critical Minerals Dataset \\d{4}\\.xlsx$|^iea_criticalminerals_\\d{2}\\.csv$",
    fallback = "IEA Critical Minerals Dataset 2026.xlsx",
    label = "IEA Critical Minerals Dataset"
  )
}

# Newest Comtrade vintage that is actually usable for a cross-country figure.
#
# The manifest resolves critmin_import_<year>.csv with `resolve: newest`, which is right
# for the index - it wants whatever is freshest - but wrong here. Comtrade publishes a
# year reporter by reporter over about eighteen months, so the newest staged file is a
# stub for most of its life: the 2025 pull in this tree carries three reporters. A
# cross-country concentration measure computed on three reporters is not wrong, it is
# meaningless, so this walks vintages newest-first and takes the first year that has
# both flows, enough reporters to compute world market shares, and the focal reporter
# in it. Whatever is skipped is named, so a stale-looking figure is always explained.
jcm_critmin_paths <- function(raw_data_path, focus_iso, min_reporters = 50L) {
  files <- list.files(raw_data_path, pattern = "^critmin_(import|export)_\\d{4}\\.csv$")
  if (length(files) == 0) {
    stop("No critmin_(import|export)_<year>.csv files in ", raw_data_path)
  }

  years <- as.integer(sub("^critmin_\\w+_(\\d{4})\\.csv$", "\\1", files))
  flows <- sub("^critmin_(\\w+)_\\d{4}\\.csv$", "\\1", files)
  paired <- intersect(years[flows == "import"], years[flows == "export"])
  if (length(paired) == 0) {
    stop("No year has both a critmin import and a critmin export file in ", raw_data_path)
  }

  skipped <- character()
  for (year in sort(paired, decreasing = TRUE)) {
    candidate <- list(
      import = file.path(raw_data_path, sprintf("critmin_import_%d.csv", year)),
      export = file.path(raw_data_path, sprintf("critmin_export_%d.csv", year)),
      year = year
    )

    reporters <- lapply(candidate[c("import", "export")], function(p) {
      unique(utils::read.csv(p, stringsAsFactors = FALSE)$reporter_iso)
    })
    n <- min(lengths(reporters))
    has_focus <- all(vapply(reporters, function(r) focus_iso %in% r, logical(1)))

    if (n >= min_reporters && has_focus) {
      if (length(skipped) > 0) {
        message(
          "Critical minerals trade: using ", year, "; skipped ",
          paste(skipped, collapse = "; "), "."
        )
      } else {
        message("Critical minerals trade: using ", year, ".")
      }
      return(candidate)
    }

    skipped <- c(skipped, sprintf(
      "%d (%d reporters%s)", year, n, if (has_focus) "" else paste0(", no ", focus_iso)
    ))
  }

  stop(
    "No critical minerals trade vintage has >= ", min_reporters, " reporters and ",
    focus_iso, ": ", paste(skipped, collapse = "; "), "."
  )
}

jcm_read_critmin_flow <- function(path, flow_label) {
  raw <- utils::read.csv(path, stringsAsFactors = FALSE)
  required <- c("ref_year", "reporter_iso", "cmd_code", "cmd_desc", "primary_value")
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0) {
    stop(basename(path), " is missing column(s): ", paste(missing, collapse = ", "))
  }

  out <- raw[, required]
  out$hs6 <- jcm_hs6(out$cmd_code)
  out$stage <- jcm_processing_stage(out$hs6)
  out$flow <- flow_label
  out$primary_value <- suppressWarnings(as.numeric(out$primary_value))
  out[!is.na(out$primary_value) & out$primary_value > 0, , drop = FALSE]
}

## Figure 1-2 data: IEA mine and refinery supply ------------------------------

# The supply sheet's Mineral column packs mineral and stage into one string
# ("Nickel - Refining", "Graphite - Battery grade"). Split it, and give each stage a
# label that reads the same across minerals.
jcm_iea_supply <- function(iea_path, horizon_year = 2040L) {
  critical <- read_iea_critical_minerals(iea_path)
  supply <- critical[grepl("^2 ", critical$Pillar), , drop = FALSE]
  if (nrow(supply) == 0) {
    stop("No supply pillar rows found in ", basename(iea_path))
  }

  base_col <- iea_critical_minerals_year_col(
    supply, iea_critical_minerals_base_year(supply), "IEA supply (base year)"
  )
  horizon_col <- iea_critical_minerals_year_col(supply, horizon_year, "IEA supply (horizon)")

  parts <- strsplit(supply$Mineral, " - ", fixed = TRUE)
  mineral <- vapply(parts, function(p) p[[1]], character(1))
  stage_raw <- vapply(parts, function(p) if (length(p) > 1) p[[2]] else NA_character_, character(1))

  out <- data.frame(
    mineral = ifelse(
      mineral == "Magnet rare earth elements", "Rare earths (magnet)", mineral
    ),
    stage_raw = stage_raw,
    # "Mining (natural)" and "Battery grade" are the graphite pair; everything else is a
    # plain Mining / Refining pair. Collapsing to two stages keeps the figures paired.
    stage = ifelse(grepl("^Mining", stage_raw), "Mine", "Refine / process"),
    entity = supply$Sector.Country,
    base = suppressWarnings(as.numeric(supply[[base_col]])),
    horizon = suppressWarnings(as.numeric(supply[[horizon_col]])),
    stringsAsFactors = FALSE
  )
  out$stage <- factor(out$stage, levels = c("Mine", "Refine / process"))

  # Graphite's two stages are not the same commodity, so keep the qualifier visible.
  out$panel <- ifelse(
    out$mineral == "Graphite",
    paste0("Graphite (", ifelse(out$stage_raw == "Mining (natural)", "natural", "battery grade"), ")"),
    out$mineral
  )

  attr(out, "base_year") <- as.integer(sub("^X", "", base_col))
  attr(out, "horizon_year") <- as.integer(sub("^X", "", horizon_col))
  out
}

# The workbook names countries in its own style - "Korea", "Viet Nam", "Democratic
# Republic of Congo" - which does not match countrycode's default English names, so the
# focal country is matched through ISO3 against the sheet's own vocabulary rather than by
# string equality. A country the sheet never names falls back to its English name, which
# then matches nothing and reads as an absence, which is the truth.
jcm_iea_entity_name <- function(supply, focus_iso, fallback) {
  entities <- unique(supply$entity[!supply$entity %in% JCM_IEA_SUMMARY_ROWS])
  isos <- suppressWarnings(
    countrycode::countrycode(entities, "country.name", "iso3c", warn = FALSE)
  )
  hit <- entities[!is.na(isos) & isos == focus_iso]
  if (length(hit) > 0) hit[[1]] else fallback
}

# Country shares of each mineral-stage.
#
# The workbook publishes a Total row for most stages but not all - the 2026 release omits
# it for cobalt refining - so where it is absent the denominator falls back to the sum of
# the published producer rows and says so. That sum is the same quantity the Total row
# reports elsewhere, but it inherits any rounding in the individual rows.
jcm_iea_shares <- function(supply) {
  key <- paste(supply$panel, supply$stage, sep = "|")
  producers <- supply[!supply$entity %in% JCM_IEA_SUMMARY_ROWS, , drop = FALSE]
  totals_row <- supply[supply$entity == "Total", , drop = FALSE]

  imputed <- character()
  denom <- function(k, col) {
    hit <- totals_row[paste(totals_row$panel, totals_row$stage, sep = "|") == k, col]
    hit <- hit[is.finite(hit)]
    if (length(hit) > 0) return(hit[[1]])
    imputed <<- unique(c(imputed, k))
    p <- producers[paste(producers$panel, producers$stage, sep = "|") == k, col]
    sum(p, na.rm = TRUE)
  }

  producers$key <- paste(producers$panel, producers$stage, sep = "|")
  for (col in c("base", "horizon")) {
    d <- vapply(producers$key, denom, numeric(1), col = col, USE.NAMES = FALSE)
    producers[[paste0("share_", col)]] <- ifelse(d > 0, producers[[col]] / d, NA_real_)
  }

  if (length(imputed) > 0) {
    message(
      "IEA supply: no Total row for ", paste(sub("\\|", " / ", imputed), collapse = "; "),
      " - denominator summed from producer rows."
    )
  }

  producers$key <- NULL
  producers
}

# Figure 1: focal country's and China's share of each mineral-stage, with the remainder
# split between other named producers and the workbook's residual "Rest of world" row.
jcm_fig1_data <- function(shares, focus_name = "Japan") {
  keep <- c("panel", "stage", "entity", "base", "share_base")
  d <- shares[, keep, drop = FALSE]
  d$group <- ifelse(
    d$entity == focus_name, focus_name,
    ifelse(d$entity == "China", "China",
           ifelse(d$entity == "Rest of world", "Rest of world", "Other named producers"))
  )

  agg <- stats::aggregate(
    cbind(share = share_base) ~ panel + stage + group,
    data = d[is.finite(d$share_base), , drop = FALSE], FUN = sum
  )

  # Every panel carries all four groups, so a zero reads as a zero rather than a gap.
  grid <- expand.grid(
    panel = unique(agg$panel), stage = unique(agg$stage),
    group = c("China", focus_name, "Other named producers", "Rest of world"),
    stringsAsFactors = FALSE
  )
  grid$stage <- factor(as.character(grid$stage), levels = levels(shares$stage))
  out <- merge(grid, agg, by = c("panel", "stage", "group"), all.x = TRUE)
  out <- out[!is.na(out$panel), , drop = FALSE]

  # A panel/stage pair the workbook does not publish (there is no graphite "Refining"
  # row, only battery grade) has no rows at all; drop those rather than plot zeros.
  published <- unique(paste(agg$panel, agg$stage))
  out <- out[paste(out$panel, out$stage) %in% published, , drop = FALSE]

  out$share[is.na(out$share)] <- 0
  out$group <- factor(out$group, levels = c("China", focus_name, "Other named producers", "Rest of world"))

  # Order panels by how concentrated China's grip is, worst at the top of the figure.
  chn <- out[out$group == "China", c("panel", "stage", "share")]
  rank_by <- stats::aggregate(share ~ panel, data = chn, FUN = max)
  out$panel <- factor(out$panel, levels = rank_by$panel[order(rank_by$share)])
  out[order(out$panel, out$stage, out$group), , drop = FALSE]
}

# Figure 2, left: the workbook's own top-3 supplier share, base year against horizon.
jcm_fig2_concentration <- function(supply) {
  top3 <- supply[supply$entity == "Top 3 share", , drop = FALSE]
  if (nrow(top3) == 0) {
    stop("IEA supply sheet carries no 'Top 3 share' rows.")
  }

  out <- top3[, c("panel", "stage", "base", "horizon")]
  out <- out[is.finite(out$base) & is.finite(out$horizon), , drop = FALSE]
  out$change <- out$horizon - out$base
  out$direction <- ifelse(out$change < 0, "Deconcentrating", "Concentrating further")
  # Panels repeat across the two stage facets (copper appears as a mine and a refinery),
  # so the level set has to be deduplicated before it can order the axis.
  out$panel <- factor(out$panel, levels = unique(out$panel[order(out$base)]))
  out[order(out$panel), , drop = FALSE]
}

# Figure 2, right: the focal country's own published output, base year against horizon.
#
# Returns NULL rather than failing when the sheet names no supply for the focal country -
# the IEA lists producers, so a country with no mine and no refinery simply is not there.
# That absence is figure 1's whole point, and it should not cost the other four figures.
jcm_fig2_focus_output <- function(supply, focus_name = "Japan") {
  d <- supply[supply$entity == focus_name, c("panel", "stage", "base", "horizon")]
  d <- d[is.finite(d$base), , drop = FALSE]
  if (nrow(d) == 0) {
    message("IEA supply sheet publishes no supply for ", focus_name,
            "; figure 2 will show the concentration panel only.")
    return(NULL)
  }
  d$growth <- ifelse(d$base > 0, d$horizon / d$base - 1, NA_real_)
  d$panel <- factor(d$panel, levels = unique(d$panel[order(d$growth)]))
  d[order(d$panel), , drop = FALSE]
}

## Figure 3 data: supplier concentration of each importer's basket ------------

# How concentrated are the world markets a given importer buys its critical minerals
# from?
#
# For each commodity, an HHI over reporters' shares of world exports measures how few
# countries the supply comes from. Weighting those by an importer's own spend on each
# commodity gives one number per importer: the concentration of the basket it actually
# buys, not of the trade universe in the abstract.
#
# Two caveats the figure carries in its caption. The Comtrade pull is partner "World", so
# these are *global* market structures weighted by an importer's basket, not that
# importer's own realised supplier mix - a country buying a concentrated commodity from an
# unusual supplier scores as exposed. And exporter shares are gross, so re-exported
# material is attributed to the last shipper.
jcm_supplier_exposure <- function(critmin_import, critmin_export) {
  exp_by_code <- stats::aggregate(
    primary_value ~ hs6 + reporter_iso, data = critmin_export, FUN = sum
  )
  world <- stats::aggregate(primary_value ~ hs6, data = exp_by_code, FUN = sum)
  names(world)[names(world) == "primary_value"] <- "world_exports"

  exp_by_code <- merge(exp_by_code, world, by = "hs6")
  exp_by_code$share <- exp_by_code$primary_value / exp_by_code$world_exports

  hhi <- stats::aggregate(
    cbind(supplier_hhi = share^2) ~ hs6, data = exp_by_code, FUN = sum
  )
  top1 <- stats::aggregate(cbind(top_supplier_share = share) ~ hs6, data = exp_by_code, FUN = max)
  n_sup <- stats::aggregate(cbind(n_suppliers = share) ~ hs6, data = exp_by_code, FUN = length)

  # Which country is the top supplier of each commodity, for the CSV.
  ord <- exp_by_code[order(exp_by_code$hs6, -exp_by_code$share), , drop = FALSE]
  lead <- ord[!duplicated(ord$hs6), c("hs6", "reporter_iso")]
  names(lead)[2] <- "top_supplier_iso"

  concentration <- Reduce(
    function(a, b) merge(a, b, by = "hs6"), list(hhi, top1, n_sup, lead)
  )

  imp <- merge(
    critmin_import[, c("reporter_iso", "hs6", "cmd_desc", "primary_value")],
    concentration, by = "hs6"
  )

  by_importer <- do.call(rbind, lapply(split(imp, imp$reporter_iso), function(d) {
    data.frame(
      reporter_iso = d$reporter_iso[[1]],
      import_bill_usd = sum(d$primary_value),
      weighted_supplier_hhi = stats::weighted.mean(d$supplier_hhi, d$primary_value),
      weighted_top_supplier_share = stats::weighted.mean(d$top_supplier_share, d$primary_value),
      n_commodities = nrow(d),
      stringsAsFactors = FALSE
    )
  }))
  rownames(by_importer) <- NULL

  list(by_importer = by_importer[order(-by_importer$import_bill_usd), , drop = FALSE],
       by_commodity = concentration[order(-concentration$supplier_hhi), , drop = FALSE])
}

## Figure 4 data: the focal country's trade by processing stage ---------------

jcm_stage_flows <- function(critmin_import, critmin_export, focus_iso = "JPN") {
  both <- rbind(critmin_import, critmin_export)
  d <- both[both$reporter_iso == focus_iso & !is.na(both$stage), , drop = FALSE]
  if (nrow(d) == 0) {
    stop("No critical mineral trade rows for ", focus_iso, ".")
  }

  agg <- stats::aggregate(primary_value ~ stage + flow, data = d, FUN = sum)
  wide <- stats::reshape(
    agg, idvar = "stage", timevar = "flow", direction = "wide"
  )
  names(wide) <- sub("^primary_value\\.", "", names(wide))
  for (col in c("Imports", "Exports")) {
    if (!col %in% names(wide)) wide[[col]] <- 0
    wide[[col]][is.na(wide[[col]])] <- 0
  }

  wide$net <- wide$Exports - wide$Imports
  wide$stage <- factor(wide$stage, levels = JCM_STAGE_LEVELS)
  wide[order(wide$stage), c("stage", "Imports", "Exports", "net"), drop = FALSE]
}

## Figure 5 data: policy activity --------------------------------------------

# Active domestic interventions per year from the NIPO annual panel.
#
# n_policies_window counts distinct policies active in that year, so the series is a
# stock, not a flow of announcements - a level that falls only when policies expire or
# are removed. It is a count rather than one of the strength scores on purpose: counts
# are directly comparable across countries and stages without depending on the
# intervention-strength scaling.
jcm_policy_activity <- function(repo_root, from_year = 2015L, tech = "Batteries") {
  path <- file.path(repo_root, config$processed_dir, "nipo_tech_year.rds")
  if (!file.exists(path)) {
    stop("NIPO annual panel not found: ", path, "\nRun scripts/10_build_themes.R first.")
  }
  panel <- as.data.frame(readRDS(path))

  d <- panel[panel$tech == tech & panel$iso3 %in% names(JCM_POLICY_PEERS), , drop = FALSE]
  if (nrow(d) == 0) {
    stop("No NIPO rows for tech '", tech, "' and the peer set.")
  }

  # The panel runs to the current calendar year, which is partial - GTA keeps recording
  # into it - so the series stops at the last complete year.
  last_complete <- min(
    max(d$announce_year, na.rm = TRUE),
    as.integer(format(Sys.Date(), "%Y")) - 1L
  )
  d <- d[d$announce_year >= from_year & d$announce_year <= last_complete, , drop = FALSE]

  out <- data.frame(
    iso3 = d$iso3,
    name = unname(JCM_POLICY_PEERS[d$iso3]),
    supply_chain = d$supply_chain,
    year = as.integer(d$announce_year),
    n_active_policies = as.integer(d$n_policies_window),
    stringsAsFactors = FALSE
  )
  out$name <- factor(out$name, levels = unname(JCM_POLICY_PEERS))
  out$supply_chain <- factor(
    out$supply_chain, levels = c("Upstream", "Midstream"),
    labels = c("Upstream (mining & raw material)", "Midstream (processing & cells)")
  )
  attr(out, "last_complete_year") <- last_complete
  out[order(out$supply_chain, out$name, out$year), , drop = FALSE]
}

## Theme ---------------------------------------------------------------------

jcm_theme <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 2),
      plot.subtitle = ggplot2::element_text(colour = "grey30", size = base_size - 1),
      plot.caption = ggplot2::element_text(colour = "grey40", size = base_size - 3, hjust = 0),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = base_size - 1),
      legend.key.size = ggplot2::unit(0.9, "lines")
    )
}

## Figures -------------------------------------------------------------------

jcm_plot_fig1 <- function(fig1, focus_name, base_year) {
  fills <- stats::setNames(
    c(JCM_CHINA_COLOUR, JCM_FOCUS_COLOUR, JCM_OTHER_COLOUR, JCM_ROW_COLOUR),
    c("China", focus_name, "Other named producers", "Rest of world")
  )

  labels <- fig1[fig1$group == focus_name, , drop = FALSE]
  labels$text <- ifelse(
    labels$share > 0, sprintf("%.1f%%", 100 * labels$share), "none"
  )

  ggplot2::ggplot(fig1, ggplot2::aes(x = share, y = panel, fill = group)) +
    # reverse = TRUE so China anchors the left edge of every bar and the focal country's
    # sliver always sits immediately next to it, where it can be compared by eye.
    ggplot2::geom_col(width = 0.7, position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_text(
      data = labels, ggplot2::aes(x = 1.02, y = panel, label = text),
      inherit.aes = FALSE, hjust = 0, size = 2.9,
      colour = JCM_FOCUS_COLOUR, fontface = "bold"
    ) +
    ggplot2::facet_wrap(~stage, nrow = 1) +
    ggplot2::scale_fill_manual(values = fills, drop = FALSE) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      breaks = c(0, 0.5, 1), limits = c(0, 1.16), expand = c(0, 0)
    ) +
    ggplot2::labs(
      title = sprintf("%s mines none of it, and refines a sliver", focus_name),
      subtitle = sprintf(
        "Share of world supply, %d. Orange figures are %s's share of that stage.",
        base_year, focus_name
      ),
      x = NULL, y = NULL, fill = NULL
    ) +
    jcm_theme() +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.major.y = ggplot2::element_blank()
    )
}

jcm_plot_fig2_left <- function(conc, base_year, horizon_year) {
  colours <- c(Deconcentrating = unname(JCM_PALETTE["green"]),
               `Concentrating further` = unname(JCM_PALETTE["orange"]))

  ggplot2::ggplot(conc, ggplot2::aes(y = panel)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = base, xend = horizon, yend = panel, colour = direction),
      linewidth = 1.1,
      arrow = grid::arrow(length = grid::unit(0.055, "in"), type = "closed")
    ) +
    ggplot2::geom_point(ggplot2::aes(x = base), size = 2.1, colour = "grey35") +
    ggplot2::facet_wrap(~stage, ncol = 1, scales = "free_y") +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = 0.06)
    ) +
    ggplot2::labs(
      title = "Supplier markets are set to loosen",
      subtitle = sprintf(
        "Top-3 country share of supply, %d (grey dot) to %d (arrow head)",
        base_year, horizon_year
      ),
      x = NULL, y = NULL, colour = NULL
    ) +
    jcm_theme() +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.major.y = ggplot2::element_blank()
    )
}

jcm_plot_fig2_right <- function(focus_output, focus_name, base_year, horizon_year) {
  # Plotted as growth rather than as levels: the focal country's four refined streams
  # differ by three orders of magnitude (Japan refines ~1,400 kt of copper and ~6 kt of
  # cobalt), so on a shared kt axis everything but copper collapses onto the origin. The
  # levels are not lost - each bar carries its own base -> horizon figures - and the
  # comparison the panel is making is about direction and pace, not size.
  # Growth and the levels behind it go in one string placed at the bar end: as two
  # separate text layers they collide on the short bars, where there is no room beside
  # the bar for a second line.
  kt <- function(x) {
    ifelse(
      abs(x) >= 100,
      formatC(x, format = "f", digits = 0, big.mark = ","),
      formatC(x, format = "f", digits = 1)
    )
  }
  focus_output$label <- ifelse(
    is.finite(focus_output$growth),
    sprintf(
      "%+.0f%%   %s → %s kt", 100 * focus_output$growth,
      kt(focus_output$base), kt(focus_output$horizon)
    ),
    "n/a"
  )
  span <- max(abs(focus_output$growth), na.rm = TRUE)

  ggplot2::ggplot(focus_output, ggplot2::aes(x = growth, y = panel)) +
    ggplot2::geom_col(width = 0.55, fill = JCM_FOCUS_COLOUR) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey20", linewidth = 0.5) +
    ggplot2::geom_text(
      ggplot2::aes(x = pmax(growth, 0), label = label),
      hjust = -0.06, size = 3, colour = "grey20"
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(min(0, 1.1 * min(focus_output$growth, na.rm = TRUE)), span * 2.0)
    ) +
    ggplot2::labs(
      title = sprintf("...and %s's own refineries are set to grow", focus_name),
      subtitle = sprintf("Change in refined output, %d to %d", base_year, horizon_year),
      x = NULL, y = NULL
    ) +
    jcm_theme() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

jcm_plot_fig3 <- function(by_importer, focus_iso, top_n, trade_year) {
  d <- utils::head(by_importer, top_n)
  d$bill_bn <- d$import_bill_usd / 1e9
  d$is_focus <- d$reporter_iso == focus_iso

  p <- ggplot2::ggplot(d, ggplot2::aes(x = bill_bn, y = weighted_supplier_hhi)) +
    ggplot2::geom_hline(
      yintercept = stats::median(d$weighted_supplier_hhi), linetype = "dashed",
      colour = "grey60", linewidth = 0.4
    ) +
    ggplot2::geom_point(
      ggplot2::aes(colour = is_focus, size = is_focus)
    ) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = JCM_FOCUS_COLOUR, `FALSE` = "grey55")) +
    ggplot2::scale_size_manual(values = c(`TRUE` = 4.2, `FALSE` = 2.4)) +
    ggplot2::scale_x_log10(
      labels = scales::label_dollar(suffix = "bn", accuracy = 1),
      # Room for the repelled label on the largest importer, which otherwise clips.
      expand = ggplot2::expansion(mult = 0.09)
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
    ggplot2::labs(
      title = "A big bill, bought from thin markets",
      subtitle = sprintf(
        "%d critical mineral imports against the concentration of the export markets they come from;\ndashed line is the median of the %d largest importers",
        trade_year, nrow(d)
      ),
      x = "Critical mineral imports (log scale)",
      y = "Import-weighted supplier HHI"
    ) +
    jcm_theme() +
    ggplot2::theme(legend.position = "none")

  if (requireNamespace("ggrepel", quietly = TRUE)) {
    p + ggrepel::geom_text_repel(
      ggplot2::aes(label = reporter_iso, colour = is_focus, fontface = ifelse(is_focus, "bold", "plain")),
      size = 3, seed = 42, min.segment.length = 0.2, segment.colour = "grey70",
      max.overlaps = Inf, show.legend = FALSE
    )
  } else {
    p + ggplot2::geom_text(
      ggplot2::aes(label = reporter_iso, colour = is_focus), size = 3,
      vjust = -0.9, show.legend = FALSE
    )
  }
}

jcm_plot_fig4 <- function(stage_flows, focus_name, trade_year) {
  long <- rbind(
    data.frame(stage = stage_flows$stage, flow = "Imports",
               value = -stage_flows$Imports / 1e9, stringsAsFactors = FALSE),
    data.frame(stage = stage_flows$stage, flow = "Exports",
               value = stage_flows$Exports / 1e9, stringsAsFactors = FALSE)
  )
  long$flow <- factor(long$flow, levels = c("Imports", "Exports"))

  span <- max(abs(long$value))

  # Net sits in its own column at the right margin rather than at its own x position,
  # which lands inside a bar whenever the two flows are close in size.
  nets <- data.frame(
    stage = stage_flows$stage,
    net = stage_flows$net / 1e9,
    label = sprintf("net %s$%.1fbn", ifelse(stage_flows$net < 0, "-", "+"), abs(stage_flows$net) / 1e9),
    stringsAsFactors = FALSE
  )
  nets$colour <- ifelse(nets$net < 0, unname(JCM_PALETTE["navy"]), JCM_FOCUS_COLOUR)

  ggplot2::ggplot(long, ggplot2::aes(x = value, y = stage, fill = flow)) +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey20", linewidth = 0.5) +
    ggplot2::geom_text(
      data = nets, ggplot2::aes(x = span * 1.29, y = stage, label = label),
      inherit.aes = FALSE, hjust = 1, size = 3, fontface = "bold", colour = nets$colour
    ) +
    ggplot2::scale_fill_manual(values = c(
      Imports = unname(JCM_PALETTE["navy"]), Exports = JCM_FOCUS_COLOUR
    )) +
    ggplot2::scale_x_continuous(
      labels = function(x) scales::label_dollar(suffix = "bn", accuracy = 1)(abs(x)),
      limits = c(-span * 1.3, span * 1.3)
    ) +
    ggplot2::scale_y_discrete(labels = function(x) sub(" & ", " &\n", x)) +
    ggplot2::labs(
      title = sprintf("%s buys ore and sells refined metal", focus_name),
      subtitle = sprintf("Critical mineral trade by processing stage, %d", trade_year),
      x = NULL, y = NULL, fill = NULL
    ) +
    jcm_theme() +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.major.y = ggplot2::element_blank()
    )
}

jcm_plot_fig5 <- function(policy, focus_name) {
  focal <- policy[policy$name == focus_name, , drop = FALSE]

  # The two panels need free y scales - the midstream counts are an order of magnitude
  # larger - but free scales also hide the very gap the figure is about, so the focal
  # country's latest count is printed on each panel.
  ends <- do.call(rbind, lapply(split(focal, focal$supply_chain, drop = TRUE), function(d) {
    d[which.max(d$year), , drop = FALSE]
  }))

  ggplot2::ggplot(policy, ggplot2::aes(x = year, y = n_active_policies, colour = name)) +
    ggplot2::geom_line(ggplot2::aes(linewidth = name == focus_name)) +
    ggplot2::geom_point(data = focal, size = 1.5) +
    ggplot2::geom_label(
      data = ends,
      ggplot2::aes(label = sprintf("%s: %d", focus_name, n_active_policies)),
      hjust = 1.05, vjust = -0.45, size = 3, fontface = "bold",
      label.size = 0, fill = "white", show.legend = FALSE
    ) +
    ggplot2::facet_wrap(~supply_chain, nrow = 1, scales = "free_y") +
    ggplot2::scale_colour_manual(values = JCM_POLICY_COLOURS) +
    ggplot2::scale_linewidth_manual(values = c(`TRUE` = 1.3, `FALSE` = 0.6), guide = "none") +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 5)) +
    ggplot2::labs(
      title = "The policy response is real, and it is all midstream",
      subtitle = "Distinct domestic industrial-policy interventions active in each year, battery supply chain",
      x = NULL, y = "Active interventions", colour = NULL
    ) +
    jcm_theme() +
    ggplot2::theme(legend.position = "bottom")
}

## Runner --------------------------------------------------------------------

jcm_write <- function(obj, out_dir, stem) {
  path <- file.path(out_dir, paste0(stem, ".csv"))
  utils::write.csv(obj, path, row.names = FALSE, na = "")
  message("Wrote ", path)
  invisible(path)
}

jcm_save <- function(plot, out_dir, stem, width, height, caption) {
  framed <- plot + ggplot2::labs(caption = caption)
  path <- file.path(out_dir, paste0(stem, ".png"))
  ggplot2::ggsave(
    path, framed, width = width, height = height, dpi = 200, bg = "white",
    device = if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else NULL
  )
  message("Wrote ", path)
  invisible(path)
}

run_japan_critical_minerals <- function(repo_root,
                                        focus_iso = "JPN",
                                        horizon_year = 2040L,
                                        policy_from = 2015L,
                                        top_importers = 20L,
                                        min_reporters = 50L,
                                        write_output = TRUE) {
  ensure_packages(c("ggplot2", "scales"))

  focus_name <- countrycode::countrycode(focus_iso, "iso3c", "country.name")
  if (is.na(focus_name)) focus_name <- focus_iso

  raw_data_path <- file.path(repo_root, config$raw_data_dir)
  if (!dir.exists(raw_data_path)) {
    stop("Raw data directory not found: ", raw_data_path)
  }
  iea_path <- jcm_iea_path(raw_data_path)
  critmin_paths <- jcm_critmin_paths(raw_data_path, focus_iso, min_reporters = min_reporters)

  ## IEA supply, figures 1 and 2
  supply <- jcm_iea_supply(iea_path, horizon_year = horizon_year)
  base_year <- attr(supply, "base_year")
  resolved_horizon <- attr(supply, "horizon_year")
  iea_name <- jcm_iea_entity_name(supply, focus_iso, focus_name)
  shares <- jcm_iea_shares(supply)

  fig1 <- jcm_fig1_data(shares, focus_name = iea_name)
  # Shares must reconstruct the whole of each published stage, not merely resemble it.
  #
  # The tolerance is loose because the workbook publishes both the producer rows and the
  # Total row rounded to three significant figures, so the two disagree by up to ~0.2%
  # by construction. Anything beyond that is a real gap - a producer row this code failed
  # to assign to a group - and should stop the run being trusted.
  stage_totals <- stats::aggregate(share ~ panel + stage, data = fig1, FUN = sum)
  worst <- max(abs(stage_totals$share - 1))
  message(sprintf("Figure 1 check: max |sum(shares) - 1| = %.2e (IEA rounding tolerance 5e-3)", worst))
  if (worst > 5e-3) {
    warning("Producer shares do not sum to the published stage total.", call. = FALSE)
  }

  concentration <- jcm_fig2_concentration(supply)
  focus_output <- jcm_fig2_focus_output(supply, focus_name = iea_name)

  ## Comtrade, figures 3 and 4
  critmin_import <- jcm_read_critmin_flow(critmin_paths$import, "Imports")
  critmin_export <- jcm_read_critmin_flow(critmin_paths$export, "Exports")
  trade_year <- critmin_paths$year

  unstaged <- sum(critmin_import$primary_value[is.na(critmin_import$stage)])
  message(sprintf(
    "Trade panel: %d commodities, %d reporters, year %d; %.2f%% of import value unclassified by stage.",
    length(unique(critmin_import$hs6)), length(unique(critmin_import$reporter_iso)),
    trade_year, 100 * unstaged / sum(critmin_import$primary_value)
  ))

  exposure <- jcm_supplier_exposure(critmin_import, critmin_export)
  stage_flows <- jcm_stage_flows(critmin_import, critmin_export, focus_iso = focus_iso)

  focus_row <- exposure$by_importer[exposure$by_importer$reporter_iso == focus_iso, , drop = FALSE]
  if (nrow(focus_row) == 1) {
    rank <- sum(exposure$by_importer$weighted_supplier_hhi >= focus_row$weighted_supplier_hhi)
    message(sprintf(
      "%s: $%.1fbn critical mineral imports, weighted supplier HHI %.3f (rank %d of %d importers).",
      focus_name, focus_row$import_bill_usd / 1e9, focus_row$weighted_supplier_hhi,
      rank, nrow(exposure$by_importer)
    ))
  }

  ## NIPO, figure 5
  policy <- jcm_policy_activity(repo_root, from_year = policy_from)

  result <- list(
    iea_shares = shares, fig1 = fig1, concentration = concentration,
    focus_output = focus_output, supplier_exposure = exposure$by_importer,
    commodity_concentration = exposure$by_commodity, stage_flows = stage_flows,
    policy = policy,
    meta = list(
      focus_iso = focus_iso, focus_name = focus_name, iea_entity_name = iea_name,
      base_year = base_year,
      horizon_year = resolved_horizon, trade_year = trade_year,
      iea_source = basename(iea_path)
    )
  )
  if (!write_output) return(invisible(result))

  out_dir <- file.path(repo_root, config$processed_dir, "charts")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  prefix <- sprintf("%s_critical_minerals", tolower(focus_iso))

  iea_cap <- sprintf(
    "Source: %s, base case projection. Mine supply is contained metal; refined supply is refinery output. Graphite's two\nstages are natural flake mining and battery-grade material, which are not the same commodity.",
    result$meta$iea_source
  )
  comtrade_src <- sprintf(
    "Source: UN Comtrade %d, reporter against partner World, on the Columbia University Critical Minerals Dashboard commodity list.",
    trade_year
  )
  exposure_cap <- paste0(
    comtrade_src,
    "\nSupplier HHI is computed over reporters' shares of world exports of each commodity, then weighted by the importer's own spend on it: it",
    "\nmeasures how thin the markets an importer buys from are, not that importer's realised supplier mix. Exporter shares are gross, so",
    "\nre-exported material is credited to the last shipper."
  )
  stage_cap <- paste0(
    comtrade_src,
    "\nProcessing stage is assigned from the HS chapter, which separates ore from chemistry from metal but not stages within a chapter."
  )

  jcm_write(fig1, out_dir, paste0(prefix, "_supply_shares"))
  jcm_write(concentration, out_dir, paste0(prefix, "_top3_concentration"))
  if (!is.null(focus_output)) {
    jcm_write(focus_output, out_dir, paste0(prefix, "_focus_refining_outlook"))
  }
  jcm_write(exposure$by_importer, out_dir, paste0(prefix, "_supplier_exposure"))
  jcm_write(exposure$by_commodity, out_dir, paste0(prefix, "_commodity_concentration"))
  jcm_write(stage_flows, out_dir, paste0(prefix, "_stage_flows"))
  jcm_write(policy, out_dir, paste0(prefix, "_policy_activity"))

  p1 <- jcm_plot_fig1(fig1, iea_name, base_year)
  p2l <- jcm_plot_fig2_left(concentration, base_year, resolved_horizon)
  p3 <- jcm_plot_fig3(exposure$by_importer, focus_iso, top_importers, trade_year)
  p4 <- jcm_plot_fig4(stage_flows, focus_name, trade_year)
  p5 <- jcm_plot_fig5(policy, focus_name)

  # The right panel only exists where the IEA names supply for the focal country.
  p2 <- if (is.null(focus_output)) {
    p2l
  } else {
    p2r <- jcm_plot_fig2_right(focus_output, iea_name, base_year, resolved_horizon)
    cowplot::ggdraw(cowplot::plot_grid(p2l, p2r, nrow = 1, rel_widths = c(1, 0.92)))
  }
  p2_width <- if (is.null(focus_output)) 7 else 12

  pngs <- c(
    jcm_save(p1, out_dir, paste0(prefix, "_01_supply_shares"), 10.5, 5.6, iea_cap),
    jcm_save(p2, out_dir, paste0(prefix, "_02_outlook"), p2_width, 6.4, iea_cap),
    jcm_save(p3, out_dir, paste0(prefix, "_03_supplier_exposure"), 9.4, 6.6, exposure_cap),
    jcm_save(p4, out_dir, paste0(prefix, "_04_stage_flows"), 8.6, 5, stage_cap),
    jcm_save(
      p5, out_dir, paste0(prefix, "_05_policy_activity"), 10, 5.2,
      sprintf(
        "Source: RMI New Industrial Policy Observatory, built on Global Trade Alert. Counts distinct domestic interventions active in each\nyear within the index's rolling window, so the level falls only as policies expire. Series stops at %d, the last complete year.",
        attr(policy, "last_complete_year")
      )
    )
  )

  result$png <- pngs
  invisible(result)
}

jcm_run_directly <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  length(file_arg) > 0 &&
    identical(
      basename(sub("^--file=", "", file_arg[1])),
      "japan_critical_minerals_security.R"
    )
}

if (jcm_run_directly()) {
  invisible(run_japan_critical_minerals(
    repo_root = repo_root,
    focus_iso = toupper(Sys.getenv("JCM_FOCUS_ISO", "JPN")),
    horizon_year = as.integer(Sys.getenv("JCM_HORIZON_YEAR", "2040")),
    policy_from = as.integer(Sys.getenv("JCM_POLICY_FROM", "2015")),
    top_importers = as.integer(Sys.getenv("JCM_TOP_IMPORTERS", "20")),
    min_reporters = as.integer(Sys.getenv("JCM_MIN_REPORTERS", "50"))
  ))
}
