# Critical minerals trade theme builder functions.

# === Critical minerals trade (UN Comtrade) ===
# These builders mirror the legacy UN Comtrade pipeline for critical minerals
# and return the tidy, canonical schema.
#
# Construction (Technical Appendix, "Critical-mineral trade": security sub-index =
# mean(market-share, HHI), demand-weighted across minerals). For reporter i, mineral m and
# technology t, with w_mt the IEA base-year demand shares renormalized over the minerals that
# have trade data:
#   critmin_market_share_it = sum_m w_mt * S_m(X_im / sum_j X_jm)   (S_m: S-curve within m
#                             over the exporters, above a zero anchor; non-exporters score 0)
#   critmin_hhi_t           = sum_m w_mt * S(-HHI_m)                 (same for every country)
# config/index_definition.yml averages the two into "Overall Minerals Trade Index". The HHI
# term is a property of the world market for the technology's minerals, so it only shifts the
# level of a technology; it is emitted for every country in `country_info`, not only the
# Comtrade reporters, so that a non-reporter is not scored below a reporter on it.
#
# Exports X_im come from the exporter's own Comtrade declarations when it reports, and from its
# partners' import declarations ("mirror" exports) when it does not:
#   X_im = reported exports of i                                 if i reports exports
#        = sum_k M_kim / r_c(m)                                  otherwise
# over the reporting importers k (World, the EU, ASEAN and other aggregate reporters excluded).
# Without the mirror the large producers that do not report - the DR Congo, Russia, Guinea,
# Gabon, New Caledonia, Mongolia, Lao PDR, Viet Nam - had no exports and scored 0, and every
# world total and HHI was computed without them. Imports are valued CIF and exports FOB, so a
# mirror flow is moved to an FOB basis: the importer's own FOB value where it declares one,
# otherwise CIF / r_c, with r_c the CIF/FOB ratio of the HS chapter estimated from the importer
# declarations that carry both values (pooled ratio for a chapter with fewer than 30). "Reports"
# is decided per economy - an economy in the export or total-export pull keeps its own figures
# for every mineral, including the zeros - so reporters' exports are never mixed with mirrors.
# An economy that has declared the year's exports since the pulls were staged (a "late
# reporter": the DR Congo, Jordan, Oman, ... for 2024) counts as a reporter, with the own
# declarations the mirror fetcher collects; the mirror is only for economies that never report.
# That rule also removes the clearest mirror artefact in the 2024 data: Iraq declares USD 1.2bn
# of copper wire from Jordan, which Jordan itself declares USD 0.3m of exporting.

# ---- Build mineral regex for commodity parsing ----
# The legacy pipeline extracts mineral names from Comtrade descriptions using a
# regex built from the IEA mineral demand list. Canonical names that Comtrade spells
# differently ("rare-earth metals", "platinum") are searched by their Comtrade wording.
critical_minerals_trade_build_minerals_pattern <- function(mineral_demand_clean) {
  minerals <- unique(harmonize_mineral_names(mineral_demand_clean$Mineral))
  minerals <- minerals[!is.na(minerals)]

  comtrade_terms <- c(
    "Graphite" = "graphite",
    "Rare Earths" = "rare.?earth",
    "PGMs" = "platinum|palladium|rhodium"
  )
  terms <- ifelse(minerals %in% names(comtrade_terms), comtrade_terms[minerals], minerals)

  paste0(paste0(unique(terms), collapse = "|"), "|", "Graphite")
}

# ---- Filter HS codes to critical minerals ----
# Match the Comtrade critical-minerals dashboard to the minerals regex so the
# API pulls only the relevant commodity codes.
critical_minerals_trade_filter_hs <- function(crit_hs, mineral_demand_clean) {
  minerals_pattern <- critical_minerals_trade_build_minerals_pattern(mineral_demand_clean)

  crit_hs %>%
    dplyr::filter(
      stringr::str_detect(
        comtradeDescription,
        stringr::regex(minerals_pattern, ignore_case = TRUE)
      )
    )
}

# ---- Mirror exports for economies that do not report ----
# Comtrade reporters whose declarations sum other reporters' (World, the EU, ASEAN): their
# imports are already in their members' figures, so they never count toward a mirror.
CRITMIN_AGGREGATE_REPORTERS <- c("W00", "WLD", "World", "EUR", "EUN", "R4")

# The economies that declare exports to Comtrade in this vintage: every reporter in the
# critical-minerals export pull or the total-export pull (a reporter of total exports with no
# critical-minerals rows exported none of them).
critical_minerals_trade_reporters <- function(critmin_export, total_export) {
  reporters <- c(critmin_export$reporter_iso, total_export$reporter_iso)
  unique(reporters[!is.na(reporters) & nzchar(reporters)])
}

# CIF/FOB ratio by HS chapter, from the import declarations that carry both a CIF and an FOB
# value. A chapter with fewer than `min_records` such declarations takes the pooled ratio,
# which is also returned as chapter "all" (used for total trade).
critical_minerals_trade_cif_fob_ratio <- function(imports, min_records = 30) {
  empty <- tibble::tibble(chapter = character(), n_records = integer(), cif_fob = numeric())
  if (is.null(imports) || !all(c("cifvalue", "fobvalue", "cmd_code") %in% names(imports))) {
    return(empty)
  }

  both <- imports %>%
    dplyr::transmute(
      chapter = substr(as.character(cmd_code), 1, 2),
      cifvalue = suppressWarnings(as.numeric(cifvalue)),
      fobvalue = suppressWarnings(as.numeric(fobvalue))
    ) %>%
    dplyr::filter(!is.na(cifvalue), !is.na(fobvalue), cifvalue > 0, fobvalue > 0, chapter != "TO")
  if (nrow(both) == 0) {
    return(empty)
  }

  pooled <- sum(both$cifvalue) / sum(both$fobvalue)
  both %>%
    dplyr::group_by(chapter) %>%
    dplyr::summarize(
      n_records = dplyr::n(),
      cif_fob = sum(cifvalue) / sum(fobvalue),
      .groups = "drop"
    ) %>%
    dplyr::mutate(cif_fob = dplyr::if_else(n_records >= min_records, cif_fob, pooled)) %>%
    dplyr::bind_rows(tibble::tibble(chapter = "all", n_records = nrow(both), cif_fob = pooled))
}

# Mirror exports at an FOB basis, one row per (exporter, HS code), from partner-level import
# declarations (scripts/fetchers/comtrade_mirror_critmin.R). Only economies in `economies` that
# are not in `reporters` are mirrored, so World, the "areas n.e.s." codes, bunkers, free zones
# and special categories never become exporters; aggregate importers are dropped so no flow is
# counted twice. cmd_code "TOTAL" rows carry each exporter's total mirror exports.
#
# Re-exports: importers record partners by country of origin (IMTS 2010), so goods that only
# transit an entrepot are credited to their producer, not to the hub.
critical_minerals_trade_build_mirror <- function(mirror_import,
                                                 reporters,
                                                 economies,
                                                 cif_fob,
                                                 year = NULL) {
  empty <- tibble::tibble(
    reporter_iso = character(), cmd_code = character(), cmd_desc = character(),
    exports = numeric(), exports_cif = numeric(), n_importers = integer()
  )
  if (is.null(mirror_import) || nrow(mirror_import) == 0) {
    return(empty)
  }

  m <- tibble::as_tibble(mirror_import) %>%
    dplyr::mutate(
      reporter_iso = as.character(reporter_iso),
      partner_iso = as.character(partner_iso),
      cmd_code = as.character(cmd_code),
      cmd_desc = if ("cmd_desc" %in% names(mirror_import)) as.character(cmd_desc) else NA_character_,
      dplyr::across(dplyr::any_of(c("cifvalue", "fobvalue", "primary_value")), ~ suppressWarnings(as.numeric(.x)))
    )
  for (col in setdiff(c("cifvalue", "fobvalue", "primary_value"), names(m))) m[[col]] <- NA_real_

  # Totals over transport modes, customs procedures and consignment partners only.
  keep_total <- function(tbl, col, total) {
    if (col %in% names(tbl)) tbl[is.na(tbl[[col]]) | as.character(tbl[[col]]) == total, , drop = FALSE] else tbl
  }
  m <- keep_total(m, "flow_code", "M")
  m <- keep_total(m, "partner2_code", "0")
  m <- keep_total(m, "mot_code", "0")
  m <- keep_total(m, "customs_code", "C00")
  if (!is.null(year) && "ref_year" %in% names(m)) {
    m <- m[suppressWarnings(as.integer(m$ref_year)) %in% year, , drop = FALSE]
  }

  ratio <- cif_fob %>% dplyr::select(chapter, cif_fob)
  pooled <- ratio$cif_fob[ratio$chapter == "all"]
  if (length(pooled) == 0 || is.na(pooled)) {
    stop("critical_minerals_trade_build_mirror(): no CIF/FOB ratio to put mirror imports on an FOB basis.")
  }

  m <- m %>%
    dplyr::filter(
      partner_iso %in% economies,
      !partner_iso %in% reporters,
      !is.na(reporter_iso), nzchar(reporter_iso),
      !reporter_iso %in% CRITMIN_AGGREGATE_REPORTERS,
      reporter_iso != partner_iso
    ) %>%
    dplyr::distinct(reporter_iso, partner_iso, cmd_code, .keep_all = TRUE)
  if (nrow(m) == 0) {
    return(empty)
  }

  m %>%
    dplyr::mutate(chapter = dplyr::if_else(cmd_code == "TOTAL", "all", substr(cmd_code, 1, 2))) %>%
    dplyr::left_join(ratio, by = "chapter") %>%
    dplyr::mutate(
      cif_fob = dplyr::coalesce(cif_fob, pooled),
      exports_cif = dplyr::coalesce(primary_value, cifvalue, fobvalue, 0),
      # The importer's own FOB valuation where it declares one, CIF deflated otherwise.
      exports = dplyr::case_when(
        !is.na(fobvalue) & fobvalue > 0 ~ fobvalue,
        !is.na(cifvalue) & cifvalue > 0 ~ cifvalue / cif_fob,
        !is.na(primary_value) & primary_value > 0 ~ primary_value / cif_fob,
        TRUE ~ 0
      )
    ) %>%
    dplyr::rename(importer_iso = reporter_iso, reporter_iso = partner_iso) %>%
    dplyr::group_by(reporter_iso, cmd_code) %>%
    dplyr::summarize(
      cmd_desc = dplyr::first(cmd_desc[!is.na(cmd_desc)]),
      exports = sum(exports),
      exports_cif = sum(exports_cif),
      n_importers = dplyr::n_distinct(importer_iso),
      .groups = "drop"
    )
}

# Late reporters: economies outside the staged pulls that have since declared their own exports
# for the year (the flow "X", partner World rows of critmin_mirror_import.csv). They are
# reporters like any other - their own declarations, zeros for the codes they do not declare -
# so the mirror is used only for economies that do not report at all. Returns the commodity rows
# (`exports`, shaped like the critmin export pull) and the TOTAL rows (`totals`, like the
# total-export pull) of the economies in `economies` that are not in `reporters`.
critical_minerals_trade_late_exports <- function(mirror_import, reporters, economies, year = NULL) {
  empty <- list(
    exports = tibble::tibble(
      reporter_iso = character(), cmd_code = character(), cmd_desc = character(),
      primary_value = numeric(), ref_year = integer()
    ),
    totals = tibble::tibble(reporter_iso = character(), primary_value = numeric())
  )
  if (is.null(mirror_import) || nrow(mirror_import) == 0 || !"flow_code" %in% names(mirror_import)) {
    return(empty)
  }

  x <- tibble::as_tibble(mirror_import)
  x <- x[as.character(x$flow_code) %in% "X", , drop = FALSE]
  # Exports to World, over transport modes, customs procedures and consignment partners.
  if ("partner_code" %in% names(x)) x <- x[as.character(x$partner_code) %in% "0", , drop = FALSE]
  for (col in intersect(c("partner2_code", "mot_code"), names(x))) {
    x <- x[is.na(x[[col]]) | as.character(x[[col]]) == "0", , drop = FALSE]
  }
  if ("customs_code" %in% names(x)) x <- x[is.na(x$customs_code) | x$customs_code == "C00", , drop = FALSE]
  if (!is.null(year) && "ref_year" %in% names(x)) {
    x <- x[suppressWarnings(as.integer(x$ref_year)) %in% year, , drop = FALSE]
  }
  x <- x[as.character(x$reporter_iso) %in% setdiff(economies, reporters), , drop = FALSE]
  if (nrow(x) == 0) {
    return(empty)
  }

  x <- x %>%
    dplyr::transmute(
      reporter_iso = as.character(reporter_iso),
      cmd_code = as.character(cmd_code),
      cmd_desc = if ("cmd_desc" %in% names(x)) as.character(cmd_desc) else NA_character_,
      primary_value = suppressWarnings(as.numeric(primary_value)),
      ref_year = suppressWarnings(as.integer(ref_year))
    ) %>%
    dplyr::distinct(reporter_iso, cmd_code, .keep_all = TRUE)

  list(
    exports = x %>% dplyr::filter(cmd_code != "TOTAL"),
    totals = x %>% dplyr::filter(cmd_code == "TOTAL") %>% dplyr::select(reporter_iso, primary_value)
  )
}

# ---- Merge import/export flows ----
# Combine import/export values, map reporter ISO3 to country names, and derive
# the mineral label from the commodity description.
#
# Imports and exports are full-joined on (reporter, HS code): the import-led left join
# dropped every reporter-code with exports but no imports (2.5% of 2024 export value).
# `mirror_exports` (critical_minerals_trade_build_mirror()) adds the non-reporters' exports;
# `export_source` records which of the two each row's exports come from. A mirror economy's
# own imports are unknown, so they stay NA.
critical_minerals_trade_build_trade <- function(critmin_import,
                                                critmin_export,
                                                country_info,
                                                minerals_pattern,
                                                mirror_exports = NULL) {
  flows <- function(tbl, value_name) {
    tbl %>%
      dplyr::mutate(cmd_code = as.character(cmd_code)) %>%
      dplyr::group_by(reporter_iso, cmd_code) %>%
      dplyr::summarize(
        cmd_desc = dplyr::first(cmd_desc),
        "{value_name}" := sum(primary_value, na.rm = TRUE),
        .groups = "drop"
      )
  }

  reported <- dplyr::full_join(
    flows(critmin_import, "imports"),
    flows(critmin_export, "exports"),
    by = c("reporter_iso", "cmd_code"),
    suffix = c("_imp", "_exp")
  ) %>%
    dplyr::mutate(
      cmd_desc = dplyr::coalesce(cmd_desc_exp, cmd_desc_imp),
      imports = dplyr::if_else(is.na(imports), 0, imports),
      export_source = "reported"
    ) %>%
    dplyr::select(-cmd_desc_imp, -cmd_desc_exp)

  mirrored <- NULL
  if (!is.null(mirror_exports) && nrow(mirror_exports) > 0) {
    # The pull's own description for a code, so the mineral regex reads one wording.
    descriptions <- reported %>%
      dplyr::filter(!is.na(cmd_desc)) %>%
      dplyr::distinct(cmd_code, .keep_all = TRUE) %>%
      dplyr::select(cmd_code, pull_desc = cmd_desc)
    # An economy that declares exports keeps them; a mirror is never added on top.
    mirrored <- mirror_exports %>%
      dplyr::filter(cmd_code != "TOTAL", !reporter_iso %in% critmin_export$reporter_iso) %>%
      dplyr::left_join(descriptions, by = "cmd_code") %>%
      dplyr::transmute(
        reporter_iso, cmd_code,
        cmd_desc = dplyr::coalesce(pull_desc, cmd_desc),
        imports = NA_real_,
        exports,
        export_source = "mirror"
      )
  }

  # One source per economy: a non-reporter that declares imports only keeps those imports
  # beside its mirror exports.
  mirror_iso <- unique(mirrored$reporter_iso)
  dplyr::bind_rows(reported, mirrored) %>%
    dplyr::mutate(export_source = dplyr::if_else(reporter_iso %in% mirror_iso, "mirror", export_source)) %>%
    dplyr::group_by(reporter_iso, cmd_code, export_source) %>%
    dplyr::summarize(
      cmd_desc = dplyr::first(cmd_desc[!is.na(cmd_desc)]),
      imports = if (all(is.na(imports))) NA_real_ else sum(imports, na.rm = TRUE),
      exports = sum(exports, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(country_info %>% dplyr::select(iso3c, country), by = c("reporter_iso" = "iso3c")) %>%
    dplyr::mutate(
      exports = dplyr::if_else(is.na(exports), 0, exports),
      trade_balance = (exports - imports) / (exports + imports),
      mineral = harmonize_mineral_names(
        stringr::str_extract(cmd_desc, stringr::regex(minerals_pattern, ignore_case = TRUE))
      ),
      supply_chain = "Upstream"
    )
}

# ---- Compute market shares and HHI ----
# Mineral-level exports, world market shares and export-market HHIs over the Comtrade
# reporters. Every reporter is completed with zero exports for the minerals it does not
# export, so the tech rollup below weights every mineral the technology needs instead of only
# the ones a country happens to trade. With mirror exports in `critmin_trade` the world totals,
# shares and HHIs run over reporters and mirrored non-reporters alike; `total_export` then needs
# the mirror economies' total exports too, for the export-share term.
critical_minerals_trade_build_hhi <- function(critmin_trade, total_export, gamma = 0.5) {
  if (!"export_source" %in% names(critmin_trade)) {
    critmin_trade$export_source <- "reported"
  }

  critmin_trade %>%
    dplyr::filter(!is.na(mineral)) %>%
    dplyr::group_by(reporter_iso, country, export_source, mineral, supply_chain) %>%
    dplyr::summarize(exports = sum(exports, na.rm = TRUE), .groups = "drop") %>%
    tidyr::complete(
      tidyr::nesting(reporter_iso, country, export_source),
      tidyr::nesting(mineral, supply_chain),
      fill = list(exports = 0)
    ) %>%
    dplyr::left_join(
      total_export %>%
        dplyr::group_by(reporter_iso) %>%
        dplyr::summarize(total_exports = sum(primary_value, na.rm = TRUE), .groups = "drop"),
      by = "reporter_iso"
    ) %>%
    dplyr::mutate(exp_share = exports / total_exports) %>%
    dplyr::group_by(mineral, supply_chain) %>%
    dplyr::mutate(
      market_share = exports / sum(exports, na.rm = TRUE),
      HHI = sum(market_share^2, na.rm = TRUE),
      # A reporter exporting none of a mineral scores 0, the same as a country the zero
      # missing-data rule fills in; exporters are ranked within the mineral above it.
      market_share_index = iea_critical_minerals_share_index(market_share, gamma = gamma)
    ) %>%
    dplyr::ungroup()
}

# World export concentration by mineral: one row per mineral, S-curve of -HHI across minerals
# (lower concentration = more secure).
critical_minerals_trade_build_mineral_hhi <- function(critmin_hhi, gamma = 0.5) {
  critmin_hhi %>%
    dplyr::distinct(mineral, supply_chain, HHI) %>%
    dplyr::mutate(hhi_index = median_scurve(-HHI, gamma = gamma))
}

# Demand weights over the minerals that have trade data, identical for every country.
critical_minerals_trade_build_weights <- function(mineral_demand_clean, traded_minerals) {
  mineral_demand_clean %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(tech)) %>%
    dplyr::transmute(mineral = harmonize_mineral_names(Mineral), tech, share_24) %>%
    dplyr::filter(mineral %in% traded_minerals) %>%
    dplyr::group_by(mineral, tech) %>%
    dplyr::summarize(share_24 = sum(share_24, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(tech) %>%
    dplyr::filter(sum(share_24) > 0) %>%
    dplyr::mutate(weight = share_24 / sum(share_24)) %>%
    dplyr::ungroup() %>%
    dplyr::select(mineral, tech, weight)
}

# ---- Roll up to technology-level trade indices ----
critical_minerals_trade_build_tech <- function(critmin_hhi, mineral_demand_clean, gamma = 0.5) {
  weights <- critical_minerals_trade_build_weights(
    mineral_demand_clean,
    traded_minerals = unique(critmin_hhi$mineral)
  )

  critmin_hhi %>%
    dplyr::inner_join(weights, by = "mineral", relationship = "many-to-many") %>%
    dplyr::group_by(reporter_iso, country, export_source, tech, supply_chain) %>%
    dplyr::summarize(
      critmin_market_share = sum(weight * market_share, na.rm = TRUE),
      critmin_market_share_index = sum(weight * market_share_index, na.rm = TRUE),
      critmin_export_share = sum(weight * exp_share, na.rm = TRUE),
      .groups = "drop"
    )
}

# Technology-level HHI component: one value per technology.
critical_minerals_trade_build_tech_hhi <- function(critmin_hhi, mineral_demand_clean, gamma = 0.5) {
  weights <- critical_minerals_trade_build_weights(
    mineral_demand_clean,
    traded_minerals = unique(critmin_hhi$mineral)
  )

  critical_minerals_trade_build_mineral_hhi(critmin_hhi, gamma = gamma) %>%
    dplyr::inner_join(weights, by = "mineral", relationship = "many-to-many") %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::summarize(
      critmin_hhi = sum(weight * HHI),
      critmin_hhi_index = sum(weight * hhi_index),
      .groups = "drop"
    )
}

# ---- Canonical tidy output ----
# `provenance` is the country's own export data: "reported" (its Comtrade export declarations),
# "mirror" (its partners' import declarations) or "none" (neither; only the world-market HHI
# rows exist for it). standardize_theme_types() keeps the theme schema only, so `source`
# carries the same distinction into the processed table.
CRITMIN_TRADE_SOURCES <- c(
  reported = "UN Comtrade (reported exports)",
  mirror = "UN Comtrade (mirror exports: partner-reported imports, FOB basis)"
)

critical_minerals_trade_build_tidy <- function(critmin_trade_tech,
                                               critmin_tech_hhi,
                                               countries,
                                               year = 2024) {
  if (!"export_source" %in% names(critmin_trade_tech)) {
    critmin_trade_tech$export_source <- "reported"
  }
  provenance <- critmin_trade_tech %>%
    dplyr::filter(!is.na(country)) %>%
    dplyr::distinct(country, provenance = export_source)
  world_source <- if (any(provenance$provenance == "mirror")) {
    "UN Comtrade (reported and mirror exports)"
  } else {
    CRITMIN_TRADE_SOURCES[["reported"]]
  }

  share_tbl <- critmin_trade_tech %>%
    dplyr::filter(!is.na(country)) %>%
    dplyr::select(
      country, tech, supply_chain,
      critmin_market_share, critmin_market_share_index, critmin_export_share
    ) %>%
    tidyr::pivot_longer(
      cols = -c(country, tech, supply_chain),
      names_to = "variable",
      values_to = "value"
    )

  hhi_tbl <- tidyr::crossing(country = unique(countries[!is.na(countries)]), critmin_tech_hhi) %>%
    tidyr::pivot_longer(
      cols = c(critmin_hhi, critmin_hhi_index),
      names_to = "variable",
      values_to = "value"
    )

  dplyr::bind_rows(share_tbl, hhi_tbl) %>%
    dplyr::left_join(provenance, by = "country") %>%
    dplyr::mutate(
      provenance = dplyr::coalesce(provenance, "none"),
      category = "Minerals Trade",
      data_type = dplyr::if_else(stringr::str_detect(variable, "_index$"), "index", "raw"),
      variable = stringr::str_remove(variable, "_index$"),
      Year = year,
      source = dplyr::if_else(
        stringr::str_detect(variable, "^critmin_hhi"),
        world_source,
        unname(CRITMIN_TRADE_SOURCES[provenance])
      ),
      explanation = dplyr::case_when(
        variable == "critmin_market_share" & data_type == "raw" ~
          "Demand-weighted share of world exports of the technology's minerals",
        variable == "critmin_market_share" ~
          "Demand-weighted within-mineral S-curve of world export market share (non-exporters = 0)",
        variable == "critmin_export_share" ~
          "Demand-weighted share of the technology's minerals in the country's total exports",
        variable == "critmin_hhi" & data_type == "raw" ~
          "Demand-weighted HHI of world exports of the technology's minerals",
        variable == "critmin_hhi" ~
          "Demand-weighted S-curve of -HHI of world mineral exports (same for every country)",
        TRUE ~ "UN Comtrade export data"
      )
    ) %>%
    dplyr::rename(Country = country) %>%
    dplyr::select(
      Country,
      tech,
      supply_chain,
      category,
      variable,
      data_type,
      value,
      Year,
      source,
      explanation,
      provenance
    )
}

# `year` defaults to the reference year the Comtrade extract itself carries, so the label
# follows the file rather than a pinned 2024.
critical_minerals_trade_year <- function(critmin_import, critmin_export) {
  ref_years <- suppressWarnings(as.integer(c(critmin_export$ref_year, critmin_import$ref_year)))
  if (all(is.na(ref_years))) 2024L else max(ref_years, na.rm = TRUE)
}

# === Exporter x mineral flows ===
# One row per exporter and mineral: exports (USD, FOB), the world market share, the mineral's
# HHI, the export share and `export_source` ("reported" / "mirror"). The theme is built from
# this table; scripts/10 also writes it out as the record of where each economy's figures come
# from. `mirror_import` is the partner-level import pull (critmin_mirror_import.csv), whose flow
# "X" rows add the late reporters' own exports; NULL reproduces the reporter-only construction.
# Rows of another year are ignored.
critical_minerals_trade_flows <- function(critmin_import,
                                          critmin_export,
                                          total_export,
                                          mineral_demand_clean,
                                          country_info,
                                          mirror_import = NULL,
                                          year = NULL,
                                          gamma = 0.5) {
  year <- year %||% critical_minerals_trade_year(critmin_import, critmin_export)
  minerals_pattern <- critical_minerals_trade_build_minerals_pattern(mineral_demand_clean)

  mirror_exports <- NULL
  if (!is.null(mirror_import) && nrow(mirror_import) > 0) {
    # Late reporters join the reporters first, so they are never mirrored.
    late <- critical_minerals_trade_late_exports(
      mirror_import,
      reporters = critical_minerals_trade_reporters(critmin_export, total_export),
      economies = country_info$iso3c,
      year = year
    )
    if (nrow(late$exports) > 0) {
      # The pull's own description for a code, so the mineral regex reads one wording.
      pull_desc <- critmin_export %>%
        dplyr::transmute(cmd_code = as.character(cmd_code), pull_desc = as.character(cmd_desc)) %>%
        dplyr::filter(!is.na(pull_desc)) %>%
        dplyr::distinct(cmd_code, .keep_all = TRUE)
      late$exports <- late$exports %>%
        dplyr::left_join(pull_desc, by = "cmd_code") %>%
        dplyr::mutate(cmd_desc = dplyr::coalesce(pull_desc, cmd_desc)) %>%
        dplyr::select(-pull_desc)
    }
    critmin_export <- dplyr::bind_rows(
      critmin_export %>% dplyr::mutate(cmd_code = as.character(cmd_code)),
      late$exports
    )
    total_export <- dplyr::bind_rows(
      total_export %>% dplyr::select(reporter_iso, primary_value),
      late$totals
    )

    cif_fob <- critical_minerals_trade_cif_fob_ratio(critmin_import)
    if (nrow(cif_fob) == 0) cif_fob <- critical_minerals_trade_cif_fob_ratio(mirror_import)
    mirror_exports <- critical_minerals_trade_build_mirror(
      mirror_import = mirror_import,
      reporters = critical_minerals_trade_reporters(critmin_export, total_export),
      economies = country_info$iso3c,
      cif_fob = cif_fob,
      year = year
    )
    if (nrow(mirror_exports) == 0 && nrow(late$exports) == 0 && nrow(late$totals) == 0) {
      warning(
        "critical_minerals_trade_flows(): the mirror import data carry no flows for ", year,
        " from non-reporting economies; they keep zero exports.",
        call. = FALSE
      )
    }
  }

  critmin_trade <- critical_minerals_trade_build_trade(
    critmin_import = critmin_import,
    critmin_export = critmin_export,
    country_info = country_info,
    minerals_pattern = minerals_pattern,
    mirror_exports = mirror_exports
  )

  mirror_totals <- if (is.null(mirror_exports)) NULL else {
    mirror_exports %>%
      dplyr::filter(cmd_code == "TOTAL") %>%
      dplyr::select(reporter_iso, primary_value = exports)
  }
  total_export <- dplyr::bind_rows(
    total_export %>% dplyr::select(reporter_iso, primary_value),
    mirror_totals
  )

  # An economy whose total exports are known but that exports none of the minerals - a late
  # reporter that declares only its total (Iraq, for 2024), or a non-reporter whose partners
  # record other goods from it but none of these - gets zero rows like every other exporter,
  # not no rows, which would leave it unscored ("none") on market share.
  traded <- critmin_trade %>%
    dplyr::filter(!is.na(mineral)) %>%
    dplyr::distinct(mineral, supply_chain)
  silent <- setdiff(unique(total_export$reporter_iso), critmin_trade$reporter_iso[!is.na(critmin_trade$mineral)])
  silent <- silent[!is.na(silent) & silent %in% country_info$iso3c]
  if (length(silent) > 0 && nrow(traded) > 0) {
    critmin_trade <- dplyr::bind_rows(
      critmin_trade,
      tibble::tibble(
        reporter_iso = silent,
        country = country_info$country[match(silent, country_info$iso3c)],
        export_source = dplyr::if_else(silent %in% mirror_totals$reporter_iso, "mirror", "reported"),
        mineral = traded$mineral[1],
        supply_chain = traded$supply_chain[1],
        exports = 0
      )
    )
  }

  critical_minerals_trade_build_hhi(
    critmin_trade = critmin_trade,
    total_export = total_export,
    gamma = gamma
  ) %>%
    dplyr::mutate(Year = year)
}

# === Public theme entrypoint ===
critical_minerals_trade <- function(critmin_import,
                                    critmin_export,
                                    total_export,
                                    mineral_demand_clean,
                                    country_info,
                                    mirror_import = NULL,
                                    year = NULL,
                                    gamma = 0.5) {
  year <- year %||% critical_minerals_trade_year(critmin_import, critmin_export)

  critmin_hhi <- critical_minerals_trade_flows(
    critmin_import = critmin_import,
    critmin_export = critmin_export,
    total_export = total_export,
    mineral_demand_clean = mineral_demand_clean,
    country_info = country_info,
    mirror_import = mirror_import,
    year = year,
    gamma = gamma
  )

  critmin_trade_tech <- critical_minerals_trade_build_tech(
    critmin_hhi = critmin_hhi,
    mineral_demand_clean = mineral_demand_clean,
    gamma = gamma
  )

  critmin_tech_hhi <- critical_minerals_trade_build_tech_hhi(
    critmin_hhi = critmin_hhi,
    mineral_demand_clean = mineral_demand_clean,
    gamma = gamma
  )

  tidy <- critical_minerals_trade_build_tidy(
    critmin_trade_tech = critmin_trade_tech,
    critmin_tech_hhi = critmin_tech_hhi,
    countries = country_info$country,
    year = year
  )

  # The overall rows the index definition adds carry no provenance; they take the country's.
  tidy %>%
    energy_security_add_overall_index() %>%
    dplyr::select(-dplyr::any_of("provenance")) %>%
    dplyr::left_join(dplyr::distinct(tidy, Country, provenance), by = "Country")
}
