# Import exposure by importer x technology x six-stage cell (docs/strategic_position.md section 3):
# how much of a stage's HS6 basket a country buys abroad, from how many suppliers, and how much of
# it comes from China or from economies of the opposite UN voting bloc. The Strategic Need inputs
# supplier_concentration and risky_supplier_exposure (R/strategic_position/strategic_need.R). Given the
# coercive-supplier pairs of the strategic posture (sp_coercive_pairs()), it also returns the shares
# from coercive and from trusted suppliers that Test 2 of the posture reads (section 7).
#
# Built from the reconciled exporter -> importer flow table of the partner themes
# (partnership_strength_build_trade_cells(), R/themes/partnership_strength/partnership_strength_helpers.R):
# per (tech, stage) and year, the importer's own declarations when it reports the cell, else every
# exporter's declaration of that flow (mirror, at the CIF/FOB factor). A code the HS6 master files
# under several technologies counts in each, so a cell is technology-attributed trade, not a
# partition of the importer's trade. scripts/15_build_partner_themes.R writes the table to
# data/processed/import_exposure_tbl.rds; nothing in the partner themes reads it.
#
# Pure: no IO.

# The ISO3 codes counted as "China" in china_share: the mainland and the two SARs, through which a
# large part of Chinese exports is re-exported (Hong Kong) or invoiced.
SP_CHINA_ISO3 <- c("CHN", "HKG", "MAC")

# One GDP value (current USD) per economy for a reference year from a WDI extract
# (data/raw/wdi_gdp.csv: iso3c, year, NY.GDP.MKTP.CD): the value of `year`, else the latest earlier
# year with a value, so an economy whose newest WDI figure lags one year is not left without a
# denominator. Returns (iso3, gdp_usd, gdp_year). WDI aggregates (WLD, EUU, ...) pass through;
# they join no importer.
sp_gdp_usd <- function(gdp_data, year, iso_col = "iso3c", value_col = "NY.GDP.MKTP.CD") {
  missing <- setdiff(c(iso_col, "year", value_col), names(gdp_data))
  if (length(missing) > 0) {
    stop("sp_gdp_usd(): gdp_data lacks column(s) ", paste(missing, collapse = ", "), ".")
  }
  tibble::tibble(
    iso3 = toupper(trimws(as.character(gdp_data[[iso_col]]))),
    gdp_year = suppressWarnings(as.integer(gdp_data$year)),
    gdp_usd = suppressWarnings(as.numeric(gdp_data[[value_col]]))
  ) %>%
    dplyr::filter(!is.na(iso3), nzchar(iso3), !is.na(gdp_year), gdp_year <= year, !is.na(gdp_usd), gdp_usd > 0) %>%
    dplyr::group_by(iso3) %>%
    dplyr::slice_max(gdp_year, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(iso3, gdp_usd, gdp_year)
}

# Import exposure per (importer_iso, tech, supply_chain) from the flows in `value_col`
# ("value_last": the last theme year; "value_first" for the first):
#   observed             the importer has a record in the cell that year (its own declaration or a
#                        mirror; a recorded zero counts). FALSE rows complete the grid of importers
#                        x cells in `trade_cells` and carry NA, never 0.
#   import_source        "importer" or "mirror" (from source_last / source_first when present)
#   imports_usd          sum of the importer's flows from every exporter
#   n_suppliers          exporters with a positive flow
#   top_supplier_iso3    the largest exporter (ties: first ISO3 alphabetically); NA without imports
#   top_supplier_share   its share of imports_usd
#   import_hhi           sum of squared supplier shares (0-1)
#   china_share          share from `china_iso3` (mainland China, Hong Kong, Macao). NA for an
#                        importer that is itself in `china_iso3`: its trade with the other two is
#                        within one economy, not exposure to a foreign supplier.
#   opposite_bloc_share  share from exporters with geo_alignment == 0 against the importer in
#                        `alignment` (reporter_iso = importer, partner_iso = exporter). A pair the
#                        table does not cover is not counted as opposite bloc;
#                        alignment_coverage_share says how much of imports_usd had a value.
#   imports_to_gdp       imports_usd / gdp_usd (NA without GDP)
# With `coercive_pairs` (sp_coercive_pairs(), R/strategic_position/supply_structure.R: reporter_iso,
# partner_iso, coercive, trusted), two more shares for the strategic posture (docs/strategic_position.md
# section 7, "Supply structure"):
#   coercive_import_share share from the importer's potentially coercive suppliers (the opposite bloc
#                        plus the coercive powers outside the importer's bloc)
#   trusted_import_share share from its trusted suppliers (same bloc, geo_alignment == 1)
# Both NA for an importer the pairs table does not cover (no alignment); a partner it does not pair
# with the importer (an unaligned exporter, a "not specified" code) counts as neither, as in
# opposite_bloc_share. For a U.S.-leaning importer coercive_import_share equals opposite_bloc_share
# (the powers are in the China-leaning bloc); for a Nonaligned one it is the China group plus Russia.
# An observed importer with zero imports gets 0 for the shares and the HHI (it has no import
# exposure, which is not the same as missing data). `trade_year`, when given, is stamped on the
# rows. `gdp` is (iso3, gdp_usd), e.g. sp_gdp_usd(wdi_gdp, year).
sp_import_exposure <- function(trade_cells,
                               alignment,
                               gdp,
                               china_iso3 = SP_CHINA_ISO3,
                               value_col = "value_last",
                               trade_year = NULL,
                               coercive_pairs = NULL) {
  keys <- c("importer_iso", "tech", "supply_chain")
  need <- c("exporter_iso", keys, value_col)
  missing <- setdiff(need, names(trade_cells))
  if (length(missing) > 0) {
    stop("sp_import_exposure(): trade_cells lacks column(s) ", paste(missing, collapse = ", "), ".")
  }
  missing <- setdiff(c("reporter_iso", "partner_iso", "geo_alignment"), names(alignment))
  if (length(missing) > 0) {
    stop("sp_import_exposure(): alignment lacks column(s) ", paste(missing, collapse = ", "), ".")
  }
  missing <- setdiff(c("iso3", "gdp_usd"), names(gdp))
  if (length(missing) > 0) {
    stop("sp_import_exposure(): gdp lacks column(s) ", paste(missing, collapse = ", "), ".")
  }
  assert_supply_chain_stages(trade_cells$supply_chain, label = "trade_cells supply_chain")
  if (anyDuplicated(gdp$iso3) > 0) stop("sp_import_exposure(): gdp has more than one row per iso3.")
  with_posture <- !is.null(coercive_pairs)
  if (with_posture) {
    missing <- setdiff(c("reporter_iso", "partner_iso", "coercive", "trusted"), names(coercive_pairs))
    if (length(missing) > 0) {
      stop("sp_import_exposure(): coercive_pairs lacks column(s) ", paste(missing, collapse = ", "), "; use sp_coercive_pairs().")
    }
    if (anyDuplicated(coercive_pairs[c("reporter_iso", "partner_iso")]) > 0) {
      stop("sp_import_exposure(): coercive_pairs has more than one row per (reporter_iso, partner_iso).")
    }
  }

  source_col <- sub("^value_", "source_", value_col)
  has_source <- source_col %in% names(trade_cells)

  flows <- tibble::tibble(
    exporter_iso = as.character(trade_cells$exporter_iso),
    importer_iso = as.character(trade_cells$importer_iso),
    tech = as.character(trade_cells$tech),
    supply_chain = as.character(trade_cells$supply_chain),
    value = as.numeric(trade_cells[[value_col]]),
    source = if (has_source) as.character(trade_cells[[source_col]]) else NA_character_
  )
  if (any(flows$value < 0, na.rm = TRUE)) stop("sp_import_exposure(): negative trade values in ", value_col, ".")
  if (any(flows$exporter_iso == flows$importer_iso, na.rm = TRUE)) {
    stop("sp_import_exposure(): trade_cells holds self-flows (exporter_iso == importer_iso).")
  }

  # The grid every importer x cell of the table, so an importer the year does not observe in a
  # cell is an explicit NA row rather than an absent one.
  grid <- tidyr::expand_grid(
    importer_iso = sort(unique(flows$importer_iso)),
    dplyr::distinct(flows, tech, supply_chain)
  )

  align <- tibble::tibble(
    importer_iso = as.character(alignment$reporter_iso),
    exporter_iso = as.character(alignment$partner_iso),
    geo_alignment = as.numeric(alignment$geo_alignment)
  ) %>%
    dplyr::filter(!is.na(geo_alignment)) %>%
    dplyr::distinct(importer_iso, exporter_iso, .keep_all = TRUE)

  obs <- flows %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::left_join(align, by = c("importer_iso", "exporter_iso"))
  rm(flows)
  if (with_posture) {
    cp <- tibble::tibble(
      importer_iso = as.character(coercive_pairs$reporter_iso),
      exporter_iso = as.character(coercive_pairs$partner_iso),
      coercive = as.logical(coercive_pairs$coercive),
      trusted = as.logical(coercive_pairs$trusted)
    )
    covered_importers <- unique(cp$importer_iso)
    obs <- dplyr::left_join(obs, cp, by = c("importer_iso", "exporter_iso"))
    rm(cp)
  } else {
    obs$coercive <- NA
    obs$trusted <- NA
  }

  # Largest supplier per importer-cell, computed apart so the summary below stays vectorised.
  top <- obs %>%
    dplyr::filter(value > 0) %>%
    dplyr::arrange(importer_iso, tech, supply_chain, dplyr::desc(value), exporter_iso) %>%
    dplyr::distinct(importer_iso, tech, supply_chain, .keep_all = TRUE) %>%
    dplyr::select(dplyr::all_of(keys), top_supplier_iso3 = exporter_iso, top_value = value)

  summ <- obs %>%
    dplyr::group_by(importer_iso, tech, supply_chain) %>%
    dplyr::summarise(
      import_source = if (all(is.na(source))) NA_character_ else paste(sort(unique(stats::na.omit(source))), collapse = "+"),
      imports_usd = sum(value),
      n_suppliers = sum(value > 0),
      sum_sq = sum(value^2),
      china_usd = sum(value[exporter_iso %in% china_iso3]),
      opposite_usd = sum(value[!is.na(geo_alignment) & geo_alignment == 0]),
      aligned_known_usd = sum(value[!is.na(geo_alignment)]),
      coercive_usd = sum(value[coercive %in% TRUE]),
      trusted_usd = sum(value[trusted %in% TRUE]),
      .groups = "drop"
    )
  rm(obs)

  out <- grid %>%
    dplyr::left_join(summ, by = keys) %>%
    dplyr::left_join(top, by = keys) %>%
    dplyr::left_join(dplyr::select(gdp, iso3, gdp_usd), by = c("importer_iso" = "iso3")) %>%
    dplyr::mutate(
      observed = !is.na(imports_usd),
      any_imports = observed & imports_usd > 0,
      top_supplier_share = dplyr::case_when(any_imports ~ top_value / imports_usd, observed ~ 0),
      import_hhi = dplyr::case_when(any_imports ~ sum_sq / imports_usd^2, observed ~ 0),
      china_share = dplyr::case_when(
        importer_iso %in% china_iso3 ~ NA_real_,
        any_imports ~ china_usd / imports_usd,
        observed ~ 0
      ),
      opposite_bloc_share = dplyr::case_when(any_imports ~ opposite_usd / imports_usd, observed ~ 0),
      alignment_coverage_share = dplyr::if_else(any_imports, aligned_known_usd / imports_usd, NA_real_),
      imports_to_gdp = imports_usd / gdp_usd
    )
  if (with_posture) {
    covered <- out$importer_iso %in% covered_importers
    out <- out %>%
      dplyr::mutate(
        coercive_import_share = dplyr::case_when(!covered ~ NA_real_, any_imports ~ coercive_usd / imports_usd, observed ~ 0),
        trusted_import_share = dplyr::case_when(!covered ~ NA_real_, any_imports ~ trusted_usd / imports_usd, observed ~ 0)
      )
  }
  out <- out %>%
    dplyr::select(
      importer_iso, tech, supply_chain, observed, import_source, imports_usd, n_suppliers,
      top_supplier_iso3, top_supplier_share, import_hhi, china_share, opposite_bloc_share,
      dplyr::any_of(c("coercive_import_share", "trusted_import_share")),
      alignment_coverage_share, gdp_usd, imports_to_gdp
    ) %>%
    dplyr::arrange(tech, supply_chain, importer_iso)
  if (!is.null(trade_year)) out$trade_year <- as.integer(trade_year)
  out
}
