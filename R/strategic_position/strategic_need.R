# Strategic Need by country x technology x six-stage cell (docs/strategic_position.md section 3):
# how exposed a country is if supply of the cell's goods is disrupted. Vulnerability only: no
# Competitive Position or Economic Opportunity input, no world constant (a value identical for
# every country cannot rank countries) and no variable the EO pillar carries with the same values.
#
#   1. Each measure of config strategic_need.families, oriented so higher = more need, is turned
#      into a mid-rank percentile across the countries of the cell that have it.
#   2. A family is the mean of its measures' percentiles; strategic_need_score is the mean of the
#      country's observed families (equal weights); strategic_need_percentile / _rank rank the score
#      across countries within the cell, NA (sn_thin_cell) when fewer than min_countries_per_cell
#      are scored or, in a cell with trade data, fewer than min_material_importers_per_cell (default:
#      min_countries_per_cell) import at least min_import_usd.
#   3. Supplier-concentration and risky-supplier measures are 0 (no need) when the country's
#      imports of the cell are below min_import_usd: an HHI of 1 on USD 50k of imports is not a
#      vulnerability. Under immaterial_need "zero" (default; config: "set to need 0 rather than
#      computed") such a country's percentile on those measures is 0. Under "tie" the measure value 0
#      is ranked with the others, which in a cell where most countries import next to nothing puts
#      the whole zero block at a mid-rank near 0.5 (Wind Extraction, Nuclear Recovery: 2026-10-27).
#      Either way the material importers are ranked over every observed country, zeros included.
#   4. Deployment has no traded basket. Its need is the technology's Assembly need (the equipment,
#      evidence "proxy"); for fuels (Coal, Gas, Oil) it also takes the Extraction import dependence
#      (the fuel itself, evidence "shared": one observation used at two stages).
#   5. Entrepots and ship registries (exclude_iso3: Hong Kong, Macao, the Marshall Islands, Liberia,
#      Panama) are not scored. Their recorded imports are re-exports or flag-of-convenience mirror
#      flows (a ship "imported" by a registry), not their own exposure, and they topped the need
#      rankings on that noise. They are taken out of the universe before any percentile, so they do
#      not move other countries' ranks; their cell rows keep the import context with
#      sn_exclusion = "entrepot" and no score.
#   6. Nonaligned importers (nonaligned_opposite_bloc: na). geo_alignment is 0.5 for every pair
#      with a nonaligned or neutral economy, so such an importer's opposite_bloc_share is 0 by
#      construction, which ranked India, Brazil, Mexico, Saudi Arabia, Singapore and 93 other
#      nonaligned economies at the bottom of that measure whatever they buy from China or Russia.
#      For them the measure is not applicable (NA, weight 0) and the risky-supplier family is the
#      China share alone. Nonalignment is read from geo_alignment_tbl (the `geo_alignment`
#      argument): an importer none of whose non-missing pair values differs from 0.5. The
#      import-exposure table cannot tell: it carries only alignment_coverage_share, and a 0
#      opposite-bloc share there may as well be an aligned importer that buys nothing from the
#      other bloc.
#
# Needs R/strategic_position/sp_helpers.R, R/utils/supply_chain_stages.R and, when country_iso3 is
# NULL, R/utils/country.R. Pure: no IO.

# ES variables that must never enter Strategic Need: world constants (one value per cell for every
# country: world export HHI of the basket, world mineral HHI, world price volatility) and the
# variables the EO pillar carries with identical values or from the same source, which belong to
# Competitive Position (docs/strategic_position.md; phase-1 audit of the ES pillar, 2026-10-27).
SP_NEED_EXCLUDED_VARIABLES <- c(
  "HHI", "critmin_hhi", "Overall Price Volatility Index",
  "market_share", "critmin_market_share", "Market Share", "Mineral Supply", "production", "net_export",
  "Annual Investment Index", "Investment Momentum Index", "Operating Capacity Index", "Pipeline Capacity Index",
  "installed_cap_index", "elec_growth_index", "Energy consumption per capita", "Energy consumption per capita growth"
)

# Defaults for rules the config leaves out (config/strategic_position.yml strategic_need names them
# all; a missing key falls back to these). No exclusions and the opposite-bloc share kept as
# measured are the pre-2026-10-27 behaviour.
SP_NEED_FUEL_TECHS <- c("Coal", "Gas", "Oil")
SP_NEED_IMMATERIAL_FAMILIES <- c("supplier_concentration", "risky_supplier_exposure")
SP_NEED_IMPORT_CATEGORY <- "Import Exposure"
SP_NEED_EXCLUDE_ISO3 <- character()
SP_NEED_NONALIGNED_RULES <- c("na", "as_is")
SP_NEED_OPPOSITE_BLOC_VARIABLE <- "opposite_bloc_share"

# NULL default (a private operator: other files may define their own %||%).
`%sp_or%` <- function(a, b) if (is.null(a)) b else a

# The configured measures as one table: family, source, category, variable, orient, fill, pillar.
# Stops on an unknown source or orientation, and on a variable that must stay out of need.
sp_need_measures <- function(cfg_need) {
  fams <- cfg_need$families
  if (is.null(fams) || length(fams) == 0 || is.null(names(fams))) {
    stop("sp_strategic_need(): cfg_need$families must be a named list of families.")
  }
  rows <- lapply(names(fams), function(f) {
    ms <- fams[[f]]$measures
    if (is.null(ms) || length(ms) == 0) stop("sp_strategic_need(): family ", f, " has no measures.")
    do.call(rbind, lapply(ms, function(m) {
      data.frame(
        family = f,
        source = as.character(m$source %sp_or% NA_character_),
        category = as.character(m$category %sp_or% SP_NEED_IMPORT_CATEGORY),
        variable = as.character(m$variable %sp_or% NA_character_),
        orient = as.character(m$orient %sp_or% "as_is"),
        fill = as.character(m$fill %sp_or% "na"),
        stringsAsFactors = FALSE
      )
    }))
  })
  out <- do.call(rbind, rows)
  bad_src <- setdiff(unique(out$source), c("es_contributions", "import_exposure"))
  if (length(bad_src) > 0) {
    stop("sp_strategic_need(): unknown measure source(s) ", paste(bad_src, collapse = ", "),
         " (es_contributions or import_exposure; Strategic Need takes no Competitive Position or EO input).")
  }
  bad_orient <- setdiff(unique(out$orient), c("invert", "as_is"))
  if (length(bad_orient) > 0) stop("sp_strategic_need(): unknown orient ", paste(bad_orient, collapse = ", "), ".")
  bad_fill <- setdiff(unique(out$fill), c("na", "structural_zero"))
  if (length(bad_fill) > 0) stop("sp_strategic_need(): unknown fill ", paste(bad_fill, collapse = ", "), ".")
  banned <- out$variable[out$source == "es_contributions" & out$variable %in% SP_NEED_EXCLUDED_VARIABLES]
  if (length(banned) > 0) {
    stop("sp_strategic_need(): ", paste(unique(banned), collapse = ", "),
         " cannot enter Strategic Need (world constant or a Competitive Position / EO variable).")
  }
  if (anyDuplicated(out[c("source", "category", "variable")]) > 0) {
    stop("sp_strategic_need(): a measure is listed twice in cfg_need$families.")
  }
  out$pillar <- ifelse(out$source == "es_contributions", "ES", "SN")
  out
}

# Whether an importer's opposite-bloc share means anything, from geo_alignment_tbl (reporter_iso,
# partner_iso, geo_alignment; scripts/12): "bloc" when at least one partner has a non-missing
# geo_alignment other than 0.5 (the importer sits in a bloc, so some partners can be opposite),
# "nonaligned" when every non-missing value is 0.5 (nonaligned or neutral economies under hard
# blocs), "no_alignment" when the table has no value for it (then no pair is counted as opposite
# either). Under soft blocs (tau > 0) pair values are rarely exactly 0.5, so nearly every importer
# is "bloc" and the rule has little effect. Returns tibble(iso3, opposite_bloc_basis).
sp_need_opposite_bloc_basis <- function(iso3, geo_alignment, tol = 1e-9) {
  missing <- setdiff(c("reporter_iso", "geo_alignment"), names(geo_alignment))
  if (length(missing) > 0) {
    stop("sp_need_opposite_bloc_basis(): geo_alignment lacks ", paste(missing, collapse = ", "), ".")
  }
  a <- as.numeric(geo_alignment$geo_alignment)
  rep_iso <- as.character(geo_alignment$reporter_iso)
  has_value <- unique(rep_iso[!is.na(a)])
  in_bloc <- unique(rep_iso[!is.na(a) & abs(a - 0.5) > tol])
  iso3 <- as.character(iso3)
  tibble::tibble(
    iso3 = iso3,
    opposite_bloc_basis = dplyr::case_when(
      iso3 %in% in_bloc ~ "bloc",
      iso3 %in% has_value ~ "nonaligned",
      TRUE ~ "no_alignment"
    )
  )
}

# Strategic Need cells and their long inputs table.
#   es_contrib       energy_security_variable_contributions (index_outputs.rds): Country, tech,
#                    supply_chain, category, variable, component_value (higher = more secure), imputed
#   import_exposure  sp_import_exposure() output (importer_iso, tech, supply_chain, observed,
#                    imports_usd, import_hhi, china_share, ...)
#   cfg_need         config strategic_position.yml `strategic_need`
#   country_iso3     optional (Country, iso3) lookup for the es_contrib names; NULL resolves them
#                    with country_name_to_iso3(). Names without a code (aggregates) are dropped and
#                    listed in attr(result, "dropped_countries").
#   techs            technologies to score (NULL: every technology with a measure)
#   fuel_techs, immaterial_families, immaterial_need, min_material_importers, exclude_iso3,
#   nonaligned_opposite_bloc, deployment_fuel_families: the rules in the header. A non-NULL
#                    argument wins, then cfg_need (fuel_import_techs, immaterial_families,
#                    immaterial_need, min_material_importers_per_cell, exclude_iso3,
#                    nonaligned_opposite_bloc), then the SP_NEED_* defaults.
#   geo_alignment    geo_alignment_tbl (reporter_iso, partner_iso, geo_alignment), needed by
#                    nonaligned_opposite_bloc = "na". Without it the rule cannot be applied: a
#                    warning, every opposite_bloc_share kept as measured, sn_opposite_bloc_basis NA.
# The country universe is the countries of es_contrib less exclude_iso3. Returns list(cells, inputs):
#   cells   one row per Country x tech x supply_chain where any measure applies: the score, its
#           percentile and rank, sn_<family>, sn_n_families, sn_n_measures, sn_basis ("direct",
#           "proxy:Assembly", "shared:Extraction", joined with "+"), sn_thin_cell, sn_thin_reason
#           (few_countries, few_material_importers), sn_exclusion ("entrepot" for exclude_iso3 rows,
#           which carry the import context and the cell-level fields but no score; NA otherwise),
#           sn_opposite_bloc_basis (sp_need_opposite_bloc_basis(); NA without geo_alignment or when
#           the rule is off), sn_cell_countries, and the import exposure of the stage the score
#           reads (import_exposure_stage: the cell's own stage, Assembly at Deployment) with
#           sn_import_material and sn_cell_material_importers (countries of the cell whose imports
#           reach min_import_usd). opposite_bloc_share is NA where it does not apply.
#   inputs  one row per scored Country x cell x applicable measure (the evidence layer): family,
#           pillar (ES or SN), category, variable, source_stage, value (as used: the ES index value,
#           or the import measure after the immateriality rule; NA where not applicable), immaterial,
#           observed, applies (FALSE for a nonaligned importer's opposite_bloc_share),
#           need_percentile, weight (equal family and within-family weights over the measures that
#           apply to the country in the cell, summing to 1 per country-cell; 0 where applies is
#           FALSE), score_weight (the same over the rows that enter the score) and evidence_status
#           (exact: import measures at a stage with basket codes; proxy: Deployment rows taken from
#           Assembly; shared: fuel Deployment rows taken from Extraction; NA: ES rows at their own
#           stage, which data/reference/index_variable_stage_evidence.csv classifies). Excluded
#           economies have no inputs rows.
# attr(result, "dropped_countries"): es_contrib names without an ISO3; "excluded_countries": the
# es_contrib names excluded by exclude_iso3; "nonaligned_iso3": importers whose opposite-bloc
# share was set to NA.
sp_strategic_need <- function(es_contrib,
                              import_exposure,
                              cfg_need,
                              country_iso3 = NULL,
                              techs = NULL,
                              fuel_techs = NULL,
                              immaterial_families = NULL,
                              immaterial_need = NULL,
                              min_material_importers = NULL,
                              exclude_iso3 = NULL,
                              nonaligned_opposite_bloc = NULL,
                              geo_alignment = NULL,
                              deployment_fuel_families = "import_dependence") {
  measures <- sp_need_measures(cfg_need)
  families <- unique(measures$family)
  min_import_usd <- as.numeric(cfg_need$min_import_usd %sp_or% 1e6)
  min_countries <- as.integer(cfg_need$min_countries_per_cell %sp_or% 5L)
  min_material <- as.integer(min_material_importers %sp_or% cfg_need$min_material_importers_per_cell %sp_or% min_countries)
  proxy_stage <- as.character(cfg_need$deployment_proxy_stage %sp_or% "Assembly")
  # yaml lists arrive as character vectors, or as lists when written in block style: unlist both.
  fuel_techs <- as.character(unlist(fuel_techs %sp_or% cfg_need$fuel_import_techs %sp_or% SP_NEED_FUEL_TECHS))
  immaterial_families <- as.character(unlist(
    immaterial_families %sp_or% cfg_need$immaterial_families %sp_or% SP_NEED_IMMATERIAL_FAMILIES
  ))
  immaterial_need <- as.character(immaterial_need %sp_or% cfg_need$immaterial_need %sp_or% "zero")
  if (!immaterial_need %in% c("zero", "tie")) stop("sp_strategic_need(): immaterial_need must be zero or tie.")
  exclude_iso3 <- toupper(as.character(unlist(exclude_iso3 %sp_or% cfg_need$exclude_iso3 %sp_or% SP_NEED_EXCLUDE_ISO3)))
  nonaligned_rule <- as.character(nonaligned_opposite_bloc %sp_or% cfg_need$nonaligned_opposite_bloc %sp_or% "as_is")
  if (length(nonaligned_rule) != 1 || !nonaligned_rule %in% SP_NEED_NONALIGNED_RULES) {
    stop("sp_strategic_need(): nonaligned_opposite_bloc must be one of ", paste(SP_NEED_NONALIGNED_RULES, collapse = ", "), ".")
  }
  has_opposite <- any(measures$source == "import_exposure" & measures$variable == SP_NEED_OPPOSITE_BLOC_VARIABLE)
  apply_nonaligned <- identical(nonaligned_rule, "na") && has_opposite
  if (apply_nonaligned && is.null(geo_alignment)) {
    warning(
      "sp_strategic_need(): nonaligned_opposite_bloc = na needs geo_alignment (geo_alignment_tbl); ",
      "without it nonaligned importers keep their opposite_bloc_share of 0.",
      call. = FALSE
    )
    apply_nonaligned <- FALSE
  }

  need_es <- c("Country", "tech", "supply_chain", "category", "variable", "component_value", "imputed")
  missing <- setdiff(need_es, names(es_contrib))
  if (length(missing) > 0) stop("sp_strategic_need(): es_contrib lacks ", paste(missing, collapse = ", "), ".")
  need_ie <- c("importer_iso", "tech", "supply_chain", "observed", "imports_usd")
  missing <- setdiff(c(need_ie, measures$variable[measures$source == "import_exposure"]), names(import_exposure))
  if (length(missing) > 0) stop("sp_strategic_need(): import_exposure lacks ", paste(missing, collapse = ", "), ".")
  assert_supply_chain_stages(es_contrib$supply_chain, label = "es_contrib supply_chain")
  assert_supply_chain_stages(import_exposure$supply_chain, label = "import_exposure supply_chain")

  # ---- country universe ----
  countries <- sort(unique(as.character(es_contrib$Country)))
  lookup <- if (is.null(country_iso3)) {
    tibble::tibble(Country = countries, iso3 = country_name_to_iso3(countries))
  } else {
    tibble::tibble(Country = as.character(country_iso3$Country), iso3 = as.character(country_iso3$iso3)) %>%
      dplyr::filter(Country %in% countries)
  }
  dropped <- setdiff(countries, lookup$Country[!is.na(lookup$iso3)])
  universe <- lookup %>% dplyr::filter(!is.na(iso3)) %>% dplyr::distinct()
  dup <- universe$iso3[duplicated(universe$iso3)]
  if (length(dup) > 0) {
    stop("sp_strategic_need(): several es_contrib names share an ISO3 code: ",
         paste(universe$Country[universe$iso3 %in% dup], collapse = ", "), ".")
  }
  # Entrepots leave the universe here, before any measure is ranked (rule 5).
  excluded <- universe[universe$iso3 %in% exclude_iso3, , drop = FALSE]
  universe <- universe[!universe$iso3 %in% exclude_iso3, , drop = FALSE]
  basis_tbl <- if (apply_nonaligned) {
    sp_need_opposite_bloc_basis(universe$iso3, geo_alignment)
  } else {
    tibble::tibble(iso3 = universe$iso3, opposite_bloc_basis = NA_character_)
  }
  # Importers whose opposite-bloc share is 0 by construction (rule 6).
  opposite_na_iso3 <- basis_tbl$iso3[basis_tbl$opposite_bloc_basis %in% c("nonaligned", "no_alignment")]

  # ---- measure values at their own stage ----
  es_m <- measures[measures$source == "es_contributions", , drop = FALSE]
  es_rows <- es_contrib %>%
    dplyr::transmute(
      Country = as.character(Country), tech = as.character(tech), supply_chain = as.character(supply_chain),
      category = as.character(category), variable = as.character(variable),
      value = as.numeric(component_value), imputed = as.logical(imputed)
    ) %>%
    dplyr::inner_join(es_m[c("family", "category", "variable", "fill")], by = c("category", "variable")) %>%
    dplyr::inner_join(universe, by = "Country")
  dup_es <- duplicated(es_rows[c("Country", "tech", "supply_chain", "category", "variable")])
  if (any(dup_es)) {
    stop("sp_strategic_need(): es_contrib has more than one row per Country x cell for ",
         paste(unique(es_rows$variable[dup_es]), collapse = ", "), ".")
  }
  # An imputed pillar value is not an observation of the country: dropped (fill na), or a source
  # zero where the config says absence is real (structural_zero).
  es_rows <- es_rows %>%
    dplyr::mutate(
      imputed = dplyr::coalesce(imputed, FALSE),
      observed = !imputed & !is.na(value),
      value = dplyr::case_when(!imputed ~ value, fill == "structural_zero" ~ 0, TRUE ~ NA_real_),
      immaterial = FALSE
    ) %>%
    dplyr::select(Country, iso3, tech, supply_chain, family, category, variable, value, observed, immaterial)

  ie_m <- measures[measures$source == "import_exposure", , drop = FALSE]
  ie <- import_exposure %>%
    dplyr::mutate(iso3 = as.character(importer_iso), tech = as.character(tech), supply_chain = as.character(supply_chain)) %>%
    dplyr::inner_join(universe, by = "iso3")
  ie_rows <- lapply(seq_len(nrow(ie_m)), function(i) {
    v <- ie_m$variable[i]
    val <- as.numeric(ie[[v]])
    obs <- as.logical(ie$observed) & !is.na(val)
    # Immaterial imports: the concentration and risky-supplier measures carry no need.
    imm <- obs & ie_m$family[i] %in% immaterial_families & ie$imports_usd < min_import_usd
    tibble::tibble(
      Country = ie$Country, iso3 = ie$iso3, tech = ie$tech, supply_chain = ie$supply_chain,
      family = ie_m$family[i], category = ie_m$category[i], variable = v,
      value = dplyr::if_else(imm, 0, dplyr::if_else(obs, val, NA_real_)),
      observed = obs, immaterial = imm
    )
  })
  native <- dplyr::bind_rows(es_rows, dplyr::bind_rows(ie_rows)) %>%
    dplyr::mutate(source_stage = supply_chain)
  if (!is.null(techs)) native <- dplyr::filter(native, tech %in% techs)

  # A measure applies to a cell when the source observes it there for at least one country.
  applicable <- native %>%
    dplyr::filter(observed) %>%
    dplyr::distinct(tech, supply_chain, source_stage, family, category, variable)

  # ---- Deployment: Assembly need as proxy; fuels also take Extraction import dependence ----
  proxy_app <- applicable %>%
    dplyr::filter(supply_chain == proxy_stage) %>%
    dplyr::mutate(supply_chain = "Deployment")
  fuel_app <- applicable %>%
    dplyr::filter(supply_chain == "Extraction", tech %in% fuel_techs, family %in% deployment_fuel_families) %>%
    dplyr::mutate(supply_chain = "Deployment")
  derived_app <- dplyr::bind_rows(proxy_app, fuel_app)
  applicable <- dplyr::bind_rows(applicable, derived_app) %>% dplyr::distinct()
  derived <- native %>%
    dplyr::select(-supply_chain) %>%
    dplyr::inner_join(derived_app, by = c("tech", "source_stage", "family", "category", "variable"))
  long <- dplyr::bind_rows(native, derived)

  # Every universe country gets a row per applicable measure; values the source lacks stay NA.
  grid <- tidyr::expand_grid(universe, applicable)
  long <- grid %>%
    dplyr::left_join(
      dplyr::select(long, Country, tech, supply_chain, source_stage, family, category, variable, value, observed, immaterial),
      by = c("Country", "tech", "supply_chain", "source_stage", "family", "category", "variable")
    ) %>%
    dplyr::left_join(measures[c("family", "category", "variable", "pillar", "orient", "source")], by = c("family", "category", "variable")) %>%
    dplyr::mutate(
      # A nonaligned importer's opposite-bloc share is not applicable: not a value of 0 to rank,
      # not a missing observation either (rule 6).
      applies = !(source == "import_exposure" & variable == SP_NEED_OPPOSITE_BLOC_VARIABLE & iso3 %in% opposite_na_iso3),
      value = dplyr::if_else(applies, value, NA_real_),
      observed = applies & dplyr::coalesce(observed, FALSE),
      immaterial = applies & dplyr::coalesce(immaterial, FALSE),
      enters = !is.na(value)
    ) %>%
    dplyr::select(-"source")

  # ---- percentiles within the cell, oriented so higher = more need ----
  long <- long %>%
    dplyr::group_by(tech, supply_chain, source_stage, family, category, variable) %>%
    dplyr::mutate(need_percentile = sp_midrank_pct(dplyr::if_else(orient == "invert", -value, value))) %>%
    dplyr::ungroup()
  if (identical(immaterial_need, "zero")) long$need_percentile[long$immaterial] <- 0

  # Weights: equal over families, then equal over measures within a family. `weight` spreads over
  # the cell's applicable measures (what the evidence layer measures coverage against), less those
  # that do not apply to the country (a nonaligned importer's opposite-bloc share: weight 0, so its
  # China share carries the family); `score_weight` over the measures that entered its score.
  long <- long %>%
    dplyr::group_by(Country, tech, supply_chain) %>%
    dplyr::mutate(
      n_fam_app = dplyr::n_distinct(family[applies]),
      n_fam_obs = dplyr::n_distinct(family[enters])
    ) %>%
    dplyr::group_by(Country, tech, supply_chain, family) %>%
    dplyr::mutate(
      weight = dplyr::if_else(applies, 1 / (n_fam_app * sum(applies)), 0),
      score_weight = dplyr::if_else(enters, 1 / (n_fam_obs * sum(enters)), 0)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      evidence_status = dplyr::case_when(
        supply_chain == "Deployment" & source_stage == proxy_stage ~ "proxy",
        supply_chain == "Deployment" & source_stage == "Extraction" ~ "shared",
        pillar == "SN" ~ "exact",
        TRUE ~ NA_character_
      ),
      basis = dplyr::case_when(
        source_stage == supply_chain ~ "direct",
        TRUE ~ paste0(dplyr::if_else(evidence_status == "proxy", "proxy:", "shared:"), source_stage)
      )
    )

  # ---- family and cell scores ----
  fam <- long %>%
    dplyr::group_by(Country, iso3, tech, supply_chain, family) %>%
    dplyr::summarise(score = sp_mean_available(need_percentile), .groups = "drop")
  basis_levels <- c("direct", paste0("proxy:", proxy_stage), "shared:Extraction")
  per_cell <- long %>%
    dplyr::group_by(Country, iso3, tech, supply_chain) %>%
    dplyr::summarise(
      sn_n_measures = sum(enters),
      sn_basis = {
        b <- unique(basis[enters])
        if (length(b) == 0) NA_character_ else paste(b[order(match(b, basis_levels))], collapse = "+")
      },
      .groups = "drop"
    )
  wide <- fam %>%
    dplyr::mutate(family = paste0("sn_", family)) %>%
    tidyr::pivot_wider(names_from = family, values_from = score)
  for (f in paste0("sn_", families)) if (!f %in% names(wide)) wide[[f]] <- NA_real_
  fam_cols <- paste0("sn_", families)

  # ---- import exposure context (the stage the score reads) ----
  ctx_cols <- c("imports_usd", "imports_to_gdp", "n_suppliers", "top_supplier_iso3", "top_supplier_share",
                "import_hhi", "china_share", "opposite_bloc_share")
  ctx <- import_exposure %>%
    dplyr::transmute(
      iso3 = as.character(importer_iso), tech = as.character(tech), import_exposure_stage = as.character(supply_chain),
      dplyr::across(dplyr::any_of(ctx_cols))
    )
  for (cc in setdiff(ctx_cols, names(ctx))) ctx[[cc]] <- NA

  cells <- wide %>%
    dplyr::mutate(
      strategic_need_score = sp_row_mean_available(as.data.frame(wide)[fam_cols]),
      sn_n_families = rowSums(!is.na(as.data.frame(wide)[fam_cols]))
    ) %>%
    dplyr::left_join(per_cell, by = c("Country", "iso3", "tech", "supply_chain")) %>%
    dplyr::mutate(import_exposure_stage = dplyr::if_else(supply_chain == "Deployment", proxy_stage, supply_chain)) %>%
    dplyr::left_join(ctx, by = c("iso3", "tech", "import_exposure_stage")) %>%
    dplyr::mutate(
      import_exposure_stage = dplyr::if_else(is.na(imports_usd), NA_character_, import_exposure_stage),
      sn_import_material = dplyr::if_else(is.na(imports_usd), NA, imports_usd >= min_import_usd),
      legacy_stage = supply_chain_stage_rollup(supply_chain)
    ) %>%
    # Thin cell: too few scored countries, or (where trade data exist) too few countries that import
    # the basket in material amounts. In the second case the import measures rank zeros and the
    # cell ranks countries on noise: Nuclear Recovery (0 material importers), Wind Extraction (1)
    # and Nuclear Extraction (2), where Australia, a uranium exporter that imports none, would rank
    # at the 0.92 need percentile (2026-10-27).
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(
      sn_cell_countries = sum(!is.na(strategic_need_score)),
      sn_cell_material_importers = sum(sn_import_material, na.rm = TRUE),
      sn_thin_reason = dplyr::case_when(
        sn_cell_countries < min_countries ~ "few_countries",
        any(!is.na(imports_usd)) & sn_cell_material_importers < min_material ~ "few_material_importers",
        TRUE ~ NA_character_
      ),
      sn_thin_cell = !is.na(sn_thin_reason),
      strategic_need_percentile = if (dplyr::first(sn_thin_cell)) NA_real_ else sp_midrank_pct(strategic_need_score),
      strategic_need_rank = if (dplyr::first(sn_thin_cell)) NA_integer_ else sp_rank_desc(strategic_need_score)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(dplyr::rename(basis_tbl, sn_opposite_bloc_basis = "opposite_bloc_basis"), by = "iso3") %>%
    dplyr::mutate(
      # The context column says the same as the score: not applicable, not zero.
      opposite_bloc_share = dplyr::if_else(iso3 %in% opposite_na_iso3, NA_real_, as.numeric(opposite_bloc_share)),
      sn_exclusion = NA_character_
    )

  # Excluded economies keep a row per cell (the import context and the cell-level fields, which
  # they did not take part in) so the table says why they have no score.
  if (nrow(excluded) > 0 && nrow(cells) > 0) {
    cell_level <- dplyr::distinct(
      cells, tech, supply_chain, legacy_stage, sn_thin_cell, sn_thin_reason, sn_cell_countries, sn_cell_material_importers
    )
    excluded_rows <- tidyr::expand_grid(excluded, cell_level) %>%
      dplyr::mutate(import_exposure_stage = dplyr::if_else(supply_chain == "Deployment", proxy_stage, supply_chain)) %>%
      dplyr::left_join(ctx, by = c("iso3", "tech", "import_exposure_stage")) %>%
      dplyr::mutate(
        import_exposure_stage = dplyr::if_else(is.na(imports_usd), NA_character_, import_exposure_stage),
        sn_import_material = dplyr::if_else(is.na(imports_usd), NA, imports_usd >= min_import_usd),
        opposite_bloc_share = as.numeric(opposite_bloc_share),
        sn_exclusion = "entrepot"
      )
    cells <- dplyr::bind_rows(cells, excluded_rows)
  }

  cells <- cells %>%
    dplyr::select(
      Country, iso3, tech, supply_chain, legacy_stage,
      strategic_need_score, strategic_need_percentile, strategic_need_rank,
      dplyr::all_of(fam_cols), sn_n_families, sn_n_measures, sn_basis, sn_thin_cell, sn_thin_reason, sn_exclusion,
      sn_opposite_bloc_basis, sn_cell_countries,
      import_exposure_stage, imports_usd, imports_to_gdp, sn_import_material, sn_cell_material_importers, n_suppliers,
      top_supplier_iso3, top_supplier_share, import_hhi, china_share, opposite_bloc_share
    ) %>%
    dplyr::arrange(tech, factor(supply_chain, levels = SUPPLY_CHAIN_STAGES), strategic_need_rank, Country)

  inputs <- long %>%
    dplyr::select(
      Country, iso3, tech, supply_chain, family, pillar, category, variable, source_stage,
      value, immaterial, observed, applies, need_percentile, weight, score_weight, evidence_status
    ) %>%
    dplyr::arrange(Country, tech, factor(supply_chain, levels = SUPPLY_CHAIN_STAGES), family, variable, source_stage)

  out <- list(cells = cells, inputs = inputs)
  attr(out, "dropped_countries") <- dropped
  attr(out, "excluded_countries") <- excluded$Country
  attr(out, "nonaligned_iso3") <- sort(opposite_na_iso3)
  out
}
