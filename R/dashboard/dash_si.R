# Dashboard L3: the Strategic Index for any weights, stage weights and pillar / policy inputs
# (pure; no IO).
#
# Exact port of build_strategic_index() (R/indices/build_indices.R) plus scripts/20's rule that
# rebuilds the index without the sectors in which a pillar is constant across countries
# (strategic_index_degenerate_sectors()):
#
#   SI_its = w_pol pol_its + w_eo EO_its + w_need (1 - ES_its) + w_sc lambda_s + w_tech g_t
#
# over the cells with EO, ES and Policy observed and both pillar cells rankable; pol is the
# within-country median S-curve of the Policy index over the country's in-sample sectors. Fossil
# exclusion (greenness) drops the Coal, Oil and Gas sectors before the S-curves, exactly as the
# pipeline's excluded_sectors does.
#
# Each cell also carries the country's policy_basis (dash_policy(): "IEA PAMS + CAT" or "IEA PAMS
# only"; NA where the cell has no Policy value) and the endowment screen of the allied picks (the
# pipeline's flag_strategic_resource_base()): resource_base is TRUE / FALSE in a screened Upstream
# (tech, stage) of bundle$si$resource_base (strategic_resource_base(): recorded reserves or
# production of the stage's resources) and NA elsewhere; pick_exclusion is "no resource base" where
# it is FALSE. The index itself is unchanged; dash_allied_selection() drops those cells when
# params$si$require_resource_base (the default) is TRUE.

# Row-wise "; "-join of the non-NA reasons (vectorised paste_reasons()).
dash_paste_reasons <- function(...) {
  parts <- list(...)
  out <- rep(NA_character_, length(parts[[1]]))
  for (r in parts) {
    out <- ifelse(is.na(out), r, ifelse(is.na(r), out, paste(out, r, sep = "; ")))
  }
  out
}

# Median S-curve within groups over the rows flagged `keep` (NA elsewhere): scurve_in_sample() by
# group.
dash_scurve_by <- function(x, keep, group) {
  out <- rep(NA_real_, length(x))
  ok <- keep & !is.na(x)
  if (!any(ok)) return(out)
  idx <- split(which(ok), group[ok])
  for (g in idx) out[g] <- median_scurve(x[g])
  out
}

# One pass of build_strategic_index().
dash_si_build <- function(eo, es, pol, tech_ghg, trl, weights, supply_chain_weights, excluded_sectors) {
  key_eo <- paste(eo$Country, eo$tech, eo$supply_chain, sep = "\r")
  m_es <- match(key_eo, paste(es$Country, es$tech, es$supply_chain, sep = "\r"))
  m_pol <- match(key_eo, paste(pol$Country, pol$tech, pol$supply_chain, sep = "\r"))
  ghg <- tech_ghg[!duplicated(tech_ghg$tech), c("tech", "ghg_index")]
  ghg_mean <- mean(ghg$ghg_index, na.rm = TRUE)

  base <- tibble::tibble(
    Country = eo$Country,
    tech = eo$tech,
    supply_chain = eo$supply_chain,
    Economic_Opportunity_Index = eo$value,
    eo_rankable = eo$rankable %in% TRUE,
    eo_rank_exclusion = eo$rank_exclusion,
    Energy_Security_Index = es$value[m_es],
    es_rankable = es$rankable[m_es] %in% TRUE,
    es_rank_exclusion = es$rank_exclusion[m_es],
    value = pol$value[m_pol],
    policy_basis = if ("policy_basis" %in% names(pol)) as.character(pol$policy_basis[m_pol]) else NA_character_,
    ghg_index = ghg$ghg_index[match(eo$tech, ghg$tech)],
    trl_index = if (is.null(trl)) NA_real_ else trl$trl_index[match(key_eo, paste(trl$Country, trl$tech, trl$supply_chain, sep = "\r"))]
  )
  # strategic_pillar_rankability(): a cell flagged not rankable without a reason gets one
  base$eo_rank_exclusion <- ifelse(!base$eo_rankable & is.na(base$eo_rank_exclusion), "flagged not rankable", base$eo_rank_exclusion)
  base$es_rank_exclusion <- ifelse(!base$es_rankable & is.na(base$es_rank_exclusion), "flagged not rankable", base$es_rank_exclusion)

  policy_countries <- unique(pol$Country[!is.na(pol$value)])
  base$sector <- paste(base$tech, base$supply_chain, sep = " - ")
  base <- base[!base$sector %in% excluded_sectors, , drop = FALSE]

  eoi <- base$Economic_Opportunity_Index
  esi <- base$Energy_Security_Index
  in_pol <- base$Country %in% policy_countries
  base$energy_security_need <- 1 - esi
  base$sc_weight <- unname(supply_chain_weights[base$supply_chain])
  base$tech_weight <- ifelse(is.na(base$ghg_index), ghg_mean, base$ghg_index)
  base$si_exclusion <- dash_paste_reasons(
    ifelse(!in_pol, "country not policy-eligible", NA_character_),
    ifelse(in_pol & is.na(base$value), "missing policy index", NA_character_),
    ifelse(is.na(eoi), "missing economic opportunity", NA_character_),
    ifelse(is.na(esi), "missing energy security", NA_character_),
    ifelse(!is.na(eoi) & !base$eo_rankable, paste0("economic opportunity not rankable (", base$eo_rank_exclusion, ")"), NA_character_),
    ifelse(!is.na(esi) & !base$es_rankable, paste0("energy security not rankable (", base$es_rank_exclusion, ")"), NA_character_),
    ifelse(is.na(base$sc_weight), "unknown supply chain stage", NA_character_)
  )
  base$in_sample <- is.na(base$si_exclusion)
  base$pol <- dash_scurve_by(base$value, base$in_sample, base$Country)
  base$eo <- dash_scurve_by(eoi, base$eo_rankable, base$Country)
  base$es_need <- dash_scurve_by(base$energy_security_need, base$es_rankable, base$Country)
  base$es <- base$es_need
  si <- weights[["policy"]] * base$pol +
    weights[["economic_opportunity"]] * base$Economic_Opportunity_Index +
    weights[["energy_security_need"]] * base$energy_security_need +
    weights[["supply_chain"]] * base$sc_weight +
    weights[["technology"]] * base$tech_weight
  base$strategic_index <- ifelse(base$in_sample, si, NA_real_)
  base
}

# strategic_index_degenerate_sectors(): sectors where ES or EO has sd <= tol over >= 2 countries
# among the scored cells.
dash_si_degenerate <- function(cells, tol = 1e-12) {
  x <- cells[!is.na(cells$strategic_index), , drop = FALSE]
  if (!nrow(x)) return(tibble::tibble(sector = character(), n_countries = integer(), es_constant = logical(), eo_constant = logical()))
  sec <- sort(unique(x$sector), method = "radix")
  rows <- lapply(sec, function(s) {
    y <- x[x$sector == s, , drop = FALSE]
    n <- length(unique(y$Country))
    es_sd <- stats::sd(y$Energy_Security_Index)
    eo_sd <- stats::sd(y$Economic_Opportunity_Index)
    tibble::tibble(
      sector = s, n_countries = n,
      es_constant = n > 1 & isTRUE(es_sd <= tol), eo_constant = n > 1 & isTRUE(eo_sd <= tol),
      es_sd = es_sd, eo_sd = eo_sd
    )
  })
  out <- dplyr::bind_rows(rows)
  out[out$es_constant | out$eo_constant, , drop = FALSE]
}

# The endowment screen of the cells (flag_strategic_resource_base()): resource_base TRUE / FALSE in
# the screened (tech, supply_chain) of rb (one row per Country x screened stage: Country, tech,
# supply_chain, resource_base, resource_evidence), a country without a row there having no recorded
# resources (FALSE); NA in the stages rb does not screen, and everywhere when rb is NULL (a bundle
# without the screen, which then screens nothing, as an unflagged table in the pipeline).
dash_si_resource_base <- function(cells, rb) {
  n <- nrow(cells)
  out <- list(resource_base = rep(NA, n), resource_evidence = rep(NA_character_, n), pick_exclusion = rep(NA_character_, n))
  if (is.null(rb) || !nrow(rb) || !n) return(out)
  screened <- paste(cells$tech, cells$supply_chain, sep = "\r") %in% unique(paste(rb$tech, rb$supply_chain, sep = "\r"))
  k <- match(paste(cells$Country, cells$tech, cells$supply_chain, sep = "\r"), paste(rb$Country, rb$tech, rb$supply_chain, sep = "\r"))
  out$resource_base <- ifelse(screened, as.logical(rb$resource_base)[k] %in% TRUE, NA)
  ev <- if ("resource_evidence" %in% names(rb)) as.character(rb$resource_evidence)[k] else rep(NA_character_, n)
  out$resource_evidence <- ifelse(screened, ev, NA_character_)
  out$pick_exclusion <- ifelse(out$resource_base %in% FALSE, "no resource base", NA_character_)
  out
}

# L3. pillars = dash_pillars(), policy = dash_policy(). Returns cells (one row per EO cell of the
# headline technologies, the pipeline's columns plus in_sample, the endowment screen, contributions
# and ranks), excluded (sector, reason), weights, stage_weights and degenerate (the sectors
# scripts/20's rule dropped).
dash_si <- function(bundle, params, pillars, policy) {
  sp <- params$si
  techs <- bundle$dims$techs
  eo <- pillars$eo[pillars$eo$tech %in% techs, , drop = FALSE]
  es <- pillars$es[pillars$es$tech %in% techs, , drop = FALSE]
  pol <- policy$index[policy$index$tech %in% techs, , drop = FALSE]
  trl <- bundle$si$trl
  fossil_sectors <- if (isTRUE(sp$fossil_excluded)) {
    as.vector(outer(sp$fossil_techs, bundle$dims$stages, paste, sep = " - "))
  } else {
    character()
  }
  excl1 <- unique(c(sp$excluded_sectors, fossil_sectors))
  cells <- dash_si_build(eo, es, pol, bundle$si$tech_ghg, trl, sp$weights, sp$stage_weights, excl1)
  degenerate <- dash_si_degenerate(cells)
  excl <- excl1
  if (nrow(degenerate)) {
    excl <- unique(c(excl1, degenerate$sector))
    cells <- dash_si_build(eo, es, pol, bundle$si$tech_ghg, trl, sp$weights, sp$stage_weights, excl)
  }

  w <- sp$weights
  cells$contrib_policy <- w[["policy"]] * cells$pol
  cells$contrib_eo <- w[["economic_opportunity"]] * cells$Economic_Opportunity_Index
  cells$contrib_es_need <- w[["energy_security_need"]] * cells$energy_security_need
  cells$contrib_sc <- w[["supply_chain"]] * cells$sc_weight
  cells$contrib_ghg <- w[["technology"]] * cells$tech_weight
  cells$rank_in_country <- NA_integer_
  cells$n_in_country <- NA_integer_
  ok <- which(!is.na(cells$strategic_index))
  if (length(ok)) {
    for (g in split(ok, cells$Country[ok])) {
      cells$rank_in_country[g] <- as.integer(rank(-cells$strategic_index[g], ties.method = "min"))
      cells$n_in_country[g] <- length(g)
    }
  }
  cmap <- bundle$pillars$eo$cells
  cells$iso3c <- cmap$iso3c[match(cells$Country, cmap$Country)]
  rb <- dash_si_resource_base(cells, bundle$si$resource_base)
  cells$resource_base <- rb$resource_base
  cells$resource_evidence <- rb$resource_evidence
  cells$pick_exclusion <- rb$pick_exclusion
  ord <- order(-cells$strategic_index, method = "radix", na.last = TRUE)
  cells <- cells[ord, c(
    "Country", "iso3c", "tech", "supply_chain", "sector",
    "Economic_Opportunity_Index", "Energy_Security_Index", "energy_security_need",
    "value", "policy_basis", "ghg_index", "trl_index", "eo", "es_need", "es", "pol",
    "sc_weight", "tech_weight", "strategic_index", "si_exclusion", "in_sample",
    "resource_base", "resource_evidence", "pick_exclusion",
    "contrib_policy", "contrib_eo", "contrib_es_need", "contrib_sc", "contrib_ghg",
    "rank_in_country", "n_in_country"
  )]
  reason <- ifelse(
    excl %in% fossil_sectors, "excluded: fossil technology (greenness)",
    ifelse(excl %in% degenerate$sector, "excluded: a pillar is constant across countries", "excluded: not reported (pipeline list)")
  )
  list(
    cells = cells,
    excluded = tibble::tibble(sector = excl, reason = reason),
    weights = w,
    stage_weights = sp$stage_weights,
    degenerate = degenerate
  )
}

# select_allied_strategic_sectors(): each coalition member's top Upstream / Midstream in-sample
# sector (ties kept), outside Geothermal and Green Hydrogen (the selection's excluded_techs). With
# params$si$require_resource_base (the default; the pipeline's require_resource_base) a cell flagged
# resource_base FALSE by dash_si() (an Upstream stage without recorded reserves or production of its
# resources) is not eligible; params = NULL applies the screen. The picks carry the cell's
# policy_basis and its screen flags (resource_base FALSE on a pick only with the screen off).
dash_allied_selection <- function(si, bundle, params = NULL) {
  allies <- bundle$si$allies
  ex_techs <- bundle$manifest$baseline_inputs$allied_excluded_techs
  if (is.null(ex_techs)) ex_techs <- c("Geothermal", "Green Hydrogen")
  screen <- is.null(params) || !isFALSE(params$si$require_resource_base)
  x <- si$cells
  col_or <- function(tbl, nm, fill) if (nm %in% names(tbl)) tbl[[nm]] else rep(fill, nrow(tbl))
  barred <- screen & col_or(x, "resource_base", NA) %in% FALSE
  x <- x[x$Country %in% allies$Country & x$supply_chain %in% c("Midstream", "Upstream") &
           !is.na(x$strategic_index) & !x$tech %in% ex_techs & !barred, , drop = FALSE]
  if (!nrow(x)) {
    return(tibble::tibble(iso3c = character(), Country = character(), tier = character(), network_node = character(),
                          tech = character(), supply_chain = character(), sector = character(), strategic_index = numeric(),
                          policy_basis = character(), resource_base = logical(), pick_exclusion = character()))
  }
  best <- stats::ave(x$strategic_index, x$Country, FUN = max)
  x <- x[x$strategic_index == best, , drop = FALSE]
  a <- allies[match(x$Country, allies$Country), , drop = FALSE]
  out <- tibble::tibble(
    iso3c = a$iso3,
    Country = x$Country,
    tier = a$tier,
    network_node = a$network_node,
    tech = x$tech,
    supply_chain = x$supply_chain,
    sector = x$sector,
    strategic_index = x$strategic_index,
    policy_basis = as.character(col_or(x, "policy_basis", NA_character_)),
    resource_base = as.logical(col_or(x, "resource_base", NA)),
    pick_exclusion = as.character(col_or(x, "pick_exclusion", NA_character_))
  )
  out[order(out$Country, out$sector, method = "radix"), , drop = FALSE]
}
