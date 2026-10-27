# CM-FDI: the Cleantech Manufacturing FDI Dataset of the Net Zero Industrial Policy Lab (Johns
# Hopkins University), https://cmfdi.netzeropolicylab.com, released under CC BY 4.0. Cite as the
# site gives it: "Bandara, P. and the Net Zero Industrial Policy Lab. Cleantech Manufacturing FDI
# Dataset, edition 2026.10.05. Johns Hopkins University. https://cmfdi.netzeropolicylab.com".
# It records cross-border greenfield and expansion projects in solar, wind, battery and electric-vehicle
# manufacturing, one row per project: the parent company and the economy of its headquarters
# (home), the destination economy and city, status, years announced and completed, technology,
# industry activity, capital investment (US$ million; blank = undisclosed, not zero), joint-venture
# fields, and a source link per field. Staged as data/raw/cm_fdi_projects.csv; settings in
# config/partnership.yml cm_fdi. Pure functions: scripts/15 reads the file and passes the table in.
#
# Use in the partnership themes (Friendshore only; Opportunity and Development read no investment
# data, and Economic Opportunity does not read CM-FDI):
#   - Investment ties (partnership_strength_cm_fdi_tie_index()). In the cells the database covers,
#     (Solar, Wind, Batteries, Electric Vehicles) x (Components, Assembly), the Friendshore
#     outbound_index is the reporter's project tie to the partner, built from the projects whose
#     parent is headquartered in the reporter (investor = home = reporter i) and that are located
#     in the partner (destination = partner j), instead of i's IMF DIP position in j. Every other
#     cell, Refining included, keeps the IMF measure.
#   - Chinese-parent ownership exposure (cm_fdi_chinese_parent_exposure()). For supplier j and
#     technology t, the share of j's foreign-invested manufacturing in t whose parent is
#     headquartered in China. It is a property of the partner (the same for every reporter) and
#     enters Friendshore as a risk: the component score is 1 - exposure. It measures the
#     parent-company case of common-mode risk, one of the upgrades the NEIS working paper "Allied
#     Industrial Base: Conceptual overview, literature review and analytical design" proposes for
#     the indices (section 7.4, network concentration and common-mode risk). Plants in two supplier
#     countries owned by the same Chinese parents do not diversify supply away from China, so a
#     supplier whose foreign-invested capacity sits with Chinese-headquartered firms scores lower.
#
# Stages (config/partnership.yml cm_fdi stages). The project file has no product or segment field:
# no column tells a cell plant from a module line, a cathode plant or a vehicle plant. A project is
# therefore counted, at its full attribution weight and value, in each configured stage (Components
# and Assembly, the old Midstream), as manufacturing capex without a product is copied onto both in
# the Investment theme (R/categories/investment/investment_momentum.R). Ties and exposure are the
# same in the two cells; the Friendshore index ranks each cell separately.
#
# Projects used by the index measures (cm_fdi_flag_index_use()): manufacturing projects (the
# Industry activity values in manufacturing_activities; R&D, design and logistics projects are kept
# for descriptive use only) whose Project Status is in keep_status (Announced, Under Construction,
# Completed, Operational: the CM-FDI working paper drops Cancelled, Paused and Closed projects) and
# whose Operational field is not in drop_operational ("Ceased operations" marks plants that have
# since closed, among them completed ones), announced in or after announced_from (null = every
# year), with a home economy other than the destination.
#
# Joint ventures: CM-FDI names one parent per project and does not code the JV partner's economy.
# The listed parent is attributed jv_parent_share of a JV project (default 0.5, an equal split, as
# ownership shares are mostly undisclosed) in counts and values. The rest is the local partner's
# domestic capital (Joint Venture Local = 1) or a foreign partner of uncoded headquarters; it is
# left out of both measures rather than guessed from the free-text JV Company field.

# CM-FDI column names this module reads, by internal name.
cm_fdi_columns <- function() {
  c(
    sector = "Sector",
    project_id = "Project ID",
    parent = "Parent company",
    home_country = "Company Country (HQ)",
    status = "Project Status",
    destination_country = "Destination Country",
    destination_city = "Destination city",
    year_announced = "Year Announced",
    year_completed = "Year Completed",
    technology = "Technology",
    activity = "Industry activity",
    capital_usd_m = "Capital Investment (US$m)",
    expansion = "Expansion",
    phase = "Phase",
    joint_venture = "Joint Venture",
    jv_local = "Joint Venture Local",
    jv_local_state = "Joint Venture Local State",
    jv_company = "Joint Venture Company",
    operational = "Operational"
  )
}

# The per-field source columns (one URL, sometimes with a note, per field), by internal name.
cm_fdi_source_columns <- function() {
  c(
    source_parent = "Source (Parent company name and HQ)",
    source_status = "Source (Project Status)",
    source_location = "Source (Project location, country and city)",
    source_amount = "Source (Amount)",
    source_ownership = "Source (Ownership share, where available)",
    source_announced = "Source (Date Announced [MM/DD/YYYY])",
    source_completed = "Source (Date Completed [MM/DD/YYYY])",
    source_activity = "Source (Industry Activity)",
    source_jv = "Source (Joint Venture)"
  )
}

# Settings of config/partnership.yml cm_fdi, with the values used when the block (or a key) is absent.
cm_fdi_default_settings <- function() {
  list(
    investment_ties = TRUE,
    chinese_parent_exposure = TRUE,
    technologies = list(Battery = "Batteries", Solar = "Solar", Wind = "Wind", EV = "Electric Vehicles"),
    # SUPPLY_CHAIN_MANUFACTURING_STAGES (R/utils/supply_chain_stages.R), spelled out so this file
    # sources without the stage helpers (scripts/85); the CM-FDI test checks they agree.
    stages = c("Components", "Assembly"),
    keep_status = c("Announced", "Under Construction", "Completed", "Operational"),
    drop_operational = c("Ceased operations", "Paused"),
    manufacturing_activities = "Manufacturing",
    announced_from = NA_integer_,
    jv_parent_share = 0.5,
    china_hq_iso3 = c("CHN", "HKG", "MAC"),
    china_supplier = "exposed",
    exposure_weighting = "blend",
    min_projects = 2L
  )
}

# The cm_fdi block of config/partnership.yml (a list, or NULL) merged over the defaults and checked.
cm_fdi_resolve_settings <- function(cfg = NULL) {
  out <- cm_fdi_default_settings()
  if (!is.null(cfg)) {
    if (!is.list(cfg)) {
      stop("cm_fdi settings must be a list (config/partnership.yml cm_fdi).")
    }
    if ("stage" %in% names(cfg)) {
      stop(
        "cm_fdi stage (one of Upstream, Midstream, Downstream) was replaced on 2026-10-27 by stages, a list of ",
        "supply-chain stages (config/partnership.yml cm_fdi: stages: [Components, Assembly])."
      )
    }
    unknown <- setdiff(names(cfg), names(out))
    if (length(unknown) > 0) {
      stop("Unknown cm_fdi settings: ", paste(unknown, collapse = ", "), ".")
    }
    for (key in names(cfg)) {
      if (!is.null(cfg[[key]])) out[[key]] <- cfg[[key]]
    }
    # announced_from: null in the YAML means every year.
    if ("announced_from" %in% names(cfg) && is.null(cfg$announced_from)) out$announced_from <- NA_integer_
  }

  for (key in c("investment_ties", "chinese_parent_exposure")) {
    if (!is.logical(out[[key]]) || length(out[[key]]) != 1 || is.na(out[[key]])) {
      stop("cm_fdi ", key, " must be true or false.")
    }
  }
  tech <- unlist(out$technologies)
  if (!is.character(tech) || length(tech) == 0 || is.null(names(tech)) || any(!nzchar(names(tech))) ||
      anyNA(tech) || anyDuplicated(names(tech))) {
    stop("cm_fdi technologies must map each CM-FDI Technology value to one pipeline technology.")
  }
  out$technologies <- tech
  # Every configured stage must be one of the six (PARTNERSHIP_STRENGTH_STAGE_ROLLUP,
  # partnership_strength_helpers.R): the Friendshore cells are keyed by them.
  out$stages <- as.character(unlist(out$stages))
  stage_names <- names(PARTNERSHIP_STRENGTH_STAGE_ROLLUP)
  if (length(out$stages) == 0 || anyNA(out$stages) || anyDuplicated(out$stages) || !all(out$stages %in% stage_names)) {
    stop(
      "cm_fdi stages must list distinct supply-chain stages (", paste(stage_names, collapse = ", "), "); got ",
      if (length(out$stages)) paste(out$stages, collapse = ", ") else "none", "."
    )
  }
  for (key in c("keep_status", "manufacturing_activities")) {
    out[[key]] <- as.character(unlist(out[[key]]))
    if (length(out[[key]]) == 0) stop("cm_fdi ", key, " must list at least one value.")
  }
  out$drop_operational <- as.character(unlist(out$drop_operational))
  out$announced_from <- suppressWarnings(as.integer(out$announced_from))
  if (length(out$announced_from) != 1) {
    stop("cm_fdi announced_from must be a single year or null.")
  }
  share <- suppressWarnings(as.numeric(out$jv_parent_share))
  if (length(share) != 1 || is.na(share) || share <= 0 || share > 1) {
    stop("cm_fdi jv_parent_share must lie in (0, 1].")
  }
  out$jv_parent_share <- share
  out$china_hq_iso3 <- toupper(as.character(unlist(out$china_hq_iso3)))
  if (!"CHN" %in% out$china_hq_iso3) {
    stop("cm_fdi china_hq_iso3 must include CHN.")
  }
  out$china_supplier <- match.arg(as.character(out$china_supplier), c("exposed", "missing"))
  out$exposure_weighting <- match.arg(as.character(out$exposure_weighting), c("blend", "value", "count"))
  out$min_projects <- suppressWarnings(as.integer(out$min_projects))
  if (length(out$min_projects) != 1 || is.na(out$min_projects) || out$min_projects < 1) {
    stop("cm_fdi min_projects must be a whole number of at least 1.")
  }
  out
}

# Parse a text column: trimmed, "" for missing.
cm_fdi_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  stringr::str_squish(x)
}

# Integer years; "", "N/A", "NA" and "TBD" are missing. Anything else that is not a year stops.
cm_fdi_parse_year <- function(x, label) {
  x <- cm_fdi_text(x)
  blank <- toupper(x) %in% c("", "N/A", "NA", "TBD")
  out <- suppressWarnings(as.integer(x))
  bad <- !blank & (is.na(out) | !grepl("^[0-9]{4}$", x))
  if (any(bad)) {
    stop("CM-FDI ", label, " has values that are not years: ", paste(utils::head(unique(x[bad]), 5), collapse = ", "))
  }
  out[blank] <- NA_integer_
  out
}

# Capital investment, US$ million. Blank and "N/A" are undisclosed (NA, never 0); thousands
# separators are dropped. Anything else that is not a number stops.
cm_fdi_parse_capital <- function(x) {
  x <- gsub(",", "", cm_fdi_text(x), fixed = TRUE)
  blank <- toupper(x) %in% c("", "N/A", "NA", "UNDISCLOSED", "NOT DISCLOSED")
  out <- suppressWarnings(as.numeric(x))
  bad <- !blank & is.na(out)
  if (any(bad)) {
    stop("CM-FDI Capital Investment (US$m) has values that are not numbers: ", paste(utils::head(unique(x[bad]), 5), collapse = ", "))
  }
  if (any(out < 0, na.rm = TRUE)) {
    stop("CM-FDI Capital Investment (US$m) has negative values.")
  }
  out[blank] <- NA_real_
  out
}

# 0/1 flags as logical; blank is NA.
cm_fdi_parse_flag <- function(x, label) {
  x <- cm_fdi_text(x)
  bad <- !x %in% c("", "0", "1")
  if (any(bad)) {
    stop("CM-FDI ", label, " must be 0 or 1: ", paste(utils::head(unique(x[bad]), 5), collapse = ", "))
  }
  dplyr::case_when(x == "1" ~ TRUE, x == "0" ~ FALSE, TRUE ~ NA)
}

# The raw CM-FDI table (as read from the CSV, all columns as text or as read) to one typed row per
# project. Country names go through the pipeline's resolver (R/utils/country.R): EU members stay
# individual economies, Hong Kong is HKG, Taiwan TWN. Stops on a missing column, a duplicated
# Project ID, a country name or a Technology value it cannot map.
cm_fdi_clean_projects <- function(raw, technologies = cm_fdi_default_settings()$technologies) {
  if (!inherits(raw, "data.frame")) {
    stop("cm_fdi_clean_projects() expects a data.frame.")
  }
  # A byte-order mark read as part of the first column name (a CSV read without fileEncoding = "UTF-8-BOM").
  names(raw) <- sub(intToUtf8(0xFEFF), "", names(raw), fixed = TRUE)
  cols <- cm_fdi_columns()
  src_cols <- cm_fdi_source_columns()
  missing_cols <- setdiff(c(cols, src_cols), names(raw))
  if (length(missing_cols) > 0) {
    stop("CM-FDI table lacks columns: ", paste(missing_cols, collapse = ", "), ".")
  }
  technologies <- unlist(technologies)

  out <- tibble::tibble(
    project_id = cm_fdi_text(raw[[cols[["project_id"]]]]),
    sector = cm_fdi_text(raw[[cols[["sector"]]]]),
    parent = cm_fdi_text(raw[[cols[["parent"]]]]),
    home_country = cm_fdi_text(raw[[cols[["home_country"]]]]),
    destination_country = cm_fdi_text(raw[[cols[["destination_country"]]]]),
    destination_city = cm_fdi_text(raw[[cols[["destination_city"]]]]),
    status = cm_fdi_text(raw[[cols[["status"]]]]),
    operational = cm_fdi_text(raw[[cols[["operational"]]]]),
    year_announced = cm_fdi_parse_year(raw[[cols[["year_announced"]]]], "Year Announced"),
    year_completed = cm_fdi_parse_year(raw[[cols[["year_completed"]]]], "Year Completed"),
    technology = cm_fdi_text(raw[[cols[["technology"]]]]),
    activity = cm_fdi_text(raw[[cols[["activity"]]]]),
    capital_usd_m = cm_fdi_parse_capital(raw[[cols[["capital_usd_m"]]]]),
    expansion = cm_fdi_parse_flag(raw[[cols[["expansion"]]]], "Expansion"),
    phase = cm_fdi_text(raw[[cols[["phase"]]]]),
    joint_venture = cm_fdi_parse_flag(raw[[cols[["joint_venture"]]]], "Joint Venture"),
    jv_local = cm_fdi_parse_flag(raw[[cols[["jv_local"]]]], "Joint Venture Local"),
    jv_local_state = cm_fdi_parse_flag(raw[[cols[["jv_local_state"]]]], "Joint Venture Local State"),
    jv_company = cm_fdi_text(raw[[cols[["jv_company"]]]])
  )
  for (nm in names(src_cols)) {
    out[[nm]] <- cm_fdi_text(raw[[src_cols[[nm]]]])
  }

  if (any(!nzchar(out$project_id))) {
    stop("CM-FDI table has rows without a Project ID.")
  }
  if (anyDuplicated(out$project_id)) {
    stop("CM-FDI Project IDs are not unique: ", paste(utils::head(unique(out$project_id[duplicated(out$project_id)]), 5), collapse = ", "))
  }

  unmapped_tech <- setdiff(unique(out$technology), names(technologies))
  if (length(unmapped_tech) > 0) {
    stop("CM-FDI Technology values without a pipeline technology (config/partnership.yml cm_fdi technologies): ",
         paste(unmapped_tech, collapse = ", "))
  }
  out$tech <- unname(technologies[out$technology])

  out$home_iso3 <- country_name_to_iso3(out$home_country)
  out$destination_iso3 <- country_name_to_iso3(out$destination_country)
  unmapped <- unique(c(out$home_country[is.na(out$home_iso3)], out$destination_country[is.na(out$destination_iso3)]))
  if (length(unmapped) > 0) {
    stop("CM-FDI country names the resolver (R/utils/country.R) cannot map: ", paste(unmapped, collapse = ", "))
  }
  out$capital_disclosed <- !is.na(out$capital_usd_m)

  dplyr::relocate(out, tech, .after = technology) %>%
    dplyr::relocate(home_iso3, .after = home_country) %>%
    dplyr::relocate(destination_iso3, .after = destination_country) %>%
    dplyr::relocate(capital_disclosed, .after = capital_usd_m)
}

# Flags which projects the index measures use, under the settings of cm_fdi_resolve_settings():
# index_use (logical), index_exclusion (the first rule that excludes the project, NA when used),
# stages (the configured stages the project counts in, "|"-separated: "Components|Assembly"),
# supply_chain, china_parent (home economy in china_hq_iso3) and attribution_weight (1, or
# jv_parent_share for a joint venture). Every project is returned, one row each.
#
# supply_chain holds the roll-up the configured stages share (Midstream for Components and
# Assembly; NA if they span roll-ups), not a stage: a project cannot be placed in one stage, and
# cm_fdi_projects_tbl (scripts/15) stays one row per project, so counting its rows counts
# projects. Its three-stage readers (the Country dashboard export keys its cells on tech and
# supply_chain) keep their key. The per-stage tables (cm_fdi_investment_ties(),
# cm_fdi_chinese_parent_exposure()) expand each project onto its stages (cm_fdi_stage_rows()).
cm_fdi_flag_index_use <- function(projects, settings = cm_fdi_resolve_settings()) {
  require_columns(
    projects,
    c("project_id", "home_iso3", "destination_iso3", "status", "operational", "activity", "year_announced", "tech", "joint_venture"),
    label = "CM-FDI projects"
  )
  s <- settings
  exclusion <- dplyr::case_when(
    !projects$status %in% s$keep_status ~ paste0("status: ", projects$status),
    projects$operational %in% s$drop_operational ~ paste0("operational: ", projects$operational),
    !projects$activity %in% s$manufacturing_activities ~ paste0("activity: ", projects$activity),
    !is.na(s$announced_from) & (is.na(projects$year_announced) | projects$year_announced < s$announced_from) ~
      paste0("announced before ", s$announced_from),
    projects$home_iso3 == projects$destination_iso3 ~ "home economy = destination",
    is.na(projects$tech) ~ "technology not mapped",
    TRUE ~ NA_character_
  )
  projects$index_use <- is.na(exclusion)
  projects$index_exclusion <- exclusion
  projects$stages <- paste(s$stages, collapse = "|")
  rollup <- unique(unname(PARTNERSHIP_STRENGTH_STAGE_ROLLUP[s$stages]))
  projects$supply_chain <- if (length(rollup) == 1) rollup else NA_character_
  projects$china_parent <- projects$home_iso3 %in% s$china_hq_iso3
  projects$attribution_weight <- dplyr::if_else(projects$joint_venture %in% TRUE, s$jv_parent_share, 1)
  projects
}

# The flagged projects with one row per (project, stage) they count in (the `stages` column of
# cm_fdi_flag_index_use()), supply_chain set to that stage. Each stage carries the project's full
# attribution weight and capital investment: copied, not split, because the file cannot say which
# stage a plant serves. A table without `stages` (built by hand) keeps its own supply_chain.
cm_fdi_stage_rows <- function(flagged) {
  if (!"stages" %in% names(flagged)) {
    return(flagged)
  }
  st <- strsplit(as.character(flagged$stages), "|", fixed = TRUE)
  out <- flagged[rep(seq_len(nrow(flagged)), lengths(st)), , drop = FALSE]
  # as.character(): with no project, unlist() is NULL, which would delete the column the ties and
  # exposure builders group on (they return an empty table instead).
  out$supply_chain <- as.character(unlist(st, use.names = FALSE))
  out
}

# The (tech, supply_chain) cells the database covers under `settings`: every mapped technology at
# every configured stage.
cm_fdi_cells <- function(settings = cm_fdi_resolve_settings()) {
  tidyr::expand_grid(tech = unique(unname(settings$technologies)), supply_chain = settings$stages)
}

# Investor -> destination ties per covered cell, from the projects flagged index_use: the
# attributed project count (sum of attribution weights), the attributed count with a disclosed
# value, and the attributed disclosed value in US$ million (NA when no project of the tie
# discloses one: undisclosed is not zero). investor_iso is the parent's home economy,
# destination_iso the project's location. One row per configured stage of the tie
# (cm_fdi_stage_rows()): the Components and Assembly rows of a tie are the same.
cm_fdi_investment_ties <- function(flagged) {
  require_columns(
    flagged,
    c("index_use", "home_iso3", "destination_iso3", "tech", "supply_chain", "attribution_weight", "capital_usd_m"),
    label = "CM-FDI flagged projects"
  )
  cm_fdi_stage_rows(flagged[flagged$index_use, , drop = FALSE]) %>%
    dplyr::group_by(investor_iso = home_iso3, destination_iso = destination_iso3, tech, supply_chain) %>%
    dplyr::summarise(
      n_projects = sum(attribution_weight),
      n_disclosed = sum(attribution_weight[!is.na(capital_usd_m)]),
      value_usd_m = if (any(!is.na(capital_usd_m))) sum((attribution_weight * capital_usd_m)[!is.na(capital_usd_m)]) else NA_real_,
      .groups = "drop"
    )
}

# Chinese-parent ownership exposure of each destination (supplier) in each covered cell: the share
# of its foreign-invested manufacturing whose parent is headquartered in china_hq_iso3 (China,
# with Hong Kong and Macao, whose parents are Chinese companies listed or seated in the SARs).
#
# Weighting (settings$exposure_weighting), with a = attribution weight, v = capital investment:
#   blend (default): value-weighted where values are disclosed, count-based for the rest. The
#     disclosed projects carry their attributed count share D/A of the cell, split among them by
#     value; each undisclosed project carries a/A:
#       exposure = (sum_undisclosed a [china] + D * sum_disclosed a v [china] / sum_disclosed a v) / A,
#     i.e. the undisclosed Chinese count share plus (D / A) times the value share.
#     All disclosed: the value share; none disclosed: the count share. Undisclosed projects are
#     neither dropped nor given an imputed value.
#   value: the value share over disclosed projects; the count share where none is disclosed.
#   count: the attributed count share.
# exposure_basis records which applied ("value", "count", "blend" for a mix).
#
# A destination without a foreign-invested project in the cell has no row (undefined, so the
# component is missing there, not 0), and so has one with fewer than settings$min_projects projects
# (default 2: with a single plant the share is 0 or 1 by construction, so one Chinese battery plant
# in Japan would read as full exposure; 1 keeps every destination with a project). Its counts are
# in n_project_rows (projects) and n_projects (attributed). Destinations in china_hq_iso3: with china_supplier
# "exposed" (default) every covered cell gets exposure 1 (basis "china_hq"): their capacity sits
# with Chinese-headquartered firms by construction, and their foreign-invested plants, which are
# non-Chinese by definition, would otherwise score them as unexposed; with "missing" they have no
# row. Each project counts in every configured stage (cm_fdi_stage_rows()), so a destination's
# Components and Assembly exposures are the same.
cm_fdi_chinese_parent_exposure <- function(flagged, settings = cm_fdi_resolve_settings()) {
  require_columns(
    flagged,
    c("index_use", "destination_iso3", "tech", "supply_chain", "attribution_weight", "capital_usd_m", "china_parent"),
    label = "CM-FDI flagged projects"
  )
  used <- cm_fdi_stage_rows(flagged[flagged$index_use & !flagged$destination_iso3 %in% settings$china_hq_iso3, , drop = FALSE])
  out <- used %>%
    dplyr::mutate(
      disclosed = !is.na(capital_usd_m),
      av = dplyr::if_else(disclosed, attribution_weight * capital_usd_m, 0)
    ) %>%
    dplyr::group_by(destination_iso = destination_iso3, tech, supply_chain) %>%
    dplyr::summarise(
      n_project_rows = dplyr::n(),
      n_projects = sum(attribution_weight),
      n_china = sum(attribution_weight[china_parent]),
      n_disclosed = sum(attribution_weight[disclosed]),
      n_undisclosed_china = sum(attribution_weight[!disclosed & china_parent]),
      value_usd_m = sum(av),
      value_china_usd_m = sum(av[china_parent]),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_project_rows >= settings$min_projects) %>%
    dplyr::mutate(
      share_count = n_china / n_projects,
      share_value = dplyr::if_else(value_usd_m > 0, value_china_usd_m / value_usd_m, NA_real_),
      value_usd_m = dplyr::if_else(n_disclosed > 0, value_usd_m, NA_real_),
      value_china_usd_m = dplyr::if_else(n_disclosed > 0, value_china_usd_m, NA_real_)
    )

  out <- switch(settings$exposure_weighting,
    blend = out %>%
      dplyr::mutate(
        chinese_parent_exposure = dplyr::if_else(
          is.na(share_value),
          share_count,
          (n_undisclosed_china + n_disclosed * share_value) / n_projects
        ),
        exposure_basis = dplyr::case_when(
          is.na(share_value) ~ "count",
          n_disclosed >= n_projects ~ "value",
          TRUE ~ "blend"
        )
      ),
    value = out %>%
      dplyr::mutate(
        chinese_parent_exposure = dplyr::coalesce(share_value, share_count),
        exposure_basis = dplyr::if_else(is.na(share_value), "count", "value")
      ),
    count = out %>%
      dplyr::mutate(chinese_parent_exposure = share_count, exposure_basis = "count")
  )
  out <- dplyr::select(out, -n_undisclosed_china)

  if (identical(settings$china_supplier, "exposed")) {
    china_rows <- tidyr::expand_grid(destination_iso = settings$china_hq_iso3, cm_fdi_cells(settings)) %>%
      dplyr::mutate(chinese_parent_exposure = 1, exposure_basis = "china_hq")
    out <- dplyr::bind_rows(out, china_rows)
  }
  out %>%
    dplyr::filter(tech %in% cm_fdi_cells(settings)$tech, supply_chain %in% settings$stages) %>%
    dplyr::arrange(tech, supply_chain, destination_iso)
}

# Everything the Friendshore builder needs from CM-FDI: the covered cells (none when both switches
# are off), the investor -> destination ties (NULL when settings$investment_ties is FALSE) and the
# exposure table (NULL when settings$chinese_parent_exposure is FALSE), from the cleaned projects of
# cm_fdi_clean_projects(). Also returns the flagged project table and the settings.
partnership_strength_cm_fdi_inputs <- function(projects, settings = cm_fdi_resolve_settings()) {
  flagged <- cm_fdi_flag_index_use(projects, settings)
  list(
    settings = settings,
    projects = flagged,
    cells = cm_fdi_cells(settings),
    ties = if (isTRUE(settings$investment_ties)) cm_fdi_investment_ties(flagged) else NULL,
    exposure = if (isTRUE(settings$chinese_parent_exposure)) cm_fdi_chinese_parent_exposure(flagged, settings) else NULL
  )
}

# The CM-FDI investment-tie index of the dyad rows `dyads` (reporter_iso, partner_iso, tech,
# supply_chain), all in covered cells, from the ties of cm_fdi_investment_ties(). Built like the
# IMF outbound index it replaces there and the two-stage trade index:
#   stage 1, within (tech, stage) over the dyads with a project (reporter = investor, partner =
#     destination): S-curve of the attributed project count and of the attributed disclosed value,
#     averaged over the measures the dyad has (value is missing where no project discloses one);
#   stage 2, within (reporter, tech, stage): S-curve, with the reporter's partners without a
#     project as one tie block below every partner with one (they take the block's mid-rank, the
#     convention of the outbound and trade indices for zeros).
# A reporter whose parents have no project in any partner of the cell gets NA for the whole cell:
# the census records no tie for it, so a constant would carry no ranking information and the
# Friendshore mean renormalizes over its other components. Returns `dyads` with cm_fdi_tie
# (the index), cm_fdi_projects and cm_fdi_value_usd_m (the raw measures, 0 / NA without a project).
partnership_strength_cm_fdi_tie_index <- function(dyads, ties) {
  require_columns(dyads, c("reporter_iso", "partner_iso", "tech", "supply_chain"), label = "dyads")
  require_columns(ties, c("investor_iso", "destination_iso", "tech", "supply_chain", "n_projects", "value_usd_m"), label = "CM-FDI ties")
  # One row per tie, so the join returns one row per dyad in the order of `dyads`
  # (partnership_strength_apply_cm_fdi() assigns the result by position).
  partnership_strength_assert_unique(ties, c("investor_iso", "destination_iso", "tech", "supply_chain"), label = "CM-FDI ties")
  keyed <- dplyr::left_join(
    dyads,
    dplyr::select(ties, reporter_iso = investor_iso, partner_iso = destination_iso, tech, supply_chain,
                  cm_fdi_projects = n_projects, cm_fdi_value_usd_m = value_usd_m),
    by = c("reporter_iso", "partner_iso", "tech", "supply_chain")
  )
  keyed$cm_fdi_projects <- dplyr::coalesce(keyed$cm_fdi_projects, 0)
  keyed %>%
    dplyr::mutate(tied = cm_fdi_projects > 0) %>%
    dplyr::group_by(tech, supply_chain) %>%
    dplyr::mutate(
      count_idx = partnership_strength_safe_scurve(dplyr::if_else(tied, cm_fdi_projects, NA_real_)),
      value_idx = partnership_strength_safe_scurve(dplyr::if_else(tied, cm_fdi_value_usd_m, NA_real_)),
      combined = partnership_strength_row_weighted_mean(list(count_idx, value_idx), c(1, 1)),
      combined = dplyr::if_else(tied, combined, -1)
    ) %>%
    dplyr::group_by(reporter_iso, tech, supply_chain) %>%
    dplyr::mutate(
      investor = any(tied),
      cm_fdi_tie = if (any(tied)) partnership_strength_safe_scurve(combined) else rep(NA_real_, dplyr::n())
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-tied, -count_idx, -value_idx, -combined, -investor)
}
