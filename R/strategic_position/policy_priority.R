# Revealed Policy Priority (docs/strategic_position.md, section 4): where a government concentrates its
# observable intervention across its own technology x six-stage cells. Compared across cells within the
# country, never across countries. Two sources stay apart until the last step:
#
# - IEA PAMS policy records (policy_outputs.rds$policy_clean), put on technologies and stages by the
#   keyword rules of R/categories/policy/iea_policy_index.R;
# - GTA NIPO interventions, put on cells through their HS6 codes (nipo_stage_cells_tbl, built by
#   R/categories/policy/nipo_stage_policy.R).
#
# CAT is not a policy-priority source. It is one country-level climate-policy rating and is carried as
# context (sp_cat_context()).
#
# Pure functions: no IO. Needs R/strategic_position/sp_helpers.R and R/utils/supply_chain_stages.R.

# How a PAMS record reaches the cells (per policy x country):
#   exact / split   names technologies and stages: 1/k of the record to each of the k cells it names
#   tech_wide       names technologies but no stage: counted per technology, never ranked
#   shared          names stages but no technology: counted per stage, never ranked
#   cross_cutting   names neither: counted per country, never ranked
SP_PAMS_LINK_CLASSES <- c("exact", "split", "tech_wide", "shared", "cross_cutting")
SP_PAMS_ATTRIBUTED <- c("exact", "split")

# Jurisdictions whose records are the country's own policy. National, state/provincial and city
# programmes are all public intervention in the country (GTA NIPO counts subnational measures toward the
# country too). "Regional" is a region inside one country or, for the EU's own entry (EUR), an EU act.
# "International" is left out: 86% of those records are one act filed under several countries (EU
# directives under every member, multi-country agreements), and, as with NIPO's EU-level acts, an act
# the country shares is not its own priority. A record filed as International under one country only
# is that country's programme (a cooperation fund, an export-credit line, an EU act under EUR) and
# counts when international_single_country = TRUE.
SP_PAMS_OWN_JURISDICTIONS <- c("National", "Subnational", "Local", "Regional")

# CAT overall rating -> score, the scale partnership_strength_clean_policy() uses (0.25 critically
# insufficient ... 1 almost sufficient). CAT's top grade, "1.5C compatible", is above that scale's top
# and is put on it (1). Keys are lower-case ASCII with the degree sign dropped.
SP_CAT_RATING_SCORES <- c(
  "critically insufficient" = 0.25,
  "highly insufficient" = 0.5,
  "insufficient" = 0.75,
  "almost sufficient" = 1,
  "1.5c compatible" = 1
)

sp_pp_check_cols <- function(tbl, cols, label) {
  if (!inherits(tbl, "data.frame")) stop(label, ": expected a data frame")
  missing <- setdiff(cols, names(tbl))
  if (length(missing) > 0) stop(label, ": missing column(s) ", paste(missing, collapse = ", "))
  invisible(TRUE)
}

# ---- IEA PAMS ---------------------------------------------------------------------------------------

# One row per PAMS policy x country x link, from the expanded policy_clean table (one row per policy x
# country x tech x stage, cross-cutting records copied onto every technology and stage, flagged by
# tech_crosscut / sc_crosscut). The copies are collapsed back to what the record names:
#
#   class          tech       supply_chain  k_links             share
#   exact / split  named      named         cells it names      1 / k (exact when k = 1)
#   tech_wide      named      NA            technologies named  1 / k
#   shared         NA         named         stages named        1 / k
#   cross_cutting  NA         NA            1                   1
#
# k counts every cell the record names, Critical Minerals included, so a record on batteries and their
# minerals gives each half; off_grid marks links to a technology outside `techs` (they reach no cell).
#
# A record entered twice (same country, title, year and links; 15 pairs share even the description) is
# kept once: the most recently modified entry, then the lowest policy_id. Links of records whose
# jurisdiction is not the country's own are kept and flagged (own_policy = FALSE) so they can be
# reported; sp_pams_cells() counts own records only. in_stock: status_norm in stock_status. Pure.
sp_pams_links <- function(policy_clean,
                          techs,
                          stock_status = "In force",
                          own_jurisdictions = SP_PAMS_OWN_JURISDICTIONS,
                          international_single_country = TRUE,
                          dedup = TRUE) {
  required <- c(
    "policy_id", "iso3", "tech", "supply_chain", "tech_crosscut", "sc_crosscut",
    "status_norm", "jurisdiction_norm", "year"
  )
  sp_pp_check_cols(policy_clean, required, "sp_pams_links(policy_clean)")
  optional <- intersect(c("country", "title_text", "tech_source", "dateModified"), names(policy_clean))

  # atomic columns only: policy_clean carries list columns (technologies, tags, policy types)
  rows <- as.data.frame(lapply(policy_clean[c(required, optional)], function(x) unname(x)), stringsAsFactors = FALSE)
  rows <- dplyr::distinct(rows, .data$policy_id, .data$iso3, .data$tech, .data$supply_chain, .keep_all = TRUE)
  rows$class <- dplyr::case_when(
    !rows$tech_crosscut & !rows$sc_crosscut ~ "attributed",
    !rows$tech_crosscut & rows$sc_crosscut ~ "tech_wide",
    rows$tech_crosscut & !rows$sc_crosscut ~ "shared",
    TRUE ~ "cross_cutting"
  )

  record_cols <- intersect(
    c("policy_id", "iso3", "country", "title_text", "year", "status_norm", "jurisdiction_norm", "dateModified"),
    names(rows)
  )
  records <- dplyr::distinct(rows[record_cols], .data$policy_id, .data$iso3, .keep_all = TRUE)
  records <- records %>%
    dplyr::group_by(.data$policy_id) %>%
    dplyr::mutate(n_countries = dplyr::n()) %>%
    dplyr::ungroup()

  # tech_source (IEA technology list vs free text) is per policy x tech: it says how sure the tech is
  tech_source <- if ("tech_source" %in% names(rows)) {
    dplyr::distinct(rows[!rows$tech_crosscut, c("policy_id", "iso3", "tech", "tech_source")],
      .data$policy_id, .data$iso3, .data$tech, .keep_all = TRUE)
  } else {
    NULL
  }

  with_k <- function(tbl, by) {
    tbl %>%
      dplyr::distinct(dplyr::across(dplyr::all_of(by))) %>%
      dplyr::group_by(.data$policy_id, .data$iso3) %>%
      dplyr::mutate(k_links = dplyr::n()) %>%
      dplyr::ungroup()
  }
  att <- with_k(rows[rows$class == "attributed", ], c("policy_id", "iso3", "tech", "supply_chain")) %>%
    dplyr::mutate(link_class = dplyr::if_else(.data$k_links == 1L, "exact", "split"))
  tw <- with_k(rows[rows$class == "tech_wide", ], c("policy_id", "iso3", "tech")) %>%
    dplyr::mutate(supply_chain = NA_character_, link_class = "tech_wide")
  sh <- with_k(rows[rows$class == "shared", ], c("policy_id", "iso3", "supply_chain")) %>%
    dplyr::mutate(tech = NA_character_, link_class = "shared")
  cc <- with_k(rows[rows$class == "cross_cutting", ], c("policy_id", "iso3")) %>%
    dplyr::mutate(tech = NA_character_, supply_chain = NA_character_, link_class = "cross_cutting")

  links <- dplyr::bind_rows(att, tw, sh, cc) %>%
    dplyr::mutate(share = 1 / .data$k_links)
  staged <- links$supply_chain[!is.na(links$supply_chain)]
  assert_supply_chain_stages(staged, label = "sp_pams_links(): policy_clean$supply_chain")

  if (isTRUE(dedup) && "title_text" %in% names(records)) {
    signature <- links %>%
      dplyr::arrange(.data$link_class, .data$tech, .data$supply_chain) %>%
      dplyr::group_by(.data$policy_id, .data$iso3) %>%
      dplyr::summarise(
        .sig = paste(.data$link_class, .data$tech, .data$supply_chain, sep = "|", collapse = ";"),
        .groups = "drop"
      )
    modified <- if ("dateModified" %in% names(records)) records$dateModified else rep(NA_real_, nrow(records))
    keep <- records %>%
      dplyr::mutate(.title = tolower(gsub("\\s+", " ", trimws(.data$title_text))), .modified = modified) %>%
      dplyr::left_join(signature, by = c("policy_id", "iso3")) %>%
      dplyr::arrange(dplyr::desc(dplyr::coalesce(.data$.modified, -Inf)), .data$policy_id) %>%
      dplyr::distinct(.data$iso3, .data$.title, .data$year, .data$.sig, .keep_all = TRUE) %>%
      dplyr::select("policy_id", "iso3")
    records <- dplyr::semi_join(records, keep, by = c("policy_id", "iso3"))
    links <- dplyr::semi_join(links, keep, by = c("policy_id", "iso3"))
  }

  links <- links %>%
    dplyr::left_join(records, by = c("policy_id", "iso3")) %>%
    dplyr::mutate(
      own_policy = .data$jurisdiction_norm %in% own_jurisdictions |
        (isTRUE(international_single_country) & .data$jurisdiction_norm == "International" & .data$n_countries == 1L),
      in_stock = .data$status_norm %in% stock_status,
      off_grid = !is.na(.data$tech) & !.data$tech %in% techs
    )
  if (!is.null(tech_source)) {
    links <- dplyr::left_join(links, tech_source, by = c("policy_id", "iso3", "tech"))
  }
  if ("dateModified" %in% names(links)) {
    # dateModified is epoch milliseconds (the IEA export)
    links$modified_year <- as.integer(format(as.POSIXct(links$dateModified / 1000, origin = "1970-01-01", tz = "UTC"), "%Y"))
    links$dateModified <- NULL
  }

  front <- c(
    "policy_id", "iso3", "country", "title_text", "year", "status_norm", "jurisdiction_norm", "n_countries",
    "own_policy", "in_stock", "link_class", "tech", "supply_chain", "k_links", "share", "off_grid"
  )
  links %>%
    dplyr::select(dplyr::any_of(front), dplyr::everything()) %>%
    dplyr::arrange(.data$iso3, .data$policy_id, match(.data$link_class, SP_PAMS_LINK_CLASSES), .data$tech, .data$supply_chain)
}

# The as-of year for PAMS stock and flow: the year of the extract's latest modification. The record
# `year` cannot serve: it runs to 2030 because some records carry an effective or target year (a 2027
# model-year standard), not the year they were adopted. Takes sp_pams_links() output (modified_year) or
# policy_clean (dateModified, epoch milliseconds). Pure.
sp_pams_as_of_year <- function(tbl) {
  if ("modified_year" %in% names(tbl)) {
    return(max(tbl$modified_year, na.rm = TRUE))
  }
  if ("dateModified" %in% names(tbl)) {
    latest <- max(tbl$dateModified, na.rm = TRUE)
    return(as.integer(format(as.POSIXct(latest / 1000, origin = "1970-01-01", tz = "UTC"), "%Y")))
  }
  stop("sp_pams_as_of_year(): needs modified_year (sp_pams_links()) or dateModified (policy_clean)")
}

# PAMS cell table: one row per country x tech x stage of the headline grid, for every country with at
# least one own PAMS record. Zeros are real where the country has records but none in the cell (PAMS
# lists every record it holds for the country).
#
#   pams_stock                 attributed records in stock (sum of shares); = pams_exact_stock + pams_split_stock
#   pams_tech_list_stock       the part of pams_stock whose technology comes from the IEA technology list
#                              (the rest was matched in the free text, which is less sure)
#   pams_n_stock               distinct stock records with a link to the cell
#   pams_flow_<w>y             attributed records whose year is in the w years ending at as_of_year
#                              (any status: a measure adopted and ended in the window was still adopted)
#   pams_tech_wide_stock       the technology's tech_wide stock (sum of 1/k over the technologies named)
#   pams_shared_stock          the stage's shared stock (sum of 1/k over the stages named)
#   pams_cross_cutting_stock   the country's cross-cutting stock records
#
# The last three are reported, never ranked. flow_status (NULL = all) restricts the flow records. Pure.
sp_pams_cells <- function(links,
                          as_of_year,
                          windows = c(1, 3, 5),
                          techs,
                          stages = SUPPLY_CHAIN_STAGES,
                          flow_status = NULL) {
  sp_pp_check_cols(
    links, c("policy_id", "iso3", "link_class", "tech", "supply_chain", "share", "in_stock", "year", "status_norm"),
    "sp_pams_cells(links)"
  )
  if (length(as_of_year) != 1 || is.na(as_of_year)) stop("sp_pams_cells(): as_of_year must be one year")
  assert_supply_chain_stages(stages, label = "sp_pams_cells(stages)")
  own <- if ("own_policy" %in% names(links)) links[links$own_policy %in% TRUE, ] else links
  keys <- c("iso3", "tech", "supply_chain")

  grid <- tidyr::expand_grid(iso3 = sort(unique(own$iso3)), tech = techs, supply_chain = stages)
  att <- own[own$link_class %in% SP_PAMS_ATTRIBUTED & own$tech %in% techs & own$supply_chain %in% stages, ]

  if (!"tech_source" %in% names(att)) att$tech_source <- NA_character_
  stock <- att[att$in_stock %in% TRUE, ] %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(
      pams_stock = sum(.data$share),
      pams_exact_stock = sum(.data$share[.data$link_class == "exact"]),
      pams_split_stock = sum(.data$share[.data$link_class == "split"]),
      pams_tech_list_stock = sum(.data$share[.data$tech_source %in% "list"]),
      pams_n_stock = dplyr::n_distinct(.data$policy_id),
      .groups = "drop"
    )
  out <- dplyr::left_join(grid, stock, by = keys)

  flow_pool <- if (is.null(flow_status)) att else att[att$status_norm %in% flow_status, ]
  flow_cols <- character()
  for (w in windows) {
    col <- paste0("pams_flow_", w, "y")
    in_window <- !is.na(flow_pool$year) & flow_pool$year > as_of_year - w & flow_pool$year <= as_of_year
    flow <- flow_pool[in_window, ] %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
      dplyr::summarise(.flow = sum(.data$share), .groups = "drop")
    names(flow)[names(flow) == ".flow"] <- col
    out <- dplyr::left_join(out, flow, by = keys)
    flow_cols <- c(flow_cols, col)
  }

  stock_links <- own[own$in_stock %in% TRUE, ]
  tech_wide <- stock_links[stock_links$link_class == "tech_wide" & stock_links$tech %in% techs, ] %>%
    dplyr::group_by(.data$iso3, .data$tech) %>%
    dplyr::summarise(pams_tech_wide_stock = sum(.data$share), .groups = "drop")
  shared <- stock_links[stock_links$link_class == "shared" & stock_links$supply_chain %in% stages, ] %>%
    dplyr::group_by(.data$iso3, .data$supply_chain) %>%
    dplyr::summarise(pams_shared_stock = sum(.data$share), .groups = "drop")
  cross <- stock_links[stock_links$link_class == "cross_cutting", ] %>%
    dplyr::group_by(.data$iso3) %>%
    dplyr::summarise(pams_cross_cutting_stock = dplyr::n_distinct(.data$policy_id), .groups = "drop")

  count_cols <- c(
    "pams_stock", "pams_exact_stock", "pams_split_stock", "pams_tech_list_stock", "pams_n_stock", flow_cols,
    "pams_tech_wide_stock", "pams_shared_stock", "pams_cross_cutting_stock"
  )
  out %>%
    dplyr::left_join(tech_wide, by = c("iso3", "tech")) %>%
    dplyr::left_join(shared, by = c("iso3", "supply_chain")) %>%
    dplyr::left_join(cross, by = "iso3") %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(count_cols), ~ dplyr::coalesce(as.numeric(.x), 0)),
      pams_n_stock = as.integer(.data$pams_n_stock),
      pams_as_of_year = as.integer(as_of_year)
    )
}

# ---- Within-country priority ------------------------------------------------------------------------

# Where a country concentrates a source's records, relative to where countries in general do. Over the
# rows given (the cells the source observes; a missing value is a cell it does not observe and stays
# out):
#
#   attention  a_ic = x_ic / sum_c x_ic                (the country's share of its records in cell c)
#   baseline   b_c  = mean of a_ic over the countries ranked   ("country_mean", the default), or
#                     sum_i x_ic / sum_i sum_c x_ic            ("pooled", every country's records)
#   rca        (a_ic + l) / (b_c + l), l = rca_smoothing / (the country's number of cells);
#              0 where the country has no records in the cell
#   priority   within-country mid-rank percentile of rca ("attention_rca") or of x ("raw")
#   priority_raw   within-country mid-rank percentile of x, always reported
#
# The baseline removes what every country shares: a keyword list that finds Deployment in most
# records, or a source that sees one stage better than another. "country_mean" weighs countries
# equally, so a country's priorities do not move when its own record count is scaled; "pooled" lets the
# largest record holders (the US, the EU) set the baseline. Countries with records in fewer than
# min_cells cells get NA (too few cells to rank) and stay out of the country_mean baseline.
#
# rca_smoothing = 0 is the plain ratio a / b. It explodes where the baseline is tiny: in PAMS a tenth
# of a keyword-split record in Geothermal - Refining (baseline 0.00003) gave the US an RCA of 44, and
# 23 of the 40 top-5 cells of the eight review countries rested on less than one record. Adding l, a
# fraction of the even-spread share 1 / n_cells, to both attention and baseline keeps the ratio where
# the baseline is large and caps it near 1 + a / l where it is tiny. At the default 0.5 no review
# country's top-5 cell rests on less than one record. The smoothing works on shares, so scaling a
# country's records still changes nothing. A cell with no records stays at 0 (the lowest priority): a
# zero is not ranked against other zeros by how common the cell is elsewhere. Columns are added as
# <prefix>_<name> (bare names when prefix is NULL), with n_cells_with_records. Pure.
sp_within_country_priority <- function(tbl,
                                       value_col,
                                       country_col = "iso3",
                                       min_cells = 3,
                                       method = c("attention_rca", "raw"),
                                       cell_cols = c("tech", "supply_chain"),
                                       baseline = c("country_mean", "pooled"),
                                       rca_smoothing = 0.5,
                                       prefix = NULL) {
  method <- match.arg(method)
  baseline <- match.arg(baseline)
  sp_pp_check_cols(tbl, c(value_col, country_col, cell_cols), "sp_within_country_priority(tbl)")
  x <- as.numeric(tbl[[value_col]])
  if (any(x < 0, na.rm = TRUE)) stop("sp_within_country_priority(): ", value_col, " has negative values")

  cell_key <- do.call(paste, c(lapply(tbl[cell_cols], as.character), sep = "\r"))
  work <- data.frame(.row = seq_len(nrow(tbl)), .country = as.character(tbl[[country_col]]), .cell = cell_key, .x = x,
    stringsAsFactors = FALSE)
  work <- work %>%
    dplyr::group_by(.data$.country) %>%
    dplyr::mutate(
      .n_rec = sum(.data$.x > 0, na.rm = TRUE),
      .total = sum(.data$.x, na.rm = TRUE),
      .att = dplyr::if_else(!is.na(.data$.x) & .data$.total > 0, .data$.x / .data$.total, NA_real_)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(.ranked = .data$.n_rec >= min_cells)

  base <- if (baseline == "country_mean") {
    work %>%
      dplyr::filter(.data$.ranked, !is.na(.data$.att)) %>%
      dplyr::group_by(.data$.cell) %>%
      dplyr::summarise(.base = mean(.data$.att), .groups = "drop")
  } else {
    grand <- sum(work$.x, na.rm = TRUE)
    work %>%
      dplyr::filter(!is.na(.data$.x)) %>%
      dplyr::group_by(.data$.cell) %>%
      dplyr::summarise(.base = if (grand > 0) sum(.data$.x) / grand else NA_real_, .groups = "drop")
  }

  work <- work %>%
    dplyr::left_join(base, by = ".cell") %>%
    dplyr::group_by(.data$.country) %>%
    dplyr::mutate(.lambda = rca_smoothing / sum(!is.na(.data$.x))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      # zero attention is the lowest priority whatever the baseline (a cell no one has records in
      # has a zero baseline and zero attention everywhere)
      .rca = dplyr::case_when(
        is.na(.data$.att) ~ NA_real_,
        .data$.att == 0 ~ 0,
        TRUE ~ (.data$.att + .data$.lambda) / (.data$.base + .data$.lambda)
      )
    ) %>%
    dplyr::group_by(.data$.country) %>%
    # ranked on 12 significant digits: sums of 1/k shares and ratios that are equal in exact
    # arithmetic differ in the last bits, and that noise would otherwise break ties
    dplyr::mutate(
      .raw = if (dplyr::first(.data$.ranked)) sp_midrank_pct(signif(.data$.x, 12)) else NA_real_,
      .pr_rca = if (dplyr::first(.data$.ranked)) sp_midrank_pct(signif(.data$.rca, 12)) else NA_real_
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$.row)

  out_names <- c("n_cells_with_records", "attention", "baseline", "rca", "priority", "priority_raw")
  if (!is.null(prefix)) out_names <- paste0(prefix, "_", out_names)
  tbl[[out_names[1]]] <- as.integer(work$.n_rec)
  tbl[[out_names[2]]] <- work$.att
  tbl[[out_names[3]]] <- ifelse(is.na(work$.x), NA_real_, work$.base)
  tbl[[out_names[4]]] <- work$.rca
  tbl[[out_names[5]]] <- if (method == "attention_rca") work$.pr_rca else work$.raw
  tbl[[out_names[6]]] <- work$.raw
  tbl
}

# ---- Combination ------------------------------------------------------------------------------------

# Revealed Policy Priority per country x tech x stage, over the full headline grid of every country
# either source covers. Returns list(cells, inputs).
#
# cells, per source s in {pams, nipo} (NIPO: all counted measures; support and protection separately):
#   <s>_priority, <s>_priority_raw, <s>_rca    sp_within_country_priority() of the stock
#   policy_support_priority, policy_protection_priority   NIPO support / protection stock
#   nipo_priority_iw                            NIPO instrument-weighted stock (sensitivity only)
#   policy_priority_score       mean of the available nipo_priority and pams_priority
#   policy_priority_percentile  within-country mid-rank percentile of the score
#   policy_priority_percentile_z   sensitivity: the same from sum(qnorm(priority)) / sqrt(n sources)
#   policy_priority_sources     "nipo+pams", "nipo", "pams" or "none"; pp_n_sources the count
#   policy_priority_source_gap  nipo_priority - pams_priority where both exist
#   <s>_momentum_<w>y, policy_momentum_<w>y   the same construction on flow over the w-year window;
#                               policy_momentum, nipo_momentum, pams_momentum are the default window
#   pams_cell_status, nipo_cell_status          ranked, below_min_cells (the source has the country but
#                               too few cells with records) or no_data (the source does not observe it)
#   policy_data_coverage        share of the two sources that observe the cell (0, 0.5, 1)
#   NIPO descriptive columns, prefixed nipo_ (support_stock and protection_stock become
#   policy_support_stock and policy_protection_stock); the PAMS cell columns as built.
#
# NIPO observes only the cells its rows cover (no Deployment; no Geothermal or Green Hydrogen basket):
# elsewhere it is no_data, not zero, and its percentiles run over its own cells. So a Deployment score
# rests on PAMS alone, a single percentile, while most other cells average two; the average of two is
# pulled toward the middle, so single-source cells reach the extremes of policy_priority_percentile more
# often (on the October 2026 data 11% of PAMS-only cells sit in their country's top decile against 4% of
# two-source cells). policy_priority_percentile_z is the variant without that effect.
#
# inputs (the evidence layer), one row per cell x source x evidence status with mass > 0: value = the
# policy signal touching the cell (PAMS: exact and split attributed stock, the technology's tech_wide
# stock / number of stages, the stage's shared stock / number of headline techs; NIPO: attributed stock,
# status nipo_evidence_status), weight = its share of the cell's score (each source in the score takes
# an equal part, divided by mass; 0 for a source not in the score). A cell no policy signal touches has
# no rows.
#
# pams_cells: sp_pams_cells() output, or sp_pams_links() output (then built here with as_of_year =
# cfg_pp$pams$as_of_year, else sp_pams_as_of_year()). nipo_cells: the NIPO stage cell table or NULL.
# cfg_pp: the policy_priority block of config/strategic_position.yml (min_cells_with_records,
# normalisation, flow_window_years; baseline and rca_smoothing when present, else the arguments or
# "country_mean" and 0.5). Pure.
sp_policy_priority <- function(pams_cells,
                               nipo_cells = NULL,
                               cfg_pp = list(),
                               windows = c(1, 3, 5),
                               techs = NULL,
                               stages = SUPPLY_CHAIN_STAGES,
                               baseline = NULL,
                               rca_smoothing = NULL,
                               nipo_evidence_status = "split") {
  min_cells <- cfg_pp$min_cells_with_records %||% 3
  method <- cfg_pp$normalisation %||% "attention_rca"
  baseline <- baseline %||% cfg_pp$baseline %||% "country_mean"
  rca_smoothing <- rca_smoothing %||% cfg_pp$rca_smoothing %||% 0.5
  default_w <- cfg_pp$flow_window_years %||% 3
  if (!default_w %in% windows) stop("sp_policy_priority(): the default flow window ", default_w, " is not in windows")
  keys <- c("iso3", "tech", "supply_chain")

  if ("link_class" %in% names(pams_cells)) {
    if (is.null(techs)) stop("sp_policy_priority(): pass techs when pams_cells are sp_pams_links() output")
    as_of <- cfg_pp$pams$as_of_year %||% sp_pams_as_of_year(pams_cells)
    pams_cells <- sp_pams_cells(pams_cells, as_of_year = as_of, windows = windows, techs = techs, stages = stages)
  }
  sp_pp_check_cols(pams_cells, c(keys, "pams_stock", paste0("pams_flow_", windows, "y")), "sp_policy_priority(pams_cells)")
  techs <- techs %||% sort(unique(pams_cells$tech))
  pams_cells <- pams_cells[pams_cells$tech %in% techs & pams_cells$supply_chain %in% stages, ]

  has_nipo <- !is.null(nipo_cells) && nrow(nipo_cells) > 0
  if (has_nipo) {
    sp_pp_check_cols(nipo_cells, c(keys, "support_stock", "protection_stock", "all_stock", paste0("all_flow_", windows, "y")),
      "sp_policy_priority(nipo_cells)")
    nipo_cells <- nipo_cells[nipo_cells$tech %in% techs & nipo_cells$supply_chain %in% stages, ]
    if (anyDuplicated(nipo_cells[keys])) stop("sp_policy_priority(): nipo_cells has duplicate cells")
    assert_supply_chain_stages(nipo_cells$supply_chain, label = "sp_policy_priority(nipo_cells)")
  }
  if (anyDuplicated(pams_cells[keys])) stop("sp_policy_priority(): pams_cells has duplicate cells")

  # one source measure -> its priority columns, keyed by cell
  rank_measure <- function(tbl, col, name, extra = FALSE) {
    r <- sp_within_country_priority(tbl, col,
      min_cells = min_cells, method = method, baseline = baseline, rca_smoothing = rca_smoothing, prefix = ".m"
    )
    out <- r[keys]
    out[[name]] <- r$.m_priority
    if (extra) {
      out[[paste0(name, "_raw")]] <- r$.m_priority_raw
      out[[sub("_priority$", "_rca", name)]] <- r$.m_rca
    }
    out
  }

  countries <- sort(unique(c(pams_cells$iso3, if (has_nipo) nipo_cells$iso3)))
  cells <- tidyr::expand_grid(iso3 = countries, tech = techs, supply_chain = stages)

  # PAMS
  pams_in <- pams_cells %>% dplyr::mutate(.pams_row = TRUE)
  cells <- cells %>%
    dplyr::left_join(pams_in, by = keys) %>%
    dplyr::left_join(rank_measure(pams_cells, "pams_stock", "pams_priority", extra = TRUE), by = keys)
  for (w in windows) {
    cells <- dplyr::left_join(cells, rank_measure(pams_cells, paste0("pams_flow_", w, "y"), paste0("pams_momentum_", w, "y")), by = keys)
  }

  # NIPO
  nipo_measures <- c(
    policy_support_priority = "support_stock",
    policy_protection_priority = "protection_stock",
    nipo_priority = "all_stock",
    nipo_priority_iw = "all_stock_iw"
  )
  if (has_nipo) {
    renamed <- nipo_cells
    other <- setdiff(names(renamed), keys)
    new_names <- paste0("nipo_", other)
    new_names[other == "support_stock"] <- "policy_support_stock"
    new_names[other == "protection_stock"] <- "policy_protection_stock"
    names(renamed)[match(other, names(renamed))] <- new_names
    renamed$.nipo_row <- TRUE
    cells <- dplyr::left_join(cells, renamed, by = keys)
    for (nm in names(nipo_measures)) {
      col <- nipo_measures[[nm]]
      if (!col %in% names(nipo_cells)) {
        cells[[nm]] <- NA_real_
        next
      }
      cells <- dplyr::left_join(cells, rank_measure(nipo_cells, col, nm, extra = nm == "nipo_priority"), by = keys)
    }
    for (w in windows) {
      cells <- dplyr::left_join(cells, rank_measure(nipo_cells, paste0("all_flow_", w, "y"), paste0("nipo_momentum_", w, "y")), by = keys)
    }
  } else {
    cells$.nipo_row <- NA
    for (nm in c(names(nipo_measures), "nipo_priority_raw", "nipo_rca", paste0("nipo_momentum_", windows, "y"))) {
      cells[[nm]] <- NA_real_
    }
    cells$policy_support_stock <- NA_real_
    cells$policy_protection_stock <- NA_real_
    cells$nipo_all_stock <- NA_real_
  }

  within_pct <- function(iso3, x) {
    stats::ave(x, iso3, FUN = sp_midrank_pct)
  }
  source_status <- function(row_present, priority) {
    dplyr::case_when(
      !(row_present %in% TRUE) ~ "no_data",
      is.na(priority) ~ "below_min_cells",
      TRUE ~ "ranked"
    )
  }

  cells$policy_priority_score <- sp_row_mean_available(cells[c("nipo_priority", "pams_priority")])
  cells$policy_priority_percentile <- within_pct(cells$iso3, cells$policy_priority_score)
  # Sensitivity: the mean of two percentiles has a narrower spread than one percentile, so cells only
  # one source observes (Deployment, Geothermal, Green Hydrogen) crowd the tails of the score. Summing
  # normal scores over sqrt(n sources) (Stouffer) keeps the spread the same for one source or two.
  z <- cbind(stats::qnorm(cells$nipo_priority), stats::qnorm(cells$pams_priority))
  n_z <- rowSums(!is.na(z))
  z_sum <- ifelse(n_z > 0, rowSums(z, na.rm = TRUE) / sqrt(n_z), NA_real_)
  cells$policy_priority_percentile_z <- within_pct(cells$iso3, z_sum)
  has_n <- !is.na(cells$nipo_priority)
  has_p <- !is.na(cells$pams_priority)
  cells$pp_n_sources <- as.integer(has_n) + as.integer(has_p)
  cells$policy_priority_sources <- dplyr::case_when(
    has_n & has_p ~ "nipo+pams",
    has_n ~ "nipo",
    has_p ~ "pams",
    TRUE ~ "none"
  )
  cells$policy_priority_source_gap <- cells$nipo_priority - cells$pams_priority

  for (w in windows) {
    m <- sp_row_mean_available(cells[c(paste0("nipo_momentum_", w, "y"), paste0("pams_momentum_", w, "y"))])
    cells[[paste0("policy_momentum_", w, "y")]] <- within_pct(cells$iso3, m)
  }
  cells$policy_momentum <- cells[[paste0("policy_momentum_", default_w, "y")]]
  cells$nipo_momentum <- cells[[paste0("nipo_momentum_", default_w, "y")]]
  cells$pams_momentum <- cells[[paste0("pams_momentum_", default_w, "y")]]

  cells$pams_cell_status <- source_status(cells$.pams_row, cells$pams_priority)
  cells$nipo_cell_status <- source_status(cells$.nipo_row, cells$nipo_priority)
  cells$policy_data_coverage <- ((cells$.pams_row %in% TRUE) + (cells$.nipo_row %in% TRUE)) / 2

  inputs <- sp_pp_inputs(cells, n_stages = length(stages), n_techs = length(techs), nipo_evidence_status = nipo_evidence_status)

  front <- c(
    keys,
    "policy_priority_score", "policy_priority_percentile", "policy_priority_percentile_z",
    "policy_priority_sources", "pp_n_sources",
    "policy_priority_source_gap", "policy_momentum", "policy_data_coverage",
    "policy_support_stock", "policy_support_priority", "policy_protection_stock", "policy_protection_priority",
    "nipo_all_stock", "nipo_priority", "nipo_priority_raw", "nipo_rca", "nipo_priority_iw", "nipo_momentum",
    "nipo_cell_status",
    "pams_stock", "pams_priority", "pams_priority_raw", "pams_rca", "pams_momentum", "pams_cell_status",
    paste0("policy_momentum_", windows, "y"), paste0("nipo_momentum_", windows, "y"), paste0("pams_momentum_", windows, "y")
  )
  cells <- cells %>%
    dplyr::select(dplyr::any_of(front), dplyr::everything(), -dplyr::any_of(c(".pams_row", ".nipo_row"))) %>%
    dplyr::arrange(.data$iso3, .data$tech, match(.data$supply_chain, stages))

  list(cells = cells, inputs = inputs)
}

# Evidence rows behind the policy priority of each cell (see sp_policy_priority()). Pure.
sp_pp_inputs <- function(cells, n_stages, n_techs, nipo_evidence_status = "split") {
  keys <- c("iso3", "tech", "supply_chain")
  num <- function(col) if (col %in% names(cells)) as.numeric(cells[[col]]) else rep(NA_real_, nrow(cells))
  piece <- function(source, variable, status, value, in_score) {
    data.frame(
      cells[keys],
      source = source, family = source, variable = variable, evidence_status = status,
      value = value, observed = TRUE, in_score = in_score,
      stringsAsFactors = FALSE
    )
  }
  pams_in <- !is.na(cells$pams_priority)
  nipo_in <- !is.na(cells$nipo_priority)
  long <- dplyr::bind_rows(
    piece("pams", "pams_exact", "exact", num("pams_exact_stock"), pams_in),
    piece("pams", "pams_split", "split", num("pams_split_stock"), pams_in),
    piece("pams", "pams_tech_wide", "tech_wide", num("pams_tech_wide_stock") / n_stages, pams_in),
    piece("pams", "pams_shared", "shared", num("pams_shared_stock") / n_techs, pams_in),
    piece("nipo", "nipo_attributed", nipo_evidence_status, num("nipo_all_stock"), nipo_in)
  )
  long <- long[!is.na(long$value) & long$value > 0, ]
  long %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(keys, "source")))) %>%
    dplyr::mutate(.source_mass = sum(.data$value)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::mutate(.n_sources = dplyr::n_distinct(.data$source[.data$in_score])) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(weight = dplyr::if_else(.data$in_score, .data$value / .data$.source_mass / .data$.n_sources, 0)) %>%
    dplyr::select(-".source_mass", -".n_sources") %>%
    dplyr::arrange(.data$iso3, .data$tech, match(.data$supply_chain, SUPPLY_CHAIN_STAGES), .data$source, .data$variable) %>%
    as.data.frame(stringsAsFactors = FALSE)
}

# ---- CAT context ------------------------------------------------------------------------------------

# Lower-case ASCII key of a CAT rating label ("1.5°C compatible" -> "1.5c compatible").
sp_cat_rating_key <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[^a-z0-9. ]", "", x)
  gsub("\\s+", " ", trimws(x))
}

# One CAT value per country, before any multiplication by a technology's GHG index: cat_context is the
# CAT overall rating on the 0.25-1 scale of partnership_strength_clean_policy(). Country context only;
# it never enters a cell score. Accepts, in order of preference:
#
# 1. the CAT ratings table (the raw "CAT_country ratings data.csv" read by scripts/10, or
#    cat_prepare_ratings() output): column "Overall.rating" or "Overall rating"; "Policies and action"
#    is carried as cat_policies_action when present;
# 2. partnership_strength_clean_policy() output (climate_policy_index);
# 3. the stored policy component table (variable "CAT Policy Index"). That value is already
#    tech-multiplied, value = c*g + (1-c)*(1-g), so c is recovered by inverting it with each
#    technology's ghg_index g (`tech_ghg`, the tech_ghg_tbl that scripts/10 writes): c = (value - 1 + g) /
#    (2g - 1), from the technologies with |2g - 1| >= 0.5. A country whose technologies disagree by more
#    than 1e-6 stops the function.
#
# Returns Country, cat_rating, cat_context, cat_rating_scope (national or EU bloc rating),
# cat_policies_action_rating, cat_policies_action and cat_basis; with keep_iso3 = TRUE also iso3 (when
# the input has iso3 or iso3c). iso3 is left out by default so a join by Country onto a table that
# already has iso3 does not split it into iso3.x / iso3.y. Pure.
sp_cat_context <- function(cat_tbl, tech_ghg = NULL, rating_scores = SP_CAT_RATING_SCORES, country_col = "Country",
                           keep_iso3 = FALSE) {
  sp_pp_check_cols(cat_tbl, country_col, "sp_cat_context(cat_tbl)")
  score_of <- function(label) unname(rating_scores[sp_cat_rating_key(label)])
  label_of <- function(score) {
    labs <- names(rating_scores)[!duplicated(rating_scores)]
    vals <- unname(rating_scores[labs])
    out <- labs[match(round(score, 6), round(vals, 6))]
    ifelse(is.na(out), NA_character_, paste0(toupper(substring(out, 1, 1)), substring(out, 2)))
  }
  iso_col <- intersect(c("iso3", "iso3c"), names(cat_tbl))[1]
  scope_of <- function(tbl) {
    if ("rating_scope" %in% names(tbl)) dplyr::coalesce(as.character(tbl$rating_scope), "national") else rep("national", nrow(tbl))
  }
  rating_col <- intersect(c("Overall.rating", "Overall rating"), names(cat_tbl))[1]
  action_col <- intersect(c("Policies.and.action", "Policies and action"), names(cat_tbl))[1]

  if (!is.na(rating_col)) {
    out <- data.frame(
      Country = as.character(cat_tbl[[country_col]]),
      cat_rating = as.character(cat_tbl[[rating_col]]),
      cat_context = score_of(cat_tbl[[rating_col]]),
      cat_rating_scope = scope_of(cat_tbl),
      cat_policies_action_rating = if (!is.na(action_col)) as.character(cat_tbl[[action_col]]) else NA_character_,
      cat_policies_action = if (!is.na(action_col)) score_of(cat_tbl[[action_col]]) else NA_real_,
      cat_basis = "CAT overall rating (ratings table)",
      stringsAsFactors = FALSE
    )
    if (!is.na(iso_col)) out$iso3 <- toupper(as.character(cat_tbl[[iso_col]]))
  } else if ("climate_policy_index" %in% names(cat_tbl)) {
    out <- data.frame(
      Country = as.character(cat_tbl[[country_col]]),
      cat_rating = label_of(as.numeric(cat_tbl$climate_policy_index)),
      cat_context = as.numeric(cat_tbl$climate_policy_index),
      cat_rating_scope = scope_of(cat_tbl),
      cat_policies_action_rating = NA_character_,
      cat_policies_action = NA_real_,
      cat_basis = "CAT overall rating (climate_policy_index)",
      stringsAsFactors = FALSE
    )
    if (!is.na(iso_col)) out$iso3 <- toupper(as.character(cat_tbl[[iso_col]]))
  } else {
    sp_pp_check_cols(cat_tbl, c("variable", "tech", "value"), "sp_cat_context(cat_tbl)")
    if (is.null(tech_ghg)) {
      stop(
        "sp_cat_context(): the CAT Policy Index component is tech-multiplied (c*g + (1-c)*(1-g)); pass ",
        "tech_ghg (tech, ghg_index) to recover c, or pass the CAT ratings table instead"
      )
    }
    sp_pp_check_cols(tech_ghg, c("tech", "ghg_index"), "sp_cat_context(tech_ghg)")
    comp <- cat_tbl[cat_tbl$variable == "CAT Policy Index", , drop = FALSE]
    comp$g <- tech_ghg$ghg_index[match(comp$tech, tech_ghg$tech)]
    comp <- comp[!is.na(comp$g) & abs(2 * comp$g - 1) >= 0.5 & !is.na(comp$value), , drop = FALSE]
    comp$c_hat <- (comp$value - 1 + comp$g) / (2 * comp$g - 1)
    comp$.bloc <- if ("explanation" %in% names(comp)) grepl("EU bloc rating", comp$explanation, fixed = TRUE) else FALSE
    comp$.iso <- if (!is.na(iso_col)) toupper(as.character(comp[[iso_col]])) else NA_character_
    comp$.country <- as.character(comp[[country_col]])
    out <- comp %>%
      dplyr::group_by(.data$.country) %>%
      dplyr::summarise(
        iso3 = dplyr::first(.data$.iso),
        cat_context = stats::median(.data$c_hat),
        .spread = max(.data$c_hat) - min(.data$c_hat),
        .bloc = any(.data$.bloc),
        .groups = "drop"
      )
    bad <- out$.country[out$.spread > 1e-6]
    if (length(bad) > 0) {
      stop("sp_cat_context(): the recovered CAT rating differs across technologies for ", paste(utils::head(bad, 5), collapse = ", "))
    }
    out <- data.frame(
      Country = out$.country,
      iso3 = out$iso3,
      cat_rating = label_of(out$cat_context),
      cat_context = out$cat_context,
      cat_rating_scope = ifelse(out$.bloc, "EU bloc rating", "national"),
      cat_policies_action_rating = NA_character_,
      cat_policies_action = NA_real_,
      cat_basis = "CAT overall rating recovered from the CAT Policy Index component (value = c*g + (1-c)*(1-g))",
      stringsAsFactors = FALSE
    )
    if (all(is.na(out$iso3))) out$iso3 <- NULL
  }
  out <- out[!is.na(out$Country) & nzchar(out$Country), , drop = FALSE]
  out <- out[!duplicated(out$Country), , drop = FALSE]
  front <- c("Country", "iso3", "cat_rating", "cat_context", "cat_rating_scope", "cat_policies_action_rating",
    "cat_policies_action", "cat_basis")
  rownames(out) <- NULL
  if (!isTRUE(keep_iso3)) front <- setdiff(front, "iso3")
  out[intersect(front, names(out))]
}
