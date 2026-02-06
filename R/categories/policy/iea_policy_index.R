# IEA PAMS policy index.

if (!exists("median_scurve", mode = "function")) {
  source(file.path(dirname(sys.frame(1)$ofile), "..", "..", "utils", "scurve.R"))
}

strip_html <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- gsub("<[^>]+>", " ", x)
  x <- gsub("&nbsp;|&amp;|&quot;", " ", x)
  stringr::str_squish(x)
}

collapse_chr_listcol <- function(x, sep = " ") {
  vapply(x, function(v) {
    v <- as.character(v)
    v <- v[!is.na(v) & nzchar(v)]
    paste(v, collapse = sep)
  }, character(1))
}

parse_json_array_one <- function(s) {
  s <- as.character(s)
  if (is.na(s) || !nzchar(s) || s == "[]") {
    return(character(0))
  }
  out <- tryCatch(jsonlite::fromJSON(s), error = function(e) NULL)
  if (is.null(out) || !is.character(out)) {
    return(character(0))
  }
  out
}

parse_json_countries_one <- function(s) {
  empty <- tibble::tibble(iso3 = character(0), country = character(0))
  s <- as.character(s)
  if (is.na(s) || !nzchar(s) || s == "[]") {
    return(empty)
  }
  if (!stringr::str_detect(s, "^\\s*\\[")) {
    return(empty)
  }

  out <- tryCatch(jsonlite::fromJSON(s, simplifyDataFrame = TRUE), error = function(e) NULL)
  if (is.null(out) || !is.data.frame(out)) {
    return(empty)
  }

  tibble::tibble(
    iso3 = toupper(as.character(out$iso3)),
    country = stringr::str_squish(as.character(out$name))
  ) %>%
    dplyr::filter(!is.na(iso3), nzchar(iso3))
}

parse_json_array_chr_cached <- function(x) {
  x_chr <- as.character(x)
  uniq <- unique(x_chr)

  parsed <- purrr::map(uniq, parse_json_array_one)
  names(parsed) <- uniq
  parsed[x_chr]
}

parse_json_countries_cached <- function(x) {
  x_chr <- as.character(x)
  uniq <- unique(x_chr)

  parsed <- purrr::map(uniq, parse_json_countries_one)
  names(parsed) <- uniq
  parsed[x_chr]
}

clean_pams_raw <- function(raw) {
  countries_list <- parse_json_countries_cached(raw[["countries"]])
  tech_list <- parse_json_array_chr_cached(raw[["technologies"]])
  tag_list <- parse_json_array_chr_cached(raw[["tags"]])
  ptype_list <- parse_json_array_chr_cached(raw[["policyType"]])

  base <- raw %>%
    dplyr::mutate(
      policy_id = dplyr::row_number(),
      title_text = stringr::str_squish(dplyr::coalesce(.data$title, "")),
      description_text = strip_html(.data$description),
      status = stringr::str_squish(dplyr::coalesce(.data$status, "")),
      jurisdiction = stringr::str_squish(dplyr::coalesce(.data$jurisdiction, "")),
      year = suppressWarnings(as.integer(.data$year)),
      yearEnded = suppressWarnings(as.integer(.data$yearEnded)),
      datePromulgated = stringr::str_squish(dplyr::coalesce(.data$datePromulgated, "")),
      dateModified = suppressWarnings(as.numeric(.data$dateModified)),
      countries_list = countries_list,
      technologies_list = tech_list,
      tags_list = tag_list,
      policyType_list = ptype_list
    ) %>%
    dplyr::mutate(
      text_all = stringr::str_to_lower(stringr::str_squish(paste(
        title_text,
        description_text,
        collapse_chr_listcol(technologies_list),
        collapse_chr_listcol(tags_list),
        collapse_chr_listcol(policyType_list),
        sep = " | "
      )))
    )

  base %>%
    dplyr::select(
      policy_id,
      title_text,
      description_text,
      status,
      jurisdiction,
      year,
      yearEnded,
      source,
      learnMore,
      datePromulgated,
      dateModified,
      text_all,
      technologies_list,
      tags_list,
      policyType_list,
      countries_list
    ) %>%
    tidyr::unnest(countries_list) %>%
    dplyr::mutate(
      iso3 = toupper(.data$iso3),
      country = stringr::str_squish(.data$country)
    )
}

TECH_RULES <- list(
  "Solar" = c("\\bsolar\\b", "\\bpv\\b", "photovoltaic"),
  "Wind" = c("\\bwind\\b", "onshore wind", "offshore wind"),
  "Batteries" = c("\\bbatter(y|ies)\\b", "storage", "utility-scale battery"),
  "Electric Vehicles" = c("\\bev\\b", "electric vehicle", "charging", "charger", "electromobil"),
  "Green Hydrogen" = c("\\bhydrogen\\b", "\\bh2\\b", "electroly(s|z)er", "ammonia"),
  "Electric Grid" = c("\\bgrid\\b", "transmission", "distribution", "interconnector", "substation"),
  "Nuclear" = c("\\bnuclear\\b", "uranium", "reactor", "smr"),
  "Geothermal" = c("\\bgeothermal\\b", "\\begs\\b", "enhanced geothermal"),
  "Oil" = c("\\boil\\b", "petroleum", "crude"),
  "Gas" = c("\\bgas\\b", "lng", "pipeline"),
  "Coal" = c("\\bcoal\\b"),
  "Critical Minerals" = c(
    "critical mineral",
    "lithium",
    "cobalt",
    "nickel",
    "graphite",
    "rare earth",
    "copper",
    "manganese"
  )
)

SUPPLY_CHAIN_RULES <- list(
  "Upstream" = c("mining", "mine", "exploration", "extraction", "ore", "lease", "concession", "drilling"),
  "Midstream" = c(
    "processing",
    "refining",
    "smelt",
    "manufactur",
    "factory",
    "plant",
    "components",
    "supply chain",
    "domestic content"
  ),
  "Downstream" = c(
    "deployment",
    "install",
    "consumer",
    "end-use",
    "adoption",
    "efficiency",
    "demand",
    "charging",
    "grid connection",
    "power plant",
    "generation"
  )
)

match_any <- function(text, patterns) {
  any(purrr::map_lgl(patterns, ~ stringr::str_detect(text, stringr::regex(.x, ignore_case = TRUE))))
}

assign_supply_chain <- function(text_all) {
  hits <- names(purrr::keep(SUPPLY_CHAIN_RULES, ~ match_any(text_all, .x)))
  if (length(hits) == 0) {
    "Cross-cutting"
  } else {
    hits
  }
}

assign_tech_two_stage <- function(technologies_vec, text_all) {
  tech_blob <- stringr::str_to_lower(paste(as.character(technologies_vec), collapse = " | "))
  text_blob <- text_all

  detect_tech_hits <- function(blob) {
    names(purrr::keep(TECH_RULES, function(pats) {
      any(purrr::map_lgl(pats, ~ stringr::str_detect(blob, stringr::regex(.x, ignore_case = TRUE))))
    }))
  }

  hits_list <- if (nzchar(tech_blob)) detect_tech_hits(tech_blob) else character(0)

  if (length(hits_list) > 0) {
    return(list(tech = hits_list, tech_source = "list"))
  }

  hits_text <- detect_tech_hits(text_blob)
  if (length(hits_text) > 0) {
    return(list(tech = hits_text, tech_source = "text"))
  }

  list(tech = "Cross-cutting", tech_source = "none")
}

normalize_policy_type <- function(policyType_vec, text_all) {
  pt <- stringr::str_to_lower(paste(policyType_vec, collapse = " | "))
  if (!nzchar(pt)) {
    pt <- text_all
  }

  dplyr::case_when(
    stringr::str_detect(pt, "tax|subsid|grant|rebate|credit|feed-in|auction|tariff|finance|loan") ~
      "Financial incentive",
    stringr::str_detect(pt, "standard|mandate|obligation|ban|require|quota|regulat|law") ~
      "Regulation/standard",
    stringr::str_detect(pt, "r&d|research|innovation|demonstrat|pilot") ~
      "R&D / Demonstration",
    stringr::str_detect(pt, "public procurement|government procurement|tender") ~
      "Public procurement",
    stringr::str_detect(pt, "information|label|awareness|training|audit") ~
      "Information/labeling",
    stringr::str_detect(pt, "voluntary|agreement|partnership") ~
      "Voluntary/coordination",
    TRUE ~ "Other"
  )
}

POLICY_TYPE_WEIGHTS_IEA <- tibble::tribble(
  ~policy_type_bucket, ~w_type,
  "Regulation/standard", 0.5,
  "Financial incentive", 0.85,
  "Public procurement", 0.70,
  "R&D / Demonstration", 0.50,
  "Information/labeling", 0.20,
  "Voluntary/coordination", 0.1,
  "Other", 0.20
)

STATUS_WEIGHTS <- tibble::tribble(
  ~status, ~w_status,
  "In force", 1.00,
  "In effect", 1.00,
  "Planned", 0.60,
  "Proposed", 0.50,
  "Ended", 0.00,
  "Expired", 0.20,
  "Unknown", 0.20
)

JURIS_WEIGHTS <- tibble::tribble(
  ~jurisdiction, ~w_juris,
  "National", 1.00,
  "Subnational", 0.5,
  "State", 0.5,
  "Province", 0.5,
  "Regional", 0.5,
  "Local", 0.25,
  "International", 0.20,
  "Unknown", 0.30
)

build_policy_table <- function(pams_country_tbl) {
  pams_country_tbl %>%
    dplyr::mutate(
      tech_info = purrr::map2(.data$technologies_list, .data$text_all, assign_tech_two_stage),
      tech_list = purrr::map(.data$tech_info, "tech"),
      tech_source = purrr::map_chr(.data$tech_info, "tech_source"),
      supply_chain_list = purrr::map(.data$text_all, assign_supply_chain),
      policy_type_bucket = purrr::map2_chr(.data$policyType_list, .data$text_all, normalize_policy_type)
    ) %>%
    dplyr::select(-.data$tech_info) %>%
    tidyr::unnest(.data$tech_list) %>%
    tidyr::unnest(.data$supply_chain_list) %>%
    dplyr::rename(tech = .data$tech_list, supply_chain = .data$supply_chain_list) %>%
    dplyr::mutate(
      tech_crosscut = (.data$tech == "Cross-cutting"),
      sc_crosscut = (.data$supply_chain == "Cross-cutting"),
      status_norm = dplyr::case_when(
        .data$status %in% STATUS_WEIGHTS$status ~ .data$status,
        stringr::str_detect(stringr::str_to_lower(.data$status), "in force|in effect") ~ "In force",
        stringr::str_detect(stringr::str_to_lower(.data$status), "ended|expired") ~ "Ended",
        stringr::str_detect(stringr::str_to_lower(.data$status), "planned") ~ "Planned",
        stringr::str_detect(stringr::str_to_lower(.data$status), "propos") ~ "Proposed",
        TRUE ~ "Unknown"
      ),
      jurisdiction_norm = dplyr::case_when(
        .data$jurisdiction %in% JURIS_WEIGHTS$jurisdiction ~ .data$jurisdiction,
        stringr::str_detect(stringr::str_to_lower(.data$jurisdiction), "national") ~ "National",
        stringr::str_detect(stringr::str_to_lower(.data$jurisdiction), "internat") ~ "International",
        stringr::str_detect(stringr::str_to_lower(.data$jurisdiction), "sub|state|province|regional") ~
          "Subnational",
        stringr::str_detect(stringr::str_to_lower(.data$jurisdiction), "local|city|municip") ~ "Local",
        TRUE ~ "Unknown"
      )
    ) %>%
    dplyr::left_join(POLICY_TYPE_WEIGHTS_IEA, by = "policy_type_bucket") %>%
    dplyr::left_join(STATUS_WEIGHTS, by = c("status_norm" = "status")) %>%
    dplyr::left_join(JURIS_WEIGHTS, by = c("jurisdiction_norm" = "jurisdiction")) %>%
    dplyr::mutate(
      w_type = dplyr::coalesce(.data$w_type, 0.30),
      w_status = dplyr::coalesce(.data$w_status, 0.50),
      w_juris = dplyr::coalesce(.data$w_juris, 0.80),
      w_tech_specific = dplyr::if_else(.data$tech_crosscut, 0.70, 1.00),
      w_tech_source = dplyr::if_else(.data$tech_source == "text" & !.data$tech_crosscut, 0.30, 1.00),
      w_tech = .data$w_tech_specific * .data$w_tech_source,
      w_sc = dplyr::if_else(.data$sc_crosscut, 0.75, 1.00),
      policy_strength = .data$w_type * .data$w_status * .data$w_juris * .data$w_tech * .data$w_sc
    )
}

expand_cross_cutting <- function(policy_tbl,
                                 tech_universe,
                                 supply_chain_universe = c("Upstream", "Midstream", "Downstream"),
                                 split_strength = FALSE) {
  stopifnot(is.character(tech_universe), length(tech_universe) > 0)
  tech_universe <- setdiff(unique(tech_universe), "Cross-cutting")
  supply_chain_universe <- unique(supply_chain_universe)

  policy_tbl %>%
    dplyr::mutate(
      tech_targets = purrr::map(tech, ~ if (.x == "Cross-cutting") tech_universe else .x),
      sc_targets = purrr::map(supply_chain, ~ if (.x == "Cross-cutting") supply_chain_universe else .x),
      expanded = purrr::map2(tech_targets, sc_targets, ~ tidyr::expand_grid(
        tech_exp = .x,
        supply_chain_exp = .y
      )),
      expansion_n = purrr::map_int(expanded, nrow)
    ) %>%
    dplyr::select(-tech_targets, -sc_targets) %>%
    tidyr::unnest(expanded) %>%
    dplyr::mutate(
      tech = tech_exp,
      supply_chain = supply_chain_exp,
      policy_strength = if (split_strength) policy_strength / expansion_n else policy_strength
    ) %>%
    dplyr::select(-tech_exp, -supply_chain_exp, -expansion_n) %>%
    dplyr::filter(tech != "Cross-cutting", supply_chain != "Cross-cutting")
}

build_policy_index <- function(policy_tbl,
                               tech_universe,
                               supply_chain_universe = c("Upstream", "Midstream", "Downstream"),
                               split_strength = FALSE) {
  policy_tbl_expanded <- expand_cross_cutting(
    policy_tbl = policy_tbl,
    tech_universe = tech_universe,
    supply_chain_universe = supply_chain_universe,
    split_strength = split_strength
  )

  agg <- policy_tbl_expanded %>%
    dplyr::group_by(.data$iso3, .data$country, .data$tech, .data$supply_chain) %>%
    dplyr::summarise(
      n_policies = dplyr::n_distinct(.data$policy_id),
      strength_sum = sum(.data$policy_strength, na.rm = TRUE),
      strength_mean = mean(.data$policy_strength, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(strength_sum_log = log1p(.data$strength_sum))

  idx <- agg %>%
    #dplyr::group_by(.data$iso3) %>%
    dplyr::mutate(policy_index = median_scurve(.data$strength_sum_log)) %>%
    dplyr::ungroup()

  if (any(idx$tech == "Cross-cutting") || any(idx$supply_chain == "Cross-cutting")) {
    stop("Cross-cutting categories remain after expansion; check expand_cross_cutting().")
  }

  list(
    policy_clean = policy_tbl_expanded,
    policy_agg = agg,
    policy_index = idx
  )
}

build_policy_index_from_raw <- function(raw_pams,
                                        tech_universe = NULL,
                                        supply_chain_universe = c("Upstream", "Midstream", "Downstream"),
                                        split_strength = FALSE) {
  if (is.null(tech_universe)) {
    tech_universe <- setdiff(names(TECH_RULES), "Cross-cutting")
  }

  pams_country <- clean_pams_raw(raw_pams)
  policy_tbl <- build_policy_table(pams_country)

  build_policy_index(
    policy_tbl = policy_tbl,
    tech_universe = tech_universe,
    supply_chain_universe = supply_chain_universe,
    split_strength = split_strength
  )
}

iea_policy_index <- function(raw_pams,
                             year = 0L,
                             tech_universe = NULL,
                             supply_chain_universe = c("Upstream", "Midstream", "Downstream"),
                             split_strength = FALSE) {
  if (is.null(tech_universe)) {
    tech_universe <- setdiff(names(TECH_RULES), "Cross-cutting")
  }

  outputs <- build_policy_index_from_raw(
    raw_pams = raw_pams,
    tech_universe = tech_universe,
    supply_chain_universe = supply_chain_universe,
    split_strength = split_strength
  )

  index_tbl <- outputs$policy_index %>%
    dplyr::transmute(
      Country = .data$country,
      tech = .data$tech,
      supply_chain = .data$supply_chain,
      category = "Policy",
      variable = "IEA PAMS Policy Index",
      data_type = "index",
      value = .data$policy_index,
      Year = as.integer(year),
      source = "IEA PAMS",
      explanation = "Policy strength index derived from IEA PAMS policy attributes"
    )

  list(
    index_tbl = index_tbl,
    outputs = outputs
  )
}
