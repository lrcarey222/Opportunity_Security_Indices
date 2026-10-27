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

# Supply-chain stage of a policy, from keywords in its text (title, description, IEA technology
# list, tags, policy type). Since 2026-10-26 the rules name the six stages of
# R/utils/supply_chain_stages.R (docs/supply_chain_stages.md); a policy may hit several, and one
# that hits none is "Cross-cutting" and is counted on every stage at the cross-cutting weight
# (w_sc 0.75, build_policy_table()). The Extraction words are matched as whole words. As bare
# substrings "ore" hit "more", "before", "foreign", "Korea" and "Singapore", "mine" hit
# "determine" and "lease" hit "release": 36.9% of PAMS policies were tagged Upstream, against
# 12.5% on the words themselves, which moved them off the cross-cutting smear onto Upstream at
# full weight.
#
# What moved when the Upstream / Midstream / Downstream lists (SUPPLY_CHAIN_RULES_LEGACY) were
# split into six (2026-10-26):
# - Refining and smelting left Midstream for Refining, which rolls up to Upstream; recycling and
#   scrap words are new (Recovery), as are "consumption" and "grid operation" (Deployment).
# - "processing" and "components" are matched in context only: on their own they hit "permit
#   processing" and "the programme has four components". Bare "plant" is dropped ("power plant"
#   stays a Deployment word; manufacturing plants are caught by the manufacturing words).
# - Assembly words skip "National Assembly" and "Assembly Bill": only assembly lines, plants and
#   the assembly of named finished goods count. Likewise "key components" (of a programme), "gas
#   separators" and fan "blades" are not Components words.
# - "scrap" is the noun (scrap metal, battery scrap). Vehicle scrappage schemes ("to scrap old
#   cars", "scrap vehicle bonus") are purchase incentives, not recovery.
# - "refined" as a verb ("the act was refined by") and cathode ray tubes are not stage words.
# Generic manufacturing words (SUPPLY_CHAIN_GENERIC_MANUFACTURING) do not say which step is
# meant, so they put a policy on both Components and Assembly unless it names one of the two.
SUPPLY_CHAIN_RULES <- list(
  "Extraction" = c(
    "\\bmining\\b", "\\bmines?\\b", "\\bexploration\\b", "\\bextraction\\b", "\\bores?\\b",
    "\\bleases?\\b", "\\bleasing\\b", "\\bconcessions?\\b", "\\bdrilling\\b", "\\bupstream\\b"
  ),
  "Refining" = c(
    "\\brefin(ing|ery|eries|er|ers)\\b",
    "(?<!\\b(was|were|been|then|further) )\\brefined\\b(?! definitions?\\b)",
    "\\bsmelt",
    paste0(
      "\\b(minerals?|ores?|metals?|lithium|nickel|cobalt|graphite|rare earths?|copper|manganese|",
      "uranium|bauxite|gas) (processing|refining)\\b"
    ),
    "\\bprocessing\\b[^.;|]{0,40}\\b(minerals?|ores?|metals?)\\b",
    "\\benrichment\\b",
    "\\bpolysilicon\\b",
    "\\bcopper cathodes?\\b",
    "\\b(battery|lithium) chemicals?\\b", "\\bbattery[- ]grade\\b", "\\blithium (hydroxide|carbonate)\\b",
    "\\brare earth (oxides?|metals?|separation)\\b"
  ),
  "Components" = c(
    "\\b(battery|solar|pv|photovoltaic|lithium-ion|li-ion) cells?\\b",
    "\\bcells? (manufactur|production|factor)",
    "\\bwafers?\\b", "\\bingots?\\b",
    "\\bmagnets?\\b",
    "(?<!copper )\\bcathodes?\\b(?! ray)", "\\b(anodes?|electrolytes?)\\b", "\\bbattery separators?\\b",
    "\\bnacelles?\\b",
    "\\b(turbine|wind|rotor) blades?\\b", "\\bblades?,? (and )?(rotors?|nacelles?|towers?)\\b",
    "\\bfuel (assembl(y|ies)|fabrication)\\b",
    "\\bpower (semiconductors?|electronics)\\b",
    "\\bparts and components\\b", "\\bcomponents and parts\\b",
    "\\b(auto|automotive|vehicle|ev|battery|solar|pv|wind|turbine) components?\\b",
    "\\bcomponents? (manufactur|production|suppl|industr|factor)",
    "\\b(manufactur\\w*|production|supply) of (\\w+ ){0,3}components?\\b"
  ),
  "Assembly" = c(
    "\\bassembly (lines?|plants?|facilit\\w*|operations?)\\b",
    "\\b(final|vehicle|car|ev|module|pack|turbine|battery) assembly\\b",
    "\\bassembled into\\b", "\\b(locally|domestically|fully) assembled\\b",
    paste0(
      "\\bassembl(e|ing) (\\w+ ){0,2}(solar|pv|vehicles?|cars?|buses|modules?|panels?|",
      "batter(y|ies)|packs?|turbines?|chargers?)\\b"
    ),
    "\\bequipment (manufactur|production)",
    paste0(
      "\\b(manufactur\\w*|production|assembly) of (\\w+ ){0,3}(equipment|vehicles?|cars?|modules?|",
      "turbines?|electroly[sz]ers?|transformers?|cables?|reactors?|inverters?|heat pumps?|chargers?|",
      "battery packs?)\\b"
    ),
    paste0(
      "\\b(vehicles?|cars?|modules?|turbines?|electroly[sz]ers?|transformers?|inverters?|chargers?|",
      "packs?) (manufactur|production|assembly|factor)"
    )
  ),
  "Deployment" = c(
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
    "generation",
    "\\bconsumption\\b",
    "grid operat"
  ),
  "Recovery" = c(
    "recycl",
    "(?<!\\bto )\\bscrap\\b(?! (vehicles?|cars?)\\b)",
    "\\bend[- ]of[- ]life\\b",
    "\\bsecond[- ]life\\b",
    "\\breprocess",
    "\\bremanufactur",
    paste0(
      "\\b(batter(y|ies)|panels?|modules?|components?|materials?|minerals?|magnets?) ",
      "(re-?use|repurpos\\w*)\\b"
    ),
    paste0(
      "\\b(re-?use|repurpos\\w*) of (\\w+ ){0,2}",
      "(batter(y|ies)|panels?|modules?|components?|materials?|minerals?|magnets?)\\b"
    )
  )
)

SUPPLY_CHAIN_GENERIC_MANUFACTURING <- c(
  "\\bmanufactur",
  "\\bfactor(y|ies)\\b", "gigafactor",
  "supply chain",
  "domestic content", "local content"
)

# Stage is relative to the technology (docs/supply_chain_stages.md). Two keyword stages mean a
# later step for one technology, and a policy's hits move for that technology only: drilling,
# exploration and leases are how a geothermal plant is sited and built (geothermal drilling is
# Deployment, user decision 2026-10-26), and refineries and smelters (green steel) are where
# green hydrogen is used (as ammonia stays Green Hydrogen Deployment in the TRL map).
IEA_TECH_STAGE_OVERRIDES <- tibble::tribble(
  ~tech, ~from, ~to,
  "Geothermal", "Extraction", "Deployment",
  "Green Hydrogen", "Refining", "Deployment"
)

# The three-stage rules used until 2026-10-26. Kept for the Sector dashboard export
# (scripts/87 -> sector_policy_compact(), whose stage_mask has one bit per Upstream / Midstream /
# Downstream) until that is migrated, and for reproducing three-stage builds
# (iea_policy_index(supply_chain_universe = c("Upstream", "Midstream", "Downstream"))).
SUPPLY_CHAIN_RULES_LEGACY <- list(
  "Upstream" = c(
    "\\bmining\\b", "\\bmines?\\b", "\\bexploration\\b", "\\bextraction\\b", "\\bores?\\b",
    "\\bleases?\\b", "\\bleasing\\b", "\\bconcessions?\\b", "\\bdrilling\\b"
  ),
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

# TRUE where text matches any of the patterns (vectorised over text). One alternation per keyword
# list rather than one regex call per word: the six-stage rules have about 60 patterns.
match_any <- function(text, patterns) {
  pattern <- paste0("(?:", patterns, ")", collapse = "|")
  stringr::str_detect(text, stringr::regex(pattern, ignore_case = TRUE))
}

# The stages a policy's text hits, in chain order, or "Cross-cutting" when it hits none.
# stages = "six" uses SUPPLY_CHAIN_RULES (a generic manufacturing word alone gives Components
# and Assembly); "legacy" the Upstream / Midstream / Downstream rules.
assign_supply_chain <- function(text_all, stages = c("six", "legacy")) {
  assign_supply_chains(text_all, stages = stages)[[1]]
}

# assign_supply_chain() for many texts at once (one regex call per stage): a list with one stage
# vector per text.
assign_supply_chains <- function(texts, stages = c("six", "legacy")) {
  stages <- match.arg(stages)
  rules <- if (stages == "six") SUPPLY_CHAIN_RULES else SUPPLY_CHAIN_RULES_LEGACY
  hit <- matrix(
    vapply(rules, function(p) match_any(texts, p), logical(length(texts))),
    nrow = length(texts), ncol = length(rules), dimnames = list(NULL, names(rules))
  )
  if (stages == "six") {
    generic <- !hit[, "Components"] & !hit[, "Assembly"] &
      match_any(texts, SUPPLY_CHAIN_GENERIC_MANUFACTURING)
    hit[generic, c("Components", "Assembly")] <- TRUE
  }
  lapply(seq_along(texts), function(i) {
    hits <- colnames(hit)[hit[i, ]]
    if (length(hits) == 0) "Cross-cutting" else hits
  })
}

# Move a policy's keyword stages for one technology (IEA_TECH_STAGE_OVERRIDES), keeping chain
# order and dropping a stage the move duplicates. "Cross-cutting" (no stage word) is not moved.
apply_tech_stage_overrides <- function(stages, tech, overrides = IEA_TECH_STAGE_OVERRIDES) {
  if (!tech %in% overrides$tech) {
    return(stages)
  }
  rules <- overrides[overrides$tech == tech, , drop = FALSE]
  moved <- dplyr::coalesce(rules$to[match(stages, rules$from)], stages)
  chain <- c(names(SUPPLY_CHAIN_RULES), names(SUPPLY_CHAIN_RULES_LEGACY), "Cross-cutting")
  unique(moved[order(match(moved, chain))])
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

# The status and jurisdiction tables carry an IEA_ prefix because nipo_policy_index.R defines
# its own jurisdiction table and scripts/10 sources both files into one environment. Under the
# shared name JURIS_WEIGHTS the NIPO table (International = 0.20) silently replaced this one
# (International = 0.66), so IEA PAMS was scored with the NIPO weight.
IEA_STATUS_WEIGHTS <- tibble::tribble(
  ~status, ~w_status,
  "In force", 1.00,
  "In effect", 1.00,
  "Planned", 0.60,
  "Proposed", 0.50,
  "Ended", 0.00,
  "Expired", 0.20,
  "Unknown", 0.20
)

IEA_JURIS_WEIGHTS <- tibble::tribble(
  ~jurisdiction, ~w_juris,
  "National", 1.00,
  "Subnational", 0.5,
  "State", 0.5,
  "Province", 0.5,
  "Regional", 0.5,
  "Local", 0.25,
  "International", 0.66,
  "Unknown", 0.30
)

# One row per policy x country x tech x supply_chain. stages = "six" (what iea_policy_index()
# uses) tags the six stages of SUPPLY_CHAIN_RULES and moves them per technology
# (IEA_TECH_STAGE_OVERRIDES); "legacy" tags Upstream / Midstream / Downstream. The default stays
# "legacy" for the Sector dashboard export (scripts/87 -> sector_policy_compact(), three stage
# bits) until that is migrated.
build_policy_table <- function(pams_country_tbl, stages = c("legacy", "six")) {
  stages <- match.arg(stages)
  out <- pams_country_tbl %>%
    dplyr::mutate(
      tech_info = purrr::map2(.data$technologies_list, .data$text_all, assign_tech_two_stage),
      tech_list = purrr::map(.data$tech_info, "tech"),
      tech_source = purrr::map_chr(.data$tech_info, "tech_source"),
      # once per policy text, not per policy x country row
      supply_chain_list = {
        texts <- unique(.data$text_all)
        assign_supply_chains(texts, stages = stages)[match(.data$text_all, texts)]
      },
      policy_type_bucket = purrr::map2_chr(.data$policyType_list, .data$text_all, normalize_policy_type)
    ) %>%
    dplyr::select(-"tech_info") %>%
    tidyr::unnest("tech_list")
  if (stages == "six") {
    # Per technology: a policy tagged Geothermal and Extraction is a Geothermal Deployment policy.
    # A cross-cutting technology keeps the keyword stages (it is not one technology).
    out <- out %>%
      dplyr::mutate(supply_chain_list = purrr::map2(
        .data$supply_chain_list, .data$tech_list, apply_tech_stage_overrides
      ))
  }
  out %>%
    tidyr::unnest("supply_chain_list") %>%
    dplyr::rename(tech = "tech_list", supply_chain = "supply_chain_list") %>%
    dplyr::mutate(
      tech_crosscut = (.data$tech == "Cross-cutting"),
      sc_crosscut = (.data$supply_chain == "Cross-cutting"),
      status_norm = dplyr::case_when(
        .data$status %in% IEA_STATUS_WEIGHTS$status ~ .data$status,
        stringr::str_detect(stringr::str_to_lower(.data$status), "in force|in effect") ~ "In force",
        stringr::str_detect(stringr::str_to_lower(.data$status), "ended|expired") ~ "Ended",
        stringr::str_detect(stringr::str_to_lower(.data$status), "planned") ~ "Planned",
        stringr::str_detect(stringr::str_to_lower(.data$status), "propos") ~ "Proposed",
        TRUE ~ "Unknown"
      ),
      jurisdiction_norm = dplyr::case_when(
        .data$jurisdiction %in% IEA_JURIS_WEIGHTS$jurisdiction ~ .data$jurisdiction,
        stringr::str_detect(stringr::str_to_lower(.data$jurisdiction), "national") ~ "National",
        stringr::str_detect(stringr::str_to_lower(.data$jurisdiction), "internat") ~ "International",
        stringr::str_detect(stringr::str_to_lower(.data$jurisdiction), "sub|state|province|regional") ~
          "Subnational",
        stringr::str_detect(stringr::str_to_lower(.data$jurisdiction), "local|city|municip") ~ "Local",
        TRUE ~ "Unknown"
      )
    ) %>%
    dplyr::left_join(POLICY_TYPE_WEIGHTS_IEA, by = "policy_type_bucket") %>%
    dplyr::left_join(IEA_STATUS_WEIGHTS, by = c("status_norm" = "status")) %>%
    dplyr::left_join(IEA_JURIS_WEIGHTS, by = c("jurisdiction_norm" = "jurisdiction")) %>%
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

# Copy cross-cutting rows onto every technology of tech_universe and, for a policy with no stage
# word, every stage of supply_chain_universe (the six stages; c("Upstream", "Midstream",
# "Downstream") for a three-stage policy table).
expand_cross_cutting <- function(policy_tbl,
                                 tech_universe,
                                 supply_chain_universe = SUPPLY_CHAIN_STAGES,
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
                               supply_chain_universe = SUPPLY_CHAIN_STAGES,
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
  # A policy table tagged with other stages than the universe (a three-stage table expanded onto
  # the six stages, or the reverse) would leave cells the index layer never joins.
  off_universe <- setdiff(unique(idx$supply_chain), supply_chain_universe)
  if (length(off_universe) > 0) {
    stop(
      "IEA PAMS stages outside supply_chain_universe: ", paste(off_universe, collapse = ", "),
      ". Build the policy table with the same stages (build_policy_table(stages = ...))."
    )
  }

  list(
    policy_clean = policy_tbl_expanded,
    policy_agg = agg,
    policy_index = idx
  )
}

# supply_chain_universe: the six stages, or c("Upstream", "Midstream", "Downstream") to
# reproduce a three-stage build (the legacy keyword rules).
build_policy_index_from_raw <- function(raw_pams,
                                        tech_universe = NULL,
                                        supply_chain_universe = SUPPLY_CHAIN_STAGES,
                                        split_strength = FALSE) {
  if (is.null(tech_universe)) {
    tech_universe <- setdiff(names(TECH_RULES), "Cross-cutting")
  }
  stages <- if (all(supply_chain_universe %in% names(SUPPLY_CHAIN_RULES_LEGACY))) "legacy" else "six"

  pams_country <- clean_pams_raw(raw_pams)
  policy_tbl <- build_policy_table(pams_country, stages = stages)

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
                             supply_chain_universe = SUPPLY_CHAIN_STAGES,
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

  # iso3c comes from the PAMS country JSON. Carrying it lets standardize_country_table() put
  # the WDI name on each row: matched by name alone, IEA spellings such as "People's Republic
  # of China" or "Islamic Republic of Iran" kept their IEA form and never met the CAT, NIPO
  # and dual-use rows for the same country.
  index_tbl <- outputs$policy_index %>%
    dplyr::transmute(
      Country = .data$country,
      iso3c = .data$iso3,
      tech = .data$tech,
      supply_chain = .data$supply_chain,
      category = "Policy",
      variable = "IEA PAMS Policy Index",
      data_type = "index",
      value = .data$policy_index,
      Year = as.integer(year),
      source = "IEA PAMS",
      explanation = paste(
        "Policy strength index derived from IEA PAMS policy attributes;",
        "stage from keyword rules on the policy text, a policy naming no stage counted on every",
        "stage at weight 0.75"
      )
    )

  list(
    index_tbl = index_tbl,
    outputs = outputs
  )
}
