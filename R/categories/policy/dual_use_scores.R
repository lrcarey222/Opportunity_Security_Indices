# Clean dual-use scores into policy component format.
#
# Provenance: data/raw/dual_use_scores_primary_secondary_tertiary.csv is a project-authored
# table of 0-1 defence-relevance scores per technology x Upstream / Midstream / Downstream stage
# (33 rows; copied onto the six supply-chain stages below), generated with an
# LLM (ChatGPT, "Thinking" model) and not drawn from a statistical source. It has no country
# dimension: the same score is copied to every country, so the component shifts sector
# levels but cannot rank countries. The `source` and `explanation` fields say so, so the
# provenance travels with every row into the policy component table.
#
# Since October 2026 the composite Policy index no longer uses it (R/indices/
# build_policy_index.R: NIPO 4, IEA PAMS 3, CAT 2). The table is still built and written
# (dual_use_scores_tbl, and rows in policy_component_tbl) for reference only.
clean_dual_use_scores <- function(raw_tbl,
                                  countries = NULL,
                                  year = 0L,
                                  source = "Dual-use scores") {
  
  cleaned <- raw_tbl %>%
    dplyr::rename(value = dual_use_score_0_1)
  
  # normalize Country column
  if (!"Country" %in% names(cleaned) && "country" %in% names(cleaned)) {
    cleaned <- dplyr::rename(cleaned, Country = country)
  }
  if (!"Country" %in% names(cleaned)) {
    cleaned$Country <- "Global"
  }
  
  # if it's a global table and user provided countries, replicate across them
  if (length(unique(cleaned$Country)) == 1 &&
      identical(cleaned$Country[[1]], "Global") &&
      !is.null(countries)) {
    countries_tbl <- tibble::tibble(Country = as.character(countries))
    cleaned_no_country <- cleaned %>% dplyr::select(-Country)
    cleaned <- tidyr::crossing(countries_tbl, cleaned_no_country)
  }
  
  # fallback value column names if dual_use_score_0_1 wasn't present
  if (!"value" %in% names(cleaned)) {
    if ("score" %in% names(cleaned)) {
      cleaned <- dplyr::rename(cleaned, value = score)
    } else if ("dual_use_score" %in% names(cleaned)) {
      cleaned <- dplyr::rename(cleaned, value = dual_use_score)
    }
  }
  
  # The table scores three stages: U / primary (Commodities) = Upstream, M / secondary
  # (Manufacturing) = Midstream, D / tertiary (Deployment-operations) = Downstream. Each score is
  # copied onto the six supply-chain stages its stage covers (supply_chain_expand_legacy(),
  # R/utils/supply_chain_stages.R): Upstream to Extraction, Refining and Recovery, Midstream to
  # Components and Assembly, Downstream to Deployment. A stage label it does not know stops.
  cleaned <- cleaned %>%
    dplyr::rename(
      tech = technology,
      supply_chain = stage
    ) %>%
    dplyr::mutate(
      tech = as.character(tech),
      source_stage = dplyr::case_when(
        tolower(supply_chain) %in% c("u", "upstream", "primary") ~ "Upstream",
        tolower(supply_chain) %in% c("m", "midstream", "secondary") ~ "Midstream",
        tolower(supply_chain) %in% c("d", "downstream", "tertiary") ~ "Downstream",
        TRUE ~ as.character(supply_chain)
      ),
      supply_chain = .data$source_stage,
      category = "Policy",
      variable = "Dual-use policy score",
      data_type = "index",
      Year = NA,
      source = "LLM-generated (ChatGPT - Thinking Model); project-authored",
      explanation = paste0(
        "Dual-use (defence) relevance score by technology and supply chain stage; ",
        "LLM-elicited judgement, identical for every country; scored for ", .data$source_stage,
        " and copied to each stage it covers; ",
        "reference only, not a component of the composite Policy index"
      ),
      value = suppressWarnings(as.numeric(value))
    ) %>%
    supply_chain_expand_legacy() %>%
    dplyr::select(-"source_stage")

  cleaned
}
