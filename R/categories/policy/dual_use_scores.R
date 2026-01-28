# Clean dual-use scores into policy component format.
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
  
  cleaned <- cleaned %>%
    dplyr::rename(
      tech = technology,
      supply_chain = stage
    ) %>%
    dplyr::mutate(
      tech = as.character(tech),
      supply_chain = dplyr::case_when(
        supply_chain %in% c("U", "Upstream", "upstream") ~ "Upstream",
        supply_chain %in% c("M", "Midstream", "midstream") ~ "Midstream",
        supply_chain %in% c("D", "Downstream", "downstream") ~ "Downstream",
        TRUE ~ as.character(supply_chain)
      ),
      category = "Policy",
      variable = "Dual-use policy score",
      data_type = "index",
      Year = NA,
      source = "ChatGPT - Thinking Model",
      explanation = "Dual-use score by technology and supply chain stage",
      value = suppressWarnings(as.numeric(value))
    )
  
  cleaned
}
