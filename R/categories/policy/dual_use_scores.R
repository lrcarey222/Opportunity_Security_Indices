# Clean dual-use scores into policy component format.
clean_dual_use_scores <- function(raw_tbl,
                                  year = 0L,
                                  source = "Dual-use scores") {
  require_columns(raw_tbl, c("technology", "stage"), label = "dual_use_score_0_1")

  cleaned <- raw_tbl

  if (!"Country" %in% names(cleaned) && "country" %in% names(cleaned)) {
    cleaned <- dplyr::rename(cleaned, Country = .data$country)
  }
  if (!"Country" %in% names(cleaned)) {
    cleaned$Country <- "Global"
  }

  if (!"value" %in% names(cleaned)) {
    if ("score" %in% names(cleaned)) {
      cleaned <- dplyr::rename(cleaned, value = .data$score)
    } else if ("dual_use_score" %in% names(cleaned)) {
      cleaned <- dplyr::rename(cleaned, value = .data$dual_use_score)
    }
  }

  cleaned <- cleaned %>%
    dplyr::rename(
      tech = .data$technology,
      supply_chain = .data$stage
    ) %>%
    dplyr::mutate(
      tech = as.character(.data$tech),
      supply_chain = dplyr::case_when(
        .data$supply_chain %in% c("U", "Upstream", "upstream") ~ "Upstream",
        .data$supply_chain %in% c("M", "Midstream", "midstream") ~ "Midstream",
        .data$supply_chain %in% c("D", "Downstream", "downstream") ~ "Downstream",
        TRUE ~ as.character(.data$supply_chain)
      )
    )

  require_columns(cleaned, c("value"), label = "dual_use_scores")

  if (!"category" %in% names(cleaned)) {
    cleaned$category <- "Policy"
  }
  if (!"variable" %in% names(cleaned)) {
    cleaned$variable <- "Dual-use policy score"
  }
  if (!"data_type" %in% names(cleaned)) {
    cleaned$data_type <- "index"
  }
  if (!"Year" %in% names(cleaned)) {
    cleaned$Year <- as.integer(year)
  }
  if (!"source" %in% names(cleaned)) {
    cleaned$source <- source
  }
  if (!"explanation" %in% names(cleaned)) {
    cleaned$explanation <- "Dual-use score by technology and supply chain stage"
  }

  cleaned %>%
    dplyr::mutate(value = suppressWarnings(as.numeric(.data$value)))
}
