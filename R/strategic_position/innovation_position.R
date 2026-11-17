# Innovation evidence for Competitive Position (pillar INN): pure, no IO.
#
# scripts/29_build_innovation_position.R writes innovation_theme.csv, one row per country x
# technology per indicator, with supply_chain "All" (patents and public RD&D describe a technology,
# not a stage). Competitive Position selects variables per technology x six-stage cell, so this
# repeats each technology-level index at the stages that `stages` names; the innovation family's
# own `stages` in config/strategic_position.yml then decides where it counts.
#
# Only the index variables enter (data_type "index", already 0-1 within technology, higher =
# stronger); the raw counts, shares and growth stay in the theme table. Rows whose supply_chain is
# a real stage (the critical-minerals RD&D rows) are kept at that stage as they are.

sp_innovation_contrib <- function(theme, stages, techs = NULL) {
  need <- c("Country", "tech", "supply_chain", "category", "variable", "data_type", "value")
  missing <- setdiff(need, names(theme))
  if (length(missing)) stop("innovation theme lacks column(s) ", paste(missing, collapse = ", "), ".")
  assert_supply_chain_stages(stages, "sp_innovation_contrib(stages)")
  d <- theme[theme$data_type == "index" & !is.na(theme$Country), , drop = FALSE]
  if (!is.null(techs)) d <- d[d$tech %in% techs, , drop = FALSE]
  whole <- d[d$supply_chain == "All", , drop = FALSE]
  staged <- d[d$supply_chain != "All", , drop = FALSE]
  expanded <- if (nrow(whole)) {
    whole[rep(seq_len(nrow(whole)), each = length(stages)), , drop = FALSE]
  } else whole
  if (nrow(expanded)) expanded$supply_chain <- rep(stages, times = nrow(whole))
  out <- rbind(expanded, staged)
  data.frame(
    Country = out$Country, tech = out$tech, supply_chain = out$supply_chain, category = out$category,
    variable = out$variable, component_value = as.numeric(out$value), imputed = FALSE,
    stringsAsFactors = FALSE
  )
}
