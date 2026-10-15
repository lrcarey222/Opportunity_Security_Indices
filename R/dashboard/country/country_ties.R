# Shared agreements and UN-vote alignment (spec_country.md A9.9). Pure.
#
# agreements: data/reference/economic_security_agreements.csv (iso3, agreement, membership_status).
# Every listed status counts (member, participant, partner, forum_member, in_force). EU-level rows
# (iso3 EUU) are not propagated to member states. "U.S. FTA" is shared only between the United States
# and a partner listed with it.

COUNTRY_PLURILATERALS <- c("G7", "NATO", "IPEF", "MSP", "MSP Forum")
COUNTRY_US_FTA <- "U.S. FTA"
COUNTRY_AGREEMENT_ORDER <- c("G7", "NATO", "IPEF", "MSP", "MSP Forum", "U.S. FTA")

country_agreements_of <- function(agreements, iso3) {
  a <- unique(agreements$agreement[agreements$iso3 == iso3])
  a[order(match(a, COUNTRY_AGREEMENT_ORDER), a)]
}

country_agreements_shared <- function(agreements, iso3, partner) {
  a <- country_agreements_of(agreements, iso3)
  b <- country_agreements_of(agreements, partner)
  shared <- intersect(intersect(a, b), COUNTRY_PLURILATERALS)
  if ((iso3 == "USA" && COUNTRY_US_FTA %in% b) || (partner == "USA" && COUNTRY_US_FTA %in% a)) shared <- c(shared, COUNTRY_US_FTA)
  shared[order(match(shared, COUNTRY_AGREEMENT_ORDER))]
}

# The alignment of iso3 with each partner: data frame partner, alignment (0, 0.5, 1), bloc, basis.
country_alignment <- function(geo_alignment, iso3) {
  g <- geo_alignment[geo_alignment$reporter_iso == iso3, , drop = FALSE]
  data.frame(partner = g$partner_iso, alignment = g$geo_alignment, bloc = g$bloc_partner, basis = g$basis_partner, stringsAsFactors = FALSE)
}

country_alignment_words <- function(a, words) {
  if (is.null(a) || is.na(a)) return(COUNTRY_DASH)
  w <- words[[format(a, nsmall = 0)]] %||% words[[as.character(a)]]
  w %||% COUNTRY_DASH
}
