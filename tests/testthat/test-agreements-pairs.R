root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
for (f in c("agr_schema.R", "agr_text_coding.R", "agr_link.R", "agr_pairs.R")) source(file.path(root, "R", "agreements", f))
cfg <- yaml::read_yaml(file.path(root, "config", "partnership_agreements.yml"))
as_of <- "2026-11-06"
eu <- c("DEU", "FRA")

mk <- function(id, parties, type, date, status = "signed", legal = "non_binding", label = "") {
  df <- data.frame(source_dataset = "curated", source_record_id = id, title = paste("Agreement", id),
                   agreement_type = type, signature_date = date, implementation_status = status,
                   legal_status = legal, partnership_label = label, stringsAsFactors = FALSE)
  pf <- agr_party_fields(list(strsplit(parties, "|", fixed = TRUE)[[1]]))
  df[names(pf)] <- pf
  agr_as_records(df)
}
cd <- function(id, dim, code, basis = "curated", n_hits = 1L) {
  data.frame(source_record_id = id, dimension = dim, code = code, basis = basis, n_hits = n_hits, stringsAsFactors = FALSE)
}
it <- agr_instrument_table(cfg)
op_trade <- it$instrument[it$operational & it$group == "trade"][1]
op_fin <- it$instrument[it$operational & it$group == "finance"][1]
many <- c("USA", "JPN", sprintf("X%02d", 1:23))   # 25 parties > max_parties_for_depth

rec <- rbind(
  mk("c:dialogue", "BRA|USA", "joint_statement", "2024-05-01"),                               # class 1
  mk("c:framework", "CHL|USA", "mou", "2023-03-01"),                                          # class 2
  mk("c:sector", "IDN|USA", "joint_statement", "2024-06-01"),                                 # class 3 (specific)
  mk("c:sector_text", "THA|USA", "joint_statement", "2024-06-01"),                            # class 3 (text grade)
  mk("c:mention", "VNM|USA", "joint_statement", "2024-06-01"),                                # class 1: instrument only mentioned
  mk("c:operational", "PHL|USA", "mou", "2024-07-01"),                                        # class 4
  mk("c:fta", "AUS|USA", "fta", "2004-05-18", status = "in_force", legal = "binding"),        # class 5 with c:crm
  mk("c:crm", "AUS|USA", "framework", "2025-10-20"),
  mk("c:inactive", "ARG|USA", "fta", "1995-01-01", status = "inactive", legal = "binding"),   # class 0
  mk("c:old", "MEX|USA", "joint_statement", "2000-01-01"),                                    # class 0: not active
  mk("c:club", paste(many, collapse = "|"), "plurilateral_forum", "2025-01-01"),             # capped
  mk("c:eu", "CHL|EUU", "framework", "2025-01-15")                                            # via EU
)
codes <- agr_as_codes(rbind(
  cd("c:sector", "sector", "critical_minerals"),
  cd("c:sector_text", "sector", "critical_minerals", "text_body", 3L),
  cd("c:mention", "instrument_types", op_fin, "text_body", 1L),
  cd("c:operational", "instrument_types", op_fin), cd("c:operational", "sector", "critical_minerals"),
  cd("c:fta", "instrument_types", op_trade, "type_implied"), cd("c:fta", "sector", "trade", "type_implied"),
  cd("c:crm", "instrument_types", op_fin), cd("c:crm", "sector", "critical_minerals"),
  cd("c:inactive", "instrument_types", op_trade, "type_implied"),
  cd("c:club", "instrument_types", op_fin), cd("c:club", "sector", "critical_minerals"),
  cd("c:eu", "instrument_types", op_fin), cd("c:eu", "sector", "critical_minerals")
))
L <- agr_link_records(rec, codes, cfg)
P <- agr_pair_profiles(L$canonical, L$canonical_codes, cfg, eu, as_of)
row <- function(p) P[P$pair == p, , drop = FALSE]

test_that("pairs are undirected, one row per pair, every party pair of an agreement", {
  expect_true(all(P$a < P$b))
  expect_equal(anyDuplicated(P$pair), 0L)
  expect_equal(row("AUS|USA")$n_agreements, 2L)
  expect_true("JPN|USA" %in% P$pair)
  expect_equal(sum(grepl("^X01\\||\\|X01$", P$pair)), 24L)
})

test_that("each depth class has its rule (all agreements)", {
  expect_equal(row("BRA|USA")$depth_all, 1L)
  expect_equal(row("CHL|USA")$depth_all, 2L)
  expect_equal(row("IDN|USA")$depth_all, 3L)
  expect_equal(row("THA|USA")$depth_all, 3L)
  expect_match(row("THA|USA")$depth_all_rule, "critical_minerals:text")
  expect_equal(row("USA|VNM")$depth_all, 1L)
  expect_equal(row("USA|VNM")$inst_finance, 1L)
  expect_equal(row("PHL|USA")$depth_all, 4L)
  expect_equal(row("PHL|USA")$inst_finance, 2L)
  expect_equal(row("AUS|USA")$depth_all, 5L)
  expect_match(row("AUS|USA")$depth_all_rule, "^5 .*\\(finance\\)")
  expect_true(row("AUS|USA")$has_trade_agreement)
  expect_match(row("BRA|USA")$depth_all_rule, "^1 ")
})

test_that("the electro-industrial scope uses only electro-industrial agreements", {
  expect_equal(row("BRA|USA")$depth_ei, 0L)
  expect_equal(row("IDN|USA")$depth_ei, 3L)
  expect_equal(row("PHL|USA")$depth_ei, 4L)
  # the FTA is not electro-industrial, so there is no trade instrument in scope: class 4, not 5
  expect_equal(row("AUS|USA")$depth_ei, 4L)
  expect_match(row("AUS|USA")$ei_sectors, "critical_minerals:specific")
})

test_that("inactive and old non-binding agreements do not count", {
  expect_equal(row("ARG|USA")$depth_all, 0L)
  expect_equal(row("ARG|USA")$n_active, 0L)
  expect_false(row("ARG|USA")$has_trade_agreement)
  expect_equal(row("MEX|USA")$depth_all, 0L)
  expect_equal(row("AUS|USA")$n_active, 2L)
  expect_equal(row("CHL|USA")$n_recent, 1L)
})

test_that("plurilaterals above max_parties_for_depth count only toward class 1", {
  expect_equal(row("JPN|USA")$depth_all, 1L)
  expect_match(row("JPN|USA")$depth_all_rule, "max_parties_for_depth")
  expect_equal(row("JPN|USA")$n_plurilateral, 1L)
})

test_that("EU agreements reach current member states via_eu, never EU-member pairs", {
  expect_equal(row("CHL|EUU")$n_via_eu, 0L)
  expect_equal(row("CHL|DEU")$n_via_eu, 1L)
  expect_equal(row("CHL|FRA")$depth_all, 4L)
  expect_false(any(P$pair %in% c("DEU|EUU", "EUU|FRA")))
  expect_true(grepl("Batteries", row("CHL|DEU")$tech_cover))
})

test_that("an accession makes pairs with the acceding country only, not again among existing members", {
  can <- data.frame(canonical_agreement_id = c("agr-base", "agr-acc"),
                    parties = c("AUS|CAN|JPN", "AUS|CAN|GBR|JPN"),
                    source_records = c("desta:989", "desta:989+1"), stringsAsFactors = FALSE)
  dd <- data.frame(source_record_id = c("desta:989", "desta:989+1"), acceding = c("", "GBR"), stringsAsFactors = FALSE)
  acc <- agr_canonical_acceding(can, dd)
  expect_equal(acc, list(`agr-acc` = "GBR"))
  pa <- agr_pair_agreements(can, cfg, eu_members = character(), acceding = acc)
  expect_setequal(pa$pair[pa$canonical_agreement_id == "agr-acc"], c("AUS|GBR", "CAN|GBR", "GBR|JPN"))
  expect_setequal(pa$pair[pa$canonical_agreement_id == "agr-base"], c("AUS|CAN", "AUS|JPN", "CAN|JPN"))
  # without the DESTA design every pair of the accession record is kept (the old behaviour)
  expect_equal(sum(agr_pair_agreements(can, cfg, character())$canonical_agreement_id == "agr-acc"), 6L)
})

test_that("the EU's seat in a club is not propagated to member states; leap-day as-of works", {
  can <- data.frame(canonical_agreement_id = c("agr-g7", "agr-mou"), parties = c("CAN|EUU|JPN|USA", "CHL|EUU"),
                    relationship_category = c("plurilateral_club", "sectoral"), stringsAsFactors = FALSE)
  pa <- agr_pair_agreements(can, cfg, eu_members = c("DEU", "FRA"))
  expect_false(any(pa$canonical_agreement_id == "agr-g7" & pa$via_eu))
  expect_setequal(pa$pair[pa$canonical_agreement_id == "agr-mou" & pa$via_eu], c("CHL|DEU", "CHL|FRA"))
})
