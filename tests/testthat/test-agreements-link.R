root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
for (f in c("agr_schema.R", "agr_text_coding.R", "agr_link.R")) source(file.path(root, "R", "agreements", f))
cfg <- yaml::read_yaml(file.path(root, "config", "partnership_agreements.yml"))

# One synthetic record per call (common schema, R/agreements/agr_schema.R).
mk <- function(id, ds, parties, type, date, title, status = "signed", legal = "non_binding") {
  df <- data.frame(source_dataset = ds, source_record_id = id, title = title, agreement_type = type,
                   signature_date = date, implementation_status = status, legal_status = legal,
                   stringsAsFactors = FALSE)
  pf <- agr_party_fields(list(strsplit(parties, "|", fixed = TRUE)[[1]]))
  df[names(pf)] <- pf
  agr_as_records(df)
}
canon_of <- function(L, id) L$links$canonical_agreement_id[L$links$source_record_id == id]
rule_of <- function(L, id) L$links$rule[L$links$source_record_id == id]

test_that("two GSP documents of one visit are one event", {
  rec <- rbind(mk("gsp:JPNUSA1", "gsp", "JPN|USA", "joint_statement", "2015-04-28", "Joint Vision Statement"),
               mk("gsp:USAJPN1", "gsp", "JPN|USA", "joint_statement", "2015-04-30", "U.S.-Japan Joint Vision Statement"),
               mk("gsp:USAJPN2", "gsp", "JPN|USA", "joint_statement", "2015-09-30", "Joint statement on cyber"))
  L <- agr_link_records(rec, agr_empty_codes(), cfg)
  expect_equal(nrow(L$links), 3L)
  expect_equal(canon_of(L, "gsp:JPNUSA1"), canon_of(L, "gsp:USAJPN1"))
  expect_false(canon_of(L, "gsp:USAJPN2") == canon_of(L, "gsp:JPNUSA1"))
  expect_true("gsp_event" %in% c(rule_of(L, "gsp:JPNUSA1"), rule_of(L, "gsp:USAJPN1")))
  expect_equal(nrow(L$canonical), 2L)
})

test_that("a GSP treaty and a DESTA FTA on one pair and year link (trade_pair_year); other years do not", {
  rec <- rbind(mk("gsp:JPNIND1", "gsp", "IND|JPN", "treaty", "2011-02-16",
                  "Comprehensive Economic Partnership Agreement between Japan and the Republic of India"),
               mk("desta:826", "desta", "IND|JPN", "fta", "2011", "India Japan", status = "in_force", legal = "binding"),
               mk("desta:900", "desta", "IND|JPN", "fta", "2023", "India Japan protocol", status = "in_force", legal = "binding"))
  codes <- agr_as_codes(data.frame(source_record_id = "gsp:JPNIND1", dimension = "sector", code = "trade",
                                   basis = "text_title", stringsAsFactors = FALSE))
  L <- agr_link_records(rec, codes, cfg)
  expect_equal(canon_of(L, "gsp:JPNIND1"), canon_of(L, "desta:826"))
  expect_true("trade_pair_year" %in% c(rule_of(L, "gsp:JPNIND1"), rule_of(L, "desta:826")))
  expect_false(canon_of(L, "desta:900") == canon_of(L, "desta:826"))
  cn <- L$canonical[L$canonical$canonical_agreement_id == canon_of(L, "desta:826"), ]
  expect_equal(cn$source_count, 2L)
  # field precedence: DESTA (structured) over GSP for type and status
  expect_equal(cn$agreement_type, "fta")
  expect_equal(cn$implementation_status, "in_force")
  expect_equal(cn$confidence, "high")
})

test_that("distinct agreements and dissimilar titles stay separate; no record is dropped", {
  rec <- rbind(mk("curated:US-CRM-JPN", "curated", "JPN|USA", "framework", "2025-10-28",
                  "United States-Japan Framework for Securing the Supply of Critical Minerals and Rare Earths"),
               mk("pmemo:USJ-SHIP", "pmemo", "JPN|USA", "mou", "2025-10-28",
                  "US-Japan memorandum on shipbuilding cooperation"),
               mk("pmemo:USJ-CMF", "pmemo", "JPN|USA", "framework", "2025-10-28",
                  "US-Japan critical minerals and rare earths framework (Oct 2025)"))
  L <- agr_link_records(rec, agr_empty_codes(), cfg)
  expect_equal(nrow(L$links), 3L)
  expect_equal(canon_of(L, "curated:US-CRM-JPN"), canon_of(L, "pmemo:USJ-CMF"))
  expect_false(canon_of(L, "pmemo:USJ-SHIP") == canon_of(L, "pmemo:USJ-CMF"))
  # two records of an exclusive source (pmemo) are never merged with each other
  expect_true(all(table(L$links$canonical_agreement_id[L$links$source_dataset == "pmemo"]) == 1L))
})

test_that("the same agreement recorded from both sides in the curated table links", {
  rec <- rbind(mk("curated:EU-CRM-USA-2026", "curated", "EUU|USA", "mou", "2026-02-04",
                  "Memorandum of Understanding between the European Union and the United States on critical minerals"),
               mk("curated:US-CRM-EUU-2026", "curated", "EUU|USA", "mou", "2026-02-04",
                  "EU-United States Memorandum of Understanding on Critical Minerals"))
  L <- agr_link_records(rec, agr_empty_codes(), cfg)
  expect_equal(nrow(L$canonical), 1L)
})

test_that("plurilaterals link only on a close party set", {
  q4 <- "AUS|IND|JPN|USA"
  rec <- rbind(mk("curated:QUAD-CRM", "curated", q4, "framework", "2025-07-01", "Quad Critical Minerals Initiative"),
               mk("pmemo:QUAD", "pmemo", q4, "framework", "2025-07-01", "Quad Critical Minerals Initiative (July 2025)"),
               mk("desta:9999", "desta", "AUS|JPN|USA", "framework", "2025-07-01", "Quad Critical Minerals Initiative"))
  L <- agr_link_records(rec, agr_empty_codes(), cfg)
  expect_equal(canon_of(L, "curated:QUAD-CRM"), canon_of(L, "pmemo:QUAD"))
  # party Jaccard 3/4 = 0.75 < party_jaccard_min
  expect_false(canon_of(L, "desta:9999") == canon_of(L, "pmemo:QUAD"))
})

test_that("canonical ids are deterministic hashes of the sorted member ids", {
  expect_equal(agr_canonical_id(c("b:1", "a:2")), agr_canonical_id(c("a:2", "b:1")))
  expect_match(agr_canonical_id("gsp:X"), "^agr-[0-9a-f]{10}$")
  rec <- rbind(mk("gsp:A1", "gsp", "AUS|NZL", "joint_statement", "2010-01-01", "Joint statement"),
               mk("gsp:A2", "gsp", "AUS|NZL", "joint_statement", "2010-01-03", "Joint statement"))
  L1 <- agr_link_records(rec, agr_empty_codes(), cfg)
  L2 <- agr_link_records(rec[2:1, ], agr_empty_codes(), cfg)
  expect_equal(sort(L1$canonical$canonical_agreement_id), sort(L2$canonical$canonical_agreement_id))
  expect_equal(L1$canonical$canonical_agreement_id, agr_canonical_id(c("gsp:A1", "gsp:A2")))
})

test_that("titles lose country names, demonyms and generic words before comparison", {
  sw <- agr_link_stopwords(cfg)
  tk <- agr_title_tokens(c("Joint Statement between Japan and the United States on Critical Minerals",
                           "Japanese-American critical minerals statement"), sw)
  expect_equal(agr_jaccard(tk[[1]], tk[[2]]), 1)
})

test_that("bilateral legs of a small plurilateral trade agreement link to it, not to its successor (trade_leg)", {
  mkf <- function(id, ds, parties, title, sig, eff) {
    r <- mk(id, ds, parties, "fta", sig, title, status = "in_force", legal = "binding")
    r$effective_date <- eff
    r
  }
  rec <- rbind(mkf("memberships:us_fta:CAN", "memberships", "CAN|USA", "NAFTA / USMCA", "", "1994-01-01"),
               mkf("memberships:us_fta:MEX", "memberships", "MEX|USA", "NAFTA / USMCA", "", "1994-01-01"),
               mkf("memberships:us_fta:CRI", "memberships", "CRI|USA", "CAFTA-DR", "", "2009-01-01"),
               mkf("desta:636", "desta", "CAN|MEX|USA", "North American Free Trade Agreement (NAFTA)", "1992", "1994"),
               mkf("desta:1007", "desta", "CAN|MEX|USA", "US Mexico Canada Agreement (USMCA)", "2018", "2020"),
               mkf("desta:188", "desta", "CRI|DOM|GTM|HND|NIC|SLV|USA",
                   "Central American Free Trade Agreement (CAFTA) Dominican Republic", "2004", "2006"),
               mkf("desta:999", "desta", "CRI|USA", "Costa Rica US investment", "2020", "2021"))
  L <- agr_link_records(rec, agr_empty_codes(), cfg)
  expect_equal(canon_of(L, "memberships:us_fta:CAN"), canon_of(L, "desta:636"))
  expect_equal(canon_of(L, "memberships:us_fta:MEX"), canon_of(L, "desta:636"))   # two legs, one source
  expect_false(canon_of(L, "desta:1007") == canon_of(L, "desta:636"))             # USMCA stays apart
  expect_equal(canon_of(L, "memberships:us_fta:CRI"), canon_of(L, "desta:188"))   # entry into force 3 years apart
  expect_true(all(c("trade_leg") %in% L$links$rule))
  cn <- L$canonical[L$canonical$canonical_agreement_id == canon_of(L, "desta:636"), ]
  expect_equal(cn$parties, "CAN|MEX|USA")   # the plurilateral's parties win
})
