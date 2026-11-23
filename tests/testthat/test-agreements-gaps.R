root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
for (f in c("agr_schema.R", "agr_pairs.R", "agr_gaps.R")) source(file.path(root, "R", "agreements", f))
cfg <- yaml::read_yaml(file.path(root, "config", "partnership_agreements.yml"))

# Tiny synthetic inputs. JPN needs Batteries at Extraction, Assembly and Deployment and Solar at
# Extraction; AUS, DEU, USA, CHN and KOR supply.
P <- function(iso3, tech, stage, need, cap, rec = NA_character_, choice = "watch", pp = NA_real_)
  data.frame(iso3 = iso3, tech = tech, supply_chain = stage,
             supply_chain_need = if (stage == "Deployment") NA_real_ else need,
             deployment_need_pct = if (stage == "Deployment") need else NA_real_,
             competitive_position_percentile = cap, recommended_partner_1 = rec,
             recommended_partner_2 = NA_character_, recommended_partner_3 = NA_character_,
             strategic_choice = choice, policy_priority_percentile = pp, stringsAsFactors = FALSE)
pos <- rbind(
  P("JPN", "Batteries", "Extraction", 0.90, 0.60, rec = "AUS", choice = "catch_up", pp = 0.90),
  P("JPN", "Batteries", "Deployment", 0.95, 0.50, pp = 0.50),
  P("JPN", "Batteries", "Assembly", 0.80, 0.40, pp = 0.85),
  P("JPN", "Solar", "Extraction", 0.80, 0.40, choice = "partner", pp = 0.40),
  P("AUS", "Batteries", "Extraction", 0.10, 0.90),
  P("AUS", "Batteries", "Assembly", 0.10, 0.85),
  P("AUS", "Solar", "Extraction", 0.10, 0.30),
  P("USA", "Batteries", "Extraction", 0.20, 0.80),
  P("CHN", "Batteries", "Extraction", 0.10, 0.95),
  P("CHN", "Solar", "Extraction", 0.10, 0.90),
  P("KOR", "Batteries", "Extraction", 0.30, 0.50),
  P("KOR", "Solar", "Extraction", 0.20, 0.80),
  P("DEU", "Batteries", "Assembly", 0.20, 0.90))
pairs <- data.frame(pair = c("AUS|JPN", "JPN|USA", "JPN|KOR", "DEU|JPN"), a = c("AUS", "JPN", "JPN", "DEU"),
                    b = c("JPN", "USA", "KOR", "JPN"), n_agreements = c(2L, 1L, 1L, 1L),
                    depth_all = c(5L, 2L, 4L, 3L), depth_ei = c(4L, 2L, 3L, 3L),
                    agreement_ids = c("agr-a|agr-b", "agr-c", "agr-d", "agr-e"), stringsAsFactors = FALSE)
canonical <- data.frame(canonical_agreement_id = c("agr-a", "agr-b", "agr-c", "agr-d", "agr-e"),
                        implementation_status = c("signed", "inactive", "signed", "in_force", "signed"),
                        n_parties = 2L, stringsAsFactors = FALSE)
canonical_codes <- data.frame(canonical_agreement_id = c("agr-a", "agr-b", "agr-c", "agr-d", "agr-e"), dimension = "sector",
                              code = c("critical_minerals", "batteries", "critical_minerals", "batteries", "critical_minerals"),
                              basis = c("curated", "curated", "text_body", "curated", "curated"),
                              n_hits = c(1L, 1L, 3L, 1L, 1L), stringsAsFactors = FALSE)
compat <- data.frame(reporter_iso = c("JPN", "JPN", "AUS"), partner_iso = c("AUS", "AUS", "JPN"),
                     tech = "Batteries", supply_chain = c("Extraction", "Deployment", "Extraction"),
                     friendshore = c(0.7, 0.8, 0.2), stringsAsFactors = FALSE)
theses <- data.frame(a = "AUS", b = "JPN", priority_compacts = "SC-MAGNETS SC-BATTERIES", stringsAsFactors = FALSE)
# One orientation only: the lookup is symmetric. JPN and CHN are in opposite blocs.
alignment <- data.frame(reporter_iso = c("JPN", "JPN", "JPN", "JPN", "DEU"), partner_iso = c("CHN", "AUS", "USA", "KOR", "JPN"),
                        geo_alignment = c(0, 1, 1, 1, 1), stringsAsFactors = FALSE)

G <- agr_gaps(pairs, canonical, canonical_codes, pos, compat, theses, cfg, alignment_tbl = alignment)
g <- G$gaps
gr <- function(n, s, t = "Batteries") g[g$need_iso3 == n & g$supplier_iso3 == s & g$tech == t, , drop = FALSE]

test_that("the universe is aligned complementarity or coverage, directed and without self pairs", {
  expect_false(any(g$need_iso3 == g$supplier_iso3))
  expect_equal(nrow(gr("JPN", "AUS")), 1L)
  expect_equal(nrow(gr("KOR", "JPN")), 1L)   # covered, so both directions
  expect_equal(nrow(gr("JPN", "CHN")), 0L)   # opposite blocs and no agreement: no row
  expect_equal(nrow(gr("JPN", "CHN", "Solar")), 0L)
  expect_equal(nrow(gr("AUS", "CHN")), 0L)   # AUS has no need
  expect_true(all(AGR_GAP_COLUMNS %in% names(g)))
})

test_that("components are read from the right side, stage by stage", {
  r <- gr("JPN", "AUS")
  expect_equal(r$need, 0.9)             # max over supply stages; Deployment is demand context
  expect_equal(r$capability, 0.9)
  expect_equal(r$recommended, 1L)
  expect_equal(r$n_cells_high, 2L)      # Extraction and Assembly
  expect_equal(r$stages_high, "Extraction|Assembly")
  expect_equal(r$alignment, 1)
  expect_false(r$alignment_blocked)
  expect_equal(r$friendshore, 0.8)      # max over the technology's cells, importer = need side
  expect_equal(r$aib_shared, "SC-BATTERIES|SC-MAGNETS")
  expect_equal(r$covered_grade, "specific")
  expect_equal(r$stages_covered, "Extraction|Refining|Recovery")   # a minerals agreement
  expect_equal(r$covering_agreements, "agr-a")                      # agr-b is inactive
  expect_equal(gr("AUS", "JPN")$friendshore, 0.2)
  expect_equal(gr("JPN", "DEU")$alignment, 1)                       # symmetric lookup
})

test_that("Deployment is demand context: no complementarity, coverage stage or execution gap there", {
  pos2 <- rbind(pos, P("JPN", "Wind", "Deployment", 0.95, 0.10, pp = 0.95), P("DEU", "Wind", "Deployment", 0.10, 0.95))
  G2 <- agr_gaps(pairs, canonical, canonical_codes, pos2, compat, theses, cfg, alignment_tbl = alignment)
  w <- G2$gaps[G2$gaps$tech == "Wind", ]
  expect_false(any(w$complementarity == "high"))       # rows only through minerals coverage upstream
  expect_false(any(grepl("Deployment", G2$gaps$stages_high)))
  expect_false(any(G2$execution_gaps$tech == "Wind"))
  expect_false(any(grepl("Deployment", G2$gaps$stages_covered)))
})

test_that("coverage is stage-aware: a minerals agreement does not cover assembly", {
  r <- gr("JPN", "DEU")
  expect_equal(r$stages_high, "Assembly")
  expect_equal(r$stages_covered, "Extraction|Refining|Recovery")
  expect_equal(r$covered_grade, "none")
  expect_equal(r$covering_agreements, "")
  expect_equal(r$quadrant, "opportunity")
})

test_that("quadrants follow aligned complementarity x stage coverage", {
  expect_equal(gr("JPN", "AUS")$quadrant, "established")
  expect_equal(gr("JPN", "USA")$quadrant, "opportunity")   # text grade only
  expect_equal(gr("JPN", "USA")$covered_grade, "text")
  expect_equal(gr("JPN", "USA")$architecture, "weak")
  expect_equal(gr("KOR", "JPN")$quadrant, "legacy")
  expect_equal(gr("USA", "JPN")$quadrant, "none")
  expect_equal(gr("USA", "JPN")$complementarity, "low")
  expect_equal(gr("JPN", "KOR", "Solar")$quadrant, "opportunity")
})

test_that("a pair in opposite blocs keeps its components but is flagged and not complementary", {
  pairs2 <- rbind(pairs, data.frame(pair = "CHN|JPN", a = "CHN", b = "JPN", n_agreements = 1L, depth_all = 1L,
                                    depth_ei = 3L, agreement_ids = "agr-f", stringsAsFactors = FALSE))
  can2 <- rbind(canonical, data.frame(canonical_agreement_id = "agr-f", implementation_status = "signed", n_parties = 2L))
  cc2 <- rbind(canonical_codes, data.frame(canonical_agreement_id = "agr-f", dimension = "sector", code = "batteries",
                                           basis = "curated", n_hits = 1L))
  g2 <- agr_gaps(pairs2, can2, cc2, pos, compat, theses, cfg, alignment_tbl = alignment)$gaps
  r <- g2[g2$need_iso3 == "JPN" & g2$supplier_iso3 == "CHN" & g2$tech == "Batteries", ]
  expect_equal(nrow(r), 1L)
  expect_true(r$alignment_blocked)
  expect_equal(r$n_cells_high, 1L)
  expect_equal(r$complementarity, "low")
  expect_equal(r$quadrant, "legacy")
  # Without an alignment table nothing is blocked.
  g3 <- agr_gaps(pairs, canonical, canonical_codes, pos, compat, theses, cfg)$gaps
  expect_equal(g3$quadrant[g3$need_iso3 == "JPN" & g3$supplier_iso3 == "CHN" & g3$tech == "Batteries"], "opportunity")
})

test_that("execution gaps are priority stages without specific coverage by an aligned capable supplier", {
  e <- G$execution_gaps
  b <- e[e$iso3 == "JPN" & e$tech == "Batteries", ]
  expect_equal(nrow(b), 1L)
  expect_equal(b$stages, "Assembly")              # Extraction is covered by AUS's agreement
  expect_equal(b$suppliers_without_coverage, "DEU|AUS")
  expect_equal(b$priority, 0.9)
  expect_equal(b$priority_basis, "policy_priority")
  s <- e[e$iso3 == "JPN" & e$tech == "Solar", ]
  expect_equal(s$suppliers_without_coverage, "KOR")   # CHN is in the opposite bloc
  expect_equal(s$priority_basis, "choice_partner")
  expect_false(any(e$iso3 == "KOR"))
})

test_that("unscored sides and missing measures are not read as low complementarity", {
  pairs3 <- rbind(pairs, data.frame(pair = "EUU|JPN", a = "EUU", b = "JPN", n_agreements = 1L, depth_all = 4L,
                                    depth_ei = 4L, agreement_ids = "agr-g", stringsAsFactors = FALSE))
  can3 <- rbind(canonical, data.frame(canonical_agreement_id = "agr-g", implementation_status = "signed", n_parties = 2L))
  cc3 <- rbind(canonical_codes, data.frame(canonical_agreement_id = "agr-g", dimension = "sector", code = "batteries",
                                           basis = "curated", n_hits = 1L))
  pos3 <- rbind(pos, P("NOR", "Batteries", "Extraction", NA_real_, NA_real_))
  pairs3 <- rbind(pairs3, data.frame(pair = "JPN|NOR", a = "JPN", b = "NOR", n_agreements = 1L, depth_all = 3L,
                                     depth_ei = 3L, agreement_ids = "agr-g", stringsAsFactors = FALSE))
  g3 <- agr_gaps(pairs3, can3, cc3, pos3, compat, theses, cfg, alignment_tbl = alignment)$gaps
  expect_false(any(g3$need_iso3 == "EUU" | g3$supplier_iso3 == "EUU"))     # the EU has no need or capability
  r <- g3[g3$need_iso3 == "NOR" & g3$supplier_iso3 == "JPN" & g3$tech == "Batteries", ]
  expect_equal(r$complementarity, "not_assessed")                            # NOR has no need measured
  expect_equal(r$quadrant, "none")
})

test_that("empty inputs give empty, typed tables", {
  G0 <- agr_gaps(pairs[0, ], canonical[0, ], canonical_codes[0, ], pos[0, ], NULL, NULL, cfg)
  expect_equal(nrow(G0$gaps), 0L)
  expect_equal(nrow(G0$execution_gaps), 0L)
})
