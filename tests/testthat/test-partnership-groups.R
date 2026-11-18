# Group memos (docs/partnership_memos.md, Group memos): the pure functions in R/partnerships/pm_group.R on synthetic
# members AAA, BBB and DDD with a non-member CCC, so nothing here depends on real country data.

pg_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
for (f in c("R/utils/supply_chain_stages.R", sort(list.files(file.path(pg_root, "R", "partnerships"), pattern = "\\.R$", full.names = TRUE))))
  source(if (file.exists(f)) f else file.path(pg_root, f), local = FALSE)
pg_cfg <- function() yaml::read_yaml(file.path(pg_root, "config", "partnership_memos.yml"))
mem <- c("AAA", "BBB", "DDD")

test_that("priorities name only other members as partners, and Deployment cells are deployment priorities", {
  sp <- data.frame(iso3 = c("AAA", "AAA", "BBB", "DDD", "CCC"), tech = "T1", supply_chain = c("Refining", "Deployment", "Refining", "Assembly", "Refining"),
                   strategic_choice = c("secure", "accelerate", "partner", "catch_up", "secure"), supply_chain_need = c(0.9, NA, 0.8, 0.7, 0.9),
                   deployment_need = c(NA, 0.85, NA, NA, NA), competitive_position_percentile = 0.5, strategic_industrial_value = 0.6,
                   recommended_partner_1 = c("BBB", NA, "AAA", "CCC", "AAA"), recommended_partner_2 = c("CCC", NA, "BBB", "AAA", NA),
                   recommended_partner_3 = c("AAA", NA, NA, NA, NA), stringsAsFactors = FALSE)
  p <- pm_group_priorities(sp, mem)
  expect_false("CCC" %in% p$iso3)
  expect_equal(p$group_partners[p$iso3 == "AAA" & !p$deployment], "BBB")
  expect_equal(p$group_partners[p$iso3 == "BBB"], "AAA")
  expect_equal(p$group_partners[p$iso3 == "DDD"], "AAA")
  expect_true(p$deployment[p$iso3 == "AAA" & p$stage == "Deployment"])
  m <- pm_group_partner_matrix(p, mem)
  expect_equal(unname(m["AAA", "BBB"]), 1L)
  expect_equal(unname(m["DDD", "AAA"]), 1L)
  expect_equal(unname(m["AAA", "AAA"]), 0L)
})

test_that("roles tables carry primary and secondary roles and a dash where a member holds no compact role", {
  sroles <- data.frame(iso3 = c("AAA", "AAA", "BBB", "DDD"), role_code = c("DMA", "CIA", "RPA", "STM"),
                       strategic_role = c("Demand & Market Anchor", "Capital & Investment Anchor", "Resource & Processing Anchor", "Specialized Technology"),
                       primary_or_secondary = c("primary", "secondary", "primary", "none"), role_strength = c(0.9, 0.8, 0.9, 0.5), stringsAsFactors = FALSE)
  croles <- data.frame(strategic_compact_id = c("SC-X", "SC-X"), iso3 = c("AAA", "BBB"), compact_role = c("Demand anchor", "Resource supplier"),
                       lead_node = c("n-1", ""), stringsAsFactors = FALSE)
  compacts <- data.frame(strategic_compact_id = c("SC-X", "SC-Y"), strategic_compact_name = c("Compact X", "Compact Y"), stringsAsFactors = FALSE)
  r <- pm_group_roles(sroles, croles, mem, compacts, c(`n-1` = "Refined metal"))
  expect_equal(r$strategic$primary[r$strategic$iso3 == "AAA"], "Demand & Market Anchor")
  expect_equal(r$strategic$secondary[r$strategic$iso3 == "AAA"], "Capital & Investment Anchor")
  expect_equal(r$strategic$brings[r$strategic$iso3 == "DDD"], "no AIB anchor role")
  x <- r$compact[r$compact$strategic_compact_id == "SC-X", ]
  expect_equal(x$AAA, "Demand anchor (refined metal)")
  expect_equal(x$BBB, "Resource supplier")
  expect_equal(x$DDD, "-")
  expect_true(all(unlist(r$compact[r$compact$strategic_compact_id == "SC-Y", mem]) == "-"))
})

test_that("the pairs table reads the bilateral summaries and marks a pair that has not been run", {
  pairs <- list(AAA_BBB = c("AAA", "BBB"), AAA_DDD = c("AAA", "DDD"))
  sums <- list(AAA_BBB = list(joint_bets = list("n-1", "T1|Refining"), recommended_compacts = list("SC-X"), mea = list(option = "upgrade", host_mechanism = "M1")), AAA_DDD = NULL)
  d <- pm_group_dyads(pairs, sums, c(`n-1` = "Refined metal", `T1|Refining` = "T1 refining"), c(`SC-X` = "Compact X"), published = "AAA_BBB")
  expect_equal(d$n_joint_bets[d$pair == "AAA_BBB"], 2L)
  expect_equal(d$joint_bets[d$pair == "AAA_BBB"], "refined metal; T1 refining")
  expect_equal(d$mea[d$pair == "AAA_BBB"], "upgrade M1")
  expect_true(is.na(d$n_joint_bets[d$pair == "AAA_DDD"]))
  expect_equal(d$joint_bets[d$pair == "AAA_DDD"], "not run")
  expect_true(d$published[d$pair == "AAA_BBB"]); expect_false(d$published[d$pair == "AAA_DDD"])
})

test_that("only a group-wide body can host; member-pair channels make a network; bundles with non-members drop out", {
  cfg <- pg_cfg()
  mech <- data.frame(mechanism_id = c("G1", "B1", "B2", "W1", "X1"), pair = c("AAA|BBB|DDD", "AAA|BBB", "BBB|DDD", "AAA|BBB|CCC", "AAA|CCC|BBB"),
                     name = c("Trio Minerals Initiative", "AAA-BBB framework", "BBB-DDD fund", "Wider forum", "AAA bundle of MOUs"),
                     mechanism_type = c("plurilateral", "framework", "finance_cooperation", "plurilateral", "bilateral_bundle"),
                     status = c("announced", "in_force", "operating", "in_force", "in_force"), evidence_ids = "", scope_compacts = "SC-X",
                     cross_sector = "FALSE", has_pipeline = c("FALSE", "TRUE", "partial", "TRUE", "TRUE"), has_finance = c("TRUE", "TRUE", "TRUE", "TRUE", "TRUE"),
                     has_kpis = "FALSE", has_review = "FALSE", reciprocal = "FALSE", sunset = "", assessed_on = "", notes = "", stringsAsFactors = FALSE)
  cm <- pm_compact_mechanisms(mech, mem, "GRP", "SC-X")
  g <- pm_group_governance(cm, mem, cfg, "Trio")
  expect_equal(g$option, "network")
  expect_true(grepl("G1", g$candidate))
  expect_false("X1" %in% g$mechanisms$mechanism_id)
  expect_true(all(c("G1", "B1", "B2", "W1") %in% g$mechanisms$mechanism_id))
  expect_true(g$mechanisms$group_body[g$mechanisms$mechanism_id == "G1"])
  expect_false(g$mechanisms$group_body[g$mechanisms$mechanism_id == "W1"])
  # the group body once signed with finance hosts: upgrade, never the wider forum
  mech$status[1] <- "signed"
  g2 <- pm_group_governance(pm_compact_mechanisms(mech, mem, "GRP", "SC-X"), mem, cfg, "Trio")
  expect_equal(g2$option, "upgrade"); expect_equal(g2$host_mechanism, "G1")
})

test_that("lane pairs add up flows by supplier and buyer, largest first", {
  lanes <- data.frame(supplier = c("AAA", "AAA", "BBB"), buyer = c("BBB", "BBB", "AAA"), flow_usd = c(5, 3, 4),
                      node_label = c("Part", "Machine", "Ore"), stringsAsFactors = FALSE)
  lp <- pm_group_lane_pairs(lanes)
  expect_equal(lp$flow_usd, c(8, 4))
  expect_equal(lp$nodes[1], "Part;Machine")
  ev <- data.frame(evidence_id = c("E1", "E2", "E3"), countries = c("AAA|BBB", "AAA|CCC", "AAA|BBB|DDD|CCC"), stringsAsFactors = FALSE)
  expect_equal(pm_group_evidence_ids(ev, mem), c("E1", "E3"))
})

test_that("a share correction reads the member's production share at the node and leaves others alone", {
  dol <- data.frame(node_id = c("n1", "n1", "n2"), iso3 = c("AAA", "BBB", "AAA"), position_share = c(0.25, 0.1, 0.3), world_share = c(0.25, 0.1, 0.3),
                    production_share = c(NA, 0.05, 0.2), anchor_position = c(TRUE, FALSE, TRUE), specialist_position = FALSE, stringsAsFactors = FALSE)
  d2 <- pm_apply_share_corrections(dol, list(list(node = "n1", iso3 = "AAA", share = "production"), list(node = "n2", iso3 = "AAA", note = "a note only")))
  expect_equal(d2$position_share, c(0, 0.1, 0.3))
  expect_equal(d2$world_share, c(0, 0.1, 0.3))
  expect_false(d2$anchor_position[1]); expect_true(d2$anchor_position[3])
})

test_that("the group host is the political body that covers the bets; narrower group bodies are its workstreams", {
  cfg <- pg_cfg()
  mech <- data.frame(mechanism_id = c("G1", "G2", "G3", "B1"), pair = c("AAA|BBB|DDD", "AAA|BBB|DDD", "AAA|BBB|DDD", "AAA|BBB"),
                     name = c("Trio ministerial cycle", "Trio solar initiative", "Trio export credit MoC", "AAA-BBB framework"),
                     mechanism_type = c("plurilateral", "plurilateral", "finance_cooperation", "framework"),
                     status = c("operating", "operating", "operating", "in_force"), evidence_ids = "", scope_compacts = c("cross_sector|SC-X", "SC-Y", "cross_sector", "SC-X"),
                     cross_sector = "FALSE", has_pipeline = "FALSE", has_finance = c("partial", "TRUE", "TRUE", "TRUE"),
                     has_kpis = "FALSE", has_review = "FALSE", reciprocal = "FALSE", sunset = "", assessed_on = "", notes = "", stringsAsFactors = FALSE)
  cm <- pm_compact_mechanisms(mech, mem, "GRP", c("SC-X", "SC-Y", "cross_sector"))
  g <- pm_group_governance(cm, mem, cfg, "Trio", bet_compacts = c("SC-X", "SC-Y"))
  expect_equal(g$option, "upgrade")
  expect_equal(g$host_mechanism, "G1")
  expect_true(any(grepl("G2", g$workstreams)))
  expect_true(grepl("G3", g$finance_channel))
})
