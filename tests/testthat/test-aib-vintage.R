# AIB vintages (R/change/aib_vintage.R): pair theses, compact membership and profiles normalized from the strategic
# layer, and the deterministic diff that reads complementarity from shared priority compacts.

av_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(av_root, "R/change/aib_vintage.R"))

av_doc <- function(theses, roles, primary = list(USA = list("RPA"), JPN = list())) {
  list(strategic = list(
    theses = theses,
    compactRoles = lapply(roles, function(r) list(compact = r[[1]], iso3 = r[[2]], role = r[[3]], allied = TRUE)),
    countries = lapply(names(primary), function(i) list(iso3 = i, allied = TRUE, primary = primary[[i]], secondary = list())),
    compacts = list(list(id = "SC-GRID", name = "Grid"), list(id = "SC-EV", name = "Electric vehicles"), list(id = "SC-SOLAR", name = "Solar"))
  ))
}
av_h <- function(id, m = "m-1") list(vintage_id = id, method = list(id = m))

prev <- aib_vintage_from_doc(av_doc(
  theses = list(list("USA", "JPN", list("SC-GRID"), list("trade")), list("AUS", "KOR", list(), list("investment")), list("DEU", "FRA", list("SC-EV"), list("trade"))),
  roles = list(list("SC-GRID", "USA", "Scale manufacturer"), list("SC-GRID", "JPN", "Processing anchor"))))
cur <- aib_vintage_from_doc(av_doc(
  theses = list(list("JPN", "USA", list("SC-GRID", "SC-EV"), list("trade")), list("AUS", "KOR", list(), list("trade")), list("CAN", "MEX", list("SC-SOLAR"), list("trade"))),
  roles = list(list("SC-GRID", "USA", "Demand anchor"), list("SC-EV", "JPN", "Processing anchor")),
  primary = list(USA = list("RPA", "DMA"), JPN = list())))

test_that("theses normalize pair order and count shared priority compacts", {
  expect_equal(prev$theses$a[1], "JPN")
  expect_equal(prev$theses$b[1], "USA")
  expect_equal(prev$theses$n_compacts, c(1L, 0L, 1L))
  expect_null(aib_vintage_from_doc(list(meta = list())))
})

test_that("the diff reads complementarity, membership and profiles deterministically", {
  r <- aib_compare(prev, cur, av_h("v1"), av_h("v2"))
  p <- r$pairs
  jp <- p[p$a == "JPN" & p$b == "USA", ]
  expect_equal(jp$kind, "compacts_changed")
  expect_equal(jp$direction, "more")
  expect_equal(jp$compacts_added, "SC-EV")
  expect_equal(p$kind[p$a == "AUS"], "purposes_changed")
  expect_equal(p$kind[p$a == "CAN"], "new_pair")
  expect_equal(p$kind[p$a == "DEU"], "dropped_pair")
  expect_equal(r$pair$n_more, 2L)          # JPN-USA gained one; CAN-MEX is new with one
  expect_equal(r$pair$n_less, 1L)          # DEU-FRA dropped
  expect_equal(r$pair$attribution, "real_world")
  m <- r$membership
  expect_setequal(paste(m$compact, m$iso3, m$kind), c("SC-GRID USA role_changed", "SC-GRID JPN left", "SC-EV JPN joined"))
  expect_equal(r$profiles$iso3, "USA")
  expect_equal(aib_compare(prev, cur, av_h("v1"), av_h("v2", "m-2"))$pair$attribution, "unattributed")
  ex <- aib_pair_explain(p, cur$compact_names)
  expect_match(ex[p$a == "JPN"], "Shared priority compacts 1 → 2 \\(added Electric vehicles\\)")
  expect_match(ex[p$a == "CAN"], "New relationship thesis with 1 shared priority compact (Solar).", fixed = TRUE)
})
