# CM-FDI in Sector Insights (R/dashboard/sector/sector_fdi.R; contract.ts FDI_SPEC): the cleaning of
# the CSV, the source links, the aggregates (flows, parent-HQ group splits, announcements by year) on
# the synthetic extract, the chunk and its contract checks, and parity with the CSV. The last test
# runs on the staged CSV (data/raw/cm_fdi_projects.csv) when it exists.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "dashboard", "sector", "sector_files.R"))
for (f in sector_files()) source(file.path(repo_root, f))

raw <- sector_synthetic_cmfdi_raw()
clean <- sector_fdi_clean(raw, sector_synthetic_iso3)
geo <- sector_synthetic_geo()
fdi <- sector_fdi_part(clean, geo)
pr <- fdi$projects
P <- function(id) pr[pr$pid == id, , drop = FALSE]

test_that("the cleaning keeps every project and states its rules", {
  expect_identical(nrow(pr), nrow(raw))
  expect_setequal(pr$pid, raw[["Project ID"]])
  # undisclosed capital investment: NA with status nq, never 0
  expect_true(is.na(P("HAR0011")$value))
  expect_identical(P("HAR0011")$value_status, "nq")
  expect_identical(P("SOL2")$value, 120.5)
  expect_identical(P("SOL2")$value_status, "obs")
  expect_false(any(pr$value %in% 0))
  # Year Completed: "N/A" and blank -> NA; a planned year is kept
  expect_true(is.na(P("HAR0011")$year_completed))
  expect_true(is.na(P("SOL2")$year_completed))
  expect_identical(P("SOL1")$year_completed, 2027L)
  # technology -> the Midstream cell; the CSV's label is kept
  expect_identical(P("EVX")$item_key, "cell:electric-vehicles-midstream")
  expect_identical(P("EVX")$technology, "EV")
  expect_identical(unique(pr$item_key[pr$technology == "Battery"]), "cell:batteries-midstream")
  # flags by the indices' settings: manufacturing = "Manufacturing" only; active drops Cancelled,
  # Paused and Closed, and plants marked "Ceased operations" or "Paused"; index_use = both
  expect_false(P("SOL3")$manufacturing)
  expect_false(P("EVZ")$manufacturing)
  expect_identical(pr$active, !pr$status %in% c("Cancelled", "Paused", "Closed") & !pr$operational %in% c("Ceased operations", "Paused"))
  expect_false(P("WND1")$active)
  expect_identical(pr$index_use, pr$manufacturing & pr$active)
  expect_identical(sort(pr$pid[pr$index_use]), sort(c("EV1", "HAR0010", "HAR0011", "RUB1", "SOL1", "WND3", "EVX", "EVY")))
  # an announcement window from the settings
  s15 <- sector_fdi_settings(list(announced_from = 2015L))
  c15 <- sector_fdi_clean(raw, sector_synthetic_iso3, s15)$projects
  expect_false(c15$index_use[c15$pid == "EVY"])
  expect_identical(sum(c15$index_use), 7L)
  expect_identical(sector_fdi_settings(list(announced_from = NULL, jv_parent_share = 0.5))$announced_from, NA_integer_)
  # flags; blank optional text -> NA
  expect_true(P("HAR0011")$jv && P("HAR0011")$jv_local && isTRUE(P("HAR0011")$jv_local_state))
  expect_true(is.na(P("EV1")$jv_local_state))
  expect_true(is.na(P("RUB1")$city))
  expect_identical(P("RUB1")$phase, "2")
  expect_true(P("RUB1")$expansion)
  expect_identical(P("HKB1")$home, "HKG")
  expect_identical(P("WND1")$city, "Camaçari (Bahia)")
  # sorted by (item, year announced, pid)
  expect_identical(order(pr$item_key, pr$year_announced, pr$pid, method = "radix"), seq_len(nrow(pr)))
})

test_that("the cleaning stops on anything not reviewed", {
  bad <- function(edit) {
    r <- raw
    r <- edit(r)
    tryCatch({ sector_fdi_clean(r, sector_synthetic_iso3); "" }, error = conditionMessage)
  }
  expect_match(bad(function(r) { r[["Project Status"]][1] <- "Suspended"; r }), "unknown Project Status")
  expect_match(bad(function(r) { r[["Capital Investment (US$m)"]][1] <- "0"; r }), "is 0")
  expect_match(bad(function(r) { r[["Capital Investment (US$m)"]][1] <- "about 5bn"; r }), "not a number")
  expect_match(bad(function(r) { r[["Destination Country"]][1] <- "Atlantis"; r }), "destination country without an ISO3")
  expect_match(bad(function(r) { r[["Project ID"]][2] <- r[["Project ID"]][1]; r }), "duplicate Project IDs")
  expect_match(bad(function(r) { r[["Technology"]][1] <- "Hydrogen"; r[["Sector"]][1] <- "Hydrogen"; r }), "unknown Technology")
  expect_match(bad(function(r) { r[["Year Completed"]][1] <- "soon"; r }), "neither a year nor N/A")
  expect_match(bad(function(r) { r[["Joint Venture"]][1] <- "yes"; r }), "is not 0 / 1")
  expect_match(bad(function(r) { r[["Destination Country"]][1] <- r[["Company Country (HQ)"]][1]; r }), "not cross-border")
  expect_match(bad(function(r) { r[["Operational"]] <- NULL; r }), "columns missing")
  expect_match(bad(function(r) { r[["Parent company"]][1] <- "Acme\nCorp"; r }), "control characters")
})

test_that("source cells ship verbatim with their clean http(s) links", {
  u <- sector_fdi_urls(c("[UNVERIFIED] https://a.org/x, https://b.org/y?q=1 (note)", "fDi Markets (FT)",
                         "https://c.org/p?a=1|2 03/23/2024", "javascript:alert(1) http://d.org/\"q\"", NA))
  expect_identical(u[[1]], c("https://a.org/x", "https://b.org/y?q=1"))
  expect_identical(u[[2]], character())
  expect_identical(u[[3]], "https://c.org/p?a=1%7C2")
  expect_identical(u[[4]], "http://d.org/%22q%22")
  expect_identical(u[[5]], character())
  s <- fdi$sources
  a <- s[s$pid == "SOL1" & s$field == "amount", , drop = FALSE]
  expect_identical(a$text, raw[["Source (Amount)"]][raw[["Project ID"]] == "SOL1"])
  expect_identical(a$url, "https://example.org/cmfdi/sol1|https://example.com/news/sol1-plant")
  expect_true(a$unverified)
  w <- s[s$pid == "WND1" & s$field == "amount", , drop = FALSE]
  expect_true(is.na(w$url))
  expect_identical(w$text, "fDi Markets (FT)")
  # blank source cells have no row
  expect_false(any(s$pid == "EV1" & s$field == "ownership"))
})

test_that("flows, group splits and years add up, and undisclosed values are never zero", {
  expect_identical(sector_fdi_problems(fdi, geo), character())
  fl <- fdi$flows
  f <- fl[fl$item_key == "cell:batteries-midstream" & fl$home == "CHN" & fl$dest == "IDN" & fl$basis == "all", , drop = FALSE]
  expect_identical(c(f$n, f$n_value), c(1L, 0L))
  expect_true(is.na(f$value))
  expect_identical(sum(fl$n[fl$basis == "all"]), nrow(pr))
  expect_identical(sum(fl$n[fl$basis == "index_use"]), sum(pr$index_use))
  g <- fdi$groups
  w <- g[g$item_key == "cell:wind-midstream" & g$dest == "WLD", , drop = FALSE]
  wa <- w[w$basis == "all", , drop = FALSE]
  expect_identical(wa$group, SECTOR_FDI_GROUPS)
  expect_identical(wa$n, c(2L, 1L, 0L, 0L))
  expect_identical(wa$value, c(260, 80, 0, 0))
  expect_equal(wa$share_value, c(260, 80, 0, 0) / 340)
  expect_equal(wa$share_n, c(2, 1, 0, 0) / 3)
  # the indices' projects drop the paused Goldwind plant and the Nordex plant that ceased operations
  wm <- w[w$basis == "index_use", , drop = FALSE]
  expect_identical(wm$n, c(1L, 0L, 0L, 0L))
  expect_identical(wm$value, c(60, 0, 0, 0))
  expect_identical(wm$share_value, c(1, 0, 0, 0))
  # a group with projects but no disclosed value: NA value and share, not zero
  bi <- g[g$item_key == "cell:batteries-midstream" & g$dest == "IDN" & g$basis == "all", , drop = FALSE]
  expect_identical(bi$n[bi$group == "china"], 1L)
  expect_true(is.na(bi$value[bi$group == "china"]))
  expect_true(all(is.na(bi$share_value)))
  # Hong Kong parents count as Chinese (settings china_hq_iso3, as in the indices), not rest
  expect_identical(P("HKB1")$home_group, "china")
  expect_identical(P("RUB1")$home_group, "rest")
  expect_identical(P("EV1")$home_group, "core")
  hk <- g[g$item_key == "cell:batteries-midstream" & g$dest == "POL" & g$basis == "all", , drop = FALSE]
  expect_identical(hk$n[hk$group == "china"], 1L)
  expect_identical(hk$n[hk$group == "rest"], 0L)
  only_chn <- sector_fdi_part(clean, geo, sector_fdi_settings(list(china_hq_iso3 = "CHN")))
  expect_identical(only_chn$projects$home_group[only_chn$projects$pid == "HKB1"], "rest")
  y <- fdi$years
  expect_identical(sum(y$n[y$basis == "all"]), nrow(pr))
  expect_true(all(y$n >= 1L))
  y24 <- y[y$item_key == "cell:batteries-midstream" & y$year == 2024L & y$basis == "all", , drop = FALSE]
  expect_true(is.na(y24$value))
})

synthetic_bundle <- function() {
  dict <- sector_dictionary_tables(yaml::read_yaml(file.path(repo_root, "config", "sector_dictionary.yml")), fill = sector_synthetic_fill())
  parts <- sector_synthetic_parts(dict)
  have <- unique(sector_data_keys(parts, parts$geo)$var)
  sector_assemble(parts, dict, ship_vars = dict$vars$id[dict$vars$id %in% have])
}
bundle <- synthetic_bundle()
chunks <- sector_chunks(bundle)
meta <- dash_osb_decode(dash_osb_encode(chunks$`sector-meta`, "sector-meta", "chunk"))$objects
sizes <- list(geo = nrow(meta$dims$geo), items = nrow(meta$dims$items), vars = nrow(meta$dict$vars))
dec <- dash_osb_decode(dash_osb_encode(chunks$`sector-fdi`, "sector-fdi", "chunk"))$objects

test_that("the measures, availability, rosters and section cards know the CM-FDI data", {
  v <- bundle$dict$vars
  expect_true(all(SECTOR_FDI_VARS %in% v$id))
  expect_true(all(v$source[v$id %in% SECTOR_FDI_VARS] == "cmfdi"))
  expect_true(all(v$licence[v$id %in% SECTOR_FDI_VARS] == "attribution"))
  # years come from the announcement years in the data
  expect_identical(v$years[v$id == "investment.fdi_projects"], paste(sort(unique(pr$year_announced)), collapse = "|"))
  expect_false(any(grepl("\\{", c(v$data_year, bundle$dict$caveats$text, bundle$dict$sources$transform, bundle$dict$sources$issues))))
  src <- bundle$dict$sources[bundle$dict$sources$id == "cmfdi", , drop = FALSE]
  expect_match(src$attribution, "CC BY 4.0")
  expect_match(src$citation, "Bandara, P. and the Net Zero Industrial Policy Lab")
  av <- bundle$avail[bundle$avail$var %in% SECTOR_FDI_VARS, , drop = FALSE]
  expect_true(all(av$chunk == "fdi"))
  expect_setequal(unique(av$item_key), unique(pr$item_key))
  expect_identical(av$n_geo[av$var == "investment.fdi_projects" & av$item_key == "cell:wind-midstream"], 1L)
  expect_setequal(bundle$rosters$geo[bundle$rosters$roster == "cmfdi"], unique(pr$dest))
  expect_setequal(bundle$rosters$geo[bundle$rosters$roster == "cmfdi_home"], unique(pr$home))
  cards <- bundle$dict$cells$section_cards[bundle$dict$cells$slug == "wind-midstream"]
  expect_true(all(SECTOR_FDI_VARS %in% strsplit(cards, "|", fixed = TRUE)[[1]]))
  # never in facts or the summary
  expect_false(any(bundle$facts$var %in% SECTOR_FDI_VARS))
  expect_false(any(bundle$summary$var %in% SECTOR_FDI_VARS))
})

test_that("sector-fdi round-trips, passes the contract checks and the checks catch breaks", {
  expect_identical(dec, dash_osb_normalize(chunks$`sector-fdi`))
  expect_identical(sector_check_chunk("sector-fdi", dec, sizes), character())
  p <- dec$fdi$projects
  expect_type(p$value, "double")
  expect_type(p$year_completed, "integer")
  expect_type(p$jv_local_state, "logical")
  expect_identical(nrow(p), nrow(pr))
  # fdi.sources.project points at the right project
  s <- dec$fdi$sources
  expect_identical(p$pid[s$project + 1L][s$field == "amount" & s$unverified], "SOL1")
  chk <- function(edit) paste(sector_check_chunk("sector-fdi", edit(dec), sizes), collapse = "\n")
  expect_match(chk(function(x) { i <- which(x$fdi$projects$value_status == "nq")[1]; x$fdi$projects$value[i] <- 5; x }), "value NaN <=> value_status nq")
  expect_match(chk(function(x) { i <- which(x$fdi$projects$value_status == "obs")[1]; x$fdi$projects$value[i] <- 0; x }), "zero or negative")
  expect_match(chk(function(x) { x$fdi$sources$url[1] <- "javascript:alert(1)"; x }), "not a clean http")
  expect_match(chk(function(x) { n <- nrow(x$fdi$sources); x$fdi$sources$project[n] <- nrow(x$fdi$projects); x }), "outside fdi.projects")
  expect_match(chk(function(x) { x$fdi$groups$share_n[1] <- x$fdi$groups$share_n[1] + 0.5; x }), "does not sum to 1")
  expect_match(chk(function(x) { i <- which(x$fdi$flows$n_value == 0L)[1]; x$fdi$flows$value[i] <- 0; x }), "value NaN <=> n_value 0")
  expect_match(chk(function(x) { x$fdi$projects$status[1] <- "Suspended"; x }), "outside the vocabulary")
  expect_match(chk(function(x) { i <- which(!x$fdi$projects$manufacturing)[1]; x$fdi$projects$index_use[i] <- TRUE; x }), "index_use without")
  expect_match(chk(function(x) { x$fdi$projects$home_group[1] <- "na"; x }), "outside the vocabulary")
})

test_that("decoded projects are identical() to the CSV after cleaning; aggregates recompute", {
  pf <- sector_fdi_parity(dec, meta, raw, sector_synthetic_iso3)
  expect_identical(pf$status, rep("PASS", nrow(pf)))
  r2 <- raw
  r2[["Parent company"]][1] <- "Someone else"
  pf2 <- sector_fdi_parity(dec, meta, r2, sector_synthetic_iso3)
  expect_match(paste(pf2$detail[pf2$status == "FAIL"], collapse = " "), "Parent company")
  r3 <- raw
  r3[["Source (Amount)"]][1] <- "https://elsewhere.example/"
  expect_identical(sector_fdi_parity(dec, meta, r3, sector_synthetic_iso3)$status[3], "FAIL")
  # flags and groups must follow the settings the export used
  pf4 <- sector_fdi_parity(dec, meta, raw, sector_synthetic_iso3, sector_fdi_settings(list(china_hq_iso3 = "CHN")))
  expect_identical(pf4$status[4], "FAIL")
})

test_that("the staged CM-FDI edition cleans as reviewed (skipped without data/raw/cm_fdi_projects.csv)", {
  p <- file.path(repo_root, "data", "raw", "cm_fdi_projects.csv")
  skip_if_not(file.exists(p), "data/raw/cm_fdi_projects.csv not staged")
  source(file.path(repo_root, "R", "utils", "country.R"))
  r <- utils::read.csv(p, colClasses = "character", check.names = FALSE, na.strings = character(), fileEncoding = "UTF-8-BOM",
                       stringsAsFactors = FALSE)
  cl <- sector_fdi_clean(r, country_name_to_iso3)
  x <- cl$projects
  expect_identical(nrow(x), nrow(r))
  expect_gte(nrow(x), 1042L)
  # edition 2026.10.05: 1,042 projects, 974 manufacturing, 259 undisclosed, 66 destinations, 30 home economies, 423 parents
  if (nrow(x) == 1042L) {
    expect_identical(sum(x$manufacturing), 974L)
    expect_identical(sum(is.na(x$value)), 259L)
    expect_identical(length(unique(x$dest)), 66L)
    expect_identical(length(unique(x$home)), 30L)
    expect_identical(length(unique(x$parent)), 423L)
    expect_identical(sum(x$jv), 215L)
  }
  u <- unlist(strsplit(stats::na.omit(cl$sources$url), "|", fixed = TRUE))
  expect_true(all(grepl("^https?://", u)))
})
