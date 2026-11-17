repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "strategic_position", "current_trade.R"))

test_that("month arithmetic crosses year ends", {
  expect_equal(ctm_month_add(202601L, -1L), 202512L)
  expect_equal(ctm_month_add(202512L, 1L), 202601L)
  expect_equal(ctm_window_months(202608L, 12L), c(202509:202512, 202601:202608))
})

test_that("windows end at each reporter's latest month and drop stale or gappy reporters", {
  da <- rbind(
    data.frame(reporter_iso = "DEU", period = ctm_window_months(202608L, 14L)),
    data.frame(reporter_iso = "KOR", period = ctm_window_months(202512L, 12L)),
    data.frame(reporter_iso = "CHN", period = ctm_window_months(202412L, 12L)),
    data.frame(reporter_iso = "GAP", period = ctm_window_months(202608L, 12L)[c(1, 3, 5, 7, 9, 11, 12)])
  )
  w <- ctm_reporter_windows(da, min_end = 202512L)
  expect_setequal(w$reporter_iso, c("DEU", "KOR"))
  expect_equal(w$window_start[w$reporter_iso == "DEU"], 202509L)
  expect_equal(w$window_end[w$reporter_iso == "KOR"], 202512L)
  d <- attr(w, "dropped")
  expect_equal(d$reason[d$reporter_iso == "CHN"], "stale")
  expect_equal(d$reason[d$reporter_iso == "GAP"], "gappy")
})

monthly_fixture <- function() {
  months <- ctm_window_months(202608L, 12L)
  grid <- expand.grid(period = months, cmd_code = sprintf("%06d", 1:10), partner_iso = c("CHN", "KOR"), stringsAsFactors = FALSE)
  grid$reporter_iso <- "USA"
  grid$flow_direction <- "import"
  grid$primary_value <- ifelse(grid$partner_iso == "CHN", 3, 1)
  # A partial April release: only two codes.
  grid <- grid[!(grid$period == 202604L & !grid$cmd_code %in% c("000001", "000002")), ]
  # A month before the window that must not count.
  extra <- grid[grid$period == 202608L, ]
  extra$period <- 202508L
  rbind(grid, extra)
}

test_that("completeness flags a partial month and T12M sums scale to twelve months", {
  m <- monthly_fixture()
  comp <- ctm_month_completeness(m)
  expect_false(comp$complete[comp$period == 202604L])
  expect_true(all(comp$complete[comp$period != 202604L]))
  w <- data.frame(reporter_iso = "USA", window_end = 202608L)
  t <- ctm_t12m_dyads(m, w, comp, min_months = 10L)
  expect_equal(unique(t$months_used), 11L)
  chn <- t[t$partner_iso == "CHN" & t$cmd_code == "000003", ]
  expect_equal(chn$primary_value, 3 * 11 * 12 / 11)
  expect_equal(unique(t$period), 9999L)
  expect_error(ctm_t12m_dyads(m, w, comp, min_months = 12L), "no reporter-flow")
})

test_that("compare keeps only own monthly reports as current and flags large China moves", {
  base <- data.frame(importer_iso = c("USA", "AUT", "IND"), tech = "Solar", supply_chain = "Assembly", observed = TRUE,
                     imports_usd = c(5e9, 1e9, 3e9), china_share = c(0.5, 0.4, 0.8), top_supplier_iso3 = c("CHN", "CHN", "CHN"),
                     top_supplier_share = c(0.5, 0.4, 0.8), import_hhi = c(0.3, 0.2, 0.65))
  current <- data.frame(importer_iso = c("USA", "AUT", "IND"), tech = "Solar", supply_chain = "Assembly", observed = TRUE,
                        import_source = c("importer", "mirror", "importer"), imports_usd = c(6e9, 9e8, 2e9),
                        china_share = c(0.45, 0.05, 0.5), top_supplier_iso3 = c("CHN", "DEU", "MYS"),
                        top_supplier_share = c(0.45, 0.5, 0.3), import_hhi = c(0.28, 0.3, 0.3))
  cmp <- ctm_exposure_compare(base, current)
  aut <- cmp[cmp$importer_iso == "AUT", ]
  expect_equal(aut$current_basis, "no_monthly_report")
  expect_true(is.na(aut$china_share_now))
  expect_false(aut$flag_china_move)
  ind <- cmp[cmp$importer_iso == "IND", ]
  expect_equal(ind$china_share_change, -0.3)
  expect_true(ind$flag_china_move)
  expect_true(ind$top_supplier_changed)
  expect_false(cmp$flag_china_move[cmp$importer_iso == "USA"])
})

blend_fixture <- function() {
  base <- data.frame(importer_iso = c("USA", "AUT", "IND", "NEW"), tech = "Solar", supply_chain = "Assembly",
                     observed = c(TRUE, TRUE, TRUE, FALSE), china_share = c(0.6, 0.4, 0.8, NA),
                     coercive_import_share = c(0.6, 0.4, 0.8, NA), import_hhi = c(0.4, 0.2, 0.65, NA),
                     imports_usd = c(10, 1, 3, NA), top_supplier_iso3 = "CHN", stringsAsFactors = FALSE)
  current <- data.frame(importer_iso = c("USA", "AUT", "IND", "NEW"), tech = "Solar", supply_chain = "Assembly",
                        observed = TRUE, current_basis = c("own_monthly", "no_monthly_report", "own_monthly", "own_monthly"),
                        china_share = c(0.2, 0.0, 0.5, 0.9), coercive_import_share = c(0.2, 0, 0.5, 0.9),
                        import_hhi = c(0.3, 0.5, 0.4, 0.8), imports_usd = c(20, 2, 3, 5), window_end = 202608L,
                        stringsAsFactors = FALSE)
  list(base = base, current = current)
}

test_that("blend averages own-monthly cells, keeps the rest and records its basis", {
  f <- blend_fixture()
  b <- ctm_blend_exposure(f$base, f$current, weight = 0.5)
  usa <- b[b$importer_iso == "USA", ]
  expect_equal(usa$china_share, 0.4)
  expect_equal(usa$imports_usd, 15)
  expect_equal(usa$china_share_base, 0.6)
  expect_equal(usa$china_share_t12m, 0.2)
  expect_equal(usa$exposure_basis, "blend")
  expect_equal(usa$exposure_window_end, 202608L)
  aut <- b[b$importer_iso == "AUT", ]
  expect_equal(aut$china_share, 0.4)
  expect_equal(aut$exposure_basis, "base")
  new <- b[b$importer_iso == "NEW", ]
  expect_true(new$observed)
  expect_equal(new$china_share, 0.9)
  expect_equal(new$exposure_basis, "current_only")
  expect_equal(b$top_supplier_iso3, rep("CHN", 4))
})

test_that("weight 0 reproduces the base and weight 1 takes the current value", {
  f <- blend_fixture()
  b0 <- ctm_blend_exposure(f$base, f$current, weight = 0)
  expect_equal(b0$china_share, f$base$china_share[order(f$base$importer_iso)])
  expect_equal(b0$observed, f$base$observed[order(f$base$importer_iso)])
  expect_true(all(b0$exposure_basis == "base"))
  b1 <- ctm_blend_exposure(f$base, f$current, weight = 1)
  expect_equal(b1$china_share[b1$importer_iso == "IND"], 0.5)
  expect_error(ctm_blend_exposure(f$base, f$current, weight = 1.5), "weight")
})

test_that("trade flags mark large China moves on blended cells only", {
  f <- blend_fixture()
  f$base$imports_usd <- c(5e8, 1e9, 3e8, NA)
  f$current$imports_usd <- c(5e8, 1e9, 3e8, 5e8)
  fl <- ctm_trade_flags(ctm_blend_exposure(f$base, f$current, weight = 0.5))
  expect_equal(fl$current_trade_flag[fl$iso3 == "USA"], "china_down")
  expect_true(is.na(fl$current_trade_flag[fl$iso3 == "AUT"]))
  expect_true(is.na(fl$current_trade_flag[fl$iso3 == "NEW"]))
  expect_equal(fl$exposure_window_end[fl$iso3 == "IND"], 202608L)
})

test_that("panel supplier shares use importers that report both years", {
  tc <- data.frame(
    exporter_iso = c("CHN", "KOR", "CHN", "KOR", "CHN"), importer_iso = c("USA", "USA", "DEU", "DEU", "AUT"),
    tech = "Batteries", supply_chain = "Assembly", value_first = c(60, 40, 50, 50, 99), value_last = c(30, 70, 80, 20, 99),
    source_first = c("importer", "importer", "importer", "importer", "mirror"),
    source_last = c("importer", "importer", "importer", "importer", "mirror"), stringsAsFactors = FALSE)
  p <- ctm_panel_supplier_shares(tc)
  chn <- p[p$exporter_iso == "CHN", ]
  expect_equal(chn$panel_share_base, 110 / 200)
  expect_equal(chn$panel_share_now, 110 / 200)
  expect_equal(chn$panel_n_importers, 2L)
  expect_equal(p$panel_share_change[p$exporter_iso == "KOR"], 0)
})

test_that("export shares move by weight x panel change, keep the cell total and skip thin panels", {
  mss <- data.frame(iso3 = c("CHN", "KOR", "JPN", "CHN"), tech = c("Batteries", "Batteries", "Batteries", "Solar"),
                    supply_chain = "Assembly", measure = "atlas_export_share", year = 2024L, is_aggregate = FALSE,
                    world_share = c(0.6, 0.2, 0.2, 0.8), value = c(0.6, 0.2, 0.2, 0.8), stringsAsFactors = FALSE)
  panel <- data.frame(exporter_iso = c("CHN", "KOR", "CHN"), tech = c("Batteries", "Batteries", "Solar"), supply_chain = "Assembly",
                      panel_share_change = c(-0.2, 0.2, 0.1), panel_n_importers = c(20L, 20L, 3L), panel_usd_now = 1e10)
  u <- ctm_update_export_shares(mss, panel, weight = 0.5)
  b <- u[u$tech == "Batteries", ]
  expect_equal(sum(b$world_share), 1)
  expect_equal(b$world_share[b$iso3 == "CHN"], 0.5)
  expect_equal(b$world_share[b$iso3 == "KOR"], 0.3)
  expect_equal(b$atlas_share[b$iso3 == "CHN"], 0.6)
  expect_true(all(b$share_basis == "atlas_current"))
  expect_equal(u$world_share[u$tech == "Solar"], 0.8)
  expect_equal(u$share_basis[u$tech == "Solar"], "atlas")
  expect_equal(ctm_update_export_shares(mss, panel, weight = 0)$world_share, mss$world_share)
})

test_that("import partners blend for reporters with current data only", {
  base <- data.frame(reporter_iso = c("USA", "USA", "AUT"), partner_iso = c("CHN", "KOR", "DEU"), tech = "Batteries",
                     supply_chain = "Assembly", imports_usd = c(100, 50, 10))
  now <- data.frame(reporter_iso = c("USA", "USA"), partner_iso = c("CHN", "MEX"), tech = "Batteries",
                    supply_chain = "Assembly", imports_usd = c(40, 60))
  b <- ctm_blend_import_partners(base, now, weight = 0.5)
  usa <- stats::setNames(b$imports_usd[b$reporter_iso == "USA"], b$partner_iso[b$reporter_iso == "USA"])
  expect_equal(usa[["CHN"]], 70)
  expect_equal(usa[["KOR"]], 25)
  expect_equal(usa[["MEX"]], 30)
  expect_equal(b$imports_usd[b$reporter_iso == "AUT"], 10)
})

test_that("Ember fill scales China and the other suppliers separately and respects coverage", {
  base <- data.frame(importer_iso = c("VNM", "VNM", "AUT"), tech = c("Solar", "Wind", "Solar"), supply_chain = "Assembly",
                     observed = TRUE, imports_usd = c(100, 100, 50), china_share = c(0.6, 0.5, 0.2),
                     coercive_import_share = c(0.7, 0.5, 0.2), opposite_bloc_share = c(0, 0, 0.2), trusted_import_share = c(0.1, 0.2, 0.5))
  code_cells <- data.frame(hs6 = c("854143", "850440", "850231"), tech = c("Solar", "Solar", "Wind"), supply_chain = "Assembly")
  ember <- data.frame(importer_iso = c("VNM", "VNM", "AUT"), hs6 = c("854143", "850231", "854143"),
                      usd_latest = c(40, 10, 30), usd_base = c(20, 10, 10))
  china_x24 <- data.frame(importer_iso = c("VNM", "VNM", "VNM", "AUT"), hs6 = c("854143", "850440", "850231", "854143"),
                          usd = c(50, 10, 10, 5))
  others <- data.frame(importer_iso = c("VNM", "VNM", "AUT"), tech = c("Solar", "Wind", "Solar"), supply_chain = "Assembly",
                       x24 = c(40, 10, 40), xnow = c(20, 10, 40))
  flags <- data.frame(importer_iso = c("VNM", "AUT"), coercive = c(TRUE, FALSE), opposite = c(FALSE, TRUE), trusted = FALSE)
  f <- ctm_ember_fill(base, c("VNM", "AUT"), code_cells, ember, china_x24, others, flags, window_end = 202608L, min_base_usd = 0)
  vs <- f[f$importer_iso == "VNM" & f$tech == "Solar", ]
  # China 60 x 2 = 120; others 40 x 0.5 = 20; total 140.
  expect_equal(vs$imports_usd, 140)
  expect_equal(vs$china_share, 120 / 140)
  # Coercive: (70 - 60) x 0.5 + 120 = 125.
  expect_equal(vs$coercive_import_share, 125 / 140)
  expect_equal(vs$trusted_import_share, 10 * 0.5 / 140)
  expect_equal(vs$fill_china_cov, 50 / 60)
  expect_equal(vs$current_basis, "ember_fill")
  expect_true(is.na(vs$import_hhi))
  # Wind: others' coverage 10 / 50 is below 0.5, so no fill.
  expect_false(any(f$importer_iso == "VNM" & f$tech == "Wind"))
  au <- f[f$importer_iso == "AUT", ]
  expect_equal(au$china_share, 30 / (30 + 40))
  expect_equal(au$opposite_bloc_share, 30 / 70)
  small <- ctm_ember_fill(base, c("VNM", "AUT"), code_cells, ember, china_x24, others, flags, min_base_usd = 15)
  expect_false(any(small$importer_iso == "AUT"))
})

test_that("blend takes Ember-fill rows column by column and leaves missing columns at base", {
  base <- data.frame(importer_iso = "VNM", tech = "Solar", supply_chain = "Assembly", observed = TRUE,
                     china_share = 0.6, import_hhi = 0.4, imports_usd = 100)
  cur <- data.frame(importer_iso = "VNM", tech = "Solar", supply_chain = "Assembly", observed = TRUE, current_basis = "ember_fill",
                    china_share = 0.8, import_hhi = NA_real_, imports_usd = 140, window_end = 202608L)
  b <- ctm_blend_exposure(base, cur, weight = 0.5)
  expect_equal(b$china_share, 0.7)
  expect_equal(b$import_hhi, 0.4)
  expect_equal(b$exposure_current_source, "ember_fill")
})

test_that("current dyads take T12M where reported, carry the rest and scale China rows by Ember", {
  annual <- data.frame(
    reporter_iso = c("USA", "USA", "VNM", "VNM", "CHN", "CHN", "USA"),
    partner_iso = c("CHN", "KOR", "CHN", "KOR", "VNM", "USA", "CHN"),
    period = c(2024L, 2024L, 2024L, 2024L, 2024L, 2024L, 2020L),
    flow_direction = c("Import", "Import", "Import", "Import", "Export", "Export", "Import"),
    cmd_code = "854143", primary_value = c(100, 50, 80, 20, 70, 90, 30), stringsAsFactors = FALSE)
  t12m <- data.frame(reporter_iso = "USA", partner_iso = "CHN", flow_direction = "import", cmd_code = "854143", primary_value = 40)
  eg <- data.frame(importer_iso = c("VNM", "USA"), hs6 = "854143", growth = c(1.5, 0.5))
  d <- ctm_current_dyads(annual, t12m, eg)
  cur <- d[d$period == 2024L, ]
  expect_equal(cur$primary_value[cur$reporter_iso == "USA"], 40)
  expect_false(any(cur$reporter_iso == "USA" & cur$partner_iso == "KOR"))
  expect_equal(cur$primary_value[cur$reporter_iso == "VNM" & cur$partner_iso == "CHN"], 120)
  expect_equal(cur$current_source[cur$reporter_iso == "VNM" & cur$partner_iso == "CHN"], "carried_ember")
  expect_equal(cur$primary_value[cur$reporter_iso == "VNM" & cur$partner_iso == "KOR"], 20)
  expect_equal(cur$primary_value[cur$reporter_iso == "CHN" & cur$partner_iso == "VNM"], 105)
  expect_equal(cur$primary_value[cur$reporter_iso == "CHN" & cur$partner_iso == "USA"], 45)
  expect_equal(d$primary_value[d$period == 2020L], 30)
})

test_that("current dyads keep codes the monthly pull did not ask for", {
  annual <- data.frame(reporter_iso = "USA", partner_iso = "KOR", period = 2024L, flow_direction = "import",
                       cmd_code = c("854143", "740311"), primary_value = c(50, 30), stringsAsFactors = FALSE)
  t12m <- data.frame(reporter_iso = "USA", partner_iso = "KOR", flow_direction = "import", cmd_code = "854143", primary_value = 60)
  d <- ctm_current_dyads(annual, t12m, data.frame(importer_iso = character(), hs6 = character(), growth = numeric()),
                         pulled_codes = c("854143", "850760"))
  expect_equal(d$primary_value[d$cmd_code == "854143"], 60)
  expect_equal(d$primary_value[d$cmd_code == "740311"], 30)
  expect_equal(d$current_source[d$cmd_code == "740311"], "carried")
})
