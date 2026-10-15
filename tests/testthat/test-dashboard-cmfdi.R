# The dashboard's reference engine (R/dashboard) against the pipeline for Friendshore's CM-FDI parts
# (commit 78be8b5, R/themes/partnership_strength/safer_friendshore.R): in the cells CM-FDI covers the
# outbound component is the CM-FDI investment tie instead of the IMF position (missing where the
# reporter's parents have no project there), and chinese_parent_exposure enters the weighted mean as
# 1 - share (missing where undefined, never 0). The hawkishness dial scales its weight like
# geo_alignment's. Runs on the synthetic bundle (dash_fixture_bundle()); no data needed.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "dashboard", "dash_files.R"))
for (f in dash_engine_files()) source(file.path(repo_root, f))
source(file.path(repo_root, "R", "themes", "partnership_strength", "safer_friendshore.R"))

# The fixture's long Friendshore table as one row per dyad cell with the pipeline's component columns.
fs_wide <- function(bundle) {
  raw <- attr(bundle, "fixture_raw")$fs_long
  raw <- raw[raw$variable != "Friendshore Index", c("reporter_iso", "partner_iso", "tech", "supply_chain", "variable", "value")]
  wide <- tidyr::pivot_wider(raw, names_from = "variable", values_from = "value")
  for (k in partnership_strength_friendshore_components()) if (!k %in% names(wide)) wide[[k]] <- NA_real_
  wide
}

for (seed in 1:2) {
  test_that(paste0("Friendshore raw scores equal the pipeline's weighted mean with the CM-FDI parts (seed ", seed, ")"), {
    b <- dash_fixture_bundle(seed = seed)
    expect_identical(b$partner$fs$outbound_source[b$partner$cells$supply_chain == "Midstream" & b$partner$cells$tech %in% c("Solar", "Wind")],
                     c("cm_fdi", "cm_fdi"))
    expect_identical(sum(b$partner$fs$outbound_source == "cm_fdi"), dim(b$partner$fs$outbound_cmfdi)[3])
    expect_identical(attr(b, "fixture_raw")$cm_fdi$n_mixed, 0L)
    for (dials in list(list(), list(hawkishness = 100), list(hawkishness = 0, greenness = 80))) {
      p <- dash_params(dials = dials, bundle = b)
      w <- p$partner$fs_weights
      wide <- fs_wide(b)
      # partnership_strength_build_friendshore_dyads(): the pipeline's components, order and score
      want <- partnership_strength_row_weighted_mean(
        list(wide$imp_trade_index, wide$econ_opp_raw, wide$es_need, wide$eo_partner, wide$outbound_index, wide$geo_alignment,
             partnership_strength_friendshore_score("chinese_parent_exposure", wide$chinese_parent_exposure)),
        unname(w[partnership_strength_friendshore_components()])
      )
      U <- b$partner$U
      ci <- (match(wide$tech, b$dims$techs) - 1L) * length(b$dims$stages) + match(wide$supply_chain, b$dims$stages)
      got <- dash_fs_adj(b$partner$fs, w, p$partner$fs_penalty)$raw[cbind(match(wide$reporter_iso, U), match(wide$partner_iso, U), ci)]
      expect_identical(got, want)
      # the CM-FDI parts change the result: some dyad cells have no tie, some have an exposure
      cm <- ci %in% which(b$partner$fs$outbound_source == "cm_fdi")
      expect_true(any(cm & is.na(wide$outbound_index)))
      expect_true(any(cm & !is.na(wide$chinese_parent_exposure)))
      expect_true(all(is.na(wide$chinese_parent_exposure[!cm])))
    }
  })
}

test_that("hawkishness scales the Chinese-parent ownership weight like the alignment weight", {
  b <- dash_fixture_bundle(seed = 1)
  w0 <- b$manifest$baseline_inputs$fs_weights
  for (h in c(0, 25, 50, 75, 100)) {
    p <- dash_params(dials = list(hawkishness = h), bundle = b)
    mH <- dash_dial_multiplier(h)
    expect_identical(p$partner$fs_weights$chinese_parent_exposure, w0$chinese_parent_exposure * mH)
    expect_identical(p$partner$fs_weights$geo_alignment, w0$geo_alignment * mH)
    expect_identical(p$partner$fs_weights$outbound_index, w0$outbound_index)
  }
  ro <- dash_dial_readout(dash_params(dials = list(hawkishness = 100), bundle = b), b)
  r <- ro[ro$parameter == "fs_cpe_share", ]
  expect_identical(r$dial, "hawkishness")
  expect_gt(r$current, r$baseline)
})

test_that("the dyad decomposition shows the CM-FDI tie, the ownership share and contributions that add up", {
  b <- dash_fixture_bundle(seed = 1)
  p <- dash_params(bundle = b)
  cells <- dash_partner_cells(b, p)
  U <- b$partner$U
  for (rp in list(c("BRA", "CHN"), c("FRA", "DEU"), c("IND", "JPN"))) {
    d <- dash_partner_decompose(b, p, cells, rp[1], rp[2])
    cm <- which(b$partner$fs$outbound_source == "cm_fdi")
    expect_identical(d$fs_outbound_source, b$partner$fs$outbound_source)
    expect_identical(d$fs_outbound_index[cm], b$partner$fs$outbound_cmfdi[match(rp[1], U), match(rp[2], U), ])
    expect_identical(d$fs_outbound_index[-cm], rep(b$partner$fs$outbound[match(rp[1], U), match(rp[2], U)], nrow(d) - length(cm)))
    expect_identical(d$fs_chinese_parent_exposure, b$partner$fs$cpe[match(rp[2], U), ])
    shares <- d[grepl("^fs_.*_share$", names(d))]
    tot <- rowSums(shares, na.rm = TRUE)
    ok <- !is.na(d$fsi_raw)
    expect_lt(max(abs(tot[ok] - d$fsi_raw[ok])), 1e-12)
    # the risk component contributes 1 - share
    k <- which(!is.na(d$fs_chinese_parent_exposure) & ok)
    if (length(k)) expect_true(all(d$fs_chinese_parent_exposure_share[k] <= (1 - d$fs_chinese_parent_exposure[k]) + 1e-12))
  }
})

test_that("dash_bundle_fs_cmfdi() finds the CM-FDI cells from the source column and flags mixed cells", {
  long <- data.frame(
    reporter_iso = "A", partner_iso = "B", tech = c("Solar", "Solar", "Wind", "Wind", "Coal"),
    supply_chain = c("Midstream", "Upstream", "Midstream", "Midstream", "Upstream"),
    variable = c("outbound_index", "outbound_index", "outbound_index", "outbound_index", "geo_alignment"),
    value = c(0.5, 0.2, 0.1, NA, 1),
    source = c("CM-FDI X (CM-FDI); author calculation", "Author calculation", "x (CM-FDI)", "Author calculation", "y (CM-FDI)"),
    stringsAsFactors = FALSE
  )
  cm <- dash_bundle_fs_cmfdi(long, c("Solar", "Wind", "Coal"))
  expect_identical(cm$cells, c(2L, 5L))
  expect_identical(cm$outbound_source[c(2, 5)], c("cm_fdi", "cm_fdi"))
  expect_identical(sum(cm$outbound_source == "imf_dip"), 7L)
  expect_identical(cm$n_rows, 2L)
  expect_identical(cm$n_mixed, 1L)
  expect_identical(cm$long$variable, c("outbound_index_cmfdi", "outbound_index", "outbound_index_cmfdi", "outbound_index_cmfdi", "geo_alignment"))
  expect_false("source" %in% names(cm$long))
  none <- dash_bundle_fs_cmfdi(long[, names(long) != "source"], c("Solar", "Wind", "Coal"))
  expect_identical(none$cells, integer())
  expect_identical(none$long$variable, long$variable)
})
