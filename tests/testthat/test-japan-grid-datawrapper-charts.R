# Japan grid Datawrapper chart helpers, on synthetic inputs.

opsi_jgd_root <- function() normalizePath(test_path("..", ".."), winslash = "/")

# Load only the pure helpers, not the script's bootstrap, IO or runner.
opsi_load_jgd <- function() {
  path <- file.path(opsi_jgd_root(), "R", "charts", "japan_grid_datawrapper.R")
  exprs <- parse(path)
  env <- globalenv()
  wanted <- c("JGD_COMPONENTS", "JGD_CHINA_CODE", "JGD_TREND_COUNTRIES", "jgd_split",
              "jgd_code_map", "jgd_covered", "jgd_hhi", "jgd_rca", "jgd_trend",
              "jgd_trend_wide", "jgd_import_concentration", "jgd_partner_scores",
              "jgd_screen_partners")
  for (e in exprs) {
    if (is.call(e) && length(e) >= 3 && identical(as.character(e[[1]]), "<-") &&
        as.character(e[[2]])[1] %in% wanted) {
      eval(e, envir = env)
    }
  }
  invisible(TRUE)
}

test_that("components are well formed and the trap codes stay out", {
  opsi_load_jgd()
  codes <- unlist(lapply(JGD_COMPONENTS$hs, jgd_split))
  expect_false(anyDuplicated(codes) > 0)
  expect_false("271600" %in% codes)
  expect_false(any(substr(codes, 1, 2) == "44"))
  expect_true(sum(JGD_COMPONENTS$strategic) >= 3 && sum(JGD_COMPONENTS$strategic) <= 5)
})

test_that("coverage is all-or-nothing per component", {
  opsi_load_jgd()
  comps <- tibble::tibble(hs = c("850423", "722511|722519"))
  expect_equal(jgd_covered(comps, c("850423", "722511")), c(TRUE, FALSE))
})

test_that("HHI is on the 0-10,000 scale", {
  opsi_load_jgd()
  expect_equal(jgd_hhi(100), 10000)
  expect_equal(jgd_hhi(c(50, 50, 0)), 5000)
  expect_true(is.na(jgd_hhi(numeric())))
})

test_that("RCA and world share use the reporters common to both inputs", {
  opsi_load_jgd()
  exports <- data.frame(reporter_iso = c("JPN", "CHN"), hs6 = "850423", value = c(10, 90))
  totals <- data.frame(reporter_iso = c("JPN", "CHN", "FRA"), total = c(100, 900, 50))
  out <- jgd_rca(exports, totals, data.frame(slug = "lpt", hs6 = "850423"))
  jp <- out[out$reporter_iso == "JPN", ]
  expect_equal(jp$world_share, 0.1)
  expect_equal(jp$rca, (10 / 100) / (100 / 1000))
})

test_that("trend shares fill missing years with zero and widen by component", {
  opsi_load_jgd()
  ex <- data.frame(reporter_iso = c("JPN", "CHN", "CHN"), hs6 = "850423",
                   year = c(2000L, 2000L, 2001L), value = c(25, 75, 50))
  tr <- jgd_trend(ex, data.frame(slug = "lpt", hs6 = "850423"))
  jp <- tr[tr$reporter_iso == "JPN", ]
  expect_equal(jp$share_pct[order(jp$year)], c(25, 0))

  wide <- jgd_trend_wide(tr, "lpt", c(lpt = "LPT"))
  expect_equal(names(wide), c("year", unname(JGD_TREND_COUNTRIES)))
})

test_that("import concentration reads shares against the World row", {
  opsi_load_jgd()
  bil <- data.frame(
    flow = c("M", "M", "M", "X"), cmd_code = "850423",
    partner_code = c(0L, 156L, 764L, 0L), partner = c("World", "China", "Thailand", "World"),
    value = c(100, 30, 70, 40)
  )
  out <- jgd_import_concentration(bil, data.frame(slug = "lpt", hs6 = "850423"))
  expect_equal(out$top_supplier, "Thailand")
  expect_equal(out$china_share, 30)
  expect_equal(out$supplier_hhi, 30^2 + 70^2)
  expect_equal(out$exports, 40)
})

test_that("partner score rescales weights when development is missing", {
  opsi_load_jgd()
  dyads <- list(
    friendshore = data.frame(partner_iso = c("USA", "USA", "THA"), partner = c("US", "US", "Thailand"),
                             variable = "Friendshore Index", value = c(0.8, 0.8, 0.6)),
    opportunity = data.frame(partner_iso = c("USA", "THA"), partner = c("US", "Thailand"),
                             variable = "Opportunity Index", value = c(0.6, 0.6)),
    development = data.frame(partner_iso = c("THA", "THA"), partner = "Thailand",
                             variable = "Development Potential Index", value = c(0.4, 0.6))
  )
  w <- list(friendshore = 0.4, opportunity = 0.4, development = 0.2)
  s <- jgd_partner_scores(dyads, w)

  us <- s[s$partner_iso == "USA", ]
  th <- s[s$partner_iso == "THA", ]
  expect_equal(us$score, 0.5 * 0.8 + 0.5 * 0.6)             # dev missing: 0.4/0.8 each
  expect_equal(th$development, 0.5)                        # sub-sector rows averaged
  expect_equal(th$score, 0.4 * 0.6 + 0.4 * 0.6 + 0.2 * 0.5)
  expect_equal(us$friendshore_w + us$opportunity_w + us$development_w, us$score)

  screened <- jgd_screen_partners(s, data.frame(reporter_iso = c("USA", "THA"),
                                                grid_imports = c(5e9, 5e8)), 1)
  expect_equal(screened$partner_iso, "USA")
})
