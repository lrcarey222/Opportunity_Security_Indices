# Japan midstream export share chart helpers, on synthetic trade.

opsi_jms_root <- function() normalizePath(test_path("..", ".."), winslash = "/")

# Load only the pure helpers, not the script's bootstrap or its runner.
opsi_load_jms <- function() {
  path <- file.path(opsi_jms_root(), "R", "charts", "japan_midstream_export_share.R")
  exprs <- parse(path)
  env <- globalenv()
  wanted <- c("JMS_HS92_MAP", "JMS_COUNTRIES", "jms_keep_code", "jms_universe",
              "jms_shares", "jms_wide")
  for (e in exprs) {
    if (is.call(e) && length(e) >= 3 && identical(as.character(e[[1]]), "<-") &&
        as.character(e[[2]])[1] %in% wanted) {
      eval(e, envir = env)
    }
  }
  invisible(TRUE)
}

test_that("timber and ores leave the universe; metal-bearing residues stay", {
  opsi_load_jms()
  expect_equal(jms_keep_code(c("440311", "260300", "250410", "262030", "850440")),
               c(FALSE, FALSE, FALSE, TRUE, TRUE))
})

test_that("later-HS codes translate to their HS92 parent, once", {
  opsi_load_jms()

  codes <- data.frame(
    hs6 = c("854142", "854143", "850440", "848610", "850421", "260300"),
    value_chain = c(rep("Midstream", 5), "Midstream"),
    stringsAsFactors = FALSE
  )
  codes <- rbind(codes, data.frame(hs6 = "850422", value_chain = "Upstream"))
  u <- jms_universe(codes, atlas_codes = c("854140", "850440", "850421"))

  expect_equal(u$hs6, c("854142", "854143", "850440", "848610", "850421"))
  expect_equal(u$hs92, c("854140", "854140", "850440", NA, "850421"))
  expect_equal(u$in_atlas, c(TRUE, TRUE, TRUE, FALSE, TRUE))
})

test_that("shares divide by world exports and fill non-exporters with zero", {
  opsi_load_jms()

  atlas <- data.frame(
    reporter_iso = c("JPN", "CHN", "FRA", "JPN", "CHN"),
    hs6 = c("854140", "854140", "854140", "999999", "854140"),
    year = c(2000L, 2000L, 2000L, 2000L, 2001L),
    value = c(20, 30, 50, 1000, 40)
  )
  universes <- list(solar = data.frame(hs92 = c("854140", "854140"), in_atlas = TRUE))
  out <- jms_shares(atlas, universes)

  jp <- out[out$reporter_iso == "JPN", ]
  expect_equal(jp$share_pct, c(20, 0))                 # 20 of 100; none in 2001
  expect_equal(out$share_pct[out$reporter_iso == "CHN"], c(30, 100))
  expect_equal(nrow(out), 2 * length(JMS_COUNTRIES))   # every country, every year

  wide <- jms_wide(out, "solar")
  expect_equal(names(wide), c("year", unname(JMS_COUNTRIES)))
  both <- jms_wide(rbind(out, transform(out, sector = "grid")), c("solar", "grid"),
                   labels = c(solar = "Solar", grid = "Grid"))
  expect_equal(names(both)[2], "Solar: Japan")
  expect_equal(ncol(both), 1 + 2 * length(JMS_COUNTRIES))
})
