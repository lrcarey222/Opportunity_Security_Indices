# Japan cleantech RCA chart helpers, on synthetic trade.

opsi_jcr_root <- function() normalizePath(test_path("..", ".."), winslash = "/")

# Load only the pure helpers, not the script's bootstrap or its runner.
opsi_load_jcr <- function() {
  path <- file.path(opsi_jcr_root(), "R", "charts", "japan_cleantech_rca.R")
  exprs <- parse(path)
  env <- globalenv()
  wanted <- c("JCR_ROWS", "jcr_split", "jcr_assign_sources", "jcr_rca")
  for (e in exprs) {
    if (is.call(e) && length(e) >= 3 && identical(as.character(e[[1]]), "<-") &&
        as.character(e[[2]])[1] %in% wanted) {
      eval(e, envir = env)
    }
  }
  invisible(TRUE)
}

test_that("RCA is the Balassa ratio and keeps non-exporters at zero", {
  opsi_load_jcr()

  exports <- data.frame(
    reporter_iso = c("JPN", "JPN", "DEU"),
    hs6 = c("850511", "850519", "850511"),
    value = c(30, 10, 60)
  )
  totals <- data.frame(reporter_iso = c("JPN", "DEU", "FRA"), total = c(400, 600, 1000))
  code_map <- data.frame(label = "Magnets", hs6 = c("850511", "850519"))

  out <- jcr_rca(exports, totals, code_map)
  jp <- out[out$reporter_iso == "JPN", ]
  de <- out[out$reporter_iso == "DEU", ]

  # World = reporters in both inputs (JPN, DEU): magnets 100 of 1000 total exports.
  expect_equal(jp$rca, (40 / 400) / (100 / 1000))
  expect_equal(de$rca, (60 / 600) / (100 / 1000))
  expect_equal(jp$world_share, 0.4)
  expect_false("FRA" %in% out$reporter_iso)          # no product data, not in world

  exports$reporter_iso[3] <- "JPN"                   # DEU now exports none
  exports <- rbind(exports, data.frame(reporter_iso = "DEU", hs6 = "999999", value = 1))
  out <- jcr_rca(exports, totals, code_map)
  expect_equal(out$rca[out$reporter_iso == "DEU"], 0)
})

test_that("a row falls back to HS92 only when the pull lacks one of its codes", {
  opsi_load_jcr()

  rows <- tibble::tibble(
    label = c("Full", "Partial", "Stranded"),
    hs = c("111111|222222", "111111|333333", "444444"),
    hs92 = c("", "333330", "")
  )
  pull <- c("111111", "222222")

  out <- jcr_assign_sources(rows[1:2, ], pull)
  expect_equal(unique(out$source[out$label == "Full"]), "comtrade")
  expect_equal(out$hs6[out$label == "Partial"], "333330")
  expect_error(jcr_assign_sources(rows, pull), "Stranded")
})

test_that("every configured row has codes and a known sector", {
  opsi_load_jcr()
  expect_true(all(nzchar(JCR_ROWS$hs)))
  expect_setequal(unique(JCR_ROWS$sector), c("Critical minerals & materials", "Nuclear", "Grids"))
  expect_false(anyDuplicated(JCR_ROWS$label) > 0)
})
