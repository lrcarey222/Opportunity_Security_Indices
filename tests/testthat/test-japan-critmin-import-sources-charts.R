# Japan critical mineral import sources chart helpers, on a synthetic bilateral pull.

opsi_jci_root <- function() normalizePath(test_path("..", ".."), winslash = "/")

# Load only the pure helpers, not the script's bootstrap, API calls or runner.
opsi_load_jci <- function() {
  path <- file.path(opsi_jci_root(), "R", "charts", "japan_critmin_import_sources.R")
  exprs <- parse(path)
  env <- globalenv()
  wanted <- c("JCI_CHINA", "JCI_MATERIALS", "jci_split", "jci_shares", "jci_wide")
  for (e in exprs) {
    if (is.call(e) && length(e) >= 3 && identical(as.character(e[[1]]), "<-") &&
        as.character(e[[2]])[1] %in% wanted) {
      eval(e, envir = env)
    }
  }
  invisible(TRUE)
}

opsi_jci_pull <- function() {
  data.frame(
    cmd_code = c("282520", "282520", "282520", "283691", "283691", "283691"),
    partner_code = c(0L, 156L, 152L, 0L, 152L, 32L),
    partner = c("World", "China", "Chile", "World", "Chile", "Argentina"),
    value = c(600, 500, 100, 400, 300, 100),
    stringsAsFactors = FALSE
  )
}

test_that("shares pool codes and take the largest non-China supplier", {
  opsi_load_jci()

  materials <- tibble::tibble(material = "Lithium", hs = "282520|283691")
  out <- jci_shares(opsi_jci_pull(), materials)

  expect_equal(out$china, 50)                    # 500 of 1000
  expect_equal(out$top_other_name, "Chile")      # 100 + 300 across both codes
  expect_equal(out$top_other, 40)
  expect_equal(out$rest, 10)
  expect_equal(out$import_value_m, 1000 / 1e6)
})

test_that("a material with no reported codes is an error, not a blank bar", {
  opsi_load_jci()
  materials <- tibble::tibble(material = "Rare earths", hs = "280530")
  expect_error(jci_shares(opsi_jci_pull(), materials), "Rare earths")
})

test_that("wide bars sum to exactly 100 after rounding", {
  opsi_load_jci()

  shares <- data.frame(material = "X", china = 33.333, top_other = 33.333, rest = 33.334,
                       top_other_name = "Y", import_value_m = 1.234, hs = "1")
  wide <- jci_wide(shares)
  expect_equal(wide$China + wide$`Largest other supplier` + wide$`All other suppliers`, 100)
  expect_equal(names(wide)[1:4], c("Material", "China", "Largest other supplier",
                                   "All other suppliers"))
})

test_that("configured materials follow the paragraph and do not share codes", {
  opsi_load_jci()
  expect_equal(JCI_MATERIALS$material[1:3], c("Diammonium phosphate", "Lithium", "Rare earths"))
  codes <- unlist(lapply(JCI_MATERIALS$hs, jci_split))
  expect_false(anyDuplicated(codes) > 0)
  expect_true(all(nchar(codes) == 6))
})
