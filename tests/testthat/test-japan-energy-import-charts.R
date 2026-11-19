# Japan energy import exposure chart helpers, on a synthetic EI panel.

opsi_jei_root <- function() normalizePath(test_path("..", ".."), winslash = "/")

# Load only the pure helpers, not the script's bootstrap or its runner.
opsi_load_jei <- function() {
  path <- file.path(opsi_jei_root(), "R", "charts", "japan_energy_import_exposure.R")
  exprs <- parse(path)
  env <- globalenv()
  wanted <- c("JEI_VARS", "JEI_ZERO_IF_MISSING", "JEI_COUNTRIES",
              "jei_latest_year", "jei_exposure", "jei_wide")
  for (e in exprs) {
    if (is.call(e) && length(e) >= 3 && identical(as.character(e[[1]]), "<-") &&
        as.character(e[[2]])[1] %in% wanted) {
      eval(e, envir = env)
    }
  }
  invisible(TRUE)
}

opsi_jei_panel <- function() {
  row <- function(country, year, vals) {
    data.frame(Country = country, Year = year, Var = names(vals), Value = unname(vals),
               stringsAsFactors = FALSE)
  }
  rbind(
    # Importer with no production at all: production variables absent, not zero.
    row("Japan", 2025L, c(tes_ej = 10, oil_tes_ej = 4, gas_tes_ej = 2, oilcons_kbd = 100)),
    row("Japan", 2026L, c(tes_ej = 10)),
    # Half-dependent on oil, a net gas exporter.
    row("Exporter", 2025L, c(tes_ej = 20, oil_tes_ej = 8, gas_tes_ej = 6, oilcons_kbd = 200,
                             oilprod_kbd = 100, gasprod_ej = 9))
  )
}

test_that("latest year ignores years with partial data", {
  opsi_load_jei()
  expect_equal(jei_latest_year(opsi_jei_panel()), 2025L)
})

test_that("TES splits into four shares that sum to 100", {
  opsi_load_jei()

  countries <- tibble::tibble(ei_name = c("Japan", "Exporter"), label = c("Japan", "Exporter"))
  out <- jei_exposure(opsi_jei_panel(), 2025L, countries)

  jp <- out[out$country == "Japan", ]
  expect_equal(c(jp$imported_oil, jp$imported_gas, jp$domestic_oil_gas, jp$other_energy),
               c(40, 20, 0, 40))

  ex <- out[out$country == "Exporter", ]
  expect_equal(ex$oil_import_dependence, 50)
  expect_equal(ex$gas_import_dependence, 0)     # net exporter floored at zero
  expect_equal(ex$imported_oil, 20)             # 8 EJ x 50% / 20 EJ
  expect_equal(ex$domestic_oil_gas, 50)

  shares <- out[, c("imported_oil", "imported_gas", "domestic_oil_gas", "other_energy")]
  expect_equal(unname(rowSums(shares)), c(100, 100))
  expect_equal(out$country[1], "Japan")         # most exposed first

  expect_equal(names(jei_wide(out))[1:3], c("country", "Imported oil", "Imported gas"))
})

test_that("a missing country is an error, not a dropped bar", {
  opsi_load_jei()
  countries <- tibble::tibble(ei_name = c("Japan", "Nowhere"), label = c("Japan", "Nowhere"))
  expect_error(jei_exposure(opsi_jei_panel(), 2025L, countries), "Nowhere")
})
