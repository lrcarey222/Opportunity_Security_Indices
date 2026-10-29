repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "dashboard", "country", "country_util.R"))
source(file.path(repo_root, "R", "dashboard", "sector", "sector_trade.R"))
source(file.path(repo_root, "R", "dashboard", "country", "country_trade_monthly.R"))

# The basket at the six stages (2026-10-28): the HS6 master's `stage` column places each code; its old
# three-stage supply_chain is not used.
basket_fixture <- function() {
  country_monthly_basket(data.frame(
    tech = c("Batteries", "Batteries", "Batteries", "Electric Vehicles", "Semiconductors"),
    supply_chain = c("Midstream", "Midstream", "Midstream", "Midstream", "Midstream"),
    sub_sector = c("Battery Cells", "Battery Cells", "Cathode Materials", "Vehicle Batteries", "ICs"),
    hs6 = c("850760", "850790", "284190", "850760", "854231"),
    stage = c("Assembly", "Assembly", "Refining", "Components", "Assembly")
  ))
}
months12 <- country_month_seq(202508L)
raw_fixture <- function() {
  r <- rbind(
    data.frame(reporter_iso = "USA", period = months12, flow_code = "M", cmd_code = "850760", primary_value = 100 + 0:11),
    data.frame(reporter_iso = "USA", period = months12, flow_code = "M", cmd_code = "850790", primary_value = 10),
    data.frame(reporter_iso = "USA", period = months12[-1], flow_code = "M", cmd_code = "284190", primary_value = 5),
    data.frame(reporter_iso = "USA", period = months12, flow_code = "M", cmd_code = "854231", primary_value = 999),
    data.frame(reporter_iso = "USA", period = months12[1:11], flow_code = "X", cmd_code = "850760", primary_value = 50)
  )
  r$period <- as.character(r$period)
  r
}
windows_fixture <- data.frame(reporter_iso = c("USA", "AUT"), first_month = c(202508L, NA), last_month = c(202607L, NA))

test_that("months roll over the year and the index is relative to the first month", {
  expect_equal(country_month_seq(202508L)[c(1, 5, 12)], c(202508L, 202512L, 202607L))
  expect_equal(country_index_100(c(50, 100, NA)), c(100, 200, NA))
  expect_true(all(is.na(country_index_100(c(0, 5, 10)))))
  expect_true(all(is.na(country_index_100(c(NA, 5)))))
})

test_that("the basket takes the master's six-stage stage; a sub-component that spans stages sits under each", {
  b <- basket_fixture()
  expect_equal(sort(unique(b$supply_chain[b$tech != "Semiconductors"])), c("Assembly", "Components", "Refining"))
  expect_setequal(intersect(b$cell, country_cells_tbl()$key), c("batteries-assembly", "batteries-refining", "electric-vehicles-components"))
  m <- data.frame(tech = "Solar", supply_chain = "Midstream", sub_sector = c("Polysilicon & Wafers", "Polysilicon & Wafers", "Other"),
                  hs6 = c("280461", "381800", "999999"), stage = c("Refining", "Components", ""))
  sb <- country_monthly_basket(m)
  expect_equal(sb$cell, c("solar-refining", "solar-components"))                           # the row without a stage is left out
  expect_equal(sb$sub_sector, c("Polysilicon & Wafers", "Polysilicon & Wafers"))
  expect_error(country_monthly_basket(m[, setdiff(names(m), "stage")]), "no `stage` column")
})

test_that("the table maps codes to cells and sub-components, counting a shared code in each cell", {
  t <- country_monthly_table(raw_fixture(), basket_fixture(), windows_fixture)
  expect_setequal(unique(t$cell), c("batteries-refining", "batteries-assembly", "electric-vehicles-components"))  # no Semiconductors cell
  expect_setequal(unique(t$stage), c("Refining", "Assembly", "Components"))
  cells <- t[t$flow == "import" & t$month == 202508L, ]
  expect_equal(cells$value_usd[cells$sub_sector == "Battery Cells"], 110)
  expect_equal(cells$value_usd[cells$sub_sector == "Vehicle Batteries"], 100)
  expect_false("AUT" %in% t$iso3)
})

test_that("a reporter with a window but no rows gets no table rows (no all-null file)", {
  w <- rbind(windows_fixture, data.frame(reporter_iso = "JPN", first_month = 202509L, last_month = 202608L))
  t <- country_monthly_table(raw_fixture(), basket_fixture(), w)
  expect_false("JPN" %in% t$iso3)
})

test_that("declared months without a row are zero and undeclared months are null", {
  t <- country_monthly_table(raw_fixture(), basket_fixture(), windows_fixture)
  cath <- t[t$flow == "import" & t$sub_sector == "Cathode Materials", ]
  expect_equal(cath$value_usd[1], 0)                    # declared month (other codes reported), no cathode row
  expect_true(all(is.na(cath$index_100)))               # first month zero -> not indexable
  ex <- t[t$flow == "export" & t$sub_sector == "Battery Cells", ]
  expect_true(is.na(ex$value_usd[12]))                  # the reporter declared no exports in the last month
  expect_equal(ex$index_100[2], 100)
})

test_that("the doc passes its check, survives a JSON round trip, and broken docs fail", {
  b <- basket_fixture()
  t <- country_monthly_table(raw_fixture(), b, windows_fixture)
  src <- list(id = "comtrade_monthly", label = "UN Comtrade, monthly")
  doc <- country_monthly_doc(t, "USA", windows_fixture[1, ], b, src)
  expect_equal(country_check_monthly_doc(doc), character())
  expect_equal(doc$window$first, "2025-08")
  expect_equal(doc$window$last, "2026-07")
  expect_equal(vapply(doc$cells, function(c) c$key, ""), c("batteries-refining", "batteries-assembly", "electric-vehicles-components"))  # chain order
  bc <- doc$cells[[2]]
  expect_equal(bc$key, "batteries-assembly")
  expect_equal(bc$label, "Batteries \u2014 Assembly")
  expect_equal(bc$subs[[1]]$name, "Battery Cells")      # largest first
  expect_equal(bc$subs[[1]]$n_codes, 2L)
  expect_equal(as.numeric(bc$subs[[1]]$imports)[c(1, 12)], c(110, 121))
  expect_null(bc$subs[[1]]$imports_i)                  # the client indexes (indexTo100)
  expect_equal(doc$window$declared$exports[12], FALSE)

  back <- jsonlite::fromJSON(as.character(country_json(doc)), simplifyVector = FALSE)
  expect_equal(country_check_monthly_doc(back), character())
  expect_equal(unlist(back$cells[[2]]$subs[[1]]$imports), as.numeric(bc$subs[[1]]$imports))

  broken <- back
  broken$window$declared <- NULL
  expect_match(country_check_monthly_doc(broken), "expected 12 true/false", all = FALSE)
  broken <- back
  broken$cells[[1]]$subs[[1]]$name <- ""
  expect_match(country_check_monthly_doc(broken), "without a name", all = FALSE)
  broken <- back
  broken$schema_version <- 2L
  expect_match(country_check_monthly_doc(broken), "needs a newer app", all = FALSE)
  broken <- back
  broken$cells[[1]]$subs[[1]]$imports[[1]] <- "5"
  expect_match(country_check_monthly_doc(broken), "length", all = FALSE)
  broken <- back
  broken$cells[[1]]$subs[[1]]$exports[[2]] <- -5
  expect_match(country_check_monthly_doc(broken), "negative value", all = FALSE)
  broken <- back
  broken$cells[[1]]$total$imports[[2]] <- 1
  expect_match(country_check_monthly_doc(broken), "do not sum", all = FALSE)
  broken <- back
  broken$cells[[1]]$key <- "batteries-midstream"                                          # a three-stage key is unknown
  expect_match(country_check_monthly_doc(broken), "unknown cell key", all = FALSE)
  broken <- back
  broken$window$months <- broken$window$months[-1]
  expect_match(country_check_monthly_doc(broken), "expected 12 months", all = FALSE)
})

test_that("a month reported with far fewer codes than usual is treated as not (fully) reported", {
  r <- raw_fixture()
  extra <- do.call(rbind, lapply(sprintf("8%05d", 1:20), function(cd) data.frame(reporter_iso = "USA", period = as.character(months12),
                                                                              flow_code = "M", cmd_code = cd, primary_value = 1)))
  r <- rbind(r, extra[!(extra$period == as.character(months12[6]) & extra$cmd_code != "800001"), ])
  t <- country_monthly_table(r, basket_fixture(), windows_fixture)
  cells <- t[t$flow == "import" & t$sub_sector == "Battery Cells", ]
  expect_true(is.na(cells$value_usd[6]))                # 5 codes that month against ~24 elsewhere
  expect_false(any(is.na(cells$value_usd[-6])))
})
