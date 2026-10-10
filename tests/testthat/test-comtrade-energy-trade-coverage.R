# The country-level trade themes (scripts/10) take basket exports, Balassa RCA and the export HHI
# from data/raw/comtrade_energy_trade.csv, with comtrade_total_export.csv as the RCA denominator.
# A basket code or a reporter missing there drops out of every cell it feeds without an error: the
# scripts/05 pull staged 2026-09-30 lacked 60 of the 256 basket codes (19 of the 20 Semiconductors
# Midstream codes among them) and 13 reporters with 2024 data. scripts/98's energy_trade stage now
# derives the file from the World rows of the all-reporter dyad pull. These tests read the staged
# files and skip when they are absent.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "scripts", "utils", "comtrade_dyads_utils.R"))
staged_dir <- file.path(repo_root, "data", "raw")

read_staged_comtrade <- function(name, columns) {
  path <- file.path(staged_dir, name)
  skip_if_not(file.exists(path), paste(name, "not staged"))
  skip_if_not_installed("data.table")
  as.data.frame(data.table::fread(path, select = columns, colClasses = "character", showProgress = FALSE))
}

# The rows of the year scripts/10 scores: the latest reference year in the file.
staged_energy_trade <- function() {
  et <- read_staged_comtrade("comtrade_energy_trade.csv",
                             c("reporter_iso", "ref_year", "flow_code", "cmd_code", "partner_code", "primary_value"))
  et[as.integer(et$ref_year) == max(as.integer(et$ref_year)), , drop = FALSE]
}

test_that("the energy-trade file holds every basket code in both flows", {
  et <- staged_energy_trade()
  view_path <- file.path(staged_dir, "hts_codes_categories_bolstered_final.csv")
  master_path <- file.path(staged_dir, "energy_hs6_master.csv")
  skip_if_not(file.exists(view_path) && file.exists(master_path), "HS6 views not staged (scripts/04)")
  basket <- unique(comtrade_dyads_pad_hs6(read.csv(view_path, colClasses = "character")$HS6))
  master <- read.csv(master_path, colClasses = "character")
  legacy <- unique(comtrade_dyads_pad_hs6(master$hs6[toupper(master$legacy_hs) == "TRUE"]))

  for (flow in c("X", "M")) {
    absent <- setdiff(basket, comtrade_dyads_pad_hs6(et$cmd_code[et$flow_code == flow]))
    # Only a legacy bridge code may be absent: it belongs to an HS edition (2002 or older) that no
    # reporter files for 2024, so the year has no record of it.
    expect_identical(setdiff(absent, legacy), character(), info = flow)
  }
})

test_that("the energy-trade file is a full reporter set of World totals", {
  et <- staged_energy_trade()
  # A newest year Comtrade has published for a handful of reporters computes shares and HHIs
  # happily on them; the 2024 files hold 142 export and 143 import reporters.
  for (flow in c("X", "M")) {
    expect_gte(length(unique(et$reporter_iso[et$flow_code == flow])), 100)
  }
  expect_true(all(as.numeric(et$partner_code) == 0))
  expect_false(anyDuplicated(paste(et$reporter_iso, et$flow_code, et$cmd_code)) > 0)
  expect_false(any(is.na(as.numeric(et$primary_value))))
  # Taiwan reports as Comtrade's "Other Asia, nes" (S19); the trade themes know it as TWN.
  expect_false(COMTRADE_DYADS_TAIWAN_ISO %in% et$reporter_iso)
})

test_that("every export reporter has its total exports, the RCA denominator", {
  et <- staged_energy_trade()
  total <- read_staged_comtrade("comtrade_total_export.csv", c("reporter_iso", "ref_year", "flow_code", "cmd_code", "primary_value"))
  year <- max(as.integer(et$ref_year))
  total <- total[as.integer(total$ref_year) == year & total$flow_code == "X" & toupper(total$cmd_code) == "TOTAL", , drop = FALSE]
  expect_false(anyDuplicated(total$reporter_iso) > 0)
  expect_true(all(as.numeric(total$primary_value) > 0))
  # trade_core_build_comtrade_rca() leaves a reporter without a total out of the world shares and
  # gives it no RCA, silently.
  expect_identical(setdiff(unique(et$reporter_iso[et$flow_code == "X"]), total$reporter_iso), character())
})
