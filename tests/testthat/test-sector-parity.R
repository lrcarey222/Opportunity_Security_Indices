# The parity ledger of the Sector Insights export (R/dashboard/sector/sector_chunks.R
# sector_parity(), sector_facts.R ledger rows) on a tiny fake processed table: prov P must be
# identical(), D identical() to the formula, X within 1e-12; ledger rows must reach the decoded chunk;
# exclusions are counted by reason; and the summary recomputed from decoded facts equals meta.summary.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "dashboard", "sector", "sector_files.R"))
for (f in sector_files()) source(file.path(repo_root, f))

# A fake processed table in the pipeline's long shape (index and weight rows included).
fake_processed <- function() {
  tibble::tibble(
    iso3c = c("USA", "DEU", "CHN", "USA", "DEU", "CHN", "USA"),
    tech = "Solar", supply_chain = "Downstream",
    variable = c(rep("Solar PV power potential per sq km", 3), rep("Solar PV power potential per sq km", 3), "Overall Potential Index"),
    data_type = c("raw", "raw", "raw", "index", "index", "index", "index"),
    value = c(4.71, 2.91, NA, 0.8, 0.2, NA, 0.6)
  )
}

decode_obs <- function(facts) {
  # Through the real chunk path: index, encode, decode, keys back to text.
  geo <- tibble::tibble(code = sort(unique(facts$geo)))
  vars <- sort(unique(facts$var))
  items <- sort(unique(facts$item_key))
  t <- sector_obs_tables(facts, vars, items, geo$code)
  dec <- dash_osb_decode(dash_osb_encode(list(facts = list(obs = t$obs, index = t$index)), "f", "chunk"))$objects$facts$obs
  tibble::tibble(var = vars[dec$var + 1L], item_key = items[dec$item + 1L], geo = geo$code[dec$geo + 1L], year = dec$year,
                 value = dec$value, status = dec$status)
}

test_that("prov P values pass through the codec identical() to the processed rows", {
  x <- sector_facts_potential(fake_processed(), fake_processed()[0, ], fake_processed()[0, ])
  expect_identical(x$facts$value, c(4.71, 2.91))
  expect_identical(x$excluded$n, 1L)
  expect_match(x$excluded$reason, "NA")
  pr <- sector_parity(decode_obs(x$facts), x$ledger)
  expect_true(all(pr$status == "PASS"))
  expect_identical(pr$n[pr$check == "parity P: resources.pv_quality identical() to data/processed"], 2L)
})

test_that("a value off by one ulp, a missing row or a wrong formula fails", {
  x <- sector_facts_potential(fake_processed(), fake_processed()[0, ], fake_processed()[0, ])
  dec <- decode_obs(x$facts)
  dec$value[1] <- dec$value[1] + 1e-15   # one ulp of 4.71
  pr <- sector_parity(dec, x$ledger)
  expect_identical(pr$status[grepl("^parity P", pr$check)], "FAIL")
  expect_match(pr$detail[grepl("^parity P", pr$check)], "1 rows differ")
  pr2 <- sector_parity(decode_obs(x$facts)[-2, ], x$ledger)
  expect_identical(pr2$status[pr2$check == "parity: ledger rows present in the decoded chunk"], "FAIL")
})

test_that("prov D is identical() to the formula and prov X within 1e-12 relative", {
  facts <- tibble::tibble(var = c("consumption.weo", "consumption.weo", "production.refining"), item_key = c("fuel:oil", "fuel:oil", "mineral:lithium"),
                          geo = c("WLD", "WLD", "CHN"), year = c(2035L, 2024L, 2025L), value = c(180.2, 180.2 - 4.7, 235), status = "obs",
                          obs_year = NA_integer_)
  ledger <- tibble::tibble(var = facts$var, item_key = facts$item_key, geo = facts$geo, year = facts$year, prov = c("P", "D", "X"),
                           table = c("future_demand_tbl", "future_demand_tbl", "major_supplier_shares_tbl"),
                           variable = c("2035", "2035 - global_growth", "world_share"), key = NA_character_,
                           source_value = c(180.2, 180.2, 235), formula_value = c(NA, 180.2 - 4.7, 235 * (1 + 1e-13)))
  pr <- sector_parity(decode_obs(facts), ledger)
  expect_true(all(pr$status == "PASS"))
  ledger$formula_value[2] <- 175.4
  ledger$formula_value[3] <- 235 * (1 + 1e-9)
  pr <- sector_parity(decode_obs(facts), ledger)
  expect_identical(pr$status[grepl("^parity D", pr$check)], "FAIL")
  expect_identical(pr$status[grepl("^parity X", pr$check)], "FAIL")
})

test_that("exclusions are counted by reason across the fact builders", {
  a <- sector_facts_potential(fake_processed(), fake_processed()[0, ], fake_processed()[0, ])
  b <- sector_facts_alignment(tibble::tibble(iso3 = c("USA", "CHN", "XKX", "PSE"), s_axis = c(1, 0, NA, 0.2)))
  all <- sector_bind_results(list(a, b))
  expect_identical(nrow(all$facts), 5L)
  expect_identical(all$excluded$n, c(1L, 1L))
  expect_identical(all$excluded$table, c("solar_pv_potential_tbl", "geo_ideal_points_tbl"))
  expect_identical(nrow(all$ledger), 5L)
  expect_true(all(all$ledger$prov == "P"))
})

test_that("the summary recomputed from the decoded synthetic chunks equals meta.summary", {
  dict <- sector_dictionary_tables(yaml::read_yaml(file.path(repo_root, "config", "sector_dictionary.yml")), fill = sector_synthetic_fill())
  parts <- sector_synthetic_parts(dict)
  have <- unique(sector_data_keys(parts, parts$geo)$var)
  b <- sector_assemble(parts, dict, ship_vars = dict$vars$id[dict$vars$id %in% have])
  ch <- sector_chunks(b)
  meta <- dash_osb_decode(dash_osb_encode(ch$`sector-meta`, "m", "chunk"))$objects
  fx <- dash_osb_decode(dash_osb_encode(ch$`sector-facts`, "f", "chunk"))$objects
  dfx <- sector_decoded_obs(meta, fx$facts$obs)
  dw <- sector_decoded_world(meta, fx$facts$world)
  recomputed <- sector_summary(dfx, dw, meta$dims$geo, b$dict$vars, parts$eu_intra)
  sm <- meta$summary
  it <- meta$dims$items
  ms <- tibble::tibble(item_key = paste0(it$axis, ":", it$slug)[sm$item + 1L], var = meta$dict$vars$id[sm$var + 1L], year = sm$year,
                       eu_mode = sm$eu_mode, W = sm$W, n = sm$n, top1_geo = sm$top1_geo + 1L, top1_share = sm$top1_share,
                       top3_share = sm$top3_share, hhi = sm$hhi, eff_n = sm$eff_n, china = sm$china, core = sm$core,
                       partner = sm$partner, rest = sm$rest, unattributed = sm$unattributed, growth = sm$growth)
  expect_identical(sector_summary_diff(recomputed, ms), 0)
  # A perturbed summary is detected.
  ms$hhi[1] <- ms$hhi[1] + 1e-9
  expect_gt(sector_summary_diff(recomputed, ms), 1e-12)
  # Every ledger-free fact still decodes to its key: the decoded keys equal the shipped facts.
  expect_setequal(paste(dfx$var, dfx$item_key, dfx$geo, dfx$year), paste(b$facts$var, b$facts$item_key, b$facts$geo, b$facts$year))
})
