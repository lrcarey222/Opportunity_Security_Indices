# The Sector Insights chunks (R/dashboard/sector/sector_chunks.R, sector_check.R): built from the
# synthetic parts, every chunk round-trips identical() through the OSB1 codec and passes the R mirror
# of the contract checks; the manifest carries the contract fields; the stale-file rule only ever
# touches sector chunk files.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "dashboard", "sector", "sector_files.R"))
for (f in sector_files()) source(file.path(repo_root, f))

synthetic_bundle <- function() {
  dict <- sector_dictionary_tables(yaml::read_yaml(file.path(repo_root, "config", "sector_dictionary.yml")), fill = sector_synthetic_fill())
  parts <- sector_synthetic_parts(dict)
  have <- unique(sector_data_keys(parts, parts$geo)$var)
  sector_assemble(parts, dict, ship_vars = dict$vars$id[dict$vars$id %in% have])
}
bundle <- synthetic_bundle()
chunks <- sector_chunks(bundle)

test_that("the synthetic bundle has every chunk of this build", {
  expect_setequal(names(chunks), c("sector-meta", "sector-facts", "sector-trade-batteries", "sector-trade-oil", "sector-policy",
                                   "sector-policy-text", "sector-series"))
  expect_true(all(c("monthly", "capacity") %in% names(chunks$`sector-series`$series)))
})

test_that("every chunk round-trips identical() and passes the contract checks", {
  meta_sizes <- NULL
  for (nm in c("sector-meta", setdiff(names(chunks), "sector-meta"))) {
    bytes <- dash_osb_encode(chunks[[nm]], nm, "chunk")
    dec <- dash_osb_decode(bytes)
    expect_identical(dec$objects, dash_osb_normalize(chunks[[nm]]), info = nm)
    if (nm == "sector-meta") meta_sizes <- list(geo = nrow(dec$objects$dims$geo), items = nrow(dec$objects$dims$items), vars = nrow(dec$objects$dict$vars))
    expect_identical(sector_check_chunk(nm, dec$objects, meta_sizes), character(), info = nm)
  }
})

test_that("the bundle is deterministic", {
  again <- sector_chunks(synthetic_bundle())
  for (nm in names(chunks)) expect_identical(dash_osb_encode(again[[nm]], nm, "chunk"), dash_osb_encode(chunks[[nm]], nm, "chunk"), info = nm)
})

test_that("the contract checks catch broken chunks", {
  meta <- dash_osb_decode(dash_osb_encode(chunks$`sector-meta`, "m", "chunk"))$objects
  sizes <- list(geo = nrow(meta$dims$geo), items = nrow(meta$dims$items), vars = nrow(meta$dict$vars))
  facts <- dash_osb_decode(dash_osb_encode(chunks$`sector-facts`, "f", "chunk"))$objects
  bad <- facts
  bad$facts$obs <- bad$facts$obs[rev(seq_len(nrow(bad$facts$obs))), ]
  expect_match(paste(sector_check_chunk("sector-facts", bad, sizes), collapse = "\n"), "not sorted")
  bad <- facts
  bad$facts$obs$geo[1] <- as.integer(sizes$geo)
  expect_match(paste(sector_check_chunk("sector-facts", bad, sizes), collapse = "\n"), "indices outside @geo")
  bad <- facts
  ok <- which(bad$facts$obs$status == "obs")[1]
  bad$facts$obs$value[ok] <- NaN
  expect_match(paste(sector_check_chunk("sector-facts", bad, sizes), collapse = "\n"), "status other than nq / na_growth / nsr")
  bad <- facts
  bad$facts$index$n[1] <- bad$facts$index$n[1] + 1L
  expect_match(paste(sector_check_chunk("sector-facts", bad, sizes), collapse = "\n"), "do not tile")
  bad <- facts
  bad$facts$obs$status[1] <- "estimated"
  expect_match(paste(sector_check_chunk("sector-facts", bad, sizes), collapse = "\n"), "outside the vocabulary")
  tr <- dash_osb_decode(dash_osb_encode(chunks$`sector-trade-batteries`, "t", "chunk"))$objects
  bad <- tr
  bad$trade$flows$v2024[1] <- 0
  expect_match(paste(sector_check_chunk("sector-trade-batteries", bad, sizes), collapse = "\n"), "zero or negative")
  bad <- tr
  bad$trade$flows$s2020[which(!is.na(bad$trade$flows$v2020))[1]] <- "none"
  expect_match(paste(sector_check_chunk("sector-trade-batteries", bad, sizes), collapse = "\n"), "NaN <=> s2020 none")
  m2 <- meta
  m2$dims$geo$group[m2$dims$geo$code == "CHN"] <- "rest"
  expect_match(paste(sector_check_chunk("sector-meta", m2), collapse = "\n"), "CHN must be in group china")
  m3 <- meta
  m3$dict$vars$year_field <- as.integer(m3$dict$vars$year_field)
  expect_match(paste(sector_check_chunk("sector-meta", m3), collapse = "\n"), "has R type integer")
  ser <- dash_osb_decode(dash_osb_encode(chunks$`sector-series`, "s", "chunk"))$objects
  ser$series$capacity$cap_status[1] <- "ramping"
  expect_match(paste(sector_check_chunk("sector-series", ser, sizes), collapse = "\n"), "cap_status has values outside")
})

test_that("observation tables are sorted, sliced and keyed by the dictionary order", {
  obs <- tibble::tibble(var = c("b.v", "a.v", "a.v", "a.v"), item_key = c("cell:x", "cell:x", "cell:x", "cell:y"),
                        geo = c("USA", "USA", "CHN", "CHN"), year = c(2024L, 2024L, 2024L, 2020L), value = c(1, 2, 3, 4),
                        status = "obs", obs_year = NA_integer_)
  t <- sector_obs_tables(obs, var_ids = c("a.v", "b.v"), item_keys = c("cell:x", "cell:y"), geo_codes = c("CHN", "USA"))
  expect_identical(t$obs$value, c(3, 2, 4, 1))
  expect_identical(t$index$start, c(0L, 2L, 3L))
  expect_identical(t$index$n, c(2L, 1L, 1L))
  expect_error(sector_obs_tables(rbind(obs, obs[1, ]), c("a.v", "b.v"), c("cell:x", "cell:y"), c("CHN", "USA")), "duplicate")
  expect_error(sector_obs_tables(obs, c("a.v", "b.v"), c("cell:x", "cell:y"), "CHN"), "geo not found: USA")
})

test_that("the manifest carries the contract fields and never includes restricted data", {
  ci <- tibble::tibble(name = c("sector-meta", "sector-trade-batteries"), file = c("sector-meta.0123abcd.osb", "sector-trade-batteries.89abcdef.osb"),
                       bytes = c(1000, 2000), bytes_gz = c(300, 600), md5 = c("a", "b"))
  m <- sector_manifest(bundle_id = "id", created = "2026-10-10T00:00:00Z", git_commit = NA_character_, phase = 1,
                       strategic_bundle_id = NA_character_, processed_mtime_max = "synthetic", pipeline_marker = NA_character_,
                       flags = list(include_restricted = FALSE, trade = TRUE, atlas = FALSE, policy_text = TRUE, series = TRUE),
                       chunk_info = ci, headlines = bundle$headlines, glyph = bundle$glyph, notes = c("a note"),
                       validation = tibble::tibble(check = "x", status = "PASS", n = 1L, detail = NA_character_),
                       sources = tibble::tibble(path = "config/sector_dictionary.yml", mtime = "synthetic", bytes = 1, md5 = "c"),
                       budget = list(sector_gz = 900, combined_gz = NULL, limit_sector_gz = 9 * 2^20, limit_combined_gz = 25 * 2^20))
  j <- jsonlite::fromJSON(sector_manifest_json(m), simplifyVector = FALSE)
  for (k in c("schema", "schema_version", "bundle_id", "created", "git_commit", "generator", "phase", "strategic_bundle_id",
              "processed_mtime_max", "pipeline_marker", "flags", "chunks", "headlines", "coverage_glyph", "notes", "validation",
              "sources", "budget", "encoding")) {
    expect_true(k %in% names(j), info = k)
  }
  expect_identical(j$schema, "osi-sector")
  expect_identical(j$schema_version, 1L)
  expect_null(j$git_commit)
  expect_false(j$flags$include_restricted)
  expect_false(j$encoding$lossy)
  expect_identical(j$chunks[[2]]$tech, "batteries")
  expect_null(j$chunks[[1]]$tech)
  expect_identical(length(j$coverage_glyph), 33L)
  expect_null(j$validation[[1]]$detail)
  expect_identical(unlist(j$notes), "a note")
  expect_null(j$budget$combined_gz)
})

test_that("stale cleanup only ever touches sector chunk files", {
  files <- c("manifest.json", "sector-meta.0123abcd.osb", "sector-meta.ffffffff.osb", "sector-trade-electric-grid.00aa11bb.osb",
             "core.9bd80ef5.osb", "sector-meta.osb", "sector-meta.0123abcd.osb.tmp", "notes.txt", "Sector-meta.0123abcd.osb")
  keep <- c("sector-meta.0123abcd.osb")
  expect_identical(sector_stale_files(files, keep), c("sector-meta.ffffffff.osb", "sector-trade-electric-grid.00aa11bb.osb"))
  expect_identical(sector_chunk_tech(c("sector-trade-electric-grid", "sector-meta")), c("electric-grid", NA))
})

test_that("per-chunk budgets apply by chunk kind", {
  ci <- tibble::tibble(name = c("sector-meta", "sector-trade-oil", "sector-facts"), bytes_gz = c(151 * 1024, 449 * 1024, 100))
  expect_identical(sector_over_budget(ci), "sector-meta")
})
