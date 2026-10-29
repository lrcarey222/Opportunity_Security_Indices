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
                                   "sector-policy-text", "sector-series", "sector-fdi", "sector-dev"))
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

test_that("per-fact notes ship in facts.notes; their caveats stay on a measure only where it has such a fact", {
  nt <- chunks$`sector-facts`$facts$notes
  expect_false(is.null(nt))
  meta <- chunks$`sector-meta`
  vid <- meta$dict$vars$id
  it <- meta$dims$items
  ik <- paste0(it$axis, ":", it$slug)[nt$item + 1L]
  got <- paste(vid[nt$var + 1L], ik, meta$dims$geo$code[nt$geo + 1L], nt$note)
  base <- nt$note %in% c("eia_production", "production_assumed_zero")
  expect_setequal(got[base], c("production.fuels fuel:gas JPN eia_production", "consumption.balance fuel:gas JPN eia_production",
                               "consumption.balance fuel:oil EST production_assumed_zero"))
  expect_true(all(nt$note %in% meta$dict$caveats$id))
  # A figure the source gives once for a technology's manufacturing ships on both Components and
  # Assembly, each fact with the note same_figure_pair (user decision C2, 2026-10-28): the notes come
  # in pairs, one per stage, and sit on those two stages only.
  pair <- nt$note == "same_figure_pair"
  expect_true(any(pair))
  expect_true(all(grepl("-(components|assembly)$", ik[pair])))
  key <- paste(vid[nt$var + 1L], sub("-(components|assembly)$", "", ik), nt$geo, nt$year)[pair]
  expect_true(all(table(key) == 2L))
  expect_setequal(unique(vid[nt$var + 1L][pair]), c("investment.annual", "manufacturing.share_now", "manufacturing.share_2035"))
  # The critical-minerals capex split between Batteries and Electric Vehicles Refining.
  split <- nt$note == "cim_minerals_split"
  expect_setequal(unique(ik[split]), c("cell:batteries-refining", "cell:electric-vehicles-refining"))
  cv <- stats::setNames(meta$dict$vars$caveats, vid)
  expect_identical(cv[["consumption.balance"]], "production_assumed_zero|eia_production")
  expect_identical(cv[["production.fuels"]], "eia_production|ei_residual")
  expect_identical(cv[["consumption.self_sufficiency"]], "self_sufficiency_ref")
  expect_identical(cv[["investment.annual"]], "cim_estimate|cim_ev_purchases|same_figure_pair|cim_minerals_split|cim_geothermal")
  expect_identical(cv[["manufacturing.share_2035"]], "projection_pipeline|iea_etp_zero|same_figure_pair")
  # The R mirror of the contract: a note without its fact is an error; an unknown note id stops the build.
  m <- dash_osb_decode(dash_osb_encode(chunks$`sector-meta`, "m", "chunk"))$objects
  sizes <- list(geo = nrow(m$dims$geo), items = nrow(m$dims$items), vars = nrow(m$dict$vars))
  facts <- dash_osb_decode(dash_osb_encode(chunks$`sector-facts`, "f", "chunk"))$objects
  expect_identical(sector_check_chunk("sector-facts", facts, sizes), character())
  bad <- facts
  bad$facts$notes$year[1] <- 1999L
  expect_match(paste(sector_check_chunk("sector-facts", bad, sizes), collapse = "; "), "facts.notes has rows without a facts.obs row")
  b2 <- bundle
  b2$fact_notes$note[1] <- "no_such_caveat"
  expect_error(sector_chunks(b2), "note ids not in dict.caveats")
  b3 <- bundle
  b3$fact_notes$geo[1] <- "HKG"
  expect_error(sector_chunks(b3), "notes for facts that are not shipped")
  # Without notes the optional table is left out.
  b4 <- bundle
  b4$fact_notes <- NULL
  expect_null(sector_chunks(b4)$`sector-facts`$facts$notes)
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
  # Schema 2 (2026-10-28): six supply-chain stages.
  expect_identical(j$schema_version, 2L)
  expect_null(j$git_commit)
  expect_false(j$flags$include_restricted)
  expect_false(j$encoding$lossy)
  expect_identical(j$chunks[[2]]$tech, "batteries")
  expect_null(j$chunks[[1]]$tech)
  # The 66 headline cells (11 technologies x 6 stages), technology-major in chain order.
  expect_identical(length(j$coverage_glyph), 66L)
  expect_identical(vapply(j$coverage_glyph, function(g) g$stage, ""), rep(SECTOR_STAGE_SLUGS, 11))
  expect_identical(unique(vapply(j$coverage_glyph, function(g) g$tech, "")), SECTOR_HEADLINE_TECH_SLUGS)
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
  # Schema 2 budgets (2026-10-28), set from the first real six-stage export as the measured size + ~15%
  # where it left less than ~15% headroom (E4; mirrored in contract.ts): facts 670 kB (577.4 kB
  # measured), each sector-trade chunk 520 kB (largest 451.3 kB measured, electric-grid), combined
  # 26.5 MiB (Sector 5.67 + Strategic 17.18 = 22.84 MiB measured); meta keeps 150 kB (110.0 kB
  # measured). Schema 1: meta 150 kB, facts 600 kB, trade 450 kB, combined 25 MiB.
  expect_identical(SECTOR_BUDGET_GZ$`sector-meta`, 150 * 1024)
  expect_identical(SECTOR_BUDGET_GZ$`sector-facts`, 670 * 1024)
  expect_identical(SECTOR_BUDGET_GZ$`sector-trade`, 520 * 1024)
  expect_identical(SECTOR_BUDGET_GZ$sector_total, 9 * 1024^2)
  expect_identical(SECTOR_BUDGET_GZ$combined_total, 26.5 * 1024^2)
  ci <- tibble::tibble(name = c("sector-meta", "sector-trade-oil", "sector-facts"), bytes_gz = c(151 * 1024, 519 * 1024, 100))
  expect_identical(sector_over_budget(ci), "sector-meta")
  ci$bytes_gz <- c(149 * 1024, 521 * 1024, 671 * 1024)
  expect_identical(sector_over_budget(ci), c("sector-trade-oil", "sector-facts"))
})

test_that("the six-stage contract rules: 66 headline cells, mineral x stage items, technology-level exclusions, CM-FDI stages", {
  meta <- dash_osb_decode(dash_osb_encode(chunks$`sector-meta`, "m", "chunk"))$objects
  sizes <- list(geo = nrow(meta$dims$geo), items = nrow(meta$dims$items), vars = nrow(meta$dict$vars))
  it <- meta$dims$items
  cells <- it[it$axis == "cell", , drop = FALSE]
  expect_identical(cells$slug[cells$headline], sector_headline_cell_slugs())
  expect_setequal(unique(cells$stage), SUPPLY_CHAIN_STAGES)
  # The non-headline cells are the dictionary's Semiconductors stages.
  expect_identical(cells$slug[!cells$headline], c("semiconductors-refining", "semiconductors-components", "semiconductors-assembly"))
  ms <- it[it$axis == "mineral_stage", , drop = FALSE]
  expect_identical(ms$slug, c("lithium-extraction", "lithium-refining", "graphite-refining", "graphite-recovery"))
  expect_identical(ms$stage, c("Extraction", "Refining", "Refining", "Recovery"))
  expect_true(all(is.na(it$stage[!it$axis %in% c("cell", "mineral_stage")])))
  # Dropping a headline cell, a mineral_stage row without its mineral, a stage on another axis or an old
  # stage label breaks the contract.
  m2 <- meta
  m2$dims$items$headline[m2$dims$items$slug == "geothermal-recovery"] <- FALSE
  expect_match(paste(sector_check_chunk("sector-meta", m2), collapse = "\n"), "lacks headline cells")
  m3 <- meta
  m3$dims$items$slug[m3$dims$items$axis == "mineral_stage"][1] <- "cobalt-extraction"
  expect_match(paste(sector_check_chunk("sector-meta", m3), collapse = "\n"), "mineral_stage rows need")
  m4 <- meta
  m4$dims$items$stage[m4$dims$items$axis == "fuel"][1] <- "Extraction"
  expect_match(paste(sector_check_chunk("sector-meta", m4), collapse = "\n"), "only cell and mineral_stage rows carry a stage")
  m5 <- meta
  m5$dims$items$stage[m5$dims$items$slug == "oil-extraction"] <- "Upstream"
  expect_match(paste(sector_check_chunk("sector-meta", m5), collapse = "\n"), "stage has values outside the vocabulary")
  # trade.excluded: technology level, the chunk's own technology (excluded codes have no stage).
  tr <- dash_osb_decode(dash_osb_encode(chunks$`sector-trade-batteries`, "t", "chunk"))$objects
  expect_identical(names(tr$trade$excluded), c("tech", "sub_sector", "hs6", "reason"))
  expect_identical(unique(tr$trade$excluded$tech), "Batteries")
  bad <- tr
  bad$trade$excluded$tech <- "Oil"
  expect_match(paste(sector_check_chunk("sector-trade-batteries", bad, sizes), collapse = "\n"), "not the chunk's technology")
  # fdi.stages: the configured stages; every row keyed on the first stage's cell.
  fd <- dash_osb_decode(dash_osb_encode(chunks$`sector-fdi`, "f", "chunk"))$objects
  expect_identical(fd$fdi$stages, c("Components", "Assembly"))
  expect_true(all(endsWith(paste0(it$axis, ":", it$slug)[fd$fdi$projects$item + 1L], "-components")))
  bad <- fd
  bad$fdi$stages <- c("Components", "Midstream")
  expect_match(paste(sector_check_chunk("sector-fdi", bad, sizes), collapse = "\n"), "fdi.stages must list distinct supply-chain stages")
  # policy.pams stage_mask: six bits, one per stage in chain order (Recovery = bit 5).
  pol <- chunks$`sector-policy`$policy$pams
  expect_true(all(pol$stage_mask >= 0L & pol$stage_mask < bitwShiftL(1L, 6L)))
  expect_true(any(bitwAnd(pol$stage_mask, bitwShiftL(1L, 5L)) != 0L))
})
