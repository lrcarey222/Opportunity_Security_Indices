# Behaviour of the ingest sync and vintage-resolution helpers. The regressions these
# cover: ingestion used to skip any file that already existed locally, which pinned
# stale vintages permanently, and readers pinned release years into file names.

opsi_load_raw_inputs_sync <- function() {
  source(
    file.path(normalizePath(test_path("..", ".."), winslash = "/"), "scripts", "utils", "raw_inputs.R"),
    local = FALSE
  )
}

# Base-R temp dir; the suite deliberately avoids extra package dependencies.
# Callers clean up with on.exit().
opsi_temp_dir <- function() {
  path <- file.path(tempdir(), paste0("opsi-", basename(tempfile(""))))
  dir.create(path, recursive = TRUE)
  path
}

opsi_write_tmp <- function(path, contents = "x", mtime = NULL) {
  writeLines(contents, path)
  if (!is.null(mtime)) Sys.setFileTime(path, mtime)
  path
}

test_that("vintage keys sort releases newest-first", {
  opsi_load_raw_inputs_sync()

  expect_equal(raw_input_vintage_key("2024-10-29 - New Energy Outlook 2024.csv"), 20241029)
  expect_equal(raw_input_vintage_key("GTA NIPO - July 2026.xlsx"), 20260701)
  expect_equal(raw_input_vintage_key("GTA NIPO - February 2026.xlsx"), 20260201)
  expect_equal(raw_input_vintage_key("WEO2025_AnnexA_Free_Dataset_World.csv"), 20250101)
  expect_equal(raw_input_vintage_key("critmin_import_2024.csv"), 20240101)
  expect_equal(raw_input_vintage_key("iea_criticalminerals_25.csv"), 20250101)
  expect_true(is.na(raw_input_vintage_key("ei_stat_review_world_energy.csv")))

  expect_gt(
    raw_input_vintage_key("GTA NIPO - July 2026.xlsx"),
    raw_input_vintage_key("GTA NIPO - February 2026.xlsx")
  )
})

test_that("resolve_versioned_raw_input picks the newest release, not the pinned one", {
  opsi_load_raw_inputs_sync()

  dir <- opsi_temp_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  opsi_write_tmp(file.path(dir, "GTA NIPO - February 2026.xlsx"))
  opsi_write_tmp(file.path(dir, "GTA NIPO - June 2026.xlsx"))
  opsi_write_tmp(file.path(dir, "GTA NIPO - July 2026.xlsx"))

  resolved <- resolve_versioned_raw_input(
    dir,
    pattern = "^GTA NIPO - .*\\.xlsx$",
    fallback = "GTA NIPO - February 2026.xlsx",
    quiet = TRUE
  )

  expect_equal(basename(resolved), "GTA NIPO - July 2026.xlsx")
})

test_that("resolve_versioned_raw_input falls back when nothing matches", {
  opsi_load_raw_inputs_sync()

  dir <- opsi_temp_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  resolved <- resolve_versioned_raw_input(
    dir,
    pattern = "^critmin_import_\\d{4}\\.csv$",
    fallback = "critmin_import_2024.csv",
    quiet = TRUE
  )

  expect_equal(basename(resolved), "critmin_import_2024.csv")
})

test_that("sync copies a newer staged file over an existing local copy", {
  opsi_load_raw_inputs_sync()

  staging <- opsi_temp_dir()
  local_dir <- opsi_temp_dir()
  on.exit(unlink(c(staging, local_dir), recursive = TRUE), add = TRUE)

  dest <- opsi_write_tmp(file.path(local_dir, "ei_stat_review_world_energy.csv"), "old", mtime = Sys.time() - 7200)
  src <- opsi_write_tmp(file.path(staging, "ei_stat_review_world_energy.csv"), "new and longer", mtime = Sys.time())

  expect_true(raw_input_needs_sync(src, dest, force = FALSE))
  expect_equal(sync_raw_file(src, dest, force = FALSE), "copied")
  expect_equal(readLines(dest), "new and longer")
})

test_that("sync is a no-op when the local copy is already current", {
  opsi_load_raw_inputs_sync()

  staging <- opsi_temp_dir()
  local_dir <- opsi_temp_dir()
  on.exit(unlink(c(staging, local_dir), recursive = TRUE), add = TRUE)

  stamp <- Sys.time() - 3600
  src <- opsi_write_tmp(file.path(staging, "same.csv"), "identical", mtime = stamp)
  dest <- opsi_write_tmp(file.path(local_dir, "same.csv"), "identical", mtime = stamp)

  expect_false(raw_input_needs_sync(src, dest, force = FALSE))
  expect_equal(sync_raw_file(src, dest, force = FALSE), "current")
})

test_that("force refresh recopies an otherwise-current file", {
  opsi_load_raw_inputs_sync()

  staging <- opsi_temp_dir()
  local_dir <- opsi_temp_dir()
  on.exit(unlink(c(staging, local_dir), recursive = TRUE), add = TRUE)

  stamp <- Sys.time() - 3600
  src <- opsi_write_tmp(file.path(staging, "same.csv"), "identical", mtime = stamp)
  dest <- opsi_write_tmp(file.path(local_dir, "same.csv"), "identical", mtime = stamp)

  expect_true(raw_input_needs_sync(src, dest, force = TRUE))
  expect_equal(sync_raw_file(src, dest, force = TRUE), "copied")
})

test_that("missing staged sources are reported rather than failing", {
  opsi_load_raw_inputs_sync()

  staging <- opsi_temp_dir()
  local_dir <- opsi_temp_dir()
  on.exit(unlink(c(staging, local_dir), recursive = TRUE), add = TRUE)

  expect_equal(
    sync_raw_file(file.path(staging, "absent.csv"), file.path(local_dir, "absent.csv")),
    "missing"
  )
})

test_that("pattern entries stage the newest matching release", {
  opsi_load_raw_inputs_sync()

  staging <- opsi_temp_dir()
  local_dir <- opsi_temp_dir()
  on.exit(unlink(c(staging, local_dir), recursive = TRUE), add = TRUE)

  opsi_write_tmp(file.path(staging, "GTA NIPO - February 2026.xlsx"))
  opsi_write_tmp(file.path(staging, "GTA NIPO - July 2026.xlsx"))

  entry <- normalize_raw_input_entry(list(
    id = "gta_nipo",
    path = "GTA NIPO - February 2026.xlsx",
    pattern = "^GTA NIPO - .*\\.xlsx$",
    source_type = "manual"
  ))

  expect_equal(sync_raw_input_entry(entry, staging, local_dir, force = FALSE), "copied")
  expect_true(file.exists(file.path(local_dir, "GTA NIPO - July 2026.xlsx")))
  expect_true(raw_input_present_locally(entry, local_dir))
})

test_that("OPSI_FORCE_REFRESH is read from the environment", {
  opsi_load_raw_inputs_sync()

  old <- Sys.getenv("OPSI_FORCE_REFRESH", unset = NA)
  on.exit(
    if (is.na(old)) Sys.unsetenv("OPSI_FORCE_REFRESH") else Sys.setenv(OPSI_FORCE_REFRESH = old),
    add = TRUE
  )

  Sys.setenv(OPSI_FORCE_REFRESH = "true")
  expect_true(opsi_force_refresh())

  Sys.setenv(OPSI_FORCE_REFRESH = "1")
  expect_true(opsi_force_refresh())

  Sys.setenv(OPSI_FORCE_REFRESH = "false")
  expect_false(opsi_force_refresh())

  Sys.unsetenv("OPSI_FORCE_REFRESH")
  expect_false(opsi_force_refresh())
})
