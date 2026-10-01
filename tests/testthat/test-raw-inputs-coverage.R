# Coverage floor for versioned Comtrade pulls. `resolve: newest` used to hand the pipeline the
# freshest critmin file even when it was a 3-reporter stub (critmin_import_2025.csv: 3
# reporters against 130 in 2024), and every share, HHI and ranking computed on it silently.

opsi_coverage_root <- function() {
  normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
}

opsi_load_raw_inputs_coverage <- function() {
  source(file.path(opsi_coverage_root(), "scripts", "utils", "raw_inputs.R"), local = FALSE)
}

opsi_coverage_dir <- function() {
  path <- file.path(tempdir(), paste0("opsi-cov-", basename(tempfile(""))))
  dir.create(path, recursive = TRUE)
  path
}

# A Comtrade-shaped extract with n distinct reporters (two rows each).
opsi_write_comtrade <- function(path, n, reporter_col = "reporter_iso") {
  reporters <- if (n > 0) sprintf("R%02d", seq_len(n)) else character()
  tbl <- data.frame(
    ref_year = rep(2024L, 2 * n),
    reporter = rep(reporters, each = 2),
    primary_value = seq_len(2 * n),
    stringsAsFactors = FALSE
  )
  names(tbl)[names(tbl) == "reporter"] <- reporter_col
  utils::write.csv(tbl, path, row.names = FALSE)
  path
}

test_that("a floor walks past a stub vintage to the newest covered one", {
  opsi_load_raw_inputs_coverage()
  dir <- opsi_coverage_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  opsi_write_comtrade(file.path(dir, "critmin_import_2023.csv"), 120)
  opsi_write_comtrade(file.path(dir, "critmin_import_2024.csv"), 130)
  opsi_write_comtrade(file.path(dir, "critmin_import_2025.csv"), 3)

  pattern <- "^critmin_import_\\d{4}\\.csv$"

  # No floor: newest wins, as before.
  newest <- resolve_versioned_raw_input(dir, pattern = pattern, quiet = TRUE)
  expect_equal(basename(newest), "critmin_import_2025.csv")
  expect_null(attr(newest, "reporters"))

  expect_message(
    covered <- resolve_versioned_raw_input(dir, pattern = pattern, min_reporters = 50),
    "skipped newer critmin_import_2025.csv = 3"
  )
  expect_equal(basename(covered), "critmin_import_2024.csv")
  expect_equal(attr(covered, "reporters"), 130L)
})

test_that("when no vintage reaches the floor the best-covered one is used, with a warning", {
  opsi_load_raw_inputs_coverage()
  dir <- opsi_coverage_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  opsi_write_comtrade(file.path(dir, "critmin_export_2024.csv"), 20)
  opsi_write_comtrade(file.path(dir, "critmin_export_2025.csv"), 3)

  expect_warning(
    resolved <- resolve_versioned_raw_input(
      dir, pattern = "^critmin_export_\\d{4}\\.csv$", min_reporters = 50, quiet = TRUE
    ),
    "No .* vintage has 50 reporters"
  )
  expect_equal(basename(resolved), "critmin_export_2024.csv")
  expect_equal(attr(resolved, "reporters"), 20L)
})

test_that("a file without a reporter column is not counted as covered", {
  opsi_load_raw_inputs_coverage()
  dir <- opsi_coverage_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  opsi_write_comtrade(file.path(dir, "critmin_export_2025.csv"), 90, reporter_col = "something_else")

  expect_warning(
    resolved <- resolve_versioned_raw_input(
      dir, pattern = "^critmin_export_\\d{4}\\.csv$", min_reporters = 50, quiet = TRUE
    ),
    "Could not count reporters"
  )
  expect_equal(basename(resolved), "critmin_export_2025.csv")
  expect_null(attr(resolved, "reporters"))

  # A named reporter column is read instead of the defaults.
  expect_equal(
    raw_input_reporter_count(file.path(dir, "critmin_export_2025.csv"), reporter_col = "something_else"),
    90L
  )
})

test_that("the floor and reporter column can come from the manifest entry", {
  opsi_load_raw_inputs_coverage()
  dir <- opsi_coverage_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  opsi_write_comtrade(file.path(dir, "critmin_total_export_2024.csv"), 60, reporter_col = "reporterISO")
  opsi_write_comtrade(file.path(dir, "critmin_total_export_2025.csv"), 3, reporter_col = "reporterISO")

  pattern <- "^critmin_total_export_\\d{4}\\.csv$"
  manifest <- list(critmin_total_export = normalize_raw_input_entry(list(
    id = "critmin_total_export",
    path = "critmin_total_export_2024.csv",
    pattern = pattern,
    min_reporters = 50,
    reporter_col = "reporterISO",
    source_type = "api"
  )))

  resolved <- resolve_versioned_raw_input(dir, pattern = pattern, manifest = manifest, quiet = TRUE)
  expect_equal(basename(resolved), "critmin_total_export_2024.csv")
  expect_equal(attr(resolved, "reporters"), 60L)

  # An explicit argument wins over the manifest.
  loose <- resolve_versioned_raw_input(dir, pattern = pattern, manifest = manifest, min_reporters = 2, quiet = TRUE)
  expect_equal(basename(loose), "critmin_total_export_2025.csv")

  # A pattern edited in the script but not the manifest still finds its entry by the fallback
  # name, so the floor is not lost silently.
  edited <- "^critmin_total_export_[0-9]{4}\\.csv$"
  by_fallback <- resolve_versioned_raw_input(
    dir, pattern = edited, fallback = "critmin_total_export_2024.csv", manifest = manifest, quiet = TRUE
  )
  expect_equal(basename(by_fallback), "critmin_total_export_2024.csv")

  # With no entry at all the caller is told the floor was not applied.
  expect_warning(
    unfloored <- resolve_versioned_raw_input(dir, pattern = edited, manifest = manifest, quiet = TRUE),
    "No manifest entry matches"
  )
  expect_equal(basename(unfloored), "critmin_total_export_2025.csv")
})

test_that("min_reporters is parsed as a positive integer", {
  opsi_load_raw_inputs_coverage()

  base <- list(id = "x", path = "x_2024.csv", pattern = "^x_\\d{4}\\.csv$", source_type = "api")
  expect_true(is.na(normalize_raw_input_entry(base)$min_reporters))
  expect_identical(normalize_raw_input_entry(c(base, min_reporters = 50))$min_reporters, 50L)
  expect_error(normalize_raw_input_entry(c(base, min_reporters = 0)), "positive integer")
  expect_error(normalize_raw_input_entry(c(base, min_reporters = "many")), "positive integer")
})

test_that("the staged manifest puts a 50-reporter floor on every critmin pull", {
  opsi_load_raw_inputs_coverage()
  manifest <- read_raw_inputs_manifest(raw_inputs_manifest_path(opsi_coverage_root()))

  for (id in c("critmin_import", "critmin_export", "critmin_total_export")) {
    expect_identical(manifest[[id]]$min_reporters, 50L, info = id)
    expect_identical(manifest[[id]]$reporter_col, "reporter_iso", info = id)
  }
})

test_that("the staged critmin pulls resolve past the 2025 stub", {
  opsi_load_raw_inputs_coverage()
  root <- opsi_coverage_root()
  raw_dir <- file.path(root, "data", "raw")
  skip_if_not(file.exists(file.path(raw_dir, "critmin_import_2024.csv")), "critmin pulls not staged")

  manifest <- read_raw_inputs_manifest(raw_inputs_manifest_path(root))
  for (id in c("critmin_import", "critmin_export", "critmin_total_export")) {
    resolved <- resolve_versioned_raw_input(raw_dir, pattern = manifest[[id]]$pattern, manifest = manifest, quiet = TRUE)
    expect_gte(attr(resolved, "reporters"), 50L)
    expect_false(grepl("2025", basename(resolved)), info = id)
  }
})

test_that("write_resolved_vintages stamps the reporter count", {
  opsi_load_raw_inputs_coverage()
  dir <- opsi_coverage_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  opsi_write_comtrade(file.path(dir, "critmin_import_2024.csv"), 70)
  resolved <- resolve_versioned_raw_input(dir, pattern = "^critmin_import_\\d{4}\\.csv$", min_reporters = 50, quiet = TRUE)

  out <- write_resolved_vintages(list(critmin_import = resolved), dir)
  stamped <- yaml::read_yaml(out)$inputs[[1]]
  expect_equal(stamped$file, "critmin_import_2024.csv")
  expect_equal(stamped$reporters, 70L)
})
