# Guards the contract that broke before: config/raw_inputs_manifest.yml was generated
# from legacy/ while the pipeline read scripts/, so the two drifted apart silently.

opsi_repo_root <- function() {
  normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
}

opsi_load_raw_inputs <- function() {
  source(file.path(opsi_repo_root(), "scripts", "utils", "raw_inputs.R"), local = FALSE)
}

test_that("manifest parses under schema v2 and has unique, well-formed entries", {
  opsi_load_raw_inputs()
  manifest <- read_raw_inputs_manifest(raw_inputs_manifest_path(opsi_repo_root()))

  expect_gt(length(manifest), 0)

  ids <- vapply(manifest, function(e) e$id, character(1))
  expect_equal(anyDuplicated(ids), 0)

  for (entry in manifest) {
    label <- entry$id

    # v1 folded long OneDrive paths across lines, which produced entries like
    # "Raw" and "NEIS Center Asia" that could never resolve to a file.
    if (!is.na(entry$path)) {
      expect_false(grepl("[\r\n]", entry$path), info = label)
      expect_match(entry$path, "\\.[A-Za-z0-9]+$", info = label)
    }

    expect_true(!is.na(entry$path) || !is.na(entry$pattern), info = label)
    expect_true(entry$source_type %in% OPSI_RAW_SOURCE_TYPES, info = label)
    expect_true(entry$staged_from %in% OPSI_RAW_STAGED_FROM, info = label)

    if (!is.na(entry$pattern)) {
      # A broken regex here would silently resolve to the fallback forever.
      expect_error(grepl(entry$pattern, "probe.csv"), NA)
      if (!is.na(entry$path)) {
        expect_match(basename(entry$path), entry$pattern, info = label)
      }
    }
  }
})

test_that("every raw input the active pipeline reads has a manifest entry", {
  opsi_load_raw_inputs()
  repo_root <- opsi_repo_root()

  manifest <- read_raw_inputs_manifest(raw_inputs_manifest_path(repo_root))
  discovered <- discover_raw_input_references(repo_root)

  expect_gt(length(discovered), 0)

  unmatched <- Filter(
    function(ref) is.null(match_raw_input_entry(manifest, ref)),
    discovered
  )

  describe <- function(ref) {
    what <- if (!is.na(ref$pattern)) paste0("pattern ", ref$pattern) else ref$path
    paste0(what, " (read by ", paste(ref$required_by, collapse = ", "), ")")
  }

  expect_equal(
    length(unmatched), 0,
    info = paste0(
      "Raw inputs referenced by scripts/ or R/ but absent from ",
      "config/raw_inputs_manifest.yml:\n",
      paste0("  - ", vapply(unmatched, describe, character(1)), collapse = "\n"),
      "\nRun: Rscript scripts/01_generate_raw_inputs_manifest.R"
    )
  )
})

test_that("manifest carries no entries the active pipeline no longer reads", {
  opsi_load_raw_inputs()
  repo_root <- opsi_repo_root()

  manifest <- read_raw_inputs_manifest(raw_inputs_manifest_path(repo_root))
  discovered <- discover_raw_input_references(repo_root)

  matched_ids <- unique(vapply(
    discovered,
    function(ref) {
      entry <- match_raw_input_entry(manifest, ref)
      if (is.null(entry)) NA_character_ else entry$id
    },
    character(1)
  ))

  stale <- Filter(
    function(e) !(e$id %in% matched_ids) && !isTRUE(e$retain),
    manifest
  )

  expect_equal(
    length(stale), 0,
    info = paste0(
      "Manifest entries no longer referenced by any active script:\n",
      paste0("  - ", vapply(stale, function(e) e$id, character(1)), collapse = "\n"),
      "\nRemove them, or set 'retain: true' if they are still needed."
    )
  )
})

test_that("required theme inputs are all declared in the manifest", {
  opsi_load_raw_inputs()
  repo_root <- opsi_repo_root()

  # 10_build_themes.R fails fast on this vector, so it is the real contract for a run.
  themes_text <- paste(
    readLines(file.path(repo_root, "scripts", "10_build_themes.R"), warn = FALSE),
    collapse = "\n"
  )
  block <- stringr::str_match(themes_text, "(?s)missing_files <- c\\((.*?)\\)\\s*\\n")

  expect_false(is.na(block[1, 1]))

  declared_vars <- trimws(strsplit(block[1, 2], ",")[[1]])
  declared_vars <- declared_vars[nzchar(declared_vars)]

  expect_gt(length(declared_vars), 10)

  # Each entry must be a path variable assigned earlier in the same script.
  for (var in declared_vars) {
    expect_match(
      themes_text,
      paste0("(?m)^", var, "\\s*<-"),
      perl = TRUE,
      info = paste0("missing_files references undefined variable: ", var)
    )
  }
})

test_that("pipeline-generated outputs are never demanded from the staging area", {
  opsi_load_raw_inputs()
  manifest <- read_raw_inputs_manifest(raw_inputs_manifest_path(opsi_repo_root()))

  staged <- raw_inputs_staged_entries(manifest)
  expect_gt(length(staged), 0)

  # source_type says where data originates; staged_from says how it can arrive locally.
  # An api source may legitimately also have a staged fallback, but something the
  # pipeline itself writes must never be requested from SharePoint.
  for (entry in staged) {
    expect_false(
      identical(entry$source_type, "generated"),
      info = paste0(entry$id, " is pipeline-generated but marked staged_from: sharepoint")
    )
  }
})

test_that("repo-staged crosswalks are present in data/reference", {
  opsi_load_raw_inputs()
  repo_root <- opsi_repo_root()

  manifest <- read_raw_inputs_manifest(raw_inputs_manifest_path(repo_root))
  repo_entries <- Filter(function(e) identical(e$staged_from, "repo"), manifest)

  # These exist so a fresh clone can build without access to anyone's OneDrive.
  expect_gt(length(repo_entries), 0)

  reference_dir <- raw_inputs_reference_dir(repo_root)
  expect_true(dir.exists(reference_dir))

  for (entry in repo_entries) {
    path <- file.path(reference_dir, entry$path)
    expect_true(
      file.exists(path),
      info = paste0(
        entry$id, " is staged_from: repo but data/reference/", entry$path, " is missing"
      )
    )
    if (file.exists(path)) {
      expect_gt(file.info(path)$size, 0)
    }
  }
})

test_that("every api-sourced input is backed by a fetcher or by the pipeline", {
  repo_root <- opsi_repo_root()
  opsi_load_raw_inputs()
  source(file.path(repo_root, "scripts", "utils", "fetchers.R"), local = FALSE)
  source_fetcher_files(repo_root)

  manifest <- read_raw_inputs_manifest(raw_inputs_manifest_path(repo_root))
  api_entries <- Filter(function(e) identical(e$source_type, "api"), manifest)
  expect_gt(length(api_entries), 0)

  for (entry in api_entries) {
    # Either a registered fetcher owns it, or an ingest step writes it directly.
    covered <- !is.null(get_fetcher(entry$id)) || identical(entry$staged_from, "pipeline")
    expect_true(
      covered,
      info = paste0(entry$id, " is source_type: api but has no fetcher and is not pipeline-written")
    )
  }
})

test_that("fetch_policy values are valid and only set where a fetcher exists", {
  repo_root <- opsi_repo_root()
  opsi_load_raw_inputs()
  source(file.path(repo_root, "scripts", "utils", "fetchers.R"), local = FALSE)
  source_fetcher_files(repo_root)

  manifest <- read_raw_inputs_manifest(raw_inputs_manifest_path(repo_root))

  for (entry in manifest) {
    expect_true(
      entry$fetch_policy %in% c("prefer", "fallback", "never"),
      info = paste0(entry$id, " has invalid fetch_policy: ", entry$fetch_policy)
    )

    if (identical(entry$fetch_policy, "prefer")) {
      expect_false(
        is.null(get_fetcher(entry$id)),
        info = paste0(entry$id, " sets fetch_policy: prefer but has no registered fetcher")
      )
    }
  }
})
