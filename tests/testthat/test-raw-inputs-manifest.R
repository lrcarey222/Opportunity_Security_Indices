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

test_that("ingestion only requires manifest entries staged from sharepoint", {
  opsi_load_raw_inputs()
  manifest <- read_raw_inputs_manifest(raw_inputs_manifest_path(opsi_repo_root()))

  staged <- raw_inputs_staged_entries(manifest)
  expect_gt(length(staged), 0)

  # Pipeline-written outputs must never be demanded from the staging area.
  for (entry in staged) {
    expect_false(
      entry$source_type %in% c("api", "generated"),
      info = paste0(entry$id, " is pipeline-generated but marked staged_from: sharepoint")
    )
  }
})
