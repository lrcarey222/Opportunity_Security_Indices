# The committed synthetic parity fixtures of the dashboard's TypeScript engine
# (dashboard/src/engine/__fixtures__/synthetic, written by scripts/86_export_dashboard_fixtures.R)
# must stay what the R reference engine computes: a few scenarios are regenerated in memory and
# compared with the committed files, object by object. A failure means R/dashboard changed: rerun
# `Rscript scripts/86_export_dashboard_fixtures.R --set synthetic` and the TypeScript parity tests
# (`npm test` in dashboard/). Skipped when the fixture directory is absent.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "dashboard", "dash_files.R"))
for (f in dash_engine_files()) source(file.path(repo_root, f))
source(file.path(repo_root, "R", "dashboard", "dash_scenarios.R"))

fixture_root <- file.path(repo_root, "dashboard", "src", "engine", "__fixtures__", "synthetic")

read_osb_file <- function(path) {
  con <- gzfile(path, "rb")
  on.exit(close(con))
  chunks <- list()
  repeat {
    b <- readBin(con, "raw", n = 1048576L)
    if (!length(b)) break
    chunks[[length(chunks) + 1]] <- b
  }
  dash_osb_decode(do.call(c, chunks))
}

# Paths (dotted) of the leaves that differ between two nested lists.
leaf_diffs <- function(x, y, path = NULL) {
  is_branch <- function(v) is.list(v) && !is.data.frame(v)
  if (is_branch(x) && is_branch(y) && !is.null(names(x)) && !is.null(names(y))) {
    keys <- union(names(x), names(y))
    out <- character()
    for (k in keys) out <- c(out, leaf_diffs(x[[k]], y[[k]], if (is.null(path)) k else paste(path, k, sep = ".")))
    return(out)
  }
  if (identical(x, y)) character() else if (is.null(path)) "<top>" else path
}

synthetic_sets <- list(
  s1 = function() dash_fixture_bundle(seed = 1),
  s2 = function() dash_fixture_tie_bundle(dash_fixture_bundle(seed = 2, policy_required = "IEA PAMS Policy Index"), digits = 1)
)

for (set in names(synthetic_sets)) {
  test_that(paste0("synthetic fixtures ", set, " are what the R engine computes"), {
    dir <- file.path(fixture_root, set)
    skip_if_not(dir.exists(dir), paste("no fixtures at", dir))
    index <- jsonlite::fromJSON(file.path(dir, "index.json"), simplifyVector = FALSE)
    manifest <- jsonlite::fromJSON(file.path(dir, "data", "manifest.json"), simplifyVector = FALSE)
    expect_identical(index$bundle_id, manifest$bundle_id)

    bundle <- synthetic_sets[[set]]()
    base <- dash_fixture_baseline(bundle)

    # the deploy-format bundle (same chunks as scripts/85 writes)
    chunks <- dash_web_chunks(bundle, base$result)
    for (ch in manifest$chunks) {
      got <- read_osb_file(file.path(dir, "data", ch$file))$objects
      expect_identical(leaf_diffs(got, dash_osb_normalize(chunks[[ch$name]])), character(), label = paste(set, ch$name))
    }

    # scenarios: the baseline, the fossil exclusion, all four dials, the endowment screen flipped
    scen <- dash_fixture_scenarios(bundle)
    for (s in Filter(function(s) s$id %in% c("S00", "S08", "S11", "S17"), scen)) {
      opts <- list(full_arrays = TRUE, contrib = TRUE, var = TRUE, sample = FALSE, n_decompose = 2L)
      fo <- dash_fixture_outputs(bundle, s, base, opts)
      got <- read_osb_file(file.path(dir, paste0(s$id, ".osb")))$objects
      expect_identical(leaf_diffs(got, dash_osb_normalize(fo$objects)), character(), label = paste(set, s$id))
    }
  })
}

test_that("fixture references resolve to identical objects and dyad subsets are deterministic", {
  bundle <- dash_fixture_bundle(seed = 1)
  base <- dash_fixture_baseline(bundle)
  scen <- dash_fixture_scenarios(bundle)
  expect_false(any(vapply(scen, function(s) s$id == "S15", logical(1))))  # S15 (network settings only) is retired
  opts <- list(full_arrays = FALSE, contrib = FALSE, var = FALSE, sample = TRUE, n_decompose = 2L, dyad_reporters = 4L)
  s00 <- dash_fixture_outputs(bundle, scen[[1]], base, opts)$objects
  s17 <- Filter(function(s) s$id == "S17", scen)[[1]]
  s17 <- dash_fixture_outputs(bundle, s17, base, opts)$objects # the endowment screen only: L1, L2 and L4 as S00
  with_refs <- dash_fixture_refs(s17, s00, "S00", min_bytes = 0)
  expect_gt(dash_fixture_count_refs(with_refs), 5L)
  expect_true(inherits(with_refs$L1$es, "dash_osb_json"))
  expect_identical(unname(with_refs$L1$es$value["ref_path"]), "L1.es")
  # a reference is only written for identical leaves
  expect_false(inherits(with_refs$params, "dash_osb_json") && "ref_scenario" %in% names(with_refs$params$value))
  reps <- dash_fixture_dyad_reporters(bundle, 4L)
  expect_identical(reps, dash_fixture_dyad_reporters(bundle, 4L))
  expect_true(all(s00$L4$psi_dyad$reporter_iso %in% reps))
  expect_identical(as.character(s00$L4$psi_dyad_reporters), reps)
})
