repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "dashboard", "yards_fences", "yf_build.R"))

read_assessments <- function() {
  utils::read.csv(file.path(repo_root, "data", "reference", "yards_fences_assessments.csv"),
                  stringsAsFactors = FALSE, check.names = FALSE, na.strings = character(), encoding = "UTF-8")
}

test_that("the curated assessment table is valid and leaves gaps as Unknown", {
  ass <- read_assessments()
  expect_identical(yf_validate_assessments(ass), character())
  expect_true(all(c("component_id", "evidence_scope", "consequence", "source", "confidence") %in% names(ass)))
  # the demonstration cases the brief asks for are present
  expect_true(all(c("grid-scada", "grid-inverter-firmware", "ree-magnets", "bess-cells", "pv-modules", "nuc-haleu") %in% ass$component_id))
  # unknowns stay unknown: e.g. no stockpile cover is asserted for any component
  expect_true(all(yf_is_unknown(ass$stockpile)))
})

test_that("invalid assessment values are reported, not coerced", {
  ass <- read_assessments()
  ass$consequence[1] <- "catastrophic"
  ass$layer[2] <- "firmware"
  ass$non_allied_top_share_override[3] <- "0.7"
  ass$override_subject[3] <- ""
  bad <- yf_validate_assessments(ass)
  expect_true(any(grepl("consequence: invalid value\\(s\\) catastrophic", bad)))
  expect_true(any(grepl("invalid layer", bad)))
  expect_true(any(grepl("override without a subject", bad)))
})

test_that("components keep curated answers and drop Unknowns", {
  comps <- yf_components(read_assessments())
  haleu <- Filter(function(c) c$id == "nuc-haleu", comps)[[1]]
  expect_identical(haleu$shareOverride, list(value = 0.9, subject = "Russia"))
  expect_identical(haleu$assessments$consequence, "high")
  expect_null(haleu$assessments$stockpile)
  fw <- Filter(function(c) c$id == "grid-inverter-firmware", comps)[[1]]
  expect_null(fw$evidence)
  expect_null(fw$assessments$separability)
  cells <- Filter(function(c) c$id == "bess-cells", comps)[[1]]
  expect_identical(cells$evidence, list(scope = "subcomponent", scopeId = "batteries-midstream::Battery Cells"))
  expect_identical(cells$secondary[[1]], list(scope = "stage", scopeId = "batteries-midstream"))
})

test_that("template filling stops on a placeholder it cannot fill", {
  expect_identical(yf_fill("{subject} holds {value}", list(subject = "China", value = "60%")), "China holds 60%")
  expect_error(yf_fill("{subject} holds {value}", list(subject = "China")), "unfilled placeholder")
})

test_that("shares rank economies only: aggregates and entities are mapped, not double counted", {
  meta <- list(
    dims = list(
      geo = data.frame(code = c("CHN", "USA", "EUU", "WLD", "E:iea_etp:us", "DEU"), name = c("China", "United States", "European Union (27)", "World", "United States (IEA)", "Germany"),
                       kind = c("country", "country", "eu", "world", "entity", "country"), tier = c("none", "core", "core", "none", "none", "core"), stringsAsFactors = FALSE),
      items = data.frame(axis = "cell", slug = "x-midstream", label = "X", tech = "X", stage = "Midstream", stringsAsFactors = FALSE)
    ),
    dict = list(vars = data.frame(id = "trade.exports", source = "s", caveats = NA, stringsAsFactors = FALSE),
                sources = data.frame(id = "s", short = "Source", edition = "2024", stringsAsFactors = FALSE))
  )
  lk <- yf_lookup(meta)
  sh <- yf_shares(geo_idx = 0:5, value = c(60, 20, 30, 100, 0.2, 10), W = 100, lk = lk)
  expect_identical(sh$iso, c("CHN", "USA", "DEU", "USA"))  # EU bloc and World dropped; the IEA entity maps to USA
  expect_equal(sh$share[1:3], c(0.6, 0.2, 0.1))
  expect_identical(sh$tier[4], "core")  # the entity takes its country's tier
})

test_that("a home economy missing from a source is unknown, not zero", {
  lk <- list(geo = data.frame(code = c("CHN", "USA", "E:iea_etp:us"), iso = c("CHN", "USA", "USA"), stringsAsFactors = FALSE))
  expect_identical(yf_home_status(0:1, c(60, 5), c("obs", "obs"), "USA", lk), "listed")
  expect_identical(yf_home_status(0:1, c(60, 0), c("obs", "zero"), "USA", lk), "zero")
  expect_identical(yf_home_status(c(0L, 2L), c(80, NA), c("obs", "nq"), "USA", lk), "unlisted")
  expect_identical(yf_home_status(0L, 60, "obs", "USA", lk), "unlisted")
  # sub-component trade has no status column: a row with 0 exports is a recorded zero
  expect_identical(yf_home_status(0:1, c(60, 0), NULL, "USA", lk), "zero")
})

test_that("the metric map keeps normalised components and headline indices away from the engine", {
  map <- yaml::read_yaml(file.path(repo_root, "config", "yards_fences_metric_map.yml"))
  norm <- Filter(function(m) isTRUE(m$normalized), map$metrics)
  expect_gt(length(norm), 0)
  expect_true(all(vapply(norm, function(m) is.null(m$engine_input), logical(1))))
  labels <- vapply(map$metrics, function(m) m$label, character(1))
  expect_false(any(grepl("Energy Security Index|Economic Opportunity Index|Strategic Index", labels)))
  # every stack layer is a curated component
  ids <- read_assessments()$component_id
  expect_true(all(unlist(lapply(map$stacks, function(s) s$layers)) %in% ids))
})

test_that("one-item lists survive JSON serialisation as arrays", {
  pri <- lapply(list(subcomponent = list("sub_exports")), function(x) I(unlist(x)))
  json <- as.character(jsonlite::toJSON(list(enginePriority = pri), auto_unbox = TRUE))
  expect_identical(json, '{"enginePriority":{"subcomponent":["sub_exports"]}}')
})
