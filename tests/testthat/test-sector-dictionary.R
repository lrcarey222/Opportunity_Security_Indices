# Sector Insights dictionary (config/sector_dictionary.yml) and its validator
# (R/dashboard/sector/sector_dictionary.R). No data needed.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "dashboard", "sector", "sector_files.R"))
for (f in sector_files()) source(file.path(repo_root, f))

dict_path <- file.path(repo_root, "config", "sector_dictionary.yml")
dict <- yaml::read_yaml(dict_path)

# The measure catalogue of dashboard/src/sector/contract.ts (id and this build's scope).
contract_catalogue <- function() {
  p <- file.path(repo_root, "dashboard", "src", "sector", "contract.ts")
  if (!file.exists(p)) return(NULL)
  ct <- readLines(p, warn = FALSE, encoding = "UTF-8")
  rows <- regmatches(ct, regexec("\\{ id: '([a-z_.0-9]+)', phase: '([^']+)', level: '([^']+)', chunks: \\[([^]]*)\\], scope: '([a-z]+)' \\}", ct))
  rows <- rows[lengths(rows) == 6]
  data.frame(id = vapply(rows, `[`, "", 2), scope = vapply(rows, `[`, "", 6), stringsAsFactors = FALSE)
}

# A minimal valid dictionary: every shared table, one measure.
minimal_dict <- function() {
  list(
    units = list(list(id = "usd", short = "USD", long = "US dollars", unit_kind = "usd"),
                 list(id = "ratio", short = "ratio", long = "ratio", unit_kind = "ratio")),
    sources = list(list(id = "src", short = "Source", citation = "A source (2026)", licence = "open")),
    caveats = list(list(id = "cv", text = "A caveat.")),
    themes = lapply(seq_along(SECTOR_THEMES), function(i) list(slug = SECTOR_THEMES[i], label = SECTOR_THEMES[i], order = i, default_var = "trade.exports")),
    families = lapply(SECTOR_FAMILIES, function(f) list(id = f, label = f)),
    techs = lapply(SECTOR_TECH_SLUGS, function(s) list(slug = s)),
    # The 66 headline cells (11 technologies x 6 stages): the validator requires each.
    cells = lapply(sector_headline_cell_slugs(), function(s) list(slug = s, cell_blurb = "A sector.")),
    vars = list(list(
      id = "trade.exports", theme = "trade", family = "exports", label = "Exports of the HS6 basket", short = "Exports",
      unit = "usd", data_year = "2024", years = list(2020, 2024), native_level = "country", item_axis = "cell", agg = "sum",
      world_basis = "sum_all", norms = list("value", "share"), source = "src", licence = "attribution", caveats = list("cv"),
      prov = "X", parity = list(none = "built from data/raw"), phase = "1"
    ))
  )
}

test_that("the dictionary of this build validates and covers the contract catalogue", {
  cat <- contract_catalogue()
  problems <- sector_dictionary_validate(dict, var_ids = cat$id)
  expect_identical(problems, character())
  ids <- vapply(dict$vars, function(v) v$id, "")
  expect_false(anyDuplicated(ids) > 0)
  skip_if(is.null(cat), "dashboard/src/sector/contract.ts not present")
  build <- cat$id[cat$scope == "build"]
  expect_setequal(ids, build)
})

test_that("every measure carries the fields captions need", {
  tabs <- sector_dictionary_tables(dict)
  v <- tabs$vars
  for (f in c("label", "unit", "data_year", "native_level", "agg", "world_basis", "source", "licence", "prov", "phase")) {
    expect_false(any(is.na(v[[f]]) | !nzchar(v[[f]])), info = f)
  }
  expect_true(all(v$unit %in% tabs$units$id))
  expect_true(all(v$source %in% tabs$sources$id))
  cv <- unlist(strsplit(stats::na.omit(v$caveats), "|", fixed = TRUE))
  expect_true(all(cv %in% tabs$caveats$id))
  expect_true(all(v$phase %in% SECTOR_PHASES))
  expect_true(all(v$licence %in% c("open", "attribution")))
})

test_that("no measure names an index, weight or contribution, and nothing licensed is listed", {
  probe <- unlist(lapply(dict$vars, function(v) c(v$id, v$label, v$short, v$parity$table, v$parity$variable)))
  hits <- probe[grepl(SECTOR_NORMATIVE_RE, probe, ignore.case = TRUE)]
  expect_identical(hits, character(0))
  # The dictionary text never maps a measure to an index / weight / contribution processed variable.
  y <- readLines(dict_path, warn = FALSE)
  par <- y[grepl("^\\s*(parity|variable|table):", y)]
  expect_false(any(grepl("\\b(index|weight|contribution)\\b|Overall", par, ignore.case = TRUE)))
  src <- unlist(lapply(dict$sources, function(s) c(s$id, s$short, s$citation, s$publisher)))
  expect_false(any(grepl("BNEF|BloombergNEF|\\bBCG\\b|Boston Consulting", src)))
  expect_true(all(vapply(dict$sources, function(s) s$licence %in% c("open", "attribution"), logical(1))))
})

test_that("units still unverified are labelled 'unit to confirm' (user decision 2026-10-10)", {
  tabs <- sector_dictionary_tables(dict)
  unit_of <- function(id) tabs$units$long[tabs$units$id == tabs$vars$unit[tabs$vars$id == id]]
  for (id in c("resources.wind_share", "resources.geo_lcoe_500", "resources.geo_lcoe_5000")) {
    expect_match(unit_of(id), "unit to confirm", info = id)
  }
  expect_match(tabs$units$long[tabs$units$id == "price_tbc"], "unit to confirm")
})

test_that("the validator accepts a minimal dictionary and names each problem", {
  expect_identical(sector_dictionary_validate(minimal_dict()), character())
  expect_identical(sector_dictionary_validate(minimal_dict(), var_ids = "trade.exports"), character())
  expect_match(sector_dictionary_validate(minimal_dict(), var_ids = "trade.imports"), "not in the measure catalogue")

  broken <- function(edit) {
    d <- minimal_dict()
    d <- edit(d)
    paste(sector_dictionary_validate(d), collapse = "\n")
  }
  expect_match(broken(function(d) { d$vars[[2]] <- d$vars[[1]]; d }), "duplicate id trade.exports")
  expect_match(broken(function(d) { d$vars[[1]]$unit <- "eur"; d }), "unit eur is not in units")
  expect_match(broken(function(d) { d$vars[[1]]$source <- "nowhere"; d }), "source nowhere is not in sources")
  expect_match(broken(function(d) { d$vars[[1]]$caveats <- list("cv", "missing"); d }), "caveats not in caveats: missing")
  expect_match(broken(function(d) { d$vars[[1]]$agg <- "ratio"; d }), "norm share needs agg sum")
  expect_match(broken(function(d) { d$vars[[1]]$norms <- list("value", "gdp"); d$vars[[1]]$native_level <- "region"; d }), "norm gdp")
  expect_match(broken(function(d) { d$vars[[1]]$label <- "Overall trade index"; d }), "names an index")
  expect_match(broken(function(d) { d$vars[[1]]$parity <- list(table = "x_tbl", variable = "trade weight"); d$vars[[1]]$prov <- "P"; d }), "names an index")
  expect_match(broken(function(d) { d$vars[[1]]$phase <- NULL; d }), "no phase")
  expect_match(broken(function(d) { d$vars[[1]]$licence <- "restricted"; d }), "licence restricted is not one of")
  expect_match(broken(function(d) { d$sources[[1]]$licence <- "restricted"; d }), "is not public")
  expect_match(broken(function(d) { d$vars[[1]]$parity <- list(); d }), "parity none")
  expect_match(broken(function(d) { d$vars[[1]]$prov <- "P"; d$vars[[1]]$parity <- list(none = "x"); d }), "prov P needs parity table")
  expect_match(broken(function(d) { d$vars[[1]]$theme <- "production"; d }), "differs from the id prefix")
  expect_match(broken(function(d) { d$vars[[1]]$item_axis <- "mineral"; d }), "needs link_stage")
  expect_match(broken(function(d) { d$families <- d$families[-1]; d }), "families: expected exactly")
  expect_match(broken(function(d) { d$vars[[1]]$primary_for <- list("batteries-assembly"); d$vars[[2]] <- modifyList(d$vars[[1]], list(id = "trade.imports")); d }),
               "two primary measures: batteries-assembly")
  # Six stages (2026-10-28): the cells are the 11 x 6 headline grid (plus Semiconductors stages), slugs
  # <tech>-<stage>; primary_for names known cells; link_stage is one or more stages, or any alone; no
  # text names an old stage.
  expect_match(broken(function(d) { d$cells[[1]]$slug <- "batteries-midstream"; d }), "not <tech>-<stage> of the six stages: batteries-midstream")
  expect_match(broken(function(d) { d$cells <- d$cells[-6]; d }), "headline cells without an entry: batteries-recovery")
  expect_match(broken(function(d) { d$cells[[2]]$cell_blurb <- NULL; d }), "batteries-refining: no cell_blurb")
  expect_match(broken(function(d) { d$vars[[1]]$primary_for <- list("batteries-upstream"); d }), "primary_for: cells not in cells: batteries-upstream")
  expect_identical(broken(function(d) { d$cells[[67]] <- list(slug = "semiconductors-refining", cell_blurb = "Chips."); d }), "")
  expect_match(broken(function(d) { d$vars[[1]]$item_axis <- "mineral"; d$vars[[1]]$link_stage <- "Upstream"; d }), "link_stage must be one or more of")
  expect_match(broken(function(d) { d$vars[[1]]$item_axis <- "mineral"; d$vars[[1]]$link_stage <- list("any", "Refining"); d }), "or any alone")
  expect_identical(broken(function(d) { d$vars[[1]]$item_axis <- "mineral"; d$vars[[1]]$link_stage <- list("Extraction", "Refining"); d }), "")
  expect_match(broken(function(d) { d$vars[[1]]$label <- "Upstream exports"; d }), "texts naming Upstream, Midstream or Downstream")
  expect_match(broken(function(d) { d$caveats[[1]]$text <- "On every Downstream sector."; d }), "texts naming Upstream")
  # The OECD's own CRS sub-sector name is not an old stage.
  expect_identical(broken(function(d) { d$caveats[[1]]$text <- "Mining less its 'Oil and gas (upstream)' sub-sector."; d }), "")
})

test_that("the dictionary of this build is six-stage: 66 headline cells, the Semiconductors stages, link stages", {
  cells <- vapply(dict$cells, function(c) c$slug, "")
  expect_identical(cells[cells %in% sector_headline_cell_slugs()], sector_headline_cell_slugs())
  expect_identical(setdiff(cells, sector_headline_cell_slugs()), c("semiconductors-refining", "semiconductors-components", "semiconductors-assembly"))
  expect_false(any(sector_has_legacy_stage(unlist(dict, use.names = FALSE))))
  tabs <- sector_dictionary_tables(dict)
  ls <- stats::setNames(tabs$vars$link_stage, tabs$vars$id)
  expect_identical(ls[["production.mining"]], "Extraction")
  expect_identical(ls[["production.refining"]], "Refining")
  expect_identical(ls[["resources.mineral_reserves"]], "Extraction")
  expect_identical(ls[["production.generation"]], "Deployment")
  expect_identical(ls[["trade.critmin_exports"]], "Extraction|Refining|Recovery")
  expect_identical(ls[["prices.latest"]], "Extraction|Refining")
  expect_identical(ls[["prices.wages"]], "any")
  ax <- stats::setNames(tabs$vars$item_axis, tabs$vars$id)
  expect_identical(unname(ax[c("trade.critmin_exports", "trade.critmin_dependence")]), c("mineral_stage", "mineral_stage"))
  pf <- unlist(strsplit(stats::na.omit(tabs$vars$primary_for), "|", fixed = TRUE))
  expect_true(all(pf %in% cells))
  expect_false(anyDuplicated(pf) > 0)
  # The figures shown on both manufacturing stages and the critical-minerals split carry their notes.
  expect_true(all(c("same_figure_pair", "cim_minerals_split") %in% tabs$caveats$id))
  expect_true(all(c("same_figure_pair", "cim_minerals_split") %in% SECTOR_DATA_CAVEATS))
  # The ILO activities serve the stages the pipeline files them under.
  expect_identical(unname(SECTOR_ACTIVITY_OF_STAGE[SUPPLY_CHAIN_STAGES]),
                   c("mining-utilities", "manufacturing", "manufacturing", "manufacturing", "construction", "mining-utilities"))
})

test_that("placeholders are filled from the data and unfilled ones are reported", {
  d <- minimal_dict()
  d$vars[[1]]$data_year <- "to {year_max}"
  d$caveats[[1]]$text <- "{n_codes} codes"
  tabs <- sector_dictionary_tables(d, fill = list(year_max = 2030))
  expect_identical(tabs$vars$data_year, "to 2030")
  expect_identical(sector_dictionary_unfilled(tabs), "{n_codes}")
  tabs2 <- sector_dictionary_tables(d, fill = list(year_max = 2030, n_codes = 255))
  expect_identical(tabs2$caveats$text, "255 codes")
  expect_identical(sector_dictionary_unfilled(tabs2), character())
  expect_identical(tabs$vars$years, "2020|2024")
  expect_identical(tabs$vars$norms, "value|share")
})
