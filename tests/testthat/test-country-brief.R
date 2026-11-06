# Country strategy briefs (R/dashboard/country/country_brief*.R, scripts/92_export_country_briefs.R): the
# facts of the Australia pilot against the published posture and AIB bundles, the source grammar and its
# lint rules, the document AST, and the JSON shape the dashboard reads.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
for (f in c("R/dashboard/country/country_util.R", "R/dashboard/country/country_memo.R", "R/dashboard/country/country_memo_lint.R",
            "R/strategic_position/pathway.R", "R/dashboard/country/country_devpart.R",
            "R/dashboard/country/country_brief.R", "R/dashboard/country/country_brief_objects.R", "R/dashboard/country/country_brief_dev.R",
            "R/dashboard/country/country_brief_doc.R", "R/dashboard/country/country_brief_charts.R")) source(file.path(repo_root, f))
cfg <- yaml::read_yaml(file.path(repo_root, "config", "country_brief.yml"))
dcfg <- yaml::read_yaml(file.path(repo_root, "config", "development_partnerships.yml"))

read_bundles <- function() {
  pd <- file.path(repo_root, "dashboard", "public", "data", "posture")
  ad <- file.path(repo_root, "dashboard", "public", "data", "aib")
  if (!file.exists(file.path(pd, "manifest.json")) || !file.exists(file.path(ad, "manifest.json"))) return(NULL)
  rj <- function(p) jsonlite::fromJSON(p, simplifyVector = FALSE)
  pm <- rj(file.path(pd, "manifest.json"))
  am <- rj(file.path(ad, "manifest.json"))
  list(core = rj(file.path(pd, pm$core)), detail = rj(file.path(pd, pm$details$AUS)), aib = rj(file.path(ad, am$files$compacts)),
       managed = rj(file.path(ad, am$files$scenarios$managed)))
}

test_that("the Australia facts follow the published bundles", {
  b <- read_bundles()
  skip_if(is.null(b), "no published posture or AIB bundle")
  f <- country_brief_facts(b$core, b$detail, b$aib, b$managed, "AUS", cfg, "30 October 2026")
  # one row per step of the Strategy Map, and the dashboard's choice of each (grammar.ts verbOf)
  expect_equal(f$counts$steps$v, 66L)
  cc <- country_brief_core_cells(b$core)
  cc <- cc[cc$iso3 == "AUS", ]
  for (ch in c("secure", "preserve", "catch_up", "partner", "accelerate", "watch", "not_assessed")) {
    expect_equal(f$counts[[ch]]$v, sum(cc$choice == ch), info = ch)
  }
  expect_equal(f$counts$mixed$v, sum(cc$yd %in% c("SM", "ML", "SL", "SML")))
  expect_equal(f$cells[["electric-vehicles-refining"]]$choice$d, "Secure")
  expect_equal(f$cells[["electric-vehicles-refining"]]$fence$d, "High")
  # High-fence capabilities: every Secure step and layer in exactly one, ranked by the highest need
  units <- country_brief_secure_units(country_brief_tables(b$core, b$detail, "AUS"))
  expect_equal(f$counts$high_units$v, nrow(units))
  needs <- vapply(f$high_fence, function(g) g$need$v, 0)
  expect_false(is.unsorted(rev(needs)))
  expect_equal(sum(vapply(f$high_fence, function(g) g$n_parts$v, 0L)), nrow(units))
  # the AIB facts come from the strategic layer and the two scenario files
  me <- Filter(function(x) identical(x$iso3, "AUS"), b$aib$strategic$countries)[[1]]
  expect_equal(f$aib$role$d, me$slots$headline)
  expect_equal(f$aib$n_compact_roles$v, length(Filter(function(r) identical(r$iso3, "AUS"), b$aib$strategic$compactRoles)))
  expect_false("Australia" %in% unlist(f$aib$compacts[["SC-NUCLEAR"]]$core_others$v))
})

test_that("facts keep their arrays in JSON and lists read as running text", {
  one <- country_brief_list("Nuclear Fuel & Reactor Systems", "neis_aib", "x")
  js <- as.character(country_json(list(a = one)))
  expect_match(js, '"v":\\["Nuclear Fuel & Reactor Systems"\\]')
  expect_match(js, '"src":\\["neis_aib"\\]')
  expect_equal(country_brief_join(c("Japan", "the United States")), "Japan and the United States")
  expect_equal(country_brief_join(c("Japan", "Korea", "Italy")), "Japan, Korea and Italy")
  # three or more items of which one holds a comma or an "and" take semicolons; two items never do
  expect_equal(country_brief_join(c("blades, towers and nacelle parts", "wind turbines")), "blades, towers and nacelle parts and wind turbines")
  expect_equal(country_brief_join(c("battery cells and packs", "wind turbines", "PV modules and inverters")),
               "battery cells and packs; wind turbines; and PV modules and inverters")
  expect_equal(country_brief_pct_text(0.004), "<1%")
  # half up, as the dashboard rounds (R's round() would print 86)
  expect_equal(country_brief_score(0.865, "s", "m")$d, "87")
  expect_equal(country_brief_pct_text(0.125), "13%")
  expect_equal(country_brief_usd_text(8.08e9), "US$8.1 billion")
  expect_equal(country_brief_usd_text(2.65e8), "US$265 million")
})

# A tiny brief: the seven sections, the choice box, the figures, the High-fence table and the generated tables.
fill <- list(country = "Testland", country_poss = "Testland's", as_of_label = "30 October 2026",
             development_title = "Development strategy — build trusted capacity by helping partners Catch Up")
facts <- list(iso3 = "TST", meta = list(editions = list()),
              lists = list(build = country_brief_list(c("widget refining", "gadget assembly"), "neis_strategy", "m", c("x-refining", "y-assembly"))),
              counts = list(build = country_brief_count(2L, "neis_strategy", "m", c("x-refining", "y-assembly"))),
              dev = list(rows = list(r1 = list(key = country_brief_fv("x-refining", "x-refining", "neis_strategy", "m"),
                                               partner = country_brief_fv("MID", "Midland", "neis_strategy", "m")))),
              cells = list(`x-refining` = list(name = country_brief_fv("x", "widget refining", "neis_strategy", "m"),
                                                need = country_brief_fv(0.86, "86", c("neis_strategy", "comtrade_matrix"), "need"),
                                                partners = country_brief_list(c("Japan", "the United States"), "neis_aib", "p"))))
section <- function(i, body) paste0("## ", country_brief_titles(cfg, fill)[i], "\n\n", body, "\n\n")
words <- paste(rep("Testland should build what it can anchor and buy the rest from trusted partners.", 140), collapse = " ")
good_src <- function(table_rows = 3) {
  rows <- paste(rep("| Widgets | It matters. | Need is {{cells.x-refining.need}}. | **Allied.** Buy from {{cells.x-refining.partners}}. |", table_rows), collapse = "\n")
  paste0(
    section(1, paste0(words, "\n\n:::choices\n- **Secure.** Widgets.\n- **Build / preserve.** Refining.\n- **Partner.** Cells.\n- **Deploy / stay open.** Grids.\n:::\n\n@figure strategy_map\nEach cell is a step.")),
    section(2, paste0("### High fence — what cannot fail\n\n| Capability | Why it matters | Main vulnerability | Recommended response |\n|---|---|---|---|\n", rows,
                      "\n\n@figure value_position\nValue against position.")),
    section(3, "It supplies widgets.\n\n@figure aib_roles\nRoles."), section(4, "Partners can catch up.\n\n@table development"),
    section(5, "Deploy faster."), section(6, "Bargains.\n\n@table policy_gaps"),
    section(7, "Confident."),
    "## ", cfg$brief$appendix_title, "\n\n| Step | Need |\n|---|---|\n| {{cells.x-refining.name|cap}} | {{cells.x-refining.need}} |\n")
}
lint_of <- function(src, review_keys = character(), dev_empty = FALSE, review_names = character(), n_high = NULL) {
  stub <- list(policy_gaps = list(head = c("Capability", "Need"), rows = list(c("{{cells.x-refining.name|cap}}", "{{cells.x-refining.need}}"))),
               development = list(head = c("Capability", "Partner"), rows = list(c("{{cells.x-refining.name|cap}}", "{{cells.x-refining.partners}}"))))
  parsed <- country_brief_expand_tables(country_brief_parse(src, cfg, fill), stub)
  res <- country_brief_resolve(parsed, facts)
  sents <- country_brief_sentences(res)
  list(parsed = parsed, res = res, sents = sents, lint = country_brief_lint(parsed, res, sents, cfg, fill, c("Japan", "Testland"), review_keys, dev_empty, review_names, n_high))
}
checks_of <- function(l, sev = "error") vapply(Filter(function(x) x$severity == sev, l$lint$items), function(x) x$check, "")

test_that("a well-formed brief lints clean, and table cells keep their token filters", {
  l <- lint_of(good_src())
  expect_equal(checks_of(l), character())
  tab <- Filter(function(b) b$type == "table", l$parsed$appendix$blocks)[[1]]
  expect_equal(tab$rows[[1]][1], "{{cells.x-refining.name|cap}}")
  doc <- country_brief_document(l$parsed, l$res, country_brief_claims(l$res, l$sents), facts, cfg, fill,
                                list(generated_at = "t", verified_at = "t", writer = "w", verifier = "v", src_sha256 = "s", claims_sha256 = "c", facts_sha256 = "f", as_of = "2026-10-30"))
  expect_equal(doc$format, "brief")
  hf <- Filter(function(b) b$type == "table", doc$sections[[2]]$blocks)[[1]]
  # a table carries its source numbers once, not on every cell
  expect_true(length(hf$refs) > 0)
  expect_equal(length(unlist(hf$rows[[1]][[3]]$refs)), 0L)
  box <- Filter(function(b) b$type == "choices", doc$sections[[1]]$blocks)[[1]]
  expect_equal(vapply(box$items, function(x) x$kind, ""), c("secure", "preserve", "partner", "accelerate"))
  expect_equal(vapply(box$items, function(x) x$label, "")[2], "Build / preserve")
  expect_match(country_brief_html(doc), "<table class=\"t\">", fixed = TRUE)
  expect_match(country_brief_markdown(doc), "| Step | Need |", fixed = TRUE)
})

test_that("the lint rules fire", {
  expect_true("digit" %in% checks_of(lint_of(sub("Deploy faster.", "Deploy 40 GW faster.", good_src(), fixed = TRUE))))
  expect_true("banned" %in% checks_of(lint_of(sub("Deploy faster.", "Deploy faster downstream.", good_src(), fixed = TRUE))))
  expect_true("token" %in% checks_of(lint_of(sub("Deploy faster.", "Deploy {{cells.x-refining.nope}}.", good_src(), fixed = TRUE))))
  expect_true("structure" %in% checks_of(lint_of(sub("@figure value_position", "", good_src(), fixed = TRUE))))
  expect_true("structure" %in% checks_of(lint_of(good_src(table_rows = 7))))
  # fewer High-fence capabilities than the minimum: all of them, however few
  expect_true("structure" %in% checks_of(lint_of(good_src(table_rows = 1))))
  expect_false("structure" %in% checks_of(lint_of(good_src(table_rows = 1), n_high = 1L)))
  expect_true("structure" %in% checks_of(lint_of(sub(country_brief_titles(cfg, fill)[3], "Deployment", good_src(), fixed = TRUE))))
  expect_true("words" %in% checks_of(lint_of(sub(words, "Short.", good_src(), fixed = TRUE))))
  expect_true("economy_name" %in% checks_of(lint_of(sub("Bargains.", "Bargains with Japan.", good_src(), fixed = TRUE)), "warning"))
  expect_true("number_word" %in% checks_of(lint_of(sub("Bargains.", "Two bargains.", good_src(), fixed = TRUE)), "warning"))
  # the required development table, and a donor with no credible pair must say so
  expect_true("structure" %in% checks_of(lint_of(sub("@table development", "", good_src(), fixed = TRUE))))
  expect_true("structure" %in% checks_of(lint_of(good_src(), dev_empty = TRUE)))
  expect_false("structure" %in% checks_of(lint_of(sub("Partners can catch up.", "No credible development-led supply solution identified.", good_src(), fixed = TRUE), dev_empty = TRUE)))
})

test_that("low-confidence and review calls never reach the Bottom line or the Agenda", {
  # the Bottom line cites x-refining; once x-refining is a review case the lint stops publication
  src <- sub("Testland should build what it can anchor", "Testland should build {{cells.x-refining.name}} and what it can anchor", good_src(), fixed = TRUE)
  expect_false("headline_review" %in% checks_of(lint_of(src)))
  expect_true("headline_review" %in% checks_of(lint_of(src, review_keys = "x-refining")))
  # the same token elsewhere (section 2) is allowed: review cases stay visible, labelled, outside the headlines
  src2 <- sub("Value against position.", "Value against position for {{cells.x-refining.name}}.", good_src(), fixed = TRUE)
  expect_false("headline_review" %in% checks_of(lint_of(src2, review_keys = "x-refining")))
})

test_that("a missing detail field never reads a longer field by partial matching", {
  b <- read_bundles()
  skip_if(is.null(b), "no published posture or AIB bundle")
  d <- list(top_supplier_share = 0.4, coercive_top_share = 0.3, policy = list())
  row <- data.frame(iso3 = "AUS", tech = "Oil", stage = "Recovery", node = "Oil|Recovery", name = "waste-oil recovery", stage_display = "Recovery",
                    choice = "watch", fence = "low", mixed = FALSE, yard_rollup = "large", need = 0.5, prize = 0.2, io = 0.2, cp = NA, share = NA,
                    pw = "no", ev = NA, conf = NA, stringsAsFactors = FALSE)
  tb <- list(layers = data.frame(node = character(), layer_id = character()))
  f <- country_brief_cell_facts(row, d, tb, cfg, function(x) x)
  expect_null(f$top_supplier)
  expect_null(f$coercive_top)
})

test_that("deployment trends and targets read as words", {
  expect_equal(country_brief_trend_text("19.8 -> 41.1 GW (2020-2025)"), "19.8 to 41.1 GW (2020-2025)")
  expect_equal(country_brief_trend_text("oil use 1.88 -> 2.24 EJ (2020-2025)"), "1.88 to 2.24 EJ (2020-2025)")
  expect_equal(country_brief_trend_text("mean 116000 EV car sales a year (2023-2025)"), "an average of 116,000 EV car sales a year (2023-2025)")
  expect_equal(country_brief_targets("other_note: Solar target 5 GW by 2030"), list())
  t <- country_brief_targets("renewables_target_shortfall: Renewables target 143.4 GW by 2030 vs 66.0 GW installed (2025), recent build 5.52 GW/yr -> 93.6 GW projected; shortfall 49.8 GW")
  expect_equal(t$target_gw$d, "143 GW")
  expect_equal(t$installed_gw$d, "66 GW")
  expect_equal(t$shortfall_gw$d, "50 GW")
  expect_equal(t$target_year$d, "2030")
  expect_equal(country_brief_targets(NULL), list())
})

test_that("the structured objects follow their selection rules on the Australia bundles", {
  b <- read_bundles()
  skip_if(is.null(b), "no published posture or AIB bundle")
  tb <- country_brief_review(country_brief_tables(b$core, b$detail, "AUS"), dcfg)
  # the review cases of the pilot: the flagged mining Preserve, the thin-anchor rare-earth Catch Up, the uranium
  # artifact and the single-indicator oil Accelerate
  rv <- stats::setNames(tb$cells$review, tb$cells$key)
  expect_equal(unname(rv[c("electric-vehicles-extraction", "wind-refining", "nuclear-refining", "oil-deployment")]),
               c("review_flag", "thin_anchor", "artifact", "single_indicator"))
  expect_true(is.na(rv[["batteries-refining"]]) && is.na(rv[["electric-vehicles-refining"]]))
  # policy gaps: at most three of each kind, ten in all, never a low-confidence choice or another review case
  gp <- country_brief_policy_gaps(tb, cfg)
  expect_lte(nrow(gp), 10)
  expect_true(all(table(gp$gap) <= 3))
  expect_false("oil-deployment" %in% gp$key)
  expect_false(any(gp$key %in% names(rv)[!is.na(rv)]))
  # low-fence cases: Watch processing or manufacturing steps only, the energy system's Accelerate technologies first
  lc <- country_brief_low_cases(tb, b$core$meta$thresholds)
  expect_true(all(lc$choice == "watch") && !any(lc$stage %in% c("Extraction", "Deployment")))
  expect_false(is.unsorted(rev(as.integer(lc$deploy_accelerate))))
  expect_false(any(lc$key %in% names(rv)[!is.na(rv)]))
  # Figure 4: the review cases sit apart from the deployment priorities
  dr <- country_brief_deployment_rank(tb, cfg)
  expect_equal(dr$review[dr$key == "oil-deployment"], "single_indicator")
  # the matrix: config columns, known levels, and a supplier role that is essential with a core seat
  mx <- country_brief_aib_matrix(b$aib, b$managed, tb, "AUS", country_brief_name_fn(b$core), cfg, country_brief_division(b$core, "AUS", tb))
  expect_true(all(mx$col %in% names(cfg$aib_matrix$columns)))
  expect_true(all(mx$level %in% c("essential", "role", "relies", "gap")))
  expect_equal(mx$level[mx$compact == "SC-NUCLEAR" & mx$col == "supplier"], "essential")
  # a demand anchor's lead share is a share of world imports: it reads in the demand cell, not as supply
  a2 <- b$aib
  i <- which(vapply(a2$strategic$compactRoles, function(r) identical(r$iso3, "AUS") && identical(r$compact, "SC-NUCLEAR"), TRUE))
  a2$strategic$compactRoles[[i]]$leadBasis <- "share of world imports"
  a2$strategic$compactRoles[[i]]$functions <- list("Scale manufacturer", "Demand anchor")
  m2 <- country_brief_aib_matrix(a2, b$managed, tb, "AUS", country_brief_name_fn(b$core), cfg, country_brief_division(b$core, "AUS", tb))
  expect_false(grepl("%", m2$text[m2$compact == "SC-NUCLEAR" & m2$col == "supplier"], fixed = TRUE))
  expect_match(m2$text[m2$compact == "SC-NUCLEAR" & m2$col == "demand"], "of world imports$")
  # Figure 3's layout: each tile holds its wrapped lines, rows stack without overlap, and a longer label makes
  # the figure taller, never shorter than the configured minimum
  lo <- country_brief_matrix_layout(mx, cfg)
  line_in <- COUNTRY_BRIEF_MATRIX_TEXT$lineheight * COUNTRY_BRIEF_MATRIX_TEXT$cell * ggplot2::.pt / 72
  expect_true(all(lo$d$h >= (lengths(strsplit(lo$d$wrapped, "\n", fixed = TRUE))) * line_in))
  expect_true(all(abs(-diff(lo$rows$y) - (head(lo$rows$h, -1) + tail(lo$rows$h, -1)) / 2) < 1e-9))
  expect_gte(lo$height_in + 1e-9, cfg$charts$matrix_height_in)
  m3 <- mx
  m3$text[1] <- paste(rep("a long cell text", 8), collapse = " ")
  expect_gt(country_brief_matrix_layout(rbind(m3, transform(m3, compact = paste0(compact, "-2"), row_order = row_order + 100)), cfg)$height_in, lo$height_in)
  expect_equal(country_brief_lc1(c("AC generators", "PGMs (iridium)", "Li-ion cells & packs", "Manganese dioxide (EMD)")),
               c("AC generators", "PGMs (iridium)", "li-ion cells & packs", "manganese dioxide (EMD)"))
  # generated tables expand into the parsed brief and resolve as facts
  f <- country_brief_facts(b$core, b$detail, b$aib, b$managed, "AUS", cfg, "30 October 2026", dcfg = dcfg)
  gen <- country_brief_generated_tables(f, list(cells = tb$cells, layers = tb$layers), cfg)
  expect_equal(length(gen$strategy_table$rows), 66L)
  expect_equal(length(gen$policy_gaps$rows), nrow(gp))
  md <- paste0(paste(vapply(seq_along(cfg$brief$sections), function(i) paste0("## ", country_brief_titles(cfg, list(country = "Australia", country_poss = "Australia's"))[i], "

Text.
"), ""), collapse = "
"),
               "
## ", cfg$brief$appendix_title, "

@table strategy_table
")
  parsed <- country_brief_expand_tables(country_brief_parse(md, cfg, list(country = "Australia", country_poss = "Australia's")), gen)
  res <- country_brief_resolve(parsed, f)
  expect_equal(res$errors, character())
  expect_true(any(vapply(res$units, function(u) identical(u$kind, "gtd"), TRUE)))
})

test_that("review cases cannot reach the headlines through lists, development rows or plain text", {
  with <- function(text) sub("Testland should build what it can anchor", text, good_src(), fixed = TRUE)
  # a list or a development row that carries a review step: error; a count that includes one: warning
  expect_true("headline_review" %in% checks_of(lint_of(with("Testland should build {{lists.build}} and what it can anchor"), review_keys = "x-refining")))
  expect_true("headline_review" %in% checks_of(lint_of(with("Testland should back {{dev.rows.r1.partner}} and what it can anchor"), review_keys = "x-refining")))
  cnt <- lint_of(with("Testland should build {{counts.build|nw}} steps and what it can anchor"), review_keys = "x-refining")
  expect_false("headline_review" %in% checks_of(cnt))
  expect_true("headline_review" %in% checks_of(cnt, "warning"))
  expect_false("headline_review" %in% checks_of(lint_of(with("Testland should build {{lists.build}} and what it can anchor"), review_keys = "z-mining")))
  # a review step named in plain text
  expect_true("headline_review" %in% checks_of(lint_of(with("Testland should build widget refining and what it can anchor"), review_keys = "x-refining",
                                                       review_names = c(`x-refining` = "widget refining"))))
})

test_that("an economy that is no primary allied holder has no division rows, and its facts still build", {
  b <- read_bundles()
  skip_if(is.null(b), "no published posture or AIB bundle")
  tb <- country_brief_tables(b$core, b$detail, "AUS")
  expect_equal(nrow(country_brief_division(b$core, "ZZZ", tb)), 0L)
})

test_that("a shared capability with different choices is read from each step's own measures", {
  tb <- list(cells = data.frame(key = c("a-mining", "b-mining"), name = c("a mining", "b mining"), tech = c("A", "A"), need = c(0.5, 0.5),
                                prize = c(0.7, 0.4), cp = c(0.8, 0.8), choice = c("preserve", "watch"), stringsAsFactors = FALSE))
  row <- data.frame(node = "ore", label = "Ore", cls = "MAINTAIN", keys = "a-mining;b-mining", choices = "preserve;watch", reviews = ";",
                    contributes = FALSE, strong = TRUE, reading = "by_need", reading_choice = NA, stringsAsFactors = FALSE)
  f <- country_brief_shared_facts(row, tb, list(contributes = list(), division = list()), cfg)
  expect_match(f$reading$d, "within the same technology")
  expect_false(grepl("because supply-chain need differs", f$reading$d))
  expect_equal(as.character(f$steps$k), c("a-mining", "b-mining"))
})
