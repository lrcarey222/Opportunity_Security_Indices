# The policy-memo pipeline (spec_country.md part B; R/dashboard/country/country_memo*.R) on the
# synthetic United States sheet: a well-formed memo source parses, resolves and lints clean (word
# bounds relaxed: the fixture memo is short); each rule of B6.1 fires on a broken copy (a stray digit, a
# number word, an unknown token, a token on a plain number, an outside economy name, a plain corner word
# beside a sector token, a causal word beside a non-top driver or a trade share, "lowest" on a sector
# that is not first in es_order, a missing coverage family, a triggered caveat absent from section 5, a
# structure error); the claims carry meanings, sources and context; the verifier's record must cover
# every claim and sentence with matching hashes; an unchanged claims file keeps the sign-off on a new
# fact sheet while a moved order position does not (B6.5); the document, its print HTML (no external
# reference) and its Markdown carry the verbatim label.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "dashboard", "country", "country_files.R"))
for (f in country_helper_files()) source(file.path(repo_root, f))
cfg <- yaml::read_yaml(file.path(repo_root, "config", "country_insights.yml"))
dict <- yaml::read_yaml(file.path(repo_root, "config", "sector_dictionary.yml"))
ctx <- country_synthetic_ctx(cfg, dict)
ctx$meta$created <- "2026-10-12T00:00:00Z"
roster_names <- ctx$roster$name
sheet <- jsonlite::fromJSON(as.character(country_json(country_fact_sheet("USA", ctx))), simplifyVector = FALSE)
cfg_short <- cfg
cfg_short$memo$words_total <- list(0, 99999)
cfg_short$memo$words_summary <- list(0, 99999)
cfg_short$memo$words_sections_hard <- rep(list(list(0, 99999)), 5)
cfg_short$memo$words_sections_target <- rep(list(list(0, 99999)), 5)

cells <- stats::setNames(sheet$cells, vapply(sheet$cells, function(c) c$key, ""))
es_o <- unlist(sheet$summary$es_order)
eo_o <- unlist(sheet$summary$eo_order)
si_o <- unlist(sheet$summary$si_order)
psi_o <- unlist(sheet$partners$psi_order)
rows <- stats::setNames(sheet$partners$rows, vapply(sheet$partners$rows, function(r) r$iso3, ""))
top_es <- function(k) Filter(function(d) isTRUE(d$top), cells[[k]]$drivers$es)
top_eo <- function(k) Filter(function(d) isTRUE(d$top), cells[[k]]$drivers$eo)
k1 <- es_o[1]; k2 <- es_o[2]; k3 <- es_o[3]
e1 <- eo_o[1]; e2 <- eo_o[2]
d_es <- top_es(k1)[[1]]$key
d_eo <- top_eo(e1)[[1]]$key
non_top <- Filter(function(d) !isTRUE(d$top), cells[[k1]]$drivers$es)
raw_tok <- NULL
for (k in es_o) for (d in top_es(k)) for (v in d$vars) if (is.null(raw_tok) && !is.null(v$raw$v)) raw_tok <- sprintf("cells.%s.drivers.es.%s.vars.%s.raw", k, d$key, v$key)
p_tie <- psi_o[vapply(psi_o, function(j) !is.null(rows[[j]]$ties$tie_rank$v), TRUE)][1]
cls <- names(Filter(function(x) length(x) > 0, sheet$policy$agreement$by_class))[1]
trade_k <- names(Filter(function(c) length(c$trade$sources) > 0, cells))[1]

good_src <- function(caveats = vapply(sheet$caveats, function(c) c$id, "")) paste0(
  ":::summary\n",
  "- {{summary.n_insecure}} of {{meta.country}}'s {{summary.n_placed}} placed sectors sit in the two low-energy-security corners; its lowest Energy Security is in {{cells.", k1, ".label_prose}}.\n",
  "- Its highest Economic Opportunity is in {{cells.", e1, ".label_prose}}, higher than {{cells.", e1, ".eo.pct}} of the other economies scored there.\n",
  "- Its top Strategic Index sector among its own sectors is {{cells.", si_o[1], ".label_prose}}.\n",
  "- The Partnership Strength Index rates {{partners.", psi_o[1], ".name}} highest for {{meta.country}}.\n",
  ":::\n\n",
  "## Energy security risks and why\n\n",
  "{{summary.n_insecure}} of its {{summary.n_placed}} placed sectors sit in the two low-energy-security corners. ",
  "Its lowest Energy Security scores are in {{cells.", k1, ".label_prose}} and {{cells.", k2, ".label_prose}}, higher than {{cells.", k1, ".es.pct}} and {{cells.", k2, ".es.pct}} of the other economies scored. ",
  "On this split, {{cells.", k1, ".label_prose}} sits in the {{cells.", k1, ".legacy_corner}} corner. ",
  "The score in {{cells.", k1, ".label_prose}} is low mainly because of {{cells.", k1, ".drivers.es.", d_es, ".category}}.",
  if (!is.null(raw_tok)) paste0(" A figure behind the scores: {{", raw_tok, "}} in {{", raw_tok, ".year}}.") else "", "\n\n",
  "## Economic opportunities and why\n\n",
  "{{summary.n_high_eo}} of its placed sectors sit in the two high-opportunity corners. ",
  "Its highest Economic Opportunity scores are in {{cells.", e1, ".label_prose}} and {{cells.", e2, ".label_prose}}, higher than {{cells.", e1, ".eo.pct}} and {{cells.", e2, ".eo.pct}} of the other economies scored. ",
  "Its {{cells.", e1, ".drivers.eo.", d_eo, ".category}} score there is {{cells.", e1, ".drivers.eo.", d_eo, ".score}}.\n\n",
  "## Sectoral priorities and the existing policy posture\n\n",
  "Among its own sectors, the Strategic Index ranks {{cells.", si_o[1], ".label_prose}} {{cells.", si_o[1], ".si.rank}}, {{cells.", si_o[2], ".label_prose}} {{cells.", si_o[2], ".si.rank}} and {{cells.", si_o[3], ".label_prose}} {{cells.", si_o[3], ".si.rank}}. ",
  "The sectors whose priority and policy activity compare as {{policy.agreement.by_class.", cls, "|n}} are {{policy.agreement.by_class.", cls, "|list}}. ",
  "The Climate Action Tracker rates {{meta.country}} {{policy.cat.overall}}, and it has {{policy.pams_total}} IEA Policies and Measures records.\n\n",
  "## Partnership priorities and why\n\n",
  "The Partnership Strength Index rates {{partners.", psi_o[1], ".name}}, {{partners.", psi_o[2], ".name}} and {{partners.", psi_o[3], ".name}} {{partners.", psi_o[1], ".rank}}, {{partners.", psi_o[2], ".rank}} and {{partners.", psi_o[3], ".rank}} for {{meta.country}}. ",
  "{{partners.", psi_o[1], ".name}} is a {{partners.", psi_o[1], ".reason}} partner for {{meta.country}}. ",
  "{{partners.", p_tie, ".name}} is its {{partners.", p_tie, ".ties.tie_rank}} trade partner on average across its energy-goods baskets. ",
  "As a partner to the coalition, {{meta.country}} ranks {{summary.as_partner.rank}}. Its allied pick is {{pick.cell}}.\n\n",
  "## Data notes and caveats\n\n",
  "All values are the NEIS baseline as of {{meta.as_of_label}}. ", paste(sprintf("{{caveats.%s}}", caveats), collapse = " "), "\n"
)

run <- function(src, sh = sheet, cf = cfg_short) {
  parsed <- country_memo_parse(src, cf)
  res <- country_memo_resolve(parsed, sh, cf)
  sents <- country_memo_sentences(res)
  claims <- country_memo_claims(res, sents, sh, cf)
  lint <- country_memo_lint(parsed, res, sents, claims, sh, cf, roster_names)
  list(parsed = parsed, res = res, sents = sents, claims = claims, lint = lint)
}
errors_of <- function(r, check = NULL) {
  e <- Filter(function(x) x$severity == "error" && (is.null(check) || x$check == check), r$lint$items)
  vapply(e, function(x) x$detail, "")
}

test_that("a well-formed memo parses, resolves and lints without errors", {
  r <- run(good_src())
  expect_equal(r$parsed$errors, character())
  expect_equal(errors_of(r), character())
  expect_length(r$parsed$summary, 4)
  expect_equal(vapply(r$parsed$sections, function(s) s$title, ""), unlist(cfg$memo$sections))
  expect_true(all(c("relative_scores", "baseline") %in% r$lint$triggered))
  # every claim has a meaning; sector and partner claims carry their context
  expect_true(all(vapply(r$claims, function(c) !is.null(c$meaning), TRUE)))
  c1 <- Filter(function(c) c$path == sprintf("cells.%s.es.pct", k1), r$claims)[[1]]
  expect_equal(c1$context$cell$es_order, 1L)
  expect_equal(c1$source, "osi_indices")
  expect_match(c1$meaning, "share of the other .* economies scored in .* whose Energy Security is lower than United States's")
  # a |list token gives one claim per element
  n_cls <- length(sheet$policy$agreement$by_class[[cls]])
  expect_equal(sum(vapply(r$claims, function(c) identical(c$path, paste0("policy.agreement.by_class.", cls)) && identical(c$filter, "list"), TRUE)), n_cls)
})

test_that("each lint rule fires on a broken copy", {
  g <- good_src()
  sub1 <- function(a, b) sub(a, b, g, fixed = TRUE)
  expect_true(length(errors_of(run(sub1("All values are", "In 2024 all values are")), "digit")) > 0)
  expect_true(length(errors_of(run(sub1("All values are", "Two kinds of data: all values are")), "number_word")) > 0)
  expect_true(any(grepl("does not resolve", errors_of(run(sub1("{{policy.pams_total}}", "{{policy.pams_totally}}")), "token"))))
  expect_true(any(grepl("plain number", errors_of(run(sub1(sprintf("{{cells.%s.es.pct}} and", k1), sprintf("{{cells.%s.es.pos}} and", k1))), "token"))))
  expect_true(length(errors_of(run(sub1("All values are", "Unlike China, all values are")), "economy_name")) > 0)
  expect_true(length(errors_of(run(sub1(sprintf("sits in the {{cells.%s.legacy_corner}} corner", k1), sprintf("sits in the Exposed corner beside {{cells.%s.es}}", k1))), "corner_word")) > 0)
  # Secure and Watch were corner words until 2026-10-28; they are now six-stage posture words, not corners
  expect_length(errors_of(run(sub1(sprintf("sits in the {{cells.%s.legacy_corner}} corner", k1), sprintf("sits in the Secure posture beside {{cells.%s.es}}", k1))), "corner_word"), 0)
  if (length(non_top)) {
    bad <- sub1(sprintf("{{cells.%s.drivers.es.%s.category}}", k1, d_es), sprintf("{{cells.%s.drivers.es.%s.category}}", k1, non_top[[1]]$key))
    expect_true(any(grepl("not a top driver", errors_of(run(bad), "causal"))))
  }
  if (!is.na(trade_k)) {
    bad <- sub1("All values are", sprintf("Imports are high mainly because of {{cells.%s.trade.sources.0.name}} ({{cells.%s.trade.sources.0.share}}). All values are", trade_k, trade_k))
    expect_true(any(grepl("context, not a cause", errors_of(run(bad), "causal"))))
  }
  expect_true(length(errors_of(run(sub1(sprintf("its lowest Energy Security is in {{cells.%s.label_prose}}", k1), sprintf("its lowest Energy Security is in {{cells.%s.label_prose}}", k3))), "order")) > 0)
  no_cov <- sub1("{{summary.n_insecure}} of its {{summary.n_placed}} placed sectors sit in the two low-energy-security corners. ", "")
  expect_true(any(grepl("summary.n_insecure", errors_of(run(no_cov), "coverage"), fixed = TRUE)))
  no_cav <- good_src(setdiff(vapply(sheet$caveats, function(c) c$id, ""), "relative_scores"))
  expect_true(any(grepl("relative_scores", errors_of(run(no_cav), "caveat"))))
  expect_true(length(errors_of(run(sub1("## Data notes and caveats", "## Notes")), "structure")) > 0)
  expect_true(length(errors_of(run(sub1("All values are", "See [the site](https://example.org). All values are")), "structure")) > 0)
  expect_true(length(errors_of(run(sub1("All values are", "The secure sectors are fine. All values are")), "secure_label")) > 0)
  expect_true(length(errors_of(run(sub1("All values are", "The CAT agrees. All values are")), "acronym")) > 0)
  # the three stages the six replaced are never a memo's own words (2026-10-28)
  expect_true(length(errors_of(run(sub1("All values are", "Its upstream sectors lead. All values are")), "legacy_stage")) > 0)
  expect_true(length(errors_of(run(sub1("All values are", "Midstream trade is small. All values are")), "legacy_stage")) > 0)
  # the word bounds of B2 apply with the real configuration
  expect_true(length(errors_of(run(g, cf = cfg), "words")) > 0)
})

test_that("six-stage words lint clean: the six-stage phrases and the capitalised stage names (2026-10-28)", {
  g <- sub("All values are", "Across the six supply-chain stages, Extraction and Refining lead the two manufacturing stages. All values are", good_src(), fixed = TRUE)
  r <- run(g)
  expect_equal(errors_of(r), character())
  caps <- Filter(function(x) identical(x$check, "capitalised"), r$lint$items)
  expect_false(any(grepl("Extraction|Refining", vapply(caps, function(x) x$detail, ""))))
  # a bare number word is still an error
  expect_true(length(errors_of(run(sub("All values are", "Six sectors lead. All values are", good_src(), fixed = TRUE)), "number_word")) > 0)
  # the sector tokens print the six-stage names
  expect_match(cells[[k1]]$label, paste0(" — (", paste(SUPPLY_CHAIN_STAGES, collapse = "|"), ")$"))
  expect_false(grepl("stream", cells[[k1]]$label_prose, ignore.case = TRUE))
})

test_that("the verifier's record must cover every claim and sentence with matching hashes; unchanged claims keep the sign-off", {
  skip_if_not(!is.na(country_sha256("x")), "no sha256 (digest or openssl)")
  r <- run(good_src())
  cd <- country_memo_claims_doc("USA", r$claims, r$sents, r$lint$triggered)
  csha <- country_sha256(as.character(country_json(cd, pretty = TRUE)))
  src_sha <- country_sha256(good_src())
  verify <- list(status = "pass", src_sha256 = src_sha, facts_sha256 = sheet$checks$facts_sha256, claims_sha256 = csha,
                 claims_checked = lapply(cd$claims, function(c) list(id = c$id, verdict = "ok")),
                 sentences_checked = lapply(cd$sentences, function(s) list(id = s$id, verdict = "ok")),
                 warnings_cleared = lapply(Filter(function(x) x$severity == "warning", r$lint$items), function(x) list(lint_id = x$id, note = "ok")),
                 issues = list())
  ok <- country_memo_verify_check(verify, cd, r$lint, src_sha, sheet$checks$facts_sha256, csha)
  expect_true(ok$ok)
  expect_false(ok$reuse)
  v2 <- verify; v2$claims_checked <- v2$claims_checked[-1]
  expect_false(country_memo_verify_check(v2, cd, r$lint, src_sha, sheet$checks$facts_sha256, csha)$ok)
  v3 <- verify; v3$claims_sha256 <- "0000"
  expect_false(country_memo_verify_check(v3, cd, r$lint, src_sha, sheet$checks$facts_sha256, csha)$ok)
  # a new fact sheet whose claims are unchanged (here only the build time differs): the sign-off is reused
  s2 <- sheet
  s2$meta$created <- "2026-10-13T00:00:00Z"
  s2$checks$facts_sha256 <- country_sha256(as.character(country_json(s2[setdiff(names(s2), "checks")])))
  r2 <- run(good_src(), sh = s2)
  cd2 <- country_memo_claims_doc("USA", r2$claims, r2$sents, r2$lint$triggered)
  csha2 <- country_sha256(as.character(country_json(cd2, pretty = TRUE)))
  expect_identical(csha2, csha)
  re <- country_memo_verify_check(verify, cd2, r2$lint, src_sha, s2$checks$facts_sha256, csha2)
  expect_true(re$ok)
  expect_true(re$reuse)
  # a moved order position changes the claims: the memo is stale
  s3 <- sheet
  s3$summary$es_order <- as.list(c(es_o[2], es_o[1], es_o[-(1:2)]))
  r3 <- run(good_src(), sh = s3)
  cd3 <- country_memo_claims_doc("USA", r3$claims, r3$sents, r3$lint$triggered)
  csha3 <- country_sha256(as.character(country_json(cd3, pretty = TRUE)))
  expect_false(identical(csha3, csha))
  expect_false(country_memo_verify_check(verify, cd3, r3$lint, src_sha, s3$checks$facts_sha256, csha3)$ok)
})

test_that("the document, print HTML and Markdown carry the label; sources are numbered in order of use", {
  r <- run(good_src())
  doc <- country_memo_document(r$res, r$claims, sheet, cfg,
                               list(generated_at = "2026-10-12T00:00:00Z", verified_at = "2026-10-12T00:00:00Z", writer = "w", verifier = "v", src_sha256 = "s", claims_sha256 = "c"))
  expect_equal(doc$label, "AI-generated from the data and checked against it; NEIS baseline as of 12 October 2026.")
  expect_equal(doc$title, "United States: energy security, economic opportunity and partnership priorities")
  expect_equal(vapply(doc$sections, function(s) s$title, ""), unlist(cfg$memo$sections))
  expect_equal(vapply(doc$sources, function(s) s$n, 1), seq_along(doc$sources))
  refs <- unlist(lapply(doc$summary, function(it) unlist(it$refs)))
  expect_true(all(refs %in% seq_along(doc$sources)))
  expect_equal(doc$words, r$lint$words$total)
  vals <- unlist(lapply(doc$summary, function(it) vapply(Filter(function(n) n$t == "value", it$inlines), function(n) n$path, "")))
  expect_true(length(vals) >= 4)
  html <- country_memo_html(doc, NULL)
  expect_equal(country_memo_html_external(html), character())
  expect_match(html, doc$label, fixed = TRUE)
  md <- country_memo_markdown(doc)
  expect_match(md, paste0("# ", doc$title), fixed = TRUE)
  tx <- country_memo_texts(doc)
  expect_equal(tx[1:3], c(doc$title, doc$subtitle, doc$label))
  expect_equal(utils::tail(tx, 1), doc$label)
})

test_that("the capitalised-word check also runs when a sentence starts with an allowlisted word", {
  warn_caps <- function(r) vapply(Filter(function(x) x$check == "capitalised", r$lint$items), function(x) x$detail, "")
  g <- good_src()
  ins <- function(txt) sub("All values are", paste(txt, "All values are"), g, fixed = TRUE)
  # "The" is allowlisted (and masked): before the fix caps[-match(...)] dropped every word of the sentence
  expect_true(any(grepl("Zebra", warn_caps(run(ins("The Zebra index agrees."))))))
  expect_true(any(grepl("Zebra", warn_caps(run(ins("Zebra indices agree with the Zebra index."))))))  # the second one is not sentence-initial
  expect_false(any(grepl("Zebra", warn_caps(run(ins("Zebra indices agree."))))))                     # sentence-initial only
  expect_length(warn_caps(run(ins("Global Trade Alert lists industrial-policy measures."))), 0)
  expect_length(warn_caps(run(ins("For context, Global Trade Alert's New Industrial Policy Observatory lists measures."))), 0)
})

test_that("the Policy index basis prints in words; a cited EU regional figure triggers its caveat", {
  idx <- country_memo_index(sheet)
  r <- country_memo_resolve_token(idx, "meta.policy_basis")
  expect_true(r$ok)
  expect_equal(r$text, cfg$words$policy_basis[[sheet$meta$policy_basis]])
  expect_false(grepl("IEA|PAMS|CAT", r$text))
  skip_if(is.null(raw_tok), "no raw figure behind a top Energy Security driver in the fixture")
  s2 <- sheet
  pm <- regmatches(raw_tok, regexec("^cells\\.([^.]+)\\.drivers\\.es\\.([^.]+)\\.vars\\.([^.]+)\\.raw$", raw_tok))[[1]]
  ci <- which(vapply(s2$cells, function(c) c$key == pm[2], TRUE))
  di <- which(vapply(s2$cells[[ci]]$drivers$es, function(d) d$key == pm[3], TRUE))
  vi <- which(vapply(s2$cells[[ci]]$drivers$es[[di]]$vars, function(v) v$key == pm[4], TRUE))
  s2$cells[[ci]]$drivers$es[[di]]$vars[[vi]]$raw$note <- "eu_region_share"
  r1 <- run(good_src())
  r2 <- run(good_src(), sh = s2)
  expect_false("eu_region_share" %in% country_memo_triggers(r1$claims, sheet, cfg))
  expect_true("eu_region_share" %in% country_memo_triggers(r2$claims, s2, cfg))
  # the driver's category alone (no variable cited) also carries it
  src3 <- sub(paste0(" A figure behind the scores: {{", raw_tok, "}} in {{", raw_tok, ".year}}."), "", good_src(), fixed = TRUE)
  src3 <- sub("## Economic opportunities", paste0("Its {{cells.", pm[2], ".drivers.es.", pm[3], ".category}} score is {{cells.", pm[2], ".drivers.es.", pm[3], ".score}}.\n\n## Economic opportunities"), src3, fixed = TRUE)
  r3 <- run(src3, sh = s2)
  expect_true("eu_region_share" %in% country_memo_triggers(r3$claims, s2, cfg))
})

test_that("the source list never nests brackets in an edition (2026-10-14)", {
  expect_equal(country_memo_source_edition("Energy Institute, Statistical Review of World Energy 2026", "2026 (data to 2025)"),
               "Energy Institute, Statistical Review of World Energy 2026 (data to 2025)")
  expect_equal(country_memo_source_edition("Voting Data, ideal points (Harvard Dataverse V39.0)", "V39.0 (sessions to 2023)"),
               "Voting Data, ideal points (Harvard Dataverse V39.0) (sessions to 2023)")
  expect_equal(country_memo_source_edition("Global Trade Alert, New Industrial Policy Observatory", "October 2026 extract (as of 5 October 2026)"),
               "Global Trade Alert, New Industrial Policy Observatory, October 2026 extract (as of 5 October 2026)")
  expect_equal(country_memo_source_edition("Atlas", "data to 2023"), "Atlas (data to 2023)")
  expect_equal(country_memo_source_edition("Atlas", NULL), "Atlas")
  r <- run(good_src())
  meta <- list(generated_at = "x", verified_at = "x", writer = "w", verifier = "v", src_sha256 = "s", claims_sha256 = "c")
  doc <- country_memo_document(r$res, r$claims, sheet, cfg, meta)
  expect_false(any(grepl("[(][^()]*[(]", vapply(doc$sources, function(s) s$text, ""))))
})

test_that("a cited figure with the eia_production note also credits the U.S. Energy Information Administration", {
  skip_if(is.null(raw_tok), "no raw figure behind a top Energy Security driver in the fixture")
  expect_equal(cfg$raw_note_sources$eia_production, "eia_intl")
  pm <- regmatches(raw_tok, regexec("^cells[.]([^.]+)[.]drivers[.]es[.]([^.]+)[.]vars[.]([^.]+)[.]raw$", raw_tok))[[1]]
  s2 <- sheet
  ci <-which(vapply(s2$cells, function(c) c$key == pm[2], TRUE))
  di <- which(vapply(s2$cells[[ci]]$drivers$es, function(d) d$key == pm[3], TRUE))
  vi <- which(vapply(s2$cells[[ci]]$drivers$es[[di]]$vars, function(v) v$key == pm[4], TRUE))
  s2$cells[[ci]]$drivers$es[[di]]$vars[[vi]]$raw$note <- "eia_production"
  s2$sources <- c(s2$sources, list(list(id = "eia_intl", label = "U.S. Energy Information Administration, International Energy Statistics", edition = NULL, licence = NULL, url = NULL)))
  meta <- list(generated_at = "x", verified_at = "x", writer = "w", verifier = "v", src_sha256 = "s", claims_sha256 = "c")
  ids <- function(d) vapply(d$sources, function(s) s$id, "")
  r1 <- run(good_src())
  expect_false("eia_intl" %in% ids(country_memo_document(r1$res, r1$claims, s2, cfg, meta)))    # no claim carries the note
  r2 <- run(good_src(), sh = s2)
  doc <- country_memo_document(r2$res, r2$claims, s2, cfg, meta)
  expect_true("eia_intl" %in% ids(doc))
  n_eia <- doc$sources[[match("eia_intl", ids(doc))]]$n
  refs <- unlist(lapply(doc$sections, function(s) lapply(s$blocks, function(b) if (identical(b$type, "ul")) lapply(b$items, function(i) unlist(i$refs)) else unlist(b$refs))))
  expect_true(n_eia %in% refs)
  # the claims themselves are unchanged by the extra credit (the value's own source stays)
  cl <- Filter(function(c) identical(c$path, raw_tok), r2$claims)[[1]]
  expect_false(identical(cl$source, "eia_intl"))
})

# ---- Memos kept under an earlier quadrant rule (the user's decision of 2026-10-15) ---------------------------

test_that("a memo written under the earlier quadrant rule stays published with a note when only quadrant claims changed", {
  rt <- function(x) jsonlite::fromJSON(as.character(country_json(x)), simplifyVector = FALSE)
  ctx_old <- ctx
  ctx_old$quadrant_rule <- "sector_median"
  old <- rt(country_fact_sheet("USA", ctx_old))
  src <- good_src()
  # the memo's claims.json as written on the earlier-rule sheet (token ids dropped, JSON round trip)
  stored <- rt(list(claims = lapply(country_memo_reclaim(src, old, cfg_short), function(c) { c$token <- NULL; c })))$claims
  moved <- country_quadrants_moved(sheet, old)
  expect_true(length(moved) > 0)
  st <- country_memo_rule_status(stored, country_memo_reclaim(src, sheet, cfg_short), country_memo_reclaim(src, old, cfg_short), moved)
  expect_equal(st$status, "rule_note")
  expect_length(st$other, 0)
  expect_true(length(st$changed) > 0)
  expect_true(all(grepl("legacy_corner|n_insecure|n_high_eo", st$changed)), info = paste(st$changed, collapse = ", "))
  # the same memo on the sheet it was written against: nothing changed, no note
  expect_equal(country_memo_rule_status(stored, country_memo_reclaim(src, old, cfg_short), country_memo_reclaim(src, old, cfg_short))$status, "current")
  # the note's words
  w <- country_memo_rule_note_words(cfg, COUNTRY_MEMO_RULE_NOTE, "United States", "2026-10-14T18:00:00Z")
  expect_match(w, "^Written on 14 October 2026, when the Grow, Upgrade, Secure and Watch corners split each sector at the median economy")
  expect_match(w, "the United States' own median sector", fixed = TRUE)
})

test_that("a memo with a changed claim that the quadrant rule does not explain still goes stale", {
  rt <- function(x) jsonlite::fromJSON(as.character(country_json(x)), simplifyVector = FALSE)
  ctx_old <- ctx
  ctx_old$quadrant_rule <- "sector_median"
  old <- rt(country_fact_sheet("USA", ctx_old))
  src <- good_src()
  stored <- rt(list(claims = lapply(country_memo_reclaim(src, old, cfg_short), function(c) { c$token <- NULL; c })))$claims
  # new data: an Energy Security percentile the memo cites moved (in both the new sheet and its earlier-rule copy)
  bump <- function(s) {
    i <- which(vapply(s$cells, function(c) c$key == k1, TRUE))
    s$cells[[i]]$es$pct <- list(v = 1, d = "1%")
    s
  }
  st <- country_memo_rule_status(stored, country_memo_reclaim(src, bump(sheet), cfg_short), country_memo_reclaim(src, bump(old), cfg_short),
                                 country_quadrants_moved(sheet, old))
  expect_equal(st$status, "stale")
  expect_true(paste0("cells.", k1, ".es.pct") %in% st$other)
  # names on a re-resolved display string (a one-element agreement list) are not a change
  named <- list(list(path = "partners.FRA.ties.agreements", filter = "list", element = "NATO", v = NULL, d = c(NATO = "NATO"), sentence_id = "s4.7"))
  plain <- list(list(path = "partners.FRA.ties.agreements", filter = "list", element = "NATO", v = NULL, d = "NATO", sentence_id = "s4.7"))
  expect_length(country_memo_claim_diff(plain, named), 0)
  expect_equal(country_memo_claim_diff(plain, list(utils::modifyList(plain[[1]], list(d = "NATO and the EU")))), "partners.FRA.ties.agreements")
  # a token that no longer resolves changes the structure: stale
  st2 <- country_memo_rule_status(stored[-1], country_memo_reclaim(src, sheet, cfg_short), country_memo_reclaim(src, old, cfg_short))
  expect_equal(st2$status, "stale")
})
