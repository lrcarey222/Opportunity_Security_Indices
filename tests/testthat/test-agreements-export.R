root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
for (f in c("agr_schema.R", "agr_text_coding.R", "agr_link.R", "agr_pairs.R", "agr_export.R")) {
  source(file.path(root, "R", "agreements", f))
}
cfg <- yaml::read_yaml(file.path(root, "config", "partnership_agreements.yml"))
eu <- c("DEU", "FRA")
js <- function(x) jsonlite::fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE, na = "null", null = "null", digits = NA),
                                     simplifyVector = FALSE)

mk <- function(id, ds, parties, type, date, url = "") {
  df <- data.frame(source_dataset = ds, source_record_id = id, title = paste("Agreement", id), agreement_type = type,
                   signature_date = date, implementation_status = "signed", legal_status = "non_binding",
                   source_url = url, stringsAsFactors = FALSE)
  pf <- agr_party_fields(list(strsplit(parties, "|", fixed = TRUE)[[1]]))
  df[names(pf)] <- pf
  agr_as_records(df)
}
rec <- rbind(mk("curated:eu-chl", "curated", "CHL|EUU", "framework", "2025-01-15", "https://commission.europa.eu/x"),
             mk("curated:deu-chl", "curated", "CHL|DEU", "mou", "2024-03-01"),
             mk("pmemo:usa-jpn", "pmemo", "JPN|USA", "framework", "2025-10-28"))
codes <- agr_as_codes(data.frame(source_record_id = c("curated:eu-chl", "curated:eu-chl", "pmemo:usa-jpn"),
                                 dimension = c("sector", "technology", "sector"),
                                 code = c("critical_minerals", "Batteries", "nuclear"),
                                 basis = "curated", stringsAsFactors = FALSE))
L <- agr_link_records(rec, codes, cfg)
flags <- agr_agreement_flags(L$canonical, L$canonical_codes, cfg, "2026-11-06")
P <- agr_pair_profiles(L$canonical, L$canonical_codes, cfg, eu, "2026-11-06")
delivery <- data.frame(canonical_agreement_id = L$links$canonical_agreement_id[L$links$source_record_id == "pmemo:usa-jpn"],
                       text = "first project", status = "announced", url = "", stringsAsFactors = FALSE)
objs <- agr_export_agreements(L$canonical, L$canonical_codes, L$links, flags, delivery, cfg)
cidx <- agr_country_agreement_ids(L$canonical, eu)
gaps <- data.frame(need_iso3 = c("DEU", "DEU", "DEU", "CHN"), supplier_iso3 = c("CHL", "AUS", "CHN", "DEU"),
                   tech = "Batteries", need = c(0.9, 0.9, 0.9, NA), capability = c(0.8, 0.95, 0.99, 0.5),
                   recommended = c(0L, 1L, 0L, 0L), friendshore = c(0.5, NA, 0.2, 0.1), aib_shared = c("SC-BATTERIES", "", "", ""),
                   complementarity = c("high", "high", "high", "low"), covered_grade = c("specific", "none", "none", "none"),
                   architecture = c("strong", "weak", "weak", "weak"),
                   quadrant = c("established", "opportunity", "opportunity", "none"), stringsAsFactors = FALSE)
exec <- data.frame(iso3 = "DEU", tech = "Solar", priority_basis = "choice:partner", priority = NA_real_,
                   suppliers_without_coverage = "CHN", stringsAsFactors = FALSE)
cfg1 <- cfg; cfg1$gaps$top_candidates <- 1L

test_that("EU member states get the EU's agreements via_eu", {
  expect_true(length(cidx$DEU$via_eu) == 1L)
  expect_equal(length(cidx$EUU$ids), 1L)
  x <- js(agr_export_country("DEU", cidx, objs, P, gaps, exec, cfg1))
  vi <- vapply(x$agreements, function(a) a$via_eu, logical(1))
  expect_equal(sum(vi), 1L)
  ids <- vapply(x$agreements, function(a) a$id, "")
  for (p in x$partners) expect_true(all(unlist(p$agreement_ids) %in% ids))
})

test_that("one-item arrays stay arrays and empty strings become null", {
  x <- js(agr_export_country("DEU", cidx, objs, P, gaps, exec, cfg1))
  eu_a <- Filter(function(a) a$via_eu, x$agreements)[[1]]
  expect_true(is.list(eu_a$techs) && length(eu_a$techs) == 1L)       # ["Batteries"], not "Batteries"
  expect_true(is.list(eu_a$sectors[[1]]) && length(eu_a$sectors[[1]]) == 2L)
  expect_true(is.list(eu_a$sources) && length(eu_a$sources) == 1L)
  expect_null(eu_a$label)
  expect_null(eu_a$trade_depth_index)
  p <- Filter(function(r) r$partner == "CHL", x$partners)[[1]]
  expect_true(is.list(p$sources))
  expect_true(is.list(p$inst) && all(names(cfg$instrument_groups) %in% names(p$inst)))
  expect_true(is.list(x$execution_gaps[[1]]$suppliers_without_coverage))
  expect_null(x$execution_gaps[[1]]$priority)
  j <- js(agr_export_country("JPN", cidx, objs, P, gaps[0, ], exec[0, ], cfg))
  expect_equal(j$agreements[[1]]$delivery[[1]]$text, "first project")
  expect_null(j$agreements[[1]]$delivery[[1]]$url)
  expect_equal(length(j$gaps), 0L)
})

test_that("gap rows are trimmed to top candidates per technology and side, plus covered partners", {
  tg <- agr_export_trim_gaps(gaps, "DEU", 1L)
  expect_setequal(paste(tg$side, tg$partner), c("needs AUS", "needs CHL", "supplies CHN"))
  x <- js(agr_export_country("DEU", cidx, objs, P, gaps, exec, cfg1))
  expect_true(all(vapply(x$gaps, function(g) g$side %in% c("needs", "supplies"), logical(1))))
  chl <- Filter(function(g) g$partner == "CHL", x$gaps)[[1]]
  expect_true(is.list(chl$aib_shared))
})

test_that("the global file is columnar with equal lengths and the vocab is complete", {
  gl <- js(agr_export_global(P, cidx, objs))
  n <- unique(vapply(gl$pairs, length, 1L))
  expect_equal(n, nrow(P))
  expect_equal(unique(vapply(gl$countries, length, 1L)), length(cidx))
  expect_true(all(c("a", "b", "n", "n_recent", "newest", "depth_all", "depth_ei", "has_sp", "has_trade", "has_sectoral",
                    "n_delivery", "ei_sectors", "types", "sources") %in% names(gl$pairs)))
  deu <- match("DEU", unlist(gl$countries$iso3))
  expect_equal(gl$countries$n_agreements[[deu]], 2L)
  v <- js(agr_export_vocab(cfg, c(curated = 2L, pmemo = 1L), c(curated = "2026-11-06")))
  expect_equal(sort(names(v$depth_classes)), as.character(0:5))
  expect_true(all(vapply(v$sectors, function(s) is.list(s$techs), logical(1))))
  src <- Filter(function(s) s$id == "gsp", v$sources)[[1]]
  expect_equal(src$n_records, 0L)
  expect_null(src$last_updated)
  expect_true(all(unlist(v$delivery_statuses) %in% names(v$implementation_statuses)))
})
