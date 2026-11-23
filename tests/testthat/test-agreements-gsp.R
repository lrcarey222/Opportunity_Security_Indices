# Global Strategic Partnership Dataset (GSP): the adapter (R/agreements/adapter_gsp.R) and the
# fetcher's offline helpers (scripts/fetchers/gsp_partnerships.R). Synthetic rows only.

root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(root, "R", "agreements", "agr_schema.R"))
source(file.path(root, "R", "agreements", "agr_text_coding.R"))
source(file.path(root, "R", "agreements", "adapter_gsp.R"))
cfg <- yaml::read_yaml(file.path(root, "config", "partnership_agreements.yml"))

gsp_row <- function(doc_id, c1, c2, type = "Joint Statement", title = "Joint Statement", fulltext = "",
                    date = "2015-03-02", year = substr(date, 1, 4), month = "", day = "",
                    source = "https://www.mofa.go.jp/a_o/page1.html") {
  data.frame(doc_id = doc_id, country1_cown = "", country1_iso3c = c1, country2_cown = "", country2_iso3c = c2,
             year = year, month = month, day = day, date = date, title = title, fulltext = fulltext,
             confidence_level1 = "TRUE", confidence_level2 = "TRUE", source = source,
             title_sp = "TRUE", factsheet_sp = "FALSE", type = type, stringsAsFactors = FALSE)
}

nm <- c(JPN = "Japan", AUS = "Australia", USA = "United States", IND = "India", CHN = "China",
        RUS = "Russia", ALB = "Albania", XKX = "Kosovo", PLW = "Palau", FRA = "France")

adapt <- function(raw, ...) agr_adapt_gsp(raw, cfg, fetched_on = "2026-11-06", country_names = nm, ...)
rec_of <- function(res, id) res$records[res$records$source_record_id == id, , drop = FALSE]

test_that("source types map to agreement type, legal status, status and date field", {
  raw <- rbind(
    gsp_row("JPNAUS201431", "JPN", "AUS", type = "Treaty", title = "Agreement between Japan and Australia"),
    gsp_row("JPNAUS201432", "JPN", "AUS", type = "Joint Statement", title = "Joint Statement"),
    gsp_row("JPNAUS201433", "JPN", "AUS", type = "Joint Statement", title = "Joint Declaration on Security Cooperation"),
    gsp_row("JPNAUS201434", "JPN", "AUS", type = "Action Plan", title = "Action Plan"),
    gsp_row("JPNAUS201435", "JPN", "AUS", type = "Joint Press Conference", title = "Press conference"),
    gsp_row("JPNAUS201436", "JPN", "AUS", type = "News", title = "Leaders meet"),
    gsp_row("JPNAUS201437", "JPN", "AUS", type = "Factsheet", title = "Fact sheet"),
    gsp_row("JPNAUS201438", "JPN", "AUS", type = "", title = "Untitled")
  )
  res <- adapt(raw)
  r <- res$records[match(paste0("gsp:JPNAUS20143", 1:8), res$records$source_record_id), ]
  expect_equal(r$agreement_type, c("treaty", "joint_statement", "joint_declaration", "framework", "other", "other", "other", "other"))
  expect_equal(r$agreement_type_raw, c("Treaty", "Joint Statement", "Joint Statement", "Action Plan", "Joint Press Conference", "News", "Factsheet", ""))
  expect_equal(r$legal_status, c("binding", rep("non_binding", 4), rep("unknown", 3)))
  expect_equal(r$legal_status_basis, c(rep("source_document_type", 5), rep("none", 3)))
  expect_equal(r$implementation_status, c("signed", rep("announced", 7)))
  expect_equal(r$implementation_status_raw, r$agreement_type_raw)
  expect_equal(r$signature_date, c("2015-03-02", rep("", 7)))
  expect_equal(r$announcement_date, c("", rep("2015-03-02", 7)))
  expect_true(all(r$primary_date == "2015-03-02"))
  expect_true(all(r$relationship_category == "overall_relationship"))
  expect_equal(r$parties[1], "AUS|JPN")
  expect_equal(r$description[c(1, 8)], c("Treaty published by Japan", "Document published by Japan"))
  expect_true(all(startsWith(r$notes, "publisher: JPN")))
  # A treaty title never makes a joint statement binding, and "Joint Declaration" only matters
  # for joint statements.
  expect_equal(adapt(gsp_row("X1", "JPN", "AUS", type = "News", title = "Joint Declaration"))$records$agreement_type, "other")
  expect_equal(agr_validate(res, cfg), character())
})

test_that("Kosovo is recoded, Abkhazia, non-ISO codes and self-pairs are dropped and counted", {
  raw <- rbind(
    gsp_row("ALBKosovo2014111", "ALB", "Kosovo"),
    gsp_row("PLWKosovo201797", "PLW", "KOSOVO"),
    gsp_row("RUSGE-AB20141124", "RUS", "GE-AB"),
    gsp_row("RUSXX2014", "RUS", "XX"),
    gsp_row("FRAFRA2014", "FRA", "FRA")
  )
  res <- adapt(raw)
  expect_equal(sort(res$records$parties), c("ALB|XKX", "PLW|XKX"))
  expect_equal(res$records$description[res$records$parties == "ALB|XKX"], "Joint Statement published by Albania")
  d <- res$extras$gsp_dropped
  expect_equal(d$reason[match(c("RUSGE-AB20141124", "RUSXX2014", "FRAFRA2014"), d$doc_id)],
               c("non_state_party", "invalid_iso3", "self_pair"))
  expect_equal(agr_validate(res, cfg), character())
})

test_that("exact duplicates are dropped and repeated doc_ids get deterministic suffixes", {
  a <- gsp_row("RUSMAR2016315", "RUS", "MAR", source = "http://www.kremlin.ru/a")
  b <- gsp_row("JPNAUS2015", "JPN", "AUS", source = "https://www.mofa.go.jp/b")
  c <- gsp_row("JPNAUS2015", "JPN", "AUS", source = "https://www.mofa.go.jp/c")
  d <- gsp_row("DZA FRA2022828", "DZA", "FRA", source = "https://www.elysee.fr/d")
  res1 <- adapt(rbind(a, a, b, c, d))
  res2 <- adapt(rbind(d, c, a, b, a))
  expect_equal(sum(res1$extras$gsp_dropped$reason == "exact_duplicate"), 1L)
  expect_setequal(res1$records$source_record_id, c("gsp:RUSMAR2016315", "gsp:JPNAUS2015", "gsp:JPNAUS2015-2", "gsp:DZAFRA2022828"))
  expect_equal(res1$records$source_record_id, res2$records$source_record_id)
  expect_equal(res1$records$source_url, res2$records$source_url)
  expect_equal(rec_of(res1, "gsp:JPNAUS2015-2")$source_url, "https://www.mofa.go.jp/c")
  expect_equal(agr_validate(res1, cfg), character())
})

test_that("blank and invalid dates fall back to the year-month-day columns", {
  raw <- rbind(
    gsp_row("PLWNOR2019560", "PLW", "NOR", date = "", year = "2019", month = "5", day = "60", type = "News"),
    gsp_row("MNGTUR2004", "MNG", "TUR", date = "", year = "2004", type = "Factsheet"),
    gsp_row("JPNRUS20031", "JPN", "RUS", date = "", year = "2003", month = "1", type = "Action Plan"),
    gsp_row("AFGDNK201352", "AFG", "DNK", date = "", year = "2013", month = "5", day = "2", type = "Treaty")
  )
  r <- adapt(raw)$records
  r <- r[match(paste0("gsp:", c("PLWNOR2019560", "MNGTUR2004", "JPNRUS20031", "AFGDNK201352")), r$source_record_id), ]
  expect_equal(r$primary_date, c("2019-05", "2004", "2003-01", "2013-05-02"))
  expect_equal(r$date_precision, c("month", "year", "month", "day"))
  expect_equal(r$signature_date, c("", "", "", "2013-05-02"))
})

test_that("partnership label comes from the title first, else a body sentence about the pair", {
  raw <- rbind(
    gsp_row("A1", "JPN", "AUS", title = "Joint Statement on the Comprehensive Strategic Partnership"),
    gsp_row("A2", "JPN", "IND", title = "Joint Statement for Special Strategic and Global Partnership"),
    gsp_row("A3", "CHN", "RUS", title = "中华人民共和国和俄罗斯联邦关于进一步深化全面战略协作伙伴关系的联合声明"),
    gsp_row("A4", "AUS", "JPN", title = "Fiji-Australia Vuvale partnership"),
    gsp_row("A5", "USA", "IND", title = "Joint Statement",
            fulltext = "The two leaders agreed to elevate bilateral relations to a Strategic Partnership. They discussed trade."),
    gsp_row("A6", "USA", "IND", title = "Joint Statement",
            fulltext = "The leaders welcomed the ASEAN-India Strategic Partnership. They discussed trade."),
    gsp_row("A7", "USA", "IND", title = "Joint Statement",
            fulltext = "President Biden and Prime Minister Modi resolved to advance the U.S.-India Comprehensive Global Strategic Partnership."),
    gsp_row("A8", "RUS", "CHN", title = "ДОГОВОР О СТРАТЕГИЧЕСКОМ ПАРТНЕРСТВЕ"),
    gsp_row("A9", "JPN", "AUS", title = "Joint Statement on Comprehensive Strategic, Security and Economic Partnership")
  )
  res <- adapt(raw)
  lab <- setNames(res$records$partnership_label, sub("^gsp:", "", res$records$source_record_id))
  expect_equal(unname(lab[paste0("A", 1:9)]),
               c("comprehensive_strategic", "special_strategic", "comprehensive_strategic", "other_partnership",
                 "strategic", "", "comprehensive_strategic", "strategic", "comprehensive_strategic"))
  expect_match(rec_of(res, "gsp:A1")$notes, "partnership label from title")
  expect_match(rec_of(res, "gsp:A5")$notes, "partnership label from body \\(1 sentence\\)")
  expect_equal(rec_of(res, "gsp:A6")$notes, "publisher: USA")
  docs <- res$extras$gsp_documents
  expect_equal(docs$label_basis[match(c("gsp:A1", "gsp:A5", "gsp:A6"), docs$source_record_id)], c("title", "body", ""))
  # Body counts are exclusive and the most named tier wins.
  b <- gsp_body_partnership_label(c("x", "y"), c(
    "Both sides reaffirmed their comprehensive strategic partnership. The two countries will deepen the comprehensive strategic partnership.",
    "The two sides aim to build a strategic partnership. Both sides will upgrade bilateral ties to a comprehensive strategic partnership."))
  expect_equal(b$label, c("comprehensive_strategic", "comprehensive_strategic"))
  expect_equal(b$n_sentences, c(2L, 2L))
  expect_equal(agr_validate(res, cfg), character())
})

test_that("official URLs, archived ones included, make a primary document; wrappers are unwrapped", {
  raw <- rbind(
    gsp_row("U1", "JPN", "AUS", source = "https://web.archive.org/web/20150101000000/http://www.mofa.go.jp/region/asia-paci/australia/joint0703.html"),
    gsp_row("U2", "VNM", "JPN", source = "https://en.vietnamplus.vn/vietnam-japan-issue-joint-statement"),
    gsp_row("U3", "JPN", "IND", source = "chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://www.mofa.go.jp/files/000034408.pdf")
  )
  r <- adapt(raw)$records
  r <- r[match(c("gsp:U1", "gsp:U2", "gsp:U3"), r$source_record_id), ]
  expect_equal(r$primary_source_available, c(TRUE, FALSE, TRUE))
  expect_equal(r$primary_document_url, c(r$source_url[1], "", "https://www.mofa.go.jp/files/000034408.pdf"))
  expect_equal(r$source_url[3], "https://www.mofa.go.jp/files/000034408.pdf")
})

test_that("codes come from title and cooperation sentences only, never source_coded", {
  raw <- rbind(
    gsp_row("C1", "JPN", "AUS", title = "Joint Statement on Hydrogen Cooperation"),
    gsp_row("C2", "USA", "IND", fulltext = paste(
      "The two sides agreed to deepen cooperation on critical minerals and to establish a working group.",
      "Steel exports were discussed.",
      "The Global Coalition to Counter ISIL was mentioned.")),
    gsp_row("C3", "CHN", "RUS", fulltext = "双方同意加强在新能源汽车和锂电池领域的合作。")
  )
  res <- adapt(raw)
  cd <- res$codes
  expect_false(any(cd$basis %in% c("source_coded", "curated", "type_implied")))
  expect_true(any(cd$source_record_id == "gsp:C1" & cd$code == "hydrogen" & cd$basis == "text_title"))
  expect_true(any(cd$source_record_id == "gsp:C2" & cd$code == "critical_minerals" & cd$basis == "text_body"))
  expect_true(any(cd$source_record_id == "gsp:C2" & cd$code == "working_group" & cd$basis == "text_body"))
  expect_false(any(cd$source_record_id == "gsp:C2" & cd$code == "steel"))
  expect_true(all(c("evs", "batteries") %in% cd$code[cd$source_record_id == "gsp:C3" & cd$dimension == "sector"]))
  r2 <- rec_of(res, "gsp:C2")
  expect_match(r2$sector, "critical_minerals")
  expect_match(r2$technology, "Batteries")
  expect_match(r2$instrument_types, "working_group")
  expect_equal(agr_validate(res, cfg), character())
})

test_that("extras describe each document; provenance dates come from the fetch and the version", {
  raw <- rbind(
    gsp_row("E1", "JPN", "AUS", fulltext = "The two leaders met in Tokyo and agreed to deepen cooperation."),
    gsp_row("E2", "CHN", "RUS", fulltext = "双方同意加强合作。"),
    gsp_row("E3", "RUS", "KAZ", fulltext = "Стороны договорились о сотрудничестве."),
    gsp_row("E4", "JPN", "IND", fulltext = "", title = "")
  )
  raw$dataset_version <- "V1.1 (2026-01-15)"
  raw$fetched_on <- "2026-10-01"
  res <- agr_adapt_gsp(raw, cfg, country_names = nm)
  docs <- res$extras$gsp_documents
  expect_equal(names(docs), c("source_record_id", "doc_id", "publisher_iso3", "partner_iso3", "type_raw", "title_sp",
                              "factsheet_sp", "n_chars", "language", "label_basis"))
  expect_equal(docs$language[match(c("gsp:E1", "gsp:E2", "gsp:E3", "gsp:E4"), docs$source_record_id)],
               c("latin", "cjk", "other", "none"))
  expect_equal(docs$publisher_iso3[docs$source_record_id == "gsp:E3"], "RUS")
  expect_equal(docs$partner_iso3[docs$source_record_id == "gsp:E3"], "KAZ")
  expect_identical(docs$title_sp[1], TRUE)
  expect_identical(docs$factsheet_sp[1], FALSE)
  expect_equal(docs$n_chars[docs$source_record_id == "gsp:E2"], nchar("双方同意加强合作。"))
  expect_true(all(res$records$source_last_updated == "2026-01-15"))
  expect_true(all(res$records$last_verified == "2026-10-01"))
  # Without a version column the V1.0 release date is used; fetched_on overrides the file.
  res0 <- agr_adapt_gsp(gsp_row("E5", "JPN", "AUS"), cfg, fetched_on = "2026-11-06", country_names = nm)
  expect_equal(res0$records$source_last_updated, GSP_V1_RELEASE_DATE)
  expect_equal(res0$records$last_verified, "2026-11-06")
  # Unknown names fall back to the ISO3 code.
  expect_equal(agr_adapt_gsp(gsp_row("E6", "TUV", "NZL"), cfg, country_names = nm)$records$description,
               "Joint Statement published by TUV")
})

test_that("the fetcher picks the newest world file and writes a fixed, keyed schema", {
  source(file.path(root, "scripts", "utils", "fetchers.R"), local = TRUE)
  source(file.path(root, "scripts", "fetchers", "gsp_partnerships.R"), local = TRUE)
  version <- list(versionNumber = 1L, versionMinorNumber = 0L, releaseTime = "2025-09-23T06:35:07Z", files = list(
    list(label = "codebook_Dataverse_20250923.pdf", dataFile = list(id = 12089888, creationDate = "2025-09-23")),
    list(label = "db_world_Dataverse_20240101.csv", dataFile = list(id = 1000, creationDate = "2024-01-01")),
    list(label = "db_world_Dataverse_20250923.csv", dataFile = list(id = 12089887, creationDate = "2025-09-23"))
  ))
  files <- gsp_dataverse_files(version)
  expect_equal(gsp_select_world_file(files)$id, "12089887")
  expect_equal(gsp_dataset_version(version), "V1.0 (2025-09-23)")
  expect_error(gsp_select_world_file(files[1, ]), "No db_world")

  up <- rbind(gsp_row("RUSMAR2016315", "RUS", "MAR"), gsp_row("RUSMAR2016315", "RUS", "MAR"),
              gsp_row("JPNAUS2015", "JPN", "AUS", source = "https://a"), gsp_row("JPNAUS2015", "JPN", "AUS", source = "https://a", title = "Other"))
  up$confidence_level2 <- NULL
  out <- suppressMessages(gsp_standardize_documents(up, source_file = "db_world.csv", dataset_version = "V1.0 (2025-09-23)",
                                                    fetched_on = "2026-11-06"))
  expect_equal(names(out), GSP_OUTPUT_COLUMNS)
  expect_equal(nrow(out), 3L)
  expect_false(anyDuplicated(out$row_key) > 0)
  expect_true(all(out$confidence_level2 == ""))
  expect_match(out$row_key[out$doc_id == "RUSMAR2016315"], "^RUSMAR2016315-[0-9a-f]{8}$")
  expect_equal(sum(grepl("-2$", out$row_key)), 1L)
  expect_error(gsp_standardize_documents(up[, setdiff(names(up), "fulltext")]), "fulltext")
  # The adapter reads the staged schema.
  res <- agr_adapt_gsp(out, cfg, country_names = nm)
  expect_equal(nrow(res$records), 3L)
  expect_equal(agr_validate(res, cfg), character())
})
