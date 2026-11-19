root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(root, "R", "agreements", "agr_schema.R"))
source(file.path(root, "R", "agreements", "agr_text_coding.R"))
source(file.path(root, "R", "agreements", "adapter_desta.R"))
cfg <- yaml::read_yaml(file.path(root, "config", "partnership_agreements.yml"))

# --- synthetic DESTA frames ----------------------------------------------------------------
# Numeric ISO codes used below.
N <- c(JPN = 392, KOR = 410, CHN = 156, SGP = 702, USA = 840, AUS = 36, DEU = 276, FRA = 250, GBR = 826,
       HRV = 191, ARG = 32, BRA = 76, PRY = 600, URY = 858, VEN = 862, BOL = 68, CAN = 124, MEX = 484,
       SEN = 686, CIV = 384, ALB = 8, KOS = 900, CHL = 152)

desta_entry <- function(number, base, name, type, year, eif, typememb, countries, roles = rep(0L, length(countries)),
                        consolidated = 0L, width = 8L) {
  row <- list(number = number, base_treaty = base, name = name, entry_type = type, consolidated = as.character(consolidated),
              year = if (is.na(year)) "" else as.character(year), entryforceyear = if (is.na(eif)) "" else as.character(eif),
              typememb = as.character(typememb), regioncon = "", wto_name = "")
  codes <- unname(N[countries])
  for (k in seq_len(width)) {
    row[[paste0("c", k)]] <- if (k <= length(codes)) as.character(codes[k]) else ""
  }
  for (k in seq_len(width)) {
    row[[paste0("mc", k)]] <- if (k <= length(roles)) as.character(roles[k]) else ""
  }
  as.data.frame(row, stringsAsFactors = FALSE)
}

desta_fixture <- function() {
  lst <- rbind(
    # RCEP-like: ASEAN member (role 1) + third countries (role 0); the dyads file would skip JPN-KOR.
    desta_entry("10", "10", "Regional Comprehensive Economic Partnership (RCEP)", "base_treaty", 2020, 2022, 3,
                c("SGP", "JPN", "KOR", "CHN"), c(1, 0, 0, 0)),
    # EU agreement; the UK withdraws in 2020; Croatia's accession is absorbed by EUU.
    desta_entry("20", "20", "EC Japan", "base_treaty", 2018, 2019, 3, c("JPN", "DEU", "FRA", "GBR", "HRV"), c(0, 1, 1, 1, 1)),
    desta_entry("20_1", "20", "EC Japan withdrawal UK", "withdrawal", 2020, 2020, 7, c("DEU", "FRA", "GBR", "HRV", "JPN"), c(2, 2, 1, 2, 2)),
    # EU agreement with an enlargement accession (typememb 6: role 2 = acceding, role 3 = counterpart).
    desta_entry("25", "25", "Chile EC", "base_treaty", 2002, 2003, 3, c("CHL", "DEU", "FRA", "GBR"), c(0, 1, 1, 1)),
    desta_entry("25+1", "25", "Chile EC Croatia accession", "accession", 2011, 2013, 6, c("CHL", "HRV"), c(3, 2)),
    desta_entry("30", "30", "Korea US", "base_treaty", 2007, 2012, 1, c("KOR", "USA")),
    desta_entry("40", "40", "Transpacific Partnership (TPP)", "base_treaty", 2016, NA, 2, c("USA", "JPN", "AUS")),
    # Plurilateral with accessions and a withdrawal.
    desta_entry("50", "50", "Mercado Pact", "base_treaty", 1991, 1991, 2, c("ARG", "BRA", "PRY", "URY")),
    desta_entry("50+1", "50", "Mercado Pact Venezuela accession", "accession", 2006, 2012, 5,
                c("ARG", "BRA", "PRY", "URY", "VEN"), c(2, 2, 2, 2, 1)),
    desta_entry("50+2", "50", "Mercado Pact Bolivia accession", "accession", 2015, NA, 5,
                c("ARG", "BRA", "PRY", "URY", "VEN", "BOL"), c(2, 2, 2, 2, 2, 1)),
    desta_entry("50_1", "50", "Mercado Pact withdrawal Venezuela", "withdrawal", 2017, 2017, 7,
                c("ARG", "BRA", "PRY", "URY", "VEN"), c(2, 2, 2, 2, 1)),
    # Base coding vs a consolidated entry (consolidated "61a" consolidates protocol 61 with 60).
    desta_entry("60", "60", "Canada Mexico", "base_treaty", 1995, 1996, 1, c("CAN", "MEX")),
    desta_entry("61", "60", "Canada Mexico Protocol", "protocol or amendment", 2010, 2011, 1, c("CAN", "MEX"), consolidated = -1),
    desta_entry("61a", "60", "Canada Mexico (consolidated)", "consolidated", 2010, 2011, 1, c("CAN", "MEX"), consolidated = 1),
    # A protocol with its own parties: the UK continuity agreement filed under EC Korea.
    desta_entry("70", "70", "EC Korea", "base_treaty", 2010, 2011, 3, c("KOR", "DEU", "FRA", "GBR"), c(0, 1, 1, 1)),
    desta_entry("70_1", "70", "EC Korea withdrawal UK", "withdrawal", 2020, 2020, 7, c("DEU", "FRA", "GBR", "KOR"), c(2, 2, 1, 2)),
    desta_entry("71", "70", "Korea UK", "protocol or amendment", 2019, 2021, 1, c("GBR", "KOR"), consolidated = -1),
    # Two-sided agreement without "EC" in the name but with every EU member of its year.
    desta_entry("80", "80", "Lome X", "base_treaty", 1975, 1976, 4, c("DEU", "FRA", "GBR", "SEN", "CIV"), c(2, 2, 2, 3, 3)),
    desta_entry("90", "90", "Albania Kosovo", "base_treaty", 2003, 2003, 1, c("ALB", "KOS")),
    desta_entry("95", "95", "Japan Korea", "negotiation", NA, NA, 1, c("JPN", "KOR"))
  )
  prov_cols <- c("mar_typedepth", "ser_specific", "ser_general", "inv_chapter", "inv_reference_service",
                 "inv_reference_bit", "inv_pre_est_oper", "inv_nt", "inv_mfn", "proc_prov", "tbt_prov", "sps_prov",
                 "tbt_coop", "regulation_cooperation_chapter", "comp_chap", "ipr_nt", "ipr_mfn",
                 "ipr_scope_substantial_dummy", "ipr_specific_enforcement_dummy", "ipr_specific_prov",
                 "comp_state_aid", "comp_subsidies", "tr_sub_prov", "comp_soe", "inv_ctry_dsm")
  prov_row <- function(number, base, ...) {
    v <- list(...)
    row <- c(list(number = number, base_treaty = base), setNames(as.list(rep("0", length(prov_cols))), prov_cols))
    for (n in names(v)) row[[n]] <- as.character(v[[n]])
    as.data.frame(row, stringsAsFactors = FALSE)
  }
  provisions <- rbind(
    prov_row("30", "30", mar_typedepth = 2, ser_specific = 1, inv_chapter = 1, inv_nt = 1, proc_prov = 2, tbt_prov = 1,
             tbt_coop = 1, comp_chap = 1, ipr_nt = 1, comp_soe = 1, inv_ctry_dsm = 1),
    prov_row("60", "60", mar_typedepth = 1, proc_prov = 1, sps_prov = 1),
    prov_row("61a", "60", mar_typedepth = 2, proc_prov = 2, sps_prov = 1, ser_specific = 1, ipr_specific_prov = 1),
    prov_row("50", "50", mar_typedepth = 3),
    prov_row("40", "40", mar_typedepth = 2, ser_specific = 1, inv_chapter = 1, proc_prov = 2, sps_prov = 1,
             comp_chap = 1, ipr_mfn = 1)
  )
  indices <- data.frame(
    number = c("30", "60", "40"), base_treaty = c("30", "60", "40"),
    full_fta = c(1, 0, 1), standards = c(1, 1, 1), investments = c(1, 0, 1), services = c(1, 0, 1),
    procurement = c(1, 0, 1), competition = c(1, 0, 1), iprs = c(1, 0, 1), depth_index = c(7, 1, 7),
    depth_rasch = c(2.1, -0.8, 2.3), flexrigid = c(3, 1, 3), enforce = c(2, 0, 2), stringsAsFactors = FALSE)
  withdrawals <- data.frame(country1 = "United Kingdom", country2 = "Japan", iso1 = 826, iso2 = 392,
                            number = "20_1", base_treaty = "20", name = "EC Japan withdrawal UK", year = 2020)
  list(lst = lst, provisions = provisions, indices = indices, withdrawals = withdrawals)
}

run_fixture <- function(fx = desta_fixture(), ...) {
  agr_adapt_desta(fx$lst, fx$provisions, fx$indices, fx$withdrawals, cfg,
                  eu_members = c("DEU", "FRA", "HRV"), fetched_on = "2026-11-06", as_of_year = 2026, ...)
}

rec_of <- function(res, id) res$records[res$records$source_record_id == id, , drop = FALSE]
design_of <- function(res, id) res$extras$desta_design[res$extras$desta_design$source_record_id == id, , drop = FALSE]

# --- tests ---------------------------------------------------------------------------------

test_that("the adapter passes the validator and skips negotiations", {
  res <- run_fixture()
  expect_equal(agr_validate(res, cfg), character())
  expect_false("desta:95" %in% res$records$source_record_id)
  expect_equal(res$extras$desta_summary$n_negotiation, 1L)
  expect_true(all(res$records$source_dataset == "desta"))
  expect_true(all(res$records$legal_status == "binding" & res$records$legal_status_basis == "source_document_type"))
  expect_true(all(res$records$relationship_category == "trade_economic"))
  expect_true(all(res$records$source_url == AGR_DESTA_SOURCE_URL & !res$records$primary_source_available))
  expect_true(all(res$records$last_verified == "2026-11-06" & res$records$source_last_updated == "2025-09"))
})

test_that("parties come from the c-columns, including members skipped by role", {
  res <- run_fixture()
  rcep <- rec_of(res, "desta:10")
  expect_equal(rcep$parties, "CHN|JPN|KOR|SGP")
  expect_equal(rcep$agreement_type, "epa")
  # Kosovo's code 900 maps to XKX.
  expect_equal(rec_of(res, "desta:90")$parties, "ALB|XKX")
})

test_that("signed agreements are not in force; entry-into-force years set the status", {
  res <- run_fixture()
  tpp <- rec_of(res, "desta:40")
  expect_equal(tpp$implementation_status, "signed")
  expect_equal(tpp$signature_date, "2016")
  expect_equal(tpp$effective_date, "")
  kus <- rec_of(res, "desta:30")
  expect_equal(kus$implementation_status, "in_force")
  expect_equal(c(kus$signature_date, kus$effective_date, kus$date_precision), c("2007", "2012", "year"))
  # An entry into force after as_of_year is still signed.
  res2 <- agr_adapt_desta(desta_fixture()$lst, NULL, NULL, NULL, cfg, c("DEU", "FRA", "HRV"), as_of_year = 2011)
  expect_equal(rec_of(res2, "desta:30")$implementation_status, "signed")
  expect_equal(agr_validate(res2, cfg), character())
})

test_that("accession records carry the parties at the time and the base coding", {
  res <- run_fixture()
  ven <- rec_of(res, "desta:50+1")
  expect_equal(ven$title, "Mercado Pact (accession of Venezuela)")
  expect_equal(ven$parties, "ARG|BRA|PRY|URY|VEN")
  expect_equal(c(ven$signature_date, ven$effective_date), c("2006", "2012"))
  bol <- rec_of(res, "desta:50+2")
  expect_equal(bol$parties, "ARG|BOL|BRA|PRY|URY")
  expect_equal(bol$implementation_status, "signed")
  expect_match(bol$notes, "VEN withdrew 2017")
  expect_equal(design_of(res, "desta:50+2")$acceding, "BOL")
  expect_equal(design_of(res, "desta:50+2")$coding_entry, "50")
  # Accessions have no provisions of their own: the base treaty's coding applies.
  expect_true("tariff_reduction" %in% agr_split(bol$instrument_types)[[1]])
})

test_that("withdrawals remove the withdrawing country; an accession whose acceding country left is inactive", {
  res <- run_fixture()
  expect_equal(rec_of(res, "desta:50")$parties, "ARG|BRA|PRY|URY")
  ven <- rec_of(res, "desta:50+1")
  expect_equal(ven$implementation_status, "inactive")
  expect_match(ven$implementation_status_raw, "VEN withdrew 2017")
  expect_equal(design_of(res, "desta:50+1")$withdrawn, "VEN")
  ecj <- rec_of(res, "desta:20")
  expect_match(ecj$notes, "GBR withdrew 2020")
  expect_equal(design_of(res, "desta:20")$withdrawn, "GBR")
})

test_that("EU member states collapse to EUU; enlargement accessions are absorbed", {
  res <- run_fixture()
  expect_equal(rec_of(res, "desta:20")$parties, "EUU|JPN")
  expect_equal(rec_of(res, "desta:20")$agreement_type, "epa")
  expect_true(design_of(res, "desta:20")$eu_collapsed)
  expect_equal(rec_of(res, "desta:25")$parties, "CHL|EUU")
  expect_false("desta:25+1" %in% res$records$source_record_id)
  sk <- res$extras$desta_skipped
  expect_equal(sk$reason[sk$desta_number == "25+1"], "eu_enlargement_absorbed_by_EUU")
  # Unnamed two-sided agreement with every EU member of its year (1975: DEU, FRA, GBR).
  expect_equal(rec_of(res, "desta:80")$parties, "CIV|EUU|SEN")
  # Intra-bloc and non-EU agreements stay country lists.
  expect_equal(rec_of(res, "desta:50")$parties, "ARG|BRA|PRY|URY")
})

test_that("a protocol with its own parties is a record; one with its parent's parties is not", {
  res <- run_fixture()
  kuk <- rec_of(res, "desta:71")
  expect_equal(kuk$parties, "GBR|KOR")
  expect_equal(kuk$implementation_status, "in_force")
  # The UK's withdrawal from EC Korea does not touch its own continuity agreement.
  expect_false(grepl("withdrew", kuk$notes))
  expect_equal(rec_of(res, "desta:70")$parties, "EUU|KOR")
  expect_false("desta:61" %in% res$records$source_record_id)
  expect_false("desta:61a" %in% res$records$source_record_id)
  res_off <- run_fixture(protocol_records = FALSE)
  expect_false("desta:71" %in% res_off$records$source_record_id)
})

test_that("consolidated coding is preferred over the base coding, with depth recomputed", {
  res <- run_fixture()
  d <- design_of(res, "desta:60")
  expect_equal(d$coding_entry, "61a")
  expect_equal(d$depth_index_source, "recomputed")
  # 61a: full_fta (typedepth 2), standards (sps), services, procurement (2), iprs = 5.
  expect_equal(d$depth_index, 5L)
  expect_equal(c(d$full_fta, d$standards, d$investments, d$services, d$procurement, d$competition, d$iprs),
               c(1L, 1L, 0L, 1L, 1L, 0L, 1L))
  ins <- agr_split(rec_of(res, "desta:60")$instrument_types)[[1]]
  expect_true(all(c("tariff_reduction", "government_procurement", "market_access") %in% ins))
  # Indices are used where they exist; nothing is coded for an uncoded treaty.
  expect_equal(design_of(res, "desta:30")$depth_index_source, "indices")
  expect_equal(design_of(res, "desta:30")$depth_rasch, 2.1)
  expect_equal(design_of(res, "desta:10")$depth_index_source, "none")
  expect_true(is.na(design_of(res, "desta:10")$depth_index))
})

test_that("depth recomputation follows the published rule and matches the indices", {
  fx <- desta_fixture()
  comp <- agr_desta_depth_components(fx$provisions)
  expect_equal(comp$depth_index, c(7L, 1L, 5L, 1L, 7L))
  chk <- agr_desta_check_depth(fx$provisions, fx$indices)
  expect_equal(chk$n_compared, 3L)
  expect_equal(chk$mismatches, character())
  # Missing values count as 0; mar_typedepth 3 (customs union) is a full FTA; proc_prov 1 is not.
  p <- data.frame(number = c("a", "b"), base_treaty = c("a", "b"), mar_typedepth = c("3", ""), proc_prov = c("1", "2"),
                  inv_reference_bit = c("", "1"), stringsAsFactors = FALSE)
  cp <- agr_desta_depth_components(p)
  expect_equal(cp$full_fta, c(1L, 0L))
  expect_equal(cp$procurement, c(0L, 1L))
  expect_equal(cp$investments, c(0L, 1L))
  bad <- fx$indices
  bad$depth_index[1] <- 6
  expect_equal(agr_desta_check_depth(fx$provisions, bad)$mismatches, "30")
})

test_that("instruments map from explicitly coded provisions only", {
  res <- run_fixture()
  codes <- res$codes
  kus <- codes[codes$source_record_id == "desta:30" & codes$dimension == "instrument_types", ]
  expect_setequal(kus$code, c("trade_preference", "tariff_reduction", "market_access", "fdi_framework",
                              "investment_facilitation", "government_procurement", "standards_cooperation",
                              "regulatory_cooperation"))
  expect_true(all(kus$basis == "source_coded"))
  expect_false(any(codes$code == "mutual_recognition"))
  # A partial-scope agreement with general procurement aims: preference only.
  base60 <- agr_desta_instruments(desta_fixture()$provisions[2, ], NULL)
  expect_setequal(base60$codes, c("trade_preference", "standards_cooperation"))
  # Every record is in the trade sector; an uncoded one has a type-implied preference.
  sec <- codes[codes$dimension == "sector", ]
  expect_setequal(sec$source_record_id, res$records$source_record_id)
  expect_true(all(sec$code == "trade" & sec$basis == "source_coded"))
  rcep <- codes[codes$source_record_id == "desta:10" & codes$dimension == "instrument_types", ]
  expect_equal(rcep$code, "trade_preference")
  expect_equal(rcep$basis, "type_implied")
  d <- design_of(res, "desta:30")
  expect_equal(c(d$soe, d$investor_state, d$state_aid), c(1L, 1L, 0L))
  expect_equal(res$records$sector[res$records$source_record_id == "desta:30"], "trade")
})

test_that("the withdrawal file names the withdrawing country when the roles do not", {
  fx <- desta_fixture()
  fx$lst$mc3[fx$lst$number == "20_1"] <- "2"
  res <- run_fixture(fx)
  expect_match(rec_of(res, "desta:20")$notes, "GBR withdrew 2020")
})

test_that("the fetcher finds the newest files on the downloads page and decodes Latin-1", {
  source(file.path(root, "scripts", "utils", "fetchers.R"), local = FALSE)
  source(file.path(root, "scripts", "fetchers", "desta.R"), local = FALSE)
  html <- paste(
    '<a href="/media/filer_public/aa/bb/x/desta_list_of_treaties_02_03.csv">csv</a>',
    '<a href="/media/filer_public/cc/dd/y/desta_list_of_treaties_02_04.csv">csv</a>',
    '<a href="/media/filer_public/cc/dd/z/desta_list_of_treaties_02_04_dyads.csv">dyads</a>',
    '<a href="/media/filer_public/ee/ff/w/desta_version_02_02.csv">old</a>',
    '<a href="https://www.designoftradeagreements.org/media/filer_public/11/22/v/desta_version_02_04.csv">new</a>',
    '<p>Current version: March 2026.</p><p>Current version: December 2023.</p>')
  files <- desta_discover_files(html)
  expect_equal(files$version[files$id == "desta_list_of_treaties"], "02_04")
  expect_equal(files$url[files$id == "desta_list_of_treaties"],
               "https://www.designoftradeagreements.org/media/filer_public/cc/dd/y/desta_list_of_treaties_02_04.csv")
  expect_match(files$url[files$id == "desta_dyads"], "_dyads\\.csv$")
  expect_match(files$url[files$id == "desta_provisions"], "11/22/v/desta_version_02_04\\.csv$")
  expect_false(files$discovered[files$id == "desta_indices"])
  expect_match(files$url[files$id == "desta_indices"], "desta_indices_version_02_03\\.csv$")
  expect_equal(attr(files, "release"), "2026-03")
  expect_equal(attr(desta_discover_files(""), "release"), "2025-09")
  latin1 <- as.raw(c(charToRaw('"number","name"\n"1","AANZFTA'), as.raw(0xa0), charToRaw('FTA"\n"2","T'), as.raw(0xfc),
                     charToRaw('rkiye"\n')))
  df <- desta_read_csv_text(desta_decode_text(latin1))
  expect_equal(df$name, c("AANZFTA FTA", "Türkiye"))
  for (id in names(DESTA_FILES)) expect_false(is.null(get_fetcher(id)))
})
