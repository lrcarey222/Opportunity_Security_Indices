root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(root, "R", "agreements", "agr_schema.R"))
source(file.path(root, "R", "agreements", "agr_text_coding.R"))
source(file.path(root, "R", "agreements", "adapter_memberships.R"))
source(file.path(root, "R", "agreements", "adapter_curated.R"))
source(file.path(root, "R", "agreements", "adapter_pmemo.R"))
source(file.path(root, "R", "agreements", "adapter_iisd.R"))
cfg <- yaml::read_yaml(file.path(root, "config", "partnership_agreements.yml"))

# --- memberships ---------------------------------------------------------------------------

test_that("memberships: U.S. FTAs become USA-X pairs and clubs one plurilateral record each", {
  tbl <- read.csv(file.path(root, "data", "reference", "economic_security_agreements.csv"),
                  colClasses = "character", stringsAsFactors = FALSE)
  res <- agr_adapt_memberships(tbl, cfg)
  rec <- res$records
  expect_equal(agr_validate(res, cfg), character())
  n_fta <- sum(tbl$agreement == "U.S. FTA")
  expect_equal(nrow(rec), n_fta + length(setdiff(unique(tbl$agreement), "U.S. FTA")))
  fta <- rec[rec$agreement_type == "fta", ]
  expect_equal(nrow(fta), n_fta)
  expect_true(all(fta$n_parties == 2L & grepl("USA", fta$parties)))
  expect_true(all(fta$legal_status == "binding" & fta$implementation_status == "in_force"))
  expect_true(all(fta$relationship_category == "trade_economic"))
  aus <- rec[rec$source_record_id == "memberships:us_fta:AUS", ]
  expect_equal(aus$title, "U.S.-Australia FTA")
  expect_equal(aus$parties, "AUS|USA")
  expect_equal(aus$effective_date, "2005-01-01")
  can <- rec[rec$source_record_id == "memberships:us_fta:CAN", ]
  expect_equal(can$title, "NAFTA / USMCA")
  expect_true(grepl("1994-01-01", can$notes) && grepl("2020-07-01", can$notes))
  cri <- rec[rec$source_record_id == "memberships:us_fta:CRI", ]
  expect_true(grepl("DOM, GTM, HND, NIC, SLV", cri$notes, fixed = TRUE))
  fc <- res$codes[res$codes$source_record_id == "memberships:us_fta:AUS", ]
  expect_setequal(paste(fc$dimension, fc$code, fc$basis),
                  c("sector trade type_implied", "instrument_types trade_preference type_implied",
                    "instrument_types tariff_reduction type_implied"))

  msp <- rec[rec$source_record_id == "memberships:msp", ]
  expect_equal(msp$agreement_type, "ministerial_initiative")
  expect_equal(msp$legal_status, "non_binding")
  expect_equal(msp$implementation_status, "inactive")
  expect_equal(msp$announcement_date, "2022-06")
  expect_equal(msp$sector, "critical_minerals")
  expect_true(all(c("USA", "EUU") %in% agr_split(msp$parties)[[1]]))
  forum <- rec[rec$source_record_id == "memberships:msp_forum", ]
  forum_members <- res$extras$members[res$extras$members$source_record_id == "memberships:msp_forum", ]
  expect_equal(sum(!forum_members$added_from_note), sum(tbl$agreement == "MSP Forum"))
  expect_true(all(forum_members$iso3[forum_members$added_from_note] %in% tbl$iso3[tbl$agreement == "MSP"]))
  expect_true("USA" %in% agr_split(forum$parties)[[1]])
  ipef <- rec[rec$source_record_id == "memberships:ipef", ]
  expect_equal(ipef$legal_status, "mixed")
  expect_equal(ipef$legal_status_basis, "curated")
  expect_equal(ipef$sector, "general_clean_energy|general_industrial|trade")
  expect_true(grepl("supply_chain_monitoring", ipef$instrument_types))
  ic <- res$codes[res$codes$source_record_id == "memberships:ipef" & res$codes$dimension == "instrument_types", ]
  expect_true(all(ic$basis == "curated" & grepl("https://", ic$snippet)))
  nato <- rec[rec$source_record_id == "memberships:nato", ]
  expect_equal(nato$agreement_type, "treaty")
  expect_equal(nato$signature_date, "1949-04-04")
  expect_equal(nato$n_parties, sum(tbl$agreement == "NATO"))
  g7 <- rec[rec$source_record_id == "memberships:g7", ]
  expect_equal(g7$primary_date, "1975")
  expect_equal(g7$sector, "")
  expect_true(all(rec$last_verified == "2026-10-08"))
})

test_that("memberships: an agreement without a spec stops the adapter", {
  tbl <- data.frame(iso3 = c("AUS", "JPN"), country = c("Australia", "Japan"), agreement = "Quad",
                    membership_status = "member", since = "2007", source = "x", url = "https://x.gov", accessed = "2026-10-08",
                    note = "", stringsAsFactors = FALSE)
  expect_error(agr_adapt_memberships(tbl, cfg), "no MEMBERSHIP_SPECS entry for agreement\\(s\\): Quad")
})

# --- curated -------------------------------------------------------------------------------

curated_fixture <- function() {
  row <- function(...) {
    x <- as.list(setNames(rep("", length(CURATED_COLUMNS)), CURATED_COLUMNS))
    v <- list(...)
    x[names(v)] <- v
    as.data.frame(x, stringsAsFactors = FALSE)
  }
  rbind(
    row(curated_id = "EU-CRM-CHL-2023", title = "MoU on sustainable raw materials value chains", parties = "EUU|CHL",
        agreement_type = "mou", agreement_type_raw = "Memorandum of Understanding", legal_status = "non_binding",
        legal_status_note = "This Memorandum does not create legally binding obligations.",
        signature_date = "2023-07-18", relationship_category = "sectoral", sectors = "critical_minerals",
        instruments = "working_group|investment_facilitation", supply_chain_segments = "Extraction|Refining",
        implementation_status = "implementation_underway", implementation_status_raw = "roadmap adopted",
        implementation_evidence = "joint roadmap adopted: https://ec.europa.eu/roadmap.",
        source_url = "https://single-market-economy.ec.europa.eu/news/x", publisher = "European Commission",
        last_verified = "2026-11-06", verification = "verified", description = "A partnership on raw materials."),
    row(curated_id = "JPN-USA-H2-2024", title = "Hydrogen cooperation statement", parties = "USA|JPN",
        agreement_type = "joint_statement", legal_status = "unknown", announcement_date = "2024-04",
        relationship_category = "sectoral", sectors = "hydrogen", implementation_status = "announced",
        source_url = "https://www.energy.gov/articles/x", primary_document_url = "", publisher = "U.S. DOE",
        last_verified = "2026-11-06", verification = "partial", description = "A statement on hydrogen.")
  )
}

test_that("curated: rows map 1:1 with curated codes, derived technologies and evidence extras", {
  res <- agr_adapt_curated(curated_fixture(), cfg)
  expect_equal(agr_validate(res, cfg), character())
  rec <- res$records
  expect_equal(rec$source_record_id, c("curated:EU-CRM-CHL-2023", "curated:JPN-USA-H2-2024"))
  expect_equal(rec$parties, c("CHL|EUU", "JPN|USA"))
  expect_equal(rec$legal_status_basis, c("curated", "none"))
  expect_true(all(res$codes$basis == "curated"))
  # Explicit stages win over the stages a sector implies.
  expect_equal(rec$supply_chain_segment[1], "Extraction|Refining")
  expect_equal(rec$technology[2], "Green Hydrogen")
  expect_true(grepl("Batteries", rec$technology[1]))
  expect_equal(rec$primary_source_available, c(TRUE, TRUE))
  expect_true(grepl("does not create legally binding", rec$notes[1]) && grepl("Publisher: European Commission", rec$notes[1]))
  ie <- res$extras$implementation_evidence
  expect_equal(ie$source_record_id, "curated:EU-CRM-CHL-2023")
  expect_equal(ie$url, "https://ec.europa.eu/roadmap")
  expect_equal(res$extras$curated_meta$verification, c("verified", "partial"))
})

test_that("curated: malformed rows stop the adapter with their curated_id", {
  bad <- curated_fixture()
  bad$parties[1] <- "EU|CHL"
  bad$sectors[2] <- "hydrogen|fusion"
  bad$announcement_date[2] <- "2024-13"
  err <- tryCatch(agr_adapt_curated(bad, cfg), error = function(e) conditionMessage(e))
  expect_match(err, "EU-CRM-CHL-2023: parties not ISO3 \\(use EUU for the EU\\): EU")
  expect_match(err, "JPN-USA-H2-2024: sectors not in vocabulary: fusion")
  expect_match(err, "JPN-USA-H2-2024: announcement_date '2024-13'")
  bad2 <- curated_fixture()
  bad2$legal_status_note[1] <- ""
  bad2$implementation_evidence[1] <- ""
  err2 <- tryCatch(agr_adapt_curated(bad2, cfg), error = function(e) conditionMessage(e))
  expect_match(err2, "needs legal_status_note")
  expect_match(err2, "needs implementation_evidence")
  expect_error(agr_adapt_curated(curated_fixture()[, -1], cfg), "lacks columns: curated_id")
})

test_that("curated: the committed CSV has the expected header and ingests", {
  path <- file.path(root, "data", "reference", "partnership_agreements", "curated_agreements.csv")
  tbl <- read.csv(path, colClasses = "character", stringsAsFactors = FALSE, check.names = FALSE)
  expect_equal(names(tbl), CURATED_COLUMNS)
  res <- agr_adapt_curated(tbl, cfg)
  expect_equal(agr_validate(res, cfg), character())
})

# --- pmemo ---------------------------------------------------------------------------------

pmemo_fixture <- function() {
  mech <- data.frame(
    mechanism_id = c("M-INV", "M-CMF", "M-QUAD"),
    pair = c("USA|JPN", "USA|JPN", "USA|JPN|AUS|IND"),
    name = c("Strategic Investment Initiative (MOU of 4 Sep 2025)",
             "Critical minerals and rare earths framework (Oct 2025)",
             "Quad Critical Minerals Initiative Framework (May 2026)"),
    mechanism_type = c("investment_vehicle", "framework", "plurilateral"),
    status = c("in_force", "signed", "announced"),
    evidence_ids = c("E1|E2|E3|E4|E5|E9", "E6|E7", "E8"),
    scope_compacts = c("SC-NUCLEAR|cross_sector", "SC-MAGNETS|SC-SEMIS", "SC-BATTERIES"),
    cross_sector = c("TRUE", "FALSE", "FALSE"), has_pipeline = c("TRUE", "FALSE", "FALSE"),
    has_finance = c("TRUE", "partial", "FALSE"), has_kpis = "FALSE", has_review = "FALSE",
    reciprocal = "FALSE", sunset = c("2029-01-19", "FALSE", "FALSE"), assessed_on = "2026-11-06",
    notes = c("Projects funded through SPVs with JBIC loans.", "Commits financing; non-binding and names no amounts.",
              "Up to US$20bn to be mobilised."),
    stringsAsFactors = FALSE)
  ev <- data.frame(
    evidence_id = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", "E11", "E12", "E13", "UJ-1"),
    item_type = c("agreement", "project", "financing", "project", "financing", "framework", "institution",
                  "plurilateral", "dispute", "dialogue", "policy", "policy", "policy", "policy"),
    name = c("MOU on Strategic Investment", "Gas generation plant", "JBIC loan for gas plant", "SMR fleet",
             "Unverified loan", "Framework for critical minerals", "Rapid Response Group", "Quad framework",
             "Dispute on selection", "Energy dialogue", "Export controls on rare earths", "DoD price floor",
             "US-Australia Critical Minerals Framework", "Framework for critical minerals (second pass)"),
    institution = c("Commerce / METI", "Commerce / METI", "JBIC / NEXI", "Commerce", "JBIC", "White House", "DOE / METI",
                    "Quad", "METI", "MOFA", "MOFCOM", "DoD", "US Govt; Govt of Australia", "US Govt; Govt of Japan"),
    countries = c("USA|JPN", "USA|JPN", "JPN|USA", "USA|JPN", "JPN|USA", "USA|JPN", "USA|JPN", "USA|JPN|AUS|IND",
                  "USA|JPN", "USA|JPN", "CHN|JPN", "USA", "USA|AUS", "USA|JPN"),
    status = c("signed", "announced", "signed", "under_construction", "signed", "signed", "announced", "announced",
               "suspended", "unknown", "in_force", "in_force", "in_force", "in_force"),
    publication_date = c("2025-09-04", "2026-02-17", "2026-05-01", "2026-03-19", "", "2025-10-27", "2025-10-27",
                         "2026-05-26", "2026-02-13", "2024-09-26", "2025-04-04", "2025-07-10", "2025-10-20", "2025-10-28"),
    access_date = "2026-11-06",
    technology = c("cross_sector", "power_generation", "power_generation", "BWRX-300 SMR", "energy_lng",
                   "rare_earths_magnets", "critical_minerals", "critical_minerals", "cross_sector", "cross_sector",
                   "Medium and heavy rare earths", "NdFeB magnets", "Critical minerals / rare earths", "Critical minerals / rare earths"),
    compact = c("cross_sector", "cross_sector", "cross_sector", "SC-NUCLEAR", "cross_sector", "SC-MAGNETS", "cross_sector",
                "cross_sector", "cross_sector", "cross_sector", "SC-MAGNETS", "SC-MAGNETS", "SC-MAGNETS", "SC-MAGNETS"),
    stage = c("cross_stage", "Deployment", "Deployment", "Deployment", "Deployment", "cross_stage", "cross_stage",
              "cross_stage", "cross_stage", "cross_stage", "Refining", "Refining", "Extraction", "Extraction"),
    instrument = c("investment_vehicle", "investment_vehicle", "loan", "investment_vehicle", "loan", "stockpile",
                   "dialogue", "investment_vehicle", "investment_vehicle", "dialogue", "export_control", "price_floor",
                   "other", "other"),
    amount_usd = c("5.5e+11", "3.33e+10", "6.3e+08", "4e+10", "", "", "", "2e+10", "", "", "", "", "8.5e+09", ""),
    source = c("CSIS", "Commerce bulletin", "JBIC press release", "Commerce bulletin", "Trade press", "White House",
               "White House", "The National", "Bloomberg", "METI", "Caixin", "SEC 8-K", "Australian PM release", "Mining.com"),
    url = c("https://www.csis.org/x", "https://content.govdelivery.com/x", "https://www.jbic.go.jp/en/x",
            "https://content.govdelivery.com/y", "", "https://www.whitehouse.gov/x", "https://www.whitehouse.gov/x",
            "https://www.thenationalnews.com/x", "https://news.bloomberglaw.com/x", "https://www.meti.go.jp/x",
            "https://x.com/a", "https://www.sec.gov/x", "https://www.pm.gov.au/x", "https://www.mining.com/x"),
    claim = c("MOU signed.", "Natural gas generation at about $33bn.", strrep("JBIC signed a loan to the SPV for gas generation. ", 10),
              "SMR fleet under construction.", "Reported loan.", "Framework commits financing; it is non-binding.",
              "Group established.", "Up to $20bn.", "Sides far apart.", "Agreed to meet periodically.",
              "Licensing requirements.", "Price floor.", "Signed a framework with a pipeline.", "Signed a framework."),
    verification = c("partial", "partial", "verified", "partial", "unverified", "verified", "verified", "partial",
                     "partial", "partial", "partial", "verified", "partial", "partial"),
    research_pass = "x", basis = "external", stringsAsFactors = FALSE)
  list(mech = mech, ev = ev)
}

test_that("pmemo: mechanisms and unlinked agreement-like rows become records", {
  fx <- pmemo_fixture()
  res <- agr_adapt_pmemo(fx$mech, fx$ev, cfg, duplicates = list(c("E6", "UJ-1")))
  expect_equal(agr_validate(res, cfg), character())
  rec <- res$records
  # E10 (dialogue) and E13 (two-country policy) are records; E11 (export control), E12 (one
  # country), E9 (dispute) and UJ-1 (duplicate of the linked E6) are not.
  expect_setequal(rec$source_record_id, c("pmemo:M-INV", "pmemo:M-CMF", "pmemo:M-QUAD", "pmemo:E10", "pmemo:E13"))
  inv <- rec[rec$source_record_id == "pmemo:M-INV", ]
  expect_equal(inv$agreement_type, "financing_agreement")
  expect_equal(inv$relationship_category, "investment_finance")
  expect_equal(inv$signature_date, "2025-09-04")
  expect_equal(inv$implementation_status, "implementation_underway")
  expect_match(inv$implementation_status_raw, "1 linked project\\(s\\) under construction or operating \\(E4\\)")
  expect_match(inv$implementation_status_raw, "financing\\(s\\) signed, verified \\(E3\\)")
  expect_equal(inv$sector, "fossil_fuels|nuclear")
  expect_equal(inv$instrument_types, "export_credit|joint_investment_fund")
  expect_equal(inv$supply_chain_segment, "Deployment")
  cmf <- rec[rec$source_record_id == "pmemo:M-CMF", ]
  expect_equal(cmf$legal_status, "non_binding")
  expect_equal(cmf$signature_date, "2025-10-27")
  # SC-SEMIS on a critical-minerals mechanism means its mineral inputs, not semiconductors.
  expect_equal(cmf$sector, "critical_minerals")
  expect_true(all(c("critical_mineral_supply", "public_finance", "working_group") %in% agr_split(cmf$instrument_types)[[1]]))
  quad <- rec[rec$source_record_id == "pmemo:M-QUAD", ]
  expect_equal(quad$n_parties, 4L)
  expect_equal(quad$agreement_type, "framework")
  expect_equal(quad$announcement_date, "2026-05-26")
  expect_equal(quad$sector, "batteries|critical_minerals")
  expect_equal(rec$agreement_type[rec$source_record_id == "pmemo:E10"], "ministerial_initiative")
  expect_equal(rec$agreement_type[rec$source_record_id == "pmemo:E13"], "framework")
  expect_true(all(res$codes$basis %in% c("curated", "type_implied")))
})

test_that("pmemo: delivery rows become implementation evidence; verification feeds curated_meta", {
  fx <- pmemo_fixture()
  res <- agr_adapt_pmemo(fx$mech, fx$ev, cfg, duplicates = list(c("E6", "UJ-1")))
  ie <- res$extras$implementation_evidence
  # E5 has no URL; disputes are never implementation evidence.
  expect_equal(ie$evidence_id, c("E2", "E3", "E4"))
  expect_true(all(ie$source_record_id == "pmemo:M-INV"))
  expect_true(all(nchar(ie$claim) <= 200))
  expect_equal(ie$amount_usd[ie$evidence_id == "E3"], 6.3e8)
  meta <- res$extras$curated_meta
  expect_equal(meta$verification[meta$source_record_id == "pmemo:M-CMF"], "verified")
  expect_equal(meta$publisher[meta$source_record_id == "pmemo:M-CMF"], "White House")
  dup <- res$extras$duplicates
  expect_match(dup$action[dup$evidence_id == "UJ-1"], "dropped: duplicate of E6 \\(M-CMF\\)")
  # Without the signed-financing rule only the project under construction counts.
  res2 <- agr_adapt_pmemo(fx$mech, fx$ev, cfg, duplicates = list(), financing_signed_counts = FALSE)
  expect_equal(res2$records$implementation_status[res2$records$source_record_id == "pmemo:M-INV"], "implementation_underway")
  expect_true("pmemo:UJ-1" %in% res2$records$source_record_id)
})

test_that("pmemo: excluded plurilateral items with incomplete party lists are never records", {
  expect_true("E37" %in% names(PMEMO_EXCLUDE))
  fx <- pmemo_fixture()
  # E10 (a dialogue naming two countries) stands in for FORGE; M-QUAD for an excluded mechanism.
  res <- agr_adapt_pmemo(fx$mech, fx$ev, cfg, duplicates = list(c("E6", "UJ-1")),
                         exclude = c(E10 = "club without a published roster", `M-QUAD` = "test"))
  expect_equal(agr_validate(res, cfg), character())
  expect_false(any(c("pmemo:E10", "pmemo:M-QUAD") %in% res$records$source_record_id))
  expect_true("pmemo:E13" %in% res$records$source_record_id)
  ex <- res$extras$excluded
  expect_setequal(ex$id, c("E10", "M-QUAD"))
  expect_equal(ex$kind[ex$id == "M-QUAD"], "mechanism")
  expect_false(any(grepl("^pmemo:(E10|M-QUAD)$", res$codes$source_record_id)))
})

test_that("pmemo: text dates are parsed at their precision", {
  expect_equal(pmemo_text_date("(MOU of 4 Sep 2025)"), "2025-09-04")
  expect_equal(pmemo_text_date("(Oct 2025) with a group"), "2025-10")
  expect_equal(pmemo_text_date("(USJTA and DTA 2019; July 2025 deal and EO 14345)"), "2019")
  expect_equal(pmemo_text_date("no date here"), "")
  expect_equal(pmemo_shorten(strrep("word ", 100)), paste0(trimws(substr(strrep("word ", 100), 1, 195)), "..."))
})

# --- iisd ----------------------------------------------------------------------------------

iisd_fixture <- function() {
  data.frame(
    "Agreement ID" = c("T-001", "T-002", "T-003"),
    "Agreement" = c("EU-South Africa Clean Trade and Investment Partnership", "India-New Zealand FTA",
                    "Japan-Korea Comprehensive Strategic Partnership on supply chains"),
    "Signatories" = c("European Union; South Africa", "India | New Zealand", "Japan, Republic of Korea"),
    "Date of signature" = c("2025-03-13", "15 March 2026", "March 2026"),
    "Type of agreement" = c("MoU", "FTA", "Strategic partnership"),
    "Text available" = c("Yes", "Yes", "No"),
    "Energy: Renewables" = c("The Participants will cooperate on solar photovoltaic manufacturing and green hydrogen projects.", "", ""),
    "Energy: Cooperation and Systems" = c("", "Yes", ""),
    "Climate: Emissions" = c("", "", "X"),
    "CRMs: Value Chain Stages" = c("The Participants will promote joint investment in refining of rare earths.", "", ""),
    "CRMs: Supply Chain Diversification / Security" = c("", "", "No"),
    "Heavy Industry: Decarbonization" = c("", "", "Cooperation on low-carbon steel production is promoted."),
    "Tariffs & Market Access" = c("", "Duty-free treatment for solar modules.", ""),
    "Text URL" = c("https://policy.trade.ec.europa.eu/x.pdf", "https://www.mfat.govt.nz/x.pdf", ""),
    check.names = FALSE, stringsAsFactors = FALSE)
}

test_that("iisd: a synthetic export is matched, coded and validated", {
  res <- agr_adapt_iisd(iisd_fixture(), cfg)
  expect_equal(agr_validate(res, cfg), character())
  rec <- res$records
  expect_equal(rec$source_record_id, c("iisd:T-001", "iisd:T-002", "iisd:T-003"))
  expect_equal(rec$parties, c("EUU|ZAF", "IND|NZL", "JPN|KOR"))
  expect_equal(rec$signature_date, c("2025-03-13", "2026-03-15", "2026-03"))
  expect_equal(rec$agreement_type, c("mou", "fta", "strategic_partnership"))
  expect_equal(rec$partnership_label, c("", "", "comprehensive_strategic"))
  expect_equal(rec$implementation_status, rep("signed", 3))
  expect_equal(rec$legal_status, rep("unknown", 3))
  expect_equal(rec$primary_source_available, c(TRUE, TRUE, FALSE))
  cd <- res$codes
  src <- cd[cd$basis == "source_coded", ]
  expect_true(all(c("general_clean_energy", "critical_minerals") %in% src$code[src$source_record_id == "iisd:T-001"]))
  expect_true(all(c("trade", "tariff_reduction", "market_access", "general_clean_energy") %in% src$code[src$source_record_id == "iisd:T-002"]))
  expect_true("general_industrial" %in% src$code[src$source_record_id == "iisd:T-003"])
  # "No" is not coded; Climate has no sector of its own.
  expect_false(any(src$source_record_id == "iisd:T-003" & src$code == "critical_minerals"))
  body <- cd[cd$basis == "text_body", ]
  expect_true(all(c("solar", "hydrogen") %in% body$code[body$source_record_id == "iisd:T-001"]))
  expect_true("steel" %in% body$code[body$source_record_id == "iisd:T-003"])
  expect_true(all(c("title", "parties", "signature_date", "agreement_type", "document_url", "tariffs_market_access") %in%
                    res$extras$column_map$canonical))
  expect_true("Climate: Emissions" %in% res$extras$topics$label[res$extras$topics$source_record_id == "iisd:T-003"])
})

test_that("iisd: long-form topics, group parties and unresolved names", {
  tbl <- data.frame(title = c("EFTA-Malaysia EEPA", "Atlantis-Japan MoC"),
                    parties = c("EFTA; MYS", "Atlantis; Japan"), type = c("FTA", "MoC"),
                    topics = c("Tariffs and Market Access; Energy", "CRMs"), date = c("2025-06-23", "2025"),
                    stringsAsFactors = FALSE)
  expect_error(agr_adapt_iisd(tbl, cfg), "do not resolve to ISO3.*Atlantis")
  res <- agr_adapt_iisd(tbl, cfg, strict = FALSE)
  expect_equal(agr_validate(res, cfg), character())
  expect_equal(nrow(res$records), 1L)
  expect_equal(res$records$parties, "CHE|ISL|LIE|MYS|NOR")
  expect_equal(res$records$sector, "general_clean_energy|trade")
  expect_true(any(res$extras$unresolved_parties$name == "Atlantis"))
  expect_error(agr_adapt_iisd(data.frame(name = "x", stringsAsFactors = FALSE), cfg), "lacks required columns")
})
