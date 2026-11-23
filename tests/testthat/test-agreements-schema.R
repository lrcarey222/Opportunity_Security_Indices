root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(root, "R", "agreements", "agr_schema.R"))
source(file.path(root, "R", "agreements", "agr_text_coding.R"))
cfg <- yaml::read_yaml(file.path(root, "config", "partnership_agreements.yml"))

test_that("party fields are sorted, keyed and typed", {
  pf <- agr_party_fields(list(c("JPN", "AUS"), c("USA", "JPN", "KOR", "JPN")))
  expect_equal(pf$parties, c("AUS|JPN", "JPN|KOR|USA"))
  expect_equal(pf$country_a, c("AUS", "JPN"))
  expect_equal(pf$country_b, c("JPN", "KOR"))
  expect_equal(pf$additional_parties, c("", "USA"))
  expect_equal(pf$n_parties, c(2L, 3L))
  expect_equal(pf$bilateral_or_plurilateral, c("bilateral", "plurilateral"))
  expect_equal(agr_pair_key(c("USA", "AUS"), c("JPN", "NZL")), c("JPN|USA", "AUS|NZL"))
})

test_that("dates normalise to day, month or year precision and invalid days fall back", {
  expect_equal(agr_norm_date(c("2013-05-02", "2019-05-60", "2019-05", "2004", "2004.0", "31/03/2016", "", "n/a")),
               c("2013-05-02", "2019-05", "2019-05", "2004", "2004", "2016-03-31", "", ""))
  expect_equal(agr_date_precision(c("2013-05-02", "2019-05", "2004", "")), c("day", "month", "year", "none"))
  expect_equal(agr_date_value("2019-05"), as.Date("2019-05-01"))
  expect_equal(agr_primary_date(c("", "2020"), c("2019-01-02", ""), c("", "2021")), c("2019-01-02", "2020"))
})

test_that("official hosts are recognised, including archived government pages", {
  urls <- c("https://www.mofa.go.jp/a_o/page.html", "https://web.archive.org/web/2015/http://www.state.gov/r/pa",
            "https://www.reuters.com/world/x", "http://www.fmprc.gov.cn/eng/x", "")
  expect_equal(agr_is_official_url(urls, cfg$official_hosts), c(TRUE, TRUE, FALSE, TRUE, FALSE))
})

test_that("body keywords need a cooperation sentence; titles do not", {
  ids <- c("t:1", "t:2", "t:3")
  titles <- c("Joint Statement on Hydrogen", "Joint Statement", "Joint Statement")
  bodies <- c("",
              "The two sides agreed to deepen cooperation on critical minerals and rare earths. They also discussed nuclear weapons.",
              "The leaders condemned the missile tests. Steel exports were discussed.")
  codes <- agr_code_documents(ids, titles, bodies, cfg)
  sec <- codes[codes$dimension == "sector", ]
  expect_true(any(sec$source_record_id == "t:1" & sec$code == "hydrogen" & sec$basis == "text_title"))
  expect_true(any(sec$source_record_id == "t:2" & sec$code == "critical_minerals" & sec$basis == "text_body"))
  expect_false(any(sec$code == "nuclear"))
  # "Steel exports were discussed." has no cooperation word.
  expect_false(any(sec$source_record_id == "t:3"))
  # Technologies and stages follow from the sector.
  tech <- codes[codes$dimension == "technology" & codes$source_record_id == "t:2", ]
  expect_true(all(c("Batteries", "Electric Vehicles") %in% tech$code))
  expect_true(all(tech$basis == "text_body"))
  seg <- codes[codes$dimension == "supply_chain_segment" & codes$source_record_id == "t:2", ]
  expect_setequal(seg$code, c("Extraction", "Refining", "Recovery"))
})

test_that("instrument keywords code operational and dialogue instruments with snippets", {
  codes <- agr_code_documents("x:1", "MOU", paste(
    "The ministers agreed to establish a working group on supply chains.",
    "JBIC and NEXI will finance a battery plant through export credit and a joint investment fund."), cfg)
  ins <- codes[codes$dimension == "instrument_types", ]
  expect_true(all(c("working_group", "export_credit", "joint_investment_fund") %in% ins$code))
  expect_true(all(nzchar(ins$snippet)))
})

test_that("CJK texts are coded for the main sector terms", {
  codes <- agr_code_documents("c:1", "", "双方同意加强在新能源汽车和锂电池领域的合作。", cfg)
  sec <- codes$code[codes$dimension == "sector"]
  expect_true(all(c("evs", "batteries") %in% sec))
})

test_that("validator accepts a well-formed result and names vocabulary breaches", {
  pf <- agr_party_fields(list(c("AUS", "JPN")))
  rec <- data.frame(source_dataset = "curated", source_record_id = "curated:1", title = "X",
                    pf, agreement_type = "mou", legal_status = "non_binding",
                    legal_status_basis = "curated", relationship_category = "sectoral",
                    implementation_status = "signed", signature_date = "2023-07-01",
                    stringsAsFactors = FALSE)
  rec <- agr_as_records(rec)
  codes <- agr_as_codes(data.frame(source_record_id = "curated:1", dimension = "sector",
                                   code = "hydrogen", basis = "curated", n_hits = 1L, snippet = ""))
  codes <- agr_derive_tech_codes(codes, cfg)
  rec <- agr_apply_codes(rec, codes)
  expect_equal(agr_validate(list(records = rec, codes = codes), cfg), character())
  expect_equal(rec$technology, "Green Hydrogen")
  bad <- rec
  bad$agreement_type <- "pact"
  bad$parties <- "AUS"
  expect_true(any(grepl("agreement_type not in vocabulary", agr_validate(list(records = bad, codes = codes), cfg))))
  expect_true(any(grepl("fewer than two parties", agr_validate(list(records = bad, codes = codes), cfg))))
})
