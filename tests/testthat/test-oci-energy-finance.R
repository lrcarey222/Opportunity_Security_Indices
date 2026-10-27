# OCI Public Finance for Energy: the extract's cleaning (R/utils/oci_energy_finance.R), the project's
# institution and sector/stage tables (data/reference/oci_energy_finance_*.csv) and their merge into
# the Development Potential aid builder (partnership_strength_oci_amounts(),
# partnership_strength_build_oecd_tech()). Offline.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "oci_energy_finance.R"))
source(file.path(repo_root, "R", "utils", "supply_chain_stages.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "stronger_development.R"))

oci_ref <- function(f) read.csv(file.path(repo_root, "data", "reference", f), stringsAsFactors = FALSE)

oci_row <- function(...) {
  base <- list(fy = 2022L, institution_group = "Japan", institution_abbr = "JBIC", institution_kind = "Export Credit",
               mechanism = "Loan", recipient_iso3 = "IND", recipient_share = 1, sector = "Solar",
               stage = "Electricity Production", amount_usd = 1e8)
  tibble::as_tibble(utils::modifyList(base, list(...)))
}

test_that("recipient strings resolve to one or several ISO3 codes, regions to none", {
  expect_equal(oci_recipient_iso3("Kenya"), "KEN")
  expect_equal(oci_recipient_iso3("Guinea,Mali"), c("GIN", "MLI"))
  expect_length(oci_recipient_iso3("Southern Africa"), 0)
  expect_length(oci_recipient_iso3("Kenya,Southern Africa"), 0)
  expect_length(oci_recipient_iso3(""), 0)
})

test_that("cleaning keeps international rows, splits multi-country deals and relabels KfW IPEX-Bank", {
  raw <- data.frame(
    date = "1/1/2023", FY = c("2022", "2022", "2023"), amountUSD = c("$100,000,000.00", "$90,000,000.00", "$5.00"),
    mechanism = "Loan", institution = c("KfW IPEX-Bank", "Japan Bank for International Co-operation", "KfW"),
    institutionAbbr = c("KfW", "JBIC", "KfW"), institutionGroup = c("Germany", "Japan", "Germany"),
    institutionKind = "Bilateral", project = c("A", "B", "C"), country = c("Kenya", "Guinea,Mali,Senegal", "Germany"),
    category = "Clean", sector = "Solar", stage = "Electricity Production",
    `International or Domestic` = c("International", "International", "Domestic"), source = "x",
    check.names = FALSE, stringsAsFactors = FALSE
  )
  out <- oci_energy_finance_clean(raw)
  expect_equal(nrow(out), 1 + 3)
  expect_equal(out$institution_abbr[out$project == "A"], "KfW-IPEX")
  expect_equal(out$amount_usd[out$project == "A"], 1e8)
  expect_equal(out$recipient_share[out$project == "B"], rep(1 / 3, 3))
  expect_setequal(out$recipient_iso3[out$project == "B"], c("GIN", "MLI", "SEN"))
})

test_that("the institution and sector/stage tables are well formed", {
  inst <- oci_ref("oci_energy_finance_institutions.csv")
  expect_equal(anyDuplicated(inst[c("institution_group", "institution_abbr")]), 0)
  expect_true(all(inst$source %in% c("oci", "crs")))
  expect_true(all(grepl("^[A-Z]{3}$", inst$lender_iso3)))
  expect_true(all(inst$replaces_crs_measure[!is.na(inst$replaces_crs_measure) & nzchar(inst$replaces_crs_measure)] %in% c("ODA", "OOF", "PSI")))
  # The CRS reporters are not counted twice; the export credit agencies the CRS lacks are counted.
  src <- function(g, a) inst$source[inst$institution_group == g & inst$institution_abbr == a]
  expect_equal(src("Japan", "JICA"), "crs")
  expect_equal(src("United States", "DFC"), "crs")
  expect_equal(src("Japan", "JBIC"), "oci")
  expect_equal(src("United States", "EXIM US"), "oci")
  expect_equal(src("Germany", "KfW-IPEX"), "oci")

  cw <- oci_ref("oci_energy_finance_crosswalk.csv")
  expect_true(all(cw$method %in% c("direct", "pooled", "exclude")))
  keyed <- cw[cw$method != "exclude", ]
  expect_true(all(keyed$tech %in% partnership_strength_headline_techs()))
  expect_true(all(keyed$supply_chain %in% SUPPLY_CHAIN_STAGES))
  methods <- unique(cw[c("sector", "stage", "method")])
  expect_equal(anyDuplicated(methods[c("sector", "stage")]), 0)
  expect_equal(anyDuplicated(keyed[c("sector", "stage", "supply_chain", "tech")]), 0)

  # Six stages (2026-10-27); `stage` stays OCI's own vocabulary. The fuel industry's transport and
  # storage ("midstream") is Deployment infrastructure, as hydrogen transport and storage is.
  stages_of <- function(sector, stage) sort(unique(keyed$supply_chain[keyed$sector == sector & keyed$stage == stage]))
  for (sec in c("Oil", "Natural Gas", "Oil and Gas")) {
    for (st in c("Exploration", "Extraction", "Exploration/Extraction")) expect_equal(stages_of(sec, st), "Extraction", info = paste(sec, st))
    for (st in c("Refining", "Petrochemical")) expect_equal(stages_of(sec, st), "Refining", info = paste(sec, st))
    for (st in c("Transportation", "Storage", "Transmission & Distribution", "Electricity Production")) {
      expect_equal(stages_of(sec, st), "Deployment", info = paste(sec, st))
    }
  }
  expect_equal(stages_of("Oil", "*"), c("Deployment", "Extraction", "Refining"))
  expect_equal(stages_of("Natural Gas", "*"), c("Extraction", "Refining"))
  expect_equal(stages_of("Oil and Gas", "*"), c("Extraction", "Refining"))
  expect_equal(stages_of("Batteries", "Electric Vehicles"), c("Assembly", "Components"))
  expect_equal(stages_of("Batteries", "*"), c("Assembly", "Components", "Deployment"))
  # OCI does not separate mining from processing finance.
  crit <- keyed[keyed$sector == "Critical Minerals", ]
  expect_equal(nrow(crit), 6)
  expect_equal(sort(unique(crit$supply_chain)), c("Extraction", "Refining"))
  expect_true(all(crit$method == "direct"))
  # Every row the old file filed Downstream is Deployment; the exclusions are untouched.
  expect_true(all(keyed$supply_chain[keyed$stage %in% c("CHP", "Point of Use", "Heating")] == "Deployment"))
  expect_equal(sum(cw$method == "exclude"), 5)
  expect_true(all(is.na(cw$supply_chain[cw$method == "exclude"]) | cw$supply_chain[cw$method == "exclude"] == ""))
  expect_false("Recovery" %in% keyed$supply_chain)
})

test_that("OCI amounts keep the counted institutions, mechanisms and stages", {
  s <- partnership_strength_development_aid_settings()
  oci <- dplyr::bind_rows(
    oci_row(),                                                                  # JBIC solar loan: counted
    oci_row(mechanism = "Guarantee"),                                           # guarantee: left out
    oci_row(institution_group = "Japan", institution_abbr = "JICA", institution_kind = "Bilateral"),  # in the CRS
    oci_row(institution_group = "China", institution_abbr = "Chexim"),          # excluded lender country
    oci_row(institution_group = "Korea", institution_abbr = "Kexim", sector = "Natural Gas", stage = "Unclear or mixed", amount_usd = 2e8),
    oci_row(sector = "Mixed or unclear - Other", stage = "Electric Vehicles"),  # metro: excluded
    oci_row(sector = "Hydro - Large"),                                          # not an index technology
    oci_row(fy = 2015L),                                                        # outside the window
    oci_row(institution_group = "Narnia", institution_abbr = "NEB")             # unlisted
  )
  expect_warning(
    parts <- partnership_strength_oci_amounts(oci, oci_ref("oci_energy_finance_institutions.csv"), oci_ref("oci_energy_finance_crosswalk.csv"), s),
    "Narnia NEB"
  )
  a <- parts$amounts
  expect_equal(nrow(a), 2)
  expect_equal(a$amount[a$purpose_code == "oci|solar|*"], 100)
  expect_equal(a$amount[a$purpose_code == "oci|natural gas|*"], 200)
  expect_equal(parts$replaces, tibble::tibble(donor_iso3 = "KOR", measure = "OOF"))
})

test_that("OCI finance adds to the CRS cells, replaces Korea's CRS OOF and leaves the cell basis to the CRS", {
  crs <- tibble::tibble(
    year = 2022L,
    donor_iso3 = c("USA", "KOR", "KOR"),
    recipient_iso3 = c("IND", "IND", "VNM"),
    purpose_code = c(23230L, 23230L, 23230L),
    measure = c("ODA", "OOF", "ODA"),
    usd_commitment = 0,
    usd_disbursement = c(10, 50, 4)
  )
  oci <- dplyr::bind_rows(
    oci_row(amount_usd = 6e7),                                                   # JBIC solar to India: 60
    oci_row(institution_group = "Korea", institution_abbr = "Kexim", recipient_iso3 = "VNM", amount_usd = 2e7),
    oci_row(sector = "Batteries", stage = "Storage", recipient_iso3 = "VNM", amount_usd = 5e7)
  )
  gdp <- tibble::tibble(iso3c = c("IND", "VNM"), year = 2022L, NY.GDP.MKTP.CD = c(3e12, 4e11))
  cells <- tibble::tibble(tech = c("Solar", "Batteries"), supply_chain = "Deployment")
  s <- partnership_strength_development_aid_settings(list(years = c(2022, 2022)))
  out <- partnership_strength_build_oecd_tech(crs, read.csv(file.path(repo_root, "data", "reference", "oecd_crs_purpose_crosswalk.csv")),
                                              gdp, cells, s, oci = oci,
                                              oci_institutions = oci_ref("oci_energy_finance_institutions.csv"),
                                              oci_crosswalk = oci_ref("oci_energy_finance_crosswalk.csv"))
  get <- function(iso, tech, col = "aid") out[[col]][out$iso3c == iso & out$tech == tech]
  # India: US ODA 10 + JBIC 60; Korea's CRS OOF (50) gives way to OCI's KEXIM.
  expect_equal(get("IND", "Solar"), 70)
  expect_equal(get("VNM", "Solar"), 4 + 20)
  # Batteries Deployment has no CRS code: it stays a stage-proxy cell, and OCI's storage deal feeds
  # the proxy. With no direct battery finance to weight it, the deal splits equally between
  # Batteries Components, Assembly and Deployment (50 / 3 each).
  expect_equal(get("VNM", "Batteries", "aid_basis"), "stage")
  expect_equal(get("VNM", "Batteries"), 4 + 20 + 50 / 3)

  s_off <- s
  s_off$energy_finance$include <- FALSE
  off <- partnership_strength_build_oecd_tech(crs, read.csv(file.path(repo_root, "data", "reference", "oecd_crs_purpose_crosswalk.csv")),
                                              gdp, cells, s_off, oci = oci,
                                              oci_institutions = oci_ref("oci_energy_finance_institutions.csv"),
                                              oci_crosswalk = oci_ref("oci_energy_finance_crosswalk.csv"))
  expect_equal(off$aid[off$iso3c == "IND" & off$tech == "Solar"], 60)
})

test_that("each source's cross-cutting finance follows its own mix, and only energy CRS rows are replaced", {
  crs <- tibble::tibble(
    year = 2022L,
    donor_iso3 = c("USA", "USA", "KOR"),
    recipient_iso3 = c("BRA", "BRA", "IND"),
    purpose_code = c(32264L, 32210L, 32171L),  # non-ferrous metals, mining policy, engineering
    measure = c("ODA", "ODA", "OOF"),
    usd_commitment = 0,
    usd_disbursement = c(9, 3, 70)
  )
  oci <- dplyr::bind_rows(
    oci_row(recipient_iso3 = "BRA", sector = "Oil and Gas", stage = "Extraction", amount_usd = 1e8),  # JBIC upstream: 50 oil, 50 gas
    oci_row(institution_group = "Korea", institution_abbr = "Kexim", recipient_iso3 = "VNM", amount_usd = 2e7)
  )
  gdp <- tibble::tibble(iso3c = c("BRA", "IND", "VNM"), year = 2022L, NY.GDP.MKTP.CD = c(2e12, 3e12, 4e11))
  cells <- tibble::tibble(tech = c("Batteries", "Oil", "Solar"), supply_chain = c("Extraction", "Extraction", "Components"))
  out <- partnership_strength_build_oecd_tech(crs, read.csv(file.path(repo_root, "data", "reference", "oecd_crs_purpose_crosswalk.csv")),
                                              gdp, cells, partnership_strength_development_aid_settings(list(years = c(2022, 2022))),
                                              oci = oci, oci_institutions = oci_ref("oci_energy_finance_institutions.csv"),
                                              oci_crosswalk = oci_ref("oci_energy_finance_crosswalk.csv"))
  get <- function(iso, tech) out$aid[out$iso3c == iso & out$tech == tech]
  # Brazil's mining-policy aid (3) follows its CRS mineral projects (batteries, grids, EVs: 1 each),
  # not JBIC's oil and gas loan.
  expect_equal(get("BRA", "Batteries"), 9 / 3 + 1)
  expect_equal(get("BRA", "Oil"), 50)
  # KEXIM's OCI rows replace Korea's energy OOF only: its engineering (industry) OOF stays, split
  # over seven technologies at Components and at Assembly (70 / 14).
  expect_equal(get("IND", "Solar"), 5)
})

test_that("the configured energy finance settings resolve", {
  cfg <- yaml::read_yaml(file.path(repo_root, "config", "partnership.yml"))$development_aid
  ef <- partnership_strength_development_aid_settings(cfg)$energy_finance
  expect_true(ef$include)
  expect_setequal(ef$mechanisms, c("Loan", "Equity", "Grant"))
  expect_equal(ef$exclude_lender_countries, "CHN")
})
