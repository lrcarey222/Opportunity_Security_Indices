# OECD CRS by purpose code: the extract's scope rules (R/utils/oecd_crs.R), the purpose crosswalk
# (data/reference/oecd_crs_purpose_crosswalk.csv) and the Development Potential aid builder
# (partnership_strength_build_oecd_tech(), stronger_development.R). Offline: the fetcher's
# download is not exercised.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "oecd_crs.R"))
source(file.path(repo_root, "R", "utils", "supply_chain_stages.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "partnership_strength_helpers.R"))
source(file.path(repo_root, "R", "themes", "partnership_strength", "stronger_development.R"))

crs_crosswalk <- function() {
  read.csv(file.path(repo_root, "data", "reference", "oecd_crs_purpose_crosswalk.csv"))
}

test_that("the extract's scope keeps energy, industry and mining purpose codes only", {
  expect_equal(
    oecd_crs_in_scope_purpose(c(23110, 23642, 32110, 32268, 32310, 15110, 24010, NA)),
    c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )
})

test_that("donor and recipient code lists keep countries with an ISO3 code", {
  # The Donor sheet as readxl reads it without names: side-by-side blocks under a title row.
  donor_sheet <- data.frame(
    a = c("List of DAC members", "DonorCode", "302", "701"),
    b = c(NA, "Data Explorer code", "USA", "JPN"),
    c = c(NA, "DonorNameE", "United States", "Japan"),
    d = c(NA, NA, NA, NA),
    e = c("List of multilateral donors", "Donor code", "905", "918"),
    f = c(NA, "Data Explorer code", "5WB002", "4EU001"),
    g = c(NA, "Donor name (EN)", "International Development Association", "EU Institutions"),
    h = c(NA, NA, NA, NA),
    i = c("List of non-DAC donors", "DonorCode", "55", NA),
    j = c(NA, "Data Explorer code", "TUR", NA),
    k = c(NA, "Donor name (EN)", "Turkiye", NA),
    stringsAsFactors = FALSE
  )
  donors <- oecd_crs_donor_codes(donor_sheet)
  expect_equal(donors$donor_code, c(302L, 701L, 55L))
  expect_equal(donors$donor_iso3, c("USA", "JPN", "TUR"))

  recipient_sheet <- data.frame(
    `Recipient code` = c(645, 998, 248),
    `Recipient name (EN)` = c("India", "Developing countries, unspecified", "Kenya"),
    ISOcode = c("IND", NA, "KEN"),
    check.names = FALSE
  )
  recipients <- oecd_crs_recipient_codes(recipient_sheet)
  expect_equal(recipients$recipient_iso3, c("IND", "KEN"))
})

test_that("a year far below its predecessors is incomplete, and the price base year is found", {
  expect_equal(
    oecd_crs_complete_years(2019:2025, c(300, 310, 400, 373, 393, 370, 12)),
    2019:2024
  )
  year <- rep(2023:2024, each = 3)
  nominal <- c(10, 20, 30, 10, 20, 30)
  deflated <- c(10.4, 20.9, 31.2, 10, 20, 30)
  expect_equal(oecd_crs_price_base_year(year, nominal, deflated), 2024L)
  expect_true(is.na(oecd_crs_price_base_year(year, nominal, nominal * 1.1)))
})

test_that("the extract keeps official bilateral country-to-country flows and sums them", {
  donors <- tibble::tibble(donor_code = c(302L, 701L), donor_iso3 = c("USA", "JPN"), donor = c("United States", "Japan"))
  recipients <- tibble::tibble(recipient_code = c(645L, 248L), recipient_iso3 = c("IND", "KEN"), recipient = c("India", "Kenya"))
  activities <- tibble::tibble(
    year = c(2023, 2023, 2023, 2023, 2023, 2023, 2023, 2022),
    donor_code = c(302, 302, 302, 905, 701, 302, 302, 302),
    recipient_code = c(645, 645, 645, 645, 248, 998, 645, 645),
    # ODA twice (summed), OOF, ODA from a multilateral, PSI, ODA to a regional code, export credit, an out-of-window year
    category = c(10, 10, 21, 10, 60, 10, 22, 10),
    bi_multi = c(1, 1, 1, 4, 1, 1, 1, 1),
    purpose_code = c(23230, 23230, 23230, 23230, 32264, 23230, 23230, 23230),
    usd_commitment_defl = 1,
    usd_disbursement_defl = c(2, 3, 4, 100, 5, 100, 100, 100)
  )
  out <- oecd_crs_aggregate(activities, donors, recipients, years = 2023)
  expect_equal(nrow(out), 3)
  expect_equal(out$usd_disbursement[out$donor_iso3 == "USA" & out$measure == "ODA"], 5)
  expect_equal(out$usd_disbursement[out$donor_iso3 == "USA" & out$measure == "OOF"], 4)
  expect_equal(out$usd_disbursement[out$donor_iso3 == "JPN"], 5)
  expect_equal(out$measure[out$donor_iso3 == "JPN"], "PSI")
})

test_that("the purpose crosswalk is well formed and maps the codes it names", {
  cw <- crs_crosswalk()
  expect_setequal(names(cw), c("purpose_code", "purpose_name", "supply_chain", "tech", "method", "note"))
  expect_true(all(cw$method %in% c("direct", "pooled")))
  expect_true(all(cw$supply_chain %in% SUPPLY_CHAIN_STAGES))
  expect_true(all(cw$tech %in% partnership_strength_headline_techs()))
  expect_true(all(oecd_crs_in_scope_purpose(cw$purpose_code)))
  expect_equal(anyDuplicated(cw[c("purpose_code", "supply_chain", "tech")]), 0)
  methods <- unique(cw[c("purpose_code", "method")])
  expect_equal(anyDuplicated(methods$purpose_code), 0)

  techs_of <- function(code) sort(unique(cw$tech[cw$purpose_code == code]))
  expect_equal(techs_of(23230), "Solar")
  expect_equal(techs_of(23510), "Nuclear")
  expect_equal(techs_of(23630), "Electric Grid")
  expect_equal(techs_of(32262), c("Gas", "Oil"))
  expect_equal(techs_of(32172), "Electric Vehicles")
  # Hydro, biofuel and non-energy industry are outside the index's technologies.
  expect_false(any(c(23220, 23270, 32161, 32168) %in% cw$purpose_code))

  # Six stages (2026-10-27): energy codes are Deployment; fossil-fuel, basic-metal and non-ferrous
  # industries Refining; engineering and transport equipment one row per technology on Components
  # and on Assembly; the pooled industrial codes over Refining, Components and Assembly (what the
  # old Midstream covered); mining Extraction. No code funds Recovery.
  stages_of <- function(code) sort(unique(cw$supply_chain[cw$purpose_code == code]))
  code <- as.integer(cw$purpose_code)
  expect_true(all(cw$supply_chain[code %/% 1000L == 23L] == "Deployment"))
  for (c in c(32167, 32169, 32170)) expect_equal(stages_of(c), "Refining", info = c)
  expect_equal(techs_of(32170), c("Batteries", "Electric Grid"))
  for (c in c(32171, 32172)) {
    expect_equal(stages_of(c), c("Assembly", "Components"), info = c)
    rows <- cw[cw$purpose_code == c, ]
    expect_equal(sort(rows$tech[rows$supply_chain == "Components"]), sort(rows$tech[rows$supply_chain == "Assembly"]), info = c)
  }
  for (c in c(32110, 32120, 32182)) {
    expect_equal(stages_of(c), c("Assembly", "Components", "Refining"), info = c)
    expect_equal(nrow(cw[cw$purpose_code == c, ]), 3 * length(techs_of(c)), info = c)
  }
  for (c in c(32261, 32262, 32264, 32268, 32210, 32220)) expect_equal(stages_of(c), "Extraction", info = c)
  expect_false("Recovery" %in% cw$supply_chain)
})

test_that("a stage no crosswalk allocates to takes its roll-up's proxy", {
  cw <- tibble::tribble(
    ~purpose_code, ~supply_chain, ~tech, ~method,
    1L, "Extraction", "Batteries", "direct",
    2L, "Refining", "Batteries", "direct",
    3L, "Extraction", "Coal", "direct",       # fossil only: not in the proxies
    4L, "Deployment", "Solar", "direct"
  )
  cells <- tidyr::expand_grid(tech = c("Batteries", "Solar"), supply_chain = c("Extraction", "Refining", "Deployment", "Recovery"))
  amounts <- tibble::tribble(
    ~recipient_iso3, ~purpose_code, ~amount,
    "AAA", 1L, 30,
    "AAA", 2L, 20,
    "AAA", 3L, 500,
    "AAA", 4L, 7,
    "BBB", 1L, 5
  )
  out <- partnership_strength_allocate_crs_purposes(amounts, cw, cells)
  get <- function(r, t, s, col = "aid") out[[col]][out$recipient_iso3 == r & out$tech == t & out$supply_chain == s]
  # Recovery: Extraction proxy + Refining proxy (the other Upstream stages), coal left out.
  expect_equal(get("AAA", "Batteries", "Recovery"), 30 + 20)
  expect_equal(get("AAA", "Solar", "Recovery"), 30 + 20)
  expect_equal(get("BBB", "Solar", "Recovery"), 5)
  expect_equal(unique(out$aid_basis[out$supply_chain == "Recovery"]), "rollup")
  # The cells of allocated stages are unchanged: a direct code, or the stage's own proxy.
  expect_equal(get("AAA", "Batteries", "Extraction", "aid_basis"), "purpose")
  expect_equal(get("AAA", "Solar", "Refining"), 20)
  expect_equal(get("AAA", "Solar", "Refining", "aid_basis"), "stage")
  expect_equal(get("AAA", "Batteries", "Deployment"), 7)
  # Deployment is the only Downstream stage: a crosswalk without it has no roll-up to borrow.
  no_dep <- partnership_strength_allocate_crs_purposes(amounts, cw[cw$supply_chain != "Deployment", ], cells)
  expect_equal(unique(no_dep$aid_basis[no_dep$supply_chain == "Deployment"]), "stage")
  expect_true(all(no_dep$aid[no_dep$supply_chain == "Deployment"] == 0))

  # "zero" keeps the rule before 2026-10-27: an empty stage's proxy is 0 for every recipient.
  zero <- partnership_strength_allocate_crs_purposes(amounts, cw, cells, empty_stage_proxy = "zero")
  expect_true(all(zero$aid[zero$supply_chain == "Recovery"] == 0))
  expect_equal(unique(zero$aid_basis[zero$supply_chain == "Recovery"]), "stage")
  expect_equal(zero$aid[zero$supply_chain != "Recovery"], out$aid[out$supply_chain != "Recovery"])
  expect_error(partnership_strength_allocate_crs_purposes(amounts, cw, cells, empty_stage_proxy = "mean"), "should be one of")

  # A stage another crosswalk allocates to is not empty here: its proxy is this crosswalk's (0).
  other <- partnership_strength_allocate_crs_purposes(amounts, cw, cells, allocated_stages = SUPPLY_CHAIN_STAGES)
  expect_true(all(other$aid[other$supply_chain == "Recovery"] == 0))
  # Legacy three-stage cells have no roll-up sibling and keep the stage proxy.
  legacy <- partnership_strength_allocate_crs_purposes(amounts, cw, tibble::tibble(tech = "Solar", supply_chain = "Upstream"))
  expect_equal(legacy$aid_basis, c("stage", "stage"))
})

test_that("direct codes split equally and pooled codes follow the recipient's direct mix", {
  cw <- tibble::tribble(
    ~purpose_code, ~supply_chain, ~tech, ~method,
    1L, "Deployment", "Solar", "direct",
    2L, "Deployment", "Wind", "direct",
    3L, "Deployment", "Solar", "direct",
    3L, "Deployment", "Wind", "direct",
    4L, "Deployment", "Coal", "direct",
    9L, "Deployment", "Solar", "pooled",
    9L, "Deployment", "Wind", "pooled",
    9L, "Deployment", "Coal", "pooled"
  )
  cells <- tibble::tibble(tech = c("Solar", "Wind", "Coal", "Batteries"), supply_chain = "Deployment")
  amounts <- tibble::tribble(
    ~recipient_iso3, ~purpose_code, ~amount,
    "AAA", 1L, 30,   # solar
    "AAA", 3L, 20,   # solar + wind, 10 each
    "AAA", 9L, 100,  # policy: AAA's direct mix is solar 40, wind 10
    "BBB", 4L, 50,   # coal only
    "CCC", 9L, 60,   # policy, no direct aid: the global mix (solar 40, wind 10, coal 50)
    "DDD", 99L, 5    # a code outside the crosswalk
  )
  out <- partnership_strength_allocate_crs_purposes(amounts, cw, cells)
  get <- function(r, t) out$aid[out$recipient_iso3 == r & out$tech == t]
  expect_equal(get("AAA", "Solar"), 30 + 10 + 100 * 40 / 50)
  expect_equal(get("AAA", "Wind"), 10 + 100 * 10 / 50)
  expect_equal(get("AAA", "Coal"), 0)
  expect_equal(get("CCC", "Solar"), 60 * 40 / 100)
  expect_equal(get("CCC", "Coal"), 60 * 50 / 100)
  # DDD received aid, none of it mapped: observed zeros.
  expect_equal(get("DDD", "Solar"), 0)

  # Batteries has no direct code: it takes the stage proxy, which leaves out coal-only codes.
  expect_equal(unique(out$aid_basis[out$tech == "Batteries"]), "stage")
  expect_equal(get("AAA", "Batteries"), 150)
  expect_equal(get("BBB", "Batteries"), 0)
  expect_equal(get("CCC", "Batteries"), 60)
  expect_equal(unique(out$aid_basis[out$tech != "Batteries"]), "purpose")

  bad <- dplyr::bind_rows(cw, tibble::tibble(purpose_code = 1L, supply_chain = "Deployment", tech = "Wind", method = "pooled"))
  expect_error(partnership_strength_allocate_crs_purposes(amounts, bad, cells), "both direct and pooled")
})

test_that("the aid builder averages the window, divides by mean GDP and ranks within each cell", {
  crs <- tibble::tibble(
    year = c(2019L, 2020L, 2021L, 2021L, 2021L, 2021L),
    donor_iso3 = "USA",
    recipient_iso3 = c("IND", "IND", "IND", "KEN", "KEN", "IND"),
    purpose_code = c(23230L, 23230L, 23230L, 23230L, 23510L, 23510L),
    measure = c("ODA", "ODA", "ODA", "ODA", "ODA", "OOF"),
    usd_commitment = 0,
    usd_disbursement = c(1000, 10, 30, 8, 6, 2)
  )
  gdp <- tibble::tibble(
    iso3c = c("IND", "IND", "KEN", "KEN"),
    year = c(2020L, 2021L, 2020L, 2021L),
    NY.GDP.MKTP.CD = c(3e12, 3.4e12, 1e11, 1.2e11)
  )
  cells <- tibble::tibble(tech = c("Solar", "Nuclear"), supply_chain = "Deployment")
  settings <- partnership_strength_development_aid_settings(list(years = c(2020, 2021), measures = "ODA"))
  out <- partnership_strength_build_oecd_tech(crs, crs_crosswalk(), gdp, cells, settings)

  ind_solar <- out[out$iso3c == "IND" & out$tech == "Solar", ]
  expect_equal(ind_solar$aid, (10 + 30) / 2)
  expect_equal(ind_solar$aid_gdp, 20 / mean(c(3e12, 3.4e12)))
  # The OOF row is outside the measures: India has no nuclear aid, an observed zero.
  expect_equal(out$aid[out$iso3c == "IND" & out$tech == "Nuclear"], 0)
  expect_gt(out$aid_index_supplychain[out$iso3c == "KEN" & out$tech == "Nuclear"],
            out$aid_index_supplychain[out$iso3c == "IND" & out$tech == "Nuclear"])
  expect_gt(out$aid_index_supplychain[out$iso3c == "IND" & out$tech == "Solar"],
            out$aid_index_supplychain[out$iso3c == "KEN" & out$tech == "Solar"])
  expect_equal(anyDuplicated(out[c("iso3c", "tech", "supply_chain")]), 0)

  expect_warning(
    partnership_strength_build_oecd_tech(crs, crs_crosswalk(), gdp, cells,
                                         partnership_strength_development_aid_settings(list(years = c(2021, 2023)))),
    "no rows for 2022, 2023"
  )
  expect_error(partnership_strength_development_aid_settings(list(measures = "Export credits")), "measures")
  expect_error(partnership_strength_development_aid_settings(list(years = c(2024, 2020))), "first <= last")
  expect_error(partnership_strength_development_aid_settings(list(flow = "net")), "flow")
})

test_that("the configured aid settings resolve", {
  cfg <- yaml::read_yaml(file.path(repo_root, "config", "partnership.yml"))$development_aid
  s <- partnership_strength_development_aid_settings(cfg)
  expect_equal(range(s$years), c(2020L, 2024L))
  expect_setequal(s$measures, c("ODA", "OOF", "PSI"))
  expect_equal(s$flow, "disbursement")
  expect_equal(s$empty_stage_proxy, "rollup")
  expect_equal(partnership_strength_development_aid_settings()$empty_stage_proxy, "rollup")
  expect_equal(partnership_strength_development_aid_settings(list(empty_stage_proxy = "Zero"))$empty_stage_proxy, "zero")
  expect_error(partnership_strength_development_aid_settings(list(empty_stage_proxy = "mean")), "rollup or zero")
})

test_that("the aid builder fills Recovery from Extraction and Refining across the CRS and OCI crosswalks", {
  crs <- tibble::tibble(
    year = 2022L, donor_iso3 = "USA", recipient_iso3 = c("IND", "IND", "KEN"),
    purpose_code = c(32264L, 32170L, 32264L),  # non-ferrous mining (Extraction), smelting (Refining)
    measure = "ODA", usd_commitment = 0, usd_disbursement = c(9, 4, 3)
  )
  gdp <- tibble::tibble(iso3c = c("IND", "KEN"), year = 2022L, NY.GDP.MKTP.CD = c(3e12, 1e11))
  cells <- tidyr::expand_grid(tech = c("Batteries", "Solar"), supply_chain = SUPPLY_CHAIN_STAGES)
  s <- partnership_strength_development_aid_settings(list(years = c(2022, 2022), measures = "ODA"))
  out <- partnership_strength_build_oecd_tech(crs, crs_crosswalk(), gdp, cells, s)
  get <- function(iso, tech, stage, col = "aid") out[[col]][out$iso3c == iso & out$tech == tech & out$supply_chain == stage]
  # India: 9 of mining over Batteries, Electric Grid, EVs (Extraction 9 in all); 4 of smelting over
  # Batteries and Electric Grid (Refining 4). Recovery is the sum, 13, in every technology.
  expect_equal(get("IND", "Batteries", "Recovery"), 9 + 4)
  expect_equal(get("IND", "Solar", "Recovery"), 9 + 4)
  expect_equal(get("KEN", "Solar", "Recovery"), 3)
  expect_equal(unique(out$aid_basis[out$supply_chain == "Recovery"]), "rollup")
  # So Recovery ranks recipients (IND above KEN) where a zero proxy would tie them.
  expect_gt(get("IND", "Solar", "Recovery", "aid_index_supplychain"), get("KEN", "Solar", "Recovery", "aid_index_supplychain"))
  s$empty_stage_proxy <- "zero"
  flat <- partnership_strength_build_oecd_tech(crs, crs_crosswalk(), gdp, cells, s)
  rec <- flat[flat$supply_chain == "Recovery", ]
  expect_true(all(rec$aid == 0))
  expect_equal(dplyr::n_distinct(rec$aid_index_supplychain), 1)
})
