# Development partnerships (R/dashboard/country/country_devpart.R, config/development_partnerships.yml,
# scripts/93_export_development_partnerships.R): the donor and recipient tests, the routes, the aid evidence
# (missing stays missing; CRS stages; Components/Assembly shared codes; the Recovery proxy), the order and the
# Country files, on a small synthetic world; and the Australia pilot on the published export when present.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
for (f in c("R/dashboard/country/country_util.R", "R/strategic_position/pathway.R", "R/dashboard/country/country_brief.R",
            "R/dashboard/country/country_devpart.R")) source(file.path(repo_root, f))
dcfg <- yaml::read_yaml(file.path(repo_root, "config", "development_partnerships.yml"))

# A step row as country_brief_core_cells() returns it, with its posture detail fields.
step_row <- function(iso3, tech, stage, choice, need = 0.5, prize = 0.8, cp = 0.3, ev = "high", conf = "high", fl = NA, pw = NA,
                     delivery = NA, aib_status = NA, trusted_alt = NA, basis = NA) {
  data.frame(iso3 = iso3, tech = tech, stage = stage, choice = choice, need = need, prize = prize, cp = cp, ev = ev, conf = conf, fl = fl,
             pw = if (is.na(pw)) (if (choice == "catch_up") "yes" else "no") else pw, delivery = delivery, aib_status = aib_status,
             trusted_alt = trusted_alt, security_rationale = NA_character_, pathway_basis = basis, partner_evidence = NA_character_,
             stringsAsFactors = FALSE)
}
anchor_ok <- "direct_supply_share;operating_capacity;feasibility;input_cost"

world <- function(rows) {
  cells <- rows[c("iso3", "tech", "stage", "choice", "need", "prize", "cp", "ev", "conf", "fl", "pw")]
  dtab <- rows[c("iso3", "tech", "stage", "delivery", "aib_status", "trusted_alt", "security_rationale", "pathway_basis", "partner_evidence")]
  income <- c(DON = "High income", OUT = "High income", RIC = "High income", MID = "Upper middle income", LOW = "Low income",
              BAD = "Lower middle income", PRE = "Upper middle income", GRN = "Lower middle income", THN = "Lower middle income")
  tier <- c(DON = "core", OUT = "none", RIC = "partner", MID = "none", LOW = "none", BAD = "none", PRE = "none", GRN = "none", THN = "none")
  screen <- data.frame(iso3 = names(income), bloc = c("U.S.-leaning", "China-leaning", "U.S.-leaning", "Nonaligned", "Nonaligned", "China-leaning", "Nonaligned", "Nonaligned", "Nonaligned"),
                       in_demand_scope = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
                       screened = c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE), screen_reason = NA_character_, stringsAsFactors = FALSE)
  steps <- country_devpart_steps(cells, dtab, income, tier, screen)
  nodes <- data.frame(key = c("wind-assembly", "batteries-refining"), node = c("turbines", "li-chem"), label = c("Turbines", "Lithium chemicals"),
                      cls = c("BUILD", "BUILD"), workstream = NA, compact = NA, compact_name = NA, hosts = c(NA, "PRE"), dev_hosts = NA, stringsAsFactors = FALSE)
  steps <- country_devpart_donor_signals(steps, nodes, list(need = 0.67), dcfg)
  steps <- country_devpart_recipient_test(steps, dcfg)
  list(steps = steps, nodes = nodes)
}
crs <- data.frame(year = 2022L, donor_iso3 = "DON", recipient_iso3 = c("MID", "MID", "MID", "LOW"), purpose_code = c("23240", "32171", "32120", "23110"),
                  measure = "ODA", usd_disbursement = c(5, 2, 1, 3), usd_commitment = 0, stringsAsFactors = FALSE)
crosswalk <- read.csv(file.path(repo_root, "data", "reference", "oecd_crs_purpose_crosswalk.csv"), stringsAsFactors = FALSE, colClasses = c(purpose_code = "character"))
rel_of <- function(dp = data.frame(partner_iso = c("MID", "LOW"), development = c(0.9, 0.1))) {
  list(aid = country_devpart_aid(crs, crosswalk, 2020:2024, "disbursement", dcfg), oci = data.frame(lender = character(), recipient = character(), tech = character(), stage = character(), usd_m = numeric()),
       oci_lenders = character(), dip = data.frame(reporter = "DON", counterpart = "MID", value = 12, conduit = "none", basis = "reported", stringsAsFactors = FALSE),
       cmfdi = data.frame(home = character(), dest = character(), tech = character(), n = integer(), usd_m = numeric()), dp = dp, flows = NULL, roles = list(), compact_map = list())
}
rows <- rbind(
  # the donor: Partner where allied supply falls short (wind assembly), a Watch with nothing else (coal assembly),
  # high need and a BUILD node (battery refining), Partner at Recovery
  step_row("DON", "Wind", "Assembly", "partner", need = 0.85, aib_status = "gap", delivery = "allied"),
  step_row("DON", "Coal", "Assembly", "watch", need = 0.3),
  step_row("DON", "Batteries", "Refining", "catch_up", need = 0.8, basis = anchor_ok),
  step_row("DON", "Batteries", "Recovery", "partner", need = 0.9),
  # a high-income economy outside the AIB demand scope, with the same need: no donor view
  step_row("OUT", "Wind", "Assembly", "partner", need = 0.85, aib_status = "gap"),
  # a high-income Catch Up: never a recipient
  step_row("RIC", "Wind", "Assembly", "catch_up", basis = anchor_ok),
  # recipients: a credible Catch Up (MID), a credible Catch Up at Recovery (LOW), a screened one (BAD), a Preserve
  # (PRE), a greenfield without an anchor (GRN), a thin anchor (THN)
  step_row("MID", "Wind", "Assembly", "catch_up", basis = anchor_ok),
  step_row("LOW", "Batteries", "Recovery", "catch_up", basis = anchor_ok),
  step_row("BAD", "Wind", "Assembly", "catch_up", basis = anchor_ok),
  step_row("PRE", "Wind", "Assembly", "preserve", cp = 0.9),
  step_row("GRN", "Wind", "Assembly", "catch_up", pw = "no", basis = "feasibility;input_cost;no_anchor"),
  step_row("THN", "Wind", "Assembly", "catch_up", basis = "adjacent_stage_share;adjacent_stage_relative;learning"),
  step_row("MID", "Coal", "Assembly", "catch_up", basis = anchor_ok),
  step_row("LOW", "Batteries", "Refining", "catch_up", basis = anchor_ok)
)
w <- world(rows)
pairs <- country_devpart_pairs(w$steps, w$nodes, rel_of(), dcfg)
pair <- function(d, r, tech, stage) pairs[pairs$donor_iso3 == d & pairs$recipient_iso3 == r & pairs$technology == tech & pairs$stage == stage, , drop = FALSE]

test_that("pairs run from a high-income donor in the AIB scope to non-high-income recipients only", {
  # donors: high income and in the AIB demand scope (RIC's Catch Up step is a donor reason through its BUILD
  # node, an allied-only reason, tier 5); OUT has the same need but sits outside the scope
  expect_setequal(unique(pairs$donor_iso3), c("DON", "RIC"))
  expect_equal(unique(pairs$donor_tier[pairs$donor_iso3 == "RIC"]), 5L)
  expect_false(any(pairs$recipient_iso3 %in% c("DON", "OUT", "RIC")))
  expect_equal(country_devpart_perspective(w$steps, "OUT", dcfg)$kind, "none")
  expect_equal(country_devpart_perspective(w$steps, "DON", dcfg)$kind, "donor")
  expect_equal(country_devpart_perspective(w$steps, "MID", dcfg)$kind, "recipient")
})

test_that("Partner with an AIB shortfall makes a candidate; Watch alone does not", {
  s <- w$steps[w$steps$iso3 == "DON", ]
  expect_equal(s$donor_tier[s$tech == "Wind"], 1L)
  expect_match(s$signals[s$tech == "Wind"], "partner")
  expect_match(s$signals[s$tech == "Wind"], "aib_shortfall")
  expect_true(is.na(s$donor_tier[s$tech == "Coal"]))
  expect_equal(pair("DON", "MID", "Wind", "Assembly")$route, "development")
  expect_equal(nrow(pair("DON", "MID", "Coal", "Assembly")), 0L)
})

test_that("a recipient needs a valid anchor and corroborated pathway, inside the trust screen", {
  expect_equal(pair("DON", "GRN", "Wind", "Assembly")$route, "excluded")
  expect_true(pair("DON", "GRN", "Wind", "Assembly")$recipient_fail %in% c("pathway", "anchor"))
  # a pathway corroborated by context signals only is a review case, never a recommendation
  thn <- pair("DON", "THN", "Wind", "Assembly")
  expect_equal(thn$route, "review")
  expect_equal(thn$recipient_fail, "thin_anchor")
  bad <- pair("DON", "BAD", "Wind", "Assembly")
  expect_equal(bad$route, "excluded")
  expect_equal(bad$recipient_fail, "trust")
})

test_that("an already competitive or capacity-hosting recipient gets the commercial route, not grants", {
  pre <- pair("DON", "PRE", "Wind", "Assembly")
  expect_equal(pre$route, "commercial")
  expect_false(grepl("project preparation|technical assistance|grant", pre$recommended_tool_family))
  expect_match(pre$recommended_tool_family, "offtake")
  # an AIB capacity host of the step's BUILD node (here PRE hosts lithium chemicals) is commercial too
  expect_true(all(pairs$route[pairs$recipient_iso3 == "PRE" & pairs$technology == "Batteries"] %in% "commercial"))
})

test_that("tools follow the stage, the constraint and the income group", {
  mid <- pair("DON", "MID", "Wind", "Assembly")
  expect_match(mid$recommended_tool_family, "^lead with development finance")  # upper middle income
  low <- pair("DON", "LOW", "Batteries", "Refining")
  expect_false(grepl("^lead with", low$recommended_tool_family))
  expect_match(low$recommended_tool_family, "then, at scale, offtake")
})

test_that("missing aid stays missing, CRS stages are respected and their limits labelled", {
  mid <- pair("DON", "MID", "Wind", "Assembly")
  # 23240 (wind) is Deployment aid, never assembly aid; 32171 (engineering) names wind at Assembly with other technologies
  expect_equal(mid$oda_status, "recorded")
  expect_equal(mid$oda_specificity, "shared_technology")
  expect_true(mid$oda_components_assembly_shared)
  expect_match(mid$caveats, "do not separate Components from Assembly")
  expect_equal(mid$current_oda, 3)  # 32171 (2) and the pooled industrial development code 32120 (1); not the wind-power 5
  expect_equal(mid$imf_dip_tie, 12)
  # Recovery: no CRS code; the Extraction + Refining aid is a flagged proxy
  low <- pair("DON", "LOW", "Batteries", "Recovery")
  expect_true(is.na(low$current_oda))
  expect_true(low$oda_proxy_flag)
  expect_match(low$caveats, "no CRS purpose code funds recycling")
  # a donor that reports no purpose-coded aid: NA, never zero
  rel2 <- rel_of()
  rel2$aid$donors <- character()
  p2 <- country_devpart_pairs(w$steps, w$nodes, rel2, dcfg)
  expect_true(all(is.na(p2$current_oda)))
  expect_true(all(p2$oda_status == "not_reporting"))
  # no direct-investment position reported: NA
  expect_true(is.na(pair("DON", "LOW", "Batteries", "Refining")$imf_dip_tie))
})

test_that("legacy Development Potential never changes a pair, its route or the order", {
  p_hi <- country_devpart_pairs(w$steps, w$nodes, rel_of(data.frame(partner_iso = c("MID", "LOW"), development = c(0.01, 0.99))), dcfg)
  keep <- setdiff(names(pairs), c("development_potential_legacy", "rationale"))
  expect_identical(pairs[keep], p_hi[keep])
})

test_that("the Country files flip between the donor and the recipient perspective", {
  aid <- rel_of()$aid
  meta <- list(created = "t", aid_years = c(2020, 2024), aid_flow = "disbursement", dip_years = c(2020, 2024), need_bar = 0.67, sources = "s")
  don <- country_devpart_country_file("DON", w$steps, pairs, aid, dcfg, meta)
  expect_equal(don$perspective, "donor")
  expect_true(all(unlist(don$pairs$donor_iso3) == "DON"))
  expect_false("excluded" %in% don$pairs$route)
  expect_true(is.list(don$aid))
  rec <- country_devpart_country_file("MID", w$steps, pairs, aid, dcfg, meta)
  expect_equal(rec$perspective, "recipient")
  expect_true(all(unlist(rec$pairs$recipient_iso3) == "MID"))
  expect_true("wind-assembly" %in% rec$steps$key)
  out <- country_devpart_country_file("OUT", w$steps, pairs, aid, dcfg, meta)
  expect_equal(out$perspective, "none")
  # array fields stay arrays in JSON even with one row
  js <- as.character(jsonlite::toJSON(rec, auto_unbox = TRUE, na = "null", null = "null"))
  expect_match(js, '"route":\\[')
})

test_that("the Australia pilot export holds the reasons and never recommends a screened or high-income partner", {
  f <- file.path(repo_root, "data", "processed", "development_partnerships", "development_partnerships.rds")
  skip_if_not(file.exists(f), "no development-partnership export")
  p <- readRDS(f)
  a <- p[p$donor_iso3 == "AUS" & p$route == "development", , drop = FALSE]
  expect_gt(nrow(a), 0)
  expect_false(any(a$recipient_income_group == "High income"))
  expect_false(any(a$recipient_screened %in% TRUE))
  expect_true(all(!is.na(a$catch_up_anchor_type)))
  expect_false(any(a$thin_anchor))
  # rare-earth refining: the battery-metal plant anchors do not establish the rare-earth capability
  ev <- p[p$donor_iso3 == "AUS" & p$key == "electric-vehicles-refining", , drop = FALSE]
  expect_false("development" %in% ev$route)
})

test_that("a constraint is raised only where its input is observed", {
  basis <- "direct_supply_share;feasibility"
  none <- country_devpart_pathway(basis, dcfg)
  expect_false(any(c("no_capacity", "input_costs") %in% none$constraints))
  expect_true(all(c("no_capacity", "input_costs") %in% none$unobserved))
  zero <- country_devpart_pathway(basis, dcfg, list(pw_operating_share = 0, pw_committed_share = 0, pw_input_cost_pct = 0.4))
  expect_true(all(c("no_capacity", "input_costs", "trade_only") %in% zero$constraints))
  expect_length(zero$unobserved, 0)
  small <- country_devpart_pathway(basis, dcfg, list(pw_operating_share = 0.001, pw_committed_share = 0, pw_input_cost_pct = 0.4))
  expect_true("small_capacity" %in% small$constraints)
  expect_false("no_capacity" %in% small$constraints)
  # one share unobserved and the other 0: not observed, never "no plant"
  half <- country_devpart_pathway(basis, dcfg, list(pw_operating_share = NA, pw_committed_share = 0))
  expect_false("no_capacity" %in% half$constraints)
  expect_true("no_capacity" %in% half$unobserved)
})

# a second world: Preserve review tests, a donor review call, capacity hosts, Secure donors, capability checks,
# CM-FDI and OCI coverage
rows2 <- rbind(
  step_row("DON", "Wind", "Assembly", "partner", need = 0.85, aib_status = "gap"),
  step_row("DON", "Solar", "Components", "partner", need = 0.85, conf = "low", aib_status = "gap"),
  step_row("DON", "Electric Grid", "Components", "secure", need = 0.5, delivery = "allied"),
  step_row("DON", "Gas", "Components", "secure", need = 0.5, delivery = "domestic", trusted_alt = 0.2),
  step_row("DON", "Batteries", "Refining", "partner", need = 0.8),
  step_row("DON", "Nuclear", "Components", "partner", need = 0.8),
  step_row("PRE", "Wind", "Assembly", "preserve", cp = 0.9, conf = "low"),
  step_row("MID", "Wind", "Assembly", "preserve", cp = 0.9),
  step_row("MID", "Solar", "Components", "catch_up", basis = anchor_ok),
  step_row("MID", "Electric Grid", "Components", "catch_up", basis = anchor_ok),
  step_row("MID", "Gas", "Components", "catch_up", basis = anchor_ok),
  step_row("PRE", "Batteries", "Refining", "catch_up", basis = anchor_ok),
  step_row("LOW", "Batteries", "Refining", "catch_up", basis = "operating_capacity;feasibility;input_cost"),
  step_row("LOW", "Nuclear", "Components", "catch_up", basis = anchor_ok)
)
w2 <- world(rows2)
rel2 <- rel_of()
rel2$cmfdi <- country_devpart_cmfdi(data.frame(home_iso3 = c("DON", "DON"), destination_iso3 = c("MID", "MID"), tech = c("Solar", "Solar"),
                                               project_id = c("a", "b"), capital_usd_m = c(NA, NA), stages = "Components", index_use = c(TRUE, FALSE),
                                               status = "Operating", stringsAsFactors = FALSE))
rel2$oci_lenders <- "DON"
rel2$oci_steps <- "Solar|Components"
p2 <- country_devpart_pairs(w2$steps, w2$nodes, rel2, dcfg)
pair2 <- function(d, r, tech, stage) p2[p2$donor_iso3 == d & p2$recipient_iso3 == r & p2$technology == tech & p2$stage == stage, , drop = FALSE]

test_that("a Preserve takes the evidence tests, and the rationale follows the route", {
  low <- pair2("DON", "PRE", "Wind", "Assembly")
  expect_equal(low$route, "review")
  expect_equal(low$recipient_fail, "confidence")
  expect_false(grepl("already Preserves", low$rationale))
  expect_equal(pair2("DON", "MID", "Wind", "Assembly")$route, "commercial")
})

test_that("a donor's review call keeps only its allied reasons", {
  s <- pair2("DON", "MID", "Solar", "Components")
  expect_true(s$donor_national_dropped)
  expect_equal(s$donor_tier, 5L)
  expect_equal(s$donor_signals, "aib_shortfall")
  expect_false(s$donor_review)
  expect_equal(s$route, "development")
})

test_that("Secure with allied delivery is a donor reason; Secure with domestic delivery and trusted alternatives is not", {
  s <- w2$steps[w2$steps$iso3 == "DON", ]
  expect_equal(s$donor_tier[s$tech == "Electric Grid"], 3L)
  expect_equal(pair2("DON", "MID", "Electric Grid", "Components")$route, "development")
  expect_true(is.na(s$donor_tier[s$tech == "Gas"]))
  expect_equal(nrow(pair2("DON", "MID", "Gas", "Components")), 0L)
})

test_that("an AIB capacity host of the step's BUILD node is commercial, with commercial tools", {
  h <- pair2("DON", "PRE", "Batteries", "Refining")
  expect_equal(nrow(h), 1L)
  expect_true(h$aib_capacity_host)
  expect_equal(h$route, "commercial")
  expect_match(h$recommended_tool_family, "offtake")
})

test_that("capability checks send a different-product anchor to review", {
  # Batteries Refining: a capacity-only anchor (mines in the capacity data) does not establish refining
  b <- pair2("DON", "LOW", "Batteries", "Refining")
  expect_equal(b$route, "review")
  expect_equal(b$recipient_fail, "capability")
  expect_match(b$caveats, "critical-minerals mines")
  # Nuclear Components: the data cannot tell reactor parts from boiler parts
  expect_equal(pair2("DON", "LOW", "Nuclear", "Components")$route, "review")
})

test_that("coverage gaps stay missing: CM-FDI, OCI and proxy-coded aid", {
  s <- pair2("DON", "MID", "Solar", "Components")
  expect_equal(s$cm_fdi_projects, 1L)               # the index_use project only
  expect_true(is.na(s$cm_fdi_usd_m))                 # capital undisclosed: NA, not 0
  expect_equal(s$oci_finance, 0)                     # a covered step with no finance
  expect_true(is.na(pair2("DON", "MID", "Electric Grid", "Components")$cm_fdi_projects))  # tech outside CM-FDI
  expect_true(is.na(pair2("DON", "MID", "Electric Grid", "Components")$oci_finance))      # step outside the OCI crosswalk
  ap <- country_devpart_aid_pair(list(donors = "DON", total = data.frame(donor_iso3 = "DON", recipient_iso3 = "MID", oda_mapped = 4),
                                      tech = data.frame(donor_iso3 = "DON", recipient_iso3 = "MID", tech = "Electric Vehicles", stage = "Assembly",
                                                        oda = 4, other = 0, named = 0, shared = 0, proxy = 3, pooled = 1, codes = "32172;32120")),
                                 "DON", "MID", "Electric Vehicles", "Assembly")
  expect_equal(ap$specificity, "proxy")
  pp <- data.frame(current_oda = c(4, 4, 0), oda_named = c(0.1, 3, 0), oda_shared = 0, oda_proxy = c(3, 0, 0), oda_pooled = c(0.9, 1, 0),
                   oda_specificity = c("technology", "technology", NA))
  expect_equal(country_devpart_specificity_ok(pp), c(FALSE, TRUE, TRUE))
})
