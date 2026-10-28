# Guards Revealed Policy Priority (R/strategic_position/policy_priority.R, docs/strategic_position.md
# section 4): how PAMS records reach cells, stock and flow, the within-country attention RCA, the
# combination of PAMS and NIPO, the evidence rows and the CAT country context. Synthetic fixtures only.

pp_repo_root <- function() {
  normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
}

source(file.path(pp_repo_root(), "R", "utils", "supply_chain_stages.R"))
source(file.path(pp_repo_root(), "R", "strategic_position", "sp_helpers.R"))
source(file.path(pp_repo_root(), "R", "strategic_position", "policy_priority.R"))

library(dplyr)

pp_all_techs <- c("Batteries", "Critical Minerals", "Solar", "Wind")
pp_techs <- c("Batteries", "Solar", "Wind")

# One PAMS record as policy_clean holds it: a record with no technology (techs = NULL) is copied onto
# every technology of pp_all_techs, one with no stage onto all six stages, as expand_cross_cutting() does.
pp_record <- function(id, iso3, techs = NULL, stages = NULL, status = "In force", juris = "National",
                      year = 2020L, title = paste("Policy", id), modified = 1.6e12) {
  tg <- tidyr::expand_grid(
    tech = if (is.null(techs)) pp_all_techs else techs,
    supply_chain = if (is.null(stages)) SUPPLY_CHAIN_STAGES else stages
  )
  data.frame(
    policy_id = id, iso3 = iso3, country = iso3, title_text = title, tg,
    tech_crosscut = is.null(techs), sc_crosscut = is.null(stages),
    status_norm = status, jurisdiction_norm = juris, year = as.integer(year),
    tech_source = if (is.null(techs)) "none" else "list", dateModified = modified,
    stringsAsFactors = FALSE
  )
}

# Synthetic NIPO cell table: every tech x non-Deployment stage, the observable cells.
pp_nipo <- function(iso3, values) {
  grid <- tidyr::expand_grid(iso3 = iso3, tech = pp_techs, supply_chain = setdiff(SUPPLY_CHAIN_STAGES, "Deployment"))
  v <- rep_len(values, nrow(grid))
  grid %>% mutate(
    support_stock = v, protection_stock = v / 2, all_stock = v, all_stock_iw = v * 1.5,
    all_flow_1y = v / 4, all_flow_3y = v / 2, all_flow_5y = v,
    n_measures_stock = as.integer(ceiling(v)), fiscal_data_coverage = 0.5
  )
}

pp_cfg <- list(flow_window_years = 3, min_cells_with_records = 3, normalisation = "attention_rca")

# ---- links ----------------------------------------------------------------------------------------

test_that("PAMS records are classified and shared out over the cells they name", {
  pc <- bind_rows(
    pp_record(1, "AAA", "Solar", c("Components", "Assembly")),
    pp_record(2, "AAA", "Wind", "Deployment"),
    pp_record(3, "AAA", "Solar", NULL),
    pp_record(4, "AAA", NULL, "Extraction"),
    pp_record(5, "AAA", NULL, NULL),
    pp_record(6, "AAA", c("Batteries", "Critical Minerals"), "Extraction")
  )
  links <- sp_pams_links(pc, techs = pp_techs)
  by_id <- split(links, links$policy_id)

  expect_equal(by_id[["1"]]$link_class, c("split", "split"))
  expect_equal(by_id[["1"]]$share, c(0.5, 0.5))
  expect_equal(by_id[["2"]]$link_class, "exact")
  expect_equal(by_id[["2"]]$share, 1)
  # copies collapse back to what the record names
  expect_equal(nrow(by_id[["3"]]), 1)
  expect_equal(by_id[["3"]]$link_class, "tech_wide")
  expect_true(is.na(by_id[["3"]]$supply_chain))
  expect_equal(nrow(by_id[["4"]]), 1)
  expect_equal(by_id[["4"]]$link_class, "shared")
  expect_true(is.na(by_id[["4"]]$tech))
  expect_equal(nrow(by_id[["5"]]), 1)
  expect_equal(by_id[["5"]]$link_class, "cross_cutting")
  # k counts the Critical Minerals cell, which is off the headline grid
  expect_equal(by_id[["6"]]$share, c(0.5, 0.5))
  expect_equal(by_id[["6"]]$off_grid[by_id[["6"]]$tech == "Critical Minerals"], TRUE)
})

test_that("a record entered twice is kept once, the latest modified", {
  pc <- bind_rows(
    pp_record(1, "AAA", "Solar", "Deployment", title = "Solar Act", modified = 1.5e12),
    pp_record(2, "AAA", "Solar", "Deployment", title = "solar  act", modified = 1.7e12),
    # same title and year but other links: a different measure
    pp_record(3, "AAA", "Wind", "Deployment", title = "Solar Act"),
    pp_record(4, "AAA", "Batteries", "Assembly"),
    pp_record(5, "AAA", "Wind", "Components")
  )
  links <- sp_pams_links(pc, techs = pp_techs)
  expect_setequal(unique(links$policy_id), c(2, 3, 4, 5))
  expect_equal(nrow(sp_pams_links(pc, techs = pp_techs, dedup = FALSE)), 5)
})

test_that("only the country's own jurisdictions count", {
  pc <- bind_rows(
    pp_record(1, "AAA", "Solar", "Deployment", juris = "International"),
    pp_record(1, "BBB", "Solar", "Deployment", juris = "International"),
    pp_record(2, "AAA", "Wind", "Deployment", juris = "International"),
    pp_record(3, "AAA", "Wind", "Assembly", juris = "Subnational")
  )
  links <- sp_pams_links(pc, techs = pp_techs)
  own <- setNames(links$own_policy, paste(links$policy_id, links$iso3))
  expect_false(own[["1 AAA"]])
  expect_false(own[["1 BBB"]])
  expect_true(own[["2 AAA"]])
  expect_true(own[["3 AAA"]])
  strict <- sp_pams_links(pc, techs = pp_techs, international_single_country = FALSE)
  expect_false(strict$own_policy[strict$policy_id == 2])
  cells <- sp_pams_cells(links, as_of_year = 2025, techs = pp_techs)
  expect_equal(sort(unique(cells$iso3)), "AAA")
  expect_equal(sum(cells$pams_stock), 2)
})

# ---- cells: stock, flow, the unranked classes ---------------------------------------------------------

test_that("stock counts records in force; flow counts any status in the window", {
  pc <- bind_rows(
    pp_record(1, "AAA", "Solar", "Deployment", year = 2025),
    pp_record(2, "AAA", "Solar", "Deployment", year = 2023, status = "Ended"),
    pp_record(3, "AAA", "Solar", "Deployment", year = 2021),
    pp_record(4, "AAA", "Solar", "Deployment", year = 2015),
    pp_record(5, "AAA", "Solar", "Deployment", year = 2027),
    pp_record(6, "AAA", "Solar", "Deployment", year = 2024, status = "Unknown")
  )
  cells <- sp_pams_cells(sp_pams_links(pc, techs = pp_techs), as_of_year = 2025, windows = c(1, 3, 5), techs = pp_techs)
  cell <- cells[cells$tech == "Solar" & cells$supply_chain == "Deployment", ]
  expect_equal(cell$pams_stock, 4) # 1, 3, 4, 5 are in force
  expect_equal(cell$pams_n_stock, 4L)
  expect_equal(cell$pams_tech_list_stock, 4)
  expect_equal(cell$pams_flow_1y, 1) # 2025
  expect_equal(cell$pams_flow_3y, 3) # 2023-2025, the ended and unknown records included
  expect_equal(cell$pams_flow_5y, 4) # 2021-2025; 2027 is after the as-of year
  expect_equal(nrow(cells), length(pp_techs) * 6)
  expect_equal(sum(cells$pams_stock), 4) # zeros everywhere else
})

test_that("cross-cutting, tech-wide and shared records are reported, not attributed", {
  base <- bind_rows(
    pp_record(1, "AAA", "Solar", "Deployment"),
    pp_record(2, "AAA", "Wind", "Components"),
    pp_record(3, "AAA", "Batteries", "Refining"),
    pp_record(4, "AAA", "Wind", c("Components", "Assembly"))
  )
  extra <- bind_rows(
    pp_record(10, "AAA", NULL, NULL),
    pp_record(11, "AAA", NULL, NULL),
    pp_record(12, "AAA", c("Solar", "Wind"), NULL),
    pp_record(13, "AAA", "Solar", NULL),
    pp_record(14, "AAA", NULL, c("Extraction", "Refining"))
  )
  c0 <- sp_pams_cells(sp_pams_links(base, techs = pp_techs), as_of_year = 2025, techs = pp_techs)
  c1 <- sp_pams_cells(sp_pams_links(bind_rows(base, extra), techs = pp_techs), as_of_year = 2025, techs = pp_techs)

  expect_equal(c1$pams_stock, c0$pams_stock)
  expect_equal(unique(c1$pams_cross_cutting_stock), 2)
  tw <- c1 %>% distinct(tech, pams_tech_wide_stock)
  expect_equal(tw$pams_tech_wide_stock[tw$tech == "Solar"], 1.5) # record 13 + half of record 12
  expect_equal(tw$pams_tech_wide_stock[tw$tech == "Wind"], 0.5)
  sh <- c1 %>% distinct(supply_chain, pams_shared_stock)
  expect_equal(sh$pams_shared_stock[sh$supply_chain == "Extraction"], 0.5)
  expect_equal(sh$pams_shared_stock[sh$supply_chain == "Deployment"], 0)

  # and none of them moves the ranking
  p0 <- sp_policy_priority(c0, NULL, pp_cfg, techs = pp_techs)$cells
  p1 <- sp_policy_priority(c1, NULL, pp_cfg, techs = pp_techs)$cells
  expect_equal(p1$pams_priority, p0$pams_priority)
  expect_equal(p1$pams_priority_raw, p0$pams_priority_raw)
})

# ---- within-country priority ----------------------------------------------------------------------

# x = country taste for the technology x a stage factor every country shares (3x at Deployment).
pp_stage_bias <- function() {
  set.seed(7)
  countries <- paste0("C", 1:6)
  taste <- matrix(runif(length(countries) * length(pp_techs), 1, 10), nrow = length(countries),
    dimnames = list(countries, pp_techs))
  stage_factor <- setNames(c(1, 1.2, 1.4, 1.1, 3, 0.8), SUPPLY_CHAIN_STAGES)
  tidyr::expand_grid(iso3 = countries, tech = pp_techs, supply_chain = SUPPLY_CHAIN_STAGES) %>%
    mutate(x = taste[cbind(iso3, tech)] * stage_factor[supply_chain])
}

test_that("percentiles run within the country, over its own observable cells", {
  tbl <- pp_stage_bias()
  # BBB is observed in four cells only; its other rows are not there
  tbl <- bind_rows(tbl, data.frame(iso3 = "BBB", tech = "Solar", supply_chain = SUPPLY_CHAIN_STAGES[1:4], x = c(1, 2, 3, 4)))
  r <- sp_within_country_priority(tbl, "x", min_cells = 3)
  b <- r[r$iso3 == "BBB", ]
  expect_equal(sort(b$priority_raw), (1:4 - 0.5) / 4)
  expect_equal(sort(b$priority), (rank(b$rca) - 0.5)[order(rank(b$rca))] / 4)
  for (cc in unique(r$iso3)) {
    p <- r$priority[r$iso3 == cc]
    expect_equal(mean(p), 0.5, info = cc) # mid-rank percentiles average 0.5 in every country
  }
})

test_that("scaling one country's counts leaves every priority unchanged", {
  tbl <- pp_stage_bias()
  tbl$x <- round(tbl$x * (1 + (seq_len(nrow(tbl)) %% 5)))
  r0 <- sp_within_country_priority(tbl, "x")
  scaled <- tbl
  scaled$x[scaled$iso3 == "C2"] <- scaled$x[scaled$iso3 == "C2"] * 25
  r1 <- sp_within_country_priority(scaled, "x")
  expect_equal(r1$priority, r0$priority)
  expect_equal(r1$priority_raw, r0$priority_raw)
  expect_equal(r1$rca, r0$rca)
})

test_that("the attention RCA removes a stage bias every country shares", {
  tbl <- pp_stage_bias()
  for (b in c("country_mean", "pooled")) {
    r <- sp_within_country_priority(tbl, "x", baseline = b, rca_smoothing = 0)
    by_stage <- r %>% group_by(supply_chain) %>% summarise(rca = mean(priority), raw = mean(priority_raw))
    expect_equal(max(by_stage$rca) - min(by_stage$rca), 0, tolerance = 1e-12, info = b)
    expect_equal(by_stage$supply_chain[which.max(by_stage$raw)], "Deployment", info = b)
    expect_gt(by_stage$raw[by_stage$supply_chain == "Deployment"], 0.8)
  }
  # the default smoothing keeps most of the correction
  s <- sp_within_country_priority(tbl, "x") %>% group_by(supply_chain) %>% summarise(rca = mean(priority), raw = mean(priority_raw))
  dep <- s$supply_chain == "Deployment"
  expect_lt(s$rca[dep] - 0.5, (s$raw[dep] - 0.5) / 2)
  raw <- sp_within_country_priority(tbl, "x", method = "raw")
  expect_equal(raw$priority, raw$priority_raw)
})

test_that("smoothing stops a sliver of a record in a rare cell from topping the country", {
  # every country is busy at Deployment; AAA also has a tenth of a record in a cell nobody else touches
  tbl <- tidyr::expand_grid(iso3 = c("AAA", "BBB", "CCC", "DDD"), tech = pp_techs, supply_chain = SUPPLY_CHAIN_STAGES) %>%
    mutate(x = case_when(supply_chain == "Deployment" ~ 10, supply_chain == "Components" ~ 2, TRUE ~ 0))
  tbl$x[tbl$iso3 == "AAA" & tbl$tech == "Solar" & tbl$supply_chain == "Deployment"] <- 30
  tbl$x[tbl$iso3 == "AAA" & tbl$tech == "Wind" & tbl$supply_chain == "Recovery"] <- 0.1
  top <- function(r) {
    a <- r[r$iso3 == "AAA", ]
    paste(a$tech, a$supply_chain)[which.max(a$priority)]
  }
  expect_equal(top(sp_within_country_priority(tbl, "x", rca_smoothing = 0)), "Wind Recovery")
  expect_equal(top(sp_within_country_priority(tbl, "x")), "Solar Deployment")
  # a cell without records stays at the bottom under any smoothing
  r <- sp_within_country_priority(tbl, "x", rca_smoothing = 2)
  expect_true(all(r$rca[r$x == 0] == 0))
})

test_that("countries with records in too few cells get no priority", {
  tbl <- data.frame(
    iso3 = rep(c("AAA", "BBB", "CCC"), each = 4),
    tech = "Solar",
    supply_chain = rep(SUPPLY_CHAIN_STAGES[1:4], 3),
    x = c(1, 2, 3, 4, 5, 0, 0, 1, 0, 0, 0, 0)
  )
  r <- sp_within_country_priority(tbl, "x", min_cells = 3)
  expect_true(all(!is.na(r$priority[r$iso3 == "AAA"])))
  expect_true(all(is.na(r$priority[r$iso3 == "BBB"])))
  expect_true(all(is.na(r$priority_raw[r$iso3 == "BBB"])))
  expect_equal(unique(r$n_cells_with_records[r$iso3 == "BBB"]), 2L)
  # an all-zero country: no attention, no priority, no error
  expect_true(all(is.na(r$attention[r$iso3 == "CCC"])))
  expect_true(all(is.na(r$priority[r$iso3 == "CCC"])))
  r2 <- sp_within_country_priority(tbl, "x", min_cells = 2)
  expect_true(all(!is.na(r2$priority[r2$iso3 == "BBB"])))
})

test_that("a singleton country gets 0.5 once it passes min_cells", {
  tbl <- data.frame(iso3 = c("AAA", "AAA", "ZZZ"), tech = "Solar", supply_chain = c("Extraction", "Refining", "Extraction"),
    x = c(2, 1, 4))
  expect_equal(sp_within_country_priority(tbl, "x", min_cells = 1)$priority[3], 0.5)
  expect_true(is.na(sp_within_country_priority(tbl, "x", min_cells = 3)$priority[3]))
})

test_that("a cell nobody has records in ranks lowest, not NaN, and a missing cell stays out", {
  tbl <- tidyr::expand_grid(iso3 = c("AAA", "BBB"), tech = "Solar", supply_chain = SUPPLY_CHAIN_STAGES) %>%
    mutate(x = c(3, 2, 1, 4, 0, 5, 1, 1, 2, 3, 0, 4))
  tbl$x[tbl$iso3 == "BBB" & tbl$supply_chain == "Recovery"] <- NA
  r <- sp_within_country_priority(tbl, "x")
  dep <- r[r$supply_chain == "Deployment", ]
  expect_equal(dep$rca, c(0, 0))
  expect_false(any(is.nan(r$rca)))
  expect_true(is.na(r$priority[r$iso3 == "BBB" & r$supply_chain == "Recovery"]))
  expect_equal(sort(r$priority_raw[r$iso3 == "BBB" & !is.na(r$x)]), sort(sp_midrank_pct(c(1, 1, 2, 3, 0))))
})

# ---- combination ----------------------------------------------------------------------------------

pp_fixture <- function() {
  set.seed(11)
  pams <- tidyr::expand_grid(iso3 = c("AAA", "BBB", "CCC", "DDD"), tech = pp_techs, supply_chain = SUPPLY_CHAIN_STAGES) %>%
    mutate(
      pams_stock = round(runif(n(), 0, 8)), pams_exact_stock = pams_stock * 0.75, pams_split_stock = pams_stock * 0.25,
      pams_flow_1y = round(pams_stock / 3), pams_flow_3y = round(pams_stock / 2), pams_flow_5y = pams_stock,
      pams_tech_wide_stock = 6, pams_shared_stock = 3, pams_cross_cutting_stock = 10
    )
  # AAA has no PAMS records in any Deployment cell, so its Deployment cells are ranked lowest
  nipo <- bind_rows(
    pp_nipo("AAA", c(5, 0, 2, 1, 3, 8, 0, 4)),
    pp_nipo("BBB", c(1, 2, 0, 7)),
    pp_nipo("EEE", c(3, 1, 4, 1, 5))
  )
  list(pams = pams, nipo = nipo)
}

test_that("the score is the mean of the available sources, with its label", {
  fx <- pp_fixture()
  out <- sp_policy_priority(fx$pams, fx$nipo, pp_cfg, techs = pp_techs)
  cells <- out$cells
  expect_equal(nrow(cells), 5 * length(pp_techs) * 6)
  both <- cells[cells$policy_priority_sources == "nipo+pams", ]
  expect_equal(both$policy_priority_score, (both$nipo_priority + both$pams_priority) / 2)
  expect_equal(both$policy_priority_source_gap, both$nipo_priority - both$pams_priority)
  expect_equal(unique(cells$policy_priority_sources[cells$iso3 == "AAA" & cells$supply_chain == "Deployment"]), "pams")
  expect_equal(unique(cells$policy_priority_sources[cells$iso3 == "CCC"]), "pams")
  eee <- cells[cells$iso3 == "EEE", ]
  expect_equal(unique(eee$policy_priority_sources[eee$supply_chain == "Deployment"]), "none")
  expect_equal(unique(eee$policy_priority_sources[eee$supply_chain != "Deployment"]), "nipo")
  expect_true(all(is.na(eee$policy_priority_percentile[eee$supply_chain == "Deployment"])))
  expect_equal(cells$pp_n_sources, as.integer(!is.na(cells$nipo_priority)) + as.integer(!is.na(cells$pams_priority)))
  # percentile of the score, within the country
  a <- cells[cells$iso3 == "AAA", ]
  expect_equal(a$policy_priority_percentile, sp_midrank_pct(a$policy_priority_score))
  z <- ifelse(is.na(a$nipo_priority), qnorm(a$pams_priority), (qnorm(a$nipo_priority) + qnorm(a$pams_priority)) / sqrt(2))
  expect_equal(a$policy_priority_percentile_z, sp_midrank_pct(z))
  # one source only: the z variant ranks exactly as the source does
  ccc <- cells[cells$iso3 == "CCC", ]
  expect_equal(ccc$policy_priority_percentile_z, ccc$policy_priority_percentile)
  expect_equal(unique(eee$policy_data_coverage[eee$supply_chain == "Deployment"]), 0)
  expect_equal(unique(a$policy_data_coverage[a$supply_chain == "Deployment"]), 0.5)
  expect_equal(unique(a$policy_data_coverage[a$supply_chain == "Refining"]), 1)
  expect_true(all(c("policy_support_stock", "policy_protection_stock", "nipo_all_stock", "nipo_n_measures_stock",
    "nipo_fiscal_data_coverage", "nipo_priority_iw") %in% names(cells)))
})

test_that("NIPO no_data cells stay out of the NIPO ranking instead of counting as zero", {
  fx <- pp_fixture()
  cells <- sp_policy_priority(fx$pams, fx$nipo, pp_cfg, techs = pp_techs)$cells
  a <- cells[cells$iso3 == "AAA", ]
  dep <- a$supply_chain == "Deployment"
  expect_true(all(is.na(a$nipo_priority[dep])))
  expect_equal(unique(a$nipo_cell_status[dep]), "no_data")
  expect_equal(mean(a$nipo_priority[!dep]), 0.5) # percentiles over its 15 NIPO cells only
  expect_equal(sum(!is.na(a$nipo_priority)), length(pp_techs) * 5)

  # zero-filling Deployment would have changed the NIPO percentiles
  filled <- bind_rows(fx$nipo, pp_nipo("AAA", 0) %>% mutate(supply_chain = "Deployment") %>% distinct())
  filled <- filled %>% distinct(iso3, tech, supply_chain, .keep_all = TRUE)
  f <- sp_policy_priority(fx$pams, filled, pp_cfg, techs = pp_techs)$cells
  fa <- f[f$iso3 == "AAA" & f$supply_chain != "Deployment", ]
  expect_false(isTRUE(all.equal(fa$nipo_priority_raw, a$nipo_priority_raw[!dep])))
})

test_that("momentum ranks the flow of the default window, and each window is reported", {
  fx <- pp_fixture()
  cells <- sp_policy_priority(fx$pams, fx$nipo, pp_cfg, windows = c(1, 3, 5), techs = pp_techs)$cells
  expect_equal(cells$policy_momentum, cells$policy_momentum_3y)
  expect_equal(cells$pams_momentum, cells$pams_momentum_3y)
  expect_equal(cells$nipo_momentum, cells$nipo_momentum_3y)
  b <- cells[cells$iso3 == "BBB", ]
  m5 <- sp_within_country_priority(fx$pams[fx$pams$iso3 %in% unique(fx$pams$iso3), ], "pams_flow_5y")
  expect_equal(b$pams_momentum_5y, m5$priority[m5$iso3 == "BBB"])
  # pams_flow_5y equals the stock here, so 5-year momentum equals the stock priority
  expect_equal(cells$pams_momentum_5y, cells$pams_priority)
  five <- sp_policy_priority(fx$pams, fx$nipo, list(flow_window_years = 5, min_cells_with_records = 3), techs = pp_techs)$cells
  expect_equal(five$policy_momentum, five$policy_momentum_5y)
  expect_error(sp_policy_priority(fx$pams, fx$nipo, list(flow_window_years = 2), techs = pp_techs), "default flow window")
})

test_that("sp_policy_priority builds the PAMS cells itself from links", {
  pc <- bind_rows(
    pp_record(1, "AAA", "Solar", "Deployment", year = 2024, modified = 1.73e12),
    pp_record(2, "AAA", "Wind", "Components", year = 2025),
    pp_record(3, "AAA", "Batteries", "Refining", year = 2019),
    pp_record(4, "BBB", "Solar", c("Components", "Assembly"))
  )
  links <- sp_pams_links(pc, techs = pp_techs)
  expect_equal(sp_pams_as_of_year(links), 2024L)
  direct <- sp_policy_priority(links, NULL, pp_cfg, techs = pp_techs)$cells
  via <- sp_policy_priority(sp_pams_cells(links, 2024, techs = pp_techs), NULL, pp_cfg, techs = pp_techs)$cells
  expect_equal(direct, via)
  expect_true(all(direct$nipo_cell_status == "no_data"))
  expect_true(all(is.na(direct$policy_priority_percentile[direct$iso3 == "BBB"]))) # two cells only
})

test_that("evidence rows weigh the stage-specific share of the policy signal", {
  fx <- pp_fixture()
  out <- sp_policy_priority(fx$pams, fx$nipo, pp_cfg, techs = pp_techs)
  inp <- out$inputs
  expect_true(all(inp$observed))
  expect_true(all(inp$value > 0))
  sums <- inp %>% group_by(iso3, tech, supply_chain) %>% summarise(w = sum(weight), any_in = any(in_score), .groups = "drop")
  expect_equal(sums$w[sums$any_in], rep(1, sum(sums$any_in)))
  expect_equal(anyDuplicated(inp[c("iso3", "tech", "supply_chain", "family", "variable")]), 0)

  # CCC Solar Deployment: PAMS only; tech_wide 6 / 6 stages = 1, shared 3 / 3 techs = 1
  cell <- out$cells %>% filter(iso3 == "CCC", tech == "Solar", supply_chain == "Deployment")
  rows <- inp %>% filter(iso3 == "CCC", tech == "Solar", supply_chain == "Deployment")
  total <- cell$pams_stock + 1 + 1
  spec <- sum(rows$weight[rows$evidence_status %in% c("exact", "split")]) / sum(rows$weight)
  expect_equal(spec, cell$pams_stock / total)
  expect_equal(rows$value[rows$evidence_status == "tech_wide"], 1)

  # a cell with both sources: each takes half the weight
  ab <- inp %>% filter(iso3 == "AAA", tech == "Wind", supply_chain == "Refining")
  if (n_distinct(ab$source[ab$in_score]) == 2) {
    expect_equal(sum(ab$weight[ab$source == "nipo"]), 0.5)
    expect_equal(sum(ab$weight[ab$source == "pams"]), 0.5)
  }
  # EEE has no PAMS, so its NIPO rows carry the whole cell
  e <- inp %>% filter(iso3 == "EEE")
  expect_equal(unique(e$source), "nipo")
  expect_equal(unique(e$evidence_status), "split")
})

# ---- CAT -----------------------------------------------------------------------------------------

test_that("CAT context is the country rating, from the ratings table", {
  ratings <- data.frame(
    Country = c("Aland", "Borduria", "Carpania", "Dorne"),
    Overall.rating = c("Critically insufficient", "Insufficient", "Almost Sufficient", "Not assessed"),
    Policies.and.action = c("Highly insufficient", "Almost sufficient", "1.5°C compatible", "Insufficient"),
    rating_scope = c("national", "EU bloc rating", "national", NA),
    stringsAsFactors = FALSE
  )
  out <- sp_cat_context(ratings)
  expect_equal(out$cat_context, c(0.25, 0.75, 1, NA))
  expect_equal(out$cat_policies_action, c(0.5, 1, 1, 0.75))
  expect_equal(out$cat_rating_scope, c("national", "EU bloc rating", "national", "national"))
  expect_equal(nrow(out), 4)
})

test_that("CAT context is recovered from the tech-multiplied component", {
  ghg <- data.frame(tech = c("Oil", "Gas", "Solar", "Wind"), ghg_index = c(0, 0.42, 0.967, 0.999))
  c_true <- c(Aland = 0.25, Borduria = 0.5, Carpania = 0.75, Dorne = 1)
  comp <- tidyr::expand_grid(Country = names(c_true), tech = ghg$tech, supply_chain = SUPPLY_CHAIN_STAGES) %>%
    mutate(
      iso3c = toupper(substr(Country, 1, 3)),
      g = ghg$ghg_index[match(tech, ghg$tech)], cc = c_true[Country],
      value = cc * g + (1 - cc) * (1 - g), variable = "CAT Policy Index",
      explanation = if_else(Country == "Carpania", "...; c is the EU bloc rating (no national CAT rating)", "...")
    ) %>%
    select(-g, -cc)
  comp <- bind_rows(comp, comp[1, ] %>% mutate(variable = "IEA PAMS Policy Index", value = 0.99))
  out <- sp_cat_context(comp, tech_ghg = ghg, keep_iso3 = TRUE)
  expect_equal(setNames(out$cat_context, out$Country), c_true[out$Country], tolerance = 1e-12)
  expect_equal(out$cat_rating[out$Country == "Carpania"], "Insufficient")
  expect_equal(out$cat_rating_scope[out$Country == "Carpania"], "EU bloc rating")
  expect_equal(out$iso3[out$Country == "Dorne"], "DOR")
  expect_false("iso3" %in% names(sp_cat_context(comp, tech_ghg = ghg))) # joins by Country stay clean
  expect_error(sp_cat_context(comp), "tech_ghg")
  bad <- comp
  bad$value[bad$Country == "Dorne" & bad$tech == "Solar"] <- 0.1
  expect_error(sp_cat_context(bad, tech_ghg = ghg), "differs across technologies")
})
