# Import exposure (R/strategic_position/import_exposure.R) and Strategic Need
# (R/strategic_position/strategic_need.R): docs/strategic_position.md section 3. Synthetic fixtures.

sn_repo_root <- function() {
  normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
}

suppressPackageStartupMessages(library(dplyr))
source(file.path(sn_repo_root(), "R", "utils", "supply_chain_stages.R"))
source(file.path(sn_repo_root(), "R", "utils", "country.R"))
source(file.path(sn_repo_root(), "R", "strategic_position", "sp_helpers.R"))
source(file.path(sn_repo_root(), "R", "strategic_position", "import_exposure.R"))
source(file.path(sn_repo_root(), "R", "strategic_position", "strategic_need.R"))

# The committed strategic_need config. Its nonaligned rule needs a geo_alignment table, which most
# fixtures have no use for, so the helper keeps opposite_bloc_share as measured unless a test asks
# for the rule (the tests of the rule pass one).
sn_cfg <- function(...) {
  cfg <- yaml::read_yaml(file.path(sn_repo_root(), "config", "strategic_position.yml"))$strategic_need
  cfg$nonaligned_opposite_bloc <- "as_is"
  utils::modifyList(cfg, list(...))
}

# geo_alignment_tbl rows: `nonaligned` reporters at 0.5 with every partner, the rest 1 within
# `bloc_a` and 0 across.
sn_geo <- function(iso, nonaligned = character(), bloc_a = character()) {
  g <- expand.grid(reporter_iso = iso, partner_iso = iso, stringsAsFactors = FALSE)
  g <- g[g$reporter_iso != g$partner_iso, ]
  same <- (g$reporter_iso %in% bloc_a) == (g$partner_iso %in% bloc_a)
  g$geo_alignment <- ifelse(g$reporter_iso %in% nonaligned | g$partner_iso %in% nonaligned, 0.5, ifelse(same, 1, 0))
  tibble::as_tibble(g)
}

# es_contrib rows: one variable for several countries in one cell.
sn_es <- function(countries, values, tech = "Batteries", stage = "Assembly", variable = "deficit_gdp",
                  category = "Trade", imputed = FALSE) {
  tibble::tibble(
    Country = countries, tech = tech, supply_chain = stage, category = category, variable = variable,
    component_value = values, imputed = rep_len(imputed, length(countries))
  )
}

# import_exposure rows, as sp_import_exposure() returns them.
sn_ie <- function(iso, imports, hhi, china, opposite, tech = "Batteries", stage = "Assembly", observed = TRUE) {
  tibble::tibble(
    importer_iso = iso, tech = tech, supply_chain = stage, observed = rep_len(observed, length(iso)),
    imports_usd = imports, n_suppliers = 3L, top_supplier_iso3 = "CHN", top_supplier_share = hhi,
    import_hhi = hhi, china_share = china, opposite_bloc_share = opposite, imports_to_gdp = imports / 1e12
  )
}

sn_lookup <- function(countries) tibble::tibble(Country = countries, iso3 = toupper(substr(countries, 1, 3)))

# The cells of one stage (Assembly fixtures also produce their Deployment proxy cells).
sn_stage <- function(res, stage = "Assembly") res$cells[res$cells$supply_chain == stage, ]

five <- c("Aland", "Borduria", "Carpania", "Dorne", "Elbonia")

test_that("import exposure: shares, HHI, China group and opposite bloc from the flow table", {
  tc <- tibble::tribble(
    ~exporter_iso, ~importer_iso, ~tech, ~supply_chain, ~value_last, ~source_last,
    "CHN", "JPN", "Batteries", "Assembly", 60, "importer",
    "HKG", "JPN", "Batteries", "Assembly", 20, "importer",
    "USA", "JPN", "Batteries", "Assembly", 20, "importer",
    "KOR", "JPN", "Batteries", "Assembly", 0, "importer",
    "CHN", "AUS", "Batteries", "Assembly", 0, "mirror",
    "USA", "AUS", "Batteries", "Assembly", 0, "mirror",
    "HKG", "CHN", "Batteries", "Assembly", 10, "importer",
    "USA", "CHN", "Batteries", "Assembly", 30, "importer",
    "CHN", "NZL", "Batteries", "Refining", 5, "mirror",
    "USA", "NZL", "Batteries", "Assembly", NA, NA
  )
  al <- tibble::tribble(
    ~reporter_iso, ~partner_iso, ~geo_alignment,
    "JPN", "CHN", 0, "JPN", "HKG", 0, "JPN", "USA", 1,
    "CHN", "USA", 0, "CHN", "HKG", 1
  )
  gdp <- tibble::tibble(iso3 = c("JPN", "CHN"), gdp_usd = c(1000, 2000))
  ie <- sp_import_exposure(tc, al, gdp, trade_year = 2024)

  jpn <- ie[ie$importer_iso == "JPN" & ie$supply_chain == "Assembly", ]
  expect_equal(jpn$imports_usd, 100)
  expect_equal(jpn$n_suppliers, 3)
  expect_equal(jpn$top_supplier_iso3, "CHN")
  expect_equal(jpn$top_supplier_share, 0.6)
  expect_equal(jpn$import_hhi, 0.6^2 + 0.2^2 + 0.2^2)
  expect_equal(jpn$china_share, 0.8)            # mainland + Hong Kong
  expect_equal(jpn$opposite_bloc_share, 0.8)
  expect_equal(jpn$alignment_coverage_share, 1)
  expect_equal(jpn$imports_to_gdp, 0.1)
  expect_equal(jpn$import_source, "importer")
  expect_equal(jpn$trade_year, 2024L)

  # An observed importer with no imports has no exposure: zeros, not NA.
  aus <- ie[ie$importer_iso == "AUS" & ie$supply_chain == "Assembly", ]
  expect_true(aus$observed)
  expect_equal(c(aus$imports_usd, aus$import_hhi, aus$china_share, aus$opposite_bloc_share, aus$top_supplier_share), c(0, 0, 0, 0, 0))
  expect_true(is.na(aus$top_supplier_iso3))
  expect_true(is.na(aus$imports_to_gdp))       # no GDP

  # China's trade with Hong Kong is not exposure to a foreign supplier.
  chn <- ie[ie$importer_iso == "CHN" & ie$supply_chain == "Assembly", ]
  expect_true(is.na(chn$china_share))
  expect_equal(chn$opposite_bloc_share, 0.75)

  # Unobserved last year (value NA) and absent cells are explicit NA rows.
  nzl_a <- ie[ie$importer_iso == "NZL" & ie$supply_chain == "Assembly", ]
  expect_false(nzl_a$observed)
  expect_true(is.na(nzl_a$imports_usd) && is.na(nzl_a$import_hhi))
  expect_false(ie$observed[ie$importer_iso == "JPN" & ie$supply_chain == "Refining"])
  expect_equal(nrow(ie), 4 * 2)
})

test_that("import exposure: GDP takes the target year or the latest earlier one", {
  wdi <- data.frame(
    iso3c = c("AAA", "AAA", "BBB", "BBB", "CCC"),
    year = c(2023, 2024, 2022, 2025, 2025),
    NY.GDP.MKTP.CD = c(10, 12, 7, 9, 5)
  )
  g <- sp_gdp_usd(wdi, 2024)
  expect_equal(g$gdp_usd[g$iso3 == "AAA"], 12)
  expect_equal(g$gdp_year[g$iso3 == "BBB"], 2022L)
  expect_false("CCC" %in% g$iso3)
})

test_that("orientation: a bigger net importer and a less self-sufficient fuel user have more need", {
  es <- sn_es(five, c(0.1, 0.3, 0.5, 0.7, 0.9))          # deficit_gdp: higher = surplus = secure
  res <- sp_strategic_need(es, sn_ie(character(), numeric(), numeric(), numeric(), numeric()),
                           sn_cfg(), country_iso3 = sn_lookup(five))
  cl <- sn_stage(res)
  expect_equal(cl$Country[order(-cl$strategic_need_score)], five)
  expect_equal(cl$strategic_need_rank[cl$Country == "Aland"], 1L)
  expect_equal(cl$strategic_need_percentile[cl$Country == "Aland"], 0.9)

  es2 <- sn_es(five, c(0.9, 0.2, 0.5, 0.6, 0.4), tech = "Coal", stage = "Extraction",
               variable = "Production surplus/deficit", category = "Energy Imports")
  res2 <- sp_strategic_need(es2, sn_ie(character(), numeric(), numeric(), numeric(), numeric()),
                            sn_cfg(), country_iso3 = sn_lookup(five))
  top <- res2$cells$Country[which.max(res2$cells$strategic_need_score)]
  expect_equal(top, "Borduria")
})

test_that("immaterial imports carry no concentration or risky-supplier need", {
  iso <- toupper(substr(five, 1, 3))
  ie <- sn_ie(iso, imports = c(5e4, 5e8, 5e8, 5e8, 5e8), hhi = c(1, 0.5, 0.4, 0.3, 0.2),
              china = c(1, 0.5, 0.4, 0.3, 0.2), opposite = c(1, 0.5, 0.4, 0.3, 0.2))
  # The universe is the es_contrib countries; their deficit_gdp is imputed, so only the import
  # families apply.
  universe <- sn_es(five, 0.5, imputed = TRUE)
  res <- sp_strategic_need(universe, ie, sn_cfg(min_material_importers_per_cell = 4), country_iso3 = sn_lookup(five))
  inp <- res$inputs
  al <- inp[inp$Country == "Aland", ]
  expect_true(all(al$immaterial))
  expect_true(all(al$value == 0))
  expect_true(all(al$observed))
  cl <- sn_stage(res)
  expect_equal(cl$strategic_need_rank[cl$Country == "Aland"], 5L)
  expect_false(cl$sn_import_material[cl$Country == "Aland"])
  expect_equal(cl$import_hhi[cl$Country == "Aland"], 1)  # context column keeps the measured value
  expect_equal(unique(cl$sn_cell_material_importers), 4L)
  # Default: need 0 rather than ranked; "tie" ranks the zero with the others (alone here: 0.1).
  expect_true(all(al$need_percentile == 0))
  tie <- sp_strategic_need(universe, ie, sn_cfg(immaterial_need = "tie", min_material_importers_per_cell = 4), country_iso3 = sn_lookup(five))
  expect_equal(unique(tie$inputs$need_percentile[tie$inputs$Country == "Aland"]), 0.1)
  expect_equal(tie$inputs$need_percentile[tie$inputs$Country == "Borduria"], res$inputs$need_percentile[res$inputs$Country == "Borduria"])
  expect_error(sp_strategic_need(universe, ie, sn_cfg(immaterial_need = "drop"), country_iso3 = sn_lookup(five)), "zero or tie")

  # Raising the threshold makes the second country immaterial too.
  res_hi <- sp_strategic_need(universe, ie, sn_cfg(min_import_usd = 1e9), country_iso3 = sn_lookup(five))
  expect_true(all(res_hi$inputs$immaterial))
})

test_that("a cell where few countries import material amounts is thin, a cell without trade data is not", {
  iso <- toupper(substr(five, 1, 3))
  es <- sn_es(five, c(0.1, 0.3, 0.5, 0.7, 0.9))
  # Only two of five import at least USD 1m: the import measures would rank zeros.
  ie <- sn_ie(iso, imports = c(5e8, 5e8, 1e3, 0, 2e5), hhi = c(0.5, 0.4, 1, 0, 1), china = 0.3, opposite = 0.3)
  cl <- sn_stage(sp_strategic_need(es, ie, sn_cfg(), country_iso3 = sn_lookup(five)))
  expect_true(all(cl$sn_thin_cell))
  expect_true(all(cl$sn_thin_reason == "few_material_importers"))
  expect_equal(unique(cl$sn_cell_material_importers), 2L)
  expect_true(all(is.na(cl$strategic_need_percentile)))
  expect_false(any(is.na(cl$strategic_need_score)))
  # The gate can be lowered (or switched off with 0).
  off <- sn_stage(sp_strategic_need(es, ie, sn_cfg(min_material_importers_per_cell = 0), country_iso3 = sn_lookup(five)))
  expect_false(any(off$sn_thin_cell))
  # No import exposure in the cell at all: the material gate does not apply.
  only_es <- sn_stage(sp_strategic_need(es, sn_ie(character(), numeric(), numeric(), numeric(), numeric()), sn_cfg(),
                                        country_iso3 = sn_lookup(five)))
  expect_false(any(only_es$sn_thin_cell))
  expect_equal(unique(only_es$sn_cell_material_importers), 0L)
})

test_that("Deployment takes the Assembly need as a labelled proxy, fuels also Extraction (shared)", {
  iso <- toupper(substr(five, 1, 3))
  es <- dplyr::bind_rows(
    sn_es(five, c(0.1, 0.3, 0.5, 0.7, 0.9), tech = "Coal", stage = "Assembly"),
    sn_es(five, c(0.9, 0.7, 0.5, 0.3, 0.1), tech = "Coal", stage = "Extraction",
          variable = "Production surplus/deficit", category = "Energy Imports"),
    sn_es(five, c(0.1, 0.3, 0.5, 0.7, 0.9), tech = "Solar", stage = "Assembly")
  )
  ie <- dplyr::bind_rows(
    sn_ie(iso, 5e8, c(0.9, 0.7, 0.5, 0.3, 0.1), 0.2, 0.2, tech = "Coal"),
    sn_ie(iso, 5e8, c(0.9, 0.7, 0.5, 0.3, 0.1), 0.2, 0.2, tech = "Solar")
  )
  res <- sp_strategic_need(es, ie, sn_cfg(), country_iso3 = sn_lookup(five))
  cl <- res$cells
  sol_a <- cl[cl$tech == "Solar" & cl$supply_chain == "Assembly", ]
  sol_d <- cl[cl$tech == "Solar" & cl$supply_chain == "Deployment", ]
  expect_equal(sol_d$strategic_need_score[order(sol_d$Country)], sol_a$strategic_need_score[order(sol_a$Country)])
  expect_true(all(sol_d$sn_basis == "proxy:Assembly"))
  expect_true(all(sol_d$import_exposure_stage == "Assembly"))
  expect_true(all(sol_a$sn_basis == "direct"))

  coal_d <- cl[cl$tech == "Coal" & cl$supply_chain == "Deployment", ]
  expect_true(all(coal_d$sn_basis == "proxy:Assembly+shared:Extraction"))
  inp <- res$inputs[res$inputs$tech == "Coal" & res$inputs$supply_chain == "Deployment", ]
  expect_setequal(unique(inp$evidence_status), c("proxy", "shared"))
  expect_true(all(inp$evidence_status[inp$source_stage == "Extraction"] == "shared"))
  expect_true(all(inp$family[inp$source_stage == "Extraction"] == "import_dependence"))
  # Elbonia: fuel self-sufficiency lowest (most need) lifts its Deployment need above Assembly's.
  expect_gt(coal_d$sn_import_dependence[coal_d$Country == "Elbonia"],
            cl$sn_import_dependence[cl$tech == "Coal" & cl$supply_chain == "Assembly" & cl$Country == "Elbonia"])
  # Own-stage evidence: import measures exact; ES rows left to the evidence table.
  own <- res$inputs[res$inputs$supply_chain == "Assembly", ]
  expect_true(all(own$evidence_status[own$pillar == "SN"] == "exact"))
  expect_true(all(is.na(own$evidence_status[own$pillar == "ES"])))
})

test_that("a large exporter with concentrated imports from a risky supplier can have high need", {
  iso <- toupper(substr(five, 1, 3))
  # Aland runs the largest surplus (lowest import dependence) but buys from one Chinese supplier.
  es <- sn_es(five, c(0.95, 0.2, 0.3, 0.4, 0.5))
  ie <- sn_ie(iso, 5e8, hhi = c(1, 0.3, 0.25, 0.2, 0.15), china = c(1, 0.1, 0.1, 0.1, 0.1),
              opposite = c(1, 0.1, 0.1, 0.1, 0.1))
  res <- sp_strategic_need(es, ie, sn_cfg(), country_iso3 = sn_lookup(five))
  al <- sn_stage(res)[sn_stage(res)$Country == "Aland", ]
  expect_equal(al$sn_import_dependence, 0.1)
  expect_gte(al$strategic_need_percentile, 0.5)
  expect_equal(al$top_supplier_iso3, "CHN")
})

test_that("missing is not zero: imputed ES values and absent trade drop out of the mean", {
  iso <- toupper(substr(five, 1, 3))
  es <- sn_es(five, c(0.1, 0.3, 0.5, 0.7, 0.9), imputed = c(TRUE, FALSE, FALSE, FALSE, FALSE))
  ie <- sn_ie(iso[-1], 5e8, hhi = c(0.4, 0.3, 0.2, 0.1), china = 0.1, opposite = 0.1)
  res <- sp_strategic_need(es, ie, sn_cfg(), country_iso3 = sn_lookup(five))
  al <- sn_stage(res)[sn_stage(res)$Country == "Aland", ]
  expect_true(is.na(al$strategic_need_score))
  expect_true(is.na(al$strategic_need_rank))
  expect_equal(al$sn_n_families, 0)
  ia <- res$inputs[res$inputs$Country == "Aland", ]
  expect_false(any(ia$observed))
  expect_true(all(is.na(ia$value)))

  # Borduria without trade data keeps its import-dependence family alone (not a zero for the rest).
  ie_b <- sn_ie(iso[-2], 5e8, hhi = c(0.5, 0.4, 0.3, 0.2), china = 0.1, opposite = 0.1)
  es_b <- sn_es(five, c(0.1, 0.3, 0.5, 0.7, 0.9))
  rb <- sp_strategic_need(es_b, ie_b, sn_cfg(), country_iso3 = sn_lookup(five)) %>% sn_stage()
  bo <- rb[rb$Country == "Borduria", ]
  expect_equal(bo$sn_n_families, 1)
  expect_equal(bo$strategic_need_score, bo$sn_import_dependence)
  expect_true(is.na(bo$sn_supplier_concentration))
})

test_that("fill structural_zero keeps an imputed ES value as a source zero (not observed, but scored)", {
  cfg <- sn_cfg()
  cfg$families$import_dependence$measures[[1]]$fill <- "structural_zero"
  es <- sn_es(five, c(0.6, 0.3, 0.5, 0.7, 0.9), imputed = c(TRUE, FALSE, FALSE, FALSE, FALSE))
  res <- sp_strategic_need(es, sn_ie(character(), numeric(), numeric(), numeric(), numeric()), cfg, country_iso3 = sn_lookup(five))
  a <- res$inputs[res$inputs$Country == "Aland" & res$inputs$supply_chain == "Assembly", ]
  expect_false(a$observed)
  expect_equal(a$value, 0)
  expect_equal(sn_stage(res)$strategic_need_rank[sn_stage(res)$Country == "Aland"], 1L)  # index 0 = least secure
})

test_that("weights: equal by family then by measure; score is the score-weighted percentile sum", {
  iso <- toupper(substr(five, 1, 3))
  es <- sn_es(five, c(0.1, 0.3, 0.5, 0.7, 0.9))
  ie <- sn_ie(iso, 5e8, hhi = c(0.4, 0.3, 0.2, 0.1, 0.05), china = c(0.5, 0.4, 0.3, NA, 0.1), opposite = 0.2)
  res <- sp_strategic_need(es, ie, sn_cfg(), country_iso3 = sn_lookup(five))
  w <- res$inputs %>%
    group_by(Country, tech, supply_chain) %>%
    summarise(w = sum(weight), sw = sum(score_weight), s = sum(score_weight * dplyr::coalesce(need_percentile, 0)), .groups = "drop") %>%
    left_join(res$cells, by = c("Country", "tech", "supply_chain"))
  expect_equal(w$w, rep(1, nrow(w)))
  expect_equal(w$sw, rep(1, nrow(w)))
  expect_equal(w$s, w$strategic_need_score)
  d <- res$inputs[res$inputs$Country == "Dorne" & res$inputs$supply_chain == "Assembly", ]
  expect_equal(d$weight[d$variable == "deficit_gdp"], 1 / 3)
  expect_equal(d$weight[d$variable == "china_share"], 1 / 6)
  expect_equal(d$score_weight[d$variable == "opposite_bloc_share"], 1 / 3)  # china_share missing
})

test_that("thin cells: a score but no percentile; a singleton is 0.5 and flagged", {
  three <- five[1:3]
  res <- sp_strategic_need(sn_es(three, c(0.1, 0.5, 0.9)), sn_ie(character(), numeric(), numeric(), numeric(), numeric()),
                           sn_cfg(), country_iso3 = sn_lookup(three))
  cl <- sn_stage(res)
  expect_true(all(cl$sn_thin_cell))
  expect_true(all(is.na(cl$strategic_need_percentile)))
  expect_true(all(is.na(cl$strategic_need_rank)))
  expect_false(any(is.na(cl$strategic_need_score)))
  expect_equal(cl$sn_cell_countries, rep(3L, 3))
  expect_true(all(cl$sn_thin_reason == "few_countries"))

  one <- sp_strategic_need(sn_es("Aland", 0.4), sn_ie(character(), numeric(), numeric(), numeric(), numeric()),
                           sn_cfg(), country_iso3 = sn_lookup("Aland")) %>% sn_stage()
  expect_equal(one$strategic_need_score, 0.5)
  expect_true(one$sn_thin_cell)
  expect_true(is.na(one$strategic_need_percentile))
  one1 <- sp_strategic_need(sn_es("Aland", 0.4), sn_ie(character(), numeric(), numeric(), numeric(), numeric()),
                            sn_cfg(min_countries_per_cell = 1), country_iso3 = sn_lookup("Aland")) %>% sn_stage()
  expect_equal(one1$strategic_need_percentile, 0.5)
  expect_false(one1$sn_thin_cell)
})

test_that("world constants and EO-identical variables cannot enter Strategic Need", {
  bad <- sn_cfg()
  bad$families$import_dependence$measures[[3]] <- list(source = "es_contributions", category = "Trade", variable = "HHI", orient = "invert", fill = "na")
  expect_error(sp_strategic_need(sn_es(five, 1:5 / 10), sn_ie(character(), numeric(), numeric(), numeric(), numeric()), bad,
                                 country_iso3 = sn_lookup(five)), "HHI")
  eo <- sn_cfg()
  eo$families$import_dependence$measures[[1]]$source <- "eo_contributions"
  expect_error(sp_strategic_need(sn_es(five, 1:5 / 10), sn_ie(character(), numeric(), numeric(), numeric(), numeric()), eo,
                                 country_iso3 = sn_lookup(five)), "unknown measure source")
})

test_that("country names resolve to ISO3 and join the import exposure", {
  countries <- c("Japan", "Germany", "India", "United States", "Australia", "European Union")
  es <- sn_es(countries, c(0.2, 0.4, 0.6, 0.8, 0.9, 0.5))
  ie <- sn_ie(c("JPN", "DEU", "IND", "USA", "AUS"), 5e8, hhi = c(0.5, 0.4, 0.3, 0.2, 0.1), china = 0.3, opposite = 0.3)
  res <- sp_strategic_need(es, ie, sn_cfg())
  expect_equal(attr(res, "dropped_countries"), "European Union")
  expect_setequal(res$cells$iso3, c("JPN", "DEU", "IND", "USA", "AUS"))
  expect_equal(sn_stage(res)$import_hhi[sn_stage(res)$Country == "Japan"], 0.5)
  expect_equal(sn_stage(res, "Deployment")$import_hhi[sn_stage(res, "Deployment")$Country == "Japan"], 0.5)
  expect_equal(res$cells$legacy_stage[1], "Midstream")
})

test_that("entrepots get no need score and do not move other countries' ranks", {
  six <- c(five, "Hong Kong")
  lookup <- tibble::tibble(Country = six, iso3 = c(toupper(substr(five, 1, 3)), "HKG"))
  es <- sn_es(six, c(0.1, 0.3, 0.5, 0.7, 0.9, 0.0))       # Hong Kong: the largest deficit
  ie <- sn_ie(lookup$iso3, 5e8, hhi = c(0.5, 0.4, 0.3, 0.2, 0.1, 0.9), china = c(0.5, 0.4, 0.3, 0.2, 0.1, 0.9),
              opposite = 0.2)
  ie$imports_usd[ie$importer_iso == "HKG"] <- 7e10
  res <- sp_strategic_need(es, ie, sn_cfg(), country_iso3 = lookup)
  cl <- sn_stage(res)
  hk <- cl[cl$iso3 == "HKG", ]
  expect_equal(nrow(hk), 1)
  expect_equal(hk$sn_exclusion, "entrepot")
  expect_true(is.na(hk$strategic_need_score) && is.na(hk$strategic_need_percentile) && is.na(hk$strategic_need_rank))
  expect_true(is.na(hk$sn_import_dependence))
  expect_equal(hk$imports_usd, 7e10)                        # the import context stays
  expect_true(hk$sn_import_material)
  expect_true(all(is.na(cl$sn_exclusion[cl$iso3 != "HKG"])))
  expect_false("HKG" %in% res$inputs$iso3)
  expect_equal(attr(res, "excluded_countries"), "Hong Kong")
  # Its Deployment proxy row is excluded too.
  expect_equal(sn_stage(res, "Deployment")$sn_exclusion[sn_stage(res, "Deployment")$iso3 == "HKG"], "entrepot")

  # The other five rank exactly as if Hong Kong were not in the data at all.
  without <- sn_stage(sp_strategic_need(es[es$Country != "Hong Kong", ], ie[ie$importer_iso != "HKG", ], sn_cfg(),
                                        country_iso3 = lookup[lookup$iso3 != "HKG", ]))
  kept <- cl[cl$iso3 != "HKG", ]
  expect_equal(kept$strategic_need_percentile[order(kept$Country)], without$strategic_need_percentile[order(without$Country)])
  expect_equal(kept$strategic_need_rank[order(kept$Country)], without$strategic_need_rank[order(without$Country)])
  expect_equal(unique(kept$sn_cell_countries), 5L)
  expect_equal(unique(hk$sn_cell_countries), 5L)            # the cell's fields, which it is not in

  # The list comes from the config; an empty argument scores Hong Kong again, ranked first.
  scored <- sn_stage(sp_strategic_need(es, ie, sn_cfg(), country_iso3 = lookup, exclude_iso3 = character()))
  expect_equal(scored$strategic_need_rank[scored$iso3 == "HKG"], 1L)
  expect_true(all(is.na(scored$sn_exclusion)))
  expect_true("HKG" %in% unlist(sn_cfg()$exclude_iso3))
})

test_that("a nonaligned importer's opposite-bloc share is not applicable; its risky family is the China share", {
  iso <- toupper(substr(five, 1, 3))
  geo <- sn_geo(iso, nonaligned = "ALA", bloc_a = c("BOR", "CAR"))
  b <- sp_need_opposite_bloc_basis(c(iso, "ZZZ"), geo)
  expect_equal(b$opposite_bloc_basis, c("nonaligned", "bloc", "bloc", "bloc", "bloc", "no_alignment"))

  es <- sn_es(five, c(0.1, 0.3, 0.5, 0.7, 0.9))
  # Aland is nonaligned, so its opposite-bloc share is 0 by construction whatever it buys.
  ie <- sn_ie(iso, 5e8, hhi = c(0.5, 0.4, 0.3, 0.2, 0.1), china = c(0.9, 0.4, 0.3, 0.2, 0.1),
              opposite = c(0, 0.6, 0.5, 0.4, 0.3))
  res <- sp_strategic_need(es, ie, sn_cfg(nonaligned_opposite_bloc = "na"), country_iso3 = sn_lookup(five),
                           geo_alignment = geo)
  expect_equal(attr(res, "nonaligned_iso3"), "ALA")
  inp <- res$inputs[res$inputs$supply_chain == "Assembly", ]
  al_opp <- inp[inp$Country == "Aland" & inp$variable == "opposite_bloc_share", ]
  expect_false(al_opp$applies)
  expect_true(is.na(al_opp$value) && is.na(al_opp$need_percentile))
  expect_equal(al_opp$weight, 0)
  expect_equal(al_opp$score_weight, 0)
  al_cn <- inp[inp$Country == "Aland" & inp$variable == "china_share", ]
  expect_equal(al_cn$weight, 1 / 3)                         # the family's whole weight
  cl <- sn_stage(res)
  al <- cl[cl$Country == "Aland", ]
  expect_equal(al$sn_risky_supplier_exposure, al_cn$need_percentile)
  expect_equal(al$sn_risky_supplier_exposure, 0.9)          # highest China share of five
  expect_true(is.na(al$opposite_bloc_share))
  expect_equal(al$sn_opposite_bloc_basis, "nonaligned")
  expect_true(all(cl$sn_opposite_bloc_basis[cl$Country != "Aland"] == "bloc"))
  # The others' opposite-bloc percentiles run over the four aligned importers only.
  opp <- inp[inp$variable == "opposite_bloc_share" & inp$applies, ]
  expect_equal(sort(opp$need_percentile), c(0.125, 0.375, 0.625, 0.875))
  w <- tapply(res$inputs$weight, paste(res$inputs$Country, res$inputs$supply_chain), sum)
  expect_true(all(abs(w - 1) < 1e-12))

  # Kept as measured, the zero ranks Aland lowest on the measure and drags its family down.
  as_is <- sn_stage(sp_strategic_need(es, ie, sn_cfg(nonaligned_opposite_bloc = "as_is"), country_iso3 = sn_lookup(five),
                                      geo_alignment = geo))
  expect_equal(as_is$sn_risky_supplier_exposure[as_is$Country == "Aland"], (0.9 + 0.1) / 2)
  expect_true(all(is.na(as_is$sn_opposite_bloc_basis)))
  expect_equal(as_is$opposite_bloc_share[as_is$Country == "Aland"], 0)

  # The rule needs the alignment table: without it, a warning and the measure as measured.
  expect_warning(
    nog <- sp_strategic_need(es, ie, sn_cfg(nonaligned_opposite_bloc = "na"), country_iso3 = sn_lookup(five)),
    "needs geo_alignment"
  )
  expect_equal(sn_stage(nog)$sn_risky_supplier_exposure, as_is$sn_risky_supplier_exposure)
  expect_error(sp_strategic_need(es, ie, sn_cfg(nonaligned_opposite_bloc = "zero"), country_iso3 = sn_lookup(five)),
               "nonaligned_opposite_bloc")
  expect_equal(yaml::read_yaml(file.path(sn_repo_root(), "config", "strategic_position.yml"))$strategic_need$nonaligned_opposite_bloc, "na")
})

test_that("fuel_import_techs and immaterial_families are read from the config", {
  iso <- toupper(substr(five, 1, 3))
  es <- dplyr::bind_rows(
    sn_es(five, c(0.1, 0.3, 0.5, 0.7, 0.9), tech = "Coal", stage = "Assembly"),
    sn_es(five, c(0.9, 0.7, 0.5, 0.3, 0.1), tech = "Coal", stage = "Extraction",
          variable = "Production surplus/deficit", category = "Energy Imports")
  )
  ie <- sn_ie(iso, c(5e4, 5e8, 5e8, 5e8, 5e8), c(0.9, 0.7, 0.5, 0.3, 0.1), c(0.9, 0.4, 0.3, 0.2, 0.1), 0.2, tech = "Coal")
  gas_only <- sp_strategic_need(es, ie, sn_cfg(fuel_import_techs = list("Gas"), min_material_importers_per_cell = 4),
                                country_iso3 = sn_lookup(five))
  coal_d <- gas_only$cells[gas_only$cells$supply_chain == "Deployment", ]
  expect_true(all(coal_d$sn_basis == "proxy:Assembly"))
  # Only supplier concentration is immaterial below USD 1m: Aland's China share is then ranked.
  conc_only <- sp_strategic_need(es, ie, sn_cfg(immaterial_families = list("supplier_concentration"),
                                               min_material_importers_per_cell = 4), country_iso3 = sn_lookup(five))
  al <- conc_only$inputs[conc_only$inputs$Country == "Aland" & conc_only$inputs$supply_chain == "Assembly", ]
  expect_true(al$immaterial[al$variable == "import_hhi"])
  expect_false(al$immaterial[al$variable == "china_share"])
  expect_equal(al$need_percentile[al$variable == "china_share"], 0.9)
  expect_equal(al$need_percentile[al$variable == "import_hhi"], 0)
})
