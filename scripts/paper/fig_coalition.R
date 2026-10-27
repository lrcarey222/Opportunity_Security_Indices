# Figures, table and numbers for Section "Geopolitical alignment and the allied coalition"
# (paper/sections/075_alignment_coalition.tex, label sec:coalition).
#
# Builds, from the alignment and coalition tables of scripts/12 and scripts/13 (data/processed/):
#   coalition_axis              every economy with its own UNGA votes on the U.S.-China axis s
#                               (sessions 74-78), one strip per coalition tier, the two quartile
#                               cuts marked, major suppliers filled, G20 and notable cases labelled
#   coalition_routes            the members by tier: agreement memberships and the supplier route
#                               (Upstream / Midstream flow measures at 5% of world supply, reserves
#                               at 10% of world reserves), with the largest qualifying share
#   coalition_alignment_matrix  dyadic alignment A_ij among the network nodes plus China and
#                               Russia, ordered by tier and s
#   tables/coalition_composition.tex  composition of the coalition, exclusions, sensitivity and the
#                               change from the hand-picked 23-member list of the previous draft
# and writes every number the text quotes to paper/results/coalition.json.
#
# Robustness of the axis (not stored by the pipeline) is rebuilt here with the pipeline's own pure
# functions (sourced from R/, nothing is written there): the all-votes series over the single
# latest session (78, 2023) and the final-passage series over its latest five sessions
# (2021-2025), each run through geo_country_bloc_membership() and derive_coalition_tiers() with
# the configured settings. The headline window is first re-derived from the raw ideal points and
# checked against geo_ideal_points_tbl.rds and coalition_tbl.rds.
#
# Run from the repository root:  Rscript scripts/paper/fig_coalition.R

find_repo_root <- function() {
  d <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  while (!file.exists(file.path(d, "scripts", "paper", "_paper_theme.R"))) {
    if (dirname(d) == d) stop("Run from inside the repository.")
    d <- dirname(d)
  }
  d
}
repo_root <- find_repo_root()
source(file.path(repo_root, "scripts", "paper", "_paper_theme.R"))
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggrepel)
  library(cowplot)
})
for (f in c("R/utils/country.R", "R/geopolitical_distance/alignment.R", "R/indices/coalition.R")) {
  source(file.path(repo_root, f))
}
data_path <- function(...) file.path(repo_root, "data", ...)
cfg_path <- function(...) file.path(repo_root, "config", ...)

# ---------------------------------------------------------------------------------------------
# Inputs
# ---------------------------------------------------------------------------------------------
geo_cfg <- yaml::read_yaml(cfg_path("geopolitical_distance.yml"))
align_cfg <- geo_cfg$alignment
coal_cfg_all <- yaml::read_yaml(cfg_path("coalition.yml"))
coal_cfg <- coal_cfg_all$coalition
eu_cfg <- coal_cfg_all$eu_node
fs_weights <- unlist(yaml::read_yaml(cfg_path("weights.yml"))$partnership_friendshore_components)
supplier_share <- as.numeric(coal_cfg$supplier_share)
reserves_share <- as.numeric(coal_cfg$supplier_reserves_share)
gdp_floor <- as.numeric(coal_cfg$gdp_floor_usd)
stopifnot(length(reserves_share) == 1, !is.na(reserves_share))

geo <- as.data.frame(readRDS(data_path("processed", "geo_ideal_points_tbl.rds")))
geo_coal <- as.data.frame(readRDS(data_path("processed", "geo_coalition_tbl.rds")))
coal <- as.data.frame(readRDS(data_path("processed", "coalition_tbl.rds")))
euw <- as.data.frame(readRDS(data_path("processed", "eu_stage_weights_tbl.rds")))
sup <- as.data.frame(readRDS(data_path("processed", "major_supplier_shares_tbl.rds")))
agreements <- utils::read.csv(data_path("reference", "economic_security_agreements.csv"),
                              stringsAsFactors = FALSE, na.strings = "", encoding = "UTF-8")
nodes <- utils::read.csv(data_path("processed", "outputs", "allied_network_nodes.csv"), stringsAsFactors = FALSE)
net_members <- utils::read.csv(data_path("processed", "outputs", "allied_network_members.csv"), stringsAsFactors = FALSE)
gdp_raw <- utils::read.csv(data_path("raw", "wdi_gdp.csv"), stringsAsFactors = FALSE, check.names = FALSE)

al <- readRDS(data_path("processed", "geo_alignment_tbl.rds"))
al <- as.data.frame(al[, c("reporter_iso", "partner_iso", "p_reporter", "p_partner", "geo_alignment")])

# Policy eligibility (index_outputs.rds) only to report the policy basis of the members, which
# the Strategic Index picks need.
io <- readRDS(data_path("processed", "outputs", "index_outputs.rds"))
pol_elig <- as.data.frame(io$policy_eligibility)
io_members <- as.data.frame(io$coalition_members)
rm(io); invisible(gc())

# Thresholds stored by scripts/13 must equal the configuration read here.
stopifnot(all(coal$gdp_floor_usd == gdp_floor), all(coal$supplier_share == supplier_share),
          all(coal$supplier_reserves_share == reserves_share))

# ---------------------------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------------------------
# The hand-picked coalition of the previous draft (history only; it survives in the code as the
# legacy/test default allied_network_default_iso3c()).
old23 <- c("USA", "CAN", "JPN", "AUS", "IND", "MEX", "KOR", "GBR", "DEU", "FRA", "ITA", "BRA", "SAU",
           "ZAF", "IDN", "NOR", "ARE", "VNM", "KEN", "DNK", "ARG", "MAR", "CHL")
stopifnot(length(unique(old23)) == 23)
# G20 member economies (the EU and the African Union are not countries).
g20 <- c("ARG", "AUS", "BRA", "CAN", "CHN", "FRA", "DEU", "IND", "IDN", "ITA", "JPN", "KOR", "MEX",
         "RUS", "SAU", "ZAF", "TUR", "GBR", "USA")
eu_code <- eu_cfg$code
eu_members <- unlist(eu_cfg$members)
stopifnot(length(eu_members) == 27)
headline_techs <- unlist(coal_cfg_all$headline_techs)
stages_updown <- unlist(coal_cfg_all$stages)

short_name <- c(
  USA = "United States", GBR = "United Kingdom", KOR = "Korea", RUS = "Russia", TUR = "Turkiye",
  COD = "DR Congo", DOM = "Dominican Rep.", SVK = "Slovakia", CZE = "Czechia", VNM = "Viet Nam",
  ARE = "UAE", ZAF = "South Africa", SAU = "Saudi Arabia", NZL = "New Zealand", PRK = "North Korea",
  IRN = "Iran", CHE = "Switzerland", VEN = "Venezuela"
)
name_of <- function(iso3) {
  nm <- coal$country[match(iso3, coal$iso3)]
  nm[is.na(nm)] <- geo$country[match(iso3[is.na(nm)], geo$iso3)]
  unname(ifelse(iso3 %in% names(short_name), short_name[iso3], ifelse(is.na(nm), iso3, nm)))
}
tech_short <- c(Batteries = "Batteries", Coal = "Coal", "Electric Grid" = "Grid",
                "Electric Vehicles" = "EVs", Gas = "Gas", Geothermal = "Geothermal",
                "Green Hydrogen" = "Hydrogen", Nuclear = "Nuclear", Oil = "Oil", Solar = "Solar",
                Wind = "Wind")
stage_short <- c(Upstream = "Up", Midstream = "Mid", Downstream = "Down")
reserves_measures <- COALITION_RESERVES_MEASURES
# The resource behind a reserves row: the mineral (USGS), the fuel (EI) or uranium (Red Book).
resource_of <- function(measure, mineral, tech) {
  dplyr::case_when(
    measure == "usgs_reserves" ~ ifelse(mineral == "Rare Earths", "Rare earth", mineral),
    measure == "ei_reserves" ~ tech,
    measure == "uranium_resources" ~ "Uranium",
    TRUE ~ NA_character_
  )
}
measure_short <- function(measure, mineral, tech) {
  min_txt <- tolower(ifelse(is.na(mineral), "", ifelse(mineral == "Rare Earths", "rare earth", mineral)))
  dplyr::case_when(
    measure == "atlas_export_share" ~ "exports",
    measure == "ei_production" ~ "production",
    measure == "uranium_mine_production" ~ "uranium mining",
    measure == "iea_manufacturing_capacity" ~ "manufacturing capacity",
    measure %in% c("iea_mineral_mining", "ei_mineral_mining") ~ paste(min_txt, "mining"),
    measure == "iea_mineral_refining" ~ paste(min_txt, "refining"),
    measure == "usgs_reserves" ~ paste(min_txt, "reserves"),
    measure == "ei_reserves" ~ paste(tolower(tech), "proved reserves"),
    measure == "uranium_resources" ~ "uranium resources",
    TRUE ~ measure
  )
}
english_number <- function(n) {
  words <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
  ifelse(n >= 1 & n <= 10, words[pmax(1, pmin(10, n))], as.character(n))
}
fmt_pct <- function(x, d = 1) formatC(100 * x, format = "f", digits = d)
r3 <- function(x) round(x, 3)
r4 <- function(x) round(x, 4)
# Missing scalars as JSON null (a logical NA), not the string "NA".
nn <- function(x) if (length(x) == 0 || all(is.na(x))) NA else x

# ---------------------------------------------------------------------------------------------
# 1. Reproduce the headline alignment and tiers from the raw ideal points
# ---------------------------------------------------------------------------------------------
read_ideal_points <- function(file) {
  raw <- utils::read.csv(data_path("raw", file), stringsAsFactors = FALSE, encoding = "UTF-8")
  geo_resolve_ideal_point_iso3(raw)$data
}
ip_all <- read_ideal_points("un_ga_ideal_points_all.csv")
ip_fp <- read_ideal_points("un_ga_ideal_points.csv")

membership_for <- function(ip, session_ids) {
  as.data.frame(geo_country_bloc_membership(
    ip, session_ids = session_ids, rule = align_cfg$bloc_rule, tau = as.numeric(align_cfg$tau),
    quantile = as.numeric(align_cfg$bloc_quantile),
    us_iso3 = geo_cfg$anchors$us_iso3, china_iso3 = geo_cfg$anchors$china_iso3
  ))
}
tiers_for <- function(bloc_tbl) {
  as.data.frame(derive_coalition_tiers(
    bloc_tbl, gdp_raw, agreements, sup,
    gdp_floor_usd = gdp_floor,
    supplier_share = supplier_share,
    supplier_reserves_share = reserves_share,
    partner_agreements = unlist(coal_cfg$partner_agreements),
    us_bloc = coal_cfg$us_bloc, china_bloc = coal_cfg$china_bloc,
    core_requires_own_vote = isTRUE(coal_cfg$core_requires_own_vote),
    supplier_requires_alignment = isTRUE(coal_cfg$supplier_requires_alignment),
    eu_members = eu_members, eu_code = eu_code, eu_node_tier = eu_cfg$tier
  ))
}

window_all <- geo_latest_sessions(ip_all, align_cfg$sessions)
head_rebuilt <- membership_for(ip_all, window_all)
own <- geo[geo$alignment_basis == "own_votes", , drop = FALSE]
k <- match(own$iso3, head_rebuilt$iso3)
stopifnot(!anyNA(k), nrow(own) == sum(!is.na(head_rebuilt$theta)),
          max(abs(own$theta - head_rebuilt$theta[k])) < 1e-9,
          max(abs(own$s_axis - head_rebuilt$s_axis[k])) < 1e-9,
          identical(own$bloc, head_rebuilt$bloc[k]))
tiers_rebuilt <- tiers_for(geo)
stopifnot(setequal(tiers_rebuilt$iso3[tiers_rebuilt$tier == "core"], coal$iso3[coal$tier == "core"]),
          setequal(tiers_rebuilt$iso3[tiers_rebuilt$tier == "partner"], coal$iso3[coal$tier == "partner"]))
rm(head_rebuilt, tiers_rebuilt)

# ---------------------------------------------------------------------------------------------
# 2. Axis, blocs and coverage
# ---------------------------------------------------------------------------------------------
s_us_cut <- unique(geo$s_us_cut)
s_cn_cut <- unique(geo$s_china_cut)
stopifnot(length(s_us_cut) == 1, length(s_cn_cut) == 1)
theta_us <- own$theta[own$iso3 == "USA"]
theta_cn <- own$theta[own$iso3 == "CHN"]
bloc_n <- table(own$bloc)
inherit <- geo[geo$alignment_basis == "inherited", c("iso3", "imputed_from")]
neutral <- geo$iso3[geo$alignment_basis == "neutral"]
beyond <- own$iso3[!is.na(own$beyond_anchor)]
fewer_sessions <- own$iso3[own$n_sessions < align_cfg$sessions]

near_cut <- own[abs(own$s_axis - s_us_cut) <= 0.04, c("iso3", "s_axis", "bloc")]
near_cut <- near_cut[order(-near_cut$s_axis), ]
near_cn_cut <- own[abs(own$s_axis - s_cn_cut) <= 0.02, c("iso3", "s_axis", "bloc")]
near_cn_cut <- near_cn_cut[order(-near_cn_cut$s_axis), ]

s_of <- function(iso3) own$s_axis[match(iso3, own$iso3)]
bloc_of <- function(iso3) geo$bloc[match(iso3, geo$iso3)]

# ---------------------------------------------------------------------------------------------
# 3. Coalition tiers, routes, EU node
# ---------------------------------------------------------------------------------------------
members <- coal[coal$tier %in% c("core", "partner"), , drop = FALSE]
core <- members$iso3[members$tier == "core"]
partners <- members$iso3[members$tier == "partner"]
node_only <- coal$iso3[coal$eu_member & coal$tier == "none"]
stopifnot(setequal(io_members$iso3, members$iso3),
          setequal(net_members$iso3[!net_members$node_only], members$iso3),
          setequal(net_members$iso3[net_members$node_only], node_only),
          nrow(nodes) == length(unique(c(members$network_node, eu_code))))
mem_flag <- function(iso3, col) members[[col]][match(iso3, members$iso3)]

route_agree_only <- partners[mem_flag(partners, "route_agreement") & !mem_flag(partners, "route_supplier")]
route_supp_only <- partners[!mem_flag(partners, "route_agreement") & mem_flag(partners, "route_supplier")]
route_both <- partners[mem_flag(partners, "route_agreement") & mem_flag(partners, "route_supplier")]
china_leaning_partners <- partners[mem_flag(partners, "bloc") == coal_cfg$china_bloc]
partner_bloc_n <- table(members$bloc[members$tier == "partner"])

agree_of <- function(iso3, which = c("all", "qualifying")) {
  which <- match.arg(which)
  col <- if (which == "all") "agreements" else "qualifying_agreements"
  coal[[col]][match(iso3, coal$iso3)]
}
split_agreements <- function(x) if (is.na(x)) character(0) else trimws(strsplit(x, ";")[[1]])
agreement_counts <- vapply(unlist(coal_cfg$partner_agreements), function(a) {
  sum(vapply(members$qualifying_agreements, function(x) a %in% split_agreements(x), logical(1)))
}, integer(1))
# Agreement members below the GDP floor (route needs GDP >= floor), not otherwise members.
agree_below_floor <- coal[!is.na(coal$qualifying_agreements) & !coal$route_agreement & coal$tier == "none",
                          c("iso3", "bloc", "gdp_usd", "qualifying_agreements", "eu_member")]
agree_below_floor <- agree_below_floor[order(-agree_below_floor$gdp_usd), ]

# Major-supplier cells: country rows, Upstream/Midstream of headline technologies, each row at
# its own threshold (flows 5%, reserves 10%; the `major` flag of major_supplier_shares()).
sup_c <- sup[!sup$is_aggregate & sup$tech %in% headline_techs & sup$supply_chain %in% stages_updown, ]
sup_c$reserves <- sup_c$measure %in% reserves_measures
stopifnot(all(sup_c$threshold == ifelse(sup_c$reserves, reserves_share, supplier_share)),
          all(sup_c$major == (!is.na(sup_c$world_share) & sup_c$world_share >= sup_c$threshold)))
major <- sup_c[sup_c$major %in% TRUE, ]
major <- major[order(major$iso3, -major$world_share), ]
major$resource <- resource_of(major$measure, major$mineral, major$tech)
major_flow <- major[!major$reserves, ]
major_res <- major[major$reserves, ]
top_flow <- major_flow[!duplicated(major_flow$iso3), ]
top_res <- major_res[!duplicated(major_res$iso3), ]
# Distinct qualifying (tech, stage) cells, either leg.
n_major_cells <- major %>% distinct(iso3, tech, supply_chain) %>% count(iso3, name = "n_cells")
major_stages <- major_flow %>% distinct(iso3, tech, supply_chain) %>% count(iso3, supply_chain) %>%
  tidyr::pivot_wider(names_from = supply_chain, values_from = n, values_fill = 0)
# Short label of the largest qualifying share: the flow cell when there is one, else the resource.
cell_label <- function(iso3) {
  vapply(iso3, function(i) {
    f <- top_flow[top_flow$iso3 == i, ]
    r <- top_res[top_res$iso3 == i, ]
    n <- n_major_cells$n_cells[n_major_cells$iso3 == i]
    if (nrow(f) == 0 && nrow(r) == 0) return("")
    lab <- if (nrow(f) > 0) {
      paste0(tech_short[f$tech], " ", stage_short[f$supply_chain], " ", fmt_pct(f$world_share, 0), "%")
    } else {
      paste0(r$resource, " reserves ", fmt_pct(r$world_share, 0), "%")
    }
    if (length(n) == 1 && n > 1) lab <- paste0(lab, " (+", n - 1, ")")
    lab
  }, character(1), USE.NAMES = FALSE)
}
detail_of <- function(d) {
  if (nrow(d) == 0) return(NA_character_)
  paste0(d$tech, " ", d$supply_chain, ": ", measure_short(d$measure, d$mineral, d$tech), " ",
         fmt_pct(d$world_share), "%")
}
supplier_detail <- function(iso3) {
  list(
    top_flow = detail_of(top_flow[top_flow$iso3 == iso3, ]),
    top_reserves = detail_of(top_res[top_res$iso3 == iso3, ]),
    reserves_qualifying = {
      x <- major_res[major_res$iso3 == iso3, ]
      x <- x[!duplicated(x$resource), ]
      if (nrow(x) == 0) list() else as.list(stats::setNames(r3(x$world_share), x$resource))
    },
    n_cells = n_major_cells$n_cells[match(iso3, n_major_cells$iso3)]
  )
}
excluded_suppliers <- coal$iso3[coal$supplier_excluded & coal$tier == "none"]
excluded_in_via_agreement <- coal$iso3[coal$supplier_excluded & coal$tier != "none"]
stopifnot(all(coal$bloc[match(excluded_suppliers, coal$iso3)] == coal_cfg$china_bloc))
excluded_reserves_only <- excluded_suppliers[!excluded_suppliers %in% major_flow$iso3]
# Every economy that passes the supplier test, by leg and bloc.
major_any <- unique(major$iso3)
major_blocs <- table(factor(ifelse(is.na(bloc_of(major_any)), "none", bloc_of(major_any)),
                            levels = c("U.S.-leaning", "Nonaligned", "China-leaning", "none")))
major_tier <- table(factor(coal$tier[match(major_any, coal$iso3)], levels = c("core", "partner", "none")))
major_flow_only <- setdiff(unique(major_flow$iso3), major_res$iso3)
major_res_only <- setdiff(unique(major_res$iso3), major_flow$iso3)
major_nonmember_other <- sort(setdiff(major_any[coal$tier[match(major_any, coal$iso3)] == "none"], excluded_suppliers))
sensitivity_add <- coal$iso3[coal$tier_with_sensitivity != coal$tier]

# The reserves leg.
partners_by_reserves_only <- partners[mem_flag(partners, "route_supplier_reserves") &
                                        !partners %in% major_flow$iso3 & !mem_flag(partners, "route_agreement")]
members_passing_reserves <- members$iso3[members$route_supplier_reserves]
# Non-members (other than China-leaning) whose largest reserves share lies in [5%, 10%), and the
# China-leaning ones.
res_near <- coal[coal$tier == "none" & !is.na(coal$max_reserves_share) &
                   coal$max_reserves_share >= supplier_share & coal$max_reserves_share < reserves_share,
                 c("iso3", "bloc", "max_reserves_share", "max_supplier_share")]
res_near <- res_near[order(-res_near$max_reserves_share), ]
res_near$resource <- vapply(res_near$iso3, function(i) {
  x <- sup_c[sup_c$iso3 == i & sup_c$reserves & !is.na(sup_c$world_share), ]
  x <- x[order(-x$world_share), ]
  resource_of(x$measure[1], x$mineral[1], x$tech[1])
}, character(1))

# World GDP coverage (WDI latest, economies in coalition_tbl, aggregates excluded there).
gdp_all <- coal[!is.na(coal$gdp_usd), c("iso3", "gdp_usd", "tier", "eu_member")]
gdp_share <- function(set) sum(gdp_all$gdp_usd[gdp_all$iso3 %in% set]) / sum(gdp_all$gdp_usd)
gdp_years <- table(coal$gdp_year[coal$iso3 %in% members$iso3])

# The 22 Up/Mid cells: share of world supply held by the coalition (members + node-only EU),
# by China, and by the other China-leaning major suppliers kept out, under the export-share
# measure (Atlas, complete country listing), the mineral mining and refining measures and the
# reserves measures.
network_economies <- c(members$iso3, node_only)
supply_cover <- sup_c %>%
  mutate(group = case_when(iso3 %in% network_economies ~ "coalition",
                           iso3 == "CHN" ~ "china",
                           iso3 %in% excluded_suppliers ~ "excluded_other",
                           TRUE ~ "rest")) %>%
  group_by(tech, supply_chain, measure, mineral, group) %>%
  summarise(share = sum(world_share, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = group, values_from = share, values_fill = 0)
exp_cover <- supply_cover[supply_cover$measure == "atlas_export_share", ]
min_cover <- supply_cover[supply_cover$measure %in% c("iea_mineral_mining", "iea_mineral_refining"), ]
res_cover <- supply_cover[supply_cover$measure %in% reserves_measures, ]
res_cover$resource <- resource_of(res_cover$measure, res_cover$mineral, res_cover$tech)
res_cover <- res_cover[!duplicated(res_cover[, c("measure", "resource")]), ]

# EU node.
eu_cells <- unique(euw[, c("tech", "supply_chain", "basis", "measures", "year", "eu_world_share", "coverage")])
eu_basis_n <- table(eu_cells$basis)
eu_max <- euw %>% group_by(tech, supply_chain, basis) %>% slice_max(weight, n = 1, with_ties = FALSE) %>%
  ungroup()
eu_hhi <- euw %>% group_by(tech, supply_chain, basis) %>% summarise(hhi = sum(weight^2), .groups = "drop")
eu_gdp_w <- euw[euw$basis == "gdp", ]
eu_gdp_w <- eu_gdp_w[!duplicated(eu_gdp_w$iso3), c("iso3", "weight")]
eu_gdp_w <- eu_gdp_w[order(-eu_gdp_w$weight), ]
node_only_gdp_share <- sum(eu_gdp_w$weight[eu_gdp_w$iso3 %in% node_only])
eu_node_gdp <- nodes$gdp_usd[nodes$network_node == eu_code]
node_gdp_rank <- rank(-nodes$gdp_usd)[nodes$network_node == eu_code]
node_only_max_weight <- euw %>% filter(iso3 %in% node_only) %>% slice_max(weight, n = 1, with_ties = FALSE)

# Policy basis of the members (a Strategic Index pick needs a Policy index; IEA PAMS is required
# and the CAT rating is added where it exists).
pol_elig$iso3 <- country_name_to_iso3(pol_elig$Country)
member_pol <- pol_elig[pol_elig$iso3 %in% members$iso3, ]
stopifnot(!anyDuplicated(member_pol$iso3))
members_without_policy <- sort(setdiff(members$iso3, member_pol$iso3[member_pol$eligible]))
members_pams_only <- sort(member_pol$iso3[member_pol$eligible & member_pol$policy_basis == "IEA PAMS only"])
policy_basis_n <- table(pol_elig$policy_basis[pol_elig$eligible])

# Comparison with the hand-picked list.
member_set <- members$iso3
old_kept <- intersect(old23, member_set)
old_left <- setdiff(old23, member_set)
new_joined <- setdiff(member_set, old23)
old_left_detail <- lapply(old_left, function(i) list(
  iso3 = i, bloc = bloc_of(i), s = r3(s_of(i)),
  gdp_usd_bn = round(coal$gdp_usd[coal$iso3 == i] / 1e9, 1),
  agreements = agree_of(i), max_supplier_share = r3(coal$max_supplier_share[coal$iso3 == i]),
  max_reserves_share = r3(coal$max_reserves_share[coal$iso3 == i])
))
old_kept_tier <- table(factor(members$tier[match(old_kept, members$iso3)], levels = c("core", "partner")))
old_routes <- members$routes[match(old_kept, members$iso3)]

# ---------------------------------------------------------------------------------------------
# 4. Robustness of the axis: single session and final-passage series
# ---------------------------------------------------------------------------------------------
alt_specs <- list(
  all_2023 = list(ip = ip_all, sessions = utils::tail(window_all, 1), label = "All votes, session 78 (2023)"),
  fp_5 = list(ip = ip_fp, sessions = geo_latest_sessions(ip_fp, align_cfg$sessions),
              label = "Final-passage votes, latest five sessions")
)
alt_results <- lapply(names(alt_specs), function(nm) {
  sp <- alt_specs[[nm]]
  m <- membership_for(sp$ip, sp$sessions)
  m <- m[!is.na(m$theta), ]
  both <- intersect(own$iso3, m$iso3)
  s_h <- own$s_axis[match(both, own$iso3)]
  s_a <- m$s_axis[match(both, m$iso3)]
  b_h <- own$bloc[match(both, own$iso3)]
  b_a <- m$bloc[match(both, m$iso3)]
  t_a <- tiers_for(m)
  mem_a <- t_a$iso3[t_a$tier %in% c("core", "partner")]
  core_a <- t_a$iso3[t_a$tier == "core"]
  list(
    spec = nm, label = sp$label,
    sessions = paste(sp$sessions, collapse = ","),
    years = paste(sort(unique(sp$ip$year[sp$ip$session %in% sp$sessions])), collapse = ","),
    n_countries = nrow(m), n_common = length(both),
    spearman_s = r3(stats::cor(s_h, s_a, method = "spearman")),
    pearson_s = r3(stats::cor(s_h, s_a)),
    cuts = list(us = r3(unique(m$s_us_cut)), china = r3(unique(m$s_china_cut))),
    same_bloc_share = r3(mean(b_h == b_a)),
    n_bloc_changes = sum(b_h != b_a),
    bloc_changes_members = {
      x <- both[b_h != b_a & both %in% c(member_set, "CHN", "RUS")]
      paste0(x, ": ", b_h[match(x, both)], " -> ", b_a[match(x, both)])
    },
    n_members = length(mem_a), n_core = length(core_a), n_partner = sum(t_a$tier == "partner"),
    core_entering = sort(setdiff(core_a, core)), core_leaving = sort(setdiff(core, core_a)),
    members_entering = sort(setdiff(mem_a, member_set)), members_leaving = sort(setdiff(member_set, mem_a)),
    core_to_partner = sort(intersect(setdiff(core, core_a), mem_a)),
    s_selected = {
      x <- c("RUS", "KAZ", "MMR", "SAU", "IND", "NZL", "MYS", "TUR", "CHE", "BRA")
      list(s = as.list(stats::setNames(r3(m$s_axis[match(x, m$iso3)]), x)),
           bloc = as.list(stats::setNames(m$bloc[match(x, m$iso3)], x)))
    },
    bloc_counts = as.list(table(m$bloc))
  )
})
names(alt_results) <- names(alt_specs)
rm(ip_all, ip_fp, alt_specs); invisible(gc())

# ---------------------------------------------------------------------------------------------
# 5. Dyadic alignment among the network nodes (plus China and Russia)
# ---------------------------------------------------------------------------------------------
node_codes <- nodes$network_node
eu_s <- with(geo[geo$iso3 %in% eu_members, ], {
  w <- coal$gdp_usd[match(iso3, coal$iso3)]
  sum(w * s_axis) / sum(w)
})
eu_s_range <- range(s_of(eu_members))
unit_s_of <- function(u) ifelse(u == eu_code, eu_s, s_of(u))
core_nodes <- nodes$network_node[nodes$tier == "core"]
partner_nodes <- nodes$network_node[nodes$tier == "partner"]
outside <- c("RUS", "CHN")
ord_by_s <- function(u) u[order(-unit_s_of(u), u)]
unit_order <- c(ord_by_s(core_nodes), ord_by_s(partner_nodes), ord_by_s(outside))
stopifnot(!anyNA(unit_s_of(unit_order)), length(unit_order) == nrow(nodes) + 2)
a_lookup <- function(i, j) {
  ii <- if (i == eu_code) eu_members else i
  jj <- if (j == eu_code) eu_members else j
  v <- al$geo_alignment[al$reporter_iso %in% ii & al$partner_iso %in% jj]
  if (length(v) == 0) NA_real_ else mean(v)
}
amat <- expand.grid(i = unit_order, j = unit_order, stringsAsFactors = FALSE)
amat <- amat[amat$i != amat$j, ]
amat$A <- mapply(a_lookup, amat$i, amat$j)
stopifnot(!anyNA(amat$A), all(amat$A %in% c(0, 0.5, 1)))
node_pairs <- amat[amat$i %in% node_codes & amat$j %in% node_codes & amat$i < amat$j, ]
pair_shares <- prop.table(table(factor(node_pairs$A, levels = c(1, 0.5, 0))))
# Member level (EU members individually).
mem_pairs <- al[al$reporter_iso %in% member_set & al$partner_iso %in% member_set &
                  al$reporter_iso < al$partner_iso, ]
mem_pair_shares <- prop.table(table(factor(mem_pairs$geo_alignment, levels = c(1, 0.5, 0))))
core_pairs <- al[al$reporter_iso %in% core & al$partner_iso %in% core & al$reporter_iso < al$partner_iso, ]
# Mean alignment of each partner with the core members.
partner_core_A <- al %>% filter(reporter_iso %in% partners, partner_iso %in% core) %>%
  group_by(reporter_iso) %>% summarise(A = mean(geo_alignment), .groups = "drop")
# Bloc of the partners -> their alignment with every core member.
partner_A_by_bloc <- table(members$bloc[match(partner_core_A$reporter_iso, members$iso3)], partner_core_A$A)
rm(al); invisible(gc())

# ---------------------------------------------------------------------------------------------
# Figure 1: coalition_axis
# ---------------------------------------------------------------------------------------------
group_levels <- c("Core", "Partner", "EU node only", "Not a member")
ax <- own[, c("iso3", "s_axis", "bloc")]
ax$tier <- coal$tier[match(ax$iso3, coal$iso3)]
ax$eu_member <- coal$eu_member[match(ax$iso3, coal$iso3)]
ax$group <- dplyr::case_when(
  ax$tier == "core" ~ "Core", ax$tier == "partner" ~ "Partner",
  ax$eu_member %in% TRUE ~ "EU node only", TRUE ~ "Not a member"
)
ax$major <- ax$iso3 %in% major$iso3
group_n <- table(factor(ax$group, levels = group_levels))
stopifnot(group_n[["Core"]] == length(core), group_n[["Partner"]] == length(partners),
          group_n[["EU node only"]] == length(node_only))

# Deterministic beeswarm, in inches (coord_fixed keeps the units equal on both axes).
axis_w <- 5.25
dot_d <- 0.072
swarm <- function(x, d = dot_d, step = dot_d / 2) {
  ord <- order(x)
  y <- rep(NA_real_, length(x))
  cand <- c(0, as.vector(rbind(seq_len(80), -seq_len(80)))) * step
  for (k in ord) {
    placed <- which(!is.na(y))
    for (yc in cand) {
      if (length(placed) == 0 || all((x[placed] - x[k])^2 + (y[placed] - yc)^2 >= d^2 * 0.999)) {
        y[k] <- yc
        break
      }
    }
  }
  y
}
ax$x <- ax$s_axis * axis_w
ax$dy <- NA_real_
for (g in group_levels) ax$dy[ax$group == g] <- swarm(ax$x[ax$group == g])
row_ext <- tapply(abs(ax$dy), factor(ax$group, levels = group_levels), max) + dot_d / 2
# Room above and below each strip for its labels (none in the EU node-only strip).
label_room <- c(Core = 0.36, Partner = 0.36, "EU node only" = 0.08, "Not a member" = 0.3)
row_gap <- 2 * label_room
centers <- numeric(0)
y_cur <- 0
for (g in rev(group_levels)) {
  y_cur <- y_cur + row_ext[[g]] + row_gap[[g]] / 2
  centers[g] <- y_cur
  y_cur <- y_cur + row_ext[[g]] + row_gap[[g]] / 2
}
ax$y <- centers[ax$group] + ax$dy
y_top <- y_cur

labelled <- c(g20, "VNM", "ISR", "UKR", "NZL", "CHE", "ARE", "KEN", "KAZ", "IRN", "PRK", "COD", "CHL",
              "QAT", "VEN")
ax$lab <- ifelse(ax$iso3 %in% labelled, name_of(ax$iso3), NA_character_)
ax$emph <- !is.na(ax$lab)
# Labels sit just outside their strip: above for points in the upper half of the swarm, below
# for the lower half, alternating along s for points on the centre line; ggrepel then spreads
# them along x only.
lab_df <- ax[ax$emph, ]
lab_df <- lab_df[order(lab_df$group, lab_df$x), ]
lab_df$dir <- NA_real_
for (g in unique(lab_df$group)) {
  sel <- which(lab_df$group == g)
  alt <- rep(c(1, -1), length.out = length(sel))
  lab_df$dir[sel] <- ifelse(lab_df$dy[sel] > dot_d / 4, 1, ifelse(lab_df$dy[sel] < -dot_d / 4, -1, alt))
}
# Hand placement where the China-leaning end of the non-member strip is crowded below the strip.
dir_override <- c(IRN = 1)
lab_df$dir[lab_df$iso3 %in% names(dir_override)] <- dir_override[lab_df$iso3[lab_df$iso3 %in% names(dir_override)]]
lab_df$target <- centers[lab_df$group] + lab_df$dir * (row_ext[lab_df$group] + 0.1)
lab_df$nudge <- lab_df$target - lab_df$y

bloc_band <- data.frame(
  xmin = c(0, s_cn_cut, s_us_cut) * axis_w, xmax = c(s_cn_cut, s_us_cut, 1) * axis_w,
  fill = c("China-leaning", "Nonaligned", "U.S.-leaning")
)
bloc_lab <- data.frame(
  x = c(0, (s_cn_cut + s_us_cut) / 2, 1) * axis_w, hjust = c(0, 0.5, 1),
  y = y_top + 0.06,
  label = c(sprintf("China-leaning (%d)\ns ≤ %.3f", bloc_n[["China-leaning"]], s_cn_cut),
            sprintf("Nonaligned (%d)", bloc_n[["Nonaligned"]]),
            sprintf("U.S.-leaning (%d)\ns ≥ %.3f", bloc_n[["U.S.-leaning"]], s_us_cut))
)
row_lab <- data.frame(y = centers[group_levels],
                      label = sprintf("%s (%d)", group_levels, as.integer(group_n[group_levels])))

p_axis <- ggplot(ax) +
  geom_rect(data = bloc_band, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = y_top, fill = fill),
            inherit.aes = FALSE, alpha = 0.10, show.legend = FALSE) +
  scale_fill_manual(values = c("China-leaning" = paper_div[["high"]], "Nonaligned" = "#ffffff",
                               "U.S.-leaning" = paper_div[["low"]])) +
  geom_vline(xintercept = c(s_cn_cut, s_us_cut) * axis_w, colour = paper_ink$muted, linewidth = 0.3) +
  geom_hline(yintercept = centers, colour = paper_ink$grid, linewidth = 0.25) +
  geom_point(aes(x = x, y = y, shape = major, alpha = emph), size = 1.55, stroke = 0.35,
             colour = paper_ink$primary, fill = paper_ink$secondary) +
  scale_shape_manual(values = c(`TRUE` = 21, `FALSE` = 1),
                     labels = c(`TRUE` = "Major supplier (≥ 5% of world supply or ≥ 10% of world reserves)",
                                `FALSE` = "Other economy"),
                     breaks = c("TRUE", "FALSE"), name = NULL) +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.45), guide = "none") +
  geom_text_repel(data = lab_df, aes(x = x, y = y, label = lab), nudge_y = lab_df$nudge,
                  direction = "x", size = 2.55, colour = paper_ink$primary,
                  min.segment.length = 0, segment.size = 0.2, segment.colour = paper_ink$muted,
                  box.padding = 0.12, point.padding = 0, max.overlaps = Inf, seed = 11,
                  force = 2, force_pull = 0.5) +
  geom_text(data = bloc_lab, aes(x = x, y = y, label = label, hjust = hjust), size = 2.65, lineheight = 0.95,
            colour = paper_ink$secondary, vjust = 0) +
  scale_x_continuous(breaks = seq(0, 1, 0.1) * axis_w, labels = sprintf("%.1f", seq(0, 1, 0.1)),
                     expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(breaks = row_lab$y, labels = row_lab$label, expand = expansion(add = c(0.02, 0.4))) +
  coord_fixed(ratio = 1, clip = "off") +
  labs(x = "Position on the U.S.–China axis, s (0 = China, 1 = United States; UNGA sessions 74–78)",
       y = NULL) +
  theme_paper(grid = "none") +
  theme(axis.text.y = element_text(size = 8, colour = paper_ink$primary, hjust = 1),
        legend.position = "bottom", legend.margin = margin(t = 0))
# Height from the fixed-ratio panel (y_top + expansion) plus the axis, legend and margins.
save_paper_fig(p_axis, "coalition_axis", width = paper_width_full, height = y_top + 0.42 + 0.95)

# ---------------------------------------------------------------------------------------------
# Figure 2: coalition_routes
# ---------------------------------------------------------------------------------------------
agree_cols <- unlist(coal_cfg$partner_agreements)
route_cols <- c(agree_cols, "Upstream", "Midstream", "Reserves")
rt <- members[, c("iso3", "tier", "gdp_usd", "bloc", "agreements", "route_supplier", "supplier_excluded")]
rt$name <- name_of(rt$iso3)
rt$name <- ifelse(rt$bloc == coal_cfg$china_bloc, paste0(rt$name, "†"), rt$name)
n_res_of <- function(i) sum(major_res$iso3 == i)
rt_long <- tidyr::expand_grid(iso3 = rt$iso3, col = route_cols) %>%
  left_join(rt, by = "iso3") %>%
  mutate(
    in_agreement = mapply(function(a, col) !is.na(a) && col %in% trimws(strsplit(a, ";")[[1]]), agreements, col),
    n_stage = mapply(function(i, col) {
      if (col == "Reserves") return(n_res_of(i))
      if (!col %in% c("Upstream", "Midstream")) return(0L)
      v <- major_stages[[col]][match(i, major_stages$iso3)]
      if (length(v) == 0 || is.na(v)) 0L else as.integer(v)
    }, iso3, col),
    fill_key = case_when(
      col %in% agree_cols & in_agreement ~ "Agreement member",
      col %in% c("Upstream", "Midstream", "Reserves") & n_stage > 0 & !supplier_excluded ~ col,
      col %in% c("Upstream", "Midstream", "Reserves") & n_stage > 0 & supplier_excluded ~ "Excluded",
      TRUE ~ "none"
    )
  )
rt_text <- rt %>% mutate(label = cell_label(iso3))
key_breaks <- c("Agreement member", "Upstream", "Midstream", "Reserves", "Excluded")
key_labels <- c("Member of the agreement", "Major supplier, Upstream", "Major supplier, Midstream",
                "Major reserves holder (≥ 10% of world reserves)", "China-leaning supplier, not counted")
reserves_fill <- paper_seq_blue[[2]]
routes_panel <- function(tier_name, title, n_rows) {
  d <- rt_long[rt_long$tier == tier_name, ]
  ord <- rt[rt$tier == tier_name, ]
  ord <- ord$name[order(-ord$gdp_usd)]
  d$name <- factor(d$name, levels = rev(ord))
  d$col <- factor(d$col, levels = route_cols)
  d$fill_key <- factor(d$fill_key, levels = c(key_breaks, "none"))
  tx <- rt_text[rt_text$tier == tier_name, ]
  tx$name <- factor(tx$name, levels = rev(ord))
  fills <- c("Agreement member" = paper_ink$secondary, Upstream = paper_stage_cols[["Upstream"]],
             Midstream = paper_stage_cols[["Midstream"]], Reserves = reserves_fill,
             Excluded = "#ffffff", none = paper_ink$neutral)
  outl <- c("Agreement member" = NA, Upstream = NA, Midstream = NA, Reserves = NA,
            Excluded = paper_ink$secondary, none = NA)
  ggplot(d, aes(x = col, y = name)) +
    # show.legend = TRUE: the core panel, which supplies the legend, has no China-leaning supplier,
    # and ggplot2 >= 3.5 leaves keys of unobserved levels empty otherwise.
    geom_tile(aes(fill = fill_key, colour = fill_key), width = 0.8, height = 0.74, linewidth = 0.45,
              show.legend = TRUE) +
    scale_fill_manual(values = fills, breaks = key_breaks, labels = key_labels, name = NULL, drop = FALSE) +
    scale_colour_manual(values = outl, breaks = key_breaks, labels = key_labels, name = NULL, drop = FALSE, guide = "none",
                        na.value = NA) +
    geom_vline(xintercept = length(agree_cols) + 0.5, colour = paper_ink$axis, linewidth = 0.3) +
    geom_text(data = tx, aes(x = length(route_cols) + 0.6, y = name, label = label), hjust = 0,
              size = 2.3, colour = paper_ink$primary, inherit.aes = FALSE) +
    scale_x_discrete(position = "top", expand = expansion(add = c(0.55, 0.4)),
                     labels = c(agree_cols, "Up", "Mid", "Reserves"), drop = FALSE) +
    coord_cartesian(clip = "off", ylim = c(0.5, n_rows + 0.5)) +
    labs(x = NULL, y = NULL, title = title) +
    theme_paper(grid = "none") +
    theme(axis.line.x = element_blank(),
          axis.text.x.top = element_text(size = 7, angle = 90, hjust = 0, vjust = 0.5, colour = paper_ink$primary),
          axis.text.y = element_text(size = 7, colour = paper_ink$primary),
          plot.title = element_text(size = 8.5, face = "bold"),
          plot.margin = margin(4, 84, 2, 2), legend.position = "none")
}
n_core <- length(core)
p_core <- routes_panel("core", sprintf("Core (%d): U.S.-leaning, GDP ≥ USD 100bn", n_core), n_core)
p_part <- routes_panel("partner", sprintf("Partners (%d): agreement or supplier route", length(partners)),
                       length(partners))
legend_src <- p_core + theme(legend.position = "bottom", legend.text = element_text(size = 7.5)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(colour = c(NA, NA, NA, NA, paper_ink$secondary), linewidth = 0.45)))

leg <- cowplot::get_plot_component(legend_src, "guide-box-bottom", return_all = TRUE)
if (is.list(leg) && !inherits(leg, "grob")) leg <- leg[[which(!vapply(leg, inherits, logical(1), "zeroGrob"))[1]]]
# Equal row heights: the partner panel is shortened to its share of rows.
row_in <- 0.128
head_in <- 0.72
h_core <- head_in + n_core * row_in
h_part <- head_in + length(partners) * row_in
leg_in <- 0.42
p_routes <- cowplot::plot_grid(
  cowplot::plot_grid(
    p_core,
    cowplot::plot_grid(p_part, NULL, ncol = 1, rel_heights = c(h_part, h_core - h_part)),
    nrow = 1, rel_widths = c(0.97, 1.03)
  ),
  leg, ncol = 1, rel_heights = c(h_core, leg_in)
) + theme(plot.background = element_rect(fill = "white", colour = NA))
save_paper_fig(p_routes, "coalition_routes", width = paper_width_full, height = h_core + leg_in)

# ---------------------------------------------------------------------------------------------
# Figure 3: coalition_alignment_matrix
# ---------------------------------------------------------------------------------------------
unit_lab <- ifelse(unit_order == eu_code, "EU", unit_order)
amat$i_f <- factor(amat$i, levels = unit_order, labels = unit_lab)
amat$j_f <- factor(amat$j, levels = rev(unit_order), labels = rev(unit_lab))
amat$A_f <- factor(amat$A, levels = c(1, 0.5, 0),
                   labels = c("Same bloc (A = 1)", "Either nonaligned (A = 0.5)", "Opposite blocs (A = 0)"))
n_core_nodes <- length(core_nodes)
n_nodes <- nrow(nodes)
n_units <- length(unit_order)
sep <- c(n_core_nodes, n_nodes) + 0.5
p_mat <- ggplot(amat, aes(x = i_f, y = j_f, fill = A_f)) +
  geom_tile(colour = "white", linewidth = 0.25) +
  scale_fill_manual(values = c(paper_div[["low"]], "#dcdad3", paper_div[["high"]]), name = NULL, drop = FALSE) +
  annotate("segment", x = sep, xend = sep, y = 0.5, yend = n_units + 0.5, colour = paper_ink$primary, linewidth = 0.35) +
  annotate("segment", x = 0.5, xend = n_units + 0.5, y = n_units + 1 - sep, yend = n_units + 1 - sep,
           colour = paper_ink$primary, linewidth = 0.35) +
  annotate("text", x = c(n_core_nodes / 2 + 0.5, (n_core_nodes + n_nodes) / 2 + 0.5, n_nodes + 0.65),
           y = n_units + 1.3, hjust = c(0.5, 0.5, 0),
           label = c(sprintf("Core nodes (%d)", n_core_nodes), sprintf("Partner nodes (%d)", length(partner_nodes)),
                     "Outside"), size = 2.6, colour = paper_ink$secondary, vjust = 0) +
  scale_x_discrete(position = "bottom", expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  coord_fixed(clip = "off") +
  labs(x = NULL, y = NULL) +
  theme_paper(grid = "none") +
  theme(axis.text.x = element_text(size = 6.4, angle = 90, hjust = 1, vjust = 0.5, colour = paper_ink$primary),
        axis.text.y = element_text(size = 6.4, colour = paper_ink$primary),
        axis.line.x = element_blank(), legend.position = "bottom",
        plot.margin = margin(16, 26, 4, 4))
save_paper_fig(p_mat, "coalition_alignment_matrix", width = paper_width_full, height = 6.55)

# ---------------------------------------------------------------------------------------------
# Table: coalition_composition.tex
# ---------------------------------------------------------------------------------------------
tex_esc <- function(x) gsub("%", "\\\\%", gsub("&", "\\\\&", x))
tex_names <- function(iso3, extra = NULL) {
  if (length(iso3) == 0) return("--")
  nm <- sub("^Turkiye$", "T\\\\\"urkiye", tex_esc(name_of(iso3)))
  if (!is.null(extra)) nm <- paste0(nm, " (", extra, ")")
  paste(nm, collapse = ", ")
}
by_gdp <- function(iso3) iso3[order(-coal$gdp_usd[match(iso3, coal$iso3)], iso3)]
agree_txt <- function(iso3) gsub("; ", ", ", coal$qualifying_agreements[match(iso3, coal$iso3)])
supp_txt <- function(iso3) tex_esc(cell_label(iso3))
ra <- by_gdp(route_agree_only); rs <- by_gdp(route_supp_only); rb <- by_gdp(route_both)
ex <- by_gdp(excluded_suppliers)
rn <- res_near$iso3[res_near$bloc != coal_cfg$china_bloc]
rows <- list(
  c("Core: U.S.-leaning bloc, GDP $\\geq$ USD 100bn", length(core), tex_names(by_gdp(core))),
  c("Partners, agreement route only", length(ra), tex_names(ra, agree_txt(ra))),
  c("Partners, supplier route only", length(rs), tex_names(rs, supp_txt(rs))),
  c("Partners, both routes", length(rb), tex_names(rb, agree_txt(rb))),
  c("EU members in the EU node only", length(node_only), tex_names(by_gdp(node_only))),
  c("Major suppliers kept out (China-leaning)", length(ex), tex_names(ex, supp_txt(ex))),
  c("Reserves holders between 5\\% and 10\\%, not members", length(rn),
    tex_names(rn, paste0(res_near$resource[match(rn, res_near$iso3)], " ",
                         fmt_pct(res_near$max_reserves_share[match(rn, res_near$iso3)]), "\\%"))),
  c("Added by the MSP Forum sensitivity", length(sensitivity_add), tex_names(by_gdp(sensitivity_add))),
  c("Hand-picked list of the previous draft: not members", length(old_left), tex_names(old_left)),
  c("Hand-picked list of the previous draft: members", length(old_kept),
    sprintf("%d in the core, %d partners", as.integer(old_kept_tier[["core"]]), as.integer(old_kept_tier[["partner"]])))
)
tab_lines <- c(
  "% Generated by scripts/paper/fig_coalition.R from data/processed/coalition_tbl.rds,",
  "% geo_ideal_points_tbl.rds and major_supplier_shares_tbl.rds",
  "\\begin{table}[htbp]",
  "\\centering",
  "\\begin{threeparttable}",
  "\\caption{Composition of the allied coalition}",
  "\\label{tab:coalition_composition}",
  "\\footnotesize",
  "\\begin{tabularx}{\\textwidth}{@{}>{\\raggedright\\arraybackslash}p{0.27\\textwidth} r >{\\raggedright\\arraybackslash}X@{}}",
  "\\toprule",
  "Group & $n$ & Economies \\\\",
  "\\midrule",
  vapply(rows, function(r) paste0(r[1], " & ", r[2], " & ", r[3], " \\\\"), character(1)),
  "\\bottomrule",
  "\\end{tabularx}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0(
    "\\item \\emph{Notes:} Economies are ordered by GDP within a group. Core: the U.S.-leaning quartile ",
    sprintf("bloc of the all-votes ideal points of \\citet{bailey2017estimating} over UNGA sessions 74--78 (2019--2023), $s\\geq %.3f$, ", s_us_cut),
    "with the economy's own UN votes and latest World Bank GDP of at least USD 100bn. Partners (not core): ",
    "a member of a U.S. free trade agreement, IPEF, the Minerals Security Partnership (MSP), the G7 or NATO with GDP ",
    "of at least USD 100bn (qualifying agreements in parentheses), or, without a GDP floor, an economy that is not ",
    "China-leaning and holds at least 5\\% of world supply in one measure of an Upstream or Midstream stage of a ",
    "headline technology (exports of the stage's HS6 basket, fuel or uranium production, mineral mining or refining, ",
    "or manufacturing capacity) or at least 10\\% of world reserves of a headline mineral, fuel or uranium. For the ",
    "supplier route, the parentheses give the largest qualifying share: the technology--stage cell of a supply measure, ",
    "or the resource when only reserves qualify, and in brackets the number of further qualifying cells. ",
    sprintf("The %s EU members below the GDP floor belong to the coalition only through the EU. ", english_number(length(node_only))),
    "The MSP Forum sensitivity admits Forum members by the agreement route. The previous draft's 23 members were fixed by hand."
  ),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)
tables_dir <- paper_dir("tables")
if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)
writeLines(enc2utf8(gsub("\\(\\+(\\d+)\\)", "[+\\1]", tab_lines)),
           file.path(tables_dir, "coalition_composition.tex"), useBytes = TRUE)

# ---------------------------------------------------------------------------------------------
# Numbers
# ---------------------------------------------------------------------------------------------
cell_cover <- function(d) {
  d <- d[order(-d$china), ]
  lapply(seq_len(nrow(d)), function(k) list(
    cell = paste(d$tech[k], d$supply_chain[k]), mineral = d$mineral[k],
    coalition = r3(d$coalition[k]), china = r3(d$china[k]), excluded_other = r3(d$excluded_other[k])
  ))
}
mineral_cover <- min_cover %>% distinct(measure, mineral, coalition, china, excluded_other) %>%
  arrange(measure, mineral)

res <- list(
  source = list(
    series = unique(geo$series), source_file = unique(geo$source_file), dataset_version = unique(geo$dataset_version),
    window_sessions = unique(own$window_sessions), window_years = unique(own$window_years),
    sessions = align_cfg$sessions, bloc_rule = align_cfg$bloc_rule, bloc_quantile = align_cfg$bloc_quantile,
    tau = align_cfg$tau, neutral_p = align_cfg$neutral_p,
    inputs_mtime = list(
      geo_ideal_points_tbl = format(file.mtime(data_path("processed", "geo_ideal_points_tbl.rds"))),
      coalition_tbl = format(file.mtime(data_path("processed", "coalition_tbl.rds"))),
      allied_network_nodes = format(file.mtime(data_path("processed", "outputs", "allied_network_nodes.csv")))
    )
  ),
  coverage = list(
    n_own_votes = nrow(own), n_inherited = nrow(inherit), n_neutral = length(neutral), neutral = neutral,
    n_alignment_rows = nrow(geo), n_with_alignment = sum(!is.na(geo$p_us)),
    inherit_sovereign_counts = as.list(table(inherit$imputed_from)),
    beyond_anchor_clamped = beyond,
    beyond_anchor_side = as.list(table(own$beyond_anchor[!is.na(own$beyond_anchor)])),
    fewer_sessions = fewer_sessions,
    fewer_sessions_n = as.list(stats::setNames(own$n_sessions[own$iso3 %in% fewer_sessions], own$iso3[own$iso3 %in% fewer_sessions])),
    theta_usa = r3(theta_us), theta_chn = r3(theta_cn)
  ),
  blocs = list(
    counts = as.list(bloc_n), s_us_cut = r3(s_us_cut), s_china_cut = r3(s_cn_cut),
    median_s = r3(stats::median(own$s_axis)), mean_s = r3(mean(own$s_axis)),
    share_s_below_half = r3(mean(own$s_axis < 0.5)),
    n_s_below_half = sum(own$s_axis < 0.5), n_s_above_half = sum(own$s_axis > 0.5),
    near_us_cut = lapply(seq_len(nrow(near_cut)), function(k) list(iso3 = near_cut$iso3[k], s = r3(near_cut$s_axis[k]), bloc = near_cut$bloc[k])),
    near_china_cut = lapply(seq_len(nrow(near_cn_cut)), function(k) list(iso3 = near_cn_cut$iso3[k], s = r3(near_cn_cut$s_axis[k]), bloc = near_cn_cut$bloc[k])),
    s_selected = {
      x <- unique(c(labelled, "IRL", "JPN", "SGP", "MYS", "OMN", "THA", "PHL", "GHA", "NAM", "GAB", "MAR", "LUX", "HUN"))
      as.list(stats::setNames(r3(s_of(x)), x))
    },
    bloc_selected = {
      x <- unique(c(labelled, "OMN", "JOR", "EGY", "KWT", "BHR"))
      as.list(stats::setNames(bloc_of(x), x))
    },
    us_leaning_below_floor = sort(geo_coal$iso3[geo_coal$bloc == "U.S.-leaning" & !geo_coal$member]),
    us_leaning_n_members = sum(geo_coal$member)
  ),
  dyadic = list(
    values = c(1, 0.5, 0),
    n_units = n_units, node_pairs = nrow(node_pairs),
    node_pair_counts = as.list(stats::setNames(as.integer(table(factor(node_pairs$A, levels = c(1, 0.5, 0)))), c("same", "nonaligned", "opposite"))),
    node_pair_shares = as.list(stats::setNames(r3(as.numeric(pair_shares)), c("same", "nonaligned", "opposite"))),
    member_pairs = nrow(mem_pairs),
    member_pair_shares = as.list(stats::setNames(r3(as.numeric(mem_pair_shares)), c("same", "nonaligned", "opposite"))),
    core_pairs_all_same = all(core_pairs$geo_alignment == 1), n_core_pairs = nrow(core_pairs),
    partner_mean_A_with_core = as.list(stats::setNames(r3(partner_core_A$A), partner_core_A$reporter_iso)),
    partner_A_with_core_by_bloc = lapply(split(as.data.frame(partner_A_by_bloc), as.data.frame(partner_A_by_bloc)$Var1),
                                         function(d) as.list(stats::setNames(d$Freq, as.character(d$Var2)))),
    eu_node_s_gdp_weighted = r3(eu_s), eu_members_s_range = r3(eu_s_range),
    fs_weight_geo_alignment = unname(fs_weights[["geo_alignment"]]),
    fs_weight_total = sum(fs_weights),
    fs_share_geo_alignment = r3(fs_weights[["geo_alignment"]] / sum(fs_weights)),
    fs_components_nonzero = names(fs_weights)[fs_weights > 0]
  ),
  coalition = list(
    gdp_floor_usd = gdp_floor, supplier_share = supplier_share, supplier_reserves_share = reserves_share,
    partner_agreements = unlist(coal_cfg$partner_agreements), sensitivity_agreements = unlist(coal_cfg$sensitivity_agreements),
    n_members = length(member_set), n_core = length(core), n_partners = length(partners),
    n_economies_with_node_only = length(network_economies),
    core = core, partners = partners,
    partner_blocs = as.list(partner_bloc_n),
    partners_agreement_only = route_agree_only, partners_supplier_only = route_supp_only, partners_both = route_both,
    n_agreement_route = length(route_agree_only) + length(route_both),
    n_supplier_route = length(route_supp_only) + length(route_both),
    members_by_qualifying_agreement = as.list(agreement_counts),
    china_leaning_partners = lapply(china_leaning_partners, function(i) list(iso3 = i, s = r3(s_of(i)), agreements = agree_of(i, "qualifying"))),
    core_with_supplier_route = sort(core[mem_flag(core, "route_supplier")]),
    core_with_agreement = sum(members$route_agreement[members$tier == "core"]),
    core_without_agreement = sort(core[!mem_flag(core, "route_agreement")]),
    excluded_suppliers = lapply(excluded_suppliers, function(i) c(list(iso3 = i, s = r3(s_of(i))), supplier_detail(i))),
    excluded_reserves_only = excluded_reserves_only,
    supplier_test = list(
      n_economies = length(major_any), by_bloc = as.list(major_blocs), by_tier = as.list(major_tier),
      n_flow_only = length(major_flow_only), n_reserves_only = length(major_res_only),
      n_both_legs = length(intersect(unique(major_flow$iso3), major_res$iso3)),
      reserves_only = sort(major_res_only),
      non_members_not_china_leaning = major_nonmember_other
    ),
    excluded_in_via_agreement = excluded_in_via_agreement,
    supplier_partner_top_cells = lapply(c(route_supp_only, route_both), function(i) c(list(iso3 = i, label = cell_label(i)), supplier_detail(i))),
    reserves = list(
      partners_by_reserves_only = partners_by_reserves_only,
      partners_by_reserves_only_detail = lapply(partners_by_reserves_only, function(i) c(list(iso3 = i), supplier_detail(i))),
      members_passing_reserves = members_passing_reserves, n_members_passing_reserves = length(members_passing_reserves),
      near_misses_5_to_10 = lapply(seq_len(nrow(res_near)), function(k) list(
        iso3 = res_near$iso3[k], bloc = res_near$bloc[k], resource = res_near$resource[k],
        max_reserves_share = r3(res_near$max_reserves_share[k]), max_supply_share = r3(res_near$max_supplier_share[k])))
    ),
    agreement_members_below_floor = lapply(seq_len(nrow(agree_below_floor)), function(k) list(
      iso3 = agree_below_floor$iso3[k], bloc = agree_below_floor$bloc[k],
      gdp_usd_bn = round(agree_below_floor$gdp_usd[k] / 1e9, 1), agreements = agree_below_floor$qualifying_agreements[k],
      eu_member = agree_below_floor$eu_member[k])),
    large_non_members = {
      x <- coal[coal$tier == "none" & !coal$eu_member & !is.na(coal$gdp_usd) & coal$gdp_usd >= gdp_floor, ]
      x <- x[order(-x$gdp_usd), ]
      lapply(seq_len(nrow(x)), function(k) list(iso3 = x$iso3[k], bloc = x$bloc[k], basis = x$alignment_basis[k],
                                                gdp_usd_bn = round(x$gdp_usd[k] / 1e9, 1),
                                                max_supplier_share = r3(x$max_supplier_share[k]),
                                                max_reserves_share = r3(x$max_reserves_share[k])))
    },
    msp_forum_sensitivity_adds = lapply(sensitivity_add, function(i) list(iso3 = i, bloc = bloc_of(i), s = r3(s_of(i)))),
    switzerland = list(s = r3(s_of("CHE")), bloc = bloc_of("CHE"), max_supplier_share = r3(coal$max_supplier_share[coal$iso3 == "CHE"]),
                       max_reserves_share = nn(r3(coal$max_reserves_share[coal$iso3 == "CHE"])),
                       gdp_usd_bn = round(coal$gdp_usd[coal$iso3 == "CHE"] / 1e9, 1),
                       agreements = nn(agree_of("CHE")), reason = coal$reason[coal$iso3 == "CHE"]),
    taiwan = list(basis = geo$alignment_basis[geo$iso3 == "TWN"], p_us = geo$p_us[geo$iso3 == "TWN"],
                  max_supplier_share = r3(coal$max_supplier_share[coal$iso3 == "TWN"]),
                  top_cell = detail_of({
                    x <- sup_c[sup_c$iso3 == "TWN" & !is.na(sup_c$world_share), ]
                    x[order(-x$world_share), ][1, ]
                  }),
                  gdp_usd = nn(coal$gdp_usd[coal$iso3 == "TWN"]), agreements = nn(agree_of("TWN")),
                  reason = coal$reason[coal$iso3 == "TWN"]),
    gdp_years = as.list(gdp_years),
    gdp_share_world = list(members = r3(gdp_share(member_set)), core = r3(gdp_share(core)),
                           partners = r3(gdp_share(partners)), network_economies = r3(gdp_share(network_economies)),
                           china = r3(gdp_share("CHN")))
  ),
  network_nodes = list(
    n_nodes = nrow(nodes), n_core_nodes = n_core_nodes, n_partner_nodes = length(partner_nodes),
    eu_code = eu_code, eu_members_in_node = length(eu_members), eu_members_core = sum(coal$eu_member & coal$tier == "core"),
    eu_node_only = node_only, eu_node_only_gdp_share_of_eu = r3(node_only_gdp_share),
    eu_node_only_max_weight = list(iso3 = node_only_max_weight$iso3, cell = paste(node_only_max_weight$tech, node_only_max_weight$supply_chain),
                                   weight = r3(node_only_max_weight$weight)),
    eu_node_gdp_usd_bn = round(eu_node_gdp / 1e9, 1), eu_node_gdp_rank_among_nodes = node_gdp_rank,
    usa_gdp_usd_bn = round(nodes$gdp_usd[nodes$network_node == "USA"] / 1e9, 1),
    core_nodes = core_nodes, partner_nodes = partner_nodes
  ),
  eu_weights = list(
    n_cells = nrow(eu_cells), basis_counts = as.list(eu_basis_n),
    min_coverage = coal_cfg_all$eu_weights$min_coverage, min_eu_world_share = coal_cfg_all$eu_weights$min_eu_world_share,
    cells_by_basis = lapply(split(paste(eu_cells$tech, eu_cells$supply_chain), eu_cells$basis), sort),
    gdp_weights_top = as.list(stats::setNames(r3(eu_gdp_w$weight[1:5]), eu_gdp_w$iso3[1:5])),
    max_weight_by_cell = lapply(seq_len(nrow(eu_max)), function(k) list(
      cell = paste(eu_max$tech[k], eu_max$supply_chain[k]), basis = eu_max$basis[k],
      iso3 = eu_max$iso3[k], weight = r3(eu_max$weight[k]))),
    hhi_by_basis = as.list(tapply(eu_hhi$hhi, eu_hhi$basis, function(x) r3(stats::median(x)))),
    hhi_range = r3(range(eu_hhi$hhi)),
    largest_weight_count_by_member = as.list(sort(table(eu_max$iso3), decreasing = TRUE)),
    max_weight_overall = r3(max(euw$weight)),
    eu_world_share_exports_range = r3(range(eu_cells$eu_world_share[eu_cells$basis == "exports"])),
    production_coverage = lapply(which(eu_cells$basis == "production"), function(k) list(
      cell = paste(eu_cells$tech[k], eu_cells$supply_chain[k]), eu_world_share = r3(eu_cells$eu_world_share[k]),
      coverage = r3(eu_cells$coverage[k])))
  ),
  supply_cover = list(
    years = lapply(split(sup_c$year, sup_c$measure), function(y) sort(unique(y))),
    note = sprintf("World shares of the %d network economies (%d members + %d EU node-only), China, and the %d other China-leaning major suppliers kept out, by Upstream/Midstream cell.",
                   length(network_economies), length(member_set), length(node_only), length(setdiff(excluded_suppliers, "CHN"))),
    exports = list(
      n_cells = nrow(exp_cover),
      coalition_median = r3(stats::median(exp_cover$coalition)), coalition_range = r3(range(exp_cover$coalition)),
      china_median = r3(stats::median(exp_cover$china)), china_range = r3(range(exp_cover$china)),
      excluded_other_median = r3(stats::median(exp_cover$excluded_other)),
      n_cells_coalition_above_china = sum(exp_cover$coalition > exp_cover$china),
      cells = cell_cover(exp_cover)
    ),
    minerals = lapply(seq_len(nrow(mineral_cover)), function(k) list(
      measure = mineral_cover$measure[k], mineral = mineral_cover$mineral[k],
      coalition = r3(mineral_cover$coalition[k]), china = r3(mineral_cover$china[k]),
      excluded_other = r3(mineral_cover$excluded_other[k]))),
    reserves = lapply(seq_len(nrow(res_cover)), function(k) list(
      measure = res_cover$measure[k], resource = res_cover$resource[k],
      coalition = r3(res_cover$coalition[k]), china = r3(res_cover$china[k]),
      excluded_other = r3(res_cover$excluded_other[k])))
  ),
  policy = list(
    n_policy_eligible = sum(pol_elig$eligible), policy_basis_counts = as.list(policy_basis_n),
    members_with_policy_index = length(member_set) - length(members_without_policy),
    members_without_policy_index = members_without_policy,
    members_iea_pams_only = members_pams_only, n_members_iea_pams_only = length(members_pams_only)
  ),
  old_list = list(
    n_old = length(old23), old = old23, kept = sort(old_kept), n_kept = length(old_kept),
    left = old_left_detail, joined = sort(new_joined), n_joined = length(new_joined),
    kept_by_tier = as.list(old_kept_tier),
    kept_partners = sort(old_kept[members$tier[match(old_kept, members$iso3)] == "partner"]),
    kept_routes = as.list(stats::setNames(old_routes, old_kept)),
    old_nodes = length(old23), new_nodes = nrow(nodes),
    joined_by_tier = as.list(table(members$tier[match(new_joined, members$iso3)])),
    joined_eu = sort(intersect(new_joined, eu_members)),
    joined_non_eu = sort(setdiff(new_joined, eu_members))
  ),
  robustness = alt_results,
  figures = c("coalition_axis", "coalition_routes", "coalition_alignment_matrix"),
  tables = "coalition_composition"
)
write_paper_results(res, "coalition")
message("Done: coalition.json, coalition_axis, coalition_routes, coalition_alignment_matrix, tables/coalition_composition.tex")
