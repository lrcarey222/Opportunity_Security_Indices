# US trade restrictiveness by technology x stage (R/strategic_position/trade_restrictiveness.R,
# docs/us_trade_restrictiveness.md): the Trade War Tracker TRI, the HS10 fallback, the rest-of-world split,
# Section 232 tags, the AD/CVD overlay and the posture verdicts. Synthetic fixtures.

tr_repo_root <- function() {
  normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
}

suppressPackageStartupMessages(library(dplyr))
source(file.path(tr_repo_root(), "R", "utils", "supply_chain_stages.R"))
source(file.path(tr_repo_root(), "R", "strategic_position", "supply_structure.R"))
source(file.path(tr_repo_root(), "R", "strategic_position", "trade_restrictiveness.R"))

test_that("tri_index is Waugh's formula: equal to the mean when tariffs are uniform, above it otherwise", {
  expect_equal(unname(tri_index(c(1, 3), c(10, 10))), c(10, 10))
  idx <- tri_index(c(1, 1), c(0, 20))
  expect_equal(unname(idx["tri"]), sqrt(200))
  expect_equal(unname(idx["mean"]), 10)
  expect_equal(unname(tri_index(c(2, 0, NA), c(5, 99, 99))), c(5, 5))
  expect_true(all(is.na(tri_index(numeric(), numeric()))))
})

test_that("month windows run back from the data month and map onto the weight year", {
  expect_equal(tri_month_window("2026-02", 3), c("2025-12", "2026-01", "2026-02"))
  expect_equal(tri_same_months(c("2025-12", "2026-01"), 2024), c("2024-12", "2024-01"))
})

tr_rows <- function() {
  data.frame(
    cty_code = c("A", "A", "A", "B", "TOTAL", "TOTAL", "TOTAL", "TOTAL"),
    hs10 = c("1111110000", "1111110000", "2222220000", "1111110000", "1111110000", "1111110000", "2222220000", "3333330000"),
    time = c("2024-03", "2026-08", "2024-05", "2024-01", "2024-03", "2026-08", "2024-05", "2024-07"),
    value = c(100, 50, 40, 60, 200, 80, 40, 30),
    duty = c(5, 10, 2, 0, 9, 14, 2, 3)
  )
}

test_that("tri_build_panel sums the weight year and the months, and adds a floored rest-of-world row", {
  p <- tri_build_panel(tr_rows(), 2024, "2026-08")
  expect_setequal(unique(p$cty_code), c("A", "B", "ROW"))
  a <- p[p$cty_code == "A" & p$hs10 == "1111110000", ]
  expect_equal(c(a$w, a$w_duty, a$`v_2026-08`, a$`d_2026-08`), c(100, 5, 50, 10))
  row1 <- p[p$cty_code == "ROW" & p$hs10 == "1111110000", ]
  expect_equal(c(row1$w, row1$w_duty, row1$`v_2026-08`), c(40, 4, 30))   # 200 - 100 - 60; 80 - 50
  expect_equal(p$w[p$cty_code == "ROW" & p$hs10 == "2222220000"], 0)      # TOTAL equals A: nothing left
  expect_equal(p$w[p$cty_code == "ROW" & p$hs10 == "3333330000"], 30)     # only in TOTAL
})

test_that("tri_line_rates falls back from HS10 to HS8 to HS6 within a partner, then leaves the line absent", {
  panel <- data.frame(
    cty_code = c("A", "A", "A", "A", "B"),
    hs10 = c("8507600020", "8507600030", "8507609000", "8507201000", "8507600020"),
    w = c(100, 0, 50, 10, 20), w_duty = c(3, 0, 1, 0, 1),
    `v_2026-08` = c(0, 40, 0, 5, 0), `d_2026-08` = c(0, 10, 0, 1, 0), check.names = FALSE
  )
  g <- tri_line_rates(panel, "2026-08")
  expect_equal(nrow(g), 4)                                  # weightless lines are not goods
  r <- stats::setNames(g$tau_collected, paste(g$cty_code, g$hs10))
  m <- stats::setNames(g$match, paste(g$cty_code, g$hs10))
  expect_equal(unname(m["A 8507600020"]), "HS8")            # re-coded: takes 8507.60.00's rate
  expect_equal(unname(r["A 8507600020"]), 25)
  expect_equal(unname(m["A 8507201000"]), "HS10")
  expect_equal(unname(r["A 8507201000"]), 20)
  expect_equal(unname(m["A 8507609000"]), "HS6")            # nothing at 8507.60.90, but 8507.60 traded
  expect_equal(unname(r["A 8507609000"]), 25)
  expect_true(is.na(r["B 8507600020"]))                     # partner B has no trade that month
  expect_equal(unname(m["B 8507600020"]), "absent")
  expect_equal(g$tau_universe[g$cty_code == "B"], 0)        # the tracker's convention
  expect_equal(g$tau_base[g$cty_code == "A" & g$hs10 == "8507600020"], 3)
})

test_that("tri_split_rest_of_world keeps total weight, caps the carve at ROW's weight and gives carved lines ROW's rates", {
  goods <- data.frame(cty_code = c("ROW", "ROW", "A"), hs10 = c("1000000000", "2000000000", "1000000000"),
                      w = c(100, 10, 50), tau_collected = c(8, 4, 2), tau_base = c(3, 3, 1), match = "HS10")
  carve <- data.frame(cty_code = c("KH", "KH", "TR"), hs10 = c("1000000000", "2000000000", "2000000000"),
                      imports = c(60, 15, 5), duty = c(6, 0, 1))
  out <- tri_split_rest_of_world(goods, carve)
  expect_equal(sum(out$w), sum(goods$w))
  kh <- out[out$cty_code == "KH" & out$hs10 == "1000000000", ]
  expect_equal(c(kh$w, kh$tau_collected, kh$tau_base), c(60, 8, 10))
  expect_equal(out$w[out$cty_code == "ROW" & out$hs10 == "1000000000"], 40)
  # 20 asked of a line where ROW holds 10: scaled to 7.5 and 2.5, ROW's row drops out
  expect_equal(out$w[out$cty_code == "KH" & out$hs10 == "2000000000"], 7.5)
  expect_equal(out$w[out$cty_code == "TR" & out$hs10 == "2000000000"], 2.5)
  expect_false(any(out$cty_code == "ROW" & out$hs10 == "2000000000"))
})

test_that("tri_partner_groups follows sp_coercive_pairs for the reporter, with blocs and the residual named", {
  blocs <- c(USA = "U.S.-leaning", JPN = "U.S.-leaning", CHN = "China-leaning", IDN = "China-leaning", IND = "Nonaligned")
  g <- expand.grid(reporter_iso = names(blocs), partner_iso = names(blocs), stringsAsFactors = FALSE)
  g <- g[g$reporter_iso != g$partner_iso, ]
  g$bloc_reporter <- unname(blocs[g$reporter_iso])
  g$bloc_partner <- unname(blocs[g$partner_iso])
  g$geo_alignment <- ifelse(g$bloc_reporter == "Nonaligned" | g$bloc_partner == "Nonaligned", 0.5,
                            ifelse(g$bloc_reporter == g$bloc_partner, 1, 0))
  pairs <- sp_coercive_pairs(g)
  expect_equal(tri_partner_groups(c("JPN", "CHN", "IDN", "IND", "XXX", "EU", "ROW"), pairs, "USA"),
               c("Trusted", "Coercive", "Coercive", "Nonaligned", "Nonaligned", "Trusted", "Unassigned (rest of world)"))
})

test_that("Section 232 tags match each list code on its own length, in the tracker's precedence", {
  lists <- list(autos = c("850760", "87038000"), metals_ia = c("7601", "740811"), metals_ib = "7308900000",
                metals_iii = character(), metals_removed = "76012000", copper = "74081100", timber = "44071100")
  hs10 <- c("8507600030", "7601103000", "7601200060", "7408110000", "7308900000", "4407110000", "8541430010")
  expect_equal(tri_section232_tags(hs10, lists),
               c("232 autos & parts", "232 metals", "none", "232 metals", "232 metals", "232 timber", "none"))
  expect_equal(tri_prefix_match("8541430010", c("8541.43", "")), TRUE)     # punctuation and blanks ignored
  expect_equal(tri_prefix_match("8541430010", "8541430020"), FALSE)
})

test_that("tri_adcvd_rates adds AD and CVD, keeps the highest order on a line and ignores orders not in force", {
  orders <- data.frame(
    family = c("Solar", "Solar", "Solar", "Pipe"), iso3 = c("CHN", "CHN", "VNM", "KOR"),
    in_force = c("TRUE", "TRUE", "TRUE", "FALSE"),
    hts = c("8541.43.0010", "8541.43.0010; 8541.42", "8541430010", "7304291010"),
    ad_rate = c("238.95", "152.84", "271.28", "5.24"), cvd_rate = c("15.24", "38.43", "", "1.33")
  )
  r <- tri_adcvd_rates(c("8541430010", "8541420010", "8541430010", "7304291010", "8541430010"),
                       c("CHN", "CHN", "VNM", "KOR", "MYS"), orders)
  expect_equal(r$adcvd, c(254.19, 191.27, 271.28, 0, 0))
  expect_equal(r$adcvd_family, c("Solar", "Solar", "Solar", "", ""))
  expect_equal(tri_adcvd_rates("8541430010", "CHN", orders[0, ])$adcvd, 0)
})

test_that("tri_adcvd_tau adds the deposit, uses the weight-year rate for absent lines and caps the total", {
  expect_equal(tri_adcvd_tau(c(12.5, NA, 3, 128), c(1, 2, 3, 2.5), c(36, 30, 0, 0), cap = 100), c(48.5, 32, 3, 100))
})

test_that("tri_basket_cells keeps current in-basket codes, copies Assembly to Deployment and rejects a code in two stages", {
  master <- data.frame(
    tech = c("Solar", "Solar", "Solar", "Solar", "Wind"), stage = c("Assembly", "Components", "Assembly", "Assembly", "Assembly"),
    hs6 = c("854143", "854142", "870390", "999999", "850231"), sub_sector = "x",
    excluded = c("FALSE", "FALSE", "FALSE", "TRUE", "FALSE"), legacy_hs = c("FALSE", "FALSE", "TRUE", "FALSE", "FALSE")
  )
  cells <- tri_basket_cells(master, "Solar")
  expect_setequal(paste(cells$stage, cells$hs6), c("Assembly 854143", "Components 854142", "Deployment 854143"))
  bad <- rbind(master, data.frame(tech = "Solar", stage = "Refining", hs6 = "854143", sub_sector = "x", excluded = "FALSE", legacy_hs = "FALSE"))
  expect_error(tri_basket_cells(bad, "Solar"), "two stages")
})

test_that("tri_by_cell reports each group's TRI, coverage under `keep`, and the 232, AD/CVD and cap shares", {
  goods <- data.frame(
    hs6 = c("854143", "854143", "854143", "730890"), iso3 = c("CHN", "VNM", "JPN", "JPN"),
    group = c("Coercive", "Coercive", "Trusted", "Trusted"), w = c(10, 30, 60, 100),
    tau = c(40, 100, 10, 50), s232 = c("none", "none", "none", "232 metals"),
    covered = c(FALSE, TRUE, FALSE, FALSE), keep = c(TRUE, TRUE, FALSE, TRUE)
  )
  cells <- data.frame(tech = "Solar", stage = c("Assembly", "Assembly"), hs6 = c("854143", "730890"))
  out <- tri_by_cell(goods, cells, "tau", "m", keep = "keep", cap = 100)
  all <- out[out$supplier_group == "All suppliers", ]
  expect_equal(all$tri, unname(tri_index(c(10, 30, 100), c(40, 100, 50))["tri"]))
  expect_equal(all$weight_coverage, 140 / 200)
  expect_equal(all$adcvd_weight_share, 30 / 140)
  expect_equal(all$at_cap_weight_share, 30 / 140)
  expect_equal(all$s232_tri_sq_share, 100 * 50^2 / (10 * 40^2 + 30 * 100^2 + 100 * 50^2))
  expect_equal(out$tri[out$supplier_group == "China"], 40)
  expect_equal(out$tri[out$supplier_group == "Trusted"], 50)
  expect_equal(out$w_total[out$supplier_group == "Trusted"], 160)
  expect_false(any(out$supplier_group == "Nonaligned"))
})

test_that("verdicts follow the posture's fence: open gate for trusted partners, low fence for Watch, high fence for Secure", {
  v <- tri_posture_verdict(
    choice = c("secure", "secure", "secure", "preserve", "partner", "watch", "watch", "not_assessed", NA, "accelerate"),
    tri_all = c(3, 20, 20, 20, 16, 4, 12, NA, 3, 30),
    tri_trusted = c(4.5, 2, 2, 7, 2, 1, 6, NA, 1, NA),
    tri_coercive = c(2.9, 30, 10, 35, 28, 2, 40, NA, 5, 40)
  )
  expect_equal(v$verdict, c("No tariff fence", "In line", "Partial fence", "Taxes trusted partners", "In line",
                            "In line", "Fence above recommendation", "Not assessed", "Not assessed", "In line"))
  expect_match(v$verdict_reason[4], "Trusted partners pay 7.0% \\(moderate\\)")
  expect_equal(tri_posture_verdict("preserve", 20, 7, 35, open = 10)$verdict, "In line")
})

test_that("tri_volume_ratios divides the recent months by the same months of the weight year, per group", {
  panel <- data.frame(hs10 = c("8541430010", "8541430010"), iso3 = c("VNM", "KOR"), group = c("Coercive", "Trusted"),
                      `v_2026-08` = c(5, 30), `v_2024-08` = c(100, 20), check.names = FALSE)
  cells <- data.frame(tech = "Solar", stage = "Assembly", hs6 = "854143")
  v <- tri_volume_ratios(panel, cells, "2026-08", "2024-08")
  expect_equal(v$volume_ratio[v$supplier_group == "All suppliers"], 35 / 120)
  expect_equal(v$volume_ratio[v$supplier_group == "Coercive"], 0.05)
})
