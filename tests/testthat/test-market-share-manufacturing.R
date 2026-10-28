# IEA 2035 clean-tech manufacturing shares (EO and ES Foreign Dependency, Midstream).
#
# The IEA extract lists China, Viet Nam, India, the EU, the US and "Other", in percent of
# world capacity. "Other" has no EI country, so the join to the EI roster dropped it, and the
# shares were then re-normalised over the five named entities: China's 2035 solar share read
# 0.899 against the IEA's 80%. The denominator is now the IEA's world total.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "utils", "scurve.R"))
source(file.path(repo_root, "R", "utils", "country.R"))
source(file.path(repo_root, "R", "utils", "schema.R"))
source(file.path(repo_root, "R", "utils", "supply_chain_stages.R"))
source(file.path(repo_root, "R", "categories", "foreign_dependency", "market_share_manufacturing.R"))
source(file.path(repo_root, "R", "categories", "foreign_dependency", "foreign_dependency.R"))

# The extract as read.csv() returns it (data/raw/iea_cleantech_Midstream.csv, solar and wind).
msm_cleantech <- function() {
  data.frame(
    X = c("China", "Viet Nam", "India", "EU", "US", "Other"),
    Solar...Current = c(79, 5, 3, 0, 0, 12),
    Solar...Pipeline = c(80, 0, 6, 0, 3, 11),
    Wind...Current = c(64, 0, 0, 16, 8, 12),
    Wind...Pipeline = c(64, 0, 0, 15, 8, 13),
    stringsAsFactors = FALSE
  )
}

msm_ei <- function() {
  data.frame(
    Country = c("China", "Viet Nam", "India", "Germany", "France", "United States"),
    EU = c(0, 0, 0, 1, 1, 0),
    stringsAsFactors = FALSE
  )
}

# Solar and wind shares sit at Assembly; Components holds the same figure as a copy.
msm_share <- function(tbl, country, tech, data_type = "raw", stage = "Assembly") {
  hit <- tbl[tbl$Country == country & tbl$tech == tech & tbl$data_type == data_type & tbl$supply_chain == stage, ]
  if (nrow(hit) != 1) stop("expected one row for ", country, "/", tech, "/", data_type)
  hit$value
}

test_that("2035 shares are shares of the IEA world total, 'Other' included", {
  out <- market_share_manufacturing_build_cleantech_midstream(msm_ei(), msm_cleantech())

  expect_equal(msm_share(out, "China", "Solar"), 0.80)
  expect_equal(msm_share(out, "India", "Solar"), 0.06)
  expect_equal(msm_share(out, "United States", "Solar"), 0.03)
  expect_equal(msm_share(out, "China", "Wind"), 0.64)
  # EU members carry the EU's share.
  expect_equal(msm_share(out, "Germany", "Wind"), 0.15)
  expect_equal(msm_share(out, "France", "Wind"), 0.15)
  # The listed economies hold what "Other" does not.
  listed <- out[out$data_type == "raw" & out$tech == "Solar" & out$supply_chain == "Assembly" & out$Country != "France", ]
  expect_equal(sum(listed$value), 1 - 0.11)
})

test_that("each technology's share is copied onto the other manufacturing stage", {
  for (out in list(market_share_manufacturing_build_cleantech_midstream(msm_ei(), msm_cleantech()),
                   foreign_dependency_build_cleantech_midstream(msm_ei(), msm_cleantech()))) {
    for (country in c("China", "India", "Germany")) {
      expect_equal(msm_share(out, country, "Solar", stage = "Components"), msm_share(out, country, "Solar"))
      expect_equal(msm_share(out, country, "Wind", "index", stage = "Components"), msm_share(out, country, "Wind", "index"))
    }
    expect_true(all(grepl("copied from Assembly", out$explanation[out$tech == "Solar" & out$supply_chain == "Components"], fixed = TRUE)))
  }
})

test_that("the index is unchanged: every share of a technology shrinks by one factor", {
  out <- market_share_manufacturing_build_cleantech_midstream(msm_ei(), msm_cleantech())
  solar <- out[out$tech == "Solar" & out$data_type == "index", ]
  solar <- solar[match(c("China", "Viet Nam", "India", "Germany", "United States"), solar$Country), ]

  # Ranked over the EI entities (China, Viet Nam, India, EU, United States), as before.
  expect_equal(solar$value, median_scurve(c(80, 0, 6, 0, 3) / 89))
})

test_that("the Energy Security copy of the builder uses the same denominator", {
  out <- foreign_dependency_build_cleantech_midstream(msm_ei(), msm_cleantech())

  expect_equal(msm_share(out, "China", "Solar"), 0.80)
  expect_equal(msm_share(out, "Germany", "Wind"), 0.15)
  expect_equal(
    msm_share(out, "China", "Solar", data_type = "index"),
    msm_share(market_share_manufacturing_build_cleantech_midstream(msm_ei(), msm_cleantech()), "China", "Solar", "index")
  )
})
