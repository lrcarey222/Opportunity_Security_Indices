# Fantasy Draft cards (R/fantasy_draft/fd_cards.R) on a tiny synthetic posture core.

fd_root <- function() normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(fd_root(), "R", "fantasy_draft", "fd_cards.R"), local = FALSE)

fd_fixture_core <- function() {
  # three countries x two techs x two stages, columnar like the posture core
  grid <- expand.grid(c = 0:2, t = 0:1, s = c(1L, 4L))   # Refining, Deployment
  n <- nrow(grid)
  p <- rep("C", n); m <- rep("preserve", n); dm <- rep(NA, n)
  dep <- grid$s == 4L
  p[dep] <- "D"; m[dep] <- NA; dm[dep] <- "watch"
  # Batteries Refining: AAA Secure, BBB Partner, CCC Watch (a Compete mode is never watch: use Deploy-style low via D)
  i <- which(grid$t == 0 & grid$s == 1L)
  p[i] <- c("S", "C", "D"); m[i] <- c(NA, "partner", NA); dm[i] <- c(NA, NA, "watch")
  # Solar Refining: not assessed anywhere
  j <- which(grid$t == 1 & grid$s == 1L)
  p[j] <- "N"; m[j] <- NA
  list(
    countries = list(iso3 = c("AAA", "BBB", "CCC"), name = c("A", "B", "C")),
    meta = list(techs = c("Batteries", "Solar"), stages = FD_STAGES),
    cells = list(c = grid$c, t = grid$t, s = grid$s, p = p, m = m, dm = dm,
                 need = rep(c(0.2, 0.5, 0.8), length.out = n), prize = rep(0.6, n), cp = rep(0.4, n))
  )
}

test_that("fences follow the Country page mapping", {
  ch <- fd_choice(c("S", "C", "C", "C", "D", "D", "N"), c(NA, "preserve", "catch_up", "partner", NA, NA, NA),
                  c(NA, NA, NA, NA, "accelerate", "watch", NA))
  expect_equal(ch, c("secure", "preserve", "catch_up", "partner", "accelerate", "watch", NA))
  expect_equal(fd_fence(ch), c("high", "medium", "medium", "medium", "medium", "low", NA))
})

test_that("cards drop unassessed and all-Low cells and keep stable ids", {
  cells <- fd_core_cells(fd_fixture_core())
  allies <- c(AA = "AAA", BB = "BBB", CC = "CCC")
  b <- fd_build_cards(cells, allies, techs = c("Batteries", "Solar"))
  # Batteries Refining kept (High + Medium); Solar Refining not assessed; both Deployment cells all-Watch
  expect_equal(b$cards$n, 102L)
  expect_equal(b$cards$n_high, 1L)
  expect_equal(b$cards$demand, 67L)
  expect_equal(b$cards$need, 50L)
  expect_equal(b$cards$ovr, as.integer(round(mean(c(50, 60, 67)))))
  keep_low <- fd_build_cards(cells, allies, techs = c("Batteries", "Solar"), drop_all_low = FALSE)
  expect_setequal(keep_low$cards$n, c(102L, 105L, 115L))
  expect_true(all(is.na(keep_low$cards$prize[keep_low$cards$stage == "Deployment"])))
  expect_equal(fd_card_id("Solar", "Recovery", c("Batteries", "Solar")), 116L)
  r <- b$ratings[b$ratings$n == 102L, ]
  expect_equal(r$fence[match(c("AA", "BB", "CC"), r$ally)], c("high", "medium", "low"))
})

test_that("bad country ids and missing countries stop", {
  cells <- fd_core_cells(fd_fixture_core())
  expect_error(fd_build_cards(cells, c(`FALSE` = "AAA"), "Batteries"), "two capital letters")
  expect_error(fd_build_cards(cells, c(ZZ = "ZZZ"), "Batteries"), "absent")
})

test_that("the payload is one window assignment that parses back", {
  b <- fd_build_cards(fd_core_cells(fd_fixture_core()), c(AA = "AAA", BB = "BBB", CC = "CCC"), c("Batteries", "Solar"))
  js <- fd_js_payload(b, list(core = "x.json"))
  expect_match(js, "window.FD_OSI = ", fixed = TRUE)
  doc <- jsonlite::fromJSON(sub(";\\s*$", "", sub("^[\\s\\S]*?window\\.FD_OSI = ", "", js, perl = TRUE)), simplifyVector = FALSE)
  expect_length(doc$stages, 6)
  expect_equal(doc$ratings$AA$`102`[[1]], "high")
  expect_length(doc$cards, 1)   # a one-card set is still an array
})
