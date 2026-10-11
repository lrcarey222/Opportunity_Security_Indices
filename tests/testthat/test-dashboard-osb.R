# The dashboard's web bundle: the OSB1 container (R/dashboard/dash_osb.R), the chunks and manifest
# (R/dashboard/dash_web.R) and the greedy network's tie rule (share_rank_digits). Fixture-based:
# runs on the synthetic bundle, no data needed.

repo_root <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "dashboard", "dash_files.R"))
for (f in dash_engine_files()) source(file.path(repo_root, f))

osb_roundtrip <- function(x, ...) dash_osb_decode(dash_osb_encode(x, "test", ...))

test_that("every node type round-trips identical(), NA patterns included", {
  set.seed(7)
  obj <- list(
    vec = list(
      dbl = c(1.5, NA, -0, 1 / 3, 1e-300, 5e-324, .Machine$double.xmax),
      nan = c(NaN, 1, NA),
      int = c(1L, NA, -2147483647L),
      lgl_na = c(TRUE, NA, FALSE),
      lgl = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE),
      chr = c("x", NA, "é", "x"),
      chr_rep = c("a", "b", "a", "a", "a", NA),
      empty_chr = character(), empty_dbl = numeric(), empty_lgl = logical()
    ),
    mat = matrix(c(0.1, 0.2, NA, 0.4, 0.1, 0.2), 2, 3, dimnames = list(c("r1", "r2"), NULL)),
    arr = array(sample(c(0.25, 0.5, NA), 60, TRUE), c(3, 4, 5)),
    arr_f64 = array(stats::runif(60), c(3, 4, 5)),
    bits = array(stats::runif(60) > 0.5, c(3, 4, 5)),
    chr_mat = matrix(c("u", "v", NA, "u"), 2),
    all_na = matrix(NA_real_, 3, 3),
    all_false = matrix(FALSE, 2, 2),
    all_same = rep(0.94, 5),
    scalar = 0.94, scalar_na = NA_real_, scalar_lgl = TRUE, scalar_int = 5L,
    named = c(x = 0.1, y = NA, z = 3),
    named_chr = c(EUU = "European Union"),
    js = dash_osb_json(list(`Solar||Upstream` = "CHN", `Wind||Midstream` = character(), k = list(1, "a", c(TRUE, NA)))),
    empty = list(),
    dropped = NULL,
    tbl = tibble::tibble(Country = c("A", "B", "A"), v = c(1, NA, 3), f = factor(c("p", "q", "p")),
                         d = as.Date("2026-01-01") + 0:2, i = 1:3, l = c(NA, TRUE, FALSE)),
    df = data.frame(x = 1:4, y = letters[1:4])[2:3, ],
    tbl0 = tibble::tibble(a = numeric(), b = character())
  )
  dec <- osb_roundtrip(obj)
  expect_identical(dec$objects, dash_osb_normalize(obj))
  expect_false("dropped" %in% names(dec$objects))
  expect_identical(dec$objects$tbl$f, c("p", "q", "p"))
  expect_identical(dec$objects$tbl$d, c("2026-01-01", "2026-01-02", "2026-01-03"))
  expect_identical(dec$objects$df$x, 2:3)
  expect_null(dimnames(dec$objects$mat))
  expect_identical(dec$objects$vec$nan, c(NaN, 1, NA))
  expect_identical(dec$objects$js$`Solar||Upstream`, "CHN")
})

test_that("the header names node types, and arrays are column-major and 8-byte aligned", {
  a <- array(as.double(seq_len(24)) + 0.5, c(2, 3, 4))
  bytes <- dash_osb_encode(list(a = a, b = c(TRUE, FALSE, NA), cnt = 1:5), "t")
  expect_identical(rawToChar(bytes[1:4]), "OSB1")
  H <- readBin(bytes[5:8], "integer", size = 4, endian = "little")
  hdr <- jsonlite::fromJSON(rawToChar(bytes[9:(8 + H)]), simplifyVector = FALSE)
  node <- hdr$objects$a
  expect_identical(node$t, "f64")
  expect_identical(as.integer(unlist(node$shape)), c(2L, 3L, 4L))
  expect_identical(hdr$objects$b$t, "bool")
  expect_identical(hdr$objects$cnt$t, "i32")
  start <- 8 + H + dash_osb_pad8(H)
  expect_identical(start %% 8, 0)
  for (nd in hdr$objects) expect_identical(nd$off %% 8L, 0L)
  vals <- readBin(bytes[(start + node$off + 1):(start + node$off + 8 * 24)], "double", n = 24, size = 8, endian = "little")
  # element [i, j, k] (1-based) sits at (i - 1) + 2 * ((j - 1) + 3 * (k - 1))
  expect_identical(vals[(2 - 1) + 2 * ((3 - 1) + 3 * (4 - 1)) + 1], a[2, 3, 4])
  expect_identical(vals[(1 - 1) + 2 * ((2 - 1) + 3 * (3 - 1)) + 1], a[1, 2, 3])
})

test_that("dict16 is lossless and chosen for doubles with few distinct values", {
  x <- array(sample(c(0.1, 1 / 3, 2 / 3, NA), 1000, TRUE), c(10, 10, 10))
  bytes <- dash_osb_encode(list(x = x), "t")
  H <- readBin(bytes[5:8], "integer", size = 4, endian = "little")
  hdr <- jsonlite::fromJSON(rawToChar(bytes[9:(8 + H)]), simplifyVector = FALSE)
  expect_identical(hdr$objects$x$t, "dict16")
  expect_identical(dash_osb_decode(bytes)$objects$x, x)
})

test_that("JSON values keep 17 significant digits and the dashboard's conventions", {
  set.seed(3)
  x <- c(stats::runif(500), 0.94, 0.1, 4^((0:100 - 50) / 50))
  expect_identical(as.numeric(jsonlite::fromJSON(dash_json_text(x))), x)
  v <- jsonlite::fromJSON(dash_json_text(dash_json_value(list(a = "one", b = 1, c = c(p = 1, q = NA), d = character(), e = NA))),
                          simplifyVector = FALSE)
  expect_identical(v$a, list("one"))         # character vectors stay arrays
  expect_identical(v$b, 1L)                  # length-1 numbers are scalars
  expect_identical(names(v$c), c("p", "q"))  # named vectors are objects
  expect_null(v$c$q)                         # NA is null
  expect_identical(v$d, list())
  expect_error(dash_json_value(c(1, Inf)), "Inf")
})

test_that("the synthetic bundle's chunks round-trip and carry the contract paths", {
  b <- dash_fixture_bundle(1)
  p <- b$manifest$baseline
  res <- dash_run(b, p)
  inp <- dash_network_inputs(b, p, res$partner$cells, policy = res$policy)
  gb <- dash_network_solve(dash_network_score(inp, p), p, bundle = b, share_rank_digits = 9)
  gb$share_rank_digits <- 9L
  b$network$greedy_baseline <- gb
  fixed <- dash_network_fixed_tables(b)
  b$network[c("member_weights", "node_size", "caps")] <- fixed[c("member_weights", "node_size", "caps")]
  expect_identical(fixed$caps$portfolio_cap, gb$portfolio$caps$portfolio_cap)
  k <- paste(fixed$member_weights$tech, fixed$member_weights$supply_chain) %in% paste(inp$stages$tech, inp$stages$supply_chain)
  expect_identical(fixed$member_weights[k, ], tibble::as_tibble(inp$member_weights))

  chunks <- dash_web_chunks(b, res)
  expect_identical(names(chunks), DASH_WEB_CHUNKS)
  for (nm in names(chunks)) {
    dec <- dash_osb_decode(dash_osb_encode(chunks[[nm]], nm))
    expect_identical(dec$objects, dash_osb_normalize(chunks[[nm]]), info = nm)
  }
  core <- dash_osb_normalize(chunks$core)
  expect_identical(core$dims$U, b$partner$U)
  expect_identical(dim(core$pillars$es$score), dim(b$pillars$es$score))
  expect_identical(core$baseline$core$si$cells$strategic_index, res$si$cells$strategic_index)
  expect_identical(core$baseline$network_summary$source, "greedy")
  expect_null(core$pillars$es$var)
  part <- dash_osb_normalize(chunks$partner)
  expect_identical(dim(part$partner$fs$imp_trade), c(length(b$partner$U), length(b$partner$U), nrow(b$partner$cells)))
  expect_identical(part$baseline$partner$psi_dyad$psi, res$partner$psi$dyad$psi)
  net <- dash_osb_normalize(chunks$network)
  expect_identical(net$network$greedy_baseline$forbidden, gb$forbidden)
  expect_identical(net$network$greedy_baseline$share_rank_digits, 9L)
  expect_null(net$network$stored)   # flags$network_consistent is FALSE in the fixture

  # A stored (MILP) design solved on the bundle's inputs becomes the baseline network
  b2 <- b
  b2$manifest$flags$network_consistent <- TRUE
  b2$network$stored <- gb[c("specialization", "flows", "diagnostics")]
  b2$network$stored$diagnostics$method <- "milp"
  ch2 <- dash_web_chunks(b2, res)
  expect_identical(dash_osb_normalize(ch2$core)$baseline$network_summary$source, "milp")
  expect_identical(dash_osb_normalize(ch2$network)$network$stored$specialization$production_share, gb$specialization$production_share)

  info <- tibble::tibble(name = names(chunks), file = paste0(names(chunks), ".00000000.osb"), bytes = 1, bytes_gz = 1,
                         md5 = "0", initial = names(chunks) == "core")
  man <- dash_web_manifest(b, info, dash_web_budget(info, 100), "id", as.POSIXct("2026-01-01", tz = "UTC"))
  js <- jsonlite::fromJSON(dash_json_text(man), simplifyVector = FALSE)
  expect_identical(js$schema_version, DASH_WEB_SCHEMA_VERSION)
  expect_length(js$dial_grid$m, 101)
  expect_identical(unlist(js$dial_grid$m), dash_dial_multiplier(0:100))
  expect_identical(js$dial_grid$m[[51]], 1L)
  expect_identical(js$baseline_params$m$H, 1L)
  expect_identical(js$baseline_params$advanced$fossil, list("auto"))
  expect_equal(js$budget$initial_gz, 101)
  sw <- js$dial_grid$stage_weights
  expect_identical(unlist(sw$Upstream)[[91]], dash_stage_weights(b$manifest$baseline_inputs$stage_weights, dash_dial_multiplier(90))[["Upstream"]])
})

test_that("share_rank_digits ranks tied target shares by score, then ISO3, not by RAS noise", {
  spec <- tibble::tibble(
    tech = "Solar", supply_chain = "Upstream",
    iso3c = c("AAA", "BBB", "CCC", "DDD"),
    production_share = c(0.4 - 1e-13, 0.4 + 1e-13, 0.15, 0.05),
    producer_score = c(0.9, 0.5, 0.7, 0.6)
  )
  noisy <- dash_build_topk_tbl(spec, portfolio_top_k = 1)
  expect_identical(noisy$iso3c[noisy$in_top_k], "BBB")              # the pipeline rule: noise decides
  rounded <- dash_build_topk_tbl(spec, portfolio_top_k = 1, share_rank_digits = 9)
  expect_identical(rounded$iso3c[rounded$in_top_k], "AAA")          # equal shares: higher score first
  expect_identical(rounded$rank_in_stage, c(1L, 1L, 2L, 3L))
  expect_identical(rounded$production_share, spec$production_share)  # shares are not changed
  expect_identical(dash_build_topk_tbl(spec, 5), allied_network_build_topk_tbl(spec, 5))
})
