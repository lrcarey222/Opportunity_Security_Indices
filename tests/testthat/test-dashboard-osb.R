# The dashboard's web bundle: the OSB1 container (R/dashboard/dash_osb.R) and the chunks and
# manifest (R/dashboard/dash_web.R). Fixture-based: runs on the synthetic bundle, no data needed.

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

  chunks <- dash_web_chunks(b, res)
  expect_identical(names(chunks), DASH_WEB_CHUNKS)
  expect_identical(DASH_WEB_CHUNKS, c("core", "vars", "partner"))  # the network chunk is retired (2026-10-17)
  expect_null(b$network)
  expect_null(p$network)
  for (nm in names(chunks)) {
    dec <- dash_osb_decode(dash_osb_encode(chunks[[nm]], nm))
    expect_identical(dec$objects, dash_osb_normalize(chunks[[nm]]), info = nm)
  }
  core <- dash_osb_normalize(chunks$core)
  expect_identical(core$dims$U, b$partner$U)
  expect_identical(dim(core$pillars$es$score), dim(b$pillars$es$score))
  expect_identical(core$baseline$core$si$cells$strategic_index, res$si$cells$strategic_index)
  expect_null(core$baseline$network_summary)
  expect_null(core$pillars$es$var)
  part <- dash_osb_normalize(chunks$partner)
  expect_identical(dim(part$partner$fs$imp_trade), c(length(b$partner$U), length(b$partner$U), nrow(b$partner$cells)))
  expect_identical(part$baseline$partner$psi_dyad$psi, res$partner$psi$dyad$psi)

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
  expect_null(js$baseline_params$network)
  expect_null(js$baseline_params$advanced$network)
  expect_null(js$baseline_inputs$lambda)
  expect_null(js$share_rank_digits)
  expect_equal(js$budget$initial_gz, 101)
  sw <- js$dial_grid$stage_weights
  expect_identical(unlist(sw$Upstream)[[91]], dash_stage_weights(b$manifest$baseline_inputs$stage_weights, dash_dial_multiplier(90))[["Upstream"]])
})

test_that("the dial readout has no network-design rows (retired 2026-10-17)", {
  b <- dash_fixture_bundle(1)
  ro <- dash_dial_readout(dash_params(dials = list(hawkishness = 80, populism = 80), bundle = b), b)
  expect_false(any(c("lambda", "edge_fs_share", "edge_po_share", "demand_need_share", "demand_size_share",
                     "node_resilience", "node_eo", "node_policy", "home_bias") %in% ro$parameter))
  expect_false(any(grepl("network|producer score", ro$label)))
  expect_error(dash_params(advanced = list(network = list(lambda = 2)), bundle = b), "Unknown advanced setting")
})
