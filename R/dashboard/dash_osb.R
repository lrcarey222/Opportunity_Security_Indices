# OSB1: the binary container of the Allied Industrial Base Explorer's data chunks and test fixtures
# (pure; raw vectors in, raw vectors out, no IO). scripts/85 and scripts/86 gzip the bytes
# (gzfile(compression = 9)); the browser inflates them and reads typed arrays straight out of the
# buffer. Layout of the decompressed bytes:
#
#   bytes 0-3   "OSB1"
#   bytes 4-7   uint32 LE  H = length of the header in bytes
#   bytes 8..   header: UTF-8 JSON (H bytes), zero-padded to a multiple of 8
#   then        data section: little-endian typed arrays, each at an 8-byte-aligned offset `off`
#               relative to the start of the data section
#
# Header: {"format": "osb", "version": 1, "schema_version": 1, "kind": "chunk" | "fixture",
# "name": ..., <meta>, "objects": {"<dotted path>": <node>, ...}}. `objects` is the R list flattened
# depth first; a path joins the list names with "." (so names must not contain "."). Node types
# (`t`), arrays column-major with `shape` (= dim, or the length of a vector; `dim: true` when the R
# object had a dim attribute):
#
#   f64     double                         Float64 at off, n elements; NA = NaN
#   i32     integer                        Int32; NA = -2147483648
#   bool    logical with NA                Uint8 0 / 1, NA = 255
#   bits    logical without NA             packed 8 per byte, first element in the lowest bit
#   dict16  double, few distinct values    Uint16 codes into `dict` (an f64 node); code 65535 = NA
#   fct     character with repeats         `levels` (JSON strings) + `codes` (an i32 or const node,
#                                          0-based, -1 = NA)
#   strv    character                      `v`: JSON array, null = NA
#   const   every element the same         `v` (number, boolean, string or null = NA), `dtype`
#   json    small R value                  `v` (see dash_json_value()) + `r` (R type tree, used only
#                                          by dash_osb_decode() to rebuild the exact R value)
#   table   data frame (rebuilt as tibble) `nrow`, `cols` (ordered: name -> vector node)
#
# Every double is stored losslessly (f64, dict16, const with 17 significant digits); nothing is
# quantised. dash_osb_decode(dash_osb_encode(x)) is identical() to dash_osb_normalize(x): tables
# become plain tibbles, factors / dates become character, dimnames, NULL elements and
# attributes other than dim are dropped.
#
# Leaf rules when flattening: a data frame is a table; a non-empty list (not wrapped with
# dash_osb_json()) is recursed into; an empty list, an atomic vector with names, a numeric or
# logical vector of length 1 without dim and anything wrapped with dash_osb_json() is a json node;
# a character vector is fct / strv (always an array, whatever its length); other atomic vectors
# and arrays are typed nodes.

DASH_OSB_MAGIC <- "OSB1"
DASH_OSB_VERSION <- 1L
DASH_OSB_SCHEMA_VERSION <- 1L
DASH_OSB_NA_CODE16 <- 65535L

# Marks an R value (list or vector) to be stored as one json node instead of being flattened.
dash_osb_json <- function(x) structure(list(value = x), class = "dash_osb_json")

# ---- JSON values ----------------------------------------------------------------------------------

# An R value as the structure jsonlite serialises with the conventions of the dashboard (the same
# in json nodes and manifest.json): a named atomic vector becomes an object of scalars; a
# character vector without names stays an array, even of length 1 or 0; a number or logical of
# length 1 without names becomes a scalar, other lengths arrays; NA becomes null; a named list an
# object, an unnamed list an array. Doubles are written with 17 significant digits (exact).
dash_json_value <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.data.frame(x)) stop("dash_json_value(): data frames go into table nodes.")
  if (is.factor(x)) x <- as.character(x)
  if (inherits(x, c("POSIXct", "POSIXlt", "Date"))) x <- dash_osb_format_time(x)
  if (is.list(x)) {
    x <- unclass(x)
    out <- lapply(x, dash_json_value)
    if (is.null(names(x))) names(out) <- NULL
    return(out)
  }
  if (!is.atomic(x)) stop("dash_json_value(): unsupported type ", typeof(x), ".")
  nm <- names(x)
  x <- as.vector(x)
  if (is.double(x) && any(is.nan(x) | is.infinite(x))) stop("dash_json_value(): Inf / NaN cannot be written as JSON.")
  if (!is.null(nm)) {
    out <- lapply(seq_along(x), function(i) if (is.na(x[[i]])) NA else x[[i]])
    names(out) <- nm
    return(out)
  }
  if (is.character(x)) return(I(x))
  x
}

# Serialise a value prepared by dash_json_value() (or a list of them).
dash_json_text <- function(x, pretty = FALSE) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, digits = I(17), na = "null", null = "null", pretty = pretty))
}

dash_osb_format_time <- function(x) {
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  format(as.POSIXct(x), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

# R type tree of a json leaf ("dbl", "int", "lgl", "chr", with "_nm" when named; lists as
# list(list = <children>, names = TRUE / FALSE)).
dash_osb_rtype <- function(x) {
  if (is.list(x)) {
    return(list(list = unname(lapply(x, dash_osb_rtype)), names = !is.null(names(x))))
  }
  code <- switch(typeof(x), double = "dbl", integer = "int", logical = "lgl", character = "chr",
                 stop("dash_osb_rtype(): unsupported type ", typeof(x), "."))
  if (!is.null(names(x))) paste0(code, "_nm") else code
}

# Rebuild an R value from parsed JSON (jsonlite simplifyVector = FALSE) and its type tree.
dash_osb_from_json <- function(v, r) {
  if (is.list(r)) {
    kids <- r$list
    if (!length(kids)) return(list())
    out <- lapply(seq_along(kids), function(i) dash_osb_from_json(v[[i]], kids[[i]]))
    if (isTRUE(r$names)) names(out) <- names(v)
    return(out)
  }
  named <- grepl("_nm$", r)
  code <- sub("_nm$", "", r)
  vals <- if (is.list(v)) v else list(v)
  vals <- lapply(vals, function(e) if (is.null(e)) NA else e)
  out <- switch(code,
                dbl = as.double(unlist(vals)),
                int = as.integer(unlist(vals)),
                lgl = as.logical(unlist(vals)),
                chr = as.character(unlist(vals)))
  if (is.null(out)) out <- switch(code, dbl = double(), int = integer(), lgl = logical(), chr = character())
  if (named) names(out) <- names(v)
  out
}

# ---- Normalisation, flattening ---------------------------------------------------------------------

dash_osb_norm_column <- function(v, label) {
  if (is.factor(v)) v <- as.character(v)
  if (inherits(v, c("POSIXct", "POSIXlt", "Date"))) v <- dash_osb_format_time(v)
  if (is.list(v)) stop("dash_osb: list column ", label, " is not supported.")
  if (!is.atomic(v) || !typeof(v) %in% c("double", "integer", "logical", "character")) {
    stop("dash_osb: column ", label, " has unsupported type ", typeof(v), ".")
  }
  attributes(v) <- NULL
  if (is.character(v)) v <- enc2utf8(v)
  v
}

dash_osb_norm_table <- function(x, label) {
  cols <- lapply(names(x), function(nm) dash_osb_norm_column(x[[nm]], paste0(label, "$", nm)))
  names(cols) <- enc2utf8(names(x))
  structure(cols, row.names = .set_row_names(nrow(x)), class = c("tbl_df", "tbl", "data.frame"))
}

dash_osb_norm_atomic <- function(x, label) {
  if (is.factor(x)) x <- as.character(x)
  if (inherits(x, c("POSIXct", "POSIXlt", "Date"))) x <- dash_osb_format_time(x)
  if (!typeof(x) %in% c("double", "integer", "logical", "character")) {
    stop("dash_osb: ", label, " has unsupported type ", typeof(x), ".")
  }
  d <- dim(x)
  nm <- if (is.null(d)) names(x) else NULL
  attributes(x) <- NULL
  if (!is.null(d)) dim(x) <- as.integer(d)
  if (!is.null(nm)) names(x) <- enc2utf8(nm)
  if (is.character(x)) x <- enc2utf8(x)
  x
}

dash_osb_norm_json <- function(x, label) {
  if (is.null(x)) return(NULL)
  if (is.data.frame(x)) stop("dash_osb: a data frame inside the json leaf ", label, ".")
  if (is.list(x)) {
    nm <- names(x)
    x <- unclass(x)
    keep <- !vapply(x, is.null, logical(1))
    out <- lapply(seq_along(x)[keep], function(i) dash_osb_norm_json(x[[i]], paste0(label, "[", i, "]")))
    if (!is.null(nm)) names(out) <- enc2utf8(nm[keep])
    if (!length(out)) out <- list()
    return(out)
  }
  x <- dash_osb_norm_atomic(x, label)
  if (!is.null(dim(x))) stop("dash_osb: a matrix inside the json leaf ", label, ".")
  if (is.double(x) && any(is.nan(x) | is.infinite(x))) stop("dash_osb: Inf / NaN in the json leaf ", label, ".")
  x
}

# One leaf: list(kind = "table" | "json" | "vec", value = normalised value).
dash_osb_leaf <- function(x, label) {
  if (inherits(x, "dash_osb_json")) return(list(kind = "json", value = dash_osb_norm_json(x$value, label)))
  if (is.data.frame(x)) return(list(kind = "table", value = dash_osb_norm_table(x, label)))
  if (is.list(x)) return(list(kind = "json", value = list()))   # an empty list (non-empty lists are recursed into)
  x <- dash_osb_norm_atomic(x, label)
  if (!is.null(names(x)) || (!is.character(x) && is.null(dim(x)) && length(x) == 1)) {
    return(list(kind = "json", value = dash_osb_norm_json(x, label)))
  }
  list(kind = "vec", value = x)
}

# Flatten a nested list into list(<dotted path> = leaf). NULL elements are dropped.
dash_osb_flatten <- function(x, prefix = NULL) {
  out <- list()
  walk <- function(obj, path) {
    if (is.null(obj)) return(invisible())
    is_branch <- is.list(obj) && !is.data.frame(obj) && !inherits(obj, "dash_osb_json") && length(obj) > 0
    if (!is_branch) {
      if (is.null(path)) stop("dash_osb_flatten(): the top level must be a non-empty list.")
      out[[path]] <<- dash_osb_leaf(obj, path)
      return(invisible())
    }
    nm <- names(obj)
    if (is.null(nm) || any(!nzchar(nm)) || anyDuplicated(nm)) {
      stop("dash_osb_flatten(): every list element needs a unique name (at ", if (is.null(path)) "<top>" else path, ").")
    }
    if (any(grepl(".", nm, fixed = TRUE))) {
      stop("dash_osb_flatten(): list names must not contain '.' (", paste(nm[grepl(".", nm, fixed = TRUE)], collapse = ", "), ").")
    }
    for (k in nm) walk(obj[[k]], if (is.null(path)) k else paste(path, k, sep = "."))
  }
  walk(x, prefix)
  out
}

# Rebuild the nested list from list(<dotted path> = value).
dash_osb_unflatten <- function(flat) {
  set_in <- function(x, path, value) {
    if (length(path) == 1) {
      if (is.null(value)) return(x)
      x[path] <- list(value)
      return(x)
    }
    child <- if (is.null(x[[path[1]]])) list() else x[[path[1]]]
    x[[path[1]]] <- set_in(child, path[-1], value)
    x
  }
  out <- list()
  for (k in names(flat)) out <- set_in(out, strsplit(k, ".", fixed = TRUE)[[1]], flat[[k]])
  out
}

# The value dash_osb_decode() returns for `objects`.
dash_osb_normalize <- function(objects) {
  flat <- dash_osb_flatten(objects)
  dash_osb_unflatten(lapply(flat, function(l) l$value))
}

# ---- Encoding --------------------------------------------------------------------------------------

dash_osb_pad8 <- function(n) (8 - n %% 8) %% 8

# A growing data section (environment): add() appends bytes at the next 8-byte boundary and returns
# their offset.
dash_osb_data_new <- function() {
  env <- new.env(parent = emptyenv())
  env$parts <- list()
  env$off <- 0
  env$add <- function(bytes) {
    off <- env$off
    pad <- dash_osb_pad8(length(bytes))
    env$parts[[length(env$parts) + 1]] <- if (pad) c(bytes, raw(pad)) else bytes
    env$off <- off + length(bytes) + pad
    off
  }
  env
}

dash_osb_shape <- function(x) I(as.integer(if (is.null(dim(x))) length(x) else dim(x)))

# A typed node for an atomic vector or array (no names); bytes go to `data`.
dash_osb_vec_node <- function(x, data) {
  n <- length(x)
  node <- list(t = NA_character_, shape = dash_osb_shape(x))
  if (!is.null(dim(x))) node$dim <- TRUE
  type <- typeof(x)
  const_node <- function(v, dtype) {
    node$t <- "const"
    node$dtype <- dtype
    node$v <- v
    node
  }
  if (type == "character") {
    if (n > 0 && all(is.na(x))) return(const_node(NA, "chr"))
    lev <- unique(x[!is.na(x)])
    if (n > 0 && length(lev) == 1 && !anyNA(x)) return(const_node(lev, "chr"))
    if (n > 0 && length(lev) < n / 2) {
      codes <- match(x, lev) - 1L
      codes[is.na(codes)] <- -1L
      node$t <- "fct"
      node$levels <- I(lev)
      node$codes <- dash_osb_vec_node(codes, data)
      return(node)
    }
    node$t <- "strv"
    node$v <- I(as.vector(x))
    return(node)
  }
  if (n > 0 && type %in% c("integer", "logical")) {
    if (all(is.na(x))) return(const_node(NA, if (type == "integer") "i32" else "bool"))
    if (!anyNA(x) && all(x == x[[1]])) return(const_node(x[[1]], if (type == "integer") "i32" else "bool"))
  }
  if (type == "double") {
    nan <- is.nan(x)
    if (n > 0 && !any(nan)) {
      if (all(is.na(x))) return(const_node(NA, "f64"))
      if (!anyNA(x) && !any(is.infinite(x)) && all(x == x[[1]])) return(const_node(x[[1]], "f64"))
    }
    dict <- if (!any(nan) && !any(is.infinite(x))) sort(unique(x[!is.na(x)])) else NULL
    if (!is.null(dict) && length(dict) <= DASH_OSB_NA_CODE16 - 1L && 2 * n + 8 * length(dict) < 8 * n) {
      codes <- match(x, dict) - 1L
      codes[is.na(x)] <- DASH_OSB_NA_CODE16
      node$t <- "dict16"
      node$n <- n
      node$off <- data$add(writeBin(codes, raw(), size = 2, endian = "little"))
      node$dict <- list(t = "f64", shape = I(length(dict)), n = length(dict),
                        off = data$add(writeBin(dict, raw(), size = 8, endian = "little")))
      return(node)
    }
    node$t <- "f64"
    node$n <- n
    node$off <- data$add(writeBin(as.vector(x), raw(), size = 8, endian = "little"))
    return(node)
  }
  if (type == "integer") {
    node$t <- "i32"
    node$n <- n
    node$off <- data$add(writeBin(as.vector(x), raw(), size = 4, endian = "little"))
    return(node)
  }
  if (type == "logical") {
    node$n <- n
    if (anyNA(x)) {
      v <- as.integer(x)
      v[is.na(v)] <- 255L
      node$t <- "bool"
      node$off <- data$add(as.raw(v))
    } else {
      node$t <- "bits"
      node$off <- data$add(packBits(c(as.vector(x), logical(dash_osb_pad8(n))), "raw"))
    }
    return(node)
  }
  stop("dash_osb: unsupported type ", type, ".")
}

dash_osb_node <- function(leaf, data) {
  switch(
    leaf$kind,
    json = list(t = "json", v = dash_json_value(leaf$value), r = dash_osb_rtype(leaf$value)),
    table = {
      x <- leaf$value
      cols <- lapply(names(x), function(nm) dash_osb_vec_node(x[[nm]], data))
      names(cols) <- names(x)
      list(t = "table", nrow = nrow(x), cols = if (length(cols)) cols else structure(list(), names = character()))
    },
    vec = dash_osb_vec_node(leaf$value, data)
  )
}

# Encode a nested list of objects into OSB1 bytes (uncompressed). `meta` (a named list of scalars)
# is added to the header (e.g. bundle_id, scenario).
dash_osb_encode <- function(objects, name, kind = c("chunk", "fixture"), meta = list()) {
  kind <- match.arg(kind)
  flat <- dash_osb_flatten(objects)
  data <- dash_osb_data_new()
  nodes <- lapply(flat, dash_osb_node, data = data)
  header <- c(
    list(format = "osb", version = DASH_OSB_VERSION, schema_version = DASH_OSB_SCHEMA_VERSION, kind = kind, name = name),
    meta,
    list(objects = nodes)
  )
  hdr <- charToRaw(enc2utf8(dash_json_text(header)))
  H <- length(hdr)
  c(charToRaw(DASH_OSB_MAGIC), writeBin(H, raw(), size = 4, endian = "little"), hdr, raw(dash_osb_pad8(H)),
    if (length(data$parts)) do.call(c, data$parts) else raw())
}

# ---- Decoding --------------------------------------------------------------------------------------

dash_osb_read_vec <- function(node, bytes, data_start) {
  shape <- as.integer(unlist(node$shape))
  n <- if (length(shape)) prod(shape) else 0L
  at <- function(off, nbytes) {
    if (!nbytes) return(raw())
    i <- data_start + off
    bytes[(i + 1):(i + nbytes)]
  }
  t <- node$t
  x <- switch(
    t,
    f64 = readBin(at(node$off, 8 * n), "double", n = n, size = 8, endian = "little"),
    i32 = readBin(at(node$off, 4 * n), "integer", n = n, size = 4, endian = "little"),
    bool = {
      v <- as.integer(at(node$off, n))
      out <- v == 1L
      out[v == 255L] <- NA
      out
    },
    bits = as.logical(rawToBits(at(node$off, ceiling(n / 8))))[seq_len(n)],
    dict16 = {
      codes <- readBin(at(node$off, 2 * n), "integer", n = n, size = 2, signed = FALSE, endian = "little")
      dict <- dash_osb_read_vec(node$dict, bytes, data_start)
      out <- rep(NA_real_, n)
      ok <- codes != DASH_OSB_NA_CODE16
      out[ok] <- dict[codes[ok] + 1L]
      out
    },
    fct = {
      lev <- as.character(unlist(lapply(node$levels, function(e) if (is.null(e)) NA_character_ else e)))
      codes <- dash_osb_read_vec(node$codes, bytes, data_start)
      out <- rep(NA_character_, n)
      ok <- codes >= 0L
      out[ok] <- lev[codes[ok] + 1L]
      out
    },
    strv = as.character(unlist(lapply(node$v, function(e) if (is.null(e)) NA_character_ else e))),
    const = {
      v <- if (is.null(node$v)) NA else node$v
      switch(node$dtype,
             f64 = rep(as.double(v), n), i32 = rep(as.integer(v), n),
             bool = rep(as.logical(v), n), chr = rep(as.character(v), n))
    },
    stop("dash_osb_decode(): unknown node type ", t, ".")
  )
  if (is.null(x)) x <- switch(t, strv = character(), fct = character(), logical())
  if (isTRUE(node$dim)) dim(x) <- shape
  x
}

# Decode OSB1 bytes (uncompressed) into list(header = <header without objects>, objects = <nested
# list>).
dash_osb_decode <- function(bytes) {
  if (length(bytes) < 8 || !identical(rawToChar(bytes[1:4]), DASH_OSB_MAGIC)) stop("dash_osb_decode(): not an OSB1 container.")
  H <- readBin(bytes[5:8], "integer", size = 4, endian = "little")
  txt <- rawToChar(bytes[9:(8 + H)])
  Encoding(txt) <- "UTF-8"
  header <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  if (!identical(header$format, "osb") || !identical(as.integer(header$version), DASH_OSB_VERSION)) {
    stop("dash_osb_decode(): unsupported format / version.")
  }
  data_start <- 8 + H + dash_osb_pad8(H)
  flat <- lapply(header$objects, function(node) {
    switch(
      node$t,
      json = dash_osb_from_json(node$v, node$r),
      table = {
        cols <- lapply(node$cols, dash_osb_read_vec, bytes = bytes, data_start = data_start)
        names(cols) <- names(node$cols)
        structure(if (length(cols)) cols else structure(list(), names = character()),
                  row.names = .set_row_names(as.integer(node$nrow)), class = c("tbl_df", "tbl", "data.frame"))
      },
      dash_osb_read_vec(node, bytes, data_start)
    )
  })
  header$objects <- NULL
  list(header = header, objects = dash_osb_unflatten(flat))
}
