# Pure helpers for the monthly UN Comtrade pull of the coalition's core members
# (scripts/99_pull_comtrade_monthly_core.R). No IO here.

COMTRADE_MONTHLY_N_MONTHS <- 12L

# "YYYYMM" integer arithmetic: add k months (k may be negative).
comtrade_month_add <- function(period, k) {
  period <- as.integer(period)
  y <- period %/% 100L
  m <- period %% 100L
  idx <- y * 12L + (m - 1L) + as.integer(k)
  as.integer((idx %/% 12L) * 100L + (idx %% 12L) + 1L)
}

# The n calendar months ending at `last`, oldest first.
comtrade_month_window <- function(last, n = COMTRADE_MONTHLY_N_MONTHS) {
  comtrade_month_add(last, seq.int(-(n - 1L), 0L))
}

# Each reporter's window: the n calendar months ending at its latest month in the monthly
# data-availability listing (getDA, freq M). `da` needs reporter_iso and period (YYYYMM).
# Reporters absent from `da` come back with NA window and reason "no monthly data", so the caller
# can say so instead of dropping them silently. Months inside the window that getDA does not
# list are counted in n_missing (Comtrade then simply returns no rows for them).
comtrade_monthly_windows <- function(da, reporters, n = COMTRADE_MONTHLY_N_MONTHS) {
  da <- da[!is.na(da$period) & da$reporter_iso %in% reporters, , drop = FALSE]
  da$period <- as.integer(da$period)
  out <- lapply(reporters, function(r) {
    p <- sort(unique(da$period[da$reporter_iso == r]))
    if (length(p) == 0) {
      return(data.frame(reporter_iso = r, first_month = NA_integer_, last_month = NA_integer_,
                        n_missing = NA_integer_, reason = "no monthly data", stringsAsFactors = FALSE))
    }
    w <- comtrade_month_window(max(p), n)
    data.frame(reporter_iso = r, first_month = w[1], last_month = w[n],
               n_missing = sum(!w %in% p), reason = "", stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# Codes of the monthly basket: every non-legacy HS6 code of the master(s) (legacy_hs rows are
# HS1992/HS2017 bridges, which no core member reports in its recent months), plus any extra code
# lists. `masters` is a list of data frames with an hs6 (or HS6) column.
comtrade_monthly_basket <- function(masters) {
  codes <- character()
  for (m in masters) {
    if (is.null(m) || nrow(m) == 0) next
    col <- intersect(c("hs6", "HS6"), names(m))[1]
    if (is.na(col)) stop("Monthly basket input has no hs6 column.")
    keep <- if ("legacy_hs" %in% names(m)) !(toupper(as.character(m$legacy_hs)) %in% c("TRUE", "T", "1")) else TRUE
    x <- gsub("\\D", "", as.character(m[[col]][keep]))
    x <- ifelse(nzchar(x), paste0(strrep("0", pmax(0L, 6L - nchar(x))), x), NA_character_)
    codes <- c(codes, x)
  }
  sort(unique(codes[!is.na(codes) & nchar(codes) == 6]))
}

# One call per (window, flow, code group): reporters sharing a window go together, at most
# `max_reporters` per call. Returns chunk_id, reporters (space-separated), first/last month, flow,
# code_group.
comtrade_monthly_plan <- function(windows, flows = c("import", "export"), n_code_groups = 3L,
                                  max_reporters = 15L) {
  w <- windows[!is.na(windows$last_month), , drop = FALSE]
  if (nrow(w) == 0) stop("No core member has monthly data.")
  rows <- list()
  for (last in sort(unique(w$last_month))) {
    reps <- sort(w$reporter_iso[w$last_month == last])
    batches <- split(reps, ceiling(seq_along(reps) / max_reporters))
    for (b in seq_along(batches)) {
      for (fl in flows) {
        for (g in seq_len(n_code_groups)) {
          rows[[length(rows) + 1L]] <- data.frame(
            chunk_id = sprintf("%d_%s_b%02d_g%d", last, fl, b, g),
            reporters = paste(batches[[b]], collapse = " "),
            first_month = comtrade_month_add(last, -(COMTRADE_MONTHLY_N_MONTHS - 1L)),
            last_month = last, flow = fl, code_group = g, stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  do.call(rbind, rows)
}

# Keep each reporter's own window. A call carries one window, but a reporter re-released later
# could in principle return months outside it; drop those so every reporter has exactly its window.
comtrade_monthly_trim <- function(df, windows) {
  if (nrow(df) == 0) return(df)
  per <- as.integer(df$period)
  w <- windows[match(df$reporter_iso, windows$reporter_iso), , drop = FALSE]
  df[!is.na(w$first_month) & per >= w$first_month & per <= w$last_month, , drop = FALSE]
}
