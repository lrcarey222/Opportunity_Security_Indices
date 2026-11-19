# Chart of the Day animated posts: each day's chart as a 1080 x 1350 px (4:5) NEIS card whose chart builds up over
# COD_SOCIAL$anim_seconds and then holds (the trade-fence explainer's LinkedIn-carousel method; pure code in
# R/chart_of_day/cod_social.R, cod_card.R and cod_anim_<chart type>.R).
#
# Reads dashboard/public/data/chart-of-day/<date>.json (written by 01_chart_of_day.R). Writes, per ground,
# <out>/<ground>/<date>.mp4 (H.264, 24 fps), <date>.gif (540 px wide, 12 fps), <date>_still.png (the final frame) and
# stills.pdf (the stills of this run as one document, for a LinkedIn document post, which cannot animate).
#
# Usage:
#   Rscript scripts/chart_of_day/03_animate.R --dates 2026-11-17,2026-11-18 [--ground blue|white|both] [--preview]
#                                             [--out <dir>] [--font-dir <dir>]
# --out defaults to <OPSI_COD_PNG_DIR>/animated when that is set, else outputs/chart_of_day/animated (gitignored).
# --preview writes six stills per chart (t = 0, .15, .35, .6, .85, 1) to <out>/<ground>/preview and no video.
# Fonts: Roboto from --font-dir (or OPSI_FONT_DIR, else <raw_data_dir>/fonts/roboto), downloaded from Google Fonts'
# GitHub when missing; Arial otherwise. Needs ragg, av (bundles FFmpeg), magick, systemfonts, jsonlite.

fa <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
repo_root <- normalizePath(file.path(dirname(sub("^--file=", "", fa[1])), "..", ".."), winslash = "/")
for (f in list.files(file.path(repo_root, "R", "chart_of_day"), pattern = "[.]R$", full.names = TRUE)) source(f)

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default = NULL) { i <- match(paste0("--", name), args); if (is.na(i)) default else args[i + 1] }
flag <- function(name) paste0("--", name) %in% args

cfg <- yaml::read_yaml(Sys.getenv("OPSI_CHART_OF_DAY", file.path(repo_root, "config", "chart_of_day.yml")))
abs_path <- function(p) if (grepl("^([A-Za-z]:)?[/\\\\]", p)) p else file.path(repo_root, p)
data_dir <- abs_path(cfg$output$dir)
dates <- strsplit(opt("dates", ""), ",", fixed = TRUE)[[1]]
if (!length(dates) || !nzchar(dates[1])) stop("pass --dates YYYY-MM-DD[,YYYY-MM-DD...]")
grounds <- switch(opt("ground", "both"), both = c("blue", "white"), opt("ground", "both"))
preview <- flag("preview")
png_dir <- Sys.getenv("OPSI_COD_PNG_DIR", "")
out_dir <- opt("out", if (nzchar(png_dir)) file.path(png_dir, "animated") else file.path(repo_root, "outputs", "chart_of_day", "animated"))
spec <- COD_SOCIAL

# --- fonts ------------------------------------------------------------------------------------------------------------
font_dir <- opt("font-dir", Sys.getenv("OPSI_FONT_DIR", file.path(repo_root, "data", "raw", "fonts", "roboto")))
weights <- c("Regular", "Bold", "Italic", "Medium")
roboto <- file.path(font_dir, paste0("Roboto-", weights, ".ttf"))
if (!all(file.exists(roboto))) {
  dir.create(font_dir, recursive = TRUE, showWarnings = FALSE)
  for (k in which(!file.exists(roboto)))
    try(utils::download.file(sprintf("https://raw.githubusercontent.com/googlefonts/roboto/main/src/hinted/Roboto-%s.ttf", weights[k]),
                             roboto[k], mode = "wb", quiet = TRUE), silent = TRUE)
}
fam <- "Arial"; fam_medium <- "Arial"
if (all(file.exists(roboto))) {
  systemfonts::register_font("Roboto", plain = roboto[1], bold = roboto[2], italic = roboto[3], bolditalic = roboto[2])
  systemfonts::register_font("Roboto Medium", plain = roboto[4], bold = roboto[2])
  fam <- "Roboto"; fam_medium <- "Roboto Medium"
} else message("Roboto not available; using Arial")

# --- the charts ---------------------------------------------------------------------------------------------------------
read_spec <- function(d) {
  f <- file.path(data_dir, paste0(d, ".json"))
  if (!file.exists(f)) stop("no chart for ", d, " (", f, ")")
  jsonlite::read_json(f, simplifyVector = TRUE)
}
long_date <- function(d) { x <- as.Date(d); sub(" 0", " ", format(x, "%A %d %B %Y")) }
# the footer: the notes' first two sentences and the source (the card appends "Chart: NEIS Center.")
footer <- function(s) c(cod_first_sentences(s$notes, 2), paste0("Source: ", sub("\\.$", "", s$source), "."))

draw_frame <- function(path, s, fn, t) {
  ragg::agg_png(path, width = spec$width, height = spec$height, units = "in", res = spec$dpi, background = COD_INK[["surface"]])
  on.exit(grDevices::dev.off())
  cd <- cod_social_card(fn(s, t, fam), s$title, s$subtitle, footer(s), ink = COD_INK, family = fam, family_medium = fam_medium,
                        eyebrow = "Chart of the day", label = long_date(s$date), spec = spec)
  grid::grid.draw(cd$grob)
  invisible(cd$body_h)
}

for (g in grounds) {
  list2env(cod_anim_palette(g), globalenv())
  gdir <- file.path(out_dir, g)
  dir.create(gdir, recursive = TRUE, showWarnings = FALSE)
  stills <- character()
  for (d in dates) {
    s <- read_spec(d)
    fname <- paste0("cod_anim_", s$chart$type)
    if (!exists(fname)) { message(d, ": no animation for chart type '", s$chart$type, "' yet; skipped"); next }
    fn <- get(fname)
    if (preview) {
      pdir <- file.path(gdir, "preview"); dir.create(pdir, showWarnings = FALSE)
      for (t in c(0, 0.15, 0.35, 0.6, 0.85, 1)) bh <- draw_frame(file.path(pdir, sprintf("%s_t%03d.png", d, round(100 * t))), s, fn, t)
      message(sprintf("%s %s (%s): preview stills in %s (chart body %.2f in tall)", g, d, s$chart$type, pdir, bh))
      next
    }
    tmp <- file.path(tempdir(), paste0("cod_frames_", d, "_", g)); unlink(tmp, recursive = TRUE); dir.create(tmp)
    n_anim <- round(spec$fps * spec$anim_seconds); n_hold <- round(spec$fps * spec$hold_seconds)
    files <- file.path(tmp, sprintf("f%04d.png", seq_len(n_anim + 1 + n_hold)))
    for (i in 0:n_anim) draw_frame(files[i + 1], s, fn, i / n_anim)
    file.copy(rep(files[n_anim + 1], n_hold), files[(n_anim + 2):length(files)], overwrite = TRUE)
    mp4 <- file.path(gdir, paste0(d, ".mp4"))
    av::av_encode_video(files, output = mp4, framerate = spec$fps, vfilter = "format=yuv420p", verbose = FALSE)
    gif_frames <- magick::image_scale(magick::image_read(files[seq(1, length(files), by = 2)]), "540")
    magick::image_write(magick::image_animate(gif_frames, delay = 8, loop = 0, optimize = TRUE), file.path(gdir, paste0(d, ".gif")))
    still <- file.path(gdir, paste0(d, "_still.png"))
    file.copy(files[n_anim + 1], still, overwrite = TRUE)
    stills <- c(stills, still)
    unlink(tmp, recursive = TRUE)
    message(sprintf("%s %s (%s): %s (%d frames, %.1f s)", g, d, s$chart$type, mp4, length(files), length(files) / spec$fps))
  }
  if (length(stills)) magick::image_write(magick::image_read(stills), path = file.path(gdir, "stills.pdf"), format = "pdf", density = spec$dpi)
}
