# Fantasy Draft cards from the six-stage posture bundle (pure; scripts/41_export_fantasy_draft_cards.R does the IO).
#
# The draft's cards are the canonical decision's technology x stage cells. Each card carries the game
# countries' mean supply-chain need (deployment need at Deployment), mean prize (strategic industrial value;
# none at Deployment) and the share of game countries whose fence is High or Medium ("allied demand"); each
# game country gets its fence, choice and own scores on every card. Fences follow the Country page
# (dashboard/src/posture/grammar.ts): High = Secure; Medium = Preserve, Catch Up, Partner, Accelerate;
# Low = Watch; NA = not assessed.

FD_STAGES <- c("Extraction", "Refining", "Components", "Assembly", "Deployment", "Recovery")
FD_MEDIUM_CHOICES <- c("preserve", "catch_up", "partner", "accelerate")

#' The core's columnar cells as one row per country x technology x stage.
fd_core_cells <- function(core) {
  cl <- core$cells
  need <- c("c", "t", "s", "p", "m", "dm", "need", "prize", "cp")
  miss <- setdiff(need, names(cl))
  if (length(miss)) stop("posture core cells lack: ", paste(miss, collapse = ", "))
  num <- function(v) as.numeric(unlist(lapply(v, function(x) if (is.null(x)) NA else x)))
  chr <- function(v) as.character(unlist(lapply(v, function(x) if (is.null(x)) NA else x)))
  # countries are columnar ({iso3: [...], name: [...]}) like the cells; a list of records also works
  ct <- core$countries
  iso <- chr(if (!is.null(ct$iso3)) ct$iso3 else lapply(ct, function(x) x$iso3))
  techs <- chr(core$meta$techs)
  stages <- chr(core$meta$stages)
  out <- data.frame(
    iso3 = iso[num(cl$c) + 1], tech = techs[num(cl$t) + 1], stage = stages[num(cl$s) + 1],
    p = chr(cl$p), m = chr(cl$m), dm = chr(cl$dm),
    need = num(cl$need), prize = num(cl$prize), cp = num(cl$cp), stringsAsFactors = FALSE
  )
  bad <- setdiff(unique(out$stage), FD_STAGES)
  if (length(bad)) stop("posture core stages not in the six-stage taxonomy: ", paste(bad, collapse = ", "))
  out
}

#' The choice word of a cell: secure, a Compete mode, a Deploy mode, or NA (not assessed).
fd_choice <- function(p, m, dm) {
  ifelse(p %in% "S", "secure", ifelse(p %in% "C", m, ifelse(p %in% "D", dm, NA_character_)))
}

#' The fence of a choice: high (Secure), medium (Preserve / Catch Up / Partner / Accelerate), low (Watch).
fd_fence <- function(choice) {
  ifelse(choice %in% "secure", "high", ifelse(choice %in% FD_MEDIUM_CHOICES, "medium",
                                              ifelse(choice %in% "watch", "low", NA_character_)))
}

#' Card id: 100 + 10 x technology position (0-based) + stage position (1-6).
fd_card_id <- function(tech, stage, techs) {
  ti <- match(tech, techs)
  si <- match(stage, FD_STAGES)
  if (anyNA(ti)) stop("technology not in the card order: ", paste(unique(tech[is.na(ti)]), collapse = ", "))
  if (anyNA(si)) stop("stage not in the six stages: ", paste(unique(stage[is.na(si)]), collapse = ", "))
  100L + 10L * (ti - 1L) + si
}

fd_pct <- function(x) ifelse(is.na(x), NA_integer_, as.integer(round(100 * x)))

#' Cards and per-country ratings for the game countries.
#' cells: fd_core_cells(); allies: named character, game id -> ISO3; techs: card order.
fd_build_cards <- function(cells, allies, techs, drop_all_low = TRUE) {
  if (is.null(names(allies)) || any(!grepl("^[A-Z]{2}$", names(allies)))) {
    stop("game country ids must be two capital letters (a bare NO key in YAML parses as FALSE): ", paste(names(allies), collapse = ", "))
  }
  missing_iso <- setdiff(unname(allies), cells$iso3)
  if (length(missing_iso)) stop("game countries absent from the posture core: ", paste(missing_iso, collapse = ", "))
  x <- cells[cells$iso3 %in% allies & cells$tech %in% techs, , drop = FALSE]
  x$ally <- names(allies)[match(x$iso3, allies)]
  x$choice <- fd_choice(x$p, x$m, x$dm)
  x$fence <- fd_fence(x$choice)
  key <- paste(x$tech, x$stage, sep = "|")
  keys <- unique(key[!is.na(x$fence)])
  n_allies <- length(allies)
  cards <- do.call(rbind, lapply(keys, function(k) {
    r <- x[key == k & !is.na(x$fence), , drop = FALSE]
    data.frame(
      tech = r$tech[1], stage = r$stage[1],
      need = fd_pct(mean(r$need, na.rm = TRUE)),
      prize = if (r$stage[1] == "Deployment" || all(is.na(r$prize))) NA_integer_ else fd_pct(mean(r$prize, na.rm = TRUE)),
      demand = fd_pct(sum(r$fence %in% c("high", "medium")) / n_allies),
      n_high = sum(r$fence == "high"), n_medium = sum(r$fence == "medium"), n_low = sum(r$fence == "low"),
      n_assessed = nrow(r), stringsAsFactors = FALSE
    )
  }))
  if (isTRUE(drop_all_low)) cards <- cards[cards$n_high + cards$n_medium > 0, , drop = FALSE]
  cards$n <- fd_card_id(cards$tech, cards$stage, techs)
  # OVR: the mean of the card's available bars (Deployment has no prize)
  bars <- cbind(cards$need, cards$prize, cards$demand)
  cards$ovr <- as.integer(round(rowMeans(bars, na.rm = TRUE)))
  cards <- cards[order(cards$n), c("n", "tech", "stage", "ovr", "need", "prize", "demand", "n_high", "n_medium", "n_low", "n_assessed")]
  rownames(cards) <- NULL
  keep <- paste(cards$tech, cards$stage, sep = "|")
  rt <- x[paste(x$tech, x$stage, sep = "|") %in% keep, , drop = FALSE]
  ratings <- data.frame(
    ally = rt$ally, n = fd_card_id(rt$tech, rt$stage, techs), fence = rt$fence, choice = rt$choice,
    need = fd_pct(rt$need), prize = ifelse(rt$stage == "Deployment", NA_integer_, fd_pct(rt$prize)), cp = fd_pct(rt$cp),
    stringsAsFactors = FALSE
  )
  ratings <- ratings[order(ratings$ally, ratings$n), , drop = FALSE]
  rownames(ratings) <- NULL
  list(cards = cards, ratings = ratings)
}

#' The game's data file: one assignment to window.FD_OSI. ratings are packed per country as
#' {card id: [fence, choice, need, prize, cp]} to keep the file small.
fd_js_payload <- function(built, source_meta = list()) {
  rt <- built$ratings
  per_ally <- lapply(split(rt, rt$ally), function(d) {
    stats::setNames(lapply(seq_len(nrow(d)), function(i) {
      list(d$fence[i], d$choice[i], d$need[i], d$prize[i], d$cp[i])
    }), as.character(d$n))
  })
  doc <- list(
    schema = "osi-fantasy-cards", version = 1L,
    source = source_meta,
    stages = I(FD_STAGES),
    cards = built$cards,
    ratings = per_ally
  )
  json <- jsonlite::toJSON(doc, auto_unbox = TRUE, na = "null", null = "null", dataframe = "rows", digits = NA)
  paste0(
    "/* GENERATED by scripts/41_export_fantasy_draft_cards.R from the six-stage posture bundle - do not edit.\n",
    "   Cards: technology x stage cells; ratings[ally][card] = [fence, choice, need, prize, position]. */\n",
    "window.FD_OSI = ", json, ";\n"
  )
}
