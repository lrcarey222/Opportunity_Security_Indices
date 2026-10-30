# CHOICE: the canonical decision of the strategic position framework (docs/strategic_position.md,
# "Choice"; spec Parts VIII-IX; design section 2). Need x Prize triages the problem; Competitive Position
# decides the industrial response; Deployment & Use is decided on its own Need.
#
#   1. Secure override (posture_engine.R: Test 1 AND one of the three routes) -> secure, whatever else.
#   2. Deployment & Use: deployment_need NA -> not_assessed; >= deployment_threshold -> deploy /
#      accelerate; else deploy / watch. Never Partner, Preserve or Catch Up: those are supply-chain answers.
#   3. Producing stages (Extraction, Refining, Components, Assembly, Recovery):
#      a. supply_chain_need or the prize NA -> not_assessed (never Watch, whatever the large-yard case);
#      b. the large-yard case (strategic_value.R sp_large_yard_case(), decision_framework.large_yard) TRUE ->
#         deploy / watch: a mature, passive, commodity node where affordable, diversified access should
#         dominate, whatever the size of the market (watch_reasons large_yard_case; variant deploy_floor where a
#         moderate learning case leaves a capability floor). With large_yard.position_first (the default, the
#         paper's gate order: a present position is a Medium-yard rationale) a high Competitive Position is
#         tested first and goes on to the triage;
#      c. else, with
#         high_need  = supply_chain_need >= need_threshold
#         high_prize = the prize >= prize_threshold, and not capped as market-only (siv_cap: a rule at any
#                      threshold, not a measurement). The prize is strategic_industrial_value (strategic_value.R:
#                      market, industrial-commons and future-option value; decision_framework.strategic_value
#                      prize_measure strategic_value), or industrial_opportunity, the market prize alone, under
#                      prize_measure market (the 2026-10-30 rule, kept for the sensitivity)
#         high_cp    = Competitive Position in the top quarter of the node's contenders (cp_preserve_pct)
#                      AND the country is a contender AND the engine's capability lower bound (Test 3 on
#                      its strict reading) is at least moderate. An unmeasured position (all of
#                      Recovery) is not high; it lowers posture_confidence where the rule reads it
#      high prize: high_cp -> compete / preserve; else a credible pathway (pathway.R) -> compete /
#                  catch_up; else high need -> compete / partner; else deploy / watch.
#      low prize:  high need: high_cp -> compete / preserve with consistency_flag
#                  review_strong_position_low_prize (spec VIII: do not mechanically Partner); else
#                  compete / partner. Low need -> deploy / watch.
#
# Observed policy is never an input. Thresholds are config (decision_framework). Every input within
# borderline_band of its threshold (and a large-yard case that holds on exactly min_signals, or fails for want
# of one signal) is listed in decision_borderline; flipping it gives the other choices
# the evidence allows (choice_possible, posture_possible) and the inputs that would change the answer
# (pivotal_inputs). decision_trace is a compact JSON array of the steps
# [{step, test, value, threshold, result}]; the engine's full trace stays in engine_trace.
#
# Pure functions: no IO. Needs posture_engine.R (sp_delivery_mode, sp_json_str) and sp_helpers.R; strategic_value.R
# for sp_choice_inputs() (sp_siv_cfg, sp_ly_cfg).

SP_CHOICES <- c("secure", "preserve", "catch_up", "partner", "accelerate", "watch", "not_assessed")
SP_CHOICE_POSTURES <- c("secure", "compete", "deploy", "not_assessed")
SP_DEPLOY_MODES <- c("accelerate", "watch")
SP_CHOICE_COMPETE_MODES <- c("preserve", "catch_up", "partner")
# review_strong_position_low_prize (D2): high need, low prize, strong position -> Preserve, for review.
# review_top_quarter_below_floor: Catch Up or Partner although Competitive Position is in the top quarter
# of the node's contenders, because the capability lower bound (a world-share position, or learning on a
# domestic base) is not met: a leading score in a fragmented node, for review rather than silent override.
SP_CHOICE_FLAGS <- c("review_strong_position_low_prize", "review_top_quarter_below_floor")
SP_CHOICE_VERB_POSTURE <- c(secure = "secure", preserve = "compete", catch_up = "compete", partner = "compete",
                            accelerate = "deploy", watch = "deploy", not_assessed = "not_assessed")

# The thresholds with their defaults (large_min: the large-yard min_signals; large_position_first: a high
# Competitive Position is tested before the large-yard case, decision_framework.large_yard.position_first).
sp_choice_thresholds <- function(cfg = list()) {
  list(need = cfg$need_threshold %||% 0.67, prize = cfg$prize_threshold %||% 0.6, cp = cfg$cp_preserve_pct %||% 0.75,
       deployment = cfg$deployment_threshold %||% 0.6, band = cfg$borderline_band %||% 0.05,
       floor = unlist(cfg$capability_floor %||% c("moderate", "strong")),
       large_min = as.integer(cfg$large_yard$min_signals %||% 5L), large_position_first = !isFALSE(cfg$large_yard$position_first))
}

# The prize column the rule reads: strategic_industrial_value (prize_measure strategic_value, the default) or
# industrial_opportunity (prize_measure market).
sp_choice_prize_col <- function(cfg = list()) {
  if (identical(sp_siv_cfg(cfg)$prize_measure, "market")) "industrial_opportunity" else "strategic_industrial_value"
}

# The rule's inputs from a cell or layer table: need and the prize at the producing stages, deployment need at
# Deployment & Use, the large-yard case (all FALSE where decision_framework.large_yard.enabled is false; NA at
# Deployment), capped (the prize is the strategic industrial value and siv_cap is market_only: never a high
# prize, at any threshold) and near (the large-yard case fails for want of one signal, large_yard_near).
# prize_col names the prize column read.
sp_choice_inputs <- function(tbl, cfg = list()) {
  n <- nrow(tbl)
  col <- function(nm) if (nm %in% names(tbl)) tbl[[nm]] else rep(NA, n)
  dep <- tbl$supply_chain == "Deployment"
  pcol <- sp_choice_prize_col(cfg)
  on <- sp_ly_cfg(cfg)$enabled
  large <- if (on) as.logical(col("large_yard_case")) else rep(FALSE, n)
  near <- if (on) as.logical(col("large_yard_near")) %in% TRUE else rep(FALSE, n)
  capped <- pcol == "strategic_industrial_value" & col("siv_cap") %in% "market_only"
  list(need = ifelse(dep, NA_real_, as.numeric(col("supply_chain_need"))), prize = ifelse(dep, NA_real_, as.numeric(col(pcol))),
       dn = ifelse(dep, as.numeric(col("deployment_need")), NA_real_), large = ifelse(dep, NA, large), prize_col = pcol,
       capped = !dep & capped, near = !dep & near)
}

# A high prize: at or above the threshold and not capped as market-only.
sp_choice_prize_high <- function(prize, th, capped = FALSE) !is.na(prize) & prize >= th$prize & !(capped %in% TRUE)

# Position among a reference set: (reference values below + half those equal) / n.
sp_choice_ref_pct <- function(v, ref) {
  ref <- ref[!is.na(ref)]
  n <- length(ref)
  if (!n) return(rep(NA_real_, length(v)))
  vapply(v, function(z) if (is.na(z)) NA_real_ else (sum(ref < z) + 0.5 * sum(ref == z)) / n, numeric(1))
}

# The economy's measured share of world supply of a node: home_share_exposure where the table carries it
# (the listed share, or the lower bound, 0 for an absent economy, where a partial listing omits it), else
# home_share. The posture engine's home_share brackets an unlisted economy at the listing's residual (it
# settles Test 3 either way), which reads as a share of about 1% for every unlisted economy (2026-10-29:
# 210 economies at 0.87% in Batteries Refining, 214 at 1.9% in Green Hydrogen Extraction, so every one of
# them counted as a contender). Contender status, the consistency counts and the bundle's share use this.
sp_measured_share <- function(tbl) {
  h <- if ("home_share" %in% names(tbl)) tbl$home_share else rep(NA_real_, nrow(tbl))
  if ("home_share_exposure" %in% names(tbl)) dplyr::coalesce(tbl$home_share_exposure, h) else h
}

# Competitive Position among the node's contenders: the Competitive Position score placed among the
# economies holding at least contenders$min_share of world supply of the node. Where fewer than
# contenders$min_n contenders exist the cross-country percentile stands in: cp_contender_basis
# few_contenders where the node measures supply shares (a high position then still needs the country to be
# a contender: a non-producer cannot preserve a mining node on its ore re-exports), all_countries where no
# supply share is measured at all (Recovery, Deployment). Adds cp_contender_pct, cp_contender_basis,
# cp_n_contenders and cp_is_contender. The share is the measured one (sp_measured_share()): an economy
# a partial listing omits is not a contender on the listing's residual.
sp_cp_contender_pct <- function(tbl, cfg = list()) {
  cc <- cfg$contenders %||% list(min_share = 0.005, min_n = 5)
  min_share <- cc$min_share %||% 0.005
  min_n <- as.integer(cc$min_n %||% 5L)
  score <- tbl$competitive_position_score
  home <- sp_measured_share(tbl)
  pct_x <- tbl$competitive_position_percentile
  is_c <- !is.na(score) & !is.na(home) & home >= min_share
  node <- paste(tbl$tech, tbl$supply_chain, sep = "|")
  out_pct <- rep(NA_real_, nrow(tbl))
  out_basis <- rep(NA_character_, nrow(tbl))
  out_n <- rep(0L, nrow(tbl))
  for (i in split(seq_len(nrow(tbl)), node)) {
    n_c <- sum(is_c[i])
    out_n[i] <- n_c
    if (n_c >= min_n) {
      out_pct[i] <- sp_choice_ref_pct(score[i], score[i][is_c[i]])
      out_basis[i] <- "contenders"
    } else {
      out_pct[i] <- pct_x[i]
      out_basis[i] <- if (any(!is.na(home[i]))) "few_contenders" else "all_countries"
    }
  }
  out_basis[is.na(out_pct)] <- NA_character_
  tbl$cp_contender_pct <- out_pct
  tbl$cp_contender_basis <- out_basis
  tbl$cp_n_contenders <- out_n
  tbl$cp_is_contender <- is_c
  tbl
}

# High Competitive Position: in the top quarter of contenders, a contender itself (wherever the node
# measures supply shares), and the engine's capability lower bound at least moderate.
sp_choice_high_cp <- function(cp_pct, basis, is_contender, cap_lo, cp_t, floor) {
  !is.na(cp_pct) & cp_pct >= cp_t & (basis %in% "all_countries" | is_contender %in% TRUE) & cap_lo %in% floor
}

# The choice rule itself, vectorised (also used by the threshold sensitivity). stage: supply_chain;
# secure: logical (the engine's Secure); need, prize, dn: numerics; cp_high: logical; pathway: yes / no /
# unknown; large: the large-yard case (logical; FALSE / NA do not decide); capped: the prize is capped as
# market-only (never high). Returns the choice verb (SP_CHOICES).
sp_choice_rule <- function(stage, secure, need, prize, cp_high, pathway, dn, th, large = FALSE, capped = FALSE) {
  dep <- stage == "Deployment"
  hn <- !is.na(need) & need >= th$need
  hp <- sp_choice_prize_high(prize, th, capped)
  ch <- cp_high %in% TRUE
  pw <- pathway %in% "yes"
  ly <- !dep & large %in% TRUE & !(isTRUE(th$large_position_first) & ch)
  dplyr::case_when(
    secure %in% TRUE ~ "secure",
    dep & is.na(dn) ~ "not_assessed",
    dep & dn >= th$deployment ~ "accelerate",
    dep ~ "watch",
    is.na(need) | is.na(prize) ~ "not_assessed",
    ly ~ "watch",
    hp & ch ~ "preserve",
    hp & pw ~ "catch_up",
    hp & hn ~ "partner",
    hp ~ "watch",
    hn & ch ~ "preserve",
    hn ~ "partner",
    TRUE ~ "watch"
  )
}

# Apply the canonical choice to the cell (or layer) table (after the engine, supply_chain_need, the prize
# (industrial_opportunity, then sp_strategic_value() and sp_large_yard_case()), pathway and deployment_need).
# cfg: the decision_framework list; post_cfg: the
# strategic_posture list (delivery, concentration_watch). The engine's own posture and variant are kept as
# engine_posture / engine_variant and its full trace as engine_trace.
sp_decision_choice <- function(tbl, cfg = list(), post_cfg = list()) {
  th <- sp_choice_thresholds(cfg)
  band <- th$band
  n <- nrow(tbl)
  col <- function(nm, default = NA) if (nm %in% names(tbl)) tbl[[nm]] else rep(default, n)
  stage <- tbl$supply_chain
  dep <- stage == "Deployment"
  excluded <- !is.na(col("posture_exclusion", NA_character_)) | is.na(col("strategic_posture", NA_character_))
  eng_post <- col("strategic_posture", NA_character_)
  eng_var <- col("posture_variant", NA_character_)
  eng_possible <- col("posture_possible", NA_character_)
  eng_piv <- col("pivotal_inputs", NA_character_)
  secure <- eng_post %in% "secure"
  inp <- sp_choice_inputs(tbl, cfg)
  need <- inp$need
  prize <- inp$prize
  dn <- inp$dn
  large <- inp$large
  pcol <- inp$prize_col
  # a market-only cap is a rule, not a measurement near the threshold: never high, never borderline
  capped <- inp$capped
  ly_n <- suppressWarnings(as.integer(col("large_yard_n", NA_integer_)))
  cp_pct <- col("cp_contender_pct", NA_real_)
  cp_basis <- col("cp_contender_basis", NA_character_)
  is_c <- col("cp_is_contender", NA)
  cap_lo <- col("t3_capability_lo", NA_character_)
  pathway <- col("pathway_credible", NA_character_)
  cp_high <- sp_choice_high_cp(cp_pct, cp_basis, is_c, cap_lo, th$cp, th$floor)
  verb <- sp_choice_rule(stage, secure, need, prize, cp_high, pathway, dn, th, large, capped)
  verb[excluded] <- NA_character_
  assessed <- !is.na(need) & !is.na(prize)
  # decided by the large-yard case (an assessed cell only): prize, position and pathway are not read
  ly_dec <- !excluded & !secure & !dep & assessed & large %in% TRUE & !(th$large_position_first & cp_high %in% TRUE)

  # Borderline inputs and the choices across them.
  near <- function(v, t) !is.na(v) & abs(v - t) < band
  live <- !excluded & !secure
  tri <- live & !dep & !ly_dec
  nb_need <- tri & near(need, th$need) & !is.na(prize)
  nb_prize <- tri & near(prize, th$prize) & !is.na(need) & !capped
  nb_cp <- tri & assessed & near(cp_pct, th$cp) &
    (cp_basis %in% "all_countries" | is_c %in% TRUE) & cap_lo %in% th$floor
  nb_dn <- live & dep & near(dn, th$deployment)
  # a large-yard case that holds on exactly min_signals is narrow: one signal less and it fails; one that fails
  # for want of a single signal (large_yard_near) is narrow the other way
  nb_large <- ly_dec & !is.na(ly_n) & ly_n == th$large_min
  nb_large_near <- tri & assessed & large %in% FALSE & inp$near
  # an undecided case on an assessed cell (a cell without need or prize is not assessed either way)
  ly_unknown <- live & !dep & assessed & is.na(large)
  flip <- function(v, t) ifelse(v >= t, t - 1, t)
  rule <- function(need. = need, prize. = prize, cp_high. = cp_high, pathway. = pathway, dn. = dn, large. = large) {
    sp_choice_rule(stage, secure, need., prize., cp_high., pathway., dn., th, large., capped)
  }
  alt_need <- ifelse(nb_need, rule(need. = flip(need, th$need)), NA_character_)
  alt_prize <- ifelse(nb_prize, rule(prize. = flip(prize, th$prize)), NA_character_)
  alt_cp <- ifelse(nb_cp, rule(cp_high. = !cp_high), NA_character_)
  alt_dn <- ifelse(nb_dn, rule(dn. = flip(dn, th$deployment)), NA_character_)
  pw_unknown <- tri & pathway %in% "unknown"
  alt_pw <- ifelse(pw_unknown, rule(pathway. = "yes"), NA_character_)
  alt_large <- ifelse(nb_large, rule(large. = FALSE), ifelse(ly_unknown | nb_large_near, rule(large. = TRUE), NA_character_))
  ly_pivotal <- ly_unknown & !is.na(alt_large) & alt_large != dplyr::coalesce(verb, "")
  sec_possible <- !excluded & grepl("secure", eng_possible)
  bl <- cbind(nb_need, nb_prize, nb_cp, nb_dn, nb_large | nb_large_near)
  colnames(bl) <- c("supply_chain_need", pcol, "cp_contender_pct", "deployment_need", "large_yard_case")
  tbl$decision_borderline <- apply(bl, 1, function(r) if (any(r)) paste(colnames(bl)[r], collapse = ";") else NA_character_)
  alts <- cbind(alt_need, alt_prize, alt_cp, alt_dn, alt_pw, alt_large)
  colnames(alts) <- c("supply_chain_need", pcol, "cp_contender_pct", "deployment_need", "pathway_credible", "large_yard_case")
  piv <- vapply(seq_len(n), function(i) {
    a <- alts[i, ]
    hit <- names(a)[!is.na(a) & a != verb[i]]
    if (sec_possible[i] && !secure[i] && !is.na(eng_piv[i])) hit <- c(eng_piv[i], hit)
    if (length(hit)) paste(hit, collapse = ";") else NA_character_
  }, character(1))
  possible <- vapply(seq_len(n), function(i) {
    if (is.na(verb[i])) return(NA_character_)
    v <- c(verb[i], stats::na.omit(alts[i, ]), if (sec_possible[i]) "secure")
    paste(SP_CHOICES[SP_CHOICES %in% v], collapse = "|")
  }, character(1))
  posture <- unname(SP_CHOICE_VERB_POSTURE[verb])
  post_possible <- vapply(strsplit(dplyr::coalesce(possible, ""), "|", fixed = TRUE), function(v) {
    if (!length(v)) return(NA_character_)
    p <- unique(unname(SP_CHOICE_VERB_POSTURE[v]))
    paste(SP_CHOICE_POSTURES[SP_CHOICE_POSTURES %in% p], collapse = "|")
  }, character(1))
  post_possible[is.na(verb)] <- NA_character_

  # Consistency: strong position, high need, low prize -> preserve, flagged for review (D2).
  hn <- !is.na(need) & need >= th$need
  hp <- sp_choice_prize_high(prize, th, capped)
  top_q <- !dep & !is.na(cp_pct) & cp_pct >= th$cp & (cp_basis %in% "all_countries" | is_c %in% TRUE)
  flag <- dplyr::case_when(
    verb %in% "preserve" & !hp & hn & cp_high ~ "review_strong_position_low_prize",
    verb %in% c("catch_up", "partner") & top_q & !cp_high ~ "review_top_quarter_below_floor",
    TRUE ~ NA_character_
  )

  # Confidence: one point per issue; not assessed is low; Secure keeps the engine's confidence.
  ev_low <- col("stage_evidence_quality_cp", NA_character_) %in% c("low", "none")
  dn_fam <- suppressWarnings(as.integer(col("dn_families_observed", NA_integer_)))
  # An unmeasured Competitive Position (all of Recovery, where no supply share or CP family is measured)
  # counts as not strong, like an unknown pathway (D3): an issue wherever the rule reads the position
  # (a high prize, or high need with a low prize); low need and a low prize give Watch whatever it is.
  # A large-yard Watch reads none of need, prize, position or pathway beyond their being measured: their issues
  # do not apply to it; a narrow case (exactly min_signals, or one signal short) and an undecided case that would
  # change the choice are issues of their own.
  cp_unmeasured <- !dep & !ly_dec & is.na(cp_pct) & verb %in% c("catch_up", "partner", "watch") & (hp | hn)
  issues <- nb_need + nb_prize + nb_cp + nb_dn + nb_large + nb_large_near + ly_pivotal + (sec_possible & !secure) +
    (!ly_dec & verb %in% c("catch_up", "partner", "watch") & hp & !cp_high & pathway %in% "unknown") + cp_unmeasured +
    (verb %in% c("preserve", "catch_up", "partner") & ev_low) + !is.na(flag) +
    (!dep & !ly_dec & verb %in% SP_CHOICES[2:6] & dplyr::coalesce(col("supply_chain_need_n_families", NA_integer_), 0L) < 2) +
    (dep & verb %in% c("accelerate", "watch") & !is.na(dn_fam) & dn_fam < 2)
  conf <- dplyr::case_when(is.na(verb) ~ NA_character_, verb == "not_assessed" ~ "low", secure ~ col("posture_confidence", NA_character_),
                           issues == 0 ~ "high", issues == 1 ~ "moderate", TRUE ~ "low")

  mode <- ifelse(verb %in% SP_CHOICE_COMPETE_MODES, verb, NA_character_)
  dmode <- ifelse(verb %in% SP_DEPLOY_MODES, verb, NA_character_)
  # Deploy variants: the engine's when it read Deploy; otherwise the concentration watch.
  watch <- post_cfg$concentration_watch %||% list(coercive_share = 0.4, import_hhi = 0.18)
  material <- !is.na(col("imports_usd", NA_real_)) & col("imports_usd", NA_real_) >= (post_cfg$exposure_min_import_usd %||% 1e6)
  diversify <- (!is.na(col("coercive_top_share", NA_real_)) & col("coercive_top_share", NA_real_) >= watch$coercive_share) |
    (!is.na(col("import_hhi", NA_real_)) & col("import_hhi", NA_real_) >= watch$import_hhi & material)
  deploy_variant <- ifelse(grepl("^deploy", eng_var), eng_var, ifelse(diversify & !dep, "deploy_diversify", "deploy"))
  # a large yard with a capability floor (a moderate learning case: the paper's large-floor, checked before its
  # concentration watch)
  ly_floor <- ly_dec & col("large_yard_floor", NA) %in% TRUE
  variant <- dplyr::case_when(
    is.na(verb) | verb == "not_assessed" ~ NA_character_,
    verb == "secure" ~ eng_var,
    posture %in% "compete" ~ paste0("compete_", verb),
    ly_floor ~ "deploy_floor",
    TRUE ~ deploy_variant
  )
  tbl$engine_posture <- eng_post
  tbl$engine_variant <- eng_var
  if ("decision_trace" %in% names(tbl) && !"engine_trace" %in% names(tbl)) tbl$engine_trace <- tbl$decision_trace
  tbl$strategic_choice <- verb
  tbl$strategic_posture <- posture
  tbl$compete_mode <- mode
  tbl$deploy_mode <- dmode
  tbl$posture_variant <- variant
  tbl$compete_strategy <- mode
  tbl$security_rationale <- ifelse(verb %in% "secure", col("security_rationale", NA_character_), NA_character_)
  tbl$consistency_flag <- flag
  tbl$posture_confidence <- conf
  tbl$choice_possible <- possible
  tbl$posture_possible <- post_possible
  tbl$pivotal_inputs <- ifelse(is.na(verb), NA_character_, piv)
  tbl$cp_high <- ifelse(dep | is.na(verb), NA, cp_high)
  tbl$need_high <- ifelse(dep, ifelse(is.na(dn), NA, dn >= th$deployment), ifelse(is.na(need), NA, hn))
  tbl$prize_high <- ifelse(dep | is.na(prize), NA, hp)
  tbl$prize_measure <- ifelse(dep | is.na(verb), NA_character_, if (pcol == "industrial_opportunity") "market" else "strategic_value")
  tbl$choice_prize <- ifelse(dep, NA_real_, prize)
  tbl$decision_step <- dplyr::case_when(
    is.na(verb) ~ NA_character_, secure ~ "secure_override", ly_dec ~ "large_yard", verb == "not_assessed" ~ "not_assessed",
    dep ~ "deployment_need", TRUE ~ "triage")
  tbl$posture_primary_rationale <- unname(c(secure = "security", preserve = "capability", catch_up = "capability", partner = "need",
                                            accelerate = "deployment", watch = "openness", not_assessed = NA_character_)[verb])
  if ("posture_secondary_rationale" %in% names(tbl)) {
    tbl$posture_secondary_rationale <- ifelse(verb %in% "secure", tbl$posture_secondary_rationale, NA_character_)
  }
  dm <- sp_delivery_mode(tbl, ifelse(is.na(posture), "deploy", posture), dplyr::coalesce(variant, "deploy"), mode, post_cfg)
  tbl$delivery_mode <- dplyr::case_when(is.na(verb) | verb == "not_assessed" ~ NA_character_,
                                        verb == "secure" ~ col("delivery_mode", NA_character_), TRUE ~ dm)
  # An attractive but currently unsupported opportunity (user spec 2026-10-30): a high prize at a producing
  # stage without a top-quarter position or a capability anchor (pathway.R catch_up_anchor), decided Partner or
  # Watch. A flag for readers, never Catch Up.
  anchor <- col("catch_up_anchor", NA)
  tbl$greenfield_option <- ifelse(is.na(verb) | dep, NA, !ly_dec & verb %in% c("partner", "watch") & hp & !(cp_high %in% TRUE) & !(anchor %in% TRUE))
  tbl$watch_reasons <- sp_watch_reasons(tbl, verb, need, prize, hp, hn, cp_high, pathway, dn, th, diversify, ly_dec)
  tbl$decision_trace <- sp_choice_trace(tbl, verb, secure, sec_possible, need, prize, cp_pct, cp_high, cap_lo, pathway, dn, th,
                                        contender = cp_basis %in% "all_countries" | is_c %in% TRUE, large = large,
                                        ly_dec = ly_dec, prize_col = pcol, ly_enabled = sp_ly_cfg(cfg)$enabled)
  tbl
}

# Why a Watch is Watch (spec XIV: "Domestic industrial priority: low"): large_yard_case (a mature, passive,
# commodity node: affordable, diversified access dominates, strategic_value.R; the triage reasons low_need,
# low_prize and weak_domestic_case are not given beside it, since the rule did not read them), low_need, low_prize,
# ample_diversified_supply (no concentration watch), weak_domestic_case (a prize without a top-quarter
# position or a credible pathway), option_value (an emerging node: keep R&D options open), and at
# Deployment low_deployment_need.
sp_watch_reasons <- function(tbl, verb, need, prize, hp, hn, cp_high, pathway, dn, th, diversify, ly_dec = rep(FALSE, nrow(tbl))) {
  dep <- tbl$supply_chain == "Deployment"
  mat <- if ("io_maturity_class" %in% names(tbl)) tbl$io_maturity_class else rep(NA_character_, nrow(tbl))
  # a large-yard Watch is decided before the triage: its triage reasons (need, prize, position) were not read
  tri <- !dep & !(ly_dec %in% TRUE)
  r <- cbind(
    large_yard_case = ly_dec %in% TRUE,
    low_need = tri & !is.na(need) & !hn,
    low_prize = tri & !is.na(prize) & !hp,
    ample_diversified_supply = !dep & !diversify,
    weak_domestic_case = tri & hp & !(cp_high %in% TRUE) & !(pathway %in% "yes"),
    option_value = !dep & mat %in% "emerging",
    low_deployment_need = dep & !is.na(dn) & dn < th$deployment
  )
  out <- apply(r, 1, function(z) if (any(z)) paste(colnames(r)[z], collapse = ";") else NA_character_)
  ifelse(verb %in% "watch", out, NA_character_)
}

# The compact decision trace: a JSON array of {step, test, value, threshold, result} (null keys left
# out). Steps: secure -> (Deployment: need) / (producing: large_yard, need, prize, position, pathway) -> choice.
# The large_yard step (when the case is enabled) carries the count of commodity signals that hold against
# min_signals; a large-yard Watch stops there.
# The position result: high; not_top_quarter; a top-quarter score that is not high because the economy
# is not a contender (not_a_contender, checked first) or because the capability floor fails
# (capability_floor_not_met); unknown where Competitive Position is not measured. contender: the
# economy is a contender, or the node measures no supply share (cp_contender_basis all_countries).
sp_choice_trace <- function(tbl, verb, secure, sec_possible, need, prize, cp_pct, cp_high, cap_lo, pathway, dn, th,
                            contender = rep(TRUE, length(verb)), large = rep(FALSE, length(verb)), ly_dec = rep(FALSE, length(verb)),
                            prize_col = "industrial_opportunity", ly_enabled = FALSE) {
  num <- function(v) ifelse(is.na(v), NA_character_, format(round(v, 3), nsmall = 0, trim = TRUE, scientific = FALSE))
  step <- function(st, test, value, thr, result, value_str = FALSE) {
    v <- if (value_str) ifelse(is.na(value), NA_character_, sp_json_str(value)) else value
    paste0("{\"step\":\"", st, "\",\"test\":\"", test, "\"",
           ifelse(is.na(v), "", paste0(",\"value\":", v)),
           ifelse(is.na(thr), "", paste0(",\"threshold\":", thr)),
           ",\"result\":", sp_json_str(result), "}")
  }
  dep <- tbl$supply_chain == "Deployment"
  routes <- if ("security_routes_met" %in% names(tbl)) tbl$security_routes_met else rep(NA_character_, nrow(tbl))
  s1 <- step("secure", "secure_override", routes, NA, ifelse(secure, "yes", ifelse(sec_possible, "unknown", "no")), value_str = TRUE)
  lvl <- function(v, t) ifelse(is.na(v), "not_assessed", ifelse(v >= t, "high", "low"))
  s_dn <- step("need", "deployment_need", num(dn), num(th$deployment), lvl(dn, th$deployment))
  s_need <- step("need", "supply_chain_need", num(need), num(th$need), lvl(need, th$need))
  s_prize <- step("prize", prize_col, num(prize), num(th$prize), lvl(prize, th$prize))
  ly_n <- if ("large_yard_n" %in% names(tbl)) suppressWarnings(as.integer(tbl$large_yard_n)) else rep(NA_integer_, nrow(tbl))
  s_ly <- step("large_yard", "large_yard_case", ifelse(is.na(ly_n), NA_character_, as.character(ly_n)), as.character(th$large_min),
               ifelse(is.na(large), "unknown", ifelse(large, "yes", "no")))
  s_ly <- if (isTRUE(ly_enabled)) paste0(s_ly, ",") else rep("", length(verb))
  pos_res <- ifelse(is.na(cp_pct), "unknown", ifelse(cp_high %in% TRUE, "high",
                    ifelse(cp_pct < th$cp, "not_top_quarter",
                           ifelse(contender %in% TRUE, "capability_floor_not_met", "not_a_contender"))))
  s_cp <- step("position", "cp_contender_pct", num(cp_pct), num(th$cp), pos_res)
  s_pw <- step("pathway", "pathway_credible",
               if ("pathway_basis" %in% names(tbl)) tbl$pathway_basis else rep(NA_character_, nrow(tbl)), NA,
               dplyr::coalesce(pathway, "unknown"), value_str = TRUE)
  s_ch <- step("choice", "choice", NA, NA, dplyr::coalesce(verb, "not_classified"))
  hp <- !is.na(prize) & prize >= th$prize
  show_cp <- !dep & !secure & !ly_dec & !is.na(need) & !is.na(prize)
  show_pw <- show_cp & hp & !(cp_high %in% TRUE)
  parts <- ifelse(secure, paste(s1, s_ch, sep = ","),
           ifelse(dep, paste(s1, s_dn, s_ch, sep = ","),
                  ifelse(ly_dec, paste0(s1, ",", s_ly, s_ch),
                         paste0(s1, ",", s_ly, s_need, ",", s_prize, ifelse(show_cp, paste0(",", s_cp), ""),
                                ifelse(show_pw, paste0(",", s_pw), ""), ",", s_ch))))
  ifelse(is.na(verb), NA_character_, paste0("[", parts, "]"))
}

# One sentence per cell saying why the choice is what it is, from the stored inputs only (the drawer's
# CHOICE line and the decision cards). Secure cells keep the engine's route wording elsewhere. Where the
# position is not high it says why: not measured (all of Recovery), a top-quarter score without a
# contender share, a top-quarter score below the capability floor, or below the top quarter.
sp_choice_reason <- function(tbl, cfg = list()) {
  th <- sp_choice_thresholds(cfg)
  p2 <- function(v) ifelse(is.na(v), "not measured", format(round(v, 2), nsmall = 2))
  col <- function(nm, default = NA) if (nm %in% names(tbl)) tbl[[nm]] else rep(default, nrow(tbl))
  v <- tbl$strategic_choice
  inp <- sp_choice_inputs(tbl, cfg)
  need <- col("supply_chain_need", NA_real_)
  prize <- ifelse(tbl$supply_chain == "Deployment", NA_real_, col(inp$prize_col, NA_real_))
  dn <- col("deployment_need", NA_real_)
  cp <- col("cp_contender_pct", NA_real_)
  pw <- dplyr::coalesce(col("pathway_basis", NA_character_), "")
  hn <- !is.na(need) & need >= th$need
  hp <- !is.na(prize) & prize >= th$prize
  need_w <- paste0(ifelse(hn, "High", "Low"), " supply-chain need (", p2(need), ")")
  # the prize in words: the market prize alone, or the strategic industrial value with its three families
  # (market, industrial commons, future option) and the market-only cap where it applies
  siv <- inp$prize_col == "strategic_industrial_value"
  fams <- paste0("market ", p2(col("siv_market", NA_real_)), ", commons ", p2(col("siv_commons", NA_real_)),
                 ", option ", p2(col("siv_option", NA_real_)))
  capped <- siv & col("siv_cap", NA_character_) %in% "market_only"
  prize_w <- if (siv) {
    ifelse(capped, paste0("a strategic industrial value held below the threshold (", p2(prize), ": the market prize alone; ", fams, ")"),
           paste0(ifelse(hp, "a high", "a low"), " strategic industrial value (", p2(prize), "; ", fams, ")"))
  } else paste0(ifelse(hp, "a high", "a low"), " industrial prize (", p2(prize), ")")
  prize_na_w <- if (siv) "the strategic industrial value" else "the industrial prize"
  # a large-yard Watch: decided by the large-yard case before need and prize are read
  ly_dec <- v %in% "watch" & tbl$supply_chain != "Deployment" & col("decision_step", NA_character_) %in% "large_yard"
  ly_w <- sub("[.]$", "", dplyr::coalesce(col("large_yard_reason", NA_character_), "Large yard"))
  # why the position is not high (the checks of sp_choice_high_cp(), in the trace's order)
  contender <- col("cp_contender_basis", NA_character_) %in% "all_countries" | col("cp_is_contender", NA) %in% TRUE
  floor_met <- col("t3_capability_lo", NA_character_) %in% th$floor
  top_q <- !is.na(cp) & cp >= th$cp
  why_not <- dplyr::case_when(
    is.na(cp) ~ "Competitive Position not measured",
    top_q & !contender ~ paste0("a top-quarter score (", p2(cp), ") without a contender share"),
    top_q & !floor_met ~ paste0("a top-quarter score (", p2(cp), ") below the capability floor"),
    TRUE ~ NA_character_
  )
  pos_cu <- dplyr::coalesce(why_not, paste0("a position below the top quarter (", p2(cp), ")"))
  pos_no <- dplyr::coalesce(why_not, "no top-quarter position")
  # why the pathway is not credible where some evidence fired (pathway.R, legs rule: too_few_legs /
  # too_few_observed; anchor rule: no_anchor / anchor_unobserved / anchor_uncorroborated /
  # corroboration_unobserved)
  nw <- function(k) c("no", "one", "two", "three", "four", "five")[k + 1]
  pw_code_re <- ";?(too_few_legs|too_few_observed|no_anchor|anchor_unobserved|anchor_uncorroborated|corroboration_unobserved)$"
  pw_legs <- sub(pw_code_re, "", pw)
  pw_n <- lengths(strsplit(pw_legs, ";", fixed = TRUE))
  # a too-few basis means the bar is above the legs that fired, whatever cfg says (or omits)
  pw_min <- pmax(cfg$pathway$min_legs %||% 1, pw_n + 1)
  pw_legs_w <- gsub(";", ", ", gsub("_", " ", pw_legs))
  pw_some <- ifelse(nzchar(pw_legs), paste0(" (only ", pw_legs_w, ")"), "")
  pw_no <- dplyr::case_when(
    grepl(";too_few_legs$", pw) ~ paste0("only ", nw(pw_n), " piece", ifelse(pw_n == 1, "", "s"), " of pathway evidence (",
                                         pw_legs_w, ") where ", nw(pw_min), " are required"),
    grepl(";too_few_observed$", pw) ~ paste0("too little pathway evidence observed (", pw_legs_w, " only, where ", nw(pw_min),
                                             " pieces are required)"),
    grepl("(^|;)no_anchor$", pw) ~ paste0("no capability anchor", pw_some),
    grepl("(^|;)anchor_unobserved$", pw) ~ paste0("no capability anchor observed", pw_some),
    grepl("(^|;)anchor_uncorroborated$", pw) ~ paste0("a capability anchor (", pw_legs_w, ") without independent corroboration"),
    grepl("(^|;)corroboration_unobserved$", pw) ~ paste0("a capability anchor (", pw_legs_w, ") with nothing observed to corroborate it"),
    TRUE ~ "no credible pathway"
  )
  # a credible pathway under the anchor rule names its anchor and what corroborates it
  rule <- col("pathway_rule", NA_character_)
  anc <- col("catch_up_anchor_type", NA_character_)
  sup <- col("pathway_support", NA_character_)
  words <- function(v) gsub(";", ", ", gsub("_", " ", v))
  pw_yes <- ifelse(rule %in% "anchor" & !is.na(anc),
                   paste0("a credible pathway (capability anchor: ", words(anc), ifelse(is.na(sup), "", paste0("; corroborated by ", words(sup))), ")"),
                   paste0("a credible pathway (", words(pw), ")"))
  cp_na <- ifelse(is.na(cp), ", with Competitive Position not measured", "")
  dplyr::case_when(
    is.na(v) ~ NA_character_,
    v == "secure" ~ paste0("An acute vulnerability (", gsub("_", " ", dplyr::coalesce(tbl$security_rationale, "security")), "): Secure, whatever the prize."),
    v == "not_assessed" & tbl$supply_chain == "Deployment" ~ "Deployment need is not measured for this technology here: not assessed.",
    v == "not_assessed" & is.na(need) & is.na(prize) ~ paste0("Neither supply-chain need nor ", prize_na_w, " is measured: not assessed."),
    v == "not_assessed" & is.na(need) ~ "Supply-chain need is not measured (too few comparable importers or no trade data): not assessed.",
    v == "not_assessed" ~ paste0("The market prize behind ", prize_na_w, " is not measured for this node: not assessed."),
    ly_dec ~ paste0(ly_w, ". Watch: keep it open."),
    v == "accelerate" ~ paste0("Deployment need ", p2(dn), " is at or above ", p2(th$deployment), ": the energy system needs more of it. This says nothing about where the equipment is made."),
    v == "watch" & tbl$supply_chain == "Deployment" ~ paste0("Deployment need ", p2(dn), " is below ", p2(th$deployment), ": no major acceleration is required."),
    v == "preserve" & !hp ~ paste0(need_w, ", ", prize_w, ", but a top-quarter position (", p2(cp), " among contenders): Preserve, flagged for review rather than handed to partners."),
    v == "preserve" ~ paste0(prize_w, " and a top-quarter position among contenders (", p2(cp), "): Preserve and extend it."),
    v == "catch_up" ~ paste0(prize_w, ", ", pos_cu, " and ", pw_yes, ": Catch Up."),
    v == "partner" & hp ~ paste0(need_w, " and ", prize_w, ", but ", pos_no, " and ", pw_no, ": Partner with trusted suppliers."),
    v == "partner" ~ paste0(need_w, " and ", prize_w, cp_na, ": a security problem without a domestic industrial prize. Partner with trusted suppliers."),
    v == "watch" & hp ~ paste0(need_w, " and ", prize_w, ", but ", pos_no, " and ", pw_no, ": Watch (keep R&D options open)."),
    v == "watch" ~ paste0(need_w, " and ", prize_w, ": Watch. Domestic industrial priority is low."),
    TRUE ~ NA_character_
  )
}

# ---- Diagnostics helpers ------------------------------------------------------------------------------
# The choice at alternative thresholds, from the stored inputs (no engine rerun): for each parameter and
# value, cells per choice and how many change against the configured thresholds. tbl: the final cell
# table. grid: list(need = c(...), prize = c(...), cp = c(...), deployment = c(...)). A prize capped as
# market-only stays low at every prize threshold (the cap is a rule, not a measurement).
sp_choice_threshold_sensitivity <- function(tbl, cfg = list(), grid = list()) {
  th0 <- sp_choice_thresholds(cfg)
  x <- tbl[!is.na(tbl$strategic_choice), , drop = FALSE]
  secure <- x$engine_posture %in% "secure"
  inp <- sp_choice_inputs(x, cfg)
  run <- function(th) {
    ch <- sp_choice_high_cp(x$cp_contender_pct, x$cp_contender_basis, x$cp_is_contender, x$t3_capability_lo, th$cp, th$floor)
    sp_choice_rule(x$supply_chain, secure, inp$need, inp$prize, ch, x$pathway_credible, inp$dn, th, inp$large, inp$capped)
  }
  base <- run(th0)
  rows <- list(data.frame(parameter = "base", value = NA_real_, t(as.matrix(table(factor(base, SP_CHOICES)))), changed_vs_base = 0L,
                          check.names = FALSE))
  for (p in names(grid)) for (v in unlist(grid[[p]])) {
    th <- th0
    th[[p]] <- v
    ch <- run(th)
    rows[[length(rows) + 1]] <- data.frame(parameter = p, value = v, t(as.matrix(table(factor(ch, SP_CHOICES)))),
                                           changed_vs_base = sum(ch != base), check.names = FALSE)
  }
  do.call(rbind, rows)
}

# The choice from the stored inputs (no engine rerun) under another decision_framework list: the engine's Secure
# kept, need, the configured prize, position, pathway, deployment need and the stored large-yard case. NA where
# the stored choice is NA. tbl: the cell (or layer) table after sp_decision_choice().
sp_choice_rerun <- function(tbl, cfg = list()) {
  th <- sp_choice_thresholds(cfg)
  inp <- sp_choice_inputs(tbl, cfg)
  ch <- sp_choice_high_cp(tbl$cp_contender_pct, tbl$cp_contender_basis, tbl$cp_is_contender, tbl$t3_capability_lo, th$cp, th$floor)
  v <- sp_choice_rule(tbl$supply_chain, tbl$engine_posture %in% "secure", inp$need, inp$prize, ch, tbl$pathway_credible, inp$dn, th,
                      inp$large, inp$capped)
  v[is.na(tbl$strategic_choice)] <- NA_character_
  v
}

# What strategic industrial value and the large-yard case change (diagnostics strategic_value_vs_market.csv):
# the choice under four rules from the stored inputs (market: the market prize and no large-yard case, the rule
# until 2026-10-30; siv_only; market_large_yard; configured: the configured rule), counted per choice, with the
# cells each moves against market. Producing stages only.
sp_choice_measure_comparison <- function(tbl, cfg = list()) {
  x <- tbl[!is.na(tbl$strategic_choice) & tbl$supply_chain != "Deployment", , drop = FALSE]
  set <- function(measure, large) {
    c2 <- cfg
    c2$strategic_value <- utils::modifyList(cfg$strategic_value %||% list(), list(prize_measure = measure))
    c2$large_yard <- utils::modifyList(cfg$large_yard %||% list(), list(enabled = large))
    c2
  }
  rules <- list(market = set("market", FALSE), siv_only = set("strategic_value", FALSE), market_large_yard = set("market", TRUE),
                configured = cfg)
  verbs <- lapply(rules, function(r) sp_choice_rerun(x, r))
  counts <- do.call(rbind, lapply(names(verbs), function(r) {
    v <- verbs[[r]]
    data.frame(rule = r, t(as.matrix(table(factor(v, SP_CHOICES)))), changed_vs_market = sum(v != verbs$market, na.rm = TRUE),
               check.names = FALSE)
  }))
  moves <- as.data.frame(table(market = verbs$market, configured = verbs$configured), stringsAsFactors = FALSE)
  moves <- moves[moves$Freq > 0, , drop = FALSE]
  names(moves)[3] <- "n_cells"
  list(counts = counts, transitions = moves[order(-moves$n_cells), , drop = FALSE])
}

# The three position-choice contradictions of spec XXI.3, for a Compete mode column: Partner despite a
# very high Competitive Position, Catch Up despite a leading one, Preserve without capability. Counted on
# the cross-country percentile (the old flags' definition) and on the contender percentile (a contender's
# own, or the cross-country one where the node measures no supply share).
sp_consistency_counts <- function(tbl, mode, label) {
  cp_x <- tbl$competitive_position_percentile
  cp_c <- if ("cp_contender_pct" %in% names(tbl)) tbl$cp_contender_pct else rep(NA_real_, nrow(tbl))
  if ("cp_is_contender" %in% names(tbl) && "cp_contender_basis" %in% names(tbl)) {
    cp_c[!(tbl$cp_is_contender %in% TRUE | tbl$cp_contender_basis %in% "all_countries")] <- NA_real_
  }
  home <- sp_measured_share(tbl)
  data.frame(
    framework = label,
    partner = sum(mode %in% "partner"), catch_up = sum(mode %in% "catch_up"), preserve = sum(mode %in% "preserve"),
    partner_despite_cp_x75 = sum(mode %in% "partner" & !is.na(cp_x) & cp_x >= 0.75),
    partner_despite_cp_contender75 = sum(mode %in% "partner" & !is.na(cp_c) & cp_c >= 0.75),
    catch_up_despite_cp_x90 = sum(mode %in% "catch_up" & !is.na(cp_x) & cp_x >= 0.9),
    catch_up_despite_leading_share10 = sum(mode %in% "catch_up" & !is.na(home) & home >= 0.10),
    catch_up_despite_cp_contender90 = sum(mode %in% "catch_up" & !is.na(cp_c) & cp_c >= 0.9),
    preserve_without_capability = sum(mode %in% "preserve" & ((!is.na(cp_x) & cp_x < 0.25) | (!is.na(home) & home < 0.005))),
    preserve_cp_contender_below50 = sum(mode %in% "preserve" & !is.na(cp_c) & cp_c < 0.5)
  )
}
