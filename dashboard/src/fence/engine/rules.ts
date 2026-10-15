// Yards & Fences decision engine: every threshold the engine uses, in one place, with its reason.
//
// These are screening conventions, not standards. The Methodology page renders this object, so a
// change here is a change to what the tool tells people; tests pin the cases that depend on them.

export const RULES = {
  /** Gate 1. Only these consequence levels can make a vulnerability "acute". */
  acuteConsequence: ['high', 'severe'] as const,

  /** Digital path: a control layer is acute only if harm would arrive within this horizon. Remote
   *  commands act at once, so anything short of "years" counts. */
  digitalHorizon: ['days', 'weeks', 'months'] as const,

  /** Physical path, coercion leg: share of world supply held by the largest supplier outside the
   *  trusted coalition. Matisek (2026) screens for "a potentially coercive supplier controlling more
   *  than half of a critical node"; shares between `marginal` and `yes` are answered "yes" only when
   *  a curator or the reader says so, and are flagged as marginal. */
  coercion: {
    yes: 0.5,
    marginal: 0.4,
    reason:
      'Matisek (2026) screening heuristic: a potentially coercive supplier controlling more than half of a critical node.',
  },

  /** Concentration is displayed against the US merger-guideline reference points the Sector module
   *  uses (dict.hhi_refs). It is never decisive on its own: it describes markets, not leverage. */
  concentration: {
    high: 0.18,
    moderate: 0.1,
    reason:
      'US DOJ/FTC Merger Guidelines (2023): HHI above 1,800 is "highly concentrated"; 1,000-1,800 moderately. A competition-policy convention, used here only to describe a market.',
  },

  /** Physical path, timing leg. Months assumed for each answer band (the band's rough midpoint;
   *  open-ended bands use a conservative value). Harm horizon = how long until a cutoff becomes
   *  intolerable; restore time = qualification time if trusted producers could cover the loss,
   *  otherwise the longer of qualification and reconstitution. Leverage exists when restore time
   *  exceeds harm horizon plus stockpile cover. */
  months: {
    timeHorizon: { days: 0.25, weeks: 1, months: 6, years: 24 },
    qualificationTime: { lt3m: 2, m3to12: 8, y1to3: 24, gt3y: 48 },
    reconstitutionTime: { lt6m: 4, m6to12: 9, y1to3: 24, gt3y: 48 },
    stockpile: { none: 0, lt3m: 2, m3to12: 8, gt12m: 15 },
  },
  timingMarginalMonths: 6,
  timingReason:
    'Restore time is compared with the time until a cutoff becomes intolerable plus the stockpile bridge (the paper: "acute harm that markets cannot remedy in time"). Matisek (2026) suggests more than roughly six to twelve months to reconstitute, and roughly a year of assured stocks, as screening reference points.',

  /** Medium yard, present position: the assessed country's share of world supply on the component's
   *  primary measure. Relative standing is the point of a competitiveness test, so a share is used;
   *  it is still only a screen and the reader can override it. */
  position: {
    strong: 0.1,
    moderate: 0.03,
    reason:
      'A 10% share of world supply marks an established position worth defending; 3-10% a foothold that needs complementary strengths to count.',
  },

  /** Gate 4: the domestic price premium protection would impose, in percent. */
  premium: {
    moderate: 10,
    high: 30,
    reason:
      'Bands for the premium the reader enters. Above 30% a moderate industrial-capability case does not by itself justify protection; the result becomes a large yard with a specified capability floor.',
  },

  /** Confidence. */
  confidence: {
    highCompleteness: 0.75,
    lowCompleteness: 0.4,
    reason:
      'High confidence needs a determinate yard, no decisive answer at a threshold edge, no decisive low-confidence assessment, and at least 75% of relevant inputs known. Below 40% known, or with two or more marginal decisive answers, confidence is low.',
  },
} as const;

export const MONTHS = RULES.months;
