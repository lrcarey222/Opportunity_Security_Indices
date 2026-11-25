// The Sector pages' own words (the NEIS copy refresh, 2026-10-30; editorial guide section 3, "Sector pages").
// Built on the shared vocabulary (content/vocabulary.ts): the module's questions, the plain names of the
// measures and the AIB terms come from there; this file only holds what the Sector pages alone say, so the
// landing, the technology chain, the profile, its lenses and its section rail use one set of words.
// Candidates for promotion to the shared vocabulary are marked "promote?". Pure data: no React, no IO.

import { AIB_MEASURE_LABEL, CONTROL_LAYER_NOTE_ECONOMY, MODULE_QUESTION, TERMS } from '../content/vocabulary.ts';

const cap = (s: string) => s.charAt(0).toUpperCase() + s.slice(1);

/** The Sector landing (/sector). The title is the Home card's question, so the click lands on the same words. */
export const SECTOR_LANDING = {
  eyebrow: 'SECTORS',
  title: MODULE_QUESTION.sector,
  lede: 'Follow a technology from extraction through deployment and recovery. See who has capability, where supply is concentrated, which stages matter strategically, and where countries should build, partner or stay open.',
  /** what the grid and the profiles show (measured values), before the one recommendation */
  measured: 'Eleven energy technologies at six stages make 66 sectors; semiconductors are shown as well, outside that headline set. The sector grid and the profiles show measured values, in each source’s own units and year, with no indices or weights.',
} as const;

/**
 * The five questions of a technology x stage page (guide: "Suggested section sequence"), each a short title
 * and the question it answers. `choice` titles the strategic-choice section (it was "Strategic posture"); its
 * question adds deployment, which the guide's list leaves out. promote? (the Country and AIB pages may want
 * the same titles for the same ideas)
 */
export const SECTOR_SECTION = {
  capability: {
    title: 'Where the capability sits',
    question: "Who produces, exports or controls this supply-chain step today, and how strong is each country's position?",
  },
  risk: {
    title: 'Where the risk sits',
    question: 'How dependent are countries on concentrated or hard-to-replace supply?',
  },
  choice: {
    title: 'What each country should do',
    question: 'Which countries should secure the step, preserve capability, catch up, partner, deploy faster or keep the market open?',
  },
  policy: {
    title: 'What policy is doing',
    question: 'Is government action concentrated where the strategic case is strongest?',
  },
  allies: {
    title: 'What allies can do',
    question: 'Can trusted suppliers cover the need through specialization, or is new capacity required?',
  },
} as const;

/** The question that opens a technology's chain page (/sector/:tech). */
export const CHAIN_QUESTION = MODULE_QUESTION.sectorLanding;
/** The question that opens a technology x stage profile (/sector/:tech/:stage). */
export const PROFILE_QUESTION = MODULE_QUESTION.sectorProfile;
/** The question that opens one economy in one sector (/sector/:tech/:stage/:economy). */
export const countryInSectorQuestion = (economy: string, sector: string): string => `How strong is ${economy} in ${sector} today, and who does it trade with?`;

/** The concentration page (guide: CONCENTRATION PAGE). */
export const CONCENTRATION_PAGE = {
  title: MODULE_QUESTION.concentration,
  lead: 'High concentration is a warning sign, not a strategy by itself.',
  /** what the page can and cannot tell (it measures concentration; substitutability is the AIB stress tests') */
  limits: 'Whether a concentration is dangerous depends on how hard the supply is to replace, which the Allied Industrial Base stress tests ask.',
} as const;

/** The plain explanation that opens the concentration-index explainer, before the arithmetic (guide: HHI EXPLAINER). promote? */
export const HHI_PLAIN =
  'Concentration measures how much of a market is controlled by a small number of suppliers. A high score matters most when alternatives are scarce, qualification takes time, or disruption would have serious consequences.';

/** The sources page (guide: SOURCE PAGE). */
export const SOURCES_PAGE = {
  title: 'Data behind the sector analysis',
  lede: 'What each measure means, where it comes from and where coverage is strong or weak, with the trade reconciliation and the full data as downloads.',
} as const;

/**
 * The Allied Industrial Base results on a Sector profile, in plain words (TERMS), with the technical name kept
 * for the footnote. Column headers are short; the footnote defines each.
 */
export const AIB_COLUMN = {
  node: cap(TERMS.node),
  cls: cap(TERMS.nodeClass),
  exposure: AIB_MEASURE_LABEL.exposure,
  dividend: AIB_MEASURE_LABEL.dividend,
  residual: AIB_MEASURE_LABEL.residual,
  lost: 'Supply lost',
  compact: 'Sectoral Compact',
} as const;

/** The control-layer note said of one economy among many (the Sector card shows every economy of a step): content/vocabulary.ts. */
export const CONTROL_LAYER_ECONOMY_NOTE = CONTROL_LAYER_NOTE_ECONOMY;

/**
 * "Where are the real bottlenecks?" on a technology's chain page (external review readiness, 2026-11-23):
 * the supply-chain steps the Allied Industrial Base stress test says need action, largest remaining gap
 * first, or, without such a step, the most concentrated measures (concentration only). Words only; the
 * figures come from the shipped AIB nodes and the chain ladder (views/Bottlenecks.tsx).
 */
export const BOTTLENECKS = {
  title: 'Where are the real bottlenecks?',
  /** the fallback list's label: concentration says who holds supply, not whether it could be replaced */
  concentrationOnly: 'Most concentrated measures (concentration only: who holds the most supply, not whether it could be replaced):',
  loading: 'Loading the stress-test results…',
} as const;

/**
 * One plain line on what a concentration index value means (HHI bands; views/HhiExplainer.tsx hhiBand reads
 * the thresholds from dict.hhi_refs). Technical detail (the thresholds and their source) stays in the tooltip.
 */
export const HHI_BAND_WORDS = {
  high: 'Highly concentrated: a few suppliers hold most of this market',
  moderate: 'Moderately concentrated: a handful of suppliers hold a large share',
  low: 'Not concentrated: supply is spread across many suppliers',
  /** a lower bound below the bands: the unattributed part could hide a larger holder */
  lowerBoundLow: 'Possibly more concentrated than shown: part of the market is not attributed to any economy',
} as const;

/** Stages without comparable country-level evidence, said once under the stage grid instead of one empty card each. */
export const NO_EVIDENCE = {
  lead: 'Comparable country-level evidence is not currently available for',
  /** "This page therefore focuses on the 2 stages for which comparable evidence is available." */
  focus: (n: number): string => `This page therefore focuses on ${n === 1 ? 'the one stage' : `the ${n} stages`} for which comparable evidence is available.`,
  none: 'No stage of this technology has comparable country-level evidence yet; each stage profile lists what data exist.',
} as const;
