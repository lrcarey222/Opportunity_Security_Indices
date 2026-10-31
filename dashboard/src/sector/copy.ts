// The Sector pages' own words (the NEIS copy refresh, 2026-10-30; editorial guide section 3, "Sector pages").
// Built on the shared vocabulary (content/vocabulary.ts): the module's questions, the plain names of the
// measures and the AIB terms come from there; this file only holds what the Sector pages alone say, so the
// landing, the technology chain, the profile, its lenses and its section rail use one set of words.
// Candidates for promotion to the shared vocabulary are marked "promote?". Pure data: no React, no IO.

import { MODULE_QUESTION, TERMS } from '../content/vocabulary.ts';

const cap = (s: string) => s.charAt(0).toUpperCase() + s.slice(1);

/** The Sector landing (/sector). The title is the Home card's question, so the click lands on the same words. */
export const SECTOR_LANDING = {
  eyebrow: 'SECTORS',
  title: MODULE_QUESTION.sector,
  lede: 'Follow a technology from extraction through deployment and recovery. See who has capability, where supply is concentrated, which stages matter strategically, and where countries should build, partner or stay open.',
  /** what the grid and the profiles show (measured values), before the one recommendation */
  measured: 'Eleven technologies at six stages make 66 sectors. The sector grid and the profiles show measured values, in each source’s own units and year, with no indices or weights.',
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
  exposure: 'Supply exposed',
  dividend: 'Extra recovered by coordinating',
  residual: 'Remaining gap',
  lost: 'Supply lost',
  compact: 'Sectoral Compact',
} as const;

/**
 * vocabulary CONTROL_LAYER_NOTE said of one economy among many (the Sector card shows every economy of a step;
 * the shared note says "this country"). promote?
 */
export const CONTROL_LAYER_ECONOMY_NOTE = 'A control layer is in the Small yard only where the data show coercive foreign control for that economy.';
