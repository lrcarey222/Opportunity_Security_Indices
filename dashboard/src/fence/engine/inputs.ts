// Yards & Fences decision engine: the input catalog (labels, questions, option wording, which gate
// each input belongs to) and value formatting. Shared by the engine's explanations and the UI.

import { DOMAINS, NUMERIC_IDS, UNKNOWN } from './types.ts';
import type { CategoricalInputId, InputId, Inputs, NumericInputId, Provenance } from './types.ts';

export type GateKey = 'gate1' | 'digital' | 'physical' | 'capability' | 'leap' | 'cost';

export interface InputDef {
  id: InputId;
  gate: GateKey;
  /** Short noun phrase for lists and the decision tree. */
  label: string;
  /** The question as the reader sees it. */
  question: string;
  help?: string;
  /** Option labels, in DOMAINS order. */
  options?: Record<string, string>;
  /** Shown in Quick mode. */
  quick?: boolean;
  /** Numeric inputs only. */
  numeric?: { min: number; max: number; step: number; unit: 'share' | 'hhi' | 'trl' | 'percent' };
}

const YPN = { no: 'No', partly: 'Partly', yes: 'Yes' };

export const INPUT_DEFS: Record<InputId, InputDef> = {
  consequence: {
    id: 'consequence',
    gate: 'gate1',
    label: 'Consequence of disruption',
    question:
      'If access disappeared tomorrow, or a foreign actor took control, how badly could it threaten critical infrastructure, defence readiness, life and safety, or a critical economic function?',
    options: { low: 'Low', moderate: 'Moderate', high: 'High', severe: 'Severe' },
    quick: true,
  },
  timeHorizon: {
    id: 'timeHorizon',
    gate: 'gate1',
    label: 'Time until intolerable',
    question: 'How quickly would that consequence become intolerable?',
    options: { days: 'Hours to days', weeks: 'Weeks', months: 'Months', years: 'Years' },
    quick: true,
  },
  digitallyActive: {
    id: 'digitallyActive',
    gate: 'digital',
    label: 'Digitally active',
    question: 'Does this capability contain software, firmware, communications, remote updates or orchestration functions?',
    options: { no: 'No', yes: 'Yes' },
  },
  remoteControl: {
    id: 'remoteControl',
    gate: 'digital',
    label: 'External control possible',
    question:
      'Could an external actor issue commands, materially alter operation, push updates, disable functions or reach sensitive operating data?',
    options: { no: 'No', limited: 'Limited', yes: 'Yes' },
    quick: true,
  },
  separability: {
    id: 'separability',
    gate: 'digital',
    label: 'Control layer separable',
    question:
      'Can the risky control layer be technically isolated, independently secured, certified and sourced from a trusted supplier?',
    help: 'This sets how much of the product the fence must surround, not whether there is a fence.',
    options: {
      separable: 'Yes, it can be isolated',
      partial: 'Partly: tied to some hardware',
      inseparable: 'No, not at present',
    },
  },
  substitutability: {
    id: 'substitutability',
    gate: 'physical',
    label: 'Substitutability',
    question: 'How easily can users switch to a different material, design or technology?',
    options: { easy: 'Easy', moderate: 'Moderate', hard: 'Hard', impossible: 'Impossible today' },
    quick: true,
  },
  qualificationTime: {
    id: 'qualificationTime',
    gate: 'physical',
    label: 'Supplier qualification time',
    question: 'How long would it take to qualify an alternative trusted supplier for critical uses?',
    options: { lt3m: 'Under 3 months', m3to12: '3-12 months', y1to3: '1-3 years', gt3y: 'Over 3 years' },
  },
  reconstitutionTime: {
    id: 'reconstitutionTime',
    gate: 'physical',
    label: 'Reconstitution time',
    question: 'How long would it take to rebuild lost production capacity with trusted producers?',
    options: { lt6m: 'Under 6 months', m6to12: '6-12 months', y1to3: '1-3 years', gt3y: 'Over 3 years' },
    quick: true,
  },
  stockpile: {
    id: 'stockpile',
    gate: 'physical',
    label: 'Inventories and stockpiles',
    question: 'After a cutoff, how long could critical demand continue from inventories and stockpiles?',
    options: { none: 'Essentially none', lt3m: 'Under 3 months', m3to12: '3-12 months', gt12m: 'Over 12 months' },
  },
  surgeCapacity: {
    id: 'surgeCapacity',
    gate: 'physical',
    label: 'Trusted surge capacity',
    question: 'Could trusted producers materially increase output to cover lost supply?',
    options: { covers: 'Yes, most of the loss', partial: 'Some of it', none: 'Little or none' },
  },
  processKnowledge: {
    id: 'processKnowledge',
    gate: 'capability',
    label: 'Process knowledge',
    question: 'Would losing production materially weaken process knowledge?',
    options: YPN,
  },
  workforceSuppliers: {
    id: 'workforceSuppliers',
    gate: 'capability',
    label: 'Skills and supplier base',
    question: 'Are there specialised workforce skills or supplier relationships that would be difficult to recreate?',
    options: YPN,
  },
  adjacentInnovation: {
    id: 'adjacentInnovation',
    gate: 'capability',
    label: 'Adjacent innovation',
    question: 'Does production materially improve the ability to innovate in adjacent or successor technologies?',
    options: YPN,
    quick: true,
  },
  complementaryCapabilities: {
    id: 'complementaryCapabilities',
    gate: 'capability',
    label: 'Complementary capabilities',
    question: 'Does the country possess complementary capabilities that make future competitiveness plausible?',
    options: YPN,
  },
  costPerfAdvantage: {
    id: 'costPerfAdvantage',
    gate: 'leap',
    label: 'Cost or performance advantage',
    question: 'Does the successor technology offer a meaningful cost or performance advantage that customers value?',
    options: YPN,
  },
  incumbentVulnerable: {
    id: 'incumbentVulnerable',
    gate: 'leap',
    label: 'Incumbent displaceable',
    question: 'Is the incumbent technological architecture vulnerable to displacement?',
    options: YPN,
  },
  scalePathway: {
    id: 'scalePathway',
    gate: 'leap',
    label: 'Path to scale',
    question: 'Is there a credible demonstration, first-plant and scale-up pathway?',
    options: YPN,
  },
  beyondNovelty: {
    id: 'beyondNovelty',
    gate: 'leap',
    label: 'Evidence beyond novelty',
    question: 'Is there evidence beyond novelty alone (pilot performance, cost curves, offtake)?',
    options: YPN,
  },
  maturity: {
    id: 'maturity',
    gate: 'cost',
    label: 'Maturity',
    question: 'How mature is the technology?',
    options: { mature: 'Mature and commoditised', scaling: 'Scaling', emerging: 'Emerging' },
  },
  nonAlliedTopShare: {
    id: 'nonAlliedTopShare',
    gate: 'physical',
    label: 'Largest non-coalition supplier',
    question: 'Share of world supply held by the largest supplier outside the trusted coalition',
    numeric: { min: 0, max: 1, step: 0.01, unit: 'share' },
  },
  topSupplierShare: {
    id: 'topSupplierShare',
    gate: 'physical',
    label: 'Largest supplier',
    question: 'Share of world supply held by the largest supplier',
    numeric: { min: 0, max: 1, step: 0.01, unit: 'share' },
  },
  hhi: {
    id: 'hhi',
    gate: 'physical',
    label: 'Supplier concentration (HHI)',
    question: 'Herfindahl-Hirschman index of supplier shares',
    numeric: { min: 0, max: 1, step: 0.005, unit: 'hhi' },
  },
  alliedShare: {
    id: 'alliedShare',
    gate: 'physical',
    label: 'Trusted (coalition) supply',
    question: 'Share of world supply held by coalition members and partners',
    numeric: { min: 0, max: 1, step: 0.01, unit: 'share' },
  },
  homeShare: {
    id: 'homeShare',
    gate: 'capability',
    label: 'Home share of world supply',
    question: "The assessed country's share of world supply",
    numeric: { min: 0, max: 0.6, step: 0.005, unit: 'share' },
  },
  trl: {
    id: 'trl',
    gate: 'cost',
    label: 'Technology readiness (TRL)',
    question: 'Technology readiness level (1-9, global)',
    numeric: { min: 1, max: 9, step: 0.1, unit: 'trl' },
  },
  premium: {
    id: 'premium',
    gate: 'cost',
    label: 'Domestic price premium',
    question: 'What price premium would insisting on domestic production impose on buyers?',
    numeric: { min: 0, max: 75, step: 1, unit: 'percent' },
    quick: true,
  },
};

export const CATEGORICAL_IDS = Object.keys(DOMAINS) as CategoricalInputId[];
export const ALL_INPUT_IDS: InputId[] = [...CATEGORICAL_IDS, ...NUMERIC_IDS];

export function isNumericId(id: InputId): id is NumericInputId {
  return (NUMERIC_IDS as readonly string[]).includes(id);
}

export function optionLabel(id: InputId, value: string | number | undefined | null): string {
  if (value === undefined || value === null || value === UNKNOWN) return 'Unknown';
  const def = INPUT_DEFS[id];
  if (def.numeric) return formatNumber(def.numeric.unit, Number(value));
  return def.options?.[String(value)] ?? String(value);
}

export function formatNumber(unit: 'share' | 'hhi' | 'trl' | 'percent', v: number): string {
  if (!Number.isFinite(v)) return 'Unknown';
  switch (unit) {
    case 'share':
      return `${(v * 100).toFixed(v < 0.1 ? 1 : 0)}%`;
    case 'hhi':
      return `HHI ${v.toFixed(3)}`;
    case 'trl':
      return `TRL ${v.toFixed(1)}`;
    case 'percent':
      return `${Math.round(v)}%`;
  }
}

export const PROVENANCE_LABEL: Record<Provenance, string> = {
  data: 'Data',
  assessment: 'Assessment',
  judgment: 'Your judgment',
  scenario: 'Scenario',
  unknown: 'Unknown',
};

/** An Inputs record with every value unknown: the starting point before evidence is attached. */
export function emptyInputs(): Inputs {
  const out = {} as Record<InputId, { value: unknown; provenance: Provenance }>;
  for (const id of ALL_INPUT_IDS) out[id] = { value: UNKNOWN, provenance: 'unknown' };
  return out as unknown as Inputs;
}
