// Yards & Fences decision engine: shared types.
//
// The engine is pure (no IO, no DOM, no randomness). Every input carries its provenance so a result
// can say which parts rest on data, on a curated assessment, on the reader's judgment, on a scenario
// assumption, or on nothing at all. There is no aggregate score: each yard answers a different
// question through explicit, tri-state rules (rules.ts), and unknown inputs are bracketed, never
// filled (classify.ts).

export type Yard = 'small' | 'medium' | 'large';
export type YardOutcome = Yard | 'mixed' | 'indeterminate';
export type Tri = 'yes' | 'no' | 'unknown';
export type Confidence = 'low' | 'moderate' | 'high';

/** Where an input value came from. Shown as DATA / ASSESSMENT / YOUR JUDGMENT / SCENARIO / UNKNOWN. */
export type Provenance = 'data' | 'assessment' | 'judgment' | 'scenario' | 'unknown';

export const UNKNOWN = 'unknown' as const;
export type Unknown = typeof UNKNOWN;

// ── Categorical inputs ──────────────────────────────────────────────────────────────────────────
// Option order runs from the "open" end to the "security" end (or, for the capability questions,
// from "no case" to "strong case"); classify.ts brackets an unknown input with its two extremes.

export const DOMAINS = {
  consequence: ['low', 'moderate', 'high', 'severe'],
  timeHorizon: ['years', 'months', 'weeks', 'days'],
  digitallyActive: ['no', 'yes'],
  remoteControl: ['no', 'limited', 'yes'],
  separability: ['separable', 'partial', 'inseparable'],
  substitutability: ['easy', 'moderate', 'hard', 'impossible'],
  qualificationTime: ['lt3m', 'm3to12', 'y1to3', 'gt3y'],
  reconstitutionTime: ['lt6m', 'm6to12', 'y1to3', 'gt3y'],
  stockpile: ['gt12m', 'm3to12', 'lt3m', 'none'],
  surgeCapacity: ['covers', 'partial', 'none'],
  processKnowledge: ['no', 'partly', 'yes'],
  workforceSuppliers: ['no', 'partly', 'yes'],
  adjacentInnovation: ['no', 'partly', 'yes'],
  complementaryCapabilities: ['no', 'partly', 'yes'],
  costPerfAdvantage: ['no', 'partly', 'yes'],
  incumbentVulnerable: ['no', 'partly', 'yes'],
  scalePathway: ['no', 'partly', 'yes'],
  beyondNovelty: ['no', 'partly', 'yes'],
  maturity: ['mature', 'scaling', 'emerging'],
} as const;

export type CategoricalInputId = keyof typeof DOMAINS;
export type CategoricalValue<K extends CategoricalInputId> = (typeof DOMAINS)[K][number];

// ── Numeric inputs (observed data by default; adjustable in the scenario lab) ────────────────────

export const NUMERIC_IDS = [
  'nonAlliedTopShare', // share of world supply held by the largest supplier outside the trusted coalition (0-1)
  'topSupplierShare', // share of the largest supplier, any country (0-1)
  'hhi', // Herfindahl-Hirschman index of supplier shares (0-1)
  'alliedShare', // share of world supply held by coalition core members and partners (0-1)
  'homeShare', // the assessed country's share of world supply (0-1)
  'trl', // technology readiness level, 1-9 (global)
  'premium', // domestic price premium the policy would impose, percent (0-75)
] as const;
export type NumericInputId = (typeof NUMERIC_IDS)[number];

export type InputId = CategoricalInputId | NumericInputId;

export interface InputValue<V> {
  value: V | Unknown;
  provenance: Provenance;
  /** Free text shown next to the value: a source, a curator's note, a data caveat. */
  note?: string;
  /** For data: the label of the supplier behind a share (e.g. "China"). */
  subject?: string;
  /** For shares: what the share is of, e.g. "world exports of this sub-component (2024)". */
  measure?: string;
  /** Curator's confidence in an assessment value. */
  assessmentConfidence?: Confidence;
}

export type Inputs = {
  [K in CategoricalInputId]: InputValue<CategoricalValue<K>>;
} & {
  [K in NumericInputId]: InputValue<number>;
};

export type Strategy = 'preserve' | 'catchUp' | 'leapAhead';

/** What the reader is assessing: one component (or a whole stage when no component is chosen). */
export interface AssessmentContext {
  country: string;
  technology: string;
  stage: 'Upstream' | 'Midstream' | 'Downstream';
  componentId?: string;
  componentLabel: string;
  /** 'control' layers are judged only on the digital path; physical layers on the physical path. */
  layer: 'control' | 'hardware' | 'material' | 'fuel' | 'structure' | 'stage';
}

export interface Assessment {
  context: AssessmentContext;
  inputs: Inputs;
  strategies: Strategy[];
  /** What benefit the reader says would justify a premium (Gate 4); informs the posture only. */
  premiumJustification?: 'security' | 'learning' | 'diversification' | 'jobs' | 'none' | Unknown;
}

// ── Gate results ─────────────────────────────────────────────────────────────────────────────────

export interface InputRef {
  id: InputId;
  value: string | number | Unknown;
  provenance: Provenance;
}

export interface Predicate {
  id: string;
  label: string;
  state: Tri;
  /** True when the answer rests on a value at a threshold's edge ("6-12 months", "partly", a share
   *  within the marginal band). Marginal answers lower confidence; they never flip the answer. */
  marginal: boolean;
  detail: string;
  inputs: InputRef[];
  applicable: boolean;
}

export type SmallScope = 'control-layer' | 'control-plus-integrated' | 'whole-product' | 'input' | 'unclear';

export interface SmallYardGate {
  state: Tri;
  acute: Predicate;
  digital: Predicate;
  physical: Predicate;
  /** Sub-tests of the physical path, for the explanation tree. */
  physicalParts: { coercion: Predicate; substitution: Predicate; timing: Predicate };
  scope: SmallScope | null;
}

export type RationaleStrength = 'strong' | 'moderate' | 'none' | Unknown;

export interface MediumYardGate {
  state: Tri;
  position: Predicate;
  learning: Predicate & { level: 'strong' | 'moderate' | 'weak' | Unknown };
  leap: Predicate;
  strength: RationaleStrength;
}

export interface LargeYardGate {
  state: Tri;
  premium: Predicate & { band: 'low' | 'moderate' | 'high' | Unknown };
  concentrationWatch: boolean;
}

export interface GateSet {
  small: SmallYardGate;
  medium: MediumYardGate;
  large: LargeYardGate;
  /** The yard these gates point to, read strictly (unknown security tests are not established). */
  yard: Yard;
  variant: YardVariant;
}

export type YardVariant =
  | 'small-control'
  | 'small-integrated'
  | 'small-whole'
  | 'small-input'
  | 'small-unclear'
  /** both paths hold: a commandable control layer and a physical chokepoint */
  | 'small-both'
  | 'medium'
  | 'medium-costly'
  | 'large'
  | 'large-floor'
  | 'large-diversify';

// ── Result ───────────────────────────────────────────────────────────────────────────────────────

export interface DecisionReason {
  kind: 'driver' | 'not-by' | 'context';
  text: string;
  inputs: InputId[];
}

export interface MissingEvidence {
  input: InputId;
  label: string;
  /** True when filling it could change the yard, not just the confidence. */
  pivotal: boolean;
  /** Plain-language effect, e.g. "If qualification takes over a year, the result could be Small Yard". */
  effect: string;
  /** Possible yards when the input is resolved to each end of its range. */
  ifLow: Yard[];
  ifHigh: Yard[];
}

export interface ChangeCondition {
  toward: Yard;
  text: string;
  input?: InputId;
  from?: string;
  to?: string;
  generic?: boolean;
}

export interface DecisionNode {
  id: string;
  question: string;
  answer: Tri | 'n/a';
  marginal?: boolean;
  detail: string;
  inputs: InputRef[];
  /** Set on the terminal node. */
  outcome?: YardOutcome;
  children?: DecisionNode[];
}

export interface YardResult {
  yard: YardOutcome;
  /** For an indeterminate result: the yard the evidence leans to when unproven security tests are
   *  treated as not met (the burden of proof is on the high fence). */
  leaning?: Yard;
  /** Every yard the unknown inputs could still produce. */
  possible: Yard[];
  variant: YardVariant;
  scope?: SmallScope;
  scopeLabels?: string[];
  confidence: Confidence;
  confidenceWhy: string[];
  evidenceCompleteness: number;
  reasons: DecisionReason[];
  missingEvidence: MissingEvidence[];
  changeConditions: ChangeCondition[];
  recommendedPosture: string;
  postureDetail: string;
  strategies: Strategy[];
  warnings: string[];
  gates: GateSet;
  path: DecisionNode[];
}
