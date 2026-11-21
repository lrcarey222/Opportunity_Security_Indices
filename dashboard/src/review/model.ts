// The shared contract of the QC / expert peer-review system (docs/QC_SYSTEM_DESIGN.md section 4).
// Pure types and constants only: no IO, no React. Every other review module, the Node CLIs in
// dashboard/scripts/review and the admin dashboard import from here, so a field changes in one place.
// Labels are never re-typed here: answer options for postures, AIB classes and compact decisions are
// read from content/vocabulary.ts and compact/decide.ts by review/vocab.ts.

import type { Verb } from '../posture/contract.ts';

export const SUBMISSION_SCHEMA = 'aib-review-submission';
export const SUBMISSION_SCHEMA_VERSION = 1;
export const CASES_SCHEMA = 'aib-review-cases';
export const REVIEW_MANIFEST_SCHEMA = 'aib-review-manifest';

// ---------------------------------------------------------------------------------------------
// Campaigns

export type CampaignStatus = 'draft' | 'active' | 'closed';
export type ReviewType = 'case' | 'methodology' | 'framework' | 'decision_use' | 'threat' | 'break_model';
export const REVIEW_TYPES: readonly ReviewType[] = ['case', 'methodology', 'framework', 'decision_use', 'threat', 'break_model'];

export interface AssignmentRules {
  assigned_per_reviewer: { min: number; max: number };
  anchors_per_reviewer: number;
  out_of_specialty: 'none' | 'optional' | 'one';
  allow_self_selected: boolean;
  /** Self-rating (1-5) at or above which a stated domain counts as expertise. */
  min_domain_confidence: number;
}

export interface Campaign {
  campaign_id: string;
  name: string;
  status: CampaignStatus;
  opens?: string | null;
  closes?: string | null;
  description?: string | null;
  review_types: ReviewType[];
  case_ids: string[];
  anchor_cases: string[];
  target_reviews_per_anchor: number;
  assignment: AssignmentRules;
  /** Empty or absent = all cards / scenarios. */
  method_card_ids?: string[];
  framework_scenario_ids?: string[];
  /** Optional pre-registration: codes only, never names. */
  reviewers?: { code: string; groups?: string[] }[];
}

// ---------------------------------------------------------------------------------------------
// Cases

/** 'sector_chain' (UX v2, docs/QC_UX_REVIEW.md): one strategic compact's value chain under a scenario, asking at
 *  which stages allied intervention is warranted. 'sector' is the earlier reserved kind (no cases use it). */
export type CaseKind = 'posture_cell' | 'aib_node' | 'compact' | 'partnership' | 'sector' | 'sector_chain';
export const CASE_KINDS: readonly CaseKind[] = ['posture_cell', 'aib_node', 'compact', 'partnership', 'sector', 'sector_chain'];

/** The four Explorer products a case reviews; each gets its own tailored brief and reveal. */
export type ReviewProduct = 'country' | 'sector' | 'partnership' | 'compact';
export const REVIEW_PRODUCT_OF_KIND: Readonly<Record<CaseKind, ReviewProduct>> = {
  posture_cell: 'country',
  aib_node: 'sector',
  sector: 'sector',
  sector_chain: 'sector',
  compact: 'compact',
  partnership: 'partnership',
};

export type AibScenarioKey = 'decoupling' | 'managed' | 'baseline';

export interface CaseKey {
  iso3?: string;
  /** Posture technology display name, e.g. "Batteries", "Electric Vehicles". */
  tech?: string;
  /** One of SUPPLY_STAGES, e.g. "Refining". */
  stage?: string;
  node?: string;
  sid?: string;
  pair?: string;
  scenario?: AibScenarioKey;
}

/** One fact shown to the reviewer: before the blind answer (context) or after it (reveal). */
export interface CaseFact {
  id: string;
  label: string;
  value: number | string | boolean | null;
  display: string;
  unit?: string | null;
  source?: string | null;
  source_year?: string | number | null;
  note?: string | null;
}

/** As authored in config/review/review_cases.yml. */
export interface CaseDefinition {
  case_id: string;
  kind: CaseKind;
  key: CaseKey;
  title: string;
  question: string;
  sector_groups: string[];
  country_groups: string[];
  stage?: string | null;
  assignment_group: string;
  anchor: boolean;
  why_selected: string;
  explorer_path: string;
  /** Snapshot field ids shown before the blind answer. Must never reveal the conclusion. */
  context_fields: string[];
  /** Snapshot field ids shown only after the blind answer. */
  reveal_fields: string[];
  decision_use?: boolean;
  threat?: boolean;
  /** UX v2: the authored parts of the case brief (the builder computes the rest from the bundles). */
  brief?: BriefAuthoring;
}

/** Authored brief text in config/review/review_cases.yml. Neutral: it must never name or hint at the conclusion. */
export interface BriefAuthoring {
  /** E.g. "How should Japan manage battery component manufacturing through 2035?" */
  decision_question: string;
  /** 2-3 sentences: whose decision this approximates, the objective, what the analysis does and does not determine. */
  decision_context: string;
  /** The objective being optimised, e.g. "Resilient and affordable supply, not maximum self-sufficiency." */
  objective?: string | null;
  /** Overrides the builder's time horizon label when set. */
  time_horizon?: string | null;
  /** Ordered evidence fact ids to show (5-8); the builder's default selection when absent. */
  evidence_ids?: string[];
  /** Assumption ids (see BriefAssumption) to show; the builder's default for the kind when absent. */
  assumption_ids?: string[];
}

// ---------------------------------------------------------------------------------------------
// Case brief (shown BEFORE the blind answer) and reasoning (shown only AFTER it). UX v2.

export interface BriefIdentityPart {
  role: 'country' | 'pair' | 'technology' | 'stage' | 'node' | 'chain' | 'compact' | 'scenario';
  label: string;
  code?: string | null;
}

export interface BriefWarning {
  id: string;
  level: 'warn' | 'info';
  /** E.g. "Production evidence limited". */
  title: string;
  /** E.g. "Current capability is inferred primarily from trade data." */
  text: string;
}

/** One neutral, observed or externally sourced fact. Never a model score, class or outcome-naming label. */
export interface BriefFact {
  id: string;
  label: string;
  value: number | string | null;
  display: string;
  /** Plain-English reading using generic conventions (e.g. DOJ/FTC HHI bands), never the model's thresholds. */
  interpretation: string | null;
  source: string | null;
  /** Source year or period, e.g. "2022-24", "latest 12 months to Aug 2026". */
  vintage: string | null;
  /** True for a neutral model estimate shown as evidence (e.g. coverage from existing trade links); labelled so. */
  estimate?: boolean;
  warnings?: BriefWarning[];
}

/** A modelling choice behind this kind of conclusion, worded neutrally; links to a method card when one exists. */
export interface BriefAssumption {
  id: string;
  label: string;
  text: string;
  method_card_id?: string | null;
}

export interface BriefDefinition {
  term: string;
  text: string;
}

/** The six-stage chain with the stage(s) this case is about highlighted. */
export interface StageLocation {
  stages: { key: string; label: string }[];
  highlight: string[];
}

/** One link of a causal chain (compact: problem -> existing trade -> gap -> ...). */
export interface ChainStep {
  id: string;
  label: string;
  display: string;
  text: string;
  /** A model estimate rather than an observed fact; always labelled as such. */
  estimate: boolean;
}

export interface PartnerSide {
  iso3: string;
  name: string;
  /** Observed strengths (e.g. "World export share 11% in battery components, 2022-24"). */
  brings: string[];
  /** Observed exposures (e.g. "Largest battery-mineral supplier: Australia, 41% of imports"). */
  exposed_on: string[];
}

export interface CaseBrief {
  product: ReviewProduct;
  identity: { title: string; parts: BriefIdentityPart[] };
  decision_question: string;
  decision_context: string;
  objective: string;
  time_horizon: { label: string; note?: string | null };
  scenario: { key: string; label: string; description: string } | null;
  stage_location: StageLocation | null;
  /** 5-8 neutral facts. */
  evidence: BriefFact[];
  assumptions: BriefAssumption[];
  /** Case-level data-quality warnings, shown immediately (not in an appendix). */
  warnings: BriefWarning[];
  /** What the data cannot show for this case, stated plainly. */
  missing_evidence: string[];
  definitions: BriefDefinition[];
  /** Tailored pre-blind sections (never the recommendation). */
  partnership?: { a: PartnerSide; b: PartnerSide; shared_problem: string[] } | null;
  compact_chain?: ChainStep[] | null;
  sector_chain?: { stage: string; label: string; facts: BriefFact[] }[] | null;
}

export interface ReasoningStep {
  /** need | prize | position | pathway | secure | large_yard | deployment | exposure | gap | concentration | coverage | dividend | materiality | ... */
  id: string;
  label: string;
  /** Plain level word: High / Moderate / Low / Yes / No / Not measured ... */
  level: string;
  value: number | null;
  threshold: number | null;
  /** One sentence: why this level. */
  sentence: string;
  /** The main evidence drivers, in plain words. */
  drivers: string[];
}

/** "Why this, not the neighbouring alternative?" */
export interface AlternativeExplanation {
  value: string;
  label: string;
  /** Why the Explorer did not choose it. */
  why_not: string;
  /** What would have to change for it to become the more appropriate conclusion. */
  would_need: string;
  /** The decisive input and its distance from the threshold, e.g. "Competitive position 0.763 vs bar 0.75". */
  margin?: string | null;
}

export interface CaseReasoning {
  conclusion: { value: string; label: string; summary: string };
  steps: ReasoningStep[];
  /** The "Therefore" sentence: the decision rule that was applied, in plain English. */
  rule: string;
  alternatives: AlternativeExplanation[];
  /** Post-reveal remainder of the compact chain (coordinated trade, new capacity, hosts, coalition, resilience after). */
  compact_chain?: ChainStep[] | null;
  partnership?: { coordination_changes: string[]; memo_choices: { kind: string; label: string; items: string[] }[] } | null;
  sector?: { explorer_stages: string[]; lead_stage: string | null; chokepoint_note?: string | null } | null;
  /** Level 3: the Explorer deep link that shows this case's analysis. */
  technical_path?: string | null;
}

export interface BundleFileStamp {
  file: string;
  sha256: string;
  created: string | null;
  git_commit: string | null;
}

export interface AnswerOption {
  value: string;
  label: string;
  definition?: string;
}

/** The conclusion a case reveals, in canonical keys. */
export interface CaseConclusion {
  /** Canonical key: a Verb or 'not_assessed' (posture), an AIB class, a CompactDecision, ... */
  value: string;
  label: string;
  /** Fence / yard / posture word where applicable. */
  detail?: string | null;
  reason?: string | null;
  confidence?: string | null;
}

export interface CaseSnapshot {
  conclusion: CaseConclusion;
  /** All facts by id; context_fields and reveal_fields index into this. */
  facts: Record<string, CaseFact>;
  /** Decision trace where the bundle has one (posture: the detail `trace`). */
  trace?: { step: string; test: string; value?: number | null; threshold?: number | null; result: string }[];
  /** Threat before -> intervention -> after, for AIB nodes and compacts (review/qc/threat.ts). */
  threat?: ThreatMetrics | null;
  /** Robustness of the conclusion under alternative specifications (review/qc/sensitivity.ts). */
  robustness?: RobustnessRecord | null;
  /** Data-quality notes, kept separate from robustness and from expert validation. */
  data_quality?: { grade: string | null; notes: string[] };
  /** UX v2: the neutral case brief (pre-blind) and the reasoning reveal (post-blind only). */
  brief?: CaseBrief | null;
  reasoning?: CaseReasoning | null;
}

/** As built into public/data/review/cases.<campaign>.json. */
export interface ReviewCase extends CaseDefinition {
  snapshot: CaseSnapshot;
  snapshot_sha256: string;
  bundle: BundleFileStamp[];
  data_release: string;
  app_commit: string;
  built_at: string;
  answer_options: AnswerOption[];
}

export interface CasesFile {
  schema: typeof CASES_SCHEMA;
  schema_version: 1;
  campaign_id: string;
  data_release: string;
  built_at: string;
  app_commit: string;
  cases: ReviewCase[];
}

// ---------------------------------------------------------------------------------------------
// Method cards and framework scenarios (config/review/*.yml -> public/data/review/*.json)

export interface MethodCard {
  card_id: string;
  title: string;
  area: 'need' | 'prize' | 'position' | 'choice_rule' | 'secure' | 'deployment' | 'aib_stress_test' | 'aib_compact' | 'aib_partners' | 'data';
  current_choice: string;
  why: string;
  example: { text: string; explorer_path?: string | null };
  alternatives: string[];
  affected_outputs: string[];
  /** File:line references in the codebase, for NEIS (not shown to external reviewers by default). */
  code_refs: string[];
  config_keys?: string[];
  method_tags: string[];
}

export interface FrameworkScenario {
  scenario_id: string;
  concept: string;
  prompt: string;
  /** Canonical keys with labels; drawn from the vocabulary constants by the builder when `option_set` is given. */
  options: AnswerOption[];
  option_set?: 'posture_choice' | 'aib_class' | 'compact_decision' | 'custom';
  intended_answer: string;
  explanation: string;
  terms: string[];
}

// ---------------------------------------------------------------------------------------------
// Reviewer profile (stays in the reviewer's browser; only a digest travels with submissions)

export type AdvisedOn = 'governments' | 'firms' | 'investors' | 'dfis' | 'none';

export interface ReviewerProfile {
  reviewer_id: string;
  name?: string | null;
  anonymous: boolean;
  organization?: string | null;
  role?: string | null;
  countries: string[];
  sectors: string[];
  stages: string[];
  methods: string[];
  /** Self-rated confidence 1-5 keyed by tag (e.g. "sector:batteries", "country:KOR", "method:trade_data"). */
  confidence: Record<string, number>;
  familiarity_neis: 0 | 1 | 2 | 3;
  familiarity_explorer: 0 | 1 | 2 | 3;
  advised_on: AdvisedOn[];
  created_at: string;
  /** UX v2: decision review (domain experts) or technical review (data / methods); default decision. */
  review_mode?: ReviewMode;
}

export type ReviewMode = 'decision' | 'technical';

export type AssignmentReason = 'assigned' | 'anchor' | 'outside' | 'self_selected';
export type ExpertiseMatch = 'in' | 'adjacent' | 'outside';

export interface Assignment {
  case_id: string;
  reason: AssignmentReason;
  match: ExpertiseMatch;
  optional: boolean;
}

// ---------------------------------------------------------------------------------------------
// Submissions

export type SubmissionKind = 'case_review' | 'method_feedback' | 'framework_answer' | 'decision_use' | 'threat_review' | 'data_issue' | 'break_model';

/** Verdict after the reveal (design 4.4). */
export type Verdict = 1 | 2 | 3 | 4 | 5 | 6 | 7;
export const VERDICT_LABEL: Readonly<Record<Verdict, string>> = {
  1: 'Matches my prior assessment.',
  2: 'Different from my prior, but the evidence persuades me.',
  3: 'Plausible, but the evidence is insufficient.',
  4: 'Probably wrong because the underlying data appear wrong.',
  5: 'Probably wrong because the methodology / weighting / threshold is wrong.',
  6: 'Probably wrong because important context is missing.',
  7: 'I do not have enough expertise to assess.',
};
/** Verdicts that require the "single most important miss" explanation. */
export const VERDICTS_NEEDING_EXPLANATION: readonly Verdict[] = [3, 4, 5, 6];

export type IssueType = 'data' | 'method' | 'context' | 'framework' | 'actionability' | 'insufficient_evidence' | 'decision_rule';
export const ISSUE_TYPE_LABEL: Readonly<Record<IssueType, string>> = {
  data: 'Data problem',
  method: 'Method problem (interpretation / weighting)',
  context: 'Missing context',
  framework: 'Framework ambiguity / poorly defined recommendation',
  actionability: 'Policy / actionability issue',
  insufficient_evidence: 'Insufficient evidence',
  decision_rule: 'Decision rule problem',
};
/** The disagreement type a verdict implies (verdicts 1, 2 and 7 imply none). */
export const VERDICT_ISSUE_TYPE: Readonly<Partial<Record<Verdict, IssueType>>> = {
  3: 'insufficient_evidence',
  4: 'data',
  5: 'method',
  6: 'context',
};

export type Severity = 'cosmetic' | 'minor' | 'material' | 'critical';
export const SEVERITY_LABEL: Readonly<Record<Severity, string>> = {
  cosmetic: 'Cosmetic',
  minor: 'Minor',
  material: 'Material',
  critical: 'Critical / could reverse strategic conclusion',
};
/** Materiality weights of the priority score (design 6.6). */
export const SEVERITY_WEIGHT: Readonly<Record<Severity, number>> = { cosmetic: 0.1, minor: 0.3, material: 0.6, critical: 1.0 };
export const MATCH_WEIGHT: Readonly<Record<ExpertiseMatch, number>> = { in: 1.0, adjacent: 0.7, outside: 0.4 };

/** "I don't know / outside my expertise" answer value, valid wherever a choice is asked. Legacy (v1) answers; v2
 *  case reviews use the three reviewer states below, and DONT_KNOW is read as OUTSIDE_EXPERTISE. */
export const DONT_KNOW = 'dont_know';

// ---------------------------------------------------------------------------------------------
// UX v2 answer vocabulary (docs/QC_UX_REVIEW.md). Stored verbatim in submissions.

/** Reviewer states offered beside the canonical conclusions on every blind question. */
export const NONE_OF_THESE = 'none_of_these';
export const INSUFFICIENT_EVIDENCE = 'insufficient_evidence';
export const OUTSIDE_EXPERTISE = 'outside_expertise';
export const REVIEWER_STATES: readonly string[] = [NONE_OF_THESE, INSUFFICIENT_EVIDENCE, OUTSIDE_EXPERTISE];
export const REVIEWER_STATE_LABEL: Readonly<Record<string, string>> = {
  [NONE_OF_THESE]: 'None of these reflects my recommendation',
  [INSUFFICIENT_EVIDENCE]: 'Insufficient evidence to judge',
  [OUTSIDE_EXPERTISE]: 'Outside my expertise',
};

export type ConfidenceLevel = 'low' | 'medium' | 'high';
export const CONFIDENCE_LEVEL_LABEL: Readonly<Record<ConfidenceLevel, string>> = { low: 'Low', medium: 'Medium', high: 'High' };
/** The 1-5 value stored in BlindAnswer.confidence for v1 consumers. */
export const CONFIDENCE_LEVEL_SCORE: Readonly<Record<ConfidenceLevel, 1 | 3 | 5>> = { low: 1, medium: 3, high: 5 };

export type DriverFactor =
  | 'concentration_security'
  | 'domestic_capability'
  | 'allied_alternatives'
  | 'market_opportunity'
  | 'demand_growth'
  | 'technological_importance'
  | 'cost_competitiveness'
  | 'missing_information'
  | 'other';
export const DRIVER_FACTOR_LABEL: Readonly<Record<DriverFactor, string>> = {
  concentration_security: 'Concentration / security exposure',
  domestic_capability: 'Domestic capability',
  allied_alternatives: 'Allied alternatives',
  market_opportunity: 'Market opportunity',
  demand_growth: 'Demand growth',
  technological_importance: 'Technological importance',
  cost_competitiveness: 'Cost competitiveness',
  missing_information: 'Missing information',
  other: 'Other',
};

/** What information is missing (insufficient-evidence branch) / would most help resolve the case (red team). */
export type EvidenceGap =
  | 'production_capacity'
  | 'market_demand'
  | 'firm_capability'
  | 'ownership'
  | 'alternative_suppliers'
  | 'allied_spare_capacity'
  | 'cost_competitiveness'
  | 'project_pipeline'
  | 'expansion_feasibility'
  | 'qualification_constraints'
  | 'technology_readiness'
  | 'workforce_infrastructure'
  | 'policy_environment'
  | 'policy_durability'
  | 'geopolitical_context'
  | 'other';
export const EVIDENCE_GAP_LABEL: Readonly<Record<EvidenceGap, string>> = {
  production_capacity: 'Actual production capacity',
  market_demand: 'Market size / demand',
  firm_capability: 'Firm-level capability',
  ownership: 'Ownership (firm-level)',
  alternative_suppliers: 'Alternative suppliers',
  allied_spare_capacity: 'Allied spare capacity',
  cost_competitiveness: 'Cost competitiveness / cost curves',
  project_pipeline: 'Project pipeline',
  expansion_feasibility: 'Capacity expansion feasibility',
  qualification_constraints: 'Qualification constraints / lead times',
  technology_readiness: 'Technology readiness',
  workforce_infrastructure: 'Workforce / infrastructure',
  policy_environment: 'Policy environment',
  policy_durability: 'Policy durability',
  geopolitical_context: 'Geopolitical context',
  other: 'Other',
};

/** The post-reveal taxonomy (stored verbatim; replaces the 1-7 scale for v2 reviews, which keep a derived `verdict`). */
export type ReviewVerdictCode =
  | 'same_same'
  | 'same_different_reasons'
  | 'persuaded'
  | 'plausible_insufficient'
  | 'wrong_fact'
  | 'missing_factor'
  | 'wrong_weighting'
  | 'wrong_rule'
  | 'too_broad'
  | 'outside_expertise';
export const REVIEW_VERDICT_CODES: readonly ReviewVerdictCode[] = [
  'same_same',
  'same_different_reasons',
  'persuaded',
  'plausible_insufficient',
  'wrong_fact',
  'missing_factor',
  'wrong_weighting',
  'wrong_rule',
  'too_broad',
  'outside_expertise',
];
export const REVIEW_VERDICT_LABEL: Readonly<Record<ReviewVerdictCode, string>> = {
  same_same: 'Same conclusion, broadly same reasoning.',
  same_different_reasons: 'Same conclusion, but for different reasons.',
  persuaded: 'Different initial conclusion, but the Explorer persuaded me.',
  plausible_insufficient: "The Explorer's conclusion is plausible, but the evidence is insufficient.",
  wrong_fact: 'I disagree because an underlying fact or data point is wrong.',
  missing_factor: 'I disagree because an important factor is missing.',
  wrong_weighting: 'I disagree with how the evidence is interpreted or weighted.',
  wrong_rule: 'I disagree with the decision rule linking the evidence to the recommendation.',
  too_broad: 'The recommendation is too broad or poorly defined.',
  outside_expertise: 'Outside my expertise.',
};
/** The v1 verdict each code maps to, so v1 aggregation and exports keep working. */
export const REVIEW_VERDICT_LEGACY: Readonly<Record<ReviewVerdictCode, Verdict>> = {
  same_same: 1,
  same_different_reasons: 1,
  persuaded: 2,
  plausible_insufficient: 3,
  wrong_fact: 4,
  missing_factor: 6,
  wrong_weighting: 5,
  wrong_rule: 5,
  too_broad: 5,
  outside_expertise: 7,
};
export const REVIEW_VERDICT_ISSUE_TYPE: Readonly<Partial<Record<ReviewVerdictCode, IssueType>>> = {
  plausible_insufficient: 'insufficient_evidence',
  wrong_fact: 'data',
  missing_factor: 'context',
  wrong_weighting: 'method',
  wrong_rule: 'decision_rule',
  too_broad: 'framework',
};
/** Codes that disagree with the Explorer (they trigger the red-team questions). */
export const DISAGREEING_VERDICT_CODES: readonly ReviewVerdictCode[] = ['plausible_insufficient', 'wrong_fact', 'missing_factor', 'wrong_weighting', 'wrong_rule', 'too_broad'];

/** "Is the Explorer's conclusion supported by the evidence?" (validity, asked separately from prior and persuasion). */
export type Validity = 'supported' | 'partly' | 'not_supported' | 'unsure';
/** "Did inspecting the evidence change your view?" */
export type Persuasion = 'changed' | 'somewhat' | 'unchanged';

/** Tailored product questions asked after the reveal; answers stored in RevealResponse.product_answers. */
export const PRODUCT_QUESTIONS: Readonly<Record<ReviewProduct, { id: string; text: string; options: { value: string; label: string }[] }[]>> = {
  country: [],
  sector: [
    {
      id: 'bottleneck_located',
      text: 'Has the Explorer located the actual strategic bottleneck in this supply chain?',
      options: [
        { value: 'yes', label: 'Yes' },
        { value: 'partly', label: 'Partly' },
        { value: 'no', label: 'No' },
        { value: 'unsure', label: 'Not sure' },
      ],
    },
  ],
  partnership: [
    {
      id: 'complementarity',
      text: 'Does this relationship represent genuine industrial complementarity or primarily diplomatic compatibility?',
      options: [
        { value: 'industrial', label: 'Genuine industrial complementarity' },
        { value: 'mostly_industrial', label: 'Mostly industrial, partly diplomatic' },
        { value: 'mostly_diplomatic', label: 'Mostly diplomatic compatibility' },
        { value: 'diplomatic', label: 'Primarily diplomatic compatibility' },
        { value: 'unsure', label: 'Not sure' },
      ],
    },
    { id: 'incentives', text: 'Do both partners have the incentive to act?', options: [{ value: 'both', label: 'Both' }, { value: 'one', label: 'Only one' }, { value: 'neither', label: 'Neither' }, { value: 'unsure', label: 'Not sure' }] },
    { id: 'asymmetry', text: 'Is the benefit roughly mutual, or strongly asymmetric?', options: [{ value: 'mutual', label: 'Roughly mutual' }, { value: 'asymmetric', label: 'Strongly asymmetric' }, { value: 'unsure', label: 'Not sure' }] },
    { id: 'durability', text: 'Would the arrangement survive a change of government or market cycle?', options: [{ value: 'durable', label: 'Likely durable' }, { value: 'fragile', label: 'Fragile' }, { value: 'unsure', label: 'Not sure' }] },
    { id: 'substitution', text: 'Could either side easily substitute another partner?', options: [{ value: 'hard', label: 'Hard to substitute' }, { value: 'easy', label: 'Easy to substitute' }, { value: 'unsure', label: 'Not sure' }] },
  ],
  compact: [
    {
      id: 'ameliorates_threat',
      text: 'Does this compact materially ameliorate the stated threat?',
      options: [
        { value: 'yes', label: 'Yes, materially' },
        { value: 'partly', label: 'Partly' },
        { value: 'no', label: 'No' },
        { value: 'unsure', label: 'Not sure' },
      ],
    },
  ],
};

export type NoteConversion = 'data_issue' | 'missing_context' | 'method_concern' | 'comment';
export const NOTE_CONVERSION_LABEL: Readonly<Record<NoteConversion, string>> = {
  data_issue: 'Data issue',
  missing_context: 'Missing context',
  method_concern: 'Methodological concern',
  comment: 'General comment',
};

/** The reviewer's scratchpad for one case (autosaved; travels with the case review when submitted). */
export interface ReviewerNote {
  id: string;
  text: string;
  created_at: string;
  updated_at: string;
  converted_to?: NoteConversion | null;
}

export interface PageContext {
  url: string;
  route: string;
  view: string | null;
  country: string | null;
  tech: string | null;
  stage: string | null;
  node: string | null;
  layer: string | null;
  pair: string | null;
  sid: string | null;
  scenario: string | null;
  displayed_result: string | null;
  metric_ids: string[];
  data_release: string | null;
  /**
   * Set when data_release is NOT the full review release: the page's stamps differ from the review manifest's (the
   * bundles named), or the page loaded none, or the review manifest could not be read (review/release.ts
   * resolveReportRelease). Null or absent when data_release is the review manifest's release ID.
   */
  release_note?: string | null;
  bundle_stamps: Record<string, { created: string | null; git_commit: string | null; bundle_id?: string | null } | null>;
  app_commit: string;
  captured_at: string;
}

export interface MetricRef {
  id: string;
  label: string;
  value: number | string | null;
  display: string;
  country?: string | null;
  tech?: string | null;
  stage?: string | null;
  source?: string | null;
  source_year?: string | number | null;
  transform?: string | null;
}

export interface BlindAnswer {
  /** Canonical option value, a REVIEWER_STATES value (v2) or DONT_KNOW (v1). For a sector_chain case: the lead stage. */
  choice: string;
  /** v2 stores CONFIDENCE_LEVEL_SCORE[confidence_level] here. */
  confidence: 1 | 2 | 3 | 4 | 5 | null;
  /** v1: case fact ids. v2: [driver] when a driver is chosen. */
  factors: string[];
  rationale?: string | null;
  answered_at: string;
  // ---- UX v2 (optional so v1 drafts and submissions stay valid)
  confidence_level?: ConfidenceLevel | null;
  /** The single most important factor driving the judgment. */
  driver?: DriverFactor | null;
  driver_text?: string | null;
  /** Sector chain: every stage the reviewer would intervene at (choice holds the one to act on first). */
  choices?: string[];
  /** Insufficient-evidence branch: what information is missing. */
  missing_evidence?: EvidenceGap[];
  missing_evidence_text?: string | null;
  /** Answer schema version: absent = 1. */
  ux_version?: 2;
}

export interface RevealResponse {
  revealed_at: string;
  /** v1 scale; for v2 reviews derived from verdict_code via REVIEW_VERDICT_LEGACY. */
  verdict: Verdict;
  main_issue?: { text: string; type: IssueType; metric_ids?: string[] } | null;
  suggested_choice?: string | null;
  evidence_wanted?: string | null;
  // ---- UX v2
  verdict_code?: ReviewVerdictCode | null;
  /** Prior: did the Explorer's conclusion differ from the blind answer (computed; null for reviewer states). */
  prior_differs?: boolean | null;
  validity?: Validity | null;
  persuasion?: Persuasion | null;
  /** Asked when the reviewer disagrees or had low confidence. */
  red_team?: { must_be_true?: string | null; resolving_evidence?: EvidenceGap | null; resolving_evidence_text?: string | null } | null;
  /** Answers to PRODUCT_QUESTIONS for the case's product, keyed by question id. */
  product_answers?: Record<string, string>;
}

export interface CaseReviewPayload {
  case_id: string;
  case_snapshot_sha256: string;
  assignment_reason: AssignmentReason;
  expertise_match: ExpertiseMatch;
  blind: BlindAnswer;
  reveal: RevealResponse;
  outside_expertise: boolean;
  time_spent_s: number | null;
  // ---- UX v2
  notes?: ReviewerNote[];
  review_mode?: ReviewMode | null;
}

export type Stance = 'retain' | 'modify' | 'reject' | 'unsure';
export type BiasDirection = 'overstates_need' | 'understates_need' | 'favours_incumbents' | 'favours_new_entrants' | 'other' | 'none' | 'unsure';

export interface MethodFeedbackPayload {
  card_id: string;
  stance: Stance;
  materiality: 1 | 2 | 3 | 4 | null;
  alternative?: string | null;
  affected_outputs?: string | null;
  bias_direction: BiasDirection;
  comment?: string | null;
  outside_expertise: boolean;
}

export interface FrameworkAnswerPayload {
  scenario_id: string;
  answer: string;
  intended_answer: string;
  correct: boolean;
  confidence: 1 | 2 | 3 | 4 | 5 | null;
  confusing_terms: string[];
  comment?: string | null;
}

/** 1 = not at all, 4 = fully. */
export type Likert4 = 1 | 2 | 3 | 4;

export interface DecisionUsePayload {
  case_id: string;
  would_do_differently: string;
  actionable: Likert4 | null;
  identifies_problem: Likert4 | null;
  separates_diagnosis_from_prescription: Likert4 | null;
  right_geography: Likert4 | null;
  evidence_required?: string | null;
  improvement?: string | null;
  decision_use?: string | null;
}

export type MissingConstraint = 'qualification' | 'infrastructure' | 'workforce' | 'financing' | 'demand' | 'permitting' | 'other';

export interface ThreatReviewPayload {
  case_id: string;
  threat_snapshot_sha256: string;
  reduces_threat: Likert4 | null;
  scale_sufficient: Likert4 | null;
  timing_plausible: Likert4 | null;
  commercially_plausible: Likert4 | null;
  missing_constraints: MissingConstraint[];
  success_looks_like?: string | null;
  stop_point?: string | null;
}

export type BreakType = 'surprising' | 'obviously_wrong' | 'misleading_data' | 'right_for_wrong_reason' | 'threshold_artifact' | 'omitted_variable';
export const BREAK_TYPE_LABEL: Readonly<Record<BreakType, string>> = {
  surprising: 'A surprising result',
  obviously_wrong: 'An obviously wrong result',
  misleading_data: 'A result driven by misleading data',
  right_for_wrong_reason: 'The right conclusion for the wrong reason',
  threshold_artifact: 'A threshold artifact',
  omitted_variable: 'A strategically important variable the model omits',
};

export interface DataIssuePayload {
  metric: MetricRef | null;
  what_is_wrong: string;
  corrected_value?: string | null;
  citation?: string | null;
  severity: Severity;
  /** Only on break_model submissions. */
  break_type?: BreakType;
}

export interface PayloadByKind {
  case_review: CaseReviewPayload;
  method_feedback: MethodFeedbackPayload;
  framework_answer: FrameworkAnswerPayload;
  decision_use: DecisionUsePayload;
  threat_review: ThreatReviewPayload;
  data_issue: DataIssuePayload;
  break_model: DataIssuePayload;
}

export interface ReviewerStamp {
  reviewer_id: string;
  name?: string | null;
  anonymous: boolean;
  /** sha256 of the canonical profile JSON (without the name), so exports can join on expertise. */
  profile_digest: string;
  /** Expertise tags travel with the submission so the admin can weight it without the full profile. */
  expertise?: { sectors: string[]; countries: string[]; stages: string[]; methods: string[]; confidence: Record<string, number> };
}

export interface Submission<K extends SubmissionKind = SubmissionKind> {
  submission_id: string;
  schema: typeof SUBMISSION_SCHEMA;
  schema_version: typeof SUBMISSION_SCHEMA_VERSION;
  campaign_id: string | null;
  kind: K;
  reviewer: ReviewerStamp;
  created_at: string;
  submitted_at: string | null;
  app_commit: string;
  data_release: string | null;
  page_context?: PageContext | null;
  payload: PayloadByKind[K];
}

export type AdjudicationStatus = 'open' | 'confirmed' | 'rejected' | 'duplicate' | 'resolved' | 'wont_fix';

/** Admin-only; a separate record so reviewer submissions stay immutable. */
export interface Adjudication {
  adjudication_id: string;
  submission_id: string;
  status: AdjudicationStatus;
  issue_type: IssueType | null;
  /** 0-1: the admin's confidence that the issue is real. */
  confidence_real: number | null;
  affected_outputs: number | string[] | null;
  note?: string | null;
  decided_by: string;
  decided_at: string;
  resolution_commit?: string | null;
  duplicate_of?: string | null;
}

// ---------------------------------------------------------------------------------------------
// Robustness and threat amelioration

export type RobustnessClass = 'stable' | 'sensitive' | 'fragile' | 'not_testable';

export interface SpecResult {
  spec_id: string;
  kind: 'local' | 'structural';
  label: string;
  choice: string;
  changed: boolean;
  fence_changed: boolean;
}

export interface RobustnessRecord {
  layer: 'bundle' | 'pipeline';
  case_key: CaseKey;
  baseline_choice: string;
  specs_run: number;
  local_flips: SpecResult[];
  structural_flips: SpecResult[];
  fence_change: boolean;
  class: RobustnessClass;
  /** Inputs / spec ids that drive the flips (e.g. "cp_contender_pct", "large_yard_min_signals"). */
  drivers: string[];
  note?: string | null;
}

export interface ThreatSide {
  hhi: number | null;
  largest_supplier: string | null;
  largest_share: number | null;
  top3_share: number | null;
  /** Share of baseline traded demand supplied by risky / non-allied sources, where exported. */
  risky_share: number | null;
  coverage: number | null;
  n_credible_suppliers: number | null;
  n1_coverage: number | null;
}

export interface ThreatMetrics {
  subject: { kind: 'aib_node' | 'compact'; id: string; scenario: AibScenarioKey; label: string };
  threat: string;
  intervention: string;
  before: ThreatSide;
  after: ThreatSide;
  lost_supply_usd: number | null;
  lost_supply_replaced_share: number | null;
  allied_coverage: number | null;
  remaining_gap: number | null;
  remaining_gap_usd: number | null;
  new_capacity_share: number | null;
  /** Not modelled anywhere in the Explorer today: always null with a note, never invented. */
  time_to_capacity_years: null;
  /** diversified: the largest share after < before and below the concentration line;
   *  shifted: a different single source now holds >= the line; unchanged; unknown. */
  dependence_outcome: 'diversified' | 'shifted' | 'unchanged' | 'unknown';
  notes: string[];
}

// ---------------------------------------------------------------------------------------------
// Golden cases (config/qc/golden_cases.yml)

export type GoldenCategory =
  | 'obvious_secure'
  | 'keep_open'
  | 'strong_producer'
  | 'import_dependent'
  | 'entrepot'
  | 'sparse'
  | 'near_threshold'
  | 'neis_central'
  | 'surprising'
  | 'method_sensitive';

export interface GoldenCase {
  id: string;
  category: GoldenCategory;
  kind: 'posture_cell' | 'aib_node' | 'compact';
  key: CaseKey;
  expect: {
    choice?: string;
    choice_in?: string[];
    fence?: 'high' | 'medium' | 'low';
    numeric?: Record<string, [number, number]>;
  };
  exact: boolean;
  change_requires_review: boolean;
  rationale: string;
  approved: { by: string; on: string; release: string | null };
}

export type GoldenStatus = 'pass' | 'fail' | 'missing' | 'not_applicable';

export interface GoldenResult {
  id: string;
  status: GoldenStatus;
  observed: Record<string, number | string | null>;
  failures: string[];
}

// Re-exported for review modules that need the canonical choice key type.
export type { Verb };
