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

export type CaseKind = 'posture_cell' | 'aib_node' | 'compact' | 'partnership' | 'sector';
export const CASE_KINDS: readonly CaseKind[] = ['posture_cell', 'aib_node', 'compact', 'partnership', 'sector'];

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
}

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

export type IssueType = 'data' | 'method' | 'context' | 'framework' | 'actionability' | 'insufficient_evidence';
export const ISSUE_TYPE_LABEL: Readonly<Record<IssueType, string>> = {
  data: 'Data problem',
  method: 'Method problem',
  context: 'Missing context',
  framework: 'Framework ambiguity',
  actionability: 'Policy / actionability issue',
  insufficient_evidence: 'Insufficient evidence',
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

/** "I don't know / outside my expertise" answer value, valid wherever a choice is asked. */
export const DONT_KNOW = 'dont_know';

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
  /** Canonical option value or DONT_KNOW. */
  choice: string;
  confidence: 1 | 2 | 3 | 4 | 5 | null;
  factors: string[];
  rationale?: string | null;
  answered_at: string;
}

export interface RevealResponse {
  revealed_at: string;
  verdict: Verdict;
  main_issue?: { text: string; type: IssueType; metric_ids?: string[] } | null;
  suggested_choice?: string | null;
  evidence_wanted?: string | null;
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
