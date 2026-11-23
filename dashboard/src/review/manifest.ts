// Validators of the review system's authored and built files (docs/QC_SYSTEM_DESIGN.md 4): campaigns,
// the case catalogue, built cases files, method cards, framework scenarios, golden cases and reviewer
// submissions. Each returns {ok, errors, warnings}; the build (build-review-data.mjs) and the export CLI
// fail on errors. The blind-first guarantee starts here: a case whose context_fields name a REVEALING fact
// (the conclusion, or anything that gives it away) is rejected. Canonical answer keys are the values of
// review/qc/answerOptions.ts answerOptions (the one source the case builder writes answer_options from), which
// reads the Explorer's own constants: posture choices from content/vocabulary.ts CHOICE_NAME (+ not_assessed),
// AIB classes from AIB_CLASS_NAME, operational compact (AIB-Cxx) types from aib/contract.ts AIB_COMPACT_TYPES,
// strategic compact (SC-*) conclusions sectoral / narrower / none (main has no compact/decide.ts), partnership
// memo boxes from MEMO_CHOICE_KINDS. Pure: no IO.

import {
  CASE_KINDS,
  CASES_SCHEMA,
  DONT_KNOW,
  REVIEWER_STATES,
  REVIEW_TYPES,
  SUBMISSION_SCHEMA,
  SUBMISSION_SCHEMA_VERSION,
  type CaseKey,
  type CaseKind,
  type SubmissionKind,
} from './model.ts';
import { AIB_COMPACT_TYPES } from '../aib/contract.ts';
import { compactTypeInfo } from '../aib/derive/types.ts';
import { conclusionOptions, MODEL_STATE_VALUES, STRATEGIC_COMPACT_DECISIONS } from './qc/answerOptions.ts';
import { ASSUMPTIONS, BRIEF_FACT_IDS_BY_KIND } from './qc/brief.ts';
import { lintAuthoredText, lintBriefNeutrality } from './qc/briefLint.ts';
import type { AnswerOption, CaseBrief, CaseConclusion } from './model.ts';
import { normaliseCountryTag, normaliseSectorTag, normaliseStageTag, METHOD_TAGS } from './tags.ts';

export interface ValidationResult {
  ok: boolean;
  errors: string[];
  /** Problems that do not fail the build but must be read (e.g. a context fact whose text names the conclusion). */
  warnings: string[];
}

const result = (errors: string[], warnings: string[] = []): ValidationResult => ({ ok: errors.length === 0, errors, warnings });

// ---- fact vocabularies ------------------------------------------------------------------------------------

const POSTURE_FACTS = [
  'need', 'prize', 'market', 'cp', 'home_share', 'import_dependence', 'import_hhi', 'china_bloc', 'trusted_alternatives', 'top_supplier', 'top_supplier_share',
  'china_share', 'opposite_bloc_share', 'trusted_share', 'coercive_top', 'coercive_top_share', 'imports_to_gdp', 'benchmark_share', 'benchmark_rca',
  'benchmark_exports', 'outlook', 'prize_note', 'pathway', 'deployment_need', 'large_yard_signals', 'partner', 'policy_alignment', 'confidence', 'borderline',
  'reason', 'choice', 'fence', 'posture',
  'coercive_import_share', 'export_trend', 'demand_outlook', 'maturity', 'deployment_trend', 'deployment_demand_note', 'deployment_target_note', 'deployment_system_note',
] as const;

/** The snapshot fact ids each case kind may name (shared vocabulary). `sector` cases reuse the posture ids. */
export const FACT_IDS_BY_KIND: Readonly<Record<CaseKind, readonly string[]>> = {
  posture_cell: POSTURE_FACTS,
  aib_node: [
    'class', 'reason', 'exposure', 'lost_usd', 'demand_usd', 'baseline_hhi', 'baseline_largest', 'baseline_largest_share', 'baseline_top3_share', 'static', 'existing',
    'optimized', 'dividend', 'residual', 'residual_usd', 'after_hhi', 'after_largest', 'after_largest_share', 'n1_reoptimized', 'diversion', 'physical_warning',
    'top_suppliers', 'hosts',
  ],
  compact: [
    'decision', 'purpose', 'core', 'steps', 'coverage_static', 'coverage_existing', 'coverage_coordinated', 'coverage_post_investment', 'lost_usd', 'exposure',
    'class_counts', 'missions', 'non_allied_relied_on', 'critical_external', 'build_hosts',
  ],
  partnership: ['pair', 'choices_summary', 'memo_verified', 'memo_as_of', 'memo_generated_at', 'headline'],
  sector: POSTURE_FACTS,
  sector_chain: ['steps', 'demand_usd', 'lost_usd', 'exposure', 'coverage_existing', 'node_classes', 'explorer_stages', 'lead_stage', 'missions', 'chokepoint'],
};

/** Fact ids that give the conclusion away: never allowed in context_fields. */
export const REVEALING_FACT_IDS: readonly string[] = [
  'choice', 'fence', 'posture', 'reason', 'confidence', 'borderline', 'partner', 'policy_alignment', 'class', 'decision', 'missions', 'choices_summary', 'headline',
  'lead_stage', 'explorer_stages', 'node_classes',
];

/**
 * The operational compact types of main's AIB bundle (aib/contract.ts AIB_COMPACT_TYPES: the data's `type`
 * strings of an AIB-Cxx compact group; aib/derive/types.ts compactTypeInfo labels them). Data keys, not labels.
 */
export const COMPACT_DECISION_KEYS: readonly string[] = AIB_COMPACT_TYPES;

/** The display label of an operational compact decision key (compactTypeInfo's short tag). */
export const compactDecisionLabel = (key: string): string => compactTypeInfo(key).short;

/**
 * The canonical conclusion keys of a case kind: every conclusion the Explorer can reach, its model states
 * (not_assessed, INSUFFICIENT DATA) included; no reviewer state and no DONT_KNOW. For a compact the key decides
 * the set: a strategic compact (SC-*) has sectoral / narrower / none, an operational one (AIB-Cxx) the bundle's
 * types. Reviewers are offered these minus the model states, plus the three reviewer states (qc/answerOptions.ts).
 */
export function canonicalAnswerKeys(kind: CaseKind, key: CaseKey = {}): string[] {
  return conclusionOptions(kind, key).map((o) => o.value);
}

/** The canonical keys a reviewer must be offered (the conclusions minus the model states). */
export const reviewerConclusionKeys = (kind: CaseKind, key: CaseKey = {}): string[] => canonicalAnswerKeys(kind, key).filter((v) => !MODEL_STATE_VALUES.includes(v));

/** The canonical keys of a framework scenario's option_set (null for custom or absent). */
export function optionSetKeys(set: unknown): string[] | null {
  switch (set) {
    case 'posture_choice':
      return canonicalAnswerKeys('posture_cell');
    case 'aib_class':
      return canonicalAnswerKeys('aib_node');
    case 'compact_decision':
      // both compact families: the strategic conclusions and the operational types
      return [...STRATEGIC_COMPACT_DECISIONS, ...COMPACT_DECISION_KEYS];
    default:
      return null;
  }
}

/** The key fields a case (or golden case) of each kind must carry. */
export const REQUIRED_KEY_FIELDS: Readonly<Record<CaseKind, readonly string[]>> = {
  posture_cell: ['iso3', 'tech', 'stage'],
  aib_node: ['node', 'scenario'],
  compact: ['sid', 'scenario'],
  partnership: ['pair'],
  sector: ['tech'],
  sector_chain: ['sid', 'scenario'],
};

const SCENARIOS = ['decoupling', 'managed', 'baseline'];

// ---- small checkers -----------------------------------------------------------------------------------------

const isObj = (x: unknown): x is Record<string, unknown> => typeof x === 'object' && x !== null && !Array.isArray(x);
const isStr = (x: unknown): x is string => typeof x === 'string' && x.trim().length > 0;
const isStrArr = (x: unknown): x is string[] => Array.isArray(x) && x.every((v) => typeof v === 'string');
const isInt = (x: unknown): x is number => typeof x === 'number' && Number.isInteger(x);
const isDateLike = (x: unknown): boolean => x === null || x === undefined || x instanceof Date || (typeof x === 'string' && /^\d{4}-\d{2}-\d{2}/.test(x));

export const CASE_ID_RE = /^CASE_\d{3,}$/;
export const GOLDEN_ID_RE = /^GC_\d{3,}$/;
export const CARD_ID_RE = /^MC_\d{3,}$/;
export const SCENARIO_ID_RE = /^FS_\d{3,}$/;
const RELEASE_RE = /^rel-[0-9a-f]{12}$/;
const SHA_RE = /^[0-9a-f]{64}$/;

function dupes(ids: string[]): string[] {
  const seen = new Set<string>();
  const out = new Set<string>();
  for (const i of ids) (seen.has(i) ? out : seen).add(i);
  return [...out];
}

function checkKey(kind: CaseKind, key: unknown, at: string, e: string[]): void {
  if (!isObj(key)) {
    e.push(`${at}.key: not an object`);
    return;
  }
  for (const f of REQUIRED_KEY_FIELDS[kind]) if (!isStr(key[f])) e.push(`${at}.key.${f}: required for kind ${kind}`);
  if (key.iso3 !== undefined && !normaliseCountryTag(key.iso3)) e.push(`${at}.key.iso3: not an ISO3 code (${String(key.iso3)})`);
  if (key.stage !== undefined && !normaliseStageTag(key.stage)) e.push(`${at}.key.stage: not a supply-chain stage (${String(key.stage)})`);
  if (key.scenario !== undefined && !SCENARIOS.includes(String(key.scenario))) e.push(`${at}.key.scenario: one of ${SCENARIOS.join(', ')}`);
}

// ---- campaigns ----------------------------------------------------------------------------------------------

/** A campaign (config/review/campaigns/<id>.yml). With `caseIds`, every case_id must exist in the catalogue. */
export function validateCampaign(obj: unknown, caseIds?: readonly string[]): ValidationResult {
  const e: string[] = [];
  const w: string[] = [];
  if (!isObj(obj)) return result(['campaign: not an object']);
  const c = obj;
  if (!isStr(c.campaign_id) || !/^[a-z0-9_-]+$/.test(c.campaign_id)) e.push('campaign_id: a lower-case slug is required');
  if (!isStr(c.name)) e.push('name: required');
  if (!['draft', 'active', 'closed'].includes(String(c.status))) e.push('status: draft, active or closed');
  if (!isDateLike(c.opens)) e.push('opens: a date (YYYY-MM-DD) or null');
  if (!isDateLike(c.closes)) e.push('closes: a date (YYYY-MM-DD) or null');
  if (!Array.isArray(c.review_types) || c.review_types.length === 0) e.push('review_types: a non-empty list');
  else for (const t of c.review_types) if (!(REVIEW_TYPES as readonly unknown[]).includes(t)) e.push(`review_types: unknown type ${String(t)}`);
  const cases = isStrArr(c.case_ids) ? c.case_ids : null;
  if (!cases) e.push('case_ids: a list of case IDs');
  else {
    for (const id of cases) if (!CASE_ID_RE.test(id)) e.push(`case_ids: ${id} is not a CASE_### id`);
    for (const d of dupes(cases)) e.push(`case_ids: ${d} listed twice`);
    if (caseIds) for (const id of cases) if (!caseIds.includes(id)) e.push(`case_ids: ${id} is not in the case catalogue`);
  }
  if (!isStrArr(c.anchor_cases)) e.push('anchor_cases: a list of case IDs');
  else {
    if (c.anchor_cases.length === 0) w.push('anchor_cases: empty (no expert-vs-expert agreement can be measured)');
    for (const id of c.anchor_cases) if (cases && !cases.includes(id)) e.push(`anchor_cases: ${id} is not in case_ids`);
    for (const d of dupes(c.anchor_cases)) e.push(`anchor_cases: ${d} listed twice`);
  }
  if (!isInt(c.target_reviews_per_anchor) || c.target_reviews_per_anchor < 1) e.push('target_reviews_per_anchor: an integer >= 1');
  else if (c.target_reviews_per_anchor < 3) w.push('target_reviews_per_anchor: below 3, anchor agreement will read "too few reviews"');
  const a = c.assignment;
  if (!isObj(a)) e.push('assignment: required');
  else {
    const per = a.assigned_per_reviewer;
    if (!isObj(per) || !isInt(per.min) || !isInt(per.max) || per.min < 0 || per.max < 1 || per.min > per.max) e.push('assignment.assigned_per_reviewer: {min, max} integers with 0 <= min <= max, max >= 1');
    if (!isInt(a.anchors_per_reviewer) || a.anchors_per_reviewer < 0) e.push('assignment.anchors_per_reviewer: an integer >= 0');
    else if (isObj(per) && isInt(per.max) && a.anchors_per_reviewer > per.max) e.push('assignment.anchors_per_reviewer: more than assigned_per_reviewer.max');
    if (!['none', 'optional', 'one'].includes(String(a.out_of_specialty))) e.push('assignment.out_of_specialty: none, optional or one');
    if (typeof a.allow_self_selected !== 'boolean') e.push('assignment.allow_self_selected: true or false');
    if (typeof a.min_domain_confidence !== 'number' || a.min_domain_confidence < 1 || a.min_domain_confidence > 5) e.push('assignment.min_domain_confidence: 1-5');
  }
  if (c.method_card_ids !== undefined && c.method_card_ids !== null) {
    if (!isStrArr(c.method_card_ids)) e.push('method_card_ids: a list');
    else for (const id of c.method_card_ids) if (!CARD_ID_RE.test(id)) e.push(`method_card_ids: ${id} is not an MC_### id`);
  }
  if (c.framework_scenario_ids !== undefined && c.framework_scenario_ids !== null) {
    if (!isStrArr(c.framework_scenario_ids)) e.push('framework_scenario_ids: a list');
    else for (const id of c.framework_scenario_ids) if (!SCENARIO_ID_RE.test(id)) e.push(`framework_scenario_ids: ${id} is not an FS_### id`);
  }
  if (c.reviewers !== undefined && c.reviewers !== null) {
    if (!Array.isArray(c.reviewers)) e.push('reviewers: a list');
    else
      c.reviewers.forEach((r, i) => {
        if (!isObj(r) || !isStr(r.code) || !/^R-[A-Z0-9]+$/.test(r.code)) e.push(`reviewers[${i}].code: R-<code> required`);
        if (isObj(r) && ('name' in r || 'email' in r || 'organization' in r)) e.push(`reviewers[${i}]: codes only, never names, emails or organizations`);
      });
  }
  return result(e, w);
}

// ---- case catalogue -----------------------------------------------------------------------------------------

function checkCaseDefinition(x: unknown, i: number, e: string[], w: string[]): string | null {
  const at = `cases[${i}]`;
  if (!isObj(x)) {
    e.push(`${at}: not an object`);
    return null;
  }
  const id = isStr(x.case_id) ? x.case_id : null;
  const where = id ? `${at} ${id}` : at;
  if (!id || !CASE_ID_RE.test(id)) e.push(`${where}.case_id: a CASE_### id is required`);
  const kind = (CASE_KINDS as readonly unknown[]).includes(x.kind) ? (x.kind as CaseKind) : null;
  if (!kind) e.push(`${where}.kind: one of ${CASE_KINDS.join(', ')}`);
  else checkKey(kind, x.key, where, e);
  for (const f of ['title', 'question', 'assignment_group', 'why_selected'] as const) if (!isStr(x[f])) e.push(`${where}.${f}: required`);
  if (!isStr(x.explorer_path) || !x.explorer_path.startsWith('/')) e.push(`${where}.explorer_path: an app path starting with /`);
  if (typeof x.anchor !== 'boolean') e.push(`${where}.anchor: true or false`);
  if (!isStrArr(x.sector_groups)) e.push(`${where}.sector_groups: a list of sector tags`);
  else for (const s of x.sector_groups) if (normaliseSectorTag(s) !== s) e.push(`${where}.sector_groups: ${s} is not a canonical sector tag`);
  if (!isStrArr(x.country_groups)) e.push(`${where}.country_groups: a list of ISO3 codes`);
  else for (const s of x.country_groups) if (normaliseCountryTag(s) !== s) e.push(`${where}.country_groups: ${s} is not an upper-case ISO3 code`);
  if (x.stage !== undefined && x.stage !== null && normaliseStageTag(x.stage) !== x.stage) e.push(`${where}.stage: ${String(x.stage)} is not a canonical stage tag`);
  if (x.decision_use !== undefined && typeof x.decision_use !== 'boolean') e.push(`${where}.decision_use: true or false`);
  if (x.threat !== undefined && typeof x.threat !== 'boolean') e.push(`${where}.threat: true or false`);
  const known = kind ? FACT_IDS_BY_KIND[kind] : [];
  if (!isStrArr(x.context_fields)) e.push(`${where}.context_fields: a list of fact ids`);
  else {
    if (x.context_fields.length === 0) w.push(`${where}.context_fields: empty (the reviewer answers blind with no facts)`);
    for (const f of x.context_fields) {
      if (REVEALING_FACT_IDS.includes(f)) e.push(`${where}.context_fields: ${f} reveals the conclusion and may only be a reveal field`);
      else if (kind && !known.includes(f)) e.push(`${where}.context_fields: ${f} is not a fact id of kind ${kind}`);
    }
    for (const d of dupes(x.context_fields)) e.push(`${where}.context_fields: ${d} listed twice`);
  }
  if (!isStrArr(x.reveal_fields)) e.push(`${where}.reveal_fields: a list of fact ids`);
  else for (const f of x.reveal_fields) if (kind && !known.includes(f)) e.push(`${where}.reveal_fields: ${f} is not a fact id of kind ${kind}`);
  if (kind === 'posture_cell' && isObj(x.key) && isStrArr(x.country_groups) && isStr(x.key.iso3) && !x.country_groups.includes(x.key.iso3))
    w.push(`${where}: key.iso3 ${x.key.iso3} is not in country_groups (assignment will not match country experts)`);
  if (kind === 'sector_chain' && isObj(x.key) && isStr(x.key.sid) && !/^SC-/i.test(x.key.sid)) e.push(`${where}.key.sid: a sector chain reads a strategic compact (SC-*)`);
  checkBriefAuthoring(x, kind, where, e, w);
  return id;
}

/**
 * The authored brief (UX v2, CaseDefinition.brief): decision_question and decision_context required, objective
 * and time_horizon optional strings, evidence_ids naming brief facts of the kind (qc/brief.ts
 * BRIEF_FACT_IDS_BY_KIND), assumption_ids naming qc/brief.ts ASSUMPTIONS. The authored text, the title and the
 * question are linted for leaks (qc/briefLint.ts): no canonical conclusion label, model score or model label.
 */
function checkBriefAuthoring(x: Record<string, unknown>, kind: CaseKind | null, where: string, e: string[], w: string[]): void {
  const texts: [string, string][] = [];
  if (isStr(x.title)) texts.push([`${where}.title`, x.title]);
  if (isStr(x.question)) texts.push([`${where}.question`, x.question]);
  const b = x.brief;
  if (b === undefined || b === null) {
    w.push(`${where}.brief: no authored brief (the builder's defaults are used)`);
  } else if (!isObj(b)) {
    e.push(`${where}.brief: an object`);
  } else {
    for (const f of ['decision_question', 'decision_context'] as const) if (!isStr(b[f])) e.push(`${where}.brief.${f}: required`);
    for (const f of ['objective', 'time_horizon'] as const) if (b[f] !== undefined && b[f] !== null && !isStr(b[f])) e.push(`${where}.brief.${f}: a string or null`);
    for (const f of ['decision_question', 'decision_context', 'objective', 'time_horizon'] as const) if (isStr(b[f])) texts.push([`${where}.brief.${f}`, b[f] as string]);
    if (b.evidence_ids !== undefined) {
      if (!isStrArr(b.evidence_ids)) e.push(`${where}.brief.evidence_ids: a list of brief fact ids`);
      else {
        const known = kind ? BRIEF_FACT_IDS_BY_KIND[kind] : [];
        for (const f of b.evidence_ids) if (kind && !known.includes(f)) e.push(`${where}.brief.evidence_ids: ${f} is not a brief fact of kind ${kind}`);
        for (const d of dupes(b.evidence_ids)) e.push(`${where}.brief.evidence_ids: ${d} listed twice`);
        if (b.evidence_ids.length && (b.evidence_ids.length < 5 || b.evidence_ids.length > 8)) w.push(`${where}.brief.evidence_ids: ${b.evidence_ids.length} facts (the brief aims for 5 to 8)`);
      }
    }
    if (b.assumption_ids !== undefined) {
      if (!isStrArr(b.assumption_ids)) e.push(`${where}.brief.assumption_ids: a list of assumption ids`);
      else for (const a of b.assumption_ids) if (!ASSUMPTIONS[a]) e.push(`${where}.brief.assumption_ids: ${a} is not an assumption id (qc/brief.ts ASSUMPTIONS)`);
    }
  }
  for (const err of lintAuthoredText(texts)) e.push(`leak: ${err}`);
}

/** The case catalogue (config/review/review_cases.yml): an array of CaseDefinition with unique, stable CASE_### IDs. */
export function validateCaseCatalogue(obj: unknown): ValidationResult {
  const e: string[] = [];
  const w: string[] = [];
  if (!Array.isArray(obj)) return result(['case catalogue: not a list']);
  const ids: string[] = [];
  obj.forEach((x, i) => {
    const id = checkCaseDefinition(x, i, e, w);
    if (id) ids.push(id);
  });
  for (const d of dupes(ids)) e.push(`case_id ${d}: used more than once (case IDs are stable and never reused)`);
  return result(e, w);
}

const wordRe = (label: string) => new RegExp(`(^|[^A-Za-z])${label.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}([^A-Za-z]|$)`, 'i');

/** A built cases file (public/data/review/cases.<campaign>.json). */
export function validateCasesFile(obj: unknown): ValidationResult {
  const e: string[] = [];
  const w: string[] = [];
  if (!isObj(obj)) return result(['cases file: not an object']);
  if (obj.schema !== CASES_SCHEMA) e.push(`schema: expected ${CASES_SCHEMA}`);
  if (obj.schema_version !== 1) e.push('schema_version: expected 1');
  if (!isStr(obj.campaign_id)) e.push('campaign_id: required');
  if (!isStr(obj.data_release) || !RELEASE_RE.test(obj.data_release)) e.push('data_release: rel-<12 hex> required');
  if (!isStr(obj.built_at)) e.push('built_at: required');
  if (typeof obj.app_commit !== 'string') e.push('app_commit: required (unknown allowed)');
  if (!Array.isArray(obj.cases)) return result([...e, 'cases: a list'], w);
  const cat = validateCaseCatalogue(obj.cases);
  e.push(...cat.errors);
  w.push(...cat.warnings);
  obj.cases.forEach((x, i) => {
    if (!isObj(x)) return;
    const where = `cases[${i}] ${String(x.case_id)}`;
    if (!isStr(x.snapshot_sha256) || !SHA_RE.test(x.snapshot_sha256)) e.push(`${where}.snapshot_sha256: 64 hex required`);
    if (!isStr(x.data_release)) e.push(`${where}.data_release: required`);
    else if (isStr(obj.data_release) && x.data_release !== obj.data_release) w.push(`${where}.data_release: ${x.data_release} differs from the file's ${obj.data_release}`);
    if (!Array.isArray(x.bundle) || x.bundle.length === 0) e.push(`${where}.bundle: the stamps of the files read are required`);
    else
      x.bundle.forEach((b, j) => {
        if (!isObj(b) || !isStr(b.file) || !isStr(b.sha256) || !SHA_RE.test(b.sha256)) e.push(`${where}.bundle[${j}]: {file, sha256, created, git_commit} required`);
        else if (b.git_commit === null) w.push(`${where}.bundle[${j}] ${b.file}: no git_commit (provenance gap)`);
      });
    const snap = x.snapshot;
    if (!isObj(snap) || !isObj(snap.conclusion) || typeof snap.conclusion.value !== 'string' || !isObj(snap.facts)) {
      e.push(`${where}.snapshot: {conclusion {value, label}, facts} required`);
      return;
    }
    const facts = snap.facts;
    for (const f of [...(isStrArr(x.context_fields) ? x.context_fields : []), ...(isStrArr(x.reveal_fields) ? x.reveal_fields : [])])
      if (!isObj(facts[f])) e.push(`${where}.snapshot.facts: ${f} is named but missing (write it with value null and say why)`);
    const opts = x.answer_options;
    if (!Array.isArray(opts) || opts.length === 0) {
      e.push(`${where}.answer_options: a non-empty list`);
      return;
    }
    const values = opts.map((o) => (isObj(o) ? o.value : undefined));
    if (opts.some((o) => !isObj(o) || !isStr(o.value) || !isStr(o.label))) e.push(`${where}.answer_options: every option needs {value, label}`);
    for (const d of dupes(values.filter(isStr))) e.push(`${where}.answer_options: ${d} listed twice`);
    const kind = x.kind as CaseKind;
    const ck = isObj(x.key) ? (x.key as CaseKey) : {};
    const canon = (CASE_KINDS as readonly string[]).includes(kind) ? canonicalAnswerKeys(kind, ck) : null;
    const v2 = isObj(snap.brief);
    if (canon) {
      for (const k of reviewerConclusionKeys(kind, ck)) if (!values.includes(k)) e.push(`${where}.answer_options: the canonical option ${k} is missing (blind options must exclude nothing canonical)`);
      for (const v of values) if (isStr(v) && v !== DONT_KNOW && !REVIEWER_STATES.includes(v) && !canon.includes(v)) e.push(`${where}.answer_options: ${v} is not a canonical ${kind} key`);
      if (v2) {
        for (const r of REVIEWER_STATES) if (!values.includes(r)) e.push(`${where}.answer_options: the reviewer state ${r} is missing`);
        for (const v of values) if (isStr(v) && MODEL_STATE_VALUES.includes(v)) e.push(`${where}.answer_options: ${v} is a model state, not a reviewer option`);
      }
    }
    const concl = snap.conclusion.value;
    if (!values.includes(concl) && !(canon && canon.includes(concl))) e.push(`${where}.snapshot.conclusion.value: ${concl} is not one of the answer options (nor a conclusion of the kind)`);
    // UX v2: the brief is neutral (the build fails on a leak) and the reasoning is present
    if (v2) {
      for (const err of lintBriefNeutrality(snap.brief as unknown as CaseBrief, snap.conclusion as unknown as CaseConclusion, opts.filter(isObj) as unknown as AnswerOption[])) e.push(`${where}.snapshot.brief: ${err}`);
      if (!isObj(snap.reasoning) && concl !== 'missing') w.push(`${where}.snapshot.reasoning: no reasoning reveal`);
    }
    // a context fact that names the conclusion leaks it, whatever its id
    const label = isStr(snap.conclusion.label) ? snap.conclusion.label : null;
    if (label && isStrArr(x.context_fields))
      for (const f of x.context_fields) {
        const fact = facts[f];
        if (isObj(fact) && typeof fact.display === 'string' && wordRe(label).test(fact.display)) w.push(`${where}: context fact ${f} displays "${fact.display}", which names the conclusion "${label}"`);
      }
  });
  return result(e, w);
}

// ---- method cards and framework scenarios ----------------------------------------------------------------

const CARD_AREAS = ['need', 'prize', 'position', 'choice_rule', 'secure', 'deployment', 'aib_stress_test', 'aib_compact', 'aib_partners', 'data'];

/** Method cards (config/review/method_cards.yml or the built method_cards.json): a list, or {cards: [...]}. */
export function validateMethodCards(obj: unknown): ValidationResult {
  const e: string[] = [];
  const list = isObj(obj) && Array.isArray(obj.cards) ? obj.cards : obj;
  if (!Array.isArray(list)) return result(['method cards: not a list']);
  const ids: string[] = [];
  list.forEach((x, i) => {
    if (!isObj(x)) return void e.push(`cards[${i}]: not an object`);
    const where = `cards[${i}] ${String(x.card_id)}`;
    if (!isStr(x.card_id) || !CARD_ID_RE.test(x.card_id)) e.push(`${where}.card_id: an MC_### id is required`);
    else ids.push(x.card_id);
    for (const f of ['title', 'current_choice', 'why'] as const) if (!isStr(x[f])) e.push(`${where}.${f}: required`);
    if (!CARD_AREAS.includes(String(x.area))) e.push(`${where}.area: one of ${CARD_AREAS.join(', ')}`);
    if (!isObj(x.example) || !isStr(x.example.text)) e.push(`${where}.example.text: required`);
    if (!isStrArr(x.alternatives) || x.alternatives.length === 0) e.push(`${where}.alternatives: at least one`);
    if (!isStrArr(x.affected_outputs)) e.push(`${where}.affected_outputs: a list`);
    if (!isStrArr(x.code_refs)) e.push(`${where}.code_refs: a list of file:line references`);
    if (!isStrArr(x.method_tags) || x.method_tags.length === 0) e.push(`${where}.method_tags: at least one method tag`);
    else for (const t of x.method_tags) if (!(METHOD_TAGS as readonly string[]).includes(t)) e.push(`${where}.method_tags: ${t} is not a method tag`);
  });
  for (const d of dupes(ids)) e.push(`card_id ${d}: used more than once`);
  return result(e);
}

/** Framework scenarios: a list, or {scenarios: [...]}; intended_answer must be one of the options. */
export function validateFrameworkScenarios(obj: unknown): ValidationResult {
  const e: string[] = [];
  const list = isObj(obj) && Array.isArray(obj.scenarios) ? obj.scenarios : obj;
  if (!Array.isArray(list)) return result(['framework scenarios: not a list']);
  const ids: string[] = [];
  list.forEach((x, i) => {
    if (!isObj(x)) return void e.push(`scenarios[${i}]: not an object`);
    const where = `scenarios[${i}] ${String(x.scenario_id)}`;
    if (!isStr(x.scenario_id) || !SCENARIO_ID_RE.test(x.scenario_id)) e.push(`${where}.scenario_id: an FS_### id is required`);
    else ids.push(x.scenario_id);
    for (const f of ['concept', 'prompt', 'explanation'] as const) if (!isStr(x[f])) e.push(`${where}.${f}: required`);
    if (x.option_set !== undefined && !['posture_choice', 'aib_class', 'compact_decision', 'custom'].includes(String(x.option_set))) e.push(`${where}.option_set: posture_choice, aib_class, compact_decision or custom`);
    const opts = Array.isArray(x.options) ? x.options : null;
    // a canonical option_set with no options listed is filled by the builder from the vocabulary constants
    const fromSet = optionSetKeys(x.option_set);
    const filledByBuilder = fromSet !== null && (opts === null || opts.length === 0);
    if (!filledByBuilder && (!opts || opts.length < 2)) e.push(`${where}.options: at least two (or a canonical option_set)`);
    const values = filledByBuilder ? fromSet : (opts ?? []).map((o) => (isObj(o) ? o.value : undefined));
    if (fromSet && !filledByBuilder)
      for (const v of values) if (isStr(v) && !fromSet.includes(v)) e.push(`${where}.options: ${v} is not a key of option_set ${String(x.option_set)}`);
    if (opts && opts.some((o) => !isObj(o) || !isStr(o.value) || !isStr(o.label))) e.push(`${where}.options: every option needs {value, label}`);
    for (const d of dupes(values.filter(isStr))) e.push(`${where}.options: ${d} listed twice`);
    if (!isStr(x.intended_answer) || !values.includes(x.intended_answer)) e.push(`${where}.intended_answer: must be one of the options`);
    if (!isStrArr(x.terms)) e.push(`${where}.terms: a list`);
  });
  for (const d of dupes(ids)) e.push(`scenario_id ${d}: used more than once`);
  return result(e);
}

// ---- golden cases -------------------------------------------------------------------------------------------

const GOLDEN_CATEGORIES = ['obvious_secure', 'keep_open', 'strong_producer', 'import_dependent', 'entrepot', 'sparse', 'near_threshold', 'neis_central', 'surprising', 'method_sensitive'];

/** Golden cases (config/qc/golden_cases.yml): a list, or {cases: [...]}. */
export function validateGoldenCases(obj: unknown): ValidationResult {
  const e: string[] = [];
  const list = isObj(obj) && Array.isArray(obj.cases) ? obj.cases : obj;
  if (!Array.isArray(list)) return result(['golden cases: not a list']);
  const ids: string[] = [];
  list.forEach((x, i) => {
    if (!isObj(x)) return void e.push(`golden[${i}]: not an object`);
    const where = `golden[${i}] ${String(x.id)}`;
    if (!isStr(x.id) || !GOLDEN_ID_RE.test(x.id)) e.push(`${where}.id: a GC_### id is required`);
    else ids.push(x.id);
    if (!GOLDEN_CATEGORIES.includes(String(x.category))) e.push(`${where}.category: one of ${GOLDEN_CATEGORIES.join(', ')}`);
    const kind = ['posture_cell', 'aib_node', 'compact'].includes(String(x.kind)) ? (x.kind as CaseKind) : null;
    if (!kind) e.push(`${where}.kind: posture_cell, aib_node or compact`);
    else checkKey(kind, x.key, where, e);
    const canon = kind ? canonicalAnswerKeys(kind, isObj(x.key) ? (x.key as CaseKey) : {}) : [];
    const ex = x.expect;
    if (!isObj(ex)) e.push(`${where}.expect: required`);
    else {
      if (ex.choice === undefined && ex.choice_in === undefined && ex.fence === undefined && ex.numeric === undefined) e.push(`${where}.expect: at least one of choice, choice_in, fence, numeric`);
      if (ex.choice !== undefined && !canon.includes(String(ex.choice))) e.push(`${where}.expect.choice: ${String(ex.choice)} is not a canonical ${String(kind)} key`);
      if (ex.choice_in !== undefined) {
        if (!isStrArr(ex.choice_in) || ex.choice_in.length === 0) e.push(`${where}.expect.choice_in: a non-empty list`);
        else for (const c of ex.choice_in) if (!canon.includes(c)) e.push(`${where}.expect.choice_in: ${c} is not a canonical ${String(kind)} key`);
      }
      if (ex.fence !== undefined && !['high', 'medium', 'low'].includes(String(ex.fence))) e.push(`${where}.expect.fence: high, medium or low`);
      if (ex.numeric !== undefined) {
        if (!isObj(ex.numeric)) e.push(`${where}.expect.numeric: {field: [lo, hi]}`);
        else
          for (const [k, r] of Object.entries(ex.numeric))
            if (!Array.isArray(r) || r.length !== 2 || !r.every((v) => typeof v === 'number' && Number.isFinite(v)) || (r[0] as number) > (r[1] as number))
              e.push(`${where}.expect.numeric.${k}: [lo, hi] with lo <= hi`);
      }
    }
    if (typeof x.exact !== 'boolean') e.push(`${where}.exact: true or false`);
    if (typeof x.change_requires_review !== 'boolean') e.push(`${where}.change_requires_review: true or false`);
    if (!isStr(x.rationale)) e.push(`${where}.rationale: required`);
    if (!isObj(x.approved) || !isStr(x.approved.by) || !isDateLike(x.approved.on) || x.approved.on === null || !(x.approved.release === null || typeof x.approved.release === 'string'))
      e.push(`${where}.approved: {by, on, release} required`);
  });
  for (const d of dupes(ids)) e.push(`golden id ${d}: used more than once`);
  return result(e);
}

// ---- submissions --------------------------------------------------------------------------------------------

export const SUBMISSION_KINDS: readonly SubmissionKind[] = ['case_review', 'method_feedback', 'framework_answer', 'decision_use', 'threat_review', 'data_issue', 'break_model'];
const SEVERITIES = ['cosmetic', 'minor', 'material', 'critical'];
const ISSUE_TYPES = ['data', 'method', 'context', 'framework', 'actionability', 'insufficient_evidence', 'decision_rule'];

/** One reviewer submission envelope and the minimum its payload kind needs (the export CLI skips failures). */
export function validateSubmission(obj: unknown): ValidationResult {
  const e: string[] = [];
  if (!isObj(obj)) return result(['submission: not an object']);
  const at = `submission ${String(obj.submission_id)}`;
  if (obj.schema !== SUBMISSION_SCHEMA) e.push(`${at}.schema: expected ${SUBMISSION_SCHEMA}`);
  if (obj.schema_version !== SUBMISSION_SCHEMA_VERSION) e.push(`${at}.schema_version: expected ${SUBMISSION_SCHEMA_VERSION}`);
  if (!isStr(obj.submission_id)) e.push(`${at}.submission_id: required`);
  const kind = (SUBMISSION_KINDS as readonly unknown[]).includes(obj.kind) ? (obj.kind as SubmissionKind) : null;
  if (!kind) e.push(`${at}.kind: one of ${SUBMISSION_KINDS.join(', ')}`);
  if (!isObj(obj.reviewer) || !isStr(obj.reviewer.reviewer_id) || typeof obj.reviewer.anonymous !== 'boolean') e.push(`${at}.reviewer: {reviewer_id, anonymous, profile_digest} required`);
  if (!isStr(obj.created_at)) e.push(`${at}.created_at: required`);
  if (typeof obj.app_commit !== 'string') e.push(`${at}.app_commit: required`);
  const p = obj.payload;
  if (!isObj(p)) return result([...e, `${at}.payload: required`]);
  switch (kind) {
    case 'case_review': {
      if (!isStr(p.case_id)) e.push(`${at}.payload.case_id: required`);
      const b = p.blind;
      const r = p.reveal;
      if (!isObj(b) || !isStr(b.choice) || !isStr(b.answered_at)) e.push(`${at}.payload.blind: {choice, answered_at} required`);
      if (!isObj(r) || !isInt(r.verdict) || r.verdict < 1 || r.verdict > 7 || !isStr(r.revealed_at)) e.push(`${at}.payload.reveal: {verdict 1-7, revealed_at} required`);
      else {
        if (isObj(b) && isStr(b.answered_at) && r.revealed_at < b.answered_at) e.push(`${at}.payload: revealed before the blind answer was stored`);
        if ([3, 4, 5, 6].includes(r.verdict as number)) {
          const mi = r.main_issue;
          if (!isObj(mi) || !isStr(mi.text) || !ISSUE_TYPES.includes(String(mi.type))) e.push(`${at}.payload.reveal.main_issue: {text, type} required for verdict ${String(r.verdict)}`);
        }
      }
      break;
    }
    case 'method_feedback':
      if (!isStr(p.card_id)) e.push(`${at}.payload.card_id: required`);
      if (!['retain', 'modify', 'reject', 'unsure'].includes(String(p.stance))) e.push(`${at}.payload.stance: retain, modify, reject or unsure`);
      break;
    case 'framework_answer':
      if (!isStr(p.scenario_id) || !isStr(p.answer) || !isStr(p.intended_answer)) e.push(`${at}.payload: {scenario_id, answer, intended_answer} required`);
      break;
    case 'decision_use':
    case 'threat_review':
      if (!isStr(p.case_id)) e.push(`${at}.payload.case_id: required`);
      break;
    case 'data_issue':
    case 'break_model':
      if (!isStr(p.what_is_wrong)) e.push(`${at}.payload.what_is_wrong: required`);
      if (!SEVERITIES.includes(String(p.severity))) e.push(`${at}.payload.severity: ${SEVERITIES.join(', ')}`);
      if (!isObj(obj.page_context)) e.push(`${at}.page_context: required for ${kind}`);
      if (kind === 'break_model' && !isStr(p.break_type)) e.push(`${at}.payload.break_type: required for break_model`);
      break;
    default:
      break;
  }
  return result(e);
}
