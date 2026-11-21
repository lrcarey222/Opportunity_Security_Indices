// Validators of the review system's authored and built files (review/manifest.ts): campaigns, the case
// catalogue (revealing context rejected, duplicate and malformed IDs rejected, fact ids per kind), built
// cases files (hashes, complete canonical answer options), method cards, framework scenarios, golden cases
// and submission envelopes.

import { describe, expect, it } from 'vitest';
import { CHOICE_NAME, AIB_CLASS_NAME } from '../content/vocabulary.ts';
import { compactTypeInfo } from '../aib/derive/types.ts';
import type { CaseDefinition } from './model.ts';
import {
  COMPACT_DECISION_KEYS,
  FACT_IDS_BY_KIND,
  REVEALING_FACT_IDS,
  canonicalAnswerKeys,
  validateCampaign,
  validateCaseCatalogue,
  validateCasesFile,
  validateFrameworkScenarios,
  validateGoldenCases,
  validateMethodCards,
  validateSubmission,
} from './manifest.ts';

const def = (x: Partial<CaseDefinition> = {}): CaseDefinition => ({
  case_id: 'CASE_001',
  kind: 'posture_cell',
  key: { iso3: 'KOR', tech: 'Batteries', stage: 'Components' },
  title: 'Korea batteries components',
  question: 'What should Korea do?',
  sector_groups: ['batteries'],
  country_groups: ['KOR'],
  stage: 'Components',
  assignment_group: 'batteries',
  anchor: true,
  why_selected: 'NEIS central',
  explorer_path: '/countries/KOR?node=Batteries%7CComponents',
  context_fields: ['import_dependence', 'top_supplier', 'home_share'],
  reveal_fields: ['choice', 'fence', 'reason', 'need', 'prize', 'cp'],
  ...x,
});

const campaign = (x: Record<string, unknown> = {}) => ({
  campaign_id: 'aib_qc_2027_round1',
  name: 'Round 1',
  status: 'draft',
  opens: '2027-01-15',
  closes: '2027-03-01',
  review_types: ['case', 'methodology'],
  case_ids: ['CASE_001', 'CASE_002'],
  anchor_cases: ['CASE_001'],
  target_reviews_per_anchor: 4,
  assignment: { assigned_per_reviewer: { min: 2, max: 4 }, anchors_per_reviewer: 1, out_of_specialty: 'optional', allow_self_selected: true, min_domain_confidence: 3 },
  ...x,
});

describe('shared fact vocabularies', () => {
  it('lists the revealing ids exactly, and every one is a fact id of some kind', () => {
    expect([...REVEALING_FACT_IDS].sort()).toEqual(['borderline', 'choice', 'choices_summary', 'class', 'confidence', 'decision', 'fence', 'headline', 'missions', 'partner', 'policy_alignment', 'posture', 'reason'].sort());
    const all = new Set(Object.values(FACT_IDS_BY_KIND).flat());
    for (const r of REVEALING_FACT_IDS) expect(all.has(r)).toBe(true);
  });
  it('reads canonical answer keys from the Explorer constants', () => {
    expect(canonicalAnswerKeys('posture_cell')).toEqual([...Object.keys(CHOICE_NAME), 'not_assessed']);
    expect(canonicalAnswerKeys('aib_node')).toEqual(Object.keys(AIB_CLASS_NAME));
    expect(canonicalAnswerKeys('partnership')).toEqual(['secure', 'preserve', 'partner', 'accelerate', 'watch']);
    expect(canonicalAnswerKeys('compact', { sid: 'SC-BATTERIES' })).toEqual(['sectoral', 'narrower', 'none']);
    expect(canonicalAnswerKeys('compact', { sid: 'AIB-C01' })).toEqual([...COMPACT_DECISION_KEYS]);
    for (const k of ['posture_cell', 'aib_node', 'compact', 'partnership', 'sector'] as const) expect(canonicalAnswerKeys(k)).not.toContain('dont_know');
    // every operational compact key is one main's compactTypeInfo recognises (a distinct tone each, label != key)
    expect(new Set(COMPACT_DECISION_KEYS.map((k) => compactTypeInfo(k).tone)).size).toBe(3);
    for (const k of COMPACT_DECISION_KEYS) expect(compactTypeInfo(k).short).not.toBe(k);
  });
});

describe('validateCampaign', () => {
  it('accepts a well-formed campaign and checks case ids against the catalogue', () => {
    expect(validateCampaign(campaign())).toMatchObject({ ok: true, errors: [] });
    const r = validateCampaign(campaign(), ['CASE_001']);
    expect(r.ok).toBe(false);
    expect(r.errors.join()).toMatch(/CASE_002 is not in the case catalogue/);
  });
  it('rejects bad statuses, anchors outside scope, impossible assignment rules and names in pre-registration', () => {
    const r = validateCampaign(
      campaign({
        status: 'open',
        anchor_cases: ['CASE_009'],
        assignment: { assigned_per_reviewer: { min: 5, max: 2 }, anchors_per_reviewer: 3, out_of_specialty: 'sometimes', allow_self_selected: 'yes', min_domain_confidence: 9 },
        reviewers: [{ code: 'R-ABC', name: 'Someone' }],
      }),
    );
    expect(r.ok).toBe(false);
    const all = r.errors.join('\n');
    expect(all).toMatch(/status/);
    expect(all).toMatch(/CASE_009 is not in case_ids/);
    expect(all).toMatch(/assigned_per_reviewer/);
    expect(all).toMatch(/out_of_specialty/);
    expect(all).toMatch(/allow_self_selected/);
    expect(all).toMatch(/min_domain_confidence/);
    expect(all).toMatch(/never names/);
  });
  it('rejects a non-object', () => {
    expect(validateCampaign(null).ok).toBe(false);
  });
});

describe('validateCaseCatalogue', () => {
  it('accepts a valid catalogue', () => {
    const r = validateCaseCatalogue([def(), def({ case_id: 'CASE_002', anchor: false })]);
    expect(r.errors).toEqual([]);
    expect(r.ok).toBe(true);
  });
  it('rejects every revealing field in context_fields', () => {
    for (const f of REVEALING_FACT_IDS) {
      const kind = (Object.keys(FACT_IDS_BY_KIND) as (keyof typeof FACT_IDS_BY_KIND)[]).find((k) => FACT_IDS_BY_KIND[k].includes(f))!;
      const key = kind === 'aib_node' ? { node: 'rare-earths', scenario: 'decoupling' as const } : kind === 'compact' ? { sid: 'SC-1', scenario: 'managed' as const } : kind === 'partnership' ? { pair: 'USA_JPN' } : { iso3: 'KOR', tech: 'Batteries', stage: 'Components' };
      const r = validateCaseCatalogue([def({ kind, key, context_fields: ['need', f].filter((x) => FACT_IDS_BY_KIND[kind].includes(x)), reveal_fields: [] })]);
      expect(r.ok, `${f} in ${kind}`).toBe(false);
      expect(r.errors.join()).toMatch(new RegExp(`${f} reveals the conclusion`));
    }
  });
  it('rejects duplicate, malformed and missing IDs', () => {
    const dup = validateCaseCatalogue([def(), def()]);
    expect(dup.ok).toBe(false);
    expect(dup.errors.join()).toMatch(/CASE_001: used more than once/);
    expect(validateCaseCatalogue([def({ case_id: 'case-1' })]).ok).toBe(false);
    expect(validateCaseCatalogue([def({ case_id: 'CASE_01' })]).ok).toBe(false);
    expect(validateCaseCatalogue({ cases: [] }).ok).toBe(false);
  });
  it('rejects unknown kinds, missing key fields, unknown fact ids and non-canonical tags', () => {
    expect(validateCaseCatalogue([def({ kind: 'country' as never })]).errors.join()).toMatch(/kind/);
    expect(validateCaseCatalogue([def({ key: { iso3: 'KOR', tech: 'Batteries' } })]).errors.join()).toMatch(/key.stage: required/);
    expect(validateCaseCatalogue([def({ kind: 'aib_node', key: { node: 'x', scenario: 'decoupling' }, context_fields: ['need'], reveal_fields: ['class'] })]).errors.join()).toMatch(/need is not a fact id of kind aib_node/);
    expect(validateCaseCatalogue([def({ reveal_fields: ['magic'] })]).errors.join()).toMatch(/magic is not a fact id/);
    expect(validateCaseCatalogue([def({ sector_groups: ['Batteries'] })]).errors.join()).toMatch(/canonical sector tag/);
    expect(validateCaseCatalogue([def({ country_groups: ['kor'] })]).errors.join()).toMatch(/ISO3/);
    expect(validateCaseCatalogue([def({ key: { iso3: 'KOR', tech: 'Batteries', stage: 'Midstream' } })]).errors.join()).toMatch(/supply-chain stage/);
  });
});

const OPTIONS = [...Object.entries(CHOICE_NAME).map(([value, label]) => ({ value, label })), { value: 'not_assessed', label: 'Not assessed' }];
const built = (x: Record<string, unknown> = {}, file: Record<string, unknown> = {}) => ({
  schema: 'aib-review-cases',
  schema_version: 1,
  campaign_id: 'aib_qc_2027_round1',
  data_release: 'rel-0123456789ab',
  built_at: '2027-01-10T00:00:00Z',
  app_commit: 'abc1234',
  cases: [
    {
      ...def(),
      snapshot: {
        conclusion: { value: 'partner', label: 'Partner' },
        facts: Object.fromEntries(['import_dependence', 'top_supplier', 'home_share', 'choice', 'fence', 'reason', 'need', 'prize', 'cp'].map((id) => [id, { id, label: id, value: 0.5, display: '50%' }])),
      },
      snapshot_sha256: 'c'.repeat(64),
      bundle: [{ file: 'posture/posture-core.f665dc19.json', sha256: 'd'.repeat(64), created: '2026-11-17T20:33:31Z', git_commit: '0496e7ba' }],
      data_release: 'rel-0123456789ab',
      app_commit: 'abc1234',
      built_at: '2027-01-10T00:00:00Z',
      answer_options: OPTIONS,
      ...x,
    },
  ],
  ...file,
});

describe('validateCasesFile', () => {
  it('accepts a complete built file', () => {
    const r = validateCasesFile(built());
    expect(r.errors).toEqual([]);
  });
  it('requires the snapshot hash and bundle stamps', () => {
    expect(validateCasesFile(built({ snapshot_sha256: 'nothex' })).errors.join()).toMatch(/snapshot_sha256/);
    expect(validateCasesFile(built({ bundle: [] })).errors.join()).toMatch(/bundle/);
    expect(validateCasesFile(built({}, { data_release: 'release-1' })).errors.join()).toMatch(/data_release/);
  });
  it('requires non-empty answer options that exclude nothing canonical', () => {
    expect(validateCasesFile(built({ answer_options: [] })).errors.join()).toMatch(/non-empty/);
    const missing = validateCasesFile(built({ answer_options: OPTIONS.filter((o) => o.value !== 'watch') }));
    expect(missing.ok).toBe(false);
    expect(missing.errors.join()).toMatch(/canonical option watch is missing/);
    const extra = validateCasesFile(built({ answer_options: [...OPTIONS, { value: 'keep_open', label: 'Keep open' }] }));
    expect(extra.errors.join()).toMatch(/keep_open is not a canonical/);
  });
  it('requires every named fact in the snapshot and warns when a context fact names the conclusion', () => {
    const r = validateCasesFile(built({ context_fields: ['import_dependence', 'top_supplier', 'home_share', 'pathway'] }));
    expect(r.errors.join()).toMatch(/pathway is named but missing/);
    const leak = built();
    (leak.cases[0].snapshot.facts as Record<string, { display: string }>).top_supplier.display = 'Partner economies supply most';
    expect(validateCasesFile(leak).warnings.join()).toMatch(/names the conclusion/);
  });
  it('flags a conclusion that is not an answer option', () => {
    const r = validateCasesFile(built({ snapshot: { conclusion: { value: 'keep_open', label: 'Keep open' }, facts: built().cases[0].snapshot.facts } }));
    expect(r.errors.join()).toMatch(/not one of the answer options/);
  });
});

describe('method cards, framework scenarios and golden cases', () => {
  const card = { card_id: 'MC_001', title: 't', area: 'need', current_choice: 'c', why: 'w', example: { text: 'e' }, alternatives: ['a'], affected_outputs: ['o'], code_refs: ['R/x.R:1'], method_tags: ['trade_data'] };
  it('validates method cards', () => {
    expect(validateMethodCards([card]).ok).toBe(true);
    expect(validateMethodCards({ cards: [card] }).ok).toBe(true);
    expect(validateMethodCards([card, card]).errors.join()).toMatch(/used more than once/);
    expect(validateMethodCards([{ ...card, method_tags: ['vibes'] }]).ok).toBe(false);
    expect(validateMethodCards([{ ...card, area: 'other' }]).ok).toBe(false);
  });
  const sc = { scenario_id: 'FS_001', concept: 'fence', prompt: 'p', explanation: 'e', terms: ['fence'], option_set: 'posture_choice', options: [{ value: 'secure', label: 'Secure' }, { value: 'watch', label: 'Watch' }], intended_answer: 'watch' };
  it('requires the intended answer to be one of the options', () => {
    expect(validateFrameworkScenarios([sc]).ok).toBe(true);
    const r = validateFrameworkScenarios([{ ...sc, intended_answer: 'keep_open' }]);
    expect(r.ok).toBe(false);
    expect(r.errors.join()).toMatch(/intended_answer: must be one of the options/);
    expect(validateFrameworkScenarios([{ ...sc, options: [sc.options[0]] }]).ok).toBe(false);
  });
  const gc = { id: 'GC_001', category: 'obvious_secure', kind: 'posture_cell', key: { iso3: 'JPN', tech: 'Electric Vehicles', stage: 'Refining' }, expect: { choice: 'secure', numeric: { need: [0.9, 1] } }, exact: true, change_requires_review: true, rationale: 'r', approved: { by: 'NEIS', on: '2026-11-20', release: null } };
  it('validates golden cases against canonical keys', () => {
    expect(validateGoldenCases([gc]).errors).toEqual([]);
    expect(validateGoldenCases([{ ...gc, expect: { choice: 'keep_open' } }]).errors.join()).toMatch(/not a canonical/);
    expect(validateGoldenCases([{ ...gc, expect: { numeric: { need: [1, 0.5] } } }]).ok).toBe(false);
    expect(validateGoldenCases([gc, gc]).errors.join()).toMatch(/used more than once/);
    // a strategic compact (SC-*) is judged on sectoral / narrower / none, an operational one on the bundle types
    expect(validateGoldenCases([{ ...gc, kind: 'compact', key: { sid: 'SC-BATTERIES', scenario: 'managed' }, expect: { choice: 'sectoral' } }]).ok).toBe(true);
    expect(validateGoldenCases([{ ...gc, kind: 'compact', key: { sid: 'SC-BATTERIES', scenario: 'managed' }, expect: { choice: 'candidate compact' } }]).ok).toBe(false);
    expect(validateGoldenCases([{ ...gc, kind: 'compact', key: { sid: 'AIB-C01', scenario: 'managed' }, expect: { choice: 'candidate compact' } }]).ok).toBe(true);
    expect(validateGoldenCases([{ ...gc, kind: 'compact', key: { sid: 'AIB-C01', scenario: 'managed' }, expect: { choice: 'narrower' } }]).ok).toBe(false);
  });
  it('fills a canonical option_set with no options and checks the intended answer against it', () => {
    const filled = { ...sc, options: [], option_set: 'posture_choice', intended_answer: 'partner' };
    expect(validateFrameworkScenarios([filled]).errors).toEqual([]);
    expect(validateFrameworkScenarios([{ ...filled, intended_answer: 'keep_open' }]).ok).toBe(false);
    expect(validateFrameworkScenarios([{ ...filled, option_set: 'compact_decision', intended_answer: 'narrower' }]).ok).toBe(true);
    expect(validateFrameworkScenarios([{ ...filled, option_set: 'custom' }]).errors.join()).toMatch(/at least two/);
    // listed options under a canonical set must be keys of that set
    expect(validateFrameworkScenarios([{ ...sc, options: [...sc.options, { value: 'keep_open', label: 'Keep open' }] }]).errors.join()).toMatch(/keep_open is not a key of option_set/);
  });
});

describe('validateSubmission', () => {
  const sub = {
    submission_id: 'SUB-1',
    schema: 'aib-review-submission',
    schema_version: 1,
    campaign_id: 'c',
    kind: 'case_review',
    reviewer: { reviewer_id: 'r', anonymous: true, profile_digest: 'x' },
    created_at: '2027-01-01T00:00:00Z',
    submitted_at: '2027-01-01T00:00:00Z',
    app_commit: 'unknown',
    data_release: null,
    payload: { case_id: 'CASE_001', blind: { choice: 'secure', answered_at: '2027-01-01T00:00:00Z' }, reveal: { verdict: 5, revealed_at: '2027-01-01T00:01:00Z', main_issue: { text: 't', type: 'method' } } },
  };
  it('accepts a valid case review and rejects a reveal before the blind answer or a missing issue', () => {
    expect(validateSubmission(sub).errors).toEqual([]);
    expect(validateSubmission({ ...sub, payload: { ...sub.payload, reveal: { ...sub.payload.reveal, revealed_at: '2026-12-31T00:00:00Z' } } }).errors.join()).toMatch(/revealed before/);
    expect(validateSubmission({ ...sub, payload: { ...sub.payload, reveal: { verdict: 4, revealed_at: '2027-01-01T00:01:00Z' } } }).errors.join()).toMatch(/main_issue/);
    expect(validateSubmission({ ...sub, schema: 'other' }).ok).toBe(false);
  });
  it('requires page context and severity on data issues', () => {
    const di = { ...sub, kind: 'data_issue', payload: { what_is_wrong: 'x', severity: 'critical', metric: null } };
    expect(validateSubmission(di).errors.join()).toMatch(/page_context/);
    expect(validateSubmission({ ...di, page_context: { url: '/' } }).ok).toBe(true);
    expect(validateSubmission({ ...di, page_context: { url: '/' }, payload: { what_is_wrong: 'x', severity: 'huge' } }).ok).toBe(false);
  });
});
