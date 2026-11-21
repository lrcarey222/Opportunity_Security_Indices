// Synthetic demo submissions (review/demo.ts): deterministic, valid envelopes, unmistakably synthetic
// (DEMO- reviewers, no names, every free text prefixed), and covering each admin-dashboard section.

import { describe, expect, it } from 'vitest';
import { CHOICE_NAME } from '../content/vocabulary.ts';
import type { FrameworkScenario, MethodCard, ReviewCase } from './model.ts';
import { buildDashboard } from './aggregate.ts';
import { SYNTHETIC, makeDemoSubmissions } from './demo.ts';
import { validateSubmission } from './manifest.ts';

const OPTIONS = [...Object.entries(CHOICE_NAME).map(([value, label]) => ({ value, label })), { value: 'not_assessed', label: 'Not assessed' }];
const rcase = (id: string, model: string, x: Partial<ReviewCase> = {}): ReviewCase => ({
  case_id: id,
  kind: 'posture_cell',
  key: { iso3: 'KOR', tech: 'Batteries', stage: 'Components' },
  title: id,
  question: 'q',
  sector_groups: ['batteries'],
  country_groups: ['KOR'],
  stage: 'Components',
  assignment_group: 'batteries',
  anchor: false,
  why_selected: 'w',
  explorer_path: '/countries/KOR',
  context_fields: [],
  reveal_fields: [],
  snapshot: { conclusion: { value: model, label: model }, facts: {} },
  snapshot_sha256: '0'.repeat(64),
  bundle: [],
  data_release: 'rel-0123456789ab',
  app_commit: 'abc',
  built_at: '2027-01-10T00:00:00Z',
  answer_options: OPTIONS,
  ...x,
});
const CASES = [rcase('CASE_001', 'partner', { anchor: true, decision_use: true }), rcase('CASE_002', 'secure', { threat: true }), rcase('CASE_003', 'watch')];
const CARDS = [{ card_id: 'MC_001', title: 't', method_tags: ['trade_data'], affected_outputs: [] }, { card_id: 'MC_002', title: 't2', method_tags: ['trade_data'], affected_outputs: [] }] as unknown as MethodCard[];
const SCEN = [{ scenario_id: 'FS_001', concept: 'c', prompt: 'p', options: OPTIONS.slice(0, 3), intended_answer: 'preserve', explanation: 'e', terms: ['fence'] }] as FrameworkScenario[];

/** Every string anywhere in a value. */
function strings(x: unknown, out: string[] = []): string[] {
  if (typeof x === 'string') out.push(x);
  else if (Array.isArray(x)) x.forEach((v) => strings(v, out));
  else if (x && typeof x === 'object') Object.values(x).forEach((v) => strings(v, out));
  return out;
}
const FREE_TEXT = ['rationale', 'text', 'evidence_wanted', 'what_is_wrong', 'corrected_value', 'citation', 'alternative', 'comment', 'would_do_differently', 'evidence_required', 'improvement', 'decision_use', 'success_looks_like', 'stop_point'];
function freeText(x: unknown, out: string[] = []): string[] {
  if (Array.isArray(x)) x.forEach((v) => freeText(v, out));
  else if (x && typeof x === 'object') for (const [k, v] of Object.entries(x)) (FREE_TEXT.includes(k) && typeof v === 'string' ? out.push(v) : freeText(v, out));
  return out;
}

describe('makeDemoSubmissions', () => {
  const subs = makeDemoSubmissions(CASES, CARDS, SCEN, 7);

  it('is deterministic for a seed and differs across seeds', () => {
    expect(makeDemoSubmissions(CASES, CARDS, SCEN, 7)).toEqual(subs);
    expect(makeDemoSubmissions(CASES, CARDS, SCEN, 8).map((s) => s.submission_id)).not.toEqual(subs.map((s) => s.submission_id));
    expect(new Set(subs.map((s) => s.submission_id)).size).toBe(subs.length);
  });

  it('produces valid submissions that are unmistakably synthetic', () => {
    for (const s of subs) {
      expect(validateSubmission(s).errors).toEqual([]);
      expect(s.reviewer.reviewer_id.startsWith('DEMO-')).toBe(true);
      expect(s.reviewer.name).toBeNull();
      expect(s.submission_id.startsWith('SUB-DEMO-')).toBe(true);
    }
    const texts = freeText(subs.map((s) => s.payload));
    expect(texts.length).toBeGreaterThan(10);
    for (const t of texts) expect(t.startsWith(SYNTHETIC)).toBe(true);
    expect(strings(subs).some((t) => /@/.test(t))).toBe(false);
  });

  it('covers agreement, persuasion, rejection, a critical issue and every submission kind', () => {
    expect(new Set(subs.map((s) => s.kind))).toEqual(new Set(['case_review', 'data_issue', 'break_model', 'method_feedback', 'framework_answer', 'decision_use', 'threat_review']));
    const d = buildDashboard({ submissions: subs, cases: CASES, cards: CARDS, scenarios: SCEN });
    const c1 = d.agreement.find((a) => a.case_id === 'CASE_001')!;
    expect(c1.too_few).toBe(false);
    expect(c1.n_persuaded).toBe(1);
    expect(c1.n_rejecting).toBe(1);
    expect(c1.n_outside).toBe(1);
    expect(d.agreement.find((a) => a.case_id === 'CASE_002')!.too_few).toBe(true);
    expect(d.unresolved_critical).toHaveLength(1);
    expect(d.priorities[0].severity).toBe('critical');
    expect(d.taxonomy.find((t) => t.issue_type === 'framework')!.total).toBeGreaterThan(0);
    expect(d.taxonomy.find((t) => t.issue_type === 'actionability')!.total).toBeGreaterThan(0);
  });

  it('degrades to fewer submissions on empty inputs', () => {
    expect(makeDemoSubmissions([], [], [], 1)).toEqual([]);
    expect(makeDemoSubmissions(CASES.slice(0, 1), [], [], 1).length).toBeGreaterThan(0);
  });
});
