// Tests of the review data build (qc/reviewData.ts) on a tiny synthetic fixture: authored content is validated
// (an invalid file stops the build), the build is deterministic (same inputs -> byte-identical documents),
// snapshots are hashed with privacy.ts, cases carry the stamps of the files they read, canonical option sets are
// filled, a case whose subject is not in the release is an error, and the run reports serialise.

import { describe, expect, it } from 'vitest';
import { DONT_KNOW, type Campaign, type CaseDefinition, type FrameworkScenario, type MethodCard } from '../model.ts';
import { canonicalJson, sha256HexSync } from '../privacy.ts';
import { validateCasesFile, validateFrameworkScenarios } from '../manifest.ts';
import { buildReviewData, caseBundleFiles, optionSetOptions, reviewManifest, validateReviewContent, type ReviewContentRaw } from './reviewData.ts';
import { robustnessSummaryRows, sensitivityRows, toCsv } from './reports.ts';
import { runShippedQc } from './run.ts';
import { campaignOf } from '../data/store.ts';
import { mkAibDoc, mkDecoded, mkNode, mkPosture, triageTrace } from './__tests__/fixtures.ts';

const postureCase: CaseDefinition = {
  case_id: 'CASE_001', kind: 'posture_cell', key: { iso3: 'KOR', tech: 'Batteries', stage: 'Components' }, title: 'Korea batteries', question: 'Which posture?',
  sector_groups: ['batteries'], country_groups: ['KOR'], stage: 'Components', assignment_group: 'batteries', anchor: true, why_selected: 'test', explorer_path: '/countries/KOR',
  context_fields: ['home_share', 'china_share'], reveal_fields: ['choice', 'need', 'cp'],
};
const nodeCase: CaseDefinition = {
  case_id: 'CASE_002', kind: 'aib_node', key: { node: 'magnets', scenario: 'decoupling' }, title: 'Magnets', question: 'Which response?', sector_groups: ['critical-minerals'],
  country_groups: [], stage: 'Components', assignment_group: 'critical-minerals', anchor: false, why_selected: 'test', explorer_path: '/aib', context_fields: ['exposure', 'baseline_hhi'],
  reveal_fields: ['class', 'residual'], threat: true,
};
const campaign: Campaign = {
  campaign_id: 'test_round', name: 'Test', status: 'draft', opens: null, closes: null, review_types: ['case', 'methodology', 'framework'], case_ids: ['CASE_001', 'CASE_002'],
  anchor_cases: ['CASE_001'], target_reviews_per_anchor: 3, assignment: { assigned_per_reviewer: { min: 1, max: 2 }, anchors_per_reviewer: 1, out_of_specialty: 'optional', allow_self_selected: true, min_domain_confidence: 3 },
  method_card_ids: [], framework_scenario_ids: [],
};
const card: MethodCard = {
  card_id: 'MC_001', title: 'A card', area: 'choice_rule', current_choice: 'c', why: 'w', example: { text: 'e' }, alternatives: ['a'], affected_outputs: ['o'], code_refs: ['R/x.R:1'], method_tags: ['indices_statistics'],
};
const scenario = { scenario_id: 'FS_001', concept: 'c', prompt: 'p', options: [], option_set: 'posture_choice', intended_answer: 'partner', explanation: 'e', terms: ['Partner'] } as unknown as FrameworkScenario;

const raw = (over: Partial<ReviewContentRaw> = {}): ReviewContentRaw => ({
  campaigns: [{ file: 'test_round.yml', doc: campaign }], catalogue: { cases: [postureCase, nodeCase] }, cards: { cards: [card] }, scenarios: { scenarios: [scenario] }, ...over,
});

function bundles() {
  const p = mkPosture([
    { iso3: 'KOR', tech: 'Batteries', stage: 'Components', choice: 'catch_up', need: 0.73, prize: 0.79, cp: 0.71, share: 0.06, pathway: 'yes', trace: triageTrace({ need: 0.73, prize: 0.79, cp: 0.71, pos: 'not_top_quarter', pathway: 'yes', choice: 'catch_up' }), extra: { china_share: 0.77 } },
  ]);
  const d = mkDecoded({ posture: p, aib: mkAibDoc([mkNode({ id: 'magnets' })]) });
  d.posture!.manifest = { ...d.posture!.manifest, core: 'posture-core.x.json', details: { KOR: 'posture-detail-KOR.x.json' } };
  d.aib!.manifest = { created: 'a', files: { compacts: 'compacts.x.json', scenarios: { managed: 'm.json', baseline: 'b.json' } } };
  d.files = ['posture/manifest.json', 'posture/posture-core.x.json', 'posture/posture-detail-KOR.x.json', 'aib/manifest.json', 'aib/compacts.x.json'].map((file) => ({ file, sha256: sha256HexSync(file), created: null, git_commit: null }));
  return d;
}

function buildOnce() {
  const v = validateReviewContent(raw());
  expect(v.errors).toEqual([]);
  const decoded = bundles();
  const run = runShippedQc({ decoded, releaseId: 'rel-000000000001', golden: null });
  return buildReviewData({ content: v.content!, decoded, run, releaseId: 'rel-000000000001', appCommit: 'abc1234', builtAt: '2026-11-20T00:00:00Z' });
}

describe('validateReviewContent', () => {
  it('accepts valid content and rejects invalid files with clear messages', () => {
    expect(validateReviewContent(raw()).content).not.toBeNull();
    const bad = validateReviewContent(raw({ catalogue: { cases: [{ ...postureCase, context_fields: ['choice'] }, nodeCase] } }));
    expect(bad.content).toBeNull();
    expect(bad.errors.join(' ')).toMatch(/review_cases.yml: .*choice/);
    const unknownCard = validateReviewContent(raw({ campaigns: [{ file: 'test_round.yml', doc: { ...campaign, method_card_ids: ['MC_999'] } }] }));
    expect(unknownCard.errors.join(' ')).toMatch(/MC_999/);
    expect(validateReviewContent(raw({ campaigns: [] })).errors.join(' ')).toMatch(/no campaign file/);
  });
});

describe('buildReviewData', () => {
  it('is deterministic: the same inputs give byte-identical documents', () => {
    expect(JSON.stringify(buildOnce())).toBe(JSON.stringify(buildOnce()));
  });
  it('builds valid cases with snapshot hashes, bundle stamps, release, commit and canonical options', () => {
    const b = buildOnce();
    expect(b.errors).toEqual([]);
    const f = b.casesFiles[0];
    expect(validateCasesFile(f).errors).toEqual([]);
    const c = f.cases[0];
    expect(c.snapshot_sha256).toBe(sha256HexSync(canonicalJson(c.snapshot)));
    expect(c.bundle.map((x) => x.file)).toEqual(['posture/manifest.json', 'posture/posture-core.x.json', 'posture/posture-detail-KOR.x.json']);
    expect(c).toMatchObject({ data_release: 'rel-000000000001', app_commit: 'abc1234', built_at: '2026-11-20T00:00:00Z' });
    expect(c.answer_options.at(-1)!.value).toBe(DONT_KNOW);
    expect(c.snapshot.robustness!.layer).toBe('bundle');
    expect(f.cases[1].snapshot.threat).toBeTruthy();
    expect(b.bundleFiles.map((x) => x.file)).toContain('aib/compacts.x.json');
  });
  it('fills canonical option sets in framework scenarios (valid again after filling)', () => {
    const b = buildOnce();
    expect(b.frameworkScenarios.scenarios[0].options.map((o) => o.value)).toEqual(['secure', 'preserve', 'catch_up', 'partner', 'accelerate', 'watch', 'not_assessed']);
    expect(validateFrameworkScenarios(b.frameworkScenarios).errors).toEqual([]);
    expect(optionSetOptions('compact_decision')!.map((o) => o.value)).toContain('sectoral');
    expect(optionSetOptions('custom')).toBeNull();
  });
  it('a compact_decision scenario gets one option family: the strategic keys for a strategic intended answer', () => {
    expect(optionSetOptions('compact_decision', 'sectoral')!.map((o) => o.value)).toEqual(['sectoral', 'narrower', 'none']);
    const operational = optionSetOptions('compact_decision', 'candidate compact')!.map((o) => o.value);
    expect(operational).not.toContain('sectoral');
    expect(operational).toContain('candidate compact');
    const fs = { ...scenario, scenario_id: 'FS_010', option_set: 'compact_decision', intended_answer: 'sectoral', terms: [] } as unknown as FrameworkScenario;
    const v = validateReviewContent(raw({ scenarios: { scenarios: [scenario, fs] } }));
    const decoded = bundles();
    const b = buildReviewData({ content: v.content!, decoded, run: runShippedQc({ decoded, releaseId: 'rel-000000000001', golden: null }), releaseId: 'rel-000000000001', appCommit: 'x', builtAt: 't' });
    expect(b.frameworkScenarios.scenarios.find((x) => x.scenario_id === 'FS_010')!.options.map((o) => o.value)).toEqual(['sectoral', 'narrower', 'none']);
    expect(b.errors).toEqual([]);
  });
  it('a case whose subject is not in the release is a build error', () => {
    const v = validateReviewContent(raw({ catalogue: { cases: [{ ...postureCase, key: { iso3: 'JPN', tech: 'Batteries', stage: 'Components' }, country_groups: ['JPN'] }, nodeCase] } }));
    const decoded = bundles();
    const run = runShippedQc({ decoded, releaseId: 'rel-000000000001' });
    const b = buildReviewData({ content: v.content!, decoded, run, releaseId: 'rel-000000000001', appCommit: 'x', builtAt: 't' });
    expect(b.errors.join(' ')).toMatch(/CASE_001: posture_cell .* is not in this release/);
  });
  it('caseBundleFiles names the managed scenario file for a managed case', () => {
    expect(caseBundleFiles({ kind: 'aib_node', key: { node: 'magnets', scenario: 'managed' } }, bundles())).toEqual(['aib/manifest.json', 'aib/compacts.x.json', 'aib/m.json']);
  });
  it('the manifest lists campaigns, outputs and the release', () => {
    const b = buildOnce();
    const decoded = bundles();
    const run = runShippedQc({ decoded, releaseId: 'rel-000000000001' });
    const m = reviewManifest({
      releaseId: 'rel-000000000001', stamps: decoded.stamps, completeness: { complete: true, missing: [] }, appCommit: 'abc1234', builtAt: 't',
      content: validateReviewContent(raw()).content!, casesFiles: b.casesFiles, outputs: [{ file: 'z.json', sha256: 'a', bytes: 1 }, { file: 'a.json', sha256: 'b', bytes: 2 }], bundleFiles: b.bundleFiles, run,
    });
    expect(m.schema).toBe('aib-review-manifest');
    expect(m.campaigns[0]).toMatchObject({ campaign_id: 'test_round', n_cases: 2, cases_file: 'cases.test_round.json', method_card_ids: ['MC_001'] });
    expect(m.files.map((f) => f.file)).toEqual(['a.json', 'z.json']);
  });
  it("the manifest carries each campaign's own rules, so the browser needs no fallback", () => {
    const b = buildOnce();
    const decoded = bundles();
    const run = runShippedQc({ decoded, releaseId: 'rel-000000000001', golden: null });
    const content = validateReviewContent(raw({ campaigns: [{ file: 'test_round.yml', doc: { ...campaign, description: ' A round. ', reviewers: [{ code: 'R-0001', groups: ['batteries'] }] } }] })).content!;
    const m = reviewManifest({
      releaseId: 'rel-000000000001', stamps: decoded.stamps, completeness: { complete: true, missing: [] }, appCommit: 'abc1234', builtAt: 't',
      content, casesFiles: b.casesFiles, outputs: [], bundleFiles: b.bundleFiles, run,
    });
    expect(m.campaigns[0]).toMatchObject({
      description: 'A round.', case_ids: ['CASE_001', 'CASE_002'], target_reviews_per_anchor: 3, assignment: campaign.assignment, reviewers: [{ code: 'R-0001', groups: ['batteries'] }],
    });
    const { campaign: c, fallback } = campaignOf(JSON.parse(JSON.stringify(m.campaigns[0])), null);
    expect(fallback).toEqual([]);
    expect(c.assignment).toEqual(campaign.assignment);
    expect(c.target_reviews_per_anchor).toBe(3);
    expect(c.case_ids).toEqual(['CASE_001', 'CASE_002']);
    expect(c.reviewers).toEqual([{ code: 'R-0001', groups: ['batteries'] }]);
  });
});

describe('QC run reports', () => {
  it('serialise to CSV with a class row per cell and the release on every row', () => {
    const decoded = bundles();
    const run = runShippedQc({ decoded, releaseId: 'rel-000000000001' });
    const rows = sensitivityRows(run);
    expect(rows.filter((r) => r.row_type === 'class').length).toBe(decoded.posture!.core.cells.length + decoded.aib!.doc.nodes.length);
    expect(rows.every((r) => r.data_release === 'rel-000000000001' && r.layer === 'bundle')).toBe(true);
    const csv = toCsv([{ a: 'x,"y"', b: 1, c: null, d: true }], ['a', 'b', 'c', 'd']);
    expect(csv).toBe('a,b,c,d\n"x,""y""",1,,TRUE\n');
    expect(robustnessSummaryRows(run)[0]).toMatchObject({ subject_kind: 'posture_cell', dimension: 'all', n: 1 });
  });
  it('the compact sensitivity CSV keeps flips and non-stable class rows only', () => {
    const decoded = bundles();
    const run = runShippedQc({ decoded, releaseId: 'rel-000000000001' });
    const all = sensitivityRows(run);
    const compact = sensitivityRows(run, { compact: true });
    expect(compact.filter((r) => r.row_type !== 'class')).toEqual(all.filter((r) => r.row_type !== 'class'));
    expect(compact.filter((r) => r.row_type === 'class').map((r) => r.class)).toEqual(all.filter((r) => r.row_type === 'class' && r.class !== 'stable').map((r) => r.class));
    expect(compact.some((r) => r.row_type === 'class' && r.class === 'stable')).toBe(false);
  });
});
