// Tests of the case snapshot builder (qc/snapshot.ts) and the answer options (qc/answerOptions.ts): for every case
// kind the facts are keyed by exactly the shared fact ids (a missing one is null with a note, never dropped), the
// conclusion is a canonical key with its canonical label and is one of the answer options, revealing ids are
// rejected from context, the fact vocabularies equal review/manifest.ts's, and canonicalJson is stable.

import { describe, expect, it } from 'vitest';
import { AIB_CLASS_NAME, CHOICE_NAME } from '../../content/vocabulary.ts';
import { DONT_KNOW, type CaseKind } from '../model.ts';
import { FACT_IDS_BY_KIND, REVEALING_FACT_IDS as MANIFEST_REVEALING } from '../manifest.ts';
import { answerOptions, NOT_ASSESSED_OPTION } from './answerOptions.ts';
import { buildCaseSnapshot, canonicalJson, FACT_IDS, REVEALING_FACT_IDS, splitFacts } from './snapshot.ts';
import { HAVE_REAL, loadReal, mkAibDoc, mkDecoded, mkNode, mkPosture, triageTrace } from './__tests__/fixtures.ts';

const KINDS: CaseKind[] = ['posture_cell', 'aib_node', 'compact', 'partnership', 'sector'];

describe('shared vocabularies', () => {
  it('fact ids and revealing ids equal review/manifest.ts (one vocabulary for builder and validator)', () => {
    for (const k of KINDS) expect([...FACT_IDS[k]]).toEqual([...FACT_IDS_BY_KIND[k]]);
    expect([...REVEALING_FACT_IDS]).toEqual([...MANIFEST_REVEALING]);
  });
  it('answer options come from the canonical constants and always end with DONT_KNOW', () => {
    const post = answerOptions('posture_cell');
    expect(post.map((o) => o.value)).toEqual(['secure', 'preserve', 'catch_up', 'partner', 'accelerate', 'watch', 'not_assessed', DONT_KNOW]);
    expect(post.find((o) => o.value === 'catch_up')!.label).toBe(CHOICE_NAME.catch_up);
    expect(post.find((o) => o.value === 'not_assessed')).toEqual(NOT_ASSESSED_OPTION);
    expect(NOT_ASSESSED_OPTION.label).toBe('Not assessed: the data cannot support a call');
    const aib = answerOptions('aib_node');
    expect(aib.find((o) => o.value === 'BUILD')!.label).toBe(AIB_CLASS_NAME.BUILD);
    expect(answerOptions('compact', { sid: 'SC-BATTERIES' }).map((o) => o.value)).toEqual(['sectoral', 'narrower', 'none', DONT_KNOW]);
    for (const k of KINDS) expect(answerOptions(k).at(-1)!.value).toBe(DONT_KNOW);
  });
});

describe('buildCaseSnapshot on synthetic bundles', () => {
  const p = mkPosture([
    { iso3: 'KOR', tech: 'Batteries', stage: 'Components', choice: 'catch_up', need: 0.73, prize: 0.79, market: 0.85, cp: 0.71, share: 0.06, pathway: 'yes', trace: triageTrace({ need: 0.73, prize: 0.79, cp: 0.71, pos: 'not_top_quarter', pathway: 'yes', choice: 'catch_up' }), extra: { china_share: 0.77, top_supplier: 'CHN', top_supplier_share: 0.77 } },
  ]);
  const node = mkNode({ id: 'rare-earth-metals', variants: { china_50: 'BUILD', release: 'BUILD' }, growth: [] });
  const sc = {
    id: 'SC-TEST', name: 'Test', purpose: 'a purpose', core: [{ iso3: 'AUS', role: 'Resource supplier', step: 'Minerals' }], steps: [{ order: 1, label: 'Minerals', nodes: [], functions: [] }],
    scenarios: { decoupling: { coverage: { static: 0.5, existing: 0.6, coordinated: 0.7, postInvestment: 0.8 }, lostUsd: 1e9, exposure: 0.5, demandUsd: 2e9, counts: { build: 1, coordinate: 0, manage: 0, low: 0, insufficient: 0, notStressed: 0, notTested: 0, noShock: 0 }, missions: [{ slot: 'primary', cls: 'BUILD', text: 'build', basis: 'b', targeted: false }], statement: 's', criticalExternal: [], nonAlliedReliedOn: ['CHN'], buildHosts: ['DEU'] } },
  };
  const memo = {
    sections: [{ id: 'summary', blocks: [{ type: 'choices', items: [{ label: 'Build together', kind: 'preserve', inlines: [{ v: 'batteries' }] }, { label: 'Stay out', kind: 'watch', inlines: [{ v: 'solar' }] }] }] }],
  };
  const b = mkDecoded({
    posture: p,
    aib: mkAibDoc([node], { strategic: { compacts: [sc] } }),
    memos: { manifest: { schema: 'osi-partnership-memos', schema_version: 1, pairs: { USA_JPN: { json: 'x.json', verified: false, as_of: '2026-10-30', generated_at: '2026-11-06' } }, compacts: {} }, manifestSha256: null, memos: { USA_JPN: memo as never } },
  });
  const cases: [CaseKind, Record<string, string>][] = [
    ['posture_cell', { iso3: 'KOR', tech: 'Batteries', stage: 'Components' }],
    ['aib_node', { node: 'rare-earth-metals', scenario: 'decoupling' }],
    ['compact', { sid: 'SC-TEST', scenario: 'decoupling' }],
    ['partnership', { pair: 'USA_JPN' }],
  ];
  it.each(cases)('%s: facts keyed by exactly the shared ids; missing ones null with a note; conclusion canonical', (kind, key) => {
    const s = buildCaseSnapshot({ kind, key }, b);
    expect(Object.keys(s.facts).sort()).toEqual([...FACT_IDS[kind]].sort());
    for (const f of Object.values(s.facts)) {
      expect(f.id).toBeTruthy();
      if (f.value === null) expect(f.note, `${kind} ${f.id}`).toBeTruthy();
    }
    const opts = answerOptions(kind, key);
    expect(opts.map((o) => o.value)).toContain(s.conclusion.value);
    expect(s.conclusion.label).toBe(opts.find((o) => o.value === s.conclusion.value)!.label);
    expect(s.data_quality).toBeTruthy();
  });
  it('posture: the conclusion, trace and robustness; the fence and posture facts', () => {
    const s = buildCaseSnapshot({ kind: 'posture_cell', key: cases[0][1] }, b);
    expect(s.conclusion).toMatchObject({ value: 'catch_up', label: CHOICE_NAME.catch_up });
    expect(s.facts.fence.value).toBe('medium');
    expect(s.facts.need.value).toBe(0.73);
    expect(s.facts.deployment_need.value).toBeNull();
    expect(s.trace!.at(-1)).toMatchObject({ test: 'choice', result: 'catch_up' });
    expect(s.robustness!.layer).toBe('bundle');
  });
  it('AIB node and compact carry their threat metrics; the compact conclusion reads the mission rule', () => {
    expect(buildCaseSnapshot({ kind: 'aib_node', key: cases[1][1] }, b).threat!.subject.id).toBe('rare-earth-metals');
    const c = buildCaseSnapshot({ kind: 'compact', key: cases[2][1] }, b);
    expect(c.conclusion.value).toBe('sectoral');
    expect(c.threat!.allied_coverage).toBe(0.7);
  });
  it('partnership: the memo box; unverified is a data-quality note', () => {
    const s = buildCaseSnapshot({ kind: 'partnership', key: { pair: 'USA_JPN' } }, b);
    expect(s.conclusion.value).toBe('preserve');
    expect(s.facts.memo_verified.value).toBe(false);
    expect(s.data_quality!.grade).toBe('unverified');
  });
  it('a subject not in the release is "missing" with every fact null and explained', () => {
    const s = buildCaseSnapshot({ kind: 'posture_cell', key: { iso3: 'ZZZ', tech: 'Batteries', stage: 'Components' } }, b);
    expect(s.conclusion.value).toBe('missing');
    expect(Object.values(s.facts).every((f) => f.value === null && !!f.note)).toBe(true);
  });
  it('splitFacts rejects a revealing id in context, for every kind', () => {
    for (const k of KINDS) {
      const s = buildCaseSnapshot({ kind: k, key: {} }, b);
      for (const id of REVEALING_FACT_IDS.filter((r) => FACT_IDS[k].includes(r))) {
        const out = splitFacts({ case_id: 'CASE_999', context_fields: [id], reveal_fields: [] }, s);
        expect(out.errors, `${k} ${id}`).toHaveLength(1);
      }
      const safe = FACT_IDS[k].filter((id) => !REVEALING_FACT_IDS.includes(id));
      expect(splitFacts({ case_id: 'CASE_999', context_fields: safe, reveal_fields: [] }, s).errors).toEqual([]);
    }
  });
  it('canonicalJson is key-order independent', () => {
    expect(canonicalJson({ b: 1, a: { d: [2, { z: 1, y: 2 }], c: null } })).toBe(canonicalJson({ a: { c: null, d: [2, { y: 2, z: 1 }] }, b: 1 }));
  });
});

describe.skipIf(!HAVE_REAL)('snapshots from the shipped bundles', () => {
  it('KOR Batteries/Components, an AIB node, a strategic compact and the USA_JPN memo', () => {
    const { decoded } = loadReal();
    const post = buildCaseSnapshot({ kind: 'posture_cell', key: { iso3: 'KOR', tech: 'Batteries', stage: 'Components' } }, decoded);
    expect(answerOptions('posture_cell').map((o) => o.value)).toContain(post.conclusion.value);
    expect(post.trace!.length).toBeGreaterThan(2);
    const node = decoded.aib!.doc.nodes[0];
    expect(buildCaseSnapshot({ kind: 'aib_node', key: { node: node.id, scenario: 'decoupling' } }, decoded).conclusion.value).toBe(node.cls);
    const sid = decoded.aib!.doc.strategic!.compacts[0].id;
    expect(['sectoral', 'narrower', 'none']).toContain(buildCaseSnapshot({ kind: 'compact', key: { sid, scenario: 'decoupling' } }, decoded).conclusion.value);
    expect(buildCaseSnapshot({ kind: 'partnership', key: { pair: 'USA_JPN' } }, decoded).conclusion.value).not.toBe('missing');
  });
});
