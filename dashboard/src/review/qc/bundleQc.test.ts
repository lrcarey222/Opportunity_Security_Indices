// Tests of the shipped-bundle checks (qc/bundleQc.ts, design 6.1) on synthetic bundles, clean and broken: NaN and
// out-of-range numbers, an invalid code, a posture invariant violation, a trace contradicting its values, missing
// stamps, AIB coverage-path / residual / class / zero-gain-host problems, the TS fallback thresholds; and the
// check set over the real bundle.

import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';
import { SUPPLY_STAGES } from '../../lib/stages.ts';
import type { RawBundles } from './bundles.ts';
import { reproduceShippedChoices } from './choiceRule.ts';
import { parseDefaultThresholds, qcSummary, runBundleQc, type QcRow } from './bundleQc.ts';
import { HAVE_REAL, loadReal, mkAibDoc, mkDecoded, mkNode, mkPosture, triageTrace, type CellSpec } from './__tests__/fixtures.ts';

/** One economy x one technology x six stages, every cell a consistent low-need low-prize Watch (Deployment: low deployment need). */
function sixCells(over: Partial<Record<string, Partial<CellSpec>>> = {}): CellSpec[] {
  return SUPPLY_STAGES.map((stage) => {
    const base: CellSpec =
      stage === 'Deployment'
        ? { tech: 'Batteries', stage, choice: 'watch', need: 0.3, trace: [{ step: 'secure', test: 'secure_override', result: 'no' }, { step: 'need', test: 'deployment_need', value: 0.3, threshold: 0.6, result: 'low' }, { step: 'choice', test: 'choice', result: 'watch' }] }
        : { tech: 'Batteries', stage, choice: 'watch', need: 0.2, prize: 0.3, cp: 0.1, share: 0, pathway: 'no', trace: triageTrace({ need: 0.2, prize: 0.3, cp: 0.1, pos: 'not_top_quarter', choice: 'watch' }) };
    return { ...base, ...(over[stage] ?? {}) };
  });
}

/** The raw columnar core of a posture (for the raw column checks). */
function rawOf(p: ReturnType<typeof mkPosture>, patch: (cols: Record<string, unknown[]>) => void = () => {}): RawBundles {
  const cells = p.core.cells;
  const cols: Record<string, unknown[]> = {
    c: cells.map(() => 0), t: cells.map(() => 0), s: cells.map((c) => SUPPLY_STAGES.indexOf(c.stage)), p: cells.map((c) => c.posture),
    m: cells.map((c) => c.mode), dm: cells.map((c) => c.deployMode), need: cells.map((c) => c.need), prize: cells.map((c) => c.prize), cp: cells.map((c) => c.cp),
  };
  patch(cols);
  return { posture: { manifest: {}, core: { cells: cols }, details: {} } };
}

const row = (rows: QcRow[], id: string) => {
  const r = rows.find((x) => x.check_id === id);
  if (!r) throw new Error(`no row ${id}`);
  return r;
};

describe('runBundleQc on a clean synthetic bundle', () => {
  const p = mkPosture(sixCells());
  const rep = reproduceShippedChoices(p.core, p.details);
  const rows = runBundleQc({ decoded: mkDecoded({ posture: p, aib: mkAibDoc([mkNode({ id: 'n1' })]) }), raw: rawOf(p), reproduction: rep, fallbackThresholds: { need: 0.67, prize: 0.6, cp: 0.75, deployment: 0.6, borderline_band: 0.05 } });
  it('every row has the shape {check_id, layer bundle, area, status, n, detail, examples}', () => {
    for (const r of rows) {
      expect(r.layer).toBe('bundle');
      expect(['pass', 'warn', 'fail', 'info']).toContain(r.status);
      expect(typeof r.detail).toBe('string');
      expect(Array.isArray(r.examples)).toBe(true);
    }
  });
  it('fails nothing', () => {
    expect(rows.filter((r) => r.status === 'fail').map((r) => `${r.check_id}: ${r.detail} ${r.examples.join(' | ')}`)).toEqual([]);
    expect(row(rows, 'BQ-POST-REPRO-01').status).toBe('pass');
    expect(row(rows, 'BQ-POST-COMPLETE-01').status).toBe('pass');
  });
});

describe('runBundleQc catches broken bundles', () => {
  it('NaN, malformed and out-of-range numbers in the core columns', () => {
    const p = mkPosture(sixCells());
    const raw = rawOf(p, (c) => {
      c.need[0] = 'NaN';
      c.prize[1] = 1.4;
      c.cp[2] = Number.POSITIVE_INFINITY;
    });
    const r = row(runBundleQc({ decoded: mkDecoded({ posture: p }), raw }), 'BQ-POST-NUM-01');
    expect(r.status).toBe('fail');
    expect(r.n).toBe(3);
    expect(r.examples.join(' ')).toMatch(/need\[0\] = "NaN"/);
    expect(r.examples.join(' ')).toMatch(/prize\[1\] = 1.4 \(outside/);
  });
  it('an invalid posture code', () => {
    const p = mkPosture(sixCells());
    const r = row(runBundleQc({ decoded: mkDecoded({ posture: p }), raw: rawOf(p, (c) => void (c.p[0] = 'X')) }), 'BQ-POST-CODES-01');
    expect(r.status).toBe('fail');
    expect(r.examples[0]).toMatch(/p="X"/);
  });
  it('posture invariant violations (Accelerate outside Deployment, Compete at Deployment)', () => {
    const p = mkPosture(sixCells());
    p.core.cells[0] = { ...p.core.cells[0], posture: 'D', deployMode: 'accelerate', mode: null };
    p.core.cells[4] = { ...p.core.cells[4], posture: 'C', mode: 'partner', deployMode: null, yards: 'medium' };
    const r = row(runBundleQc({ decoded: mkDecoded({ posture: p }) }), 'BQ-POST-INV-01');
    expect(r.status).toBe('fail');
    expect(r.examples.join(' ')).toMatch(/Accelerate outside Deployment/);
    expect(r.examples.join(' ')).toMatch(/Compete at Deployment/);
  });
  it('a trace word that contradicts the shipped value, and a trace that does not end in the shipped choice', () => {
    const p = mkPosture(sixCells({ Refining: { need: 0.9, choice: 'watch', trace: triageTrace({ need: 0.2, prize: 0.3, cp: 0.1, pos: 'not_top_quarter', choice: 'partner' }) } }));
    const rows = runBundleQc({ decoded: mkDecoded({ posture: p }) });
    expect(row(rows, 'BQ-POST-TRACE-03').status).toBe('fail');
    expect(row(rows, 'BQ-POST-TRACE-03').examples.join(' ')).toMatch(/supply_chain_need: value 0.9 against 0.67 reads high, trace says low/);
    expect(row(rows, 'BQ-POST-TRACE-01').status).toBe('fail');
  });
  it('a borderline tag missing where the value sits inside the band', () => {
    const p = mkPosture(sixCells({ Refining: { need: 0.69, prize: 0.3, choice: 'partner', trace: triageTrace({ need: 0.69, prize: 0.3, cp: 0.1, pos: 'not_top_quarter', choice: 'partner' }) } }));
    const r = row(runBundleQc({ decoded: mkDecoded({ posture: p }) }), 'BQ-POST-BORDER-01');
    expect(r.status).toBe('fail');
    expect(r.examples[0]).toMatch(/supply_chain_need: value 0.69 is within 0.05 of the bar but the cell lacks the borderline tag/);
  });
  it('an incomplete grid (a missing cell)', () => {
    const p = mkPosture(sixCells().slice(0, 5));
    expect(row(runBundleQc({ decoded: mkDecoded({ posture: p }) }), 'BQ-POST-COMPLETE-01').status).toBe('fail');
  });
  it('missing stamps: posture commit and the release key', () => {
    const p = mkPosture(sixCells());
    const d = mkDecoded({ posture: p });
    d.stamps = { ...d.stamps, posture: { created: '2026-01-01T00:00:00Z', git_commit: null }, aib: null };
    d.posture!.manifest = { schema: 'osi-posture', schema_version: 2, created: '2026-01-01T00:00:00Z' };
    const rows = runBundleQc({ decoded: d });
    expect(row(rows, 'BQ-PROV-POSTURE-01').status).toBe('warn');
    expect(row(rows, 'BQ-REL-01')).toMatchObject({ status: 'warn' });
    expect(row(rows, 'BQ-REL-01').examples).toEqual(['posture.git_commit', 'aib']);
  });
  it('the TS fallback deployment threshold differing from the core', () => {
    const p = mkPosture(sixCells());
    const r = row(runBundleQc({ decoded: mkDecoded({ posture: p }), fallbackThresholds: { deployment: 0.67 } }), 'BQ-POST-FALLBACK-01');
    expect(r.status).toBe('warn');
    expect(r.examples[0]).toMatch(/deployment: DEFAULT_THRESHOLDS 0.67 vs core meta 0.6/);
  });
  it('AIB: coverage path, residual, class vocabulary, zero-gain rank-1 host', () => {
    const doc = mkAibDoc(
      [
        mkNode({ id: 'bad-path', static: 0.7, existing: 0.6, optimized: 0.8, residual: 0.2 }),
        mkNode({ id: 'bad-resid', static: 0.5, existing: 0.6, optimized: 0.8, residual: 0.1 }),
        mkNode({ id: 'bad-cls', cls: 'MAYBE' }),
      ],
      { investmentFlows: { hosts: [{ node: 'bad-path', host: 'PNG', rank: 1, gain: 0, rankBasis: 'scaling host first' }] } },
    );
    const rows = runBundleQc({ decoded: mkDecoded({ aib: doc }) });
    expect(row(rows, 'BQ-AIB-PATH-01')).toMatchObject({ status: 'fail', n: 1 });
    expect(row(rows, 'BQ-AIB-RESID-01')).toMatchObject({ status: 'fail', n: 1 });
    expect(row(rows, 'BQ-AIB-CLASS-01').status).toBe('fail');
    expect(row(rows, 'BQ-AIB-HOST-01').status).toBe('warn');
    expect(row(rows, 'BQ-AIB-HOST-01').examples[0]).toMatch(/PNG gain 0/);
  });
  it('a missing bundle fails its schema row instead of crashing', () => {
    const rows = runBundleQc({ decoded: mkDecoded({}) });
    expect(row(rows, 'BQ-POST-SCHEMA-01').status).toBe('fail');
    expect(row(rows, 'BQ-AIB-SCHEMA-01').status).toBe('fail');
    expect(qcSummary(rows).fail).toBeGreaterThanOrEqual(2);
  });
});

describe('parseDefaultThresholds', () => {
  it('reads DEFAULT_THRESHOLDS from posture/contract.ts', () => {
    const src = readFileSync(fileURLToPath(new URL('../../posture/contract.ts', import.meta.url)), 'utf8');
    const t = parseDefaultThresholds(src);
    expect(t).not.toBeNull();
    expect(Object.keys(t!).sort()).toEqual(['borderline_band', 'cp', 'deployment', 'need', 'prize']);
  });
  it('returns null on text without the constant', () => {
    expect(parseDefaultThresholds('const x = 1;')).toBeNull();
  });
});

describe.skipIf(!HAVE_REAL)('runBundleQc on the shipped bundles', () => {
  it('runs every check; the posture grid is complete and the trace ends in the shipped choice', () => {
    const { decoded, load } = loadReal();
    const p = decoded.posture!;
    const rows = runBundleQc({ decoded, raw: load.raw, loadProblems: load.problems, reproduction: reproduceShippedChoices(p.core, p.details) });
    const ids = rows.map((r) => r.check_id);
    expect(new Set(ids).size).toBe(ids.length);
    for (const id of ['BQ-POST-COMPLETE-01', 'BQ-POST-COMPLETE-02', 'BQ-POST-TRACE-01', 'BQ-POST-TRACE-03', 'BQ-POST-INV-01', 'BQ-AIB-PATH-01', 'BQ-AIB-RESID-01']) expect(row(rows, id).status, id).toBe('pass');
    // provenance gaps known on main: detail, AIB and memo files carry no git commit
    expect(row(rows, 'BQ-PROV-AIB-01').status).toBe('warn');
  });
});
