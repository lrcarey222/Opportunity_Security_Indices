// Pure derivations behind the views (spec_react.md 8.3: one pure function per chart, unit-tested):
// Show-mode values, colour domains, regions, network comparisons, chart helpers, formats and CSV.

import { describe, expect, it } from 'vitest';
import { cellRows, colorDomain, withShown, type MetricRow } from './Overview/derive.ts';
import { fitDomain, median, regionOf, REGIONS } from './Space/derive.ts';
import { compareSpecs, designGap, hhi, type SpecRow } from './Network/derive.ts';
import { declutter, fitLabel, labelMargin, symDomain } from '../charts/theme.ts';
import { fmtDelta, fmtNum, fmtPct, fmtRankChange, fmtSigned, stageSlug } from '../lib/format.ts';
import { rowsToCsv, tableToCsv, withBom } from '../lib/csv.ts';
import type { Table } from '../engine/types.ts';

type Base = Omit<MetricRow, 'shown' | 'shown_ok'>;
const row = (p: Partial<Base>): Base => ({
  iso3c: 'AAA', Country: 'A', tech: 'Batteries', supply_chain: 'Midstream', value: 0.5, value_base: 0.4, delta: 0.1, ok: true, ok_base: true, reason: null, ...p,
});

describe('Overview: Show modes and colour domains', () => {
  it('picks the scenario, the paper or the change, with its validity', () => {
    const rows = [row({}), row({ iso3c: 'BBB', ok_base: false }), row({ iso3c: 'CCC', ok: false, reason: null })];
    const sc = withShown(rows, 'scenario');
    expect(sc.map((r) => r.shown)).toEqual([0.5, 0.5, 0.5]);
    expect(sc.map((r) => r.shown_ok)).toEqual([true, true, false]);
    expect(sc[2].reason).toBe('not scored');
    const ba = withShown(rows, 'baseline');
    expect(ba.map((r) => r.shown)).toEqual([0.4, 0.4, 0.4]);
    expect(ba.map((r) => r.shown_ok)).toEqual([true, false, true]);
    const ch = withShown(rows, 'change');
    expect(ch.map((r) => r.shown)).toEqual([0.1, 0.1, 0.1]);
    // a change is valid only where both sides are
    expect(ch.map((r) => r.shown_ok)).toEqual([true, false, false]);
  });

  it('bounded indices use [0, 1]; ranks reverse; changes are symmetric', () => {
    const rows = withShown([row({ value: 3, value_base: 5, delta: 2 }), row({ value: 7, value_base: 6, delta: -1 })], 'scenario');
    expect(colorDomain(rows, 'es', 'scenario')).toEqual({ kind: 'seq', lo: 0, hi: 1, reverse: false });
    expect(colorDomain(rows, 'si_rank', 'scenario')).toEqual({ kind: 'seq', lo: 1, hi: 7, reverse: true });
    const ch = withShown([row({ delta: -0.3 }), row({ delta: 0.1 })], 'change');
    expect(colorDomain(ch, 'es', 'change')).toEqual({ kind: 'div', lo: -0.3, hi: 0.3, reverse: false });
    // nothing moved: a unit domain, never a zero-width one
    expect(colorDomain(withShown([row({ delta: 0 })], 'change'), 'es', 'change').hi).toBe(1);
  });

  it('selects one technology and stage (PSI has one row per partner)', () => {
    const rows = withShown([row({}), row({ tech: 'Solar' }), row({ supply_chain: 'Upstream' })], 'scenario');
    expect(cellRows(rows, 'es', 'Batteries', 'Midstream')).toHaveLength(1);
    expect(cellRows(rows, 'psi', 'Batteries', 'Midstream')).toHaveLength(3);
  });
});

describe('Space: regions and domains', () => {
  it('splits at the medians in the paper orientation (x = ES, y = EO)', () => {
    expect(regionOf(0.1, 0.9, 0.5, 0.5)).toBe(REGIONS[0]);
    expect(regionOf(0.9, 0.9, 0.5, 0.5)).toBe(REGIONS[1]);
    expect(regionOf(0.1, 0.1, 0.5, 0.5)).toBe(REGIONS[2]);
    expect(regionOf(0.9, 0.1, 0.5, 0.5)).toBe(REGIONS[3]);
    expect(regionOf(0.5, 0.5, 0.5, 0.5)).toBe(REGIONS[1]);
    expect(regionOf(null, 0.5, 0.5, 0.5)).toBeNull();
  });

  it('medians and fitted domains', () => {
    expect(median([3, 1, 2])).toBe(2);
    expect(median([4, 1, 2, 3])).toBe(2.5);
    expect(Number.isNaN(median([]))).toBe(true);
    const [lo, hi] = fitDomain([0.22, 0.31, 0.78, null]);
    expect(lo).toBeCloseTo(0.15, 12);
    expect(hi).toBeCloseTo(0.85, 12);
    expect(fitDomain([])).toEqual([0, 1]);
    const [a, b] = fitDomain([0.5, 0.51]);
    expect(b - a).toBeGreaterThanOrEqual(0.2 - 1e-12);
    expect(fitDomain([0, 1])).toEqual([0, 1]);
  });
});

describe('Network: comparisons', () => {
  const spec = (node: string, share: number, tech = 'Batteries', supply_chain = 'Midstream'): SpecRow => ({ tech, supply_chain, node, share, selected: share > 0, score: null });

  it('compares producer sets, concentration and shares per stage', () => {
    const cur = [spec('USA', 0.5), spec('CAN', 0.3), spec('MEX', 0.2), spec('USA', 1, 'Solar')];
    const ref = [spec('USA', 0.6), spec('CAN', 0.4), spec('USA', 1, 'Solar')];
    const { stage, node } = compareSpecs(cur, ref);
    const b = stage.find((s) => s.tech === 'Batteries')!;
    expect(b.producers_added).toBe('MEX');
    expect(b.producers_removed).toBe('');
    expect(b.jaccard).toBeCloseTo(2 / 3, 12);
    expect(b.hhi).toBeCloseTo(0.25 + 0.09 + 0.04, 12);
    expect(b.hhi_ref).toBeCloseTo(0.36 + 0.16, 12);
    expect(b.max_abs_share_delta).toBeCloseTo(0.2, 12);
    expect(b.changed).toBe(true);
    const s = stage.find((x) => x.tech === 'Solar')!;
    expect(s.changed).toBe(false);
    expect(s.jaccard).toBe(1);
    expect(node.filter((r) => r.tech === 'Batteries').map((r) => r.node).sort()).toEqual(['CAN', 'MEX', 'USA']);
    const gap = designGap(stage, node)!;
    expect(gap.medianJaccard).toBeCloseTo((2 / 3 + 1) / 2, 12);
    expect(gap.meanAbs).toBeCloseTo((0.1 + 0.1 + 0.2 + 0) / 4, 12);
    expect(hhi([0.5, 0.5])).toBe(0.5);
  });
});

describe('chart helpers', () => {
  it('symmetric domains have a floor', () => {
    expect(symDomain([0, 0, null])).toEqual([-0.01, 0.01]);
    expect(symDomain([-0.3, 0.1])).toEqual([-0.3, 0.3]);
  });

  it('label margins and truncation', () => {
    expect(labelMargin(['ab'], { min: 40 })).toBe(40);
    expect(labelMargin(['x'.repeat(100)], { max: 200 })).toBe(200);
    expect(fitLabel('Democratic Republic of Congo', 60)).toMatch(/…$/);
    expect(fitLabel('Peru', 60)).toBe('Peru');
  });

  it('declutter keeps non-overlapping labels in priority order', () => {
    const boxes = [
      { x: 0, y: 0, w: 20, h: 10 },
      { x: 5, y: 2, w: 20, h: 10 },
      { x: 100, y: 0, w: 20, h: 10 },
    ];
    expect([...declutter(boxes)].sort()).toEqual([0, 2]);
    expect([...declutter(boxes, [1, 0, 2])].sort()).toEqual([1, 2]);
  });
});

describe('formats', () => {
  it('numbers, shares and changes use a true minus and text direction', () => {
    expect(fmtNum(-0.1234)).toBe('−0.123');
    expect(fmtNum(null)).toBe('–');
    expect(fmtPct(0.1234)).toBe('12.3%');
    expect(fmtSigned(0.0123)).toBe('+0.012');
    expect(fmtSigned(-0.0004)).toBe('0.000');
    expect(fmtDelta(-0.237, 1, true)).toBe('↓ −23.7 pp');
    expect(fmtDelta(0.0001, 1, true)).toBe('no change');
    expect(fmtRankChange(2)).toBe('↑ up 2');
    expect(fmtRankChange(-1)).toBe('↓ down 1');
    expect(fmtRankChange(0)).toBe('no change');
    expect(stageSlug('Electric Vehicles', 'Midstream')).toBe('electric-vehicles-midstream');
  });
});

describe('CSV', () => {
  it('writes R column names, NA as empty, quotes where needed, CRLF, optional BOM', () => {
    const t: Table = {
      nrow: 2,
      cols: {
        Country: ['Korea, Rep.', null],
        value: new Float64Array([0.1, NaN]),
        n: new Int32Array([3, -2147483648]),
      },
    } as unknown as Table;
    expect(tableToCsv(t)).toBe('Country,value,n\r\n"Korea, Rep.",0.1,3\r\n,,\r\n');
    expect(rowsToCsv([{ a: 1, b: true, c: null, d: 'x"y' }])).toBe('a,b,c,d\r\n1,TRUE,,"x""y"\r\n');
    expect(withBom('a').charCodeAt(0)).toBe(0xfeff);
  });
});
