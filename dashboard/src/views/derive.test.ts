// Pure derivations behind the views (spec_react.md 8.3: one pure function per chart, unit-tested):
// Show-mode values, colour domains, regions, chart helpers, formats and CSV.

import { describe, expect, it } from 'vitest';
import { cellRows, scaleValues, sideWords, triOptions, withShown, type MetricRow } from './Overview/derive.ts';
import { triScale } from '../charts/scale.ts';
import { fitDomain, median, regionOf, REGIONS } from './Space/derive.ts';
import { declutter, fitLabel, labelMargin, symDomain } from '../charts/theme.ts';
import { fmtDelta, fmtNum, fmtPct, fmtRankChange, fmtSigned, stageSlug } from '../lib/format.ts';
import { rowsToCsv, tableToCsv, withBom } from '../lib/csv.ts';
import type { Table } from '../engine/types.ts';

type Base = Omit<MetricRow, 'shown' | 'shown_ok'>;
const row = (p: Partial<Base>): Base => ({
  iso3c: 'AAA', Country: 'A', tech: 'Batteries', supply_chain: 'Refining', value: 0.5, value_base: 0.4, delta: 0.1, ok: true, ok_base: true, reason: null, ...p,
});

describe('Overview: Show modes and colour domains', () => {
  it('picks the scenario, the NEIS baseline or the change, with its validity', () => {
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

  it('three-colour options: median centre, ranks reversed (rank 1 blue), changes centred on zero', () => {
    expect(triOptions('es', 'scenario')).toEqual({ center: 'median', reverse: false });
    expect(triOptions('si_rank', 'baseline')).toEqual({ center: 'median', reverse: true });
    expect(triOptions('si_rank', 'change')).toEqual({ center: 'zero', reverse: false });
    expect(triOptions('psi', 'change')).toEqual({ center: 'zero', reverse: false });
    // rank 1 is the blue end, the largest rank the orange end
    const s = triScale([1, 2, 3, 4, 5], triOptions('si_rank', 'scenario'));
    expect(s.pos(1)).toBe(1);
    expect(s.pos(5)).toBe(0);
    expect(s.pos(3)).toBe(0.5);
  });

  it('scale values follow the rankable switch; side words name the median', () => {
    const rows = withShown([row({}), row({ iso3c: 'BBB', ok: false, value: 0.9 }), row({ iso3c: 'CCC', value: null })], 'scenario');
    expect(scaleValues(rows, true)).toEqual([0.5]);
    expect(scaleValues(rows, false)).toEqual([0.5, 0.9]);
    expect(sideWords('above', 'es', 'scenario', '0.42', 'economies shown')).toBe('above the median of the economies shown (0.42)');
    expect(sideWords('median', 'eo', 'baseline', '0.42', 'economies shown')).toBe('at the median of the economies shown (0.42)');
    expect(sideWords('below', 'si_rank', 'scenario', '7', 'economies shown')).toMatch(/^nearer the top of its own list/);
    expect(sideWords('above', 'si_rank', 'change', '0', 'economies shown')).toBe('moved up its own list');
    expect(sideWords('below', 'es', 'change', '0', 'economies shown')).toBe('fell against the baseline');
    expect(sideWords('above', 'eo', 'change', '0', 'economies shown')).toBe('rose against the baseline');
    expect(sideWords(null, 'es', 'scenario', '0', 'x')).toBeNull();
  });

  it('selects one technology and stage (PSI has one row per partner)', () => {
    const rows = withShown([row({}), row({ tech: 'Solar' }), row({ supply_chain: 'Extraction' })], 'scenario');
    expect(cellRows(rows, 'es', 'Batteries', 'Refining')).toHaveLength(1);
    expect(cellRows(rows, 'es', 'Batteries', 'Extraction')).toHaveLength(1);
    expect(cellRows(rows, 'psi', 'Batteries', 'Refining')).toHaveLength(3);
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
    // the pcell-<slug> chunk names of the six-stage Strategic bundle
    expect(stageSlug('Electric Vehicles', 'Assembly')).toBe('electric-vehicles-assembly');
    expect(stageSlug('Batteries', 'Refining')).toBe('batteries-refining');
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
