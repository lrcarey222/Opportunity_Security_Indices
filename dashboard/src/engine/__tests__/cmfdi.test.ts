// Friendshore's CM-FDI parts in the TypeScript engine (commit 78be8b5; R/dashboard/dash_partner.R): on
// the committed synthetic bundle s1 (the Components and Assembly cells of Solar and Wind are CM-FDI
// cells with the same projects, as the pipeline copies them), the Friendshore cells equal R's bit
// for bit, the ownership component moves only the CM-FDI cells, the CM-FDI tie replaces the IMF
// position there, and the hawkishness dial scales the ownership weight like the alignment weight.
import { describe, expect, it } from 'vitest';
import { fileURLToPath } from 'node:url';
import { join } from 'node:path';
import { get, loadFixture, loadWebBundle } from './parity/harness.ts';
import { fsCells, fsOutboundSource, partnerDecompose, poCells } from '../partner.ts';
import { makeParams } from '../params.ts';
import type { NDArray, Params } from '../types.ts';

const dir = fileURLToPath(new URL('../__fixtures__/synthetic/s1/', import.meta.url));
const bundle = loadWebBundle(join(dir, 'data'));
const pa = bundle.partner!;
const U = pa.U.length;
const C = pa.cells.nrow;
const UU = U * U;
const cm = fsOutboundSource(pa.fs, C).flatMap((s, c) => (s === 'cm_fdi' ? [c] : []));

const withFsWeights = (p: Params, w: Record<string, number>): Params => ({ ...p, partner: { ...p.partner, fs_weights: { ...p.partner.fs_weights, ...w } } });

describe('Friendshore CM-FDI parts', () => {
  it('the bundle carries CM-FDI cells, ties and an undefined exposure', () => {
    const tech = pa.cells.cols.tech as string[];
    const stage = pa.cells.cols.supply_chain as string[];
    expect(cm.map((c) => `${tech[c]} - ${stage[c]}`)).toEqual(['Solar - Components', 'Solar - Assembly', 'Wind - Components', 'Wind - Assembly']);
    expect(pa.fs.outbound_cmfdi?.shape).toEqual([U, U, 4]);
    // Components and Assembly carry the same projects: one tie and one exposure per technology, equal
    // wherever both stages have the dyad (each cell keeps its own dyads)
    const sameWhereBoth = (a: (i: number) => number, b: (i: number) => number, n: number) => {
      let both = 0;
      for (let i = 0; i < n; i++) {
        if (a(i) !== a(i) || b(i) !== b(i)) continue;
        both++;
        expect(a(i)).toBe(b(i));
      }
      expect(both).toBeGreaterThan(0);
    };
    const tie = (pa.fs.outbound_cmfdi as NDArray<Float64Array>).data;
    for (const k of [0, 2]) sameWhereBoth((i) => tie[i + UU * k], (i) => tie[i + UU * (k + 1)], UU);
    const cpe = pa.fs.cpe as NDArray<Float64Array>;
    for (const k of [0, 2]) sameWhereBoth((p) => cpe.data[p + U * cm[k]], (p) => cpe.data[p + U * cm[k + 1]], U);
    expect(cpe.shape).toEqual([U, C]);
    const jpn = pa.U.indexOf('JPN');
    const chn = pa.U.indexOf('CHN');
    for (const c of cm) {
      expect(cpe.data[jpn + U * c]).toBeNaN();
      expect(cpe.data[chn + U * c]).toBe(1);
    }
    for (let c = 0; c < C; c++) if (!cm.includes(c)) for (let p = 0; p < U; p++) expect(cpe.data[p + U * c]).toBeNaN();
  });

  it('Friendshore cells equal R bit for bit at the baseline and under hawkishness 100', () => {
    for (const [id, spec] of [['S00', null], ['S02', { dials: { hawkishness: 100 } }]] as const) {
      const params = makeParams(spec, bundle);
      const want = (get(loadFixture(dir, `${id}.osb`), 'L4.fs_index') as NDArray<Float64Array>).data;
      const got = fsCells(bundle, params);
      expect(got.length).toBe(want.length);
      let bad = 0;
      for (let i = 0; i < got.length; i++) if (!Object.is(got[i], want[i])) bad++;
      expect(bad).toBe(0);
    }
  });

  it('without the ownership component only the CM-FDI cells change', () => {
    const params = makeParams(null, bundle);
    const a = fsCells(bundle, params);
    const b = fsCells(bundle, withFsWeights(params, { chinese_parent_exposure: 0 }));
    const changed = new Set<number>();
    for (let c = 0; c < C; c++) for (let i = 0; i < UU; i++) if (!Object.is(a[i + UU * c], b[i + UU * c])) changed.add(c);
    expect([...changed].sort()).toEqual(cm);
    // the Opportunity cells do not read CM-FDI
    expect(poCells(bundle, params)).toEqual(poCells(bundle, withFsWeights(params, { chinese_parent_exposure: 0 })));
  });

  it('hawkishness scales the ownership weight like the alignment weight', () => {
    const base = makeParams(null, bundle);
    for (const h of [0, 25, 75, 100]) {
      const p = makeParams({ dials: { hawkishness: h } }, bundle);
      const m = p.m.H;
      expect(p.partner.fs_weights.chinese_parent_exposure).toBe(base.partner.fs_weights.chinese_parent_exposure * m);
      expect(p.partner.fs_weights.geo_alignment).toBe(base.partner.fs_weights.geo_alignment * m);
      expect(p.partner.fs_weights.outbound_index).toBe(base.partner.fs_weights.outbound_index);
    }
  });

  it('the decomposition reads the CM-FDI tie in the CM-FDI cells and scores ownership as 1 - share', () => {
    const params = makeParams(null, bundle);
    const cells = { fs_index: fsCells(bundle, params), po_index: poCells(bundle, params) };
    const r = pa.U.indexOf('BRA');
    const p = pa.U.indexOf('CHN');
    const d = partnerDecompose(bundle, params, cells, 'BRA', 'CHN');
    const ob = d.cols.fs_outbound_index as Float64Array;
    const src = d.cols.fs_outbound_source as string[];
    const tie = (pa.fs.outbound_cmfdi as NDArray<Float64Array>).data;
    cm.forEach((c, k) => {
      expect(src[c]).toBe('cm_fdi');
      expect(Object.is(ob[c], tie[r + U * p + UU * k])).toBe(true);
    });
    const own = d.cols.fs_chinese_parent_exposure as Float64Array;
    const ownShare = d.cols.fs_chinese_parent_exposure_share as Float64Array;
    const raw = d.cols.fsi_raw as Float64Array;
    for (const c of cm) {
      expect(own[c]).toBe(1); // CHN as a supplier: exposed
      expect(ownShare[c]).toBe(0); // contributes 1 - 1 = 0
    }
    // the contributions add up to the raw score
    const shareCols = Object.keys(d.cols).filter((k) => /^fs_.*_share$/.test(k));
    for (let c = 0; c < C; c++) {
      if (raw[c] !== raw[c]) continue;
      const tot = shareCols.reduce((s, k) => {
        const v = (d.cols[k] as Float64Array)[c];
        return v === v ? s + v : s;
      }, 0);
      expect(Math.abs(tot - raw[c])).toBeLessThan(1e-12);
    }
  });
});
