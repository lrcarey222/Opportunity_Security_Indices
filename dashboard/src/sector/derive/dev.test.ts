import { describe, expect, it } from 'vitest';
import type { SectorDevChunk } from '../contract.ts';
import { conduitDropped, devPartners, devSankey, devTotals, dipFlows, dipWindow, isDevFlowVar, odaFlows, odaYears, OTHER_FROM, OTHER_TO, positiveTotal } from './dev.ts';

// geo rows: 0 DEU, 1 JPN, 2 USA, 3 BRA, 4 IDN, 5 HKG, 6 CHN
const chunk = (): SectorDevChunk => ({
  'dev.oda': {
    nrow: 6,
    cols: {
      item: Int32Array.from([1, 1, 1, 1, 2, 2]),
      year: Int32Array.from([2023, 2023, 2024, 2024, 2024, 2024]),
      donor: Int32Array.from([0, 1, 0, 2, 0, 1]),
      recipient: Int32Array.from([3, 4, 3, 4, 3, 6]),
      value: Float64Array.from([10, 30, 12, 8, 5, -0.5]),
    },
  },
  'dev.dip': {
    nrow: 4,
    cols: {
      reporter: Int32Array.from([0, 0, 1, 5]),
      counterpart: Int32Array.from([5, 6, 2, 6]),
      value: Float64Array.from([3500, 21000, 120000, 160000]),
      year_min: Int32Array.from([2020, 2020, 2020, 2021]),
      year_max: Int32Array.from([2024, 2024, 2023, 2024]),
      n_years: Int32Array.from([5, 5, 4, 4]),
      conduit: ['counterpart', 'none', 'none', 'investor'],
    },
  },
  'dev.conduits': { nrow: 1, cols: { geo: Int32Array.from([5]), dej_top10: Uint8Array.from([1]) } },
});

describe('Development & finance flows (sector-dev)', () => {
  it('names the flow measures', () => {
    expect(isDevFlowVar('development.oda')).toBe(true);
    expect(isDevFlowVar('development.fdi_out')).toBe(true);
    expect(isDevFlowVar('development.oda_received')).toBe(false);
  });

  it('aid flows of an item, one year or every year summed', () => {
    const c = chunk();
    expect(odaYears(c, 1)).toEqual([2023, 2024]);
    expect(odaFlows(c, 1, 2024).map((f) => [f.from, f.to, f.v])).toEqual([[0, 3, 12], [2, 4, 8]]);
    const all = odaFlows(c, 1, null);
    expect(all.find((f) => f.from === 0 && f.to === 3)?.v).toBe(22);
    expect(all).toHaveLength(3);
  });

  it('positions: conduit centres kept and flagged, or dropped as Friendshore drops them', () => {
    const c = chunk();
    expect(dipFlows(c, false)).toHaveLength(4);
    expect(dipFlows(c, true).map((f) => f.to)).toEqual([6, 2]);
    expect(dipFlows(c, true, 'counterpart')).toHaveLength(3);
    expect(conduitDropped('investor', 'counterpart')).toBe(false);
    expect(conduitDropped('investor', 'both')).toBe(true);
    expect(dipWindow(c)).toBe('2020-2024');
  });

  it('Sankey links fold small sources and destinations; negatives stay out of the picture', () => {
    const c = chunk();
    const f = odaFlows(c, 2, null);
    expect(positiveTotal(f)).toBe(5);
    const s = devSankey(f, { topFrom: 1, topTo: 1, fold: 0 });
    expect(s.total).toBe(5);
    expect(s.links).toEqual([{ from: 0, to: 3, v: 5, share: 1, conduit: false }]);
    const d = devSankey(dipFlows(c, false), { topFrom: 1, topTo: 1, fold: 0 });
    expect(d.links[0]).toMatchObject({ from: 5, to: 6, conduit: true });
    expect(d.links[1]).toMatchObject({ from: OTHER_FROM, to: OTHER_TO, v: 123500, conduit: true });
    expect(d.links.some((l) => l.to === OTHER_TO)).toBe(true);
    expect(d.links.reduce((a, l) => a + l.share, 0)).toBeCloseTo(1, 12);
  });

  it('partners and totals of one economy', () => {
    const c = chunk();
    const p = devPartners(dipFlows(c, false), 0, 'out');
    expect(p.map((x) => [x.geo, x.conduit])).toEqual([[6, 'none'], [5, 'counterpart']]);
    expect(p[0].share).toBeCloseTo(21000 / 24500, 12);
    const t = devTotals(odaFlows(c, 1, null));
    expect(t.get(4)).toEqual({ geo: 4, out: 0, in: 38 });
  });
});
