// engine.cellPartners(): one reporter's partners in one sector, scenario and baseline, ranked among the
// sovereign economies scored in that sector (the Partners view's sector choice).

import { describe, expect, it } from 'vitest';
import { cellPartners } from '../partner.ts';
import { toRows } from '../table.ts';
import type { Bundle, PartnerCells } from '../types.ts';

const U = ['AAA', 'BBB', 'CCC', 'DDD'];
const C = 2;
const bundle = { partner: { U, sovereign: Uint8Array.from([1, 1, 1, 0]), cells: { nrow: C, cols: {} } } } as unknown as Pick<Bundle, 'partner'>;

/** [U, U, C] column-major from a function of (reporter, partner, cell). */
function arr(f: (r: number, p: number, c: number) => number): Float64Array {
  const n = U.length;
  const a = new Float64Array(n * n * C);
  for (let c = 0; c < C; c++) for (let p = 0; p < n; p++) for (let r = 0; r < n; r++) a[r + n * (p + n * c)] = f(r, p, c);
  return a;
}

describe('cellPartners', () => {
  const fs = arr((r, p, c) => (r === p ? NaN : c === 1 ? [0.2, 0.9, 0.9, 0.99][p] : 0.5));
  const po = arr((r, p, c) => (r === p || p === 1 ? NaN : c === 1 ? [0.1, NaN, 0.3, 0.7][p] : 0.4));
  const base: PartnerCells = { fs_index: arr((r, p) => (r === p ? NaN : 0.6)), po_index: arr(() => NaN) };
  const cells: PartnerCells = { fs_index: fs, po_index: po };

  it('reads the reporter row of one cell and ranks sovereign partners with ties at the lowest rank', () => {
    const rows = toRows(cellPartners(bundle, cells, base, 'AAA', 1));
    expect(rows.map((r) => r.partner_iso)).toEqual(['BBB', 'CCC', 'DDD']);
    const by = Object.fromEntries(rows.map((r) => [r.partner_iso, r]));
    expect(by.BBB.fs).toBe(0.9);
    expect(by.BBB.fs_rank).toBe(1);
    expect(by.CCC.fs_rank).toBe(1);
    // the non-sovereign economy keeps its value but gets no rank, and does not push others down
    expect(by.DDD.fs).toBe(0.99);
    expect(by.DDD.fs_rank).toBeNull();
    expect(by.DDD.sovereign).toBe(false);
    expect(by.BBB.po).toBeNull();
    expect(by.BBB.po_rank).toBeNull();
    expect(by.CCC.po_rank).toBe(1);
    expect(by.CCC.fs_base).toBe(0.6);
    expect(by.CCC.fs_rank_base).toBe(1);
    expect(by.CCC.po_base).toBeNull();
  });

  it('works with the baseline cells as both runs and rejects an unknown reporter or sector', () => {
    const t = cellPartners(bundle, base, base, 'CCC', 0);
    expect(t.nrow).toBe(3);
    expect(() => cellPartners(bundle, cells, base, 'ZZZ', 0)).toThrow();
    expect(() => cellPartners(bundle, cells, base, 'AAA', 2)).toThrow();
  });
});
