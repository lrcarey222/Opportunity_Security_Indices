// The stage-panel grid of the Strategic views' faceted charts (views/stageFacets.ts, 2026-10-29): six
// panels in chain order, side by side on a wide screen and wrapped on a narrow one, with exact gaps.

import { describe, expect, it } from 'vitest';
import { SUPPLY_STAGES } from '../lib/stages.ts';
import { stageCols, stageFacetLayout, stageGrid } from './stageFacets.ts';

const SIX = [...SUPPLY_STAGES];

describe('stage panels', () => {
  it('all six side by side when each panel fits, else 3, 2 or 1 a row', () => {
    expect(stageCols(6, 900, 140)).toBe(6);
    expect(stageCols(6, 600, 140)).toBe(3);
    expect(stageCols(6, 300, 140)).toBe(2);
    expect(stageCols(6, 200, 140)).toBe(1);
    // a chart that keeps three a row (and scrolls instead) never goes below its smallest choice
    expect(stageCols(6, 200, 140, [6, 3])).toBe(3);
    expect(stageCols(1, 50, 140)).toBe(1);
    // fewer stages than a candidate: never more columns than stages
    expect(stageCols(2, 900, 140)).toBe(2);
  });

  it('lays the stages out left to right, then down, in the given (chain) order', () => {
    const g = stageGrid(SIX, 2);
    expect([g.cols, g.rows]).toEqual([2, 3]);
    expect(g.panels.map((p) => [p.stage, p.col, p.row])).toEqual([
      ['Extraction', 0, 0], ['Refining', 1, 0],
      ['Components', 0, 1], ['Assembly', 1, 1],
      ['Deployment', 0, 2], ['Recovery', 1, 2],
    ]);
    expect(g.at('Recovery')).toMatchObject({ col: 1, row: 2, label: 'Recovery' });
    expect(g.at('Midstream')).toBeUndefined();
    expect(stageGrid(SIX, 6).rows).toBe(1);
    expect(stageGrid(SIX, 3).panels.map((p) => p.row)).toEqual([0, 0, 0, 1, 1, 1]);
  });

  it('sizes the panels, the gaps and the height exactly (band step = panel + gap, no outer padding)', () => {
    const base = { marginLeft: 100, marginRight: 10, marginTop: 30, marginBottom: 40, panelHeight: 200, minPanel: 120, gapX: 10, gapY: 50 };
    const wide = stageFacetLayout(SIX, { ...base, width: 1000 });
    expect(wide.grid.cols).toBe(6);
    expect(wide.height).toBe(30 + 200 + 40);
    expect(wide.panelWidth).toBeCloseTo((890 - 50) / 6, 10);
    expect(wide.fx.domain).toEqual([0, 1, 2, 3, 4, 5]);
    expect(wide.fy.domain).toEqual([0]);
    expect(wide.fy.paddingInner).toBe(0);
    // the band arithmetic: n * step - gap = inner width
    const step = wide.panelWidth + 10;
    expect(6 * step - 10).toBeCloseTo(890, 10);
    expect(wide.fx.paddingInner).toBeCloseTo(10 / step, 12);
    const phone = stageFacetLayout(SIX, { ...base, width: 360 });
    expect([phone.grid.cols, phone.grid.rows]).toEqual([2, 3]);
    expect(phone.height).toBe(30 + 3 * 200 + 2 * 50 + 40);
    expect(phone.fy.paddingInner).toBeCloseTo(50 / 250, 12);
    expect(phone.facet.fx({ stage: 'Assembly' })).toBe(1);
    expect(phone.facet.fy({ stage: 'Assembly' })).toBe(1);
    expect(phone.facet.fx({ stage: 'Upstream' })).toBeUndefined();
  });
});
