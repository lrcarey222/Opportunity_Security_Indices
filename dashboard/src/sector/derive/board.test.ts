// Concentration across sectors (spec_sector.md 4.7) on the TS fixture and builder A's synthetic bundle:
// board columns only where data exist, in chain order; technology tiles resolve to the stage cell or its
// linked fuel; mineral sub-rows; tiles are latest OBSERVED years; the family split groups by stage and
// sorts by the lens; trends pair 2020 with 2024 (trade) and the base year with the last projection.

import { describe, expect, it } from 'vitest';
import { buildSectorFixture } from '../__fixtures__/build.ts';
import { haveSynthetic, readSyntheticChunk } from '../__fixtures__/__tests__/synthetic.ts';
import { validateChunk } from '../contract.ts';
import { modelOf } from '../data/model.ts';
import { BOARD_COLS, boardTile, concentrationBoard, familyChoices, familySplit, latestSummaries, mineralsOfTech, mineralTop3Trend, tradeHhiTrend } from './board.ts';

describe('concentration board on the TS fixture', () => {
  const model = modelOf(validateChunk('sector-meta', buildSectorFixture().chunks['sector-meta']));

  it('columns with data only, in chain order', () => {
    const b = concentrationBoard(model, 'members');
    const order = BOARD_COLS.map((c) => c.key);
    const keys = b.cols.map((c) => c.key);
    expect(keys).toEqual(order.filter((k) => keys.includes(k)));
    expect(keys).toContain('exports_mid');
    expect(keys).toContain('refining');
  });

  it('technology tiles: Batteries exports mid on the cell; Oil mining on fuel:oil; minerals as sub-rows', () => {
    const b = concentrationBoard(model, 'members');
    expect(boardTile(b, 'batteries', 'exports_mid')?.v.id).toBe('trade.exports');
    const oil = boardTile(b, 'oil', 'mining');
    expect(oil?.v.id).toBe('production.fuels');
    expect(oil?.item.slug).toBe('oil');
    expect(oil?.path).toBe('/sector/oil/upstream');
    const bat = b.rows.find((r) => r.key === 'batteries')!;
    expect(bat.children.map((c) => c.mineral?.slug)).toEqual(['graphite', 'lithium']);
    const ref = boardTile(b, 'batteries:lithium', 'refining')!;
    expect(ref.s.year).toBe(2025);
    expect(ref.path).toBe('/sector/minerals/lithium');
    expect(ref.hash).toBe('mining-refining');
    expect(boardTile(b, 'geothermal', 'exports_mid')).toBeNull();
  });

  it('minerals of a technology: heaviest link first, no duplicates', () => {
    expect(mineralsOfTech(model, 'batteries').map((m) => m.slug)).toEqual(['graphite', 'lithium']);
    expect(mineralsOfTech(model, 'electric-vehicles').map((m) => m.slug)).toEqual(['lithium']);
  });

  it('family split: stage panels, sorted by the lens, minerals in their own panel', () => {
    const rows = familySplit(model, 'primary', 'china', 'members');
    const sections = [...new Set(rows.map((r) => r.section))];
    expect(sections).toEqual(['upstream', 'midstream', 'downstream', 'minerals'].filter((s) => sections.includes(s)));
    for (const s of sections) {
      const xs = rows.filter((r) => r.section === s).map((r) => r.lens).filter(Number.isFinite);
      expect(xs).toEqual([...xs].sort((a, z) => z - a));
    }
    expect(rows.some((r) => r.section === 'minerals' && r.words === 'refining')).toBe(true);
    expect(familyChoices(model, 'members')[0]).toBe('primary');
  });

  it('latest summaries skip projections; trends pair the right years', () => {
    for (const s of latestSummaries(model, 'members')) {
      const v = model.vars[s.var];
      expect(v.projection_from === null || s.year < v.projection_from).toBe(true);
    }
    const t = tradeHhiTrend(model, 'members');
    expect(t.length).toBeGreaterThan(0);
    for (const r of t) expect([r.a.year, r.b.year]).toEqual([2020, 2024]);
    const m = mineralTop3Trend(model, 'members');
    expect(m.length).toBeGreaterThan(0);
    for (const r of m) {
      expect(r.b.year).toBe(2025);
      expect(r.a.year).toBe(2040);
    }
  });
});

describe.skipIf(!haveSynthetic())('concentration board on the R synthetic bundle', () => {
  it('builds tiles for both EU modes, every tile an observed year with a world total', () => {
    const model = modelOf(validateChunk('sector-meta', readSyntheticChunk('sector-meta')!));
    for (const eu of ['members', 'bloc'] as const) {
      const b = concentrationBoard(model, eu);
      expect(b.tiles.size).toBeGreaterThan(0);
      for (const t of b.tiles.values()) {
        expect(Number.isFinite(t.s.W)).toBe(true);
        expect(t.v.projection_from === null || t.s.year < t.v.projection_from).toBe(true);
      }
    }
    expect(boardTile(concentrationBoard(model, 'members'), 'oil', 'mining')?.item.slug).toBe('oil');
  });
});
