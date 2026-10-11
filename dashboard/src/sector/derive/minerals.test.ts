// Mineral derivations (spec_sector.md 4.5) on the TS fixture and, when present, builder A's synthetic
// bundle: index rows from meta.summary (projections excluded), the technologies that use a mineral,
// IEA demand by technology, share lines with projections flagged, the share-vs-share scatter (no
// point invented at 0 for a value listed without a figure) and the commodity of a mineral.

import { describe, expect, it } from 'vitest';
import { buildSectorFixture, fixtureGeo, fixtureItem } from '../__fixtures__/build.ts';
import { haveSynthetic, readSyntheticChunk } from '../__fixtures__/__tests__/synthetic.ts';
import { refSizes, validateChunk } from '../contract.ts';
import { factsIndexOf } from '../data/facts.ts';
import { modelOf } from '../data/model.ts';
import {
  commodityOf, demandByTech, MINE_VAR, mineralIndexRows, MINING_VAR, observedSummary, REFINING_VAR, RESERVES_VAR, shareLines, shareScatter, summaryYears, techsOfMineral,
} from './minerals.ts';

describe('minerals on the TS fixture', () => {
  const fx = buildSectorFixture();
  const model = modelOf(validateChunk('sector-meta', fx.chunks['sector-meta']));
  const facts = factsIndexOf(validateChunk('sector-facts', fx.chunks['sector-facts']));
  const li = model.items[fixtureItem('mineral', 'lithium')];
  const v = (id: string) => model.varById.get(id)!;

  it('index rows: USGS mine production, IEA refining at the observed year, technologies by share', () => {
    const rows = mineralIndexRows(model, 'members');
    expect(rows.map((r) => r.item.slug)).toEqual(['lithium', 'graphite']);
    const r = rows[0];
    expect(r.refining?.year).toBe(2025);
    expect(r.refining?.top1_geo).toBe(fixtureGeo('CHN'));
    expect(r.mining?.year).toBe(2025);
    expect(r.mine?.year).toBe(2025);
    expect(r.demand).toBe(165);
    // links: batteries-upstream 0.62, electric-vehicles-upstream 0.30; demand shares override (EV 120/165, batteries 40/165)
    expect(r.techs.map((t) => t.slug)).toEqual(['electric-vehicles', 'batteries']);
    expect(r.techs[0].share).toBeCloseTo(120 / 165, 12);
  });

  it('observedSummary never returns a projected year; summaryYears lists them all', () => {
    expect(observedSummary(model, v(MINING_VAR), li, 'members')?.year).toBe(2025);
    expect(summaryYears(model, v(REFINING_VAR), li, 'members')).toEqual([2025, 2030, 2035, 2040]);
    expect(observedSummary(model, null, li, 'members')).toBeNull();
  });

  it('demand by technology: observed vs the latest STEPS year, shares of the observed total', () => {
    const { rows, total } = demandByTech(model, li);
    expect(rows.map((r) => r.tech)).toEqual(['electric-vehicles', 'batteries']);
    expect(rows[0]).toMatchObject({ observed: 120, observedYear: 2025, projected: 520, projectedYear: 2040, scenario: 'STEPS' });
    expect(rows[0].share).toBeCloseTo(120 / 165, 12);
    expect(total).toMatchObject({ observed: 165, projected: 690 });
    const gr = model.items[fixtureItem('mineral', 'graphite')];
    expect(demandByTech(model, gr).total).toBeNull();
  });

  it('share lines: top holders of the first and last year plus China and the highlight; projections flagged', () => {
    const usa = model.geos[fixtureGeo('USA')];
    const lines = shareLines(model, facts, v(MINING_VAR), li, 'members', { top: 2, highlight: usa });
    const ids = lines.map((l) => l.id).sort();
    expect(ids).toEqual(['BRA', 'CHN', 'USA']);
    const chn = lines.find((l) => l.id === 'CHN')!;
    expect(chn.group).toBe('china');
    expect(lines.find((l) => l.id === 'BRA')!.group).toBe('neutral');
    expect(lines.find((l) => l.id === 'USA')!.group).toBe('core');
    expect(chn.points.map((p) => p.x)).toEqual([2025, 2030, 2035, 2040]);
    expect(chn.points.map((p) => !!p.projection)).toEqual([false, true, true, true]);
    const w25 = facts.world(v(MINING_VAR).idx, li.idx, 2025)!.value;
    expect(chn.points[0].y).toBeCloseTo(60 / w25, 12);
  });

  it('share scatter: both shares of their source world totals; a value without a figure is never plotted', () => {
    const prod = v(MINE_VAR);
    const res = v(RESERVES_VAR);
    const s = shareScatter(model, facts, prod, res, li, 2025, 2025, 'members');
    const wp = facts.world(prod.idx, li.idx, 2025)!.value;
    const wr = facts.world(res.idx, li.idx, 2025)!.value;
    const bra = s.points.find((p) => p.code === 'BRA')!;
    expect(bra.x).toBeCloseTo(42 / wp, 12);
    expect(bra.y).toBeCloseTo(3000 / wr, 12);
    expect(s.points.find((p) => p.code === 'RUS')).toMatchObject({ x: 0, y: 0 });
    expect(s.points.some((p) => p.code === 'IDN')).toBe(false);
    expect(s.points.find((p) => p.code === 'CHN')?.group).toBe('china');
  });

  it('commodity of a mineral: same slug, else none; a technology without a demand total keeps no share', () => {
    expect(commodityOf(model, li)).toBeNull();
    // graphite: link batteries-upstream 0.9; EV appears only in demand rows and there is no total row
    const t = techsOfMineral(model, model.items[fixtureItem('mineral', 'graphite')]);
    expect(t.map((x) => x.slug)).toEqual(['batteries', 'electric-vehicles']);
    expect(t[1].share).toBeNull();
  });
});

describe.skipIf(!haveSynthetic())('minerals on the R synthetic bundle', () => {
  it('index rows, the commodity of lithium and scatter points agree with the bundle', () => {
    const meta = validateChunk('sector-meta', readSyntheticChunk('sector-meta')!);
    const model = modelOf(meta);
    const facts = factsIndexOf(validateChunk('sector-facts', readSyntheticChunk('sector-facts')!, refSizes(meta)));
    const rows = mineralIndexRows(model, 'members');
    expect(rows.length).toBeGreaterThan(0);
    const li = model.item('mineral', 'lithium')!;
    const r = rows.find((x) => x.item.slug === 'lithium')!;
    for (const s of [r.mine, r.mining, r.refining, r.reserves]) if (s) expect(model.vars[s.var].projection_from === null || s.year < (model.vars[s.var].projection_from as number)).toBe(true);
    expect(commodityOf(model, li)?.axis).toBe('commodity');
    const prod = model.varById.get(MINE_VAR)!;
    const res = model.varById.get(RESERVES_VAR)!;
    const yp = facts.latestYear(prod.idx, li.idx)!;
    const yr = facts.latestYear(res.idx, li.idx)!;
    const s = shareScatter(model, facts, prod, res, li, yp, yr, 'members');
    for (const p of s.points) {
      expect(p.x).toBeGreaterThanOrEqual(0);
      expect(p.y).toBeGreaterThanOrEqual(0);
      expect(Number.isFinite(p.x) && Number.isFinite(p.y)).toBe(true);
    }
    expect(demandByTech(model, li).rows.length).toBeGreaterThan(0);
  });
});
