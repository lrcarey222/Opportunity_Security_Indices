// The theme explorer's pure logic (spec_sector.md 4.6) on the TS fixture: layouts by native level,
// URL fallbacks for measure / item / year (projection years only when chosen), Show-as legality, the
// economies scope, the KPI strip, the result line, table rows with the roster's absentees, the
// across-sectors rows and the TRL fallback cells named in the caveat.

import { describe, expect, it } from 'vitest';
import { buildSectorFixture, fixtureItem } from '../__fixtures__/build.ts';
import { validateChunk } from '../contract.ts';
import { factsIndexOf } from '../data/facts.ts';
import { modelOf } from '../data/model.ts';
import { acrossRows, defaultItem, exploreRows, inScope, kpis, layoutBanner, layoutOf, pickItem, pickVar, pickYear, resultLine, shareLegal, themeVars } from './explore.ts';
import { slice } from './summary.ts';

const fx = buildSectorFixture();
const model = modelOf(validateChunk('sector-meta', fx.chunks['sector-meta']));
const facts = factsIndexOf(validateChunk('sector-facts', fx.chunks['sector-facts']));
const v = (id: string) => model.varById.get(id)!;

describe('explore: measures, items, years', () => {
  it('layouts by native level and the special forms', () => {
    expect(layoutOf(v('trade.exports'))).toBe('country');
    expect(layoutOf(v('trade.flow'))).toBe('dyad');
    expect(layoutOf(v('production.ev_region'))).toBe('region');
    expect(layoutOf(v('manufacturing.share_now'))).toBe('entity');
    expect(layoutOf(v('prices.latest'))).toBe('world');
    expect(layoutOf(v('technology.trl'))).toBe('cell_global');
    expect(layoutOf(v('policy.records'))).toBe('policy');
    expect(layoutOf(v('consumption.mineral_demand'))).toBe('mineral_demand');
    expect(layoutBanner(v('prices.latest'))).toBe('World prices: the same for every country.');
    expect(layoutBanner(v('trade.exports'))).toBeNull();
  });

  it('the measure falls back to the theme default; vars without data are not offered', () => {
    expect(pickVar(model, 'trade', null)?.id).toBe('trade.exports');
    expect(pickVar(model, 'trade', 'trade.rca')?.id).toBe('trade.rca');
    expect(pickVar(model, 'trade', 'prices.latest')?.id).toBe('trade.exports');
    expect(themeVars(model, 'trade').every((x) => model.availOf(x).length > 0)).toBe(true);
  });

  it('sub-component trade is a view of exports / imports, not a measure (C5.4)', () => {
    expect(themeVars(model, 'trade').map((x) => x.id)).not.toContain('trade.sub_exports');
    expect(themeVars(model, 'trade').map((x) => x.id)).not.toContain('trade.sub_imports');
    expect(pickVar(model, 'trade', 'trade.sub_imports')?.id).toBe('trade.imports');
    expect(pickVar(model, 'trade', 'trade.sub_exports')?.id).toBe('trade.exports');
  });

  it('cell items come from tech + stage (Strategic vocabulary); others from the item slug; else the default', () => {
    const ex = v('trade.exports');
    expect(pickItem(model, ex, { tech: 'Oil', stage: 'Upstream', item: null })?.slug).toBe('oil-upstream');
    expect(pickItem(model, ex, { tech: 'Geothermal', stage: 'Upstream', item: null })?.slug).toBe(defaultItem(model, ex)?.slug);
    expect(defaultItem(model, ex)?.slug).toBe('batteries-midstream');
    expect(pickItem(model, v('resources.mineral_reserves'), { tech: null, stage: null, item: 'graphite' })?.slug).toBe('graphite');
  });

  it('years: the latest observed unless a valid year is chosen; projections only when chosen', () => {
    const m = v('production.mining');
    expect(pickYear(m, [2025, 2030, 2035, 2040], null)).toBe(2025);
    expect(pickYear(m, [2025, 2030, 2035, 2040], 2035)).toBe(2035);
    expect(pickYear(m, [2025, 2030], 1999)).toBe(2025);
    expect(pickYear(m, [], null)).toBeNull();
  });

  it('share of world is legal only with a world basis on an additive measure', () => {
    expect(shareLegal(v('trade.exports'))).toBe(true);
    expect(shareLegal(v('trade.rca'))).toBe(false);
    expect(shareLegal(v('trade.net'))).toBe(false);
  });
});

describe('explore: KPI strip, result line, table and across sectors', () => {
  const ex = v('trade.exports');
  const bm = model.items[fixtureItem('cell', 'batteries-midstream')];
  const sl = slice(model, facts, ex, bm.idx, 2024, 'members');

  it('scope: coalition and coalition + China', () => {
    const chn = model.geoByCode.get('CHN')!;
    const usa = model.geoByCode.get('USA')!;
    expect(inScope(chn, 'coalition')).toBe(false);
    expect(inScope(chn, 'focus')).toBe(true);
    expect(inScope(usa, 'coalition')).toBe(true);
  });

  it('KPIs: China first, coalition counts, the highlight rank', () => {
    const usa = model.geoByCode.get('USA')!;
    const k = kpis(model, ex, sl.rows, sl.world, usa, 2024);
    expect(k.china.rank).toBe(1);
    expect(k.coalitionTotal).toBeGreaterThan(0);
    expect(k.coalitionWith).toBeLessThanOrEqual(k.coalitionTotal);
    expect(k.highlight?.rank).toBeGreaterThan(1);
    expect(k.conc.top1?.geo).toBe(model.geoByCode.get('CHN')!.idx);
  });

  it('the result line counts data, zeros, values without a figure and economies not covered', () => {
    expect(resultLine(model, ex, sl.rows, 'all')).toMatch(/^\d+ economies with data/);
    const mine = v('production.mine_usgs');
    const li = model.items[fixtureItem('mineral', 'lithium')];
    const ms = slice(model, facts, mine, li.idx, 2025, 'members');
    const line = resultLine(model, mine, ms.rows, 'all');
    expect(line).toMatch(/^4 economies with data · 1 report zero · 1 listed without a figure/);
    expect(line).toMatch(/not covered|none recorded/);
  });

  it('table rows rank within the scope; absentees are added on request, unranked', () => {
    const all = exploreRows(model, ex, sl.rows, sl.world, 2024, 'all', false);
    const coal = exploreRows(model, ex, sl.rows, sl.world, 2024, 'coalition', false);
    expect(coal.every((r) => inScope(r.g, 'coalition'))).toBe(true);
    expect(coal[0].rank).toBe(1);
    expect(all.length).toBeGreaterThan(coal.length);
    const mine = v('production.mine_usgs');
    const li = model.items[fixtureItem('mineral', 'lithium')];
    const ms = slice(model, facts, mine, li.idx, 2025, 'members');
    const base = exploreRows(model, mine, ms.rows, ms.world, 2025, 'all', false);
    const withAbsent = exploreRows(model, mine, ms.rows, ms.world, 2025, 'all', true);
    expect(withAbsent.length).toBeGreaterThan(base.length);
    const absent = withAbsent.filter((r) => !ms.rows.some((x) => x.geo === r.g.idx));
    expect(absent.every((r) => r.value === null && r.rank === null && (r.state.state === 'not_covered' || r.state.state === 'none'))).toBe(true);
    // withheld (nq) keeps its row, unranked
    expect(base.find((r) => r.g.code === 'IDN')).toMatchObject({ value: null, rank: null });
  });

  it('across sectors: one summary per cell with a world total, grouped by stage', () => {
    const rows = acrossRows(model, ex, 'members', 2024);
    expect(rows.map((r) => r.item.slug)).toContain('batteries-midstream');
    const order = rows.map((r) => r.section);
    expect(order).toEqual([...order].sort((a, z) => ['upstream', 'midstream', 'downstream'].indexOf(a) - ['upstream', 'midstream', 'downstream'].indexOf(z)));
    expect(acrossRows(model, v('trade.rca'), 'members', 2024)).toEqual([]);
  });
});
