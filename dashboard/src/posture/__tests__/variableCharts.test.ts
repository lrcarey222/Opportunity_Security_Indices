// The variable -> Sector chart table (src/posture/variableCharts.ts, 2026-10-30): every mapped measure exists in
// the Sector catalogue (config/sector_dictionary.yml `vars`, read from the file so the two cannot drift) in the
// theme the link opens, its item rule fits the measure's item axis, its first chart is one the measure's layout
// has, and "Share of world" is asked only of a measure with a world total. Every variable has charts or says why
// it has none (a composite, a node attribute, data the Sector bundle does not carry), never both. The user's
// example holds: the Prize section's Investment momentum opens the map of /sector/explore/investment. The
// commons and option families say they are node attributes; the picker follows the node's stage and technology;
// the link carries the measure, the node and the economy.

import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { describe, expect, it } from 'vitest';
import type { VarInfo } from '../../sector/data/model.ts';
import { layoutOf, partsOf } from '../../sector/derive/explore.ts';
import { parseSectorSearch } from '../../sector/url.ts';
import { chartsAt, chartState, exploreHref, fenceVariables, VARIABLE_CHARTS, type FenceVariable, type VariableAnalysis } from '../variableCharts.ts';

interface DictVar {
  id: string;
  theme: string;
  native_level: string;
  item_axis: string;
  agg: string;
  world_basis: string;
}

/** The `vars:` list of the Sector dictionary: each `  - id:` entry with its scalar fields. */
function dictionaryVars(): Map<string, DictVar> {
  const yml = readFileSync(resolve(process.cwd(), '../config/sector_dictionary.yml'), 'utf8');
  const body = yml.slice(yml.search(/^vars:\s*$/m));
  const out = new Map<string, DictVar>();
  for (const block of body.split(/^ {2}- id: /m).slice(1)) {
    const id = block.split('\n')[0].trim();
    const field = (k: string) => new RegExp(`^ {4}${k}: *(.*)$`, 'm').exec(block)?.[1].trim().replace(/^"|"$/g, '') ?? '';
    out.set(id, { id, theme: field('theme'), native_level: field('native_level'), item_axis: field('item_axis'), agg: field('agg'), world_basis: field('world_basis') });
  }
  return out;
}

const DICT = dictionaryVars();
const ANALYSES = Object.keys(VARIABLE_CHARTS) as VariableAnalysis[];
const all = (): [VariableAnalysis, FenceVariable][] => ANALYSES.flatMap((a) => VARIABLE_CHARTS[a].map((v) => [a, v] as [VariableAnalysis, FenceVariable]));

describe('variable charts: every mapped measure exists in the Sector catalogue', () => {
  it('the dictionary parses (75 measures, the ones the table names among them)', () => {
    expect(DICT.size).toBeGreaterThanOrEqual(70);
    expect(DICT.get('investment.annual')).toMatchObject({ theme: 'investment', native_level: 'country', item_axis: 'cell' });
    expect(DICT.get('production.mining')).toMatchObject({ theme: 'production', item_axis: 'mineral' });
  });

  it('each measure is in the dictionary, in the theme its link opens, with an item rule that fits its axis', () => {
    const seen = new Set<string>();
    for (const [a, v] of all()) {
      for (const c of v.charts) {
        const d = DICT.get(c.m);
        expect(d, `${a}.${v.id}: ${c.m} is not a measure of config/sector_dictionary.yml`).toBeTruthy();
        if (!d) continue;
        seen.add(c.m);
        expect(d.theme, `${a}.${v.id}: ${c.m} is in theme ${d.theme}, not ${c.theme}`).toBe(c.theme);
        const axis = d.item_axis;
        if (c.item.kind === 'cell' || c.item.kind === 'deployment') expect(axis, `${a}.${v.id}: ${c.m} has no cell items`).toBe('cell');
        else if (c.item.kind === 'none') expect(axis, `${a}.${v.id}: ${c.m} has items`).toBe('none');
        else expect(['cell', 'none'], `${a}.${v.id}: ${c.m} (${axis}) is not a linked or per-technology item`).not.toContain(axis);
        // the first chart is one the measure's layout has
        const layout = layoutOf({ id: d.id, theme: d.theme, native_level: d.native_level } as unknown as VarInfo);
        expect(partsOf(layout), `${a}.${v.id}: ${c.m} (${layout}) has no '${c.chart}' chart`).toContain(c.chart);
        // Share of world only where the measure has a world total
        if (c.as === 'share') expect(d.world_basis !== 'none' && (d.agg === 'sum' || d.world_basis === 'one'), `${a}.${v.id}: ${c.m} has no share of world`).toBe(true);
        expect(c.note.length, `${a}.${v.id}: ${c.m} needs a note`).toBeGreaterThan(30);
      }
    }
    expect(seen.size).toBeGreaterThanOrEqual(20);
  });

  it('each variable has Sector charts or says why it has none, never both', () => {
    for (const [a, v] of all()) {
      const has = v.charts.length > 0;
      expect(has !== !!v.none, `${a}.${v.id}`).toBe(true);
    }
    // the ids are unique within an analysis (they are the table's column ids)
    for (const a of ANALYSES) expect(new Set(VARIABLE_CHARTS[a].map((v) => v.id)).size).toBe(VARIABLE_CHARTS[a].length);
  });

  it('the user’s example: Investment momentum opens the map of /sector/explore/investment', () => {
    const inv = fenceVariables('prize').find((v) => v.label === 'Investment momentum') as FenceVariable;
    const [c] = chartsAt(inv, { tech: 'Solar', stage: 'Assembly' });
    expect(c).toMatchObject({ theme: 'investment', m: 'investment.annual', chart: 'map', basis: 'source' });
    const href = exploreHref(c.theme, chartState(c, { tech: 'Solar', stage: 'Assembly' }, 'USA'));
    expect(href).toBe('/sector/explore/investment?c=USA&m=investment.annual&tech=Solar&stage=Assembly');
    // the Sector page reads the link back without a problem
    const q = parseSectorSearch(href.split('?')[1]);
    expect(q.problems).toEqual([]);
    expect(q.state).toMatchObject({ c: 'USA', m: 'investment.annual', tech: 'Solar', stage: 'Assembly' });
  });

  it('the Strategic Industrial Value: the market family maps, commons and option are node attributes, the composites say so', () => {
    const vs = fenceVariables('prize', { siv: true, prizeLabel: 'Strategic Industrial Value' });
    const by = (id: string) => vs.find((v) => v.id === id) as FenceVariable;
    expect(vs.map((v) => v.id)).toEqual(['opportunity', 'market', 'commons', 'option', 'node-prize', 'home', 'investment', 'outlook', 'need']);
    for (const id of ['commons', 'option']) expect(by(id).none).toMatch(/attribute of each supply-chain step .*no measure of it/);
    for (const id of ['opportunity', 'market']) expect(by(id).none).toMatch(/^A composite score/);
    for (const id of ['node-prize', 'home', 'investment']) expect(by(id).charts.length, id).toBeGreaterThan(0);
    // a core whose prize is the market measure: Industrial Opportunity, without the three families
    const io = fenceVariables('prize', { siv: false, prizeLabel: 'Industrial Opportunity' });
    expect(io.map((v) => v.id)).toEqual(['opportunity', 'node-prize', 'home', 'investment', 'outlook', 'need']);
    expect(io[0]).toMatchObject({ label: 'Industrial Opportunity' });
    expect(io[0].none).toMatch(/world demand growth and investment momentum/);
  });

  it('a variable’s charts follow the node’s stage and technology', () => {
    const share = VARIABLE_CHARTS.position.find((v) => v.id === 'share') as FenceVariable;
    expect(chartsAt(share, { tech: 'Batteries', stage: 'Refining' }).map((c) => c.m)).toEqual(['production.refining', 'trade.exports']);
    expect(chartsAt(share, { tech: 'Batteries', stage: 'Extraction' }).map((c) => c.m)).toEqual(['production.mining', 'trade.exports']);
    expect(chartsAt(share, { tech: 'Oil', stage: 'Extraction' }).map((c) => c.m)).toEqual(['production.fuels', 'trade.exports']);
    expect(chartsAt(share, { tech: 'Nuclear', stage: 'Extraction' }).map((c) => c.m)).toEqual(['production.uranium', 'trade.exports']);
    expect(chartsAt(share, { tech: 'Solar', stage: 'Components' }).map((c) => c.m)).toEqual(['manufacturing.share_now', 'trade.exports']);
    // the node prize: mineral demand at the mineral stages, the WEO outlook of the technology's fuel, EV sales for EVs
    const prize = VARIABLE_CHARTS.prize.find((v) => v.id === 'node-prize') as FenceVariable;
    expect(chartsAt(prize, { tech: 'Solar', stage: 'Refining' }).map((c) => c.m)).toEqual(['consumption.mineral_demand', 'consumption.weo']);
    expect(chartsAt(prize, { tech: 'Solar', stage: 'Assembly' }).map((c) => c.m)).toEqual(['consumption.weo']);
    expect(chartsAt(prize, { tech: 'Electric Vehicles', stage: 'Assembly' }).map((c) => c.m)).toEqual(['consumption.ev_sales']);
    expect(chartsAt(prize, { tech: 'Batteries', stage: 'Assembly' })).toEqual([]);
    // deployment: the Target / climate gap has the build rate for power technologies only; the alternative has no chart
    const gap = VARIABLE_CHARTS.deployment.find((v) => v.id === 'gap') as FenceVariable;
    expect(chartsAt(gap, { tech: 'Solar', stage: 'Deployment' }).map((c) => c.m)).toEqual(['investment.power_additions']);
    expect(chartsAt(gap, { tech: 'Batteries', stage: 'Deployment' })).toEqual([]);
    expect((VARIABLE_CHARTS.deployment.find((v) => v.id === 'alt') as FenceVariable).none).toMatch(/no country measure/);
    // Batteries at Deployment & Use: the variable renewables storage firms (solar generation), and deployment investment over time
    const dep = (id: string) => chartsAt(VARIABLE_CHARTS.deployment.find((v) => v.id === id) as FenceVariable, { tech: 'Batteries', stage: 'Deployment' });
    expect(dep('demand').map((c) => [c.m, chartState(c, { tech: 'Batteries', stage: 'Deployment' }, null).item])).toEqual([['production.generation', 'solar']]);
    expect(dep('system').map((c) => c.m)).toEqual(['production.generation']);
    expect(dep('trend').map((c) => [c.m, c.chart])).toEqual([['investment.annual', 'over-time']]);
    // Solar at Deployment & Use: its own generation first, then the build rate and deployment investment
    const sol = (id: string) => chartsAt(VARIABLE_CHARTS.deployment.find((v) => v.id === id) as FenceVariable, { tech: 'Solar', stage: 'Deployment' }).map((c) => c.m);
    expect(sol('trend')).toEqual(['production.generation', 'investment.power_additions', 'investment.annual']);
    expect(sol('system')).toEqual(['consumption.balance']);
    // home-market growth of a manufacturing node reads the technology's deployment at home
    const home = VARIABLE_CHARTS.prize.find((v) => v.id === 'home') as FenceVariable;
    expect(chartsAt(home, { tech: 'Batteries', stage: 'Assembly' }).map((c) => c.m)).toEqual(['investment.annual']);
    expect(chartsAt(home, { tech: 'Solar', stage: 'Components' }).map((c) => c.m)).toEqual(['production.generation', 'investment.power_additions', 'investment.annual']);
  });

  it('the need variables: import dependence on trade, the suppliers on bilateral flows (import partners first)', () => {
    const need = VARIABLE_CHARTS.need;
    const at = (id: string) => chartsAt(need.find((v) => v.id === id) as FenceVariable, { tech: 'Batteries', stage: 'Refining' });
    expect(at('import').map((c) => c.m)).toEqual(['trade.net', 'trade.imports']);
    expect(at('concentration')).toMatchObject([{ m: 'trade.flow', chart: 'partners' }]);
    expect(at('risky')).toMatchObject([{ m: 'trade.flow', chart: 'partners' }]);
    expect(at('trusted').map((c) => c.m)).toEqual(['trade.exports', 'production.refining']);
    expect(need.find((v) => v.id === 'need')?.none).toMatch(/^A composite score/);
  });

  it('the state of a chart: the cell, the technology’s Deployment cell, a fixed item, Share of world', () => {
    const v = (a: VariableAnalysis, id: string) => VARIABLE_CHARTS[a].find((x) => x.id === id) as FenceVariable;
    const [ev] = chartsAt(v('prize', 'node-prize'), { tech: 'Electric Vehicles', stage: 'Assembly' });
    expect(chartState(ev, { tech: 'Electric Vehicles', stage: 'Assembly' }, 'DEU')).toEqual({ m: 'consumption.ev_sales', c: 'DEU', tech: 'Electric Vehicles', stage: 'Deployment' });
    const [weo] = chartsAt(v('prize', 'node-prize'), { tech: 'Wind', stage: 'Components' });
    expect(chartState(weo, { tech: 'Wind', stage: 'Components' }, null)).toEqual({ m: 'consumption.weo', c: null, item: 'wind' });
    const [mine] = chartsAt(v('position', 'share'), { tech: 'Batteries', stage: 'Extraction' });
    expect(chartState(mine, { tech: 'Batteries', stage: 'Extraction' }, 'AUS')).toEqual({ m: 'production.mining', c: 'AUS', as: 'share' });
    expect(exploreHref('production', chartState(mine, { tech: 'Batteries', stage: 'Extraction' }, 'AUS'))).toBe('/sector/explore/production?c=AUS&m=production.mining&as=share');
  });
});
