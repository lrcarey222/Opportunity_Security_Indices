// The variable -> Sector chart table (src/posture/variableCharts.ts, 2026-10-30; indicators, not composites,
// 2026-10-31): every mapped measure exists in the Sector catalogue (config/sector_dictionary.yml `vars`, read from
// the file so the two cannot drift) in the theme the link opens, its item rule fits the measure's item axis, its
// first chart is one the measure's layout has, and "Share of world" is asked only of a measure with a world total.
// Every family has indicators or says why it has none, never both. The picker's options are indicators, each a
// labelled Sector chart, grouped by the family they feed in the composite's own structure; no composite or family
// score is an option; only the indicators with a chart at the step are offered; the families it cannot offer are
// named in one line with where to see them instead. The user's example holds: the Prize section's Investment
// momentum opens the map of /sector/explore/investment. The link carries the measure, the step and the economy.

import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { describe, expect, it } from 'vitest';
import { AVOID_RE } from '../../content/vocabulary.ts';
import type { VarInfo } from '../../sector/data/model.ts';
import { layoutOf, partsOf } from '../../sector/derive/explore.ts';
import { parseSectorSearch } from '../../sector/url.ts';
import {
  chartlessAt, chartlessLine, chartsAt, chartState, exploreHref, familyGroup, fenceFamilies, findVariable, VARIABLE_CHARTS, variableOptions, type ChartNode, type FenceFamily, type VariableAnalysis,
} from '../variableCharts.ts';

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
const all = (): [VariableAnalysis, FenceFamily][] => ANALYSES.flatMap((a) => VARIABLE_CHARTS[a].map((v) => [a, v] as [VariableAnalysis, FenceFamily]));
const fam = (a: VariableAnalysis, id: string) => VARIABLE_CHARTS[a].find((v) => v.id === id) as FenceFamily;

/** Every step a fence table can hold: the technologies of the Sector data at each stage. */
const TECHS = ['Batteries', 'Coal', 'Electric Grid', 'Electric Vehicles', 'Gas', 'Geothermal', 'Green Hydrogen', 'Nuclear', 'Oil', 'Semiconductors', 'Solar', 'Wind'];
const STAGES = ['Extraction', 'Refining', 'Components', 'Assembly', 'Recovery', 'Deployment'];
const STEPS: ChartNode[] = TECHS.flatMap((tech) => STAGES.map((stage) => ({ tech, stage })));
/** Each analysis' pickers: the prize with its Strategic Industrial Value and as the market measure alone. */
const PICKERS: [string, FenceFamily[]][] = [
  ['need', fenceFamilies('need')],
  ['prize (SIV)', fenceFamilies('prize', { siv: true, prizeLabel: 'Strategic Industrial Value' })],
  ['prize (market)', fenceFamilies('prize', { siv: false, prizeLabel: 'Industrial Opportunity' })],
  ['position', fenceFamilies('position')],
  ['deployment', fenceFamilies('deployment')],
];
/** The composite scores and the family scores the tables show: never an option. */
const COMPOSITES = ['Supply-chain vulnerability', 'Strategic Industrial Value', 'Industrial Opportunity', 'Market prize', 'Market outlook', 'Competitive Position', 'Deployment need'];
const SCORES = new Set([...COMPOSITES, ...all().map(([, f]) => f.label)]);

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
        expect(['source', 'related'], `${a}.${v.id}: ${c.m} needs a basis`).toContain(c.basis);
      }
    }
    expect(seen.size).toBeGreaterThanOrEqual(20);
  });

  it('each family has indicators or says why it has none, never both', () => {
    for (const [a, v] of all()) {
      const has = v.charts.length > 0;
      expect(has !== !!v.none, `${a}.${v.id}`).toBe(true);
    }
    // the ids are unique within an analysis (the table's column ids where it shows the family)
    for (const a of ANALYSES) expect(new Set(VARIABLE_CHARTS[a].map((v) => v.id)).size).toBe(VARIABLE_CHARTS[a].length);
  });

  it('the user’s example: Investment momentum opens the map of /sector/explore/investment', () => {
    const inv = fenceFamilies('prize').find((v) => v.label === 'Investment momentum') as FenceFamily;
    const [c] = chartsAt(inv, { tech: 'Solar', stage: 'Assembly' });
    expect(c).toMatchObject({ label: 'Annual clean-technology investment', theme: 'investment', m: 'investment.annual', chart: 'map', basis: 'source' });
    const href = exploreHref(c.theme, chartState(c, { tech: 'Solar', stage: 'Assembly' }, 'USA'));
    expect(href).toBe('/sector/explore/investment?c=USA&m=investment.annual&tech=Solar&stage=Assembly');
    // the Sector page reads the link back without a problem
    const q = parseSectorSearch(href.split('?')[1]);
    expect(q.problems).toEqual([]);
    expect(q.state).toMatchObject({ c: 'USA', m: 'investment.annual', tech: 'Solar', stage: 'Assembly' });
  });

  it('the composites’ own structure: the need’s four families, the prize’s market parts then commons and option, Competitive Position then the pathway, deployment need', () => {
    expect(fenceFamilies('need').map(familyGroup)).toEqual(['Import dependence', 'Supplier concentration', 'Risky supplier exposure', 'Lack of trusted alternatives']);
    const siv = fenceFamilies('prize', { siv: true, prizeLabel: 'Strategic Industrial Value' });
    expect(siv.map(familyGroup)).toEqual(['World demand growth (market prize)', 'Home-market growth (market prize)', 'Investment momentum (market prize)', 'Industrial commons', 'Future option']);
    for (const id of ['commons', 'option']) expect(siv.find((f) => f.id === id)?.none).toMatch(/attribute of each supply-chain step .*no measure of it/);
    // a core whose prize is the market measure: the market prize's parts, named after it; no commons or option
    const io = fenceFamilies('prize', { siv: false, prizeLabel: 'Industrial Opportunity' });
    expect(io.map(familyGroup)).toEqual(['World demand growth (Industrial Opportunity)', 'Home-market growth (Industrial Opportunity)', 'Investment momentum (Industrial Opportunity)']);
    expect(fenceFamilies('position').map(familyGroup)).toEqual([
      'World supply share', 'Export position', 'Structural trade share', 'Current investment', 'Operating capacity', 'Input cost', 'Resource base', 'Trade momentum',
      'Export feasibility (credible pathway)', 'Committed and announced pipeline (credible pathway)',
    ]);
    expect(fenceFamilies('deployment').map(familyGroup)).toEqual(['Demand adequacy', 'Target / climate gap', 'System need', 'Climate-aligned (alternative)', 'Deployment trend']);
  });

  it('no composite or family score is an option: every option is one indicator of its family, at every step', () => {
    // the composites are not families of the table any more
    for (const [a, f] of all()) {
      expect(COMPOSITES, `${a}.${f.id}`).not.toContain(f.label);
      expect(['need', 'opportunity', 'market', 'cp', 'outlook'], `${a}.${f.id}`).not.toContain(f.id);
    }
    let n = 0;
    for (const [name, fs] of PICKERS) {
      for (const step of STEPS) {
        const opts = variableOptions(fs, step);
        n += opts.length;
        for (const o of opts) {
          expect(SCORES.has(o.label), `${name} at ${step.tech} ${step.stage}: "${o.label}" is a score, not an indicator`).toBe(false);
          // the option is one Sector chart of the family it is grouped under, with its basis and note
          expect(o.family.charts, `${name}: ${o.value}`).toContain(o.chart);
          expect(o.label).toBe(o.chart.label);
          expect(o.group).toBe(familyGroup(o.family));
          expect(DICT.has(o.chart.m), `${name}: ${o.chart.m}`).toBe(true);
          expect(findVariable(fs, o.value)).toEqual({ family: o.family, chart: o.chart });
        }
        // the values are unique in a picker
        expect(new Set(opts.map((o) => o.value)).size).toBe(opts.length);
      }
    }
    expect(n).toBeGreaterThan(500);
    expect(findVariable(fenceFamilies('need'), '')).toBeNull();
    expect(findVariable(fenceFamilies('need'), 'need')).toBeNull();
  });

  it('every indicator has a short plain label saying what is measured', () => {
    for (const [a, f] of all()) {
      for (const c of f.charts) {
        expect(c.label, `${a}.${f.id}: ${c.m}`).toMatch(/^[A-Z][^.]{4,55}$/);
        expect(c.label, `${a}.${f.id}: ${c.label}`).not.toMatch(/\bnodes?\b|\bvariable\b/i);
        expect(c.label).not.toMatch(AVOID_RE);
      }
      // two indicators of one family never share a label
      expect(new Set(f.charts.map((c) => c.label)).size, `${a}.${f.id}`).toBe(f.charts.length);
    }
    // the user's list, a sample
    const labels = new Set(all().flatMap(([, f]) => f.charts.map((c) => c.label)));
    for (const l of [
      'Net trade in the step’s goods', 'Imports of the step’s goods', 'Fuel production as a share of use', 'Who supplies the imports', 'Imports from China and the opposite bloc',
      'World exports by supplier group', 'Share of world manufacturing capacity', 'Refined production, share of world', 'Mine production, share of world',
      'Clean-energy demand for the step’s minerals', 'World energy supply outlook', 'Electric-car sales', 'Annual clean-technology investment', 'Electricity generation from the technology',
      'Power capacity added', 'Electric cars on the road', 'Change in oil use per person', 'Deployment investment', 'Manufacturing capacity by status',
      'Announced capacity, share of world 2035', 'Export feasibility (Harvard Atlas)', 'Wages in the step’s activity', 'Lending rate', 'Mineral reserves', 'Fuel reserves', 'Uranium resources',
      'Electric share of new-car sales', 'Solar and wind generation', 'Balance of the fuel it displaces',
    ]) expect(labels.has(l), l).toBe(true);
  });

  it('grouping by family: the prize at a Solar Refining step, its market parts in order, the options of each', () => {
    const groups = (fs: FenceFamily[], step: ChartNode) => {
      const out: [string, string[]][] = [];
      for (const o of variableOptions(fs, step)) {
        const last = out[out.length - 1];
        if (last && last[0] === o.group) last[1].push(o.label);
        else out.push([o.group, [o.label]]);
      }
      return out;
    };
    const siv = fenceFamilies('prize', { siv: true });
    expect(groups(siv, { tech: 'Solar', stage: 'Refining' })).toEqual([
      ['World demand growth (market prize)', ['Clean-energy demand for the step’s minerals', 'World energy supply outlook']],
      ['Home-market growth (market prize)', ['Electricity generation from the technology', 'Power capacity added', 'Deployment investment']],
      ['Investment momentum (market prize)', ['Annual clean-technology investment']],
    ]);
    // each group once, in the composite's order (a group never splits)
    for (const [, fs] of PICKERS) {
      for (const step of STEPS) {
        const g = groups(fs, step).map(([x]) => x);
        expect(new Set(g).size).toBe(g.length);
        expect(g).toEqual(fs.map(familyGroup).filter((x) => g.includes(x)));
      }
    }
    // the High fence at Batteries Refining
    expect(groups(fenceFamilies('need'), { tech: 'Batteries', stage: 'Refining' })).toEqual([
      ['Import dependence', ['Net trade in the step’s goods', 'Imports of the step’s goods']],
      ['Supplier concentration', ['Who supplies the imports']],
      ['Risky supplier exposure', ['Imports from China and the opposite bloc']],
      ['Lack of trusted alternatives', ['World exports by supplier group', 'Refined production, share of world']],
    ]);
  });

  it('step filtering: only the indicators with a chart at the step are offered', () => {
    const at = (fs: FenceFamily[], tech: string, stage: string) => variableOptions(fs, { tech, stage }).map((o) => o.value);
    // fuel self-sufficiency only at a fossil fuel's Extraction step
    expect(at(fenceFamilies('need'), 'Oil', 'Extraction').filter((v) => v.startsWith('import:'))).toEqual(['import:0', 'import:1', 'import:2']);
    expect(at(fenceFamilies('need'), 'Solar', 'Components').filter((v) => v.startsWith('import:'))).toEqual(['import:0', 'import:1']);
    // Batteries at Deployment & Use: the variable renewables storage firms, and deployment investment over time
    const dep = variableOptions(fenceFamilies('deployment'), { tech: 'Batteries', stage: 'Deployment' });
    expect(dep.map((o) => [o.group, o.label, o.chart.m, o.chart.chart])).toEqual([
      ['Demand adequacy', 'Solar and wind generation', 'production.generation', 'over-time'],
      ['System need', 'Solar and wind generation', 'production.generation', 'over-time'],
      ['Deployment trend', 'Deployment investment', 'investment.annual', 'over-time'],
    ]);
    // a Solar Assembly step's position: no resource base, the manufacturing share and the exports for world supply share
    const pos = variableOptions(fenceFamilies('position'), { tech: 'Solar', stage: 'Assembly' });
    expect(pos.filter((o) => o.family.id === 'share').map((o) => o.label)).toEqual(['Share of world manufacturing capacity', 'Exports, share of world']);
    expect(pos.some((o) => o.family.id === 'resource')).toBe(false);
    // the prize of an EV Assembly step: electric-car sales for world demand
    const ev = variableOptions(fenceFamilies('prize'), { tech: 'Electric Vehicles', stage: 'Assembly' });
    expect(ev.filter((o) => o.family.id === 'node-prize').map((o) => o.chart.m)).toEqual(['consumption.ev_sales']);
  });

  it('the families it cannot offer: named in one line with where to see them (the row’s details; the monthly trade card)', () => {
    const line = (fs: FenceFamily[], tech: string, stage: string, selected = false) => chartlessLine(chartlessAt(fs, { tech, stage }), { selected });
    const siv = fenceFamilies('prize', { siv: true });
    expect(line(siv, 'Solar', 'Assembly')).toBe('No Sector chart for Industrial commons and Future option: select a row to see them in its details.');
    // a family empty at this step is named so
    expect(chartlessAt(siv, { tech: 'Batteries', stage: 'Assembly' }).map((c) => [c.family.id, c.reason])).toEqual([['node-prize', 'step'], ['commons', 'none'], ['option', 'none']]);
    expect(line(siv, 'Batteries', 'Assembly', true)).toBe('No Sector chart for World demand growth (none at this step), Industrial commons and Future option: the selected row’s details show them.');
    // trade momentum: the monthly trade card
    expect(line(fenceFamilies('position'), 'Solar', 'Assembly')).toBe(
      'No Sector chart for Resource base (none at this step) and Trade momentum: select a row to see them in its details; for trade momentum, see also the monthly trade card in the Position section.',
    );
    expect(line(fenceFamilies('deployment'), 'Batteries', 'Deployment')).toBe(
      'No Sector chart for Target / climate gap (none at this step) and Climate-aligned (alternative): select a row to see them in its details.',
    );
    // one family: "it"; every family charted: no line
    expect(line(fenceFamilies('deployment'), 'Solar', 'Deployment')).toBe('No Sector chart for Climate-aligned (alternative): select a row to see it in its details.');
    expect(line(fenceFamilies('need'), 'Batteries', 'Refining')).toBeNull();
    expect(line(fenceFamilies('prize', { siv: false }), 'Solar', 'Refining')).toBeNull();
    // never an invented measure: a chartless family has no indicator at the step
    for (const [, fs] of PICKERS) for (const step of STEPS) for (const c of chartlessAt(fs, step)) expect(chartsAt(c.family, step)).toEqual([]);
    for (const [, fs] of PICKERS) for (const c of chartlessAt(fs, { tech: 'Solar', stage: 'Assembly' })) expect(c.reason === 'none' ? !!c.family.none : c.family.charts.length > 0).toBe(true);
    expect(line(siv, 'Solar', 'Assembly')).not.toMatch(AVOID_RE);
  });

  it('an indicator’s charts follow the step’s stage and technology', () => {
    const share = fam('position', 'share');
    expect(chartsAt(share, { tech: 'Batteries', stage: 'Refining' }).map((c) => c.m)).toEqual(['production.refining', 'trade.exports']);
    expect(chartsAt(share, { tech: 'Batteries', stage: 'Extraction' }).map((c) => c.m)).toEqual(['production.mining', 'trade.exports']);
    expect(chartsAt(share, { tech: 'Oil', stage: 'Extraction' }).map((c) => c.m)).toEqual(['production.fuels', 'trade.exports']);
    expect(chartsAt(share, { tech: 'Nuclear', stage: 'Extraction' }).map((c) => c.m)).toEqual(['production.uranium', 'trade.exports']);
    expect(chartsAt(share, { tech: 'Solar', stage: 'Components' }).map((c) => c.m)).toEqual(['manufacturing.share_now', 'trade.exports']);
    // world demand growth: mineral demand at the mineral stages, the WEO outlook of the technology's fuel, EV sales for EVs
    const prize = fam('prize', 'node-prize');
    expect(chartsAt(prize, { tech: 'Solar', stage: 'Refining' }).map((c) => c.m)).toEqual(['consumption.mineral_demand', 'consumption.weo']);
    expect(chartsAt(prize, { tech: 'Solar', stage: 'Assembly' }).map((c) => c.m)).toEqual(['consumption.weo']);
    expect(chartsAt(prize, { tech: 'Electric Vehicles', stage: 'Assembly' }).map((c) => c.m)).toEqual(['consumption.ev_sales']);
    expect(chartsAt(prize, { tech: 'Batteries', stage: 'Assembly' })).toEqual([]);
    // deployment: the Target / climate gap has the build rate for power technologies only; the alternative has no chart
    const gap = fam('deployment', 'gap');
    expect(chartsAt(gap, { tech: 'Solar', stage: 'Deployment' }).map((c) => c.m)).toEqual(['investment.power_additions']);
    expect(chartsAt(gap, { tech: 'Batteries', stage: 'Deployment' })).toEqual([]);
    expect(fam('deployment', 'alt').none).toMatch(/no country measure/);
    // Batteries at Deployment & Use: the variable renewables storage firms (solar generation), and deployment investment over time
    const dep = (id: string) => chartsAt(fam('deployment', id), { tech: 'Batteries', stage: 'Deployment' });
    expect(dep('demand').map((c) => [c.m, chartState(c, { tech: 'Batteries', stage: 'Deployment' }, null).item])).toEqual([['production.generation', 'solar']]);
    expect(dep('system').map((c) => c.m)).toEqual(['production.generation']);
    expect(dep('trend').map((c) => [c.m, c.chart])).toEqual([['investment.annual', 'over-time']]);
    // Solar at Deployment & Use: its own generation first, then the build rate and deployment investment
    const sol = (id: string) => chartsAt(fam('deployment', id), { tech: 'Solar', stage: 'Deployment' }).map((c) => c.m);
    expect(sol('trend')).toEqual(['production.generation', 'investment.power_additions', 'investment.annual']);
    expect(sol('system')).toEqual(['consumption.balance']);
    // home-market growth of a manufacturing step reads the technology's deployment at home
    const home = fam('prize', 'home');
    expect(chartsAt(home, { tech: 'Batteries', stage: 'Assembly' }).map((c) => c.m)).toEqual(['investment.annual']);
    expect(chartsAt(home, { tech: 'Solar', stage: 'Components' }).map((c) => c.m)).toEqual(['production.generation', 'investment.power_additions', 'investment.annual']);
  });

  it('the need families: import dependence on trade, the suppliers on bilateral flows (import partners first)', () => {
    const at = (id: string) => chartsAt(fam('need', id), { tech: 'Batteries', stage: 'Refining' });
    expect(at('import').map((c) => c.m)).toEqual(['trade.net', 'trade.imports']);
    expect(at('concentration')).toMatchObject([{ m: 'trade.flow', chart: 'partners', label: 'Who supplies the imports' }]);
    expect(at('risky')).toMatchObject([{ m: 'trade.flow', chart: 'partners', label: 'Imports from China and the opposite bloc' }]);
    expect(at('trusted').map((c) => c.m)).toEqual(['trade.exports', 'production.refining']);
    // the supply-chain vulnerability composite is not a family
    expect(VARIABLE_CHARTS.need.map((f) => f.id)).toEqual(['import', 'concentration', 'risky', 'trusted']);
  });

  it('the state of a chart: the cell, the technology’s Deployment cell, a fixed item, Share of world', () => {
    const [ev] = chartsAt(fam('prize', 'node-prize'), { tech: 'Electric Vehicles', stage: 'Assembly' });
    expect(chartState(ev, { tech: 'Electric Vehicles', stage: 'Assembly' }, 'DEU')).toEqual({ m: 'consumption.ev_sales', c: 'DEU', tech: 'Electric Vehicles', stage: 'Deployment' });
    const [weo] = chartsAt(fam('prize', 'node-prize'), { tech: 'Wind', stage: 'Components' });
    expect(chartState(weo, { tech: 'Wind', stage: 'Components' }, null)).toEqual({ m: 'consumption.weo', c: null, item: 'wind' });
    const [mine] = chartsAt(fam('position', 'share'), { tech: 'Batteries', stage: 'Extraction' });
    expect(chartState(mine, { tech: 'Batteries', stage: 'Extraction' }, 'AUS')).toEqual({ m: 'production.mining', c: 'AUS', as: 'share' });
    expect(exploreHref('production', chartState(mine, { tech: 'Batteries', stage: 'Extraction' }, 'AUS'))).toBe('/sector/explore/production?c=AUS&m=production.mining&as=share');
  });
});
