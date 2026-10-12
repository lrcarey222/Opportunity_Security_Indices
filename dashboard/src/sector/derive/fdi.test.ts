// CM-FDI derivations (sector-fdi): the pure helpers on hand-made projects (links, words, value states,
// group splits, rankings, the Sankey fold, the EU's inbound / outbound rule), then the R synthetic bundle
// (or SECTOR_BUNDLE_DIR, e.g. public/data/sector for a local run on the real export): an undisclosed
// capital investment is null and never 0, and re-summing the projects of each (cell, basis) in table
// order reproduces the chunk's fdi.flows, fdi.groups and fdi.years: counts exactly, sums and shares to
// within 1e-12 relative (R's sum() accumulates in long double, so 17 of the 626 disclosed flow sums
// of the 2026-10-11 export differ from a double re-sum in the last bit).

import { describe, expect, it } from 'vitest';
import { haveSynthetic, readSyntheticChunk } from '../__fixtures__/__tests__/synthetic.ts';
import { FDI_GROUPS, refSizes, validateChunk, type SectorFdiChunk, type SectorMetaChunk } from '../contract.ts';
import { modelOf, type GeoInfo, type SectorModel, type SourceInfo } from '../data/model.ts';
import {
  coalitionShare, countryProjects, disclosedWords, fdiByGeo, fdiData, fdiFlows, fdiGroupSplit, fdiSankey, fdiTotals, fdiYearBars, fdiYears, filterProjects, fmtUsdM, geoMembers, geoRank, hasFdi, hostOf, isFdiVar,
  jvWords, NOT_DISCLOSED, OTHER_DESTS, OTHER_HOMES, partialYearNote, projectsCsv, rankWords, safeHttpUrl, sourceNote, splitShares, yearsCsv, type FdiGroup, type FdiProject,
} from './fdi.ts';
import { layoutBanner, layoutOf, themeVars } from './explore.ts';

let seq = 0;
/** A hand-made project (defaults: a disclosed manufacturing project the indices use). */
function proj(o: Partial<FdiProject>): FdiProject {
  const row = seq++;
  return {
    row, pid: `P${row}`, item: 0, technology: 'Battery', parent: `Parent ${row}`, home: 1, dest: 2, city: null, status: 'Completed', operational: 'Operational',
    activity: 'Manufacturing', yearAnnounced: 2020, yearCompleted: 2022, value: 100, expansion: false, phase: null, jv: false, jvLocal: false, jvLocalState: null,
    jvCompany: null, manufacturing: true, active: true, indexUse: true, homeGroup: 'core', ...o,
  };
}

describe('CM-FDI helpers', () => {
  it('links: only clean http(s) URLs; hosts without www; notes without links or the [UNVERIFIED] mark', () => {
    expect(safeHttpUrl('https://www.reuters.com/a?b=1')).toBe('https://www.reuters.com/a?b=1');
    expect(safeHttpUrl('http://example.org/x')).toBe('http://example.org/x');
    expect(safeHttpUrl('javascript:alert(1)')).toBeNull();
    expect(safeHttpUrl('data:text/html,hi')).toBeNull();
    expect(safeHttpUrl('ftp://example.org')).toBeNull();
    expect(safeHttpUrl('')).toBeNull();
    expect(safeHttpUrl(null)).toBeNull();
    expect(hostOf('https://www.reuters.com/a')).toBe('reuters.com');
    expect(sourceNote('https://a.org/x (year only)')).toBe('(year only)');
    expect(sourceNote('[UNVERIFIED] fDi Markets (FT)')).toBe('fDi Markets (FT)');
  });

  it('value words: an undisclosed value is "not disclosed", never 0', () => {
    expect(fmtUsdM(null)).toBe(NOT_DISCLOSED);
    expect(fmtUsdM(NaN)).toBe(NOT_DISCLOSED);
    expect(fmtUsdM(1800)).toBe('USD 1.8 bn');
    expect(fmtUsdM(240)).toBe('USD 240 m');
    expect(fmtUsdM(13900, 'table')).toBe('13,900');
    expect(disclosedWords({ n: 3, nValue: 0, value: null })).toMatch(/^not disclosed \(none of 3 projects/);
    expect(disclosedWords({ n: 3, nValue: 2, value: 300 })).toBe('USD 300 m (2 of 3 projects disclose a value)');
    expect(jvWords(proj({ jv: false }))).toBe('no');
    expect(jvWords(proj({ jv: true, jvCompany: 'Ultium Cells', jvLocal: true, jvLocalState: true }))).toBe('Ultium Cells · with a local partner (state-owned)');
  });

  it('the edition year is flagged as partial', () => {
    const src = { edition: '2026.10.05' } as SourceInfo;
    expect(partialYearNote(src, 2026)).toBe('2026 is a partial year: announcements up to the edition of 5 October 2026.');
    expect(partialYearNote(src, 2025)).toBeNull();
    expect(partialYearNote(null, 2026)).toBeNull();
  });

  it('group split: shares of counts sum to 1; an empty group is 0, a group with only undisclosed values is null', () => {
    const ps = [proj({ homeGroup: 'china', value: 300 }), proj({ homeGroup: 'china', value: null }), proj({ homeGroup: 'core', value: 100 }), proj({ homeGroup: 'partner', value: null })];
    const s = fdiGroupSplit(ps);
    expect(s.n).toBe(4);
    expect(s.value).toBe(400);
    expect(s.nValue).toBe(2);
    expect(s.parts.china.shareN).toBe(0.5);
    expect(s.parts.china.shareValue).toBe(0.75);
    expect(s.parts.partner.value).toBeNull();
    expect(Number.isNaN(s.parts.partner.shareValue)).toBe(true);
    expect(s.parts.rest.value).toBe(0);
    expect(FDI_GROUPS.reduce((a, g) => a + s.parts[g].shareN, 0)).toBeCloseTo(1, 12);
    const byValue = splitShares(s, 'value');
    expect(byValue.partner).toBeUndefined();
    expect((byValue.china ?? 0) + (byValue.core ?? 0) + (byValue.rest ?? 0)).toBeCloseTo(1, 12);
  });

  it('rankings: by count, or by disclosed value with "not disclosed" economies last', () => {
    const ps = [proj({ dest: 5, value: null }), proj({ dest: 5, value: null }), proj({ dest: 6, value: 10 }), proj({ dest: 7, value: 500 })];
    expect(fdiByGeo(ps, 'dest', 'n').map((r) => r.geo)).toEqual([5, 7, 6]);
    const v = fdiByGeo(ps, 'dest', 'value');
    expect(v.map((r) => r.geo)).toEqual([7, 6, 5]);
    expect(v[2].value).toBeNull();
    // ranks by project count, ties shared (6 and 7 both have 1 project behind 5's 2), never broken by value
    expect(geoRank(v, 6)).toEqual({ rank: 2, n: 3, tied: 1 });
    expect(geoRank(v, 7)).toEqual({ rank: 2, n: 3, tied: 1 });
    expect(geoRank(v, 5)).toEqual({ rank: 1, n: 3, tied: 0 });
    expect(geoRank(v, 99)).toEqual({ rank: null, n: 3, tied: 0 });
    expect(rankWords(geoRank(v, 6), 'destinations')).toBe('#2 of 3 destinations (tied with 1 other)');
    expect(rankWords(geoRank(v, 5), 'destinations')).toBe('#1 of 3 destinations');
    expect(rankWords(geoRank(v, 99), 'destinations')).toBeNull();
  });

  it('coalition share: a coalition group with only undisclosed values adds 0, never NaN', () => {
    const s = fdiGroupSplit([proj({ homeGroup: 'core', value: 300 }), proj({ homeGroup: 'partner', value: null }), proj({ homeGroup: 'china', value: 100 })]);
    expect(coalitionShare(s, 'n')).toBeCloseTo(2 / 3, 12);
    expect(coalitionShare(s, 'value')).toBe(0.75);
    expect(Number.isNaN(coalitionShare(fdiGroupSplit([proj({ value: null })]), 'value'))).toBe(true);
  });

  it('Sankey: the top investors and destinations, the rest folded; value mode skips undisclosed flows', () => {
    const ps = [
      proj({ home: 1, dest: 10, value: 50 }), proj({ home: 1, dest: 11, value: 30 }), proj({ home: 2, dest: 10, value: null }),
      proj({ home: 3, dest: 12, value: 5 }), proj({ home: 4, dest: 13, value: 1 }),
    ];
    const flows = fdiFlows(ps);
    const n = fdiSankey(flows, 'n', { topH: 2, topD: 2 });
    expect(n.total).toBe(5);
    expect(n.links.reduce((a, l) => a + l.v, 0)).toBe(5);
    expect(n.links.some((l) => l.home === OTHER_HOMES)).toBe(true);
    expect(n.links.some((l) => l.dest === OTHER_DESTS)).toBe(true);
    const v = fdiSankey(flows, 'value', { topH: 8, topD: 10 });
    expect(v.total).toBe(86);
    expect(v.links.some((l) => l.home === 2)).toBe(false);
  });

  it('years: every year of the window, a year without an announcement has 0 projects', () => {
    const bars = fdiYearBars(fdiYears([proj({ yearAnnounced: 2018, homeGroup: 'china' }), proj({ yearAnnounced: 2020, value: null })]), 2018, 2020);
    expect(bars.map((b) => [b.year, b.total.n])).toEqual([[2018, 1], [2019, 0], [2020, 1]]);
    expect(bars[2].groups.core.value).toBeNull();
    expect(bars[0].groups.china.value).toBe(100);
    // the CSV tells "no project" (0) from "not disclosed" (empty)
    const model = { varById: new Map(), sources: new Map(), items: [], geos: [] } as unknown as SectorModel;
    const rows = yearsCsv(model, bars, 'batteries-midstream', 'all').split(/\r?\n/);
    expect(rows[0]).toContain('capital_investment_usd_m,value_status');
    expect(rows.find((r) => r.startsWith('2020,core,'))).toMatch(/,1,0,,not disclosed,/);
    expect(rows.find((r) => r.startsWith('2019,core,'))).toMatch(/,0,0,0,no project,/);
    expect(rows.find((r) => r.startsWith('2018,china,'))).toMatch(/,1,1,100,disclosed \(sum\),/);
  });

  it('inbound and outbound: the EU node counts its members together and leaves intra-EU projects out', () => {
    const geos = [
      { idx: 0, code: 'DEU', kind: 'country', eu_member: true },
      { idx: 1, code: 'FRA', kind: 'country', eu_member: true },
      { idx: 2, code: 'CHN', kind: 'country', eu_member: false },
      { idx: 3, code: 'EUU', kind: 'eu', eu_member: false },
    ] as unknown as GeoInfo[];
    const model = { geos } as unknown as SectorModel;
    const ps = [proj({ home: 2, dest: 0 }), proj({ home: 0, dest: 1 }), proj({ home: 1, dest: 2 })];
    const eu = countryProjects(ps, geoMembers(model, geos[3]));
    expect(eu.inbound.map((p) => p.pid)).toEqual([ps[0].pid]);
    expect(eu.outbound.map((p) => p.pid)).toEqual([ps[2].pid]);
    const deu = countryProjects(ps, geoMembers(model, geos[0]));
    expect(deu.inbound.map((p) => p.pid)).toEqual([ps[0].pid]);
    expect(deu.outbound.map((p) => p.pid)).toEqual([ps[1].pid]);
  });

  it('filters: basis, status, activity, window, economy', () => {
    const ps = [proj({ indexUse: false, status: 'Cancelled' }), proj({ yearAnnounced: 2010 }), proj({ activity: 'Research & Development', indexUse: false }), proj({ home: 9 })];
    expect(filterProjects(ps, { basis: 'index_use' }).length).toBe(2);
    expect(filterProjects(ps, { statuses: ['Cancelled'] }).length).toBe(1);
    expect(filterProjects(ps, { activities: ['Manufacturing'] }).length).toBe(3);
    expect(filterProjects(ps, { from: 2015 }).length).toBe(3);
    expect(filterProjects(ps, { geo: 9 }).length).toBe(1);
    expect(fdiTotals([]).yearMin).toBeNull();
  });
});

// ---- the bundle --------------------------------------------------------------------------------

/** The same value state, and the same number to within 1e-12 relative (null = NaN in the chunk). */
const same = (a: number | null, b: number) => (a === null || Number.isNaN(a) ? Number.isNaN(b) : !Number.isNaN(b) && Math.abs(a - b) <= 1e-12 * Math.max(1, Math.abs(b)));

describe.skipIf(!haveSynthetic())('CM-FDI on the R bundle', () => {
  const meta = haveSynthetic() ? (validateChunk('sector-meta', readSyntheticChunk('sector-meta')!) as SectorMetaChunk) : null;
  const raw = haveSynthetic() ? readSyntheticChunk('sector-fdi') : null;

  it.skipIf(!raw)('projects: an undisclosed value is null (never 0); links are http(s) only', () => {
    const chunk = validateChunk('sector-fdi', raw!, refSizes(meta!)) as SectorFdiChunk;
    const d = fdiData(chunk);
    const p = chunk['fdi.projects'].cols;
    expect(d.projects.length).toBe(chunk['fdi.projects'].nrow);
    for (const x of d.projects) {
      const nq = p.value_status[x.row] === 'nq';
      expect(x.value === null).toBe(nq);
      if (x.value !== null) expect(x.value).toBeGreaterThan(0);
      for (const c of d.sourcesOf(x.row)) for (const u of c.urls) expect(/^https?:\/\//.test(u)).toBe(true);
    }
    expect(fdiData(chunk)).toBe(d);
  });

  it.skipIf(!raw)('re-summing the projects reproduces fdi.flows, fdi.groups and fdi.years', () => {
    const chunk = validateChunk('sector-fdi', raw!, refSizes(meta!)) as SectorFdiChunk;
    const model = modelOf(meta!);
    const d = fdiData(chunk);
    const W = model.world?.idx ?? model.geoByCode.get('WLD')?.idx ?? -1;
    for (const basis of ['all', 'index_use'] as const) {
      const ps = filterProjects(d.projects, { basis });
      // flows
      const f = chunk['fdi.flows'];
      const mine = new Map(fdiFlows(ps).map((x) => [`${x.item}|${x.home}|${x.dest}`, x]));
      let nf = 0;
      for (let i = 0; i < f.nrow; i++) {
        if (f.cols.basis[i] !== basis) continue;
        nf++;
        const x = mine.get(`${f.cols.item[i]}|${f.cols.home[i]}|${f.cols.dest[i]}`);
        expect(x, `flow ${i}`).toBeDefined();
        expect([x!.n, x!.nValue, x!.yearMin, x!.yearMax]).toEqual([f.cols.n[i], f.cols.n_value[i], f.cols.year_min[i], f.cols.year_max[i]]);
        expect(same(x!.value, f.cols.value[i])).toBe(true);
      }
      expect(nf).toBe(mine.size);
      // groups
      const g = chunk['fdi.groups'];
      for (let i = 0; i < g.nrow; i++) {
        if (g.cols.basis[i] !== basis) continue;
        const item = g.cols.item[i];
        const dest = g.cols.dest[i];
        const split = fdiGroupSplit(ps.filter((x) => x.item === item && (dest === W || x.dest === dest)));
        const part = split.parts[g.cols.group[i] as FdiGroup];
        expect([part.n, part.nValue]).toEqual([g.cols.n[i], g.cols.n_value[i]]);
        expect(same(part.shareN, g.cols.share_n[i])).toBe(true);
        expect(same(part.value, g.cols.value[i])).toBe(true);
        expect(same(part.shareValue, g.cols.share_value[i])).toBe(true);
      }
      // years
      const y = chunk['fdi.years'];
      const rows = new Map<string, { n: number; nValue: number; value: number | null }>();
      for (const item of new Set(ps.map((x) => x.item))) for (const r of fdiYears(ps.filter((x) => x.item === item))) rows.set(`${item}|${r.year}|${r.group}`, r);
      let ny = 0;
      for (let i = 0; i < y.nrow; i++) {
        if (y.cols.basis[i] !== basis) continue;
        ny++;
        const r = rows.get(`${y.cols.item[i]}|${y.cols.year[i]}|${y.cols.group[i]}`);
        expect(r, `year row ${i}`).toBeDefined();
        expect([r!.n, r!.nValue]).toEqual([y.cols.n[i], y.cols.n_value[i]]);
        expect(same(r!.value, y.cols.value[i])).toBe(true);
      }
      expect(ny).toBe(rows.size);
    }
  });

  it.skipIf(!raw)('the CSV says "not disclosed", carries the CC BY attribution and only http(s) links', () => {
    const chunk = validateChunk('sector-fdi', raw!, refSizes(meta!)) as SectorFdiChunk;
    const model = modelOf(meta!);
    const d = fdiData(chunk);
    const csv = projectsCsv(model, d, d.projects, () => 'Inbound');
    const head = csv.split(/\r?\n/)[0];
    expect(head.startsWith('direction,project_id,')).toBe(true);
    expect(head).toContain('capital_investment_usd_m');
    expect(head).toContain('attribution');
    if (d.projects.some((x) => x.value === null)) expect(csv).toContain(NOT_DISCLOSED);
    expect(csv).toContain('CC BY 4.0');
    expect(csv).not.toMatch(/javascript:/i);
  });

  it('the measures: an investment layout in Explore, data in the four Midstream cells only', () => {
    const model = modelOf(meta!);
    const ids = ['investment.fdi_projects', 'investment.fdi_value', 'investment.fdi_parent_share'];
    for (const id of ids) {
      const v = model.varById.get(id);
      if (!v) continue;
      expect(isFdiVar(id)).toBe(true);
      expect(layoutOf(v)).toBe('fdi');
      expect(layoutBanner(v)).toMatch(/Not a country total/);
    }
    if (ids.some((id) => model.varById.has(id))) {
      expect(themeVars(model, 'investment').map((v) => v.id)).toEqual(expect.arrayContaining(ids.filter((id) => model.varById.has(id))));
      const covered = model.cells.filter((c) => hasFdi(model, c)).map((c) => c.slug).sort();
      expect(covered).toEqual(['batteries-midstream', 'electric-vehicles-midstream', 'solar-midstream', 'wind-midstream']);
    }
    expect(isFdiVar('investment.annual')).toBe(false);
    expect(hasFdi(model, null)).toBe(false);
  });
});
