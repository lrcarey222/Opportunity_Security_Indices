// Sectoral Compacts derivations on the synthetic fixture and hand-made cases: class order, colours and
// mix; coverage steps; nodes by class with the technology filter; concentration; Sankey flows (summed by
// pair and relationship, top N, node filter); the coalition curve and its target coalitions; the role
// matrix; the formats; the module's nav and the shell's place of /compacts paths.

import { afterEach, describe, expect, it } from 'vitest';
import type { AibCompact, AibDoc } from '../contract.ts';
import { classInfo, classMix, classOrder, CLASS_INFO, INTERVENTION_CLASSES, parseClassMix } from '../derive/classes.ts';
import { coalitionCurve, curveFloor, targetLabel, targetMarks } from '../derive/coalition.ts';
import { compactVsFragmented, coverageSteps, globalOptimum, largestGain, nodeCoverageSteps } from '../derive/coverage.ts';
import { capLabel, FRONTIER_SERIES, frontierPoints, frontierReadings, frontierRows, nodesWithFrontier } from '../derive/frontier.ts';
import { compactTypeInfo } from '../derive/types.ts';
import { investmentFlows, nodesWithRelationships, tradeFlows } from '../derive/flows.ts';
import { fmtPp, fmtShare, fmtUsd, listWords, nameOf, plural } from '../derive/format.ts';
import { classCounts, compactNodes, concentration, mixedTreatments, nodeCsvRow, nodeRow, nodesByClass, technologiesOf } from '../derive/nodes.ts';
import { canonicalRole, ROLE_ORDER, roleMatrix, rolesUsed } from '../derive/roles.ts';
import { aibNavItems, aibNavKeyOf, compactIdOfPath, rememberCompact, resetLastCompact } from '../nav.ts';
import { aibTabHref, lastLocations, moduleResumeHref, rememberLocation, resetLastLocations } from '../../shell/lastLocation.ts';
import { AIB_NAV, aibNavKeyOf as shellAibNavKeyOf, MODULES, placeOf } from '../../shell/modules.ts';
import { pageTitle } from '../../shell/title.ts';
import { readDoc, SYNTHETIC_DIR } from './fixture.ts';

const doc: AibDoc = readDoc(SYNTHETIC_DIR);
const names = doc.countries;
const c01 = doc.compacts[0];
const c02 = doc.compacts[1];
const nodeOf = (id: string) => doc.nodes.find((n) => n.id === id) ?? null;

afterEach(() => {
  resetLastCompact();
  resetLastLocations();
});

describe('classes', () => {
  it('orders the five classes, intervention first; unknown classes last and never a new colour', () => {
    expect(['LOW INTERVENTION', 'COORDINATE', 'INSUFFICIENT DATA', 'BUILD', 'MANAGE SPECIALIZATION'].sort((a, b) => classOrder(a) - classOrder(b))).toEqual([
      'COORDINATE', 'BUILD', 'MANAGE SPECIALIZATION', 'LOW INTERVENTION', 'INSUFFICIENT DATA',
    ]);
    expect(classOrder('NEW CLASS')).toBe(5);
    expect(classInfo('NEW CLASS')).toMatchObject({ label: 'NEW CLASS', mark: 'hatch' });
    expect(INTERVENTION_CLASSES.map((c) => CLASS_INFO[c].mark)).toEqual(['fill', 'fill', 'fill']);
    // the intervention hues are the validated categorical triple, each distinct
    expect(new Set(INTERVENTION_CLASSES.map((c) => CLASS_INFO[c].color)).size).toBe(3);
    expect(CLASS_INFO['LOW INTERVENTION'].mark).toBe('hollow');
    expect(CLASS_INFO['INSUFFICIENT DATA'].mark).toBe('hatch');
  });

  it('counts the class mix in view order', () => {
    expect(classMix(['BUILD', 'COORDINATE', 'BUILD'])).toEqual([
      { cls: 'COORDINATE', n: 1 },
      { cls: 'BUILD', n: 2 },
    ]);
    expect(classCounts(doc.nodes)).toEqual({ COORDINATE: 3, BUILD: 1, 'MANAGE SPECIALIZATION': 1, 'LOW INTERVENTION': 1, 'INSUFFICIENT DATA': 1 });
  });

  it("parses the compact's class mix of the lost supply", () => {
    expect(parseClassMix(c01.classMix)).toEqual([
      { cls: 'BUILD', share: 0.67 },
      { cls: 'MANAGE SPECIALIZATION', share: 0.33 },
    ]);
    expect(parseClassMix('BUILD 78%; COORDINATE 22%')).toEqual([
      { cls: 'BUILD', share: 0.78 },
      { cls: 'COORDINATE', share: 0.22 },
    ]);
    expect(parseClassMix('BUILD 12.5%; nonsense')).toEqual([{ cls: 'BUILD', share: 0.125 }]);
    expect(parseClassMix('')).toEqual([]);
    expect(parseClassMix(null)).toEqual([]);
    expect(parseClassMix(undefined)).toEqual([]);
  });
});

describe('coverage steps', () => {
  it('a compact: shock -> existing -> trade -> trade + investment, nulls kept', () => {
    const s = coverageSteps(c01.metrics);
    expect(s.map((x) => x.key)).toEqual(['static', 'existing', 'trade', 'investment']);
    expect(s.map((x) => x.value)).toEqual([0.3391, 0.4993, 0.79, 0.85]);
    expect(largestGain(s)).toMatchObject({ from: { key: 'existing' }, to: { key: 'trade' } });
    expect(largestGain(s)!.gain).toBeCloseTo(0.2907, 6);
    const missing = coverageSteps({ ...c01.metrics, investment: null });
    expect(missing[3].value).toBeNull();
    expect(largestGain([missing[3]])).toBeNull();
    // the trade compact keeps everyone's existing response, so it never falls below existing relationships
    for (const c of doc.compacts) {
      const v = coverageSteps(c.metrics).map((x) => x.value as number);
      expect(v[2], c.id).toBeGreaterThanOrEqual(v[1]);
      expect(v[3], c.id).toBeGreaterThanOrEqual(v[2]);
    }
  });

  it('the global optimum is a reference beside the steps; compact vs fragmented action for the anchors', () => {
    expect(globalOptimum(c01.metrics)).toBe(0.8117);
    expect(globalOptimum({ ...c01.metrics, globalOptimum: undefined })).toBeNull();
    expect(compactVsFragmented(c01.metrics)).toEqual({ fragmented: 0.47, compact: 0.93, gain: 0.46 });
    // the gain falls back to the difference; nothing when the export has neither coverage
    expect(compactVsFragmented({ ...c01.metrics, compactGain: null })!.gain).toBeCloseTo(0.46, 9);
    expect(compactVsFragmented({ ...c01.metrics, anchorFragmented: undefined, anchorCompact: undefined })).toBeNull();
    expect(compactVsFragmented({ ...c01.metrics, anchorFragmented: null, anchorCompact: 0.9, compactGain: null })).toEqual({ fragmented: null, compact: 0.9, gain: null });
  });

  it('compact types in words: the unnecessary one is muted and says by how much', () => {
    expect(compactTypeInfo('candidate compact')).toMatchObject({ short: 'Candidate compact', tone: 'compact' });
    expect(compactTypeInfo('bilateral arrangement sufficient')).toMatchObject({ short: 'Bilateral arrangement sufficient', tone: 'bilateral' });
    const u = compactTypeInfo('compact unnecessary: existing relationships do nearly as well', 0.025);
    expect(u).toMatchObject({ short: 'Compact unnecessary', tone: 'unnecessary' });
    expect(u.explanation).toMatch(/^Existing relationships do nearly as well: .*\(\+2\.5 pp\)\.$/);
    expect(compactTypeInfo('compact unnecessary: existing relationships do nearly as well', null).explanation).not.toMatch(/pp/);
    expect(compactTypeInfo('something new')).toMatchObject({ short: 'something new', tone: 'compact' });
  });

  it('a node has three steps', () => {
    const n = nodeOf('grid-mid-transformers')!;
    expect(nodeCoverageSteps(n).map((x) => [x.key, x.value])).toEqual([
      ['static', 0.88],
      ['existing', null],
      ['trade', null],
    ]);
  });
});

describe('nodes', () => {
  it('groups every node by class (all five groups), by lost supply within a group; filters by technology', () => {
    const g = nodesByClass(doc.nodes);
    expect(g.map((x) => x.cls)).toEqual(['COORDINATE', 'BUILD', 'MANAGE SPECIALIZATION', 'LOW INTERVENTION', 'INSUFFICIENT DATA']);
    expect(g[0].nodes.map((n) => n.label)).toEqual(['Anode Materials', 'Wind Towers & Nacelles', 'Natural Graphite']);
    expect(g[0].lostUsd).toBeCloseTo(2.68e9, 0);
    const wind = nodesByClass(doc.nodes, 'Wind');
    expect(wind.flatMap((x) => x.nodes).map((n) => n.id).sort()).toEqual(['ev-wind-mid-magnets', 'ev-wind-mid-ree-compounds', 'wind-down-towers']);
    expect(wind.find((x) => x.cls === 'COORDINATE')!.nodes.map((n) => n.id)).toEqual(['wind-down-towers']);
    expect(wind.find((x) => x.cls === 'LOW INTERVENTION')!.nodes).toEqual([]);
    expect(technologiesOf(doc.nodes)).toEqual(['Batteries', 'Electric Grid', 'Electric Vehicles', 'Solar', 'Wind']);
  });

  it("a compact's nodes in its order; concentration is the largest optimized supplier share", () => {
    const ns = compactNodes(c01, nodeOf);
    expect(ns.map((n) => n.id)).toEqual(['ev-wind-mid-magnets', 'ev-wind-mid-ree-compounds']);
    expect(compactNodes({ ...c01, nodes: ['ev-wind-mid-magnets', 'nope'] } as AibCompact, nodeOf)).toHaveLength(1);
    const k = concentration(ns)!;
    expect([k.node.id, k.share, k.supplier]).toEqual(['ev-wind-mid-ree-compounds', 0.58, 'CCC']);
    expect(concentration([nodeOf('grid-mid-transformers')!])).toBeNull();
    expect(mixedTreatments(ns)).toBe(true);
    expect(mixedTreatments(compactNodes(c02, nodeOf))).toBe(false);
  });

  it('table and CSV rows resolve names and join arrays', () => {
    const r = nodeRow(nodeOf('ev-wind-mid-magnets')!, names);
    expect(r).toMatchObject({ optimizedLargest: 'Borduria', baselineLargest: 'Zembla', hosts: 'Dorvalia; Elbonia', technologies: 'Electric Vehicles; Wind', compact: 'AIB-C01' });
    const csv = nodeCsvRow(r);
    expect(Object.keys(csv)[0]).toBe('node_id');
    expect(csv.classification).toBe('BUILD');
    expect(nodeRow(nodeOf('grid-mid-transformers')!, names).optimizedLargest).toBeNull();
  });
});

describe('specialization-resilience frontier', () => {
  it('rows by supplier limit with the second shock and the specialization; one point per line and limit', () => {
    const n = nodeOf('bat-up-graphite')!;
    const rows = frontierRows(n, names);
    expect(rows.map((r) => r.cap)).toEqual([0.25, 0.4, 0.6, 1]);
    expect(rows[0]).toMatchObject({ coverage: 0.97, secondShock: 0.8051, specialization: 0.48, largestName: 'Avalon', largestShare: 0.17 });
    expect(frontierPoints(rows)).toHaveLength(12);
    expect(FRONTIER_SERIES.map((s) => s.key)).toEqual(['coverage', 'secondShock', 'specialization']);
    // a frontier written before the additive fields: coverage only
    const old = { ...n, frontier: n.frontier.map((f) => ({ cap: f.cap, coverage: f.coverage, largestShare: f.largestShare, largest: f.largest })) };
    expect(frontierPoints(frontierRows(old, names)).map((p) => p.key)).toEqual(['coverage', 'coverage', 'coverage', 'coverage']);
    expect(nodesWithFrontier(doc.nodes).map((x) => x.id)).not.toContain('grid-mid-transformers');
  });

  it('the two readings in words: a limit, none reached, or not in the export', () => {
    expect(frontierReadings(nodeOf('bat-up-graphite')!)).toEqual({ costless: '25%', required: '25%' });
    expect(frontierReadings(nodeOf('ev-wind-mid-magnets')!)).toEqual({ costless: '40%', required: 'no tested limit reaches it' });
    const { tightestCostless: _t, minRequiredDiversification: _m, ...old } = nodeOf('bat-up-graphite')!;
    void _t;
    void _m;
    expect(frontierReadings({ ...old, frontier: [] })).toEqual({ costless: null, required: null });
    expect(capLabel(1)).toBe('no limit (100%)');
    expect(capLabel(0.4)).toBe('40%');
    expect(capLabel(null)).toBe('–');
  });
});

describe('Sankey flows', () => {
  it('trade: summed by exporter, importer and relationship, largest first, top N, node filter', () => {
    const all = tradeFlows(c01);
    // CCC -> BBB appears as new (compounds) and expanded (magnets): two flows
    expect(all.filter((f) => f.producer === 'CCC' && f.consumer === 'BBB').map((f) => f.relationship).sort()).toEqual(['expanded', 'new']);
    expect(all[0]).toMatchObject({ producer: 'BBB', consumer: 'AAA', value: 1.4e9, relationship: 'expanded' });
    expect(all.map((f) => f.value)).toEqual([...all.map((f) => f.value)].sort((a, b) => b - a));
    expect(tradeFlows(c01, null, 2)).toHaveLength(2);
    expect(tradeFlows(c01, 'ev-wind-mid-ree-compounds').every((f) => f.nodes.every((n) => n === 'ev-wind-mid-ree-compounds'))).toBe(true);
    // a sum over nodes
    const twice = { ...c01, trade: [...c01.trade, { ...c01.trade[0], node: 'ev-wind-mid-magnets' }] };
    expect(tradeFlows(twice).find((f) => f.producer === 'CCC' && f.consumer === 'AAA')).toMatchObject({ value: 2.2e9, nodes: ['ev-wind-mid-ree-compounds', 'ev-wind-mid-magnets'] });
    // non-positive, null and self flows are dropped
    const odd = { ...c01, trade: [{ ...c01.trade[0], usd: null }, { ...c01.trade[0], usd: -1 }, { ...c01.trade[0], to: 'CCC' }] };
    expect(tradeFlows(odd)).toEqual([]);
  });

  it('investment: summed by investor and host; none in a compact without investment', () => {
    expect(investmentFlows(c01).map((f) => [f.producer, f.consumer, f.value])).toEqual([
      ['AAA', 'DDD', 0.48e9],
      ['BBB', 'DDD', 0.31e9],
      ['AAA', 'EEE', 0.22e9],
    ]);
    expect(investmentFlows(c02)).toEqual([]);
    expect(nodesWithRelationships(c02)).toEqual(['bat-mid-anode', 'bat-up-graphite']);
  });
});

describe('coalition curve', () => {
  it('one point per step, members counted, names resolved, step 0 the anchors', () => {
    const pts = coalitionCurve(c01, names);
    expect(pts.map((p) => [p.step, p.n, p.addedName])).toEqual([
      [0, 2, 'Demand anchors'],
      [1, 3, 'Carpania'],
      [2, 4, 'Dorvalia'],
      [3, 5, 'Elbonia'],
      [4, 6, 'Freedonia'],
      [5, 7, 'Genovia'],
    ]);
    expect(pts[1].nodes['ev-wind-mid-magnets']).toBe(0.8);
  });

  it('marks the first step reaching each target, which matches the exported coalitions', () => {
    const pts = coalitionCurve(c01, names);
    const marks = targetMarks(pts, doc.meta.targets);
    expect(marks.map((m) => [m.target, m.point?.n ?? null])).toEqual([
      [0.9, 4],
      [0.95, 6],
      [0.99, 7],
    ]);
    expect(marks.map((m) => m.point?.members.length)).toEqual([c01.coalition.t90.length, c01.coalition.t95.length, c01.coalition.t99.length]);
    // a path that never reaches the top target
    const m2 = targetMarks(coalitionCurve(c02, names), doc.meta.targets);
    expect(m2.map((m) => m.point?.n ?? null)).toEqual([2, 2, null]);
    expect(c02.coalition.reached99).toBe(false);
    expect(curveFloor(pts)).toBe(0.6);
    expect(curveFloor([])).toBe(0);
    expect(targetLabel(0.95)).toBe('95%');
    expect(targetLabel(0.9)).toBe('90%');
  });
});

describe('role matrix', () => {
  it('canonical roles, the long Supplier form included; unknown roles are kept aside', () => {
    expect(canonicalRole('Supplier (coalition member)')).toBe('Supplier');
    expect(canonicalRole('demand anchor')).toBe('Demand anchor');
    expect(canonicalRole('Capacity-building / investment host')).toBe('Capacity-building / investment host');
    expect(canonicalRole('Cheerleader')).toBeNull();
    expect(ROLE_ORDER).toHaveLength(8);
  });

  it('one row per economy, several roles each; anchors first, members, then associates', () => {
    const rows = roleMatrix(c01, names);
    expect(rows.map((r) => r.name)).toEqual(['Avalon', 'Borduria', 'Carpania', 'Dorvalia', 'Elbonia', 'Freedonia', 'Genovia']);
    const ccc = rows.find((r) => r.iso3 === 'CCC')!;
    expect(Object.keys(ccc.roles).sort()).toEqual(['Specialized supplier', 'Technology partner']);
    expect(rows.find((r) => r.iso3 === 'FFF')!.roles.Supplier).toBe('adds supply in the coalition test');
    expect(rows.find((r) => r.iso3 === 'GGG')!.member).toBe(false);
    expect(rolesUsed(rows)).toEqual([...ROLE_ORDER]);
    const odd = roleMatrix({ ...c01, roles: [{ iso3: 'QQQ', role: 'Cheerleader', basis: 'b', member: true }] }, names);
    expect(odd[0]).toMatchObject({ name: 'QQQ', other: [{ role: 'Cheerleader', basis: 'b' }] });
  });
});

describe('formats', () => {
  it('shares, percentage points, dollars, names and lists', () => {
    expect(fmtShare(0.6609)).toBe('66.1%');
    expect(fmtShare(null)).toBe('–');
    expect(fmtPp(0.3121)).toBe('+31.2 pp');
    expect(fmtPp(-0.04)).toBe('−4.0 pp');
    expect(fmtPp(0)).toBe('0.0 pp');
    expect(fmtUsd(13.8e9)).toBe('$13.8 bn');
    expect(fmtUsd(240e9)).toBe('$240 bn');
    expect(fmtUsd(340e6)).toBe('$340 m');
    expect(fmtUsd(4.5e6)).toBe('$4.5 m');
    expect(fmtUsd(250e3)).toBe('$0.25 m');
    expect(fmtUsd(0)).toBe('$0');
    expect(fmtUsd(null)).toBe('–');
    expect(nameOf(names, 'AAA')).toBe('Avalon');
    expect(nameOf(names, 'QQQ')).toBe('QQQ');
    expect(listWords(['a', 'b', 'c'])).toBe('a, b and c');
    expect(plural(1, 'node')).toBe('1 node');
    expect(plural(2, 'economy', 'economies')).toBe('2 economies');
  });
});

describe('nav and the shell', () => {
  it('the module nav: Compacts, the compact on screen (else the last, else the first) and About', () => {
    expect(aibNavItems('/compacts', 'AIB-C01')).toEqual([
      { key: 'compacts', label: 'Compacts', to: '/compacts', active: true },
      { key: 'compact', label: 'Compact', to: '/compacts/AIB-C01', active: false },
      { key: 'about', label: 'About & data', to: '/compacts/about', active: false },
    ]);
    expect(aibNavItems('/compacts/AIB-C02', 'AIB-C01')[1]).toEqual({ key: 'compact', label: 'Compact', to: '/compacts/AIB-C02', active: true });
    rememberCompact('AIB-C02');
    expect(aibNavItems('/compacts/about', 'AIB-C01').map((i) => [i.to, i.active])).toEqual([
      ['/compacts', false],
      ['/compacts/AIB-C02', false],
      ['/compacts/about', true],
    ]);
    resetLastCompact();
    expect(aibNavItems('/compacts', null).map((i) => i.key)).toEqual(['compacts', 'about']);
    expect(compactIdOfPath('/compacts/AIB-C01')).toBe('AIB-C01');
    expect(compactIdOfPath('/compacts/about')).toBeNull();
    expect(compactIdOfPath('/compacts')).toBeNull();
    expect(compactIdOfPath('/countries/JPN')).toBeNull();
    expect(aibNavKeyOf('/compacts/About')).toBe('about');
    expect(aibNavKeyOf('/overview')).toBeNull();
  });

  it('the shell knows the fourth module: place, static nav, tab and resume targets, titles', () => {
    expect(MODULES.find((m) => m.id === 'aib')).toEqual({ id: 'aib', label: 'Sectoral Compacts', tag: 'COORDINATION DESIGN', home: '/compacts' });
    for (const p of ['/compacts', '/compacts/about', '/compacts/AIB-C01']) expect(placeOf(p)).toBe('aib');
    expect(placeOf('/compact')).toBeNull();
    expect(AIB_NAV.map((n) => n.label)).toEqual(['Compacts', 'About & data']);
    expect(shellAibNavKeyOf('/compacts/AIB-C01')).toBe('compact');
    expect(shellAibNavKeyOf('/compacts')).toBe('compacts');
    expect(aibTabHref('/overview')).toBe('/compacts');
    expect(moduleResumeHref('aib')).toBe('/compacts');
    rememberLocation('/compacts/AIB-C02', '?x=1');
    expect(lastLocations().aib).toBe('/compacts/AIB-C02?x=1');
    expect(aibTabHref('/sector')).toBe('/compacts/AIB-C02?x=1');
    expect(aibTabHref('/compacts/AIB-C02')).toBe('/compacts');
    expect(moduleResumeHref('aib')).toBe('/compacts/AIB-C02?x=1');
    resetLastLocations();
    expect(lastLocations().aib).toBeUndefined();
    expect(pageTitle('About Sectoral Compacts', 'aib')).toBe('About Sectoral Compacts | Sectoral Compacts | Allied Industrial Base Explorer | NEIS Center');
  });
});
