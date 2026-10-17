// Sectoral Compacts derivations on the synthetic fixture and hand-made cases: class order, colours and
// mix; policy needs in words; coverage steps; nodes by class with the technology and strategic-scope
// filters; concentration; the frontier's two N-1 readings (and an older bundle's secondShock); the class
// variants and the growth-bound sensitivity; Sankey flows (summed by pair and relationship, top N, node
// filter); the coalition curve and its target marks; the exact vs greedy coalition (the tie-break, an
// unreached target's widest set, an empty member list) and the six tiers with what would replace a member;
// the physical-capability flag of every class shown; the role matrix; the run settings in words; the
// formats; the module's nav and the shell's place of /compacts paths.

import { afterEach, describe, expect, it } from 'vitest';
import { coalitionMembersOf, type AibCompact, type AibDoc } from '../contract.ts';
import { classInfo, classMix, classOrder, CLASS_INFO, INTERVENTION_CLASSES, needsAddToClass, parseClassMix, parseNodePolicyNeeds, policyNeedsText } from '../derive/classes.ts';
import { coalitionCurve, curveFloor, targetLabel, targetMarks } from '../derive/coalition.ts';
import { compactVsFragmented, coverageSteps, globalOptimum, largestGain, nodeCoverageSteps } from '../derive/coverage.ts';
import { capLabel, FRONTIER_SERIES, frontierPoints, frontierReadings, frontierRows, hasReoptimizedN1, nodesWithFrontier } from '../derive/frontier.ts';
import { compactTypeInfo } from '../derive/types.ts';
import { investmentFlows, nodesWithRelationships, tradeFlows } from '../derive/flows.ts';
import { compactTitle, fmtPp, fmtShare, fmtUsd, listWords, nameOf, plural } from '../derive/format.ts';
import { classCounts, compactNodes, concentration, mixedTreatments, nodeCsvRow, nodeRow, nodesByClass, outOfScopeCount, scopedNodes, technologiesOf } from '../derive/nodes.ts';
import { canonicalRole, ROLE_ORDER, roleMatrix, rolesUsed } from '../derive/roles.ts';
import { countryRoleRow, countryRoleRows, countryRolesUsed } from '../derive/countryRoles.ts';
import { buildGaps, dividendOf, dpRows, filterChoices, hostRows, investmentNodes, NO_FILTER, pairInvestmentFlows, pairTradeFlows, specializedSuppliers, topNodes, topPairs } from '../derive/partnerships.ts';
import { mapPoints } from '../charts/StrategicMap.tsx';
import {
  classVariants, constraintClasses, growthBoundWords, growthFlag, growthRows, robustnessSummary, variantWarning, warnedGrowthBounds, warnedVariants,
} from '../derive/sensitivity.ts';
import { candidateUniverse, coverageBase, demandScope, shockSemantics, strategicScope, supplyConstraint } from '../derive/settings.ts';
import {
  additionsNeeded, coalitionMethod, coalitionTargets, coalitionTiers, isGreedySetMethod, replacementWords, tierEconomies, tierName, tierShares,
} from '../derive/tiers.ts';
import { aibNavItems, aibNavKeyOf, compactIdOfPath, rememberCompact, resetLastCompact } from '../nav.ts';
import { aibTabHref, lastLocations, moduleResumeHref, rememberLocation, resetLastLocations } from '../../shell/lastLocation.ts';
import { AIB_NAV, aibNavKeyOf as shellAibNavKeyOf, MODULES, placeOf } from '../../shell/modules.ts';
import { pageTitle } from '../../shell/title.ts';
import { readDoc, stripMethodology, SYNTHETIC_DIR } from './fixture.ts';

const doc: AibDoc = readDoc(SYNTHETIC_DIR);
const older: AibDoc = stripMethodology(structuredClone(doc));
const names = doc.countries;
const c01 = doc.compacts[0];
const c02 = doc.compacts[1];
const c03 = doc.compacts[2];
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
    expect(classCounts(doc.nodes)).toEqual({ COORDINATE: 3, BUILD: 2, 'MANAGE SPECIALIZATION': 1, 'LOW INTERVENTION': 1, 'INSUFFICIENT DATA': 1 });
    expect(classCounts(scopedNodes(doc.nodes))).toEqual({ COORDINATE: 3, BUILD: 1, 'MANAGE SPECIALIZATION': 1, 'LOW INTERVENTION': 1, 'INSUFFICIENT DATA': 1 });
  });

  it('policy needs in words, in the class order; whether they say more than the class', () => {
    expect(policyNeedsText(['MANAGE SPECIALIZATION', 'BUILD'])).toBe('BUILD + MANAGE SPECIALIZATION');
    expect(policyNeedsText([])).toBe('none');
    expect(policyNeedsText(undefined)).toBeNull();
    expect(policyNeedsText(nodeOf('ev-wind-mid-ree-compounds')!.policyNeeds)).toBe('COORDINATE + MANAGE SPECIALIZATION');
    expect(needsAddToClass('BUILD', ['BUILD'])).toBe(false);
    expect(needsAddToClass('BUILD', ['COORDINATE', 'BUILD'])).toBe(true);
    // a LOW INTERVENTION node can still need coordination
    expect(needsAddToClass('LOW INTERVENTION', ['COORDINATE'])).toBe(true);
    expect(needsAddToClass('LOW INTERVENTION', [])).toBe(false);
    expect(parseNodePolicyNeeds(c01.nodePolicyNeeds)).toEqual([
      { node: 'Permanent Magnets & Magnet Parts', needs: 'COORDINATE + BUILD' },
      { node: 'Rare-Earth Compounds', needs: 'COORDINATE + MANAGE SPECIALIZATION' },
    ]);
    expect(parseNodePolicyNeeds('Rare-Earth Compounds + Ores: none')).toEqual([{ node: 'Rare-Earth Compounds + Ores', needs: 'none' }]);
    expect(parseNodePolicyNeeds(null)).toEqual([]);
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
    expect(technologiesOf(doc.nodes)).toEqual(['Batteries', 'Electric Grid', 'Electric Vehicles', 'Natural Gas', 'Solar', 'Wind']);
  });

  it('the strategic scope: in-scope nodes by default, every node on request; an older bundle has every node in scope', () => {
    expect(outOfScopeCount(doc.nodes)).toBe(1);
    expect(scopedNodes(doc.nodes).map((n) => n.id)).not.toContain('gas-mid-turbines');
    expect(scopedNodes(doc.nodes, true)).toHaveLength(doc.nodes.length);
    expect(technologiesOf(scopedNodes(doc.nodes))).not.toContain('Natural Gas');
    expect(nodesByClass(scopedNodes(doc.nodes)).find((g) => g.cls === 'BUILD')!.nodes.map((n) => n.id)).toEqual(['ev-wind-mid-magnets']);
    // every compact node is in scope
    for (const c of doc.compacts) for (const id of c.nodes) expect(nodeOf(id)!.inStrategicScope, id).toBe(true);
    expect(outOfScopeCount(older.nodes)).toBe(0);
    expect(scopedNodes(older.nodes)).toHaveLength(older.nodes.length);
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

  it('table and CSV rows carry the policy needs, the warning, the physical validation, the scope, both constraint classes and the N-1 readings', () => {
    const r = nodeRow(nodeOf('wind-down-towers')!, names);
    expect(r).toMatchObject({
      policyNeeds: 'COORDINATE', warning: 'PHYSICAL CAPABILITY NOT VALIDATED', physicalValidation: 'trade only; physical data incomplete', physicalData: 'incomplete',
      inStrategicScope: true, productionConstrainedCls: 'COORDINATE', tradeOnlyCls: 'COORDINATE', coordinateSensitiveToGrowth: true, n1Reoptimized: 0.77, instantaneousN1: 0.6696,
    });
    const csv = nodeCsvRow(r);
    expect(csv).toMatchObject({ policy_needs: 'COORDINATE', classification_warning: 'PHYSICAL CAPABILITY NOT VALIDATED', in_strategic_scope: true, class_trade_only: 'COORDINATE', n1_reoptimized_coverage: 0.77 });
    expect(nodeRow(nodeOf('gas-mid-turbines')!, names)).toMatchObject({ inStrategicScope: false, policyNeeds: 'COORDINATE + BUILD', warning: null });
    // an older bundle: none of them, and every node in scope
    const o = nodeRow(older.nodes[0], names);
    expect(o).toMatchObject({ policyNeeds: null, warning: null, physicalValidation: null, inStrategicScope: true, tradeOnlyCls: null, coordinateSensitiveToGrowth: null, n1Reoptimized: null });
  });

  it('table and CSV rows carry the warning of every class shown and the growth-bound flag', () => {
    // a node whose headline class is validated, but not its trade-only class
    const ree = nodeRow(nodeOf('ev-wind-mid-ree-compounds')!, names);
    expect(ree).toMatchObject({ warning: null, tradeOnlyCls: 'COORDINATE', tradeOnlyWarning: true, productionConstrainedWarning: false, warnedVariants: 'trade_only', warnedGrowthBounds: '', growthFlag: null });
    expect(nodeCsvRow(ree)).toMatchObject({ class_trade_only_warning: true, class_production_constrained_warning: false, warning_under_variants: 'trade_only', warning_under_growth_bounds: '' });
    const towers = nodeRow(nodeOf('wind-down-towers')!, names);
    expect(towers.warnedGrowthBounds).toBe('1.25x; 2x; 4x; unlimited');
    expect(nodeCsvRow(towers).growth_bound_flag).toBe(
      'Headline COORDINATE; LOW INTERVENTION (PHYSICAL CAPABILITY NOT VALIDATED) at 4x and unlimited: the class depends on the existing-relationship growth bound.',
    );
    // an older bundle does not say
    expect(nodeRow(older.nodes[0], names)).toMatchObject({ tradeOnlyWarning: null, productionConstrainedWarning: null, warnedVariants: null, warnedGrowthBounds: null, growthFlag: null });
    // a node without growth rows in a current export: nothing flagged
    expect(nodeRow(nodeOf('grid-mid-transformers')!, names)).toMatchObject({ warnedVariants: '', warnedGrowthBounds: '' });
  });
});

describe('specialization-resilience frontier', () => {
  it('rows by supplier limit with both N-1 readings and the specialization; one point per line and limit', () => {
    const n = nodeOf('bat-up-graphite')!;
    const rows = frontierRows(n, names);
    expect(rows.map((r) => r.cap)).toEqual([0.25, 0.4, 0.6, 1]);
    expect(rows[0]).toMatchObject({ coverage: 0.97, n1Reoptimized: 0.88, instantaneousN1: 0.8051, n1Removed: 'AAA', n1RemovedName: 'Avalon', specialization: 0.48, largestName: 'Avalon', largestShare: 0.17 });
    expect(frontierPoints(rows)).toHaveLength(16);
    expect(FRONTIER_SERIES.map((s) => s.key)).toEqual(['coverage', 'n1Reoptimized', 'instantaneousN1', 'specialization']);
    expect(FRONTIER_SERIES.map((s) => s.label)).toEqual([
      'Coverage',
      'N-1 re-optimized (largest remaining supplier removed, trade re-optimized)',
      'Instantaneous N-1 retained coverage (coverage × (1 − largest share))',
      'Specialization score',
    ]);
    // every line its own dash pattern and colour
    expect(new Set(FRONTIER_SERIES.map((s) => s.dash)).size).toBe(4);
    expect(new Set(FRONTIER_SERIES.map((s) => s.color)).size).toBe(4);
    // the instantaneous reading is mechanical: coverage x (1 - largest share)
    for (const r of rows) expect(r.instantaneousN1!).toBeCloseTo(r.coverage! * (1 - r.largestShare!), 4);
    expect(hasReoptimizedN1(n)).toBe(true);
    // a frontier written before the additive fields: coverage only
    const bare = { ...n, frontier: n.frontier.map((f) => ({ cap: f.cap, coverage: f.coverage, largestShare: f.largestShare, largest: f.largest })) };
    expect(frontierPoints(frontierRows(bare, names)).map((p) => p.key)).toEqual(['coverage', 'coverage', 'coverage', 'coverage']);
    // an older bundle: its secondShock is the instantaneous reading; no re-optimized one
    const o = older.nodes.find((x) => x.id === 'bat-up-graphite')!;
    expect(frontierRows(o, names)[0]).toMatchObject({ instantaneousN1: 0.8051, n1Reoptimized: null, n1Removed: null });
    expect(new Set(frontierPoints(frontierRows(o, names)).map((p) => p.key))).toEqual(new Set(['coverage', 'instantaneousN1', 'specialization']));
    expect(hasReoptimizedN1(o)).toBe(false);
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

  it('marks the first step of the greedy path reaching each target; the exact coalitions can be smaller', () => {
    const pts = coalitionCurve(c01, names);
    const marks = targetMarks(pts, doc.meta.targets);
    expect(marks.map((m) => [m.target, m.point?.n ?? null])).toEqual([
      [0.9, 4],
      [0.95, 6],
      [0.99, 7],
    ]);
    // here the greedy path finds the exact coalitions
    expect(marks.map((m) => m.point?.members.length)).toEqual([c01.coalition.t90.length, c01.coalition.t95.length, c01.coalition.t99.length]);
    // a path that never reaches the top target, and that takes three economies where the exact program needs two
    const m2 = targetMarks(coalitionCurve(c02, names), doc.meta.targets);
    expect(m2.map((m) => m.point?.n ?? null)).toEqual([3, 3, null]);
    expect(c02.coalition.t95).toHaveLength(2);
    expect(c02.coalition.reached99).toBe(false);
    expect(curveFloor(pts)).toBe(0.6);
    expect(curveFloor([])).toBe(0);
    expect(targetLabel(0.95)).toBe('95%');
    expect(targetLabel(0.9)).toBe('90%');
  });
});

describe('coalition: exact, greedy and tiers', () => {
  it('the method: exact when the middle target is reached, the greedy coalition beside it only when it differs', () => {
    expect(coalitionTargets(doc.meta.targets)).toEqual({ mid: 0.95, top: 0.99 });
    const m1 = coalitionMethod(c01, doc.meta.targets);
    expect(m1).toMatchObject({ exact: true, reached: true, greedyDiffers: false, label: 'Exact minimum coalition' });
    expect(m1.method).toMatch(/^exact \(binary program/);
    const m2 = coalitionMethod(c02, doc.meta.targets);
    expect(m2).toMatchObject({ exact: true, greedyDiffers: true, greedy: ['AAA', 'BBB', 'CCC'] });
    // the tie-break among minimum-size coalitions: solved, or not
    expect(m1.tieBreakSolved).toBe(true);
    expect(m2.tieBreakSolved).toBe(false);
    // the middle target unreached: the method says so, the members are the widest greedy coalition, never called exact
    const m3 = coalitionMethod(c03, doc.meta.targets);
    expect(m3).toMatchObject({ exact: false, reached: false, widest: true, greedy: [], greedyDiffers: false, members: ['AAA', 'EEE', 'FFF'] });
    expect(m3.label).toBe('Widest coalition found (the 95% target is not reached)');
    expect(isGreedySetMethod(c03.coalition.method)).toBe(true);
    expect(isGreedySetMethod(c01.coalition.method)).toBe(false);
    // the exact program stopped at its time limit: minimality not proven
    const timed = { ...c01, coalition: { ...c01.coalition, method: `${c01.coalition.method}; the solver hit its time limit: best coalition found, minimality not proven` } };
    expect(coalitionMethod(timed, doc.meta.targets)).toMatchObject({ exact: false, proven: false, reached: true, label: 'Best coalition found (minimality not proven)' });
    // an older bundle: no method, no greedy coalition, no tie-break flag
    expect(coalitionMethod(older.compacts[0], older.meta.targets)).toMatchObject({ method: null, exact: false, reached: true, greedy: null, tieBreakSolved: null, label: 'Minimum coalition' });
  });

  it('an export that left the members of an unreached target empty: the members are the roles with member = true', () => {
    const empty = { ...c03, coalition: { ...c03.coalition, members: [] } };
    expect(coalitionMembersOf(empty)).toEqual(['AAA', 'EEE', 'FFF']);
    expect(coalitionMethod(empty, doc.meta.targets).members).toEqual(['AAA', 'EEE', 'FFF']);
    expect(tierEconomies(empty, names).filter((e) => e.member).map((e) => e.iso3)).toEqual(['AAA', 'EEE', 'FFF']);
    expect(coalitionMembersOf(c01)).toBe(c01.coalition.members);
  });

  it('the six tiers in order, each economy once, with critical and leave-one-out flags and single substitutes', () => {
    const shares = tierShares(doc.compacts);
    expect(shares).toEqual({ criticalMin: '1%', externalMin: '5%' });
    const g = coalitionTiers(c01, names, doc.meta.targets, shares);
    expect(g.map((x) => x.label)).toEqual([
      'Core coordinating members', 'Critical supply partners', 'Associate partners', 'Critical external dependencies', 'Critical external dependencies (marginal)', 'External dependencies',
    ]);
    expect(g.map((x) => x.economies.map((e) => e.iso3))).toEqual([['AAA', 'BBB', 'DDD', 'EEE', 'FFF'], ['CCC'], ['GGG'], ['HHH'], ['JJJ'], ['III']]);
    const ccc = g[1].economies[0];
    // a critical supply partner has no single substitute: two economies together replace it
    expect(ccc).toMatchObject({ critical: true, looCritical: true, substitutes: [], substitutesNeeded: 2, member: true });
    // the tier roles are not functional roles
    expect(ccc.roles).toEqual(['Specialized supplier', 'Technology partner']);
    expect(g[0].economies[0]).toMatchObject({ iso3: 'AAA', anchor: true, critical: false, looCritical: false, substitutesNeeded: 0 });
    expect(g[0].economies.find((e) => e.iso3 === 'EEE')).toMatchObject({ substitutes: ['GGG', 'HHH'], substituteNames: ['Genovia', 'Hyperborea'], substitutesNeeded: 1, looCritical: true });
    expect(g[3].economies[0].critical).toBe(true);
    expect(g[4].economies[0]).toMatchObject({ iso3: 'JJJ', critical: true, member: false, substitutesNeeded: null });
    // what each tier means
    expect(g[0].meaning).toMatch(/that are not critical supply partners: the demand anchors, and suppliers one other candidate could replace/);
    expect(g[1].meaning).toMatch(/no single other candidate can replace: without one, only several other economies together restore the target, or none does/);
    expect(g[2].meaning).toMatch(/^The fewest additions to the core coalition, held in, that reach the 99% target\. The 99% minimum coalition solved on its own need not contain the core\.$/);
    expect(g[3].meaning).toMatch(/each supplies at least 1% of the demand anchors' demand there/);
    expect(g[4].meaning).toMatch(/under 1% of the anchors' demand there: critical only because the coalition sits near the target/);
    expect(g[5].meaning).toMatch(/at least 5% of the demand anchors' demand/);
    // members = core + critical supply partners
    expect([...g[0].economies, ...g[1].economies].map((e) => e.iso3).sort()).toEqual([...c01.coalition.members].sort());
    // associates that do not reach the top target say so; an unreached middle target qualifies the core and critical tiers
    expect(coalitionTiers(c02, names, doc.meta.targets)[2].meaning).toMatch(/\(here no additions reach it\)/);
    const g3 = coalitionTiers(c03, names, doc.meta.targets);
    expect(g3[0].meaning).toMatch(/^No coalition reaches the 95% target: the members are the widest coalition the greedy path found/);
    expect(g3[1]).toMatchObject({ economies: [] });
    expect(g3[1].meaning).toMatch(/^Not tested/);
    expect(tierEconomies(c02, names).map((e) => [e.iso3, e.tier])).toEqual([
      ['AAA', 'core coordinating member'],
      ['CCC', 'critical supply partner'],
      ['BBB', 'external dependency'],
    ]);
    expect(tierName('critical external dependency')).toBe('Critical external dependency');
    expect(tierName('critical external dependency (marginal)')).toBe('Critical external dependency (marginal)');
    expect(tierName(null)).toBe('–');
    // an older bundle: members are core, the other role holders associates
    expect(tierEconomies(older.compacts[0], names).map((e) => [e.iso3, e.tier])).toEqual([
      ['AAA', 'core coordinating member'],
      ['BBB', 'core coordinating member'],
      ['CCC', 'core coordinating member'],
      ['DDD', 'core coordinating member'],
      ['EEE', 'core coordinating member'],
      ['FFF', 'core coordinating member'],
      ['GGG', 'associate partner'],
    ]);
  });

  it('what would replace a member: several economies together or none for a critical partner, single substitutes as alternatives for a core member', () => {
    const t1 = new Map(tierEconomies(c01, names).map((e) => [e.iso3, e]));
    const t2 = new Map(tierEconomies(c02, names).map((e) => [e.iso3, e]));
    expect(replacementWords(t1.get('CCC')!, 0.95, true)).toBe('no single other economy can replace it: without it, the coalition needs 2 other economies together to reach the 95% target');
    expect(replacementWords(t2.get('CCC')!, 0.95, true)).toBe('without it, no coalition reaches the 95% target');
    expect(replacementWords(t1.get('EEE')!, 0.95, true)).toBe('any one of Genovia or Hyperborea could replace its supply');
    expect(replacementWords(t1.get('DDD')!, 0.95, true)).toBe('Genovia could replace its supply');
    // a demand anchor the rest does without: nothing to add; a non-member: nothing
    expect(replacementWords(t1.get('AAA')!, 0.95, true)).toBe('');
    expect(replacementWords(t1.get('HHH')!, 0.95, true)).toBe('');
    // a supplier the rest of the coalition does without
    expect(replacementWords({ ...t1.get('FFF')!, substitutes: [], substituteNames: [], substitutesNeeded: 0 }, 0.95, true)).toBe('the rest of the coalition reaches the 95% target without its supply');
    // five listed: the exporter keeps the five that supply most
    const five = ['A', 'B', 'C', 'D', 'E'];
    expect(replacementWords({ ...t1.get('FFF')!, substitutes: five, substituteNames: five }, 0.95, true)).toBe('any one of A, B, C, D or E (the five that supply most) could replace its supply');
    // the target out of reach: nothing tested
    expect(tierEconomies(c03, names).map((e) => replacementWords(e, 0.95, false))).toEqual(['', '', '']);
    // an export before substitutesNeeded: its substitutes were what the exact program added
    expect(replacementWords({ ...t1.get('CCC')!, substitutes: ['GGG', 'HHH'], substituteNames: ['Genovia', 'Hyperborea'], substitutesNeeded: undefined }, 0.95, true)).toBe(
      'without it, the exact program adds Genovia and Hyperborea',
    );
    // the table column
    expect(additionsNeeded(t1.get('CCC')!, true)).toBe('2');
    expect(additionsNeeded(t2.get('CCC')!, true)).toBe('none suffices');
    expect(additionsNeeded(t1.get('AAA')!, true)).toBe('0');
    expect(additionsNeeded(t1.get('HHH')!, true)).toBeNull();
    expect(additionsNeeded(tierEconomies(c03, names)[0], false)).toBeNull();
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

  it('one row per economy, several roles each, with its tier; anchors first, members, associates, then external dependencies', () => {
    const rows = roleMatrix(c01, names);
    expect(rows.map((r) => r.name)).toEqual(['Avalon', 'Borduria', 'Carpania', 'Dorvalia', 'Elbonia', 'Freedonia', 'Genovia', 'Hyperborea', 'Jotunheim', 'Illyria']);
    expect(rows.find((r) => r.iso3 === 'JJJ')).toMatchObject({ member: false, tier: 'critical external dependency (marginal)', critical: true, roles: {}, other: [] });
    const ccc = rows.find((r) => r.iso3 === 'CCC')!;
    expect(Object.keys(ccc.roles).sort()).toEqual(['Specialized supplier', 'Technology partner']);
    // the tier role is the tier, not an unknown role
    expect(ccc).toMatchObject({ tier: 'critical supply partner', critical: true, other: [] });
    expect(rows.find((r) => r.iso3 === 'FFF')!.roles.Supplier).toBe('adds supply in the coalition test');
    expect(rows.find((r) => r.iso3 === 'GGG')).toMatchObject({ member: false, tier: 'associate partner' });
    expect(rows.find((r) => r.iso3 === 'HHH')).toMatchObject({ member: false, tier: 'critical external dependency', roles: {}, other: [] });
    expect(rows.find((r) => r.iso3 === 'III')).toMatchObject({ member: false, tier: 'external dependency' });
    expect(rolesUsed(rows)).toEqual([...ROLE_ORDER]);
    const odd = roleMatrix({ ...c01, roles: [{ iso3: 'QQQ', role: 'Cheerleader', basis: 'b', member: true }] }, names);
    expect(odd[0]).toMatchObject({ name: 'QQQ', other: [{ role: 'Cheerleader', basis: 'b' }] });
  });
});

describe('class variants and the growth-bound sensitivity', () => {
  it('the class under each variant, grouped, with the headline setting of each group', () => {
    const v = classVariants(nodeOf('ev-wind-mid-ree-compounds')!, doc.meta);
    expect(v.map((r) => [r.group, r.key, r.cls, r.headline, r.differs])).toEqual([
      ['Supply constraint', 'production_constrained', 'MANAGE SPECIALIZATION', true, false],
      ['Supply constraint', 'trade_only', 'COORDINATE', false, true],
      ['Shock semantics', 'hold', 'MANAGE SPECIALIZATION', true, false],
      ['Shock semantics', 'release', 'MANAGE SPECIALIZATION', false, false],
      ['Supply scenario', 'reallocation', 'MANAGE SPECIALIZATION', true, false],
      ['Supply scenario', 'conservative', 'BUILD', false, true],
      ['Shock', 'zzz_100', 'MANAGE SPECIALIZATION', true, false],
      ['Shock', 'zzz_50', 'MANAGE SPECIALIZATION', false, false],
      ['Shock', 'largest_subsector', 'BUILD', false, true],
    ]);
    expect(v.find((r) => r.key === 'trade_only')!.setting).toBe('Trade-only');
    expect(v.find((r) => r.key === 'zzz_50')!.setting).toBe('Zembla supply halved');
    expect(constraintClasses(nodeOf('bat-up-graphite')!)).toEqual({
      productionConstrained: 'COORDINATE', tradeOnly: 'LOW INTERVENTION', productionConstrainedWarning: false, tradeOnlyWarning: true, differs: true,
    });
    expect(constraintClasses(older.nodes[0])).toBeNull();
    expect(classVariants(older.nodes[0], older.meta)).toEqual([]);
  });

  it('every class shown carries its own warning: per variant, the headline settings from the headline warning', () => {
    // the headline class is validated; the trade-only one is not
    const ree = classVariants(nodeOf('ev-wind-mid-ree-compounds')!, doc.meta);
    expect(ree.filter((r) => r.warning).map((r) => r.key)).toEqual(['trade_only']);
    expect(ree.every((r) => typeof r.warning === 'boolean')).toBe(true);
    // a warned headline: the headline settings the export does not list as variants carry it too
    const towers = classVariants(nodeOf('wind-down-towers')!, doc.meta);
    expect(towers.filter((r) => r.headline).map((r) => [r.key, r.warning])).toEqual([
      ['production_constrained', true],
      ['hold', true],
      ['reallocation', true],
      ['zzz_100', true],
    ]);
    expect(variantWarning(nodeOf('solar-down-modules')!, 'conservative')).toBe(false);
    expect(warnedVariants(nodeOf('bat-up-graphite')!)).toEqual(['trade_only']);
    // an export that wrote [] for an empty named list, and one that predates the flags
    expect(warnedVariants({ ...nodeOf('bat-up-graphite')!, variantWarnings: [] as unknown as Record<string, boolean> })).toEqual([]);
    const { variantWarnings: _vw, ...noFlags } = nodeOf('bat-up-graphite')!;
    void _vw;
    expect(variantWarning(noFlags, 'trade_only')).toBeNull();
    expect(classVariants(noFlags, doc.meta).every((r) => r.warning === null || r.headline)).toBe(true);
    expect(warnedVariants(noFlags)).toBeNull();
    // growth bounds
    expect(growthRows(nodeOf('solar-down-modules')!).map((g) => g.warning)).toEqual([true, true, true, true]);
    expect(growthRows(nodeOf('bat-mid-anode')!).map((g) => g.warning)).toEqual([false, false, false, false]);
    expect(warnedGrowthBounds(nodeOf('bat-mid-anode')!)).toEqual([]);
    expect(warnedGrowthBounds({ ...nodeOf('bat-mid-anode')!, growth: nodeOf('bat-mid-anode')!.growth!.map(({ warning: _w, ...g }) => (void _w, g)) })).toBeNull();
    // the disclosure summary
    expect(robustnessSummary(nodeOf('ev-wind-mid-ree-compounds')!, doc.meta)).toBe(
      'Class under other assumptions and growth bounds · differs under 3 variants · physical capability not validated under 1 other setting',
    );
  });

  it('growth rows tightest first with the 2x headline; a COORDINATE class that depends on the bound is flagged', () => {
    const towers = nodeOf('wind-down-towers')!;
    expect(growthRows(towers).map((g) => [g.label, g.cls, g.headline])).toEqual([
      ['1.25x', 'COORDINATE', false],
      ['2x', 'COORDINATE', true],
      ['4x', 'LOW INTERVENTION', false],
      ['unlimited', 'LOW INTERVENTION', false],
    ]);
    // the headline row is the node's own existing coverage and dividend
    expect(growthRows(towers)[1]).toMatchObject({ existing: towers.existing, dividend: towers.dividend });
    // every class named under other bounds carries its own physical-capability flag
    expect(growthFlag(towers)).toBe(
      'Headline COORDINATE; LOW INTERVENTION (PHYSICAL CAPABILITY NOT VALIDATED) at 4x and unlimited: the class depends on the existing-relationship growth bound.',
    );
    // a LOW INTERVENTION node that is COORDINATE only under the tightest bound is flagged too, worded from its headline class
    expect(growthFlag(nodeOf('solar-down-modules')!)).toBe(
      'Headline LOW INTERVENTION; COORDINATE (PHYSICAL CAPABILITY NOT VALIDATED) only at 1.25x: the class depends on the existing-relationship growth bound.',
    );
    // warned at some of its bounds only: the warning names those bounds; unwarned: no tag
    const partly = { ...towers, growth: towers.growth!.map((g) => (g.label === 'unlimited' ? { ...g, warning: false } : g)) };
    expect(growthFlag(partly)).toBe(
      'Headline COORDINATE; LOW INTERVENTION at 4x and unlimited (PHYSICAL CAPABILITY NOT VALIDATED at 4x): the class depends on the existing-relationship growth bound.',
    );
    const unwarned = { ...towers, growth: towers.growth!.map((g) => ({ ...g, warning: false })) };
    expect(growthFlag(unwarned)).toBe('Headline COORDINATE; LOW INTERVENTION at 4x and unlimited: the class depends on the existing-relationship growth bound.');
    expect(growthFlag(nodeOf('bat-mid-anode')!)).toBeNull();
    expect(growthRows(nodeOf('grid-mid-transformers')!)).toEqual([]);
    expect(growthRows(older.nodes[0])).toEqual([]);
    expect(growthBoundWords('1.25x')).toBe('up to 1.25× its observed size');
    expect(growthBoundWords('unlimited')).toBe('no bound');
  });
});

describe('run settings in words', () => {
  it('demand scope and candidate partners apart, semantics, constraint and strategic scope; nothing for an older bundle', () => {
    // "coalition" is the allied core and partner tiers, not a compact's coalition
    expect(demandScope(doc.meta)).toEqual({
      mode: 'coalition',
      label: 'Allied core and partner economies',
      words: "the allied core and partner economies of the coalition table (6 economies), not a compact's coalition: the importers whose lost supply is restored",
    });
    expect(demandScope({ ...doc.meta, demandScope: 'global', nDemandScope: null })!.words).toBe('every economy in the trade data: the importers whose lost supply is restored');
    expect(candidateUniverse(doc.meta)).toMatchObject({ mode: 'global', words: 'any economy in the trade data may add or redirect supply' });
    expect(shockSemantics(doc.meta)).toMatchObject({ mode: 'hold', label: 'Hold' });
    expect(shockSemantics(doc.meta)!.words).toMatch(/keeps buying, so other exporters' sales to it stay with it in both supply scenarios/);
    expect(shockSemantics({ ...doc.meta, shockSemantics: 'release' })!.words).toMatch(/that share of other exporters' sales to it is freed/);
    // production-constrained: physical evidence decides who can supply; shares never become trade values
    expect(supplyConstraint(doc.meta)).toMatchObject({ mode: 'production_constrained', label: 'Production-constrained' });
    const pc = supplyConstraint(doc.meta)!.words;
    expect(pc).toMatch(/physical evidence decides who can supply: an exporter the data list as a producer keeps its full trade response/);
    expect(pc).toMatch(/keeps its current shipments to the demand scope but cannot expand or redirect supply/);
    expect(pc).toMatch(/Production shares are not converted into trade values/);
    expect(pc).not.toMatch(/world trade|larger of/);
    // any configured strategic scope is shown as written
    expect(strategicScope({ ...doc.meta, strategicScope: 'critical_minerals', scopeTechs: ['Lithium'] })).toMatchObject({ mode: 'critical_minerals', label: 'critical_minerals', techs: ['Lithium'] });
    const st = strategicScope(doc.meta)!;
    expect(st).toMatchObject({ mode: 'electro_industrial', label: 'Electro-industrial' });
    expect(st.words).toMatch(/^compacts are formed only from the nodes of Batteries, .* and Nuclear; other nodes stay in the data, outside the headline$/);
    expect(coverageBase(doc.meta)).toBe("the demand scope's baseline traded demand");
    for (const f of [demandScope, candidateUniverse, shockSemantics, supplyConstraint, strategicScope]) expect(f(older.meta)).toBeNull();
    expect(coverageBase(older.meta)).toBe("the universe's baseline traded demand");
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
  it('the module nav (IA refactor): Overview, Partnerships, Sectoral Compacts (every compact page), Country roles', () => {
    expect(aibNavItems('/aib', 'AIB-C01')).toEqual([
      { key: 'overview', label: 'Overview', to: '/aib', active: true },
      { key: 'partnerships', label: 'Partnerships', to: '/aib/partnerships', active: false },
      { key: 'compacts', label: 'Sectoral Compacts', to: '/compacts', active: false },
      { key: 'roles', label: 'Country roles', to: '/aib/roles', active: false },
    ]);
    expect(aibNavItems('/compacts/AIB-C02').filter((i) => i.active).map((i) => i.key)).toEqual(['compacts']);
    expect(aibNavItems('/compacts/about').filter((i) => i.active)).toEqual([]);
    rememberCompact('AIB-C02');
    resetLastCompact();
    expect(compactIdOfPath('/compacts/AIB-C01')).toBe('AIB-C01');
    expect(compactIdOfPath('/compacts/about')).toBeNull();
    expect(compactIdOfPath('/compacts')).toBeNull();
    expect(compactIdOfPath('/countries/JPN')).toBeNull();
    expect(aibNavKeyOf('/compacts/About')).toBe('about');
    expect(aibNavKeyOf('/aib/roles')).toBe('roles');
    expect(aibNavKeyOf('/overview')).toBeNull();
  });

  it('the shell knows the module: place, static nav, tab and resume targets, titles', () => {
    expect(MODULES.find((m) => m.id === 'aib')).toMatchObject({ id: 'aib', label: 'Allied Industrial Base', home: '/aib' });
    for (const p of ['/compacts', '/compacts/about', '/compacts/AIB-C01', '/aib', '/aib/roles']) expect(placeOf(p)).toBe('aib');
    expect(placeOf('/compact')).toBeNull();
    expect(AIB_NAV.map((n) => n.label)).toEqual(['Overview', 'Partnerships', 'Sectoral Compacts', 'Country roles']);
    expect(shellAibNavKeyOf('/compacts/AIB-C01')).toBe('compacts');
    expect(shellAibNavKeyOf('/compacts')).toBe('compacts');
    expect(aibTabHref('/overview')).toBe('/aib');
    expect(moduleResumeHref('aib')).toBe('/aib');
    rememberLocation('/compacts/AIB-C02', '?x=1');
    expect(lastLocations().aib).toBe('/compacts/AIB-C02?x=1');
    expect(aibTabHref('/sector')).toBe('/compacts/AIB-C02?x=1');
    expect(aibTabHref('/compacts/AIB-C02')).toBe('/aib');
    expect(moduleResumeHref('aib')).toBe('/compacts/AIB-C02?x=1');
    resetLastLocations();
    expect(lastLocations().aib).toBeUndefined();
    expect(pageTitle('About Sectoral Compacts', 'aib')).toBe('About Sectoral Compacts | Allied Industrial Base | Allied Industrial Base Explorer | NEIS Center');
  });
});

describe('country roles across compacts (IA refactor)', () => {
  it("gathers each economy's roles and seats from the compacts, with no score of its own", () => {
    const rows = countryRoleRows(doc, names);
    expect(rows.length).toBeGreaterThan(0);
    for (const c of doc.compacts) {
      for (const r of c.roles) {
        const k = canonicalRole(r.role);
        if (k) expect(rows.find((x) => x.iso3 === r.iso3)?.roles[k]).toContain(c.id);
      }
      for (const iso of c.coalition.members) expect(rows.find((x) => x.iso3 === iso)?.member).toContain(c.id);
    }
    const seats = (r: (typeof rows)[number]) => new Set([...r.member, ...r.associate]).size;
    for (let i = 1; i < rows.length; i++) expect(seats(rows[i - 1])).toBeGreaterThanOrEqual(seats(rows[i]));
    expect(countryRoleRow(doc, names, rows[0].iso3.toLowerCase())).toEqual(rows[0]);
    expect(countryRoleRow(doc, names, 'ZZZ')).toBeNull();
    expect(countryRolesUsed(rows).every((r) => (ROLE_ORDER as readonly string[]).includes(r))).toBe(true);
  });
});

describe('partnerships and the overview (IA refactor)', () => {
  it('sums the model trade edges by exporter, importer and relationship across compacts, largest first; filters by economy and node', () => {
    const all = pairTradeFlows(doc, NO_FILTER);
    const edges = doc.compacts.flatMap((c) => c.trade).filter((e) => typeof e.usd === 'number' && e.usd > 0 && e.from !== e.to);
    expect(all.reduce((s, p) => s + p.value, 0)).toBeCloseTo(edges.reduce((s, e) => s + (e.usd as number), 0), 3);
    for (let i = 1; i < all.length; i++) expect(all[i - 1].value).toBeGreaterThanOrEqual(all[i].value);
    const iso = all[0].producer;
    expect(pairTradeFlows(doc, { ...NO_FILTER, country: iso }).every((p) => p.producer === iso || p.consumer === iso)).toBe(true);
    const node = edges[0].node;
    expect(pairTradeFlows(doc, { ...NO_FILTER, node }).every((p) => p.nodes.every((n) => n === node))).toBe(true);
    const inv = pairInvestmentFlows(doc, NO_FILTER);
    const invEdges = doc.compacts.flatMap((c) => c.investment).filter((e) => typeof e.usd === 'number' && e.usd > 0 && e.investor !== e.host);
    expect(inv.reduce((s, p) => s + p.value, 0)).toBeCloseTo(invEdges.reduce((s, e) => s + (e.usd as number), 0), 3);
  });

  it('the filter choices, the hosts, the specialized suppliers and the capacity gaps come from the nodes as exported', () => {
    const ch = filterChoices(doc, NO_FILTER);
    expect(ch.techs).toEqual([...new Set(doc.nodes.flatMap((n) => n.techs))].sort((a, b) => a.localeCompare(b, 'en')));
    const tech = ch.techs[0];
    expect(filterChoices(doc, { ...NO_FILTER, tech }).nodes.every((n) => n.techs.includes(tech))).toBe(true);
    const hosts = hostRows(doc.nodes, NO_FILTER);
    for (const h of hosts) for (const id of h.first) expect(nodeOf(id)?.hosts[0]).toBe(h.iso3);
    const sup = specializedSuppliers(doc.nodes, NO_FILTER);
    expect(sup.reduce((s, r) => s + r.nodes.length, 0)).toBe(doc.nodes.filter((n) => n.largest).length);
    const gaps = buildGaps(doc.nodes, NO_FILTER);
    expect(gaps.every((n) => n.cls === 'BUILD')).toBe(true);
    expect(gaps.length).toBe(doc.nodes.filter((n) => n.cls === 'BUILD').length);
    const inv = investmentNodes(doc.nodes, NO_FILTER);
    expect(inv.every((n) => n.hosts.length > 0)).toBe(true);
  });

  it('the coordination dividend is the exported one, else optimized minus existing; top nodes skip missing values', () => {
    const n = doc.nodes[0];
    expect(dividendOf({ ...n, dividend: 0.12 })).toBe(0.12);
    expect(dividendOf({ ...n, dividend: null, optimized: 0.9, existing: 0.6 })).toBeCloseTo(0.3);
    expect(dividendOf({ ...n, dividend: null, optimized: null, existing: 0.6 })).toBeNull();
    const top = topNodes(doc.nodes, (x) => x.exposure, 3);
    expect(top.length).toBeLessThanOrEqual(3);
    for (let i = 1; i < top.length; i++) expect(top[i - 1].exposure as number).toBeGreaterThanOrEqual(top[i].exposure as number);
    const pts = mapPoints(doc.nodes);
    expect(pts.every((p) => p.x === p.node.exposure && p.y === dividendOf(p.node))).toBe(true);
  });

  it('top partner pairs: an economy by its own rank, else the highest scores; development potential by economy within the scope', () => {
    const rows = [
      { reporter_iso: 'AAA', side: 'fs' as const, partner_iso: 'BBB', value: 0.4, rank: 2 },
      { reporter_iso: 'AAA', side: 'fs' as const, partner_iso: 'CCC', value: 0.3, rank: 1 },
      { reporter_iso: 'DDD', side: 'fs' as const, partner_iso: 'AAA', value: 0.9, rank: 1 },
      { reporter_iso: 'DDD', side: 'po' as const, partner_iso: 'AAA', value: 1, rank: 1 },
      { reporter_iso: 'EEE', side: 'fs' as const, partner_iso: 'EEE', value: 1, rank: 1 },
    ];
    expect(topPairs(rows, 'fs', 'AAA').map((r) => r.partner_iso)).toEqual(['CCC', 'BBB']);
    expect(topPairs(rows, 'fs', null).map((r) => r.reporter_iso + r.partner_iso)).toEqual(['DDDAAA', 'AAABBB', 'AAACCC']);
    const cells = [
      { tech: 'Solar', supply_chain: 'Midstream' },
      { tech: 'Wind', supply_chain: 'Upstream' },
    ];
    const dp = [
      { partner_iso: 'AAA', cell_id: 1, value: 0.2 },
      { partner_iso: 'AAA', cell_id: 2, value: 0.7 },
      { partner_iso: 'BBB', cell_id: 1, value: 0.5 },
      { partner_iso: 'CCC', cell_id: 1, value: null },
    ];
    expect(dpRows(dp, cells, [], NO_FILTER).map((r) => [r.iso3, r.value, r.tech])).toEqual([
      ['AAA', 0.7, 'Wind'],
      ['BBB', 0.5, 'Solar'],
    ]);
    expect(dpRows(dp, cells, [], { ...NO_FILTER, tech: 'Solar' }).map((r) => [r.iso3, r.value])).toEqual([
      ['BBB', 0.5],
      ['AAA', 0.2],
    ]);
  });
});

describe('compactTitle (display only)', () => {
  it('shortens an exporter name cut to "and N more"; keeps other names whole', () => {
    const nodes = Array.from({ length: 10 }, (_, i) => `n${i}`);
    expect(compactTitle({ name: 'Electric Vehicles/Wind: Charging Infrastructure, Electric Drive Units & Motors, Motor & Generator Parts, and 7 more', nodes })).toBe(
      'Electric Vehicles/Wind: Charging Infrastructure and 9 more',
    );
    expect(compactTitle({ name: 'Solar: Silicon Metal, Solar Cells', nodes: ['a', 'b'] })).toBe('Solar: Silicon Metal, Solar Cells');
  });
});
