// Sectoral Compacts derivations on the synthetic fixture and hand-made cases: class order, colours and
// mix; policy needs in words; coverage steps; nodes by class with the technology and strategic-scope
// filters; concentration; the frontier's two N-1 readings (and an older bundle's secondShock); the class
// variants and the growth-bound sensitivity; Sankey flows (summed by pair and relationship, top N, node
// filter); the coalition curve and its target marks; the exact vs greedy coalition (the tie-break, an
// unreached target's widest set, an empty member list) and the six tiers with what would replace a member;
// the physical-capability flag of every class shown; the role matrix; the run settings in words; the
// formats; the module's nav and the shell's place of /compacts paths; the diversification scenario (rows,
// the demand split, totals, order, filters, the node block per status, the CSV row, the variant class).

import { afterEach, describe, expect, it } from 'vitest';
import { AIB_DIVERSIFY_STATUSES, coalitionMembersOf, type AibCompact, type AibDoc, type AibNode } from '../contract.ts';
import { classInfo, classMix, classOrder, CLASS_INFO, INTERVENTION_CLASSES, needsAddToClass, parseClassMix, parseNodePolicyNeeds, policyNeedsText } from '../derive/classes.ts';
import { coalitionCurve, curveFloor, targetLabel, targetMarks } from '../derive/coalition.ts';
import { compactVsFragmented, coverageSteps, globalOptimum, largestGain, nodeCoverageSteps } from '../derive/coverage.ts';
import { capLabel, FRONTIER_SERIES, frontierPoints, frontierReadings, frontierRows, hasReoptimizedN1, nodesWithFrontier } from '../derive/frontier.ts';
import { compactTypeInfo } from '../derive/types.ts';
import { investmentFlows, nodesWithRelationships, tradeFlows } from '../derive/flows.ts';
import { compactTitle, fmtPp, fmtShare, fmtUsd, joinLabels, listWords, nameOf, plural } from '../derive/format.ts';
import { classCounts, compactNodes, concentration, mixedTreatments, nodeCsvRow, nodeRow, nodesByClass, outOfScopeCount, scopedNodes, technologiesOf } from '../derive/nodes.ts';
import { canonicalRole, ROLE_ORDER, roleMatrix, rolesUsed } from '../derive/roles.ts';
import { countryRoleRow, countryRoleRows, countryRolesUsed } from '../derive/countryRoles.ts';
import {
  buildGaps, dividendOf, dpRows, filterChoices, hostRows, investmentNodes, NO_FILTER, osiCellPick, pairInvestmentFlows, pairTradeFlows, scoredCellsOf, specializedSuppliers, topNodes, topPairs,
} from '../derive/partnerships.ts';
import {
  groupedSectorWords, indexLabel, indexOfNode, indexOfSector, indicesOfCompact, nodeWhere, osiCellHref, osiCellOf, profiledCellsOfNode, sectorGroups, taxonomyOf, techOptions,
} from '../derive/taxonomy.ts';
import { mapPoints } from '../charts/StrategicMap.tsx';
import {
  classVariants, constraintClasses, growthBoundWords, growthFlag, growthRows, robustnessSummary, variantWarning, warnedGrowthBounds, warnedVariants,
} from '../derive/sensitivity.ts';
import {
  candidateUniverse, coverageBase, demandScope, DIVERSIFY_BENCHMARK_WORDS, DIVERSIFY_LABEL, diversifyRules, diversifySetting, HHI_WORDS, nodeTaxonomy, shockSemantics, strategicScope,
  strategicScopeShort, supplyConstraint,
} from '../derive/settings.ts';
import {
  additionsNeeded, coalitionMethod, coalitionTargets, coalitionTiers, isGreedySetMethod, replacementWords, tierEconomies, tierName, tierShares,
} from '../derive/tiers.ts';
import { aibNavItems, aibNavKeyOf, compactIdOfPath, rememberCompact, resetLastCompact } from '../nav.ts';
import { aibTabHref, lastLocations, moduleResumeHref, rememberLocation, resetLastLocations } from '../../shell/lastLocation.ts';
import { AIB_NAV, aibNavKeyOf as shellAibNavKeyOf, MODULES, placeOf } from '../../shell/modules.ts';
import { pageTitle } from '../../shell/title.ts';
import { countryEdges, countryRoleMatrix, countryRolesOf, edgeWords, everyShock, fmtPts, namedRoles, roleHolders, sideLevel } from '../derive/network.ts';
import { cellWords } from '../views/networkParts.tsx';
import { hostRanksOf, hostsOfNode, investmentPairs, investorHostFlows, partnerTotals, rankWords } from '../derive/investment.ts';
import { readDoc, stripDiversify, stripMethodology, SYNTHETIC_DIR, toMasterTaxonomy } from './fixture.ts';
import {
  aboveWords, basisWords, benchmarkWords, cappedWords, demandSplit, DIVERSIFY_STATUS_WORDS, diversifyClassText, diversifyCsvRow, diversifyIndices, diversifyLabel, diversifyRow, diversifyRows,
  rowStatusLabel,
  diversifySectorOptions, diversifyTotals, diversifyWords, filterDiversify, hasDiversify, hhiAxisMax, NO_DIVERSIFY_FILTER, orderDiversify, relationshipsOf, relationshipWords, statusWords,
  totalsWords,
} from '../derive/diversify.ts';
import { hhiTicks } from '../charts/DiversifyChart.tsx';

// the synthetic fixture is a dictionary bundle (strategic scope all); `master` is its OSI-basket form
// (strategic scope electro_industrial, the electrolysers node outside it); `older` predates both
const doc: AibDoc = readDoc(SYNTHETIC_DIR);
const master: AibDoc = toMasterTaxonomy(structuredClone(doc));
const older: AibDoc = stripMethodology(structuredClone(doc));
const tx = taxonomyOf(doc.meta);
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
    // strategic scope all: every node; the master form leaves the electrolysers (BUILD) outside
    expect(classCounts(scopedNodes(doc.nodes))).toEqual(classCounts(doc.nodes));
    expect(classCounts(scopedNodes(master.nodes))).toEqual({ COORDINATE: 3, BUILD: 1, 'MANAGE SPECIALIZATION': 1, 'LOW INTERVENTION': 1, 'INSUFFICIENT DATA': 1 });
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
    // node labels with commas survive (the parts end where the needs end)
    expect(parseNodePolicyNeeds(c01.nodePolicyNeeds)).toEqual([
      { node: 'Magnets, magnet parts & assemblies', needs: 'COORDINATE + BUILD' },
      { node: 'Rare-earth oxides & compounds', needs: 'COORDINATE + MANAGE SPECIALIZATION' },
    ]);
    expect(parseNodePolicyNeeds('Rare-Earth Compounds + Ores: none')).toEqual([{ node: 'Rare-Earth Compounds + Ores', needs: 'none' }]);
    // a label holding ": " or "; " (the dictionary has none, an older segment name had "PGMs: iridium/ruthenium") stays whole
    expect(parseNodePolicyNeeds('PGMs: iridium/ruthenium: BUILD; Odd; label: none; Electric buses, trucks & tractors: COORDINATE + BUILD')).toEqual([
      { node: 'PGMs: iridium/ruthenium', needs: 'BUILD' },
      { node: 'Odd; label', needs: 'none' },
      { node: 'Electric buses, trucks & tractors', needs: 'COORDINATE + BUILD' },
    ]);
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
  it('groups every node by class (all five groups), by lost supply within a group; filters by sector (dictionary) or technology (master)', () => {
    const g = nodesByClass(doc.nodes);
    expect(g.map((x) => x.cls)).toEqual(['COORDINATE', 'BUILD', 'MANAGE SPECIALIZATION', 'LOW INTERVENTION', 'INSUFFICIENT DATA']);
    expect(g[0].nodes.map((n) => n.label)).toEqual(['Synthetic graphite (anode-grade, spherical)', 'Wind towers, nacelles & hubs', 'Natural graphite']);
    expect(g[0].lostUsd).toBeCloseTo(2.68e9, 0);
    const graphite = nodesByClass(doc.nodes, 'Graphite');
    expect(graphite.flatMap((x) => x.nodes).map((n) => n.id).sort()).toEqual(['bat-mid-anode', 'bat-up-graphite']);
    expect(graphite.find((x) => x.cls === 'LOW INTERVENTION')!.nodes).toEqual([]);
    // the dictionary's sectors, "/" kept; never an OSI technology name the dictionary does not use
    expect(technologiesOf(doc.nodes)).toEqual(['Electrolysers/H2 equipment', 'Graphite', 'Magnets', 'Onshore wind', 'Rare earths', 'Solar', 'Transformers']);
    expect(nodesByClass(doc.nodes, 'Wind').flatMap((x) => x.nodes)).toEqual([]);
    // the master form: OSI technologies from the crosswalk
    const wind = nodesByClass(master.nodes, 'Wind');
    expect(wind.flatMap((x) => x.nodes).map((n) => n.id).sort()).toEqual(['ev-wind-mid-magnets', 'ev-wind-mid-ree-compounds', 'wind-down-towers']);
    expect(wind.find((x) => x.cls === 'COORDINATE')!.nodes.map((n) => n.id)).toEqual(['wind-down-towers']);
    expect(technologiesOf(master.nodes)).toEqual(['Batteries', 'Electric Grid', 'Electric Vehicles', 'Green Hydrogen', 'Solar', 'Wind']);
  });

  it('the strategic scope: in-scope nodes by default, every node on request; all in a dictionary bundle; an older bundle has every node in scope', () => {
    // the dictionary bundle's scope is "all"
    expect(outOfScopeCount(doc.nodes)).toBe(0);
    expect(scopedNodes(doc.nodes)).toHaveLength(doc.nodes.length);
    // a configured scope (the master form): the electrolysers node, as Green Hydrogen, is outside
    expect(outOfScopeCount(master.nodes)).toBe(1);
    expect(scopedNodes(master.nodes).map((n) => n.id)).not.toContain('h2-down-electrolysers');
    expect(scopedNodes(master.nodes, true)).toHaveLength(master.nodes.length);
    expect(technologiesOf(scopedNodes(master.nodes))).not.toContain('Green Hydrogen');
    expect(nodesByClass(scopedNodes(master.nodes)).find((g) => g.cls === 'BUILD')!.nodes.map((n) => n.id)).toEqual(['ev-wind-mid-magnets']);
    // every compact node is in scope
    for (const c of master.compacts) for (const id of c.nodes) expect(master.nodes.find((n) => n.id === id)!.inStrategicScope, id).toBe(true);
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

  it('table and CSV rows resolve names and join arrays; a dictionary node names its sectors and index, never technologies', () => {
    const r = nodeRow(nodeOf('ev-wind-mid-magnets')!, names, tx);
    expect(r).toMatchObject({
      optimizedLargest: 'Borduria', baselineLargest: 'Zembla', hosts: 'Dorvalia; Elbonia', technologies: 'Magnets', compact: 'AIB-C01',
      dictionary: true, indexCode: 'B', indexName: 'Electrotechnologies', osiCells: 'electric-vehicles-midstream; wind-midstream',
    });
    const csv = nodeCsvRow(r);
    expect(Object.keys(csv).slice(0, 7)).toEqual(['node_id', 'sub_sector', 'sectors', 'index_code', 'index_name', 'osi_cells', 'stage']);
    expect(csv).not.toHaveProperty('technologies');
    expect(csv.classification).toBe('BUILD');
    expect(nodeRow(nodeOf('grid-mid-transformers')!, names).optimizedLargest).toBeNull();
    // a node with no basket code: no OSI cell
    expect(nodeCsvRow(nodeRow(nodeOf('h2-down-electrolysers')!, names, tx))).toMatchObject({ sectors: 'Electrolysers/H2 equipment', index_code: 'F', osi_cells: '' });
    // master (or no taxonomy given): the technologies column, no index
    const m = nodeCsvRow(nodeRow(master.nodes.find((n) => n.id === 'ev-wind-mid-magnets')!, names, taxonomyOf(master.meta)));
    expect(m).toMatchObject({ technologies: 'Electric Vehicles; Wind' });
    expect(m).not.toHaveProperty('sectors');
    expect(m).not.toHaveProperty('index_code');
  });

  it('table and CSV rows carry the policy needs, the warning, the physical validation, the scope, both constraint classes and the N-1 readings', () => {
    const r = nodeRow(nodeOf('wind-down-towers')!, names);
    expect(r).toMatchObject({
      policyNeeds: 'COORDINATE', warning: 'PHYSICAL CAPABILITY NOT VALIDATED', physicalValidation: 'trade only; physical data incomplete', physicalData: 'incomplete',
      inStrategicScope: true, productionConstrainedCls: 'COORDINATE', tradeOnlyCls: 'COORDINATE', coordinateSensitiveToGrowth: true, n1Reoptimized: 0.77, instantaneousN1: 0.6696,
    });
    const csv = nodeCsvRow(r);
    expect(csv).toMatchObject({ policy_needs: 'COORDINATE', classification_warning: 'PHYSICAL CAPABILITY NOT VALIDATED', in_strategic_scope: true, class_trade_only: 'COORDINATE', n1_reoptimized_coverage: 0.77 });
    expect(nodeRow(nodeOf('h2-down-electrolysers')!, names)).toMatchObject({ inStrategicScope: true, policyNeeds: 'COORDINATE + BUILD', warning: null });
    expect(nodeRow(master.nodes.find((n) => n.id === 'h2-down-electrolysers')!, names)).toMatchObject({ inStrategicScope: false, technologies: 'Green Hydrogen' });
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
      // the diversification scenario: its own group; within the cap, so it did not run
      ['Diversification', 'diversify_benchmark', null, false, false],
    ]);
    expect(v.filter((r) => r.notRun).map((r) => r.key)).toEqual(['diversify_benchmark']);
    expect(v.find((r) => r.key === 'diversify_benchmark')!.setting).toBe("Diversified to its sector's concentration");
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
    expect(strategicScope({ ...master.meta, strategicScope: 'critical_minerals', scopeTechs: ['Lithium'] })).toMatchObject({ mode: 'critical_minerals', label: 'critical_minerals', techs: ['Lithium'] });
    const st = strategicScope(master.meta)!;
    expect(st).toMatchObject({ mode: 'electro_industrial', label: 'Electro-industrial' });
    expect(st.words).toMatch(/^compacts are formed only from the nodes of Batteries, .* and Nuclear; other nodes stay in the data, outside the headline$/);
    expect(strategicScope({ ...master.meta, strategicScope: 'all' })).toMatchObject({ label: 'All technologies', words: 'compacts can be formed from the nodes of every technology' });
    // the dictionary bundle: every sector, in words of sectors and indices
    expect(strategicScope(doc.meta)).toMatchObject({
      mode: 'all', label: 'All sectors', words: 'compacts can be formed from the nodes of every sector of the electro-industrial dictionary (8 sectors in 6 indices)',
    });
    expect([strategicScopeShort(doc.meta), strategicScopeShort(master.meta), strategicScopeShort(master.meta, 'all')]).toEqual(['all sectors', 'electro-industrial technologies', 'all technologies']);
    // where the nodes come from
    const nt = nodeTaxonomy(doc.meta);
    expect(nt).toMatchObject({ mode: 'dictionary', dictionary: true, label: 'Electro-industrial dictionary (core scope)' });
    expect(nt.words).toMatch(/^nodes come from the electro-industrial HS6 dictionary \(RMI Electro-Industrial Stack\): 6 indices > 8 sectors > stage/);
    expect(nt.words).toMatch(/core scope: the dictionary's core codes: those dedicated or predominant in their sector \(specificity 1-2\), plus a few shared codes it keeps as core with a written justification, the headline; primary rows only, so each code sits in one node$/);
    // a scope the words do not know is still named once
    expect(nodeTaxonomy({ ...doc.meta, dictionaryScope: 'wide' }).label).toBe('Electro-industrial dictionary (wide scope)');
    expect(nodeTaxonomy(master.meta)).toMatchObject({ mode: 'master', dictionary: false, label: 'OSI basket' });
    expect(nodeTaxonomy(older.meta)).toMatchObject({ mode: 'master' });
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
  it('the module nav (IA refactor): Overview, Partnerships, Sectoral Compacts (every compact page), Country roles, Diversification', () => {
    expect(aibNavItems('/aib', 'AIB-C01')).toEqual([
      { key: 'overview', label: 'Overview', to: '/aib', active: true },
      { key: 'partnerships', label: 'Partnerships', to: '/aib/partnerships', active: false },
      { key: 'compacts', label: 'Sectoral Compacts', to: '/compacts', active: false },
      { key: 'roles', label: 'Country roles', to: '/aib/roles', active: false },
      { key: 'diversification', label: 'Managed Interdependence', to: '/aib/diversification', active: false },
    ]);
    expect(aibNavItems('/aib/diversification').filter((i) => i.active).map((i) => i.key)).toEqual(['diversification']);
    expect(aibNavKeyOf('/aib/Diversification')).toBe('diversification');
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
    for (const p of ['/compacts', '/compacts/about', '/compacts/AIB-C01', '/aib', '/aib/roles', '/aib/diversification']) expect(placeOf(p)).toBe('aib');
    expect(placeOf('/compact')).toBeNull();
    expect(AIB_NAV.map((n) => n.label)).toEqual(['Overview', 'Partnerships', 'Sectoral Compacts', 'Country roles', 'Managed Interdependence']);
    expect(AIB_NAV.find((n) => n.key === 'diversification')!.to).toBe('/aib/diversification');
    expect(shellAibNavKeyOf('/aib/diversification')).toBe('diversification');
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
    // the engine's OSI cells; the third shares its technology name with the dictionary's Solar sector
    const cells = [
      { tech: 'Solar', supply_chain: 'Midstream' },
      { tech: 'Wind', supply_chain: 'Upstream' },
      { tech: 'Solar', supply_chain: 'Downstream' },
    ];
    const dp = [
      { partner_iso: 'AAA', cell_id: 1, value: 0.2 },
      { partner_iso: 'AAA', cell_id: 2, value: 0.7 },
      { partner_iso: 'BBB', cell_id: 1, value: 0.5 },
      { partner_iso: 'CCC', cell_id: 1, value: null },
      { partner_iso: 'DDD', cell_id: 3, value: 0.9 },
    ];
    expect(dpRows(dp, cells, doc.nodes, NO_FILTER, { osiTech: false }).map((r) => [r.iso3, r.value, r.tech, r.stage])).toEqual([
      ['DDD', 0.9, 'Solar', 'Downstream'],
      ['AAA', 0.7, 'Wind', 'Upstream'],
      ['BBB', 0.5, 'Solar', 'Midstream'],
    ]);
    // the dictionary's Solar sector (a Downstream node) reaches OSI Solar Midstream through its codes, never Solar Downstream by name
    for (const f of [{ ...NO_FILTER, tech: 'Solar' }, { ...NO_FILTER, tech: 'Solar', stage: 'Downstream' }, { ...NO_FILTER, node: 'solar-down-modules' }]) {
      expect(dpRows(dp, cells, doc.nodes, f, { osiTech: false }).map((r) => [r.iso3, r.value, r.stage])).toEqual([
        ['BBB', 0.5, 'Midstream'],
        ['AAA', 0.2, 'Midstream'],
      ]);
    }
    // a Midstream rare-earth node whose codes sit in the OSI Upstream cells
    expect(dpRows(dp, cells, doc.nodes, { ...NO_FILTER, node: 'ev-wind-mid-ree-compounds' }, { osiTech: false }).map((r) => [r.iso3, r.tech, r.stage])).toEqual([['AAA', 'Wind', 'Upstream']]);
    // no basket code: no cell, no rows (the view says so); a sector name the nodes do not use: none either
    expect(dpRows(dp, cells, doc.nodes, { ...NO_FILTER, tech: 'Electrolysers/H2 equipment' }, { osiTech: false })).toEqual([]);
    expect(dpRows(dp, cells, [], { ...NO_FILTER, tech: 'Solar' }, { osiTech: false })).toEqual([]);
    // the economy filter still applies
    expect(dpRows(dp, cells, doc.nodes, { ...NO_FILTER, tech: 'Solar', country: 'AAA' }, { osiTech: false }).map((r) => r.iso3)).toEqual(['AAA']);
    // a master bundle (OSI technologies): the filter's technology and stage limit the cells, so the
    // rare-earth node in Electric Vehicles and Wind never lends Wind + Upstream its Electric Vehicles cell
    const mcells = [
      { tech: 'Electric Vehicles', supply_chain: 'Upstream' },
      { tech: 'Wind', supply_chain: 'Upstream' },
      { tech: 'Electric Vehicles', supply_chain: 'Midstream' },
      { tech: 'Wind', supply_chain: 'Midstream' },
    ];
    const mdp = [
      { partner_iso: 'AAA', cell_id: 1, value: 0.9 },
      { partner_iso: 'AAA', cell_id: 2, value: 0.3 },
      { partner_iso: 'BBB', cell_id: 1, value: 0.8 },
      { partner_iso: 'BBB', cell_id: 4, value: 0.6 },
      { partner_iso: 'CCC', cell_id: 3, value: 0.7 },
    ];
    const M = { osiTech: true };
    expect(dpRows(mdp, mcells, master.nodes, { ...NO_FILTER, tech: 'Wind', stage: 'Upstream' }, M).map((r) => [r.iso3, r.value, r.tech, r.stage])).toEqual([['AAA', 0.3, 'Wind', 'Upstream']]);
    expect(dpRows(mdp, mcells, master.nodes, { ...NO_FILTER, tech: 'Wind' }, M).map((r) => [r.iso3, r.tech, r.stage])).toEqual([
      ['BBB', 'Wind', 'Midstream'],
      ['AAA', 'Wind', 'Upstream'],
    ]);
    expect(dpRows(mdp, mcells, master.nodes, { ...NO_FILTER, stage: 'Midstream' }, M).map((r) => [r.iso3, r.tech])).toEqual([
      ['CCC', 'Electric Vehicles'],
      ['BBB', 'Wind'],
    ]);
    // a node alone: every cell it sits in, both technologies (as before the crosswalk)
    expect(dpRows(mdp, mcells, master.nodes, { ...NO_FILTER, node: 'ev-wind-mid-ree-compounds' }, M).map((r) => [r.iso3, r.tech])).toEqual([
      ['AAA', 'Electric Vehicles'],
      ['BBB', 'Electric Vehicles'],
    ]);
    // the same filter read without the master rule would take the other technology's cell
    expect(dpRows(mdp, mcells, master.nodes, { ...NO_FILTER, tech: 'Wind', stage: 'Upstream' }, { osiTech: false }).map((r) => r.tech)).toContain('Electric Vehicles');
  });

  it('the filter as OSI cells (osiCellPick): the dominant cell of the matching nodes, every cell they sit in, none for a node without basket codes', () => {
    expect(osiCellPick(doc.nodes, NO_FILTER, { osiTech: false })).toBeNull();
    // a sector named like an OSI technology at another stage: its crosswalk cell, not the name
    const solar = osiCellPick(doc.nodes, { ...NO_FILTER, tech: 'Solar' }, { osiTech: false })!;
    expect(solar.cell).toEqual({ slug: 'solar-midstream', tech: 'Solar', stage: 'Midstream' });
    expect(solar.nodes.map((n) => n.id)).toEqual(['solar-down-modules']);
    // a node in two cells: the first (the largest share of its trade) is the dominant one
    const magnets = osiCellPick(doc.nodes, { ...NO_FILTER, tech: 'Magnets' }, { osiTech: false })!;
    expect(magnets.cell?.slug).toBe('electric-vehicles-midstream');
    expect(magnets.cells.map((c) => c.slug)).toEqual(['electric-vehicles-midstream', 'wind-midstream']);
    expect(magnets.from?.id).toBe('ev-wind-mid-magnets');
    // several matching nodes: the one with the largest lost supply decides; the others' cells follow
    const graphite = osiCellPick(doc.nodes, { ...NO_FILTER, tech: 'Graphite' }, { osiTech: false })!;
    expect([graphite.from?.id, graphite.cell?.slug]).toEqual(['bat-mid-anode', 'batteries-midstream']);
    expect(graphite.cells.map((c) => c.slug)).toEqual(['batteries-midstream', 'batteries-upstream']);
    // no basket code: no cell, the node listed as unmapped
    const h2 = osiCellPick(doc.nodes, { ...NO_FILTER, tech: 'Electrolysers/H2 equipment' }, { osiTech: false })!;
    expect([h2.cell, h2.cells, h2.unmapped.map((n) => n.id)]).toEqual([null, [], ['h2-down-electrolysers']]);
    // a stage alone spans several cells: no dominant cell, every cell listed
    const down = osiCellPick(doc.nodes, { ...NO_FILTER, stage: 'Downstream' }, { osiTech: false })!;
    expect(down.cell).toBeNull();
    expect(down.cells.map((c) => c.slug)).toEqual(['solar-midstream', 'wind-midstream']);
    expect(down.unmapped.map((n) => n.id)).toEqual(['h2-down-electrolysers']);
    expect([h2.unscored, h2.unscoredNodes]).toEqual([[], []]);
    // the master form: the filter names OSI technologies, so its technology and stage limit the cells; the
    // magnets node (Electric Vehicles and Wind, the largest lost supply) never lends Wind its Electric Vehicles cell
    const M = { osiTech: true };
    const windMid = osiCellPick(master.nodes, { ...NO_FILTER, tech: 'Wind', stage: 'Midstream' }, M)!;
    expect([windMid.from?.id, windMid.cell?.slug, windMid.cells.map((c) => c.slug)]).toEqual(['ev-wind-mid-magnets', 'wind-midstream', ['wind-midstream']]);
    const windUp = osiCellPick(master.nodes, { ...NO_FILTER, tech: 'Wind', stage: 'Upstream' }, M)!;
    expect([windUp.from?.id, windUp.cell?.slug, windUp.cells.map((c) => c.slug)]).toEqual(['ev-wind-mid-ree-compounds', 'wind-upstream', ['wind-upstream']]);
    // a technology alone: its own cells only, the dominant from the largest lost supply
    const wind = osiCellPick(master.nodes, { ...NO_FILTER, tech: 'Wind' }, M)!;
    expect([wind.cell?.slug, wind.cells.map((c) => c.slug)]).toEqual(['wind-midstream', ['wind-midstream', 'wind-upstream']]);
    // a stage alone: every technology's cell at that stage; a node alone: both of its cells
    expect(osiCellPick(master.nodes, { ...NO_FILTER, stage: 'Upstream' }, M)!.cells.map((c) => c.slug)).toEqual(['electric-vehicles-upstream', 'wind-upstream', 'batteries-upstream']);
    expect(osiCellPick(master.nodes, { ...NO_FILTER, node: 'ev-wind-mid-magnets' }, M)!.cells.map((c) => c.slug)).toEqual(['electric-vehicles-midstream', 'wind-midstream']);
    // without the master rule the same filter would read the magnets node's first cell
    expect(osiCellPick(master.nodes, { ...NO_FILTER, tech: 'Wind', stage: 'Midstream' }, { osiTech: false })!.cell?.slug).toBe('electric-vehicles-midstream');
  });

  it('only the cells the build keeps partner indices for count: a node in an unscored cell (Semiconductors) falls to its next cell, else to a notice', () => {
    // the engine's cells (no Semiconductors, as in the Strategic bundle)
    const scored = scoredCellsOf([
      { tech: 'Solar', supply_chain: 'Midstream' },
      { tech: 'Batteries', supply_chain: 'Midstream' },
    ]);
    expect([...scored]).toEqual(['solar-midstream', 'batteries-midstream']);
    const node = (id: string, label: string, techs: string[], yfCells: string[], lostUsd: number): AibNode => ({ ...doc.nodes[0], id, label, techs, stage: 'Midstream', yfCells, lostUsd });
    // the crosswalk orders ties alphabetically: the first cell of wafers is Semiconductors Midstream
    const wafers = node('wafers', 'Wafers', ['Semiconductor materials'], ['semiconductors-midstream', 'solar-midstream'], 3e9);
    const chips = node('chips', 'Chips', ['Semiconductor materials'], ['semiconductors-midstream'], 9e9);
    const packs = node('cells', 'Li-ion cells & packs', ['Cells/packs'], ['batteries-midstream'], 1e9);
    const nodes = [wafers, chips, packs];
    const D = { osiTech: false, scored };
    const semi = osiCellPick(nodes, { ...NO_FILTER, tech: 'Semiconductor materials' }, D)!;
    // chips has the larger lost supply but no scored cell: wafers decides, through Solar Midstream
    expect([semi.from?.id, semi.cell?.slug, semi.cells.map((c) => c.slug)]).toEqual(['wafers', 'solar-midstream', ['solar-midstream']]);
    expect([semi.unscored.map((c) => c.slug), semi.unscoredNodes.map((n) => n.id), semi.unmapped]).toEqual([['semiconductors-midstream'], ['chips'], []]);
    // without the scored set the unscored cell would win
    expect(osiCellPick(nodes, { ...NO_FILTER, node: 'wafers' }, { osiTech: false })!.cell?.slug).toBe('semiconductors-midstream');
    expect(osiCellPick(nodes, { ...NO_FILTER, node: 'wafers' }, D)!.cell?.slug).toBe('solar-midstream');
    // a node only in unscored cells: no cell, listed apart from the nodes without a basket code
    const onlyChips = osiCellPick(nodes, { ...NO_FILTER, node: 'chips' }, D)!;
    expect([onlyChips.cell, onlyChips.cells, onlyChips.unscored.map((c) => c.slug), onlyChips.unscoredNodes.map((n) => n.id), onlyChips.unmapped]).toEqual([
      null, [], ['semiconductors-midstream'], ['chips'], [],
    ]);
    // dpRows reads the engine's cells as the scored set: the Development Potential of wafers is Solar Midstream's
    const dp = [
      { partner_iso: 'AAA', cell_id: 1, value: 0.4 },
      { partner_iso: 'BBB', cell_id: 2, value: 0.9 },
    ];
    const engine = [
      { tech: 'Solar', supply_chain: 'Midstream' },
      { tech: 'Batteries', supply_chain: 'Midstream' },
    ];
    expect(dpRows(dp, engine, nodes, { ...NO_FILTER, tech: 'Semiconductor materials' }, { osiTech: false }).map((r) => [r.iso3, r.tech, r.stage])).toEqual([['AAA', 'Solar', 'Midstream']]);
    expect(dpRows(dp, engine, nodes, { ...NO_FILTER, node: 'chips' }, { osiTech: false })).toEqual([]);
  });
});

describe('compactTitle (display only): from the structured fields, never by parsing the name', () => {
  it('sectors joined with " / ", node labels whole (commas kept), the first and N more when long', () => {
    const c01t = compactTitle(c01, doc.nodes);
    expect(c01t).toBe('Magnets / Rare earths: Magnets, magnet parts & assemblies; Rare-earth oxides & compounds');
    // a lookup by id works the same
    expect(compactTitle(c01, nodeOf)).toBe(c01t);
    expect(compactTitle(c02, doc.nodes)).toBe('Graphite: Synthetic graphite (anode-grade, spherical); Natural graphite');
    expect(compactTitle(c03, doc.nodes)).toBe('Onshore wind: Wind towers, nacelles & hubs');
    // a name written with commas inside labels and "and N more": never split, the labels come from the nodes
    const labels = ['Electric buses, trucks & tractors', 'Electric & plug-in cars', 'Gallium, germanium & indium', 'Heat pumps & AC units', 'Wafers'];
    const nodes = labels.map((label, i) => ({ id: `n${i}`, label, techs: ['EVs/drivetrains'] }));
    const big = { name: 'EVs/drivetrains / Semiconductor materials: Electric buses, trucks & tractors, Electric & plug-in cars, Gallium, germanium & indium, and 2 more', nodes: nodes.map((n) => n.id), technologies: ['EVs/drivetrains', 'Semiconductor materials'] };
    expect(compactTitle(big, nodes)).toBe('EVs/drivetrains / Semiconductor materials: Electric buses, trucks & tractors (and 4 more)');
    // a first label without a comma keeps the plain form
    expect(compactTitle({ ...big, nodes: ['n1', 'n0', 'n2', 'n3'] }, nodes)).toBe('EVs/drivetrains / Semiconductor materials: Electric & plug-in cars and 3 more');
    expect(compactTitle(big, nodes, { full: true })).toBe(`EVs/drivetrains / Semiconductor materials: ${labels.join('; ')}`);
    // no technologies listed: the nodes' own, alphabetical; no node found: the exported name
    expect(compactTitle({ name: 'x', nodes: ['n4', 'n0'] }, nodes)).toBe('EVs/drivetrains: Wafers; Electric buses, trucks & tractors');
    expect(compactTitle({ name: 'Solar: Silicon Metal, Solar Cells', nodes: ['a', 'b'] }, [])).toBe('Solar: Silicon Metal, Solar Cells');
    // a master compact: the same rule
    expect(compactTitle(master.compacts[0], master.nodes)).toBe('Electric Vehicles / Wind: Magnets, magnet parts & assemblies; Rare-earth oxides & compounds');
    expect(joinLabels(['A', 'B'])).toBe('A, B');
    expect(joinLabels(['A, B', 'C'])).toBe('A, B; C');
  });
});

describe('taxonomy (2026-10-19): sectors grouped by index, the words, the OSI crosswalk', () => {
  it('a dictionary bundle says sector(s) and knows the indices; a master one says technology', () => {
    expect([tx.dictionary, tx.scope, tx.one, tx.Many, tx.all, tx.csv]).toEqual([true, 'core', 'sector', 'Sectors', 'All sectors', 'sectors']);
    const mt = taxonomyOf(master.meta);
    expect([mt.dictionary, mt.scope, mt.one, mt.Many, mt.all, mt.csv, mt.indices]).toEqual([false, null, 'technology', 'Technologies', 'All technologies', 'technologies', []]);
    expect(indexOfSector(tx, 'Magnets')).toMatchObject({ code: 'B', name: 'Electrotechnologies' });
    expect(indexOfSector(tx, 'Wind')).toBeNull();
    expect(indexLabel(indexOfNode(tx, nodeOf('h2-down-electrolysers')!)!)).toBe('F Industrial Transition');
    // the index from the sector when the node does not carry it
    expect(indexOfNode(tx, { indexCode: null, techs: ['Transformers'] })?.code).toBe('C');
    expect(indexOfNode(mt, master.nodes[0])).toBeNull();
    // a compact spanning two indices, in code order; from its sectors when the export has no compact.indices
    expect(indicesOfCompact(tx, c01).map(indexLabel)).toEqual(['A Materials & Inputs', 'B Electrotechnologies']);
    expect(indicesOfCompact(tx, { ...c01, indices: undefined }).map((ix) => ix.code)).toEqual(['A', 'B']);
    expect(indicesOfCompact(mt, master.compacts[0])).toEqual([]);
  });

  it('sector choices grouped by index (A..F, alphabetical within, unknown last); a master bundle is one plain list', () => {
    const groups = sectorGroups(tx, technologiesOf(doc.nodes));
    expect(groups.map((g) => [g.index?.code ?? null, g.sectors])).toEqual([
      ['A', ['Graphite', 'Rare earths']],
      ['B', ['Magnets']],
      ['C', ['Transformers']],
      ['D', ['Onshore wind', 'Solar']],
      ['F', ['Electrolysers/H2 equipment']],
    ]);
    expect(sectorGroups(tx, ['Solar', 'Nonesuch']).map((g) => [g.index?.code ?? null, g.sectors])).toEqual([
      ['D', ['Solar']],
      [null, ['Nonesuch']],
    ]);
    const opts = techOptions(tx, technologiesOf(doc.nodes));
    expect(opts[0]).toEqual({ value: 'Graphite', label: 'Graphite', group: 'A Materials & Inputs' });
    expect(opts.find((o) => o.value === 'Electrolysers/H2 equipment')!.group).toBe('F Industrial Transition');
    expect(techOptions(taxonomyOf(master.meta), technologiesOf(master.nodes)).every((o) => o.group === undefined)).toBe(true);
    expect(groupedSectorWords(tx, c01.technologies)).toBe('A Materials & Inputs: Rare earths; B Electrotechnologies: Magnets');
  });

  it('where a node sits: index > sector > stage (dictionary), technologies and stage (master); the OSI cell of a crosswalk id', () => {
    expect(nodeWhere(tx, nodeOf('solar-down-modules')!)).toBe('D Generation › Solar · Downstream');
    expect(nodeWhere(taxonomyOf(master.meta), master.nodes.find((n) => n.id === 'ev-wind-mid-magnets')!)).toBe('Electric Vehicles, Wind · Midstream');
    expect(osiCellOf('electric-vehicles-midstream')).toEqual({ slug: 'electric-vehicles-midstream', tech: 'Electric Vehicles', stage: 'Midstream' });
    expect(osiCellOf('green-hydrogen-downstream')).toMatchObject({ tech: 'Green Hydrogen', stage: 'Downstream' });
    expect(osiCellOf('magnets')).toBeNull();
  });
});

describe('emergent network roles and investment flows (2026-10-17)', () => {
  it('a network role is named at 1 point of node demand, minor below it, none without a selected relationship', () => {
    expect(sideLevel({ partners: 1, selected: 2, headline: 0.01, cumulative: 0.02, upstream: 0, midstream: 1, downstream: 0 })).toBe('named');
    expect(sideLevel({ partners: 1, selected: 2, headline: 0.004, cumulative: 0.01, upstream: 0, midstream: 1, downstream: 0 })).toBe('minor');
    expect(sideLevel({ partners: 0, selected: 0, headline: 0, cumulative: 0.02, upstream: 0, midstream: 0, downstream: 0 })).toBe('minor');
    expect(sideLevel({ partners: 0, selected: 0, headline: 0, cumulative: 0, upstream: 0, midstream: 0, downstream: 0 })).toBe('none');
    expect(sideLevel(null)).toBe('none');
    expect([fmtPts(0.2461), fmtPts(0.05), fmtPts(0.0004), fmtPts(0), fmtPts(null)]).toEqual(['25 pts', '5.0 pts', '<0.1 pts', '0 pts', '–']);
  });

  it('the matrix reads the summary beside the compacts: Avalon buys, invests and leads graphite; Carpania supplies and specializes; Dorvalia hosts; Zembla (shocked) is a minor supplier', () => {
    const rows = countryRoleMatrix(doc, names);
    const by = new Map(rows.map((r) => [r.iso3, r]));
    expect(namedRoles(by.get('AAA')!)).toEqual(['specialized', 'investor', 'buyer']);
    expect(by.get('AAA')!.cells.supplier.level).toBe('minor');
    expect(namedRoles(by.get('CCC')!)).toEqual(['supplier', 'specialized', 'bridge']);
    expect(by.get('CCC')!.cells.specialized.nodes).toEqual(['ev-wind-mid-ree-compounds', 'bat-mid-anode']);
    expect(by.get('DDD')!.cells.host.level).toBe('named');
    expect(by.get('EEE')!.cells.host.level).toBe('minor');
    expect(by.get('ZZZ')!.cells.supplier.level).toBe('minor');
    // strategic scope all: Borduria is the largest supplier of the magnets and of the electrolysers (outside every compact)
    expect(by.get('BBB')!.cells.specialized.nodes).toEqual(['ev-wind-mid-magnets', 'h2-down-electrolysers']);
    // a configured scope that leaves the electrolysers (Green Hydrogen) outside: the magnets only
    expect(countryRoleMatrix(master, names).find((r) => r.iso3 === 'BBB')!.cells.specialized.nodes).toEqual(['ev-wind-mid-magnets']);
    expect(roleHolders(rows, 'investor').map((r) => r.iso3)).toEqual(['AAA', 'BBB']);
    expect(countryRolesOf(doc, names, 'xyz').cells.buyer.level).toBe('none');
    // an older bundle: the compacts' roles alone (a demand anchor is still named)
    const old = countryRoleMatrix(stripMethodology(structuredClone(doc)), names);
    expect(old.find((r) => r.iso3 === 'AAA')!.cells.buyer.level).toBe('named');
    expect(old.find((r) => r.iso3 === 'AAA')!.cells.investor.level).toBe('none');
  });

  it('"every shock": the supply shocks, and the diversification scenario named apart where the export counts it in the network', () => {
    // the export lists every configured shock, the diversification scenario among them (it removes no supplier)
    expect(doc.network!.variant.shocks).toContain('diversify_benchmark');
    const e = everyShock(doc);
    expect(e).toEqual({
      supplyShocks: 3,
      diversify: true,
      phrase: 'every shock and the diversification scenario',
      note: '"every shock" sums the 3 supply shocks and the diversification scenario, which removes no supplier',
    });
    const cell = countryRolesOf(doc, names, 'AAA').cells.buyer;
    expect(cellWords('buyer', cell, e.phrase)).toContain(' across every shock and the diversification scenario');
    expect(cellWords('buyer', cell)).toContain(' across every shock;');
    // an export written before the scenario: the configured shocks alone
    const old = everyShock(stripDiversify(structuredClone(doc)));
    expect(old).toEqual({ supplyShocks: 3, diversify: false, phrase: 'every shock', note: '"every shock" sums the 3 configured shocks' });
  });

  it("an economy's edges: headline contributions only, largest first, each direction its own relationship", () => {
    const e = countryEdges(doc, 'AAA');
    expect(e.map((x) => [x.partner, x.supplies])).toEqual([['CCC', false], ['EEE', false], ['DDD', false], ['EEE', true]]);
    expect(edgeWords(e[2])).toEqual({ trade: 'buys from', investment: 'invests in' });
    expect(edgeWords(countryEdges(doc, 'DDD')[0])).toEqual({ trade: 'supplies', investment: 'hosts new capacity for' });
    expect(countryEdges(doc, 'ZZZ')).toEqual([]);
  });

  it('investment flows: the best-ranked host by default, alternatives on request, never summed', () => {
    const best = investmentPairs(doc, { ...NO_FILTER });
    expect(best.map((p) => `${p.investor}>${p.host}`)).toEqual(['AAA>DDD', 'BBB>DDD', 'AAA>CCC']);
    expect(investmentPairs(doc, { ...NO_FILTER, rank: 'all' })).toHaveLength(5);
    expect(investmentPairs(doc, { ...NO_FILTER, tech: 'Graphite' })).toEqual([]);
    // the filter reads the bundle's own sector names ("/" kept)
    expect(investmentPairs(doc, { ...NO_FILTER, tech: 'Electrolysers/H2 equipment' }).map((p) => p.node)).toEqual(['h2-down-electrolysers']);
    expect(investmentPairs(doc, { ...NO_FILTER, country: 'DDD', side: 'investor' })).toEqual([]);
    expect(investmentPairs(doc, { ...NO_FILTER, nodes: ['ev-wind-mid-magnets'], rank: 'all' }).map((p) => p.rank)).toEqual([1, 1, 2, 2]);
    const all = investmentPairs(doc, { ...NO_FILTER, rank: 'all' });
    expect(investorHostFlows(all).map((x) => [x.producer, x.consumer, x.value])).toEqual([['AAA', 'DDD', 480000000], ['BBB', 'DDD', 310000000], ['AAA', 'CCC', 90000000]]);
    expect(partnerTotals(all, 'AAA', 'investor').map((t) => [t.iso3, t.usd])).toEqual([['DDD', 480000000], ['CCC', 90000000]]);
    expect(partnerTotals(all, 'DDD', 'host').map((t) => t.iso3)).toEqual(['AAA', 'BBB']);
    expect(hostsOfNode(doc, 'ev-wind-mid-magnets').map((h) => h.host)).toEqual(['DDD', 'EEE']);
    expect(hostRanksOf(doc, 'EEE').map((h) => h.rank)).toEqual([2]);
    expect([rankWords(1), rankWords(2, 5), rankWords(3), rankWords(4, 5)]).toEqual(['best host', '2nd of 5', '3rd', '4th of 5']);
    expect(investmentPairs(stripMethodology(structuredClone(doc)), NO_FILTER)).toEqual([]);
  });
});

describe('the diversification scenario (2026-10-19)', () => {
  const rows = diversifyRows(doc.nodes);
  const rowOf = (id: string) => rows.find((r) => r.node.id === id)!;
  const ids = (rs: readonly { node: AibNode }[]) => rs.map((r) => r.node.id);

  it('rows: every node the scenario assessed, ran only where it diversified; the supply to shift is excess x allied demand', () => {
    expect(rows).toHaveLength(7);
    expect(rows.some((r) => r.node.id === 'grid-mid-transformers')).toBe(false);
    expect(ids(rows.filter((r) => r.ran))).toEqual(['ev-wind-mid-magnets', 'bat-mid-anode', 'wind-down-towers']);
    expect(ids(rows.filter((r) => r.above))).toEqual(['ev-wind-mid-ree-compounds', 'ev-wind-mid-magnets', 'bat-mid-anode', 'wind-down-towers']);
    const mag = rowOf('ev-wind-mid-magnets');
    expect(mag.demandUsd).toBe(9.6e9);
    expect(mag.shiftUsd! / 1e9).toBeCloseTo(0.34 * 9.6, 9);
    expect(mag.gapUsd).toBe(2112000000);
    expect(mag.tradeUsd! / 1e9).toBeCloseTo(0.12 * 9.6, 9);
    expect(mag.hhiGap).toBeCloseTo(0.31, 9);
    // within the cap: nothing to shift, no split; no benchmark: no distance
    expect(rowOf('ev-wind-mid-ree-compounds').shiftUsd).toBe(0);
    expect(rowOf('ev-wind-mid-ree-compounds').split).toBeNull();
    expect(rowOf('h2-down-electrolysers').hhiGap).toBeNull();
    // the class under the scenario carries its own physical-capability flag (null without a class)
    expect([rowOf('wind-down-towers').clsWarning, mag.clsWarning, rowOf('solar-down-modules').clsWarning]).toEqual([true, false, null]);
    expect(diversifyRow(nodeOf('wind-down-towers')!, 'another_id')!.clsWarning).toBe(false);
    const { variantWarnings: _vw, ...noFlags } = nodeOf('wind-down-towers')!;
    void _vw;
    expect(diversifyRow(noFlags)!.clsWarning).toBeNull();
    // already as diversified, yet above the benchmark by less than the tolerance
    expect(ids(rows.filter((r) => r.nearBenchmark))).toEqual(['bat-up-graphite']);
    expect(rowOf('bat-up-graphite').above).toBe(false);
    expect(diversifyRow(nodeOf('grid-mid-transformers')!)).toBeNull();
    expect(diversifyRows(older.nodes)).toEqual([]);
    expect(hasDiversify(doc)).toBe(true);
    expect(hasDiversify(stripDiversify(structuredClone(doc)))).toBe(false);
    expect(diversifyLabel(doc.meta)).toBe("Diversified to its sector's concentration");
    expect(diversifyLabel(older.meta)).toBe(DIVERSIFY_LABEL);
  });

  it('the demand split is to scale: kept + restored by trade + new capacity = 1', () => {
    for (const r of rows.filter((x) => x.ran)) expect(r.split!.kept + r.split!.trade + r.split!.build).toBeCloseTo(1, 12);
    const mag = rowOf('ev-wind-mid-magnets').split!;
    expect([mag.kept, mag.trade, mag.build].map((x) => Number(x.toFixed(6)))).toEqual([0.66, 0.12, 0.22]);
    expect(rowOf('bat-mid-anode').split!.build).toBeCloseTo(0, 12);
    // an earlier run with coverage after trade below the coverage before it: the lower figure is kept
    const low = demandSplit({ static: 0.8, optimized: 0.75 })!;
    expect([low.kept, low.trade, low.build].map((x) => Number(x.toFixed(6)))).toEqual([0.75, 0, 0.25]);
    expect(demandSplit({ static: 1.0001, optimized: 1.0001 })).toEqual({ kept: 1, trade: 0, build: 0 });
    expect(demandSplit({ static: null, optimized: 0.5 })).toBeNull();
  });

  it('totals: nodes above their benchmark of those with one, the supply to shift, what trade restores, the gap, the most-capped suppliers', () => {
    const t = diversifyTotals(doc.nodes);
    expect([t.nodes, t.assessed, t.withBenchmark, t.above, t.diversified, t.withinCap, t.already, t.nearBenchmark, t.noBenchmark, t.closedByTrade]).toEqual([8, 7, 6, 4, 3, 1, 2, 1, 1, 1]);
    expect(t.demandUsd / 1e9).toBeCloseTo(9.6 + 3.1 + 2 + 4.2, 9);
    expect(t.shiftUsd / 1e9).toBeCloseTo(3.264 + 0.372 + 0.14, 9);
    expect(t.shiftShare!).toBeCloseTo(3.776 / 18.9, 9);
    expect(t.tradeUsd / 1e9).toBeCloseTo(1.152 + 0.372 + 0.06, 9);
    expect(t.gapUsd / 1e9).toBeCloseTo(2.112 + 0.08, 9);
    // the fixture is consistent (what trade restores plus the gap is the supply to shift); the words never rely on it
    expect((t.tradeUsd + t.gapUsd) / 1e9).toBeCloseTo(t.shiftUsd / 1e9, 9);
    expect(t.capped).toEqual([{ iso3: 'ZZZ', nodes: 2 }, { iso3: 'AAA', nodes: 1 }, { iso3: 'BBB', nodes: 1 }]);
    expect(cappedWords(t.capped, names)).toBe('Zembla (2 nodes), Avalon (1) and Borduria (1)');
    expect(totalsWords(t)).toBe(
      '4 of the 6 nodes with a benchmark are more concentrated than their benchmark by more than a small tolerance (in 1 of them no supplier to the allied market is above the cap, so nothing shifts). ' +
        '1 other node sits above its benchmark by less than that and is left as it is. ' +
        'Capping their suppliers in the allied market at the share that would bring each down to its benchmark shifts $3.8 bn of supply (20% of the allied demand in those nodes) away from the capped suppliers. ' +
        'Existing relationships and optimized trade restore $1.6 bn with the cap holding; $2.2 bn is left for new capacity, and in 1 node trade replaces everything.',
    );
    // the most-capped suppliers follow the tiles, not the lede
    expect(totalsWords(t)).not.toContain('Most often capped');
    expect(aboveWords({ above: 1, withBenchmark: 6 })).toBe('1 of the 6 nodes with a benchmark is more concentrated than its benchmark by more than a small tolerance');
    expect(totalsWords({ ...t, nearBenchmark: 2 })).toContain(' 2 other nodes sit above their benchmark by less than that and are left as they are. ');
    expect(totalsWords({ ...t, nearBenchmark: 0 })).not.toContain('other node');
    const none = diversifyTotals(older.nodes);
    expect([none.assessed, none.above, none.shiftUsd, none.shiftShare]).toEqual([0, 0, 0, null]);
    expect(totalsWords(none)).toBe('');
  });

  it('order: diversified by capacity gap in USD, then within the cap, already diversified, no benchmark', () => {
    expect(ids(orderDiversify(rows))).toEqual([
      'ev-wind-mid-magnets', 'wind-down-towers', 'bat-mid-anode', 'ev-wind-mid-ree-compounds', 'bat-up-graphite', 'solar-down-modules', 'h2-down-electrolysers',
    ]);
  });

  it('filters by index and sector; by default only the nodes diversified', () => {
    expect(ids(filterDiversify(rows, NO_DIVERSIFY_FILTER, tx))).toEqual(['ev-wind-mid-magnets', 'bat-mid-anode', 'wind-down-towers']);
    expect(filterDiversify(rows, { ...NO_DIVERSIFY_FILTER, all: true }, tx)).toHaveLength(7);
    expect(ids(filterDiversify(rows, { index: 'A', sector: null, all: false }, tx))).toEqual(['bat-mid-anode']);
    expect(ids(filterDiversify(rows, { index: 'A', sector: null, all: true }, tx))).toEqual(['ev-wind-mid-ree-compounds', 'bat-mid-anode', 'bat-up-graphite']);
    expect(ids(filterDiversify(rows, { index: null, sector: 'Graphite', all: true }, tx))).toEqual(['bat-mid-anode', 'bat-up-graphite']);
    expect(diversifyIndices(rows, tx).map((ix) => ix.code)).toEqual(['A', 'B', 'D', 'F']);
    expect(diversifySectorOptions(rows, tx, 'A')).toEqual([
      { value: 'Graphite', label: 'Graphite', group: 'A Materials & Inputs' },
      { value: 'Rare earths', label: 'Rare earths', group: 'A Materials & Inputs' },
    ]);
    expect(diversifySectorOptions(rows, tx, null).map((o) => o.value)).toEqual(['Graphite', 'Rare earths', 'Magnets', 'Onshore wind', 'Solar', 'Electrolysers/H2 equipment']);
  });

  it('the node block in words, per status', () => {
    expect(Object.keys(DIVERSIFY_STATUS_WORDS)).toEqual([...AIB_DIVERSIFY_STATUSES]);
    expect(statusWords('no benchmark').label).toBe('No benchmark');
    expect(statusWords('something new')).toEqual({ label: 'something new', meaning: 'something new' });
    expect(rowStatusLabel(rowOf('bat-up-graphite'))).toBe('Already as diversified as its benchmark (above it by less than a small tolerance)');
    expect(rowStatusLabel(rowOf('solar-down-modules'))).toBe('Already as diversified as its benchmark');
    expect(diversifyWords(rowOf('ev-wind-mid-magnets'), names)).toEqual([
      'More concentrated than its benchmark: concentration (HHI) 0.46 against 0.15 for the other HS6 lines of heading 8505.',
      'The supplier-share cap that would bring it down to the benchmark is 22%, applied to the allied market. Zembla and Avalon are above it and lose the excess: 34.0% of allied demand ($3.3 bn) has to shift.',
      'With the cap holding, existing relationships restore coverage to 71.0% and optimized trade to 78.0%; new capacity has to fill the remaining 22.0% ($2.1 bn).',
      'Class under the scenario: BUILD. Leading hosts for the new capacity: Carpania and Dorvalia. Largest new or expanded relationships: Borduria → Carpania; Dorvalia → Avalon.',
    ]);
    const anode = diversifyWords(rowOf('bat-mid-anode'), names);
    expect(anode[1]).toContain('Zembla is above it and loses the excess: 12.0% of allied demand ($372 m) has to shift');
    expect(anode[2]).toBe('With the cap holding, existing relationships restore coverage to 95.0% and optimized trade to 100.0%; trade under the cap replaces all of it, so no new capacity is needed.');
    expect(anode[3]).not.toContain('Leading hosts');
    // the class under the scenario with its physical-capability flag
    expect(diversifyWords(rowOf('wind-down-towers'), names)[3]).toMatch(/^Class under the scenario: COORDINATE \(PHYSICAL CAPABILITY NOT VALIDATED\)\. /);
    expect(diversifyWords(rowOf('ev-wind-mid-magnets'), names)[3]).not.toContain('PHYSICAL CAPABILITY');
    // no capped supplier named (an export that lists none): the supply to shift as its own sentence
    expect(diversifyWords({ ...rowOf('bat-mid-anode'), d: { ...rowOf('bat-mid-anode').d, capped: [], cap: null } }, names)[1]).toBe(
      'A supplier-share cap that would bring it down to the benchmark applies to the allied market. 12.0% of allied demand ($372 m) has to shift.',
    );
    expect(diversifyWords(rowOf('ev-wind-mid-ree-compounds'), names)).toEqual([
      'More concentrated than its benchmark worldwide: concentration (HHI) 0.39 against 0.31 for the other HS6 lines of heading 2846.',
      'But no supplier to the allied market is above the 48% cap that would bring it down to the benchmark, so nothing has to shift and the scenario does not run.',
    ]);
    expect(diversifyWords(rowOf('solar-down-modules'), names)).toEqual([
      'Already as diversified as its benchmark: concentration (HHI) 0.12 against 0.19 for the other HS6 lines of headings 8541 and 8501. The scenario leaves it as it is.',
    ]);
    expect(diversifyWords(rowOf('bat-up-graphite'), names)).toEqual([
      'Within a small tolerance of its benchmark: concentration (HHI) 0.26 against 0.26 for the other HS6 lines of heading 2504. It is above the benchmark by less than the margin the scenario acts on, so the scenario leaves it as it is.',
    ]);
    // no block sentence compares a node with "its sector" (the benchmark is mostly the other lines of its heading)
    for (const r of rows) for (const w of diversifyWords(r, names)) expect(w, r.node.id).not.toMatch(/than its sector|diversified as its sector/);
    expect(diversifyWords(rowOf('h2-down-electrolysers'), names)).toEqual([
      'No benchmark: its customs heading has no other line and its sector no other node to compare it with, so the scenario leaves it as it is. Its concentration (HHI) is 0.20.',
    ]);
    expect(diversifyClassText(null)).toBe('not run');
    expect(diversifyClassText('BUILD')).toBe('BUILD');
    expect(diversifyClassText('LOW INTERVENTION', true)).toBe('LOW INTERVENTION (PHYSICAL CAPABILITY NOT VALIDATED)');
    expect(diversifyClassText(null, true)).toBe('not run');
  });

  it('basis, relationships, the axis, the Sector Profile of a cell and the CSV row', () => {
    expect(basisWords('other HS6 lines of heading 8507')).toBe('the other HS6 lines of heading 8507');
    expect(basisWords('other HS6 lines of heading 2826; 7410')).toBe('the other HS6 lines of headings 2826 and 7410');
    expect(basisWords('other nodes of the sector')).toBe('the other nodes of its sector');
    expect(basisWords(null)).toBe('its benchmark');
    expect(benchmarkWords({ currentHhi: 0.3839, benchmarkHhi: 0.0858, basis: 'other HS6 lines of heading 8507' })).toBe('0.38 against 0.09 for the other HS6 lines of heading 8507');
    expect(relationshipsOf('DEU->FRA; DEU->ESP;  ;bad')).toEqual([{ from: 'DEU', to: 'FRA' }, { from: 'DEU', to: 'ESP' }]);
    expect(relationshipsOf(null)).toEqual([]);
    expect(relationshipWords('ZZZ->AAA; QQQ->BBB', names)).toBe('Zembla → Avalon; QQQ → Borduria');
    expect(hhiAxisMax(rows)).toBe(0.5);
    expect(hhiAxisMax([])).toBe(0.1);
    expect(hhiTicks(0.5)).toEqual([0, 0.1, 0.2, 0.3, 0.4, 0.5]);
    expect(hhiTicks(1)).toEqual([0, 0.2, 0.4, 0.6, 0.8, 1]);
    expect(osiCellHref(osiCellOf('electric-vehicles-midstream')!)).toBe('/sector/electric-vehicles/midstream');
    expect(osiCellHref(osiCellOf('heat-pumps-downstream')!)).toBeNull();
    expect(profiledCellsOfNode(nodeOf('ev-wind-mid-magnets')!).map((c) => c.href)).toEqual(['/sector/electric-vehicles/midstream', '/sector/wind/midstream']);
    expect(profiledCellsOfNode(nodeOf('h2-down-electrolysers')!)).toEqual([]);
    const csv = diversifyCsvRow(rowOf('ev-wind-mid-magnets'), names, tx);
    expect(csv).toMatchObject({
      node_id: 'ev-wind-mid-magnets', index_code: 'B', index_name: 'Electrotechnologies', sectors: 'Magnets', status: 'diversified to the benchmark', supplier_cap: 0.22,
      capped_suppliers: 'Zembla; Avalon', capped_iso3: 'ZZZ; AAA', capacity_gap_usd: 2112000000, class_under_scenario: 'BUILD', class_under_scenario_warning: false, headline_class: 'BUILD',
      investment_hosts: 'Carpania; Dorvalia', largest_new_or_expanded_relationships: 'Borduria → Carpania; Dorvalia → Avalon', compact: 'AIB-C01', within_tolerance_of_benchmark: false,
    });
    expect(diversifyCsvRow(rowOf('wind-down-towers'), names, tx)).toMatchObject({ class_under_scenario: 'COORDINATE', class_under_scenario_warning: true });
    expect(diversifyCsvRow(rowOf('bat-up-graphite'), names, tx).within_tolerance_of_benchmark).toBe(true);
    const sol = diversifyCsvRow(rowOf('solar-down-modules'), names, tx);
    expect([sol.status, sol.supplier_cap, sol.capacity_gap_usd, sol.class_under_scenario, sol.class_under_scenario_warning, sol.capped_suppliers]).toEqual([
      'already as diversified as its benchmark', null, null, null, null, '',
    ]);
    expect(Object.keys(diversifyCsvRow(rowOf('solar-down-modules'), names, taxonomyOf(master.meta)))).toContain('technologies');
  });

  it('settings in words, and the class under the scenario among the variants: "not run" where it did not run, never a robustness difference', () => {
    const s = diversifySetting(doc.meta)!;
    expect([s.mode, s.shockId, s.benchmark, s.label]).toEqual(['hs4', 'diversify_benchmark', 'hs4', "Diversified to its sector's concentration"]);
    expect(s.words).toContain('HS4 customs heading');
    expect(diversifySetting(older.meta)).toBeNull();
    expect(diversifySetting({ ...doc.meta, diversify: { shockId: 'x', label: null, benchmark: 'sector' } })!.words).toBe(DIVERSIFY_BENCHMARK_WORDS.sector.words);
    const rules = diversifyRules(doc.meta);
    expect(rules).toHaveLength(6);
    expect(rules[0]).toBe(HHI_WORDS);
    expect(rules[2]).toContain('(the demand scope: 6 economies)');
    expect(rules[2]).toContain('its own purchases stay protected');
    expect(rules[2]).toContain('more concentrated than its benchmark, by more than a small tolerance, gets the supplier-share cap that would bring');
    // an export that states the tolerance gets it named
    expect(diversifyRules({ ...doc.meta, diversify: { ...doc.meta.diversify!, minExcess: 0.005 } })[2]).toContain('by more than a small tolerance (0.005 of HHI), gets');
    expect(rules[3]).toContain('with the cap holding for every supplier in both');
    expect(rules[4]).toBe('The scenario removes no supplier. It runs under the headline settings: hold semantics, production-constrained supply, the reallocation supply assumption, allied demand, and any economy may supply.');
    expect(rules[5]).toBe(`It is a counterfactual market structure, not a forecast. Dollar values are ${doc.meta.tradeYear} trade values.`);
    const mag = nodeOf('ev-wind-mid-magnets')!;
    expect(classVariants(mag, doc.meta).filter((v) => v.group === 'Diversification').map((v) => [v.key, v.cls, v.notRun])).toEqual([['diversify_benchmark', 'BUILD', false]]);
    expect(classVariants(nodeOf('solar-down-modules')!, doc.meta).find((v) => v.group === 'Diversification')!.notRun).toBe(true);
    // a different class under the scenario is not a variant that changes the class
    const other = { ...mag, variants: { ...mag.variants, diversify_benchmark: 'COORDINATE' } };
    expect(classVariants(other, doc.meta).find((v) => v.key === 'diversify_benchmark')!.differs).toBe(true);
    expect(robustnessSummary(other, doc.meta)).toBe(robustnessSummary(mag, doc.meta));
    // an older bundle has no such row
    expect(classVariants(older.nodes[0], older.meta).some((v) => v.group === 'Diversification')).toBe(false);
  });
});
