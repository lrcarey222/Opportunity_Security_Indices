// Page work B (2026-10-19), the derivations: the country roles, the network and the investment flows under
// the toggle, on the synthetic fixture. Decoupling reads the headline only (an edge without a headline
// contribution is dropped; a role is minor only where the headline contributes); Managed Interdependence
// reads its scenario file; today's network (the Baseline file) is the reference: who supplies whom, the
// roles gained or lost, today's suppliers against the scenario's added ones; the every-shock totals named
// as Decoupling's supply shocks.

import { describe, expect, it } from 'vitest';
import { countryNamesOf } from '../contract.ts';
import { addedSuppliers, tradeFlows } from '../derive/flows.ts';
import { flowsDocOf, investmentPairs } from '../derive/investment.ts';
import {
  changeHolders, countryRoleMatrix, countryRolesOf, decoupledEconomy, edgeTotal, everyShockLabel, fmtPts, headlinePartnersOf, roleChanges, roleNamedWords, scenarioRoleSource, sideLevel,
  supplyPairs, underScenario,
} from '../derive/network.ts';
import { compactDetail, scenarioInvestmentFlows } from '../derive/scenarios.ts';
import { NO_FILTER } from '../derive/partnerships.ts';
import { cellWords } from '../views/networkParts.tsx';
import { readDoc, readScenarioDoc, stripScenarios, SYNTHETIC_DIR } from './fixture.ts';

const doc = readDoc(SYNTHETIC_DIR);
const mi = readScenarioDoc('managed', SYNTHETIC_DIR)!;
const bl = readScenarioDoc('baseline', SYNTHETIC_DIR)!;
const names = countryNamesOf(doc);

describe('the roles under the toggle', () => {
  it('a side read on the headline only: minor only where the headline contributes', () => {
    const side = { partners: 0, selected: 0, headline: 0, cumulative: 0.02, upstream: 0, midstream: 0, downstream: 0 };
    expect(sideLevel(side)).toBe('minor');
    expect(sideLevel(side, 0.01, true)).toBe('none');
    expect(sideLevel({ ...side, headline: 0.004 }, 0.01, true)).toBe('minor');
    expect(sideLevel({ ...side, headline: 0.01 }, 0.01, true)).toBe('named');
  });

  it('Decoupling: the headline network (the edge with no headline contribution dropped), the main compacts, the largest optimized supplier', () => {
    const src = scenarioRoleSource(doc, 'decoupling')!;
    expect(src.headlineOnly).toBe(true);
    expect(src.network!.edges.every((e) => edgeTotal(e) > 0)).toBe(true);
    expect(src.network!.edges.some((e) => e.from === 'ZZZ')).toBe(false);
    expect(doc.network!.edges.some((e) => e.from === 'ZZZ')).toBe(true);
    expect(src.largestOf.get('CCC')).toEqual(['ev-wind-mid-ree-compounds', 'bat-mid-anode']);
    // EEE hosts new capacity only under another shock: minor in the export's own reading, none under Decoupling
    expect(countryRolesOf(doc, names, 'EEE').cells.host.level).toBe('minor');
    expect(countryRolesOf(doc, names, 'EEE', src).cells.host.level).toBe('none');
    // the main document's call without a source is unchanged
    expect(countryRoleMatrix(doc, names).length).toBeGreaterThanOrEqual(countryRoleMatrix(doc, names, src).length);
  });

  it('Managed Interdependence: its file\'s network, compacts and largest suppliers; null without the file', () => {
    expect(scenarioRoleSource(doc, 'managed', null)).toBeNull();
    const src = scenarioRoleSource(doc, 'managed', mi)!;
    expect(src.network).toBe(mi.network);
    const rows = countryRoleMatrix(doc, { ...names, ...mi.countries }, src);
    const row = (iso: string) => rows.find((r) => r.iso3 === iso)!;
    // Freedonia appears in the scenario's network and compacts only
    expect(row('FFF').cells.supplier.level).toBe('named');
    expect(row('FFF').member).toEqual(['AIB-C03']);
    expect(row('CCC').cells.host.level).toBe('named');
    // the largest supplier where the scenario ran (node.diversify.largest), today's elsewhere
    expect(row('BBB').cells.specialized.nodes).toEqual(['ev-wind-mid-magnets']);
    expect(row('GGG').cells.specialized.nodes).toEqual(['grid-mid-transformers']);
  });

  it('the words name the scenario and leave out an every-shock total where there is none', () => {
    const cell = countryRolesOf(doc, names, 'CCC', scenarioRoleSource(doc, 'managed', mi)).cells.supplier;
    const under = underScenario(doc.meta, 'managed');
    expect(under).toBe('under Managed Interdependence');
    const w = cellWords('supplier', cell, null, under);
    expect(w).toContain(`${fmtPts(0.0769)} under Managed Interdependence`);
    expect(w).not.toContain('across');
    expect(cellWords('supplier', cell)).toContain('under the headline shock, ');
    expect(roleNamedWords('host', under)).toContain('1 point of node demand under Managed Interdependence');
    expect(fmtPts(18.16683)).toBe('1,817 pts');
  });

  it('Decoupling counts the partners of its headline relationships; the network\'s own counts sum every shock and say so', () => {
    expect(headlinePartnersOf(doc.network!).get('AAA')).toEqual({ supplier: 1, buyer: 3, host: 0, investor: 1 });
    const src = scenarioRoleSource(doc, 'decoupling')!;
    const cell = countryRolesOf(doc, names, 'AAA', src).cells.buyer;
    // ZZZ supplies AAA only under another shock: three suppliers under Decoupling, four across every shock
    expect(cell.headlinePartners).toBe(3);
    expect(cell.side!.partners).toBe(4);
    const l = everyShockLabel(doc);
    const w = cellWords('buyer', cell, l.phrase, 'under Decoupling');
    expect(w).toMatch(new RegExp(`^${fmtPts(0.3)} under Decoupling with 3 suppliers; across ${l.phrase}: ${fmtPts(0.9)}, 4 suppliers \\(upstream \\d+, midstream \\d+, downstream \\d+\\), 9 selected relationships`));
    // Managed Interdependence (its shock alone) and the export's own reading keep the side's counts
    expect(countryRolesOf(doc, names, 'CCC', scenarioRoleSource(doc, 'managed', mi)).cells.supplier.headlinePartners).toBeUndefined();
    expect(countryRolesOf(doc, names, 'AAA').cells.buyer.headlinePartners).toBeUndefined();
    expect(cellWords('buyer', countryRolesOf(doc, names, 'AAA').cells.buyer)).toContain('; 4 suppliers (upstream');
  });

  it('"every shock" is labelled as Decoupling\'s supply shocks, with their labels', () => {
    const l = everyShockLabel(doc);
    expect(l.phrase).toBe("Decoupling's three supply shocks and the diversification scenario");
    expect(l.header).toBe("Decoupling's 3 supply shocks and the diversification scenario");
    expect(l.note).toBe(
      "The every-shock totals sum Decoupling's three supply shocks (Zembla supply removed; Zembla supply halved; Largest supplier of the sub-sector removed), and the diversification scenario, which removes no supplier.",
    );
    const plain = everyShockLabel({ ...doc, network: { ...doc.network!, variant: { ...doc.network!.variant, shocks: ['zzz_100', 'zzz_50'] } } });
    expect(plain.phrase).toBe("Decoupling's two supply shocks");
  });

  it('the economy Decoupling removes, from its shock label', () => {
    expect(decoupledEconomy(doc.meta, names)).toBe('ZZZ');
    expect(decoupledEconomy({ ...doc.meta, shocks: doc.meta.shocks.map((s) => ({ ...s, label: 'Something else' })) }, names)).toBeNull();
  });
});

describe('today against the scenario', () => {
  it('who supplies whom: every pair of either network, today by observed USD, the scenario by contribution; one economy', () => {
    const src = scenarioRoleSource(doc, 'decoupling')!;
    const p = supplyPairs(bl.network!, src.network, null);
    expect(p.today.length).toBe(bl.network!.edges.filter((e) => e.trade > 0).length);
    expect(p.today[0].today!.tradeUsd).toBe(Math.max(...bl.network!.edges.map((e) => e.tradeUsd)));
    expect(p.scenario.map((x) => `${x.from}>${x.to}`)[0]).toBe('CCC>AAA');
    expect(p.scenario.every((x, i, a) => i === 0 || edgeTotal(a[i - 1].scenario!) >= edgeTotal(x.scenario!))).toBe(true);
    const dd = p.scenario.find((x) => x.from === 'DDD' && x.to === 'BBB')!;
    expect(dd.today).toBeNull();
    const one = supplyPairs(bl.network!, src.network, 'ddd');
    expect(one.all.every((x) => x.from === 'DDD' || x.to === 'DDD')).toBe(true);
  });

  it('Decoupling: the removed supplier loses its roles; new suppliers, hosts, investors and largest suppliers are gains', () => {
    const src = scenarioRoleSource(doc, 'decoupling')!;
    const cmp = roleChanges({ scenario: 'decoupling', today: bl.network!, network: src.network, nodes: doc.nodes, names, removed: decoupledEconomy(doc.meta, names) });
    expect(cmp.nodes).toBe(7);
    const r = (iso: string) => cmp.rows.find((x) => x.iso3 === iso)!;
    expect(r('ZZZ')).toMatchObject({ removed: true, lost: ['supplier', 'specialized'], gained: [] });
    expect(r('ZZZ').today.largestOf).toHaveLength(7);
    expect(r('DDD').gained).toEqual(['supplier', 'host']);
    expect(r('AAA').gained).toEqual(['specialized', 'investor']);
    expect(changeHolders(cmp.rows, 'specialized', 'gained').map((x) => x.iso3)).toEqual(['BBB', 'CCC', 'AAA', 'EEE', 'FFF']);
    expect(changeHolders(cmp.rows, 'investor', 'gained').map((x) => x.iso3)).toEqual(['AAA', 'BBB']);
    expect(cmp.rows.every((x) => x.capped.length === 0)).toBe(true);
  });

  it('Managed Interdependence: nobody is removed; the capped suppliers supply less', () => {
    const src = scenarioRoleSource(doc, 'managed', mi)!;
    const cmp = roleChanges({ scenario: 'managed', today: bl.network!, network: src.network, nodes: doc.nodes, names, removed: 'ZZZ' });
    expect(cmp.nodes).toBe(8);
    const r = (iso: string) => cmp.rows.find((x) => x.iso3 === iso)!;
    expect(r('ZZZ')).toMatchObject({ removed: false, lost: [], gained: [], capped: ['ev-wind-mid-magnets', 'bat-mid-anode'] });
    expect(r('BBB').gained).toEqual(['specialized', 'investor']);
    expect(r('BBB').capped).toEqual(['wind-down-towers']);
    expect(r('CCC').gained).toEqual(['host']);
    expect(r('DDD').gained).toEqual(['supplier']);
    expect(cmp.rows.some((x) => x.lost.length > 0)).toBe(false);
  });

  it('a compact: today\'s suppliers against the scenario\'s added trade by exporter', () => {
    const c = doc.compacts[0];
    const d = addedSuppliers(c.trade);
    expect(d.reduce((s, x) => s + x.usd, 0)).toBeCloseTo(c.trade.filter((e) => e.from !== e.to && (e.usd ?? 0) > 0).reduce((s, e) => s + (e.usd ?? 0), 0));
    expect(d.every((x, i, a) => i === 0 || a[i - 1].usd >= x.usd)).toBe(true);
    const node = c.trade[0].node;
    expect(addedSuppliers(c.trade, node).every((x) => x.nodes.every((n) => n === node))).toBe(true);
    // the Sankey reads any scenario's relationships (the Managed Interdependence compact)
    const det = compactDetail(c, 'managed', mi);
    expect(tradeFlows(det).reduce((s, f) => s + f.value, 0)).toBeCloseTo(det.trade.filter((e) => e.from !== e.to).reduce((s, e) => s + (e.usd ?? 0), 0));
  });

  it('the investment flows of a scenario read through flowsDocOf(); Baseline has none', () => {
    const flows = scenarioInvestmentFlows(doc, 'managed', mi)!;
    const pairs = investmentPairs(flowsDocOf(doc, flows), { ...NO_FILTER, rank: 'best' });
    expect(pairs.map((p) => `${p.investor}>${p.host}`)).toEqual(['AAA>CCC', 'BBB>CCC']);
    expect(investmentPairs(flowsDocOf(doc, null), { ...NO_FILTER })).toEqual([]);
    expect(scenarioInvestmentFlows(doc, 'baseline', bl)).toBeNull();
  });

  it('an older bundle without the scenarios: Decoupling still reads its headline network', () => {
    const old = stripScenarios(structuredClone(doc));
    const src = scenarioRoleSource(old, 'decoupling')!;
    expect(countryRoleMatrix(old, names, src).length).toBeGreaterThan(0);
  });
});
