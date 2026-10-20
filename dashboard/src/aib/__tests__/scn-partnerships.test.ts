// The partnership lenses' AIB derivations (2026-10-19) on the synthetic fixture: the compacts and nodes
// under the toggled scenario (Decoupling the main document, Managed Interdependence its file: only the
// nodes it ran on, their class, gap and hosts), the coordinated trade summed over a scenario's compacts,
// today's trade from the Baseline file (pairs and the largest suppliers), the compact coalitions with an
// economy's seats and dependencies, and what each lens names for the cross-lens summary.

import { describe, expect, it } from 'vitest';
import { coalitionMembersOf } from '../contract.ts';
import {
  buildGaps, hostRows, investmentNodes, lensBuildGaps, lensCoalitions, lensHostRows, lensInvestmentNodes, lensNodes, lensSpecializedSuppliers, NO_FILTER, pairInvestmentFlows,
  pairInvestmentFlowsOf, pairTradeFlows, pairTradeFlowsOf, partnersInPairs, scenarioCompacts, sideTotals, specializedSuppliers, todayPairs, todaySuppliers,
} from '../derive/partnerships.ts';
import { nodeScenario } from '../derive/scenarios.ts';
import { haveFixture, readDoc, readScenarioDoc } from './fixture.ts';

describe.skipIf(!haveFixture())('partnership lenses under the scenarios (synthetic fixture)', () => {
  const doc = readDoc();
  const managed = readScenarioDoc('managed')!;
  const baseline = readScenarioDoc('baseline')!;

  it('scenarioCompacts: Decoupling is the main document; Managed Interdependence its file (null until it is loaded)', () => {
    const dec = scenarioCompacts(doc, 'decoupling')!;
    expect(dec.map((c) => c.id)).toEqual(doc.compacts.map((c) => c.id));
    dec.forEach((c, i) => {
      expect(c.trade).toBe(doc.compacts[i].trade);
      expect(c.members).toEqual(coalitionMembersOf(doc.compacts[i]));
      expect(c.criticalExternal).toEqual([...new Set([...(doc.compacts[i].coalition.criticalExternal ?? []), ...(doc.compacts[i].coalition.criticalExternalMarginal ?? [])])]);
    });
    expect(scenarioCompacts(doc, 'managed', null)).toBeNull();
    const mi = scenarioCompacts(doc, 'managed', managed)!;
    for (const c of mi) {
      const m = managed.compacts.find((x) => x.id === c.id)!;
      expect(c.nodes).toEqual(doc.compacts.find((x) => x.id === c.id)!.nodes);
      expect(c.trade).toEqual(m.trade);
      expect(c.members).toEqual(m.coalition ? m.coalition.members : null);
    }
    // a compact the file does not solve: no coalition, no relationship, a reason
    const one = { ...managed, compacts: managed.compacts.slice(1) };
    const miss = scenarioCompacts(doc, 'managed', one)!.find((c) => c.id === managed.compacts[0].id)!;
    expect(miss.members).toBeNull();
    expect(miss.trade).toEqual([]);
    expect(miss.reason).toMatch(/no entry for this compact/);
  });

  it('the coordinated trade of a scenario: pairTradeFlowsOf over its compacts (the main one unchanged for Decoupling)', () => {
    expect(pairTradeFlowsOf(doc.nodes, scenarioCompacts(doc, 'decoupling')!, NO_FILTER)).toEqual(pairTradeFlows(doc, NO_FILTER));
    expect(pairInvestmentFlowsOf(doc.nodes, scenarioCompacts(doc, 'decoupling')!, NO_FILTER)).toEqual(pairInvestmentFlows(doc, NO_FILTER));
    const mi = pairTradeFlowsOf(doc.nodes, scenarioCompacts(doc, 'managed', managed)!, NO_FILTER);
    const want = managed.compacts.flatMap((c) => c.trade).filter((e) => (e.usd ?? 0) > 0 && e.from !== e.to).reduce((s, e) => s + (e.usd ?? 0), 0);
    expect(mi.reduce((s, p) => s + p.value, 0)).toBeCloseTo(want, 0);
    // an economy's partners in either direction, largest first; one side summed
    const iso = mi[0].producer;
    expect(partnersInPairs(mi, iso)).toEqual([...new Set(mi.filter((p) => p.producer === iso || p.consumer === iso).map((p) => (p.producer === iso ? p.consumer : p.producer)))]);
    expect(sideTotals([{ producer: 'A', consumer: 'B', value: 1 }, { producer: 'C', consumer: 'B', value: 3 }, { producer: 'A', consumer: 'D', value: 3 }], 'producer')).toEqual(['A', 'C']);
  });

  it('lensNodes: Decoupling reads every node; Managed Interdependence its class, gap and hosts where it ran, the rest unchanged', () => {
    const dec = lensNodes(doc.nodes, 'decoupling');
    expect(dec).toHaveLength(doc.nodes.length);
    expect(lensInvestmentNodes(dec, NO_FILTER).map((r) => r.node.id)).toEqual(investmentNodes(doc.nodes, NO_FILTER).map((n) => n.id));
    expect(lensHostRows(dec, NO_FILTER)).toEqual(hostRows(doc.nodes, NO_FILTER));
    expect(lensBuildGaps(dec, NO_FILTER).map((r) => r.node.id)).toEqual(buildGaps(doc.nodes, NO_FILTER).map((n) => n.id));
    expect(lensSpecializedSuppliers(dec, NO_FILTER)).toEqual(specializedSuppliers(doc.nodes, NO_FILTER));
    const mi = lensNodes(doc.nodes, 'managed', managed.investmentFlows);
    const ran = doc.nodes.filter((n) => n.diversify?.status === 'diversified to the benchmark');
    expect(mi.map((r) => r.node.id)).toEqual(doc.nodes.map((n) => n.id));
    expect(mi.filter((r) => !r.unchanged).map((r) => r.node.id)).toEqual(ran.map((n) => n.id));
    // a node the scenario leaves unchanged: no class, gap or host; today's largest supplier (the supplier view counts it)
    for (const r of mi.filter((x) => x.unchanged)) {
      expect([r.cls, r.residual, r.hosts]).toEqual([null, 0, []]);
      expect(r.largest).toBe(nodeScenario(r.node, 'managed').largest);
    }
    expect(lensInvestmentNodes(mi, NO_FILTER).every((r) => !r.unchanged)).toBe(true);
    for (const r of mi.filter((x) => !x.unchanged)) {
      expect(r.cls).toBe(r.node.diversify!.cls);
      expect(r.residual).toBe(r.node.diversify!.residual);
      if (r.node.diversify!.hosts.length) expect(r.hosts).toEqual(r.node.diversify!.hosts);
    }
    // the best host's gain comes from the scenario's investment flows
    const withGain = mi.find((r) => managed.investmentFlows?.hosts.some((h) => h.node === r.node.id && h.rank === 1));
    if (withGain) expect(withGain.bestGain).toBe(managed.investmentFlows!.hosts.find((h) => h.node === withGain.node.id && h.rank === 1)!.gain);
    // the BUILD gaps under Managed Interdependence are its own
    expect(lensBuildGaps(mi, NO_FILTER).map((r) => r.node.id)).toEqual(ran.filter((n) => n.diversify!.cls === 'BUILD').map((n) => n.id));
  });

  it("today's trade: the Baseline file's pairs (an economy, a sector) and the largest suppliers summed over the nodes in scope", () => {
    const all = todayPairs(baseline.network, doc.nodes, NO_FILTER);
    expect(all.length).toBe(baseline.network!.edges.filter((e) => e.from !== e.to && e.tradeUsd > 0).length);
    expect(all[0].value).toBe(Math.max(...baseline.network!.edges.map((e) => e.tradeUsd)));
    const iso = all[0].consumer;
    const mine = todayPairs(baseline.network, doc.nodes, { ...NO_FILTER, country: iso });
    expect(mine.length).toBeGreaterThan(0);
    for (const p of mine) expect([p.producer, p.consumer]).toContain(iso);
    const node = doc.nodes.find((n) => baseline.network!.edges.some((e) => e.nodes.includes(n.id)))!;
    for (const p of todayPairs(baseline.network, doc.nodes, { ...NO_FILTER, node: node.id })) expect(p.nodes).toEqual([node.id]);
    expect(todayPairs(undefined, doc.nodes, NO_FILTER)).toEqual([]);
    // no filter: no in-scope supply; a sub-sector filter ranks the pairs by their supplier's own supply there
    // (each node's top suppliers), not by the pair's trade across every node
    expect(all.every((p) => p.scopeUsd === null)).toBe(true);
    const one = todayPairs(baseline.network, doc.nodes, { ...NO_FILTER, node: node.id }, baseline);
    const sup1 = new Map(todaySuppliers(doc.nodes, { ...NO_FILTER, node: node.id }, baseline).map((x) => [x.iso3, x.usd]));
    for (const p of one) expect(p.scopeUsd).toBe(sup1.get(p.producer) ?? 0);
    expect(one.map((p) => p.scopeUsd)).toEqual([...one.map((p) => p.scopeUsd)].sort((x, y) => (y ?? 0) - (x ?? 0)));
    // suppliers: the Baseline file's top ten per node; the top five of node.baseline while it loads
    const sup = todaySuppliers(doc.nodes, NO_FILTER, baseline);
    const want = new Map<string, number>();
    for (const r of baseline.nodes) for (const [x, usd] of r.suppliers) want.set(x, (want.get(x) ?? 0) + (usd ?? 0));
    expect(sup[0].iso3).toBe([...want.entries()].sort((a, b) => b[1] - a[1])[0][0]);
    expect(sup[0].usd).toBeCloseTo([...want.values()].sort((a, b) => b - a)[0], 0);
    const early = todaySuppliers(doc.nodes, NO_FILTER, null);
    expect(early.length).toBeGreaterThan(0);
    for (const s of early) expect(s.nodes.length).toBeLessThanOrEqual(doc.nodes.length);
  });

  it('lensCoalitions: the coalitions in scope; an economy\'s seats, its co-members and the compacts that depend on it from outside', () => {
    const dec = scenarioCompacts(doc, 'decoupling')!;
    const co = lensCoalitions(dec, doc.nodes, NO_FILTER);
    expect(co.compacts.map((c) => c.id)).toEqual(doc.compacts.map((c) => c.id));
    const counts = new Map<string, number>();
    for (const c of doc.compacts) for (const m of coalitionMembersOf(c)) counts.set(m, (counts.get(m) ?? 0) + 1);
    expect(co.members.map((m) => [m.iso3, m.n])).toEqual([...counts.entries()].sort((a, b) => b[1] - a[1] || a[0].localeCompare(b[0])));
    // a member: its seats and co-members (never itself)
    const iso = co.members[0].iso3;
    const mine = lensCoalitions(dec, doc.nodes, { ...NO_FILTER, country: iso });
    expect(mine.seats).toEqual(doc.compacts.filter((c) => coalitionMembersOf(c).includes(iso)).map((c) => c.id));
    expect(mine.members.map((m) => m.iso3)).not.toContain(iso);
    // a critical dependency outside a coalition
    const ext = doc.compacts.flatMap((c) => c.coalition.criticalExternal ?? [])[0];
    if (ext) {
      const dep = lensCoalitions(dec, doc.nodes, { ...NO_FILTER, country: ext });
      expect(dep.dependency.length).toBeGreaterThan(0);
      expect(dep.seats).toEqual([]);
    }
    // under Managed Interdependence: the file's coalitions
    const mi = lensCoalitions(scenarioCompacts(doc, 'managed', managed)!, doc.nodes, NO_FILTER);
    const miMembers = new Set(managed.compacts.flatMap((c) => c.coalition?.members ?? []));
    expect(new Set(mi.members.map((m) => m.iso3))).toEqual(miMembers);
  });
});
