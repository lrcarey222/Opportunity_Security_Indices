// The Country profile's partnership lenses and AIB role under the scenarios (2026-10-19; aib_scenarios_spec.md
// s.5 C and s.6), pure, on the synthetic fixtures: the coordinated trade of any scenario's compacts
// (aibTradeOf, the same as aibCountry's for Decoupling), the AIB document as Managed Interdependence reads it
// (aibDocUnder: the file's coalitions, roles and relationships on the same compact ids and nodes; a compact
// the file does not solve keeps none; nodes the scenario did not run on lose their hosts), the BUILD nodes
// that concern an economy under a scenario (buildNodesFor), and the fact sheet's Today lens (tradeTiesToday,
// agreementRows).

import { describe, expect, it } from 'vitest';
import type { AibDoc } from '../../aib/contract.ts';
import { lensNodes } from '../../aib/derive/partnerships.ts';
import { nodeScenario } from '../../aib/derive/scenarios.ts';
import { haveFixture as haveAib, readDoc, readScenarioDoc, SYNTHETIC_DIR } from '../../aib/__tests__/fixture.ts';
import { agreementRows, aibCountry, aibDocUnder, aibTradeOf, buildNodesFor, tradeTiesToday } from '../derive/framing.ts';
import { haveFixture, readSheet } from './fixture.ts';

describe.skipIf(!haveFixture() || !haveAib(SYNTHETIC_DIR))('Country partnerships under the scenarios (pure)', () => {
  const doc = readDoc(SYNTHETIC_DIR);
  const managed = readScenarioDoc('managed', SYNTHETIC_DIR)!;
  const sheet = readSheet('USA');

  it('aibTradeOf: the coordinated trade of any compacts; the main document gives aibCountry its pairs', () => {
    const iso = doc.compacts.flatMap((c) => c.trade)[0].from;
    const a = aibCountry(doc, doc.countries, iso);
    const t = aibTradeOf(doc.compacts, iso);
    expect(t.pairs).toEqual(a.pairs);
    expect(t.trade).toEqual(a.trade);
    // the scenario's compacts: its own edges, both directions, largest partner first
    const mi = managed.compacts.flatMap((c) => c.trade).filter((e) => e.from !== e.to && (e.usd ?? 0) > 0);
    const who = mi[0].to;
    const m = aibTradeOf(managed.compacts, who);
    const mine = mi.filter((e) => e.from === who || e.to === who);
    expect(m.trade.n).toBe(mine.length);
    expect(m.trade.usd).toBeCloseTo(mine.reduce((s, e) => s + (e.usd ?? 0), 0), 0);
    expect(m.pairs.map((p) => p.usd)).toEqual([...m.pairs.map((p) => p.usd)].sort((x, y) => y - x));
    expect(aibTradeOf([], who)).toEqual({ pairs: [], trade: { n: 0, usd: 0, nNew: 0 }, nodeUsd: new Map() });
  });

  it('aibDocUnder: Decoupling is the document; Managed Interdependence its file on the same compacts (null until loaded)', () => {
    expect(aibDocUnder(doc, 'decoupling')).toBe(doc);
    expect(aibDocUnder(doc, 'managed', null)).toBeNull();
    const d = aibDocUnder(doc, 'managed', managed)!;
    expect(d.compacts.map((c) => [c.id, c.nodes])).toEqual(doc.compacts.map((c) => [c.id, c.nodes]));
    for (const c of d.compacts) {
      const m = managed.compacts.find((x) => x.id === c.id)!;
      expect(c.trade).toBe(m.trade);
      expect(c.roles).toBe(m.roles);
      expect(c.coalition.members).toEqual(m.coalition?.members ?? []);
    }
    expect(d.network).toBe(managed.network);
    expect(d.investmentFlows).toBe(managed.investmentFlows);
    // nodes: the scenario's largest supplier where it ran; today's (as on /aib/roles) and no host elsewhere
    for (const n of d.nodes) {
      const s = nodeScenario(doc.nodes.find((x) => x.id === n.id)!, 'managed');
      expect(n.largest).toBe(s.largest);
      if (!s.ran) expect(n.hosts).toEqual([]);
    }
    // the type, archetype and policy needs of a compact the file solves are the scenario's own
    for (const c of d.compacts) {
      const m = managed.compacts.find((x) => x.id === c.id);
      if (m?.type) expect([c.type, c.archetype, c.policyNeeds]).toEqual([m.type, m.archetype, m.policyNeeds]);
    }
    // a compact the file does not solve: no coalition, no role, no relationship
    const first = doc.compacts[0].id;
    const thin = aibDocUnder(doc, 'managed', { ...managed, compacts: managed.compacts.filter((c) => c.id !== first) })!;
    const c0 = thin.compacts.find((c) => c.id === first)!;
    expect([c0.coalition.members, c0.roles, c0.trade, c0.investment]).toEqual([[], [], [], []]);
  });

  it('buildNodesFor: the BUILD nodes of its compacts or where it is a listed host, largest gap first, under either scenario', () => {
    for (const [id, flows] of [['decoupling', doc.investmentFlows], ['managed', managed.investmentFlows]] as const) {
      const rows = lensNodes(doc.nodes, id, flows);
      const build = rows.filter((r) => r.cls === 'BUILD');
      const host = build.flatMap((r) => r.hosts)[0];
      if (!host) continue;
      const got = buildNodesFor(rows, [], host);
      expect(got.length).toBeGreaterThan(0);
      for (const r of got) expect([r.cls, r.hosts.includes(host)]).toEqual(['BUILD', true]);
      expect(got.map((r) => r.residual ?? -1)).toEqual([...got.map((r) => r.residual ?? -1)].sort((a, b) => b - a));
      // a compact's id brings its BUILD nodes in whoever hosts them
      const cid = build.find((r) => r.node.compact)?.node.compact;
      if (cid) expect(buildNodesFor(rows, [cid], 'ZZZ').map((r) => r.node.id)).toEqual(build.filter((r) => r.node.compact === cid).sort((a, b) => (b.residual ?? -1) - (a.residual ?? -1)).map((r) => r.node.id));
    }
    // a node Managed Interdependence did not run on never counts under it
    const mi = lensNodes(doc.nodes, 'managed', managed.investmentFlows);
    const all = buildNodesFor(mi, doc.compacts.map((c) => c.id), 'ZZZ');
    for (const r of all) expect(r.node.diversify?.status).toBe('diversified to the benchmark');
  });

  it("the fact sheet's Today lens: trade ties in tie order (never itself) and the partners sharing an agreement", () => {
    const ties = tradeTiesToday(sheet);
    expect(ties).toEqual(sheet.partners.tie_order.filter((x) => x !== 'USA'));
    expect(ties).not.toContain('USA');
    const ag = agreementRows(sheet);
    for (const r of ag) expect(r.ties.agreements.length).toBeGreaterThan(0);
    const n = ag.map((r) => r.ties.agreements.length);
    expect(n).toEqual([...n].sort((a, b) => b - a));
    expect(ag.length).toBe(sheet.partners.rows.filter((r) => r.iso3 !== 'USA' && r.ties.agreements.length > 0).length);
  });

  it('types: aibDocUnder keeps the document shape the role section reads', () => {
    const d: AibDoc | null = aibDocUnder(doc, 'managed', managed);
    expect(d && Object.keys(d).sort()).toEqual(Object.keys(doc).sort());
  });
});
