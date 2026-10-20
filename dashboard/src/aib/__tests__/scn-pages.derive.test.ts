// Page work A of the scenario toggle (2026-10-19), the pure part: a compact as Managed Interdependence
// solves it (the scenario file over the main document's compact; none without a coalition), the nodes by
// their class under a scenario (the unchanged ones apart), the Overview's three-way rows (the same nodes in
// all three; the Managed Interdependence row equal to the explainer's totals, on the synthetic fixture and
// on the real export when present), the compacts ordered and tabled per scenario, the CSV columns that name
// a scenario, the words of an unchanged node, the cap as the first coverage step, the strategic map under a
// scenario, and the explainer's supply to shift (the exported USD where there is one).

import { describe, expect, it } from 'vitest';
import type { AibDoc, AibManagedScenarioDoc } from '../contract.ts';
import { mapPoints, mapPointsFor } from '../charts/StrategicMap.tsx';
import { coverageSteps } from '../derive/coverage.ts';
import { ALREADY_DIVERSIFIED, diversifyRow, diversifyTotals, diversifyWords, NO_BENCHMARK, WITHIN_CAP } from '../derive/diversify.ts';
import { nodesByClass } from '../derive/nodes.ts';
import {
  compactScenarioCsvRow, compactScenarioRows, compactsByScenario, compactUnchangedReason, compactUnchangedReasonOf, managedCompact, managedCompacts, managedOf, nodeScenarioCsv, nodeScenarioNote, overviewRows, restoredUsd,
  scenarioClassGroups, topScenarioNodes, UNCHANGED, unchangedWords,
} from '../derive/scenarioPages.ts';
import { classMixText, comparableNodes, nodeScenario, runMetaOf, scenarioCsvFields, scenarioTotals } from '../derive/scenarios.ts';
import { haveFixture, readDoc, readScenarioDoc, REAL_DIR, SYNTHETIC_DIR } from './fixture.ts';

const doc = (): AibDoc => structuredClone(readDoc(SYNTHETIC_DIR));
const managed = (): AibManagedScenarioDoc => structuredClone(readScenarioDoc('managed', SYNTHETIC_DIR)!);
const nodeOf = (d: AibDoc) => (id: string) => d.nodes.find((n) => n.id === id) ?? null;

describe('a compact under Managed Interdependence', () => {
  it('the scenario file over the main document: its coalition, roles, metrics, path and relationships; the main compact kept otherwise', () => {
    const d = doc();
    const md = managed();
    const c = d.compacts[0];
    const m = md.compacts.find((x) => x.id === c.id)!;
    const v = managedCompact(c, md)!;
    expect(v).not.toBeNull();
    expect(v.id).toBe(c.id);
    expect(v.nodes).toEqual(c.nodes);
    expect(v.type).toBe(c.type);
    expect(v.coalition).toEqual(m.coalition);
    expect(v.roles).toEqual(m.roles);
    expect(v.metrics).toEqual(m.metrics);
    expect(v.path).toEqual(m.path);
    expect(v.trade).toEqual(m.trade);
    expect(v.investment).toEqual(m.investment);
    expect(v.classMix).toBe(c.managed!.classMix);
    expect(v.secondaryClass).toBeNull();
    // the main document's compact is untouched
    expect(c.coalition).not.toBe(m.coalition);
    expect(managedCompacts(d.compacts, md).map((x) => x.id)).toEqual(d.compacts.map((x) => x.id));
  });

  it("the compact's type, archetype, policy needs and secondary class under the scenario, where the file has them", () => {
    const d = doc();
    const md = managed();
    const c = d.compacts[0];
    const i = md.compacts.findIndex((x) => x.id === c.id);
    md.compacts[i] = {
      ...md.compacts[i],
      type: 'compact unnecessary: existing relationships do nearly as well',
      archetype: 'Integrated Compact',
      policyNeeds: ['monitor'],
      secondaryClass: 'BUILD',
    };
    const v = managedCompact(c, md)!;
    expect([v.type, v.archetype, v.policyNeeds, v.secondaryClass]).toEqual(['compact unnecessary: existing relationships do nearly as well', 'Integrated Compact', ['monitor'], 'BUILD']);
    // the main document's compact keeps Decoupling's
    expect([c.type, c.archetype]).toEqual([readDoc(SYNTHETIC_DIR).compacts[0].type, readDoc(SYNTHETIC_DIR).compacts[0].archetype]);
  });

  it('none without the file, without an entry, or without a coalition', () => {
    const d = doc();
    const md = managed();
    const c = d.compacts[2];
    expect(managedCompact(c, null)).toBeNull();
    const noEntry = { ...md, compacts: md.compacts.filter((x) => x.id !== c.id) };
    expect(managedCompact(c, noEntry)).toBeNull();
    const m = md.compacts.find((x) => x.id === c.id)!;
    m.coalition = null;
    m.metrics = null;
    m.coalitionReason = 'no node of this compact is more concentrated than its benchmark';
    expect(managedCompact(c, md)).toBeNull();
    expect(managedCompacts(d.compacts, md).map((x) => x.id)).toEqual(d.compacts.filter((x) => x.id !== c.id).map((x) => x.id));
    expect(managedOf(md)).toBe(md);
    expect(managedOf(readScenarioDoc('baseline', SYNTHETIC_DIR))).toBeNull();
    expect(managedOf(null)).toBeNull();
  });

  it("why it leaves a compact unchanged, in the scenario file's words (the R mirror's), from the nodes' statuses", () => {
    expect(compactUnchangedReason([ALREADY_DIVERSIFIED, ALREADY_DIVERSIFIED])).toBe('no node of this compact is above its benchmark by more than a small tolerance');
    expect(compactUnchangedReason([NO_BENCHMARK, null])).toBe('no node of this compact has a concentration benchmark');
    expect(compactUnchangedReason([ALREADY_DIVERSIFIED, WITHIN_CAP, NO_BENCHMARK])).toBe('no node of this compact has a supplier above its benchmark cap in the allied market');
    expect(compactUnchangedReason([null, undefined])).toBe('the scenario did not assess any node of this compact');
    expect(compactUnchangedReason(['something else'])).toBe('the scenario did not run on any node of this compact');
    const d = doc();
    const ree = d.nodes.find((n) => n.id === 'ev-wind-mid-ree-compounds')!;
    expect(compactUnchangedReasonOf([ree])).toBe('no node of this compact has a supplier above its benchmark cap in the allied market');
  });

  it.skipIf(!haveFixture(REAL_DIR))("on the real export, the words match the scenario file's reason for every compact without a coalition", () => {
    const d = readDoc(REAL_DIR);
    const md = readScenarioDoc('managed', REAL_DIR);
    if (!md || md.scenario !== 'managed') return;
    const byId = new Map(d.nodes.map((n) => [n.id, n]));
    const none = md.compacts.filter((x) => !x.coalition && x.coalitionReason);
    for (const m of none) {
      const c = d.compacts.find((x) => x.id === m.id)!;
      expect(compactUnchangedReasonOf(c.nodes.map((id) => byId.get(id)).filter((n): n is NonNullable<typeof n> => !!n)), m.id).toBe(m.coalitionReason);
    }
  });
});

describe('nodes under a scenario', () => {
  it('Decoupling groups the nodes by their headline class, as before', () => {
    const d = doc();
    const a = scenarioClassGroups(d.nodes, 'decoupling');
    const b = nodesByClass(d.nodes);
    expect(a.map((g) => g.cls)).toEqual(b.map((g) => g.cls));
    expect(a.map((g) => g.nodes.map((n) => n.id))).toEqual(b.map((g) => g.nodes.map((n) => n.id)));
    expect(a.map((g) => g.lostUsd)).toEqual(b.map((g) => g.lostUsd));
    expect(a.some((g) => g.unchanged)).toBe(false);
  });

  it('Managed Interdependence: the nodes it runs on by their class under it, the rest unchanged and last; the lost supply is the supply shifted', () => {
    const d = doc();
    const g = scenarioClassGroups(d.nodes, 'managed');
    const ran = d.nodes.filter((n) => nodeScenario(n, 'managed').ran);
    expect(g[g.length - 1].cls).toBe(UNCHANGED);
    expect(g[g.length - 1].unchanged).toBe(true);
    expect(g[g.length - 1].info.label).toBe('Unchanged');
    expect(g[g.length - 1].nodes.map((n) => n.id).sort()).toEqual(d.nodes.filter((n) => !nodeScenario(n, 'managed').ran).map((n) => n.id).sort());
    expect(g[g.length - 1].lostUsd).toBe(0);
    const classed = g.filter((x) => !x.unchanged);
    expect(classed.flatMap((x) => x.nodes.map((n) => n.id)).sort()).toEqual(ran.map((n) => n.id).sort());
    for (const x of classed) for (const n of x.nodes) expect(n.diversify!.cls).toBe(x.cls);
    expect(classed.reduce((s, x) => s + x.lostUsd, 0)).toBeCloseTo(diversifyTotals(d.nodes).shiftUsd, 0);
    // one sector only
    const tech = ran[0].techs[0];
    expect(scenarioClassGroups(d.nodes, 'managed', tech).flatMap((x) => x.nodes).every((n) => n.techs.includes(tech))).toBe(true);
  });

  it('the largest values where the scenario ran; the words of an unchanged node by its status', () => {
    const d = doc();
    const rows = d.nodes.map((n) => nodeScenario(n, 'managed'));
    const top = topScenarioNodes(rows, (r) => r.exposure);
    expect(top.every((r) => r.ran)).toBe(true);
    expect(top.map((r) => r.exposure)).toEqual([...top.map((r) => r.exposure)].sort((a, b) => (b ?? 0) - (a ?? 0)));
    expect(unchangedWords(ALREADY_DIVERSIFIED)).toBe('unchanged: already as diversified as its benchmark');
    expect(unchangedWords(WITHIN_CAP)).toBe('unchanged: no supplier to the allied market is above the cap');
    expect(unchangedWords(NO_BENCHMARK)).toBe('unchanged: no benchmark to compare it with');
    expect(unchangedWords(null)).toBe('unchanged: not assessed in this scenario');
    const ree = rows.find((r) => r.node.id === 'ev-wind-mid-ree-compounds')!;
    expect(nodeScenarioNote(ree)).toBe('unchanged: no supplier to the allied market is above the cap');
    expect(nodeScenarioNote(rows.find((r) => r.ran)!)).toBeNull();
    expect(nodeScenarioNote(nodeScenario(d.nodes[0], 'decoupling'))).toBeNull();
  });

  it("a node's three-way CSV columns: Baseline's concentration, Managed Interdependence where it ran", () => {
    const d = doc();
    const mag = d.nodes.find((n) => n.id === 'ev-wind-mid-magnets')!;
    const row = nodeScenarioCsv(mag);
    expect(row).toMatchObject({ baseline_hhi: mag.baseline!.hhi, managed_ran: true, managed_class: 'BUILD', managed_gap_usd: mag.diversify!.residualUsd });
    const ree = nodeScenarioCsv(d.nodes.find((n) => n.id === 'ev-wind-mid-ree-compounds')!);
    expect(ree).toMatchObject({ managed_ran: false, managed_class: null, managed_shifted_usd: null, managed_status: WITHIN_CAP });
  });

  it('the strategic map under Managed Interdependence: only the nodes it runs on, by their class under it', () => {
    const d = doc();
    const pts = mapPointsFor(d.nodes.map((n) => nodeScenario(n, 'managed')));
    expect(pts.map((p) => p.node.id).sort()).toEqual(d.nodes.filter((n) => nodeScenario(n, 'managed').ran).map((n) => n.id).sort());
    for (const p of pts) {
      expect(p.cls).toBe(p.node.diversify!.cls);
      expect(p.x).toBe(p.node.diversify!.excess);
    }
    // Decoupling: the map as before
    expect(mapPointsFor(d.nodes.map((n) => nodeScenario(n, 'decoupling'))).map((p) => p.node.id)).toEqual(mapPoints(d.nodes).map((p) => p.node.id));
  });
});

describe("the Overview's three scenarios", () => {
  it('three rows in order over the same nodes; Baseline nothing lost; Decoupling the nodes own figures', () => {
    const d = doc();
    const rows = overviewRows(d.nodes, d.meta);
    expect(rows.map((r) => [r.id, r.label])).toEqual([
      ['baseline', 'Baseline'],
      ['managed', 'Managed Interdependence'],
      ['decoupling', 'Decoupling'],
    ]);
    expect(new Set(rows.map((r) => r.nodes)).size).toBe(1);
    expect(rows[0]).toMatchObject({ lostUsd: 0, residualUsd: 0, restoredUsd: 0, affectedNodes: 0, affectedDemandUsd: 0, classMix: '' });
    expect(rows[0].split).toEqual({ kept: 1, trade: 0, build: 0 });
    const comparable = comparableNodes(d.nodes);
    expect(rows[2].lostUsd).toBe(comparable.reduce((s, n) => s + (n.lostUsd ?? 0), 0));
    expect(rows[2].affectedNodes).toBe(comparable.filter((n) => (n.lostUsd ?? 0) > 0).length);
    expect(rows[2].classMix).toBe(classMixText(scenarioTotals(d.nodes, 'decoupling').byClass.map((g) => ({ cls: g.cls, usd: g.lostUsd }))));
    expect(classMixText([{ cls: 'BUILD', usd: 2 }, { cls: 'COORDINATE', usd: 1 }])).toBe('BUILD 67% · COORDINATE 33%');
    expect(classMixText([])).toBe('');
    // a share above 0 that rounds to 0% reads "<1%"
    expect(classMixText([{ cls: 'BUILD', usd: 999 }, { cls: 'COORDINATE', usd: 1 }])).toBe('BUILD 100% · COORDINATE <1%');
  });

  it("the Managed Interdependence row equals the explainer's totals: supply to shift, restored by trade, left for new capacity, nodes", () => {
    const d = doc();
    const mi = overviewRows(d.nodes, d.meta)[1];
    const t = diversifyTotals(d.nodes);
    expect(mi.lostUsd).toBeCloseTo(t.shiftUsd, 3);
    expect(mi.restoredUsd).toBeCloseTo(t.tradeUsd, 3);
    expect(mi.residualUsd).toBeCloseTo(t.gapUsd, 3);
    expect(mi.nodesRun).toBe(t.diversified);
    expect(mi.affectedNodes).toBe(t.diversified);
    expect(restoredUsd(d.nodes.map((n) => nodeScenario(n, 'baseline')))).toBe(0);
  });

  it.skipIf(!haveFixture(REAL_DIR))('on the real export too, to the dollar as printed', () => {
    const d = readDoc(REAL_DIR);
    const mi = overviewRows(d.nodes, d.meta)[1];
    const t = diversifyTotals(d.nodes);
    if (!t.diversified) return;
    expect(Math.abs(mi.lostUsd! - t.shiftUsd)).toBeLessThan(1);
    expect(Math.abs(mi.residualUsd! - t.gapUsd)).toBeLessThan(1);
    expect(Math.abs(mi.restoredUsd - t.tradeUsd)).toBeLessThan(1);
    expect(mi.nodesRun).toBe(t.diversified);
  });
});

describe('the compacts per scenario', () => {
  it('ordered by the supply each scenario removes or shifts, the document order on a tie', () => {
    const d = doc();
    expect(compactsByScenario(d.compacts, 'decoupling', nodeOf(d)).map((c) => c.id)).toEqual(d.compacts.map((c) => c.id));
    const last = d.compacts[d.compacts.length - 1];
    last.managed!.lostUsd = 1e12;
    expect(compactsByScenario(d.compacts, 'managed', nodeOf(d))[0].id).toBe(last.id);
    // Baseline: nothing lost anywhere, the document order
    expect(compactsByScenario(d.compacts, 'baseline', nodeOf(d)).map((c) => c.id)).toEqual(d.compacts.map((c) => c.id));
  });

  it('the table rows and their CSV: each compact under the three scenarios', () => {
    const d = doc();
    const rows = compactScenarioRows(d.compacts, nodeOf(d));
    expect(rows.map((r) => r.compact.id)).toEqual(d.compacts.map((c) => c.id));
    const r = rows[0];
    expect([r.baseline.id, r.managed.id, r.decoupling.id]).toEqual(['baseline', 'managed', 'decoupling']);
    const csv = compactScenarioCsvRow(r, 'Title');
    expect(csv).toMatchObject({
      compact: r.compact.id,
      title: 'Title',
      managed_nodes_run: r.compact.managed!.nodesRun,
      managed_lost_or_shifted_usd: r.compact.managed!.lostUsd,
      decoupling_lost_or_shifted_usd: r.compact.metrics.lostUsd,
      baseline_hhi: r.compact.baseline!.hhi,
    });
    expect(String(csv.baseline_top_suppliers)).toMatch(/^[A-Z]{3} \d+(\.\d)?%; /);
  });
});

describe('CSV columns and steps that name the scenario', () => {
  it('aib_scenario and aib_scenario_shock (never "scenario" or "shock", which the run columns write); the run shock of one scenario', () => {
    const d = doc();
    expect(scenarioCsvFields(d.meta, 'managed')).toEqual({ aib_scenario: 'Managed Interdependence', aib_scenario_shock: 'diversify_benchmark' });
    expect(scenarioCsvFields(d.meta, 'baseline')).toEqual({ aib_scenario: 'Baseline', aib_scenario_shock: 'none (observed trade)' });
    expect(scenarioCsvFields(d.meta, 'decoupling').aib_scenario_shock).toBe(d.meta.headlineShock);
    expect(scenarioCsvFields(null, 'managed').aib_scenario).toBe('Managed Interdependence');
    expect(runMetaOf(d.meta, 'decoupling')).toBe(d.meta);
    expect(runMetaOf(d.meta, 'managed').headlineShock).toBe('diversify_benchmark');
  });

  it('the first coverage step is the cap under Managed Interdependence', () => {
    const d = doc();
    expect(coverageSteps(d.compacts[0].metrics).map((s) => s.label)[0]).toBe('Shock (static)');
    const s = coverageSteps(d.compacts[0].metrics, true);
    expect(s.map((x) => x.label)).toEqual(['Cap (static)', 'Existing relationships', 'Trade compact', 'Trade + investment response']);
    expect(s[0].value).toBe(d.compacts[0].metrics.static);
  });
});

describe("the explainer's figures", () => {
  it('the supply to shift is the exported USD where the export has it; the node block can leave out the coverage it shows beside', () => {
    const d = doc();
    const mag = d.nodes.find((n) => n.id === 'ev-wind-mid-magnets')!;
    mag.diversify!.lostUsd = 1234;
    expect(diversifyRow(mag)!.shiftUsd).toBe(1234);
    delete mag.diversify!.lostUsd;
    expect(diversifyRow(mag)!.shiftUsd).toBeCloseTo(mag.diversify!.excess! * mag.demandUsd!, 3);
    const row = diversifyRow(mag)!;
    const all = diversifyWords(row, {});
    const short = diversifyWords(row, {}, { coverage: false });
    expect(all.length - short.length).toBe(1);
    expect(all.find((w) => w.startsWith('With the cap holding'))).toBeTruthy();
    expect(short.find((w) => w.startsWith('With the cap holding'))).toBeUndefined();
  });
});
