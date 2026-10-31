// The strategic layer's derivations (2026-10-20; derive/strategic.ts) and links (nav.ts), on the synthetic
// fixture's `strategic` section and, where the export is present, on the real bundle (public/data/aib):
// the compact hierarchy (every node on one spine, every workstream in at most one compact, the steps in a
// deterministic order), scenario persistence (the same compact ids, steps and nodes in every scenario; only
// the statuses change), the policy portfolio (the counts aggregate the node statuses), the division of
// labour (dots binned from the export's shares, rows along the chain), the Strategic AIB Roles (decoded with
// their evidence; no economy holds every role; the primary roles within the cap), the compact-specific
// roles (supported by a division cell at their lead node), the narratives, the bilateral theses (either
// order) and the strategic routes.

import { describe, expect, it } from 'vitest';
import { AIB_STRATEGIC_ROLE_CODES, AIB_STRATEGIC_SCENARIO_IDS, type AibDoc, type AibStrategic } from '../contract.ts';
import { STRATEGIC_FRAMING } from '../derive/scenarios.ts';
import {
  activeWorkstreamsOf, alsoTouchingOf, compactNodeIds, compactRoleOf, compactRolesOfCompact, compactRolesOfCountry, compactScenarioOf, coreCompactsOf, countryStrategicOf,
  divisionCells, divisionIntensity, divisionLegend, divisionMatrix, divisionValueText, hostTieWords, missionsOf, narrativeOf, nodeHomeOf, nodeStatusAcross, policyMixWords, roleDefOf, roleHoldersOf,
  roleName, scenarioHasCoalitions, scenarioHasInvestment, spanWords, spineOf, spineStatusInfo, statusCounts, strategicCompactOf, strategicCompacts, strategicCountries, strategicFraming,
  strategicScenarioDef, strategicScenarioFor, strategicScenarios, thesesOf, thesisOf, workstreamHomeOf, workstreamOf, workstreamsOf, type DivisionCell,
} from '../derive/strategic.ts';
import {
  aibNavKeyOf, compactIdOfPath, compactsPageOf, isStrategicId, strategicCompactHref, strategicCompactsHref, strategicIdOfPath, workstreamHref, workstreamsHref,
} from '../nav.ts';
import { haveFixture, readDoc, REAL_DIR, SYNTHETIC_DIR } from './fixture.ts';

const doc = (): AibDoc => structuredClone(readDoc(SYNTHETIC_DIR));
const d = doc();
const s = d.strategic!;
const nodeOf = (id: string) => d.nodes.find((n) => n.id === id) ?? null;
const REE = 'ev-wind-mid-ree-compounds';
const MAG = 'ev-wind-mid-magnets';
const cell = (o: Partial<DivisionCell>): DivisionCell => ({ iso3: 'X', node: 'n', supply: null, demand: null, hostRank: null, flags: { anchor: false, specialist: false, host: false, momentum: false }, ...o });

/** The checks every strategic section passes (the fixture's and the real export's). */
function hierarchyAndPersistence(st: AibStrategic, docOf: AibDoc) {
  const ids = st.scenarios.map((x) => x.id);
  const spines = st.compacts.map(compactNodeIds);
  const all = spines.flat();
  // every node on one spine only, a node of the document
  expect(new Set(all).size).toBe(all.length);
  const nodes = new Set(docOf.nodes.map((n) => n.id));
  for (const n of all) expect(nodes.has(n), n).toBe(true);
  // every workstream (compact candidate) in at most one strategic compact, and listed by it
  const owner = new Map<string, string>();
  for (const c of st.compacts) for (const w of c.workstreams) {
    expect(owner.has(w), w).toBe(false);
    owner.set(w, c.id);
  }
  for (const w of st.workstreams) {
    expect(workstreamHomeOf(st, w.id)?.id ?? null).toBe(w.compact);
    if (w.compact) expect(owner.get(w.id)).toBe(w.compact);
  }
  for (const c of st.compacts) {
    // the steps rise strictly: the order is deterministic
    const orders = c.steps.map((x) => x.order);
    expect([...orders].sort((a, b) => a - b)).toEqual(orders);
    expect(new Set(orders).size).toBe(orders.length);
    // persistence: every scenario has the compact, the same spine in the same order
    expect(Object.keys(c.scenarios).sort()).toEqual([...ids].sort());
    for (const sid of ids) {
      const steps = spineOf(c, sid);
      expect(steps.flatMap((x) => x.nodes.map((n) => n.id)), `${c.id} ${sid}`).toEqual(compactNodeIds(c));
      expect(steps.map((x) => x.label)).toEqual(c.steps.map((x) => x.label));
      // the policy portfolio aggregates the node statuses
      const sc = compactScenarioOf(c, sid)!;
      const tally = new Map<string, number>();
      for (const n of steps.flatMap((x) => x.nodes)) tally.set(n.status!, (tally.get(n.status!) ?? 0) + 1);
      expect(Object.fromEntries(statusCounts(sc).map((x) => [x.status, x.n])), `${c.id} ${sid}`).toEqual(Object.fromEntries(tally));
      // the active workstreams are the compact's own
      for (const w of sc.activeWorkstreams) expect(c.workstreams).toContain(w);
    }
  }
  // roles: never all seven, never more primary roles than the cap
  for (const c of st.countries) {
    const x = countryStrategicOf(st, c.iso3)!;
    expect(x.primary.length + x.secondary.length + x.beyondCap.length, c.iso3).toBeLessThan(AIB_STRATEGIC_ROLE_CODES.length);
    expect(x.primary.length).toBeLessThanOrEqual(st.thresholds.maxPrimary);
    // deterministic from the gates: a qualifying role clears the secondary bar on both gates
    for (const r of [...x.primary, ...x.secondary]) for (const g of r.gates) expect(g.pct ?? 0, `${c.iso3} ${r.code}`).toBeGreaterThanOrEqual(st.thresholds.pSecondary - 1e-9);
  }
  // theses read the same in either order
  for (const t of st.theses.slice(0, 50)) {
    const ab = thesisOf(st, t[0], t[1])!;
    const ba = thesisOf(st, t[1], t[0])!;
    expect([ab.a, ab.b]).toEqual([t[0], t[1]]);
    expect([ba.a, ba.b]).toEqual([t[1], t[0]]);
    expect(ba.compacts.map((c) => c.id)).toEqual(ab.compacts.map((c) => c.id));
  }
}

describe('the strategic layer: scenarios and compacts', () => {
  it('the scenario a strategic view reads: the toggle when listed, else the default (Managed Interdependence)', () => {
    expect(strategicScenarios(s).map((x) => x.id)).toEqual([...AIB_STRATEGIC_SCENARIO_IDS]);
    expect(strategicScenarioFor(s, 'managed')).toBe('managed');
    expect(strategicScenarioFor(s, 'decoupling')).toBe('decoupling');
    expect(strategicScenarioFor(s, 'partial')).toBe('partial');
    expect(strategicScenarioFor(s, null)).toBe('managed');
    expect(strategicScenarioFor(s, 'nope')).toBe('managed');
    const odd = { ...structuredClone(s), defaultScenario: 'gone' };
    expect(strategicScenarioFor(odd, null)).toBe('baseline');
    expect(scenarioHasCoalitions(s, 'managed')).toBe(true);
    expect(scenarioHasCoalitions(s, 'partial')).toBe(false);
    expect(scenarioHasInvestment(s, 'baseline')).toBe(false);
    expect(strategicScenarioDef(s, 'partial')!.label).toBe('Partial supplier disruption');
    expect(strategicScenarioDef(s, null)).toBeNull();
    expect(strategicFraming(s)).toBe(s.framing);
    expect(strategicFraming(null)).toBe(STRATEGIC_FRAMING);
  });

  it('the hierarchy and its persistence across scenarios (the fixture)', () => {
    hierarchyAndPersistence(s, d);
    expect(strategicCompacts(s).map((c) => c.id)).toEqual(['SC-MAGNETS', 'SC-BATTERIES', 'SC-WIND', 'SC-SOLAR', 'SC-GRID']);
    expect(strategicCompactOf(s, 'sc-magnets')!.id).toBe('SC-MAGNETS');
    expect(strategicCompactOf(s, 'SC-NOPE')).toBeNull();
    expect(strategicCompactOf(s, null)).toBeNull();
    expect(nodeHomeOf(s, MAG)).toMatchObject({ order: 4, step: 'Magnets', fn: 'components' });
    expect(nodeHomeOf(s, MAG)!.compact.id).toBe('SC-MAGNETS');
    expect(nodeHomeOf(s, 'zinc')).toBeNull();
    expect(spanWords(strategicCompactOf(s, 'SC-MAGNETS')!)).toBe('Separated oxides → Magnets');
  });

  it('the spine: steps in order, each node with its status, coverage and host under the scenario; only the statuses change', () => {
    const c = strategicCompactOf(s, 'SC-MAGNETS')!;
    const managed = spineOf(c, 'managed', nodeOf);
    expect(managed.map((x) => [x.order, x.label])).toEqual([
      [2, 'Separated oxides'],
      [4, 'Magnets'],
    ]);
    expect(managed[0].nodes[0]).toMatchObject({ id: REE, label: 'Rare-earth oxides & compounds', fn: 'processing', status: 'NOT STRESSED', coverage: 1, host: null });
    expect(managed[1].nodes[0]).toMatchObject({ id: MAG, label: 'Magnets, magnet parts & assemblies', fn: 'components', status: 'BUILD', coverage: 0.78, host: 'CCC' });
    expect(managed[1].nodes[0].node).toBe(nodeOf(MAG));
    // without the document's nodes: the id as the label
    expect(spineOf(c, 'decoupling')[1].nodes[0]).toMatchObject({ label: MAG, status: 'BUILD', host: 'DDD', node: null });
    // a scenario the compact has no entry for: no statuses, the same nodes
    expect(spineOf(c, 'nope').flatMap((x) => x.nodes.map((n) => [n.id, n.status]))).toEqual([
      [REE, null],
      [MAG, null],
    ]);
    expect(nodeStatusAcross(s, c, MAG).map((r) => [r.scenario.id, r.status])).toEqual([
      ['baseline', 'NO SHOCK'],
      ['managed', 'BUILD'],
      ['partial', 'COORDINATE'],
      ['decoupling', 'BUILD'],
    ]);
    expect(statusCounts(compactScenarioOf(c, 'decoupling'))).toEqual([
      { status: 'BUILD', n: 1 },
      { status: 'MANAGE SPECIALIZATION', n: 1 },
    ]);
    expect(statusCounts(null)).toEqual([]);
    // two nodes in one step (SC-GRID), the INSUFFICIENT DATA one without coverage
    const grid = spineOf(strategicCompactOf(s, 'SC-GRID')!, 'decoupling', nodeOf);
    expect(grid).toHaveLength(1);
    expect(grid[0].nodes.map((n) => [n.fn, n.status, n.coverage])).toEqual([
      ['components', 'INSUFFICIENT DATA', null],
      ['systems', 'BUILD', 0.7],
    ]);
  });

  it('2026-10-21 review: a host is named only without a tie (the fifth tuple element counts the tie); NOT TESTED is counted and read', () => {
    const c = structuredClone(strategicCompactOf(s, 'SC-MAGNETS')!);
    const sc = c.scenarios.managed!;
    // older bundles carry four elements: no tie figure
    expect(spineOf(c, 'managed')[1].nodes[0].ties).toBeNull();
    sc.nodes[1] = [MAG, 'BUILD', 0.78, null, 4];
    sc.nodes[0] = [REE, 'NOT TESTED', null, null, null];
    sc.counts = { ...sc.counts, notStressed: 0, notTested: 1 };
    const sp = spineOf(c, 'managed', nodeOf);
    expect(sp[1].nodes[0]).toMatchObject({ host: null, ties: 4 });
    expect(nodeStatusAcross(s, c, MAG).find((r) => r.scenario.id === 'managed')).toMatchObject({ host: null, ties: 4 });
    expect(statusCounts(sc)).toEqual([
      { status: 'BUILD', n: 1 },
      { status: 'NOT TESTED', n: 1 },
    ]);
    // the statuses print in plain words; the data's code stays the status
    expect(spineStatusInfo('NOT TESTED')).toMatchObject({ status: 'NOT TESTED', label: 'Not tested', mark: 'hatch', intervention: false });
    expect(spineStatusInfo('NOT TESTED').meaning).toMatch(/comparable goods/);
    const nm = (iso: string) => `name:${iso}`;
    expect(hostTieWords('CCC', 1, nm)).toBe('name:CCC');
    expect(hostTieWords(null, 4, nm)).toBe('4 hosts tie');
    expect(hostTieWords(null, null, nm)).toBeNull();
  });

  it('status marks and words: the classes as their chips, NOT STRESSED and NO SHOCK dashed; an unknown status reads as no data', () => {
    expect(spineStatusInfo('BUILD')).toMatchObject({ color: '#8b9d17', mark: 'fill', intervention: true });
    expect(spineStatusInfo('LOW INTERVENTION')).toMatchObject({ mark: 'hollow', intervention: false });
    expect(spineStatusInfo('NOT STRESSED')).toMatchObject({ label: 'Unaffected', mark: 'dashed', intervention: false });
    expect(spineStatusInfo('NO SHOCK')).toMatchObject({ label: 'No disruption' });
    expect(spineStatusInfo('NO SHOCK').meaning).toMatch(/^Today's network/);
    expect(spineStatusInfo('BUILD').label).toBe('Build capacity');
    expect(spineStatusInfo('PANIC')).toMatchObject({ label: 'PANIC', mark: 'hatch', intervention: false });
    expect(spineStatusInfo(null).status).toBe('NO SHOCK');
  });

  it('the portfolio in words: missions in slot order, the policy mix', () => {
    const c = strategicCompactOf(s, 'SC-MAGNETS')!;
    const dec = compactScenarioOf(c, 'decoupling');
    expect(missionsOf(dec).map((m) => [m.slot, m.cls])).toEqual([
      ['primary', 'BUILD'],
      ['secondary', 'MANAGE SPECIALIZATION'],
      ['supporting', 'COORDINATE'],
    ]);
    expect(missionsOf(dec)[0].text).toBe('build missing capacity in magnets');
    expect(policyMixWords(dec)).toBe('Build capacity + Manage specialization + Coordinate trade');
    expect(policyMixWords(compactScenarioOf(c, 'baseline'))).toBe('no disruption');
    expect(policyMixWords(compactScenarioOf(strategicCompactOf(s, 'SC-SOLAR')!, 'managed'))).toBe('Low intervention');
    expect(policyMixWords(null)).toBe('–');
    expect(missionsOf(compactScenarioOf(strategicCompactOf(s, 'SC-BATTERIES')!, 'partial'))).toEqual([]);
  });

  it('workstreams: assigned to one compact, active by scenario, with their compact candidate', () => {
    const rows = workstreamsOf(s, 'SC-MAGNETS', d, 'partial');
    expect(rows.map((r) => [r.id, r.home, r.active])).toEqual([['AIB-C01', 'SC-MAGNETS', true]]);
    expect(rows[0].candidate).toBe(d.compacts.find((c) => c.id === 'AIB-C01'));
    expect(workstreamsOf(s, 'SC-MAGNETS', d, 'baseline')[0].active).toBe(false);
    expect(workstreamsOf(s, 'SC-SOLAR')).toEqual([]);
    expect(workstreamsOf(s, 'SC-NOPE')).toEqual([]);
    expect(workstreamOf(s, 'aib-c02')!.compact).toBe('SC-BATTERIES');
    expect(workstreamOf(s, 'AIB-C99')).toBeNull();
    expect(workstreamHomeOf(s, 'AIB-C03')!.id).toBe('SC-WIND');
    expect(activeWorkstreamsOf(strategicCompactOf(s, 'SC-BATTERIES')!, 'managed')).toEqual(['AIB-C02']);
    expect(activeWorkstreamsOf(strategicCompactOf(s, 'SC-BATTERIES')!, 'partial')).toEqual([]);
    // a workstream assigned elsewhere that also spans the compact
    const s2 = structuredClone(s);
    s2.compacts[2].alsoTouching = ['AIB-C01'];
    expect(alsoTouchingOf(s2, 'SC-WIND', d, 'managed').map((r) => [r.id, r.home, r.active])).toEqual([['AIB-C01', 'SC-MAGNETS', true]]);
    expect(alsoTouchingOf(s, 'SC-WIND')).toEqual([]);
  });
});

describe('the strategic layer: the division of labour', () => {
  const mags = strategicCompactOf(s, 'SC-MAGNETS')!;

  it('cells decode the tuples; the dots bin the shares (2%, 10%, 25%) and the host ranks (1, 2-3, 4-5)', () => {
    expect(divisionCells(mags)[0]).toEqual({ iso3: 'ZZZ', node: REE, supply: 0.7, demand: 0, hostRank: null, flags: { anchor: true, specialist: false, host: false, momentum: false } });
    expect(divisionCells(mags).find((c) => c.iso3 === 'CCC' && c.node === MAG)!.flags).toEqual({ anchor: true, specialist: true, host: true, momentum: false });
    expect([0.7, 0.25, 0.11, 0.1, 0.04, 0.02, 0.019, 0].map((x) => divisionIntensity(cell({ supply: x }), 'supply'))).toEqual([3, 3, 2, 2, 1, 1, 0, 0]);
    expect(divisionIntensity(cell({ demand: 0.41 }), 'demand')).toBe(3);
    expect([1, 2, 3, 4, 5, 6, null].map((r) => divisionIntensity(cell({ hostRank: r }), 'host'))).toEqual([3, 2, 2, 1, 1, 1, 0]);
    expect(divisionIntensity(null, 'supply')).toBe(0);
    expect(divisionValueText(cell({ supply: 0.18 }), 'supply')).toBe('18%');
    expect(divisionValueText(cell({ hostRank: 2 }), 'host')).toBe('rank 2');
    expect(divisionValueText(null, 'demand')).toBe('–');
    expect(divisionLegend('supply').levels.map((l) => l.words)).toEqual(['2% to 10%', '10% to 25%', '25% or more']);
    expect(divisionLegend('host').levels.map((l) => l.words)).toEqual(['ranked 4th or lower', 'ranked 2nd or 3rd', 'the best-ranked host']);
    expect(divisionLegend('demand').note).toMatch(/no economy is expected to build every node/);
  });

  it('the matrix: the nodes in spine order; the economies with a dot, along the chain; allied or not; their compact role', () => {
    const m = divisionMatrix(s, mags, { measure: 'supply', nodeOf });
    expect(m.nodes.map((n) => [n.id, n.label, n.step])).toEqual([
      [REE, 'Rare-earth oxides & compounds', 'Separated oxides'],
      [MAG, 'Magnets, magnet parts & assemblies', 'Magnets'],
    ]);
    expect(m.rows.map((r) => [r.iso3, r.levels])).toEqual([
      ['ZZZ', [3, 3]],
      ['CCC', [2, 2]],
      ['DDD', [1, 1]],
      ['AAA', [1, 2]],
      ['BBB', [0, 1]],
      ['EEE', [0, 1]],
    ]);
    expect(m.hidden).toBe(0);
    expect(m.rows.map((r) => r.allied)).toEqual([false, true, true, true, true, true]);
    expect(m.rows[0].role).toBe('Processing anchor (non-allied supplier)');
    expect(m.rows[3].cells[1]!.supply).toBe(0.18);
    // the cap, allied only, exactly these economies, another measure
    const top = divisionMatrix(s, mags, { maxRows: 2 });
    expect([top.rows.map((r) => r.iso3), top.hidden]).toEqual([['ZZZ', 'CCC'], 4]);
    expect(divisionMatrix(s, mags, { alliedOnly: true }).rows.map((r) => r.iso3)).not.toContain('ZZZ');
    const fixed = divisionMatrix(s, mags, { countries: ['FFF', 'AAA'] });
    expect(fixed.rows.map((r) => [r.iso3, r.levels, r.total])).toEqual([
      ['FFF', [0, 0], 0],
      ['AAA', [1, 2], 3],
    ]);
    expect(divisionMatrix(s, mags, { measure: 'host' }).rows.map((r) => [r.iso3, r.levels])).toEqual([
      ['CCC', [0, 3]],
      ['DDD', [0, 3]],
      ['EEE', [0, 2]],
    ]);
    expect(divisionMatrix(s, mags, { measure: 'demand' }).rows[0].iso3).toBe('AAA');
  });
});

describe('the strategic layer: countries, compact roles, narratives and theses', () => {
  it('a country\'s Strategic AIB Roles, decoded with both gates; the role vocabulary', () => {
    const c = countryStrategicOf(s, 'ccc')!;
    expect([c.primary, c.secondary, c.beyondCap].map((l) => l.map((r) => r.code))).toEqual([['STM', 'RPA'], ['SMI', 'CBH'], ['DSP']]);
    expect(c.roles[0]).toEqual({
      code: 'STM',
      name: 'Specialized Technology & Manufacturing Anchor',
      noun: 'specialized technology',
      status: 'primary',
      rank: 1,
      strength: 0.95,
      supplyRole: true,
      gates: [
        { metric: 'spec_value_share', label: 'value of specialist component positions', value: 0.03, pct: 0.97 },
        { metric: 'spec_compacts', label: 'compacts with a specialist position', value: 2, pct: 0.95 },
      ],
    });
    expect(c.roles.find((r) => r.code === 'DMA')!.status).toBeNull();
    expect(c.metrics.spec_value_share).toBe(0.03);
    expect(countryStrategicOf(s, 'GGG')!.metrics).toEqual({});
    const z = countryStrategicOf(s, 'ZZZ')!;
    expect([z.allied, z.majorSupplier, z.roles.length, z.roles.every((r) => r.supplyRole)]).toEqual([false, true, 3, true]);
    expect(countryStrategicOf(s, 'NOPE')).toBeNull();
    expect(countryStrategicOf(s, 'FFF')!.primary).toEqual([]);
    expect(roleName(s, 'CBH')).toBe('Coordination / Bridge Hub');
    expect(roleName(s, 'XYZ')).toBe('XYZ');
    expect(roleDefOf(s, 'DSP')!.gates).toEqual(['host_gap_usd_bn', 'scaleup_rate']);
    expect(roleHoldersOf(s, 'SMI').map((h) => [h.iso3, h.role.status])).toEqual([
      ['ZZZ', 'primary'],
      ['BBB', 'primary'],
      ['CCC', 'secondary'],
      ['EEE', 'secondary'],
    ]);
    expect(roleHoldersOf(s, 'SMI', 'primary').map((h) => h.iso3)).toEqual(['ZZZ', 'BBB']);
    expect(strategicCountries(s, { alliedOnly: true }).map((c) => c.iso3)).toEqual(['AAA', 'BBB', 'CCC', 'DDD', 'EEE', 'FFF']);
    expect(strategicCountries(s).slice(-2).map((c) => c.iso3)).toEqual(['GGG', 'ZZZ']);
  });

  it('compact-specific roles: the same country in different roles by compact, each supported by a division cell at its lead node', () => {
    expect(compactRolesOfCountry(s, 'aaa').map((r) => [r.compact, r.role])).toEqual([
      ['SC-WIND', 'Demand anchor'],
      ['SC-MAGNETS', 'Demand anchor'],
      ['SC-BATTERIES', 'Demand anchor'],
      ['SC-GRID', 'Demand anchor'],
      ['SC-SOLAR', 'Demand anchor'],
    ]);
    expect(new Set(compactRolesOfCountry(s, 'CCC').map((r) => r.role))).toEqual(new Set(['Processing anchor', 'Capacity-building host']));
    expect(compactRolesOfCompact(s, 'sc-magnets').map((r) => r.iso3)).toEqual(['ZZZ', 'AAA', 'CCC', 'BBB', 'DDD', 'EEE']);
    expect(compactRoleOf(s, 'SC-MAGNETS', 'ddd')!.role).toBe('Capacity-building host');
    expect(compactRoleOf(s, 'SC-SOLAR', 'DDD')).toBeNull();
    expect(compactRolesOfCountry(s, null)).toEqual([]);
    // the evidence: a lead node is one where the economy has a division cell
    for (const r of s.compactRoles.filter((x) => x.leadNode)) {
      const c = strategicCompactOf(s, r.compact)!;
      expect(divisionCells(c).some((x) => x.iso3 === r.iso3 && x.node === r.leadNode), `${r.compact} ${r.iso3}`).toBe(true);
    }
    expect(coreCompactsOf(s, 'ZZZ').map((x) => [x.compact.id, x.allied])).toEqual([
      ['SC-MAGNETS', false],
      ['SC-BATTERIES', false],
      ['SC-SOLAR', false],
      ['SC-GRID', false],
    ]);
    expect(coreCompactsOf(s, 'AAA')).toHaveLength(5);
  });

  it('narratives from their slots; none for an economy without roles', () => {
    const n = narrativeOf(s, 'AAA')!;
    expect(n.narrative).toMatch(/^Avalon: Combines demand & market and capital & investment roles; secondary coordination \/ bridge role\./);
    expect(n.slots!.partners).toBe('Carpania, Borduria, Elbonia');
    expect(narrativeOf(s, 'FFF')).toBeNull();
    expect(narrativeOf(s, null)).toBeNull();
  });

  it('bilateral theses: either order, the complementarity from both compact roles, the logic', () => {
    const t = thesisOf(s, 'ccc', 'AAA')!;
    expect([t.a, t.b]).toEqual(['CCC', 'AAA']);
    expect(t.compacts.map((c) => c.id)).toEqual(['SC-MAGNETS', 'SC-BATTERIES']);
    expect(t.logic).toEqual(['trade', 'technology', 'resilience / second sourcing']);
    expect(t.complementarity.map((x) => [x.compact.id, x.a?.role, x.b?.role])).toEqual([
      ['SC-MAGNETS', 'Processing anchor', 'Demand anchor'],
      ['SC-BATTERIES', 'Processing anchor', 'Demand anchor'],
    ]);
    expect(t.rolesA.map((r) => r.code)).toEqual(['STM', 'RPA', 'SMI', 'CBH']);
    expect(t.rolesB.map((r) => r.code)).toEqual(['DMA', 'CIA', 'CBH']);
    expect(thesisOf(s, 'AAA', 'GGG')).toBeNull();
    expect(thesisOf(s, null, 'AAA')).toBeNull();
    expect(thesesOf(s, 'EEE').map((x) => [x.b, x.compacts.length])).toEqual([
      ['AAA', 2],
      ['BBB', 1],
      ['CCC', 0],
    ]);
  });
});

describe('the strategic routes and links', () => {
  it('paths: the landing, a strategic compact, the Workstreams index, a workstream, About', () => {
    expect(compactsPageOf('/compacts')).toBe('strategic');
    expect(compactsPageOf('/compacts/s')).toBe('strategic');
    expect(compactsPageOf('/compacts/s/SC-MAGNETS')).toBe('strategic-compact');
    expect(compactsPageOf('/compacts/workstreams')).toBe('workstreams');
    expect(compactsPageOf('/compacts/AIB-C01')).toBe('workstream');
    expect(compactsPageOf('/compacts/about')).toBe('about');
    expect(compactsPageOf('/aib')).toBeNull();
    expect(strategicIdOfPath('/compacts/s/SC-MAGNETS')).toBe('SC-MAGNETS');
    expect(strategicIdOfPath('/compacts/SC-MAGNETS')).toBeNull();
    expect(compactIdOfPath('/compacts/workstreams')).toBeNull();
    expect(compactIdOfPath('/compacts/s')).toBeNull();
    expect(compactIdOfPath('/compacts/AIB-C01')).toBe('AIB-C01');
    for (const p of ['/compacts', '/compacts/workstreams', '/compacts/s/SC-MAGNETS', '/compacts/AIB-C01']) expect(aibNavKeyOf(p), p).toBe('compacts');
    expect(isStrategicId('sc-magnets')).toBe(true);
    expect(isStrategicId('AIB-C01')).toBe(false);
    expect(isStrategicId(null)).toBe(false);
  });

  it('hrefs carry the toggle (Managed Interdependence never written)', () => {
    expect(strategicCompactHref('SC-MAGNETS')).toBe('/compacts/s/SC-MAGNETS');
    expect(strategicCompactHref('SC-MAGNETS', 'managed')).toBe('/compacts/s/SC-MAGNETS');
    expect(strategicCompactHref('SC-MAGNETS', 'decoupling', 'division')).toBe('/compacts/s/SC-MAGNETS?scn=decoupling#division');
    expect(strategicCompactsHref()).toBe('/compacts');
    expect(strategicCompactsHref('decoupling')).toBe('/compacts?scn=decoupling');
    expect(workstreamsHref()).toBe('/compacts/workstreams');
    expect(workstreamsHref(undefined, 'SC-WIND')).toBe('/compacts/workstreams#sc-SC-WIND');
    expect(workstreamHref('AIB-C01', 'decoupling')).toBe('/compacts/AIB-C01?scn=decoupling');
  });
});

describe.skipIf(!haveFixture(REAL_DIR))('the strategic layer of the real export (public/data/aib)', () => {
  const real = readDoc(REAL_DIR);
  const rs = real.strategic;

  it.skipIf(!rs)('hierarchy, persistence, portfolio counts and roles hold; Japan, Korea and the United States do not share one role set', () => {
    const st = rs!;
    hierarchyAndPersistence(st, real);
    expect(st.defaultScenario).toBe('managed');
    expect(strategicScenarioFor(st, null)).toBe('managed');
    // the matrix of every compact: its spine as columns, at most 12 rows
    for (const c of st.compacts) {
      const m = divisionMatrix(st, c, { nodeOf: (id) => real.nodes.find((n) => n.id === id) ?? null });
      expect(m.nodes.map((n) => n.id)).toEqual(compactNodeIds(c));
      expect(m.rows.length).toBeLessThanOrEqual(12);
    }
    const sets = ['JPN', 'KOR', 'USA'].flatMap((iso) => {
      const c = countryStrategicOf(st, iso);
      return c ? [c.primary.map((r) => r.code).sort().join(' ')] : [];
    });
    if (sets.length === 3) expect(new Set(sets).size).toBeGreaterThan(1);
  });
});
