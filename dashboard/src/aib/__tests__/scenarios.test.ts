// The scenario toggle's foundation (2026-10-19): the contract of the main document's scenario fields and of
// the two lazily loaded scenario files (tolerant checks, an older bundle still valid, the paths of the R
// mirror R/dashboard/aib/aib_check.R), the store's lazy loading, the URL state (scn) and every link that
// carries it, the Country profile's host key, and the derivations every page builds on (labels, words,
// colours, node / compact / set figures per scenario, the detail sections and accessors).

import { describe, expect, it } from 'vitest';
import {
  AIB_SCENARIO_FILE_RE, checkAibManifest, checkCompactsDoc, checkScenarioDoc, normalizeScenarioDoc, scenarioDefsOf, scenarioFilesOf, scenarioNamesOf, supplyScenariosOf,
  type AibDoc, type AibManagedScenarioDoc,
} from '../contract.ts';
import { AibStore, scenarioKey } from '../data/store.ts';
import { CLASS_INFO } from '../derive/classes.ts';
import { diversifyTotals } from '../derive/diversify.ts';
import {
  BASELINE_NO_COALITION, comparableNodes, compactDetail, compactScenario, compactScenarios, compactSuppliersToday, effectiveScenario, hasBaseline, hasScenarios, headlineNetwork,
  lostByClassOf, nodeScenario, nodeScenarios, nodeSuppliersToday, parseClassMix, scenarioCaveat, SCENARIO_COLOR, SCENARIO_DEFAULTS, scenarioInfo, scenarioInvestmentFlows,
  scenarioLabel, scenarioList, scenarioNameOf, scenarioNetwork, scenarioShockLabel, scenarioTotals, scenarioTotalsAll,
} from '../derive/scenarios.ts';
import { aibNavItems, compactHref, rolesHref, strategicCompactHref, workstreamsHref } from '../nav.ts';
import { clearScenarioSearch, DEFAULT_TOGGLE, hasScenarioKey, hasStrayScenario, scenarioCarry, scenarioOfSearch, SCENARIO_TOGGLES, setScenarioSearch, withScenario } from '../scenarioUrl.ts';
import { aibTabHref, rememberLocation, resetLastLocations } from '../../shell/lastLocation.ts';
import { moduleItems } from '../../shell/Header.tsx';
import { COUNTRY_KEY_ORDER, parseCountrySearch, serializeCountrySearch } from '../../country/url.ts';
import { fixtureSource, readDoc, readManifest, readScenarioDoc, stripDiversify, stripMethodology, stripScenarios, SYNTHETIC_DIR } from './fixture.ts';

const doc = (): AibDoc => structuredClone(readDoc(SYNTHETIC_DIR));
const managed = (): AibManagedScenarioDoc => structuredClone(readScenarioDoc('managed', SYNTHETIC_DIR)!);
const baseline = () => structuredClone(readScenarioDoc('baseline', SYNTHETIC_DIR)!);
type Loose = Record<string, unknown>;
const loose = (x: unknown) => x as Loose;

describe('the scenario contract (2026-10-19)', () => {
  it('the manifest names both scenario files; absent, null and unknown entries pass; a wrong name is reported', () => {
    const m = readManifest(SYNTHETIC_DIR);
    expect(checkAibManifest(m)).toEqual([]);
    const files = scenarioFilesOf(m);
    expect(Object.keys(files).sort()).toEqual(['baseline', 'managed']);
    for (const f of Object.values(files)) expect(AIB_SCENARIO_FILE_RE.test(f!)).toBe(true);
    const older = structuredClone(m);
    delete loose(older.files).scenarios;
    expect(checkAibManifest(older)).toEqual([]);
    expect(scenarioFilesOf(older)).toEqual({});
    expect(checkAibManifest({ ...m, files: { ...m.files, scenarios: null } })).toEqual([]);
    expect(checkAibManifest({ ...m, files: { ...m.files, scenarios: { managed: null, other: 'x.json' } } })).toEqual([]);
    const bad = { ...m, files: { ...m.files, scenarios: { managed: 'scenario-baseline.0123abcd.json', baseline: 'scenario-baseline.json' } } };
    expect(checkAibManifest(bad)).toEqual([
      'manifest.files.scenarios.managed: not a scenario-managed.<8 hex>.json file name',
      'manifest.files.scenarios.baseline: not a scenario-baseline.<8 hex>.json file name',
    ]);
    expect(scenarioFilesOf(bad)).toEqual({});
    expect(checkAibManifest({ ...m, files: { ...m.files, scenarios: 'scenario-managed.0123abcd.json' } })).toEqual(['manifest.files.scenarios: not an object']);
  });

  it('the main document carries the scenario list, node.baseline, the diversify run fields, compact.managed and compact.baseline; it validates', () => {
    const d = doc();
    expect(checkCompactsDoc(d)).toEqual([]);
    expect(scenarioDefsOf(d.meta).map((s) => s.id)).toEqual(['baseline', 'managed', 'decoupling']);
    expect(scenarioDefsOf(d.meta)[1].shockId).toBe('diversify_benchmark');
    expect(supplyScenariosOf(d.meta)).toEqual(['conservative', 'reallocation']);
    // every optimized node has today's figures; the INSUFFICIENT DATA node has none
    expect(d.nodes.filter((n) => !n.baseline).map((n) => n.id)).toEqual(['grid-mid-transformers']);
    const mag = d.nodes.find((n) => n.id === 'ev-wind-mid-magnets')!;
    expect(mag.baseline!.largest).toBe(mag.baseLargest);
    expect(mag.baseline!.suppliers).toHaveLength(5);
    expect(mag.diversify!.lostUsd).toBe(Math.round(mag.diversify!.excess! * mag.demandUsd!));
    expect(d.nodes.find((n) => n.id === 'bat-up-graphite')!.diversify!.lostUsd).toBeNull();
    for (const c of d.compacts) {
      expect(c.managed!.nodesRun).toBe(1);
      expect(c.baseline!.suppliers.length).toBeGreaterThan(0);
    }
  });

  it('an older bundle validates: the supply scenarios in meta.scenarios, no scenario fields', () => {
    const older = stripScenarios(doc());
    expect(older.meta.scenarios).toEqual(['conservative', 'reallocation']);
    expect(older.meta.supplyScenarios).toBeUndefined();
    expect(older.nodes.some((n) => n.baseline || (n.diversify && 'lostUsd' in n.diversify))).toBe(false);
    expect(older.compacts.some((c) => c.managed || c.baseline)).toBe(false);
    expect(checkCompactsDoc(older)).toEqual([]);
    expect(scenarioDefsOf(older.meta)).toEqual([]);
    expect(supplyScenariosOf(older.meta)).toEqual(['conservative', 'reallocation']);
    expect(checkCompactsDoc(stripDiversify(doc()))).toEqual([]);
    expect(checkCompactsDoc(stripMethodology(doc()))).toEqual([]);
    // the objects without meta.supplyScenarios: the headline scenario alone
    const m = doc().meta;
    delete m.supplyScenarios;
    expect(supplyScenariosOf(m)).toEqual(['reallocation']);
  });

  it('bad scenario fields are reported with their paths (the R mirror names the same ones)', () => {
    const d = doc();
    const defs = d.meta.scenarios as unknown as Loose[];
    defs[1].id = 'diversify';
    defs[2].id = 'baseline';
    delete defs[0].words;
    loose(d.meta).supplyScenarios = 'reallocation';
    const i = d.nodes.findIndex((n) => n.id === 'ev-wind-mid-magnets');
    const n = d.nodes[i];
    (n.baseline!.suppliers as unknown[])[0] = ['ZZZ'];
    n.baseline!.largestShare = 1.4;
    n.diversify!.lostUsd = -1;
    n.diversify!.newLinks = 2.5;
    d.compacts[0].managed!.nodesRun = 3;
    delete loose(d.compacts[0].managed).optimized;
    loose(d.compacts[1]).baseline = 'none';
    (d.meta.scenarios as unknown[]).push('conservative');
    const p = checkCompactsDoc(d);
    for (const want of [
      "meta.scenarios[1].id: 'diversify' is not one of baseline, managed, decoupling",
      "meta.scenarios[2].id: duplicate 'baseline'",
      'meta.scenarios[0].words: expected a string',
      'meta.scenarios[3]: expected a scenario object (meta.scenarios mixes names and scenario objects)',
      'meta.supplyScenarios: expected an array (a one-item array collapsed to a string?)',
      `nodes[${i}].baseline.suppliers[0]: expected [ISO3, share]`,
      `nodes[${i}].baseline.largestShare: 1.4 above 1`,
      `nodes[${i}].diversify.lostUsd: -1 below 0`,
      `nodes[${i}].diversify.newLinks: 2.5 is not an integer`,
      'compacts[0].managed.nodesRun: 3 above 2',
      'compacts[0].managed.optimized: expected a number or null',
      'compacts[1].baseline: not an object',
    ]) {
      expect(p).toContain(want);
    }
    expect(p).toHaveLength(12);
  });

  it('both scenario files validate, with and without the main document; normalized, the gaps are filled', () => {
    const main = doc();
    expect(checkScenarioDoc(managed(), main, 'managed')).toEqual([]);
    expect(checkScenarioDoc(baseline(), main, 'baseline')).toEqual([]);
    expect(checkScenarioDoc(managed())).toEqual([]);
    expect(checkScenarioDoc(baseline())).toEqual([]);
    // the baseline network: observed, no shock
    const b = baseline();
    expect(b.network!.variant).toMatchObject({ shock: 'baseline', scenario: 'observed', shocks: [] });
    expect(b.network!.edges.every((e) => e.investment === 0 && e.nShocks === 0 && e.cumTrade === e.trade)).toBe(true);
    expect(b.nodes.map((n) => n.id)).toEqual(main.nodes.filter((n) => n.baseline).map((n) => n.id));
    // a tolerant managed compact: no arrays, no metrics, coalition null with its reason
    const m = managed();
    const c = loose(m.compacts[2]);
    for (const k of ['anchors', 'suppliers', 'hosts', 'technologyPartners', 'bridges', 'associates', 'roles', 'metrics', 'path', 'trade', 'investment']) delete c[k];
    c.coalition = null;
    c.coalitionReason = 'no node of this compact is more concentrated than its benchmark';
    c.nodesRun = 0;
    loose(m).countries = [];
    delete loose(m).network;
    delete loose(m).investmentFlows;
    expect(checkScenarioDoc(m, main, 'managed')).toEqual([]);
    const nm = normalizeScenarioDoc(m);
    expect(nm.compacts[2]).toMatchObject({ coalition: null, anchors: [], roles: [], metrics: null, path: [], trade: [], investment: [], type: null, archetype: null, policyNeeds: [], secondaryClass: null });
    // the type, archetype, policy needs and secondary class under the scenario: optional, checked when present
    const t = managed();
    Object.assign(loose(t.compacts[0]), { type: 'candidate compact', archetype: 'Integrated Compact', policyNeeds: ['monitor'], secondaryClass: null });
    expect(checkScenarioDoc(t, main, 'managed')).toEqual([]);
    Object.assign(loose(t.compacts[0]), { type: 'a compact', archetype: 'Some Compact', policyNeeds: 'monitor' });
    expect(checkScenarioDoc(t, main, 'managed')).toEqual([
      "compacts[0].type: 'a compact' is not one of candidate compact, bilateral arrangement sufficient, compact unnecessary: existing relationships do nearly as well",
      "compacts[0].archetype: 'Some Compact' is not one of Trade / Coordination Compact, Investment / Capacity Compact, Specialization Compact, Integrated Compact",
      'compacts[0].policyNeeds: expected an array (a one-item array collapsed to a string?)',
    ]);
    expect(nm.countries).toEqual({});
    expect(scenarioNamesOf(nm)).toEqual({});
  });

  it('bad scenario files are reported with their paths', () => {
    const main = doc();
    const m = managed();
    loose(m.compacts[2]).coalition = null;
    m.compacts[1].id = 'AIB-C09';
    loose(m.compacts[0].coalition).reached99 = 'yes';
    loose(m.compacts[0].trade[0]).relationship = 'domestic';
    m.network!.edges[0].nodes = ['wafers'];
    m.investmentFlows!.pairs[0].rank = 9;
    loose(m).shockId = null;
    const p = checkScenarioDoc(m, main, 'baseline');
    for (const want of [
      "document.scenario: 'managed' where the manifest names a baseline file",
      'document.shockId: expected a string',
      'compacts[2].coalitionReason: expected a string when coalition is null',
      "compacts[1].id: 'AIB-C09' is not a compact of the main document",
      'compacts[0].coalition.reached99: expected true or false',
      "compacts[0].trade[0].relationship: 'domestic' is not one of new, expanded",
      "network.edges[0].nodes: 'wafers' is not a node of the document",
      'investmentFlows.pairs[0].rank: 9 above 5',
    ]) {
      expect(p).toContain(want);
    }
    expect(p).toHaveLength(8);
    const b = baseline();
    (b.nodes[0].suppliers as unknown[])[1] = ['AAA', -5, 0.2];
    b.nodes[1].id = b.nodes[0].id;
    b.compacts[0].id = 'AIB-C77';
    loose(b).schema = 'osi-aib';
    const pb = checkScenarioDoc(b, main);
    for (const want of ["document.schema: expected 'osi-aib-scenario'", 'nodes[0].suppliers[1][1]: -5 below 0', `nodes[1].id: duplicate '${b.nodes[0].id}'`, "compacts[0].id: 'AIB-C77' is not a compact of the main document"]) {
      expect(pb).toContain(want);
    }
    expect(checkScenarioDoc('x')).toEqual(['document: not an object']);
    expect(checkScenarioDoc({ ...managed(), version: 2 })).toEqual(['document: version 2 needs a newer app']);
    expect(checkScenarioDoc({ ...managed(), scenario: 'decoupling' })).toContain("document.scenario: 'decoupling' is not one of managed, baseline");
  });
});

describe('the store loads the scenario files lazily', () => {
  it('the manifest and the document never fetch them; scenario() does, once, cross-checked and normalized; prefetch never throws', async () => {
    const src = fixtureSource(SYNTHETIC_DIR);
    const s = new AibStore(src);
    const m = await s.manifest();
    await s.doc(m);
    expect(src.calls).toEqual(['manifest.json', m.files.compacts]);
    const md = await s.scenario(m, 'managed');
    expect(md!.scenario).toBe('managed');
    await s.scenario(m, 'managed');
    const bd = await s.scenario(m, 'baseline');
    expect(bd!.scenario).toBe('baseline');
    expect(src.calls).toEqual(['manifest.json', m.files.compacts, m.files.scenarios!.managed, m.files.scenarios!.baseline]);
    expect(s.peek(scenarioKey(m.files.scenarios!.managed!))).toBe(md);
    // a manifest without scenario files: null, nothing fetched
    const older = new AibStore(fixtureSource(SYNTHETIC_DIR, undefined, { noScenarioFiles: true }));
    const om = await older.manifest();
    expect(await older.scenario(om, 'managed')).toBeNull();
  });

  it('a broken file is a CONTRACT error (cross-checked against the loaded document), retried on the next call; prefetch swallows it', async () => {
    let n = 0;
    const src = fixtureSource(SYNTHETIC_DIR, undefined, {
      editScenario: (d) => {
        n += 1;
        d.compacts[0].id = 'AIB-C99';
      },
    });
    const s = new AibStore(src);
    const m = await s.manifest();
    await s.doc(m);
    await expect(s.scenario(m, 'managed')).rejects.toMatchObject({ code: 'CONTRACT' });
    const e = await s.scenario(m, 'managed').catch((x: Error) => x);
    expect((e as Error).message).toMatch(/is not a compact of the main document/);
    expect(n).toBe(2);
    expect(() => s.prefetchScenario(m, 'managed')).not.toThrow();
    s.prefetchScenario(null, 'managed');
  });
});

describe('the scenario in the URL (scn)', () => {
  it('reads Managed Interdependence unless scn=decoupling (the default since 2026-10-20); a stray value is flagged; writes omit the default', () => {
    expect(DEFAULT_TOGGLE).toBe('managed');
    expect(SCENARIO_TOGGLES).toEqual(['managed', 'decoupling']);
    expect(scenarioOfSearch('')).toBe('managed');
    expect(scenarioOfSearch('?scn=decoupling')).toBe('decoupling');
    expect(scenarioOfSearch('?scn=baseline')).toBe('managed');
    // a link written before the default changed still opens Managed Interdependence
    expect(scenarioOfSearch('?scn=managed')).toBe('managed');
    expect(scenarioOfSearch(new URLSearchParams('mode=trade&scn=decoupling'))).toBe('decoupling');
    expect(hasStrayScenario('?scn=decoupling')).toBe(false);
    expect(hasStrayScenario('?scn=managed')).toBe(true);
    expect(hasStrayScenario('?scn=foo&x=1')).toBe(true);
    expect(hasStrayScenario('?scn=decoupling&scn=decoupling')).toBe(true);
    expect(hasStrayScenario('?x=1')).toBe(false);
    expect(hasScenarioKey('?x=1&scn=decoupling')).toBe(true);
    expect(hasScenarioKey('?x=1')).toBe(false);
    expect(setScenarioSearch('?mode=trade', 'decoupling')).toBe('?mode=trade&scn=decoupling');
    expect(setScenarioSearch('?scn=decoupling&mode=trade', 'managed')).toBe('?mode=trade');
    expect(setScenarioSearch('?scn=foo', 'managed')).toBe('');
    expect(clearScenarioSearch('?scn=decoupling&mode=trade')).toBe('?mode=trade');
    expect(scenarioCarry('?mode=x&scn=decoupling')).toBe('?scn=decoupling');
    expect(scenarioCarry('?mode=x&scn=managed')).toBe('');
    expect(scenarioCarry('?mode=x')).toBe('');
  });

  it('withScenario carries the choice on any href: a path, a query, a hash', () => {
    expect(withScenario('/compacts/AIB-C01', 'decoupling')).toBe('/compacts/AIB-C01?scn=decoupling');
    expect(withScenario('/compacts/AIB-C01', 'managed')).toBe('/compacts/AIB-C01');
    expect(withScenario('/aib/roles#role-USA', 'decoupling')).toBe('/aib/roles?scn=decoupling#role-USA');
    expect(withScenario('/aib/partnerships?mode=investment&scn=decoupling#x', 'managed')).toBe('/aib/partnerships?mode=investment#x');
    expect(withScenario('/aib/partnerships?mode=investment&scn=managed#x', 'managed')).toBe('/aib/partnerships?mode=investment#x');
    expect(withScenario('/aib/partnerships?mode=investment', '?scn=decoupling')).toBe('/aib/partnerships?mode=investment&scn=decoupling');
    expect(compactHref('AIB-C01', 'decoupling')).toBe('/compacts/AIB-C01?scn=decoupling');
    expect(compactHref('AIB-C01')).toBe('/compacts/AIB-C01');
    expect(rolesHref('usa', 'decoupling')).toBe('/aib/roles?economy=USA&scn=decoupling#role-USA');
    expect(strategicCompactHref('SC-MAGNETS', 'decoupling', 'spine')).toBe('/compacts/s/SC-MAGNETS?scn=decoupling#spine');
    expect(workstreamsHref('decoupling', 'SC-MAGNETS')).toBe('/compacts/workstreams?scn=decoupling#sc-SC-MAGNETS');
  });

  it('the module nav, the static fallback and the header tab carry it', () => {
    expect(aibNavItems('/aib', null, '?scn=decoupling&mode=trade').map((i) => i.to)).toEqual([
      '/aib?scn=decoupling', '/aib/partnerships?scn=decoupling', '/compacts?scn=decoupling', '/aib/roles?scn=decoupling', '/aib/diversification?scn=decoupling',
    ]);
    expect(aibNavItems('/aib').map((i) => i.to)[0]).toBe('/aib');
    expect(aibNavItems('/aib', null, '?scn=managed').map((i) => i.to)[0]).toBe('/aib');
    const fallback = moduleItems('aib', { module: null, items: [] }, '/compacts', '?scn=decoupling');
    expect(fallback.every((i) => i.to.endsWith('?scn=decoupling'))).toBe(true);
    expect(fallback.find((i) => i.active)!.key).toBe('compacts');
    expect(aibTabHref('/compacts/AIB-C01', '?scn=decoupling')).toBe('/aib?scn=decoupling');
    expect(aibTabHref('/compacts/AIB-C01', '?scn=foo')).toBe('/aib');
    rememberLocation('/compacts/AIB-C02', '?scn=decoupling');
    expect(aibTabHref('/sector', '')).toBe('/compacts/AIB-C02?scn=decoupling');
    resetLastLocations();
  });

  it('the Country profile keeps scn as a host key (parsed, written last) and has the Geopolitical tab', () => {
    expect(COUNTRY_KEY_ORDER).toContain('scn');
    expect(parseCountrySearch('?cell=oil-extraction&scn=decoupling').state.scn).toBe('decoupling');
    // the default, as an older link writes it: read as nothing, without a notice
    const old = parseCountrySearch('?scn=managed');
    expect(old.state.scn).toBeUndefined();
    expect(old.problems).toEqual([]);
    const p = parseCountrySearch('?scn=baseline');
    expect(p.state.scn).toBeUndefined();
    expect(p.problems).toEqual(["scn=baseline ('decoupling' or nothing)"]);
    expect(serializeCountrySearch({ cell: 'oil-extraction', ptab: 'investment', scn: 'decoupling' })).toBe('?cell=oil-extraction&ptab=investment&scn=decoupling');
    expect(serializeCountrySearch({ cell: 'oil-extraction' })).toBe('?cell=oil-extraction');
    expect(parseCountrySearch('?ptab=geopolitical').state.ptab).toBe('geopolitical');
  });
});

describe('derive/scenarios', () => {
  const d = doc();
  const nodeOf = (id: string) => d.nodes.find((n) => n.id === id) ?? null;

  it('labels, words, shocks and colours: the bundle first, the defaults after; one colour each, none a class colour', () => {
    expect(scenarioList(d.meta).map((s) => s.label)).toEqual(['Baseline', 'Managed Interdependence', 'Decoupling']);
    expect(scenarioInfo(d.meta, 'decoupling').words).toMatch(/^Zembla stops supplying/);
    expect(scenarioInfo(d.meta, 'managed').shockId).toBe('diversify_benchmark');
    // an older bundle: the defaults, the shocked economy named from the headline shock's label
    const older = stripScenarios(doc()).meta;
    expect(scenarioInfo(older, 'managed').words).toBe(SCENARIO_DEFAULTS.managed.words);
    expect(scenarioInfo(older, 'decoupling').words).toMatch(/^Zembla stops supplying the allied market \(it keeps buying\)/);
    expect(scenarioInfo(older, 'decoupling').shockId).toBe('zzz_100');
    expect(scenarioInfo(null, 'decoupling').words).toBe(SCENARIO_DEFAULTS.decoupling.words);
    expect(scenarioLabel('managed')).toBe('Managed Interdependence');
    expect(scenarioShockLabel(d.meta, 'baseline')).toBe('No shock: trade as it is today (2024)');
    expect(scenarioShockLabel(d.meta, 'decoupling')).toBe('Zembla supply removed');
    expect(scenarioShockLabel(d.meta, 'managed')).toBe('no supplier removed; caps above the benchmark');
    expect(scenarioCaveat(d.meta)).toMatch(/not forecasts; dollar values are 2024 trade values/);
    const colours = Object.values(SCENARIO_COLOR);
    expect(new Set(colours).size).toBe(3);
    const classColours = new Set(Object.values(CLASS_INFO).map((c) => c.color.toLowerCase()));
    expect(colours.some((c) => classColours.has(c.toLowerCase()))).toBe(false);
  });

  it('the toggle shows only with the scenarios; Decoupling otherwise', () => {
    expect(hasScenarios(d)).toBe(true);
    expect(hasBaseline(d)).toBe(true);
    const older = stripScenarios(doc());
    expect(hasScenarios(older)).toBe(false);
    expect(hasBaseline(older)).toBe(false);
    expect(effectiveScenario('managed', true)).toBe('managed');
    expect(effectiveScenario('managed', false)).toBe('decoupling');
  });

  it('a node under each scenario: Baseline full, Managed Interdependence where it ran (else unchanged), Decoupling the node', () => {
    const mag = nodeOf('ev-wind-mid-magnets')!;
    const [b, m, x] = nodeScenarios(mag);
    expect(b).toMatchObject({ id: 'baseline', ran: true, lostUsd: 0, static: 1, optimized: 1, residualUsd: 0, cls: null, split: { kept: 1, trade: 0, build: 0 } });
    expect(b.hhi).toBe(mag.baseline!.hhi);
    expect(b.largest).toBe('ZZZ');
    expect(m).toMatchObject({ id: 'managed', ran: true, cls: 'BUILD', static: 0.66, optimized: 0.78, residualUsd: 2112000000, hhi: 0.18, largest: 'BBB', newLinks: 3 });
    expect(m.lostUsd).toBe(mag.diversify!.lostUsd);
    expect(m.dividend).toBeCloseTo(0.07, 6);
    expect(m.split!.kept + m.split!.trade + m.split!.build).toBeCloseTo(1, 9);
    expect(x).toMatchObject({ id: 'decoupling', ran: true, cls: 'BUILD', lostUsd: mag.lostUsd, static: mag.static, optimized: mag.optimized, hhi: mag.hhi });
    expect(x.residualUsd).toBeCloseTo(mag.residual! * mag.demandUsd!, 0);
    // within the cap: unchanged, with its status
    const ree = nodeScenario(nodeOf('ev-wind-mid-ree-compounds')!, 'managed');
    expect(ree).toMatchObject({ ran: false, lostUsd: 0, static: 1, cls: null, status: 'within the cap in the allied market' });
    // never assessed (INSUFFICIENT DATA): unchanged under the scenario, no figures under Decoupling
    const tr = nodeOf('grid-mid-transformers')!;
    expect(nodeScenario(tr, 'managed')).toMatchObject({ ran: false, status: null });
    expect(nodeScenario(tr, 'decoupling')).toMatchObject({ ran: false, split: null });
    expect(nodeScenario(tr, 'baseline').hhi).toBeNull();
  });

  it('a compact under each scenario: compact.managed equals its nodes aggregated; Decoupling optimized is the global optimum', () => {
    for (const c of d.compacts) {
      const [b, m, x] = compactScenarios(c, nodeOf);
      expect(b).toMatchObject({ id: 'baseline', static: 1, optimized: 1, lostUsd: 0, classMix: null, source: 'export' });
      expect(b.suppliers).toEqual(c.baseline!.suppliers);
      expect(b.hhi).toBe(c.baseline!.hhi);
      expect(m.source).toBe('export');
      expect(m.nodesRun).toBe(1);
      // the export's figures and the nodes' agree (the fallback for a bundle without compact.managed)
      const noExport = structuredClone(c);
      delete noExport.managed;
      const agg = compactScenario(noExport, 'managed', nodeOf);
      expect(agg.source).toBe('nodes');
      for (const k of ['static', 'existing', 'optimized', 'residual'] as const) expect(agg[k]).toBeCloseTo(m[k]!, 3);
      expect(agg.lostUsd).toBeCloseTo(m.lostUsd!, -1);
      expect(agg.residualUsd).toBeCloseTo(m.residualUsd!, -1);
      expect(agg.classMix).toBe(m.classMix);
      expect(x.optimized).toBe(c.metrics.globalOptimum);
      expect(x.static).toBe(c.metrics.static);
      expect(x.classMix).toBe(c.classMix);
    }
    expect(parseClassMix('BUILD 67%; MANAGE SPECIALIZATION 33%')).toEqual([
      { cls: 'BUILD', share: 0.67 },
      { cls: 'MANAGE SPECIALIZATION', share: 0.33 },
    ]);
    expect(parseClassMix(null)).toEqual([]);
  });

  it('a set of nodes under each scenario: the same nodes weighed, the Managed Interdependence totals those of the Diversification page', () => {
    const all = scenarioTotalsAll(d.nodes);
    expect(all.map((t) => t.id)).toEqual(['baseline', 'managed', 'decoupling']);
    expect(new Set(all.map((t) => t.nodes)).size).toBe(1);
    expect(all[0].nodes).toBe(comparableNodes(d.nodes).length);
    expect(all[0]).toMatchObject({ static: 1, optimized: 1, lostUsd: 0, residualUsd: 0, affectedDemandUsd: 0, byClass: [] });
    const mi = scenarioTotals(d.nodes, 'managed');
    const dt = diversifyTotals(d.nodes);
    expect(mi.nodesRun).toBe(dt.diversified);
    expect(mi.residualUsd).toBeCloseTo(dt.gapUsd, -1);
    expect(mi.byClass.map((g) => g.cls)).toEqual(['COORDINATE', 'BUILD']);
    expect(lostByClassOf(d.nodes.map((n) => nodeScenario(n, 'decoupling'))).reduce((s, g) => s + g.lostUsd, 0)).toBeCloseTo(
      d.nodes.reduce((s, n) => s + (n.lostUsd ?? 0), 0),
      0,
    );
    const x = all[2];
    expect(x.lostUsd).toBe(comparableNodes(d.nodes).reduce((s, n) => s + (n.lostUsd ?? 0), 0));
  });

  it('the detail of a compact per scenario: Decoupling its own, Managed Interdependence from the file (pending without it), Baseline none', () => {
    const c = d.compacts[0];
    expect(compactDetail(c, 'decoupling')).toMatchObject({ status: 'ready', coalition: c.coalition, trade: c.trade });
    expect(compactDetail(c, 'managed')).toMatchObject({ status: 'pending', coalition: null, trade: [] });
    const md = normalizeScenarioDoc(managed());
    const det = compactDetail(c, 'managed', md);
    expect(det.status).toBe('ready');
    expect(det.coalition!.members).toEqual(['AAA', 'BBB', 'CCC', 'DDD']);
    expect(det.trade.every((t) => t.node === 'ev-wind-mid-magnets')).toBe(true);
    expect(compactDetail(c, 'baseline')).toMatchObject({ status: 'none', reason: BASELINE_NO_COALITION });
    const none = structuredClone(md);
    none.compacts[0] = { ...none.compacts[0], coalition: null, coalitionReason: 'no node of this compact is more concentrated than its benchmark', nodesRun: 0 };
    expect(compactDetail(c, 'managed', none)).toMatchObject({ status: 'ready', coalition: null, reason: 'no node of this compact is more concentrated than its benchmark' });
    none.compacts = [];
    expect(compactDetail(c, 'managed', none).status).toBe('none');
  });

  it('the network and the flows per scenario; today\'s suppliers from the file, else from the main document', () => {
    const net = d.network!;
    const head = scenarioNetwork(d, 'decoupling')!;
    expect(head.edges.every((e) => e.trade > 0 || e.investment > 0)).toBe(true);
    expect(head.edges.length).toBe(net.edges.filter((e) => e.trade > 0 || e.investment > 0).length);
    expect(scenarioNetwork(d, 'decoupling', null, { headlineOnly: false })).toBe(net);
    expect(headlineNetwork(net).countries.length).toBeLessThanOrEqual(net.countries.length);
    const md = managed();
    const bd = baseline();
    expect(scenarioNetwork(d, 'managed')).toBeNull();
    expect(scenarioNetwork(d, 'managed', md)!.variant.shock).toBe('diversify_benchmark');
    expect(scenarioNetwork(d, 'managed', bd)).toBeNull();
    expect(scenarioNetwork(d, 'baseline', bd)!.variant.shock).toBe('baseline');
    expect(scenarioInvestmentFlows(d, 'decoupling')).toBe(d.investmentFlows);
    expect(scenarioInvestmentFlows(d, 'managed', md)!.pairs).toHaveLength(3);
    expect(scenarioInvestmentFlows(d, 'baseline', bd)).toBeNull();
    const mag = nodeOf('ev-wind-mid-magnets')!;
    const fromDoc = nodeSuppliersToday(mag);
    expect(fromDoc).toHaveLength(5);
    expect(fromDoc[0]).toEqual({ iso3: 'ZZZ', share: 0.64, usd: 0.64 * mag.demandUsd! });
    const fromFile = nodeSuppliersToday(mag, bd);
    expect(fromFile).toHaveLength(8);
    expect(fromFile.slice(0, 5).map((s) => [s.iso3, s.share])).toEqual(fromDoc.map((s) => [s.iso3, s.share]));
    const c = d.compacts[0];
    expect(compactSuppliersToday(c).map((s) => s.iso3)).toEqual(c.baseline!.suppliers.map((s) => s[0]));
    expect(compactSuppliersToday(c, bd).slice(0, 5).map((s) => s.iso3)).toEqual(c.baseline!.suppliers.map((s) => s[0]));
    const name = scenarioNameOf((iso) => `main:${iso}`, md);
    expect(name('AAA')).toBe('Avalon');
    expect(name('QQQ')).toBe('main:QQQ');
  });
});
