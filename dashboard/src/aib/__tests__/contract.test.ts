// The Sectoral Compacts contract and store: the synthetic fixture passes both checks; missing, badly
// typed and collapsed fields are reported with their paths; a newer version asks for a reload; the
// tolerated shapes of the R export ([] for an empty named list, null numbers and NA strings) pass; the
// additive and methodology fields are optional (an older bundle, secondShock and all, validates) and
// checked when present (the diversification scenario of 2026-10-19 too: four statuses, nulls where it did
// not run); the store maps every failure to NO_DATA / NEWER / CONTRACT / NETWORK and can
// retry. The strategic layer (2026-10-20): the synthetic section validates, a bundle without it too, the R
// export's shapes are tolerated, a newer section asks for a reload, and broken fields are reported with the
// paths of the R mirror (aib_check_strategic()).

import { describe, expect, it } from 'vitest';
import {
  AIB_DIVERSIFY_SHOCK, AIB_DIVERSIFY_STATUSES, AIB_DOC_FILE_RE, AIB_RESERVED, AIB_STRATEGIC_ROLE_CODES, AIB_STRATEGIC_SCENARIO_IDS, AIB_STRATEGIC_VERSION, checkAibManifest, checkCompactsDoc,
  coalitionMembersOf, countryNamesOf, diversifyMetaOf, diversifyShockIdOf, indicesOf, inStrategicScope, isDictionary, needsNewerApp, nodeCoverageOf, strategicOf, variantsOf, variantWarningsOf,
  type AibDoc,
} from '../contract.ts';
import { AibLoadError, AibStore, httpAibSource, type AibSource } from '../data/store.ts';
import { fixtureSource, readDoc, readManifest, stripDiversify, stripMethodology, stripScenarios, stripStrategic, SYNTHETIC_DIR, toMasterTaxonomy } from './fixture.ts';

const doc = (): AibDoc => structuredClone(readDoc(SYNTHETIC_DIR));
type Loose = Record<string, unknown>;
const loose = (x: unknown) => x as Loose;

describe('checkAibManifest', () => {
  it('passes the synthetic manifest', () => {
    const m = readManifest(SYNTHETIC_DIR);
    expect(checkAibManifest(m)).toEqual([]);
    expect(AIB_DOC_FILE_RE.test(m.files.compacts)).toBe(true);
  });

  it('reports missing and badly typed fields', () => {
    const m = loose(structuredClone(readManifest(SYNTHETIC_DIR)));
    delete m.created;
    m.schema = 'osi-country';
    m.files = { compacts: 'compacts.json' };
    const p = checkAibManifest(m);
    expect(p).toContain('manifest.created: expected a string');
    expect(p).toContain("manifest.schema: expected 'osi-aib'");
    expect(p).toContain('manifest.files.compacts: not a compacts.<8 hex>.json file name');
    expect(checkAibManifest(null)).toEqual(['manifest: not an object']);
    expect(checkAibManifest([])).toEqual(['manifest: not an object']);
  });

  it('a newer version needs a newer app', () => {
    const m = { ...readManifest(SYNTHETIC_DIR), version: 3 };
    expect(checkAibManifest(m)).toEqual(['manifest: version 3 needs a newer app']);
  });
});

describe('checkCompactsDoc', () => {
  it('passes the synthetic document', () => {
    expect(checkCompactsDoc(readDoc(SYNTHETIC_DIR))).toEqual([]);
  });

  it('reports missing fields with their paths', () => {
    const d = doc();
    delete loose(d.meta).tradeYear;
    delete loose(d.compacts[0]).metrics;
    delete loose(d.nodes[2]).frontier;
    delete loose(d.compacts[1].coalition).reached99;
    const p = checkCompactsDoc(d);
    expect(p).toContain('meta.tradeYear: expected a number');
    expect(p).toContain('compacts[0].metrics: not an object');
    expect(p).toContain('nodes[2].frontier: expected an array');
    expect(p).toContain('compacts[1].coalition.reached99: expected true or false');
  });

  it('reports badly typed values, unknown enums and out-of-range shares', () => {
    const d = doc();
    loose(d.nodes[0]).exposure = '71%';
    loose(d.nodes[1]).largestShare = 1.4;
    loose(d.nodes[3]).cls = 'PANIC';
    loose(d.compacts[0]).archetype = 'Grand Compact';
    loose(d.compacts[0].trade[0]).relationship = 'old';
    loose(d.compacts[1].roles[0]).member = 'yes';
    loose(d.compacts[0].path[1]).step = 1.5;
    const p = checkCompactsDoc(d);
    expect(p).toContain('nodes[0].exposure: expected a number or null');
    expect(p).toContain('nodes[1].largestShare: 1.4 above 1');
    expect(p).toContain("nodes[3].cls: 'PANIC' is not one of meta.classes");
    expect(p.some((x) => x.startsWith("compacts[0].archetype: 'Grand Compact' is not one of"))).toBe(true);
    expect(p.some((x) => x.startsWith("compacts[0].trade[0].relationship: 'old' is not one of"))).toBe(true);
    expect(p).toContain('compacts[1].roles[0].member: expected true or false');
    expect(p).toContain('compacts[0].path[1].step: 1.5 is not an integer');
  });

  it('reports a one-item array collapsed to a scalar (the jsonlite auto_unbox trap)', () => {
    const d = doc();
    loose(d.compacts[1]).anchors = 'AAA';
    loose(d.nodes[0]).techs = 'Wind';
    loose(d.meta).targets = 0.9;
    const p = checkCompactsDoc(d);
    expect(p).toContain('compacts[1].anchors: expected an array (a one-item array collapsed to a string?)');
    expect(p).toContain('nodes[0].techs: expected an array (a one-item array collapsed to a string?)');
    expect(p).toContain('meta.targets: expected an array');
  });

  it('checks references between compacts and nodes, and unique ids', () => {
    const d = doc();
    d.compacts[0].nodes.push('no-such-node');
    d.nodes[4].compact = 'AIB-C99';
    d.nodes.push(structuredClone(d.nodes[0]));
    const p = checkCompactsDoc(d);
    expect(p).toContain("compacts[0].nodes: 'no-such-node' is not a node of the document");
    expect(p).toContain("nodes[4].compact: 'AIB-C99' is not a compact of the document");
    expect(p).toContain(`nodes[${d.nodes.length - 1}].id: duplicate '${d.nodes[0].id}'`);
  });

  it('tolerates the shapes the R export writes: [] for empty named lists, null numbers, NA strings', () => {
    const d = doc();
    loose(d.compacts[0].path[0]).nodeCoverage = [];
    loose(d).countries = [];
    d.nodes[0].quality = null;
    d.nodes[0].baseLargest = null;
    d.nodes[0].hhi = null;
    d.compacts[0].metrics.investment = null;
    d.compacts[1].coalition.maxCoverage = null;
    expect(checkCompactsDoc(d)).toEqual([]);
    expect(nodeCoverageOf(d.compacts[0].path[0])).toEqual({});
    expect(countryNamesOf(d)).toEqual({});
    // ... but a non-empty array where an object belongs is a problem
    loose(d.compacts[0].path[1]).nodeCoverage = [0.5];
    expect(checkCompactsDoc(d)).toContain('compacts[0].path[1].nodeCoverage: expected an object of node coverages');
  });

  it('the additive fields (2026-10-15) are optional: a bundle written before them still validates', () => {
    const d = doc();
    for (const c of d.compacts) {
      for (const k of ['globalOptimum', 'anchorFragmented', 'anchorCompact', 'compactGain']) delete loose(c.metrics)[k];
      delete c.classMix;
      delete c.secondaryClass;
    }
    for (const n of d.nodes) {
      delete n.tightestCostless;
      delete n.minRequiredDiversification;
      for (const f of n.frontier) delete f.specialization;
    }
    expect(checkCompactsDoc(d)).toEqual([]);
  });

  it('the methodology fields (2026-10-16) are optional: an older bundle, with secondShock in its frontier, still validates', () => {
    const d = stripMethodology(doc());
    expect(checkCompactsDoc(d)).toEqual([]);
    // the older bundle wrote the instantaneous N-1 as secondShock: still accepted (and still range-checked)
    expect(d.nodes[0].frontier[0].secondShock).toBeCloseTo(0.5325, 6);
    d.nodes[0].frontier[0].secondShock = 1.5;
    expect(checkCompactsDoc(d)).toEqual(['nodes[0].frontier[0].secondShock: 1.5 above 1']);
    const m = loose(structuredClone(readManifest(SYNTHETIC_DIR)));
    for (const k of ['demandScope', 'candidateUniverse', 'shockSemantics', 'supplyConstraint']) delete m[k];
    expect(checkAibManifest(m)).toEqual([]);
    // the synthetic document carries every one of them, and no secondShock
    const full = doc();
    expect(full.meta).toMatchObject({ demandScope: 'coalition', candidateUniverse: 'global', shockSemantics: 'hold', supplyConstraint: 'production_constrained', strategicScope: 'all' });
    expect(full.nodes.flatMap((n) => n.frontier).some((f) => 'secondShock' in f)).toBe(false);
    expect(full.nodes.every((n) => Array.isArray(n.policyNeeds) && typeof n.inStrategicScope === 'boolean')).toBe(true);
  });

  it('the methodology fields are checked when present; nulls and an empty variants list ([]) are tolerated', () => {
    const d = doc();
    const n0 = loose(d.nodes[0]);
    n0.policyNeeds = ['BUILD', 'PANIC'];
    n0.warning = 7;
    n0.physicalData = 'partial';
    n0.inStrategicScope = 'yes';
    n0.coordinateSensitiveToGrowth = 1;
    n0.n1Reoptimized = 1.3;
    n0.diversion = -0.1;
    loose(d.nodes[1]).policyNeeds = 'BUILD';
    loose(d.nodes[1]).variants = ['BUILD'];
    loose(d.nodes[2].variants!).release = 'PANIC';
    loose(d.nodes[3]).growth = { label: '2x' };
    loose(d.nodes[4].growth![0]).cls = 'PANIC';
    loose(d.nodes[4].growth![1]).existing = 1.2;
    loose(d.nodes[5].frontier[0]).n1Removed = 5;
    loose(d.nodes[5].frontier[1]).instantaneousN1 = -0.1;
    loose(d.compacts[0].coalition).greedy = 'AAA';
    loose(d.compacts[0].coalition).method = 3;
    loose(d.compacts[0]).physicalWarnings = 'Wind Towers';
    loose(d.compacts[0].roles[0]).tier = 5;
    loose(d.compacts[0].roles[0]).critical = 'no';
    loose(d.compacts[0].roles[1]).substitutes = 'GGG';
    loose(d.meta).demandScope = 'planet';
    loose(d.meta).nDemandScope = 5.5;
    loose(d.meta).semantics = ['hold', 'grab'];
    loose(d.meta).strategicScope = 5;
    const p = checkCompactsDoc(d);
    for (const want of [
      "nodes[0].policyNeeds[1]: 'PANIC' is not one of COORDINATE, BUILD, MANAGE SPECIALIZATION",
      'nodes[0].warning: expected a string or null',
      "nodes[0].physicalData: 'partial' is not one of complete, incomplete, none",
      'nodes[0].inStrategicScope: expected true or false',
      'nodes[0].coordinateSensitiveToGrowth: expected true or false',
      'nodes[0].n1Reoptimized: 1.3 above 1',
      'nodes[0].diversion: -0.1 below 0',
      'nodes[1].policyNeeds: expected an array (a one-item array collapsed to a string?)',
      'nodes[1].variants: expected an object of variant -> class',
      "nodes[2].variants.release: 'PANIC' is not one of meta.classes",
      'nodes[3].growth: expected an array (a one-item array collapsed to an object?)',
      "nodes[4].growth[0].cls: 'PANIC' is not one of meta.classes",
      'nodes[4].growth[1].existing: 1.2 above 1',
      'nodes[5].frontier[0].n1Removed: expected a string or null',
      'nodes[5].frontier[1].instantaneousN1: -0.1 below 0',
      'compacts[0].coalition.greedy: expected an array (a one-item array collapsed to a string?)',
      'compacts[0].coalition.method: expected a string or null',
      'compacts[0].physicalWarnings: expected an array (a one-item array collapsed to a string?)',
      'compacts[0].roles[0].tier: expected a string or null',
      'compacts[0].roles[0].critical: expected true or false',
      'compacts[0].roles[1].substitutes: expected an array (a one-item array collapsed to a string?)',
      "meta.demandScope: 'planet' is not one of coalition, configured, global",
      'meta.nDemandScope: 5.5 is not an integer',
      "meta.semantics[1]: 'grab' is not one of hold, release",
      'meta.strategicScope: expected a string or null',
    ]) {
      expect(p).toContain(want);
    }
    // nulls where R had NA, and [] for an empty named list, pass
    const ok = doc();
    loose(ok.nodes[0]).variants = [];
    ok.nodes[0].warning = null;
    ok.nodes[0].physicalData = null;
    ok.nodes[0].n1Reoptimized = null;
    ok.nodes[1].variants = { ...ok.nodes[1].variants, release: null };
    ok.nodes[1].growth = [{ label: '2x', existing: null, dividend: null, cls: null }];
    ok.compacts[0].roles[0].tier = null;
    ok.compacts[0].coalition.method = null;
    ok.meta.nDemandScope = null;
    ok.meta.shockSemantics = null;
    // any configured strategic scope is accepted as written
    ok.meta.strategicScope = 'critical_minerals';
    expect(checkCompactsDoc(ok)).toEqual([]);
    expect(variantsOf(ok.nodes[0])).toEqual({});
    expect(inStrategicScope({ ...ok.nodes[0], inStrategicScope: undefined })).toBe(true);
    // strategic scope "all" (the dictionary): every node is in it; the master form leaves the electrolysers (Green Hydrogen) outside
    expect(ok.nodes.every(inStrategicScope)).toBe(true);
    expect(inStrategicScope(toMasterTaxonomy(doc()).nodes.find((n) => n.id === 'h2-down-electrolysers')!)).toBe(false);
    const m = { ...readManifest(SYNTHETIC_DIR), demandScope: 3 };
    expect(checkAibManifest(m)).toEqual(['manifest.demandScope: expected a string or null']);
  });

  it('the final-model fields: per-variant and per-growth warnings, single substitutes and substitutesNeeded, leave-one-out, the marginal tier and the coalition flags', () => {
    // the synthetic document carries every one of them
    const full = doc();
    expect(full.nodes.every((n) => n.variantWarnings && !Array.isArray(n.variantWarnings) && Object.keys(n.variantWarnings).join() === Object.keys(n.variants ?? {}).join())).toBe(true);
    expect(full.nodes.some((n) => !n.warning && Object.values(n.variantWarnings ?? {}).some(Boolean))).toBe(true);
    expect(full.nodes.flatMap((n) => n.growth ?? []).every((g) => typeof g.warning === 'boolean')).toBe(true);
    expect(full.compacts.every((c) => typeof c.coalition.associatesReach99 === 'boolean' && typeof c.coalition.robustTieBreakSolved === 'boolean' && Array.isArray(c.coalition.criticalExternalMarginal))).toBe(true);
    expect(full.compacts.flatMap((c) => c.roles).every((r) => typeof r.looCritical === 'boolean' && r.substitutesNeeded !== undefined)).toBe(true);
    expect(full.compacts.flatMap((c) => c.roles).some((r) => r.tier === 'critical external dependency (marginal)')).toBe(true);
    // checked when present
    const d = doc();
    loose(d.nodes[0].variantWarnings!).trade_only = 'yes';
    loose(d.nodes[1]).variantWarnings = ['trade_only'];
    loose(d.nodes[2].growth![0]).warning = 1;
    loose(d.compacts[0].coalition).criticalExternalMarginal = 'JJJ';
    loose(d.compacts[0].coalition).associatesReach99 = 'yes';
    loose(d.compacts[0].coalition).robustTieBreakSolved = 0;
    loose(d.compacts[0].roles[0]).looCritical = 'no';
    loose(d.compacts[0].roles[1]).substitutesNeeded = 1.5;
    loose(d.compacts[0].roles[2]).substitutesNeeded = -1;
    const p = checkCompactsDoc(d);
    for (const want of [
      'nodes[0].variantWarnings.trade_only: expected true or false',
      'nodes[1].variantWarnings: expected an object of variant -> true or false',
      'nodes[2].growth[0].warning: expected true or false',
      'compacts[0].coalition.criticalExternalMarginal: expected an array (a one-item array collapsed to a string?)',
      'compacts[0].coalition.associatesReach99: expected true or false',
      'compacts[0].coalition.robustTieBreakSolved: expected true or false',
      'compacts[0].roles[0].looCritical: expected true or false',
      'compacts[0].roles[1].substitutesNeeded: 1.5 is not an integer',
      'compacts[0].roles[2].substitutesNeeded: -1 below 0',
    ]) {
      expect(p).toContain(want);
    }
    expect(p).toHaveLength(9);
    // [] for an empty named list and null where nothing restores the target pass; variantWarningsOf reads both
    const ok = doc();
    loose(ok.nodes[0]).variantWarnings = [];
    ok.compacts[0].roles[2].substitutesNeeded = null;
    expect(checkCompactsDoc(ok)).toEqual([]);
    expect(variantWarningsOf(ok.nodes[0])).toEqual({});
    expect(variantWarningsOf(ok.nodes[1])).toEqual(ok.nodes[1].variantWarnings);
    delete ok.nodes[1].variantWarnings;
    expect(variantWarningsOf(ok.nodes[1])).toBeNull();
    // the widest greedy set of an unreached target is in members; an older export's [] falls back to the member roles
    const c03 = ok.compacts[2];
    expect(c03.coalition.method).toMatch(/^greedy_sufficient_coalition/);
    expect(coalitionMembersOf(c03)).toEqual(['AAA', 'EEE', 'FFF']);
    expect(coalitionMembersOf({ ...c03, coalition: { ...c03.coalition, members: [] } })).toEqual(['AAA', 'EEE', 'FFF']);
  });

  it('the additive fields are checked when present; the new compact type is accepted', () => {
    const d = doc();
    expect(d.compacts.some((c) => c.type === 'compact unnecessary: existing relationships do nearly as well')).toBe(true);
    loose(d.compacts[0].metrics).globalOptimum = '81%';
    d.compacts[0].metrics.anchorCompact = 1.2;
    d.compacts[0].metrics.compactGain = null;
    loose(d.compacts[1]).classMix = 7;
    loose(d.compacts[1]).secondaryClass = ['BUILD'];
    loose(d.nodes[0]).tightestCostless = 'tight';
    d.nodes[1].minRequiredDiversification = null;
    loose(d.nodes[2].frontier[0]).secondShock = -0.2;
    loose(d.nodes[2].frontier[1]).specialization = 'high';
    loose(d.compacts[2]).type = 'compact unnecessary';
    const p = checkCompactsDoc(d);
    expect(p).toContain('compacts[0].metrics.globalOptimum: expected a number or null');
    expect(p).toContain('compacts[0].metrics.anchorCompact: 1.2 above 1');
    expect(p).toContain('compacts[1].classMix: expected a string or null');
    expect(p).toContain('compacts[1].secondaryClass: expected a string or null');
    expect(p).toContain('nodes[0].tightestCostless: expected a number or null');
    expect(p).toContain('nodes[2].frontier[0].secondShock: -0.2 below 0');
    expect(p).toContain('nodes[2].frontier[1].specialization: expected a number or null');
    expect(p.some((x) => x.startsWith("compacts[2].type: 'compact unnecessary' is not one of"))).toBe(true);
    // null where a value is missing is fine
    expect(p.some((x) => /compactGain|minRequiredDiversification/.test(x))).toBe(false);
  });

  it('the network and investment flows (2026-10-17) are optional and checked when present, with the paths of the R mirror (R/dashboard/aib/aib_check.R)', () => {
    const old = stripMethodology(doc());
    expect(old.network).toBeUndefined();
    expect(checkCompactsDoc(old)).toEqual([]);
    const d = doc();
    expect(d.network?.countries.length).toBeGreaterThan(0);
    expect(d.investmentFlows?.pairs.length).toBeGreaterThan(0);
    const n = loose(d.network);
    const countries = n.countries as Loose[];
    const edges = n.edges as Loose[];
    edges[0].nodes = 'bat-mid-anode';
    edges[1].from = edges[1].to;
    edges[2].stages = ['Sidestream'];
    countries[1].iso3 = countries[0].iso3;
    loose(countries[2].supplier).headline = -0.1;
    countries[3].roles = ['broker'];
    const f = loose(d.investmentFlows);
    const pairs = f.pairs as Loose[];
    pairs[0].host = pairs[0].investor;
    pairs[1].rank = 7;
    pairs[2].node = 'wafers';
    pairs[3].usd = null;
    (f.hosts as Loose[])[0].capabilityTier = 1.5;
    loose(f.variant).maxRank = 5;
    const p = checkCompactsDoc(d);
    expect(p).toContain('network.edges[0].nodes: expected an array (a one-item array collapsed to a string?)');
    expect(p).toContain('network.edges[1]: supplier and buyer are the same economy');
    expect(p).toContain("network.edges[2].stages: 'Sidestream' is not one of Upstream, Midstream, Downstream");
    expect(p).toContain(`network.countries[1].iso3: duplicate '${countries[0].iso3}'`);
    expect(p).toContain('network.countries[2].supplier.headline: -0.1 below 0');
    expect(p).toContain("network.countries[3].roles: 'broker' is not one of supplier, buyer, investment_host, investor");
    expect(p).toContain('investmentFlows.pairs[0]: investor and host are the same economy');
    expect(p).toContain('investmentFlows.pairs[1].rank: 7 above 5');
    expect(p).toContain("investmentFlows.pairs[2].node: 'wafers' is not a node of the document");
    expect(p).toContain('investmentFlows.pairs[3].usd: expected a number');
    expect(p).toContain('investmentFlows.hosts[0].capabilityTier: 1.5 is not an integer');
    expect(checkCompactsDoc({ ...doc(), network: [] })).toContain('network: not an object');
  });

  it('the taxonomy fields (2026-10-19) are optional, checked when present and cross-checked against meta.indices; a master bundle without them validates', () => {
    const full = doc();
    expect(isDictionary(full.meta)).toBe(true);
    expect(full.meta).toMatchObject({ taxonomy: 'dictionary', dictionaryScope: 'core' });
    expect(indicesOf(full.meta).map((ix) => ix.code)).toEqual(['A', 'B', 'C', 'D', 'E', 'F']);
    expect(full.nodes.every((n) => typeof n.indexCode === 'string')).toBe(true);
    expect(full.compacts.find((c) => c.id === 'AIB-C01')!.indices).toEqual(['A', 'B']);
    // sector names with "/", labels with commas, a Downstream node and a node with no OSI cell
    expect(full.nodes.some((n) => n.techs.some((t) => t.includes('/')))).toBe(true);
    expect(full.nodes.some((n) => n.label.includes(','))).toBe(true);
    expect(full.nodes.some((n) => n.stage === 'Downstream')).toBe(true);
    expect(full.nodes.some((n) => !n.yfCells.length)).toBe(true);
    // the master form (an older bundle): no taxonomy fields, OSI technology names; still valid
    const master = toMasterTaxonomy(doc());
    expect(isDictionary(master.meta)).toBe(false);
    expect(indicesOf(master.meta)).toEqual([]);
    expect(master.nodes.every((n) => n.indexCode === undefined) && master.compacts.every((c) => c.indices === undefined)).toBe(true);
    expect(master.nodes.find((n) => n.id === 'ev-wind-mid-magnets')!.techs).toEqual(['Electric Vehicles', 'Wind']);
    expect(checkCompactsDoc(master)).toEqual([]);
    // null where R had NA (a master export writes them), an unknown taxonomy word read as master
    const na = doc();
    na.meta.dictionaryScope = null;
    na.nodes[0].indexCode = null;
    na.meta.taxonomy = 'something-new';
    expect(checkCompactsDoc(na)).toEqual([]);
    expect(isDictionary(na.meta)).toBe(false);
    // checked when present
    const d = doc();
    loose(d.meta).taxonomy = 3;
    loose(d.meta.indices![1]).code = 'A';
    loose(d.meta.indices![2]).code = 'grid';
    loose(d.meta.indices![3]).sectors = 'Solar';
    loose(d.nodes[0]).indexCode = 'Z';
    loose(d.nodes[1]).indexCode = 2;
    loose(d.compacts[0]).indices = 'A';
    loose(d.compacts[1]).indices = ['A', 'Q'];
    const p = checkCompactsDoc(d);
    for (const want of [
      'meta.taxonomy: expected a string or null',
      "meta.indices[1].code: duplicate 'A'",
      "meta.indices[2].code: 'grid' is not one capital letter",
      'meta.indices[3].sectors: expected an array (a one-item array collapsed to a string?)',
      "nodes[0].indexCode: 'Z' is not an index of meta.indices",
      'nodes[1].indexCode: expected a string or null',
      'compacts[0].indices: expected an array (a one-item array collapsed to a string?)',
      "compacts[1].indices: 'Q' is not an index of meta.indices",
    ]) {
      expect(p).toContain(want);
    }
    expect(checkCompactsDoc({ ...doc(), meta: { ...doc().meta, indices: { A: 'x' } } })).toContain('meta.indices: expected an array (a one-item array collapsed to an object?)');
  });

  it('a newer document version needs a newer app; a non-object is one problem', () => {
    expect(checkCompactsDoc({ ...doc(), version: 3 })).toContain('document: version 3 needs a newer app');
    expect(checkCompactsDoc('x')).toEqual(['document: not an object']);
  });

  it('caps the problem list', () => {
    const d = doc();
    for (let i = 0; i < 300; i++) d.nodes.push({ ...structuredClone(d.nodes[0]), id: `n${i}`, exposure: 'x' as unknown as number });
    expect(checkCompactsDoc(d).length).toBe(200);
  });
});

describe('AibStore', () => {
  it('loads the manifest, then the document it names; both are cached', async () => {
    const src = fixtureSource(SYNTHETIC_DIR);
    const s = new AibStore(src);
    const m = await s.manifest();
    const d = await s.doc(m);
    expect(d.compacts.map((c) => c.id)).toEqual(['AIB-C01', 'AIB-C02', 'AIB-C03']);
    await s.manifest();
    await s.doc(m);
    expect(src.calls).toEqual(['manifest.json', m.files.compacts]);
    expect(s.peek('manifest')).toBe(m);
    expect(s.peek(`doc:${m.files.compacts}`)).toBe(d);
  });

  it('maps a missing manifest to NO_DATA, a newer one to NEWER and a broken document to CONTRACT; failures retry', async () => {
    let n = 0;
    const missing: AibSource = {
      manifest: async () => {
        n += 1;
        throw new AibLoadError('NO_DATA', 'data/aib/manifest.json does not exist (HTTP 404).');
      },
      json: async () => ({}),
    };
    const s = new AibStore(missing);
    await expect(s.manifest()).rejects.toMatchObject({ code: 'NO_DATA' });
    await expect(s.manifest()).rejects.toMatchObject({ code: 'NO_DATA' });
    expect(n).toBe(2);

    const newer = new AibStore({ manifest: async () => ({ ...readManifest(SYNTHETIC_DIR), version: 3 }), json: async () => ({}) });
    await expect(newer.manifest()).rejects.toMatchObject({ code: 'NEWER' });

    const broken = new AibStore(fixtureSource(SYNTHETIC_DIR, (d) => delete loose(d).nodes));
    const m = await broken.manifest();
    await expect(broken.doc(m)).rejects.toMatchObject({ code: 'CONTRACT' });
    const e = (await broken.doc(m).then(
      () => null,
      (x: unknown) => x,
    )) as AibLoadError;
    expect(e.message).toMatch(/nodes: expected an array/);
  });

  it('httpAibSource: a 404 or an HTML fallback for the manifest is NO_DATA; a failed document is NETWORK', async () => {
    const res = (status: number, body: string) => new Response(body, { status });
    const seen: [string, RequestInit | undefined][] = [];
    const src404 = httpAibSource('/data/aib/', { fetch: async (u, i) => (seen.push([u, i]), res(404, 'not found')) });
    await expect(src404.manifest()).rejects.toMatchObject({ code: 'NO_DATA' });
    expect(seen[0]).toEqual(['/data/aib/manifest.json', { cache: 'no-cache' }]);
    await expect(src404.json('compacts.0123abcd.json')).rejects.toMatchObject({ code: 'NETWORK' });
    const html = httpAibSource('/x/data/aib/', { fetch: async () => res(200, '<!doctype html><html></html>') });
    await expect(html.manifest()).rejects.toMatchObject({ code: 'NO_DATA' });
    await expect(html.json('compacts.0123abcd.json')).rejects.toMatchObject({ code: 'CONTRACT' });
    const down = httpAibSource('/data/aib/', { fetch: async () => res(503, '') });
    await expect(down.manifest()).rejects.toMatchObject({ code: 'NETWORK' });
    const ok = httpAibSource('/data/aib/', { fetch: async () => res(200, JSON.stringify(readManifest(SYNTHETIC_DIR))) });
    expect(checkAibManifest(await ok.manifest())).toEqual([]);
  });
});

describe('the diversification scenario (2026-10-19)', () => {
  const ALL = ['diversified to the benchmark', 'within the cap in the allied market', 'already as diversified as its benchmark', 'no benchmark'];

  it('the synthetic document carries meta.diversify and node.diversify in every status; it validates, and so does an older bundle without them', () => {
    const d = doc();
    expect(AIB_DIVERSIFY_STATUSES).toEqual(ALL);
    expect(d.meta.diversify).toEqual({ shockId: 'diversify_benchmark', label: "Diversified to its sector's concentration", benchmark: 'hs4' });
    expect(d.meta.shocks.map((s) => s.id)).toContain('diversify_benchmark');
    expect(new Set(d.nodes.flatMap((n) => (n.diversify ? [n.diversify.status] : [])))).toEqual(new Set(ALL));
    // one node the scenario did not assess
    expect(d.nodes.filter((n) => n.diversify === undefined).map((n) => n.id)).toEqual(['grid-mid-transformers']);
    expect(checkCompactsDoc(d)).toEqual([]);
    expect(diversifyMetaOf(d.meta)).toEqual(d.meta.diversify);
    expect(diversifyShockIdOf(d.meta)).toBe('diversify_benchmark');
    // older bundles: without the scenario, and before the methodology fields
    const older = stripDiversify(doc());
    expect(older.meta.diversify).toBeUndefined();
    expect(older.nodes.some((n) => n.diversify !== undefined || 'diversify_benchmark' in (n.variants ?? {}))).toBe(false);
    expect(older.meta.shocks.map((s) => s.id)).not.toContain('diversify_benchmark');
    expect(checkCompactsDoc(older)).toEqual([]);
    expect(diversifyMetaOf(older.meta)).toBeNull();
    expect(diversifyShockIdOf(older.meta)).toBe(AIB_DIVERSIFY_SHOCK);
    expect(checkCompactsDoc(stripMethodology(doc()))).toEqual([]);
  });

  it('every status validates, with nulls where the scenario did not run; the variant class is null there', () => {
    const d = doc();
    const byStatus = (s: string) => d.nodes.find((n) => n.diversify?.status === s)!;
    for (const s of ALL) {
      const n = byStatus(s);
      const ran = s === 'diversified to the benchmark';
      for (const k of ['static', 'existing', 'optimized', 'residual', 'residualUsd', 'cls'] as const) expect(n.diversify![k] === null, `${s} ${k}`).toBe(!ran);
      expect(variantsOf(n).diversify_benchmark ?? null).toBe(n.diversify!.cls);
      expect(n.diversify!.cap === null).toBe(s === 'already as diversified as its benchmark' || s === 'no benchmark');
    }
    // a no-benchmark node: every nullable field null, the arrays empty
    const nb = byStatus('no benchmark').diversify!;
    expect([nb.benchmarkHhi, nb.basis, nb.cap, nb.excess, nb.partners]).toEqual([null, null, null, null, null]);
    expect([nb.capped, nb.hosts]).toEqual([[], []]);
    // one diversified node with two capped suppliers and one where trade replaces everything
    expect(d.nodes.find((n) => n.id === 'ev-wind-mid-magnets')!.diversify!.capped).toEqual(['ZZZ', 'AAA']);
    expect(d.nodes.find((n) => n.id === 'bat-mid-anode')!.diversify!.residual).toBe(0);
    // shares at both ends and every field null at once pass
    const ok = doc();
    Object.assign(ok.nodes[1].diversify!, { currentHhi: 1, benchmarkHhi: 0, cap: 0, excess: 1, static: 0, existing: 0, optimized: 1, residual: 0, residualUsd: 0 });
    Object.assign(ok.nodes[0].diversify!, { currentHhi: null, benchmarkHhi: null, basis: null, cap: null, excess: null, partners: null, cls: null });
    loose(ok.meta.diversify!).label = null;
    loose(ok.meta.diversify!).benchmark = 'sector';
    loose(ok.meta.diversify!).minExcess = 0.005;
    expect(checkCompactsDoc(ok)).toEqual([]);
    // the tolerance is a share of HHI: outside 0-1 it is reported
    const bad = doc();
    loose(bad.meta.diversify!).minExcess = -0.1;
    expect(checkCompactsDoc(bad).some((x) => x.startsWith('meta.diversify.minExcess'))).toBe(true);
  });

  it('bad values are reported with their paths', () => {
    const d = doc();
    const at = (id: string) => d.nodes.findIndex((n) => n.id === id);
    const mag = at('ev-wind-mid-magnets');
    const ree = at('ev-wind-mid-ree-compounds');
    const sol = at('solar-down-modules');
    const h2 = at('h2-down-electrolysers');
    const x = (i: number) => loose(d.nodes[i].diversify);
    x(mag).status = 'diversified';
    x(mag).cap = 1.2;
    x(mag).capped = 'ZZZ';
    x(mag).cls = 'REBUILD';
    x(mag).residualUsd = -5;
    x(ree).excess = '0';
    x(ree).basis = 7;
    delete x(sol).hosts;
    x(sol).static = -0.1;
    loose(d.nodes[h2]).diversify = [];
    delete loose(d.meta.diversify).shockId;
    loose(d.meta.diversify).benchmark = 4;
    const p = checkCompactsDoc(d);
    for (const want of [
      `nodes[${mag}].diversify.status: 'diversified' is not one of ${ALL.join(', ')}`,
      `nodes[${mag}].diversify.cap: 1.2 above 1`,
      `nodes[${mag}].diversify.capped: expected an array (a one-item array collapsed to a string?)`,
      `nodes[${mag}].diversify.cls: 'REBUILD' is not one of meta.classes`,
      `nodes[${mag}].diversify.residualUsd: -5 below 0`,
      `nodes[${ree}].diversify.excess: expected a number or null`,
      `nodes[${ree}].diversify.basis: expected a string or null`,
      `nodes[${sol}].diversify.hosts: expected an array`,
      `nodes[${sol}].diversify.static: -0.1 below 0`,
      `nodes[${h2}].diversify: not an object`,
      'meta.diversify.shockId: expected a string',
      'meta.diversify.benchmark: expected a string or null',
    ]) {
      expect(p).toContain(want);
    }
    expect(p).toHaveLength(12);
    const m = doc();
    loose(m.meta).diversify = 'hs4';
    expect(checkCompactsDoc(m)).toEqual(['meta.diversify: not an object']);
    expect(diversifyMetaOf(m.meta)).toBeNull();
  });
});

describe('the strategic layer (2026-10-20)', () => {
  const strat = () => doc().strategic!;

  it('the synthetic document carries it and validates; a bundle without it validates; strategicOf reads it', () => {
    const d = doc();
    const s = strategicOf(d)!;
    expect(s).not.toBeNull();
    expect(s.version).toBe(AIB_STRATEGIC_VERSION);
    expect(s.defaultScenario).toBe('managed');
    expect(s.scenarios.map((x) => x.id)).toEqual([...AIB_STRATEGIC_SCENARIO_IDS]);
    expect(s.roleDefs.map((r) => r.code)).toEqual([...AIB_STRATEGIC_ROLE_CODES]);
    expect(checkCompactsDoc(d)).toEqual([]);
    // every node of the document on exactly one spine; every compact candidate a workstream
    const spine = s.compacts.flatMap((c) => c.steps.flatMap((st) => st.nodes));
    expect(new Set(spine).size).toBe(spine.length);
    expect([...spine].sort()).toEqual(d.nodes.map((n) => n.id).sort());
    expect(s.workstreams.map((w) => w.id)).toEqual(d.compacts.map((c) => c.id));
    // older bundles
    expect(strategicOf(stripStrategic(doc()))).toBeNull();
    expect(checkCompactsDoc(stripStrategic(doc()))).toEqual([]);
    expect(checkCompactsDoc(stripScenarios(doc()))).toEqual([]);
    expect(stripScenarios(doc()).strategic).toBeUndefined();
    expect(strategicOf(null)).toBeNull();
  });

  it('the reserved path segments: about, workstreams and s are never compact ids', () => {
    expect([...AIB_RESERVED]).toEqual(['about', 'workstreams', 's']);
    const d = stripStrategic(doc());
    const id = d.compacts[0].id;
    d.compacts[0].id = 'Workstreams';
    for (const n of d.nodes) if (n.compact === id) n.compact = 'Workstreams';
    expect(checkCompactsDoc(d)).toEqual(["compacts[0].id: 'Workstreams' is a reserved path segment"]);
  });

  it('the shapes the R export writes are tolerated: null partners without coalitions, [] metrics, null slots, no framing, gaps in the step orders', () => {
    const d = doc();
    const s = d.strategic!;
    expect(s.compacts[0].scenarios.baseline!.criticalPartners).toBeNull();
    expect(s.compacts[0].scenarios.partial!.coalition).toBeNull();
    expect(s.compacts[0].steps.map((x) => x.order)).toEqual([2, 4]);
    loose(s.countries.find((c) => c.iso3 === 'GGG')!).metrics = [];
    delete loose(s.countries.find((c) => c.iso3 === 'FFF')!).metrics;
    delete loose(s).framing;
    loose(s.countries[0]).slots = null;
    loose(s.countries[0]).narrative = null;
    expect(checkCompactsDoc(d)).toEqual([]);
  });

  it('a newer section asks for a reload; a non-object is one problem', () => {
    const d = doc();
    d.strategic!.version = AIB_STRATEGIC_VERSION + 1;
    const p = checkCompactsDoc(d);
    expect(p).toEqual([`strategic: version ${AIB_STRATEGIC_VERSION + 1} needs a newer app`]);
    expect(needsNewerApp(p)).toBe(true);
    const x = doc();
    loose(x).strategic = 'x';
    expect(checkCompactsDoc(x)).toEqual(['strategic: not an object']);
  });

  it('bad values are reported with the paths of the R mirror (aib_check_strategic())', () => {
    const d = doc();
    const s = d.strategic!;
    const iUsa = s.countries.findIndex((c) => c.iso3 === 'AAA');
    const iZ = s.countries.findIndex((c) => c.iso3 === 'ZZZ');
    loose(s).defaultScenario = 'stress';
    s.compacts[0].steps[1].order = 2;
    s.compacts[1].steps[0].nodes = ['bat-up-graphite', 'zinc'];
    delete loose(s.compacts[0].scenarios).partial;
    s.compacts[0].scenarios.decoupling!.counts.build = 9;
    loose(s.compacts[0].scenarios.decoupling!).criticalPartners = null;
    s.compacts[0].division[0][5] = 'x';
    s.workstreams[0].compact = 'SC-Z';
    s.countries[iUsa].primary = ['DMA', 'CIA', 'CBH', 'SMI'];
    s.countries[iZ].secondary = ['DMA'];
    s.compactRoles[0].leadNode = 'cell';
    s.theses[0][3] = ['friendship'];
    const p = checkCompactsDoc(d);
    const has = (msg: string) => p.some((x) => x.startsWith(msg));
    for (const want of [
      "strategic.defaultScenario: 'stress' is not a listed scenario",
      'strategic.compacts[0].steps[1].order: 2 does not follow 2',
      "strategic.compacts[1].steps[0].nodes: 'zinc' is not a node of the document",
      'strategic.compacts[1].steps[0].functions: expected one per node',
      "strategic.compacts[0].scenarios: no 'partial'",
      'strategic.compacts[0].scenarios.decoupling.counts.build: 9 where the spine has 1',
      'strategic.compacts[0].scenarios.decoupling.criticalPartners: expected an array',
      'strategic.compacts[0].division[0][5]: expected flags',
      "strategic.workstreams[0].compact: 'SC-Z' is not a strategic compact",
      `strategic.countries[${iUsa}].primary: more than 3 roles`,
      `strategic.countries[${iZ}].secondary: 'DMA' is not a supply role`,
      "strategic.compactRoles[0].leadNode: 'cell' is not a node of",
      "strategic.theses[0][3]: 'friendship' is not a relationship logic",
    ]) {
      expect(has(want), want).toBe(true);
    }
    // a compact id twice, or a node in two compacts; a spine out of step order; the role lists against the tuples
    const d2 = doc();
    const s2 = d2.strategic!;
    s2.compacts[1].id = s2.compacts[0].id;
    s2.compacts[2].steps[0].nodes = ['ev-wind-mid-magnets'];
    s2.compacts[3].scenarios.managed!.nodes = [];
    s2.countries[0].primary = ['DMA'];
    s2.countries[0].roles[0] = ['DMA', 'primary', 1, 2, null, null, null, null];
    const p2 = checkCompactsDoc(d2);
    const has2 = (msg: string) => p2.some((x) => x.startsWith(msg));
    expect(has2(`strategic.compacts[1].id: duplicate '${s2.compacts[0].id}'`)).toBe(true);
    expect(has2("strategic.compacts[2].steps[0].nodes: 'ev-wind-mid-magnets' is in more than one step or compact")).toBe(true);
    expect(has2("strategic.compacts[3].scenarios.managed.nodes: not the compact's spine nodes in step order")).toBe(true);
    expect(has2('strategic.countries[0].roles[0][3]: 2 above 1')).toBe(true);
    // the tuples and lists keep their shape (the jsonlite auto_unbox trap)
    const d3 = doc();
    loose(d3.strategic!.compacts[0]).workstreams = 'AIB-C01';
    loose(d3.strategic!.compacts[0].scenarios.managed!.nodes)[0] = { node: 'x' };
    loose(d3.strategic!.theses)[0] = ['AAA', 'BBB'];
    const p3 = checkCompactsDoc(d3);
    expect(p3).toContain('strategic.compacts[0].workstreams: expected an array (a one-item array collapsed to a string?)');
    expect(p3).toContain('strategic.compacts[0].scenarios.managed.nodes[0]: expected [node, status, coverage, host] or [node, status, coverage, host, tied hosts]');
    expect(p3).toContain('strategic.theses[0]: expected [ISO3, ISO3, compacts, logic]');
    // 2026-10-21: the optional fields of the review (a fifth spine element, NOT TESTED, targeted missions, the
    // allied-only slots, the stress note) pass when well formed and are reported when not
    const d5 = doc();
    const k5 = d5.strategic!.compacts[0];
    const m5 = k5.scenarios.managed!;
    m5.nodes[0] = [m5.nodes[0][0], 'NOT TESTED', null, null, 3];
    m5.counts = { ...m5.counts, notTested: 1 };
    Object.assign(m5, { alliedOutsideCore: [], nonAlliedReliedOn: null, buildHostsNonAllied: [], buildHostTies: 2, stressNote: 'a note' });
    if (m5.missions[0]) m5.missions[0].targeted = true;
    const p5 = checkCompactsDoc(d5).filter((x) => x.includes('scenarios.managed') && !x.includes('counts.'));
    expect(p5).toEqual([]);
    loose(m5.nodes[0])[4] = 0;
    Object.assign(m5, { buildHostTies: -1, stressNote: 3, alliedOutsideCore: 'DEU' });
    const p6 = checkCompactsDoc(d5);
    expect(p6.some((x) => x.startsWith('strategic.compacts[0].scenarios.managed.nodes[0][4]'))).toBe(true);
    expect(p6.some((x) => x.startsWith('strategic.compacts[0].scenarios.managed.buildHostTies'))).toBe(true);
    expect(p6.some((x) => x.startsWith('strategic.compacts[0].scenarios.managed.stressNote'))).toBe(true);
    expect(p6.some((x) => x.startsWith('strategic.compacts[0].scenarios.managed.alliedOutsideCore'))).toBe(true);
    // a NOT TESTED node needs its count
    const d7 = doc();
    const m7 = d7.strategic!.compacts[0].scenarios.managed!;
    m7.nodes[0] = [m7.nodes[0][0], 'NOT TESTED', null, null];
    expect(checkCompactsDoc(d7)).toContain('strategic.compacts[0].scenarios.managed.counts.notTested: absent where the spine has NOT TESTED nodes');
    // every compact of the document is a workstream
    const d4 = doc();
    d4.strategic!.workstreams = d4.strategic!.workstreams.slice(1);
    d4.strategic!.compacts[0].workstreams = [];
    for (const sc of Object.values(d4.strategic!.compacts[0].scenarios)) sc!.activeWorkstreams = [];
    expect(checkCompactsDoc(d4)).toEqual([`strategic.workstreams: compact '${d4.compacts[0].id}' of the document is not a workstream`]);
    expect(strat().compacts.length).toBe(5);
  });

  it("a country's items (2026-10-21) are optional; when present they name role gates, functions, compacts, economies and nodes", () => {
    const ok = doc();
    const c = ok.strategic!.countries[0];
    c.items = { firstOn: ['demand_mean_share'], contributes: [['ev-wind-mid-magnets', 0.18]], focus: ['components'], needs: [], compacts: ['SC-MAGNETS'], partners: [ok.strategic!.countries[1].iso3], capacity: [['ev-wind-mid-magnets', 0.6]], capacityUsdBn: 0.6 };
    ok.strategic!.countries[1].items = null;
    expect(checkCompactsDoc(ok)).toEqual([]);
    const bad = doc();
    loose(bad.strategic!.countries[0]).items = { firstOn: ['popularity'], contributes: [['zinc', 1.5]], focus: ['retail'], needs: [['ev-wind-mid-magnets']], compacts: ['SC-Z'], partners: ['XXX'], capacity: [['ev-wind-mid-magnets', -1]], capacityUsdBn: -2 };
    const p = checkCompactsDoc(bad);
    const at = 'strategic.countries[0].items';
    for (const want of [
      `${at}.firstOn: 'popularity' is not a role gate`,
      `${at}.focus: 'retail' is not a node function`,
      `${at}.compacts: 'SC-Z' is not a strategic compact`,
      `${at}.partners: 'XXX' is not an economy of strategic.countries`,
      `${at}.contributes[0][0]: not a node of a strategic compact`,
      `${at}.contributes[0][1]: 1.5 above 1`,
      `${at}.needs[0]: expected [node id, number]`,
      `${at}.capacity[0][1]: -1 below 0`,
      `${at}.capacityUsdBn: -2 below 0`,
    ]) {
      expect(p.some((x) => x.startsWith(want)), want).toBe(true);
    }
  });
});
