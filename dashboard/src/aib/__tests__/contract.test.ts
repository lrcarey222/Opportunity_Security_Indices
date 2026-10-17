// The Sectoral Compacts contract and store: the synthetic fixture passes both checks; missing, badly
// typed and collapsed fields are reported with their paths; a newer version asks for a reload; the
// tolerated shapes of the R export ([] for an empty named list, null numbers and NA strings) pass; the
// additive and methodology fields are optional (an older bundle, secondShock and all, validates) and
// checked when present; the store maps every failure to NO_DATA / NEWER / CONTRACT / NETWORK and can
// retry.

import { describe, expect, it } from 'vitest';
import { AIB_DOC_FILE_RE, checkAibManifest, checkCompactsDoc, coalitionMembersOf, countryNamesOf, inStrategicScope, nodeCoverageOf, variantsOf, variantWarningsOf, type AibDoc } from '../contract.ts';
import { AibLoadError, AibStore, httpAibSource, type AibSource } from '../data/store.ts';
import { fixtureSource, readDoc, readManifest, stripMethodology, SYNTHETIC_DIR } from './fixture.ts';

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
    const m = { ...readManifest(SYNTHETIC_DIR), version: 2 };
    expect(checkAibManifest(m)).toEqual(['manifest: version 2 needs a newer app']);
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
    expect(full.meta).toMatchObject({ demandScope: 'coalition', candidateUniverse: 'global', shockSemantics: 'hold', supplyConstraint: 'production_constrained', strategicScope: 'electro_industrial' });
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
    expect(inStrategicScope(ok.nodes.find((n) => n.id === 'gas-mid-turbines')!)).toBe(false);
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

    const newer = new AibStore({ manifest: async () => ({ ...readManifest(SYNTHETIC_DIR), version: 2 }), json: async () => ({}) });
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
