// The Sectoral Compacts contract and store: the synthetic fixture passes both checks; missing, badly
// typed and collapsed fields are reported with their paths; a newer version asks for a reload; the
// tolerated shapes of the R export ([] for an empty named list, null numbers and NA strings) pass; the
// store maps every failure to NO_DATA / NEWER / CONTRACT / NETWORK and can retry.

import { describe, expect, it } from 'vitest';
import { AIB_DOC_FILE_RE, checkAibManifest, checkCompactsDoc, countryNamesOf, nodeCoverageOf, type AibDoc } from '../contract.ts';
import { AibLoadError, AibStore, httpAibSource, type AibSource } from '../data/store.ts';
import { fixtureSource, readDoc, readManifest, SYNTHETIC_DIR } from './fixture.ts';

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
      for (const f of n.frontier) {
        delete f.secondShock;
        delete f.specialization;
      }
    }
    expect(checkCompactsDoc(d)).toEqual([]);
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
