// The agreements data contract (docs/partnership_agreements.md section 9) on the synthetic bundle: every file
// passes; the checks name a collapsed one-item array, an unknown code, a depth outside 0-5, a grade or quadrant
// outside its list, ragged columns, a wrong file name and a newer version; the store loads the files lazily
// (manifest first, an economy's file only when asked, one promise each) and says NO_DATA without a bundle.

import { describe, expect, it } from 'vitest';
import { AGR_COUNTRY_FILE_RE, AGR_GLOBAL_FILE_RE, checkAgreementsCountry, checkAgreementsGlobal, checkAgreementsManifest, DEPTH_CLASSES } from '../contract.ts';
import { AgreementsLoadError, AgreementsStore, httpAgreementsSource } from '../data.ts';
import { emptySource, fixtureSource, readCountry, readGlobal, readManifest } from './fixture.ts';

const m = readManifest();
const clone = <T>(x: T): T => structuredClone(x);

describe('the synthetic bundle passes the contract', () => {
  it('manifest, global file and every economy file', () => {
    expect(checkAgreementsManifest(m)).toEqual([]);
    expect(checkAgreementsGlobal(readGlobal(), m)).toEqual([]);
    for (const iso of Object.keys(m.files.countries)) expect(checkAgreementsCountry(readCountry(iso), iso, m), iso).toEqual([]);
  });

  it('covers what the views need: all six depth classes, both grades, delivery evidence, an EU agreement through a member state, several sources, gaps', () => {
    const g = readGlobal();
    const depths = new Set([...g.pairs.depth_all, ...g.pairs.depth_ei]);
    for (const d of DEPTH_CLASSES) expect(depths.has(d), `depth ${d}`).toBe(true);
    const jpn = readCountry('JPN');
    const deu = readCountry('DEU');
    expect(jpn.agreements.some((a) => a.sectors.some(([, b]) => b === 'text_body'))).toBe(true);
    expect(jpn.agreements.some((a) => a.sectors.some(([, b]) => b === 'curated'))).toBe(true);
    expect(jpn.agreements.some((a) => a.delivery.length > 0)).toBe(true);
    expect(deu.agreements.some((a) => a.via_eu)).toBe(true);
    expect(jpn.agreements.some((a) => a.source_count >= 2)).toBe(true);
    expect(new Set(jpn.gaps.map((x) => x.quadrant))).toEqual(new Set(['established', 'opportunity', 'legacy', 'none']));
    expect(jpn.execution_gaps.length).toBeGreaterThan(0);
    expect(AGR_GLOBAL_FILE_RE.test(m.files.global)).toBe(true);
    for (const f of Object.values(m.files.countries)) expect(AGR_COUNTRY_FILE_RE.test(f)).toBe(true);
  });
});

describe('the checks', () => {
  it('manifest: a newer version, a bad file name, a country file naming another economy, depth keys, delivery statuses', () => {
    expect(checkAgreementsManifest({ ...m, version: 2 }).join(' ')).toMatch(/needs a newer app/);
    expect(checkAgreementsManifest({ ...m, files: { ...m.files, global: 'agreements-global.json' } }).join(' ')).toMatch(/files.global/);
    expect(checkAgreementsManifest({ ...m, files: { ...m.files, countries: { JPN: m.files.countries.AUS } } }).join(' ')).toMatch(/the file names AUS/);
    const v = clone(m);
    delete (v.vocab.depth_classes as Record<string, string>)['5'];
    expect(checkAgreementsManifest(v).join(' ')).toMatch(/depth_classes: expected the keys 0 to 5/);
    const w = clone(m);
    w.vocab.delivery_statuses = ['delivered'];
    expect(checkAgreementsManifest(w).join(' ')).toMatch(/'delivered' is not an implementation status/);
    expect(checkAgreementsManifest({ ...m, caveats: 'one caveat' }).join(' ')).toMatch(/caveats: expected an array \(a one-item array collapsed to a string\?\)/);
    expect(checkAgreementsManifest(null)).toEqual(['manifest: not an object']);
  });

  it('manifest, additive vocabularies: the instrument bases are coding bases, the compacts an id -> name map; both optional', () => {
    expect(m.vocab.established_instrument_bases).toEqual(['source_coded', 'curated', 'type_implied']);
    expect(m.vocab.compacts?.['SC-BATTERIES']).toBe('Battery Materials & Cells');
    const b = clone(m);
    b.vocab.established_instrument_bases = ['source_coded', 'guessed'];
    expect(checkAgreementsManifest(b).join(' ')).toMatch(/established_instrument_bases: 'guessed' is not a coding basis/);
    const one = clone(m);
    (one.vocab as unknown as Record<string, unknown>).established_instrument_bases = 'curated';
    expect(checkAgreementsManifest(one).join(' ')).toMatch(/established_instrument_bases: expected an array \(a one-item array collapsed to a string\?\)/);
    const c = clone(m);
    (c.vocab as unknown as Record<string, unknown>).compacts = { 'SC-BATTERIES': 3 };
    expect(checkAgreementsManifest(c).join(' ')).toMatch(/compacts.SC-BATTERIES: expected a string/);
    const empty = clone(m);
    (empty.vocab as unknown as Record<string, unknown>).compacts = [];
    expect(checkAgreementsManifest(empty)).toEqual([]);
    const none = clone(m);
    delete none.vocab.compacts;
    delete none.vocab.established_instrument_bases;
    expect(checkAgreementsManifest(none)).toEqual([]);
  });

  it('global: ragged columns, an unordered pair, a depth outside 0-5, an unknown sector', () => {
    const g = readGlobal();
    const r = clone(g);
    r.pairs.n = r.pairs.n.slice(1);
    expect(checkAgreementsGlobal(r, m).join(' ')).toMatch(/different lengths/);
    const u = clone(g);
    [u.pairs.a[0], u.pairs.b[0]] = [u.pairs.b[0], u.pairs.a[0]];
    expect(checkAgreementsGlobal(u, m).join(' ')).toMatch(/not ordered a < b/);
    const d = clone(g);
    d.pairs.depth_all[0] = 6;
    expect(checkAgreementsGlobal(d, m).join(' ')).toMatch(/depth_all: '6' is not one of 0, 1, 2, 3, 4, 5/);
    const s = clone(g);
    s.pairs.ei_sectors[0] = 'unobtainium';
    expect(checkAgreementsGlobal(s, m).join(' ')).toMatch(/'unobtainium' is not in the vocabulary/);
    // R's pair profile writes ei_sectors as id:grade; read as well as plain ids, a wrong grade named
    const graded = clone(g);
    graded.pairs.ei_sectors = graded.pairs.ei_sectors.map((x) => (x ? x.split('|').map((id, i) => `${id}:${i ? 'text' : 'specific'}`).join('|') : x));
    expect(checkAgreementsGlobal(graded, m)).toEqual([]);
    const badGrade = clone(g);
    const at = badGrade.pairs.ei_sectors.findIndex(Boolean);
    badGrade.pairs.ei_sectors[at] = `${badGrade.pairs.ei_sectors[at]!.split('|')[0]}:guessed`;
    expect(checkAgreementsGlobal(badGrade, m).join(' ')).toMatch(/has no grade specific or text/);
    const one = clone(g);
    (one.pairs as unknown as Record<string, unknown>).a = 'AUS';
    expect(checkAgreementsGlobal(one, m).join(' ')).toMatch(/collapsed to a string/);
  });

  it('global, the additive pair columns: optional, checked when present (years, vocabulary codes, stages, length)', () => {
    const g = readGlobal();
    for (const k of ['years', 'years_ei', 'igroups', 'legal', 'status', 'stages'] as const) expect(g.pairs[k]?.length, k).toBe(g.pairs.a.length);
    const none = clone(g);
    for (const k of ['years', 'years_ei', 'igroups', 'legal', 'status', 'stages'] as const) delete none.pairs[k];
    expect(checkAgreementsGlobal(none, m)).toEqual([]);
    const y = clone(g);
    y.pairs.years![0] = '2014|20x5';
    expect(checkAgreementsGlobal(y, m).join(' ')).toMatch(/years: '20x5' is not a year/);
    const ig = clone(g);
    ig.pairs.igroups![0] = 'trade|bribery';
    expect(checkAgreementsGlobal(ig, m).join(' ')).toMatch(/igroups: 'bribery' is not in the vocabulary/);
    const lg = clone(g);
    lg.pairs.legal![0] = 'binding|maybe';
    expect(checkAgreementsGlobal(lg, m).join(' ')).toMatch(/legal: 'maybe' is not in the vocabulary/);
    const st = clone(g);
    st.pairs.status![0] = 'ratified';
    expect(checkAgreementsGlobal(st, m).join(' ')).toMatch(/status: 'ratified' is not in the vocabulary/);
    const sg = clone(g);
    sg.pairs.stages![0] = 'Midstream';
    expect(checkAgreementsGlobal(sg, m).join(' ')).toMatch(/stages: 'Midstream' is not in the vocabulary/);
    const short = clone(g);
    short.pairs.years_ei = short.pairs.years_ei!.slice(1);
    expect(checkAgreementsGlobal(short, m).join(' ')).toMatch(/years_ei: \d+ values, expected \d+/);
    const col = clone(g);
    (col.pairs as unknown as Record<string, unknown>).status = 'in_force';
    expect(checkAgreementsGlobal(col, m).join(' ')).toMatch(/status: expected an array \(a one-item array collapsed to a string\?\)/);
  });

  it('country: the wrong economy, a collapsed parties array, an unknown basis, a grade, a quadrant, a missing agreement id, no source', () => {
    const c = readCountry('JPN');
    expect(checkAgreementsCountry(c, 'AUS', m).join(' ')).toMatch(/JPN is not AUS/);
    const one = clone(c);
    (one.agreements[0] as unknown as Record<string, unknown>).techs = 'Batteries';
    expect(checkAgreementsCountry(one, 'JPN', m).join(' ')).toMatch(/techs: expected an array \(a one-item array collapsed to a string\?\)/);
    const b = clone(c);
    b.agreements[0].sectors = [['critical_minerals', 'guessed']];
    expect(checkAgreementsCountry(b, 'JPN', m).join(' ')).toMatch(/'guessed' is not a coding basis/);
    const gr = clone(c);
    gr.partners[0].sectors = [['hydrogen', 'strong' as 'specific']];
    expect(checkAgreementsCountry(gr, 'JPN', m).join(' ')).toMatch(/'strong' is not a grade/);
    const q = clone(c);
    (q.gaps[0] as unknown as Record<string, unknown>).quadrant = 'promising';
    expect(checkAgreementsCountry(q, 'JPN', m).join(' ')).toMatch(/quadrant: 'promising'/);
    // complementarity: high, low or not_assessed (need or capability data missing)
    expect(c.gaps.some((x) => x.complementarity === 'not_assessed')).toBe(true);
    const cp = clone(c);
    (cp.gaps[0] as unknown as Record<string, unknown>).complementarity = 'medium';
    expect(checkAgreementsCountry(cp, 'JPN', m).join(' ')).toMatch(/complementarity: 'medium' is not one of high, low, not_assessed/);
    const id = clone(c);
    id.partners[0].agreement_ids = ['agr-missing'];
    expect(checkAgreementsCountry(id, 'JPN', m).join(' ')).toMatch(/'agr-missing' is not in agreements/);
    const ns = clone(c);
    ns.agreements[0].sources = [];
    expect(checkAgreementsCountry(ns, 'JPN', m).join(' ')).toMatch(/no source record/);
    const dp = clone(c);
    (dp.partners[0] as unknown as Record<string, unknown>).depth_all = 7;
    expect(checkAgreementsCountry(dp, 'JPN', m).join(' ')).toMatch(/depth_all: '7'/);
    // jsonlite's empty named list for an instrument map is accepted
    const em = clone(c);
    (em.partners[0] as unknown as Record<string, unknown>).inst = [];
    expect(checkAgreementsCountry(em, 'JPN', m)).toEqual([]);
  });
});

describe('the store', () => {
  it('loads the manifest, then the global file and an economy file on demand, one promise each', async () => {
    const src = fixtureSource();
    const s = new AgreementsStore(src);
    const man = await s.manifest();
    expect(src.calls).toEqual(['manifest.json']);
    const [a, b] = await Promise.all([s.country(man, 'JPN'), s.country(man, 'JPN')]);
    expect(a).toBe(b);
    expect(src.calls).toEqual(['manifest.json', man.files.countries.JPN]);
    await s.global(man);
    expect(src.calls).toHaveLength(3);
    expect(s.peek('country:JPN')).toBe(a);
  });

  it('says NO_DATA without a bundle, CONTRACT for a broken file, and retries after a failure', async () => {
    await expect(new AgreementsStore(emptySource()).manifest()).rejects.toMatchObject({ code: 'NO_DATA' });
    const s = new AgreementsStore(fixtureSource(undefined, (f, x) => (f.startsWith('agreements-JPN') ? { ...(x as object), iso3: 'AUS' } : x)));
    const man = await s.manifest();
    await expect(s.country(man, 'JPN')).rejects.toMatchObject({ code: 'CONTRACT' });
    await expect(s.country(man, 'XXX')).rejects.toBeInstanceOf(AgreementsLoadError);
  });

  it('the HTTP source fetches the manifest without the cache and reads a 404 manifest as no data', async () => {
    const seen: [string, RequestInit | undefined][] = [];
    const src = httpAgreementsSource('/data/agreements/', {
      fetch: async (u, i) => {
        seen.push([u, i]);
        return new Response('not found', { status: 404 });
      },
    });
    await expect(src.manifest()).rejects.toMatchObject({ code: 'NO_DATA' });
    expect(seen[0]).toEqual(['/data/agreements/manifest.json', { cache: 'no-cache' }]);
    await expect(src.json('agreements-JPN.12345678.json')).rejects.toMatchObject({ code: 'NETWORK' });
    const html = httpAgreementsSource('/data/agreements/', { fetch: async () => new Response('<!doctype html>', { status: 200 }) });
    await expect(html.manifest()).rejects.toMatchObject({ code: 'NO_DATA' });
  });
});
