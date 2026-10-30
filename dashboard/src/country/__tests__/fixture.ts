// The country builder's R-written synthetic fixture (scripts/88 --synthetic ->
// src/country/__fixtures__/synthetic/data/country/), for tests only (node fs): the manifest, the sheets
// and a CountrySource over the files. COUNTRY_DATA_DIR overrides the directory (e.g.
// public/data/country for a local run on the real export).
//
// Schema 3 (2026-10-29, design D6): the Legacy OSI corners are namespaced (legacy_corner, legacy_*). Until
// the R side re-exports the fixture, a schema-2 manifest or fact sheet is upgraded in memory here
// (toSchema3: the renames only, the corner computation unchanged); a schema-3 export passes through as it is.

import { existsSync, readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import type { CountryManifest, FactSheet } from '../contract.ts';
import { CountryLoadError, type CountrySource } from '../data/store.ts';

export const FIXTURE_DIR = resolve(process.cwd(), process.env.COUNTRY_DATA_DIR || 'src/country/__fixtures__/synthetic/data/country');

export function haveFixture(dir = FIXTURE_DIR): boolean {
  return existsSync(resolve(dir, 'manifest.json'));
}

type Obj = Record<string, unknown>;
const isObj = (x: unknown): x is Obj => typeof x === 'object' && x !== null && !Array.isArray(x);
/** schema 2 corner key -> schema 3 */
const CORNER_V3: Readonly<Record<string, string>> = { grow: 'legacy_grow', upgrade: 'legacy_upgrade', secure: 'legacy_exposed', watch: 'legacy_stable' };
const rekey = (o: unknown): unknown => (isObj(o) ? Object.fromEntries(Object.entries(o).map(([k, v]) => [CORNER_V3[k] ?? k, v])) : o);

/** True when the fixture on disk is still a schema-2 export (the in-memory upgrade is in use). */
export function fixtureIsSchema2(dir = FIXTURE_DIR): boolean {
  return haveFixture(dir) && (JSON.parse(readFileSync(resolve(dir, 'manifest.json'), 'utf8')) as { schema_version?: number }).schema_version === 2;
}

/**
 * A schema-2 Country manifest or fact sheet in the schema-3 shape (design D6): quadrant -> legacy_corner
 * (values legacy_*), quadrant_reason -> legacy_corner_reason, summary.quadrants -> summary.legacy_corners,
 * words.quadrant -> words.legacy_corner, countries[].quadrants -> countries[].legacy_corners, schema_version 3.
 * Anything else (schema 3, drivers, memo and trade files) is returned as it is.
 */
export function toSchema3<T>(doc: T): T {
  if (!isObj(doc) || doc.schema_version !== 2) return doc;
  if (doc.schema === 'osi-country') {
    const countries = Array.isArray(doc.countries)
      ? doc.countries.map((c) => {
          if (!isObj(c) || !('quadrants' in c)) return c;
          const { quadrants, ...rest } = c;
          return { ...rest, legacy_corners: quadrants === null ? null : rekey(quadrants) };
        })
      : doc.countries;
    return { ...doc, schema_version: 3, countries } as T;
  }
  if (doc.schema === 'osi-country-facts') {
    const words = isObj(doc.words) && 'quadrant' in doc.words ? (({ quadrant, ...w }) => ({ ...w, legacy_corner: rekey(quadrant) }))(doc.words) : doc.words;
    const summary = isObj(doc.summary) && 'quadrants' in doc.summary ? (({ quadrants, ...s }) => ({ ...s, legacy_corners: rekey(quadrants) }))(doc.summary) : doc.summary;
    const cells = Array.isArray(doc.cells)
      ? doc.cells.map((c) => {
          if (!isObj(c) || !('quadrant' in c)) return c;
          const { quadrant, quadrant_reason, ...rest } = c;
          return { ...rest, legacy_corner: typeof quadrant === 'string' ? (CORNER_V3[quadrant] ?? quadrant) : quadrant, legacy_corner_reason: quadrant_reason ?? null };
        })
      : doc.cells;
    return { ...doc, schema_version: 3, words, summary, cells } as T;
  }
  return doc;
}

export function readManifest(dir = FIXTURE_DIR): CountryManifest {
  return toSchema3(JSON.parse(readFileSync(resolve(dir, 'manifest.json'), 'utf8')) as CountryManifest);
}

export function readJson<T = unknown>(file: string, dir = FIXTURE_DIR): T {
  return toSchema3(JSON.parse(readFileSync(resolve(dir, file), 'utf8')) as T);
}

/** The fact sheet of an economy of the fixture. */
export function readSheet(iso3: string, dir = FIXTURE_DIR): FactSheet {
  const e = readManifest(dir).countries.find((c) => c.iso3 === iso3);
  if (!e || !e.file) throw new Error(`no sheet for ${iso3}`);
  return readJson<FactSheet>(e.file, dir);
}

/** A CountrySource over the fixture's files (records the files requested). */
export function fixtureSource(dir = FIXTURE_DIR): CountrySource & { calls: string[] } {
  const calls: string[] = [];
  return {
    calls,
    async manifest() {
      calls.push('manifest.json');
      if (!haveFixture(dir)) throw new CountryLoadError('NO_DATA', 'No country fixture.');
      return readManifest(dir);
    },
    async json(file) {
      calls.push(file);
      if (!existsSync(resolve(dir, file))) throw new CountryLoadError('NETWORK', `${file} could not be fetched (HTTP 404).`);
      return readJson(file, dir);
    },
    url: (file) => `/data/country/${file}`,
  };
}
