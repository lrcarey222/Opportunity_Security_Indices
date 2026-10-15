// The country builder's R-written synthetic fixture (scripts/88 --synthetic ->
// src/country/__fixtures__/synthetic/data/country/), for tests only (node fs): the manifest, the sheets
// and a CountrySource over the files. COUNTRY_DATA_DIR overrides the directory (e.g.
// public/data/country for a local run on the real export).

import { existsSync, readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import type { CountryManifest, FactSheet } from '../contract.ts';
import { CountryLoadError, type CountrySource } from '../data/store.ts';

export const FIXTURE_DIR = resolve(process.cwd(), process.env.COUNTRY_DATA_DIR || 'src/country/__fixtures__/synthetic/data/country');

export function haveFixture(dir = FIXTURE_DIR): boolean {
  return existsSync(resolve(dir, 'manifest.json'));
}

export function readManifest(dir = FIXTURE_DIR): CountryManifest {
  return JSON.parse(readFileSync(resolve(dir, 'manifest.json'), 'utf8')) as CountryManifest;
}

export function readJson<T = unknown>(file: string, dir = FIXTURE_DIR): T {
  return JSON.parse(readFileSync(resolve(dir, file), 'utf8')) as T;
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
