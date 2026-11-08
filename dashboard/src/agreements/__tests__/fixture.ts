// The SYNTHETIC partnership-agreements bundle (src/agreements/__fixtures__/synthetic/data/agreements/, written by
// src/agreements/__fixtures__/make-synthetic.mjs), for tests only (node fs): the manifest, the global file, an
// economy's file, and an AgreementsSource over the files (`edit` changes a file's parsed JSON before the
// store checks it). AGREEMENTS_DATA_DIR runs the tests on another bundle (e.g. public/data/agreements).

import { existsSync, readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import type { AgreementsCountry, AgreementsGlobal, AgreementsManifest } from '../contract.ts';
import { AgreementsLoadError, type AgreementsSource } from '../data.ts';

export const FIXTURE_DIR = resolve(process.cwd(), process.env.AGREEMENTS_DATA_DIR || 'src/agreements/__fixtures__/synthetic/data/agreements');

export const haveFixture = (dir = FIXTURE_DIR): boolean => existsSync(resolve(dir, 'manifest.json'));

export function readJson<T = unknown>(file: string, dir = FIXTURE_DIR): T {
  return JSON.parse(readFileSync(resolve(dir, file), 'utf8')) as T;
}

export const readManifest = (dir = FIXTURE_DIR): AgreementsManifest => readJson<AgreementsManifest>('manifest.json', dir);
export const readGlobal = (dir = FIXTURE_DIR): AgreementsGlobal => readJson<AgreementsGlobal>(readManifest(dir).files.global, dir);
export const readCountry = (iso3: string, dir = FIXTURE_DIR): AgreementsCountry => readJson<AgreementsCountry>(readManifest(dir).files.countries[iso3], dir);

/** A source over the fixture's files, recording the files requested. */
export function fixtureSource(dir = FIXTURE_DIR, edit?: (file: string, x: unknown) => unknown): AgreementsSource & { calls: string[] } {
  const calls: string[] = [];
  const load = (file: string) => {
    calls.push(file);
    const path = resolve(dir, file);
    if (!existsSync(path)) {
      if (file === 'manifest.json') return Promise.reject(new AgreementsLoadError('NO_DATA', 'data/agreements/manifest.json does not exist (HTTP 404).'));
      return Promise.reject(new AgreementsLoadError('NETWORK', `data/agreements/${file} does not exist (HTTP 404).`));
    }
    const x = JSON.parse(readFileSync(path, 'utf8')) as unknown;
    return Promise.resolve(edit ? edit(file, x) : x);
  };
  return { calls, manifest: () => load('manifest.json'), json: (file) => load(file) };
}

/** A source with no bundle (CI without the export). */
export function emptySource(): AgreementsSource {
  return {
    manifest: () => Promise.reject(new AgreementsLoadError('NO_DATA', 'data/agreements/manifest.json does not exist (HTTP 404).')),
    json: (file) => Promise.reject(new AgreementsLoadError('NETWORK', `data/agreements/${file} does not exist (HTTP 404).`)),
  };
}
