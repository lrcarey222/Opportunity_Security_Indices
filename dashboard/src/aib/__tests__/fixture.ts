// The SYNTHETIC Sectoral Compacts fixture (src/aib/__fixtures__/synthetic/data/aib/: invented economies
// and numbers, two compacts, six nodes), for tests only (node fs): the manifest, the document and an
// AibSource over the files. AIB_DATA_DIR overrides the directory (e.g. public/data/aib for a local run of
// the view tests on the real export).

import { existsSync, readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import type { AibDoc, AibManifest } from '../contract.ts';
import { AibLoadError, type AibSource } from '../data/store.ts';

const here = resolve(process.cwd());
export const SYNTHETIC_DIR = resolve(here, 'src/aib/__fixtures__/synthetic/data/aib');
export const FIXTURE_DIR = resolve(here, process.env.AIB_DATA_DIR || 'src/aib/__fixtures__/synthetic/data/aib');
/** The real export (scripts/84), when present; AIB_REAL_DIR checks another export directory. */
export const REAL_DIR = resolve(here, process.env.AIB_REAL_DIR || 'public/data/aib');

export function haveFixture(dir = FIXTURE_DIR): boolean {
  return existsSync(resolve(dir, 'manifest.json'));
}

export function readManifest(dir = FIXTURE_DIR): AibManifest {
  return JSON.parse(readFileSync(resolve(dir, 'manifest.json'), 'utf8')) as AibManifest;
}

export function readDoc(dir = FIXTURE_DIR): AibDoc {
  return JSON.parse(readFileSync(resolve(dir, readManifest(dir).files.compacts), 'utf8')) as AibDoc;
}

/** An AibSource over a directory's files (records the files requested); `edit` changes the document. */
export function fixtureSource(dir = FIXTURE_DIR, edit?: (doc: AibDoc) => void): AibSource & { calls: string[] } {
  const calls: string[] = [];
  return {
    calls,
    async manifest() {
      calls.push('manifest.json');
      if (!haveFixture(dir)) throw new AibLoadError('NO_DATA', 'data/aib/manifest.json does not exist (HTTP 404).');
      return readManifest(dir);
    },
    async json(file) {
      calls.push(file);
      if (!existsSync(resolve(dir, file))) throw new AibLoadError('NETWORK', `data/aib/${file} does not exist (HTTP 404).`);
      const doc = JSON.parse(readFileSync(resolve(dir, file), 'utf8')) as AibDoc;
      if (edit) edit(doc);
      return doc;
    },
  };
}
