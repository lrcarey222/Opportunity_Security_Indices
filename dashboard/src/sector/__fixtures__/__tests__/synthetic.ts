// Builder A's synthetic sector bundle (scripts/87 --synthetic -> tests/fixtures/sector/synthetic/), for
// tests only (node fs): the manifest, decoded chunks and a SectorSource over the files, so the views
// and the summary parity also run on the R-written bundle, not just the TS fixture. Absent bundle:
// haveSynthetic() is false and the tests that use it skip.

import { existsSync, readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { gunzipSync } from 'node:zlib';
import { decodeOsb, isGzip, type OsbDecoded } from '../../../engine/codec.ts';
import type { SectorChunkName, SectorManifest } from '../../contract.ts';
import { SectorLoadError, type SectorSource } from '../../data/store.ts';

/** The bundle directory: A's synthetic bundle, or SECTOR_BUNDLE_DIR (e.g. public/data/sector for a local run on the real export). */
export const SYNTH_DIR = resolve(process.cwd(), process.env.SECTOR_BUNDLE_DIR || 'tests/fixtures/sector/synthetic');

export function haveSynthetic(): boolean {
  return existsSync(resolve(SYNTH_DIR, 'manifest.json'));
}

export function readSyntheticManifest(): SectorManifest {
  return JSON.parse(readFileSync(resolve(SYNTH_DIR, 'manifest.json'), 'utf8')) as SectorManifest;
}

/** The decoded objects of one chunk file of the bundle. */
export function readSyntheticFile(file: string): OsbDecoded {
  let b: Uint8Array = readFileSync(resolve(SYNTH_DIR, file));
  if (isGzip(b)) b = gunzipSync(b);
  const out = new Uint8Array(b.length);
  out.set(b);
  return decodeOsb(out);
}

/** The decoded objects of a chunk by name, or null when the bundle has no such chunk. */
export function readSyntheticChunk(name: SectorChunkName): OsbDecoded | null {
  const c = readSyntheticManifest().chunks.find((x) => x.name === name);
  return c ? readSyntheticFile(c.file) : null;
}

/** A SectorSource over the bundle's files (records the files requested). */
export function syntheticSource(): SectorSource & { calls: string[] } {
  const calls: string[] = [];
  const cache = new Map<string, OsbDecoded>();
  return {
    calls,
    async manifest() {
      calls.push('manifest.json');
      if (!haveSynthetic()) throw new SectorLoadError('NO_DATA', 'No synthetic sector bundle.');
      return readSyntheticManifest();
    },
    async chunk(file) {
      calls.push(file);
      let d = cache.get(file);
      if (!d) {
        if (!existsSync(resolve(SYNTH_DIR, file))) throw new SectorLoadError('NETWORK', `${file} could not be fetched (HTTP 404).`);
        d = readSyntheticFile(file);
        cache.set(file, d);
      }
      return d;
    },
  };
}
