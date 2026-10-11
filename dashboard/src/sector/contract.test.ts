// The contract on fixtures (spec_sector.md 10.2): every chunk of the TS fixture passes validateChunk();
// when builder A's committed R fixture (scripts/87 --synthetic -> tests/fixtures/sector/synthetic/)
// exists, every chunk of it is decoded with engine/codec.ts and validated too; TECH_SLUGS and
// RESERVED_SEGMENTS do not intersect.

import { describe, expect, it } from 'vitest';
import { existsSync, readdirSync, readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { gunzipSync } from 'node:zlib';
import { decodeOsb, isGzip } from '../engine/codec.ts';
import {
  checkChunk, isSectorManifest, MINERAL_SLUGS, refSizes, RESERVED_SEGMENTS, sectorManifestProblems, TECH_SLUGS, validateChunk,
  type SectorChunkName, type SectorManifest, type SectorMetaChunk,
} from './contract.ts';
import { buildSectorFixture } from './__fixtures__/build.ts';

describe('slugs', () => {
  it('no technology slug equals a reserved segment', () => {
    const reserved = new Set<string>(RESERVED_SEGMENTS);
    expect(TECH_SLUGS.filter((t) => reserved.has(t))).toEqual([]);
    expect(MINERAL_SLUGS.filter((t) => reserved.has(t))).toEqual([]);
  });
});

describe('TS fixture', () => {
  const fx = buildSectorFixture();
  it('has a valid manifest', () => {
    expect(sectorManifestProblems(fx.manifest)).toEqual([]);
    expect(isSectorManifest(fx.manifest)).toBe(true);
  });
  it('every chunk passes validateChunk', () => {
    const meta = validateChunk('sector-meta', fx.chunks['sector-meta']);
    const ref = refSizes(meta);
    for (const name of Object.keys(fx.chunks) as SectorChunkName[]) {
      const res = checkChunk(name, fx.chunks[name], name === 'sector-meta' ? undefined : ref);
      expect(res.errors, name).toEqual([]);
    }
  });
});

// ---- builder A's synthetic bundle (committed by scripts/87 --synthetic) --------------------------

const synthDir = resolve(process.cwd(), 'tests/fixtures/sector/synthetic');
const manifestPath = resolve(synthDir, 'manifest.json');
const haveSynth = existsSync(manifestPath);

function readChunk(file: string) {
  let b: Uint8Array = readFileSync(resolve(synthDir, file));
  if (isGzip(b)) b = gunzipSync(b);
  const out = new Uint8Array(b.length);
  out.set(b);
  return decodeOsb(out);
}

describe.skipIf(!haveSynth)('R synthetic fixture (scripts/87 --synthetic)', () => {
  it('manifest conforms and lists files that exist', () => {
    const m = JSON.parse(readFileSync(manifestPath, 'utf8')) as SectorManifest;
    expect(sectorManifestProblems(m)).toEqual([]);
    const files = new Set(readdirSync(synthDir));
    for (const c of m.chunks) expect(files.has(c.file), c.file).toBe(true);
  });
  it('every chunk decodes and passes validateChunk', () => {
    const m = JSON.parse(readFileSync(manifestPath, 'utf8')) as SectorManifest;
    const metaInfo = m.chunks.find((c) => c.name === 'sector-meta');
    expect(metaInfo).toBeTruthy();
    const meta = validateChunk('sector-meta', readChunk(metaInfo!.file)) as SectorMetaChunk;
    const ref = refSizes(meta);
    for (const c of m.chunks) {
      const res = checkChunk(c.name, readChunk(c.file), c.name === 'sector-meta' ? undefined : ref);
      expect(res.errors, c.name).toEqual([]);
    }
  });
});

describe.skipIf(!haveSynth)('headlines of the R synthetic manifest fill the questions (8.8, 2.8)', () => {
  it('every question whose headline the manifest carries gets a fact; the others none', async () => {
    const { QUESTIONS, questionFact } = await import('../content/questions.ts');
    const m = JSON.parse(readFileSync(manifestPath, 'utf8')) as SectorManifest;
    const ids = new Set(m.headlines.map((h) => h.id));
    for (const q of QUESTIONS) {
      if (!q.headline_id) continue;
      const fact = questionFact(q, m.headlines);
      if (ids.has(q.headline_id)) {
        expect(fact, q.id).not.toBeNull();
        expect(fact!.text, q.id).not.toMatch(/\{|\}|NaN|undefined/);
      } else expect(fact, q.id).toBeNull();
    }
  });
});
