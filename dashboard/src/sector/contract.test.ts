// The contract on fixtures (spec_sector.md 10.2): every chunk of the TS fixture passes validateChunk();
// when builder A's committed R fixture (scripts/87 --synthetic -> tests/fixtures/sector/synthetic/)
// exists, every chunk of it is decoded with engine/codec.ts and validated too; TECH_SLUGS and
// RESERVED_SEGMENTS do not intersect. Schema 2 (2026-10-28): the six stages of lib/stages.ts, the 66-cell
// headline grid, mineral_stage items, trade.excluded per technology, fdi.stages and the measured budgets.

import { describe, expect, it } from 'vitest';
import { existsSync, readdirSync, readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { gunzipSync } from 'node:zlib';
import { decodeOsb, isGzip } from '../engine/codec.ts';
import {
  ACTIVITY_OF_STAGE, checkChunk, fdiCellSlug, isSectorManifest, MINERAL_REDIRECTS, MINERAL_SLUGS, refSizes, RESERVED_SEGMENTS, SECTOR_BUDGET_GZ, SECTOR_SCHEMA_VERSION, sectorManifestProblems,
  STAGE_LABELS, STAGE_MASK_ORDER, STAGE_SHORTS, STAGE_SLUGS, stageSlugOf, TECH_SLUGS, validateChunk,
  type SectorChunkName, type SectorManifest, type SectorMetaChunk,
} from './contract.ts';
import { buildSectorFixture } from './__fixtures__/build.ts';
import { SUPPLY_STAGE_SLUGS, SUPPLY_STAGES } from '../lib/stages.ts';

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

// ---- schema 2: six stages ------------------------------------------------------------------------

/** A shallow copy of a decoded table with one column's values replaced at some rows. */
function withCol(t: { nrow: number; cols: Record<string, unknown> }, col: string, set: (old: unknown[]) => unknown[]): { nrow: number; cols: Record<string, unknown> } {
  const old = t.cols[col] as ArrayLike<unknown>;
  const next = set(Array.from(old));
  const typed = old instanceof Uint8Array ? Uint8Array.from(next as number[]) : old instanceof Int32Array ? Int32Array.from(next as number[]) : old instanceof Float64Array ? Float64Array.from(next as number[]) : next;
  return { nrow: t.nrow, cols: { ...t.cols, [col]: typed } };
}

describe('schema 2: the six stages (2026-10-28)', () => {
  const fx = buildSectorFixture();
  type Tbl = { nrow: number; cols: Record<string, unknown> };
  const meta = fx.chunks['sector-meta'] as Record<string, Tbl>;
  const items = meta['dims.items'];
  const slugs = items.cols.slug as (string | null)[];
  const rowOf = (slug: string) => slugs.indexOf(slug);
  const metaErrors = (patch: Record<string, Tbl>) => checkChunk('sector-meta', { ...meta, ...patch }).errors;

  it('stages, labels, shorts and the policy stage mask are the six of lib/stages.ts, in chain order', () => {
    expect(SECTOR_SCHEMA_VERSION).toBe(2);
    expect([...STAGE_SLUGS]).toEqual([...SUPPLY_STAGE_SLUGS]);
    expect(STAGE_SLUGS.map((s) => STAGE_LABELS[s])).toEqual([...SUPPLY_STAGES]);
    expect(STAGE_SLUGS.map((s) => STAGE_SHORTS[s])).toEqual(['Ext', 'Ref', 'Com', 'Asm', 'Dep', 'Rec']);
    expect([...STAGE_MASK_ORDER]).toEqual(['extraction', 'refining', 'components', 'assembly', 'deployment', 'recovery']);
    expect(stageSlugOf('Refining')).toBe('refining');
    expect(stageSlugOf('Midstream')).toBeNull();
    // the ILO wage activity of each stage (R SECTOR_ACTIVITY_OF_STAGE)
    expect(STAGE_SLUGS.map((s) => ACTIVITY_OF_STAGE[s])).toEqual(['mining-utilities', 'manufacturing', 'manufacturing', 'manufacturing', 'construction', 'mining-utilities']);
    expect(MINERAL_REDIRECTS.uranium).toBe('/sector/nuclear/extraction');
  });

  it('a schema-1 manifest or a 33-row coverage glyph is refused', () => {
    expect(sectorManifestProblems({ ...fx.manifest, schema_version: 1 })).toContain('schema_version is 1, expected 2');
    const problems = sectorManifestProblems({ ...fx.manifest, coverage_glyph: fx.manifest.coverage_glyph.slice(0, 33) });
    expect(problems).toContain('coverage_glyph has 33 rows, expected 66');
    expect(sectorManifestProblems({ ...fx.manifest, coverage_glyph: fx.manifest.coverage_glyph.map((g, i) => (i ? g : { ...g, stage: 'upstream' })) }).some((p) => p.startsWith('coverage_glyph[0]'))).toBe(true);
    expect(fx.manifest.coverage_glyph).toHaveLength(66);
  });

  it('dims.items: every headline cell of the 11 x 6 grid, mineral_stage rows of a mineral, a stage only on cells and mineral stages', () => {
    expect(metaErrors({})).toEqual([]);
    // a headline cell missing (its row turned into another axis)
    const gone = rowOf('wind-recovery');
    const noCell = withCol(withCol(withCol(withCol(items, 'axis', (xs) => xs.map((x, i) => (i === gone ? 'policy_tag' : x))), 'stage', (xs) => xs.map((x, i) => (i === gone ? null : x))), 'tech', (xs) => xs.map((x, i) => (i === gone ? null : x))), 'headline', (xs) => xs.map((x, i) => (i === gone ? 0 : x)));
    expect(metaErrors({ 'dims.items': noCell })).toContain('dims.items lacks 1 headline cells of the 11 x 6 grid (e.g. wind-recovery)');
    // a mineral_stage row whose slug is not <mineral>-<its stage>
    const ms = rowOf('lithium-refining');
    expect(metaErrors({ 'dims.items': withCol(items, 'slug', (xs) => xs.map((x, i) => (i === ms ? 'lithium-recovery' : x))) }).some((e) => e.startsWith('dims.items mineral_stage rows need'))).toBe(true);
    expect(metaErrors({ 'dims.items': withCol(items, 'tech', (xs) => xs.map((x, i) => (i === ms ? 'Batteries' : x))) }).some((e) => e.startsWith('dims.items mineral_stage rows need'))).toBe(true);
    // a stage on a fuel row
    const fuel = slugs.findIndex((s, i) => s === 'oil' && (items.cols.axis as string[])[i] === 'fuel');
    expect(metaErrors({ 'dims.items': withCol(items, 'stage', (xs) => xs.map((x, i) => (i === fuel ? 'Extraction' : x))) }).some((e) => e.startsWith('dims.items: only cell and mineral_stage rows carry a stage'))).toBe(true);
    // an old stage label is not a stage
    const cell = rowOf('batteries-assembly');
    expect(metaErrors({ 'dims.items': withCol(items, 'stage', (xs) => xs.map((x, i) => (i === cell ? 'Midstream' : x))) }).length).toBeGreaterThan(0);
  });

  it('trade.excluded is per technology: a row naming another technology is an error', () => {
    const ref = refSizes(validateChunk('sector-meta', fx.chunks['sector-meta']));
    const trade = fx.chunks['sector-trade-batteries'] as Record<string, Tbl>;
    expect(checkChunk('sector-trade-batteries', trade, ref).errors).toEqual([]);
    expect((trade['trade.excluded'].cols.tech as string[]).every((t) => t === 'Batteries')).toBe(true);
    const bad = { ...trade, 'trade.excluded': withCol(trade['trade.excluded'], 'tech', (xs) => xs.map(() => 'Solar')) };
    expect(checkChunk('sector-trade-batteries', bad, ref).errors.some((e) => e.startsWith("trade.excluded.tech is not the chunk's technology (Batteries)"))).toBe(true);
  });

  it("fdiCellSlug: the cells of fdi.stages read the first stage's cell; other stages none", () => {
    const st = ['Components', 'Assembly'];
    expect(fdiCellSlug('batteries-assembly', st)).toBe('batteries-components');
    expect(fdiCellSlug('wind-components', st)).toBe('wind-components');
    expect(fdiCellSlug('batteries-refining', st)).toBeNull();
    expect(fdiCellSlug('batteries-assembly', [])).toBeNull();
    expect(fdiCellSlug('batteries-midstream', st)).toBeNull();
  });

  it('budgets measured on the six-stage export (E4): facts 670 kB, trade 520 kB a chunk, meta 150 kB, combined 32.5 MiB', () => {
    expect(SECTOR_BUDGET_GZ['sector-facts']).toBe(670 * 1024);
    expect(SECTOR_BUDGET_GZ['sector-trade']).toBe(520 * 1024);
    expect(SECTOR_BUDGET_GZ['sector-meta']).toBe(150 * 1024);
    expect(SECTOR_BUDGET_GZ.sector_total).toBe(9 * 1024 * 1024);
    expect(SECTOR_BUDGET_GZ.combined_total).toBe(32.5 * 1024 * 1024);
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
  it('is six-stage: schema 2, a 66-row glyph, fdi.stages Components and Assembly (an old stage list is refused)', () => {
    const m = JSON.parse(readFileSync(manifestPath, 'utf8')) as SectorManifest;
    expect(m.schema_version).toBe(2);
    expect(m.coverage_glyph).toHaveLength(66);
    expect(m.coverage_glyph[0]).toMatchObject({ tech: 'batteries', stage: 'extraction' });
    expect(m.coverage_glyph[65]).toMatchObject({ tech: 'wind', stage: 'recovery' });
    const meta = validateChunk('sector-meta', readChunk(m.chunks.find((c) => c.name === 'sector-meta')!.file)) as SectorMetaChunk;
    const fdi = m.chunks.find((c) => c.name === 'sector-fdi');
    if (fdi) {
      const d = readChunk(fdi.file);
      expect(d.flat['fdi.stages']).toEqual(['Components', 'Assembly']);
      const res = checkChunk('sector-fdi', { ...d.flat, 'fdi.stages': ['Midstream'] }, refSizes(meta));
      expect(res.errors).toContain('fdi.stages must list distinct supply-chain stages');
    }
    const axes = new Set(meta['dims.items'].cols.axis);
    expect(axes.has('mineral_stage')).toBe(true);
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
