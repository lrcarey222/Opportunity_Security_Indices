// Fixture loading and comparison helpers for the parity tests (spec_react.md 6.9). Node only.

import { existsSync, readFileSync } from 'node:fs';
import { join } from 'node:path';
import { gunzipSync } from 'node:zlib';
import { addChunk, assembleBundle } from '../../bundle.ts';
import { decodeOsb, isNDArray, isTable, type OsbDecoded } from '../../codec.ts';
import { NA_BOOL, NA_INT } from '../../numeric.ts';
import { makeParams } from '../../params.ts';
import type { Bundle, Column, Manifest, NDArray, Table } from '../../types.ts';

export function readOsbFile(path: string): OsbDecoded {
  let b: Uint8Array = readFileSync(path);
  if (b[0] === 0x1f && b[1] === 0x8b) b = gunzipSync(b);
  const copy = new Uint8Array(b.length);
  copy.set(b);
  return decodeOsb(copy);
}

/** A deploy-format bundle directory (manifest.json + chunks) -> Bundle with every chunk loaded. */
export function loadWebBundle(dir: string, chunks: string[] = ['core', 'vars', 'partner', 'network']): Bundle {
  const manifest = JSON.parse(readFileSync(join(dir, 'manifest.json'), 'utf8')) as Manifest;
  const file = (name: string) => manifest.chunks.find((c) => c.name === name)?.file;
  const core = readOsbFile(join(dir, file('core') as string));
  const bundle = assembleBundle(manifest, core, (b) => makeParams(null, b));
  for (const name of chunks) {
    if (name === 'core') continue;
    const f = file(name);
    if (f) addChunk(bundle, name, readOsbFile(join(dir, f)));
  }
  return bundle;
}

const fixtureCache = new Map<string, Record<string, unknown>>();

/** A fixture scenario file with its references resolved: a leaf that scripts/86 found identical()
 * to the reference scenario's is stored as {ref_scenario, ref_path} (dash_fixture_refs()). */
export function loadFixture(dir: string, file: string): Record<string, unknown> {
  const path = join(dir, file);
  const hit = fixtureCache.get(path);
  if (hit) return hit;
  const objects = readOsbFile(path).objects as Record<string, unknown>;
  const one = (v: unknown) => (Array.isArray(v) ? v[0] : v);
  const resolve = (node: unknown): unknown => {
    if (node === null || typeof node !== 'object' || ArrayBuffer.isView(node) || Array.isArray(node) || isTable(node) || isNDArray(node)) return node;
    const o = node as Record<string, unknown>;
    if ('ref_scenario' in o && 'ref_path' in o) {
      const ref = loadFixture(dir, `${String(one(o.ref_scenario))}.osb`);
      const target = get(ref, String(one(o.ref_path)));
      if (target === undefined) throw new Error(`${file}: unresolved reference to ${String(one(o.ref_scenario))}:${String(one(o.ref_path))}`);
      return target;
    }
    for (const k of Object.keys(o)) o[k] = resolve(o[k]);
    return o;
  };
  resolve(objects);
  // keep only the reference scenario (S00) cached: the others are read once per suite
  if (file === 'S00.osb') fixtureCache.set(path, objects);
  return objects;
}

export interface FixtureScenario {
  id: string;
  label: string;
  dials: Record<string, number>;
  advanced: Record<string, unknown> | unknown[];
  file: string;
  layers: string[];
  options: Record<string, unknown>;
}

export interface FixtureIndex {
  set: string;
  bundle_id: string;
  scenarios: FixtureScenario[];
}

export function loadIndex(dir: string): FixtureIndex | null {
  const p = join(dir, 'index.json');
  if (!existsSync(p)) return null;
  return JSON.parse(readFileSync(p, 'utf8')) as FixtureIndex;
}

export function get(obj: unknown, path: string): unknown {
  let cur = obj as Record<string, unknown> | undefined;
  for (const part of path.split('.')) {
    if (cur === undefined || cur === null) return undefined;
    cur = cur[part] as Record<string, unknown> | undefined;
  }
  return cur;
}

// ---- Comparisons ------------------------------------------------------------------------------------

export interface Diff {
  path: string;
  problem: string;
}

const isNum = (c: Column): c is Float64Array => c instanceof Float64Array;

function colToNumbers(c: Column): Float64Array | null {
  if (c instanceof Float64Array) return c;
  if (c instanceof Int32Array) return Float64Array.from(c, (v) => (v === NA_INT ? NaN : v));
  if (c instanceof Uint8Array) return Float64Array.from(c, (v) => (v === NA_BOOL ? NaN : v));
  if (Array.isArray(c) && c.every((v) => v === null)) return new Float64Array(c.length).fill(NaN);
  return null;
}

/** Compare two columns: numbers within tol with identical NA patterns, everything else exact. */
export function compareColumn(path: string, a: Column | undefined, e: Column, tol: number, diffs: Diff[]): number {
  if (a === undefined) {
    diffs.push({ path, problem: 'missing column' });
    return Infinity;
  }
  if (a.length !== e.length) {
    diffs.push({ path, problem: `length ${a.length} vs ${e.length}` });
    return Infinity;
  }
  const ea = Array.isArray(e) && !e.every((v) => v === null);
  if (ea || (Array.isArray(a) && !a.every((v) => v === null))) {
    const as = a as (string | null)[];
    const es = e as (string | null)[];
    let bad = 0;
    let first = -1;
    for (let i = 0; i < es.length; i++) {
      if (as[i] !== es[i]) {
        bad++;
        if (first < 0) first = i;
      }
    }
    if (bad) diffs.push({ path, problem: `${bad} strings differ, first [${first}]: ${JSON.stringify(as[first])} vs ${JSON.stringify(es[first])}` });
    return bad ? Infinity : 0;
  }
  const an = colToNumbers(a);
  const en = colToNumbers(e);
  if (!an || !en) {
    diffs.push({ path, problem: 'type mismatch' });
    return Infinity;
  }
  const exact = !(isNum(e) || isNum(a)); // integers and logicals compare exactly
  let naMis = 0;
  let maxd = 0;
  let worst = -1;
  for (let i = 0; i < en.length; i++) {
    const x = an[i];
    const y = en[i];
    const xn = x !== x;
    const yn = y !== y;
    if (xn || yn) {
      if (xn !== yn) {
        naMis++;
        if (worst < 0) worst = i;
      }
      continue;
    }
    const d = Math.abs(x - y);
    if (d > maxd) {
      maxd = d;
      if (d > (exact ? 0 : tol)) worst = i;
    }
  }
  if (naMis) diffs.push({ path, problem: `${naMis} NA mismatches, first [${worst}]: ${an[worst]} vs ${en[worst]}` });
  if (maxd > (exact ? 0 : tol)) diffs.push({ path, problem: `max |diff| ${maxd.toExponential(3)} > ${exact ? 0 : tol} at [${worst}]: ${an[worst]} vs ${en[worst]}` });
  return maxd;
}

/** Compare a table with the expected one (the expected columns, in any order). */
export function compareTable(path: string, a: Table | undefined, e: Table, tol: number, diffs: Diff[], opts: { ignore?: string[] } = {}): number {
  if (!a) {
    diffs.push({ path, problem: 'missing table' });
    return Infinity;
  }
  if (a.nrow !== e.nrow) {
    diffs.push({ path, problem: `nrow ${a.nrow} vs ${e.nrow}` });
    return Infinity;
  }
  let maxd = 0;
  for (const [k, c] of Object.entries(e.cols)) {
    if (opts.ignore?.includes(k)) continue;
    maxd = Math.max(maxd, compareColumn(`${path}$${k}`, a.cols[k], c, tol, diffs));
  }
  return maxd;
}

export function compareArray(path: string, a: Column, e: unknown, tol: number, diffs: Diff[]): number {
  const ev = isNDArray(e) ? (e as NDArray).data : (e as Column);
  return compareColumn(path, a, ev, tol, diffs);
}

export function asTable(x: unknown): Table {
  if (!isTable(x)) throw new Error('not a table');
  return x;
}

export function report(diffs: Diff[]): string {
  return diffs.map((d) => `${d.path}: ${d.problem}`).join('\n');
}
