// Columnar tables (spec_react.md 6.1): { nrow, cols: { name: column } } with R column names and R
// column types: double -> Float64Array (NA = NaN), integer -> Int32Array (NA = NA_INT), logical ->
// Uint8Array (0 / 1, NA = NA_BOOL = 255), character -> (string | null)[]. Pure helpers.

import type { Column, NDArray, Table } from './codec.ts';
import { EngineError, NA_BOOL, NA_INT } from './numeric.ts';

export type { Column, NDArray, Table };
export type Str = (string | null)[];

export function table(cols: Record<string, Column>, nrow?: number): Table {
  const names = Object.keys(cols);
  const n = nrow ?? (names.length ? cols[names[0]].length : 0);
  for (const k of names) {
    if (cols[k].length !== n) throw new EngineError('TABLE', `Column ${k} has ${cols[k].length} rows, expected ${n}.`);
  }
  return { nrow: n, cols };
}

function get(t: Table, name: string): Column {
  const c = t.cols[name];
  if (c === undefined) throw new EngineError('TABLE', `Missing column ${name}.`);
  return c;
}

/** A numeric column as Float64Array (integer columns converted; NA_INT -> NaN). */
export function num(t: Table, name: string): Float64Array {
  const c = get(t, name);
  if (c instanceof Float64Array) return c;
  if (c instanceof Int32Array) return Float64Array.from(c, (v) => (v === NA_INT ? NaN : v));
  if (c instanceof Uint8Array) return Float64Array.from(c, (v) => (v === NA_BOOL ? NaN : v));
  if (Array.isArray(c) && c.every((v) => v === null)) return new Float64Array(c.length).fill(NaN);
  throw new EngineError('TABLE', `Column ${name} is not numeric.`);
}

/** An integer column (doubles truncated; NaN -> NA_INT). */
export function int(t: Table, name: string): Int32Array {
  const c = get(t, name);
  if (c instanceof Int32Array) return c;
  if (c instanceof Float64Array) return Int32Array.from(c, (v) => (v !== v ? NA_INT : v));
  throw new EngineError('TABLE', `Column ${name} is not integer.`);
}

/** A character column. */
export function str(t: Table, name: string): Str {
  const c = get(t, name);
  if (Array.isArray(c)) return c;
  if (c.length === 0) return [];
  throw new EngineError('TABLE', `Column ${name} is not character.`);
}

/** A logical column (0 / 1 / NA_BOOL). */
export function lgl(t: Table, name: string): Uint8Array {
  const c = get(t, name);
  if (c instanceof Uint8Array) return c;
  if (Array.isArray(c) && c.every((v) => v === null)) return new Uint8Array(c.length).fill(NA_BOOL);
  throw new EngineError('TABLE', `Column ${name} is not logical.`);
}

export function has(t: Table | null | undefined, name: string): boolean {
  return !!t && Object.prototype.hasOwnProperty.call(t.cols, name);
}

/** Rows idx of a column (same type). */
export function take<T extends Column>(c: T, idx: ArrayLike<number>): T {
  const n = idx.length;
  if (Array.isArray(c)) {
    const out: (string | null)[] = new Array(n);
    for (let i = 0; i < n; i++) out[i] = idx[i] < 0 ? null : c[idx[i]];
    return out as T;
  }
  if (c instanceof Float64Array) {
    const out = new Float64Array(n);
    for (let i = 0; i < n; i++) out[i] = idx[i] < 0 ? NaN : c[idx[i]];
    return out as T;
  }
  if (c instanceof Int32Array) {
    const out = new Int32Array(n);
    for (let i = 0; i < n; i++) out[i] = idx[i] < 0 ? NA_INT : c[idx[i]];
    return out as T;
  }
  const u = c as Uint8Array;
  const out = new Uint8Array(n);
  for (let i = 0; i < n; i++) out[i] = idx[i] < 0 ? NA_BOOL : u[idx[i]];
  return out as T;
}

/** Rows idx of a table (an index < 0 gives an NA row, like a non-matching join). */
export function takeRows(t: Table, idx: ArrayLike<number>, cols?: string[]): Table {
  const out: Record<string, Column> = {};
  for (const k of cols ?? Object.keys(t.cols)) out[k] = take(get(t, k), idx);
  return { nrow: idx.length, cols: out };
}

/** Columns in the given order (a subset). */
export function select(t: Table, cols: string[]): Table {
  const out: Record<string, Column> = {};
  for (const k of cols) out[k] = get(t, k);
  return { nrow: t.nrow, cols: out };
}

/** Rows where keep[i] is true. */
export function filterRows(t: Table, keep: ArrayLike<boolean | number>): Table {
  const idx: number[] = [];
  for (let i = 0; i < t.nrow; i++) if (keep[i]) idx.push(i);
  return takeRows(t, idx);
}

/** Row-bind tables with the same columns (columns of the first; missing ones filled with NA). */
export function bindRows(ts: Table[]): Table {
  const parts = ts.filter((t) => t && t.nrow >= 0);
  if (!parts.length) return { nrow: 0, cols: {} };
  const names: string[] = [];
  for (const t of parts) for (const k of Object.keys(t.cols)) if (!names.includes(k)) names.push(k);
  const n = parts.reduce((a, t) => a + t.nrow, 0);
  const out: Record<string, Column> = {};
  for (const k of names) {
    const proto = parts.find((t) => k in t.cols)!.cols[k];
    let col: Column;
    if (Array.isArray(proto)) col = new Array<string | null>(n).fill(null);
    else if (proto instanceof Float64Array) col = new Float64Array(n).fill(NaN);
    else if (proto instanceof Int32Array) col = new Int32Array(n).fill(NA_INT);
    else col = new Uint8Array(n).fill(NA_BOOL);
    let off = 0;
    for (const t of parts) {
      const c = t.cols[k];
      if (c) {
        if (Array.isArray(col)) for (let i = 0; i < t.nrow; i++) (col as Str)[off + i] = (c as Str)[i];
        else (col as Float64Array).set(c as Float64Array, off);
      }
      off += t.nrow;
    }
    out[k] = col;
  }
  return { nrow: n, cols: out };
}

/** Row objects for the UI (never used inside the engine). */
export function toRows(t: Table): Record<string, string | number | boolean | null>[] {
  const keys = Object.keys(t.cols);
  const rows: Record<string, string | number | boolean | null>[] = new Array(t.nrow);
  for (let i = 0; i < t.nrow; i++) {
    const r: Record<string, string | number | boolean | null> = {};
    for (const k of keys) {
      const c = t.cols[k];
      if (Array.isArray(c)) r[k] = c[i];
      else if (c instanceof Float64Array) r[k] = c[i] !== c[i] ? null : c[i];
      else if (c instanceof Int32Array) r[k] = c[i] === NA_INT ? null : c[i];
      else r[k] = c[i] === NA_BOOL ? null : c[i] === 1;
    }
    rows[i] = r;
  }
  return rows;
}

/** Key strings of rows (columns joined with "\r"; NA as the string "NA" like paste()). */
export function rowKeys(cols: ArrayLike<unknown>[], n: number): string[] {
  const out: string[] = new Array(n);
  for (let i = 0; i < n; i++) {
    let s = '';
    for (let j = 0; j < cols.length; j++) {
      const v = cols[j][i];
      s += (j ? '\r' : '') + (v === null || v === undefined ? 'NA' : String(v));
    }
    out[i] = s;
  }
  return out;
}

/** match(x, table): 0-based index of the first match, -1 when none. */
export function matchIdx(x: ArrayLike<string | null>, tbl: ArrayLike<string | null>): Int32Array {
  const m = new Map<string | null, number>();
  for (let i = tbl.length - 1; i >= 0; i--) m.set(tbl[i], i);
  const out = new Int32Array(x.length);
  for (let i = 0; i < x.length; i++) {
    const k = m.get(x[i]);
    out[i] = k === undefined ? -1 : k;
  }
  return out;
}

/** Logical vector from booleans. */
export function lglOf(xs: ArrayLike<boolean | null>): Uint8Array {
  const out = new Uint8Array(xs.length);
  for (let i = 0; i < xs.length; i++) out[i] = xs[i] === null ? NA_BOOL : xs[i] ? 1 : 0;
  return out;
}

/** Element of an NDArray matrix [i, j] (0-based, column-major). */
export function at2<T extends Column>(a: NDArray<T>, i: number, j: number): T[number] {
  return a.data[i + a.shape[0] * j] as T[number];
}

export function asNDArray<T extends Column>(x: unknown, name: string): NDArray<T> {
  if (typeof x === 'object' && x !== null && 'shape' in x && 'data' in x) return x as NDArray<T>;
  throw new EngineError('BUNDLE', `${name} is not a matrix / array.`);
}
