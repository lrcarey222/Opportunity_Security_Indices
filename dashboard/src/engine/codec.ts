// OSB1 decoder (spec_react.md 4.2; R/dashboard/dash_osb.R writes it). Pure: bytes in (already
// inflated), object tree out. Typed arrays are views on the buffer where the layout allows it
// (f64, i32, bool), so a decoded chunk costs no copies; dict16, bits, fct and const nodes are
// expanded.
//
// Decoded values:
//   f64 / dict16 / const f64   Float64Array (NA = NaN)
//   i32 / const i32            Int32Array (NA = NA_INT)
//   bool / bits / const bool   Uint8Array (0 / 1, NA = 255)
//   fct / strv / const chr     (string | null)[]
//   json                       the JSON value (R conventions: named vectors are objects, character
//                              vectors arrays, length-1 numbers / logicals scalars, NA null)
//   table                      Table: { nrow, cols: { name: column } } (columns as above)
// A node with `dim: true` (an R matrix / array) decodes to NDArray { shape, data }.

import { EngineError, NA_BOOL, NA_INT } from './numeric.ts';

export type Column = Float64Array | Int32Array | Uint8Array | (string | null)[];

export interface Table {
  nrow: number;
  cols: Record<string, Column>;
}

export interface NDArray<T extends Column = Column> {
  shape: number[];
  data: T;
}

export type OsbValue = Column | NDArray | Table | unknown;

export interface OsbHeader {
  format: string;
  version: number;
  schema_version: number;
  kind: 'chunk' | 'fixture';
  name: string;
  [k: string]: unknown;
}

export interface OsbDecoded {
  header: OsbHeader;
  /** flat map: dotted path -> value */
  flat: Record<string, OsbValue>;
  /** nested object tree (paths split on '.') */
  objects: Record<string, unknown>;
}

interface Node {
  t: string;
  shape?: number[];
  dim?: boolean;
  n?: number;
  off?: number;
  dict?: Node;
  levels?: (string | null)[];
  codes?: Node;
  v?: unknown;
  dtype?: string;
  nrow?: number;
  cols?: Record<string, Node>;
  r?: unknown;
}

export function isTable(x: unknown): x is Table {
  return typeof x === 'object' && x !== null && 'nrow' in x && 'cols' in x;
}

export function isNDArray(x: unknown): x is NDArray {
  return typeof x === 'object' && x !== null && 'shape' in x && 'data' in x && !('nrow' in x);
}

const MAGIC = [0x4f, 0x53, 0x42, 0x31]; // "OSB1"

/** True for gzip bytes (1f 8b). */
export function isGzip(bytes: Uint8Array): boolean {
  return bytes.length >= 2 && bytes[0] === 0x1f && bytes[1] === 0x8b;
}

/** Decode an OSB1 container (inflated bytes). */
export function decodeOsb(input: Uint8Array | ArrayBuffer): OsbDecoded {
  let bytes = input instanceof Uint8Array ? input : new Uint8Array(input);
  if (bytes.length < 8 || MAGIC.some((b, i) => bytes[i] !== b)) {
    throw new EngineError('DATA_FORMAT', 'Not an OSB1 container (inflate gzip first).');
  }
  if (bytes.byteOffset % 8 !== 0) bytes = bytes.slice(); // typed-array views need 8-byte alignment
  const buf = bytes.buffer;
  const base = bytes.byteOffset;
  const dv = new DataView(buf, base, bytes.length);
  const H = dv.getUint32(4, true);
  const headerText = new TextDecoder('utf-8').decode(bytes.subarray(8, 8 + H));
  const header = JSON.parse(headerText) as OsbHeader & { objects: Record<string, Node> };
  if (header.format !== 'osb' || header.version !== 1) {
    throw new EngineError('DATA_FORMAT', `Unsupported OSB format / version (${header.format} ${header.version}).`);
  }
  const dataStart = base + 8 + H + ((8 - (H % 8)) % 8);

  const prod = (s: number[] | undefined) => (s ?? []).reduce((a, b) => a * b, 1);
  const shapeOf = (node: Node): number[] => (Array.isArray(node.shape) ? node.shape : [node.shape as unknown as number]);

  function readVec(node: Node): Column {
    const shape = shapeOf(node);
    const n = shape.length ? prod(shape) : 0;
    switch (node.t) {
      case 'f64':
        return new Float64Array(buf, dataStart + (node.off as number), n);
      case 'i32':
        return new Int32Array(buf, dataStart + (node.off as number), n);
      case 'bool':
        return new Uint8Array(buf, dataStart + (node.off as number), n);
      case 'bits': {
        const src = new Uint8Array(buf, dataStart + (node.off as number), Math.ceil(n / 8));
        const out = new Uint8Array(n);
        for (let i = 0; i < n; i++) out[i] = (src[i >> 3] >> (i & 7)) & 1;
        return out;
      }
      case 'dict16': {
        const codes = new Uint16Array(buf.slice(dataStart + (node.off as number), dataStart + (node.off as number) + 2 * n));
        const dict = readVec(node.dict as Node) as Float64Array;
        const out = new Float64Array(n);
        for (let i = 0; i < n; i++) {
          const c = codes[i];
          out[i] = c === 65535 ? NaN : dict[c];
        }
        return out;
      }
      case 'fct': {
        const lev = (node.levels ?? []) as (string | null)[];
        const codes = readVec(node.codes as Node) as Int32Array;
        const out: (string | null)[] = new Array(n);
        for (let i = 0; i < n; i++) {
          const c = codes[i];
          out[i] = c >= 0 ? (lev[c] ?? null) : null;
        }
        return out;
      }
      case 'strv': {
        const v = (node.v ?? []) as (string | null)[];
        return Array.isArray(v) ? v.slice() : [v];
      }
      case 'const': {
        const v = node.v === undefined ? null : node.v;
        switch (node.dtype) {
          case 'f64':
            return new Float64Array(n).fill(v === null ? NaN : (v as number));
          case 'i32':
            return new Int32Array(n).fill(v === null ? NA_INT : (v as number));
          case 'bool':
            return new Uint8Array(n).fill(v === null ? NA_BOOL : v ? 1 : 0);
          case 'chr':
            return new Array<string | null>(n).fill(v as string | null);
          default:
            throw new EngineError('DATA_FORMAT', `Unknown const dtype ${node.dtype}.`);
        }
      }
      default:
        throw new EngineError('DATA_FORMAT', `Unknown OSB node type ${node.t}.`);
    }
  }

  function readNode(node: Node): OsbValue {
    switch (node.t) {
      case 'json':
        return node.v === undefined ? null : node.v;
      case 'table': {
        const cols: Record<string, Column> = {};
        for (const [k, c] of Object.entries(node.cols ?? {})) cols[k] = readVec(c);
        return { nrow: node.nrow as number, cols } satisfies Table;
      }
      default: {
        const data = readVec(node);
        if (node.dim) return { shape: shapeOf(node), data } satisfies NDArray;
        return data;
      }
    }
  }

  const flat: Record<string, OsbValue> = {};
  for (const [path, node] of Object.entries(header.objects)) flat[path] = readNode(node);
  const { objects: _objects, ...rest } = header;
  void _objects;
  return { header: rest as OsbHeader, flat, objects: unflatten(flat) };
}

/** Nested object from dotted paths. */
export function unflatten(flat: Record<string, unknown>): Record<string, unknown> {
  const out: Record<string, unknown> = {};
  for (const [path, value] of Object.entries(flat)) {
    const parts = path.split('.');
    let cur = out;
    for (let i = 0; i < parts.length - 1; i++) {
      const p = parts[i];
      if (typeof cur[p] !== 'object' || cur[p] === null || isTable(cur[p]) || isNDArray(cur[p]) || ArrayBuffer.isView(cur[p])) {
        cur[p] = {};
      }
      cur = cur[p] as Record<string, unknown>;
    }
    cur[parts[parts.length - 1]] = value;
  }
  return out;
}

/** Inflate gzip bytes with the platform's DecompressionStream. */
export async function gunzipStream(bytes: Uint8Array): Promise<Uint8Array> {
  const ds = new DecompressionStream('gzip');
  const stream = new Blob([bytes as BlobPart]).stream().pipeThrough(ds);
  const ab = await new Response(stream).arrayBuffer();
  return new Uint8Array(ab);
}

/** Bytes of a fetched file -> decoded OSB; gzip is sniffed and inflated with `inflate`. */
export async function readOsb(
  bytes: Uint8Array,
  inflate: (b: Uint8Array) => Promise<Uint8Array> | Uint8Array = gunzipStream,
): Promise<OsbDecoded> {
  const raw = isGzip(bytes) ? await inflate(bytes) : bytes;
  return decodeOsb(raw);
}
