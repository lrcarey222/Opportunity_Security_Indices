// Node-only loader of the shipped bundles (dashboard/public/data) for the QC layer and the review-data builder
// (docs/QC_SYSTEM_DESIGN.md 3, 6.1). It reads the manifests and every JSON document the QC layer decodes, hashes
// each file it reads (sha256 of the bytes on disk) and records its stamps, and returns RawBundles for
// qc/bundles.ts decodeBundles(). Never imported by browser code: node:fs and node:crypto are reached through
// process.getBuiltinModule (Node >= 22.3) so the app's typecheck, which has no Node types, still covers this file.

import type { BundleFileStamp } from '../../model.ts';
import type { RawBundles } from '../bundles.ts';
import { memosManifestSha256 } from '../../release.ts';

interface FsLike {
  readFileSync(path: string): Uint8Array;
  existsSync(path: string): boolean;
}
interface HashLike {
  update(data: Uint8Array | string): HashLike;
  digest(enc: 'hex'): string;
}
interface CryptoLike {
  createHash(alg: string): HashLike;
}
interface ProcessLike {
  getBuiltinModule(id: string): unknown;
}

const proc = (): ProcessLike => {
  const p = (globalThis as unknown as { process?: ProcessLike }).process;
  if (!p || typeof p.getBuiltinModule !== 'function') throw new Error('loadBundles: Node >= 22.3 is required (process.getBuiltinModule)');
  return p;
};
const fs = (): FsLike => proc().getBuiltinModule('node:fs') as FsLike;
const crypto = (): CryptoLike => proc().getBuiltinModule('node:crypto') as CryptoLike;

/** Hex sha256 of bytes or text (synchronous: node:crypto). */
export function sha256Sync(data: Uint8Array | string): string {
  return crypto().createHash('sha256').update(data).digest('hex');
}

const join = (...parts: string[]): string => parts.map((p, i) => (i === 0 ? p.replace(/[\\/]+$/, '') : p.replace(/^[\\/]+|[\\/]+$/g, ''))).join('/');
const isObj = (v: unknown): v is Record<string, unknown> => typeof v === 'object' && v !== null && !Array.isArray(v);
const strOrNull = (v: unknown): string | null => (typeof v === 'string' && v !== '' ? v : null);

export interface LoadedFile {
  /** path relative to the data dir, forward slashes (e.g. "posture/manifest.json") */
  rel: string;
  text: string;
  json: unknown;
  stamp: BundleFileStamp;
}

export interface LoadOptions {
  /** read the 214 posture detail documents (default true) */
  details?: boolean;
  /** restrict the posture details to these ISO3 codes (default: all in the manifest) */
  detailIsos?: readonly string[] | null;
  /** read the memo JSON documents named by the partnerships manifest (default true) */
  memos?: boolean;
}

export interface LoadResult {
  raw: RawBundles;
  /** problems reading files (a missing file is a QC finding, never a crash) */
  problems: string[];
  /** rel path -> exact text read (the memos part of the release key hashes the partnerships manifest text) */
  texts: Record<string, string>;
}

/**
 * Read the shipped bundles under dataDir (dashboard/public/data). Every file read is hashed and stamped: created and
 * git_commit are the document's own fields where it has them (detail documents carry created only; AIB, memo and
 * scenario documents carry no commit), else null.
 */
export function loadRawBundles(dataDir: string, opts: LoadOptions = {}): LoadResult {
  const f = fs();
  const problems: string[] = [];
  const files: BundleFileStamp[] = [];
  const texts: Record<string, string> = {};
  const dec = new TextDecoder('utf-8');
  const read = (rel: string): LoadedFile | null => {
    const path = join(dataDir, rel);
    if (!f.existsSync(path)) {
      problems.push(`missing file: ${rel}`);
      return null;
    }
    const bytes = f.readFileSync(path);
    const text = dec.decode(bytes);
    let json: unknown = null;
    try {
      json = JSON.parse(text);
    } catch (e) {
      problems.push(`${rel} is not valid JSON: ${e instanceof Error ? e.message : String(e)}`);
    }
    const created = isObj(json) ? (strOrNull(json.created) ?? strOrNull(json.generated_at) ?? strOrNull(isObj(json.meta) ? json.meta.built : null)) : null;
    const stamp: BundleFileStamp = { file: rel, sha256: sha256Sync(bytes), created, git_commit: isObj(json) ? strOrNull(json.git_commit) : null };
    files.push(stamp);
    texts[rel] = text;
    return { rel, text, json, stamp };
  };

  // posture
  let posture: RawBundles['posture'] = null;
  const pm = read('posture/manifest.json');
  if (pm && isObj(pm.json)) {
    const coreFile = strOrNull(pm.json.core);
    const core = coreFile ? read(`posture/${coreFile}`) : null;
    if (!coreFile) problems.push('posture/manifest.json names no core file');
    const details: Record<string, unknown> = {};
    if (opts.details !== false && isObj(pm.json.details)) {
      const want = opts.detailIsos ? new Set(opts.detailIsos) : null;
      for (const [iso, file] of Object.entries(pm.json.details).sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0))) {
        if (want && !want.has(iso)) continue;
        if (typeof file !== 'string') {
          problems.push(`posture/manifest.json details.${iso} is not a file name`);
          continue;
        }
        const d = read(`posture/${file}`);
        if (d) details[iso] = d.json;
      }
    }
    posture = { manifest: pm.json, core: core?.json ?? null, details };
  }

  // AIB
  let aib: RawBundles['aib'] = null;
  const am = read('aib/manifest.json');
  if (am && isObj(am.json)) {
    const fl = isObj(am.json.files) ? am.json.files : {};
    const sc = isObj(fl.scenarios) ? fl.scenarios : {};
    const docFile = strOrNull(fl.compacts);
    const doc = docFile ? read(`aib/${docFile}`) : null;
    if (!docFile) problems.push('aib/manifest.json names no compacts file');
    const managedFile = strOrNull(sc.managed);
    const baselineFile = strOrNull(sc.baseline);
    const managed = managedFile ? read(`aib/${managedFile}`) : null;
    const baseline = baselineFile ? read(`aib/${baselineFile}`) : null;
    aib = { manifest: am.json, doc: doc?.json ?? null, managed: managed?.json ?? null, baseline: baseline?.json ?? null };
  }

  // partnership memos (pairs and compacts named by the manifest)
  let partnerships: RawBundles['partnerships'] = null;
  const mm = read('partnerships/manifest.json');
  if (mm) {
    const memos: Record<string, unknown> = {};
    if (opts.memos !== false && isObj(mm.json)) {
      for (const group of ['pairs', 'compacts'] as const) {
        const g = mm.json[group];
        if (!isObj(g)) continue;
        for (const id of Object.keys(g).sort()) {
          const e = g[id];
          const file = isObj(e) ? strOrNull(e.json) : null;
          if (!file) {
            problems.push(`partnerships/manifest.json ${group}.${id} names no json file`);
            continue;
          }
          const d = read(`partnerships/${file}`);
          if (d) memos[id] = d.json;
        }
      }
    }
    // the memos stamp hashes the manifest text LF-normalised (release.ts memosManifestSha256, the browser's function),
    // so a CRLF checkout and the Pages copy give the same release ID
    partnerships = { manifest: mm.json, manifestSha256: memosManifestSha256(mm.text), memos };
  }

  const cm = read('country/manifest.json');
  const sm = read('sector/manifest.json');
  const em = read('manifest.json');
  return {
    raw: {
      posture,
      aib,
      partnerships,
      country: cm ? { manifest: cm.json } : null,
      sector: sm ? { manifest: sm.json } : null,
      strategic: em ? { manifest: em.json } : null,
      files,
    },
    problems,
    texts,
  };
}

/** Read a UTF-8 text file (for the CLIs' YAML and config reads). Null when absent. */
export function readTextFile(path: string): string | null {
  const f = fs();
  if (!f.existsSync(path)) return null;
  return new TextDecoder('utf-8').decode(f.readFileSync(path));
}
