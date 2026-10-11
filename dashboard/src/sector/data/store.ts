// SectorStore (spec_sector.md 8.10): fetches data/sector/manifest.json (no-cache) and checks it with
// isSectorManifest(); fetches chunks with the default cache (they are immutable); sniffs gzip and
// inflates it (DecompressionStream, fflate where that is missing, as the worker does); decodes with
// engine/codec.ts; validates with validateChunk() (non-meta chunks against refSizes(meta)); caches the
// decoded chunks for the page life. Concurrent requests share one promise; a failure is retryable.
// Main thread, never the engine worker. No React here: SectorDataProvider adapts it.

import { decodeOsb, gunzipStream, isGzip, type OsbDecoded } from '../../engine/codec.ts';
import { fetchSectorChunkBytes, loadSectorManifest } from '../../shell/sectorManifest.ts';
import {
  chunkInfo, refSizes, SECTOR_DATA_DIR, SECTOR_MANIFEST_FILE, SectorContractError, sectorManifestProblems, validateChunk,
  type ChunkFor, type SectorChunkName, type SectorManifest, type SectorMetaChunk,
} from '../contract.ts';

export type LoadStatus = 'idle' | 'loading' | 'ready' | 'error';

/** Why a load failed: no bundle at all, a chunk the bundle does not have, a bad file, or the network. */
export type LoadErrorCode = 'NO_DATA' | 'NOT_IN_BUNDLE' | 'CONTRACT' | 'FORMAT' | 'NETWORK';

export class SectorLoadError extends Error {
  readonly code: LoadErrorCode;
  constructor(code: LoadErrorCode, message: string) {
    super(message);
    this.name = 'SectorLoadError';
    this.code = code;
  }
}

export interface LoadState<T> {
  status: LoadStatus;
  data: T | null;
  error: SectorLoadError | null;
  /** gzip bytes from the manifest (chunks) */
  bytes: number | null;
}

/** Where the bytes come from. The default reads over HTTP; tests pass decoded objects directly. */
export interface SectorSource {
  /** the parsed manifest.json; throws SectorLoadError('NO_DATA') when it does not exist */
  manifest(): Promise<unknown>;
  /** the decoded objects of a chunk file (relative to the sector data directory) */
  chunk(file: string): Promise<OsbDecoded | Record<string, unknown>>;
}

type FetchLike = (url: string, init?: RequestInit) => Promise<Response>;

/** Inflate gzip: the platform's DecompressionStream, else fflate (a separate chunk). */
export async function inflateGzip(bytes: Uint8Array): Promise<Uint8Array> {
  if (typeof DecompressionStream !== 'undefined') {
    try {
      return await gunzipStream(bytes);
    } catch {
      // fall through to fflate
    }
  }
  const { gunzipSync } = await import('fflate');
  return gunzipSync(bytes);
}

/** The sector data directory under the app's base URL ("/data/sector/", "/explorer/data/sector/"). */
export function sectorDataBase(): string {
  const base = (import.meta.env?.BASE_URL as string | undefined) ?? '/';
  return `${base.endsWith('/') ? base : `${base}/`}${SECTOR_DATA_DIR}`;
}

function loadErrorOf(e: unknown, what: string): SectorLoadError {
  if (e instanceof SectorLoadError) return e;
  const msg = String((e as Error)?.message ?? e);
  if (/HTTP 404/.test(msg) || e instanceof SyntaxError) return new SectorLoadError('NO_DATA', `No sector data: ${what} does not exist.`);
  if (/manifest:/.test(msg) && /newer app/.test(msg)) return new SectorLoadError('CONTRACT', 'This sector data build needs a newer app.');
  if (/Sector data manifest: (?!HTTP)/.test(msg)) return new SectorLoadError('CONTRACT', msg);
  return new SectorLoadError('NETWORK', `${what} could not be fetched (${msg}).`);
}

/**
 * The shell's shared fetchers (src/shell/sectorManifest.ts): Home's idle prefetch and this store use
 * the same promises, so a prefetched manifest or chunk is never requested twice. The manifest is
 * fetched with cache 'no-cache', chunks with the default cache.
 */
export function shellSource(opts: { inflate?: (b: Uint8Array) => Promise<Uint8Array> | Uint8Array } = {}): SectorSource {
  const inflate = opts.inflate ?? inflateGzip;
  return {
    async manifest() {
      try {
        return await loadSectorManifest();
      } catch (e) {
        throw loadErrorOf(e, 'data/sector/manifest.json');
      }
    },
    async chunk(file) {
      let bytes: Uint8Array;
      try {
        bytes = await fetchSectorChunkBytes(file);
      } catch (e) {
        throw loadErrorOf(e, file);
      }
      try {
        return decodeOsb(isGzip(bytes) ? await inflate(bytes) : bytes);
      } catch (e) {
        throw new SectorLoadError('FORMAT', `${file} is not a readable OSB1 file (${String((e as Error)?.message ?? e)}).`);
      }
    },
  };
}

/** A plain HTTP source with an injectable fetch (tests, tools). */
export function httpSource(base = sectorDataBase(), opts: { fetch?: FetchLike; inflate?: (b: Uint8Array) => Promise<Uint8Array> | Uint8Array } = {}): SectorSource {
  const f: FetchLike = opts.fetch ?? ((u, i) => fetch(u, i));
  const inflate = opts.inflate ?? inflateGzip;
  return {
    async manifest() {
      let res: Response;
      try {
        res = await f(`${base}${SECTOR_MANIFEST_FILE}`, { cache: 'no-cache' });
      } catch (e) {
        throw new SectorLoadError('NETWORK', `The sector data could not be fetched (${String((e as Error)?.message ?? e)}).`);
      }
      if (res.status === 404) throw new SectorLoadError('NO_DATA', 'No sector data: data/sector/manifest.json does not exist.');
      if (!res.ok) throw new SectorLoadError('NETWORK', `The sector manifest could not be fetched (HTTP ${res.status}).`);
      const text = await res.text();
      try {
        return JSON.parse(text) as unknown;
      } catch {
        // a dev server's index.html fallback is not a manifest
        throw new SectorLoadError('NO_DATA', 'No sector data: data/sector/manifest.json is not JSON.');
      }
    },
    async chunk(file) {
      let res: Response;
      try {
        res = await f(`${base}${file}`);
      } catch (e) {
        throw new SectorLoadError('NETWORK', `${file} could not be fetched (${String((e as Error)?.message ?? e)}).`);
      }
      if (!res.ok) throw new SectorLoadError('NETWORK', `${file} could not be fetched (HTTP ${res.status}).`);
      const bytes = new Uint8Array(await res.arrayBuffer());
      try {
        return decodeOsb(isGzip(bytes) ? await inflate(bytes) : bytes);
      } catch (e) {
        throw new SectorLoadError('FORMAT', `${file} is not a readable OSB1 file (${String((e as Error)?.message ?? e)}).`);
      }
    },
  };
}

const IDLE: LoadState<never> = Object.freeze({ status: 'idle', data: null, error: null, bytes: null }) as LoadState<never>;

type AnyChunk = ChunkFor<SectorChunkName>;

export class SectorStore {
  private readonly source: SectorSource;
  private manifestS: LoadState<SectorManifest> = IDLE;
  private manifestP: Promise<SectorManifest> | null = null;
  private readonly states = new Map<SectorChunkName, LoadState<AnyChunk>>();
  private readonly promises = new Map<SectorChunkName, Promise<AnyChunk>>();
  private readonly listeners = new Set<() => void>();
  /** contract warnings per chunk (open vocabularies, expected counts) */
  readonly warnings = new Map<string, string[]>();

  constructor(source: SectorSource = shellSource()) {
    this.source = source;
  }

  subscribe = (fn: () => void): (() => void) => {
    this.listeners.add(fn);
    return () => this.listeners.delete(fn);
  };

  private emit() {
    for (const fn of [...this.listeners]) fn();
  }

  manifestState = (): LoadState<SectorManifest> => this.manifestS;

  chunkState<N extends SectorChunkName>(name: N): LoadState<ChunkFor<N>> {
    return (this.states.get(name) ?? IDLE) as LoadState<ChunkFor<N>>;
  }

  /** The manifest (fetched once; a failure is retried on the next call). */
  loadManifest(): Promise<SectorManifest> {
    if (this.manifestS.status === 'ready' && this.manifestS.data) return Promise.resolve(this.manifestS.data);
    if (this.manifestP) return this.manifestP;
    this.manifestS = { status: 'loading', data: null, error: null, bytes: null };
    this.emit();
    const p = (async () => {
      const raw = await this.source.manifest();
      const problems = sectorManifestProblems(raw);
      if (problems.length) {
        const newer = problems.includes('this sector data build needs a newer app');
        throw new SectorLoadError('CONTRACT', newer ? 'This sector data build needs a newer app.' : `The sector manifest does not match the contract: ${problems.slice(0, 4).join('; ')}`);
      }
      return raw as SectorManifest;
    })();
    this.manifestP = p;
    p.then(
      (m) => {
        this.manifestS = { status: 'ready', data: m, error: null, bytes: null };
        this.manifestP = null;
        this.emit();
      },
      (e) => {
        this.manifestS = { status: 'error', data: null, error: toLoadError(e), bytes: null };
        this.manifestP = null;
        this.emit();
      },
    );
    return p;
  }

  /** True when the bundle lists the chunk (false before the manifest loads). */
  hasChunk(name: SectorChunkName): boolean {
    const m = this.manifestS.data;
    return !!m && !!chunkInfo(m, name);
  }

  /** gzip bytes of a chunk from the manifest, or null. */
  chunkBytes(name: SectorChunkName): number | null {
    const m = this.manifestS.data;
    return m ? (chunkInfo(m, name)?.bytes_gz ?? null) : null;
  }

  /** A decoded, validated chunk (fetched once per page life; concurrent calls share one promise). */
  load<N extends SectorChunkName>(name: N): Promise<ChunkFor<N>> {
    const st = this.states.get(name);
    if (st?.status === 'ready' && st.data) return Promise.resolve(st.data as ChunkFor<N>);
    const pending = this.promises.get(name);
    if (pending) return pending as Promise<ChunkFor<N>>;
    this.set(name, { status: 'loading', data: null, error: null, bytes: this.chunkBytes(name) });
    const p = (async () => {
      const manifest = await this.loadManifest();
      const info = chunkInfo(manifest, name);
      if (!info) throw new SectorLoadError('NOT_IN_BUNDLE', `${name} is not part of this sector data build.`);
      if (this.states.get(name)?.bytes !== info.bytes_gz) this.set(name, { status: 'loading', data: null, error: null, bytes: info.bytes_gz });
      const meta = name === 'sector-meta' ? null : await this.load('sector-meta');
      const objects = await this.source.chunk(info.file);
      try {
        return validateChunk(name, objects, meta ? refSizes(meta as SectorMetaChunk) : undefined, (w) => this.warnings.set(name, w)) as ChunkFor<N>;
      } catch (e) {
        if (e instanceof SectorContractError) throw new SectorLoadError('CONTRACT', e.message);
        throw e;
      }
    })();
    this.promises.set(name, p as Promise<AnyChunk>);
    p.then(
      (data) => {
        this.promises.delete(name);
        this.set(name, { status: 'ready', data: data as AnyChunk, error: null, bytes: this.chunkBytes(name) });
      },
      (e) => {
        this.promises.delete(name);
        this.set(name, { status: 'error', data: null, error: toLoadError(e), bytes: this.chunkBytes(name) });
      },
    );
    return p;
  }

  /** Retry a failed load (the manifest, or one chunk). */
  retry(name: SectorChunkName | 'manifest'): void {
    if (name === 'manifest') {
      if (this.manifestS.status === 'error') {
        this.manifestS = IDLE;
        void this.loadManifest().catch(() => undefined);
      }
      return;
    }
    const st = this.states.get(name);
    if (st?.status === 'error') {
      this.states.delete(name);
      if (this.manifestS.status === 'error') this.manifestS = IDLE;
      void this.load(name).catch(() => undefined);
    }
  }

  private set(name: SectorChunkName, s: LoadState<AnyChunk>) {
    this.states.set(name, s);
    this.emit();
  }
}

function toLoadError(e: unknown): SectorLoadError {
  if (e instanceof SectorLoadError) return e;
  if (e instanceof SectorContractError) return new SectorLoadError('CONTRACT', e.message);
  return new SectorLoadError('NETWORK', String((e as Error)?.message ?? e));
}

let shared: SectorStore | null = null;

/**
 * The page's store (module-level, created on first use). Home's idle prefetch (builder B) calls
 * `getSectorStore().loadManifest()` and `.load('sector-meta')`; the Sector module reads the same
 * cache, so the landing is instant after a Home visit.
 */
export function getSectorStore(): SectorStore {
  if (!shared) shared = new SectorStore();
  return shared;
}

/** Tests only: replace the shared store. */
export function setSectorStoreForTests(s: SectorStore | null): void {
  shared = s;
}
