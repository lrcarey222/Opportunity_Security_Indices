// AibStore: fetches data/aib/manifest.json (no-cache) and checks it with checkAibManifest(); fetches the
// compacts document the manifest names (immutable, default cache), checks it with checkCompactsDoc()
// and caches it for the page life. Concurrent requests share one promise; a failure is retryable (the
// cached promise is dropped). No React here. Tests swap the store with setAibStoreForTests().

import {
  AIB_DATA_DIR, AIB_MANIFEST_FILE, AIB_VERSION, AibContractError, checkAibManifest, checkCompactsDoc, needsNewerApp,
  type AibDoc, type AibManifest,
} from '../contract.ts';

export type AibLoadCode = 'NO_DATA' | 'NEWER' | 'CONTRACT' | 'NETWORK';

export class AibLoadError extends Error {
  readonly code: AibLoadCode;
  constructor(code: AibLoadCode, message: string) {
    super(message);
    this.name = 'AibLoadError';
    this.code = code;
  }
}

/** Where the files come from: HTTP in the app, the fixture directory in tests. */
export interface AibSource {
  /** the parsed manifest.json; throws AibLoadError('NO_DATA') when it does not exist */
  manifest(): Promise<unknown>;
  /** a parsed JSON file of the data directory */
  json(file: string): Promise<unknown>;
}

type FetchLike = (url: string, init?: RequestInit) => Promise<Response>;

/** The compacts data directory under the app's base URL ("/data/aib/", "/explorer/data/aib/"). */
export function aibDataBase(): string {
  const base = (import.meta.env?.BASE_URL as string | undefined) ?? '/';
  return `${base.endsWith('/') ? base : `${base}/`}${AIB_DATA_DIR}`;
}

/** The HTTP source (same origin only). */
export function httpAibSource(base = aibDataBase(), opts: { fetch?: FetchLike } = {}): AibSource {
  const f: FetchLike = opts.fetch ?? ((u, i) => fetch(u, i));
  const get = async (file: string, init: RequestInit | undefined, what: string): Promise<unknown> => {
    const isManifest = file === AIB_MANIFEST_FILE;
    let res: Response;
    try {
      res = await f(`${base}${file}`, init);
    } catch (e) {
      throw new AibLoadError('NETWORK', `${what} could not be fetched (${String((e as Error)?.message ?? e)}).`);
    }
    if (res.status === 404) throw new AibLoadError(isManifest ? 'NO_DATA' : 'NETWORK', `${what} does not exist (HTTP 404).`);
    if (!res.ok) throw new AibLoadError('NETWORK', `${what} could not be fetched (HTTP ${res.status}).`);
    const text = await res.text();
    try {
      return JSON.parse(text) as unknown;
    } catch {
      // a dev server's index.html fallback for a missing file is not JSON
      throw new AibLoadError(isManifest ? 'NO_DATA' : 'CONTRACT', `${what} is not a JSON file.`);
    }
  };
  return {
    manifest: () => get(AIB_MANIFEST_FILE, { cache: 'no-cache' }, 'data/aib/manifest.json'),
    json: (file) => get(file, undefined, `data/aib/${file}`),
  };
}

export interface LoadState<T> {
  status: 'loading' | 'ready' | 'error';
  data: T | null;
  error: AibLoadError | null;
}

const NEWER_MESSAGE = 'This data was exported for a newer version of the explorer; reload.';

/** Any failure as an AibLoadError (a newer schema asks for a reload). */
export function toLoadError(e: unknown, what: string): AibLoadError {
  if (e instanceof AibLoadError) return e;
  if (e instanceof AibContractError) {
    if (needsNewerApp(e.problems)) return new AibLoadError('NEWER', NEWER_MESSAGE);
    return new AibLoadError('CONTRACT', `${what} does not match the contract (${e.problems.slice(0, 3).join('; ')}).`);
  }
  return new AibLoadError('NETWORK', `${what} could not be loaded (${String((e as Error)?.message ?? e)}).`);
}

export class AibStore {
  readonly source: AibSource;
  private manifestP: Promise<AibManifest> | null = null;
  private readonly docs = new Map<string, Promise<AibDoc>>();
  /** settled values, for synchronous reads after the first load */
  private readonly done = new Map<string, unknown>();
  /** the files requested, in order (tests) */
  readonly calls: string[] = [];

  constructor(source: AibSource = httpAibSource()) {
    this.source = source;
  }

  peek<T>(key: string): T | undefined {
    return this.done.get(key) as T | undefined;
  }

  manifest(): Promise<AibManifest> {
    if (!this.manifestP) {
      this.calls.push(AIB_MANIFEST_FILE);
      const p = this.source.manifest().then((x) => {
        const problems = checkAibManifest(x);
        if (problems.length) throw new AibContractError('Compacts manifest', problems);
        const m = x as AibManifest;
        if (Math.floor(m.version) > AIB_VERSION) throw new AibLoadError('NEWER', NEWER_MESSAGE);
        this.done.set('manifest', m);
        return m;
      });
      this.manifestP = p.catch((e) => {
        this.manifestP = null;
        throw toLoadError(e, 'The compacts manifest');
      });
    }
    return this.manifestP;
  }

  /** The document the manifest names. */
  doc(m: AibManifest): Promise<AibDoc> {
    const file = m.files.compacts;
    let p = this.docs.get(file);
    if (!p) {
      this.calls.push(file);
      p = this.source
        .json(file)
        .then((x) => {
          const problems = checkCompactsDoc(x);
          if (problems.length) throw new AibContractError(`Compacts document ${file}`, problems);
          this.done.set(`doc:${file}`, x);
          return x as AibDoc;
        })
        .catch((e) => {
          this.docs.delete(file);
          throw toLoadError(e, 'The compacts data');
        });
      this.docs.set(file, p);
    }
    return p;
  }
}

let shared: AibStore | null = null;
let forTests: AibStore | null = null;

/** The page-wide store (tests can swap it). */
export function aibStore(): AibStore {
  if (forTests) return forTests;
  if (!shared) shared = new AibStore();
  return shared;
}

export function setAibStoreForTests(s: AibStore | null): void {
  forTests = s;
}
