// The agreements data (public/data/agreements/, scripts/37): data/agreements/manifest.json is fetched with
// no-cache and checked against the contract; the global file and each economy's file (hashed names, cached
// for good) are fetched lazily, one shared promise each, checked, and kept for the page life. A failure is
// retryable (the cached promise is dropped). A missing manifest is 'NO_DATA': the page says the bundle is
// absent (CI has no data) and the rest of the explorer is unaffected. Hooks return a LoadState and retry().

import { useCallback, useEffect, useState } from 'react';
import {
  AGREEMENTS_DATA_DIR, AGREEMENTS_MANIFEST_FILE, AGREEMENTS_VERSION, AgreementsContractError, checkAgreementsCountry, checkAgreementsGlobal, checkAgreementsManifest,
  type AgreementsCountry, type AgreementsGlobal, type AgreementsManifest,
} from './contract.ts';

export type AgreementsLoadCode = 'NO_DATA' | 'NEWER' | 'CONTRACT' | 'NETWORK';

export class AgreementsLoadError extends Error {
  readonly code: AgreementsLoadCode;
  constructor(code: AgreementsLoadCode, message: string) {
    super(message);
    this.name = 'AgreementsLoadError';
    this.code = code;
  }
}

/** Where the files come from: HTTP in the app, the fixture directory in tests. */
export interface AgreementsSource {
  /** the parsed manifest; throws AgreementsLoadError('NO_DATA') when it does not exist */
  manifest(): Promise<unknown>;
  /** a parsed JSON file of the data directory */
  json(file: string): Promise<unknown>;
}

type FetchLike = (url: string, init?: RequestInit) => Promise<Response>;

/** The agreements data directory under the app's base URL ("/data/agreements/"). */
export function agreementsDataBase(): string {
  const base = (import.meta.env?.BASE_URL as string | undefined) ?? '/';
  return `${base.endsWith('/') ? base : `${base}/`}${AGREEMENTS_DATA_DIR}`;
}

/** The HTTP source (same origin only). */
export function httpAgreementsSource(base = agreementsDataBase(), opts: { fetch?: FetchLike } = {}): AgreementsSource {
  const f: FetchLike = opts.fetch ?? ((u, i) => fetch(u, i));
  const get = async (file: string, init: RequestInit | undefined): Promise<unknown> => {
    const what = `data/agreements/${file}`;
    const isManifest = file === AGREEMENTS_MANIFEST_FILE;
    let res: Response;
    try {
      res = await f(`${base}${file}`, init);
    } catch (e) {
      throw new AgreementsLoadError('NETWORK', `${what} could not be fetched (${String((e as Error)?.message ?? e)}).`);
    }
    if (res.status === 404) throw new AgreementsLoadError(isManifest ? 'NO_DATA' : 'NETWORK', `${what} does not exist (HTTP 404).`);
    if (!res.ok) throw new AgreementsLoadError('NETWORK', `${what} could not be fetched (HTTP ${res.status}).`);
    const text = await res.text();
    try {
      return JSON.parse(text) as unknown;
    } catch {
      // a dev server's index.html fallback for a missing file is not JSON
      throw new AgreementsLoadError(isManifest ? 'NO_DATA' : 'CONTRACT', `${what} is not a JSON file.`);
    }
  };
  return {
    manifest: () => get(AGREEMENTS_MANIFEST_FILE, { cache: 'no-cache' }),
    json: (file) => get(file, undefined),
  };
}

function loadError(e: unknown, what: string): AgreementsLoadError {
  if (e instanceof AgreementsLoadError) return e;
  if (e instanceof AgreementsContractError) {
    const newer = e.problems.some((p) => /needs a newer app/.test(p));
    return new AgreementsLoadError(newer ? 'NEWER' : 'CONTRACT', newer ? 'This data was exported for a newer version of the explorer; reload.' : `${what} does not match the contract (${e.problems.slice(0, 3).join('; ')}).`);
  }
  return new AgreementsLoadError('NETWORK', `${what} could not be loaded (${String((e as Error)?.message ?? e)}).`);
}

export class AgreementsStore {
  readonly source: AgreementsSource;
  private manifestP: Promise<AgreementsManifest> | null = null;
  private globalP: Promise<AgreementsGlobal> | null = null;
  private readonly countries = new Map<string, Promise<AgreementsCountry>>();
  /** settled values, for synchronous reads after the first load */
  private readonly done = new Map<string, unknown>();
  /** the files requested, in order (tests) */
  readonly calls: string[] = [];

  constructor(source: AgreementsSource = httpAgreementsSource()) {
    this.source = source;
  }

  peek<T>(key: string): T | undefined {
    return this.done.get(key) as T | undefined;
  }

  manifest(): Promise<AgreementsManifest> {
    if (!this.manifestP) {
      this.calls.push(AGREEMENTS_MANIFEST_FILE);
      const p = this.source.manifest().then((x) => {
        const problems = checkAgreementsManifest(x);
        if (problems.length) throw new AgreementsContractError('Agreements manifest', problems);
        const m = x as AgreementsManifest;
        if (Math.floor(m.version) > AGREEMENTS_VERSION) throw new AgreementsLoadError('NEWER', 'This data was exported for a newer version of the explorer; reload.');
        this.done.set('manifest', m);
        return m;
      });
      this.manifestP = p.catch((e) => {
        this.manifestP = null;
        throw loadError(e, 'The agreements manifest');
      });
    }
    return this.manifestP;
  }

  global(m: AgreementsManifest): Promise<AgreementsGlobal> {
    if (!this.globalP) {
      const file = m.files.global;
      this.calls.push(file);
      this.globalP = this.source
        .json(file)
        .then((x) => {
          const problems = checkAgreementsGlobal(x, m);
          if (problems.length) throw new AgreementsContractError('Agreements global file', problems);
          this.done.set('global', x);
          return x as AgreementsGlobal;
        })
        .catch((e) => {
          this.globalP = null;
          throw loadError(e, 'The agreements of all economies');
        });
    }
    return this.globalP;
  }

  country(m: AgreementsManifest, iso3: string): Promise<AgreementsCountry> {
    const file = m.files.countries?.[iso3];
    if (!file) return Promise.reject(new AgreementsLoadError('NETWORK', `There is no agreements file for ${iso3}.`));
    let p = this.countries.get(file);
    if (!p) {
      this.calls.push(file);
      p = this.source
        .json(file)
        .then((x) => {
          const problems = checkAgreementsCountry(x, iso3, m);
          if (problems.length) throw new AgreementsContractError(`Agreements of ${iso3}`, problems);
          this.done.set(`country:${iso3}`, x);
          return x as AgreementsCountry;
        })
        .catch((e) => {
          this.countries.delete(file);
          throw loadError(e, `The agreements of ${iso3}`);
        });
      this.countries.set(file, p);
    }
    return p;
  }
}

let shared: AgreementsStore | null = null;
let forTests: AgreementsStore | null = null;

/** The page-wide store (tests can swap it). */
export function agreementsStore(): AgreementsStore {
  if (forTests) return forTests;
  if (!shared) shared = new AgreementsStore();
  return shared;
}

export function setAgreementsStoreForTests(s: AgreementsStore | null): void {
  forTests = s;
}

// ---- hooks ------------------------------------------------------------------------------------------------

export interface Loaded<T> {
  status: 'idle' | 'loading' | 'ready' | 'error';
  data: T | null;
  error: AgreementsLoadError | null;
  retry: () => void;
}

type State<T> = Omit<Loaded<T>, 'retry'> & { key: string | null };

function useLoad<T>(key: string | null, load: (() => Promise<T>) | null): Loaded<T> {
  const store = agreementsStore();
  const initial = (): Omit<Loaded<T>, 'retry'> => {
    if (!key) return { status: 'idle', data: null, error: null };
    const v = store.peek<T>(key);
    return v !== undefined ? { status: 'ready', data: v, error: null } : { status: 'loading', data: null, error: null };
  };
  const [state, setState] = useState<State<T>>(() => ({ ...initial(), key }));
  const [attempt, setAttempt] = useState(0);
  useEffect(() => {
    if (!key || !load) return;
    let alive = true;
    const peeked = store.peek<T>(key);
    if (peeked !== undefined) {
      setState({ status: 'ready', data: peeked, error: null, key });
      return;
    }
    setState({ status: 'loading', data: null, error: null, key });
    load().then(
      (data) => alive && setState({ status: 'ready', data, error: null, key }),
      (e: unknown) => alive && setState({ status: 'error', data: null, error: e instanceof AgreementsLoadError ? e : new AgreementsLoadError('NETWORK', String((e as Error)?.message ?? e)), key }),
    );
    return () => {
      alive = false;
    };
    // load is derived from key; attempt re-runs after a failure
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [key, attempt]);
  const retry = useCallback(() => setAttempt((a) => a + 1), []);
  // a key change shows loading until the effect runs (never the previous economy's data)
  const s = state.key === key ? state : { ...initial(), key };
  return { status: s.status, data: s.data, error: s.error, retry };
}

export function useAgreementsManifest(): Loaded<AgreementsManifest> {
  return useLoad<AgreementsManifest>('manifest', () => agreementsStore().manifest());
}

/** The global file, once the manifest is in. */
export function useAgreementsGlobal(m: AgreementsManifest | null): Loaded<AgreementsGlobal> {
  return useLoad<AgreementsGlobal>(m ? 'global' : null, m ? () => agreementsStore().global(m) : null);
}

/** An economy's file, when the manifest names one for it ('idle' otherwise). */
export function useAgreementsCountry(m: AgreementsManifest | null, iso3: string | null): Loaded<AgreementsCountry> {
  const ok = m && iso3 && m.files.countries?.[iso3] ? iso3 : null;
  return useLoad<AgreementsCountry>(ok ? `country:${ok}` : null, ok && m ? () => agreementsStore().country(m, ok) : null);
}
