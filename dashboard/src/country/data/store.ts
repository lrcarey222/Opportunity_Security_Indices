// CountryStore (spec_country.md A4.0, A8.5): fetches data/country/manifest.json (no-cache) and checks
// it with checkCountryManifest(); fetches fact sheets and memo documents (immutable, default cache),
// validates them against the contract and caches them for the page life. Concurrent requests share
// one promise; a failure is retryable (the cached promise is dropped). No React here.

import {
  COUNTRY_DATA_DIR, COUNTRY_MANIFEST_FILE, COUNTRY_SCHEMA_VERSION, CountryContractError, checkCountryManifest, checkDriversDoc, checkFactSheet, checkMemoDoc,
  type CountryEntry, type CountryManifest, type DriversDoc, type FactSheet, type MemoDoc,
} from '../contract.ts';

export type CountryLoadCode = 'NO_DATA' | 'NEWER' | 'CONTRACT' | 'NETWORK';

export class CountryLoadError extends Error {
  readonly code: CountryLoadCode;
  constructor(code: CountryLoadCode, message: string) {
    super(message);
    this.name = 'CountryLoadError';
    this.code = code;
  }
}

/** Where the files come from: HTTP in the app, the fixture directory in tests. */
export interface CountrySource {
  /** the parsed manifest.json; throws CountryLoadError('NO_DATA') when it does not exist */
  manifest(): Promise<unknown>;
  /** a parsed JSON file of the data directory */
  json(file: string): Promise<unknown>;
  /** the URL of a file (download links) */
  url(file: string): string;
}

type FetchLike = (url: string, init?: RequestInit) => Promise<Response>;

/** The country data directory under the app's base URL ("/data/country/", "/explorer/data/country/"). */
export function countryDataBase(): string {
  const base = (import.meta.env?.BASE_URL as string | undefined) ?? '/';
  return `${base.endsWith('/') ? base : `${base}/`}${COUNTRY_DATA_DIR}`;
}

/** The HTTP source (same origin only). */
export function httpCountrySource(base = countryDataBase(), opts: { fetch?: FetchLike } = {}): CountrySource {
  const f: FetchLike = opts.fetch ?? ((u, i) => fetch(u, i));
  const get = async (file: string, init: RequestInit | undefined, what: string): Promise<unknown> => {
    let res: Response;
    try {
      res = await f(`${base}${file}`, init);
    } catch (e) {
      throw new CountryLoadError('NETWORK', `${what} could not be fetched (${String((e as Error)?.message ?? e)}).`);
    }
    if (res.status === 404) throw new CountryLoadError(file === COUNTRY_MANIFEST_FILE ? 'NO_DATA' : 'NETWORK', `${what} does not exist (HTTP 404).`);
    if (!res.ok) throw new CountryLoadError('NETWORK', `${what} could not be fetched (HTTP ${res.status}).`);
    const text = await res.text();
    try {
      return JSON.parse(text) as unknown;
    } catch {
      // a dev server's index.html fallback for a missing file is not JSON
      throw new CountryLoadError(file === COUNTRY_MANIFEST_FILE ? 'NO_DATA' : 'CONTRACT', `${what} is not a JSON file.`);
    }
  };
  return {
    manifest: () => get(COUNTRY_MANIFEST_FILE, { cache: 'no-cache' }, 'data/country/manifest.json'),
    json: (file) => get(file, undefined, file),
    url: (file) => `${base}${file}`,
  };
}

export interface LoadState<T> {
  status: 'loading' | 'ready' | 'error';
  data: T | null;
  error: CountryLoadError | null;
}

/** A newer schema than this app reads: the module's error screen asks for a reload (A5). */
function contractError(e: unknown, what: string): CountryLoadError {
  if (e instanceof CountryLoadError) return e;
  if (e instanceof CountryContractError) {
    const newer = e.problems.some((p) => /needs a newer app/.test(p));
    return new CountryLoadError(newer ? 'NEWER' : 'CONTRACT', newer ? 'This data was exported for a newer version of the explorer; reload.' : `${what} does not match the contract (${e.problems.slice(0, 3).join('; ')}).`);
  }
  return new CountryLoadError('NETWORK', `${what} could not be loaded (${String((e as Error)?.message ?? e)}).`);
}

export class CountryStore {
  readonly source: CountrySource;
  private manifestP: Promise<CountryManifest> | null = null;
  private readonly sheets = new Map<string, Promise<FactSheet>>();
  private readonly memos = new Map<string, Promise<MemoDoc>>();
  private readonly drivers = new Map<string, Promise<DriversDoc>>();
  /** settled values, for synchronous reads after the first load */
  private readonly done = new Map<string, unknown>();
  readonly calls: string[] = [];

  constructor(source: CountrySource = httpCountrySource()) {
    this.source = source;
  }

  peek<T>(key: string): T | undefined {
    return this.done.get(key) as T | undefined;
  }

  manifest(): Promise<CountryManifest> {
    if (!this.manifestP) {
      this.calls.push('manifest');
      const p = this.source.manifest().then((x) => {
        const problems = checkCountryManifest(x);
        if (problems.length) throw new CountryContractError('Country manifest', problems);
        const m = x as CountryManifest;
        if (Math.floor(m.schema_version) > COUNTRY_SCHEMA_VERSION) throw new CountryLoadError('NEWER', 'This data was exported for a newer version of the explorer; reload.');
        this.done.set('manifest', m);
        return m;
      });
      this.manifestP = p.catch((e) => {
        this.manifestP = null;
        throw contractError(e, 'The country manifest');
      });
    }
    return this.manifestP;
  }

  sheet(entry: CountryEntry): Promise<FactSheet> {
    const file = entry.file;
    if (!file) return Promise.reject(new CountryLoadError('NETWORK', `${entry.name} has no fact sheet.`));
    let p = this.sheets.get(file);
    if (!p) {
      this.calls.push(file);
      p = this.source
        .json(file)
        .then((x) => {
          const problems = checkFactSheet(x, entry);
          if (problems.length) throw new CountryContractError(`Fact sheet ${entry.iso3}`, problems);
          this.done.set(`sheet:${entry.iso3}`, x);
          return x as FactSheet;
        })
        .catch((e) => {
          this.sheets.delete(file);
          throw contractError(e, `The fact sheet of ${entry.name}`);
        });
      this.sheets.set(file, p);
    }
    return p;
  }

  memo(entry: CountryEntry): Promise<MemoDoc> {
    const file = entry.memo?.json;
    if (!file) return Promise.reject(new CountryLoadError('NETWORK', `${entry.name} has no memo.`));
    let p = this.memos.get(file);
    if (!p) {
      this.calls.push(file);
      p = this.source
        .json(file)
        .then((x) => {
          const problems = checkMemoDoc(x, entry.iso3);
          if (problems.length) throw new CountryContractError(`Memo ${entry.iso3}`, problems);
          this.done.set(`memo:${entry.iso3}`, x);
          return x as MemoDoc;
        })
        .catch((e) => {
          this.memos.delete(file);
          throw contractError(e, `The memo for ${entry.name}`);
        });
      this.memos.set(file, p);
    }
    return p;
  }

  /** The companion drivers file of a sheet (the "why" data of the sectors the sheet does not carry). */
  driversDoc(entry: CountryEntry, sheet?: FactSheet): Promise<DriversDoc> {
    const file = entry.drivers;
    if (!file) return Promise.reject(new CountryLoadError('NETWORK', `${entry.name} has no drivers file.`));
    let p = this.drivers.get(file);
    if (!p) {
      this.calls.push(file);
      p = this.source
        .json(file)
        .then((x) => {
          const problems = checkDriversDoc(x, sheet);
          if (problems.length) throw new CountryContractError(`Drivers ${entry.iso3}`, problems);
          this.done.set(`drivers:${entry.iso3}`, x);
          return x as DriversDoc;
        })
        .catch((e) => {
          this.drivers.delete(file);
          throw contractError(e, `The drivers file of ${entry.name}`);
        });
      this.drivers.set(file, p);
    }
    return p;
  }

  url(file: string): string {
    return this.source.url(file);
  }
}

let shared: CountryStore | null = null;
let forTests: CountryStore | null = null;

/** The page-wide store (tests can swap it). */
export function countryStore(): CountryStore {
  if (forTests) return forTests;
  if (!shared) shared = new CountryStore();
  return shared;
}

export function setCountryStoreForTests(s: CountryStore | null): void {
  forTests = s;
}
