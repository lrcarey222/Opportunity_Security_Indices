// PostureStore: fetches data/posture/manifest.json (no-cache), the core document and one country's
// detail document (content-hashed, default cache), checks them against the contract and caches them for
// the page life. Concurrent requests share one promise; a failure is retryable. Hooks below.

import { useCallback, useEffect, useState } from 'react';
import { checkPostureDetail, checkPostureManifest, decodeCore, POSTURE_DATA_DIR, PostureContractError, type PostureCore, type PostureDetail, type PostureManifest } from './contract.ts';

type FetchLike = (url: string, init?: RequestInit) => Promise<Response>;

export function postureDataBase(): string {
  const base = (import.meta.env?.BASE_URL as string | undefined) ?? '/';
  return `${base.endsWith('/') ? base : `${base}/`}${POSTURE_DATA_DIR}`;
}

export class PostureStore {
  private cache = new Map<string, Promise<unknown>>();
  private settled = new Map<string, unknown>();
  constructor(private readonly base = postureDataBase(), private readonly fetcher: FetchLike = (u, i) => fetch(u, i)) {}

  private get<T>(key: string, load: () => Promise<T>): Promise<T> {
    let p = this.cache.get(key) as Promise<T> | undefined;
    if (!p) {
      p = load().then(
        (v) => {
          this.settled.set(key, v);
          return v;
        },
        (e: unknown) => {
          this.cache.delete(key);
          throw e;
        },
      );
      this.cache.set(key, p);
    }
    return p;
  }
  peek<T>(key: string): T | undefined {
    return this.settled.get(key) as T | undefined;
  }
  private async json(file: string, init?: RequestInit): Promise<unknown> {
    const res = await this.fetcher(`${this.base}${file}`, init);
    if (!res.ok) throw new Error(`${file} could not be fetched (HTTP ${res.status}).`);
    const text = await res.text();
    try {
      return JSON.parse(text) as unknown;
    } catch {
      throw new Error(`${file} is not a JSON file.`);
    }
  }
  manifest(): Promise<PostureManifest> {
    return this.get('manifest', async () => checkPostureManifest(await this.json('manifest.json', { cache: 'no-cache' })));
  }
  core(): Promise<PostureCore> {
    return this.get('core', async () => decodeCore(await this.json((await this.manifest()).core)));
  }
  detail(iso3: string): Promise<PostureDetail> {
    return this.get(`detail:${iso3}`, async () => {
      const file = (await this.manifest()).details[iso3];
      if (!file) throw new Error(`No posture detail for ${iso3}.`);
      return checkPostureDetail(await this.json(file));
    });
  }
}

let shared: PostureStore | null = null;
export function postureStore(): PostureStore {
  shared ??= new PostureStore();
  return shared;
}
/** Tests: replace the shared store. */
export function setPostureStore(s: PostureStore | null): void {
  shared = s;
}

export interface PostureLoad<T> {
  status: 'loading' | 'ready' | 'error';
  data: T | null;
  error: string | null;
  retry: () => void;
}

function useLoad<T>(key: string | null, load: () => Promise<T>): PostureLoad<T> {
  const store = postureStore();
  const [state, setState] = useState<{ key: string | null; status: PostureLoad<T>['status']; data: T | null; error: string | null }>(() => {
    const v = key ? store.peek<T>(key) : undefined;
    return { key, status: v !== undefined ? 'ready' : 'loading', data: v ?? null, error: null };
  });
  const [attempt, setAttempt] = useState(0);
  useEffect(() => {
    if (!key) return;
    let alive = true;
    const v = store.peek<T>(key);
    if (v !== undefined) {
      setState({ key, status: 'ready', data: v, error: null });
      return;
    }
    setState({ key, status: 'loading', data: null, error: null });
    load().then(
      (data) => alive && setState({ key, status: 'ready', data, error: null }),
      (e: unknown) => alive && setState({ key, status: 'error', data: null, error: e instanceof PostureContractError ? e.message : String((e as Error)?.message ?? e) }),
    );
    return () => {
      alive = false;
    };
    // load follows key
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [key, attempt]);
  const retry = useCallback(() => setAttempt((a) => a + 1), []);
  const s = state.key === key ? state : { key, status: 'loading' as const, data: null, error: null };
  return { status: s.status, data: s.data, error: s.error, retry };
}

export function usePostureCore(): PostureLoad<PostureCore> {
  return useLoad('core', () => postureStore().core());
}
export function usePostureDetail(iso3: string | null): PostureLoad<PostureDetail> {
  return useLoad(iso3 ? `detail:${iso3}` : null, () => postureStore().detail(iso3 as string));
}
