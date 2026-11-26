// Loads the change bundle (data/changes/manifest.json, no-cache, then the content-hashed document). A missing
// manifest (HTTP 404, or a build without the change layer) is "no history": status 'absent', never an error.

import { useEffect, useState } from 'react';
import { CHANGES_DATA_DIR, checkChangesBundle, checkChangesManifest, type ChangesBundle } from './contract.ts';

type FetchLike = (url: string, init?: RequestInit) => Promise<Response>;

export function changesDataBase(): string {
  const base = (import.meta.env?.BASE_URL as string | undefined) ?? '/';
  return `${base.endsWith('/') ? base : `${base}/`}${CHANGES_DATA_DIR}`;
}

export async function loadChanges(base = changesDataBase(), fetcher: FetchLike = (u, i) => fetch(u, i)): Promise<ChangesBundle | null> {
  const m = await fetcher(`${base}manifest.json`, { cache: 'no-cache' });
  if (m.status === 404) return null;
  if (!m.ok) throw new Error(`changes/manifest.json could not be fetched (HTTP ${m.status}).`);
  let manifestJson: unknown;
  try {
    manifestJson = JSON.parse(await m.text());
  } catch {
    // a static host that answers every path with the app shell: treat as no history
    return null;
  }
  const manifest = checkChangesManifest(manifestJson);
  const res = await fetcher(`${base}${manifest.changes}`);
  if (!res.ok) throw new Error(`${manifest.changes} could not be fetched (HTTP ${res.status}).`);
  return checkChangesBundle(JSON.parse(await res.text()));
}

let shared: Promise<ChangesBundle | null> | null = null;
let settled: ChangesBundle | null | undefined;

/** Tests: replace the shared loader (null resets it). */
export function setChangesLoader(p: Promise<ChangesBundle | null> | null): void {
  shared = p;
  settled = undefined;
  p?.then(
    (v) => (settled = v),
    () => undefined,
  );
}

function sharedChanges(): Promise<ChangesBundle | null> {
  if (!shared) {
    shared = loadChanges().then(
      (v) => (settled = v),
      (e: unknown) => {
        shared = null;
        throw e;
      },
    );
  }
  return shared;
}

export interface ChangesLoad {
  status: 'loading' | 'ready' | 'absent' | 'error';
  data: ChangesBundle | null;
  error: string | null;
}

export function useChanges(): ChangesLoad {
  const [s, set] = useState<ChangesLoad>(() =>
    settled === undefined ? { status: 'loading', data: null, error: null } : { status: settled ? 'ready' : 'absent', data: settled, error: null },
  );
  useEffect(() => {
    if (settled !== undefined) return;
    let alive = true;
    sharedChanges().then(
      (d) => alive && set({ status: d ? 'ready' : 'absent', data: d, error: null }),
      (e: unknown) => alive && set({ status: 'error', data: null, error: String((e as Error)?.message ?? e) }),
    );
    return () => {
      alive = false;
    };
  }, []);
  return s;
}
