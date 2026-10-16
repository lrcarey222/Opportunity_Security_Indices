// React hooks over AibStore: the manifest and the compacts document. Each returns a LoadState and a
// retry(); a settled value is read synchronously on the next mount (no flash of "Loading").

import { useCallback, useEffect, useState } from 'react';
import type { AibDoc, AibManifest } from '../contract.ts';
import { aibStore, AibLoadError, type LoadState } from './store.ts';

export interface Loaded<T> extends LoadState<T> {
  retry: () => void;
}

function useLoad<T>(key: string | null, load: (() => Promise<T>) | null): Loaded<T> {
  const store = aibStore();
  const initial = (): LoadState<T> => {
    if (!key) return { status: 'loading', data: null, error: null };
    const v = store.peek<T>(key);
    return v !== undefined ? { status: 'ready', data: v, error: null } : { status: 'loading', data: null, error: null };
  };
  const [state, setState] = useState<LoadState<T> & { key: string | null }>(() => ({ ...initial(), key }));
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
      (e: unknown) =>
        alive && setState({ status: 'error', data: null, error: e instanceof AibLoadError ? e : new AibLoadError('NETWORK', String((e as Error)?.message ?? e)), key }),
    );
    return () => {
      alive = false;
    };
    // load is derived from key; attempt re-runs after a failure
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [key, attempt]);
  const retry = useCallback(() => setAttempt((a) => a + 1), []);
  // a key change shows loading until the effect runs
  const s = state.key === key ? state : { ...initial(), key };
  return { status: s.status, data: s.data, error: s.error, retry };
}

export function useAibManifest(): Loaded<AibManifest> {
  return useLoad<AibManifest>('manifest', () => aibStore().manifest());
}

/** The compacts document of a loaded manifest (idle until the manifest is there). */
export function useCompactsDoc(manifest: AibManifest | null): Loaded<AibDoc> {
  const file = manifest?.files.compacts ?? null;
  return useLoad<AibDoc>(file ? `doc:${file}` : null, manifest ? () => aibStore().doc(manifest) : null);
}
