// React hooks over AibStore: the manifest and the compacts document. Each returns a LoadState and a
// retry(); a settled value is read synchronously on the next mount (no flash of "Loading").
// The scenarios (2026-10-19): useAibScenarioDoc() loads one lazily loaded scenario file (anywhere under
// the router: the AIB pages and the Country profile alike), useAibScenario() reads and writes the toggle
// in the URL (`scn`).

import { useCallback, useEffect, useState } from 'react';
import { useLocation, useNavigate } from 'react-router';
import {
  scenarioFilesOf, type AibBaselineScenarioDoc, type AibDoc, type AibManagedScenarioDoc, type AibManifest, type AibScenarioDoc, type AibScenarioFileId, type AibScenarioId,
} from '../contract.ts';
import { scenarioOfSearch, setScenarioSearch, withScenario, type AibToggle } from '../scenarioUrl.ts';
import { aibStore, AibLoadError, scenarioKey, type LoadState } from './store.ts';

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

/** A scenario file as a page reads it. */
export interface ScenarioLoad<T> {
  /** the checked file (null while loading, on an error, when missing, and for Decoupling: the main document) */
  doc: T | null;
  loading: boolean;
  error: AibLoadError | null;
  retry: () => void;
  /** the manifest names no file for this scenario (an older bundle): show the main document's figures only */
  missing: boolean;
}

const isFileId = (id: AibScenarioId | null): id is AibScenarioFileId => id === 'managed' || id === 'baseline';

/**
 * The lazily loaded file of a scenario ('managed' or 'baseline'; 'decoupling' and null are idle: the main
 * document is Decoupling). Loads the manifest itself, so it works outside AibLayout (the Country profile).
 * A settled file is read synchronously on the next mount.
 */
export function useAibScenarioDoc(id: 'managed'): ScenarioLoad<AibManagedScenarioDoc>;
export function useAibScenarioDoc(id: 'baseline'): ScenarioLoad<AibBaselineScenarioDoc>;
export function useAibScenarioDoc(id: AibScenarioId | null): ScenarioLoad<AibScenarioDoc>;
export function useAibScenarioDoc(id: AibScenarioId | null): ScenarioLoad<AibScenarioDoc> {
  const m = useAibManifest();
  const fileId = isFileId(id) ? id : null;
  const file = m.data && fileId ? (scenarioFilesOf(m.data)[fileId] ?? null) : null;
  const l = useLoad<AibScenarioDoc | null>(file ? scenarioKey(file) : null, file && m.data && fileId ? () => aibStore().scenario(m.data!, fileId) : null);
  if (!fileId) return { doc: null, loading: false, error: null, retry: l.retry, missing: false };
  if (m.status === 'error') return { doc: null, loading: false, error: m.error, retry: m.retry, missing: false };
  if (m.status !== 'ready') return { doc: null, loading: true, error: null, retry: l.retry, missing: false };
  if (!file) return { doc: null, loading: false, error: null, retry: l.retry, missing: true };
  return { doc: l.status === 'ready' ? l.data : null, loading: l.status === 'loading', error: l.status === 'error' ? l.error : null, retry: l.retry, missing: false };
}

/** The toggle as the URL holds it. */
export interface ScenarioState {
  /** the URL's choice (scn): use AibCtx.scenario in the AIB pages, which falls back to Decoupling for a bundle without scenarios */
  scenario: AibToggle;
  /** write the choice (replaces the history entry; keeps every other key and the hash; scroll kept) */
  setScenario: (s: AibToggle) => void;
  /** an href with the choice carried (withScenario) */
  href: (to: string) => string;
}

/** Read and write the scenario toggle (`scn`) of the current URL; works under any route. */
export function useAibScenario(): ScenarioState {
  const loc = useLocation();
  const nav = useNavigate();
  const scenario = scenarioOfSearch(loc.search);
  const setScenario = useCallback(
    (s: AibToggle) => nav({ pathname: loc.pathname, search: setScenarioSearch(loc.search, s), hash: loc.hash }, { replace: true, preventScrollReset: true }),
    [nav, loc.pathname, loc.search, loc.hash],
  );
  const href = useCallback((to: string) => withScenario(to, scenario), [scenario]);
  return { scenario, setScenario, href };
}
