// SectorDataProvider (spec_sector.md 8.10): the store in a context plus useSyncExternalStore hooks.
// useSectorManifest(), useSectorMeta() (+ the model), useSectorChunk(name, enabled) ->
// { status, data, error, bytes, retry }. Loads start in effects, never during render.

import { createContext, useContext, useEffect, useMemo, useSyncExternalStore, type ReactNode } from 'react';
import type { ChunkFor, SectorChunkName, SectorManifest, SectorMetaChunk } from '../contract.ts';
import { getSectorStore, type LoadState, type SectorStore } from './store.ts';
import { modelOf, type SectorModel } from './model.ts';

const StoreContext = createContext<SectorStore | null>(null);

export function SectorDataProvider({ store, children }: { store?: SectorStore; children: ReactNode }) {
  const s = useMemo(() => store ?? getSectorStore(), [store]);
  return <StoreContext.Provider value={s}>{children}</StoreContext.Provider>;
}

export function useSectorStore(): SectorStore {
  return useContext(StoreContext) ?? getSectorStore();
}

export interface ChunkHandle<T> extends LoadState<T> {
  retry: () => void;
}

/** The manifest (loads on mount). */
export function useSectorManifest(): ChunkHandle<SectorManifest> {
  const store = useSectorStore();
  const st = useSyncExternalStore(store.subscribe, store.manifestState, store.manifestState);
  useEffect(() => {
    if (st.status === 'idle') void store.loadManifest().catch(() => undefined);
  }, [store, st.status]);
  return useMemo(() => ({ ...st, retry: () => store.retry('manifest') }), [st, store]);
}

/** One chunk; `enabled` false defers the load (lazy sections). */
export function useSectorChunk<N extends SectorChunkName>(name: N | null, enabled = true): ChunkHandle<ChunkFor<N>> {
  const store = useSectorStore();
  const get = () => (name ? store.chunkState(name) : (IDLE_STATE as LoadState<ChunkFor<N>>));
  const st = useSyncExternalStore(store.subscribe, get, get);
  useEffect(() => {
    if (name && enabled && st.status === 'idle') void store.load(name).catch(() => undefined);
  }, [store, name, enabled, st.status]);
  return useMemo(() => ({ ...st, retry: () => (name ? store.retry(name) : undefined) }), [st, store, name]);
}

const IDLE_STATE: LoadState<never> = { status: 'idle', data: null, error: null, bytes: null };

/** sector-meta and its model. */
export function useSectorMeta(): ChunkHandle<SectorMetaChunk> & { model: SectorModel | null } {
  const h = useSectorChunk('sector-meta');
  const model = useMemo(() => (h.data ? modelOf(h.data) : null), [h.data]);
  return useMemo(() => ({ ...h, model }), [h, model]);
}

/** The model, for components rendered only once meta is ready (SectorLayout gates on it). */
export function useModel(): SectorModel {
  const { model } = useSectorMeta();
  if (!model) throw new Error('useModel() needs sector-meta; render inside the loaded Sector layout.');
  return model;
}
