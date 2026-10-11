// Chunk hooks for the views (spec_sector.md 8.10 lazy map): facts, trade per technology, policy,
// policy text, series; the SectorBar status line; lazy section mounting (800 px from the viewport).

import { useEffect, useMemo, useRef, useState, useSyncExternalStore } from 'react';
import { isTradeTechSlug, TECH_LABELS, tradeChunkName, type SectorChunkName, type TechSlug } from '../contract.ts';
import { factsIndexOf, seriesIndexOf, type FactsIndex } from '../data/facts.ts';
import { useSectorChunk, useSectorStore, type ChunkHandle } from '../data/SectorDataProvider.tsx';
import type { SectorTradeChunk } from '../contract.ts';
import { fmtSize } from '../format.ts';

export function useFacts(enabled = true): { facts: FactsIndex | null; h: ChunkHandle<import('../contract.ts').SectorFactsChunk> } {
  const h = useSectorChunk('sector-facts', enabled);
  const facts = useMemo(() => (h.data ? factsIndexOf(h.data) : null), [h.data]);
  return { facts, h };
}

export function useSeries(enabled = true) {
  const h = useSectorChunk('sector-series', enabled);
  const series = useMemo(() => (h.data ? seriesIndexOf(h.data) : null), [h.data]);
  return { series, h };
}

/** The trade chunk of a technology (null name when the tech has no basket). */
export function useTrade(tech: TechSlug | null | undefined, enabled = true): { trade: SectorTradeChunk | null; h: ChunkHandle<SectorTradeChunk>; has: boolean } {
  const name = tech && isTradeTechSlug(tech) ? tradeChunkName(tech) : null;
  const store = useSectorStore();
  const h = useSectorChunk(name as SectorChunkName | null, enabled && !!name) as ChunkHandle<SectorTradeChunk>;
  return { trade: h.data, h, has: !!name && (store.hasChunk(name) || store.manifestState().status !== 'ready') };
}

export function usePolicy(enabled = true) {
  return useSectorChunk('sector-policy', enabled);
}

export function usePolicyText(enabled = true) {
  return useSectorChunk('sector-policy-text', enabled);
}

const CHUNK_WORDS: Record<string, string> = {
  'sector-meta': 'the sector summary',
  'sector-facts': 'the sector facts',
  'sector-policy': 'policy data',
  'sector-policy-text': 'policy titles',
  'sector-series': 'time series',
};

/** "trade data for Batteries" etc. */
export function chunkWords(name: string): string {
  if (name.startsWith('sector-trade-')) {
    const t = name.slice('sector-trade-'.length) as TechSlug;
    return `trade data for ${TECH_LABELS[t] ?? t}`;
  }
  return CHUNK_WORDS[name] ?? name;
}

/** "Loading trade data for Batteries, 310 kB" while any chunk loads, else null. */
export function useLoadingStatus(): string | null {
  const store = useSectorStore();
  const names: SectorChunkName[] = ['sector-meta', 'sector-facts', 'sector-policy', 'sector-policy-text', 'sector-series'];
  const get = () => {
    const m = store.manifestState().data;
    const all = m ? (m.chunks.map((c) => c.name) as SectorChunkName[]) : names;
    for (const n of all) {
      const s = store.chunkState(n);
      if (s.status === 'loading') return `Loading ${chunkWords(n)}${s.bytes ? `, ${fmtSize(s.bytes)}` : ''}`;
    }
    return null;
  };
  return useSyncExternalStore(store.subscribe, get, get);
}

/** True once the element comes within `margin` px of the viewport (sticky). Without IntersectionObserver (tests), true at once. */
export function useNear<T extends Element>(margin = 800): [React.RefObject<T | null>, boolean] {
  const ref = useRef<T>(null);
  const [near, setNear] = useState(() => typeof IntersectionObserver === 'undefined');
  useEffect(() => {
    if (near) return;
    const el = ref.current;
    if (!el || typeof IntersectionObserver === 'undefined') {
      setNear(true);
      return;
    }
    const io = new IntersectionObserver(
      (es) => {
        if (es.some((e) => e.isIntersecting)) {
          setNear(true);
          io.disconnect();
        }
      },
      { rootMargin: `${margin}px 0px` },
    );
    io.observe(el);
    return () => io.disconnect();
  }, [near, margin]);
  return [ref, near];
}

/** Open a hash target once it exists (section anchors survive copy-paste and trigger lazy loads). */
export function useScrollToHash(hash: string, ready: boolean): void {
  const done = useRef<string | null>(null);
  useEffect(() => {
    if (!ready || !hash || done.current === hash) return;
    const id = decodeURIComponent(hash.replace(/^#/, ''));
    const el = typeof document !== 'undefined' ? document.getElementById(id) : null;
    if (el) {
      done.current = hash;
      el.scrollIntoView?.({ block: 'start' });
    }
  }, [hash, ready]);
}
