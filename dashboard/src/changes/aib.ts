// Partnership changes (scripts/46_build_aib_vintages.R, R/change/aib_vintage.R): dashboard/public/data/changes/
// {aib-manifest.json, aib-changes.<md5-8>.json}. Pairs of economies whose shared priority compacts grew or shrank
// between AIB vintages, compact membership changes, and each pair of vintages' attribution. Computed in R; read here.
// A missing manifest means no partnership history (status 'absent').

import { useEffect, useState } from 'react';
import { changesDataBase } from './store.ts';

export interface AibPairChange {
  a: string;
  b: string;
  kind: 'new_pair' | 'dropped_pair' | 'compacts_changed' | 'purposes_changed';
  direction: 'more' | 'less' | 'same';
  delta: number;
  n_compacts_prev: number;
  n_compacts_cur: number;
  compacts_added: string;
  compacts_removed: string;
  attribution: 'real_world' | 'unattributed';
  explanation: string;
}

export interface AibVintagePair {
  prev: string;
  cur: string;
  method_changed: boolean;
  attribution: 'real_world' | 'unattributed';
  n_more: number;
  n_less: number;
  n_new: number;
  n_dropped: number;
  n_membership: number;
  detail: boolean;
  changes: AibPairChange[];
  membership: { compact: string; iso3: string; kind: 'joined' | 'left' | 'role_changed'; role_prev: string | null; role_cur: string | null }[];
}

export interface AibChanges {
  schema: 'osi-aib-changes';
  vintages: { vintage_id: string; created: string }[];
  compact_names: Record<string, string>;
  pairs: AibVintagePair[];
}

const isObj = (x: unknown): x is Record<string, unknown> => typeof x === 'object' && x !== null && !Array.isArray(x);

export function checkAibChanges(x: unknown): AibChanges {
  if (!isObj(x) || x.schema !== 'osi-aib-changes' || !Array.isArray(x.pairs)) throw new Error('Not an osi-aib-changes document.');
  for (const p of x.pairs as Record<string, unknown>[]) {
    p.changes = Array.isArray(p.changes) ? p.changes : [];
    p.membership = Array.isArray(p.membership) ? p.membership : [];
  }
  x.compact_names = isObj(x.compact_names) ? x.compact_names : {};
  return x as unknown as AibChanges;
}

export const aibVintageDate = (c: AibChanges, id: string): string => (c.vintages.find((v) => v.vintage_id === id)?.created ?? id).slice(0, 10);

/** The latest pair of AIB vintages that changed some pair thesis (the window the panels speak about), or null. */
export function latestAibChange(c: AibChanges): AibVintagePair | null {
  for (let i = c.pairs.length - 1; i >= 0; i--) if (c.pairs[i].detail && c.pairs[i].changes.length) return c.pairs[i];
  return null;
}

/** The latest change to one pair's thesis within the bundle's detail window (a, b in either order), with its vintage pair. */
export function pairThesisChange(c: AibChanges, x: string, y: string): { change: AibPairChange; pair: AibVintagePair } | null {
  const [a, b] = x < y ? [x, y] : [y, x];
  for (let i = c.pairs.length - 1; i >= 0; i--) {
    const ch = c.pairs[i].changes.find((r) => r.a === a && r.b === b);
    if (ch) return { change: ch, pair: c.pairs[i] };
  }
  return null;
}

let shared: Promise<AibChanges | null> | null = null;

export async function loadAibChanges(base = changesDataBase(), fetcher: typeof fetch = (u, i) => fetch(u, i)): Promise<AibChanges | null> {
  const m = await fetcher(`${base}aib-manifest.json`, { cache: 'no-cache' });
  if (!m.ok) return null;
  let man: { changes?: string };
  try {
    man = JSON.parse(await m.text()) as { changes?: string };
  } catch {
    return null;
  }
  if (!man.changes) return null;
  const res = await fetcher(`${base}${man.changes}`);
  if (!res.ok) return null;
  return checkAibChanges(JSON.parse(await res.text()));
}

/** Tests: replace the shared loader. */
export function setAibChangesLoader(p: Promise<AibChanges | null> | null): void {
  shared = p;
}

export function useAibChanges(): AibChanges | null {
  const [v, setV] = useState<AibChanges | null>(null);
  useEffect(() => {
    let alive = true;
    shared ??= loadAibChanges().catch(() => null);
    shared.then((d) => alive && setV(d));
    return () => {
      alive = false;
    };
  }, []);
  return v;
}

export const AIB_ATTRIBUTION_LABEL = { real_world: 'New data (same method)', unattributed: 'Method changed; data not verified' } as const;
