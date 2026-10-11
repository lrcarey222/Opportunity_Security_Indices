// The caption contract (spec_sector.md 3.9): a card names the measures it shows (vars: VarRef[]) and
// the views' resolver turns them into the sub line, the vintage badge, the caveats, the coverage line
// and the span note, from the dictionary. The kit stays free of the dictionary: SectorLayout provides
// the resolver once (<CaptionProvider resolve={…}>).

import { createContext, useContext, useMemo, type ReactNode } from 'react';
import type { CaptionInfo, CaptionResolver, VarRef } from './types.ts';

const CaptionContext = createContext<CaptionResolver | null>(null);

export function CaptionProvider({ resolve, children }: { resolve: CaptionResolver; children: ReactNode }) {
  return <CaptionContext.Provider value={resolve}>{children}</CaptionContext.Provider>;
}

/** Without a resolver (a card rendered on its own): the var ids, so a missing caption is visible. */
export function fallbackCaption(vars: readonly VarRef[]): CaptionInfo {
  const years = vars.map((v) => (v.window ? `${v.window[0]}–${v.window[1]}` : v.year != null ? String(v.year) : '')).filter(Boolean);
  return { sub: vars.map((v) => v.var).join(' · '), badge: years.length ? [...new Set(years)].join(', ') : null, caveats: [], coverage: null, spanNote: null };
}

/** The caption of a card's vars, overridden field by field by `override`. */
export function useCaption(vars: readonly VarRef[], override?: Partial<CaptionInfo>): CaptionInfo {
  const resolve = useContext(CaptionContext);
  // both compared by value, so callers may pass fresh arrays and objects on every render
  const key = JSON.stringify(vars);
  const okey = JSON.stringify(override ?? null);
  return useMemo(() => {
    const vs = JSON.parse(key) as VarRef[];
    const base = resolve ? resolve(vs) : fallbackCaption(vs);
    return { ...base, ...((JSON.parse(okey) as Partial<CaptionInfo> | null) ?? {}) };
  }, [resolve, key, okey]);
}
