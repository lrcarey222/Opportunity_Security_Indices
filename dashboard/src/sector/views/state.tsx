// Sector URL state in React (spec_sector.md 2.7): one parsed state per location, shared by the
// layout and the views; setters push or replace history by the 2.7 rule; links between views keep
// `c` and `eu` only. The "Ignored in the link" problems are shown once by the layout.
// SectorStateProvider (2026-10-30) gives an embedded explorer (the Country profile's variable charts) the same
// API over state held in memory: its setters never touch the page's URL, and its links go to the Sector pages.

import { createContext, useCallback, useContext, useMemo, useState, type ReactNode } from 'react';
import { useLocation, useNavigate } from 'react-router';
import { STAGE_LABELS, TECH_LABELS } from '../contract.ts';
import type { SectorModel } from '../data/model.ts';
import { parseSectorSearch, patchReplaces, SECTOR_DEFAULTS, sectorHref, writeSectorSearch, type SectorUrlCtx, type SectorUrlState } from '../url.ts';

export interface SectorUrlApi {
  state: SectorUrlState;
  problems: string[];
  /** change keys of the current view (push, or replace for year / show-as / scale / item) */
  set: (patch: Partial<SectorUrlState>, opts?: { replace?: boolean; hash?: string }) => void;
  /** a link to another Sector path keeping c and eu, plus view keys */
  link: (path: string, view?: Partial<SectorUrlState>, hash?: string) => string;
}

const Ctx = createContext<SectorUrlApi | null>(null);

export function urlCtxOf(model: SectorModel | null): SectorUrlCtx {
  if (!model) return {};
  return {
    countries: new Set(model.countries.map((g) => g.code)),
    vars: new Set(model.vars.map((v) => v.id)),
    items: new Set(model.items.map((x) => x.slug)),
    techs: new Set(Object.values(TECH_LABELS)),
    stages: new Set(Object.values(STAGE_LABELS)),
  };
}

export function SectorUrlProvider({ model, children }: { model: SectorModel | null; children: ReactNode }) {
  const loc = useLocation();
  const navigate = useNavigate();
  const ctx = useMemo(() => urlCtxOf(model), [model]);
  const parsed = useMemo(() => parseSectorSearch(loc.search, ctx), [loc.search, ctx]);
  const set = useCallback(
    (patch: Partial<SectorUrlState>, opts: { replace?: boolean; hash?: string } = {}) => {
      const next = { ...parsed.state, ...patch };
      const s = writeSectorSearch(next);
      navigate({ pathname: loc.pathname, search: s ? `?${s}` : '', hash: opts.hash ?? loc.hash }, { replace: opts.replace ?? patchReplaces(patch), preventScrollReset: true });
    },
    [parsed.state, navigate, loc.pathname, loc.hash],
  );
  const link = useCallback((path: string, view: Partial<SectorUrlState> = {}, hash?: string) => sectorHref(path, parsed.state, view, hash), [parsed.state]);
  const value = useMemo<SectorUrlApi>(() => ({ state: parsed.state, problems: parsed.problems, set, link }), [parsed, set, link]);
  return <Ctx.Provider value={value}>{children}</Ctx.Provider>;
}

/**
 * The Sector state in memory, for an explorer embedded in another page: `initial` over the defaults, `set`
 * merges a patch, `link` builds a Sector page link from it (c and eu kept, as SectorUrlProvider's).
 */
export function SectorStateProvider({ initial, children }: { initial: Partial<SectorUrlState>; children: ReactNode }) {
  const [state, setState] = useState<SectorUrlState>(() => ({ ...SECTOR_DEFAULTS, ...initial }));
  const set = useCallback((patch: Partial<SectorUrlState>) => setState((s) => ({ ...s, ...patch })), []);
  const link = useCallback((path: string, view: Partial<SectorUrlState> = {}, hash?: string) => sectorHref(path, state, view, hash), [state]);
  const value = useMemo<SectorUrlApi>(() => ({ state, problems: [], set, link }), [state, set, link]);
  return <Ctx.Provider value={value}>{children}</Ctx.Provider>;
}

export function useSectorUrl(): SectorUrlApi {
  const v = useContext(Ctx);
  if (!v) throw new Error('useSectorUrl() needs SectorUrlProvider (SectorLayout).');
  return v;
}
