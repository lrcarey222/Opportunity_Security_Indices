// Module navigation registration (spec_sector.md 2.2). The shell never imports module state: each
// module layout registers its tier-2 links (with whatever query they carry, e.g. Strategic's
// scenario) through useRegisterModuleNav(), and the header renders what is registered. Until a
// module registers, the header falls back to the static lists in modules.ts.

import { createContext, useCallback, useContext, useEffect, useMemo, useState, type ReactNode } from 'react';
import type { ModuleId } from './modules.ts';

export interface ModuleNavItem {
  /** stable key */
  key: string;
  label: string;
  /** pathname + optional search */
  to: string;
  /** the link is the current view (aria-current="page") */
  active?: boolean;
}

export interface ModuleNavState {
  module: ModuleId | null;
  items: readonly ModuleNavItem[];
}

interface ModuleNavCtx extends ModuleNavState {
  setItems: (module: ModuleId, items: readonly ModuleNavItem[] | null) => void;
}

const ModuleNavContext = createContext<ModuleNavCtx | null>(null);

const sameItems = (a: readonly ModuleNavItem[], b: readonly ModuleNavItem[]) =>
  a.length === b.length && a.every((x, i) => x.key === b[i].key && x.label === b[i].label && x.to === b[i].to && !!x.active === !!b[i].active);

export function ModuleNavProvider({ children }: { children: ReactNode }) {
  const [reg, setReg] = useState<ModuleNavState>({ module: null, items: [] });
  // stable, so registrations never re-run (and never clear themselves) when the registry changes
  const setItems = useCallback((module: ModuleId, items: readonly ModuleNavItem[] | null) => {
    setReg((r) => {
      if (items === null) return r.module === module ? { module: null, items: [] } : r;
      if (r.module === module && sameItems(r.items, items)) return r;
      return { module, items: [...items] };
    });
  }, []);
  const value = useMemo<ModuleNavCtx>(() => ({ ...reg, setItems }), [reg, setItems]);
  return <ModuleNavContext.Provider value={value}>{children}</ModuleNavContext.Provider>;
}

/** The registered nav (module null when none is registered). */
export function useModuleNav(): ModuleNavState {
  const v = useContext(ModuleNavContext);
  return v ? { module: v.module, items: v.items } : { module: null, items: [] };
}

/**
 * Register a module's tier-2 links for as long as the caller is mounted. Safe outside AppRoot (a
 * no-op), so a module layout can be rendered on its own in tests. `items` may be a new array on
 * every render: registration compares by value.
 */
export function useRegisterModuleNav(module: ModuleId, items: readonly ModuleNavItem[]): void {
  const ctx = useContext(ModuleNavContext);
  const setItems = ctx?.setItems;
  const key = JSON.stringify(items);
  useEffect(() => {
    if (!setItems) return;
    setItems(module, JSON.parse(key) as ModuleNavItem[]);
  }, [setItems, module, key]);
  useEffect(() => (setItems ? () => setItems(module, null) : undefined), [setItems, module]);
}
