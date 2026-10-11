// Page-level data notes (spec_sector.md 3.9, 4.0): caveats a card cannot show inline (beyond its first
// two) are added here and listed under "Data notes (n)" in the SectorBar, de-duplicated, in the order
// they were added. SectorLayout mounts PageNotesProvider around the SectorBar and the outlet; outside
// it (tests, a card on its own) adding is a no-op.

import { createContext, useCallback, useContext, useEffect, useMemo, useState, type ReactNode } from 'react';

interface PageNotesCtx {
  notes: readonly string[];
  add: (owner: string, notes: readonly string[]) => void;
  remove: (owner: string) => void;
}

const Ctx = createContext<PageNotesCtx | null>(null);

export function PageNotesProvider({ children }: { children: ReactNode }) {
  const [byOwner, setByOwner] = useState<ReadonlyMap<string, readonly string[]>>(new Map());
  const add = useCallback((owner: string, notes: readonly string[]) => {
    setByOwner((m) => {
      const prev = m.get(owner);
      if (prev && prev.length === notes.length && prev.every((n, i) => n === notes[i])) return m;
      const n = new Map(m);
      n.set(owner, [...notes]);
      return n;
    });
  }, []);
  const remove = useCallback((owner: string) => {
    setByOwner((m) => {
      if (!m.has(owner)) return m;
      const n = new Map(m);
      n.delete(owner);
      return n;
    });
  }, []);
  const notes = useMemo(() => [...new Set([...byOwner.values()].flat())], [byOwner]);
  const value = useMemo(() => ({ notes, add, remove }), [notes, add, remove]);
  return <Ctx.Provider value={value}>{children}</Ctx.Provider>;
}

/** The page's notes (empty outside a provider). */
export function usePageNotes(): readonly string[] {
  return useContext(Ctx)?.notes ?? [];
}

/** Add notes for as long as the caller is mounted (`owner` = a stable id, e.g. React's useId()). */
export function useAddPageNotes(owner: string, notes: readonly string[]): void {
  const ctx = useContext(Ctx);
  const add = ctx?.add;
  const remove = ctx?.remove;
  const key = JSON.stringify(notes);
  useEffect(() => {
    if (!add) return;
    const ns = JSON.parse(key) as string[];
    if (ns.length) add(owner, ns);
    else remove?.(owner);
  }, [add, remove, owner, key]);
  useEffect(() => (remove ? () => remove(owner) : undefined), [remove, owner]);
}
