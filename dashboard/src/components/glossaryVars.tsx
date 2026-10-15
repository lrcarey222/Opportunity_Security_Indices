// The glossary's data-dependent numbers (spec_country.md C3.2): a module that has loaded a manifest can
// supply newer values ({ n_core: '31', lambda: '0.78', ... }) for the glossary's {{placeholders}}; without
// a provider the glossary shows GLOSSARY_DEFAULTS. InfoTip, Term and GlossaryList read through here.

import { createContext, useContext, useMemo, type ReactNode } from 'react';
import { glossaryWith, type GlossaryEntry, type GlossaryVars } from '../content/glossary.ts';

const VarsContext = createContext<GlossaryVars>({});

export function GlossaryVarsProvider({ vars, children }: { vars: GlossaryVars; children: ReactNode }) {
  const parent = useContext(VarsContext);
  const key = JSON.stringify({ ...parent, ...vars });
  const value = useMemo(() => JSON.parse(key) as GlossaryVars, [key]);
  return <VarsContext.Provider value={value}>{children}</VarsContext.Provider>;
}

const cache = new Map<string, { list: GlossaryEntry[]; byId: Map<string, GlossaryEntry> }>();

/** The glossary with the placeholders of the nearest provider filled in. */
export function useGlossary(): { list: readonly GlossaryEntry[]; byId: ReadonlyMap<string, GlossaryEntry> } {
  const vars = useContext(VarsContext);
  const key = JSON.stringify(vars);
  return useMemo(() => {
    let hit = cache.get(key);
    if (!hit) {
      const list = glossaryWith(vars);
      hit = { list, byId: new Map(list.map((e) => [e.id, e])) };
      cache.set(key, hit);
    }
    return hit;
    // (vars is fully described by key)
  }, [key]);
}
