// Shared pieces of one compact's page: its context (the compact, its nodes, names, meta) and an
// economy's name that never prints a bare code (an <abbr> with the code when the data have no name).

import { createContext, useContext } from 'react';
import type { AibCompact, AibMeta, AibNode } from '../../contract.ts';

export interface CompactCtx {
  c: AibCompact;
  /** the compact's nodes, in its own order */
  nodes: AibNode[];
  meta: AibMeta;
  names: Readonly<Record<string, string>>;
  name: (iso3: string | null | undefined) => string;
  nodeLabel: (id: string) => string;
  shockLabel: string;
}

const Ctx = createContext<CompactCtx | null>(null);
export const CompactProvider = Ctx.Provider;

export function useCompact(): CompactCtx {
  const v = useContext(Ctx);
  if (!v) throw new Error('useCompact outside a compact page');
  return v;
}

/** An economy by name; its code in an <abbr> when the data have no name for it. */
export function Economy({ iso3 }: { iso3: string | null | undefined }) {
  const { names } = useCompact();
  if (!iso3) return <>–</>;
  const n = names[iso3];
  return n ? <>{n}</> : <abbr title={`Economy code ${iso3}`}>{iso3}</abbr>;
}

