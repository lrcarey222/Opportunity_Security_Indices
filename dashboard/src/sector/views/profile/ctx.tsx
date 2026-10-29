// Profile context (spec_sector.md 4.3): the cell, its technology and stage, the loaded facts, the
// highlight and the EU mode, shared by the sections; plus the per-page CSV registry behind "Download
// this sector (.zip)" (every card's CSV, dictionary.csv, sources.csv, README.txt).

import { createContext, useCallback, useContext, useEffect, useId, useMemo, useRef, type ReactNode } from 'react';
import type { StageSlug, TechSlug } from '../../contract.ts';
import type { FactsIndex } from '../../data/facts.ts';
import type { GeoInfo, ItemInfo, SectorModel, VarInfo } from '../../data/model.ts';

export interface ProfileCtx {
  model: SectorModel;
  cell: ItemInfo;
  tech: TechSlug;
  stage: StageSlug;
  facts: FactsIndex | null;
  highlight: GeoInfo | null;
  eu: 'members' | 'bloc';
}

const Ctx = createContext<ProfileCtx | null>(null);

export function ProfileProvider({ value, children }: { value: ProfileCtx; children: ReactNode }) {
  return <Ctx.Provider value={value}>{children}</Ctx.Provider>;
}

export function useProfile(): ProfileCtx {
  const v = useContext(Ctx);
  if (!v) throw new Error('useProfile() outside the profile');
  return v;
}

// ---- CSV registry (the sector ZIP) -------------------------------------------------------------

export type CsvBuilder = () => { name: string; text: string } | null;

interface Registry {
  set: (id: string, entry: { build: CsvBuilder; vars: string[] } | null) => void;
  all: () => { build: CsvBuilder; vars: string[] }[];
}

const RegistryCtx = createContext<Registry | null>(null);

export function CsvRegistryProvider({ children }: { children: ReactNode }) {
  const map = useRef(new Map<string, { build: CsvBuilder; vars: string[] }>());
  const value = useMemo<Registry>(
    () => ({
      set: (id, entry) => {
        if (entry) map.current.set(id, entry);
        else map.current.delete(id);
      },
      all: () => [...map.current.values()],
    }),
    [],
  );
  return <RegistryCtx.Provider value={value}>{children}</RegistryCtx.Provider>;
}

/** Register a card's CSV for the sector ZIP; returns the same builder for ChartCard's `csv`. */
export function useCardCsv(build: CsvBuilder, vars: string[] = []): CsvBuilder {
  const reg = useContext(RegistryCtx);
  const id = useId();
  const ref = useRef(build);
  ref.current = build;
  const stable = useCallback(() => ref.current(), []);
  const key = vars.join('|');
  useEffect(() => {
    reg?.set(id, { build: stable, vars: key ? key.split('|') : [] });
    return () => reg?.set(id, null);
  }, [reg, id, stable, key]);
  return stable;
}

export function useCsvRegistry(): Registry | null {
  return useContext(RegistryCtx);
}

/** A var by id, or null (dictionary-driven sections skip vars the bundle does not have). */
export function vOf(model: SectorModel, id: string): VarInfo | null {
  return model.varById.get(id) ?? null;
}

/**
 * The item a var is measured on for a cell: the cell itself when the var has data for it, else the
 * economy-wide item (item axis none), else null. Linked items (minerals, fuels, commodities) are
 * handled by the sections that use them.
 */
export function itemForCell(model: SectorModel, v: VarInfo, cell: ItemInfo): ItemInfo | null {
  if (model.availOf(v, cell.idx).length) return cell;
  if (v.item_axis === 'none' && model.none && model.availOf(v, model.none.idx).length) return model.none;
  return null;
}

/** Linked items of an axis that have data for a var (fuels of Oil Extraction, minerals of Batteries Extraction). */
export function linkedWith(model: SectorModel, cell: ItemInfo, axis: 'mineral' | 'fuel' | 'commodity' | 'activity', v: VarInfo | null): ItemInfo[] {
  return model
    .itemsOfCell(cell.idx, axis)
    .map((l) => l.item)
    .filter((it) => !v || model.availOf(v, it.idx).length > 0);
}
