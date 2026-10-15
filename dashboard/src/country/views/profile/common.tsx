// Shared pieces of the Country profile (spec_country.md A4.2): the profile context (sheet, manifest
// entry, URL state and its setters), the section wrapper (an h2 with its anchor) and the CSV helper
// (columns in plain words, last two columns `source` and `baseline_as_of`, A4.0).

import { createContext, useContext, type ReactNode } from 'react';
import { rowsToCsv } from '../../../lib/csv.ts';
import type { CountryEntry, CountryManifest, DriversDoc, FactSheet } from '../../contract.ts';
import type { CountryUrlState } from '../../url.ts';

export interface ProfileCtx {
  sheet: FactSheet;
  entry: CountryEntry;
  manifest: CountryManifest;
  state: CountryUrlState;
  /** the sector of the "why" panel and the highlighted scatter point (URL `cell`, else the default) */
  cell: string | null;
  /** the selected partner (URL `partner`, else the top Partnership Strength partner) */
  partner: string | null;
  setCell: (key: string | null) => void;
  setPartner: (iso3: string | null) => void;
  setAll: (all: boolean) => void;
  /** the companion drivers file, when the selected sector needs it */
  driversDoc: DriversDoc | null;
  driversStatus: 'loading' | 'ready' | 'error' | 'idle';
  retryDrivers: () => void;
  /** the URL of a file of the data directory (downloads) */
  fileUrl: (file: string) => string;
}

const Ctx = createContext<ProfileCtx | null>(null);

export const ProfileProvider = Ctx.Provider;

export function useProfile(): ProfileCtx {
  const v = useContext(Ctx);
  if (!v) throw new Error('useProfile outside the Country profile');
  return v;
}

/** A page section: <section id> with an h2 (cards inside use h3). */
export function Section({ id, title, intro, children }: { id: string; title: string; intro?: ReactNode; children: ReactNode }) {
  return (
    <section id={id} aria-labelledby={`${id}-h`} className="scroll-mt-16">
      <h2 id={`${id}-h`} className="rule-neis-sm mb-2 text-xl font-medium text-paper lg:text-2xl">
        {title}
      </h2>
      {intro && <p className="mb-5 max-w-3xl text-[0.9375rem] leading-relaxed text-paper-dim">{intro}</p>}
      <div className="space-y-6">{children}</div>
    </section>
  );
}

/** CSV text with `source` and `baseline_as_of` as the last two columns. */
export function csvWithSource(rows: Record<string, unknown>[], source: string, asOf: string): string {
  return rowsToCsv(rows.map((r) => ({ ...r, source, baseline_as_of: asOf })));
}

/** A quiet right-aligned link row under a card. */
export function LinkRow({ children }: { children: ReactNode }) {
  return <div className="mt-2 flex flex-wrap justify-end gap-x-5 gap-y-1 text-sm">{children}</div>;
}
