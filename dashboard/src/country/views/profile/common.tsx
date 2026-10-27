// Shared pieces of the Country profile (spec_country.md A4.2; IA refactor 2026-10-16): the profile context
// (sheet, manifest entry, URL state and its setters, the disclosures lifted to the page so they survive
// the engine mounting, the Allied Industrial Base document), the Perspective context (the Strategic Index
// under the reader's dials, else null = the NEIS baseline), the section wrapper (an h2 with its anchor and
// its basis eyebrow), the controlled disclosure (Fold) and the CSV helper (columns in plain words, last
// two columns `source` and `baseline_as_of`, A4.0).

import { createContext, useContext, type ReactNode } from 'react';
import { rowsToCsv } from '../../../lib/csv.ts';
import { Loading } from '../../../components/ui.tsx';
import { useScenarioOptional } from '../../../state/EngineProvider.tsx';
import type { AibDoc } from '../../../aib/contract.ts';
import type { CountryEntry, CountryManifest, DriversDoc, FactSheet } from '../../contract.ts';
import type { SiMap } from '../../derive/framing.ts';
import type { CountryUrlState, PartnerTab } from '../../url.ts';

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
  // ---- IA refactor 2026-10-16 (optional so older callers and tests can build a context without them)
  /** the Partnerships tab (URL `ptab`) */
  setPtab?: (t: PartnerTab) => void;
  /** a disclosure of the page is open (lifted: the engine mounting re-renders the body) */
  isOpen?: (id: string) => boolean;
  setOpen?: (id: string, open: boolean) => void;
  /** open a disclosure and scroll to it */
  reveal?: (id: string) => void;
  /** mount the model (the Perspective, or a block that follows it) */
  wantEngine?: () => void;
  /** set Strategic view keys (tech, stage) on the URL with the country's own state */
  setStrategicView?: (pairs: [string, string][]) => void;
  /** the Allied Industrial Base document (null while loading or when unavailable) */
  aib?: { doc: AibDoc | null; status: 'loading' | 'ready' | 'error'; names: Readonly<Record<string, string>> };
  /** the URL carries Perspective (dial) settings */
  customInUrl?: boolean;
}

const Ctx = createContext<ProfileCtx | null>(null);

export const ProfileProvider = Ctx.Provider;

export function useProfile(): ProfileCtx {
  const v = useContext(Ctx);
  if (!v) throw new Error('useProfile outside the Country profile');
  return v;
}

// ---- the Perspective ---------------------------------------------------------------------------------

/** The Strategic Index under the reader's Perspective. `map` null = the NEIS baseline (the fact sheet). */
export interface PerspectiveSi {
  status: 'baseline' | 'loading' | 'custom';
  map: SiMap | null;
  /** a newer run is on its way */
  updating: boolean;
}

export const BASELINE_SI: PerspectiveSi = { status: 'baseline', map: null, updating: false };
export const LOADING_SI: PerspectiveSi = { status: 'loading', map: null, updating: true };

const SiCtx = createContext<PerspectiveSi>(BASELINE_SI);
export const PerspectiveSiProvider = SiCtx.Provider;
export const usePerspectiveSi = (): PerspectiveSi => useContext(SiCtx);

// ---- facts and preferences (plan.md 4) -----------------------------------------------------------

export type Basis = 'facts' | 'prefs';
export const BASIS_LABEL: Record<Basis, string> = { facts: 'Observed / modelled facts', prefs: 'Preference-adjusted priorities' };

/** The basis of a block, as a quiet tag ("Preference-adjusted priorities · your perspective"). */
export function BasisTag({ kind, note }: { kind: Basis; note?: string }) {
  return (
    // `.tag` is unlayered CSS with white-space: nowrap, so a Tailwind `whitespace-normal` loses: a long note
    // (e.g. the policy section's) would push a 390 px page sideways; the inline style wins
    <span className="tag max-w-full" style={{ whiteSpace: 'normal' }} data-basis={kind}>
      {BASIS_LABEL[kind]}
      {note ? ` · ${note}` : ''}
    </span>
  );
}

/** "NEIS baseline" / "your perspective" / "NEIS baseline (model loading)". */
export function perspectiveNote(si: PerspectiveSi): string {
  return si.status === 'custom' ? 'your perspective' : si.status === 'loading' ? 'NEIS baseline until the model loads' : 'NEIS baseline';
}

/** A page section: <section id> with an h2 (cards inside use h3); `basis` adds the eyebrow above the h2. */
export function Section({
  id, title, intro, basis, level = 2, aliases, children,
}: { id: string; title: string; intro?: ReactNode; basis?: ReactNode; level?: 2 | 3; aliases?: readonly string[]; children: ReactNode }) {
  const H = level === 3 ? 'h3' : 'h2';
  return (
    <section id={id} aria-labelledby={`${id}-h`} className="scroll-mt-16">
      {/* old section ids (links from before the IA refactor) still scroll here */}
      {aliases?.map((a) => <span key={a} id={a} className="block scroll-mt-16" aria-hidden="true" />)}
      {basis && <div className="mb-1.5 flex flex-wrap gap-1.5">{basis}</div>}
      <H id={`${id}-h`} className={level === 3 ? 'rule-neis-sm mb-2 text-base font-medium text-paper lg:text-lg' : 'rule-neis-sm mb-2 text-xl font-medium text-paper lg:text-2xl'}>
        {title}
      </H>
      {intro && <p className="mb-5 max-w-3xl text-[0.9375rem] leading-relaxed text-paper-dim">{intro}</p>}
      <div className="space-y-6">{children}</div>
    </section>
  );
}

/**
 * A disclosure whose open state lives on the page (ProfileCtx.isOpen / setOpen), so it survives the
 * engine mounting; the content renders only while open. `engine` mounts the model on opening.
 */
export function Fold({
  id, summary, children, engine, badge, onOpen,
}: { id: string; summary: ReactNode; children: ReactNode; engine?: boolean; badge?: ReactNode; onOpen?: () => void }) {
  const p = useProfile();
  // a block of the model renders only inside the page's ready EngineScope (else: still loading)
  const engineReady = useScenarioOptional() !== null;
  const open = p.isOpen?.(id) ?? false;
  const toggle = () => {
    if (!open && engine) p.wantEngine?.();
    if (!open) onOpen?.();
    p.setOpen?.(id, !open);
  };
  return (
    <details id={`fold-${id}`} className="disclosure scroll-mt-16 border-t hairline" open={open}>
      <summary
        className="flex items-center gap-2 py-2.5 text-sm font-medium text-paper-dim hover:text-paper"
        onClick={(e) => {
          e.preventDefault();
          toggle();
        }}
      >
        <span className="disclosure-chevron text-paper-mute" aria-hidden="true">
          ›
        </span>
        <span className="flex-1">{summary}</span>
        {badge}
      </summary>
      {open && <div className="space-y-6 pb-4 pt-1">{engine && !engineReady ? <Loading label="Loading the model" /> : children}</div>}
    </details>
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
