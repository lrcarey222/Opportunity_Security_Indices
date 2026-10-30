// Posture counts (spec XI.6): a compact list for the Sector card; the Country tile (kept for the pages that
// still mount it); and the AIB division's three header metrics as StatTiles. The Country Strategy Map's counts
// sit in its legend (ChoiceLegend, one count per choice, grouped by fence) since 2026-10-30.

import { useMemo } from 'react';
import { StatTile } from '../components/ui.tsx';
import type { PostureCore } from './contract.ts';
import { aibHeaderTiles, countPostures, countryCells, postureCountsText, type PostureCounts } from './derive.ts';
import { POSTURE_COLOR, POSTURE_LABEL, POSTURE_ORDER, VERB_LABEL, VERB_ORDER } from './grammar.ts';
import { Swatch } from './PostureLegend.tsx';
import { usePostureCore } from './store.ts';

/** The counts as a description list ("Posture counts"): one chip per family, not assessed, then the six verbs. */
export function PostureCountsList({ counts, className }: { counts: PostureCounts; className?: string }) {
  return (
    <dl className={`flex flex-wrap gap-2 ${className ?? ''}`} aria-label="Posture counts" data-testid="posture-counts">
      {POSTURE_ORDER.map((p) => (
        <div key={p} className="flex items-center gap-2 rounded-md bg-ink-900/60 px-3 py-1.5">
          <Swatch color={POSTURE_COLOR[p]} size={12} />
          <dt className="text-xs text-paper-mute">{POSTURE_LABEL[p]}</dt>
          <dd className="tabular text-sm font-medium text-paper">{counts[p]}</dd>
        </div>
      ))}
      {counts.N > 0 && (
        <div className="flex items-center gap-2 rounded-md bg-ink-900/60 px-3 py-1.5">
          <Swatch hatched size={12} />
          <dt className="text-xs text-paper-mute">Not assessed</dt>
          <dd className="tabular text-sm font-medium text-paper">{counts.N}</dd>
        </div>
      )}
      <div className="flex items-center gap-1.5 rounded-md bg-ink-900/60 px-3 py-1.5 text-xs text-paper-mute">
        {VERB_ORDER.filter((v) => v !== 'secure').map((v) => (
          <span key={v}>
            {VERB_LABEL[v]} <span className="tabular text-paper">{counts[v]}</span>
          </span>
        ))}
      </div>
    </dl>
  );
}

export interface CountryPostureCountsLoad {
  status: 'loading' | 'ready' | 'error';
  /** null while loading, on an error, or when the economy has no classified node */
  counts: PostureCounts | null;
  /** the number of classified nodes (0: no posture) */
  nodes: number;
  error: string | null;
}

/** Pure: one economy's counts from a loaded core (null counts when it has no classified node). */
export function countryPostureCounts(core: PostureCore, iso3: string): { counts: PostureCounts | null; nodes: number } {
  const cells = countryCells(core, iso3);
  return { counts: cells.length ? countPostures(cells) : null, nodes: cells.length };
}

/** One economy's counts, loading the posture core (shared with the card: one fetch per page). */
export function useCountryPostureCounts(iso3: string): CountryPostureCountsLoad {
  const core = usePostureCore();
  return useMemo(() => {
    if (core.status === 'error') return { status: 'error', counts: null, nodes: 0, error: core.error };
    if (!core.data) return { status: 'loading', counts: null, nodes: 0, error: null };
    return { status: 'ready', ...countryPostureCounts(core.data, iso3), error: null };
  }, [core.status, core.data, core.error, iso3]);
}

/**
 * A Country tile: "STRATEGIC POSTURE", the three family counts with their swatches, the six verbs below,
 * and (with `link`) a link to the page's strategy section. The Country page's redesign drops its tile row
 * (spec XI.2); the tile stays for any page that still shows one.
 */
export function CountryPostureTile({ iso3, label = 'STRATEGIC POSTURE', link }: { iso3: string; label?: string; link?: { href: string; label: string } }) {
  const load = useCountryPostureCounts(iso3);
  const linkEl = link ? (
    <a className="link" href={link.href}>
      {link.label}
    </a>
  ) : null;
  if (load.status !== 'ready' || !load.counts) {
    const [value, sub] =
      load.status === 'loading'
        ? ['…', 'Loading the strategic posture']
        : load.status === 'error'
          ? ['Not available', 'The posture data could not be loaded']
          : ['No posture', 'No node is classified in the strategic position framework'];
    return <StatTile label={label} value={<span className="text-base">{value}</span>} sub={sub} />;
  }
  const n = load.counts;
  const words = postureCountsText(n);
  return (
    <StatTile
      label={label}
      value={
        <span className="flex flex-wrap items-center gap-x-3 gap-y-1 text-base" data-testid="posture-tile-counts">
          <span className="sr-only">{words.value}</span>
          {POSTURE_ORDER.map((p) => (
            <span key={p} className="inline-flex items-center gap-1.5" aria-hidden="true">
              <Swatch color={POSTURE_COLOR[p]} size={12} />
              {POSTURE_LABEL[p]} <span className="tabular">{n[p]}</span>
            </span>
          ))}
        </span>
      }
      sub={
        <span>
          {words.sub}
          {linkEl ? <> · {linkEl}</> : null}
        </span>
      }
    />
  );
}

/** The AIB division's three header metrics as a list of StatTiles (nothing while the core loads or fails: the card says why). */
export function AibDivisionStats({ label = 'The allied base at a glance', className }: { label?: string; className?: string }) {
  const core = usePostureCore();
  if (!core.data) return null;
  return (
    <ul className={`grid grid-cols-1 gap-3 sm:grid-cols-3 ${className ?? ''}`} aria-label={label} data-testid="aib-division-stats">
      {aibHeaderTiles(core.data).map((t) => (
        <li key={t.id}>
          <StatTile label={t.label} value={t.value} sub={t.sub} />
        </li>
      ))}
    </ul>
  );
}
