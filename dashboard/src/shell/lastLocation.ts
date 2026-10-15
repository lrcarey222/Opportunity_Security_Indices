// The last location per module (spec_sector.md 2.5, 2.6): an in-memory { strategic, sector } of
// pathname + search, updated by a route-change effect in AppRoot. No storage: a reload starts from
// the URL. Module tabs and Home's buttons link through these helpers. Pure apart from the store.

import { SCENARIO_KEYS, SHARED_VIEW_KEYS } from '../state/url.ts';
import { isShellStageSlug, isShellTechSlug } from './sectorSlugs.ts';
import { placeOf, sectorCellOfPath, SECTOR_NAV } from './modules.ts';

export interface LastLocations {
  strategic: string | null;
  sector: string | null;
  /** the last Sector profile cell, "/sector/batteries/midstream" (PROFILE's target) */
  sectorCell: string | null;
}

const store: LastLocations = { strategic: null, sector: null, sectorCell: null };

/** Record a location (called on every route change). Home and unknown paths are not recorded. */
export function rememberLocation(pathname: string, search: string): void {
  const place = placeOf(pathname);
  if (place === 'strategic') store.strategic = pathname + search;
  else if (place === 'sector') {
    store.sector = pathname + search;
    const cell = sectorCellOfPath(pathname);
    if (cell) store.sectorCell = `/sector/${cell.tech}/${cell.stage}`;
  }
}

export function lastLocations(): Readonly<LastLocations> {
  return { ...store };
}

/** Forget every location (tests). */
export function resetLastLocations(): void {
  store.strategic = null;
  store.sector = null;
  store.sectorCell = null;
}

const CARRY = new Set<string>([...SCENARIO_KEYS, ...SHARED_VIEW_KEYS]);

/**
 * The Strategic keys that travel between Strategic views (the scenario plus show / tech / stage), in
 * their written order: the same set as state/url.ts navSearch(), read off a canonical search string.
 */
export function strategicCarry(search: string): string {
  const q = new URLSearchParams(search);
  const out = new URLSearchParams();
  for (const [k, v] of q) if (CARRY.has(k)) out.append(k, v);
  const s = out.toString();
  return s ? `?${s}` : '';
}

/** Keys the Sector module keeps when moving between its views (2.7). */
const SECTOR_KEEP = ['c', 'eu'];

/** The c and eu keys of a Sector search. */
export function sectorCarry(search: string): string {
  const q = new URLSearchParams(search);
  const out = new URLSearchParams();
  for (const k of SECTOR_KEEP) {
    const v = q.get(k);
    if (v) out.set(k, v);
  }
  const s = out.toString();
  return s ? `?${s}` : '';
}

const ISO3 = /^[A-Z]{3}$/;
const slugOf = (label: string | null): string | null => (label ? label.trim().toLowerCase().replace(/\s+/g, '-') : null);

/** Set (or keep) `c` on a path + search. */
function withC(href: string, c: string | null): string {
  if (!c) return href;
  const i = href.indexOf('?');
  const path = i < 0 ? href : href.slice(0, i);
  const q = new URLSearchParams(i < 0 ? '' : href.slice(i + 1));
  q.set('c', c);
  // keep the Sector key order (c first, then the rest as they were)
  const rest = [...q.entries()].filter(([k]) => k !== 'c');
  const out = new URLSearchParams([['c', c], ...rest]);
  return `${path}?${out.toString()}`;
}

/** The country a Strategic location is about (/country/:iso3, /strategic/:iso3, /partners/:iso3). */
export function strategicCountry(pathname: string): string | null {
  const m = /^\/(country|strategic|partners)\/([A-Za-z]{3})\/?$/.exec(pathname);
  const iso = m ? m[2].toUpperCase() : null;
  return iso && ISO3.test(iso) ? iso : null;
}

/**
 * Where the SECTOR INSIGHTS tab goes from `pathname + search` (2.6):
 *   * inside Sector: the Sector landing, keeping c and eu;
 *   * from Strategic with tech and stage set: /sector/:tech/:stage?c=…;
 *   * else the last Sector location (with c added when Strategic names a country);
 *   * else /sector?c=… (or /sector).
 */
export function sectorTabHref(pathname: string, search: string, last: Readonly<LastLocations> = store): string {
  const place = placeOf(pathname);
  if (place === 'sector') return `/sector${sectorCarry(search)}`;
  let c: string | null = null;
  if (place === 'strategic') {
    c = strategicCountry(pathname);
    const q = new URLSearchParams(search);
    const tech = slugOf(q.get('tech'));
    const stage = slugOf(q.get('stage'));
    if (tech && stage && isShellTechSlug(tech) && isShellStageSlug(stage)) return withC(`/sector/${tech}/${stage}`, c);
  }
  if (last.sector) return withC(last.sector, c);
  return withC('/sector', c);
}

/**
 * Where the STRATEGIC INSIGHTS tab goes: inside Strategic, the Overview with the scenario carried;
 * elsewhere the last Strategic location unchanged (scenario kept), else /overview. It never
 * silently changes the Strategic view.
 */
export function strategicTabHref(pathname: string, search: string, last: Readonly<LastLocations> = store): string {
  if (placeOf(pathname) === 'strategic') return `/overview${strategicCarry(search)}`;
  return last.strategic ?? '/overview';
}

/** Home's OPEN buttons: the last location of the module, else its landing. */
export function moduleResumeHref(id: 'sector' | 'strategic', last: Readonly<LastLocations> = store): string {
  return id === 'sector' ? (last.sector ?? '/sector') : (last.strategic ?? '/overview');
}

/** PROFILE in the Sector nav: the last visited cell, else Batteries — Midstream. */
export function sectorProfileHref(last: Readonly<LastLocations> = store): string {
  return last.sectorCell ?? (SECTOR_NAV.find((n) => n.key === 'profile')?.to as string);
}
