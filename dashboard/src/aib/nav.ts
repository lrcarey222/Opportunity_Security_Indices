// The Allied Industrial Base tier-2 links (IA refactor, 2026-10-16): Overview (/aib), Partnerships
// (/aib/partnerships), Sectoral Compacts (/compacts and everything under it), Country roles (/aib/roles)
// and Managed Interdependence (/aib/diversification, 2026-10-19: the diversification scenario, the
// explainer of the toggle's Managed Interdependence; the path is kept). /compacts/about keeps its path
// (About lists it). AibLayout registers them with the shell; the shell's static AIB_NAV (shell/modules.ts)
// is the same list before it does. Every link carries the scenario toggle (`scn`, scenarioUrl.ts). The
// last-compact memory stays for links that want "the compact you were reading". Pure apart from the
// in-memory last id.
//
// The strategic layer (2026-10-20): /compacts lands on the Strategic Sectoral Compacts; a strategic compact
// is /compacts/s/:sid (SC-... ids); the compact candidates, now its Workstreams, are listed at
// /compacts/workstreams and keep /compacts/:id (AIB-Cxx). All of them sit under the Sectoral Compacts link.

import { AIB_RESERVED, AIB_STRATEGIC_ID_RE } from './contract.ts';
import { scenarioCarry, withScenario, type AibToggle } from './scenarioUrl.ts';

export { withScenario } from './scenarioUrl.ts';

/** The nav label of the Managed Interdependence explainer (/aib/diversification). */
export const MI_NAV_LABEL = 'Managed Interdependence';
/** The nav label of the Sectoral Compacts (the strategic compacts, their workstreams and every compact page). */
export const COMPACTS_NAV_LABEL = 'Sectoral Compacts';
/** What the compact candidates are called in the pages since 2026-10-20 (/compacts/workstreams). */
export const WORKSTREAMS_LABEL = 'Workstreams';

/** The path segment of the Workstreams index (/compacts/workstreams) and the strategic compacts' prefix (/compacts/s/:sid). */
export const WORKSTREAMS_SEGMENT = 'workstreams';
export const STRATEGIC_SEGMENT = 's';

export type AibNavKey = 'overview' | 'partnerships' | 'compacts' | 'roles' | 'diversification' | 'about';

/** Which Sectoral Compacts page a pathname shows: the strategic landing, a strategic compact, the Workstreams index, a workstream (AIB-Cxx) or About. */
export type CompactsPage = 'strategic' | 'strategic-compact' | 'workstreams' | 'workstream' | 'about';

let lastId: string | null = null;

const segments = (pathname: string): string[] => pathname.split('/').filter(Boolean);

/** The compact (workstream) id of /compacts/:id (as written in the path), or null. */
export function compactIdOfPath(pathname: string): string | null {
  const s = segments(pathname);
  if (s[0] !== 'compacts' || s.length !== 2) return null;
  return (AIB_RESERVED as readonly string[]).includes(s[1].toLowerCase()) ? null : decodeURIComponent(s[1]);
}

/** The strategic compact id of /compacts/s/:sid (as written in the path), or null. */
export function strategicIdOfPath(pathname: string): string | null {
  const s = segments(pathname);
  if (s[0] !== 'compacts' || s.length !== 3 || s[1].toLowerCase() !== STRATEGIC_SEGMENT) return null;
  return decodeURIComponent(s[2]);
}

/** Which Sectoral Compacts page a pathname shows (null outside /compacts). */
export function compactsPageOf(pathname: string): CompactsPage | null {
  const s = segments(pathname);
  if (s[0] !== 'compacts') return null;
  if (s.length === 1) return 'strategic';
  const v = s[1].toLowerCase();
  if (v === 'about') return 'about';
  if (v === WORKSTREAMS_SEGMENT) return 'workstreams';
  if (v === STRATEGIC_SEGMENT) return s.length > 2 ? 'strategic-compact' : 'strategic';
  return 'workstream';
}

/** Which view a pathname shows (every Sectoral Compacts page but About belongs to Sectoral Compacts). */
export function aibNavKeyOf(pathname: string): AibNavKey | null {
  const s = segments(pathname);
  if (s[0] === 'aib') {
    if (s.length === 1) return 'overview';
    const v = s[1].toLowerCase();
    return v === 'partnerships' ? 'partnerships' : v === 'roles' ? 'roles' : v === 'diversification' ? 'diversification' : null;
  }
  if (s[0] !== 'compacts') return null;
  return s.length > 1 && s[1].toLowerCase() === 'about' ? 'about' : 'compacts';
}

/** Remember the compact on screen (the Compact link's target). */
export function rememberCompact(id: string | null): void {
  if (id) lastId = id;
}

export function lastCompact(): string | null {
  return lastId;
}

/** Forget the last compact (tests). */
export function resetLastCompact(): void {
  lastId = null;
}

const carry = (h: string, scenario?: AibToggle): string => (scenario ? withScenario(h, scenario) : h);

/** The href of a compact candidate, now a Workstream (/compacts/AIB-C01; with `scenario`: the toggle carried, withScenario). */
export const compactHref = (id: string, scenario?: AibToggle): string => carry(`/compacts/${encodeURIComponent(id)}`, scenario);

/** The href of a workstream (the same page as compactHref; the name the strategic pages use). */
export const workstreamHref = compactHref;

/** The href of a strategic compact (/compacts/s/SC-MAGNETS; `hash`: a section of the page; with `scenario`: the toggle carried). */
export const strategicCompactHref = (id: string, scenario?: AibToggle, hash?: string): string =>
  carry(`/compacts/${STRATEGIC_SEGMENT}/${encodeURIComponent(id)}${hash ? `#${hash}` : ''}`, scenario);

/** The Strategic Sectoral Compacts landing (/compacts). */
export const strategicCompactsHref = (scenario?: AibToggle): string => carry('/compacts', scenario);

/**
 * The Workstreams index (/compacts/workstreams; `compact`: a strategic compact whose workstreams to show,
 * as #sc-SC-MAGNETS for the page to scroll to; with `scenario`: the toggle carried).
 */
export const workstreamsHref = (scenario?: AibToggle, compact?: string | null): string => carry(`/compacts/${WORKSTREAMS_SEGMENT}${compact ? `#sc-${compact}` : ''}`, scenario);

/** A strategic compact id ("SC-...") rather than a workstream id (AIB-Cxx): /compacts/:id sends it to /compacts/s/:id. */
export const isStrategicId = (id: string | null | undefined): boolean => !!id && AIB_STRATEGIC_ID_RE.test(id.toUpperCase());

/**
 * The href of the Country roles view; with an economy, its strategic summary chosen (`economy=ISO3`) and the
 * page scrolled to its operational row (#role-ISO3); with `scenario`: the toggle carried.
 */
export const rolesHref = (iso3?: string | null, scenario?: AibToggle): string => {
  if (!iso3) return carry('/aib/roles', scenario);
  const iso = iso3.toUpperCase();
  return carry(`/aib/roles?economy=${encodeURIComponent(iso)}#role-${iso}`, scenario);
};

/** The AIB Partnerships page for a pair (its relationship thesis and three lenses), the toggle carried. */
export const pairHref = (a: string, b: string, scenario?: AibToggle): string =>
  carry(`/aib/partnerships?country=${encodeURIComponent(a.toUpperCase())}&partner=${encodeURIComponent(b.toUpperCase())}`, scenario);

/**
 * The tier-2 links; `search` (the current query) carries the scenario toggle onto every link.
 * `_firstId` is kept for the callers: the nav no longer names a compact.
 */
export function aibNavItems(pathname: string, _firstId?: string | null, search = ''): { key: string; label: string; to: string; active?: boolean }[] {
  const active = aibNavKeyOf(pathname);
  const q = scenarioCarry(search);
  return [
    { key: 'overview', label: 'Overview', to: `/aib${q}`, active: active === 'overview' },
    { key: 'partnerships', label: 'Partnerships', to: `/aib/partnerships${q}`, active: active === 'partnerships' },
    { key: 'compacts', label: COMPACTS_NAV_LABEL, to: `/compacts${q}`, active: active === 'compacts' },
    { key: 'roles', label: 'Country roles', to: `/aib/roles${q}`, active: active === 'roles' },
    { key: 'diversification', label: MI_NAV_LABEL, to: `/aib/diversification${q}`, active: active === 'diversification' },
  ];
}
