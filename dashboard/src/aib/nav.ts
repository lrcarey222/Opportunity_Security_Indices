// The Allied Industrial Base tier-2 links (IA refactor, 2026-10-16): Overview (/aib), Partnerships
// (/aib/partnerships), Sectoral Compacts (/compacts and every /compacts/:id), Country roles (/aib/roles)
// and Managed Interdependence (/aib/diversification, 2026-10-19: the diversification scenario, the
// explainer of the toggle's Managed Interdependence; the path is kept). /compacts/about keeps its path
// (About lists it). AibLayout registers them with the shell; the shell's static AIB_NAV (shell/modules.ts)
// is the same list before it does. Every link carries the scenario toggle (`scn`, scenarioUrl.ts). The
// last-compact memory stays for links that want "the compact you were reading". Pure apart from the
// in-memory last id.

import { AIB_RESERVED } from './contract.ts';
import { scenarioCarry, withScenario, type AibToggle } from './scenarioUrl.ts';

export { withScenario } from './scenarioUrl.ts';

/** The nav label of the Managed Interdependence explainer (/aib/diversification). */
export const MI_NAV_LABEL = 'Managed Interdependence';

export type AibNavKey = 'overview' | 'partnerships' | 'compacts' | 'roles' | 'diversification' | 'about';

let lastId: string | null = null;

const segments = (pathname: string): string[] => pathname.split('/').filter(Boolean);

/** The compact id of /compacts/:id (as written in the path), or null. */
export function compactIdOfPath(pathname: string): string | null {
  const s = segments(pathname);
  if (s[0] !== 'compacts' || s.length !== 2) return null;
  return (AIB_RESERVED as readonly string[]).includes(s[1].toLowerCase()) ? null : decodeURIComponent(s[1]);
}

/** Which view a pathname shows (a compact's page belongs to Sectoral Compacts). */
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

/** The href of a compact (with `scenario`: the toggle carried, withScenario). */
export const compactHref = (id: string, scenario?: AibToggle): string => {
  const h = `/compacts/${encodeURIComponent(id)}`;
  return scenario ? withScenario(h, scenario) : h;
};

/** The href of the Country roles view, optionally scrolled to one economy's row (with `scenario`: the toggle carried). */
export const rolesHref = (iso3?: string | null, scenario?: AibToggle): string => {
  const h = iso3 ? `/aib/roles#role-${iso3.toUpperCase()}` : '/aib/roles';
  return scenario ? withScenario(h, scenario) : h;
};

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
    { key: 'compacts', label: 'Sectoral Compacts', to: `/compacts${q}`, active: active === 'compacts' },
    { key: 'roles', label: 'Country roles', to: `/aib/roles${q}`, active: active === 'roles' },
    { key: 'diversification', label: MI_NAV_LABEL, to: `/aib/diversification${q}`, active: active === 'diversification' },
  ];
}
