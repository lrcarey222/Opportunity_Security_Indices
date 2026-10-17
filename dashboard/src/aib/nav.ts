// The Allied Industrial Base tier-2 links (IA refactor, 2026-10-16): Overview (/aib), Partnerships
// (/aib/partnerships), Sectoral Compacts (/compacts and every /compacts/:id) and Country roles
// (/aib/roles). /compacts/about keeps its path (About lists it). AibLayout registers them with the shell;
// the shell's static AIB_NAV (shell/modules.ts) is the same list before it does. The last-compact memory
// stays for links that want "the compact you were reading". Pure apart from the in-memory last id.

import { AIB_RESERVED } from './contract.ts';

export type AibNavKey = 'overview' | 'partnerships' | 'compacts' | 'roles' | 'about';

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
    return v === 'partnerships' ? 'partnerships' : v === 'roles' ? 'roles' : null;
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

/** The href of a compact. */
export const compactHref = (id: string): string => `/compacts/${encodeURIComponent(id)}`;

/** The href of the Country roles view, optionally scrolled to one economy's row. */
export const rolesHref = (iso3?: string | null): string => (iso3 ? `/aib/roles#role-${iso3.toUpperCase()}` : '/aib/roles');

// `_firstId` is kept for the callers: the nav no longer names a compact
export function aibNavItems(pathname: string, _firstId?: string | null): { key: string; label: string; to: string; active?: boolean }[] {
  const active = aibNavKeyOf(pathname);
  return [
    { key: 'overview', label: 'Overview', to: '/aib', active: active === 'overview' },
    { key: 'partnerships', label: 'Partnerships', to: '/aib/partnerships', active: active === 'partnerships' },
    { key: 'compacts', label: 'Sectoral Compacts', to: '/compacts', active: active === 'compacts' },
    { key: 'roles', label: 'Country roles', to: '/aib/roles', active: active === 'roles' },
  ];
}
