// The Sectoral Compacts tier-2 links: Compacts, Compact (the compact on screen, else the last visited one,
// else the first of the document) and About & data. AibLayout registers them with the shell; the shell's
// static AIB_NAV (shell/modules.ts) is the fallback before it does. Pure apart from the in-memory last id.

import { AIB_RESERVED } from './contract.ts';

export type AibNavKey = 'compacts' | 'compact' | 'about';

let lastId: string | null = null;

const segments = (pathname: string): string[] => pathname.split('/').filter(Boolean);

/** The compact id of /compacts/:id (as written in the path), or null. */
export function compactIdOfPath(pathname: string): string | null {
  const s = segments(pathname);
  if (s[0] !== 'compacts' || s.length !== 2) return null;
  return (AIB_RESERVED as readonly string[]).includes(s[1].toLowerCase()) ? null : decodeURIComponent(s[1]);
}

/** Which view a pathname shows. */
export function aibNavKeyOf(pathname: string): AibNavKey | null {
  const s = segments(pathname);
  if (s[0] !== 'compacts') return null;
  if (s.length === 1) return 'compacts';
  if (s[1].toLowerCase() === 'about') return 'about';
  return 'compact';
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

export function aibNavItems(pathname: string, firstId: string | null): { key: string; label: string; to: string; active?: boolean }[] {
  const active = aibNavKeyOf(pathname);
  const id = compactIdOfPath(pathname) ?? lastId ?? firstId;
  return [
    { key: 'compacts', label: 'Compacts', to: '/compacts', active: active === 'compacts' },
    ...(id ? [{ key: 'compact', label: 'Compact', to: compactHref(id), active: active === 'compact' }] : []),
    { key: 'about', label: 'About & data', to: '/compacts/about', active: active === 'about' },
  ];
}
