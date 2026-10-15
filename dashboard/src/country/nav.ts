// The Country Insights tier-2 links (spec_country.md A2.1): Countries, Country profile (the last
// visited country, else the United States) and About & data. CountryLayout registers them with the
// shell; the shell's static COUNTRY_NAV (shell/modules.ts) is the fallback before it does. Pure apart
// from the in-memory last ISO3.

import { COUNTRY_RESERVED } from './contract.ts';

export type CountryNavKey = 'countries' | 'profile' | 'about';

let lastIso: string | null = null;

/** The ISO3 of a profile or memo path (/countries/JPN, /countries/JPN/memo), or null. */
export function countryIsoOfPath(pathname: string): string | null {
  const m = /^\/countries\/([A-Za-z0-9]{3})(\/memo)?\/?$/.exec(pathname);
  if (!m) return null;
  const s = m[1].toLowerCase();
  if ((COUNTRY_RESERVED as readonly string[]).includes(s)) return null;
  return m[1].toUpperCase();
}

/** Which Country view a pathname shows. */
export function countryNavKeyOf(pathname: string): CountryNavKey | null {
  const s = pathname.split('/').filter(Boolean);
  if (s[0] !== 'countries') return null;
  if (s.length === 1) return 'countries';
  if (s[1] === 'about') return 'about';
  return 'profile';
}

/** Remember the profile ISO3 (PROFILE's target). */
export function rememberCountry(iso3: string | null): void {
  if (iso3) lastIso = iso3.toUpperCase();
}

export function lastCountry(): string | null {
  return lastIso;
}

export function countryNavItems(pathname: string): { key: string; label: string; to: string; active?: boolean }[] {
  const iso = countryIsoOfPath(pathname) ?? lastIso ?? 'USA';
  const active = countryNavKeyOf(pathname);
  return [
    { key: 'countries', label: 'Countries', to: '/countries', active: active === 'countries' },
    { key: 'profile', label: 'Country profile', to: `/countries/${iso}`, active: active === 'profile' },
    { key: 'about', label: 'About & data', to: '/countries/about', active: active === 'about' },
  ];
}
