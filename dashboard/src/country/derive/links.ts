// Bridge links from Country Insights to the other modules (spec_country.md A2.2): Strategic links keep
// the remembered scenario (the dial keys of the last Strategic location) and set the view's own keys;
// Sector links highlight the country with `c`.

import { lastLocations, strategicCarry } from '../../shell/lastLocation.ts';
import type { CellFacts } from '../contract.ts';

/** A Strategic path with the remembered scenario keys and `set` (tech, stage, ...) merged in. */
export function strategicLink(path: string, set: Record<string, string> = {}): string {
  const last = lastLocations().strategic;
  const i = last ? last.indexOf('?') : -1;
  const carried = strategicCarry(last && i >= 0 ? last.slice(i) : '');
  const q = new URLSearchParams(carried.startsWith('?') ? carried.slice(1) : carried);
  // a view's own tech/stage replace the carried ones; the others keep their order
  for (const [k, v] of Object.entries(set)) q.set(k, v);
  const s = q.toString();
  return `${path}${s ? `?${s}` : ''}`;
}

export const strategicCountryLink = (iso3: string) => strategicLink(`/strategic/${iso3}`);
export const countryScoresLink = (iso3: string) => strategicLink(`/country/${iso3}`);
/** The Partners view of an economy; with a sector, that sector's partners (`tech`, `stage`). */
export const partnersLink = (iso3: string, c?: { tech: string; stage: string } | null) =>
  strategicLink(`/partners/${iso3}`, c ? { tech: c.tech, stage: c.stage } : {});
export const spaceLink = (c: CellFacts | null) => strategicLink('/space', c ? { tech: c.tech, stage: c.stage } : {});

/** Sector Insights with the country highlighted. */
export const sectorCountryLink = (iso3: string) => `/sector?c=${iso3}`;

const slug = (s: string) => s.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '');

/**
 * The raw development data behind Development Potential (World Bank indicators, OECD aid flows, IMF
 * direct-investment positions) in Sector Insights: with a sector, the partner's page in that sector at
 * its "Aid and investment partners" section; else the World Bank indicators with the partner highlighted.
 */
export const devDataLink = (iso3: string | null, c?: { tech: string; stage: string } | null) =>
  c && iso3
    ? `/sector/${slug(c.tech)}/${slug(c.stage)}/${iso3}#aid-investment`
    : `/sector/explore/development?m=development.wdi${iso3 ? `&c=${iso3}` : ''}`;

/** The Sector profile of one sector (technology and stage) with the economy highlighted (IA refactor 2026-10-16). */
export const sectorProfileLink = (c: { tech: string; stage: string }, iso3?: string | null) =>
  `/sector/${slug(c.tech)}/${slug(c.stage)}${iso3 ? `?c=${iso3}` : ''}`;
