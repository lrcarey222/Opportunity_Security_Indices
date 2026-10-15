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
export const partnersLink = (iso3: string) => strategicLink(`/partners/${iso3}`);
export const spaceLink = (c: CellFacts | null) => strategicLink('/space', c ? { tech: c.tech, stage: c.stage } : {});
/** The network view of one stage (its path segment is the cell key, lib/format.ts stageSlug()). */
export const networkLink = (c: CellFacts | null) => strategicLink(c ? `/network/${c.key}` : '/network');

/** Sector Insights with the country highlighted. */
export const sectorCountryLink = (iso3: string) => `/sector?c=${iso3}`;
