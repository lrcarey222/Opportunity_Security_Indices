// Country URL state (spec_country.md A3). Pure. The conventions of the other modules: defaults are
// omitted, keys are written in one fixed order (cell partner all), invalid values are dropped and listed
// (the "Ignored in the link" notice), keys this module does not know are ignored silently.
//
//   cell     a sector key of the sheet ("oil-upstream"); default: the first high-priority sector
//   partner  an ISO3 among the sheet's partner rows; default: the top Partnership Strength partner
//   all      1 = every in-sample sector in the priorities table (else the top 15)
//
// Selecting a sector or a partner replaces the history entry; changing country pushes one.

import { isCellKey } from './contract.ts';

export interface CountryUrlState {
  cell: string | null;
  partner: string | null;
  all: boolean;
}

export const COUNTRY_KEY_ORDER = ['cell', 'partner', 'all'] as const;
export type CountryKey = (typeof COUNTRY_KEY_ORDER)[number];

export const COUNTRY_DEFAULTS: CountryUrlState = { cell: null, partner: null, all: false };

/** What the parser checks against (both optional: before the sheet loads only the syntax is checked). */
export interface CountryUrlCtx {
  /** the cell keys of the sheet */
  cells?: ReadonlySet<string>;
  /** the ISO3 codes of the sheet's partner rows */
  partners?: ReadonlySet<string>;
}

export interface CountryParsed {
  state: CountryUrlState;
  problems: string[];
}

const ISO3 = /^[A-Z0-9]{3}$/;

/** Read a query string. */
export function parseCountrySearch(search: string, ctx: CountryUrlCtx = {}): CountryParsed {
  const q = new URLSearchParams(search.startsWith('?') ? search.slice(1) : search);
  const state: CountryUrlState = { ...COUNTRY_DEFAULTS };
  const problems: string[] = [];
  const seen = new Set<string>();
  const bad = (k: string, v: string, why: string) => problems.push(`${k}=${v} (${why})`);
  for (const [k, raw] of q.entries()) {
    if (!(COUNTRY_KEY_ORDER as readonly string[]).includes(k)) continue;
    if (seen.has(k)) {
      bad(k, raw, 'repeated; the first value is used');
      continue;
    }
    seen.add(k);
    const v = raw.trim();
    switch (k as CountryKey) {
      case 'cell': {
        const c = v.toLowerCase();
        if (!isCellKey(c)) bad(k, raw, 'not a sector');
        else if (ctx.cells && !ctx.cells.has(c)) bad(k, raw, 'not a sector of this country');
        else state.cell = c;
        break;
      }
      case 'partner': {
        const p = v.toUpperCase();
        if (!ISO3.test(p)) bad(k, raw, 'not an ISO3 code');
        else if (ctx.partners && !ctx.partners.has(p)) bad(k, raw, 'not among the partners listed for this country');
        else state.partner = p;
        break;
      }
      case 'all':
        if (v === '1') state.all = true;
        else if (v !== '0' && v !== '') bad(k, raw, "'1' or nothing");
        break;
    }
  }
  return { state, problems };
}

/** Write a query string ("" or "?cell=..."): defaults omitted, fixed key order. */
export function serializeCountrySearch(s: Partial<CountryUrlState>): string {
  const q = new URLSearchParams();
  if (s.cell) q.set('cell', s.cell);
  if (s.partner) q.set('partner', s.partner);
  if (s.all) q.set('all', '1');
  const t = q.toString();
  return t ? `?${t}` : '';
}

/** The profile path of an economy, with optional state. */
export function countryHref(iso3: string, s: Partial<CountryUrlState> = {}, hash?: string): string {
  return `/countries/${iso3.toUpperCase()}${serializeCountrySearch(s)}${hash ? `#${hash}` : ''}`;
}
