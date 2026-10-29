// Country URL state (spec_country.md A3). Pure. The conventions of the other modules: defaults are
// omitted, keys are written in one fixed order (cell partner all ptab), invalid values are dropped and listed
// (the "Ignored in the link" notice), keys this module does not know are ignored silently.
//
//   cell     a sector key of the sheet ("oil-extraction": a technology at one of the six supply-chain stages);
//            default: the first high-priority sector. An old three-stage key (design U1): <tech>-downstream
//            reads as <tech>-deployment with a notice; <tech>-upstream and <tech>-midstream split into several
//            stages, so they are dropped with a notice naming the stages that replaced them
//   partner  an ISO3 among the sheet's partner rows; default: the top Partnership Strength partner
//   all      1 = every in-sample sector in the priorities table (else the top 15)
//   ptab     the Partnerships tab: trade (default), investment, development (IA refactor 2026-10-16) or
//            geopolitical (2026-10-19)
//   scn      the AIB scenario toggle (2026-10-19; src/aib/scenarioUrl.ts): decoupling, else absent (Managed
//            Interdependence, the default since 2026-10-20; an older link's scn=managed reads as the default
//            and is dropped without a notice). A host key here, so the embedded Strategic views keep it and
//            the profile's own writes carry it; the AIB lens of the Partnerships section and the AIB role
//            section follow it.
//
// Selecting a sector or a partner replaces the history entry; changing country pushes one.

import { legacyStageNote, legacyStageOfSlug, legacyStageTarget } from '../lib/stages.ts';
import { COUNTRY_TECHS, isCellKey } from './contract.ts';

export type PartnerTab = 'trade' | 'investment' | 'development' | 'geopolitical';
export const PARTNER_TABS: readonly PartnerTab[] = ['trade', 'investment', 'development', 'geopolitical'];

export interface CountryUrlState {
  cell: string | null;
  partner: string | null;
  all: boolean;
  ptab: PartnerTab;
  /** the AIB scenario toggle: 'decoupling', or absent (Managed Interdependence, the default) */
  scn?: 'decoupling';
}

export const COUNTRY_KEY_ORDER = ['cell', 'partner', 'all', 'ptab', 'scn'] as const;
export type CountryKey = (typeof COUNTRY_KEY_ORDER)[number];

export const COUNTRY_DEFAULTS: CountryUrlState = { cell: null, partner: null, all: false, ptab: 'trade' };

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
const LEGACY_CELL = /^([a-z0-9]+(?:-[a-z0-9]+)*)-(upstream|midstream|downstream)$/;

/**
 * An old three-stage cell key of a Country technology ("oil-downstream"; design U1): the six-stage key it
 * maps to exactly (Downstream is Deployment) or null (Upstream and Midstream split), and the notice's
 * words ("Oil — Downstream is now Oil — Deployment"); null when the value is not such a key.
 */
export function legacyCellOf(v: string): { to: string | null; note: string } | null {
  const m = LEGACY_CELL.exec(v);
  const tech = m ? COUNTRY_TECHS.find((t) => t.slug === m[1]) : undefined;
  const legacy = m ? legacyStageOfSlug(m[2]) : null;
  if (!m || !tech || !legacy) return null;
  const to = legacyStageTarget(m[2]);
  return { to: to ? `${tech.slug}-${to}` : null, note: legacyStageNote(legacy, tech.label).replace(/\.$/, '') };
}

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
        const old = isCellKey(c) ? null : legacyCellOf(c);
        if (old) {
          // an old three-stage link (design U1)
          if (!old.to) bad(k, raw, `${old.note}; the sector was dropped`);
          else if (ctx.cells && !ctx.cells.has(old.to)) bad(k, raw, `${old.note}, which is not a sector of this country`);
          else {
            state.cell = old.to;
            bad(k, raw, `${old.note}; showing it`);
          }
        } else if (!isCellKey(c)) bad(k, raw, 'not a sector');
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
      case 'ptab': {
        const t = v.toLowerCase();
        if ((PARTNER_TABS as readonly string[]).includes(t)) state.ptab = t as PartnerTab;
        else bad(k, raw, 'trade, investment, development or geopolitical');
        break;
      }
      case 'scn':
        // 'managed' is the default (a link written before 2026-10-20): read as nothing, no notice
        if (v === 'decoupling') state.scn = 'decoupling';
        else if (v !== 'managed') bad(k, raw, "'decoupling' or nothing");
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
  if (s.ptab && s.ptab !== 'trade') q.set('ptab', s.ptab);
  if (s.scn === 'decoupling') q.set('scn', 'decoupling');
  const t = q.toString();
  return t ? `?${t}` : '';
}

/** The profile path of an economy, with optional state. */
export function countryHref(iso3: string, s: Partial<CountryUrlState> = {}, hash?: string): string {
  return `/countries/${iso3.toUpperCase()}${serializeCountrySearch(s)}${hash ? `#${hash}` : ''}`;
}
