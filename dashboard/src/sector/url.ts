// Sector URL state (spec_sector.md 2.7). Pure. The conventions match the Strategic parser
// (state/url.ts): defaults are omitted, keys are written in one fixed order, invalid values are dropped
// and listed (the "Ignored in the link" notice), keys the Sector parser does not know (Strategic keys
// included) are ignored silently. Navigation inside the module keeps `c` and `eu` only.

export type Lens = 'china' | 'top1' | 'top3' | 'hhi' | 'coalition';
export type FamKey = 'primary' | 'exports' | 'investment' | 'production' | 'capacity' | 'reserves' | 'consumption';
export type EuParam = 'members' | 'bloc';
export type TradeDir = 'exports' | 'imports';
export type ShowAs = 'value' | 'share' | 'gdp';
export type ScaleParam = 'log' | 'linear';
export type GeoScope = 'all' | 'coalition' | 'focus';

export interface SectorUrlState {
  /** highlighted economy: an ISO3 of dims.geo (kind country), or EUU when eu = bloc */
  c: string | null;
  eu: EuParam;
  lens: Lens;
  fam: FamKey;
  /** a var id */
  m: string | null;
  /** a year valid for m (null = latest) */
  y: number | null;
  tdir: TradeDir;
  /** include cross-cutting policies */
  xc: boolean;
  /** Strategic vocabulary ("Batteries", "Midstream") */
  tech: string | null;
  stage: string | null;
  /** item slug */
  item: string | null;
  /** a trade sub-component of the cell (a trade.codes.sub_sector name); null = the whole basket */
  sub: string | null;
  as: ShowAs;
  /** null = the dictionary's default */
  scale: ScaleParam | null;
  geo: GeoScope;
}

export const SECTOR_KEY_ORDER = ['c', 'eu', 'lens', 'fam', 'm', 'y', 'tdir', 'xc', 'tech', 'stage', 'item', 'sub', 'as', 'scale', 'geo'] as const;
export type SectorKey = (typeof SECTOR_KEY_ORDER)[number];
/** Keys kept when navigating between Sector views. */
export const SECTOR_NAV_KEYS: readonly SectorKey[] = ['c', 'eu'];

export const LENSES: readonly Lens[] = ['china', 'top1', 'top3', 'hhi', 'coalition'];
export const FAM_KEYS: readonly FamKey[] = ['primary', 'exports', 'investment', 'production', 'capacity', 'reserves', 'consumption'];
export const GEO_SCOPES: readonly GeoScope[] = ['all', 'coalition', 'focus'];

export const SECTOR_DEFAULTS: SectorUrlState = {
  c: null,
  eu: 'members',
  lens: 'china',
  fam: 'primary',
  m: null,
  y: null,
  tdir: 'exports',
  xc: false,
  tech: null,
  stage: null,
  item: null,
  sub: null,
  as: 'value',
  scale: null,
  geo: 'all',
};

/** What the parser can check against (all optional: before sector-meta loads only the syntax is checked). */
export interface SectorUrlCtx {
  /** ISO3 codes of dims.geo rows of kind country */
  countries?: ReadonlySet<string>;
  /** var ids of dict.vars */
  vars?: ReadonlySet<string>;
  /** item slugs of dims.items */
  items?: ReadonlySet<string>;
  /** Strategic tech labels and stage labels */
  techs?: ReadonlySet<string>;
  stages?: ReadonlySet<string>;
  /** `as=gdp` exists only from phase 2 */
  gdp?: boolean;
}

export interface SectorParsed {
  state: SectorUrlState;
  problems: string[];
}

const ISO3 = /^[A-Z]{3}$/;
const VAR_RE = /^[a-z]+\.[a-z0-9_]+$/;
const SLUG_RE = /^[a-z0-9]+(-[a-z0-9]+)*$/;

/** Read a query string. Invalid values are dropped and listed in `problems`; unknown keys are ignored. */
export function parseSectorSearch(search: string, ctx: SectorUrlCtx = {}): SectorParsed {
  const q = new URLSearchParams(search.startsWith('?') ? search.slice(1) : search);
  const problems: string[] = [];
  const state: SectorUrlState = { ...SECTOR_DEFAULTS };
  const seen = new Set<string>();
  const bad = (k: string, v: string, why: string) => problems.push(`${k}=${v} (${why})`);
  let cRaw: string | null = null;

  for (const [k, raw] of q.entries()) {
    if (!(SECTOR_KEY_ORDER as readonly string[]).includes(k)) continue; // Strategic and unknown keys: silent
    if (seen.has(k)) {
      bad(k, raw, 'repeated; the first value is used');
      continue;
    }
    seen.add(k);
    const v = raw.trim();
    switch (k as SectorKey) {
      case 'c': {
        const code = v.toUpperCase();
        if (code === 'EUU') cRaw = code; // checked after eu is known
        else if (!ISO3.test(code)) bad(k, raw, 'not an ISO3 code');
        else if (ctx.countries && !ctx.countries.has(code)) bad(k, raw, 'not an economy in the sector data');
        else state.c = code;
        break;
      }
      case 'eu':
        if (v === 'bloc') state.eu = 'bloc';
        else if (v !== 'members') bad(k, raw, "'bloc' or 'members'");
        break;
      case 'lens':
        if ((LENSES as readonly string[]).includes(v)) state.lens = v as Lens;
        else bad(k, raw, `one of ${LENSES.join(', ')}`);
        break;
      case 'fam':
        if ((FAM_KEYS as readonly string[]).includes(v)) state.fam = v as FamKey;
        else bad(k, raw, `one of ${FAM_KEYS.join(', ')}`);
        break;
      case 'm':
        if (!VAR_RE.test(v)) bad(k, raw, 'not a measure id');
        else if (ctx.vars && !ctx.vars.has(v)) bad(k, raw, 'not a measure in the sector data');
        else state.m = v;
        break;
      case 'y': {
        const y = /^\d{4}$/.test(v) ? Number(v) : NaN;
        if (!Number.isInteger(y) || y < 1900 || y > 2100) bad(k, raw, 'not a year');
        else state.y = y;
        break;
      }
      case 'tdir':
        if (v === 'imports') state.tdir = 'imports';
        else if (v !== 'exports') bad(k, raw, "'imports' or 'exports'");
        break;
      case 'xc':
        if (v === '1') state.xc = true;
        else if (v !== '0') bad(k, raw, "'1' or '0'");
        break;
      case 'tech':
        if (ctx.techs ? ctx.techs.has(v) : v.length > 0) state.tech = v;
        else bad(k, raw, 'not a technology');
        break;
      case 'stage':
        if (ctx.stages ? ctx.stages.has(v) : ['Upstream', 'Midstream', 'Downstream'].includes(v)) state.stage = v;
        else bad(k, raw, 'not a stage');
        break;
      case 'item':
        if (!SLUG_RE.test(v)) bad(k, raw, 'not an item slug');
        else if (ctx.items && !ctx.items.has(v)) bad(k, raw, 'not an item in the sector data');
        else state.item = v;
        break;
      case 'sub':
        // checked against the cell's sub-components by the view (an unknown name shows the whole basket)
        if (!v || v.length > 120 || /[\u0000-\u001f]/.test(v)) bad(k, raw, 'not a sub-component name');
        else state.sub = v;
        break;
      case 'as':
        if (v === 'share') state.as = 'share';
        else if (v === 'gdp' && ctx.gdp) state.as = 'gdp';
        else if (v === 'gdp') bad(k, raw, '% of GDP is not in this build');
        else if (v !== 'value') bad(k, raw, "'share' or 'value'");
        break;
      case 'scale':
        if (v === 'log' || v === 'linear') state.scale = v;
        else bad(k, raw, "'log' or 'linear'");
        break;
      case 'geo':
        if ((GEO_SCOPES as readonly string[]).includes(v)) state.geo = v as GeoScope;
        else bad(k, raw, `one of ${GEO_SCOPES.join(', ')}`);
        break;
    }
  }
  if (cRaw === 'EUU') {
    if (state.eu === 'bloc') state.c = 'EUU';
    else bad('c', cRaw, 'EUU needs eu=bloc');
  }
  return { state, problems };
}

/** The query string of a state (no leading '?'): defaults omitted, keys in SECTOR_KEY_ORDER. */
export function writeSectorSearch(state: Partial<SectorUrlState>): string {
  const s = { ...SECTOR_DEFAULTS, ...state };
  const parts: string[] = [];
  const put = (k: SectorKey, v: string) => parts.push(`${k}=${encodeURIComponent(v)}`);
  for (const k of SECTOR_KEY_ORDER) {
    switch (k) {
      case 'c':
        if (s.c) put(k, s.c);
        break;
      case 'eu':
        if (s.eu === 'bloc') put(k, 'bloc');
        break;
      case 'lens':
        if (s.lens !== 'china') put(k, s.lens);
        break;
      case 'fam':
        if (s.fam !== 'primary') put(k, s.fam);
        break;
      case 'm':
        if (s.m) put(k, s.m);
        break;
      case 'y':
        if (s.y !== null && s.y !== undefined) put(k, String(s.y));
        break;
      case 'tdir':
        if (s.tdir === 'imports') put(k, 'imports');
        break;
      case 'xc':
        if (s.xc) put(k, '1');
        break;
      case 'tech':
        if (s.tech) put(k, s.tech);
        break;
      case 'stage':
        if (s.stage) put(k, s.stage);
        break;
      case 'item':
        if (s.item) put(k, s.item);
        break;
      case 'sub':
        if (s.sub) put(k, s.sub);
        break;
      case 'as':
        if (s.as !== 'value') put(k, s.as);
        break;
      case 'scale':
        if (s.scale) put(k, s.scale);
        break;
      case 'geo':
        if (s.geo !== 'all') put(k, s.geo);
        break;
    }
  }
  return parts.join('&');
}

/** "?c=USA&eu=bloc" for links between Sector views (only the shared keys), or "". */
export function navSectorSearch(state: Partial<SectorUrlState>): string {
  const keep: Partial<SectorUrlState> = { c: state.c ?? null, eu: state.eu ?? 'members' };
  if (keep.c === 'EUU' && keep.eu !== 'bloc') keep.c = null;
  const s = writeSectorSearch(keep);
  return s ? `?${s}` : '';
}

/** A path plus the shared keys and optional view keys: sectorHref('/sector/batteries', state, { m: 'trade.exports' }). */
export function sectorHref(path: string, state: Partial<SectorUrlState>, view: Partial<SectorUrlState> = {}, hash?: string): string {
  const s = writeSectorSearch({ c: state.c ?? null, eu: state.eu ?? 'members', ...view });
  return `${path}${s ? `?${s}` : ''}${hash ? `#${hash}` : ''}`;
}

/** Keys whose change replaces the history entry (year, show-as, scale, item, sub-component); the rest push. */
export const REPLACE_KEYS: readonly SectorKey[] = ['y', 'as', 'scale', 'item', 'sub'];

/** True when a patch touches only replace-keys. */
export function patchReplaces(patch: Partial<SectorUrlState>): boolean {
  const keys = Object.keys(patch) as SectorKey[];
  return keys.length > 0 && keys.every((k) => REPLACE_KEYS.includes(k));
}
