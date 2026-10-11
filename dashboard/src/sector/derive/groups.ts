// EU-27 as one bloc (spec_sector.md 3.7) and the group helpers of the "who" lens (3.4). Pure.
//
// Members mode (the default): EU-level rows (kind eu, "EUU") are dropped from country views, so the
// EU is never counted twice. Bloc mode: a source with its own EU row (EI T-EU, IEA "EU", CAT, PAMS) is
// used as reported and its members are dropped; otherwise an additive measure (agg sum) sums the 27
// members into one EUU row; ratios without a source EU row and intensive measures (RCA, prices, wages,
// rates, TRL, quality) are not defined for the bloc and keep the member rows ("shown per member").
// Trade measures need the dyads (intra-EU flows removed first): see derive/trade.ts.

import type { GeoGroup, SplitPart, StatusCode } from '../contract.ts';
import type { FactRow } from '../data/facts.ts';
import type { GeoInfo, SectorModel, VarInfo } from '../data/model.ts';

export type BlocMode =
  /** members mode, or a slice without EU members */
  | 'members'
  /** the source's own EU row is used */
  | 'source_eu'
  /** the members were summed into one EUU row */
  | 'summed'
  /** not defined for the bloc: the member rows are kept */
  | 'undefined'
  /** a trade measure without the trade chunk: the member rows are kept ("needs trade data") */
  | 'needs_trade';

export interface BlocResult {
  rows: FactRow[];
  mode: BlocMode;
  /** EU members folded into the bloc (summed mode) */
  members: number[];
}

/** Trade vars whose bloc values need the dyads. */
export const TRADE_BLOC_VARS = new Set(['trade.exports', 'trade.imports', 'trade.net']);

/** The rows of a slice in members mode: EU-level rows dropped. */
export function membersRows(rows: readonly FactRow[], model: SectorModel): FactRow[] {
  return rows.filter((r) => model.geos[r.geo]?.kind !== 'eu');
}

/** The rows of a slice in an EU mode. */
export function euRows(rows: readonly FactRow[], model: SectorModel, v: VarInfo, eu: 'members' | 'bloc'): BlocResult {
  if (eu === 'members') return { rows: membersRows(rows, model), mode: 'members', members: [] };
  const euRow = rows.find((r) => model.geos[r.geo]?.kind === 'eu');
  const memberRows = rows.filter((r) => model.geos[r.geo]?.eu_member);
  if (euRow) {
    return { rows: rows.filter((r) => !model.geos[r.geo]?.eu_member), mode: 'source_eu', members: memberRows.map((r) => r.geo) };
  }
  if (!memberRows.length) return { rows: [...rows], mode: 'members', members: [] };
  if (TRADE_BLOC_VARS.has(v.id)) return { rows: membersRows(rows, model), mode: 'needs_trade', members: [] };
  if (v.agg !== 'sum' || !model.eu) return { rows: membersRows(rows, model), mode: 'undefined', members: [] };
  let sum = 0;
  let any = false;
  const statuses = new Set<StatusCode>();
  for (const r of memberRows) {
    if (!Number.isFinite(r.value)) continue;
    sum += r.value;
    any = true;
    statuses.add(r.status);
  }
  const out = rows.filter((r) => !model.geos[r.geo]?.eu_member && model.geos[r.geo]?.kind !== 'eu');
  if (any) out.push({ geo: model.eu.idx, value: sum, status: blocStatus(statuses), obs_year: null });
  out.sort((a, z) => a.geo - z.geo);
  return { rows: out, mode: 'summed', members: memberRows.map((r) => r.geo) };
}

/** The state of a summed bloc row: est or mirror when any member is, zero when all are. */
export function blocStatus(s: Set<StatusCode>): StatusCode {
  if (s.has('est')) return 'est';
  if (s.has('mirror')) return 'mirror';
  if (s.size === 1 && s.has('zero')) return 'zero';
  return 'obs';
}

/** Words under every bloc-mode chart (3.7). */
export function blocNote(model: SectorModel): string {
  const outside = model.euOutsideCoalition;
  return outside.length
    ? `EU-27 counted as one. ${outside.join(', ')} ${outside.length === 1 ? 'is an EU member' : 'are EU members'} outside the coalition tiers; in bloc mode their values count in the core.`
    : 'EU-27 counted as one.';
}

/** Words for a bloc mode, for notes ("as reported by EI", "shown per member"). */
export function blocModeWords(mode: BlocMode, source: string): string | null {
  switch (mode) {
    case 'source_eu':
      return `The EU row is the one reported by ${source}.`;
    case 'summed':
      return 'The EU row sums its 27 members.';
    case 'undefined':
      return 'Not defined for the EU as a bloc: shown per member.';
    case 'needs_trade':
      return 'EU as one bloc needs the trade data (intra-EU flows are removed first); member rows shown until it loads.';
    default:
      return null;
  }
}

export const GROUP_ORDER: readonly SplitPart[] = ['china', 'core', 'partner', 'rest', 'unattributed'];

export const GROUP_LABEL: Record<SplitPart | 'na', string> = {
  china: 'China',
  core: 'Coalition core',
  partner: 'Coalition partners',
  rest: 'Rest of world',
  unattributed: 'Not attributed',
  na: 'Not attributed',
};

/** Short group words for tables ("core", "partner"). */
export const GROUP_SHORT: Record<SplitPart | 'na', string> = {
  china: 'China',
  core: 'core',
  partner: 'partner',
  rest: 'rest',
  unattributed: 'not attributed',
  na: 'not attributed',
};

/** The split part of a geo (na -> unattributed). */
export function partOf(g: GeoInfo | null | undefined): SplitPart {
  const grp: GeoGroup | undefined = g?.group;
  return grp === 'china' || grp === 'core' || grp === 'partner' || grp === 'rest' ? grp : 'unattributed';
}

/** Is a geo a coalition member (core or partner)? */
export const isCoalition = (g: GeoInfo | null | undefined): boolean => !!g && (g.group === 'core' || g.group === 'partner');

/** Economy options for the highlight picker: Coalition core / Coalition partners / Other economies (China first). */
export function countryOptions(model: SectorModel, eu: 'members' | 'bloc'): { iso3c: string; country: string; group: string }[] {
  const label = (g: GeoInfo) => (g.group === 'core' ? 'Coalition core' : g.group === 'partner' ? 'Coalition partners' : 'Other economies');
  const order = (g: GeoInfo) => (g.group === 'core' ? 0 : g.group === 'partner' ? 1 : 2);
  const xs = model.countries
    .filter((g) => !(eu === 'bloc' && g.eu_member))
    .slice()
    .sort((a, z) => order(a) - order(z) || (a.code === 'CHN' ? -1 : z.code === 'CHN' ? 1 : 0) || a.name.localeCompare(z.name, 'en'))
    .map((g) => ({ iso3c: g.code, country: g.name, group: label(g) }));
  if (eu === 'bloc' && model.eu) xs.unshift({ iso3c: model.eu.code, country: model.eu.name, group: 'Coalition core' });
  return xs;
}
