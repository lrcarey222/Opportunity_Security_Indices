// Value states (spec_sector.md 3.5): one vocabulary everywhere. The shipped status of a fact row, plus
// the client-derived states: not covered (no row, outside the source's roster), no value recorded (no
// row, inside the roster), regional (a region / entity / EU value), old (obs_year older than the var's
// stale_years). Pure.

import type { StatusCode } from '../contract.ts';
import type { FactRow } from '../data/facts.ts';
import type { GeoInfo, SectorModel, VarInfo } from '../data/model.ts';
import { ASSUMED_ZERO_WORDS, isAssumedZero } from './notes.ts';

export type ValueState = StatusCode | 'not_covered' | 'none' | 'regional';

export interface StateOf {
  state: ValueState;
  /** obs_year older than stale_years */
  old: boolean;
  /** the per-row year when it differs from the slice year */
  obsYear: number | null;
  /** nsr with the value an assumed zero production gives (net balance, self-sufficiency): drawn as a value */
  assumed?: boolean;
}

export const STATE_LABEL: Record<ValueState | 'old', string> = {
  obs: 'Observed',
  zero: 'Reported zero',
  nsr: 'Not reported separately',
  nq: 'Listed without a figure',
  mirror: 'Mirror',
  est: 'Estimate',
  na_growth: 'Base year zero',
  not_covered: 'Not covered',
  none: 'No value recorded',
  regional: 'Regional value',
  old: 'Old',
};

/** States shown on maps as a hatch (3.5). */
export const HATCH_STATES: ReadonlySet<ValueState> = new Set(['nsr', 'nq', 'na_growth', 'not_covered', 'none']);
/** States with a value on bars and maps. */
export const VALUE_STATES: ReadonlySet<ValueState> = new Set(['obs', 'mirror', 'est', 'zero']);

/** The state of a geo in a slice. */
export function stateOf(row: FactRow | null | undefined, geo: GeoInfo, v: VarInfo, roster: Set<number> | null, sliceYear?: number): StateOf {
  if (geo.kind === 'region' || geo.kind === 'entity' || geo.kind === 'eu') {
    if (v.native_level === 'country' || v.native_level === 'dyad') return { state: 'regional', old: false, obsYear: null };
  }
  if (!row) {
    if (roster && !roster.has(geo.idx)) return { state: 'not_covered', old: false, obsYear: null };
    return { state: roster ? 'none' : 'not_covered', old: false, obsYear: null };
  }
  const obsYear = row.obs_year;
  const ref = sliceYear ?? null;
  const old = v.stale_years !== null && obsYear !== null && ref !== null && ref - obsYear > v.stale_years;
  // "Reported zero" belongs to additive measures; a 0 of a bounded or ratio measure (a UN-vote position
  // of 0 = China's) is an observation like any other value
  const state = row.status === 'zero' && v.agg !== 'sum' ? 'obs' : row.status;
  const out: StateOf = { state, old, obsYear: obsYear !== null && obsYear !== ref ? obsYear : null };
  if (isAssumedZero(v.id, row)) out.assumed = true;
  return out;
}

/** Tooltip words of a state (3.5). */
export function stateWords(s: StateOf, v: VarInfo, model: SectorModel, geo?: GeoInfo | null): string {
  const src = v.source.short;
  let text: string;
  switch (s.state) {
    case 'obs':
      text = 'Observed';
      break;
    case 'zero':
      text = 'Reported zero';
      break;
    case 'nsr':
      text = s.assumed ? ASSUMED_ZERO_WORDS : `Not reported separately by ${src} (in its Rest of world)`;
      break;
    case 'nq':
      text = /usgs/i.test(v.source.id + src) ? 'Listed by USGS without a figure (withheld or not available)' : `Listed by ${src} without a figure (under 1% or not reported)`;
      break;
    case 'mirror':
      text = "From partners' declarations (×1.06)";
      break;
    case 'est':
      text = 'Includes estimated Q4';
      break;
    case 'na_growth':
      text = 'Base year zero';
      break;
    case 'not_covered': {
      const r = model.rosterOf(v);
      text = r ? `Not in ${src} (${r.size} economies)` : `Not covered by ${src}`;
      break;
    }
    case 'none':
      text = `No value recorded by ${src}`;
      break;
    case 'regional': {
      const region = geo ? regionOf(model, geo) : null;
      text = region ? `Part of ${region.name}: see the regional panel` : 'A regional value: see the regional panel';
      break;
    }
  }
  if (s.old && s.obsYear !== null) text += ` · observed ${s.obsYear}`;
  else if (s.obsYear !== null) text += ` · ${s.obsYear}`;
  return text;
}

/** Short table words: "withheld", "< 1% or n.r.", "new", "mirror". */
export function stateShort(s: StateOf, v: VarInfo): string {
  switch (s.state) {
    case 'nq':
      return /usgs/i.test(v.source.id + v.source.short) ? 'withheld' : '< 1% or n.r.';
    case 'nsr':
      return s.assumed ? 'production assumed 0' : 'not separately reported';
    case 'na_growth':
      return 'new';
    case 'not_covered':
      return 'not covered';
    case 'none':
      return 'none recorded';
    case 'regional':
      return 'regional';
    case 'mirror':
      return 'mirror';
    case 'est':
      return 'estimate';
    case 'zero':
      return 'reported zero';
    default:
      return s.old && s.obsYear !== null ? `observed ${s.obsYear}` : 'observed';
  }
}

/** The region (dims.region_members) a country belongs to, if any. */
export function regionOf(model: SectorModel, geo: GeoInfo): GeoInfo | null {
  if (geo.kind !== 'country') return geo.kind === 'region' ? geo : null;
  for (const [r, members] of model.regionMembers) if (members.includes(geo.idx)) return model.geos[r] ?? null;
  return null;
}

/** Counts of states over a slice and a roster, for result lines ("142 with data · 3 report zero · 75 not covered"). */
export function stateCounts(rows: readonly FactRow[], model: SectorModel, v: VarInfo): { withData: number; zero: number; noFigure: number; notCovered: number; total: number } {
  const roster = model.rosterOf(v);
  const countryRows = rows.filter((r) => model.geos[r.geo]?.kind === 'country');
  const have = new Set(countryRows.map((r) => r.geo));
  let zero = 0;
  let withData = 0;
  let noFigure = 0;
  for (const r of countryRows) {
    if (v.agg === 'sum' && (r.status === 'zero' || r.value === 0)) zero++;
    else if (Number.isFinite(r.value)) withData++;
    else noFigure++;
  }
  const notCovered = model.countries.filter((g) => !have.has(g.idx) && (!roster || !roster.has(g.idx))).length;
  return { withData, zero, noFigure, notCovered, total: model.countries.length };
}
