// World totals, shares and concentration (spec_sector.md 3.6). Pure; mirrored in R by
// sector_concentration() (scripts/87 precomputes meta.summary with it) and checked against it within
// 1e-12 on the fixture and the real bundle.
//
// For one var, one item and one year:
//   * holders: rows whose geo is a country, the EU, a region or an entity, with a finite value and a
//     group (china / core / partner / rest); residual rows ("Rest of world", "Other") and the world
//     row are not holders. Values are >= 0 (concentration is defined only for additive measures).
//   * W: facts.world by basis; without a world row, the sum of holders and residuals (sum_all,
//     sum_listed), 1 (one), or undefined (none: no shares).
//   * share_i = x_i / W; unattributed u = max(0, 1 - sum share_i); HHI = sum share_i^2, a lower bound
//     when u > 0.05; effective n = 1 / HHI; top-1 and the top-3 share; the group split china | core |
//     partner | rest | unattributed sums to 1; n = holders with a value > 0.
//   * a region or entity in group 'na' (an IEA EV region whose members span groups: North America,
//     Other Asia Pacific, Rest of World) is a holder (ranked as a region), but its share counts as
//     unattributed in the group split; the HHI lower-bound flag reads the source residual only.

import type { SplitPart, WorldBasis } from '../contract.ts';
import type { FactRow } from '../data/facts.ts';
import type { GeoInfo } from '../data/model.ts';

export const HHI_LOWER_BOUND_U = 0.05;
/** Below this, 1 - sum(shares) is floating-point rounding, not an unattributed residual. */
export const UNATTRIBUTED_EPS = 1e-12;

export interface ShareRow {
  geo: number;
  value: number;
  /** x / W, or null without a world total */
  share: number | null;
  /** 1 = largest; ties share the better rank */
  rank: number;
}

export interface Concentration {
  W: number | null;
  basis: WorldBasis;
  /** holders with a value > 0 */
  n: number;
  /** holders with a finite value, largest first */
  rows: ShareRow[];
  top1: { geo: number; share: number } | null;
  top3_share: number;
  hhi: number;
  hhi_lower_bound: boolean;
  eff_n: number;
  split: Record<SplitPart, number>;
}

const HOLDER_KINDS = new Set(['country', 'eu', 'region', 'entity']);

/** Is this geo a holder (a ranked economy, the EU, or a region or entity, mixed regions included)? */
export function isHolder(g: GeoInfo | null | undefined): boolean {
  return !!g && HOLDER_KINDS.has(g.kind) && (g.group !== 'na' || g.kind === 'region' || g.kind === 'entity');
}

/** A holder whose members span groups (group na): ranked, never attributed to a group. */
export const isMixedRegion = (g: GeoInfo | null | undefined): boolean => !!g && (g.kind === 'region' || g.kind === 'entity') && g.group === 'na';

/**
 * Concentration of one slice. `geos` is dims.geo as GeoInfo (indexed by row), `world` the facts.world
 * row (or null), `basis` the dictionary's world_basis.
 */
export function concentration(rows: readonly FactRow[], geos: readonly GeoInfo[], world: { value: number } | null, basis: WorldBasis): Concentration {
  const holders: { geo: number; value: number; group: GeoInfo['group'] }[] = [];
  let residual = 0;
  let sumHolders = 0;
  for (const r of rows) {
    if (!Number.isFinite(r.value)) continue;
    const g: GeoInfo | undefined = geos[r.geo];
    if (!g) continue;
    if (isHolder(g)) {
      holders.push({ geo: r.geo, value: r.value, group: g.group });
      sumHolders += r.value;
    } else if (g.kind === 'residual' || (g.group === 'na' && g.kind !== 'world')) {
      residual += r.value;
    }
  }
  holders.sort((a, z) => z.value - a.value || a.geo - z.geo);

  let W: number | null;
  if (basis === 'none') W = null;
  else if (world && Number.isFinite(world.value)) W = world.value;
  else if (basis === 'one') W = 1;
  else W = sumHolders + residual;
  if (W !== null && !(W > 0)) W = null;

  const out: ShareRow[] = [];
  let rank = 0;
  let prev = NaN;
  holders.forEach((h, i) => {
    if (h.value !== prev) rank = i + 1;
    prev = h.value;
    out.push({ geo: h.geo, value: h.value, share: W === null ? null : h.value / W, rank });
  });
  const n = holders.filter((h) => h.value > 0).length;
  const split: Record<SplitPart, number> = { china: 0, core: 0, partner: 0, rest: 0, unattributed: 0 };
  if (W === null) {
    return { W, basis, n, rows: out, top1: null, top3_share: NaN, hhi: NaN, hhi_lower_bound: false, eff_n: NaN, split: { china: NaN, core: NaN, partner: NaN, rest: NaN, unattributed: NaN } };
  }
  let sumShare = 0;
  let hhi = 0;
  let mixed = 0;
  holders.forEach((h) => {
    const s = h.value / W;
    sumShare += s;
    hhi += s * s;
    if (h.group === 'na') mixed += s;
    else split[h.group as Exclude<SplitPart, 'unattributed'>] += s;
  });
  // a rounding residual (1 - sum < 1e-12) is no unattributed band
  const u = 1 - sumShare < UNATTRIBUTED_EPS ? 0 : 1 - sumShare;
  split.unattributed = u + mixed;
  const top3 = holders.slice(0, 3).reduce((a, h) => a + h.value / W, 0);
  const top1 = holders.length && holders[0].value > 0 ? { geo: holders[0].geo, share: holders[0].value / W } : null;
  return {
    W,
    basis,
    n,
    rows: out,
    top1,
    top3_share: top3,
    hhi,
    hhi_lower_bound: u > HHI_LOWER_BOUND_U,
    eff_n: hhi > 0 ? 1 / hhi : NaN,
    split,
  };
}

/** The share and rank of one geo in a concentration, or null. */
export function positionOf(c: Concentration, geo: number): ShareRow | null {
  return c.rows.find((r) => r.geo === geo) ?? null;
}

/** "% of world" or "% of the economies listed by {source}" (3.6: a share of a listed sum is never a world share). */
export function basisWords(basis: WorldBasis, source: string): string {
  switch (basis) {
    case 'source_world':
    case 'sum_all':
    case 'one':
      return 'of world';
    case 'sum_listed':
      return `of the economies listed by ${source}`;
    default:
      return '';
  }
}

/** Does this basis give a world share (vs a share of a listed sum)? */
export const isWorldBasis = (b: WorldBasis): boolean => b === 'source_world' || b === 'sum_all' || b === 'one';

export type GrowthKind = 'pct_2020_2024' | 'cagr_2018_2025' | 'cagr_2022_2025' | 'cagr_2015_2025' | 'pct_2024_2025' | 'cagr_2013_2023';

export const GROWTH_YEARS: Record<GrowthKind, [number, number, 'pct' | 'cagr']> = {
  pct_2020_2024: [2020, 2024, 'pct'],
  cagr_2018_2025: [2018, 2025, 'cagr'],
  cagr_2022_2025: [2022, 2025, 'cagr'],
  cagr_2015_2025: [2015, 2025, 'cagr'],
  pct_2024_2025: [2024, 2025, 'pct'],
  cagr_2013_2023: [2013, 2023, 'cagr'],
};

/**
 * Growth between a base and a current value (3.6): percent change or CAGR. `new` when the base is
 * zero or below 1% of the current value; NaN never reaches the UI (null instead).
 */
export function growth(kind: GrowthKind, base: number, current: number): { value: number | null; isNew: boolean } {
  if (!Number.isFinite(base) || !Number.isFinite(current)) return { value: null, isNew: false };
  if (base <= 0 || (current > 0 && base < 0.01 * current)) return { value: null, isNew: current > 0 };
  const [y0, y1, how] = GROWTH_YEARS[kind];
  const v = how === 'pct' ? current / base - 1 : Math.pow(current / base, 1 / (y1 - y0)) - 1;
  return { value: Number.isFinite(v) ? v : null, isNew: false };
}

/** Growth text: "+104%", "new", "n/a". */
export function growthWords(g: { value: number | null; isNew: boolean }): string | null {
  if (g.isNew) return 'new';
  return g.value === null ? null : `${g.value >= 0 ? '+' : '−'}${Math.abs(g.value * 100).toFixed(1)}%`;
}
