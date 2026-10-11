// One concentration summary (a meta.summary row) recomputed from facts, in an EU mode (spec_sector.md
// 3.6, 3.7). The Atlas, chain, Concentration and Home render from meta.summary alone; profiles
// recompute from facts with this, so the highlight and the EU switch stay live, and a test asserts
// that the recomputation equals meta.summary. Trade vars in bloc mode need the trade chunk (intra-EU
// flows removed first). Pure.

import type { SectorTradeChunk, WorldBasis } from '../contract.ts';
import type { FactRow, FactsIndex } from '../data/facts.ts';
import type { SectorModel, SummaryInfo, VarInfo } from '../data/model.ts';
import { concentration, growth, type Concentration, type GrowthKind } from './concentration.ts';
import { euRows, TRADE_BLOC_VARS, type BlocMode } from './groups.ts';
import { blocFlows, countryTotals, flowsOf, matrixTotal, type TradeYear } from './trade.ts';

export interface Slice {
  rows: FactRow[];
  world: { value: number } | null;
  basis: WorldBasis;
  mode: BlocMode;
}

/** The rows and world total of (var, item, year) in an EU mode. */
export function slice(model: SectorModel, facts: FactsIndex, v: VarInfo, item: number, year: number, eu: 'members' | 'bloc', trade?: SectorTradeChunk | null): Slice {
  const raw = facts.rows(v.idx, item, year);
  const w = facts.world(v.idx, item, year);
  const basis = (w?.basis ?? v.world_basis) as WorldBasis;
  if (eu === 'bloc' && TRADE_BLOC_VARS.has(v.id) && trade && (year === 2020 || year === 2024) && model.eu) {
    const flows = blocFlows(flowsOf(trade, item, year as TradeYear), model);
    if (flows.length) {
      const t = countryTotals(flows);
      const rows: FactRow[] = [];
      for (const x of t.values()) {
        const value = v.id === 'trade.exports' ? x.exports : v.id === 'trade.imports' ? x.imports : x.net;
        if (v.id !== 'trade.net' && !(value > 0)) continue;
        const status = v.id === 'trade.imports' && x.imports > 0 && x.importsMirror === x.imports ? 'mirror' : 'obs';
        rows.push({ geo: x.geo, value, status, obs_year: null });
      }
      rows.sort((a, z) => a.geo - z.geo);
      return { rows, world: v.world_basis === 'none' ? null : { value: matrixTotal(flows) }, basis, mode: 'summed' };
    }
  }
  const r = euRows(raw, model, v, eu);
  return { rows: r.rows, world: w ? { value: w.value } : null, basis, mode: r.mode };
}

/** The growth kind of a var's summary (3.6). */
export function growthKindOf(v: VarInfo, year: number): GrowthKind | null {
  if (v.id.startsWith('trade.') && v.native_level === 'country' && year === 2024 && v.years.includes(2020)) return 'pct_2020_2024';
  if (v.id === 'investment.annual' && year === 2025) return 'cagr_2018_2025';
  return null;
}

export type SummaryLike = Omit<SummaryInfo, 'row' | 'item' | 'var' | 'year' | 'eu_mode'>;

/** A summary row recomputed from facts (and the trade chunk for trade vars in bloc mode). */
export function summaryFromFacts(model: SectorModel, facts: FactsIndex, v: VarInfo, item: number, year: number, eu: 'members' | 'bloc', trade?: SectorTradeChunk | null): SummaryLike & { conc: Concentration; mode: BlocMode } {
  const s = slice(model, facts, v, item, year, eu, trade);
  const c = concentration(s.rows, model.geos, s.world, s.basis);
  const gk = growthKindOf(v, year);
  let g: number = NaN;
  if (gk && c.W !== null) {
    const y0 = gk === 'pct_2020_2024' ? 2020 : 2018;
    const s0 = slice(model, facts, v, item, y0, eu, trade);
    const c0 = concentration(s0.rows, model.geos, s0.world, s0.basis);
    if (c0.W !== null) g = growth(gk, c0.W, c.W).value ?? NaN;
  }
  return {
    W: c.W ?? NaN,
    basis: s.basis,
    n: c.n,
    top1_geo: c.top1?.geo ?? null,
    top1_share: c.top1?.share ?? NaN,
    top3_share: c.top3_share,
    hhi: c.hhi,
    hhi_lower_bound: c.hhi_lower_bound,
    eff_n: c.eff_n,
    china: c.split.china,
    core: c.split.core,
    partner: c.split.partner,
    rest: c.split.rest,
    unattributed: c.split.unattributed,
    growth: g,
    growth_kind: gk,
    conc: c,
    mode: s.mode,
  };
}

/** Relative difference of two numbers (NaN = NaN counts as equal). */
export function relDiff(a: number, b: number): number {
  if (Number.isNaN(a) && Number.isNaN(b)) return 0;
  if (a === b) return 0;
  return Math.abs(a - b) / Math.max(Math.abs(a), Math.abs(b), 1e-300);
}

/** Fields of a summary row compared by the parity test (3.6: within 1e-12). */
export const SUMMARY_NUM_FIELDS = ['W', 'top1_share', 'top3_share', 'hhi', 'eff_n', 'china', 'core', 'partner', 'rest', 'unattributed'] as const;

/**
 * Fields on [0, 1] (shares, HHI): compared with an ABSOLUTE tolerance, because a residual such as
 * 1 - sum(shares) is 0 in one language and 1.1e-16 in the other (R's sum() accumulates in long
 * double), and a relative difference near 0 is meaningless. W and effective n are compared relatively.
 */
export const SUMMARY_UNIT_FIELDS: ReadonlySet<string> = new Set(['top1_share', 'top3_share', 'hhi', 'china', 'core', 'partner', 'rest', 'unattributed']);

/** Every mismatch between meta.summary and the recomputation (empty when they agree). */
export function summaryParity(model: SectorModel, facts: FactsIndex, trades: (item: number) => SectorTradeChunk | null, tol = 1e-12): string[] {
  const out: string[] = [];
  for (const s of model.summary) {
    const v = model.vars[s.var];
    if (!v) continue;
    const r = summaryFromFacts(model, facts, v, s.item, s.year, s.eu_mode, trades(s.item));
    for (const f of SUMMARY_NUM_FIELDS) {
      const a = s[f];
      const b = r[f];
      const d = SUMMARY_UNIT_FIELDS.has(f) && Number.isFinite(a) && Number.isFinite(b) ? Math.abs(a - b) : relDiff(a, b);
      if (d > tol) out.push(`${v.id} ${model.items[s.item]?.slug} ${s.year} ${s.eu_mode}: ${f} ${a} vs ${b}`);
    }
    if (s.n !== r.n) out.push(`${v.id} ${model.items[s.item]?.slug} ${s.year} ${s.eu_mode}: n ${s.n} vs ${r.n}`);
    if (s.top1_geo !== r.top1_geo) out.push(`${v.id} ${model.items[s.item]?.slug} ${s.year} ${s.eu_mode}: top1 ${s.top1_geo} vs ${r.top1_geo}`);
    if (s.hhi_lower_bound !== r.hhi_lower_bound) out.push(`${v.id} ${model.items[s.item]?.slug} ${s.year} ${s.eu_mode}: hhi_lower_bound`);
  }
  return out;
}
