// Overview derivations (pure; spec_react.md 8.6 view 1; port of shiny mod_overview.R
// overview_metric_data() and show_values()): one long table of the metric for every economy x
// technology x stage with the scenario value, the paper's value, the change and rankability.

import type { CorePayload, PartnerPayload } from '../../worker/protocol.ts';
import type { Metric, ShowMode } from '../../state/url.ts';
import { rowsOf, numOr, strOr } from '../common.tsx';

export interface MetricRow {
  iso3c: string | null;
  Country: string;
  tech: string;
  supply_chain: string;
  value: number | null;
  value_base: number | null;
  /** scenario minus paper; for ranks, rank_base - rank (positive = moved up) */
  delta: number | null;
  ok: boolean;
  ok_base: boolean;
  reason: string | null;
  shown: number | null;
  shown_ok: boolean;
}

export const METRIC_LABEL: Record<Metric, string> = {
  es: 'Energy Security (ES)',
  eo: 'Economic Opportunity (EO)',
  si_rank: 'Strategic Index: rank within country',
  si: 'Strategic Index: level',
  policy: 'Policy index',
  psi: 'Partnership Strength Index (as a partner)',
};

export const isRankMetric = (m: Metric): boolean => m === 'si_rank';

export function metricRows(metric: Metric, core: CorePayload, partner: PartnerPayload | null, isoOf: (country: string) => string | null): Omit<MetricRow, 'shown' | 'shown_ok'>[] {
  if (metric === 'es' || metric === 'eo') {
    return rowsOf(metric === 'es' ? core.compare.es : core.compare.eo).map((r) => ({
      iso3c: strOr(r.iso3c),
      Country: String(r.Country),
      tech: String(r.tech),
      supply_chain: String(r.supply_chain),
      value: numOr(r.value),
      value_base: numOr(r.value_base),
      delta: numOr(r.delta),
      ok: r.rankable === true,
      ok_base: r.rankable_base === true,
      reason: strOr(r.rank_exclusion),
    }));
  }
  if (metric === 'si' || metric === 'si_rank') {
    return rowsOf(core.compare.si).map((r) => {
      const v = metric === 'si' ? numOr(r.strategic_index) : numOr(r.rank_in_country);
      const vb = metric === 'si' ? numOr(r.strategic_index_base) : numOr(r.rank_in_country_base);
      return {
        iso3c: strOr(r.iso3c),
        Country: String(r.Country),
        tech: String(r.tech),
        supply_chain: String(r.supply_chain),
        value: v,
        value_base: vb,
        delta: metric === 'si' ? numOr(r.delta) : numOr(r.rank_delta),
        ok: r.in_sample === true,
        ok_base: r.in_sample_base === true,
        reason: strOr(r.si_exclusion),
      };
    });
  }
  if (metric === 'policy') {
    return rowsOf(core.compare.policy).map((r) => {
      const v = numOr(r.value);
      const vb = numOr(r.value_base);
      return {
        iso3c: isoOf(String(r.Country)),
        Country: String(r.Country),
        tech: String(r.tech),
        supply_chain: String(r.supply_chain),
        value: v,
        value_base: vb,
        delta: numOr(r.delta),
        ok: v !== null,
        ok_base: vb !== null,
        reason: v === null ? 'no Policy value' : null,
      };
    });
  }
  if (!partner) return [];
  return rowsOf(partner.compare.psi_partner).map((r) => ({
    iso3c: strOr(r.partner_iso),
    Country: String(r.Country ?? r.partner_iso),
    tech: '',
    supply_chain: '',
    value: numOr(r.psi),
    value_base: numOr(r.psi_base),
    delta: numOr(r.delta),
    ok: r.rankable === true,
    ok_base: r.rankable === true,
    reason: strOr(r.rank_exclusion),
  }));
}

/** The displayed value and its validity for a Show mode (shiny show_values()). */
export function withShown(rows: Omit<MetricRow, 'shown' | 'shown_ok'>[], mode: ShowMode): MetricRow[] {
  return rows.map((r) => {
    const shown = mode === 'baseline' ? r.value_base : mode === 'change' ? r.delta : r.value;
    const shown_ok = mode === 'baseline' ? r.ok_base : mode === 'change' ? r.ok && r.ok_base : r.ok;
    return { ...r, shown, shown_ok, reason: r.reason ?? (!r.ok ? 'not scored' : null) };
  });
}

/** Rows of one technology and stage (PSI has one row per partner). */
export function cellRows(rows: MetricRow[], metric: Metric, tech: string, stage: string): MetricRow[] {
  if (metric === 'psi') return rows;
  return rows.filter((r) => r.tech === tech && r.supply_chain === stage);
}

/** Colour domain of the shown values: fixed [0, 1] for the bounded indices; ranks reversed. */
export function colorDomain(rows: MetricRow[], metric: Metric, mode: ShowMode): { kind: 'seq' | 'div'; lo: number; hi: number; reverse: boolean } {
  const vals = rows.filter((r) => r.shown_ok && r.shown !== null).map((r) => r.shown as number);
  if (mode === 'change') {
    const L = vals.reduce((m, v) => Math.max(m, Math.abs(v)), 0) || 1;
    return { kind: 'div', lo: -L, hi: L, reverse: false };
  }
  if (metric === 'si_rank') return { kind: 'seq', lo: 1, hi: Math.max(2, ...vals), reverse: true };
  if (metric === 'es' || metric === 'eo' || metric === 'policy') return { kind: 'seq', lo: 0, hi: 1, reverse: false };
  if (!vals.length) return { kind: 'seq', lo: 0, hi: 1, reverse: false };
  const lo = Math.min(...vals);
  const hi = Math.max(...vals);
  return { kind: 'seq', lo: metric === 'si' ? 0 : lo, hi: hi > lo ? hi : lo + 1e-9, reverse: false };
}
