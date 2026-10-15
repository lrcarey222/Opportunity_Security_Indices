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
  es: 'Energy Security',
  eo: 'Economic Opportunity',
  si_rank: 'Strategic Index: rank within the economy',
  si: 'Strategic Index: score',
  policy: 'Policy activity',
  psi: 'Partnership Strength (as a partner)',
};

/** The measure as a noun phrase for running text ("each cell is the member's {noun} in that sector"). */
export const METRIC_NOUN: Record<Metric, string> = {
  es: 'Energy Security',
  eo: 'Economic Opportunity',
  si_rank: 'rank on the Strategic Index',
  si: 'Strategic Index score',
  policy: 'Policy activity',
  psi: 'Partnership Strength as a partner',
};

/** The glossary id of each metric (the first use on a page carries its InfoTip). */
export const METRIC_TERM: Record<Metric, string> = {
  es: 'es',
  eo: 'eo',
  si_rank: 'si',
  si: 'si',
  policy: 'policy_index',
  psi: 'psi',
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

/**
 * The three-colour scale's options for a metric and Show mode (spec_country.md C1.5): white at the
 * median of the values shown, orange lower, blue higher; changes centre on zero (orange fell, blue
 * rose; for ranks a positive change is a move up the economy's own list); ranks reverse, so rank 1
 * (an economy's top sector) is blue.
 */
export function triOptions(metric: Metric, mode: ShowMode): { center: 'median' | 'zero'; reverse: boolean } {
  if (mode === 'change') return { center: 'zero', reverse: false };
  return { center: 'median', reverse: metric === 'si_rank' };
}

/** The values that define a chart's scale: the rows drawn (shown, and rankable when "Rankable only"). */
export function scaleValues(rows: MetricRow[], rankableOnly: boolean): number[] {
  return rows.filter((r) => r.shown !== null && (r.shown_ok || !rankableOnly)).map((r) => r.shown as number);
}

/**
 * Where a value sits on a three-colour scale, in words (the side of the median is never carried by
 * colour alone near the middle: tooltips and tables say it). `side` is TriScale.side(v); `mid` the
 * median as printed; `noun` what the median is taken over ("economies shown").
 */
export function sideWords(side: 'below' | 'median' | 'above' | null, metric: Metric, mode: ShowMode, mid: string, noun: string): string | null {
  if (side === null) return null;
  if (mode === 'change') {
    if (metric === 'si_rank') return side === 'above' ? 'moved up its own list' : side === 'below' ? 'moved down its own list' : 'no change in rank';
    return side === 'above' ? 'rose against the paper' : side === 'below' ? 'fell against the paper' : 'no change';
  }
  if (metric === 'si_rank') {
    if (side === 'median') return `at the median rank of the ${noun} (${mid})`;
    return side === 'below' ? `nearer the top of its own list than the median of the ${noun} (rank ${mid})` : `further down its own list than the median of the ${noun} (rank ${mid})`;
  }
  return side === 'median' ? `at the median of the ${noun} (${mid})` : `${side} the median of the ${noun} (${mid})`;
}
