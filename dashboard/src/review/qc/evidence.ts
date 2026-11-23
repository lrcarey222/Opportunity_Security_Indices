// Neutral evidence wording for the case brief (UX v2, docs/qc_audit/QC_EVIDENCE_CATALOGUE.md 1.2): number
// formats, the generic interpretation conventions a reviewer reads facts with (never the Explorer's own
// thresholds), country names, and the parsers that turn the bundles' compact notes ("World demand +301% 2024-35
// (WEO 2025 STEPS) ...", "demand_growth: WEO Current Policies ...") into plain words with their source and
// vintage. Pure: no IO.
//
// Conventions (all external or descriptive, none an Explorer bar):
// - HHI (0-1): below 0.10 unconcentrated, 0.10-0.18 moderately, above 0.18 highly concentrated (US Department of
//   Justice and Federal Trade Commission Merger Guidelines 2023, Guideline 1; the same bands as
//   config/sector_dictionary.yml hhi_refs).
// - One supplier's share: 50% or more a majority, 25-50% one large supplier, 10-25% one of several, under 10%
//   minor (plain-language convention; the EU Critical Raw Materials Act benchmark of 65% from one third country
//   is quoted as an external anchor).
// - RCA (Balassa): under 1 no revealed specialisation; 1-2 weak; 2-4 medium; over 4 strong (Hinloopen and van
//   Marrewijk 2001 classes).
// - Growth over a period is restated as an annual rate.

import type { AibDoc } from '../../aib/contract.ts';
import { monthLabel } from '../../posture/detail.ts';
import type { DecodedBundles } from './bundles.ts';

export const nn = (v: unknown): number | null => (typeof v === 'number' && Number.isFinite(v) ? v : null);

/** "76.6%" (one decimal; "0.0%" for a tiny non-zero share is written "under 0.1%"). */
export function pct(v: number | null, dp = 1): string {
  if (v === null) return 'not measured';
  const p = v * 100;
  if (p > 0 && p < 0.05 && dp === 1) return 'under 0.1%';
  return `${p.toFixed(dp)}%`;
}
/** "US$3.3bn", "US$623m" */
export function usd(v: number | null): string {
  if (v === null) return 'not measured';
  const a = Math.abs(v);
  if (a >= 1e9) return `US$${(v / 1e9).toFixed(1)}bn`;
  if (a >= 1e6) return `US$${Math.round(v / 1e6)}m`;
  return `US$${Math.round(v).toLocaleString('en-US')}`;
}
/** "+46%" / "-2%" */
export const signedPct = (p: number): string => `${p >= 0 ? '+' : ''}${Math.round(p)}%`;

/** Annual rate of a total change over `years` (in %). */
export const annualRate = (totalPct: number, years: number): number => (Math.pow(1 + totalPct / 100, 1 / years) - 1) * 100;

export function growthWords(totalPct: number, years: number): string {
  const r = annualRate(totalPct, years);
  if (Math.abs(r) < 0.5) return 'roughly flat';
  return `about ${Math.abs(r).toFixed(1)}% a year ${r > 0 ? 'growth' : 'decline'}`;
}

export const HHI_SOURCE = 'US Department of Justice and Federal Trade Commission, Merger Guidelines 2023';
/** DOJ/FTC 2023 bands, in words. */
export function hhiWords(h: number | null): string | null {
  if (h === null) return null;
  if (h < 0.1) return 'unconcentrated by the US merger-guideline bands (below 0.10)';
  if (h <= 0.18) return 'moderately concentrated by the US merger-guideline bands (0.10 to 0.18)';
  return 'highly concentrated by the US merger-guideline bands (above 0.18)';
}

/** One supplier's share of a market or of a buyer's imports, in words. */
export function supplierShareWords(s: number | null): string | null {
  if (s === null) return null;
  if (s >= 0.5) return 'a majority from one supplier';
  if (s >= 0.25) return 'one large supplier';
  if (s >= 0.1) return 'one of several suppliers';
  return 'a minor supplier';
}

/** Revealed comparative advantage classes. */
export function rcaWords(r: number | null): string | null {
  if (r === null) return null;
  if (r < 1) return 'no revealed export specialisation (below 1)';
  if (r < 2) return 'weak export specialisation (1 to 2)';
  if (r < 4) return 'medium export specialisation (2 to 4)';
  return 'strong export specialisation (above 4)';
}

/** Technology readiness on the IEA's 1-11 scale. */
export function trlWords(t: number | null): string | null {
  if (t === null) return null;
  if (t >= 10) return 'integrated at scale or proven stable (10 or more)';
  if (t >= 9) return 'commercially available (9 or more)';
  if (t >= 6) return 'demonstration to early adoption (6 to 9)';
  return 'early stage (below 6)';
}

/** Economies treated as transit hubs for the entrepot warning (curated; the Explorer itself treats only HKG and MAC so). */
export const TRANSIT_HUBS: readonly string[] = ['NLD', 'BEL', 'SGP', 'ARE', 'HKG', 'MAC'];

/** ISO3 -> name: the posture core's countries, then the AIB document's, then the code itself. */
export function countryNames(b: Pick<DecodedBundles, 'posture' | 'aib'>): (iso3: string | null | undefined) => string {
  const m = new Map<string, string>();
  for (const [k, v] of Object.entries((b.aib?.doc as AibDoc | undefined)?.countries ?? {})) if (typeof v === 'string') m.set(k, v);
  for (const c of b.posture?.core.countries ?? []) m.set(c.iso3, c.name);
  if (!m.has('CHN')) m.set('CHN', 'China');
  return (iso3) => (iso3 ? (m.get(iso3) ?? iso3) : 'not named');
}

/** "Aug 2026" from 202608 (posture/detail.ts monthLabel). */
export const windowEnd = (ym: number | null): string | null => monthLabel(ym);

// ---- the prize note -----------------------------------------------------------------------------------------------

export interface DemandOutlook {
  /** total world-demand change 2024 -> 2035, % */
  totalPct: number | null;
  fromYear: number;
  toYear: number;
  source: string;
  /** capex momentum, % change and window, when the note has it */
  capexPct: number | null;
  capexWindow: string | null;
  capexSource: string | null;
}

/**
 * Parse a posture prize note: "World demand +301% 2024-35 (WEO 2025 STEPS); capex +29% 2021-22 to 2024-25 (CIM)."
 * The scenario words are expanded: WEO 2025 STEPS = IEA World Energy Outlook 2025, Stated Policies Scenario;
 * "IEA minerals STEPS" = IEA Critical Minerals Outlook, Stated Policies; CIM = Clean Investment Monitor.
 */
export function parsePrizeNote(note: string | null | undefined): DemandOutlook | null {
  if (!note) return null;
  const m = /World demand ([+-−]?\d+(?:\.\d+)?)% (\d{4})-(\d{2,4})\s*\(([^)]*)\)/.exec(note);
  if (!m) return null;
  const from = Number(m[2]);
  const to = m[3].length === 2 ? 2000 + Number(m[3]) : Number(m[3]);
  const tag = m[4];
  const source = /minerals/i.test(tag)
    ? 'International Energy Agency Critical Minerals Outlook, Stated Policies Scenario'
    : /WEO\s*(\d{4})/.test(tag)
      ? `International Energy Agency World Energy Outlook ${/WEO\s*(\d{4})/.exec(tag)![1]}, Stated Policies Scenario`
      : tag;
  const c = /capex ([+-−]?\d+(?:\.\d+)?)% (\d{4}-\d{2}) to (\d{4}-\d{2})\s*\(([^)]*)\)/.exec(note);
  return {
    totalPct: Number(m[1].replace('−', '-')),
    fromYear: from,
    toYear: to,
    source,
    capexPct: c ? Number(c[1].replace('−', '-')) : null,
    capexWindow: c ? `${c[2]} to ${c[3]}` : null,
    capexSource: c ? (/CIM/.test(c[4]) ? 'Clean Investment Monitor (world clean-technology manufacturing investment)' : c[4]) : null,
  };
}

// ---- deployment notes ---------------------------------------------------------------------------------------------

/** "demand_growth: WEO Current Policies 2024-2035: 1.68%/yr; ... | output_growth: ..." -> plain words. */
export function cleanNote(note: string | null | undefined): string | null {
  if (!note) return null;
  let first = true;
  return note
    .split('|')
    .map((part) => part.trim().replace(/^([a-z]+(?:_[a-z]+)+):\s*/, (_m, k: string) => `${k.replace(/_/g, ' ')}: `))
    .filter(Boolean)
    .join('; ')
    .replace(/\bWEO\b/g, () => {
      const w = first ? 'International Energy Agency World Energy Outlook 2025' : 'the same outlook';
      first = false;
      return w;
    })
    .replace(/(\d)%\/yr/g, '$1% a year')
    .replace(/ -> /g, ' to ');
}

// ---- AIB words ----------------------------------------------------------------------------------------------------

/**
 * A node flag in plain words: "production more concentrated than trade: CHN holds 97% of GCIM operational capacity:
 * Solar Polysilicon vs 10% of exports, ..." -> "Production is more concentrated than trade: China holds 97% of
 * operational production capacity, against 10% of exports."
 */
export function flagWords(flag: string, name: (iso: string) => string): string {
  const core = productionFlagCore(flag, name);
  if (core) return `Production is more concentrated than trade: ${core}.`;
  return flag.replace(/\bGCIM\b/g, 'capacity database').replace(/^./, (c) => c.toUpperCase());
}

/**
 * The heart of a production-versus-trade flag, "China holds 97% of operational production capacity, against 10% of
 * exports" (GCIM operational capacity, IEA mineral mining or refining output); null for another flag.
 */
export function productionFlagCore(flag: string, name: (iso: string) => string): string | null {
  const m = /([A-Z]{3}) holds (\d+%) of (\S+?)(?: operational capacity)?:[^v]*vs (\d+%) of exports/.exec(flag);
  if (!m || !/production more concentrated than trade/i.test(flag)) return null;
  const what = /GCIM/.test(m[3]) ? 'operational production capacity' : /mining/.test(m[3]) ? 'world mine output' : /refining/.test(m[3]) ? 'world refining output' : 'production';
  return `${name(m[1])} holds ${m[2]} of ${what}, against ${m[4]} of exports`;
}

/** The physical-data status of an AIB node, in words. */
export function physicalDataWords(status: string | null | undefined): string {
  if (status === 'complete') return 'Production data cover the main producers of this product.';
  if (status === 'incomplete') return 'Production data cover part of world production of this product.';
  return 'No production data: supply is read from trade only.';
}
