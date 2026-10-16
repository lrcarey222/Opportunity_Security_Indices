// The auxiliary chunks of the Strategic bundle (scripts/85, R/dashboard/dash_ties.R), read on the main
// thread on demand by the Partners view and the Country partners section; never by the engine:
//
//   ties          IMF direct investment positions by partner (both directions), the conduit financial
//                 centres, the CM-FDI factory-project flows behind Friendshore's investment tie,
//                 Development Potential by sector, agreements, UN-vote alignment and each economy's top
//                 partners by Friendshore / Partnership Opportunity (best five sectors)
//   pcell-<slug>  one per sector: each economy's top 10 friendshoring opportunities (Friendshore) and export markets
//                 (Partnership Opportunity) at the paper baseline, with their Partnership Strength rank
//
// The loaders cache one promise per file; the derive functions below are pure (unit-tested).

import { readOsb } from '../../engine/codec.ts';
import { toRows } from '../../engine/table.ts';
import type { Manifest, NDArray, Table } from '../../engine/types.ts';
import { stageSlug } from '../../lib/format.ts';

export type DipMeasure = 'out_debt' | 'in_debt' | 'out_all' | 'in_all';

export interface TiesMeta {
  dip_years: number[];
  dip_gdp_year: number;
  dip_unit: string;
  dip_source: string;
  conduit_rule: string;
  conduit_scope: string;
  top_k: number;
  cmfdi_attribution?: string;
}

export interface DipRow {
  reporter_iso: string;
  partner_iso: string;
  measure: DipMeasure;
  year: number | null;
  value: number | null;
  mean_idx: number | null;
}

export interface CmfdiRow {
  investor_iso: string;
  destination_iso: string;
  tech: string;
  supply_chain: string;
  n_projects: number;
  n_disclosed: number | null;
  value_usd_m: number | null;
}

export interface TopRow {
  reporter_iso: string;
  side: 'fs' | 'po';
  k: number;
  partner_iso: string;
  value: number | null;
  rank: number | null;
  n: number | null;
  psi_rank: number | null;
}

export interface Ties {
  meta: TiesMeta;
  /** the economies of the partner arrays (dims.U) and their names */
  U: string[];
  U_names: string[];
  dip: DipRow[];
  conduits: { iso3: string; name: string; dej_top10: boolean; in_rule: boolean }[];
  cmfdi: CmfdiRow[];
  dp: { partner_iso: string; cell_id: number; value: number | null }[];
  agreements: { iso3: string; agreement: string }[];
  /** UN-vote alignment [economy, partner] over `U` (column-major), or null */
  geo: NDArray | null;
  top_all: TopRow[];
}

export interface SectorTop {
  cell: { tech: string; supply_chain: string; slug: string };
  top: TopRow[];
}

/** The four sectors whose investment tie is the CM-FDI factory projects (config/partnership.yml cm_fdi). */
export const CMFDI_SECTORS = ['Solar', 'Wind', 'Batteries', 'Electric Vehicles'];
export const hasCmfdi = (tech: string, stage: string): boolean => stage === 'Midstream' && CMFDI_SECTORS.includes(tech);

export const CMFDI_ATTRIBUTION =
  'Source: Cleantech Manufacturing FDI Dataset (CM-FDI), P. Bandara and the Net Zero Industrial Policy Lab, Johns Hopkins University, CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/). Changes: project counts and sums of disclosed capital computed by the OSI authors; no record altered.';

// ---- loading ----------------------------------------------------------------------------------------

const dataBase = (): string => {
  const base = (import.meta.env?.BASE_URL as string | undefined) ?? '/';
  return `${base.endsWith('/') ? base : `${base}/`}data/`;
};

let manifestP: Promise<Manifest> | null = null;
const chunkP = new Map<string, Promise<Record<string, unknown> | null>>();

async function getBytes(url: string, noCache = false): Promise<Uint8Array> {
  const res = await fetch(url, noCache ? { cache: 'no-cache' } : {});
  if (!res.ok) throw new Error(`HTTP ${res.status} for ${url}`);
  return new Uint8Array(await res.arrayBuffer());
}

function strategicManifest(m?: Manifest): Promise<Manifest> {
  if (m) return Promise.resolve(m);
  if (!manifestP) {
    manifestP = getBytes(`${dataBase()}manifest.json`, true).then((b) => JSON.parse(new TextDecoder().decode(b)) as Manifest);
    manifestP.catch(() => (manifestP = null));
  }
  return manifestP;
}

/** An auxiliary chunk's objects; null when this data build has no such chunk. */
function auxChunk(name: string, m?: Manifest): Promise<Record<string, unknown> | null> {
  let p = chunkP.get(name);
  if (!p) {
    p = strategicManifest(m).then(async (man) => {
      const info = man.chunks.find((c) => c.name === name);
      if (!info) return null;
      return (await readOsb(await getBytes(dataBase() + info.file))).objects;
    });
    p.catch(() => chunkP.delete(name));
    chunkP.set(name, p);
  }
  return p;
}

/** The `ties` chunk (null: not in this data build). */
export async function loadTies(m?: Manifest): Promise<Ties | null> {
  // scripts/85 writes the chunk's objects at its root (meta, U, dip, ...)
  const o = await auxChunk('ties', m);
  if (!o) return null;
  return parseTies((o.ties && typeof o.ties === 'object' ? o.ties : o) as Record<string, unknown>);
}

/** One sector's top partners at the baseline (null: not in this data build). */
export async function loadSectorTop(tech: string, stage: string, m?: Manifest): Promise<SectorTop | null> {
  const o = await auxChunk(`pcell-${stageSlug(tech, stage)}`, m);
  return o ? parseSectorTop(o) : null;
}

// ---- parsing (pure) -----------------------------------------------------------------------------------

const rows = <T>(t: unknown): T[] => (t && typeof t === 'object' && 'cols' in (t as object) ? (toRows(t as Table) as unknown as T[]) : []);

export function parseTies(o: Record<string, unknown>): Ties {
  const meta = (o.meta ?? {}) as Partial<TiesMeta>;
  const years = Array.isArray(meta.dip_years) ? meta.dip_years.map(Number) : [];
  return {
    meta: {
      dip_years: years,
      dip_gdp_year: Number(meta.dip_gdp_year ?? NaN),
      dip_unit: String(meta.dip_unit ?? 'US$ millions'),
      dip_source: String(meta.dip_source ?? 'IMF, Direct Investment Positions'),
      conduit_rule: String(meta.conduit_rule ?? 'conduits'),
      conduit_scope: String(meta.conduit_scope ?? 'both'),
      top_k: Number(meta.top_k ?? 10),
      cmfdi_attribution: typeof meta.cmfdi_attribution === 'string' ? meta.cmfdi_attribution : undefined,
    },
    U: Array.isArray(o.U) ? o.U.map(String) : [],
    U_names: Array.isArray(o.U_names) ? o.U_names.map(String) : [],
    dip: rows<DipRow>(o.dip),
    conduits: rows(o.conduits),
    cmfdi: rows<CmfdiRow>(o.cmfdi),
    dp: rows(o.dp),
    agreements: rows(o.agreements),
    geo: o.geo && typeof o.geo === 'object' && 'shape' in (o.geo as object) ? (o.geo as NDArray) : null,
    top_all: rows<TopRow>(o.top_all),
  };
}

export function parseSectorTop(o: Record<string, unknown>): SectorTop {
  const c = (o.cell ?? {}) as Record<string, unknown>;
  return { cell: { tech: String(c.tech), supply_chain: String(c.supply_chain), slug: String(c.slug) }, top: rows<TopRow>(o.top) };
}

// ---- derived views (pure) -----------------------------------------------------------------------------

export interface PositionRow {
  partner_iso: string;
  /** the economy's position in the partner (outward), US$ millions */
  out: number | null;
  out_year: number | null;
  /** the partner's position in the economy (inward), US$ millions */
  in: number | null;
  in_year: number | null;
  /** read from the partner's own report because the economy reports none for this pair */
  out_mirror: boolean;
  in_mirror: boolean;
  /** the outward debt-instrument position Friendshore ranks (mean of the index years), US$ millions */
  idx_mean: number | null;
  conduit: boolean;
}

/**
 * The economy's bilateral direct investment by partner. kind 'all': the headline positions (all
 * financial instruments, net, directional); 'debt': the gross debt-instrument positions (the outward
 * one is what Friendshore ranks). The economy's own report first; where it reports nothing for a pair,
 * the partner's report of the mirror flow (its inward position from the economy is the economy's
 * outward one, and vice versa), flagged.
 */
export function positionsOf(ties: Ties, iso: string, kind: 'all' | 'debt'): PositionRow[] {
  const mOut: DipMeasure = kind === 'all' ? 'out_all' : 'out_debt';
  const mIn: DipMeasure = kind === 'all' ? 'in_all' : 'in_debt';
  const conduit = new Set(ties.conduits.filter((c) => c.in_rule).map((c) => c.iso3));
  const by = new Map<string, PositionRow>();
  const row = (p: string): PositionRow => {
    let r = by.get(p);
    if (!r) {
      r = { partner_iso: p, out: null, out_year: null, in: null, in_year: null, out_mirror: false, in_mirror: false, idx_mean: null, conduit: conduit.has(p) };
      by.set(p, r);
    }
    return r;
  };
  for (const d of ties.dip) {
    if (d.reporter_iso === iso && d.partner_iso !== iso) {
      if (d.measure === 'out_debt' && d.mean_idx !== null) row(d.partner_iso).idx_mean = d.mean_idx;
      if (d.value === null) continue;
      if (d.measure === mOut) Object.assign(row(d.partner_iso), { out: d.value, out_year: d.year, out_mirror: false });
      else if (d.measure === mIn) Object.assign(row(d.partner_iso), { in: d.value, in_year: d.year, in_mirror: false });
    }
  }
  for (const d of ties.dip) {
    if (d.partner_iso !== iso || d.reporter_iso === iso || d.value === null) continue;
    const r = by.get(d.reporter_iso);
    // the partner's inward position from the economy = the economy's outward position in the partner
    if (d.measure === mIn && (!r || r.out === null)) Object.assign(row(d.reporter_iso), { out: d.value, out_year: d.year, out_mirror: true });
    else if (d.measure === mOut && (!r || r.in === null)) Object.assign(row(d.reporter_iso), { in: d.value, in_year: d.year, in_mirror: true });
  }
  return [...by.values()].filter((r) => r.out !== null || r.in !== null);
}

/** Partners by size of the two positions together (largest first; ties by ISO3). */
export function bySize(xs: PositionRow[]): PositionRow[] {
  const s = (r: PositionRow) => Math.abs(r.out ?? 0) + Math.abs(r.in ?? 0);
  return [...xs].sort((a, b) => s(b) - s(a) || (a.partner_iso < b.partner_iso ? -1 : 1));
}

export interface ProjectFlowRow {
  partner_iso: string;
  /** projects by the economy's firms in the partner */
  out_projects: number;
  out_value: number | null;
  out_disclosed: number | null;
  /** projects by the partner's firms in the economy */
  in_projects: number;
  in_value: number | null;
  in_disclosed: number | null;
}

/** The CM-FDI factory projects in one sector, both directions, by partner (largest first). */
export function projectFlows(ties: Ties, iso: string, tech: string, stage: string): ProjectFlowRow[] {
  const by = new Map<string, ProjectFlowRow>();
  const row = (p: string) => {
    let r = by.get(p);
    if (!r) {
      r = { partner_iso: p, out_projects: 0, out_value: null, out_disclosed: null, in_projects: 0, in_value: null, in_disclosed: null };
      by.set(p, r);
    }
    return r;
  };
  const add = (a: number | null, b: number | null) => (b === null ? a : (a ?? 0) + b);
  for (const f of ties.cmfdi) {
    if (f.tech !== tech || f.supply_chain !== stage || f.investor_iso === f.destination_iso) continue;
    if (f.investor_iso === iso) {
      const r = row(f.destination_iso);
      r.out_projects += f.n_projects;
      r.out_value = add(r.out_value, f.value_usd_m);
      r.out_disclosed = add(r.out_disclosed, f.n_disclosed);
    } else if (f.destination_iso === iso) {
      const r = row(f.investor_iso);
      r.in_projects += f.n_projects;
      r.in_value = add(r.in_value, f.value_usd_m);
      r.in_disclosed = add(r.in_disclosed, f.n_disclosed);
    }
  }
  return [...by.values()].sort((a, b) => b.out_projects + b.in_projects - (a.out_projects + a.in_projects) || (a.partner_iso < b.partner_iso ? -1 : 1));
}

/** Development Potential of every partner in one sector (cell_id as in dims.cells). */
export function dpOfCell(ties: Ties, cellId: number): Map<string, number | null> {
  return new Map(ties.dp.filter((d) => d.cell_id === cellId).map((d) => [d.partner_iso, d.value]));
}

/** The agreements two economies share (the coalition's economic-security agreements). */
export function sharedAgreements(ties: Ties, a: string, b: string): string[] {
  const of = (x: string) => new Set(ties.agreements.filter((g) => g.iso3 === x).map((g) => g.agreement));
  const A = of(a);
  return [...of(b)].filter((g) => A.has(g)).sort();
}

/** UN-vote alignment of the pair (null: not in the data). */
export function alignmentOf(ties: Ties, a: string, b: string): number | null {
  const U = ties.U;
  if (!ties.geo) return null;
  const i = U.indexOf(a);
  const j = U.indexOf(b);
  if (i < 0 || j < 0) return null;
  const v = Number((ties.geo.data as ArrayLike<number>)[i + U.length * j]);
  return Number.isFinite(v) ? v : null;
}

/** An economy's name from the chunk (the ISO3 code when it is not one of U). */
export function tiesName(ties: Ties | null, iso: string): string {
  const i = ties ? ties.U.indexOf(iso) : -1;
  return i >= 0 ? (ties as Ties).U_names[i] ?? iso : iso;
}

/** US$ millions as words ("$12.3 billion", "$450 million", "$0.4 million"). */
export function fmtUsdM(v: number | null): string {
  if (v === null || !Number.isFinite(v)) return '–';
  const a = Math.abs(v);
  const sign = v < 0 ? '−' : '';
  if (a >= 1e6) return `${sign}$${(a / 1e6).toFixed(a >= 1e7 ? 0 : 1)} trillion`;
  if (a >= 1e3) return `${sign}$${(a / 1e3).toFixed(a >= 1e4 ? 0 : 1)} billion`;
  return `${sign}$${a >= 10 ? Math.round(a) : a.toFixed(1)} million`;
}
