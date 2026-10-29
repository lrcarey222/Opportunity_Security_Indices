// Static data of a loaded build, derived once from the worker's `ready` message: the manifest, the
// dimensions as row objects with lookups, the shipped baseline, the URL context and the pure-helper
// "bundle" the main thread needs for the instant dial readout (makeParams / dialReadout).

import { parseBaselineInputs } from '../engine/bundle.ts';
import { defaultAdvanced } from '../engine/params.ts';
import { has, toRows } from '../engine/table.ts';
import type { BaselineInputs, Dims, Manifest, Params, Table } from '../engine/types.ts';
import type { BaselineCore } from '../worker/protocol.ts';
import type { UrlCtx } from './url.ts';

export interface CountryRow {
  iso3c: string;
  iso_n3: string | null;
  country: string;
  region: string | null;
  sovereign: boolean;
  tier: 'core' | 'partner' | 'none';
  network_node: string | null;
  eu_member: boolean;
  lon: number | null;
  lat: number | null;
  bloc: string | null;
}

export interface NodeRow {
  network_node: string;
  label: string;
  lon: number | null;
  lat: number | null;
  n_members: number;
  tier: string | null;
}

export interface CellRow {
  tech: string;
  supply_chain: string;
  sector: string;
}

export interface AppData {
  manifest: Manifest;
  dims: Dims;
  baseline: BaselineCore;
  /** what makeParams / dialReadout need on the main thread */
  bundleLike: { manifest: Manifest; baselineInputs: BaselineInputs; baselineParams: Params };
  urlCtx: UrlCtx;
  countries: CountryRow[];
  byIso: Map<string, CountryRow>;
  byName: Map<string, CountryRow>;
  /** coalition members (core, then partners; alphabetical) */
  coalition: CountryRow[];
  nodes: NodeRow[];
  nodeByCode: Map<string, NodeRow>;
  techs: string[];
  stages: string[];
  cells: CellRow[];
  /** economies with pillar data (country selects) */
  scored: CountryRow[];
  /** reporters of the partner arrays (dims.U) */
  U: string[];
  Uindex: Map<string, number>;
  defaultTech: string;
  defaultStage: string;
  defaultCountry: string;
  fixture: boolean;
}

const s = (v: unknown): string | null => (typeof v === 'string' ? v : null);
const n = (v: unknown): number | null => (typeof v === 'number' && Number.isFinite(v) ? v : null);

function countryRows(t: Table): CountryRow[] {
  return toRows(t).map((r) => ({
    iso3c: String(r.iso3c),
    iso_n3: s(r.iso_n3),
    country: s(r.country) ?? String(r.iso3c),
    region: s(r.region),
    sovereign: r.sovereign === true,
    tier: r.tier === 'core' ? 'core' : r.tier === 'partner' ? 'partner' : 'none',
    network_node: s(r.network_node),
    eu_member: r.eu_member === true,
    lon: n(r.lon),
    lat: n(r.lat),
    bloc: s(r.bloc),
  }));
}

export function buildAppData(manifest: Manifest, dims: Dims, baseline: BaselineCore): AppData {
  const baselineInputs = parseBaselineInputs(manifest.baseline_inputs);
  const defaults = defaultAdvanced({ baselineInputs });
  const countries = countryRows(dims.countries);
  const byIso = new Map(countries.map((c) => [c.iso3c, c]));
  const byName = new Map(countries.map((c) => [c.country, c]));
  // the pillar tables use the pipeline's country names; map any that differ from dims.countries
  for (const t of [baseline.pillars.es, baseline.pillars.eo]) {
    if (!has(t, 'iso3c') || !has(t, 'Country')) continue;
    const iso = t.cols.iso3c as (string | null)[];
    const nm = t.cols.Country as (string | null)[];
    for (let i = 0; i < t.nrow; i++) {
      const c = iso[i] ? byIso.get(iso[i] as string) : undefined;
      if (c && nm[i] && !byName.has(nm[i] as string)) byName.set(nm[i] as string, c);
    }
  }
  const tierOrder = (t: string) => (t === 'core' ? 0 : t === 'partner' ? 1 : 2);
  const coalition = countries
    .filter((c) => c.tier === 'core' || c.tier === 'partner')
    .sort((a, b) => tierOrder(a.tier) - tierOrder(b.tier) || a.country.localeCompare(b.country, 'en'));
  const nodes: NodeRow[] = toRows(dims.nodes).map((r) => ({
    network_node: String(r.network_node),
    label: s(r.label) ?? String(r.network_node),
    lon: n(r.lon),
    lat: n(r.lat),
    n_members: n(r.n_members) ?? 1,
    tier: s(r.tier),
  }));
  const cells: CellRow[] = toRows(dims.cells).map((r) => ({
    tech: String(r.tech),
    supply_chain: String(r.supply_chain),
    sector: s(r.sector) ?? `${r.tech} - ${r.supply_chain}`,
  }));
  const scoredIso = new Set<string>();
  for (const t of [baseline.pillars.es, baseline.pillars.eo]) {
    if (!has(t, 'iso3c')) continue;
    for (const v of t.cols.iso3c as (string | null)[]) if (v) scoredIso.add(v);
  }
  const scored = countries.filter((c) => scoredIso.has(c.iso3c)).sort((a, b) => a.country.localeCompare(b.country, 'en'));
  const techs = [...dims.techs];
  const stages = [...dims.stages];
  const pick = (want: string, xs: string[]) => (xs.includes(want) ? want : (xs[0] ?? want));
  const U = [...dims.U];
  return {
    manifest,
    dims,
    baseline,
    bundleLike: { manifest, baselineInputs, baselineParams: baseline.params },
    urlCtx: { esCategories: [...dims.es_categories], eoCategories: [...dims.eo_categories], defaults, techs, stages },
    countries,
    byIso,
    byName,
    coalition,
    nodes,
    nodeByCode: new Map(nodes.map((x) => [x.network_node, x])),
    techs,
    stages,
    cells,
    scored,
    U,
    Uindex: new Map(U.map((x, i) => [x, i])),
    defaultTech: pick('Batteries', techs),
    // Batteries — Refining (design E6, the posture module's default node)
    defaultStage: pick('Refining', stages),
    defaultCountry: scoredIso.has('USA') ? 'USA' : (scored[0]?.iso3c ?? 'USA'),
    fixture: /synthetic|fixture/i.test(manifest.paper?.title ?? '') || manifest.schema !== 'osi-dashboard',
  };
}

/** The ISO3 code of a pipeline country name (tables keyed by Country). */
export function isoOf(app: AppData, country: string | null): string | null {
  if (!country) return null;
  return app.byName.get(country)?.iso3c ?? null;
}

export function countryName(app: AppData, iso: string | null): string {
  if (!iso) return '';
  return app.byIso.get(iso)?.country ?? app.nodeByCode.get(iso)?.label ?? iso;
}

export function nodeLabel(app: AppData, code: string | null): string {
  if (!code) return '';
  return app.nodeByCode.get(code)?.label ?? app.byIso.get(code)?.country ?? code;
}
