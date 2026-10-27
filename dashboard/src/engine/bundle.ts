// The engine's bundle from manifest.json and the decoded chunks (spec_react.md 4.3, 6.1). Pure.

import { isNDArray, isTable, type OsbDecoded, type Table } from './codec.ts';
import { EngineError } from './numeric.ts';
import type { BaselineInputs, Bundle, Manifest, PartnerInputs, PillarInputs } from './types.ts';

export const SCHEMA = 'osi-dashboard';
export const SCHEMA_VERSION = 1;
/** The engine's chunks (the network chunk of the retired allied network design is gone, 2026-10-17). */
export const CHUNK_NAMES = ['core', 'vars', 'partner'] as const;
export type ChunkName = (typeof CHUNK_NAMES)[number];

/** A JSON value that R wrote as a character vector of length 1 (["x"]) -> "x". */
export function s1(v: unknown): string {
  if (Array.isArray(v)) return v.length ? String(v[0]) : '';
  return v === null || v === undefined ? '' : String(v);
}

/** A JSON value that R wrote as a character vector -> string[]. */
export function sv(v: unknown): string[] {
  if (Array.isArray(v)) return v.map((x) => String(x));
  if (v === null || v === undefined) return [];
  return [String(v)];
}

function isPlainObject(x: unknown): x is Record<string, unknown> {
  return typeof x === 'object' && x !== null && !Array.isArray(x) && !ArrayBuffer.isView(x) && !isTable(x) && !isNDArray(x);
}

function mergeInto(target: Record<string, unknown>, source: Record<string, unknown>): void {
  for (const [k, v] of Object.entries(source)) {
    if (isPlainObject(v) && isPlainObject(target[k])) mergeInto(target[k] as Record<string, unknown>, v);
    else target[k] = v;
  }
}

/** bundle$manifest$baseline_inputs from manifest.json, character scalars unboxed. */
export function parseBaselineInputs(raw: Record<string, unknown>): BaselineInputs {
  const r = raw;
  const num = (v: unknown) => (v === null || v === undefined ? null : Number(v));
  return {
    ...(r as object),
    tau: num(r.tau),
    psi_tau: num(r.psi_tau),
    allow_partial_categories: r.allow_partial_categories === true,
    policy_iea_var: s1(r.policy_iea_var),
    policy_cat_var: s1(r.policy_cat_var),
    excluded_sectors: sv(r.excluded_sectors),
    allied_excluded_techs: r.allied_excluded_techs === null || r.allied_excluded_techs === undefined ? null : sv(r.allied_excluded_techs),
    require_resource_base: r.require_resource_base !== false,
    fossil_techs: sv(r.fossil_techs),
    psi_view: s1(r.psi_view) as 'coalition' | 'world',
  } as BaselineInputs;
}

function pillarInputs(x: Record<string, unknown>, name: string): PillarInputs {
  for (const k of ['cells', 'categories', 'score', 'present', 'cw_imp', 'cw_tot', 'n_var']) {
    if (x[k] === undefined) throw new EngineError('BUNDLE', `pillars.${name}.${k} is missing.`);
  }
  return {
    ...(x as object),
    categories: sv(x.categories),
    categories_in_data: sv(x.categories_in_data),
    allow_partial_categories: x.allow_partial_categories === true,
  } as PillarInputs;
}

/** Add one decoded chunk to a bundle (or start the bundle from `core`). */
export function addChunk(bundle: Bundle, name: string, chunk: OsbDecoded): Bundle {
  const o = chunk.objects as Record<string, Record<string, unknown>>;
  if (o.baseline) mergeInto(bundle.baseline, o.baseline);
  if (name === 'vars') {
    const p = o.pillars as Record<string, Record<string, unknown>> | undefined;
    if (p?.es?.var) bundle.pillars.es.var = p.es.var as Table;
    if (p?.eo?.var) bundle.pillars.eo.var = p.eo.var as Table;
  } else if (name === 'partner') {
    const pa = o.partner as Record<string, unknown>;
    if (!pa) throw new EngineError('BUNDLE', 'The partner chunk has no partner object.');
    const psi = (pa.psi ?? {}) as Record<string, unknown>;
    bundle.partner = {
      ...(pa as object),
      U: bundle.dims.U,
      names: bundle.dims.U_names,
      sovereign: bundle.dims.U_sovereign,
      cells: bundle.dims.cells,
      psi: { coalition_reporters: sv(psi.coalition_reporters), tau: psi.tau === null || psi.tau === undefined ? null : Number(psi.tau) },
    } as PartnerInputs;
  }
  bundle.loaded.add(name);
  return bundle;
}

/** The bundle from manifest.json and the core chunk; further chunks are added with addChunk(). */
export function assembleBundle(manifest: Manifest, core: OsbDecoded, makeBaselineParams: (b: Bundle) => Bundle['baselineParams']): Bundle {
  if (manifest.schema !== SCHEMA) throw new EngineError('SCHEMA', `Unknown data schema ${manifest.schema}.`);
  if (manifest.schema_version > SCHEMA_VERSION) {
    throw new EngineError('SCHEMA', 'This data build needs a newer app; reload.');
  }
  const o = core.objects as Record<string, Record<string, unknown>>;
  const d = o.dims;
  if (!d) throw new EngineError('BUNDLE', 'The core chunk has no dims.');
  const dims = {
    ...(d as object),
    techs: sv(d.techs),
    stages: sv(d.stages),
    fossil_techs: sv(d.fossil_techs),
    es_categories: sv(d.es_categories),
    eo_categories: sv(d.eo_categories),
    U: sv(d.U),
    U_names: sv(d.U_names),
  } as Bundle['dims'];
  const pil = o.pillars as Record<string, Record<string, unknown>>;
  const pol = o.policy as Record<string, unknown>;
  const si = o.si as Record<string, unknown>;
  const bundle: Bundle = {
    manifest,
    baselineInputs: parseBaselineInputs(manifest.baseline_inputs),
    baselineParams: undefined as unknown as Bundle['baselineParams'],
    dims,
    pillars: { es: pillarInputs(pil.es, 'es'), eo: pillarInputs(pil.eo, 'eo') },
    policy: {
      cells: pol.cells as Table,
      variables: sv(pol.variables),
      values: pol.values as Bundle['policy']['values'],
      required: pol.required ? sv(pol.required) : null,
      components: pol.components ? sv(pol.components) : null,
    },
    si: { tech_ghg: si.tech_ghg as Table, trl: (si.trl as Table) ?? null, allies: si.allies as Table, resource_base: (si.resource_base as Table) ?? null },
    baseline: {},
    loaded: new Set(),
  };
  addChunk(bundle, 'core', core);
  bundle.baselineParams = makeBaselineParams(bundle);
  return bundle;
}
