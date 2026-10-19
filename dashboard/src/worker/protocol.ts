// Messages between the main thread and the engine worker (spec_react.md section 7). Plain
// structured-cloneable objects; tables are columnar (src/engine/table.ts).

import type { Manifest, Dims, Table, ScenarioSpec, Params, NumRecord, PsiView } from '../engine/types.ts';

// The network chunk and layer of the original allied network design are retired (2026-10-17).
export type ChunkName = 'core' | 'vars' | 'partner';
export type RunLayer = 'core' | 'partner';

export interface RunWant {
  partner?: boolean;
  contrib?: boolean;
}

// ---- main -> worker -------------------------------------------------------------------------------

export type ToWorker =
  | { type: 'init'; dataBase: string }
  | { type: 'run'; id: number; spec: ScenarioSpec; want: RunWant }
  | { type: 'load'; id: number; chunk: Exclude<ChunkName, 'core'> }
  | { type: 'variables'; id: number; spec: ScenarioSpec; pillar: 'es' | 'eo'; country: string; tech?: string; stage?: string }
  | { type: 'decompose'; id: number; spec: ScenarioSpec; reporter: string; partner: string }
  | { type: 'dyads'; id: number; spec: ScenarioSpec; reporter: string }
  /** one reporter's partners in one sector (cell index into dims.cells): engine.cellPartners() */
  | { type: 'cellPartners'; id: number; spec: ScenarioSpec; reporter: string; cell: number }
  | { type: 'tables'; id: number; spec: ScenarioSpec; include: Array<RunLayer> }
  | { type: 'cancel'; id: number };

// ---- payloads -------------------------------------------------------------------------------------

/** The shipped baseline L1-L3 tables and summaries (posted with `ready`). */
export interface BaselineCore {
  params: Params;
  readout: Table;
  pillars: { es: Table; eo: Table };
  policy: { index: Table; eligibility: Table; weights: NumRecord };
  si: { cells: Table; excluded: Table; degenerate: Table; weights: NumRecord; stage_weights: NumRecord };
  allied: Table;
  psi_partner: Table | null;
  psi_view: PsiView;
  psi_reporters: string[];
}

export interface CorePayload {
  is_baseline: boolean;
  params: Params;
  readout: Table;
  notes: string[];
  pillars: { es: Table; eo: Table; es_contrib?: Table; eo_contrib?: Table };
  policy: { index: Table; eligibility: Table; weights: NumRecord };
  si: { cells: Table; excluded: Table; degenerate: Table; weights: NumRecord; stage_weights: NumRecord };
  allied: Table;
  compare: { si: Table; es: Table; eo: Table; policy: Table; allied: Table };
}

export interface PartnerPayload {
  is_baseline: boolean;
  /** best-five matrices [reporter + U * partner] over dims.U */
  top5: { fs: Float64Array; po: Float64Array; fs_exists: Uint8Array; po_exists: Uint8Array };
  psi: { partner: Table; reporters: string[]; view: PsiView };
  compare: { psi_partner: Table };
}

// ---- worker -> main -------------------------------------------------------------------------------

export interface SelfCheckMessage {
  type: 'selfcheck';
  layer: RunLayer;
  ok: boolean;
  n: number;
  max_abs_diff: number;
  detail?: string;
}

export type FromWorker =
  | { type: 'ready'; manifest: Manifest; dims: Dims; baseline: BaselineCore; ms: number; engine: string }
  | { type: 'chunk'; name: ChunkName; state: 'loading' | 'loaded' | 'error'; bytes?: number; ms?: number; error?: string }
  | { type: 'result'; id: number; layer: 'core'; key: string; source: 'baseline' | 'computed'; payload: CorePayload; ms: number; cached: boolean }
  | { type: 'result'; id: number; layer: 'partner'; key: string; source: 'baseline' | 'computed'; payload: PartnerPayload; ms: number; cached: boolean }
  | { type: 'progress'; id: number; layer: 'partner'; phase: string; current: number; total: number }
  | { type: 'done'; id: number; ms: number }
  | { type: 'reply'; id: number; payload: unknown }
  | SelfCheckMessage
  | { type: 'error'; id?: number; layer?: string; code: string; message: string };

export type ResultMessage = Extract<FromWorker, { type: 'result' }>;
