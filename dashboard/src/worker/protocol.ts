// Messages between the main thread and the engine worker (spec_react.md section 7). Plain
// structured-cloneable objects; tables are columnar (src/engine/table.ts).

import type { Manifest, Dims, Table, ScenarioSpec, Params, NumRecord, PsiView } from '../engine/types.ts';

export type ChunkName = 'core' | 'vars' | 'partner' | 'network';
export type RunLayer = 'core' | 'partner' | 'network';

export interface RunWant {
  partner?: boolean;
  network?: boolean;
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
  network_summary: { source: string; specialization: Table; diagnostics: Table } | null;
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

export interface NetworkPayload {
  is_baseline: boolean;
  stages: { tech: string; supply_chain: string }[];
  scored: { nodes: Table } | null;
  solution: {
    specialization: Table;
    flows: Table;
    diagnostics: Table;
    member_shares: Table | null;
    portfolio: { caps: Table; counts: Table; topk: Table; converged: boolean | null; iters: number | null; violations_remaining: number | null } | null;
    forbidden: Record<string, string[]>;
  };
  /** what the solution is: the greedy solver, or the paper's stored MILP design (baseline only) */
  method: 'greedy' | 'milp' | 'stored';
  /** the design comparisons are made against */
  reference: 'greedy' | 'milp';
  compare: { stage: Table; node: Table; node_summary: Table } | null;
  concentration: Table;
  elapsed_ms: number;
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
  | { type: 'result'; id: number; layer: 'network'; key: string; source: 'baseline' | 'computed'; payload: NetworkPayload; ms: number; cached: boolean }
  | { type: 'progress'; id: number; layer: 'partner' | 'network'; phase: string; current: number; total: number; tech?: string; supply_chain?: string; iter?: number }
  | { type: 'done'; id: number; ms: number }
  | { type: 'reply'; id: number; payload: unknown }
  | SelfCheckMessage
  | { type: 'error'; id?: number; layer?: string; code: string; message: string };

export type ResultMessage = Extract<FromWorker, { type: 'result' }>;
