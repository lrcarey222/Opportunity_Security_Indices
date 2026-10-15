// Main-thread client of the engine worker (spec_react.md 7). One EngineClient per page:
//
//   const client = EngineClient.create();                  // starts the worker, sends `init`
//   useSyncExternalStore(client.subscribe, client.getSnapshot)   // status, manifest, dims, baseline,
//                                                          // chunk states, self-check results
//   const h = client.run(spec, { partner: true }, { onResult, onProgress });
//   const outcome = await h.done;                          // 'done' | 'superseded' | 'cancelled' | 'error'
//   await client.variables(spec, 'es', 'USA');             // replies (tables)
//
// A new run supersedes every older one: their callbacks are detached at once and their `done`
// resolves with status 'superseded' (the worker drops them at its next yield point). Requests
// (`variables`, `decompose`, `dyads`, `tables`, `load`) are independent of runs.

import type { Dims, Manifest, ScenarioSpec, Table } from '../engine/types.ts';
import type {
  BaselineCore, ChunkName, CorePayload, FromWorker, NetworkPayload, PartnerPayload, ResultMessage, RunLayer, RunWant, SelfCheckMessage, ToWorker,
} from './protocol.ts';

/** What the client needs of a Worker (a real one, or an in-process stand-in in tests). */
export interface WorkerLike {
  postMessage(msg: ToWorker): void;
  onmessage: ((e: MessageEvent<FromWorker>) => void) | null;
  onerror?: ((e: ErrorEvent) => void) | null;
  terminate?: () => void;
}

export interface ChunkStatus {
  state: 'idle' | 'loading' | 'loaded' | 'error';
  bytes?: number;
  ms?: number;
  error?: string;
}

export interface EngineFailure {
  code: string;
  message: string;
  layer?: string;
}

/** The client's observable state (immutable snapshots; a new object on every change). */
export interface EngineState {
  status: 'starting' | 'ready' | 'error';
  manifest: Manifest | null;
  dims: Dims | null;
  baseline: BaselineCore | null;
  engine: string | null;
  readyMs: number | null;
  chunks: Record<ChunkName, ChunkStatus>;
  selfcheck: Partial<Record<RunLayer, SelfCheckMessage>>;
  /** a fatal error (no data, schema mismatch, worker failed to start) */
  error: EngineFailure | null;
}

export type CoreResult = Extract<ResultMessage, { layer: 'core' }>;
export type PartnerResult = Extract<ResultMessage, { layer: 'partner' }>;
export type NetworkResult = Extract<ResultMessage, { layer: 'network' }>;
export type ProgressMessage = Extract<FromWorker, { type: 'progress' }>;

export interface RunCallbacks {
  onResult?: (msg: ResultMessage) => void;
  onProgress?: (msg: ProgressMessage) => void;
}

export interface RunOutcome {
  status: 'done' | 'superseded' | 'cancelled' | 'error';
  core?: CoreResult;
  partner?: PartnerResult;
  network?: NetworkResult;
  error?: EngineFailure;
  ms?: number;
}

export interface RunHandle {
  id: number;
  done: Promise<RunOutcome>;
  cancel(): void;
}

export class EngineRequestError extends Error {
  constructor(public code: string, message: string) {
    super(message);
    this.name = 'EngineRequestError';
  }
}

interface PendingRun {
  cb: RunCallbacks;
  outcome: RunOutcome;
  resolve: (o: RunOutcome) => void;
}

interface PendingRequest {
  resolve: (v: unknown) => void;
  reject: (e: Error) => void;
}

/** A request message without its id (distributes over the union). */
type RequestMessage = Extract<ToWorker, { id: number }> extends infer M ? (M extends { type: 'run' | 'cancel' } ? never : M extends unknown ? Omit<M, 'id'> : never) : never;

const idleChunks = (): Record<ChunkName, ChunkStatus> => ({
  core: { state: 'idle' }, vars: { state: 'idle' }, partner: { state: 'idle' }, network: { state: 'idle' },
});

export class EngineClient {
  private state: EngineState = {
    status: 'starting', manifest: null, dims: null, baseline: null, engine: null, readyMs: null, chunks: idleChunks(), selfcheck: {}, error: null,
  };
  private listeners = new Set<() => void>();
  private nextId = 1;
  private runs = new Map<number, PendingRun>();
  private requests = new Map<number, PendingRequest>();
  private readyWaiters: { resolve: (s: EngineState) => void; reject: (e: Error) => void }[] = [];

  /** Start the module worker and load `<dataBase>manifest.json` (default `<BASE_URL>data/`). */
  static create(dataBase?: string): EngineClient {
    const worker = new Worker(new URL('./engine.worker.ts', import.meta.url), { type: 'module', name: 'osi-engine' });
    return new EngineClient(worker as unknown as WorkerLike, dataBase ?? `${import.meta.env.BASE_URL}data/`);
  }

  constructor(private worker: WorkerLike, dataBase: string) {
    worker.onmessage = (e) => this.onMessage(e.data);
    worker.onerror = (e) => this.fail({ code: 'WORKER', message: `The engine worker failed: ${e.message || 'script error'}` });
    worker.postMessage({ type: 'init', dataBase });
  }

  // ---- store (useSyncExternalStore) -----------------------------------------------------------------

  subscribe = (listener: () => void): (() => void) => {
    this.listeners.add(listener);
    return () => this.listeners.delete(listener);
  };

  getSnapshot = (): EngineState => this.state;

  private setState(patch: Partial<EngineState>): void {
    this.state = { ...this.state, ...patch };
    for (const l of this.listeners) l();
  }

  /** Resolves once the core data are loaded (rejects on a fatal error). */
  whenReady(): Promise<EngineState> {
    if (this.state.status === 'ready') return Promise.resolve(this.state);
    if (this.state.status === 'error') return Promise.reject(new EngineRequestError(this.state.error?.code ?? 'ENGINE', this.state.error?.message ?? 'engine error'));
    return new Promise((resolve, reject) => this.readyWaiters.push({ resolve, reject }));
  }

  private fail(err: EngineFailure): void {
    this.setState({ status: 'error', error: err });
    for (const w of this.readyWaiters.splice(0)) w.reject(new EngineRequestError(err.code, err.message));
    for (const [id, r] of this.runs) {
      this.runs.delete(id);
      r.outcome.status = 'error';
      r.outcome.error = err;
      r.resolve(r.outcome);
    }
    for (const [id, q] of this.requests) {
      this.requests.delete(id);
      q.reject(new EngineRequestError(err.code, err.message));
    }
  }

  // ---- runs ---------------------------------------------------------------------------------------

  /** Compute a scenario: core (L1-L3) always, partner and network when wanted. Supersedes older runs. */
  run(spec: ScenarioSpec, want: RunWant = {}, cb: RunCallbacks = {}): RunHandle {
    for (const [id, r] of this.runs) {
      this.runs.delete(id);
      r.outcome.status = 'superseded';
      r.resolve(r.outcome);
    }
    const id = this.nextId++;
    let resolve!: (o: RunOutcome) => void;
    const done = new Promise<RunOutcome>((res) => (resolve = res));
    const outcome: RunOutcome = { status: 'done' };
    if (this.state.status === 'error') {
      outcome.status = 'error';
      outcome.error = this.state.error ?? { code: 'ENGINE', message: 'engine error' };
      resolve(outcome);
    } else {
      this.runs.set(id, { cb, outcome, resolve });
      this.worker.postMessage({ type: 'run', id, spec, want });
    }
    return {
      id,
      done,
      cancel: () => {
        const r = this.runs.get(id);
        if (!r) return;
        this.runs.delete(id);
        this.worker.postMessage({ type: 'cancel', id });
        r.outcome.status = 'cancelled';
        r.resolve(r.outcome);
      },
    };
  }

  // ---- requests -----------------------------------------------------------------------------------

  private request<T>(msg: RequestMessage): Promise<T> {
    if (this.state.status === 'error') {
      return Promise.reject(new EngineRequestError(this.state.error?.code ?? 'ENGINE', this.state.error?.message ?? 'engine error'));
    }
    const id = this.nextId++;
    return new Promise<T>((resolve, reject) => {
      this.requests.set(id, { resolve: resolve as (v: unknown) => void, reject });
      this.worker.postMessage({ ...msg, id } as ToWorker);
    });
  }

  /** Fetch and decode a lazy chunk ahead of need (a run or request loads it anyway). */
  load(chunk: Exclude<ChunkName, 'core'>): Promise<void> {
    return this.request<unknown>({ type: 'load', chunk }).then(() => undefined);
  }

  /** Variable-level contributions of one country's pillar scores (vars chunk). */
  variables(spec: ScenarioSpec, pillar: 'es' | 'eo', country: string, tech?: string, stage?: string): Promise<Table> {
    return this.request<Table>({ type: 'variables', spec, pillar, country, tech, stage });
  }

  /** The FS / PO components of one dyad (partner chunk). */
  decompose(spec: ScenarioSpec, reporter: string, partner: string): Promise<Table> {
    return this.request<Table>({ type: 'decompose', spec, reporter, partner });
  }

  /** One reporter's PSI dyads against the baseline (partner chunk). */
  dyads(spec: ScenarioSpec, reporter: string): Promise<Table> {
    return this.request<Table>({ type: 'dyads', spec, reporter });
  }

  /** One reporter's partners in one sector (cell index into dims.cells), scenario and baseline (partner chunk). */
  cellPartners(spec: ScenarioSpec, reporter: string, cell: number): Promise<Table> {
    return this.request<Table>({ type: 'cellPartners', spec, reporter, cell });
  }

  /** The download tables of a scenario (dash_scenario_tables()). */
  tables(spec: ScenarioSpec, include: RunLayer[] = ['core']): Promise<Record<string, Table>> {
    return this.request<Record<string, Table>>({ type: 'tables', spec, include });
  }

  terminate(): void {
    this.worker.terminate?.();
    this.fail({ code: 'TERMINATED', message: 'The engine was stopped.' });
  }

  // ---- incoming -----------------------------------------------------------------------------------

  private onMessage(msg: FromWorker): void {
    switch (msg.type) {
      case 'ready':
        this.setState({ status: 'ready', manifest: msg.manifest, dims: msg.dims, baseline: msg.baseline, engine: msg.engine, readyMs: msg.ms, error: null });
        for (const w of this.readyWaiters.splice(0)) w.resolve(this.state);
        break;
      case 'chunk':
        this.setState({ chunks: { ...this.state.chunks, [msg.name]: { state: msg.state, bytes: msg.bytes, ms: msg.ms, error: msg.error } } });
        break;
      case 'selfcheck':
        this.setState({ selfcheck: { ...this.state.selfcheck, [msg.layer]: msg } });
        break;
      case 'result': {
        const r = this.runs.get(msg.id);
        if (!r) return;
        if (msg.layer === 'core') r.outcome.core = msg;
        else if (msg.layer === 'partner') r.outcome.partner = msg;
        else r.outcome.network = msg;
        r.cb.onResult?.(msg);
        break;
      }
      case 'progress':
        this.runs.get(msg.id)?.cb.onProgress?.(msg);
        break;
      case 'done': {
        const r = this.runs.get(msg.id);
        if (!r) return;
        this.runs.delete(msg.id);
        r.outcome.ms = msg.ms;
        r.resolve(r.outcome);
        break;
      }
      case 'reply': {
        const q = this.requests.get(msg.id);
        if (!q) return;
        this.requests.delete(msg.id);
        q.resolve(msg.payload);
        break;
      }
      case 'error': {
        const err: EngineFailure = { code: msg.code, message: msg.message, layer: msg.layer };
        if (msg.id !== undefined) {
          const r = this.runs.get(msg.id);
          if (r) {
            this.runs.delete(msg.id);
            r.outcome.status = 'error';
            r.outcome.error = err;
            r.resolve(r.outcome);
            return;
          }
          const q = this.requests.get(msg.id);
          if (q) {
            this.requests.delete(msg.id);
            q.reject(new EngineRequestError(msg.code, msg.message));
            return;
          }
          return;
        }
        // no id: init failed (no data, schema) -> fatal before `ready`
        if (this.state.status !== 'ready') this.fail(err);
        break;
      }
    }
  }
}

export type { CorePayload, PartnerPayload, NetworkPayload, BaselineCore, ChunkName, RunWant, RunLayer, SelfCheckMessage };
