// The engine host: everything the Web Worker does (spec_react.md 7), written against injected
// `post` and `fetchBytes` functions so it runs (and is tested) in Node as well as in a worker. It
// owns data loading, the decoded bundle, the layer caches, the runs (newest supersedes older ones)
// and the self-check. The engine (src/engine) stays pure.

import { addChunk, assembleBundle, SCHEMA_VERSION, type ChunkName as EngineChunk } from '../engine/bundle.ts';
import { readOsb, type OsbDecoded } from '../engine/codec.ts';
import { compare, compareDyads, LayerCache, layerKeys, resultNotes, run, scenarioTables, type CompareResult, type DashResult } from '../engine/engine.ts';
import { ENGINE_VERSION } from '../engine/version.ts';
import { EngineError, yieldNow } from '../engine/numeric.ts';
import { dialReadout, isBaseline, makeParams } from '../engine/params.ts';
import { cellPartners, fsCells, partnerDecompose, partnerTop5, poCells, psi } from '../engine/partner.ts';
import { variableContrib } from '../engine/pillars.ts';
import { selfCheckCore, selfCheckPartner } from '../engine/selfcheck.ts';
import type {
  Bundle, Manifest, Params, PartnerCells, PillarsResult, PolicyResult, PsiResult, ScenarioSpec, SiResult, Table, Top5Result,
} from '../engine/types.ts';
import type { BaselineCore, ChunkName, CorePayload, FromWorker, PartnerPayload, RunLayer, RunWant, ToWorker } from './protocol.ts';

export type Post = (msg: FromWorker, transfer: Transferable[]) => void;
export type FetchBytes = (url: string, opts: { noCache?: boolean }) => Promise<Uint8Array>;
export type Inflate = (bytes: Uint8Array) => Promise<Uint8Array>;

const now = () => (typeof performance !== 'undefined' ? performance.now() : Date.now());

/** Deep copy of a payload with every typed array copied into its own buffer (a view on a chunk
 * buffer would otherwise clone the whole chunk); returns the copy and the buffers to transfer. */
export function detach<T>(x: T): { value: T; transfer: Transferable[] } {
  const transfer: Transferable[] = [];
  const seen = new Map<unknown, unknown>();
  const walk = (v: unknown): unknown => {
    if (v === null || typeof v !== 'object') return v;
    if (seen.has(v)) return seen.get(v);
    if (ArrayBuffer.isView(v)) {
      const c = (v as unknown as Float64Array).slice();
      transfer.push(c.buffer);
      seen.set(v, c);
      return c;
    }
    if (Array.isArray(v)) {
      const out: unknown[] = [];
      seen.set(v, out);
      for (const e of v) out.push(walk(e));
      return out;
    }
    if (v instanceof Set || v instanceof Map) return v;
    const out: Record<string, unknown> = {};
    seen.set(v, out);
    for (const [k, e] of Object.entries(v as Record<string, unknown>)) out[k] = walk(e);
    return out;
  };
  return { value: walk(x) as T, transfer };
}

const get = (o: unknown, path: string): unknown =>
  path.split('.').reduce<unknown>((x, k) => (x && typeof x === 'object' ? (x as Record<string, unknown>)[k] : undefined), o);

const errMessage = (e: unknown) => (e instanceof Error ? e.message : String(e));

class Superseded extends Error {}

interface PartnerState {
  cells: PartnerCells | null; // null: the shipped baseline (no cells computed)
  top5: Top5Result;
  psi: PsiResult;
}

/** Worker cache limits (spec_react.md 6.8); FS / PO cell arrays are 12.7 MB each. */
export const CACHE_LIMITS = { pillars: 8, policy: 8, si: 8, psi: 8, top5: 8, fs_cells: 4, po_cells: 4 };

export class EngineHost {
  private bundle: Bundle | null = null;
  private manifest: Manifest | null = null;
  private dataBase = '';
  private initP: Promise<void> | null = null;
  private chunkLoads = new Map<string, Promise<void>>();
  private cache: LayerCache;
  private latestRun = 0;
  private busy = 0;
  private baselineCore: DashResult | null = null;
  private selfChecked = new Set<string>();

  constructor(
    private post: Post,
    private fetchBytes: FetchBytes,
    private inflate?: Inflate,
    opts: { cacheLimits?: Partial<typeof CACHE_LIMITS>; selfCheck?: boolean } = {},
  ) {
    this.cache = new LayerCache({ ...CACHE_LIMITS, ...opts.cacheLimits });
    this.selfCheckOn = opts.selfCheck !== false;
  }

  private selfCheckOn: boolean;

  // ---- messages -----------------------------------------------------------------------------------

  /** Handle one message. Runs interleave at their yield points; a newer run supersedes older ones. */
  async handle(msg: ToWorker): Promise<void> {
    try {
      if (msg.type === 'init') {
        this.initP = this.init(msg.dataBase);
        await this.initP;
        return;
      }
      if (msg.type === 'cancel') {
        if (msg.id >= this.latestRun) this.latestRun = msg.id + 1;
        return;
      }
      if (msg.type === 'run') this.latestRun = Math.max(this.latestRun, msg.id);
      // every other message waits for init (the client may send before `ready`)
      if (this.initP) await this.initP.catch(() => undefined);
      if (!this.bundle) throw new EngineError('DATA_LOAD', 'The engine is not initialised (no data).');
      switch (msg.type) {
        case 'load':
          await this.ensureChunk(msg.chunk);
          this.reply(msg.id, { chunk: msg.chunk, loaded: true });
          break;
        case 'run':
          await this.run(msg.id, msg.spec, msg.want);
          break;
        case 'variables':
          await this.variables(msg.id, msg.spec, msg.pillar, msg.country, msg.tech, msg.stage);
          break;
        case 'decompose':
          await this.decompose(msg.id, msg.spec, msg.reporter, msg.partner);
          break;
        case 'dyads':
          await this.dyads(msg.id, msg.spec, msg.reporter);
          break;
        case 'cellPartners':
          await this.cellPartners(msg.id, msg.spec, msg.reporter, msg.cell);
          break;
        case 'tables':
          await this.tables(msg.id, msg.spec, msg.include);
          break;
      }
    } catch (e) {
      if (e instanceof Superseded) return;
      const id = 'id' in msg ? (msg as { id: number }).id : undefined;
      const code = e instanceof EngineError ? e.code : 'ENGINE';
      this.send({ type: 'error', id, code, message: errMessage(e) });
    }
  }

  private send(msg: FromWorker, transfer: Transferable[] = []): void {
    this.post(msg, transfer);
  }

  private reply(id: number, payload: unknown): void {
    const d = detach(payload);
    this.send({ type: 'reply', id, payload: d.value }, d.transfer);
  }

  // ---- data ---------------------------------------------------------------------------------------

  private async readChunk(name: string): Promise<OsbDecoded> {
    const info = this.manifest?.chunks.find((c) => c.name === name);
    if (!info) throw new EngineError('DATA_LOAD', `The data build has no ${name} chunk.`);
    const bytes = await this.fetchBytes(this.dataBase + info.file, {});
    return readOsb(bytes, this.inflate);
  }

  private async init(dataBase: string): Promise<void> {
    const t0 = now();
    this.dataBase = dataBase.endsWith('/') ? dataBase : `${dataBase}/`;
    let manifest: Manifest;
    try {
      const raw = await this.fetchBytes(`${this.dataBase}manifest.json`, { noCache: true });
      manifest = JSON.parse(new TextDecoder().decode(raw)) as Manifest;
    } catch (e) {
      throw new EngineError('DATA_LOAD', `No data: could not read ${this.dataBase}manifest.json (${errMessage(e)}).`);
    }
    if (typeof manifest.schema_version !== 'number' || manifest.schema_version > SCHEMA_VERSION) {
      throw new EngineError('SCHEMA', `This data build (schema ${manifest.schema_version}) needs a newer app; reload the page.`);
    }
    this.manifest = manifest;
    this.send({ type: 'chunk', name: 'core', state: 'loading' });
    let core: OsbDecoded;
    try {
      core = await this.readChunk('core');
    } catch (e) {
      this.send({ type: 'chunk', name: 'core', state: 'error', error: errMessage(e) });
      throw e instanceof EngineError ? e : new EngineError('DATA_LOAD', `Could not load the core data: ${errMessage(e)}`);
    }
    const b = assembleBundle(manifest, core, (x) => makeParams(null, x));
    this.bundle = b;
    this.chunkLoads.set('core', Promise.resolve());
    const info = manifest.chunks.find((c) => c.name === 'core');
    this.send({ type: 'chunk', name: 'core', state: 'loaded', bytes: info?.bytes_gz, ms: now() - t0 });
    const base = this.shippedBaselineCore(b);
    const d = detach({ manifest, dims: b.dims, baseline: base });
    this.send({ type: 'ready', manifest: d.value.manifest, dims: d.value.dims, baseline: d.value.baseline, ms: now() - t0, engine: ENGINE_VERSION }, d.transfer);
    this.idle(() => this.runSelfCheck('core'));
  }

  private async ensureChunk(name: ChunkName): Promise<void> {
    if (!this.bundle) throw new EngineError('DATA_LOAD', 'The engine is not initialised (no data).');
    const existing = this.chunkLoads.get(name);
    if (existing) return existing;
    const t0 = now();
    this.send({ type: 'chunk', name, state: 'loading' });
    const p = this.readChunk(name).then(
      (ch) => {
        addChunk(this.bundle as Bundle, name as EngineChunk, ch);
        const info = this.manifest?.chunks.find((c) => c.name === name);
        this.send({ type: 'chunk', name, state: 'loaded', bytes: info?.bytes_gz, ms: now() - t0 });
        if (name === 'partner') this.idle(() => this.runSelfCheck('partner'));
      },
      (e) => {
        this.chunkLoads.delete(name); // a later request retries
        this.send({ type: 'chunk', name, state: 'error', error: errMessage(e) });
        throw new EngineError('DATA_LOAD', `Could not load the ${name} data: ${errMessage(e)}`);
      },
    );
    this.chunkLoads.set(name, p);
    return p;
  }

  /** Run f when no run is in progress (polls every 50 ms). */
  private idle(f: () => void | Promise<void>): void {
    if (!this.selfCheckOn) return;
    setTimeout(() => {
      if (this.busy > 0) {
        this.idle(f);
        return;
      }
      void Promise.resolve()
        .then(f)
        .catch(() => undefined);
    }, 50);
  }

  private async runSelfCheck(layer: RunLayer): Promise<void> {
    if (this.selfChecked.has(layer) || !this.bundle) return;
    this.selfChecked.add(layer);
    const b = this.bundle;
    let r;
    try {
      if (layer === 'core') r = selfCheckCore(b, 1e-9, { cache: this.cache });
      else {
        // the baseline FS and PO cells one at a time (cached and pinned), yielding to runs in between,
        // so the check never blocks a dial move for more than one cell pass
        const keys = layerKeys(b.baselineParams);
        for (const layerName of ['fs_cells', 'po_cells'] as const) {
          if (!this.cache.has(layerName, keys[layerName])) {
            const f = layerName === 'fs_cells' ? fsCells : poCells;
            this.cache.set(layerName, keys[layerName], f(b, b.baselineParams), true);
          }
          await yieldNow();
          if (this.busy > 0) {
            this.selfChecked.delete(layer);
            this.idle(() => this.runSelfCheck(layer));
            return;
          }
        }
        r = selfCheckPartner(b, 1e-12, { cache: this.cache });
      }
    } catch (e) {
      r = { ok: false, n: 0, max_abs_diff: NaN, detail: `self-check failed to run: ${errMessage(e)}` };
    }
    this.send({ type: 'selfcheck', layer, ok: r.ok, n: r.n, max_abs_diff: r.max_abs_diff, detail: r.detail });
  }

  // ---- baseline tables ------------------------------------------------------------------------------

  private shippedBaselineCore(b: Bundle): BaselineCore {
    const bc = b.baseline.core as Record<string, unknown>;
    const pol = bc.policy as Record<string, unknown>;
    const si = bc.si as Record<string, unknown>;
    const view = get(b.baseline, 'psi_view');
    return {
      params: b.baselineParams,
      readout: dialReadout(b.baselineParams, b),
      pillars: { es: bc.es as Table, eo: bc.eo as Table },
      policy: { index: pol.index as Table, eligibility: pol.eligibility as Table, weights: pol.weights as Record<string, number> },
      si: {
        cells: si.cells as Table, excluded: si.excluded as Table, degenerate: si.degenerate as Table,
        weights: si.weights as Record<string, number>, stage_weights: si.stage_weights as Record<string, number>,
      },
      allied: bc.allied as Table,
      psi_partner: (b.baseline.psi_partner as Table) ?? null,
      psi_view: (Array.isArray(view) ? view[0] : view) === 'world' ? 'world' : 'coalition',
      psi_reporters: (b.baseline.psi_reporters as string[]) ?? [],
    };
  }

  /** The shipped baseline as a DashResult (L1-L3), the reference of every comparison. */
  private baselineResult(b: Bundle): DashResult {
    if (this.baselineCore) return this.baselineCore;
    const s = this.shippedBaselineCore(b);
    this.baselineCore = {
      params: b.baselineParams,
      pillars: { es: s.pillars.es, eo: s.pillars.eo } as PillarsResult,
      policy: s.policy as PolicyResult,
      si: s.si as SiResult,
      allied: s.allied,
      partner: null,
      keys: layerKeys(b.baselineParams),
      notes: resultNotes(b, b.baselineParams),
    };
    return this.baselineCore;
  }

  /** The shipped baseline best-five and PSI (partner chunk). */
  private shippedBaselinePartner(b: Bundle): { top5: Top5Result; psi: PsiResult } {
    const bp = get(b.baseline, 'partner') as (Record<string, { data: unknown }> & { psi_dyad: Table }) | undefined;
    if (!bp) throw new EngineError('DATA_LOAD', 'The partner data are not loaded.');
    const s = this.shippedBaselineCore(b);
    const top5: Top5Result = {
      fs: bp.top5_fs.data as Float64Array, po: bp.top5_po.data as Float64Array,
      fs_exists: bp.fs_exists.data as Uint8Array, po_exists: bp.po_exists.data as Uint8Array,
    };
    return { top5, psi: { dyad: bp.psi_dyad, partner: s.psi_partner as Table, reporters: s.psi_reporters, view: s.psi_view } };
  }

  /** The baseline FS / PO cells (computed once, cached and pinned). */
  private baselineCells(b: Bundle): PartnerCells {
    const params = b.baselineParams;
    const keys = layerKeys(params);
    let fs = this.cache.get<Float64Array>('fs_cells', keys.fs_cells);
    if (!fs) fs = this.cache.set('fs_cells', keys.fs_cells, fsCells(b, params), true);
    let po = this.cache.get<Float64Array>('po_cells', keys.po_cells);
    if (!po) po = this.cache.set('po_cells', keys.po_cells, poCells(b, params), true);
    return { fs_index: fs, po_index: po };
  }

  // ---- runs ---------------------------------------------------------------------------------------

  private check(id: number | null): void {
    if (id !== null && id < this.latestRun) throw new Superseded();
  }

  private async pause(id: number | null): Promise<void> {
    await yieldNow();
    this.check(id);
  }

  private coreResult(b: Bundle, params: Params, contrib: boolean): DashResult {
    return run(b, params, this.cache, { partner: false, contributions: contrib });
  }

  /** FS / PO cells, best-five and PSI of a scenario (cached per layer). id = null: not a run
   * (no progress messages, never superseded). */
  private async partnerState(id: number | null, b: Bundle, params: Params): Promise<{ state: PartnerState & { cells: PartnerCells }; cached: boolean }> {
    const keys = layerKeys(params);
    const progress = (phase: string, current: number, total: number) => {
      if (id !== null) this.send({ type: 'progress', id, layer: 'partner', phase, current, total });
    };
    let cached = true;
    let fs = this.cache.get<Float64Array>('fs_cells', keys.fs_cells);
    if (!fs) {
      cached = false;
      progress('friendshore', 0, 4);
      fs = this.cache.set('fs_cells', keys.fs_cells, fsCells(b, params));
      await this.pause(id);
    }
    let po = this.cache.get<Float64Array>('po_cells', keys.po_cells);
    if (!po) {
      cached = false;
      progress('opportunity', 1, 4);
      po = this.cache.set('po_cells', keys.po_cells, poCells(b, params));
      await this.pause(id);
    }
    const cells = { fs_index: fs, po_index: po };
    let top5 = this.cache.get<Top5Result>('top5', keys.top5);
    if (!top5) {
      cached = false;
      progress('best-five', 2, 4);
      top5 = this.cache.set('top5', keys.top5, partnerTop5(b, cells, params));
      await this.pause(id);
    }
    const pk = `${keys.psi}|${keys.top5}`;
    let p = this.cache.get<PsiResult>('psi', pk);
    if (!p) {
      cached = false;
      progress('psi', 3, 4);
      p = this.cache.set('psi', pk, psi(b, top5, params));
    }
    return { state: { cells, top5, psi: p }, cached };
  }

  async run(id: number, spec: ScenarioSpec, want: RunWant): Promise<void> {
    this.busy++;
    const tRun = now();
    try {
      const b = this.bundle as Bundle;
      const params = makeParams(spec, b);
      const baseline = isBaseline(params, b);
      const base = this.baselineResult(b);

      // L1-L3 (always)
      let t0 = now();
      let core: DashResult;
      let source: 'baseline' | 'computed' = 'computed';
      let cached = false;
      if (baseline && !want.contrib) {
        core = base;
        source = 'baseline';
        cached = true;
      } else {
        const keys = layerKeys(params);
        cached = this.cache.has('si', keys.si) && this.cache.has('pillars', keys.pillars);
        core = this.coreResult(b, params, want.contrib === true);
        if (baseline) source = 'baseline';
      }
      this.check(id);
      const cmp: CompareResult = compare(core, base);
      const corePayload: CorePayload = {
        is_baseline: baseline,
        params,
        readout: dialReadout(params, b),
        notes: core.notes,
        pillars: { es: core.pillars.es, eo: core.pillars.eo, ...(want.contrib ? { es_contrib: core.pillars.es_contrib, eo_contrib: core.pillars.eo_contrib } : {}) },
        policy: { index: core.policy.index, eligibility: core.policy.eligibility, weights: core.policy.weights },
        si: { cells: core.si.cells, excluded: core.si.excluded, degenerate: core.si.degenerate, weights: core.si.weights, stage_weights: core.si.stage_weights },
        allied: core.allied,
        compare: { si: cmp.si, es: cmp.es, eo: cmp.eo, policy: cmp.policy, allied: cmp.allied },
      };
      const dc = detach(corePayload);
      this.send({ type: 'result', id, layer: 'core', key: core.keys.si, source, payload: dc.value, ms: now() - t0, cached }, dc.transfer);

      if (want.partner) {
        await this.ensureChunk('partner');
        this.check(id);
        t0 = now();
        const shipped = this.shippedBaselinePartner(b);
        let part: PartnerState;
        let pCached = false;
        let pSource: 'baseline' | 'computed' = 'computed';
        if (baseline) {
          // the shipped baseline tables (exact)
          part = { cells: null, top5: shipped.top5, psi: shipped.psi };
          pSource = 'baseline';
          pCached = true;
        } else {
          const r = await this.partnerState(id, b, params);
          part = r.state;
          pCached = r.cached;
        }
        const pc = compare({ ...core, partner: { cells: emptyCells, top5: part.top5, psi: part.psi } }, { ...base, partner: { cells: emptyCells, ...shipped } });
        const payload: PartnerPayload = {
          is_baseline: baseline,
          top5: { fs: part.top5.fs, po: part.top5.po, fs_exists: part.top5.fs_exists, po_exists: part.top5.po_exists },
          psi: { partner: part.psi.partner, reporters: part.psi.reporters, view: part.psi.view },
          compare: { psi_partner: pc.psi_partner as Table },
        };
        const dp = detach(payload);
        this.send({ type: 'result', id, layer: 'partner', key: layerKeys(params).psi, source: pSource, payload: dp.value, ms: now() - t0, cached: pCached }, dp.transfer);
      }
      this.send({ type: 'done', id, ms: now() - tRun });
    } finally {
      this.busy--;
    }
  }

  // ---- replies ------------------------------------------------------------------------------------

  private async variables(id: number, spec: ScenarioSpec, pillar: 'es' | 'eo', country: string, tech?: string, stage?: string): Promise<void> {
    await this.ensureChunk('vars');
    const b = this.bundle as Bundle;
    const params = makeParams(spec, b);
    const core = this.coreResult(b, params, false);
    this.reply(id, variableContrib(b, core.pillars, pillar, country, tech ?? null, stage ?? null));
  }

  private async decompose(id: number, spec: ScenarioSpec, reporter: string, partner: string): Promise<void> {
    await this.ensureChunk('partner');
    const b = this.bundle as Bundle;
    const params = makeParams(spec, b);
    const cells = isBaseline(params, b) ? this.baselineCells(b) : (await this.partnerState(null, b, params)).state.cells;
    this.reply(id, partnerDecompose(b, params, cells, reporter, partner));
  }

  private async dyads(id: number, spec: ScenarioSpec, reporter: string): Promise<void> {
    await this.ensureChunk('partner');
    const b = this.bundle as Bundle;
    const params = makeParams(spec, b);
    const shipped = this.shippedBaselinePartner(b);
    const cur = isBaseline(params, b) ? shipped.psi : (await this.partnerState(null, b, params)).state.psi;
    this.reply(id, compareDyads(cur, shipped.psi, reporter));
  }

  private async cellPartners(id: number, spec: ScenarioSpec, reporter: string, cell: number): Promise<void> {
    await this.ensureChunk('partner');
    const b = this.bundle as Bundle;
    const params = makeParams(spec, b);
    const base = this.baselineCells(b);
    const cells = isBaseline(params, b) ? base : (await this.partnerState(null, b, params)).state.cells;
    this.reply(id, cellPartners(b, cells, base, reporter, cell));
  }

  private async tables(id: number, spec: ScenarioSpec, include: RunLayer[]): Promise<void> {
    const b = this.bundle as Bundle;
    const params = makeParams(spec, b);
    const baseline = isBaseline(params, b);
    const core = this.coreResult(b, params, true);
    const base = this.baselineResult(b);
    let result: DashResult = core;
    let shipped: { top5: Top5Result; psi: PsiResult } | null = null;
    if (include.includes('partner')) {
      await this.ensureChunk('partner');
      shipped = this.shippedBaselinePartner(b);
      const part = baseline ? { cells: emptyCells, ...shipped } : (await this.partnerState(null, b, params)).state;
      result = { ...core, partner: part };
    }
    const cmp = compare(result, { ...base, partner: shipped ? { cells: emptyCells, ...shipped } : null });
    this.reply(id, scenarioTables(result, cmp, b));
  }
}

const emptyCells: PartnerCells = { fs_index: new Float64Array(0), po_index: new Float64Array(0) };

/** fetch() as FetchBytes. */
export const fetchBytesHttp: FetchBytes = async (url, opts) => {
  const res = await fetch(url, opts.noCache ? { cache: 'no-cache' } : {});
  if (!res.ok) throw new Error(`HTTP ${res.status} for ${url}`);
  return new Uint8Array(await res.arrayBuffer());
};
